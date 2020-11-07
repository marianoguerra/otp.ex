defmodule :cowboy_static do
  use Bitwise
  require Record

  Record.defrecord(:r_file_info, :file_info,
    size: :undefined,
    type: :undefined,
    access: :undefined,
    atime: :undefined,
    mtime: :undefined,
    ctime: :undefined,
    mode: :undefined,
    links: :undefined,
    major_device: :undefined,
    minor_device: :undefined,
    inode: :undefined,
    uid: :undefined,
    gid: :undefined
  )

  Record.defrecord(:r_file_descriptor, :file_descriptor,
    module: :undefined,
    data: :undefined
  )

  def init(req, {name, path}) do
    init_opts(req, {name, path, []})
  end

  def init(req, {name, app, path})
      when name === :priv_file or name === :priv_dir do
    init_opts(req, {name, app, path, []})
  end

  def init(req, opts) do
    init_opts(req, opts)
  end

  defp init_opts(req, {:priv_file, app, path, extra}) do
    {privPath, howToAccess} = priv_path(app, path)
    init_info(req, absname(privPath), howToAccess, extra)
  end

  defp init_opts(req, {:file, path, extra}) do
    init_info(req, absname(path), :direct, extra)
  end

  defp init_opts(req, {:priv_dir, app, path, extra}) do
    {privPath, howToAccess} = priv_path(app, path)
    init_dir(req, privPath, howToAccess, extra)
  end

  defp init_opts(req, {:dir, path, extra}) do
    init_dir(req, path, :direct, extra)
  end

  defp priv_path(app, path) do
    case :code.priv_dir(app) do
      {:error, :bad_name} ->
        :erlang.error(
          {:badarg, 'Can\'t resolve the priv_dir of application ' ++ :erlang.atom_to_list(app)}
        )

      privDir when is_list(path) ->
        {privDir ++ '/' ++ path, how_to_access_app_priv(privDir)}

      privDir when is_binary(path) ->
        {<<:erlang.list_to_binary(privDir)::binary, ?/, path::binary>>,
         how_to_access_app_priv(privDir)}
    end
  end

  defp how_to_access_app_priv(privDir) do
    case :filelib.is_dir(privDir) do
      true ->
        :direct

      false ->
        how_to_access_app_priv1(privDir)
    end
  end

  defp how_to_access_app_priv1(dir) do
    archive = :filename.dirname(dir)

    case archive do
      ^dir ->
        :direct

      _ ->
        case :filelib.is_regular(archive) do
          true ->
            {:archive, archive}

          false ->
            how_to_access_app_priv1(archive)
        end
    end
  end

  defp absname(path) when is_list(path) do
    :filename.absname(:erlang.list_to_binary(path))
  end

  defp absname(path) when is_binary(path) do
    :filename.absname(path)
  end

  defp init_dir(req, path, howToAccess, extra)
       when is_list(path) do
    init_dir(req, :erlang.list_to_binary(path), howToAccess, extra)
  end

  defp init_dir(req, path, howToAccess, extra) do
    dir = fullpath(:filename.absname(path))

    case :cowboy_req.path_info(req) do
      :undefined ->
        {:ok, :cowboy_req.reply(500, req), :error}

      pathInfo ->
        case validate_reserved(pathInfo) do
          :error ->
            {:cowboy_rest, req, :error}

          :ok ->
            filepath = :filename.join([dir | pathInfo])
            len = byte_size(dir)

            case fullpath(filepath) do
              <<^dir::size(len)-binary, ?/, _::binary>> ->
                init_info(req, filepath, howToAccess, extra)

              <<^dir::size(len)-binary>> ->
                init_info(req, filepath, howToAccess, extra)

              _ ->
                {:cowboy_rest, req, :error}
            end
        end
    end
  end

  defp validate_reserved([]) do
    :ok
  end

  defp validate_reserved([p | tail]) do
    case validate_reserved1(p) do
      :ok ->
        validate_reserved(tail)

      :error ->
        :error
    end
  end

  defp validate_reserved1(<<>>) do
    :ok
  end

  defp validate_reserved1(<<?/, _::bits>>) do
    :error
  end

  defp validate_reserved1(<<?\\, _::bits>>) do
    :error
  end

  defp validate_reserved1(<<0, _::bits>>) do
    :error
  end

  defp validate_reserved1(<<_, rest::bits>>) do
    validate_reserved1(rest)
  end

  defp fullpath(path) do
    fullpath(:filename.split(path), [])
  end

  defp fullpath([], acc) do
    :filename.join(:lists.reverse(acc))
  end

  defp fullpath(["." | tail], acc) do
    fullpath(tail, acc)
  end

  defp fullpath([".." | tail], acc = [_]) do
    fullpath(tail, acc)
  end

  defp fullpath([".." | tail], [_ | acc]) do
    fullpath(tail, acc)
  end

  defp fullpath([segment | tail], acc) do
    fullpath(tail, [segment | acc])
  end

  defp init_info(req, path, howToAccess, extra) do
    info = read_file_info(path, howToAccess)
    {:cowboy_rest, req, {path, info, extra}}
  end

  defp read_file_info(path, :direct) do
    case :file.read_file_info(
           path,
           [{:time, :universal}]
         ) do
      {:ok, info} ->
        {:direct, info}

      error ->
        error
    end
  end

  defp read_file_info(path, {:archive, archive}) do
    case :file.read_file_info(
           archive,
           [{:time, :universal}]
         ) do
      {:ok, archiveInfo} ->
        pathS = :erlang.binary_to_list(path)

        case :erl_prim_loader.read_file_info(pathS) do
          {:ok, containedFileInfo} ->
            info =
              fix_archived_file_info(
                archiveInfo,
                containedFileInfo
              )

            {:archive, info}

          :error ->
            {:error, :enoent}
        end

      error ->
        error
    end
  end

  defp fix_archived_file_info(archiveInfo, containedFileInfo) do
    r_file_info(archiveInfo,
      size: r_file_info(containedFileInfo, :size),
      type: r_file_info(containedFileInfo, :type),
      access: :read
    )
  end

  def malformed_request(req, state) do
    {state === :error, req, state}
  end

  def forbidden(req, state = {_, {_, r_file_info(type: :directory)}, _}) do
    {true, req, state}
  end

  def forbidden(req, state = {_, {:error, :eacces}, _}) do
    {true, req, state}
  end

  def forbidden(req, state = {_, {_, r_file_info(access: access)}, _})
      when access === :write or access === :none do
    {true, req, state}
  end

  def forbidden(req, state) do
    {false, req, state}
  end

  def content_types_provided(req, state = {path, _, extra})
      when is_list(extra) do
    case :lists.keyfind(:mimetypes, 1, extra) do
      false ->
        {[{:cow_mimetypes.web(path), :get_file}], req, state}

      {:mimetypes, module, function} ->
        {[{apply(module, function, [path]), :get_file}], req, state}

      {:mimetypes, type} ->
        {[{type, :get_file}], req, state}
    end
  end

  def charsets_provided(req, state = {path, _, extra}) do
    case :lists.keyfind(:charset, 1, extra) do
      false ->
        :no_call

      {:charset, module, function} ->
        {[apply(module, function, [path])], req, state}

      {:charset, charset} when is_binary(charset) ->
        {[charset], req, state}
    end
  end

  def ranges_provided(req, state) do
    {[{"bytes", :auto}], req, state}
  end

  def resource_exists(req, state = {_, {_, r_file_info(type: :regular)}, _}) do
    {true, req, state}
  end

  def resource_exists(req, state) do
    {false, req, state}
  end

  def generate_etag(
        req,
        state = {path, {_, r_file_info(size: size, mtime: mtime)}, extra}
      ) do
    case :lists.keyfind(:etag, 1, extra) do
      false ->
        {generate_default_etag(size, mtime), req, state}

      {:etag, module, function} ->
        {apply(module, function, [path, size, mtime]), req, state}

      {:etag, false} ->
        {:undefined, req, state}
    end
  end

  defp generate_default_etag(size, mtime) do
    {:strong,
     :erlang.integer_to_binary(
       :erlang.phash2(
         {size, mtime},
         4_294_967_295
       )
     )}
  end

  def last_modified(req, state = {_, {_, r_file_info(mtime: modified)}, _}) do
    {modified, req, state}
  end

  def get_file(
        req,
        state = {path, {:direct, r_file_info(size: size)}, _}
      ) do
    {{:sendfile, 0, size, path}, req, state}
  end

  def get_file(req, state = {path, {:archive, _}, _}) do
    pathS = :erlang.binary_to_list(path)
    {:ok, bin, _} = :erl_prim_loader.get_file(pathS)
    {bin, req, state}
  end
end
