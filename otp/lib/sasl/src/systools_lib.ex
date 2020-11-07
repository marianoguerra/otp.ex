defmodule :m_systools_lib do
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

  def file_term2binary(fileIn, fileOut) do
    case read_term(fileIn) do
      {:ok, term} ->
        case :file.write_file(
               fileOut,
               :erlang.term_to_binary(term)
             ) do
          :ok ->
            :ok

          {:error, error} ->
            {:error, {:open, fileOut, error}}
        end

      other ->
        other
    end
  end

  def read_term(file) do
    case :file.open(file, [:read]) do
      {:ok, stream} ->
        res = read_term_from_stream(stream, file)

        case :file.close(stream) do
          :ok ->
            res

          {:error, error} ->
            {:error, {:close, file, error}}
        end

      {:error, error} ->
        {:error, {:open, file, error}}
    end
  end

  def read_term_from_stream(stream, file) do
    encoding = :epp.set_encoding(stream)

    r =
      :io.request(
        stream,
        {:get_until, encoding, :"", :erl_scan, :tokens, [1]}
      )

    case r do
      {:ok, toks, _EndLine} ->
        case :erl_parse.parse_term(toks) do
          {:ok, term} ->
            {:ok, term}

          {:error, error} ->
            {:error, {:parse, file, error}}
        end

      {:error, _E, _EndLine} ->
        {:error, {:read, file}}

      {:eof, _EndLine} ->
        {:error, {:read, file}}
    end
  end

  def get_dirs(regPath) when is_list(regPath) do
    names = :filename.split(regPath)
    expNames = expand_names(names)

    try do
      get_dirs(expNames, [], true)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  def get_dirs(_) do
    {:error, :badarg}
  end

  def get_path(regPath) when is_list(regPath) do
    f = fn regP ->
      case get_dirs(regP) do
        {:ok, dirs} ->
          {true, dirs}

        _ ->
          false
      end
    end

    flat(:lists.zf(f, regPath), [])
  end

  def get_path(_) do
    []
  end

  defp expand_names(names) do
    :lists.map(
      fn
        '*' ->
          {true, '[^/]+'}

        n ->
          case :lists.member(?*, n) do
            true ->
              {true, expand(n, [])}

            _ ->
              {false, n}
          end
      end,
      names
    )
  end

  defp expand([?* | t], ack) do
    expand(t, '*]/^[' ++ ack)
  end

  defp expand([h | t], ack) do
    expand(t, [h | ack])
  end

  defp expand([], ack) do
    :lists.reverse(ack)
  end

  defp get_dirs([{false, name} | t], f, root) do
    get_dirs(t, add_dir(name, f, root), false)
  end

  defp get_dirs([{true, regName} | t], f, root) do
    get_dirs(t, add_dirs(regName, f, root), false)
  end

  defp get_dirs([], f, _) do
    {:ok, f}
  end

  defp add_dir(name, [], true) do
    case dir_p(name) do
      true ->
        [name]

      _ ->
        []
    end
  end

  defp add_dir(name, dirs, _Root) do
    :lists.zf(
      fn d0 ->
        d = :filename.join(d0, name)

        case dir_p(d) do
          true ->
            {true, d}

          _ ->
            false
        end
      end,
      dirs
    )
  end

  defp add_dirs(regName, _Dirs, true) do
    case regexp_match(regName, '.', true) do
      {true, addDirs} ->
        addDirs

      _ ->
        []
    end
  end

  defp add_dirs(regName, dirs, root) do
    fun = fn dir ->
      regexp_match(regName, dir, root)
    end

    flat(:lists.zf(fun, dirs), [])
  end

  defp regexp_match(regName, d0, root) do
    case :file.list_dir(d0) do
      {:ok, files} when length(files) > 0 ->
        case :re.compile(regName, [:unicode]) do
          {:ok, mP} ->
            fR = fn f ->
              case :re.run(f, mP, [{:capture, :first, :list}]) do
                {:match, [^f]} ->
                  dirF = join(d0, f, root)

                  case dir_p(dirF) do
                    true ->
                      {true, dirF}

                    _ ->
                      false
                  end

                _ ->
                  false
              end
            end

            {true, :lists.zf(fR, files)}

          _ ->
            false
        end

      _ ->
        false
    end
  end

  defp join(_, f, true) do
    f
  end

  defp join(dir, f, _) do
    :filename.join(dir, f)
  end

  defp dir_p(dirF) do
    case :file.read_file_info(dirF) do
      {:ok, info} when r_file_info(info, :type) == :directory ->
        true

      _ ->
        false
    end
  end

  defp flat([h | t], ack) when is_list(hd(h)) do
    flat(t, :lists.reverse(h) ++ ack)
  end

  defp flat([[] | t], ack) do
    flat(t, ack)
  end

  defp flat([h | t], ack) do
    flat(t, [h | ack])
  end

  defp flat([], ack) do
    :lists.reverse(ack)
  end

  def werror(options, warnings) do
    :lists.member(
      :warnings_as_errors,
      options
    ) and warnings !== []
  end
end
