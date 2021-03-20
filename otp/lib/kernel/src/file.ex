defmodule :m_file do
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

  def native_name_encoding() do
    :erlang.nif_error(:undef)
  end

  def format_error({_Line, :file, :undefined_script}) do
    'no value returned from script'
  end

  def format_error({line, :file, {class, reason, stacktrace}}) do
    :io_lib.format('~w: evaluation failed with reason ~w:~w and stacktrace ~w', [
      line,
      class,
      reason,
      stacktrace
    ])
  end

  def format_error({line, :file, {reason, stacktrace}}) do
    :io_lib.format('~w: evaluation failed with reason ~w and stacktrace ~w', [
      line,
      reason,
      stacktrace
    ])
  end

  def format_error({line, mod, reason}) do
    :io_lib.format('~w: ~ts', [line, mod.format_error(reason)])
  end

  def format_error(:badarg) do
    'bad argument'
  end

  def format_error(:system_limit) do
    'a system limit was hit, probably not enough ports'
  end

  def format_error(:terminated) do
    'the file server process is terminated'
  end

  def format_error(errorId) do
    :erl_posix_msg.message(errorId)
  end

  def pid2name(pid) when is_pid(pid) do
    case :erlang.whereis(:file_server_2) do
      :undefined ->
        :undefined

      _ ->
        case :ets.lookup(:file_io_servers, pid) do
          [{_, name} | _] ->
            {:ok, name}

          _ ->
            :undefined
        end
    end
  end

  def get_cwd() do
    call(:get_cwd, [])
  end

  def get_cwd(drive) do
    check_and_call(:get_cwd, [file_name(drive)])
  end

  def set_cwd(dirname) do
    check_and_call(:set_cwd, [file_name(dirname)])
  end

  def delete(name) do
    check_and_call(:delete, [file_name(name)])
  end

  def delete(name, opts) when is_list(opts) do
    args = [file_name(name), opts]

    case check_args(args) do
      :ok ->
        case :lists.member(:raw, opts) do
          true ->
            [fileName | _] = args
            :prim_file.delete(fileName)

          false ->
            call(:delete, args)
        end

      error ->
        error
    end
  end

  def rename(from, to) do
    check_and_call(
      :rename,
      [file_name(from), file_name(to)]
    )
  end

  def make_dir(name) do
    check_and_call(:make_dir, [file_name(name)])
  end

  def del_dir(name) do
    check_and_call(:del_dir, [file_name(name)])
  end

  def del_dir_r(file) do
    case read_link_info(file) do
      {:ok, r_file_info(type: :directory)} ->
        case list_dir_all(file) do
          {:ok, names} ->
            :lists.foreach(
              fn name ->
                del_dir_r(:filename.join(file, name))
              end,
              names
            )

          {:error, _Reason} ->
            :ok
        end

        del_dir(file)

      {:ok, _FileInfo} ->
        delete(file)

      {:error, _Reason} = error ->
        error
    end
  end

  def read_file_info(ioDevice)
      when is_pid(ioDevice) or
             elem(ioDevice, 0) === :file_descriptor do
    read_file_info(ioDevice, [])
  end

  def read_file_info(name) do
    check_and_call(:read_file_info, [file_name(name)])
  end

  def read_file_info(ioDevice, opts)
      when is_pid(ioDevice) and
             is_list(opts) do
    file_request(ioDevice, {:read_handle_info, opts})
  end

  def read_file_info(r_file_descriptor(module: module) = handle, opts)
      when is_list(opts) do
    module.read_handle_info(handle, opts)
  end

  def read_file_info(name, opts) when is_list(opts) do
    args = [file_name(name), opts]

    case check_args(args) do
      :ok ->
        case :lists.member(:raw, opts) do
          true ->
            [fileName | _] = args
            :prim_file.read_file_info(fileName, opts)

          false ->
            call(:read_file_info, args)
        end

      error ->
        error
    end
  end

  def altname(name) do
    check_and_call(:altname, [file_name(name)])
  end

  def read_link_info(name) do
    check_and_call(:read_link_info, [file_name(name)])
  end

  def read_link_info(name, opts) when is_list(opts) do
    args = [file_name(name), opts]

    case check_args(args) do
      :ok ->
        case :lists.member(:raw, opts) do
          true ->
            [fileName | _] = args
            :prim_file.read_link_info(fileName, opts)

          false ->
            call(:read_link_info, args)
        end

      error ->
        error
    end
  end

  def read_link(name) do
    check_and_call(:read_link, [file_name(name)])
  end

  def read_link_all(name) do
    check_and_call(:read_link_all, [file_name(name)])
  end

  def write_file_info(name, info = r_file_info()) do
    check_and_call(
      :write_file_info,
      [file_name(name), info]
    )
  end

  def write_file_info(name, info = r_file_info(), opts) when is_list(opts) do
    args = [file_name(name), info, opts]

    case check_args(args) do
      :ok ->
        case :lists.member(:raw, opts) do
          true ->
            [fileName | _] = args
            :prim_file.write_file_info(fileName, info, opts)

          false ->
            call(:write_file_info, args)
        end

      error ->
        error
    end
  end

  def list_dir(name) do
    check_and_call(:list_dir, [file_name(name)])
  end

  def list_dir_all(name) do
    check_and_call(:list_dir_all, [file_name(name)])
  end

  def read_file(name) do
    check_and_call(:read_file, [file_name(name)])
  end

  def make_link(old, new) do
    check_and_call(
      :make_link,
      [file_name(old), file_name(new)]
    )
  end

  def make_symlink(old, new) do
    check_and_call(
      :make_symlink,
      [file_name(old), file_name(new)]
    )
  end

  def write_file(name, bin) do
    check_and_call(
      :write_file,
      [file_name(name), make_binary(bin)]
    )
  end

  def write_file(name, iOData, modeList) when is_list(modeList) do
    case :lists.member(:raw, modeList) do
      true ->
        try do
          :erlang.iolist_size(iOData)
        catch
          :error, error ->
            {:error, error}
        else
          _Size ->
            do_write_file(name, iOData, modeList)
        end

      false ->
        case make_binary(iOData) do
          bin when is_binary(bin) ->
            do_write_file(name, bin, modeList)

          error ->
            error
        end
    end
  end

  defp do_write_file(name, iOData, modeList) do
    case open(name, [:binary, :write | modeList]) do
      {:ok, handle} ->
        case write(handle, iOData) do
          :ok ->
            close(handle)

          e1 ->
            _ = close(handle)
            e1
        end

      e2 ->
        e2
    end
  end

  def raw_read_file_info(name) do
    read_file_info(name, [:raw])
  end

  def raw_write_file_info(name, r_file_info() = info) do
    write_file_info(name, info, [:raw])
  end

  def open(item, modeList) when is_list(modeList) do
    case {:lists.member(:raw, modeList), :lists.member(:ram, modeList)} do
      {false, false} ->
        args = [file_name(item) | modeList]

        case check_args(args) do
          :ok ->
            [fileName | _] = args
            call(:open, [fileName, modeList])

          error ->
            error
        end

      {true, false} ->
        :raw_file_io.open(file_name(item), modeList)

      {false, true} ->
        :ram_file.open(item, modeList)

      {true, true} ->
        :erlang.error(:badarg, [item, modeList])
    end
  end

  def open(item, mode) do
    open(item, mode_list(mode))
  end

  def close(file) when is_pid(file) do
    case file_request(file, :close) do
      {:error, :terminated} ->
        :ok

      other ->
        other
    end
  end

  def close(r_file_descriptor(module: module) = handle) do
    module.close(handle)
  end

  def close(_) do
    {:error, :badarg}
  end

  def advise(file, offset, length, advise)
      when is_pid(file) do
    file_request(file, {:advise, offset, length, advise})
  end

  def advise(r_file_descriptor(module: module) = handle, offset, length, advise) do
    module.advise(handle, offset, length, advise)
  end

  def advise(_, _, _, _) do
    {:error, :badarg}
  end

  def allocate(file, offset, length) when is_pid(file) do
    file_request(file, {:allocate, offset, length})
  end

  def allocate(r_file_descriptor(module: module) = handle, offset, length) do
    module.allocate(handle, offset, length)
  end

  def read(file, sz)
      when is_pid(file) or
             (is_atom(file) and
                is_integer(sz) and sz >= 0) do
    case :io.request(
           file,
           {:get_chars, :latin1, :"", sz}
         ) do
      data when is_list(data) or is_binary(data) ->
        {:ok, data}

      other ->
        other
    end
  end

  def read(r_file_descriptor(module: module) = handle, sz)
      when is_integer(sz) and sz >= 0 do
    module.read(handle, sz)
  end

  def read(_, _) do
    {:error, :badarg}
  end

  def read_line(file) when is_pid(file) or is_atom(file) do
    case :io.request(file, {:get_line, :latin1, :""}) do
      data when is_list(data) or is_binary(data) ->
        {:ok, data}

      other ->
        other
    end
  end

  def read_line(r_file_descriptor(module: module) = handle) do
    module.read_line(handle)
  end

  def read_line(_) do
    {:error, :badarg}
  end

  def pread(file, l) when is_pid(file) and is_list(l) do
    pread_int(file, l, [])
  end

  def pread(r_file_descriptor(module: module) = handle, l) when is_list(l) do
    module.pread(handle, l)
  end

  def pread(_, _) do
    {:error, :badarg}
  end

  defp pread_int(_File, [], r) do
    {:ok, :lists.reverse(r)}
  end

  defp pread_int(file, [{at, sz} | t], r)
       when is_integer(sz) and sz >= 0 do
    case pread(file, at, sz) do
      {:ok, data} ->
        pread_int(file, t, [data | r])

      :eof ->
        pread_int(file, t, [:eof | r])

      {:error, _} = error ->
        error
    end
  end

  defp pread_int(_, _, _) do
    {:error, :badarg}
  end

  def pread(file, at, sz)
      when is_pid(file) and
             is_integer(sz) and sz >= 0 do
    file_request(file, {:pread, at, sz})
  end

  def pread(r_file_descriptor(module: module) = handle, offs, sz)
      when is_integer(sz) and sz >= 0 do
    module.pread(handle, offs, sz)
  end

  def pread(_, _, _) do
    {:error, :badarg}
  end

  def write(file, bytes)
      when is_pid(file) or is_atom(file) do
    case make_binary(bytes) do
      bin when is_binary(bin) ->
        :io.request(file, {:put_chars, :latin1, bin})

      error ->
        error
    end
  end

  def write(r_file_descriptor(module: module) = handle, bytes) do
    module.write(handle, bytes)
  end

  def write(_, _) do
    {:error, :badarg}
  end

  def pwrite(file, l) when is_pid(file) and is_list(l) do
    pwrite_int(file, l, 0)
  end

  def pwrite(r_file_descriptor(module: module) = handle, l) when is_list(l) do
    module.pwrite(handle, l)
  end

  def pwrite(_, _) do
    {:error, :badarg}
  end

  defp pwrite_int(_File, [], _R) do
    :ok
  end

  defp pwrite_int(file, [{at, bytes} | t], r) do
    case pwrite(file, at, bytes) do
      :ok ->
        pwrite_int(file, t, r + 1)

      {:error, reason} ->
        {:error, {r, reason}}
    end
  end

  defp pwrite_int(_, _, _) do
    {:error, :badarg}
  end

  def pwrite(file, at, bytes) when is_pid(file) do
    file_request(file, {:pwrite, at, bytes})
  end

  def pwrite(r_file_descriptor(module: module) = handle, offs, bytes) do
    module.pwrite(handle, offs, bytes)
  end

  def pwrite(_, _, _) do
    {:error, :badarg}
  end

  def datasync(file) when is_pid(file) do
    file_request(file, :datasync)
  end

  def datasync(r_file_descriptor(module: module) = handle) do
    module.datasync(handle)
  end

  def datasync(_) do
    {:error, :badarg}
  end

  def sync(file) when is_pid(file) do
    file_request(file, :sync)
  end

  def sync(r_file_descriptor(module: module) = handle) do
    module.sync(handle)
  end

  def sync(_) do
    {:error, :badarg}
  end

  def position(file, at) when is_pid(file) do
    file_request(file, {:position, at})
  end

  def position(r_file_descriptor(module: module) = handle, at) do
    module.position(handle, at)
  end

  def position(_, _) do
    {:error, :badarg}
  end

  def truncate(file) when is_pid(file) do
    file_request(file, :truncate)
  end

  def truncate(r_file_descriptor(module: module) = handle) do
    module.truncate(handle)
  end

  def truncate(_) do
    {:error, :badarg}
  end

  def copy(source, dest) do
    copy_int(source, dest, :infinity)
  end

  def copy(source, dest, length)
      when (is_integer(length) and length >= 0) or
             is_atom(length) do
    copy_int(source, dest, length)
  end

  def copy(_, _, _) do
    {:error, :badarg}
  end

  defp copy_int(source, dest, length)
       when (is_pid(source) and
               is_pid(dest)) or
              (is_pid(source) and
                 elem(dest, 0) === :file_descriptor) or
              (elem(source, 0) === :file_descriptor and
                 is_pid(dest)) do
    copy_opened_int(source, dest, length, 0)
  end

  defp copy_int(
         r_file_descriptor(module: module) = source,
         r_file_descriptor(module: module) = dest,
         length
       ) do
    module.copy(source, dest, length)
  end

  defp copy_int(r_file_descriptor() = source, r_file_descriptor() = dest, length) do
    copy_opened_int(source, dest, length, 0)
  end

  defp copy_int({sourceName, sourceOpts}, {destName, destOpts}, length)
       when is_list(sourceOpts) and is_list(destOpts) do
    check_and_call(
      :copy,
      [file_name(sourceName), sourceOpts, file_name(destName), destOpts, length]
    )
  end

  defp copy_int({sourceName, sourceOpts}, dest, length)
       when (is_list(sourceOpts) and is_pid(dest)) or
              (is_list(sourceOpts) and
                 elem(dest, 0) === :file_descriptor) do
    case file_name(sourceName) do
      {:error, _} = error ->
        error

      source ->
        case open(source, [:read | sourceOpts]) do
          {:ok, handle} ->
            result = copy_opened_int(handle, dest, length, 0)
            _ = close(handle)
            result

          {:error, _} = error ->
            error
        end
    end
  end

  defp copy_int(source, {destName, destOpts}, length)
       when (is_pid(source) and is_list(destOpts)) or
              (elem(source, 0) === :file_descriptor and
                 is_list(destOpts)) do
    case file_name(destName) do
      {:error, _} = error ->
        error

      dest ->
        case open(dest, [:write | destOpts]) do
          {:ok, handle} ->
            case copy_opened_int(source, handle, length, 0) do
              {:ok, _} = oK ->
                case close(handle) do
                  :ok ->
                    oK

                  error ->
                    error
                end

              error ->
                _ = close(handle)
                error
            end

          {:error, _} = error ->
            error
        end
    end
  end

  defp copy_int(source, dest, length)
       when is_pid(source) or
              elem(source, 0) === :file_descriptor do
    copy_int(source, {dest, []}, length)
  end

  defp copy_int({_SourceName, sourceOpts} = source, dest, length)
       when is_list(sourceOpts) do
    copy_int(source, {dest, []}, length)
  end

  defp copy_int(source, dest, length)
       when is_pid(dest) or
              elem(dest, 0) === :file_descriptor do
    copy_int({source, []}, dest, length)
  end

  defp copy_int(source, {_DestName, destOpts} = dest, length)
       when is_list(destOpts) do
    copy_int({source, []}, dest, length)
  end

  defp copy_int(source, dest, length) do
    copy_int({source, []}, {dest, []}, length)
  end

  def copy_opened(source, dest, length)
      when (is_integer(length) and length >= 0) or
             is_atom(length) do
    copy_opened_int(source, dest, length)
  end

  def copy_opened(_, _, _) do
    {:error, :badarg}
  end

  defp copy_opened_int(source, dest, length)
       when is_pid(source) and
              is_pid(dest) do
    copy_opened_int(source, dest, length, 0)
  end

  defp copy_opened_int(source, dest, length)
       when is_pid(source) and
              elem(dest, 0) === :file_descriptor do
    copy_opened_int(source, dest, length, 0)
  end

  defp copy_opened_int(source, dest, length)
       when elem(source, 0) === :file_descriptor and
              is_pid(dest) do
    copy_opened_int(source, dest, length, 0)
  end

  defp copy_opened_int(source, dest, length)
       when elem(source, 0) === :file_descriptor and
              elem(dest, 0) === :file_descriptor do
    copy_opened_int(source, dest, length, 0)
  end

  defp copy_opened_int(_, _, _) do
    {:error, :badarg}
  end

  defp copy_opened_int(_, _, length, copied) when length <= 0 do
    {:ok, copied}
  end

  defp copy_opened_int(source, dest, length, copied) do
    n =
      cond do
        length > 65536 ->
          65536

        true ->
          length
      end

    case read(source, n) do
      {:ok, data} ->
        m =
          cond do
            is_binary(data) ->
              byte_size(data)

            is_list(data) ->
              length(data)
          end

        case write(dest, data) do
          :ok ->
            cond do
              m < n ->
                {:ok, copied + m}

              true ->
                newLength =
                  cond do
                    is_atom(length) ->
                      length

                    true ->
                      length - m
                  end

                copy_opened_int(source, dest, newLength, copied + m)
            end

          {:error, _} = error ->
            error
        end

      :eof ->
        {:ok, copied}

      {:error, _} = error ->
        error
    end
  end

  def ipread_s32bu_p32bu(file, pos, maxSize) when is_pid(file) do
    ipread_s32bu_p32bu_int(file, pos, maxSize)
  end

  def ipread_s32bu_p32bu(r_file_descriptor(module: module) = handle, pos, maxSize) do
    module.ipread_s32bu_p32bu(handle, pos, maxSize)
  end

  def ipread_s32bu_p32bu(_, _, _) do
    {:error, :badarg}
  end

  def ipread_s32bu_p32bu_int(file, pos, infinity) when is_atom(infinity) do
    ipread_s32bu_p32bu_int(file, pos, 1 <<< (31 - 1))
  end

  def ipread_s32bu_p32bu_int(file, pos, maxSize)
      when is_integer(maxSize) and
             maxSize >= 0 do
    cond do
      maxSize < 1 <<< 31 ->
        case pread(file, pos, 8) do
          {:ok, header} ->
            ipread_s32bu_p32bu_2(file, header, maxSize)

          error ->
            error
        end

      true ->
        {:error, :einval}
    end
  end

  def ipread_s32bu_p32bu_int(_File, _Pos, _MaxSize) do
    {:error, :badarg}
  end

  defp ipread_s32bu_p32bu_2(
         _File,
         <<0::size(32)-big-unsigned, pos::size(32)-big-unsigned>>,
         _MaxSize
       ) do
    {:ok, {0, pos, :eof}}
  end

  defp ipread_s32bu_p32bu_2(
         file,
         <<size::size(32)-big-unsigned, pos::size(32)-big-unsigned>>,
         maxSize
       )
       when size <= maxSize do
    case pread(file, pos, size) do
      {:ok, data} ->
        {:ok, {size, pos, data}}

      :eof ->
        {:ok, {size, pos, :eof}}

      error ->
        error
    end
  end

  defp ipread_s32bu_p32bu_2(_File, <<_::size(8)-binary>>, _MaxSize) do
    :eof
  end

  defp ipread_s32bu_p32bu_2(_File, <<_::binary>>, _MaxSize) do
    :eof
  end

  defp ipread_s32bu_p32bu_2(file, header, maxSize) when is_list(header) do
    ipread_s32bu_p32bu_2(file, :erlang.list_to_binary(header), maxSize)
  end

  def consult(file) do
    case open(file, [:read]) do
      {:ok, fd} ->
        r = consult_stream(fd)
        _ = close(fd)
        r

      error ->
        error
    end
  end

  def path_consult(path, file) do
    case path_open(path, file, [:read]) do
      {:ok, fd, full} ->
        case consult_stream(fd) do
          {:ok, list} ->
            _ = close(fd)
            {:ok, list, full}

          e1 ->
            _ = close(fd)
            e1
        end

      e2 ->
        e2
    end
  end

  def eval(file) do
    eval(file, :erl_eval.new_bindings())
  end

  def eval(file, bs) do
    case open(file, [:read]) do
      {:ok, fd} ->
        r = eval_stream(fd, :ignore, bs)
        _ = close(fd)
        r

      error ->
        error
    end
  end

  def path_eval(path, file) do
    path_eval(path, file, :erl_eval.new_bindings())
  end

  def path_eval(path, file, bs) do
    case path_open(path, file, [:read]) do
      {:ok, fd, full} ->
        case eval_stream(fd, :ignore, bs) do
          :ok ->
            _ = close(fd)
            {:ok, full}

          e1 ->
            _ = close(fd)
            e1
        end

      e2 ->
        e2
    end
  end

  def script(file) do
    script(file, :erl_eval.new_bindings())
  end

  def script(file, bs) do
    case open(file, [:read]) do
      {:ok, fd} ->
        r = eval_stream(fd, :return, bs)
        _ = close(fd)
        r

      error ->
        error
    end
  end

  def path_script(path, file) do
    path_script(path, file, :erl_eval.new_bindings())
  end

  def path_script(path, file, bs) do
    case path_open(path, file, [:read]) do
      {:ok, fd, full} ->
        case eval_stream(fd, :return, bs) do
          {:ok, r} ->
            _ = close(fd)
            {:ok, r, full}

          e1 ->
            _ = close(fd)
            e1
        end

      e2 ->
        e2
    end
  end

  def path_open(pathList, name, mode) do
    case file_name(name) do
      {:error, _} = error ->
        error

      fileName ->
        case :filename.pathtype(fileName) do
          :relative ->
            path_open_first(pathList, fileName, mode, :enoent)

          _ ->
            case open(name, mode) do
              {:ok, fd} ->
                {:ok, fd, name}

              error ->
                error
            end
        end
    end
  end

  def change_mode(name, mode) when is_integer(mode) do
    write_file_info(name, r_file_info(mode: mode))
  end

  def change_owner(name, ownerId) when is_integer(ownerId) do
    write_file_info(name, r_file_info(uid: ownerId))
  end

  def change_owner(name, ownerId, groupId)
      when is_integer(ownerId) and is_integer(groupId) do
    write_file_info(name, r_file_info(uid: ownerId, gid: groupId))
  end

  def change_group(name, groupId) when is_integer(groupId) do
    write_file_info(name, r_file_info(gid: groupId))
  end

  def change_time(name, {{y, m, d}, {h, min, sec}} = time)
      when is_integer(y) and is_integer(m) and
             is_integer(d) and is_integer(h) and is_integer(min) and
             is_integer(sec) do
    write_file_info(name, r_file_info(mtime: time))
  end

  def change_time(
        name,
        {{aY, aM, aD}, {aH, aMin, aSec}} = atime,
        {{mY, mM, mD}, {mH, mMin, mSec}} = mtime
      )
      when is_integer(aY) and is_integer(aM) and
             is_integer(aD) and is_integer(aH) and
             is_integer(aMin) and is_integer(aSec) and
             is_integer(mY) and is_integer(mM) and is_integer(mD) and
             is_integer(mH) and is_integer(mMin) and
             is_integer(mSec) do
    write_file_info(name, r_file_info(atime: atime, mtime: mtime))
  end

  def sendfile(file, _Sock, _Offet, _Bytes, _Opts)
      when is_pid(file) do
    {:error, :badarg}
  end

  def sendfile(file, sock, offset, bytes, []) do
    sendfile(file, sock, offset, bytes, 1 <<< 20, [], [], [])
  end

  def sendfile(file, sock, offset, bytes, opts) do
    try do
      :proplists.get_value(:chunk_size, opts, 1 <<< 20)
    catch
      :error, _ ->
        {:error, :badarg}
    else
      chunkSize0 when is_integer(chunkSize0) ->
        chunkSize = :erlang.min(chunkSize0, 1 <<< 20)
        sendfile(file, sock, offset, bytes, chunkSize, [], [], opts)

      _Other ->
        {:error, :badarg}
    end
  end

  def sendfile(filename, sock) do
    case :file.open(filename, [:read, :raw, :binary]) do
      {:error, reason} ->
        {:error, reason}

      {:ok, fd} ->
        res = sendfile(fd, sock, 0, 0, [])
        _ = :file.close(fd)
        res
    end
  end

  defp sendfile(
         r_file_descriptor(module: mod) = fd,
         sock,
         offset,
         bytes,
         chunkSize,
         headers,
         trailers,
         opts
       )
       when is_integer(offset) and is_integer(bytes) do
    case sock do
      {:"$inet", _, _} ->
        sendfile_fallback(fd, sock, offset, bytes, chunkSize, headers, trailers)

      _ when is_port(sock) ->
        case mod.sendfile(fd, sock, offset, bytes, chunkSize, headers, trailers, opts) do
          {:error, :enotsup} ->
            sendfile_fallback(fd, sock, offset, bytes, chunkSize, headers, trailers)

          else__ ->
            else__
        end
    end
  end

  defp sendfile(_, _, _, _, _, _, _, _) do
    {:error, :badarg}
  end

  defp sendfile_fallback(file, sock, offset, bytes, chunkSize, headers, trailers)
       when headers == [] or is_integer(headers) do
    case sendfile_fallback(file, sock, offset, bytes, chunkSize) do
      {:ok, bytesSent}
      when is_list(trailers) and
             trailers !== [] and is_integer(headers) ->
        sendfile_send(sock, trailers, bytesSent + headers)

      {:ok, bytesSent}
      when is_list(trailers) and
             trailers !== [] ->
        sendfile_send(sock, trailers, bytesSent)

      {:ok, bytesSent} when is_integer(headers) ->
        {:ok, bytesSent + headers}

      else__ ->
        else__
    end
  end

  defp sendfile_fallback(file, sock, offset, bytes, chunkSize, headers, trailers) do
    case sendfile_send(sock, headers, 0) do
      {:ok, bytesSent} ->
        sendfile_fallback(file, sock, offset, bytes, chunkSize, bytesSent, trailers)

      else__ ->
        else__
    end
  end

  defp sendfile_fallback(file, sock, offset, bytes, chunkSize)
       when 0 <= bytes do
    {:ok, currPos} = :file.position(file, {:cur, 0})

    case :file.position(file, {:bof, offset}) do
      {:ok, _NewPos} ->
        res = sendfile_fallback_int(file, sock, bytes, chunkSize, 0)
        _ = :file.position(file, {:bof, currPos})
        res

      error ->
        error
    end
  end

  defp sendfile_fallback(_, _, _, _, _) do
    {:error, :einval}
  end

  defp sendfile_fallback_int(file, sock, bytes, chunkSize, bytesSent)
       when bytes > bytesSent or bytes == 0 do
    size =
      cond do
        bytes == 0 ->
          chunkSize

        bytes - bytesSent < chunkSize ->
          bytes - bytesSent

        true ->
          chunkSize
      end

    case :file.read(file, size) do
      {:ok, data} ->
        case sendfile_send(sock, data, bytesSent) do
          {:ok, newBytesSent} ->
            sendfile_fallback_int(file, sock, bytes, chunkSize, newBytesSent)

          error ->
            error
        end

      :eof ->
        {:ok, bytesSent}

      error ->
        error
    end
  end

  defp sendfile_fallback_int(_File, _Sock, bytesSent, _ChunkSize, bytesSent) do
    {:ok, bytesSent}
  end

  defp sendfile_send(sock, data, old) do
    len = :erlang.iolist_size(data)

    case :gen_tcp.send(sock, data) do
      :ok ->
        {:ok, len + old}

      else__ ->
        else__
    end
  end

  defp consult_stream(fd) do
    _ = :epp.set_encoding(fd)
    consult_stream(fd, 1, [])
  end

  defp consult_stream(fd, line, acc) do
    case :io.read(fd, :"", line) do
      {:ok, term, endLine} ->
        consult_stream(fd, endLine, [term | acc])

      {:error, error, _Line} ->
        {:error, error}

      {:eof, _Line} ->
        {:ok, :lists.reverse(acc)}
    end
  end

  defp eval_stream(fd, handling, bs) do
    _ = :epp.set_encoding(fd)
    eval_stream(fd, handling, 1, :undefined, [], bs)
  end

  defp eval_stream(fd, h, line, last, e, bs) do
    eval_stream2(:io.parse_erl_exprs(fd, :"", line), fd, h, last, e, bs)
  end

  defp eval_stream2({:ok, form, endLine}, fd, h, last, e, bs0) do
    try do
      :erl_eval.exprs(form, bs0)
    catch
      class, reason ->
        error = {endLine, :file, {class, reason, __STACKTRACE__}}
        eval_stream(fd, h, endLine, last, [error | e], bs0)
    else
      {:value, v, bs} ->
        eval_stream(fd, h, endLine, {v}, e, bs)
    end
  end

  defp eval_stream2({:error, what, endLine}, fd, h, last, e, bs) do
    eval_stream(fd, h, endLine, last, [what | e], bs)
  end

  defp eval_stream2({:eof, endLine}, _Fd, h, last, e, _Bs) do
    case {h, last, e} do
      {:return, {val}, []} ->
        {:ok, val}

      {:return, :undefined, ^e} ->
        {:error,
         hd(
           :lists.reverse(
             e,
             [{endLine, :file, :undefined_script}]
           )
         )}

      {:ignore, _, []} ->
        :ok

      {_, _, [_ | _] = ^e} ->
        {:error, hd(:lists.reverse(e))}
    end
  end

  defp path_open_first([path | rest], name, mode, lastError) do
    case file_name(path) do
      {:error, _} = error ->
        error

      filePath ->
        fileName = fname_join(filePath, name)

        case open(fileName, mode) do
          {:ok, fd} ->
            {:ok, fd, fileName}

          {:error, reason}
          when reason === :enoent or
                 reason === :enotdir ->
            path_open_first(rest, name, mode, lastError)

          error ->
            error
        end
    end
  end

  defp path_open_first([], _Name, _Mode, lastError) do
    {:error, lastError}
  end

  defp fname_join('.', name) do
    name
  end

  defp fname_join(dir, name) do
    :filename.join(dir, name)
  end

  defp file_name(n) when is_binary(n) do
    n
  end

  defp file_name(n) do
    try do
      file_name_1(n, :file.native_name_encoding())
    catch
      reason ->
        {:error, reason}
    end
  end

  defp file_name_1([c | t], :latin1)
       when is_integer(c) and
              c < 256 do
    [c | file_name_1(t, :latin1)]
  end

  defp file_name_1([c | t], :utf8) when is_integer(c) do
    [c | file_name_1(t, :utf8)]
  end

  defp file_name_1([h | t], e) do
    file_name_1(h, e) ++ file_name_1(t, e)
  end

  defp file_name_1([], _) do
    []
  end

  defp file_name_1(n, _) when is_atom(n) do
    :erlang.atom_to_list(n)
  end

  defp file_name_1(_, _) do
    throw(:badarg)
  end

  defp make_binary(bin) when is_binary(bin) do
    bin
  end

  defp make_binary(list) do
    try do
      :erlang.iolist_to_binary(list)
    catch
      :error, reason ->
        {:error, reason}
    end
  end

  defp mode_list(:read) do
    [:read]
  end

  defp mode_list(:write) do
    [:write]
  end

  defp mode_list(:read_write) do
    [:read, :write]
  end

  defp mode_list({:binary, mode}) when is_atom(mode) do
    [:binary | mode_list(mode)]
  end

  defp mode_list({:character, mode}) when is_atom(mode) do
    mode_list(mode)
  end

  defp mode_list(_) do
    [{:error, :badarg}]
  end

  defp call(command, args) when is_list(args) do
    x = :erlang.dt_spread_tag(true)
    y = :gen_server.call(:file_server_2, :erlang.list_to_tuple([command | args]), :infinity)
    :erlang.dt_restore_tag(x)
    y
  end

  defp check_and_call(command, args) when is_list(args) do
    case check_args(args) do
      :ok ->
        call(command, args)

      error ->
        error
    end
  end

  defp check_args([{:error, _} = error | _Rest]) do
    error
  end

  defp check_args([_Name | rest]) do
    check_args(rest)
  end

  defp check_args([]) do
    :ok
  end

  defp file_request(io, request) do
    ref = :erlang.monitor(:process, io)
    send(io, {:file_request, self(), ref, request})

    receive do
      {:file_reply, ^ref, reply} ->
        :erlang.demonitor(ref, [:flush])
        reply

      {:DOWN, ^ref, _, _, _} ->
        {:error, :terminated}
    end
  end
end
