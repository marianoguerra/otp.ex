defmodule :m_prim_file do
  use Bitwise

  def internal_name2native(_) do
    :erlang.nif_error(:undefined)
  end

  def internal_native2name(_) do
    :erlang.nif_error(:undefined)
  end

  def internal_normalize_utf8(_) do
    :erlang.nif_error(:undefined)
  end

  def is_translatable(_) do
    :erlang.nif_error(:undefined)
  end

  def start() do
    helper_loop()
  end

  defp helper_loop() do
    receive do
      {:close, fRef} when is_reference(fRef) ->
        delayed_close_nif(fRef)

      _ ->
        :ok
    end

    helper_loop()
  end

  def on_load() do
    pid = :erts_internal.spawn_system_process(:prim_file, :start, [])

    :ok =
      :erlang.load_nif(
        :erlang.atom_to_list(:prim_file),
        pid
      )
  end

  def copy(
        r_file_descriptor(module: :prim_file) = source,
        r_file_descriptor(module: :prim_file) = dest,
        length
      )
      when (is_integer(length) and length >= 0) or
             is_atom(length) do
    :file.copy_opened(source, dest, length)
  end

  def open(name, modes) do
    try do
      open_nif(encode_path(name), modes)
    catch
      :error, :badarg ->
        {:error, :badarg}
    else
      {:ok, ref} ->
        {:ok, make_fd(ref, modes)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp make_fd(fRef, modes) do
    r_file_descriptor(module: :prim_file, data: build_fd_data(fRef, modes))
  end

  def close(fd) do
    try do
      %{:handle => fRef} = get_fd_data(fd)
      close_nif(fRef)
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def read(fd, size) do
    try do
      %{:handle => fRef, :r_ahead_size => rASz, :r_buffer => rBuf} = get_fd_data(fd)
      read_1(fRef, rBuf, :prim_buffer.size(rBuf), rASz, size)
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  defp read_1(_FRef, rBuf, rBufSz, _RASz, rSz)
       when rBufSz >= rSz do
    {:ok, :prim_buffer.read(rBuf, rSz)}
  end

  defp read_1(fRef, rBuf, rBufSz, rASz, rSz)
       when rBufSz > 0 do
    buffered = :prim_buffer.read(rBuf, rBufSz)

    case read_1(fRef, rBuf, 0, rASz, rSz - rBufSz) do
      {:ok, data} ->
        {:ok, <<buffered::binary, data::binary>>}

      :eof ->
        {:ok, buffered}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp read_1(fRef, rBuf, rBufSz, rASz, rSz)
       when rBufSz === 0 do
    case read_nif(fRef, rASz + rSz) do
      {:ok, data} when byte_size(data) > rSz ->
        {first, rest} = :erlang.split_binary(data, rSz)
        :prim_buffer.write(rBuf, [rest])
        {:ok, first}

      {:ok, data} when byte_size(data) <= rSz ->
        {:ok, data}

      :eof ->
        :eof

      {:error, reason} ->
        {:error, reason}
    end
  end

  def read_line(fd) do
    try do
      %{:handle => fRef, :r_ahead_size => rASz, :r_buffer => rBuf} = get_fd_data(fd)
      searchResult = :prim_buffer.find_byte_index(rBuf, ?\n)
      lineSize = max(256, rASz)
      read_line_1(fRef, rBuf, searchResult, lineSize)
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  defp read_line_1(fRef, rBuf, :not_found, lineSize) do
    case read_nif(fRef, lineSize) do
      {:ok, data} ->
        :prim_buffer.write(rBuf, [data])
        searchResult = :prim_buffer.find_byte_index(rBuf, ?\n)
        read_line_1(fRef, rBuf, searchResult, lineSize)

      :eof ->
        case :prim_buffer.size(rBuf) do
          size when size > 0 ->
            {:ok, :prim_buffer.read(rBuf, size)}

          size when size === 0 ->
            :eof
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp read_line_1(_FRef, rBuf, {:ok, lFIndex}, _LineSize) do
    cRIndex = lFIndex - 1

    case :prim_buffer.read(rBuf, lFIndex + 1) do
      <<line::size(cRIndex)-binary, "\r\n">> ->
        {:ok, <<line::binary, "\n">>}

      line ->
        {:ok, line}
    end
  end

  def write(fd, iOData) do
    try do
      %{:handle => fRef} = get_fd_data(fd)
      reset_write_position(fd)
      write_1(fRef, :erlang.iolist_to_iovec(iOData))
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  defp write_1(fRef, iOVec) do
    case write_nif(fRef, iOVec) do
      {:continue, remainder} ->
        write_1(fRef, remainder)

      :ok ->
        :ok

      {:error, reason} ->
        {:error, reason}
    end
  end

  def truncate(fd) do
    try do
      %{:handle => fRef} = get_fd_data(fd)
      reset_write_position(fd)
      truncate_nif(fRef)
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def advise(fd, offset, length, advise) do
    try do
      %{:handle => fRef} = get_fd_data(fd)
      advise_nif(fRef, offset, length, advise)
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def allocate(fd, offset, length) do
    try do
      %{:handle => fRef} = get_fd_data(fd)
      allocate_nif(fRef, offset, length)
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def sync(fd) do
    try do
      %{:handle => fRef} = get_fd_data(fd)
      sync_nif(fRef, 0)
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def datasync(fd) do
    try do
      %{:handle => fRef} = get_fd_data(fd)
      sync_nif(fRef, 1)
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def position(fd, {:cur, offset}) do
    try do
      %{:r_buffer => rBuf} = get_fd_data(fd)
      position_1(fd, :cur, offset - :prim_buffer.size(rBuf))
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def position(fd, {mark, offset}) do
    try do
      position_1(fd, mark, offset)
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def position(fd, :cur) do
    position(fd, {:cur, 0})
  end

  def position(fd, :bof) do
    position(fd, {:bof, 0})
  end

  def position(fd, :eof) do
    position(fd, {:eof, 0})
  end

  def position(fd, offset) do
    position(fd, {:bof, offset})
  end

  defp position_1(fd, mark, offset) do
    %{:handle => fRef, :r_buffer => rBuf} = get_fd_data(fd)
    :prim_buffer.wipe(rBuf)
    seek_nif(fRef, mark, offset)
  end

  def pread(fd, offset, size) do
    try do
      %{:handle => fRef} = get_fd_data(fd)
      pread_nif(fRef, offset, size)
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def pread(fd, locNums) do
    try do
      %{:handle => fRef} = get_fd_data(fd)
      pread_list(fRef, locNums, [])
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  defp pread_list(_FRef, [], resultList) do
    {:ok, reverse_list(resultList)}
  end

  defp pread_list(fRef, [{offset, size} | rest], resultList) do
    case pread_nif(fRef, offset, size) do
      {:ok, data} ->
        pread_list(fRef, rest, [data | resultList])

      :eof ->
        pread_list(fRef, rest, [:eof | resultList])

      {:error, reason} ->
        {:error, reason}
    end
  end

  def pwrite(fd, offset, iOData) do
    try do
      %{:handle => fRef, :r_buffer => rBuf} = get_fd_data(fd)
      :prim_buffer.wipe(rBuf)
      pwrite_plain(fRef, offset, :erlang.iolist_to_iovec(iOData))
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  defp pwrite_plain(fRef, offset, iOVec) do
    case pwrite_nif(fRef, offset, iOVec) do
      {:continue, bytesWritten, remainder} ->
        pwrite_plain(fRef, offset + bytesWritten, remainder)

      :ok ->
        :ok

      {:error, reason} ->
        {:error, reason}
    end
  end

  def pwrite(fd, locBytes) do
    try do
      %{:handle => fRef, :r_buffer => rBuf} = get_fd_data(fd)
      :prim_buffer.wipe(rBuf)
      pwrite_list(fRef, locBytes, 0)
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  defp pwrite_list(_FRef, [], _Successes) do
    :ok
  end

  defp pwrite_list(fRef, [{offset, iOData} | rest], successes) do
    case pwrite_plain(fRef, offset, :erlang.iolist_to_iovec(iOData)) do
      {:error, reason} ->
        {:error, {successes, reason}}

      :ok ->
        pwrite_list(fRef, rest, successes + 1)
    end
  end

  def sendfile(fd, socket, offset, bytes, _ChunkSize, [], [], _Flags) do
    try do
      advise(fd, offset, bytes, :sequential)
      :prim_inet.sendfile(socket, get_handle(fd), offset, bytes)
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def sendfile(_Fd, _Socket, _Offset, _Bytes, _ChunkSize, _Headers, _Trailers, _Flags) do
    {:error, :enotsup}
  end

  def ipread_s32bu_p32bu(fd, offset, infinity) when is_atom(infinity) do
    ipread_s32bu_p32bu(fd, offset, 1 <<< (31 - 1))
  end

  def ipread_s32bu_p32bu(fd, offset, maxSize)
      when is_integer(offset) and
             is_integer(maxSize) do
    try do
      %{:handle => fRef} = get_fd_data(fd)
      ipread_s32bu_p32bu_nif(fRef, offset, maxSize)
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def ipread_s32bu_p32bu(_Fd, _Offset, _MaxSize) do
    {:error, :badarg}
  end

  defp ipread_s32bu_p32bu_nif(_FRef, _Offset, _MaxSize) do
    :erlang.nif_error(:undef)
  end

  def get_handle(fd) do
    try do
      %{:handle => fRef} = get_fd_data(fd)
      get_handle_nif(fRef)
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  defp reset_write_position(fd) do
    %{:r_buffer => rBuf} = r_file_descriptor(fd, :data)

    case :prim_buffer.size(rBuf) do
      size when size > 0 ->
        position(fd, :cur)

      size when size === 0 ->
        :ok
    end
  end

  defp get_fd_data(r_file_descriptor(data: data)) do
    %{:owner => owner} = data

    case self() do
      ^owner ->
        data

      _ ->
        :erlang.error(:not_on_controlling_process)
    end
  end

  defp build_fd_data(fRef, modes) do
    defaults = %{
      :owner => self(),
      :handle => fRef,
      :r_ahead_size => 0,
      :r_buffer => :prim_buffer.new()
    }

    fill_fd_option_map(modes, defaults)
  end

  defp fill_fd_option_map([], map) do
    map
  end

  defp fill_fd_option_map([:read_ahead | modes], map) do
    fill_fd_option_map(
      [{:read_ahead, 64 <<< 10} | modes],
      map
    )
  end

  defp fill_fd_option_map([{:read_ahead, size} | modes], map) do
    fill_fd_option_map(
      modes,
      %{map | :r_ahead_size => size}
    )
  end

  defp fill_fd_option_map([_Ignored | modes], map) do
    fill_fd_option_map(modes, map)
  end

  defp open_nif(_Name, _Modes) do
    :erlang.nif_error(:undef)
  end

  defp close_nif(_FileRef) do
    :erlang.nif_error(:undef)
  end

  defp read_nif(_FileRef, _Size) do
    :erlang.nif_error(:undef)
  end

  defp write_nif(_FileRef, _IOVec) do
    :erlang.nif_error(:undef)
  end

  defp pread_nif(_FileRef, _Offset, _Size) do
    :erlang.nif_error(:undef)
  end

  defp pwrite_nif(_FileRef, _Offset, _IOVec) do
    :erlang.nif_error(:undef)
  end

  defp seek_nif(_FileRef, _Mark, _Offset) do
    :erlang.nif_error(:undef)
  end

  defp sync_nif(_FileRef, _DataOnly) do
    :erlang.nif_error(:undef)
  end

  defp advise_nif(_FileRef, _Offset, _Length, _Advise) do
    :erlang.nif_error(:undef)
  end

  defp allocate_nif(_FileRef, _Offset, _Length) do
    :erlang.nif_error(:undef)
  end

  defp truncate_nif(_FileRef) do
    :erlang.nif_error(:undef)
  end

  defp get_handle_nif(_FileRef) do
    :erlang.nif_error(:undef)
  end

  defp delayed_close_nif(_FileRef) do
    :erlang.nif_error(:undef)
  end

  defp read_handle_info_nif(_FileRef) do
    :erlang.nif_error(:undef)
  end

  def read_file(filename) do
    try do
      read_file_nif(encode_path(filename))
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  defp read_file_nif(_Filename) do
    :erlang.nif_error(:undef)
  end

  def write_file(filename, bytes) do
    write_file(filename, bytes, [])
  end

  defp write_file(filename, bytes, modes) do
    case open(filename, [[:write, :binary] | modes]) do
      {:ok, fd} ->
        result = write(fd, bytes)
        close(fd)
        result

      {:error, reason} ->
        {:error, reason}
    end
  end

  def read_link(name) do
    read_link_1(name, false)
  end

  def read_link_all(name) do
    read_link_1(name, true)
  end

  defp read_link_1(name, acceptRawNames) do
    try do
      read_link_nif(encode_path(name))
    catch
      :error, :badarg ->
        {:error, :badarg}
    else
      {:ok, rawName} ->
        translate_raw_name(rawName, acceptRawNames)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp translate_raw_name(rawName, silentFailure) do
    case decode_path(rawName) do
      converted when is_list(converted) ->
        {:ok, converted}

      {:error, _Reason} when silentFailure === false ->
        {:error, :einval}

      {:error, _Reason} when silentFailure === true ->
        {:ok, rawName}
    end
  end

  def list_dir(name) do
    list_dir_1(name, true)
  end

  def list_dir_all(name) do
    list_dir_1(name, false)
  end

  defp list_dir_1(name, skipInvalid) do
    try do
      list_dir_nif(encode_path(name))
    catch
      :error, :badarg ->
        {:error, :badarg}
    else
      {:ok, rawNames} ->
        list_dir_convert(rawNames, skipInvalid, [])

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp list_dir_convert([], _SkipInvalid, result) do
    {:ok, result}
  end

  defp list_dir_convert([rawName | rest], skipInvalid, result) do
    case decode_path(rawName) do
      converted when is_list(converted) ->
        list_dir_convert(rest, skipInvalid, [converted | result])

      {:error, _} when skipInvalid === false ->
        list_dir_convert(rest, skipInvalid, [rawName | result])

      {:error, :ignore} ->
        list_dir_convert(rest, skipInvalid, result)

      {:error, :warning} ->
        try do
          send(
            :logger,
            {:log, :warning, 'Non-unicode filename ~p ignored\n', [rawName],
             %{
               :pid => self(),
               :gl => :erlang.group_leader(),
               :time => :os.system_time(:microsecond),
               :error_logger => %{:tag => :warning_msg}
             }}
          )
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

        list_dir_convert(rest, skipInvalid, result)

      {:error, _} ->
        {:error, {:no_translation, rawName}}
    end
  end

  def read_file_info(filename) do
    read_info_1(filename, 1, :local)
  end

  def read_file_info(filename, opts) do
    read_info_1(filename, 1, proplist_get_value(:time, opts, :local))
  end

  def read_link_info(name) do
    read_info_1(name, 0, :local)
  end

  def read_link_info(name, opts) do
    read_info_1(name, 0, proplist_get_value(:time, opts, :local))
  end

  defp read_info_1(name, followLinks, timeType) do
    try do
      case read_info_nif(encode_path(name), followLinks) do
        {:error, reason} ->
          {:error, reason}

        fileInfo ->
          {:ok, adjust_times(fileInfo, timeType)}
      end
    catch
      :error, _ ->
        {:error, :badarg}
    end
  end

  def read_handle_info(fd) do
    read_handle_info_1(fd, :local)
  end

  def read_handle_info(fd, opts) do
    read_handle_info_1(
      fd,
      proplist_get_value(:time, opts, :local)
    )
  end

  defp read_handle_info_1(fd, timeType) do
    try do
      %{:handle => fRef} = get_fd_data(fd)

      case read_handle_info_nif(fRef) do
        {:error, reason} ->
          {:error, reason}

        fileInfo ->
          {:ok, adjust_times(fileInfo, timeType)}
      end
    catch
      :error, _ ->
        {:error, :badarg}
    end
  end

  defp adjust_times(fileInfo, timeType) do
    cTime =
      from_posix_seconds(
        r_file_info(fileInfo, :ctime),
        timeType
      )

    mTime =
      from_posix_seconds(
        r_file_info(fileInfo, :mtime),
        timeType
      )

    aTime =
      from_posix_seconds(
        r_file_info(fileInfo, :atime),
        timeType
      )

    r_file_info(fileInfo, ctime: cTime, mtime: mTime, atime: aTime)
  end

  def write_file_info(filename, info) do
    write_file_info_1(filename, info, :local)
  end

  def write_file_info(filename, info, opts) do
    write_file_info_1(filename, info, proplist_get_value(:time, opts, :local))
  end

  defp write_file_info_1(filename, info, timeType) do
    r_file_info(mode: modes, uid: uid, gid: gid, atime: aTime0, mtime: mTime0, ctime: cTime0) =
      info

    try do
      aTime = file_info_convert_atime(aTime0, timeType)
      mTime = file_info_convert_mtime(mTime0, aTime, timeType)
      cTime = file_info_convert_ctime(cTime0, mTime, timeType)
      encodedName = encode_path(filename)
      throw_on_error(set_owner(encodedName, uid, gid))
      throw_on_error(set_permissions(encodedName, modes))
      throw_on_error(set_time(encodedName, aTime, mTime, cTime))
    catch
      reason ->
        {:error, reason}

      :error, _ ->
        {:error, :badarg}
    end
  end

  defp set_owner(encodedName, uid, :undefined) do
    set_owner(encodedName, uid, -1)
  end

  defp set_owner(encodedName, :undefined, gid) do
    set_owner(encodedName, -1, gid)
  end

  defp set_owner(encodedName, uid, gid) do
    set_owner_nif(encodedName, uid, gid)
  end

  defp set_owner_nif(_Path, _Uid, _Gid) do
    :erlang.nif_error(:undef)
  end

  defp set_permissions(_EncodedName, :undefined) do
    :ok
  end

  defp set_permissions(encodedName, permissions) do
    set_permissions_nif(encodedName, permissions)
  end

  defp set_permissions_nif(_Path, _Permissions) do
    :erlang.nif_error(:undef)
  end

  defp set_time(encodedName, aTime, mTime, cTime) do
    set_time_nif(encodedName, aTime, mTime, cTime)
  end

  defp set_time_nif(_Path, _ATime, _MTime, _CTime) do
    :erlang.nif_error(:undef)
  end

  defp throw_on_error(:ok) do
    :ok
  end

  defp throw_on_error({:error, :enotsup}) do
    :ok
  end

  defp throw_on_error({:error, reason}) do
    throw(reason)
  end

  defp file_info_convert_atime(aTime, timeType) when aTime !== :undefined do
    to_posix_seconds(aTime, timeType)
  end

  defp file_info_convert_atime(:undefined, :local) do
    to_posix_seconds(:erlang.localtime(), :local)
  end

  defp file_info_convert_atime(:undefined, :universal) do
    to_posix_seconds(:erlang.universaltime(), :universal)
  end

  defp file_info_convert_atime(:undefined, :posix) do
    :erlang.universaltime_to_posixtime(:erlang.universaltime())
  end

  defp file_info_convert_mtime(:undefined, aTime, _TimeType) do
    aTime
  end

  defp file_info_convert_mtime(mTime, _ATime, timeType) do
    to_posix_seconds(mTime, timeType)
  end

  defp file_info_convert_ctime(:undefined, mTime, _TimeType) do
    mTime
  end

  defp file_info_convert_ctime(cTime, _MTime, timeType) do
    to_posix_seconds(cTime, timeType)
  end

  def get_cwd([letter, ?:])
      when letter >= ?A and
             letter <= ?Z do
    get_dcwd(letter - ?A + 1)
  end

  def get_cwd([letter, ?:])
      when letter >= ?a and
             letter <= ?z do
    get_dcwd(letter - ?a + 1)
  end

  def get_cwd([_ | _]) do
    {:error, :einval}
  end

  def get_cwd(_) do
    {:error, :badarg}
  end

  defp get_dcwd(index) do
    try do
      get_device_cwd_nif(index)
    catch
      :error, :badarg ->
        {:error, :badarg}
    else
      {:ok, rawPath} ->
        {:ok, decode_path(rawPath)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  def get_cwd() do
    try do
      get_cwd_nif()
    catch
      :error, :badarg ->
        {:error, :badarg}
    else
      {:ok, rawPath} ->
        {:ok, decode_path(rawPath)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  def set_cwd(path) do
    try do
      case is_path_translatable(path) do
        true ->
          set_cwd_nif(encode_path(path))

        false ->
          {:error, :no_translation}
      end
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def delete(path) do
    try do
      del_file_nif(encode_path(path))
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def rename(source, destination) do
    try do
      rename_nif(
        encode_path(source),
        encode_path(destination)
      )
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def make_dir(path) do
    try do
      make_dir_nif(encode_path(path))
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def del_dir(path) do
    try do
      del_dir_nif(encode_path(path))
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def make_link(existing, new) do
    try do
      make_hard_link_nif(
        encode_path(existing),
        encode_path(new)
      )
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def make_symlink(existing, new) do
    try do
      make_soft_link_nif(
        encode_path(existing),
        encode_path(new)
      )
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def altname(path) do
    try do
      altname_nif(encode_path(path))
    catch
      :error, :badarg ->
        {:error, :badarg}
    else
      {:ok, rawPath} ->
        {:ok, decode_path(rawPath)}

      other ->
        other
    end
  end

  defp list_dir_nif(_Path) do
    :erlang.nif_error(:undef)
  end

  defp read_link_nif(_Path) do
    :erlang.nif_error(:undef)
  end

  defp read_info_nif(_Path, _FollowLinks) do
    :erlang.nif_error(:undef)
  end

  defp make_hard_link_nif(_Existing, _New) do
    :erlang.nif_error(:undef)
  end

  defp make_soft_link_nif(_Existing, _New) do
    :erlang.nif_error(:undef)
  end

  defp rename_nif(_Source, _Destination) do
    :erlang.nif_error(:undef)
  end

  defp make_dir_nif(_Path) do
    :erlang.nif_error(:undef)
  end

  defp del_file_nif(_Path) do
    :erlang.nif_error(:undef)
  end

  defp del_dir_nif(_Path) do
    :erlang.nif_error(:undef)
  end

  defp get_device_cwd_nif(_DevicePath) do
    :erlang.nif_error(:undef)
  end

  defp set_cwd_nif(_Path) do
    :erlang.nif_error(:undef)
  end

  defp get_cwd_nif() do
    :erlang.nif_error(:undef)
  end

  defp altname_nif(_Path) do
    :erlang.nif_error(:undef)
  end

  defp reverse_list(list) do
    :lists.reverse(list, [])
  end

  defp proplist_get_value(_Key, [], default) do
    default
  end

  defp proplist_get_value(key, [{key, value} | _Rest], _Default) do
    value
  end

  defp proplist_get_value(key, [key | _Rest], _Default) do
    true
  end

  defp proplist_get_value(key, [_Other | rest], default) do
    proplist_get_value(key, rest, default)
  end

  defp encode_path(path) do
    :prim_file.internal_name2native(path)
  end

  defp decode_path(nativePath) when is_binary(nativePath) do
    :prim_file.internal_native2name(nativePath)
  end

  defp is_path_translatable(path) when is_list(path) do
    true
  end

  defp is_path_translatable(path) do
    :prim_file.is_translatable(path)
  end

  defp from_posix_seconds(seconds, :posix) when is_integer(seconds) do
    seconds
  end

  defp from_posix_seconds(seconds, :universal) when is_integer(seconds) do
    :erlang.posixtime_to_universaltime(seconds)
  end

  defp from_posix_seconds(seconds, :local) when is_integer(seconds) do
    :erlang.universaltime_to_localtime(:erlang.posixtime_to_universaltime(seconds))
  end

  defp to_posix_seconds(seconds, :posix) when is_integer(seconds) do
    seconds
  end

  defp to_posix_seconds({_, _} = datetime, :universal) do
    :erlang.universaltime_to_posixtime(datetime)
  end

  defp to_posix_seconds({_, _} = datetime, :local) do
    :erlang.universaltime_to_posixtime(:erlang.localtime_to_universaltime(datetime))
  end
end
