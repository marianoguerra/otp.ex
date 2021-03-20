defmodule :m_logger_std_h do
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

  def filesync(name) do
    :logger_h_common.filesync(:logger_std_h, name)
  end

  def adding_handler(config) do
    :logger_h_common.adding_handler(config)
  end

  def changing_config(setOrUpdate, oldConfig, newConfig) do
    :logger_h_common.changing_config(setOrUpdate, oldConfig, newConfig)
  end

  def removing_handler(config) do
    :logger_h_common.removing_handler(config)
  end

  def log(logEvent, config) do
    :logger_h_common.log(logEvent, config)
  end

  def filter_config(config) do
    :logger_h_common.filter_config(config)
  end

  def init(name, config) do
    myConfig =
      :maps.with(
        [:type, :file, :modes, :file_check, :max_no_bytes, :max_no_files, :compress_on_rotate],
        config
      )

    case file_ctrl_start(name, myConfig) do
      {:ok, fileCtrlPid} ->
        {:ok, Map.put(myConfig, :file_ctrl_pid, fileCtrlPid)}

      error ->
        error
    end
  end

  def check_config(name, :set, :undefined, newHConfig) do
    check_h_config(
      merge_default_config(
        name,
        normalize_config(newHConfig)
      )
    )
  end

  def check_config(name, setOrUpdate, oldHConfig, newHConfig0) do
    writeOnce =
      :maps.with(
        [:type, :file, :modes],
        oldHConfig
      )

    default =
      case setOrUpdate do
        :set ->
          merge_default_config(name, writeOnce)

        :update ->
          oldHConfig
      end

    newHConfig =
      :maps.merge(
        default,
        normalize_config(newHConfig0)
      )

    case :maps.with([:type, :file, :modes], newHConfig) do
      ^writeOnce ->
        check_h_config(newHConfig)

      other ->
        {:error, {:illegal_config_change, :logger_std_h, writeOnce, other}}
    end
  end

  defp check_h_config(hConfig) do
    case check_h_config(
           :maps.get(:type, hConfig),
           :maps.to_list(hConfig)
         ) do
      :ok ->
        {:ok, fix_file_opts(hConfig)}

      {:error, {key, value}} ->
        {:error, {:invalid_config, :logger_std_h, %{key => value}}}
    end
  end

  defp check_h_config(type, [{:type, type} | config])
       when type === :standard_io or
              type === :standard_error or type === :file do
    check_h_config(type, config)
  end

  defp check_h_config(
         {:device, device},
         [{:type, {:device, device}} | config]
       ) do
    check_h_config({:device, device}, config)
  end

  defp check_h_config(:file, [{:file, file} | config])
       when is_list(file) do
    check_h_config(:file, config)
  end

  defp check_h_config(:file, [{:modes, modes} | config])
       when is_list(modes) do
    check_h_config(:file, config)
  end

  defp check_h_config(:file, [{:max_no_bytes, size} | config])
       when (is_integer(size) and size > 0) or size === :infinity do
    check_h_config(:file, config)
  end

  defp check_h_config(:file, [{:max_no_files, num} | config])
       when is_integer(num) and num >= 0 do
    check_h_config(:file, config)
  end

  defp check_h_config(:file, [{:compress_on_rotate, bool} | config])
       when is_boolean(bool) do
    check_h_config(:file, config)
  end

  defp check_h_config(:file, [{:file_check, fileCheck} | config])
       when is_integer(fileCheck) and fileCheck >= 0 do
    check_h_config(:file, config)
  end

  defp check_h_config(_Type, [other | _]) do
    {:error, other}
  end

  defp check_h_config(_Type, []) do
    :ok
  end

  defp normalize_config(%{type: {:file, file}} = hConfig) do
    normalize_config(Map.merge(hConfig, %{type: :file, file: file}))
  end

  defp normalize_config(%{type: {:file, file, modes}} = hConfig) do
    normalize_config(Map.merge(hConfig, %{type: :file, file: file, modes: modes}))
  end

  defp normalize_config(%{file: file} = hConfig) do
    Map.put(hConfig, :file, :filename.absname(file))
  end

  defp normalize_config(hConfig) do
    hConfig
  end

  defp merge_default_config(name, %{type: type} = hConfig) do
    merge_default_config(name, type, hConfig)
  end

  defp merge_default_config(name, %{file: _} = hConfig) do
    merge_default_config(name, :file, hConfig)
  end

  defp merge_default_config(name, hConfig) do
    merge_default_config(name, :standard_io, hConfig)
  end

  defp merge_default_config(name, type, hConfig) do
    :maps.merge(get_default_config(name, type), hConfig)
  end

  defp get_default_config(name, :file) do
    %{
      type: :file,
      file: :filename.absname(:erlang.atom_to_list(name)),
      modes: [:raw, :append],
      file_check: 0,
      max_no_bytes: :infinity,
      max_no_files: 0,
      compress_on_rotate: false
    }
  end

  defp get_default_config(_Name, type) do
    %{type: type}
  end

  defp fix_file_opts(%{modes: modes} = hConfig) do
    Map.put(hConfig, :modes, fix_modes(modes))
  end

  defp fix_file_opts(hConfig) do
    Map.put(hConfig, :filesync_repeat_interval, :no_repeat)
  end

  defp fix_modes(modes) do
    modes1 =
      case (for m <- modes,
                :lists.member(m, [:write, :append, :exclusive]) do
              m
            end) do
        [] ->
          [:append | modes]

        _ ->
          modes
      end

    modes2 =
      case :lists.member(:raw, modes) do
        false ->
          [:raw | modes1]

        true ->
          modes1
      end

    case :lists.partition(
           fn
             :delayed_write ->
               true

             {:delayed_write, _, _} ->
               true

             _ ->
               false
           end,
           modes2
         ) do
      {[], _} ->
        [:delayed_write | modes2]

      _ ->
        modes2
    end
  end

  def config_changed(
        _Name,
        %{
          file_check: fileCheck,
          max_no_bytes: size,
          max_no_files: count,
          compress_on_rotate: compress
        },
        %{
          file_check: fileCheck,
          max_no_bytes: size,
          max_no_files: count,
          compress_on_rotate: compress
        } = state
      ) do
    state
  end

  def config_changed(
        _Name,
        %{
          file_check: fileCheck,
          max_no_bytes: size,
          max_no_files: count,
          compress_on_rotate: compress
        },
        %{file_ctrl_pid: fileCtrlPid} = state
      ) do
    send(
      fileCtrlPid,
      {:update_config,
       %{
         file_check: fileCheck,
         max_no_bytes: size,
         max_no_files: count,
         compress_on_rotate: compress
       }}
    )

    %{
      state
      | file_check: fileCheck,
        max_no_bytes: size,
        max_no_files: count,
        compress_on_rotate: compress
    }
  end

  def config_changed(_Name, _NewHConfig, state) do
    state
  end

  def filesync(_Name, syncAsync, %{file_ctrl_pid: fileCtrlPid} = state) do
    result = file_ctrl_filesync(syncAsync, fileCtrlPid)
    {result, state}
  end

  def write(_Name, syncAsync, bin, %{file_ctrl_pid: fileCtrlPid} = state) do
    result = file_write(syncAsync, fileCtrlPid, bin)
    {result, state}
  end

  def reset_state(_Name, state) do
    state
  end

  def handle_info(_Name, {:EXIT, pid, why}, %{file_ctrl_pid: pid} = state) do
    exit({:error, {:write_failed, :maps.with([:type, :file, :modes], state), why}})
  end

  def handle_info(_, _, state) do
    state
  end

  def terminate(_Name, _Reason, %{file_ctrl_pid: fWPid}) do
    case :erlang.is_process_alive(fWPid) do
      true ->
        :erlang.unlink(fWPid)
        _ = file_ctrl_stop(fWPid)
        mRef = :erlang.monitor(:process, fWPid)

        receive do
          {:DOWN, ^mRef, _, _, _} ->
            :ok
        after
          5000 ->
            :erlang.exit(fWPid, :kill)
            :ok
        end

      false ->
        :ok
    end
  end

  defp open_log_file(
         handlerName,
         %{type: :file, file: fileName, modes: modes, file_check: fileCheck}
       ) do
    try do
      case :filelib.ensure_dir(fileName) do
        :ok ->
          case :file.open(fileName, modes) do
            {:ok, fd} ->
              {:ok, r_file_info(inode: iNode)} =
                :file.read_file_info(
                  fileName,
                  [:raw]
                )

              updateModes = [:append | modes -- [:write, :append, :exclusive]]

              {:ok,
               %{
                 handler_name: handlerName,
                 file_name: fileName,
                 modes: updateModes,
                 file_check: fileCheck,
                 fd: fd,
                 inode: iNode,
                 last_check: timestamp(),
                 synced: false,
                 write_res: :ok,
                 sync_res: :ok
               }}

            error ->
              error
          end

        error ->
          error
      end
    catch
      _, reason ->
        {:error, reason}
    end
  end

  defp close_log_file(%{fd: fd}) do
    _ = :file.datasync(fd)
    _ = :file.close(fd)
    :ok
  end

  defp close_log_file(_) do
    :ok
  end

  defp delayed_write_close(%{fd: fd}) do
    case :file.close(fd) do
      {:error, _} ->
        :file.close(fd)

      res ->
        res
    end
  end

  defp file_ctrl_start(handlerName, hConfig) do
    starter = self()

    fileCtrlPid =
      spawn_link(fn ->
        file_ctrl_init(handlerName, hConfig, starter)
      end)

    receive do
      {^fileCtrlPid, :ok} ->
        {:ok, fileCtrlPid}

      {^fileCtrlPid, error} ->
        error
    after
      5000 ->
        {:error, :file_ctrl_process_not_started}
    end
  end

  defp file_ctrl_stop(pid) do
    send(pid, :stop)
  end

  defp file_write(:async, pid, bin) do
    send(pid, {:log, bin})
    :ok
  end

  defp file_write(:sync, pid, bin) do
    file_ctrl_call(pid, {:log, bin})
  end

  defp file_ctrl_filesync(:async, pid) do
    send(pid, :filesync)
    :ok
  end

  defp file_ctrl_filesync(:sync, pid) do
    file_ctrl_call(pid, :filesync)
  end

  defp file_ctrl_call(pid, msg) do
    mRef = :erlang.monitor(:process, pid)
    send(pid, {msg, {self(), mRef}})

    receive do
      {^mRef, result} ->
        :erlang.demonitor(mRef, [:flush])
        result

      {:DOWN, ^mRef, _Type, _Object, reason} ->
        {:error, reason}
    after
      5000 ->
        :erlang.demonitor(mRef, [:flush])
        {:error, {:no_response, pid}}
    end
  end

  defp file_ctrl_init(
         handlerName,
         %{
           type: :file,
           max_no_bytes: size,
           max_no_files: count,
           compress_on_rotate: compress,
           file: fileName
         } = hConfig,
         starter
       ) do
    :erlang.process_flag(:message_queue_data, :off_heap)

    case open_log_file(handlerName, hConfig) do
      {:ok, state} ->
        send(starter, {self(), :ok})

        rotState =
          update_rotation(
            {size, count, compress},
            state
          )

        file_ctrl_loop(rotState)

      {:error, reason} ->
        send(starter, {self(), {:error, {:open_failed, fileName, reason}}})
    end
  end

  defp file_ctrl_init(handlerName, %{type: {:device, dev}}, starter) do
    send(starter, {self(), :ok})
    file_ctrl_loop(%{handler_name: handlerName, dev: dev})
  end

  defp file_ctrl_init(handlerName, %{type: stdDev}, starter) do
    send(starter, {self(), :ok})
    file_ctrl_loop(%{handler_name: handlerName, dev: stdDev})
  end

  defp file_ctrl_loop(state) do
    receive do
      {:log, bin} ->
        state1 = write_to_dev(bin, state)
        file_ctrl_loop(state1)

      {{:log, bin}, {from, mRef}} ->
        state1 = ensure_file(state)
        state2 = write_to_dev(bin, state1)
        send(from, {mRef, :ok})
        file_ctrl_loop(state2)

      :filesync ->
        state1 = sync_dev(state)
        file_ctrl_loop(state1)

      {:filesync, {from, mRef}} ->
        state1 = ensure_file(state)
        state2 = sync_dev(state1)
        send(from, {mRef, :ok})
        file_ctrl_loop(state2)

      {:update_config,
       %{
         file_check: fileCheck,
         max_no_bytes: size,
         max_no_files: count,
         compress_on_rotate: compress
       }} ->
        state1 = update_rotation({size, count, compress}, state)
        file_ctrl_loop(Map.put(state1, :file_check, fileCheck))

      :stop ->
        close_log_file(state)
        :stopped
    end
  end

  defp maybe_ensure_file(%{file_check: 0} = state) do
    ensure_file(state)
  end

  defp maybe_ensure_file(%{last_check: t0, file_check: checkInt} = state)
       when is_integer(checkInt) do
    t = timestamp()

    cond do
      t - t0 > checkInt ->
        ensure_file(state)

      true ->
        state
    end
  end

  defp maybe_ensure_file(state) do
    state
  end

  defp ensure_file(%{inode: iNode0, file_name: fileName, modes: modes} = state) do
    case :file.read_file_info(fileName, [:raw]) do
      {:ok, r_file_info(inode: ^iNode0)} ->
        Map.put(state, :last_check, timestamp())

      _ ->
        close_log_file(state)

        case :file.open(fileName, modes) do
          {:ok, fd} ->
            {:ok, r_file_info(inode: iNode)} =
              :file.read_file_info(
                fileName,
                [:raw]
              )

            Map.merge(state, %{
              fd: fd,
              inode: iNode,
              last_check: timestamp(),
              synced: true,
              sync_res: :ok
            })

          error ->
            exit({:could_not_reopen_file, error})
        end
    end
  end

  defp ensure_file(state) do
    state
  end

  defp write_to_dev(bin, %{dev: devName} = state) do
    :io.put_chars(devName, bin)
    state
  end

  defp write_to_dev(bin, state) do
    state1 = %{fd: fd} = maybe_ensure_file(state)
    result = :file.write(fd, bin)
    state2 = maybe_rotate_file(bin, state1)
    maybe_notify_error(:write, result, state2)
    Map.merge(state2, %{synced: false, write_res: result})
  end

  defp sync_dev(%{synced: false} = state) do
    state1 = %{fd: fd} = maybe_ensure_file(state)
    result = :file.datasync(fd)
    maybe_notify_error(:filesync, result, state1)
    Map.merge(state1, %{synced: true, sync_res: result})
  end

  defp sync_dev(state) do
    state
  end

  defp update_rotation({:infinity, _, _}, state) do
    maybe_remove_archives(0, state)
    :maps.remove(:rotation, state)
  end

  defp update_rotation(
         {size, count, compress},
         %{file_name: fileName} = state
       ) do
    maybe_remove_archives(count, state)

    {:ok, r_file_info(size: currSize)} =
      :file.read_file_info(
        fileName,
        [:raw]
      )

    state1 =
      Map.put(state, :rotation, %{
        size: size,
        count: count,
        compress: compress,
        curr_size: currSize
      })

    maybe_update_compress(0, state1)
    maybe_rotate_file(0, state1)
  end

  defp maybe_remove_archives(count, %{file_name: fileName} = state) do
    archive = rot_file_name(fileName, count, false)
    compressedArchive = rot_file_name(fileName, count, true)

    case {:file.read_file_info(archive, [:raw]), :file.read_file_info(compressedArchive, [:raw])} do
      {{:error, :enoent}, {:error, :enoent}} ->
        :ok

      _ ->
        _ = :file.delete(archive)
        _ = :file.delete(compressedArchive)
        maybe_remove_archives(count + 1, state)
    end
  end

  defp maybe_update_compress(count, %{rotation: %{count: count}}) do
    :ok
  end

  defp maybe_update_compress(
         n,
         %{file_name: fileName, rotation: %{compress: compress}} = state
       ) do
    archive = rot_file_name(fileName, n, not compress)

    case :file.read_file_info(archive, [:raw]) do
      {:ok, _} when compress ->
        compress_file(archive)

      {:ok, _} ->
        decompress_file(archive)

      _ ->
        :ok
    end

    maybe_update_compress(n + 1, state)
  end

  defp maybe_rotate_file(bin, %{rotation: _} = state)
       when is_binary(bin) do
    maybe_rotate_file(byte_size(bin), state)
  end

  defp maybe_rotate_file(
         addSize,
         %{rotation: %{size: rotSize, curr_size: currSize} = rotation} = state
       ) do
    newSize = currSize + addSize

    cond do
      newSize > rotSize ->
        rotate_file(Map.put(state, :rotation, Map.put(rotation, :curr_size, newSize)))

      true ->
        Map.put(state, :rotation, Map.put(rotation, :curr_size, newSize))
    end
  end

  defp maybe_rotate_file(_Bin, state) do
    state
  end

  defp rotate_file(%{file_name: fileName, modes: modes, rotation: rotation} = state) do
    state1 = sync_dev(state)
    _ = delayed_write_close(state)
    rotate_files(fileName, :maps.get(:count, rotation), :maps.get(:compress, rotation))

    case :file.open(fileName, modes) do
      {:ok, fd} ->
        {:ok, r_file_info(inode: iNode)} =
          :file.read_file_info(
            fileName,
            [:raw]
          )

        Map.merge(state1, %{fd: fd, inode: iNode, rotation: Map.put(rotation, :curr_size, 0)})

      error ->
        exit({:could_not_reopen_file, error})
    end
  end

  defp rotate_files(fileName, 0, _Compress) do
    _ = :file.delete(fileName)
    :ok
  end

  defp rotate_files(fileName, 1, compress) do
    fileName0 = fileName ++ '.0'
    _ = :file.rename(fileName, fileName0)

    cond do
      compress ->
        compress_file(fileName0)

      true ->
        :ok
    end

    :ok
  end

  defp rotate_files(fileName, count, compress) do
    _ =
      :file.rename(
        rot_file_name(fileName, count - 2, compress),
        rot_file_name(fileName, count - 1, compress)
      )

    rotate_files(fileName, count - 1, compress)
  end

  defp rot_file_name(fileName, count, false) do
    fileName ++ '.' ++ :erlang.integer_to_list(count)
  end

  defp rot_file_name(fileName, count, true) do
    rot_file_name(fileName, count, false) ++ '.gz'
  end

  defp compress_file(fileName) do
    {:ok, in__} = :file.open(fileName, [:read, :binary])
    {:ok, out} = :file.open(fileName ++ '.gz', [:write])
    z = :zlib.open()
    :zlib.deflateInit(z, :default, :deflated, 31, 8, :default)
    compress_data(z, in__, out)
    :zlib.deflateEnd(z)
    :zlib.close(z)
    _ = :file.close(in__)
    _ = :file.close(out)
    _ = :file.delete(fileName)
    :ok
  end

  defp compress_data(z, in__, out) do
    case :file.read(in__, 100_000) do
      {:ok, data} ->
        compressed = :zlib.deflate(z, data)
        _ = :file.write(out, compressed)
        compress_data(z, in__, out)

      :eof ->
        compressed = :zlib.deflate(z, <<>>, :finish)
        _ = :file.write(out, compressed)
        :ok
    end
  end

  defp decompress_file(fileName) do
    {:ok, in__} = :file.open(fileName, [:read, :binary])

    {:ok, out} =
      :file.open(
        :filename.rootname(fileName, '.gz'),
        [:write]
      )

    z = :zlib.open()
    :zlib.inflateInit(z, 31)
    decompress_data(z, in__, out)
    :zlib.inflateEnd(z)
    :zlib.close(z)
    _ = :file.close(in__)
    _ = :file.close(out)
    _ = :file.delete(fileName)
    :ok
  end

  defp decompress_data(z, in__, out) do
    case :file.read(in__, 1000) do
      {:ok, data} ->
        decompressed = :zlib.inflate(z, data)
        _ = :file.write(out, decompressed)
        decompress_data(z, in__, out)

      :eof ->
        :ok
    end
  end

  defp maybe_notify_error(_Op, :ok, _State) do
    :ok
  end

  defp maybe_notify_error(op, result, %{write_res: wR, sync_res: sR})
       when (op == :write and result == wR) or (op == :filesync and result == sR) do
    :ok
  end

  defp maybe_notify_error(op, error, %{handler_name: handlerName, file_name: fileName}) do
    :logger_h_common.error_notify({handlerName, op, fileName, error})
    :ok
  end

  defp timestamp() do
    :erlang.monotonic_time(:millisecond)
  end
end
