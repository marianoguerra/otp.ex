defmodule :m_logger_disk_log_h do
  use Bitwise

  def filesync(name) do
    :logger_h_common.filesync(:logger_disk_log_h, name)
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

  def init(
        name,
        %{file: file, type: type, max_no_bytes: mNB, max_no_files: mNF}
      ) do
    case open_disk_log(name, file, type, mNB, mNF) do
      :ok ->
        {:ok,
         %{
           log_opts: %{file: file, type: type, max_no_bytes: mNB, max_no_files: mNF},
           prev_log_result: :ok,
           prev_sync_result: :ok,
           prev_disk_log_info: :undefined
         }}

      error ->
        error
    end
  end

  def check_config(name, :set, :undefined, hConfig0) do
    hConfig =
      merge_default_logopts(
        name,
        :maps.merge(
          get_default_config(),
          hConfig0
        )
      )

    check_config(hConfig)
  end

  def check_config(_Name, setOrUpdate, oldHConfig, newHConfig0) do
    writeOnce =
      :maps.with(
        [:type, :file, :max_no_files, :max_no_bytes],
        oldHConfig
      )

    default =
      case setOrUpdate do
        :set ->
          :maps.merge(get_default_config(), writeOnce)

        :update ->
          oldHConfig
      end

    newHConfig = :maps.merge(default, newHConfig0)

    case :maps.with(
           [:type, :file, :max_no_files, :max_no_bytes],
           newHConfig
         ) do
      ^writeOnce ->
        check_config(newHConfig)

      other ->
        {old, new} = :logger_server.diff_maps(writeOnce, other)
        {:error, {:illegal_config_change, :logger_disk_log_h, old, new}}
    end
  end

  defp check_config(hConfig) do
    case check_h_config(:maps.to_list(hConfig)) do
      :ok ->
        {:ok, hConfig}

      {:error, {key, value}} ->
        {:error, {:invalid_config, :logger_disk_log_h, %{key => value}}}
    end
  end

  defp check_h_config([{:file, file} | config]) when is_list(file) do
    check_h_config(config)
  end

  defp check_h_config([{:max_no_files, :undefined} | config]) do
    check_h_config(config)
  end

  defp check_h_config([{:max_no_files, n} | config])
       when is_integer(n) and n > 0 do
    check_h_config(config)
  end

  defp check_h_config([{:max_no_bytes, :infinity} | config]) do
    check_h_config(config)
  end

  defp check_h_config([{:max_no_bytes, n} | config])
       when is_integer(n) and n > 0 do
    check_h_config(config)
  end

  defp check_h_config([{:type, type} | config])
       when type == :wrap or
              type == :halt do
    check_h_config(config)
  end

  defp check_h_config([other | _]) do
    {:error, other}
  end

  defp check_h_config([]) do
    :ok
  end

  defp get_default_config() do
    %{}
  end

  defp merge_default_logopts(name, hConfig) do
    type = :maps.get(:type, hConfig, :wrap)

    {defaultNoFiles, defaultNoBytes} =
      case type do
        :halt ->
          {:undefined, :infinity}

        _wrap ->
          {10, 1_048_576}
      end

    {:ok, dir} = :file.get_cwd()

    defaults = %{
      file: :filename.join(dir, name),
      max_no_files: defaultNoFiles,
      max_no_bytes: defaultNoBytes,
      type: type
    }

    :maps.merge(defaults, hConfig)
  end

  def filesync(name, _Mode, state) do
    result = :disk_log.sync(name)
    maybe_notify_error(name, :filesync, result, :prev_sync_result, state)
  end

  def write(name, mode, bin, state) do
    result = disk_log_write(name, mode, bin)
    maybe_notify_error(name, :log, result, :prev_log_result, state)
  end

  def reset_state(_Name, state) do
    Map.merge(state, %{
      prev_log_result: :ok,
      prev_sync_result: :ok,
      prev_disk_log_info: :undefined
    })
  end

  def handle_info(
        name,
        {:disk_log, _Node, log, info = {:truncated, _NoLostItems}},
        state
      ) do
    maybe_notify_status(name, log, info, :prev_disk_log_info, state)
  end

  def handle_info(
        name,
        {:disk_log, _Node, log, info = {:blocked_log, _Items}},
        state
      ) do
    maybe_notify_status(name, log, info, :prev_disk_log_info, state)
  end

  def handle_info(name, {:disk_log, _Node, log, info = :full}, state) do
    maybe_notify_status(name, log, info, :prev_disk_log_info, state)
  end

  def handle_info(
        name,
        {:disk_log, _Node, log, info = {:error_status, _Status}},
        state
      ) do
    maybe_notify_status(name, log, info, :prev_disk_log_info, state)
  end

  def handle_info(_, _, state) do
    state
  end

  def terminate(name, _Reason, _State) do
    _ = close_disk_log(name, :normal)
    :ok
  end

  defp open_disk_log(name, file, type, maxNoBytes, maxNoFiles) do
    case :filelib.ensure_dir(file) do
      :ok ->
        size =
          cond do
            type == :halt ->
              maxNoBytes

            type == :wrap ->
              {maxNoBytes, maxNoFiles}
          end

        opts = [
          {:name, name},
          {:file, file},
          {:size, size},
          {:type, type},
          {:linkto, self()},
          {:repair, false},
          {:format, :external},
          {:notify, true},
          {:quiet, true},
          {:mode, :read_write}
        ]

        case :disk_log.open(opts) do
          {:ok, ^name} ->
            :ok

          error = {:error, _Reason} ->
            error
        end

      error ->
        error
    end
  end

  defp close_disk_log(name, _) do
    _ = :disk_log.sync(name)
    _ = :disk_log.close(name)
    :ok
  end

  defp disk_log_write(name, :sync, bin) do
    :disk_log.blog(name, bin)
  end

  defp disk_log_write(name, :async, bin) do
    :disk_log.balog(name, bin)
  end

  defp maybe_notify_error(name, op, result, key, %{log_opts: logOpts} = state) do
    {result, error_notify_new({name, op, logOpts, result}, result, key, state)}
  end

  defp maybe_notify_status(name, log, info, key, state) do
    error_notify_new({:disk_log, name, log, info}, info, key, state)
  end

  defp error_notify_new(term, what, key, state) do
    error_notify_new(what, :maps.get(key, state), term)
    Map.put(state, key, what)
  end

  defp error_notify_new(:ok, _Prev, _Term) do
    :ok
  end

  defp error_notify_new(same, same, _Term) do
    :ok
  end

  defp error_notify_new(_New, _Prev, term) do
    :logger_h_common.error_notify(term)
  end
end
