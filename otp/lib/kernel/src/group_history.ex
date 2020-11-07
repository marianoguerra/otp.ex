defmodule :m_group_history do
  use Bitwise

  def load() do
    wait_for_kernel_safe_sup()

    case history_status() do
      :enabled ->
        try do
          open_log()
        catch
          :exit, _ ->
            []
        else
          {:ok, :"$#group_history"} ->
            read_full_log(:"$#group_history")

          {:repaired, :"$#group_history", {:recovered, good}, {:badbytes, bad}} ->
            report_repairs(:"$#group_history", good, bad)
            read_full_log(:"$#group_history")

          {:error, {:need_repair, _FileName}} ->
            repair_log(:"$#group_history")

          {:error, {:arg_mismatch, :repair, true, false}} ->
            repair_log(:"$#group_history")

          {:error, {:name_already_open, _}} ->
            show_rename_warning()
            read_full_log(:"$#group_history")

          {:error, {:size_mismatch, current, new}} ->
            show_size_warning(current, new)
            resize_log(:"$#group_history", current, new)
            load()

          {:error, {:invalid_header, {:vsn, version}}} ->
            upgrade_version(:"$#group_history", version)
            load()

          {:error, reason} ->
            handle_open_error(reason)
            disable_history()
            []
        end

      _ ->
        []
    end
  end

  def add(line) do
    add(line, history_status())
  end

  defp add(line, :enabled) do
    case :lists.member(line, to_drop()) do
      false ->
        case :disk_log.log(:"$#group_history", line) do
          :ok ->
            :ok

          {:error, :no_such_log} ->
            _ = open_log()
            :disk_log.log(:"$#group_history", line)

          {:error, _Other} ->
            :ok
        end

      true ->
        :ok
    end
  end

  defp add(_Line, :disabled) do
    :ok
  end

  defp wait_for_kernel_safe_sup() do
    case :erlang.whereis(:kernel_safe_sup) do
      :undefined ->
        :timer.sleep(50)
        wait_for_kernel_safe_sup()

      _ ->
        :ok
    end
  end

  defp repair_log(name) do
    opts = :lists.keydelete(:size, 1, log_options())

    case :disk_log.open(opts) do
      {:repaired, :"$#group_history", {:recovered, good}, {:badbytes, bad}} ->
        report_repairs(:"$#group_history", good, bad)

      _ ->
        :ok
    end

    _ = :disk_log.close(name)
    load()
  end

  defp history_status() do
    skip = is_user() or not init_running()

    case :application.get_env(:kernel, :shell_history) do
      {:ok, :enabled} when not skip ->
        :enabled

      :undefined when not skip ->
        :disabled

      _ ->
        :disabled
    end
  end

  defp is_user() do
    case :erlang.process_info(self(), :registered_name) do
      {:registered_name, :user} ->
        true

      _ ->
        false
    end
  end

  defp init_running() do
    case :init.get_status() do
      {:stopping, _} ->
        false

      _ ->
        true
    end
  end

  defp open_log() do
    opts = log_options()
    _ = ensure_path(opts)
    :disk_log.open(opts)
  end

  defp log_options() do
    path = find_path()
    file = :filename.join([path, 'erlang-shell-log'])
    size = find_wrap_values()

    [
      {:name, :"$#group_history"},
      {:file, file},
      {:repair, true},
      {:format, :internal},
      {:type, :wrap},
      {:size, size},
      {:notify, false},
      {:head, {:vsn, :EFE_TODO_VSN_MACRO}},
      {:quiet, true},
      {:mode, :read_write}
    ]
  end

  defp ensure_path(opts) do
    {:file, path} = :lists.keyfind(:file, 1, opts)
    :filelib.ensure_dir(path)
  end

  defp read_full_log(name) do
    case :disk_log.chunk(name, :start) do
      {:error, :no_such_log} ->
        show_unexpected_close_warning()
        []

      :eof ->
        []

      {cont, logs} ->
        :lists.reverse(
          maybe_drop_header(logs) ++
            read_full_log(
              name,
              cont
            )
        )
    end
  end

  defp read_full_log(name, cont) do
    case :disk_log.chunk(name, cont) do
      {:error, :no_such_log} ->
        show_unexpected_close_warning()
        []

      :eof ->
        []

      {nextCont, logs} ->
        maybe_drop_header(logs) ++ read_full_log(name, nextCont)
    end
  end

  defp maybe_drop_header([{:vsn, _} | rest]) do
    rest
  end

  defp maybe_drop_header(logs) do
    logs
  end

  defp handle_open_error({:arg_mismatch, optName, currentVal, newVal}) do
    show(
      :"$#erlang-history-arg-mismatch",
      'Log file argument ~p changed value from ~p to ~p and cannot be automatically updated. Please clear the history files and try again.~n',
      [optName, currentVal, newVal]
    )
  end

  defp handle_open_error({:not_a_log_file, fileName}) do
    show_invalid_file_warning(fileName)
  end

  defp handle_open_error({:invalid_index_file, fileName}) do
    show_invalid_file_warning(fileName)
  end

  defp handle_open_error({:invalid_header, term}) do
    show(
      :"$#erlang-history-invalid-header",
      'Shell history expects to be able to use the log files which currently have unknown headers (~p) and may belong to another mechanism. History logging will be disabled.~n',
      [term]
    )
  end

  defp handle_open_error({:file_error, fileName, reason}) do
    show(
      :"$#erlang-history-file-error",
      'Error handling File ~ts. Reason: ~p~nHistory logging will be disabled.~n',
      [fileName, reason]
    )
  end

  defp handle_open_error(err) do
    show_unexpected_warning({:disk_log, :open, 1}, err)
  end

  defp find_wrap_values() do
    confSize =
      case :application.get_env(
             :kernel,
             :shell_history_file_bytes
           ) do
        :undefined ->
          1024 * 512

        {:ok, s} ->
          s
      end

    sizePerFile = max(50 * 1024, div(confSize, 10))

    fileCount =
      cond do
        sizePerFile > 50 * 1024 ->
          10

        sizePerFile <= 50 * 1024 ->
          max(1, div(confSize, sizePerFile))
      end

    {sizePerFile, fileCount}
  end

  defp report_repairs(_, _, 0) do
    :ok
  end

  defp report_repairs(_, good, bad) do
    show(
      :"$#erlang-history-report-repairs",
      'The shell history log file was corrupted and was repaired. ~p bytes were recovered and ~p were lost.~n',
      [good, bad]
    )
  end

  defp resize_log(name, _OldSize, newSize) do
    show(
      :"$#erlang-history-resize-attempt",
      'Attempting to resize the log history file to ~p...',
      [newSize]
    )

    opts = :lists.keydelete(:size, 1, log_options())

    _ =
      case :disk_log.open(opts) do
        {:error, {:need_repair, _}} ->
          _ = repair_log(name)
          :disk_log.open(opts)

        _ ->
          :ok
      end

    case :disk_log.change_size(name, newSize) do
      :ok ->
        show(:"$#erlang-history-resize-result", 'ok~n', [])

      {:error, {:new_size_too_small, _, _}} ->
        show(:"$#erlang-history-resize-result", 'failed (new size is too small)~n', [])
        disable_history()

      {:error, reason} ->
        show(:"$#erlang-history-resize-result", 'failed (~p)~n', [reason])
        disable_history()
    end
  end

  defp upgrade_version(_Name, unsupported) do
    show(
      :"$#erlang-history-upgrade",
      'The version for the shell logs found on disk (~p) is not supported by the current version (~p)~n',
      [unsupported, :EFE_TODO_VSN_MACRO]
    )

    disable_history()
  end

  defp disable_history() do
    show(:"$#erlang-history-disable", 'Disabling shell history logging.~n', [])
    :application.set_env(:kernel, :shell_history, :force_disabled)
  end

  defp find_path() do
    case :application.get_env(
           :kernel,
           :shell_history_path
         ) do
      :undefined ->
        :filename.basedir(:user_cache, 'erlang-history')

      {:ok, path} ->
        path
    end
  end

  defp to_drop() do
    case :application.get_env(
           :kernel,
           :shell_history_drop
         ) do
      :undefined ->
        :application.set_env(:kernel, :shell_history_drop, [])
        []

      {:ok, v} when is_list(v) ->
        for ln <- v do
          ln ++ '\n'
        end

      {:ok, _} ->
        []
    end
  end

  defp show_rename_warning() do
    show(
      :"$#erlang-history-rename-warn",
      'A history file with a different path has already been started for the shell of this node. The old name will keep being used for this session.~n',
      []
    )
  end

  defp show_invalid_file_warning(fileName) do
    show(
      :"$#erlang-history-invalid-file",
      'Shell history expects to be able to use the file ~ts which currently exists and is not a file usable for history logging purposes. History logging will be disabled.~n',
      [fileName]
    )
  end

  defp show_unexpected_warning({m, f, a}, term) do
    show(
      :"$#erlang-history-unexpected-return",
      'unexpected return value from ~p:~p/~p: ~p~nshell history will be disabled for this session.~n',
      [m, f, a, term]
    )
  end

  defp show_unexpected_close_warning() do
    show(
      :"$#erlang-history-unexpected-close",
      'The shell log file has mysteriously closed. Ignoring currently unread history.~n',
      []
    )
  end

  defp show_size_warning(_Current, _New) do
    show(
      :"$#erlang-history-size",
      'The configured log history file size is different from the size of the log file on disk.~n',
      []
    )
  end

  defp show(key, format, args) do
    case :erlang.get(key) do
      :undefined ->
        :io.format(:standard_error, format, args)
        :erlang.put(key, true)
        :ok

      true ->
        :ok
    end
  end
end
