defmodule :m_heart do
  use Bitwise
  require Record

  Record.defrecord(:r_state, :state,
    port: :undefined,
    cmd: :undefined,
    options: :undefined,
    callback: :undefined
  )

  def start() do
    case :erlang.whereis(:heart) do
      :undefined ->
        pid = spawn(:heart, :init, [self(), :erlang.whereis(:init)])
        wait_for_init_ack(pid)

      pid ->
        {:ok, pid}
    end
  end

  defp wait_for_init_ack(from) do
    receive do
      {:ok, ^from} = ok ->
        ok

      {:no_heart, ^from} ->
        :ignore

      {error, ^from} ->
        {:error, error}
    end
  end

  def init(starter, parent) do
    :erlang.process_flag(:trap_exit, true)
    :erlang.process_flag(:priority, :max)
    :erlang.register(:heart, self())

    case (try do
            start_portprogram()
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, port} ->
        send(starter, {:ok, self()})
        loop(parent, r_state(port: port, cmd: [], options: []))

      :no_heart ->
        send(starter, {:no_heart, self()})

      :error ->
        send(starter, {:start_error, self()})
    end
  end

  def set_cmd(cmd) do
    send(:heart, {self(), :set_cmd, cmd})
    wait()
  end

  def get_cmd() do
    send(:heart, {self(), :get_cmd})
    wait()
  end

  def clear_cmd() do
    send(:heart, {self(), :clear_cmd})
    wait()
  end

  def set_callback(module, function) do
    send(:heart, {self(), :set_callback, {module, function}})
    wait()
  end

  def get_callback() do
    send(:heart, {self(), :get_callback})
    wait()
  end

  def clear_callback() do
    send(:heart, {self(), :clear_callback})
    wait()
  end

  def set_options(options) do
    send(:heart, {self(), :set_options, options})
    wait()
  end

  def get_options() do
    send(:heart, {self(), :get_options})
    wait()
  end

  def cycle() do
    send(:heart, {self(), :cycle})
    wait()
  end

  defp wait() do
    receive do
      {:heart, res} ->
        res
    end
  end

  defp start_portprogram() do
    check_start_heart()
    heartCmd = 'heart -pid ' ++ :os.getpid() ++ ' ' ++ get_heart_timeouts()

    try do
      :erlang.open_port({:spawn, heartCmd}, [{:packet, 2}])
    catch
      _, reason ->
        report_problem({{:open_port, reason}, {:heart, :start_portprogram, []}})
        :error
    else
      port when is_port(port) ->
        case wait_ack(port) do
          :ok ->
            :erlang.register(:heart_port, port)
            {:ok, port}

          {:error, reason} ->
            report_problem({{:port_problem, reason}, {:heart, :start_portprogram, []}})
            :error
        end
    end
  end

  defp get_heart_timeouts() do
    case :os.getenv('HEART_BEAT_TIMEOUT') do
      false ->
        ''

      h when is_list(h) ->
        '-ht ' ++ h
    end
  end

  defp check_start_heart() do
    case :init.get_argument(:heart) do
      {:ok, [[]]} ->
        :ok

      :error ->
        throw(:no_heart)

      {:ok, [[x | _] | _]} ->
        report_problem(
          {{:bad_heart_flag, :erlang.list_to_atom(x)}, {:heart, :check_start_heart, []}}
        )

        throw(:error)
    end
  end

  defp wait_ack(port) do
    receive do
      {^port, {:data, [1]}} ->
        :ok

      {:EXIT, ^port, :badsig} ->
        wait_ack(port)

      {:EXIT, ^port, reason} ->
        {:error, reason}
    end
  end

  defp loop(parent, r_state(port: port) = s) do
    _ = send_heart_beat(s)

    receive do
      {from, :set_cmd, newCmd0} ->
        enc = :file.native_name_encoding()

        case (try do
                :unicode.characters_to_binary(newCmd0, enc, enc)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          newCmd
          when is_binary(newCmd) and
                 byte_size(newCmd) < 2047 ->
            _ = send_heart_cmd(port, newCmd)
            _ = wait_ack(port)
            send(from, {:heart, :ok})
            loop(parent, r_state(s, cmd: newCmd))

          _ ->
            send(from, {:heart, {:error, {:bad_cmd, newCmd0}}})
            loop(parent, s)
        end

      {from, :clear_cmd} ->
        send(from, {:heart, :ok})
        _ = send_heart_cmd(port, [])
        _ = wait_ack(port)
        loop(parent, r_state(s, cmd: []))

      {from, :get_cmd} ->
        send(from, {:heart, get_heart_cmd(port)})
        loop(parent, s)

      {from, :set_callback, callback} ->
        case callback do
          {m, f} when is_atom(m) and is_atom(f) ->
            send(from, {:heart, :ok})
            loop(parent, r_state(s, callback: callback))

          _ ->
            send(from, {:heart, {:error, {:bad_callback, callback}}})
            loop(parent, s)
        end

      {from, :get_callback} ->
        res =
          case r_state(s, :callback) do
            :undefined ->
              :none

            cb ->
              {:ok, cb}
          end

        send(from, {:heart, res})
        loop(parent, s)

      {from, :clear_callback} ->
        send(from, {:heart, :ok})
        loop(parent, r_state(s, callback: :undefined))

      {from, :set_options, options} ->
        case validate_options(options) do
          validated when is_list(validated) ->
            send(from, {:heart, :ok})
            loop(parent, r_state(s, options: validated))

          _ ->
            send(from, {:heart, {:error, {:bad_options, options}}})
            loop(parent, s)
        end

      {from, :get_options} ->
        res =
          case r_state(s, :options) do
            [] ->
              :none

            cb ->
              {:ok, cb}
          end

        send(from, {:heart, res})
        loop(parent, s)

      {from, :cycle} ->
        do_cycle_port_program(from, parent, s)

      {:EXIT, ^parent, :shutdown} ->
        no_reboot_shutdown(port)

      {:EXIT, ^parent, reason} ->
        :erlang.exit(port, reason)
        exit(reason)

      {:EXIT, ^port, :badsig} ->
        loop(parent, s)

      {:EXIT, ^port, _Reason} ->
        exit({:port_terminated, {:heart, :loop, [parent, s]}})

      _ ->
        loop(parent, s)
    after
      5000 ->
        loop(parent, s)
    end
  end

  defp no_reboot_shutdown(port) do
    _ = send_shutdown(port)

    receive do
      {:EXIT, ^port, reason} when reason !== :badsig ->
        exit(:normal)
    end
  end

  defp validate_options(opts) do
    validate_options(opts, [])
  end

  defp validate_options([], res) do
    res
  end

  defp validate_options([:check_schedulers = opt | opts], res) do
    validate_options(opts, [opt | res])
  end

  defp validate_options(_, _) do
    :error
  end

  defp do_cycle_port_program(caller, parent, r_state(port: port) = s) do
    :erlang.unregister(:heart_port)

    case (try do
            start_portprogram()
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, newPort} ->
        _ = send_shutdown(port)

        receive do
          {:EXIT, ^port, _Reason} ->
            _ = send_heart_cmd(newPort, r_state(s, :cmd))
            send(caller, {:heart, :ok})
            loop(parent, r_state(s, port: newPort))
        after
          10000 ->
            _ = send_heart_cmd(newPort, r_state(s, :cmd))
            send(caller, {:heart, {:error, :stop_error}})
            loop(parent, r_state(s, port: newPort))
        end

      :no_heart ->
        send(caller, {:heart, {:error, :no_heart}})
        loop(parent, s)

      :error ->
        send(caller, {:heart, {:error, :start_error}})
        loop(parent, s)
    end
  end

  defp send_heart_beat(r_state(port: port, callback: cb, options: opts)) do
    :ok = check_system(opts)
    :ok = check_callback(cb)
    send(port, {self(), {:command, [2]}})
  end

  defp send_heart_cmd(port, []) do
    send(port, {self(), {:command, [5]}})
  end

  defp send_heart_cmd(port, cmd) do
    send(port, {self(), {:command, [4 | cmd]}})
  end

  defp get_heart_cmd(port) do
    send(port, {self(), {:command, [6]}})

    receive do
      {^port, {:data, [7 | cmd]}} ->
        {:ok, cmd}
    end
  end

  defp check_system([]) do
    :ok
  end

  defp check_system([:check_schedulers | opts]) do
    :ok = :erts_internal.system_check(:schedulers)
    check_system(opts)
  end

  defp check_callback(callback) do
    case callback do
      :undefined ->
        :ok

      {m, f} ->
        :erlang.apply(m, f, [])
    end
  end

  defp send_shutdown(port) do
    send(port, {self(), {:command, [3]}})
  end

  defp report_problem(error) do
    :erlang.display(error)
  end
end
