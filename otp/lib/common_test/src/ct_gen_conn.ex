defmodule :m_ct_gen_conn do
  use Bitwise
  require Record

  Record.defrecord(:r_gen_opts, :gen_opts,
    callback: :undefined,
    name: :undefined,
    address: :undefined,
    init_data: :undefined,
    reconnect: true,
    forward: false,
    use_existing: true,
    old: false,
    conn_pid: :undefined,
    cb_state: :undefined,
    ct_util_server: :undefined
  )

  def start(address, initData, callbackMod, opts)
      when is_list(opts) do
    do_start(address, initData, callbackMod, opts)
  end

  def start(name, address, initData, callbackMod) do
    do_start(address, initData, callbackMod, [{:name, name}, {:old, true}])
  end

  def stop(handle) do
    call(handle, :stop, 5000)
  end

  def get_conn_pid(handle) do
    call(handle, :get_conn_pid)
  end

  def log(heading, format, args) do
    log(:log, [heading, format, args])
  end

  def start_log(heading) do
    log(:start_log, [heading])
  end

  def cont_log(format, args) do
    log(:cont_log, [format, args])
  end

  def cont_log_no_timestamp(format, args) do
    log(:cont_log_no_timestamp, [format, args])
  end

  def end_log() do
    log(:end_log, [])
  end

  def do_within_time(fun, timeout) do
    self = self()
    silent = :erlang.get(:silent)

    tmpPid =
      spawn_link(fn ->
        :ct_util.mark_process()
        :erlang.put(:silent, silent)
        r = fun.()
        send(self, {self(), r})
      end)

    connPid = :erlang.get(:conn_pid)

    receive do
      {^tmpPid, result} ->
        result

      {:EXIT, ^connPid, _Reason} = m ->
        :erlang.unlink(tmpPid)
        :erlang.exit(tmpPid, :kill)
        send(self(), m)
        {:error, :connection_closed}
    after
      timeout ->
        :erlang.exit(tmpPid, :kill)

        receive do
          {^tmpPid, result} ->
            receive do
              {:EXIT, ^tmpPid, _reason} ->
                :ok
            end

            result

          {:EXIT, ^tmpPid, :killed} ->
            {:error, :timeout}
        end
    end
  end

  defp do_start(address, initData, callbackMod, opts0) do
    opts =
      check_opts(
        opts0,
        r_gen_opts(callback: callbackMod, address: address, init_data: initData)
      )

    case :ct_util.does_connection_exist(r_gen_opts(opts, :name), address, callbackMod) do
      {:ok, pid} when r_gen_opts(opts, :use_existing) ->
        log('ct_gen_conn:start', 'Using existing connection!\n', [])
        {:ok, pid}

      {:ok, pid} when not r_gen_opts(opts, :use_existing) ->
        {:error, {:connection_exists, pid}}

      false ->
        do_start(opts)
    end
  end

  defp do_start(opts) do
    self = self()

    pid =
      spawn(fn ->
        init_gen(self, opts)
      end)

    mRef = :erlang.monitor(:process, pid)

    receive do
      {:connected, ^pid} ->
        :erlang.demonitor(mRef, [:flush])

        :ct_util.register_connection(
          r_gen_opts(opts, :name),
          r_gen_opts(opts, :address),
          r_gen_opts(opts, :callback),
          pid
        )

        {:ok, pid}

      {error, ^pid} ->
        receive do
          {:DOWN, ^mRef, :process, _, _} ->
            :ok
        end

        error

      {:DOWN, ^mRef, :process, _, reason} ->
        log('ct_gen_conn:start', 'Connection process died: ~tp\n', [reason])
        {:error, {:connection_process_died, reason}}
    end
  end

  def check_opts(opts0) do
    check_opts(opts0, r_gen_opts())
  end

  defp check_opts([{:name, name} | t], opts) do
    check_opts(t, r_gen_opts(opts, name: name))
  end

  defp check_opts([{:reconnect, bool} | t], opts) do
    check_opts(t, r_gen_opts(opts, reconnect: bool))
  end

  defp check_opts([{:forward_messages, bool} | t], opts) do
    check_opts(t, r_gen_opts(opts, forward: bool))
  end

  defp check_opts([{:use_existing_connection, bool} | t], opts) do
    check_opts(t, r_gen_opts(opts, use_existing: bool))
  end

  defp check_opts([{:old, bool} | t], opts) do
    check_opts(t, r_gen_opts(opts, old: bool))
  end

  defp check_opts([], opts) do
    opts
  end

  def call(pid, msg) do
    call(pid, msg, :infinity)
  end

  def call(pid, msg, timeout) do
    mRef = :erlang.monitor(:process, pid)
    ref = make_ref()
    send(pid, {msg, {self(), ref}})

    receive do
      {^ref, result} ->
        :erlang.demonitor(mRef, [:flush])

        case result do
          {:retry, _Data} ->
            call(pid, result)

          other ->
            other
        end

      {:DOWN, ^mRef, :process, _, reason} ->
        {:error, {:process_down, pid, reason}}
    after
      timeout ->
        :erlang.demonitor(mRef, [:flush])
        log('ct_gen_conn', 'Connection process ~w not responding. Killing now!', [pid])
        :erlang.exit(pid, :kill)
        {:error, {:process_down, pid, :forced_termination}}
    end
  end

  def return({to, ref}, result) do
    send(to, {ref, result})
    :ok
  end

  defp init_gen(parent, opts) do
    :erlang.process_flag(:trap_exit, true)
    :ct_util.mark_process()
    :erlang.put(:silent, false)

    try do
      r_gen_opts(opts, :callback).init(
        r_gen_opts(opts, :name),
        r_gen_opts(opts, :address),
        r_gen_opts(opts, :init_data)
      )
    catch
      {:error, reason} ->
        send(parent, {{:error, reason}, self()})
    else
      {:ok, connPid, state} when is_pid(connPid) ->
        :erlang.link(connPid)
        :erlang.put(:conn_pid, connPid)
        ctUtilServer = :erlang.whereis(:ct_util_server)
        :erlang.link(ctUtilServer)
        send(parent, {:connected, self()})
        loop(r_gen_opts(opts, conn_pid: connPid, cb_state: state, ct_util_server: ctUtilServer))

      {:error, reason} ->
        send(parent, {{:error, reason}, self()})
    end
  end

  defp loop(opts) do
    receive do
      {:EXIT, pid, reason} when pid == r_gen_opts(opts, :conn_pid) ->
        case r_gen_opts(opts, :reconnect) do
          true ->
            log('Connection down!\nOpening new!', 'Reason: ~tp\nAddress: ~tp\n', [
              reason,
              r_gen_opts(opts, :address)
            ])

            case reconnect(opts) do
              {:ok, newPid, newState} ->
                :erlang.link(newPid)
                :erlang.put(:conn_pid, newPid)
                loop(r_gen_opts(opts, conn_pid: newPid, cb_state: newState))

              error ->
                :ct_util.unregister_connection(self())
                log('Reconnect failed. Giving up!', 'Reason: ~tp\n', [error])
            end

          false ->
            :ct_util.unregister_connection(self())
            log('Connection closed!', 'Reason: ~tp\n', [reason])
        end

      {:EXIT, pid, reason} ->
        case r_gen_opts(opts, :ct_util_server) do
          ^pid ->
            exit(reason)

          _ ->
            loop(opts)
        end

      {:stop, from} ->
        :ct_util.unregister_connection(self())
        connPid = r_gen_opts(opts, :conn_pid)
        :erlang.unlink(connPid)

        r_gen_opts(opts, :callback).terminate(
          connPid,
          r_gen_opts(opts, :cb_state)
        )

        return(from, :ok)
        :ok

      {{:retry, {error, _Name, cPid, _Msg}}, from}
      when cPid == r_gen_opts(opts, :conn_pid) ->
        return =
          case error do
            {:error, _} ->
              error

            reason ->
              {:error, reason}
          end

        return(from, return)
        loop(opts)

      {{:retry, {_Error, _Name, _CPid, msg}}, from} ->
        log('Rerunning command', 'Connection reestablished. Rerunning command...', [])

        {return, newState} =
          r_gen_opts(opts, :callback).handle_msg(
            msg,
            r_gen_opts(opts, :cb_state)
          )

        return(from, return)
        loop(r_gen_opts(opts, cb_state: newState))

      {:get_conn_pid, from} ->
        return(from, r_gen_opts(opts, :conn_pid))
        loop(opts)

      {msg, from = {pid, _Ref}}
      when is_pid(pid) and
             r_gen_opts(opts, :old) == true ->
        {return, newState} =
          r_gen_opts(opts, :callback).handle_msg(
            msg,
            r_gen_opts(opts, :cb_state)
          )

        return(from, return)
        loop(r_gen_opts(opts, cb_state: newState))

      {msg, from = {pid, _Ref}} when is_pid(pid) ->
        case r_gen_opts(opts, :callback).handle_msg(msg, from, r_gen_opts(opts, :cb_state)) do
          {:reply, reply, newState} ->
            return(from, reply)
            loop(r_gen_opts(opts, cb_state: newState))

          {:noreply, newState} ->
            loop(r_gen_opts(opts, cb_state: newState))

          {:stop, reply, newState} ->
            :ct_util.unregister_connection(self())
            connPid = r_gen_opts(opts, :conn_pid)
            :erlang.unlink(connPid)
            r_gen_opts(opts, :callback).terminate(connPid, newState)
            return(from, reply)
        end

      msg when r_gen_opts(opts, :forward) == true ->
        case r_gen_opts(opts, :callback).handle_msg(
               msg,
               r_gen_opts(opts, :cb_state)
             ) do
          {:noreply, newState} ->
            loop(r_gen_opts(opts, cb_state: newState))

          {:stop, newState} ->
            :ct_util.unregister_connection(self())
            connPid = r_gen_opts(opts, :conn_pid)
            :erlang.unlink(connPid)
            r_gen_opts(opts, :callback).terminate(connPid, newState)
        end
    end
  end

  defp reconnect(opts) do
    r_gen_opts(opts, :callback).reconnect(
      r_gen_opts(opts, :address),
      r_gen_opts(opts, :cb_state)
    )
  end

  defp log(func, args) do
    case :erlang.get(:silent) do
      true when not false ->
        :ok

      _ ->
        apply(:ct_logs, func, args)
    end
  end
end
