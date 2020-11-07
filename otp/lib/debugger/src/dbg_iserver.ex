defmodule :m_dbg_iserver do
  use Bitwise
  import Kernel, except: [send: 2]
  @behaviour :gen_server
  require Record

  Record.defrecord(:r_proc, :proc,
    pid: :undefined,
    meta: :undefined,
    attpid: :undefined,
    status: :undefined,
    info: {},
    exit_info: {},
    function: :undefined
  )

  Record.defrecord(:r_state, :state,
    db: :undefined,
    procs: [],
    breaks: [],
    auto: :undefined,
    stack: :undefined,
    subs: []
  )

  def start() do
    :gen_server.start({:local, :dbg_iserver}, :dbg_iserver, [], [])
  end

  def stop() do
    :gen_server.cast(:dbg_iserver, :stop)
  end

  def find() do
    :global.whereis_name(:dbg_iserver)
  end

  def call(request) do
    :gen_server.call(:dbg_iserver, request, :infinity)
  end

  def call(int, request) do
    :gen_server.call(int, request, :infinity)
  end

  def cast(request) do
    :gen_server.cast(:dbg_iserver, request)
  end

  def cast(int, request) do
    :gen_server.cast(int, request)
  end

  def safe_call(request) do
    {:ok, _} = ensure_started()
    call(request)
  end

  def safe_cast(request) do
    {:ok, _} = ensure_started()
    cast(request)
  end

  defp ensure_started() do
    case :erlang.whereis(:dbg_iserver) do
      :undefined ->
        start()

      pid ->
        {:ok, pid}
    end
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)
    :global.register_name(:dbg_iserver, self())
    db = :ets.new(:dbg_iserver, [:ordered_set, :protected])
    {:ok, r_state(db: db, auto: false, stack: :no_tail)}
  end

  def handle_call({:attached, attPid, pid}, _From, state) do
    {true, proc} = get_proc({:pid, pid}, r_state(state, :procs))

    case r_proc(proc, :attpid) do
      :undefined ->
        :erlang.link(attPid)

        case r_proc(proc, :status) do
          :exit ->
            args = [self(), attPid, pid, r_proc(proc, :info), r_proc(proc, :exit_info)]
            meta = spawn_link(:dbg_ieval, :exit_info, args)
            proc2 = r_proc(proc, meta: meta, attpid: attPid)
            procs = :lists.keyreplace(pid, r_proc(:pid), r_state(state, :procs), proc2)
            {:reply, {:ok, meta}, r_state(state, procs: procs)}

          _Status ->
            meta = r_proc(proc, :meta)
            send(meta, {:attached, attPid})

            procs =
              :lists.keyreplace(
                pid,
                r_proc(:pid),
                r_state(state, :procs),
                r_proc(proc, attpid: attPid)
              )

            {:reply, {:ok, meta}, r_state(state, procs: procs)}
        end

      _AttPid ->
        {:reply, :error, state}
    end
  end

  def handle_call(:get_auto_attach, _From, state) do
    {:reply, r_state(state, :auto), state}
  end

  def handle_call(:get_stack_trace, _From, state) do
    {:reply, r_state(state, :stack), state}
  end

  def handle_call(:snapshot, _From, state) do
    reply =
      for proc <- r_state(state, :procs) do
        {r_proc(proc, :pid), r_proc(proc, :function), r_proc(proc, :status), r_proc(proc, :info)}
      end

    {:reply, reply, state}
  end

  def handle_call({:get_meta, pid}, _From, state) do
    reply =
      case get_proc(
             {:pid, pid},
             r_state(state, :procs)
           ) do
        {true, proc} ->
          {:ok, r_proc(proc, :meta)}

        false ->
          {:error, :not_interpreted}
      end

    {:reply, reply, state}
  end

  def handle_call({:get_attpid, pid}, _From, state) do
    reply =
      case get_proc(
             {:pid, pid},
             r_state(state, :procs)
           ) do
        {true, proc} ->
          {:ok, r_proc(proc, :attpid)}

        false ->
          {:error, :not_interpreted}
      end

    {:reply, reply, state}
  end

  def handle_call({:new_break, point, options}, _From, state) do
    case :lists.keymember(point, 1, r_state(state, :breaks)) do
      false ->
        break = {point, options}
        send_all([:subscriber, :meta, :attached], {:new_break, break}, state)
        breaks = keyinsert(break, 1, r_state(state, :breaks))
        {:reply, :ok, r_state(state, breaks: breaks)}

      true ->
        {:reply, {:error, :break_exists}, state}
    end
  end

  def handle_call(:all_breaks, _From, state) do
    {:reply, r_state(state, :breaks), state}
  end

  def handle_call({:all_breaks, mod}, _From, state) do
    reply =
      for break = {{m, _}, _} <- r_state(state, :breaks),
          m === mod do
        break
      end

    {:reply, reply, state}
  end

  def handle_call({:new_process, pid, meta, function}, _From, state) do
    :erlang.link(meta)

    reply =
      case auto_attach(:init, r_state(state, :auto), pid) do
        attPid when is_pid(attPid) ->
          :break

        :ignore ->
          :running
      end

    proc = r_proc(pid: pid, meta: meta, status: :running, function: function)
    send_all(:subscriber, {:new_process, {pid, function, :running, {}}}, state)
    {:reply, reply, r_state(state, procs: r_state(state, :procs) ++ [proc])}
  end

  def handle_call({:load, mod, src, bin}, _From, state) do
    db = r_state(state, :db)
    modDb = :ets.new(mod, [:ordered_set, :public])

    modDbs =
      case :ets.lookup(db, {mod, :refs}) do
        [] ->
          []

        [{{^mod, :refs}, modDbs1}] ->
          modDbs1
      end

    :ets.insert(db, {{mod, :refs}, [modDb | modDbs]})
    :ets.insert(db, {modDb, []})
    {:ok, ^mod} = :dbg_iload.load_mod(mod, src, bin, modDb)
    send_all([:subscriber, :attached], {:interpret, mod}, state)
    {:reply, {:module, mod}, state}
  end

  def handle_call({:get_module_db, mod, pid}, _From, state) do
    db = r_state(state, :db)

    reply =
      case :ets.lookup(db, {mod, :refs}) do
        [] ->
          :not_found

        [{{^mod, :refs}, [modDb | _ModDbs]}] ->
          [{^modDb, pids}] = :ets.lookup(db, modDb)
          :ets.insert(db, {modDb, [pid | pids]})
          modDb
      end

    {:reply, reply, state}
  end

  def handle_call({:lookup, mod, key}, _From, state) do
    db = r_state(state, :db)

    reply =
      case :ets.lookup(db, {mod, :refs}) do
        [] ->
          :not_found

        [{{^mod, :refs}, [modDb | _ModDbs]}] ->
          case :ets.lookup(modDb, key) do
            [] ->
              :not_found

            [{^key, value}] ->
              {:ok, value}
          end
      end

    {:reply, reply, state}
  end

  def handle_call({:functions, mod}, _From, state) do
    db = r_state(state, :db)

    reply =
      case :ets.lookup(db, {mod, :refs}) do
        [] ->
          []

        [{{^mod, :refs}, [modDb | _ModDbs]}] ->
          pattern = {{mod, :"$1", :"$2", :_}, :_}
          :ets.match(modDb, pattern)
      end

    {:reply, reply, state}
  end

  def handle_call({:contents, mod, pid}, _From, state) do
    db = r_state(state, :db)

    [{{^mod, :refs}, modDbs}] =
      :ets.lookup(
        db,
        {mod, :refs}
      )

    modDb =
      cond do
        pid === :any ->
          hd(modDbs)

        true ->
          :lists.foldl(
            fn
              t, :not_found ->
                [{^t, pids}] = :ets.lookup(db, t)

                case :lists.member(pid, pids) do
                  true ->
                    t

                  false ->
                    :not_found
                end

              _T, t ->
                t
            end,
            :not_found,
            modDbs
          )
      end

    [{:mod_bin, bin}] = :ets.lookup(modDb, :mod_bin)
    {:reply, {:ok, bin}, state}
  end

  def handle_call({:raw_contents, mod, pid}, _From, state) do
    db = r_state(state, :db)

    case :ets.lookup(db, {mod, :refs}) do
      [{{^mod, :refs}, modDbs}] ->
        modDb =
          cond do
            pid === :any ->
              hd(modDbs)

            true ->
              :lists.foldl(
                fn
                  t, :not_found ->
                    [{^t, pids}] = :ets.lookup(db, t)

                    case :lists.member(pid, pids) do
                      true ->
                        t

                      false ->
                        :not_found
                    end

                  _T, t ->
                    t
                end,
                :not_found,
                modDbs
              )
          end

        [{:mod_raw, bin}] = :ets.lookup(modDb, :mod_raw)
        {:reply, {:ok, bin}, state}

      [] ->
        {:reply, :not_found, state}
    end
  end

  def handle_call({:is_interpreted, mod, name, arity}, _From, state) do
    db = r_state(state, :db)

    reply =
      case :ets.lookup(db, {mod, :refs}) do
        [] ->
          false

        [{{^mod, :refs}, [modDb | _ModDbs]}] ->
          pattern = {{mod, name, arity, :_}, :_}

          case :ets.match_object(modDb, pattern) do
            [{_Key, clauses}] ->
              {true, clauses}

            [] ->
              false
          end
      end

    {:reply, reply, state}
  end

  def handle_call(:all_interpreted, _From, state) do
    db = r_state(state, :db)
    mods = :ets.select(db, [{{{:"$1", :refs}, :_}, [], [:"$1"]}])
    {:reply, mods, state}
  end

  def handle_call({:file, mod}, from, state) do
    {:reply, res, _} = handle_call({:lookup, mod, :mod_file}, from, state)

    reply =
      case res do
        {:ok, file} ->
          file

        :not_found ->
          {:error, :not_loaded}
      end

    {:reply, reply, state}
  end

  def handle_cast(:stop, state) do
    {:stop, :shutdown, state}
  end

  def handle_cast({:subscribe, sub}, state) do
    {:noreply, r_state(state, subs: [sub | r_state(state, :subs)])}
  end

  def handle_cast({:attach, pid, {mod, func, args}}, state) do
    spawn(mod, func, [pid | args])
    {:noreply, state}
  end

  def handle_cast({:set_auto_attach, false}, state) do
    send_all(:subscriber, {:auto_attach, false}, state)
    {:noreply, r_state(state, auto: false)}
  end

  def handle_cast({:set_auto_attach, flags, function}, state) do
    send_all(:subscriber, {:auto_attach, {flags, function}}, state)
    {:noreply, r_state(state, auto: {flags, function})}
  end

  def handle_cast({:set_stack_trace, flag}, state) do
    send_all(:subscriber, {:stack_trace, flag}, state)
    {:noreply, r_state(state, stack: flag)}
  end

  def handle_cast(:clear, state) do
    procs =
      :lists.filter(
        fn r_proc(status: status) ->
          status !== :exit
        end,
        r_state(state, :procs)
      )

    {:noreply, r_state(state, procs: procs)}
  end

  def handle_cast({:delete_break, point}, state) do
    case :lists.keymember(point, 1, r_state(state, :breaks)) do
      true ->
        send_all([:subscriber, :meta, :attached], {:delete_break, point}, state)
        breaks = :lists.keydelete(point, 1, r_state(state, :breaks))
        {:noreply, r_state(state, breaks: breaks)}

      false ->
        {:noreply, state}
    end
  end

  def handle_cast({:break_option, point, option, value}, state) do
    case :lists.keyfind(point, 1, r_state(state, :breaks)) do
      {^point, options} ->
        n =
          case option do
            :status ->
              1

            :action ->
              2

            :condition ->
              4
          end

        options2 = list_setelement(n, options, value)
        send_all([:subscriber, :meta, :attached], {:break_options, {point, options2}}, state)
        breaks = :lists.keyreplace(point, 1, r_state(state, :breaks), {point, options2})
        {:noreply, r_state(state, breaks: breaks)}

      false ->
        {:noreply, state}
    end
  end

  def handle_cast(:no_break, state) do
    send_all([:subscriber, :meta, :attached], :no_break, state)
    {:noreply, r_state(state, breaks: [])}
  end

  def handle_cast({:no_break, mod}, state) do
    send_all([:subscriber, :meta, :attached], {:no_break, mod}, state)

    breaks =
      :lists.filter(
        fn {{m, _L}, _O} ->
          m !== mod
        end,
        r_state(state, :breaks)
      )

    {:noreply, r_state(state, breaks: breaks)}
  end

  def handle_cast({:set_status, meta, status, info}, state) do
    {true, proc} = get_proc({:meta, meta}, r_state(state, :procs))
    send_all(:subscriber, {:new_status, r_proc(proc, :pid), status, info}, state)

    cond do
      status === :break ->
        _ = auto_attach(:break, r_state(state, :auto), proc)
        :ok

      true ->
        :ok
    end

    proc2 = r_proc(proc, status: status, info: info)

    {:noreply,
     r_state(state, procs: :lists.keyreplace(meta, r_proc(:meta), r_state(state, :procs), proc2))}
  end

  def handle_cast({:set_exit_info, meta, exitInfo}, state) do
    {true, proc} = get_proc({:meta, meta}, r_state(state, :procs))

    procs =
      :lists.keyreplace(
        meta,
        r_proc(:meta),
        r_state(state, :procs),
        r_proc(proc, exit_info: exitInfo)
      )

    {:noreply, r_state(state, procs: procs)}
  end

  def handle_cast({:delete, mod}, state) do
    db = r_state(state, :db)

    case :ets.lookup(db, {mod, :refs}) do
      [] ->
        {:noreply, state}

      [{{^mod, :refs}, modDbs}] ->
        :ets.delete(db, {mod, :refs})

        allPids =
          :lists.foldl(
            fn modDb, pidsAcc ->
              [{^modDb, pids}] = :ets.lookup(db, modDb)
              :ets.delete(db, modDb)
              :ets.delete(modDb)
              pidsAcc ++ pids
            end,
            [],
            modDbs
          )

        :lists.foreach(
          fn pid ->
            case get_proc({:pid, pid}, r_state(state, :procs)) do
              {true, proc} ->
                send(r_proc(proc, :meta), {:old_code, mod})

              false ->
                :ignore
            end
          end,
          allPids
        )

        send_all([:subscriber, :attached], {:no_interpret, mod}, state)
        handle_cast({:no_break, mod}, state)
    end
  end

  def handle_info({:EXIT, who, why}, state) do
    case get_proc({:meta, who}, r_state(state, :procs)) do
      {true, r_proc(status: :exit)} ->
        {:noreply, state}

      {true, proc} ->
        pid = r_proc(proc, :pid)
        exitInfo = r_proc(proc, :exit_info)

        meta =
          case r_proc(proc, :attpid) do
            attPid when is_pid(attPid) ->
              spawn_link(:dbg_ieval, :exit_info, [self(), attPid, pid, why, exitInfo])

            :undefined ->
              _ = auto_attach(:exit, r_state(state, :auto), pid)
              who
          end

        send_all(:subscriber, {:new_status, pid, :exit, why}, state)

        procs =
          :lists.keyreplace(
            who,
            r_proc(:meta),
            r_state(state, :procs),
            r_proc(proc, meta: meta, status: :exit, info: why)
          )

        {:noreply, r_state(state, procs: procs)}

      false ->
        case get_proc({:attpid, who}, r_state(state, :procs)) do
          {true, proc} ->
            case r_proc(proc, :status) do
              :exit ->
                send(r_proc(proc, :meta), :stop)

              _Status ->
                send(r_proc(proc, :meta), :detached)
            end

            procs =
              :lists.keyreplace(
                r_proc(proc, :pid),
                r_proc(:pid),
                r_state(state, :procs),
                r_proc(proc, attpid: :undefined)
              )

            {:noreply, r_state(state, procs: procs)}

          false ->
            subs = :lists.delete(who, r_state(state, :subs))
            {:noreply, r_state(state, subs: subs)}
        end
    end
  end

  def terminate(_Reason, _State) do
    ebinDir = :filename.join(:code.lib_dir(:debugger), 'ebin')
    :code.unstick_dir(ebinDir)
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp auto_attach(why, auto, r_proc(attpid: attpid, pid: pid)) do
    case attpid do
      :undefined ->
        auto_attach(why, auto, pid)

      _ when is_pid(attpid) ->
        :ignore
    end
  end

  defp auto_attach(why, auto, pid) when is_pid(pid) do
    case auto do
      false ->
        :ignore

      {flags, {mod, func, args}} ->
        case :lists.member(why, flags) do
          true ->
            spawn(mod, func, [pid | args])

          false ->
            :ignore
        end
    end
  end

  defp keyinsert(tuple1, n, [tuple2 | tuples]) do
    cond do
      :erlang.element(n, tuple1) <
          :erlang.element(
            n,
            tuple2
          ) ->
        [[tuple1, tuple2] | tuples]

      true ->
        [tuple2 | keyinsert(tuple1, n, tuples)]
    end
  end

  defp keyinsert(tuple, _N, []) do
    [tuple]
  end

  defp list_setelement(n, l, e) do
    list_setelement(1, n, l, e)
  end

  defp list_setelement(i, i, [_ | t], e) do
    [e | t]
  end

  defp list_setelement(i, n, [h | t], e) do
    [h | list_setelement(i + 1, n, t, e)]
  end

  defp mapfilter(fun, [h | t]) do
    case fun.(h) do
      :ignore ->
        mapfilter(fun, t)

      h2 ->
        [h2 | mapfilter(fun, t)]
    end
  end

  defp mapfilter(_Fun, []) do
    []
  end

  defp send_all([type | types], msg, state) do
    send_all(type, msg, state)
    send_all(types, msg, state)
  end

  defp send_all([], _Msg, _State) do
    :ok
  end

  defp send_all(:subscriber, msg, state) do
    send_all(r_state(state, :subs), msg)
  end

  defp send_all(:meta, msg, state) do
    metas =
      for proc <- r_state(state, :procs) do
        r_proc(proc, :meta)
      end

    send_all(metas, msg)
  end

  defp send_all(:attached, msg, state) do
    attPids =
      mapfilter(
        fn proc ->
          case r_proc(proc, :attpid) do
            pid when is_pid(pid) ->
              pid

            :undefined ->
              :ignore
          end
        end,
        r_state(state, :procs)
      )

    send_all(attPids, msg)
  end

  defp send_all(pids, msg) do
    :lists.foreach(
      fn pid ->
        send(pid, msg)
      end,
      pids
    )
  end

  defp send(pid, msg) do
    send(pid, {:int, msg})
    :ok
  end

  defp get_proc({type, pid}, procs) do
    index =
      case type do
        :pid ->
          r_proc(:pid)

        :meta ->
          r_proc(:meta)

        :attpid ->
          r_proc(:attpid)
      end

    case :lists.keyfind(pid, index, procs) do
      false ->
        false

      proc ->
        {true, proc}
    end
  end
end
