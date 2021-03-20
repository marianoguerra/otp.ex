defmodule :m_fprof do
  use Bitwise
  import Kernel, except: [apply: 3, apply: 2]
  @author :"raimo@erix.ericsson.se"
  defp dbg(level, f, a) when level >= 9 do
    :io.format(f, a)
    :ok
  end

  defp dbg(_, _, _) do
    :ok
  end

  def apply({m, f}, args)
      when is_atom(m) and is_atom(f) and
             is_list(args) do
    apply_1(m, f, args, [])
  end

  def apply(fun, args)
      when is_function(fun) and
             is_list(args) do
    apply_1(fun, args, [])
  end

  def apply(a, b) do
    :erlang.error(:badarg, [a, b])
  end

  def apply(m, f, args)
      when is_atom(m) and is_atom(f) and
             is_list(args) do
    apply_1(m, f, args, [])
  end

  def apply({m, f}, args, options)
      when is_atom(m) and
             is_atom(f) and is_list(args) and
             is_list(options) do
    apply_1(m, f, args, options)
  end

  def apply(fun, args, options)
      when is_function(fun) and
             is_list(args) and is_list(options) do
    apply_1(fun, args, options)
  end

  def apply(a, b, c) do
    :erlang.error(:badarg, [a, b, c])
  end

  def apply(m, f, args, options)
      when is_atom(m) and
             is_atom(f) and is_list(args) and
             is_list(options) do
    apply_1(m, f, args, options)
  end

  def apply(a, b, c, d) do
    :erlang.error(:badarg, [a, b, c, d])
  end

  defp apply_1(m, f, args, options) do
    arity = length(args)
    apply_1(Function.capture(m, f, arity), args, options)
  end

  defp apply_1(function, args, options) do
    {[_, procs, continue], options_1} =
      getopts(
        options,
        [:start, :procs, :continue]
      )

    procs_1 =
      case procs do
        [{:procs, p}] when is_list(p) ->
          p

        _ ->
          []
      end

    case continue do
      [] ->
        apply_start_stop(function, args, procs_1, options_1)

      [:continue] ->
        apply_continue(function, args, procs_1, options_1)

      _ ->
        :erlang.error(:badarg, [function, args, options])
    end
  end

  defp apply_start_stop(function, args, procs, options) do
    ref = make_ref()
    parent = self()

    child =
      spawn(fn ->
        mRef = :erlang.monitor(:process, parent)

        receive do
          {^parent, ^ref, :start_trace} ->
            case trace([
                   :start,
                   {:procs, [parent | procs]}
                   | options
                 ]) do
              :ok ->
                try do
                  send(parent, {self(), ref, :trace_started})
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end

                receive do
                  {^parent, ^ref, :stop_trace} ->
                    trace([:stop])

                    try do
                      send(parent, {self(), ref, :trace_stopped})
                    catch
                      :error, e -> {:EXIT, {e, __STACKTRACE__}}
                      :exit, e -> {:EXIT, e}
                      e -> e
                    end

                    :done

                  {:DOWN, ^mRef, _, _, _} ->
                    trace([:stop])
                end

              {:error, reason} ->
                exit(reason)
            end

          {:DOWN, ^mRef, _, _, _} ->
            :done
        end
      end)

    mRef = :erlang.monitor(:process, child)

    try do
      send(child, {self(), ref, :start_trace})
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    receive do
      {^child, ^ref, :trace_started} ->
        try do
          :erlang.apply(function, args)
        after
          try do
            send(child, {self(), ref, :stop_trace})
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

          receive do
            {^child, ^ref, :trace_stopped} ->
              receive do
                {:DOWN, ^mRef, _, _, _} ->
                  :ok
              end

            {:DOWN, ^mRef, _, _, _} ->
              trace([:stop])
          end
        end

      {:DOWN, ^mRef, _, _, reason} ->
        exit(reason)
    end
  end

  defp apply_continue(function, args, procs, options) do
    ref = make_ref()
    parent = self()

    child =
      spawn(fn ->
        mRef = :erlang.monitor(:process, parent)

        receive do
          {^parent, ^ref, :start_trace} ->
            case trace([
                   :start,
                   {:procs, [parent | procs]}
                   | options
                 ]) do
              :ok ->
                exit({ref, :trace_started})

              {:error, reason} ->
                exit(reason)
            end

          {:DOWN, ^mRef, _, _, _} ->
            :done
        end
      end)

    mRef = :erlang.monitor(:process, child)

    try do
      send(child, {self(), ref, :start_trace})
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    receive do
      {:DOWN, ^mRef, _, _, {^ref, :trace_started}} ->
        :erlang.apply(function, args)

      {:DOWN, ^mRef, _, _, reason} ->
        exit(reason)
    end
  end

  require Record

  Record.defrecord(:r_trace_start, :trace_start,
    procs: :undefined,
    mode: :undefined,
    type: :undefined,
    dest: :undefined
  )

  Record.defrecord(:r_trace_stop, :trace_stop, [])

  Record.defrecord(:r_profile, :profile,
    src: :undefined,
    group_leader: :undefined,
    dump: :undefined,
    flags: :undefined
  )

  Record.defrecord(:r_profile_start, :profile_start,
    group_leader: :undefined,
    dump: :undefined,
    flags: :undefined
  )

  Record.defrecord(:r_profile_stop, :profile_stop, [])

  Record.defrecord(:r_analyse, :analyse,
    group_leader: :undefined,
    dest: :undefined,
    flags: :undefined,
    cols: :undefined,
    callers: :undefined,
    sort: :undefined,
    totals: :undefined,
    details: :undefined
  )

  Record.defrecord(:r_stop, :stop, reason: :undefined)
  Record.defrecord(:r_get_state, :get_state, [])
  Record.defrecord(:r_save_profile, :save_profile, file: :undefined)
  Record.defrecord(:r_load_profile, :load_profile, file: :undefined)

  def trace(:start, filename) do
    trace([:start, {:file, filename}])
  end

  def trace(:verbose, filename) do
    trace([:start, :verbose, {:file, filename}])
  end

  def trace(option, value) when is_atom(option) do
    trace([{option, value}])
  end

  def trace(option, value) do
    :erlang.error(:badarg, [option, value])
  end

  def trace(:stop) do
    call(r_trace_stop())
  end

  def trace(:verbose) do
    trace([:start, :verbose])
  end

  def trace([:stop]) do
    call(r_trace_stop())
  end

  def trace({opt, _Val} = option) when is_atom(opt) do
    trace([option])
  end

  def trace(option) when is_atom(option) do
    trace([option])
  end

  def trace(options) when is_list(options) do
    case getopts(
           options,
           [:start, :stop, :procs, :verbose, :file, :tracer, :cpu_time]
         ) do
      {[[], [:stop], [], [], [], [], []], []} ->
        call(r_trace_stop())

      {[[:start], [], procs, verbose, file, tracer, cpuTime], []} ->
        {type, dest} =
          case {file, tracer} do
            {[], [{:tracer, pid} = t]}
            when is_pid(pid) or
                   is_port(pid) ->
              t

            {[:file], []} ->
              {:file, 'fprof.trace'}

            {[{:file, []}], []} ->
              {:file, 'fprof.trace'}

            {[{:file, _} = f], []} ->
              f

            {[], []} ->
              {:file, 'fprof.trace'}

            _ ->
              :erlang.error(:badarg, [options])
          end

        v =
          case verbose do
            [] ->
              :normal

            [:verbose] ->
              :verbose

            [{:verbose, true}] ->
              :verbose

            [{:verbose, false}] ->
              :normal

            _ ->
              :erlang.error(:badarg, [options])
          end

        cT =
          case cpuTime do
            [] ->
              :wallclock

            [:cpu_time] ->
              :cpu_time

            [{:cpu_time, true}] ->
              :cpu_time

            [{:cpu_time, false}] ->
              :wallclock

            _ ->
              :erlang.error(:badarg, [options])
          end

        call(
          r_trace_start(
            procs:
              case procs do
                [] ->
                  [self()]

                [{:procs, p}] when is_list(p) ->
                  p

                [{:procs, p}] ->
                  [p]

                _ ->
                  :erlang.error(:badarg, [options])
              end,
            mode: {v, cT},
            type: type,
            dest: dest
          )
        )

      _ ->
        :erlang.error(:badarg, [options])
    end
  end

  def trace(options) do
    :erlang.error(:badarg, [options])
  end

  def profile() do
    profile([])
  end

  def profile(option, value) when is_atom(option) do
    profile([{option, value}])
  end

  def profile(option, value) do
    :erlang.error(:badarg, [option, value])
  end

  def profile(option) when is_atom(option) do
    profile([option])
  end

  def profile({opt, _Val} = option) when is_atom(opt) do
    profile([option])
  end

  def profile(options) when is_list(options) do
    case getopts(
           options,
           [:start, :stop, :file, :dump, :append]
         ) do
      {[start, [], file, dump, append], []} ->
        {target, flags} =
          case {dump, append} do
            {[], []} ->
              {[], []}

            {[:dump], []} ->
              {:erlang.group_leader(), []}

            {[{:dump, []}], []} ->
              {'fprof.dump', []}

            {[{:dump, []}], [:append]} ->
              {'fprof.dump', [:append]}

            {[{:dump, d}], [:append]} when is_pid(d) ->
              :erlang.error(:badarg, [options])

            {[{:dump, d}], [:append]} ->
              {d, [:append]}

            {[{:dump, d}], []} ->
              {d, []}

            _ ->
              :erlang.error(:badarg, [options])
          end

        case {start, file} do
          {[:start], []} ->
            call(
              r_profile_start(group_leader: :erlang.group_leader(), dump: target, flags: flags)
            )

          {[], _} ->
            src =
              case file do
                [] ->
                  'fprof.trace'

                [:file] ->
                  'fprof.trace'

                [{:file, []}] ->
                  'fprof.trace'

                [{:file, f}] ->
                  f

                _ ->
                  :erlang.error(:badarg, [options])
              end

            call(
              r_profile(
                src: src,
                group_leader: :erlang.group_leader(),
                dump: target,
                flags: flags
              )
            )

          _ ->
            :erlang.error(:badarg, [options])
        end

      {[[], [:stop], [], [], []], []} ->
        call(r_profile_stop())

      _ ->
        :erlang.error(:badarg, [options])
    end
  end

  def profile(options) do
    :erlang.error(:badarg, [options])
  end

  def analyse() do
    analyse([])
  end

  def analyse(option, value) when is_atom(option) do
    analyse([{option, value}])
  end

  def analyse(option, value) do
    :erlang.error(:badarg, [option, value])
  end

  def analyse(option) when is_atom(option) do
    analyse([option])
  end

  def analyse({opt, _Val} = option) when is_atom(opt) do
    analyse([option])
  end

  def analyse(options) when is_list(options) do
    case getopts(
           options,
           [:dest, :append, :cols, :callers, :no_callers, :sort, :totals, :details, :no_details]
         ) do
      {[dest, append, cols, callers, noCallers, sort, totals, details, noDetails], []} ->
        {target, flags} =
          case {dest, append} do
            {[], []} ->
              {:erlang.group_leader(), []}

            {[:dest], []} ->
              {:erlang.group_leader(), []}

            {[{:dest, []}], []} ->
              {'fprof.analysis', []}

            {[{:dest, []}], [:append]} ->
              {'fprof.analysis', [:append]}

            {[{:dest, f}], [:append]} when is_pid(f) ->
              :erlang.error(:badarg, [options])

            {[{:dest, f}], [:append]} ->
              {f, [:append]}

            {[{:dest, f}], []} ->
              {f, []}

            _ ->
              :erlang.error(:badarg, [options])
          end

        call(
          r_analyse(
            group_leader: :erlang.group_leader(),
            dest: target,
            flags: flags,
            cols:
              case cols do
                [] ->
                  80

                [{:cols, c}] when is_integer(c) and c > 0 ->
                  c

                _ ->
                  :erlang.error(:badarg, [options])
              end,
            callers:
              case {callers, noCallers} do
                {[], []} ->
                  true

                {[:callers], []} ->
                  true

                {[{:callers, true}], []} ->
                  true

                {[{:callers, false}], []} ->
                  false

                {[], [:no_callers]} ->
                  false

                _ ->
                  :erlang.error(:badarg, [options])
              end,
            sort:
              case sort do
                [] ->
                  :acc

                [{:sort, :acc}] ->
                  :acc

                [{:sort, :own}] ->
                  :own

                _ ->
                  :erlang.error(:badarg, [options])
              end,
            totals:
              case totals do
                [] ->
                  false

                [:totals] ->
                  true

                [{:totals, true}] ->
                  true

                [{:totals, false}] ->
                  false

                _ ->
                  :erlang.error(:badarg, [options])
              end,
            details:
              case {details, noDetails} do
                {[], []} ->
                  true

                {[:details], []} ->
                  true

                {[{:details, true}], []} ->
                  true

                {[{:details, false}], []} ->
                  false

                {[], [:no_details]} ->
                  false

                _ ->
                  :erlang.error(:badarg, [options])
              end
          )
        )

      _ ->
        :erlang.error(:badarg, [options])
    end
  end

  def analyse(options) do
    :erlang.error(:badarg, [options])
  end

  def get_state() do
    just_call(r_get_state())
  end

  def save_profile() do
    save_profile([])
  end

  def save_profile(option, value) when is_atom(option) do
    save_profile([{option, value}])
  end

  def save_profile(option, value) do
    :erlang.error(:badarg, [option, value])
  end

  def save_profile(option) when is_atom(option) do
    save_profile([option])
  end

  def save_profile(options) when is_list(options) do
    case getopts(options, [:file]) do
      {[file], []} ->
        call(
          r_save_profile(
            file:
              case file do
                [] ->
                  'fprof.profile'

                [{:file, f}] ->
                  f

                _ ->
                  :erlang.error(:badarg, [options])
              end
          )
        )

      _ ->
        :erlang.error(:badarg, [options])
    end
  end

  def save_profile(options) do
    :erlang.error(:badarg, [options])
  end

  def load_profile() do
    load_profile([])
  end

  def load_profile(option, value) when is_atom(option) do
    load_profile([{option, value}])
  end

  def load_profile(option, value) do
    :erlang.error(:badarg, [option, value])
  end

  def load_profile(option) when is_atom(option) do
    load_profile([option])
  end

  def load_profile(options) when is_list(options) do
    case getopts(options, [:file]) do
      {[file], []} ->
        call(
          r_load_profile(
            file:
              case file do
                [] ->
                  'fprof.profile'

                [{:file, f}] ->
                  f

                _ ->
                  :erlang.error(:badarg, [options])
              end
          )
        )

      _ ->
        :erlang.error(:badarg, [options])
    end
  end

  def load_profile(options) do
    :erlang.error(:badarg, [options])
  end

  def code_change() do
    just_call(:"$code_change")
  end

  Record.defrecord(:r_clocks, :clocks, id: :undefined, cnt: 0, own: 0, acc: 0)

  Record.defrecord(:r_proc, :proc,
    id: :undefined,
    parent: :undefined,
    spawned_as: :undefined,
    init_log: [],
    init_cnt: 2
  )

  Record.defrecord(:r_misc, :misc,
    id: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_funcstat, :funcstat,
    callers_sum: :undefined,
    called_sum: :undefined,
    callers: [],
    called: []
  )

  def start() do
    spawn_3step(
      fn ->
        try do
          :erlang.register(:fprof_server, self())
        catch
          :error, :badarg ->
            {{:error, {:already_started, :erlang.whereis(:fprof_server)}}, :already_started}
        else
          true ->
            :erlang.process_flag(:trap_exit, true)
            {{:ok, self()}, :loop}
        end
      end,
      fn x ->
        x
      end,
      fn
        :loop ->
          :erlang.put(:trace_state, :idle)
          :erlang.put(:profile_state, {:idle, :undefined})
          :erlang.put(:pending_stop, [])
          server_loop([])

        :already_started ->
          :ok
      end
    )
  end

  def stop() do
    stop(:normal)
  end

  def stop(:kill) do
    case :erlang.whereis(:fprof_server) do
      :undefined ->
        :ok

      pid ->
        :erlang.exit(pid, :kill)
        :ok
    end
  end

  def stop(reason) do
    just_call(r_stop(reason: reason))
    :ok
  end

  def call(request) do
    case :erlang.whereis(:fprof_server) do
      :undefined ->
        start()
        just_call(request)

      server ->
        just_call(server, request)
    end
  end

  def just_call(request) do
    just_call(:erlang.whereis(:fprof_server), request)
  end

  defp just_call(:undefined, _) do
    {:EXIT, :fprof_server, :noproc}
  end

  defp just_call(pid, request) do
    mref = :erlang.monitor(:process, pid)

    receive do
      {:DOWN, ^mref, _, _, reason} ->
        {:EXIT, pid, reason}
    after
      0 ->
        tag = {mref, self()}

        {t, demonitor} =
          case request do
            r_stop() ->
              {:infinity, false}

            _ ->
              {0, true}
          end

        try do
          send(pid, {:fprof_server, tag, request})
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

        receive do
          {:fprof_server, ^mref, reply} ->
            case demonitor do
              true ->
                :erlang.demonitor(mref)

              false ->
                :ok
            end

            receive do
              {:DOWN, ^mref, _, _, _} ->
                :ok
            after
              t ->
                :ok
            end

            reply

          {:DOWN, ^mref, _, _, reason} ->
            receive do
              {:fprof_server, ^mref, _} ->
                :ok
            after
              t ->
                :ok
            end

            {:EXIT, pid, reason}
        after
          :infinity ->
            :timeout
        end
    end
  end

  def reply({mref, pid}, reply)
      when is_reference(mref) and
             is_pid(pid) do
    try do
      send(pid, {:fprof_server, mref, reply})
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  defp server_loop(state) do
    receive do
      {:fprof_server, {mref, pid} = tag, :"$code_change"}
      when is_reference(mref) and is_pid(pid) ->
        reply(tag, :ok)
        :fprof."$code_change"(state)

      {:fprof_server, {mref, pid} = tag, request}
      when is_reference(mref) and is_pid(pid) ->
        server_loop(handle_req(request, tag, state))

      other ->
        server_loop(handle_other(other, state))
    end
  end

  def unquote(:"$code_change")(state) do
    case :lists.keysearch(:time, 1, __MODULE__.module_info(:compile)) do
      {:value, {:time, {y, m, d, hH, mM, sS}}} ->
        :io.format(
          '~n~w: code change to compile time ' ++ '~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w~n',
          [:fprof, y, m, d, hH, mM, sS]
        )

      false ->
        :ok
    end

    server_loop(state)
  end

  defp try_pending_stop(state) do
    case {:erlang.get(:trace_state), :erlang.get(:profile_state), :erlang.get(:pending_stop)} do
      {:idle, {:idle, _}, [_ | _] = pendingStop} ->
        reason = :erlang.get(:stop_reason)
        reply = result(reason)

        :lists.foreach(
          fn tag ->
            reply(tag, reply)
          end,
          pendingStop
        )

        exit(reason)

      _ ->
        state
    end
  end

  defp handle_req(
         r_trace_start(procs: procs, mode: mode, type: :file, dest: filename),
         tag,
         state
       ) do
    case {:erlang.get(:trace_state), :erlang.get(:pending_stop)} do
      {:idle, []} ->
        trace_off()
        port = open_dbg_trace_port(:file, filename)

        case trace_on(procs, port, mode) do
          :ok ->
            :erlang.put(:trace_state, :running)
            :erlang.put(:trace_type, :file)
            :erlang.put(:trace_pid, port)
            reply(tag, :ok)
            state

          error ->
            reply(tag, error)
            state
        end

      _ ->
        reply(tag, {:error, :already_tracing})
        state
    end
  end

  defp handle_req(
         r_trace_start(procs: procs, mode: mode, type: :tracer, dest: tracer),
         tag,
         state
       ) do
    case {:erlang.get(:trace_state), :erlang.get(:pending_stop)} do
      {:idle, []} ->
        trace_off()

        case trace_on(procs, tracer, mode) do
          :ok ->
            :erlang.put(:trace_state, :running)
            :erlang.put(:trace_type, :tracer)
            :erlang.put(:trace_pid, tracer)
            reply(tag, :ok)
            state

          error ->
            reply(tag, error)
            state
        end

      _ ->
        reply(tag, {:error, :already_tracing})
        state
    end
  end

  defp handle_req(r_trace_stop(), tag, state) do
    case :erlang.get(:trace_state) do
      :running ->
        tracePid = :erlang.get(:trace_pid)
        trace_off()

        case :erlang.erase(:trace_type) do
          :file ->
            try do
              :erlang.port_close(tracePid)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end

            :erlang.put(:trace_state, :stopping)
            :erlang.put(:trace_tag, tag)
            state

          :tracer ->
            :erlang.erase(:trace_pid)
            :erlang.put(:trace_state, :idle)

            case {:erlang.get(:profile_state), :erlang.get(:profile_type),
                  :erlang.get(:profile_pid)} do
              {:running, :tracer, ^tracePid} ->
                :erlang.exit(tracePid, :normal)
                :erlang.put(:profile_tag, tag)
                state

              _ ->
                reply(tag, :ok)
                try_pending_stop(state)
            end
        end

      _ ->
        reply(tag, {:error, :not_tracing})
        state
    end
  end

  defp handle_req(
         r_profile(src: filename, group_leader: groupLeader, dump: dump, flags: flags),
         tag,
         state
       ) do
    case {:erlang.get(:profile_state), :erlang.get(:pending_stop)} do
      {{:idle, _}, []} ->
        case ensure_open(dump, [:write | flags]) do
          {:already_open, dumpPid} ->
            :erlang.put(:profile_dump, dumpPid)
            :erlang.put(:profile_close_dump, false)

          {:ok, dumpPid} ->
            :erlang.put(:profile_dump, dumpPid)
            :erlang.put(:profile_close_dump, true)

          {:error, _} = error ->
            reply(tag, error)
            state
        end

        table =
          :ets.new(
            :fprof,
            [:set, :public, {:keypos, r_clocks(:id)}]
          )

        pid =
          spawn_link_dbg_trace_client(filename, table, groupLeader, :erlang.get(:profile_dump))

        :erlang.put(:profile_state, :running)
        :erlang.put(:profile_type, :file)
        :erlang.put(:profile_pid, pid)
        :erlang.put(:profile_tag, tag)
        :erlang.put(:profile_table, table)
        state

      _ ->
        reply(tag, {:error, :already_profiling})
        state
    end
  end

  defp handle_req(
         r_profile_start(group_leader: groupLeader, dump: dump, flags: flags),
         tag,
         state
       ) do
    case {:erlang.get(:profile_state), :erlang.get(:pending_stop)} do
      {{:idle, _}, []} ->
        case ensure_open(dump, [:write | flags]) do
          {:already_open, dumpPid} ->
            :erlang.put(:profile_dump, dumpPid)
            :erlang.put(:profile_close_dump, false)

          {:ok, dumpPid} ->
            :erlang.put(:profile_dump, dumpPid)
            :erlang.put(:profile_close_dump, true)

          {:error, _} = error ->
            reply(tag, error)
            state
        end

        table =
          :ets.new(
            :fprof,
            [:set, :public, {:keypos, r_clocks(:id)}]
          )

        pid = spawn_link_trace_client(table, groupLeader, :erlang.get(:profile_dump))
        :erlang.put(:profile_state, :running)
        :erlang.put(:profile_type, :tracer)
        :erlang.put(:profile_pid, pid)
        :erlang.put(:profile_table, table)
        reply(tag, {:ok, pid})
        state

      _ ->
        reply(tag, {:error, :already_profiling})
        state
    end
  end

  defp handle_req(r_profile_stop(), tag, state) do
    case {:erlang.get(:profile_state), :erlang.get(:profile_type)} do
      {:running, :tracer} ->
        profilePid = :erlang.get(:profile_pid)

        case {:erlang.get(:trace_state), :erlang.get(:trace_type), :erlang.get(:trace_pid)} do
          {:running, :tracer, ^profilePid} ->
            trace_off()
            :erlang.erase(:trace_type)
            :erlang.erase(:trace_pid)
            :erlang.put(:trace_state, :idle)

          _ ->
            :ok
        end

        :erlang.exit(profilePid, :normal)
        :erlang.put(:profile_tag, tag)
        state

      {:running, :file} ->
        reply(tag, {:error, :profiling_file})
        state

      {_, _} ->
        reply(tag, {:error, :not_profiling})
        state
    end
  end

  defp handle_req(r_analyse(dest: dest, flags: flags) = request, tag, state) do
    case :erlang.get(:profile_state) do
      {:idle, :undefined} ->
        reply(tag, {:error, :no_profile})
        state

      {:idle, _} ->
        case ensure_open(dest, [:write | flags]) do
          {:error, _} = error ->
            reply(tag, error)
            state

          {destState, destPid} ->
            profileTable = :erlang.get(:profile_table)

            reply(
              tag,
              spawn_3step(
                fn ->
                  do_analyse(
                    profileTable,
                    r_analyse(request, dest: destPid)
                  )
                end,
                fn result ->
                  {result, :finish}
                end,
                fn :finish ->
                  :ok
                end
              )
            )

            case destState do
              :already_open ->
                :ok

              :ok ->
                :ok = :file.close(destPid)
            end

            state
        end

      _ ->
        reply(tag, {:error, :profiling})
        state
    end
  end

  defp handle_req(r_stop(reason: reason), tag, state) do
    pendingStop = :erlang.get(:pending_stop)

    case pendingStop do
      [] ->
        :erlang.put(:stop_reason, reason)

      _ ->
        :ok
    end

    :erlang.put(:pending_stop, [tag | pendingStop])
    try_pending_stop(state)
  end

  defp handle_req(r_get_state(), tag, state) do
    reply(tag, {:ok, :erlang.get()})
    state
  end

  defp handle_req(r_save_profile(file: file), tag, state) do
    case :erlang.get(:profile_state) do
      {:idle, :undefined} ->
        reply(tag, {:error, :no_profile})

      {:idle, _} ->
        reply(
          tag,
          :ets.tab2file(:erlang.get(:profile_table), file)
        )

        state

      _ ->
        reply(tag, {:error, :profiling})
        state
    end
  end

  defp handle_req(r_load_profile(file: file), tag, state) do
    case :erlang.get(:profile_state) do
      {:idle, result} ->
        case :ets.file2tab(file) do
          {:ok, table} ->
            :erlang.put(:profile_state, {:idle, :ok})

            case result do
              {:error, :no_profile} ->
                :ets.delete(:erlang.put(:profile_table, table))

              _ ->
                :erlang.put(:profile_table, table)
            end

            reply(tag, :ok)
            state

          error ->
            reply(tag, error)
            state
        end

      _ ->
        reply(tag, {:error, :profiling})
        state
    end
  end

  defp handle_req(request, tag, state) do
    :io.format('~n~p:handle_req, unknown request - ~p~n', [:fprof, request])
    reply(tag, {:error, :unknown_request})
    state
  end

  defp handle_other({:EXIT, pid, reason} = other, state)
       when is_pid(pid) or is_port(pid) do
    case {:erlang.get(:trace_state), :erlang.get(:trace_pid)} do
      {:running, ^pid} ->
        trace_off()
        :io.format('~n~p:handle_other, unexpected ~p (trace_pid)~n', [:fprof, other])
        :erlang.put(:trace_state, :idle)
        :erlang.erase(:trace_type)
        :erlang.erase(:trace_pid)
        try_pending_stop(state)

      {:stopping, ^pid} ->
        :erlang.put(:trace_state, :idle)
        :erlang.erase(:trace_pid)
        reply(:erlang.erase(:trace_tag), result(reason))
        try_pending_stop(state)

      _ ->
        case {:erlang.get(:profile_state), :erlang.get(:profile_pid)} do
          {:running, ^pid} ->
            result = result(reason)
            :erlang.put(:profile_state, {:idle, result})
            :erlang.erase(:profile_type)
            :erlang.erase(:profile_pid)

            case :erlang.erase(:profile_close_dump) do
              true ->
                :file.close(:erlang.erase(:profile_dump))

              false ->
                :erlang.erase(:profile_dump)
            end

            reply(:erlang.erase(:profile_tag), result)
            try_pending_stop(state)

          _ ->
            :io.format('~n~p:handle_other, unexpected ~p~n', [:fprof, other])
            state
        end
    end
  end

  defp handle_other(other, state) do
    :io.format('~p:handle_other, unknown - ~p', [:fprof, other])
    state
  end

  defp result(:normal) do
    :ok
  end

  defp result(reason) do
    {:error, reason}
  end

  defp ensure_open(pid, _Options) when is_pid(pid) do
    {:already_open, pid}
  end

  defp ensure_open([], _Options) do
    {:already_open, :undefined}
  end

  defp ensure_open(filename, options)
       when is_atom(filename) or
              is_list(filename) do
    :file.open(filename, [{:encoding, :utf8} | options])
  end

  def getopts(list, options)
      when is_list(list) and
             is_list(options) do
    getopts_1(options, list, [])
  end

  defp getopts_1([], list, result) do
    {:lists.reverse(result), list}
  end

  defp getopts_1([option | options], list, result) do
    {optvals, remaining} = getopts_2(list, option, [], [])
    getopts_1(options, remaining, [optvals | result])
  end

  defp getopts_2([], _Option, result, remaining) do
    {:lists.reverse(result), :lists.reverse(remaining)}
  end

  defp getopts_2([option | tail], option, result, remaining) do
    getopts_2(tail, option, [option | result], remaining)
  end

  defp getopts_2([optval | tail], option, result, remaining)
       when :erlang.element(1, optval) === option do
    getopts_2(tail, option, [optval | result], remaining)
  end

  defp getopts_2([other | tail], option, result, remaining) do
    getopts_2(tail, option, result, [other | remaining])
  end

  def setopts(options) when is_list(options) do
    :lists.append(options)
  end

  defp spawn_3step(funPrelude, funAck, funBody) do
    spawn_3step(:spawn, funPrelude, funAck, funBody)
  end

  defp spawn_link_3step(funPrelude, funAck, funBody) do
    spawn_3step(:spawn_link, funPrelude, funAck, funBody)
  end

  defp spawn_3step(spawn, funPrelude, funAck, funBody)
       when spawn === :spawn or spawn === :spawn_link do
    parent = self()
    ref = make_ref()

    child =
      apply(:erlang, spawn, [
        fn ->
          ack = funPrelude.()

          try do
            send(parent, {self(), ref, ack})
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

          mRef = :erlang.monitor(:process, parent)

          receive do
            {^parent, ^ref, go} ->
              :erlang.demonitor(mRef, [:flush])
              funBody.(go)

            {:DOWN, ^mRef, _, _, _} ->
              :ok
          end
        end
      ])

    mRef = :erlang.monitor(:process, child)

    receive do
      {^child, ^ref, ack} ->
        :erlang.demonitor(mRef, [:flush])

        try do
          funAck.(ack)
        catch
          class, reason ->
            try do
              :erlang.exit(child, :kill)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end

            :erlang.raise(class, reason, __STACKTRACE__)
        else
          {result, go} ->
            try do
              send(child, {parent, ref, go})
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end

            result
        end

      {:DOWN, ^mRef, _, _, reason} ->
        receive do
          {^child, ^ref, _Ack} ->
            :ok
        after
          0 ->
            :ok
        end

        case spawn do
          :spawn_link ->
            receive do
              {:EXIT, ^reason} ->
                :ok
            after
              0 ->
                :ok
            end

          :spawn ->
            :ok
        end

        exit(reason)
    end
  end

  def trace_off() do
    try do
      :erlang.trace_delivered(:all)
    catch
      :error, :undef ->
        :ok
    else
      ref ->
        receive do
          {:trace_delivered, :all, ^ref} ->
            :ok
        end
    end

    try do
      :erlang.trace(:all, false, [:all, :cpu_timestamp])
    catch
      :error, :badarg ->
        :erlang.trace(:all, false, [:all])
    end

    :erlang.trace_pattern(:on_load, false, [:local])
    :erlang.trace_pattern({:_, :_, :_}, false, [:local])
    :ok
  end

  def trace_on(procs, tracer, {v, cT}) do
    case (case cT do
            :cpu_time ->
              try do
                :erlang.trace(:all, true, [:cpu_timestamp])
              catch
                :error, :badarg ->
                  {:error, :not_supported}
              else
                _ ->
                  :ok
              end

            :wallclock ->
              :ok
          end) do
      :ok ->
        matchSpec = [{:_, [], [{:message, {{:cp, {:caller}}}}]}]
        :erlang.trace_pattern(:on_load, matchSpec, [:local])
        :erlang.trace_pattern({:_, :_, :_}, matchSpec, [:local])

        :lists.foreach(
          fn p ->
            :erlang.trace(p, true, [
              {:tracer, tracer}
              | trace_flags(v)
            ])
          end,
          procs
        )

        :ok

      error ->
        error
    end
  end

  defp trace_flags(:normal) do
    [:call, :return_to, :running, :procs, :garbage_collection, :arity, :timestamp, :set_on_spawn]
  end

  defp trace_flags(:verbose) do
    [
      :call,
      :return_to,
      :send,
      :receive,
      :running,
      :procs,
      :garbage_collection,
      :timestamp,
      :set_on_spawn
    ]
  end

  defp open_dbg_trace_port(type, spec) do
    fun = :dbg.trace_port(type, spec)
    fun.()
  end

  defp spawn_link_dbg_trace_client(file, table, groupLeader, dump) do
    case :dbg.trace_client(:file, file, {&handler/2, {:init, groupLeader, table, dump}}) do
      pid when is_pid(pid) ->
        :erlang.link(pid)
        pid

      other ->
        exit(other)
    end
  end

  defp spawn_link_trace_client(table, groupLeader, dump) do
    parent = self()

    spawn_link_3step(
      fn ->
        :erlang.process_flag(:trap_exit, true)
        {self(), :go}
      end,
      fn ack ->
        ack
      end,
      fn :go ->
        init = {:init, groupLeader, table, dump}
        tracer_loop(parent, &handler/2, init)
      end
    )
  end

  defp tracer_loop(parent, handler, state) do
    receive do
      trace when :erlang.element(1, trace) === :trace ->
        tracer_loop(parent, handler, handler.(trace, state))

      trace when :erlang.element(1, trace) === :trace_ts ->
        tracer_loop(parent, handler, handler.(trace, state))

      {:EXIT, ^parent, reason} ->
        _ = handler(:end_of_trace, state)
        exit(reason)

      _ ->
        tracer_loop(parent, handler, state)
    end
  end

  defp handler(
         :end_of_trace,
         {:init, groupLeader, table, dump}
       ) do
    dump(dump, :start_of_trace)
    dump(dump, :end_of_trace)
    info(groupLeader, dump, 'Empty trace!~n', [])
    end_of_trace(table, :undefined)
    :done
  end

  defp handler(
         :end_of_trace,
         {:error, reason, _, groupLeader, dump}
       ) do
    info(groupLeader, dump, '~nEnd of erroneous trace!~n', [])
    exit(reason)
  end

  defp handler(
         :end_of_trace,
         {_, tS, groupLeader, table, dump}
       ) do
    dump(dump, :end_of_trace)
    info(groupLeader, dump, '~nEnd of trace!~n', [])
    end_of_trace(table, tS)
    :done
  end

  defp handler(trace, {:init, groupLeader, table, dump}) do
    dump(dump, :start_of_trace)
    info(groupLeader, dump, 'Reading trace data...~n', [])

    try do
      trace_handler(trace, table, groupLeader, dump)
    catch
      error ->
        dump(dump, {:error, error})
        end_of_trace(table, :undefined)
        {:error, error, 1, groupLeader, dump}
    else
      tS ->
        :ets.insert(table, r_misc(id: :first_ts, data: tS))
        :ets.insert(table, r_misc(id: :last_ts_n, data: {tS, 1}))
        {1, tS, groupLeader, table, dump}
    end
  end

  defp handler(_, {:error, reason, m, groupLeader, dump}) do
    n = m + 1
    info_dots(groupLeader, dump, n)
    {:error, reason, n, groupLeader, dump}
  end

  defp handler(trace, {m, tS0, groupLeader, table, dump}) do
    n = m + 1
    info_dots(groupLeader, dump, n)

    try do
      trace_handler(trace, table, groupLeader, dump)
    catch
      error ->
        dump(dump, {:error, error})
        end_of_trace(table, tS0)
        {:error, error, n, groupLeader, dump}
    else
      tS ->
        :ets.insert(table, r_misc(id: :last_ts_n, data: {tS, n}))
        {n, tS, groupLeader, table, dump}
    end
  end

  defp end_of_trace(table, tS) do
    procs = :erlang.get()
    :erlang.put(:table, table)
    dbg(2, 'get() -> ~p~n', [procs])

    _ =
      :lists.map(
        fn {pid, _} when is_pid(pid) ->
          trace_exit(table, pid, tS)
        end,
        procs
      )

    _ = :erlang.erase()
    :ok
  end

  defp info_dots(groupLeader, groupLeader, _) do
    :ok
  end

  defp info_dots(groupLeader, _, n) do
    cond do
      rem(n, 100_000) === 0 ->
        :io.format(groupLeader, ',~n', [])

      rem(n, 50000) === 0 ->
        :io.format(groupLeader, '.~n', [])

      rem(n, 1000) === 0 ->
        :io.put_chars(groupLeader, '.')

      true ->
        :ok
    end
  end

  defp info_suspect_call(groupLeader, groupLeader, _, _) do
    :ok
  end

  defp info_suspect_call(groupLeader, _, func, pid) do
    :io.format(
      groupLeader,
      '~nWarning: ~tp called in ~p - trace may become corrupt!~n',
      parsify([func, pid])
    )
  end

  defp info(groupLeader, groupLeader, _, _) do
    :ok
  end

  defp info(groupLeader, _, format, list) do
    :io.format(groupLeader, format, list)
  end

  defp dump_stack(:undefined, _, _) do
    false
  end

  defp dump_stack(dump, stack, term) do
    {depth, _D} =
      case stack do
        :undefined ->
          {0, 0}

        _ ->
          case length(stack) do
            0 ->
              {0, 0}

            n ->
              {n, length(hd(stack))}
          end
      end

    :io.format(dump, '~s~tp.~n', [:lists.duplicate(depth, '  '), parsify(term)])
    true
  end

  defp dump(:undefined, _) do
    false
  end

  defp dump(dump, term) do
    :io.format(dump, '~tp.~n', [parsify(term)])
    true
  end

  defp trace_handler({:trace_ts, pid, :call, _MFA, _TS} = trace, _Table, _, dump) do
    stack = :erlang.get(pid)
    dump_stack(dump, stack, trace)
    throw({:incorrect_trace_data, :fprof, 1521, [trace, stack]})
  end

  defp trace_handler(
         {:trace_ts, pid, :call, {_M, _F, arity} = func, {:cp, cP}, tS} = trace,
         table,
         groupLeader,
         dump
       )
       when is_integer(arity) do
    dump_stack(dump, :erlang.get(pid), trace)

    case func do
      {:erlang, :trace, 3} ->
        info_suspect_call(groupLeader, dump, func, pid)

      {:erlang, :trace_pattern, 3} ->
        info_suspect_call(groupLeader, dump, func, pid)

      _ ->
        :ok
    end

    trace_call(table, pid, func, tS, cP)
    tS
  end

  defp trace_handler(
         {:trace_ts, pid, :call, {_M, _F, args} = mFArgs, {:cp, cP}, tS} = trace,
         table,
         _,
         dump
       )
       when is_list(args) do
    dump_stack(dump, :erlang.get(pid), trace)
    func = mfarity(mFArgs)
    trace_call(table, pid, func, tS, cP)
    tS
  end

  defp trace_handler({:trace_ts, pid, :return_to, :undefined, tS} = trace, table, _, dump) do
    dump_stack(dump, :erlang.get(pid), trace)
    trace_return_to(table, pid, :undefined, tS)
    tS
  end

  defp trace_handler(
         {:trace_ts, pid, :return_to, {_M, _F, arity} = func, tS} = trace,
         table,
         _,
         dump
       )
       when is_integer(arity) do
    dump_stack(dump, :erlang.get(pid), trace)
    trace_return_to(table, pid, func, tS)
    tS
  end

  defp trace_handler(
         {:trace_ts, pid, :return_to, {_M, _F, args} = mFArgs, tS} = trace,
         table,
         _,
         dump
       )
       when is_list(args) do
    dump_stack(dump, :erlang.get(pid), trace)
    func = mfarity(mFArgs)
    trace_return_to(table, pid, func, tS)
    tS
  end

  defp trace_handler({:trace_ts, pid, :spawn, child, mFArgs, tS} = trace, table, _, dump) do
    dump_stack(dump, :erlang.get(pid), trace)
    trace_spawn(table, child, mFArgs, tS, pid)
    tS
  end

  defp trace_handler({:trace_ts, pid, :spawned, parent, mFArgs, tS} = trace, table, _, dump) do
    dump_stack(dump, :erlang.get(pid), trace)
    trace_spawn(table, pid, mFArgs, tS, parent)
    tS
  end

  defp trace_handler({:trace_ts, pid, :exit, _Reason, tS} = trace, table, _, dump) do
    dump_stack(dump, :erlang.get(pid), trace)
    trace_exit(table, pid, tS)
    tS
  end

  defp trace_handler({:trace_ts, pid, :out, 0, tS} = trace, table, _, dump) do
    dump_stack(dump, :erlang.get(pid), trace)
    trace_out(table, pid, :undefined, tS)
    tS
  end

  defp trace_handler({:trace_ts, pid, :out, {_M, _F, arity} = func, tS} = trace, table, _, dump)
       when is_integer(arity) do
    dump_stack(dump, :erlang.get(pid), trace)
    trace_out(table, pid, func, tS)
    tS
  end

  defp trace_handler({:trace_ts, pid, :out, {_M, _F, args} = mFArgs, tS} = trace, table, _, dump)
       when is_list(args) do
    dump_stack(dump, :erlang.get(pid), trace)
    func = mfarity(mFArgs)
    trace_out(table, pid, func, tS)
    tS
  end

  defp trace_handler({:trace_ts, pid, :in, 0, tS} = trace, table, _, dump) do
    dump_stack(dump, :erlang.get(pid), trace)
    trace_in(table, pid, :undefined, tS)
    tS
  end

  defp trace_handler({:trace_ts, pid, :in, {_M, _F, arity} = func, tS} = trace, table, _, dump)
       when is_integer(arity) do
    dump_stack(dump, :erlang.get(pid), trace)
    trace_in(table, pid, func, tS)
    tS
  end

  defp trace_handler({:trace_ts, pid, :in, {_M, _F, args} = mFArgs, tS} = trace, table, _, dump)
       when is_list(args) do
    dump_stack(dump, :erlang.get(pid), trace)
    func = mfarity(mFArgs)
    trace_in(table, pid, func, tS)
    tS
  end

  defp trace_handler({:trace_ts, pid, :gc_minor_start, _Func, tS} = trace, table, _, dump) do
    dump_stack(dump, :erlang.get(pid), trace)
    trace_gc_start(table, pid, tS)
    tS
  end

  defp trace_handler({:trace_ts, pid, :gc_major_start, _Func, tS} = trace, table, _, dump) do
    dump_stack(dump, :erlang.get(pid), trace)
    trace_gc_start(table, pid, tS)
    tS
  end

  defp trace_handler({:trace_ts, pid, :gc_start, _Func, tS} = trace, table, _, dump) do
    dump_stack(dump, :erlang.get(pid), trace)
    trace_gc_start(table, pid, tS)
    tS
  end

  defp trace_handler({:trace_ts, pid, :gc_minor_end, _Func, tS} = trace, table, _, dump) do
    dump_stack(dump, :erlang.get(pid), trace)
    trace_gc_end(table, pid, tS)
    tS
  end

  defp trace_handler({:trace_ts, pid, :gc_major_end, _Func, tS} = trace, table, _, dump) do
    dump_stack(dump, :erlang.get(pid), trace)
    trace_gc_end(table, pid, tS)
    tS
  end

  defp trace_handler({:trace_ts, pid, :gc_end, _Func, tS} = trace, table, _, dump) do
    dump_stack(dump, :erlang.get(pid), trace)
    trace_gc_end(table, pid, tS)
    tS
  end

  defp trace_handler({:trace_ts, pid, :link, _OtherPid, tS} = trace, _Table, _, dump) do
    dump_stack(dump, :erlang.get(pid), trace)
    tS
  end

  defp trace_handler({:trace_ts, pid, :unlink, _OtherPid, tS} = trace, _Table, _, dump) do
    dump_stack(dump, :erlang.get(pid), trace)
    tS
  end

  defp trace_handler({:trace_ts, pid, :getting_linked, _OtherPid, tS} = trace, _Table, _, dump) do
    dump_stack(dump, :erlang.get(pid), trace)
    tS
  end

  defp trace_handler({:trace_ts, pid, :getting_unlinked, _OtherPid, tS} = trace, _Table, _, dump) do
    dump_stack(dump, :erlang.get(pid), trace)
    tS
  end

  defp trace_handler({:trace_ts, pid, :register, _Name, tS} = trace, _Table, _, dump) do
    dump_stack(dump, :erlang.get(pid), trace)
    tS
  end

  defp trace_handler({:trace_ts, pid, :unregister, _Name, tS} = trace, _Table, _, dump) do
    dump_stack(dump, :erlang.get(pid), trace)
    tS
  end

  defp trace_handler({:trace_ts, pid, :send, _OtherPid, _Msg, tS} = trace, _Table, _, dump) do
    dump_stack(dump, :erlang.get(pid), trace)
    tS
  end

  defp trace_handler(
         {:trace_ts, pid, :send_to_non_existing_process, _OtherPid, _Msg, tS} = trace,
         _Table,
         _,
         dump
       ) do
    dump_stack(dump, :erlang.get(pid), trace)
    tS
  end

  defp trace_handler({:trace_ts, pid, :receive, _Msg, tS} = trace, _Table, _, dump) do
    dump_stack(dump, :erlang.get(pid), trace)
    tS
  end

  defp trace_handler(trace, _Table, _, dump) do
    dump(dump, trace)
    throw({:incorrect_trace_data, :fprof, 1719, [trace]})
  end

  defp trace_call(table, pid, func, tS, cP) do
    stack = get_stack(pid)
    dbg(0, 'trace_call(~p, ~p, ~p, ~p)~n~p~n', [pid, func, tS, cP, stack])

    {proc, initCnt} =
      case :ets.lookup(table, pid) do
        [r_proc(init_cnt: n) = p] ->
          {p, n}

        [] ->
          {:undefined, 0}
      end

    case stack do
      [] ->
        init_log(table, proc, func)

        oldStack =
          cond do
            cP === :undefined ->
              stack

            true ->
              [[{cP, tS}]]
          end

        :erlang.put(
          pid,
          trace_call_push(table, pid, func, tS, oldStack)
        )

      [[{^func, firstInTS}]] when initCnt === 2 ->
        init_log(table, proc, func)

        oldStack =
          cond do
            cP === :undefined ->
              []

            true ->
              [[{cP, firstInTS}]]
          end

        :erlang.put(
          pid,
          trace_call_push(table, pid, func, firstInTS, oldStack)
        )

      [[{:suspend, _} | _] | _] ->
        throw({:inconsistent_trace_data, :fprof, 1801, [pid, func, tS, cP, stack]})

      [[{:garbage_collect, _} | _] | _] ->
        throw({:inconsistent_trace_data, :fprof, 1804, [pid, func, tS, cP, stack]})

      [[{^cP, _} | _], [{^cP, _} | _] | _] ->
        init_log(table, proc, func)

        :erlang.put(
          pid,
          trace_call_shove(table, pid, func, tS, stack)
        )

      [[{^cP, _} | _] | _] ->
        init_log(table, proc, func)

        :erlang.put(
          pid,
          trace_call_push(table, pid, func, tS, stack)
        )

      [_, [{^cP, _} | _] | _] ->
        init_log(table, proc, func)

        :erlang.put(
          pid,
          trace_call_shove(table, pid, func, tS, stack)
        )

      [
        [{func0, _} | _],
        [{func0, _} | _],
        [{^cP, _} | _]
        | _
      ] ->
        init_log(table, proc, func)

        :erlang.put(
          pid,
          trace_call_shove(
            table,
            pid,
            func,
            tS,
            trace_return_to_int(table, pid, func0, tS, stack)
          )
        )

      [[{_, tS0} | _] = level0] ->
        init_log(table, proc, func)

        oldStack =
          cond do
            cP === :undefined ->
              stack

            true ->
              [level0, [{cP, tS0}]]
          end

        :erlang.put(
          pid,
          trace_call_shove(table, pid, func, tS, oldStack)
        )

      [_ | _] ->
        oldStack =
          cond do
            cP === :undefined ->
              trace_return_to_int(table, pid, cP, tS, stack)

            true ->
              init_log(table, proc, cP)
              trace_call_shove(table, pid, cP, tS, stack)
          end

        init_log(table, pid, func)

        :erlang.put(
          pid,
          trace_call_push(table, pid, func, tS, oldStack)
        )
    end

    :ok
  end

  defp trace_call_push(table, pid, func, tS, stack) do
    case stack do
      [] ->
        :ok

      [_ | _] ->
        trace_clock(table, pid, tS, stack, r_clocks(:own))
    end

    newStack = [[{func, tS}] | stack]
    trace_clock(table, pid, 1, newStack, r_clocks(:cnt))
    newStack
  end

  defp trace_call_shove(table, pid, func, tS, stack) do
    trace_clock(table, pid, tS, stack, r_clocks(:own))

    [[_ | newLevel0] | newStack1] =
      case stack do
        [] ->
          [[{func, tS}]]

        [level0 | stack1] ->
          [
            trace_call_collapse([
              {func, tS}
              | level0
            ])
            | stack1
          ]
      end

    newStack = [[{func, tS} | newLevel0] | newStack1]
    trace_clock(table, pid, 1, newStack, r_clocks(:cnt))
    newStack
  end

  def trace_call_collapse([]) do
    []
  end

  def trace_call_collapse([_] = stack) do
    stack
  end

  def trace_call_collapse([_, _] = stack) do
    stack
  end

  def trace_call_collapse([_ | stack1] = stack) do
    trace_call_collapse_1(stack, stack1, 1)
  end

  defp trace_call_collapse_1(stack, [], _) do
    stack
  end

  defp trace_call_collapse_1([{func0, _} | _] = stack, [{func0, _} | s1] = s, n) do
    case trace_call_collapse_2(stack, s, n) do
      true ->
        s

      false ->
        trace_call_collapse_1(stack, s1, n + 1)
    end
  end

  defp trace_call_collapse_1(stack, [_ | s1], n) do
    trace_call_collapse_1(stack, s1, n + 1)
  end

  defp trace_call_collapse_2(_, _, 0) do
    true
  end

  defp trace_call_collapse_2(
         [{func1, _} | [{func2, _} | _] = stack2],
         [{func1, _} | [{func2, _} | _] = s2],
         n
       ) do
    trace_call_collapse_2(stack2, s2, n - 1)
  end

  defp trace_call_collapse_2([{func1, _} | _], [{func1, _} | _], _N) do
    false
  end

  defp trace_call_collapse_2(_Stack, [_], _N) do
    false
  end

  defp trace_call_collapse_2(stack, [_ | s], n) do
    trace_call_collapse_2(stack, s, n)
  end

  defp trace_call_collapse_2(_Stack, [], _N) do
    false
  end

  defp trace_return_to(table, pid, func, tS) do
    stack = get_stack(pid)
    dbg(0, 'trace_return_to(~p, ~p, ~p)~n~p~n', [pid, func, tS, stack])

    case stack do
      [[{:suspend, _} | _] | _] ->
        throw({:inconsistent_trace_data, :fprof, 1949, [pid, func, tS, stack]})

      [[{:garbage_collect, _} | _] | _] ->
        throw({:inconsistent_trace_data, :fprof, 1952, [pid, func, tS, stack]})

      [_ | _] ->
        :erlang.put(
          pid,
          trace_return_to_int(table, pid, func, tS, stack)
        )

      [] ->
        :erlang.put(
          pid,
          trace_return_to_int(table, pid, func, tS, stack)
        )
    end

    :ok
  end

  defp trace_return_to_int(table, pid, func, tS, stack) do
    trace_clock(table, pid, tS, stack, r_clocks(:own))

    case trace_return_to_2(table, pid, func, tS, stack) do
      {:undefined, _} ->
        [[{func, tS}] | stack]

      {[[{^func, _} | level0] | stack1], _} ->
        [[{func, tS} | level0] | stack1]

      {newStack, _} ->
        newStack
    end
  end

  defp trace_return_to_1(_, _, :undefined, _, []) do
    {[], []}
  end

  defp trace_return_to_1(_, _, _, _, []) do
    {:undefined, []}
  end

  defp trace_return_to_1(table, pid, func, tS, [[{func, _} | level0] | stack1] = stack) do
    charged = trace_return_to_3([level0 | stack1], [])

    case :lists.member(func, charged) do
      false ->
        trace_clock(table, pid, tS, stack, r_clocks(:acc))
        {stack, [func | charged]}

      true ->
        {stack, charged}
    end
  end

  defp trace_return_to_1(table, pid, func, tS, stack) do
    trace_return_to_2(table, pid, func, tS, stack)
  end

  defp trace_return_to_2(table, pid, func, tS, [] = stack) do
    trace_return_to_1(table, pid, func, tS, stack)
  end

  defp trace_return_to_2(table, pid, func, tS, [[] | stack1]) do
    trace_return_to_1(table, pid, func, tS, stack1)
  end

  defp trace_return_to_2(table, pid, func, tS, [[{func0, _} | level1] | stack1] = stack) do
    case trace_return_to_2(table, pid, func, tS, [level1 | stack1]) do
      {:undefined, _} = r ->
        r

      {newStack, charged} = r ->
        case :lists.member(func0, charged) do
          false ->
            trace_clock(table, pid, tS, stack, r_clocks(:acc))
            {newStack, [func0 | charged]}

          true ->
            r
        end
    end
  end

  defp trace_return_to_3([], r) do
    r
  end

  defp trace_return_to_3([[] | stack1], r) do
    trace_return_to_3(stack1, r)
  end

  defp trace_return_to_3([[{func0, _} | level0] | stack1], r) do
    trace_return_to_3([level0 | stack1], [func0 | r])
  end

  defp trace_spawn(table, pid, mFArgs, tS, parent) do
    stack = :erlang.get(pid)
    dbg(0, 'trace_spawn(~p, ~p, ~p, ~p)~n~p~n', [pid, mFArgs, tS, parent, stack])

    case stack do
      :undefined ->
        {m, f, args} = mFArgs
        oldStack = [[{{m, f, length(args)}, tS}]]

        :erlang.put(
          pid,
          trace_call_push(table, pid, :suspend, tS, oldStack)
        )

        :ets.insert(
          table,
          r_proc(id: pid, parent: parent, spawned_as: mFArgs)
        )

      _ ->
        :ok
    end
  end

  defp trace_exit(table, pid, tS) do
    stack = :erlang.erase(pid)
    dbg(0, 'trace_exit(~p, ~p)~n~p~n', [pid, tS, stack])

    case stack do
      :undefined ->
        :ok

      [] ->
        :ok

      [_ | _] = ^stack ->
        _ = trace_return_to_int(table, pid, :undefined, tS, stack)
        :ok
    end

    :ok
  end

  defp trace_out(table, pid, func, tS) do
    stack = get_stack(pid)
    dbg(0, 'trace_out(~p, ~p, ~p)~n~p~n', [pid, func, tS, stack])

    case stack do
      [] ->
        :erlang.put(
          pid,
          trace_call_push(
            table,
            pid,
            :suspend,
            tS,
            case func do
              :undefined ->
                []

              _ ->
                [[{func, tS}]]
            end
          )
        )

      [[{:suspend, _}] | _] ->
        :erlang.put(pid, [[{:suspend, tS}] | stack])

      [_ | _] ->
        :erlang.put(
          pid,
          trace_call_push(table, pid, :suspend, tS, stack)
        )
    end
  end

  defp trace_in(table, pid, func, tS) do
    stack = :erlang.get(pid)
    dbg(0, 'trace_in(~p, ~p, ~p)~n~p~n', [pid, func, tS, stack])

    case stack do
      :undefined ->
        :erlang.put(pid, [[{func, tS}]])

      [] ->
        :erlang.put(pid, [[{func, tS}]])

      [[{:suspend, _}]] ->
        :erlang.put(
          pid,
          trace_return_to_int(table, pid, :undefined, tS, stack)
        )

      [[{:suspend, _}] | [[{:suspend, _}] | _] = newStack] ->
        :erlang.put(pid, newStack)

      [[{:suspend, _}], [{func1, _} | _] | _] ->
        :erlang.put(
          pid,
          trace_return_to_int(table, pid, func1, tS, stack)
        )

      _ ->
        throw({:inconsistent_trace_data, :fprof, 2112, [pid, func, tS, stack]})
    end
  end

  defp trace_gc_start(table, pid, tS) do
    stack = get_stack(pid)
    dbg(0, 'trace_gc_start(~p, ~p)~n~p~n', [pid, tS, stack])

    :erlang.put(
      pid,
      trace_call_push(table, pid, :garbage_collect, tS, stack)
    )
  end

  defp trace_gc_end(table, pid, tS) do
    stack = :erlang.get(pid)
    dbg(0, 'trace_gc_end(~p, ~p)~n~p~n', [pid, tS, stack])

    case stack do
      :undefined ->
        :erlang.put(pid, [])

      [] ->
        :ok

      [[{:garbage_collect, _}]] ->
        :erlang.put(
          pid,
          trace_return_to_int(table, pid, :undefined, tS, stack)
        )

      [[{:garbage_collect, _}], [{func1, _} | _] | _] ->
        :erlang.put(
          pid,
          trace_return_to_int(table, pid, func1, tS, stack)
        )

      _ ->
        throw({:inconsistent_trace_data, :fprof, 2138, [pid, tS, stack]})
    end
  end

  defp get_stack(id) do
    case :erlang.get(id) do
      :undefined ->
        []

      stack ->
        stack
    end
  end

  defp mfarity({m, f, args}) when is_list(args) do
    {m, f, length(args)}
  end

  defp mfarity(mFA) do
    mFA
  end

  defp init_log(_Table, _Proc, :suspend) do
    :ok
  end

  defp init_log(_Table, _Proc, :void) do
    :ok
  end

  defp init_log(_Table, :undefined, _Entry) do
    :ok
  end

  defp init_log(_Table, r_proc(init_cnt: 0), _Entry) do
    :ok
  end

  defp init_log(table, r_proc(init_cnt: n, init_log: l) = proc, entry) do
    :ets.insert(
      table,
      r_proc(proc, init_cnt: n - 1, init_log: [entry | l])
    )
  end

  defp init_log(table, id, entry) do
    proc =
      case :ets.lookup(table, id) do
        [p] ->
          p

        [] ->
          :undefined
      end

    init_log(table, proc, entry)
  end

  defp trace_clock(_Table, _Pid, _T, [[{:suspend, _}], [{:suspend, _}] | _] = _Stack, _Clock) do
    dbg(9, 'trace_clock(Table, ~w, ~w, ~w, ~w)~n', [_Pid, _T, _Stack, _Clock])
    :ok
  end

  defp trace_clock(table, pid, t, [[{:garbage_collect, tS0}], [{:suspend, _}]], clock) do
    trace_clock_1(table, pid, t, tS0, :undefined, :garbage_collect, clock)
  end

  defp trace_clock(
         table,
         pid,
         t,
         [
           [{:garbage_collect, tS0}],
           [{:suspend, _}],
           [
             {func2, _}
             | _
           ]
           | _
         ],
         clock
       ) do
    trace_clock_1(table, pid, t, tS0, func2, :garbage_collect, clock)
  end

  defp trace_clock(table, pid, t, [[{func0, tS0}, {func1, _} | _] | _], clock) do
    trace_clock_1(table, pid, t, tS0, func1, func0, clock)
  end

  defp trace_clock(table, pid, t, [[{func0, tS0}], [{func1, _} | _] | _], clock) do
    trace_clock_1(table, pid, t, tS0, func1, func0, clock)
  end

  defp trace_clock(table, pid, t, [[{func0, tS0}]], clock) do
    trace_clock_1(table, pid, t, tS0, :undefined, func0, clock)
  end

  defp trace_clock(_, _, _, [], _) do
    :ok
  end

  defp trace_clock_1(table, pid, _, _, caller, :suspend, r_clocks(:own)) do
    clock_add(table, {pid, caller, :suspend}, r_clocks(:own), 0)
  end

  defp trace_clock_1(table, pid, t, tS, caller, func, clock) do
    clock_add(
      table,
      {pid, caller, func},
      clock,
      cond do
        is_integer(t) ->
          t

        true ->
          ts_sub(t, tS)
      end
    )
  end

  defp clock_add(table, id, clock, t) do
    dbg(1, 'clock_add(Table, ~w, ~w, ~w)~n', [id, clock, t])

    try do
      :ets.update_counter(table, id, {clock, t})
      :ok
    catch
      :error, :badarg ->
        :ets.insert(table, r_clocks(id: id))
        x = :ets.update_counter(table, id, {clock, t})

        cond do
          x >= 0 ->
            :ok

          true ->
            dbg(0, 'Negative counter value ~p ~p ~p ~p~n', [x, id, clock, t])
        end

        :ok
    end
  end

  defp clocks_add(table, r_clocks(id: id) = clocks) do
    dbg(1, 'clocks_add(Table, ~w)~n', [clocks])

    case :ets.lookup(table, id) do
      [clocks0] ->
        :ets.insert(table, clocks_sum(clocks, clocks0, id))

      [] ->
        :ets.insert(table, clocks)
    end
  end

  defp clocks_sum(
         r_clocks(id: _Id1, cnt: cnt1, own: own1, acc: acc1),
         r_clocks(id: _Id2, cnt: cnt2, own: own2, acc: acc2),
         id
       ) do
    r_clocks(id: id, cnt: cnt1 + cnt2, own: own1 + own2, acc: acc1 + acc2)
  end

  defp ts_sub({a, b, c} = _T, {a0, b0, c0} = _T0) do
    x = ((a - a0) * 1_000_000 + (b - b0)) * 1_000_000 + c - c0

    cond do
      x >= 0 ->
        :ok

      true ->
        dbg(9, 'Negative counter value ~p ~p ~p~n', [x, _T, _T0])
    end

    x
  end

  defp ts_sub(_, _) do
    :undefined
  end

  defp do_analyse(table, analyse) do
    dbg(5, 'do_analyse_1(~p, ~p)~n', [table, analyse])

    result =
      try do
        do_analyse_1(table, analyse)
      catch
        error ->
          error
      end

    dbg(5, 'do_analyse_1(_, _) ->~p~n', [result])
    result
  end

  defp do_analyse_1(
         table,
         r_analyse(
           group_leader: groupLeader,
           dest: io,
           cols: cols0,
           callers: printCallers,
           sort: sort,
           totals: printTotals,
           details: printDetails
         ) = _Analyse
       ) do
    waste = 11
    minCols = waste + 12

    cols =
      cond do
        cols0 < minCols ->
          minCols

        true ->
          cols0
      end

    width = div(cols - waste, 12)
    fnameWidth = cols - waste - 5 * width
    dest = {io, [fnameWidth, width, 2 * width, 2 * width]}

    sortElement =
      case sort do
        :own ->
          r_clocks(:own)

        :acc ->
          r_clocks(:acc)
      end

    _Erase = :erlang.erase()
    dbg(2, 'erase() -> ~p~n', [_Erase])
    :io.format(groupLeader, 'Processing data...~n', [])

    pidTable =
      :ets.new(
        :fprof,
        [:set, :private, {:keypos, r_clocks(:id)}]
      )

    procTable =
      :ets.new(
        :fprof,
        [:set, :private, {:keypos, r_proc(:id)}]
      )

    ets_select_foreach(table, [{:_, [], [:"$_"]}], 100, fn
      r_clocks(id: {pid, caller, func}) = clocks ->
        case printDetails do
          true ->
            funcstat_pd(pid, caller, func, clocks)
            clocks_add(pidTable, r_clocks(clocks, id: pid))

          false ->
            :ok
        end

        clocks_add(pidTable, r_clocks(clocks, id: :totals))

        case printTotals do
          true ->
            funcstat_pd(:totals, caller, func, clocks)

          false ->
            :ok
        end

      r_proc() = proc ->
        :ets.insert(procTable, proc)

      r_misc() = misc ->
        :ets.insert(procTable, misc)
    end)

    dbg(3, 'get() -> ~p~n', [:erlang.get()])

    {firstTS, lastTS, _TraceCnt} =
      case {:ets.lookup(procTable, :first_ts), :ets.lookup(procTable, :last_ts_n)} do
        {[r_misc(data: fTS)], [r_misc(data: {lTS, tC})]}
        when fTS !== :undefined and lTS !== :undefined ->
          {fTS, lTS, tC}

        _ ->
          throw({:error, :empty_trace})
      end

    totals0 =
      case :ets.lookup(pidTable, :totals) do
        [t0] ->
          :ets.delete(pidTable, :totals)
          t0

        _ ->
          throw({:error, :empty_trace})
      end

    totals = r_clocks(totals0, acc: ts_sub(lastTS, firstTS))
    dbg(3, 'Totals0 =  ~p~n', [totals0])
    dbg(3, 'PidTable =  ~p~n', [:ets.tab2list(pidTable)])
    dbg(3, 'ProcTable =  ~p~n', [:ets.tab2list(procTable)])
    dbg(4, 'Totals = ~p~n', [totals])

    :lists.foreach(
      fn {{pid, _Func}, funcstat} ->
        :erlang.put(
          pid,
          [
            funcstat
            | case :erlang.get(pid) do
                :undefined ->
                  []

                other ->
                  other
              end
          ]
        )
      end,
      :erlang.erase()
    )

    dbg(4, 'get() -> ~p~n', [:erlang.get()])

    pidSorted =
      postsort_r(
        :lists.sort(
          :ets.select(
            pidTable,
            [
              {:_, [],
               [
                 [
                   {:element, r_clocks(:own), :"$_"}
                   | :"$_"
                 ]
               ]}
            ]
          )
        )
      )

    dbg(4, 'PidSorted = ~p~n', [pidSorted])
    :io.format(groupLeader, 'Creating output...~n', [])
    println(dest, '%% ', [], 'Analysis results:', '')
    println(dest, '{  ', :analysis_options, ',', '')
    println(dest, ' [{', {:callers, printCallers}, '},', '')
    println(dest, '  {', {:sort, sort}, '},', '')
    println(dest, '  {', {:totals, printTotals}, '},', '')
    println(dest, '  {', {:details, printDetails}, '}]}.', '')
    println(dest)

    :lists.foreach(
      fn {r_clocks() = clocks, procOrPid, funcstatList} ->
        println(dest, '%  ', :head, '', '')

        case procOrPid do
          r_proc() ->
            println(dest, '[{ ', clocks, '},', '%%')
            print_proc(dest, procOrPid)

          :totals ->
            println(dest, '[{ ', clocks, '}].', '%%%')

          _ when is_pid(procOrPid) ->
            println(dest, '[{ ', clocks, '}].', '%%')
        end

        println(dest)

        :lists.foreach(
          fn r_funcstat(
               callers_sum: callersSum,
               callers: callers,
               called: called
             ) ->
            case {printCallers, callers} do
              {true, _} ->
                print_callers(dest, callers)
                println(dest, ' { ', callersSum, '},', '%')
                print_called(dest, called)
                println(dest)

              {false, _} ->
                println(dest, '{  ', callersSum, '}.', '')
            end

            :ok
          end,
          funcstat_sort_r(
            funcstatList,
            sortElement
          )
        )

        println(dest)
      end,
      :lists.map(
        fn r_clocks(id: pid) = clocks ->
          proc =
            case :ets.lookup(
                   procTable,
                   pid
                 ) do
              [] ->
                pid

              [procX] ->
                procX
            end

          funcstatList =
            case :erlang.get(pid) do
              :undefined ->
                []

              fL ->
                fL
            end

          {clocks, proc, funcstatList}
        end,
        case printDetails do
          true ->
            [totals | pidSorted]

          false ->
            [totals]
        end
      )
    )

    :ets.delete(pidTable)
    :ets.delete(procTable)
    :io.format(groupLeader, 'Done!~n', [])
    :ok
  end

  defp print_proc({:undefined, _}, _) do
    :ok
  end

  defp print_proc(
         dest,
         r_proc(id: _Pid, parent: parent, spawned_as: spawnedAs, init_log: initLog)
       ) do
    case {parent, spawnedAs, initLog} do
      {:undefined, :undefined, []} ->
        println(dest, '   ', [], '].', '')

      {_, :undefined, []} ->
        println(dest, ' { ', {:spawned_by, parsify(parent)}, '}].', '')

      _ ->
        println(dest, ' { ', {:spawned_by, parsify(parent)}, '},', '')

        case {spawnedAs, initLog} do
          {_, []} ->
            println(dest, ' { ', {:spawned_as, spawnedAs}, '}].', '')

          {:undefined, _} ->
            println(dest, ' { ', {:initial_calls, :lists.reverse(initLog)}, '}].', '')

          _ ->
            println(dest, ' { ', {:spawned_as, spawnedAs}, '},', '')
            println(dest, ' { ', {:initial_calls, :lists.reverse(initLog)}, '}].', '')
        end
    end
  end

  def print_callers(dest, []) do
    println(dest, '{[', [], '],', '')
  end

  def print_callers(dest, [clocks]) do
    println(dest, '{[{', clocks, '}],', '')
  end

  def print_callers(dest, [clocks | tail]) do
    println(dest, '{[{', clocks, '},', '')
    print_callers_1(dest, tail)
  end

  defp print_callers_1(dest, [clocks]) do
    println(dest, '  {', clocks, '}],', '')
  end

  defp print_callers_1(dest, [clocks | tail]) do
    println(dest, '  {', clocks, '},', '')
    print_callers_1(dest, tail)
  end

  def print_func(dest, clocks) do
    println(dest, ' { ', clocks, '},', '%')
  end

  def print_called(dest, []) do
    println(dest, ' [', [], ']}.', '')
  end

  def print_called(dest, [clocks]) do
    println(dest, ' [{', clocks, '}]}.', '')
  end

  def print_called(dest, [clocks | tail]) do
    println(dest, ' [{', clocks, '},', '')
    print_called_1(dest, tail)
  end

  defp print_called_1(dest, [clocks]) do
    println(dest, '  {', clocks, '}]}.', '')
  end

  defp print_called_1(dest, [clocks | tail]) do
    println(dest, '  {', clocks, '},', '')
    print_called_1(dest, tail)
  end

  defp println({:undefined, _}) do
    :ok
  end

  defp println({io, _}) do
    :io.nl(io)
  end

  def println({:undefined, _}, _Head, _, _Tail, _Comment) do
    :ok
  end

  def println(
        {io, [w1, w2, w3, w4]},
        head,
        r_clocks(id: pid, cnt: cnt, acc: _, own: own),
        tail,
        comment
      )
      when is_pid(pid) do
    :io.put_chars(
      io,
      [
        pad(head, ?\s, 3),
        flat_format(parsify(pid), ?,, w1),
        flat_format(cnt, ?,, w2, :right),
        flat_format(:undefined, ?,, w3, :right),
        flat_format(own * 0.001, [], w4 - 1, :right),
        pad(tail, ?\s, 4),
        pad(?\s, comment, 4),
        :io_lib.nl()
      ]
    )
  end

  def println(
        {io, [w1, w2, w3, w4]},
        head,
        r_clocks(id: {_M, _F, _A} = func, cnt: cnt, acc: acc, own: own),
        tail,
        comment
      ) do
    :io.put_chars(
      io,
      [
        pad(head, ?\s, 3),
        flat_format(func, ?,, w1),
        flat_format(cnt, ?,, w2, :right),
        flat_format(acc * 0.001, ?,, w3, :right),
        flat_format(own * 0.001, [], w4 - 1, :right),
        pad(tail, ?\s, 4),
        pad(?\s, comment, 4),
        :io_lib.nl()
      ]
    )
  end

  def println(
        {io, [w1, w2, w3, w4]},
        head,
        r_clocks(id: id, cnt: cnt, acc: acc, own: own),
        tail,
        comment
      ) do
    :io.put_chars(
      io,
      [
        pad(head, ?\s, 3),
        flat_format(parsify(id), ?,, w1),
        flat_format(cnt, ?,, w2, :right),
        flat_format(acc * 0.001, ?,, w3, :right),
        flat_format(own * 0.001, [], w4 - 1, :right),
        pad(tail, ?\s, 4),
        pad(?\s, comment, 4),
        :io_lib.nl()
      ]
    )
  end

  def println({io, [w1, w2, w3, w4]}, head, :head, tail, comment) do
    :io.put_chars(
      io,
      [
        pad(head, ?\s, 3),
        pad(' ', ?\s, w1),
        pad(?\s, ' CNT ', w2),
        pad(?\s, ' ACC ', w3),
        pad(?\s, ' OWN', w4 - 1),
        pad(
          tail,
          ?\s,
          4
        ),
        pad(
          ?\s,
          comment,
          4
        ),
        :io_lib.nl()
      ]
    )
  end

  def println({io, _}, head, [], tail, comment) do
    :io.format(io, '~s~ts~ts~n', [pad(head, ?\s, 3), tail, comment])
  end

  def println({io, _}, head, {tag, term}, tail, comment) do
    :io.format(io, '~s~tp, ~tp~ts~ts~n', [
      pad(head, ?\s, 3),
      parsify(tag),
      parsify(term),
      tail,
      comment
    ])
  end

  def println({io, _}, head, term, tail, comment) do
    :io.format(io, '~s~tp~ts~ts~n', [pad(head, ?\s, 3), parsify(term), tail, comment])
  end

  defp funcstat_pd(pid, func1, func0, clocks) do
    :erlang.put(
      {pid, func0},
      case :erlang.get({pid, func0}) do
        :undefined ->
          r_funcstat(
            callers_sum: r_clocks(clocks, id: func0),
            called_sum: r_clocks(id: func0),
            callers: [r_clocks(clocks, id: func1)]
          )

        r_funcstat(
          callers_sum: callersSum,
          callers: callers
        ) = funcstatCallers ->
          r_funcstat(funcstatCallers,
            callers_sum:
              clocks_sum(
                callersSum,
                clocks,
                func0
              ),
            callers: insert_call(clocks, func1, callers)
          )
      end
    )

    :erlang.put(
      {pid, func1},
      case :erlang.get({pid, func1}) do
        :undefined ->
          r_funcstat(
            callers_sum: r_clocks(id: func1),
            called_sum: r_clocks(clocks, id: func1),
            called: [r_clocks(clocks, id: func0)]
          )

        r_funcstat(
          called_sum: calledSum,
          called: called
        ) = funcstatCalled ->
          r_funcstat(funcstatCalled,
            called_sum:
              clocks_sum(
                calledSum,
                clocks,
                func1
              ),
            called: insert_call(clocks, func0, called)
          )
      end
    )
  end

  defp insert_call(clocks, func, clocksList) do
    insert_call(clocks, func, clocksList, [])
  end

  defp insert_call(clocks, func, [r_clocks(id: func) = c | t], acc) do
    [clocks_sum(c, clocks, func) | t ++ acc]
  end

  defp insert_call(clocks, func, [h | t], acc) do
    insert_call(clocks, func, t, [h | acc])
  end

  defp insert_call(clocks, func, [], acc) do
    [r_clocks(clocks, id: func) | acc]
  end

  defp funcstat_sort_r(funcstatList, element) do
    funcstat_sort_r_1(funcstatList, element, [])
  end

  defp funcstat_sort_r_1([], _, r) do
    postsort_r(:lists.sort(r))
  end

  defp funcstat_sort_r_1(
         [
           r_funcstat(callers_sum: r_clocks() = clocks, callers: callers, called: called) =
             funcstat
           | l
         ],
         element,
         r
       ) do
    funcstat_sort_r_1(l, element, [
      [
        :erlang.element(element, clocks)
        | r_funcstat(funcstat,
            callers:
              clocks_sort_r(
                callers,
                element
              ),
            called:
              clocks_sort_r(
                called,
                element
              )
          )
      ]
      | r
    ])
  end

  defp clocks_sort_r(l, e) do
    clocks_sort_r_1(l, e, [])
  end

  defp clocks_sort_r_1([], _, r) do
    postsort_r(:lists.sort(r))
  end

  defp clocks_sort_r_1([r_clocks() = c | l], e, r) do
    clocks_sort_r_1(l, e, [[:erlang.element(e, c) | c] | r])
  end

  defp postsort_r(l) do
    postsort_r(l, [])
  end

  defp postsort_r([], r) do
    r
  end

  defp postsort_r([[_ | c] | l], r) do
    postsort_r(l, [c | r])
  end

  defp flat_format(f, trailer) when is_float(f) do
    :lists.flatten([:io_lib.format('~.3f', [f]), trailer])
  end

  defp flat_format(w, trailer) do
    :lists.flatten([:io_lib.format('~tp', [w]), trailer])
  end

  defp flat_format(term, trailer, width) do
    flat_format(term, trailer, width, :left)
  end

  defp flat_format(term, trailer, width, :left) do
    flat_format(term, trailer, width, {:left, ?\s})
  end

  defp flat_format(term, trailer, width, {:left, filler}) do
    pad(flat_format(term, trailer), filler, width)
  end

  defp flat_format(term, trailer, width, :right) do
    flat_format(term, trailer, width, {:right, ?\s})
  end

  defp flat_format(term, trailer, width, {:right, filler}) do
    pad(filler, flat_format(term, trailer), width)
  end

  defp pad(char, l, size)
       when is_integer(char) and
              is_list(l) and is_integer(size) do
    list = :lists.flatten(l)
    length = length(list)

    cond do
      length >= size ->
        list

      true ->
        :lists.append(
          :lists.duplicate(size - length, char),
          list
        )
    end
  end

  defp pad(l, char, size)
       when is_list(l) and
              is_integer(char) and is_integer(size) do
    list = :lists.flatten(l)
    length = length(list)

    cond do
      length >= size ->
        list

      true ->
        :lists.append(
          list,
          :lists.duplicate(size - length, char)
        )
    end
  end

  defp ets_select_foreach(table, matchSpec, limit, fun) do
    :ets.safe_fixtable(table, true)

    ets_select_foreach_1(
      :ets.select(table, matchSpec, limit),
      fun
    )
  end

  defp ets_select_foreach_1(:"$end_of_table", _) do
    :ok
  end

  defp ets_select_foreach_1({matches, continuation}, fun) do
    dbg(2, 'Matches = ~p~n', [matches])
    :lists.foreach(fun, matches)
    ets_select_foreach_1(:ets.select(continuation), fun)
  end

  def parsify([]) do
    []
  end

  def parsify([hd | tl]) do
    [parsify(hd) | parsify(tl)]
  end

  def parsify({a, b}) do
    {parsify(a), parsify(b)}
  end

  def parsify({a, b, c}) do
    {parsify(a), parsify(b), parsify(c)}
  end

  def parsify(tuple) when is_tuple(tuple) do
    :erlang.list_to_tuple(parsify(:erlang.tuple_to_list(tuple)))
  end

  def parsify(map) when is_map(map) do
    :maps.from_list(parsify(:maps.to_list(map)))
  end

  def parsify(pid) when is_pid(pid) do
    :erlang.pid_to_list(pid)
  end

  def parsify(port) when is_port(port) do
    :erlang.port_to_list(port)
  end

  def parsify(ref) when is_reference(ref) do
    :erlang.ref_to_list(ref)
  end

  def parsify(fun) when is_function(fun) do
    :erlang.fun_to_list(fun)
  end

  def parsify(term) do
    term
  end
end
