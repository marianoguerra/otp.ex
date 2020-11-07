defmodule :m_etop do
  use Bitwise
  @author :"siri@erix.ericsson.se"
  require Record

  Record.defrecord(:r_etop_info, :etop_info,
    now: {0, 0, 0},
    n_procs: 0,
    wall_clock: :undefined,
    runtime: :undefined,
    run_queue: 0,
    alloc_areas: [],
    memi: [{:total, 0}, {:processes, 0}, {:ets, 0}, {:atom, 0}, {:code, 0}, {:binary, 0}],
    procinfo: []
  )

  Record.defrecord(:r_etop_proc_info, :etop_proc_info,
    pid: :undefined,
    mem: 0,
    reds: 0,
    name: :undefined,
    runtime: 0,
    cf: :undefined,
    mq: 0
  )

  Record.defrecord(:r_opts, :opts,
    node: node(),
    port: 8415,
    accum: false,
    intv: 5000,
    lines: 10,
    width: 700,
    height: 340,
    sort: :runtime,
    tracing: :on,
    out_mod: :etop_txt,
    out_proc: :undefined,
    server: :undefined,
    host: :undefined,
    tracer: :undefined,
    store: :undefined,
    accum_tab: :undefined,
    remote: :undefined
  )

  def help() do
    :io.format(
      'Usage of the Erlang top program~n~nOptions are set as command line parameters as in -node my@host~nor as parameters to etop:start([{node, my@host}, {...}]).~n~nOptions are:~n  node        atom       Required   The erlang node to measure ~n  port        integer    The used port, NOTE: due to a bug this program~n                         will hang if the port is not avaiable~n  accumulate  boolean    If true execution time is accumulated ~n  lines       integer    Number of displayed processes~n  interval    integer    Display update interval in secs~n  sort        runtime | reductions | memory | msg_q~n                         What information to sort by~n                         Default: runtime (reductions if tracing=off)~n  tracing     on | off   etop uses the erlang trace facility, and thus~n                         no other tracing is possible on the node while~n                         etop is running, unless this option is set to~n                         \'off\'. Also helpful if the etop tracing causes~n                         too high load on the measured node.~n                         With tracing off, runtime is not measured!~n  setcookie   string     Only applicable on operating system command~n                         line. Set cookie for the etop node, must be~n                         same as the cookie for the measured node.~n                         This is not an etop parameter~n'
    )
  end

  def stop() do
    case :erlang.whereis(:etop_server) do
      :undefined ->
        :not_started

      pid when is_pid(pid) ->
        send(:etop_server, :stop)
    end
  end

  def config(key, value) do
    case check_runtime_config(key, value) do
      :ok ->
        send(:etop_server, {:config, {key, value}})
        :ok

      :error ->
        {:error, :illegal_opt}
    end
  end

  defp check_runtime_config(:lines, l) when is_integer(l) and l > 0 do
    :ok
  end

  defp check_runtime_config(:interval, i) when is_integer(i) and i > 0 do
    :ok
  end

  defp check_runtime_config(:sort, s)
       when s === :runtime or
              s === :reductions or s === :memory or s === :msg_q do
    :ok
  end

  defp check_runtime_config(:accumulate, a)
       when a === true or
              a === false do
    :ok
  end

  defp check_runtime_config(_Key, _Value) do
    :error
  end

  def dump(file) do
    case :file.open(file, [:write, {:encoding, :utf8}]) do
      {:ok, fd} ->
        send(:etop_server, {:dump, fd})

      error ->
        error
    end
  end

  def start() do
    start([])
  end

  def start(opts) do
    :erlang.process_flag(:trap_exit, true)

    config1 =
      handle_args(
        :init.get_arguments() ++ opts,
        r_opts()
      )

    config2 = r_opts(config1, server: self())
    node = getopt(:node, config2)

    case :net_adm.ping(node) do
      :pang when node != node() ->
        :io.format('Error Couldn\'t connect to node ~p ~n~n', [node])
        help()
        exit('connection error')

      _pong ->
        check_runtime_tools_vsn(node)
    end

    config3 =
      cond do
        r_opts(config2, :tracing) == :on and node != node() ->
          :etop_tr.setup_tracer(config2)

        true ->
          cond do
            r_opts(config2, :sort) == :runtime ->
              r_opts(config2, sort: :reductions, tracing: :off)

            true ->
              r_opts(config2, tracing: :off)
          end
      end

    accumTab =
      :ets.new(
        :accum_tab,
        [:set, :public, {:keypos, r_etop_proc_info(:pid)}]
      )

    config4 = r_opts(config3, accum_tab: accumTab)
    out = spawn_link(r_opts(config4, :out_mod), :init, [config4])
    config5 = r_opts(config4, out_proc: out)
    init_data_handler(config5)
    :ok
  end

  defp check_runtime_tools_vsn(node) do
    case :rpc.call(node, :observer_backend, :vsn, []) do
      {:ok, vsn} ->
        check_vsn(vsn)

      _ ->
        exit('Faulty version of runtime_tools on remote node')
    end
  end

  defp check_vsn(_Vsn) do
    :ok
  end

  defp init_data_handler(config) do
    :erlang.register(:etop_server, self())

    reader =
      cond do
        r_opts(config, :tracing) == :on ->
          :etop_tr.reader(config)

        true ->
          :undefined
      end

    data_handler(reader, config)
  end

  defp data_handler(reader, opts) do
    receive do
      :stop ->
        stop(opts)
        :ok

      {:config, {key, value}} ->
        data_handler(reader, putopt(key, value, opts))

      {:dump, fd} ->
        send(r_opts(opts, :out_proc), {:dump, fd})
        data_handler(reader, opts)

      {:EXIT, ePid, reason} when ePid == r_opts(opts, :out_proc) ->
        case reason do
          :normal ->
            :ok

          _ ->
            :io.format('Output server crashed: ~tp~n', [reason])
        end

        stop(opts)
        :out_proc_stopped

      {:EXIT, ^reader, :eof} ->
        :io.format('Lost connection to node ~p exiting~n', [r_opts(opts, :node)])
        stop(opts)
        :connection_lost

      _ ->
        data_handler(reader, opts)
    end
  end

  defp stop(opts) do
    r_opts(opts, :out_mod).stop(r_opts(opts, :out_proc))

    cond do
      r_opts(opts, :tracing) == :on ->
        :etop_tr.stop_tracer(opts)

      true ->
        :ok
    end

    :erlang.unregister(:etop_server)
  end

  def update(r_opts(store: store, node: node, tracing: tracing, intv: interval) = opts) do
    pid = :erlang.spawn_link(node, :observer_backend, :etop_collect, [self()])

    info =
      receive do
        {^pid, i} ->
          i
      after
        interval ->
          :io.format('Timeout when waiting for process info from node ~p; exiting~n', [node])
          exit(:timeout)
      end

    r_etop_info(procinfo: procInfo) = info

    procInfo1 =
      cond do
        tracing == :on ->
          pI =
            :lists.map(
              fn pI = r_etop_proc_info(pid: p) ->
                case :ets.lookup(store, p) do
                  [{^p, t}] ->
                    r_etop_proc_info(pI, runtime: t)

                  [] ->
                    pI
                end
              end,
              procInfo
            )

          pI

        true ->
          :lists.map(
            fn pI ->
              r_etop_proc_info(pI, runtime: :-)
            end,
            procInfo
          )
      end

    procInfo2 = sort(opts, procInfo1)
    r_etop_info(info, procinfo: procInfo2)
  end

  defp sort(opts, pI) do
    tag = get_tag(r_opts(opts, :sort))

    pI1 =
      cond do
        r_opts(opts, :accum) ->
          pI

        true ->
          accumTab = r_opts(opts, :accum_tab)

          :lists.map(
            fn r_etop_proc_info(pid: pid, reds: reds, runtime: rT) = i ->
              newI =
                case :ets.lookup(accumTab, pid) do
                  [r_etop_proc_info(reds: oldReds, runtime: :-)] ->
                    r_etop_proc_info(i,
                      reds: reds - oldReds,
                      runtime: :-
                    )

                  [r_etop_proc_info(reds: oldReds, runtime: oldRT)] ->
                    r_etop_proc_info(i,
                      reds: reds - oldReds,
                      runtime: rT - oldRT
                    )

                  [] ->
                    i
                end

              :ets.insert(accumTab, i)
              newI
            end,
            pI
          )
      end

    pI2 = :lists.reverse(:lists.keysort(tag, pI1))
    :lists.sublist(pI2, r_opts(opts, :lines))
  end

  defp get_tag(:runtime) do
    r_etop_proc_info(:runtime)
  end

  defp get_tag(:memory) do
    r_etop_proc_info(:mem)
  end

  defp get_tag(:reductions) do
    r_etop_proc_info(:reds)
  end

  defp get_tag(:msg_q) do
    r_etop_proc_info(:mq)
  end

  def getopt(what, config) when elem(config, 0) === :opts do
    case what do
      :node ->
        r_opts(config, :node)

      :port ->
        r_opts(config, :port)

      :accum ->
        r_opts(config, :accum)

      :intv ->
        r_opts(config, :intv)

      :lines ->
        r_opts(config, :lines)

      :sort ->
        r_opts(config, :sort)

      :width ->
        r_opts(config, :width)

      :height ->
        r_opts(config, :height)

      :store ->
        r_opts(config, :store)

      :host ->
        r_opts(config, :host)
    end
  end

  defp putopt(key, value, config)
       when elem(config, 0) === :opts do
    config1 = handle_args([{key, value}], config)
    send(r_opts(config1, :out_proc), {:config, {key, value}, config1})
    config1
  end

  defp handle_args([{:node, [nodeString]} | r], config)
       when is_list(nodeString) do
    node = :erlang.list_to_atom(nodeString)
    newC = r_opts(config, node: node)
    handle_args(r, newC)
  end

  defp handle_args([{:node, node} | r], config)
       when is_atom(node) do
    newC = r_opts(config, node: node)
    handle_args(r, newC)
  end

  defp handle_args([{:port, port} | r], config)
       when is_integer(port) do
    newC = r_opts(config, port: port)
    handle_args(r, newC)
  end

  defp handle_args([{:port, [port]} | r], config)
       when is_list(port) do
    newC = r_opts(config, port: :erlang.list_to_integer(port))
    handle_args(r, newC)
  end

  defp handle_args([{:interval, time} | r], config)
       when is_integer(time) do
    newC = r_opts(config, intv: time * 1000)
    handle_args(r, newC)
  end

  defp handle_args([{:interval, [time]} | r], config)
       when is_list(time) do
    newC = r_opts(config, intv: :erlang.list_to_integer(time) * 1000)
    handle_args(r, newC)
  end

  defp handle_args([{:lines, lines} | r], config)
       when is_integer(lines) do
    newC = r_opts(config, lines: lines)
    handle_args(r, newC)
  end

  defp handle_args([{:lines, [lines]} | r], config)
       when is_list(lines) do
    newC = r_opts(config, lines: :erlang.list_to_integer(lines))
    handle_args(r, newC)
  end

  defp handle_args([{:accumulate, bool} | r], config)
       when is_atom(bool) do
    newC = r_opts(config, accum: bool)
    handle_args(r, newC)
  end

  defp handle_args([{:accumulate, [bool]} | r], config)
       when is_list(bool) do
    newC = r_opts(config, accum: :erlang.list_to_atom(bool))
    handle_args(r, newC)
  end

  defp handle_args([{:sort, sort} | r], config)
       when is_atom(sort) do
    newC = r_opts(config, sort: sort)
    handle_args(r, newC)
  end

  defp handle_args([{:sort, [sort]} | r], config)
       when is_list(sort) do
    newC = r_opts(config, sort: :erlang.list_to_atom(sort))
    handle_args(r, newC)
  end

  defp handle_args([{:output, output} | r], config)
       when is_atom(output) do
    newC = r_opts(config, out_mod: output(output))
    handle_args(r, newC)
  end

  defp handle_args([{:output, [output]} | r], config)
       when is_list(output) do
    newC = r_opts(config, out_mod: output(:erlang.list_to_atom(output)))
    handle_args(r, newC)
  end

  defp handle_args([{:tracing, onOff} | r], config)
       when is_atom(onOff) do
    newC = r_opts(config, tracing: onOff)
    handle_args(r, newC)
  end

  defp handle_args([{:tracing, [onOff]} | r], config)
       when is_list(onOff) do
    newC = r_opts(config, tracing: :erlang.list_to_atom(onOff))
    handle_args(r, newC)
  end

  defp handle_args([_ | r], c) do
    handle_args(r, c)
  end

  defp handle_args([], c) do
    c
  end

  defp output(:graphical) do
    exit({:deprecated, 'Use observer instead'})
  end

  defp output(:text) do
    :etop_txt
  end

  def loadinfo(sysI, prev) do
    r_etop_info(n_procs: procs, run_queue: rQ, now: now, wall_clock: wC, runtime: rT) = sysI
    cpu = calculate_cpu_utilization(wC, rT, r_etop_info(prev, :runtime))

    clock =
      :io_lib.format(
        '~2.2.0w:~2.2.0w:~2.2.0w',
        :erlang.tuple_to_list(
          :erlang.element(
            2,
            :calendar.now_to_datetime(now)
          )
        )
      )

    {cpu, procs, rQ, clock}
  end

  defp calculate_cpu_utilization({_, wC}, {_, rT}, _) do
    case {wC, rT} do
      {0, 0} ->
        0

      {0, _} ->
        100

      _ ->
        round(100 * rT / wC)
    end
  end

  defp calculate_cpu_utilization(_, :undefined, _) do
    0
  end

  defp calculate_cpu_utilization(wC, rTInfo, :undefined) do
    zeroRT =
      for {id, _, _} <- rTInfo do
        {id, 0, 0}
      end

    calculate_cpu_utilization(wC, rTInfo, zeroRT)
  end

  defp calculate_cpu_utilization(_, rTInfo, prevRTInfo) do
    sum =
      :lists.foldl(
        fn {{_, a0, t0}, {_, a1, t1}}, {aAcc, tAcc} ->
          {a1 - a0 + aAcc, t1 - t0 + tAcc}
        end,
        {0, 0},
        :lists.zip(prevRTInfo, rTInfo)
      )

    case sum do
      {0, 0} ->
        0

      {active, total} ->
        round(100 * active / total)
    end
  end

  def meminfo(memI, [tag | tags]) do
    [round(get_mem(tag, memI) / 1024) | meminfo(memI, tags)]
  end

  def meminfo(_MemI, []) do
    []
  end

  defp get_mem(tag, memI) do
    case :lists.keysearch(tag, 1, memI) do
      {:value, {^tag, i}} ->
        i

      _ ->
        0
    end
  end
end
