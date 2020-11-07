defmodule :m_ttb do
  use Bitwise
  @author :"siri@erix.ericsson.se"
  @author :"bartlomiej.puzon@erlang-solutions.com"
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

  def start_trace(nodes, patterns, {procs, flags}, options) do
    {:ok, _} = tracer(nodes, options)

    for args <- patterns do
      {:ok, _} = apply(:ttb, :tpl, :erlang.tuple_to_list(args))
    end

    {:ok, _} = p(procs, flags)
  end

  def tracer() do
    tracer(node())
  end

  def tracer(:shell) do
    tracer(node(), :shell)
  end

  def tracer(:dbg) do
    tracer(node(), {:shell, :only})
  end

  def tracer(nodes) do
    tracer(nodes, [])
  end

  def tracer(nodes, opt) do
    {pI, client, traci} = opt(opt)
    pid = start(traci)
    store(:tracer, [nodes, opt])
    do_tracer(nodes, pI, client, [{:ttb_control, pid} | traci])
  end

  defp do_tracer(nodes0, pI, client, traci) do
    nodes = nods(nodes0)
    clients = clients(nodes, client)
    do_tracer(clients, pI, traci)
  end

  defp do_tracer(clients, pI, traci) do
    shell = :proplists.get_value(:shell, traci, false)

    ipPortSpec =
      case :proplists.get_value(
             :queue_size,
             traci
           ) do
        :undefined ->
          0

        qS ->
          {0, qS}
      end

    defShell = fn trace ->
      :dbg.dhandler(trace, :standard_io)
    end

    {clientSucc, succ} =
      :lists.foldl(
        fn
          {n, {:local, file}, tF}, {cS, s} ->
            {tF2, fileInfo, shellOutput} =
              case shell do
                :only ->
                  {:none, :shell_only, defShell}

                true ->
                  {tF, {:file, file}, defShell}

                {:only, fun} ->
                  {:none, :shell_only, fun}

                fun
                when is_function(fun) ->
                  {tF, {:file, file}, fun}

                _ ->
                  {tF, {:file, file}, false}
              end

            host =
              case n do
                :nonode@nohost ->
                  {:ok, h} = :inet.gethostname()
                  h

                _ ->
                  [_, h] =
                    :string.lexemes(
                      :erlang.atom_to_list(n),
                      '@'
                    )

                  h
              end

            case (try do
                    :dbg.tracer(
                      n,
                      :port,
                      :dbg.trace_port(
                        :ip,
                        ipPortSpec
                      )
                    )
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              {:ok, ^n} ->
                {:ok, port} =
                  :dbg.trace_port_control(
                    n,
                    :get_listen_port
                  )

                {:ok, t} = :dbg.get_tracer(n)
                :rpc.call(n, :seq_trace, :set_system_tracer, [t])

                :dbg.trace_client(
                  :ip,
                  {host, port},
                  {&ip_to_file/2, {fileInfo, shellOutput}}
                )

                {[
                   {n, {:local, file, port}, tF2}
                   | cS
                 ], [n | s]}

              other ->
                display_warning(
                  n,
                  {:cannot_open_ip_trace_port, host, other}
                )

                {cS, s}
            end

          {n, c, _} = client, {cS, s} ->
            case (try do
                    :dbg.tracer(
                      n,
                      :port,
                      :dbg.trace_port(
                        :file,
                        c
                      )
                    )
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              {:ok, ^n} ->
                {:ok, t} = :dbg.get_tracer(n)
                :rpc.call(n, :seq_trace, :set_system_tracer, [t])
                {[client | cS], [n | s]}

              other ->
                display_warning(n, other)
                {cS, s}
            end
        end,
        {[], []},
        clients
      )

    case succ do
      [] ->
        {:ok, succ}

      _list ->
        write_info(clientSucc, pI, traci)
        {:ok, succ}
    end
  end

  defp opt(opt) when is_list(opt) do
    opt(opt, {true, :ttb, []})
  end

  defp opt(opt) do
    opt([opt])
  end

  defp opt(
         [{:process_info, pI} | o],
         {_, client, traci}
       ) do
    opt(o, {pI, client, traci})
  end

  defp opt([{:file, client} | o], {pI, _, traci}) do
    opt(
      o,
      {pI, client, [{:logfile, get_logname(client)} | traci]}
    )
  end

  defp opt(
         [{:handler, handler} | o],
         {pI, client, traci}
       ) do
    opt(o, {pI, client, [{:handler, handler} | traci]})
  end

  defp opt(
         [{:timer, {mSec, stopOpts}} | o],
         {pI, client, traci}
       ) do
    opt(
      o,
      {pI, client, [{:timer, {mSec, stopOpts}} | traci]}
    )
  end

  defp opt([{:timer, mSec} | o], {pI, client, traci}) do
    opt(o, {pI, client, [{:timer, {mSec, []}} | traci]})
  end

  defp opt(
         [{:overload_check, {mSec, m, f}} | o],
         {pI, client, traci}
       ) do
    opt(
      o,
      {pI, client, [{:overload_check, {mSec, m, f}} | traci]}
    )
  end

  defp opt([:shell | o], {pI, client, traci}) do
    opt(o, {pI, client, [{:shell, true} | traci]})
  end

  defp opt([{:shell, type} | o], {pI, client, traci}) do
    opt(o, {pI, client, [{:shell, type} | traci]})
  end

  defp opt([:resume | o], {pI, client, traci}) do
    opt(o, {pI, client, [{:resume, {true, 10000}} | traci]})
  end

  defp opt([{:resume, mSec} | o], {pI, client, traci}) do
    opt(o, {pI, client, [{:resume, {true, mSec}} | traci]})
  end

  defp opt([{:flush, mSec} | o], {pI, client, traci}) do
    opt(o, {pI, client, [{:flush, mSec} | traci]})
  end

  defp opt(
         [{:queue_size, queueSize} | o],
         {pI, client, traci}
       ) do
    opt(o, {pI, client, [{:queue_size, queueSize} | traci]})
  end

  defp opt([], opt) do
    ensure_opt(opt)
  end

  defp ensure_opt({pI, client, traci}) do
    case {:proplists.get_value(:flush, traci), client} do
      {:undefined, _} ->
        :ok

      {_, {:local, _}} ->
        exit(:flush_unsupported_with_ip_trace_port)

      {_, _} ->
        :ok
    end

    needIpTracer = :proplists.get_value(:shell, traci, false) != false

    case {needIpTracer, client} do
      {false, _} ->
        {pI, client, traci}

      {true, :ttb} ->
        {pI, {:local, :ttb}, traci}

      {true, {:local, file}} ->
        {pI, {:local, file}, traci}

      {true, _} ->
        exit(:local_client_required_on_shell_tracing)
    end
  end

  defp get_logname({:local, f}) do
    get_logname(f)
  end

  defp get_logname({:wrap, f}) do
    :filename.basename(f)
  end

  defp get_logname({:wrap, f, _, _}) do
    :filename.basename(f)
  end

  defp get_logname(f) do
    :filename.basename(f)
  end

  defp nods(:all) do
    nodes1 = remove_active([node() | :erlang.nodes()])
    remove_faulty_runtime_tools_vsn(nodes1)
  end

  defp nods(node) when is_atom(node) do
    nods([node])
  end

  defp nods(nodes) when is_list(nodes) do
    nodes1 = remove_active(nodes)
    nodes2 = remove_noexist(nodes1)
    remove_faulty_runtime_tools_vsn(nodes2)
  end

  defp remove_active(nodes) do
    active = get_nodes()

    :lists.filter(
      fn n ->
        case :lists.member(n, active) do
          false ->
            true

          true ->
            display_warning(n, :already_started)
            false
        end
      end,
      nodes
    )
  end

  defp remove_noexist(nodes) do
    :lists.filter(
      fn
        n when n === node() ->
          true

        n ->
          case :net_adm.ping(n) do
            :pong ->
              true

            :pang ->
              display_warning(n, :no_connection)
              false
          end
      end,
      nodes
    )
  end

  defp remove_faulty_runtime_tools_vsn(nodes) do
    :lists.filter(
      fn n ->
        case :rpc.call(n, :observer_backend, :vsn, []) do
          {:ok, vsn} ->
            check_vsn(n, vsn)

          _Error ->
            display_warning(n, :faulty_vsn_of_runtime_tools)
            false
        end
      end,
      nodes
    )
  end

  defp check_vsn(_Node, _Vsn) do
    true
  end

  defp clients(nodes, {:wrap, name}) do
    f = fn node ->
      traceFile = name(node, name)
      {node, {traceFile ++ '.', :wrap, '.wrp'}, traceFile}
    end

    :lists.map(f, nodes)
  end

  defp clients(nodes, {:wrap, name, size, count}) do
    f = fn node ->
      traceFile = name(node, name)
      {node, {traceFile ++ '.', :wrap, '.wrp', size, count}, traceFile}
    end

    :lists.map(f, nodes)
  end

  defp clients(nodes, {:local, realClient}) do
    wrapClients = clients(nodes, realClient)

    f = fn {node, client, traceFile} ->
      {node, {:local, client}, traceFile}
    end

    :lists.map(f, wrapClients)
  end

  defp clients(nodes, name) do
    f = fn node ->
      traceFile = name(node, name)
      {node, traceFile, traceFile}
    end

    :lists.map(f, nodes)
  end

  defp name(node, filename) do
    dir = :filename.dirname(filename)
    file = :filename.basename(filename)

    :filename.join(
      dir,
      :erlang.atom_to_list(node) ++ '-' ++ file
    )
  end

  defp store(func, args) do
    last =
      case :ets.last(:ttb_history_table) do
        :"$end_of_table" ->
          0

        int when is_integer(int) ->
          int
      end

    :ets.insert(
      :ttb_history_table,
      {last + 1, {:ttb, func, args}}
    )
  end

  def list_history() do
    case :ets.info(:ttb_history_table) do
      :undefined ->
        {:error, :not_running}

      _info ->
        :ets.tab2list(:ttb_history_table)
    end
  end

  def run_history([h | t]) do
    case run_history(h) do
      :ok ->
        run_history(t)

      {:error, :not_found} ->
        {:error, {:not_found, h}}
    end
  end

  def run_history(:all) do
    currentHist = :ets.tab2list(:ttb_history_table)
    :ets.delete_all_objects(:ttb_history_table)

    for {_, mFA} <- currentHist do
      run_printed(mFA, true)
    end
  end

  def run_history(:all_silent) do
    currentHist = :ets.tab2list(:ttb_history_table)
    :ets.delete_all_objects(:ttb_history_table)

    for {_, mFA} <- currentHist do
      run_printed(mFA, false)
    end
  end

  def run_history([]) do
    :ok
  end

  def run_history(n) do
    case (try do
            :ets.lookup(:ttb_history_table, n)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      [{^n, {m, f, a}}] ->
        run_printed({m, f, a}, true)

      _ ->
        {:error, :not_found}
    end
  end

  defp run_printed({m, f, a}, verbose) do
    verbose and print_func(m, f, a)
    r = apply(m, f, a)
    verbose and print_result(r)
  end

  def write_config(configFile, :all) do
    write_config(configFile, [:_])
  end

  def write_config(configFile, config) do
    write_config(configFile, config, [])
  end

  def write_config(configFile, :all, opt) do
    write_config(configFile, [:_], opt)
  end

  def write_config(configFile, config, opt) when not is_list(opt) do
    write_config(configFile, config, [opt])
  end

  def write_config(configFile, nums, opt)
      when (is_list(nums) and
              is_integer(hd(nums))) or
             nums === [:_] do
    f = fn n ->
      :ets.select(:ttb_history_table, [{{n, :"$1"}, [], [:"$1"]}])
    end

    config = :lists.append(:lists.map(f, nums))
    do_write_config(configFile, config, opt)
  end

  def write_config(configFile, config, opt) when is_list(config) do
    case check_config(config, []) do
      {:ok, config1} ->
        do_write_config(configFile, config1, opt)

      error ->
        error
    end
  end

  defp do_write_config(configFile, config, opt) do
    case opt do
      [:append] ->
        :ok

      [] ->
        :file.delete(configFile)
    end

    write_binary(configFile, config)
  end

  defp check_config([{:ttb = mod, func, args} | rest], acc) do
    case :erlang.function_exported(mod, func, length(args)) do
      true ->
        check_config(rest, [{mod, func, args} | acc])

      false ->
        {:error, {:not_exported, {mod, func, args}}}
    end
  end

  defp check_config([{mod, func, args} | rest], acc) do
    check_config(rest, [{mod, func, args} | acc])
  end

  defp check_config([], acc) do
    {:ok, :lists.reverse(acc)}
  end

  defp check_config([other | _Rest], _Acc) do
    {:error, {:illegal_config, other}}
  end

  def list_config(configFile) do
    case :file.read_file(configFile) do
      {:ok, b} ->
        read_config(b, [], 1)

      error ->
        error
    end
  end

  defp read_config(<<>>, acc, _N) do
    :lists.reverse(acc)
  end

  defp read_config(b, acc, n) do
    {{m, f, a}, rest} = get_term(b)
    read_config(rest, [{n, {m, f, a}} | acc], n + 1)
  end

  def run_config(configFile) do
    case list_config(configFile) do
      config when is_list(config) ->
        :lists.foreach(
          fn {_, {m, f, a}} ->
            print_func(m, f, a)
            r = apply(m, f, a)
            print_result(r)
          end,
          config
        )

      error ->
        error
    end
  end

  def run_config(configFile, n) do
    case list_config(configFile) do
      config when is_list(config) ->
        case :lists.keysearch(n, 1, config) do
          {:value, {^n, {m, f, a}}} ->
            print_func(m, f, a)
            apply(m, f, a)

          false ->
            {:error, :not_found}
        end

      error ->
        error
    end
  end

  defp print_func(m, f, a) do
    args = arg_list(a, [])
    :io.format('~w:~tw(~ts) ->~n', [m, f, args])
  end

  defp print_result(r) do
    :io.format('~tp~n~n', [r])
  end

  defp arg_list([], []) do
    ''
  end

  defp arg_list([a1], acc) do
    acc ++ :io_lib.format('~tw', [a1])
  end

  defp arg_list([a1 | a], acc) do
    arg_list(a, acc ++ :io_lib.format('~tw,', [a1]))
  end

  def p(procsPorts0, flags0) do
    ensure_no_overloaded_nodes()
    store(:p, [procsPorts0, flags0])
    no_store_p(procsPorts0, flags0)
  end

  defp no_store_p(procsPorts0, flags0) do
    case transform_flags(to_list(flags0)) do
      {:error, reason} ->
        {:error, reason}

      flags ->
        procsPorts = procs_ports(procsPorts0)

        case :lists.foldl(
               fn p, {pMatched, ps} ->
                 case :dbg.p(p, flags) do
                   {:ok, matched} ->
                     {[{p, matched} | pMatched], [p | ps]}

                   {:error, reason} ->
                     display_warning(p, reason)
                     {pMatched, ps}
                 end
               end,
               {[], []},
               procsPorts
             ) do
          {[], []} ->
            {:error, :no_match}

          {succMatched, succ} ->
            no_store_write_trace_info(:flags, {succ, flags})
            send(:ttb, :trace_started)
            {:ok, succMatched}
        end
    end
  end

  defp transform_flags([:clear]) do
    [:clear]
  end

  defp transform_flags(flags) do
    :dbg.transform_flags([:timestamp | flags])
  end

  defp procs_ports(procs) when is_list(procs) do
    :lists.foldl(
      fn p, acc ->
        proc_port(p) ++ acc
      end,
      [],
      procs
    )
  end

  defp procs_ports(proc) do
    proc_port(proc)
  end

  defp proc_port(p)
       when p === :all or p === :ports or
              p === :processes or p === :existing or
              p === :existing_ports or p === :existing_processes or
              p === :new or p === :new_ports or
              p === :new_processes do
    [p]
  end

  defp proc_port(name) when is_atom(name) do
    [name]
  end

  defp proc_port(pid) when is_pid(pid) do
    [pid]
  end

  defp proc_port(port) when is_port(port) do
    [port]
  end

  defp proc_port({:global, name}) do
    case :global.whereis_name(name) do
      pid when is_pid(pid) ->
        [pid]

      :undefined ->
        []
    end
  end

  def tp(a, b) do
    ensure_no_overloaded_nodes()
    store(:tp, [a, ms(b)])
    :dbg.tp(a, ms(b))
  end

  def tp(a, b, c) do
    ensure_no_overloaded_nodes()
    store(:tp, [a, b, ms(c)])
    :dbg.tp(a, b, ms(c))
  end

  def tp(a, b, c, d) do
    ensure_no_overloaded_nodes()
    store(:tp, [a, b, c, ms(d)])
    :dbg.tp(a, b, c, ms(d))
  end

  def tpl(a, b) do
    ensure_no_overloaded_nodes()
    store(:tpl, [a, ms(b)])
    :dbg.tpl(a, ms(b))
  end

  def tpl(a, b, c) do
    ensure_no_overloaded_nodes()
    store(:tpl, [a, b, ms(c)])
    :dbg.tpl(a, b, ms(c))
  end

  def tpl(a, b, c, d) do
    ensure_no_overloaded_nodes()
    store(:tpl, [a, b, c, ms(d)])
    :dbg.tpl(a, b, c, ms(d))
  end

  def tpe(a, b) do
    ensure_no_overloaded_nodes()
    store(:tpe, [a, ms(b)])
    :dbg.tpe(a, ms(b))
  end

  def ctp() do
    store(:ctp, [])
    :dbg.ctp()
  end

  def ctp(a) do
    store(:ctp, [a])
    :dbg.ctp(a)
  end

  def ctp(a, b) do
    store(:ctp, [a, b])
    :dbg.ctp(a, b)
  end

  def ctp(a, b, c) do
    store(:ctp, [a, b, c])
    :dbg.ctp(a, b, c)
  end

  def ctpl() do
    store(:ctpl, [])
    :dbg.ctpl()
  end

  def ctpl(a) do
    store(:ctpl, [a])
    :dbg.ctpl(a)
  end

  def ctpl(a, b) do
    store(:ctpl, [a, b])
    :dbg.ctpl(a, b)
  end

  def ctpl(a, b, c) do
    store(:ctpl, [a, b, c])
    :dbg.ctpl(a, b, c)
  end

  def ctpg() do
    store(:ctpg, [])
    :dbg.ctpg()
  end

  def ctpg(a) do
    store(:ctpg, [a])
    :dbg.ctpg(a)
  end

  def ctpg(a, b) do
    store(:ctpg, [a, b])
    :dbg.ctpg(a, b)
  end

  def ctpg(a, b, c) do
    store(:ctpg, [a, b, c])
    :dbg.ctpg(a, b, c)
  end

  def ctpe(a) do
    store(:ctpe, [a])
    :dbg.ctpe(a)
  end

  defp ms(:return) do
    [{:_, [], [{:return_trace}]}]
  end

  defp ms(:caller) do
    [{:_, [], [{:message, {:caller}}]}]
  end

  defp ms({:codestr, funStr}) do
    {:ok, mS} = string2ms(funStr)
    mS
  end

  defp ms(other) do
    other
  end

  defp ensure_no_overloaded_nodes() do
    overloaded =
      case :erlang.whereis(:ttb) do
        :undefined ->
          []

        _ ->
          send(:ttb, {:get_overloaded, self()})

          receive do
            {:overloaded, o} ->
              o
          end
      end

    case overloaded do
      [] ->
        :ok

      ^overloaded ->
        exit({:error, :overload_protection_active, overloaded})
    end
  end

  defp string2ms(funStr) do
    case :erl_scan.string(fix_dot(funStr)) do
      {:ok, tokens, _} ->
        case :erl_parse.parse_exprs(tokens) do
          {:ok, [expression]} ->
            case expression do
              {_, _, {:clauses, clauses}} ->
                {:ok, :ms_transform.transform_from_shell(:dbg, clauses, [])}

              _ ->
                {:error, :fun_format}
            end

          _ ->
            {:error, :fun_format}
        end

      _ ->
        {:error, :fun_format}
    end
  end

  defp fix_dot(funStr) do
    [h | rest] = :lists.reverse(funStr)

    case h do
      ?. ->
        funStr

      ^h ->
        :lists.reverse([[?., h] | rest])
    end
  end

  def seq_trigger_ms() do
    seq_trigger_ms(:all)
  end

  def seq_trigger_ms(:all) do
    seq_trigger_ms([:send, :receive, :print, :timestamp])
  end

  def seq_trigger_ms(flag) when is_atom(flag) do
    seq_trigger_ms([flag], [])
  end

  def seq_trigger_ms(flags) do
    seq_trigger_ms(flags, [])
  end

  defp seq_trigger_ms([flag | flags], body) do
    case :lists.member(
           flag,
           [:send, :receive, :print, :timestamp]
         ) do
      true ->
        seq_trigger_ms(
          flags,
          [{:set_seq_token, flag, true} | body]
        )

      false ->
        {:error, {:illegal_flag, flag}}
    end
  end

  defp seq_trigger_ms([], body) do
    [{:_, [], body}]
  end

  def write_trace_info(key, what) do
    store(:write_trace_info, [key, what])
    no_store_write_trace_info(key, what)
  end

  defp no_store_write_trace_info(key, what) do
    case :erlang.whereis(:ttb) do
      :undefined ->
        :ok

      pid when is_pid(pid) ->
        send(:ttb, {:write_trace_info, key, what})
    end

    :ok
  end

  def stop() do
    stop([])
  end

  def stop(opts) when is_list(opts) do
    fetch = stop_opts(opts)

    result =
      case :erlang.whereis(:ttb) do
        :undefined ->
          :ok

        pid when is_pid(pid) ->
          send(:ttb, {:stop, fetch, self()})

          receive do
            {:ttb, r} ->
              r
          end
      end

    case {fetch, result} do
      {:nofetch, _} ->
        :ok

      {_, {:stopped, _}} ->
        :io.format('Stored logs in ~ts~n', [:erlang.element(2, result)])

      {_, _} ->
        :ok
    end

    stop_return(result, opts)
  end

  def stop(opts) do
    stop([opts])
  end

  defp stop_opts(opts) do
    fetchDir = :proplists.get_value(:fetch_dir, opts)
    ensure_fetch_dir(fetchDir)

    formatData =
      case :proplists.get_value(
             :format,
             opts
           ) do
        :undefined ->
          false

        true ->
          {:format, []}

        fOpts ->
          {:format, fOpts}
      end

    case {formatData, :lists.member(:return_fetch_dir, opts)} do
      {false, true} ->
        {:fetch, fetchDir}

      {false, false} ->
        case :lists.member(:nofetch, opts) do
          false ->
            {:fetch, fetchDir}

          true ->
            :nofetch
        end

      {^formatData, _} ->
        {formatData, fetchDir}
    end
  end

  defp ensure_fetch_dir(:undefined) do
    :ok
  end

  defp ensure_fetch_dir(dir) do
    case :filelib.is_file(dir) do
      true ->
        throw({:error, :exists, dir})

      false ->
        :ok
    end
  end

  defp stop_return(r, opts) do
    case {:lists.member(:return_fetch_dir, opts), r} do
      {true, _} ->
        r

      {false, {:stopped, _}} ->
        :stopped

      {false, _} ->
        :stopped
    end
  end

  defp start(sessionInfo) do
    case :erlang.whereis(:ttb) do
      :undefined ->
        parent = self()

        pid =
          spawn(fn ->
            init(parent, sessionInfo)
          end)

        receive do
          {:started, ^pid} ->
            :ok
        end

        pid

      pid when is_pid(pid) ->
        pid
    end
  end

  defp init(parent, sessionInfo) do
    :erlang.register(:ttb, self())

    :ets.new(
      :ttb_history_table,
      [:ordered_set, :named_table, :public]
    )

    send(parent, {:started, self()})

    newSessionInfo = [
      [{:partials, 0}, {:dead_nodes, []}]
      | sessionInfo
    ]

    try_send_flush_tick(newSessionInfo)
    loop(:dict.new(), newSessionInfo)
  end

  defp loop(nodeInfo, sessionInfo) do
    receive do
      {:init_node, node, metaFile, pI, traci} ->
        :erlang.monitor_node(node, true)

        {absoluteMetaFile, metaPid} =
          case :rpc.call(
                 node,
                 :observer_backend,
                 :ttb_init_node,
                 [metaFile, pI, traci]
               ) do
            {:ok, mF, mP} ->
              {mF, mP}

            {:badrpc, :nodedown} ->
              {metaFile, :undefined}
          end

        loop(
          :dict.store(node, {absoluteMetaFile, metaPid}, nodeInfo),
          sessionInfo
        )

      {:ip_to_file_trace_port, port, sender} ->
        ports = :proplists.get_value(:ip_to_file_trace_ports, sessionInfo, [])

        newSessionInfo = [
          {:ip_to_file_trace_ports, [port | ports]}
          | sessionInfo
        ]

        send(sender, {:ttb, :ok})
        loop(nodeInfo, newSessionInfo)

      {:get_nodes, sender} ->
        send(sender, {:ttb, :dict.fetch_keys(nodeInfo)})
        loop(nodeInfo, sessionInfo)

      {:write_trace_info, key, what} ->
        :dict.fold(
          fn node, {_MetaFile, metaPid}, _ ->
            :rpc.call(node, :observer_backend, :ttb_write_trace_info, [metaPid, key, what])
          end,
          :ok,
          nodeInfo
        )

        loop(nodeInfo, sessionInfo)

      {:nodedown, node} ->
        newState = make_node_dead(node, nodeInfo, sessionInfo)
        loop(:dict.erase(node, nodeInfo), newState)

      {:noderesumed, node, reporter} ->
        {metaFile, currentSuffix, newState} = make_node_alive(node, sessionInfo)
        fetch_partial_result(node, metaFile, currentSuffix)

        spawn(fn ->
          resume_trace(reporter)
        end)

        loop(nodeInfo, newState)

      {:timeout, stopOpts} ->
        spawn(:ttb, :stop, [stopOpts])
        loop(nodeInfo, sessionInfo)

      {:node_overloaded, node} ->
        :io.format('Overload check activated on node: ~p.~n', [node])

        {overloaded, sI} =
          {:proplists.get_value(:overloaded, sessionInfo, []),
           :lists.keydelete(:overloaded, 1, sessionInfo)}

        loop(
          nodeInfo,
          [{:overloaded, [node | overloaded]} | sI]
        )

      {:get_overloaded, pid} ->
        send(pid, {:overloaded, :proplists.get_value(:overloaded, sessionInfo, [])})
        loop(nodeInfo, sessionInfo)

      :trace_started ->
        case :proplists.get_value(:timer, sessionInfo) do
          :undefined ->
            :ok

          {mSec, stopOpts} ->
            :erlang.send_after(mSec, self(), {:timeout, stopOpts})
        end

        loop(nodeInfo, sessionInfo)

      :flush_timeout ->
        for node <- :dict.fetch_keys(nodeInfo) do
          :dbg.flush_trace_port(node)
        end

        try_send_flush_tick(sessionInfo)
        loop(nodeInfo, sessionInfo)

      {:stop, :nofetch, sender} ->
        do_stop(:nofetch, sender, nodeInfo, sessionInfo)

      {:stop, fetchSpec, sender} ->
        case :proplists.get_value(:shell, sessionInfo, false) do
          :only ->
            do_stop(:nofetch, sender, nodeInfo, sessionInfo)

          _ ->
            do_stop(fetchSpec, sender, nodeInfo, sessionInfo)
        end
    end
  end

  defp do_stop(:nofetch, sender, nodeInfo, sessionInfo) do
    write_config('ttb_last_config', :all)

    :dict.fold(
      fn node, {_, metaPid}, _ ->
        :rpc.call(node, :observer_backend, :ttb_stop, [metaPid])
      end,
      :ok,
      nodeInfo
    )

    stop_ip_to_file_trace_ports(sessionInfo)
    :dbg.stop_clear()
    :ets.delete(:ttb_history_table)
    send(sender, {:ttb, :stopped})
  end

  defp do_stop({fetchOrFormat, userDir}, sender, nodeInfo, sessionInfo) do
    write_config('ttb_last_config', :all)
    localhost = host(node())

    dir =
      get_fetch_dir(
        userDir,
        :proplists.get_value(:logfile, sessionInfo)
      )

    :ok = :filelib.ensure_dir(:filename.join(dir, '*'))

    allNodesAndMeta =
      :dict.fold(
        fn node, {metaFile, metaPid}, nodes ->
          :rpc.call(node, :observer_backend, :ttb_stop, [metaPid])
          [{node, metaFile} | nodes]
        end,
        [],
        nodeInfo
      )

    stop_ip_to_file_trace_ports(sessionInfo)
    :dbg.stop_clear()

    allNodes =
      :lists.map(
        fn {node, metaFile} ->
          spawn(fn ->
            fetch_report(localhost, dir, node, metaFile)
          end)

          node
        end,
        allNodesAndMeta
      )

    :ets.delete(:ttb_history_table)
    wait_for_fetch(allNodes)

    copy_partials(
      dir,
      :proplists.get_value(:partials, sessionInfo)
    )

    absname = :filename.absname(dir)

    case fetchOrFormat do
      :fetch ->
        :ok

      {:format, opts} ->
        format(dir, opts)
    end

    send(sender, {:ttb, {:stopped, absname}})
  end

  defp stop_ip_to_file_trace_ports(sessionInfo) do
    :lists.foreach(
      fn port ->
        case :lists.member(port, :erlang.ports()) do
          true ->
            :dbg.deliver_and_flush(port)
            :erlang.port_close(port)

          false ->
            :ok
        end
      end,
      :proplists.get_value(:ip_to_file_trace_ports, sessionInfo, [])
    )
  end

  defp make_node_dead(node, nodeInfo, sessionInfo) do
    {metaFile, _} = :dict.fetch(node, nodeInfo)

    newDeadNodes = [
      {node, metaFile}
      | :proplists.get_value(:dead_nodes, sessionInfo)
    ]

    [
      {:dead_nodes, newDeadNodes}
      | :lists.keydelete(:dead_nodes, 1, sessionInfo)
    ]
  end

  defp make_node_alive(node, sessionInfo) do
    deadNodes =
      :proplists.get_value(
        :dead_nodes,
        sessionInfo
      )

    partials = :proplists.get_value(:partials, sessionInfo)
    {:value, {_, metaFile}, dn2} = :lists.keytake(node, 1, deadNodes)
    sessionInfo2 = :lists.keyreplace(:dead_nodes, 1, sessionInfo, {:dead_nodes, dn2})

    {metaFile, partials + 1,
     :lists.keyreplace(:partials, 1, sessionInfo2, {:partials, partials + 1})}
  end

  defp try_send_flush_tick(state) do
    case :proplists.get_value(:flush, state) do
      :undefined ->
        :ok

      mSec ->
        :erlang.send_after(mSec, self(), :flush_timeout)
    end
  end

  defp get_fetch_dir(:undefined, :undefined) do
    'ttb_upload_' ++ 'ttb' ++ ts()
  end

  defp get_fetch_dir(:undefined, logname) do
    'ttb_upload_' ++ logname ++ ts()
  end

  defp get_fetch_dir(dir, _) do
    dir
  end

  defp resume_trace(reporter) do
    :ttb.run_history(:all_silent)
    send(reporter, :trace_resumed)
  end

  defp get_nodes() do
    send(:ttb, {:get_nodes, self()})

    receive do
      {:ttb, nodes} ->
        nodes
    end
  end

  defp ts() do
    {{y, m, d}, {h, min, s}} = :calendar.now_to_local_time(:erlang.timestamp())
    :io_lib.format('-~4.4.0w~2.2.0w~2.2.0w-~2.2.0w~2.2.0w~2.2.0w', [y, m, d, h, min, s])
  end

  defp copy_partials(_, 0) do
    :ok
  end

  defp copy_partials(dir, num) do
    partialDir = 'ttb_partial_result' ++ :erlang.integer_to_list(num)

    :file.rename(
      partialDir,
      :filename.join(dir, partialDir)
    )

    copy_partials(dir, num - 1)
  end

  defp fetch_partial_result(node, metaFile, current) do
    dirName = 'ttb_partial_result' ++ :erlang.integer_to_list(current)

    case :file.list_dir(dirName) do
      {:error, :enoent} ->
        :ok

      {:ok, files} ->
        for file <- files do
          :file.delete(:filename.join(dirName, file))
        end

        :file.del_dir(dirName)
    end

    :file.make_dir(dirName)
    fetch(host(node()), dirName, node, metaFile)
  end

  defp fetch_report(localhost, dir, node, metaFile) do
    fetch(localhost, dir, node, metaFile)
    send(:ttb, {:fetch_complete, node})
  end

  defp fetch(localhost, dir, node, metaFile) do
    case host(node) == localhost or is_local(metaFile) do
      true ->
        files = get_filenames(node, metaFile)

        :lists.foreach(
          fn file0 ->
            dest =
              :filename.join(
                dir,
                :filename.basename(file0)
              )

            :file.rename(file0, dest)
          end,
          files
        )

      false ->
        {:ok, lSock} =
          :gen_tcp.listen(
            0,
            [:binary, {:packet, 2}, {:active, false}]
          )

        {:ok, port} = :inet.port(lSock)
        enc = :file.native_name_encoding()

        args =
          case :rpc.call(node, :erlang, :function_exported, [:observer_backend, :ttb_fetch, 3]) do
            true ->
              [metaFile, {port, localhost}, enc]

            false ->
              [metaFile, {port, localhost}]
          end

        :rpc.cast(node, :observer_backend, :ttb_fetch, args)
        {:ok, sock} = :gen_tcp.accept(lSock)
        receive_files(dir, sock, :undefined, enc)
        :ok = :gen_tcp.close(lSock)
        :ok = :gen_tcp.close(sock)
    end
  end

  defp is_local({:local, _, _}) do
    true
  end

  defp is_local(_) do
    false
  end

  defp get_filenames(_N, {:local, f, _}) do
    :observer_backend.ttb_get_filenames(f)
  end

  defp get_filenames(n, f) do
    :rpc.call(n, :observer_backend, :ttb_get_filenames, [f])
  end

  defp receive_files(dir, sock, fd, enc) do
    case :gen_tcp.recv(sock, 0) do
      {:ok, <<0, bin::binary>>} ->
        :file.write(fd, bin)
        receive_files(dir, sock, fd, enc)

      {:ok, <<code, bin::binary>>}
      when code == 1 or
             code == 2 or code == 3 ->
        file0 = decode_filename(code, bin, enc)
        file = :filename.join(dir, file0)
        {:ok, fd1} = :file.open(file, [:raw, :write])
        receive_files(dir, sock, fd1, enc)

      {:error, :closed} ->
        :ok = :file.close(fd)
    end
  end

  defp decode_filename(1, bin, _Enc) do
    :erlang.binary_to_list(bin)
  end

  defp decode_filename(2, bin, enc) do
    :unicode.characters_to_list(bin, enc)
  end

  defp decode_filename(3, bin, :latin1) do
    file0 = :unicode.characters_to_list(bin, :utf8)

    file =
      for x <- file0 do
        case x do
          high when high > 255 ->
            ['\\\\x{', :erlang.integer_to_list(x, 16), ?}]

          low ->
            low
        end
      end

    :io.format(
      'Warning: fetching file with faulty filename encoding ~ts~nWill be written as ~ts~n',
      [file0, file]
    )

    file
  end

  defp host(node) do
    [_name, host] = :string.lexemes(:erlang.atom_to_list(node), '@')
    host
  end

  defp wait_for_fetch([]) do
    :ok
  end

  defp wait_for_fetch(nodes) do
    receive do
      {:fetch_complete, node} ->
        wait_for_fetch(:lists.delete(node, nodes))
    end
  end

  defp write_info(nodes, pI, traci) do
    {:ok, cwd} = :file.get_cwd()

    :lists.foreach(
      fn
        {n, {:local, c, _}, f} ->
          metaFile =
            case f do
              :none ->
                :none

              ^f ->
                absFile = :filename.join(cwd, f) ++ '.ti'
                :file.delete(absFile)
                absFile
            end

          traci1 = [[{:node, n}, {:file, c}] | traci]
          {:ok, port} = :dbg.get_tracer(n)
          send(:ttb, {:init_node, n, {:local, metaFile, port}, pI, traci1})

        {n, c, f} ->
          metaFile = f ++ '.ti'
          traci1 = [[{:node, n}, {:file, c}] | traci]
          send(:ttb, {:init_node, n, metaFile, pI, traci1})
      end,
      nodes
    )
  end

  def get_et_handler() do
    {&:ttb_et.handler/4, :initial}
  end

  def format(files) do
    format(files, [])
  end

  def format(files, opt) do
    {out, handler, disableSort} = format_opt(opt)
    :ets.new(:ttb, [:named_table])
    format(files, out, handler, disableSort)
  end

  defp format(file, out, handler, disableSort)
       when is_list(file) and is_integer(hd(file)) do
    files =
      case :filelib.is_dir(file) do
        true ->
          list = :filelib.wildcard(:filename.join(file, 'ttb_partial_result' ++ '*'))
          :lists.append(collect_files([file | list]))

        false ->
          [file]
      end

    format(files, out, handler, disableSort)
  end

  defp format(files, out, handler, disableSort)
       when is_list(files) and is_list(hd(files)) do
    stopDbg =
      case :erlang.whereis(:dbg) do
        :undefined ->
          true

        _ ->
          false
      end

    details =
      :lists.foldl(
        fn file, acc ->
          [prepare(file) | acc]
        end,
        [],
        files
      )

    fd = get_fd(out)
    realHandler = get_handler(handler, files)
    r = do_format(fd, details, disableSort, realHandler)
    :file.close(fd)
    :ets.delete(:ttb)

    case stopDbg do
      true ->
        :dbg.stop_clear()

      false ->
        :ok
    end

    r
  end

  defp collect_files(dirs) do
    :lists.map(
      fn dir ->
        metaFiles = :filelib.wildcard(:filename.join(dir, '*.ti'))

        :lists.map(
          fn m ->
            sub = :filename.rootname(m, '.ti')

            case :filelib.is_file(sub) do
              true ->
                sub

              false ->
                sub ++ '.*.wrp'
            end
          end,
          metaFiles
        )
      end,
      dirs
    )
  end

  defp get_handler(:undefined, files) do
    {traci, _} = read_traci(hd(files))

    case :dict.find(:handler, traci) do
      :error ->
        {&defaulthandler/4, :initial}

      {:ok, [handler]} ->
        handler
    end
  end

  defp get_handler(handler, _) do
    handler
  end

  defp prepare(file) do
    {traci, proci} = read_traci(file)
    node = get_node(traci)

    :lists.foreach(
      fn {pid, pI} ->
        :ets.insert(:ttb, {pid, pI, node})
      end,
      proci
    )

    fileOrWrap = get_file(file, traci)
    {fileOrWrap, traci}
  end

  defp format_opt(opt) when is_list(opt) do
    out =
      case :lists.keysearch(:out, 1, opt) do
        {:value, {:out, o}} ->
          o

        _ ->
          :standard_io
      end

    handler =
      case :lists.keysearch(:handler, 1, opt) do
        {:value, {:handler, h}} ->
          h

        _ ->
          :undefined
      end

    disableSort = :proplists.get_value(:disable_sort, opt, false)
    {out, handler, disableSort}
  end

  defp format_opt(opt) do
    format_opt([opt])
  end

  defp read_traci(file) do
    metaFile = get_metafile(file)

    case :file.read_file(metaFile) do
      {:ok, b} ->
        interpret_binary(b, :dict.new(), [])

      _ ->
        :io.format('Warning: no meta data file: ~ts~n', [metaFile])
        {:dict.new(), []}
    end
  end

  defp get_metafile(file) do
    case :filename.rootname(file, '.wrp') do
      ^file ->
        file ++ '.ti'

      wrap ->
        :filename.rootname(wrap) ++ '.ti'
    end
  end

  defp interpret_binary(<<>>, dict, p) do
    {dict, :lists.reverse(p)}
  end

  defp interpret_binary(b, dict, p) do
    {term, rest} = get_term(b)

    {dict1, p1} =
      case term do
        {:pid, pI} ->
          {dict, [pI | p]}

        {key, val} ->
          {:dict.update(
             key,
             fn val0 ->
               [val | val0]
             end,
             [val],
             dict
           ), p}
      end

    interpret_binary(rest, dict1, p1)
  end

  defp get_fd(out) do
    case out do
      :standard_io ->
        out

      _file ->
        :file.delete(out)

        case :file.open(out, [:append, {:encoding, :utf8}]) do
          {:ok, fd} ->
            fd

          error ->
            exit(error)
        end
    end
  end

  defp get_node(traci) do
    case :dict.find(:node, traci) do
      {:ok, [node]} ->
        node

      :error ->
        :unknown
    end
  end

  defp get_file(file, traci) do
    case :dict.find(:file, traci) do
      {:ok, [client]} ->
        check_client(client, file)

      :error ->
        check_exists(file)
    end
  end

  defp check_client(client, file) when is_list(client) do
    check_exists(file)
  end

  defp check_client(client, file)
       when is_tuple(client) and
              :erlang.element(2, client) == :wrap do
    root = :filename.rootname(file, '.wrp')

    case :filename.extension(root) do
      '.*' ->
        part1 = :filename.rootname(root, '*')
        :erlang.setelement(1, client, part1)

      _ ->
        check_exists(file)
    end
  end

  defp check_exists(file) do
    case :file.read_file_info(file) do
      {:ok, r_file_info(type: :regular)} ->
        file

      _ ->
        exit({:error, :no_file})
    end
  end

  defp do_format(fd, details, disableSort, handler) do
    clients =
      :lists.foldl(
        fn {fileOrWrap, traci}, acc ->
          [start_client(fileOrWrap, traci) | acc]
        end,
        [],
        details
      )

    init_collector(fd, clients, disableSort, handler)
  end

  defp start_client(fileOrWrap, traci) do
    :dbg.trace_client(:file, fileOrWrap, {&handler/2, :dict.to_list(traci)})
  end

  defp handler(trace, traci) do
    receive do
      {:get, collector} ->
        send(collector, {self(), {trace, traci}})

      :done ->
        :ok
    end

    traci
  end

  defp handler2(trace, {fd, traci, {fun, state}})
       when is_function(fun) do
    {fun, fun.(fd, trace, traci, state)}
  end

  defp handler2(trace, {fd, traci, {{m, f}, state}})
       when is_atom(m) and is_atom(f) do
    {{m, f}, apply(m, f, [fd, trace, traci, state])}
  end

  defp defaulthandler(fd, trace, _Traci, :initial) do
    :dbg.dhandler(trace, fd)
  end

  defp defaulthandler(_Fd, trace, _Traci, state) do
    :dbg.dhandler(trace, state)
  end

  defp init_collector(fd, clients, disableSort, handler) do
    collected = get_first(clients)

    case disableSort do
      true ->
        collector(fd, collected, disableSort, handler)

      false ->
        collector(fd, sort(collected), disableSort, handler)
    end
  end

  defp collector(fd, [{_, {client, {trace, traci}}} | rest], disableSort, commonState) do
    trace1 = update_procinfo(trace)

    commonState2 =
      handler2(
        trace1,
        {fd, traci, commonState}
      )

    case get_next(client) do
      :end_of_trace ->
        collector(fd, rest, disableSort, commonState2)

      next ->
        case disableSort do
          false ->
            collector(fd, sort([next | rest]), disableSort, commonState2)

          true ->
            collector(fd, [next | rest], disableSort, commonState2)
        end
    end
  end

  defp collector(fd, [], _, commonState) do
    handler2(
      :end_of_trace,
      {fd, :end_of_trace, commonState}
    )

    :ok
  end

  defp update_procinfo({:drop, _N} = trace) do
    trace
  end

  defp update_procinfo(trace)
       when :erlang.element(
              1,
              trace
            ) == :seq_trace do
    info = :erlang.element(3, trace)

    info1 =
      case info do
        {:send, serial, from, to, msg} ->
          {:send, serial, get_procinfo(from), get_procinfo(to), msg}

        {:receive, serial, from, to, msg} ->
          {:receive, serial, get_procinfo(from), get_procinfo(to), msg}

        {:print, serial, from, void, userInfo} ->
          {:print, serial, get_procinfo(from), void, userInfo}

        other ->
          other
      end

    :erlang.setelement(3, trace, info1)
  end

  defp update_procinfo(trace)
       when :erlang.element(
              3,
              trace
            ) == :send do
    pI = get_procinfo(:erlang.element(5, trace))
    :erlang.setelement(5, trace, pI)
  end

  defp update_procinfo(trace) do
    pid = :erlang.element(2, trace)
    procInfo = get_procinfo(pid)
    :erlang.setelement(2, trace, procInfo)
  end

  defp get_procinfo(pid) when is_pid(pid) or is_port(pid) do
    case :ets.lookup(:ttb, pid) do
      [pI] ->
        pI

      [] ->
        pid
    end
  end

  defp get_procinfo(name) when is_atom(name) do
    case :ets.match_object(:ttb, {:_, name, node()}) do
      [pI] ->
        pI

      [] ->
        name
    end
  end

  defp get_procinfo({name, node}) when is_atom(name) do
    case :ets.match_object(:ttb, {:_, name, node}) do
      [pI] ->
        pI

      [] ->
        {name, node}
    end
  end

  defp get_first([client | clients]) do
    send(client, {:get, self()})

    receive do
      {^client, {:end_of_trace, _}} ->
        get_first(clients)

      {^client, {trace, _}} = next ->
        [{timestamp(trace), next} | get_first(clients)]
    end
  end

  defp get_first([]) do
    []
  end

  defp get_next(client) when is_pid(client) do
    send(client, {:get, self()})

    receive do
      {^client, {:end_of_trace, _}} ->
        :end_of_trace

      {^client, {trace, traci}} ->
        {timestamp(trace), {client, {trace, traci}}}
    end
  end

  defp sort(list) do
    :lists.keysort(1, list)
  end

  defp timestamp(trace)
       when :erlang.element(
              1,
              trace
            ) === :trace_ts or
              (:erlang.element(1, trace) === :seq_trace and
                 tuple_size(trace) === 4) do
    :erlang.element(tuple_size(trace), trace)
  end

  defp timestamp(_Trace) do
    0
  end

  defp to_list(atom) when is_atom(atom) do
    [atom]
  end

  defp to_list(list) when is_list(list) do
    list
  end

  defp write_binary(file, termList) do
    {:ok, fd} = :file.open(file, [:raw, :append])
    :observer_backend.ttb_write_binary(fd, termList)
    :file.close(fd)
  end

  defp get_term(b) do
    <<s::size(8), b2::binary>> = b
    <<t::size(s)-binary, rest::binary>> = b2

    case :erlang.binary_to_term(t) do
      {:"$size", sz} ->
        <<t1::size(sz)-binary, rest1::binary>> = rest
        {:erlang.binary_to_term(t1), rest1}

      term ->
        {term, rest}
    end
  end

  defp display_warning(item, warning) do
    :io.format('Warning: {~tw,~tw}~n', [warning, item])
  end

  defp ip_to_file({:metadata, _, _}, {:shell_only, _} = state) do
    state
  end

  defp ip_to_file(trace, {:shell_only, fun} = state) do
    fun.(trace)
    state
  end

  defp ip_to_file(trace, {{:file, file}, shellOutput}) do
    fun = :dbg.trace_port(:file, file)
    port = fun.()
    p(port, :clear)
    send(:ttb, {:ip_to_file_trace_port, port, self()})

    receive do
      {:ttb, :ok} ->
        :ok
    end

    case trace do
      {:metadata, _, _} ->
        :ok

      ^trace ->
        show_trace(trace, shellOutput)
    end

    ip_to_file(trace, {port, shellOutput})
  end

  defp ip_to_file({:metadata, metaFile, metaData}, state) do
    {:ok, metaFd} =
      :file.open(
        metaFile,
        [:write, :raw, :append]
      )

    :file.write(metaFd, metaData)
    :file.close(metaFd)
    state
  end

  defp ip_to_file(trace, {port, shellOutput}) do
    show_trace(trace, shellOutput)
    b = :erlang.term_to_binary(trace)
    :erlang.port_command(port, b)
    {port, shellOutput}
  end

  defp show_trace(trace, fun) when is_function(fun) do
    fun.(trace)
  end

  defp show_trace(_, _) do
    :ok
  end

  def dump_ti(file) do
    {:ok, b} = :file.read_file(file)
    dump_ti(b, [])
  end

  defp dump_ti(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp dump_ti(b, acc) do
    {term, rest} = get_term(b)
    dump_ti(rest, [term | acc])
  end
end
