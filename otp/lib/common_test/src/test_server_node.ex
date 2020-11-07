defmodule :m_test_server_node do
  use Bitwise
  require Record

  Record.defrecord(:r_target_info, :target_info,
    os_family: :undefined,
    os_type: :undefined,
    host: :undefined,
    version: :undefined,
    system_version: :undefined,
    root_dir: :undefined,
    emulator: :undefined,
    otp_release: :undefined,
    username: :undefined,
    cookie: :undefined,
    naming: :undefined,
    master: :undefined
  )

  Record.defrecord(:r_par, :par,
    type: :undefined,
    target: :undefined,
    naming: :undefined,
    master: :undefined,
    cookie: :undefined
  )

  Record.defrecord(:r_cover, :cover,
    app: :undefined,
    file: :undefined,
    incl: :undefined,
    excl: :undefined,
    level: :undefined,
    mods: :undefined,
    stop: true,
    cross: :undefined
  )

  Record.defrecord(:r_slave_info, :slave_info,
    name: :undefined,
    socket: :undefined,
    client: :undefined
  )

  def is_release_available(rel) when is_atom(rel) do
    is_release_available(:erlang.atom_to_list(rel))
  end

  def is_release_available(rel) do
    case :os.type() do
      {:unix, _} ->
        erl = find_release(rel)

        case erl do
          :none ->
            false

          _ ->
            :filelib.is_regular(erl)
        end

      _ ->
        false
    end
  end

  def nodedown(sock) do
    match = r_slave_info(name: :"$1", socket: sock, client: :"$2", _: :_)

    case :ets.match(:slave_tab, match) do
      [[node, _Client]] ->
        :gen_tcp.close(sock)
        :ets.delete(:slave_tab, node)
        :slave_died

      [] ->
        :ok
    end
  end

  def start_tracer_node(traceFile, tI) do
    match = r_slave_info(name: :"$1", _: :_)

    slaveNodes =
      :lists.map(
        fn [n] ->
          [' ', n]
        end,
        :ets.match(:slave_tab, match)
      )

    targetNode = node()
    cookie = r_target_info(tI, :cookie)

    {:ok, lSock} =
      :gen_tcp.listen(
        0,
        [:binary, {:reuseaddr, true}, {:packet, 2}]
      )

    {:ok, tracePort} = :inet.port(lSock)
    prog = quote_progname(pick_erl_program(:default))

    cmd =
      :lists.concat([
        prog,
        ' -sname tracer -hidden -setcookie ',
        cookie,
        ' -s ',
        :test_server_node,
        ' trc ',
        traceFile,
        ' ',
        tracePort,
        ' ',
        r_target_info(tI, :os_family)
      ])

    spawn(fn ->
      print_data(:erlang.open_port({:spawn, cmd}, [:stream]))
    end)

    case :gen_tcp.accept(lSock, 20000) do
      {:ok, sock} ->
        :gen_tcp.close(lSock)

        receive do
          {:tcp, ^sock, result} when is_binary(result) ->
            case unpack(result) do
              :error ->
                :gen_tcp.close(sock)
                {:error, :timeout}

              {:ok, :started} ->
                trace_nodes(sock, [targetNode | slaveNodes])
                {:ok, sock}

              {:ok, error} ->
                error
            end

          {:tcp_closed, ^sock} ->
            :gen_tcp.close(sock)
            {:error, :could_not_start_tracernode}
        after
          20000 ->
            :gen_tcp.close(sock)
            {:error, :timeout}
        end

      error ->
        :gen_tcp.close(lSock)
        {:error, {:could_not_start_tracernode, error}}
    end
  end

  def trace_nodes(sock, nodes) do
    bin = :erlang.term_to_binary({:add_nodes, nodes})
    :ok = :gen_tcp.send(sock, tag_trace_message(bin))
    receive_ack(sock)
  end

  defp receive_ack(sock) do
    receive do
      {:tcp, ^sock, bin} when is_binary(bin) ->
        case unpack(bin) do
          :error ->
            receive_ack(sock)

          {:ok, _} ->
            :ok
        end

      _ ->
        receive_ack(sock)
    end
  end

  def stop_tracer_node(sock) do
    bin = :erlang.term_to_binary(id(:stop))
    :ok = :gen_tcp.send(sock, tag_trace_message(bin))

    receive do
      {:tcp_closed, ^sock} ->
        :gen_tcp.close(sock)
    end

    :ok
  end

  def trc([traceFile, portAtom, type]) do
    {result, patterns} =
      case :file.consult(traceFile) do
        {:ok, tI} ->
          pat = parse_trace_info(:lists.flatten(tI))
          {:started, pat}

        error ->
          {error, []}
      end

    port = :erlang.list_to_integer(:erlang.atom_to_list(portAtom))

    case (try do
            :gen_tcp.connect('localhost', port, [:binary, {:reuseaddr, true}, {:packet, 2}])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, sock} ->
        binResult = :erlang.term_to_binary(result)
        :ok = :gen_tcp.send(sock, tag_trace_message(binResult))
        trc_loop(sock, patterns, type)

      _else ->
        :ok
    end

    :erlang.halt()
  end

  defp trc_loop(sock, patterns, type) do
    receive do
      {:tcp, ^sock, bin} ->
        case unpack(bin) do
          :error ->
            :ttb.stop()
            :gen_tcp.close(sock)

          {:ok, {:add_nodes, nodes}} ->
            add_nodes(nodes, patterns, type)
            ^bin = :erlang.term_to_binary(id(:ok))
            :ok = :gen_tcp.send(sock, tag_trace_message(bin))
            trc_loop(sock, patterns, type)

          {:ok, :stop} ->
            :ttb.stop()
            :gen_tcp.close(sock)
        end

      {:tcp_closed, ^sock} ->
        :ttb.stop()
        :gen_tcp.close(sock)
    end
  end

  defp add_nodes(nodes, patterns, _Type) do
    {:ok, _} =
      :ttb.tracer(
        nodes,
        [
          {:file, {:local, :test_server}},
          {:handler, {{:test_server_node, :handle_debug}, :initial}}
        ]
      )

    {:ok, _} = :ttb.p(:all, [:call, :timestamp])

    :lists.foreach(
      fn
        {tP, m, f, a, pat} ->
          apply(:ttb, tP, [m, f, a, pat])

        {cTP, m, f, a} ->
          apply(:ttb, cTP, [m, f, a])
      end,
      patterns
    )
  end

  defp parse_trace_info([{tP, m, pat} | pats])
       when tP === :tp or
              tP === :tpl do
    [{tP, m, :_, :_, pat} | parse_trace_info(pats)]
  end

  defp parse_trace_info([{tP, m, f, pat} | pats])
       when tP === :tp or
              tP === :tpl do
    [{tP, m, f, :_, pat} | parse_trace_info(pats)]
  end

  defp parse_trace_info([{tP, m, f, a, pat} | pats])
       when tP === :tp or
              tP === :tpl do
    [{tP, m, f, a, pat} | parse_trace_info(pats)]
  end

  defp parse_trace_info([cTP | pats])
       when cTP === :ctp or
              cTP === :ctpl or cTP === :ctpg do
    [{cTP, :_, :_, :_} | parse_trace_info(pats)]
  end

  defp parse_trace_info([{cTP, m} | pats])
       when cTP === :ctp or
              cTP === :ctpl or cTP === :ctpg do
    [{cTP, m, :_, :_} | parse_trace_info(pats)]
  end

  defp parse_trace_info([{cTP, m, f} | pats])
       when cTP === :ctp or
              cTP === :ctpl or cTP === :ctpg do
    [{cTP, m, f, :_} | parse_trace_info(pats)]
  end

  defp parse_trace_info([{cTP, m, f, a} | pats])
       when cTP === :ctp or
              cTP === :ctpl or cTP === :ctpg do
    [{cTP, m, f, a} | parse_trace_info(pats)]
  end

  defp parse_trace_info([]) do
    []
  end

  defp parse_trace_info([_other | pats]) do
    parse_trace_info(pats)
  end

  def handle_debug(out, trace, tI, :initial) do
    handle_debug(out, trace, tI, 0)
  end

  def handle_debug(_Out, :end_of_trace, _TI, n) do
    n
  end

  def handle_debug(out, trace, _TI, n) do
    print_trc(out, trace, n)
    n + 1
  end

  defp print_trc(out, {:trace_ts, p, :call, {m, f, a}, c, ts}, n) do
    :io.format(
      out,
      '~w: ~s~nProcess   : ~w~nCall      : ~w:~tw/~w~nArguments : ~tp~nCaller    : ~tw~n~n',
      [n, ts(ts), p, m, f, length(a), a, c]
    )
  end

  defp print_trc(out, {:trace_ts, p, :call, {m, f, a}, ts}, n) do
    :io.format(out, '~w: ~s~nProcess   : ~w~nCall      : ~w:~tw/~w~nArguments : ~tp~n~n', [
      n,
      ts(ts),
      p,
      m,
      f,
      length(a),
      a
    ])
  end

  defp print_trc(out, {:trace_ts, p, :return_from, {m, f, a}, r, ts}, n) do
    :io.format(
      out,
      '~w: ~s~nProcess      : ~w~nReturn from  : ~w:~tw/~w~nReturn value : ~tp~n~n',
      [n, ts(ts), p, m, f, a, r]
    )
  end

  defp print_trc(out, {:drop, x}, n) do
    :io.format(out, '~w: Tracer dropped ~w messages - too busy~n~n', [n, x])
  end

  defp print_trc(out, trace, n) do
    ts = :erlang.element(:erlang.size(trace), trace)
    :io.format(out, '~w: ~s~nTrace        : ~tp~n~n', [n, ts(ts), trace])
  end

  defp ts({_, _, micro} = now) do
    {{y, m, d}, {h, min, s}} = :calendar.now_to_local_time(now)

    :io_lib.format('~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w,~6.6.0w', [
      y,
      m,
      d,
      h,
      min,
      s,
      micro
    ])
  end

  def start_node(slaveName, :slave, options, from, tI)
      when is_list(slaveName) do
    start_node_slave(:erlang.list_to_atom(slaveName), options, from, tI)
  end

  def start_node(slaveName, :slave, options, from, tI) do
    start_node_slave(slaveName, options, from, tI)
  end

  def start_node(slaveName, :peer, options, from, tI)
      when is_atom(slaveName) do
    start_node_peer(:erlang.atom_to_list(slaveName), options, from, tI)
  end

  def start_node(slaveName, :peer, options, from, tI) do
    start_node_peer(slaveName, options, from, tI)
  end

  def start_node(_SlaveName, _Type, _Options, _From, _TI) do
    :not_implemented_yet
  end

  defp start_node_peer(slaveName, optList, from, tI) do
    suppliedArgs = start_node_get_option_value(:args, optList, [])
    cleanup = start_node_get_option_value(:cleanup, optList, true)
    hostStr = :test_server_sup.hoststr()

    {:ok, lSock} =
      :gen_tcp.listen(
        0,
        [:binary, {:reuseaddr, true}, {:packet, 2}]
      )

    {:ok, waitPort} = :inet.port(lSock)

    nodeStarted =
      :lists.concat([' -s ', :test_server_node, ' node_started ', hostStr, ' ', waitPort])

    crashDir = :test_server_sup.crash_dump_dir()
    crashFile = :filename.join([crashDir, 'erl_crash_dump.' ++ cast_to_list(slaveName)])
    crashArgs = :lists.concat([' -env ERL_CRASH_DUMP "', crashFile, '" '])
    failOnError = start_node_get_option_value(:fail_on_error, optList, true)
    prog0 = start_node_get_option_value(:erl, optList, :default)
    prog = quote_progname(pick_erl_program(prog0))

    args =
      case :string.find(suppliedArgs, '-setcookie') do
        :nomatch ->
          '-setcookie ' ++ r_target_info(tI, :cookie) ++ ' ' ++ suppliedArgs

        _ ->
          suppliedArgs
      end

    cmd =
      :lists.concat([
        prog,
        ' -detached ',
        r_target_info(tI, :naming),
        ' ',
        slaveName,
        nodeStarted,
        crashArgs,
        ' ',
        args
      ])

    opts =
      case start_node_get_option_value(:env, optList, []) do
        [] ->
          []

        env ->
          [{:env, env}]
      end

    try do
      :erlang.open_port({:spawn, cmd}, [:stream | opts])
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    tmo = 60000 * :test_server.timetrap_scale_factor()

    case start_node_get_option_value(:wait, optList, true) do
      true ->
        ret = wait_for_node_started(lSock, tmo, :undefined, cleanup, tI, self())

        case {ret, failOnError} do
          {{{:ok, node}, warning}, _} ->
            :gen_server.reply(
              from,
              {{:ok, node}, hostStr, cmd, [], warning}
            )

          {_, false} ->
            :gen_server.reply(from, {ret, hostStr, cmd})

          {_, true} ->
            :gen_server.reply(from, {:fail, {ret, hostStr, cmd}})
        end

      false ->
        nodename = :erlang.list_to_atom(slaveName ++ '@' ++ hostStr)
        i = '=== Not waiting for node'

        :gen_server.reply(
          from,
          {{:ok, nodename}, hostStr, cmd, i, []}
        )

        self = self()
        spawn_link(wait_for_node_started_fun(lSock, tmo, cleanup, tI, self))
        :ok
    end
  end

  defp wait_for_node_started_fun(lSock, tmo, cleanup, tI, self) do
    fn ->
      {{:ok, _}, _} = wait_for_node_started(lSock, tmo, :undefined, cleanup, tI, self)

      receive do
      after
        :infinity ->
          :ok
      end
    end
  end

  defp start_node_slave(slaveName, optList, from, _TI) do
    suppliedArgs = start_node_get_option_value(:args, optList, [])
    cleanup = start_node_get_option_value(:cleanup, optList, true)
    crashDir = :test_server_sup.crash_dump_dir()
    crashFile = :filename.join([crashDir, 'erl_crash_dump.' ++ cast_to_list(slaveName)])
    crashArgs = :lists.concat([' -env ERL_CRASH_DUMP "', crashFile, '" '])
    args = :lists.concat([' ', suppliedArgs, crashArgs])
    prog0 = start_node_get_option_value(:erl, optList, :default)
    prog = pick_erl_program(prog0)

    ret =
      case start_which_node(optList) do
        {:error, reason} ->
          {{:error, reason}, :undefined, :undefined}

        host0 ->
          do_start_node_slave(host0, slaveName, args, prog, cleanup)
      end

    :gen_server.reply(from, ret)
  end

  defp do_start_node_slave(host0, slaveName, args, prog, cleanup) do
    host =
      case host0 do
        :local ->
          :test_server_sup.hoststr()

        _ ->
          cast_to_list(host0)
      end

    cmd = prog ++ ' ' ++ args

    case :slave.start(host, slaveName, args, :no_link, prog) do
      {:ok, nodename} ->
        case cleanup do
          true ->
            :ets.insert(:slave_tab, r_slave_info(name: nodename))

          false ->
            :ok
        end

        {{:ok, nodename}, host, cmd, [], []}

      ret ->
        {ret, host, cmd}
    end
  end

  defp wait_for_node_started(lSock, timeout, client, cleanup, tI, ctrlPid) do
    case :gen_tcp.accept(lSock, timeout) do
      {:ok, sock} ->
        :gen_tcp.close(lSock)

        receive do
          {:tcp, ^sock, started0} when is_binary(started0) ->
            case unpack(started0) do
              :error ->
                :gen_tcp.close(sock)
                {:error, :connection_closed}

              {:ok, started} ->
                version = r_target_info(tI, :otp_release)
                vsnStr = r_target_info(tI, :system_version)
                {:ok, nodename, w} = handle_start_node_return(version, vsnStr, started)

                case cleanup do
                  true ->
                    :ets.insert(
                      :slave_tab,
                      r_slave_info(name: nodename, socket: sock, client: client)
                    )

                  false ->
                    :ok
                end

                :ok = :gen_tcp.controlling_process(sock, ctrlPid)
                :test_server_ctrl.node_started(nodename)
                {{:ok, nodename}, w}
            end

          {:tcp_closed, ^sock} ->
            :gen_tcp.close(sock)
            {:error, :connection_closed}
        after
          timeout ->
            :gen_tcp.close(sock)
            {:error, :timeout}
        end

      {:error, reason} ->
        :gen_tcp.close(lSock)
        {:error, {:no_connection, reason}}
    end
  end

  defp handle_start_node_return(version, vsnStr, {:started, node, version, vsnStr}) do
    {:ok, node, []}
  end

  defp handle_start_node_return(version, vsnStr, {:started, node, oVersion, oVsnStr}) do
    str =
      :io_lib.format(
        'WARNING: Started node reports different system version than current node! Current node version: ~p, ~p Started node version: ~p, ~p',
        [version, vsnStr, oVersion, oVsnStr]
      )

    str1 = :lists.flatten(str)
    {:ok, node, str1}
  end

  def node_started([host, portAtom]) do
    spawn(node_started_fun(host, portAtom))
  end

  defp node_started_fun(host, portAtom) do
    fn ->
      node_started(host, portAtom)
    end
  end

  defp node_started(host, portAtom) do
    {_, version} = :init.script_id()
    vsnStr = :erlang.system_info(:system_version)
    port = :erlang.list_to_integer(:erlang.atom_to_list(portAtom))

    case (try do
            :gen_tcp.connect(host, port, [:binary, {:reuseaddr, true}, {:packet, 2}])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, sock} ->
        started = :erlang.term_to_binary({:started, node(), version, vsnStr})
        :ok = :gen_tcp.send(sock, tag_trace_message(started))

        receive do
          _Anyting ->
            :gen_tcp.close(sock)
            :erlang.halt()
        end

      _else ->
        :erlang.halt()
    end
  end

  defp tag_trace_message(m) do
    [1 | m]
  end

  defp start_which_node(optlist) do
    case start_node_get_option_value(:remote, optlist) do
      :undefined ->
        :local

      true ->
        case find_remote_host() do
          {:error, other} ->
            {:error, other}

          rHost ->
            rHost
        end
    end
  end

  defp find_remote_host() do
    hostList = :test_server_ctrl.get_hosts()

    case :lists.delete(
           :test_server_sup.hoststr(),
           hostList
         ) do
      [] ->
        {:error, :no_remote_hosts}

      [rHost | _Rest] ->
        rHost
    end
  end

  defp start_node_get_option_value(key, list) do
    start_node_get_option_value(key, list, :undefined)
  end

  defp start_node_get_option_value(key, list, default) do
    case :lists.keysearch(key, 1, list) do
      {:value, {^key, value}} ->
        value

      false ->
        default
    end
  end

  def stop_node(name) do
    case :ets.lookup(:slave_tab, name) do
      [r_slave_info()] ->
        :ets.delete(:slave_tab, name)
        :ok

      [] ->
        {:error, :not_a_slavenode}
    end
  end

  def kill_nodes() do
    case :ets.match_object(:slave_tab, :_) do
      [] ->
        []

      list ->
        :lists.map(
          fn sI ->
            kill_node(sI)
          end,
          list
        )
    end
  end

  defp kill_node(sI) do
    name = r_slave_info(sI, :name)
    :ets.delete(:slave_tab, name)

    case r_slave_info(sI, :socket) do
      :undefined ->
        try do
          :rpc.call(name, :erlang, :halt, [])
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

      sock ->
        :gen_tcp.close(sock)
    end

    name
  end

  defp cast_to_list(x) when is_list(x) do
    x
  end

  defp cast_to_list(x) when is_atom(x) do
    :erlang.atom_to_list(x)
  end

  defp cast_to_list(x) do
    :lists.flatten(:io_lib.format('~tw', [x]))
  end

  defp pick_erl_program(:default) do
    :ct.get_progname()
  end

  defp pick_erl_program(l) do
    p = random_element(l)

    case p do
      {:prog, s} ->
        s

      {:release, s} ->
        clear_erl_aflags()
        find_release(s)

      :this ->
        :ct.get_progname()
    end
  end

  defp clear_erl_aflags() do
    :os.unsetenv('ERL_AFLAGS')
  end

  defp quote_progname(progname) do
    do_quote_progname(:string.lexemes(progname, ' '))
  end

  defp do_quote_progname([prog]) do
    '"' ++ prog ++ '"'
  end

  defp do_quote_progname([[prog, arg] | args]) do
    case :os.find_executable(prog) do
      false ->
        do_quote_progname([prog ++ ' ' ++ arg | args])

      _ ->
        '"' ++
          prog ++
          '"' ++
          :lists.flatten(
            :lists.map(
              fn x ->
                [' ', x]
              end,
              [arg | args]
            )
          )
    end
  end

  defp random_element(l) do
    :lists.nth(:rand.uniform(length(l)), l)
  end

  defp find_release(:latest) do
    '/usr/local/otp/releases/latest/bin/erl'
  end

  defp find_release(:previous) do
    'kaka'
  end

  defp find_release(rel) do
    find_release(:os.type(), rel)
  end

  defp find_release({:unix, :sunos}, rel) do
    case :os.cmd('uname -p') do
      'sparc' ++ _ ->
        '/usr/local/otp/releases/otp_beam_solaris8_' ++ rel ++ '/bin/erl'

      _ ->
        :none
    end
  end

  defp find_release({:unix, :linux}, rel) do
    candidates = find_rel_linux(rel)

    case :lists.dropwhile(
           fn n ->
             not :filelib.is_regular(n)
           end,
           candidates
         ) do
      [] ->
        :none

      [erl | _] ->
        erl
    end
  end

  defp find_release(_, _) do
    :none
  end

  defp find_rel_linux(rel) do
    try do
      case ubuntu_release() do
        :none ->
          :none

        [ubuntuRel | _] ->
          throw(find_rel_ubuntu(rel, ubuntuRel))
      end

      case suse_release() do
        :none ->
          :none

        suseRel ->
          throw(find_rel_suse(rel, suseRel))
      end

      []
    catch
      result ->
        result
    end
  end

  defp find_rel_suse(rel, suseRel) do
    root = '/usr/local/otp/releases/sles'

    case suseRel do
      '11' ->
        find_rel_suse_1(
          rel,
          root ++ '11'
        ) ++
          find_rel_suse_1(
            rel,
            root ++ '10'
          ) ++
          find_rel_suse_1(
            rel,
            root ++ '9'
          )

      '10' ->
        find_rel_suse_1(
          rel,
          root ++ '10'
        ) ++ find_rel_suse_1(rel, root ++ '9')

      '9' ->
        find_rel_suse_1(rel, root ++ '9')

      _ ->
        []
    end
  end

  defp find_rel_suse_1(rel, rootWc) do
    case :erlang.system_info(:wordsize) do
      4 ->
        find_rel_suse_2(rel, rootWc ++ '_32')

      8 ->
        find_rel_suse_2(
          rel,
          rootWc ++ '_64'
        ) ++ find_rel_suse_2(rel, rootWc ++ '_32')
    end
  end

  defp find_rel_suse_2(rel, rootWc) do
    relDir = :filename.dirname(rootWc)
    pat = :filename.basename(rootWc ++ '_' ++ rel) ++ '.*'

    case :file.list_dir(relDir) do
      {:ok, dirs} ->
        case :lists.filter(
               fn dir ->
                 case :re.run(dir, pat, [:unicode]) do
                   :nomatch ->
                     false

                   _ ->
                     true
                 end
               end,
               dirs
             ) do
          [] ->
            []

          [r | _] ->
            [:filename.join([relDir, r, 'bin', 'erl'])]
        end

      _ ->
        []
    end
  end

  defp suse_release() do
    case :file.open('/etc/SuSE-release', [:read]) do
      {:ok, fd} ->
        try do
          suse_release(fd)
        after
          :file.close(fd)
        end

      {:error, _} ->
        :none
    end
  end

  defp suse_release(fd) do
    case :io.get_line(fd, :"") do
      :eof ->
        :none

      line when is_list(line) ->
        case :re.run(line, '^VERSION\\s*=\\s*(\\d+) *', [{:capture, :all_but_first, :list}]) do
          :nomatch ->
            suse_release(fd)

          {:match, [version]} ->
            version
        end
    end
  end

  defp find_rel_ubuntu(_Rel, ubuntuRel)
       when is_integer(ubuntuRel) and
              ubuntuRel < 16 do
    []
  end

  defp find_rel_ubuntu(rel, ubuntuRel) when is_integer(ubuntuRel) do
    root = '/usr/local/otp/releases/ubuntu'

    :lists.foldl(
      fn chkUbuntuRel, acc ->
        find_rel_ubuntu_aux1(
          rel,
          root ++ :erlang.integer_to_list(chkUbuntuRel)
        ) ++ acc
      end,
      [],
      :lists.seq(16, ubuntuRel)
    )
  end

  defp find_rel_ubuntu_aux1(rel, rootWc) do
    case :erlang.system_info(:wordsize) do
      4 ->
        find_rel_ubuntu_aux2(rel, rootWc ++ '_32')

      8 ->
        find_rel_ubuntu_aux2(
          rel,
          rootWc ++ '_64'
        ) ++
          find_rel_ubuntu_aux2(
            rel,
            rootWc ++ '_32'
          )
    end
  end

  defp find_rel_ubuntu_aux2(rel, rootWc) do
    relDir = :filename.dirname(rootWc)
    pat = :filename.basename(rootWc ++ '_' ++ rel) ++ '.*'

    case :file.list_dir(relDir) do
      {:ok, dirs} ->
        case :lists.filter(
               fn dir ->
                 case :re.run(dir, pat, [:unicode]) do
                   :nomatch ->
                     false

                   _ ->
                     true
                 end
               end,
               dirs
             ) do
          [] ->
            []

          [r | _] ->
            [:filename.join([relDir, r, 'bin', 'erl'])]
        end

      _ ->
        []
    end
  end

  defp ubuntu_release() do
    case :file.open('/etc/lsb-release', [:read]) do
      {:ok, fd} ->
        try do
          ubuntu_release(fd, :undefined, :undefined)
        after
          :file.close(fd)
        end

      {:error, _} ->
        :none
    end
  end

  defp ubuntu_release(_Fd, distrId, rel)
       when distrId != :undefined and rel != :undefined do
    ubuntu =
      case distrId do
        'Ubuntu' ->
          true

        'ubuntu' ->
          true

        _ ->
          false
      end

    case ubuntu do
      false ->
        :none

      true ->
        rel
    end
  end

  defp ubuntu_release(fd, distroId, rel) do
    case :io.get_line(fd, :"") do
      :eof ->
        :none

      line when is_list(line) ->
        case :re.run(line, '^DISTRIB_ID=(\\w+)$', [{:capture, :all_but_first, :list}]) do
          {:match, [newDistroId]} ->
            ubuntu_release(fd, newDistroId, rel)

          :nomatch ->
            case :re.run(line, '^DISTRIB_RELEASE=(\\d+(?:\\.\\d+)*)$', [
                   {:capture, :all_but_first, :list}
                 ]) do
              {:match, [relList]} ->
                newRel =
                  :lists.map(
                    fn n ->
                      :erlang.list_to_integer(n)
                    end,
                    :string.lexemes(relList, '.')
                  )

                ubuntu_release(fd, distroId, newRel)

              :nomatch ->
                ubuntu_release(fd, distroId, rel)
            end
        end
    end
  end

  defp unpack(bin) do
    {one, term} = :erlang.split_binary(bin, 1)

    case :erlang.binary_to_list(one) do
      [1] ->
        case (try do
                {:ok, :erlang.binary_to_term(term)}
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            :error

          {:ok, _} = res ->
            res
        end

      _ ->
        :error
    end
  end

  defp id(i) do
    i
  end

  defp print_data(port) do
    :ct_util.mark_process()

    receive do
      {^port, {:data, bytes}} ->
        :io.put_chars(bytes)
        print_data(port)

      {^port, :eof} ->
        send(port, {self(), :close})

        receive do
          {^port, :closed} ->
            true
        end

        receive do
          {:EXIT, ^port, _} ->
            :ok
        after
          1 ->
            :ok
        end
    end
  end
end
