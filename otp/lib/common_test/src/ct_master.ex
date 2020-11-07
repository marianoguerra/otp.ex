defmodule :m_ct_master do
  use Bitwise
  require Record
  Record.defrecord(:r_event, :event, name: :undefined, node: :undefined, data: :undefined)

  Record.defrecord(:r_conn, :conn,
    handle: :undefined,
    targetref: :undefined,
    address: :undefined,
    callback: :undefined
  )

  Record.defrecord(:r_testspec, :testspec,
    spec_dir: :undefined,
    nodes: [],
    init: [],
    label: [],
    profile: [],
    logdir: ['.'],
    logopts: [],
    basic_html: [],
    esc_chars: [],
    verbosity: [],
    silent_connections: [],
    cover: [],
    cover_stop: [],
    config: [],
    userconfig: [],
    event_handler: [],
    ct_hooks: [],
    enable_builtin_hooks: true,
    release_shell: false,
    include: [],
    auto_compile: [],
    abort_if_missing_suites: [],
    stylesheet: [],
    multiply_timetraps: [],
    scale_timetraps: [],
    create_priv_dir: [],
    alias: [],
    tests: [],
    unknown: [],
    merge_tests: true
  )

  Record.defrecord(:r_cover, :cover,
    app: :none,
    local_only: false,
    level: :details,
    excl_mods: [],
    incl_mods: [],
    cross: [],
    src: []
  )

  Record.defrecord(:r_conn_log, :conn_log,
    header: true,
    client: :undefined,
    name: :undefined,
    address: :undefined,
    conn_pid: :undefined,
    action: :undefined,
    module: :undefined
  )

  Record.defrecord(:r_state, :state,
    node_ctrl_pids: [],
    logdirs: [],
    results: [],
    locks: [],
    blocked: []
  )

  def run_test(node, opts) do
    run_test([{node, opts}])
  end

  def run_test({node, opts}) do
    run_test([{node, opts}])
  end

  def run_test(nodeOptsList) when is_list(nodeOptsList) do
    start_master(nodeOptsList)
  end

  def run([tS | testSpecs], allowUserTerms, inclNodes, exclNodes)
      when is_list(tS) and is_list(inclNodes) and
             is_list(exclNodes) do
    case (try do
            :ct_testspec.collect_tests_from_file([tS], inclNodes, allowUserTerms)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, reason} ->
        [{:error, reason} | run(testSpecs, allowUserTerms, inclNodes, exclNodes)]

      tests ->
        runResult =
          :lists.map(
            fn {specs,
                tSRec =
                  r_testspec(
                    logdir: allLogDirs,
                    config: stdCfgFiles,
                    userconfig: userCfgFiles,
                    include: allIncludes,
                    init: allInitOpts,
                    event_handler: allEvHs
                  )} ->
              allCfgFiles = {stdCfgFiles, userCfgFiles}
              runSkipPerNode = :ct_testspec.prepare_tests(tSRec)

              runSkipPerNode2 =
                exclude_nodes(
                  exclNodes,
                  runSkipPerNode
                )

              tSList =
                cond do
                  is_integer(hd(tS)) ->
                    [tS]

                  true ->
                    tS
                end

              {specs,
               run_all(
                 runSkipPerNode2,
                 allLogDirs,
                 allCfgFiles,
                 allEvHs,
                 allIncludes,
                 [],
                 [],
                 allInitOpts,
                 tSList
               )}
            end,
            tests
          )

        runResult ++ run(testSpecs, allowUserTerms, inclNodes, exclNodes)
    end
  end

  def run([], _, _, _) do
    []
  end

  def run(tS, allowUserTerms, inclNodes, exclNodes)
      when is_list(inclNodes) and is_list(exclNodes) do
    run([tS], allowUserTerms, inclNodes, exclNodes)
  end

  def run(testSpecs, inclNodes, exclNodes) do
    run(testSpecs, false, inclNodes, exclNodes)
  end

  def run(testSpecs = [tS | _]) when is_list(tS) do
    run(testSpecs, false, [], [])
  end

  def run(tS) do
    run([tS], false, [], [])
  end

  defp exclude_nodes([exclNode | exNs], runSkipPerNode) do
    exclude_nodes(
      exNs,
      :lists.keydelete(exclNode, 1, runSkipPerNode)
    )
  end

  defp exclude_nodes([], runSkipPerNode) do
    runSkipPerNode
  end

  def run_on_node([tS | testSpecs], allowUserTerms, node)
      when is_list(tS) and is_atom(node) do
    case (try do
            :ct_testspec.collect_tests_from_file([tS], [node], allowUserTerms)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, reason} ->
        [{:error, reason} | run_on_node(testSpecs, allowUserTerms, node)]

      tests ->
        runResult =
          :lists.map(
            fn {specs,
                tSRec =
                  r_testspec(
                    logdir: allLogDirs,
                    config: stdCfgFiles,
                    init: allInitOpts,
                    include: allIncludes,
                    userconfig: userCfgFiles,
                    event_handler: allEvHs
                  )} ->
              allCfgFiles = {stdCfgFiles, userCfgFiles}

              {run, skip} =
                :ct_testspec.prepare_tests(
                  tSRec,
                  node
                )

              tSList =
                cond do
                  is_integer(hd(tS)) ->
                    [tS]

                  true ->
                    tS
                end

              {specs,
               run_all(
                 [{node, run, skip}],
                 allLogDirs,
                 allCfgFiles,
                 allEvHs,
                 allIncludes,
                 [],
                 [],
                 allInitOpts,
                 tSList
               )}
            end,
            tests
          )

        runResult ++ run_on_node(testSpecs, allowUserTerms, node)
    end
  end

  def run_on_node([], _, _) do
    []
  end

  def run_on_node(tS, allowUserTerms, node) when is_atom(node) do
    run_on_node([tS], allowUserTerms, node)
  end

  def run_on_node(testSpecs, node) do
    run_on_node(testSpecs, false, node)
  end

  defp run_all(
         [{node, run, skip} | rest],
         allLogDirs,
         {allStdCfgFiles, allUserCfgFiles} = allCfgFiles,
         allEvHs,
         allIncludes,
         nodeOpts,
         logDirs,
         initOptions,
         specs
       ) do
    logDir =
      :lists.foldl(
        fn
          {n, dir}, _Found when n == node ->
            dir

          {_N, _Dir}, found ->
            found

          dir, '.' ->
            dir

          _Dir, found ->
            found
        end,
        '.',
        allLogDirs
      )

    stdCfgFiles =
      :lists.foldr(
        fn
          {n, f}, fs when n == node ->
            [f | fs]

          {_N, _F}, fs ->
            fs

          f, fs ->
            [f | fs]
        end,
        [],
        allStdCfgFiles
      )

    userCfgFiles =
      :lists.foldr(
        fn
          {n, f}, fs when n == node ->
            [{:userconfig, f} | fs]

          {_N, _F}, fs ->
            fs

          f, fs ->
            [{:userconfig, f} | fs]
        end,
        [],
        allUserCfgFiles
      )

    includes =
      :lists.foldr(
        fn
          {n, i}, acc when n === node ->
            [i | acc]

          {_, _}, acc ->
            acc

          i, acc ->
            [i | acc]
        end,
        [],
        allIncludes
      )

    evHs =
      :lists.foldr(
        fn
          {n, h, a}, hs when n == node ->
            [{h, a} | hs]

          {_N, _H, _A}, hs ->
            hs

          {h, a}, hs ->
            [{h, a} | hs]
        end,
        [],
        allEvHs
      )

    nO =
      {node,
       [
         {:prepared_tests, {run, skip}, specs},
         {:logdir, logDir},
         {:include, includes},
         {:config, stdCfgFiles},
         {:event_handler, evHs}
       ] ++ userCfgFiles}

    run_all(
      rest,
      allLogDirs,
      allCfgFiles,
      allEvHs,
      allIncludes,
      [nO | nodeOpts],
      [logDir | logDirs],
      initOptions,
      specs
    )
  end

  defp run_all([], allLogDirs, _, allEvHs, _AllIncludes, nodeOpts, logDirs, initOptions, specs) do
    handlers =
      for {master, h, a} <- allEvHs,
          master == :master do
        {h, a}
      end

    masterLogDir =
      case :lists.keysearch(:master, 1, allLogDirs) do
        {:value, {_, dir}} ->
          dir

        false ->
          '.'
      end

    log(:tty, 'Master Logdir', '~ts', [masterLogDir])
    start_master(:lists.reverse(nodeOpts), handlers, masterLogDir, logDirs, initOptions, specs)
    :ok
  end

  def abort() do
    call(:abort)
  end

  def abort(nodes) when is_list(nodes) do
    call({:abort, nodes})
  end

  def abort(node) when is_atom(node) do
    abort([node])
  end

  def progress() do
    call(:progress)
  end

  def get_event_mgr_ref() do
    :ct_master_event
  end

  def basic_html(bool) do
    :application.set_env(:common_test_master, :basic_html, bool)
    :ok
  end

  def esc_chars(bool) do
    :application.set_env(:common_test_master, :esc_chars, bool)
    :ok
  end

  defp start_master(nodeOptsList) do
    start_master(nodeOptsList, [], '.', [], [], [])
  end

  defp start_master(nodeOptsList, evHandlers, masterLogDir, logDirs, initOptions, specs) do
    master =
      spawn_link(:ct_master, :init_master, [
        self(),
        nodeOptsList,
        evHandlers,
        masterLogDir,
        logDirs,
        initOptions,
        specs
      ])

    receive do
      {^master, result} ->
        result
    end
  end

  def init_master(parent, nodeOptsList, evHandlers, masterLogDir, logDirs, initOptions, specs) do
    case :erlang.whereis(:ct_master) do
      :undefined ->
        :erlang.register(:ct_master, self())
        :ct_util.mark_process()
        :ok

      _Pid ->
        :io.format('~nWarning: ct_master already running!~n')
        exit(:aborted)
    end

    {mLPid, _} =
      :ct_master_logs.start(
        masterLogDir,
        for {n, _} <- nodeOptsList do
          n
        end
      )

    log(:all, 'Master Logger process started', '~w', [mLPid])

    case specs do
      [] ->
        :ok

      _ ->
        specsStr =
          :lists.map(
            fn name ->
              name ++ ' '
            end,
            specs
          )

        :ct_master_logs.log('Test Specification file(s)', '~ts', [:lists.flatten(specsStr)])
    end

    {:ok, _} = start_ct_master_event()
    :ct_master_event.add_handler()

    add = fn {h, args} ->
      log(:all, 'Adding Event Handler', '~w', [h])

      case :gen_event.add_handler(:ct_master_event, h, args) do
        :ok ->
          :ok

        {:EXIT, why} ->
          exit(why)

        other ->
          exit({:event_handler, other})
      end
    end

    :lists.foreach(add, evHandlers)

    case :erlang.whereis(:ct_master_event) do
      :undefined ->
        exit({:ct_master_event, :undefined})

      pid when is_pid(pid) ->
        :ok
    end

    init_master1(parent, nodeOptsList, initOptions, logDirs)
  end

  defp start_ct_master_event() do
    case :ct_master_event.start_link() do
      {:error, {:already_started, pid}} ->
        {:ok, pid}

      else__ ->
        else__
    end
  end

  defp init_master1(parent, nodeOptsList, initOptions, logDirs) do
    {inaccessible, nodeOptsList1, initOptions1} = init_nodes(nodeOptsList, initOptions)

    case inaccessible do
      [] ->
        init_master2(parent, nodeOptsList, logDirs)

      _ ->
        :io.format('~nThe following nodes are inaccessible: ~p~n~n', [inaccessible])
        :io.format('Proceed(p), Rescan(r) or Abort(a)? ')

        case :io.get_line(:"[p/r/a]>") do
          'p\n' ->
            log(:html, 'Inaccessible Nodes', 'Proceeding without: ~p', [inaccessible])
            init_master2(parent, nodeOptsList1, logDirs)

          'r\n' ->
            init_master1(parent, nodeOptsList, initOptions1, logDirs)

          _ ->
            log(:html, 'Aborting Tests', '', [])
            :ct_master_event.stop()
            :ct_master_logs.stop()
            exit(:aborted)
        end
    end
  end

  defp init_master2(parent, nodeOptsList, logDirs) do
    :erlang.process_flag(:trap_exit, true)
    cookie = :erlang.get_cookie()
    log(:all, 'Cookie', '~tw', [cookie])

    log(:all, 'Starting Tests', 'Tests starting on: ~p', [
      for {n, _} <- nodeOptsList do
        n
      end
    ])

    spawnAndMon = fn {node, opts} ->
      :erlang.monitor_node(node, true)
      log(:all, 'Test Info', 'Starting test(s) on ~w...', [node])
      {:erlang.spawn_link(node, :ct_master, :init_node_ctrl, [self(), cookie, opts]), node}
    end

    nodeCtrlPids = :lists.map(spawnAndMon, nodeOptsList)

    result =
      master_loop(
        r_state(
          node_ctrl_pids: nodeCtrlPids,
          logdirs: logDirs
        )
      )

    send(parent, {self(), result})
  end

  defp master_loop(r_state(node_ctrl_pids: [], logdirs: logDirs, results: finished)) do
    str =
      :lists.map(
        fn {node, result} ->
          :io_lib.format(
            '~-40.40.*ts~tp\n',
            [?_, :erlang.atom_to_list(node), result]
          )
        end,
        :lists.reverse(finished)
      )

    log(:all, 'TEST RESULTS', '~ts', [str])
    log(:all, 'Info', 'Updating log files', [])
    refresh_logs(logDirs, [])
    :ct_master_event.stop()
    :ct_master_logs.stop()
    :ok
  end

  defp master_loop(
         state =
           r_state(node_ctrl_pids: nodeCtrlPids, results: results, locks: locks, blocked: blocked)
       ) do
    receive do
      {:EXIT, pid, reason} ->
        case get_node(pid, nodeCtrlPids) do
          {node, nodeCtrlPids1} ->
            :erlang.monitor_node(node, false)

            case reason do
              :normal ->
                log(:all, 'Test Info', 'Test(s) on node ~w finished.', [node])
                master_loop(r_state(state, node_ctrl_pids: nodeCtrlPids1))

              bad ->
                error =
                  case bad do
                    what when what !== :killed and is_atom(what) ->
                      {:error, bad}

                    _ ->
                      bad
                  end

                log(:all, 'Test Info', 'Test on node ~w failed! Reason: ~tp', [node, error])
                {locks1, blocked1} = update_queue(:exit, node, locks, blocked)

                master_loop(
                  r_state(state,
                    node_ctrl_pids: nodeCtrlPids1,
                    results: [{node, error} | results],
                    locks: locks1,
                    blocked: blocked1
                  )
                )
            end

          :undefined ->
            log(:all, 'Test Info', 'Warning! Process ~w has terminated. Reason: ~tp', [
              pid,
              reason
            ])

            master_loop(state)
        end

      {:nodedown, node} ->
        case get_pid(node, nodeCtrlPids) do
          {_Pid, nodeCtrlPids1} ->
            :erlang.monitor_node(node, false)
            log(:all, 'Test Info', 'No connection to testnode ~w!', [node])
            {locks1, blocked1} = update_queue(:exit, node, locks, blocked)

            master_loop(
              r_state(state,
                node_ctrl_pids: nodeCtrlPids1,
                results: [{node, :nodedown} | results],
                locks: locks1,
                blocked: blocked1
              )
            )

          :undefined ->
            master_loop(state)
        end

      {pid, {:result, result}} ->
        {node, _} = get_node(pid, nodeCtrlPids)

        master_loop(
          r_state(state,
            results: [
              {node, result}
              | results
            ]
          )
        )

      {:call, :progress, from} ->
        reply(master_progress(nodeCtrlPids, results), from)
        master_loop(state)

      {:call, :abort, from} ->
        :lists.foreach(
          fn {pid, node} ->
            log(:all, 'Test Info', 'Aborting tests on ~w', [node])
            :erlang.exit(pid, :kill)
          end,
          nodeCtrlPids
        )

        reply(:ok, from)
        master_loop(state)

      {:call, {:abort, nodes}, from} ->
        :lists.foreach(
          fn node ->
            case :lists.keysearch(node, 2, nodeCtrlPids) do
              {:value, {pid, ^node}} ->
                log(:all, 'Test Info', 'Aborting tests on ~w', [node])
                :erlang.exit(pid, :kill)

              false ->
                :ok
            end
          end,
          nodes
        )

        reply(:ok, from)
        master_loop(state)

      {:call, r_event(name: name, node: node, data: data), from} ->
        {op, lock} =
          case name do
            :start_make ->
              {:take, {:make, data}}

            :finished_make ->
              {:release, {:make, data}}

            :start_write_file ->
              {:take, {:write_file, data}}

            :finished_write_file ->
              {:release, {:write_file, data}}
          end

        {locks1, blocked1} = update_queue(op, node, from, lock, locks, blocked)

        cond do
          op == :release ->
            reply(:ok, from)

          true ->
            :ok
        end

        master_loop(r_state(state, locks: locks1, blocked: blocked1))

      {:cast, event} when elem(event, 0) === :event ->
        :ct_master_event.notify(event)
        master_loop(state)
    end
  end

  defp update_queue(:take, node, from, lock = {op, resource}, locks, blocked) do
    case :lists.keysearch(lock, 1, locks) do
      {:value, {_Lock, owner}} ->
        log(:html, 'Lock Info', 'Node ~w blocked on ~w by ~w. Resource: ~tp', [
          node,
          op,
          owner,
          resource
        ])

        blocked1 = blocked ++ [{lock, node, from}]
        {locks, blocked1}

      false ->
        locks1 = [{lock, node} | locks]
        reply(:ok, from)
        {locks1, blocked}
    end
  end

  defp update_queue(:release, node, _From, lock = {op, resource}, locks, blocked) do
    locks1 = :lists.delete({lock, node}, locks)

    case :lists.keysearch(lock, 1, blocked) do
      {:value, e = {^lock, someNode, waitingPid}} ->
        blocked1 = :lists.delete(e, blocked)

        log(:html, 'Lock Info', 'Node ~w proceeds with ~w. Resource: ~tp', [
          someNode,
          op,
          resource
        ])

        reply(:ok, waitingPid)
        {locks1, blocked1}

      false ->
        {locks1, blocked}
    end
  end

  defp update_queue(:exit, node, locks, blocked) do
    nodeLocks =
      :lists.foldl(
        fn
          {l, n}, ls when n == node ->
            [l | ls]

          _, ls ->
            ls
        end,
        [],
        locks
      )

    release_locks(node, nodeLocks, locks, blocked)
  end

  defp release_locks(node, [lock | ls], locks, blocked) do
    {locks1, blocked1} = update_queue(:release, node, :undefined, lock, locks, blocked)
    release_locks(node, ls, locks1, blocked1)
  end

  defp release_locks(_, [], locks, blocked) do
    {locks, blocked}
  end

  defp get_node(pid, nodeCtrlPids) do
    case :lists.keysearch(pid, 1, nodeCtrlPids) do
      {:value, {^pid, node}} ->
        {node, :lists.keydelete(pid, 1, nodeCtrlPids)}

      false ->
        :undefined
    end
  end

  defp get_pid(node, nodeCtrlPids) do
    case :lists.keysearch(node, 2, nodeCtrlPids) do
      {:value, {pid, ^node}} ->
        {pid, :lists.keydelete(node, 2, nodeCtrlPids)}

      false ->
        :undefined
    end
  end

  defp ping_nodes(nodeOptions) do
    ping_nodes(nodeOptions, [], [])
  end

  defp ping_nodes([nO = {node, _Opts} | nOs], inaccessible, nodeOpts) do
    case :net_adm.ping(node) do
      :pong ->
        ping_nodes(nOs, inaccessible, [nO | nodeOpts])

      _ ->
        ping_nodes(nOs, [node | inaccessible], nodeOpts)
    end
  end

  defp ping_nodes([], inaccessible, nodeOpts) do
    {:lists.reverse(inaccessible), :lists.reverse(nodeOpts)}
  end

  defp master_progress(nodeCtrlPids, results) do
    results ++
      :lists.map(
        fn {_Pid, node} ->
          {node, :ongoing}
        end,
        nodeCtrlPids
      )
  end

  defp refresh_logs([d | dirs], refreshed) do
    case :lists.member(d, dirs) do
      true ->
        case :lists.keymember(d, 1, refreshed) do
          true ->
            refresh_logs(dirs, refreshed)

          false ->
            {:ok, cwd} = :file.get_cwd()

            case (try do
                    :ct_run.refresh_logs(d)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              {:EXIT, reason} ->
                :ok = :file.set_cwd(cwd)
                refresh_logs(dirs, [{d, {:error, reason}} | refreshed])

              result ->
                refresh_logs(dirs, [{d, result} | refreshed])
            end
        end

      false ->
        refresh_logs(dirs, refreshed)
    end
  end

  defp refresh_logs([], refreshed) do
    str =
      :lists.map(
        fn {d, result} ->
          :io_lib.format('Refreshing logs in ~tp... ~tp', [d, result])
        end,
        refreshed
      )

    log(:all, 'Info', '~ts', [str])
  end

  def init_node_ctrl(masterPid, cookie, opts) do
    :erlang.process_flag(:trap_exit, true)
    :ct_util.mark_process()
    masterNode = node(masterPid)
    :erlang.group_leader(:erlang.whereis(:user), self())
    :io.format('~n********** node_ctrl process ~w started on ~w **********~n', [self(), node()])

    case :erlang.get_cookie() do
      ^cookie ->
        :erlang.set_cookie(node(masterPid), cookie)

      _ ->
        :ok
    end

    case :erlang.whereis(:ct_util_server) do
      :undefined ->
        :ok

      pid ->
        :erlang.exit(pid, :kill)
    end

    {:ok, _} = start_ct_event()
    :ct_event.add_handler([{:master, masterPid}])

    result =
      case (try do
              :ct.run_test(opts)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        :ok ->
          :finished_ok

        other ->
          other
      end

    :ct_event.stop()

    case :net_adm.ping(masterNode) do
      :pong ->
        send(masterPid, {self(), {:result, result}})

      :pang ->
        :io.format('Warning! Connection to master node ~w is lost. Can\'t report result!~n~n', [
          masterNode
        ])
    end
  end

  defp start_ct_event() do
    case :ct_event.start_link() do
      {:error, {:already_started, pid}} ->
        {:ok, pid}

      else__ ->
        else__
    end
  end

  def status(masterPid, event = r_event(name: :start_make)) do
    call(masterPid, event)
  end

  def status(masterPid, event = r_event(name: :finished_make)) do
    call(masterPid, event)
  end

  def status(masterPid, event = r_event(name: :start_write_file)) do
    call(masterPid, event)
  end

  def status(
        masterPid,
        event = r_event(name: :finished_write_file)
      ) do
    call(masterPid, event)
  end

  def status(masterPid, event) do
    cast(masterPid, event)
  end

  defp log(to, heading, str, args) do
    cond do
      to == :all or to == :tty ->
        chars = ['=== ', heading, ' ===\n', :io_lib.format(str, args), '\n']
        :io.put_chars(chars)

      true ->
        :ok
    end

    cond do
      to == :all or to == :html ->
        :ct_master_logs.log(heading, str, args)

      true ->
        :ok
    end
  end

  defp call(msg) do
    call(:erlang.whereis(:ct_master), msg)
  end

  defp call(:undefined, _Msg) do
    {:error, :not_running}
  end

  defp call(pid, msg) do
    ref = :erlang.monitor(:process, pid)
    send(pid, {:call, msg, self()})

    return =
      receive do
        {^pid, result} ->
          result

        {:DOWN, ^ref, _, _, _} ->
          {:error, :master_died}
      end

    :erlang.demonitor(ref, [:flush])
    return
  end

  defp reply(result, to) do
    send(to, {self(), result})
    :ok
  end

  defp init_nodes(nodeOptions, initOptions) do
    _ = ping_nodes(nodeOptions)
    start_nodes(initOptions)
    eval_on_nodes(initOptions)
    {inaccessible, nodeOptions1} = ping_nodes(nodeOptions)

    initOptions1 =
      filter_accessible(
        initOptions,
        inaccessible
      )

    {inaccessible, nodeOptions1, initOptions1}
  end

  defp filter_accessible(initOptions, inaccessible) do
    for {node, option} <- initOptions,
        :lists.member(node, inaccessible) do
      {node, option}
    end
  end

  defp start_nodes(initOptions) do
    :lists.foreach(
      fn {nodeName, options} ->
        [nodeS, hostS] =
          :string.lexemes(
            :erlang.atom_to_list(nodeName),
            '@'
          )

        node = :erlang.list_to_atom(nodeS)
        host = :erlang.list_to_atom(hostS)
        hasNodeStart = :lists.keymember(:node_start, 1, options)
        isAlive = :lists.member(nodeName, :erlang.nodes())

        case {hasNodeStart, isAlive} do
          {false, false} ->
            :io.format('WARNING: Node ~w is not alive but has no node_start option~n', [nodeName])

          {false, true} ->
            :io.format('Node ~w is alive~n', [nodeName])

          {true, false} ->
            {:node_start, nodeStart} = :lists.keyfind(:node_start, 1, options)

            {:value, {:callback_module, callback}, nodeStart2} =
              :lists.keytake(:callback_module, 1, nodeStart)

            case callback.start(host, node, nodeStart2) do
              {:ok, ^nodeName} ->
                :io.format('Node ~w started successfully with callback ~w~n', [nodeName, callback])

              {:error, reason, _NodeName} ->
                :io.format('Failed to start node ~w with callback ~w! Reason: ~tp~n', [
                  nodeName,
                  callback,
                  reason
                ])
            end

          {true, true} ->
            :io.format('WARNING: Node ~w is alive but has node_start option~n', [nodeName])
        end
      end,
      initOptions
    )
  end

  defp eval_on_nodes(initOptions) do
    :lists.foreach(
      fn {nodeName, options} ->
        hasEval = :lists.keymember(:eval, 1, options)
        isAlive = :lists.member(nodeName, :erlang.nodes())

        case {hasEval, isAlive} do
          {false, _} ->
            :ok

          {true, false} ->
            :io.format('WARNING: Node ~w is not alive but has eval option~n', [nodeName])

          {true, true} ->
            {:eval, mFAs} = :lists.keyfind(:eval, 1, options)
            evaluate(nodeName, mFAs)
        end
      end,
      initOptions
    )
  end

  defp evaluate(node, [{m, f, a} | mFAs]) do
    case :rpc.call(node, m, f, a) do
      {:badrpc, reason} ->
        :io.format('WARNING: Failed to call ~w:~tw/~w on node ~w due to ~tp~n', [
          m,
          f,
          length(a),
          node,
          reason
        ])

      result ->
        :io.format('Called ~w:~tw/~w on node ~w, result: ~tp~n', [m, f, length(a), node, result])
    end

    evaluate(node, mFAs)
  end

  defp evaluate(_Node, []) do
    :ok
  end

  defp cast(:undefined, _Msg) do
    {:error, :not_running}
  end

  defp cast(pid, msg) do
    send(pid, {:cast, msg})
    :ok
  end
end
