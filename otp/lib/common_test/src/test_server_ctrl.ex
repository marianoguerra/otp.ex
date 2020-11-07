defmodule :m_test_server_ctrl do
  use Bitwise
  import Kernel, except: [to_string: 1]
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

  Record.defrecord(:r_state, :state,
    jobs: [],
    levels: {1, 19, 10},
    reject_io_reqs: false,
    multiply_timetraps: 1,
    scale_timetraps: true,
    create_priv_dir: :auto_per_run,
    finish: false,
    target_info: :undefined,
    trc: false,
    cover: false,
    wait_for_node: [],
    testcase_callback: :undefined,
    idle_notify: [],
    get_totals: false,
    random_seed: :undefined
  )

  def add_dir(name, job = [dir | _Dirs]) when is_list(dir) do
    add_job(
      cast_to_list(name),
      :lists.map(
        fn d ->
          {:dir, cast_to_list(d)}
        end,
        job
      )
    )
  end

  def add_dir(name, dir) do
    add_job(cast_to_list(name), {:dir, cast_to_list(dir)})
  end

  def add_dir(name, job = [dir | _Dirs], pattern)
      when is_list(dir) do
    add_job(
      cast_to_list(name),
      :lists.map(
        fn d ->
          {:dir, cast_to_list(d), cast_to_list(pattern)}
        end,
        job
      )
    )
  end

  def add_dir(name, dir, pattern) do
    add_job(
      cast_to_list(name),
      {:dir, cast_to_list(dir), cast_to_list(pattern)}
    )
  end

  def add_module(mod) when is_atom(mod) do
    add_job(:erlang.atom_to_list(mod), {mod, :all})
  end

  def add_module(name, mods) when is_list(mods) do
    add_job(
      cast_to_list(name),
      :lists.map(
        fn mod ->
          {mod, :all}
        end,
        mods
      )
    )
  end

  def add_conf(name, mod, conf) when is_tuple(conf) do
    add_job(cast_to_list(name), {mod, [conf]})
  end

  def add_conf(name, mod, confs) when is_list(confs) do
    add_job(cast_to_list(name), {mod, confs})
  end

  def add_case(mod, case__)
      when is_atom(mod) and
             is_atom(case__) do
    add_job(:erlang.atom_to_list(mod), {mod, case__})
  end

  def add_case(name, mod, case__)
      when is_atom(mod) and
             is_atom(case__) do
    add_job(name, {mod, case__})
  end

  def add_cases(mod, cases)
      when is_atom(mod) and
             is_list(cases) do
    add_job(:erlang.atom_to_list(mod), {mod, cases})
  end

  def add_cases(name, mod, cases)
      when is_atom(mod) and
             is_list(cases) do
    add_job(name, {mod, cases})
  end

  def add_spec(spec) do
    name = :filename.rootname(spec, '.spec')

    case :filelib.is_file(spec) do
      true ->
        add_job(name, {:spec, spec})

      false ->
        {:error, :nofile}
    end
  end

  def add_dir_with_skip(name, job = [dir | _Dirs], skip)
      when is_list(dir) do
    add_job(
      cast_to_list(name),
      :lists.map(
        fn d ->
          {:dir, cast_to_list(d)}
        end,
        job
      ),
      skip
    )
  end

  def add_dir_with_skip(name, dir, skip) do
    add_job(cast_to_list(name), {:dir, cast_to_list(dir)}, skip)
  end

  def add_dir_with_skip(name, job = [dir | _Dirs], pattern, skip)
      when is_list(dir) do
    add_job(
      cast_to_list(name),
      :lists.map(
        fn d ->
          {:dir, cast_to_list(d), cast_to_list(pattern)}
        end,
        job
      ),
      skip
    )
  end

  def add_dir_with_skip(name, dir, pattern, skip) do
    add_job(cast_to_list(name), {:dir, cast_to_list(dir), cast_to_list(pattern)}, skip)
  end

  def add_module_with_skip(mod, skip) when is_atom(mod) do
    add_job(:erlang.atom_to_list(mod), {mod, :all}, skip)
  end

  def add_module_with_skip(name, mods, skip) when is_list(mods) do
    add_job(
      cast_to_list(name),
      :lists.map(
        fn mod ->
          {mod, :all}
        end,
        mods
      ),
      skip
    )
  end

  def add_conf_with_skip(name, mod, conf, skip) when is_tuple(conf) do
    add_job(cast_to_list(name), {mod, [conf]}, skip)
  end

  def add_conf_with_skip(name, mod, confs, skip) when is_list(confs) do
    add_job(cast_to_list(name), {mod, confs}, skip)
  end

  def add_case_with_skip(mod, case__, skip)
      when is_atom(mod) and
             is_atom(case__) do
    add_job(:erlang.atom_to_list(mod), {mod, case__}, skip)
  end

  def add_case_with_skip(name, mod, case__, skip)
      when is_atom(mod) and
             is_atom(case__) do
    add_job(name, {mod, case__}, skip)
  end

  def add_cases_with_skip(mod, cases, skip)
      when is_atom(mod) and
             is_list(cases) do
    add_job(:erlang.atom_to_list(mod), {mod, cases}, skip)
  end

  def add_cases_with_skip(name, mod, cases, skip)
      when is_atom(mod) and
             is_list(cases) do
    add_job(name, {mod, cases}, skip)
  end

  def add_tests_with_skip(logDir, tests, skip) do
    add_job(
      logDir,
      :lists.map(
        fn
          {dir, :all, :all} ->
            {dir, {:dir, dir}}

          {dir, mods, :all} ->
            {dir,
             :lists.map(
               fn m ->
                 {m, :all}
               end,
               mods
             )}

          {dir, mod, cases} ->
            {dir, {mod, cases}}
        end,
        tests
      ),
      skip
    )
  end

  defp parse_cmd_line(cmds) do
    parse_cmd_line(cmds, [], [], :local, false, false, :undefined)
  end

  defp parse_cmd_line([[:SPEC, spec] | cmds], specList, names, param, trc, cov, tCCB) do
    case :file.consult(spec) do
      {:ok, termList} ->
        name = :filename.rootname(spec)
        parse_cmd_line(cmds, termList ++ specList, [name | names], param, trc, cov, tCCB)

      {:error, reason} ->
        :io.format('Can\'t open ~tw: ~tp\n', [spec, :file.format_error(reason)])
        parse_cmd_line(cmds, specList, names, param, trc, cov, tCCB)
    end
  end

  defp parse_cmd_line([[:NAME, name] | cmds], specList, names, param, trc, cov, tCCB) do
    parse_cmd_line(
      cmds,
      specList,
      [{:name, :erlang.atom_to_list(name)} | names],
      param,
      trc,
      cov,
      tCCB
    )
  end

  defp parse_cmd_line([[:SKIPMOD, mod] | cmds], specList, names, param, trc, cov, tCCB) do
    parse_cmd_line(
      cmds,
      [{:skip, {mod, 'by command line'}} | specList],
      names,
      param,
      trc,
      cov,
      tCCB
    )
  end

  defp parse_cmd_line([[:SKIPCASE, mod, case__] | cmds], specList, names, param, trc, cov, tCCB) do
    parse_cmd_line(
      cmds,
      [{:skip, {mod, case__, 'by command line'}} | specList],
      names,
      param,
      trc,
      cov,
      tCCB
    )
  end

  defp parse_cmd_line([[:DIR, dir] | cmds], specList, names, param, trc, cov, tCCB) do
    name = :filename.basename(dir)

    parse_cmd_line(
      cmds,
      [{:topcase, {:dir, name}} | specList],
      [name | names],
      param,
      trc,
      cov,
      tCCB
    )
  end

  defp parse_cmd_line([[:MODULE, mod] | cmds], specList, names, param, trc, cov, tCCB) do
    parse_cmd_line(
      cmds,
      [{:topcase, {mod, :all}} | specList],
      [:erlang.atom_to_list(mod) | names],
      param,
      trc,
      cov,
      tCCB
    )
  end

  defp parse_cmd_line([[:CASE, mod, case__] | cmds], specList, names, param, trc, cov, tCCB) do
    parse_cmd_line(
      cmds,
      [{:topcase, {mod, case__}} | specList],
      [:erlang.atom_to_list(mod) | names],
      param,
      trc,
      cov,
      tCCB
    )
  end

  defp parse_cmd_line([[:TRACE, trc] | cmds], specList, names, param, _Trc, cov, tCCB) do
    parse_cmd_line(cmds, specList, names, param, trc, cov, tCCB)
  end

  defp parse_cmd_line(
         [[:COVER, app, cF, analyse] | cmds],
         specList,
         names,
         param,
         trc,
         _Cov,
         tCCB
       ) do
    parse_cmd_line(cmds, specList, names, param, trc, {{app, cF}, analyse}, tCCB)
  end

  defp parse_cmd_line(
         [[:TESTCASE_CALLBACK, mod, func] | cmds],
         specList,
         names,
         param,
         trc,
         cov,
         _
       ) do
    parse_cmd_line(cmds, specList, names, param, trc, cov, {mod, func})
  end

  defp parse_cmd_line([obj | _Cmds], _SpecList, _Names, _Param, _Trc, _Cov, _TCCB) do
    :io.format('~w: Bad argument: ~tw\n', [:test_server_ctrl, obj])
    :io.format(' Use the `ts\' module to start tests.\n', [])
    :io.format(' (If you ARE using `ts\', there is a bug in `ts\'.)\n', [])
    :erlang.halt(1)
  end

  defp parse_cmd_line([], specList, names, param, trc, cov, tCCB) do
    nameList = :lists.reverse(names, ['suite'])

    name =
      case :lists.keysearch(:name, 1, nameList) do
        {:value, {:name, n}} ->
          n

        false ->
          hd(nameList)
      end

    {:lists.reverse(specList), name, param, trc, cov, tCCB}
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

  def start(_) do
    start()
  end

  def start_link(_) do
    start_link()
  end

  def start() do
    case :gen_server.start({:local, :test_server_ctrl}, :test_server_ctrl, [], []) do
      {:error, {:already_started, pid}} ->
        {:ok, pid}

      other ->
        other
    end
  end

  defp start_link() do
    case :gen_server.start_link({:local, :test_server_ctrl}, :test_server_ctrl, [], []) do
      {:error, {:already_started, pid}} ->
        {:ok, pid}

      other ->
        other
    end
  end

  def run_test(commandLine) do
    :erlang.process_flag(:trap_exit, true)
    {specList, name, param, trc, cov, tCCB} = parse_cmd_line(commandLine)
    {:ok, _TSPid} = start_link(param)

    case trc do
      false ->
        :ok

      file ->
        trc(file)
    end

    case cov do
      false ->
        :ok

      {{app, coverFile}, analyse} ->
        cover(app, maybe_file(coverFile), analyse)
    end

    testcase_callback(tCCB)
    add_job(name, {:command_line, specList})
    wait_finish()
  end

  defp maybe_file(:none) do
    :none
  end

  defp maybe_file(coverFile) do
    :erlang.atom_to_list(coverFile)
  end

  def idle_notify(fun) do
    {:ok, pid} = controller_call({:idle_notify, fun})
    pid
  end

  def start_get_totals(fun) do
    {:ok, pid} = controller_call({:start_get_totals, fun})
    pid
  end

  def stop_get_totals() do
    :ok = controller_call(:stop_get_totals)
    :ok
  end

  def wait_finish() do
    oldTrap = :erlang.process_flag(:trap_exit, true)
    {:ok, pid} = finish(true)
    :erlang.link(pid)

    receive do
      {:EXIT, ^pid, _} ->
        :ok
    end

    :erlang.process_flag(:trap_exit, oldTrap)
    :ok
  end

  def abort_current_testcase(reason) do
    controller_call({:abort_current_testcase, reason})
  end

  def abort() do
    oldTrap = :erlang.process_flag(:trap_exit, true)
    {:ok, pid} = finish(:abort)
    :erlang.link(pid)

    receive do
      {:EXIT, ^pid, _} ->
        :ok
    end

    :erlang.process_flag(:trap_exit, oldTrap)
    :ok
  end

  defp finish(abort) do
    controller_call({:finish, abort})
  end

  def stop() do
    controller_call(:stop)
  end

  def jobs() do
    controller_call(:jobs)
  end

  def get_levels() do
    controller_call(:get_levels)
  end

  def set_levels(show, major, minor) do
    controller_call({:set_levels, show, major, minor})
  end

  def reject_io_reqs(bool) do
    controller_call({:reject_io_reqs, bool})
  end

  def multiply_timetraps(n) do
    controller_call({:multiply_timetraps, n})
  end

  def scale_timetraps(bool) do
    controller_call({:scale_timetraps, bool})
  end

  def get_timetrap_parameters() do
    controller_call(:get_timetrap_parameters)
  end

  def create_priv_dir(value) do
    controller_call({:create_priv_dir, value})
  end

  def trc(traceFile) do
    controller_call({:trace, traceFile}, 2 * 20000)
  end

  def stop_trace() do
    controller_call(:stop_trace)
  end

  def node_started(node) do
    :gen_server.cast(
      :test_server_ctrl,
      {:node_started, node}
    )
  end

  def cover(app, analyse) when is_atom(app) do
    cover(app, :none, analyse)
  end

  def cover(coverFile, analyse) do
    cover(:none, coverFile, analyse)
  end

  def cover(app, coverFile, analyse) do
    {excl, incl, cross} = read_cover_file(coverFile)

    coverInfo =
      r_cover(app: app, file: coverFile, excl: excl, incl: incl, cross: cross, level: analyse)

    controller_call({:cover, coverInfo})
  end

  def cover(coverInfo) do
    controller_call({:cover, coverInfo})
  end

  def cover_compile(app, file, excl, incl, cross, analyse, stop) do
    cover_compile(
      r_cover(
        app: app,
        file: file,
        excl: excl,
        incl: incl,
        cross: cross,
        level: analyse,
        stop: stop
      )
    )
  end

  def testcase_callback(modFunc) do
    controller_call({:testcase_callback, modFunc})
  end

  def set_random_seed(seed) do
    controller_call({:set_random_seed, seed})
  end

  def kill_slavenodes() do
    controller_call(:kill_slavenodes)
  end

  def get_hosts() do
    :erlang.get(:test_server_hosts)
  end

  defp add_job(name, topCase) do
    add_job(name, topCase, [])
  end

  defp add_job(name, topCase, skip) do
    suiteName =
      case name do
        '.' ->
          'current_dir'

        '..' ->
          'parent_dir'

        other ->
          other
      end

    dir = :filename.absname(suiteName)
    controller_call({:add_job, dir, suiteName, topCase, skip})
  end

  defp controller_call(arg) do
    case (try do
            :gen_server.call(:test_server_ctrl, arg, :infinity)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, {{:badarg, _}, {:gen_server, :call, _}}} ->
        exit(:test_server_ctrl_not_running)

      {:EXIT, reason} ->
        exit(reason)

      other ->
        other
    end
  end

  defp controller_call(arg, timeout) do
    case (try do
            :gen_server.call(:test_server_ctrl, arg, timeout)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, {{:badarg, _}, {:gen_server, :call, _}}} ->
        exit(:test_server_ctrl_not_running)

      {:EXIT, reason} ->
        exit(reason)

      other ->
        other
    end
  end

  def init([]) do
    case :os.getenv('TEST_SERVER_CALL_TRACE') do
      false ->
        :ok

      '' ->
        :ok

      traceSpec ->
        :test_server_sup.call_trace(traceSpec)
    end

    :erlang.process_flag(:trap_exit, true)

    case :init.get_argument(:test_server_format_exception) do
      {:ok, [[tSFE]]} ->
        :application.set_env(:test_server, :format_exception, :erlang.list_to_atom(tSFE))

      _ ->
        :ok
    end

    :test_server_sup.cleanup_crash_dumps()
    :test_server_sup.util_start()
    state = r_state(jobs: [], finish: false)
    tI0 = :test_server.init_target_info()
    targetHost = :test_server_sup.hoststr()
    tI = r_target_info(tI0, host: targetHost, naming: naming(), master: targetHost)

    _ =
      :ets.new(
        :slave_tab,
        [:named_table, :set, :public, {:keypos, 2}]
      )

    set_hosts([r_target_info(tI, :host)])
    {:ok, r_state(state, target_info: tI)}
  end

  defp naming() do
    case :lists.member(?., :test_server_sup.hoststr()) do
      true ->
        '-name'

      false ->
        '-sname'
    end
  end

  def handle_call(:kill_slavenodes, _From, state) do
    nodes = :test_server_node.kill_nodes()
    {:reply, nodes, state}
  end

  def handle_call({:set_hosts, hosts}, _From, state) do
    set_hosts(hosts)
    {:reply, :ok, state}
  end

  def handle_call(:get_hosts, _From, state) do
    hosts = get_hosts()
    {:reply, hosts, state}
  end

  def handle_call({:add_job, dir, name, topCase, skip}, _From, state) do
    logDir = dir ++ '.logs'

    extraTools =
      case r_state(state, :cover) do
        false ->
          []

        coverInfo ->
          [{:cover, coverInfo}]
      end

    extraTools1 =
      case r_state(state, :random_seed) do
        :undefined ->
          extraTools

        seed ->
          [{:random_seed, seed} | extraTools]
      end

    case :lists.keysearch(name, 1, r_state(state, :jobs)) do
      false ->
        case topCase do
          {:spec, specName} ->
            pid =
              spawn_tester(
                :test_server_ctrl,
                :do_spec,
                [
                  specName,
                  {r_state(state, :multiply_timetraps), r_state(state, :scale_timetraps)}
                ],
                logDir,
                name,
                r_state(state, :levels),
                r_state(state, :reject_io_reqs),
                r_state(state, :create_priv_dir),
                r_state(state, :testcase_callback),
                extraTools1
              )

            newJobs = [{name, pid} | r_state(state, :jobs)]
            {:reply, :ok, r_state(state, jobs: newJobs)}

          {:command_line, specList} ->
            pid =
              spawn_tester(
                :test_server_ctrl,
                :do_spec_list,
                [
                  specList,
                  {r_state(state, :multiply_timetraps), r_state(state, :scale_timetraps)}
                ],
                logDir,
                name,
                r_state(state, :levels),
                r_state(state, :reject_io_reqs),
                r_state(state, :create_priv_dir),
                r_state(state, :testcase_callback),
                extraTools1
              )

            newJobs = [{name, pid} | r_state(state, :jobs)]
            {:reply, :ok, r_state(state, jobs: newJobs)}

          ^topCase ->
            case r_state(state, :get_totals) do
              {cliPid, fun} ->
                result = count_test_cases(topCase, skip)
                fun.(cliPid, result)
                {:reply, :ok, state}

              _ ->
                cfg = make_config([])

                pid =
                  spawn_tester(
                    :test_server_ctrl,
                    :do_test_cases,
                    [
                      topCase,
                      skip,
                      cfg,
                      {r_state(state, :multiply_timetraps), r_state(state, :scale_timetraps)}
                    ],
                    logDir,
                    name,
                    r_state(state, :levels),
                    r_state(state, :reject_io_reqs),
                    r_state(state, :create_priv_dir),
                    r_state(state, :testcase_callback),
                    extraTools1
                  )

                newJobs = [{name, pid} | r_state(state, :jobs)]
                {:reply, :ok, r_state(state, jobs: newJobs)}
            end
        end

      _ ->
        {:reply, {:error, :name_already_in_use}, state}
    end
  end

  def handle_call(:jobs, _From, state) do
    {:reply, r_state(state, :jobs), state}
  end

  def handle_call({:abort_current_testcase, reason}, _From, state) do
    case r_state(state, :jobs) do
      [{_, pid} | _] ->
        send(pid, {:abort_current_testcase, reason, self()})

        receive do
          {^pid, :abort_current_testcase, result} ->
            {:reply, result, state}
        after
          10000 ->
            {:reply, {:error, :no_testcase_running}, state}
        end

      _ ->
        {:reply, {:error, :no_testcase_running}, state}
    end
  end

  def handle_call({:finish, fini}, _From, state) do
    case r_state(state, :jobs) do
      [] ->
        :lists.foreach(
          fn {cli, fun} ->
            fun.(cli, fini)
          end,
          r_state(state, :idle_notify)
        )

        state2 = r_state(state, finish: false)
        {:stop, :shutdown, {:ok, self()}, state2}

      _SomeJobs ->
        state2 = r_state(state, finish: fini)
        {:reply, {:ok, self()}, state2}
    end
  end

  def handle_call({:idle_notify, fun}, {cli, _Ref}, state) do
    case r_state(state, :jobs) do
      [] ->
        send(self(), :report_idle)

      _ ->
        :ok
    end

    subscribed = r_state(state, :idle_notify)
    {:reply, {:ok, self()}, r_state(state, idle_notify: [{cli, fun} | subscribed])}
  end

  def handle_call({:start_get_totals, fun}, {cli, _Ref}, state) do
    {:reply, {:ok, self()}, r_state(state, get_totals: {cli, fun})}
  end

  def handle_call(:stop_get_totals, {_Cli, _Ref}, state) do
    {:reply, :ok, r_state(state, get_totals: false)}
  end

  def handle_call(:get_levels, _From, state) do
    {:reply, r_state(state, :levels), state}
  end

  def handle_call({:set_levels, show, major, minor}, _From, state) do
    {:reply, :ok, r_state(state, levels: {show, major, minor})}
  end

  def handle_call({:reject_io_reqs, bool}, _From, state) do
    {:reply, :ok, r_state(state, reject_io_reqs: bool)}
  end

  def handle_call({:multiply_timetraps, n}, _From, state) do
    {:reply, :ok, r_state(state, multiply_timetraps: n)}
  end

  def handle_call({:scale_timetraps, bool}, _From, state) do
    {:reply, :ok, r_state(state, scale_timetraps: bool)}
  end

  def handle_call(:get_timetrap_parameters, _From, state) do
    {:reply, {r_state(state, :multiply_timetraps), r_state(state, :scale_timetraps)}, state}
  end

  def handle_call({:trace, traceFile}, _From, state = r_state(trc: false)) do
    tI = r_state(state, :target_info)

    case :test_server_node.start_tracer_node(
           traceFile,
           tI
         ) do
      {:ok, tracer} ->
        {:reply, :ok, r_state(state, trc: tracer)}

      error ->
        {:reply, error, state}
    end
  end

  def handle_call({:trace, _TraceFile}, _From, state) do
    {:reply, {:error, :already_tracing}, state}
  end

  def handle_call(:stop_trace, _From, state = r_state(trc: false)) do
    {:reply, {:error, :not_tracing}, state}
  end

  def handle_call(:stop_trace, _From, state) do
    r = :test_server_node.stop_tracer_node(r_state(state, :trc))
    {:reply, r, r_state(state, trc: false)}
  end

  def handle_call({:cover, coverInfo}, _From, state) do
    {:reply, :ok, r_state(state, cover: coverInfo)}
  end

  def handle_call({:create_priv_dir, value}, _From, state) do
    {:reply, :ok, r_state(state, create_priv_dir: value)}
  end

  def handle_call({:testcase_callback, modFunc}, _From, state) do
    case modFunc do
      {mod, func} ->
        _ =
          case :code.is_loaded(mod) do
            {:file, _} ->
              :ok

            false ->
              :code.load_file(mod)
          end

        case :erlang.function_exported(mod, func, 4) do
          true ->
            :ok

          false ->
            :io.format(:user, 'WARNING! Callback function ~w:~tw/4 undefined.~n~n', [mod, func])
        end

      _ ->
        :ok
    end

    {:reply, :ok, r_state(state, testcase_callback: modFunc)}
  end

  def handle_call({:set_random_seed, seed}, _From, state) do
    {:reply, :ok, r_state(state, random_seed: seed)}
  end

  def handle_call(:stop, _From, state) do
    {:stop, :shutdown, :ok, state}
  end

  def handle_call(:get_target_info, _From, state) do
    {:reply, r_state(state, :target_info), state}
  end

  def handle_call({:start_node, name, type, options}, from, state) do
    :test_server_node.start_node(name, type, options, from, r_state(state, :target_info))
    {:noreply, state}
  end

  def handle_call({:wait_for_node, node}, from, state) do
    newWaitList =
      case :ets.lookup(:slave_tab, node) do
        [] ->
          [{node, from} | r_state(state, :wait_for_node)]

        _ ->
          :gen_server.reply(from, :ok)
          r_state(state, :wait_for_node)
      end

    {:noreply, r_state(state, wait_for_node: newWaitList)}
  end

  def handle_call({:stop_node, name}, _From, state) do
    r = :test_server_node.stop_node(name)
    {:reply, r, state}
  end

  def handle_call({:is_release_available, release}, _From, state) do
    r = :test_server_node.is_release_available(release)
    {:reply, r, state}
  end

  defp set_hosts(hosts) do
    :erlang.put(:test_server_hosts, hosts)
  end

  def handle_cast({:node_started, node}, state) do
    case r_state(state, :trc) do
      false ->
        :ok

      trc ->
        :test_server_node.trace_nodes(trc, [node])
    end

    newWaitList =
      case :lists.keysearch(node, 1, r_state(state, :wait_for_node)) do
        {:value, {^node, from}} ->
          :gen_server.reply(from, :ok)
          :lists.keydelete(node, 1, r_state(state, :wait_for_node))

        false ->
          r_state(state, :wait_for_node)
      end

    {:noreply, r_state(state, wait_for_node: newWaitList)}
  end

  def handle_info(:report_idle, state) do
    finish = r_state(state, :finish)

    :lists.foreach(
      fn {cli, fun} ->
        fun.(cli, finish)
      end,
      r_state(state, :idle_notify)
    )

    {:noreply, r_state(state, idle_notify: [])}
  end

  def handle_info({:EXIT, pid, reason}, state) do
    case :lists.keysearch(pid, 2, r_state(state, :jobs)) do
      false ->
        {:noreply, state}

      {:value, {name, _}} ->
        newJobs = :lists.keydelete(pid, 2, r_state(state, :jobs))

        case reason do
          :normal ->
            :fine

          :killed ->
            :io.format('Suite ~ts was killed\n', [name])

          _Other ->
            :io.format('Suite ~ts was killed with reason ~tp\n', [name, reason])
        end

        state2 = r_state(state, jobs: newJobs)
        finish = r_state(state2, :finish)

        case newJobs do
          [] ->
            :lists.foreach(
              fn {cli, fun} ->
                fun.(cli, finish)
              end,
              r_state(state2, :idle_notify)
            )

            case finish do
              false ->
                {:noreply, r_state(state2, idle_notify: [])}

              _ ->
                {:stop, :shutdown, r_state(state2, finish: false)}
            end

          _ ->
            case finish do
              :abort ->
                :lists.foreach(
                  fn {cli, fun} ->
                    fun.(cli, finish)
                  end,
                  r_state(state2, :idle_notify)
                )

                {:stop, :shutdown, r_state(state2, finish: false)}

              _ ->
                {:noreply, state2}
            end
        end
    end
  end

  def handle_info({:tcp_closed, sock}, state = r_state(trc: sock)) do
    {:noreply, r_state(state, trc: false)}
  end

  def handle_info({:tcp_closed, sock}, state) do
    :test_server_node.nodedown(sock)
    {:noreply, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  def terminate(_Reason, state) do
    :test_server_sup.util_stop()

    case r_state(state, :trc) do
      false ->
        :ok

      sock ->
        :test_server_node.stop_tracer_node(sock)
    end

    :ok = kill_all_jobs(r_state(state, :jobs))
    _ = :test_server_node.kill_nodes()
    :ok
  end

  defp kill_all_jobs([{_Name, jobPid} | jobs]) do
    :erlang.exit(jobPid, :kill)
    kill_all_jobs(jobs)
  end

  defp kill_all_jobs([]) do
    :ok
  end

  defp spawn_tester(
         mod,
         func,
         args,
         dir,
         name,
         levels,
         rejectIoReqs,
         createPrivDir,
         tCCallback,
         extraTools
       ) do
    spawn_link(fn ->
      init_tester(
        mod,
        func,
        args,
        dir,
        name,
        levels,
        rejectIoReqs,
        createPrivDir,
        tCCallback,
        extraTools
      )
    end)
  end

  defp init_tester(
         mod,
         func,
         args,
         dir,
         name,
         {_, _, minLev} = levels,
         rejectIoReqs,
         createPrivDir,
         tCCallback,
         extraTools
       ) do
    :erlang.process_flag(:trap_exit, true)
    _ = :test_server_io.start_link()
    :erlang.put(:app, :common_test)
    :erlang.put(:test_server_name, name)
    :erlang.put(:test_server_dir, dir)
    :erlang.put(:test_server_total_time, 0)
    :erlang.put(:test_server_ok, 0)
    :erlang.put(:test_server_failed, 0)
    :erlang.put(:test_server_skipped, {0, 0})
    :erlang.put(:test_server_minor_level, minLev)
    :erlang.put(:test_server_create_priv_dir, createPrivDir)

    :erlang.put(
      :test_server_random_seed,
      :proplists.get_value(:random_seed, extraTools)
    )

    :erlang.put(:test_server_testcase_callback, tCCallback)

    case :os.getenv('TEST_SERVER_FRAMEWORK') do
      fW when fW === false or fW === 'undefined' ->
        :erlang.put(:test_server_framework, :"$none")

      fW ->
        :erlang.put(
          :test_server_framework_name,
          :erlang.list_to_atom(fW)
        )

        case :os.getenv('TEST_SERVER_FRAMEWORK_NAME') do
          fWName when fWName === false or fWName === 'undefined' ->
            :erlang.put(:test_server_framework_name, :"$none")

          fWName ->
            :erlang.put(
              :test_server_framework_name,
              :erlang.list_to_atom(fWName)
            )
        end
    end

    fWLogDir =
      case :test_server_sup.framework_call(:get_log_dir, [], []) do
        {:ok, fwDir} ->
          fwDir

        _ ->
          :filename.dirname(dir)
      end

    :erlang.put(:test_server_framework_logdir, fWLogDir)
    logOpts = :test_server_sup.framework_call(:get_logopts, [], [])
    :erlang.put(:test_server_logopts, logOpts)
    startedExtraTools = start_extra_tools(extraTools)
    :test_server_io.set_job_name(name)

    :test_server_io.set_gl_props([
      {:levels, levels},
      {:auto_nl, not :lists.member(:no_nl, logOpts)},
      {:reject_io_reqs, rejectIoReqs}
    ])

    :erlang.group_leader(
      :test_server_io.get_gl(true),
      self()
    )

    {timeMy, result} = ts_tc(mod, func, args)
    set_io_buffering(:undefined)
    :test_server_io.set_job_name(:undefined)

    try do
      stop_extra_tools(startedExtraTools)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    case result do
      {:EXIT, :test_suites_done} ->
        :ok

      {:EXIT, _Pid, reason} ->
        print(1, 'EXIT, reason ~tp', [reason])

      {:EXIT, reason} ->
        report_severe_error(reason)
        print(1, 'EXIT, reason ~tp', [reason])
    end

    time = timeMy / 1_000_000

    successStr =
      case :erlang.get(:test_server_failed) do
        0 ->
          'Ok'

        _ ->
          'FAILED'
      end

    {skippedN, skipStr} =
      case :erlang.get(:test_server_skipped) do
        {0, 0} ->
          {0, ''}

        {uSkipped, aSkipped} ->
          skipped = uSkipped + aSkipped
          {skipped, :io_lib.format(', ~w Skipped', [skipped])}
      end

    okN = :erlang.get(:test_server_ok)
    failedN = :erlang.get(:test_server_failed)

    print(
      :html,
      '\n</tbody>\n<tfoot>\n<tr><td></td><td><b>TOTAL</b></td><td></td><td></td><td></td><td>~.3fs</td><td><b>~ts</b></td><td>~w Ok, ~w Failed~ts of ~w</td></tr>\n</tfoot>\n',
      [time, successStr, okN, failedN, skipStr, okN + failedN + skippedN]
    )

    :test_server_io.stop([:major, :html, :unexpected_io])
    {unexpectedIoName, unexpectedIoFooter} = :erlang.get(:test_server_unexpected_footer)

    {:ok, unexpectedIoFd} =
      open_html_file(
        unexpectedIoName,
        [:append]
      )

    :io.put_chars(unexpectedIoFd, '\n</pre>\n' ++ unexpectedIoFooter)
    :ok = :file.close(unexpectedIoFd)
  end

  defp report_severe_error(reason) do
    :test_server_sup.framework_call(
      :report,
      [:severe_error, reason]
    )
  end

  defp ts_tc(m, f, a) do
    before = :erlang.monotonic_time()

    result =
      try do
        apply(m, f, a)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    after__ = :erlang.monotonic_time()
    elapsed = :erlang.convert_time_unit(after__ - before, :native, :micro_seconds)
    {elapsed, result}
  end

  defp start_extra_tools(extraTools) do
    start_extra_tools(extraTools, [])
  end

  defp start_extra_tools([{:cover, coverInfo} | extraTools], started) do
    case start_cover(coverInfo) do
      {:ok, newCoverInfo} ->
        start_extra_tools(
          extraTools,
          [{:cover, newCoverInfo} | started]
        )

      {:error, _} ->
        start_extra_tools(extraTools, started)
    end
  end

  defp start_extra_tools([_ | extraTools], started) do
    start_extra_tools(extraTools, started)
  end

  defp start_extra_tools([], started) do
    started
  end

  defp stop_extra_tools(extraTools) do
    testDir = :erlang.get(:test_server_log_dir_base)

    case :lists.keymember(:cover, 1, extraTools) do
      false ->
        write_default_coverlog(testDir)

      true ->
        :ok
    end

    stop_extra_tools(extraTools, testDir)
  end

  defp stop_extra_tools([{:cover, coverInfo} | extraTools], testDir) do
    stop_cover(coverInfo, testDir)
    stop_extra_tools(extraTools, testDir)
  end

  defp stop_extra_tools([], _) do
    :ok
  end

  def do_spec(specName, timetrapSpec) when is_list(specName) do
    case :file.consult(specName) do
      {:ok, termList} ->
        do_spec_list(termList, timetrapSpec)

      {:error, reason} ->
        :io.format('Can\'t open ~ts: ~tp\n', [specName, reason])
        {:error, {:cant_open_spec, reason}}
    end
  end

  def do_spec_list(termList0, timetrapSpec) do
    nodes = []

    termList =
      case :lists.keysearch(:hosts, 1, termList0) do
        {:value, {:hosts, hosts0}} ->
          hosts =
            :lists.map(
              fn h ->
                cast_to_list(h)
              end,
              hosts0
            )

          controller_call({:set_hosts, hosts})
          :lists.keydelete(:hosts, 1, termList0)

        _ ->
          termList0
      end

    defaultConfig = make_config([{:nodes, nodes}])
    {topCases, skipList, config} = do_spec_terms(termList, [], [], defaultConfig)
    do_test_cases(topCases, skipList, config, timetrapSpec)
  end

  defp do_spec_terms([], topCases, skipList, config) do
    {topCases, skipList, config}
  end

  defp do_spec_terms([{:topcase, topCase} | terms], topCases, skipList, config) do
    do_spec_terms(terms, [topCase | topCases], skipList, config)
  end

  defp do_spec_terms([{:skip, skip} | terms], topCases, skipList, config) do
    do_spec_terms(terms, topCases, [skip | skipList], config)
  end

  defp do_spec_terms([{:nodes, nodes} | terms], topCases, skipList, config) do
    do_spec_terms(terms, topCases, skipList, update_config(config, {:nodes, nodes}))
  end

  defp do_spec_terms([{:diskless, how} | terms], topCases, skipList, config) do
    do_spec_terms(terms, topCases, skipList, update_config(config, {:diskless, how}))
  end

  defp do_spec_terms([{:config, moreConfig} | terms], topCases, skipList, config) do
    do_spec_terms(terms, topCases, skipList, config ++ moreConfig)
  end

  defp do_spec_terms([{:default_timeout, tmo} | terms], topCases, skipList, config) do
    do_spec_terms(terms, topCases, skipList, update_config(config, {:default_timeout, tmo}))
  end

  defp do_spec_terms([{:require_nodenames, numNames} | terms], topCases, skipList, config) do
    nodeNames0 = generate_nodenames(numNames)
    nodeNames = :lists.delete([], nodeNames0)
    do_spec_terms(terms, topCases, skipList, update_config(config, {:nodenames, nodeNames}))
  end

  defp do_spec_terms([other | terms], topCases, skipList, config) do
    :io.format('** WARNING: Spec file contains unknown directive ~tp\n', [other])
    do_spec_terms(terms, topCases, skipList, config)
  end

  defp generate_nodenames(num) do
    hosts =
      case controller_call(:get_hosts) do
        [] ->
          tI = controller_call(:get_target_info)
          [r_target_info(tI, :host)]

        list ->
          list
      end

    generate_nodenames2(num, hosts, [])
  end

  defp generate_nodenames2(0, _Hosts, acc) do
    acc
  end

  defp generate_nodenames2(n, hosts, acc) do
    host = :lists.nth(rem(n, length(hosts)) + 1, hosts)

    name =
      :erlang.list_to_atom(
        temp_nodename(
          'nod',
          []
        ) ++ '@' ++ host
      )

    generate_nodenames2(n - 1, hosts, [name | acc])
  end

  defp temp_nodename([], acc) do
    :lists.flatten(acc)
  end

  defp temp_nodename([chr | base], acc) do
    {a, b, c} = :os.timestamp()

    new = [
      chr
      | :erlang.integer_to_list(chr ^^^ a ^^^ (b + a) ^^^ (c + b))
    ]

    temp_nodename(base, [new | acc])
  end

  defp count_test_cases(topCases, skipCases) when is_list(topCases) do
    case collect_all_cases(topCases, skipCases) do
      {:error, _Why} = error ->
        error

      testSpec ->
        {get_suites(testSpec, []),
         case remove_conf(testSpec) do
           {:repeats, _} ->
             :unknown

           testSpec1 ->
             length(testSpec1)
         end}
    end
  end

  defp count_test_cases(topCase, skipCases) do
    count_test_cases([topCase], skipCases)
  end

  defp remove_conf(cases) do
    remove_conf(cases, [], false)
  end

  defp remove_conf([{:conf, _Ref, props, _MF} | cases], noConf, repeats) do
    case get_repeat(props) do
      :undefined ->
        remove_conf(cases, noConf, repeats)

      {_RepType, 1} ->
        remove_conf(cases, noConf, repeats)

      _ ->
        remove_conf(cases, noConf, true)
    end
  end

  defp remove_conf([{:make, _Ref, _MF} | cases], noConf, repeats) do
    remove_conf(cases, noConf, repeats)
  end

  defp remove_conf(
         [
           {:skip_case, {{_M, :all}, _Cmt}, _Mode}
           | cases
         ],
         noConf,
         repeats
       ) do
    remove_conf(cases, noConf, repeats)
  end

  defp remove_conf([{:skip_case, {type, _Ref, _MF, _Cmt}} | cases], noConf, repeats)
       when type == :conf or type == :make do
    remove_conf(cases, noConf, repeats)
  end

  defp remove_conf(
         [
           {:skip_case, {type, _Ref, _MF, _Cmt}, _Mode}
           | cases
         ],
         noConf,
         repeats
       )
       when type == :conf or type == :make do
    remove_conf(cases, noConf, repeats)
  end

  defp remove_conf([c = {mod, :error_in_suite, _} | cases], noConf, repeats) do
    fwMod = get_fw_mod(:test_server_ctrl)

    cond do
      mod == fwMod ->
        remove_conf(cases, noConf, repeats)

      true ->
        remove_conf(cases, [c | noConf], repeats)
    end
  end

  defp remove_conf([c = {:repeat, _, _} | cases], noConf, _Repeats) do
    remove_conf(cases, [c | noConf], true)
  end

  defp remove_conf([c | cases], noConf, repeats) do
    remove_conf(cases, [c | noConf], repeats)
  end

  defp remove_conf([], noConf, true) do
    {:repeats, :lists.reverse(noConf)}
  end

  defp remove_conf([], noConf, false) do
    :lists.reverse(noConf)
  end

  defp get_suites(
         [
           {:skip_case, {{mod, _F}, _Cmt}, _Mode}
           | tests
         ],
         mods
       )
       when is_atom(mod) do
    case add_mod(mod, mods) do
      true ->
        get_suites(tests, [mod | mods])

      false ->
        get_suites(tests, mods)
    end
  end

  defp get_suites([{mod, _Case} | tests], mods)
       when is_atom(mod) do
    case add_mod(mod, mods) do
      true ->
        get_suites(tests, [mod | mods])

      false ->
        get_suites(tests, mods)
    end
  end

  defp get_suites([{mod, _Func, _Args} | tests], mods)
       when is_atom(mod) do
    case add_mod(mod, mods) do
      true ->
        get_suites(tests, [mod | mods])

      false ->
        get_suites(tests, mods)
    end
  end

  defp get_suites([_ | tests], mods) do
    get_suites(tests, mods)
  end

  defp get_suites([], mods) do
    :lists.reverse(mods)
  end

  defp add_mod(mod, mods) do
    case :lists.reverse(:erlang.atom_to_list(mod)) do
      'ETIUS_' ++ _ ->
        case :lists.member(mod, mods) do
          true ->
            false

          false ->
            true
        end

      _ ->
        false
    end
  end

  def do_test_cases(topCases, skipCases, config, multiplyTimetrap)
      when is_integer(multiplyTimetrap) or
             multiplyTimetrap == :infinity do
    do_test_cases(topCases, skipCases, config, {multiplyTimetrap, true})
  end

  def do_test_cases(topCases, skipCases, config, timetrapData)
      when is_list(topCases) and is_tuple(timetrapData) do
    {:ok, testDir} = start_log_file()
    fwMod = get_fw_mod(:test_server_ctrl)

    case collect_all_cases(topCases, skipCases) do
      {:error, why} ->
        print(1, 'Error starting: ~tp', [why])
        exit(:test_suites_done)

      testSpec0 ->
        n =
          case remove_conf(testSpec0) do
            {:repeats, _} ->
              :unknown

            tS ->
              length(tS)
          end

        :erlang.put(:test_server_cases, n)
        :erlang.put(:test_server_case_num, 0)
        testSpec = add_init_and_end_per_suite(testSpec0, :undefined, :undefined, fwMod)
        tI = get_target_info()

        print(1, 'Starting test~ts', [
          print_if_known(n, {', ~w test cases', [n]}, {' (with repeated test cases)', []})
        ])

        test = :erlang.get(:test_server_name)

        testName =
          cond do
            is_list(test) ->
              :lists.flatten(:io_lib.format('~ts', [test]))

            true ->
              :lists.flatten(:io_lib.format('~tp', [test]))
          end

        testDescr = 'Test ' ++ testName ++ ' results'

        :test_server_sup.framework_call(
          :report,
          [:tests_start, {test, n}]
        )

        {header, footer} =
          case :test_server_sup.framework_call(
                 :get_html_wrapper,
                 [testDescr, true, testDir, {[], [2, 3, 4, 7, 8], [1, 6]}],
                 ''
               ) do
            empty when empty == '' or :erlang.element(2, empty) == '' ->
              :erlang.put(:basic_html, true)

              {[html_header(testDescr), '<h2>Results for test ', testName, '</h2>\n'],
               '\n</body>\n</html>\n'}

            {:basic_html, html0, html1} ->
              :erlang.put(:basic_html, true)
              {html0 ++ ['<h1>Results for <i>', testName, '</i></h1>\n'], html1}

            {:xhtml, html0, html1} ->
              :erlang.put(:basic_html, false)
              {html0 ++ ['<h1>Results for <i>', testName, '</i></h1>\n'], html1}
          end

        print(:html, '~ts', [header])
        print(:html, xhtml('<p>', '<h4>'))
        print_timestamp(:html, 'Test started at ')
        print(:html, xhtml('</p>', '</h4>'))
        print(:html, xhtml('\n<p><b>Host info:</b><br>\n', '\n<p><b>Host info:</b><br />\n'))

        print_who(
          :test_server_sup.hoststr(),
          :test_server_sup.get_username()
        )

        print(
          :html,
          xhtml(
            '<br>Used Erlang v~ts in <tt>~ts</tt></p>\n',
            '<br />Used Erlang v~ts in "~ts"</p>\n'
          ),
          [:erlang.system_info(:version), :code.root_dir()]
        )

        cond do
          fwMod == :test_server_ctrl ->
            print(
              :html,
              xhtml('\n<p><b>Target Info:</b><br>\n', '\n<p><b>Target Info:</b><br />\n')
            )

            print_who(r_target_info(tI, :host), r_target_info(tI, :username))

            print(
              :html,
              xhtml(
                '<br>Used Erlang v~ts in <tt>~ts</tt></p>\n',
                '<br />Used Erlang v~ts in "~ts"</p>\n'
              ),
              [r_target_info(tI, :version), r_target_info(tI, :root_dir)]
            )

          true ->
            case :test_server_sup.framework_call(
                   :target_info,
                   []
                 ) do
              targetInfo
              when is_list(targetInfo) and
                     length(targetInfo) > 0 ->
                print(
                  :html,
                  xhtml('\n<p><b>Target info:</b><br>\n', '\n<p><b>Target info:</b><br />\n')
                )

                print(:html, '~ts</p>\n', [targetInfo])

              _ ->
                :ok
            end
        end

        coverLog =
          case :erlang.get(:test_server_cover_log_dir) do
            :undefined ->
              'cover.html'

            absLogDir ->
              absLog = :filename.join(absLogDir, 'cover.html')
              make_relative(absLog, testDir)
          end

        print(
          :html,
          '<p><ul>\n<li><a href="~ts">Full textual log</a></li>\n<li><a href="~ts">Coverage log</a></li>\n<li><a href="~ts">Unexpected I/O log</a></li>\n</ul></p>\n',
          ['suite.log', coverLog, 'unexpected_io.log.html']
        )

        print(
          :html,
          '<p>~ts</p>\n' ++
            xhtml(
              '<table bgcolor="white" border="3" cellpadding="5">\n' ++ '<thead>\n',
              '<table id="' ++ 'SortableTable' ++ '">\n' ++ '<thead>\n'
            ) ++
            '<tr><th>Num</th><th>Module</th><th>Group</th>' ++
            '<th>Case</th><th>Log</th><th>Time</th><th>Result</th>' ++
            '<th>Comment</th></tr>\n</thead>\n<tbody>\n',
          [
            print_if_known(
              n,
              {'<i>Executing <b>~w</b> test cases...</i>' ++ xhtml('\n<br>\n', '\n<br />\n'),
               [n]},
              {'', []}
            )
          ]
        )

        print(:major, '=cases         ~w', [:erlang.get(:test_server_cases)])
        print(:major, '=user          ~ts', [r_target_info(tI, :username)])
        print(:major, '=host          ~ts', [r_target_info(tI, :host)])

        case controller_call(:get_hosts) do
          [] ->
            print(:major, '=hosts         ~ts', [r_target_info(tI, :host)])
            controller_call({:set_hosts, [r_target_info(tI, :host)]})

          hosts ->
            str =
              :lists.flatten(
                :lists.map(
                  fn x ->
                    [x, ' ']
                  end,
                  hosts
                )
              )

            print(:major, '=hosts         ~ts', [str])
        end

        print(:major, '=emulator_vsn  ~ts', [r_target_info(tI, :version)])
        print(:major, '=emulator      ~ts', [r_target_info(tI, :emulator)])
        print(:major, '=otp_release   ~ts', [r_target_info(tI, :otp_release)])
        print(:major, '=started       ~s', [:lists.flatten(timestamp_get(''))])
        :test_server_io.set_footer(footer)
        run_test_cases(testSpec, config, timetrapData)
    end
  end

  def do_test_cases(topCase, skipCases, config, timetrapSpec) do
    do_test_cases([topCase], skipCases, config, timetrapSpec)
  end

  defp start_log_file() do
    dir = :erlang.get(:test_server_dir)

    case :file.make_dir(dir) do
      :ok ->
        :ok

      {:error, :eexist} ->
        :ok

      mkDirError ->
        log_file_error(mkDirError, dir)
    end

    testDir = timestamp_filename_get(:filename.join(dir, 'run.'))

    testDir1 =
      case :file.make_dir(testDir) do
        :ok ->
          testDir

        {:error, :eexist} ->
          :timer.sleep(1000)

          testDirX =
            timestamp_filename_get(
              :filename.join(
                dir,
                'run.'
              )
            )

          case :file.make_dir(testDirX) do
            :ok ->
              testDirX

            mkDirError2 ->
              log_file_error(mkDirError2, testDirX)
          end

        mkDirError2 ->
          log_file_error(mkDirError2, testDir)
      end

    filenameMode = :file.native_name_encoding()
    :ok = write_file(:filename.join(dir, 'last_name'), testDir1 ++ '\n', filenameMode)
    :ok = write_file('last_name', testDir1 ++ '\n', filenameMode)
    :erlang.put(:test_server_log_dir_base, testDir1)
    majorName = :filename.join(testDir1, 'suite.log')
    htmlName = majorName ++ '.html'
    unexpectedName = :filename.join(testDir1, 'unexpected_io.log.html')
    {:ok, major} = open_utf8_file(majorName)
    {:ok, html} = open_html_file(htmlName)

    {unexpHeader, unexpFooter} =
      case :test_server_sup.framework_call(
             :get_html_wrapper,
             ['Unexpected I/O log', false, testDir, :undefined],
             ''
           ) do
        uEmpty
        when uEmpty == '' or
               :erlang.element(2, uEmpty) == '' ->
          {html_header('Unexpected I/O log'), '\n</body>\n</html>\n'}

        {:basic_html, uH, uF} ->
          {uH, uF}

        {:xhtml, uH, uF} ->
          {uH, uF}
      end

    {:ok, unexpected} = open_html_file(unexpectedName)

    :io.put_chars(unexpected, [
      unexpHeader,
      xhtml('<br>\n<h2>Unexpected I/O</h2>', '<br />\n<h3>Unexpected I/O</h3>'),
      '\n<pre>\n'
    ])

    :erlang.put(
      :test_server_unexpected_footer,
      {unexpectedName, unexpFooter}
    )

    :test_server_io.set_fd(:major, major)
    :test_server_io.set_fd(:html, html)
    :test_server_io.set_fd(:unexpected_io, unexpected)
    topDir = :filename.dirname(:erlang.get(:test_server_framework_logdir))
    redirectLink = :filename.join(topDir, 'suite.log.latest' ++ '.html')
    make_html_link(redirectLink, htmlName, :redirect)
    make_html_link(:filename.absname('last_test' ++ '.html'), htmlName, :filename.basename(dir))
    linkName = :filename.join(dir, 'last_link')
    make_html_link(linkName ++ '.html', htmlName, :filename.basename(dir))
    privDir = :filename.join(testDir1, 'log_private')
    :ok = :file.make_dir(privDir)
    :erlang.put(:test_server_priv_dir, privDir ++ '/')
    print_timestamp(:major, 'Suite started at ')
    logInfo = [{:topdir, dir}, {:rundir, :lists.flatten(testDir1)}]

    :test_server_sup.framework_call(
      :report,
      [:loginfo, logInfo]
    )

    {:ok, testDir1}
  end

  defp log_file_error(error, dir) do
    exit({:cannot_create_log_dir, {error, :lists.flatten(dir)}})
  end

  defp make_html_link(linkName, target, explanation) do
    targetL = :filename.split(target)
    pwdL = :filename.split(:filename.dirname(linkName))

    href =
      case :lists.prefix(pwdL, targetL) do
        true ->
          uri_encode(
            :filename.join(
              :lists.nthtail(
                length(pwdL),
                targetL
              )
            )
          )

        false ->
          'file:' ++ uri_encode(target)
      end

    h =
      cond do
        explanation == :redirect ->
          meta = ['<meta http-equiv="refresh" content="0; url=', href, '" />\n']
          [html_header('redirect', meta), '</html>\n']

        true ->
          [
            html_header(explanation),
            '<h1>Last test</h1>\n<a href="',
            href,
            '">',
            explanation,
            '</a>\n</body>\n</html>\n'
          ]
      end

    :ok = write_html_file(linkName, h)
  end

  defp start_minor_log_file(mod, func, parallelTC) do
    mFA = {mod, func, 1}
    logDir = :erlang.get(:test_server_log_dir_base)
    name = minor_log_file_name(mod, func)
    absName = :filename.join(logDir, name)

    case parallelTC or
           :erlang.element(
             1,
             :file.read_file_info(absName)
           ) == :ok do
      false ->
        start_minor_log_file1(mod, func, logDir, absName, mFA)

      true ->
        tag = :test_server_sup.unique_name()
        name1 = minor_log_file_name(mod, func, [?. | tag])
        absName1 = :filename.join(logDir, name1)
        start_minor_log_file1(mod, func, logDir, absName1, mFA)
    end
  end

  defp start_minor_log_file1(mod, func, logDir, absName, mFA) do
    {:ok, fd} = open_html_file(absName)
    lev = :erlang.get(:test_server_minor_level) + 1000
    :erlang.put(:test_server_minor_fd, fd)
    :test_server_gl.set_minor_fd(:erlang.group_leader(), fd, mFA)
    testDescr = :io_lib.format('Test ~w:~tw result', [mod, func])

    {header, footer} =
      case :test_server_sup.framework_call(
             :get_html_wrapper,
             [testDescr, false, :filename.dirname(absName), :undefined],
             ''
           ) do
        empty when empty == '' or :erlang.element(2, empty) == '' ->
          :erlang.put(:basic_html, true)
          {html_header(testDescr), '\n</body>\n</html>\n'}

        {:basic_html, html0, html1} ->
          :erlang.put(:basic_html, true)
          {html0, html1}

        {:xhtml, html0, html1} ->
          :erlang.put(:basic_html, false)
          {html0, html1}
      end

    :erlang.put(:test_server_minor_footer, footer)
    :io.put_chars(fd, header)
    :io.put_chars(fd, '<a name="top"></a>')
    :io.put_chars(fd, '<pre>\n')
    srcListing = downcase(:erlang.atom_to_list(mod)) ++ '.src.html'

    case get_fw_mod(:test_server_ctrl) do
      ^mod when func == :error_in_suite ->
        :ok

      _ ->
        {info, arity} =
          cond do
            func == :init_per_suite or func == :end_per_suite ->
              {'Config function: ', 1}

            func == :init_per_group or func == :end_per_group ->
              {'Config function: ', 2}

            true ->
              {'Test case: ', 1}
          end

        case {:filelib.is_file(
                :filename.join(
                  logDir,
                  srcListing
                )
              ),
              :lists.member(
                :no_src,
                :erlang.get(:test_server_logopts)
              )} do
          {true, false} ->
            print(
              lev,
              ['$tc_html', info ++ '<a href="~ts#~ts">~w:~tw/~w</a> (click for source code)\n'],
              [
                uri_encode(srcListing),
                uri_encode(:erlang.atom_to_list(func) ++ '-1', :utf8),
                mod,
                func,
                arity
              ]
            )

          _ ->
            print(lev, ['$tc_html', info ++ '~w:~tw/~w\n'], [mod, func, arity])
        end
    end

    absName
  end

  defp stop_minor_log_file() do
    :test_server_gl.unset_minor_fd(:erlang.group_leader())
    fd = :erlang.get(:test_server_minor_fd)
    footer = :erlang.get(:test_server_minor_footer)
    :io.put_chars(fd, '</pre>\n' ++ footer)
    :ok = :file.close(fd)
    :erlang.put(:test_server_minor_fd, :undefined)
  end

  defp minor_log_file_name(mod, func) do
    minor_log_file_name(mod, func, '')
  end

  defp minor_log_file_name(mod, func, tag) do
    name =
      downcase(
        :lists.flatten(
          :io_lib.format(
            '~w.~tw~s~s',
            [mod, func, tag, '.html']
          )
        )
      )

    ok = :file.native_name_encoding() == :utf8 or :io_lib.printable_latin1_list(name)

    cond do
      ok ->
        name

      true ->
        exit({:error, :unicode_name_on_latin1_file_system})
    end
  end

  defp downcase(s) do
    downcase(s, [])
  end

  defp downcase([uc | rest], result)
       when ?A <= uc and
              uc <= ?Z do
    downcase(rest, [uc - ?A + ?a | result])
  end

  defp downcase([c | rest], result) do
    downcase(rest, [c | result])
  end

  defp downcase([], result) do
    :lists.reverse(result)
  end

  defp html_convert_modules(testSpec, _Config, fwMod) do
    mods = html_isolate_modules(testSpec, fwMod)
    html_convert_modules(mods)

    copy_html_files(
      :erlang.get(:test_server_dir),
      :erlang.get(:test_server_log_dir_base)
    )
  end

  defp html_isolate_modules(list, fwMod) do
    html_isolate_modules(list, :sets.new(), fwMod)
  end

  defp html_isolate_modules([], set, _) do
    :sets.to_list(set)
  end

  defp html_isolate_modules([{:skip_case, {_Case, _Cmt}, _Mode} | cases], set, fwMod) do
    html_isolate_modules(cases, set, fwMod)
  end

  defp html_isolate_modules([{:conf, _Ref, props, {fwMod, _Func}} | cases], set, fwMod) do
    set1 =
      case :proplists.get_value(:suite, props) do
        :undefined ->
          set

        mod ->
          :sets.add_element(mod, set)
      end

    html_isolate_modules(cases, set1, fwMod)
  end

  defp html_isolate_modules([{:conf, _Ref, _Props, {mod, _Func}} | cases], set, fwMod) do
    html_isolate_modules(cases, :sets.add_element(mod, set), fwMod)
  end

  defp html_isolate_modules(
         [
           {:skip_case, {:conf, _Ref, {fwMod, _Func}, _Cmt}, mode}
           | cases
         ],
         set,
         fwMod
       ) do
    set1 =
      case :proplists.get_value(
             :suite,
             get_props(mode)
           ) do
        :undefined ->
          set

        mod ->
          :sets.add_element(mod, set)
      end

    html_isolate_modules(cases, set1, fwMod)
  end

  defp html_isolate_modules(
         [
           {:skip_case, {:conf, _Ref, {mod, _Func}, _Cmt}, _Props}
           | cases
         ],
         set,
         fwMod
       ) do
    html_isolate_modules(cases, :sets.add_element(mod, set), fwMod)
  end

  defp html_isolate_modules([{mod, _Case} | cases], set, fwMod) do
    html_isolate_modules(cases, :sets.add_element(mod, set), fwMod)
  end

  defp html_isolate_modules([{mod, _Case, _Args} | cases], set, fwMod) do
    html_isolate_modules(cases, :sets.add_element(mod, set), fwMod)
  end

  defp html_convert_modules([mod | mods]) do
    case :code.which(mod) do
      path when is_list(path) ->
        srcFile = :filename.rootname(path) ++ '.erl'

        foundSrcFile =
          case :file.read_file_info(srcFile) do
            {:ok, sInfo} ->
              {srcFile, sInfo}

            {:error, _} ->
              modInfo = mod.module_info(:compile)

              case :proplists.get_value(:source, modInfo) do
                :undefined ->
                  :undefined

                otherSrcFile ->
                  case :file.read_file_info(otherSrcFile) do
                    {:ok, sInfo} ->
                      {otherSrcFile, sInfo}

                    {:error, _} ->
                      :undefined
                  end
              end
          end

        case foundSrcFile do
          :undefined ->
            html_convert_modules(mods)

          {srcFile1, srcFileInfo} ->
            destDir = :erlang.get(:test_server_dir)
            name = :erlang.atom_to_list(mod)
            destFile = :filename.join(destDir, downcase(name) ++ '.src.html')
            _ = html_possibly_convert(srcFile1, srcFileInfo, destFile)
            html_convert_modules(mods)
        end

      _Other ->
        html_convert_modules(mods)
    end
  end

  defp html_convert_modules([]) do
    :ok
  end

  defp html_possibly_convert(src, srcInfo, dest) do
    case :file.read_file_info(dest) do
      {:ok, destInfo}
      when r_file_info(destInfo, :mtime) >= r_file_info(srcInfo, :mtime) ->
        :ok

      _ ->
        inclPath =
          case :application.get_env(
                 :test_server,
                 :include
               ) do
            {:ok, incls} ->
              incls

            _ ->
              []
          end

        outDir = :erlang.get(:test_server_log_dir_base)

        case :test_server_sup.framework_call(
               :get_html_wrapper,
               ['Module ' ++ src, false, outDir, :undefined, encoding(src)],
               ''
             ) do
          empty when empty == '' or :erlang.element(2, empty) == '' ->
            :erl2html2.convert(src, dest, inclPath)

          {_, header, _} ->
            :erl2html2.convert(src, dest, inclPath, header)
        end
    end
  end

  defp copy_html_files(inDir, outDir) do
    files = :filelib.wildcard(:filename.join(inDir, '*' ++ '.src.html'))

    :lists.foreach(
      fn src ->
        copy_html_file(src, outDir)
      end,
      files
    )
  end

  defp copy_html_file(src, destDir) do
    dest = :filename.join(destDir, :filename.basename(src))

    case :file.read_file(src) do
      {:ok, bin} ->
        :ok = write_binary_file(dest, bin)

      {:error, _Reason} ->
        :io.format('File ~ts: read failed\n', [src])
    end
  end

  defp add_init_and_end_per_suite([{:make, _, _} = case__ | cases], lastMod, lastRef, fwMod) do
    [case__ | add_init_and_end_per_suite(cases, lastMod, lastRef, fwMod)]
  end

  defp add_init_and_end_per_suite(
         [
           {:skip_case, {{mod, :all}, _}, _} = case__
           | cases
         ],
         lastMod,
         lastRef,
         fwMod
       )
       when mod !== lastMod do
    {preCases, nextMod, nextRef} = do_add_end_per_suite_and_skip(lastMod, lastRef, mod, fwMod)
    preCases ++ [case__ | add_init_and_end_per_suite(cases, nextMod, nextRef, fwMod)]
  end

  defp add_init_and_end_per_suite(
         [
           {:skip_case, {{mod, _}, _Cmt}, _Mode} = case__
           | cases
         ],
         lastMod,
         lastRef,
         fwMod
       )
       when mod !== lastMod do
    {preCases, nextMod, nextRef} = do_add_init_and_end_per_suite(lastMod, lastRef, mod, fwMod)
    preCases ++ [case__ | add_init_and_end_per_suite(cases, nextMod, nextRef, fwMod)]
  end

  defp add_init_and_end_per_suite(
         [
           {:skip_case, {:conf, _, {mod, _}, _}, _} = case__
           | cases
         ],
         lastMod,
         lastRef,
         fwMod
       )
       when mod !== lastMod do
    {preCases, nextMod, nextRef} = do_add_init_and_end_per_suite(lastMod, lastRef, mod, fwMod)
    preCases ++ [case__ | add_init_and_end_per_suite(cases, nextMod, nextRef, fwMod)]
  end

  defp add_init_and_end_per_suite(
         [
           {:skip_case, {:conf, _, {mod, _}, _}} = case__
           | cases
         ],
         lastMod,
         lastRef,
         fwMod
       )
       when mod !== lastMod do
    {preCases, nextMod, nextRef} = do_add_init_and_end_per_suite(lastMod, lastRef, mod, fwMod)
    preCases ++ [case__ | add_init_and_end_per_suite(cases, nextMod, nextRef, fwMod)]
  end

  defp add_init_and_end_per_suite(
         [
           {:conf, ref, props, {fwMod, func}} = case__
           | cases
         ],
         lastMod,
         lastRef,
         fwMod
       ) do
    case :proplists.get_value(:suite, props) do
      suite when suite !== :undefined and suite !== lastMod ->
        {preCases, nextMod, nextRef} =
          do_add_init_and_end_per_suite(lastMod, lastRef, suite, fwMod)

        case1 =
          {:conf, ref, [{:suite, nextMod} | :proplists.delete(:suite, props)], {fwMod, func}}

        preCases ++ [case1 | add_init_and_end_per_suite(cases, nextMod, nextRef, fwMod)]

      _ ->
        [case__ | add_init_and_end_per_suite(cases, lastMod, lastRef, fwMod)]
    end
  end

  defp add_init_and_end_per_suite(
         [{:conf, _, _, {mod, _}} = case__ | cases],
         lastMod,
         lastRef,
         fwMod
       )
       when mod !== lastMod and mod !== fwMod do
    {preCases, nextMod, nextRef} = do_add_init_and_end_per_suite(lastMod, lastRef, mod, fwMod)
    preCases ++ [case__ | add_init_and_end_per_suite(cases, nextMod, nextRef, fwMod)]
  end

  defp add_init_and_end_per_suite([skipCase | cases], lastMod, lastRef, fwMod)
       when :erlang.element(1, skipCase) == :skip_case or
              :erlang.element(1, skipCase) == :auto_skip_case do
    [skipCase | add_init_and_end_per_suite(cases, lastMod, lastRef, fwMod)]
  end

  defp add_init_and_end_per_suite([{:conf, _, _, _} = case__ | cases], lastMod, lastRef, fwMod) do
    [case__ | add_init_and_end_per_suite(cases, lastMod, lastRef, fwMod)]
  end

  defp add_init_and_end_per_suite(
         [{:repeat, {mod, _}, _} = case__ | cases],
         lastMod,
         lastRef,
         fwMod
       )
       when mod !== lastMod and mod !== fwMod do
    {preCases, nextMod, nextRef} = do_add_init_and_end_per_suite(lastMod, lastRef, mod, fwMod)
    preCases ++ [case__ | add_init_and_end_per_suite(cases, nextMod, nextRef, fwMod)]
  end

  defp add_init_and_end_per_suite([{:repeat, _, _} = case__ | cases], lastMod, lastRef, fwMod) do
    [case__ | add_init_and_end_per_suite(cases, lastMod, lastRef, fwMod)]
  end

  defp add_init_and_end_per_suite([{mod, _} = case__ | cases], lastMod, lastRef, fwMod)
       when mod !== lastMod and mod !== fwMod do
    {preCases, nextMod, nextRef} = do_add_init_and_end_per_suite(lastMod, lastRef, mod, fwMod)
    preCases ++ [case__ | add_init_and_end_per_suite(cases, nextMod, nextRef, fwMod)]
  end

  defp add_init_and_end_per_suite([{mod, _, _} = case__ | cases], lastMod, lastRef, fwMod)
       when mod !== lastMod and mod !== fwMod do
    {preCases, nextMod, nextRef} = do_add_init_and_end_per_suite(lastMod, lastRef, mod, fwMod)
    preCases ++ [case__ | add_init_and_end_per_suite(cases, nextMod, nextRef, fwMod)]
  end

  defp add_init_and_end_per_suite([case__ | cases], lastMod, lastRef, fwMod) do
    [case__ | add_init_and_end_per_suite(cases, lastMod, lastRef, fwMod)]
  end

  defp add_init_and_end_per_suite([], _LastMod, :undefined, _FwMod) do
    []
  end

  defp add_init_and_end_per_suite([], _LastMod, :skipped_suite, _FwMod) do
    []
  end

  defp add_init_and_end_per_suite([], lastMod, lastRef, fwMod) do
    case {:erlang.function_exported(lastMod, :end_per_suite, 1),
          :erlang.function_exported(lastMod, :init_per_suite, 1)} do
      {false, false} ->
        case :erlang.function_exported(fwMod, :end_per_suite, 1) do
          true ->
            [{:conf, lastRef, [{:suite, lastMod}], {fwMod, :end_per_suite}}]

          false ->
            [{:conf, lastRef, [], {lastMod, :end_per_suite}}]
        end

      _ ->
        [{:conf, lastRef, [], {lastMod, :end_per_suite}}]
    end
  end

  defp do_add_init_and_end_per_suite(lastMod, lastRef, mod, fwMod) do
    _ =
      case :code.is_loaded(mod) do
        false ->
          :code.load_file(mod)

        _ ->
          :ok
      end

    {init, nextMod, nextRef} =
      case {:erlang.function_exported(mod, :init_per_suite, 1),
            :erlang.function_exported(mod, :end_per_suite, 1)} do
        {false, false} ->
          case :erlang.function_exported(fwMod, :init_per_suite, 1) do
            true ->
              ref = make_ref()
              {[{:conf, ref, [{:suite, mod}], {fwMod, :init_per_suite}}], mod, ref}

            false ->
              {[], mod, :undefined}
          end

        _ ->
          ref = make_ref()
          {[{:conf, ref, [], {mod, :init_per_suite}}], mod, ref}
      end

    cases =
      cond do
        lastRef == :undefined ->
          init

        lastRef == :skipped_suite ->
          init

        true ->
          case {:erlang.function_exported(lastMod, :end_per_suite, 1),
                :erlang.function_exported(lastMod, :init_per_suite, 1)} do
            {false, false} ->
              case :erlang.function_exported(fwMod, :end_per_suite, 1) do
                true ->
                  [
                    {:conf, lastRef, [{:suite, lastMod}], {fwMod, :end_per_suite}}
                    | init
                  ]

                false ->
                  [
                    {:conf, lastRef, [], {lastMod, :end_per_suite}}
                    | init
                  ]
              end

            _ ->
              [{:conf, lastRef, [], {lastMod, :end_per_suite}} | init]
          end
      end

    {cases, nextMod, nextRef}
  end

  defp do_add_end_per_suite_and_skip(lastMod, lastRef, mod, fwMod) do
    case lastRef do
      no when no == :undefined or no == :skipped_suite ->
        {[], mod, :skipped_suite}

      _Ref ->
        case {:erlang.function_exported(lastMod, :end_per_suite, 1),
              :erlang.function_exported(lastMod, :init_per_suite, 1)} do
          {false, false} ->
            case :erlang.function_exported(fwMod, :end_per_suite, 1) do
              true ->
                {[{:conf, lastRef, [], {fwMod, :end_per_suite}}], mod, :skipped_suite}

              false ->
                {[{:conf, lastRef, [], {lastMod, :end_per_suite}}], mod, :skipped_suite}
            end

          _ ->
            {[{:conf, lastRef, [], {lastMod, :end_per_suite}}], mod, :skipped_suite}
        end
    end
  end

  defp run_test_cases(testSpec, config, timetrapData) do
    :test_server.init_valgrind()

    case :lists.member(
           :no_src,
           :erlang.get(:test_server_logopts)
         ) do
      true ->
        :ok

      false ->
        fwMod = get_fw_mod(:test_server_ctrl)
        html_convert_modules(testSpec, config, fwMod)
    end

    run_test_cases_loop(testSpec, [config], timetrapData, [], [])

    {allSkippedN, userSkipN, autoSkipN, skipStr} =
      case :erlang.get(:test_server_skipped) do
        {0, 0} ->
          {0, 0, 0, ''}

        {uS, aS} ->
          {uS + aS, uS, aS, :io_lib.format(', ~w skipped', [uS + aS])}
      end

    okN = :erlang.get(:test_server_ok)
    failedN = :erlang.get(:test_server_failed)

    print(1, 'TEST COMPLETE, ~w ok, ~w failed~ts of ~w test cases\n', [
      okN,
      failedN,
      skipStr,
      okN + failedN + allSkippedN
    ])

    :test_server_sup.framework_call(
      :report,
      [:tests_done, {okN, failedN, {userSkipN, autoSkipN}}]
    )

    print(:major, '=finished      ~s', [:lists.flatten(timestamp_get(''))])
    print(:major, '=failed        ~w', [failedN])
    print(:major, '=successful    ~w', [okN])
    print(:major, '=user_skipped  ~w', [userSkipN])
    print(:major, '=auto_skipped  ~w', [autoSkipN])
    exit(:test_suites_done)
  end

  defp run_test_cases_loop(
         [
           {skipTag, caseData = {type, _Ref, _Case, _Comment}}
           | cases
         ],
         config,
         timetrapData,
         mode,
         status
       )
       when (skipTag == :auto_skip_case or skipTag == :skip_case) and
              (type == :conf or type == :make) do
    run_test_cases_loop([{skipTag, caseData, mode} | cases], config, timetrapData, mode, status)
  end

  defp run_test_cases_loop(
         [
           {skipTag, {type, ref, case__, comment}, skipMode}
           | cases
         ],
         config,
         timetrapData,
         mode,
         status
       )
       when (skipTag == :auto_skip_case or skipTag == :skip_case) and
              (type == :conf or type == :make) do
    :ok = :file.set_cwd(:filename.dirname(:erlang.get(:test_server_dir)))
    currIOHandler = :erlang.get(:test_server_common_io_handler)
    parentMode = tl(mode)

    {autoOrUser, reportTag} =
      cond do
        skipTag == :auto_skip_case ->
          {:auto, :tc_auto_skip}

        skipTag == :skip_case ->
          {:user, :tc_user_skip}
      end

    case {curr_ref(mode), check_props(:parallel, mode)} do
      {^ref, ^ref} ->
        case check_props(:parallel, parentMode) do
          false ->
            _ = handle_test_case_io_and_status()
            set_io_buffering(:undefined)
            {mod, func} = skip_case(autoOrUser, ref, 0, case__, comment, false, skipMode)
            confData = {mod, {func, get_name(skipMode)}, comment}

            :test_server_sup.framework_call(
              :report,
              [reportTag, confData]
            )

            run_test_cases_loop(
              cases,
              config,
              timetrapData,
              parentMode,
              delete_status(ref, status)
            )

          _ ->
            _ = wait_for_cases(ref)
            {mod, func} = skip_case(autoOrUser, ref, 0, case__, comment, true, skipMode)
            confData = {mod, {func, get_name(skipMode)}, comment}

            :test_server_sup.framework_call(
              :report,
              [reportTag, confData]
            )

            case currIOHandler do
              {^ref, _} ->
                set_io_buffering(:undefined)

              _ ->
                :ok
            end

            run_test_cases_loop(
              cases,
              config,
              timetrapData,
              parentMode,
              delete_status(ref, status)
            )
        end

      {^ref, false} ->
        {mod, func} = skip_case(autoOrUser, ref, 0, case__, comment, false, skipMode)
        confData = {mod, {func, get_name(skipMode)}, comment}

        :test_server_sup.framework_call(
          :report,
          [reportTag, confData]
        )

        grName = get_name(mode)

        cases1 =
          case get_tc_results(status) do
            {_, _, fails} when length(fails) > 0 ->
              case :lists.member({:group_result, grName}, fails) do
                true ->
                  case check_prop(:sequence, parentMode) do
                    false ->
                      cases

                    parentRef ->
                      reason = {:group_result, grName, :failed}
                      skip_cases_upto(parentRef, cases, reason, :tc, parentMode, skipTag)
                  end

                false ->
                  cases
              end

            _ ->
              cases
          end

        run_test_cases_loop(cases1, config, timetrapData, parentMode, delete_status(ref, status))

      {^ref, _} ->
        {mod, func} = skip_case(autoOrUser, ref, 0, case__, comment, true, skipMode)
        confData = {mod, {func, get_name(skipMode)}, comment}

        :test_server_sup.framework_call(
          :report,
          [reportTag, confData]
        )

        case currIOHandler do
          {^ref, _} ->
            set_io_buffering(:undefined)

          _ ->
            :ok
        end

        run_test_cases_loop(cases, config, timetrapData, tl(mode), delete_status(ref, status))

      {_, false} ->
        {mod, func} = skip_case(autoOrUser, ref, 0, case__, comment, false, skipMode)
        confData = {mod, {func, get_name(skipMode)}, comment}

        :test_server_sup.framework_call(
          :report,
          [reportTag, confData]
        )

        run_test_cases_loop(cases, config, timetrapData, [conf(ref, []) | mode], status)

      {_, ref0} when is_reference(ref0) ->
        cond do
          currIOHandler == :undefined ->
            set_io_buffering({ref, self()})

          true ->
            :ok
        end

        {mod, func} = skip_case(autoOrUser, ref, 0, case__, comment, true, skipMode)
        confData = {mod, {func, get_name(skipMode)}, comment}

        :test_server_sup.framework_call(
          :report,
          [reportTag, confData]
        )

        run_test_cases_loop(cases, config, timetrapData, [conf(ref, []) | mode], status)
    end
  end

  defp run_test_cases_loop(
         [
           {:auto_skip_case, {case__, comment}, skipMode}
           | cases
         ],
         config,
         timetrapData,
         mode,
         status
       ) do
    {mod, func} =
      skip_case(
        :auto,
        :undefined,
        :erlang.get(:test_server_case_num) + 1,
        case__,
        comment,
        is_io_buffered(),
        skipMode
      )

    :test_server_sup.framework_call(
      :report,
      [:tc_auto_skip, {mod, {func, get_name(skipMode)}, comment}]
    )

    run_test_cases_loop(
      cases,
      config,
      timetrapData,
      mode,
      update_status(:skipped, mod, func, status)
    )
  end

  defp run_test_cases_loop(
         [
           {:skip_case, {{mod, :all} = case__, comment}, skipMode}
           | cases
         ],
         config,
         timetrapData,
         mode,
         status
       ) do
    _ = skip_case(:user, :undefined, 0, case__, comment, false, skipMode)

    :test_server_sup.framework_call(
      :report,
      [:tc_user_skip, {mod, {:all, get_name(skipMode)}, comment}]
    )

    run_test_cases_loop(cases, config, timetrapData, mode, status)
  end

  defp run_test_cases_loop(
         [
           {:skip_case, {case__, comment}, skipMode}
           | cases
         ],
         config,
         timetrapData,
         mode,
         status
       ) do
    {mod, func} =
      skip_case(
        :user,
        :undefined,
        :erlang.get(:test_server_case_num) + 1,
        case__,
        comment,
        is_io_buffered(),
        skipMode
      )

    :test_server_sup.framework_call(
      :report,
      [:tc_user_skip, {mod, {func, get_name(skipMode)}, comment}]
    )

    run_test_cases_loop(
      cases,
      config,
      timetrapData,
      mode,
      update_status(:skipped, mod, func, status)
    )
  end

  defp run_test_cases_loop(
         [
           {:conf, ref, props, {mod, func}}
           | _Cases
         ] = cs0,
         config,
         timetrapData,
         mode0,
         status
       ) do
    currIOHandler = :erlang.get(:test_server_common_io_handler)

    {startConf, mode, iOHandler, confTime, status1} =
      case {curr_ref(mode0), check_props(:parallel, mode0)} do
        {^ref, ^ref} ->
          case check_props(:parallel, tl(mode0)) do
            false ->
              okSkipFail = handle_test_case_io_and_status()
              :ok = :file.set_cwd(:filename.dirname(:erlang.get(:test_server_dir)))
              after__ = :os.timestamp()
              before = :erlang.get(:test_server_parallel_start_time)
              elapsed = :timer.now_diff(after__, before) / 1_000_000
              :erlang.put(:test_server_total_time, elapsed)
              {false, tl(mode0), :undefined, elapsed, update_status(ref, okSkipFail, status)}

            _ ->
              okSkipFail = wait_for_cases(ref)
              queue_test_case_io(ref, self(), 0, mod, func)

              elapsed =
                :timer.now_diff(
                  :os.timestamp(),
                  conf_start(
                    ref,
                    mode0
                  )
                ) / 1_000_000

              case currIOHandler do
                {^ref, _} ->
                  {false, tl(mode0), :undefined, elapsed, update_status(ref, okSkipFail, status)}

                _ ->
                  {false, tl(mode0), currIOHandler, elapsed,
                   update_status(ref, okSkipFail, status)}
              end
          end

        {^ref, false} ->
          {false, tl(mode0), :undefined,
           :timer.now_diff(
             :os.timestamp(),
             conf_start(ref, mode0)
           ) / 1_000_000, status}

        {^ref, _} ->
          queue_test_case_io(ref, self(), 0, mod, func)

          elapsed =
            :timer.now_diff(
              :os.timestamp(),
              conf_start(
                ref,
                mode0
              )
            ) / 1_000_000

          case currIOHandler do
            {^ref, _} ->
              {false, tl(mode0), :undefined, elapsed, status}

            _ ->
              {false, tl(mode0), currIOHandler, elapsed, status}
          end

        {_, false} ->
          case :lists.member(:parallel, props) do
            true ->
              :erlang.put(
                :test_server_parallel_start_time,
                :os.timestamp()
              )

              :erlang.put(:test_server_queued_io, [])

            false ->
              :ok
          end

          {true, [conf(ref, props) | mode0], :undefined, 0, status}

        {_, _Ref0} ->
          queue_test_case_io(ref, self(), 0, mod, func)

          iOHandler1 =
            cond do
              currIOHandler == :undefined ->
                iOH = {ref, self()}
                set_io_buffering(iOH)
                iOH

              true ->
                currIOHandler
            end

          {true, [conf(ref, props) | mode0], iOHandler1, 0, status}
      end

    {[_Conf | cases1] = cs1, shuffle} =
      cond do
        startConf ->
          case get_shuffle(props) do
            :undefined ->
              {cs0, :undefined}

            {_, :repeated} ->
              currSeed = :erlang.get(:test_server_curr_random_seed)
              {shuffle_cases(ref, cs0, currSeed), {:shuffle, currSeed}}

            {_, seed} ->
              useSeed =
                case :os.getenv('TS_RANDOM_SEED') do
                  undef
                  when undef == false or
                         undef == 'undefined' ->
                    case :erlang.get(:test_server_random_seed) do
                      :undefined ->
                        seed

                      tSRS ->
                        tSRS
                    end

                  numStr ->
                    :erlang.list_to_tuple(
                      for nS <-
                            :string.lexemes(
                              numStr,
                              [?\s, ?:, ?,]
                            ) do
                        :erlang.list_to_integer(nS)
                      end
                    )
                end

              {shuffle_cases(ref, cs0, useSeed), {:shuffle, useSeed}}
          end

        not startConf ->
          {cs0, :undefined}
      end

    {repeating, status2, cases, reportRepeatStop} =
      cond do
        startConf ->
          case get_repeat(props) do
            :undefined ->
              {false,
               new_status(
                 ref,
                 status1
               ), cases1,
               fn ->
                 :ok
               end}

            {_RepType, n}
            when n <= 1 ->
              {false,
               new_status(
                 ref,
                 status1
               ), cases1,
               fn ->
                 :ok
               end}

            _ ->
              {copied, _} =
                copy_cases(
                  ref,
                  make_ref(),
                  cs1
                )

              {true,
               new_status(
                 ref,
                 copied,
                 status1
               ), cases1,
               fn ->
                 :ok
               end}
          end

        not startConf ->
          repVal = get_repeat(get_props(mode0))

          reportStop = fn ->
            print(
              :minor,
              '~n*** Stopping repeat operation ~w',
              [repVal]
            )

            print(
              1,
              'Stopping repeat operation ~w',
              [repVal]
            )
          end

          copiedCases = get_copied_cases(status1)

          endStatus =
            delete_status(
              ref,
              status1
            )

          case repVal do
            :undefined ->
              {false, endStatus, cases1,
               fn ->
                 :ok
               end}

            {_RepType, n}
            when n <= 1 ->
              {false, endStatus, cases1,
               fn ->
                 :ok
               end}

            {:repeat, _} ->
              {true, endStatus, copiedCases ++ cases1,
               fn ->
                 :ok
               end}

            {:repeat_until_all_ok, _} ->
              {restCs, fun} =
                case get_tc_results(status1) do
                  {_, _, []} ->
                    {cases1, reportStop}

                  _ ->
                    {copiedCases ++ cases1,
                     fn ->
                       :ok
                     end}
                end

              {true, endStatus, restCs, fun}

            {:repeat_until_any_ok, _} ->
              {restCs, fun} =
                case get_tc_results(status1) do
                  {ok, _, _Fails}
                  when length(ok) > 0 ->
                    {cases1, reportStop}

                  _ ->
                    {copiedCases ++ cases1,
                     fn ->
                       :ok
                     end}
                end

              {true, endStatus, restCs, fun}

            {:repeat_until_any_fail, _} ->
              {restCs, fun} =
                case get_tc_results(status1) do
                  {_, _, fails}
                  when length(fails) > 0 ->
                    {cases1, reportStop}

                  _ ->
                    {copiedCases ++ cases1,
                     fn ->
                       :ok
                     end}
                end

              {true, endStatus, restCs, fun}

            {:repeat_until_all_fail, _} ->
              {restCs, fun} =
                case get_tc_results(status1) do
                  {[], _, _} ->
                    {cases1, reportStop}

                  _ ->
                    {copiedCases ++ cases1,
                     fn ->
                       :ok
                     end}
                end

              {true, endStatus, restCs, fun}
          end
      end

    reportAbortRepeat = fn
      what when repeating ->
        print(:minor, '~n*** Aborting repeat operation (configuration case ~w)', [what])
        print(1, 'Aborting repeat operation (configuration case ~w)', [what])

      _ ->
        :ok
    end

    cfgProps =
      cond do
        startConf ->
          cond do
            shuffle == :undefined ->
              [{:tc_group_properties, props}]

            true ->
              [{:tc_group_properties, [shuffle | delete_shuffle(props)]}]
          end

        not startConf ->
          {tcOk, tcSkip, tcFail} = get_tc_results(status1)

          [
            {:tc_group_properties, get_props(mode0)},
            {:tc_group_result, [{:ok, tcOk}, {:skipped, tcSkip}, {:failed, tcFail}]}
          ]
      end

    suiteName = :proplists.get_value(:suite, props)

    case :erlang.get(:test_server_create_priv_dir) do
      :auto_per_run ->
        tSDirs = [
          {:priv_dir, :erlang.get(:test_server_priv_dir)},
          {:data_dir, get_data_dir(mod, suiteName)}
        ]

      _ ->
        tSDirs = [{:data_dir, get_data_dir(mod, suiteName)}]
    end

    actualCfg =
      cond do
        not startConf ->
          update_config(hd(config), tSDirs ++ cfgProps)

        true ->
          groupPath =
            :lists.flatmap(
              fn
                {_Ref, [], _T} ->
                  []

                {_Ref, grProps, _T} ->
                  [grProps]
              end,
              mode0
            )

          update_config(
            hd(config),
            tSDirs ++
              [
                {:tc_group_path, groupPath}
                | cfgProps
              ]
          )
      end

    currMode = curr_mode(ref, mode0, mode)

    confCaseResult =
      run_test_case(ref, 0, mod, func, [actualCfg], :skip_init, timetrapData, currMode)

    case confCaseResult do
      {_, newCfg, _}
      when func == :init_per_suite and
             is_list(newCfg) ->
        case :lists.filter(
               fn
                 {_, _} ->
                   false

                 _ ->
                   true
               end,
               newCfg
             ) do
          [] ->
            set_io_buffering(iOHandler)
            stop_minor_log_file()
            run_test_cases_loop(cases, [newCfg | config], timetrapData, mode, status2)

          bad ->
            print(:minor, '~n*** ~tw returned bad elements in Config: ~tp.~n', [func, bad])
            reason = {:failed, {mod, :init_per_suite, :bad_return}}
            cases2 = skip_cases_upto(ref, cases, reason, :conf, currMode, :auto_skip_case)
            set_io_buffering(iOHandler)
            stop_minor_log_file()
            run_test_cases_loop(cases2, config, timetrapData, mode, delete_status(ref, status2))
        end

      {_, newCfg, _} when startConf and is_list(newCfg) ->
        print_conf_time(confTime)
        set_io_buffering(iOHandler)
        stop_minor_log_file()
        run_test_cases_loop(cases, [newCfg | config], timetrapData, mode, status2)

      {_, {:framework_error, {fwMod, fwFunc}, reason}, _} ->
        print(:minor, '~n*** ~w failed in ~tw. Reason: ~tp~n', [fwMod, fwFunc, reason])
        print(1, '~w failed in ~tw. Reason: ~tp~n', [fwMod, fwFunc, reason])
        exit(:framework_error)

      {_, fail, _}
      when :erlang.element(1, fail) == :EXIT or
             :erlang.element(1, fail) == :timetrap_timeout or
             :erlang.element(1, fail) == :user_timetrap_error or
             :erlang.element(1, fail) == :failed ->
        {cases2, config1, status3} =
          cond do
            startConf ->
              reportAbortRepeat.(:failed)
              print(:minor, '~n*** ~tw failed.~n    Skipping all cases.', [func])
              reason = {:failed, {mod, func, fail}}

              {skip_cases_upto(ref, cases, reason, :conf, currMode, :auto_skip_case), config,
               update_status(
                 :failed,
                 :group_result,
                 get_name(mode),
                 delete_status(
                   ref,
                   status2
                 )
               )}

            not startConf ->
              reportRepeatStop.()
              print_conf_time(confTime)
              {cases, tl(config), delete_status(ref, status2)}
          end

        set_io_buffering(iOHandler)
        stop_minor_log_file()
        run_test_cases_loop(cases2, config1, timetrapData, mode, status3)

      {_, {:auto_skip, skipReason}, _} ->
        {cases2, config1, status3} =
          cond do
            startConf ->
              reportAbortRepeat.(:auto_skipped)
              print(:minor, '~n*** ~tw auto skipped.~n    Skipping all cases.', [func])

              {skip_cases_upto(ref, cases, skipReason, :conf, currMode, :auto_skip_case), config,
               delete_status(ref, status2)}

            not startConf ->
              reportRepeatStop.()
              print_conf_time(confTime)
              {cases, tl(config), delete_status(ref, status2)}
          end

        set_io_buffering(iOHandler)
        stop_minor_log_file()
        run_test_cases_loop(cases2, config1, timetrapData, mode, status3)

      {_, {skip, reason}, _}
      when startConf and (skip == :skip or skip == :skipped) ->
        reportAbortRepeat.(:skipped)
        print(:minor, '~n*** ~tw skipped.~n    Skipping all cases.', [func])
        set_io_buffering(iOHandler)
        stop_minor_log_file()

        run_test_cases_loop(
          skip_cases_upto(ref, cases, reason, :conf, currMode, :skip_case),
          [hd(config) | config],
          timetrapData,
          mode,
          delete_status(ref, status2)
        )

      {_, {:skip_and_save, reason, _SavedConfig}, _}
      when startConf ->
        reportAbortRepeat.(:skipped)
        print(:minor, '~n*** ~tw skipped.~n    Skipping all cases.', [func])
        set_io_buffering(iOHandler)
        stop_minor_log_file()

        run_test_cases_loop(
          skip_cases_upto(ref, cases, reason, :conf, currMode, :skip_case),
          [hd(config) | config],
          timetrapData,
          mode,
          delete_status(ref, status2)
        )

      {_, _Other, _} when func == :init_per_suite ->
        print(:minor, '~n*** init_per_suite failed to return a Config list.~n', [])
        reason = {:failed, {mod, :init_per_suite, :bad_return}}
        cases2 = skip_cases_upto(ref, cases, reason, :conf, currMode, :auto_skip_case)
        set_io_buffering(iOHandler)
        stop_minor_log_file()
        run_test_cases_loop(cases2, config, timetrapData, mode, delete_status(ref, status2))

      {_, _Other, _} when startConf ->
        print_conf_time(confTime)
        set_io_buffering(iOHandler)
        reportRepeatStop.()
        stop_minor_log_file()
        run_test_cases_loop(cases, [hd(config) | config], timetrapData, mode, status2)

      {_, _EndConfRetVal, opts} ->
        grName = get_name(mode0, func)

        {cases2, status3} =
          case :lists.keysearch(:return_group_result, 1, opts) do
            {:value, {_, :failed}} ->
              case {curr_ref(mode), check_prop(:sequence, mode)} do
                {parentRef, parentRef} ->
                  reason = {:group_result, grName, :failed}

                  {skip_cases_upto(parentRef, cases, reason, :tc, mode, :auto_skip_case),
                   update_status(:failed, :group_result, grName, delete_status(ref, status2))}

                _ ->
                  {cases,
                   update_status(:failed, :group_result, grName, delete_status(ref, status2))}
              end

            {:value, {_, groupResult}} ->
              {cases,
               update_status(groupResult, :group_result, grName, delete_status(ref, status2))}

            false ->
              {cases, update_status(:ok, :group_result, grName, delete_status(ref, status2))}
          end

        print_conf_time(confTime)
        reportRepeatStop.()
        set_io_buffering(iOHandler)
        stop_minor_log_file()
        run_test_cases_loop(cases2, tl(config), timetrapData, mode, status3)
    end
  end

  defp run_test_cases_loop(
         [{:make, ref, {mod, func, args}} | cases0],
         config,
         timetrapData,
         mode,
         status
       ) do
    case run_test_case(ref, 0, mod, func, args, :skip_init, timetrapData) do
      {_, why = {:EXIT, _}, _} ->
        print(:minor, '~n*** ~tw failed.~n    Skipping all cases.', [func])
        reason = {:failed, {mod, func, why}}
        cases = skip_cases_upto(ref, cases0, reason, :conf, mode, :auto_skip_case)
        stop_minor_log_file()
        run_test_cases_loop(cases, config, timetrapData, mode, status)

      {_, _Whatever, _} ->
        stop_minor_log_file()
        run_test_cases_loop(cases0, config, timetrapData, mode, status)
    end
  end

  defp run_test_cases_loop(
         [{:conf, _Ref, _Props, _X} = conf | _Cases0],
         config,
         _TimetrapData,
         _Mode,
         _Status
       ) do
    :erlang.error(:badarg, [conf, config])
  end

  defp run_test_cases_loop(
         [{:repeat, case__, {repeatType, n}} | cases0],
         config,
         timeTrapData,
         mode,
         status
       ) do
    ref = make_ref()
    parallel = check_prop(:parallel, mode) !== false
    sequence = check_prop(:sequence, mode) !== false
    repeatStop = repeatType === :repeat_until_fail or repeatType === :repeat_until_ok

    cond do
      parallel and repeatStop ->
        :erlang.error({:illegal_combination, {:parallel, repeatType}})

      sequence and repeatStop ->
        :erlang.error({:illegal_combination, {:sequence, repeatType}})

      true ->
        mode1 = [
          {ref, [{:repeat, {repeatType, 1, n}}], :os.timestamp()}
          | mode
        ]

        run_test_cases_loop([case__ | cases0], config, timeTrapData, mode1, status)
    end
  end

  defp run_test_cases_loop([{mod, case__} | cases], config, timetrapData, mode, status) do
    actualCfg =
      case :erlang.get(:test_server_create_priv_dir) do
        :auto_per_run ->
          update_config(
            hd(config),
            [{:priv_dir, :erlang.get(:test_server_priv_dir)}, {:data_dir, get_data_dir(mod)}]
          )

        _ ->
          update_config(
            hd(config),
            [{:data_dir, get_data_dir(mod)}]
          )
      end

    run_test_cases_loop(
      [
        {mod, case__, [actualCfg]}
        | cases
      ],
      config,
      timetrapData,
      mode,
      status
    )
  end

  defp run_test_cases_loop(
         [{mod, func, args} = case__ | cases],
         config,
         timetrapData,
         mode0,
         status
       ) do
    {num, runInit} =
      case fwMod = get_fw_mod(:test_server_ctrl) do
        ^mod when func == :error_in_suite ->
          {-1, :skip_init}

        _ ->
          {:erlang.put(
             :test_server_case_num,
             :erlang.get(:test_server_case_num) + 1
           ), :run_init}
      end

    mode =
      case mode0 do
        [{_, [{:repeat, {_, _, _}}], _} | restMode] ->
          restMode

        _ ->
          mode0
      end

    case check_prop(
           :parallel,
           mode
         ) === false and is_io_buffered() do
      true ->
        queue_test_case_io(:undefined, self(), num + 1, mod, func)

      false ->
        :ok
    end

    case run_test_case(:undefined, num + 1, mod, func, args, runInit, timetrapData, mode) do
      {_, {:framework_error, {^fwMod, fwFunc}, reason}, _} ->
        print(:minor, '~n*** ~w failed in ~tw. Reason: ~tp~n', [fwMod, fwFunc, reason])
        print(1, '~w failed in ~tw. Reason: ~tp~n', [fwMod, fwFunc, reason])
        stop_minor_log_file()
        exit(:framework_error)

      {time, retVal, _} ->
        retTag =
          cond do
            is_tuple(retVal) ->
              :erlang.element(1, retVal)

            true ->
              :undefined
          end

        {result, failed, status1} =
          case retTag do
            skip
            when skip == :skip or
                   skip == :skipped ->
              {:skipped, false, update_status(:skipped, mod, func, status)}

            fail
            when fail == :EXIT or
                   fail == :failed ->
              {:failed, true, update_status(:failed, mod, func, status)}

            _ when time == :died and retVal !== :ok ->
              {:failed, true, update_status(:failed, mod, func, status)}

            _ ->
              {:ok, false, update_status(:ok, mod, func, status)}
          end

        case check_prop(:sequence, mode) do
          false ->
            {cases1, mode1} = check_repeat_testcase(case__, result, cases, mode0)
            stop_minor_log_file()
            run_test_cases_loop(cases1, config, timetrapData, mode1, status1)

          ref ->
            cond do
              not failed ->
                {cases1, mode1} = check_repeat_testcase(case__, result, cases, mode0)
                stop_minor_log_file()
                run_test_cases_loop(cases1, config, timetrapData, mode1, status1)

              true ->
                print(:minor, '~n*** ~tw failed.~n    Skipping all other cases in sequence.', [
                  func
                ])

                {cases1, mode1} = check_repeat_testcase(case__, result, cases, mode0)
                reason = {:failed, {mod, func}}
                cases2 = skip_cases_upto(ref, cases1, reason, :tc, mode, :auto_skip_case)
                stop_minor_log_file()
                run_test_cases_loop(cases2, config, timetrapData, mode1, status1)
            end
        end

      pid ->
        queue_test_case_io(:undefined, pid, num + 1, mod, func)
        {cases1, mode1} = check_repeat_testcase(case__, :ok, cases, mode0)
        run_test_cases_loop(cases1, config, timetrapData, mode1, status)
    end
  end

  defp run_test_cases_loop([], _Config, _TimetrapData, _, _) do
    :ok
  end

  defp new_status(ref, status) do
    [{ref, {{[], [], []}, []}} | status]
  end

  defp new_status(ref, copiedCases, status) do
    [{ref, {{[], [], []}, copiedCases}} | status]
  end

  defp delete_status(ref, status) do
    :lists.keydelete(ref, 1, status)
  end

  defp update_status(:ok, mod, func, [{ref, {{ok, skip, fail}, cs}} | status]) do
    [
      {ref, {{ok ++ [{mod, func}], skip, fail}, cs}}
      | status
    ]
  end

  defp update_status(:skipped, mod, func, [{ref, {{ok, skip, fail}, cs}} | status]) do
    [
      {ref, {{ok, skip ++ [{mod, func}], fail}, cs}}
      | status
    ]
  end

  defp update_status(:failed, mod, func, [{ref, {{ok, skip, fail}, cs}} | status]) do
    [
      {ref, {{ok, skip, fail ++ [{mod, func}]}, cs}}
      | status
    ]
  end

  defp update_status(_, _, _, []) do
    []
  end

  defp update_status(ref, {ok, skip, fail}, [{ref, {{ok0, skip0, fail0}, cs}} | status]) do
    [
      {ref, {{ok0 ++ ok, skip0 ++ skip, fail0 ++ fail}, cs}}
      | status
    ]
  end

  defp get_copied_cases([{_, {_, cases}} | _Status]) do
    cases
  end

  defp get_tc_results([{_, {okSkipFail, _}} | _Status]) do
    okSkipFail
  end

  defp get_tc_results([]) do
    {[], [], []}
  end

  defp conf(ref, props) do
    {ref, props, :os.timestamp()}
  end

  defp curr_ref([{ref, _Props, _} | _]) do
    ref
  end

  defp curr_ref([]) do
    :undefined
  end

  defp curr_mode(ref, mode0, mode1) do
    case curr_ref(mode1) do
      ^ref ->
        mode1

      _ ->
        mode0
    end
  end

  defp get_props([{_, props, _} | _]) do
    props
  end

  defp get_props([]) do
    []
  end

  defp check_prop(_Attrib, []) do
    false
  end

  defp check_prop(attrib, [{ref, props, _} | _]) do
    case :lists.member(attrib, props) do
      true ->
        ref

      false ->
        false
    end
  end

  defp check_props(attrib, mode) do
    case (for {r, ps, _} <- mode,
              :lists.member(attrib, ps) do
            r
          end) do
      [] ->
        false

      [ref | _] ->
        ref
    end
  end

  defp get_name(mode, def__) do
    case get_name(mode) do
      :undefined ->
        def__

      name ->
        name
    end
  end

  defp get_name([{_Ref, props, _} | _]) do
    :proplists.get_value(:name, props)
  end

  defp get_name([]) do
    :undefined
  end

  defp conf_start(ref, mode) do
    case :lists.keysearch(ref, 1, mode) do
      {:value, {_, _, t}} ->
        t

      false ->
        0
    end
  end

  defp get_data_dir(mod) do
    get_data_dir(mod, :undefined)
  end

  defp get_data_dir(mod, suite) do
    useMod =
      cond do
        suite == :undefined ->
          mod

        true ->
          suite
      end

    case :code.which(useMod) do
      :non_existing ->
        print(12, 'The module ~w is not loaded', [mod])
        []

      :cover_compiled ->
        mainCoverNode = :cover.get_main_node()
        {:file, file} = :rpc.call(mainCoverNode, :cover, :is_compiled, [useMod])
        do_get_data_dir(useMod, file)

      fullPath ->
        do_get_data_dir(useMod, fullPath)
    end
  end

  defp do_get_data_dir(mod, file) do
    :filename.dirname(file) ++ '/' ++ :erlang.atom_to_list(mod) ++ '_data/'
  end

  defp print_conf_time(0) do
    :ok
  end

  defp print_conf_time(confTime) do
    print(:major, '=group_time    ~.3fs', [confTime])
    print(:minor, '~n=== Total execution time of group: ~.3fs~n', [confTime])
  end

  defp print_props([]) do
    :ok
  end

  defp print_props(props) do
    print(:major, '=group_props   ~tp', [props])
    print(:minor, 'Group properties: ~tp~n', [props])
  end

  defp get_repeat(props) do
    get_prop(
      [
        :repeat,
        :repeat_until_all_ok,
        :repeat_until_any_ok,
        :repeat_until_any_fail,
        :repeat_until_all_fail
      ],
      :forever,
      props
    )
  end

  defp update_repeat(props) do
    case get_repeat(props) do
      :undefined ->
        props

      {repType, n} ->
        props1 =
          cond do
            n == :forever ->
              [{repType, n} | :lists.keydelete(repType, 1, props)]

            n < 3 ->
              :lists.keydelete(repType, 1, props)

            n >= 3 ->
              [{repType, n - 1} | :lists.keydelete(repType, 1, props)]
          end

        case get_shuffle(props1) do
          :undefined ->
            props1

          _ ->
            [{:shuffle, :repeated} | delete_shuffle(props1)]
        end
    end
  end

  defp get_shuffle(props) do
    get_prop([:shuffle], :os.timestamp(), props)
  end

  defp delete_shuffle(props) do
    delete_prop([:shuffle], props)
  end

  defp get_prop([item | items], default, props) do
    case :lists.keysearch(item, 1, props) do
      {:value, r} ->
        r

      false ->
        case :lists.member(item, props) do
          true ->
            {item, default}

          false ->
            get_prop(items, default, props)
        end
    end
  end

  defp get_prop([], _Def, _Props) do
    :undefined
  end

  defp delete_prop([item | items], props) do
    props1 =
      :lists.delete(
        item,
        :lists.keydelete(item, 1, props)
      )

    delete_prop(items, props1)
  end

  defp delete_prop([], props) do
    props
  end

  defp shuffle_cases(ref, cases, :undefined) do
    shuffle_cases(ref, cases, :rand.seed_s(:exsplus))
  end

  defp shuffle_cases(ref, [{:conf, ref, _, _} = start | cases], seed0) do
    {n, casesToShuffle, rest} = cases_to_shuffle(ref, cases)

    seed =
      case seed0 do
        {x, y, z} when is_integer(x + y + z) ->
          :rand.seed(:exsplus, seed0)

        _ ->
          seed0
      end

    shuffledCases = random_order(n, :rand.uniform_s(n, seed), casesToShuffle, [])
    [start | shuffledCases] ++ rest
  end

  defp cases_to_shuffle(ref, cases) do
    cases_to_shuffle(ref, cases, 1, [])
  end

  defp cases_to_shuffle(ref, [{:conf, ref, _, _} | _] = cs, n, ix) do
    {n - 1, ix, cs}
  end

  defp cases_to_shuffle(ref, [{:skip_case, {_, ref, _, _}, _} | _] = cs, n, ix) do
    {n - 1, ix, cs}
  end

  defp cases_to_shuffle(ref, [{:conf, ref1, _, _} = c | cs], n, ix) do
    {cs1, rest} = get_subcases(ref1, cs, [])
    cases_to_shuffle(ref, rest, n + 1, [{n, [c | cs1]} | ix])
  end

  defp cases_to_shuffle(ref, [{:skip_case, {_, ref1, _, _}, _} = c | cs], n, ix) do
    {cs1, rest} = get_subcases(ref1, cs, [])
    cases_to_shuffle(ref, rest, n + 1, [{n, [c | cs1]} | ix])
  end

  defp cases_to_shuffle(ref, [c | cs], n, ix) do
    cases_to_shuffle(ref, cs, n + 1, [{n, [c]} | ix])
  end

  defp get_subcases(subRef, [{:conf, subRef, _, _} = c | cs], subCs) do
    {:lists.reverse([c | subCs]), cs}
  end

  defp get_subcases(subRef, [{:skip_case, {_, subRef, _, _}, _} = c | cs], subCs) do
    {:lists.reverse([c | subCs]), cs}
  end

  defp get_subcases(subRef, [c | cs], subCs) do
    get_subcases(subRef, cs, [c | subCs])
  end

  defp random_order(1, {_Pos, seed}, [{_Ix, caseOrGroup}], shuffled) do
    :erlang.put(:test_server_curr_random_seed, seed)
    shuffled ++ caseOrGroup
  end

  defp random_order(n, {pos, newSeed}, ixCases, shuffled) do
    {first, [{_Ix, caseOrGroup} | rest]} =
      :lists.split(
        pos - 1,
        ixCases
      )

    random_order(n - 1, :rand.uniform_s(n - 1, newSeed), first ++ rest, shuffled ++ caseOrGroup)
  end

  defp skip_case(type, ref, caseNum, case__, comment, sendSync, mode) do
    mF =
      {mod, func} =
      case case__ do
        {m, f, _A} ->
          {m, f}

        {m, f} ->
          {m, f}
      end

    cond do
      sendSync ->
        queue_test_case_io(ref, self(), caseNum, mod, func)
        send(self(), {:started, ref, self(), caseNum, mod, func})
        :test_server_io.start_transaction()
        skip_case1(type, caseNum, mod, func, comment, mode)
        :test_server_io.end_transaction()
        send(self(), {:finished, ref, self(), caseNum, mod, func, :skipped, {0, :skipped, []}})

      not sendSync ->
        skip_case1(type, caseNum, mod, func, comment, mode)
    end

    mF
  end

  defp skip_case1(type, caseNum, mod, func, comment, mode) do
    {{col0, col1}, _} = get_font_style(caseNum > 0, mode)

    resultCol =
      cond do
        type == :auto ->
          '#FFA64D'

        type == :user ->
          '#FF8000'
      end

    print(:major, '~n=case          ~w:~tw', [mod, func])

    groupName =
      case get_name(mode) do
        :undefined ->
          ''

        grName ->
          grName1 = cast_to_list(grName)
          print(:major, '=group_props   ~tp', [[{:name, grName1}]])
          grName1
      end

    print(:major, '=started       ~s', [:lists.flatten(timestamp_get(''))])
    comment1 = reason_to_string(comment)

    cond do
      type == :auto ->
        print(:major, '=result        auto_skipped: ~ts', [comment1])

      type == :user ->
        print(:major, '=result        skipped: ~ts', [comment1])
    end

    cond do
      caseNum == 0 ->
        print(2, '*** Skipping ~tw ***', [{mod, func}])

      true ->
        print(2, '*** Skipping test case #~w ~tw ***', [caseNum, {mod, func}])
    end

    tR = xhtml('<tr valign="top">', '<tr class="' ++ odd_or_even() ++ '">')

    ^groupName =
      case get_name(mode) do
        :undefined ->
          ''

        name ->
          cast_to_list(name)
      end

    print(
      :html,
      tR ++
        '<td>' ++
        col0 ++
        '~ts' ++
        col1 ++
        '</td><td>' ++
        col0 ++
        '~w' ++
        col1 ++
        '</td><td>' ++
        col0 ++
        '~ts' ++
        col1 ++
        '</td><td>' ++
        col0 ++
        '~tw' ++
        col1 ++
        '</td><td>' ++
        col0 ++
        '< >' ++
        col1 ++
        '</td><td>' ++
        col0 ++
        '0.000s' ++ col1 ++ '</td><td><font color="~ts">SKIPPED</font></td><td>~ts</td></tr>\n',
      [num2str(caseNum), fw_name(mod), groupName, func, resultCol, comment1]
    )

    cond do
      caseNum > 0 ->
        {uS, aS} = :erlang.get(:test_server_skipped)

        case type do
          :user ->
            :erlang.put(:test_server_skipped, {uS + 1, aS})

          :auto ->
            :erlang.put(:test_server_skipped, {uS, aS + 1})
        end

        :erlang.put(:test_server_case_num, caseNum)

      true ->
        :ok
    end
  end

  defp skip_cases_upto(ref, cases, reason, origin, mode, skipType) do
    {_, modified, rest} =
      modify_cases_upto(
        ref,
        {:skip, reason, origin, mode, skipType},
        cases
      )

    modified ++ rest
  end

  defp copy_cases(origRef, newRef, cases) do
    {original, altered, rest} = modify_cases_upto(origRef, {:copy, newRef}, cases)
    {altered, original ++ altered ++ rest}
  end

  defp modify_cases_upto(ref, modOp, cases) do
    {original, altered, rest} = modify_cases_upto(ref, modOp, cases, [], [])
    {:lists.reverse(original), :lists.reverse(altered), rest}
  end

  defp modify_cases_upto(ref, {:copy, newRef} = op, [{:conf, ref, props, mF} = c | t], orig, alt) do
    modify_cases_upto(ref, op, t, [c | orig], [{:conf, newRef, update_repeat(props), mF} | alt])
  end

  defp modify_cases_upto(ref, modOp, cases, orig, alt) do
    case :lists.any(
           fn
             {_, r, _, _} when r == ref ->
               true

             {_, r, _} when r == ref ->
               true

             {:skip_case, {_, r, _, _}, _} when r == ref ->
               true

             {:skip_case, {_, r, _, _}} when r == ref ->
               true

             _ ->
               false
           end,
           cases
         ) do
      true ->
        modify_cases_upto1(ref, modOp, cases, orig, alt)

      false ->
        {[], [], cases}
    end
  end

  defp modify_cases_upto1(
         ref,
         {:skip, reason, :conf, mode, :skip_case},
         [{:conf, ref, _Props, mF} | t],
         orig,
         alt
       ) do
    {orig, [{:skip_case, {:conf, ref, mF, reason}, mode} | alt], t}
  end

  defp modify_cases_upto1(
         ref,
         {:skip, reason, :conf, mode, :auto_skip_case},
         [{:conf, ref, _Props, mF} | t],
         orig,
         alt
       ) do
    {orig,
     [
       {:auto_skip_case, {:conf, ref, mF, reason}, mode}
       | alt
     ], t}
  end

  defp modify_cases_upto1(ref, {:copy, newRef}, [{:conf, ref, props, mF} = c | t], orig, alt) do
    {[c | orig], [{:conf, newRef, update_repeat(props), mF} | alt], t}
  end

  defp modify_cases_upto1(
         ref,
         {:skip, _, :tc, _, _},
         [{:conf, ref, _Props, _MF} | _] = cs,
         orig,
         alt
       ) do
    {orig, alt, cs}
  end

  defp modify_cases_upto1(
         ref,
         {:skip, reason, _, mode, skipType},
         [{:make, ref, mF} | t],
         orig,
         alt
       ) do
    {orig, [{skipType, {:make, ref, mF, reason}, mode} | alt], t}
  end

  defp modify_cases_upto1(ref, {:copy, newRef}, [{:make, ref, mF} = m | t], orig, alt) do
    {[m | orig], [{:make, newRef, mF} | alt], t}
  end

  defp modify_cases_upto1(
         ref,
         {:skip, reason, _, mode, skipType},
         [{:skip_case, {type, ref, mF, _Cmt}, _} | t],
         orig,
         alt
       ) do
    {orig, [{skipType, {type, ref, mF, reason}, mode} | alt], t}
  end

  defp modify_cases_upto1(
         ref,
         {:skip, reason, _, mode, skipType},
         [{:skip_case, {type, ref, mF, _Cmt}} | t],
         orig,
         alt
       ) do
    {orig, [{skipType, {type, ref, mF, reason}, mode} | alt], t}
  end

  defp modify_cases_upto1(
         ref,
         {:copy, newRef},
         [{:skip_case, {type, ref, mF, cmt}, mode} = c | t],
         orig,
         alt
       ) do
    {[c | orig], [{:skip_case, {type, newRef, mF, cmt}, mode} | alt], t}
  end

  defp modify_cases_upto1(
         ref,
         {:copy, newRef},
         [{:skip_case, {type, ref, mF, cmt}} = c | t],
         orig,
         alt
       ) do
    {[c | orig], [{:skip_case, {type, newRef, mF, cmt}} | alt], t}
  end

  defp modify_cases_upto1(ref, modOp, [{:skip_case, {_F, _Cmt}, _Mode} = mF | t], orig, alt) do
    modify_cases_upto1(ref, modOp, t, [mF | orig], [mF | alt])
  end

  defp modify_cases_upto1(
         ref,
         {:skip, reason, _, mode, :skip_case} = op,
         [{_M, _F} = mF | t],
         orig,
         alt
       ) do
    modify_cases_upto1(ref, op, t, orig, [{:skip_case, {mF, reason}, mode} | alt])
  end

  defp modify_cases_upto1(
         ref,
         {:skip, reason, _, mode, :auto_skip_case} = op,
         [{_M, _F} = mF | t],
         orig,
         alt
       ) do
    modify_cases_upto1(ref, op, t, orig, [{:auto_skip_case, {mF, reason}, mode} | alt])
  end

  defp modify_cases_upto1(ref, copyOp, [{_M, _F} = mF | t], orig, alt) do
    modify_cases_upto1(ref, copyOp, t, [mF | orig], [mF | alt])
  end

  defp modify_cases_upto1(
         ref,
         {:skip, reason, fType, mode, skipType},
         [{:conf, otherRef, props, _MF} | t],
         orig,
         alt
       ) do
    case hd(mode) do
      {^otherRef, _, _} ->
        modify_cases_upto1(ref, {:skip, reason, fType, tl(mode), skipType}, t, orig, alt)

      _ ->
        mode1 = [conf(otherRef, props) | mode]
        modify_cases_upto1(ref, {:skip, reason, fType, mode1, skipType}, t, orig, alt)
    end
  end

  defp modify_cases_upto1(
         ref,
         {:skip, reason, _, mode, skipType} = op,
         [{:repeat, {_M, _F} = mF, _Repeat} | t],
         orig,
         alt
       ) do
    modify_cases_upto1(ref, op, t, orig, [{skipType, {mF, reason}, mode} | alt])
  end

  defp modify_cases_upto1(ref, {:skip, _, _, _, _} = op, [{skipType, _, _} | t], orig, alt)
       when skipType === :skip_case or
              skipType === :auto_skip_case do
    modify_cases_upto1(ref, op, t, orig, alt)
  end

  defp modify_cases_upto1(ref, {:skip, reason, _, mode, skipType} = op, [other | t], orig, alt) do
    modify_cases_upto1(ref, op, t, orig, [{skipType, {other, reason}, mode} | alt])
  end

  defp modify_cases_upto1(ref, copyOp, [c | t], orig, alt) do
    modify_cases_upto1(ref, copyOp, t, [c | orig], [c | alt])
  end

  defp set_io_buffering(iOHandler) do
    :erlang.put(:test_server_common_io_handler, iOHandler)
  end

  defp is_io_buffered() do
    :erlang.get(:test_server_common_io_handler) !== :undefined
  end

  defp queue_test_case_io(ref, pid, num, mod, func) do
    entry = {ref, pid, num, mod, func}

    :erlang.put(
      :test_server_queued_io,
      :erlang.get(:test_server_queued_io) ++ [entry]
    )
  end

  defp wait_for_cases(ref) do
    case :erlang.get(:test_server_queued_io) do
      [] ->
        {[], [], []}

      cases ->
        [_Start | tCs] =
          :lists.dropwhile(
            fn
              {r, _, _, _, _}
              when r == ref ->
                false

              _ ->
                true
            end,
            cases
          )

        wait_and_resend(ref, tCs, [], [], [])
    end
  end

  defp wait_and_resend(ref, [{otherRef, _, 0, _, _} | ps], ok, skip, fail)
       when is_reference(otherRef) and otherRef != ref do
    ps1 = rm_cases_upto(otherRef, ps)
    wait_and_resend(ref, ps1, ok, skip, fail)
  end

  defp wait_and_resend(ref, [{_, currPid, caseNum, mod, func} | ps] = cases, ok, skip, fail) do
    receive do
      {:finished, _Ref, ^currPid, ^caseNum, ^mod, ^func, result, _RetVal} = msg ->
        send(self(), msg)
        mF = {mod, func}

        {ok1, skip1, fail1} =
          case result do
            :ok ->
              {[mF | ok], skip, fail}

            :skipped ->
              {ok, [mF | skip], fail}

            :failed ->
              {ok, skip, [mF | fail]}
          end

        wait_and_resend(ref, ps, ok1, skip1, fail1)

      {:EXIT, ^currPid, reason} when reason != :normal ->
        {:value, {_, _, ^caseNum, ^mod, ^func}} = :lists.keysearch(currPid, 2, cases)

        print(1, 'Error! Process for test case #~w (~w:~tw) died! Reason: ~tp', [
          caseNum,
          mod,
          func,
          reason
        ])

        exit({:unexpected_termination, {caseNum, mod, func}, {currPid, reason}})
    end
  end

  defp wait_and_resend(_, [], ok, skip, fail) do
    {:lists.reverse(ok), :lists.reverse(skip), :lists.reverse(fail)}
  end

  defp rm_cases_upto(ref, [{ref, _, 0, _, _} | ps]) do
    ps
  end

  defp rm_cases_upto(ref, [_ | ps]) do
    rm_cases_upto(ref, ps)
  end

  defp handle_test_case_io_and_status() do
    case :erlang.get(:test_server_queued_io) do
      [] ->
        {[], [], []}

      cases ->
        result = handle_io_and_exit_loop([], cases, [], [], [])
        main = self()

        :lists.foreach(
          fn
            {_, pid, _, _, _} when pid != main ->
              receive do
                {:EXIT, ^pid, :normal} ->
                  :ok
              after
                1000 ->
                  :ok
              end

            _ ->
              :ok
          end,
          cases
        )

        result
    end
  end

  defp handle_io_and_exit_loop(
         [],
         [
           {:undefined, currPid, caseNum, mod, func}
           | ps
         ] = cases,
         ok,
         skip,
         fail
       ) do
    receive do
      {:started, _, ^currPid, ^caseNum, ^mod, ^func} ->
        {ok1, skip1, fail1} =
          case handle_io_and_exits(self(), currPid, caseNum, mod, func, cases) do
            {:ok, mF} ->
              {[mF | ok], skip, fail}

            {:skipped, mF} ->
              {ok, [mF | skip], fail}

            {:failed, mF} ->
              {ok, skip, [mF | fail]}
          end

        handle_io_and_exit_loop([], ps, ok1, skip1, fail1)
    after
      1000 ->
        exit({:testcase_failed_to_start, mod, func})
    end
  end

  defp handle_io_and_exit_loop(
         refs,
         [{ref, currPid, caseNum, mod, func} | ps] = cases,
         ok,
         skip,
         fail
       ) do
    receive do
      {:started, _, ^currPid, ^caseNum, ^mod, ^func} ->
        _ = handle_io_and_exits(self(), currPid, caseNum, mod, func, cases)

        refs1 =
          case refs do
            [^ref | rs] ->
              rs

            _ when is_reference(ref) ->
              [ref | refs]

            _ ->
              refs
          end

        handle_io_and_exit_loop(refs1, ps, ok, skip, fail)
    after
      1000 ->
        exit({:testcase_failed_to_start, mod, func})
    end
  end

  defp handle_io_and_exit_loop(_, [], ok, skip, fail) do
    {:lists.reverse(ok), :lists.reverse(skip), :lists.reverse(fail)}
  end

  defp handle_io_and_exits(main, currPid, caseNum, mod, func, cases) do
    receive do
      {:abort_current_testcase = tag, _Reason, from} ->
        send(from, {self(), tag, {:error, :parallel_group}})
        handle_io_and_exits(main, currPid, caseNum, mod, func, cases)

      {:finished, _, ^main, ^caseNum, ^mod, ^func, result, _RetVal} ->
        :test_server_io.print_buffered(currPid)
        {result, {mod, func}}

      {:finished, _, ^currPid, ^caseNum, ^mod, ^func, result, retVal} ->
        :test_server_io.print_buffered(currPid)

        case result do
          :ok ->
            :erlang.put(
              :test_server_ok,
              :erlang.get(:test_server_ok) + 1
            )

          :failed ->
            :erlang.put(
              :test_server_failed,
              :erlang.get(:test_server_failed) + 1
            )

          :skipped ->
            skipCounters =
              update_skip_counters(
                retVal,
                :erlang.get(:test_server_skipped)
              )

            :erlang.put(:test_server_skipped, skipCounters)
        end

        {result, {mod, func}}

      {:EXIT, tCPid, reason} when reason != :normal ->
        :test_server_io.print_buffered(currPid)
        {:value, {_, _, num, m, f}} = :lists.keysearch(tCPid, 2, cases)

        print(1, 'Error! Process for test case #~w (~w:~tw) died! Reason: ~tp', [
          num,
          m,
          f,
          reason
        ])

        exit({:unexpected_termination, {num, m, f}, {tCPid, reason}})
    end
  end

  defp run_test_case(ref, num, mod, func, args, runInit, timetrapData) do
    :ok = :file.set_cwd(:filename.dirname(:erlang.get(:test_server_dir)))
    run_test_case1(ref, num, mod, func, args, runInit, timetrapData, [], self())
  end

  defp run_test_case(ref, num, mod, func, args, :skip_init, timetrapData, mode) do
    run_test_case1(ref, num, mod, func, args, :skip_init, timetrapData, mode, self())
  end

  defp run_test_case(ref, num, mod, func, args, runInit, timetrapData, mode) do
    :ok = :file.set_cwd(:filename.dirname(:erlang.get(:test_server_dir)))
    main = self()

    case check_prop(:parallel, mode) do
      false ->
        run_test_case1(ref, num, mod, func, args, runInit, timetrapData, mode, main)

      _Ref ->
        dictionary = :erlang.get()
        {:dictionary, ^dictionary} = :erlang.process_info(self(), :dictionary)

        spawn_link(fn ->
          :erlang.process_flag(:trap_exit, true)
          :ct_util.mark_process()

          _ =
            for {key, val} <- dictionary do
              :erlang.put(key, val)
            end

          set_io_buffering({:tc, main})
          run_test_case1(ref, num, mod, func, args, runInit, timetrapData, mode, main)
        end)
    end
  end

  defp run_test_case1(ref, num, mod, func, args, runInit, timetrapData, mode, main) do
    :erlang.group_leader(
      :test_server_io.get_gl(main == self()),
      self()
    )

    case is_io_buffered() do
      false ->
        :ok

      true ->
        :test_server_io.start_transaction()
        send(main, {:started, ref, self(), num, mod, func})
        :ok
    end

    tSDir = :erlang.get(:test_server_dir)
    print(:major, '=case          ~w:~tw', [mod, func])
    minorName = start_minor_log_file(mod, func, self() != main)
    minorBase = :filename.basename(minorName)
    print(:major, '=logfile       ~ts', [:filename.basename(minorName)])

    updatedArgs =
      case :erlang.get(:test_server_create_priv_dir) do
        :auto_per_run ->
          update_config(hd(args), [{:tc_logfile, minorName}])

        privDirMode ->
          runDir = :filename.dirname(minorName)

          ext =
            cond do
              num == 0 ->
                int = :erlang.unique_integer([:positive, :monotonic])
                :lists.flatten(:io_lib.format('.cfg.~w', [int]))

              true ->
                :lists.flatten(:io_lib.format('.~w', [num]))
            end

          privDir = :filename.join(runDir, 'log_private') ++ ext

          cond do
            privDirMode == :auto_per_tc ->
              :ok = :file.make_dir(privDir)

            privDirMode == :manual_per_tc ->
              :ok
          end

          update_config(
            hd(args),
            [{:priv_dir, privDir ++ '/'}, {:tc_logfile, minorName}]
          )
      end

    grName = get_name(mode)

    :test_server_sup.framework_call(
      :report,
      [:tc_start, {{mod, {func, grName}}, minorName}]
    )

    {:ok, cwd} = :file.get_cwd()

    args2Print =
      cond do
        is_list(updatedArgs) ->
          :lists.keydelete(:tc_group_result, 1, updatedArgs)

        true ->
          updatedArgs
      end

    cond do
      runInit == :skip_init ->
        print_props(get_props(mode))

      true ->
        :ok
    end

    print(:minor, '~ts', [
      escape_chars(:io_lib.format('Config value:\n\n    ~tp\n', [args2Print]))
    ])

    print(:minor, 'Current directory is ~tp\n', [cwd])

    grNameStr =
      case grName do
        :undefined ->
          ''

        name ->
          cast_to_list(name)
      end

    print(:major, '=started       ~s', [:lists.flatten(timestamp_get(''))])
    {{col0, col1}, style} = get_font_style(runInit == :run_init, mode)
    tR = xhtml('<tr valign="top">', '<tr class="' ++ odd_or_even() ++ '">')
    encMinorBase = uri_encode(minorBase)

    print(
      :html,
      tR ++
        '<td>' ++
        col0 ++
        '~ts' ++
        col1 ++
        '</td><td>' ++
        col0 ++
        '~w' ++
        col1 ++
        '</td><td>' ++
        col0 ++
        '~ts' ++
        col1 ++
        '</td><td><a href="~ts">~tw</a></td><td><a href="~ts#top">&lt;</a> <a href="~ts#end">&gt;</a></td>',
      [num2str(num), fw_name(mod), grNameStr, encMinorBase, func, encMinorBase, encMinorBase]
    )

    do_unless_parallel(main, &:erlang.yield/0)

    {result, detectedFail, procsBefore, procsAfter} =
      run_test_case_apply(num, mod, func, [updatedArgs], grName, runInit, timetrapData)

    {time, retVal, loc, opts, comment} =
      case result do
        normal = {_Time, _RetVal, _Loc, _Opts, _Comment} ->
          normal

        {:died, dReason, dLoc, dCmt} ->
          {:died, dReason, dLoc, [], dCmt}
      end

    print(:minor, '<a name="end"></a>', [], :internal_raw)
    print(:minor, '\n', [], :internal_raw)
    print_timestamp(:minor, 'Ended at ')
    print(:major, '=ended         ~s', [timestamp_get('')])

    do_unless_parallel(
      main,
      fn ->
        :file.set_cwd(:filename.dirname(tSDir))
      end
    )

    status =
      case {time, retVal} do
        {:died, {:timetrap_timeout, timetrapTimeout}} ->
          progress(
            :failed,
            num,
            mod,
            func,
            grName,
            loc,
            :timetrap_timeout,
            timetrapTimeout,
            comment,
            style
          )

        {:died, reason = {:auto_skip, _Why}} ->
          progress(:skip, num, mod, func, grName, loc, reason, time, comment, style)

        {:died, {skip, reason}}
        when skip == :skip or
               skip == :skipped ->
          progress(:skip, num, mod, func, grName, loc, reason, time, comment, style)

        {:died, reason} when reason !== :ok ->
          progress(:failed, num, mod, func, grName, loc, reason, time, comment, style)

        {_, {:EXIT, {skip, reason}}}
        when skip == :skip or
               skip == :skipped or
               skip == :auto_skip ->
          progress(:skip, num, mod, func, grName, loc, reason, time, comment, style)

        {_, {:EXIT, _Pid, {skip, reason}}}
        when skip == :skip or
               skip == :skipped ->
          progress(:skip, num, mod, func, grName, loc, reason, time, comment, style)

        {_, {:EXIT, _Pid, reason}} ->
          progress(:failed, num, mod, func, grName, loc, reason, time, comment, style)

        {_, {:EXIT, reason}} ->
          progress(:failed, num, mod, func, grName, loc, reason, time, comment, style)

        {_, {fail, reason}}
        when fail === :fail or
               fail === :failed ->
          progress(:failed, num, mod, func, grName, loc, reason, time, comment, style)

        {_, reason = {:auto_skip, _Why}} ->
          progress(:skip, num, mod, func, grName, loc, reason, time, comment, style)

        {_, {skip, reason}}
        when skip == :skip or
               skip == :skipped ->
          progress(:skip, num, mod, func, grName, loc, reason, time, comment, style)

        {^time, ^retVal} ->
          case detectedFail do
            [] ->
              progress(:ok, num, mod, func, grName, loc, retVal, time, comment, style)

            reason ->
              progress(:failed, num, mod, func, grName, loc, reason, time, comment, style)
          end
      end

    case {runInit, status} do
      {:skip_init, _} ->
        :ok

      {_, :ok} ->
        :erlang.put(
          :test_server_ok,
          :erlang.get(:test_server_ok) + 1
        )

      {_, :failed} ->
        :erlang.put(
          :test_server_failed,
          :erlang.get(:test_server_failed) + 1
        )

      {_, :skip} ->
        {uS, aS} = :erlang.get(:test_server_skipped)
        :erlang.put(:test_server_skipped, {uS + 1, aS})

      {_, :auto_skip} ->
        {uS, aS} = :erlang.get(:test_server_skipped)
        :erlang.put(:test_server_skipped, {uS, aS + 1})
    end

    case self() do
      ^main ->
        case :test_server_sup.framework_call(:warn, [:processes], true) do
          true ->
            cond do
              procsBefore < procsAfter ->
                print(:minor, 'WARNING: ~w more processes in system after test case', [
                  procsAfter - procsBefore
                ])

              procsBefore > procsAfter ->
                print(:minor, 'WARNING: ~w less processes in system after test case', [
                  procsBefore - procsAfter
                ])

              true ->
                :ok
            end

          false ->
            :ok
        end

        case :test_server_sup.framework_call(:warn, [:nodes], true) do
          true ->
            case (try do
                    controller_call(:kill_slavenodes)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              {:EXIT, _} = exit ->
                print(
                  :minor,
                  'WARNING: There might be slavenodes left in the system. I tried to kill them, but I failed: ~tp\n',
                  [exit]
                )

              [] ->
                :ok

              list ->
                print(
                  :minor,
                  'WARNING: ~w slave nodes in system after test' ++
                    'case. Tried to killed them.~n' ++ '         Names:~tp',
                  [length(list), list]
                )
            end

          false ->
            :ok
        end

      _ ->
        :ok
    end

    cond do
      is_number(time) ->
        :erlang.put(
          :test_server_total_time,
          :erlang.get(:test_server_total_time) + time
        )

      true ->
        :ok
    end

    :test_server_sup.check_new_crash_dumps()

    case is_io_buffered() do
      false ->
        :ok

      true ->
        :test_server_io.end_transaction()

        send(
          main,
          {:finished, ref, self(), num, mod, func,
           cond do
             status == :skip ->
               :skipped

             status == :auto_skip ->
               :skipped

             true ->
               status
           end, {time, retVal, opts}}
        )

        :ok
    end

    {time, retVal, opts}
  end

  defp do_unless_parallel(main, action) when is_function(action, 0) do
    case self() do
      ^main ->
        action.()

      _ ->
        :ok
    end
  end

  defp num2str(0) do
    ''
  end

  defp num2str(n) do
    :erlang.integer_to_list(n)
  end

  defp progress(:skip, caseNum, mod, func, grName, loc, reason, time, comment, {st0, st1}) do
    {reason1, {color, ret, reportTag}} =
      if_auto_skip(
        reason,
        fn ->
          {'#FFA64D', :auto_skip, :auto_skipped}
        end,
        fn ->
          {'#FF8000', :skip, :skipped}
        end
      )

    print(:major, '=result        ~w: ~tp', [reportTag, reason1])

    print(1, '*** SKIPPED ~ts ***', [
      get_info_str(mod, func, caseNum, :erlang.get(:test_server_cases))
    ])

    :test_server_sup.framework_call(
      :report,
      [:tc_done, {mod, {func, grName}, {reportTag, reason1}}]
    )

    timeStr =
      :io_lib.format(
        cond do
          is_float(time) ->
            '~.3fs'

          true ->
            '~w'
        end,
        [time]
      )

    reasonStr = escape_chars(reason_to_string(reason1))

    reasonStr1 =
      :lists.flatten(
        for s <-
              :string.lexemes(
                reasonStr,
                [?\n]
              ) do
          :string.trim(s, :leading, ' ')
        end
      )

    reasonLength = :string.length(reasonStr1)

    reasonStr2 =
      cond do
        reasonLength > 80 ->
          :string.slice(reasonStr1, 0, 77) ++ '...'

        true ->
          reasonStr1
      end

    comment1 =
      case comment do
        '' ->
          ''

        _ ->
          xhtml('<br>(', '<br />(') ++ to_string(comment) ++ ')'
      end

    print(
      :html,
      '<td>' ++
        st0 ++
        '~ts' ++ st1 ++ '</td><td><font color="~ts">SKIPPED</font></td><td>~ts~ts</td></tr>\n',
      [timeStr, color, reasonStr2, comment1]
    )

    formatLoc = :test_server_sup.format_loc(loc)
    print(:minor, '=== Location: ~ts', [formatLoc])
    print(:minor, '=== Reason: ~ts', [reasonStr1])
    ret
  end

  defp progress(
         :failed,
         caseNum,
         mod,
         func,
         grName,
         loc,
         :timetrap_timeout,
         t,
         comment0,
         {st0, st1}
       ) do
    print(:major, '=result        failed: timeout, ~tp', [loc])

    print(1, '*** FAILED ~ts ***', [
      get_info_str(mod, func, caseNum, :erlang.get(:test_server_cases))
    ])

    :test_server_sup.framework_call(
      :report,
      [:tc_done, {mod, {func, grName}, {:failed, :timetrap_timeout}}]
    )

    formatLastLoc = :test_server_sup.format_loc(get_last_loc(loc))
    errorReason = :io_lib.format('{timetrap_timeout,~ts}', [formatLastLoc])

    comment =
      case comment0 do
        '' ->
          '<font color="red">' ++ errorReason ++ '</font>'

        _ ->
          '<font color="red">' ++
            errorReason ++ xhtml('</font><br>', '</font><br />') ++ to_string(comment0)
      end

    print(
      :html,
      '<td>' ++
        st0 ++
        '~.3fs' ++ st1 ++ '</td><td><font color="red">FAILED</font></td><td>~ts</td></tr>\n',
      [t / 1000, comment]
    )

    formatLoc = :test_server_sup.format_loc(loc)
    print(:minor, '=== Location: ~ts', [formatLoc])
    print(:minor, '=== Reason: timetrap timeout', [])
    :failed
  end

  defp progress(
         :failed,
         caseNum,
         mod,
         func,
         grName,
         loc,
         {:testcase_aborted, reason},
         _T,
         comment0,
         {st0, st1}
       ) do
    print(:major, '=result        failed: testcase_aborted, ~tp', [loc])

    print(1, '*** FAILED ~ts ***', [
      get_info_str(mod, func, caseNum, :erlang.get(:test_server_cases))
    ])

    :test_server_sup.framework_call(
      :report,
      [:tc_done, {mod, {func, grName}, {:failed, :testcase_aborted}}]
    )

    formatLastLoc = :test_server_sup.format_loc(get_last_loc(loc))
    errorReason = :io_lib.format('{testcase_aborted,~ts}', [formatLastLoc])

    comment =
      case comment0 do
        '' ->
          '<font color="red">' ++ errorReason ++ '</font>'

        _ ->
          '<font color="red">' ++
            errorReason ++ xhtml('</font><br>', '</font><br />') ++ to_string(comment0)
      end

    print(
      :html,
      '<td>' ++
        st0 ++ 'died' ++ st1 ++ '</td><td><font color="red">FAILED</font></td><td>~ts</td></tr>\n',
      [comment]
    )

    formatLoc = :test_server_sup.format_loc(loc)
    print(:minor, '=== Location: ~ts', [formatLoc])

    print(:minor, '~ts', [
      escape_chars(:io_lib.format('=== Reason: {testcase_aborted,~tp}', [reason]))
    ])

    :failed
  end

  defp progress(:failed, caseNum, mod, func, grName, :unknown, reason, time, comment0, {st0, st1}) do
    print(:major, '=result        failed: ~tp, ~w', [reason, :unknown_location])

    print(1, '*** FAILED ~ts ***', [
      get_info_str(mod, func, caseNum, :erlang.get(:test_server_cases))
    ])

    :test_server_sup.framework_call(
      :report,
      [:tc_done, {mod, {func, grName}, {:failed, reason}}]
    )

    timeStr =
      :io_lib.format(
        cond do
          is_float(time) ->
            '~.3fs'

          true ->
            '~w'
        end,
        [time]
      )

    errorReason =
      escape_chars(
        :lists.flatten(
          :io_lib.format(
            '~tp',
            [reason]
          )
        )
      )

    errorReason1 =
      :lists.flatten(
        for s <-
              :string.lexemes(
                errorReason,
                [?\n]
              ) do
          :string.trim(s, :leading, ' ')
        end
      )

    errorReasonLength = :string.length(errorReason1)

    errorReason2 =
      cond do
        errorReasonLength > 63 ->
          :string.slice(errorReason1, 0, 60) ++ '...'

        true ->
          errorReason1
      end

    comment =
      case comment0 do
        '' ->
          '<font color="red">' ++ errorReason2 ++ '</font>'

        _ ->
          '<font color="red">' ++
            errorReason2 ++ xhtml('</font><br>', '</font><br />') ++ to_string(comment0)
      end

    print(
      :html,
      '<td>' ++
        st0 ++ '~ts' ++ st1 ++ '</td><td><font color="red">FAILED</font></td><td>~ts</td></tr>\n',
      [timeStr, comment]
    )

    print(:minor, '=== Location: ~w', [:unknown])
    {fStr, formattedReason} = format_exception(reason)

    print(:minor, '~ts', [
      escape_chars(
        :io_lib.format(
          '=== Reason: ' ++ fStr,
          [formattedReason]
        )
      )
    ])

    :failed
  end

  defp progress(:failed, caseNum, mod, func, grName, loc, reason, time, comment0, {st0, st1}) do
    {locMaj, locMin} =
      cond do
        func == :error_in_suite ->
          case get_fw_mod(:undefined) do
            ^mod ->
              {:unknown_location, :unknown}

            _ ->
              {loc, loc}
          end

        true ->
          {loc, loc}
      end

    print(:major, '=result        failed: ~tp, ~tp', [reason, locMaj])

    print(1, '*** FAILED ~ts ***', [
      get_info_str(mod, func, caseNum, :erlang.get(:test_server_cases))
    ])

    :test_server_sup.framework_call(
      :report,
      [:tc_done, {mod, {func, grName}, {:failed, reason}}]
    )

    timeStr =
      :io_lib.format(
        cond do
          is_float(time) ->
            '~.3fs'

          true ->
            '~w'
        end,
        [time]
      )

    comment =
      case comment0 do
        '' ->
          ''

        _ ->
          xhtml('<br>', '<br />') ++ to_string(comment0)
      end

    formatLastLoc = :test_server_sup.format_loc(get_last_loc(locMaj))

    print(
      :html,
      '<td>' ++
        st0 ++
        '~ts' ++
        st1 ++
        '</td><td><font color="red">FAILED</font></td><td><font color="red">~ts</font>~ts</td></tr>\n',
      [timeStr, formatLastLoc, comment]
    )

    formatLoc = :test_server_sup.format_loc(locMin)
    print(:minor, '=== Location: ~ts', [formatLoc])
    {fStr, formattedReason} = format_exception(reason)

    print(:minor, '~ts', [
      '=== Reason: ' ++
        escape_chars(
          :io_lib.format(
            fStr,
            [formattedReason]
          )
        )
    ])

    :failed
  end

  defp progress(:ok, _CaseNum, mod, func, grName, _Loc, retVal, time, comment0, {st0, st1}) do
    print(:minor, 'successfully completed test case', [])

    :test_server_sup.framework_call(
      :report,
      [:tc_done, {mod, {func, grName}, :ok}]
    )

    timeStr =
      :io_lib.format(
        cond do
          is_float(time) ->
            '~.3fs'

          true ->
            '~w'
        end,
        [time]
      )

    comment =
      case retVal do
        {:comment, retComment} ->
          string = to_string(retComment)
          htmlCmt = :test_server_sup.framework_call(:format_comment, [string], string)
          print(:major, '=result        ok: ~ts', [string])
          '<td>' ++ htmlCmt ++ '</td>'

        _ ->
          print(:major, '=result        ok', [])

          case comment0 do
            '' ->
              '<td></td>'

            _ ->
              '<td>' ++ to_string(comment0) ++ '</td>'
          end
      end

    print(:major, '=elapsed       ~p', [time])

    print(
      :html,
      '<td>' ++ st0 ++ '~ts' ++ st1 ++ '</td><td><font color="green">Ok</font></td>~ts</tr>\n',
      [timeStr, comment]
    )

    print(:minor, '~ts', [escape_chars(:io_lib.format('=== Returned value: ~tp', [retVal]))])
    :ok
  end

  def escape_chars(term)
      when not is_list(term) and
             not is_binary(term) do
    esc_chars_in_list(:io_lib.format('~tp', [term]))
  end

  def escape_chars(list = [term | _])
      when not is_list(term) and
             not is_integer(term) do
    esc_chars_in_list(:io_lib.format('~tp', [list]))
  end

  def escape_chars(list) do
    esc_chars_in_list(list)
  end

  defp esc_chars_in_list([bin | io]) when is_binary(bin) do
    [bin | esc_chars_in_list(io)]
  end

  defp esc_chars_in_list([list | io]) when is_list(list) do
    [esc_chars_in_list(list) | esc_chars_in_list(io)]
  end

  defp esc_chars_in_list([?< | io]) do
    ['&lt;' | esc_chars_in_list(io)]
  end

  defp esc_chars_in_list([?> | io]) do
    ['&gt;' | esc_chars_in_list(io)]
  end

  defp esc_chars_in_list([?& | io]) do
    ['&amp;' | esc_chars_in_list(io)]
  end

  defp esc_chars_in_list([char | io]) when is_integer(char) do
    [char | esc_chars_in_list(io)]
  end

  defp esc_chars_in_list([]) do
    []
  end

  defp esc_chars_in_list(bin) do
    bin
  end

  defp get_fw_mod(mod) do
    case :erlang.get(:test_server_framework) do
      :undefined ->
        case :os.getenv('TEST_SERVER_FRAMEWORK') do
          fW when fW === false or fW === 'undefined' ->
            mod

          fW ->
            :erlang.list_to_atom(fW)
        end

      :"$none" ->
        mod

      fW ->
        fW
    end
  end

  defp fw_name(:test_server_ctrl) do
    :test_server
  end

  defp fw_name(mod) do
    case :erlang.get(:test_server_framework_name) do
      :undefined ->
        case get_fw_mod(:undefined) do
          :undefined ->
            mod

          ^mod ->
            case :os.getenv('TEST_SERVER_FRAMEWORK_NAME') do
              fWName when fWName === false or fWName === 'undefined' ->
                mod

              fWName ->
                :erlang.list_to_atom(fWName)
            end

          _ ->
            mod
        end

      :"$none" ->
        mod

      fWName ->
        case get_fw_mod(mod) do
          ^mod ->
            fWName

          _ ->
            mod
        end
    end
  end

  defp if_auto_skip(reason = {:failed, {_, :init_per_testcase, _}}, true__, _False) do
    {reason, true__.()}
  end

  defp if_auto_skip({:skip, reason = {:failed, {_, :init_per_testcase, _}}}, true__, _False) do
    {reason, true__.()}
  end

  defp if_auto_skip({:auto_skip, reason}, true__, _False) do
    {reason, true__.()}
  end

  defp if_auto_skip(reason, _True, false__) do
    {reason, false__.()}
  end

  defp update_skip_counters({_T, pat, _Opts}, {uS, aS}) do
    {_, result} =
      if_auto_skip(
        pat,
        fn ->
          {uS, aS + 1}
        end,
        fn ->
          {uS + 1, aS}
        end
      )

    result
  end

  defp update_skip_counters(pat, {uS, aS}) do
    {_, result} =
      if_auto_skip(
        pat,
        fn ->
          {uS, aS + 1}
        end,
        fn ->
          {uS + 1, aS}
        end
      )

    result
  end

  defp get_info_str(mod, func, 0, _Cases) do
    :io_lib.format('~tw', [{mod, func}])
  end

  defp get_info_str(_Mod, _Func, caseNum, :unknown) do
    'test case ' ++ :erlang.integer_to_list(caseNum)
  end

  defp get_info_str(_Mod, _Func, caseNum, cases) do
    'test case ' ++ :erlang.integer_to_list(caseNum) ++ ' of ' ++ :erlang.integer_to_list(cases)
  end

  defp print_if_known(known, {sK, aK}, {sU, aU}) do
    {s, a} =
      cond do
        known == :unknown ->
          {sU, aU}

        true ->
          {sK, aK}
      end

    :io_lib.format(s, a)
  end

  def to_string(term) when is_list(term) do
    case (try do
            :io_lib.format('~ts', [term])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        :lists.flatten(:io_lib.format('~tp', [term]))

      string ->
        :lists.flatten(string)
    end
  end

  def to_string(term) do
    :lists.flatten(:io_lib.format('~tp', [term]))
  end

  defp get_last_loc(loc) when is_tuple(loc) do
    loc
  end

  defp get_last_loc([loc | _]) when is_tuple(loc) do
    [loc]
  end

  defp get_last_loc(loc) do
    loc
  end

  defp reason_to_string({:failed, {_, failFunc, :bad_return}}) do
    :erlang.atom_to_list(failFunc) ++ ' bad return value'
  end

  defp reason_to_string({:failed, {_, failFunc, {:timetrap_timeout, _}}}) do
    :erlang.atom_to_list(failFunc) ++ ' timed out'
  end

  defp reason_to_string(fWInitFail = {:failed, {_CB, :init_tc, _Reason}}) do
    to_string(fWInitFail)
  end

  defp reason_to_string({:failed, {_, failFunc, _}}) do
    :erlang.atom_to_list(failFunc) ++ ' failed'
  end

  defp reason_to_string(other) do
    to_string(other)
  end

  defp get_font_style(normalCase, mode) do
    prop =
      cond do
        not normalCase ->
          :default

        true ->
          case check_prop(:parallel, mode) do
            false ->
              case check_prop(:sequence, mode) do
                false ->
                  :default

                _ ->
                  :sequence
              end

            _ ->
              :parallel
          end
      end

    {col, st0, st1} = get_font_style1(prop)

    {{'<font color=' ++ col ++ '>', '</font>'},
     {'<font color=' ++ col ++ '>' ++ st0, st1 ++ '</font>'}}
  end

  defp get_font_style1(:parallel) do
    {'"darkslategray"', '<i>', '</i>'}
  end

  defp get_font_style1(:sequence) do
    {'"saddlebrown"', '', ''}
  end

  defp get_font_style1(:default) do
    {'"black"', '', ''}
  end

  defp format_exception(reason = {_Error, stack}) when is_list(stack) do
    case get_fw_mod(:undefined) do
      :undefined ->
        case :application.get_env(
               :test_server,
               :format_exception
             ) do
          {:ok, false} ->
            {'~tp', reason}

          _ ->
            do_format_exception(reason)
        end

      fW ->
        case :application.get_env(fW, :format_exception) do
          {:ok, false} ->
            {'~tp', reason}

          _ ->
            do_format_exception(reason)
        end
    end
  end

  defp format_exception(error) do
    format_exception({error, []})
  end

  defp do_format_exception(reason = {error, stack}) do
    stackFun = fn _, _, _ ->
      false
    end

    pF = fn term, i ->
      :io_lib.format(
        '~.' ++ :erlang.integer_to_list(i) ++ 'tp',
        [term]
      )
    end

    case (try do
            :erl_error.format_exception(1, :error, error, stack, stackFun, pF, :utf8)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _R} ->
        {'~tp', reason}

      formatted ->
        formatted1 = :re.replace(formatted, 'exception error: ', '', [{:return, :list}, :unicode])
        {'~ts', :lists.flatten(formatted1)}
    end
  end

  defp run_test_case_apply(caseNum, mod, func, args, name, runInit, timetrapData) do
    :test_server.run_test_case_apply({caseNum, mod, func, args, name, runInit, timetrapData})
  end

  def print(detail, format) do
    print(detail, format, [])
  end

  def print(detail, format, args) do
    print(detail, format, args, :internal)
  end

  def print(detail, ['$tc_html', format], args, printer) do
    msg = :io_lib.format(format, args)
    print_or_buffer(detail, ['$tc_html', msg], printer)
  end

  def print(detail, format, args, printer) do
    msg = :io_lib.format(format, args)
    print_or_buffer(detail, msg, printer)
  end

  defp print_or_buffer(detail, msg, printer) do
    :test_server_gl.print(:erlang.group_leader(), detail, msg, printer)
  end

  def print_timestamp(detail, leader) do
    print(detail, timestamp_get(leader), [])
  end

  defp print_who(host, user) do
    userStr =
      case user do
        '' ->
          ''

        _ ->
          ' by ' ++ user
      end

    print(:html, 'Run~ts on ~ts', [userStr, host])
  end

  def format(format) do
    format(:minor, format, [])
  end

  def format(:major, format) do
    format(:major, format, [])
  end

  def format(:minor, format) do
    format(:minor, format, [])
  end

  def format(detail, format) when is_integer(detail) do
    format(detail, format, [])
  end

  def format(format, args) do
    format(:minor, format, args)
  end

  def format(detail, format, args) do
    str =
      case (try do
              :io_lib.format(format, args)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:EXIT, _} ->
          :io_lib.format('illegal format; ~tp with args ~tp.\n', [format, args])

        valid ->
          valid
      end

    print_or_buffer(detail, str, self())
  end

  def xhtml(hTML, xHTML) do
    case :erlang.get(:basic_html) do
      true ->
        hTML

      _ ->
        xHTML
    end
  end

  defp odd_or_even() do
    case :erlang.get(:odd_or_even) do
      :even ->
        :erlang.put(:odd_or_even, :odd)
        'even'

      _ ->
        :erlang.put(:odd_or_even, :even)
        'odd'
    end
  end

  defp timestamp_filename_get(leader) do
    timestamp_get_internal(leader, '~ts~w-~2.2.0w-~2.2.0w_~2.2.0w.~2.2.0w.~2.2.0w')
  end

  defp timestamp_get(leader) do
    timestamp_get_internal(leader, '~ts~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w')
  end

  defp timestamp_get_internal(leader, format) do
    {yY, mM, dD, h, m, s} = time_get()

    :lists.flatten(
      :io_lib.format(
        format,
        [leader, yY, mM, dD, h, m, s]
      )
    )
  end

  defp time_get() do
    {yY, mM, dD} = :erlang.date()
    {h, m, s} = :erlang.time()

    case :erlang.date() do
      {^yY, ^mM, ^dD} ->
        {yY, mM, dD, h, m, s}

      _NewDay ->
        time_get()
    end
  end

  defp make_config(initial) do
    initial
  end

  defp update_config(config, {key, val}) do
    case :lists.keymember(key, 1, config) do
      true ->
        :lists.keyreplace(key, 1, config, {key, val})

      false ->
        [{key, val} | config]
    end
  end

  defp update_config(config, [assoc | assocs]) do
    newConfig = update_config(config, assoc)
    update_config(newConfig, assocs)
  end

  defp update_config(config, []) do
    config
  end

  Record.defrecord(:r_cc, :cc,
    mod: :undefined,
    skip: :undefined
  )

  defp collect_all_cases(top, skip) when is_list(skip) do
    result =
      case collect_cases(top, r_cc(mod: [], skip: skip), []) do
        {:ok, cases, _St} ->
          cases

        other ->
          other
      end

    result
  end

  defp collect_cases([], st, _) do
    {:ok, [], st}
  end

  defp collect_cases([case__ | cs0], st0, mode) do
    case collect_cases(case__, st0, mode) do
      {:ok, flatCases1, st1} ->
        case collect_cases(cs0, st1, mode) do
          {:ok, flatCases2, st} ->
            {:ok, flatCases1 ++ flatCases2, st}

          {:error, _Reason} = error ->
            error
        end

      {:error, _Reason} = error ->
        error
    end
  end

  defp collect_cases({:module, case__}, st, mode)
       when is_atom(case__) and is_atom(r_cc(st, :mod)) do
    collect_case({r_cc(st, :mod), case__}, st, mode)
  end

  defp collect_cases({:module, mod, case__}, st, mode) do
    collect_case({mod, case__}, st, mode)
  end

  defp collect_cases({:module, mod, case__, args}, st, mode) do
    collect_case({mod, case__, args}, st, mode)
  end

  defp collect_cases({:dir, subDir}, st, mode) do
    collect_files(subDir, '*_SUITE', st, mode)
  end

  defp collect_cases({:dir, subDir, pattern}, st, mode) do
    collect_files(subDir, pattern ++ '*', st, mode)
  end

  defp collect_cases({:conf, initF, caseList, finMF}, st, mode)
       when is_atom(initF) do
    collect_cases({:conf, [], {r_cc(st, :mod), initF}, caseList, finMF}, st, mode)
  end

  defp collect_cases({:conf, initMF, caseList, finF}, st, mode)
       when is_atom(finF) do
    collect_cases({:conf, [], initMF, caseList, {r_cc(st, :mod), finF}}, st, mode)
  end

  defp collect_cases({:conf, initMF, caseList, finMF}, st0, mode) do
    collect_cases({:conf, [], initMF, caseList, finMF}, st0, mode)
  end

  defp collect_cases({:conf, props, initF, caseList, finMF}, st, mode)
       when is_atom(initF) do
    case init_props(props) do
      {:error, _} ->
        {:ok, [], st}

      props1 ->
        collect_cases({:conf, props1, {r_cc(st, :mod), initF}, caseList, finMF}, st, mode)
    end
  end

  defp collect_cases({:conf, props, initMF, caseList, finF}, st, mode)
       when is_atom(finF) do
    case init_props(props) do
      {:error, _} ->
        {:ok, [], st}

      props1 ->
        collect_cases({:conf, props1, initMF, caseList, {r_cc(st, :mod), finF}}, st, mode)
    end
  end

  defp collect_cases({:conf, props, initMF, caseList, finMF} = conf, st, mode) do
    case init_props(props) do
      {:error, _} ->
        {:ok, [], st}

      props1 ->
        ref = make_ref()
        skips = r_cc(st, :skip)

        props2 = [
          {:suite, r_cc(st, :mod)}
          | :lists.delete(
              :suite,
              props1
            )
        ]

        mode1 = [{ref, props2, :undefined} | mode]

        case in_skip_list({r_cc(st, :mod), conf}, skips) do
          {true, comment} ->
            {:ok,
             [
               {:skip_case, {:conf, ref, initMF, comment}, mode1}
               | [] ++ [{:conf, ref, [], finMF}]
             ], st}

          {true, name, comment} when is_atom(name) ->
            case collect_cases(caseList, st, mode1) do
              {:ok, [], _St} = empty ->
                empty

              {:ok, flatCases, st1} ->
                cases2Skip = flatCases ++ [{:conf, ref, keep_name(props1), finMF}]
                skipped = skip_cases_upto(ref, cases2Skip, comment, :conf, mode1, :skip_case)

                {:ok,
                 [
                   {:skip_case, {:conf, ref, initMF, comment}, mode1}
                   | skipped
                 ], st1}

              {:error, _Reason} = error ->
                error
            end

          {true, toSkip, _} when is_list(toSkip) ->
            case collect_cases(caseList, r_cc(st, skip: toSkip ++ skips), mode1) do
              {:ok, [], _St} = empty ->
                empty

              {:ok, flatCases, st1} ->
                {:ok,
                 [
                   {:conf, ref, props1, initMF}
                   | flatCases ++ [{:conf, ref, keep_name(props1), finMF}]
                 ], r_cc(st1, skip: skips)}

              {:error, _Reason} = error ->
                error
            end

          false ->
            case collect_cases(caseList, st, mode1) do
              {:ok, [], _St} = empty ->
                empty

              {:ok, flatCases, st1} ->
                {:ok,
                 [
                   {:conf, ref, props1, initMF}
                   | flatCases ++ [{:conf, ref, keep_name(props1), finMF}]
                 ], st1}

              {:error, _Reason} = error ->
                error
            end
        end
    end
  end

  defp collect_cases({:make, initMFA, caseList, finMFA}, st0, mode) do
    case collect_cases(caseList, st0, mode) do
      {:ok, [], _St} = empty ->
        empty

      {:ok, flatCases, st} ->
        ref = make_ref()
        {:ok, [{:make, ref, initMFA} | flatCases ++ [{:make, ref, finMFA}]], st}

      {:error, _Reason} = error ->
        error
    end
  end

  defp collect_cases({:repeat, {module, case__}, repeat}, st, mode) do
    case (try do
            collect_case([case__], r_cc(st, mod: module), [], mode)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, [{^module, ^case__}], _} ->
        {:ok, [{:repeat, {module, case__}, repeat}], st}

      other ->
        {:error, other}
    end
  end

  defp collect_cases({module, cases}, st, mode)
       when is_list(cases) do
    case (try do
            collect_case(cases, r_cc(st, mod: module), [], mode)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      result = {:ok, _, _} ->
        result

      other ->
        {:error, other}
    end
  end

  defp collect_cases({_Mod, _Case} = spec, st, mode) do
    collect_case(spec, st, mode)
  end

  defp collect_cases({_Mod, _Case, _Args} = spec, st, mode) do
    collect_case(spec, st, mode)
  end

  defp collect_cases(case__, st, mode)
       when is_atom(case__) and
              is_atom(r_cc(st, :mod)) do
    collect_case({r_cc(st, :mod), case__}, st, mode)
  end

  defp collect_cases(other, st, _Mode) do
    {:error, {:bad_subtest_spec, r_cc(st, :mod), other}}
  end

  defp collect_case({mod, {:conf, _, _, _, _} = conf}, st, mode) do
    collect_case_invoke(mod, conf, [], st, mode)
  end

  defp collect_case(mFA, st, mode) do
    case in_skip_list(mFA, r_cc(st, :skip)) do
      {true, comment} when comment != :make_failed ->
        {:ok, [{:skip_case, {mFA, comment}, mode}], st}

      _ ->
        case mFA do
          {mod, case__} ->
            collect_case_invoke(mod, case__, mFA, st, mode)

          {_Mod, _Case, _Args} ->
            {:ok, [mFA], st}
        end
    end
  end

  defp collect_case([], st, acc, _Mode) do
    {:ok, acc, st}
  end

  defp collect_case([case__ | cases], st, acc, mode) do
    {:ok, flatCases, newSt} = collect_case({r_cc(st, :mod), case__}, st, mode)
    collect_case(cases, newSt, acc ++ flatCases, mode)
  end

  defp collect_case_invoke(mod, case__, mFA, st, mode) do
    case get_fw_mod(:undefined) do
      :undefined ->
        case (try do
                apply(mod, case__, [:suite])
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            {:ok, [mFA], st}

          suite ->
            collect_subcases(mod, case__, mFA, st, suite, mode)
        end

      _ ->
        suite = :test_server_sup.framework_call(:get_suite, [mod, case__], [])
        collect_subcases(mod, case__, mFA, st, suite, mode)
    end
  end

  defp collect_subcases(mod, case__, mFA, st, suite, mode) do
    case suite do
      [] when case__ == :all ->
        {:ok, [], st}

      [] when :erlang.element(1, case__) == :conf ->
        {:ok, [], st}

      [] ->
        {:ok, [mFA], st}

      {:req, reqList} ->
        collect_case_deny(mod, case__, mFA, reqList, [], st, mode)

      {:req, reqList, subCases} ->
        collect_case_deny(mod, case__, mFA, reqList, subCases, st, mode)

      {skip, reason} when skip == :skip or skip == :skipped ->
        {:ok, [{:skip_case, {mFA, reason}, mode}], st}

      {:error, reason} ->
        throw(reason)

      subCases ->
        collect_case_subcases(mod, case__, subCases, st, mode)
    end
  end

  defp collect_case_subcases(mod, case__, subCases, st0, mode) do
    oldMod = r_cc(st0, :mod)

    case collect_cases(subCases, r_cc(st0, mod: mod), mode) do
      {:ok, flatCases, st} ->
        {:ok, flatCases, r_cc(st, mod: oldMod)}

      {:error, reason} ->
        {:error, {{mod, case__}, reason}}
    end
  end

  defp collect_files(dir, pattern, st, mode) do
    {:ok, cwd} = :file.get_cwd()
    dir1 = :filename.join(cwd, dir)
    wc = :filename.join([dir1, pattern ++ '{.erl,' ++ :code.objfile_extension() ++ '}'])

    case (try do
            :filelib.wildcard(wc)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, reason} ->
        :io.format('Could not collect files: ~tp~n', [reason])
        {:error, {:collect_fail, dir, pattern}}

      files ->
        mods =
          :lists.foldl(
            fn file, acc ->
              mod = fullname_to_mod(file)

              case :lists.member(mod, acc) do
                true ->
                  acc

                false ->
                  [mod | acc]
              end
            end,
            [],
            files
          )

        tests =
          for mod <- :lists.sort(mods) do
            {mod, :all}
          end

        collect_cases(tests, st, mode)
    end
  end

  defp fullname_to_mod(path) when is_list(path) do
    :erlang.list_to_atom(:filename.rootname(:filename.basename(path)))
  end

  defp collect_case_deny(mod, case__, mFA, reqList, subCases, st, mode) do
    case {check_deny(reqList, r_cc(st, :skip)), subCases} do
      {{:denied, comment}, _SubCases} ->
        {:ok, [{:skip_case, {mFA, comment}, mode}], st}

      {:granted, []} ->
        {:ok, [mFA], st}

      {:granted, ^subCases} ->
        collect_case_subcases(mod, case__, subCases, st, mode)
    end
  end

  defp check_deny([req | reqs], denyList) do
    case check_deny_req(req, denyList) do
      {:denied, _Comment} = denied ->
        denied

      :granted ->
        check_deny(reqs, denyList)
    end
  end

  defp check_deny([], _DenyList) do
    :granted
  end

  defp check_deny(req, denyList) do
    check_deny([req], denyList)
  end

  defp check_deny_req({req, val}, denyList) do
    case :lists.keysearch(req, 1, denyList) do
      {:value, {_Req, denyVal}} when val >= denyVal ->
        {:denied, :io_lib.format('Requirement ~tp=~tp', [req, val])}

      _ ->
        check_deny_req(req, denyList)
    end
  end

  defp check_deny_req(req, denyList) do
    case :lists.member(req, denyList) do
      true ->
        {:denied, :io_lib.format('Requirement ~tp', [req])}

      false ->
        :granted
    end
  end

  defp in_skip_list(
         {mod, {:conf, props, initMF, _CaseList, _FinMF}},
         skipList
       ) do
    case in_skip_list(initMF, skipList) do
      {true, _} = yes ->
        yes

      _ ->
        case :proplists.get_value(:name, props) do
          :undefined ->
            false

          name ->
            toSkip =
              :lists.flatmap(
                fn
                  {m, {:conf, sProps, _, sCaseList, _}, cmt}
                  when m == mod ->
                    case :proplists.get_value(
                           :name,
                           sProps
                         ) do
                      :all ->
                        [{m, :all, cmt}]

                      ^name ->
                        case sCaseList do
                          :all ->
                            [{m, :all, cmt}]

                          _ ->
                            for f <- sCaseList do
                              {m, f, cmt}
                            end
                        end

                      _ ->
                        []
                    end

                  _ ->
                    []
                end,
                skipList
              )

            case toSkip do
              [] ->
                false

              _ ->
                case :lists.keysearch(:all, 2, toSkip) do
                  {:value, {_, _, cmt}} ->
                    {true, name, cmt}

                  _ ->
                    {true, toSkip, ''}
                end
            end
        end
    end
  end

  defp in_skip_list({mod, func, _Args}, skipList) do
    in_skip_list({mod, func}, skipList)
  end

  defp in_skip_list({mod, func}, [{mod, funcs, comment} | skipList])
       when is_list(funcs) do
    case :lists.member(func, funcs) do
      true ->
        {true, comment}

      _ ->
        in_skip_list({mod, func}, skipList)
    end
  end

  defp in_skip_list(
         {mod, func},
         [{mod, func, comment} | _SkipList]
       ) do
    {true, comment}
  end

  defp in_skip_list({mod, _Func}, [{mod, comment} | _SkipList]) do
    {true, comment}
  end

  defp in_skip_list({mod, func}, [_ | skipList]) do
    in_skip_list({mod, func}, skipList)
  end

  defp in_skip_list(_, []) do
    false
  end

  defp init_props(props) do
    case get_repeat(props) do
      repeat = {_RepType, n} when n < 2 ->
        cond do
          n == 0 ->
            {:error, {:invalid_property, repeat}}

          true ->
            :lists.delete(repeat, props)
        end

      _ ->
        props
    end
  end

  defp keep_name(props) do
    :lists.filter(
      fn
        {:name, _} ->
          true

        {:suite, _} ->
          true

        _ ->
          false
      end,
      props
    )
  end

  def get_target_info() do
    controller_call(:get_target_info)
  end

  def start_node(name, type, options) do
    t = 10 * 20000 * :test_server.timetrap_scale_factor()
    format(:minor, 'Attempt to start ~w node ~tp with options ~tp', [type, name, options])

    case controller_call(
           {:start_node, name, type, options},
           t
         ) do
      {{:ok, nodename}, host, cmd, info, warning} ->
        format(:minor, 'Successfully started node ~w on ~tp with command: ~ts', [
          nodename,
          host,
          cmd
        ])

        format(:major, '=node_start    ~w', [nodename])

        case info do
          [] ->
            :ok

          _ ->
            format(:minor, info)
        end

        case warning do
          [] ->
            :ok

          _ ->
            format(1, warning)
            format(:minor, warning)
        end

        {:ok, nodename}

      {:fail, {ret, host, cmd}} ->
        format(:minor, 'Failed to start node ~tp on ~tp with command: ~ts~nReason: ~tp', [
          name,
          host,
          cmd,
          ret
        ])

        {:fail, ret}

      {ret, :undefined, :undefined} ->
        format(:minor, 'Failed to start node ~tp: ~tp', [name, ret])
        ret

      {ret, host, cmd} ->
        format(:minor, 'Failed to start node ~tp on ~tp with command: ~ts~nReason: ~tp', [
          name,
          host,
          cmd,
          ret
        ])

        ret
    end
  end

  def wait_for_node(slave) do
    t = 10000 * :test_server.timetrap_scale_factor()

    case (try do
            controller_call({:wait_for_node, slave}, t)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, {:timeout, _}} ->
        {:error, :timeout}

      :ok ->
        :ok
    end
  end

  def is_release_available(release) do
    controller_call({:is_release_available, release})
  end

  def stop_node(slave) do
    controller_call({:stop_node, slave})
  end

  def i() do
    hformat('Pid', 'Initial Call', 'Current Function', 'Reducts', 'Msgs')
    line = :lists.duplicate(27, '-')
    hformat(line, line, line, line, line)
    display_info(:erlang.processes(), 0, 0)
  end

  def p(a, b, c) do
    pinfo(ts_pid(a, b, c))
  end

  def p(x) when is_atom(x) do
    pinfo(:erlang.whereis(x))
  end

  def p({a, b, c}) do
    pinfo(ts_pid(a, b, c))
  end

  def p(x) do
    pinfo(x)
  end

  def t() do
    t(:wall_clock)
  end

  def t(x) do
    :erlang.element(1, :erlang.statistics(x))
  end

  def pi(item, x) do
    :lists.keysearch(item, 1, p(x))
  end

  def pi(item, a, b, c) do
    :lists.keysearch(item, 1, p(a, b, c))
  end

  defp ts_pid(x, y, z)
       when is_integer(x) and
              is_integer(y) and is_integer(z) do
    :erlang.list_to_pid(
      '<' ++
        :erlang.integer_to_list(x) ++
        '.' ++ :erlang.integer_to_list(y) ++ '.' ++ :erlang.integer_to_list(z) ++ '>'
    )
  end

  defp display_info([pid | t], r, m) do
    case pinfo(pid) do
      :undefined ->
        display_info(t, r, m)

      info ->
        call = fetch(:initial_call, info)

        curr =
          case fetch(:current_function, info) do
            {mod, f, args} when is_list(args) ->
              {mod, f, length(args)}

            other ->
              other
          end

        reds = fetch(:reductions, info)
        lM = fetch(:message_queue_len, info)

        pformat(
          :io_lib.format('~w', [pid]),
          :io_lib.format('~tw', [call]),
          :io_lib.format('~tw', [curr]),
          reds,
          lM
        )

        display_info(t, r + reds, m + lM)
    end
  end

  defp display_info([], r, m) do
    line = :lists.duplicate(27, '-')
    hformat(line, line, line, line, line)
    pformat('Total', '', '', r, m)
  end

  defp hformat(a1, a2, a3, a4, a5) do
    :io.format('~-10s ~-27s ~-27s ~8s ~4s~n', [a1, a2, a3, a4, a5])
  end

  defp pformat(a1, a2, a3, a4, a5) do
    :io.format('~-10s ~-27s ~-27s ~8w ~4w~n', [a1, a2, a3, a4, a5])
  end

  defp fetch(key, info) do
    case :lists.keysearch(key, 1, info) do
      {:value, {_, val}} ->
        val

      _ ->
        0
    end
  end

  defp pinfo(p) do
    node = node()

    case node(p) do
      ^node ->
        :erlang.process_info(p)

      _ ->
        :rpc.call(node(p), :erlang, :process_info, [p])
    end
  end

  defp start_cover(r_cover() = coverInfo) do
    cover_compile(coverInfo)
  end

  defp start_cover({:log, coverLogDir} = coverInfo) do
    :erlang.put(:test_server_cover_log_dir, coverLogDir)
    {:ok, coverInfo}
  end

  defp cover_compile(coverInfo) do
    :test_server.cover_compile(coverInfo)
  end

  defp read_cover_file(:none) do
    {[], [], []}
  end

  defp read_cover_file(coverFile) do
    case :file.consult(coverFile) do
      {:ok, list} ->
        case check_cover_file(list, [], [], []) do
          {:ok, exclude, include, cross} ->
            {exclude, include, cross}

          :error ->
            :io.fwrite('Faulty format of CoverFile ~tp\n', [coverFile])
            {[], [], []}
        end

      {:error, reason} ->
        :io.fwrite('Can\'t read CoverFile ~ts\nReason: ~tp\n', [coverFile, reason])
        {[], [], []}
    end
  end

  defp check_cover_file([{:exclude, :all} | rest], _, include, cross) do
    check_cover_file(rest, :all, include, cross)
  end

  defp check_cover_file([{:exclude, exclude} | rest], _, include, cross) do
    case :lists.all(
           fn m ->
             is_atom(m)
           end,
           exclude
         ) do
      true ->
        check_cover_file(rest, exclude, include, cross)

      false ->
        :error
    end
  end

  defp check_cover_file([{:include, include} | rest], exclude, _, cross) do
    case :lists.all(
           fn m ->
             is_atom(m)
           end,
           include
         ) do
      true ->
        check_cover_file(rest, exclude, include, cross)

      false ->
        :error
    end
  end

  defp check_cover_file([{:cross, cross} | rest], exclude, include, _) do
    case check_cross(cross) do
      true ->
        check_cover_file(rest, exclude, include, cross)

      false ->
        :error
    end
  end

  defp check_cover_file([], exclude, include, cross) do
    {:ok, exclude, include, cross}
  end

  defp check_cross([{tag, modules} | rest]) do
    case :lists.all(
           fn m ->
             is_atom(m)
           end,
           [tag | modules]
         ) do
      true ->
        check_cross(rest)

      false ->
        false
    end
  end

  defp check_cross([]) do
    true
  end

  defp stop_cover(r_cover() = coverInfo, testDir) do
    cover_analyse(coverInfo, testDir)
    :ok
  end

  defp stop_cover(_CoverInfo, _TestDir) do
    :ok
  end

  defp make_relative(absDir, vsDir) do
    dirTokens = :filename.split(absDir)
    vsTokens = :filename.split(vsDir)
    :filename.join(make_relative1(dirTokens, vsTokens))
  end

  defp make_relative1([t | dirTs], [t | vsTs]) do
    make_relative1(dirTs, vsTs)
  end

  defp make_relative1(last = [_File], []) do
    last
  end

  defp make_relative1(last = [_File], vsTs) do
    ups =
      for _ <- vsTs do
        '../'
      end

    ups ++ last
  end

  defp make_relative1(dirTs, []) do
    dirTs
  end

  defp make_relative1(dirTs, vsTs) do
    ups =
      for _ <- vsTs do
        '../'
      end

    ups ++ dirTs
  end

  def cover_analyse(coverInfo, testDir) do
    write_default_cross_coverlog(testDir)

    {:ok, coverLog} =
      open_html_file(
        :filename.join(
          testDir,
          'cover.html'
        )
      )

    write_coverlog_header(coverLog)
    r_cover(app: app, file: coverFile, excl: excluded, cross: cross) = coverInfo
    :io.fwrite(coverLog, '<h1>Coverage for application \'~w\'</h1>\n', [app])

    :io.fwrite(coverLog, '<p><a href="~ts">Coverdata collected over all tests</a></p>', [
      'cross_cover.html'
    ])

    :io.fwrite(coverLog, '<p>CoverFile: <code>~tp</code>\n', [coverFile])
    :ok = write_cross_cover_info(testDir, cross)

    case length(:cover.imported_modules()) do
      imps when imps > 0 ->
        :io.fwrite(coverLog, '<p>Analysis includes data from ~w imported module(s).\n', [imps])

      _ ->
        :ok
    end

    :io.fwrite(coverLog, '<p>Excluded module(s): <code>~tp</code>\n', [excluded])

    coverage =
      :test_server.cover_analyse(
        testDir,
        coverInfo
      )

    :ok =
      write_binary_file(
        :filename.join(testDir, 'cover.log'),
        :erlang.term_to_binary(coverage)
      )

    case :lists.filter(
           fn
             {_M, {_, _, _}} ->
               false

             _ ->
               true
           end,
           coverage
         ) do
      [] ->
        :ok

      bad ->
        :io.fwrite(coverLog, '<p>Analysis failed for ~w module(s): <code>~w</code>\n', [
          length(bad),
          for {badM, {_, _Why}} <- bad do
            badM
          end
        ])
    end

    totPercent =
      write_cover_result_table(
        coverLog,
        coverage
      )

    :ok =
      write_binary_file(
        :filename.join(testDir, 'total_cover.log'),
        :erlang.term_to_binary(totPercent)
      )
  end

  def cross_cover_analyse(analyse, tagDirs0) do
    tagDirs = get_latest_run_dirs(tagDirs0)
    tagMods = get_all_cross_info(tagDirs, [])
    tagDirMods = add_cross_modules(tagMods, tagDirs)
    coverdataFiles = get_coverdata_files(tagDirMods)

    :lists.foreach(
      fn cDF ->
        :cover.import(cDF)
      end,
      coverdataFiles
    )

    :io.fwrite('Cover analysing...\n', [])

    detailsFun =
      case analyse do
        :details ->
          fn dir, m ->
            outFile =
              :filename.join(
                dir,
                :erlang.atom_to_list(m) ++ '.CROSS_COVER.html'
              )

            case :cover.analyse_to_file(m, outFile, [:html]) do
              {:ok, _} ->
                {:file, outFile}

              error ->
                error
            end
          end

        _ ->
          fn _, _ ->
            :undefined
          end
      end

    coverage = analyse_tests(tagDirMods, detailsFun, [])
    :cover.stop()
    write_cross_cover_logs(coverage, tagDirMods)
  end

  defp write_cross_cover_info(_Dir, []) do
    :ok
  end

  defp write_cross_cover_info(dir, cross) do
    write_binary_file(
      :filename.join(dir, 'cross_cover.info'),
      :erlang.term_to_binary(cross)
    )
  end

  defp write_cross_cover_logs([{tag, coverage} | t], tagDirMods) do
    case :lists.keyfind(tag, 1, tagDirMods) do
      {_, dir, mods} when mods !== [] ->
        :ok =
          write_binary_file(
            :filename.join(dir, 'cross_cover.log'),
            :erlang.term_to_binary(coverage)
          )

        coverLogName = :filename.join(dir, 'cross_cover.html')
        {:ok, coverLog} = open_html_file(coverLogName)
        write_coverlog_header(coverLog)
        :io.fwrite(coverLog, '<h1>Coverage results for \'~w\' from all tests</h1>\n', [tag])
        write_cover_result_table(coverLog, coverage)
        :io.fwrite('Written file ~tp\n', [coverLogName])

      _ ->
        :ok
    end

    write_cross_cover_logs(t, tagDirMods)
  end

  defp write_cross_cover_logs([], _) do
    :io.fwrite('done\n', [])
  end

  defp get_latest_run_dirs([{tag, dir} | rest]) do
    [
      {tag, get_latest_run_dir(dir)}
      | get_latest_run_dirs(rest)
    ]
  end

  defp get_latest_run_dirs([]) do
    []
  end

  defp get_latest_run_dir(dir) do
    case :filelib.wildcard(:filename.join(dir, 'run.[1-2]*')) do
      [] ->
        dir

      [h | t] ->
        get_latest_dir(t, h)
    end
  end

  defp get_latest_dir([h | t], latest) when h > latest do
    get_latest_dir(t, h)
  end

  defp get_latest_dir([_ | t], latest) do
    get_latest_dir(t, latest)
  end

  defp get_latest_dir([], latest) do
    latest
  end

  defp get_all_cross_info([{_Tag, dir} | rest], acc) do
    case :file.read_file(:filename.join(dir, 'cross_cover.info')) do
      {:ok, bin} ->
        tagMods = :erlang.binary_to_term(bin)
        get_all_cross_info(rest, tagMods ++ acc)

      _ ->
        get_all_cross_info(rest, acc)
    end
  end

  defp get_all_cross_info([], acc) do
    acc
  end

  defp add_cross_modules(tagMods, tagDirs) do
    do_add_cross_modules(
      tagMods,
      for {tag, dir} <- tagDirs do
        {tag, dir, []}
      end
    )
  end

  defp do_add_cross_modules([{tag, mods1} | tagMods], tagDirMods) do
    newTagDirMods =
      case :lists.keytake(tag, 1, tagDirMods) do
        {:value, {^tag, dir, mods}, rest} ->
          [
            {tag, dir, :lists.umerge(:lists.sort(mods1), mods)}
            | rest
          ]

        false ->
          tagDirMods
      end

    do_add_cross_modules(tagMods, newTagDirMods)
  end

  defp do_add_cross_modules([], tagDirMods) do
    for {tag, dir, mods} <- tagDirMods do
      {tag, dir, :lists.reverse(mods)}
    end
  end

  defp get_coverdata_files(tagDirMods) do
    :lists.flatmap(
      fn {_, latestDir, _} ->
        :filelib.wildcard(:filename.join(latestDir, 'all.coverdata'))
      end,
      tagDirMods
    )
  end

  defp analyse_tests([{tag, lastTest, modules} | t], detailsFun, acc) do
    cov = analyse_modules(lastTest, modules, detailsFun, [])
    analyse_tests(t, detailsFun, [{tag, cov} | acc])
  end

  defp analyse_tests([], _DetailsFun, acc) do
    acc
  end

  defp analyse_modules(dir, [m | modules], detailsFun, acc) do
    {:ok, {^m, {cov, notCov}}} = :cover.analyse(m, :module)
    acc1 = [{m, {cov, notCov, detailsFun.(dir, m)}} | acc]
    analyse_modules(dir, modules, detailsFun, acc1)
  end

  defp analyse_modules(_Dir, [], _DetailsFun, acc) do
    acc
  end

  defp write_coverlog_header(coverLog) do
    case (try do
            :io.put_chars(coverLog, html_header('Coverage results'))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, reason} ->
        :io.format(
          '\n\nERROR: Could not write normal heading in coverlog.\nCoverLog: ~tw\nReason: ~tp\n',
          [coverLog, reason]
        )

        :io.format(coverLog, '<html><body>\n', [])

      _ ->
        :ok
    end
  end

  defp format_analyse(m, cov, notCov, :undefined) do
    :io_lib.fwrite(
      '<tr><td>~w</td><td align=right>~w %</td><td align=right>~w</td><td align=right>~w</td></tr>\n',
      [m, pc(cov, notCov), cov, notCov]
    )
  end

  defp format_analyse(m, cov, notCov, {:file, file}) do
    :io_lib.fwrite(
      '<tr><td><a href="~ts">~w</a></td><td align=right>~w %</td><td align=right>~w</td><td align=right>~w</td></tr>\n',
      [uri_encode(:filename.basename(file)), m, pc(cov, notCov), cov, notCov]
    )
  end

  defp format_analyse(m, cov, notCov, {:lines, lines}) do
    coverOutName = :erlang.atom_to_list(m) ++ '.COVER.html'
    {:ok, coverOut} = open_html_file(coverOutName)
    write_not_covered(coverOut, m, lines)
    :ok = :file.close(coverOut)

    :io_lib.fwrite(
      '<tr><td><a href="~ts">~w</a></td><td align=right>~w %</td><td align=right>~w</td><td align=right>~w</td></tr>\n',
      [uri_encode(coverOutName), m, pc(cov, notCov), cov, notCov]
    )
  end

  defp format_analyse(m, cov, notCov, {:error, _}) do
    :io_lib.fwrite(
      '<tr><td>~w</td><td align=right>~w %</td><td align=right>~w</td><td align=right>~w</td></tr>\n',
      [m, pc(cov, notCov), cov, notCov]
    )
  end

  defp pc(0, 0) do
    0
  end

  defp pc(cov, notCov) do
    round(cov / (cov + notCov) * 100)
  end

  defp write_not_covered(coverOut, m, lines) do
    :io.put_chars(
      coverOut,
      html_header('Coverage results for ' ++ :erlang.atom_to_list(m))
    )

    :io.fwrite(
      coverOut,
      'The following lines in module ~w are not covered:\n<table border=3 cellpadding=5>\n<th>Line Number</th>\n',
      [m]
    )

    :lists.foreach(
      fn
        {{_M, line}, {0, 1}} ->
          :io.fwrite(coverOut, '<tr><td>~w</td></tr>\n', [line])

        _ ->
          :ok
      end,
      lines
    )

    :io.put_chars(coverOut, '</table>\n</body>\n</html>\n')
  end

  defp write_default_coverlog(testDir) do
    {:ok, coverLog} =
      open_html_file(
        :filename.join(
          testDir,
          'cover.html'
        )
      )

    write_coverlog_header(coverLog)
    :io.put_chars(coverLog, 'Cover tool is not used\n</body></html>\n')
    :ok = :file.close(coverLog)
  end

  defp write_default_cross_coverlog(testDir) do
    {:ok, crossCoverLog} =
      open_html_file(
        :filename.join(
          testDir,
          'cross_cover.html'
        )
      )

    write_coverlog_header(crossCoverLog)

    :io.put_chars(crossCoverLog, [
      'No cross cover modules exist for this application,',
      xhtml('<br>', '<br />'),
      'or cross cover analysis is not completed.\n</body></html>\n'
    ])

    :ok = :file.close(crossCoverLog)
  end

  defp write_cover_result_table(coverLog, coverage) do
    :io.fwrite(
      coverLog,
      '<p><table border=3 cellpadding=5>\n<tr><th>Module</th><th>Covered (%)</th><th>Covered (Lines)</th><th>Not covered (Lines)</th>\n',
      []
    )

    {totCov, totNotCov} =
      :lists.foldl(
        fn
          {m, {cov, notCov, details}}, {accCov, accNotCov} ->
            str = format_analyse(m, cov, notCov, details)
            :io.fwrite(coverLog, '~ts', [str])
            {accCov + cov, accNotCov + notCov}

          {_M, {:error, _Reason}}, {accCov, accNotCov} ->
            {accCov, accNotCov}
        end,
        {0, 0},
        coverage
      )

    totPercent = pc(totCov, totNotCov)

    :io.fwrite(
      coverLog,
      '<tr><th align=left>Total</th><th align=right>~w %</th><th align=right>~w</th><th align=right>~w</th></tr>\n</table>\n</body>\n</html>\n',
      [totPercent, totCov, totNotCov]
    )

    :ok = :file.close(coverLog)
    totPercent
  end

  defp html_header(title) do
    [
      '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN">\n<!-- autogenerated by \'',
      :erlang.atom_to_list(:test_server_ctrl),
      '\'. -->\n<html>\n<head>\n<title>',
      title,
      '</title>\n<meta http-equiv="cache-control" content="no-cache"></meta>\n<meta http-equiv="content-type" content="text/html; charset=utf-8"></meta>\n</head>\n<body bgcolor="white" text="black" link="blue" vlink="purple" alink="red">\n'
    ]
  end

  defp html_header(title, meta) do
    [
      '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN">\n<!-- autogenerated by \'',
      :erlang.atom_to_list(:test_server_ctrl),
      '\'. -->\n<html>\n<head>\n<title>',
      title,
      '</title>\n'
    ] ++ meta ++ ['</head>\n']
  end

  defp open_html_file(file) do
    open_utf8_file(file)
  end

  defp open_html_file(file, opts) do
    open_utf8_file(file, opts)
  end

  defp write_html_file(file, content) do
    write_file(file, content, :utf8)
  end

  defp open_utf8_file(file) do
    case :file.open(
           file,
           allOpts = [:write, {:encoding, :utf8}]
         ) do
      {:error, reason} ->
        {:error, {reason, {file, allOpts}}}

      result ->
        result
    end
  end

  defp open_utf8_file(file, opts) do
    case :file.open(
           file,
           allOpts = [{:encoding, :utf8} | opts]
         ) do
      {:error, reason} ->
        {:error, {reason, {file, allOpts}}}

      result ->
        result
    end
  end

  defp write_file(file, content, :latin1) do
    :file.write_file(file, content)
  end

  defp write_file(file, content, :utf8) do
    write_binary_file(
      file,
      :unicode.characters_to_binary(content)
    )
  end

  defp write_binary_file(file, content) do
    :file.write_file(file, content)
  end

  def uri_encode(file) do
    encoding = :file.native_name_encoding()
    uri_encode(file, encoding)
  end

  def uri_encode(file, encoding) do
    components = :filename.split(file)

    :filename.join(
      for c <- components do
        uri_encode_comp(c, encoding)
      end
    )
  end

  defp uri_encode_comp([char | chars], encoding) do
    reserved = :sets.is_element(char, reserved())

    case (char > 127 and encoding == :latin1) or reserved do
      true ->
        [
          ?%
          | :erlang.integer_to_list(
              char,
              16
            )
        ] ++ uri_encode_comp(chars, encoding)

      false ->
        [char | uri_encode_comp(chars, encoding)]
    end
  end

  defp uri_encode_comp([], _) do
    []
  end

  defp reserved() do
    :sets.from_list([
      ?;,
      ?&,
      ?=,
      ?+,
      ?,,
      ??,
      ?#,
      ?[,
      ?],
      ?<,
      ?>,
      ?",
      ?{,
      ?},
      ?|,
      ?\\,
      ?',
      ?^,
      ?%,
      ?\s
    ])
  end

  defp encoding(file) do
    case :epp.read_encoding(file) do
      :none ->
        :epp.default_encoding()

      e ->
        e
    end
  end

  defp check_repeat_testcase(case__, result, cases, [
         {ref, [{:repeat, repeatData0}], startTime} | mode0
       ]) do
    case do_update_repeat_data(result, repeatData0) do
      false ->
        {cases, mode0}

      repeatData ->
        {[case__ | cases], [{ref, [{:repeat, repeatData}], startTime} | mode0]}
    end
  end

  defp check_repeat_testcase(_, _, cases, mode) do
    {cases, mode}
  end

  defp do_update_repeat_data(_, {rT, n, n}) when is_integer(n) do
    report_repeat_testcase(n, n)
    report_stop_repeat_testcase(:done, {rT, n})
    false
  end

  defp do_update_repeat_data(:ok, {:repeat_until_ok = rT, m, n}) do
    report_repeat_testcase(m, n)
    report_stop_repeat_testcase(rT, {rT, n})
    false
  end

  defp do_update_repeat_data(:failed, {:repeat_until_fail = rT, m, n}) do
    report_repeat_testcase(m, n)
    report_stop_repeat_testcase(rT, {rT, n})
    false
  end

  defp do_update_repeat_data(_, {rT, m, n}) when is_integer(m) do
    report_repeat_testcase(m, n)
    {rT, m + 1, n}
  end

  defp do_update_repeat_data(_, {_, m, n} = repeatData) do
    report_repeat_testcase(m, n)
    repeatData
  end

  defp report_stop_repeat_testcase(reason, repVal) do
    print(:minor, '~n*** Stopping test case repeat operation: ~w', [reason])
    print(1, 'Stopping test case repeat operation: ~w', [repVal])
  end

  defp report_repeat_testcase(m, :forever) do
    print(:minor, '~n=== Repeated test case: ~w of infinity', [m])
  end

  defp report_repeat_testcase(m, n) do
    print(:minor, '~n=== Repeated test case: ~w of ~w', [m, n])
  end
end
