defmodule :m_ct do
  use Bitwise
  require Record

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

  def install(opts) do
    :ct_run.install(opts)
  end

  def run(testDir, suite, cases) do
    :ct_run.run(testDir, suite, cases)
  end

  def run(testDir, suite) do
    :ct_run.run(testDir, suite)
  end

  def run(testDirs) do
    :ct_run.run(testDirs)
  end

  def run_test(opts) do
    :ct_run.run_test(opts)
  end

  def run_testspec(testSpec) do
    :ct_run.run_testspec(testSpec)
  end

  def step(testDir, suite, case__) do
    :ct_run.step(testDir, suite, case__)
  end

  def step(testDir, suite, case__, opts) do
    :ct_run.step(testDir, suite, case__, opts)
  end

  def start_interactive() do
    _ = :ct_util.start(:interactive)
    :ok
  end

  def stop_interactive() do
    :ct_util.stop(:normal)
    :ok
  end

  def require(required) do
    :ct_config.require(required)
  end

  def require(name, required) do
    :ct_config.require(name, required)
  end

  def get_config(required) do
    :ct_config.get_config(required, :undefined, [])
  end

  def get_config(required, default) do
    :ct_config.get_config(required, default, [])
  end

  def get_config(required, default, opts) do
    :ct_config.get_config(required, default, opts)
  end

  def reload_config(required) do
    :ct_config.reload_config(required)
  end

  def get_testspec_terms() do
    case :ct_util.get_testdata(:testspec) do
      :undefined ->
        :undefined

      currSpecRec ->
        :ct_testspec.testspec_rec2list(currSpecRec)
    end
  end

  def get_testspec_terms(tags) do
    case :ct_util.get_testdata(:testspec) do
      :undefined ->
        :undefined

      currSpecRec ->
        :ct_testspec.testspec_rec2list(tags, currSpecRec)
    end
  end

  def escape_chars(ioList) do
    :ct_logs.escape_chars(ioList)
  end

  def escape_chars(format, args) do
    try do
      :io_lib.format(format, args)
    catch
      _, reason ->
        {:error, reason}
    else
      ioList ->
        :ct_logs.escape_chars(ioList)
    end
  end

  def log(format) do
    log(:default, 50, format, [], [])
  end

  def log(x1, x2) do
    {category, importance, format, args} =
      cond do
        is_atom(x1) ->
          {x1, 50, x2, []}

        is_integer(x1) ->
          {:default, x1, x2, []}

        is_list(x1) ->
          {:default, 50, x1, x2}
      end

    log(category, importance, format, args, [])
  end

  def log(x1, x2, x3) do
    {category, importance, format, args, opts} =
      cond do
        is_atom(x1) and
            is_integer(x2) ->
          {x1, x2, x3, [], []}

        is_atom(x1) and
            is_list(x2) ->
          {x1, 50, x2, x3, []}

        is_integer(x1) ->
          {:default, x1, x2, x3, []}

        is_list(x1) and
            is_list(x2) ->
          {:default, 50, x1, x2, x3}
      end

    log(category, importance, format, args, opts)
  end

  def log(x1, x2, x3, x4) do
    {category, importance, format, args, opts} =
      cond do
        is_atom(x1) and
            is_integer(x2) ->
          {x1, x2, x3, x4, []}

        is_atom(x1) and
            is_list(x2) ->
          {x1, 50, x2, x3, x4}

        is_integer(x1) ->
          {:default, x1, x2, x3, x4}
      end

    log(category, importance, format, args, opts)
  end

  def log(category, importance, format, args, opts) do
    :ct_logs.tc_log(category, importance, format, args, opts)
  end

  def print(format) do
    print(:default, 50, format, [], [])
  end

  def print(x1, x2) do
    {category, importance, format, args} =
      cond do
        is_atom(x1) ->
          {x1, 50, x2, []}

        is_integer(x1) ->
          {:default, x1, x2, []}

        is_list(x1) ->
          {:default, 50, x1, x2}
      end

    print(category, importance, format, args, [])
  end

  def print(x1, x2, x3) do
    {category, importance, format, args, opts} =
      cond do
        is_atom(x1) and
            is_integer(x2) ->
          {x1, x2, x3, [], []}

        is_atom(x1) and
            is_list(x2) ->
          {x1, 50, x2, x3, []}

        is_integer(x1) ->
          {:default, x1, x2, x3, []}

        is_list(x1) and
            is_list(x2) ->
          {:default, 50, x1, x2, x3}
      end

    print(category, importance, format, args, opts)
  end

  def print(x1, x2, x3, x4) do
    {category, importance, format, args, opts} =
      cond do
        is_atom(x1) and
            is_integer(x2) ->
          {x1, x2, x3, x4, []}

        is_atom(x1) and
            is_list(x2) ->
          {x1, 50, x2, x3, x4}

        is_integer(x1) ->
          {:default, x1, x2, x3, x4}
      end

    print(category, importance, format, args, opts)
  end

  def print(category, importance, format, args, opts) do
    :ct_logs.tc_print(category, importance, format, args, opts)
  end

  def pal(format) do
    pal(:default, 50, format, [])
  end

  def pal(x1, x2) do
    {category, importance, format, args} =
      cond do
        is_atom(x1) ->
          {x1, 50, x2, []}

        is_integer(x1) ->
          {:default, x1, x2, []}

        is_list(x1) ->
          {:default, 50, x1, x2}
      end

    pal(category, importance, format, args, [])
  end

  def pal(x1, x2, x3) do
    {category, importance, format, args, opts} =
      cond do
        is_atom(x1) and
            is_integer(x2) ->
          {x1, x2, x3, [], []}

        is_atom(x1) and
            is_list(x2) ->
          {x1, 50, x2, x3, []}

        is_integer(x1) ->
          {:default, x1, x2, x3, []}

        is_list(x1) and
            is_list(x2) ->
          {:default, 50, x1, x2, x3}
      end

    pal(category, importance, format, args, opts)
  end

  def pal(x1, x2, x3, x4) do
    {category, importance, format, args, opts} =
      cond do
        is_atom(x1) and
            is_integer(x2) ->
          {x1, x2, x3, x4, []}

        is_atom(x1) and
            is_list(x2) ->
          {x1, 50, x2, x3, x4}

        is_integer(x1) ->
          {:default, x1, x2, x3, x4}
      end

    pal(category, importance, format, args, opts)
  end

  def pal(category, importance, format, args, opts) do
    :ct_logs.tc_pal(category, importance, format, args, opts)
  end

  def set_verbosity(category, level) do
    :ct_util.set_verbosity({category, level})
  end

  def get_verbosity(category) do
    :ct_util.get_verbosity(category)
  end

  def capture_start() do
    :test_server.capture_start()
  end

  def capture_stop() do
    :test_server.capture_stop()
  end

  def capture_get() do
    capture_get([:default])
  end

  def capture_get([exclCat | exclCategories]) do
    strs = :test_server.capture_get()

    catsStr = [
      :erlang.atom_to_list(exclCat)
      | for eC <- exclCategories do
          [?| | :erlang.atom_to_list(eC)]
        end
    ]

    {:ok, mP} =
      :re.compile(
        '<div class="(' ++ :lists.flatten(catsStr) ++ ')">.*',
        [:unicode]
      )

    :lists.flatmap(
      fn str ->
        case :re.run(str, mP) do
          {:match, _} ->
            []

          :nomatch ->
            [str]
        end
      end,
      strs
    )
  end

  def capture_get([]) do
    :test_server.capture_get()
  end

  def fail(reason) do
    try do
      exit({:test_case_failed, reason})
    catch
      class, r ->
        case __STACKTRACE__ do
          [{:ct, :fail, 1, _} | stk] ->
            :ok

          stk ->
            :ok
        end

        :erlang.raise(class, r, stk)
    end
  end

  def fail(format, args) do
    try do
      :io_lib.format(format, args)
    catch
      _, badArgs ->
        exit({badArgs, {:ct, :fail, [format, args]}})
    else
      str ->
        try do
          exit({:test_case_failed, :lists.flatten(str)})
        catch
          class, r ->
            case __STACKTRACE__ do
              [{:ct, :fail, 2, _} | stk] ->
                :ok

              stk ->
                :ok
            end

            :erlang.raise(class, r, stk)
        end
    end
  end

  def comment(comment) when is_list(comment) do
    formatted =
      case (try do
              :io_lib.format('~ts', [comment])
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:EXIT, _} ->
          :io_lib.format('~tp', [comment])

        string ->
          string
      end

    send_html_comment(:lists.flatten(formatted))
  end

  def comment(comment) do
    formatted = :io_lib.format('~tp', [comment])
    send_html_comment(:lists.flatten(formatted))
  end

  def comment(format, args)
      when is_list(format) and
             is_list(args) do
    formatted =
      case (try do
              :io_lib.format(format, args)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:EXIT, reason} ->
          exit({reason, {:ct, :comment, [format, args]}})

        string ->
          :lists.flatten(string)
      end

    send_html_comment(formatted)
  end

  defp send_html_comment(comment) do
    html = '<font color="green">' ++ comment ++ '</font>'
    :ct_util.set_testdata({{:comment, :erlang.group_leader()}, html})
    :test_server.comment(html)
  end

  def make_priv_dir() do
    :test_server.make_priv_dir()
  end

  def get_target_name(handle) do
    :ct_util.get_target_name(handle)
  end

  def get_progname() do
    case :init.get_argument(:progname) do
      {:ok, [[prog]]} ->
        prog

      _Other ->
        'no_prog_name'
    end
  end

  def parse_table(data) do
    :ct_util.parse_table(data)
  end

  def listenv(telnet) do
    :ct_util.listenv(telnet)
  end

  def testcases(testDir, suite) do
    case make_and_load(testDir, suite) do
      e = {:error, _} ->
        e

      _ ->
        case (try do
                suite.all()
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, reason} ->
            {:error, reason}

          tCs ->
            tCs
        end
    end
  end

  defp make_and_load(dir, suite) do
    envInclude =
      :string.lexemes(
        :os.getenv('CT_INCLUDE_PATH', ''),
        [?:, ?\s, ?,]
      )

    startInclude =
      case :init.get_argument(:include) do
        {:ok, [dirs]} ->
          dirs

        _ ->
          []
      end

    userInclude = envInclude ++ startInclude

    case :ct_run.run_make(dir, suite, userInclude) do
      mErr = {:error, _} ->
        mErr

      _ ->
        testDir = :ct_util.get_testdir(dir, suite)

        file =
          :filename.join(
            testDir,
            :erlang.atom_to_list(suite)
          )

        case :code.soft_purge(suite) do
          true ->
            :code.load_abs(file)

          false ->
            {:module, suite}
        end
    end
  end

  def userdata(testDir, suite) do
    case make_and_load(testDir, suite) do
      e = {:error, _} ->
        e

      _ ->
        info =
          try do
            suite.suite()
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        get_userdata(info, 'suite/0')
    end
  end

  defp get_userdata({:EXIT, {undef, _}}, spec)
       when undef == :undef or undef == :function_clause do
    {:error, :erlang.list_to_atom(spec ++ ' is not defined')}
  end

  defp get_userdata({:EXIT, reason}, spec) do
    {:error, {:erlang.list_to_atom('error in ' ++ spec), reason}}
  end

  defp get_userdata(list, _) when is_list(list) do
    fun = fn
      {:userdata, data}, acc ->
        [data | acc]

      _, acc ->
        acc
    end

    case :lists.foldl(fun, [], list) do
      terms ->
        :lists.flatten(:lists.reverse(terms))
    end
  end

  defp get_userdata(_BadTerm, spec) do
    {:error, :erlang.list_to_atom(spec ++ ' must return a list')}
  end

  def userdata(testDir, suite, {:group, groupName}) do
    case make_and_load(testDir, suite) do
      e = {:error, _} ->
        e

      _ ->
        info =
          try do
            apply(suite, :group, [groupName])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        get_userdata(
          info,
          'group(' ++ :erlang.atom_to_list(groupName) ++ ')'
        )
    end
  end

  def userdata(testDir, suite, case__) when is_atom(case__) do
    case make_and_load(testDir, suite) do
      e = {:error, _} ->
        e

      _ ->
        info =
          try do
            apply(suite, case__, [])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        get_userdata(info, :erlang.atom_to_list(case__) ++ '/0')
    end
  end

  def get_status() do
    case get_testdata(:curr_tc) do
      {:ok, testCase} ->
        case get_testdata(:stats) do
          {:ok, {ok, failed, skipped = {userSkipped, autoSkipped}}} ->
            [
              {:current, testCase},
              {:successful, ok},
              {:failed, failed},
              {:skipped, skipped},
              {:total, ok + failed + userSkipped + autoSkipped}
            ]

          err1 ->
            err1
        end

      err2 ->
        err2
    end
  end

  defp get_testdata(key) do
    case (try do
            :ct_util.get_testdata(key)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, :ct_util_server_not_running} ->
        :no_tests_running

      error = {:error, _Reason} ->
        error

      {:EXIT, _Reason} ->
        :no_tests_running

      :undefined ->
        {:error, :no_testdata}

      [currTC] when key == :curr_tc ->
        {:ok, currTC}

      data ->
        {:ok, data}
    end
  end

  def abort_current_testcase(reason) do
    :test_server_ctrl.abort_current_testcase(reason)
  end

  def get_event_mgr_ref() do
    :ct_event
  end

  def encrypt_config_file(srcFileName, encryptFileName) do
    :ct_config.encrypt_config_file(
      srcFileName,
      encryptFileName
    )
  end

  def encrypt_config_file(srcFileName, encryptFileName, keyOrFile) do
    :ct_config.encrypt_config_file(srcFileName, encryptFileName, keyOrFile)
  end

  def decrypt_config_file(encryptFileName, targetFileName) do
    :ct_config.decrypt_config_file(
      encryptFileName,
      targetFileName
    )
  end

  def decrypt_config_file(encryptFileName, targetFileName, keyOrFile) do
    :ct_config.decrypt_config_file(encryptFileName, targetFileName, keyOrFile)
  end

  def add_config(callback, config) do
    :ct_config.add_config(callback, config)
  end

  def remove_config(callback, config) do
    :ct_config.remove_config(callback, config)
  end

  def timetrap(time) do
    :test_server.timetrap_cancel()
    :test_server.timetrap(time)
  end

  def get_timetrap_info() do
    :test_server.get_timetrap_info()
  end

  def sleep({:hours, hs}) do
    sleep(trunc(hs * 1000 * 60 * 60))
  end

  def sleep({:minutes, ms}) do
    sleep(trunc(ms * 1000 * 60))
  end

  def sleep({:seconds, ss}) do
    sleep(trunc(ss * 1000))
  end

  def sleep(time) do
    :test_server.adjusted_sleep(time)
  end

  def notify(name, data) do
    :ct_event.notify(name, data)
  end

  def sync_notify(name, data) do
    :ct_event.sync_notify(name, data)
  end

  def break(comment) do
    case {:ct_util.get_testdata(:starter), :ct_util.get_testdata(:release_shell)} do
      {:ct, releaseSh} when releaseSh != true ->
        warning = 'ct:break/1 can only be used if release_shell == true.\n'
        :ct_logs.log('Warning!', warning, [])
        :io.format(:user, 'Warning! ' ++ warning, [])
        {:error, :"enable break with release_shell option"}

      _ ->
        case get_testdata(:curr_tc) do
          {:ok, {_, _TestCase}} ->
            :test_server.break(:ct, comment)

          {:ok, cases} when is_list(cases) ->
            {:error,
             {:"multiple cases running",
              for {_, tC} <- cases do
                tC
              end}}

          error = {:error, _} ->
            error

          error ->
            {:error, error}
        end
    end
  end

  def break(testCase, comment) do
    case {:ct_util.get_testdata(:starter), :ct_util.get_testdata(:release_shell)} do
      {:ct, releaseSh} when releaseSh != true ->
        warning = 'ct:break/2 can only be used if release_shell == true.\n'
        :ct_logs.log('Warning!', warning, [])
        :io.format(:user, 'Warning! ' ++ warning, [])
        {:error, :"enable break with release_shell option"}

      _ ->
        case get_testdata(:curr_tc) do
          {:ok, cases} when is_list(cases) ->
            case :lists.keymember(testCase, 2, cases) do
              true ->
                :test_server.break(:ct, testCase, comment)

              false ->
                {:error, :"test case not running"}
            end

          {:ok, {_, ^testCase}} ->
            :test_server.break(:ct, testCase, comment)

          error = {:error, _} ->
            error

          error ->
            {:error, error}
        end
    end
  end

  def continue() do
    :test_server.continue()
  end

  def continue(testCase) do
    :test_server.continue(testCase)
  end

  def remaining_test_procs() do
    :ct_util.remaining_test_procs()
  end
end
