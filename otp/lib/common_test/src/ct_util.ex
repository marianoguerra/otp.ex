defmodule :m_ct_util do
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

  Record.defrecord(:r_suite_data, :suite_data,
    key: :undefined,
    name: :undefined,
    value: :undefined
  )

  def start() do
    start(:normal, '.', [{:default, 100}, {:"$unspecified", 100}])
  end

  def start(logDir) when is_list(logDir) do
    start(:normal, logDir, [{:default, 100}, {:"$unspecified", 100}])
  end

  def start(mode) do
    start(mode, '.', [{:default, 100}, {:"$unspecified", 100}])
  end

  def start(logDir, verbosity) when is_list(logDir) do
    start(:normal, logDir, verbosity)
  end

  def start(mode, logDir, verbosity) do
    case :erlang.whereis(:ct_util_server) do
      :undefined ->
        s = self()

        pid =
          spawn_link(fn ->
            do_start(s, mode, logDir, verbosity)
          end)

        receive do
          {^pid, :started} ->
            pid

          {^pid, error} ->
            exit(error)

          {_Ref, {^pid, error}} ->
            exit(error)
        end

      pid ->
        case get_mode() do
          :interactive when mode == :interactive ->
            pid

          :interactive ->
            {:error, :interactive_mode}

          _OtherMode ->
            pid
        end
    end
  end

  defp do_start(parent, mode, logDir, verbosity) do
    :erlang.process_flag(:trap_exit, true)
    :erlang.register(:ct_util_server, self())
    mark_process()
    create_table(:ct_connections, r_conn(:handle))
    create_table(:ct_boards, 2)
    create_table(:ct_suite_data, r_suite_data(:key))
    create_table(:ct_verbosity_table, 1)

    _ =
      for {cat, lvl} <- verbosity do
        :ets.insert(:ct_verbosity_table, {cat, lvl})
      end

    {:ok, startDir} = :file.get_cwd()

    case :file.set_cwd(logDir) do
      :ok ->
        :ok

      e ->
        exit(e)
    end

    doExit = fn reason ->
      :ok = :file.set_cwd(startDir)
      exit(reason)
    end

    opts =
      case read_opts() do
        {:ok, opts1} ->
          opts1

        error ->
          send(parent, {self(), error})
          doExit.(error)
      end

    case :ct_event.start_link() do
      {:error, {:already_started, _}} ->
        :ok

      _ ->
        :ct_event.add_handler()
    end

    try do
      :ct_config.start(mode)
    catch
      _Class, cfgError ->
        doExit.(cfgError)
    else
      _ ->
        :ok
    end

    _ =
      case :lists.keysearch(:event_handler, 1, opts) do
        {:value, {_, handlers}} ->
          add = fn {h, args} ->
            case (try do
                    :gen_event.add_handler(:ct_event, h, args)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              :ok ->
                :ok

              {:EXIT, why} ->
                doExit.(why)

              other ->
                doExit.({:event_handler, other})
            end
          end

          case (try do
                  :lists.foreach(add, handlers)
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end) do
            {:EXIT, reason} ->
              send(parent, {self(), reason})

            _ ->
              :ok
          end

        false ->
          :ok
      end

    case :ct_default_gl.start_link(:erlang.group_leader()) do
      {:ok, _} ->
        :ok

      :ignore ->
        :ok
    end

    {startTime, testLogDir} = :ct_logs.init(mode, verbosity)

    :ct_event.notify(
      r_event(name: :test_start, node: node(), data: {startTime, :lists.flatten(testLogDir)})
    )

    _ =
      try do
        :ct_hooks.init(opts)
      catch
        _, cTHReason ->
          errorInfo =
            cond do
              is_atom(cTHReason) ->
                :io_lib.format('{~tp,~tp}', [cTHReason, __STACKTRACE__])

              true ->
                cTHReason
            end

          :ct_logs.tc_print(:"Suite Callback", errorInfo, [])
          send(self(), {{:stop, {self(), {:user_error, cTHReason}}}, {parent, make_ref()}})
      else
        :ok ->
          send(parent, {self(), :started})

        {:fail, cTHReason} ->
          :ct_logs.tc_print(:"Suite Callback", cTHReason, [])
          send(self(), {{:stop, {self(), {:user_error, cTHReason}}}, {parent, make_ref()}})
      end

    loop(mode, [], startDir)
  end

  defp create_table(tableName, keyPos) do
    create_table(tableName, :set, keyPos)
  end

  def create_table(tableName, type, keyPos) do
    try do
      :ets.delete(tableName)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    _ =
      :ets.new(
        tableName,
        [type, :named_table, :public, {:keypos, keyPos}]
      )

    :ok
  end

  def read_opts() do
    case :file.consult(:ct_run.variables_file_name('./')) do
      {:ok, opts} ->
        {:ok, opts}

      {:error, :enoent} ->
        {:error, :not_installed}

      error ->
        {:error, {:bad_installation, error}}
    end
  end

  def save_suite_data(key, value) do
    call({:save_suite_data, {key, :undefined, value}})
  end

  def save_suite_data(key, name, value) do
    call({:save_suite_data, {key, name, value}})
  end

  def save_suite_data_async(key, value) do
    save_suite_data_async(key, :undefined, value)
  end

  def save_suite_data_async(key, name, value) do
    cast({:save_suite_data, {key, name, value}})
  end

  def read_suite_data(key) do
    call({:read_suite_data, key})
  end

  def delete_suite_data() do
    call({:delete_suite_data, :all})
  end

  def delete_suite_data(key) do
    call({:delete_suite_data, key})
  end

  def match_delete_suite_data(keyPat) do
    call({:match_delete_suite_data, keyPat})
  end

  def delete_testdata() do
    call(:delete_testdata)
  end

  def delete_testdata(key) do
    call({:delete_testdata, key})
  end

  def match_delete_testdata(keyPat) do
    call({:match_delete_testdata, keyPat})
  end

  def update_testdata(key, fun) do
    update_testdata(key, fun, [])
  end

  def update_testdata(key, fun, opts) do
    call({:update_testdata, key, fun, opts})
  end

  def set_testdata(testData) do
    call({:set_testdata, testData})
  end

  def set_testdata_async(testData) do
    cast({:set_testdata, testData})
  end

  def get_testdata(key) do
    call({:get_testdata, key})
  end

  def get_testdata(key, timeout) do
    call({:get_testdata, key}, timeout)
  end

  def set_cwd(dir) do
    call({:set_cwd, dir})
  end

  def reset_cwd() do
    call(:reset_cwd)
  end

  def get_start_dir() do
    call(:get_start_dir)
  end

  def set_verbosity(elem = {_Category, _Level}) do
    try do
      :ets.insert(:ct_verbosity_table, elem)
    catch
      _, reason ->
        {:error, reason}
    else
      _ ->
        :ok
    end
  end

  def get_verbosity(category) do
    try do
      :ets.lookup(:ct_verbosity_table, category)
    catch
      _, reason ->
        {:error, reason}
    else
      [{^category, level}] ->
        level

      _ ->
        :undefined
    end
  end

  defp loop(mode, testData, startDir) do
    receive do
      {:update_last_run_index, from} ->
        :ct_logs.make_last_run_index()
        return(from, :ok)
        loop(mode, testData, startDir)

      {{:save_suite_data, {key, name, value}}, from} ->
        :ets.insert(
          :ct_suite_data,
          r_suite_data(key: key, name: name, value: value)
        )

        return(from, :ok)
        loop(mode, testData, startDir)

      {{:read_suite_data, key}, from} ->
        case :ets.lookup(:ct_suite_data, key) do
          [r_suite_data(key: ^key, name: :undefined, value: value)] ->
            return(from, value)

          [r_suite_data(key: ^key, name: name, value: value)] ->
            return(from, {name, value})

          _ ->
            return(from, :undefined)
        end

        loop(mode, testData, startDir)

      {{:delete_suite_data, key}, from} ->
        cond do
          key == :all ->
            :ets.delete_all_objects(:ct_suite_data)

          true ->
            :ets.delete(:ct_suite_data, key)
        end

        return(from, :ok)
        loop(mode, testData, startDir)

      {{:match_delete_suite_data, keyPat}, from} ->
        :ets.match_delete(
          :ct_suite_data,
          r_suite_data(key: keyPat, name: :_, value: :_)
        )

        return(from, :ok)
        loop(mode, testData, startDir)

      {:delete_testdata, from} ->
        return(from, :ok)
        loop(from, [], startDir)

      {{:delete_testdata, key}, from} ->
        testData1 = :lists.keydelete(key, 1, testData)
        return(from, :ok)
        loop(from, testData1, startDir)

      {{:match_delete_testdata, {key1, key2}}, from} ->
        testData1 =
          :lists.filter(
            fn
              {key, _} when not is_tuple(key) ->
                true

              {key, _} when tuple_size(key) !== 2 ->
                true

              {{_, keyB}, _} when key1 == :_ ->
                keyB !== key2

              {{keyA, _}, _} when key2 == :_ ->
                keyA !== key1

              _ when key1 == :_ or key2 == :_ ->
                false

              _ ->
                true
            end,
            testData
          )

        return(from, :ok)
        loop(from, testData1, startDir)

      {{:set_testdata, new = {key, _Val}}, from} ->
        testData1 = :lists.keydelete(key, 1, testData)
        return(from, :ok)
        loop(mode, [new | testData1], startDir)

      {{:get_testdata, :all}, from} ->
        return(from, testData)
        loop(from, testData, startDir)

      {{:get_testdata, key}, from} ->
        case :lists.keysearch(key, 1, testData) do
          {:value, {^key, val}} ->
            return(from, val)

          _ ->
            return(from, :undefined)
        end

        loop(from, testData, startDir)

      {{:update_testdata, key, fun, opts}, from} ->
        testData1 =
          case :lists.keysearch(key, 1, testData) do
            {:value, {^key, val}} ->
              try do
                fun.(val)
              catch
                _, error ->
                  return(from, {:error, error})
                  testData
              else
                :"$delete" ->
                  return(from, :deleted)
                  :lists.keydelete(key, 1, testData)

                newVal ->
                  return(from, newVal)
                  [{key, newVal} | :lists.keydelete(key, 1, testData)]
              end

            _ ->
              case :lists.member(:create, opts) do
                true ->
                  initVal = fun.(:undefined)
                  return(from, initVal)
                  [{key, initVal} | testData]

                false ->
                  return(from, :undefined)
                  testData
              end
          end

        loop(from, testData1, startDir)

      {{:set_cwd, dir}, from} ->
        return(from, :file.set_cwd(dir))
        loop(from, testData, startDir)

      {:reset_cwd, from} ->
        return(from, :file.set_cwd(startDir))
        loop(from, testData, startDir)

      {:get_start_dir, from} ->
        return(from, startDir)
        loop(from, testData, startDir)

      {{:stop, info}, from} ->
        :test_server_io.reset_state()

        {miscIoName, miscIoDivider, miscIoFooter} =
          :proplists.get_value(
            :misc_io_log,
            testData
          )

        {:ok, miscIoFd} =
          :file.open(
            miscIoName,
            [:append, {:encoding, :utf8}]
          )

        :io.put_chars(miscIoFd, miscIoDivider)
        :test_server_io.set_fd(:unexpected_io, miscIoFd)
        time = :calendar.local_time()
        :ct_event.sync_notify(r_event(name: :test_done, node: node(), data: time))

        callbacks =
          try do
            :ets.lookup_element(:ct_suite_data, :ct_hooks, r_suite_data(:value))
          catch
            :error, :badarg ->
              []
          else
            cTHMods ->
              cTHMods
          end

        :ct_hooks.terminate(callbacks)
        close_connections(:ets.tab2list(:ct_connections))
        :ets.delete(:ct_connections)
        :ets.delete(:ct_boards)
        :ets.delete(:ct_suite_data)
        :ets.delete(:ct_verbosity_table)
        :io.put_chars(miscIoFd, '\n</pre>\n' ++ miscIoFooter)
        :test_server_io.stop([:unexpected_io])
        :test_server_io.finish()
        :ct_logs.close(info, startDir)
        :ct_event.stop()
        :ct_config.stop()
        :ct_default_gl.stop()
        :ok = :file.set_cwd(startDir)
        return(from, info)

      {ref, _Msg} when is_reference(ref) ->
        loop(mode, testData, startDir)

      {:get_mode, from} ->
        return(from, mode)
        loop(mode, testData, startDir)

      {:EXIT, _Pid, :normal} ->
        loop(mode, testData, startDir)

      {:EXIT, pid, reason} ->
        case :ets.lookup(:ct_connections, pid) do
          [r_conn(address: a, callback: cB)] ->
            errorStr = :io_lib.format('~tp', [reason])
            errorHtml = :ct_logs.escape_chars(errorStr)

            :ct_logs.tc_log_async(
              :ct_error_notify,
              99,
              'CT Error Notification',
              'Connection process died: Pid: ~w, Address: ~tp, Callback: ~w\nReason: ~ts\n\n',
              [pid, a, cB, errorHtml]
            )

            try do
              cB.close(pid)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end

            unregister_connection(pid)
            loop(mode, testData, startDir)

          _ ->
            :io.format('\n\nct_util_server got EXIT from ~w: ~tp\n\n', [pid, reason])
            :ok = :file.set_cwd(startDir)
            exit(reason)
        end
    end
  end

  defp close_connections([r_conn(handle: handle, callback: cB) | conns]) do
    cB.close(handle)
    close_connections(conns)
  end

  defp close_connections([]) do
    :ok
  end

  def get_key_from_name(name) do
    :ct_config.get_key_from_name(name)
  end

  def register_connection(targetName, address, callback, handle) do
    targetRef =
      case :ct_config.get_key_from_name(targetName) do
        {:ok, _Key} ->
          targetName

        _ ->
          handle
      end

    :ets.insert(
      :ct_connections,
      r_conn(handle: handle, targetref: targetRef, address: address, callback: callback)
    )

    :ok
  end

  def unregister_connection(handle) do
    :ets.delete(:ct_connections, handle)
    :ok
  end

  def does_connection_exist(targetName, address, callback) do
    case :ct_config.get_key_from_name(targetName) do
      {:ok, _Key} ->
        case :ets.select(
               :ct_connections,
               [
                 {r_conn(
                    handle: :"$1",
                    targetref: targetName,
                    address: address,
                    callback: callback
                  ), [], [:"$1"]}
               ]
             ) do
          [handle] ->
            {:ok, handle}

          [] ->
            false
        end

      _ ->
        false
    end
  end

  def get_connection(targetName, callback) do
    case :ct_config.get_key_from_name(targetName) do
      {:ok, _Key} ->
        case :ets.select(
               :ct_connections,
               [
                 {r_conn(
                    handle: :"$1",
                    address: :"$2",
                    targetref: targetName,
                    callback: callback
                  ), [], [{{:"$1", :"$2"}}]}
               ]
             ) do
          [result] ->
            {:ok, result}

          [] ->
            {:error, :no_registered_connection}
        end

      error ->
        error
    end
  end

  def get_connections(connPid) do
    conns = :ets.tab2list(:ct_connections)

    :lists.flatmap(
      fn r_conn(targetref: targetName, handle: handle, callback: callback, address: address) ->
        case :ct_gen_conn.get_conn_pid(handle) do
          ^connPid when is_atom(targetName) ->
            [{targetName, handle, callback, address}]

          ^connPid ->
            [{:undefined, handle, callback, address}]

          _ ->
            []
        end
      end,
      conns
    )
  end

  def get_target_name(handle) do
    case :ets.select(
           :ct_connections,
           [{r_conn(handle: handle, targetref: :"$1", _: :_), [], [:"$1"]}]
         ) do
      [targetName] when is_atom(targetName) ->
        {:ok, targetName}

      _ ->
        {:error, {:unknown_connection, handle}}
    end
  end

  def close_connections() do
    close_connections(:ets.tab2list(:ct_connections))
    :ok
  end

  def override_silence_all_connections() do
    protocols = [:telnet, :ftp, :rpc, :snmp, :ssh]
    override_silence_connections(protocols)
    protocols
  end

  def override_silence_connections(conns) when is_list(conns) do
    conns1 =
      :lists.map(
        fn
          {c, b} ->
            {c, b}

          c ->
            {c, true}
        end,
        conns
      )

    set_testdata({:override_silent_connections, conns1})
  end

  def get_overridden_silenced_connections() do
    case get_testdata(:override_silent_connections) do
      {:error, _} ->
        :undefined

      conns ->
        conns
    end
  end

  def delete_overridden_silenced_connections() do
    delete_testdata(:override_silent_connections)
  end

  def silence_all_connections() do
    protocols = [:telnet, :ftp, :rpc, :snmp]
    silence_connections(protocols)
    protocols
  end

  def silence_connections(conn) when is_tuple(conn) do
    silence_connections([conn])
  end

  def silence_connections(conn) when is_atom(conn) do
    silence_connections([{conn, true}])
  end

  def silence_connections(conns) when is_list(conns) do
    conns1 =
      :lists.map(
        fn
          {c, b} ->
            {c, b}

          c ->
            {c, true}
        end,
        conns
      )

    set_testdata({:silent_connections, conns1})
  end

  def is_silenced(conn) do
    is_silenced(conn, :infinity)
  end

  def is_silenced(conn, timeout) do
    case get_testdata(:silent_connections, timeout) do
      conns when is_list(conns) ->
        case :lists.keysearch(conn, 1, conns) do
          {:value, {^conn, true}} ->
            true

          _ ->
            false
        end

      error = {:error, _} ->
        error

      _ ->
        false
    end
  end

  def reset_silent_connections() do
    delete_testdata(:silent_connections)
  end

  def stop(info) do
    case :erlang.whereis(:ct_util_server) do
      :undefined ->
        :ok

      ctUtilPid ->
        ref = :erlang.monitor(:process, ctUtilPid)
        call({:stop, info})

        receive do
          {:DOWN, ^ref, _, _, _} ->
            :ok
        end
    end
  end

  def update_last_run_index() do
    call(:update_last_run_index)
  end

  def get_mode() do
    call(:get_mode)
  end

  def listenv(telnet) do
    case :ct_telnet.send(telnet, 'listenv') do
      :ok ->
        {:ok, data, _} =
          :ct_telnet.expect(telnet, ['(^.+)=(.*$)'], [{:timeout, seconds(3)}, :repeat])

        {:ok,
         for [_, name, val] <- data do
           {name, val}
         end}

      {:error, reason} ->
        {:error, {:could_not_send_command, telnet, 'listenv', reason}}
    end
  end

  def parse_table(data) do
    {heading, rest} = get_headings(data)
    lines = parse_row(rest, [], :erlang.size(heading))
    {heading, lines}
  end

  defp get_headings(['|' ++ headings | rest]) do
    {remove_space(:string.lexemes(headings, '|'), []), rest}
  end

  defp get_headings([_ | rest]) do
    get_headings(rest)
  end

  defp get_headings([]) do
    {{}, []}
  end

  defp parse_row(['|' ++ _ = row | t], rows, numCols)
       when numCols > 1 do
    case :string.lexemes(row, '|') do
      values when length(values) === numCols ->
        parse_row(t, [remove_space(values, []) | rows], numCols)

      values when length(values) < numCols ->
        parse_row([row ++ '\n' ++ hd(t) | tl(t)], rows, numCols)
    end
  end

  defp parse_row(['|' ++ x = row | t], rows, 1 = numCols) do
    case :string.find(x, [?|]) do
      :nomatch ->
        parse_row([row ++ '\n' ++ hd(t) | tl(t)], rows, numCols)

      _Else ->
        parse_row(
          t,
          [remove_space(:string.lexemes(row, '|'), []) | rows],
          numCols
        )
    end
  end

  defp parse_row([_Skip | t], rows, numCols) do
    parse_row(t, rows, numCols)
  end

  defp parse_row([], rows, _NumCols) do
    :lists.reverse(rows)
  end

  defp remove_space([str | rest], acc) do
    remove_space(
      rest,
      [
        :string.trim(:string.trim(str, :both, [?\s]), :both, [?'])
        | acc
      ]
    )
  end

  defp remove_space([], acc) do
    :erlang.list_to_tuple(:lists.reverse(acc))
  end

  def is_test_dir(dir) do
    :lists.last(
      :string.lexemes(
        :filename.basename(dir),
        '_'
      )
    ) == 'test'
  end

  def get_testdir(dir, :all) do
    abs = abs_name(dir)

    case is_test_dir(abs) do
      true ->
        abs

      false ->
        absTest = :filename.join(abs, 'test')

        case :filelib.is_dir(absTest) do
          true ->
            absTest

          false ->
            abs
        end
    end
  end

  def get_testdir(dir, [suite | _]) when is_atom(suite) do
    get_testdir(dir, :erlang.atom_to_list(suite))
  end

  def get_testdir(dir, [suite | _]) when is_list(suite) do
    get_testdir(dir, suite)
  end

  def get_testdir(dir, suite) when is_atom(suite) do
    get_testdir(dir, :erlang.atom_to_list(suite))
  end

  def get_testdir(dir, suite) when is_list(suite) do
    abs = abs_name(dir)

    case is_test_dir(abs) do
      true ->
        abs

      false ->
        absTest = :filename.join(abs, 'test')

        mod =
          case :filename.extension(suite) do
            '.erl' ->
              suite

            _ ->
              suite ++ '.erl'
          end

        case :filelib.is_file(:filename.join(absTest, mod)) do
          true ->
            absTest

          false ->
            abs
        end
    end
  end

  def get_testdir(dir, _) do
    get_testdir(dir, :all)
  end

  def get_attached(tCPid) do
    case :dbg_iserver.safe_call({:get_attpid, tCPid}) do
      {:ok, attPid} when is_pid(attPid) ->
        attPid

      _ ->
        :undefined
    end
  end

  def kill_attached(:undefined, _AttPid) do
    :ok
  end

  def kill_attached(_TCPid, :undefined) do
    :ok
  end

  def kill_attached(tCPid, attPid) do
    case :erlang.process_info(tCPid) do
      :undefined ->
        :erlang.exit(attPid, :kill)

      _ ->
        :ok
    end
  end

  def warn_duplicates(suites) do
    warn = fn mod ->
      case (try do
              apply(mod, :sequences, [])
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:EXIT, _} ->
          :ok

        [] ->
          :ok

        _ ->
          :io.format(
            :ct_default_gl,
            '~nWARNING! Deprecated function: ~w:sequences/0.~n         Use group with sequence property instead.~n',
            [mod]
          )
      end
    end

    :lists.foreach(warn, suites)
    :ok
  end

  def mark_process() do
    mark_process(:system)
  end

  def mark_process(type) do
    :erlang.put(:ct_process_type, type)
  end

  def is_marked(pid) do
    is_marked(pid, :system)
  end

  def is_marked(pid, type) do
    case :erlang.process_info(pid, :dictionary) do
      {:dictionary, list} ->
        type == :proplists.get_value(:ct_process_type, list)

      :undefined ->
        false
    end
  end

  def remaining_test_procs() do
    procs = :erlang.processes()

    {sharedGL, otherGLs, procs2} =
      :lists.foldl(
        fn pid, procTypes = {shared, other, procs1} ->
          case is_marked(
                 pid,
                 :group_leader
               ) do
            true ->
              cond do
                not is_pid(shared) ->
                  case :test_server_io.get_gl(true) do
                    ^pid ->
                      {pid, other,
                       :lists.delete(
                         pid,
                         procs1
                       )}

                    _ ->
                      {shared,
                       [
                         pid
                         | other
                       ], procs1}
                  end

                true ->
                  {shared, [pid | other], procs1}
              end

            false ->
              case is_marked(pid) do
                true ->
                  {shared, other,
                   :lists.delete(
                     pid,
                     procs1
                   )}

                false ->
                  procTypes
              end
          end
        end,
        {:undefined, [], procs},
        procs
      )

    allGLs = [sharedGL | otherGLs]

    testProcs =
      :lists.flatmap(
        fn pid ->
          case :erlang.process_info(
                 pid,
                 :group_leader
               ) do
            {:group_leader, gL} ->
              case :lists.member(gL, allGLs) do
                true ->
                  [{pid, gL}]

                false ->
                  []
              end

            :undefined ->
              []
          end
        end,
        procs2
      )

    {testProcs, sharedGL, otherGLs}
  end

  def get_profile_data() do
    get_profile_data(:all)
  end

  def get_profile_data(keyOrStartDir) do
    cond do
      is_atom(keyOrStartDir) ->
        get_profile_data(keyOrStartDir, get_start_dir())

      is_list(keyOrStartDir) ->
        get_profile_data(:all, keyOrStartDir)
    end
  end

  def get_profile_data(key, startDir) do
    profile =
      case :application.get_env(
             :common_test,
             :profile
           ) do
        {:ok, :undefined} ->
          :default

        {:ok, prof} ->
          prof

        _ ->
          :default
      end

    get_profile_data(profile, key, startDir)
  end

  defp get_profile_data(profile, key, startDir) do
    file =
      case profile do
        :default ->
          '.common_test'

        _ when is_list(profile) ->
          '.common_test' ++ '.' ++ profile

        _ when is_atom(profile) ->
          '.common_test' ++ '.' ++ :erlang.atom_to_list(profile)
      end

    fullNameWD = :filename.join(startDir, file)

    {whichFile, result} =
      case :file.consult(fullNameWD) do
        {:error, :enoent} ->
          case :init.get_argument(:home) do
            {:ok, [[homeDir]]} ->
              fullNameHome = :filename.join(homeDir, file)
              {fullNameHome, :file.consult(fullNameHome)}

            _ ->
              {file, {:error, :enoent}}
          end

        consulted ->
          {fullNameWD, consulted}
      end

    case result do
      {:error, :enoent} when profile != :default ->
        :io.format(:ct_default_gl, '~nERROR! Missing profile file ~tp~n', [file])
        :undefined

      {:error, :enoent} when profile == :default ->
        :undefined

      {:error, reason} ->
        :io.format(:ct_default_gl, '~nERROR! Error in profile file ~tp: ~tp~n', [
          whichFile,
          reason
        ])

        :undefined

      {:ok, data} ->
        data1 =
          case data do
            [list] when is_list(list) ->
              list

            _ when is_list(data) ->
              data

            _ ->
              :io.format(:ct_default_gl, '~nERROR! Invalid profile data in ~tp~n', [whichFile])
              []
          end

        cond do
          key == :all ->
            data1

          true ->
            :proplists.get_value(key, data)
        end
    end
  end

  defp call(msg) do
    call(msg, :infinity)
  end

  defp call(msg, timeout) do
    case {self(), :erlang.whereis(:ct_util_server)} do
      {_, :undefined} ->
        {:error, :ct_util_server_not_running}

      {pid, pid} ->
        {:error, :bad_invocation}

      {self, pid} ->
        mRef = :erlang.monitor(:process, pid)
        ref = make_ref()
        send(:ct_util_server, {msg, {self, ref}})

        receive do
          {^ref, result} ->
            :erlang.demonitor(mRef, [:flush])
            result

          {:DOWN, ^mRef, :process, _, reason} ->
            {:error, {:ct_util_server_down, reason}}
        after
          timeout ->
            {:error, :timeout}
        end
    end
  end

  defp return({to, ref}, result) do
    send(to, {ref, result})
    :ok
  end

  defp cast(msg) do
    send(:ct_util_server, {msg, {:ct_util_server, make_ref()}})
    :ok
  end

  defp seconds(t) do
    :test_server.seconds(t)
  end

  defp abs_name('/') do
    '/'
  end

  defp abs_name(dir0) do
    abs = :filename.absname(dir0)

    dir =
      case :lists.reverse(abs) do
        [?/ | rest] ->
          :lists.reverse(rest)

        _ ->
          abs
      end

    abs_name1(dir, [])
  end

  defp abs_name1([drv, ?:, ?/], acc) do
    split = [[drv, ?:, ?/] | acc]
    abs_name2(split, [])
  end

  defp abs_name1('/', acc) do
    split = ['/' | acc]
    abs_name2(split, [])
  end

  defp abs_name1(dir, acc) do
    abs_name1(
      :filename.dirname(dir),
      [:filename.basename(dir) | acc]
    )
  end

  defp abs_name2(['..' | t], [_ | acc]) do
    abs_name2(t, acc)
  end

  defp abs_name2(['.' | t], acc) do
    abs_name2(t, acc)
  end

  defp abs_name2([h | t], acc) do
    abs_name2(t, [h | acc])
  end

  defp abs_name2([], acc) do
    :filename.join(:lists.reverse(acc))
  end

  def open_url(:iexplore, args, uRL) do
    {:ok, r} = :win32reg.open([:read])
    :ok = :win32reg.change_key(r, 'applications\\iexplore.exe\\shell\\open\\command')

    _ =
      case :win32reg.values(r) do
        {:ok, paths} ->
          path = :proplists.get_value(:default, paths)
          [cmd | _] = :string.lexemes(path, '%')
          cmd1 = cmd ++ ' ' ++ args ++ ' ' ++ uRL
          :io.format(:ct_default_gl, '~nOpening ~ts with command:~n  ~ts~n', [uRL, cmd1])
          :erlang.open_port({:spawn, cmd1}, [])

        _ ->
          :io.format(:ct_default_gl, '~nNo path to iexplore.exe~n', [])
      end

    :win32reg.close(r)
    :ok
  end

  def open_url(prog, args, uRL) do
    progStr =
      cond do
        is_atom(prog) ->
          :erlang.atom_to_list(prog)

        is_list(prog) ->
          prog
      end

    cmd = progStr ++ ' ' ++ args ++ ' ' ++ uRL
    :io.format(:ct_default_gl, '~nOpening ~ts with command:~n  ~ts~n', [uRL, cmd])
    :erlang.open_port({:spawn, cmd}, [])
    :ok
  end
end
