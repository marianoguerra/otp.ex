defmodule :m_system_information do
  use Bitwise
  @behaviour :gen_server
  require Record
  Record.defrecord(:r_state, :state, report: :undefined)

  def start() do
    :gen_server.start({:local, :system_information}, :system_information, [], [])
  end

  def stop() do
    :gen_server.call(:system_information, :stop, :infinity)
  end

  def load_report() do
    load_report(:data, report())
  end

  def load_report(:file, file) do
    load_report(:data, from_file(file))
  end

  def load_report(:data, report) do
    :ok = start_internal()
    :gen_server.call(:system_information, {:load_report, report}, :infinity)
  end

  def report() do
    {:ok, fd} = :file.open([], [:ram, :read, :write])
    to_fd(fd)
    {:ok, _} = :file.position(fd, :bof)
    from_fd(fd)
  end

  def to_file(file) do
    case :file.open(
           file,
           [:raw, :write, :binary, :delayed_write]
         ) do
      {:ok, fd} ->
        try do
          to_fd(fd)
        after
          :file.close(fd)
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  def from_file(file) do
    {:ok, fd} = :file.open(file, [:raw, :read])

    try do
      from_fd(fd)
    after
      :file.close(fd)
    end
  end

  def applications() do
    applications([])
  end

  def applications(opts) when is_list(opts) do
    :gen_server.call(:system_information, {:applications, opts}, :infinity)
  end

  def application(app) when is_atom(app) do
    application(app, [])
  end

  def application(app, opts)
      when is_atom(app) and
             is_list(opts) do
    :gen_server.call(:system_information, {:application, app, opts}, :infinity)
  end

  def environment() do
    environment([])
  end

  def environment(opts) when is_list(opts) do
    :gen_server.call(:system_information, {:environment, opts}, :infinity)
  end

  def module(m) when is_atom(m) do
    module(m, [])
  end

  def module(m, opts) when is_atom(m) and is_list(opts) do
    :gen_server.call(:system_information, {:module, m, opts}, :infinity)
  end

  def modules(opt) when is_atom(opt) do
    :gen_server.call(:system_information, {:modules, opt}, :infinity)
  end

  def sanity_check() do
    case check_runtime_dependencies() do
      [] ->
        :ok

      issues ->
        {:failed, issues}
    end
  end

  def init([]) do
    {:ok, r_state()}
  end

  def handle_call(:stop, _From, s) do
    {:stop, :normal, :ok, s}
  end

  def handle_call({:load_report, report}, _From, s) do
    version =
      get_value(
        [:system_info, :system_version],
        report
      )

    :io.format('Loaded report from system version: ~s~n', [version])
    {:reply, :ok, r_state(s, report: report)}
  end

  def handle_call(_Req, _From, r_state(report: :undefined) = s) do
    {:reply, {:error, :report_not_loaded}, s}
  end

  def handle_call({:applications, opts}, _From, r_state(report: report) = s) do
    :ok =
      print_applications(
        get_value([:code], report),
        opts
      )

    {:reply, :ok, s}
  end

  def handle_call({:application, app, opts}, _From, r_state(report: report) = s) do
    data =
      get_value(
        [app],
        for {:application, appInfo} <-
              get_value(
                [:code],
                report
              ) do
          appInfo
        end
      )

    :ok = print_application({app, data}, opts)
    {:reply, :ok, s}
  end

  def handle_call({:environment, opts}, _From, r_state(report: report) = s) do
    choices =
      case :proplists.get_bool(:full, opts) do
        true ->
          [:environment]

        false ->
          [:environment_erts]
      end

    :ok =
      print_environments(
        get_value(choices, report),
        opts
      )

    {:reply, :ok, s}
  end

  def handle_call({:module, m, opts}, _From, r_state(report: report) = s) do
    mods =
      find_modules_from_code(
        m,
        get_value([:code], report)
      )

    print_modules_from_code(m, mods, opts)
    {:reply, :ok, s}
  end

  def handle_call({:modules, :native}, _From, r_state(report: report) = s) do
    codes =
      get_native_modules_from_code(
        get_value(
          [:code],
          report
        )
      )

    :io.format('~p~n', [codes])
    {:reply, :ok, s}
  end

  def handle_call(_Request, _From, state) do
    {:reply, :ok, state}
  end

  def handle_cast(_Msg, state) do
    {:noreply, state}
  end

  def handle_info(_Info, state) do
    {:noreply, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp start_internal() do
    case start() do
      {:ok, _} ->
        :ok

      {:error, {:already_started, _}} ->
        :ok

      error ->
        error
    end
  end

  defp get_value([], data) do
    data
  end

  defp get_value([k | ks], data) do
    get_value(ks, :proplists.get_value(k, data, []))
  end

  defp find_modules_from_code(m, [{:code, info} | codes]) do
    case find_modules(m, get_value([:modules], info)) do
      [] ->
        find_modules_from_code(m, codes)

      mods ->
        path = get_value([:path], info)
        [{path, mods} | find_modules_from_code(m, codes)]
    end
  end

  defp find_modules_from_code(m, [{:application, {app, info}} | codes]) do
    case find_modules(m, get_value([:modules], info)) do
      [] ->
        find_modules_from_code(m, codes)

      mods ->
        path = get_value([:path], info)
        vsn = get_value([:vsn], info)

        [
          {app, vsn, path, mods}
          | find_modules_from_code(
              m,
              codes
            )
        ]
    end
  end

  defp find_modules_from_code(_, []) do
    []
  end

  defp find_modules(m, [{m, _} = info | ms]) do
    [info | find_modules(m, ms)]
  end

  defp find_modules(m, [_ | ms]) do
    find_modules(m, ms)
  end

  defp find_modules(_, []) do
    []
  end

  defp get_native_modules_from_code([{:application, {app, info}} | cs]) do
    case get_native_modules(
           get_value(
             [:modules],
             info
           )
         ) do
      [] ->
        get_native_modules_from_code(cs)

      mods ->
        path = get_value([:path], info)
        vsn = get_value([:vsn], info)

        [
          {app, vsn, path, mods}
          | get_native_modules_from_code(cs)
        ]
    end
  end

  defp get_native_modules_from_code([{:code, info} | cs]) do
    case get_native_modules(
           get_value(
             [:modules],
             info
           )
         ) do
      [] ->
        get_native_modules_from_code(cs)

      mods ->
        path = get_value([:path], info)
        [{path, mods} | get_native_modules_from_code(cs)]
    end
  end

  defp get_native_modules_from_code([]) do
    []
  end

  defp get_native_modules([]) do
    []
  end

  defp get_native_modules([{mod, info} | ms]) do
    case :proplists.get_value(:native, info) do
      false ->
        get_native_modules(ms)

      _ ->
        [mod | get_native_modules(ms)]
    end
  end

  defp print_applications([{:application, app} | apps], opts) do
    print_application(app, opts)
    print_applications(apps, opts)
  end

  defp print_applications([{:code, _} | apps], opts) do
    print_applications(apps, opts)
  end

  defp print_applications([], _) do
    :ok
  end

  defp print_application({app, info}, opts) do
    vsn = get_value([:vsn], info)
    :io.format(' * ~w-~s~n', [app, vsn])

    case :proplists.get_bool(:full, opts) do
      true ->
        _ =
          for minfo <- get_value([:modules], info) do
            print_module(minfo)
          end

        :ok

      false ->
        :ok
    end
  end

  defp print_environments([env | envs], opts) do
    print_environment(env, opts)
    print_environments(envs, opts)
  end

  defp print_environments([], _) do
    :ok
  end

  defp print_environment({_Key, false}, _) do
    :ok
  end

  defp print_environment({key, value}, _) do
    :io.format(' - ~s = ~ts~n', [key, value])
  end

  defp print_modules_from_code(m, [info | ms], opts) do
    print_module_from_code(m, info)

    case :proplists.get_bool(:full, opts) do
      true ->
        print_modules_from_code(m, ms, opts)

      false ->
        :ok
    end
  end

  defp print_modules_from_code(_, [], _) do
    :ok
  end

  defp print_module_from_code(m, {path, [{m, modInfo}]}) do
    :io.format(' from path "~ts" (no application):~n', [path])
    :io.format('     - compiler: ~s~n', [get_value([:compiler], modInfo)])
    :io.format('     -      md5: ~s~n', [get_value([:md5], modInfo)])
    :io.format('     -   native: ~w~n', [get_value([:native], modInfo)])
    :io.format('     -   loaded: ~w~n', [get_value([:loaded], modInfo)])
    :ok
  end

  defp print_module_from_code(m, {app, vsn, path, [{m, modInfo}]}) do
    :io.format(' from path "~ts" (~w-~s):~n', [path, app, vsn])
    :io.format('     - compiler: ~s~n', [get_value([:compiler], modInfo)])
    :io.format('     -      md5: ~s~n', [get_value([:md5], modInfo)])
    :io.format('     -   native: ~w~n', [get_value([:native], modInfo)])
    :io.format('     -   loaded: ~w~n', [get_value([:loaded], modInfo)])
    :ok
  end

  defp print_module({mod, modInfo}) do
    :io.format('   - ~w:~n', [mod])
    :io.format('     - compiler: ~s~n', [get_value([:compiler], modInfo)])
    :io.format('     -      md5: ~s~n', [get_value([:md5], modInfo)])
    :io.format('     -   native: ~w~n', [get_value([:native], modInfo)])
    :io.format('     -   loaded: ~w~n', [get_value([:loaded], modInfo)])
    :ok
  end

  defp erlang_system_info() do
    erlang_system_info([
      :allocator,
      :check_io,
      :otp_release,
      :port_limit,
      :process_limit,
      :smp_support,
      :system_version,
      :system_architecture,
      :threads,
      :thread_pool_size,
      {:wordsize, :internal},
      {:wordsize, :external},
      {:cpu_topology, :defined},
      {:cpu_topology, :detected},
      :scheduler_bind_type,
      :scheduler_bindings,
      :compat_rel,
      :schedulers_state,
      :build_type,
      :logical_processors,
      :logical_processors_online,
      :logical_processors_available,
      :driver_version,
      :nif_version,
      :emu_args,
      :ethread_info,
      :beam_jump_table,
      :taints
    ])
  end

  defp erlang_system_info([]) do
    []
  end

  defp erlang_system_info([type | types]) do
    [
      {type, :erlang.system_info(type)}
      | erlang_system_info(types)
    ]
  end

  defp os_getenv_erts_specific() do
    os_getenv_erts_specific([
      'BINDIR',
      'DIALYZER_EMULATOR',
      'CERL_DETACHED_PROG',
      'EMU',
      'ERL_CONSOLE_MODE',
      'ERL_CRASH_DUMP',
      'ERL_CRASH_DUMP_NICE',
      'ERL_CRASH_DUMP_SECONDS',
      'ERL_EPMD_PORT',
      'ERL_EMULATOR_DLL',
      'ERL_FULLSWEEP_AFTER',
      'ERL_LIBS',
      'ERL_MAX_PORTS',
      'ERL_MAX_ETS_TABLES',
      'ERL_NO_KERNEL_POLL',
      'ERL_THREAD_POOL_SIZE',
      'ERLC_EMULATOR',
      'ESCRIPT_EMULATOR',
      'HOME',
      'HOMEDRIVE',
      'HOMEPATH',
      'LANG',
      'LC_ALL',
      'LC_CTYPE',
      'PATH',
      'PROGNAME',
      'RELDIR',
      'ROOTDIR',
      'TERM',
      'COMSPEC',
      'HEART_COMMAND',
      'RUN_ERL_LOG_ALIVE_MINUTES',
      'RUN_ERL_LOG_ACTIVITY_MINUTES',
      'RUN_ERL_LOG_ALIVE_FORMAT',
      'RUN_ERL_LOG_ALIVE_IN_UTC',
      'RUN_ERL_LOG_GENERATIONS',
      'RUN_ERL_LOG_MAXSIZE',
      'RUN_ERL_DISABLE_FLOWCNTRL',
      'CALLER_DRV_USE_OUTPUTV',
      'ERL_INET_GETHOST_DEBUG',
      'ERL_EFILE_THREAD_SHORT_CIRCUIT',
      'ERL_WINDOW_TITLE',
      'ERL_ABORT_ON_FAILURE',
      'TTYSL_DEBUG_LOG'
    ])
  end

  defp os_getenv_erts_specific([]) do
    []
  end

  defp os_getenv_erts_specific([key | keys]) do
    [{key, :os.getenv(key)} | os_getenv_erts_specific(keys)]
  end

  defp split_env(env) do
    split_env(env, [])
  end

  defp split_env([?= | vs], key) do
    {:lists.reverse(key), vs}
  end

  defp split_env([i | vs], key) do
    split_env(vs, [i | key])
  end

  defp split_env([], kV) do
    :lists.reverse(kV)
  end

  defp from_fd(fd) do
    try do
      [{:system_information_version, '1.0'}, {:system_information, data}] = consult_fd(fd)
      data
    catch
      _, _ ->
        :erlang.error(:bad_report_file)
    end
  end

  defp consult_fd(fd) do
    consult_fd_1(fd, [], {:ok, []})
  end

  defp consult_fd_1(fd, cont0, readResult) do
    data =
      case readResult do
        {:ok, characters} ->
          characters

        :eof ->
          :eof
      end

    case :erl_scan.tokens(cont0, data, 1) do
      {:done, {:ok, tokens, _}, next} ->
        {:ok, term} = :erl_parse.parse_term(tokens)
        [term | consult_fd_1(fd, [], {:ok, next})]

      {:more, cont} ->
        consult_fd_1(fd, cont, :file.read(fd, 1 <<< 20))

      {:done, {:eof, _}, :eof} ->
        []
    end
  end

  defp to_fd(fd) do
    emitChunk = fn format, args ->
      :ok = :file.write(fd, :io_lib.format(format, args))
    end

    emitChunk.(
      '{system_information_version, ~w}.~n{system_information,[{init_arguments,~w},{code_paths,~w},',
      ['1.0', :init.get_arguments(), :code.get_path()]
    )

    emit_code_info(emitChunk)

    emitChunk.(
      ',{system_info,~w},{erts_compile_info,~w},{beam_dynamic_libraries,~w},{environment_erts,~w},{environment,~w},{sanity_check,~w}]}.~n',
      [
        erlang_system_info(),
        :erlang.system_info(:compile_info),
        get_dynamic_libraries(),
        os_getenv_erts_specific(),
        for env <- :os.getenv() do
          split_env(env)
        end,
        sanity_check()
      ]
    )
  end

  defp emit_code_info(emitChunk) do
    emitChunk.('{code, [', [])

    comma_separated_foreach(
      emitChunk,
      fn path ->
        case is_application_path(path) do
          true ->
            emit_application_info(emitChunk, path)

          false ->
            emit_code_path_info(emitChunk, path)
        end
      end,
      :code.get_path()
    )

    emitChunk.(']}', [])
  end

  defp emit_application_info(emitChunk, path) do
    [appfile | _] =
      :filelib.wildcard(
        :filename.join(
          path,
          '*.app'
        )
      )

    case :file.consult(appfile) do
      {:ok, [{:application, app, info}]} ->
        rtDeps = :proplists.get_value(:runtime_dependencies, info, [])
        description = :proplists.get_value(:description, info, [])
        version = :proplists.get_value(:vsn, info, [])

        emitChunk.(
          '{application, {~w,[{description,~w},{vsn,~w},{path,~w},{runtime_dependencies,~w},',
          [app, description, version, path, rtDeps]
        )

        emit_module_info_from_path(emitChunk, path)
        emitChunk.(']}}', [])
    end
  end

  defp emit_code_path_info(emitChunk, path) do
    emitChunk.('{code, [{path, ~w},', [path])
    emit_module_info_from_path(emitChunk, path)
    emitChunk.(']}', [])
  end

  defp emit_module_info_from_path(emitChunk, path) do
    beamFiles = :filelib.wildcard(:filename.join(path, '*.beam'))
    emitChunk.('{modules, [', [])

    comma_separated_foreach(
      emitChunk,
      fn beam ->
        emit_module_info(emitChunk, beam)
      end,
      beamFiles
    )

    emitChunk.(']}', [])
  end

  defp emit_module_info(emitChunk, beam) do
    {:ok, {mod, md5}} = :beam_lib.md5(beam)
    compilerVersion = get_compiler_version(beam)
    native = beam_is_native_compiled(beam)

    loaded =
      case :code.is_loaded(mod) do
        false ->
          false

        _ ->
          true
      end

    emitChunk.(
      '{~w,[{loaded,~w},{native,~w},{compiler,~w},{md5,~w}]}',
      [mod, loaded, native, compilerVersion, hexstring(md5)]
    )
  end

  defp comma_separated_foreach(_EmitChunk, _Fun, []) do
    :ok
  end

  defp comma_separated_foreach(_EmitChunk, fun, [h]) do
    fun.(h)
  end

  defp comma_separated_foreach(emitChunk, fun, [h | t]) do
    fun.(h)
    emitChunk.(',', [])
    comma_separated_foreach(emitChunk, fun, t)
  end

  defp is_application_path(path) do
    case :filelib.wildcard(:filename.join(path, '*.app')) do
      [] ->
        false

      _ ->
        true
    end
  end

  defp hexstring(bin) when is_binary(bin) do
    :lists.flatten(
      for <<(<<v>> <- bin)>> do
        :io_lib.format('~2.16.0b', [v])
      end
    )
  end

  defp get_compiler_version(beam) do
    case :beam_lib.chunks(beam, [:compile_info]) do
      {:ok, {_, [{:compile_info, info}]}} ->
        :proplists.get_value(:version, info)

      _ ->
        :undefined
    end
  end

  defp beam_is_native_compiled(beam) do
    chunks = get_value([:chunks], :beam_lib.info(beam))

    case check_known_hipe_chunks(chunks) do
      [] ->
        false

      [arch] ->
        {true, arch}

      archs ->
        {true, archs}
    end
  end

  defp check_known_hipe_chunks([{tag, _, _} | cs]) do
    case is_chunk_tag_hipe_arch(tag) do
      false ->
        check_known_hipe_chunks(cs)

      {true, arch} ->
        [arch | check_known_hipe_chunks(cs)]
    end
  end

  defp check_known_hipe_chunks([]) do
    []
  end

  defp is_chunk_tag_hipe_arch(tag) do
    case tag do
      'HA64' ->
        {true, :amd64}

      'HARM' ->
        {true, :arm}

      'HPPC' ->
        {true, :powerpc}

      'HP64' ->
        {true, :ppc64}

      'HS8P' ->
        {true, :ultrasparc}

      _ ->
        false
    end
  end

  defp get_dynamic_libraries() do
    beam = :filename.join([:os.getenv('BINDIR'), get_beam_name()])

    case :os.type() do
      {:unix, :darwin} ->
        :os.cmd('otool -L ' ++ beam)

      _ ->
        :os.cmd('ldd ' ++ beam)
    end
  end

  defp get_beam_name() do
    type =
      case :erlang.system_info(:build_type) do
        :opt ->
          ''

        typeName ->
          '.' ++ :erlang.atom_to_list(typeName)
      end

    flavor =
      case :erlang.system_info(:smp_support) do
        false ->
          ''

        true ->
          '.smp'
      end

    beam = :os.getenv('EMU', 'beam')
    beam ++ type ++ flavor
  end

  defp vsnstr2vsn(vsnStr) do
    :erlang.list_to_tuple(
      :lists.map(
        fn part ->
          :erlang.list_to_integer(part)
        end,
        :string.lexemes(vsnStr, '.')
      )
    )
  end

  defp rtdepstrs2rtdeps([]) do
    []
  end

  defp rtdepstrs2rtdeps([rTDep | rTDeps]) do
    [appStr, vsnStr] = :string.lexemes(rTDep, '-')

    [
      {:erlang.list_to_atom(appStr), vsnstr2vsn(vsnStr)}
      | rtdepstrs2rtdeps(rTDeps)
    ]
  end

  defp build_app_table([], appTab) do
    appTab
  end

  defp build_app_table([app | apps], appTab0) do
    appTab1 =
      try do
        appFile = :code.where_is_file(:erlang.atom_to_list(app) ++ '.app')
        {:ok, [{:application, ^app, info}]} = :file.consult(appFile)
        vsnStr = :proplists.get_value(:vsn, info)
        vsn = vsnstr2vsn(vsnStr)
        rTDepStrs = :proplists.get_value(:runtime_dependencies, info, [])
        rTDeps = rtdepstrs2rtdeps(rTDepStrs)
        :gb_trees.insert(app, {vsn, rTDeps}, appTab0)
      catch
        _, _ ->
          appTab0
      end

    build_app_table(apps, appTab1)
  end

  defp meets_min_req(vsn, vsn) do
    true
  end

  defp meets_min_req({x}, vsnReq) do
    meets_min_req({x, 0, 0}, vsnReq)
  end

  defp meets_min_req({x, y}, vsnReq) do
    meets_min_req({x, y, 0}, vsnReq)
  end

  defp meets_min_req(vsn, {x}) do
    meets_min_req(vsn, {x, 0, 0})
  end

  defp meets_min_req(vsn, {x, y}) do
    meets_min_req(vsn, {x, y, 0})
  end

  defp meets_min_req({x, _Y, _Z}, {xReq, _YReq, _ZReq})
       when x > xReq do
    true
  end

  defp meets_min_req({x, y, _Z}, {x, yReq, _ZReq}) when y > yReq do
    true
  end

  defp meets_min_req({x, y, z}, {x, y, zReq}) when z > zReq do
    true
  end

  defp meets_min_req({_X, _Y, _Z}, {_XReq, _YReq, _ZReq}) do
    false
  end

  defp meets_min_req(vsn, vsnReq) do
    gp_meets_min_req(
      mk_gp_vsn_list(vsn),
      mk_gp_vsn_list(vsnReq)
    )
  end

  defp gp_meets_min_req([[x, y, z] | _Vs], [x, y, z]) do
    true
  end

  defp gp_meets_min_req([[x, y, z] | _Vs], [xReq, yReq, zReq]) do
    meets_min_req({x, y, z}, {xReq, yReq, zReq})
  end

  defp gp_meets_min_req([[x, y, z] | vs], [[x, y, z] | vReqs]) do
    gp_meets_min_req_tail(vs, vReqs)
  end

  defp gp_meets_min_req(_Vsn, _VReq) do
    false
  end

  defp gp_meets_min_req_tail([v | vs], [v | vReqs]) do
    gp_meets_min_req_tail(vs, vReqs)
  end

  defp gp_meets_min_req_tail([], []) do
    true
  end

  defp gp_meets_min_req_tail([_V | _Vs], []) do
    true
  end

  defp gp_meets_min_req_tail([v | _Vs], [vReq]) when v > vReq do
    true
  end

  defp gp_meets_min_req_tail(_Vs, _VReqs) do
    false
  end

  defp mk_gp_vsn_list(vsn) do
    [[x, y, z] | tail] = :erlang.tuple_to_list(vsn)
    [[x, y, z] | remove_trailing_zeroes(tail)]
  end

  defp remove_trailing_zeroes([]) do
    []
  end

  defp remove_trailing_zeroes([0 | vs]) do
    case remove_trailing_zeroes(vs) do
      [] ->
        []

      newVs ->
        [0 | newVs]
    end
  end

  defp remove_trailing_zeroes([v | vs]) do
    [v | remove_trailing_zeroes(vs)]
  end

  defp mk_app_vsn_str({app, vsn}) do
    mk_app_vsn_str(app, vsn)
  end

  defp mk_app_vsn_str(app, vsn) do
    vsnList = :erlang.tuple_to_list(vsn)

    :lists.flatten([
      :erlang.atom_to_list(app),
      ?-,
      :erlang.integer_to_list(hd(vsnList)),
      :lists.map(
        fn part ->
          [?., :erlang.integer_to_list(part)]
        end,
        tl(vsnList)
      )
    ])
  end

  defp otp_17_0_vsns_orddict() do
    [
      {:asn1, {3, 0}},
      {:common_test, {1, 8}},
      {:compiler, {5, 0}},
      {:cosEvent, {2, 1, 15}},
      {:cosEventDomain, {1, 1, 14}},
      {:cosFileTransfer, {1, 1, 16}},
      {:cosNotification, {1, 1, 21}},
      {:cosProperty, {1, 1, 17}},
      {:cosTime, {1, 1, 14}},
      {:cosTransactions, {1, 2, 14}},
      {:crypto, {3, 3}},
      {:debugger, {4, 0}},
      {:dialyzer, {2, 7}},
      {:diameter, {1, 6}},
      {:edoc, {0, 7, 13}},
      {:eldap, {1, 0, 3}},
      {:erl_docgen, {0, 3, 5}},
      {:erl_interface, {3, 7, 16}},
      {:erts, {6, 0}},
      {:et, {1, 5}},
      {:eunit, {2, 2, 7}},
      {:gs, {1, 5, 16}},
      {:hipe, {3, 10, 3}},
      {:ic, {4, 3, 5}},
      {:inets, {5, 10}},
      {:jinterface, {1, 5, 9}},
      {:kernel, {3, 0}},
      {:megaco, {3, 17, 1}},
      {:mnesia, {4, 12}},
      {:observer, {2, 0}},
      {:odbc, {2, 10, 20}},
      {:orber, {3, 6, 27}},
      {:os_mon, {2, 2, 15}},
      {:ose, {1, 0}},
      {:otp_mibs, {1, 0, 9}},
      {:parsetools, {2, 0, 11}},
      {:percept, {0, 8, 9}},
      {:public_key, {0, 22}},
      {:reltool, {0, 6, 5}},
      {:runtime_tools, {1, 8, 14}},
      {:sasl, {2, 4}},
      {:snmp, {4, 25, 1}},
      {:ssh, {3, 0, 1}},
      {:ssl, {5, 3, 4}},
      {:stdlib, {2, 0}},
      {:syntax_tools, {1, 6, 14}},
      {:test_server, {3, 7}},
      {:tools, {2, 6, 14}},
      {:typer, {0, 9, 6}},
      {:webtool, {0, 8, 10}},
      {:wx, {1, 2}},
      {:xmerl, {1, 3, 7}}
    ]
  end

  defp otp_17_0_vsns_tab() do
    :gb_trees.from_orddict(otp_17_0_vsns_orddict())
  end

  defp check_runtime_dependency({app, depVsn}, appTab) do
    case :gb_trees.lookup(app, appTab) do
      :none ->
        false

      {:value, {vsn, _}} ->
        meets_min_req(vsn, depVsn)
    end
  end

  defp check_runtime_dependencies(app, appTab, otpMinVsnTab) do
    case :gb_trees.lookup(app, appTab) do
      :none ->
        [{:invalid_app_file, app}]

      {:value, {vsn, rTDeps}} ->
        rTD =
          case :lists.foldl(
                 fn rTDep, acc ->
                   case check_runtime_dependency(
                          rTDep,
                          appTab
                        ) do
                     true ->
                       acc

                     false ->
                       [mk_app_vsn_str(rTDep) | acc]
                   end
                 end,
                 [],
                 rTDeps
               ) do
            [] ->
              []

            missingDeps ->
              [{:missing_runtime_dependencies, mk_app_vsn_str(app, vsn), missingDeps}]
          end

        case :gb_trees.lookup(app, otpMinVsnTab) do
          :none ->
            rTD

          {:value, minVsn} ->
            case meets_min_req(vsn, minVsn) do
              true ->
                rTD

              false ->
                [
                  {:invalid_application_version, mk_app_vsn_str(app, vsn)}
                  | rTD
                ]
            end
        end
    end
  end

  defp app_file_to_app(aF) do
    :erlang.list_to_atom(:filename.basename(aF, '.app'))
  end

  defp get_apps() do
    get_apps(:code.get_path(), [])
  end

  defp get_apps([], apps) do
    :lists.usort(apps)
  end

  defp get_apps([path | paths], apps) do
    case :filelib.wildcard(:filename.join(path, '*.app')) do
      [] ->
        get_apps(paths, apps)

      [appFile] ->
        get_apps(paths, [app_file_to_app(appFile) | apps])

      [_AppFile | _] = appFiles ->
        :lists.map(
          fn aF ->
            app_file_to_app(aF)
          end,
          appFiles
        ) ++ apps
    end
  end

  defp check_runtime_dependencies() do
    otpMinVsnTab = otp_17_0_vsns_tab()
    apps = get_apps()
    appTab = build_app_table(apps, :gb_trees.empty())

    :lists.foldl(
      fn app, acc ->
        case check_runtime_dependencies(app, appTab, otpMinVsnTab) do
          [] ->
            acc

          issues ->
            issues ++ acc
        end
      end,
      [],
      apps
    )
  end
end
