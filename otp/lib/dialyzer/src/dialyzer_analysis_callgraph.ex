defmodule :m_dialyzer_analysis_callgraph do
  use Bitwise
  require Record

  Record.defrecord(:r_analysis, :analysis,
    analysis_pid: :undefined,
    type: :succ_typings,
    defines: [],
    doc_plt: :undefined,
    files: [],
    include_dirs: [],
    start_from: :byte_code,
    plt: :undefined,
    use_contracts: true,
    race_detection: false,
    behaviours_chk: false,
    timing: false,
    timing_server: :none,
    callgraph_file: '',
    solvers: :undefined
  )

  Record.defrecord(:r_options, :options,
    files: [],
    files_rec: [],
    analysis_type: :succ_typings,
    timing: false,
    defines: [],
    from: :byte_code,
    get_warnings: :maybe,
    init_plts: [],
    include_dirs: [],
    output_plt: :none,
    legal_warnings: :ordsets.new(),
    report_mode: :normal,
    erlang_mode: false,
    use_contracts: true,
    output_file: :none,
    output_format: :formatted,
    filename_opt: :basename,
    indent_opt: true,
    callgraph_file: '',
    check_plt: true,
    solvers: [],
    native: :maybe,
    native_cache: true
  )

  Record.defrecord(:r_contract, :contract, contracts: [], args: [], forms: [])

  Record.defrecord(:r_analysis_state, :analysis_state,
    codeserver: :undefined,
    analysis_type: :succ_typings,
    defines: [],
    doc_plt: :undefined,
    include_dirs: [],
    parent: :undefined,
    legal_warnings: :undefined,
    plt: :undefined,
    start_from: :byte_code,
    use_contracts: true,
    timing_server: :undefined,
    solvers: :undefined
  )

  Record.defrecord(:r_server_state, :server_state, parent: :undefined)

  def start(parent, legalWarnings, analysis) do
    timingServer = :dialyzer_timing.init(r_analysis(analysis, :timing))

    racesOn =
      :ordsets.is_element(
        :warn_race_condition,
        legalWarnings
      )

    analysis0 =
      r_analysis(analysis,
        race_detection: racesOn,
        timing_server: timingServer
      )

    analysis1 = expand_files(analysis0)
    analysis2 = run_analysis(analysis1, legalWarnings)
    state = r_server_state(parent: parent)
    loop(state, analysis2, :none)
    :dialyzer_timing.stop(timingServer)
  end

  defp run_analysis(analysis, legalWarnings) do
    self = self()

    fun = fn ->
      analysis_start(self, analysis, legalWarnings)
    end

    r_analysis(analysis, analysis_pid: spawn_link(fun))
  end

  defp loop(
         r_server_state(parent: parent) = state,
         r_analysis(analysis_pid: analPid) = analysis,
         extCalls
       ) do
    receive do
      {^analPid, :log, logMsg} ->
        send_log(parent, logMsg)
        loop(state, analysis, extCalls)

      {^analPid, :warnings, warnings} ->
        send_warnings(parent, warnings)
        loop(state, analysis, extCalls)

      {^analPid, :cserver, cServer, plt} ->
        skip_ets_transfer(analPid)
        send_codeserver_plt(parent, cServer, plt)
        loop(state, analysis, extCalls)

      {^analPid, :done, plt, docPlt} ->
        send_ext_calls(parent, extCalls)
        send_analysis_done(parent, plt, docPlt)

      {^analPid, :ext_calls, newExtCalls} ->
        loop(state, analysis, newExtCalls)

      {^analPid, :ext_types, extTypes} ->
        send_ext_types(parent, extTypes)
        loop(state, analysis, extCalls)

      {^analPid, :mod_deps, modDeps} ->
        send_mod_deps(parent, modDeps)
        loop(state, analysis, extCalls)

      {^parent, :stop} ->
        :erlang.exit(analPid, :kill)
        :ok
    end
  end

  defp analysis_start(parent, analysis, legalWarnings) do
    cServer = :dialyzer_codeserver.new()
    plt = r_analysis(analysis, :plt)

    state =
      r_analysis_state(
        codeserver: cServer,
        analysis_type: r_analysis(analysis, :type),
        defines: r_analysis(analysis, :defines),
        doc_plt: r_analysis(analysis, :doc_plt),
        include_dirs: r_analysis(analysis, :include_dirs),
        plt: plt,
        parent: parent,
        legal_warnings: legalWarnings,
        start_from: r_analysis(analysis, :start_from),
        use_contracts: r_analysis(analysis, :use_contracts),
        timing_server: r_analysis(analysis, :timing_server),
        solvers: r_analysis(analysis, :solvers)
      )

    files = :ordsets.from_list(r_analysis(analysis, :files))

    {callgraph, tmpCServer0} =
      compile_and_store(
        files,
        state
      )

    args = {plt, analysis, parent}

    newCServer =
      remote_type_postprocessing(
        tmpCServer0,
        args
      )

    dump_callgraph(callgraph, state, analysis)
    allNodes = :dialyzer_callgraph.all_nodes(callgraph)
    plt1_a = :dialyzer_plt.delete_list(plt, allNodes)

    plt1 =
      :dialyzer_plt.insert_callbacks(
        plt1_a,
        newCServer
      )

    state1 = r_analysis_state(state, codeserver: newCServer, plt: plt1)
    exports = :dialyzer_codeserver.get_exports(newCServer)

    nonExports =
      :sets.subtract(
        :sets.from_list(allNodes),
        exports
      )

    nonExportsList = :sets.to_list(nonExports)

    newCallgraph =
      case r_analysis(analysis, :race_detection) do
        true ->
          :dialyzer_callgraph.put_race_detection(true, callgraph)

        false ->
          callgraph
      end

    state2 = analyze_callgraph(newCallgraph, state1)
    r_analysis_state(plt: plt2, doc_plt: docPlt, codeserver: codeserver0) = state2
    {codeserver, plt3} = move_data(codeserver0, plt2)
    :dialyzer_callgraph.dispose_race_server(newCallgraph)
    dummyPlt = :dialyzer_plt.new()
    send_codeserver_plt(parent, codeserver, dummyPlt)
    :dialyzer_plt.delete(dummyPlt)
    plt4 = :dialyzer_plt.delete_list(plt3, nonExportsList)
    send_analysis_done(parent, plt4, docPlt)
  end

  defp remote_type_postprocessing(tmpCServer, args) do
    fun = fn ->
      exit(
        try do
          remote_type_postproc(tmpCServer, args)
        catch
          {:error, _} = error ->
            error
        else
          r ->
            r
        end
      )
    end

    {pid, ref} = :erlang.spawn_monitor(fun)
    :dialyzer_codeserver.give_away(tmpCServer, pid)
    send(pid, {self(), :go})

    receive do
      {:DOWN, ^ref, :process, ^pid, return} ->
        skip_ets_transfer(pid)

        case return do
          {:error, _ErrorMsg} = error ->
            exit(error)

          _ ->
            return
        end
    end
  end

  defp remote_type_postproc(tmpCServer0, args) do
    {plt, analysis, parent} = args

    (fn ->
       caller =
         receive do
           {pid, :go} ->
             pid
         end

       tmpCServer1 =
         :dialyzer_utils.merge_types(
           tmpCServer0,
           plt
         )

       newExpTypes = :dialyzer_codeserver.get_temp_exported_types(tmpCServer0)
       oldExpTypes0 = :dialyzer_plt.get_exported_types(plt)

       r_analysis(
         start_from: startFrom,
         timing_server: timingServer
       ) = analysis

       files = :ordsets.from_list(r_analysis(analysis, :files))

       remMods =
         for f <- files do
           case startFrom do
             :byte_code ->
               :erlang.list_to_atom(:filename.basename(f, '.beam'))

             :src_code ->
               :erlang.list_to_atom(:filename.basename(f, '.erl'))
           end
         end

       oldExpTypes1 =
         :dialyzer_utils.sets_filter(
           remMods,
           oldExpTypes0
         )

       mergedExpTypes = :sets.union(newExpTypes, oldExpTypes1)

       tmpCServer2 =
         :dialyzer_codeserver.finalize_exported_types(
           mergedExpTypes,
           tmpCServer1
         )

       tmpServer4 =
         (
           :dialyzer_timing.start_stamp(timingServer, 'remote')

           _T =
             (
               tmpCServer3 = :dialyzer_utils.process_record_remote_types(tmpCServer2)
               :dialyzer_contracts.process_contract_remote_types(tmpCServer3)
             )

           :dialyzer_timing.end_stamp(timingServer)
           _T
         )

       rcv_and_send_ext_types(caller, parent)
       :dialyzer_codeserver.give_away(tmpServer4, caller)
       tmpServer4
     end).()
  end

  defp skip_ets_transfer(pid) do
    receive do
      {:"ETS-TRANSFER", _Tid, ^pid, _HeriData} ->
        skip_ets_transfer(pid)
    after
      0 ->
        :ok
    end
  end

  defp move_data(cServer, plt) do
    {cServer1, records} = :dialyzer_codeserver.extract_records(cServer)
    plt1 = :dialyzer_plt.insert_types(plt, records)
    {newCServer, expTypes} = :dialyzer_codeserver.extract_exported_types(cServer1)

    newPlt =
      :dialyzer_plt.insert_exported_types(
        plt1,
        expTypes
      )

    {newCServer, newPlt}
  end

  defp analyze_callgraph(
         callgraph,
         r_analysis_state(
           codeserver: codeserver,
           doc_plt: docPlt,
           plt: plt,
           timing_server: timingServer,
           parent: parent,
           solvers: solvers
         ) = state
       ) do
    case r_analysis_state(state, :analysis_type) do
      :plt_build ->
        newPlt =
          :dialyzer_succ_typings.analyze_callgraph(
            callgraph,
            plt,
            codeserver,
            timingServer,
            solvers,
            parent
          )

        :dialyzer_callgraph.delete(callgraph)
        r_analysis_state(state, plt: newPlt, doc_plt: docPlt)

      :succ_typings ->
        {warnings, newPlt, newDocPlt} =
          :dialyzer_succ_typings.get_warnings(
            callgraph,
            plt,
            docPlt,
            codeserver,
            timingServer,
            solvers,
            parent
          )

        :dialyzer_callgraph.delete(callgraph)
        warnings1 = filter_warnings(warnings, codeserver)
        send_warnings(r_analysis_state(state, :parent), warnings1)
        r_analysis_state(state, plt: newPlt, doc_plt: newDocPlt)
    end
  end

  Record.defrecord(:r_compile_init, :compile_init,
    callgraph: :undefined,
    codeserver: :undefined,
    defines: [],
    include_dirs: [],
    start_from: :byte_code,
    use_contracts: true,
    legal_warnings: :undefined
  )

  defp make_compile_init(
         r_analysis_state(
           codeserver: codeserver,
           defines: defs,
           include_dirs: dirs,
           use_contracts: useContracts,
           legal_warnings: legalWarnings,
           start_from: startFrom
         ),
         callgraph
       ) do
    r_compile_init(
      callgraph: callgraph,
      codeserver: codeserver,
      defines:
        for {macro, val} <- defs do
          {:d, macro, val}
        end,
      include_dirs:
        for d <- dirs do
          {:i, d}
        end,
      use_contracts: useContracts,
      legal_warnings: legalWarnings,
      start_from: startFrom
    )
  end

  defp compile_and_store(
         files,
         r_analysis_state(codeserver: cServer, timing_server: timing, parent: parent) = state
       ) do
    send_log(parent, 'Reading files and computing callgraph... ')
    {t1, _} = :erlang.statistics(:wall_clock)
    callgraph = :dialyzer_callgraph.new()
    compileInit = make_compile_init(state, callgraph)

    {{failed, modules}, nextLabel} =
      (
        :dialyzer_timing.start_stamp(timing, 'compile')

        _C1 =
          :dialyzer_coordinator.parallel_job(
            :compile,
            files,
            compileInit,
            timing
          )

        :dialyzer_timing.end_stamp(timing)
        _C1
      )

    cServer2 =
      :dialyzer_codeserver.set_next_core_label(
        nextLabel,
        cServer
      )

    case failed === [] do
      true ->
        modDict =
          :lists.foldl(
            fn f, dict ->
              modFile = :lists.last(:filename.split(f))
              mod = :filename.basename(modFile, '.beam')
              :dict.append(mod, f, dict)
            end,
            :dict.new(),
            files
          )

        check_for_duplicate_modules(modDict)

      false ->
        msg =
          :io_lib.format(
            'Could not scan the following file(s):~n~ts',
            [
              for {_Filename, reason} <- failed do
                reason
              end
            ]
          )

        exit({:error, msg})
    end

    {t2, _} = :erlang.statistics(:wall_clock)
    msg1 = :io_lib.format('done in ~.2f secs\nRemoving edges... ', [(t2 - t1) / 1000])
    send_log(parent, msg1)

    ^callgraph =
      (
        :dialyzer_timing.start_stamp(timing, 'clean')
        _C2 = cleanup_callgraph(state, cServer2, callgraph, modules)
        :dialyzer_timing.end_stamp(timing)
        _C2
      )

    {t3, _} = :erlang.statistics(:wall_clock)
    msg2 = :io_lib.format('done in ~.2f secs\n', [(t3 - t2) / 1000])
    send_log(parent, msg2)
    {callgraph, cServer2}
  end

  def compile_init_result() do
    {[], []}
  end

  def add_to_result(file, newData, {failed, mods}, initData) do
    case newData do
      {:error, reason} ->
        {[{file, reason} | failed], mods}

      {:ok, v, e, mod} ->
        callgraph = r_compile_init(initData, :callgraph)
        :dialyzer_callgraph.add_edges(e, v, callgraph)
        {failed, [mod | mods]}
    end
  end

  def start_compilation(
        file,
        r_compile_init(
          callgraph: callgraph,
          codeserver: codeserver,
          defines: defines,
          include_dirs: includeD,
          use_contracts: useContracts,
          legal_warnings: legalWarnings,
          start_from: startFrom
        )
      ) do
    case startFrom do
      :src_code ->
        compile_src(file, includeD, defines, callgraph, codeserver, useContracts, legalWarnings)

      :byte_code ->
        compile_byte(file, callgraph, codeserver, useContracts, legalWarnings)
    end
  end

  defp cleanup_callgraph(
         r_analysis_state(plt: initPlt, parent: parent, codeserver: codeServer),
         cServer,
         callgraph,
         modules
       ) do
    moduleDeps = :dialyzer_callgraph.module_deps(callgraph)
    send_mod_deps(parent, moduleDeps)
    {callgraph1, extCalls} = :dialyzer_callgraph.remove_external(callgraph)

    extCalls1 =
      for call = {_From, to} <- extCalls,
          not :dialyzer_plt.contains_mfa(initPlt, to) do
        call
      end

    {badCalls1, realExtCalls} =
      cond do
        extCalls1 === [] ->
          {[], []}

        true ->
          moduleSet = :sets.from_list(modules)
          pltModuleSet = :dialyzer_plt.all_modules(initPlt)

          allModules =
            :sets.union(
              moduleSet,
              pltModuleSet
            )

          pred = fn {_From, {m, _F, _A}} ->
            :sets.is_element(m, allModules)
          end

          :lists.partition(pred, extCalls1)
      end

    nonLocalCalls = :dialyzer_callgraph.non_local_calls(callgraph1)

    badCalls2 =
      for call = {_From, to} <- nonLocalCalls,
          not :dialyzer_codeserver.is_exported(to, cServer) do
        call
      end

    case badCalls1 ++ badCalls2 do
      [] ->
        :ok

      badCalls ->
        send_bad_calls(parent, badCalls, codeServer)
    end

    cond do
      realExtCalls === [] ->
        :ok

      true ->
        send_ext_calls(
          parent,
          :lists.usort(
            for {_From, to} <- realExtCalls do
              to
            end
          )
        )
    end

    callgraph1
  end

  defp compile_src(file, includes, defines, callgraph, cServer, useContracts, legalWarnings) do
    defaultIncludes = default_includes(:filename.dirname(file))
    srcCompOpts = :dialyzer_utils.src_compiler_opts()
    compOpts = srcCompOpts ++ includes ++ defines ++ defaultIncludes

    case :dialyzer_utils.get_core_from_src(
           file,
           compOpts
         ) do
      {:error, _Msg} = error ->
        error

      {:ok, core} ->
        compile_common(core, callgraph, cServer, useContracts, legalWarnings)
    end
  end

  defp compile_byte(file, callgraph, cServer, useContracts, legalWarnings) do
    case :dialyzer_utils.get_core_from_beam(file) do
      {:error, _} = error ->
        error

      {:ok, core} ->
        compile_common(core, callgraph, cServer, useContracts, legalWarnings)
    end
  end

  defp compile_common(core, callgraph, cServer, useContracts, legalWarnings) do
    mod = :cerl.concrete(:cerl.module_name(core))

    case :dialyzer_utils.get_record_and_type_info(core) do
      {:error, _} = error ->
        error

      {:ok, recInfo} ->
        cServer1 = :dialyzer_codeserver.store_temp_records(mod, recInfo, cServer)

        case :dialyzer_utils.get_fun_meta_info(mod, core, legalWarnings) do
          {:error, _} = error ->
            error

          metaFunInfo ->
            cServer2 =
              :dialyzer_codeserver.insert_fun_meta_info(
                metaFunInfo,
                cServer1
              )

            case useContracts do
              true ->
                case :dialyzer_utils.get_spec_info(mod, core, recInfo) do
                  {:error, _} = error ->
                    error

                  {:ok, specInfo, callbackInfo} ->
                    cServer3 =
                      :dialyzer_codeserver.store_temp_contracts(
                        mod,
                        specInfo,
                        callbackInfo,
                        cServer2
                      )

                    store_core(mod, core, callgraph, cServer3)
                end

              false ->
                store_core(mod, core, callgraph, cServer2)
            end
        end
    end
  end

  defp store_core(mod, core, callgraph, cServer) do
    exp = get_exports_from_core(core)
    expTypes = get_exported_types_from_core(core)

    ^cServer =
      :dialyzer_codeserver.insert_exports(
        exp,
        cServer
      )

    ^cServer =
      :dialyzer_codeserver.insert_temp_exported_types(
        expTypes,
        cServer
      )

    coreTree = :cerl.from_records(core)
    coreSize = :cerl_trees.size(coreTree)
    {:ok, coreSize, {mod, coreTree, callgraph, cServer}}
  end

  def continue_compilation(
        nextLabel,
        {mod, coreTree, callgraph, cServer}
      ) do
    {labeledTree, _NewNextLabel} = :cerl_trees.label(coreTree, nextLabel)
    labeledCore = :cerl.to_records(labeledTree)
    store_code_and_build_callgraph(mod, labeledCore, callgraph, cServer)
  end

  defp get_exported_types_from_core(core) do
    attrs = :cerl.module_attrs(core)

    expTypes1 =
      for {l1, l2} <- attrs,
          :cerl.is_literal(l1),
          :cerl.is_literal(l2),
          :cerl.concrete(l1) === :export_type do
        :cerl.concrete(l2)
      end

    expTypes2 = :lists.flatten(expTypes1)
    m = :cerl.atom_val(:cerl.module_name(core))

    :sets.from_list(
      for {f, a} <- expTypes2 do
        {m, f, a}
      end
    )
  end

  defp get_exports_from_core(core) do
    tree = :cerl.from_records(core)
    exports1 = :cerl.module_exports(tree)

    exports2 =
      for v <- exports1 do
        :cerl.var_name(v)
      end

    m = :cerl.atom_val(:cerl.module_name(tree))

    for {f, a} <- exports2 do
      {m, f, a}
    end
  end

  defp store_code_and_build_callgraph(mod, core, callgraph, cServer) do
    coreTree = :cerl.from_records(core)

    {vertices, edges} =
      :dialyzer_callgraph.scan_core_tree(
        coreTree,
        callgraph
      )

    ^cServer = :dialyzer_codeserver.insert(mod, coreTree, cServer)
    {:ok, vertices, edges, mod}
  end

  defp expand_files(
         analysis =
           r_analysis(
             files: files,
             start_from: startFrom
           )
       ) do
    ext =
      case startFrom do
        :byte_code ->
          '.beam'

        :src_code ->
          '.erl'
      end

    case expand_files(files, ext, []) do
      [] ->
        msg =
          'No ' ++
            ext ++
            ' files to analyze' ++
            case startFrom do
              :byte_code ->
                ' (no --src specified?)'

              :src_code ->
                ''
            end

        exit({:error, msg})

      newFiles ->
        r_analysis(analysis, files: newFiles)
    end
  end

  defp expand_files([file | left], ext, fileAcc) do
    case :filelib.is_dir(file) do
      true ->
        {:ok, list} = :file.list_dir(file)

        newFiles =
          :lists.foldl(
            fn x, acc ->
              case :filename.extension(x) === ext do
                true ->
                  [:filename.join(file, x) | acc]

                false ->
                  acc
              end
            end,
            fileAcc,
            list
          )

        expand_files(left, ext, newFiles)

      false ->
        expand_files(left, ext, [file | fileAcc])
    end
  end

  defp expand_files([], _Ext, fileAcc) do
    fileAcc
  end

  defp check_for_duplicate_modules(modDict) do
    duplicates =
      :dict.filter(
        fn
          _, [_] ->
            false

          _, _Files ->
            true
        end,
        modDict
      )

    case :dict.size(duplicates) === 0 do
      true ->
        :ok

      false ->
        mods =
          for {_, x} <- :dict.to_list(duplicates) do
            x
          end

        msg = :io_lib.format('Duplicate modules: ~p', [mods])
        exit({:error, msg})
    end
  end

  defp default_includes(dir) do
    l1 = ['..', '../incl', '../inc', '../include']

    for x <- l1 do
      {:i, :filename.join(dir, x)}
    end
  end

  defp rcv_and_send_ext_types(sendTo, parent) do
    self = self()
    send(self, {self, :done})

    case rcv_ext_types(self, []) do
      [] ->
        :ok

      extTypes ->
        send(parent, {sendTo, :ext_types, extTypes})
        :ok
    end
  end

  defp rcv_ext_types(self, extTypes) do
    receive do
      {^self, :ext_types, extType} ->
        rcv_ext_types(self, [extType | extTypes])

      {^self, :done} ->
        :lists.usort(extTypes)
    end
  end

  defp send_log(parent, msg) do
    send(parent, {self(), :log, msg})
    :ok
  end

  defp send_warnings(_Parent, []) do
    :ok
  end

  defp send_warnings(parent, warnings) do
    send(parent, {self(), :warnings, warnings})
    :ok
  end

  defp filter_warnings(warnings, codeserver) do
    for {tag, warningInfo, _Warning} = tWW <- warnings,
        is_ok_fun(warningInfo, codeserver),
        is_ok_tag(tag, warningInfo, codeserver) do
      tWW
    end
  end

  defp is_ok_fun({_F, _L, module}, _Codeserver)
       when is_atom(module) do
    true
  end

  defp is_ok_fun(
         {_Filename, _Line, {_M, _F, _A} = mFA},
         codeserver
       ) do
    not :dialyzer_utils.is_suppressed_fun(mFA, codeserver)
  end

  defp is_ok_tag(tag, {_F, _L, morMFA}, codeserver) do
    not :dialyzer_utils.is_suppressed_tag(morMFA, tag, codeserver)
  end

  defp send_analysis_done(parent, plt, docPlt) do
    send(parent, {self(), :done, plt, docPlt})
    :ok
  end

  defp send_ext_calls(_Parent, :none) do
    :ok
  end

  defp send_ext_calls(parent, extCalls) do
    send(parent, {self(), :ext_calls, extCalls})
    :ok
  end

  defp send_ext_types(parent, extTypes) do
    send(parent, {self(), :ext_types, extTypes})
    :ok
  end

  defp send_codeserver_plt(parent, cServer, plt) do
    :ok = :dialyzer_codeserver.give_away(cServer, parent)
    send(parent, {self(), :cserver, cServer, plt})
    :ok
  end

  defp send_bad_calls(parent, badCalls, codeServer) do
    formatedBadCalls = format_bad_calls(badCalls, codeServer, [])
    warnings = filter_warnings(formatedBadCalls, codeServer)
    send_warnings(parent, warnings)
  end

  defp send_mod_deps(parent, moduleDeps) do
    send(parent, {self(), :mod_deps, moduleDeps})
    :ok
  end

  defp format_bad_calls([{{_, _, _}, {_, :module_info, a}} | left], codeServer, acc)
       when a === 0 or a === 1 do
    format_bad_calls(left, codeServer, acc)
  end

  defp format_bad_calls([{fromMFA, {m, f, a} = to} | left], codeServer, acc) do
    {_Var, funCode} =
      :dialyzer_codeserver.lookup_mfa_code(
        fromMFA,
        codeServer
      )

    msg = {:call_to_missing, [m, f, a]}
    {file, line} = find_call_file_and_line(fromMFA, funCode, to, codeServer)
    warningInfo = {file, line, fromMFA}
    newAcc = [{:warn_callgraph, warningInfo, msg} | acc]
    format_bad_calls(left, codeServer, newAcc)
  end

  defp format_bad_calls([], _CodeServer, acc) do
    acc
  end

  defp find_call_file_and_line({module, _, _}, tree, mFA, codeServer) do
    fun = fn subTree, acc ->
      case :cerl.is_c_call(subTree) do
        true ->
          m = :cerl.call_module(subTree)
          f = :cerl.call_name(subTree)
          a = :cerl.call_arity(subTree)

          case :cerl.is_c_atom(m) and :cerl.is_c_atom(f) do
            true ->
              case {:cerl.concrete(m), :cerl.concrete(f), a} do
                ^mFA ->
                  ann = :cerl.get_ann(subTree)

                  [
                    {get_file(codeServer, module, ann), get_line(ann)}
                    | acc
                  ]

                {:erlang, :make_fun, 3} ->
                  [cA1, cA2, cA3] = :cerl.call_args(subTree)

                  case :cerl.is_c_atom(cA1) and :cerl.is_c_atom(cA2) and :cerl.is_c_int(cA3) do
                    true ->
                      case {:cerl.concrete(cA1), :cerl.concrete(cA2), :cerl.concrete(cA3)} do
                        ^mFA ->
                          ann = :cerl.get_ann(subTree)

                          [
                            {get_file(codeServer, module, ann), get_line(ann)}
                            | acc
                          ]

                        _ ->
                          acc
                      end

                    false ->
                      acc
                  end

                _ ->
                  acc
              end

            false ->
              acc
          end

        false ->
          acc
      end
    end

    hd(:cerl_trees.fold(fun, [], tree))
  end

  defp get_line([line | _]) when is_integer(line) do
    line
  end

  defp get_line([_ | tail]) do
    get_line(tail)
  end

  defp get_line([]) do
    -1
  end

  defp get_file(codeserver, module, [{:file, fakeFile} | _]) do
    :dialyzer_codeserver.translate_fake_file(codeserver, module, fakeFile)
  end

  defp get_file(codeserver, module, [_ | tail]) do
    get_file(codeserver, module, tail)
  end

  defp dump_callgraph(_CallGraph, _State, r_analysis(callgraph_file: '')) do
    :ok
  end

  defp dump_callgraph(callGraph, state, r_analysis(callgraph_file: file) = analysis) do
    extension = :filename.extension(file)
    start_Msg = :io_lib.format('Dumping the callgraph... ', [])
    send_log(r_analysis_state(state, :parent), start_Msg)
    {t1, _} = :erlang.statistics(:wall_clock)
    dump_callgraph(callGraph, state, analysis, extension)
    {t2, _} = :erlang.statistics(:wall_clock)
    finish_Msg = :io_lib.format('done in ~2f secs\n', [(t2 - t1) / 1000])
    send_log(r_analysis_state(state, :parent), finish_Msg)
    :ok
  end

  defp dump_callgraph(callGraph, _State, r_analysis(callgraph_file: file), '.dot') do
    :dialyzer_callgraph.to_dot(callGraph, file)
  end

  defp dump_callgraph(callGraph, _State, r_analysis(callgraph_file: file), '.ps') do
    args = '-Gratio=compress -Gsize="100,100"'
    :dialyzer_callgraph.to_ps(callGraph, file, args)
  end

  defp dump_callgraph(callGraph, state, r_analysis(callgraph_file: file), _Ext) do
    case :file.open(file, [:write]) do
      {:ok, fd} ->
        :io.format(fd, '~p', [callGraph])
        :ok = :file.close(fd)

      {:error, reason} ->
        msg = :io_lib.format('Could not open output file ~tp, Reason: ~p\n', [file, reason])
        send_log(r_analysis_state(state, :parent), msg)
    end
  end
end
