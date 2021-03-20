defmodule :m_cover do
  use Bitwise
  require Record

  Record.defrecord(:r_main_state, :main_state,
    compiled: [],
    imported: [],
    stopper: :undefined,
    local_only: false,
    nodes: [],
    lost_nodes: []
  )

  Record.defrecord(:r_remote_data, :remote_data,
    module: :undefined,
    file: :undefined,
    code: :undefined,
    mapping: :undefined,
    clauses: :undefined
  )

  Record.defrecord(:r_remote_state, :remote_state,
    compiled: [],
    main_node: :undefined
  )

  Record.defrecord(:r_bump, :bump, module: :_, function: :_, arity: :_, clause: :_, line: :_)

  Record.defrecord(:r_vars, :vars,
    module: :undefined,
    init_info: [],
    function: :undefined,
    arity: :undefined,
    clause: :undefined,
    lines: :undefined,
    no_bump_lines: :undefined,
    depth: :undefined,
    is_guard: false
  )

  def start() do
    case :erlang.whereis(:cover_server) do
      :undefined ->
        starter = self()

        pid =
          spawn(fn ->
            :erlang.put(:start, [])
            init_main(starter)
          end)

        ref = :erlang.monitor(:process, pid)

        return =
          receive do
            {:cover_server, :started} ->
              {:ok, pid}

            {:cover_server, {:error, error}} ->
              {:error, error}

            {:DOWN, ^ref, _Type, _Object, info} ->
              {:error, info}
          end

        :erlang.demonitor(ref)
        return

      pid ->
        {:error, {:already_started, pid}}
    end
  end

  def start(node) when is_atom(node) do
    start([node])
  end

  def start(nodes) do
    call({:start_nodes, remove_myself(nodes, [])})
  end

  def local_only() do
    call(:local_only)
  end

  def compile(modFile) do
    compile_module(modFile, [])
  end

  def compile(modFile, options) do
    compile_module(modFile, options)
  end

  def compile_module(modFile)
      when is_atom(modFile) or
             is_list(modFile) do
    compile_module(modFile, [])
  end

  def compile_module(modFile, options)
      when is_atom(modFile) or
             (is_list(modFile) and
                is_integer(hd(modFile))) do
    [r] = compile_module([modFile], options)
    r
  end

  def compile_module(modFiles, options) when is_list(options) do
    absFiles =
      for modFile <- modFiles do
        file =
          case modFile do
            _ when is_atom(modFile) ->
              :erlang.atom_to_list(modFile)

            _ when is_list(modFile) ->
              modFile
          end

        withExt =
          case :filename.extension(file) do
            '.erl' ->
              file

            _ ->
              file ++ '.erl'
          end

        :filename.absname(withExt)
      end

    compile_modules(absFiles, options)
  end

  def compile_directory() do
    case :file.get_cwd() do
      {:ok, dir} ->
        compile_directory(dir, [])

      error ->
        error
    end
  end

  def compile_directory(dir) when is_list(dir) do
    compile_directory(dir, [])
  end

  def compile_directory(dir, options)
      when is_list(dir) and
             is_list(options) do
    case :file.list_dir(dir) do
      {:ok, files} ->
        erlFiles =
          for file <- files,
              :filename.extension(file) === '.erl' do
            :filename.join(dir, file)
          end

        compile_modules(erlFiles, options)

      error ->
        error
    end
  end

  defp compile_modules(files, options) do
    options2 = filter_options(options)
    call({:compile, files, options2})
  end

  defp filter_options(options) do
    :lists.filter(
      fn option ->
        case option do
          {:i, dir} when is_list(dir) ->
            true

          {:d, _Macro} ->
            true

          {:d, _Macro, _Value} ->
            true

          :export_all ->
            true

          _ ->
            false
        end
      end,
      options
    )
  end

  def compile_beam(modFile0)
      when is_atom(modFile0) or
             (is_list(modFile0) and is_integer(hd(modFile0))) do
    case compile_beams([modFile0]) do
      [{:error, {:non_existing, _}}] ->
        {:error, :non_existing}

      [result] ->
        result
    end
  end

  def compile_beam(modFiles) when is_list(modFiles) do
    compile_beams(modFiles)
  end

  def compile_beam_directory() do
    case :file.get_cwd() do
      {:ok, dir} ->
        compile_beam_directory(dir)

      error ->
        error
    end
  end

  def compile_beam_directory(dir) when is_list(dir) do
    case :file.list_dir(dir) do
      {:ok, files} ->
        beamFiles =
          for file <- files,
              :filename.extension(file) === '.beam' do
            :filename.join(dir, file)
          end

        compile_beams(beamFiles)

      error ->
        error
    end
  end

  defp compile_beams(modFiles0) do
    modFiles = get_mods_and_beams(modFiles0, [])
    call({:compile_beams, modFiles})
  end

  defp get_mods_and_beams([module | modFiles], acc)
       when is_atom(module) do
    case :code.which(module) do
      :non_existing ->
        get_mods_and_beams(
          modFiles,
          [{:error, {:non_existing, module}} | acc]
        )

      file ->
        get_mods_and_beams([{module, file} | modFiles], acc)
    end
  end

  defp get_mods_and_beams([file | modFiles], acc) when is_list(file) do
    {withExt, withoutExt} =
      case :filename.rootname(
             file,
             '.beam'
           ) do
        ^file ->
          {file ++ '.beam', file}

        rootname ->
          {file, rootname}
      end

    absFile = :filename.absname(withExt)
    module = :erlang.list_to_atom(:filename.basename(withoutExt))
    get_mods_and_beams([{module, absFile} | modFiles], acc)
  end

  defp get_mods_and_beams([{module, file} | modFiles], acc) do
    case :lists.keyfind(module, 2, acc) do
      {:ok, ^module, ^file} ->
        get_mods_and_beams(modFiles, acc)

      {:ok, ^module, _OtherFile} ->
        get_mods_and_beams(
          modFiles,
          [{:error, {:duplicate, module}} | acc]
        )

      _ ->
        get_mods_and_beams(
          modFiles,
          [{:ok, module, file} | acc]
        )
    end
  end

  defp get_mods_and_beams([], acc) do
    :lists.reverse(acc)
  end

  def analyse() do
    analyse(:_)
  end

  def analyse(analysis)
      when analysis === :coverage or analysis === :calls do
    analyse(:_, analysis)
  end

  def analyse(level)
      when level === :line or level === :clause or level === :function or level === :module do
    analyse(:_, level)
  end

  def analyse(module) do
    analyse(module, :coverage)
  end

  def analyse(analysis, level)
      when (analysis === :coverage or analysis === :calls) and
             (level === :line or level === :clause or level === :function or level === :module) do
    analyse(:_, analysis, level)
  end

  def analyse(module, analysis)
      when analysis === :coverage or analysis === :calls do
    analyse(module, analysis, :function)
  end

  def analyse(module, level)
      when level === :line or level === :clause or level === :function or level === :module do
    analyse(module, :coverage, level)
  end

  def analyse(module, analysis, level)
      when analysis === :coverage or
             (analysis === :calls and
                level === :line) or level === :clause or level === :function or level === :module do
    call({{:analyse, analysis, level}, module})
  end

  def analyze() do
    analyse()
  end

  def analyze(module) do
    analyse(module)
  end

  def analyze(module, analysis) do
    analyse(module, analysis)
  end

  def analyze(module, analysis, level) do
    analyse(module, analysis, level)
  end

  def analyse_to_file() do
    analyse_to_file(:_)
  end

  def analyse_to_file(arg) do
    case is_options(arg) do
      true ->
        analyse_to_file(:_, arg)

      false ->
        analyse_to_file(arg, [])
    end
  end

  def analyse_to_file(module, outFile)
      when is_list(outFile) and
             is_integer(hd(outFile)) do
    analyse_to_file(module, [{:outfile, outFile}])
  end

  def analyse_to_file(module, options) when is_list(options) do
    call({{:analyse_to_file, options}, module})
  end

  def analyse_to_file(module, outFile, options)
      when is_list(outFile) do
    analyse_to_file(module, [{:outfile, outFile} | options])
  end

  def analyze_to_file() do
    analyse_to_file()
  end

  def analyze_to_file(module) do
    analyse_to_file(module)
  end

  def analyze_to_file(module, optOrOut) do
    analyse_to_file(module, optOrOut)
  end

  def analyze_to_file(module, outFile, options) do
    analyse_to_file(module, outFile, options)
  end

  def async_analyse_to_file(module) do
    do_spawn(:cover, :analyse_to_file, [module])
  end

  def async_analyse_to_file(module, outFileOrOpts) do
    do_spawn(:cover, :analyse_to_file, [module, outFileOrOpts])
  end

  def async_analyse_to_file(module, outFile, options) do
    do_spawn(:cover, :analyse_to_file, [module, outFile, options])
  end

  defp is_options([:html]) do
    true
  end

  defp is_options([:html | opts]) do
    is_options(opts)
  end

  defp is_options([{opt, _} | _])
       when opt == :outfile or
              opt == :outdir do
    true
  end

  defp is_options(_) do
    false
  end

  defp do_spawn(m, f, a) do
    spawn_link(fn ->
      case apply(m, f, a) do
        {:ok, _} ->
          :ok

        {:error, reason} ->
          exit(reason)
      end
    end)
  end

  def async_analyze_to_file(module) do
    async_analyse_to_file(module)
  end

  def async_analyze_to_file(module, outFileOrOpts) do
    async_analyse_to_file(module, outFileOrOpts)
  end

  def async_analyze_to_file(module, outFile, options) do
    async_analyse_to_file(module, outFile, options)
  end

  defp outfilename(:undefined, module, hTML) do
    outfilename(module, hTML)
  end

  defp outfilename(outDir, module, hTML) do
    :filename.join(outDir, outfilename(module, hTML))
  end

  defp outfilename(module, true) do
    :erlang.atom_to_list(module) ++ '.COVER.html'
  end

  defp outfilename(module, false) do
    :erlang.atom_to_list(module) ++ '.COVER.out'
  end

  def export(file) do
    export(file, :_)
  end

  def export(file, module) do
    call({:export, file, module})
  end

  def import(file) do
    call({:import, file})
  end

  def modules() do
    call(:modules)
  end

  def imported_modules() do
    call(:imported_modules)
  end

  def imported() do
    call(:imported)
  end

  def which_nodes() do
    call(:which_nodes)
  end

  def is_compiled(module) when is_atom(module) do
    call({:is_compiled, module})
  end

  def reset(module) when is_atom(module) do
    call({:reset, module})
  end

  def reset() do
    call(:reset)
  end

  def stop() do
    call(:stop)
  end

  def stop(node) when is_atom(node) do
    stop([node])
  end

  def stop(nodes) do
    call({:stop, remove_myself(nodes, [])})
  end

  def flush(node) when is_atom(node) do
    flush([node])
  end

  def flush(nodes) do
    call({:flush, remove_myself(nodes, [])})
  end

  def get_main_node() do
    call(:get_main_node)
  end

  defp call(request) do
    ref = :erlang.monitor(:process, :cover_server)

    receive do
      {:DOWN, ^ref, _Type, _Object, :noproc} ->
        :erlang.demonitor(ref)
        {:ok, _} = start()
        call(request)
    after
      0 ->
        send(:cover_server, {self(), request})

        return =
          receive do
            {:DOWN, ^ref, _Type, _Object, info} ->
              exit(info)

            {:cover_server, reply} ->
              reply
          end

        :erlang.demonitor(ref, [:flush])
        return
    end
  end

  defp reply(from, reply) do
    send(from, {:cover_server, reply})
    :ok
  end

  defp is_from(from) do
    is_pid(from)
  end

  defp remote_call(node, request) do
    ref = :erlang.monitor(:process, {:cover_server, node})

    receive do
      {:DOWN, ^ref, _Type, _Object, :noproc} ->
        :erlang.demonitor(ref)
        {:error, :node_dead}
    after
      0 ->
        send({:cover_server, node}, request)

        return =
          receive do
            {:DOWN, ^ref, _Type, _Object, _Info} ->
              case request do
                {:remote, :stop} ->
                  :ok

                _ ->
                  {:error, :node_dead}
              end

            {:cover_server, reply} ->
              reply
          end

        :erlang.demonitor(ref, [:flush])
        return
    end
  end

  defp remote_reply(proc, reply) when is_pid(proc) do
    send(proc, {:cover_server, reply})
    :ok
  end

  defp remote_reply(mainNode, reply) do
    send({:cover_server, mainNode}, {:cover_server, reply})
    :ok
  end

  defp init_main(starter) do
    try do
      :erlang.register(:cover_server, self())
    catch
      :error, :badarg ->
        case :erlang.whereis(:cover_server) do
          :undefined ->
            init_main(starter)

          pid ->
            send(starter, {:cover_server, {:error, {:already_started, pid}}})
        end
    else
      true ->
        :cover_internal_mapping_table =
          :ets.new(
            :cover_internal_mapping_table,
            [:ordered_set, :public, :named_table]
          )

        :cover_internal_clause_table =
          :ets.new(
            :cover_internal_clause_table,
            [:set, :public, :named_table]
          )

        :cover_binary_code_table =
          :ets.new(
            :cover_binary_code_table,
            [:set, :public, :named_table]
          )

        :cover_collected_remote_data_table =
          :ets.new(
            :cover_collected_remote_data_table,
            [:set, :public, :named_table]
          )

        :cover_collected_remote_clause_table =
          :ets.new(
            :cover_collected_remote_clause_table,
            [:set, :public, :named_table]
          )

        :ok = :net_kernel.monitor_nodes(true)
        send(starter, {:cover_server, :started})
        main_process_loop(r_main_state())
    end
  end

  def main_process_loop(state) do
    receive do
      {from, :local_only} ->
        case state do
          r_main_state(compiled: [], nodes: []) ->
            reply(from, :ok)
            main_process_loop(r_main_state(state, local_only: true))

          r_main_state() ->
            reply(from, {:error, :too_late})
            main_process_loop(state)
        end

      {from, {:start_nodes, nodes}} ->
        case r_main_state(state, :local_only) do
          false ->
            {startedNodes, state1} = do_start_nodes(nodes, state)
            reply(from, {:ok, startedNodes})
            main_process_loop(state1)

          true ->
            reply(from, {:error, :local_only})
            main_process_loop(state)
        end

      {from, {:compile, files, options}} ->
        {r, s} = do_compile(files, options, state)
        reply(from, r)
        :cover.main_process_loop(s)

      {from, {:compile_beams, modsAndFiles}} ->
        {r, s} = do_compile_beams(modsAndFiles, state)
        reply(from, r)
        :cover.main_process_loop(s)

      {from, {:export, outFile, module}} ->
        spawn(fn ->
          :erlang.put(:export, {outFile, module})
          do_export(module, outFile, from, state)
        end)

        main_process_loop(state)

      {from, {:import, file}} ->
        case :file.open(file, [:read, :binary, :raw]) do
          {:ok, fd} ->
            imported = do_import_to_table(fd, file, r_main_state(state, :imported))
            reply(from, :ok)
            :ok = :file.close(fd)
            main_process_loop(r_main_state(state, imported: imported))

          {:error, reason} ->
            reply(from, {:error, {:cant_open_file, file, reason}})
            main_process_loop(state)
        end

      {from, :modules} ->
        {loadedModules, compiled} =
          get_compiled_still_loaded(
            r_main_state(state, :nodes),
            r_main_state(state, :compiled)
          )

        reply(from, loadedModules)
        main_process_loop(r_main_state(state, compiled: compiled))

      {from, :imported_modules} ->
        importedModules =
          :lists.map(
            fn {mod, _File, _ImportFile} ->
              mod
            end,
            r_main_state(state, :imported)
          )

        reply(from, importedModules)
        main_process_loop(state)

      {from, :imported} ->
        reply(
          from,
          get_all_importfiles(r_main_state(state, :imported), [])
        )

        main_process_loop(state)

      {from, :which_nodes} ->
        reply(from, r_main_state(state, :nodes))
        main_process_loop(state)

      {from, :reset} ->
        :lists.foreach(
          fn {module, _File} ->
            do_reset_main_node(module, r_main_state(state, :nodes))
          end,
          r_main_state(state, :compiled)
        )

        reply(from, :ok)
        main_process_loop(r_main_state(state, imported: []))

      {from, {:stop, nodes}} ->
        remote_collect(:_, nodes, true)
        reply(from, :ok)
        nodes1 = r_main_state(state, :nodes) -- nodes
        lostNodes1 = r_main_state(state, :lost_nodes) -- nodes

        main_process_loop(
          r_main_state(state,
            nodes: nodes1,
            lost_nodes: lostNodes1
          )
        )

      {from, {:flush, nodes}} ->
        remote_collect(:_, nodes, false)
        reply(from, :ok)
        main_process_loop(state)

      {from, :stop} ->
        :lists.foreach(
          fn node ->
            remote_call(node, {:remote, :stop})
          end,
          r_main_state(state, :nodes)
        )

        reload_originals(r_main_state(state, :compiled))
        :ets.delete(:cover_internal_mapping_table)
        :ets.delete(:cover_internal_clause_table)
        :ets.delete(:cover_binary_code_table)
        :ets.delete(:cover_collected_remote_data_table)
        :ets.delete(:cover_collected_remote_clause_table)
        delete_all_counters()
        :erlang.unregister(:cover_server)
        reply(from, :ok)

      {from, {{:analyse, analysis, level}, :_}} ->
        r = analyse_all(analysis, level, state)
        reply(from, r)
        main_process_loop(state)

      {from, {{:analyse, analysis, level}, modules}}
      when is_list(modules) ->
        r = analyse_list(modules, analysis, level, state)
        reply(from, r)
        main_process_loop(state)

      {from, {{:analyse, analysis, level}, module}} ->
        s =
          try do
            loaded = is_loaded(module, state)

            spawn(fn ->
              :erlang.put(:analyse, {module, analysis, level})
              do_parallel_analysis(module, analysis, level, loaded, from, state)
            end)

            state
          catch
            reason ->
              reply(from, {:error, {:not_cover_compiled, module}})
              not_loaded(module, reason, state)
          end

        main_process_loop(s)

      {from, {{:analyse_to_file, opts}, :_}} ->
        r = analyse_all_to_file(opts, state)
        reply(from, r)
        main_process_loop(state)

      {from, {{:analyse_to_file, opts}, modules}}
      when is_list(modules) ->
        r = analyse_list_to_file(modules, opts, state)
        reply(from, r)
        main_process_loop(state)

      {from, {{:analyse_to_file, opts}, module}} ->
        s =
          try do
            loaded = is_loaded(module, state)

            spawn_link(fn ->
              :erlang.put(:analyse_to_file, {module, opts})
              do_parallel_analysis_to_file(module, opts, loaded, from, state)
            end)

            state
          catch
            reason ->
              reply(from, {:error, {:not_cover_compiled, module}})
              not_loaded(module, reason, state)
          end

        main_process_loop(s)

      {from, {:is_compiled, module}} ->
        s =
          try do
            is_loaded(module, state)
          catch
            reason ->
              reply(from, false)
              not_loaded(module, reason, state)
          else
            {:loaded, file} ->
              reply(from, {:file, file})
              state

            {:imported, _File, _ImportFiles} ->
              reply(from, false)
              state
          end

        main_process_loop(s)

      {from, {:reset, module}} ->
        s =
          try do
            loaded = is_loaded(module, state)

            r =
              case loaded do
                {:loaded, _File} ->
                  do_reset_main_node(module, r_main_state(state, :nodes))

                {:imported, _File, _} ->
                  do_reset_collection_table(module)
              end

            imported = remove_imported(module, r_main_state(state, :imported))
            reply(from, r)
            r_main_state(state, imported: imported)
          catch
            reason ->
              reply(from, {:error, {:not_cover_compiled, module}})
              not_loaded(module, reason, state)
          end

        main_process_loop(s)

      {:DOWN, _MRef, :process, {:cover_server, node}, _Info} ->
        {nodes, lost} =
          case :lists.member(
                 node,
                 r_main_state(state, :nodes)
               ) do
            true ->
              n = r_main_state(state, :nodes) -- [node]
              l = [node | r_main_state(state, :lost_nodes)]
              {n, l}

            false ->
              {r_main_state(state, :nodes), r_main_state(state, :lost_nodes)}
          end

        main_process_loop(
          r_main_state(state,
            nodes: nodes,
            lost_nodes: lost
          )
        )

      {:nodeup, node} ->
        state1 =
          case :lists.member(
                 node,
                 r_main_state(state, :lost_nodes)
               ) do
            true ->
              sync_compiled(node, state)

            false ->
              state
          end

        main_process_loop(state1)

      {:nodedown, _} ->
        main_process_loop(state)

      {from, :get_main_node} ->
        reply(from, node())
        main_process_loop(state)

      :get_status ->
        :io.format('~tp~n', [state])
        main_process_loop(state)
    end
  end

  defp init_remote(starter, mainNode) do
    :erlang.register(:cover_server, self())

    :cover_internal_mapping_table =
      :ets.new(
        :cover_internal_mapping_table,
        [:ordered_set, :public, :named_table]
      )

    :cover_internal_clause_table =
      :ets.new(
        :cover_internal_clause_table,
        [:set, :public, :named_table]
      )

    send(starter, {self(), :started})
    remote_process_loop(r_remote_state(main_node: mainNode))
  end

  def remote_process_loop(state) do
    receive do
      {:remote, :load_compiled, compiled} ->
        compiled1 = load_compiled(compiled, r_remote_state(state, :compiled))
        remote_reply(r_remote_state(state, :main_node), :ok)
        :cover.remote_process_loop(r_remote_state(state, compiled: compiled1))

      {:remote, :unload, unloadedModules} ->
        unload(unloadedModules)

        compiled =
          update_compiled(
            unloadedModules,
            r_remote_state(state, :compiled)
          )

        remote_reply(r_remote_state(state, :main_node), :ok)
        remote_process_loop(r_remote_state(state, compiled: compiled))

      {:remote, :reset, module} ->
        reset_counters(module)
        remote_reply(r_remote_state(state, :main_node), :ok)
        remote_process_loop(state)

      {:remote, :collect, module, collectorPid} ->
        send(self(), {:remote, :collect, module, collectorPid, :cover_server})

      {:remote, :collect, modules0, collectorPid, from} ->
        modules =
          case modules0 do
            :_ ->
              for {m, _} <- r_remote_state(state, :compiled) do
                m
              end

            _ ->
              modules0
          end

        spawn(fn ->
          :erlang.put(
            :remote_collect,
            {modules, collectorPid, from}
          )

          do_collect(modules, collectorPid, from)
        end)

        remote_process_loop(state)

      {:remote, :stop} ->
        reload_originals(r_remote_state(state, :compiled))
        :ets.delete(:cover_internal_mapping_table)
        :ets.delete(:cover_internal_clause_table)
        delete_all_counters()
        :erlang.unregister(:cover_server)
        :ok

      {:remote, :get_compiled} ->
        remote_reply(r_remote_state(state, :main_node), r_remote_state(state, :compiled))
        remote_process_loop(state)

      {from, :get_main_node} ->
        remote_reply(from, r_remote_state(state, :main_node))
        remote_process_loop(state)

      :get_status ->
        :io.format('~tp~n', [state])
        remote_process_loop(state)

      m ->
        :io.format('WARNING: remote cover_server received\n~p\n', [m])

        case m do
          {from, _} ->
            case is_from(from) do
              true ->
                reply(from, {:error, :not_main_node})

              false ->
                :ok
            end

          _ ->
            :ok
        end

        remote_process_loop(state)
    end
  end

  defp do_collect(modules, collectorPid, from) do
    _ =
      pmap(
        fn module ->
          send_counters(module, collectorPid)
        end,
        modules
      )

    send(collectorPid, :done)
    remote_reply(from, :ok)
  end

  defp send_chunk(collectorPid, chunk) do
    send(collectorPid, {:chunk, chunk, self()})

    receive do
      :continue ->
        :ok
    end
  end

  defp get_downs([]) do
    :ok
  end

  defp get_downs(mons) do
    receive do
      {:DOWN, ref, _Type, pid, _Reason} = down ->
        case :lists.member({pid, ref}, mons) do
          true ->
            get_downs(:lists.delete({pid, ref}, mons))

          false ->
            send(self(), down)
            get_downs(mons)
        end
    end
  end

  defp reload_originals(compiled) do
    _ =
      pmap(
        &do_reload_original/1,
        for {m, _} <- compiled do
          m
        end
      )

    :ok
  end

  defp do_reload_original(module) do
    case :code.which(module) do
      :cover_compiled ->
        _ = :code.purge(module)
        _ = :code.delete(module)
        _ = :code.load_file(module)
        _ = :code.purge(module)

      _ ->
        :ignore
    end
  end

  defp load_compiled([data | compiled], acc) do
    r_remote_data(
      module: module,
      file: file,
      code: beam,
      mapping: initialMapping,
      clauses: initialClauses
    ) = data

    :ets.insert(
      :cover_internal_mapping_table,
      initialMapping
    )

    :ets.insert(
      :cover_internal_clause_table,
      initialClauses
    )

    maybe_create_counters(module, true)

    sticky =
      case :code.is_sticky(module) do
        true ->
          :code.unstick_mod(module)
          true

        false ->
          false
      end

    newAcc =
      case :code.load_binary(module, :cover_compiled, beam) do
        {:module, ^module} ->
          add_compiled(module, file, acc)

        _ ->
          do_clear(module)
          acc
      end

    case sticky do
      true ->
        :code.stick_mod(module)

      false ->
        :ok
    end

    load_compiled(compiled, newAcc)
  end

  defp load_compiled([], acc) do
    acc
  end

  defp unload([module | modules]) do
    do_clear(module)
    do_reload_original(module)
    unload(modules)
  end

  defp unload([]) do
    :ok
  end

  defp do_start_nodes(nodes, state) do
    thisNode = node()

    startedNodes =
      :lists.foldl(
        fn node, acc ->
          case :rpc.call(node, :cover, :remote_start, [thisNode]) do
            {:ok, _RPid} ->
              :erlang.monitor(
                :process,
                {:cover_server, node}
              )

              [node | acc]

            error ->
              :io.format('Could not start cover on ~w: ~tp\n', [node, error])
              acc
          end
        end,
        [],
        nodes
      )

    {_LoadedModules, compiled} =
      get_compiled_still_loaded(
        r_main_state(state, :nodes),
        r_main_state(state, :compiled)
      )

    remote_load_compiled(startedNodes, compiled)

    state1 =
      r_main_state(state,
        nodes: r_main_state(state, :nodes) ++ startedNodes,
        compiled: compiled
      )

    {startedNodes, state1}
  end

  def remote_start(mainNode) do
    case :erlang.whereis(:cover_server) do
      :undefined ->
        starter = self()

        pid =
          spawn(fn ->
            :erlang.put(:remote_start, {mainNode})
            init_remote(starter, mainNode)
          end)

        ref = :erlang.monitor(:process, pid)

        return =
          receive do
            {^pid, :started} ->
              {:ok, pid}

            {:DOWN, ^ref, _Type, _Object, info} ->
              {:error, info}
          end

        :erlang.demonitor(ref)
        return

      pid ->
        {:error, {:already_started, pid}}
    end
  end

  defp sync_compiled(node, state) do
    r_main_state(compiled: compiled0, nodes: nodes, lost_nodes: lost) = state

    state1 =
      case remote_call(
             node,
             {:remote, :get_compiled}
           ) do
        {:error, :node_dead} ->
          {_, s} = do_start_nodes([node], state)
          s

        {:error, _} ->
          state

        remoteCompiled ->
          {_, compiled} =
            get_compiled_still_loaded(
              nodes,
              compiled0
            )

          unload =
            for {uM, _} = u <- remoteCompiled,
                false == :lists.member(u, compiled) do
              uM
            end

          remote_unload([node], unload)

          load =
            for l <- compiled,
                false == :lists.member(l, remoteCompiled) do
              l
            end

          remote_load_compiled([node], load)
          r_main_state(state, compiled: compiled, nodes: [node | nodes])
      end

    r_main_state(state1, lost_nodes: lost -- [node])
  end

  defp remote_load_compiled(nodes, compiled) do
    remote_load_compiled(nodes, compiled, [], 0)
  end

  defp remote_load_compiled(_Nodes, [], [], _ModNum) do
    :ok
  end

  defp remote_load_compiled(nodes, compiled, acc, modNum)
       when compiled == [] or modNum == 10 do
    remoteLoadData = get_downs_r(acc)

    :lists.foreach(
      fn node ->
        remote_call(
          node,
          {:remote, :load_compiled, remoteLoadData}
        )
      end,
      nodes
    )

    remote_load_compiled(nodes, compiled, [], 0)
  end

  defp remote_load_compiled(nodes, [mF | rest], acc, modNum) do
    remote_load_compiled(
      nodes,
      rest,
      [
        spawn_job_r(fn ->
          get_data_for_remote_loading(mF)
        end)
        | acc
      ],
      modNum + 1
    )
  end

  defp spawn_job_r(fun) do
    spawn_monitor(fn ->
      exit(fun.())
    end)
  end

  defp get_downs_r([]) do
    []
  end

  defp get_downs_r(mons) do
    receive do
      {:DOWN, ref, _Type, pid, r_remote_data() = r} ->
        [r | get_downs_r(:lists.delete({pid, ref}, mons))]

      {:DOWN, ref, _Type, pid, reason} = down ->
        case :lists.member({pid, ref}, mons) do
          true ->
            exit(reason)

          false ->
            send(self(), down)
            get_downs_r(mons)
        end
    end
  end

  defp get_data_for_remote_loading({module, file}) do
    [{^module, code}] = :ets.lookup(:cover_binary_code_table, module)
    mapping = counters_mapping_table(module)

    initialClauses =
      :ets.lookup(
        :cover_internal_clause_table,
        module
      )

    r_remote_data(
      module: module,
      file: file,
      code: code,
      mapping: mapping,
      clauses: initialClauses
    )
  end

  defp remote_unload(nodes, unloadedModules) do
    :lists.foreach(
      fn node ->
        remote_call(node, {:remote, :unload, unloadedModules})
      end,
      nodes
    )
  end

  defp remote_reset(module, nodes) do
    :lists.foreach(
      fn node ->
        remote_call(node, {:remote, :reset, module})
      end,
      nodes
    )
  end

  defp remote_collect(modules, nodes, stop) do
    _ =
      pmap(
        fn node ->
          :erlang.put(:remote_collect, {modules, nodes, stop})
          do_collection(node, modules, stop)
        end,
        nodes
      )

    :ok
  end

  defp do_collection(node, module, stop) do
    collectorPid = spawn(&collector_proc/0)

    case remote_call(
           node,
           {:remote, :collect, module, collectorPid, self()}
         ) do
      {:error, :node_dead} ->
        send(collectorPid, :done)
        :ok

      :ok when stop ->
        remote_call(node, {:remote, :stop})

      :ok ->
        :ok
    end
  end

  defp collector_proc() do
    :erlang.put(:collector_proc, [])

    receive do
      {:chunk, chunk, from} ->
        insert_in_collection_table(chunk)
        send(from, :continue)
        collector_proc()

      :done ->
        :ok
    end
  end

  defp insert_in_collection_table([{key, val} | chunk]) do
    insert_in_collection_table(key, val)
    insert_in_collection_table(chunk)
  end

  defp insert_in_collection_table([]) do
    :ok
  end

  defp insert_in_collection_table(key, val) do
    case :ets.member(
           :cover_collected_remote_data_table,
           key
         ) do
      true ->
        _ = :ets.update_counter(:cover_collected_remote_data_table, key, val)
        :ok

      false ->
        case :ets.insert_new(
               :cover_collected_remote_data_table,
               {key, val}
             ) do
          false ->
            insert_in_collection_table(key, val)

          _ ->
            :ok
        end
    end
  end

  defp remove_myself([node | nodes], acc) when node === node() do
    remove_myself(nodes, acc)
  end

  defp remove_myself([node | nodes], acc) do
    remove_myself(nodes, [node | acc])
  end

  defp remove_myself([], acc) do
    acc
  end

  defp analyse_info(_Module, []) do
    :ok
  end

  defp analyse_info(module, imported) do
    imported_info('Analysis', module, imported)
  end

  defp export_info(_Module, []) do
    :ok
  end

  defp export_info(_Module, _Imported) do
    :ok
  end

  defp export_info([]) do
    :ok
  end

  defp export_info(_Imported) do
    :ok
  end

  defp get_all_importfiles([{_M, _F, importFiles} | imported], acc) do
    newAcc = do_get_all_importfiles(importFiles, acc)
    get_all_importfiles(imported, newAcc)
  end

  defp get_all_importfiles([], acc) do
    acc
  end

  defp do_get_all_importfiles([importFile | importFiles], acc) do
    case :lists.member(importFile, acc) do
      true ->
        do_get_all_importfiles(importFiles, acc)

      false ->
        do_get_all_importfiles(importFiles, [importFile | acc])
    end
  end

  defp do_get_all_importfiles([], acc) do
    acc
  end

  defp imported_info(text, module, imported) do
    case :lists.keysearch(module, 1, imported) do
      {:value, {^module, _File, importFiles}} ->
        :io.format('~ts includes data from imported files\n~tp\n', [text, importFiles])

      false ->
        :ok
    end
  end

  defp add_imported(module, file, importFile, imported) do
    add_imported(module, file, :filename.absname(importFile), imported, [])
  end

  defp add_imported(m, f1, importFile, [{m, _F2, importFiles} | imported], acc) do
    case :lists.member(importFile, importFiles) do
      true ->
        :io.fwrite('WARNING: Module ~w already imported from ~tp~nNot importing again!~n', [
          m,
          importFile
        ])

        :dont_import

      false ->
        newEntry = {m, f1, [importFile | importFiles]}
        {:ok, :lists.reverse([newEntry | acc]) ++ imported}
    end
  end

  defp add_imported(m, f, importFile, [h | imported], acc) do
    add_imported(m, f, importFile, imported, [h | acc])
  end

  defp add_imported(m, f, importFile, [], acc) do
    {:ok, :lists.reverse([{m, f, [importFile]} | acc])}
  end

  defp remove_imported(module, imported) do
    case :lists.keysearch(module, 1, imported) do
      {:value, {^module, _, importFiles}} ->
        :io.fwrite('WARNING: Deleting data for module ~w imported from~n~tp~n', [
          module,
          importFiles
        ])

        :lists.keydelete(module, 1, imported)

      false ->
        imported
    end
  end

  defp add_compiled(module, file1, [{module, _File2} | compiled]) do
    [{module, file1} | compiled]
  end

  defp add_compiled(module, file, [h | compiled]) do
    [h | add_compiled(module, file, compiled)]
  end

  defp add_compiled(module, file, []) do
    [{module, file}]
  end

  defp are_loaded([module | modules], state, loaded, imported, error) do
    try do
      is_loaded(module, state)
    catch
      _ ->
        are_loaded(modules, state, loaded, imported, [{:not_cover_compiled, module} | error])
    else
      {:loaded, file} ->
        are_loaded(modules, state, [{module, file} | loaded], imported, error)

      {:imported, file, _} ->
        are_loaded(modules, state, loaded, [{module, file} | imported], error)
    end
  end

  defp are_loaded([], _State, loaded, imported, error) do
    {loaded, imported, error}
  end

  defp is_loaded(module, state) do
    case get_file(module, r_main_state(state, :compiled)) do
      {:ok, file} ->
        case :code.which(module) do
          :cover_compiled ->
            {:loaded, file}

          _ ->
            throw(:unloaded)
        end

      false ->
        case get_file(module, r_main_state(state, :imported)) do
          {:ok, file, importFiles} ->
            {:imported, file, importFiles}

          false ->
            throw(:not_loaded)
        end
    end
  end

  defp get_file(module, [{module, file} | _T]) do
    {:ok, file}
  end

  defp get_file(module, [{module, file, importFiles} | _T]) do
    {:ok, file, importFiles}
  end

  defp get_file(module, [_H | t]) do
    get_file(module, t)
  end

  defp get_file(_Module, []) do
    false
  end

  defp get_beam_file(module, :cover_compiled, compiled) do
    {:value, {^module, file}} = :lists.keysearch(module, 1, compiled)

    case :filename.extension(file) do
      '.erl' ->
        {:error, :no_beam}

      '.beam' ->
        {:ok, file}
    end
  end

  defp get_beam_file(_Module, beamFile, _Compiled) do
    {:ok, beamFile}
  end

  defp get_modules(compiled) do
    :lists.map(
      fn {module, _File} ->
        module
      end,
      compiled
    )
  end

  defp update_compiled(
         [module | modules],
         [{module, _File} | compiled]
       ) do
    update_compiled(modules, compiled)
  end

  defp update_compiled(modules, [h | compiled]) do
    [h | update_compiled(modules, compiled)]
  end

  defp update_compiled(_Modules, []) do
    []
  end

  defp get_compiled_still_loaded(nodes, compiled0) do
    compiledModules = get_modules(compiled0)

    loadedModules =
      :lists.filter(
        fn module ->
          case :code.which(module) do
            :cover_compiled ->
              true

            _ ->
              false
          end
        end,
        compiledModules
      )

    unloadedModules = compiledModules -- loadedModules

    compiled =
      case unloadedModules do
        [] ->
          compiled0

        _ ->
          :lists.foreach(
            fn module ->
              do_clear(module)
            end,
            unloadedModules
          )

          remote_unload(nodes, unloadedModules)
          update_compiled(unloadedModules, compiled0)
      end

    {loadedModules, compiled}
  end

  defp do_compile_beams(modsAndFiles, state) do
    result0 =
      pmap(
        fn
          {:ok, module, file} ->
            do_compile_beam(module, file, state)

          error ->
            error
        end,
        modsAndFiles
      )

    compiled =
      for {:ok, m, f} <- result0 do
        {m, f}
      end

    remote_load_compiled(r_main_state(state, :nodes), compiled)
    fix_state_and_result(result0, state, [])
  end

  defp do_compile_beam(module, beamFile0, state) do
    case get_beam_file(module, beamFile0, r_main_state(state, :compiled)) do
      {:ok, beamFile} ->
        localOnly = r_main_state(state, :local_only)
        userOptions = get_compile_options(module, beamFile)

        case do_compile_beam1(module, beamFile, userOptions, localOnly) do
          {:ok, ^module} ->
            {:ok, module, beamFile}

          :error ->
            {:error, beamFile}

          {:error, reason} ->
            {:error, {reason, beamFile}}
        end

      {:error, :no_beam} ->
        {:error, {:already_cover_compiled, :no_beam_found, module}}
    end
  end

  defp fix_state_and_result([{:ok, module, beamFile} | rest], state, acc) do
    compiled = add_compiled(module, beamFile, r_main_state(state, :compiled))
    imported = remove_imported(module, r_main_state(state, :imported))

    newState =
      r_main_state(state,
        compiled: compiled,
        imported: imported
      )

    fix_state_and_result(rest, newState, [{:ok, module} | acc])
  end

  defp fix_state_and_result([error | rest], state, acc) do
    fix_state_and_result(rest, state, [error | acc])
  end

  defp fix_state_and_result([], state, acc) do
    {:lists.reverse(acc), state}
  end

  defp do_compile(files, options, state) do
    localOnly = r_main_state(state, :local_only)

    result0 =
      pmap(
        fn file ->
          do_compile1(file, options, localOnly)
        end,
        files
      )

    compiled =
      for {:ok, m, f} <- result0 do
        {m, f}
      end

    remote_load_compiled(r_main_state(state, :nodes), compiled)
    fix_state_and_result(result0, state, [])
  end

  defp do_compile1(file, options, localOnly) do
    case do_compile2(file, options, localOnly) do
      {:ok, module} ->
        {:ok, module, file}

      :error ->
        {:error, file}
    end
  end

  defp do_compile2(file, userOptions, localOnly) do
    options = [:debug_info, :binary, :report_errors, :report_warnings] ++ userOptions

    case :compile.file(file, options) do
      {:ok, module, binary} ->
        do_compile_beam1(module, binary, userOptions, localOnly)

      :error ->
        :error
    end
  end

  defp do_compile_beam1(module, beam, userOptions, localOnly) do
    do_clear(module)

    case get_abstract_code(module, beam) do
      :no_abstract_code = e ->
        {:error, e}

      :encrypted_abstract_code = e ->
        {:error, e}

      {:raw_abstract_v1, code} ->
        forms0 = :epp.interpret_file_attribute(code)

        case find_main_filename(forms0) do
          {:ok, mainFile} ->
            do_compile_beam2(module, beam, userOptions, forms0, mainFile, localOnly)

          error ->
            error
        end

      {_VSN, _Code} ->
        {:error, :no_abstract_code}
    end
  end

  defp get_abstract_code(module, beam) do
    case :beam_lib.chunks(beam, [:abstract_code]) do
      {:ok, {^module, [{:abstract_code, abstractCode}]}} ->
        abstractCode

      {:error, :beam_lib, {:key_missing_or_invalid, _, _}} ->
        :encrypted_abstract_code

      error ->
        error
    end
  end

  defp do_compile_beam2(module, beam, userOptions, forms0, mainFile, localOnly) do
    init_counter_mapping(module)
    {forms, vars} = transform(forms0, module, mainFile, localOnly)
    maybe_create_counters(module, not localOnly)
    sourceInfo = get_source_info(module, beam)
    options = sourceInfo ++ userOptions
    {:ok, ^module, binary} = :compile.forms(forms, options)

    case :code.load_binary(module, :cover_compiled, binary) do
      {:module, ^module} ->
        initInfo = :lists.reverse(r_vars(vars, :init_info))

        :ets.insert(
          :cover_internal_clause_table,
          {module, initInfo}
        )

        :ets.insert(:cover_binary_code_table, {module, binary})
        {:ok, module}

      _Error ->
        do_clear(module)
        :error
    end
  end

  defp get_source_info(module, beam) do
    compile = get_compile_info(module, beam)

    case :lists.keyfind(:source, 1, compile) do
      {:source, _} = tuple ->
        [tuple]

      false ->
        []
    end
  end

  defp get_compile_options(module, beam) do
    compile = get_compile_info(module, beam)

    case :lists.keyfind(:options, 1, compile) do
      {:options, options} ->
        filter_options(options)

      false ->
        []
    end
  end

  defp get_compile_info(module, beam) do
    case :beam_lib.chunks(beam, [:compile_info]) do
      {:ok, {^module, [{:compile_info, compile}]}} ->
        compile

      _ ->
        []
    end
  end

  defp transform(code, module, mainFile, localOnly) do
    vars0 = r_vars(module: module)
    {:ok, mungedForms0, vars} = transform_2(code, [], vars0, mainFile, :on)
    mungedForms = patch_code(module, mungedForms0, localOnly)
    {mungedForms, vars}
  end

  defp find_main_filename([{:attribute, _, :file, {mainFile, _}} | _]) do
    {:ok, mainFile}
  end

  defp find_main_filename([_ | rest]) do
    find_main_filename(rest)
  end

  defp find_main_filename([]) do
    {:error, :no_file_attribute}
  end

  defp transform_2([form0 | forms], mungedForms, vars, mainFile, switch) do
    form = expand(form0)

    case munge(form, vars, mainFile, switch) do
      :ignore ->
        transform_2(forms, mungedForms, vars, mainFile, switch)

      {mungedForm, vars2, newSwitch} ->
        transform_2(forms, [mungedForm | mungedForms], vars2, mainFile, newSwitch)
    end
  end

  defp transform_2([], mungedForms, vars, _, _) do
    {:ok, :lists.reverse(mungedForms), vars}
  end

  defp expand(expr) do
    allVars =
      :sets.from_list(
        :ordsets.to_list(
          vars(
            [],
            expr
          )
        )
      )

    {expr1, _} = expand(expr, allVars, 1)
    expr1
  end

  defp expand({:clause, line, pattern, guards, body}, vs, n) do
    {expandedBody, n2} = expand(body, vs, n)
    {{:clause, line, pattern, guards, expandedBody}, n2}
  end

  defp expand({:op, _Line, :andalso, exprL, exprR}, vs, n) do
    {expandedExprL, n2} = expand(exprL, vs, n)
    {expandedExprR, n3} = expand(exprR, vs, n2)
    anno = :erlang.element(2, expandedExprL)
    {bool_switch(expandedExprL, expandedExprR, {:atom, anno, false}, vs, n3), n3 + 1}
  end

  defp expand({:op, _Line, :orelse, exprL, exprR}, vs, n) do
    {expandedExprL, n2} = expand(exprL, vs, n)
    {expandedExprR, n3} = expand(exprR, vs, n2)
    anno = :erlang.element(2, expandedExprL)
    {bool_switch(expandedExprL, {:atom, anno, true}, expandedExprR, vs, n3), n3 + 1}
  end

  defp expand(t, vs, n) when is_tuple(t) do
    {tL, n2} = expand(:erlang.tuple_to_list(t), vs, n)
    {:erlang.list_to_tuple(tL), n2}
  end

  defp expand([e | es], vs, n) do
    {e2, n2} = expand(e, vs, n)
    {es2, n3} = expand(es, vs, n2)
    {[e2 | es2], n3}
  end

  defp expand(t, _Vs, n) do
    {t, n}
  end

  defp vars(a, {:var, _, v}) when v !== :_ do
    [v | a]
  end

  defp vars(a, t) when is_tuple(t) do
    vars(a, :erlang.tuple_to_list(t))
  end

  defp vars(a, [e | es]) do
    vars(vars(a, e), es)
  end

  defp vars(a, _T) do
    a
  end

  defp bool_switch(e, t, f, allVars, auxVarN) do
    line = :erlang.element(2, e)
    auxVar = {:var, line, aux_var(allVars, auxVarN)}

    {:case, line, e,
     [
       {:clause, line, [{:atom, line, true}], [], [t]},
       {:clause, line, [{:atom, line, false}], [], [f]},
       {:clause, :erl_anno.set_generated(true, line), [auxVar], [],
        [
          {:call, line, {:remote, line, {:atom, line, :erlang}, {:atom, line, :error}},
           [{:tuple, line, [{:atom, line, :badarg}, auxVar]}]}
        ]}
     ]}
  end

  defp aux_var(vars, n) do
    name = :erlang.list_to_atom(:lists.concat([:_, n]))

    case :sets.is_element(name, vars) do
      true ->
        aux_var(vars, n + 1)

      false ->
        name
    end
  end

  defp munge({:function, line, function, arity, clauses}, vars, _MainFile, :on) do
    vars2 =
      r_vars(vars,
        function: function,
        arity: arity,
        clause: 1,
        lines: [],
        no_bump_lines: [],
        depth: 1
      )

    {mungedClauses, vars3} = munge_clauses(clauses, vars2)
    {{:function, line, function, arity, mungedClauses}, vars3, :on}
  end

  defp munge(form = {:attribute, _, :file, {mainFile, _}}, vars, mainFile, _Switch) do
    {form, vars, :on}
  end

  defp munge(form = {:attribute, _, :file, {_InclFile, _}}, vars, _MainFile, _Switch) do
    {form, vars, :off}
  end

  defp munge({:attribute, _, :compile, {:parse_transform, _}}, _Vars, _MainFile, _Switch) do
    :ignore
  end

  defp munge(form, vars, _MainFile, switch) do
    {form, vars, switch}
  end

  defp munge_clauses(clauses, vars) do
    munge_clauses(clauses, vars, r_vars(vars, :lines), [])
  end

  defp munge_clauses([clause | clauses], vars, lines, mClauses) do
    {:clause, line, pattern, guards, body} = clause
    {mungedGuards, _Vars} = munge_exprs(guards, r_vars(vars, is_guard: true), [])

    case r_vars(vars, :depth) do
      1 ->
        {mungedBody, vars2} =
          munge_body(
            body,
            r_vars(vars, depth: 2)
          )

        clauseInfo =
          {r_vars(vars2, :module), r_vars(vars2, :function), r_vars(vars2, :arity),
           r_vars(vars2, :clause), length(r_vars(vars2, :lines))}

        initInfo = [clauseInfo | r_vars(vars2, :init_info)]

        vars3 =
          r_vars(vars2,
            init_info: initInfo,
            clause: r_vars(vars2, :clause) + 1,
            lines: [],
            no_bump_lines: [],
            depth: 1
          )

        newBumps = r_vars(vars2, :lines)
        newLines = newBumps ++ lines

        munge_clauses(clauses, vars3, newLines, [
          {:clause, line, pattern, mungedGuards, mungedBody}
          | mClauses
        ])

      2 ->
        lines0 = r_vars(vars, :lines)
        {mungedBody, vars2} = munge_body(body, vars)
        newBumps = new_bumps(vars2, vars)
        newLines = newBumps ++ lines

        munge_clauses(clauses, r_vars(vars2, lines: lines0), newLines, [
          {:clause, line, pattern, mungedGuards, mungedBody}
          | mClauses
        ])
    end
  end

  defp munge_clauses([], vars, lines, mungedClauses) do
    {:lists.reverse(mungedClauses), r_vars(vars, lines: lines)}
  end

  defp munge_body(expr, vars) do
    munge_body(expr, vars, [], [])
  end

  defp munge_body([expr | body], vars, mungedBody, lastExprBumpLines) do
    line = :erl_anno.line(:erlang.element(2, expr))
    lines = r_vars(vars, :lines)

    case :lists.member(line, lines) do
      true ->
        {mungedExpr, vars2} = munge_expr(expr, vars)
        newBumps = new_bumps(vars2, vars)
        noBumpLines = [line | r_vars(vars, :no_bump_lines)]
        vars3 = r_vars(vars2, no_bump_lines: noBumpLines)
        mungedBody1 = maybe_fix_last_expr(mungedBody, vars3, lastExprBumpLines)
        mungedExprs1 = [mungedExpr | mungedBody1]
        munge_body(body, vars3, mungedExprs1, newBumps)

      false ->
        bump = bump_call(vars, line)
        lines2 = [line | lines]

        {mungedExpr, vars2} =
          munge_expr(
            expr,
            r_vars(vars, lines: lines2)
          )

        newBumps = new_bumps(vars2, vars)

        noBumpLines =
          subtract(
            r_vars(vars2, :no_bump_lines),
            newBumps
          )

        vars3 = r_vars(vars2, no_bump_lines: noBumpLines)
        mungedBody1 = maybe_fix_last_expr(mungedBody, vars3, lastExprBumpLines)
        mungedExprs1 = [mungedExpr, bump | mungedBody1]
        munge_body(body, vars3, mungedExprs1, newBumps)
    end
  end

  defp munge_body([], vars, mungedBody, _LastExprBumpLines) do
    {:lists.reverse(mungedBody), vars}
  end

  defp maybe_fix_last_expr(mungedExprs, vars, lastExprBumpLines) do
    case last_expr_needs_fixing(
           vars,
           lastExprBumpLines
         ) do
      {:yes, line} ->
        fix_last_expr(mungedExprs, line, vars)

      :no ->
        mungedExprs
    end
  end

  defp last_expr_needs_fixing(vars, lastExprBumpLines) do
    case common_elems(
           r_vars(vars, :no_bump_lines),
           lastExprBumpLines
         ) do
      [line] ->
        {:yes, line}

      _ ->
        :no
    end
  end

  defp fix_last_expr([mungedExpr | mungedExprs], line, vars) do
    bump = bump_call(vars, line)
    [fix_expr(mungedExpr, line, bump) | mungedExprs]
  end

  defp fix_expr({:if, l, clauses}, line, bump) do
    fixedClauses = fix_clauses(clauses, line, bump)
    {:if, l, fixedClauses}
  end

  defp fix_expr({:case, l, expr, clauses}, line, bump) do
    fixedExpr = fix_expr(expr, line, bump)
    fixedClauses = fix_clauses(clauses, line, bump)
    {:case, l, fixedExpr, fixedClauses}
  end

  defp fix_expr({:receive, l, clauses}, line, bump) do
    fixedClauses = fix_clauses(clauses, line, bump)
    {:receive, l, fixedClauses}
  end

  defp fix_expr({:receive, l, clauses, expr, body}, line, bump) do
    fixedClauses = fix_clauses(clauses, line, bump)
    fixedExpr = fix_expr(expr, line, bump)
    fixedBody = fix_expr(body, line, bump)
    {:receive, l, fixedClauses, fixedExpr, fixedBody}
  end

  defp fix_expr({:try, l, exprs, clauses, catchClauses, after__}, line, bump) do
    fixedExprs = fix_expr(exprs, line, bump)
    fixedClauses = fix_clauses(clauses, line, bump)
    fixedCatchClauses = fix_clauses(catchClauses, line, bump)
    fixedAfter = fix_expr(after__, line, bump)
    {:try, l, fixedExprs, fixedClauses, fixedCatchClauses, fixedAfter}
  end

  defp fix_expr([e | es], line, bump) do
    [fix_expr(e, line, bump) | fix_expr(es, line, bump)]
  end

  defp fix_expr(t, line, bump) when is_tuple(t) do
    :erlang.list_to_tuple(fix_expr(:erlang.tuple_to_list(t), line, bump))
  end

  defp fix_expr(e, _Line, _Bump) do
    e
  end

  defp fix_clauses([], _Line, _Bump) do
    []
  end

  defp fix_clauses(cs, line, bump) do
    case bumps_line(:lists.last(cs), line) do
      true ->
        fix_cls(cs, line, bump)

      false ->
        cs
    end
  end

  defp fix_cls([], _Line, _Bump) do
    []
  end

  defp fix_cls([cl | cls], line, bump) do
    case bumps_line(cl, line) do
      true ->
        for c <- [cl | cls] do
          fix_expr(c, line, bump)
        end

      false ->
        {:clause, cL, p, g, body} = cl
        uniqueVarName = :erlang.list_to_atom(:lists.concat(['$cover$ ', line]))
        a = :erl_anno.new(0)
        v = {:var, a, uniqueVarName}
        [last | rest] = :lists.reverse(body)

        body1 =
          :lists.reverse(
            rest,
            [{:match, a, v, last}, bump, v]
          )

        [{:clause, cL, p, g, body1} | fix_cls(cls, line, bump)]
    end
  end

  defp bumps_line(e, l) do
    try do
      bumps_line1(e, l)
    catch
      true ->
        true
    end
  end

  defp bumps_line1({:BUMP, line, _}, line) do
    throw(true)
  end

  defp bumps_line1([e | es], line) do
    bumps_line1(e, line)
    bumps_line1(es, line)
  end

  defp bumps_line1(t, line) when is_tuple(t) do
    bumps_line1(:erlang.tuple_to_list(t), line)
  end

  defp bumps_line1(_, _) do
    false
  end

  defp bump_call(vars, line) do
    {:BUMP, line, counter_index(vars, line)}
  end

  defp munge_expr({:match, line, exprL, exprR}, vars) do
    {mungedExprL, vars2} = munge_expr(exprL, vars)
    {mungedExprR, vars3} = munge_expr(exprR, vars2)
    {{:match, line, mungedExprL, mungedExprR}, vars3}
  end

  defp munge_expr({:tuple, line, exprs}, vars) do
    {mungedExprs, vars2} = munge_exprs(exprs, vars, [])
    {{:tuple, line, mungedExprs}, vars2}
  end

  defp munge_expr({:record, line, name, exprs}, vars) do
    {mungedExprFields, vars2} = munge_exprs(exprs, vars, [])
    {{:record, line, name, mungedExprFields}, vars2}
  end

  defp munge_expr({:record, line, arg, name, exprs}, vars) do
    {mungedArg, vars2} = munge_expr(arg, vars)
    {mungedExprFields, vars3} = munge_exprs(exprs, vars2, [])
    {{:record, line, mungedArg, name, mungedExprFields}, vars3}
  end

  defp munge_expr({:record_field, line, exprL, exprR}, vars) do
    {mungedExprR, vars2} = munge_expr(exprR, vars)
    {{:record_field, line, exprL, mungedExprR}, vars2}
  end

  defp munge_expr({:map, line, fields}, vars) do
    {mungedFields, vars2} = munge_exprs(fields, vars, [])
    {{:map, line, mungedFields}, vars2}
  end

  defp munge_expr({:map, line, arg, fields}, vars) do
    {mungedArg, vars2} = munge_expr(arg, vars)
    {mungedFields, vars3} = munge_exprs(fields, vars2, [])
    {{:map, line, mungedArg, mungedFields}, vars3}
  end

  defp munge_expr({:map_field_assoc, line, name, value}, vars) do
    {mungedName, vars2} = munge_expr(name, vars)
    {mungedValue, vars3} = munge_expr(value, vars2)
    {{:map_field_assoc, line, mungedName, mungedValue}, vars3}
  end

  defp munge_expr({:map_field_exact, line, name, value}, vars) do
    {mungedName, vars2} = munge_expr(name, vars)
    {mungedValue, vars3} = munge_expr(value, vars2)
    {{:map_field_exact, line, mungedName, mungedValue}, vars3}
  end

  defp munge_expr({:cons, line, exprH, exprT}, vars) do
    {mungedExprH, vars2} = munge_expr(exprH, vars)
    {mungedExprT, vars3} = munge_expr(exprT, vars2)
    {{:cons, line, mungedExprH, mungedExprT}, vars3}
  end

  defp munge_expr({:op, line, op, exprL, exprR}, vars) do
    {mungedExprL, vars2} = munge_expr(exprL, vars)
    {mungedExprR, vars3} = munge_expr(exprR, vars2)
    {{:op, line, op, mungedExprL, mungedExprR}, vars3}
  end

  defp munge_expr({:op, line, op, expr}, vars) do
    {mungedExpr, vars2} = munge_expr(expr, vars)
    {{:op, line, op, mungedExpr}, vars2}
  end

  defp munge_expr({:catch, line, expr}, vars) do
    {mungedExpr, vars2} = munge_expr(expr, vars)
    {{:catch, line, mungedExpr}, vars2}
  end

  defp munge_expr(
         {:call, line1, {:remote, line2, exprM, exprF}, exprs},
         vars
       ) do
    {mungedExprM, vars2} = munge_expr(exprM, vars)
    {mungedExprF, vars3} = munge_expr(exprF, vars2)
    {mungedExprs, vars4} = munge_exprs(exprs, vars3, [])
    {{:call, line1, {:remote, line2, mungedExprM, mungedExprF}, mungedExprs}, vars4}
  end

  defp munge_expr({:call, line, expr, exprs}, vars) do
    {mungedExpr, vars2} = munge_expr(expr, vars)
    {mungedExprs, vars3} = munge_exprs(exprs, vars2, [])
    {{:call, line, mungedExpr, mungedExprs}, vars3}
  end

  defp munge_expr({:lc, line, expr, qs}, vars) do
    {mungedExpr, vars2} =
      munge_expr(
        cond do
          :erlang.element(1, expr) === :block ->
            expr

          true ->
            {:block, :erl_anno.new(0), [expr]}
        end,
        vars
      )

    {mungedQs, vars3} = munge_qualifiers(qs, vars2)
    {{:lc, line, mungedExpr, mungedQs}, vars3}
  end

  defp munge_expr({:bc, line, expr, qs}, vars) do
    {mungedExpr, vars2} =
      munge_expr(
        cond do
          :erlang.element(1, expr) === :block ->
            expr

          true ->
            {:block, :erl_anno.new(0), [expr]}
        end,
        vars
      )

    {mungedQs, vars3} = munge_qualifiers(qs, vars2)
    {{:bc, line, mungedExpr, mungedQs}, vars3}
  end

  defp munge_expr({:block, line, body}, vars) do
    {mungedBody, vars2} = munge_body(body, vars)
    {{:block, line, mungedBody}, vars2}
  end

  defp munge_expr({:if, line, clauses}, vars) do
    {mungedClauses, vars2} = munge_clauses(clauses, vars)
    {{:if, line, mungedClauses}, vars2}
  end

  defp munge_expr({:case, line, expr, clauses}, vars) do
    {mungedExpr, vars2} = munge_expr(expr, vars)
    {mungedClauses, vars3} = munge_clauses(clauses, vars2)
    {{:case, line, mungedExpr, mungedClauses}, vars3}
  end

  defp munge_expr({:receive, line, clauses}, vars) do
    {mungedClauses, vars2} = munge_clauses(clauses, vars)
    {{:receive, line, mungedClauses}, vars2}
  end

  defp munge_expr({:receive, line, clauses, expr, body}, vars) do
    {mungedExpr, vars1} = munge_expr(expr, vars)
    {mungedClauses, vars2} = munge_clauses(clauses, vars1)

    {mungedBody, vars3} =
      munge_body(
        body,
        r_vars(vars2, lines: r_vars(vars1, :lines))
      )

    vars4 =
      r_vars(vars3,
        lines:
          r_vars(vars2, :lines) ++
            new_bumps(
              vars3,
              vars2
            )
      )

    {{:receive, line, mungedClauses, mungedExpr, mungedBody}, vars4}
  end

  defp munge_expr(
         {:try, line, body, clauses, catchClauses, after__},
         vars
       ) do
    {mungedBody, vars1} = munge_body(body, vars)
    {mungedClauses, vars2} = munge_clauses(clauses, vars1)
    {mungedCatchClauses, vars3} = munge_clauses(catchClauses, vars2)
    {mungedAfter, vars4} = munge_body(after__, vars3)
    {{:try, line, mungedBody, mungedClauses, mungedCatchClauses, mungedAfter}, vars4}
  end

  defp munge_expr({:fun, line, {:clauses, clauses}}, vars) do
    {mungedClauses, vars2} = munge_clauses(clauses, vars)
    {{:fun, line, {:clauses, mungedClauses}}, vars2}
  end

  defp munge_expr({:named_fun, line, name, clauses}, vars) do
    {mungedClauses, vars2} = munge_clauses(clauses, vars)
    {{:named_fun, line, name, mungedClauses}, vars2}
  end

  defp munge_expr({:bin, line, binElements}, vars) do
    {mungedBinElements, vars2} = munge_exprs(binElements, vars, [])
    {{:bin, line, mungedBinElements}, vars2}
  end

  defp munge_expr(
         {:bin_element, line, value, size, typeSpecifierList},
         vars
       ) do
    {mungedValue, vars2} = munge_expr(value, vars)
    {mungedSize, vars3} = munge_expr(size, vars2)
    {{:bin_element, line, mungedValue, mungedSize, typeSpecifierList}, vars3}
  end

  defp munge_expr(form, vars) do
    {form, vars}
  end

  defp munge_exprs([expr | exprs], vars, mungedExprs)
       when r_vars(vars, :is_guard) === true and is_list(expr) do
    {mungedExpr, _Vars} = munge_exprs(expr, vars, [])
    munge_exprs(exprs, vars, [mungedExpr | mungedExprs])
  end

  defp munge_exprs([expr | exprs], vars, mungedExprs) do
    {mungedExpr, vars2} = munge_expr(expr, vars)
    munge_exprs(exprs, vars2, [mungedExpr | mungedExprs])
  end

  defp munge_exprs([], vars, mungedExprs) do
    {:lists.reverse(mungedExprs), vars}
  end

  defp munge_qualifiers(qualifiers, vars) do
    munge_qs(qualifiers, vars, [])
  end

  defp munge_qs([{:generate, line, pattern, expr} | qs], vars, mQs) do
    l = :erlang.element(2, expr)
    {mungedExpr, vars2} = munge_expr(expr, vars)
    munge_qs1(qs, l, {:generate, line, pattern, mungedExpr}, vars, vars2, mQs)
  end

  defp munge_qs([{:b_generate, line, pattern, expr} | qs], vars, mQs) do
    l = :erlang.element(2, expr)
    {mExpr, vars2} = munge_expr(expr, vars)
    munge_qs1(qs, l, {:b_generate, line, pattern, mExpr}, vars, vars2, mQs)
  end

  defp munge_qs([expr | qs], vars, mQs) do
    l = :erlang.element(2, expr)
    {mungedExpr, vars2} = munge_expr(expr, vars)
    munge_qs1(qs, l, mungedExpr, vars, vars2, mQs)
  end

  defp munge_qs([], vars, mQs) do
    {:lists.reverse(mQs), vars}
  end

  defp munge_qs1(qs, line, nQ, vars, vars2, mQs) do
    case new_bumps(vars2, vars) do
      [_] ->
        munge_qs(qs, vars2, [nQ | mQs])

      _ ->
        {mungedTrue, vars3} =
          munge_expr(
            {:block, :erl_anno.new(0), [{:atom, line, true}]},
            vars2
          )

        munge_qs(qs, vars3, [nQ, mungedTrue | mQs])
    end
  end

  defp new_bumps(r_vars(lines: new), r_vars(lines: old)) do
    subtract(new, old)
  end

  defp subtract(l1, l2) do
    for e <- l1, not :lists.member(e, l2) do
      e
    end
  end

  defp common_elems(l1, l2) do
    for e <- l1, :lists.member(e, l2) do
      e
    end
  end

  defp init_counter_mapping(mod) do
    true =
      :ets.insert_new(
        :cover_internal_mapping_table,
        {mod, 0}
      )

    :ok
  end

  defp counter_index(vars, line) do
    r_vars(module: mod, function: f, arity: a, clause: c) = vars
    key = r_bump(module: mod, function: f, arity: a, clause: c, line: line)

    case :ets.lookup(
           :cover_internal_mapping_table,
           key
         ) do
      [] ->
        index = :ets.update_counter(:cover_internal_mapping_table, mod, {2, 1})

        true =
          :ets.insert(
            :cover_internal_mapping_table,
            {key, index}
          )

        index

      [{^key, index}] ->
        index
    end
  end

  defp maybe_create_counters(mod, true) do
    cref = create_counters(mod)
    key = {:cover, mod}
    :persistent_term.put(key, cref)
    :ok
  end

  defp maybe_create_counters(_Mod, false) do
    :ok
  end

  defp create_counters(mod) do
    size0 = :ets.lookup_element(:cover_internal_mapping_table, mod, 2)
    size = max(1, size0)
    cref = :counters.new(size, [:write_concurrency])

    :ets.insert(
      :cover_internal_mapping_table,
      {{:counters, mod}, cref}
    )

    cref
  end

  defp patch_code(mod, forms, false) do
    a = :erl_anno.new(0)
    abstrKey = {:tuple, a, [{:atom, a, :cover}, {:atom, a, mod}]}
    patch_code1(forms, {:distributed, abstrKey})
  end

  defp patch_code(mod, forms, true) do
    cref = create_counters(mod)
    abstrCref = cid_to_abstract(cref)
    patch_code1(forms, {:local_only, abstrCref})
  end

  defp patch_code1(
         {:BUMP, _Line, index},
         {:distributed, abstrKey}
       ) do
    a = :erlang.element(2, abstrKey)
    getCref = {:call, a, {:remote, a, {:atom, a, :persistent_term}, {:atom, a, :get}}, [abstrKey]}

    {:call, a, {:remote, a, {:atom, a, :counters}, {:atom, a, :add}},
     [getCref, {:integer, a, index}, {:integer, a, 1}]}
  end

  defp patch_code1(
         {:BUMP, _Line, index},
         {:local_only, abstrCref}
       ) do
    a = :erlang.element(2, abstrCref)

    {:call, a, {:remote, a, {:atom, a, :counters}, {:atom, a, :add}},
     [abstrCref, {:integer, a, index}, {:integer, a, 1}]}
  end

  defp patch_code1({:clauses, cs}, key) do
    {:clauses,
     for el <- cs do
       patch_code1(el, key)
     end}
  end

  defp patch_code1([_ | _] = list, key) do
    for el <- list do
      patch_code1(el, key)
    end
  end

  defp patch_code1(tuple, key) when tuple_size(tuple) >= 3 do
    acc = [
      :erlang.element(2, tuple),
      :erlang.element(
        1,
        tuple
      )
    ]

    patch_code_tuple(3, tuple_size(tuple), tuple, key, acc)
  end

  defp patch_code1(other, _Key) do
    other
  end

  defp patch_code_tuple(i, size, tuple, key, acc) when i <= size do
    el = patch_code1(:erlang.element(i, tuple), key)
    patch_code_tuple(i + 1, size, tuple, key, [el | acc])
  end

  defp patch_code_tuple(_I, _Size, _Tuple, _Key, acc) do
    :erlang.list_to_tuple(:lists.reverse(acc))
  end

  defp cid_to_abstract(cref0) do
    a = :erl_anno.new(0)
    cref = :erlang.binary_to_term(:erlang.term_to_binary(cref0))
    {:write_concurrency, ref} = cref
    {:tuple, a, [{:atom, a, :write_concurrency}, {:integer, a, ref}]}
  end

  defp send_counters(mod, collectorPid) do
    process = fn chunk ->
      send_chunk(collectorPid, chunk)
    end

    move_counters(mod, process)
  end

  defp move_counters(mod) do
    move_counters(mod, &insert_in_collection_table/1)
  end

  defp move_counters(mod, process) do
    pattern = {r_bump(module: mod, _: :_), :_}
    matches = :ets.match_object(:cover_internal_mapping_table, pattern, 20000)
    cref = get_counters_ref(mod)
    move_counters1(matches, cref, process)
  end

  defp move_counters1({mappings, continuation}, cref, process) do
    move = fn {key, index} ->
      count = :counters.get(cref, index)
      :ok = :counters.sub(cref, index, count)
      {key, count}
    end

    process.(:lists.map(move, mappings))
    move_counters1(:ets.match_object(continuation), cref, process)
  end

  defp move_counters1(:"$end_of_table", _Cref, _Process) do
    :ok
  end

  defp counters_mapping_table(mod) do
    mapping = counters_mapping(mod)
    cref = get_counters_ref(mod)
    %{size: size} = :counters.info(cref)
    [{mod, size} | mapping]
  end

  defp get_counters_ref(mod) do
    :ets.lookup_element(:cover_internal_mapping_table, {:counters, mod}, 2)
  end

  defp counters_mapping(mod) do
    pattern = {r_bump(module: mod, _: :_), :_}

    :ets.match_object(
      :cover_internal_mapping_table,
      pattern
    )
  end

  defp clear_counters(mod) do
    _ = :persistent_term.erase({:cover, mod})
    :ets.delete(:cover_internal_mapping_table, mod)
    pattern = {r_bump(module: mod, _: :_), :_}

    _ =
      :ets.match_delete(
        :cover_internal_mapping_table,
        pattern
      )

    :ok
  end

  defp reset_counters(mod) do
    pattern = {r_bump(module: mod, _: :_), :"$1"}
    matchSpec = [{pattern, [], [:"$1"]}]
    matches = :ets.select(:cover_internal_mapping_table, matchSpec, 20000)
    cref = get_counters_ref(mod)
    reset_counters1(matches, cref)
  end

  defp reset_counters1({indices, continuation}, cref) do
    _ =
      for n <- indices do
        :counters.put(cref, n, 0)
      end

    reset_counters1(:ets.select(continuation), cref)
  end

  defp reset_counters1(:"$end_of_table", _Cref) do
    :ok
  end

  defp delete_all_counters() do
    _ =
      for {:cover, _} = key <- :persistent_term.get() do
        :persistent_term.erase(key)
      end

    :ok
  end

  defp collect(nodes) do
    allClauses = :ets.tab2list(:cover_internal_clause_table)

    mon1 =
      spawn_monitor(fn ->
        pmap(&move_modules/1, allClauses)
      end)

    mon2 =
      spawn_monitor(fn ->
        remote_collect(:_, nodes, false)
      end)

    get_downs([mon1, mon2])
  end

  defp collect(modules, nodes) do
    mS =
      for m <- modules do
        {{:"$1", :_}, [{:==, :"$1", m}], [:"$_"]}
      end

    clauses = :ets.select(:cover_internal_clause_table, mS)

    mon1 =
      spawn_monitor(fn ->
        pmap(&move_modules/1, clauses)
      end)

    mon2 =
      spawn_monitor(fn ->
        remote_collect(:_, nodes, false)
      end)

    get_downs([mon1, mon2])
  end

  defp collect(module, clauses, nodes) do
    move_modules({module, clauses})
    remote_collect([module], nodes, false)
  end

  defp move_modules({module, clauses}) do
    :ets.insert(
      :cover_collected_remote_clause_table,
      {module, clauses}
    )

    move_counters(module)
  end

  defp find_source(module, file0) do
    try do
      root = :filename.rootname(file0, '.beam')
      root == file0 and throw(file0)
      file = root ++ '.erl'
      throw_file(file)
      beamDir = :filename.dirname(file)
      base = :filename.basename(file)
      throw_file(:filename.join([beamDir, '..', 'src', base]))

      info =
        try do
          :lists.keyfind(:source, 1, module.module_info(:compile))
        catch
          :error, :undef ->
            throw({:beam, file0})
        end

      false == info and throw({:beam, file0})
      {:source, srcFile} = info
      throw_file(splice(beamDir, srcFile))
      throw_file(srcFile)
      {:beam, file0}
    catch
      path ->
        path
    end
  end

  defp throw_file(path) do
    false != path and :filelib.is_file(path) and throw(path)
  end

  defp splice(beamDir, srcFile) do
    case :lists.splitwith(
           fn c ->
             c != 'src'
           end,
           revsplit(srcFile)
         ) do
      {t, [_ | _]} ->
        :filename.join([beamDir, '..', 'src' | :lists.reverse(t)])

      {_, []} ->
        false
    end
  end

  defp revsplit(path) do
    :lists.reverse(:filename.split(path))
  end

  defp analyse_list(modules, analysis, level, state) do
    {loadedMF, importedMF, error} = are_loaded(modules, state, [], [], [])

    loaded =
      for {m, _} <- loadedMF do
        m
      end

    imported =
      for {m, _} <- importedMF do
        m
      end

    collect(loaded, r_main_state(state, :nodes))

    mS =
      for m <- loaded ++ imported do
        {{:"$1", :_}, [{:==, :"$1", m}], [:"$_"]}
      end

    allClauses =
      :ets.select(
        :cover_collected_remote_clause_table,
        mS
      )

    fun = fn {module, clauses} ->
      do_analyse(module, analysis, level, clauses)
    end

    {:result, :lists.flatten(pmap(fun, allClauses)), error}
  end

  defp analyse_all(analysis, level, state) do
    collect(r_main_state(state, :nodes))
    allClauses = :ets.tab2list(:cover_collected_remote_clause_table)

    fun = fn {module, clauses} ->
      do_analyse(module, analysis, level, clauses)
    end

    {:result, :lists.flatten(pmap(fun, allClauses)), []}
  end

  defp do_parallel_analysis(module, analysis, level, loaded, from, state) do
    analyse_info(module, r_main_state(state, :imported))

    c =
      case loaded do
        {:loaded, _File} ->
          [{^module, clauses}] =
            :ets.lookup(
              :cover_internal_clause_table,
              module
            )

          collect(module, clauses, r_main_state(state, :nodes))
          clauses

        _ ->
          [{^module, clauses}] =
            :ets.lookup(
              :cover_collected_remote_clause_table,
              module
            )

          clauses
      end

    r = do_analyse(module, analysis, level, c)
    reply(from, {:ok, r})
  end

  defp do_analyse(module, analysis, :line, _Clauses) do
    pattern = {r_bump(module: module), :_}

    bumps =
      :ets.match_object(
        :cover_collected_remote_data_table,
        pattern
      )

    fun =
      case analysis do
        :coverage ->
          fn
            {r_bump(line: l), 0} ->
              {{module, l}, {0, 1}}

            {r_bump(line: l), _N} ->
              {{module, l}, {1, 0}}
          end

        :calls ->
          fn {r_bump(line: l), n} ->
            {{module, l}, n}
          end
      end

    :lists.keysort(1, :lists.map(fun, bumps))
  end

  defp do_analyse(module, analysis, :clause, _Clauses) do
    pattern = {r_bump(module: module), :_}

    bumps =
      :lists.keysort(
        1,
        :ets.match_object(
          :cover_collected_remote_data_table,
          pattern
        )
      )

    analyse_clause(analysis, bumps)
  end

  defp do_analyse(module, analysis, :function, clauses) do
    clauseResult = do_analyse(module, analysis, :clause, clauses)
    merge_clauses(clauseResult, merge_fun(analysis))
  end

  defp do_analyse(module, analysis, :module, clauses) do
    functionResult = do_analyse(module, analysis, :function, clauses)

    result =
      merge_functions(
        functionResult,
        merge_fun(analysis)
      )

    {module, result}
  end

  defp analyse_clause(_, []) do
    []
  end

  defp analyse_clause(
         :coverage,
         [
           {r_bump(module: m, function: f, arity: a, clause: c), _}
           | _
         ] = bumps
       ) do
    analyse_clause_cov(bumps, {m, f, a, c}, 0, 0, [])
  end

  defp analyse_clause(:calls, bumps) do
    analyse_clause_calls(bumps, {:x, :x, :x, :x}, [])
  end

  defp analyse_clause_cov(
         [
           {r_bump(module: m, function: f, arity: a, clause: c), n}
           | bumps
         ],
         {m, f, a, c} = clause,
         ls,
         notCov,
         acc
       ) do
    analyse_clause_cov(
      bumps,
      clause,
      ls + 1,
      cond do
        n == 0 ->
          notCov + 1

        true ->
          notCov
      end,
      acc
    )
  end

  defp analyse_clause_cov(
         [
           {r_bump(module: m1, function: f1, arity: a1, clause: c1), _}
           | _
         ] = bumps,
         clause,
         ls,
         notCov,
         acc
       ) do
    analyse_clause_cov(bumps, {m1, f1, a1, c1}, 0, 0, [{clause, {ls - notCov, notCov}} | acc])
  end

  defp analyse_clause_cov([], clause, ls, notCov, acc) do
    :lists.reverse(acc, [{clause, {ls - notCov, notCov}}])
  end

  defp analyse_clause_calls(
         [
           {r_bump(module: m, function: f, arity: a, clause: c), _}
           | bumps
         ],
         {m, f, a, c} = clause,
         acc
       ) do
    analyse_clause_calls(bumps, clause, acc)
  end

  defp analyse_clause_calls(
         [
           {r_bump(module: m1, function: f1, arity: a1, clause: c1), n}
           | bumps
         ],
         _Clause,
         acc
       ) do
    analyse_clause_calls(bumps, {m1, f1, a1, c1}, [{{m1, f1, a1, c1}, n} | acc])
  end

  defp analyse_clause_calls([], _Clause, acc) do
    :lists.reverse(acc)
  end

  defp merge_fun(:coverage) do
    fn {cov1, notCov1}, {cov2, notCov2} ->
      {cov1 + cov2, notCov1 + notCov2}
    end
  end

  defp merge_fun(:calls) do
    fn calls1, calls2 ->
      calls1 + calls2
    end
  end

  defp merge_clauses(clauses, mFun) do
    merge_clauses(clauses, mFun, [])
  end

  defp merge_clauses(
         [
           {{m, f, a, _C1}, r1},
           {{m, f, a, c2}, r2}
           | clauses
         ],
         mFun,
         result
       ) do
    merge_clauses(
      [
        {{m, f, a, c2}, mFun.(r1, r2)}
        | clauses
      ],
      mFun,
      result
    )
  end

  defp merge_clauses([{{m, f, a, _C}, r} | clauses], mFun, result) do
    merge_clauses(clauses, mFun, [{{m, f, a}, r} | result])
  end

  defp merge_clauses([], _Fun, result) do
    :lists.reverse(result)
  end

  defp merge_functions([{_MFA, r} | functions], mFun) do
    merge_functions(functions, mFun, r)
  end

  defp merge_functions([], _MFun) do
    {0, 0}
  end

  defp merge_functions([{_MFA, r} | functions], mFun, result) do
    merge_functions(functions, mFun, mFun.(result, r))
  end

  defp merge_functions([], _MFun, result) do
    result
  end

  defp analyse_list_to_file(modules, opts, state) do
    {loadedMF, importedMF, error} = are_loaded(modules, state, [], [], [])

    collect(
      for {m, _} <- loadedMF do
        m
      end,
      r_main_state(state, :nodes)
    )

    outDir = :proplists.get_value(:outdir, opts)
    hTML = :lists.member(:html, opts)

    fun = fn {module, file} ->
      outFile = outfilename(outDir, module, hTML)
      do_analyse_to_file(module, file, outFile, hTML, state)
    end

    {ok, error1} =
      split_ok_error(
        pmap(
          fun,
          loadedMF ++ importedMF
        ),
        [],
        []
      )

    {:result, ok, error ++ error1}
  end

  defp analyse_all_to_file(opts, state) do
    collect(r_main_state(state, :nodes))
    allModules = get_all_modules(state)
    outDir = :proplists.get_value(:outdir, opts)
    hTML = :lists.member(:html, opts)

    fun = fn {module, file} ->
      outFile = outfilename(outDir, module, hTML)
      do_analyse_to_file(module, file, outFile, hTML, state)
    end

    {ok, error} = split_ok_error(pmap(fun, allModules), [], [])
    {:result, ok, error}
  end

  defp get_all_modules(state) do
    get_all_modules(
      r_main_state(state, :compiled) ++ r_main_state(state, :imported),
      []
    )
  end

  defp get_all_modules([{module, file} | rest], acc) do
    get_all_modules(rest, [{module, file} | acc])
  end

  defp get_all_modules([{module, file, _} | rest], acc) do
    case :lists.keymember(module, 1, acc) do
      true ->
        get_all_modules(rest, acc)

      false ->
        get_all_modules(rest, [{module, file} | acc])
    end
  end

  defp get_all_modules([], acc) do
    acc
  end

  defp split_ok_error([{:ok, r} | result], ok, error) do
    split_ok_error(result, [r | ok], error)
  end

  defp split_ok_error([{:error, r} | result], ok, error) do
    split_ok_error(result, ok, [r | error])
  end

  defp split_ok_error([], ok, error) do
    {ok, error}
  end

  defp do_parallel_analysis_to_file(module, opts, loaded, from, state) do
    file =
      case loaded do
        {:loaded, file0} ->
          [{^module, clauses}] =
            :ets.lookup(
              :cover_internal_clause_table,
              module
            )

          collect(module, clauses, r_main_state(state, :nodes))
          file0

        {:imported, file0, _} ->
          file0
      end

    hTML = :lists.member(:html, opts)

    outFile =
      case :proplists.get_value(
             :outfile,
             opts
           ) do
        :undefined ->
          outfilename(:proplists.get_value(:outdir, opts), module, hTML)

        f ->
          f
      end

    reply(
      from,
      do_analyse_to_file(module, file, outFile, hTML, state)
    )
  end

  defp do_analyse_to_file(module, file, outFile, hTML, state) do
    case find_source(module, file) do
      {:beam, _BeamFile} ->
        {:error, {:no_source_code_found, module}}

      erlFile ->
        analyse_info(module, r_main_state(state, :imported))
        do_analyse_to_file1(module, outFile, erlFile, hTML)
    end
  end

  defp do_analyse_to_file1(module, outFile, erlFile, hTML) do
    case :file.open(
           erlFile,
           [:read, :raw, :read_ahead]
         ) do
      {:ok, inFd} ->
        case :file.open(
               outFile,
               [:write, :raw, :delayed_write]
             ) do
          {:ok, outFd} ->
            enc = encoding(erlFile)

            cond do
              hTML ->
                header = create_header(outFile, enc)
                h1Bin = :unicode.characters_to_binary(header, enc, enc)
                :ok = :file.write(outFd, h1Bin)

              true ->
                :ok
            end

            {{y, mo, d}, {h, mi, s}} = :calendar.local_time()

            timestamp =
              :io_lib.format(
                '~p-~s-~s at ~s:~s:~s',
                [
                  y,
                  :string.pad(:erlang.integer_to_list(mo), 2, :leading, ?0),
                  :string.pad(:erlang.integer_to_list(d), 2, :leading, ?0),
                  :string.pad(:erlang.integer_to_list(h), 2, :leading, ?0),
                  :string.pad(
                    :erlang.integer_to_list(mi),
                    2,
                    :leading,
                    ?0
                  ),
                  :string.pad(
                    :erlang.integer_to_list(s),
                    2,
                    :leading,
                    ?0
                  )
                ]
              )

            outFileInfo =
              cond do
                hTML ->
                  create_footer(erlFile, timestamp)

                true ->
                  [
                    'File generated from ',
                    erlFile,
                    ' by COVER ',
                    timestamp,
                    '\n\n',
                    '****************************************************************************\n\n'
                  ]
              end

            h2Bin = :unicode.characters_to_binary(outFileInfo, enc, enc)
            :ok = :file.write(outFd, h2Bin)
            pattern = {r_bump(module: module, line: :"$1", _: :_), :"$2"}
            mS = [{pattern, [{:is_integer, :"$1"}, {:>, :"$1", 0}], [{{:"$1", :"$2"}}]}]

            covLines0 =
              :lists.keysort(
                1,
                :ets.select(
                  :cover_collected_remote_data_table,
                  mS
                )
              )

            covLines = merge_dup_lines(covLines0)
            print_lines(module, covLines, inFd, outFd, 1, hTML)

            cond do
              hTML ->
                :ok = :file.write(outFd, close_html())

              true ->
                :ok
            end

            :ok = :file.close(outFd)
            :ok = :file.close(inFd)
            {:ok, outFile}

          {:error, reason} ->
            {:error, {:file, outFile, reason}}
        end

      {:error, reason} ->
        {:error, {:file, erlFile, reason}}
    end
  end

  defp merge_dup_lines(covLines) do
    merge_dup_lines(covLines, [])
  end

  defp merge_dup_lines([{l, n} | t], [{l, nAcc} | tAcc]) do
    merge_dup_lines(t, [{l, nAcc + n} | tAcc])
  end

  defp merge_dup_lines([{l, n} | t], acc) do
    merge_dup_lines(t, [{l, n} | acc])
  end

  defp merge_dup_lines([], acc) do
    :lists.reverse(acc)
  end

  defp print_lines(module, covLines, inFd, outFd, l, hTML) do
    case :file.read_line(inFd) do
      :eof ->
        :ignore

      {:ok, rawLine} ->
        line = escape_lt_and_gt(rawLine, hTML)

        case covLines do
          [{^l, n} | covLines1] ->
            cond do
              n === 0 and hTML === true ->
                missedLine = table_row('miss', line, l, n)
                :ok = :file.write(outFd, missedLine)

              hTML === true ->
                hitLine = table_row('hit', line, l, n)
                :ok = :file.write(outFd, hitLine)

              n < 1_000_000 ->
                str = :string.pad(:erlang.integer_to_list(n), 6, :leading, ?\s)
                :ok = :file.write(outFd, [str, fill1(), line])

              n < 10_000_000 ->
                str = :erlang.integer_to_list(n)
                :ok = :file.write(outFd, [str, fill2(), line])

              true ->
                str = :erlang.integer_to_list(n)
                :ok = :file.write(outFd, [str, fill3(), line])
            end

            print_lines(module, covLines1, inFd, outFd, l + 1, hTML)

          _ ->
            nonCoveredContent =
              cond do
                hTML ->
                  table_row(line, l)

                true ->
                  [tab(), line]
              end

            :ok = :file.write(outFd, nonCoveredContent)
            print_lines(module, covLines, inFd, outFd, l + 1, hTML)
        end
    end
  end

  defp tab() do
    '        |  '
  end

  defp fill1() do
    '..|  '
  end

  defp fill2() do
    '.|  '
  end

  defp fill3() do
    '|  '
  end

  defp create_header(outFile, enc) do
    [
      '<!doctype html>\n<html>\n<head>\n<meta charset="',
      html_encoding(enc),
      '">\n<title>',
      outFile,
      '</title>\n<style>'
    ] ++
      read_stylesheet() ++
      ['</style>\n', '</head>\n<body>\n<h1><code>', outFile, '</code></h1>\n']
  end

  defp create_footer(erlFile, timestamp) do
    [
      '<footer><p>File generated from <code>',
      erlFile,
      '</code> by <a href="http://erlang.org/doc/man/cover.html">cover</a> at ',
      timestamp,
      '</p></footer>\n<table>\n<tbody>\n'
    ]
  end

  defp close_html() do
    [
      '</tbody>\n',
      '<thead>\n',
      '<tr>\n',
      '<th>Line</th>\n',
      '<th>Hits</th>\n',
      '<th>Source</th>\n',
      '</tr>\n',
      '</thead>\n',
      '</table>\n',
      '</body>\n</html>\n'
    ]
  end

  defp table_row(cssClass, line, l, n) do
    ['<tr class="', cssClass, '">\n', table_data(line, l, n)]
  end

  defp table_row(line, l) do
    ['<tr>\n', table_data(line, l, '')]
  end

  defp table_data(line, l, n) do
    lineNoNL = line -- '\n'

    [
      '<td class="line" id="L',
      :erlang.integer_to_list(l),
      '">',
      '<a href="#L',
      :erlang.integer_to_list(l),
      '">',
      :erlang.integer_to_list(l),
      '</a></td>\n',
      '<td class="hits">',
      maybe_integer_to_list(n),
      '</td>\n',
      '<td class="source"><code>',
      lineNoNL,
      '</code></td>\n</tr>\n'
    ]
  end

  defp maybe_integer_to_list(0) do
    '<pre style="display: inline;">:-(</pre>'
  end

  defp maybe_integer_to_list(n) when is_integer(n) do
    :erlang.integer_to_list(n)
  end

  defp maybe_integer_to_list(_) do
    ''
  end

  defp read_stylesheet() do
    privDir = :code.priv_dir(:tools)
    {:ok, css} = :file.read_file(:filename.join(privDir, 'styles.css'))
    [css]
  end

  defp do_export(module, outFile, from, state) do
    case :file.open(
           outFile,
           [:write, :binary, :raw, :delayed_write]
         ) do
      {:ok, fd} ->
        reply =
          case module do
            :_ ->
              export_info(r_main_state(state, :imported))
              collect(r_main_state(state, :nodes))
              do_export_table(r_main_state(state, :compiled), r_main_state(state, :imported), fd)

            _ ->
              export_info(module, r_main_state(state, :imported))

              try do
                is_loaded(module, state)
              catch
                _ ->
                  {:error, {:not_cover_compiled, module}}
              else
                {:loaded, file} ->
                  [{^module, clauses}] =
                    :ets.lookup(
                      :cover_internal_clause_table,
                      module
                    )

                  collect(module, clauses, r_main_state(state, :nodes))
                  do_export_table([{module, file}], [], fd)

                {:imported, file, importFiles} ->
                  imported = [{module, file, importFiles}]
                  do_export_table([], imported, fd)
              end
          end

        :ok = :file.close(fd)
        reply(from, reply)

      {:error, reason} ->
        reply(
          from,
          {:error, {:cant_open_file, outFile, reason}}
        )
    end
  end

  defp do_export_table(compiled, imported, fd) do
    modList = merge(imported, compiled)
    write_module_data(modList, fd)
  end

  defp merge(
         [{module, file, _ImportFiles} | imported],
         moduleList
       ) do
    case :lists.keymember(module, 1, moduleList) do
      true ->
        merge(imported, moduleList)

      false ->
        merge(imported, [{module, file} | moduleList])
    end
  end

  defp merge([], moduleList) do
    moduleList
  end

  defp write_module_data([{module, file} | modList], fd) do
    write({:file, module, file}, fd)

    [clauses] =
      :ets.lookup(
        :cover_collected_remote_clause_table,
        module
      )

    write(clauses, fd)

    moduleData =
      :ets.match_object(
        :cover_collected_remote_data_table,
        {r_bump(module: module), :_}
      )

    do_write_module_data(moduleData, fd)
    write_module_data(modList, fd)
  end

  defp write_module_data([], _Fd) do
    :ok
  end

  defp do_write_module_data([h | t], fd) do
    write(h, fd)
    do_write_module_data(t, fd)
  end

  defp do_write_module_data([], _Fd) do
    :ok
  end

  defp write(element, fd) do
    bin = :erlang.term_to_binary(element, [:compressed])

    case byte_size(bin) do
      size when size > 255 ->
        sizeBin = :erlang.term_to_binary({:"$size", size})

        :ok =
          :file.write(
            fd,
            <<byte_size(sizeBin)::size(8), sizeBin::binary, bin::binary>>
          )

      size ->
        :ok =
          :file.write(
            fd,
            <<size::size(8), bin::binary>>
          )
    end

    :ok
  end

  defp do_import_to_table(fd, importFile, imported) do
    do_import_to_table(fd, importFile, imported, [])
  end

  defp do_import_to_table(fd, importFile, imported, dontImport) do
    case get_term(fd) do
      {:file, module, file} ->
        case add_imported(module, file, importFile, imported) do
          {:ok, newImported} ->
            do_import_to_table(fd, importFile, newImported, dontImport)

          :dont_import ->
            do_import_to_table(fd, importFile, imported, [module | dontImport])
        end

      {key = r_bump(module: module), val} ->
        case :lists.member(module, dontImport) do
          false ->
            insert_in_collection_table(key, val)

          true ->
            :ok
        end

        do_import_to_table(fd, importFile, imported, dontImport)

      {module, clauses} ->
        case :lists.member(module, dontImport) do
          false ->
            :ets.insert(
              :cover_collected_remote_clause_table,
              {module, clauses}
            )

          true ->
            :ok
        end

        do_import_to_table(fd, importFile, imported, dontImport)

      :eof ->
        imported
    end
  end

  defp get_term(fd) do
    case :file.read(fd, 1) do
      {:ok, <<size1::size(8)>>} ->
        {:ok, bin1} = :file.read(fd, size1)

        case :erlang.binary_to_term(bin1) do
          {:"$size", size2} ->
            {:ok, bin2} = :file.read(fd, size2)
            :erlang.binary_to_term(bin2)

          term ->
            term
        end

      :eof ->
        :eof
    end
  end

  defp do_reset_main_node(module, nodes) do
    reset_counters(module)
    do_reset_collection_table(module)
    remote_reset(module, nodes)
  end

  defp do_reset_collection_table(module) do
    :ets.delete(
      :cover_collected_remote_clause_table,
      module
    )

    :ets.match_delete(
      :cover_collected_remote_data_table,
      {r_bump(module: module), :_}
    )
  end

  defp do_clear(module) do
    :ets.match_delete(
      :cover_internal_clause_table,
      {module, :_}
    )

    clear_counters(module)

    case :lists.member(
           :cover_collected_remote_data_table,
           :ets.all()
         ) do
      true ->
        :ets.match_delete(
          :cover_collected_remote_data_table,
          {r_bump(module: module), :_}
        )

      false ->
        :ok
    end
  end

  defp not_loaded(module, :unloaded, state) do
    do_clear(module)
    remote_unload(r_main_state(state, :nodes), [module])

    compiled =
      update_compiled(
        [module],
        r_main_state(state, :compiled)
      )

    r_main_state(state, compiled: compiled)
  end

  defp not_loaded(_Module, _Else, state) do
    state
  end

  defp escape_lt_and_gt(rawline, hTML) when hTML !== true do
    rawline
  end

  defp escape_lt_and_gt(rawline, _HTML) do
    escape_lt_and_gt1(rawline, [])
  end

  defp escape_lt_and_gt1([?< | t], acc) do
    escape_lt_and_gt1(t, [?;, ?t, ?l, ?& | acc])
  end

  defp escape_lt_and_gt1([?> | t], acc) do
    escape_lt_and_gt1(t, [?;, ?t, ?g, ?& | acc])
  end

  defp escape_lt_and_gt1([?& | t], acc) do
    escape_lt_and_gt1(t, [?;, ?p, ?m, ?a, ?& | acc])
  end

  defp escape_lt_and_gt1([], acc) do
    :lists.reverse(acc)
  end

  defp escape_lt_and_gt1([h | t], acc) do
    escape_lt_and_gt1(t, [h | acc])
  end

  defp pmap(fun, list) do
    nTot = length(list)
    nProcs = :erlang.system_info(:schedulers) * 2
    nPerProc = div(nTot, nProcs) + 1
    mons = pmap_spawn(fun, nPerProc, list, [])
    pmap_collect(mons, [])
  end

  defp pmap_spawn(_, _, [], mons) do
    mons
  end

  defp pmap_spawn(fun, nPerProc, list, mons) do
    {l1, l2} =
      cond do
        length(list) >= nPerProc ->
          :lists.split(nPerProc, list)

        true ->
          {list, []}
      end

    mon =
      spawn_monitor(fn ->
        exit({:pmap_done, :lists.map(fun, l1)})
      end)

    pmap_spawn(fun, nPerProc, l2, [mon | mons])
  end

  defp pmap_collect([], acc) do
    :lists.append(acc)
  end

  defp pmap_collect(mons, acc) do
    receive do
      {:DOWN, ref, :process, pid, {:pmap_done, result}} ->
        pmap_collect(
          :lists.delete({pid, ref}, mons),
          [result | acc]
        )

      {:DOWN, ref, :process, pid, reason} = down ->
        case :lists.member({pid, ref}, mons) do
          true ->
            exit(reason)

          false ->
            send(self(), down)
            pmap_collect(mons, acc)
        end
    end
  end

  defp encoding(file) do
    case :file.native_name_encoding() do
      :latin1 ->
        case :epp.read_encoding(file) do
          :none ->
            :epp.default_encoding()

          e ->
            e
        end

      :utf8 ->
        :utf8
    end
  end

  defp html_encoding(:latin1) do
    'iso-8859-1'
  end

  defp html_encoding(:utf8) do
    'utf-8'
  end
end
