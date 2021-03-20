defmodule :m_typer do
  use Bitwise
  require Record

  Record.defrecord(:r_analysis, :analysis,
    mode: :undefined,
    macros: [],
    includes: [],
    codeserver: :dialyzer_codeserver.new(),
    callgraph: :dialyzer_callgraph.new(),
    files: [],
    plt: :none,
    no_spec: false,
    show_succ: false,
    edoc: false,
    fms: [],
    ex_func: map__new(),
    record: map__new(),
    func: map__new(),
    inc_func: map__new(),
    trust_plt: :dialyzer_plt.new()
  )

  Record.defrecord(:r_args, :args, files: [], files_r: [], trusted: [])

  def start() do
    _ =
      :io.setopts(
        :standard_error,
        [{:encoding, :unicode}]
      )

    _ = :io.setopts([{:encoding, :unicode}])
    {args, analysis} = process_cl_args()
    timer = :dialyzer_timing.init(false)
    trustedFiles = filter_fd(r_args(args, :trusted), [], &is_erl_file/1)
    analysis2 = extract(analysis, trustedFiles)
    all_Files = get_all_files(args)
    analysis3 = r_analysis(analysis2, files: all_Files)
    analysis4 = collect_info(analysis3)
    typeInfo = get_type_info(analysis4)
    :dialyzer_timing.stop(timer)
    show_or_annotate(typeInfo)
    :erlang.halt(0)
  end

  defp extract(
         r_analysis(macros: macros, includes: includes, trust_plt: trustPLT) = analysis,
         trustedFiles
       ) do
    ds =
      for {name, value} <- macros do
        {:d, name, value}
      end

    codeServer = :dialyzer_codeserver.new()

    fun = fn file, cS ->
      allIncludes = [
        :filename.dirname(:filename.dirname(file))
        | includes
      ]

      is =
        for dir <- allIncludes do
          {:i, dir}
        end

      compOpts = :dialyzer_utils.src_compiler_opts() ++ is ++ ds

      case :dialyzer_utils.get_core_from_src(
             file,
             compOpts
           ) do
        {:ok, core} ->
          case :dialyzer_utils.get_record_and_type_info(core) do
            {:ok, recDict} ->
              mod = :erlang.list_to_atom(:filename.basename(file, '.erl'))

              case :dialyzer_utils.get_spec_info(mod, core, recDict) do
                {:ok, specDict, cbDict} ->
                  cS1 =
                    :dialyzer_codeserver.store_temp_records(
                      mod,
                      recDict,
                      cS
                    )

                  :dialyzer_codeserver.store_temp_contracts(
                    mod,
                    specDict,
                    cbDict,
                    cS1
                  )

                {:error, reason} ->
                  compile_error([reason])
              end

            {:error, reason} ->
              compile_error([reason])
          end

        {:error, reason} ->
          compile_error(reason)
      end
    end

    codeServer1 = :lists.foldl(fun, codeServer, trustedFiles)

    newCodeServer =
      try do
        codeServer2 =
          :dialyzer_utils.merge_types(
            codeServer1,
            trustPLT
          )

        newExpTypes = :dialyzer_codeserver.get_temp_exported_types(codeServer1)

        case :sets.size(newExpTypes) do
          0 ->
            :ok
        end

        codeServer3 =
          :dialyzer_codeserver.finalize_exported_types(
            newExpTypes,
            codeServer2
          )

        codeServer4 = :dialyzer_utils.process_record_remote_types(codeServer3)
        :dialyzer_contracts.process_contract_remote_types(codeServer4)
      catch
        {:error, errorMsg} ->
          compile_error(errorMsg)
      end

    contractsDict = :dialyzer_codeserver.get_contracts(newCodeServer)
    contracts = :orddict.from_list(:dict.to_list(contractsDict))

    newTrustPLT =
      :dialyzer_plt.insert_contract_list(
        trustPLT,
        contracts
      )

    r_analysis(analysis, trust_plt: newTrustPLT)
  end

  defp get_type_info(
         r_analysis(callgraph: callGraph, trust_plt: trustPLT, codeserver: codeServer) = analysis
       ) do
    strippedCallGraph = remove_external(callGraph, trustPLT)

    try do
      newPlt = :dialyzer_succ_typings.analyze_callgraph(strippedCallGraph, trustPLT, codeServer)

      r_analysis(analysis,
        callgraph: strippedCallGraph,
        trust_plt: newPlt
      )
    catch
      :error, what ->
        fatal_error(:io_lib.format('Analysis failed with message: ~tp', [{what, __STACKTRACE__}]))

      {:dialyzer_succ_typing_error, msg} ->
        fatal_error(:io_lib.format('Analysis failed with message: ~ts', [msg]))
    end
  end

  defp remove_external(callGraph, pLT) do
    {strippedCG0, ext} = :dialyzer_callgraph.remove_external(callGraph)

    case get_external(ext, pLT) do
      [] ->
        :ok

      externals ->
        msg(:io_lib.format(' Unknown functions: ~tp\n', [:lists.usort(externals)]))
        extTypes = rcv_ext_types()

        case extTypes do
          [] ->
            :ok

          _ ->
            msg(:io_lib.format(' Unknown types: ~tp\n', [extTypes]))
        end
    end

    strippedCG0
  end

  defp get_external(exts, plt) do
    fun = fn {_From, to = {m, f, a}}, acc ->
      case :dialyzer_plt.contains_mfa(plt, to) do
        false ->
          case :erl_bif_types.is_known(m, f, a) do
            true ->
              acc

            false ->
              [to | acc]
          end

        true ->
          acc
      end
    end

    :lists.foldl(fun, [], exts)
  end

  Record.defrecord(:r_info, :info,
    records: :maps.new(),
    functions: [],
    types: map__new(),
    edoc: false
  )

  Record.defrecord(:r_inc, :inc, map: map__new(), filter: [])

  defp show_or_annotate(r_analysis(mode: mode, fms: files) = analysis) do
    case mode do
      :show ->
        show(analysis)

      :show_exported ->
        show(analysis)

      :annotate ->
        fun = fn {file, module} ->
          info = get_final_info(file, module, analysis)
          write_typed_file(file, info)
        end

        :lists.foreach(fun, files)

      :annotate_inc_files ->
        incInfo = write_and_collect_inc_info(analysis)
        write_inc_files(incInfo)
    end
  end

  defp write_and_collect_inc_info(analysis) do
    fun = fn {file, module}, inc ->
      info = get_final_info(file, module, analysis)
      write_typed_file(file, info)
      incFuns = get_functions(file, analysis)
      collect_imported_functions(incFuns, r_info(info, :types), inc)
    end

    newInc = :lists.foldl(fun, r_inc(), r_analysis(analysis, :fms))
    clean_inc(newInc)
  end

  defp write_inc_files(inc) do
    fun = fn file ->
      val = map__lookup(file, r_inc(inc, :map))

      functions =
        for {key, _} <- val do
          key
        end

      val1 =
        for {{_Line, f, a}, type} <- val do
          {{f, a}, type}
        end

      info =
        r_info(
          types: map__from_list(val1),
          records: :maps.new(),
          functions: :lists.keysort(1, functions)
        )

      write_typed_file(file, info)
    end

    :lists.foreach(fun, :dict.fetch_keys(r_inc(inc, :map)))
  end

  defp show(analysis) do
    fun = fn {file, module} ->
      info = get_final_info(file, module, analysis)
      show_type_info(file, info)
    end

    :lists.foreach(fun, r_analysis(analysis, :fms))
  end

  defp get_final_info(file, module, analysis) do
    records = get_records(file, analysis)
    types = get_types(module, analysis, records)
    functions = get_functions(file, analysis)
    edoc = r_analysis(analysis, :edoc)
    r_info(records: records, functions: functions, types: types, edoc: edoc)
  end

  defp collect_imported_functions(functions, types, inc) do
    fun = fn {file, _} = obj, i ->
      case is_yecc_gen(file, i) do
        {true, newI} ->
          newI

        {false, newI} ->
          check_imported_functions(obj, newI, types)
      end
    end

    :lists.foldl(fun, inc, functions)
  end

  defp is_yecc_gen(file, r_inc(filter: fs) = inc) do
    case :lists.member(file, fs) do
      true ->
        {true, inc}

      false ->
        case :filename.extension(file) do
          '.yrl' ->
            rootname = :filename.rootname(file, '.yrl')
            obj = rootname ++ '.erl'

            case :lists.member(obj, fs) do
              true ->
                {true, inc}

              false ->
                newInc = r_inc(inc, filter: [obj | fs])
                {true, newInc}
            end

          _ ->
            case :filename.basename(file) do
              'yeccpre.hrl' ->
                {true, inc}

              _ ->
                {false, inc}
            end
        end
    end
  end

  defp check_imported_functions({file, {line, f, a}}, inc, types) do
    incMap = r_inc(inc, :map)
    fA = {f, a}
    type = get_type_info(fA, types)

    case map__lookup(file, incMap) do
      :none ->
        obj = {file, [{fA, {line, type}}]}
        newMap = map__insert(obj, incMap)
        r_inc(inc, map: newMap)

      val ->
        case :lists.keyfind(fA, 1, val) do
          false ->
            obj = {file, val ++ [{fA, {line, type}}]}
            newMap = map__insert(obj, incMap)
            r_inc(inc, map: newMap)

          ^type ->
            inc

          _ ->
            inc_warning(fA, file)
            elem = :lists.keydelete(fA, 1, val)

            newMap =
              case elem do
                [] ->
                  map__remove(file, incMap)

                _ ->
                  map__insert({file, elem}, incMap)
              end

            r_inc(inc, map: newMap)
        end
    end
  end

  defp inc_warning({f, a}, file) do
    :io.format('      ***Warning: Skip function ~tp/~p ', [f, a])
    :io.format('in file ~tp because of inconsistent type\n', [file])
  end

  defp clean_inc(inc) do
    inc1 = remove_yecc_generated_file(inc)
    normalize_obj(inc1)
  end

  defp remove_yecc_generated_file(r_inc(filter: filter) = inc) do
    fun = fn key, r_inc(map: map) = i ->
      r_inc(i, map: map__remove(key, map))
    end

    :lists.foldl(fun, inc, filter)
  end

  defp normalize_obj(tmpInc) do
    fun = fn key, val, inc ->
      newVal =
        for {{f, a}, {line, type}} <- val do
          {{line, f, a}, type}
        end

      map__insert({key, newVal}, inc)
    end

    r_inc(tmpInc, map: map__fold(fun, map__new(), r_inc(tmpInc, :map)))
  end

  defp get_records(file, analysis) do
    map__lookup(file, r_analysis(analysis, :record))
  end

  defp get_types(module, analysis, records) do
    typeInfoPlt = r_analysis(analysis, :trust_plt)

    typeInfo =
      case :dialyzer_plt.lookup_module(
             typeInfoPlt,
             module
           ) do
        :none ->
          []

        {:value, list} ->
          list
      end

    codeServer = r_analysis(analysis, :codeserver)

    typeInfoList =
      case r_analysis(analysis, :show_succ) do
        true ->
          for i <- typeInfo do
            convert_type_info(i)
          end

        false ->
          for i <- typeInfo do
            get_type(i, codeServer, records)
          end
      end

    map__from_list(typeInfoList)
  end

  defp convert_type_info({{_M, f, a}, range, arg}) do
    {{f, a}, {range, arg}}
  end

  defp get_type({{m, f, a} = mFA, range, arg}, codeServer, records) do
    case :dialyzer_codeserver.lookup_mfa_contract(
           mFA,
           codeServer
         ) do
      :error ->
        {{f, a}, {range, arg}}

      {:ok, {_FileLine, contract, _Xtra}} ->
        sig = :erl_types.t_fun(arg, range)

        case :dialyzer_contracts.check_contract(
               contract,
               sig
             ) do
          :ok ->
            {{f, a}, {:contract, contract}}

          {:range_warnings, _} ->
            {{f, a}, {:contract, contract}}

          {:error, {:overlapping_contract, []}} ->
            {{f, a}, {:contract, contract}}

          {:error, :invalid_contract} ->
            cString = :dialyzer_contracts.contract_to_string(contract)
            sigString = :dialyzer_utils.format_sig(sig, records)

            msg =
              :io_lib.format(
                'Error in contract of function ~w:~tw/~w\n\t The contract is: ' ++
                  cString ++ '\n' ++ '\t but the inferred signature is: ~ts',
                [m, f, a, sigString]
              )

            fatal_error(msg)

          {:error, errorStr} when is_list(errorStr) ->
            msg =
              :io_lib.format('Error in contract of function ~w:~tw/~w: ~ts', [m, f, a, errorStr])

            fatal_error(msg)
        end
    end
  end

  defp get_functions(file, analysis) do
    case r_analysis(analysis, :mode) do
      :show ->
        funcs = map__lookup(file, r_analysis(analysis, :func))
        inc_Funcs = map__lookup(file, r_analysis(analysis, :inc_func))
        remove_module_info(funcs) ++ normalize_incFuncs(inc_Funcs)

      :show_exported ->
        ex_Funcs = map__lookup(file, r_analysis(analysis, :ex_func))
        remove_module_info(ex_Funcs)

      :annotate ->
        funcs = map__lookup(file, r_analysis(analysis, :func))
        remove_module_info(funcs)

      :annotate_inc_files ->
        map__lookup(file, r_analysis(analysis, :inc_func))
    end
  end

  defp normalize_incFuncs(functions) do
    for {_FileName, funInfo} <- functions do
      funInfo
    end
  end

  defp remove_module_info(funInfoList) do
    f = fn
      {_, :module_info, 0} ->
        false

      {_, :module_info, 1} ->
        false

      {line, f, a}
      when is_integer(line) and is_atom(f) and
             is_integer(a) ->
        true
    end

    :lists.filter(f, funInfoList)
  end

  defp write_typed_file(file, info) do
    :io.format('      Processing file: ~tp\n', [file])
    dir = :filename.dirname(file)
    rootName = :filename.basename(:filename.rootname(file))
    ext = :filename.extension(file)
    typerAnnDir = :filename.join(dir, 'typer_ann')
    tmpNewFilename = :lists.concat([rootName, '.ann', ext])

    newFileName =
      :filename.join(
        typerAnnDir,
        tmpNewFilename
      )

    case :file.make_dir(typerAnnDir) do
      {:error, reason} ->
        case reason do
          :eexist ->
            case :file.delete(newFileName) do
              :ok ->
                :ok

              {:error, :enoent} ->
                :ok

              {:error, _} ->
                msg = :io_lib.format('Error in deleting file ~ts\n', [newFileName])
                fatal_error(msg)
            end

            write_typed_file(file, info, newFileName)

          :enospc ->
            msg = :io_lib.format('Not enough space in ~tp\n', [dir])
            fatal_error(msg)

          :eacces ->
            msg = :io_lib.format('No write permission in ~tp\n', [dir])
            fatal_error(msg)

          _ ->
            msg = :io_lib.format('Unhandled error ~ts when writing ~tp\n', [reason, dir])
            fatal_error(msg)
        end

      :ok ->
        write_typed_file(file, info, newFileName)
    end
  end

  defp write_typed_file(file, info, newFileName) do
    {:ok, binary} = :file.read_file(file)
    chars = :unicode.characters_to_list(binary)
    write_typed_file(chars, newFileName, info, 1, [])
    :io.format('             Saved as: ~tp\n', [newFileName])
  end

  defp write_typed_file(chars, file, r_info(functions: []), _LNo, _Acc) do
    :ok = :file.write_file(file, :unicode.characters_to_binary(chars), [:append])
  end

  defp write_typed_file([ch | chs] = chars, file, info, lineNo, acc) do
    [{line, f, a} | restFuncs] = r_info(info, :functions)

    case line do
      1 ->
        :ok = raw_write(f, a, info, file, [])
        newInfo = r_info(info, functions: restFuncs)
        newAcc = []
        write_typed_file(chars, file, newInfo, line, newAcc)

      _ ->
        case ch do
          10 ->
            newLineNo = lineNo + 1

            {newInfo, newAcc} =
              case newLineNo do
                ^line ->
                  :ok = raw_write(f, a, info, file, [ch | acc])
                  {r_info(info, functions: restFuncs), []}

                _ ->
                  {info, [ch | acc]}
              end

            write_typed_file(chs, file, newInfo, newLineNo, newAcc)

          _ ->
            write_typed_file(chs, file, info, lineNo, [ch | acc])
        end
    end
  end

  defp raw_write(f, a, info, file, content) do
    typeInfo = get_type_string(f, a, info, :file)
    contentList = :lists.reverse(content) ++ typeInfo ++ '\n'
    contentBin = :unicode.characters_to_binary(contentList)
    :file.write_file(file, contentBin, [:append])
  end

  defp get_type_string(f, a, info, mode) do
    type = get_type_info({f, a}, r_info(info, :types))

    typeStr =
      case type do
        {:contract, c} ->
          :dialyzer_contracts.contract_to_string(c)

        {retType, argType} ->
          sig = :erl_types.t_fun(argType, retType)
          :dialyzer_utils.format_sig(sig, r_info(info, :records))
      end

    case r_info(info, :edoc) do
      false ->
        case {mode, type} do
          {:file, {:contract, _}} ->
            ''

          _ ->
            prefix = :lists.concat(['-spec ', :erl_types.atom_to_string(f)])
            :lists.concat([prefix, typeStr, '.'])
        end

      true ->
        prefix = :lists.concat(['%% @spec ', f])
        :lists.concat([prefix, typeStr, '.'])
    end
  end

  defp show_type_info(file, info) do
    :io.format('\n%% File: ~tp\n%% ', [file])
    outputString = :lists.concat(['~.', length(file) + 8, 'c~n'])
    :io.fwrite(outputString, [?-])

    fun = fn {_LineNo, f, a} ->
      typeInfo = get_type_string(f, a, info, :show)
      :io.format('~ts\n', [typeInfo])
    end

    :lists.foreach(fun, r_info(info, :functions))
  end

  defp get_type_info(func, types) do
    case map__lookup(func, types) do
      :none ->
        msg = :io_lib.format('No type info for function: ~tp\n', [func])
        fatal_error(msg)

      {:contract, _Fun} = c ->
        c

      {_RetType, _ArgType} = rA ->
        rA
    end
  end

  defp process_cl_args() do
    argList = :init.get_plain_arguments()
    {args, analysis} = analyze_args(argList, r_args(), r_analysis())

    {args,
     case r_analysis(analysis, :mode) do
       :undefined ->
         r_analysis(analysis, mode: :show)

       mode when is_atom(mode) ->
         analysis
     end}
  end

  defp analyze_args([], args, analysis) do
    {args, analysis}
  end

  defp analyze_args(argList, args, analysis) do
    {result, rest} = cl(argList)
    {newArgs, newAnalysis} = analyze_result(result, args, analysis)
    analyze_args(rest, newArgs, newAnalysis)
  end

  defp cl(['-h' | _]) do
    help_message()
  end

  defp cl(['--help' | _]) do
    help_message()
  end

  defp cl(['-v' | _]) do
    version_message()
  end

  defp cl(['--version' | _]) do
    version_message()
  end

  defp cl(['--edoc' | opts]) do
    {:edoc, opts}
  end

  defp cl(['--show' | opts]) do
    {{:mode, :show}, opts}
  end

  defp cl(['--show_exported' | opts]) do
    {{:mode, :show_exported}, opts}
  end

  defp cl(['--show-exported' | opts]) do
    {{:mode, :show_exported}, opts}
  end

  defp cl(['--show_success_typings' | opts]) do
    {:show_succ, opts}
  end

  defp cl(['--show-success-typings' | opts]) do
    {:show_succ, opts}
  end

  defp cl(['--annotate' | opts]) do
    {{:mode, :annotate}, opts}
  end

  defp cl(['--annotate-inc-files' | opts]) do
    {{:mode, :annotate_inc_files}, opts}
  end

  defp cl(['--no_spec' | opts]) do
    {:no_spec, opts}
  end

  defp cl(['--plt', plt | opts]) do
    {{:plt, plt}, opts}
  end

  defp cl(['-D' ++ def__ | opts]) do
    case def__ do
      '' ->
        fatal_error('no variable name specified after -D')

      _ ->
        defPair = process_def_list(:re.split(def__, '=', [{:return, :list}, :unicode]))
        {{:def, defPair}, opts}
    end
  end

  defp cl(['-I', dir | opts]) do
    {{:inc, dir}, opts}
  end

  defp cl(['-I' ++ dir | opts]) do
    case dir do
      '' ->
        fatal_error('no include directory specified after -I')

      _ ->
        {{:inc, dir}, opts}
    end
  end

  defp cl(['-T' | opts]) do
    {files, restOpts} = :dialyzer_cl_parse.collect_args(opts)

    case files do
      [] ->
        fatal_error('no file or directory specified after -T')

      [_ | _] ->
        {{:trusted, files}, restOpts}
    end
  end

  defp cl(['-r' | opts]) do
    {files, restOpts} = :dialyzer_cl_parse.collect_args(opts)
    {{:files_r, files}, restOpts}
  end

  defp cl(['-pa', dir | opts]) do
    {{:pa, dir}, opts}
  end

  defp cl(['-pz', dir | opts]) do
    {{:pz, dir}, opts}
  end

  defp cl(['-' ++ h | _]) do
    fatal_error('unknown option -' ++ h)
  end

  defp cl(opts) do
    {files, restOpts} = :dialyzer_cl_parse.collect_args(opts)
    {{:files, files}, restOpts}
  end

  defp process_def_list(l) do
    case l do
      [name, value] ->
        {:ok, tokens, _} = :erl_scan.string(value ++ '.')
        {:ok, erlValue} = :erl_parse.parse_term(tokens)
        {:erlang.list_to_atom(name), erlValue}

      [name] ->
        {:erlang.list_to_atom(name), true}
    end
  end

  defp analyze_result({:files, val}, args, analysis) do
    newVal = r_args(args, :files) ++ val
    {r_args(args, files: newVal), analysis}
  end

  defp analyze_result({:files_r, val}, args, analysis) do
    newVal = r_args(args, :files_r) ++ val
    {r_args(args, files_r: newVal), analysis}
  end

  defp analyze_result({:trusted, val}, args, analysis) do
    newVal = r_args(args, :trusted) ++ val
    {r_args(args, trusted: newVal), analysis}
  end

  defp analyze_result(:edoc, args, analysis) do
    {args, r_analysis(analysis, edoc: true)}
  end

  defp analyze_result({:mode, mode}, args, analysis) do
    case r_analysis(analysis, :mode) do
      :undefined ->
        {args, r_analysis(analysis, mode: mode)}

      oldMode ->
        mode_error(oldMode, mode)
    end
  end

  defp analyze_result({:def, val}, args, analysis) do
    newVal = r_analysis(analysis, :macros) ++ [val]
    {args, r_analysis(analysis, macros: newVal)}
  end

  defp analyze_result({:inc, val}, args, analysis) do
    newVal = r_analysis(analysis, :includes) ++ [val]
    {args, r_analysis(analysis, includes: newVal)}
  end

  defp analyze_result({:plt, plt}, args, analysis) do
    {args, r_analysis(analysis, plt: plt)}
  end

  defp analyze_result(:show_succ, args, analysis) do
    {args, r_analysis(analysis, show_succ: true)}
  end

  defp analyze_result(:no_spec, args, analysis) do
    {args, r_analysis(analysis, no_spec: true)}
  end

  defp analyze_result({:pa, dir}, args, analysis) do
    true = :code.add_patha(dir)
    {args, analysis}
  end

  defp analyze_result({:pz, dir}, args, analysis) do
    true = :code.add_pathz(dir)
    {args, analysis}
  end

  defp get_all_files(r_args(files: fs, files_r: ds)) do
    case filter_fd(fs, ds, &test_erl_file_exclude_ann/1) do
      [] ->
        fatal_error('no file(s) to analyze')

      allFiles ->
        allFiles
    end
  end

  defp test_erl_file_exclude_ann(file) do
    case is_erl_file(file) do
      true ->
        case :re.run(file, '[.]ann[.]erl$', [:unicode]) do
          {:match, _} ->
            false

          :nomatch ->
            true
        end

      false ->
        false
    end
  end

  defp is_erl_file(file) do
    :filename.extension(file) === '.erl'
  end

  defp filter_fd(file_Dir, dir_R, fun) do
    all_File_1 = process_file_and_dir(file_Dir, fun)
    all_File_2 = process_dir_rec(dir_R, fun)
    remove_dup(all_File_1 ++ all_File_2)
  end

  defp process_file_and_dir(file_Dir, testFun) do
    fun = fn elem, acc ->
      case :filelib.is_regular(elem) do
        true ->
          process_file(elem, testFun, acc)

        false ->
          check_dir(elem, false, acc, testFun)
      end
    end

    :lists.foldl(fun, [], file_Dir)
  end

  defp process_dir_rec(dirs, testFun) do
    fun = fn dir, acc ->
      check_dir(dir, true, acc, testFun)
    end

    :lists.foldl(fun, [], dirs)
  end

  defp check_dir(dir, recursive, acc, fun) do
    case :file.list_dir(dir) do
      {:ok, files} ->
        {tmpDirs, tmpFiles} = split_dirs_and_files(files, dir)

        case recursive do
          false ->
            finalFiles = process_file_and_dir(tmpFiles, fun)
            acc ++ finalFiles

          true ->
            tmpAcc1 = process_file_and_dir(tmpFiles, fun)
            tmpAcc2 = process_dir_rec(tmpDirs, fun)
            acc ++ tmpAcc1 ++ tmpAcc2
        end

      {:error, :eacces} ->
        fatal_error('no access permission to dir "' ++ dir ++ '"')

      {:error, :enoent} ->
        fatal_error('cannot access ' ++ dir ++ ': No such file or directory')

      {:error, _Reason} ->
        fatal_error('error involving a use of file:list_dir/1')
    end
  end

  defp process_file(file, testFun, acc) do
    case testFun.(file) do
      true ->
        acc ++ [file]

      false ->
        acc
    end
  end

  defp split_dirs_and_files(elems, dir) do
    test_Fun = fn elem, {dirAcc, fileAcc} ->
      file = :filename.join(dir, elem)

      case :filelib.is_regular(file) do
        false ->
          {[file | dirAcc], fileAcc}

        true ->
          {dirAcc, [file | fileAcc]}
      end
    end

    {dirs, files} = :lists.foldl(test_Fun, {[], []}, elems)
    {:lists.reverse(dirs), :lists.reverse(files)}
  end

  defp remove_dup(files) do
    test_Dup = fn file, acc ->
      case :lists.member(file, acc) do
        true ->
          acc

        false ->
          [file | acc]
      end
    end

    reversed_Elems = :lists.foldl(test_Dup, [], files)
    :lists.reverse(reversed_Elems)
  end

  Record.defrecord(:r_tmpAcc, :tmpAcc,
    file: :undefined,
    module: :undefined,
    funcAcc: [],
    incFuncAcc: [],
    dialyzerObj: []
  )

  defp collect_info(analysis) do
    newPlt =
      try do
        get_dialyzer_plt(analysis)
      catch
        {:dialyzer_error, _Reason} ->
          fatal_error('Dialyzer\'s PLT is missing or is not up-to-date; please (re)create it')
      else
        dialyzerPlt ->
          :dialyzer_plt.merge_plts([r_analysis(analysis, :trust_plt), dialyzerPlt])
      end

    newAnalysis =
      :lists.foldl(
        &collect_one_file_info/2,
        r_analysis(analysis, trust_plt: newPlt),
        r_analysis(analysis, :files)
      )

    tmpCServer = r_analysis(newAnalysis, :codeserver)

    newCServer =
      try do
        tmpCServer1 =
          :dialyzer_utils.merge_types(
            tmpCServer,
            newPlt
          )

        newExpTypes = :dialyzer_codeserver.get_temp_exported_types(tmpCServer)
        oldExpTypes = :dialyzer_plt.get_exported_types(newPlt)
        mergedExpTypes = :sets.union(newExpTypes, oldExpTypes)

        tmpCServer2 =
          :dialyzer_codeserver.finalize_exported_types(
            mergedExpTypes,
            tmpCServer1
          )

        tmpCServer3 = :dialyzer_utils.process_record_remote_types(tmpCServer2)
        :dialyzer_contracts.process_contract_remote_types(tmpCServer3)
      catch
        {:error, errorMsg} ->
          fatal_error(errorMsg)
      end

    r_analysis(newAnalysis, codeserver: newCServer)
  end

  defp collect_one_file_info(file, analysis) do
    ds =
      for {name, val} <- r_analysis(analysis, :macros) do
        {:d, name, val}
      end

    includes = [
      :filename.dirname(file)
      | r_analysis(analysis, :includes)
    ]

    is =
      for dir <- includes do
        {:i, dir}
      end

    options = :dialyzer_utils.src_compiler_opts() ++ is ++ ds

    case :dialyzer_utils.get_core_from_src(
           file,
           options
         ) do
      {:error, reason} ->
        compile_error(reason)

      {:ok, core} ->
        case :dialyzer_utils.get_record_and_type_info(core) do
          {:error, reason} ->
            compile_error([reason])

          {:ok, records} ->
            mod = :cerl.concrete(:cerl.module_name(core))

            case :dialyzer_utils.get_spec_info(mod, core, records) do
              {:error, reason} ->
                compile_error([reason])

              {:ok, specInfo, cbInfo} ->
                expTypes = get_exported_types_from_core(core)
                analyze_core_tree(core, records, specInfo, cbInfo, expTypes, analysis, file)
            end
        end
    end
  end

  defp analyze_core_tree(core, records, specInfo, cbInfo, expTypes, analysis, file) do
    module = :cerl.concrete(:cerl.module_name(core))
    tmpTree = :cerl.from_records(core)
    cS1 = r_analysis(analysis, :codeserver)
    nextLabel = :dialyzer_codeserver.get_next_core_label(cS1)
    {tree, newLabel} = :cerl_trees.label(tmpTree, nextLabel)
    cS2 = :dialyzer_codeserver.insert(module, tree, cS1)

    cS3 =
      :dialyzer_codeserver.set_next_core_label(
        newLabel,
        cS2
      )

    cS4 = :dialyzer_codeserver.store_temp_records(module, records, cS3)

    cS5 =
      case r_analysis(analysis, :no_spec) do
        true ->
          cS4

        false ->
          :dialyzer_codeserver.store_temp_contracts(module, specInfo, cbInfo, cS4)
      end

    oldExpTypes = :dialyzer_codeserver.get_temp_exported_types(cS5)
    mergedExpTypes = :sets.union(expTypes, oldExpTypes)

    cS6 =
      :dialyzer_codeserver.insert_temp_exported_types(
        mergedExpTypes,
        cS5
      )

    ex_Funcs =
      for {_, _, {f, a}} <- :cerl.module_exports(tree) do
        {0, f, a}
      end

    cG = r_analysis(analysis, :callgraph)
    {v, e} = :dialyzer_callgraph.scan_core_tree(tree, cG)
    :dialyzer_callgraph.add_edges(e, v, cG)
    fun = &analyze_one_function/2
    all_Defs = :cerl.module_defs(tree)
    acc = :lists.foldl(fun, r_tmpAcc(file: file, module: module), all_Defs)

    exported_FuncMap =
      map__insert(
        {file, ex_Funcs},
        r_analysis(analysis, :ex_func)
      )

    sorted_Functions = :lists.keysort(1, r_tmpAcc(acc, :funcAcc))

    funcMap =
      map__insert(
        {file, sorted_Functions},
        r_analysis(analysis, :func)
      )

    incFuncMap =
      map__insert(
        {file, r_tmpAcc(acc, :incFuncAcc)},
        r_analysis(analysis, :inc_func)
      )

    fMs = r_analysis(analysis, :fms) ++ [{file, module}]

    recordMap =
      map__insert(
        {file, records},
        r_analysis(analysis, :record)
      )

    r_analysis(analysis,
      fms: fMs,
      callgraph: cG,
      codeserver: cS6,
      ex_func: exported_FuncMap,
      inc_func: incFuncMap,
      record: recordMap,
      func: funcMap
    )
  end

  defp analyze_one_function({var, funBody} = function, acc) do
    f = :cerl.fname_id(var)
    a = :cerl.fname_arity(var)
    tmpDialyzerObj = {{r_tmpAcc(acc, :module), f, a}, function}
    newDialyzerObj = r_tmpAcc(acc, :dialyzerObj) ++ [tmpDialyzerObj]
    anno = :cerl.get_ann(funBody)
    lineNo = get_line(anno)
    fileName = get_file(anno)
    baseName = :filename.basename(fileName)
    funcInfo = {lineNo, f, a}
    originalName = r_tmpAcc(acc, :file)

    {funcAcc, incFuncAcc} =
      case fileName === originalName or baseName === originalName do
        true ->
          {r_tmpAcc(acc, :funcAcc) ++ [funcInfo], r_tmpAcc(acc, :incFuncAcc)}

        false ->
          {r_tmpAcc(acc, :funcAcc), r_tmpAcc(acc, :incFuncAcc) ++ [{fileName, funcInfo}]}
      end

    r_tmpAcc(acc, funcAcc: funcAcc, incFuncAcc: incFuncAcc, dialyzerObj: newDialyzerObj)
  end

  defp get_line([line | _]) when is_integer(line) do
    line
  end

  defp get_line([_ | t]) do
    get_line(t)
  end

  defp get_line([]) do
    :none
  end

  defp get_file([{:file, file} | _]) do
    file
  end

  defp get_file([_ | t]) do
    get_file(t)
  end

  defp get_file([]) do
    'no_file'
  end

  defp get_dialyzer_plt(r_analysis(plt: pltFile0)) do
    pltFile =
      case pltFile0 === :none do
        true ->
          :dialyzer_plt.get_default_plt()

        false ->
          pltFile0
      end

    :dialyzer_plt.from_file(pltFile)
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

  defp fatal_error(slogan) do
    msg(:io_lib.format('typer: ~ts\n', [slogan]))
    :erlang.halt(1)
  end

  defp mode_error(oldMode, newMode) do
    msg =
      :io_lib.format('Mode was previously set to \'~s\'; cannot set it to \'~s\' now', [
        oldMode,
        newMode
      ])

    fatal_error(msg)
  end

  defp compile_error(reason) do
    joinedString =
      :lists.flatten(
        for x <- reason do
          x ++ '\n'
        end
      )

    msg = 'Analysis failed with error report:\n' ++ joinedString
    fatal_error(msg)
  end

  defp msg(msg) do
    :io.format(:standard_error, '~ts', [msg])
  end

  defp version_message() do
    :io.format('TypEr version ' ++ :EFE_TODO_VSN_MACRO ++ '\n')
    :erlang.halt(0)
  end

  defp help_message() do
    s =
      " Usage: typer [--help] [--version] [--plt PLT] [--edoc]\n              [--show | --show-exported | --annotate | --annotate-inc-files]\n              [-Ddefine]* [-I include_dir]* [-pa dir]* [-pz dir]*\n              [-T application]* [-r] file*\n\n Options:\n   -r dir*\n       search directories recursively for .erl files below them\n   --show\n       Prints type specifications for all functions on stdout.\n       (this is the default behaviour; this option is not really needed)\n   --show-exported (or --show_exported)\n       Same as --show, but prints specifications for exported functions only\n       Specs are displayed sorted alphabetically on the function's name\n   --annotate\n       Annotates the specified files with type specifications\n   --annotate-inc-files\n       Same as --annotate but annotates all -include() files as well as\n       all .erl files (use this option with caution - has not been tested much)\n   --edoc\n       Prints type information as Edoc @spec comments, not as type specs\n   --plt PLT\n       Use the specified dialyzer PLT file rather than the default one\n   -T file*\n       The specified file(s) already contain type specifications and these\n       are to be trusted in order to print specs for the rest of the files\n       (Multiple files or dirs, separated by spaces, can be specified.)\n   -Dname (or -Dname=value)\n       pass the defined name(s) to TypEr\n       (The syntax of defines is the same as that used by \"erlc\".)\n   -I include_dir\n       pass the include_dir to TypEr\n       (The syntax of includes is the same as that used by \"erlc\".)\n   -pa dir\n   -pz dir\n       Set code path options to TypEr\n       (This is useful for files that use parse tranforms.)\n   --version (or -v)\n       prints the Typer version and exits\n   --help (or -h)\n       prints this message and exits\n\n Note:\n   * denotes that multiple occurrences of these options are possible.\n"

    :io.put_chars(s)
    :erlang.halt(0)
  end

  defp rcv_ext_types() do
    self = self()
    send(self, {self, :done})
    rcv_ext_types(self, [])
  end

  defp rcv_ext_types(self, extTypes) do
    receive do
      {^self, :ext_types, extType} ->
        rcv_ext_types(self, [extType | extTypes])

      {^self, :done} ->
        :lists.usort(extTypes)
    end
  end

  defp map__new() do
    :dict.new()
  end

  defp map__insert(object, map) do
    {key, value} = object
    :dict.store(key, value, map)
  end

  defp map__lookup(key, map) do
    try do
      :dict.fetch(key, map)
    catch
      :error, _ ->
        :none
    end
  end

  defp map__from_list(list) do
    :dict.from_list(list)
  end

  defp map__remove(key, dict) do
    :dict.erase(key, dict)
  end

  defp map__fold(fun, acc0, dict) do
    :dict.fold(fun, acc0, dict)
  end
end
