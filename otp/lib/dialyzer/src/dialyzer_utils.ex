defmodule :m_dialyzer_utils do
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
  Record.defrecord(:r_c_alias, :c_alias, anno: [], var: :undefined, pat: :undefined)
  Record.defrecord(:r_c_apply, :c_apply, anno: [], op: :undefined, args: :undefined)

  Record.defrecord(:r_c_binary, :c_binary,
    anno: [],
    segments: :undefined
  )

  Record.defrecord(:r_c_bitstr, :c_bitstr,
    anno: [],
    val: :undefined,
    size: :undefined,
    unit: :undefined,
    type: :undefined,
    flags: :undefined
  )

  Record.defrecord(:r_c_call, :c_call,
    anno: [],
    module: :undefined,
    name: :undefined,
    args: :undefined
  )

  Record.defrecord(:r_c_case, :c_case, anno: [], arg: :undefined, clauses: :undefined)

  Record.defrecord(:r_c_catch, :c_catch,
    anno: [],
    body: :undefined
  )

  Record.defrecord(:r_c_clause, :c_clause,
    anno: [],
    pats: :undefined,
    guard: :undefined,
    body: :undefined
  )

  Record.defrecord(:r_c_cons, :c_cons, anno: [], hd: :undefined, tl: :undefined)
  Record.defrecord(:r_c_fun, :c_fun, anno: [], vars: :undefined, body: :undefined)

  Record.defrecord(:r_c_let, :c_let, anno: [], vars: :undefined, arg: :undefined, body: :undefined)

  Record.defrecord(:r_c_letrec, :c_letrec, anno: [], defs: :undefined, body: :undefined)

  Record.defrecord(:r_c_literal, :c_literal,
    anno: [],
    val: :undefined
  )

  Record.defrecord(:r_c_map, :c_map,
    anno: [],
    arg: :EFE_TODO_NESTED_RECORD,
    es: :undefined,
    is_pat: false
  )

  Record.defrecord(:r_c_map_pair, :c_map_pair,
    anno: [],
    op: :undefined,
    key: :undefined,
    val: :undefined
  )

  Record.defrecord(:r_c_module, :c_module,
    anno: [],
    name: :undefined,
    exports: :undefined,
    attrs: :undefined,
    defs: :undefined
  )

  Record.defrecord(:r_c_primop, :c_primop, anno: [], name: :undefined, args: :undefined)

  Record.defrecord(:r_c_receive, :c_receive,
    anno: [],
    clauses: :undefined,
    timeout: :undefined,
    action: :undefined
  )

  Record.defrecord(:r_c_seq, :c_seq, anno: [], arg: :undefined, body: :undefined)

  Record.defrecord(:r_c_try, :c_try,
    anno: [],
    arg: :undefined,
    vars: :undefined,
    body: :undefined,
    evars: :undefined,
    handler: :undefined
  )

  Record.defrecord(:r_c_tuple, :c_tuple, anno: [], es: :undefined)

  Record.defrecord(:r_c_values, :c_values,
    anno: [],
    es: :undefined
  )

  Record.defrecord(:r_c_var, :c_var, anno: [], name: :undefined)

  def get_core_from_src(file) do
    get_core_from_src(file, [])
  end

  def get_core_from_src(file, opts) do
    case :compile.noenv_file(
           file,
           opts ++ src_compiler_opts()
         ) do
      :error ->
        {:error, []}

      {:error, errors, _} ->
        {:error, format_errors(errors)}

      {:ok, _, core} ->
        {:ok, :dialyzer_clean_core.clean(core)}
    end
  end

  def get_core_from_beam(file) do
    get_core_from_beam(file, [])
  end

  def get_core_from_beam(file, opts) do
    case :beam_lib.chunks(file, [:debug_info]) do
      {:ok, {module, [{:debug_info, {:debug_info_v1, backend, metadata}}]}} ->
        case backend.debug_info(:core_v1, module, metadata, opts ++ src_compiler_opts()) do
          {:ok, core} ->
            {:ok, :dialyzer_clean_core.clean(core)}

          {:error, _} ->
            {:error, '  Could not get Core Erlang code for: ' ++ file ++ '\n'}
        end

      _ ->
        {:error,
         '  Could not get Core Erlang code for: ' ++
           file ++ '\n' ++ '  Recompile with +debug_info or analyze starting from source code'}
    end
  end

  def get_record_and_type_info(core) do
    module = :cerl.concrete(:cerl.module_name(core))
    tuples = core_to_attr_tuples(core)
    get_record_and_type_info(tuples, module, :maps.new(), 'nofile')
  end

  defp get_record_and_type_info(
         [{:record, line, [{name, fields0}]} | left],
         module,
         recDict,
         file
       ) do
    {:ok, fields} = get_record_fields(fields0, recDict)
    arity = length(fields)
    fN = {file, line}
    newRecDict = :maps.put({:record, name}, {fN, [{arity, fields}]}, recDict)
    get_record_and_type_info(left, module, newRecDict, file)
  end

  defp get_record_and_type_info(
         [
           {:type, line, [{{:record, name}, fields0, []}]}
           | left
         ],
         module,
         recDict,
         file
       ) do
    {:ok, fields} = get_record_fields(fields0, recDict)
    arity = length(fields)
    fN = {file, line}
    newRecDict = :maps.put({:record, name}, {fN, [{arity, fields}]}, recDict)
    get_record_and_type_info(left, module, newRecDict, file)
  end

  defp get_record_and_type_info([{attr, line, [{name, typeForm}]} | left], module, recDict, file)
       when attr === :type or attr === :opaque do
    fN = {file, line}

    try do
      add_new_type(attr, name, typeForm, [], module, fN, recDict)
    catch
      {:error, _} = error ->
        error
    else
      newRecDict ->
        get_record_and_type_info(left, module, newRecDict, file)
    end
  end

  defp get_record_and_type_info(
         [{attr, line, [{name, typeForm, args}]} | left],
         module,
         recDict,
         file
       )
       when attr === :type or attr === :opaque do
    fN = {file, line}

    try do
      add_new_type(attr, name, typeForm, args, module, fN, recDict)
    catch
      {:error, _} = error ->
        error
    else
      newRecDict ->
        get_record_and_type_info(left, module, newRecDict, file)
    end
  end

  defp get_record_and_type_info([{:file, _, [{includeFile, _}]} | left], module, recDict, _File) do
    get_record_and_type_info(left, module, recDict, includeFile)
  end

  defp get_record_and_type_info([_Other | left], module, recDict, file) do
    get_record_and_type_info(left, module, recDict, file)
  end

  defp get_record_and_type_info([], _Module, recDict, _File) do
    {:ok, recDict}
  end

  defp add_new_type(typeOrOpaque, name, typeForm, argForms, module, fN, recDict) do
    arity = length(argForms)

    case :erl_types.type_is_defined(typeOrOpaque, name, arity, recDict) do
      true ->
        msg = flat_format('Type ~ts/~w already defined\n', [name, arity])
        throw({:error, msg})

      false ->
        try do
          :erl_types.t_var_names(argForms)
        catch
          _, _ ->
            throw(
              {:error,
               flat_format('Type declaration for ~tw does not have variables as parameters', [
                 name
               ])}
            )
        else
          argNames ->
            :maps.put(
              {typeOrOpaque, name, arity},
              {{module, fN, typeForm, argNames}, :erl_types.t_any()},
              recDict
            )
        end
    end
  end

  defp get_record_fields(fields, recDict) do
    fs = get_record_fields(fields, recDict, [])

    {:ok,
     for {name, form} <- fs do
       {name, form, :erl_types.t_any()}
     end}
  end

  defp get_record_fields(
         [
           {:typed_record_field, ordRecField, typeForm}
           | left
         ],
         recDict,
         acc
       ) do
    name =
      case ordRecField do
        {:record_field, _Line, name0} ->
          :erl_parse.normalise(name0)

        {:record_field, _Line, name0, _Init} ->
          :erl_parse.normalise(name0)
      end

    get_record_fields(left, recDict, [{name, typeForm} | acc])
  end

  defp get_record_fields([{:record_field, _Line, name} | left], recDict, acc) do
    a = :erl_anno.set_generated(true, :erl_anno.new(1))

    newAcc = [
      {:erl_parse.normalise(name), {:var, a, :_}}
      | acc
    ]

    get_record_fields(left, recDict, newAcc)
  end

  defp get_record_fields([{:record_field, _Line, name, _Init} | left], recDict, acc) do
    a = :erl_anno.set_generated(true, :erl_anno.new(1))

    newAcc = [
      {:erl_parse.normalise(name), {:var, a, :_}}
      | acc
    ]

    get_record_fields(left, recDict, newAcc)
  end

  defp get_record_fields([], _RecDict, acc) do
    :lists.reverse(acc)
  end

  def process_record_remote_types(cServer) do
    expTypes = :dialyzer_codeserver.get_exported_types(cServer)
    mods = :dialyzer_codeserver.all_temp_modules(cServer)
    process_opaque_types0(mods, cServer, expTypes)
    varTable = :erl_types.var_table__new()
    recordTable = :dialyzer_codeserver.get_temp_records_table(cServer)

    moduleFun = fn module ->
      recordMap =
        :dialyzer_codeserver.lookup_temp_mod_records(
          module,
          cServer
        )

      recordFun = fn {key, value}, c2 ->
        case key do
          {:record, name} ->
            fieldFun = fn {arity, fields}, c4 ->
              mRA = {module, name, arity}
              site = {:record, mRA}

              {fields1, c7} =
                :lists.mapfoldl(
                  fn {fieldName, field, _}, c5 ->
                    check_remote(
                      field,
                      expTypes,
                      mRA,
                      recordTable
                    )

                    {fieldT, c6} =
                      :erl_types.t_from_form(
                        field,
                        expTypes,
                        site,
                        recordTable,
                        varTable,
                        c5
                      )

                    {{fieldName, field, fieldT}, c6}
                  end,
                  c4,
                  fields
                )

              {{arity, fields1}, c7}
            end

            {fileLine, fields} = value
            {fieldsList, c3} = :lists.mapfoldl(fieldFun, c2, :orddict.to_list(fields))
            {{key, {fileLine, :orddict.from_list(fieldsList)}}, c3}

          {_TypeOrOpaque, name, nArgs} ->
            mTA = {module, name, nArgs}
            {{_Module, _FileLine, form, _ArgNames}, _Type} = value
            check_remote(form, expTypes, mTA, recordTable)
            {{key, value}, c2}
        end
      end

      cache = :erl_types.cache__new()

      {recordList, _NewCache} =
        :lists.mapfoldl(
          recordFun,
          cache,
          :maps.to_list(recordMap)
        )

      :dialyzer_codeserver.store_temp_records(
        module,
        :maps.from_list(recordList),
        cServer
      )
    end

    :lists.foreach(moduleFun, mods)
    check_record_fields(mods, cServer, expTypes)
    :dialyzer_codeserver.finalize_records(cServer)
  end

  defp process_opaque_types0(allModules, cServer, tempExpTypes) do
    process_opaque_types(allModules, cServer, tempExpTypes)
    process_opaque_types(allModules, cServer, tempExpTypes)
  end

  defp process_opaque_types(allModules, cServer, tempExpTypes) do
    varTable = :erl_types.var_table__new()
    recordTable = :dialyzer_codeserver.get_temp_records_table(cServer)

    moduleFun = fn module ->
      recordMap =
        :dialyzer_codeserver.lookup_temp_mod_records(
          module,
          cServer
        )

      recordFun = fn {key, value}, c2 ->
        case key do
          {:opaque, name, nArgs} ->
            {{_Module, _FileLine, form, _ArgNames} = f, _Type} = value
            site = {:type, {module, name, nArgs}}

            {type, c3} =
              :erl_types.t_from_form(
                form,
                tempExpTypes,
                site,
                recordTable,
                varTable,
                c2
              )

            {{key, {f, type}}, c3}

          {:type, _Name, _NArgs} ->
            {{key, value}, c2}

          {:record, _RecName} ->
            {{key, value}, c2}
        end
      end

      c0 = :erl_types.cache__new()
      {recordList, _NewCache} = :lists.mapfoldl(recordFun, c0, :maps.to_list(recordMap))

      :dialyzer_codeserver.store_temp_records(
        module,
        :maps.from_list(recordList),
        cServer
      )
    end

    :lists.foreach(moduleFun, allModules)
  end

  defp check_record_fields(allModules, cServer, tempExpTypes) do
    varTable = :erl_types.var_table__new()
    recordTable = :dialyzer_codeserver.get_temp_records_table(cServer)

    checkFun = fn module ->
      checkForm = fn form, site, c1 ->
        :erl_types.t_check_record_fields(
          form,
          tempExpTypes,
          site,
          recordTable,
          varTable,
          c1
        )
      end

      recordMap =
        :dialyzer_codeserver.lookup_temp_mod_records(
          module,
          cServer
        )

      recordFun = fn {key, value}, c2 ->
        case key do
          {:record, name} ->
            fieldFun = fn {arity, fields}, c3 ->
              site = {:record, {module, name, arity}}

              :lists.foldl(
                fn {_, field, _}, c4 ->
                  checkForm.(
                    field,
                    site,
                    c4
                  )
                end,
                c3,
                fields
              )
            end

            {fileLine, fields} = value

            fun = fn ->
              :lists.foldl(fieldFun, c2, fields)
            end

            msg_with_position(fun, fileLine)

          {_OpaqueOrType, name, nArgs} ->
            site = {:type, {module, name, nArgs}}
            {{_Module, fileLine, form, _ArgNames}, _Type} = value

            fun = fn ->
              checkForm.(form, site, c2)
            end

            msg_with_position(fun, fileLine)
        end
      end

      c0 = :erl_types.cache__new()
      _ = :lists.foldl(recordFun, c0, :maps.to_list(recordMap))
    end

    :lists.foreach(checkFun, allModules)
  end

  defp msg_with_position(fun, fileLine) do
    try do
      fun.()
    catch
      {:error, msg} ->
        {file, line} = fileLine
        baseName = :filename.basename(file)
        newMsg = :io_lib.format('~ts:~p: ~ts', [baseName, line, msg])
        throw({:error, newMsg})
    end
  end

  defp check_remote(form, expTypes, what, recordTable) do
    :erl_types.t_from_form_check_remote(form, expTypes, what, recordTable)
  end

  def merge_types(cServer, plt) do
    allNewModules = :dialyzer_codeserver.all_temp_modules(cServer)
    allNewModulesSet = :sets.from_list(allNewModules)
    allOldModulesSet = :dialyzer_plt.all_modules(plt)

    allModulesSet =
      :sets.union(
        allNewModulesSet,
        allOldModulesSet
      )

    moduleFun = fn module ->
      keepOldFun = fn ->
        case :dialyzer_plt.get_module_types(
               plt,
               module
             ) do
          :none ->
            :no

          {:value, oldRecords} ->
            case :sets.is_element(
                   module,
                   allNewModulesSet
                 ) do
              true ->
                :no

              false ->
                {:yes, oldRecords}
            end
        end
      end

      records =
        case keepOldFun.() do
          :no ->
            :dialyzer_codeserver.lookup_temp_mod_records(
              module,
              cServer
            )

          {:yes, oldRecords} ->
            oldRecords
        end

      :dialyzer_codeserver.store_temp_records(module, records, cServer)
    end

    :lists.foreach(moduleFun, :sets.to_list(allModulesSet))
    cServer
  end

  def get_spec_info(modName, core, recordsMap) do
    tuples = core_to_attr_tuples(core)

    optionalCallbacks0 =
      get_optional_callbacks(
        tuples,
        modName
      )

    optionalCallbacks = :gb_sets.from_list(optionalCallbacks0)

    get_spec_info(
      tuples,
      :maps.new(),
      :maps.new(),
      recordsMap,
      modName,
      optionalCallbacks,
      'nofile'
    )
  end

  defp get_optional_callbacks(tuples, modName) do
    for {:optional_callbacks, _, o} <- tuples, is_fa_list(o), {f, a} <- o do
      {modName, f, a}
    end
  end

  defp get_spec_info(
         [{contract, ln, [{id, typeSpec}]} | left],
         specMap,
         callbackMap,
         recordsMap,
         modName,
         optCb,
         file
       )
       when contract === :spec or
              (contract === :callback and
                 is_list(typeSpec)) do
    mFA =
      case id do
        {_, _, _} = t ->
          t

        {f, a} ->
          {modName, f, a}
      end

    xtra =
      for _ <- [:EFE_DUMMY_GEN],
          :gb_sets.is_member(mFA, optCb) do
        :optional_callback
      end

    activeMap =
      case contract do
        :spec ->
          specMap

        :callback ->
          callbackMap
      end

    try do
      :maps.find(mFA, activeMap)
    catch
      {:error, error} ->
        {:error, flat_format('  Error while parsing contract in line ~w: ~ts\n', [ln, error])}
    else
      :error ->
        specData = {typeSpec, xtra}

        newActiveMap =
          :dialyzer_contracts.store_tmp_contract(
            modName,
            mFA,
            {file, ln},
            specData,
            activeMap,
            recordsMap
          )

        {newSpecMap, newCallbackMap} =
          case contract do
            :spec ->
              {newActiveMap, callbackMap}

            :callback ->
              {specMap, newActiveMap}
          end

        get_spec_info(left, newSpecMap, newCallbackMap, recordsMap, modName, optCb, file)

      {:ok, {{otherFile, l}, _D}} ->
        {mod, fun, arity} = mFA

        msg =
          flat_format('  Contract/callback for function ~w:~tw/~w already defined in ~ts:~w\n', [
            mod,
            fun,
            arity,
            otherFile,
            l
          ])

        throw({:error, msg})
    end
  end

  defp get_spec_info(
         [{:file, _, [{includeFile, _}]} | left],
         specMap,
         callbackMap,
         recordsMap,
         modName,
         optCb,
         _File
       ) do
    get_spec_info(left, specMap, callbackMap, recordsMap, modName, optCb, includeFile)
  end

  defp get_spec_info([_Other | left], specMap, callbackMap, recordsMap, modName, optCb, file) do
    get_spec_info(left, specMap, callbackMap, recordsMap, modName, optCb, file)
  end

  defp get_spec_info([], specMap, callbackMap, _RecordsMap, _ModName, _OptCb, _File) do
    {:ok, specMap, callbackMap}
  end

  defp core_to_attr_tuples(core) do
    for {key, value} <- :cerl.module_attrs(core) do
      {:cerl.concrete(key), get_core_line(:cerl.get_ann(key)), :cerl.concrete(value)}
    end
  end

  defp get_core_line([l | _As]) when is_integer(l) do
    l
  end

  defp get_core_line([_ | as]) do
    get_core_line(as)
  end

  defp get_core_line([]) do
    :undefined
  end

  def get_fun_meta_info(m, core, legalWarnings) do
    functions =
      :lists.map(
        &:cerl.var_name/1,
        :cerl.module_vars(core)
      )

    try do
      {get_nowarn_unused_function(m, core, functions), get_func_suppressions(m, core, functions)}
    catch
      {:error, _} = error ->
        error
    else
      {noWarn, funcSupp} ->
        warnings0 = get_options(core, legalWarnings)
        warnings = :ordsets.to_list(warnings0)

        moduleWarnings =
          for w <- warnings do
            {m, w}
          end

        rawProps = :lists.append([noWarn, funcSupp, moduleWarnings])

        process_options(
          :dialyzer_utils.family(rawProps),
          warnings0
        )
    end
  end

  defp process_options([{m, _} = mod | left], warnings)
       when is_atom(m) do
    [mod | process_options(left, warnings)]
  end

  defp process_options(
         [{{_M, _F, _A} = mFA, opts} | left],
         warnings
       ) do
    wL =
      case :lists.member(:nowarn_function, opts) do
        true ->
          [{:nowarn_function, :func}]

        false ->
          ws = :dialyzer_options.build_warnings(opts, warnings)

          modOnly =
            for w <- :ordsets.subtract(warnings, ws) do
              {w, :mod}
            end

          funOnly =
            for w <- :ordsets.subtract(ws, warnings) do
              {w, :func}
            end

          :ordsets.union(modOnly, funOnly)
      end

    case wL do
      [] ->
        process_options(left, warnings)

      _ ->
        [{mFA, wL} | process_options(left, warnings)]
    end
  end

  defp process_options([], _Warnings) do
    []
  end

  defp get_nowarn_unused_function(m, core, functions) do
    opts = get_options_with_tag(:compile, core)
    warn = :erl_lint.bool_option(:warn_unused_function, :nowarn_unused_function, true, opts)
    attrFile = collect_attribute(core, :compile)
    tagsFaList = check_fa_list(attrFile, :nowarn_unused_function, functions)

    fAs =
      case warn do
        false ->
          functions

        true ->
          for {{[:nowarn_unused_function], _L, _File}, fA} <- tagsFaList do
            fA
          end
      end

    for {f, a} <- fAs do
      {{m, f, a}, :no_unused}
    end
  end

  defp get_func_suppressions(m, core, functions) do
    attrFile = collect_attribute(core, :dialyzer)
    tagsFAs = check_fa_list(attrFile, :*, functions)

    fun = fn
      {{[:nowarn_function], _L, _File}, _FA} ->
        :ok

      {optLFile, _FA} ->
        _ = get_options1([optLFile], :ordsets.new())
    end

    :lists.foreach(fun, tagsFAs)

    for {{warnings, _L, _File}, {f, a}} <- tagsFAs,
        w <- warnings do
      {{m, f, a}, w}
    end
  end

  defp get_options(core, legalWarnings) do
    attrFile = collect_attribute(core, :dialyzer)
    get_options1(attrFile, legalWarnings)
  end

  defp get_options1([{args, l, file} | left], warnings) do
    opts =
      for o <- args, is_atom(o) do
        o
      end

    try do
      :dialyzer_options.build_warnings(opts, warnings)
    catch
      {:dialyzer_options_error, msg} ->
        msg1 = flat_format('  ~ts:~w: ~ts', [file, l, msg])
        throw({:error, msg1})
    else
      newWarnings ->
        get_options1(left, newWarnings)
    end
  end

  defp get_options1([], warnings) do
    warnings
  end

  defp collect_attribute(core, tag) do
    collect_attribute(:cerl.module_attrs(core), tag, 'nofile')
  end

  defp collect_attribute([{key, value} | t], tag, file) do
    case :cerl.concrete(key) do
      ^tag ->
        [
          {:cerl.concrete(value), get_core_line(:cerl.get_ann(key)), file}
          | collect_attribute(t, tag, file)
        ]

      :file ->
        [{includeFile, _}] = :cerl.concrete(value)
        collect_attribute(t, tag, includeFile)

      _ ->
        collect_attribute(t, tag, file)
    end
  end

  defp collect_attribute([], _Tag, _File) do
    []
  end

  def is_suppressed_fun(mFA, codeServer) do
    lookup_fun_property(mFA, :nowarn_function, codeServer, false)
  end

  def is_suppressed_tag(morMFA, tag, codeserver) do
    not lookup_fun_property(morMFA, tag, codeserver, true)
  end

  defp lookup_fun_property({m, _F, _A} = mFA, property, codeServer, noInfoReturn) do
    case :dialyzer_codeserver.lookup_meta_info(
           mFA,
           codeServer
         ) do
      :error ->
        lookup_fun_property(m, property, codeServer, noInfoReturn)

      {:ok, mFAPropList} ->
        case :proplists.get_value(property, mFAPropList, :no) do
          :mod ->
            false

          :func ->
            true

          :no ->
            lookup_fun_property(m, property, codeServer, noInfoReturn)
        end
    end
  end

  defp lookup_fun_property(m, property, codeServer, noInfoReturn)
       when is_atom(m) do
    case :dialyzer_codeserver.lookup_meta_info(
           m,
           codeServer
         ) do
      :error ->
        noInfoReturn

      {:ok, mPropList} ->
        :proplists.is_defined(property, mPropList)
    end
  end

  def sets_filter([], expTypes) do
    expTypes
  end

  def sets_filter([mod | mods], expTypes) do
    newExpTypes =
      :sets.filter(
        fn {m, _F, _A} ->
          m !== mod
        end,
        expTypes
      )

    sets_filter(mods, newExpTypes)
  end

  def src_compiler_opts() do
    [
      :no_copt,
      :to_core,
      :binary,
      :return_errors,
      :no_inline,
      :strict_record_tests,
      :strict_record_updates,
      :dialyzer,
      :no_spawn_compiler_process
    ]
  end

  defp format_errors([{mod, errors} | left]) do
    formatedError =
      for {line, m, desc} <- errors do
        :io_lib.format('~ts:~w: ~ts\n', [mod, line, m.format_error(desc)])
      end

    [:lists.flatten(formatedError) | format_errors(left)]
  end

  defp format_errors([]) do
    []
  end

  def format_sig(type) do
    format_sig(type, :maps.new())
  end

  def format_sig(type, recDict) do
    'fun(' ++ sig =
      :lists.flatten(
        :erl_types.t_to_string(
          type,
          recDict
        )
      )

    ')' ++ revSig = :lists.reverse(sig)
    :lists.reverse(revSig)
  end

  defp flat_format(fmt, lst) do
    :lists.flatten(:io_lib.format(fmt, lst))
  end

  defp get_options_with_tag(tag, core) do
    for {key, value} <- :cerl.module_attrs(core),
        :cerl.concrete(key) === tag,
        o <- :cerl.concrete(value) do
      o
    end
  end

  defp check_fa_list(attrFile, tag, functions) do
    funcTab = :gb_sets.from_list(functions)
    check_fa_list1(attrFile, tag, funcTab)
  end

  defp check_fa_list1([{args, l, file} | left], tag, funcs) do
    termsL =
      for {tags, terms0} <- args,
          tag0 <- :lists.flatten([tags]),
          tag === :* or tag === tag0,
          term <- :lists.flatten([terms0]) do
        {{[tag0], l, file}, term}
      end

    case :lists.dropwhile(
           fn {_, t} ->
             is_fa(t)
           end,
           termsL
         ) do
      [] ->
        :ok

      [{_, bad} | _] ->
        msg1 = flat_format('  Bad function ~tw in line ~ts:~w', [bad, file, l])
        throw({:error, msg1})
    end

    case :lists.dropwhile(
           fn {_, fA} ->
             is_known(fA, funcs)
           end,
           termsL
         ) do
      [] ->
        :ok

      [{_, {f, a}} | _] ->
        msg2 = flat_format('  Unknown function ~tw/~w in line ~ts:~w', [f, a, file, l])
        throw({:error, msg2})
    end

    termsL ++ check_fa_list1(left, tag, funcs)
  end

  defp check_fa_list1([], _Tag, _Funcs) do
    []
  end

  defp is_known(fA, funcs) do
    :gb_sets.is_element(fA, funcs)
  end

  defp is_fa_list([e | l]) do
    is_fa(e) and is_fa_list(l)
  end

  defp is_fa_list([]) do
    true
  end

  defp is_fa_list(_) do
    false
  end

  defp is_fa({funcName, arity})
       when is_atom(funcName) and
              is_integer(arity) and arity >= 0 do
    true
  end

  defp is_fa(_) do
    false
  end

  def pp_hook() do
    &pp_hook/3
  end

  defp pp_hook(node, ctxt, cont) do
    case :cerl.type(node) do
      :binary ->
        pp_binary(node, ctxt, cont)

      :bitstr ->
        pp_segment(node, ctxt, cont)

      :map ->
        pp_map(node, ctxt, cont)

      :literal ->
        case :cerl.concrete(node) do
          map when is_map(map) ->
            pp_map(node, ctxt, cont)

          bitstr when is_bitstring(bitstr) ->
            pp_binary(node, ctxt, cont)

          _ ->
            cont.(node, ctxt)
        end

      _ ->
        cont.(node, ctxt)
    end
  end

  defp pp_binary(node, ctxt, cont) do
    :prettypr.beside(
      :prettypr.text('<<'),
      :prettypr.beside(
        pp_segments(cerl_binary_segments(node), ctxt, cont),
        :prettypr.text('>>')
      )
    )
  end

  defp pp_segments([seg], ctxt, cont) do
    pp_segment(seg, ctxt, cont)
  end

  defp pp_segments([], _Ctxt, _Cont) do
    :prettypr.text('')
  end

  defp pp_segments([seg | rest], ctxt, cont) do
    :prettypr.beside(
      pp_segment(seg, ctxt, cont),
      :prettypr.beside(
        :prettypr.text(','),
        pp_segments(rest, ctxt, cont)
      )
    )
  end

  defp pp_segment(node, ctxt, cont) do
    val = :cerl.bitstr_val(node)
    size = :cerl.bitstr_size(node)
    unit = :cerl.bitstr_unit(node)
    type = :cerl.bitstr_type(node)
    flags = :cerl.bitstr_flags(node)

    restPP =
      case {concrete(unit), concrete(type), concrete(flags)} do
        {1, :integer, [:unsigned, :big]} ->
          case concrete(size) do
            8 ->
              :prettypr.text('')

            _ ->
              pp_size(size, ctxt, cont)
          end

        {8, :binary, [:unsigned, :big]} ->
          sizePP = pp_size(size, ctxt, cont)

          :prettypr.beside(
            sizePP,
            :prettypr.beside(
              :prettypr.text('/'),
              pp_atom(type)
            )
          )

        _What ->
          sizePP = pp_size(size, ctxt, cont)
          unitPP = pp_unit(unit, ctxt, cont)
          optsPP = pp_opts(type, flags)

          :prettypr.beside(
            sizePP,
            :prettypr.beside(optsPP, unitPP)
          )
      end

    :prettypr.beside(cont.(val, ctxt), restPP)
  end

  defp concrete(cerl) do
    try do
      :cerl.concrete(cerl)
    catch
      _, _ ->
        :anything_unexpected
    end
  end

  defp pp_size(size, ctxt, cont) do
    case :cerl.is_c_atom(size) do
      true ->
        :prettypr.text('')

      false ->
        :prettypr.beside(:prettypr.text(':'), cont.(size, ctxt))
    end
  end

  defp pp_opts(type, flags) do
    finalFlags =
      case :cerl.atom_val(type) do
        :binary ->
          []

        :float ->
          keep_endian(:cerl.concrete(flags))

        :integer ->
          keep_all(:cerl.concrete(flags))

        :utf8 ->
          []

        :utf16 ->
          []

        :utf32 ->
          []
      end

    :prettypr.beside(
      :prettypr.text('/'),
      :prettypr.beside(pp_atom(type), pp_flags(finalFlags))
    )
  end

  defp pp_flags([]) do
    :prettypr.text('')
  end

  defp pp_flags([flag | flags]) do
    :prettypr.beside(
      :prettypr.text('-'),
      :prettypr.beside(pp_atom(flag), pp_flags(flags))
    )
  end

  defp keep_endian(flags) do
    for x <- flags,
        :erlang.or(x === :little, x === :native) do
      :cerl.c_atom(x)
    end
  end

  defp keep_all(flags) do
    for x <- flags,
        :erlang.or(
          :erlang.or(x === :little, x === :native),
          x === :signed
        ) do
      :cerl.c_atom(x)
    end
  end

  defp pp_unit(unit, ctxt, cont) do
    case :cerl.concrete(unit) do
      n when is_integer(n) ->
        :prettypr.beside(
          :prettypr.text('-'),
          :prettypr.beside(
            :prettypr.text('unit:'),
            cont.(unit, ctxt)
          )
        )

      _ ->
        :prettypr.text('')
    end
  end

  defp pp_atom(atom) do
    string = :erlang.atom_to_list(:cerl.atom_val(atom))
    :prettypr.text(string)
  end

  defp pp_map(node, ctxt, cont) do
    arg = :cerl.map_arg(node)

    before =
      case :cerl.is_c_map_empty(arg) do
        true ->
          :prettypr.floating(:prettypr.text('\#{'))

        false ->
          :prettypr.beside(
            cont.(arg, ctxt),
            :prettypr.floating(:prettypr.text('\#{'))
          )
      end

    :prettypr.beside(
      before,
      :prettypr.beside(
        :prettypr.par(
          seq(:cerl.map_es(node), :prettypr.floating(:prettypr.text(',')), ctxt, cont)
        ),
        :prettypr.floating(:prettypr.text('}'))
      )
    )
  end

  defp seq([h | t], separator, ctxt, fun) do
    case t do
      [] ->
        [fun.(h, ctxt)]

      _ ->
        [:prettypr.beside(fun.(h, ctxt), separator) | seq(t, separator, ctxt, fun)]
    end
  end

  defp seq([], _, _, _) do
    [:prettypr.empty()]
  end

  defp cerl_binary_segments(r_c_literal(val: b)) when is_bitstring(b) do
    segs_from_bitstring(b)
  end

  defp cerl_binary_segments(cBinary) do
    :cerl.binary_segments(cBinary)
  end

  defp segs_from_bitstring(<<h, t::bitstring>>) do
    [
      r_c_bitstr(
        val: r_c_literal(val: h),
        size: r_c_literal(val: 8),
        unit: r_c_literal(val: 1),
        type: r_c_literal(val: :integer),
        flags: r_c_literal(val: [:unsigned, :big])
      )
      | segs_from_bitstring(t)
    ]
  end

  defp segs_from_bitstring(<<>>) do
    []
  end

  defp segs_from_bitstring(bitstring) do
    n = bit_size(bitstring)
    <<i::size(n)>> = bitstring

    [
      r_c_bitstr(
        val: r_c_literal(val: i),
        size: r_c_literal(val: n),
        unit: r_c_literal(val: 1),
        type: r_c_literal(val: :integer),
        flags: r_c_literal(val: [:unsigned, :big])
      )
    ]
  end

  def refold_pattern(pat) do
    case :cerl.is_literal(pat) and find_map(:cerl.concrete(pat)) do
      true ->
        tree = refold_concrete_pat(:cerl.concrete(pat))
        patAnn = :cerl.get_ann(pat)

        case :proplists.is_defined(:label, patAnn) do
          true ->
            :cerl.set_ann(tree, patAnn)

          false ->
            [{:label, label}] = :cerl.get_ann(tree)
            :cerl.set_ann(tree, [{:label, label} | patAnn])
        end

      false ->
        pat
    end
  end

  defp find_map(%{}) do
    true
  end

  defp find_map(tuple) when is_tuple(tuple) do
    find_map(:erlang.tuple_to_list(tuple))
  end

  defp find_map([h | t]) do
    find_map(h) or find_map(t)
  end

  defp find_map(_) do
    false
  end

  defp refold_concrete_pat(val) do
    case val do
      _ when is_tuple(val) ->
        els =
          :lists.map(
            &refold_concrete_pat/1,
            :erlang.tuple_to_list(val)
          )

        case :lists.all(&:cerl.is_literal/1, els) do
          true ->
            :cerl.abstract(val)

          false ->
            label(:cerl.c_tuple_skel(els))
        end

      [h | t] ->
        case :erlang.and(
               :cerl.is_literal(hP = refold_concrete_pat(h)),
               :cerl.is_literal(tP = refold_concrete_pat(t))
             ) do
          true ->
            :cerl.abstract(val)

          false ->
            label(:cerl.c_cons_skel(hP, tP))
        end

      m when is_map(m) ->
        label(
          :cerl.c_map_pattern(
            for {k, v} <- :maps.to_list(m) do
              :cerl.c_map_pair_exact(
                :cerl.abstract(k),
                refold_concrete_pat(v)
              )
            end
          )
        )

      _ ->
        :cerl.abstract(val)
    end
  end

  defp label(tree) do
    label = -:erlang.unique_integer([:positive])
    :cerl.set_ann(tree, [{:label, label}])
  end

  def ets_tab2list(t) do
    f = fn vs, a ->
      vs ++ a
    end

    ets_take(:ets.first(t), t, f, [])
  end

  def ets_move(t1, t2) do
    f = fn es, a ->
      true = :ets.insert(t2, es)
      a
    end

    [] = ets_take(:ets.first(t1), t1, f, [])
    :ok
  end

  defp ets_take(:"$end_of_table", t, f, a) do
    case :ets.first(t) do
      :"$end_of_table" ->
        a

      key ->
        ets_take(key, t, f, a)
    end
  end

  defp ets_take(key, t, f, a) do
    vs = :ets.lookup(t, key)
    key1 = :ets.next(t, key)
    true = :ets.delete(t, key)
    ets_take(key1, t, f, f.(vs, a))
  end

  def parallelism() do
    :erlang.system_info(:schedulers_online)
  end

  def family(l) do
    :sofs.to_external(:sofs.rel2fam(:sofs.relation(l)))
  end
end
