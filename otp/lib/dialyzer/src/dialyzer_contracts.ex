defmodule :m_dialyzer_contracts do
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

  Record.defrecord(:r_tmp_contract, :tmp_contract,
    contract_funs: [],
    forms: []
  )

  def get_contract_return(r_contract(contracts: cs, args: genArgs)) do
    process_contracts(cs, genArgs)
  end

  def get_contract_return(r_contract(contracts: cs), args) do
    process_contracts(cs, args)
  end

  def get_contract_args(r_contract(args: args)) do
    args
  end

  defp get_contract_signature(r_contract(contracts: cs, args: generalDomain)) do
    range = process_contracts(cs, generalDomain)
    :erl_types.t_fun(generalDomain, range)
  end

  def is_overloaded(r_contract(contracts: cs)) do
    case cs do
      [_] ->
        true

      [[_, _] | _] ->
        false
    end
  end

  def contract_to_string(r_contract(forms: forms)) do
    contract_to_string_1(forms)
  end

  defp contract_to_string_1([{contract, []}]) do
    strip_fun(:erl_types.t_form_to_string(contract))
  end

  defp contract_to_string_1([{contract, []} | rest]) do
    strip_fun(:erl_types.t_form_to_string(contract)) ++ '\n    ; ' ++ contract_to_string_1(rest)
  end

  defp contract_to_string_1([{contract, constraints}]) do
    strip_fun(:erl_types.t_form_to_string(contract)) ++
      ' when ' ++ constraints_to_string(constraints)
  end

  defp contract_to_string_1([{contract, constraints} | rest]) do
    strip_fun(:erl_types.t_form_to_string(contract)) ++
      ' when ' ++ constraints_to_string(constraints) ++ ';' ++ contract_to_string_1(rest)
  end

  defp strip_fun('fun(' ++ string) do
    butlast(string)
  end

  defp butlast([]) do
    []
  end

  defp butlast([_]) do
    []
  end

  defp butlast([h | t]) do
    [h | butlast(t)]
  end

  defp constraints_to_string([]) do
    ''
  end

  defp constraints_to_string([
         {:type, _, :constraint, [{:atom, _, what}, types]}
         | rest
       ]) do
    s = constraint_to_string(what, types)

    case rest do
      [] ->
        s

      _ ->
        s ++ ', ' ++ constraints_to_string(rest)
    end
  end

  defp constraint_to_string(:is_subtype, [{:var, _, var}, t]) do
    :erlang.atom_to_list(var) ++ ' :: ' ++ :erl_types.t_form_to_string(t)
  end

  defp constraint_to_string(what, types) do
    :erlang.atom_to_list(what) ++
      '(' ++
      sequence(
        for t <- types do
          :erl_types.t_form_to_string(t)
        end,
        ','
      ) ++ ')'
  end

  defp sequence([], _Delimiter) do
    ''
  end

  defp sequence([h], _Delimiter) do
    h
  end

  defp sequence([h | t], delimiter) do
    h ++ delimiter ++ sequence(t, delimiter)
  end

  def process_contract_remote_types(codeServer) do
    mods = :dialyzer_codeserver.all_temp_modules(codeServer)
    recordTable = :dialyzer_codeserver.get_records_table(codeServer)
    expTypes = :dialyzer_codeserver.get_exported_types(codeServer)

    moduleFun = fn moduleName ->
      contractFun = fn {mFA, {file, tmpContract, xtra}}, c0 ->
        r_tmp_contract(
          contract_funs: cFuns,
          forms: forms
        ) = tmpContract

        {newCs, c2} =
          :lists.mapfoldl(
            fn cFun, c1 ->
              cFun.(
                expTypes,
                recordTable,
                c1
              )
            end,
            c0,
            cFuns
          )

        args = general_domain(newCs)
        contract = r_contract(contracts: newCs, args: args, forms: forms)
        {{mFA, {file, contract, xtra}}, c2}
      end

      cache = :erl_types.cache__new()

      {contractMap, callbackMap} =
        :dialyzer_codeserver.get_temp_contracts(
          moduleName,
          codeServer
        )

      {newContractList, cache1} =
        :lists.mapfoldl(
          contractFun,
          cache,
          :maps.to_list(contractMap)
        )

      {newCallbackList, _NewCache} =
        :lists.mapfoldl(contractFun, cache1, :maps.to_list(callbackMap))

      :dialyzer_codeserver.store_contracts(
        moduleName,
        :maps.from_list(newContractList),
        :maps.from_list(newCallbackList),
        codeServer
      )
    end

    :lists.foreach(moduleFun, mods)
    :dialyzer_codeserver.finalize_contracts(codeServer)
  end

  def check_contracts(contracts, callgraph, funTypes, modOpaques) do
    foldFun = fn {label, type}, newContracts ->
      case :dialyzer_callgraph.lookup_name(
             label,
             callgraph
           ) do
        {:ok, {m, f, a} = mFA} ->
          case :orddict.find(mFA, contracts) do
            {:ok, contract} ->
              {^m, opaques} = :lists.keyfind(m, 1, modOpaques)

              case check_contract(contract, type, opaques) do
                :ok ->
                  case :erl_bif_types.is_known(m, f, a) do
                    true ->
                      newContracts

                    false ->
                      [{mFA, contract} | newContracts]
                  end

                {:range_warnings, _} ->
                  [{mFA, contract} | newContracts]

                {:error, _Error} ->
                  newContracts
              end

            :error ->
              newContracts
          end

        :error ->
          newContracts
      end
    end

    :orddict.from_list(:lists.foldl(foldFun, [], :orddict.to_list(funTypes)))
  end

  def check_contract(contract, succType) do
    check_contract(contract, succType, :universe)
  end

  defp check_contract(r_contract(contracts: contracts), succType, opaques) do
    try do
      contracts1 =
        for {contract, constraints} <- contracts do
          {contract, insert_constraints(constraints)}
        end

      contracts2 =
        for {contract, map} <- contracts1 do
          :erl_types.t_subst(contract, map)
        end

      genDomains =
        for c <- contracts2 do
          :erl_types.t_fun_args(c)
        end

      case check_domains(genDomains) do
        :error ->
          {:error, {:overlapping_contract, []}}

        :ok ->
          infList =
            for contract <- contracts2 do
              {contract, :erl_types.t_inf(contract, succType, opaques)}
            end

          case check_contract_inf_list(infList, succType, opaques) do
            {:error, _} = invalid ->
              invalid

            :ok ->
              case check_extraneous(contracts2, succType) do
                {:error, :invalid_contract} = err ->
                  err

                {:error, {:extra_range, _, _}} = err ->
                  missingError = check_missing(contracts2, succType)
                  {:range_warnings, [err | missingError]}

                :ok ->
                  case check_missing(contracts2, succType) do
                    [] ->
                      :ok

                    errorL ->
                      {:range_warnings, errorL}
                  end
              end
          end
      end
    catch
      {:error, _} = error ->
        error
    end
  end

  defp check_domains([_]) do
    :ok
  end

  defp check_domains([dom | doms]) do
    fun = fn d ->
      :erl_types.any_none_or_unit(
        :erl_types.t_inf_lists(
          dom,
          d
        )
      )
    end

    case :lists.all(fun, doms) do
      true ->
        check_domains(doms)

      false ->
        :error
    end
  end

  defp check_contract_inf_list(list, succType, opaques) do
    case check_contract_inf_list(list, succType, opaques, []) do
      :ok ->
        :ok

      {:error, []} ->
        {:error, :invalid_contract}

      {:error, [{sigRange, contrRange} | _]} ->
        case :erl_types.t_find_opaque_mismatch(sigRange, contrRange, opaques) do
          :error ->
            {:error, :invalid_contract}

          {:ok, _T1, t2} ->
            {:error, {:opaque_mismatch, t2}}
        end
    end
  end

  defp check_contract_inf_list([{contract, funType} | left], succType, opaques, oM) do
    funArgs = :erl_types.t_fun_args(funType)

    case :lists.any(
           &:erl_types.t_is_none_or_unit/1,
           funArgs
         ) do
      true ->
        check_contract_inf_list(left, succType, opaques, oM)

      false ->
        sTRange = :erl_types.t_fun_range(succType)

        case :erl_types.t_is_none_or_unit(sTRange) do
          true ->
            :ok

          false ->
            range = :erl_types.t_fun_range(funType)

            case :erl_types.t_is_none(
                   :erl_types.t_inf(
                     sTRange,
                     range
                   )
                 ) do
              true ->
                cR = :erl_types.t_fun_range(contract)
                newOM = [{sTRange, cR} | oM]
                check_contract_inf_list(left, succType, opaques, newOM)

              false ->
                :ok
            end
        end
    end
  end

  defp check_contract_inf_list([], _SuccType, _Opaques, oM) do
    {:error, oM}
  end

  defp check_extraneous([], _SuccType) do
    :ok
  end

  defp check_extraneous([c | cs], succType) do
    case check_extraneous_1(c, succType) do
      {:error, _} = error ->
        error

      :ok ->
        check_extraneous(cs, succType)
    end
  end

  defp check_extraneous_1(contract, succType) do
    cRng = :erl_types.t_fun_range(contract)
    cRngs = :erl_types.t_elements(cRng)
    sTRng = :erl_types.t_fun_range(succType)
    :ok

    case (for cR <- cRngs,
              :erl_types.t_is_none(:erl_types.t_inf(cR, sTRng)) do
            cR
          end) do
      [] ->
        case bad_extraneous_list(
               cRng,
               sTRng
             ) or
               bad_extraneous_map(
                 cRng,
                 sTRng
               ) do
          true ->
            {:error, :invalid_contract}

          false ->
            :ok
        end

      cRs ->
        {:error, {:extra_range, :erl_types.t_sup(cRs), sTRng}}
    end
  end

  defp bad_extraneous_list(cRng, sTRng) do
    cRngList = list_part(cRng)
    sTRngList = list_part(sTRng)

    case is_not_nil_list(cRngList) and is_not_nil_list(sTRngList) do
      false ->
        false

      true ->
        cRngElements = :erl_types.t_list_elements(cRngList)
        sTRngElements = :erl_types.t_list_elements(sTRngList)
        inf = :erl_types.t_inf(cRngElements, sTRngElements)
        :erl_types.t_is_none(inf)
    end
  end

  defp list_part(type) do
    :erl_types.t_inf(:erl_types.t_list(), type)
  end

  defp is_not_nil_list(type) do
    :erl_types.t_is_list(type) and not :erl_types.t_is_nil(type)
  end

  defp bad_extraneous_map(cRng, sTRng) do
    cRngMap = map_part(cRng)
    sTRngMap = map_part(sTRng)

    not is_empty_map(cRngMap) and not is_empty_map(sTRngMap) and
      is_empty_map(
        :erl_types.t_inf(
          cRngMap,
          sTRngMap
        )
      )
  end

  defp map_part(type) do
    :erl_types.t_inf(:erl_types.t_map(), type)
  end

  defp is_empty_map(type) do
    :erl_types.t_is_equal(type, :erl_types.t_from_term(%{}))
  end

  defp check_missing(contracts, succType) do
    cRanges =
      for c <- contracts do
        :erl_types.t_fun_range(c)
      end

    allCRange = :erl_types.t_sup(cRanges)
    sTRng = :erl_types.t_fun_range(succType)
    sTRngs = :erl_types.t_elements(sTRng)

    case (for sTR <- sTRngs,
              :erl_types.t_is_none(
                :erl_types.t_inf(
                  sTR,
                  allCRange
                )
              ) do
            sTR
          end) do
      [] ->
        []

      sTRs ->
        [{:error, {:missing_range, :erl_types.t_sup(sTRs), allCRange}}]
    end
  end

  defp process_contracts(overContracts, args) do
    process_contracts(overContracts, args, :erl_types.t_none())
  end

  defp process_contracts([overContract | left], args, accRange) do
    newAccRange =
      case process_contract(
             overContract,
             args
           ) do
        :error ->
          accRange

        {:ok, range} ->
          :erl_types.t_sup(accRange, range)
      end

    process_contracts(left, args, newAccRange)
  end

  defp process_contracts([], _Args, accRange) do
    accRange
  end

  defp process_contract({contract, constraints}, callTypes0) do
    callTypesFun =
      :erl_types.t_fun(
        callTypes0,
        :erl_types.t_any()
      )

    contArgsFun =
      :erl_types.t_fun(
        :erl_types.t_fun_args(contract),
        :erl_types.t_any()
      )

    :ok

    case solve_constraints(contArgsFun, callTypesFun, constraints) do
      {:ok, varMap} ->
        {:ok,
         :erl_types.t_subst(
           :erl_types.t_fun_range(contract),
           varMap
         )}

      :error ->
        :error
    end
  end

  defp solve_constraints(contract, call, constraints) do
    cMap = insert_constraints(constraints)
    contract1 = :erl_types.t_subst(contract, cMap)
    contrArgs = :erl_types.t_fun_args(contract1)
    callArgs = :erl_types.t_fun_args(call)
    infList = :erl_types.t_inf_lists(contrArgs, callArgs)

    case :erl_types.any_none_or_unit(infList) do
      true ->
        :error

      false ->
        {:ok, cMap}
    end
  end

  def contracts_without_fun(contracts, allFuns0, callgraph) do
    allFuns1 =
      for {label, arity} <- allFuns0 do
        {:dialyzer_callgraph.lookup_name(label, callgraph), arity}
      end

    allFuns2 =
      for {{:ok, {m, f, _}}, a} <- allFuns1 do
        {m, f, a}
      end

    allContractMFAs = :maps.keys(contracts)
    errorContractMFAs = allContractMFAs -- allFuns2

    for mFA <- errorContractMFAs do
      warn_spec_missing_fun(mFA, contracts)
    end
  end

  defp warn_spec_missing_fun({m, f, a} = mFA, contracts) do
    {{file, line}, _Contract, _Xtra} =
      :maps.get(
        mFA,
        contracts
      )

    warningInfo = {file, line, mFA}
    {:warn_contract_syntax, warningInfo, {:spec_missing_fun, [m, f, a]}}
  end

  defp insert_constraints(constraints) do
    insert_constraints(constraints, :maps.new())
  end

  defp insert_constraints([{:subtype, type1, type2} | left], map) do
    case :erl_types.t_is_var(type1) do
      true ->
        name = :erl_types.t_var_name(type1)

        map1 =
          case :maps.find(name, map) do
            :error ->
              :maps.put(name, type2, map)

            {:ok, varType} ->
              :maps.put(name, :erl_types.t_inf(varType, type2), map)
          end

        insert_constraints(left, map1)

      false ->
        throw(
          {:error,
           :io_lib.format(
             'First argument of is_subtype constraint must be a type variable: ~tp\n',
             [type1]
           )}
        )
    end
  end

  defp insert_constraints([], map) do
    map
  end

  def store_tmp_contract(module, mFA, fileLine, {typeSpec, xtra}, specMap, recordsDict) do
    tmpContract = contract_from_form(typeSpec, module, mFA, recordsDict, fileLine)
    :maps.put(mFA, {fileLine, tmpContract, xtra}, specMap)
  end

  defp contract_from_form(forms, module, mFA, recDict, fileLine) do
    {cFuns, forms1} = contract_from_form(forms, module, mFA, recDict, fileLine, [], [])
    r_tmp_contract(contract_funs: cFuns, forms: forms1)
  end

  defp contract_from_form(
         [{:type, _, :fun, [_, _]} = form | left],
         module,
         mFA,
         recDict,
         fileLine,
         typeAcc,
         formAcc
       ) do
    typeFun = fn expTypes, recordTable, cache ->
      {newType, newCache} =
        try do
          from_form_with_check(form, expTypes, module, mFA, recordTable, cache)
        catch
          {:error, msg} ->
            {file, line} = fileLine

            newMsg =
              :io_lib.format(
                '~ts:~p: ~ts',
                [:filename.basename(file), line, msg]
              )

            throw({:error, newMsg})
        end

      newTypeNoVars = :erl_types.subst_all_vars_to_any(newType)
      {{newTypeNoVars, []}, newCache}
    end

    newTypeAcc = [typeFun | typeAcc]
    newFormAcc = [{form, []} | formAcc]
    contract_from_form(left, module, mFA, recDict, fileLine, newTypeAcc, newFormAcc)
  end

  defp contract_from_form(
         [
           {:type, _L1, :bounded_fun, [{:type, _L2, :fun, [_, _]} = form, constr]}
           | left
         ],
         module,
         mFA,
         recDict,
         fileLine,
         typeAcc,
         formAcc
       ) do
    typeFun = fn expTypes, recordTable, cache ->
      {constr1, varTable, cache1} =
        process_constraints(constr, module, mFA, recDict, expTypes, recordTable, cache)

      {newType, newCache} =
        from_form_with_check(form, expTypes, module, mFA, recordTable, varTable, cache1)

      newTypeNoVars = :erl_types.subst_all_vars_to_any(newType)
      {{newTypeNoVars, constr1}, newCache}
    end

    newTypeAcc = [typeFun | typeAcc]
    newFormAcc = [{form, constr} | formAcc]
    contract_from_form(left, module, mFA, recDict, fileLine, newTypeAcc, newFormAcc)
  end

  defp contract_from_form([], _Mod, _MFA, _RecDict, _FileLine, typeAcc, formAcc) do
    {:lists.reverse(typeAcc), :lists.reverse(formAcc)}
  end

  defp process_constraints(constrs, module, mFA, recDict, expTypes, recordTable, cache) do
    {init0, newCache} =
      initialize_constraints(constrs, module, mFA, recDict, expTypes, recordTable, cache)

    init = remove_cycles(init0)
    constraints_fixpoint(init, module, mFA, recDict, expTypes, recordTable, newCache)
  end

  defp initialize_constraints(constrs, module, mFA, recDict, expTypes, recordTable, cache) do
    initialize_constraints(constrs, module, mFA, recDict, expTypes, recordTable, cache, [])
  end

  defp initialize_constraints([], _Module, _MFA, _RecDict, _ExpTypes, _RecordTable, cache, acc) do
    {acc, cache}
  end

  defp initialize_constraints(
         [constr | rest],
         module,
         mFA,
         recDict,
         expTypes,
         recordTable,
         cache,
         acc
       ) do
    case constr do
      {:type, _, :constraint, [{:atom, _, :is_subtype}, [type1, type2]]} ->
        varTable = :erl_types.var_table__new()
        {t1, newCache} = final_form(type1, expTypes, module, mFA, recordTable, varTable, cache)
        entry = {t1, type2}

        initialize_constraints(rest, module, mFA, recDict, expTypes, recordTable, newCache, [
          entry | acc
        ])

      {:type, _, :constraint, [{:atom, _, name}, list]} ->
        n = length(list)
        throw({:error, :io_lib.format('Unsupported type guard ~tw/~w\n', [name, n])})
    end
  end

  defp constraints_fixpoint(constrs, module, mFA, recDict, expTypes, recordTable, cache) do
    varTable = :erl_types.var_table__new()

    {varTab, newCache} =
      constraints_to_dict(constrs, module, mFA, recDict, expTypes, recordTable, varTable, cache)

    constraints_fixpoint(varTab, module, mFA, constrs, recDict, expTypes, recordTable, newCache)
  end

  defp constraints_fixpoint(
         oldVarTab,
         module,
         mFA,
         constrs,
         recDict,
         expTypes,
         recordTable,
         cache
       ) do
    {newVarTab, newCache} =
      constraints_to_dict(constrs, module, mFA, recDict, expTypes, recordTable, oldVarTab, cache)

    case newVarTab do
      ^oldVarTab ->
        fun = fn key, value, acc ->
          [{:subtype, :erl_types.t_var(key), value} | acc]
        end

        finalConstrs = :maps.fold(fun, [], newVarTab)
        {finalConstrs, newVarTab, newCache}

      _Other ->
        constraints_fixpoint(
          newVarTab,
          module,
          mFA,
          constrs,
          recDict,
          expTypes,
          recordTable,
          newCache
        )
    end
  end

  defp final_form(form, expTypes, module, mFA, recordTable, varTable, cache) do
    from_form_with_check(form, expTypes, module, mFA, recordTable, varTable, cache)
  end

  defp from_form_with_check(form, expTypes, module, mFA, recordTable, cache) do
    varTable = :erl_types.var_table__new()
    from_form_with_check(form, expTypes, module, mFA, recordTable, varTable, cache)
  end

  defp from_form_with_check(form, expTypes, module, mFA, recordTable, varTable, cache) do
    {_, f, a} = mFA
    site = {:spec, {module, f, a}}
    c1 = :erl_types.t_check_record_fields(form, expTypes, site, recordTable, varTable, cache)
    :erl_types.t_from_form(form, expTypes, site, recordTable, varTable, c1)
  end

  defp constraints_to_dict(constrs, module, mFA, recDict, expTypes, recordTable, varTab, cache) do
    {subtypes, newCache} =
      constraints_to_subs(constrs, module, mFA, recDict, expTypes, recordTable, varTab, cache, [])

    {insert_constraints(subtypes), newCache}
  end

  defp constraints_to_subs(
         [],
         _Module,
         _MFA,
         _RecDict,
         _ExpTypes,
         _RecordTable,
         _VarTab,
         cache,
         acc
       ) do
    {acc, cache}
  end

  defp constraints_to_subs(
         [{t1, form2} | rest],
         module,
         mFA,
         recDict,
         expTypes,
         recordTable,
         varTab,
         cache,
         acc
       ) do
    {t2, newCache} = final_form(form2, expTypes, module, mFA, recordTable, varTab, cache)
    newAcc = [{:subtype, t1, t2} | acc]

    constraints_to_subs(
      rest,
      module,
      mFA,
      recDict,
      expTypes,
      recordTable,
      varTab,
      newCache,
      newAcc
    )
  end

  defp remove_cycles(constrs0) do
    uses = find_uses(constrs0)
    g = :digraph.new()

    vs0 =
      for {v, _} <- uses do
        v
      end ++
        for {_, v} <- uses do
          v
        end

    vs = :lists.usort(vs0)

    :lists.foreach(
      fn v ->
        _ = :digraph.add_vertex(g, v)
      end,
      vs
    )

    :lists.foreach(
      fn {from, to} ->
        _ = :digraph.add_edge(g, {from, to}, from, to, [])
      end,
      uses
    )

    :ok = remove_cycles(g, vs)

    toRemove =
      :ordsets.subtract(
        :ordsets.from_list(uses),
        :ordsets.from_list(:digraph.edges(g))
      )

    constrs = remove_uses(toRemove, constrs0)
    :digraph.delete(g)
    constrs
  end

  defp find_uses([{var, form} | constrs]) do
    usedVars = form_vars(form, [])
    varName = :erl_types.t_var_name(var)

    for usedVar <- usedVars do
      {varName, usedVar}
    end ++ find_uses(constrs)
  end

  defp find_uses([]) do
    []
  end

  defp form_vars({:var, _, :_}, vs) do
    vs
  end

  defp form_vars({:var, _, v}, vs) do
    [v | vs]
  end

  defp form_vars(t, vs) when is_tuple(t) do
    form_vars(:erlang.tuple_to_list(t), vs)
  end

  defp form_vars([e | es], vs) do
    form_vars(es, form_vars(e, vs))
  end

  defp form_vars(_, vs) do
    vs
  end

  defp remove_cycles(g, vs) do
    numberOfEdges = :digraph.no_edges(g)

    :lists.foreach(
      fn v ->
        case :digraph.get_cycle(g, v) do
          false ->
            true

          [^v] ->
            :digraph.del_edge(g, {v, v})

          [[^v, v1] | _] ->
            :digraph.del_edge(g, {v, v1})
        end
      end,
      vs
    )

    case :digraph.no_edges(g) === numberOfEdges do
      true ->
        :ok

      false ->
        remove_cycles(g, vs)
    end
  end

  defp remove_uses([], constrs) do
    constrs
  end

  defp remove_uses([{var, use} | toRemove], constrs0) do
    constrs = remove_uses(var, use, constrs0)
    remove_uses(toRemove, constrs)
  end

  defp remove_uses(_Var, _Use, []) do
    []
  end

  defp remove_uses(var, use, [constr | constrs]) do
    {v, form} = constr

    newConstr =
      case :erl_types.t_var_name(v) === var do
        true ->
          {v, remove_use(form, use)}

        false ->
          constr
      end

    [newConstr | remove_uses(var, use, constrs)]
  end

  defp remove_use({:var, l, v}, v) do
    {:var, l, :_}
  end

  defp remove_use(t, v) when is_tuple(t) do
    :erlang.list_to_tuple(
      remove_use(
        :erlang.tuple_to_list(t),
        v
      )
    )
  end

  defp remove_use([e | es], v) do
    [remove_use(e, v) | remove_use(es, v)]
  end

  defp remove_use(t, _V) do
    t
  end

  defp general_domain(list) do
    general_domain(list, :erl_types.t_none())
  end

  defp general_domain([{sig, constraints} | left], accSig) do
    map = insert_constraints(constraints)
    sig1 = :erl_types.t_subst(sig, map)
    general_domain(left, :erl_types.t_sup(accSig, sig1))
  end

  defp general_domain([], accSig) do
    accSig1 = :erl_types.subst_all_vars_to_any(accSig)
    :erl_types.t_fun_args(accSig1)
  end

  def get_invalid_contract_warnings(modules, codeServer, plt) do
    get_invalid_contract_warnings_modules(modules, codeServer, plt, [])
  end

  defp get_invalid_contract_warnings_modules([mod | mods], codeServer, plt, acc) do
    contracts1 =
      :dialyzer_codeserver.lookup_mod_contracts(
        mod,
        codeServer
      )

    newAcc =
      case :maps.size(contracts1) === 0 do
        true ->
          acc

        false ->
          contracts2 = :maps.to_list(contracts1)

          records =
            :dialyzer_codeserver.lookup_mod_records(
              mod,
              codeServer
            )

          opaques = :erl_types.t_opaque_from_records(records)
          get_invalid_contract_warnings_funs(contracts2, plt, records, opaques, acc)
      end

    get_invalid_contract_warnings_modules(mods, codeServer, plt, newAcc)
  end

  defp get_invalid_contract_warnings_modules([], _CodeServer, _Plt, acc) do
    acc
  end

  defp get_invalid_contract_warnings_funs(
         [{mFA, {fileLine, contract, _Xtra}} | left],
         plt,
         recDict,
         opaques,
         acc
       ) do
    case :dialyzer_plt.lookup(plt, mFA) do
      :none ->
        get_invalid_contract_warnings_funs(left, plt, recDict, opaques, acc)

      {:value, {ret, args}} ->
        sig = :erl_types.t_fun(args, ret)
        {m, _F, _A} = mFA
        {file, line} = fileLine
        warningInfo = {file, line, mFA}

        newAcc =
          case check_contract(contract, sig, opaques) do
            {:error, :invalid_contract} ->
              [
                invalid_contract_warning(mFA, warningInfo, sig, recDict)
                | acc
              ]

            {:error, {:opaque_mismatch, t2}} ->
              w = contract_opaque_warning(mFA, warningInfo, t2, sig, recDict)
              [w | acc]

            {:error, {:overlapping_contract, []}} ->
              [overlapping_contract_warning(mFA, warningInfo) | acc]

            {:range_warnings, errors} ->
              fun = fn
                {:error, {:extra_range, extraRanges, sTRange}}, acc0 ->
                  warn =
                    case t_from_forms_without_remote(
                           r_contract(contract, :forms),
                           mFA,
                           recDict
                         ) do
                      {:ok, noRemoteType} ->
                        cRet = :erl_types.t_fun_range(noRemoteType)

                        :erl_types.t_is_subtype(
                          extraRanges,
                          cRet
                        )

                      :unsupported ->
                        true
                    end

                  case warn do
                    true ->
                      [
                        extra_range_warning(mFA, warningInfo, extraRanges, sTRange)
                        | acc0
                      ]

                    false ->
                      acc0
                  end

                {:error, {:missing_range, extraRanges, cRange}}, acc0 ->
                  [
                    missing_range_warning(mFA, warningInfo, extraRanges, cRange)
                    | acc0
                  ]
              end

              :lists.foldl(fun, acc, errors)

            {:error, msg} ->
              [{:warn_contract_syntax, warningInfo, msg} | acc]

            :ok ->
              {^m, f, a} = mFA
              cSig0 = get_contract_signature(contract)
              cSig = :erl_types.subst_all_vars_to_any(cSig0)

              case :erl_bif_types.is_known(m, f, a) do
                true ->
                  bifArgs = :erl_bif_types.arg_types(m, f, a)
                  bifRet = :erl_bif_types.type(m, f, a)
                  bifSig = :erl_types.t_fun(bifArgs, bifRet)

                  case check_contract(contract, bifSig, opaques) do
                    {:error, _} ->
                      [
                        invalid_contract_warning(mFA, warningInfo, bifSig, recDict)
                        | acc
                      ]

                    {:range_warnings, _} ->
                      picky_contract_check(cSig, bifSig, mFA, warningInfo, contract, recDict, acc)

                    :ok ->
                      picky_contract_check(cSig, bifSig, mFA, warningInfo, contract, recDict, acc)
                  end

                false ->
                  picky_contract_check(cSig, sig, mFA, warningInfo, contract, recDict, acc)
              end
          end

        get_invalid_contract_warnings_funs(left, plt, recDict, opaques, newAcc)
    end
  end

  defp get_invalid_contract_warnings_funs([], _Plt, _RecDict, _Opaques, acc) do
    acc
  end

  defp invalid_contract_warning({m, f, a}, warningInfo, succType, recDict) do
    succTypeStr =
      :dialyzer_utils.format_sig(
        succType,
        recDict
      )

    {:warn_contract_types, warningInfo, {:invalid_contract, [m, f, a, succTypeStr]}}
  end

  defp contract_opaque_warning({m, f, a}, warningInfo, opType, succType, recDict) do
    opaqueStr = :erl_types.t_to_string(opType)

    succTypeStr =
      :dialyzer_utils.format_sig(
        succType,
        recDict
      )

    {:warn_contract_types, warningInfo,
     {:contract_with_opaque, [m, f, a, opaqueStr, succTypeStr]}}
  end

  defp overlapping_contract_warning({m, f, a}, warningInfo) do
    {:warn_contract_types, warningInfo, {:overlapping_contract, [m, f, a]}}
  end

  defp extra_range_warning({m, f, a}, warningInfo, extraRanges, sTRange) do
    eRangesStr = :erl_types.t_to_string(extraRanges)
    sTRangeStr = :erl_types.t_to_string(sTRange)
    {:warn_contract_supertype, warningInfo, {:extra_range, [m, f, a, eRangesStr, sTRangeStr]}}
  end

  defp missing_range_warning({m, f, a}, warningInfo, extraRanges, cRange) do
    eRangesStr = :erl_types.t_to_string(extraRanges)
    cRangeStr = :erl_types.t_to_string(cRange)
    {:warn_contract_subtype, warningInfo, {:missing_range, [m, f, a, eRangesStr, cRangeStr]}}
  end

  defp picky_contract_check(cSig0, sig0, mFA, warningInfo, contract, recDict, acc) do
    cSig = :erl_types.t_abstract_records(cSig0, recDict)
    sig = :erl_types.t_abstract_records(sig0, recDict)

    case :erl_types.t_is_equal(cSig, sig) do
      true ->
        acc

      false ->
        case :erl_types.t_is_none(:erl_types.t_fun_range(sig)) and
               :erl_types.t_is_unit(:erl_types.t_fun_range(cSig)) do
          true ->
            acc

          false ->
            case extra_contract_warning(mFA, warningInfo, contract, cSig0, sig0, recDict) do
              :no_warning ->
                acc

              {:warning, warning} ->
                [warning | acc]
            end
        end
    end
  end

  defp extra_contract_warning(mFA, warningInfo, contract, cSig, sig, recDict) do
    {isRemoteTypesRelated, subtypeRelation} =
      is_remote_types_related(contract, cSig, sig, mFA, recDict)

    case isRemoteTypesRelated do
      true ->
        :no_warning

      false ->
        {m, f, a} = mFA

        sigString =
          :lists.flatten(
            :dialyzer_utils.format_sig(
              sig,
              recDict
            )
          )

        contractString = contract_to_string(contract)

        {tag, msg} =
          case subtypeRelation do
            :contract_is_subtype ->
              {:warn_contract_subtype, {:contract_subtype, [m, f, a, contractString, sigString]}}

            :contract_is_supertype ->
              {:warn_contract_supertype,
               {:contract_supertype, [m, f, a, contractString, sigString]}}

            :neither ->
              {:warn_contract_not_equal, {:contract_diff, [m, f, a, contractString, sigString]}}
          end

        {:warning, {tag, warningInfo, msg}}
    end
  end

  defp is_remote_types_related(contract, cSig, sig, mFA, recDict) do
    case :erl_types.t_is_subtype(cSig, sig) do
      true ->
        {false, :contract_is_subtype}

      false ->
        case :erl_types.t_is_subtype(sig, cSig) do
          true ->
            case t_from_forms_without_remote(r_contract(contract, :forms), mFA, recDict) do
              {:ok, noRemoteTypeSig} ->
                case blame_remote(cSig, noRemoteTypeSig, sig) do
                  true ->
                    {true, :neither}

                  false ->
                    {false, :contract_is_supertype}
                end

              :unsupported ->
                {false, :contract_is_supertype}
            end

          false ->
            {false, :neither}
        end
    end
  end

  defp t_from_forms_without_remote([{fType, []}], mFA, recDict) do
    site = {:spec, mFA}
    type1 = :erl_types.t_from_form_without_remote(fType, site, recDict)
    {:ok, :erl_types.subst_all_vars_to_any(type1)}
  end

  defp t_from_forms_without_remote([{_FType, _Constrs}], _MFA, _RecDict) do
    :unsupported
  end

  defp t_from_forms_without_remote(_Forms, _MFA, _RecDict) do
    :unsupported
  end

  defp blame_remote(contractSig, noRemoteContractSig, sig) do
    cArgs = :erl_types.t_fun_args(contractSig)
    cRange = :erl_types.t_fun_range(contractSig)
    nRArgs = :erl_types.t_fun_args(noRemoteContractSig)
    nRRange = :erl_types.t_fun_range(noRemoteContractSig)
    sArgs = :erl_types.t_fun_args(sig)
    sRange = :erl_types.t_fun_range(sig)
    blame_remote_list([cRange | cArgs], [nRRange | nRArgs], [sRange | sArgs])
  end

  defp blame_remote_list([], [], []) do
    true
  end

  defp blame_remote_list([cArg | cArgs], [nRArg | nRArgs], [sArg | sArgs]) do
    case :erl_types.t_is_equal(cArg, nRArg) do
      true ->
        case not :erl_types.t_is_equal(cArg, sArg) do
          true ->
            false

          false ->
            blame_remote_list(cArgs, nRArgs, sArgs)
        end

      false ->
        case :erl_types.t_is_subtype(sArg, nRArg) and
               not :erl_types.t_is_subtype(
                 nRArg,
                 sArg
               ) do
          true ->
            false

          false ->
            blame_remote_list(cArgs, nRArgs, sArgs)
        end
    end
  end
end
