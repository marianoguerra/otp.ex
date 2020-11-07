defmodule :m_qlc_pt do
  use Bitwise
  require Record

  Record.defrecord(:r_qlc_lc, :qlc_lc,
    lc: :undefined,
    opt: :undefined
  )

  Record.defrecord(:r_state, :state,
    imp: :undefined,
    overridden: :undefined,
    maxargs: :undefined,
    records: :undefined,
    xwarnings: [],
    intro_vars: :undefined,
    node_info: :undefined
  )

  def parse_transform(forms0, options) do
    :ok
    imported = is_qlc_q_imported(forms0)

    {forms, formsNoShadows, state} =
      initiate(
        forms0,
        imported
      )

    nodeInfo = r_state(state, :node_info)

    try do
      case called_from_type_checker(options) do
        true ->
          l = anno0()
          {:tuple, _, fs0} = abstr(r_qlc_lc(), l)

          f = fn _Id, lC, a ->
            init = simple(l, :V, lC, l)
            {{:tuple, l, set_field(r_qlc_lc(:lc), fs0, init)}, a}
          end

          {forms1, :ok} = qlc_mapfold(f, :ok, forms, state)
          forms1

        false ->
          case compile_messages(forms, formsNoShadows, options, state) do
            {[], warnings} ->
              :ok
              {newForms, state1} = transform(formsNoShadows, state)
              extraWs = r_state(state1, :xwarnings)
              {[], wForms} = no_duplicates(newForms, [], warnings, extraWs, options)

              restore_locations(
                wForms,
                state
              ) ++ restore_anno(newForms, nodeInfo)

            {errors, warnings} ->
              :ok
              {eForms, wForms} = no_duplicates(formsNoShadows, errors, warnings, [], options)
              restore_locations(eForms ++ wForms, state) ++ forms0
          end
      end
    after
      true = :ets.delete(nodeInfo)
    end
  end

  def transform_from_evaluator(lC, bindings) do
    :ok
    transform_expression(lC, bindings, false)
  end

  def transform_expression(lC, bindings) do
    transform_expression(lC, bindings, true)
  end

  defp called_from_type_checker(options) do
    :lists.member(:type_checker, options)
  end

  defp transform_expression(lC, bs0, withLintErrors) do
    l = anno1()

    as =
      for {v, _Val} <- bs0 do
        {:var, l, v}
      end

    ar = length(as)

    f =
      {:function, l, :bar, ar,
       [{:clause, l, as, [], [{:call, l, {:remote, l, {:atom, l, :qlc}, {:atom, l, :q}}, [lC]}]}]}

    forms0 = [{:attribute, l, :file, {'foo', l}}, {:attribute, l, :module, :foo}, f]
    {forms, formsNoShadows, state} = initiate(forms0, false)
    nodeInfo = r_state(state, :node_info)
    options = []

    try do
      compile_messages(forms, formsNoShadows, options, state)
    else
      {errors0, _Warnings} ->
        case restore_locations(errors0, state) do
          [] ->
            {newForms, _State1} = transform(formsNoShadows, state)
            newForms1 = restore_anno(newForms, nodeInfo)
            {:function, ^l, :bar, ^ar, [{:clause, ^l, ^as, [], [nF]}]} = :lists.last(newForms1)
            {:ok, nF}

          errors when withLintErrors ->
            {:not_ok, mforms(:error, errors)}

          errors ->
            [{:error, reason} | _] = mforms(:error, errors)
            {:not_ok, {:error, :qlc, reason}}
        end
    after
      true = :ets.delete(nodeInfo)
    end
  end

  defp initiate(forms0, imported) do
    nodeInfo = :ets.new(:qlc, [])
    true = :ets.insert(nodeInfo, {:var_n, 0})

    exclude_integers_from_unique_line_numbers(
      forms0,
      nodeInfo
    )

    :ok
    isOverridden = set_up_overridden(forms0)

    state0 =
      r_state(
        imp: imported,
        overridden: isOverridden,
        maxargs: 20,
        records: record_attributes(forms0),
        node_info: nodeInfo
      )

    forms = save_anno(forms0, nodeInfo)
    formsNoShadows = no_shadows(forms, state0)
    introVars = intro_variables(formsNoShadows, state0)
    state = r_state(state0, intro_vars: introVars)
    {forms, formsNoShadows, state}
  end

  defp exclude_integers_from_unique_line_numbers(forms, nodeInfo) do
    integers = find_integers(forms)

    :lists.foreach(
      fn i ->
        :ets.insert(nodeInfo, {i})
      end,
      integers
    )
  end

  defp find_integers(forms) do
    f = fn a ->
      fs1 =
        map_anno(
          fn _ ->
            a
          end,
          forms
        )

      :ordsets.from_list(integers(fs1, []))
    end

    :ordsets.to_list(
      :ordsets.intersection(
        f.(anno0()),
        f.(anno1())
      )
    )
  end

  defp integers([e | es], l) do
    integers(es, integers(e, l))
  end

  defp integers(t, l) when is_tuple(t) do
    integers(:erlang.tuple_to_list(t), l)
  end

  defp integers(i, l) when is_integer(i) and i > 0 do
    [i | l]
  end

  defp integers(_, l) do
    l
  end

  Record.defrecord(:r_qid, :qid,
    lcid: :undefined,
    no: :undefined
  )

  defp mforms(tag, l) do
    :lists.sort(
      for {_File, ms} <- l, m <- ms do
        {tag, m}
      end
    )
  end

  defp no_duplicates(forms, errors, warnings0, extraWarnings0, options) do
    extraWarnings =
      for w = {_File, [{_, :qlc, tag}]} <- extraWarnings0,
          not :lists.member(
            tag,
            [:nomatch_pattern, :nomatch_filter]
          ) do
        w
      end

    warnings1 =
      mforms(warnings0) --
        (for {file, [{l, :qlc, m}]} <- mforms(extraWarnings),
             :lists.member(
               m,
               [:nomatch_pattern, :nomatch_filter]
             ) do
           {file, [{l, :v3_core, :nomatch}]}
         end ++
           for {file, [{l, :qlc, m}]} <- mforms(extraWarnings),
               m === :nomatch_filter do
             {file, [{l, :sys_core_fold, :nomatch_guard}]}
           end)

    warnings = warnings1 ++ extraWarnings
    {es1, ws1} = compile_forms(forms, options)
    es = mforms(errors) -- mforms(es1)
    ws = mforms(warnings) -- mforms(ws1)
    {mforms2(:error, es), mforms2(:warning, ws)}
  end

  defp mforms(l) do
    :lists.sort(
      for {file, ms} <- l, m <- ms do
        {file, [m]}
      end
    )
  end

  defp mforms2(tag, l) do
    line = anno0()

    mL =
      :lists.flatmap(
        fn {file, ms} ->
          for m <- ms do
            [{:attribute, line, :file, {file, 0}}, {tag, m}]
          end
        end,
        :lists.sort(l)
      )

    :lists.flatten(:lists.sort(mL))
  end

  defp restore_locations([t | ts], state) do
    [
      restore_locations(t, state)
      | restore_locations(
          ts,
          state
        )
    ]
  end

  defp restore_locations(t, state) when is_tuple(t) do
    :erlang.list_to_tuple(
      restore_locations(
        :erlang.tuple_to_list(t),
        state
      )
    )
  end

  defp restore_locations(i, state) when i > 0 do
    restore_loc(i, state)
  end

  defp restore_locations(t, _State) do
    t
  end

  defp is_qlc_q_imported(forms) do
    for {:attribute, _, :import, {:qlc, fAs}} <- forms,
        {:q, 1} <- fAs do
      []
    end !== []
  end

  defp record_attributes(forms) do
    for a = {:attribute, _, :record, _D} <- forms do
      a
    end
  end

  defp compile_messages(forms, formsNoShadows, options, state) do
    bGenF = fn
      _QId, {:b_generate, line, _P, _LE} = bGen, gA, a ->
        m = {loc(line), :qlc, :binary_generator}
        {bGen, [{:erlang.get(:qlc_current_file), [m]} | gA], a}

      _QId, q, gA, a ->
        {q, gA, a}
    end

    {_, bGens} = qual_fold(bGenF, [], [], forms, state)
    genForm = used_genvar_check(formsNoShadows, state)
    :ok
    {gEs, _} = compile_forms([genForm], options)
    usedGenVarMsgs = used_genvar_messages(gEs, state)
    nodeInfo = r_state(state, :node_info)

    warnFun = fn _Id, lC, a ->
      {lc_nodes(lC, nodeInfo), a}
    end

    {wForms, :ok} = qlc_mapfold(warnFun, :ok, forms, state)
    {es, ws} = compile_forms(wForms, options)
    lcEs = lc_messages(es, nodeInfo)
    lcWs = lc_messages(ws, nodeInfo)

    errors =
      badarg(
        forms,
        state
      ) ++ usedGenVarMsgs ++ lcEs ++ bGens

    warnings = lcWs
    {errors, warnings}
  end

  defp badarg(forms, state) do
    f = fn
      _Id, {:lc, _L, _E, _Qs} = lC, es ->
        {lC, es}

      id, a, es ->
        e = {get_lcid_line(id), :qlc, :not_a_query_list_comprehension}
        {a, [{:erlang.get(:qlc_current_file), [e]} | es]}
    end

    {_, e0} = qlc_mapfold(f, [], forms, state)
    e0
  end

  defp lc_nodes(e, nodeInfo) do
    map_anno(
      fn anno ->
        n = :erl_anno.line(anno)
        [{^n, data}] = :ets.lookup(nodeInfo, n)
        nData = %{data | :inside_lc => true}
        true = :ets.insert(nodeInfo, {n, nData})
        anno
      end,
      e
    )
  end

  defp used_genvar_messages(msL, s) do
    for {_, ms} <- msL,
        {xLoc, :erl_lint, {:unbound_var, _}} <- ms,
        {loc, file, v} <- [genvar_pos(xLoc, s)] do
      {file, [{loc, :qlc, {:used_generator_variable, v}}]}
    end
  end

  defp lc_messages(msL, nodeInfo) do
    for {file, ms} <- msL do
      {file,
       for {loc, mod, t} <- ms, lc_loc(loc, nodeInfo) do
         {loc, mod, t}
       end}
    end
  end

  defp lc_loc(n, nodeInfo) do
    case :ets.lookup(nodeInfo, n) do
      [{^n, %{:inside_lc => true}}] ->
        true

      [{^n, _}] ->
        false
    end
  end

  defp genvar_pos(location, s) do
    case :ets.lookup(r_state(s, :node_info), location) do
      [{^location, %{:genvar_pos => pos}}] ->
        pos

      [] ->
        location
    end
  end

  defp intro_variables(formsNoShadows, state) do
    nodeInfo = r_state(state, :node_info)

    fun = fn
      qId, {t, _L, p0, _E0} = q, {gVs, qIds}, foo
      when t === :b_generate or t === :generate ->
        pVs =
          :qlc.var_ufold(
            fn {:var, _, v} ->
              {qId, v}
            end,
            p0
          )

        {q, {:ordsets.to_list(pVs) ++ gVs, [{qId, []} | qIds]}, foo}

      qId, filter0, {gVs, qIds}, foo ->
        vs = :ordsets.to_list(:qlc.vars(filter0))
        anyLine = anno0()

        vars =
          for v <- vs do
            {:var, anyLine, v}
          end

        lC = embed_vars(vars, anyLine)
        lC1 = intro_anno(lC, :before, qId, nodeInfo)
        lC2 = intro_anno(lC, :after, qId, nodeInfo)
        filter = {:block, anyLine, [lC1, filter0, lC2]}
        {filter, {gVs, [{qId, []} | qIds]}, foo}
    end

    acc0 = {[], []}
    {fForms, {genVars, qIds}} = qual_fold(fun, acc0, [], formsNoShadows, state)
    es0 = compile_errors(fForms)

    before =
      for {l, :erl_lint, {:unbound_var, v}} <- es0,
          {_L, {qId, :before}} <- :ets.lookup(nodeInfo, l) do
        {qId, v}
      end

    after__ =
      for {l, :erl_lint, {:unbound_var, v}} <- es0,
          {_L, {qId, :after}} <- :ets.lookup(nodeInfo, l) do
        {qId, v}
      end

    unsafe =
      for {l, :erl_lint, {:unsafe_var, v, _Where}} <- es0,
          {_L, {qId, :after}} <- :ets.lookup(nodeInfo, l) do
        {qId, v}
      end

    :ok
    :ok
    :ok
    :ok
    iV = (before -- after__) -- unsafe
    i1 = family(iV ++ genVars)

    :sofs.to_external(
      :sofs.family_union(
        :sofs.family(qIds),
        i1
      )
    )
  end

  defp intro_anno(lC, where, qId, nodeInfo) do
    data = {qId, where}

    fun = fn anno ->
      location = :erl_anno.location(anno)
      true = :ets.insert(nodeInfo, {location, data})
      anno
    end

    map_anno(fun, save_anno(lC, nodeInfo))
  end

  defp compile_errors(formsNoShadows) do
    case compile_forms(formsNoShadows, []) do
      {[], _Warnings} ->
        []

      {errors, _Warnings} ->
        :ok

        :lists.flatmap(
          fn {_File, es} ->
            es
          end,
          errors
        )
    end
  end

  defp compile_forms(forms0, options) do
    exclude = fn
      :eof ->
        true

      :warning ->
        true

      :error ->
        true

      _ ->
        false
    end

    forms =
      for f <- forms0,
          not exclude.(:erlang.element(1, f)) do
        f
      end ++ [{:eof, 0}]

    try do
      case :compile.noenv_forms(
             forms,
             compile_options(options)
           ) do
        {:ok, _ModName, ws0} ->
          {[], ws0}

        {:error, es0, ws0} ->
          {es0, ws0}
      end
    catch
      _, _ ->
        case :erl_lint.module(forms, lint_options(options)) do
          {:ok, warnings} ->
            {[], warnings}

          {:error, errors, warnings} ->
            {errors, warnings}
        end
    end
  end

  defp compile_options(options) do
    no = [
      [:report, :report_errors, :report_warnings, :P, :E]
      | bitstr_options()
    ]

    [
      [:strong_validation, :return]
      | skip_options(
          no,
          options
        )
    ]
  end

  defp lint_options(options) do
    skip_options(bitstr_options(), options)
  end

  defp skip_options(skip, options) do
    for o <- options, not :lists.member(o, skip) do
      o
    end
  end

  defp bitstr_options() do
    [:binary_comprehension, :bitlevel_binaries]
  end

  defp used_genvar_check(formsNoShadows, state) do
    nodeInfo = r_state(state, :node_info)

    f = fn
      qId, {t, ln, _P, lE} = q, {qsIVs0, exprs0}, iVsSoFar0
      when t === :b_generate or t === :generate ->
        f = fn var ->
          {:var, anno0, origVar} = undo_no_shadows(var, state)
          {:var, anno, _} = newVar = save_anno(var, nodeInfo)
          location0 = :erl_anno.location(anno0)
          location = :erl_anno.location(anno)
          [{^location, data}] = :ets.lookup(nodeInfo, location)
          pos = {location0, :erlang.get(:qlc_current_file), origVar}
          nData = %{data | :genvar_pos => pos}
          true = :ets.insert(nodeInfo, {location, nData})
          newVar
        end

        vs =
          for {:var, _, v} = var <- :qlc.var_fold(f, [], lE),
              :lists.member(v, iVsSoFar0) do
            var
          end

        exprs =
          case vs do
            [] ->
              exprs0

            _ ->
              [embed_vars(vs, ln) | exprs0]
          end

        {qsIVs, iVsSoFar} = q_intro_vars(qId, qsIVs0, iVsSoFar0)
        {q, {qsIVs, exprs}, iVsSoFar}

      qId, filter, {qsIVs0, exprs}, iVsSoFar0 ->
        {qsIVs, iVsSoFar} = q_intro_vars(qId, qsIVs0, iVsSoFar0)
        {filter, {qsIVs, exprs}, iVsSoFar}
    end

    acc0 = {r_state(state, :intro_vars), [{:atom, anno0(), true}]}
    {_, {[], exprs}} = qual_fold(f, acc0, [], formsNoShadows, state)

    functionNames =
      for {:function, _, name, _, _} <- formsNoShadows do
        name
      end

    uniqueFName = :qlc.aux_name(:used_genvar, 1, :gb_sets.from_list(functionNames))
    a = anno0()
    {:function, a, uniqueFName, 0, [{:clause, a, [], [], :lists.reverse(exprs)}]}
  end

  defp q_intro_vars(qId, [{qId, iVs} | qsIVs], iVsSoFar) do
    {qsIVs, iVs ++ iVsSoFar}
  end

  defp transform(formsNoShadows, state) do
    _ = :erlang.system_flag(:backtrace_depth, 500)
    introVars = r_state(state, :intro_vars)
    allVars = :gb_sets.from_list(:ordsets.to_list(:qlc.vars(formsNoShadows)))
    :ok

    f1 = fn
      qId, {:generate, _, p, lE}, foo, {goI, sI} ->
        {{qId, goI, sI, {:gen, p, lE}}, foo, {goI + 3, sI + 2}}

      qId, f, foo, {goI, sI} ->
        {{qId, goI, sI, {:fil, f}}, foo, {goI + 2, sI + 1}}
    end

    templS = :qlc.template_state()
    goState = {templS + 1, templS + 1}
    {modifiedForms1, _} = qual_fold(f1, [], goState, formsNoShadows, state)

    {_, source0} =
      qual_fold(
        fn
          _QId, {:generate, _, _P, _E} = q, dict, foo ->
            {q, dict, foo}

          qId, f, dict, foo ->
            {f, :maps.put(qId, f, dict), foo}
        end,
        :maps.new(),
        [],
        formsNoShadows,
        state
      )

    {_, source} =
      qlc_mapfold(
        fn id, {:lc, _L, e, _Qs} = lC, dict ->
          {lC, :maps.put(id, e, dict)}
        end,
        source0,
        formsNoShadows,
        state
      )

    f2 = fn id, {:lc, _L, e, qs}, {introVs0, xWarn0} ->
      lcNo = get_lcid_no(id)
      lcL = get_lcid_line(id)

      [rL, fun, go, nGV, s0, rL0, go0, aT, err] =
        aux_vars([:RL, :Fun, :Go, :C, :S0, :RL0, :Go0, :AT, :E], lcNo, allVars)

      :ok

      {introVs, restIntroVs} =
        :lists.split(
          length(qs),
          introVs0
        )

      introVs_Qs = :lists.zip(introVs, qs)

      f = fn
        {{qId, iVs}, {qId, goI, sI, {:gen, p, lE}}}, allIVs0 ->
          gV = aux_var(:C, lcNo, r_qid(qId, :no), 1, allVars)
          genIVs = [gV | iVs]
          {{qId, {genIVs, {{:gen, p, lE, gV}, goI, sI}}}, genIVs ++ allIVs0}

        {{qId, iVs}, {qId, goI, sI, {:fil, f}}}, allIVs0 ->
          {{qId, {iVs, {{:fil, f}, goI, sI}}}, iVs ++ allIVs0}
      end

      {qCs, allIVs} = :lists.mapfoldl(f, [], introVs_Qs)
      dependencies = qualifier_dependencies(qs, introVs)
      l = no_compiler_warning(lcL)

      {eqColumnConstants, equalColumnConstants, extraConsts, sizeInfo} =
        constants_and_sizes(qs, e, dependencies, allIVs, state)

      {joinInfo, xWarn} = join_kind(qs, lcL, allIVs, dependencies, state)
      fWarn = warn_failing_qualifiers(qs, allIVs, dependencies, state)
      jQs = join_quals(joinInfo, qCs, l, lcNo, extraConsts, allVars)
      xQCs = qCs ++ jQs
      cs0 = clauses(xQCs, rL, fun, go, nGV, err, allIVs, state)
      template = template(e, rL, fun, go, aT, l, allIVs, state)
      fin = final(rL, allIVs, l, state)
      funC = {:fun, l, {:clauses, fin ++ template ++ cs0}}

      as0 =
        pack_args(
          abst_vars(
            [
              [s0, rL0, fun, go0]
              | replace(allIVs, allIVs, nil)
            ],
            l
          ),
          l,
          state
        )

      asW = abst_vars([s0, rL0, go0], l)

      funW =
        {:fun, l,
         {:clauses,
          [
            {:clause, l, asW, [],
             [{:match, l, {:var, l, fun}, funC}, {:call, l, {:var, l, fun}, as0}]}
          ]}}

      origE0 = :erlang.map_get(id, source)
      origE = undo_no_shadows(origE0, state)
      qCode = qcode(origE, xQCs, source, l, state)
      qdata = qdata(xQCs, l)
      templateInfo = template_columns(qs, e, allIVs, dependencies, state)
      mSQs = match_spec_quals(e, dependencies, qs, state)

      opt =
        opt_info(
          templateInfo,
          sizeInfo,
          joinInfo,
          mSQs,
          l,
          eqColumnConstants,
          equalColumnConstants
        )

      lCTuple =
        case qlc_kind(origE, qs, state) do
          :qlc ->
            {:tuple, l, [{:atom, l, :qlc_v1}, funW, qCode, qdata, opt]}

          {:simple, pL, lE, v} ->
            init = closure(lE, l)
            simple(l, v, init, pL)
        end

      lCFun = {:fun, l, {:clauses, [{:clause, l, [], [], [lCTuple]}]}}
      {:tuple, _, fs0} = abstr(r_qlc_lc(), l)
      fs = set_field(r_qlc_lc(:lc), fs0, lCFun)
      {{:tuple, l, fs}, {restIntroVs, fWarn ++ xWarn ++ xWarn0}}
    end

    {nForms, {[], xW}} = qlc_mapfold(f2, {introVars, []}, modifiedForms1, state)
    display_forms(nForms)
    {nForms, r_state(state, xwarnings: xW)}
  end

  defp join_kind(qs, lcL, allIVs, dependencies, state) do
    {equalCols2, equalColsN} = equal_columns(qs, allIVs, dependencies, state)
    {matchCols2, matchColsN} = eq_columns(qs, allIVs, dependencies, state)

    tables =
      :lists.usort(
        for {c, _Skip} <- equalCols2,
            {t, _} <- c do
          t
        end ++
          for {c, _Skip} <- equalCols2, t <- c, is_integer(t) do
            t
          end
      )

    cond do
      equalColsN !== [] or matchColsN !== [] ->
        {[], [{:erlang.get(:qlc_current_file), [{lcL, :qlc, :too_complex_join}]}]}

      equalCols2 === [] and matchCols2 === [] ->
        {[], []}

      length(tables) > 2 ->
        {[], [{:erlang.get(:qlc_current_file), [{lcL, :qlc, :too_many_joins}]}]}

      equalCols2 === matchCols2 ->
        {equalCols2, []}

      true ->
        {{equalCols2, matchCols2}, []}
    end
  end

  defp qlc_kind(origE, qs, state) do
    {origFilterData, origGeneratorData} =
      qual_data(
        undo_no_shadows(
          qs,
          state
        )
      )

    origAllFilters = filters_as_one(origFilterData)
    {_FilterData, generatorData} = qual_data(qs)

    case {origE, origAllFilters, origGeneratorData} do
      {{:var, _, v}, {:atom, _, true}, [{_, {:gen, {:var, patternL, v}, _LE}}]} ->
        [{_, {:gen, _, lE}}] = generatorData
        {:simple, patternL, lE, v}

      _ ->
        :qlc
    end
  end

  defp warn_failing_qualifiers(qualifiers, allIVs, dependencies, state) do
    {filterData, generatorData} = qual_data(qualifiers)
    anon = 1

    bindFun = fn _Op, value ->
      is_bindable(value)
    end

    {pFrame, _PatternVars} = pattern_frame(generatorData, bindFun, anon, state)
    {_, _, imported} = filter_info(filterData, allIVs, dependencies, state)
    pFrames = frame2frames(pFrame)

    {_, warnings} =
      :lists.foldl(
        fn
          {_QId, {:fil, _Filter}}, {[] = frames, warnings} ->
            {frames, warnings}

          {_QId, {:fil, filter}}, {frames, warnings} ->
            case filter(reset_anno(filter), frames, bindFun, state, imported) do
              [] ->
                {[],
                 [
                   {:erlang.get(:qlc_current_file),
                    [
                      {loc(
                         :erlang.element(
                           2,
                           filter
                         )
                       ), :qlc, :nomatch_filter}
                    ]}
                   | warnings
                 ]}

              frames1 ->
                {frames1, warnings}
            end

          {_QId, {:gen, pattern, _}}, {frames, warnings} ->
            case pattern(pattern, anon, [], bindFun, state) do
              {:failed, _, _} ->
                {frames,
                 [
                   {:erlang.get(:qlc_current_file),
                    [
                      {loc(
                         :erlang.element(
                           2,
                           pattern
                         )
                       ), :qlc, :nomatch_pattern}
                    ]}
                   | warnings
                 ]}

              _ ->
                {frames, warnings}
            end
        end,
        {pFrames, []},
        filterData ++ generatorData
      )

    warnings
  end

  defp opt_info(templateInfo, sizes, joinInfo, mSQs, l, eqColumnConstants0, equalColumnConstants0) do
    szCls =
      for {c, sz} <- :lists.sort(sizes) do
        {:clause, l, [{:integer, l, c}], [], [{:integer, l, sz}]}
      end ++ [{:clause, l, [{:var, l, :_}], [], [{:atom, l, :undefined}]}]

    s = [{:size, {:fun, l, {:clauses, szCls}}}]

    j =
      case joinInfo do
        [] ->
          []

        _ ->
          [{:join, abstr(joinInfo, l)}]
      end

    tCls0 =
      :lists.append(
        for {eqType, templateColumns} <- templateInfo do
          for {col, templCols} <- templateColumns do
            {:clause, l, [abstr(col, l), eqType], [], [abstr(templCols, l)]}
          end
        end
      )

    tCls = :lists.sort(tCls0) ++ [{:clause, l, [{:var, l, :_}, {:var, l, :_}], [], [{nil, l}]}]
    t = [{:template, {:fun, l, {:clauses, tCls}}}]
    eqColumnConstants = opt_column_constants(eqColumnConstants0)
    cCs = opt_constants(l, eqColumnConstants)
    eqC = {:constants, {:fun, l, {:clauses, cCs}}}
    equalColumnConstants = opt_column_constants(equalColumnConstants0)
    eCCs = opt_constants(l, equalColumnConstants)
    equalC = {:equal_constants, {:fun, l, {:clauses, eCCs}}}

    c = [
      eqC
      | for true <- [cCs !== eCCs] do
          equalC
        end
    ]

    constCols =
      for {{idNo, col}, [_], _FilNs} <- equalColumnConstants do
        {idNo, col}
      end

    constColsFamily = family_list(constCols)

    nSortedCols0 =
      for {idNo, cols} <- constColsFamily do
        {idNo, hd(:lists.seq(1, length(cols) + 1) -- cols)}
      end

    nCls =
      for {idNo, n} <- nSortedCols0, n > 0 do
        {:clause, l, [{:integer, l, idNo}], [], [{:integer, l, n - 1}]}
      end ++ [{:clause, l, [{:var, l, :_}], [], [{:integer, l, 0}]}]

    n = [{:n_leading_constant_columns, {:fun, l, {:clauses, nCls}}}]

    constCls =
      for {idNo, cols} <- constColsFamily do
        {:clause, l, [{:integer, l, idNo}], [], [abstr(cols, l)]}
      end ++ [{:clause, l, [{:var, l, :_}], [], [{nil, l}]}]

    cC = [{:constant_columns, {:fun, l, {:clauses, constCls}}}]

    mSCls =
      for {g, mS, fs} <- mSQs do
        {:clause, l, [{:integer, l, g}], [], [{:tuple, l, [mS, abstr(fs, l)]}]}
      end ++ [{:clause, l, [{:var, l, :_}], [], [{:atom, l, :undefined}]}]

    mS = [{:match_specs, {:fun, l, {:clauses, mSCls}}}]

    cls =
      for {tag, v} <- :lists.append([j, s, t, c, n, cC, mS]) do
        {:clause, l, [{:atom, l, tag}], [], [v]}
      end ++ [{:clause, l, [{:var, l, :_}], [], [{:atom, l, :undefined}]}]

    {:fun, l, {:clauses, cls}}
  end

  defp opt_column_constants(columnConstants0) do
    for {{idNo, _Col}, const, _FilNs} = cC <- columnConstants0,
        :erlang.or(idNo !== 0, length(const) === 1) do
      cC
    end
  end

  defp opt_constants(l, columnConstants) do
    ns =
      :lists.usort(
        for {{idNo, _Col}, _Const, _FilNs} <- columnConstants do
          idNo
        end
      )

    for idNo <- ns do
      {:clause, l, [{:integer, l, idNo}], [], [column_fun(columnConstants, idNo, l)]}
    end ++ [{:clause, l, [{:var, l, :_}], [], [{:atom, l, :no_column_fun}]}]
  end

  defp abstr(term, anno) do
    :erl_parse.abstract(term, loc(anno))
  end

  defp join_quals(joinInfo, qCs, l, lcNo, extraConstants, allVars) do
    {lastGoI, lastSI} =
      :lists.foldl(
        fn
          {_QId, {_QIVs, {{:fil, _}, goI, sI}}}, {goI0, _SI0}
          when goI >= goI0 ->
            {goI + 2, sI + 1}

          {_QId, {_QIVs, {{:gen, _, _, _}, goI, sI}}}, {goI0, _SI0}
          when goI >= goI0 ->
            {goI + 3, sI + 2}

          _, a ->
            a
        end,
        {0, 0},
        qCs
      )

    lastQId =
      :lists.max(
        for {qId, {_QIVs, {_Q, _GoI, _SI}}} <- qCs do
          qId
        end
      )

    qNums =
      case joinInfo do
        {equalCols, matchCols} ->
          eQs = join_qnums(equalCols)
          mQs = join_qnums(matchCols)

          for {q1, q2} <- mQs do
            {q1, q2, :"=:="}
          end ++
            for {q1, q2} <- eQs -- mQs do
              {q1, q2, :==}
            end

        equalCols ->
          for {q1, q2} <- join_qnums(equalCols) do
            {q1, q2, :==}
          end
      end

    lD =
      for {q1, q2, op} <- :lists.usort(qNums) do
        [{qId1, p1, gV1, qIVs1}] =
          for {qId, {qIVs, {{:gen, p, _, gV}, _GoI, _SI}}} <- qCs,
              r_qid(qId, :no) === q1 do
            {qId, p, gV, qIVs}
          end

        [{qId2, p2, qIVs2}] =
          for {qId, {qIVs, {{:gen, p, _, gV}, _, _}}} <- qCs,
              r_qid(qId, :no) === q2 do
            {qId, p, qIVs -- [gV]}
          end

        {qId1, op, p1, gV1, qIVs1 ++ qIVs2, qId2, p2}
      end

    aux =
      abst_vars(
        aux_vars([:F, :H, :O, :C], lcNo, allVars),
        l
      )

    f = fn {qId1, op, p1, gV1, qIVs, qId2, p2}, {qId, goI, sI} ->
      aP1 = anon_pattern(p1)
      aP2 = anon_pattern(p2)
      cs1 = join_handle_constants(qId1, extraConstants)
      cs2 = join_handle_constants(qId2, extraConstants)
      h1 = join_handle(aP1, l, aux, cs1)
      h2 = join_handle(aP2, l, aux, cs2)
      join = {:join, op, r_qid(qId1, :no), r_qid(qId2, :no), h1, h2, cs1, cs2}

      g =
        {nQId = r_qid(qId, no: r_qid(qId, :no) + 1),
         {qIVs, {{:gen, {:cons, l, p1, p2}, join, gV1}, goI, sI}}}

      a = {nQId, goI + 3, sI + 2}
      {g, a}
    end

    {qs, _} = :lists.mapfoldl(f, {lastQId, lastGoI, lastSI}, lD)
    qs
  end

  defp join_qnums(cols) do
    :lists.usort(
      for {[{q1, _C1}, {q2, _C2}], _Skip} <- cols do
        {q1, q2}
      end
    )
  end

  defp anon_pattern(p) do
    moreThanOnce = :lists.usort(occ_vars(p) -- :qlc.vars(p))

    {aP, :foo} =
      var_mapfold(
        fn {:var, l, v}, a ->
          case :lists.member(v, moreThanOnce) do
            true ->
              {{:var, l, v}, a}

            false ->
              {{:var, l, :_}, a}
          end
        end,
        :foo,
        p
      )

    aP
  end

  defp join_handle(aP, l, [f, h, o, c], constants) do
    case {aP, constants} do
      {{:var, _, _}, []} ->
        {:fun, l, {:clauses, [{:clause, l, [h], [], [h]}]}}

      _ ->
        a = anno0()

        g0 =
          for {col, cs} <- constants do
            call = {:call, a, {:atom, a, :element}, [{:integer, a, col}, o]}

            list2op(
              for {con, op} <- cs do
                {:op, a, op, con, call}
              end,
              :or
            )
          end

        g =
          cond do
            g0 === [] ->
              g0

            true ->
              [g0]
          end

        cC1 = {:clause, l, [aP], g, [{:cons, l, o, closure({:call, l, f, [f, c]}, l)}]}
        cC2 = {:clause, l, [{:var, l, :_}], [], [{:call, l, f, [f, c]}]}
        case__ = {:case, l, o, [cC1, cC2]}

        cls = [
          {:clause, l, [{:var, l, :_}, {nil, l}], [], [{nil, l}]},
          {:clause, l, [f, {:cons, l, o, c}], [], [case__]},
          {:clause, l, [f, c], [[{:call, l, {:atom, l, :is_function}, [c]}]],
           [{:call, l, f, [f, {:call, l, c, []}]}]},
          {:clause, l, [{:var, l, :_}, c], [], [c]}
        ]

        fun = {:fun, l, {:clauses, cls}}

        {:fun, l,
         {:clauses,
          [{:clause, l, [h], [], [{:match, l, f, fun}, closure({:call, l, f, [f, h]}, l)]}]}}
    end
  end

  defp join_handle_constants(qId, extraConstants) do
    idNo = r_qid(qId, :no)

    case :lists.keyfind(idNo, 1, extraConstants) do
      {^idNo, constOps} ->
        constOps

      false ->
        []
    end
  end

  defp column_fun(columns, qualifierNumber, lcL) do
    a = anno0()

    colCls0 =
      for {{cIdNo, col}, vs0, {fTag, fils}} <- columns,
          cIdNo === qualifierNumber do
        true = vs0 !== []
        vs1 = list2cons(vs0)

        fils1 =
          {:tuple, a,
           [
             {:atom, a, fTag},
             :lists.foldr(
               fn f, ac ->
                 {:cons, a, {:integer, a, f}, ac}
               end,
               {nil, a},
               fils
             )
           ]}

        tag =
          case :ordsets.to_list(:qlc.vars(vs1)) do
            imp when length(imp) > 0 and length(vs0) > 1 ->
              :usort_needed

            _ ->
              :values
          end

        vs = {:tuple, a, [{:atom, a, tag}, vs1, fils1]}
        {:clause, a, [:erl_parse.abstract(col)], [], [vs]}
      end ++ [{:clause, a, [{:var, a, :_}], [], [{:atom, a, false}]}]

    colCls = set_anno(colCls0, lcL)
    {:fun, lcL, {:clauses, colCls}}
  end

  defp template_columns(qs0, e0, allIVs, dependencies, state) do
    e = expand_expr_records(pre_expand(e0), state)
    templateAsPattern = template_as_pattern(e)
    qs = [templateAsPattern | qs0]
    equalColumns = equal_columns2(qs, allIVs, dependencies, state)
    matchColumns = eq_columns2(qs, allIVs, dependencies, state)
    equal = template_cols(equalColumns)
    match = template_cols(matchColumns)
    l = anno0()

    cond do
      match === equal ->
        [{{:var, l, :_}, match}]

      true ->
        [{{:atom, l, :==}, equal}, {{:atom, l, :"=:="}, match}]
    end
  end

  defp equal_columns2(qualifiers, allIVs, dependencies, state) do
    {jI, _Skip} = join_info(qualifiers, allIVs, dependencies, state, _JoinOp = :==)
    jI
  end

  defp eq_columns2(qualifiers, allIVs, dependencies, state) do
    {jI, _SKip} = join_info(qualifiers, allIVs, dependencies, state, _JoinOp = :"=:=")
    jI
  end

  defp template_cols(columnClasses) do
    :lists.sort(
      for class <- columnClasses,
          {idNo, col} <- class,
          idNo !== 0,
          [] !==
            (cs =
               (for {0, c} <- class do
                  c
                end)) do
        {{idNo, col}, :lists.usort(cs)}
      end
    )
  end

  defp template_as_pattern(e) do
    p = simple_template(e)
    {r_qid(lcid: :template, no: 0), :foo, :foo, {:gen, p, {nil, anno0()}}}
  end

  defp simple_template(
         {:call, l, {:remote, _, {:atom, _, :erlang}, {:atom, _, :element}} = call,
          [{:integer, _, i} = a1, a2]}
       )
       when i > 0 do
    {:call, l, call, [a1, simple_template(a2)]}
  end

  defp simple_template({:var, _, _} = e) do
    e
  end

  defp simple_template({:tuple, l, es}) do
    {:tuple, l,
     for e <- es do
       simple_template(e)
     end}
  end

  defp simple_template({:cons, l, h, t}) do
    {:cons, l, simple_template(h), simple_template(t)}
  end

  defp simple_template(e) do
    case (try do
            :erl_parse.normalise(e)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        unique_var()

      _ ->
        e
    end
  end

  defp qualifier_dependencies(qualifiers, introVs) do
    intro =
      :sofs.relation(
        for {qId, iVs} <- introVs,
            iV <- iVs do
          {iV, qId}
        end
      )

    {filterData, _} = qual_data(qualifiers)

    used =
      :sofs.relation(
        for {qId, {:fil, f}} <- filterData,
            uV <- :qlc.vars(f) do
          {qId, uV}
        end
      )

    depend =
      :sofs.strict_relation(
        :sofs.relative_product(
          used,
          intro
        )
      )

    g = :sofs.family_to_digraph(:sofs.relation_to_family(depend))

    dep0 =
      for v <- :digraph.vertices(g) do
        {v, :digraph_utils.reachable_neighbours([v], g)}
      end

    true = :digraph.delete(g)
    filterIds = :sofs.set(filter_ids(qualifiers))
    dep1 = :sofs.restriction(:sofs.family(dep0), filterIds)

    noDep =
      :sofs.constant_function(
        filterIds,
        :sofs.empty_set()
      )

    :sofs.to_external(:sofs.family_union(dep1, noDep))
  end

  defp filter_ids(qualifiers) do
    {filterData, _} = qual_data(qualifiers)

    for {qId, _} <- filterData do
      qId
    end
  end

  defp match_spec_quals(template, dependencies, qualifiers, state) do
    {filterData, generatorData} = qual_data(qualifiers)

    noFilterGIds =
      for {gId, _} <- generatorData do
        gId
      end --
        :lists.flatmap(
          fn {_, gIds} ->
            gIds
          end,
          dependencies
        )

    filters = filter_list(filterData, dependencies, state)

    candidates =
      for {qId, [qId2]} <- dependencies,
          {gQId, {:gen, pattern, _}} <- generatorData,
          gQId === qId2,
          {fQId, {:fil, f}} = filter <- filters,
          fQId === qId do
        {r_qid(qId2, :no), pattern, [filter], f}
      end ++
        for {gId, {:gen, pattern, _}} <- generatorData,
            :lists.member(gId, noFilterGIds) do
          {r_qid(gId, :no), pattern, [], {:atom, anno0(), true}}
        end

    e = {nil, anno0()}

    gF =
      for {gNum, pattern, filter, f} <- candidates,
          :no !== try_ms(e, pattern, f, state) do
        {{gNum, pattern}, filter}
      end

    gFF =
      :sofs.relation_to_family(
        :sofs.relation(
          gF,
          [{:gnum_pattern, [:filter]}]
        )
      )

    gFFL = :sofs.to_external(:sofs.family_union(gFF))

    try do
      [{{gNum, pattern}, gFilterData}] = gFFL
      true = length(gFilterData) === length(filterData)
      [_] = generatorData
      abstrMS = gen_ms(template, pattern, gFilterData, state)
      [{gNum, abstrMS, :all}]
    catch
      _, _ ->
        {templVar, _} = anon_var({:var, anno0(), :_}, 0)

        for {{^gNum, ^pattern}, ^gFilterData} <- gFFL do
          one_gen_match_spec(gNum, pattern, gFilterData, state, templVar)
        end
    end
  end

  defp one_gen_match_spec(gNum, pattern0, gFilterData, state, templVar) do
    {e, pattern} = pattern_as_template(pattern0, templVar)
    abstrMS = gen_ms(e, pattern, gFilterData, state)

    {gNum, abstrMS,
     for {fId, _} <- gFilterData do
       r_qid(fId, :no)
     end}
  end

  defp gen_ms(e, pattern, gFilterData, state) do
    {:ok, mS, aMS} = try_ms(e, pattern, filters_as_one(gFilterData), state)

    case mS do
      [{:"$1", [true], [:"$1"]}] ->
        {:atom, anno0(), :no_match_spec}

      _ ->
        aMS
    end
  end

  defp pattern_as_template({:var, _, :_}, templVar) do
    {templVar, templVar}
  end

  defp pattern_as_template({:var, _, _} = v, _TemplVar) do
    {v, v}
  end

  defp pattern_as_template({:match, l, e, {:var, _, :_}}, templVar) do
    {templVar, {:match, l, e, templVar}}
  end

  defp pattern_as_template({:match, l, {:var, _, :_}, e}, templVar) do
    {templVar, {:match, l, e, templVar}}
  end

  defp pattern_as_template(
         {:match, _, _E, {:var, _, _} = v} = p,
         _TemplVar
       ) do
    {v, p}
  end

  defp pattern_as_template(
         {:match, _, {:var, _, _} = v, _E} = p,
         _TemplVar
       ) do
    {v, p}
  end

  defp pattern_as_template(e, templVar) do
    l = anno0()
    {templVar, {:match, l, e, templVar}}
  end

  defp constants_and_sizes(qualifiers0, e, dependencies, allIVs, state) do
    templateAsPattern = template_as_pattern(e)
    qualifiers = [templateAsPattern | qualifiers0]
    {filterData, generatorData} = qual_data(qualifiers)
    {filter, anon1, imported} = filter_info(filterData, allIVs, dependencies, state)

    patBindFun = fn _Op, value ->
      is_bindable(value)
    end

    {patternFrame, patternVars} = pattern_frame(generatorData, patBindFun, anon1, state)
    patternFrames = frame2frames(patternFrame)

    filterFun = fn bindFun ->
      filter(filter, patternFrames, bindFun, state, imported)
    end

    szFs = filterFun.(patBindFun)
    sizeInfo = pattern_sizes(patternVars, szFs)
    selectorFun = const_selector(imported)

    patternConstants =
      :lists.flatten(
        frames_to_columns(
          patternFrames,
          patternVars,
          deref_pattern(imported),
          selectorFun,
          imported,
          :"=:="
        )
      )

    {eqColumnConstants, _EqExtraConsts} =
      constants(
        filterFun,
        patternVars,
        patternConstants,
        patternFrame,
        filterData,
        dependencies,
        _LookupOp1 = :"=:=",
        imported,
        state
      )

    {equalColumnConstants, equalExtraConsts} =
      constants(
        filterFun,
        patternVars,
        patternConstants,
        patternFrame,
        filterData,
        dependencies,
        _LookupOp2 = :==,
        imported,
        state
      )

    extraCon1 =
      for {consts, op} <- [{equalExtraConsts, :==}],
          {{gId, col}, val} <- consts do
        {{gId, col}, {val, op}}
      end

    extraConstants =
      family_list(
        for {{gId, col}, valOps} <- family_list(extraCon1) do
          {gId, {col, valOps}}
        end
      )

    {eqColumnConstants, equalColumnConstants, extraConstants, sizeInfo}
  end

  defp constants(
         filterFun,
         patternVars,
         patternConstants,
         patternFrame,
         filterData,
         dependencies,
         lookupOp,
         imported,
         state
       ) do
    bindFun = fn _Op, value ->
      is_bindable(value)
    end

    fs = filterFun.(bindFun)
    selectorFun = const_selector(imported)

    columnConstants0 =
      frames_to_columns(
        fs,
        patternVars,
        deref_lookup(imported, lookupOp),
        selectorFun,
        imported,
        lookupOp
      )

    columnConstants1 = :lists.flatten(columnConstants0)

    extraConstants =
      for {{gId, col}, vals} <- columnConstants1 -- patternConstants, gId !== 0, val <- vals do
        {{gId, col}, val}
      end

    columnConstants =
      lu_skip(
        columnConstants1,
        filterData,
        patternFrame,
        patternVars,
        dependencies,
        state,
        imported,
        lookupOp
      )

    {columnConstants, extraConstants}
  end

  defp deref_lookup(imported, :==) do
    fn pV, f ->
      deref_values(pV, f, imported)
    end
  end

  defp deref_lookup(imported, :"=:=") do
    bFun = fn dV, op ->
      op === :"=:=" or free_of_integers(dV, imported)
    end

    fn pV, f ->
      deref_values(pV, f, bFun, imported)
    end
  end

  defp lu_skip(
         colConstants,
         filterData,
         patternFrame,
         patternVars,
         dependencies,
         state,
         imported,
         lookupOp
       ) do
    failSelector = fn _Frame ->
      fn value ->
        {:yes, value}
      end
    end

    patternFrames = frame2frames(patternFrame)

    patternColumns =
      :lists.flatten(
        frames_to_columns(
          patternFrames,
          patternVars,
          deref_pattern(imported),
          failSelector,
          imported,
          lookupOp
        )
      )

    bindFun = fn _Op, value ->
      is_bindable(value)
    end

    colFil =
      for {fId, {:fil, fil}} <- filter_list(filterData, dependencies, state),
          [] !== (sFs = safe_filter(reset_anno(fil), patternFrames, bindFun, state, imported)),
          {gId, pV} <- patternVars,
          [] !==
            (cols =
               hd(
                 frames_to_columns(
                   sFs,
                   [{gId, pV}],
                   deref_lu_skip(
                     lookupOp,
                     imported
                   ),
                   const_selector(imported),
                   imported,
                   lookupOp
                 )
               )),
          length(d = cols -- patternColumns) === 1,
          {{_, col} = column, constants} <- d,
          :lists.all(
            fn frame ->
              {varI, frameI} = unify_column(frame, pV, col, bindFun, imported)
              varValues = deref_skip(varI, frameI, lookupOp, imported)
              {nV, f1} = unify_column(patternFrame, pV, col, bindFun, imported)
              f2 = unify_var_bindings(varValues, :"=:=", nV, f1, bindFun, imported, false)

              lookedUpConstants =
                case :lists.keyfind(
                       column,
                       1,
                       colConstants
                     ) do
                  false ->
                    []

                  {^column, lUCs} ->
                    lUCs
                end

              length(varValues) <= 1 and constants -- lookedUpConstants === [] and
                bindings_is_subset(
                  frame,
                  f2,
                  imported
                )
            end,
            sFs
          ) do
        {column, r_qid(fId, :no)}
      end

    colFils = family_list(colFil)

    for {col, constants} <- colConstants do
      {col, constants, skip_tag(col, colFils, filterData)}
    end
  end

  defp deref_skip(e, f, _LookupOp, imported) do
    deref(e, f, imported)
  end

  defp deref_lu_skip(:==, imported) do
    bFun = fn dV, op ->
      op === :== or free_of_integers(dV, imported)
    end

    fn pV, f ->
      deref_values(pV, f, bFun, imported)
    end
  end

  defp deref_lu_skip(:"=:=", imported) do
    fn pV, f ->
      deref_values(pV, f, imported)
    end
  end

  defp equal_columns(qualifiers, allIVs, dependencies, state) do
    {cs, skip} = join_info(qualifiers, allIVs, dependencies, state, _JoinOp = :==)
    join_gens(cs, qualifiers, skip)
  end

  defp eq_columns(qualifiers, allIVs, dependencies, state) do
    {cs, skip} = join_info(qualifiers, allIVs, dependencies, state, _JoinOp = :"=:=")
    join_gens(cs, qualifiers, skip)
  end

  defp join_gens(cs0, qs, skip) do
    cs =
      for c <- cs0 do
        family_list(c)
      end

    {fD, _GeneratorData} = qual_data(qs)

    {join_gens2(
       :lists.filter(
         fn c ->
           length(c) === 2
         end,
         cs
       ),
       fD,
       skip
     ),
     join_gens2(
       :lists.filter(
         fn c ->
           length(c) > 2
         end,
         cs
       ),
       fD,
       skip
     )}
  end

  defp join_gens2(cs0, filterData, skip) do
    for j <-
          :lists.append(
            for c <- cs0 do
              :qlc.all_selections(c)
            end
          ) do
      {j,
       skip_tag(
         case :lists.keyfind(j, 1, skip) do
           {^j, filL} ->
             filL

           false ->
             []
         end,
         filterData
       )}
    end
  end

  defp skip_tag(filList, filterData) do
    {cond do
       length(filterData) === length(filList) ->
         :all

       true ->
         :some
     end, filList}
  end

  defp skip_tag(col, colFils, filterData) do
    case :lists.keyfind(col, 1, colFils) do
      {^col, filL} ->
        tag =
          cond do
            length(filterData) === length(filL) ->
              :all

            true ->
              :some
          end

        {tag, filL}

      false ->
        {:some, []}
    end
  end

  defp join_info(qualifiers, allIVs, dependencies, state, joinOp) do
    {filterData, generatorData} = qual_data(qualifiers)
    {filter, anon1, imported} = filter_info(filterData, allIVs, dependencies, state)

    bindFun = fn _Op, v ->
      bind_no_const(v, imported)
    end

    {patternFrame, patternVars} = pattern_frame(generatorData, bindFun, anon1, state)
    patternFrames = frame2frames(patternFrame)
    fs = filter(filter, patternFrames, bindFun, state, imported)
    selectorFun = no_const_selector(imported)

    cols =
      frames_to_columns(
        fs,
        patternVars,
        fn pV1, f ->
          deref_join(pV1, f, joinOp)
        end,
        selectorFun,
        imported,
        :"=:="
      )

    jC = join_classes(cols)

    skip =
      join_skip(jC, filterData, patternFrame, patternVars, dependencies, state, imported, joinOp)

    {jC, skip}
  end

  defp deref_join(e, frame, :==) do
    deref_values(e, frame, _Imp = [])
  end

  defp deref_join(e, frame, :"=:=") do
    deref_values(
      e,
      frame,
      fn _DV, op ->
        op === :"=:="
      end,
      :all
    )
  end

  defp join_classes(cols0) do
    colVar = :sofs.relation(:lists.append(cols0))
    cols = :sofs.partition(2, colVar)

    for cs <- :sofs.to_external(cols), length(cs) > 1 do
      for {c, _} <- cs do
        c
      end
    end
  end

  defp join_skip(
         joinClasses,
         filterData,
         patternFrame,
         patternVars,
         dependencies,
         state,
         imported,
         joinOp
       ) do
    patternFrames = frame2frames(patternFrame)

    colFil =
      for [{q1, c1}, {q2, c2}] = joinClass <- joinClasses,
          {gId1, pV1} <- patternVars,
          r_qid(gId1, :no) === q1,
          {gId2, pV2} <- patternVars,
          r_qid(gId2, :no) === q2,
          {fId, {:fil, fil}} <- filter_list(filterData, dependencies, state),
          {:value, {_, gIds}} <- [:lists.keysearch(fId, 1, dependencies)],
          gIds === :lists.sort([gId1, gId2]),
          (
            bindFun = fn _Op, v ->
              is_bindable(v)
            end

            {v1, jF1} = unify_column(patternFrame, pV1, c1, bindFun, imported)
            {v2, jF2} = unify_column(jF1, pV2, c2, bindFun, imported)
            jF = unify(joinOp, v1, v2, jF2, bindFun, imported)
            sFs = safe_filter(reset_anno(fil), patternFrames, bindFun, state, imported)
            jImp = :qlc.vars([sFs, jF])

            :lists.all(
              fn frame ->
                bindings_is_subset(frame, jF, jImp)
              end,
              sFs
            ) and sFs !== []
          ) do
        {joinClass, r_qid(fId, :no)}
      end

    family_list(colFil)
  end

  defp filter_info(filterData, allIVs, dependencies, state) do
    filterList = filter_list(filterData, dependencies, state)
    filter0 = reset_anno(filters_as_one(filterList))
    anon0 = 0
    {filter, anon1} = anon_var(filter0, anon0)

    imported =
      :ordsets.subtract(
        :qlc.vars(filter),
        :ordsets.from_list(allIVs)
      )

    {filter, anon1, imported}
  end

  defp filter_list(filterData, dependencies, state) do
    sel_gf(filterData, 1, dependencies, state, [], [])
  end

  defp sel_gf([], _N, _Deps, _RDs, _Gens, _Gens1) do
    []
  end

  defp sel_gf([{r_qid(no: n) = id, {:fil, f}} = fil | fData], n, deps, state, gens, gens1) do
    case is_guard_test(f, state) do
      true ->
        {^id, gIds} = :lists.keyfind(id, 1, deps)

        case length(gIds) <= 1 do
          true ->
            case generators_in_scope(gIds, gens1) do
              true ->
                [fil | sel_gf(fData, n + 1, deps, state, gens, gens1)]

              false ->
                sel_gf(fData, n + 1, deps, state, [], [])
            end

          false ->
            case generators_in_scope(gIds, gens) do
              true ->
                [fil | sel_gf(fData, n + 1, deps, state, gens, [])]

              false ->
                sel_gf(fData, n + 1, deps, state, [], [])
            end
        end

      false ->
        sel_gf(fData, n + 1, deps, state, [], [])
    end
  end

  defp sel_gf(fData, n, deps, state, gens, gens1) do
    sel_gf(fData, n + 1, deps, state, [n | gens], [n | gens1])
  end

  defp generators_in_scope(genIds, genNumbers) do
    :lists.all(
      fn r_qid(no: n) ->
        :lists.member(n, genNumbers)
      end,
      genIds
    )
  end

  defp pattern_frame(generatorData, bindFun, anon1, state) do
    frame0 = []

    {patternFrame, _Anon2, patternVars} =
      :lists.foldl(
        fn {qId, {:gen, pattern, _}}, {f0, an0, pVs} ->
          {f1, an1, pV} = pattern(pattern, an0, f0, bindFun, state)
          {f1, an1, [{qId, pV} | pVs]}
        end,
        {frame0, anon1, []},
        generatorData
      )

    {patternFrame, patternVars}
  end

  defp const_selector(imported) do
    selector(imported, &is_const/2)
  end

  defp no_const_selector(imported) do
    selector(
      imported,
      fn v, i ->
        not is_const(v, i)
      end
    )
  end

  defp selector(imported, testFun) do
    fn _Frame ->
      fn value ->
        case testFun.(value, imported) do
          true ->
            {:yes, value}

          false ->
            :no
        end
      end
    end
  end

  defp bind_no_const(value, imported) do
    case is_const(value, imported) do
      true ->
        false

      false ->
        is_bindable(value)
    end
  end

  defp is_const(value, imported) do
    [] ===
      :ordsets.to_list(
        :ordsets.subtract(
          :qlc.vars(value),
          imported
        )
      )
  end

  defp is_bindable(value) do
    case normalise(value) do
      {:ok, _C} ->
        true

      :not_ok ->
        false
    end
  end

  defp pattern(p0, anonI, frame0, bindFun, state) do
    p1 =
      try do
        expand_pattern_records(p0, state)
      catch
        _, _ ->
          p0
      end

    p2 = reset_anno(p1)
    {p3, anonN} = anon_var(p2, anonI)
    {p4, f1} = match_in_pattern(tuple2cons(p3), frame0, bindFun)
    {p, f2} = element_calls(p4, f1, bindFun, _Imp = [])
    {:var, _, patternVar} = uniqueVar = unique_var()
    f = unify(:"=:=", uniqueVar, p, f2, bindFun, _Imported = [])
    {f, anonN, patternVar}
  end

  defp frame2frames(:failed) do
    []
  end

  defp frame2frames(f) do
    [f]
  end

  defp match_in_pattern({:match, _, e10, e20}, f0, bF) do
    {e1, f1} = match_in_pattern(e10, f0, bF)
    {e2, f} = match_in_pattern(e20, f1, bF)

    e =
      case bF.(:"=:=", e1) do
        true ->
          e1

        false ->
          e2
      end

    {e, unify(:"=:=", e1, e2, f, bF, _Imported = [])}
  end

  defp match_in_pattern(t, f0, bF) when is_tuple(t) do
    {l, f} = match_in_pattern(:erlang.tuple_to_list(t), f0, bF)
    {:erlang.list_to_tuple(l), f}
  end

  defp match_in_pattern([e0 | es0], f0, bF) do
    {e, f1} = match_in_pattern(e0, f0, bF)
    {es, f} = match_in_pattern(es0, f1, bF)
    {[e | es], f}
  end

  defp match_in_pattern(e, f, _BF) do
    {e, f}
  end

  defp anon_var(e, anonI) do
    var_mapfold(
      fn
        {:var, l, :_}, n ->
          {{:var, l, n}, n + 1}

        var, n ->
          {var, n}
      end,
      anonI,
      e
    )
  end

  defp reset_anno(t) do
    set_anno(t, anno0())
  end

  defp set_anno(t, a) do
    map_anno(
      fn _L ->
        a
      end,
      t
    )
  end

  Record.defrecord(:r_fstate, :fstate,
    state: :undefined,
    bind_fun: :undefined,
    imported: :undefined
  )

  defp filter(_E, [] = frames0, _BF, _State, _Imported) do
    frames0
  end

  defp filter(e0, frames0, bF, state, imported) do
    e = pre_expand(e0)
    fState = r_fstate(state: state, bind_fun: bF, imported: imported)
    filter1(e, frames0, fState)
  end

  defp filter1({:op, _, op, l0, r0}, fs, fS)
       when op === :"=:=" or
              op === :== do
    r_fstate(state: s, bind_fun: bF, imported: imported) = fS

    :lists.flatmap(
      fn f0 ->
        {l, f1} = prep_expr(l0, f0, s, bF, imported)
        {r, f2} = prep_expr(r0, f1, s, bF, imported)

        case unify(op, l, r, f2, bF, imported) do
          :failed ->
            []

          f ->
            [f]
        end
      end,
      fs
    )
  end

  defp filter1({:op, _, op, l, r}, fs, fS)
       when op === :and or
              op === :andalso do
    filter1(r, filter1(l, fs, fS), fS)
  end

  defp filter1({:op, _, op, l, r}, fs, fS)
       when op === :or or
              op === :orelse or op === :xor do
    filter1(l, fs, fS) ++ filter1(r, fs, fS)
  end

  defp filter1({:atom, _, atom}, _Fs, _FS)
       when atom !== true do
    []
  end

  defp filter1(
         {:call, l, {:remote, _, {:atom, _, :erlang}, {:atom, _, :is_record}}, [t, r]},
         fs,
         fS
       ) do
    filter1(
      {:op, l, :"=:=",
       {:call, l, {:remote, l, {:atom, l, :erlang}, {:atom, l, :element}}, [{:integer, l, 1}, t]},
       r},
      fs,
      fS
    )
  end

  defp filter1(
         {:call, l, {:remote, l1, {:atom, _, :erlang} = m, {:atom, l2, :is_record}}, [t, r, _Sz]},
         fs,
         fS
       ) do
    filter1({:call, l, {:remote, l1, m, {:atom, l2, :is_record}}, [t, r]}, fs, fS)
  end

  defp filter1(_E, fs, _FS) do
    fs
  end

  defp safe_filter(_E, [] = frames0, _BF, _State, _Imported) do
    frames0
  end

  defp safe_filter(e0, frames0, bF, state, imported) do
    e = pre_expand(e0)
    fState = r_fstate(state: state, bind_fun: bF, imported: imported)
    safe_filter1(e, frames0, fState)
  end

  defp safe_filter1({:op, _, op, l0, r0}, fs, fS)
       when op === :"=:=" or
              op === :== do
    r_fstate(state: s, bind_fun: bF, imported: imported) = fS

    :lists.flatmap(
      fn f0 ->
        {l, f1} = prep_expr(l0, f0, s, bF, imported)
        {r, f2} = prep_expr(r0, f1, s, bF, imported)

        case safe_unify(op, l, r, f2, bF, imported) do
          :failed ->
            []

          f ->
            [f]
        end
      end,
      fs
    )
  end

  defp safe_filter1({:op, _, op, l, r}, fs, fS)
       when op === :and or
              op === :andalso do
    safe_filter1(r, safe_filter1(l, fs, fS), fS)
  end

  defp safe_filter1({:op, _, op, l, r}, fs, fS)
       when op === :or or
              op === :orelse do
    safe_filter1(l, fs, fS) ++ safe_filter1(r, fs, fS)
  end

  defp safe_filter1({:atom, _, true}, fs, _FS) do
    fs
  end

  defp safe_filter1(_E, _Fs, _FS) do
    []
  end

  defp pre_expand({:call, l1, {:atom, l2, :record}, as}) do
    pre_expand({:call, l1, {:atom, l2, :is_record}, as})
  end

  defp pre_expand({:call, l, {:atom, _, _} = f, as}) do
    pre_expand({:call, l, {:remote, l, {:atom, l, :erlang}, f}, as})
  end

  defp pre_expand({:call, l, {:tuple, _, [m, f]}, as}) do
    pre_expand({:call, l, {:remote, l, m, f}, as})
  end

  defp pre_expand(t) when is_tuple(t) do
    :erlang.list_to_tuple(pre_expand(:erlang.tuple_to_list(t)))
  end

  defp pre_expand([e | es]) do
    [pre_expand(e) | pre_expand(es)]
  end

  defp pre_expand(t) do
    t
  end

  defp frames_to_columns([], _PatternVars, _DerefFun, _SelectorFun, _Imp, _CompOp) do
    []
  end

  defp frames_to_columns(fs, patternVars, derefFun, selectorFun, imp, compOp) do
    sizesVarsL =
      for {patternId, pV} <- patternVars do
        patVar = {:var, anno0(), pV}

        patternSizes =
          for f <- fs do
            pattern_size([f], patVar, false)
          end

        maxPZ = :lists.max([0 | patternSizes -- [:undefined]])
        vars = pat_vars(maxPZ)
        {r_qid(patternId, :no), patVar, patternSizes, vars}
      end

    bF = fn _Op, value ->
      is_bindable(value)
    end

    fun = fn {_PatN, patVar, patSizes, vars}, frames ->
      for {sz, frame} <- :lists.zip(patSizes, frames) do
        unify(:"=:=", pat_tuple(sz, vars), patVar, frame, bF, imp)
      end
    end

    nFs = :lists.foldl(fun, fs, sizesVarsL)

    for {patN, _PatVar, patSizes, vars} <- sizesVarsL do
      frames2cols(nFs, patN, patSizes, vars, derefFun, selectorFun, compOp)
    end
  end

  defp frames2cols(fs, patN, patSizes, vars, derefFun, selectorFun, compOp) do
    rs =
      for {f, patSz} <- :lists.zip(fs, patSizes) do
        rL =
          for {v, col} <-
                :lists.zip(
                  :lists.sublist(
                    vars,
                    patSz
                  ),
                  :lists.seq(1, patSz)
                ),
              tl(consts = derefFun.(v, f)) === [],
              (const = selectorFun.(f).(hd(consts))) !== :no do
            {{patN, col}, cons2tuple(:erlang.element(2, const))}
          end

        :sofs.relation(rL)
      end

    ss = :sofs.from_sets(rs)

    d =
      :sofs.intersection(
        :sofs.projection(
          fn s ->
            :sofs.projection(1, s)
          end,
          ss
        )
      )

    cs =
      :sofs.restriction(
        :sofs.relation_to_family(:sofs.union(ss)),
        d
      )

    for {_, vs} = c <- :sofs.to_external(cs),
        not col_ignore(vs, compOp) do
      c
    end
  end

  defp pat_vars(n) do
    for _ <- :lists.seq(1, n) do
      unique_var()
    end
  end

  defp pat_tuple(sz, vars) when is_integer(sz) and sz > 0 do
    tupleTail = unique_var()
    {:cons_tuple, list2cons(:lists.sublist(vars, sz) ++ tupleTail)}
  end

  defp pat_tuple(_, _Vars) do
    unique_var()
  end

  defp col_ignore(_Vs, :"=:=") do
    false
  end

  defp col_ignore(vs, :==) do
    length(vs) !==
      length(
        :lists.usort(
          for v <- vs do
            :erlang.element(2, normalise(v))
          end
        )
      )
  end

  defp pattern_sizes(patternVars, fs) do
    for {qId, pV} <- patternVars,
        :undefined !== (size = pattern_size(fs, {:var, anno0(), pV}, true)) do
      {r_qid(qId, :no), size}
    end
  end

  defp pattern_size(fs, patternVar, exact) do
    fun = fn f ->
      deref_pattern(_Imported = []).(patternVar, f)
    end

    derefs = :lists.flatmap(fun, fs)

    szs =
      for {:cons_tuple, cs} <- derefs do
        pattern_sz(cs, 0, exact)
      end

    case :lists.usort(szs) do
      [sz] when is_integer(sz) and sz >= 0 ->
        sz

      [] when not exact ->
        0

      _ ->
        :undefined
    end
  end

  defp pattern_sz({:cons, _, _C, e}, col, exact) do
    pattern_sz(e, col + 1, exact)
  end

  defp pattern_sz({nil, _}, sz, _Exact) do
    sz
  end

  defp pattern_sz(_, _Sz, true) do
    :undefined
  end

  defp pattern_sz(_, sz, false) do
    sz
  end

  defp deref_pattern(imported) do
    fn pV, f ->
      deref_values(pV, f, imported)
    end
  end

  defp prep_expr(e, f, s, bF, imported) do
    element_calls(tuple2cons(expand_expr_records(e, s)), f, bF, imported)
  end

  defp unify_column(frame, var, col, bindFun, imported) do
    a = anno0()

    call =
      {:call, a, {:remote, a, {:atom, a, :erlang}, {:atom, a, :element}},
       [{:integer, a, col}, {:var, a, var}]}

    element_calls(call, frame, bindFun, imported)
  end

  defp element_calls(
         {:call, _, {:remote, _, {:atom, _, :erlang}, {:atom, _, :element}},
          [{:integer, _, i}, term0]},
         f0,
         bF,
         imported
       )
       when i > 0 do
    tupleTail = unique_var()

    varsL =
      for _ <- :lists.seq(1, i) do
        unique_var()
      end

    vars = varsL ++ tupleTail
    tuple = {:cons_tuple, list2cons(vars)}
    varI = :lists.nth(i, varsL)
    {term, f} = element_calls(term0, f0, bF, imported)
    {varI, unify(:"=:=", tuple, term, f, bF, imported)}
  end

  defp element_calls(t, f0, bF, imported) when is_tuple(t) do
    {l, f} = element_calls(:erlang.tuple_to_list(t), f0, bF, imported)
    {:erlang.list_to_tuple(l), f}
  end

  defp element_calls([e0 | es0], f0, bF, imported) do
    {e, f1} = element_calls(e0, f0, bF, imported)
    {es, f} = element_calls(es0, f1, bF, imported)
    {[e | es], f}
  end

  defp element_calls(e, f, _BF, _Imported) do
    {e, f}
  end

  defp unique_var() do
    {:var, anno0(), make_ref()}
  end

  defp is_unique_var({:var, _L, v}) do
    is_reference(v)
  end

  defp expand_pattern_records(p, state) do
    a = anno0()
    e = {:case, a, {:atom, a, true}, [{:clause, a, [p], [], [{:atom, a, true}]}]}

    {:case, _, _, [{:clause, ^a, [nP], _, _}]} =
      expand_expr_records(
        e,
        state
      )

    nP
  end

  defp expand_expr_records(e, state) do
    recordDefs = r_state(state, :records)
    a = anno1()
    forms0 = recordDefs ++ [{:function, a, :foo, 0, [{:clause, a, [], [], [pe(e)]}]}]

    forms =
      :erl_expand_records.module(
        forms0,
        [:no_strict_record_tests]
      )

    {:function, _, :foo, 0, [{:clause, _, [], [], [nE]}]} = :lists.last(forms)
    nE
  end

  defp pe({:op, line, op, a}) do
    :erl_eval.partial_eval({:op, line, op, pe(a)})
  end

  defp pe({:op, line, op, l, r}) do
    :erl_eval.partial_eval({:op, line, op, pe(l), pe(r)})
  end

  defp pe(t) when is_tuple(t) do
    :erlang.list_to_tuple(pe(:erlang.tuple_to_list(t)))
  end

  defp pe([e | es]) do
    [pe(e) | pe(es)]
  end

  defp pe(e) do
    e
  end

  defp unify(op, e1, e2, f, bF, imported) do
    unify(op, e1, e2, f, bF, imported, false)
  end

  defp safe_unify(op, e1, e2, f, bF, imported) do
    unify(op, e1, e2, f, bF, imported, true)
  end

  defp unify(_Op, _E1, _E2, :failed, _BF, _Imported, _Safe) do
    :failed
  end

  defp unify(_Op, e, e, f, _BF, _Imported, _Safe) do
    f
  end

  defp unify(op, {:var, _, _} = var, e2, f, bF, imported, safe) do
    extend_frame(op, var, e2, f, bF, imported, safe)
  end

  defp unify(op, e1, {:var, _, _} = var, f, bF, imported, safe) do
    extend_frame(op, var, e1, f, bF, imported, safe)
  end

  defp unify(op, {:cons_tuple, es1}, {:cons_tuple, es2}, f, bF, imported, safe) do
    unify(op, es1, es2, f, bF, imported, safe)
  end

  defp unify(op, {:cons, _, l1, r1}, {:cons, _, l2, r2}, f, bF, imported, safe) do
    e = unify(op, l1, l2, f, bF, imported, safe)
    unify(op, r1, r2, e, bF, imported, safe)
  end

  defp unify(op, e1, e2, f, _BF, _Imported, safe) do
    try do
      {:ok, c1} = normalise(e1)
      {:ok, c2} = normalise(e2)

      cond do
        op === :"=:=" and c1 === c2 ->
          f

        op === :== and c1 == c2 ->
          f

        true ->
          :failed
      end
    catch
      :error, _ when safe ->
        :failed

      :error, _ when not safe ->
        f
    end
  end

  Record.defrecord(:r_bind, :bind, var: :undefined, value: :undefined, op: :undefined)

  defp extend_frame(op, var, value, f, bF, imported, safe) do
    case var_values(var, f) do
      [] ->
        case value do
          {:var, _, _} ->
            case var_values(value, f) do
              [] ->
                add_binding(op, value, var, f, bF, imported, safe)

              valsOps ->
                maybe_add_binding(valsOps, op, value, var, f, bF, imported, safe)
            end

          _ ->
            add_binding(op, var, value, f, bF, imported, safe)
        end

      valsOps ->
        maybe_add_binding(valsOps, op, var, value, f, bF, imported, safe)
    end
  end

  defp maybe_add_binding(valsOps, op, var, value, f0, bF, imported, safe) do
    case unify_var_bindings(valsOps, op, value, f0, bF, imported, safe) do
      :failed ->
        :failed

      f ->
        case already_bound(op, var, value, f) do
          true ->
            f

          false ->
            add_binding(op, var, value, f, bF, imported, safe)
        end
    end
  end

  defp already_bound(op, var, value, f) do
    bFun = fn _DV, bOp ->
      op === bOp
    end

    derefValue = deref_value(value, op, f, bFun, :all)
    derefVar = deref_var(var, f, bFun, :all)
    derefValue -- derefVar === []
  end

  defp unify_var_bindings([], _Op, _Value, f, _BF, _Imported, _Safe) do
    f
  end

  defp unify_var_bindings([{varValue, op2} | bindings], op1, value, f0, bF, imported, safe) do
    op = deref_op(op1, op2)

    case unify(op, varValue, value, f0, bF, imported, safe) do
      :failed ->
        :failed

      f ->
        unify_var_bindings(bindings, op1, value, f, bF, imported, safe)
    end
  end

  defp deref_op(:"=:=", :"=:=") do
    :"=:="
  end

  defp deref_op(_, _) do
    :==
  end

  defp var_values(var, frame) do
    for r_bind(value: value, op: op) <-
          var_bindings(
            var,
            frame
          ) do
      {value, op}
    end
  end

  defp deref_var(var, frame, imported) do
    deref_var(
      var,
      frame,
      fn _DV, _Op ->
        true
      end,
      imported
    )
  end

  defp deref_var(var, frame, bFun, imported) do
    :lists.usort(
      for r_bind(
            value: value,
            op: op
          ) <- var_bindings(var, frame),
          valOp <- deref_value(value, op, frame, bFun, imported) do
        valOp
      end
    )
  end

  defp deref_value(value, op, frame, bFun, imported) do
    :lists.usort(
      for {val, _Op} = valOp <- deref(value, frame, bFun, imported) do
        {val, value_op(valOp, op, imported)}
      end
    )
  end

  defp add_binding(op, var0, value0, f, bF, imported, safe) do
    {var, value} = maybe_swap_var_value(var0, value0, f, imported)

    case bF.(op, value) do
      true ->
        add_binding2(var, value, op, f)

      false when safe ->
        :failed

      false when not safe ->
        f
    end
  end

  defp add_binding2(var, value, op, f) do
    case occurs(var, value, f) do
      true ->
        :failed

      false ->
        [r_bind(var: var, value: value, op: op) | f]
    end
  end

  defp maybe_swap_var_value(var, value, frame, imported) do
    case do_swap_var_value(var, value, frame, imported) do
      true ->
        {value, var}

      false ->
        {var, value}
    end
  end

  defp do_swap_var_value({:var, _, v1} = var1, {:var, _, v2} = var2, f, imported) do
    case swap_vv(var1, var2, f) do
      [] ->
        case swap_vv(var2, var1, f) do
          [] ->
            :ordsets.is_element(v1, imported) and
              not :ordsets.is_element(
                v2,
                imported
              )

          _Bs ->
            true
        end

      _Bs ->
        false
    end
  end

  defp do_swap_var_value(_, _, _F, _Imp) do
    false
  end

  defp swap_vv(v1, v2, f) do
    for r_bind(value: v) <- var_bindings(v1, f), v === v2 do
      v
    end
  end

  defp normalise(e) do
    case (try do
            :erl_parse.normalise(var2const(cons2tuple(e)))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        :not_ok

      c ->
        {:ok, c}
    end
  end

  defp occurs(v, v, _F) do
    true
  end

  defp occurs(v, {:var, _, _} = var, f) do
    :lists.any(
      fn b ->
        occurs(v, r_bind(b, :value), f)
      end,
      var_bindings(var, f)
    )
  end

  defp occurs(v, t, f) when is_tuple(t) do
    :lists.any(
      fn e ->
        occurs(v, e, f)
      end,
      :erlang.tuple_to_list(t)
    )
  end

  defp occurs(v, [e | es], f) do
    occurs(v, e, f) or occurs(v, es, f)
  end

  defp occurs(_V, _E, _F) do
    false
  end

  defp deref_values(e, frame, imported) do
    deref_values(
      e,
      frame,
      fn _DV, _Op ->
        true
      end,
      imported
    )
  end

  defp deref_values(e, frame, bFun, imported) do
    :lists.usort(
      for {v, op} <- deref(e, frame, bFun, imported),
          bFun.(v, op) do
        v
      end
    )
  end

  defp deref(e, f, imp) do
    bFun = fn _DV, _Op ->
      true
    end

    deref(e, f, bFun, imp)
  end

  defp deref({:var, _, _} = v, f, bFun, imp) do
    dBs =
      :lists.flatmap(
        fn b ->
          deref_binding(b, f, bFun, imp)
        end,
        var_bindings(v, f)
      )

    case dBs do
      [] ->
        [{v, :"=:="}]

      _ ->
        :lists.usort(dBs)
    end
  end

  defp deref(t, f, bFun, imp) when is_tuple(t) do
    for {dL, op} <- deref(:erlang.tuple_to_list(t), f, bFun, imp) do
      {:erlang.list_to_tuple(dL), op}
    end
  end

  defp deref(es, f, bFun, imp) when is_list(es) do
    l =
      for c <- es do
        deref(c, f, bFun, imp)
      end

    :lists.usort(
      for s <- all_comb(l) do
        deref_list(s)
      end
    )
  end

  defp deref(e, _F, _BFun, _Imp) do
    [{e, :"=:="}]
  end

  defp var_bindings(var, f) do
    for r_bind(var: v) = b <- f, v === var do
      b
    end
  end

  defp deref_binding(bind, frame, bFun, imp) do
    r_bind(value: value, op: op0) = bind

    for {val, _Op} = valOp <- deref(value, frame, bFun, imp),
        bFun.(val, op = value_op(valOp, op0, imp)) do
      {val, op}
    end
  end

  defp deref_list(l) do
    op =
      case :lists.usort(
             for {_Val, op} <- l do
               op
             end
           ) do
        [:"=:="] ->
          :"=:="

        _ ->
          :==
      end

    {for {v, _Op} <- l do
       v
     end, op}
  end

  defp value_op({_V, :==}, _BindOp, _Imp) do
    :==
  end

  defp value_op({_V, :"=:="}, _BindOp = :"=:=", _Imp) do
    :"=:="
  end

  defp value_op({v, :"=:="}, _BindOp = :==, imp) do
    case free_of_integers(v, imp) do
      true ->
        :"=:="

      false ->
        :==
    end
  end

  defp all_comb([]) do
    [[]]
  end

  defp all_comb([cs | iCs]) do
    for c <- cs, l <- all_comb(iCs) do
      [c | l]
    end
  end

  defp free_of_integers(v, imported) do
    not has_integer(v) and not has_imported_vars(v, imported)
  end

  defp has_imported_vars(value, :all) do
    :qlc.vars(value) !== []
  end

  defp has_imported_vars(value, imported) do
    for var <- :qlc.vars(value),
        :lists.member(var, imported) do
      var
    end !== []
  end

  defp has_integer(abstr) do
    try do
      has_int(abstr)
    catch
      true ->
        true
    end
  end

  defp has_int({:integer, _, i}) when :erlang.float(i) == i do
    throw(true)
  end

  defp has_int({:float, _, f}) when round(f) == f do
    throw(true)
  end

  defp has_int(t) when is_tuple(t) do
    has_int(:erlang.tuple_to_list(t))
  end

  defp has_int([e | es]) do
    has_int(e)
    has_int(es)
  end

  defp has_int(_) do
    false
  end

  defp tuple2cons({:tuple, _, es}) do
    {:cons_tuple, list2cons(tuple2cons(es))}
  end

  defp tuple2cons(t) when is_tuple(t) do
    :erlang.list_to_tuple(tuple2cons(:erlang.tuple_to_list(t)))
  end

  defp tuple2cons([e | es]) do
    [tuple2cons(e) | tuple2cons(es)]
  end

  defp tuple2cons(e) do
    e
  end

  defp list2cons([e | es]) do
    {:cons, anno0(), e, list2cons(es)}
  end

  defp list2cons([]) do
    {nil, anno0()}
  end

  defp list2cons(e) do
    e
  end

  defp cons2tuple({:cons_tuple, es}) do
    {:tuple, anno0(), cons2list(es)}
  end

  defp cons2tuple(t) when is_tuple(t) do
    :erlang.list_to_tuple(cons2tuple(:erlang.tuple_to_list(t)))
  end

  defp cons2tuple([e | es]) do
    [cons2tuple(e) | cons2tuple(es)]
  end

  defp cons2tuple(e) do
    e
  end

  defp cons2list({:cons, _, l, r}) do
    [cons2tuple(l) | cons2list(r)]
  end

  defp cons2list({nil, _}) do
    []
  end

  defp cons2list(e) do
    [cons2tuple(e)]
  end

  defp bindings_is_subset(f1, f2, imported) do
    bF = fn _Op, _Value ->
      true
    end

    f =
      :lists.foldl(
        fn r_bind(var: v, value: value, op: op), frame ->
          unify(op, v, value, frame, bF, imported)
        end,
        f2,
        f1
      )

    bindings_subset(f, f2, imported) and bindings_subset(f2, f, imported)
  end

  defp bindings_subset(f1, f2, imp) do
    vars =
      :lists.usort(
        for r_bind(var: v) <- f1,
            not is_unique_var(v) do
          v
        end
      )

    :lists.all(
      fn v ->
        deref_var(v, f1, imp) === deref_var(v, f2, imp)
      end,
      vars
    )
  end

  defp try_ms(e, p, fltr, state) do
    l = anno1()
    fun = {:fun, l, {:clauses, [{:clause, l, [p], [[fltr]], [e]}]}}
    expr = {:call, l, {:remote, l, {:atom, l, :ets}, {:atom, l, :fun2ms}}, [fun]}
    form = {:function, l, :foo, 0, [{:clause, l, [], [], [expr]}]}

    x =
      :ms_transform.parse_transform(
        r_state(state, :records) ++ [form],
        []
      )

    case (try do
            {:function, ^l, :foo, 0, [{:clause, ^l, [], [], [mS0]}]} = :lists.last(x)
            mS = :erl_parse.normalise(var2const(mS0))
            xMS = :ets.match_spec_compile(mS)
            true = :ets.is_compiled_ms(xMS)
            {:ok, mS, mS0}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _Reason} ->
        :no

      reply ->
        reply
    end
  end

  defp filters_as_one([]) do
    {:atom, anno0(), true}
  end

  defp filters_as_one(filterData) do
    [
      {_, {:fil, filter1}}
      | filters
    ] = :lists.reverse(filterData)

    :lists.foldr(
      fn {_QId, {:fil, filter}}, abstF ->
        {:op, anno0(), :andalso, filter, abstF}
      end,
      filter1,
      filters
    )
  end

  defp qual_data(qualifiers) do
    f = fn t ->
      for {qId, _, _, q} <- qualifiers,
          :erlang.element(1, q) === t do
        {qId, q}
      end
    end

    {f.(:fil), f.(:gen)}
  end

  defp set_field(pos, fs, data) do
    :lists.sublist(
      fs,
      pos - 1
    ) ++ [data] ++ :lists.nthtail(pos, fs)
  end

  defp qdata(
         [
           {r_qid(no: qIdNo), {_QIVs, {{:gen, _P, lE, _GV}, goI, sI}}}
           | qCs
         ],
         l
       ) do
    init =
      case lE do
        {:join, op, q1, q2, h1, h2, cs1_0, cs2_0} ->
          cs1 = qcon(cs1_0)
          cs2 = qcon(cs2_0)
          compat = {:atom, l, :v1}
          cF = closure({:tuple, l, [cs1, cs2, compat]}, l)

          {:tuple, l,
           [{:atom, l, :join}, {:atom, l, op}, {:integer, l, q1}, {:integer, l, q2}, h1, h2, cF]}

        _ ->
          closure(lE, l)
      end

    {:cons, l,
     {:tuple, l,
      [
        {:integer, l, qIdNo},
        {:integer, l, goI},
        {:integer, l, sI},
        {:tuple, l, [{:atom, l, :gen}, init]}
      ]}, qdata(qCs, l)}
  end

  defp qdata(
         [
           {r_qid(no: qIdNo), {_QIVs, {{:fil, _F}, goI, sI}}}
           | qCs
         ],
         l
       ) do
    {:cons, l,
     {:tuple, l, [{:integer, l, qIdNo}, {:integer, l, goI}, {:integer, l, sI}, {:atom, l, :fil}]},
     qdata(qCs, l)}
  end

  defp qdata([], l) do
    {nil, l}
  end

  defp qcon(cs) do
    a = anno0()

    list2cons(
      for {col, constOps} <- cs do
        {:tuple, a, [{:integer, a, col}, list2cons(qcon1(constOps))]}
      end
    )
  end

  defp qcon1(constOps) do
    a = anno0()

    for {const, op} <- constOps do
      {:tuple, a, [const, abstr(op, a)]}
    end
  end

  defp qcode(e, qCs, source, l, state) do
    cL =
      for {_, c} <-
            :lists.keysort(
              1,
              [
                {:qlc.template_state(), e}
                | qcode(qCs, source, state)
              ]
            ) do
        bin = :erlang.term_to_binary(c, [:compressed])

        {:bin, l,
         [{:bin_element, l, {:string, l, :erlang.binary_to_list(bin)}, :default, :default}]}
      end

    {:fun, l, {:clauses, [{:clause, l, [], [], [{:tuple, l, cL}]}]}}
  end

  defp qcode(
         [
           {_QId, {_QIvs, {{:gen, p, _LE, _GV}, goI, _SI}}}
           | qCs
         ],
         source,
         state
       ) do
    [{goI, undo_no_shadows(p, state)} | qcode(qCs, source, state)]
  end

  defp qcode([{qId, {_QIVs, {{:fil, _F}, goI, _SI}}} | qCs], source, state) do
    origF = :erlang.map_get(qId, source)
    [{goI, undo_no_shadows(origF, state)} | qcode(qCs, source, state)]
  end

  defp qcode([], _Source, _State) do
    []
  end

  defp closure(code, l) do
    {:fun, l, {:clauses, [{:clause, l, [], [], [code]}]}}
  end

  defp simple(l, var, init, anno) do
    {:tuple, l, [{:atom, l, :simple_v1}, {:atom, l, var}, init, abstr(loc(anno), anno)]}
  end

  defp clauses([{qId, {qIVs, {qualData, goI, s}}} | qCs], rL, fun, go, nGV, e, iVs, st) do
    :ok
    :ok
    :ok
    l = no_compiler_warning(get_lcid_line(r_qid(qId, :lcid)))

    cs =
      case qualData do
        {:gen, p, _LE, gV} ->
          generator(s, qIVs, p, gV, nGV, e, iVs, rL, fun, go, goI, l, st)

        {:fil, f} ->
          filter(f, l, qIVs, s, rL, fun, go, goI, iVs, st)
      end

    cs ++ clauses(qCs, rL, fun, go, nGV, e, iVs, st)
  end

  defp clauses([], _RL, _Fun, _Go, _NGV, _IVs, _E, _St) do
    []
  end

  defp final(rL, iVs, l, state) do
    iAs = replace(iVs, iVs, :_)

    asL =
      pack_args(
        [
          {:integer, l, 0}
          | abst_vars(
              [rL, :_, :_] ++ iAs,
              l
            )
        ],
        l,
        state
      )

    grd = [is_list_c(rL, l)]
    rev = {:call, l, {:remote, l, {:atom, l, :lists}, {:atom, l, :reverse}}, [{:var, l, rL}]}
    cL = {:clause, l, asL, [grd], [rev]}

    asF =
      pack_args(
        [
          {:integer, l, 0}
          | abst_vars(
              [:_, :_, :_] ++ iAs,
              l
            )
        ],
        l,
        state
      )

    cF = {:clause, l, asF, [], [{nil, l}]}
    [cL, cF]
  end

  defp template(e, rL, fun, go, aT, l, iVs, state) do
    i = :qlc.template_state()
    goI = :qlc.template_state()
    aRL = {:cons, l, e, abst_vars(rL, l)}
    next = next(go, goI, l)
    as0 = abst_vars([rL, fun, go] ++ iVs, l)
    as = pack_args([{:integer, l, i} | as0], l, state)

    nAs =
      pack_args(
        [next, aRL] ++
          abst_vars(
            [fun, go] ++ iVs,
            l
          ),
        l,
        state
      )

    grd = [is_list_c(rL, l)]
    cL = {:clause, l, as, [grd], [{:call, l, {:var, l, fun}, nAs}]}

    f =
      case split_args([next | as0], l, state) do
        {argsL, argsT} ->
          call = {:call, l, {:var, l, fun}, argsL ++ [{:var, l, aT}]}

          {:block, l,
           [
             {:match, l, {:var, l, aT}, argsT},
             {:fun, l, {:clauses, [{:clause, l, [], [], [call]}]}}
           ]}

        fNAs ->
          {:fun, l, {:clauses, [{:clause, l, [], [], [{:call, l, {:var, l, fun}, fNAs}]}]}}
      end

    cF = {:clause, l, as, [], [{:cons, l, e, f}]}
    [cL, cF]
  end

  defp generator(s, qIVs, p, gV, nGV, e, iVs, rL, fun, go, goI, l, state) do
    comAs = abst_vars([rL, fun, go], l)
    initC = generator_init(s, l, gV, rL, fun, go, goI, iVs, state)

    as = [
      {:integer, l, s + 1}
      | comAs ++ abst_vars(replace(qIVs -- [gV], iVs, :_), l)
    ]

    matchS = next(go, goI + 1, l)

    asM0 = [
      matchS
      | comAs ++
          abst_vars(
            replace([gV], iVs, nGV),
            l
          )
    ]

    asM = pack_args(asM0, l, state)
    contS = {:integer, l, s + 1}
    qIVs__GV = qIVs -- [gV]
    tmp = replace([gV], replace(qIVs__GV, iVs, nil), nGV)
    asC = pack_args([contS | comAs ++ abst_vars(tmp, l)], l, state)
    doneS = next(go, goI, l)

    asD0 = [
      doneS
      | comAs ++
          abst_vars(
            replace(qIVs, iVs, nil),
            l
          )
    ]

    asD = pack_args(asD0, l, state)
    csL = generator_list(p, gV, nGV, as, asM, asC, asD, fun, l, state)
    csF = generator_cont(p, gV, nGV, e, as, asM, asC, asD, fun, l, state)
    [initC | csL ++ csF]
  end

  defp generator_init(s, l, gV, rL, fun, go, goI, iVs, state) do
    as0 =
      abst_vars(
        [rL, fun, go] ++ replace([gV], iVs, :_),
        l
      )

    as = pack_args([{:integer, l, s} | as0], l, state)
    next = next(go, goI + 2, l)
    nAs = pack_args([{:integer, l, s + 1} | replace([{:var, l, :_}], as0, next)], l, state)
    {:clause, l, as, [], [{:call, l, {:var, l, fun}, nAs}]}
  end

  defp generator_list(p, gV, nGV, as, asM, asC, asD, fun, l, state) do
    as1 = pack_args(replace([{:var, l, gV}], as, {:cons, l, p, {:var, l, nGV}}), l, state)

    as2 =
      pack_args(replace([{:var, l, gV}], as, {:cons, l, {:var, l, :_}, {:var, l, nGV}}), l, state)

    as3 = pack_args(replace([{:var, l, gV}], as, {nil, l}), l, state)
    cM = {:clause, l, as1, [], [{:call, l, {:var, l, fun}, asM}]}
    cC = {:clause, l, as2, [], [{:call, l, {:var, l, fun}, asC}]}
    cD = {:clause, l, as3, [], [{:call, l, {:var, l, fun}, asD}]}
    [cM, cC, cD]
  end

  defp generator_cont(p, gV, nGV, e, as0, asM, asC, asD, fun, l, state) do
    as = pack_args(as0, l, state)
    cF1 = {:cons, l, p, {:var, l, nGV}}
    cF2 = {:cons, l, {:var, l, :_}, {:var, l, nGV}}
    cF3 = {nil, l}
    cF4 = {:var, l, e}
    cM = {:clause, l, [cF1], [], [{:call, l, {:var, l, fun}, asM}]}
    cC = {:clause, l, [cF2], [], [{:call, l, {:var, l, fun}, asC}]}
    cD = {:clause, l, [cF3], [], [{:call, l, {:var, l, fun}, asD}]}
    cE = {:clause, l, [cF4], [], [cF4]}
    cls = [cM, cC, cD, cE]
    b = {:case, l, {:call, l, {:var, l, gV}, []}, cls}
    [{:clause, l, as, [], [b]}]
  end

  defp filter(e, l, qIVs, s, rL, fun, go, goI, iVs, state) do
    iAs = replace(qIVs, iVs, :_)

    as =
      pack_args(
        [
          {:integer, l, s}
          | abst_vars(
              [rL, fun, go] ++ iAs,
              l
            )
        ],
        l,
        state
      )

    nAs = abst_vars([rL, fun, go] ++ iVs, l)
    tNext = next(go, goI + 1, l)
    fNext = next(go, goI, l)
    nAsT = pack_args([tNext | nAs], l, state)
    nAsF = pack_args([fNext | nAs], l, state)

    body =
      case is_guard_test(e, state) do
        true ->
          cT = {:clause, l, [], [[e]], [{:call, l, {:var, l, fun}, nAsT}]}
          cF = {:clause, l, [], [[{:atom, l, true}]], [{:call, l, {:var, l, fun}, nAsF}]}
          [{:if, l, [cT, cF]}]

        false ->
          cT = {:clause, l, [{:atom, l, true}], [], [{:call, l, {:var, l, fun}, nAsT}]}
          cF = {:clause, l, [{:atom, l, false}], [], [{:call, l, {:var, l, fun}, nAsF}]}
          [{:case, l, e, [cT, cF]}]
      end

    [{:clause, l, as, [], body}]
  end

  defp pack_args(args, l, state) do
    case split_args(args, l, state) do
      {argsL, argsT} ->
        argsL ++ [argsT]

      _ ->
        args
    end
  end

  defp split_args(args, l, state)
       when length(args) > r_state(state, :maxargs) do
    {:lists.sublist(args, r_state(state, :maxargs) - 1),
     {:tuple, l, :lists.nthtail(r_state(state, :maxargs) - 1, args)}}
  end

  defp split_args(args, _L, _State) do
    args
  end

  defp replace(es, iEs, r) do
    for e <- iEs do
      case :lists.member(e, es) do
        true ->
          r

        false ->
          e
      end
    end
  end

  defp is_list_c(v, l) do
    {:call, l, {:atom, l, :is_list}, [{:var, l, v}]}
  end

  defp next(go, goI, l) do
    {:call, l, {:atom, l, :element}, [{:integer, l, goI}, {:var, l, go}]}
  end

  defp aux_vars(vars, lcN, allVars) do
    for name <- vars do
      aux_var(name, lcN, 0, 1, allVars)
    end
  end

  defp aux_var(name, lcN, qN, n, allVars) do
    :qlc.aux_name(:lists.concat([name, lcN, :_, qN, :_]), n, allVars)
  end

  defp no_compiler_warning(l) do
    anno = :erl_anno.new(l)
    :erl_anno.set_generated(true, anno)
  end

  defp loc(a) do
    :erl_anno.location(a)
  end

  defp list2op([e], _Op) do
    e
  end

  defp list2op([e | es], op) do
    {:op, anno0(), op, e, list2op(es, op)}
  end

  defp anno0() do
    :erl_anno.new(0)
  end

  defp anno1() do
    :erl_anno.new(1)
  end

  defp qual_fold(fun, globAcc0, acc0, forms, state) do
    f = fn
      id, {:lc, l, e, qs0}, gA0 ->
        {qs, gA, _NA} = qual_fold(qs0, fun, gA0, acc0, id, 1, [])
        {{:lc, l, e, qs}, gA}

      _Id, expr, gA ->
        {expr, gA}
    end

    qlc_mapfold(f, globAcc0, forms, state)
  end

  defp qual_fold([q0 | qs], f, gA0, a0, id, no, nQs) do
    qId = qid(id, no)
    {q, gA, a} = f.(qId, q0, gA0, a0)
    qual_fold(qs, f, gA, a, id, no + 1, [q | nQs])
  end

  defp qual_fold([], _F, gA, a, _Id, _No, nQs) do
    {:lists.reverse(nQs), gA, a}
  end

  defp qlc_mapfold(fun, acc0, forms0, state) do
    {forms, a, _NNo} = qlcmf(forms0, fun, r_state(state, :imp), acc0, 1)
    :erlang.erase(:qlc_current_file)
    {forms, a}
  end

  defp qlcmf([e0 | es0], f, imp, a0, no0) do
    {e, a1, no1} = qlcmf(e0, f, imp, a0, no0)
    {es, a, no} = qlcmf(es0, f, imp, a1, no1)
    {[e | es], a, no}
  end

  defp qlcmf(
         {:call, l1, {:remote, l2, {:atom, l3, :qlc}, {:atom, l4, :q}}, [lC0 | os0]},
         f,
         imp,
         a0,
         no0
       )
       when length(os0) < 2 do
    {os, a1, no1} = qlcmf(os0, f, imp, a0, no0)
    {lC, a2, no} = qlcmf(lC0, f, imp, a1, no1)
    nL = make_lcid(l1, no)
    {t, a} = f.(nL, lC, a2)
    {{:call, l1, {:remote, l2, {:atom, l3, :qlc}, {:atom, l4, :q}}, [t | os]}, a, no + 1}
  end

  defp qlcmf({:call, l, {:atom, l2, :q}, [lC0 | os0]}, f, imp = true, a0, no0)
       when length(os0) < 2 do
    {os, a1, no1} = qlcmf(os0, f, imp, a0, no0)
    {lC, a2, no} = qlcmf(lC0, f, imp, a1, no1)
    nL = make_lcid(l, no)
    {t, a} = f.(nL, lC, a2)
    {{:call, l, {:atom, l2, :q}, [t | os]}, a, no + 1}
  end

  defp qlcmf({:attribute, _L, :file, {file, _Line}} = attr, _F, _Imp, a, no) do
    :erlang.put(:qlc_current_file, file)
    {attr, a, no}
  end

  defp qlcmf(t, f, imp, a0, no0) when is_tuple(t) do
    {tL, a, no} = qlcmf(:erlang.tuple_to_list(t), f, imp, a0, no0)
    {:erlang.list_to_tuple(tL), a, no}
  end

  defp qlcmf(t, _F, _Imp, a, no) do
    {t, a, no}
  end

  defp occ_vars(e) do
    :qlc.var_fold(
      fn {:var, _L, v} ->
        v
      end,
      [],
      e
    )
  end

  defp save_anno(abstr, nodeInfo) do
    f = fn anno ->
      n = next_slot(nodeInfo)
      location = :erl_anno.location(anno)
      data = {n, %{:location => location}}
      true = :ets.insert(nodeInfo, data)
      :erl_anno.new(n)
    end

    map_anno(f, abstr)
  end

  defp next_slot(t) do
    i = :ets.update_counter(t, :var_n, 1)

    case :ets.lookup(t, i) do
      [] ->
        i

      _ ->
        next_slot(t)
    end
  end

  defp restore_anno(abstr, nodeInfo) do
    f = fn anno ->
      location = :erl_anno.location(anno)

      case :ets.lookup(nodeInfo, location) do
        [{^location, data}] ->
          origLocation = :maps.get(:location, data)
          :erl_anno.set_location(origLocation, anno)

        [{^location}] ->
          anno

        [] ->
          anno
      end
    end

    map_anno(f, abstr)
  end

  defp restore_loc(location, r_state(node_info: nodeInfo)) do
    case :ets.lookup(nodeInfo, location) do
      [{^location, %{:location => origLocation}}] ->
        origLocation

      [{^location}] ->
        location

      [] ->
        location
    end
  end

  defp no_shadows(forms0, state) do
    allVars = :gb_sets.from_list(:ordsets.to_list(:qlc.vars(forms0)))
    :ok

    vFun = fn _Id, lC, vs ->
      nos(lC, vs)
    end

    lI = :ets.new(:qlc, [])
    uV = :ets.new(:qlc, [])
    d0 = :maps.new()
    s1 = {lI, d0, uV, allVars, [], state}
    _ = qlc_mapfold(vFun, s1, forms0, state)
    :ok

    singletons =
      :ets.select(
        uV,
        :ets.fun2ms(fn {k, 0} ->
          k
        end)
      )

    :ok
    true = :ets.delete_all_objects(lI)
    true = :ets.delete_all_objects(uV)
    s2 = {lI, d0, uV, allVars, singletons, state}
    {forms, _} = qlc_mapfold(vFun, s2, forms0, state)
    true = :ets.delete(lI)
    true = :ets.delete(uV)
    forms
  end

  defp nos([e0 | es0], s0) do
    {e, s1} = nos(e0, s0)
    {es, s} = nos(es0, s1)
    {[e | es], s}
  end

  defp nos({:fun, l, {:clauses, cs}}, s) do
    nCs =
      for {:clause, ln, h0, g0, b0} <- cs do
        {h, s1} = nos_pattern(h0, s)
        {[g, b], _} = nos([g0, b0], s1)
        {:clause, ln, h, g, b}
      end

    {{:fun, l, {:clauses, nCs}}, s}
  end

  defp nos({:named_fun, loc, name, cs}, s) do
    {{:var, nLoc, nName}, s1} =
      case name do
        :_ ->
          s

        ^name ->
          nos_pattern({:var, loc, name}, s)
      end

    nCs =
      for {:clause, cLoc, h0, g0, b0} <- cs do
        {h, s2} = nos_pattern(h0, s1)
        {[g, b], _} = nos([g0, b0], s2)
        {:clause, cLoc, h, g, b}
      end

    {{:named_fun, nLoc, nName, nCs}, s}
  end

  defp nos({:lc, l, e0, qs0}, s) do
    f = fn
      {t, ln, p0, lE0}, qS0
      when t === :b_generate or
             t === :generate ->
        {lE, _} = nos(lE0, qS0)
        {p, qS} = nos_pattern(p0, qS0)
        {{t, ln, p, lE}, qS}

      filter, qS ->
        nos(filter, qS)
    end

    {qs, s1} = :lists.mapfoldl(f, s, qs0)
    {e, _} = nos(e0, s1)
    {{:lc, l, e, qs}, s}
  end

  defp nos(
         {:var, l, v} = var,
         {_LI, vs, uV, _A, _Sg, state} = s
       )
       when v !== :_ do
    case used_var(v, vs, uV) do
      {true, vN} ->
        nos_var(l, v, state)
        {{:var, l, vN}, s}

      false ->
        {var, s}
    end
  end

  defp nos(t, s0) when is_tuple(t) do
    {tL, s} = nos(:erlang.tuple_to_list(t), s0)
    {:erlang.list_to_tuple(tL), s}
  end

  defp nos(t, s) do
    {t, s}
  end

  defp nos_pattern(p, s) do
    {t, nS, _} = nos_pattern(p, s, [])
    {t, nS}
  end

  defp nos_pattern([p0 | ps0], s0, pVs0) do
    {p, s1, pVs1} = nos_pattern(p0, s0, pVs0)
    {ps, s, pVs} = nos_pattern(ps0, s1, pVs1)
    {[p | ps], s, pVs}
  end

  defp nos_pattern({:var, l, v}, {lI, vs0, uV, a, sg, state}, pVs0)
       when v !== :_ do
    {name, vs, pVs} =
      case :lists.keyfind(v, 1, pVs0) do
        {^v, vN} ->
          _ = used_var(v, vs0, uV)
          {vN, vs0, pVs0}

        false ->
          {vN, vs1} = next_var(v, vs0, a, lI, uV)

          n =
            case :lists.member(vN, sg) do
              true ->
                :_

              false ->
                vN
            end

          {n, vs1, [{v, vN} | pVs0]}
      end

    nos_var(l, v, state)
    {{:var, l, name}, {lI, vs, uV, a, sg, state}, pVs}
  end

  defp nos_pattern(t, s0, pVs0) when is_tuple(t) do
    {tL, s, pVs} = nos_pattern(:erlang.tuple_to_list(t), s0, pVs0)
    {:erlang.list_to_tuple(tL), s, pVs}
  end

  defp nos_pattern(t, s, pVs) do
    {t, s, pVs}
  end

  defp nos_var(anno, name, state) do
    nodeInfo = r_state(state, :node_info)
    location = :erl_anno.location(anno)

    case :ets.lookup(nodeInfo, location) do
      [{^location, %{:name => _}}] ->
        true

      [{^location, data}] ->
        true =
          :ets.insert(
            nodeInfo,
            {location, %{data | :name => name}}
          )

      [] ->
        true
    end
  end

  defp used_var(v, vs, uV) do
    case :maps.find(v, vs) do
      {:ok, value} ->
        vN = :qlc.name_suffix(v, value)
        _ = :ets.update_counter(uV, vN, 1)
        {true, vN}

      :error ->
        false
    end
  end

  defp next_var(v, vs, allVars, lI, uV) do
    nValue =
      case :ets.lookup(lI, v) do
        [{^v, value}] ->
          value + 1

        [] ->
          1
      end

    true = :ets.insert(lI, {v, nValue})
    vN = :qlc.name_suffix(v, nValue)

    case :gb_sets.is_member(vN, allVars) do
      true ->
        next_var(v, vs, allVars, lI, uV)

      false ->
        true = :ets.insert(uV, {vN, 0})
        nVs = :maps.put(v, nValue, vs)
        {vN, nVs}
    end
  end

  defp undo_no_shadows(e, state) do
    var_map(
      fn anno ->
        undo_no_shadows1(anno, state)
      end,
      e
    )
  end

  defp undo_no_shadows1({:var, anno, _} = var, state) do
    location = :erl_anno.location(anno)
    nodeInfo = r_state(state, :node_info)

    case :ets.lookup(nodeInfo, location) do
      [{^location, %{:name => name}}] ->
        {:var, anno, name}

      _ ->
        var
    end
  end

  defp make_lcid(anno, no) when is_integer(no) and no > 0 do
    {no, :erl_anno.line(anno)}
  end

  defp get_lcid_no({no, _Line}) do
    no
  end

  defp get_lcid_line({_No, line}) do
    line
  end

  defp qid(lCId, no) do
    r_qid(no: no, lcid: lCId)
  end

  defp abst_vars([v | vs], l) do
    [abst_vars(v, l) | abst_vars(vs, l)]
  end

  defp abst_vars([], _L) do
    []
  end

  defp abst_vars(nil, l) do
    {nil, l}
  end

  defp abst_vars(v, l) do
    {:var, l, v}
  end

  defp embed_vars(vars, l) do
    embed_expr({:tuple, l, vars}, l)
  end

  defp embed_expr(expr, l) do
    {:lc, l, expr, [{:generate, l, {:var, l, :_}, {nil, l}}]}
  end

  defp var2const(e) do
    var_map(
      fn {:var, l, v} ->
        {:atom, l, v}
      end,
      e
    )
  end

  defp var_map(f, {:var, _, _} = v) do
    f.(v)
  end

  defp var_map(f, {:named_fun, nLoc, nName, cs}) do
    {:var, loc, name} = f.({:var, nLoc, nName})
    {:named_fun, loc, name, var_map(f, cs)}
  end

  defp var_map(f, t) when is_tuple(t) do
    :erlang.list_to_tuple(
      var_map(
        f,
        :erlang.tuple_to_list(t)
      )
    )
  end

  defp var_map(f, [e | es]) do
    [var_map(f, e) | var_map(f, es)]
  end

  defp var_map(_F, e) do
    e
  end

  defp var_mapfold(f, a, {:var, _, _} = v) do
    f.(v, a)
  end

  defp var_mapfold(f, a0, t) when is_tuple(t) do
    {l, a} = var_mapfold(f, a0, :erlang.tuple_to_list(t))
    {:erlang.list_to_tuple(l), a}
  end

  defp var_mapfold(f, a0, [e0 | es0]) do
    {e, a1} = var_mapfold(f, a0, e0)
    {es, a} = var_mapfold(f, a1, es0)
    {[e | es], a}
  end

  defp var_mapfold(_F, a, e) do
    {e, a}
  end

  defp map_anno(f, abstrList) when is_list(abstrList) do
    for abstr <- abstrList do
      map_anno1(f, abstr)
    end
  end

  defp map_anno(f, abstr) do
    map_anno1(f, abstr)
  end

  defp map_anno1(f, abstr) do
    :erl_parse.map_anno(f, abstr)
  end

  defp family_list(l) do
    :sofs.to_external(family(l))
  end

  defp family(l) do
    :sofs.relation_to_family(:sofs.relation(l))
  end

  defp is_guard_test(e, r_state(records: rDs, overridden: isOverridden)) do
    :erl_lint.is_guard_test(e, rDs, isOverridden)
  end

  defp set_up_overridden(forms) do
    locals =
      for {:function, _, name, arity, _} <- forms do
        {name, arity}
      end

    imports0 =
      for {:attribute, _, :import, fs} <- forms do
        fs
      end

    imports1 = :lists.flatten(imports0)

    imports2 =
      for {_, fs} <- imports1 do
        fs
      end

    imports = :lists.flatten(imports2)
    overridden = :gb_sets.from_list(imports ++ locals)

    fn fA ->
      :gb_sets.is_element(fA, overridden)
    end
  end

  defp display_forms(_) do
    :ok
  end
end
