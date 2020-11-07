defmodule :m_dialyzer_behaviours do
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

  Record.defrecord(:r_state, :state,
    plt: :undefined,
    codeserver: :undefined,
    filename: :undefined,
    behlines: :undefined,
    records: :undefined
  )

  def check_callbacks(module, attrs, records, plt, codeserver) do
    {behaviours, behLines} = get_behaviours(attrs)

    case behaviours do
      [] ->
        []

      _ ->
        mFA = {module, :module_info, 0}

        {_Var, code} =
          :dialyzer_codeserver.lookup_mfa_code(
            mFA,
            codeserver
          )

        file = get_file(codeserver, module, :cerl.get_ann(code))

        state =
          r_state(
            plt: plt,
            filename: file,
            behlines: behLines,
            codeserver: codeserver,
            records: records
          )

        warnings = get_warnings(module, behaviours, state)

        for w <- warnings do
          add_tag_warning_info(module, w, state)
        end
    end
  end

  defp get_behaviours(attrs) do
    behaviourListsAndLine =
      for {l1, l2} <- attrs,
          :cerl.is_literal(l1),
          :cerl.is_literal(l2),
          :cerl.concrete(l1) === :behaviour or :cerl.concrete(l1) === :behavior do
        {:cerl.concrete(l2), hd(:cerl.get_ann(l2))}
      end

    behaviours =
      :lists.append(
        for {behs, _} <- behaviourListsAndLine do
          behs
        end
      )

    behLines =
      for {l1, l} <- behaviourListsAndLine,
          b <- l1 do
        {b, l}
      end

    {behaviours, behLines}
  end

  defp get_warnings(module, behaviours, state) do
    get_warnings(module, behaviours, state, [])
  end

  defp get_warnings(_, [], _, acc) do
    acc
  end

  defp get_warnings(module, [behaviour | rest], state, acc) do
    newAcc = check_behaviour(module, behaviour, state, acc)
    get_warnings(module, rest, state, newAcc)
  end

  defp check_behaviour(module, behaviour, r_state(plt: plt) = state, acc) do
    case :dialyzer_plt.lookup_callbacks(plt, behaviour) do
      :none ->
        [{:callback_info_missing, [behaviour]} | acc]

      {:value, callbacks} ->
        check_all_callbacks(module, behaviour, callbacks, state, acc)
    end
  end

  defp check_all_callbacks(_Module, _Behaviour, [], _State, acc) do
    acc
  end

  defp check_all_callbacks(
         module,
         behaviour,
         [cb | rest],
         r_state(plt: plt, codeserver: codeserver, records: records) = state,
         acc
       ) do
    {{^behaviour, function, arity}, {{_BehFile, _BehLine}, callback, xtra}} = cb
    cbMFA = {module, function, arity}
    cbReturnType = :dialyzer_contracts.get_contract_return(callback)
    cbArgTypes = :dialyzer_contracts.get_contract_args(callback)
    acc0 = acc

    acc1 =
      case :dialyzer_plt.lookup(plt, cbMFA) do
        :none ->
          case :lists.member(:optional_callback, xtra) do
            true ->
              acc0

            false ->
              [
                {:callback_missing, [behaviour, function, arity]}
                | acc0
              ]
          end

        {:value, retArgTypes} ->
          acc00 = acc0
          {returnType, argTypes} = retArgTypes

          acc01 =
            case :erl_types.t_is_subtype(
                   returnType,
                   cbReturnType
                 ) do
              true ->
                acc00

              false ->
                case :erl_types.t_is_none(
                       :erl_types.t_inf(
                         returnType,
                         cbReturnType
                       )
                     ) do
                  false ->
                    acc00

                  true ->
                    [
                      {:callback_type_mismatch,
                       [
                         behaviour,
                         function,
                         arity,
                         :erl_types.t_to_string(
                           returnType,
                           records
                         ),
                         :erl_types.t_to_string(
                           cbReturnType,
                           records
                         )
                       ]}
                      | acc00
                    ]
                end
            end

          case :erl_types.any_none(
                 :erl_types.t_inf_lists(
                   argTypes,
                   cbArgTypes
                 )
               ) do
            false ->
              acc01

            true ->
              find_mismatching_args(
                :type,
                argTypes,
                cbArgTypes,
                behaviour,
                function,
                arity,
                records,
                1,
                acc01
              )
          end
      end

    acc2 =
      case :dialyzer_codeserver.lookup_mfa_contract(
             cbMFA,
             codeserver
           ) do
        :error ->
          acc1

        {:ok, {{file, line}, contract, _Xtra}} ->
          acc10 = acc1
          specReturnType0 = :dialyzer_contracts.get_contract_return(contract)
          specArgTypes0 = :dialyzer_contracts.get_contract_args(contract)
          specReturnType = :erl_types.subst_all_vars_to_any(specReturnType0)

          specArgTypes =
            for argT0 <- specArgTypes0 do
              :erl_types.subst_all_vars_to_any(argT0)
            end

          acc11 =
            case :erl_types.t_is_subtype(
                   specReturnType,
                   cbReturnType
                 ) do
              true ->
                acc10

              false ->
                extraType =
                  :erl_types.t_subtract(
                    specReturnType,
                    cbReturnType
                  )

                [
                  {:callback_spec_type_mismatch,
                   [
                     file,
                     line,
                     behaviour,
                     function,
                     arity,
                     :erl_types.t_to_string(extraType, records),
                     :erl_types.t_to_string(
                       cbReturnType,
                       records
                     )
                   ]}
                  | acc10
                ]
            end

          case :erl_types.any_none(
                 :erl_types.t_inf_lists(
                   specArgTypes,
                   cbArgTypes
                 )
               ) do
            false ->
              acc11

            true ->
              find_mismatching_args(
                {:spec, file, line},
                specArgTypes,
                cbArgTypes,
                behaviour,
                function,
                arity,
                records,
                1,
                acc11
              )
          end
      end

    newAcc = acc2
    check_all_callbacks(module, behaviour, rest, state, newAcc)
  end

  defp find_mismatching_args(_, [], [], _Beh, _Function, _Arity, _Records, _N, acc) do
    acc
  end

  defp find_mismatching_args(
         kind,
         [type | rest],
         [cbType | cbRest],
         behaviour,
         function,
         arity,
         records,
         n,
         acc
       ) do
    case :erl_types.t_is_none(
           :erl_types.t_inf(
             type,
             cbType
           )
         ) do
      false ->
        find_mismatching_args(kind, rest, cbRest, behaviour, function, arity, records, n + 1, acc)

      true ->
        info = [
          behaviour,
          function,
          arity,
          n,
          :erl_types.t_to_string(type, records),
          :erl_types.t_to_string(cbType, records)
        ]

        newAcc = [
          case kind do
            :type ->
              {:callback_arg_type_mismatch, info}

            {:spec, file, line} ->
              {:callback_spec_arg_type_mismatch, [[file, line] | info]}
          end
          | acc
        ]

        find_mismatching_args(
          kind,
          rest,
          cbRest,
          behaviour,
          function,
          arity,
          records,
          n + 1,
          newAcc
        )
    end
  end

  defp add_tag_warning_info(module, {tag, [b | _R]} = warn, state)
       when tag === :callback_missing or
              tag === :callback_info_missing do
    {^b, line} = :lists.keyfind(b, 1, r_state(state, :behlines))

    category =
      case tag do
        :callback_missing ->
          :warn_behaviour

        :callback_info_missing ->
          :warn_undefined_callbacks
      end

    {category, {r_state(state, :filename), line, module}, warn}
  end

  defp add_tag_warning_info(module, {tag, [[file, line] | r]}, _State)
       when tag === :callback_spec_type_mismatch or
              tag === :callback_spec_arg_type_mismatch do
    {:warn_behaviour, {file, line, module}, {tag, r}}
  end

  defp add_tag_warning_info(module, {_Tag, [[_B, fun, arity] | _R]} = warn, state) do
    {_A, funCode} =
      :dialyzer_codeserver.lookup_mfa_code(
        {module, fun, arity},
        r_state(state, :codeserver)
      )

    anns = :cerl.get_ann(funCode)
    file = get_file(r_state(state, :codeserver), module, anns)
    warningInfo = {file, get_line(anns), {module, fun, arity}}
    {:warn_behaviour, warningInfo, warn}
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
end
