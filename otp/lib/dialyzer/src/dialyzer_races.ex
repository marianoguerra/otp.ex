defmodule :m_dialyzer_races do
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

  Record.defrecord(:r_beg_clause, :beg_clause,
    arg: :undefined,
    pats: :undefined,
    guard: :undefined
  )

  Record.defrecord(:r_end_clause, :end_clause,
    arg: :undefined,
    pats: :undefined,
    guard: :undefined
  )

  Record.defrecord(:r_end_case, :end_case, clauses: :undefined)

  Record.defrecord(:r_curr_fun, :curr_fun,
    status: :undefined,
    mfa: :undefined,
    label: :undefined,
    def_vars: :undefined,
    arg_types: :undefined,
    call_vars: :undefined,
    var_map: :undefined
  )

  Record.defrecord(:r_dep_call, :dep_call,
    call_name: :undefined,
    args: :undefined,
    arg_types: :undefined,
    vars: :undefined,
    state: :undefined,
    file_line: :undefined,
    var_map: :undefined
  )

  Record.defrecord(:r_fun_call, :fun_call,
    caller: :undefined,
    callee: :undefined,
    arg_types: :undefined,
    vars: :undefined
  )

  Record.defrecord(:r_let_tag, :let_tag,
    var: :undefined,
    arg: :undefined
  )

  Record.defrecord(:r_warn_call, :warn_call,
    call_name: :undefined,
    args: :undefined,
    var_map: :undefined
  )

  Record.defrecord(:r_race_fun, :race_fun,
    mfa: :undefined,
    args: :undefined,
    arg_types: :undefined,
    vars: :undefined,
    file_line: :undefined,
    index: :undefined,
    fun_mfa: :undefined,
    fun_label: :undefined
  )

  Record.defrecord(:r_races, :races,
    curr_fun: :undefined,
    curr_fun_label: :undefined,
    curr_fun_args: :empty,
    new_table: :no_t,
    race_list: [],
    race_list_size: 0,
    race_tags: [],
    race_analysis: false,
    race_warnings: []
  )

  def store_race_call(fun, argTypes, args, fileLine, state) do
    races = :dialyzer_dataflow.state__get_races(state)
    currFun = r_races(races, :curr_fun)
    currFunLabel = r_races(races, :curr_fun_label)
    raceTags = r_races(races, :race_tags)
    cleanState = :dialyzer_dataflow.state__records_only(state)

    {newRaceList, newRaceListSize, newRaceTags, newTable} =
      case currFun do
        {_Module, :module_info, a} when a === 0 or a === 1 ->
          {[], 0, raceTags, :no_t}

        _Thing ->
          raceList = r_races(races, :race_list)
          raceListSize = r_races(races, :race_list_size)

          case fun do
            {:erlang, :get_module_info, a} when a === 1 or a === 2 ->
              {[], 0, raceTags, :no_t}

            {:erlang, :register, 2} ->
              varArgs = format_args(args, argTypes, cleanState, :register)

              raceFun =
                r_race_fun(
                  mfa: fun,
                  args: varArgs,
                  arg_types: argTypes,
                  vars: args,
                  file_line: fileLine,
                  index: raceListSize,
                  fun_mfa: currFun,
                  fun_label: currFunLabel
                )

              {[
                 r_warn_call(call_name: :register, args: varArgs)
                 | raceList
               ], raceListSize + 1, [raceFun | raceTags], :no_t}

            {:erlang, :unregister, 1} ->
              varArgs = format_args(args, argTypes, cleanState, :unregister)

              raceFun =
                r_race_fun(
                  mfa: fun,
                  args: varArgs,
                  arg_types: argTypes,
                  vars: args,
                  file_line: fileLine,
                  index: raceListSize,
                  fun_mfa: currFun,
                  fun_label: currFunLabel
                )

              {[
                 r_warn_call(call_name: :unregister, args: varArgs)
                 | raceList
               ], raceListSize + 1, [raceFun | raceTags], :no_t}

            {:erlang, :whereis, 1} ->
              varArgs = format_args(args, argTypes, cleanState, :whereis)

              {[
                 r_dep_call(
                   call_name: :whereis,
                   args: varArgs,
                   arg_types: argTypes,
                   vars: args,
                   state: cleanState,
                   file_line: fileLine
                 )
                 | raceList
               ], raceListSize + 1, raceTags, :no_t}

            {:ets, :insert, 2} ->
              varArgs = format_args(args, argTypes, cleanState, :ets_insert)

              raceFun =
                r_race_fun(
                  mfa: fun,
                  args: varArgs,
                  arg_types: argTypes,
                  vars: args,
                  file_line: fileLine,
                  index: raceListSize,
                  fun_mfa: currFun,
                  fun_label: currFunLabel
                )

              {[
                 r_warn_call(call_name: :ets_insert, args: varArgs)
                 | raceList
               ], raceListSize + 1, [raceFun | raceTags], :no_t}

            {:ets, :lookup, 2} ->
              varArgs = format_args(args, argTypes, cleanState, :ets_lookup)

              {[
                 r_dep_call(
                   call_name: :ets_lookup,
                   args: varArgs,
                   arg_types: argTypes,
                   vars: args,
                   state: cleanState,
                   file_line: fileLine
                 )
                 | raceList
               ], raceListSize + 1, raceTags, :no_t}

            {:ets, :new, 2} ->
              varArgs = format_args(args, argTypes, cleanState, :ets_new)
              [varArgs1, varArgs2, _, options] = varArgs

              newTable1 =
                case :lists.member('\'public\'', options) do
                  true ->
                    case :lists.member('\'named_table\'', options) do
                      true ->
                        {:named, varArgs1, varArgs2}

                      false ->
                        :other
                    end

                  false ->
                    :no_t
                end

              {raceList, raceListSize, raceTags, newTable1}

            {:mnesia, :dirty_read, a} when a === 1 or a === 2 ->
              varArgs =
                case a do
                  1 ->
                    format_args(args, argTypes, cleanState, :mnesia_dirty_read1)

                  2 ->
                    format_args(args, argTypes, cleanState, :mnesia_dirty_read2)
                end

              {[
                 r_dep_call(
                   call_name: :mnesia_dirty_read,
                   args: varArgs,
                   arg_types: argTypes,
                   vars: args,
                   state: cleanState,
                   file_line: fileLine
                 )
                 | raceList
               ], raceListSize + 1, raceTags, :no_t}

            {:mnesia, :dirty_write, a} when a === 1 or a === 2 ->
              varArgs =
                case a do
                  1 ->
                    format_args(args, argTypes, cleanState, :mnesia_dirty_write1)

                  2 ->
                    format_args(args, argTypes, cleanState, :mnesia_dirty_write2)
                end

              raceFun =
                r_race_fun(
                  mfa: fun,
                  args: varArgs,
                  arg_types: argTypes,
                  vars: args,
                  file_line: fileLine,
                  index: raceListSize,
                  fun_mfa: currFun,
                  fun_label: currFunLabel
                )

              {[
                 r_warn_call(call_name: :mnesia_dirty_write, args: varArgs)
                 | raceList
               ], raceListSize + 1, [raceFun | raceTags], :no_t}

            int when is_integer(int) ->
              {[
                 r_fun_call(caller: currFun, callee: int, arg_types: argTypes, vars: args)
                 | raceList
               ], raceListSize + 1, raceTags, :no_t}

            _Other ->
              callgraph = :dialyzer_dataflow.state__get_callgraph(state)

              case :digraph.vertex(
                     :dialyzer_callgraph.get_digraph(callgraph),
                     fun
                   ) do
                {^fun, :confirmed} ->
                  {[
                     r_fun_call(caller: currFun, callee: fun, arg_types: argTypes, vars: args)
                     | raceList
                   ], raceListSize + 1, raceTags, :no_t}

                false ->
                  {raceList, raceListSize, raceTags, :no_t}
              end
          end
      end

    state__renew_info(newRaceList, newRaceListSize, newRaceTags, newTable, state)
  end

  def race(state) do
    races = :dialyzer_dataflow.state__get_races(state)
    raceTags = r_races(races, :race_tags)

    retState =
      case raceTags do
        [] ->
          state

        [
          r_race_fun(
            mfa: fun,
            args: varArgs,
            arg_types: argTypes,
            vars: args,
            file_line: fileLine,
            index: index,
            fun_mfa: currFun,
            fun_label: currFunLabel
          )
          | t
        ] ->
          callgraph = :dialyzer_dataflow.state__get_callgraph(state)

          {:ok, [_Args, code]} =
            :dict.find(
              currFun,
              :dialyzer_callgraph.get_race_code(callgraph)
            )

          raceList = :lists.reverse(code)

          raceWarnTag =
            case fun do
              {:erlang, :register, 2} ->
                :warn_whereis_register

              {:erlang, :unregister, 1} ->
                :warn_whereis_unregister

              {:ets, :insert, 2} ->
                :warn_ets_lookup_insert

              {:mnesia, :dirty_write, _A} ->
                :warn_mnesia_dirty_read_write
            end

          state1 =
            state__renew_curr_fun(
              currFun,
              state__renew_curr_fun_label(
                currFunLabel,
                state__renew_race_list(
                  :lists.nthtail(
                    length(raceList) - index,
                    raceList
                  ),
                  state
                )
              )
            )

          depList = fixup_race_list(raceWarnTag, varArgs, state1)
          {state2, raceWarn} = get_race_warn(fun, args, argTypes, depList, state)
          {file, line} = fileLine

          currMFA =
            :dialyzer_dataflow.state__find_function(
              currFun,
              state
            )

          warningInfo = {file, line, currMFA}

          race(
            state__add_race_warning(
              state__renew_race_tags(
                t,
                state2
              ),
              raceWarn,
              raceWarnTag,
              warningInfo
            )
          )
      end

    state__renew_race_tags([], retState)
  end

  defp fixup_race_list(raceWarnTag, warnVarArgs, state) do
    races = :dialyzer_dataflow.state__get_races(state)
    currFun = r_races(races, :curr_fun)
    currFunLabel = r_races(races, :curr_fun_label)
    raceList = r_races(races, :race_list)
    callgraph = :dialyzer_dataflow.state__get_callgraph(state)
    digraph = :dialyzer_callgraph.get_digraph(callgraph)
    calls = :digraph.edges(digraph)

    raceTag =
      case raceWarnTag do
        :warn_whereis_register ->
          :whereis_register

        :warn_whereis_unregister ->
          :whereis_unregister

        :warn_ets_lookup_insert ->
          :ets_lookup_insert

        :warn_mnesia_dirty_read_write ->
          :mnesia_dirty_read_write
      end

    newRaceList = [raceTag | raceList]
    cleanState = :dialyzer_dataflow.state__cleanup(state)

    newState =
      state__renew_race_list(
        newRaceList,
        cleanState
      )

    depList1 =
      fixup_race_forward_pullout(
        currFun,
        currFunLabel,
        calls,
        :lists.reverse(newRaceList),
        [],
        currFun,
        warnVarArgs,
        raceWarnTag,
        :dict.new(),
        [],
        [],
        [],
        2 * 5,
        newState
      )

    parents = fixup_race_backward(currFun, calls, calls, [], 5)
    uParents = :lists.usort(parents)
    filtered = filter_parents(uParents, uParents, digraph)

    newParents =
      case :lists.member(currFun, filtered) do
        true ->
          filtered

        false ->
          [currFun | filtered]
      end

    depList2 =
      fixup_race_list_helper(newParents, calls, currFun, warnVarArgs, raceWarnTag, newState)

    :dialyzer_dataflow.dispose_state(cleanState)
    :lists.usort(cleanup_dep_calls(depList1 ++ depList2))
  end

  defp fixup_race_list_helper(parents, calls, currFun, warnVarArgs, raceWarnTag, state) do
    case parents do
      [] ->
        []

      [head | tail] ->
        callgraph = :dialyzer_dataflow.state__get_callgraph(state)

        code =
          case :dict.find(
                 head,
                 :dialyzer_callgraph.get_race_code(callgraph)
               ) do
            :error ->
              []

            {:ok, [_A, c]} ->
              c
          end

        {:ok, funLabel} =
          :dialyzer_callgraph.lookup_label(
            head,
            callgraph
          )

        depList1 =
          fixup_race_forward_pullout(
            head,
            funLabel,
            calls,
            code,
            [],
            currFun,
            warnVarArgs,
            raceWarnTag,
            :dict.new(),
            [],
            [],
            [],
            2 * 5,
            state
          )

        depList2 = fixup_race_list_helper(tail, calls, currFun, warnVarArgs, raceWarnTag, state)
        depList1 ++ depList2
    end
  end

  defp fixup_race_forward_pullout(
         currFun,
         currFunLabel,
         calls,
         code,
         raceList,
         initFun,
         warnVarArgs,
         raceWarnTag,
         raceVarMap,
         funDefVars,
         funCallVars,
         funArgTypes,
         nestingLevel,
         state
       ) do
    tState = :dialyzer_dataflow.state__duplicate(state)

    {depList, newCurrFun, newCurrFunLabel, newCalls, newCode, newRaceList, newRaceVarMap,
     newFunDefVars, newFunCallVars, newFunArgTypes,
     newNestingLevel} =
      fixup_race_forward(
        currFun,
        currFunLabel,
        calls,
        code,
        raceList,
        initFun,
        warnVarArgs,
        raceWarnTag,
        raceVarMap,
        funDefVars,
        funCallVars,
        funArgTypes,
        nestingLevel,
        cleanup_race_code(tState)
      )

    :dialyzer_dataflow.dispose_state(tState)

    case newCode do
      [] ->
        depList

      [
        r_fun_call(caller: ^newCurrFun, callee: call, arg_types: funTypes, vars: funArgs)
        | tail
      ] ->
        callgraph = :dialyzer_dataflow.state__get_callgraph(state)
        okCall = {:ok, call}

        {name, label} =
          case is_integer(call) do
            true ->
              case :dialyzer_callgraph.lookup_name(
                     call,
                     callgraph
                   ) do
                :error ->
                  {okCall, okCall}

                n ->
                  {n, okCall}
              end

            false ->
              {okCall,
               :dialyzer_callgraph.lookup_label(
                 call,
                 callgraph
               )}
          end

        {newCurrFun1, newCurrFunLabel1, newCalls1, newCode1, newRaceList1, newRaceVarMap1,
         newFunDefVars1, newFunCallVars1, newFunArgTypes1,
         newNestingLevel1} =
          case label === :error do
            true ->
              {newCurrFun, newCurrFunLabel, newCalls, tail, newRaceList, newRaceVarMap,
               newFunDefVars, newFunCallVars, newFunArgTypes, newNestingLevel}

            false ->
              {:ok, fun} = name
              {:ok, int} = label

              case :dict.find(
                     fun,
                     :dialyzer_callgraph.get_race_code(callgraph)
                   ) do
                :error ->
                  {newCurrFun, newCurrFunLabel, newCalls, tail, newRaceList, newRaceVarMap,
                   newFunDefVars, newFunCallVars, newFunArgTypes, newNestingLevel}

                {:ok, [args, codeB]} ->
                  races = :dialyzer_dataflow.state__get_races(state)

                  {retCurrFun, retCurrFunLabel, retCalls, retCode, retRaceList, retRaceVarMap,
                   retFunDefVars, retFunCallVars, retFunArgTypes,
                   retNestingLevel} =
                    fixup_race_forward_helper(
                      newCurrFun,
                      newCurrFunLabel,
                      fun,
                      int,
                      newCalls,
                      newCalls,
                      [
                        r_curr_fun(
                          status: :out,
                          mfa: newCurrFun,
                          label: newCurrFunLabel,
                          var_map: newRaceVarMap,
                          def_vars: newFunDefVars,
                          call_vars: newFunCallVars,
                          arg_types: newFunArgTypes
                        )
                        | tail
                      ],
                      newRaceList,
                      initFun,
                      funArgs,
                      funTypes,
                      raceWarnTag,
                      newRaceVarMap,
                      newFunDefVars,
                      newFunCallVars,
                      newFunArgTypes,
                      newNestingLevel,
                      args,
                      codeB,
                      r_races(races, :race_list)
                    )

                  case retCode do
                    [r_curr_fun() | _CodeTail] ->
                      {newCurrFun, newCurrFunLabel, retCalls, retCode, retRaceList, newRaceVarMap,
                       newFunDefVars, newFunCallVars, newFunArgTypes, retNestingLevel}

                    _Else ->
                      {retCurrFun, retCurrFunLabel, retCalls, retCode, retRaceList, retRaceVarMap,
                       retFunDefVars, retFunCallVars, retFunArgTypes, retNestingLevel}
                  end
              end
          end

        depList ++
          fixup_race_forward_pullout(
            newCurrFun1,
            newCurrFunLabel1,
            newCalls1,
            newCode1,
            newRaceList1,
            initFun,
            warnVarArgs,
            raceWarnTag,
            newRaceVarMap1,
            newFunDefVars1,
            newFunCallVars1,
            newFunArgTypes1,
            newNestingLevel1,
            state
          )
    end
  end

  defp fixup_race_forward(
         currFun,
         currFunLabel,
         calls,
         code,
         raceList,
         initFun,
         warnVarArgs,
         raceWarnTag,
         raceVarMap,
         funDefVars,
         funCallVars,
         funArgTypes,
         nestingLevel,
         state
       ) do
    case code do
      [] ->
        {[], currFun, currFunLabel, calls, code, raceList, raceVarMap, funDefVars, funCallVars,
         funArgTypes, nestingLevel}

      [head | tail] ->
        callgraph = :dialyzer_dataflow.state__get_callgraph(state)

        {newRL, depList, newNL, return} =
          case head do
            r_dep_call(call_name: :whereis) ->
              case raceWarnTag do
                warnWhereis
                when warnWhereis === :warn_whereis_register or
                       warnWhereis === :warn_whereis_unregister ->
                  {[
                     r_dep_call(head, var_map: raceVarMap)
                     | raceList
                   ], [], nestingLevel, false}

                _Other ->
                  {raceList, [], nestingLevel, false}
              end

            r_dep_call(call_name: :ets_lookup) ->
              case raceWarnTag do
                :warn_ets_lookup_insert ->
                  {[
                     r_dep_call(head, var_map: raceVarMap)
                     | raceList
                   ], [], nestingLevel, false}

                _Other ->
                  {raceList, [], nestingLevel, false}
              end

            r_dep_call(call_name: :mnesia_dirty_read) ->
              case raceWarnTag do
                :warn_mnesia_dirty_read_write ->
                  {[
                     r_dep_call(head, var_map: raceVarMap)
                     | raceList
                   ], [], nestingLevel, false}

                _Other ->
                  {raceList, [], nestingLevel, false}
              end

            r_warn_call(call_name: regCall)
            when regCall === :register or regCall === :unregister ->
              case raceWarnTag do
                warnWhereis
                when warnWhereis === :warn_whereis_register or
                       warnWhereis === :warn_whereis_unregister ->
                  {[
                     r_warn_call(head, var_map: raceVarMap)
                     | raceList
                   ], [], nestingLevel, false}

                _Other ->
                  {raceList, [], nestingLevel, false}
              end

            r_warn_call(call_name: :ets_insert) ->
              case raceWarnTag do
                :warn_ets_lookup_insert ->
                  {[
                     r_warn_call(head, var_map: raceVarMap)
                     | raceList
                   ], [], nestingLevel, false}

                _Other ->
                  {raceList, [], nestingLevel, false}
              end

            r_warn_call(call_name: :mnesia_dirty_write) ->
              case raceWarnTag do
                :warn_mnesia_dirty_read_write ->
                  {[
                     r_warn_call(head, var_map: raceVarMap)
                     | raceList
                   ], [], nestingLevel, false}

                _Other ->
                  {raceList, [], nestingLevel, false}
              end

            r_fun_call(
              caller: ^currFun,
              callee: ^initFun
            ) ->
              {raceList, [], nestingLevel, false}

            r_fun_call(caller: ^currFun) ->
              {raceList, [], nestingLevel - 1, false}

            :beg_case ->
              {[head | raceList], [], nestingLevel, false}

            r_beg_clause() ->
              {[r_beg_clause() | raceList], [], nestingLevel, false}

            r_end_clause() ->
              {[r_end_clause() | raceList], [], nestingLevel, false}

            r_end_case() ->
              {[head | raceList], [], nestingLevel, false}

            r_let_tag() ->
              {raceList, [], nestingLevel, false}

            r_curr_fun(
              status: :in,
              mfa: ^initFun,
              label: _InitFunLabel,
              var_map: _NewRVM,
              def_vars: newFDV,
              call_vars: newFCV,
              arg_types: _NewFAT
            ) ->
              {[
                 r_curr_fun(
                   status: :out,
                   var_map: raceVarMap,
                   def_vars: newFDV,
                   call_vars: newFCV
                 )
                 | raceList
               ], [], nestingLevel - 1, false}

            r_curr_fun(status: :in, def_vars: newFDV, call_vars: newFCV) ->
              {[
                 r_curr_fun(
                   status: :out,
                   var_map: raceVarMap,
                   def_vars: newFDV,
                   call_vars: newFCV
                 )
                 | raceList
               ], [], nestingLevel - 1, false}

            r_curr_fun(status: :out) ->
              {[
                 r_curr_fun(
                   status: :in,
                   var_map: raceVarMap
                 )
                 | raceList
               ], [], nestingLevel + 1, false}

            raceTag ->
              publicTables = :dialyzer_callgraph.get_public_tables(callgraph)
              namedTables = :dialyzer_callgraph.get_named_tables(callgraph)

              warnVarArgs1 =
                var_type_analysis(
                  funDefVars,
                  funArgTypes,
                  warnVarArgs,
                  raceWarnTag,
                  raceVarMap,
                  :dialyzer_dataflow.state__records_only(state)
                )

              {newDepList, isPublic, _Return} =
                get_deplist_paths(
                  raceList,
                  warnVarArgs1,
                  raceWarnTag,
                  raceVarMap,
                  0,
                  publicTables,
                  namedTables
                )

              {newHead, newDepList1} =
                case raceTag do
                  :whereis_register ->
                    {[
                       r_warn_call(
                         call_name: :register,
                         args: warnVarArgs,
                         var_map: raceVarMap
                       )
                     ], newDepList}

                  :whereis_unregister ->
                    {[
                       r_warn_call(
                         call_name: :unregister,
                         args: warnVarArgs,
                         var_map: raceVarMap
                       )
                     ], newDepList}

                  :ets_lookup_insert ->
                    newWarnCall = [
                      r_warn_call(
                        call_name: :ets_insert,
                        args: warnVarArgs,
                        var_map: raceVarMap
                      )
                    ]

                    [tab, names, _, _] = warnVarArgs

                    case isPublic or
                           compare_var_list(
                             tab,
                             publicTables,
                             raceVarMap
                           ) or length(names -- namedTables) < length(names) do
                      true ->
                        {newWarnCall, newDepList}

                      false ->
                        {newWarnCall, []}
                    end

                  :mnesia_dirty_read_write ->
                    {[
                       r_warn_call(
                         call_name: :mnesia_dirty_write,
                         args: warnVarArgs,
                         var_map: raceVarMap
                       )
                     ], newDepList}
                end

              {newHead ++ raceList, newDepList1, nestingLevel,
               is_last_race(raceTag, initFun, tail, callgraph)}
          end

        {newCurrFun, newCurrFunLabel, newCode, newRaceList, newRaceVarMap, newFunDefVars,
         newFunCallVars, newFunArgTypes, newNestingLevel,
         pullOut} =
          case head do
            r_fun_call(caller: ^currFun) ->
              case newNL === 0 do
                true ->
                  {currFun, currFunLabel, tail, newRL, raceVarMap, funDefVars, funCallVars,
                   funArgTypes, newNL, false}

                false ->
                  {currFun, currFunLabel, code, newRL, raceVarMap, funDefVars, funCallVars,
                   funArgTypes, newNL, true}
              end

            r_beg_clause(arg: arg, pats: pats, guard: guard) ->
              {raceVarMap1, removeClause} =
                race_var_map_guard(
                  arg,
                  pats,
                  guard,
                  raceVarMap,
                  :bind
                )

              case removeClause do
                true ->
                  {raceList2,
                   r_curr_fun(
                     mfa: currFun2,
                     label: currFunLabel2,
                     var_map: raceVarMap2,
                     def_vars: funDefVars2,
                     call_vars: funCallVars2,
                     arg_types: funArgTypes2
                   ), code2,
                   nestingLevel2} =
                    remove_clause(
                      newRL,
                      r_curr_fun(
                        mfa: currFun,
                        label: currFunLabel,
                        var_map: raceVarMap1,
                        def_vars: funDefVars,
                        call_vars: funCallVars,
                        arg_types: funArgTypes
                      ),
                      tail,
                      newNL
                    )

                  {currFun2, currFunLabel2, code2, raceList2, raceVarMap2, funDefVars2,
                   funCallVars2, funArgTypes2, nestingLevel2, false}

                false ->
                  {currFun, currFunLabel, tail, newRL, raceVarMap1, funDefVars, funCallVars,
                   funArgTypes, newNL, false}
              end

            r_end_clause(arg: arg, pats: pats, guard: guard) ->
              {raceVarMap1, _RemoveClause} =
                race_var_map_guard(arg, pats, guard, raceVarMap, :unbind)

              {currFun, currFunLabel, tail, newRL, raceVarMap1, funDefVars, funCallVars,
               funArgTypes, newNL, false}

            r_end_case(clauses: clauses) ->
              raceVarMap1 =
                race_var_map_clauses(
                  clauses,
                  raceVarMap
                )

              {currFun, currFunLabel, tail, newRL, raceVarMap1, funDefVars, funCallVars,
               funArgTypes, newNL, false}

            r_let_tag(var: var, arg: arg) ->
              {currFun, currFunLabel, tail, newRL, race_var_map(var, arg, raceVarMap, :bind),
               funDefVars, funCallVars, funArgTypes, newNL, false}

            r_curr_fun(
              mfa: currFun1,
              label: currFunLabel1,
              var_map: raceVarMap1,
              def_vars: funDefVars1,
              call_vars: funCallVars1,
              arg_types: funArgTypes1
            ) ->
              case newNL === 0 do
                true ->
                  {currFun, currFunLabel, remove_nonlocal_functions(tail, 1), newRL, raceVarMap,
                   funDefVars, funCallVars, funArgTypes, newNL, false}

                false ->
                  {currFun1, currFunLabel1, tail, newRL, raceVarMap1, funDefVars1, funCallVars1,
                   funArgTypes1, newNL, false}
              end

            _Thing ->
              {currFun, currFunLabel, tail, newRL, raceVarMap, funDefVars, funCallVars,
               funArgTypes, newNL, false}
          end

        case return do
          true ->
            {depList, newCurrFun, newCurrFunLabel, calls, [], newRaceList, newRaceVarMap,
             newFunDefVars, newFunCallVars, newFunArgTypes, newNestingLevel}

          false ->
            newNestingLevel1 =
              case newNestingLevel === 0 do
                true ->
                  newNestingLevel + 1

                false ->
                  newNestingLevel
              end

            case pullOut do
              true ->
                {depList, newCurrFun, newCurrFunLabel, calls, newCode, newRaceList, newRaceVarMap,
                 newFunDefVars, newFunCallVars, newFunArgTypes, newNestingLevel1}

              false ->
                {retDepList, newCurrFun1, newCurrFunLabel1, newCalls1, newCode1, newRaceList1,
                 newRaceVarMap1, newFunDefVars1, newFunCallVars1, newFunArgTypes1,
                 newNestingLevel2} =
                  fixup_race_forward(
                    newCurrFun,
                    newCurrFunLabel,
                    calls,
                    newCode,
                    newRaceList,
                    initFun,
                    warnVarArgs,
                    raceWarnTag,
                    newRaceVarMap,
                    newFunDefVars,
                    newFunCallVars,
                    newFunArgTypes,
                    newNestingLevel1,
                    state
                  )

                {depList ++ retDepList, newCurrFun1, newCurrFunLabel1, newCalls1, newCode1,
                 newRaceList1, newRaceVarMap1, newFunDefVars1, newFunCallVars1, newFunArgTypes1,
                 newNestingLevel2}
            end
        end
    end
  end

  defp get_deplist_paths(
         raceList,
         warnVarArgs,
         raceWarnTag,
         raceVarMap,
         currLevel,
         publicTables,
         namedTables
       ) do
    case raceList do
      [] ->
        {[], false, true}

      [head | tail] ->
        case head do
          r_end_case() ->
            {raceList1, depList1, isPublic1, continue1} =
              handle_case(
                tail,
                warnVarArgs,
                raceWarnTag,
                raceVarMap,
                currLevel,
                publicTables,
                namedTables
              )

            case continue1 do
              true ->
                {depList2, isPublic2, continue2} =
                  get_deplist_paths(
                    raceList1,
                    warnVarArgs,
                    raceWarnTag,
                    raceVarMap,
                    currLevel,
                    publicTables,
                    namedTables
                  )

                {depList1 ++ depList2, isPublic1 or isPublic2, continue2}

              false ->
                {depList1, isPublic1, false}
            end

          r_beg_clause() ->
            get_deplist_paths(
              fixup_before_case_path(tail),
              warnVarArgs,
              raceWarnTag,
              raceVarMap,
              currLevel,
              publicTables,
              namedTables
            )

          r_curr_fun(status: :in, var_map: raceVarMap1) ->
            {depList, isPublic, continue} =
              get_deplist_paths(
                tail,
                warnVarArgs,
                raceWarnTag,
                raceVarMap,
                currLevel + 1,
                publicTables,
                namedTables
              )

            isPublic1 =
              case raceWarnTag do
                :warn_ets_lookup_insert ->
                  [tabs, names, _, _] = warnVarArgs

                  isPublic or
                    :lists.any(
                      fn t ->
                        compare_var_list(
                          t,
                          publicTables,
                          raceVarMap1
                        )
                      end,
                      tabs
                    ) or length(names -- namedTables) < length(names)

                _ ->
                  true
              end

            {depList, isPublic1, continue}

          r_curr_fun(
            status: :out,
            var_map: raceVarMap1,
            def_vars: funDefVars,
            call_vars: funCallVars
          ) ->
            warnVarArgs1 =
              var_analysis(
                for defVar <- funDefVars do
                  format_arg(defVar)
                end,
                for callVar <- funCallVars do
                  format_arg(callVar)
                end,
                warnVarArgs,
                raceWarnTag
              )

            {warnVarArgs2, stop} =
              case raceWarnTag do
                :warn_whereis_register ->
                  [wVA1, wVA2, wVA3, wVA4] = warnVarArgs1

                  vars =
                    :lists.flatten(
                      for v <- wVA1 do
                        find_all_bound_vars(
                          v,
                          raceVarMap1
                        )
                      end
                    )

                  case {vars, currLevel} do
                    {[], 0} ->
                      {warnVarArgs, true}

                    {[], _} ->
                      {warnVarArgs, false}

                    _ ->
                      {[vars, wVA2, wVA3, wVA4], false}
                  end

                :warn_whereis_unregister ->
                  [wVA1, wVA2] = warnVarArgs1

                  vars =
                    :lists.flatten(
                      for v <- wVA1 do
                        find_all_bound_vars(
                          v,
                          raceVarMap1
                        )
                      end
                    )

                  case {vars, currLevel} do
                    {[], 0} ->
                      {warnVarArgs, true}

                    {[], _} ->
                      {warnVarArgs, false}

                    _ ->
                      {[vars, wVA2], false}
                  end

                :warn_ets_lookup_insert ->
                  [wVA1, wVA2, wVA3, wVA4] = warnVarArgs1

                  vars1 =
                    :lists.flatten(
                      for v1 <- wVA1 do
                        find_all_bound_vars(
                          v1,
                          raceVarMap1
                        )
                      end
                    )

                  vars2 =
                    :lists.flatten(
                      for v2 <- wVA3 do
                        find_all_bound_vars(
                          v2,
                          raceVarMap1
                        )
                      end
                    )

                  case {vars1, vars2, currLevel} do
                    {[], _, 0} ->
                      {warnVarArgs, true}

                    {[], _, _} ->
                      {warnVarArgs, false}

                    {_, [], 0} ->
                      {warnVarArgs, true}

                    {_, [], _} ->
                      {warnVarArgs, false}

                    _ ->
                      {[vars1, wVA2, vars2, wVA4], false}
                  end

                :warn_mnesia_dirty_read_write ->
                  [wVA1, wVA2 | t] = warnVarArgs1

                  vars =
                    :lists.flatten(
                      for v <- wVA1 do
                        find_all_bound_vars(
                          v,
                          raceVarMap1
                        )
                      end
                    )

                  case {vars, currLevel} do
                    {[], 0} ->
                      {warnVarArgs, true}

                    {[], _} ->
                      {warnVarArgs, false}

                    _ ->
                      {[vars, wVA2 | t], false}
                  end
              end

            case stop do
              true ->
                {[], false, false}

              false ->
                currLevel1 =
                  case currLevel do
                    0 ->
                      currLevel

                    _ ->
                      currLevel - 1
                  end

                get_deplist_paths(
                  tail,
                  warnVarArgs2,
                  raceWarnTag,
                  raceVarMap1,
                  currLevel1,
                  publicTables,
                  namedTables
                )
            end

          r_warn_call(call_name: regCall, args: warnVarArgs1, var_map: raceVarMap1)
          when regCall === :register or regCall === :unregister ->
            case compare_first_arg(warnVarArgs, warnVarArgs1, raceVarMap1) do
              true ->
                {[], false, false}

              newWarnVarArgs ->
                get_deplist_paths(
                  tail,
                  newWarnVarArgs,
                  raceWarnTag,
                  raceVarMap,
                  currLevel,
                  publicTables,
                  namedTables
                )
            end

          r_warn_call(call_name: :ets_insert, args: warnVarArgs1, var_map: raceVarMap1) ->
            case compare_ets_insert(warnVarArgs, warnVarArgs1, raceVarMap1) do
              true ->
                {[], false, false}

              newWarnVarArgs ->
                get_deplist_paths(
                  tail,
                  newWarnVarArgs,
                  raceWarnTag,
                  raceVarMap,
                  currLevel,
                  publicTables,
                  namedTables
                )
            end

          r_warn_call(call_name: :mnesia_dirty_write, args: warnVarArgs1, var_map: raceVarMap1) ->
            case compare_first_arg(warnVarArgs, warnVarArgs1, raceVarMap1) do
              true ->
                {[], false, false}

              newWarnVarArgs ->
                get_deplist_paths(
                  tail,
                  newWarnVarArgs,
                  raceWarnTag,
                  raceVarMap,
                  currLevel,
                  publicTables,
                  namedTables
                )
            end

          r_dep_call(var_map: raceVarMap1) ->
            {depList, isPublic, continue} =
              get_deplist_paths(
                tail,
                warnVarArgs,
                raceWarnTag,
                raceVarMap,
                currLevel,
                publicTables,
                namedTables
              )

            {refine_race(head, warnVarArgs, raceWarnTag, depList, raceVarMap1), isPublic,
             continue}
        end
    end
  end

  defp handle_case(
         raceList,
         warnVarArgs,
         raceWarnTag,
         raceVarMap,
         currLevel,
         publicTables,
         namedTables
       ) do
    case raceList do
      [] ->
        {[], [], false, true}

      [head | tail] ->
        case head do
          r_end_clause() ->
            {restRaceList, depList1, isPublic1, continue1} =
              do_clause(
                tail,
                warnVarArgs,
                raceWarnTag,
                raceVarMap,
                currLevel,
                publicTables,
                namedTables
              )

            {retRaceList, depList2, isPublic2, continue2} =
              handle_case(
                restRaceList,
                warnVarArgs,
                raceWarnTag,
                raceVarMap,
                currLevel,
                publicTables,
                namedTables
              )

            {retRaceList, depList1 ++ depList2, isPublic1 or isPublic2, continue1 or continue2}

          :beg_case ->
            {tail, [], false, false}
        end
    end
  end

  defp do_clause(
         raceList,
         warnVarArgs,
         raceWarnTag,
         raceVarMap,
         currLevel,
         publicTables,
         namedTables
       ) do
    {depList, isPublic, continue} =
      get_deplist_paths(
        fixup_case_path(
          raceList,
          0
        ),
        warnVarArgs,
        raceWarnTag,
        raceVarMap,
        currLevel,
        publicTables,
        namedTables
      )

    {fixup_case_rest_paths(raceList, 0), depList, isPublic, continue}
  end

  defp fixup_case_path(raceList, nestingLevel) do
    case raceList do
      [] ->
        []

      [head | tail] ->
        {newNestingLevel, return} =
          case head do
            :beg_case ->
              {nestingLevel - 1, false}

            r_end_case() ->
              {nestingLevel + 1, false}

            r_beg_clause() ->
              case nestingLevel === 0 do
                true ->
                  {nestingLevel, true}

                false ->
                  {nestingLevel, false}
              end

            _Other ->
              {nestingLevel, false}
          end

        case return do
          true ->
            []

          false ->
            [head | fixup_case_path(tail, newNestingLevel)]
        end
    end
  end

  defp fixup_before_case_path(raceList) do
    case raceList do
      [] ->
        []

      [head | tail] ->
        case head do
          r_end_clause() ->
            fixup_before_case_path(fixup_case_rest_paths(tail, 0))

          :beg_case ->
            tail
        end
    end
  end

  defp fixup_case_rest_paths(raceList, nestingLevel) do
    case raceList do
      [] ->
        []

      [head | tail] ->
        {newNestingLevel, return} =
          case head do
            :beg_case ->
              {nestingLevel - 1, false}

            r_end_case() ->
              {nestingLevel + 1, false}

            r_beg_clause() ->
              case nestingLevel === 0 do
                true ->
                  {nestingLevel, true}

                false ->
                  {nestingLevel, false}
              end

            _Other ->
              {nestingLevel, false}
          end

        case return do
          true ->
            tail

          false ->
            fixup_case_rest_paths(tail, newNestingLevel)
        end
    end
  end

  defp fixup_race_forward_helper(
         currFun,
         currFunLabel,
         fun,
         funLabel,
         calls,
         callsToAnalyze,
         code,
         raceList,
         initFun,
         newFunArgs,
         newFunTypes,
         raceWarnTag,
         raceVarMap,
         funDefVars,
         funCallVars,
         funArgTypes,
         nestingLevel,
         args,
         codeB,
         stateRaceList
       ) do
    case calls do
      [] ->
        {newRaceList,
         r_curr_fun(
           mfa: newCurrFun,
           label: newCurrFunLabel,
           var_map: newRaceVarMap,
           def_vars: newFunDefVars,
           call_vars: newFunCallVars,
           arg_types: newFunArgTypes
         ), newCode,
         newNestingLevel} =
          remove_clause(
            raceList,
            r_curr_fun(
              mfa: currFun,
              label: currFunLabel,
              var_map: raceVarMap,
              def_vars: funDefVars,
              call_vars: funCallVars,
              arg_types: funArgTypes
            ),
            code,
            nestingLevel
          )

        {newCurrFun, newCurrFunLabel, callsToAnalyze, newCode, newRaceList, newRaceVarMap,
         newFunDefVars, newFunCallVars, newFunArgTypes, newNestingLevel}

      [head | tail] ->
        case head do
          {^initFun, ^initFun}
          when currFun === initFun and
                 fun === initFun ->
            newCallsToAnalyze = :lists.delete(head, callsToAnalyze)
            newRaceVarMap = race_var_map(args, newFunArgs, raceVarMap, :bind)

            retC =
              fixup_all_calls(
                initFun,
                initFun,
                funLabel,
                args,
                codeB ++
                  [
                    r_curr_fun(
                      status: :out,
                      mfa: initFun,
                      label: currFunLabel,
                      var_map: raceVarMap,
                      def_vars: funDefVars,
                      call_vars: funCallVars,
                      arg_types: funArgTypes
                    )
                  ],
                code,
                raceVarMap
              )

            newCode =
              fixup_all_calls(
                initFun,
                initFun,
                funLabel,
                args,
                codeB ++
                  [
                    r_curr_fun(
                      status: :out,
                      mfa: initFun,
                      label: currFunLabel,
                      var_map: newRaceVarMap,
                      def_vars: args,
                      call_vars: newFunArgs,
                      arg_types: newFunTypes
                    )
                  ],
                [
                  r_curr_fun(
                    status: :in,
                    mfa: fun,
                    label: funLabel,
                    var_map: newRaceVarMap,
                    def_vars: args,
                    call_vars: newFunArgs,
                    arg_types: newFunTypes
                  )
                  | :lists.reverse(stateRaceList)
                ] ++ retC,
                newRaceVarMap
              )

            {initFun, funLabel, newCallsToAnalyze, newCode, raceList, newRaceVarMap, args,
             newFunArgs, newFunTypes, nestingLevel}

          {^currFun, ^fun} ->
            newCallsToAnalyze = :lists.delete(head, callsToAnalyze)
            newRaceVarMap = race_var_map(args, newFunArgs, raceVarMap, :bind)

            retC =
              case fun do
                ^initFun ->
                  fixup_all_calls(
                    currFun,
                    fun,
                    funLabel,
                    args,
                    :lists.reverse(stateRaceList) ++
                      [
                        r_curr_fun(
                          status: :out,
                          mfa: currFun,
                          label: currFunLabel,
                          var_map: raceVarMap,
                          def_vars: funDefVars,
                          call_vars: funCallVars,
                          arg_types: funArgTypes
                        )
                      ],
                    code,
                    raceVarMap
                  )

                _Other1 ->
                  fixup_all_calls(
                    currFun,
                    fun,
                    funLabel,
                    args,
                    codeB ++
                      [
                        r_curr_fun(
                          status: :out,
                          mfa: currFun,
                          label: currFunLabel,
                          var_map: raceVarMap,
                          def_vars: funDefVars,
                          call_vars: funCallVars,
                          arg_types: funArgTypes
                        )
                      ],
                    code,
                    raceVarMap
                  )
              end

            newCode =
              case fun do
                ^initFun ->
                  [
                    r_curr_fun(
                      status: :in,
                      mfa: fun,
                      label: funLabel,
                      var_map: newRaceVarMap,
                      def_vars: args,
                      call_vars: newFunArgs,
                      arg_types: newFunTypes
                    )
                    | :lists.reverse(stateRaceList)
                  ] ++ retC

                _ ->
                  [
                    r_curr_fun(
                      status: :in,
                      mfa: fun,
                      label: funLabel,
                      var_map: newRaceVarMap,
                      def_vars: args,
                      call_vars: newFunArgs,
                      arg_types: newFunTypes
                    )
                    | codeB
                  ] ++ retC
              end

            {fun, funLabel, newCallsToAnalyze, newCode, raceList, newRaceVarMap, args, newFunArgs,
             newFunTypes, nestingLevel}

          {_TupleA, _TupleB} ->
            fixup_race_forward_helper(
              currFun,
              currFunLabel,
              fun,
              funLabel,
              tail,
              callsToAnalyze,
              code,
              raceList,
              initFun,
              newFunArgs,
              newFunTypes,
              raceWarnTag,
              raceVarMap,
              funDefVars,
              funCallVars,
              funArgTypes,
              nestingLevel,
              args,
              codeB,
              stateRaceList
            )
        end
    end
  end

  defp fixup_race_backward(currFun, calls, callsToAnalyze, parents, height) do
    case height === 0 do
      true ->
        parents

      false ->
        case calls do
          [] ->
            case is_integer(currFun) or
                   :lists.member(
                     currFun,
                     parents
                   ) do
              true ->
                parents

              false ->
                [currFun | parents]
            end

          [head | tail] ->
            {parent, tupleB} = head

            case tupleB === currFun do
              true ->
                newCallsToAnalyze = :lists.delete(head, callsToAnalyze)

                newParents =
                  fixup_race_backward(
                    parent,
                    newCallsToAnalyze,
                    newCallsToAnalyze,
                    parents,
                    height - 1
                  )

                fixup_race_backward(currFun, tail, newCallsToAnalyze, newParents, height)

              false ->
                fixup_race_backward(currFun, tail, callsToAnalyze, parents, height)
            end
        end
    end
  end

  defp are_bound_labels(label1, label2, raceVarMap) do
    case :dict.find(label1, raceVarMap) do
      :error ->
        false

      {:ok, labels} ->
        :lists.member(
          label2,
          labels
        ) or are_bound_labels_helper(labels, label1, label2, raceVarMap)
    end
  end

  defp are_bound_labels_helper(labels, oldLabel, compLabel, raceVarMap) do
    case :dict.size(raceVarMap) do
      0 ->
        false

      _ ->
        case labels do
          [] ->
            false

          [head | tail] ->
            newRaceVarMap = :dict.erase(oldLabel, raceVarMap)

            are_bound_labels(head, compLabel, newRaceVarMap) or
              are_bound_labels_helper(
                tail,
                head,
                compLabel,
                newRaceVarMap
              )
        end
    end
  end

  defp are_bound_vars(vars1, vars2, raceVarMap) do
    case is_list(vars1) and is_list(vars2) do
      true ->
        case vars1 do
          [] ->
            false

          [aHead | aTail] ->
            case vars2 do
              [] ->
                false

              [pHead | pTail] ->
                are_bound_vars(aHead, pHead, raceVarMap) and
                  are_bound_vars(aTail, pTail, raceVarMap)
            end
        end

      false ->
        {newVars1, newVars2, isList} =
          case is_list(vars1) do
            true ->
              case vars1 do
                [var1] ->
                  {var1, vars2, true}

                _Thing ->
                  {vars1, vars2, false}
              end

            false ->
              case is_list(vars2) do
                true ->
                  case vars2 do
                    [var2] ->
                      {vars1, var2, true}

                    _Thing ->
                      {vars1, vars2, false}
                  end

                false ->
                  {vars1, vars2, true}
              end
          end

        case isList do
          true ->
            case :cerl.type(newVars1) do
              :var ->
                case :cerl.type(newVars2) do
                  :var ->
                    aLabel = :cerl_trees.get_label(newVars1)
                    pLabel = :cerl_trees.get_label(newVars2)

                    are_bound_labels(aLabel, pLabel, raceVarMap) or
                      are_bound_labels(
                        pLabel,
                        aLabel,
                        raceVarMap
                      )

                  :alias ->
                    are_bound_vars(newVars1, :cerl.alias_var(newVars2), raceVarMap)

                  :values ->
                    are_bound_vars(newVars1, :cerl.values_es(newVars2), raceVarMap)

                  _Other ->
                    false
                end

              :tuple ->
                case :cerl.type(newVars2) do
                  :tuple ->
                    are_bound_vars(:cerl.tuple_es(newVars1), :cerl.tuple_es(newVars2), raceVarMap)

                  :alias ->
                    are_bound_vars(newVars1, :cerl.alias_var(newVars2), raceVarMap)

                  :values ->
                    are_bound_vars(newVars1, :cerl.values_es(newVars2), raceVarMap)

                  _Other ->
                    false
                end

              :cons ->
                case :cerl.type(newVars2) do
                  :cons ->
                    are_bound_vars(
                      :cerl.cons_hd(newVars1),
                      :cerl.cons_hd(newVars2),
                      raceVarMap
                    ) and
                      are_bound_vars(
                        :cerl.cons_tl(newVars1),
                        :cerl.cons_tl(newVars2),
                        raceVarMap
                      )

                  :alias ->
                    are_bound_vars(newVars1, :cerl.alias_var(newVars2), raceVarMap)

                  :values ->
                    are_bound_vars(newVars1, :cerl.values_es(newVars2), raceVarMap)

                  _Other ->
                    false
                end

              :alias ->
                case :cerl.type(newVars2) do
                  :alias ->
                    are_bound_vars(
                      :cerl.alias_var(newVars1),
                      :cerl.alias_var(newVars2),
                      raceVarMap
                    )

                  _Other ->
                    are_bound_vars(:cerl.alias_var(newVars1), newVars2, raceVarMap)
                end

              :values ->
                case :cerl.type(newVars2) do
                  :values ->
                    are_bound_vars(
                      :cerl.values_es(newVars1),
                      :cerl.values_es(newVars2),
                      raceVarMap
                    )

                  _Other ->
                    are_bound_vars(:cerl.values_es(newVars1), newVars2, raceVarMap)
                end

              _Other ->
                false
            end

          false ->
            false
        end
    end
  end

  defp callgraph__renew_tables(table, callgraph) do
    case table do
      {:named, nameLabel, names} ->
        pTablesToAdd =
          case nameLabel do
            :no_label ->
              []

            _Other ->
              [nameLabel]
          end

        namesToAdd = filter_named_tables(names)
        pTables = :dialyzer_callgraph.get_public_tables(callgraph)
        nTables = :dialyzer_callgraph.get_named_tables(callgraph)

        :dialyzer_callgraph.put_public_tables(
          :lists.usort(pTablesToAdd ++ pTables),
          :dialyzer_callgraph.put_named_tables(
            namesToAdd ++ nTables,
            callgraph
          )
        )

      _Other ->
        callgraph
    end
  end

  defp cleanup_clause_code(
         r_curr_fun(mfa: currFun) = currTuple,
         code,
         nestingLevel,
         localNestingLevel
       ) do
    case code do
      [] ->
        {currTuple, []}

      [head | tail] ->
        {newLocalNestingLevel, newNestingLevel, newCurrTuple, return} =
          case head do
            :beg_case ->
              {localNestingLevel, nestingLevel + 1, currTuple, false}

            r_end_case() ->
              {localNestingLevel, nestingLevel - 1, currTuple, false}

            r_end_clause() ->
              case nestingLevel === 0 do
                true ->
                  {localNestingLevel, nestingLevel, currTuple, true}

                false ->
                  {localNestingLevel, nestingLevel, currTuple, false}
              end

            r_fun_call(caller: ^currFun) ->
              {localNestingLevel - 1, nestingLevel, currTuple, false}

            r_curr_fun(status: :in) ->
              {localNestingLevel - 1, nestingLevel, head, false}

            r_curr_fun(status: :out) ->
              {localNestingLevel + 1, nestingLevel, head, false}

            other when other !== r_fun_call() ->
              {localNestingLevel, nestingLevel, currTuple, false}
          end

        case return do
          true ->
            {newCurrTuple, tail}

          false ->
            cleanup_clause_code(newCurrTuple, tail, newNestingLevel, newLocalNestingLevel)
        end
    end
  end

  defp cleanup_dep_calls(depList) do
    case depList do
      [] ->
        []

      [
        r_dep_call(
          call_name: callName,
          arg_types: argTypes,
          vars: vars,
          state: state,
          file_line: fileLine
        )
        | t
      ] ->
        [
          r_dep_call(
            call_name: callName,
            arg_types: argTypes,
            vars: vars,
            state: state,
            file_line: fileLine
          )
          | cleanup_dep_calls(t)
        ]
    end
  end

  defp cleanup_race_code(state) do
    callgraph = :dialyzer_dataflow.state__get_callgraph(state)

    :dialyzer_dataflow.state__put_callgraph(
      :dialyzer_callgraph.race_code_new(callgraph),
      state
    )
  end

  defp filter_named_tables(namesList) do
    case namesList do
      [] ->
        []

      [head | tail] ->
        newHead =
          case :string.find(head, '()', :trailing) do
            :nomatch ->
              [head]

            _Other ->
              []
          end

        newHead ++ filter_named_tables(tail)
    end
  end

  defp filter_parents(parents, newParents, digraph) do
    case parents do
      [] ->
        newParents

      [head | tail] ->
        newParents1 = filter_parents_helper1(head, tail, newParents, digraph)
        filter_parents(tail, newParents1, digraph)
    end
  end

  defp filter_parents_helper1(first, rest, newParents, digraph) do
    case rest do
      [] ->
        newParents

      [head | tail] ->
        newParents1 = filter_parents_helper2(first, head, newParents, digraph)
        filter_parents_helper1(first, tail, newParents1, digraph)
    end
  end

  defp filter_parents_helper2(parent1, parent2, newParents, digraph) do
    case :digraph.get_path(digraph, parent1, parent2) do
      false ->
        case :digraph.get_path(digraph, parent2, parent1) do
          false ->
            newParents

          _Vertices ->
            newParents -- [parent1]
        end

      _Vertices ->
        newParents -- [parent2]
    end
  end

  defp find_all_bound_vars(label, raceVarMap) do
    case :dict.find(label, raceVarMap) do
      :error ->
        [label]

      {:ok, labels} ->
        :lists.usort(labels ++ find_all_bound_vars_helper(labels, label, raceVarMap))
    end
  end

  defp find_all_bound_vars_helper(labels, label, raceVarMap) do
    case :dict.size(raceVarMap) do
      0 ->
        []

      _ ->
        case labels do
          [] ->
            []

          [head | tail] ->
            newRaceVarMap = :dict.erase(label, raceVarMap)

            find_all_bound_vars(
              head,
              newRaceVarMap
            ) ++
              find_all_bound_vars_helper(
                tail,
                head,
                newRaceVarMap
              )
        end
    end
  end

  defp fixup_all_calls(currFun, nextFun, nextFunLabel, args, codeToReplace, code, raceVarMap) do
    case code do
      [] ->
        []

      [head | tail] ->
        newCode =
          case head do
            r_fun_call(caller: ^currFun, callee: callee, arg_types: funArgTypes, vars: funArgs)
            when callee === nextFun or callee === nextFunLabel ->
              raceVarMap1 = race_var_map(args, funArgs, raceVarMap, :bind)

              [
                r_curr_fun(
                  status: :in,
                  mfa: nextFun,
                  label: nextFunLabel,
                  var_map: raceVarMap1,
                  def_vars: args,
                  call_vars: funArgs,
                  arg_types: funArgTypes
                )
                | codeToReplace
              ]

            _Other ->
              [head]
          end

        retCode =
          fixup_all_calls(currFun, nextFun, nextFunLabel, args, codeToReplace, tail, raceVarMap)

        newCode ++ retCode
    end
  end

  defp is_last_race(raceTag, initFun, code, callgraph) do
    case code do
      [] ->
        true

      [head | tail] ->
        case head do
          ^raceTag ->
            false

          r_fun_call(callee: fun) ->
            funName =
              case is_integer(fun) do
                true ->
                  case :dialyzer_callgraph.lookup_name(
                         fun,
                         callgraph
                       ) do
                    :error ->
                      fun

                    {:ok, name} ->
                      name
                  end

                false ->
                  fun
              end

            digraph = :dialyzer_callgraph.get_digraph(callgraph)

            case funName === initFun or
                   :digraph.get_path(
                     digraph,
                     funName,
                     initFun
                   ) do
              false ->
                is_last_race(raceTag, initFun, tail, callgraph)

              _Vertices ->
                false
            end

          _Other ->
            is_last_race(raceTag, initFun, tail, callgraph)
        end
    end
  end

  defp lists_key_member(member, list, n) when is_integer(member) do
    case list do
      [] ->
        0

      [head | tail] ->
        newN = n + 1

        case head do
          ^member ->
            newN

          _Other ->
            lists_key_member(member, tail, newN)
        end
    end
  end

  defp lists_key_member(_M, _L, _N) do
    0
  end

  defp lists_key_member_lists(memberList, list) do
    case memberList do
      [] ->
        0

      [head | tail] ->
        case lists_key_member(head, list, 0) do
          0 ->
            lists_key_member_lists(tail, list)

          other ->
            other
        end
    end
  end

  defp lists_key_members_lists(memberList, list) do
    case memberList do
      [] ->
        []

      [head | tail] ->
        :lists.usort(
          lists_key_members_lists_helper(head, list, 1) ++
            lists_key_members_lists(
              tail,
              list
            )
        )
    end
  end

  defp lists_key_members_lists_helper(elem, list, n) when is_integer(elem) do
    case list do
      [] ->
        []

      [head | tail] ->
        newHead =
          case head === elem do
            true ->
              [n]

            false ->
              []
          end

        newHead ++ lists_key_members_lists_helper(elem, tail, n + 1)
    end
  end

  defp lists_key_members_lists_helper(_Elem, _List, _N) do
    [0]
  end

  defp lists_key_replace(n, list, newMember) do
    {before, [_ | after__]} = :lists.split(n - 1, list)
    before ++ [newMember | after__]
  end

  defp lists_get(0, _List) do
    :no_label
  end

  defp lists_get(n, list) do
    :lists.nth(n, list)
  end

  defp refine_race(raceCall, warnVarArgs, raceWarnTag, dependencyList, raceVarMap) do
    case raceWarnTag do
      warnWhereis
      when warnWhereis === :warn_whereis_register or warnWhereis === :warn_whereis_unregister ->
        case raceCall do
          r_dep_call(call_name: :ets_lookup) ->
            dependencyList

          r_dep_call(call_name: :mnesia_dirty_read) ->
            dependencyList

          r_dep_call(call_name: :whereis, args: varArgs) ->
            refine_race_helper(
              raceCall,
              varArgs,
              warnVarArgs,
              raceWarnTag,
              dependencyList,
              raceVarMap
            )
        end

      :warn_ets_lookup_insert ->
        case raceCall do
          r_dep_call(call_name: :whereis) ->
            dependencyList

          r_dep_call(call_name: :mnesia_dirty_read) ->
            dependencyList

          r_dep_call(call_name: :ets_lookup, args: varArgs) ->
            refine_race_helper(
              raceCall,
              varArgs,
              warnVarArgs,
              raceWarnTag,
              dependencyList,
              raceVarMap
            )
        end

      :warn_mnesia_dirty_read_write ->
        case raceCall do
          r_dep_call(call_name: :whereis) ->
            dependencyList

          r_dep_call(call_name: :ets_lookup) ->
            dependencyList

          r_dep_call(call_name: :mnesia_dirty_read, args: varArgs) ->
            refine_race_helper(
              raceCall,
              varArgs,
              warnVarArgs,
              raceWarnTag,
              dependencyList,
              raceVarMap
            )
        end
    end
  end

  defp refine_race_helper(raceCall, varArgs, warnVarArgs, raceWarnTag, dependencyList, raceVarMap) do
    case compare_types(varArgs, warnVarArgs, raceWarnTag, raceVarMap) do
      true ->
        [raceCall | dependencyList]

      false ->
        dependencyList
    end
  end

  defp remove_clause(raceList, currTuple, code, nestingLevel) do
    newRaceList = fixup_case_rest_paths(raceList, 0)
    {newCurrTuple, newCode} = cleanup_clause_code(currTuple, code, 0, nestingLevel)
    returnTuple = {newRaceList, newCurrTuple, newCode, nestingLevel}

    case newRaceList do
      [:beg_case | rTail] ->
        case newCode do
          [r_end_case() | cTail] ->
            remove_clause(rTail, newCurrTuple, cTail, nestingLevel)

          _Other ->
            returnTuple
        end

      _Else ->
        returnTuple
    end
  end

  defp remove_nonlocal_functions(code, nestingLevel) do
    case code do
      [] ->
        []

      [h | t] ->
        newNL =
          case h do
            r_curr_fun(status: :in) ->
              nestingLevel + 1

            r_curr_fun(status: :out) ->
              nestingLevel - 1

            _Other ->
              nestingLevel
          end

        case newNL === 0 do
          true ->
            t

          false ->
            remove_nonlocal_functions(t, newNL)
        end
    end
  end

  defp renew_curr_fun(currFun, races) do
    r_races(races, curr_fun: currFun)
  end

  defp renew_curr_fun_label(currFunLabel, races) do
    r_races(races, curr_fun_label: currFunLabel)
  end

  defp renew_race_list(raceList, races) do
    r_races(races, race_list: raceList)
  end

  defp renew_race_list_size(raceListSize, races) do
    r_races(races, race_list_size: raceListSize)
  end

  defp renew_race_tags(raceTags, races) do
    r_races(races, race_tags: raceTags)
  end

  defp renew_table(table, races) do
    r_races(races, new_table: table)
  end

  defp state__renew_curr_fun(currFun, state) do
    races = :dialyzer_dataflow.state__get_races(state)

    :dialyzer_dataflow.state__put_races(
      renew_curr_fun(
        currFun,
        races
      ),
      state
    )
  end

  defp state__renew_curr_fun_label(currFunLabel, state) do
    races = :dialyzer_dataflow.state__get_races(state)

    :dialyzer_dataflow.state__put_races(
      renew_curr_fun_label(
        currFunLabel,
        races
      ),
      state
    )
  end

  defp state__renew_race_list(raceList, state) do
    races = :dialyzer_dataflow.state__get_races(state)

    :dialyzer_dataflow.state__put_races(
      renew_race_list(
        raceList,
        races
      ),
      state
    )
  end

  defp state__renew_race_tags(raceTags, state) do
    races = :dialyzer_dataflow.state__get_races(state)

    :dialyzer_dataflow.state__put_races(
      renew_race_tags(
        raceTags,
        races
      ),
      state
    )
  end

  defp state__renew_info(raceList, raceListSize, raceTags, table, state) do
    callgraph = :dialyzer_dataflow.state__get_callgraph(state)
    races = :dialyzer_dataflow.state__get_races(state)

    :dialyzer_dataflow.state__put_callgraph(
      callgraph__renew_tables(
        table,
        callgraph
      ),
      :dialyzer_dataflow.state__put_races(
        renew_table(
          table,
          renew_race_list(
            raceList,
            renew_race_list_size(
              raceListSize,
              renew_race_tags(
                raceTags,
                races
              )
            )
          )
        ),
        state
      )
    )
  end

  defp any_args(strList) do
    case strList do
      [] ->
        false

      [head | tail] ->
        case :string.find(head, '()', :trailing) do
          :nomatch ->
            any_args(tail)

          _Other ->
            true
        end
    end
  end

  defp bind_dict_vars(key, label, raceVarMap) do
    case key === label do
      true ->
        raceVarMap

      false ->
        case :dict.find(key, raceVarMap) do
          :error ->
            :dict.store(key, [label], raceVarMap)

          {:ok, labels} ->
            case :lists.member(label, labels) do
              true ->
                raceVarMap

              false ->
                :dict.store(key, [label | labels], raceVarMap)
            end
        end
    end
  end

  defp bind_dict_vars_list(key, labels, raceVarMap) do
    case labels do
      [] ->
        raceVarMap

      [head | tail] ->
        bind_dict_vars_list(key, tail, bind_dict_vars(key, head, raceVarMap))
    end
  end

  defp compare_ets_insert(oldWarnVarArgs, newWarnVarArgs, raceVarMap) do
    [old1, old2, old3, old4] = oldWarnVarArgs
    [new1, new2, new3, new4] = newWarnVarArgs

    bool =
      case any_args(old2) do
        true ->
          compare_var_list(new1, old1, raceVarMap)

        false ->
          case any_args(new2) do
            true ->
              compare_var_list(new1, old1, raceVarMap)

            false ->
              compare_var_list(new1, old1, raceVarMap) or old2 === new2
          end
      end

    case bool do
      true ->
        case any_args(old4) do
          true ->
            case compare_list_vars(old3, ets_list_args(new3), [], raceVarMap) do
              true ->
                true

              args3 ->
                lists_key_replace(3, oldWarnVarArgs, args3)
            end

          false ->
            case any_args(new4) do
              true ->
                case compare_list_vars(old3, ets_list_args(new3), [], raceVarMap) do
                  true ->
                    true

                  args3 ->
                    lists_key_replace(3, oldWarnVarArgs, args3)
                end

              false ->
                case compare_list_vars(old3, ets_list_args(new3), [], raceVarMap) do
                  true ->
                    true

                  args3 ->
                    lists_key_replace(
                      4,
                      lists_key_replace(3, oldWarnVarArgs, args3),
                      old4 -- new4
                    )
                end
            end
        end

      false ->
        oldWarnVarArgs
    end
  end

  defp compare_first_arg(oldWarnVarArgs, newWarnVarArgs, raceVarMap) do
    [old1, old2 | _OldT] = oldWarnVarArgs
    [new1, new2 | _NewT] = newWarnVarArgs

    case any_args(old2) do
      true ->
        case compare_var_list(new1, old1, raceVarMap) do
          true ->
            true

          false ->
            oldWarnVarArgs
        end

      false ->
        case any_args(new2) do
          true ->
            case compare_var_list(new1, old1, raceVarMap) do
              true ->
                true

              false ->
                oldWarnVarArgs
            end

          false ->
            case compare_var_list(new1, old1, raceVarMap) do
              true ->
                true

              false ->
                lists_key_replace(2, oldWarnVarArgs, old2 -- new2)
            end
        end
    end
  end

  defp compare_argtypes(argTypes, warnArgTypes) do
    :lists.any(
      fn x ->
        :lists.member(x, warnArgTypes)
      end,
      argTypes
    )
  end

  defp compare_types(varArgs, warnVarArgs, raceWarnTag, raceVarMap) do
    case raceWarnTag do
      :warn_whereis_register ->
        [vA1, vA2] = varArgs
        [wVA1, wVA2, _, _] = warnVarArgs

        case any_args(vA2) do
          true ->
            compare_var_list(vA1, wVA1, raceVarMap)

          false ->
            case any_args(wVA2) do
              true ->
                compare_var_list(vA1, wVA1, raceVarMap)

              false ->
                compare_var_list(vA1, wVA1, raceVarMap) or compare_argtypes(vA2, wVA2)
            end
        end

      :warn_whereis_unregister ->
        [vA1, vA2] = varArgs
        [wVA1, wVA2] = warnVarArgs

        case any_args(vA2) do
          true ->
            compare_var_list(vA1, wVA1, raceVarMap)

          false ->
            case any_args(wVA2) do
              true ->
                compare_var_list(vA1, wVA1, raceVarMap)

              false ->
                compare_var_list(vA1, wVA1, raceVarMap) or compare_argtypes(vA2, wVA2)
            end
        end

      :warn_ets_lookup_insert ->
        [vA1, vA2, vA3, vA4] = varArgs
        [wVA1, wVA2, wVA3, wVA4] = warnVarArgs

        bool =
          case any_args(vA2) do
            true ->
              compare_var_list(vA1, wVA1, raceVarMap)

            false ->
              case any_args(wVA2) do
                true ->
                  compare_var_list(vA1, wVA1, raceVarMap)

                false ->
                  compare_var_list(vA1, wVA1, raceVarMap) or
                    compare_argtypes(
                      vA2,
                      wVA2
                    )
              end
          end

        bool and
          case any_args(vA4) do
            true ->
              compare_var_list(vA3, wVA3, raceVarMap)

            false ->
              case any_args(wVA4) do
                true ->
                  compare_var_list(vA3, wVA3, raceVarMap)

                false ->
                  compare_var_list(vA3, wVA3, raceVarMap) or
                    compare_argtypes(
                      vA4,
                      wVA4
                    )
              end
          end

      :warn_mnesia_dirty_read_write ->
        [vA1, vA2 | _] = varArgs
        [wVA1, wVA2 | _] = warnVarArgs

        case any_args(vA2) do
          true ->
            compare_var_list(vA1, wVA1, raceVarMap)

          false ->
            case any_args(wVA2) do
              true ->
                compare_var_list(vA1, wVA1, raceVarMap)

              false ->
                compare_var_list(vA1, wVA1, raceVarMap) or compare_argtypes(vA2, wVA2)
            end
        end
    end
  end

  defp compare_list_vars(varList1, varList2, newVarList1, raceVarMap) do
    case varList1 do
      [] ->
        case newVarList1 do
          [] ->
            true

          _Other ->
            newVarList1
        end

      [head | tail] ->
        newHead =
          case compare_var_list(head, varList2, raceVarMap) do
            true ->
              []

            false ->
              [head]
          end

        compare_list_vars(tail, varList2, newHead ++ newVarList1, raceVarMap)
    end
  end

  defp compare_vars(var1, var2, raceVarMap)
       when is_integer(var1) and is_integer(var2) do
    var1 === var2 or are_bound_labels(var1, var2, raceVarMap) or
      are_bound_labels(
        var2,
        var1,
        raceVarMap
      )
  end

  defp compare_vars(_Var1, _Var2, _RaceVarMap) do
    false
  end

  defp compare_var_list(var, varList, raceVarMap) do
    :lists.any(
      fn v ->
        compare_vars(var, v, raceVarMap)
      end,
      varList
    )
  end

  defp ets_list_args(maybeList) do
    case is_list(maybeList) do
      true ->
        try do
          for t <- maybeList do
            ets_tuple_args(t)
          end
        catch
          _, _ ->
            [:no_label]
        end

      false ->
        [ets_tuple_args(maybeList)]
    end
  end

  defp ets_list_argtypes(listStr) do
    listStr1 = :string.trim(listStr, :leading, '$[')
    :string.trim(listStr1, :trailing, '$]$.$,')
  end

  defp ets_tuple_args(maybeTuple) do
    case is_tuple(maybeTuple) do
      true ->
        :erlang.element(1, maybeTuple)

      false ->
        :no_label
    end
  end

  defp ets_tuple_argtypes2(tupleList, elemList) do
    case tupleList do
      [] ->
        elemList

      [h | t] ->
        ets_tuple_argtypes2(
          t,
          ets_tuple_argtypes2_helper(h, [], 0) ++ elemList
        )
    end
  end

  defp ets_tuple_argtypes2_helper(tupleStr, elemStr, nestingLevel) do
    case tupleStr do
      [] ->
        []

      [h | t] ->
        {newElemStr, newNestingLevel, return} =
          case h do
            ?{ when nestingLevel === 0 ->
              {elemStr, nestingLevel + 1, false}

            ?{ ->
              {[h | elemStr], nestingLevel + 1, false}

            ?[ ->
              {[h | elemStr], nestingLevel + 1, false}

            ?( ->
              {[h | elemStr], nestingLevel + 1, false}

            ?} ->
              {[h | elemStr], nestingLevel - 1, false}

            ?] ->
              {[h | elemStr], nestingLevel - 1, false}

            ?) ->
              {[h | elemStr], nestingLevel - 1, false}

            ?, when nestingLevel === 1 ->
              {:lists.reverse(elemStr), nestingLevel, true}

            _Other ->
              {[h | elemStr], nestingLevel, false}
          end

        case return do
          true ->
            :string.lexemes(newElemStr, ' |')

          false ->
            ets_tuple_argtypes2_helper(t, newElemStr, newNestingLevel)
        end
    end
  end

  defp ets_tuple_argtypes1(str, tuple, tupleList, nestingLevel) do
    case str do
      [] ->
        tupleList

      [h | t] ->
        {newTuple, newNestingLevel, add} =
          case h do
            ?{ ->
              {[h | tuple], nestingLevel + 1, false}

            ?} ->
              case nestingLevel do
                1 ->
                  {[h | tuple], nestingLevel - 1, true}

                _Else ->
                  {[h | tuple], nestingLevel - 1, false}
              end

            _Other1 when nestingLevel === 0 ->
              {tuple, nestingLevel, false}

            _Other2 ->
              {[h | tuple], nestingLevel, false}
          end

        case add do
          true ->
            ets_tuple_argtypes1(t, [], [:lists.reverse(newTuple) | tupleList], newNestingLevel)

          false ->
            ets_tuple_argtypes1(t, newTuple, tupleList, newNestingLevel)
        end
    end
  end

  defp format_arg(:bypassed) do
    :no_label
  end

  defp format_arg(arg0) do
    arg = :cerl.fold_literal(arg0)

    case :cerl.type(arg) do
      :var ->
        :cerl_trees.get_label(arg)

      :tuple ->
        :erlang.list_to_tuple(
          for a <- :cerl.tuple_es(arg) do
            format_arg(a)
          end
        )

      :cons ->
        [
          format_arg(:cerl.cons_hd(arg))
          | format_arg(:cerl.cons_tl(arg))
        ]

      :alias ->
        format_arg(:cerl.alias_var(arg))

      :literal ->
        case :cerl.is_c_nil(arg) do
          true ->
            []

          false ->
            :no_label
        end

      _Other ->
        :no_label
    end
  end

  def format_args([], [], _State, _Call) do
    []
  end

  def format_args(argList, typeList, cleanState, call) do
    format_args_2(
      format_args_1(argList, typeList, cleanState),
      call
    )
  end

  defp format_args_1([arg], [type], cleanState) do
    [format_arg(arg), format_type(type, cleanState)]
  end

  defp format_args_1([arg | args], [type | types], cleanState) do
    list =
      case arg === :bypassed do
        true ->
          [:no_label, format_type(type, cleanState)]

        false ->
          case :cerl.is_literal(:cerl.fold_literal(arg)) do
            true ->
              [:no_label, format_cerl(arg)]

            false ->
              [format_arg(arg), format_type(type, cleanState)]
          end
      end

    list ++ format_args_1(args, types, cleanState)
  end

  defp format_args_2(strArgList, call) do
    case call do
      :whereis ->
        lists_key_replace(2, strArgList, :string.lexemes(:lists.nth(2, strArgList), ' |'))

      :register ->
        lists_key_replace(2, strArgList, :string.lexemes(:lists.nth(2, strArgList), ' |'))

      :unregister ->
        lists_key_replace(2, strArgList, :string.lexemes(:lists.nth(2, strArgList), ' |'))

      :ets_new ->
        strArgList1 =
          lists_key_replace(
            2,
            strArgList,
            :string.lexemes(
              :lists.nth(
                2,
                strArgList
              ),
              ' |'
            )
          )

        lists_key_replace(
          4,
          strArgList1,
          :string.lexemes(
            ets_list_argtypes(
              :lists.nth(
                4,
                strArgList1
              )
            ),
            ' |'
          )
        )

      :ets_lookup ->
        strArgList1 =
          lists_key_replace(
            2,
            strArgList,
            :string.lexemes(
              :lists.nth(
                2,
                strArgList
              ),
              ' |'
            )
          )

        lists_key_replace(4, strArgList1, :string.lexemes(:lists.nth(4, strArgList1), ' |'))

      :ets_insert ->
        strArgList1 =
          lists_key_replace(
            2,
            strArgList,
            :string.lexemes(
              :lists.nth(
                2,
                strArgList
              ),
              ' |'
            )
          )

        lists_key_replace(
          4,
          strArgList1,
          ets_tuple_argtypes2(
            ets_tuple_argtypes1(
              :lists.nth(
                4,
                strArgList1
              ),
              [],
              [],
              0
            ),
            []
          )
        )

      :mnesia_dirty_read1 ->
        lists_key_replace(
          2,
          strArgList,
          for t <-
                :string.lexemes(
                  :lists.nth(2, strArgList),
                  ' |'
                ) do
            mnesia_tuple_argtypes(t)
          end
        )

      :mnesia_dirty_read2 ->
        lists_key_replace(2, strArgList, :string.lexemes(:lists.nth(2, strArgList), ' |'))

      :mnesia_dirty_write1 ->
        lists_key_replace(
          2,
          strArgList,
          for r <-
                :string.lexemes(
                  :lists.nth(2, strArgList),
                  ' |'
                ) do
            mnesia_record_tab(r)
          end
        )

      :mnesia_dirty_write2 ->
        lists_key_replace(2, strArgList, :string.lexemes(:lists.nth(2, strArgList), ' |'))

      :function_call ->
        strArgList
    end
  end

  defp format_cerl(tree) do
    :cerl_prettypr.format(
      :cerl.set_ann(tree, []),
      [{:hook, :dialyzer_utils.pp_hook()}, {:noann, true}, {:paper, 100_000}, {:ribbon, 100_000}]
    )
  end

  defp format_type(type, state) do
    r = :dialyzer_dataflow.state__get_records(state)
    :erl_types.t_to_string(type, r)
  end

  defp mnesia_record_tab(recordStr) do
    case :erl_scan.string(recordStr) do
      {:ok, [{:"#", _}, {:atom, _, name} | _], _} ->
        :io_lib.write_string(:erlang.atom_to_list(name), ?')

      _ ->
        recordStr
    end
  end

  defp mnesia_tuple_argtypes(tupleStr) do
    tupleStr1 = :string.trim(tupleStr, :leading, '${')
    [tupleStr2 | _T] = :string.lexemes(tupleStr1, ' ,')
    :lists.flatten(:string.lexemes(tupleStr2, ' |'))
  end

  defp race_var_map(vars1, vars2, raceVarMap, op) do
    case vars1 === :no_arg or vars1 === :bypassed or vars2 === :bypassed do
      true ->
        raceVarMap

      false ->
        case is_list(vars1) and is_list(vars2) do
          true ->
            case vars1 do
              [] ->
                raceVarMap

              [aHead | aTail] ->
                case vars2 do
                  [] ->
                    raceVarMap

                  [pHead | pTail] ->
                    newRaceVarMap = race_var_map(aHead, pHead, raceVarMap, op)
                    race_var_map(aTail, pTail, newRaceVarMap, op)
                end
            end

          false ->
            {newVars1, newVars2, bool} =
              case is_list(vars1) do
                true ->
                  case vars1 do
                    [var1] ->
                      {var1, vars2, true}

                    _Thing ->
                      {vars1, vars2, false}
                  end

                false ->
                  case is_list(vars2) do
                    true ->
                      case vars2 do
                        [var2] ->
                          {vars1, var2, true}

                        _Thing ->
                          {vars1, vars2, false}
                      end

                    false ->
                      {vars1, vars2, true}
                  end
              end

            case bool do
              true ->
                case :cerl.type(newVars1) do
                  :var ->
                    case :cerl.type(newVars2) do
                      :var ->
                        aLabel = :cerl_trees.get_label(newVars1)
                        pLabel = :cerl_trees.get_label(newVars2)

                        case op do
                          :bind ->
                            tempRaceVarMap = bind_dict_vars(aLabel, pLabel, raceVarMap)
                            bind_dict_vars(pLabel, aLabel, tempRaceVarMap)

                          :unbind ->
                            tempRaceVarMap = unbind_dict_vars(aLabel, pLabel, raceVarMap)
                            unbind_dict_vars(pLabel, aLabel, tempRaceVarMap)
                        end

                      :alias ->
                        race_var_map(newVars1, :cerl.alias_var(newVars2), raceVarMap, op)

                      :values ->
                        race_var_map(newVars1, :cerl.values_es(newVars2), raceVarMap, op)

                      _Other ->
                        raceVarMap
                    end

                  :tuple ->
                    case :cerl.type(newVars2) do
                      :tuple ->
                        race_var_map(
                          :cerl.tuple_es(newVars1),
                          :cerl.tuple_es(newVars2),
                          raceVarMap,
                          op
                        )

                      :alias ->
                        race_var_map(newVars1, :cerl.alias_var(newVars2), raceVarMap, op)

                      :values ->
                        race_var_map(newVars1, :cerl.values_es(newVars2), raceVarMap, op)

                      _Other ->
                        raceVarMap
                    end

                  :cons ->
                    case :cerl.type(newVars2) do
                      :cons ->
                        newRaceVarMap =
                          race_var_map(
                            :cerl.cons_hd(newVars1),
                            :cerl.cons_hd(newVars2),
                            raceVarMap,
                            op
                          )

                        race_var_map(
                          :cerl.cons_tl(newVars1),
                          :cerl.cons_tl(newVars2),
                          newRaceVarMap,
                          op
                        )

                      :alias ->
                        race_var_map(newVars1, :cerl.alias_var(newVars2), raceVarMap, op)

                      :values ->
                        race_var_map(newVars1, :cerl.values_es(newVars2), raceVarMap, op)

                      _Other ->
                        raceVarMap
                    end

                  :alias ->
                    case :cerl.type(newVars2) do
                      :alias ->
                        race_var_map(
                          :cerl.alias_var(newVars1),
                          :cerl.alias_var(newVars2),
                          raceVarMap,
                          op
                        )

                      _Other ->
                        race_var_map(:cerl.alias_var(newVars1), newVars2, raceVarMap, op)
                    end

                  :values ->
                    case :cerl.type(newVars2) do
                      :values ->
                        race_var_map(
                          :cerl.values_es(newVars1),
                          :cerl.values_es(newVars2),
                          raceVarMap,
                          op
                        )

                      _Other ->
                        race_var_map(:cerl.values_es(newVars1), newVars2, raceVarMap, op)
                    end

                  _Other ->
                    raceVarMap
                end

              false ->
                raceVarMap
            end
        end
    end
  end

  defp race_var_map_clauses(clauses, raceVarMap) do
    case clauses do
      [] ->
        raceVarMap

      [r_end_clause(arg: arg, pats: pats, guard: guard) | t] ->
        {raceVarMap1, _RemoveClause} = race_var_map_guard(arg, pats, guard, raceVarMap, :bind)
        race_var_map_clauses(t, raceVarMap1)
    end
  end

  defp race_var_map_guard(arg, pats, guard, raceVarMap, op) do
    {newRaceVarMap, removeClause} =
      case :cerl.type(guard) do
        :call ->
          callName = :cerl.call_name(guard)

          case :cerl.is_literal(callName) do
            true ->
              case :cerl.concrete(callName) do
                :"=:=" ->
                  [arg1, arg2] = :cerl.call_args(guard)
                  {race_var_map(arg1, arg2, raceVarMap, op), false}

                :== ->
                  [arg1, arg2] = :cerl.call_args(guard)
                  {race_var_map(arg1, arg2, raceVarMap, op), false}

                :"=/=" ->
                  case op do
                    :bind ->
                      [arg1, arg2] = :cerl.call_args(guard)
                      {raceVarMap, are_bound_vars(arg1, arg2, raceVarMap)}

                    :unbind ->
                      {raceVarMap, false}
                  end

                _Other ->
                  {raceVarMap, false}
              end

            false ->
              {raceVarMap, false}
          end

        _Other ->
          {raceVarMap, false}
      end

    {raceVarMap1, removeClause1} =
      race_var_map_guard_helper1(arg, pats, race_var_map(arg, pats, newRaceVarMap, op), op)

    {raceVarMap1, removeClause or removeClause1}
  end

  defp race_var_map_guard_helper1(arg, pats, raceVarMap, op) do
    case arg === :no_arg or arg === :bypassed do
      true ->
        {raceVarMap, false}

      false ->
        case :cerl.type(arg) do
          :call ->
            case pats do
              [newPat] ->
                modName = :cerl.call_module(arg)
                callName = :cerl.call_name(arg)

                case :cerl.is_literal(modName) and :cerl.is_literal(callName) do
                  true ->
                    case {:cerl.concrete(modName), :cerl.concrete(callName)} do
                      {:erlang, :"=:="} ->
                        race_var_map_guard_helper2(arg, newPat, true, raceVarMap, op)

                      {:erlang, :==} ->
                        race_var_map_guard_helper2(arg, newPat, true, raceVarMap, op)

                      {:erlang, :"=/="} ->
                        race_var_map_guard_helper2(arg, newPat, false, raceVarMap, op)

                      _Else ->
                        {raceVarMap, false}
                    end

                  false ->
                    {raceVarMap, false}
                end

              _Other ->
                {raceVarMap, false}
            end

          _Other ->
            {raceVarMap, false}
        end
    end
  end

  defp race_var_map_guard_helper2(arg, pat0, bool, raceVarMap, op) do
    pat = :cerl.fold_literal(pat0)

    case :cerl.type(pat) do
      :literal ->
        [arg1, arg2] = :cerl.call_args(arg)

        case :cerl.concrete(pat) do
          ^bool ->
            {race_var_map(arg1, arg2, raceVarMap, op), false}

          _Else ->
            case op do
              :bind ->
                {raceVarMap, are_bound_vars(arg1, arg2, raceVarMap)}

              :unbind ->
                {raceVarMap, false}
            end
        end

      _Else ->
        {raceVarMap, false}
    end
  end

  defp unbind_dict_vars(var, var, raceVarMap) do
    raceVarMap
  end

  defp unbind_dict_vars(var1, var2, raceVarMap) do
    case :dict.find(var1, raceVarMap) do
      :error ->
        raceVarMap

      {:ok, labels} ->
        case labels do
          [] ->
            :dict.erase(var1, raceVarMap)

          _Else ->
            case :lists.member(var2, labels) do
              true ->
                unbind_dict_vars(
                  var1,
                  var2,
                  bind_dict_vars_list(
                    var1,
                    labels -- [var2],
                    :dict.erase(
                      var1,
                      raceVarMap
                    )
                  )
                )

              false ->
                unbind_dict_vars_helper(labels, var1, var2, raceVarMap)
            end
        end
    end
  end

  defp unbind_dict_vars_helper(labels, key, compLabel, raceVarMap) do
    case :dict.size(raceVarMap) do
      0 ->
        raceVarMap

      _ ->
        case labels do
          [] ->
            raceVarMap

          [head | tail] ->
            newRaceVarMap =
              case are_bound_labels(head, compLabel, raceVarMap) or
                     are_bound_labels(
                       compLabel,
                       head,
                       raceVarMap
                     ) do
                true ->
                  bind_dict_vars_list(
                    key,
                    labels -- [head],
                    :dict.erase(
                      key,
                      raceVarMap
                    )
                  )

                false ->
                  raceVarMap
              end

            unbind_dict_vars_helper(tail, key, compLabel, newRaceVarMap)
        end
    end
  end

  defp var_analysis(funDefArgs, funCallArgs, warnVarArgs, raceWarnTag) do
    case raceWarnTag do
      :warn_whereis_register ->
        [wVA1, wVA2, wVA3, wVA4] = warnVarArgs
        argNos = lists_key_members_lists(wVA1, funDefArgs)

        [
          for n <- argNos do
            lists_get(n, funCallArgs)
          end,
          wVA2,
          wVA3,
          wVA4
        ]

      :warn_whereis_unregister ->
        [wVA1, wVA2] = warnVarArgs
        argNos = lists_key_members_lists(wVA1, funDefArgs)

        [
          for n <- argNos do
            lists_get(n, funCallArgs)
          end,
          wVA2
        ]

      :warn_ets_lookup_insert ->
        [wVA1, wVA2, wVA3, wVA4] = warnVarArgs
        argNos1 = lists_key_members_lists(wVA1, funDefArgs)
        argNos2 = lists_key_members_lists(wVA3, funDefArgs)

        [
          for n1 <- argNos1 do
            lists_get(n1, funCallArgs)
          end,
          wVA2,
          for n2 <- argNos2 do
            lists_get(n2, funCallArgs)
          end,
          wVA4
        ]

      :warn_mnesia_dirty_read_write ->
        [wVA1, wVA2 | t] = warnVarArgs
        argNos = lists_key_members_lists(wVA1, funDefArgs)

        [
          for n <- argNos do
            lists_get(n, funCallArgs)
          end,
          wVA2 | t
        ]
    end
  end

  defp var_type_analysis(
         funDefArgs,
         funCallTypes,
         warnVarArgs,
         raceWarnTag,
         raceVarMap,
         cleanState
       ) do
    funVarArgs = format_args(funDefArgs, funCallTypes, cleanState, :function_call)

    case raceWarnTag do
      :warn_whereis_register ->
        [wVA1, wVA2, wVA3, wVA4] = warnVarArgs
        vars = find_all_bound_vars(wVA1, raceVarMap)

        case lists_key_member_lists(vars, funVarArgs) do
          0 ->
            [vars, wVA2, wVA3, wVA4]

          n when is_integer(n) ->
            newWVA2 =
              :string.lexemes(
                :lists.nth(n + 1, funVarArgs),
                ' |'
              )

            [vars, newWVA2, wVA3, wVA4]
        end

      :warn_whereis_unregister ->
        [wVA1, wVA2] = warnVarArgs
        vars = find_all_bound_vars(wVA1, raceVarMap)

        case lists_key_member_lists(vars, funVarArgs) do
          0 ->
            [vars, wVA2]

          n when is_integer(n) ->
            newWVA2 =
              :string.lexemes(
                :lists.nth(n + 1, funVarArgs),
                ' |'
              )

            [vars, newWVA2]
        end

      :warn_ets_lookup_insert ->
        [wVA1, wVA2, wVA3, wVA4] = warnVarArgs
        vars1 = find_all_bound_vars(wVA1, raceVarMap)

        firstVarArg =
          case lists_key_member_lists(
                 vars1,
                 funVarArgs
               ) do
            0 ->
              [vars1, wVA2]

            n1 when is_integer(n1) ->
              newWVA2 =
                :string.lexemes(
                  :lists.nth(
                    n1 + 1,
                    funVarArgs
                  ),
                  ' |'
                )

              [vars1, newWVA2]
          end

        vars2 =
          :lists.flatten(
            for a <- ets_list_args(wVA3) do
              find_all_bound_vars(a, raceVarMap)
            end
          )

        case lists_key_member_lists(vars2, funVarArgs) do
          0 ->
            firstVarArg ++ [vars2, wVA4]

          n2 when is_integer(n2) ->
            newWVA4 =
              ets_tuple_argtypes2(
                ets_tuple_argtypes1(
                  :lists.nth(
                    n2 + 1,
                    funVarArgs
                  ),
                  [],
                  [],
                  0
                ),
                []
              )

            firstVarArg ++ [vars2, newWVA4]
        end

      :warn_mnesia_dirty_read_write ->
        [wVA1, wVA2 | t] = warnVarArgs

        arity =
          case t do
            [] ->
              1

            _Else ->
              2
          end

        vars = find_all_bound_vars(wVA1, raceVarMap)

        case lists_key_member_lists(vars, funVarArgs) do
          0 ->
            [vars, wVA2 | t]

          n when is_integer(n) ->
            newWVA2 =
              case arity do
                1 ->
                  for r <-
                        :string.lexemes(
                          :lists.nth(2, funVarArgs),
                          ' |'
                        ) do
                    mnesia_record_tab(r)
                  end

                2 ->
                  :string.lexemes(:lists.nth(n + 1, funVarArgs), ' |')
              end

            [vars, newWVA2 | t]
        end
    end
  end

  defp add_race_warning(warn, r_races(race_warnings: warns) = races) do
    r_races(races, race_warnings: [warn | warns])
  end

  defp get_race_warn(fun, args, argTypes, depList, state) do
    {m, f, _A} = fun

    case depList do
      [] ->
        {state, :no_race}

      _Other ->
        {state, {:race_condition, [m, f, args, argTypes, state, depList]}}
    end
  end

  def get_race_warnings(r_races(race_warnings: raceWarnings), state) do
    get_race_warnings_helper(raceWarnings, state)
  end

  defp get_race_warnings_helper(warnings, state) do
    case warnings do
      [] ->
        {:dialyzer_dataflow.state__get_races(state), state}

      [h | t] ->
        {raceWarnTag, warningInfo, {:race_condition, [m, f, a, aT, s, depList]}} = h

        reason =
          case raceWarnTag do
            :warn_whereis_register ->
              get_reason(
                :lists.keysort(7, depList),
                'might fail due to a possible race condition caused by its combination with '
              )

            :warn_whereis_unregister ->
              get_reason(
                :lists.keysort(7, depList),
                'might fail due to a possible race condition caused by its combination with '
              )

            :warn_ets_lookup_insert ->
              get_reason(
                :lists.keysort(7, depList),
                'might have an unintended effect due to ' ++
                  'a possible race condition ' ++ 'caused by its combination with '
              )

            :warn_mnesia_dirty_read_write ->
              get_reason(
                :lists.keysort(7, depList),
                'might have an unintended effect due to ' ++
                  'a possible race condition ' ++ 'caused by its combination with '
              )
          end

        w =
          {:warn_race_condition, warningInfo,
           {:race_condition, [m, f, :dialyzer_dataflow.format_args(a, aT, s), reason]}}

        get_race_warnings_helper(
          t,
          :dialyzer_dataflow.state__add_warning(
            w,
            state
          )
        )
    end
  end

  defp get_reason(dependencyList, reason) do
    case dependencyList do
      [] ->
        ''

      [
        r_dep_call(
          call_name: call,
          arg_types: argTypes,
          vars: args,
          state: state,
          file_line: {file, line}
        )
        | t
      ] ->
        r =
          reason ++
            case call do
              :whereis ->
                'the erlang:whereis'

              :ets_lookup ->
                'the ets:lookup'

              :mnesia_dirty_read ->
                'the mnesia:dirty_read'
            end ++
            :dialyzer_dataflow.format_args(args, argTypes, state) ++
            ' call in ' ++
            :filename.basename(file) ++ ' on line ' ++ :lists.flatten(:io_lib.write(line))

        case t do
          [] ->
            r

          _ ->
            get_reason(t, r ++ ', ')
        end
    end
  end

  defp state__add_race_warning(state, raceWarn, raceWarnTag, warningInfo) do
    case raceWarn do
      :no_race ->
        state

      _Else ->
        races = :dialyzer_dataflow.state__get_races(state)
        warn = {raceWarnTag, warningInfo, raceWarn}

        :dialyzer_dataflow.state__put_races(
          add_race_warning(
            warn,
            races
          ),
          state
        )
    end
  end

  def beg_clause_new(arg, pats, guard) do
    r_beg_clause(arg: arg, pats: pats, guard: guard)
  end

  def cleanup(r_races(race_list: raceList)) do
    r_races(race_list: raceList)
  end

  def end_case_new(clauses) do
    r_end_case(clauses: clauses)
  end

  def end_clause_new(arg, pats, guard) do
    r_end_clause(arg: arg, pats: pats, guard: guard)
  end

  def get_curr_fun(r_races(curr_fun: currFun)) do
    currFun
  end

  def get_curr_fun_args(r_races(curr_fun_args: currFunArgs)) do
    currFunArgs
  end

  def get_new_table(r_races(new_table: table)) do
    table
  end

  def get_race_analysis(r_races(race_analysis: raceAnalysis)) do
    raceAnalysis
  end

  def get_race_list(r_races(race_list: raceList)) do
    raceList
  end

  def get_race_list_size(r_races(race_list_size: raceListSize)) do
    raceListSize
  end

  def get_race_list_and_size(
        r_races(
          race_list: raceList,
          race_list_size: raceListSize
        )
      ) do
    {raceList, raceListSize}
  end

  def let_tag_new(var, arg) do
    r_let_tag(var: var, arg: arg)
  end

  def new() do
    r_races()
  end

  def put_curr_fun(currFun, currFunLabel, races) do
    r_races(races, curr_fun: currFun, curr_fun_label: currFunLabel, curr_fun_args: :empty)
  end

  def put_fun_args(args, r_races(curr_fun_args: currFunArgs) = races) do
    case currFunArgs do
      :empty ->
        r_races(races, curr_fun_args: args)

      _Other ->
        races
    end
  end

  def put_race_analysis(analysis, races) do
    r_races(races, race_analysis: analysis)
  end

  def put_race_list(raceList, raceListSize, races) do
    r_races(races,
      race_list: raceList,
      race_list_size: raceListSize
    )
  end
end
