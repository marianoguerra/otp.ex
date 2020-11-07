defmodule :m_ct_groups do
  use Bitwise

  def find_groups(mod, grNames, tCs, groupDefs)
      when is_atom(grNames) or length(grNames) == 1 do
    find_groups1(mod, grNames, tCs, groupDefs)
  end

  def find_groups(mod, groups, tCs, groupDefs) when groups != [] do
    :lists.append(
      for grNames <- groups do
        find_groups1(mod, [grNames], tCs, groupDefs)
      end
    )
  end

  def find_groups(_Mod, [], _TCs, _GroupDefs) do
    []
  end

  defp find_groups1(mod, grNames, tCs, groupDefs) do
    {grNames1, findAll} =
      case grNames do
        name when is_atom(name) and name != :all ->
          {[name], true}

        [path] when is_list(path) ->
          {path, false}

        path ->
          {path, true}
      end

    tCs1 =
      cond do
        (is_atom(tCs) and tCs != :all) or is_tuple(tCs) ->
          [tCs]

        true ->
          tCs
      end

    found = find(mod, grNames1, tCs1, groupDefs, [], groupDefs, findAll)

    for conf <- found, conf != :NOMATCH do
      conf
    end
  end

  defp find(mod, :all, :all, [{name, props, tests} | gs], known, defs, _)
       when is_atom(name) and is_list(props) and
              is_list(tests) do
    cyclic_test(mod, name, known)

    trim(make_conf(mod, name, props, find(mod, :all, :all, tests, [name | known], defs, true))) ++
      find(mod, :all, :all, gs, known, defs, true)
  end

  defp find(mod, :all, tCs, [{name, props, tests} | gs], known, defs, _)
       when is_atom(name) and is_list(props) and
              is_list(tests) do
    cyclic_test(mod, name, known)
    tests1 = modify_tc_list(tests, tCs, [])

    trim(make_conf(mod, name, props, find(mod, :all, tCs, tests1, [name | known], defs, true))) ++
      find(mod, :all, tCs, gs, known, defs, true)
  end

  defp find(mod, [name | grNames] = sPath, tCs, [{name, props, tests} | gs], known, defs, findAll)
       when is_atom(name) and is_list(props) and
              is_list(tests) do
    cyclic_test(mod, name, known)
    tests1 = modify_tc_list(tests, tCs, grNames)

    trim(
      make_conf(mod, name, props, find(mod, grNames, tCs, tests1, [name | known], defs, findAll))
    ) ++ find(mod, sPath, tCs, gs, known, defs, findAll)
  end

  defp find(mod, [], tCs, tests, _Known, _Defs, false) do
    cases =
      :lists.flatmap(
        fn
          tC
          when is_atom(tC) and
                 tCs == :all ->
            [{mod, tC}]

          {:group, _} ->
            []

          {:testcase, tC, [prop]}
          when is_atom(tC) and
                 tC == :all ->
            [{:repeat, {mod, tC}, prop}]

          {_, _} = tC when tCs == :all ->
            [tC]

          tC when is_atom(tC) ->
            tuple = {mod, tC}

            case :lists.member(tuple, tCs) do
              true ->
                [tuple]

              false ->
                case :lists.member(tC, tCs) do
                  true ->
                    [tuple]

                  false ->
                    []
                end
            end

          {:testcase, tC, [prop]} when is_atom(tC) ->
            tuple = {mod, tC}

            case :lists.member(tuple, tCs) do
              true ->
                [{:repeat, tuple, prop}]

              false ->
                case :lists.member(tC, tCs) do
                  true ->
                    [{:repeat, tuple, prop}]

                  false ->
                    []
                end
            end

          _ ->
            []
        end,
        tests
      )

    cond do
      cases == [] ->
        [:NOMATCH]

      true ->
        cases
    end
  end

  defp find(_Mod, [_ | _], _TCs, [], _Known, _Defs, _) do
    [:NOMATCH]
  end

  defp find(mod, grNames, tCs, [{name, props, tests} | gs], known, defs, findAll)
       when is_atom(name) and is_list(props) and
              is_list(tests) do
    cyclic_test(mod, name, known)
    tests1 = modify_tc_list(tests, tCs, grNames)

    trim(
      make_conf(mod, name, props, find(mod, grNames, tCs, tests1, [name | known], defs, findAll))
    ) ++ find(mod, grNames, tCs, gs, known, defs, findAll)
  end

  defp find(mod, grNames, tCs, [{:group, name1} | gs], known, defs, findAll)
       when is_atom(name1) do
    find(mod, grNames, tCs, [expand(mod, name1, defs) | gs], known, defs, findAll)
  end

  defp find(mod, grNames, tCs, [{:group, extMod, extGrp} | gs], known, defs, findAll)
       when is_atom(extMod) and is_atom(extGrp) do
    externalDefs = extMod.groups()
    externalTCs = find(extMod, extGrp, tCs, [{:group, extGrp}], [], externalDefs, findAll)
    externalTCs ++ find(mod, grNames, tCs, gs, known, defs, findAll)
  end

  defp find(mod, grNames, tCs, [{name1, tests} | gs], known, defs, findAll)
       when is_atom(name1) and is_list(tests) do
    find(mod, grNames, tCs, [{name1, [], tests} | gs], known, defs, findAll)
  end

  defp find(mod, grNames, tCs, [{externalTC, case__} = tC | gs], known, defs, findAll)
       when is_atom(externalTC) and is_atom(case__) do
    [tC | find(mod, grNames, tCs, gs, known, defs, findAll)]
  end

  defp find(mod, grNames, :all, [tC | gs], known, defs, findAll)
       when is_atom(tC) do
    [{mod, tC} | find(mod, grNames, :all, gs, known, defs, findAll)]
  end

  defp find(mod, grNames, :all, [{m, tC} | gs], known, defs, findAll)
       when is_atom(m) and m != :group and is_atom(tC) do
    [{m, tC} | find(mod, grNames, :all, gs, known, defs, findAll)]
  end

  defp find(mod, grNames, :all, [{:testcase, tC, [prop]} | gs], known, defs, findAll)
       when is_atom(tC) do
    [{:repeat, {mod, tC}, prop} | find(mod, grNames, :all, gs, known, defs, findAll)]
  end

  defp find(mod, grNames, tCs, [tC | gs], known, defs, findAll)
       when is_atom(tC) or
              (:erlang.size(tC) == 3 and
                 :erlang.element(
                   1,
                   tC
                 ) == :testcase) or
              (:erlang.size(tC) == 2 and
                 :erlang.element(
                   1,
                   tC
                 ) != :group) do
    case__ =
      case tC do
        _ when is_atom(tC) ->
          tuple = {mod, tC}

          case :lists.member(tuple, tCs) do
            true ->
              tuple

            false ->
              case :lists.member(tC, tCs) do
                true ->
                  {mod, tC}

                false ->
                  []
              end
          end

        {:testcase, tC0, [prop]} when is_atom(tC0) ->
          tuple = {mod, tC0}

          case :lists.member(tuple, tCs) do
            true ->
              {:repeat, tuple, prop}

            false ->
              case :lists.member(tC0, tCs) do
                true ->
                  {:repeat, {mod, tC0}, prop}

                false ->
                  []
              end
          end

        _ ->
          case :lists.member(tC, tCs) do
            true ->
              {mod, tC}

            false ->
              []
          end
      end

    cond do
      case__ == [] ->
        find(mod, grNames, tCs, gs, known, defs, findAll)

      true ->
        [case__ | find(mod, grNames, tCs, gs, known, defs, findAll)]
    end
  end

  defp find(mod, _GrNames, _TCs, [badTerm | _Gs], known, _Defs, _FindAll) do
    where =
      cond do
        length(known) == 0 ->
          :erlang.atom_to_list(mod) ++ ':groups/0'

        true ->
          'group ' ++
            :erlang.atom_to_list(:lists.last(known)) ++
            ' in ' ++ :erlang.atom_to_list(mod) ++ ':groups/0'
      end

    term = :io_lib.format('~tp', [badTerm])
    e = 'Bad term ' ++ :lists.flatten(term) ++ ' in ' ++ where
    throw({:error, :erlang.list_to_atom(e)})
  end

  defp find(_Mod, _GrNames, _TCs, [], _Known, _Defs, _) do
    []
  end

  defp trim({:conf, props, init, tests, end__}) do
    try do
      trim(tests)
    catch
      _ ->
        []
    else
      [] ->
        []

      tests1 ->
        [{:conf, props, init, tests1, end__}]
    end
  end

  defp trim(tests) when is_list(tests) do
    tests1 =
      :lists.flatmap(
        fn test ->
          isConf =
            case test do
              {:conf, _, _, _, _} ->
                true

              _ ->
                false
            end

          try do
            trim_test(test)
          catch
            _ ->
              [:NOMATCH]
          else
            [] ->
              []

            test1 when isConf ->
              [{:conf, test1}]

            test1 ->
              [test1]
          end
        end,
        tests
      )

    case :lists.keymember(:conf, 1, tests1) do
      true ->
        :lists.flatmap(
          fn
            {:conf, test} ->
              [test]

            :NOMATCH ->
              []

            test ->
              [test]
          end,
          tests1
        )

      false ->
        case :lists.member(:NOMATCH, tests1) do
          true ->
            throw(:NOMATCH)

          false ->
            tests1
        end
    end
  end

  defp trim_test({:conf, props, init, tests, end__}) do
    case trim(tests) do
      [] ->
        []

      tests1 ->
        {:conf, props, init, tests1, end__}
    end
  end

  defp trim_test(:NOMATCH) do
    throw(:NOMATCH)
  end

  defp trim_test(test) do
    test
  end

  defp modify_tc_list(grSpecTs, :all, []) do
    grSpecTs
  end

  defp modify_tc_list(grSpecTs, tSCs, []) do
    modify_tc_list1(grSpecTs, tSCs)
  end

  defp modify_tc_list(grSpecTs, _TSCs, _) do
    for test <- grSpecTs, not is_atom(test), :erlang.element(1, test) !== :testcase do
      test
    end
  end

  defp modify_tc_list1(grSpecTs, tSCs) do
    grSpecTs1 =
      :lists.flatmap(
        fn
          test = {:testcase, tC, _} ->
            case :lists.keysearch(tC, 2, tSCs) do
              {:value, _} ->
                [test]

              _ ->
                case :lists.member(tC, tSCs) do
                  true ->
                    [test]

                  false ->
                    []
                end
            end

          test
          when is_tuple(test) and
                 :erlang.size(test) > 2 ->
            [test]

          test = {:group, _} ->
            [test]

          test = {_M, tC} ->
            case :lists.member(tC, tSCs) do
              true ->
                [test]

              false ->
                []
            end

          test when is_atom(test) ->
            case :lists.keysearch(test, 2, tSCs) do
              {:value, _} ->
                [test]

              _ ->
                case :lists.member(test, tSCs) do
                  true ->
                    [test]

                  false ->
                    []
                end
            end

          test ->
            [test]
        end,
        grSpecTs
      )

    {tSCs2, grSpecTs3} =
      :lists.foldr(
        fn tC, {tSCs1, grSpecTs2} ->
          case :lists.member(
                 tC,
                 grSpecTs1
               ) do
            true ->
              {[tC | tSCs1], :lists.delete(tC, grSpecTs2)}

            false ->
              case :lists.keysearch(tC, 2, grSpecTs) do
                {:value, test} ->
                  {[test | tSCs1], :lists.keydelete(tC, 2, grSpecTs2)}

                false ->
                  {tSCs1, grSpecTs2}
              end
          end
        end,
        {[], grSpecTs1},
        tSCs
      )

    tSCs2 ++ grSpecTs3
  end

  def delete_subs([{:conf, _, _, _, _} = conf | confs], all) do
    all1 = delete_conf(conf, all)

    case is_sub(conf, all1) do
      true ->
        delete_subs(confs, all1)

      false ->
        delete_subs(confs, all)
    end
  end

  def delete_subs([_Else | confs], all) do
    delete_subs(confs, all)
  end

  def delete_subs([], all) do
    all
  end

  defp delete_conf({:conf, props, _, _, _}, confs) do
    name = :proplists.get_value(:name, props)

    for conf = {:conf, props0, _, _, _} <- confs,
        name !== :proplists.get_value(:name, props0) do
      conf
    end
  end

  defp is_sub(
         {:conf, props, _, _, _} = conf,
         [{:conf, _, _, tests, _} | confs]
       ) do
    name = :proplists.get_value(:name, props)

    case :lists.any(
           fn
             {:conf, props0, _, _, _} ->
               case :proplists.get_value(:name, props0) do
                 n when n == name ->
                   true

                 _ ->
                   false
               end

             _ ->
               false
           end,
           tests
         ) do
      true ->
        true

      false ->
        is_sub(conf, tests) or is_sub(conf, confs)
    end
  end

  defp is_sub(conf, [_TC | tests]) do
    is_sub(conf, tests)
  end

  defp is_sub(_Conf, []) do
    false
  end

  defp cyclic_test(mod, name, names) do
    case :lists.member(name, names) do
      true ->
        e =
          'Cyclic reference to group ' ++
            :erlang.atom_to_list(name) ++ ' in ' ++ :erlang.atom_to_list(mod) ++ ':groups/0'

        throw({:error, :erlang.list_to_atom(e)})

      false ->
        :ok
    end
  end

  defp expand(mod, name, defs) do
    case :lists.keysearch(name, 1, defs) do
      {:value, def__} ->
        def__

      false ->
        e =
          'Invalid group ' ++
            :erlang.atom_to_list(name) ++ ' in ' ++ :erlang.atom_to_list(mod) ++ ':groups/0'

        throw({:error, :erlang.list_to_atom(e)})
    end
  end

  def make_all_conf(dir, mod, props, testSpec) do
    _ = load_abs(dir, mod)
    make_all_conf(mod, props, testSpec)
  end

  def make_all_conf(mod, props, testSpec) do
    case (try do
            apply(mod, :groups, [])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        exit({:invalid_group_definition, mod})

      groupDefs when is_list(groupDefs) ->
        case (try do
                find_groups(mod, :all, testSpec, groupDefs)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:error, _} = error ->
            [{:ct_framework, :error_in_suite, [[error]]}]

          [] ->
            exit({:invalid_group_spec, mod})

          _ConfTests ->
            make_conf(mod, :all, props, testSpec)
        end
    end
  end

  def make_conf(dir, mod, name, props, testSpec) do
    _ = load_abs(dir, mod)
    make_conf(mod, name, props, testSpec)
  end

  defp load_abs(dir, mod) do
    case :code.is_loaded(mod) do
      false ->
        :code.load_abs(
          :filename.join(
            dir,
            :erlang.atom_to_list(mod)
          )
        )

      _ ->
        :ok
    end
  end

  defp make_conf(mod, name, props, testSpec) do
    _ =
      case :code.is_loaded(mod) do
        false ->
          :code.load_file(mod)

        _ ->
          :ok
      end

    {initConf, endConf, extraProps} =
      case {:erlang.function_exported(mod, :init_per_group, 2),
            :erlang.function_exported(mod, :end_per_group, 2)} do
        {false, false} ->
          :ct_logs.log(
            'TEST INFO',
            'init_per_group/2 and end_per_group/2 missing for group ~tw in ~w, using default.',
            [name, mod]
          )

          {{:ct_framework, :init_per_group}, {:ct_framework, :end_per_group}, [{:suite, mod}]}

        _ ->
          {{mod, :init_per_group}, {mod, :end_per_group}, []}
      end

    {:conf, [{:name, name} | props ++ extraProps], initConf, testSpec, endConf}
  end

  def expand_groups([h | t], confTests, mod) do
    [expand_groups(h, confTests, mod) | expand_groups(t, confTests, mod)]
  end

  def expand_groups([], _ConfTests, _Mod) do
    []
  end

  def expand_groups({:group, name}, confTests, mod) do
    expand_groups({:group, name, :default, []}, confTests, mod)
  end

  def expand_groups({:group, name, :default}, confTests, mod) do
    expand_groups({:group, name, :default, []}, confTests, mod)
  end

  def expand_groups({:group, name, oRProps}, confTests, mod)
      when is_list(oRProps) do
    expand_groups({:group, name, oRProps, []}, confTests, mod)
  end

  def expand_groups({:group, name, oRProps, subORSpec}, confTests, mod) do
    findConf = fn conf = {:conf, props, init, ts, end__} ->
      case :proplists.get_value(:name, props) do
        ^name when oRProps == :default ->
          [conf]

        ^name ->
          props1 =
            case :proplists.get_value(:suite, props) do
              :undefined ->
                oRProps

              suiteName ->
                [{:suite, suiteName} | oRProps]
            end

          [{:conf, [{:name, name} | props1], init, ts, end__}]

        _ ->
          []
      end
    end

    case :lists.flatmap(findConf, confTests) do
      [] ->
        throw({:error, invalid_ref_msg(name, mod)})

      matching when subORSpec == [] ->
        matching

      matching ->
        override_props(matching, subORSpec, name, mod)
    end
  end

  def expand_groups(seqOrTC, _ConfTests, _Mod) do
    seqOrTC
  end

  def search_and_override([conf = {:conf, props, init, tests, end__}], oRSpec, mod) do
    insProps = fn
      grName, :undefined, ps ->
        [{:name, grName} | ps]

      grName, suite, ps ->
        [[{:name, grName}, {:suite, suite}] | ps]
    end

    name = :proplists.get_value(:name, props)
    suite = :proplists.get_value(:suite, props)

    case :lists.keysearch(name, 1, oRSpec) do
      {:value, {^name, :default}} ->
        [conf]

      {:value, {^name, oRProps}} ->
        [{:conf, insProps.(name, suite, oRProps), init, tests, end__}]

      {:value, {^name, :default, []}} ->
        [conf]

      {:value, {^name, :default, subORSpec}} ->
        override_props([conf], subORSpec, name, mod)

      {:value, {^name, oRProps, subORSpec}} ->
        override_props(
          [{:conf, insProps.(name, suite, oRProps), init, tests, end__}],
          subORSpec,
          name,
          mod
        )

      _ ->
        [{:conf, props, init, search_and_override(tests, oRSpec, mod), end__}]
    end
  end

  defp override_props([{:conf, props, init, tests, end__} | confs], subORSpec, name, mod) do
    {subs, subORSpec1} = override_sub_props(tests, [], subORSpec, mod)

    [
      {:conf, props, init, subs, end__}
      | override_props(confs, subORSpec1, name, mod)
    ]
  end

  defp override_props([], [], _, _) do
    []
  end

  defp override_props([], subORSpec, name, mod) do
    es =
      for spec <- subORSpec do
        invalid_ref_msg(name, :erlang.element(1, spec), mod)
      end

    throw({:error, es})
  end

  defp override_sub_props([], new, oRSpec, _) do
    {:lists.reverse(new), oRSpec}
  end

  defp override_sub_props([t = {:conf, props, init, tests, end__} | ts], new, oRSpec, mod) do
    name = :proplists.get_value(:name, props)
    suite = :proplists.get_value(:suite, props)

    case :lists.keysearch(name, 1, oRSpec) do
      {:value, spec} ->
        props1 =
          case :erlang.element(2, spec) do
            :default ->
              props

            oRProps when suite == :undefined ->
              [{:name, name} | oRProps]

            oRProps ->
              [[{:name, name}, {:suite, suite}] | oRProps]
          end

        case (try do
                :erlang.element(3, spec)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          undef
          when undef == [] or
                 :EXIT == :erlang.element(1, undef) ->
            override_sub_props(
              ts,
              [{:conf, props1, init, tests, end__} | new],
              :lists.keydelete(name, 1, oRSpec),
              mod
            )

          subORSpec when is_list(subORSpec) ->
            case override_sub_props(tests, [], subORSpec, mod) do
              {subs, []} ->
                override_sub_props(
                  ts,
                  [{:conf, props1, init, subs, end__} | new],
                  :lists.keydelete(name, 1, oRSpec),
                  mod
                )

              {_, nonEmptySpec} ->
                es =
                  for grRef <- nonEmptySpec do
                    invalid_ref_msg(name, :erlang.element(1, grRef), mod)
                  end

                throw({:error, es})
            end

          badGrSpec ->
            throw({:error, {:invalid_form, badGrSpec}})
        end

      _ ->
        override_sub_props(ts, [t | new], oRSpec, mod)
    end
  end

  defp override_sub_props([tC | ts], new, oRSpec, mod) do
    override_sub_props(ts, [tC | new], oRSpec, mod)
  end

  defp invalid_ref_msg(name, mod) do
    e =
      'Invalid reference to group ' ++
        :erlang.atom_to_list(name) ++ ' in ' ++ :erlang.atom_to_list(mod) ++ ':all/0'

    :erlang.list_to_atom(e)
  end

  defp invalid_ref_msg(name0, name1, mod) do
    e =
      'Invalid reference to group ' ++
        :erlang.atom_to_list(name1) ++
        ' from ' ++ :erlang.atom_to_list(name0) ++ ' in ' ++ :erlang.atom_to_list(mod) ++ ':all/0'

    :erlang.list_to_atom(e)
  end
end
