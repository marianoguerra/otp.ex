defmodule :m_systools_rc do
  use Bitwise
  require Record

  Record.defrecord(:r_release, :release,
    name: :undefined,
    vsn: :undefined,
    erts_vsn: :undefined,
    applications: :undefined,
    incl_apps: :undefined
  )

  Record.defrecord(:r_application, :application,
    name: :undefined,
    type: :permanent,
    vsn: '',
    id: '',
    description: '',
    modules: [],
    uses: [],
    includes: [],
    regs: [],
    env: [],
    maxT: :infinity,
    maxP: :infinity,
    mod: [],
    start_phases: :undefined,
    dir: ''
  )

  def translate_scripts(scripts, appls, preAppls) do
    translate_scripts(:up, scripts, appls, preAppls)
  end

  def translate_scripts(mode, scripts, appls, preAppls) do
    scripts2 = expand_scripts(scripts)

    case (try do
            do_translate_scripts(mode, scripts2, appls, preAppls)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, newScript} ->
        {:ok, newScript}

      {:error, reason} ->
        {:error, :systools_rc, reason}

      {:EXIT, reason} ->
        {:error, :systools_rc, reason}
    end
  end

  defp expand_scripts([script | scripts]) do
    [expand_script(script) | expand_scripts(scripts)]
  end

  defp expand_scripts([]) do
    []
  end

  defp expand_script([i | script]) do
    i2 =
      case i do
        {:load_module, mod} ->
          {:load_module, mod, :brutal_purge, :brutal_purge, []}

        {:load_module, mod, mods} when is_list(mods) ->
          {:load_module, mod, :brutal_purge, :brutal_purge, mods}

        {:update, mod} ->
          {:update, mod, :soft, :brutal_purge, :brutal_purge, []}

        {:update, mod, :supervisor} ->
          {:update, mod, :static, :default, {:advanced, []}, :brutal_purge, :brutal_purge, []}

        {:update, mod, change} when is_tuple(change) ->
          {:update, mod, change, :brutal_purge, :brutal_purge, []}

        {:update, mod, change} when change == :soft ->
          {:update, mod, change, :brutal_purge, :brutal_purge, []}

        {:update, mod, mods} when is_list(mods) ->
          {:update, mod, :soft, :brutal_purge, :brutal_purge, mods}

        {:update, mod, change, mods}
        when is_tuple(change) and
               is_list(mods) ->
          {:update, mod, change, :brutal_purge, :brutal_purge, mods}

        {:update, mod, change, mods}
        when change == :soft and
               is_list(mods) ->
          {:update, mod, change, :brutal_purge, :brutal_purge, mods}

        {:add_application, application} ->
          {:add_application, application, :permanent}

        _ ->
          i
      end

    cond do
      is_list(i2) ->
        i2 ++ expand_script(script)

      true ->
        [i2 | expand_script(script)]
    end
  end

  defp expand_script([]) do
    []
  end

  defp do_translate_scripts(mode, scripts, appls, preAppls) do
    mergedScript = merge_scripts(scripts)
    translate_merged_script(mode, mergedScript, appls, preAppls)
  end

  defp translate_merged_script(mode, script, appls, preAppls) do
    check_syntax(script)
    script1 = normalize_instrs(script)
    {before, after__} = split_script(script1)
    check_script(before, after__)
    {before1, after1} = translate_independent_instrs(before, after__, appls, preAppls)
    {before2, after2} = translate_dependent_instrs(mode, before1, after1, appls)
    before3 = merge_load_object_code(before2)
    {before4, after4} = sort_emulator_restart(mode, before3, after2)
    newScript = before4 ++ [:point_of_no_return | after4]
    check_syntax(newScript)
    {:ok, newScript}
  end

  defp merge_scripts(scripts) do
    {before, after__} =
      :lists.foldl(
        fn script, {b1, a1} ->
          {b2, a2} = split_script(script)
          {b1 ++ b2, a1 ++ a2}
        end,
        {[], []},
        scripts
      )

    before ++ [:point_of_no_return | after__]
  end

  defp split_script(script) do
    {before, after__} = split_instrs(script)

    :lists.foreach(
      fn
        {:load_object_code, _} ->
          :ok

        {:apply, _} ->
          :ok

        instruction ->
          throw({:error, {:bad_op_before_point_of_no_return, instruction}})
      end,
      before
    )

    {found, rest} =
      split(
        fn
          {:load_object_code, _} ->
            true

          _ ->
            false
        end,
        after__
      )

    {before ++ found, rest}
  end

  defp split_instrs(script) do
    split_instrs(script, [])
  end

  defp split_instrs([:point_of_no_return | t], before) do
    case :lists.member(:point_of_no_return, t) do
      true ->
        throw({:error, :too_many_point_of_no_return})

      false ->
        {:lists.reverse(before), t}
    end
  end

  defp split_instrs([h | t], before) do
    split_instrs(t, [h | before])
  end

  defp split_instrs([], before) do
    {[], :lists.reverse(before)}
  end

  defp check_script(before, after__) do
    check_load(before, after__)
    check_suspend_resume(after__)
    check_start_stop(after__)
  end

  defp check_load(before, after__) do
    :lists.foreach(
      fn
        {:load, {mod, _, _}} ->
          case find_object_code(mod, before) do
            true ->
              :ok

            false ->
              throw({:error, {:no_object_code, mod}})
          end

        _ ->
          :ok
      end,
      after__
    )
  end

  defp find_object_code(mod, [{:load_object_code, {_, _, mods}} | t]) do
    case :lists.member(mod, mods) do
      true ->
        true

      false ->
        find_object_code(mod, t)
    end
  end

  defp find_object_code(mod, [_ | t]) do
    find_object_code(mod, t)
  end

  defp find_object_code(_Mod, []) do
    false
  end

  defp check_suspend_resume(script) do
    suspended =
      :lists.map(
        fn
          {mod, _Timeout} ->
            mod

          mod ->
            mod
        end,
        :lists.flatten(
          for {:suspend, x} <- script do
            x
          end
        )
      )

    resumed =
      :lists.flatten(
        for {:resume, x} <- script do
          x
        end
      )

    codeChanged =
      :lists.flatten(
        for {:code_change, _, {x, _}} <- script do
          x
        end
      )

    case difference(suspended, resumed) do
      [] ->
        :ok

      s2 ->
        throw({:error, {:suspended_not_resumed, s2}})
    end

    case difference(resumed, suspended) do
      [] ->
        :ok

      r2 ->
        throw({:error, {:resumed_not_suspended, r2}})
    end

    case difference(codeChanged, suspended) do
      [] ->
        :ok

      c2 ->
        throw({:error, {:code_change_not_suspended, c2}})
    end
  end

  defp check_start_stop(script) do
    start =
      :lists.flatten(
        for {:start, x} <- script do
          x
        end
      )

    stop =
      :lists.flatten(
        for {:stop, x} <- script do
          x
        end
      )

    case difference(start, stop) do
      [] ->
        :ok

      s2 ->
        throw({:error, {:start_not_stop, s2}})
    end

    case difference(stop, start) do
      [] ->
        :ok

      s3 ->
        throw({:error, {:stop_not_start, s3}})
    end
  end

  defp normalize_instrs(script) do
    :lists.map(
      fn
        {:update, mod, change, prePurge, postPurge, mods} ->
          {:update, mod, :dynamic, :default, change, prePurge, postPurge, mods}

        {:update, mod, timeout, change, prePurge, postPurge, mods} ->
          {:update, mod, :dynamic, timeout, change, prePurge, postPurge, mods}

        {:add_module, mod} ->
          {:add_module, mod, []}

        {:delete_module, mod} ->
          {:delete_module, mod, []}

        i ->
          i
      end,
      script
    )
  end

  defp translate_independent_instrs(before, after__, appls, preAppls) do
    after1 = translate_application_instrs(after__, appls, preAppls)
    translate_add_module_instrs(before, after1)
  end

  defp translate_application_instrs(script, appls, preAppls) do
    l =
      :lists.map(
        fn
          {:add_application, appl, type} ->
            case :lists.keysearch(appl, r_application(:name), appls) do
              {:value, application} ->
                mods = r_application(application, :modules)

                applyL =
                  case type do
                    :none ->
                      []

                    :load ->
                      [{:apply, {:application, :load, [appl]}}]

                    _ ->
                      [{:apply, {:application, :start, [appl, type]}}]
                  end

                for m <- mods do
                  {:add_module, m, []}
                end ++ applyL

              false ->
                throw({:error, {:no_such_application, appl}})
            end

          {:remove_application, appl} ->
            case :lists.keysearch(appl, r_application(:name), appls) do
              {:value, _Application} ->
                throw({:error, {:removed_application_present, appl}})

              false ->
                :ignore
            end

            case :lists.keysearch(appl, r_application(:name), preAppls) do
              {:value, remApplication} ->
                mods = r_application(remApplication, :modules)

                [{:apply, {:application, :stop, [appl]}}] ++
                  for m <- mods do
                    {:remove, {m, :brutal_purge, :brutal_purge}}
                  end ++ [{:purge, mods}, {:apply, {:application, :unload, [appl]}}]

              false ->
                throw({:error, {:no_such_application, appl}})
            end

          {:restart_application, appl} ->
            case :lists.keysearch(appl, r_application(:name), preAppls) do
              {:value, preApplication} ->
                preMods = r_application(preApplication, :modules)

                case :lists.keysearch(appl, r_application(:name), appls) do
                  {:value, postApplication} ->
                    postMods = r_application(postApplication, :modules)
                    type = r_application(postApplication, :type)

                    apply =
                      case type do
                        :none ->
                          []

                        :load ->
                          [{:apply, {:application, :load, [appl]}}]

                        _ ->
                          [{:apply, {:application, :start, [appl, type]}}]
                      end

                    [{:apply, {:application, :stop, [appl]}}] ++
                      for m <- preMods do
                        {:remove, {m, :brutal_purge, :brutal_purge}}
                      end ++
                      [{:purge, preMods}] ++
                      for m <- postMods do
                        {:add_module, m, []}
                      end ++ apply

                  false ->
                    throw({:error, {:no_such_application, appl}})
                end

              false ->
                throw({:error, {:no_such_application, appl}})
            end

          x ->
            x
        end,
        script
      )

    :lists.flatten(l)
  end

  defp translate_add_module_instrs(before, after__) do
    nAfter =
      :lists.map(
        fn
          {:add_module, mod, mods} ->
            {:load_module, mod, :brutal_purge, :brutal_purge, mods}

          i ->
            i
        end,
        after__
      )

    {before, nAfter}
  end

  defp translate_dependent_instrs(mode, before, after__, appls) do
    g = make_dependency_graph(after__)
    wCs = :digraph_utils.components(g)
    {nBefore, nAfter} = translate_dep_loop(g, wCs, after__, appls, [], [], mode)
    :digraph.delete(g)
    {before ++ nBefore, nAfter}
  end

  defp translate_dep_loop(g, wCs, [i | is], appls, before, after__, mode)
       when is_tuple(i) and :erlang.size(i) > 1 do
    iName = :erlang.element(1, i)

    case :lists.member(
           iName,
           [:update, :load_module, :add_module, :delete_module]
         ) do
      true ->
        mod = :erlang.element(2, i)
        depIs = get_dependent_instructions(g, wCs, mod)
        {b2, a2} = translate_dep_to_low(mode, depIs, appls)
        remIs = difference([i | is], depIs)
        translate_dep_loop(g, wCs, remIs, appls, before ++ b2, after__ ++ a2, mode)

      false ->
        translate_dep_loop(g, wCs, is, appls, before, after__ ++ [i], mode)
    end
  end

  defp translate_dep_loop(g, wCs, [i | is], appls, before, after__, mode) do
    translate_dep_loop(g, wCs, is, appls, before, after__ ++ [i], mode)
  end

  defp translate_dep_loop(_G, _WCs, [], _Appls, before, after__, _Mode) do
    {before, after__}
  end

  defp make_dependency_graph(instructions) do
    depIs =
      :lists.filter(
        fn
          i when is_tuple(i) ->
            iName = :erlang.element(1, i)

            :lists.member(
              iName,
              [:update, :load_module, :add_module, :delete_module]
            )

          _ ->
            false
        end,
        instructions
      )

    {vDs, _} =
      :lists.mapfoldl(
        fn i, n ->
          mod = :erlang.element(2, i)
          mods = :erlang.element(:erlang.size(i), i)
          {{mod, mods, {n, i}}, n + 1}
        end,
        1,
        depIs
      )

    g = :digraph.new()

    :lists.foreach(
      fn {mod, _Mods, data} ->
        case :digraph.vertex(g, mod) do
          false ->
            :digraph.add_vertex(g, mod, data)

          _ ->
            throw({:error, {:muldef_module, mod}})
        end
      end,
      vDs
    )

    :lists.foreach(
      fn {mod, mods, _Data} ->
        :lists.foreach(
          fn m ->
            case :digraph.add_edge(g, mod, m) do
              {:error, _Reason} ->
                throw({:error, {:undef_module, m}})

              _ ->
                :ok
            end
          end,
          mods
        )
      end,
      vDs
    )

    g
  end

  defp get_dependent_instructions(g, wCs, mod) do
    case :lists.filter(
           fn c ->
             :lists.member(mod, c)
           end,
           wCs
         ) do
      [wC] ->
        h = restriction(wC, g)
        s = condensation(h)
        ts = :digraph_utils.topsort(s)

        depIss =
          :lists.map(
            fn t ->
              nIs =
                :lists.map(
                  fn v ->
                    {_, data} =
                      :digraph.vertex(
                        h,
                        v
                      )

                    data
                  end,
                  t
                )

              sortedNIs = :lists.keysort(1, nIs)

              :lists.map(
                fn {_N, i} ->
                  i
                end,
                sortedNIs
              )
            end,
            ts
          )

        depIs = :lists.flatten(depIss)
        :digraph.delete(h)
        :digraph.delete(s)
        depIs

      [] ->
        throw({:error, {:undef_module, mod}})

      _ ->
        throw({:error, {:muldef_module, mod}})
    end
  end

  defp translate_dep_to_low(mode, instructions, appls) do
    updateMods =
      filtermap(
        fn
          {:update, mod, _, :default, _, _, _, _} ->
            {true, mod}

          {:update, mod, _, t, _, _, _, _} ->
            {true, {mod, t}}

          _ ->
            false
        end,
        instructions
      )

    revUpdateMods = :lists.reverse(updateMods)

    suspendInstrs =
      cond do
        updateMods == [] ->
          []

        true ->
          [{:suspend, updateMods}]
      end

    resumeInstrs =
      cond do
        updateMods == [] ->
          []

        true ->
          [
            {:resume,
             :lists.map(
               fn
                 {mod, _T} ->
                   mod

                 mod ->
                   mod
               end,
               revUpdateMods
             )}
          ]
      end

    loadRemoveInstrs0 =
      filtermap(
        fn
          {:update, mod, _, _, _, preP, postP, _} ->
            {true, {:load, {mod, preP, postP}}}

          {:load_module, mod, preP, postP, _} ->
            {true, {:load, {mod, preP, postP}}}

          {:delete_module, mod, _} ->
            {true, [{:remove, {mod, :brutal_purge, :brutal_purge}}, {:purge, [mod]}]}

          _ ->
            false
        end,
        instructions
      )

    loadRemoveInstrs = :lists.flatten(loadRemoveInstrs0)
    revLoadRemoveInstrs = :lists.flatten(:lists.reverse(loadRemoveInstrs0))

    loadObjCodeInstrs =
      filtermap(
        fn
          {:load, {mod, _, _}} ->
            {lib, libVsn} = get_lib(mod, appls)
            {true, {:load_object_code, {lib, libVsn, [mod]}}}

          _ ->
            false
        end,
        loadRemoveInstrs
      )

    cond do
      mode == :up ->
        codeChangeMods =
          filtermap(
            fn
              {:update, mod, _, _, {:advanced, extra}, _, _, _} ->
                {true, {mod, extra}}

              _ ->
                false
            end,
            instructions
          )

        codeChangeInstrs =
          cond do
            codeChangeMods == [] ->
              []

            true ->
              [{:code_change, :up, codeChangeMods}]
          end

        {loadObjCodeInstrs,
         suspendInstrs ++ revLoadRemoveInstrs ++ codeChangeInstrs ++ resumeInstrs}

      mode == :dn ->
        preCodeChangeMods =
          for {:update, mod, :dynamic, _, {:advanced, extra}, _, _, _} <- instructions do
            {mod, extra}
          end

        preCodeChangeInstrs =
          cond do
            preCodeChangeMods == [] ->
              []

            true ->
              [{:code_change, :down, preCodeChangeMods}]
          end

        postCodeChangeMods =
          for {:update, mod, :static, _, {:advanced, extra}, _, _, _} <- instructions do
            {mod, extra}
          end

        postCodeChangeInstrs =
          cond do
            postCodeChangeMods == [] ->
              []

            true ->
              [{:code_change, :down, postCodeChangeMods}]
          end

        {loadObjCodeInstrs,
         suspendInstrs ++
           preCodeChangeInstrs ++ loadRemoveInstrs ++ postCodeChangeInstrs ++ resumeInstrs}
    end
  end

  defp get_lib(
         mod,
         [r_application(name: name, vsn: vsn, modules: modules) | t]
       ) do
    case :lists.member(mod, modules) do
      true ->
        {name, vsn}

      false ->
        get_lib(mod, t)
    end
  end

  defp get_lib(mod, []) do
    throw({:error, {:no_such_module, mod}})
  end

  defp merge_load_object_code(before) do
    {found, rest} =
      split(
        fn
          {:load_object_code, _} ->
            true

          _ ->
            false
        end,
        before
      )

    mlo(found) ++ rest
  end

  defp mlo([
         {:load_object_code, {lib, libVsn, mods}}
         | t
       ]) do
    {same, other} =
      split(
        fn
          {:load_object_code, {lib2, libVsn2, _Mods2}}
          when lib == lib2 and libVsn == libVsn2 ->
            true

          {:load_object_code, {lib2, libVsn2, _Mods2}}
          when lib == lib2 ->
            throw({:error, {:conflicting_versions, lib, libVsn, libVsn2}})

          _ ->
            false
        end,
        t
      )

    oCode0 =
      :lists.foldr(
        fn {:load_object_code, {_, _, ms}}, res ->
          u = union(ms, res)
          u
        end,
        [],
        same
      )

    oCode1 = union(mods, oCode0)

    [
      {:load_object_code, {lib, libVsn, oCode1}}
      | mlo(other)
    ]
  end

  defp mlo([]) do
    []
  end

  defp sort_emulator_restart(mode, before, after__) do
    {before1, after1} =
      case filter_out(
             :restart_new_emulator,
             after__
           ) do
        ^after__ ->
          {before, after__}

        a1 when mode == :up ->
          {[:restart_new_emulator | before], a1}

        a1 when mode == :dn ->
          {before, a1 ++ [:restart_emulator]}
      end

    after2 =
      case filter_out(
             :restart_emulator,
             after1
           ) do
        ^after1 ->
          after1

        a2 ->
          a2 ++ [:restart_emulator]
      end

    {before1, after2}
  end

  defp filter_out(what, list) do
    :lists.filter(
      fn
        x when x === what ->
          false

        _ ->
          true
      end,
      list
    )
  end

  defp check_syntax([h | t]) do
    check_op(h)
    check_syntax(t)
  end

  defp check_syntax([]) do
    :ok
  end

  defp check_op(:mnesia_backup) do
    throw({:error, {:not_yet_implemented, :mnesia_backup}})
  end

  defp check_op({:update, mod, change, prePurge, postPurge, mods}) do
    check_mod(mod)
    check_change(change)
    check_purge(prePurge)
    check_purge(postPurge)
    check_list(mods)

    :lists.foreach(
      fn m ->
        check_mod(m)
      end,
      mods
    )
  end

  defp check_op({:update, mod, timeout, change, prePurge, postPurge, mods}) do
    check_mod(mod)
    check_timeout(timeout)
    check_change(change)
    check_purge(prePurge)
    check_purge(postPurge)
    check_list(mods)

    :lists.foreach(
      fn m ->
        check_mod(m)
      end,
      mods
    )
  end

  defp check_op({:update, mod, modType, timeout, change, prePurge, postPurge, mods}) do
    check_mod(mod)
    check_mod_type(modType)
    check_timeout(timeout)
    check_change(change)
    check_purge(prePurge)
    check_purge(postPurge)
    check_list(mods)

    :lists.foreach(
      fn m ->
        check_mod(m)
      end,
      mods
    )
  end

  defp check_op({:load_module, mod, prePurge, postPurge, mods}) do
    check_mod(mod)
    check_purge(prePurge)
    check_purge(postPurge)
    check_list(mods)

    :lists.foreach(
      fn m ->
        check_mod(m)
      end,
      mods
    )
  end

  defp check_op({:add_module, mod}) do
    check_mod(mod)
  end

  defp check_op({:add_module, mod, mods}) do
    check_mod(mod)
    check_list(mods)

    :lists.foreach(
      fn m ->
        check_mod(m)
      end,
      mods
    )
  end

  defp check_op({:delete_module, mod}) do
    check_mod(mod)
  end

  defp check_op({:delete_module, mod, mods}) do
    check_mod(mod)
    check_list(mods)

    :lists.foreach(
      fn m ->
        check_mod(m)
      end,
      mods
    )
  end

  defp check_op({:remove_application, appl}) do
    check_appl(appl)
  end

  defp check_op({:add_application, appl, type}) do
    check_appl(appl)
    check_start_type(type)
  end

  defp check_op({:restart_application, appl}) do
    check_appl(appl)
  end

  defp check_op(:restart) do
    :ok
  end

  defp check_op(:reboot) do
    :ok
  end

  defp check_op({:load_object_code, {lib, libVsn, mods}}) do
    check_lib(lib)
    check_lib_vsn(libVsn)
    check_list(mods)

    :lists.foreach(
      fn m ->
        check_mod(m)
      end,
      mods
    )
  end

  defp check_op(:point_of_no_return) do
    :ok
  end

  defp check_op({:load, {mod, prePurge, postPurge}}) do
    check_mod(mod)
    check_purge(prePurge)
    check_purge(postPurge)
  end

  defp check_op({:remove, {mod, prePurge, postPurge}}) do
    check_mod(mod)
    check_purge(prePurge)
    check_purge(postPurge)
  end

  defp check_op({:purge, mods}) do
    check_list(mods)

    :lists.foreach(
      fn m ->
        check_mod(m)
      end,
      mods
    )
  end

  defp check_op({:suspend, mods}) do
    check_list(mods)

    :lists.foreach(
      fn
        {m, t} ->
          check_mod(m)
          check_timeout(t)

        m ->
          check_mod(m)
      end,
      mods
    )
  end

  defp check_op({:resume, mods}) do
    check_list(mods)

    :lists.foreach(
      fn m ->
        check_mod(m)
      end,
      mods
    )
  end

  defp check_op({:code_change, mods}) do
    check_list(mods)

    :lists.foreach(
      fn
        {m, _Extra} ->
          check_mod(m)

        x ->
          throw({:error, {:bad_code_change, x}})
      end,
      mods
    )
  end

  defp check_op({:code_change, mode, mods}) do
    check_list(mods)
    check_mode(mode)

    :lists.foreach(
      fn
        {m, _Extra} ->
          check_mod(m)

        x ->
          throw({:error, {:bad_code_change, x}})
      end,
      mods
    )
  end

  defp check_op({:stop, mods}) do
    check_list(mods)

    :lists.foreach(
      fn m ->
        check_mod(m)
      end,
      mods
    )
  end

  defp check_op({:start, mods}) do
    check_list(mods)

    :lists.foreach(
      fn m ->
        check_mod(m)
      end,
      mods
    )
  end

  defp check_op({:sync_nodes, _Id, {m, f, a}}) do
    check_mod(m)
    check_func(f)
    check_args(a)
  end

  defp check_op({:sync_nodes, _Id, nodes}) do
    check_list(nodes)

    :lists.foreach(
      fn node ->
        check_node(node)
      end,
      nodes
    )
  end

  defp check_op({:apply, {m, f, a}}) do
    check_mod(m)
    check_func(f)
    check_args(a)
  end

  defp check_op(:restart_new_emulator) do
    :ok
  end

  defp check_op(:restart_emulator) do
    :ok
  end

  defp check_op(x) do
    throw({:error, {:bad_instruction, x}})
  end

  defp check_mod(mod) when is_atom(mod) do
    :ok
  end

  defp check_mod(mod) do
    throw({:error, {:bad_module, mod}})
  end

  defp check_change(:soft) do
    :ok
  end

  defp check_change({:advanced, _}) do
    :ok
  end

  defp check_change(change) do
    throw({:error, {:bad_change, change}})
  end

  defp check_mod_type(:static) do
    :ok
  end

  defp check_mod_type(:dynamic) do
    :ok
  end

  defp check_mod_type(modType) do
    throw({:error, {:bad_mod_type, modType}})
  end

  defp check_purge(:soft_purge) do
    :ok
  end

  defp check_purge(:brutal_purge) do
    :ok
  end

  defp check_purge(purge) do
    throw({:error, {:bad_purge_method, purge}})
  end

  defp check_list(list) when is_list(list) do
    :ok
  end

  defp check_list(list) do
    throw({:error, {:bad_list, list}})
  end

  defp check_args(args) when is_list(args) do
    :ok
  end

  defp check_args(args) do
    throw({:error, {:bad_args_list, args}})
  end

  defp check_node(node) when is_atom(node) do
    :ok
  end

  defp check_node(node) do
    throw({:error, {:bad_node, node}})
  end

  defp check_appl(appl) when is_atom(appl) do
    :ok
  end

  defp check_appl(appl) do
    throw({:error, {:bad_application, appl}})
  end

  defp check_start_type(:none) do
    :ok
  end

  defp check_start_type(:load) do
    :ok
  end

  defp check_start_type(:temporary) do
    :ok
  end

  defp check_start_type(:transient) do
    :ok
  end

  defp check_start_type(:permanent) do
    :ok
  end

  defp check_start_type(t) do
    throw({:error, {:bad_start_type, t}})
  end

  defp check_func(func) when is_atom(func) do
    :ok
  end

  defp check_func(func) do
    throw({:error, {:bad_func, func}})
  end

  defp check_lib(lib) when is_atom(lib) do
    :ok
  end

  defp check_lib(lib) do
    throw({:error, {:bad_lib, lib}})
  end

  defp check_lib_vsn(libVsn) when is_list(libVsn) do
    :ok
  end

  defp check_lib_vsn(libVsn) do
    throw({:error, {:bad_lib_vsn, libVsn}})
  end

  defp check_timeout(:default) do
    :ok
  end

  defp check_timeout(:infinity) do
    :ok
  end

  defp check_timeout(int) when is_integer(int) and int > 0 do
    :ok
  end

  defp check_timeout(t) do
    throw({:error, {:bad_timeout, t}})
  end

  defp check_mode(:up) do
    :ok
  end

  defp check_mode(:down) do
    :ok
  end

  defp check_mode(mode) do
    throw({:error, {:bad_mode, mode}})
  end

  def format_error({:bad_op_before_point_of_no_return, instruction}) do
    :io_lib.format('Bad instruction ~p~nbefore point_of_no_return~n', [instruction])
  end

  def format_error({:no_object_code, mod}) do
    :io_lib.format('No load_object_code found for module: ~w~n', [mod])
  end

  def format_error({:suspended_not_resumed, mods}) do
    :io_lib.format('Suspended but not resumed: ~p~n', [mods])
  end

  def format_error({:resumed_not_suspended, mods}) do
    :io_lib.format('Resumed but not suspended: ~p~n', [mods])
  end

  def format_error({:code_change_not_suspended, mods}) do
    :io_lib.format('Code changed but not suspended: ~p~n', [mods])
  end

  def format_error({:start_not_stop, mods}) do
    :io_lib.format('Started but not stopped: ~p~n', [mods])
  end

  def format_error({:stop_not_start, mods}) do
    :io_lib.format('Stopped but not started: ~p~n', [mods])
  end

  def format_error({:no_such_application, app}) do
    :io_lib.format('Started undefined application: ~w~n', [app])
  end

  def format_error({:removed_application_present, app}) do
    :io_lib.format('Removed application present: ~w~n', [app])
  end

  def format_error(:dup_mnesia_backup) do
    :io_lib.format('Duplicate mnesia_backup~n', [])
  end

  def format_error(:bad_mnesia_backup) do
    :io_lib.format('mnesia_backup in bad position~n', [])
  end

  def format_error({:conflicting_versions, lib, v1, v2}) do
    :io_lib.format('Conflicting versions for ~w, ~ts and ~ts~n', [lib, v1, v2])
  end

  def format_error({:no_appl_vsn, appl}) do
    :io_lib.format('No version specified for application: ~w~n', [appl])
  end

  def format_error({:no_such_module, mod}) do
    :io_lib.format('No such module: ~w~n', [mod])
  end

  def format_error(:too_many_point_of_no_return) do
    :io_lib.format('Too many point_of_no_return~n', [])
  end

  def format_error({:bad_instruction, x}) do
    :io_lib.format('Bad instruction: ~tp~n', [x])
  end

  def format_error({:bad_module, x}) do
    :io_lib.format('Bad module: ~tp(should be atom())~n', [x])
  end

  def format_error({:bad_code_change, x}) do
    :io_lib.format('Bad code_change: ~tp(should be {Mod, Extra})~n', [x])
  end

  def format_error({:bad_change, x}) do
    :io_lib.format('Bad change spec: ~tp(should be soft | {advanced, E})~n', [x])
  end

  def format_error({:bad_mod_type, x}) do
    :io_lib.format('Bad module type: ~tp(should be static | dynamic)~n', [x])
  end

  def format_error({:bad_purge_method, x}) do
    :io_lib.format('Bad purge method: ~tp(should be soft_purge | brutal_purge)~n', [x])
  end

  def format_error({:bad_list, x}) do
    :io_lib.format('Bad list: ~tp~n', [x])
  end

  def format_error({:bad_args_list, x}) do
    :io_lib.format('Bad argument list: ~tp~n', [x])
  end

  def format_error({:bad_node, x}) do
    :io_lib.format('Bad node: ~tp(should be atom())~n', [x])
  end

  def format_error({:bad_application, x}) do
    :io_lib.format('Bad application: ~tp(should be atom())~n', [x])
  end

  def format_error({:bad_func, x}) do
    :io_lib.format('Bad function: ~tp(should be atom())~n', [x])
  end

  def format_error({:bad_lib, x}) do
    :io_lib.format('Bad library: ~tp(should be atom())~n', [x])
  end

  def format_error({:bad_lib_vsn, x}) do
    :io_lib.format('Bad library version: ~tp(should be string())~n', [x])
  end

  def format_error({:bad_timeout, x}) do
    :io_lib.format('Bad timeout: ~tp(should be infinity | int() > 0)~n', [x])
  end

  def format_error({:undef_module, mod}) do
    :io_lib.format('Undefined module: ~p~n', [mod])
  end

  def format_error({:muldef_module, mod}) do
    :io_lib.format('Multiply defined module: ~p~n', [mod])
  end

  def format_error(e) do
    :io_lib.format('~tp~n', [e])
  end

  defp filtermap(f, list) do
    :lists.zf(f, list)
  end

  defp split(fun, [h | t]) do
    {found, rest} = split(fun, t)

    case fun.(h) do
      true ->
        {[h | found], rest}

      false ->
        {found, [h | rest]}
    end
  end

  defp split(_Fun, []) do
    {[], []}
  end

  defp union([h | t], l) do
    case :lists.member(h, l) do
      true ->
        union(t, l)

      false ->
        [h | union(t, l)]
    end
  end

  defp union([], l) do
    l
  end

  defp difference([h | t], l) do
    case :lists.member(h, l) do
      true ->
        difference(t, l)

      false ->
        [h | difference(t, l)]
    end
  end

  defp difference([], _) do
    []
  end

  defp condensation(g) do
    h = :digraph.new()
    hVs = :digraph_utils.strong_components(g)

    :lists.foreach(
      fn hV ->
        :digraph.add_vertex(h, hV)
      end,
      hVs
    )

    :lists.foreach(
      fn hV1 ->
        gRs = :digraph_utils.reachable(hV1, g)

        :lists.foreach(
          fn hV2 ->
            cond do
              hV1 != hV2 ->
                case :lists.member(
                       hd(hV2),
                       gRs
                     ) do
                  true ->
                    :digraph.add_edge(h, hV1, hV2)

                  _ ->
                    :ok
                end

              true ->
                :ok
            end
          end,
          hVs
        )
      end,
      hVs
    )

    h
  end

  defp restriction(rs, g) do
    h = :digraph.new()

    :lists.foreach(
      fn r ->
        case :digraph.vertex(g, r) do
          {^r, data} ->
            :digraph.add_vertex(h, r, data)

          _ ->
            :ok
        end
      end,
      rs
    )

    gEs = :digraph.edges(g)

    :lists.foreach(
      fn gE ->
        {_, gV1, gV2, gData} = :digraph.edge(g, gE)

        case {:digraph.vertex(h, gV1), :digraph.vertex(h, gV2)} do
          {{^gV1, _}, {^gV2, _}} ->
            :digraph.add_edge(h, gE, gV1, gV2, gData)

          _ ->
            :ok
        end
      end,
      gEs
    )

    h
  end
end
