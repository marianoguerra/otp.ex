defmodule :m_ct_hooks do
  use Bitwise
  require Record

  Record.defrecord(:r_ct_hook_config, :ct_hook_config,
    id: :undefined,
    module: :undefined,
    prio: :undefined,
    scope: :undefined,
    opts: [],
    state: [],
    groups: []
  )

  def init(opts) do
    call(
      get_builtin_hooks(opts) ++
        get_new_hooks(
          opts,
          :undefined
        ),
      :ok,
      :init,
      []
    )
  end

  def groups(mod, groups) do
    info =
      try do
        :proplists.get_value(:ct_hooks, mod.suite(), [])
      catch
        _, _ ->
          [{:ct_hooks, []}]
      else
        cTHooks when is_list(cTHooks) ->
          [{:ct_hooks, cTHooks}]

        cTHook when is_atom(cTHook) ->
          [{:ct_hooks, [cTHook]}]
      end

    case call(&call_generic/3, info ++ [{:"$ct_groups", groups}], [:post_groups, mod]) do
      [{:"$ct_groups", newGroups}] ->
        newGroups

      other ->
        other
    end
  end

  def all(mod, tests) do
    info =
      try do
        :proplists.get_value(:ct_hooks, mod.suite(), [])
      catch
        _, _ ->
          [{:ct_hooks, []}]
      else
        cTHooks when is_list(cTHooks) ->
          [{:ct_hooks, cTHooks}]

        cTHook when is_atom(cTHook) ->
          [{:ct_hooks, [cTHook]}]
      end

    case call(&call_generic/3, info ++ [{:"$ct_all", tests}], [:post_all, mod]) do
      [{:"$ct_all", newTests}] ->
        newTests

      other ->
        other
    end
  end

  def terminate(hooks) do
    call(
      for r_ct_hook_config(id: hookId) <- hooks do
        {hookId, &call_terminate/3}
      end,
      :ct_hooks_terminate_dummy,
      :terminate,
      hooks
    )

    :ok
  end

  def init_tc(mod, :init_per_suite, config) do
    info =
      try do
        :proplists.get_value(:ct_hooks, mod.suite(), [])
      catch
        :error, :undef ->
          [{:ct_hooks, []}]
      else
        list when is_list(list) ->
          [{:ct_hooks, list}]

        cTHook when is_atom(cTHook) ->
          [{:ct_hooks, [cTHook]}]
      end

    call(&call_generic/3, config ++ info, [:pre_init_per_suite, mod])
  end

  def init_tc(mod, :end_per_suite, config) do
    call(&call_generic/3, config, [:pre_end_per_suite, mod])
  end

  def init_tc(mod, {:init_per_group, groupName, properties}, config) do
    maybe_start_locker(mod, groupName, properties)
    call(&call_generic_fallback/3, config, [:pre_init_per_group, mod, groupName])
  end

  def init_tc(mod, {:end_per_group, groupName, _}, config) do
    call(&call_generic_fallback/3, config, [:pre_end_per_group, mod, groupName])
  end

  def init_tc(mod, {:init_per_testcase, tC}, config) do
    call(&call_generic_fallback/3, config, [:pre_init_per_testcase, mod, tC])
  end

  def init_tc(mod, {:end_per_testcase, tC}, config) do
    call(&call_generic_fallback/3, config, [:pre_end_per_testcase, mod, tC])
  end

  def init_tc(mod, tC = :error_in_suite, config) do
    call(&call_generic_fallback/3, config, [:pre_init_per_testcase, mod, tC])
  end

  def end_tc(mod, :init_per_suite, config, _Result, return) do
    call(&call_generic/3, return, [:post_init_per_suite, mod, config], :"$ct_no_change")
  end

  def end_tc(mod, :end_per_suite, config, result, _Return) do
    call(&call_generic/3, result, [:post_end_per_suite, mod, config], :"$ct_no_change")
  end

  def end_tc(mod, {:init_per_group, groupName, _}, config, _Result, return) do
    call(
      &call_generic_fallback/3,
      return,
      [:post_init_per_group, mod, groupName, config],
      :"$ct_no_change"
    )
  end

  def end_tc(mod, {:end_per_group, groupName, properties}, config, result, _Return) do
    res =
      call(
        &call_generic_fallback/3,
        result,
        [:post_end_per_group, mod, groupName, config],
        :"$ct_no_change"
      )

    maybe_stop_locker(mod, groupName, properties)
    res
  end

  def end_tc(mod, {:init_per_testcase, tC}, config, result, _Return) do
    call(
      &call_generic_fallback/3,
      result,
      [:post_init_per_testcase, mod, tC, config],
      :"$ct_no_change"
    )
  end

  def end_tc(mod, {:end_per_testcase, tC}, config, result, _Return) do
    call(
      &call_generic_fallback/3,
      result,
      [:post_end_per_testcase, mod, tC, config],
      :"$ct_no_change"
    )
  end

  def end_tc(mod, tC = :error_in_suite, config, result, _Return) do
    call(
      &call_generic_fallback/3,
      result,
      [:post_end_per_testcase, mod, tC, config],
      :"$ct_no_change"
    )
  end

  def on_tc_skip(how, {suite, case__, reason}) do
    call(&call_cleanup/3, {how, reason}, [:on_tc_skip, suite, case__])
  end

  def on_tc_fail(_How, {suite, case__, reason}) do
    call(&call_cleanup/3, reason, [:on_tc_fail, suite, case__])
  end

  defp call_id(r_ct_hook_config(module: mod, opts: opts) = hook, config, scope) do
    id = catch_apply(mod, :id, [opts], make_ref())
    {config, r_ct_hook_config(hook, id: id, scope: scope(scope))}
  end

  defp call_init(r_ct_hook_config(module: mod, opts: opts, id: id, prio: p) = hook, config, _Meta) do
    case mod.init(id, opts) do
      {:ok, newState} when p === :undefined ->
        {config, r_ct_hook_config(hook, state: newState, prio: 0)}

      {:ok, newState} ->
        {config, r_ct_hook_config(hook, state: newState)}

      {:ok, newState, prio} when p === :undefined ->
        {config, r_ct_hook_config(hook, state: newState, prio: prio)}

      {:ok, newState, _} ->
        {config, r_ct_hook_config(hook, state: newState)}

      newState ->
        {config, r_ct_hook_config(hook, state: newState)}
    end
  end

  defp call_terminate(r_ct_hook_config(module: mod, state: state) = hook, _, _) do
    catch_apply(mod, :terminate, [state], :ok)
    {[], hook}
  end

  defp call_cleanup(r_ct_hook_config(module: mod, state: state) = hook, reason, [function | args]) do
    newState = catch_apply(mod, function, args ++ [reason, state], state, true)
    {reason, r_ct_hook_config(hook, state: newState)}
  end

  defp call_generic(hook, value, meta) do
    do_call_generic(hook, value, meta, false)
  end

  defp call_generic_fallback(hook, value, meta) do
    do_call_generic(hook, value, meta, true)
  end

  defp do_call_generic(
         r_ct_hook_config(module: mod) = hook,
         [{:"$ct_groups", groups}],
         [:post_groups | args],
         fallback
       ) do
    newGroups = catch_apply(mod, :post_groups, args ++ [groups], groups, fallback)
    {[{:"$ct_groups", newGroups}], r_ct_hook_config(hook, groups: newGroups)}
  end

  defp do_call_generic(
         r_ct_hook_config(module: mod, groups: groups) = hook,
         [{:"$ct_all", tests}],
         [:post_all | args],
         fallback
       ) do
    newTests = catch_apply(mod, :post_all, args ++ [tests, groups], tests, fallback)
    {[{:"$ct_all", newTests}], hook}
  end

  defp do_call_generic(
         r_ct_hook_config(module: mod, state: state) = hook,
         value,
         [function | args],
         fallback
       ) do
    {newValue, newState} =
      catch_apply(mod, function, args ++ [value, state], {value, state}, fallback)

    {newValue, r_ct_hook_config(hook, state: newState)}
  end

  defp call(fun, config, meta) do
    maybe_lock()
    hooks = get_hooks()

    calls =
      get_new_hooks(
        config,
        fun
      ) ++
        for r_ct_hook_config(id: hookId) <- hooks do
          {hookId, fun}
        end

    res = call(resort(calls, hooks, meta), remove(:ct_hooks, config), meta, hooks)
    maybe_unlock()
    res
  end

  defp call(fun, config, meta, noChangeRet)
       when is_function(fun) do
    case call(fun, config, meta) do
      ^config ->
        noChangeRet

      newReturn ->
        newReturn
    end
  end

  defp call([{hook, :call_id, nextFun} | rest], config, meta, hooks) do
    try do
      {^config, r_ct_hook_config(id: newId) = newHook} = call_id(hook, config, meta)

      {newHooks, newRest} =
        case :lists.keyfind(newId, r_ct_hook_config(:id), hooks) do
          false when nextFun === :undefined ->
            {hooks ++ [newHook], rest ++ [{newId, :call_init}]}

          existingHook when is_tuple(existingHook) ->
            {hooks, rest}

          _
          when hd(meta) === :post_groups or
                 hd(meta) === :post_all ->
            {hooks ++ [newHook], rest ++ [{newId, nextFun}]}

          _ ->
            {hooks ++ [newHook], rest ++ [{newId, :call_init}, {newId, nextFun}]}
        end

      call(resort(newRest, newHooks, meta), config, meta, newHooks)
    catch
      error, reason ->
        :ct_logs.log('Suite Hook', 'Failed to start a CTH: ~tp:~tp', [
          error,
          {reason, __STACKTRACE__}
        ])

        call([], {:fail, 'Failed to start CTH, see the CT Log for details'}, meta, hooks)
    end
  end

  defp call([{hookId, :call_init} | rest], config, meta, hooks) do
    call([{hookId, &call_init/3} | rest], config, meta, hooks)
  end

  defp call([{hookId, fun} | rest], config, meta, hooks) do
    try do
      hook = :lists.keyfind(hookId, r_ct_hook_config(:id), hooks)
      {newConf, newHook} = fun.(hook, config, meta)
      newCalls = get_new_hooks(newConf, fun)
      newHooks = :lists.keyreplace(hookId, r_ct_hook_config(:id), hooks, newHook)

      call(
        resort(newCalls ++ rest, newHooks, meta),
        remove(:ct_hooks, newConf),
        meta,
        terminate_if_scope_ends(hookId, meta, newHooks)
      )
    catch
      {:error_in_cth_call, reason} ->
        call(rest, {:fail, reason}, meta, terminate_if_scope_ends(hookId, meta, hooks))
    end
  end

  defp call([], config, _Meta, hooks) do
    save_suite_data_async(hooks)
    config
  end

  defp remove(key, list) when is_list(list) do
    for conf <- list,
        is_tuple(conf) === false or
          :erlang.element(
            1,
            conf
          ) !== key do
      conf
    end
  end

  defp remove(_, else__) do
    else__
  end

  defp scope([:pre_init_per_testcase, suiteName, tC | _]) do
    [:post_init_per_testcase, suiteName, tC]
  end

  defp scope([:pre_end_per_testcase, suiteName, tC | _]) do
    [:post_end_per_testcase, suiteName, tC]
  end

  defp scope([
         :pre_init_per_group,
         suiteName,
         groupName
         | _
       ]) do
    [:post_end_per_group, suiteName, groupName]
  end

  defp scope([
         :post_init_per_group,
         suiteName,
         groupName
         | _
       ]) do
    [:post_end_per_group, suiteName, groupName]
  end

  defp scope([:pre_init_per_suite, suiteName | _]) do
    [:post_end_per_suite, suiteName]
  end

  defp scope([:post_init_per_suite, suiteName | _]) do
    [:post_end_per_suite, suiteName]
  end

  defp scope([:post_groups, suiteName | _]) do
    [:post_groups, suiteName]
  end

  defp scope([:post_all, suiteName | _]) do
    [:post_all, suiteName]
  end

  defp scope(:init) do
    :none
  end

  defp strip_config([:post_init_per_testcase, suiteName, tC | _]) do
    [:post_init_per_testcase, suiteName, tC]
  end

  defp strip_config([:post_end_per_testcase, suiteName, tC | _]) do
    [:post_end_per_testcase, suiteName, tC]
  end

  defp strip_config([
         :post_init_per_group,
         suiteName,
         groupName
         | _
       ]) do
    [:post_init_per_group, suiteName, groupName]
  end

  defp strip_config([
         :post_end_per_group,
         suiteName,
         groupName
         | _
       ]) do
    [:post_end_per_group, suiteName, groupName]
  end

  defp strip_config([:post_init_per_suite, suiteName | _]) do
    [:post_init_per_suite, suiteName]
  end

  defp strip_config([:post_end_per_suite, suiteName | _]) do
    [:post_end_per_suite, suiteName]
  end

  defp strip_config(other) do
    other
  end

  defp terminate_if_scope_ends(hookId, [:on_tc_skip, suite, {:end_per_group, name}], hooks) do
    terminate_if_scope_ends(hookId, [:post_end_per_group, suite, name], hooks)
  end

  defp terminate_if_scope_ends(hookId, [:on_tc_skip, suite, :end_per_suite], hooks) do
    terminate_if_scope_ends(hookId, [:post_end_per_suite, suite], hooks)
  end

  defp terminate_if_scope_ends(hookId, function0, hooks) do
    function = strip_config(function0)

    case :lists.keyfind(hookId, r_ct_hook_config(:id), hooks) do
      r_ct_hook_config(id: ^hookId, scope: ^function) = hook ->
        case function do
          [allOrGroup, _]
          when allOrGroup === :post_all or
                 allOrGroup === :post_groups ->
            :ok

          _ ->
            terminate([hook])
        end

        :lists.keydelete(hookId, r_ct_hook_config(:id), hooks)

      _ ->
        hooks
    end
  end

  defp get_new_hooks(config, fun) do
    :lists.map(
      fn
        newHook when is_atom(newHook) ->
          {r_ct_hook_config(module: newHook), :call_id, fun}

        {newHook, opts} ->
          {r_ct_hook_config(module: newHook, opts: opts), :call_id, fun}

        {newHook, opts, prio} ->
          {r_ct_hook_config(module: newHook, opts: opts, prio: prio), :call_id, fun}
      end,
      get_new_hooks(config)
    )
  end

  defp get_new_hooks(config) when is_list(config) do
    :lists.flatmap(
      fn
        {:ct_hooks, hookConfigs}
        when is_list(hookConfigs) ->
          hookConfigs

        {:ct_hooks, hookConfig} when is_atom(hookConfig) ->
          [hookConfig]

        _ ->
          []
      end,
      config
    )
  end

  defp get_new_hooks(_Config) do
    []
  end

  defp get_builtin_hooks(opts) do
    case :proplists.get_value(
           :enable_builtin_hooks,
           opts
         ) do
      false ->
        []

      _Else ->
        for hookConf <- [r_ct_hook_config(module: :cth_log_redirect, opts: [], prio: :ctfirst)] do
          {hookConf, :call_id, :undefined}
        end
    end
  end

  defp save_suite_data_async(hooks) do
    :ct_util.save_suite_data_async(:ct_hooks, hooks)
  end

  defp get_hooks() do
    :lists.keysort(
      r_ct_hook_config(:prio),
      :ct_util.read_suite_data(:ct_hooks)
    )
  end

  defp resort(calls, hooks, [f | _R])
       when f == :pre_end_per_testcase or
              f == :post_end_per_testcase or
              f == :pre_end_per_group or f == :post_end_per_group or
              f == :pre_end_per_suite or f == :post_end_per_suite do
    :lists.reverse(resort(calls, hooks))
  end

  defp resort(calls, hooks, _Meta) do
    resort(calls, hooks)
  end

  defp resort(calls, hooks) do
    :lists.sort(
      fn
        {_, _, _}, _ ->
          true

        _, {_, _, _} ->
          false

        {_, :call_init}, _ ->
          true

        _, {_, :call_init} ->
          false

        {id1, _}, {id2, _} ->
          p1 = r_ct_hook_config(:lists.keyfind(id1, r_ct_hook_config(:id), hooks), :prio)
          p2 = r_ct_hook_config(:lists.keyfind(id2, r_ct_hook_config(:id), hooks), :prio)

          cond do
            p1 == p2 ->
              pos(id1, hooks) < pos(id2, hooks)

            p1 == :ctfirst ->
              true

            p2 == :ctfirst ->
              false

            p1 == :ctlast ->
              false

            p2 == :ctlast ->
              true

            true ->
              p1 < p2
          end
      end,
      calls
    )
  end

  defp pos(id, hooks) do
    pos(id, hooks, 0)
  end

  defp pos(id, [r_ct_hook_config(id: id) | _], num) do
    num
  end

  defp pos(id, [_ | rest], num) do
    pos(id, rest, num + 1)
  end

  defp catch_apply(m, f, a, default) do
    catch_apply(m, f, a, default, false)
  end

  defp catch_apply(m, f, a, default, fallback) do
    not :erlang.module_loaded(m) and
      try do
        m.module_info()
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    case :erlang.function_exported(m, f, length(a)) do
      false when fallback ->
        catch_apply(m, f, tl(a), default, false)

      false ->
        default

      true ->
        catch_apply(m, f, a)
    end
  end

  defp catch_apply(m, f, a) do
    try do
      :erlang.apply(m, f, a)
    catch
      _, reason ->
        :ct_logs.log('Suite Hook', 'Call to CTH failed: ~w:~tp', [
          :error,
          {reason, __STACKTRACE__}
        ])

        throw(
          {:error_in_cth_call,
           :lists.flatten(:io_lib.format('~w:~tw/~w CTH call failed', [m, f, length(a)]))}
        )
    end
  end

  defp maybe_start_locker(mod, groupName, opts) do
    case :lists.member(:parallel, opts) do
      true ->
        {:ok, _Pid} = :ct_hooks_lock.start({mod, groupName})
        :ok

      false ->
        :ok
    end
  end

  defp maybe_stop_locker(mod, groupName, opts) do
    case :lists.member(:parallel, opts) do
      true ->
        :stopped = :ct_hooks_lock.stop({mod, groupName})

      false ->
        :ok
    end
  end

  defp maybe_lock() do
    :locked = :ct_hooks_lock.request()
  end

  defp maybe_unlock() do
    :unlocked = :ct_hooks_lock.release()
  end
end
