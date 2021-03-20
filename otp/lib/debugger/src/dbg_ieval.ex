defmodule :m_dbg_ieval do
  use Bitwise
  require Record
  Record.defrecord(:r_ieval, :ieval, level: 1, line: - 1,
                                 module: :undefined, function: :undefined,
                                 arguments: :undefined, top: false)
  def eval(mod, func, args) do
    debugged = self()
    int = :dbg_iserver.find()
    case (:dbg_iserver.call(int, {:get_meta, debugged})) do
      {:ok, meta} ->
        send(meta, {:re_entry, debugged,
                      {:eval, {mod, func, args}}})
        meta
      {:error, :not_interpreted} ->
        spawn(fn () ->
                   meta(int, debugged, mod, func, args)
              end)
    end
  end

  def exit_info(int, attPid, origPid, reason, exitInfo) do
    :erlang.put(:int, int)
    :erlang.put(:attached, attPid)
    :erlang.put(:breakpoints,
                  :dbg_iserver.call(int, :all_breaks))
    :erlang.put(:self, origPid)
    :erlang.put(:exit_info, exitInfo)
    case (exitInfo) do
      {{mod, line}, bs, s} ->
        :dbg_istk.from_external(s)
        le = :dbg_istk.stack_level()
        :dbg_icmd.tell_attached({:exit_at, {mod, line}, reason,
                                   le})
        exit_loop(origPid, reason, bs,
                    r_ieval(module: mod, line: line))
      {} ->
        :dbg_istk.init()
        :dbg_icmd.tell_attached({:exit_at, :null, reason, 1})
        exit_loop(origPid, reason, :erl_eval.new_bindings(),
                    r_ieval())
    end
  end

  def eval_expr(expr, bs, ieval) do
    exitInfo = :erlang.get(:exit_info)
    stacktrace = :erlang.get(:stacktrace)
    try do
      debugged_cmd({:eval, expr, bs}, bs, ieval)
    catch
      class, reason ->
        result = (case (class) do
                    :throw ->
                      reason
                    _ ->
                      {:EXIT, reason}
                  end)
        :erlang.put(:exit_info, exitInfo)
        :erlang.put(:stacktrace, stacktrace)
        {:value, result, bs}
    end
  end

  def check_exit_msg({:EXIT, int, reason}, _Bs, r_ieval(level: le)) do
    cond do
      le === 1 ->
        exit(reason)
      le > 1 ->
        exit({int, reason})
    end
  end

  def check_exit_msg({:DOWN, _, _, _, reason}, bs,
           r_ieval(level: le, module: mod, line: li)) do
    exitInfo = (case (:erlang.get(:exit_info)) do
                  :undefined when le === 1 ->
                    {}
                  :undefined when le > 1 ->
                    stackExternal = (:dbg_istk.delayed_to_external()).()
                    {{mod, li}, bs, stackExternal}
                  exitInfo0 when is_function(exitInfo0, 0) ->
                    exitInfo0.()
                end)
    :dbg_iserver.cast(:erlang.get(:int),
                        {:set_exit_info, self(), exitInfo})
    cond do
      le === 1 ->
        exit(reason)
      le > 1 ->
        exit({:erlang.get(:self), reason})
    end
  end

  def check_exit_msg(_Msg, _Bs, _Ieval) do
    :ignore
  end

  def exception(class, reason, bs, ieval) do
    exception(class, reason, bs, ieval, false)
  end

  defp exception(class, reason, bs, ieval, false) do
    do_exception(class, reason,
                   :dbg_istk.delayed_stacktrace(:no_args, ieval), bs,
                   ieval)
  end

  defp exception(class, reason, bs, ieval, true) do
    do_exception(class, reason,
                   :dbg_istk.delayed_stacktrace(:include_args, ieval), bs,
                   ieval)
  end

  defp do_exception(class, reason, stacktrace, bs,
            r_ieval(module: m, line: line)) do
    stackFun = :dbg_istk.delayed_to_external()
    exitInfo = fn () ->
                    {{m, line}, bs, stackFun.()}
               end
    :erlang.put(:exit_info, exitInfo)
    :erlang.put(:stacktrace, stacktrace)
    apply(:erlang, class, [reason])
  end

  defp meta(int, debugged, m, f, as) do
    :erlang.process_flag(:trap_exit, true)
    :erlang.monitor(:process, debugged)
    pargs = (case ({m, f}) do
               {:dbg_ieval, evalFun} when evalFun === :eval_fun or
                                            evalFun === :eval_named_fun
                                          ->
                 {mx, fx} = :lists.last(as)
                 {mx, fx, :lists.nth(2, as)}
               _ ->
                 {m, f, as}
             end)
    status = :dbg_iserver.call(int,
                                 {:new_process, debugged, self(), pargs})
    :erlang.put(:int, int)
    :erlang.put(:attached, :undefined)
    :erlang.put(:breakpoints,
                  :dbg_iserver.call(int, :all_breaks))
    :erlang.put(:cache, [])
    :erlang.put(:next_break, status)
    :erlang.put(:self, debugged)
    :dbg_istk.init()
    :erlang.put(:stacktrace, [])
    :erlang.put(:trace_stack,
                  :dbg_iserver.call(int, :get_stack_trace))
    :erlang.put(:trace, false)
    :erlang.put(:user_eval, [])
    ieval = r_ieval()
    send(debugged, {:sys, self(),
                      eval_mfa(debugged, m, f, as, ieval)})
    :dbg_iserver.cast(int, {:set_status, self(), :idle, {}})
    :dbg_icmd.tell_attached(:idle)
    meta_loop(debugged, :erl_eval.new_bindings(), ieval)
  end

  defp debugged_cmd(cmd, bs, ieval) do
    debugged = :erlang.get(:self)
    send(debugged, {:sys, self(), {:command, cmd}})
    meta_loop(debugged, bs, ieval)
  end

  defp meta_loop(debugged, bs, r_ieval(level: le) = ieval) do
    receive do
      {:sys, ^debugged, {:value, val}} ->
        {:value, val, bs}
      {:sys, ^debugged, {:value, val, bs2}} ->
        {:value, val, merge_bindings(bs2, bs, ieval)}
      {:sys, ^debugged, {:exception, {class, reason, stk}}} ->
        case (:erlang.get(:exit_info)) do
          :undefined ->
            makeStk0 = :dbg_istk.delayed_stacktrace()
            makeStk = fn depth0 ->
                           depth = max(0, depth0 - length(stk))
                           stk ++ makeStk0.(depth)
                      end
            do_exception(class, reason, makeStk, bs, ieval)
          _ ->
            apply(:erlang, class, [reason])
        end
      {:re_entry, ^debugged, {:eval, {m, f, as}}}
          when le === 1 ->
        :dbg_istk.init()
        :erlang.put(:stacktrace, [])
        :erlang.put(:exit_info, :undefined)
        :dbg_iserver.cast(:erlang.get(:int),
                            {:set_status, self(), :running, {}})
        :dbg_icmd.tell_attached(:running)
        :dbg_icmd.tell_attached({:re_entry, m, f})
        send(debugged, {:sys, self(),
                          eval_mfa(debugged, m, f, as, ieval)})
        :dbg_iserver.cast(:erlang.get(:int),
                            {:set_status, self(), :idle, {}})
        :dbg_icmd.tell_attached(:idle)
        meta_loop(debugged, bs, ieval)
      {:re_entry, ^debugged, {:eval, {m, f, as}}} when le > 1
                                                       ->
        ieval2 = r_ieval(ieval, module: :undefined,  line: - 1)
        send(debugged, {:sys, self(),
                          eval_mfa(debugged, m, f, as, ieval2)})
        meta_loop(debugged, bs, ieval)
      msg ->
        check_exit_msg(msg, bs, ieval)
        :dbg_icmd.handle_msg(msg, :idle, bs, ieval)
        meta_loop(debugged, bs, ieval)
    end
  end

  defp exit_loop(origPid, reason, bs, ieval) do
    receive do
      msg ->
        check_exit_msg(msg, bs, ieval)
        :dbg_icmd.handle_msg(msg, :exit_at, bs, ieval)
        exit_loop(origPid, reason, bs, ieval)
    end
  end

  defp trace(what, args) do
    trace(what, args, :erlang.get(:trace))
  end

  defp trace(:return, {_Le, {:dbg_apply, _, _, _}}, _Bool) do
    :ignore
  end

  defp trace(what, args, true) do
    fun = fn p ->
               format_trace(what, args, p)
          end
    :dbg_icmd.tell_attached({:trace_output, fun})
  end

  defp trace(_What, _Args, false) do
    :ignore
  end

  defp format_trace(what, args, p) do
    case (what) do
      :send ->
        {to, msg} = args
        :io_lib.format('==> ~w : ' ++ p ++ '~n', [to, msg])
      :receivex ->
        {le, timeoutP} = args
        tail = (case (timeoutP) do
                  true ->
                    'with timeout~n'
                  false ->
                    '~n'
                end)
        :io_lib.format('   (~w) receive ' ++ tail, [le])
      :received when args === :null ->
        :io_lib.format('~n', [])
      :received ->
        :io_lib.format('~n<== ' ++ p ++ '~n', [args])
      :call ->
        {called, {le, li, m, f, as}} = args
        case (called) do
          :extern ->
            :io_lib.format('++ (~w) <~w> ~w:~tw~ts~n', [le, li, m, f, format_args(as, p)])
          :local ->
            :io_lib.format('++ (~w) <~w> ~tw~ts~n', [le, li, f, format_args(as, p)])
        end
      :call_fun ->
        {le, li, f, as} = args
        :io_lib.format('++ (~w) <~w> ~tw~ts~n', [le, li, f, format_args(as, p)])
      :return ->
        {le, val} = args
        :io_lib.format('-- (~w) ' ++ p ++ '~n', [le, val])
      :bif ->
        {le, li, m, f, as} = args
        :io_lib.format('++ (~w) <~w> ~w:~tw~ts~n', [le, li, m, f, format_args(as, p)])
    end
  end

  defp format_args(as, p) when is_list(as) do
    [?(, format_args1(as, p), ?)]
  end

  defp format_args(a, p) do
    [?/, :io_lib.format(p, [a])]
  end

  defp format_args1([a], p) do
    [:io_lib.format(p, [a])]
  end

  defp format_args1([a | as], p) do
    [:io_lib.format(p, [a]), ?, | format_args1(as, p)]
  end

  defp format_args1([], _) do
    []
  end

  defp catch_value(:error, reason) do
    {:EXIT, {reason, get_stacktrace()}}
  end

  defp catch_value(:exit, reason) do
    {:EXIT, reason}
  end

  defp catch_value(:throw, reason) do
    reason
  end

  defp eval_mfa(debugged, m, f, as, r_ieval(level: le) = ieval0) do
    int = :erlang.get(:int)
    bs = :erl_eval.new_bindings()
    ieval = r_ieval(ieval0, level: le + 1,  top: true)
    try do
      do_eval_function(m, f, as, bs, :extern, ieval)
    catch
      :exit, {^debugged, reason} ->
        exit(reason)
      :exit, {^int, reason} ->
        exit(reason)
      class, reason ->
        {:exception, {class, reason, get_stacktrace()}}
    else
      {:value, val, _Bs} ->
        trace(:return, {le, val})
        {:ready, val}
    end
  end

  defp eval_function(mod, name, as, bs, called, ieval0, lc) do
    tail = lc and :erlang.get(:trace_stack) === :no_tail
    case (tail) do
      false ->
        ieval = :dbg_istk.push(bs, ieval0, lc)
        {:value, val, _} = do_eval_function(mod, name, as, bs,
                                              called, ieval)
        :dbg_istk.pop()
        trace(:return, {r_ieval(ieval, :level), val})
        {:value, val, bs}
      true ->
        do_eval_function(mod, name, as, bs, called, ieval0)
    end
  end

  defp do_eval_function(mod, fun, as0, bs0, _, ieval0)
      when is_function(fun) or
             (mod === :dbg_ieval and
                fun === :eval_fun or fun === :eval_named_fun) do
    r_ieval(level: le, line: li, top: top) = ieval0
    case (lambda(fun, as0)) do
      {[{:clause, fc, _, _, _} | _] = cs, module, name, as,
         bs} ->
        ieval = r_ieval(ieval0, module: module,  function: name, 
                            arguments: as0,  line: fc)
        trace(:call_fun, {le, li, name, as})
        fnk_clauses(cs, as, bs, ieval)
      :not_interpreted when top ->
        trace(:call_fun, {le, li, fun, as0})
        {:value, {:dbg_apply, :erlang, :apply, [fun, as0]}, bs0}
      :not_interpreted ->
        trace(:call_fun, {le, li, fun, as0})
        debugged_cmd({:apply, :erlang, :apply, [fun, as0]}, bs0,
                       ieval0)
      {:error, reason} ->
        exception(:error, reason, bs0, ieval0)
    end
  end

  defp do_eval_function(mod, name, as0, bs0, called, ieval0) do
    r_ieval(level: le, line: li, top: top) = ieval0
    trace(:call, {called, {le, li, mod, name, as0}})
    ieval = r_ieval(ieval0, module: mod,  function: name, 
                        arguments: as0)
    case (get_function(mod, name, as0, called)) do
      [{:clause, fcLine, _, _, _} | _] = cs ->
        fnk_clauses(cs, as0, :erl_eval.new_bindings(),
                      r_ieval(ieval, line: fcLine))
      :not_interpreted when top ->
        {:value, {:dbg_apply, mod, name, as0}, bs0}
      :not_interpreted ->
        debugged_cmd({:apply, mod, name, as0}, bs0, ieval)
      :undef ->
        exception(:error, :undef, bs0, ieval, true)
    end
  end

  defp lambda(:eval_fun, [cs, as, bs, {mod, name} = f]) do
    cond do
      length(:erlang.element(3, hd(cs))) === length(as) ->
        db_ref(mod)
        {cs, mod, name, as, bs}
      true ->
        {:error, {:badarity, {f, as}}}
    end
  end

  defp lambda(:eval_named_fun,
            [cs, as, bs0, fName, rF, {mod, name} = f]) do
    cond do
      length(:erlang.element(3, hd(cs))) === length(as) ->
        db_ref(mod)
        bs1 = add_binding(fName, rF, bs0)
        {cs, mod, name, as, bs1}
      true ->
        {:error, {:badarity, {f, as}}}
    end
  end

  defp lambda(fun, as) when is_function(fun) do
    case (:erlang.fun_info(fun, :module)) do
      {:module, :dbg_ieval} ->
        {mod, name, bs, cs} = (case (:erlang.fun_info(fun,
                                                        :env)) do
                                 {:env, [{{m, f}, bs0, cs0}]} ->
                                   {m, f, bs0, cs0}
                                 {:env, [{{m, f}, bs0, cs0, fName}]} ->
                                   {m, f, add_binding(fName, fun, bs0), cs0}
                               end)
        {:arity, arity} = :erlang.fun_info(fun, :arity)
        cond do
          length(as) === arity ->
            db_ref(mod)
            {cs, mod, name, as, bs}
          true ->
            {:error, {:badarity, {fun, as}}}
        end
      _ ->
        :not_interpreted
    end
  end

  defp get_function(mod, name, args, :local) do
    arity = length(args)
    key = {mod, name, arity}
    case (cached(key)) do
      false ->
        dbRef = db_ref(mod)
        case (:dbg_idb.match_object(dbRef,
                                      {{mod, name, arity, :_}, :_})) do
          [{{^mod, ^name, ^arity, exp}, clauses}] ->
            cache(key, {exp, clauses})
            clauses
          _ ->
            :undef
        end
      {_Exp, cs} ->
        cs
    end
  end

  defp get_function(mod, name, args, :extern) do
    arity = length(args)
    key = {mod, name, arity}
    case (cached(key)) do
      false ->
        case (db_ref(mod)) do
          :not_found ->
            :not_interpreted
          dbRef ->
            case (:dbg_idb.lookup(dbRef,
                                    {mod, name, arity, true})) do
              {:ok, data} ->
                cache(key, {true, data})
                data
              :not_found ->
                case (:dbg_idb.lookup(dbRef, :module)) do
                  {:ok, _} ->
                    :undef
                  :not_found ->
                    :not_interpreted
                end
            end
        end
      {true, cs} ->
        cs
      {false, _} ->
        :undef
    end
  end

  defp db_ref(mod) do
    case (:erlang.get({mod, :db})) do
      :undefined ->
        case (:dbg_iserver.call(:erlang.get(:int),
                                  {:get_module_db, mod, :erlang.get(:self)})) do
          :not_found ->
            :not_found
          modDb ->
            node = node(:erlang.get(:int))
            dbRef = (cond do
                       node !== node() ->
                         {node, modDb}
                       true ->
                         modDb
                     end)
            :erlang.put({mod, :db}, dbRef)
            dbRef
        end
      dbRef ->
        dbRef
    end
  end

  defp cache(key, data) do
    :erlang.put(:cache,
                  :lists.sublist([{key, data} | :erlang.get(:cache)], 5))
  end

  defp cached(key) do
    case (:lists.keyfind(key, 1, :erlang.get(:cache))) do
      {^key, data} ->
        data
      false ->
        false
    end
  end

  defp fnk_clauses([{:clause, line, pars, gs, body} | cs], as, bs0,
            ieval) do
    case (head_match(pars, as, [], bs0)) do
      {:match, bs1} ->
        bs = add_bindings(bs1, bs0)
        case (guard(gs, bs)) do
          true ->
            seq(body, bs, r_ieval(ieval, line: line))
          false ->
            fnk_clauses(cs, as, bs0, ieval)
        end
      :nomatch ->
        fnk_clauses(cs, as, bs0, ieval)
    end
  end

  defp fnk_clauses([], _As, bs, ieval) do
    exception(:error, :function_clause, bs, ieval, true)
  end

  defp seq([e], bs0, ieval) do
    case (:dbg_icmd.cmd(e, bs0, ieval)) do
      {:skip, bs} ->
        {:value, :skipped, bs}
      bs ->
        expr(e, bs, ieval)
    end
  end

  defp seq([e | es], bs0, ieval) do
    case (:dbg_icmd.cmd(e, bs0, ieval)) do
      {:skip, bs} ->
        seq(es, bs, ieval)
      bs1 ->
        {:value, _, bs} = expr(e, bs1, r_ieval(ieval, top: false))
        seq(es, bs, ieval)
    end
  end

  defp seq([], bs, _) do
    {:value, true, bs}
  end

  defp expr({:var, line, v}, bs, ieval) do
    case (binding(v, bs)) do
      {:value, val} ->
        {:value, val, bs}
      :unbound ->
        exception(:error, {:unbound, v}, bs,
                    r_ieval(ieval, line: line))
    end
  end

  defp expr({:value, _, val}, bs, _Ieval) do
    {:value, val, bs}
  end

  defp expr({:value, val}, bs, _Ieval) do
    {:value, val, bs}
  end

  defp expr({:cons, line, h0, t0}, bs0, ieval0) do
    ieval = r_ieval(ieval0, line: line,  top: false)
    {:value, h, bs1} = expr(h0, bs0, ieval)
    {:value, t, bs2} = expr(t0, bs0, ieval)
    {:value, [h | t], merge_bindings(bs2, bs1, ieval)}
  end

  defp expr({:tuple, line, es0}, bs0, ieval) do
    {vs, bs} = eval_list(es0, bs0, r_ieval(ieval, line: line))
    {:value, :erlang.list_to_tuple(vs), bs}
  end

  defp expr({:map, line, fs}, bs0, ieval) do
    {map, bs} = eval_new_map_fields(fs, bs0,
                                      r_ieval(ieval, line: line,  top: false),
                                      &expr/3)
    {:value, map, bs}
  end

  defp expr({:map, line, e0, fs0}, bs0, ieval0) do
    ieval = r_ieval(ieval0, line: line,  top: false)
    {:value, e, bs1} = expr(e0, bs0, ieval)
    {fs, bs2} = eval_map_fields(fs0, bs0, ieval)
    _ = :maps.put(:k, :v, e)
    value = :lists.foldl(fn {:map_assoc, k, v}, mi ->
                              :maps.put(k, v, mi)
                            {:map_exact, k, v}, mi ->
                              :maps.update(k, v, mi)
                         end,
                           e, fs)
    {:value, value, merge_bindings(bs2, bs1, ieval)}
  end

  defp expr({:block, line, es}, bs, ieval) do
    seq(es, bs, r_ieval(ieval, line: line))
  end

  defp expr({:catch, line, expr}, bs0, ieval) do
    try do
      expr(expr, bs0, r_ieval(ieval, line: line,  top: false))
    catch
      class, reason ->
        :erlang.put(:exit_info, :undefined)
        :dbg_istk.pop(r_ieval(ieval, :level))
        value = catch_value(class, reason)
        trace(:return, {r_ieval(ieval, :level), value})
        {:value, value, bs0}
    end
  end

  defp expr({:try, line, es, caseCs, catchCs, []}, bs0,
            ieval0) do
    ieval = r_ieval(ieval0, line: line)
    try do
      seq(es, bs0, r_ieval(ieval, top: false))
    catch
      class, reason when catchCs !== [] ->
        catch_clauses({class, reason, get_stacktrace()},
                        catchCs, bs0, ieval)
    else
      {:value, val, bs} = value ->
        case (caseCs) do
          [] ->
            value
          _ ->
            case_clauses(val, caseCs, bs, :try_clause, ieval)
        end
    end
  end

  defp expr({:try, line, es, caseCs, catchCs, as}, bs0,
            ieval0) do
    ieval = r_ieval(ieval0, line: line)
    try do
      seq(es, bs0, r_ieval(ieval, top: false))
    catch
      class, reason when catchCs !== [] ->
        catch_clauses({class, reason, get_stacktrace()},
                        catchCs, bs0, ieval)
    else
      {:value, val, bs} = value ->
        case (caseCs) do
          [] ->
            value
          _ ->
            case_clauses(val, caseCs, bs, :try_clause, ieval)
        end
    after
      seq(as, bs0, r_ieval(ieval, top: false))
    end
  end

  defp expr({:case, line, e, cs}, bs0, ieval) do
    {:value, val, bs} = expr(e, bs0,
                               r_ieval(ieval, line: line,  top: false))
    case_clauses(val, cs, bs, :case_clause,
                   r_ieval(ieval, line: line))
  end

  defp expr({:if, line, cs}, bs, ieval) do
    if_clauses(cs, bs, r_ieval(ieval, line: line))
  end

  defp expr({:andalso, line, e1, e2}, bs0, ieval) do
    case (expr(e1, bs0,
                 r_ieval(ieval, line: line,  top: false))) do
      {:value, false, _} = res ->
        res
      {:value, true, bs} ->
        {:value, val, _} = expr(e2, bs,
                                  r_ieval(ieval, line: line,  top: false))
        {:value, val, bs}
      {:value, val, bs} ->
        exception(:error, {:badarg, val}, bs, ieval)
    end
  end

  defp expr({:orelse, line, e1, e2}, bs0, ieval) do
    case (expr(e1, bs0,
                 r_ieval(ieval, line: line,  top: false))) do
      {:value, true, _} = res ->
        res
      {:value, false, bs} ->
        {:value, val, _} = expr(e2, bs,
                                  r_ieval(ieval, line: line,  top: false))
        {:value, val, bs}
      {:value, val, bs} ->
        exception(:error, {:badarg, val}, bs, ieval)
    end
  end

  defp expr({:match, line, lhs, rhs0}, bs0, ieval0) do
    ieval = r_ieval(ieval0, line: line)
    {:value, rhs, bs1} = expr(rhs0, bs0,
                                r_ieval(ieval, top: false))
    case (match(lhs, rhs, bs1)) do
      {:match, bs} ->
        {:value, rhs, bs}
      :nomatch ->
        exception(:error, {:badmatch, rhs}, bs1, ieval)
    end
  end

  defp expr({:make_fun, line, name, cs}, bs,
            r_ieval(module: module) = ieval) do
    arity = length(:erlang.element(3, hd(cs)))
    info = {{module, name}, bs, cs}
    fun = (case (arity) do
             0 ->
               fn () ->
                    eval_fun([], info)
               end
             1 ->
               fn a ->
                    eval_fun([a], info)
               end
             2 ->
               fn a, b ->
                    eval_fun([a, b], info)
               end
             3 ->
               fn a, b, c ->
                    eval_fun([a, b, c], info)
               end
             4 ->
               fn a, b, c, d ->
                    eval_fun([a, b, c, d], info)
               end
             5 ->
               fn a, b, c, d, e ->
                    eval_fun([a, b, c, d, e], info)
               end
             6 ->
               fn a, b, c, d, e, f ->
                    eval_fun([a, b, c, d, e, f], info)
               end
             7 ->
               fn a, b, c, d, e, f, g ->
                    eval_fun([a, b, c, d, e, f, g], info)
               end
             8 ->
               fn a, b, c, d, e, f, g, h ->
                    eval_fun([a, b, c, d, e, f, g, h], info)
               end
             9 ->
               fn a, b, c, d, e, f, g, h, i ->
                    eval_fun([a, b, c, d, e, f, g, h, i], info)
               end
             10 ->
               fn a, b, c, d, e, f, g, h, i, j ->
                    eval_fun([a, b, c, d, e, f, g, h, i, j], info)
               end
             11 ->
               fn a, b, c, d, e, f, g, h, i, j, k ->
                    eval_fun([a, b, c, d, e, f, g, h, i, j, k], info)
               end
             12 ->
               fn a, b, c, d, e, f, g, h, i, j, k, l ->
                    eval_fun([a, b, c, d, e, f, g, h, i, j, k, l], info)
               end
             13 ->
               fn a, b, c, d, e, f, g, h, i, j, k, l, m ->
                    eval_fun([a, b, c, d, e, f, g, h, i, j, k, l, m], info)
               end
             14 ->
               fn a, b, c, d, e, f, g, h, i, j, k, l, m, n ->
                    eval_fun([a, b, c, d, e, f, g, h, i, j, k, l, m, n],
                               info)
               end
             15 ->
               fn a, b, c, d, e, f, g, h, i, j, k, l, m, n, o ->
                    eval_fun([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o],
                               info)
               end
             16 ->
               fn a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p ->
                    eval_fun([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o,
                                                                            p],
                               info)
               end
             17 ->
               fn a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q ->
                    eval_fun([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o,
                                                                            p,
                                                                                q],
                               info)
               end
             18 ->
               fn a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q,
                    r ->
                    eval_fun([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o,
                                                                            p,
                                                                                q,
                                                                                    r],
                               info)
               end
             19 ->
               fn a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r,
                    s ->
                    eval_fun([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o,
                                                                            p,
                                                                                q,
                                                                                    r,
                                                                                        s],
                               info)
               end
             20 ->
               fn a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r,
                    s, t ->
                    eval_fun([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o,
                                                                            p,
                                                                                q,
                                                                                    r,
                                                                                        s,
                                                                                            t],
                               info)
               end
             _Other ->
               exception(:error, {:argument_limit, {:fun, cs}}, bs,
                           r_ieval(ieval, line: line))
           end)
    {:value, fun, bs}
  end

  defp expr({:make_named_fun, line, name, fName, cs}, bs,
            r_ieval(module: module) = ieval) do
    arity = length(:erlang.element(3, hd(cs)))
    info = {{module, name}, bs, cs, fName}
    fun = (case (arity) do
             0 ->
               fn rF
                ->
                 eval_named_fun([], rF, info)
               end
             1 ->
               fn rF
               a ->
                 eval_named_fun([a], rF, info)
               end
             2 ->
               fn rF
               a, b ->
                 eval_named_fun([a, b], rF, info)
               end
             3 ->
               fn rF
               a, b, c ->
                 eval_named_fun([a, b, c], rF, info)
               end
             4 ->
               fn rF
               a, b, c, d ->
                 eval_named_fun([a, b, c, d], rF, info)
               end
             5 ->
               fn rF
               a, b, c, d, e ->
                 eval_named_fun([a, b, c, d, e], rF, info)
               end
             6 ->
               fn rF
               a, b, c, d, e, f ->
                 eval_named_fun([a, b, c, d, e, f], rF, info)
               end
             7 ->
               fn rF
               a, b, c, d, e, f, g ->
                 eval_named_fun([a, b, c, d, e, f, g], rF, info)
               end
             8 ->
               fn rF
               a, b, c, d, e, f, g, h ->
                 eval_named_fun([a, b, c, d, e, f, g, h], rF, info)
               end
             9 ->
               fn rF
               a, b, c, d, e, f, g, h, i ->
                 eval_named_fun([a, b, c, d, e, f, g, h, i], rF, info)
               end
             10 ->
               fn rF
               a, b, c, d, e, f, g, h, i, j ->
                 eval_named_fun([a, b, c, d, e, f, g, h, i, j], rF, info)
               end
             11 ->
               fn rF
               a, b, c, d, e, f, g, h, i, j, k ->
                 eval_named_fun([a, b, c, d, e, f, g, h, i, j, k], rF,
                                  info)
               end
             12 ->
               fn rF
               a, b, c, d, e, f, g, h, i, j, k, l ->
                 eval_named_fun([a, b, c, d, e, f, g, h, i, j, k, l], rF,
                                  info)
               end
             13 ->
               fn rF
               a, b, c, d, e, f, g, h, i, j, k, l, m ->
                 eval_named_fun([a, b, c, d, e, f, g, h, i, j, k, l, m],
                                  rF, info)
               end
             14 ->
               fn rF
               a, b, c, d, e, f, g, h, i, j, k, l, m, n ->
                 eval_named_fun([a, b, c, d, e, f, g, h, i, j, k, l, m,
                                                                         n],
                                  rF, info)
               end
             15 ->
               fn rF
               a, b, c, d, e, f, g, h, i, j, k, l, m, n, o ->
                 eval_named_fun([a, b, c, d, e, f, g, h, i, j, k, l, m,
                                                                         n, o],
                                  rF, info)
               end
             16 ->
               fn rF
               a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p ->
                 eval_named_fun([a, b, c, d, e, f, g, h, i, j, k, l, m,
                                                                         n, o,
                                                                                p],
                                  rF, info)
               end
             17 ->
               fn rF
               a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q ->
                 eval_named_fun([a, b, c, d, e, f, g, h, i, j, k, l, m,
                                                                         n, o,
                                                                                p,
                                                                                    q],
                                  rF, info)
               end
             18 ->
               fn rF
               a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r ->
                 eval_named_fun([a, b, c, d, e, f, g, h, i, j, k, l, m,
                                                                         n, o,
                                                                                p,
                                                                                    q,
                                                                                        r],
                                  rF, info)
               end
             19 ->
               fn rF
               a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r,
                 s ->
                 eval_named_fun([a, b, c, d, e, f, g, h, i, j, k, l, m,
                                                                         n, o,
                                                                                p,
                                                                                    q,
                                                                                        r,
                                                                                            s],
                                  rF, info)
               end
             20 ->
               fn rF
               a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s,
                 t ->
                 eval_named_fun([a, b, c, d, e, f, g, h, i, j, k, l, m,
                                                                         n, o,
                                                                                p,
                                                                                    q,
                                                                                        r,
                                                                                            s,
                                                                                                t],
                                  rF, info)
               end
             _Other ->
               exception(:error,
                           {:argument_limit, {:named_fun, fName, cs}}, bs,
                           r_ieval(ieval, line: line))
           end)
    {:value, fun, bs}
  end

  defp expr({:make_ext_fun, line, mFA0}, bs0, ieval0) do
    {[m, f, a], bs} = eval_list(mFA0, bs0, ieval0)
    try do
      :erlang.make_fun(m, f, a)
    catch
      :error, :badarg ->
        ieval1 = r_ieval(ieval0, line: line)
        ieval2 = :dbg_istk.push(bs0, ieval1, false)
        ieval = r_ieval(ieval2, module: :erlang, 
                            function: :make_fun,  arguments: [m, f, a], 
                            line: - 1)
        exception(:error, :badarg, bs, ieval, true)
    else
      value ->
        {:value, value, bs}
    end
  end

  defp expr({:local_call, line, f, as0, lc}, bs0,
            r_ieval(module: m) = ieval0) do
    ieval = r_ieval(ieval0, line: line)
    {as, bs} = eval_list(as0, bs0, ieval)
    eval_function(m, f, as, bs, :local, ieval, lc)
  end

  defp expr({:call_remote, line, m, f, as0, lc}, bs0,
            ieval0) do
    ieval = r_ieval(ieval0, line: line)
    {as, bs} = eval_list(as0, bs0, ieval)
    eval_function(m, f, as, bs, :extern, ieval, lc)
  end

  defp expr({:dbg, line, :self, []}, bs, r_ieval(level: le)) do
    trace(:bif, {le, line, :erlang, :self, []})
    self = :erlang.get(:self)
    trace(:return, {le, self})
    {:value, self, bs}
  end

  defp expr({:dbg, line, :raise, as0}, bs0,
            r_ieval(level: le) = ieval0) do
    ieval = r_ieval(ieval0, line: line)
    {[class, reason, stk0] = as, bs} = eval_list(as0, bs0,
                                                   ieval)
    trace(:bif, {le, line, :erlang, :raise, as})
    try do
      error = :erlang.raise(class, reason, stk0)
      trace(:return, {le, error})
      {:value, error, bs}
    catch
      _, _ ->
        stkFun = fn _ ->
                      __STACKTRACE__
                 end
        do_exception(class, reason, stkFun, bs, ieval)
    end
  end

  defp expr({:dbg, line, :throw, as0}, bs0,
            r_ieval(level: le) = ieval0) do
    ieval = r_ieval(ieval0, line: line)
    {[term], bs} = eval_list(as0, bs0, ieval)
    trace(:bif, {le, line, :erlang, :throw, [term]})
    exception(:throw, term, bs, ieval)
  end

  defp expr({:dbg, line, :error, as0}, bs0,
            r_ieval(level: le) = ieval0) do
    ieval = r_ieval(ieval0, line: line)
    {[term], bs} = eval_list(as0, bs0, ieval)
    trace(:bif, {le, line, :erlang, :error, [term]})
    exception(:error, term, bs, ieval)
  end

  defp expr({:dbg, line, :exit, as0}, bs0,
            r_ieval(level: le) = ieval0) do
    ieval = r_ieval(ieval0, line: line)
    {[term], bs} = eval_list(as0, bs0, ieval)
    trace(:bif, {le, line, :erlang, :exit, [term]})
    exception(:exit, term, bs, ieval)
  end

  defp expr({:safe_bif, line, m, f, as0}, bs0,
            r_ieval(level: le) = ieval0) do
    ieval1 = r_ieval(ieval0, line: line)
    {as, bs} = eval_list(as0, bs0, ieval1)
    trace(:bif, {le, line, m, f, as})
    ieval2 = :dbg_istk.push(bs0, ieval1, false)
    ieval = r_ieval(ieval2, module: m,  function: f, 
                        arguments: as,  line: - 1)
    {_, value, _} = (res = safe_bif(m, f, as, bs, ieval))
    trace(:return, {le, value})
    :dbg_istk.pop()
    res
  end

  defp expr({:bif, line, m, f, as0}, bs0,
            r_ieval(level: le) = ieval0) do
    ieval1 = r_ieval(ieval0, line: line)
    {as, bs} = eval_list(as0, bs0, ieval1)
    trace(:bif, {le, line, m, f, as})
    ieval2 = :dbg_istk.push(bs0, ieval1, false)
    ieval = r_ieval(ieval2, module: m,  function: f, 
                        arguments: as,  line: - 1)
    {_, value, _} = (res = debugged_cmd({:apply, m, f, as},
                                          bs, ieval))
    trace(:return, {le, value})
    :dbg_istk.pop()
    res
  end

  defp expr({:op, line, op, as0}, bs0, ieval0) do
    ieval = r_ieval(ieval0, line: line)
    {as, bs} = eval_list(as0, bs0, ieval)
    try do
      apply(:erlang, op, as)
    catch
      class, reason ->
        exception(class, reason, bs, ieval)
    else
      value ->
        {:value, value, bs}
    end
  end

  defp expr({:apply_fun, line, fun0, as0, lc}, bs0,
            r_ieval(level: le) = ieval0) do
    ieval = r_ieval(ieval0, line: line)
    funValue = (case (expr(fun0, bs0, ieval)) do
                  {:value, {:dbg_apply, mx, fx, asx}, bsx} ->
                    debugged_cmd({:apply, mx, fx, asx}, bsx,
                                   r_ieval(ieval, level: le + 1))
                  otherFunValue ->
                    otherFunValue
                end)
    case (funValue) do
      {:value, fun, bs1} when is_function(fun) ->
        {as, bs} = eval_list(as0, bs1, ieval)
        eval_function(:undefined, fun, as, bs, :extern, ieval,
                        lc)
      {:value, {m, f}, bs1} when (is_atom(m) and is_atom(f))
                                 ->
        {as, bs} = eval_list(as0, bs1, ieval)
        eval_function(m, f, as, bs, :extern, ieval, lc)
      {:value, badFun, bs1} ->
        exception(:error, {:badfun, badFun}, bs1, ieval)
    end
  end

  defp expr({:apply, line, as0, lc}, bs0, ieval0) do
    ieval = r_ieval(ieval0, line: line)
    {[m, f, as], bs} = eval_list(as0, bs0, ieval)
    eval_function(m, f, as, bs, :extern, ieval, lc)
  end

  defp expr({:receive, line, cs}, bs0,
            r_ieval(level: le) = ieval) do
    trace(:receivex, {le, false})
    eval_receive(:erlang.get(:self), cs, bs0,
                   r_ieval(ieval, line: line))
  end

  defp expr({:receive, line, cs, to, toExprs}, bs0,
            r_ieval(level: le) = ieval0) do
    ieval = r_ieval(ieval0, line: line)
    {:value, toVal, toBs} = expr(to, bs0,
                                   r_ieval(ieval, top: false))
    trace(:receivex, {le, true})
    check_timeoutvalue(toVal, toBs, to, ieval)
    {stamp, _} = :erlang.statistics(:wall_clock)
    eval_receive(:erlang.get(:self), cs, toVal, toExprs,
                   toBs, bs0, 0, stamp, ieval)
  end

  defp expr({:send, line, to0, msg0}, bs0, ieval0) do
    ieval = r_ieval(ieval0, line: line)
    ieval1 = r_ieval(ieval, top: false)
    {:value, to, bs1} = expr(to0, bs0, ieval1)
    {:value, msg, bs2} = expr(msg0, bs0, ieval1)
    bs = merge_bindings(bs2, bs1, ieval)
    eval_send(to, msg, bs, ieval)
  end

  defp expr({:bin, line, fs}, bs0, ieval0) do
    ieval = r_ieval(ieval0, line: line,  top: false)
    try do
      :eval_bits.expr_grp(fs, bs0,
                            fn e, b ->
                                 expr(e, b, ieval)
                            end,
                            [], false)
    catch
      class, reason ->
        exception(class, reason, bs0, ieval)
    end
  end

  defp expr({:lc, _Line, e, qs}, bs, ieval) do
    eval_lc(e, qs, bs, ieval)
  end

  defp expr({:bc, _Line, e, qs}, bs, ieval) do
    eval_bc(e, qs, bs, ieval)
  end

  defp expr(e, _Bs, _Ieval) do
    :erlang.error({:NYI, e})
  end

  defp eval_fun(as, {info, bs, cs}) do
    :dbg_debugged.eval(:dbg_ieval, :eval_fun,
                         [cs, as, bs, info])
  end

  defp eval_named_fun(as, rF, {info, bs, cs, fName}) do
    :dbg_debugged.eval(:dbg_ieval, :eval_named_fun,
                         [cs, as, bs, fName, rF, info])
  end

  defp eval_lc(e, qs, bs, ieval) do
    {:value, eval_lc1(e, qs, bs, ieval), bs}
  end

  defp eval_lc1(e, [{:generate, line, p, l0} | qs], bs0,
            ieval0) do
    ieval = r_ieval(ieval0, line: line)
    {:value, l1, bs1} = expr(l0, bs0, r_ieval(ieval, top: false))
    compFun = fn newBs ->
                   eval_lc1(e, qs, newBs, ieval)
              end
    eval_generate(l1, p, bs1, compFun, ieval)
  end

  defp eval_lc1(e, [{:b_generate, line, p, l0} | qs], bs0,
            ieval0) do
    ieval = r_ieval(ieval0, line: line)
    {:value, bin, _} = expr(l0, bs0, r_ieval(ieval, top: false))
    compFun = fn newBs ->
                   eval_lc1(e, qs, newBs, ieval)
              end
    eval_b_generate(bin, p, bs0, compFun, ieval)
  end

  defp eval_lc1(e, [{:guard, q} | qs], bs0, ieval) do
    case (guard(q, bs0)) do
      true ->
        eval_lc1(e, qs, bs0, ieval)
      false ->
        []
    end
  end

  defp eval_lc1(e, [q | qs], bs0, ieval) do
    case (expr(q, bs0, r_ieval(ieval, top: false))) do
      {:value, true, bs} ->
        eval_lc1(e, qs, bs, ieval)
      {:value, false, _Bs} ->
        []
      {:value, v, bs} ->
        exception(:error, {:bad_filter, v}, bs, ieval)
    end
  end

  defp eval_lc1(e, [], bs, ieval) do
    {:value, v, _} = expr(e, bs, r_ieval(ieval, top: false))
    [v]
  end

  defp eval_bc(e, qs, bs, ieval) do
    val = :erlang.list_to_bitstring(eval_bc1(e, qs, bs,
                                               ieval))
    {:value, val, bs}
  end

  defp eval_bc1(e, [{:generate, line, p, l0} | qs], bs0,
            ieval0) do
    ieval = r_ieval(ieval0, line: line)
    {:value, l1, bs1} = expr(l0, bs0, r_ieval(ieval, top: false))
    compFun = fn newBs ->
                   eval_bc1(e, qs, newBs, ieval)
              end
    eval_generate(l1, p, bs1, compFun, ieval)
  end

  defp eval_bc1(e, [{:b_generate, line, p, l0} | qs], bs0,
            ieval0) do
    ieval = r_ieval(ieval0, line: line)
    {:value, bin, _} = expr(l0, bs0, r_ieval(ieval, top: false))
    compFun = fn newBs ->
                   eval_bc1(e, qs, newBs, ieval)
              end
    eval_b_generate(bin, p, bs0, compFun, ieval)
  end

  defp eval_bc1(e, [{:guard, q} | qs], bs0, ieval) do
    case (guard(q, bs0)) do
      true ->
        eval_bc1(e, qs, bs0, ieval)
      false ->
        []
    end
  end

  defp eval_bc1(e, [q | qs], bs0, ieval) do
    case (expr(q, bs0, r_ieval(ieval, top: false))) do
      {:value, true, bs} ->
        eval_bc1(e, qs, bs, ieval)
      {:value, false, _Bs} ->
        []
      {:value, v, bs} ->
        exception(:error, {:bad_filter, v}, bs, ieval)
    end
  end

  defp eval_bc1(e, [], bs, ieval) do
    {:value, v, _} = expr(e, bs, r_ieval(ieval, top: false))
    [v]
  end

  defp eval_generate([v | rest], p, bs0, compFun, ieval) do
    case ((try do
            match1(p, v, :erl_eval.new_bindings(), bs0)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end)) do
      {:match, bsn} ->
        bs2 = add_bindings(bsn, bs0)
        compFun.(bs2) ++ eval_generate(rest, p, bs0, compFun,
                                         ieval)
      :nomatch ->
        eval_generate(rest, p, bs0, compFun, ieval)
    end
  end

  defp eval_generate([], _P, _Bs0, _CompFun, _Ieval) do
    []
  end

  defp eval_generate(term, _P, bs, _CompFun, ieval) do
    exception(:error, {:bad_generator, term}, bs, ieval)
  end

  defp eval_b_generate(<<_ :: bitstring>> = bin, p, bs0, compFun,
            ieval) do
    mfun = match_fun(bs0)
    efun = fn exp, bs ->
                expr(exp, bs, r_ieval())
           end
    case (:eval_bits.bin_gen(p, bin,
                               :erl_eval.new_bindings(), bs0, mfun, efun)) do
      {:match, rest, bs1} ->
        bs2 = add_bindings(bs1, bs0)
        compFun.(bs2) ++ eval_b_generate(rest, p, bs0, compFun,
                                           ieval)
      {:nomatch, rest} ->
        eval_b_generate(rest, p, bs0, compFun, ieval)
      :done ->
        []
    end
  end

  defp eval_b_generate(term, _P, bs, _CompFun, ieval) do
    exception(:error, {:bad_generator, term}, bs, ieval)
  end

  defp safe_bif(m, f, as, bs, ieval) do
    try do
      apply(m, f, as)
    catch
      class, reason ->
        exception(class, reason, bs, ieval, true)
    else
      value ->
        {:value, value, bs}
    end
  end

  defp eval_send(to, msg, bs, ieval) do
    try do
      send(to, msg)
    catch
      class, reason ->
        exception(class, reason, bs, ieval)
    else
      ^msg ->
        trace(:send, {to, msg})
        {:value, msg, bs}
    end
  end

  defp eval_receive(debugged, cs, bs0,
            r_ieval(module: m, line: line, level: le) = ieval) do
    :erlang.trace(debugged, true, [:receive])
    {_, msgs} = :erlang.process_info(debugged, :messages)
    case (receive_clauses(cs, bs0, msgs)) do
      :nomatch ->
        :dbg_iserver.cast(:erlang.get(:int),
                            {:set_status, self(), :waiting, {}})
        :dbg_icmd.tell_attached({:wait_at, m, line, le})
        eval_receive1(debugged, cs, bs0, ieval)
      {:eval, b, bs, msg} ->
        rec_mess(debugged, msg, bs, ieval)
        seq(b, bs, ieval)
    end
  end

  defp eval_receive1(debugged, cs, bs0, ieval) do
    msgs = do_receive(debugged, bs0, ieval)
    case (receive_clauses(cs, bs0, msgs)) do
      :nomatch ->
        eval_receive1(debugged, cs, bs0, ieval)
      {:eval, b, bs, msg} ->
        rec_mess(debugged, msg, bs0, ieval)
        :dbg_iserver.cast(:erlang.get(:int),
                            {:set_status, self(), :running, {}})
        :dbg_icmd.tell_attached(:running)
        seq(b, bs, ieval)
    end
  end

  defp check_timeoutvalue(toVal, _, _, _) when (is_integer(toVal) and
                                  toVal >= 0) do
    true
  end

  defp check_timeoutvalue(:infinity, _, _, _) do
    true
  end

  defp check_timeoutvalue(_ToVal, toBs, to, ieval) do
    line = :erlang.element(2, to)
    exception(:error, :timeout_value, toBs,
                r_ieval(ieval, line: line))
  end

  defp eval_receive(debugged, cs, 0, toExprs, toBs, bs0, 0, _Stamp,
            ieval) do
    {_, msgs} = :erlang.process_info(debugged, :messages)
    case (receive_clauses(cs, bs0, msgs)) do
      :nomatch ->
        trace(:received, :null)
        seq(toExprs, toBs, ieval)
      {:eval, b, bs, msg} ->
        rec_mess_no_trace(debugged, msg, bs0, ieval)
        seq(b, bs, ieval)
    end
  end

  defp eval_receive(debugged, cs, toVal, toExprs, toBs, bs0, 0,
            stamp, r_ieval(module: m, line: line, level: le) = ieval) do
    :erlang.trace(debugged, true, [:receive])
    {_, msgs} = :erlang.process_info(debugged, :messages)
    case (receive_clauses(cs, bs0, msgs)) do
      :nomatch ->
        {stamp1, time1} = newtime(stamp, toVal)
        :dbg_iserver.cast(:erlang.get(:int),
                            {:set_status, self(), :waiting, {}})
        :dbg_icmd.tell_attached({:wait_after_at, m, line, le})
        eval_receive(debugged, cs, time1, toExprs, toBs, bs0,
                       :infinity, stamp1, ieval)
      {:eval, b, bs, msg} ->
        rec_mess(debugged, msg, bs0, ieval)
        seq(b, bs, ieval)
    end
  end

  defp eval_receive(debugged, cs, toVal, toExprs, toBs, bs0, _,
            stamp, ieval) do
    case (do_receive(debugged, toVal, stamp, bs0, ieval)) do
      :timeout ->
        trace(:received, :null)
        rec_mess(debugged)
        :dbg_iserver.cast(:erlang.get(:int),
                            {:set_status, self(), :running, {}})
        :dbg_icmd.tell_attached(:running)
        seq(toExprs, toBs, ieval)
      msgs ->
        case (receive_clauses(cs, bs0, msgs)) do
          :nomatch ->
            {stamp1, time1} = newtime(stamp, toVal)
            eval_receive(debugged, cs, time1, toExprs, toBs, bs0,
                           :infinity, stamp1, ieval)
          {:eval, b, bs, msg} ->
            rec_mess(debugged, msg, bs0, ieval)
            :dbg_iserver.cast(:erlang.get(:int),
                                {:set_status, self(), :running, {}})
            :dbg_icmd.tell_attached(:running)
            seq(b, bs, ieval)
        end
    end
  end

  defp do_receive(debugged, bs, ieval) do
    receive do
      {:trace, ^debugged, :receive, msg} ->
        [msg]
      msg ->
        check_exit_msg(msg, bs, ieval)
        :dbg_icmd.handle_msg(msg, :wait_at, bs, ieval)
        do_receive(debugged, bs, ieval)
    end
  end

  defp do_receive(debugged, time, stamp, bs, ieval) do
    receive do
      {:trace, ^debugged, :receive, msg} ->
        [msg]
      {:user, :timeout} ->
        :timeout
      msg ->
        check_exit_msg(msg, bs, ieval)
        :dbg_icmd.handle_msg(msg, :wait_after_at, bs, ieval)
        {stamp1, time1} = newtime(stamp, time)
        do_receive(debugged, time1, stamp1, bs, ieval)
    after time ->
      :timeout
    end
  end

  defp newtime(stamp, :infinity) do
    {stamp, :infinity}
  end

  defp newtime(stamp, time) do
    {stamp1, _} = :erlang.statistics(:wall_clock)
    case (time - (stamp1 - stamp)) do
      newTime when newTime > 0 ->
        {stamp1, newTime}
      _ ->
        {stamp1, 0}
    end
  end

  defp rec_mess(debugged, msg, bs, ieval) do
    :erlang.trace(debugged, false, [:receive])
    flush_traces(debugged)
    send(debugged, {:sys, self(), {:receive, msg}})
    rec_ack(debugged, bs, ieval)
  end

  defp rec_mess(debugged) do
    :erlang.trace(debugged, false, [:receive])
    flush_traces(debugged)
  end

  defp rec_mess_no_trace(debugged, msg, bs, ieval) do
    send(debugged, {:sys, self(), {:receive, msg}})
    rec_ack(debugged, bs, ieval)
  end

  defp rec_ack(debugged, bs, ieval) do
    receive do
      {^debugged, :rec_acked} ->
        true
      msg ->
        check_exit_msg(msg, bs, ieval)
        :io.format('***WARNING*** Unexp msg ~p, ieval ~p~n', [msg, ieval])
    end
  end

  defp flush_traces(debugged) do
    receive do
      {:trace, ^debugged, :receive, _} ->
        flush_traces(debugged)
    after 0 ->
      true
    end
  end

  defp eval_list(es, bs, ieval) do
    eval_list_1(es, [], bs, bs, r_ieval(ieval, top: false))
  end

  defp eval_list_1([e | es], vs, bsOrig, bs0, ieval) do
    {:value, v, bs1} = expr(e, bsOrig, ieval)
    eval_list_1(es, [v | vs], bsOrig,
                  merge_bindings(bs1, bs0, ieval), ieval)
  end

  defp eval_list_1([], vs, _, bs, _Ieval) do
    {:lists.reverse(vs, []), bs}
  end

  defp if_clauses([{:clause, _, [], g, b} | cs], bs, ieval) do
    case (guard(g, bs)) do
      true ->
        seq(b, bs, ieval)
      false ->
        if_clauses(cs, bs, ieval)
    end
  end

  defp if_clauses([], bs, ieval) do
    exception(:error, :if_clause, bs, ieval)
  end

  defp case_clauses(val, [{:clause, _, [p], g, b} | cs], bs0, error,
            ieval) do
    case (match(p, val, bs0)) do
      {:match, bs} ->
        case (guard(g, bs)) do
          true ->
            seq(b, bs, ieval)
          false ->
            case_clauses(val, cs, bs0, error, ieval)
        end
      :nomatch ->
        case_clauses(val, cs, bs0, error, ieval)
    end
  end

  defp case_clauses(val, [], bs, error, ieval) do
    exception(:error, {error, val}, bs, ieval)
  end

  defp catch_clauses(exception, [{:clause, _, [p], g, b} | catchCs],
            bs0, ieval) do
    case (match(p, exception, bs0)) do
      {:match, bs} ->
        case (guard(g, bs)) do
          true ->
            :erlang.put(:exit_info, :undefined)
            :dbg_istk.pop(r_ieval(ieval, :level))
            seq(b, bs, ieval)
          false ->
            catch_clauses(exception, catchCs, bs0, ieval)
        end
      :nomatch ->
        catch_clauses(exception, catchCs, bs0, ieval)
    end
  end

  defp catch_clauses({class, reason, _}, [], _Bs, _Ieval) do
    apply(:erlang, class, [reason])
  end

  defp receive_clauses(cs, bs0, [msg | msgs]) do
    case (rec_clauses(cs, bs0, msg)) do
      :nomatch ->
        receive_clauses(cs, bs0, msgs)
      {:eval, b, bs} ->
        {:eval, b, bs, msg}
    end
  end

  defp receive_clauses(_, _, []) do
    :nomatch
  end

  defp rec_clauses([{:clause, _, pars, g, b} | cs], bs0, msg) do
    case (rec_match(pars, msg, bs0)) do
      {:match, bs} ->
        case (guard(g, bs)) do
          true ->
            trace(:received, msg)
            {:eval, b, bs}
          false ->
            rec_clauses(cs, bs0, msg)
        end
      :nomatch ->
        rec_clauses(cs, bs0, msg)
    end
  end

  defp rec_clauses([], _, _) do
    :nomatch
  end

  defp guard([], _) do
    true
  end

  defp guard(gs, bs) do
    or_guard(gs, bs)
  end

  defp or_guard([g | gs], bs) do
    and_guard(g, bs) or or_guard(gs, bs)
  end

  defp or_guard([], _) do
    false
  end

  defp and_guard([g | gs], bs) do
    case ((try do
            guard_expr(g, bs)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end)) do
      {:value, true} ->
        and_guard(gs, bs)
      _ ->
        false
    end
  end

  defp and_guard([], _) do
    true
  end

  defp guard_exprs([a0 | as0], bs) do
    {:value, a} = guard_expr(a0, bs)
    {:values, as} = guard_exprs(as0, bs)
    {:values, [a | as]}
  end

  defp guard_exprs([], _) do
    {:values, []}
  end

  defp guard_expr({:andalso, _, e1, e2}, bs) do
    case (guard_expr(e1, bs)) do
      {:value, false} = res ->
        res
      {:value, true} ->
        case (guard_expr(e2, bs)) do
          {:value, _Val} = res ->
            res
        end
    end
  end

  defp guard_expr({:orelse, _, e1, e2}, bs) do
    case (guard_expr(e1, bs)) do
      {:value, true} = res ->
        res
      {:value, false} ->
        case (guard_expr(e2, bs)) do
          {:value, _Val} = res ->
            res
        end
    end
  end

  defp guard_expr({:dbg, _, :self, []}, _) do
    {:value, :erlang.get(:self)}
  end

  defp guard_expr({:safe_bif, _, :erlang, :not, as0}, bs) do
    {:values, as} = guard_exprs(as0, bs)
    {:value, apply(:erlang, :not, as)}
  end

  defp guard_expr({:safe_bif, _, mod, func, as0}, bs) do
    {:values, as} = guard_exprs(as0, bs)
    {:value, apply(mod, func, as)}
  end

  defp guard_expr({:var, _, v}, bs) do
    {:value, _} = binding(v, bs)
  end

  defp guard_expr({:value, _, val}, _Bs) do
    {:value, val}
  end

  defp guard_expr({:cons, _, h0, t0}, bs) do
    {:value, h} = guard_expr(h0, bs)
    {:value, t} = guard_expr(t0, bs)
    {:value, [h | t]}
  end

  defp guard_expr({:tuple, _, es0}, bs) do
    {:values, es} = guard_exprs(es0, bs)
    {:value, :erlang.list_to_tuple(es)}
  end

  defp guard_expr({:map, _, fs}, bs0) do
    f = fn g0, b0, _ ->
             {:value, g} = guard_expr(g0, b0)
             {:value, g, b0}
        end
    {map, _} = eval_new_map_fields(fs, bs0, r_ieval(top: false),
                                     f)
    {:value, map}
  end

  defp guard_expr({:map, _, e0, fs0}, bs) do
    {:value, e} = guard_expr(e0, bs)
    fs = eval_map_fields_guard(fs0, bs)
    value = :lists.foldl(fn {:map_assoc, k, v}, mi ->
                              :maps.put(k, v, mi)
                            {:map_exact, k, v}, mi ->
                              :maps.update(k, v, mi)
                         end,
                           e, fs)
    {:value, value}
  end

  defp guard_expr({:bin, _, flds}, bs) do
    {:value, v, _Bs} = :eval_bits.expr_grp(flds, bs,
                                             fn e, b ->
                                                  {:value, v} = guard_expr(e, b)
                                                  {:value, v, b}
                                             end,
                                             [], false)
    {:value, v}
  end

  defp eval_map_fields(fs, bs, ieval) do
    eval_map_fields(fs, bs, ieval, &expr/3)
  end

  defp eval_map_fields_guard(fs0, bs) do
    {fs, _} = eval_map_fields(fs0, bs, r_ieval(),
                                fn g0, bs0, _ ->
                                     {:value, g} = guard_expr(g0, bs0)
                                     {:value, g, bs0}
                                end)
    fs
  end

  defp eval_map_fields(fs, bs, ieval, f) do
    eval_map_fields(fs, bs, ieval, f, [])
  end

  defp eval_map_fields([{:map_field_assoc, line, k0, v0} | fs], bs0,
            ieval0, f, acc) do
    ieval = r_ieval(ieval0, line: line)
    {:value, k, bs1} = f.(k0, bs0, ieval)
    {:value, v, bs2} = f.(v0, bs1, ieval)
    eval_map_fields(fs, bs2, ieval0, f,
                      [{:map_assoc, k, v} | acc])
  end

  defp eval_map_fields([{:map_field_exact, line, k0, v0} | fs], bs0,
            ieval0, f, acc) do
    ieval = r_ieval(ieval0, line: line)
    {:value, k, bs1} = f.(k0, bs0, ieval)
    {:value, v, bs2} = f.(v0, bs1, ieval)
    eval_map_fields(fs, bs2, ieval0, f,
                      [{:map_exact, k, v} | acc])
  end

  defp eval_map_fields([], bs, _Ieval, _F, acc) do
    {:lists.reverse(acc), bs}
  end

  defp eval_new_map_fields(fs, bs0, ieval, f) do
    eval_new_map_fields(fs, bs0, ieval, f, [])
  end

  defp eval_new_map_fields([{line, k0, v0} | fs], bs0, ieval0, f, acc) do
    ieval = r_ieval(ieval0, line: line)
    {:value, k, bs1} = f.(k0, bs0, ieval)
    {:value, v, bs2} = f.(v0, bs1, ieval)
    eval_new_map_fields(fs, bs2, ieval0, f, [{k, v} | acc])
  end

  defp eval_new_map_fields([], bs, _, _, acc) do
    {:maps.from_list(:lists.reverse(acc)), bs}
  end

  defp match(pat, term, bs) do
    try do
      match1(pat, term, bs, bs)
    catch
      result ->
        result
    end
  end

  defp match1({:value, _, v}, v, bs, _BBs) do
    {:match, bs}
  end

  defp match1({:var, _, :_}, term, bs, _BBs) do
    {:match, add_anon(term, bs)}
  end

  defp match1({:var, _, name}, term, bs, _BBs) do
    case (binding(name, bs)) do
      {:value, ^term} ->
        {:match, bs}
      {:value, _} ->
        throw(:nomatch)
      :unbound ->
        {:match, [{name, term} | bs]}
    end
  end

  defp match1({:match, _, pat1, pat2}, term, bs0, bBs) do
    {:match, bs1} = match1(pat1, term, bs0, bBs)
    match1(pat2, term, bs1, bBs)
  end

  defp match1({:cons, _, h, t}, [h1 | t1], bs0, bBs) do
    {:match, bs} = match1(h, h1, bs0, bBs)
    match1(t, t1, bs, bBs)
  end

  defp match1({:tuple, _, elts}, tuple, bs, bBs)
      when length(elts) === tuple_size(tuple) do
    match_tuple(elts, tuple, 1, bs, bBs)
  end

  defp match1({:map, _, fields}, map, bs, bBs)
      when is_map(map) do
    match_map(fields, map, bs, bBs)
  end

  defp match1({:bin, _, fs}, b, bs0, bBs)
      when is_bitstring(b) do
    try do
      :eval_bits.match_bits(fs, b, bs0, bBs, match_fun(bBs),
                              fn e, bs ->
                                   expr(e, bs, r_ieval())
                              end,
                              false)
    catch
      _, _ ->
        throw(:nomatch)
    end
  end

  defp match1(_, _, _, _) do
    throw(:nomatch)
  end

  defp match_fun(bBs) do
    fn :match, {l, r, bs} ->
         match1(l, r, bs, bBs)
       :binding, {name, bs} ->
         binding(name, bs)
       :add_binding, {name, val, bs} ->
         add_binding(name, val, bs)
    end
  end

  defp match_tuple([e | es], tuple, i, bs0, bBs) do
    {:match, bs} = match1(e, :erlang.element(i, tuple), bs0,
                            bBs)
    match_tuple(es, tuple, i + 1, bs, bBs)
  end

  defp match_tuple([], _, _, bs, _BBs) do
    {:match, bs}
  end

  defp match_map([{:map_field_exact, _, k0, pat} | fs], map, bs0,
            bBs) do
    try do
      guard_expr(k0, bBs)
    catch
      _, _ ->
        throw(:nomatch)
    else
      {:value, k} ->
        case (map) do
          %{^k => value} ->
            {:match, bs} = match1(pat, value, bs0, bBs)
            match_map(fs, map, bs, bBs)
          %{} ->
            throw(:nomatch)
        end
    end
  end

  defp match_map([], _, bs, _BBs) do
    {:match, bs}
  end

  defp head_match([par | pars], [arg | args], bs0, bBs) do
    try do
      match1(par, arg, bs0, bBs)
    catch
      result ->
        result
    else
      {:match, bs} ->
        head_match(pars, args, bs, bBs)
    end
  end

  defp head_match([], [], bs, _) do
    {:match, bs}
  end

  defp rec_match([par], msg, bs0) do
    match(par, msg, bs0)
  end

  defp binding(name, [{name, val} | _]) do
    {:value, val}
  end

  defp binding(name, [_, {name, val} | _]) do
    {:value, val}
  end

  defp binding(name, [_, _, {name, val} | _]) do
    {:value, val}
  end

  defp binding(name, [_, _, _, {name, val} | _]) do
    {:value, val}
  end

  defp binding(name, [_, _, _, _, {name, val} | _]) do
    {:value, val}
  end

  defp binding(name, [_, _, _, _, _, {name, val} | _]) do
    {:value, val}
  end

  defp binding(name, [_, _, _, _, _, _ | bs]) do
    binding(name, bs)
  end

  defp binding(name, [_, _, _, _, _ | bs]) do
    binding(name, bs)
  end

  defp binding(name, [_, _, _, _ | bs]) do
    binding(name, bs)
  end

  defp binding(name, [_, _, _ | bs]) do
    binding(name, bs)
  end

  defp binding(name, [_, _ | bs]) do
    binding(name, bs)
  end

  defp binding(name, [_ | bs]) do
    binding(name, bs)
  end

  defp binding(_, []) do
    :unbound
  end

  defp add_anon(val, [{:_, _} | bs]) do
    [{:_, val} | bs]
  end

  defp add_anon(val, [b1, {:_, _} | bs]) do
    [b1, {:_, val} | bs]
  end

  defp add_anon(val, [b1, b2, {:_, _} | bs]) do
    [b1, b2, {:_, val} | bs]
  end

  defp add_anon(val, [b1, b2, b3, {:_, _} | bs]) do
    [b1, b2, b3, {:_, val} | bs]
  end

  defp add_anon(val, [b1, b2, b3, b4, {:_, _} | bs]) do
    [b1, b2, b3, b4, {:_, val} | bs]
  end

  defp add_anon(val, [b1, b2, b3, b4, b5, {:_, _} | bs]) do
    [b1, b2, b3, b4, b5, {:_, val} | bs]
  end

  defp add_anon(val, [b1, b2, b3, b4, b5, b6 | bs]) do
    [b1, b2, b3, b4, b5, b6 | add_anon(val, bs)]
  end

  defp add_anon(val, [b1, b2, b3, b4, b5 | bs]) do
    [b1, b2, b3, b4, b5 | add_anon(val, bs)]
  end

  defp add_anon(val, [b1, b2, b3, b4 | bs]) do
    [b1, b2, b3, b4 | add_anon(val, bs)]
  end

  defp add_anon(val, [b1, b2, b3 | bs]) do
    [b1, b2, b3 | add_anon(val, bs)]
  end

  defp add_anon(val, [b1, b2 | bs]) do
    [b1, b2 | add_anon(val, bs)]
  end

  defp add_anon(val, [b1 | bs]) do
    [b1 | add_anon(val, bs)]
  end

  defp add_anon(val, []) do
    [{:_, val}]
  end

  defp merge_bindings(bs, bs, _Ieval) do
    bs
  end

  defp merge_bindings([{name, v} | b1s], b2s, ieval) do
    case (binding(name, b2s)) do
      {:value, ^v} ->
        merge_bindings(b1s, b2s, ieval)
      {:value, _} when name === :_ ->
        b2s1 = :lists.keydelete(:_, 1, b2s)
        [{name, v} | merge_bindings(b1s, b2s1, ieval)]
      {:value, _} ->
        exception(:error, {:badmatch, v}, b2s, ieval)
      :unbound ->
        [{name, v} | merge_bindings(b1s, b2s, ieval)]
    end
  end

  defp merge_bindings([], b2s, _Ieval) do
    b2s
  end

  defp add_bindings(bs1, []) do
    bs1
  end

  defp add_bindings([{name, v} | bs], toBs0) do
    toBs = add_binding(name, v, toBs0)
    add_bindings(bs, toBs)
  end

  defp add_bindings([], toBs) do
    toBs
  end

  defp add_binding(n, val, [{n, _} | bs]) do
    [{n, val} | bs]
  end

  defp add_binding(n, val, [b1, {n, _} | bs]) do
    [b1, {n, val} | bs]
  end

  defp add_binding(n, val, [b1, b2, {n, _} | bs]) do
    [b1, b2, {n, val} | bs]
  end

  defp add_binding(n, val, [b1, b2, b3, {n, _} | bs]) do
    [b1, b2, b3, {n, val} | bs]
  end

  defp add_binding(n, val, [b1, b2, b3, b4, {n, _} | bs]) do
    [b1, b2, b3, b4, {n, val} | bs]
  end

  defp add_binding(n, val, [b1, b2, b3, b4, b5, {n, _} | bs]) do
    [b1, b2, b3, b4, b5, {n, val} | bs]
  end

  defp add_binding(n, val, [b1, b2, b3, b4, b5, b6 | bs]) do
    [b1, b2, b3, b4, b5, b6 | add_binding(n, val, bs)]
  end

  defp add_binding(n, val, [b1, b2, b3, b4, b5 | bs]) do
    [b1, b2, b3, b4, b5 | add_binding(n, val, bs)]
  end

  defp add_binding(n, val, [b1, b2, b3, b4 | bs]) do
    [b1, b2, b3, b4 | add_binding(n, val, bs)]
  end

  defp add_binding(n, val, [b1, b2, b3 | bs]) do
    [b1, b2, b3 | add_binding(n, val, bs)]
  end

  defp add_binding(n, val, [b1, b2 | bs]) do
    [b1, b2 | add_binding(n, val, bs)]
  end

  defp add_binding(n, val, [b1 | bs]) do
    [b1 | add_binding(n, val, bs)]
  end

  defp add_binding(n, val, []) do
    [{n, val}]
  end

  defp get_stacktrace() do
    case (:erlang.get(:stacktrace)) do
      makeStk when is_function(makeStk, 1) ->
        depth = :erlang.system_flag(:backtrace_depth, 8)
        :erlang.system_flag(:backtrace_depth, depth)
        stk = makeStk.(depth)
        :erlang.put(:stacktrace, stk)
        stk
      stk when is_list(stk) ->
        stk
    end
  end

end