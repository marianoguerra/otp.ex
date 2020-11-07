defmodule :m_erl_eval do
  use Bitwise
  import :lists, only: [foldl: 3, member: 2, reverse: 1]
  def exprs(exprs, bs) do
    case (check_command(exprs, bs)) do
      :ok ->
        exprs(exprs, bs, :none, :none, :none)
      {:error, {_Line, _Mod, error}} ->
        :erlang.raise(:error, error,
                        :erlang.element(2,
                                          :erlang.process_info(self(),
                                                                 :current_stacktrace)))
    end
  end

  def exprs(exprs, bs, lf) do
    exprs(exprs, bs, lf, :none, :none)
  end

  def exprs(exprs, bs, lf, ef) do
    exprs(exprs, bs, lf, ef, :none)
  end

  defp exprs([e], bs0, lf, ef, rBs) do
    expr(e, bs0, lf, ef, rBs)
  end

  defp exprs([e | es], bs0, lf, ef, rBs) do
    rBs1 = :none
    {:value, _V, bs} = expr(e, bs0, lf, ef, rBs1)
    exprs(es, bs, lf, ef, rBs)
  end

  def expr(e, bs) do
    case (check_command([e], bs)) do
      :ok ->
        expr(e, bs, :none, :none, :none)
      {:error, {_Line, _Mod, error}} ->
        :erlang.raise(:error, error,
                        :erlang.element(2,
                                          :erlang.process_info(self(),
                                                                 :current_stacktrace)))
    end
  end

  def expr(e, bs, lf) do
    expr(e, bs, lf, :none, :none)
  end

  def expr(e, bs, lf, ef) do
    expr(e, bs, lf, ef, :none)
  end

  def check_command(es, bs) do
    opts = [:bitlevel_binaries, :binary_comprehension]
    case (:erl_lint.exprs_opt(es, bindings(bs), opts)) do
      {:ok, _Ws} ->
        :ok
      {:error, [{_File, [error | _]}], _Ws} ->
        {:error, error}
    end
  end

  def fun_data(f) when is_function(f) do
    case (:erlang.fun_info(f, :module)) do
      {:module, :erl_eval} ->
        case (:erlang.fun_info(f, :env)) do
          {:env, [{fBs, _FLf, _FEf, fCs}]} ->
            {:fun_data, fBs, fCs}
          {:env, [{fBs, _FLf, _FEf, fCs, fName}]} ->
            {:named_fun_data, fBs, fName, fCs}
        end
      _ ->
        false
    end
  end

  def fun_data(_T) do
    false
  end

  def expr({:var, _, v}, bs, _Lf, _Ef, rBs) do
    case (binding(v, bs)) do
      {:value, val} ->
        ret_expr(val, bs, rBs)
      :unbound ->
        :erlang.raise(:error, {:unbound, v},
                        :erlang.element(2,
                                          :erlang.process_info(self(),
                                                                 :current_stacktrace)))
    end
  end

  def expr({:char, _, c}, bs, _Lf, _Ef, rBs) do
    ret_expr(c, bs, rBs)
  end

  def expr({:integer, _, i}, bs, _Lf, _Ef, rBs) do
    ret_expr(i, bs, rBs)
  end

  def expr({:float, _, f}, bs, _Lf, _Ef, rBs) do
    ret_expr(f, bs, rBs)
  end

  def expr({:atom, _, a}, bs, _Lf, _Ef, rBs) do
    ret_expr(a, bs, rBs)
  end

  def expr({:string, _, s}, bs, _Lf, _Ef, rBs) do
    ret_expr(s, bs, rBs)
  end

  def expr({nil, _}, bs, _Lf, _Ef, rBs) do
    ret_expr([], bs, rBs)
  end

  def expr({:cons, _, h0, t0}, bs0, lf, ef, rBs) do
    {:value, h, bs1} = expr(h0, bs0, lf, ef, :none)
    {:value, t, bs2} = expr(t0, bs0, lf, ef, :none)
    ret_expr([h | t], merge_bindings(bs1, bs2), rBs)
  end

  def expr({:lc, _, e, qs}, bs, lf, ef, rBs) do
    eval_lc(e, qs, bs, lf, ef, rBs)
  end

  def expr({:bc, _, e, qs}, bs, lf, ef, rBs) do
    eval_bc(e, qs, bs, lf, ef, rBs)
  end

  def expr({:tuple, _, es}, bs0, lf, ef, rBs) do
    {vs, bs} = expr_list(es, bs0, lf, ef)
    ret_expr(:erlang.list_to_tuple(vs), bs, rBs)
  end

  def expr({:record_field, _, _, name, _}, _Bs, _Lf, _Ef,
           _RBs) do
    :erlang.raise(:error, {:undef_record, name},
                    :erlang.element(2,
                                      :erlang.process_info(self(),
                                                             :current_stacktrace)))
  end

  def expr({:record_index, _, name, _}, _Bs, _Lf, _Ef,
           _RBs) do
    :erlang.raise(:error, {:undef_record, name},
                    :erlang.element(2,
                                      :erlang.process_info(self(),
                                                             :current_stacktrace)))
  end

  def expr({:record, _, name, _}, _Bs, _Lf, _Ef, _RBs) do
    :erlang.raise(:error, {:undef_record, name},
                    :erlang.element(2,
                                      :erlang.process_info(self(),
                                                             :current_stacktrace)))
  end

  def expr({:record, _, _, name, _}, _Bs, _Lf, _Ef, _RBs) do
    :erlang.raise(:error, {:undef_record, name},
                    :erlang.element(2,
                                      :erlang.process_info(self(),
                                                             :current_stacktrace)))
  end

  def expr({:map, _, binding, es}, bs0, lf, ef, rBs) do
    {:value, map0, bs1} = expr(binding, bs0, lf, ef, :none)
    {vs, bs2} = eval_map_fields(es, bs0, lf, ef)
    _ = :maps.put(:k, :v, map0)
    map1 = :lists.foldl(fn {:map_assoc, k, v}, mi ->
                             :maps.put(k, v, mi)
                           {:map_exact, k, v}, mi ->
                             :maps.update(k, v, mi)
                        end,
                          map0, vs)
    ret_expr(map1, merge_bindings(bs2, bs1), rBs)
  end

  def expr({:map, _, es}, bs0, lf, ef, rBs) do
    {vs, bs} = eval_map_fields(es, bs0, lf, ef)
    ret_expr(:lists.foldl(fn {:map_assoc, k, v}, mi ->
                               :maps.put(k, v, mi)
                          end,
                            :maps.new(), vs),
               bs, rBs)
  end

  def expr({:block, _, es}, bs, lf, ef, rBs) do
    exprs(es, bs, lf, ef, rBs)
  end

  def expr({:if, _, cs}, bs, lf, ef, rBs) do
    if_clauses(cs, bs, lf, ef, rBs)
  end

  def expr({:case, _, e, cs}, bs0, lf, ef, rBs) do
    {:value, val, bs} = expr(e, bs0, lf, ef, :none)
    case_clauses(val, cs, bs, lf, ef, rBs)
  end

  def expr({:try, _, b, cases, catches, aB}, bs, lf, ef,
           rBs) do
    try_clauses(b, cases, catches, aB, bs, lf, ef, rBs)
  end

  def expr({:receive, _, cs}, bs, lf, ef, rBs) do
    receive_clauses(cs, bs, lf, ef, rBs)
  end

  def expr({:receive, _, cs, e, tB}, bs0, lf, ef, rBs) do
    {:value, t, bs} = expr(e, bs0, lf, ef, :none)
    receive_clauses(t, cs, {tB, bs}, bs0, lf, ef, rBs)
  end

  def expr({:fun, _Line, {:function, mod0, name0, arity0}},
           bs0, lf, ef, rBs) do
    {[mod, name, arity], bs} = expr_list([mod0, name0,
                                            arity0],
                                           bs0, lf, ef)
    f = :erlang.make_fun(mod, name, arity)
    ret_expr(f, bs, rBs)
  end

  def expr({:fun, _Line, {:function, name, arity}}, _Bs0,
           _Lf, _Ef, _RBs) do
    :erlang.raise(:error, :undef,
                    [{:erl_eval, name, arity} | :erlang.element(2,
                                                                  :erlang.process_info(self(),
                                                                                         :current_stacktrace))])
  end

  def expr({:fun, line, {:clauses, cs}} = ex, bs, lf, ef,
           rBs) do
    {ex1, _} = hide_calls(ex, 0)
    {:ok, used} = :erl_lint.used_vars([ex1], bs)
    en = :orddict.filter(fn k, _V ->
                              member(k, used)
                         end,
                           bs)
    info = {en, lf, ef, cs}
    f = (case (length(:erlang.element(3, hd(cs)))) do
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
                              p, q],
                             info)
             end
           18 ->
             fn a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q,
                  r ->
                  eval_fun([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o,
                              p, q, r],
                             info)
             end
           19 ->
             fn a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r,
                  s ->
                  eval_fun([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o,
                              p, q, r, s],
                             info)
             end
           20 ->
             fn a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r,
                  s, t ->
                  eval_fun([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o,
                              p, q, r, s, t],
                             info)
             end
           _Other ->
             l = :erl_anno.location(line)
             :erlang.raise(:error,
                             {:argument_limit, {:fun, l, to_terms(cs)}},
                             :erlang.element(2,
                                               :erlang.process_info(self(),
                                                                      :current_stacktrace)))
         end)
    ret_expr(f, bs, rBs)
  end

  def expr({:named_fun, line, name, cs} = ex, bs, lf, ef,
           rBs) do
    {ex1, _} = hide_calls(ex, 0)
    {:ok, used} = :erl_lint.used_vars([ex1], bs)
    en = :orddict.filter(fn k, _V ->
                              member(k, used)
                         end,
                           bs)
    info = {en, lf, ef, cs, name}
    f = (case (length(:erlang.element(3, hd(cs)))) do
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
                                 n, o, p],
                                rF, info)
             end
           17 ->
             fn rF
             a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q ->
               eval_named_fun([a, b, c, d, e, f, g, h, i, j, k, l, m,
                                 n, o, p, q],
                                rF, info)
             end
           18 ->
             fn rF
             a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r ->
               eval_named_fun([a, b, c, d, e, f, g, h, i, j, k, l, m,
                                 n, o, p, q, r],
                                rF, info)
             end
           19 ->
             fn rF
             a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r,
               s ->
               eval_named_fun([a, b, c, d, e, f, g, h, i, j, k, l, m,
                                 n, o, p, q, r, s],
                                rF, info)
             end
           20 ->
             fn rF
             a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s,
               t ->
               eval_named_fun([a, b, c, d, e, f, g, h, i, j, k, l, m,
                                 n, o, p, q, r, s, t],
                                rF, info)
             end
           _Other ->
             l = :erl_anno.location(line)
             :erlang.raise(:error,
                             {:argument_limit,
                                {:named_fun, l, name, to_terms(cs)}},
                             :erlang.element(2,
                                               :erlang.process_info(self(),
                                                                      :current_stacktrace)))
         end)
    ret_expr(f, bs, rBs)
  end

  def expr({:call, _,
            {:remote, _, {:atom, _, :qlc}, {:atom, _, :q}},
            [{:lc, _, _E, _Qs} = lC | as0]},
           bs0, lf, ef, rBs)
      when length(as0) <= 1 do
    maxLine = find_maxline(lC)
    {lC1, d} = hide_calls(lC, maxLine)
    case (:qlc.transform_from_evaluator(lC1, bs0)) do
      {:ok, {:call, l, remote, [qLC]}} ->
        qLC1 = unhide_calls(qLC, maxLine, d)
        expr({:call, l, remote, [qLC1 | as0]}, bs0, lf, ef, rBs)
      {:not_ok, error} ->
        ret_expr(error, bs0, rBs)
    end
  end

  def expr({:call, l1,
            {:remote, l2,
               {:record_field, _, {:atom, _, :""},
                  {:atom, _, :qlc} = mod},
               {:atom, _, :q} = func},
            [{:lc, _, _E, _Qs} | as0] = as},
           bs, lf, ef, rBs)
      when length(as0) <= 1 do
    expr({:call, l1, {:remote, l2, mod, func}, as}, bs, lf,
           ef, rBs)
  end

  def expr({:call, _, {:remote, _, mod, func}, as0}, bs0,
           lf, ef, rBs) do
    {:value, m, bs1} = expr(mod, bs0, lf, ef, :none)
    {:value, f, bs2} = expr(func, bs0, lf, ef, :none)
    {as, bs3} = expr_list(as0, merge_bindings(bs1, bs2), lf,
                            ef)
    case (is_atom(m) and :erl_internal.bif(m, f,
                                             length(as))) do
      true ->
        bif(f, as, bs3, ef, rBs)
      false ->
        do_apply(m, f, as, bs3, ef, rBs)
    end
  end

  def expr({:call, _, {:atom, _, func}, as0}, bs0, lf, ef,
           rBs) do
    case (:erl_internal.bif(func, length(as0))) do
      true ->
        {as, bs} = expr_list(as0, bs0, lf, ef)
        bif(func, as, bs, ef, rBs)
      false ->
        local_func(func, as0, bs0, lf, ef, rBs)
    end
  end

  def expr({:call, _, func0, as0}, bs0, lf, ef, rBs) do
    {:value, func, bs1} = expr(func0, bs0, lf, ef, :none)
    {as, bs2} = expr_list(as0, bs1, lf, ef)
    case (func) do
      {m, f} when (is_atom(m) and is_atom(f)) ->
        :erlang.raise(:error, {:badfun, func},
                        :erlang.element(2,
                                          :erlang.process_info(self(),
                                                                 :current_stacktrace)))
      _ ->
        do_apply(func, as, bs2, ef, rBs)
    end
  end

  def expr({:catch, _, expr}, bs0, lf, ef, rBs) do
    try do
      expr(expr, bs0, lf, ef, :none)
    catch
      term ->
        ret_expr(term, bs0, rBs)
      :exit, reason ->
        ret_expr({:EXIT, reason}, bs0, rBs)
      :error, reason ->
        ret_expr({:EXIT, {reason, __STACKTRACE__}}, bs0, rBs)
    else
      {:value, v, bs} ->
        ret_expr(v, bs, rBs)
    end
  end

  def expr({:match, _, lhs, rhs0}, bs0, lf, ef, rBs) do
    {:value, rhs, bs1} = expr(rhs0, bs0, lf, ef, :none)
    case (match(lhs, rhs, bs1)) do
      {:match, bs} ->
        ret_expr(rhs, bs, rBs)
      :nomatch ->
        :erlang.raise(:error, {:badmatch, rhs},
                        :erlang.element(2,
                                          :erlang.process_info(self(),
                                                                 :current_stacktrace)))
    end
  end

  def expr({:op, _, op, a0}, bs0, lf, ef, rBs) do
    {:value, a, bs} = expr(a0, bs0, lf, ef, :none)
    eval_op(op, a, bs, ef, rBs)
  end

  def expr({:op, _, :andalso, l0, r0}, bs0, lf, ef, rBs) do
    {:value, l, bs1} = expr(l0, bs0, lf, ef, :none)
    v = (case (l) do
           true ->
             {:value, r, _} = expr(r0, bs1, lf, ef, :none)
             r
           false ->
             false
           _ ->
             :erlang.raise(:error, {:badarg, l},
                             :erlang.element(2,
                                               :erlang.process_info(self(),
                                                                      :current_stacktrace)))
         end)
    ret_expr(v, bs1, rBs)
  end

  def expr({:op, _, :orelse, l0, r0}, bs0, lf, ef, rBs) do
    {:value, l, bs1} = expr(l0, bs0, lf, ef, :none)
    v = (case (l) do
           true ->
             true
           false ->
             {:value, r, _} = expr(r0, bs1, lf, ef, :none)
             r
           _ ->
             :erlang.raise(:error, {:badarg, l},
                             :erlang.element(2,
                                               :erlang.process_info(self(),
                                                                      :current_stacktrace)))
         end)
    ret_expr(v, bs1, rBs)
  end

  def expr({:op, _, op, l0, r0}, bs0, lf, ef, rBs) do
    {:value, l, bs1} = expr(l0, bs0, lf, ef, :none)
    {:value, r, bs2} = expr(r0, bs0, lf, ef, :none)
    eval_op(op, l, r, merge_bindings(bs1, bs2), ef, rBs)
  end

  def expr({:bin, _, fs}, bs0, lf, ef, rBs) do
    evalFun = fn e, b ->
                   expr(e, b, lf, ef, :none)
              end
    {:value, v, bs} = :eval_bits.expr_grp(fs, bs0, evalFun)
    ret_expr(v, bs, rBs)
  end

  def expr({:remote, _, _, _}, _Bs, _Lf, _Ef, _RBs) do
    :erlang.raise(:error, {:badexpr, :":"},
                    :erlang.element(2,
                                      :erlang.process_info(self(),
                                                             :current_stacktrace)))
  end

  def expr({:value, _, val}, bs, _Lf, _Ef, rBs) do
    ret_expr(val, bs, rBs)
  end

  defp find_maxline(lC) do
    :erlang.put(:"$erl_eval_max_line", 0)
    f = fn a ->
             l = :erl_anno.line(a)
             case (:erlang.and(is_integer(l),
                                 l > :erlang.get(:"$erl_eval_max_line"))) do
               true ->
                 :erlang.put(:"$erl_eval_max_line", l)
               false ->
                 :ok
             end
        end
    _ = :erl_parse.map_anno(f, lC)
    :erlang.erase(:"$erl_eval_max_line")
  end

  defp hide_calls(lC, maxLine) do
    lineId0 = maxLine + 1
    {nLC, _, d} = hide(lC, lineId0, :maps.new())
    {nLC, d}
  end

  defp hide({:value, l, v}, id, d) do
    a = :erl_anno.new(id)
    {{:atom, a, :ok}, id + 1,
       :maps.put(id, {:value, l, v}, d)}
  end

  defp hide({:call, l, {:atom, _, n} = atom, args}, id0,
            d0) do
    {nArgs, id, d} = hide(args, id0, d0)
    c = (case (:erl_internal.bif(n, length(args))) do
           true ->
             {:call, l, atom, nArgs}
           false ->
             a = :erl_anno.new(id)
             {:call, a, {:remote, l, {:atom, l, :m}, {:atom, l, :f}},
                nArgs}
         end)
    {c, id + 1, :maps.put(id, {:call, atom}, d)}
  end

  defp hide(t0, id0, d0) when is_tuple(t0) do
    {l, id, d} = hide(:erlang.tuple_to_list(t0), id0, d0)
    {:erlang.list_to_tuple(l), id, d}
  end

  defp hide([e0 | es0], id0, d0) do
    {e, id1, d1} = hide(e0, id0, d0)
    {es, id, d} = hide(es0, id1, d1)
    {[e | es], id, d}
  end

  defp hide(e, id, d) do
    {e, id, d}
  end

  defp unhide_calls({:atom, a, :ok} = e, maxLine, d) do
    l = :erl_anno.line(a)
    cond do
      l > maxLine ->
        :erlang.map_get(l, d)
      true ->
        e
    end
  end

  defp unhide_calls({:call, a,
             {:remote, l, {:atom, l, :m}, {:atom, l, :f}} = f, args},
            maxLine, d) do
    line = :erl_anno.line(a)
    cond do
      line > maxLine ->
        {:call, atom} = :erlang.map_get(line, d)
        {:call, l, atom, unhide_calls(args, maxLine, d)}
      true ->
        {:call, a, f, unhide_calls(args, maxLine, d)}
    end
  end

  defp unhide_calls(t, maxLine, d) when is_tuple(t) do
    :erlang.list_to_tuple(unhide_calls(:erlang.tuple_to_list(t),
                                         maxLine, d))
  end

  defp unhide_calls([e | es], maxLine, d) do
    [unhide_calls(e, maxLine, d) | unhide_calls(es, maxLine,
                                                  d)]
  end

  defp unhide_calls(e, _MaxLine, _D) do
    e
  end

  defp local_func(func, as0, bs0, {:value, f}, ef, :value) do
    {as1, _Bs1} = expr_list(as0, bs0, {:value, f}, ef)
    f.(func, as1)
  end

  defp local_func(func, as0, bs0, {:value, f}, ef, rBs) do
    {as1, bs1} = expr_list(as0, bs0, {:value, f}, ef)
    ret_expr(f.(func, as1), bs1, rBs)
  end

  defp local_func(func, as0, bs0, {:value, f, eas}, ef, rBs) do
    fun = fn name, args ->
               apply(f, [[name, args] | eas])
          end
    local_func(func, as0, bs0, {:value, fun}, ef, rBs)
  end

  defp local_func(func, as, bs, {:eval, f}, _Ef, rBs) do
    local_func2(f.(func, as, bs), rBs)
  end

  defp local_func(func, as, bs, {:eval, f, eas}, _Ef, rBs) do
    local_func2(apply(f, [[func, as, bs] | eas]), rBs)
  end

  defp local_func(func, as0, bs0, {m, f}, ef, rBs) do
    {as1, bs1} = expr_list(as0, bs0, {m, f}, ef)
    ret_expr(apply(m, f, [func, as1]), bs1, rBs)
  end

  defp local_func(func, as, _Bs, {m, f, eas}, _Ef, rBs) do
    local_func2(apply(m, f, [[func, as] | eas]), rBs)
  end

  defp local_func(func, as0, _Bs0, :none, _Ef, _RBs) do
    :erlang.raise(:error, :undef,
                    [{:erl_eval, func, length(as0)} | :erlang.element(2,
                                                                        :erlang.process_info(self(),
                                                                                               :current_stacktrace))])
  end

  defp local_func2({:value, v, bs}, rBs) do
    ret_expr(v, bs, rBs)
  end

  defp local_func2({:eval, f, as, bs}, rBs) do
    do_apply(f, as, bs, :none, rBs)
  end

  defp bif(:apply, [:erlang, :apply, as], bs, ef, rBs) do
    bif(:apply, as, bs, ef, rBs)
  end

  defp bif(:apply, [m, f, as], bs, ef, rBs) do
    do_apply(m, f, as, bs, ef, rBs)
  end

  defp bif(:apply, [f, as], bs, ef, rBs) do
    do_apply(f, as, bs, ef, rBs)
  end

  defp bif(name, as, bs, ef, rBs) do
    do_apply(:erlang, name, as, bs, ef, rBs)
  end

  defp do_apply({m, f} = func, as, bs0, ef, rBs)
      when (tuple_size(m) >= 1 and
              is_atom(:erlang.element(1, m)) and is_atom(f)) do
    case (ef) do
      :none when rBs === :value ->
        apply(m, f, as)
      :none ->
        ret_expr(apply(m, f, as), bs0, rBs)
      {:value, fun} when rBs === :value ->
        fun.(func, as)
      {:value, fun} ->
        ret_expr(fun.(func, as), bs0, rBs)
    end
  end

  defp do_apply(func, as, bs0, ef, rBs) do
    env = (cond do
             is_function(func) ->
               case ({:erlang.fun_info(func, :module),
                        :erlang.fun_info(func, :env)}) do
                 {{:module, :erl_eval}, {:env, env1}} when env1 !== [] ->
                   {:env, env1}
                 _ ->
                   :no_env
               end
             true ->
               :no_env
           end)
    case ({env, ef}) do
      {{:env, [{fBs, fLf, fEf, fCs}]}, _} ->
        nRBs = (cond do
                  rBs === :none ->
                    bs0
                  true ->
                    rBs
                end)
        case ({:erlang.fun_info(func, :arity), length(as)}) do
          {{:arity, arity}, arity} ->
            eval_fun(fCs, as, fBs, fLf, fEf, nRBs)
          _ ->
            :erlang.raise(:error, {:badarity, {func, as}},
                            :erlang.element(2,
                                              :erlang.process_info(self(),
                                                                     :current_stacktrace)))
        end
      {{:env, [{fBs, fLf, fEf, fCs, fName}]}, _} ->
        nRBs = (cond do
                  rBs === :none ->
                    bs0
                  true ->
                    rBs
                end)
        case ({:erlang.fun_info(func, :arity), length(as)}) do
          {{:arity, arity}, arity} ->
            eval_named_fun(fCs, as, fBs, fLf, fEf, fName, func,
                             nRBs)
          _ ->
            :erlang.raise(:error, {:badarity, {func, as}},
                            :erlang.element(2,
                                              :erlang.process_info(self(),
                                                                     :current_stacktrace)))
        end
      {:no_env, :none} when rBs === :value ->
        apply(func, as)
      {:no_env, :none} ->
        ret_expr(apply(func, as), bs0, rBs)
      {:no_env, {:value, f}} when rBs === :value ->
        f.(func, as)
      {:no_env, {:value, f}} ->
        ret_expr(f.(func, as), bs0, rBs)
    end
  end

  defp do_apply(mod, func, as, bs0, ef, rBs) do
    case (ef) do
      :none when rBs === :value ->
        apply(mod, func, as)
      :none ->
        ret_expr(apply(mod, func, as), bs0, rBs)
      {:value, f} when rBs === :value ->
        f.({mod, func}, as)
      {:value, f} ->
        ret_expr(f.({mod, func}, as), bs0, rBs)
    end
  end

  defp eval_lc(e, qs, bs, lf, ef, rBs) do
    ret_expr(:lists.reverse(eval_lc1(e, qs, bs, lf, ef,
                                       [])),
               bs, rBs)
  end

  defp eval_lc1(e, [{:generate, _, p, l0} | qs], bs0, lf, ef,
            acc0) do
    {:value, l1, _Bs1} = expr(l0, bs0, lf, ef, :none)
    compFun = fn bs, acc ->
                   eval_lc1(e, qs, bs, lf, ef, acc)
              end
    eval_generate(l1, p, bs0, lf, ef, compFun, acc0)
  end

  defp eval_lc1(e, [{:b_generate, _, p, l0} | qs], bs0, lf, ef,
            acc0) do
    {:value, bin, _Bs1} = expr(l0, bs0, lf, ef, :none)
    compFun = fn bs, acc ->
                   eval_lc1(e, qs, bs, lf, ef, acc)
              end
    eval_b_generate(bin, p, bs0, lf, ef, compFun, acc0)
  end

  defp eval_lc1(e, [f | qs], bs0, lf, ef, acc) do
    compFun = fn bs ->
                   eval_lc1(e, qs, bs, lf, ef, acc)
              end
    eval_filter(f, bs0, lf, ef, compFun, acc)
  end

  defp eval_lc1(e, [], bs, lf, ef, acc) do
    {:value, v, _} = expr(e, bs, lf, ef, :none)
    [v | acc]
  end

  defp eval_bc(e, qs, bs, lf, ef, rBs) do
    ret_expr(eval_bc1(e, qs, bs, lf, ef, <<>>), bs, rBs)
  end

  defp eval_bc1(e, [{:b_generate, _, p, l0} | qs], bs0, lf, ef,
            acc0) do
    {:value, bin, _Bs1} = expr(l0, bs0, lf, ef, :none)
    compFun = fn bs, acc ->
                   eval_bc1(e, qs, bs, lf, ef, acc)
              end
    eval_b_generate(bin, p, bs0, lf, ef, compFun, acc0)
  end

  defp eval_bc1(e, [{:generate, _, p, l0} | qs], bs0, lf, ef,
            acc0) do
    {:value, list, _Bs1} = expr(l0, bs0, lf, ef, :none)
    compFun = fn bs, acc ->
                   eval_bc1(e, qs, bs, lf, ef, acc)
              end
    eval_generate(list, p, bs0, lf, ef, compFun, acc0)
  end

  defp eval_bc1(e, [f | qs], bs0, lf, ef, acc) do
    compFun = fn bs ->
                   eval_bc1(e, qs, bs, lf, ef, acc)
              end
    eval_filter(f, bs0, lf, ef, compFun, acc)
  end

  defp eval_bc1(e, [], bs, lf, ef, acc) do
    {:value, v, _} = expr(e, bs, lf, ef, :none)
    <<acc :: bitstring, v :: bitstring>>
  end

  defp eval_generate([v | rest], p, bs0, lf, ef, compFun, acc) do
    case (match(p, v, new_bindings(), bs0)) do
      {:match, bsn} ->
        bs2 = add_bindings(bsn, bs0)
        newAcc = compFun.(bs2, acc)
        eval_generate(rest, p, bs0, lf, ef, compFun, newAcc)
      :nomatch ->
        eval_generate(rest, p, bs0, lf, ef, compFun, acc)
    end
  end

  defp eval_generate([], _P, _Bs0, _Lf, _Ef, _CompFun, acc) do
    acc
  end

  defp eval_generate(term, _P, _Bs0, _Lf, _Ef, _CompFun, _Acc) do
    :erlang.raise(:error, {:bad_generator, term},
                    :erlang.element(2,
                                      :erlang.process_info(self(),
                                                             :current_stacktrace)))
  end

  defp eval_b_generate(<<_ :: bitstring>> = bin, p, bs0, lf, ef,
            compFun, acc) do
    mfun = match_fun(bs0)
    efun = fn exp, bs ->
                expr(exp, bs, lf, ef, :none)
           end
    case (:eval_bits.bin_gen(p, bin, new_bindings(), bs0,
                               mfun, efun)) do
      {:match, rest, bs1} ->
        bs2 = add_bindings(bs1, bs0)
        newAcc = compFun.(bs2, acc)
        eval_b_generate(rest, p, bs0, lf, ef, compFun, newAcc)
      {:nomatch, rest} ->
        eval_b_generate(rest, p, bs0, lf, ef, compFun, acc)
      :done ->
        acc
    end
  end

  defp eval_b_generate(term, _P, _Bs0, _Lf, _Ef, _CompFun, _Acc) do
    :erlang.raise(:error, {:bad_generator, term},
                    :erlang.element(2,
                                      :erlang.process_info(self(),
                                                             :current_stacktrace)))
  end

  defp eval_filter(f, bs0, lf, ef, compFun, acc) do
    case (:erl_lint.is_guard_test(f)) do
      true ->
        case (guard_test(f, bs0, lf, ef)) do
          {:value, true, bs1} ->
            compFun.(bs1)
          {:value, false, _} ->
            acc
        end
      false ->
        case (expr(f, bs0, lf, ef, :none)) do
          {:value, true, bs1} ->
            compFun.(bs1)
          {:value, false, _} ->
            acc
          {:value, v, _} ->
            :erlang.raise(:error, {:bad_filter, v},
                            :erlang.element(2,
                                              :erlang.process_info(self(),
                                                                     :current_stacktrace)))
        end
    end
  end

  defp eval_map_fields(fs, bs, lf, ef) do
    eval_map_fields(fs, bs, lf, ef, [])
  end

  defp eval_map_fields([{:map_field_assoc, _, k0, v0} | fs], bs0, lf,
            ef, acc) do
    {:value, k1, bs1} = expr(k0, bs0, lf, ef, :none)
    {:value, v1, bs2} = expr(v0, bs1, lf, ef, :none)
    eval_map_fields(fs, bs2, lf, ef,
                      [{:map_assoc, k1, v1} | acc])
  end

  defp eval_map_fields([{:map_field_exact, _, k0, v0} | fs], bs0, lf,
            ef, acc) do
    {:value, k1, bs1} = expr(k0, bs0, lf, ef, :none)
    {:value, v1, bs2} = expr(v0, bs1, lf, ef, :none)
    eval_map_fields(fs, bs2, lf, ef,
                      [{:map_exact, k1, v1} | acc])
  end

  defp eval_map_fields([], bs, _Lf, _Ef, acc) do
    {:lists.reverse(acc), bs}
  end

  defp ret_expr(v, _Bs, :value) do
    v
  end

  defp ret_expr(v, bs, :none) do
    {:value, v, bs}
  end

  defp ret_expr(v, _Bs, rBs) when is_list(rBs) do
    {:value, v, rBs}
  end

  defp eval_fun(as, {bs0, lf, ef, cs}) do
    eval_fun(cs, as, bs0, lf, ef, :value)
  end

  defp eval_fun([{:clause, _, h, g, b} | cs], as, bs0, lf, ef,
            rBs) do
    case (match_list(h, as, new_bindings(), bs0)) do
      {:match, bsn} ->
        bs1 = add_bindings(bsn, bs0)
        case (guard(g, bs1, lf, ef)) do
          true ->
            exprs(b, bs1, lf, ef, rBs)
          false ->
            eval_fun(cs, as, bs0, lf, ef, rBs)
        end
      :nomatch ->
        eval_fun(cs, as, bs0, lf, ef, rBs)
    end
  end

  defp eval_fun([], as, _Bs, _Lf, _Ef, _RBs) do
    :erlang.raise(:error, :function_clause,
                    [{:erl_eval, :"-inside-an-interpreted-fun-", as} | :erlang.element(2,
                                                             :erlang.process_info(self(),
                                                                                    :current_stacktrace))])
  end

  defp eval_named_fun(as, fun, {bs0, lf, ef, cs, name}) do
    eval_named_fun(cs, as, bs0, lf, ef, name, fun, :value)
  end

  defp eval_named_fun([{:clause, _, h, g, b} | cs], as, bs0, lf, ef,
            name, fun, rBs) do
    bs1 = add_binding(name, fun, bs0)
    case (match_list(h, as, new_bindings(), bs1)) do
      {:match, bsn} ->
        bs2 = add_bindings(bsn, bs1)
        case (guard(g, bs2, lf, ef)) do
          true ->
            exprs(b, bs2, lf, ef, rBs)
          false ->
            eval_named_fun(cs, as, bs0, lf, ef, name, fun, rBs)
        end
      :nomatch ->
        eval_named_fun(cs, as, bs0, lf, ef, name, fun, rBs)
    end
  end

  defp eval_named_fun([], as, _Bs, _Lf, _Ef, _Name, _Fun, _RBs) do
    :erlang.raise(:error, :function_clause,
                    [{:erl_eval, :"-inside-an-interpreted-fun-", as} | :erlang.element(2,
                                                             :erlang.process_info(self(),
                                                                                    :current_stacktrace))])
  end

  def expr_list(es, bs) do
    expr_list(es, bs, :none, :none)
  end

  def expr_list(es, bs, lf) do
    expr_list(es, bs, lf, :none)
  end

  def expr_list(es, bs, lf, ef) do
    expr_list(es, [], bs, bs, lf, ef)
  end

  defp expr_list([e | es], vs, bsOrig, bs0, lf, ef) do
    {:value, v, bs1} = expr(e, bsOrig, lf, ef, :none)
    expr_list(es, [v | vs], bsOrig,
                merge_bindings(bs1, bs0), lf, ef)
  end

  defp expr_list([], vs, _, bs, _Lf, _Ef) do
    {reverse(vs), bs}
  end

  defp eval_op(op, arg1, arg2, bs, ef, rBs) do
    do_apply(:erlang, op, [arg1, arg2], bs, ef, rBs)
  end

  defp eval_op(op, arg, bs, ef, rBs) do
    do_apply(:erlang, op, [arg], bs, ef, rBs)
  end

  defp if_clauses([{:clause, _, [], g, b} | cs], bs, lf, ef,
            rBs) do
    case (guard(g, bs, lf, ef)) do
      true ->
        exprs(b, bs, lf, ef, rBs)
      false ->
        if_clauses(cs, bs, lf, ef, rBs)
    end
  end

  defp if_clauses([], _Bs, _Lf, _Ef, _RBs) do
    :erlang.raise(:error, :if_clause,
                    :erlang.element(2,
                                      :erlang.process_info(self(),
                                                             :current_stacktrace)))
  end

  defp try_clauses(b, cases, catches, aB, bs, lf, ef, rBs) do
    check_stacktrace_vars(catches, bs)
    try do
      exprs(b, bs, lf, ef, :none)
    catch
      class, reason when catches === [] ->
        :erlang.raise(class, reason, __STACKTRACE__)
      class, reason ->
        v = {class, reason, __STACKTRACE__}
        case (match_clause(catches, [v], bs, lf, ef)) do
          {b2, bs2} ->
            exprs(b2, bs2, lf, ef, rBs)
          :nomatch ->
            :erlang.raise(class, reason, __STACKTRACE__)
        end
    else
      {:value, v, bs1} when cases === [] ->
        ret_expr(v, bs1, rBs)
      {:value, v, bs1} ->
        case (match_clause(cases, [v], bs1, lf, ef)) do
          {b2, bs2} ->
            exprs(b2, bs2, lf, ef, rBs)
          :nomatch ->
            :erlang.raise(:error, {:try_clause, v},
                            :erlang.element(2,
                                              :erlang.process_info(self(),
                                                                     :current_stacktrace)))
        end
    after
      cond do
        aB === [] ->
          bs
        true ->
          exprs(aB, bs, lf, ef, :none)
      end
    end
  end

  defp check_stacktrace_vars([{:clause, _, [{:tuple, _, [_, _, sTV]}], _,
              _} |
               cs],
            bs) do
    case (sTV) do
      {:var, _, v} ->
        case (binding(v, bs)) do
          {:value, _} ->
            :erlang.raise(:error, :stacktrace_bound,
                            :erlang.element(2,
                                              :erlang.process_info(self(),
                                                                     :current_stacktrace)))
          :unbound ->
            check_stacktrace_vars(cs, bs)
        end
      _ ->
        :erlang.raise(:error,
                        {:illegal_stacktrace_variable, sTV},
                        :erlang.element(2,
                                          :erlang.process_info(self(),
                                                                 :current_stacktrace)))
    end
  end

  defp check_stacktrace_vars([], _Bs) do
    :ok
  end

  defp case_clauses(val, cs, bs, lf, ef, rBs) do
    case (match_clause(cs, [val], bs, lf, ef)) do
      {b, bs1} ->
        exprs(b, bs1, lf, ef, rBs)
      :nomatch ->
        :erlang.raise(:error, {:case_clause, val},
                        :erlang.element(2,
                                          :erlang.process_info(self(),
                                                                 :current_stacktrace)))
    end
  end

  defp receive_clauses(cs, bs, lf, ef, rBs) do
    receive_clauses(:infinity, cs, :unused, bs, lf, ef, rBs)
  end

  defp receive_clauses(t, cs, tB, bs, lf, ef, rBs) do
    f = fn m ->
             match_clause(cs, [m], bs, lf, ef)
        end
    case (:prim_eval.receive(f, t)) do
      {b, bs1} ->
        exprs(b, bs1, lf, ef, rBs)
      :timeout ->
        {b, bs1} = tB
        exprs(b, bs1, lf, ef, rBs)
    end
  end

  def match_clause(cs, vs, bs, lf) do
    match_clause(cs, vs, bs, lf, :none)
  end

  defp match_clause([{:clause, _, h, g, b} | cs], vals, bs, lf,
            ef) do
    case (match_list(h, vals, bs)) do
      {:match, bs1} ->
        case (guard(g, bs1, lf, ef)) do
          true ->
            {b, bs1}
          false ->
            match_clause(cs, vals, bs, lf, ef)
        end
      :nomatch ->
        match_clause(cs, vals, bs, lf, ef)
    end
  end

  defp match_clause([], _Vals, _Bs, _Lf, _Ef) do
    :nomatch
  end

  defp guard(l = [g | _], bs0, lf, ef) when is_list(g) do
    guard1(l, bs0, lf, ef)
  end

  defp guard(l, bs0, lf, ef) do
    guard0(l, bs0, lf, ef)
  end

  defp guard1([g | gs], bs0, lf, ef) when is_list(g) do
    case (guard0(g, bs0, lf, ef)) do
      true ->
        true
      false ->
        guard1(gs, bs0, lf, ef)
    end
  end

  defp guard1([], _Bs, _Lf, _Ef) do
    false
  end

  defp guard0([g | gs], bs0, lf, ef) do
    case (:erl_lint.is_guard_test(g)) do
      true ->
        case (guard_test(g, bs0, lf, ef)) do
          {:value, true, bs} ->
            guard0(gs, bs, lf, ef)
          {:value, false, _} ->
            false
        end
      false ->
        :erlang.raise(:error, :guard_expr,
                        :erlang.element(2,
                                          :erlang.process_info(self(),
                                                                 :current_stacktrace)))
    end
  end

  defp guard0([], _Bs, _Lf, _Ef) do
    true
  end

  defp guard_test({:call, l, {:atom, ln, f}, as0}, bs0, lf, ef) do
    tT = type_test(f)
    g = {:call, l, {:atom, ln, tT}, as0}
    expr_guard_test(g, bs0, lf, ef)
  end

  defp guard_test({:call, l,
             {:remote, lr, {:atom, lm, :erlang}, {:atom, lf, f}},
             as0},
            bs0, lf, ef) do
    tT = type_test(f)
    g = {:call, l,
           {:remote, lr, {:atom, lm, :erlang}, {:atom, lf, tT}},
           as0}
    expr_guard_test(g, bs0, lf, ef)
  end

  defp guard_test(g, bs0, lf, ef) do
    expr_guard_test(g, bs0, lf, ef)
  end

  defp expr_guard_test(g, bs0, lf, ef) do
    try do
      {:value, true, _} = expr(g, bs0, lf, ef, :none)
    catch
      :error, _ ->
        {:value, false, bs0}
    end
  end

  defp type_test(:integer) do
    :is_integer
  end

  defp type_test(:float) do
    :is_float
  end

  defp type_test(:number) do
    :is_number
  end

  defp type_test(:atom) do
    :is_atom
  end

  defp type_test(:list) do
    :is_list
  end

  defp type_test(:tuple) do
    :is_tuple
  end

  defp type_test(:pid) do
    :is_pid
  end

  defp type_test(:reference) do
    :is_reference
  end

  defp type_test(:port) do
    :is_port
  end

  defp type_test(:function) do
    :is_function
  end

  defp type_test(:binary) do
    :is_binary
  end

  defp type_test(:record) do
    :is_record
  end

  defp type_test(:map) do
    :is_map
  end

  defp type_test(test) do
    test
  end

  defp match(pat, term, bs) do
    match(pat, term, bs, bs)
  end

  defp match(pat, term, bs, bBs) do
    case ((try do
            match1(pat, term, bs, bBs)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end)) do
      :invalid ->
        :erlang.raise(:error, {:illegal_pattern, to_term(pat)},
                        :erlang.element(2,
                                          :erlang.process_info(self(),
                                                                 :current_stacktrace)))
      other ->
        other
    end
  end

  defp string_to_conses([], _, tail) do
    tail
  end

  defp string_to_conses([e | rest], line, tail) do
    {:cons, line, {:integer, line, e},
       string_to_conses(rest, line, tail)}
  end

  defp match1({:atom, _, a0}, a, bs, _BBs) do
    case (a) do
      ^a0 ->
        {:match, bs}
      _ ->
        throw(:nomatch)
    end
  end

  defp match1({:integer, _, i0}, i, bs, _BBs) do
    case (i) do
      ^i0 ->
        {:match, bs}
      _ ->
        throw(:nomatch)
    end
  end

  defp match1({:float, _, f0}, f, bs, _BBs) do
    case (f) do
      ^f0 ->
        {:match, bs}
      _ ->
        throw(:nomatch)
    end
  end

  defp match1({:char, _, c0}, c, bs, _BBs) do
    case (c) do
      ^c0 ->
        {:match, bs}
      _ ->
        throw(:nomatch)
    end
  end

  defp match1({:var, _, :_}, _, bs, _BBs) do
    {:match, bs}
  end

  defp match1({:var, _, name}, term, bs, _BBs) do
    case (binding(name, bs)) do
      {:value, ^term} ->
        {:match, bs}
      {:value, _} ->
        throw(:nomatch)
      :unbound ->
        {:match, add_binding(name, term, bs)}
    end
  end

  defp match1({:match, _, pat1, pat2}, term, bs0, bBs) do
    {:match, bs1} = match1(pat1, term, bs0, bBs)
    match1(pat2, term, bs1, bBs)
  end

  defp match1({:string, _, s0}, s, bs, _BBs) do
    case (s) do
      ^s0 ->
        {:match, bs}
      _ ->
        throw(:nomatch)
    end
  end

  defp match1({nil, _}, nil__, bs, _BBs) do
    case (nil__) do
      [] ->
        {:match, bs}
      _ ->
        throw(:nomatch)
    end
  end

  defp match1({:cons, _, h, t}, [h1 | t1], bs0, bBs) do
    {:match, bs} = match1(h, h1, bs0, bBs)
    match1(t, t1, bs, bBs)
  end

  defp match1({:cons, _, _, _}, _, _Bs, _BBs) do
    throw(:nomatch)
  end

  defp match1({:tuple, _, elts}, tuple, bs, bBs)
      when length(elts) === tuple_size(tuple) do
    match_tuple(elts, tuple, 1, bs, bBs)
  end

  defp match1({:tuple, _, _}, _, _Bs, _BBs) do
    throw(:nomatch)
  end

  defp match1({:map, _, fs}, %{} = map, bs, bBs) do
    match_map(fs, map, bs, bBs)
  end

  defp match1({:map, _, _}, _, _Bs, _BBs) do
    throw(:nomatch)
  end

  defp match1({:bin, _, fs}, <<_ :: bitstring>> = b, bs0,
            bBs) do
    evalFun = fn e, bs ->
                   case (:erl_lint.is_guard_expr(e)) do
                     true ->
                       :ok
                     false ->
                       throw(:invalid)
                   end
                   try do
                     expr(e, bs, :none, :none, :none)
                   catch
                     :error, {:unbound, _} ->
                       throw(:invalid)
                   end
              end
    :eval_bits.match_bits(fs, b, bs0, bBs, match_fun(bBs),
                            evalFun)
  end

  defp match1({:bin, _, _}, _, _Bs, _BBs) do
    throw(:nomatch)
  end

  defp match1({:op, _, :"++", {nil, _}, r}, term, bs, bBs) do
    match1(r, term, bs, bBs)
  end

  defp match1({:op, _, :"++", {:cons, li, {:integer, l2, i}, t},
             r},
            term, bs, bBs) do
    match1({:cons, li, {:integer, l2, i},
              {:op, li, :"++", t, r}},
             term, bs, bBs)
  end

  defp match1({:op, _, :"++", {:cons, li, {:char, l2, c}, t}, r},
            term, bs, bBs) do
    match1({:cons, li, {:char, l2, c}, {:op, li, :"++", t, r}},
             term, bs, bBs)
  end

  defp match1({:op, _, :"++", {:string, li, l}, r}, term, bs,
            bBs) do
    match1(string_to_conses(l, li, r), term, bs, bBs)
  end

  defp match1({:op, line, op, a}, term, bs, bBs) do
    case (partial_eval({:op, line, op, a})) do
      {:op, ^line, ^op, ^a} ->
        throw(:invalid)
      x ->
        match1(x, term, bs, bBs)
    end
  end

  defp match1({:op, line, op, l, r}, term, bs, bBs) do
    case (partial_eval({:op, line, op, l, r})) do
      {:op, ^line, ^op, ^l, ^r} ->
        throw(:invalid)
      x ->
        match1(x, term, bs, bBs)
    end
  end

  defp match1(_, _, _Bs, _BBs) do
    throw(:invalid)
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

  defp match_map([{:map_field_exact, _, k, v} | fs], map, bs0,
            bBs) do
    vm = (try do
            {:value, ke, _} = expr(k, bBs)
            :maps.get(ke, map)
          catch
            :error, _ ->
              throw(:nomatch)
          end)
    {:match, bs} = match1(v, vm, bs0, bBs)
    match_map(fs, map, bs, bBs)
  end

  defp match_map([], _, bs, _) do
    {:match, bs}
  end

  defp match_list(ps, ts, bs) do
    match_list(ps, ts, bs, bs)
  end

  defp match_list([p | ps], [t | ts], bs0, bBs) do
    case (match(p, t, bs0, bBs)) do
      {:match, bs1} ->
        match_list(ps, ts, bs1, bBs)
      :nomatch ->
        :nomatch
    end
  end

  defp match_list([], [], bs, _BBs) do
    {:match, bs}
  end

  defp match_list(_, _, _Bs, _BBs) do
    :nomatch
  end

  def new_bindings() do
    :orddict.new()
  end

  def bindings(bs) do
    :orddict.to_list(bs)
  end

  def binding(name, bs) do
    case (:orddict.find(name, bs)) do
      {:ok, val} ->
        {:value, val}
      :error ->
        :unbound
    end
  end

  def add_binding(name, val, bs) do
    :orddict.store(name, val, bs)
  end

  def del_binding(name, bs) do
    :orddict.erase(name, bs)
  end

  defp add_bindings(bs1, bs2) do
    foldl(fn {name, val}, bs ->
               :orddict.store(name, val, bs)
          end,
            bs2, :orddict.to_list(bs1))
  end

  defp merge_bindings(bs1, bs2) do
    foldl(fn {name, val}, bs ->
               case (:orddict.find(name, bs)) do
                 {:ok, ^val} ->
                   bs
                 {:ok, v1} ->
                   :erlang.raise(:error, {:badmatch, v1},
                                   :erlang.element(2,
                                                     :erlang.process_info(self(),
                                                                            :current_stacktrace)))
                 :error ->
                   :orddict.store(name, val, bs)
               end
          end,
            bs2, :orddict.to_list(bs1))
  end

  defp to_terms(abstrs) do
    for abstr <- abstrs do
      to_term(abstr)
    end
  end

  defp to_term(abstr) do
    :erl_parse.anno_to_term(abstr)
  end

  def subst_values_for_vars({:var, a, v} = var, bs) do
    case (:erl_eval.binding(v, bs)) do
      {:value, value} ->
        {:value, a, value}
      :unbound ->
        var
    end
  end

  def subst_values_for_vars(l, bs) when is_list(l) do
    for e <- l do
      subst_values_for_vars(e, bs)
    end
  end

  def subst_values_for_vars(t, bs) when is_tuple(t) do
    :erlang.list_to_tuple(subst_values_for_vars(:erlang.tuple_to_list(t),
                                                  bs))
  end

  def subst_values_for_vars(t, _Bs) do
    t
  end

  def extended_parse_exprs(tokens) do
    ts = tokens_fixup(tokens)
    case (:erl_parse.parse_exprs(ts)) do
      {:ok, exprs0} ->
        {exprs, bs} = expr_fixup(exprs0)
        {:ok, reset_expr_anno(exprs), bs}
      _ErrorInfo ->
        :erl_parse.parse_exprs(reset_token_anno(ts))
    end
  end

  defp tokens_fixup([]) do
    []
  end

  defp tokens_fixup([t | ts] = ts0) do
    try do
      token_fixup(ts0)
    catch
      _, _ ->
        [t | tokens_fixup(ts)]
    else
      {newT, newTs} ->
        [newT | tokens_fixup(newTs)]
    end
  end

  defp token_fixup(ts) do
    {annoL, newTs, fixupTag} = unscannable(ts)
    string = :lists.append(for a <- annoL do
                             :erl_anno.text(a)
                           end)
    _ = (fixup_fun(fixupTag)).(string)
    newAnno = :erl_anno.set_text(fixup_text(fixupTag),
                                   hd(annoL))
    {{:string, newAnno, string}, newTs}
  end

  defp unscannable([[{:"#", a1}, {:var, a2, :Fun}, {:"<", a3},
              {:atom, a4, _}, {:".", a5}, {:float, a6, _}, {:">", a7}] |
               ts]) do
    {[a1, a2, a3, a4, a5, a6, a7], ts, :function}
  end

  defp unscannable([[{:"#", a1}, {:var, a2, :Fun}, {:"<", a3},
              {:atom, a4, _}, {:".", a5}, {:atom, a6, _}, {:".", a7},
              {:integer, a8, _}, {:">", a9}] |
               ts]) do
    {[a1, a2, a3, a4, a5, a6, a7, a8, a9], ts, :function}
  end

  defp unscannable([[{:"<", a1}, {:float, a2, _}, {:".", a3},
              {:integer, a4, _}, {:">", a5}] |
               ts]) do
    {[a1, a2, a3, a4, a5], ts, :pid}
  end

  defp unscannable([[{:"#", a1}, {:var, a2, :Port}, {:"<", a3},
              {:float, a4, _}, {:">", a5}] |
               ts]) do
    {[a1, a2, a3, a4, a5], ts, :port}
  end

  defp unscannable([[{:"#", a1}, {:var, a2, :Ref}, {:"<", a3},
              {:float, a4, _}, {:".", a5}, {:float, a6, _}, {:">", a7}] |
               ts]) do
    {[a1, a2, a3, a4, a5, a6, a7], ts, :reference}
  end

  defp expr_fixup(expr0) do
    {expr, bs, _} = expr_fixup(expr0,
                                 :erl_eval.new_bindings(), 1)
    {expr, bs}
  end

  defp expr_fixup({:string, a, s} = t, bs0, i) do
    try do
      string_fixup(a, s)
    catch
      _, _ ->
        {t, bs0, i}
    else
      value ->
        var = new_var(i)
        bs = :erl_eval.add_binding(var, value, bs0)
        {{:var, a, var}, bs, i + 1}
    end
  end

  defp expr_fixup(tuple, bs0, i0) when is_tuple(tuple) do
    {l, bs, i} = expr_fixup(:erlang.tuple_to_list(tuple),
                              bs0, i0)
    {:erlang.list_to_tuple(l), bs, i}
  end

  defp expr_fixup([e0 | es0], bs0, i0) do
    {e, bs1, i1} = expr_fixup(e0, bs0, i0)
    {es, bs, i} = expr_fixup(es0, bs1, i1)
    {[e | es], bs, i}
  end

  defp expr_fixup(t, bs, i) do
    {t, bs, i}
  end

  defp string_fixup(a, s) do
    text = :erl_anno.text(a)
    fixupTag = fixup_tag(text, s)
    (fixup_fun(fixupTag)).(s)
  end

  defp new_var(i) do
    :erlang.list_to_atom(:lists.concat([:__ExtendedParseExprs_,
                                          i, :__]))
  end

  defp reset_token_anno(tokens) do
    for t <- tokens do
      :erlang.setelement(2, t,
                           (reset_anno()).(:erlang.element(2, t)))
    end
  end

  defp reset_expr_anno(exprs) do
    for e <- exprs do
      :erl_parse.map_anno(reset_anno(), e)
    end
  end

  defp reset_anno() do
    fn a ->
         :erl_anno.new(:erl_anno.location(a))
    end
  end

  defp fixup_fun(:function) do
    &function/1
  end

  defp fixup_fun(:pid) do
    &:erlang.list_to_pid/1
  end

  defp fixup_fun(:port) do
    &:erlang.list_to_port/1
  end

  defp fixup_fun(:reference) do
    &:erlang.list_to_ref/1
  end

  defp function(s) do
    {:ok,
       [[_, _, _, {:atom, _, module}, _, {:atom, _, function},
           _, {:integer, _, arity}] |
            _],
       _} = :erl_scan.string(s)
    :erlang.make_fun(module, function, arity)
  end

  defp fixup_text(:function) do
    'function'
  end

  defp fixup_text(:pid) do
    'pid'
  end

  defp fixup_text(:port) do
    'port'
  end

  defp fixup_text(:reference) do
    'reference'
  end

  defp fixup_tag('function', '#' ++ _) do
    :function
  end

  defp fixup_tag('pid', '<' ++ _) do
    :pid
  end

  defp fixup_tag('port', '#' ++ _) do
    :port
  end

  defp fixup_tag('reference', '#' ++ _) do
    :reference
  end

  def extended_parse_term(tokens) do
    case (extended_parse_exprs(tokens)) do
      {:ok, [expr], bindings} ->
        try do
          normalise(expr, bindings)
        catch
          _, _ ->
            loc = :erl_anno.location(:erlang.element(2, expr))
            {:error, {loc, :erl_eval, 'bad term'}}
        else
          term ->
            {:ok, term}
        end
      {:ok, [[_, expr] | _], _Bindings} ->
        loc = :erl_anno.location(:erlang.element(2, expr))
        {:error, {loc, :erl_eval, 'bad term'}}
      {:error, _} = error ->
        error
    end
  end

  defp normalise({:var, _, v}, bs) do
    {:value, value} = :erl_eval.binding(v, bs)
    value
  end

  defp normalise({:char, _, c}, _Bs) do
    c
  end

  defp normalise({:integer, _, i}, _Bs) do
    i
  end

  defp normalise({:float, _, f}, _Bs) do
    f
  end

  defp normalise({:atom, _, a}, _Bs) do
    a
  end

  defp normalise({:string, _, s}, _Bs) do
    s
  end

  defp normalise({nil, _}, _Bs) do
    []
  end

  defp normalise({:bin, _, fs}, bs) do
    {:value, b, _} = :eval_bits.expr_grp(fs, [],
                                           fn e, _ ->
                                                {:value, normalise(e, bs), []}
                                           end,
                                           [], true)
    b
  end

  defp normalise({:cons, _, head, tail}, bs) do
    [normalise(head, bs) | normalise(tail, bs)]
  end

  defp normalise({:tuple, _, args}, bs) do
    :erlang.list_to_tuple(normalise_list(args, bs))
  end

  defp normalise({:map, _, pairs}, bs) do
    :maps.from_list(:lists.map(fn {:map_field_assoc, _, k,
                                     v} ->
                                    {normalise(k, bs), normalise(v, bs)}
                               end,
                                 pairs))
  end

  defp normalise({:op, _, :"+", {:char, _, i}}, _Bs) do
    i
  end

  defp normalise({:op, _, :"+", {:integer, _, i}}, _Bs) do
    i
  end

  defp normalise({:op, _, :"+", {:float, _, f}}, _Bs) do
    f
  end

  defp normalise({:op, _, :"-", {:char, _, i}}, _Bs) do
    - i
  end

  defp normalise({:op, _, :"-", {:integer, _, i}}, _Bs) do
    - i
  end

  defp normalise({:op, _, :"-", {:float, _, f}}, _Bs) do
    - f
  end

  defp normalise({:fun, _,
             {:function, {:atom, _, m}, {:atom, _, f},
                {:integer, _, a}}},
            _Bs) do
    Function.capture(m, f, a)
  end

  defp normalise_list([h | t], bs) do
    [normalise(h, bs) | normalise_list(t, bs)]
  end

  defp normalise_list([], _Bs) do
    []
  end

  def is_constant_expr(expr) do
    case (eval_expr(expr)) do
      {:ok, x} when is_number(x) ->
        true
      _ ->
        false
    end
  end

  defp eval_expr(expr) do
    case ((try do
            ev_expr(expr)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end)) do
      x when is_integer(x) ->
        {:ok, x}
      x when is_float(x) ->
        {:ok, x}
      x when is_atom(x) ->
        {:ok, x}
      {:EXIT, reason} ->
        {:error, reason}
      _ ->
        {:error, :badarg}
    end
  end

  def partial_eval(expr) do
    line = line(expr)
    case ((try do
            ev_expr(expr)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end)) do
      x when is_integer(x) ->
        ret_expr(expr, {:integer, line, x})
      x when is_float(x) ->
        ret_expr(expr, {:float, line, x})
      x when is_atom(x) ->
        ret_expr(expr, {:atom, line, x})
      _ ->
        expr
    end
  end

  defp ev_expr({:op, _, op, l, r}) do
    apply(:erlang, op, [ev_expr(l), ev_expr(r)])
  end

  defp ev_expr({:op, _, op, a}) do
    apply(:erlang, op, [ev_expr(a)])
  end

  defp ev_expr({:integer, _, x}) do
    x
  end

  defp ev_expr({:char, _, x}) do
    x
  end

  defp ev_expr({:float, _, x}) do
    x
  end

  defp ev_expr({:atom, _, x}) do
    x
  end

  defp ev_expr({:tuple, _, es}) do
    :erlang.list_to_tuple(for x <- es do
                            ev_expr(x)
                          end)
  end

  defp ev_expr({nil, _}) do
    []
  end

  defp ev_expr({:cons, _, h, t}) do
    [ev_expr(h) | ev_expr(t)]
  end

  def eval_str(str) when is_list(str) do
    case (:erl_scan.tokens([], str, 0)) do
      {:more, _} ->
        {:error, 'Incomplete form (missing .<cr>)??'}
      {:done, {:ok, toks, _}, rest} ->
        case (all_white(rest)) do
          true ->
            case (:erl_parse.parse_exprs(toks)) do
              {:ok, exprs} ->
                case ((try do
                        :erl_eval.exprs(exprs, :erl_eval.new_bindings())
                      catch
                        :error, e -> {:EXIT, {e, __STACKTRACE__}}
                        :exit, e -> {:EXIT, e}
                        e -> e
                      end)) do
                  {:value, val, _} ->
                    {:ok, val}
                  other ->
                    {:error, :lists.flatten(:io_lib.format('*** eval: ~p', [other]))}
                end
              {:error, {_Line, mod, args}} ->
                msg = :lists.flatten(:io_lib.format('*** ~ts',
                                                      [mod.format_error(args)]))
                {:error, msg}
            end
          false ->
            {:error, :lists.flatten(:io_lib.format('Non-white space found after end-of-form :~ts', [rest]))}
        end
    end
  end

  def eval_str(bin) when is_binary(bin) do
    eval_str(:erlang.binary_to_list(bin))
  end

  defp all_white([?\s | t]) do
    all_white(t)
  end

  defp all_white([?\n | t]) do
    all_white(t)
  end

  defp all_white([?\t | t]) do
    all_white(t)
  end

  defp all_white([]) do
    true
  end

  defp all_white(_) do
    false
  end

  defp ret_expr(_Old, new) do
    new
  end

  defp line(expr) do
    :erlang.element(2, expr)
  end

end