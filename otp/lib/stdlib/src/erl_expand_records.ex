defmodule :m_erl_expand_records do
  use Bitwise
  import :lists, only: [duplicate: 2, foldl: 3, foldr: 3, map: 2, reverse: 1, sort: 1]
  require Record

  Record.defrecord(:r_exprec, :exprec,
    compile: [],
    vcount: 0,
    calltype: %{},
    records: %{},
    strict_ra: [],
    checked_ra: []
  )

  def module(fs0, opts0) do
    opts = compiler_options(fs0) ++ opts0
    calltype = init_calltype(fs0)
    st0 = r_exprec(compile: opts, calltype: calltype)
    {fs, _St} = forms(fs0, st0)
    fs
  end

  defp compiler_options(forms) do
    :lists.flatten(
      for {:attribute, _, :compile, c} <- forms do
        c
      end
    )
  end

  defp init_calltype(forms) do
    locals =
      for {:function, _, name, arity, _} <- forms do
        {{name, arity}, :local}
      end

    ctype = :maps.from_list(locals)
    init_calltype_imports(forms, ctype)
  end

  defp init_calltype_imports(
         [{:attribute, _, :import, {mod, fs}} | t],
         ctype0
       ) do
    true = is_atom(mod)

    ctype =
      foldl(
        fn fA, acc ->
          %{acc | fA => {:imported, mod}}
        end,
        ctype0,
        fs
      )

    init_calltype_imports(t, ctype)
  end

  defp init_calltype_imports([_ | t], ctype) do
    init_calltype_imports(t, ctype)
  end

  defp init_calltype_imports([], ctype) do
    ctype
  end

  defp forms(
         [
           {:attribute, _, :record, {name, defs}} = attr
           | fs
         ],
         st0
       ) do
    nDefs = normalise_fields(defs)
    st = r_exprec(st0, records: :maps.put(name, nDefs, r_exprec(st0, :records)))
    {fs1, st1} = forms(fs, st)
    {[attr | fs1], st1}
  end

  defp forms([{:function, l, n, a, cs0} | fs0], st0) do
    {cs, st1} = clauses(cs0, st0)
    {fs, st2} = forms(fs0, st1)
    {[{:function, l, n, a, cs} | fs], st2}
  end

  defp forms([f | fs0], st0) do
    {fs, st} = forms(fs0, st0)
    {[f | fs], st}
  end

  defp forms([], st) do
    {[], st}
  end

  defp clauses([{:clause, line, h0, g0, b0} | cs0], st0) do
    {h1, st1} = head(h0, st0)
    {g1, st2} = guard(g0, st1)
    {h, g} = optimize_is_record(h1, g1, st2)
    {b, st3} = exprs(b0, st2)
    {cs, st4} = clauses(cs0, st3)
    {[{:clause, line, h, g, b} | cs], st4}
  end

  defp clauses([], st) do
    {[], st}
  end

  defp head(as, st) do
    pattern_list(as, st)
  end

  defp pattern({:var, _, :_} = var, st) do
    {var, st}
  end

  defp pattern({:var, _, _} = var, st) do
    {var, st}
  end

  defp pattern({:char, _, _} = char, st) do
    {char, st}
  end

  defp pattern({:integer, _, _} = int, st) do
    {int, st}
  end

  defp pattern({:float, _, _} = float, st) do
    {float, st}
  end

  defp pattern({:atom, _, _} = atom, st) do
    {atom, st}
  end

  defp pattern({:string, _, _} = string, st) do
    {string, st}
  end

  defp pattern({nil, _} = nil__, st) do
    {nil__, st}
  end

  defp pattern({:cons, line, h, t}, st0) do
    {tH, st1} = pattern(h, st0)
    {tT, st2} = pattern(t, st1)
    {{:cons, line, tH, tT}, st2}
  end

  defp pattern({:tuple, line, ps}, st0) do
    {tPs, st1} = pattern_list(ps, st0)
    {{:tuple, line, tPs}, st1}
  end

  defp pattern({:map, line, ps}, st0) do
    {tPs, st1} = pattern_list(ps, st0)
    {{:map, line, tPs}, st1}
  end

  defp pattern({:map_field_exact, line, k0, v0}, st0) do
    {k, st1} = expr(k0, st0)
    {v, st2} = pattern(v0, st1)
    {{:map_field_exact, line, k, v}, st2}
  end

  defp pattern({:record_index, line, name, field}, st) do
    {index_expr(line, field, name, record_fields(name, st)), st}
  end

  defp pattern({:record, line0, name, pfs}, st0) do
    fs = record_fields(name, st0)
    {tMs, st1} = pattern_list(pattern_fields(fs, pfs), st0)
    line = mark_record(line0, st1)
    {{:tuple, line, [{:atom, line0, name} | tMs]}, st1}
  end

  defp pattern({:bin, line, es0}, st0) do
    {es1, st1} = pattern_bin(es0, st0)
    {{:bin, line, es1}, st1}
  end

  defp pattern({:match, line, pat1, pat2}, st0) do
    {tH, st1} = pattern(pat2, st0)
    {tT, st2} = pattern(pat1, st1)
    {{:match, line, tT, tH}, st2}
  end

  defp pattern({:op, line, op, a0}, st0) do
    {a, st1} = pattern(a0, st0)
    {{:op, line, op, a}, st1}
  end

  defp pattern({:op, line, op, l0, r0}, st0) do
    {l, st1} = pattern(l0, st0)
    {r, st2} = pattern(r0, st1)
    {{:op, line, op, l, r}, st2}
  end

  defp pattern_list([p0 | ps0], st0) do
    {p, st1} = pattern(p0, st0)
    {ps, st2} = pattern_list(ps0, st1)
    {[p | ps], st2}
  end

  defp pattern_list([], st) do
    {[], st}
  end

  defp guard([g0 | gs0], st0) do
    {g, st1} = guard_tests(g0, st0)
    {gs, st2} = guard(gs0, st1)
    {[g | gs], st2}
  end

  defp guard([], st) do
    {[], st}
  end

  defp guard_tests(gts0, st0) do
    {gts1, st1} = guard_tests1(gts0, st0)
    {gts1, r_exprec(st1, checked_ra: [])}
  end

  defp guard_tests1([gt0 | gts0], st0) do
    {gt1, st1} = guard_test(gt0, st0)
    {gts1, st2} = guard_tests1(gts0, st1)
    {[gt1 | gts1], st2}
  end

  defp guard_tests1([], st) do
    {[], st}
  end

  defp guard_test(g0, st0) do
    in_guard(fn ->
      {g1, st1} = guard_test1(g0, st0)
      strict_record_access(g1, st1)
    end)
  end

  defp guard_test1({:call, line, {:atom, lt, tname}, as}, st) do
    test = {:atom, lt, normalise_test(tname, length(as))}
    expr({:call, line, test, as}, st)
  end

  defp guard_test1(test, st) do
    expr(test, st)
  end

  defp normalise_test(:atom, 1) do
    :is_atom
  end

  defp normalise_test(:binary, 1) do
    :is_binary
  end

  defp normalise_test(:float, 1) do
    :is_float
  end

  defp normalise_test(:function, 1) do
    :is_function
  end

  defp normalise_test(:integer, 1) do
    :is_integer
  end

  defp normalise_test(:list, 1) do
    :is_list
  end

  defp normalise_test(:number, 1) do
    :is_number
  end

  defp normalise_test(:pid, 1) do
    :is_pid
  end

  defp normalise_test(:port, 1) do
    :is_port
  end

  defp normalise_test(:record, 2) do
    :is_record
  end

  defp normalise_test(:reference, 1) do
    :is_reference
  end

  defp normalise_test(:tuple, 1) do
    :is_tuple
  end

  defp normalise_test(name, _) do
    name
  end

  defp is_in_guard() do
    :erlang.get(:erl_expand_records_in_guard) !== :undefined
  end

  defp in_guard(f) do
    :undefined =
      :erlang.put(
        :erl_expand_records_in_guard,
        true
      )

    res = f.()
    true = :erlang.erase(:erl_expand_records_in_guard)
    res
  end

  defp record_test(line, term, name, st) do
    case is_in_guard() do
      false ->
        record_test_in_body(line, term, name, st)

      true ->
        record_test_in_guard(line, term, name, st)
    end
  end

  defp record_test_in_guard(line, term, name, st) do
    case not_a_tuple(term) do
      true ->
        expr({:atom, line, false}, st)

      false ->
        fs = record_fields(name, st)
        nLine = no_compiler_warning(line)

        expr(
          {:call, nLine, {:remote, nLine, {:atom, nLine, :erlang}, {:atom, nLine, :is_record}},
           [term, {:atom, line, name}, {:integer, line, length(fs) + 1}]},
          st
        )
    end
  end

  defp not_a_tuple({:atom, _, _}) do
    true
  end

  defp not_a_tuple({:integer, _, _}) do
    true
  end

  defp not_a_tuple({:float, _, _}) do
    true
  end

  defp not_a_tuple({nil, _}) do
    true
  end

  defp not_a_tuple({:cons, _, _, _}) do
    true
  end

  defp not_a_tuple({:char, _, _}) do
    true
  end

  defp not_a_tuple({:string, _, _}) do
    true
  end

  defp not_a_tuple({:record_index, _, _, _}) do
    true
  end

  defp not_a_tuple({:bin, _, _}) do
    true
  end

  defp not_a_tuple({:op, _, _, _}) do
    true
  end

  defp not_a_tuple({:op, _, _, _, _}) do
    true
  end

  defp not_a_tuple(_) do
    false
  end

  defp record_test_in_body(line, expr, name, st0) do
    fs = record_fields(name, st0)
    {var, st} = new_var(line, st0)
    nLine = no_compiler_warning(line)

    expr(
      {:block, line,
       [
         {:match, line, var, expr},
         {:call, nLine, {:remote, nLine, {:atom, nLine, :erlang}, {:atom, nLine, :is_record}},
          [var, {:atom, line, name}, {:integer, line, length(fs) + 1}]}
       ]},
      st
    )
  end

  defp exprs([e0 | es0], st0) do
    {e, st1} = expr(e0, st0)
    {es, st2} = exprs(es0, st1)
    {[e | es], st2}
  end

  defp exprs([], st) do
    {[], st}
  end

  defp expr({:var, _, _} = var, st) do
    {var, st}
  end

  defp expr({:char, _, _} = char, st) do
    {char, st}
  end

  defp expr({:integer, _, _} = int, st) do
    {int, st}
  end

  defp expr({:float, _, _} = float, st) do
    {float, st}
  end

  defp expr({:atom, _, _} = atom, st) do
    {atom, st}
  end

  defp expr({:string, _, _} = string, st) do
    {string, st}
  end

  defp expr({nil, _} = nil__, st) do
    {nil__, st}
  end

  defp expr({:cons, line, h0, t0}, st0) do
    {h, st1} = expr(h0, st0)
    {t, st2} = expr(t0, st1)
    {{:cons, line, h, t}, st2}
  end

  defp expr({:lc, line, e0, qs0}, st0) do
    {qs1, st1} = lc_tq(line, qs0, st0)
    {e1, st2} = expr(e0, st1)
    {{:lc, line, e1, qs1}, st2}
  end

  defp expr({:bc, line, e0, qs0}, st0) do
    {qs1, st1} = lc_tq(line, qs0, st0)
    {e1, st2} = expr(e0, st1)
    {{:bc, line, e1, qs1}, st2}
  end

  defp expr({:tuple, line, es0}, st0) do
    {es1, st1} = expr_list(es0, st0)
    {{:tuple, line, es1}, st1}
  end

  defp expr({:map, line, es0}, st0) do
    {es1, st1} = expr_list(es0, st0)
    {{:map, line, es1}, st1}
  end

  defp expr({:map, line, arg0, es0}, st0) do
    {arg1, st1} = expr(arg0, st0)
    {es1, st2} = expr_list(es0, st1)
    {{:map, line, arg1, es1}, st2}
  end

  defp expr({:map_field_assoc, line, k0, v0}, st0) do
    {k, st1} = expr(k0, st0)
    {v, st2} = expr(v0, st1)
    {{:map_field_assoc, line, k, v}, st2}
  end

  defp expr({:map_field_exact, line, k0, v0}, st0) do
    {k, st1} = expr(k0, st0)
    {v, st2} = expr(v0, st1)
    {{:map_field_exact, line, k, v}, st2}
  end

  defp expr({:record_index, line, name, f}, st) do
    i = index_expr(line, f, name, record_fields(name, st))
    expr(i, st)
  end

  defp expr({:record, line0, name, is}, st) do
    line = mark_record(line0, st)

    expr(
      {:tuple, line,
       [
         {:atom, line0, name}
         | record_inits(
             record_fields(
               name,
               st
             ),
             is
           )
       ]},
      st
    )
  end

  defp expr({:record_field, line, r, name, f}, st) do
    get_record_field(line, r, f, name, st)
  end

  defp expr({:record, _, r, name, us}, st0) do
    {ue, st1} = record_update(r, name, record_fields(name, st0), us, st0)
    expr(ue, st1)
  end

  defp expr({:bin, line, es0}, st0) do
    {es1, st1} = expr_bin(es0, st0)
    {{:bin, line, es1}, st1}
  end

  defp expr({:block, line, es0}, st0) do
    {es, st1} = exprs(es0, st0)
    {{:block, line, es}, st1}
  end

  defp expr({:if, line, cs0}, st0) do
    {cs, st1} = clauses(cs0, st0)
    {{:if, line, cs}, st1}
  end

  defp expr({:case, line, e0, cs0}, st0) do
    {e, st1} = expr(e0, st0)
    {cs, st2} = clauses(cs0, st1)
    {{:case, line, e, cs}, st2}
  end

  defp expr({:receive, line, cs0}, st0) do
    {cs, st1} = clauses(cs0, st0)
    {{:receive, line, cs}, st1}
  end

  defp expr({:receive, line, cs0, to0, toEs0}, st0) do
    {to, st1} = expr(to0, st0)
    {toEs, st2} = exprs(toEs0, st1)
    {cs, st3} = clauses(cs0, st2)
    {{:receive, line, cs, to, toEs}, st3}
  end

  defp expr({:fun, lf, {:function, f, a}} = fun0, st0) do
    case :erl_internal.bif(f, a) do
      true ->
        {as, st1} = new_vars(a, lf, st0)
        cs = [{:clause, lf, as, [], [{:call, lf, {:atom, lf, f}, as}]}]
        fun = {:fun, lf, {:clauses, cs}}
        expr(fun, st1)

      false ->
        {fun0, st0}
    end
  end

  defp expr({:fun, _, {:function, _M, _F, _A}} = fun, st) do
    {fun, st}
  end

  defp expr({:fun, line, {:clauses, cs0}}, st0) do
    {cs, st1} = clauses(cs0, st0)
    {{:fun, line, {:clauses, cs}}, st1}
  end

  defp expr({:named_fun, line, name, cs0}, st0) do
    {cs, st1} = clauses(cs0, st0)
    {{:named_fun, line, name, cs}, st1}
  end

  defp expr(
         {:call, line, {:atom, _, :is_record}, [a, {:atom, _, name}]},
         st
       ) do
    record_test(line, a, name, st)
  end

  defp expr(
         {:call, line, {:remote, _, {:atom, _, :erlang}, {:atom, _, :is_record}},
          [a, {:atom, _, name}]},
         st
       ) do
    record_test(line, a, name, st)
  end

  defp expr(
         {:call, line, {:tuple, _, [{:atom, _, :erlang}, {:atom, _, :is_record}]},
          [a, {:atom, _, name}]},
         st
       ) do
    record_test(line, a, name, st)
  end

  defp expr(
         {:call, line, {:atom, _La, :record_info}, [_, _] = as0},
         st0
       ) do
    {as, st1} = expr_list(as0, st0)
    record_info_call(line, as, st1)
  end

  defp expr(
         {:call, line, {:atom, _La, n} = atom, as0},
         st0
       ) do
    {as, st1} = expr_list(as0, st0)
    ar = length(as)
    nA = {n, ar}

    case r_exprec(st0, :calltype) do
      %{^nA => :local} ->
        {{:call, line, atom, as}, st1}

      %{^nA => {:imported, module}} ->
        modAtom = {:atom, line, module}
        {{:call, line, {:remote, line, modAtom, atom}, as}, st1}

      _ ->
        case :erl_internal.bif(n, ar) do
          true ->
            modAtom = {:atom, line, :erlang}
            {{:call, line, {:remote, line, modAtom, atom}, as}, st1}

          false ->
            {{:call, line, atom, as}, st1}
        end
    end
  end

  defp expr({:call, line, {:remote, lr, m, f}, as0}, st0) do
    {[[m1, f1] | as1], st1} = expr_list([[m, f] | as0], st0)
    {{:call, line, {:remote, lr, m1, f1}, as1}, st1}
  end

  defp expr({:call, line, f, as0}, st0) do
    {[fun1 | as1], st1} = expr_list([f | as0], st0)
    {{:call, line, fun1, as1}, st1}
  end

  defp expr({:try, line, es0, scs0, ccs0, as0}, st0) do
    {es1, st1} = exprs(es0, st0)
    {scs1, st2} = clauses(scs0, st1)
    {ccs1, st3} = clauses(ccs0, st2)
    {as1, st4} = exprs(as0, st3)
    {{:try, line, es1, scs1, ccs1, as1}, st4}
  end

  defp expr({:catch, line, e0}, st0) do
    {e, st1} = expr(e0, st0)
    {{:catch, line, e}, st1}
  end

  defp expr({:match, line, p0, e0}, st0) do
    {e, st1} = expr(e0, st0)
    {p, st2} = pattern(p0, st1)
    {{:match, line, p, e}, st2}
  end

  defp expr({:op, line, :not, a0}, st0) do
    {a, st1} = bool_operand(a0, st0)
    {{:op, line, :not, a}, st1}
  end

  defp expr({:op, line, op, a0}, st0) do
    {a, st1} = expr(a0, st0)
    {{:op, line, op, a}, st1}
  end

  defp expr({:op, line, op, l0, r0}, st0)
       when op === :and or op === :or do
    {l, st1} = bool_operand(l0, st0)
    {r, st2} = bool_operand(r0, st1)
    {{:op, line, op, l, r}, st2}
  end

  defp expr({:op, line, op, l0, r0}, st0)
       when op === :andalso or op === :orelse do
    {l, st1} = bool_operand(l0, st0)
    {r, st2} = bool_operand(r0, st1)
    {{:op, line, op, l, r}, r_exprec(st2, checked_ra: r_exprec(st1, :checked_ra))}
  end

  defp expr({:op, line, op, l0, r0}, st0) do
    {l, st1} = expr(l0, st0)
    {r, st2} = expr(r0, st1)
    {{:op, line, op, l, r}, st2}
  end

  defp expr_list([e0 | es0], st0) do
    {e, st1} = expr(e0, st0)
    {es, st2} = expr_list(es0, st1)
    {[e | es], st2}
  end

  defp expr_list([], st) do
    {[], st}
  end

  defp bool_operand(e0, st0) do
    {e1, st1} = expr(e0, st0)
    strict_record_access(e1, st1)
  end

  defp strict_record_access(e, r_exprec(strict_ra: []) = st) do
    {e, st}
  end

  defp strict_record_access(e0, st0) do
    r_exprec(strict_ra: strictRA, checked_ra: checkedRA) = st0

    {new, nC} =
      :lists.foldl(
        fn {key, _L, _R, _Sz} = a, {l, c} ->
          case :lists.keymember(key, 1, c) do
            true ->
              {l, c}

            false ->
              {[a | l], [a | c]}
          end
        end,
        {[], checkedRA},
        strictRA
      )

    e1 =
      cond do
        new === [] ->
          e0

        true ->
          conj(new, e0)
      end

    st1 = r_exprec(st0, strict_ra: [], checked_ra: nC)
    expr(e1, st1)
  end

  defp conj([], _E) do
    :empty
  end

  defp conj([{{name, _Rp}, l, r, sz} | aL], e) do
    nL = no_compiler_warning(l)

    t1 =
      {:op, nL, :orelse,
       {:call, nL, {:remote, nL, {:atom, nL, :erlang}, {:atom, nL, :is_record}},
        [r, {:atom, nL, name}, {:integer, nL, sz}]}, {:atom, nL, :fail}}

    t2 =
      case conj(aL, :none) do
        :empty ->
          t1

        c ->
          {:op, nL, :and, c, t1}
      end

    case e do
      :none ->
        case t2 do
          {:op, _, :and, _, _} ->
            t2

          _ ->
            {:op, nL, :and, t2, {:atom, nL, true}}
        end

      _ ->
        {:op, nL, :and, t2, e}
    end
  end

  defp lc_tq(line, [{:generate, lg, p0, g0} | qs0], st0) do
    {g1, st1} = expr(g0, st0)
    {p1, st2} = pattern(p0, st1)
    {qs1, st3} = lc_tq(line, qs0, st2)
    {[{:generate, lg, p1, g1} | qs1], st3}
  end

  defp lc_tq(line, [{:b_generate, lg, p0, g0} | qs0], st0) do
    {g1, st1} = expr(g0, st0)
    {p1, st2} = pattern(p0, st1)
    {qs1, st3} = lc_tq(line, qs0, st2)
    {[{:b_generate, lg, p1, g1} | qs1], st3}
  end

  defp lc_tq(line, [f0 | qs0], r_exprec(calltype: calltype) = st0) do
    isOverriden = fn fA ->
      case calltype do
        %{^fA => :local} ->
          true

        %{^fA => {:imported, _}} ->
          true

        _ ->
          false
      end
    end

    case :erl_lint.is_guard_test(f0, [], isOverriden) do
      true ->
        {f1, st1} = guard_test(f0, st0)
        {qs1, st2} = lc_tq(line, qs0, st1)
        {[f1 | qs1], st2}

      false ->
        {f1, st1} = expr(f0, st0)
        {qs1, st2} = lc_tq(line, qs0, st1)
        {[f1 | qs1], st2}
    end
  end

  defp lc_tq(_Line, [], st0) do
    {[], r_exprec(st0, checked_ra: [])}
  end

  defp normalise_fields(fs) do
    map(
      fn
        {:record_field, lf, field} ->
          {:record_field, lf, field, {:atom, lf, :undefined}}

        {:typed_record_field, {:record_field, lf, field}, _Type} ->
          {:record_field, lf, field, {:atom, lf, :undefined}}

        {:typed_record_field, field, _Type} ->
          field

        f ->
          f
      end,
      fs
    )
  end

  defp record_fields(r, st) do
    :maps.get(r, r_exprec(st, :records))
  end

  defp find_field(
         f,
         [{:record_field, _, {:atom, _, f}, val} | _]
       ) do
    {:ok, val}
  end

  defp find_field(f, [_ | fs]) do
    find_field(f, fs)
  end

  defp find_field(_, []) do
    :error
  end

  defp field_names(fs) do
    map(
      fn {:record_field, _, field, _Val} ->
        field
      end,
      fs
    )
  end

  defp index_expr(line, {:atom, _, f}, _Name, fs) do
    {:integer, line, index_expr(f, fs, 2)}
  end

  defp index_expr(f, [{:record_field, _, {:atom, _, f}, _} | _], i) do
    i
  end

  defp index_expr(f, [_ | fs], i) do
    index_expr(f, fs, i + 1)
  end

  defp get_record_field(line, r, index, name, st) do
    case strict_record_tests(r_exprec(st, :compile)) do
      false ->
        sloppy_get_record_field(line, r, index, name, st)

      true ->
        strict_get_record_field(line, r, index, name, st)
    end
  end

  defp strict_get_record_field(line, r, {:atom, _, f} = index, name, st0) do
    case is_in_guard() do
      false ->
        {var, st} = new_var(line, st0)
        fs = record_fields(name, st)
        i = index_expr(f, fs, 2)
        p = record_pattern(2, i, var, length(fs) + 1, line, [{:atom, line, name}])
        nLine = no_compiler_warning(line)
        rLine = mark_record(nLine, st)

        e =
          {:case, nLine, r,
           [
             {:clause, nLine, [{:tuple, rLine, p}], [], [var]},
             {:clause, nLine, [{:var, nLine, :_}], [],
              [
                {:call, nLine, {:remote, nLine, {:atom, nLine, :erlang}, {:atom, nLine, :error}},
                 [{:tuple, nLine, [{:atom, nLine, :badrecord}, {:atom, nLine, name}]}]}
              ]}
           ]}

        expr(e, st)

      true ->
        fs = record_fields(name, st0)
        i = index_expr(line, index, name, fs)
        {expR, st1} = expr(r, st0)
        a0 = :erl_anno.new(0)

        expRp =
          :erl_parse.map_anno(
            fn _A ->
              a0
            end,
            expR
          )

        rA = {{name, expRp}, line, expR, length(fs) + 1}
        st2 = r_exprec(st1, strict_ra: [rA | r_exprec(st1, :strict_ra)])

        {{:call, line, {:remote, line, {:atom, line, :erlang}, {:atom, line, :element}},
          [i, expR]}, st2}
    end
  end

  defp record_pattern(i, i, var, sz, line, acc) do
    record_pattern(i + 1, i, var, sz, line, [var | acc])
  end

  defp record_pattern(cur, i, var, sz, line, acc) when cur <= sz do
    record_pattern(cur + 1, i, var, sz, line, [{:var, line, :_} | acc])
  end

  defp record_pattern(_, _, _, _, _, acc) do
    reverse(acc)
  end

  defp sloppy_get_record_field(line, r, index, name, st) do
    fs = record_fields(name, st)
    i = index_expr(line, index, name, fs)

    expr(
      {:call, line, {:remote, line, {:atom, line, :erlang}, {:atom, line, :element}}, [i, r]},
      st
    )
  end

  defp strict_record_tests([:strict_record_tests | _]) do
    true
  end

  defp strict_record_tests([:no_strict_record_tests | _]) do
    false
  end

  defp strict_record_tests([_ | os]) do
    strict_record_tests(os)
  end

  defp strict_record_tests([]) do
    true
  end

  defp strict_record_updates([:strict_record_updates | _]) do
    true
  end

  defp strict_record_updates([:no_strict_record_updates | _]) do
    false
  end

  defp strict_record_updates([_ | os]) do
    strict_record_updates(os)
  end

  defp strict_record_updates([]) do
    false
  end

  defp pattern_fields(fs, ms) do
    wildcard = record_wildcard_init(ms)

    map(
      fn {:record_field, l, {:atom, _, f}, _} ->
        case find_field(f, ms) do
          {:ok, match} ->
            match

          :error when wildcard === :none ->
            {:var, l, :_}

          :error ->
            wildcard
        end
      end,
      fs
    )
  end

  defp record_inits(fs, is) do
    wildcardInit = record_wildcard_init(is)

    map(
      fn {:record_field, _, {:atom, _, f}, d} ->
        case find_field(f, is) do
          {:ok, init} ->
            init

          :error when wildcardInit === :none ->
            d

          :error ->
            wildcardInit
        end
      end,
      fs
    )
  end

  defp record_wildcard_init([{:record_field, _, {:var, _, :_}, d} | _]) do
    d
  end

  defp record_wildcard_init([_ | is]) do
    record_wildcard_init(is)
  end

  defp record_wildcard_init([]) do
    :none
  end

  defp record_update(r, name, fs, us0, st0) do
    line = :erlang.element(2, r)
    {pre, us, st1} = record_exprs(us0, st0)
    nf = length(fs)
    nu = length(us)
    nc = nf - nu
    {var, st2} = new_var(line, st1)
    strictUpdates = strict_record_updates(r_exprec(st2, :compile))

    {update, st} =
      cond do
        nu === 0 ->
          record_match(var, name, line, fs, us, st2)

        nu <= nc and not strictUpdates ->
          {record_setel(var, name, fs, us), st2}

        true ->
          record_match(var, name, :erlang.element(2, hd(us)), fs, us, st2)
      end

    {{:block, line, pre ++ [{:match, line, var, r}, update]}, st}
  end

  defp record_match(r, name, lr, fs, us, st0) do
    {ps, news, st1} = record_upd_fs(fs, us, st0)
    nLr = no_compiler_warning(lr)
    rLine = mark_record(lr, st1)

    {{:case, lr, r,
      [
        {:clause, lr, [{:tuple, rLine, [{:atom, lr, name} | ps]}], [],
         [{:tuple, rLine, [{:atom, lr, name} | news]}]},
        {:clause, nLr, [{:var, nLr, :_}], [],
         [
           call_error(
             nLr,
             {:tuple, nLr, [{:atom, nLr, :badrecord}, {:atom, nLr, name}]}
           )
         ]}
      ]}, st1}
  end

  defp record_upd_fs(
         [
           {:record_field, lf, {:atom, _La, f}, _Val}
           | fs
         ],
         us,
         st0
       ) do
    {p, st1} = new_var(lf, st0)
    {ps, news, st2} = record_upd_fs(fs, us, st1)

    case find_field(f, us) do
      {:ok, new} ->
        {[p | ps], [new | news], st2}

      :error ->
        {[p | ps], [p | news], st2}
    end
  end

  defp record_upd_fs([], _, st) do
    {[], [], st}
  end

  defp record_setel(r, name, fs, us0) do
    us1 =
      foldl(
        fn {:record_field, lf, field, val}, acc ->
          {:integer, _, fieldIndex} = i = index_expr(lf, field, name, fs)
          [{fieldIndex, {i, lf, val}} | acc]
        end,
        [],
        us0
      )

    us2 = sort(us1)

    us =
      for {_, t} <- us2 do
        t
      end

    lr = :erlang.element(2, hd(us))
    wildcards = duplicate(length(fs), {:var, lr, :_})
    nLr = no_compiler_warning(lr)

    {:case, lr, r,
     [
       {:clause, lr, [{:tuple, lr, [{:atom, lr, name} | wildcards]}], [],
        [
          foldr(
            fn {i, lf, val}, acc ->
              {:call, lf, {:remote, lf, {:atom, lf, :erlang}, {:atom, lf, :setelement}},
               [i, acc, val]}
            end,
            r,
            us
          )
        ]},
       {:clause, nLr, [{:var, nLr, :_}], [],
        [
          call_error(
            nLr,
            {:tuple, nLr, [{:atom, nLr, :badrecord}, {:atom, nLr, name}]}
          )
        ]}
     ]}
  end

  defp record_info_call(line, [{:atom, _Li, info}, {:atom, _Ln, name}], st) do
    case info do
      :size ->
        {{:integer, line, 1 + length(record_fields(name, st))}, st}

      :fields ->
        {make_list(field_names(record_fields(name, st)), line), st}
    end
  end

  defp record_exprs(us, st) do
    record_exprs(us, st, [], [])
  end

  defp record_exprs(
         [
           {:record_field, lf, {:atom, _La, _F} = name, val} = field0
           | us
         ],
         st0,
         pre,
         fs
       ) do
    case is_simple_val(val) do
      true ->
        record_exprs(us, st0, pre, [field0 | fs])

      false ->
        {var, st} = new_var(lf, st0)
        bind = {:match, lf, var, val}
        field = {:record_field, lf, name, var}
        record_exprs(us, st, [bind | pre], [field | fs])
    end
  end

  defp record_exprs([], st, pre, fs) do
    {reverse(pre), fs, st}
  end

  defp is_simple_val({:var, _, _}) do
    true
  end

  defp is_simple_val(val) do
    try do
      :erl_parse.normalise(val)
      true
    catch
      :error, _ ->
        false
    end
  end

  defp pattern_bin(es0, st) do
    foldr(
      fn e, acc ->
        pattern_element(e, acc)
      end,
      {[], st},
      es0
    )
  end

  defp pattern_element(
         {:bin_element, line, expr0, size0, type},
         {es, st0}
       ) do
    {expr, st1} = pattern(expr0, st0)

    {size, st2} =
      case size0 do
        :default ->
          {size0, st1}

        _ ->
          expr(size0, st1)
      end

    {[{:bin_element, line, expr, size, type} | es], st2}
  end

  defp expr_bin(es0, st) do
    foldr(
      fn e, acc ->
        bin_element(e, acc)
      end,
      {[], st},
      es0
    )
  end

  defp bin_element(
         {:bin_element, line, expr, size, type},
         {es, st0}
       ) do
    {expr1, st1} = expr(expr, st0)

    {size1, st2} =
      cond do
        size === :default ->
          {:default, st1}

        true ->
          expr(size, st1)
      end

    {[{:bin_element, line, expr1, size1, type} | es], st2}
  end

  defp new_vars(n, l, st) do
    new_vars(n, l, st, [])
  end

  defp new_vars(n, l, st0, vs) when n > 0 do
    {v, st1} = new_var(l, st0)
    new_vars(n - 1, l, st1, [v | vs])
  end

  defp new_vars(0, _L, st, vs) do
    {vs, st}
  end

  defp new_var(l, st0) do
    {new, st1} = new_var_name(st0)
    {{:var, l, new}, st1}
  end

  defp new_var_name(st) do
    c = r_exprec(st, :vcount)
    {:erlang.list_to_atom('rec' ++ :erlang.integer_to_list(c)), r_exprec(st, vcount: c + 1)}
  end

  defp make_list(ts, line) do
    foldr(
      fn h, t ->
        {:cons, line, h, t}
      end,
      {nil, line},
      ts
    )
  end

  defp call_error(l, r) do
    {:call, l, {:remote, l, {:atom, l, :erlang}, {:atom, l, :error}}, [r]}
  end

  defp optimize_is_record(h0, g0, r_exprec(compile: opts)) do
    case opt_rec_vars(g0) do
      [] ->
        {h0, g0}

      rs0 ->
        case :lists.member(:dialyzer, opts) do
          true ->
            {h0, g0}

          false ->
            {h, rs} = opt_pattern_list(h0, rs0)
            g = opt_remove(g0, rs)
            {h, g}
        end
    end
  end

  defp opt_rec_vars([g | gs]) do
    rs = opt_rec_vars_1(g, :orddict.new())
    opt_rec_vars(gs, rs)
  end

  defp opt_rec_vars([]) do
    :orddict.new()
  end

  defp opt_rec_vars([g | gs], rs0) do
    rs1 = opt_rec_vars_1(g, :orddict.new())
    rs = :ordsets.intersection(rs0, rs1)
    opt_rec_vars(gs, rs)
  end

  defp opt_rec_vars([], rs) do
    rs
  end

  defp opt_rec_vars_1([t | ts], rs0) do
    rs = opt_rec_vars_2(t, rs0)
    opt_rec_vars_1(ts, rs)
  end

  defp opt_rec_vars_1([], rs) do
    rs
  end

  defp opt_rec_vars_2({:op, _, :and, a1, a2}, rs) do
    opt_rec_vars_1([a1, a2], rs)
  end

  defp opt_rec_vars_2({:op, _, :andalso, a1, a2}, rs) do
    opt_rec_vars_1([a1, a2], rs)
  end

  defp opt_rec_vars_2(
         {:op, _, :orelse, arg, {:atom, _, :fail}},
         rs
       ) do
    opt_rec_vars_2(arg, rs)
  end

  defp opt_rec_vars_2(
         {:call, _, {:remote, _, {:atom, _, :erlang}, {:atom, _, :is_record}},
          [{:var, _, v}, {:atom, _, tag}, {:integer, _, sz}]},
         rs
       ) do
    :orddict.store(v, {tag, sz}, rs)
  end

  defp opt_rec_vars_2(
         {:call, _, {:atom, _, :is_record}, [{:var, _, v}, {:atom, _, tag}, {:integer, _, sz}]},
         rs
       ) do
    :orddict.store(v, {tag, sz}, rs)
  end

  defp opt_rec_vars_2(_, rs) do
    rs
  end

  defp opt_pattern_list(ps, rs) do
    opt_pattern_list(ps, rs, [])
  end

  defp opt_pattern_list([p0 | ps], rs0, acc) do
    {p, rs} = opt_pattern(p0, rs0)
    opt_pattern_list(ps, rs, [p | acc])
  end

  defp opt_pattern_list([], rs, acc) do
    {reverse(acc), rs}
  end

  defp opt_pattern({:var, _, v} = var, rs0) do
    case :orddict.find(v, rs0) do
      {:ok, {tag, sz}} ->
        rs = :orddict.store(v, {:remove, tag, sz}, rs0)
        {opt_var(var, tag, sz), rs}

      _ ->
        {var, rs0}
    end
  end

  defp opt_pattern({:cons, line, h0, t0}, rs0) do
    {h, rs1} = opt_pattern(h0, rs0)
    {t, rs} = opt_pattern(t0, rs1)
    {{:cons, line, h, t}, rs}
  end

  defp opt_pattern({:tuple, line, es0}, rs0) do
    {es, rs} = opt_pattern_list(es0, rs0)
    {{:tuple, line, es}, rs}
  end

  defp opt_pattern({:match, line, pa0, pb0}, rs0) do
    {pa, rs1} = opt_pattern(pa0, rs0)
    {pb, rs} = opt_pattern(pb0, rs1)
    {{:match, line, pa, pb}, rs}
  end

  defp opt_pattern(p, rs) do
    {p, rs}
  end

  defp opt_var({:var, line, _} = var, tag, sz) do
    rp = record_pattern(2, -1, :ignore, sz, line, [{:atom, line, tag}])
    {:match, line, {:tuple, line, rp}, var}
  end

  defp opt_remove(gs, rs) do
    for g <- gs do
      opt_remove_1(g, rs)
    end
  end

  defp opt_remove_1(ts, rs) do
    for t <- ts do
      opt_remove_2(t, rs)
    end
  end

  defp opt_remove_2({:op, l, :and = op, a1, a2}, rs) do
    {:op, l, op, opt_remove_2(a1, rs), opt_remove_2(a2, rs)}
  end

  defp opt_remove_2({:op, l, :andalso = op, a1, a2}, rs) do
    {:op, l, op, opt_remove_2(a1, rs), opt_remove_2(a2, rs)}
  end

  defp opt_remove_2({:op, l, :orelse, a1, a2}, rs) do
    {:op, l, :orelse, opt_remove_2(a1, rs), a2}
  end

  defp opt_remove_2(
         {:call, line, {:remote, _, {:atom, _, :erlang}, {:atom, _, :is_record}},
          [{:var, _, v}, {:atom, _, tag}, {:integer, _, sz}]} = a,
         rs
       ) do
    case :orddict.find(v, rs) do
      {:ok, {:remove, ^tag, ^sz}} ->
        {:atom, line, true}

      _ ->
        a
    end
  end

  defp opt_remove_2(
         {:call, line, {:atom, _, :is_record}, [{:var, _, v}, {:atom, _, tag}, {:integer, _, sz}]} =
           a,
         rs
       ) do
    case :orddict.find(v, rs) do
      {:ok, {:remove, ^tag, ^sz}} ->
        {:atom, line, true}

      _ ->
        a
    end
  end

  defp opt_remove_2(a, _) do
    a
  end

  defp no_compiler_warning(anno) do
    :erl_anno.set_generated(true, anno)
  end

  defp mark_record(anno, st) do
    case :lists.member(:dialyzer, r_exprec(st, :compile)) do
      true ->
        :erl_anno.set_record(true, anno)

      false ->
        anno
    end
  end
end
