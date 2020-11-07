defmodule :m_dbg_iload do
  use Bitwise

  def load_mod(mod, file, binary, db) do
    flag = :erlang.process_flag(:trap_exit, true)
    pid = spawn_link(load_mod1(mod, file, binary, db))

    receive do
      {:EXIT, ^pid, what} ->
        :erlang.process_flag(:trap_exit, flag)
        what
    end
  end

  defp load_mod1(mod, file, binary, db) do
    fn ->
      store_module(mod, file, binary, db)
      exit({:ok, mod})
    end
  end

  defp store_module(mod, file, binary, db) do
    {:interpreter_module, exp, abst, src, mD5} = :erlang.binary_to_term(binary)

    forms =
      case abstr(abst) do
        {:abstract_v1, _} ->
          exit({mod, :too_old_beam_file})

        {:abstract_v2, _} ->
          exit({mod, :too_old_beam_file})

        {:raw_abstract_v1, code0} ->
          code = interpret_file_attribute(code0)
          standard_transforms(code)
      end

    :dbg_idb.insert(db, :mod_file, file)
    :dbg_idb.insert(db, :defs, [])
    :erlang.put(:vcount, 0)
    :erlang.put(:fun_count, 0)
    :erlang.put(:funs, [])
    :erlang.put(:mod_md5, mD5)
    store_forms(forms, mod, db, exp)
    :erlang.erase(:mod_md5)
    :erlang.erase(:current_function)
    :erlang.erase(:vcount)
    :erlang.erase(:funs)
    :erlang.erase(:fun_count)
    newBinary = store_mod_line_no(mod, db, :erlang.binary_to_list(src))
    :dbg_idb.insert(db, :mod_bin, newBinary)
    :dbg_idb.insert(db, :mod_raw, <<src::binary, 0::size(8)>>)
  end

  defp standard_transforms(forms0) do
    forms = :erl_expand_records.module(forms0, [])
    :erl_internal.add_predefined_functions(forms)
  end

  defp interpret_file_attribute(code) do
    :epp.interpret_file_attribute(code)
  end

  defp abstr(bin) when is_binary(bin) do
    :erlang.binary_to_term(bin)
  end

  defp abstr(term) do
    term
  end

  defp store_forms([{:function, _, name, arity, cs0} | fs], mod, db, exp) do
    fA = {name, arity}
    :erlang.put(:current_function, fA)
    cs = clauses(cs0)
    exported = :lists.member(fA, exp)
    :dbg_idb.insert(db, {mod, name, arity, exported}, cs)
    store_forms(fs, mod, db, exp)
  end

  defp store_forms([{:attribute, _, _Name, _Val} | fs], mod, db, exp) do
    store_forms(fs, mod, db, exp)
  end

  defp store_forms([_ | fs], mod, db, exp) do
    store_forms(fs, mod, db, exp)
  end

  defp store_forms([], _, _, _) do
    :ok
  end

  defp store_mod_line_no(mod, db, contents) do
    store_mod_line_no(mod, db, contents, 1, 0, [])
  end

  defp store_mod_line_no(_, _, [], _, _, newCont) do
    :erlang.list_to_binary(:lists.reverse(newCont))
  end

  defp store_mod_line_no(mod, db, contents, lineNo, pos, newCont)
       when is_integer(lineNo) do
    {contTail, pos1, newCont1} = store_line(mod, db, contents, lineNo, pos, newCont)
    store_mod_line_no(mod, db, contTail, lineNo + 1, pos1, newCont1)
  end

  defp store_line(_, db, contents, lineNo, pos, newCont) do
    {contHead, contTail, posNL} = get_nl(contents, pos + 8, [])
    :dbg_idb.insert(db, lineNo, {pos + 8, posNL})
    {contTail, posNL + 1, [make_lineno(lineNo, 8, contHead) | newCont]}
  end

  defp make_lineno(n, p, acc) do
    s = :erlang.integer_to_list(n)
    s ++ [?: | spaces(p - length(s) - 1, acc)]
  end

  defp spaces(p, acc) when p > 0 do
    spaces(p - 1, [?\s | acc])
  end

  defp spaces(_, acc) do
    acc
  end

  defp get_nl([10 | t], pos, head) do
    {:lists.reverse([10 | head]), t, pos}
  end

  defp get_nl([h | t], pos, head) do
    get_nl(t, pos + 1, [h | head])
  end

  defp get_nl([], pos, head) do
    {:lists.reverse(head), [], pos}
  end

  defp clauses([c0 | cs]) do
    c1 = clause(c0, true)
    [c1 | clauses(cs)]
  end

  defp clauses([]) do
    []
  end

  defp clause({:clause, anno, h0, g0, b0}, lc) do
    h1 = head(h0)
    g1 = guard(g0)
    b1 = exprs(b0, lc)
    {:clause, ln(anno), h1, g1, b1}
  end

  defp head(ps) do
    patterns(ps)
  end

  defp patterns([p0 | ps]) do
    p1 = pattern(p0)
    [p1 | patterns(ps)]
  end

  defp patterns([]) do
    []
  end

  defp pattern({:var, anno, v}) do
    {:var, ln(anno), v}
  end

  defp pattern({:char, anno, i}) do
    {:value, ln(anno), i}
  end

  defp pattern({:integer, anno, i}) do
    {:value, ln(anno), i}
  end

  defp pattern({:match, anno, pat1, pat2}) do
    {:match, ln(anno), pattern(pat1), pattern(pat2)}
  end

  defp pattern({:float, anno, f}) do
    {:value, ln(anno), f}
  end

  defp pattern({:atom, anno, a}) do
    {:value, ln(anno), a}
  end

  defp pattern({:string, anno, s}) do
    {:value, ln(anno), s}
  end

  defp pattern({nil, anno}) do
    {:value, ln(anno), []}
  end

  defp pattern({:cons, anno, h0, t0}) do
    h1 = pattern(h0)
    t1 = pattern(t0)
    {:cons, ln(anno), h1, t1}
  end

  defp pattern({:tuple, anno, ps0}) do
    ps1 = pattern_list(ps0)
    {:tuple, ln(anno), ps1}
  end

  defp pattern({:map, anno, fs0}) do
    fs1 =
      :lists.map(
        fn {:map_field_exact, l, k, v} ->
          {:map_field_exact, l, gexpr(k), pattern(v)}
        end,
        fs0
      )

    {:map, ln(anno), fs1}
  end

  defp pattern({:op, _, :-, {:integer, anno, i}}) do
    {:value, ln(anno), -i}
  end

  defp pattern({:op, _, :+, {:integer, anno, i}}) do
    {:value, ln(anno), i}
  end

  defp pattern({:op, _, :-, {:char, anno, i}}) do
    {:value, ln(anno), -i}
  end

  defp pattern({:op, _, :+, {:char, anno, i}}) do
    {:value, ln(anno), i}
  end

  defp pattern({:op, _, :-, {:float, anno, i}}) do
    {:value, ln(anno), -i}
  end

  defp pattern({:op, _, :+, {:float, anno, i}}) do
    {:value, ln(anno), i}
  end

  defp pattern({:bin, anno, grp}) do
    grp1 = pattern_list(bin_expand_strings(grp))
    {:bin, ln(anno), grp1}
  end

  defp pattern({:bin_element, anno, expr0, size0, type0}) do
    {size1, type} = make_bit_type(anno, size0, type0)
    expr1 = pattern(expr0)
    expr = coerce_to_float(expr1, type0)
    size = expr(size1, false)
    {:bin_element, ln(anno), expr, size, type}
  end

  defp pattern({:op, _, :++, {nil, _}, r}) do
    pattern(r)
  end

  defp pattern({:op, _, :++, {:cons, li, h, t}, r}) do
    pattern({:cons, li, h, {:op, li, :++, t, r}})
  end

  defp pattern({:op, _, :++, {:string, li, l}, r}) do
    pattern(string_to_conses(li, l, r))
  end

  defp pattern({:op, _Line, _Op, _A} = op) do
    pattern(:erl_eval.partial_eval(op))
  end

  defp pattern({:op, _Line, _Op, _L, _R} = op) do
    pattern(:erl_eval.partial_eval(op))
  end

  defp string_to_conses(anno, cs, tail) do
    :lists.foldr(
      fn c, t ->
        {:cons, anno, {:char, anno, c}, t}
      end,
      tail,
      cs
    )
  end

  defp coerce_to_float({:value, anno, int} = e, [:float | _])
       when is_integer(int) do
    try do
      {:value, anno, :erlang.float(int)}
    catch
      :error, :badarg ->
        e
    end
  end

  defp coerce_to_float(e, _) do
    e
  end

  defp pattern_list([p0 | ps]) do
    p1 = pattern(p0)
    [p1 | pattern_list(ps)]
  end

  defp pattern_list([]) do
    []
  end

  defp guard([g0 | gs]) do
    g1 = and_guard(g0)
    [g1 | guard(gs)]
  end

  defp guard([]) do
    []
  end

  defp and_guard([g0 | gs]) do
    g1 = guard_test(g0)
    [g1 | and_guard(gs)]
  end

  defp and_guard([]) do
    []
  end

  defp guard_test({:call, anno, {:remote, _, {:atom, _, :erlang}, {:atom, _, f}}, as0}) do
    as = gexpr_list(as0)
    {:safe_bif, ln(anno), :erlang, f, as}
  end

  defp guard_test({:op, anno, op, l0}) do
    true =
      :erl_internal.arith_op(
        op,
        1
      ) or :erl_internal.bool_op(op, 1)

    l1 = gexpr(l0)
    {:safe_bif, ln(anno), :erlang, op, [l1]}
  end

  defp guard_test({:op, anno, op, l0, r0})
       when op === :andalso or
              op === :orelse do
    l1 = gexpr(l0)
    r1 = gexpr(r0)
    {op, ln(anno), l1, r1}
  end

  defp guard_test({:op, anno, op, l0, r0}) do
    true =
      :erl_internal.comp_op(
        op,
        2
      ) or
        :erl_internal.bool_op(
          op,
          2
        ) or
        :erl_internal.arith_op(
          op,
          2
        )

    l1 = gexpr(l0)
    r1 = gexpr(r0)
    {:safe_bif, ln(anno), :erlang, op, [l1, r1]}
  end

  defp guard_test({:var, _, _} = v) do
    v
  end

  defp guard_test({:atom, anno, true}) do
    {:value, ln(anno), true}
  end

  defp guard_test({:atom, anno, _}) do
    {:value, ln(anno), false}
  end

  defp guard_test({:integer, anno, _}) do
    {:value, ln(anno), false}
  end

  defp guard_test({:char, anno, _}) do
    {:value, ln(anno), false}
  end

  defp guard_test({:float, anno, _}) do
    {:value, ln(anno), false}
  end

  defp guard_test({:string, anno, _}) do
    {:value, ln(anno), false}
  end

  defp guard_test({nil, anno}) do
    {:value, ln(anno), false}
  end

  defp guard_test({:cons, anno, _, _}) do
    {:value, ln(anno), false}
  end

  defp guard_test({:tuple, anno, _}) do
    {:value, ln(anno), false}
  end

  defp guard_test({:map, anno, _}) do
    {:value, ln(anno), false}
  end

  defp guard_test({:map, anno, _, _}) do
    {:value, ln(anno), false}
  end

  defp guard_test({:bin, anno, _}) do
    {:value, ln(anno), false}
  end

  defp gexpr({:var, anno, v}) do
    {:var, ln(anno), v}
  end

  defp gexpr({:integer, anno, i}) do
    {:value, ln(anno), i}
  end

  defp gexpr({:char, anno, i}) do
    {:value, ln(anno), i}
  end

  defp gexpr({:float, anno, f}) do
    {:value, ln(anno), f}
  end

  defp gexpr({:atom, anno, a}) do
    {:value, ln(anno), a}
  end

  defp gexpr({:string, anno, s}) do
    {:value, ln(anno), s}
  end

  defp gexpr({nil, anno}) do
    {:value, ln(anno), []}
  end

  defp gexpr({:cons, anno, h0, t0}) do
    case {gexpr(h0), gexpr(t0)} do
      {{:value, line, h1}, {:value, line, t1}} ->
        {:value, line, [h1 | t1]}

      {h1, t1} ->
        {:cons, ln(anno), h1, t1}
    end
  end

  defp gexpr({:tuple, anno, es0}) do
    es1 = gexpr_list(es0)
    {:tuple, ln(anno), es1}
  end

  defp gexpr({:map, anno, fs0}) do
    new_map(fs0, anno, &gexpr/1)
  end

  defp gexpr({:map, anno, e0, fs0}) do
    e1 = gexpr(e0)
    fs1 = map_fields(fs0, &gexpr/1)
    {:map, ln(anno), e1, fs1}
  end

  defp gexpr({:bin, anno, flds0}) do
    flds = gexpr_list(bin_expand_strings(flds0))
    {:bin, ln(anno), flds}
  end

  defp gexpr({:bin_element, anno, expr0, size0, type0}) do
    {size1, type} = make_bit_type(anno, size0, type0)
    expr = gexpr(expr0)
    size = gexpr(size1)
    {:bin_element, ln(anno), expr, size, type}
  end

  defp gexpr({:call, anno, {:remote, _, {:atom, _, :erlang}, {:atom, _, :self}}, []}) do
    {:dbg, ln(anno), :self, []}
  end

  defp gexpr({:call, anno, {:remote, _, {:atom, _, :erlang}, {:atom, _, f}}, as0}) do
    as = gexpr_list(as0)
    {:safe_bif, ln(anno), :erlang, f, as}
  end

  defp gexpr({:op, anno, op, a0}) do
    :erl_internal.arith_op(op, 1)
    a1 = gexpr(a0)
    {:safe_bif, ln(anno), :erlang, op, [a1]}
  end

  defp gexpr({:op, anno, op, l0, r0})
       when op === :andalso or
              op === :orelse do
    l1 = gexpr(l0)
    r1 = gexpr(r0)
    {op, ln(anno), l1, r1}
  end

  defp gexpr({:op, anno, op, l0, r0}) do
    true =
      :erl_internal.arith_op(
        op,
        2
      ) or
        :erl_internal.comp_op(
          op,
          2
        ) or
        :erl_internal.bool_op(
          op,
          2
        )

    l1 = gexpr(l0)
    r1 = gexpr(r0)
    {:safe_bif, ln(anno), :erlang, op, [l1, r1]}
  end

  defp gexpr_list([e0 | es]) do
    e1 = gexpr(e0)
    [e1 | gexpr_list(es)]
  end

  defp gexpr_list([]) do
    []
  end

  defp exprs([e], lc) do
    [expr(e, lc)]
  end

  defp exprs([e0 | es], lc) do
    e1 = expr(e0, false)
    [e1 | exprs(es, lc)]
  end

  defp exprs([], _Lc) do
    []
  end

  defp expr({:var, anno, v}, _Lc) do
    {:var, ln(anno), v}
  end

  defp expr({:integer, anno, i}, _Lc) do
    {:value, ln(anno), i}
  end

  defp expr({:char, anno, i}, _Lc) do
    {:value, ln(anno), i}
  end

  defp expr({:float, anno, f}, _Lc) do
    {:value, ln(anno), f}
  end

  defp expr({:atom, anno, a}, _Lc) do
    {:value, ln(anno), a}
  end

  defp expr({:string, anno, s}, _Lc) do
    {:value, ln(anno), s}
  end

  defp expr({nil, anno}, _Lc) do
    {:value, ln(anno), []}
  end

  defp expr({:cons, anno, h0, t0}, _Lc) do
    case {expr(h0, false), expr(t0, false)} do
      {{:value, line, h1}, {:value, line, t1}} ->
        {:value, line, [h1 | t1]}

      {h1, t1} ->
        {:cons, ln(anno), h1, t1}
    end
  end

  defp expr({:tuple, anno, es0}, _Lc) do
    es1 = expr_list(es0)
    {:tuple, ln(anno), es1}
  end

  defp expr({:map, anno, fs}, _Lc) do
    new_map(fs, anno, fn e ->
      expr(e, false)
    end)
  end

  defp expr({:map, anno, e0, fs0}, _Lc) do
    e1 = expr(e0, false)
    fs1 = map_fields(fs0)
    {:map, ln(anno), e1, fs1}
  end

  defp expr({:block, anno, es0}, lc) do
    es1 = exprs(es0, lc)
    {:block, ln(anno), es1}
  end

  defp expr({:if, anno, cs0}, lc) do
    cs1 = icr_clauses(cs0, lc)
    {:if, ln(anno), cs1}
  end

  defp expr({:case, anno, e0, cs0}, lc) do
    e1 = expr(e0, false)
    cs1 = icr_clauses(cs0, lc)
    {:case, ln(anno), e1, cs1}
  end

  defp expr({:receive, anno, cs0}, lc) do
    cs1 = icr_clauses(cs0, lc)
    {:receive, ln(anno), cs1}
  end

  defp expr({:receive, anno, cs0, to0, toEs0}, lc) do
    to1 = expr(to0, false)
    toEs1 = exprs(toEs0, lc)
    cs1 = icr_clauses(cs0, lc)
    {:receive, ln(anno), cs1, to1, toEs1}
  end

  defp expr({:fun, anno, {:clauses, cs0}}, _Lc) do
    cs = fun_clauses(cs0)
    name = new_fun_name()
    {:make_fun, ln(anno), name, cs}
  end

  defp expr({:fun, anno, {:function, f, a}}, _Lc) do
    line = ln(anno)
    as = new_vars(a, line)
    name = new_fun_name()
    cs = [{:clause, line, as, [], [{:local_call, line, f, as, true}]}]
    {:make_fun, line, name, cs}
  end

  defp expr({:named_fun, anno, fName, cs0}, _Lc) do
    cs = fun_clauses(cs0)
    name = new_fun_name()
    {:make_named_fun, ln(anno), name, fName, cs}
  end

  defp expr(
         {:fun, anno, {:function, {:atom, _, m}, {:atom, _, f}, {:integer, _, a}}},
         _Lc
       )
       when 0 <= a and a <= 255 do
    {:value, ln(anno), :erlang.make_fun(m, f, a)}
  end

  defp expr({:fun, anno, {:function, m, f, a}}, _Lc) do
    mFA = expr_list([m, f, a])
    {:make_ext_fun, ln(anno), mFA}
  end

  defp expr(
         {:call, anno, {:remote, _, {:atom, _, :erlang}, {:atom, _, :self}}, []},
         _Lc
       ) do
    {:dbg, ln(anno), :self, []}
  end

  defp expr(
         {:call, anno, {:remote, _, {:atom, _, :erlang}, {:atom, _, :throw}}, [_] = as},
         _Lc
       ) do
    {:dbg, ln(anno), :throw, expr_list(as)}
  end

  defp expr(
         {:call, anno, {:remote, _, {:atom, _, :erlang}, {:atom, _, :error}}, [_] = as},
         _Lc
       ) do
    {:dbg, ln(anno), :error, expr_list(as)}
  end

  defp expr(
         {:call, anno, {:remote, _, {:atom, _, :erlang}, {:atom, _, :exit}}, [_] = as},
         _Lc
       ) do
    {:dbg, ln(anno), :exit, expr_list(as)}
  end

  defp expr(
         {:call, anno, {:remote, _, {:atom, _, :erlang}, {:atom, _, :raise}}, [_, _, _] = as},
         _Lc
       ) do
    {:dbg, ln(anno), :raise, expr_list(as)}
  end

  defp expr(
         {:call, anno, {:remote, _, {:atom, _, :erlang}, {:atom, _, :apply}}, [_, _, _] = as0},
         lc
       ) do
    as = expr_list(as0)
    {:apply, ln(anno), as, lc}
  end

  defp expr(
         {:call, anno, {:remote, _, {:atom, _, mod}, {:atom, _, func}}, as0},
         lc
       ) do
    as = expr_list(as0)

    case :erlang.is_builtin(mod, func, length(as)) do
      false ->
        {:call_remote, ln(anno), mod, func, as, lc}

      true ->
        case bif_type(mod, func, length(as0)) do
          :safe ->
            {:safe_bif, ln(anno), mod, func, as}

          :unsafe ->
            {:bif, ln(anno), mod, func, as}
        end
    end
  end

  defp expr(
         {:call, anno, {:remote, _, mod0, func0}, as0},
         lc
       ) do
    mod = expr(mod0, false)
    func = expr(func0, false)
    as = consify(expr_list(as0))
    {:apply, ln(anno), [mod, func, as], lc}
  end

  defp expr({:call, anno, {:atom, _, func}, as0}, lc) do
    as = expr_list(as0)
    {:local_call, ln(anno), func, as, lc}
  end

  defp expr({:call, anno, fun0, as0}, lc) do
    fun = expr(fun0, false)
    as = expr_list(as0)
    {:apply_fun, ln(anno), fun, as, lc}
  end

  defp expr({:catch, anno, e0}, _Lc) do
    e1 = expr(e0, false)
    {:catch, ln(anno), e1}
  end

  defp expr(
         {:try, anno, es0, caseCs0, catchCs0, as0},
         lc
       ) do
    es = expr_list(es0)
    caseCs = icr_clauses(caseCs0, lc)
    catchCs = icr_clauses(catchCs0, lc)
    as = expr_list(as0)
    {:try, ln(anno), es, caseCs, catchCs, as}
  end

  defp expr({:lc, _, _, _} = compr, _Lc) do
    expr_lc_bc(compr)
  end

  defp expr({:bc, _, _, _} = compr, _Lc) do
    expr_lc_bc(compr)
  end

  defp expr({:match, anno, p0, e0}, _Lc) do
    e1 = expr(e0, false)
    p1 = pattern(p0)
    {:match, ln(anno), p1, e1}
  end

  defp expr({:op, anno, op, a0}, _Lc) do
    a1 = expr(a0, false)
    {:op, ln(anno), op, [a1]}
  end

  defp expr({:op, anno, :++, l0, r0}, _Lc) do
    l1 = expr(l0, false)
    r1 = expr(r0, false)
    {:op, ln(anno), :append, [l1, r1]}
  end

  defp expr({:op, anno, :--, l0, r0}, _Lc) do
    l1 = expr(l0, false)
    r1 = expr(r0, false)
    {:op, ln(anno), :subtract, [l1, r1]}
  end

  defp expr({:op, anno, :!, l0, r0}, _Lc) do
    l1 = expr(l0, false)
    r1 = expr(r0, false)
    {:send, ln(anno), l1, r1}
  end

  defp expr({:op, anno, op, l0, r0}, _Lc)
       when op === :andalso or op === :orelse do
    l1 = expr(l0, false)
    r1 = expr(r0, false)
    {op, ln(anno), l1, r1}
  end

  defp expr({:op, anno, op, l0, r0}, _Lc) do
    l1 = expr(l0, false)
    r1 = expr(r0, false)
    {:op, ln(anno), op, [l1, r1]}
  end

  defp expr({:bin, anno, grp}, _Lc) do
    grp1 = expr_list(bin_expand_strings(grp))
    {:bin, ln(anno), grp1}
  end

  defp expr(
         {:bin_element, anno, expr0, size0, type0},
         _Lc
       ) do
    {size1, type} = make_bit_type(anno, size0, type0)
    expr = expr(expr0, false)
    size = expr(size1, false)
    {:bin_element, ln(anno), expr, size, type}
  end

  defp consify([a | as]) do
    {:cons, 0, a, consify(as)}
  end

  defp consify([]) do
    {:value, 0, []}
  end

  defp make_bit_type(line, :default, type0) do
    case :erl_bits.set_bit_type(:default, type0) do
      {:ok, :all, bt} ->
        {{:atom, line, :all}, :erl_bits.as_list(bt)}

      {:ok, :undefined, bt} ->
        {{:atom, line, :undefined}, :erl_bits.as_list(bt)}

      {:ok, size, bt} ->
        {{:integer, line, size}, :erl_bits.as_list(bt)}
    end
  end

  defp make_bit_type(_Line, size, type0) do
    {:ok, ^size, bt} = :erl_bits.set_bit_type(size, type0)
    {size, :erl_bits.as_list(bt)}
  end

  defp expr_lc_bc({tag, anno, e0, gs0}) do
    gs =
      :lists.map(
        fn
          {:generate, l, p0, qs} ->
            {:generate, l, pattern(p0), expr(qs, false)}

          {:b_generate, l, p0, qs} ->
            {:b_generate, l, pattern(p0), expr(qs, false)}

          expr ->
            case is_guard_test(expr) do
              true ->
                {:guard, guard([[expr]])}

              false ->
                expr(expr, false)
            end
        end,
        gs0
      )

    {tag, ln(anno), expr(e0, false), gs}
  end

  defp is_guard_test(expr) do
    isOverridden = fn {_, _} ->
      true
    end

    :erl_lint.is_guard_test(expr, [], isOverridden)
  end

  defp bin_expand_strings(es) do
    :lists.foldr(
      fn
        {:bin_element, line, {:string, _, s}, sz, ts}, es1 ->
          :lists.foldr(
            fn c, es2 ->
              [
                {:bin_element, line, {:char, line, c}, sz, ts}
                | es2
              ]
            end,
            es1,
            s
          )

        e, es1 ->
          [e | es1]
      end,
      [],
      es
    )
  end

  defp expr_list([e0 | es]) do
    e1 = expr(e0, false)
    [e1 | expr_list(es)]
  end

  defp expr_list([]) do
    []
  end

  defp icr_clauses([c0 | cs], lc) do
    c1 = clause(c0, lc)
    [c1 | icr_clauses(cs, lc)]
  end

  defp icr_clauses([], _) do
    []
  end

  defp fun_clauses([{:clause, a, h, g, b} | cs]) do
    [
      {:clause, ln(a), head(h), guard(g), exprs(b, true)}
      | fun_clauses(cs)
    ]
  end

  defp fun_clauses([]) do
    []
  end

  defp new_map(fs0, anno, f) do
    line = ln(anno)
    fs1 = map_fields(fs0, f)

    fs2 =
      for {:map_field_assoc, l, k, v} <- fs1 do
        {l, k, v}
      end

    try do
      {:value, line, map_literal(fs2, %{})}
    catch
      :not_literal ->
        {:map, line, fs2}
    end
  end

  defp map_literal([{_, {:value, _, k}, {:value, _, v}} | t], m) do
    map_literal(t, :maps.put(k, v, m))
  end

  defp map_literal([_ | _], _) do
    throw(:not_literal)
  end

  defp map_literal([], m) do
    m
  end

  defp map_fields(fs) do
    map_fields(
      fs,
      fn e ->
        expr(e, false)
      end
    )
  end

  defp map_fields([{:map_field_assoc, a, n, v} | fs], f) do
    [
      {:map_field_assoc, ln(a), f.(n), f.(v)}
      | map_fields(fs)
    ]
  end

  defp map_fields([{:map_field_exact, a, n, v} | fs], f) do
    [
      {:map_field_exact, ln(a), f.(n), f.(v)}
      | map_fields(fs)
    ]
  end

  defp map_fields([], _) do
    []
  end

  defp new_var_name() do
    c = :erlang.get(:vcount)
    :erlang.put(:vcount, c + 1)
    :erlang.list_to_atom('%' ++ :erlang.integer_to_list(c))
  end

  defp new_vars(n, l) do
    new_vars(n, l, [])
  end

  defp new_vars(n, l, vs) when n > 0 do
    v = {:var, l, new_var_name()}
    new_vars(n - 1, l, [v | vs])
  end

  defp new_vars(0, _, vs) do
    vs
  end

  defp new_fun_name() do
    {f, a} = :erlang.get(:current_function)
    i = :erlang.get(:fun_count)
    :erlang.put(:fun_count, i + 1)

    name =
      '-' ++
        :erlang.atom_to_list(f) ++
        '/' ++ :erlang.integer_to_list(a) ++ '-fun-' ++ :erlang.integer_to_list(i) ++ '-'

    :erlang.list_to_atom(name)
  end

  defp ln(anno) do
    :erl_anno.line(anno)
  end

  defp bif_type(:erlang, name, arity) do
    case :erl_internal.guard_bif(name, arity) do
      true ->
        :safe

      false ->
        bif_type(name)
    end
  end

  defp bif_type(_, _, _) do
    :unsafe
  end

  defp bif_type(:register) do
    :safe
  end

  defp bif_type(:unregister) do
    :safe
  end

  defp bif_type(:whereis) do
    :safe
  end

  defp bif_type(:registered) do
    :safe
  end

  defp bif_type(:setelement) do
    :safe
  end

  defp bif_type(:atom_to_list) do
    :safe
  end

  defp bif_type(:list_to_atom) do
    :safe
  end

  defp bif_type(:integer_to_list) do
    :safe
  end

  defp bif_type(:list_to_integer) do
    :safe
  end

  defp bif_type(:float_to_list) do
    :safe
  end

  defp bif_type(:list_to_float) do
    :safe
  end

  defp bif_type(:tuple_to_list) do
    :safe
  end

  defp bif_type(:list_to_tuple) do
    :safe
  end

  defp bif_type(:make_ref) do
    :safe
  end

  defp bif_type(:time) do
    :safe
  end

  defp bif_type(:date) do
    :safe
  end

  defp bif_type(:processes) do
    :safe
  end

  defp bif_type(:process_info) do
    :safe
  end

  defp bif_type(:load_module) do
    :safe
  end

  defp bif_type(:delete_module) do
    :safe
  end

  defp bif_type(:halt) do
    :safe
  end

  defp bif_type(:check_process_code) do
    :safe
  end

  defp bif_type(:purge_module) do
    :safe
  end

  defp bif_type(:pid_to_list) do
    :safe
  end

  defp bif_type(:list_to_pid) do
    :safe
  end

  defp bif_type(:module_loaded) do
    :safe
  end

  defp bif_type(:binary_to_term) do
    :safe
  end

  defp bif_type(:term_to_binary) do
    :safe
  end

  defp bif_type(:nodes) do
    :safe
  end

  defp bif_type(:is_alive) do
    :safe
  end

  defp bif_type(:disconnect_node) do
    :safe
  end

  defp bif_type(:binary_to_list) do
    :safe
  end

  defp bif_type(:list_to_binary) do
    :safe
  end

  defp bif_type(:split_binary) do
    :safe
  end

  defp bif_type(:hash) do
    :safe
  end

  defp bif_type(:pre_loaded) do
    :safe
  end

  defp bif_type(:set_cookie) do
    :safe
  end

  defp bif_type(:get_cookie) do
    :safe
  end

  defp bif_type(_) do
    :unsafe
  end
end
