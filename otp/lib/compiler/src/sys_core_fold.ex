defmodule :m_sys_core_fold do
  use Bitwise
  import :cerl, only: [ann_c_cons: 3, ann_c_map: 3, ann_c_tuple: 2]

  import :lists,
    only: [
      all: 2,
      any: 2,
      flatten: 1,
      foldl: 3,
      foldr: 3,
      keyfind: 3,
      map: 2,
      mapfoldl: 3,
      member: 2,
      reverse: 1,
      reverse: 2,
      unzip: 1
    ]

  require Record
  Record.defrecord(:r_c_alias, :c_alias, anno: [], var: :undefined, pat: :undefined)
  Record.defrecord(:r_c_apply, :c_apply, anno: [], op: :undefined, args: :undefined)

  Record.defrecord(:r_c_binary, :c_binary,
    anno: [],
    segments: :undefined
  )

  Record.defrecord(:r_c_bitstr, :c_bitstr,
    anno: [],
    val: :undefined,
    size: :undefined,
    unit: :undefined,
    type: :undefined,
    flags: :undefined
  )

  Record.defrecord(:r_c_call, :c_call,
    anno: [],
    module: :undefined,
    name: :undefined,
    args: :undefined
  )

  Record.defrecord(:r_c_case, :c_case, anno: [], arg: :undefined, clauses: :undefined)

  Record.defrecord(:r_c_catch, :c_catch,
    anno: [],
    body: :undefined
  )

  Record.defrecord(:r_c_clause, :c_clause,
    anno: [],
    pats: :undefined,
    guard: :undefined,
    body: :undefined
  )

  Record.defrecord(:r_c_cons, :c_cons, anno: [], hd: :undefined, tl: :undefined)
  Record.defrecord(:r_c_fun, :c_fun, anno: [], vars: :undefined, body: :undefined)

  Record.defrecord(:r_c_let, :c_let, anno: [], vars: :undefined, arg: :undefined, body: :undefined)

  Record.defrecord(:r_c_letrec, :c_letrec, anno: [], defs: :undefined, body: :undefined)

  Record.defrecord(:r_c_literal, :c_literal,
    anno: [],
    val: :undefined
  )

  Record.defrecord(:r_c_map, :c_map,
    anno: [],
    arg: :EFE_TODO_NESTED_RECORD,
    es: :undefined,
    is_pat: false
  )

  Record.defrecord(:r_c_map_pair, :c_map_pair,
    anno: [],
    op: :undefined,
    key: :undefined,
    val: :undefined
  )

  Record.defrecord(:r_c_module, :c_module,
    anno: [],
    name: :undefined,
    exports: :undefined,
    attrs: :undefined,
    defs: :undefined
  )

  Record.defrecord(:r_c_primop, :c_primop, anno: [], name: :undefined, args: :undefined)

  Record.defrecord(:r_c_receive, :c_receive,
    anno: [],
    clauses: :undefined,
    timeout: :undefined,
    action: :undefined
  )

  Record.defrecord(:r_c_seq, :c_seq, anno: [], arg: :undefined, body: :undefined)

  Record.defrecord(:r_c_try, :c_try,
    anno: [],
    arg: :undefined,
    vars: :undefined,
    body: :undefined,
    evars: :undefined,
    handler: :undefined
  )

  Record.defrecord(:r_c_tuple, :c_tuple, anno: [], es: :undefined)

  Record.defrecord(:r_c_values, :c_values,
    anno: [],
    es: :undefined
  )

  Record.defrecord(:r_c_var, :c_var, anno: [], name: :undefined)
  Record.defrecord(:r_sub, :sub, v: [], s: :cerl_sets.new(), t: %{}, in_guard: false)

  def module(r_c_module(defs: ds0) = mod, opts) do
    :erlang.put(
      :no_inline_list_funcs,
      not member(:inline_list_funcs, opts)
    )

    init_warnings()

    ds1 =
      for d <- ds0 do
        function_1(d)
      end

    :erlang.erase(:new_var_num)
    :erlang.erase(:no_inline_list_funcs)
    {:ok, r_c_module(mod, defs: ds1), get_warnings()}
  end

  defp function_1({r_c_var(name: {f, arity}) = name, b0}) do
    try do
      count = :cerl_trees.next_free_variable_name(b0)
      :erlang.put(:new_var_num, count)

      b =
        find_fixpoint(
          fn core ->
            expr(core, :value, sub_new())
          end,
          b0,
          20
        )

      {name, b}
    catch
      class, error ->
        :io.fwrite('Function: ~w/~w\n', [f, arity])
        :erlang.raise(class, error, __STACKTRACE__)
    end
  end

  defp find_fixpoint(_OptFun, core, 0) do
    core
  end

  defp find_fixpoint(optFun, core0, max) do
    case optFun.(core0) do
      ^core0 ->
        core0

      core ->
        find_fixpoint(optFun, core, max - 1)
    end
  end

  defp body(body, sub) do
    body(body, :value, sub)
  end

  defp body(r_c_values(anno: a, es: es0), :value, sub) do
    es1 = expr_list(es0, :value, sub)
    r_c_values(anno: a, es: es1)
  end

  defp body(e, ctxt, sub) do
    :ignore
    expr(e, ctxt, sub)
  end

  defp guard(expr, sub) do
    :ignore
    expr(expr, :value, r_sub(sub, in_guard: true))
  end

  defp expr(expr, sub) do
    expr(expr, :value, sub)
  end

  defp expr(r_c_var() = v, ctxt, sub) do
    case ctxt do
      :effect ->
        void()

      :value ->
        sub_get_var(v, sub)
    end
  end

  defp expr(r_c_literal(val: val) = l, ctxt, _Sub) do
    case ctxt do
      :effect ->
        case val do
          [] ->
            l

          _ when is_atom(val) ->
            void()

          _ ->
            add_warning(l, :useless_building)
            void()
        end

      :value ->
        l
    end
  end

  defp expr(r_c_cons(anno: anno, hd: h0, tl: t0) = cons, ctxt, sub) do
    h1 = expr(h0, ctxt, sub)
    t1 = expr(t0, ctxt, sub)

    case ctxt do
      :effect ->
        add_warning(cons, :useless_building)
        make_effect_seq([h1, t1], sub)

      :value ->
        ann_c_cons(anno, h1, t1)
    end
  end

  defp expr(r_c_tuple(anno: anno, es: es0) = tuple, ctxt, sub) do
    es = expr_list(es0, ctxt, sub)

    case ctxt do
      :effect ->
        add_warning(tuple, :useless_building)
        make_effect_seq(es, sub)

      :value ->
        ann_c_tuple(anno, es)
    end
  end

  defp expr(r_c_map(anno: anno, arg: v0, es: es0) = map, ctxt, sub) do
    es = pair_list(es0, ctxt, sub)

    case ctxt do
      :effect ->
        add_warning(map, :useless_building)
        make_effect_seq(es, sub)

      :value ->
        v = expr(v0, ctxt, sub)
        ann_c_map(anno, v, es)
    end
  end

  defp expr(r_c_binary(segments: ss) = bin0, ctxt, sub) do
    case ctxt do
      :effect ->
        add_warning(bin0, :useless_building)

      :value ->
        :ok
    end

    bin1 = r_c_binary(bin0, segments: bitstr_list(ss, sub))
    bin = bin_un_utf(bin1)
    eval_binary(bin)
  end

  defp expr(r_c_fun() = fun, :effect, _) do
    add_warning(fun, :useless_building)
    void()
  end

  defp expr(r_c_fun(vars: vs0, body: b0) = fun, ctxt0, sub0) do
    {vs1, sub1} = var_list(vs0, sub0)

    ctxt =
      case ctxt0 do
        {:letrec, ctxt1} ->
          ctxt1

        :value ->
          :value
      end

    b1 = body(b0, ctxt, sub1)
    r_c_fun(fun, vars: vs1, body: b1)
  end

  defp expr(r_c_seq(arg: arg0, body: b0) = seq0, ctxt, sub) do
    b1 = body(b0, ctxt, sub)
    arg = body(arg0, :effect, sub)

    case will_fail(arg) do
      true ->
        arg

      false ->
        case {ctxt, is_safe_simple(arg)} do
          {:effect, true} ->
            b1

          {:effect, false} ->
            case is_safe_simple(b1) do
              true ->
                arg

              false ->
                r_c_seq(seq0, arg: arg, body: b1)
            end

          {:value, true} ->
            b1

          {:value, false} ->
            r_c_seq(seq0, arg: arg, body: b1)
        end
    end
  end

  defp expr(r_c_let() = let0, ctxt, sub) do
    let = opt_case_in_let(let0)

    case simplify_let(let, sub) do
      :impossible ->
        :ignore
        opt_simple_let(let, ctxt, sub)

      expr ->
        expr
    end
  end

  defp expr(r_c_letrec(body: r_c_var()) = letrec, :effect, _Sub) do
    add_warning(letrec, :useless_building)
    void()
  end

  defp expr(r_c_letrec(defs: fs0, body: b0) = letrec, ctxt, sub) do
    fs1 =
      map(
        fn {name, fb} ->
          case ctxt === :effect and
                 is_fun_effect_safe(
                   name,
                   b0
                 ) do
            true ->
              {name, expr(fb, {:letrec, :effect}, sub)}

            false ->
              {name, expr(fb, {:letrec, :value}, sub)}
          end
        end,
        fs0
      )

    b1 = body(b0, ctxt, sub)
    r_c_letrec(letrec, defs: fs1, body: b1)
  end

  defp expr(r_c_case() = case0, ctxt, sub) do
    case opt_bool_case(case0, sub) do
      r_c_case(anno: anno, arg: arg0, clauses: cs0) = case1 ->
        arg1 = body(arg0, :value, sub)
        litExpr = :cerl.is_literal(arg1)
        {arg2, cs1} = case_opt(arg1, cs0, sub)
        cs2 = clauses(arg2, cs1, ctxt, sub, litExpr, anno)
        case__ = r_c_case(case1, arg: arg2, clauses: cs2)
        warn_no_clause_match(case1, case__)
        expr = eval_case(case__, sub)
        move_case_into_arg(expr, sub)

      other ->
        expr(other, ctxt, sub)
    end
  end

  defp expr(r_c_apply(anno: anno, op: op0, args: as0) = apply0, _, sub) do
    op1 = expr(op0, :value, sub)
    as1 = expr_list(as0, :value, sub)

    case :cerl.is_data(op1) and not is_literal_fun(op1) do
      false ->
        apply = r_c_apply(apply0, op: op1, args: as1)
        fold_apply(apply, op1, as1)

      true ->
        add_warning(apply0, :invalid_call)

        err =
          r_c_call(
            anno: anno,
            module: r_c_literal(val: :erlang),
            name: r_c_literal(val: :error),
            args: [r_c_tuple(es: [r_c_literal(val: :badfun), op1])]
          )

        make_effect_seq(as1 ++ [err], sub)
    end
  end

  defp expr(r_c_call(module: m0, name: n0) = call0, ctxt, sub) do
    m1 = expr(m0, :value, sub)
    n1 = expr(n0, :value, sub)
    call = r_c_call(call0, module: m1, name: n1)

    case useless_call(ctxt, call) do
      :no ->
        call(call, m1, n1, sub)

      {:yes, seq} ->
        expr(seq, ctxt, sub)
    end
  end

  defp expr(r_c_primop(name: r_c_literal(val: :build_stacktrace)), :effect, _Sub) do
    void()
  end

  defp expr(r_c_primop(args: as0) = prim, _, sub) do
    as1 = expr_list(as0, :value, sub)
    r_c_primop(prim, args: as1)
  end

  defp expr(r_c_catch(anno: anno, body: b), :effect, sub) do
    var = r_c_var(name: :catch_value)
    evs = [r_c_var(name: :Class), r_c_var(name: :Reason), r_c_var(name: :Stk)]
    try = r_c_try(anno: anno, arg: b, vars: [var], body: var, evars: evs, handler: void())
    expr(try, :effect, sub)
  end

  defp expr(r_c_catch(body: b0) = catch__, _, sub) do
    b1 = body(b0, :value, sub)

    case is_safe_simple(b1) do
      true ->
        b1

      false ->
        r_c_catch(catch__, body: b1)
    end
  end

  defp expr(
         r_c_try(
           arg: e0,
           vars: [r_c_var(name: x)],
           body: r_c_var(name: x),
           handler: r_c_literal(val: false) = false__
         ) = try,
         _,
         sub
       ) do
    e1 = body(e0, :value, sub)

    case will_fail(e1) do
      false ->
        case is_safe_bool_expr(e1) or is_safe_simple(e1) do
          true ->
            e1

          false ->
            r_c_try(try, arg: e1)
        end

      true ->
        false__
    end
  end

  defp expr(
         r_c_try(anno: a, arg: e0, vars: vs0, body: b0, evars: evs0, handler: h0) = try,
         _,
         sub0
       ) do
    e1 = body(e0, :value, sub0)
    {vs1, sub1} = var_list(vs0, sub0)
    b1 = body(b0, :value, sub1)

    case is_safe_simple(e1) do
      true ->
        expr(r_c_let(anno: a, vars: vs1, arg: e1, body: b1), :value, sub0)

      false ->
        {evs1, sub2} = var_list(evs0, sub0)
        h1 = body(h0, :value, sub2)
        r_c_try(try, arg: e1, vars: vs1, body: b1, evars: evs1, handler: h1)
    end
  end

  defp is_fun_effect_safe(r_c_var() = fVar, body) do
    ifes_1(fVar, body, true)
  end

  defp ifes_1(fVar, r_c_alias(pat: pat), _Safe) do
    ifes_1(fVar, pat, false)
  end

  defp ifes_1(fVar, r_c_apply(op: op, args: args), safe) do
    ifes_list(fVar, args, false) and ifes_1(fVar, op, safe)
  end

  defp ifes_1(fVar, r_c_binary(segments: segments), _Safe) do
    ifes_list(fVar, segments, false)
  end

  defp ifes_1(fVar, r_c_bitstr(val: val, size: size, unit: unit), _Safe) do
    ifes_list(fVar, [val, size, unit], false)
  end

  defp ifes_1(fVar, r_c_call(args: args), _Safe) do
    ifes_list(fVar, args, false)
  end

  defp ifes_1(fVar, r_c_case(arg: arg, clauses: clauses), safe) do
    ifes_1(fVar, arg, false) and ifes_list(fVar, clauses, safe)
  end

  defp ifes_1(fVar, r_c_catch(body: body), _Safe) do
    ifes_1(fVar, body, false)
  end

  defp ifes_1(fVar, r_c_clause(pats: pats, guard: guard, body: body), safe) do
    ifes_list(fVar, pats, false) and ifes_1(fVar, guard, false) and ifes_1(fVar, body, safe)
  end

  defp ifes_1(fVar, r_c_cons(hd: hd, tl: tl), _Safe) do
    ifes_1(fVar, hd, false) and ifes_1(fVar, tl, false)
  end

  defp ifes_1(fVar, r_c_fun(body: body), _Safe) do
    ifes_1(fVar, body, false)
  end

  defp ifes_1(fVar, r_c_let(arg: arg, body: body), safe) do
    ifes_1(fVar, arg, false) and ifes_1(fVar, body, safe)
  end

  defp ifes_1(fVar, r_c_letrec(defs: defs, body: body), safe) do
    funs =
      for {_, fun} <- defs do
        fun
      end

    ifes_list(fVar, funs, false) and ifes_1(fVar, body, safe)
  end

  defp ifes_1(_FVar, r_c_literal(), _Safe) do
    true
  end

  defp ifes_1(fVar, r_c_map(arg: arg, es: elements), _Safe) do
    ifes_1(fVar, arg, false) and ifes_list(fVar, elements, false)
  end

  defp ifes_1(fVar, r_c_map_pair(key: key, val: val), _Safe) do
    ifes_1(fVar, key, false) and ifes_1(fVar, val, false)
  end

  defp ifes_1(fVar, r_c_primop(args: args), _Safe) do
    ifes_list(fVar, args, false)
  end

  defp ifes_1(fVar, r_c_seq(arg: arg, body: body), safe) do
    ifes_1(fVar, arg, true) and ifes_1(fVar, body, safe)
  end

  defp ifes_1(fVar, r_c_try(arg: arg, handler: handler, body: body), safe) do
    ifes_1(fVar, arg, false) and ifes_1(fVar, handler, safe) and ifes_1(fVar, body, safe)
  end

  defp ifes_1(fVar, r_c_tuple(es: elements), _Safe) do
    ifes_list(fVar, elements, false)
  end

  defp ifes_1(fVar, r_c_values(es: elements), _Safe) do
    ifes_list(fVar, elements, false)
  end

  defp ifes_1(r_c_var(name: name), r_c_var(name: name), safe) do
    safe
  end

  defp ifes_1(_FVar, r_c_var(), _Safe) do
    true
  end

  defp ifes_list(fVar, [e | es], safe) do
    ifes_1(fVar, e, safe) and ifes_list(fVar, es, safe)
  end

  defp ifes_list(_FVar, [], _Safe) do
    true
  end

  defp expr_list(es, ctxt, sub) do
    for e <- es do
      expr(e, ctxt, sub)
    end
  end

  defp pair_list(es, ctxt, sub) do
    for e <- es do
      pair(e, ctxt, sub)
    end
  end

  defp pair(r_c_map_pair(key: k, val: v), :effect, sub) do
    make_effect_seq([k, v], sub)
  end

  defp pair(r_c_map_pair(key: k0, val: v0) = pair, :value = ctxt, sub) do
    k = expr(k0, ctxt, sub)
    v = expr(v0, ctxt, sub)
    r_c_map_pair(pair, key: k, val: v)
  end

  defp bitstr_list(es, sub) do
    for e <- es do
      bitstr(e, sub)
    end
  end

  defp bitstr(r_c_bitstr(val: val, size: size) = binSeg, sub) do
    r_c_bitstr(binSeg,
      val: expr(val, sub),
      size: expr(size, :value, sub)
    )
  end

  defp is_literal_fun(r_c_literal(val: f)) do
    is_function(f)
  end

  defp is_literal_fun(_) do
    false
  end

  defp is_safe_simple(r_c_var() = var) do
    not :cerl.is_c_fname(var)
  end

  defp is_safe_simple(r_c_cons(hd: h, tl: t)) do
    is_safe_simple(h) and is_safe_simple(t)
  end

  defp is_safe_simple(r_c_tuple(es: es)) do
    is_safe_simple_list(es)
  end

  defp is_safe_simple(r_c_literal()) do
    true
  end

  defp is_safe_simple(
         r_c_call(module: r_c_literal(val: :erlang), name: r_c_literal(val: name), args: args)
       )
       when is_atom(name) do
    numArgs = length(args)

    case :erl_internal.bool_op(name, numArgs) do
      true ->
        all(&is_bool_expr/1, args)

      false ->
        :erl_bifs.is_safe(:erlang, name, numArgs) and
          (:erl_internal.comp_op(
             name,
             numArgs
           ) or
             :erl_internal.new_type_test(
               name,
               numArgs
             ))
    end
  end

  defp is_safe_simple(_) do
    false
  end

  defp is_safe_simple_list(es) do
    all(
      fn e ->
        is_safe_simple(e)
      end,
      es
    )
  end

  defp will_fail(r_c_let(arg: a, body: b)) do
    will_fail(a) or will_fail(b)
  end

  defp will_fail(
         r_c_call(module: r_c_literal(val: mod), name: r_c_literal(val: name), args: args)
       ) do
    :erl_bifs.is_exit_bif(mod, name, length(args))
  end

  defp will_fail(r_c_primop(name: r_c_literal(val: :match_fail), args: [_])) do
    true
  end

  defp will_fail(_) do
    false
  end

  defp bin_un_utf(r_c_binary(anno: anno, segments: ss) = bin) do
    r_c_binary(bin, segments: bin_un_utf_1(ss, anno))
  end

  defp bin_un_utf_1(
         [r_c_bitstr(val: r_c_literal(), type: r_c_literal(val: :utf8)) = h | t],
         anno
       ) do
    bin_un_utf_eval(h, anno) ++ bin_un_utf_1(t, anno)
  end

  defp bin_un_utf_1(
         [r_c_bitstr(val: r_c_literal(), type: r_c_literal(val: :utf16)) = h | t],
         anno
       ) do
    bin_un_utf_eval(h, anno) ++ bin_un_utf_1(t, anno)
  end

  defp bin_un_utf_1(
         [r_c_bitstr(val: r_c_literal(), type: r_c_literal(val: :utf32)) = h | t],
         anno
       ) do
    bin_un_utf_eval(h, anno) ++ bin_un_utf_1(t, anno)
  end

  defp bin_un_utf_1([h | t], anno) do
    [h | bin_un_utf_1(t, anno)]
  end

  defp bin_un_utf_1([], _) do
    []
  end

  defp bin_un_utf_eval(bitstr, anno) do
    segments = [bitstr]

    case eval_binary(r_c_binary(anno: anno, segments: segments)) do
      r_c_literal(anno: ^anno, val: bytes) when is_binary(bytes) ->
        for b <- :erlang.binary_to_list(bytes) do
          r_c_bitstr(
            anno: anno,
            val: r_c_literal(anno: anno, val: b),
            size: r_c_literal(anno: anno, val: 8),
            unit: r_c_literal(anno: anno, val: 1),
            type: r_c_literal(anno: anno, val: :integer),
            flags: r_c_literal(anno: anno, val: [:unsigned, :big])
          )
        end

      _ ->
        segments
    end
  end

  defp eval_binary(r_c_binary(anno: anno, segments: ss) = bin) do
    try do
      r_c_literal(anno: anno, val: eval_binary_1(ss, <<>>))
    catch
      :impossible ->
        bin

      {:badarg, warning} ->
        add_warning(bin, warning)

        r_c_call(
          anno: anno,
          module: r_c_literal(val: :erlang),
          name: r_c_literal(val: :error),
          args: [r_c_literal(val: :badarg)]
        )
    end
  end

  defp eval_binary_1(
         [
           r_c_bitstr(
             val: r_c_literal(val: val),
             size: r_c_literal(val: sz),
             unit: r_c_literal(val: unit),
             type: r_c_literal(val: type),
             flags: r_c_literal(val: flags)
           )
           | ss
         ],
         acc0
       ) do
    endian = bs_endian(flags)

    case type do
      :binary when is_bitstring(val) ->
        cond do
          sz === :all ->
            :ok

          sz * unit <= bit_size(val) ->
            :ok

          true ->
            throw({:badarg, :embedded_binary_size})
        end

      :integer when is_integer(val) ->
        cond do
          sz * unit <= 256 ->
            :ok

          true ->
            case count_bits(val) do
              bitsNeeded when 2 * bitsNeeded >= sz * unit ->
                :ok

              _ ->
                throw(:impossible)
            end
        end

      :float when is_float(val) ->
        try do
          sz * unit
        catch
          :error, _ ->
            throw({:badarg, :bad_float_size})
        else
          32 ->
            :ok

          64 ->
            :ok

          _ ->
            throw({:badarg, :bad_float_size})
        end

      :utf8 ->
        :ok

      :utf16 ->
        :ok

      :utf32 ->
        :ok

      _ ->
        throw(:impossible)
    end

    case endian === :native and type !== :binary do
      true ->
        throw(:impossible)

      false ->
        :ok
    end

    try do
      eval_binary_2(acc0, val, sz, unit, type, endian)
    catch
      :error, _ ->
        throw(:impossible)
    else
      acc ->
        eval_binary_1(ss, acc)
    end
  end

  defp eval_binary_1([], acc) do
    acc
  end

  defp eval_binary_1(_, _) do
    throw(:impossible)
  end

  defp eval_binary_2(acc, val, size, unit, :integer, :little) do
    <<acc::bitstring, val::size(size * unit)-little>>
  end

  defp eval_binary_2(acc, val, size, unit, :integer, :big) do
    <<acc::bitstring, val::size(size * unit)-big>>
  end

  defp eval_binary_2(acc, val, _Size, _Unit, :utf8, _) do
    try do
      <<acc::bitstring, val::utf8>>
    catch
      :error, _ ->
        throw({:badarg, :bad_unicode})
    end
  end

  defp eval_binary_2(acc, val, _Size, _Unit, :utf16, :big) do
    try do
      <<acc::bitstring, val::big-utf16>>
    catch
      :error, _ ->
        throw({:badarg, :bad_unicode})
    end
  end

  defp eval_binary_2(acc, val, _Size, _Unit, :utf16, :little) do
    try do
      <<acc::bitstring, val::little-utf16>>
    catch
      :error, _ ->
        throw({:badarg, :bad_unicode})
    end
  end

  defp eval_binary_2(acc, val, _Size, _Unit, :utf32, :big) do
    try do
      <<acc::bitstring, val::big-utf32>>
    catch
      :error, _ ->
        throw({:badarg, :bad_unicode})
    end
  end

  defp eval_binary_2(acc, val, _Size, _Unit, :utf32, :little) do
    try do
      <<acc::bitstring, val::little-utf32>>
    catch
      :error, _ ->
        throw({:badarg, :bad_unicode})
    end
  end

  defp eval_binary_2(acc, val, size, unit, :float, :little) do
    <<acc::bitstring, val::size(size * unit)-little-float>>
  end

  defp eval_binary_2(acc, val, size, unit, :float, :big) do
    <<acc::bitstring, val::size(size * unit)-big-float>>
  end

  defp eval_binary_2(acc, val, :all, unit, :binary, _) do
    case bit_size(val) do
      size when rem(size, unit) === 0 ->
        <<acc::bitstring, val::size(size)-bitstring>>

      size ->
        throw({:badarg, {:embedded_unit, unit, size}})
    end
  end

  defp eval_binary_2(acc, val, size, unit, :binary, _) do
    <<acc::bitstring, val::size(size * unit)-bitstring>>
  end

  defp bs_endian([:big = e | _]) do
    e
  end

  defp bs_endian([:little = e | _]) do
    e
  end

  defp bs_endian([:native = e | _]) do
    e
  end

  defp bs_endian([_ | fs]) do
    bs_endian(fs)
  end

  defp count_bits(int) do
    count_bits_1(abs(int), 64)
  end

  defp count_bits_1(0, bits) do
    bits
  end

  defp count_bits_1(int, bits) do
    count_bits_1(int >>> 64, bits + 64)
  end

  defp useless_call(
         :effect,
         r_c_call(module: r_c_literal(val: mod), name: r_c_literal(val: name), args: args) = call
       ) do
    a = length(args)

    case :erl_bifs.is_safe(mod, name, a) do
      false ->
        case :erl_bifs.is_pure(mod, name, a) do
          true ->
            add_warning(call, :result_ignored)

          false ->
            :ok
        end

        :no

      true ->
        add_warning(call, {:no_effect, {mod, name, a}})
        {:yes, make_effect_seq(args, sub_new())}
    end
  end

  defp useless_call(_, _) do
    :no
  end

  defp make_effect_seq([h | t], sub) do
    case is_safe_simple(h) do
      true ->
        make_effect_seq(t, sub)

      false ->
        r_c_seq(arg: h, body: make_effect_seq(t, sub))
    end
  end

  defp make_effect_seq([], _) do
    void()
  end

  defp fold_apply(apply, r_c_literal(val: fun), args)
       when is_function(fun) do
    {:module, mod} = :erlang.fun_info(fun, :module)
    {:name, name} = :erlang.fun_info(fun, :name)
    {:arity, arity} = :erlang.fun_info(fun, :arity)

    cond do
      arity === length(args) ->
        r_c_call(
          anno: r_c_apply(apply, :anno),
          module: r_c_literal(val: mod),
          name: r_c_literal(val: name),
          args: args
        )

      true ->
        apply
    end
  end

  defp fold_apply(apply, _, _) do
    apply
  end

  defp call(r_c_call(args: as0) = call0, r_c_literal(val: m) = m0, r_c_literal(val: n) = n0, sub) do
    as1 = expr_list(as0, :value, sub)

    case simplify_call(call0, m, n, as1) do
      r_c_literal() = lit ->
        lit

      r_c_call(args: as) = call ->
        case :erlang.get(:no_inline_list_funcs) do
          true ->
            fold_call(call, m0, n0, as, sub)

          false ->
            case :sys_core_fold_lists.call(call, m, n, as) do
              :none ->
                fold_call(call, m0, n0, as, sub)

              core ->
                expr(core, sub)
            end
        end
    end
  end

  defp call(r_c_call(args: as0) = call, m, n, sub) do
    as = expr_list(as0, :value, sub)
    fold_call(r_c_call(call, args: as), m, n, as, sub)
  end

  defp simplify_call(call, :maps, :get, [key, map]) do
    rewrite_call(call, :erlang, :map_get, [key, map])
  end

  defp simplify_call(call, :maps, :is_key, [key, map]) do
    rewrite_call(call, :erlang, :is_map_key, [key, map])
  end

  defp simplify_call(_Call, :maps, :new, []) do
    r_c_literal(val: %{})
  end

  defp simplify_call(call, :maps, :size, [map]) do
    rewrite_call(call, :erlang, :map_size, [map])
  end

  defp simplify_call(call, _, _, args) do
    r_c_call(call, args: args)
  end

  defp rewrite_call(call, mod, func, args) do
    modLit = r_c_literal(val: mod)
    funcLit = r_c_literal(val: func)
    r_c_call(call, module: modLit, name: funcLit, args: args)
  end

  defp fold_call(call, r_c_literal(val: m), r_c_literal(val: f), args, sub) do
    fold_call_1(call, m, f, args, sub)
  end

  defp fold_call(call, _M, _N, _Args, _Sub) do
    call
  end

  defp fold_call_1(call, :erlang, :apply, [fun, args], _) do
    simplify_fun_apply(call, fun, args)
  end

  defp fold_call_1(call, :erlang, :apply, [mod, func, args], _) do
    simplify_apply(call, mod, func, args)
  end

  defp fold_call_1(call, mod, name, args, sub) do
    numArgs = length(args)

    case :erl_bifs.is_pure(mod, name, numArgs) do
      false ->
        call

      true ->
        fold_call_2(call, mod, name, args, sub)
    end
  end

  defp fold_call_2(call, module, name, args, sub) do
    case all(&:cerl.is_literal/1, args) do
      true ->
        fold_lit_args(call, module, name, args)

      false ->
        fold_non_lit_args(call, module, name, args, sub)
    end
  end

  defp fold_lit_args(call, module, name, args0) do
    args =
      for a <- args0 do
        :cerl.concrete(a)
      end

    try do
      apply(module, name, args)
    catch
      :error, reason ->
        eval_failure(call, reason)
    else
      val ->
        case :cerl.is_literal_term(val) do
          true ->
            :cerl.ann_abstract(:cerl.get_ann(call), val)

          false ->
            call
        end
    end
  end

  defp fold_non_lit_args(call, :erlang, :length, [arg], _) do
    eval_length(call, arg)
  end

  defp fold_non_lit_args(call, :erlang, :++, [arg1, arg2], _) do
    eval_append(call, arg1, arg2)
  end

  defp fold_non_lit_args(call, :lists, :append, [arg1, arg2], _) do
    eval_append(call, arg1, arg2)
  end

  defp fold_non_lit_args(call, _, _, _, _) do
    call
  end

  defp eval_length(call, core) do
    eval_length(call, core, 0)
  end

  defp eval_length(call, r_c_literal(val: val), len0) do
    try do
      len = len0 + length(val)
      r_c_literal(anno: r_c_call(call, :anno), val: len)
    catch
      _, _ ->
        eval_failure(call, :badarg)
    end
  end

  defp eval_length(call, r_c_cons(tl: t), len) do
    eval_length(call, t, len + 1)
  end

  defp eval_length(call, _List, 0) do
    call
  end

  defp eval_length(call, list, len) do
    a = r_c_call(call, :anno)

    r_c_call(
      anno: a,
      module: r_c_literal(anno: a, val: :erlang),
      name: r_c_literal(anno: a, val: :+),
      args: [r_c_literal(anno: a, val: len), r_c_call(call, args: [list])]
    )
  end

  defp eval_append(call, r_c_literal(val: cs1) = s1, r_c_literal(val: cs2)) do
    try do
      r_c_literal(s1, val: cs1 ++ cs2)
    catch
      :error, :badarg ->
        eval_failure(call, :badarg)
    end
  end

  defp eval_append(call, r_c_literal(val: cs), list) when length(cs) <= 4 do
    anno = r_c_call(call, :anno)

    foldr(
      fn c, l ->
        ann_c_cons(anno, r_c_literal(val: c), l)
      end,
      list,
      cs
    )
  end

  defp eval_append(call, r_c_cons(anno: anno, hd: h, tl: t), list) do
    ann_c_cons(anno, h, eval_append(call, t, list))
  end

  defp eval_append(call, x, y) do
    r_c_call(call, args: [x, y])
  end

  defp eval_failure(call, reason) do
    add_warning(call, {:eval_failure, reason})

    r_c_call(call,
      module: r_c_literal(val: :erlang),
      name: r_c_literal(val: :error),
      args: [r_c_literal(val: reason)]
    )
  end

  defp simplify_apply(call, mod, func, args0) do
    case is_atom_or_var(mod) and is_atom_or_var(func) do
      true ->
        case get_fixed_args(args0, []) do
          :error ->
            call

          {:ok, args} ->
            r_c_call(call, module: mod, name: func, args: args)
        end

      false ->
        call
    end
  end

  defp is_atom_or_var(r_c_literal(val: atom)) when is_atom(atom) do
    true
  end

  defp is_atom_or_var(r_c_var()) do
    true
  end

  defp is_atom_or_var(_) do
    false
  end

  defp simplify_fun_apply(r_c_call(anno: anno) = call, fun, args0) do
    case get_fixed_args(args0, []) do
      :error ->
        call

      {:ok, args} ->
        r_c_apply(anno: anno, op: fun, args: args)
    end
  end

  defp get_fixed_args(r_c_literal(val: moreArgs0), args)
       when length(moreArgs0) >= 0 do
    moreArgs =
      for arg <- moreArgs0 do
        r_c_literal(val: arg)
      end

    {:ok, reverse(args, moreArgs)}
  end

  defp get_fixed_args(r_c_cons(hd: arg, tl: t), args) do
    get_fixed_args(t, [arg | args])
  end

  defp get_fixed_args(_, _) do
    :error
  end

  defp clause(r_c_clause(pats: ps0) = cl, cexpr, ctxt, sub0) do
    try do
      pattern_list(ps0, sub0)
    catch
      :nomatch ->
        r_c_clause(cl,
          anno: [:compiler_generated],
          guard: r_c_literal(val: false)
        )
    else
      {ps1, sub1} ->
        clause_1(cl, ps1, cexpr, ctxt, sub1)
    end
  end

  defp clause_1(r_c_clause(guard: g0, body: b0) = cl, ps1, cexpr, ctxt, sub1) do
    gSub =
      case {cexpr, ps1, g0} do
        {_, _, r_c_literal()} ->
          sub1

        {r_c_var(), [r_c_var() = var], _} ->
          case :cerl.is_c_fname(cexpr) do
            false ->
              sub_set_var(var, cexpr, sub1)

            true ->
              sub1
          end

        _ ->
          sub1
      end

    g1 = guard(g0, gSub)
    b1 = body(b0, ctxt, sub1)
    r_c_clause(cl, pats: ps1, guard: g1, body: b1)
  end

  defp let_substs(vs0, as0, sub0) do
    {vs1, sub1} = var_list(vs0, sub0)
    {vs2, as1, ss} = let_substs_1(vs1, as0, sub1)

    sub2 =
      sub_add_scope(
        for r_c_var(name: v) <- vs2 do
          v
        end,
        sub1
      )

    {vs2, as1,
     foldl(
       fn {v, s}, sub ->
         sub_set_name(v, s, sub)
       end,
       sub2,
       ss
     )}
  end

  defp let_substs_1(vs, r_c_values(es: as), sub) do
    let_subst_list(vs, as, sub)
  end

  defp let_substs_1([v], a, sub) do
    let_subst_list([v], [a], sub)
  end

  defp let_substs_1(vs, a, _) do
    {vs, a, []}
  end

  defp let_subst_list([v | vs0], [a0 | as0], sub) do
    {vs1, as1, ss} = let_subst_list(vs0, as0, sub)

    case is_subst(a0) do
      true ->
        a =
          case is_compiler_generated(v) and not is_compiler_generated(a0) do
            true ->
              ann = [:compiler_generated | :cerl.get_ann(a0)]
              :cerl.set_ann(a0, ann)

            false ->
              a0
          end

        {vs1, as1, sub_subst_var(v, a, sub) ++ ss}

      false ->
        {[v | vs1], [a0 | as1], ss}
    end
  end

  defp let_subst_list([], [], _) do
    {[], [], []}
  end

  defp pattern(r_c_var() = pat, isub, osub) do
    case sub_is_in_scope(pat, isub) do
      true ->
        v1 = make_var_name()
        pat1 = r_c_var(name: v1)
        {pat1, sub_set_var(pat, pat1, sub_add_scope([v1], osub))}

      false ->
        {pat, sub_add_scope([r_c_var(pat, :name)], osub)}
    end
  end

  defp pattern(r_c_literal() = pat, _, osub) do
    {pat, osub}
  end

  defp pattern(r_c_cons(anno: anno, hd: h0, tl: t0), isub, osub0) do
    {h1, osub1} = pattern(h0, isub, osub0)
    {t1, osub2} = pattern(t0, isub, osub1)
    {ann_c_cons(anno, h1, t1), osub2}
  end

  defp pattern(r_c_tuple(anno: anno, es: es0), isub, osub0) do
    {es1, osub1} = pattern_list(es0, isub, osub0)
    {ann_c_tuple(anno, es1), osub1}
  end

  defp pattern(r_c_map(anno: anno, es: es0) = map, isub, osub0) do
    {es1, osub1} = map_pair_pattern_list(es0, isub, osub0)
    {r_c_map(map, anno: anno, es: es1), osub1}
  end

  defp pattern(r_c_binary(segments: v0) = pat, isub, osub0) do
    {v1, osub1} = bin_pattern_list(v0, isub, osub0)
    {r_c_binary(pat, segments: v1), osub1}
  end

  defp pattern(r_c_alias(var: v0, pat: p0) = pat, isub, osub0) do
    {v1, osub1} = pattern(v0, isub, osub0)
    {p1, osub} = pattern(p0, isub, osub1)
    {r_c_alias(pat, var: v1, pat: p1), osub}
  end

  defp map_pair_pattern_list(ps0, isub, osub0) do
    {ps, {_, osub}} = mapfoldl(&map_pair_pattern/2, {isub, osub0}, ps0)
    {ps, osub}
  end

  defp map_pair_pattern(
         r_c_map_pair(op: r_c_literal(val: :exact), key: k0, val: v0) = pair,
         {isub, osub0}
       ) do
    k = expr(k0, isub)
    {v, osub} = pattern(v0, isub, osub0)
    {r_c_map_pair(pair, key: k, val: v), {isub, osub}}
  end

  defp bin_pattern_list(ps, isub, osub0) do
    mapfoldl(
      fn p, osub ->
        bin_pattern(p, isub, osub)
      end,
      osub0,
      ps
    )
  end

  defp bin_pattern(r_c_bitstr(val: e0, size: size0) = pat0, isub, osub0) do
    size2 =
      case {size0, expr(size0, isub)} do
        {r_c_var(), r_c_literal(val: :all)} ->
          r_c_literal(anno: [:size_was_all], val: :bad_size)

        {_, size1} ->
          size1
      end

    {e1, osub} = pattern(e0, isub, osub0)
    pat = r_c_bitstr(pat0, val: e1, size: size2)
    bin_pat_warn(pat)
    {pat, osub}
  end

  defp pattern_list(ps, sub) do
    pattern_list(ps, sub, sub)
  end

  defp pattern_list(ps0, isub, osub0) do
    mapfoldl(
      fn p, osub ->
        pattern(p, isub, osub)
      end,
      osub0,
      ps0
    )
  end

  defp var_list(vs, sub0) do
    mapfoldl(
      fn r_c_var() = v, sub ->
        pattern(v, sub, sub)
      end,
      sub0,
      vs
    )
  end

  defp bin_pat_warn(
         r_c_bitstr(
           type: r_c_literal(val: type),
           val: val0,
           size: r_c_literal(anno: sizeAnno, val: sz),
           unit: r_c_literal(val: unit),
           flags: fl
         ) = pat
       ) do
    case {type, sz} do
      {_, _} when is_integer(sz) and sz >= 0 ->
        :ok

      {:binary, :all} ->
        :ok

      {:utf8, :undefined} ->
        :ok

      {:utf16, :undefined} ->
        :ok

      {:utf32, :undefined} ->
        :ok

      {_, _} ->
        case member(:size_was_all, sizeAnno) do
          true ->
            add_warning(pat, {:nomatch_bit_syntax_size, :all})

          false ->
            add_warning(pat, {:nomatch_bit_syntax_size, sz})
        end

        throw(:nomatch)
    end

    case {type, val0} do
      {:integer, r_c_literal(val: val)} when is_integer(val) ->
        signedness = signedness(fl)
        totalSz = sz * unit
        bit_pat_warn_int(val, totalSz, signedness, pat)

      {:float, r_c_literal(val: val)} when is_float(val) ->
        :ok

      {:utf8, r_c_literal(val: val)} when is_integer(val) ->
        bit_pat_warn_unicode(val, pat)

      {:utf16, r_c_literal(val: val)} when is_integer(val) ->
        bit_pat_warn_unicode(val, pat)

      {:utf32, r_c_literal(val: val)} when is_integer(val) ->
        bit_pat_warn_unicode(val, pat)

      {_, r_c_literal(val: val)} ->
        add_warning(pat, {:nomatch_bit_syntax_type, val, type})
        throw(:nomatch)

      {_, _} ->
        :ok
    end
  end

  defp bin_pat_warn(r_c_bitstr(type: r_c_literal(val: type), val: val0, flags: fl) = pat) do
    case {type, val0} do
      {:integer, r_c_literal(val: val)} when is_integer(val) ->
        case signedness(fl) do
          :unsigned when val < 0 ->
            add_warning(pat, {:nomatch_bit_syntax_unsigned, val})
            throw(:nomatch)

          _ ->
            :ok
        end

      {:float, r_c_literal(val: val)} when is_float(val) ->
        :ok

      {_, r_c_literal(val: val)} ->
        add_warning(pat, {:nomatch_bit_syntax_type, val, type})
        throw(:nomatch)

      {_, _} ->
        :ok
    end
  end

  defp bit_pat_warn_int(val, 0, :signed, pat) do
    cond do
      val === 0 ->
        :ok

      true ->
        add_warning(
          pat,
          {:nomatch_bit_syntax_truncated, :signed, val, 0}
        )

        throw(:nomatch)
    end
  end

  defp bit_pat_warn_int(val, sz, :signed, pat) do
    cond do
      val < 0 and val >>> (sz - 1) !== -1 ->
        add_warning(
          pat,
          {:nomatch_bit_syntax_truncated, :signed, val, sz}
        )

        throw(:nomatch)

      val > 0 and val >>> (sz - 1) !== 0 ->
        add_warning(
          pat,
          {:nomatch_bit_syntax_truncated, :signed, val, sz}
        )

        throw(:nomatch)

      true ->
        :ok
    end
  end

  defp bit_pat_warn_int(val, _Sz, :unsigned, pat) when val < 0 do
    add_warning(pat, {:nomatch_bit_syntax_unsigned, val})
    throw(:nomatch)
  end

  defp bit_pat_warn_int(val, sz, :unsigned, pat) do
    cond do
      val >>> sz === 0 ->
        :ok

      true ->
        add_warning(
          pat,
          {:nomatch_bit_syntax_truncated, :unsigned, val, sz}
        )

        throw(:nomatch)
    end
  end

  defp bit_pat_warn_unicode(u, _Pat) when 0 <= u and u <= 1_114_111 do
    :ok
  end

  defp bit_pat_warn_unicode(u, pat) do
    add_warning(pat, {:nomatch_bit_syntax_unicode, u})
    throw(:nomatch)
  end

  defp signedness(r_c_literal(val: flags)) do
    [s] =
      for f <- flags,
          f === :signed or f === :unsigned do
        f
      end

    s
  end

  defp is_subst(r_c_var(name: {_, _})) do
    false
  end

  defp is_subst(r_c_var()) do
    true
  end

  defp is_subst(r_c_literal()) do
    true
  end

  defp is_subst(_) do
    false
  end

  defp sub_new() do
    r_sub(v: :orddict.new(), s: :cerl_sets.new(), t: %{})
  end

  defp sub_new(r_sub() = sub) do
    r_sub(sub, v: :orddict.new(), t: %{})
  end

  defp sub_get_var(r_c_var(name: v) = var, r_sub(v: s)) do
    case :orddict.find(v, s) do
      {:ok, val} ->
        val

      :error ->
        var
    end
  end

  defp sub_set_var(r_c_var(name: v), val, sub) do
    sub_set_name(v, val, sub)
  end

  defp sub_set_name(v, val, r_sub(v: s, s: scope, t: tdb0) = sub) do
    tdb1 = kill_types(v, tdb0)
    tdb = copy_type(v, val, tdb1)
    r_sub(sub, v: :orddict.store(v, val, s), s: :cerl_sets.add_element(v, scope), t: tdb)
  end

  defp sub_subst_var(r_c_var(name: v), val, r_sub(v: s0)) do
    [{v, val}] ++
      for {k, r_c_var(name: v1)} <- s0, v1 === v do
        {k, val}
      end
  end

  defp sub_add_scope(vs, r_sub(s: scope0) = sub) do
    scope =
      foldl(
        fn v, s when is_integer(v) or is_atom(v) ->
          :cerl_sets.add_element(v, s)
        end,
        scope0,
        vs
      )

    r_sub(sub, s: scope)
  end

  defp sub_subst_scope(r_sub(v: s0, s: scope) = sub) do
    initial =
      case s0 do
        [{negInt, _} | _]
        when is_integer(negInt) and
               negInt < 0 ->
          negInt - 1

        _ ->
          -1
      end

    s = sub_subst_scope_1(:cerl_sets.to_list(scope), initial, s0)
    r_sub(sub, v: :orddict.from_list(s))
  end

  defp sub_subst_scope_1([h | t], key, acc) do
    sub_subst_scope_1(t, key - 1, [{key, r_c_var(name: h)} | acc])
  end

  defp sub_subst_scope_1([], _, acc) do
    acc
  end

  defp sub_is_in_scope(r_c_var(name: v), r_sub(s: scope)) do
    :cerl_sets.is_element(v, scope)
  end

  defp warn_no_clause_match(caseOrig, caseOpt) do
    origCs = :cerl.case_clauses(caseOrig)
    optCs = :cerl.case_clauses(caseOpt)

    case any(
           fn c ->
             not is_compiler_generated(c)
           end,
           origCs
         ) and all(&is_compiler_generated/1, optCs) do
      true ->
        add_warning(caseOrig, :no_clause_match)

      false ->
        :ok
    end
  end

  defp clauses(e, [c0 | cs], ctxt, sub, litExpr, anno) do
    r_c_clause(pats: ps, guard: g) = c1 = clause(c0, e, ctxt, sub)

    case {will_match(e, ps), will_succeed(g)} do
      {:yes, :yes} ->
        case litExpr do
          false ->
            line = get_line(:cerl.get_ann(c1))
            shadow_warning(cs, line, anno)

          true ->
            :ok
        end

        [c1]

      {_Mat, :no} ->
        add_warning(c1, :nomatch_guard)
        clauses(e, cs, ctxt, sub, litExpr, anno)

      {_Mat, _Suc} ->
        [c1 | clauses(e, cs, ctxt, sub, litExpr, anno)]
    end
  end

  defp clauses(_, [], _, _, _, _) do
    []
  end

  defp shadow_warning([c | cs], :none, anno) do
    add_warning(c, :nomatch_shadow)
    shadow_warning(cs, :none, anno)
  end

  defp shadow_warning([c | cs], line, anno) do
    case keyfind(:function, 1, anno) do
      {:function, {name, arity}} ->
        add_warning(c, {:nomatch_shadow, line, {name, arity}})

      _ ->
        add_warning(c, {:nomatch_shadow, line})
    end

    shadow_warning(cs, line, anno)
  end

  defp shadow_warning([], _, _) do
    :ok
  end

  defp will_succeed(r_c_literal(val: true)) do
    :yes
  end

  defp will_succeed(r_c_literal(val: false)) do
    :no
  end

  defp will_succeed(_Guard) do
    :maybe
  end

  defp will_match(r_c_values(es: es), ps) do
    will_match_1(:cerl_clauses.match_list(ps, es))
  end

  defp will_match(e, [p]) do
    will_match_1(:cerl_clauses.match(p, e))
  end

  defp will_match_1({false, _}) do
    :maybe
  end

  defp will_match_1({true, _}) do
    :yes
  end

  defp opt_bool_case(r_c_case() = case__, r_sub(in_guard: true)) do
    case__
  end

  defp opt_bool_case(r_c_case(arg: arg) = case0, r_sub(in_guard: false)) do
    case is_bool_expr(arg) do
      false ->
        case0

      true ->
        try do
          opt_bool_clauses(case0)
        catch
          :impossible ->
            case0
        else
          case__ ->
            opt_bool_not(case__)
        end
    end
  end

  defp opt_bool_clauses(r_c_case(clauses: cs) = case__) do
    r_c_case(case__, clauses: opt_bool_clauses(cs, false, false))
  end

  defp opt_bool_clauses(cs, true, true) do
    case cs do
      [_ | _] ->
        shadow_warning(cs, :none, [])
        []

      [] ->
        []
    end
  end

  defp opt_bool_clauses(
         [
           r_c_clause(
             pats: [r_c_literal(val: lit)],
             guard: r_c_literal(val: true)
           ) = c
           | cs
         ],
         seenT,
         seenF
       ) do
    case is_boolean(lit) do
      false ->
        add_warning(c, :nomatch_clause_type)
        opt_bool_clauses(cs, seenT, seenF)

      true ->
        case {lit, seenT, seenF} do
          {false, _, false} ->
            [c | opt_bool_clauses(cs, seenT, true)]

          {true, false, _} ->
            [c | opt_bool_clauses(cs, true, seenF)]

          _ ->
            add_warning(c, :nomatch_shadow)
            opt_bool_clauses(cs, seenT, seenF)
        end
    end
  end

  defp opt_bool_clauses(
         [r_c_clause(pats: ps, guard: r_c_literal(val: true)) = c | cs],
         seenT,
         seenF
       ) do
    case ps do
      [r_c_var()] ->
        throw(:impossible)

      [r_c_alias()] ->
        throw(:impossible)

      _ ->
        add_warning(c, :nomatch_clause_type)
        opt_bool_clauses(cs, seenT, seenF)
    end
  end

  defp opt_bool_clauses([_ | _], _, _) do
    throw(:impossible)
  end

  defp opt_bool_not(r_c_case(arg: arg, clauses: cs0) = case0) do
    case arg do
      r_c_call(
        anno: anno,
        module: r_c_literal(val: :erlang),
        name: r_c_literal(val: :not),
        args: [expr]
      ) ->
        cs =
          for c <- cs0 do
            opt_bool_not_invert(c)
          end ++
            [
              r_c_clause(
                anno: [:compiler_generated],
                pats: [r_c_var(name: :cor_variable)],
                guard: r_c_literal(val: true),
                body:
                  r_c_call(
                    anno: anno,
                    module: r_c_literal(val: :erlang),
                    name: r_c_literal(val: :error),
                    args: [r_c_literal(val: :badarg)]
                  )
              )
            ]

        case__ = r_c_case(case0, arg: expr, clauses: cs)
        opt_bool_not(case__)

      _ ->
        opt_bool_case_redundant(case0)
    end
  end

  defp opt_bool_not_invert(r_c_clause(pats: [r_c_literal(val: bool)]) = c) do
    r_c_clause(c, pats: [r_c_literal(val: not bool)])
  end

  defp opt_bool_case_redundant(r_c_case(arg: arg, clauses: cs) = case__) do
    case all(&opt_bool_case_redundant_1/1, cs) do
      true ->
        arg

      false ->
        case__
    end
  end

  defp opt_bool_case_redundant_1(
         r_c_clause(pats: [r_c_literal(val: b)], body: r_c_literal(val: b))
       ) do
    true
  end

  defp opt_bool_case_redundant_1(_) do
    false
  end

  defp eval_case(
         r_c_case(
           arg: e,
           clauses: [r_c_clause(pats: ps0, guard: r_c_literal(val: true), body: b)]
         ) = case__,
         sub
       ) do
    es =
      case :cerl.is_c_values(e) do
        true ->
          :cerl.values_es(e)

        false ->
          [e]
      end

    vs = make_vars([], length(es))

    case :cerl_clauses.match_list(ps0, vs) do
      {false, _} ->
        case__

      {true, bs} ->
        eval_case_warn(b)
        {ps, as} = unzip(bs)
        innerLet = :cerl.c_let(ps, :core_lib.make_values(as), b)
        let = :cerl.c_let(vs, e, innerLet)
        expr(let, sub_new(sub))
    end
  end

  defp eval_case(case__, _) do
    case__
  end

  defp eval_case_warn(
         r_c_primop(anno: anno, name: r_c_literal(val: :match_fail), args: [_]) = core
       ) do
    case keyfind(:eval_failure, 1, anno) do
      false ->
        :ok

      {:eval_failure, reason} ->
        add_warning(core, {:eval_failure, reason})
    end
  end

  defp eval_case_warn(_) do
    :ok
  end

  defp case_opt(arg, cs0, sub) do
    cs1 =
      for c <- cs0 do
        {:cerl.clause_pats(c), c, [], []}
      end

    args0 =
      case :cerl.is_c_values(arg) do
        false ->
          [arg]

        true ->
          :cerl.values_es(arg)
      end

    litExpr = :cerl.is_literal(arg)
    {args, cs2} = case_opt_args(args0, cs1, sub, litExpr, [])

    cs =
      for {[], c, ps, bs} <- cs2 do
        :cerl.update_c_clause(
          c,
          reverse(ps),
          letify(bs, :cerl.clause_guard(c)),
          letify(bs, :cerl.clause_body(c))
        )
      end

    {:core_lib.make_values(args), cs}
  end

  defp case_opt_args([a0 | as0], cs0, sub, litExpr, acc) do
    case case_opt_arg(a0, sub, cs0, litExpr) do
      {:error, cs1} ->
        cs =
          for {[p | ps], c, psAcc, bs} <- cs1 do
            {ps, c, [p | psAcc], bs}
          end

        case_opt_args(as0, cs, sub, litExpr, [a0 | acc])

      {:ok, as1, cs} ->
        case_opt_args(as1 ++ as0, cs, sub, litExpr, acc)
    end
  end

  defp case_opt_args([], cs, _Sub, _LitExpr, acc) do
    {reverse(acc), cs}
  end

  defp case_opt_arg(e0, sub, cs, litExpr) do
    case :cerl.is_c_var(e0) do
      false ->
        case_opt_arg_1(e0, cs, litExpr)

      true ->
        case case_will_var_match(cs) do
          true ->
            {:error, cs}

          false ->
            e = case_expand_var(e0, sub)
            case_opt_arg_1(e, cs, litExpr)
        end
    end
  end

  defp case_opt_arg_1(e0, cs0, litExpr) do
    case :cerl.is_data(e0) do
      false ->
        {:error, cs0}

      true ->
        e = case_opt_compiler_generated(e0)
        cs = case_opt_nomatch(e, cs0, litExpr)

        case :cerl.is_literal(e) do
          true ->
            case_opt_lit(e, cs)

          false ->
            case_opt_data(e, cs)
        end
    end
  end

  defp case_will_var_match(cs) do
    all(
      fn {[p | _], _, _, _} ->
        case :cerl_clauses.match(p, :any) do
          {true, _} ->
            true

          _ ->
            false
        end
      end,
      cs
    )
  end

  defp case_opt_compiler_generated(core) do
    f = fn c ->
      case :cerl.type(c) do
        :alias ->
          c

        :var ->
          c

        _ ->
          :cerl.set_ann(c, [:compiler_generated])
      end
    end

    :cerl_trees.map(f, core)
  end

  defp case_expand_var(e, r_sub(t: tdb)) do
    key = :cerl.var_name(e)

    case tdb do
      %{^key => t} ->
        t

      _ ->
        e
    end
  end

  defp case_opt_nomatch(e, [{[p | _], c, _, _} = current | cs], litExpr) do
    case :cerl_clauses.match(p, e) do
      :none ->
        case litExpr do
          false ->
            add_warning(c, :nomatch_clause_type)

          true ->
            :ok
        end

        case_opt_nomatch(e, cs, litExpr)

      _ ->
        [current | case_opt_nomatch(e, cs, litExpr)]
    end
  end

  defp case_opt_nomatch(_, [], _) do
    []
  end

  defp case_opt_lit(lit, cs0) do
    try do
      case_opt_lit_1(lit, cs0)
    catch
      :impossible ->
        {:error, cs0}
    else
      cs ->
        {:ok, [], cs}
    end
  end

  defp case_opt_lit_1(e, [{[p | ps], c, psAcc, bs0} | cs]) do
    case :cerl_clauses.match(p, e) do
      {true, bs} ->
        [{ps, c, psAcc, bs ++ bs0} | case_opt_lit_1(e, cs)]

      {false, _} ->
        throw(:impossible)
    end
  end

  defp case_opt_lit_1(_, []) do
    []
  end

  defp case_opt_data(e, cs0) do
    typeSig = {:cerl.data_type(e), :cerl.data_arity(e)}

    try do
      case_opt_data_1(cs0, typeSig)
    catch
      :impossible ->
        {:error, cs0}
    else
      cs ->
        es = :cerl.data_es(e)
        {:ok, es, cs}
    end
  end

  defp case_opt_data_1([{[p0 | ps0], c, psAcc, bs0} | cs], typeSig) do
    p = case_opt_compiler_generated(p0)
    {ps1, bs} = case_opt_data_2(p, typeSig, bs0)

    [
      {ps1 ++ ps0, c, psAcc, bs}
      | case_opt_data_1(
          cs,
          typeSig
        )
    ]
  end

  defp case_opt_data_1([], _) do
    []
  end

  defp case_opt_data_2(p, typeSig, bs0) do
    case case_analyze_pat(p) do
      {[], pat} when pat !== :none ->
        dataEs = :cerl.data_es(p)
        {dataEs, bs0}

      {[v | vs], :none} ->
        {type, arity} = typeSig
        ann = [:compiler_generated]
        vars = make_vars(ann, arity)
        data = :cerl.ann_make_data(ann, type, vars)

        bs = [
          {v, data}
          | for var <- vs do
              {var, v}
            end ++ bs0
        ]

        {vars, bs}

      {[v | vs], pat} when pat !== :none ->
        {type, _} = typeSig
        dataEs = :cerl.data_es(pat)
        vars = pat_to_expr_list(dataEs)
        ann = [:compiler_generated]
        data = :cerl.ann_make_data(ann, type, vars)

        bs = [
          {v, data}
          | for var <- vs do
              {var, v}
            end ++ bs0
        ]

        {dataEs, bs}
    end
  end

  defp case_analyze_pat(p) do
    case_analyze_pat_1(p, [], :none)
  end

  defp case_analyze_pat_1(p, vs, pat) do
    case :cerl.type(p) do
      :alias ->
        v = :cerl.alias_var(p)
        apat = :cerl.alias_pat(p)
        case_analyze_pat_1(apat, [v | vs], pat)

      :var ->
        {[p | vs], pat}

      _ ->
        {vs, p}
    end
  end

  defp pat_to_expr(p) do
    case :cerl.type(p) do
      :alias ->
        :cerl.alias_var(p)

      :var ->
        p

      _ ->
        case :cerl.is_data(p) do
          false ->
            throw(:impossible)

          true ->
            es = pat_to_expr_list(:cerl.data_es(p))
            :cerl.update_data(p, :cerl.data_type(p), es)
        end
    end
  end

  defp pat_to_expr_list(ps) do
    for p <- ps do
      pat_to_expr(p)
    end
  end

  defp make_vars(a, max) do
    make_vars(a, 1, max)
  end

  defp make_vars(a, i, max) when i <= max do
    [make_var(a) | make_vars(a, i + 1, max)]
  end

  defp make_vars(_, _, _) do
    []
  end

  defp make_var(a) do
    r_c_var(anno: a, name: make_var_name())
  end

  defp make_var_name() do
    n = :erlang.get(:new_var_num)
    :erlang.put(:new_var_num, n + 1)
    n
  end

  defp letify(bs, body) do
    ann = :cerl.get_ann(body)

    foldr(
      fn {v, val}, b ->
        :cerl.ann_c_let(ann, [v], val, b)
      end,
      body,
      bs
    )
  end

  defp opt_not_in_let(r_c_let(vars: [_] = vs0, arg: arg0, body: body0) = let) do
    case opt_not_in_let_0(vs0, arg0, body0) do
      {[], r_c_values(es: []), body} ->
        body

      {vs, arg, body} ->
        r_c_let(let, vars: vs, arg: arg, body: body)
    end
  end

  defp opt_not_in_let(let) do
    let
  end

  defp opt_not_in_let_0([r_c_var(name: v)] = vs0, arg0, body0) do
    case :cerl.type(body0) do
      :call ->
        case opt_not_in_let_1(v, body0, arg0) do
          :no ->
            {vs0, arg0, body0}

          {:yes, body} ->
            {[], r_c_values(es: []), body}
        end

      :let ->
        letArg = :cerl.let_arg(body0)

        case opt_not_in_let_1(v, letArg, arg0) do
          :no ->
            {vs0, arg0, body0}

          {:yes, arg} ->
            letBody = :cerl.let_body(body0)

            case :core_lib.is_var_used(v, letBody) do
              true ->
                {vs0, arg0, body0}

              false ->
                letVars = :cerl.let_vars(body0)
                {letVars, arg, letBody}
            end
        end

      _ ->
        {vs0, arg0, body0}
    end
  end

  defp opt_not_in_let_1(v, call, body) do
    case call do
      r_c_call(
        module: r_c_literal(val: :erlang),
        name: r_c_literal(val: :not),
        args: [r_c_var(name: ^v)]
      ) ->
        opt_not_in_let_2(body, call)

      _ ->
        :no
    end
  end

  defp opt_not_in_let_2(r_c_case(clauses: cs0) = case__, notCall) do
    vars = make_vars([], 1)
    body = r_c_call(notCall, args: vars)

    cs =
      for r_c_clause(body: b) = c <- cs0 do
        let = r_c_let(vars: vars, arg: b, body: body)
        r_c_clause(c, body: opt_not_in_let(let))
      end

    {:yes, r_c_case(case__, clauses: cs)}
  end

  defp opt_not_in_let_2(r_c_call() = call0, _NotCall) do
    invert_call(call0)
  end

  defp opt_not_in_let_2(_, _) do
    :no
  end

  defp invert_call(
         r_c_call(module: r_c_literal(val: :erlang), name: r_c_literal(val: name0), args: [_, _]) =
           call
       ) do
    case inverse_rel_op(name0) do
      :no ->
        :no

      name ->
        {:yes, r_c_call(call, name: r_c_literal(val: name))}
    end
  end

  defp invert_call(r_c_call()) do
    :no
  end

  defp inverse_rel_op(:"=:=") do
    :"=/="
  end

  defp inverse_rel_op(:"=/=") do
    :"=:="
  end

  defp inverse_rel_op(:==) do
    :"/="
  end

  defp inverse_rel_op(:"/=") do
    :==
  end

  defp inverse_rel_op(:>) do
    :"=<"
  end

  defp inverse_rel_op(:<) do
    :>=
  end

  defp inverse_rel_op(:>=) do
    :<
  end

  defp inverse_rel_op(:"=<") do
    :>
  end

  defp inverse_rel_op(_) do
    :no
  end

  defp opt_bool_case_in_let(r_c_let(vars: vs, arg: arg, body: b) = let, sub) do
    opt_bool_case_in_let_1(vs, arg, b, let, sub)
  end

  defp opt_bool_case_in_let_1(
         [r_c_var(name: v)],
         arg,
         r_c_case(arg: r_c_var(name: v)) = case0,
         let,
         sub
       ) do
    case is_simple_case_arg(arg) do
      true ->
        case__ = opt_bool_case(r_c_case(case0, arg: arg), sub)

        case :core_lib.is_var_used(v, case__) do
          false ->
            case__

          true ->
            let
        end

      false ->
        let
    end
  end

  defp opt_bool_case_in_let_1(_, _, _, let, _) do
    let
  end

  defp is_simple_case_arg(r_c_cons()) do
    true
  end

  defp is_simple_case_arg(r_c_tuple()) do
    true
  end

  defp is_simple_case_arg(r_c_call()) do
    true
  end

  defp is_simple_case_arg(r_c_apply()) do
    true
  end

  defp is_simple_case_arg(_) do
    false
  end

  defp is_bool_expr(
         r_c_call(module: r_c_literal(val: :erlang), name: r_c_literal(val: name), args: args)
       ) do
    numArgs = length(args)

    :erl_internal.comp_op(
      name,
      numArgs
    ) or
      :erl_internal.new_type_test(
        name,
        numArgs
      ) or
      :erl_internal.bool_op(
        name,
        numArgs
      )
  end

  defp is_bool_expr(
         r_c_try(
           arg: e,
           vars: [r_c_var(name: x)],
           body: r_c_var(name: x),
           handler: r_c_literal(val: false)
         )
       ) do
    is_bool_expr(e)
  end

  defp is_bool_expr(r_c_case(clauses: cs)) do
    is_bool_expr_list(cs)
  end

  defp is_bool_expr(r_c_clause(body: b)) do
    is_bool_expr(b)
  end

  defp is_bool_expr(r_c_let(body: b)) do
    is_bool_expr(b)
  end

  defp is_bool_expr(r_c_literal(val: val)) do
    is_boolean(val)
  end

  defp is_bool_expr(_) do
    false
  end

  defp is_bool_expr_list([c | cs]) do
    is_bool_expr(c) and is_bool_expr_list(cs)
  end

  defp is_bool_expr_list([]) do
    true
  end

  defp is_safe_bool_expr(core) do
    is_safe_bool_expr_1(core, :cerl_sets.new())
  end

  defp is_safe_bool_expr_1(
         r_c_call(
           module: r_c_literal(val: :erlang),
           name: r_c_literal(val: :is_function),
           args: [a, r_c_literal(val: arity)]
         ),
         _BoolVars
       )
       when is_integer(arity) and arity >= 0 do
    is_safe_simple(a)
  end

  defp is_safe_bool_expr_1(
         r_c_call(
           module: r_c_literal(val: :erlang),
           name: r_c_literal(val: :is_function)
         ),
         _BoolVars
       ) do
    false
  end

  defp is_safe_bool_expr_1(
         r_c_call(module: r_c_literal(val: :erlang), name: r_c_literal(val: name), args: args),
         boolVars
       ) do
    numArgs = length(args)

    case (:erl_internal.comp_op(
            name,
            numArgs
          ) or
            :erl_internal.new_type_test(
              name,
              numArgs
            )) and is_safe_simple_list(args) do
      true ->
        true

      false ->
        :erl_internal.bool_op(
          name,
          numArgs
        ) and
          is_safe_bool_expr_list(
            args,
            boolVars
          )
    end
  end

  defp is_safe_bool_expr_1(r_c_let(vars: vars, arg: arg, body: b), boolVars) do
    case is_safe_simple(arg) do
      true ->
        case {is_safe_bool_expr_1(arg, boolVars), vars} do
          {true, [r_c_var(name: v)]} ->
            is_safe_bool_expr_1(
              b,
              :cerl_sets.add_element(v, boolVars)
            )

          {false, _} ->
            is_safe_bool_expr_1(b, boolVars)
        end

      false ->
        false
    end
  end

  defp is_safe_bool_expr_1(r_c_literal(val: val), _BoolVars) do
    is_boolean(val)
  end

  defp is_safe_bool_expr_1(r_c_var(name: v), boolVars) do
    :cerl_sets.is_element(v, boolVars)
  end

  defp is_safe_bool_expr_1(_, _) do
    false
  end

  defp is_safe_bool_expr_list([c | cs], boolVars) do
    case is_safe_bool_expr_1(c, boolVars) do
      true ->
        is_safe_bool_expr_list(cs, boolVars)

      false ->
        false
    end
  end

  defp is_safe_bool_expr_list([], _) do
    true
  end

  defp simplify_let(r_c_let(arg: arg) = let, sub) do
    move_let_into_expr(let, arg, sub)
  end

  defp move_let_into_expr(
         r_c_let(vars: innerVs0, body: innerBody0) = inner,
         r_c_let(vars: outerVs0, arg: arg0, body: outerBody0) = outer,
         sub0
       ) do
    arg = body(arg0, sub0)
    scopeSub0 = sub_subst_scope(r_sub(sub0, t: %{}))
    {outerVs, scopeSub} = var_list(outerVs0, scopeSub0)
    outerBody = body(outerBody0, scopeSub)
    {innerVs, sub} = var_list(innerVs0, sub0)
    innerBody = body(innerBody0, sub)

    r_c_let(outer,
      vars: outerVs,
      arg: arg,
      body: r_c_let(inner, vars: innerVs, arg: outerBody, body: innerBody)
    )
  end

  defp move_let_into_expr(
         r_c_let(vars: lvs0, body: lbody0) = let,
         r_c_case(arg: cexpr0, clauses: [ca0 | cs0]) = case__,
         sub0
       ) do
    case not is_failing_clause(ca0) and are_all_failing_clauses(cs0) do
      true ->
        cexpr = body(cexpr0, sub0)
        caPats0 = r_c_clause(ca0, :pats)
        g0 = r_c_clause(ca0, :guard)
        b0 = r_c_clause(ca0, :body)
        scopeSub0 = sub_subst_scope(r_sub(sub0, t: %{}))

        try do
          pattern_list(caPats0, scopeSub0)
        catch
          :nomatch ->
            :impossible
        else
          {caPats, scopeSub} ->
            g = guard(g0, scopeSub)
            b1 = body(b0, scopeSub)
            {lvs, b2, sub1} = let_substs(lvs0, b1, sub0)

            sub2 =
              r_sub(sub1,
                s:
                  :cerl_sets.union(
                    r_sub(scopeSub, :s),
                    r_sub(sub1, :s)
                  )
              )

            lbody = body(lbody0, sub2)
            b = r_c_let(let, vars: lvs, arg: :core_lib.make_values(b2), body: lbody)
            ca = r_c_clause(ca0, pats: caPats, guard: g, body: b)

            cs =
              for c <- cs0 do
                clause(c, cexpr, :value, sub0)
              end

            r_c_case(case__, arg: cexpr, clauses: [ca | cs])
        end

      false ->
        :impossible
    end
  end

  defp move_let_into_expr(
         r_c_let(vars: lvs0, body: lbody0) = let,
         r_c_seq(arg: sarg0, body: sbody0) = seq,
         sub0
       ) do
    sarg = body(sarg0, sub0)
    sbody1 = body(sbody0, sub0)
    {lvs, sbody, sub} = let_substs(lvs0, sbody1, sub0)
    lbody = body(lbody0, sub)

    r_c_seq(seq,
      arg: sarg,
      body: r_c_let(let, vars: lvs, arg: :core_lib.make_values(sbody), body: lbody)
    )
  end

  defp move_let_into_expr(_Let, _Expr, _Sub) do
    :impossible
  end

  defp are_all_failing_clauses(cs) do
    all(&is_failing_clause/1, cs)
  end

  defp is_failing_clause(r_c_clause(body: b)) do
    will_fail(b)
  end

  defp opt_build_stacktrace(
         r_c_let(
           vars: [r_c_var(name: cooked)],
           arg: r_c_primop(name: r_c_literal(val: :build_stacktrace), args: [rawStk]),
           body: body
         ) = let
       ) do
    case body do
      r_c_call(
        module: r_c_literal(val: :erlang),
        name: r_c_literal(val: :raise),
        args: [class, exp, r_c_var(name: ^cooked)]
      ) ->
        case :core_lib.is_var_used(
               cooked,
               r_c_cons(hd: class, tl: exp)
             ) do
          true ->
            let

          false ->
            r_c_primop(name: r_c_literal(val: :raw_raise), args: [class, exp, rawStk])
        end

      r_c_let(vars: [r_c_var(name: v)], arg: arg, body: b0)
      when v !== cooked ->
        case :core_lib.is_var_used(cooked, arg) do
          false ->
            b = opt_build_stacktrace(r_c_let(let, body: b0))
            r_c_let(body, body: b)

          true ->
            let
        end

      r_c_seq(arg: arg, body: b0) ->
        case :core_lib.is_var_used(cooked, arg) do
          false ->
            b = opt_build_stacktrace(r_c_let(let, body: b0))
            r_c_seq(body, body: b)

          true ->
            let
        end

      r_c_case(clauses: cs0) ->
        nilBody = r_c_literal(val: [])

        cs1 =
          for c <- cs0 do
            r_c_clause(c, body: nilBody)
          end

        case__ = r_c_case(body, clauses: cs1)

        case :core_lib.is_var_used(cooked, case__) do
          false ->
            cs =
              for r_c_clause(body: b0) = c <- cs0 do
                b = opt_build_stacktrace(r_c_let(let, body: b0))
                r_c_clause(c, body: b)
              end

            r_c_case(body, clauses: cs)

          true ->
            let
        end

      _ ->
        let
    end
  end

  defp opt_build_stacktrace(expr) do
    expr
  end

  defp opt_case_in_let(r_c_let(vars: vs, arg: arg0, body: b) = let0) do
    case matches_data(vs, b) do
      {:yes, typeSig} ->
        case delay_build(arg0, typeSig) do
          :no ->
            let0

          {:yes, vars, arg, data} ->
            innerLet = r_c_let(let0, arg: data)
            r_c_let(let0, vars: vars, arg: arg, body: innerLet)
        end

      :no ->
        let0
    end
  end

  defp matches_data(
         [r_c_var(name: v)],
         r_c_case(arg: r_c_var(name: v), clauses: [r_c_clause(pats: [p]) | _])
       ) do
    case :cerl.is_data(p) do
      false ->
        :no

      true ->
        case :cerl.data_type(p) do
          {:atomic, _} ->
            :no

          type ->
            {:yes, {type, :cerl.data_arity(p)}}
        end
    end
  end

  defp matches_data(_, _) do
    :no
  end

  defp delay_build(core, typeSig) do
    case :cerl.is_data(core) do
      true ->
        :no

      false ->
        delay_build_1(core, typeSig)
    end
  end

  defp delay_build_1(core0, typeSig) do
    try do
      delay_build_expr(core0, typeSig)
    catch
      :impossible ->
        :no
    else
      core ->
        {type, arity} = typeSig
        ann = [:compiler_generated]
        vars = make_vars(ann, arity)
        data = :cerl.ann_make_data(ann, type, vars)
        {:yes, vars, core, data}
    end
  end

  defp delay_build_cs([r_c_clause(body: b0) = c0 | cs], typeSig) do
    b = delay_build_expr(b0, typeSig)
    c = r_c_clause(c0, body: b)
    [c | delay_build_cs(cs, typeSig)]
  end

  defp delay_build_cs([], _) do
    []
  end

  defp delay_build_expr(core, {type, arity} = typeSig) do
    case :cerl.is_data(core) do
      false ->
        delay_build_expr_1(core, typeSig)

      true ->
        case {:cerl.data_type(core), :cerl.data_arity(core)} do
          {^type, ^arity} ->
            :core_lib.make_values(:cerl.data_es(core))

          {_, _} ->
            throw(:impossible)
        end
    end
  end

  defp delay_build_expr_1(r_c_case(clauses: cs0) = case__, typeSig) do
    cs = delay_build_cs(cs0, typeSig)
    r_c_case(case__, clauses: cs)
  end

  defp delay_build_expr_1(r_c_let(body: b0) = let, typeSig) do
    b = delay_build_expr(b0, typeSig)
    r_c_let(let, body: b)
  end

  defp delay_build_expr_1(r_c_seq(body: b0) = seq, typeSig) do
    b = delay_build_expr(b0, typeSig)
    r_c_seq(seq, body: b)
  end

  defp delay_build_expr_1(core, _TypeSig) do
    case will_fail(core) do
      true ->
        core

      false ->
        throw(:impossible)
    end
  end

  defp opt_simple_let(let0, ctxt, sub) do
    case opt_not_in_let(let0) do
      r_c_let() = let ->
        opt_simple_let_0(let, ctxt, sub)

      expr ->
        expr(expr, ctxt, sub)
    end
  end

  defp opt_simple_let_0(r_c_let(arg: arg0) = let, ctxt, sub) do
    arg = body(arg0, :value, sub)

    case will_fail(arg) do
      true ->
        arg

      false ->
        opt_simple_let_1(let, arg, ctxt, sub)
    end
  end

  defp opt_simple_let_1(r_c_let(vars: vs0, body: b0) = let, arg0, ctxt, sub0) do
    {vs, args, sub1} = let_substs(vs0, arg0, sub0)
    bodySub = update_let_types(vs, args, sub1)
    sub = r_sub(sub1, v: [], s: :cerl_sets.new())
    b = body(b0, ctxt, bodySub)
    arg = :core_lib.make_values(args)
    opt_simple_let_2(let, vs, arg, b, b0, sub)
  end

  defp opt_simple_let_2(let0, vs0, arg0, body, prevBody, sub) do
    case {vs0, arg0, body} do
      {[r_c_var(name: v)], arg1, r_c_var(name: v)} ->
        arg1

      {[], r_c_values(es: []), _} ->
        body

      {[r_c_var(name: v) = var] = vars0, arg1, body} ->
        case :core_lib.is_var_used(v, body) do
          false ->
            arg = maybe_suppress_warnings(arg1, var, prevBody)
            r_c_seq(arg: arg, body: body)

          true ->
            let1 = r_c_let(let0, vars: vars0, arg: arg1, body: body)
            post_opt_let(let1, sub)
        end

      {_, _, _} ->
        let1 = r_c_let(let0, vars: vs0, arg: arg0, body: body)
        post_opt_let(let1, sub)
    end
  end

  defp post_opt_let(let0, sub) do
    let1 = opt_bool_case_in_let(let0, sub)
    opt_build_stacktrace(let1)
  end

  defp maybe_suppress_warnings(arg, r_c_var(name: v), prevBody) do
    case should_suppress_warning(arg) do
      true ->
        arg

      false ->
        case :core_lib.is_var_used(v, prevBody) do
          true ->
            suppress_warning([arg])

          false ->
            arg
        end
    end
  end

  defp suppress_warning([h | t]) do
    case :cerl.is_literal(h) do
      true ->
        suppress_warning(t)

      false ->
        case :cerl.is_data(h) do
          true ->
            suppress_warning(:cerl.data_es(h) ++ t)

          false ->
            case t do
              [] ->
                h

              [_ | _] ->
                :cerl.c_seq(h, suppress_warning(t))
            end
        end
    end
  end

  defp suppress_warning([]) do
    void()
  end

  defp move_case_into_arg(
         r_c_case(
           arg: r_c_let(vars: outerVars0, arg: outerArg, body: innerArg0) = outer,
           clauses: innerClauses
         ) = inner,
         sub
       ) do
    scopeSub0 = sub_subst_scope(r_sub(sub, t: %{}))
    {outerVars, scopeSub} = var_list(outerVars0, scopeSub0)
    innerArg = body(innerArg0, scopeSub)

    r_c_let(outer,
      vars: outerVars,
      arg: outerArg,
      body: r_c_case(inner, arg: innerArg, clauses: innerClauses)
    )
  end

  defp move_case_into_arg(
         r_c_case(
           arg:
             r_c_case(
               arg: outerArg,
               clauses: [outerCa0, outerCb]
             ) = outer,
           clauses: innerClauses
         ) = inner0,
         sub
       ) do
    case is_failing_clause(outerCb) do
      true ->
        r_c_clause(pats: outerPats0, guard: outerGuard0, body: innerArg0) = outerCa0
        scopeSub0 = sub_subst_scope(r_sub(sub, t: %{}))

        {outerPats, scopeSub} =
          pattern_list(
            outerPats0,
            scopeSub0
          )

        outerGuard = guard(outerGuard0, scopeSub)
        innerArg = body(innerArg0, scopeSub)
        inner = r_c_case(inner0, arg: innerArg, clauses: innerClauses)
        outerCa = r_c_clause(outerCa0, pats: outerPats, guard: outerGuard, body: inner)
        r_c_case(outer, arg: outerArg, clauses: [outerCa, outerCb])

      false ->
        inner0
    end
  end

  defp move_case_into_arg(
         r_c_case(
           arg: r_c_seq(arg: outerArg, body: innerArg) = outer,
           clauses: innerClauses
         ) = inner,
         _Sub
       ) do
    r_c_seq(outer,
      arg: outerArg,
      body: r_c_case(inner, arg: innerArg, clauses: innerClauses)
    )
  end

  defp move_case_into_arg(expr, _) do
    expr
  end

  defp update_let_types(vs, args, sub) when is_list(args) do
    update_let_types_1(vs, args, sub)
  end

  defp update_let_types(_Vs, _Arg, sub) do
    sub
  end

  defp update_let_types_1([r_c_var(name: v) | vs], [a | as], sub0) do
    sub = update_types(v, a, sub0)
    update_let_types_1(vs, as, sub)
  end

  defp update_let_types_1([], [], sub) do
    sub
  end

  defp update_types(v, r_c_tuple() = p, r_sub(t: tdb) = sub) do
    r_sub(sub, t: %{tdb | v => p})
  end

  defp update_types(_, _, sub) do
    sub
  end

  defp kill_types(v, tdb) do
    :maps.from_list(kill_types2(v, :maps.to_list(tdb)))
  end

  defp kill_types2(v, [{v, _} | tdb]) do
    kill_types2(v, tdb)
  end

  defp kill_types2(v, [{_, r_c_tuple() = tuple} = entry | tdb]) do
    case :core_lib.is_var_used(v, tuple) do
      false ->
        [entry | kill_types2(v, tdb)]

      true ->
        kill_types2(v, tdb)
    end
  end

  defp kill_types2(_, []) do
    []
  end

  defp copy_type(v, r_c_var(name: src), tdb) do
    case tdb do
      %{^src => type} ->
        %{tdb | v => type}

      _ ->
        tdb
    end
  end

  defp copy_type(_, _, tdb) do
    tdb
  end

  defp void() do
    r_c_literal(val: :ok)
  end

  defp init_warnings() do
    :erlang.put({:sys_core_fold, :warnings}, [])
  end

  defp add_warning(core, term) do
    case should_suppress_warning(core) do
      true ->
        :ok

      false ->
        anno = :cerl.get_ann(core)
        line = get_line(anno)
        file = get_file(anno)
        key = {:sys_core_fold, :warnings}

        case :erlang.get(key) do
          [{^file, [{^line, :sys_core_fold, ^term}]} | _] ->
            :ok

          ws ->
            :erlang.put(
              key,
              [{file, [{line, :sys_core_fold, term}]} | ws]
            )
        end
    end
  end

  defp get_line([line | _]) when is_integer(line) do
    line
  end

  defp get_line([_ | t]) do
    get_line(t)
  end

  defp get_line([]) do
    :none
  end

  defp get_file([{:file, file} | _]) do
    file
  end

  defp get_file([_ | t]) do
    get_file(t)
  end

  defp get_file([]) do
    'no_file'
  end

  defp should_suppress_warning(core) do
    is_compiler_generated(core) or is_result_unwanted(core)
  end

  defp is_compiler_generated(core) do
    ann = :cerl.get_ann(core)
    member(:compiler_generated, ann)
  end

  defp is_result_unwanted(core) do
    ann = :cerl.get_ann(core)
    member(:result_not_wanted, ann)
  end

  defp get_warnings() do
    :ordsets.from_list(:erlang.erase({:sys_core_fold, :warnings}))
  end

  def format_error({:eval_failure, reason}) do
    flatten(:io_lib.format('this expression will fail with a \'~p\' exception', [reason]))
  end

  def format_error(:embedded_binary_size) do
    'binary construction will fail with a \'badarg\' exception (field size for binary/bitstring greater than actual size)'
  end

  def format_error({:embedded_unit, unit, size}) do
    m =
      :io_lib.format(
        'binary construction will fail with a \'badarg\' exception (size ~p cannot be evenly divided by unit ~p)',
        [size, unit]
      )

    flatten(m)
  end

  def format_error(:bad_unicode) do
    'binary construction will fail with a \'badarg\' exception (invalid Unicode code point in a utf8/utf16/utf32 segment)'
  end

  def format_error(:bad_float_size) do
    'binary construction will fail with a \'badarg\' exception (invalid size for a float segment)'
  end

  def format_error({:nomatch_shadow, line, {name, arity}}) do
    m =
      :io_lib.format(
        'this clause for ~ts/~B cannot match because a previous clause at line ~p always matches',
        [name, arity, line]
      )

    flatten(m)
  end

  def format_error({:nomatch_shadow, line}) do
    m =
      :io_lib.format(
        'this clause cannot match because a previous clause at line ~p always matches',
        [line]
      )

    flatten(m)
  end

  def format_error(:nomatch_shadow) do
    'this clause cannot match because a previous clause always matches'
  end

  def format_error(:nomatch_guard) do
    'the guard for this clause evaluates to \'false\''
  end

  def format_error({:nomatch_bit_syntax_truncated, signess, val, sz}) do
    s =
      case signess do
        :signed ->
          'a \'signed\''

        :unsigned ->
          'an \'unsigned\''
      end

    f =
      'this clause cannot match because the value ~P will not fit in ~s binary segment of size ~p'

    flatten(:io_lib.format(f, [val, 10, s, sz]))
  end

  def format_error({:nomatch_bit_syntax_unsigned, val}) do
    f =
      'this clause cannot match because the negative value ~P will never match the value of an \'unsigned\' binary segment'

    flatten(:io_lib.format(f, [val, 10]))
  end

  def format_error({:nomatch_bit_syntax_size, sz}) do
    f = 'this clause cannot match because \'~P\' is not a valid size for a binary segment'
    flatten(:io_lib.format(f, [sz, 10]))
  end

  def format_error({:nomatch_bit_syntax_type, val, type}) do
    f = 'this clause cannot match because \'~P\' is not of the expected type \'~p\''
    flatten(:io_lib.format(f, [val, 10, type]))
  end

  def format_error({:nomatch_bit_syntax_unicode, val}) do
    f = 'this clause cannot match because the value ~p is not a valid Unicode code point'
    flatten(:io_lib.format(f, [val]))
  end

  def format_error(:no_clause_match) do
    'no clause will ever match'
  end

  def format_error(:nomatch_clause_type) do
    'this clause cannot match because of different types/sizes'
  end

  def format_error({:no_effect, {:erlang, f, a}}) do
    {fmt, args} =
      case :erl_internal.comp_op(f, a) do
        true ->
          {'use of operator ~p has no effect', [f]}

        false ->
          case :erl_internal.bif(f, a) do
            false ->
              {'the call to erlang:~p/~p has no effect', [f, a]}

            true ->
              {'the call to ~p/~p has no effect', [f, a]}
          end
      end

    flatten(:io_lib.format(fmt, args))
  end

  def format_error(:result_ignored) do
    'the result of the expression is ignored (suppress the warning by assigning the expression to the _ variable)'
  end

  def format_error(:invalid_call) do
    'invalid function call'
  end

  def format_error(:useless_building) do
    'a term is constructed, but never used'
  end
end
