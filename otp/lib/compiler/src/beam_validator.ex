defmodule :m_beam_validator do
  use Bitwise
  import Kernel, except: [min: 2]
  import :lists, only: [dropwhile: 2, foldl: 3, member: 2, reverse: 2, zip: 2]
  require Record
  Record.defrecord(:r_t_atom, :t_atom, elements: :any)
  Record.defrecord(:r_t_bitstring, :t_bitstring, size_unit: 1)
  Record.defrecord(:r_t_bs_context, :t_bs_context, tail_unit: 1, slots: 0, valid: 0)
  Record.defrecord(:r_t_bs_matchable, :t_bs_matchable, tail_unit: 1)
  Record.defrecord(:r_t_float, :t_float, elements: :any)
  Record.defrecord(:r_t_fun, :t_fun, arity: :any, type: :any)
  Record.defrecord(:r_t_integer, :t_integer, elements: :any)

  Record.defrecord(:r_t_map, :t_map,
    super_key: :any,
    super_value: :any
  )

  Record.defrecord(:r_t_cons, :t_cons,
    type: :any,
    terminator: :any
  )

  Record.defrecord(:r_t_list, :t_list,
    type: :any,
    terminator: :any
  )

  Record.defrecord(:r_t_tuple, :t_tuple, size: 0, exact: false, elements: %{})

  Record.defrecord(:r_t_union, :t_union,
    atom: :none,
    list: :none,
    number: :none,
    tuple_set: :none,
    other: :none
  )

  def validate({mod, exp, attr, fs, lc}, level)
      when is_atom(mod) and is_list(exp) and
             is_list(attr) and is_integer(lc) do
    ft = build_function_table(fs, %{})

    case validate_0(fs, mod, level, ft) do
      [] ->
        :ok

      es0 ->
        es =
          for e <- es0 do
            {:beam_validator, e}
          end

        {:error, [{:erlang.atom_to_list(mod), es}]}
    end
  end

  def format_error({{_M, f, a}, {i, off, :limit}}) do
    :io_lib.format(
      'function ~p/~p+~p:~n  An implementation limit was reached.~n  Try reducing the complexity of this function.~n~n  Instruction: ~p~n',
      [f, a, off, i]
    )
  end

  def format_error({{_M, f, a}, {:undef_labels, lbls}}) do
    :io_lib.format(
      'function ~p/~p:~n  Internal consistency check failed - please report this bug.~n  The following label(s) were referenced but not defined:~n',
      [f, a]
    ) ++
      '  ' ++
      for l <- lbls do
        [:erlang.integer_to_list(l), ' ']
      end ++ '\n'
  end

  def format_error({{_M, f, a}, {i, off, desc}}) do
    :io_lib.format(
      'function ~p/~p+~p:~n  Internal consistency check failed - please report this bug.~n  Instruction: ~p~n  Error:       ~p:~n',
      [f, a, off, i, desc]
    )
  end

  def format_error(error) do
    :io_lib.format('~p~n', [error])
  end

  defp validate_0([], _Module, _Level, _Ft) do
    []
  end

  defp validate_0([{:function, name, arity, entry, code} | fs], module, level, ft) do
    mFA = {module, name, arity}

    try do
      validate_1(code, mFA, entry, level, ft)
    catch
      error ->
        [error | validate_0(fs, module, level, ft)]

      class, error ->
        :io.fwrite('Function: ~w/~w\n', [name, arity])
        :erlang.raise(class, error, __STACKTRACE__)
    else
      _ ->
        validate_0(fs, module, level, ft)
    end
  end

  Record.defrecord(:r_t_abstract, :t_abstract, kind: :undefined)
  Record.defrecord(:r_value_ref, :value_ref, id: :undefined)
  Record.defrecord(:r_value, :value, op: :undefined, args: :undefined, type: :undefined)

  Record.defrecord(:r_st, :st,
    vs: %{},
    xs: %{},
    ys: %{},
    f: init_fregs(),
    fragile: :cerl_sets.new(),
    numy: :none,
    h: 0,
    hl: 0,
    hf: 0,
    fls: :undefined,
    ct: [],
    setelem: false,
    puts_left: :none,
    recv_marker: :none,
    ms_positions: %{}
  )

  Record.defrecord(:r_vst, :vst,
    current: :none,
    level: :undefined,
    branched: %{},
    labels: :cerl_sets.new(),
    ft: %{},
    ref_ctr: 0
  )

  defp build_function_table(
         [{:function, _, arity, entry, code0} | fs],
         acc
       ) do
    code =
      dropwhile(
        fn
          {:label, l} when l === entry ->
            false

          _ ->
            true
        end,
        code0
      )

    case code do
      [{:label, ^entry} | is] ->
        info = %{arity: arity, parameter_info: find_parameter_info(is, %{})}
        build_function_table(fs, Map.put(acc, entry, info))

      _ ->
        build_function_table(fs, acc)
    end
  end

  defp build_function_table([], acc) do
    acc
  end

  defp find_parameter_info([{:%, {:var_info, reg, info}} | is], acc) do
    find_parameter_info(is, Map.put(acc, reg, info))
  end

  defp find_parameter_info([{:%, _} | is], acc) do
    find_parameter_info(is, acc)
  end

  defp find_parameter_info(_, acc) do
    acc
  end

  defp validate_1(is, mFA0, entry, level, ft) do
    {offset, mFA, header, body} = extract_header(is, mFA0, entry, 1, [])
    vst0 = init_vst(mFA, level, ft)
    vst1 = validate_instrs(body, mFA, offset, vst0)
    vst = validate_instrs(header, mFA, 1, vst1)
    validate_branches(mFA, vst)
  end

  defp extract_header(
         [
           {:func_info, {:atom, mod}, {:atom, name}, arity} = i
           | is
         ],
         mFA0,
         entry,
         offset,
         acc
       ) do
    {_, ^name, ^arity} = mFA0
    mFA = {mod, name, arity}

    case is do
      [{:label, ^entry} | _] ->
        header = reverse(acc, [i])
        {offset + 1, mFA, header, is}

      _ ->
        :erlang.error({mFA, :no_entry_label})
    end
  end

  defp extract_header([{:label, _} = i | is], mFA, entry, offset, acc) do
    extract_header(is, mFA, entry, offset + 1, [i | acc])
  end

  defp extract_header([{:line, _} = i | is], mFA, entry, offset, acc) do
    extract_header(is, mFA, entry, offset + 1, [i | acc])
  end

  defp extract_header(_Is, mFA, _Entry, _Offset, _Acc) do
    :erlang.error({mFA, :invalid_function_header})
  end

  defp init_vst({_, _, arity}, level, ft) do
    vst = r_vst(branched: %{}, current: r_st(), ft: ft, labels: :cerl_sets.new(), level: level)
    init_function_args(arity - 1, vst)
  end

  defp init_function_args(-1, vst) do
    vst
  end

  defp init_function_args(x, vst) do
    init_function_args(
      x - 1,
      create_term(:any, :argument, [], {:x, x}, vst)
    )
  end

  defp kill_heap_allocation(st) do
    r_st(st, h: 0, hl: 0, hf: 0)
  end

  defp validate_branches(mFA, vst) do
    r_vst(branched: targets0, labels: labels0) = vst
    targets = :maps.keys(targets0)
    labels = :cerl_sets.to_list(labels0)

    case targets -- labels do
      [_ | _] = undef ->
        error = {:undef_labels, undef}
        :erlang.error({mFA, error})

      [] ->
        vst
    end
  end

  defp validate_instrs([i | is], mFA, offset, vst0) do
    validate_instrs(
      is,
      mFA,
      offset + 1,
      try do
        vst = validate_mutation(i, vst0)
        vi(i, vst)
      catch
        error ->
          :erlang.error({mFA, {i, offset, error}})
      end
    )
  end

  defp validate_instrs([], _MFA, _Offset, vst) do
    vst
  end

  defp vi(
         {:label, lbl},
         r_vst(current: st0, ref_ctr: counter0, branched: branched0, labels: labels0) = vst
       ) do
    {st, counter} = merge_states(lbl, st0, branched0, counter0)
    branched = Map.put(branched0, lbl, st)
    labels = :cerl_sets.add_element(lbl, labels0)
    r_vst(vst, current: st, ref_ctr: counter, branched: branched, labels: labels)
  end

  defp vi(_I, r_vst(current: :none) = vst) do
    vst
  end

  defp vi({:%, {:var_info, reg, info}}, vst) do
    validate_var_info(info, reg, vst)
  end

  defp vi({:%, {:remove_fragility, reg}}, vst) do
    remove_fragility(reg, vst)
  end

  defp vi({:%, _}, vst) do
    vst
  end

  defp vi({:line, _}, vst) do
    vst
  end

  defp vi({:move, src, dst}, vst) do
    assign(src, dst, vst)
  end

  defp vi({:swap, regA, regB}, vst0) do
    assert_movable(regA, vst0)
    assert_movable(regB, vst0)
    sources = [regA, regB]
    vst1 = propagate_fragility(regA, sources, vst0)
    vst2 = propagate_fragility(regB, sources, vst1)
    vrefA = get_reg_vref(regA, vst2)
    vrefB = get_reg_vref(regB, vst2)
    vst = set_reg_vref(vrefB, regA, vst2)
    set_reg_vref(vrefA, regB, vst)
  end

  defp vi({:fmove, src, {:fr, _} = dst}, vst) do
    assert_type(r_t_float(), src, vst)
    set_freg(dst, vst)
  end

  defp vi({:fmove, {:fr, _} = src, dst}, vst0) do
    assert_freg_set(src, vst0)
    assert_fls(:checked, vst0)
    vst = eat_heap_float(vst0)
    create_term(r_t_float(), :fmove, [], dst, vst)
  end

  defp vi({:kill, reg}, vst) do
    create_tag(:initialized, :kill, [], reg, vst)
  end

  defp vi({:init, reg}, vst) do
    create_tag(:initialized, :init, [], reg, vst)
  end

  defp vi({:init_yregs, {:list, yregs}}, vst0) do
    case :ordsets.from_list(yregs) do
      [] ->
        :erlang.error(:empty_list)

      ^yregs ->
        :ok

      _ ->
        :erlang.error(:not_ordset)
    end

    foldl(
      fn y, vst ->
        create_tag(:initialized, :init, [], y, vst)
      end,
      vst0,
      yregs
    )
  end

  defp vi({:jump, {:f, lbl}}, vst) do
    assert_no_exception(lbl)
    branch(lbl, vst, &kill_state/1)
  end

  defp vi(
         {:select_val, src, {:f, fail}, {:list, choices}},
         vst
       ) do
    assert_term(src, vst)
    assert_choices(choices)
    validate_select_val(fail, choices, src, vst)
  end

  defp vi(
         {:select_tuple_arity, tuple, {:f, fail}, {:list, choices}},
         vst
       ) do
    assert_type(r_t_tuple(), tuple, vst)
    assert_arities(choices)
    validate_select_tuple_arity(fail, choices, tuple, vst)
  end

  defp vi(
         {:test, :has_map_fields, {:f, lbl}, src, {:list, list}},
         vst
       ) do
    verify_has_map_fields(lbl, src, list, vst)
  end

  defp vi({:test, :is_atom, {:f, lbl}, [src]}, vst) do
    type_test(lbl, r_t_atom(), src, vst)
  end

  defp vi({:test, :is_binary, {:f, lbl}, [src]}, vst) do
    type_test(lbl, r_t_bitstring(size_unit: 8), src, vst)
  end

  defp vi({:test, :is_bitstr, {:f, lbl}, [src]}, vst) do
    type_test(lbl, r_t_bitstring(), src, vst)
  end

  defp vi({:test, :is_boolean, {:f, lbl}, [src]}, vst) do
    type_test(lbl, :beam_types.make_boolean(), src, vst)
  end

  defp vi({:test, :is_float, {:f, lbl}, [src]}, vst) do
    type_test(lbl, r_t_float(), src, vst)
  end

  defp vi({:test, :is_tuple, {:f, lbl}, [src]}, vst) do
    type_test(lbl, r_t_tuple(), src, vst)
  end

  defp vi({:test, :is_integer, {:f, lbl}, [src]}, vst) do
    type_test(lbl, r_t_integer(), src, vst)
  end

  defp vi(
         {:test, :is_nonempty_list, {:f, lbl}, [src]},
         vst
       ) do
    type_test(lbl, r_t_cons(), src, vst)
  end

  defp vi({:test, :is_number, {:f, lbl}, [src]}, vst) do
    type_test(lbl, :number, src, vst)
  end

  defp vi({:test, :is_list, {:f, lbl}, [src]}, vst) do
    type_test(lbl, r_t_list(), src, vst)
  end

  defp vi({:test, :is_map, {:f, lbl}, [src]}, vst) do
    type_test(lbl, r_t_map(), src, vst)
  end

  defp vi({:test, :is_nil, {:f, lbl}, [src]}, vst) do
    assert_term(src, vst)

    branch(
      lbl,
      vst,
      fn failVst ->
        update_ne_types(src, nil, failVst)
      end,
      fn succVst ->
        update_eq_types(src, nil, succVst)
      end
    )
  end

  defp vi(
         {:test, :test_arity, {:f, lbl}, [tuple, sz]},
         vst
       )
       when is_integer(sz) do
    assert_type(r_t_tuple(), tuple, vst)
    type = r_t_tuple(exact: true, size: sz)
    type_test(lbl, type, tuple, vst)
  end

  defp vi(
         {:test, :is_tagged_tuple, {:f, lbl}, [src, sz, atom]},
         vst
       ) do
    assert_term(src, vst)
    es = %{1 => get_literal_type(atom)}
    type = r_t_tuple(exact: true, size: sz, elements: es)
    type_test(lbl, type, src, vst)
  end

  defp vi(
         {:test, :is_eq_exact, {:f, lbl}, [src, val] = ss},
         vst
       ) do
    assert_no_exception(lbl)
    validate_src(ss, vst)

    branch(
      lbl,
      vst,
      fn failVst ->
        update_ne_types(src, val, failVst)
      end,
      fn succVst ->
        update_eq_types(src, val, succVst)
      end
    )
  end

  defp vi(
         {:test, :is_ne_exact, {:f, lbl}, [src, val] = ss},
         vst
       ) do
    assert_no_exception(lbl)
    validate_src(ss, vst)

    branch(
      lbl,
      vst,
      fn failVst ->
        update_eq_types(src, val, failVst)
      end,
      fn succVst ->
        update_ne_types(src, val, succVst)
      end
    )
  end

  defp vi({:get_list, src, d1, d2}, vst0) do
    assert_not_literal(src)
    assert_type(r_t_cons(), src, vst0)
    srcType = get_term_type(src, vst0)
    {headType, _, _} = :beam_call_types.types(:erlang, :hd, [srcType])
    {tailType, _, _} = :beam_call_types.types(:erlang, :tl, [srcType])
    vst = extract_term(headType, :get_hd, [src], d1, vst0)
    extract_term(tailType, :get_tl, [src], d2, vst, vst0)
  end

  defp vi({:get_hd, src, dst}, vst) do
    assert_not_literal(src)
    assert_type(r_t_cons(), src, vst)
    srcType = get_term_type(src, vst)
    {headType, _, _} = :beam_call_types.types(:erlang, :hd, [srcType])
    extract_term(headType, :get_hd, [src], dst, vst)
  end

  defp vi({:get_tl, src, dst}, vst) do
    assert_not_literal(src)
    assert_type(r_t_cons(), src, vst)
    srcType = get_term_type(src, vst)
    {tailType, _, _} = :beam_call_types.types(:erlang, :tl, [srcType])
    extract_term(tailType, :get_tl, [src], dst, vst)
  end

  defp vi({:get_tuple_element, src, n, dst}, vst) do
    index = n + 1
    assert_not_literal(src)
    assert_type(r_t_tuple(size: index), src, vst)
    r_t_tuple(elements: es) = normalize(get_term_type(src, vst))
    type = :beam_types.get_tuple_element(index, es)
    extract_term(type, {:bif, :element}, [{:integer, index}, src], dst, vst)
  end

  defp vi({:test_heap, heap, live}, vst) do
    test_heap(heap, live, vst)
  end

  defp vi({:allocate, stk, live}, vst) do
    allocate(:uninitialized, stk, 0, live, vst)
  end

  defp vi({:allocate_heap, stk, heap, live}, vst) do
    allocate(:uninitialized, stk, heap, live, vst)
  end

  defp vi({:allocate_zero, stk, live}, vst) do
    allocate(:initialized, stk, 0, live, vst)
  end

  defp vi({:allocate_heap_zero, stk, heap, live}, vst) do
    allocate(:initialized, stk, heap, live, vst)
  end

  defp vi(
         {:deallocate, stkSize},
         r_vst(current: r_st(numy: stkSize)) = vst
       ) do
    verify_no_ct(vst)
    deallocate(vst)
  end

  defp vi({:deallocate, _}, r_vst(current: r_st(numy: numY))) do
    :erlang.error({:allocated, numY})
  end

  defp vi({:trim, n, remaining}, r_vst(current: st0) = vst) do
    r_st(numy: numY) = st0

    cond do
      n <= numY and n + remaining === numY ->
        r_vst(vst, current: trim_stack(n, 0, numY, st0))

      n > numY or n + remaining !== numY ->
        :erlang.error({:trim, n, remaining, :allocated, numY})
    end
  end

  defp vi({:put_list, a, b, dst}, vst0) do
    vst = eat_heap(2, vst0)
    head = get_term_type(a, vst)
    tail = get_term_type(b, vst)
    create_term(:beam_types.make_cons(head, tail), :put_list, [a, b], dst, vst)
  end

  defp vi({:put_tuple2, dst, {:list, elements}}, vst0) do
    _ =
      for el <- elements do
        assert_term(el, vst0)
      end

    size = length(elements)
    vst = eat_heap(size + 1, vst0)

    {es, _} =
      foldl(
        fn val, {es0, index} ->
          type = get_term_type(val, vst0)
          es = :beam_types.set_tuple_element(index, type, es0)
          {es, index + 1}
        end,
        {%{}, 1},
        elements
      )

    type = r_t_tuple(exact: true, size: size, elements: es)
    create_term(type, :put_tuple2, [], dst, vst)
  end

  defp vi({:put_tuple, sz, dst}, vst0)
       when is_integer(sz) do
    vst1 = eat_heap(1, vst0)
    vst = create_term(r_t_abstract(kind: :unfinished_tuple), :put_tuple, [], dst, vst1)
    r_vst(current: st0) = vst
    st = r_st(st0, puts_left: {sz, {dst, sz, %{}}})
    r_vst(vst, current: st)
  end

  defp vi({:put, src}, vst0) do
    assert_term(src, vst0)
    vst = eat_heap(1, vst0)
    r_vst(current: st0) = vst

    case st0 do
      r_st(puts_left: :none) ->
        :erlang.error(:not_building_a_tuple)

      r_st(puts_left: {1, {dst, sz, es0}}) ->
        elementType = get_term_type(src, vst0)
        es = :beam_types.set_tuple_element(sz, elementType, es0)
        st = r_st(st0, puts_left: :none)
        type = r_t_tuple(exact: true, size: sz, elements: es)
        create_term(type, :put_tuple, [], dst, r_vst(vst, current: st))

      r_st(puts_left: {putsLeft, {dst, sz, es0}})
      when is_integer(putsLeft) ->
        index = sz - putsLeft + 1
        elementType = get_term_type(src, vst0)
        es = :beam_types.set_tuple_element(index, elementType, es0)
        st = r_st(st0, puts_left: {putsLeft - 1, {dst, sz, es}})
        r_vst(vst, current: st)
    end
  end

  defp vi({:set_tuple_element, src, tuple, n}, vst) do
    i = n + 1
    assert_term(src, vst)
    assert_type(r_t_tuple(size: i), tuple, vst)

    r_t_tuple(elements: es0) =
      type =
      normalize(
        get_term_type(
          tuple,
          vst
        )
      )

    es = :beam_types.set_tuple_element(i, get_term_type(src, vst), es0)
    override_type(r_t_tuple(type, elements: es), tuple, vst)
  end

  defp vi({:apply, live}, vst) do
    validate_body_call(:apply, live + 2, vst)
  end

  defp vi({:apply_last, live, n}, vst) do
    validate_tail_call(n, :apply, live + 2, vst)
  end

  defp vi({:call, live, func}, vst) do
    validate_body_call(func, live, vst)
  end

  defp vi({:call_ext, live, func}, vst) do
    validate_body_call(func, live, vst)
  end

  defp vi({:call_only, live, func}, vst) do
    validate_tail_call(:none, func, live, vst)
  end

  defp vi({:call_ext_only, live, func}, vst) do
    validate_tail_call(:none, func, live, vst)
  end

  defp vi({:call_last, live, func, n}, vst) do
    validate_tail_call(n, func, live, vst)
  end

  defp vi({:call_ext_last, live, func, n}, vst) do
    validate_tail_call(n, func, live, vst)
  end

  defp vi({:call_fun, live}, vst) do
    fun = {:x, live}
    assert_term(fun, vst)

    branch(0, vst, fn succVst0 ->
      succVst = update_type(&meet/2, r_t_fun(arity: live), fun, succVst0)
      validate_body_call(:fun, live + 1, succVst)
    end)
  end

  defp vi(
         {:make_fun2, {:f, lbl}, _, _, numFree},
         r_vst(ft: ft) = vst0
       ) do
    %{arity: arity0} = :erlang.map_get(lbl, ft)
    arity = arity0 - numFree
    true = arity >= 0
    vst = prune_x_regs(numFree, vst0)
    verify_call_args(:make_fun, numFree, vst)
    verify_y_init(vst)
    type = r_t_fun(arity: arity)
    create_term(type, :make_fun, [], {:x, 0}, vst)
  end

  defp vi(
         {:make_fun3, {:f, lbl}, _, _, dst, {:list, env}},
         r_vst(ft: ft) = vst0
       ) do
    _ =
      for e <- env do
        assert_term(e, vst0)
      end

    numFree = length(env)
    %{arity: arity0} = :erlang.map_get(lbl, ft)
    arity = arity0 - numFree
    true = arity >= 0
    vst = eat_heap_fun(vst0)
    type = r_t_fun(arity: arity)
    create_term(type, :make_fun, [], dst, vst)
  end

  defp vi(:return, vst) do
    assert_durable_term({:x, 0}, vst)
    verify_return(vst)
  end

  defp vi({:bif, op, {:f, fail}, ss, dst}, vst0) do
    case is_float_arith_bif(op, ss) do
      true ->
        0 = fail
        validate_float_arith_bif(ss, dst, vst0)

      false ->
        validate_src(ss, vst0)
        validate_bif(:bif, op, fail, ss, dst, vst0, vst0)
    end
  end

  defp vi(
         {:gc_bif, op, {:f, fail}, live, ss, dst},
         vst0
       ) do
    validate_src(ss, vst0)
    verify_live(live, vst0)
    verify_y_init(vst0)
    r_vst(current: st0) = vst0
    st = kill_heap_allocation(st0)
    vst = prune_x_regs(live, r_vst(vst0, current: st))
    validate_bif(:gc_bif, op, fail, ss, dst, vst0, vst)
  end

  defp vi(:send, vst) do
    validate_body_call(:send, 2, vst)
  end

  defp vi({:loop_rec, {:f, fail}, dst}, vst) do
    assert_no_exception(fail)

    branch(fail, vst, fn succVst0 ->
      {ref, succVst} = new_value(:any, :loop_rec, [], succVst0)
      mark_fragile(dst, set_reg_vref(ref, dst, succVst))
    end)
  end

  defp vi({:loop_rec_end, lbl}, vst) do
    assert_no_exception(lbl)
    verify_y_init(vst)
    kill_state(vst)
  end

  defp vi({:recv_mark, {:f, fail}}, vst)
       when is_integer(fail) do
    assert_no_exception(fail)
    set_receive_marker(:initialized, vst)
  end

  defp vi({:recv_set, {:f, fail}}, vst)
       when is_integer(fail) do
    assert_no_exception(fail)
    set_receive_marker(:committed, vst)
  end

  defp vi(:remove_message, vst0) do
    vst = set_receive_marker(:none, vst0)
    remove_fragility(vst)
  end

  defp vi(:timeout, vst0) do
    vst = set_receive_marker(:none, vst0)
    prune_x_regs(0, vst)
  end

  defp vi({:wait, {:f, lbl}}, vst) do
    assert_no_exception(lbl)
    verify_y_init(vst)
    branch(lbl, vst, &kill_state/1)
  end

  defp vi({:wait_timeout, {:f, lbl}, src}, vst0) do
    assert_no_exception(lbl)
    assert_term(src, vst0)
    verify_y_init(vst0)
    vst = branch(lbl, prune_x_regs(0, vst0))
    branch(0, vst)
  end

  defp vi({:catch, dst, {:f, fail}}, vst)
       when fail !== :none do
    init_try_catch_branch(:catchtag, dst, fail, vst)
  end

  defp vi({:try, dst, {:f, fail}}, vst)
       when fail !== :none do
    init_try_catch_branch(:trytag, dst, fail, vst)
  end

  defp vi(
         {:catch_end, reg},
         r_vst(current: r_st(ct: [tag | _])) = vst0
       ) do
    case get_tag_type(reg, vst0) do
      {:catchtag, _Fail} = ^tag ->
        vst1 = kill_catch_tag(reg, vst0)
        vst = set_receive_marker(:none, vst1)
        create_term(:any, :catch_end, [], {:x, 0}, vst)

      type ->
        :erlang.error({:wrong_tag_type, type})
    end
  end

  defp vi(
         {:try_end, reg},
         r_vst(current: r_st(ct: [tag | _])) = vst
       ) do
    case get_tag_type(reg, vst) do
      {:trytag, _Fail} = ^tag ->
        kill_catch_tag(reg, vst)

      type ->
        :erlang.error({:wrong_tag_type, type})
    end
  end

  defp vi(
         {:try_case, reg},
         r_vst(current: r_st(ct: [tag | _])) = vst0
       ) do
    case get_tag_type(reg, vst0) do
      {:trytag, _Fail} = ^tag ->
        vst1 = kill_catch_tag(reg, vst0)
        vst2 = prune_x_regs(0, vst1)
        vst3 = set_receive_marker(:none, vst2)
        classType = r_t_atom(elements: [:error, :exit, :throw])
        vst4 = create_term(classType, :try_case, [], {:x, 0}, vst3)
        vst = create_term(:any, :try_case, [], {:x, 1}, vst4)
        create_term(:any, :try_case, [], {:x, 2}, vst)

      type ->
        :erlang.error({:wrong_tag_type, type})
    end
  end

  defp vi(:build_stacktrace, vst0) do
    assert_float_checked(vst0)
    verify_y_init(vst0)
    verify_live(1, vst0)
    vst = prune_x_regs(1, vst0)
    reg = {:x, 0}
    assert_durable_term(reg, vst)
    create_term(r_t_list(), :build_stacktrace, [], reg, vst)
  end

  defp vi(
         {:get_map_elements, {:f, fail}, src, {:list, list}},
         vst
       ) do
    verify_get_map(fail, src, list, vst)
  end

  defp vi(
         {:put_map_assoc = op, {:f, fail}, src, dst, live, {:list, list}},
         vst
       ) do
    verify_put_map(op, fail, src, dst, live, list, vst)
  end

  defp vi(
         {:put_map_exact = op, {:f, fail}, src, dst, live, {:list, list}},
         vst
       ) do
    verify_put_map(op, fail, src, dst, live, list, vst)
  end

  defp vi({:bs_get_tail, ctx, dst, live}, vst0) do
    assert_type(r_t_bs_context(), ctx, vst0)
    verify_live(live, vst0)
    verify_y_init(vst0)
    r_t_bs_context(tail_unit: unit) = get_raw_type(ctx, vst0)
    vst = prune_x_regs(live, vst0)
    extract_term(r_t_bitstring(size_unit: unit), :bs_get_tail, [ctx], dst, vst, vst0)
  end

  defp vi(
         {:bs_start_match4, fail, live, src, dst},
         vst
       ) do
    validate_bs_start_match(fail, live, 0, src, dst, vst)
  end

  defp vi(
         {:test, :bs_start_match3, {:f, _} = fail, live, [src], dst},
         vst
       ) do
    validate_bs_start_match(fail, live, 0, src, dst, vst)
  end

  defp vi(
         {:test, :bs_start_match2, {:f, _} = fail, live, [src, slots], dst},
         vst
       ) do
    validate_bs_start_match(fail, live, slots, src, dst, vst)
  end

  defp vi(
         {:test, :bs_match_string, {:f, fail}, [ctx, stride, {:string, string}]},
         vst
       ) do
    true = is_bitstring(string)
    validate_bs_skip(fail, ctx, stride, vst)
  end

  defp vi(
         {:test, :bs_skip_bits2, {:f, fail}, [ctx, size, unit, _Flags]},
         vst
       ) do
    assert_term(size, vst)

    stride =
      case get_raw_type(size, vst) do
        r_t_integer(elements: {same, same}) ->
          same * unit

        _ ->
          unit
      end

    validate_bs_skip(fail, ctx, stride, vst)
  end

  defp vi(
         {:test, :bs_test_tail2, {:f, fail}, [ctx, _Size]},
         vst
       ) do
    assert_no_exception(fail)
    assert_type(r_t_bs_context(), ctx, vst)
    branch(fail, vst)
  end

  defp vi(
         {:test, :bs_test_unit, {:f, fail}, [ctx, unit]},
         vst
       ) do
    assert_type(r_t_bs_context(), ctx, vst)
    type = r_t_bs_context(tail_unit: unit)

    branch(
      fail,
      vst,
      fn failVst ->
        update_type(&subtract/2, type, ctx, failVst)
      end,
      fn succVst0 ->
        succVst = update_bs_unit(ctx, unit, succVst0)
        update_type(&meet/2, type, ctx, succVst)
      end
    )
  end

  defp vi(
         {:test, :bs_skip_utf8, {:f, fail}, [ctx, live, _]},
         vst
       ) do
    validate_bs_skip(fail, ctx, 8, live, vst)
  end

  defp vi(
         {:test, :bs_skip_utf16, {:f, fail}, [ctx, live, _]},
         vst
       ) do
    validate_bs_skip(fail, ctx, 16, live, vst)
  end

  defp vi(
         {:test, :bs_skip_utf32, {:f, fail}, [ctx, live, _]},
         vst
       ) do
    validate_bs_skip(fail, ctx, 32, live, vst)
  end

  defp vi(
         {:test, :bs_get_binary2 = op, {:f, fail}, live, [ctx, {:atom, :all}, unit, _], dst},
         vst
       ) do
    type = r_t_bitstring(size_unit: unit)
    validate_bs_get_all(op, fail, ctx, live, unit, type, dst, vst)
  end

  defp vi(
         {:test, :bs_get_binary2 = op, {:f, fail}, live, [ctx, {:integer, sz}, unit, _], dst},
         vst
       ) do
    stride = unit * max(1, sz)
    type = r_t_bitstring(size_unit: stride)
    validate_bs_get(op, fail, ctx, live, stride, type, dst, vst)
  end

  defp vi(
         {:test, :bs_get_binary2 = op, {:f, fail}, live, [ctx, _, unit, _], dst},
         vst
       ) do
    type = r_t_bitstring(size_unit: unit)
    validate_bs_get(op, fail, ctx, live, unit, type, dst, vst)
  end

  defp vi(
         {:test, :bs_get_integer2 = op, {:f, fail}, live,
          [ctx, {:integer, sz}, unit, {:field_flags, flags}], dst},
         vst
       ) do
    numBits = unit * sz
    stride = max(1, numBits)

    type =
      case member(:unsigned, flags) do
        true when 0 <= numBits and numBits <= 64 ->
          :beam_types.make_integer(0, 1 <<< (numBits - 1))

        _ ->
          r_t_integer()
      end

    validate_bs_get(op, fail, ctx, live, stride, type, dst, vst)
  end

  defp vi(
         {:test, :bs_get_integer2 = op, {:f, fail}, live, [ctx, _Sz, unit, _Flags], dst},
         vst
       ) do
    validate_bs_get(op, fail, ctx, live, unit, r_t_integer(), dst, vst)
  end

  defp vi(
         {:test, :bs_get_float2 = op, {:f, fail}, live, [ctx, {:integer, sz}, unit, _], dst},
         vst
       ) do
    stride = unit * max(1, sz)
    validate_bs_get(op, fail, ctx, live, stride, r_t_float(), dst, vst)
  end

  defp vi(
         {:test, :bs_get_float2 = op, {:f, fail}, live, [ctx, _, _, _], dst},
         vst
       ) do
    validate_bs_get(op, fail, ctx, live, 32, r_t_float(), dst, vst)
  end

  defp vi(
         {:test, :bs_get_utf8 = op, {:f, fail}, live, [ctx, _], dst},
         vst
       ) do
    type = :beam_types.make_integer(0, 1_114_111)
    validate_bs_get(op, fail, ctx, live, 8, type, dst, vst)
  end

  defp vi(
         {:test, :bs_get_utf16 = op, {:f, fail}, live, [ctx, _], dst},
         vst
       ) do
    type = :beam_types.make_integer(0, 1_114_111)
    validate_bs_get(op, fail, ctx, live, 16, type, dst, vst)
  end

  defp vi(
         {:test, :bs_get_utf32 = op, {:f, fail}, live, [ctx, _], dst},
         vst
       ) do
    type = :beam_types.make_integer(0, 1_114_111)
    validate_bs_get(op, fail, ctx, live, 32, type, dst, vst)
  end

  defp vi({:test, _Op, {:f, lbl}, src}, vst) do
    validate_src(src, vst)
    branch(lbl, vst)
  end

  defp vi({:bs_save2, ctx, savePoint}, vst) do
    bsm_save(ctx, savePoint, vst)
  end

  defp vi({:bs_restore2, ctx, savePoint}, vst) do
    bsm_restore(ctx, savePoint, vst)
  end

  defp vi({:bs_get_position, ctx, dst, live}, vst0) do
    assert_type(r_t_bs_context(), ctx, vst0)
    verify_live(live, vst0)
    verify_y_init(vst0)
    r_t_bs_context(tail_unit: unit) = get_raw_type(ctx, vst0)
    vst1 = prune_x_regs(live, vst0)

    vst =
      create_term(
        r_t_abstract(kind: {:ms_position, unit}),
        :bs_get_position,
        [ctx],
        dst,
        vst1,
        vst0
      )

    mark_current_ms_position(ctx, dst, vst)
  end

  defp vi({:bs_set_position, ctx, pos}, vst0) do
    assert_type(r_t_bs_context(), ctx, vst0)
    assert_type(r_t_abstract(kind: {:ms_position, 1}), pos, vst0)
    r_t_abstract(kind: {:ms_position, unit}) = get_raw_type(pos, vst0)
    vst = override_type(r_t_bs_context(tail_unit: unit), ctx, vst0)
    mark_current_ms_position(ctx, pos, vst)
  end

  defp vi({:fconv, src, {:fr, _} = dst}, vst) do
    assert_term(src, vst)

    branch(
      0,
      vst,
      fn failVst ->
        set_fls(:checked, failVst)
      end,
      fn succVst0 ->
        succVst = update_type(&meet/2, :number, src, succVst0)
        set_freg(dst, succVst)
      end
    )
  end

  defp vi(:fclearerror, vst) do
    case get_fls(vst) do
      :undefined ->
        :ok

      :checked ->
        :ok

      fls ->
        :erlang.error({:bad_floating_point_state, fls})
    end

    set_fls(:cleared, vst)
  end

  defp vi({:fcheckerror, _}, vst0) do
    assert_fls(:cleared, vst0)
    vst = set_fls(:checked, vst0)
    branch(0, vst)
  end

  defp vi(
         {:func_info, {:atom, _Mod}, {:atom, _Name}, arity},
         vst
       ) do
    r_vst(current: r_st(numy: numY)) = vst

    cond do
      numY === :none ->
        verify_live(arity, vst)
        verify_call_args(:func_info, arity, vst)
        branch(0, vst, &kill_state/1)

      numY !== :none ->
        :erlang.error({:allocated, numY})
    end
  end

  defp vi({:badmatch, src}, vst) do
    assert_durable_term(src, vst)
    branch(0, vst, &kill_state/1)
  end

  defp vi({:case_end, src}, vst) do
    assert_durable_term(src, vst)
    branch(0, vst, &kill_state/1)
  end

  defp vi(:if_end, vst) do
    branch(0, vst, &kill_state/1)
  end

  defp vi({:try_case_end, src}, vst) do
    assert_durable_term(src, vst)
    branch(0, vst, &kill_state/1)
  end

  defp vi(:raw_raise = i, vst0) do
    validate_body_call(i, 3, vst0)
  end

  defp vi(:bs_init_writable = i, vst) do
    validate_body_call(i, 1, vst)
  end

  defp vi(
         {:bs_init2, {:f, fail}, sz, heap, live, _, dst},
         vst0
       ) do
    verify_live(live, vst0)
    verify_y_init(vst0)

    cond do
      is_integer(sz) ->
        :ok

      true ->
        assert_term(sz, vst0)
    end

    vst = heap_alloc(heap, vst0)

    branch(fail, vst, fn succVst0 ->
      succVst = prune_x_regs(live, succVst0)
      create_term(r_t_bitstring(size_unit: 8), :bs_init2, [], dst, succVst, succVst0)
    end)
  end

  defp vi(
         {:bs_init_bits, {:f, fail}, sz, heap, live, _, dst},
         vst0
       ) do
    verify_live(live, vst0)
    verify_y_init(vst0)

    cond do
      is_integer(sz) ->
        :ok

      true ->
        assert_term(sz, vst0)
    end

    vst = heap_alloc(heap, vst0)

    branch(fail, vst, fn succVst0 ->
      succVst = prune_x_regs(live, succVst0)
      create_term(r_t_bitstring(), :bs_init_bits, [], dst, succVst)
    end)
  end

  defp vi({:bs_add, {:f, fail}, [a, b, _], dst}, vst) do
    assert_term(a, vst)
    assert_term(b, vst)

    branch(fail, vst, fn succVst ->
      create_term(r_t_integer(), :bs_add, [a, b], dst, succVst)
    end)
  end

  defp vi({:bs_utf8_size, {:f, fail}, a, dst}, vst) do
    assert_term(a, vst)

    branch(fail, vst, fn succVst ->
      create_term(r_t_integer(), :bs_utf8_size, [a], dst, succVst)
    end)
  end

  defp vi({:bs_utf16_size, {:f, fail}, a, dst}, vst) do
    assert_term(a, vst)

    branch(fail, vst, fn succVst ->
      create_term(r_t_integer(), :bs_utf16_size, [a], dst, succVst)
    end)
  end

  defp vi(
         {:bs_append, {:f, fail}, bits, heap, live, unit, bin, _Flags, dst},
         vst0
       ) do
    verify_live(live, vst0)
    verify_y_init(vst0)
    assert_term(bits, vst0)
    assert_term(bin, vst0)
    vst = heap_alloc(heap, vst0)

    branch(fail, vst, fn succVst0 ->
      succVst = prune_x_regs(live, succVst0)
      create_term(r_t_bitstring(size_unit: unit), :bs_append, [bin], dst, succVst, succVst0)
    end)
  end

  defp vi(
         {:bs_private_append, {:f, fail}, bits, unit, bin, _Flags, dst},
         vst
       ) do
    assert_term(bits, vst)
    assert_term(bin, vst)

    branch(fail, vst, fn succVst ->
      create_term(r_t_bitstring(size_unit: unit), :bs_private_append, [bin], dst, succVst)
    end)
  end

  defp vi({:bs_put_string, sz, _}, vst)
       when is_integer(sz) do
    vst
  end

  defp vi(
         {:bs_put_binary, {:f, fail}, sz, _, _, src},
         vst
       ) do
    assert_term(sz, vst)
    assert_term(src, vst)

    branch(fail, vst, fn succVst ->
      update_type(&meet/2, r_t_bitstring(), src, succVst)
    end)
  end

  defp vi(
         {:bs_put_float, {:f, fail}, sz, _, _, src},
         vst
       ) do
    assert_term(sz, vst)
    assert_term(src, vst)

    branch(fail, vst, fn succVst ->
      update_type(&meet/2, r_t_float(), src, succVst)
    end)
  end

  defp vi(
         {:bs_put_integer, {:f, fail}, sz, _, _, src},
         vst
       ) do
    assert_term(sz, vst)
    assert_term(src, vst)

    branch(fail, vst, fn succVst ->
      update_type(&meet/2, r_t_integer(), src, succVst)
    end)
  end

  defp vi({:bs_put_utf8, {:f, fail}, _, src}, vst) do
    assert_term(src, vst)

    branch(fail, vst, fn succVst ->
      update_type(&meet/2, r_t_integer(), src, succVst)
    end)
  end

  defp vi({:bs_put_utf16, {:f, fail}, _, src}, vst) do
    assert_term(src, vst)

    branch(fail, vst, fn succVst ->
      update_type(&meet/2, r_t_integer(), src, succVst)
    end)
  end

  defp vi({:bs_put_utf32, {:f, fail}, _, src}, vst) do
    assert_term(src, vst)

    branch(fail, vst, fn succVst ->
      update_type(&meet/2, r_t_integer(), src, succVst)
    end)
  end

  defp vi(_, _) do
    :erlang.error(:unknown_instruction)
  end

  defp validate_var_info([{:fun_type, type} | info], reg, vst0) do
    vst = update_type(&meet/2, r_t_fun(type: type), reg, vst0)
    validate_var_info(info, reg, vst)
  end

  defp validate_var_info([{:type, :none} | _Info], _Reg, vst) do
    kill_state(vst)
  end

  defp validate_var_info([{:type, type} | info], reg, vst0) do
    vst = update_type(&meet/2, type, reg, vst0)
    validate_var_info(info, reg, vst)
  end

  defp validate_var_info([_ | info], reg, vst) do
    validate_var_info(info, reg, vst)
  end

  defp validate_var_info([], _Reg, vst) do
    vst
  end

  defp validate_tail_call(deallocate, func, live, r_vst(current: r_st(numy: numY)) = vst0) do
    assert_float_checked(vst0)
    verify_y_init(vst0)
    verify_live(live, vst0)
    verify_call_args(func, live, vst0)

    case will_call_succeed(func, vst0) do
      :yes when deallocate === numY ->
        vst = deallocate(vst0)
        verify_return(vst)

      :maybe when deallocate === numY ->
        vst = deallocate(vst0)
        branch(0, vst, &verify_return/1)

      :no ->
        branch(0, vst0, &kill_state/1)

      _ when deallocate !== numY ->
        :erlang.error({:allocated, numY})
    end
  end

  defp validate_body_call(func, live, r_vst(current: r_st(numy: numY)) = vst)
       when is_integer(numY) do
    assert_float_checked(vst)
    verify_y_init(vst)
    verify_live(live, vst)
    verify_call_args(func, live, vst)

    succFun = fn r_vst(current: st0) = succVst0 ->
      {retType, _, _} = call_types(func, live, succVst0)
      true = retType !== :none
      st = r_st(st0, f: init_fregs())
      succVst = prune_x_regs(0, r_vst(succVst0, current: st))
      create_term(retType, :call, [], {:x, 0}, succVst)
    end

    case will_call_succeed(func, vst) do
      :yes ->
        succFun.(vst)

      :maybe ->
        branch(0, vst, succFun)

      :no ->
        branch(0, vst, &kill_state/1)
    end
  end

  defp validate_body_call(_, _, r_vst(current: r_st(numy: numY))) do
    :erlang.error({:allocated, numY})
  end

  defp assert_float_checked(vst) do
    case get_fls(vst) do
      :undefined ->
        :ok

      :checked ->
        :ok

      fls ->
        :erlang.error({:unsafe_instruction, {:float_error_state, fls}})
    end
  end

  defp init_try_catch_branch(kind, dst, fail, vst0) do
    assert_no_exception(fail)
    tag = {kind, [fail]}
    vst = create_tag(tag, :try_catch, [], dst, vst0)
    r_vst(current: st0) = vst
    r_st(ct: tags) = st0
    st = r_st(st0, ct: [tag | tags])
    r_vst(vst, current: st)
  end

  defp verify_has_map_fields(lbl, src, list, vst) do
    assert_type(r_t_map(), src, vst)
    assert_unique_map_keys(list)
    verify_map_fields(list, src, lbl, vst)
  end

  defp verify_map_fields([key | keys], map, lbl, vst) do
    assert_term(key, vst)

    case bif_types(:map_get, [key, map], vst) do
      {:none, _, _} ->
        kill_state(vst)

      {_, _, _} ->
        verify_map_fields(keys, map, lbl, vst)
    end
  end

  defp verify_map_fields([], _Map, lbl, vst) do
    branch(lbl, vst)
  end

  defp verify_get_map(fail, src, list, vst0) do
    assert_no_exception(fail)
    assert_not_literal(src)
    assert_type(r_t_map(), src, vst0)

    branch(
      fail,
      vst0,
      fn failVst ->
        clobber_map_vals(list, src, failVst)
      end,
      fn succVst ->
        keys = extract_map_keys(list)
        assert_unique_map_keys(keys)
        extract_map_vals(list, src, succVst, succVst)
      end
    )
  end

  defp clobber_map_vals([key, dst | t], map, vst0) do
    case is_reg_initialized(dst, vst0) do
      true ->
        vst = extract_term(:any, {:bif, :map_get}, [key, map], dst, vst0)
        clobber_map_vals(t, map, vst)

      false ->
        clobber_map_vals(t, map, vst0)
    end
  end

  defp clobber_map_vals([], _Map, vst) do
    vst
  end

  defp is_reg_initialized({:x, _} = reg, r_vst(current: r_st(xs: xs))) do
    :erlang.is_map_key(reg, xs)
  end

  defp is_reg_initialized({:y, _} = reg, r_vst(current: r_st(ys: ys))) do
    case ys do
      %{^reg => val} ->
        val !== :uninitialized

      %{} ->
        false
    end
  end

  defp is_reg_initialized(v, r_vst()) do
    :erlang.error({:not_a_register, v})
  end

  defp extract_map_keys([key, _Val | t]) do
    [key | extract_map_keys(t)]
  end

  defp extract_map_keys([]) do
    []
  end

  defp extract_map_vals([key, dst | vs], map, vst0, vsti0) do
    assert_term(key, vst0)

    case bif_types(:map_get, [key, map], vst0) do
      {:none, _, _} ->
        kill_state(vsti0)

      {dstType, _, _} ->
        vsti = extract_term(dstType, {:bif, :map_get}, [key, map], dst, vsti0)
        extract_map_vals(vs, map, vst0, vsti)
    end
  end

  defp extract_map_vals([], _Map, _Vst0, vst) do
    vst
  end

  defp verify_put_map(op, fail, src, dst, live, list, vst0) do
    assert_type(r_t_map(), src, vst0)
    verify_live(live, vst0)
    verify_y_init(vst0)

    _ =
      for term <- list do
        assert_term(term, vst0)
      end

    vst = heap_alloc(0, vst0)

    succFun = fn succVst0 ->
      succVst = prune_x_regs(live, succVst0)
      keys = extract_map_keys(list)
      assert_unique_map_keys(keys)
      type = put_map_type(src, list, vst)
      create_term(type, op, [src], dst, succVst, succVst0)
    end

    case op do
      :put_map_exact ->
        branch(fail, vst, succFun)

      :put_map_assoc ->
        0 = fail
        succFun.(vst)
    end
  end

  defp put_map_type(map0, list, vst) do
    map = normalize(get_term_type(map0, vst))
    pmt_1(list, vst, map)
  end

  defp pmt_1([key0, value0 | list], vst, acc0) do
    key = normalize(get_term_type(key0, vst))
    value = normalize(get_term_type(value0, vst))
    {acc, _, _} = :beam_call_types.types(:maps, :put, [key, value, acc0])
    pmt_1(list, vst, acc)
  end

  defp pmt_1([], _Vst, acc) do
    acc
  end

  defp verify_return(r_vst(current: r_st(numy: numY)))
       when numY !== :none do
    :erlang.error({:stack_frame, numY})
  end

  defp verify_return(r_vst(current: r_st(recv_marker: mark)))
       when mark !== :none do
    :erlang.error({:return_with_receive_marker, mark})
  end

  defp verify_return(vst) do
    assert_float_checked(vst)
    verify_no_ct(vst)
    kill_state(vst)
  end

  defp validate_bif(kind, op, fail, ss, dst, origVst, vst) do
    assert_float_checked(vst)

    case {will_bif_succeed(op, ss, vst), fail} do
      {:yes, _} ->
        validate_bif_1(kind, op, :cannot_fail, ss, dst, origVst, vst)

      {:no, _} ->
        branch(fail, vst, &kill_state/1)

      {:maybe, _} ->
        validate_bif_1(kind, op, fail, ss, dst, origVst, vst)
    end
  end

  defp validate_bif_1(kind, op, :cannot_fail, ss, dst, origVst, vst0) do
    {type, argTypes, _CanSubtract} = bif_types(op, ss, vst0)
    zippedArgs = zip(ss, argTypes)

    vst =
      foldl(
        fn {a, t}, v ->
          update_type(&meet/2, t, a, v)
        end,
        vst0,
        zippedArgs
      )

    true = type !== :none
    extract_term(type, {kind, op}, ss, dst, vst, origVst)
  end

  defp validate_bif_1(kind, op, fail, ss, dst, origVst, vst) do
    {type, argTypes, canSubtract} = bif_types(op, ss, vst)
    zippedArgs = zip(ss, argTypes)

    failFun =
      case canSubtract do
        true ->
          fn failVst0 ->
            foldl(
              fn {a, t}, v ->
                update_type(&subtract/2, t, a, v)
              end,
              failVst0,
              zippedArgs
            )
          end

        false ->
          fn s ->
            s
          end
      end

    succFun = fn succVst0 ->
      succVst =
        foldl(
          fn {a, t}, v ->
            update_type(&meet/2, t, a, v)
          end,
          succVst0,
          zippedArgs
        )

      extract_term(type, {kind, op}, ss, dst, succVst, origVst)
    end

    branch(fail, vst, failFun, succFun)
  end

  defp validate_bs_start_match({:atom, :resume}, live, 0, src, dst, vst0) do
    assert_type(r_t_bs_context(), src, vst0)
    verify_live(live, vst0)
    verify_y_init(vst0)
    vst = assign(src, dst, vst0)
    prune_x_regs(live, vst)
  end

  defp validate_bs_start_match({:atom, :no_fail}, live, slots, src, dst, vst0) do
    verify_live(live, vst0)
    verify_y_init(vst0)
    vst1 = update_type(&meet/2, r_t_bs_matchable(), src, vst0)
    srcType = get_movable_term_type(src, vst1)
    tailUnit = :beam_types.get_bs_matchable_unit(srcType)
    ctxType = r_t_bs_context(slots: slots, tail_unit: tailUnit)
    vst = prune_x_regs(live, vst1)
    extract_term(ctxType, :bs_start_match, [src], dst, vst, vst0)
  end

  defp validate_bs_start_match({:f, fail}, live, slots, src, dst, vst) do
    assert_no_exception(fail)

    branch(
      fail,
      vst,
      fn failVst ->
        update_type(&subtract/2, r_t_bs_matchable(), src, failVst)
      end,
      fn succVst ->
        validate_bs_start_match({:atom, :no_fail}, live, slots, src, dst, succVst)
      end
    )
  end

  defp validate_bs_get(op, fail, ctx, live, stride, type, dst, vst) do
    assert_no_exception(fail)
    assert_type(r_t_bs_context(), ctx, vst)
    verify_live(live, vst)
    verify_y_init(vst)

    branch(fail, vst, fn succVst0 ->
      succVst1 = advance_bs_context(ctx, stride, succVst0)
      succVst = prune_x_regs(live, succVst1)
      extract_term(type, op, [ctx], dst, succVst, succVst0)
    end)
  end

  defp validate_bs_get_all(op, fail, ctx, live, stride, type, dst, vst) do
    assert_no_exception(fail)
    assert_type(r_t_bs_context(), ctx, vst)
    verify_live(live, vst)
    verify_y_init(vst)

    branch(fail, vst, fn succVst0 ->
      succVst1 = update_bs_unit(ctx, stride, succVst0)
      succVst2 = advance_bs_context(ctx, stride, succVst1)
      succVst = prune_x_regs(live, succVst2)
      extract_term(type, op, [ctx], dst, succVst, succVst0)
    end)
  end

  defp validate_bs_skip(fail, ctx, stride, vst) do
    validate_bs_skip(fail, ctx, stride, :no_live, vst)
  end

  defp validate_bs_skip(fail, ctx, stride, live, vst) do
    assert_no_exception(fail)
    assert_type(r_t_bs_context(), ctx, vst)
    validate_bs_skip_1(fail, ctx, stride, live, vst)
  end

  defp validate_bs_skip_1(fail, ctx, stride, :no_live, vst) do
    branch(fail, vst, fn succVst ->
      advance_bs_context(ctx, stride, succVst)
    end)
  end

  defp validate_bs_skip_1(fail, ctx, stride, live, vst) do
    verify_y_init(vst)
    verify_live(live, vst)

    branch(fail, vst, fn succVst0 ->
      succVst = advance_bs_context(ctx, stride, succVst0)
      prune_x_regs(live, succVst)
    end)
  end

  defp advance_bs_context(ctx, stride, vst0) do
    ctxType0 = get_raw_type(ctx, vst0)

    ctxType =
      case stride do
        0 ->
          ctxType0

        n ->
          r_t_bs_context(ctxType0, tail_unit: n)
      end

    vst = update_type(&join/2, ctxType, ctx, vst0)
    invalidate_current_ms_position(ctx, vst)
  end

  defp update_bs_unit(ctx, unit, r_vst(current: st) = vst) do
    ctxRef = get_reg_vref(ctx, vst)

    case r_st(st, :ms_positions) do
      %{^ctxRef => posRef} ->
        posType = r_t_abstract(kind: {:ms_position, unit})
        update_type(&meet/2, posType, posRef, vst)

      %{} ->
        vst
    end
  end

  defp mark_current_ms_position(ctx, pos, r_vst(current: st0) = vst) do
    ctxRef = get_reg_vref(ctx, vst)
    posRef = get_reg_vref(pos, vst)
    r_st(ms_positions: msPos0) = st0
    msPos = Map.put(msPos0, ctxRef, posRef)
    st = r_st(st0, ms_positions: msPos)
    r_vst(vst, current: st)
  end

  defp invalidate_current_ms_position(ctx, r_vst(current: st0) = vst) do
    ctxRef = get_reg_vref(ctx, vst)
    r_st(ms_positions: msPos0) = st0

    case msPos0 do
      %{^ctxRef => _} ->
        msPos = :maps.remove(ctxRef, msPos0)
        st = r_st(st0, ms_positions: msPos)
        r_vst(vst, current: st)

      %{} ->
        vst
    end
  end

  defp type_test(fail, type, reg, vst) do
    assert_term(reg, vst)
    assert_no_exception(fail)

    branch(
      fail,
      vst,
      fn failVst ->
        update_type(&subtract/2, type, reg, failVst)
      end,
      fn succVst ->
        update_type(&meet/2, type, reg, succVst)
      end
    )
  end

  defp validate_mutation(i, vst) do
    vm_1(i, vst)
  end

  defp vm_1({:move, _, _}, vst) do
    vst
  end

  defp vm_1({:swap, _, _}, vst) do
    vst
  end

  defp vm_1(
         {:call_ext, 3, {:extfunc, :erlang, :setelement, 3}},
         r_vst(current: r_st() = st) = vst
       ) do
    r_vst(vst, current: r_st(st, setelem: true))
  end

  defp vm_1(
         {:set_tuple_element, _, _, _},
         r_vst(current: r_st(setelem: false))
       ) do
    :erlang.error(:illegal_context_for_set_tuple_element)
  end

  defp vm_1(
         {:set_tuple_element, _, _, _},
         r_vst(current: r_st(setelem: true)) = vst
       ) do
    vst
  end

  defp vm_1({:get_tuple_element, _, _, _}, vst) do
    vst
  end

  defp vm_1({:line, _}, vst) do
    vst
  end

  defp vm_1(_, r_vst(current: r_st(setelem: true) = st) = vst) do
    r_vst(vst, current: r_st(st, setelem: false))
  end

  defp vm_1(_, vst) do
    vst
  end

  defp kill_state(vst) do
    r_vst(vst, current: :none)
  end

  defp verify_call_args(_, 0, r_vst()) do
    :ok
  end

  defp verify_call_args({:f, lbl}, live, r_vst(ft: ft) = vst) do
    case ft do
      %{^lbl => funcInfo} ->
        %{arity: ^live, parameter_info: paramInfo} = funcInfo
        verify_local_args(live - 1, paramInfo, %{}, vst)

      %{} ->
        :erlang.error(:local_call_to_unknown_function)
    end
  end

  defp verify_call_args(_, live, vst) do
    verify_remote_args_1(live - 1, vst)
  end

  defp verify_remote_args_1(-1, _) do
    :ok
  end

  defp verify_remote_args_1(x, vst) do
    assert_durable_term({:x, x}, vst)
    verify_remote_args_1(x - 1, vst)
  end

  defp verify_local_args(-1, _ParamInfo, _CtxIds, _Vst) do
    :ok
  end

  defp verify_local_args(x, paramInfo, ctxRefs, vst) do
    reg = {:x, x}
    assert_not_fragile(reg, vst)

    case get_movable_term_type(reg, vst) do
      r_t_bs_context() = type ->
        vRef = get_reg_vref(reg, vst)

        case ctxRefs do
          %{^vRef => other} ->
            :erlang.error({:multiple_match_contexts, [reg, other]})

          %{} ->
            verify_arg_type(reg, type, paramInfo, vst)
            verify_local_args(x - 1, paramInfo, Map.put(ctxRefs, vRef, reg), vst)
        end

      type ->
        verify_arg_type(reg, type, paramInfo, vst)
        verify_local_args(x - 1, paramInfo, ctxRefs, vst)
    end
  end

  defp verify_arg_type(reg, givenType, paramInfo, vst) do
    case {paramInfo, givenType} do
      {%{^reg => info}, r_t_bs_context()} ->
        case member(:accepts_match_context, info) do
          true ->
            verify_arg_type_1(reg, givenType, info, vst)

          false ->
            :erlang.error(:no_bs_start_match2)
        end

      {_, r_t_bs_context()} ->
        :erlang.error(:no_bs_start_match2)

      {%{^reg => info}, _} ->
        verify_arg_type_1(reg, givenType, info, vst)

      {%{}, _} ->
        :ok
    end
  end

  defp verify_arg_type_1(reg, givenType, info, vst) do
    requiredType = :proplists.get_value(:type, info, :any)

    case meet(givenType, requiredType) do
      type when type !== :none and r_vst(vst, :level) === :weak ->
        :ok

      ^givenType ->
        true = givenType !== :none
        :ok

      _ ->
        :erlang.error({:bad_arg_type, reg, givenType, requiredType})
    end
  end

  defp allocate(tag, stk, heap, live, r_vst(current: r_st(numy: :none) = st) = vst0) do
    verify_live(live, vst0)
    vst1 = r_vst(vst0, current: r_st(st, numy: stk))
    vst2 = prune_x_regs(live, vst1)
    vst = init_stack(tag, stk - 1, vst2)
    heap_alloc(heap, vst)
  end

  defp allocate(_, _, _, _, r_vst(current: r_st(numy: numy))) do
    :erlang.error({:existing_stack_frame, {:size, numy}})
  end

  defp deallocate(r_vst(current: st) = vst) do
    r_vst(vst, current: r_st(st, ys: %{}, numy: :none))
  end

  defp init_stack(_Tag, -1, vst) do
    vst
  end

  defp init_stack(tag, y, vst) do
    init_stack(tag, y - 1, create_tag(tag, :allocate, [], {:y, y}, vst))
  end

  defp trim_stack(from, to, top, r_st(ys: ys0) = st)
       when from === top do
    ys =
      :maps.filter(
        fn {:y, y}, _ ->
          y < to
        end,
        ys0
      )

    r_st(st, numy: to, ys: ys)
  end

  defp trim_stack(from, to, top, st0) do
    src = {:y, from}
    dst = {:y, to}
    r_st(ys: ys0) = st0

    ys =
      case ys0 do
        %{^src => ref} ->
          Map.put(ys0, dst, ref)

        %{} ->
          :erlang.error({:invalid_shift, src, dst})
      end

    st = r_st(st0, ys: ys)
    trim_stack(from + 1, to + 1, top, st)
  end

  defp test_heap(heap, live, vst0) do
    verify_live(live, vst0)
    verify_y_init(vst0)
    vst = prune_x_regs(live, vst0)
    heap_alloc(heap, vst)
  end

  defp heap_alloc(heap, r_vst(current: st0) = vst) do
    st1 = kill_heap_allocation(st0)
    st = heap_alloc_1(heap, st1)
    r_vst(vst, current: st)
  end

  defp heap_alloc_1({:alloc, alloc}, st) do
    heap_alloc_2(alloc, st)
  end

  defp heap_alloc_1(heapWords, st) when is_integer(heapWords) do
    r_st(st, h: heapWords)
  end

  defp heap_alloc_2([{:words, heapWords} | t], st0) do
    st = r_st(st0, h: heapWords)
    heap_alloc_2(t, st)
  end

  defp heap_alloc_2([{:funs, funs} | t], st0) do
    st = r_st(st0, hl: funs)
    heap_alloc_2(t, st)
  end

  defp heap_alloc_2([{:floats, floats} | t], st0) do
    st = r_st(st0, hf: floats)
    heap_alloc_2(t, st)
  end

  defp heap_alloc_2([], st) do
    st
  end

  defp prune_x_regs(live, r_vst(current: st0) = vst)
       when is_integer(live) do
    r_st(fragile: fragile0, xs: xs0) = st0

    fragile =
      :cerl_sets.filter(
        fn
          {:x, x} ->
            x < live

          {:y, _} ->
            true
        end,
        fragile0
      )

    xs =
      :maps.filter(
        fn {:x, x}, _ ->
          x < live
        end,
        xs0
      )

    st = r_st(st0, fragile: fragile, xs: xs)
    r_vst(vst, current: st)
  end

  defp assert_choices([{tag, _}, {:f, _} | t]) do
    cond do
      tag === :atom or tag === :float or tag === :integer ->
        assert_choices_1(t, tag)

      true ->
        :erlang.error(:bad_select_list)
    end
  end

  defp assert_choices([]) do
    :ok
  end

  defp assert_choices_1([{tag, _}, {:f, _} | t], tag) do
    assert_choices_1(t, tag)
  end

  defp assert_choices_1([_, {:f, _} | _], _Tag) do
    :erlang.error(:bad_select_list)
  end

  defp assert_choices_1([], _Tag) do
    :ok
  end

  defp assert_arities([arity, {:f, _} | t]) when is_integer(arity) do
    assert_arities(t)
  end

  defp assert_arities([]) do
    :ok
  end

  defp assert_arities(_) do
    :erlang.error(:bad_tuple_arity_list)
  end

  defp is_float_arith_bif(:fadd, [_, _]) do
    true
  end

  defp is_float_arith_bif(:fdiv, [_, _]) do
    true
  end

  defp is_float_arith_bif(:fmul, [_, _]) do
    true
  end

  defp is_float_arith_bif(:fnegate, [_]) do
    true
  end

  defp is_float_arith_bif(:fsub, [_, _]) do
    true
  end

  defp is_float_arith_bif(_, _) do
    false
  end

  defp validate_float_arith_bif(ss, dst, vst0) do
    _ =
      for s <- ss do
        assert_freg_set(s, vst0)
      end

    assert_fls(:cleared, vst0)
    vst = set_fls(:cleared, vst0)
    set_freg(dst, vst)
  end

  defp assert_fls(fls, vst) do
    case get_fls(vst) do
      ^fls ->
        :ok

      otherFls ->
        :erlang.error({:bad_floating_point_state, otherFls})
    end
  end

  defp set_fls(fls, r_vst(current: r_st() = st) = vst)
       when is_atom(fls) do
    r_vst(vst, current: r_st(st, fls: fls))
  end

  defp get_fls(r_vst(current: r_st(fls: fls))) when is_atom(fls) do
    fls
  end

  defp init_fregs() do
    0
  end

  defp set_freg(
         {:fr, fr} = freg,
         r_vst(current: r_st(f: fregs0) = st) = vst
       ) do
    check_limit(freg)
    bit = 1 <<< fr

    cond do
      fregs0 &&& bit === 0 ->
        fregs = fregs0 ||| bit
        r_vst(vst, current: r_st(st, f: fregs))

      true ->
        vst
    end
  end

  defp set_freg(fr, _) do
    :erlang.error({:bad_target, fr})
  end

  defp assert_freg_set({:fr, fr} = freg, r_vst(current: r_st(f: fregs)))
       when is_integer(fr) and 0 <= fr do
    cond do
      fregs >>> fr &&& 1 === 0 ->
        :erlang.error({:uninitialized_reg, freg})

      true ->
        :ok
    end
  end

  defp assert_freg_set(fr, _) do
    :erlang.error({:bad_source, fr})
  end

  defp assert_unique_map_keys([]) do
    :erlang.error(:empty_field_list)
  end

  defp assert_unique_map_keys([_]) do
    :ok
  end

  defp assert_unique_map_keys([_, _ | _] = ls) do
    vs =
      for l <- ls do
        assert_literal(l)
        l
      end

    case length(vs) === :cerl_sets.size(:cerl_sets.from_list(vs)) do
      true ->
        :ok

      false ->
        :erlang.error(:keys_not_unique)
    end
  end

  defp bsm_save(reg, {:atom, :start}, vst) do
    assert_type(r_t_bs_context(), reg, vst)
    vst
  end

  defp bsm_save(reg, savePoint, vst) do
    case get_movable_term_type(reg, vst) do
      r_t_bs_context(valid: bits, slots: slots) = ctxt0
      when savePoint < slots ->
        ctx =
          r_t_bs_context(ctxt0,
            valid: bits ||| 1 <<< savePoint,
            slots: slots
          )

        override_type(ctx, reg, vst)

      _ ->
        :erlang.error({:illegal_save, savePoint})
    end
  end

  defp bsm_restore(reg, {:atom, :start}, vst) do
    assert_type(r_t_bs_context(), reg, vst)
    vst
  end

  defp bsm_restore(reg, savePoint, vst) do
    case get_movable_term_type(reg, vst) do
      r_t_bs_context(valid: bits, slots: slots) when savePoint < slots ->
        case bits &&& 1 <<< savePoint do
          0 ->
            :erlang.error({:illegal_restore, savePoint, :not_set})

          _ ->
            vst
        end

      _ ->
        :erlang.error({:illegal_restore, savePoint, :range})
    end
  end

  defp validate_select_val(_Fail, _Choices, _Src, r_vst(current: :none) = vst) do
    vst
  end

  defp validate_select_val(fail, [val, {:f, l} | t], src, vst0) do
    vst =
      branch(
        l,
        vst0,
        fn branchVst ->
          update_eq_types(src, val, branchVst)
        end,
        fn failVst ->
          update_ne_types(src, val, failVst)
        end
      )

    validate_select_val(fail, t, src, vst)
  end

  defp validate_select_val(fail, [], _Src, vst) do
    branch(fail, vst, fn succVst ->
      kill_state(succVst)
    end)
  end

  defp validate_select_tuple_arity(_Fail, _Choices, _Src, r_vst(current: :none) = vst) do
    vst
  end

  defp validate_select_tuple_arity(fail, [arity, {:f, l} | t], tuple, vst0) do
    type = r_t_tuple(exact: true, size: arity)

    vst =
      branch(
        l,
        vst0,
        fn branchVst ->
          update_type(&meet/2, type, tuple, branchVst)
        end,
        fn failVst ->
          update_type(&subtract/2, type, tuple, failVst)
        end
      )

    validate_select_tuple_arity(fail, t, tuple, vst)
  end

  defp validate_select_tuple_arity(fail, [], _, r_vst() = vst) do
    branch(fail, vst, fn succVst ->
      kill_state(succVst)
    end)
  end

  defp infer_types(compareOp, {kind, _} = lHS, rHS, vst)
       when kind === :x or kind === :y do
    infer_types(compareOp, get_reg_vref(lHS, vst), rHS, vst)
  end

  defp infer_types(compareOp, lHS, {kind, _} = rHS, vst)
       when kind === :x or kind === :y do
    infer_types(compareOp, lHS, get_reg_vref(rHS, vst), vst)
  end

  defp infer_types(compareOp, lHS, rHS, r_vst(current: r_st(vs: vs)) = vst0) do
    case vs do
      %{^lHS => lEntry, ^rHS => rEntry} ->
        vst = infer_types_1(lEntry, rHS, compareOp, vst0)
        infer_types_1(rEntry, lHS, compareOp, vst)

      %{^lHS => lEntry} ->
        infer_types_1(lEntry, rHS, compareOp, vst0)

      %{^rHS => rEntry} ->
        infer_types_1(rEntry, lHS, compareOp, vst0)

      %{} ->
        vst0
    end
  end

  defp infer_types_1(r_value(op: {:bif, :"=:="}, args: [lHS, rHS]), val, op, vst) do
    case val do
      {:atom, bool}
      when (op === :eq_exact and bool) or
             (op === :ne_exact and not bool) ->
        update_eq_types(lHS, rHS, vst)

      {:atom, bool}
      when (op === :ne_exact and bool) or
             (op === :eq_exact and not bool) ->
        update_ne_types(lHS, rHS, vst)

      _ ->
        vst
    end
  end

  defp infer_types_1(r_value(op: {:bif, :"=/="}, args: [lHS, rHS]), val, op, vst) do
    case val do
      {:atom, bool}
      when (op === :ne_exact and bool) or
             (op === :eq_exact and not bool) ->
        update_ne_types(lHS, rHS, vst)

      {:atom, bool}
      when (op === :eq_exact and bool) or
             (op === :ne_exact and not bool) ->
        update_eq_types(lHS, rHS, vst)

      _ ->
        vst
    end
  end

  defp infer_types_1(
         r_value(
           op: {:bif, :element},
           args: [{:integer, index}, tuple]
         ),
         val,
         op,
         vst
       )
       when index >= 1 do
    elementType = get_term_type(val, vst)
    es = :beam_types.set_tuple_element(index, elementType, %{})
    tupleType = r_t_tuple(size: index, elements: es)

    case op do
      :eq_exact ->
        update_type(&meet/2, tupleType, tuple, vst)

      :ne_exact ->
        case :beam_types.is_singleton_type(elementType) do
          true when es !== %{} ->
            update_type(&subtract/2, tupleType, tuple, vst)

          _ ->
            vst
        end
    end
  end

  defp infer_types_1(r_value(op: {:bif, :is_atom}, args: [src]), val, op, vst) do
    infer_type_test_bif(r_t_atom(), src, val, op, vst)
  end

  defp infer_types_1(r_value(op: {:bif, :is_boolean}, args: [src]), val, op, vst) do
    infer_type_test_bif(:beam_types.make_boolean(), src, val, op, vst)
  end

  defp infer_types_1(r_value(op: {:bif, :is_binary}, args: [src]), val, op, vst) do
    infer_type_test_bif(r_t_bitstring(size_unit: 8), src, val, op, vst)
  end

  defp infer_types_1(r_value(op: {:bif, :is_bitstring}, args: [src]), val, op, vst) do
    infer_type_test_bif(r_t_bitstring(), src, val, op, vst)
  end

  defp infer_types_1(r_value(op: {:bif, :is_float}, args: [src]), val, op, vst) do
    infer_type_test_bif(r_t_float(), src, val, op, vst)
  end

  defp infer_types_1(r_value(op: {:bif, :is_integer}, args: [src]), val, op, vst) do
    infer_type_test_bif(r_t_integer(), src, val, op, vst)
  end

  defp infer_types_1(r_value(op: {:bif, :is_list}, args: [src]), val, op, vst) do
    infer_type_test_bif(r_t_list(), src, val, op, vst)
  end

  defp infer_types_1(r_value(op: {:bif, :is_map}, args: [src]), val, op, vst) do
    infer_type_test_bif(r_t_map(), src, val, op, vst)
  end

  defp infer_types_1(r_value(op: {:bif, :is_number}, args: [src]), val, op, vst) do
    infer_type_test_bif(:number, src, val, op, vst)
  end

  defp infer_types_1(r_value(op: {:bif, :is_tuple}, args: [src]), val, op, vst) do
    infer_type_test_bif(r_t_tuple(), src, val, op, vst)
  end

  defp infer_types_1(r_value(op: {:bif, :tuple_size}, args: [tuple]), {:integer, arity}, op, vst) do
    type = r_t_tuple(exact: true, size: arity)

    case op do
      :eq_exact ->
        update_type(&meet/2, type, tuple, vst)

      :ne_exact ->
        update_type(&subtract/2, type, tuple, vst)
    end
  end

  defp infer_types_1(_, _, _, vst) do
    vst
  end

  defp infer_type_test_bif(type, src, val, op, vst) do
    case val do
      {:atom, bool}
      when (op === :eq_exact and bool) or
             (op === :ne_exact and not bool) ->
        update_type(&meet/2, type, src, vst)

      {:atom, bool}
      when (op === :ne_exact and bool) or
             (op === :eq_exact and not bool) ->
        update_type(&subtract/2, type, src, vst)

      _ ->
        vst
    end
  end

  defp assign({:y, _} = src, {:y, _} = dst, vst) do
    case get_raw_type(src, vst) do
      :initialized ->
        create_tag(:initialized, :init, [], dst, vst)

      _ ->
        assign_1(src, dst, vst)
    end
  end

  defp assign({kind, _} = src, dst, vst)
       when kind === :x or
              kind === :y do
    assign_1(src, dst, vst)
  end

  defp assign(literal, dst, vst) do
    type = get_literal_type(literal)
    create_term(type, :move, [literal], dst, vst)
  end

  defp create_tag(tag, _Op, _Ss, {:y, _} = dst, r_vst(current: r_st(ys: ys0) = st0) = vst) do
    case :maps.get(dst, ys0, :uninitialized) do
      {:catchtag, _} = prev ->
        :erlang.error(prev)

      {:trytag, _} = prev ->
        :erlang.error(prev)

      _ ->
        check_try_catch_tags(tag, dst, vst)
        ys = Map.put(ys0, dst, tag)
        st = r_st(st0, ys: ys)
        remove_fragility(dst, r_vst(vst, current: st))
    end
  end

  defp create_tag(_Tag, _Op, _Ss, dst, _Vst) do
    :erlang.error({:invalid_tag_register, dst})
  end

  defp kill_tag(
         {:y, _} = reg,
         r_vst(current: r_st(ys: ys0) = st0) = vst
       ) do
    _ = get_tag_type(reg, vst)
    ys = Map.put(ys0, reg, :initialized)
    r_vst(vst, current: r_st(st0, ys: ys))
  end

  defp create_term(type, op, ss0, dst, vst0) do
    create_term(type, op, ss0, dst, vst0, vst0)
  end

  defp create_term(type, op, ss0, dst, vst0, origVst) do
    {ref, vst1} = new_value(type, op, resolve_args(ss0, origVst), vst0)
    vst = remove_fragility(dst, vst1)
    set_reg_vref(ref, dst, vst)
  end

  defp extract_term(type, op, ss0, dst, vst0) do
    extract_term(type, op, ss0, dst, vst0, vst0)
  end

  defp extract_term(type, op, ss0, dst, vst0, origVst) do
    {ref, vst1} = new_value(type, op, resolve_args(ss0, origVst), vst0)
    vst = propagate_fragility(dst, ss0, vst1)
    set_reg_vref(ref, dst, vst)
  end

  defp resolve_args([{kind, _} = src | args], vst)
       when kind === :x or kind === :y do
    [get_reg_vref(src, vst) | resolve_args(args, vst)]
  end

  defp resolve_args([lit | args], vst) do
    assert_literal(lit)
    [lit | resolve_args(args, vst)]
  end

  defp resolve_args([], _Vst) do
    []
  end

  defp override_type(type, reg, vst) do
    update_type(
      fn _, t ->
        t
      end,
      type,
      reg,
      vst
    )
  end

  defp update_type(merge, with, r_value_ref() = ref, vst) do
    current = get_raw_type(ref, vst)

    case merge.(current, with) do
      :none ->
        throw({:type_conflict, current, with})

      type ->
        set_type(type, ref, vst)
    end
  end

  defp update_type(merge, with, {kind, _} = reg, vst)
       when kind === :x or kind === :y do
    update_type(merge, with, get_reg_vref(reg, vst), vst)
  end

  defp update_type(merge, with, literal, vst) do
    type = get_literal_type(literal)

    case merge.(type, with) do
      :none ->
        throw({:type_conflict, type, with})

      _Type ->
        vst
    end
  end

  defp update_eq_types(lHS, rHS, vst0) do
    lType = get_term_type(lHS, vst0)
    rType = get_term_type(rHS, vst0)
    vst1 = update_type(&meet/2, rType, lHS, vst0)
    vst = update_type(&meet/2, lType, rHS, vst1)
    infer_types(:eq_exact, lHS, rHS, vst)
  end

  defp update_ne_types(lHS, rHS, vst0) do
    vst1 = update_ne_types_1(lHS, rHS, vst0)
    vst = update_ne_types_1(rHS, lHS, vst1)
    infer_types(:ne_exact, lHS, rHS, vst)
  end

  defp update_ne_types_1(lHS, rHS, vst0) do
    rType = get_term_type(rHS, vst0)

    case :beam_types.is_singleton_type(rType) do
      true ->
        vst = update_type(&subtract/2, rType, lHS, vst0)
        lType = get_term_type(lHS, vst)

        case :beam_types.get_singleton_value(lType) do
          {:ok, value} ->
            infer_types(:eq_exact, lHS, value_to_literal(value), vst)

          :error ->
            vst
        end

      false ->
        vst0
    end
  end

  defp assign_1(src, dst, vst0) do
    assert_movable(src, vst0)
    vst = propagate_fragility(dst, [src], vst0)
    set_reg_vref(get_reg_vref(src, vst), dst, vst)
  end

  defp set_reg_vref(ref, {:x, _} = dst, vst) do
    check_limit(dst)
    r_vst(current: r_st(xs: xs0) = st0) = vst
    st = r_st(st0, xs: Map.put(xs0, dst, ref))
    r_vst(vst, current: st)
  end

  defp set_reg_vref(ref, {:y, _} = dst, r_vst(current: r_st(ys: ys0) = st0) = vst) do
    check_limit(dst)

    case ys0 do
      %{^dst => {:catchtag, _} = tag} ->
        :erlang.error(tag)

      %{^dst => {:trytag, _} = tag} ->
        :erlang.error(tag)

      %{^dst => _} ->
        st = r_st(st0, ys: Map.put(ys0, dst, ref))
        r_vst(vst, current: st)

      %{} ->
        :erlang.error({:invalid_store, dst})
    end
  end

  defp set_reg_vref(_Ref, dst, _Vst) do
    :erlang.error({:invalid_register, dst})
  end

  defp get_reg_vref({:x, _} = src, r_vst(current: r_st(xs: xs))) do
    check_limit(src)

    case xs do
      %{^src => r_value_ref() = ref} ->
        ref

      %{} ->
        :erlang.error({:uninitialized_reg, src})
    end
  end

  defp get_reg_vref({:y, _} = src, r_vst(current: r_st(ys: ys))) do
    check_limit(src)

    case ys do
      %{^src => r_value_ref() = ref} ->
        ref

      %{^src => :initialized} ->
        :erlang.error({:unassigned, src})

      %{^src => tag} when tag !== :uninitialized ->
        :erlang.error(tag)

      %{} ->
        :erlang.error({:uninitialized_reg, src})
    end
  end

  defp get_reg_vref(src, _Vst) do
    :erlang.error({:invalid_register, src})
  end

  defp set_type(type, r_value_ref() = ref, r_vst(current: r_st(vs: vs0) = st) = vst) do
    %{^ref => r_value() = entry} = vs0
    vs = Map.put(vs0, ref, r_value(entry, type: type))
    r_vst(vst, current: r_st(st, vs: vs))
  end

  defp new_value(:none, _, _, _) do
    :erlang.error(:creating_none_value)
  end

  defp new_value(type, op, ss, r_vst(current: r_st(vs: vs0) = st, ref_ctr: counter) = vst) do
    ref = r_value_ref(id: counter)
    vs = Map.put(vs0, ref, r_value(op: op, args: ss, type: type))
    {ref, r_vst(vst, current: r_st(st, vs: vs), ref_ctr: counter + 1)}
  end

  defp kill_catch_tag(
         reg,
         r_vst(current: r_st(ct: [tag | tags]) = st) = vst0
       ) do
    vst =
      r_vst(vst0,
        current:
          r_st(st,
            ct: tags,
            fls: :undefined
          )
      )

    ^tag = get_tag_type(reg, vst)
    kill_tag(reg, vst)
  end

  defp check_try_catch_tags(type, {:y, n} = reg, vst) do
    case is_try_catch_tag(type) do
      true ->
        case collect_try_catch_tags(n - 1, vst, []) do
          [_ | _] = bad ->
            :erlang.error({:bad_try_catch_nesting, reg, bad})

          [] ->
            :ok
        end

      false ->
        :ok
    end
  end

  defp assert_no_exception(0) do
    :erlang.error(:throws_exception)
  end

  defp assert_no_exception(_) do
    :ok
  end

  defp assert_term(src, vst) do
    _ = get_term_type(src, vst)
    :ok
  end

  defp assert_movable(src, vst) do
    _ = get_movable_term_type(src, vst)
    :ok
  end

  defp assert_literal(src) do
    case is_literal(src) do
      true ->
        :ok

      false ->
        :erlang.error({:literal_required, src})
    end
  end

  defp assert_not_literal(src) do
    case is_literal(src) do
      true ->
        :erlang.error({:literal_not_allowed, src})

      false ->
        :ok
    end
  end

  defp is_literal(nil) do
    true
  end

  defp is_literal({:atom, a}) when is_atom(a) do
    true
  end

  defp is_literal({:float, f}) when is_float(f) do
    true
  end

  defp is_literal({:integer, i}) when is_integer(i) do
    true
  end

  defp is_literal({:literal, _L}) do
    true
  end

  defp is_literal(_) do
    false
  end

  defp value_to_literal([]) do
    nil
  end

  defp value_to_literal(a) when is_atom(a) do
    {:atom, a}
  end

  defp value_to_literal(f) when is_float(f) do
    {:float, f}
  end

  defp value_to_literal(i) when is_integer(i) do
    {:integer, i}
  end

  defp value_to_literal(other) do
    {:literal, other}
  end

  defp normalize(r_t_abstract() = a) do
    :erlang.error({:abstract_type, a})
  end

  defp normalize(t) do
    :beam_types.normalize(t)
  end

  defp join(same, same) do
    same
  end

  defp join(
         r_t_abstract(kind: {:ms_position, unitA}),
         r_t_abstract(kind: {:ms_position, unitB})
       ) do
    r_t_abstract(kind: {:ms_position, gcd(unitA, unitB)})
  end

  defp join(r_t_abstract() = a, b) do
    r_t_abstract(kind: {:join, a, b})
  end

  defp join(a, r_t_abstract() = b) do
    r_t_abstract(kind: {:join, a, b})
  end

  defp join(a, b) do
    :beam_types.join(a, b)
  end

  defp meet(same, same) do
    same
  end

  defp meet(
         r_t_abstract(kind: {:ms_position, unitA}),
         r_t_abstract(kind: {:ms_position, unitB})
       ) do
    unit = div(unitA * unitB, gcd(unitA, unitB))
    r_t_abstract(kind: {:ms_position, unit})
  end

  defp meet(r_t_abstract() = a, b) do
    r_t_abstract(kind: {:meet, a, b})
  end

  defp meet(a, r_t_abstract() = b) do
    r_t_abstract(kind: {:meet, a, b})
  end

  defp meet(a, b) do
    :beam_types.meet(a, b)
  end

  defp subtract(r_t_abstract() = a, b) do
    r_t_abstract(kind: {:subtract, a, b})
  end

  defp subtract(a, r_t_abstract() = b) do
    r_t_abstract(kind: {:subtract, a, b})
  end

  defp subtract(a, b) do
    :beam_types.subtract(a, b)
  end

  defp assert_type(requiredType, term, vst) do
    givenType = get_movable_term_type(term, vst)

    case meet(requiredType, givenType) do
      ^givenType ->
        :ok

      _RequiredType ->
        :erlang.error({:bad_type, {:needed, requiredType}, {:actual, givenType}})
    end
  end

  defp validate_src(ss, vst) when is_list(ss) do
    _ =
      for s <- ss do
        assert_term(s, vst)
      end

    :ok
  end

  defp get_term_type(src, vst) do
    case get_movable_term_type(src, vst) do
      r_t_bs_context() ->
        :erlang.error({:match_context, src})

      r_t_abstract() ->
        :erlang.error({:abstract_term, src})

      type ->
        type
    end
  end

  defp get_movable_term_type(src, vst) do
    case get_raw_type(src, vst) do
      r_t_abstract(kind: :unfinished_tuple = kind) ->
        :erlang.error({kind, src})

      :initialized ->
        :erlang.error({:unassigned, src})

      :uninitialized ->
        :erlang.error({:uninitialized_reg, src})

      {:catchtag, _} ->
        :erlang.error({:catchtag, src})

      {:trytag, _} ->
        :erlang.error({:trytag, src})

      type ->
        type
    end
  end

  defp get_tag_type({:y, _} = src, vst) do
    case get_raw_type(src, vst) do
      {:catchtag, _} = tag ->
        tag

      {:trytag, _} = tag ->
        tag

      :uninitialized = tag ->
        tag

      :initialized = tag ->
        tag

      other ->
        :erlang.error({:invalid_tag, src, other})
    end
  end

  defp get_tag_type(src, _) do
    :erlang.error({:invalid_tag_register, src})
  end

  defp get_raw_type({:x, x} = src, r_vst(current: r_st(xs: xs)) = vst)
       when is_integer(x) do
    check_limit(src)

    case xs do
      %{^src => r_value_ref() = ref} ->
        get_raw_type(ref, vst)

      %{} ->
        :uninitialized
    end
  end

  defp get_raw_type({:y, y} = src, r_vst(current: r_st(ys: ys)) = vst)
       when is_integer(y) do
    check_limit(src)

    case ys do
      %{^src => r_value_ref() = ref} ->
        get_raw_type(ref, vst)

      %{^src => tag} ->
        tag

      %{} ->
        :uninitialized
    end
  end

  defp get_raw_type(r_value_ref() = ref, r_vst(current: r_st(vs: vs))) do
    case vs do
      %{^ref => r_value(type: type)} ->
        type

      %{} ->
        :none
    end
  end

  defp get_raw_type(src, r_vst(current: r_st())) do
    get_literal_type(src)
  end

  defp get_literal_type(nil) do
    :beam_types.make_type_from_value([])
  end

  defp get_literal_type({:atom, a}) when is_atom(a) do
    :beam_types.make_type_from_value(a)
  end

  defp get_literal_type({:float, f}) when is_float(f) do
    :beam_types.make_type_from_value(f)
  end

  defp get_literal_type({:integer, i}) when is_integer(i) do
    :beam_types.make_type_from_value(i)
  end

  defp get_literal_type({:literal, l}) do
    :beam_types.make_type_from_value(l)
  end

  defp get_literal_type(t) do
    :erlang.error({:not_literal, t})
  end

  defp branch(lbl, vst0, failFun, succFun) do
    validate_branch(lbl, vst0)
    r_vst(current: st0) = vst0

    try do
      failFun.(vst0)
    catch
      {:type_conflict, _, _} ->
        succFun.(vst0)
    else
      vst1 ->
        vst2 = fork_state(lbl, vst1)
        vst = r_vst(vst2, current: st0)

        try do
          succFun.(vst)
        catch
          {:type_conflict, _, _} ->
            kill_state(vst)
        else
          v ->
            v
        end
    end
  end

  defp validate_branch(lbl, r_vst(current: r_st(ct: tags))) do
    validate_branch_1(lbl, tags)
  end

  defp validate_branch_1(lbl, [{:trytag, failLbls} | tags]) do
    case :ordsets.is_element(lbl, failLbls) do
      true ->
        :erlang.error({:illegal_branch, :try_handler, lbl})

      false ->
        validate_branch_1(lbl, tags)
    end
  end

  defp validate_branch_1(lbl, [_ | tags]) do
    validate_branch_1(lbl, tags)
  end

  defp validate_branch_1(_Lbl, _Tags) do
    :ok
  end

  defp branch(fail, vst, succFun) do
    branch(
      fail,
      vst,
      fn v ->
        v
      end,
      succFun
    )
  end

  defp branch(fail, vst) do
    branch(fail, vst, fn v ->
      v
    end)
  end

  defp fork_state(0, vst0) do
    r_vst(current: r_st(ct: catchTags, numy: numY)) = vst0
    assert_float_checked(vst0)
    verify_y_init(vst0)

    case catchTags do
      [{_, [fail]} | _] when is_integer(fail) ->
        true = fail !== 0
        true = numY !== :none
        vst = set_receive_marker(:none, vst0)
        fork_state(fail, vst)

      [] ->
        vst0

      _ ->
        :erlang.error(:ambiguous_catch_try_state)
    end
  end

  defp fork_state(
         l,
         r_vst(current: st0, branched: branched0, ref_ctr: counter0) = vst
       ) do
    {st, counter} = merge_states(l, st0, branched0, counter0)
    branched = Map.put(branched0, l, st)
    r_vst(vst, branched: branched, ref_ctr: counter)
  end

  defp merge_states(l, st, branched, counter) when l !== 0 do
    case branched do
      %{^l => otherSt} ->
        merge_states_1(st, otherSt, counter)

      %{} ->
        {st, counter}
    end
  end

  defp merge_states_1(st, :none, counter) do
    {st, counter}
  end

  defp merge_states_1(:none, st, counter) do
    {st, counter}
  end

  defp merge_states_1(stA, stB, counter0) do
    r_st(
      xs: xsA,
      ys: ysA,
      vs: vsA,
      fragile: fragA,
      numy: numYA,
      h: hA,
      ct: ctA,
      recv_marker: markerA,
      ms_positions: msPosA
    ) = stA

    r_st(
      xs: xsB,
      ys: ysB,
      vs: vsB,
      fragile: fragB,
      numy: numYB,
      h: hB,
      ct: ctB,
      recv_marker: markerB,
      ms_positions: msPosB
    ) = stB

    {xs, merge0, counter1} = merge_regs(xsA, xsB, %{}, counter0)
    {ys, merge, counter} = merge_regs(ysA, ysB, merge0, counter1)
    vs = merge_values(merge, vsA, vsB)
    marker = merge_receive_marker(markerA, markerB)
    msPos = merge_ms_positions(msPosA, msPosB, vs)
    fragile = merge_fragility(fragA, fragB)
    numY = merge_stk(numYA, numYB)
    ct = merge_ct(ctA, ctB)

    st =
      r_st(
        xs: xs,
        ys: ys,
        vs: vs,
        fragile: fragile,
        numy: numY,
        h: min(hA, hB),
        ct: ct,
        recv_marker: marker,
        ms_positions: msPos
      )

    {st, counter}
  end

  defp merge_regs(rsA, rsB, merge, counter) do
    keys =
      cond do
        map_size(rsA) <= map_size(rsB) ->
          :maps.keys(rsA)

        map_size(rsA) > map_size(rsB) ->
          :maps.keys(rsB)
      end

    merge_regs_1(keys, rsA, rsB, %{}, merge, counter)
  end

  defp merge_regs_1([reg | keys], rsA, rsB, regs, merge0, counter0) do
    case {rsA, rsB} do
      {%{^reg => r_value_ref() = refA}, %{reg => r_value_ref() = refB}} ->
        {ref, merge, counter} = merge_vrefs(refA, refB, merge0, counter0)
        merge_regs_1(keys, rsA, rsB, Map.put(regs, reg, ref), merge, counter)

      {%{^reg => tagA}, %{^reg => tagB}} ->
        {:y, _} = reg
        merge_regs_1(keys, rsA, rsB, Map.put(regs, reg, merge_tags(tagA, tagB)), merge0, counter0)

      {%{}, %{}} ->
        merge_regs_1(keys, rsA, rsB, regs, merge0, counter0)
    end
  end

  defp merge_regs_1([], _, _, regs, merge, counter) do
    {regs, merge, counter}
  end

  defp merge_tags(same, same) do
    same
  end

  defp merge_tags(:uninitialized, _) do
    :uninitialized
  end

  defp merge_tags(_, :uninitialized) do
    :uninitialized
  end

  defp merge_tags({:trytag, lblsA}, {:trytag, lblsB}) do
    {:trytag, :ordsets.union(lblsA, lblsB)}
  end

  defp merge_tags({:catchtag, lblsA}, {:catchtag, lblsB}) do
    {:catchtag, :ordsets.union(lblsA, lblsB)}
  end

  defp merge_tags(_A, _B) do
    :initialized
  end

  defp merge_vrefs(ref, ref, merge, counter) do
    {ref, Map.put(merge, ref, ref), counter}
  end

  defp merge_vrefs(refA, refB, merge, counter) do
    key = {refA, refB}

    case merge do
      %{^key => ref} ->
        {ref, merge, counter}

      %{} ->
        ref = r_value_ref(id: counter)
        {ref, Map.put(merge, key, ref), counter + 1}
    end
  end

  defp merge_values(merge, vsA, vsB) do
    :maps.fold(
      fn spec, new, acc ->
        mv_1(spec, new, vsA, vsB, acc)
      end,
      %{},
      merge
    )
  end

  defp mv_1(same, same, vsA, vsB, acc0) do
    r_value(
      type: typeA,
      args: args
    ) = entryA = :erlang.map_get(same, vsA)

    r_value(
      type: typeB,
      args: ^args
    ) = entryB = :erlang.map_get(same, vsB)

    entry =
      case join(typeA, typeB) do
        ^typeA ->
          entryA

        ^typeB ->
          entryB

        joinedType ->
          r_value(entryA, type: joinedType)
      end

    acc = Map.put(acc0, same, entry)
    mv_args(args, vsA, vsB, acc)
  end

  defp mv_1({refA, refB}, new, vsA, vsB, acc) do
    r_value(type: typeA) = :erlang.map_get(refA, vsA)
    r_value(type: typeB) = :erlang.map_get(refB, vsB)
    Map.put(acc, new, r_value(op: :join, args: [], type: join(typeA, typeB)))
  end

  defp mv_args([r_value_ref() = arg | args], vsA, vsB, acc0) do
    case acc0 do
      %{^arg => _} ->
        mv_args(args, vsA, vsB, acc0)

      %{} ->
        acc = mv_1(arg, arg, vsA, vsB, acc0)
        mv_args(args, vsA, vsB, acc)
    end
  end

  defp mv_args([_ | args], vsA, vsB, acc) do
    mv_args(args, vsA, vsB, acc)
  end

  defp mv_args([], _VsA, _VsB, acc) do
    acc
  end

  defp merge_fragility(fragileA, fragileB) do
    :cerl_sets.union(fragileA, fragileB)
  end

  defp merge_ms_positions(msPosA, msPosB, vs) do
    keys =
      cond do
        map_size(msPosA) <= map_size(msPosB) ->
          :maps.keys(msPosA)

        map_size(msPosA) > map_size(msPosB) ->
          :maps.keys(msPosB)
      end

    merge_ms_positions_1(keys, msPosA, msPosB, vs, %{})
  end

  defp merge_ms_positions_1([key | keys], msPosA, msPosB, vs, acc) do
    case {msPosA, msPosB} do
      {%{^key => pos}, %{^key => pos}}
      when :erlang.is_map_key(pos, vs) ->
        merge_ms_positions_1(keys, msPosA, msPosB, vs, Map.put(acc, key, pos))

      {%{}, %{}} ->
        merge_ms_positions_1(keys, msPosA, msPosB, vs, acc)
    end
  end

  defp merge_ms_positions_1([], _MsPosA, _MsPosB, _Vs, acc) do
    acc
  end

  defp merge_receive_marker(same, same) do
    same
  end

  defp merge_receive_marker(:none, :initialized) do
    :initialized
  end

  defp merge_receive_marker(:initialized, :none) do
    :initialized
  end

  defp merge_receive_marker(_, _) do
    :undecided
  end

  defp merge_stk(s, s) do
    s
  end

  defp merge_stk(_, _) do
    :undecided
  end

  defp merge_ct(s, s) do
    s
  end

  defp merge_ct(ct0, ct1) do
    merge_ct_1(ct0, ct1)
  end

  defp merge_ct_1([], []) do
    []
  end

  defp merge_ct_1(
         [{:trytag, lblsA} | ctA],
         [{:trytag, lblsB} | ctB]
       ) do
    [
      {:trytag, :ordsets.union(lblsA, lblsB)}
      | merge_ct_1(ctA, ctB)
    ]
  end

  defp merge_ct_1(
         [{:catchtag, lblsA} | ctA],
         [{:catchtag, lblsB} | ctB]
       ) do
    [
      {:catchtag, :ordsets.union(lblsA, lblsB)}
      | merge_ct_1(ctA, ctB)
    ]
  end

  defp merge_ct_1(_, _) do
    :undecided
  end

  defp verify_y_init(r_vst(current: r_st(numy: numY, ys: ys)) = vst)
       when is_integer(numY) do
    highestY =
      :maps.fold(
        fn {:y, y}, _, acc ->
          max(y, acc)
        end,
        -1,
        ys
      )

    true = numY > highestY
    verify_y_init_1(numY - 1, vst)
    :ok
  end

  defp verify_y_init(
         r_vst(
           current:
             r_st(
               numy: :undecided,
               ys: ys
             )
         ) = vst
       ) do
    highestY =
      :maps.fold(
        fn {:y, y}, _, acc ->
          max(y, acc)
        end,
        -1,
        ys
      )

    verify_y_init_1(highestY, vst)
  end

  defp verify_y_init(r_vst()) do
    :ok
  end

  defp verify_y_init_1(-1, _Vst) do
    :ok
  end

  defp verify_y_init_1(y, vst) do
    reg = {:y, y}
    assert_not_fragile(reg, vst)

    case get_raw_type(reg, vst) do
      :uninitialized ->
        :erlang.error({:uninitialized_reg, reg})

      _ ->
        verify_y_init_1(y - 1, vst)
    end
  end

  defp verify_live(0, _Vst) do
    :ok
  end

  defp verify_live(live, vst)
       when is_integer(live) and
              0 < live and live <= 1023 do
    verify_live_1(live - 1, vst)
  end

  defp verify_live(live, _Vst) do
    :erlang.error({:bad_number_of_live_regs, live})
  end

  defp verify_live_1(-1, _) do
    :ok
  end

  defp verify_live_1(x, vst) when is_integer(x) do
    reg = {:x, x}

    case get_raw_type(reg, vst) do
      :uninitialized ->
        :erlang.error({reg, :not_live})

      _ ->
        verify_live_1(x - 1, vst)
    end
  end

  defp verify_no_ct(r_vst(current: r_st(numy: :none))) do
    :ok
  end

  defp verify_no_ct(r_vst(current: r_st(numy: :undecided))) do
    :erlang.error(:unknown_size_of_stackframe)
  end

  defp verify_no_ct(r_vst(current: st) = vst) do
    case collect_try_catch_tags(r_st(st, :numy) - 1, vst, []) do
      [_ | _] = bad ->
        :erlang.error({:unfinished_catch_try, bad})

      [] ->
        :ok
    end
  end

  defp collect_try_catch_tags(-1, _Vst, acc) do
    acc
  end

  defp collect_try_catch_tags(y, vst, acc0) do
    tag = get_raw_type({:y, y}, vst)

    acc =
      case is_try_catch_tag(tag) do
        true ->
          [{{:y, y}, tag} | acc0]

        false ->
          acc0
      end

    collect_try_catch_tags(y - 1, vst, acc)
  end

  defp is_try_catch_tag({:catchtag, _}) do
    true
  end

  defp is_try_catch_tag({:trytag, _}) do
    true
  end

  defp is_try_catch_tag(_) do
    false
  end

  defp eat_heap(n, r_vst(current: r_st(h: heap0) = st) = vst) do
    case heap0 - n do
      neg when neg < 0 ->
        :erlang.error({:heap_overflow, {:left, heap0}, {:wanted, n}})

      heap ->
        r_vst(vst, current: r_st(st, h: heap))
    end
  end

  defp eat_heap_fun(r_vst(current: r_st(hl: heapFuns0) = st) = vst) do
    case heapFuns0 - 1 do
      neg when neg < 0 ->
        :erlang.error({:heap_overflow, {:left, {heapFuns0, :funs}}, {:wanted, {1, :funs}}})

      heapFuns ->
        r_vst(vst, current: r_st(st, hl: heapFuns))
    end
  end

  defp eat_heap_float(r_vst(current: r_st(hf: heapFloats0) = st) = vst) do
    case heapFloats0 - 1 do
      neg when neg < 0 ->
        :erlang.error({:heap_overflow, {:left, {heapFloats0, :floats}}, {:wanted, {1, :floats}}})

      heapFloats ->
        r_vst(vst, current: r_st(st, hf: heapFloats))
    end
  end

  defp set_receive_marker(
         new,
         r_vst(current: r_st(recv_marker: current) = st0) = vst
       ) do
    case {current, new} do
      {:none, :initialized} ->
        :ok

      {:initialized, :committed} ->
        :ok

      {:none, :committed} ->
        :ok

      {_, :none} ->
        :ok

      {_, _} ->
        :erlang.error({:invalid_receive_marker_change, current, new})
    end

    st = r_st(st0, recv_marker: new)
    r_vst(vst, current: st)
  end

  defp mark_fragile(reg, vst) do
    r_vst(current: r_st(fragile: fragile0) = st0) = vst
    fragile = :cerl_sets.add_element(reg, fragile0)
    st = r_st(st0, fragile: fragile)
    r_vst(vst, current: st)
  end

  defp propagate_fragility(reg, args, r_vst(current: st0) = vst) do
    r_st(fragile: fragile0) = st0
    sources = :cerl_sets.from_list(args)

    fragile =
      case :cerl_sets.is_disjoint(
             sources,
             fragile0
           ) do
        true ->
          :cerl_sets.del_element(reg, fragile0)

        false ->
          :cerl_sets.add_element(reg, fragile0)
      end

    st = r_st(st0, fragile: fragile)
    r_vst(vst, current: st)
  end

  defp remove_fragility(reg, vst) do
    r_vst(current: r_st(fragile: fragile0) = st0) = vst

    case :cerl_sets.is_element(reg, fragile0) do
      true ->
        fragile = :cerl_sets.del_element(reg, fragile0)
        st = r_st(st0, fragile: fragile)
        r_vst(vst, current: st)

      false ->
        vst
    end
  end

  defp remove_fragility(r_vst(current: st0) = vst) do
    st = r_st(st0, fragile: :cerl_sets.new())
    r_vst(vst, current: st)
  end

  defp assert_durable_term(src, vst) do
    assert_term(src, vst)
    assert_not_fragile(src, vst)
  end

  defp assert_not_fragile({kind, _} = src, vst)
       when kind === :x or
              kind === :y do
    check_limit(src)
    r_vst(current: r_st(fragile: fragile)) = vst

    case :cerl_sets.is_element(src, fragile) do
      true ->
        :erlang.error({:fragile_message_reference, src})

      false ->
        :ok
    end
  end

  defp assert_not_fragile(lit, r_vst()) do
    assert_literal(lit)
    :ok
  end

  defp bif_types(op, ss, vst) do
    args =
      for arg <- ss do
        normalize(get_term_type(arg, vst))
      end

    :beam_call_types.types(:erlang, op, args)
  end

  defp call_types({:extfunc, m, f, a}, a, vst) do
    args = get_call_args(a, vst)
    :beam_call_types.types(m, f, args)
  end

  defp call_types(_, a, vst) do
    {:any, get_call_args(a, vst), false}
  end

  defp will_bif_succeed(:raise, [_, _], _Vst) do
    :no
  end

  defp will_bif_succeed(op, ss, vst) do
    case is_float_arith_bif(op, ss) do
      true ->
        :yes

      false ->
        args =
          for arg <- ss do
            normalize(get_term_type(arg, vst))
          end

        :beam_call_types.will_succeed(:erlang, op, args)
    end
  end

  defp will_call_succeed({:extfunc, m, f, a}, vst) do
    :beam_call_types.will_succeed(m, f, get_call_args(a, vst))
  end

  defp will_call_succeed(:bs_init_writable, _Vst) do
    :yes
  end

  defp will_call_succeed(:raw_raise, _Vst) do
    :no
  end

  defp will_call_succeed(_Call, _Vst) do
    :maybe
  end

  defp get_call_args(arity, vst) do
    get_call_args_1(0, arity, vst)
  end

  defp get_call_args_1(arity, arity, _) do
    []
  end

  defp get_call_args_1(n, arity, vst) when n < arity do
    argType = normalize(get_movable_term_type({:x, n}, vst))
    [argType | get_call_args_1(n + 1, arity, vst)]
  end

  defp check_limit({:x, x} = src) when is_integer(x) do
    cond do
      0 <= x and x < 1023 ->
        :ok

      1023 <= x ->
        :erlang.error(:limit)

      x < 0 ->
        :erlang.error({:bad_register, src})
    end
  end

  defp check_limit({:y, y} = src) when is_integer(y) do
    cond do
      0 <= y and y < 1024 ->
        :ok

      1024 <= y ->
        :erlang.error(:limit)

      y < 0 ->
        :erlang.error({:bad_register, src})
    end
  end

  defp check_limit({:fr, fr} = src) when is_integer(fr) do
    cond do
      0 <= fr and fr < 1023 ->
        :ok

      1023 <= fr ->
        :erlang.error(:limit)

      fr < 0 ->
        :erlang.error({:bad_register, src})
    end
  end

  defp min(a, b)
       when is_integer(a) and is_integer(b) and
              a < b do
    a
  end

  defp min(a, b) when is_integer(a) and is_integer(b) do
    b
  end

  defp gcd(a, b) do
    case rem(a, b) do
      0 ->
        b

      x ->
        gcd(b, x)
    end
  end

  defp error(error) do
    throw(error)
  end
end
