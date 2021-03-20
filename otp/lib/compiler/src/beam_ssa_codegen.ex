defmodule :m_beam_ssa_codegen do
  use Bitwise

  import :lists,
    only: [
      foldl: 3,
      keymember: 3,
      keysort: 2,
      map: 2,
      mapfoldl: 3,
      member: 2,
      reverse: 1,
      reverse: 2,
      sort: 1,
      splitwith: 2,
      takewhile: 2
    ]

  require Record

  Record.defrecord(:r_b_module, :b_module,
    anno: %{},
    name: :undefined,
    exports: :undefined,
    attributes: :undefined,
    body: :undefined
  )

  Record.defrecord(:r_b_function, :b_function,
    anno: %{},
    args: :undefined,
    bs: :undefined,
    cnt: :undefined
  )

  Record.defrecord(:r_b_blk, :b_blk, anno: %{}, is: :undefined, last: :undefined)
  Record.defrecord(:r_b_set, :b_set, anno: %{}, dst: :none, op: :undefined, args: [])
  Record.defrecord(:r_b_ret, :b_ret, anno: %{}, arg: :undefined)

  Record.defrecord(:r_b_br, :b_br, anno: %{}, bool: :undefined, succ: :undefined, fail: :undefined)

  Record.defrecord(:r_b_switch, :b_switch,
    anno: %{},
    arg: :undefined,
    fail: :undefined,
    list: :undefined
  )

  Record.defrecord(:r_b_var, :b_var, name: :undefined)
  Record.defrecord(:r_b_literal, :b_literal, val: :undefined)
  Record.defrecord(:r_b_remote, :b_remote, mod: :undefined, name: :undefined, arity: :undefined)

  Record.defrecord(:r_b_local, :b_local,
    name: :undefined,
    arity: :undefined
  )

  Record.defrecord(:r_cg, :cg,
    lcount: 1,
    functable: %{},
    labels: %{},
    used_labels: :gb_sets.empty(),
    regs: %{},
    ultimate_fail: 1,
    catches: :gb_sets.empty(),
    fc_label: 1
  )

  def module(
        r_b_module(name: mod, exports: es, attributes: attrs, body: fs),
        _Opts
      ) do
    {asm, st} = functions(fs, {:atom, mod})
    {:ok, {mod, es, attrs, asm, r_cg(st, :lcount)}}
  end

  Record.defrecord(:r_need, :need, h: 0, l: 0, f: 0)
  Record.defrecord(:r_cg_blk, :cg_blk, anno: %{}, is: [], last: :undefined)

  Record.defrecord(:r_cg_set, :cg_set,
    anno: %{},
    dst: :undefined,
    op: :undefined,
    args: :undefined
  )

  Record.defrecord(:r_cg_alloc, :cg_alloc,
    anno: %{},
    stack: :none,
    words: :EFE_TODO_NESTED_RECORD,
    live: :undefined,
    def_yregs: []
  )

  Record.defrecord(:r_cg_br, :cg_br, bool: :undefined, succ: :undefined, fail: :undefined)

  Record.defrecord(:r_cg_ret, :cg_ret,
    arg: :undefined,
    dealloc: :none
  )

  Record.defrecord(:r_cg_switch, :cg_switch, arg: :undefined, fail: :undefined, list: :undefined)

  defp functions(forms, atomMod) do
    mapfoldl(
      fn f, st ->
        function(f, atomMod, st)
      end,
      r_cg(lcount: 1),
      forms
    )
  end

  defp function(r_b_function(anno: anno, bs: blocks), atomMod, st0) do
    %{func_info: {_, name, arity}} = anno

    try do
      assert_exception_block(blocks)
      regs = :maps.get(:registers, anno)
      st1 = r_cg(st0, labels: %{}, used_labels: :gb_sets.empty(), regs: regs)
      {fi, st2} = new_label(st1)
      {entry, st3} = local_func_label(name, arity, st2)
      {ult, st4} = new_label(st3)
      labels = Map.merge(r_cg(st4, :labels), %{0 => entry, 1 => 0})

      st5 =
        r_cg(st4,
          labels: labels,
          used_labels: :gb_sets.singleton(entry),
          ultimate_fail: ult
        )

      {body, st} = cg_fun(blocks, r_cg(st5, fc_label: fi))

      asm =
        [{:label, fi}, line(anno), {:func_info, atomMod, {:atom, name}, arity}] ++
          add_parameter_annos(
            body,
            anno
          ) ++ [{:label, ult}, :if_end]

      func = {:function, name, arity, entry, asm}
      {func, st}
    catch
      class, error ->
        :io.fwrite('Function: ~w/~w\n', [name, arity])
        :erlang.raise(class, error, __STACKTRACE__)
    end
  end

  defp assert_exception_block(blocks) do
    case blocks do
      %{1 => blk} ->
        r_b_blk(
          is: [
            r_b_set(
              op: :call,
              dst: ret,
              args: [
                r_b_remote(mod: r_b_literal(val: :erlang), name: r_b_literal(val: :error)),
                r_b_literal(val: :badarg)
              ]
            )
          ],
          last: r_b_ret(arg: ret)
        ) = blk

        :ok

      %{} ->
        :ok
    end
  end

  defp add_parameter_annos([{:label, _} = entry | body], anno) do
    paramTypes = :maps.get(:parameter_info, anno, %{})

    annos =
      :maps.fold(
        fn
          k, v, acc
          when :erlang.is_map_key(k, paramTypes) ->
            info = :erlang.map_get(k, paramTypes)
            [{:%, {:var_info, v, info}} | acc]

          _K, _V, acc ->
            acc
        end,
        [],
        :maps.get(:registers, anno)
      )

    [entry | sort(annos)] ++ body
  end

  defp cg_fun(blocks, st0) do
    linear0 = linearize(blocks)
    st = collect_catch_labels(linear0, st0)
    linear1 = need_heap(linear0)
    linear2 = prefer_xregs(linear1, st)
    linear3 = liveness(linear2, st)
    linear4 = defined(linear3, st)
    linear5 = opt_allocate(linear4, st)
    linear = fix_wait_timeout(linear5)
    cg_linear(linear, st)
  end

  defp collect_catch_labels(linear, st) do
    labels = collect_catch_labels_1(linear)
    r_cg(st, catches: :gb_sets.from_list(labels))
  end

  defp collect_catch_labels_1([{l, r_cg_blk(is: [r_cg_set(op: :landingpad) | _])} | bs]) do
    [l | collect_catch_labels_1(bs)]
  end

  defp collect_catch_labels_1([_ | bs]) do
    collect_catch_labels_1(bs)
  end

  defp collect_catch_labels_1([]) do
    []
  end

  defp need_heap(bs0) do
    bs1 = need_heap_allocs(bs0, %{})
    {bs, r_need(h: 0, l: 0, f: 0)} = need_heap_blks(reverse(bs1), r_need(), [])
    bs
  end

  defp need_heap_allocs(
         [{l, r_cg_blk(is: is0, last: terminator) = blk0} | bs],
         counts0
       ) do
    next = next_block(bs)
    successors = successors(terminator)

    counts =
      foldl(
        fn s, cnts ->
          case cnts do
            %{^s => c} ->
              %{cnts | s => c + 1}

            %{} when s === next ->
              Map.put(cnts, s, 1)

            %{} ->
              Map.put(cnts, s, 42)
          end
        end,
        counts0,
        successors
      )

    case counts do
      %{^l => 1} ->
        [{l, blk0} | need_heap_allocs(bs, counts)]

      %{^l => _} ->
        is =
          case need_heap_never(is0) do
            true ->
              is0

            false ->
              [r_cg_alloc() | is0]
          end

        blk = r_cg_blk(blk0, is: is)
        [{l, blk} | need_heap_allocs(bs, counts)]

      %{} ->
        [{l, blk0} | need_heap_allocs(bs, counts)]
    end
  end

  defp need_heap_allocs([], _) do
    []
  end

  defp need_heap_never([r_cg_alloc() | _]) do
    true
  end

  defp need_heap_never([r_cg_set(op: :recv_next) | _]) do
    true
  end

  defp need_heap_never([r_cg_set(op: :wait) | _]) do
    true
  end

  defp need_heap_never(_) do
    false
  end

  defp need_heap_blks([{l, r_cg_blk(is: is0) = blk0} | bs], h0, acc) do
    {is1, h1} = need_heap_is(reverse(is0), h0, [])
    {ns, h} = need_heap_terminator(bs, l, h1)
    is = ns ++ is1
    blk = r_cg_blk(blk0, is: is)
    need_heap_blks(bs, h, [{l, blk} | acc])
  end

  defp need_heap_blks([], h, acc) do
    {acc, h}
  end

  defp need_heap_is([r_cg_alloc(words: words) = alloc0 | is], n, acc) do
    alloc = r_cg_alloc(alloc0, words: add_heap_words(n, words))
    need_heap_is(is, r_need(), [alloc | acc])
  end

  defp need_heap_is([r_cg_set(anno: anno, op: :bs_init) = i0 | is], n, acc) do
    alloc =
      case need_heap_need(n) do
        [r_cg_alloc(words: need)] ->
          alloc(need)

        [] ->
          0
      end

    i = r_cg_set(i0, anno: Map.put(anno, :alloc, alloc))
    need_heap_is(is, r_need(), [i | acc])
  end

  defp need_heap_is([r_cg_set(op: op, args: args) = i | is], n, acc) do
    case classify_heap_need(op, args) do
      {:put, words} ->
        need_heap_is(is, add_heap_words(n, words), [i | acc])

      {:put_fun, nArgs} ->
        need_heap_is(is, add_heap_fun(n, nArgs), [i | acc])

      :put_float ->
        need_heap_is(is, add_heap_float(n), [i | acc])

      :neutral ->
        need_heap_is(is, n, [i | acc])

      :gc ->
        need_heap_is(is, r_need(), [i] ++ need_heap_need(n) ++ acc)
    end
  end

  defp need_heap_is([], n, acc) do
    {acc, n}
  end

  defp need_heap_terminator([{_, r_cg_blk(last: r_cg_br(succ: l, fail: l))} | _], l, n) do
    {[], n}
  end

  defp need_heap_terminator([{_, r_cg_blk(is: is, last: r_cg_br(succ: l))} | _], l, n) do
    case need_heap_need(n) do
      [] ->
        {[], r_need()}

      [_ | _] = alloc ->
        case reverse(is) do
          [r_cg_set(op: :succeeded), r_cg_set(op: :bs_init) | _] ->
            {[], n}

          [r_cg_set(op: :bs_put) | _] ->
            {[], n}

          _ ->
            {alloc, r_need()}
        end
    end
  end

  defp need_heap_terminator([{_, r_cg_blk()} | _], _, n) do
    {need_heap_need(n), r_need()}
  end

  defp need_heap_terminator([], _, h) do
    {need_heap_need(h), r_need()}
  end

  defp need_heap_need(r_need(h: 0, l: 0, f: 0)) do
    []
  end

  defp need_heap_need(r_need() = n) do
    [r_cg_alloc(words: n)]
  end

  defp add_heap_words(
         r_need(h: h1, l: l1, f: f1),
         r_need(h: h2, l: l2, f: f2)
       ) do
    r_need(h: h1 + h2, l: l1 + l2, f: f1 + f2)
  end

  defp add_heap_words(r_need(h: heap) = n, words) when is_integer(words) do
    r_need(n, h: heap + words)
  end

  defp add_heap_fun(r_need(h: heap, l: lambdas) = n, nArgs) do
    r_need(n, h: heap + nArgs, l: lambdas + 1)
  end

  defp add_heap_float(r_need(f: f) = n) do
    r_need(n, f: f + 1)
  end

  def classify_heap_need(:put_list, _) do
    {:put, 2}
  end

  def classify_heap_need(:put_tuple_arity, [r_b_literal(val: words)]) do
    {:put, words + 1}
  end

  def classify_heap_need(:put_tuple, elements) do
    {:put, length(elements) + 1}
  end

  def classify_heap_need(:make_fun, args) do
    {:put_fun, length(args) - 1}
  end

  def classify_heap_need({:bif, name}, args) do
    case is_gc_bif(name, args) do
      false ->
        :neutral

      true ->
        :gc
    end
  end

  def classify_heap_need({:float, op}, _Args) do
    case op do
      :get ->
        :put_float

      _ ->
        :neutral
    end
  end

  def classify_heap_need(name, _Args) do
    classify_heap_need(name)
  end

  defp classify_heap_need(:bs_add) do
    :gc
  end

  defp classify_heap_need(:bs_get) do
    :gc
  end

  defp classify_heap_need(:bs_get_tail) do
    :gc
  end

  defp classify_heap_need(:bs_init) do
    :gc
  end

  defp classify_heap_need(:bs_init_writable) do
    :gc
  end

  defp classify_heap_need(:bs_match_string) do
    :gc
  end

  defp classify_heap_need(:bs_put) do
    :neutral
  end

  defp classify_heap_need(:bs_restore) do
    :neutral
  end

  defp classify_heap_need(:bs_save) do
    :neutral
  end

  defp classify_heap_need(:bs_get_position) do
    :gc
  end

  defp classify_heap_need(:bs_set_position) do
    :neutral
  end

  defp classify_heap_need(:bs_skip) do
    :gc
  end

  defp classify_heap_need(:bs_start_match) do
    :gc
  end

  defp classify_heap_need(:bs_test_tail) do
    :neutral
  end

  defp classify_heap_need(:bs_utf16_size) do
    :neutral
  end

  defp classify_heap_need(:bs_utf8_size) do
    :neutral
  end

  defp classify_heap_need(:build_stacktrace) do
    :gc
  end

  defp classify_heap_need(:call) do
    :gc
  end

  defp classify_heap_need(:catch_end) do
    :gc
  end

  defp classify_heap_need(:copy) do
    :neutral
  end

  defp classify_heap_need(:extract) do
    :gc
  end

  defp classify_heap_need(:get_hd) do
    :neutral
  end

  defp classify_heap_need(:get_map_element) do
    :neutral
  end

  defp classify_heap_need(:get_tl) do
    :neutral
  end

  defp classify_heap_need(:get_tuple_element) do
    :neutral
  end

  defp classify_heap_need(:has_map_field) do
    :neutral
  end

  defp classify_heap_need(:is_nonempty_list) do
    :neutral
  end

  defp classify_heap_need(:is_tagged_tuple) do
    :neutral
  end

  defp classify_heap_need(:kill_try_tag) do
    :gc
  end

  defp classify_heap_need(:landingpad) do
    :gc
  end

  defp classify_heap_need(:match_fail) do
    :gc
  end

  defp classify_heap_need(:nop) do
    :neutral
  end

  defp classify_heap_need(:new_try_tag) do
    :gc
  end

  defp classify_heap_need(:old_make_fun) do
    :gc
  end

  defp classify_heap_need(:peek_message) do
    :gc
  end

  defp classify_heap_need(:put_map) do
    :gc
  end

  defp classify_heap_need(:put_tuple_elements) do
    :neutral
  end

  defp classify_heap_need(:raw_raise) do
    :gc
  end

  defp classify_heap_need(:recv_next) do
    :gc
  end

  defp classify_heap_need(:remove_message) do
    :neutral
  end

  defp classify_heap_need(:resume) do
    :gc
  end

  defp classify_heap_need(:set_tuple_element) do
    :gc
  end

  defp classify_heap_need(:succeeded) do
    :neutral
  end

  defp classify_heap_need(:timeout) do
    :gc
  end

  defp classify_heap_need(:wait) do
    :gc
  end

  defp classify_heap_need(:wait_timeout) do
    :gc
  end

  defp prefer_xregs(linear, st) do
    prefer_xregs(linear, st, %{0 => %{}})
  end

  defp prefer_xregs([{l, r_cg_blk(is: is0, last: last0) = blk0} | bs], st, map0) do
    copies0 = :maps.get(l, map0)
    {is, copies} = prefer_xregs_is(is0, st, copies0, [])
    last = prefer_xregs_terminator(last0, copies, st)
    blk = r_cg_blk(blk0, is: is, last: last)
    successors = successors(last)
    map = prefer_xregs_successors(successors, copies, map0)
    [{l, blk} | prefer_xregs(bs, st, map)]
  end

  defp prefer_xregs([], _St, _Map) do
    []
  end

  defp prefer_xregs_successors([l | ls], copies0, map0) do
    case map0 do
      %{^l => copies1} ->
        copies = merge_copies(copies0, copies1)
        map = %{map0 | l => copies}
        prefer_xregs_successors(ls, copies0, map)

      %{} ->
        map = Map.put(map0, l, copies0)
        prefer_xregs_successors(ls, copies0, map)
    end
  end

  defp prefer_xregs_successors([], _, map) do
    map
  end

  defp prefer_xregs_is([r_cg_alloc() = i | is], st, copies0, acc) do
    copies =
      case i do
        r_cg_alloc(stack: :none, words: r_need(h: 0, l: 0, f: 0)) ->
          copies0

        r_cg_alloc() ->
          %{}
      end

    prefer_xregs_is(is, st, copies, [i | acc])
  end

  defp prefer_xregs_is([r_cg_set(op: :copy, dst: dst, args: [src]) = i | is], st, copies0, acc) do
    copies1 = prefer_xregs_prune(i, copies0, st)

    copies =
      case beam_args([src, dst], st) do
        [same, same] ->
          copies1

        [_, _] ->
          Map.put(copies1, dst, src)
      end

    prefer_xregs_is(is, st, copies, [i | acc])
  end

  defp prefer_xregs_is([r_cg_set(op: :call, dst: dst) = i0 | is], st, copies, acc) do
    i = prefer_xregs_call(i0, copies, st)
    prefer_xregs_is(is, st, %{dst => {:x, 0}}, [i | acc])
  end

  defp prefer_xregs_is([r_cg_set(op: :old_make_fun, dst: dst) = i0 | is], st, copies, acc) do
    i = prefer_xregs_call(i0, copies, st)
    prefer_xregs_is(is, st, %{dst => {:x, 0}}, [i | acc])
  end

  defp prefer_xregs_is([r_cg_set(op: :set_tuple_element) = i | is], st, copies, acc) do
    prefer_xregs_is(is, st, copies, [i | acc])
  end

  defp prefer_xregs_is([r_cg_set(args: args0) = i0 | is], st, copies0, acc) do
    args =
      for a <- args0 do
        do_prefer_xreg(a, copies0, st)
      end

    i = r_cg_set(i0, args: args)
    copies = prefer_xregs_prune(i, copies0, st)
    prefer_xregs_is(is, st, copies, [i | acc])
  end

  defp prefer_xregs_is([], _St, copies, acc) do
    {reverse(acc), copies}
  end

  defp prefer_xregs_terminator(r_cg_br(bool: arg0) = i, copies, st) do
    arg = do_prefer_xreg(arg0, copies, st)
    r_cg_br(i, bool: arg)
  end

  defp prefer_xregs_terminator(r_cg_ret(arg: arg0) = i, copies, st) do
    arg = do_prefer_xreg(arg0, copies, st)
    r_cg_ret(i, arg: arg)
  end

  defp prefer_xregs_terminator(r_cg_switch(arg: arg0) = i, copies, st) do
    arg = do_prefer_xreg(arg0, copies, st)
    r_cg_switch(i, arg: arg)
  end

  defp prefer_xregs_prune(r_cg_set(anno: %{clobbers: true}), _, _) do
    %{}
  end

  defp prefer_xregs_prune(r_cg_set(dst: dst), copies, st) do
    dstReg = beam_arg(dst, st)

    f = fn _, alias ->
      beam_arg(alias, st) !== dstReg
    end

    :maps.filter(f, copies)
  end

  defp prefer_xregs_call(r_cg_set(args: [f0 | args0]) = i, copies, st) do
    f =
      case f0 do
        r_b_var() ->
          do_prefer_xreg(f0, copies, st)

        r_b_remote(mod: mod, name: name) ->
          r_b_remote(f0,
            mod: do_prefer_xreg(mod, copies, st),
            name: do_prefer_xreg(name, copies, st)
          )

        _ ->
          f0
      end

    args =
      for a <- args0 do
        do_prefer_xreg(a, copies, st)
      end

    r_cg_set(i, args: [f | args])
  end

  defp do_prefer_xreg(r_b_var() = a, copies, st) do
    case {beam_arg(a, st), copies} do
      {{:y, _}, %{^a => copy}} ->
        copy

      {_, _} ->
        a
    end
  end

  defp do_prefer_xreg(a, _, _) do
    a
  end

  defp merge_copies(copies0, copies1)
       when map_size(copies0) <= map_size(copies1) do
    :maps.filter(
      fn k, v ->
        case copies1 do
          %{^k => ^v} ->
            true

          %{} ->
            false
        end
      end,
      copies0
    )
  end

  defp merge_copies(copies0, copies1) do
    merge_copies(copies1, copies0)
  end

  defp liveness(linear, r_cg(regs: regs)) do
    liveness(reverse(linear), %{}, regs, [])
  end

  defp liveness([{l, r_cg_blk(is: is0, last: last0) = blk0} | bs], liveMap0, regs, acc) do
    successors = liveness_successors(last0)

    live0 =
      :ordsets.union(
        for s <- successors do
          liveness_get(s, liveMap0)
        end
      )

    live1 = liveness_terminator(last0, live0)
    {is, live} = liveness_is(reverse(is0), regs, live1, [])
    liveMap = Map.put(liveMap0, l, live)
    blk = r_cg_blk(blk0, is: is)
    liveness(bs, liveMap, regs, [{l, blk} | acc])
  end

  defp liveness([], _LiveMap, _Regs, acc) do
    acc
  end

  defp liveness_get(s, liveMap) do
    case liveMap do
      %{^s => live} ->
        live

      %{} ->
        []
    end
  end

  defp liveness_successors(terminator) do
    successors(terminator) -- [1]
  end

  defp liveness_is([r_cg_alloc() = i0 | is], regs, live, acc) do
    i = r_cg_alloc(i0, live: num_live(live, regs))
    liveness_is(is, regs, live, [i | acc])
  end

  defp liveness_is([r_cg_set(dst: dst, args: args) = i0 | is], regs, live0, acc) do
    live1 = liveness_clobber(i0, live0, regs)
    i1 = liveness_yregs_anno(i0, live1, regs)
    live2 = liveness_args(args, live1)
    live = :ordsets.del_element(dst, live2)
    i = liveness_anno(i1, live, regs)
    liveness_is(is, regs, live, [i | acc])
  end

  defp liveness_is([], _, live, acc) do
    {acc, live}
  end

  defp liveness_terminator(r_cg_br(bool: arg), live) do
    liveness_terminator_1(arg, live)
  end

  defp liveness_terminator(r_cg_switch(arg: arg), live) do
    liveness_terminator_1(arg, live)
  end

  defp liveness_terminator(r_cg_ret(arg: arg), live) do
    liveness_terminator_1(arg, live)
  end

  defp liveness_terminator_1(r_b_var() = v, live) do
    :ordsets.add_element(v, live)
  end

  defp liveness_terminator_1(r_b_literal(), live) do
    live
  end

  defp liveness_terminator_1(reg, live) do
    _ = verify_beam_register(reg)
    :ordsets.add_element(reg, live)
  end

  defp liveness_args([r_b_var() = v | as], live) do
    liveness_args(as, :ordsets.add_element(v, live))
  end

  defp liveness_args([r_b_remote(mod: mod, name: name) | as], live) do
    liveness_args([mod, name | as], live)
  end

  defp liveness_args([a | as], live) do
    case is_beam_register(a) do
      true ->
        liveness_args(as, :ordsets.add_element(a, live))

      false ->
        liveness_args(as, live)
    end
  end

  defp liveness_args([], live) do
    live
  end

  defp liveness_anno(r_cg_set(op: op) = i, live, regs) do
    case need_live_anno(op) do
      true ->
        numLive = num_live(live, regs)
        anno = Map.put(r_cg_set(i, :anno), :live, numLive)
        r_cg_set(i, anno: anno)

      false ->
        i
    end
  end

  defp liveness_yregs_anno(r_cg_set(op: op, dst: dst) = i, live0, regs) do
    case need_live_anno(op) do
      true ->
        live = :ordsets.del_element(dst, live0)

        liveYregs =
          for v <- live, is_yreg(v, regs) do
            v
          end

        anno = Map.put(r_cg_set(i, :anno), :live_yregs, liveYregs)
        r_cg_set(i, anno: anno)

      false ->
        i
    end
  end

  defp liveness_clobber(r_cg_set(anno: anno), live, regs) do
    case anno do
      %{clobbers: true} ->
        for r <- live, is_yreg(r, regs) do
          r
        end

      _ ->
        live
    end
  end

  defp is_yreg(r, regs) do
    case regs do
      %{^r => {:y, _}} ->
        true

      %{} ->
        false
    end
  end

  defp num_live(live, regs) do
    rs =
      :ordsets.from_list(
        for v <- live do
          get_register(v, regs)
        end
      )

    num_live_1(rs, 0)
  end

  defp num_live_1([{:x, x} | t], x) do
    num_live_1(t, x + 1)
  end

  defp num_live_1([{:x, _} | _] = t, x) do
    num_live_1(t, x + 1)
  end

  defp num_live_1([{:y, _} | _], x) do
    x
  end

  defp num_live_1([{:z, _} | _], x) do
    x
  end

  defp num_live_1([{:fr, _} | t], x) do
    num_live_1(t, x)
  end

  defp num_live_1([], x) do
    x
  end

  defp get_live(r_cg_set(anno: %{live: live})) do
    live
  end

  defp need_live_anno(op) do
    case op do
      {:bif, _} ->
        true

      :bs_get ->
        true

      :bs_init ->
        true

      :bs_get_position ->
        true

      :bs_get_tail ->
        true

      :bs_start_match ->
        true

      :bs_skip ->
        true

      :call ->
        true

      :put_map ->
        true

      _ ->
        false
    end
  end

  defp defined(linear, r_cg(regs: regs)) do
    __MODULE__.def(linear, %{}, regs)
  end

  defp def([{l, r_cg_blk(is: is0, last: last) = blk0} | bs], defMap0, regs) do
    def0 = def_get(l, defMap0)
    {is, def__, maybeDef} = def_is(is0, regs, def0, [])
    defMap = def_successors(last, def__, maybeDef, defMap0)
    blk = r_cg_blk(blk0, is: is)
    [{l, blk} | __MODULE__.def(bs, defMap, regs)]
  end

  defp def([], _, _) do
    []
  end

  defp def_get(l, defMap) do
    case defMap do
      %{^l => def__} ->
        def__

      %{} ->
        []
    end
  end

  defp def_is([r_cg_alloc(anno: anno0) = i0 | is], regs, def__, acc) do
    i = r_cg_alloc(i0, anno: Map.put(anno0, :def_yregs, def__))
    def_is(is, regs, def__, [i | acc])
  end

  defp def_is([r_cg_set(op: :succeeded, args: [var]) = i], regs, def__, acc) do
    maybeDef = def_add_yreg(var, [], regs)
    {reverse(acc, [i]), def__, maybeDef}
  end

  defp def_is(
         [
           r_cg_set(op: :kill_try_tag, args: [r_b_var() = tag]) = i
           | is
         ],
         regs,
         def0,
         acc
       ) do
    def__ = :ordsets.del_element(tag, def0)
    def_is(is, regs, def__, [i | acc])
  end

  defp def_is(
         [
           r_cg_set(op: :catch_end, args: [r_b_var() = tag | _]) = i
           | is
         ],
         regs,
         def0,
         acc
       ) do
    def__ = :ordsets.del_element(tag, def0)
    def_is(is, regs, def__, [i | acc])
  end

  defp def_is([r_cg_set(anno: anno0, op: :call, dst: dst) = i0 | is], regs, def0, acc) do
    %{live_yregs: liveYregVars} = anno0

    liveRegs =
      :gb_sets.from_list(
        for v <- liveYregVars do
          :maps.get(v, regs)
        end
      )

    kill0 = :ordsets.subtract(def0, liveYregVars)

    kill =
      for k <- kill0,
          not :gb_sets.is_element(:maps.get(k, regs), liveRegs) do
        k
      end

    anno = Map.merge(anno0, %{def_yregs: def0, kill_yregs: kill})
    i = r_cg_set(i0, anno: anno)
    def1 = :ordsets.subtract(def0, kill)
    def__ = def_add_yreg(dst, def1, regs)
    def_is(is, regs, def__, [i | acc])
  end

  defp def_is(
         [
           r_cg_set(anno: anno0, op: {:bif, bif}, dst: dst, args: args) = i0
           | is
         ],
         regs,
         def0,
         acc
       ) do
    arity = length(args)

    i =
      case is_gc_bif(bif, args) or not :erl_bifs.is_safe(:erlang, bif, arity) do
        true ->
          r_cg_set(i0, anno: Map.put(anno0, :def_yregs, def0))

        false ->
          i0
      end

    def__ = def_add_yreg(dst, def0, regs)
    def_is(is, regs, def__, [i | acc])
  end

  defp def_is([r_cg_set(anno: anno0, dst: dst) = i0 | is], regs, def0, acc) do
    i =
      case need_y_init(i0) do
        true ->
          r_cg_set(i0, anno: Map.put(anno0, :def_yregs, def0))

        false ->
          i0
      end

    def__ = def_add_yreg(dst, def0, regs)
    def_is(is, regs, def__, [i | acc])
  end

  defp def_is([], _, def__, acc) do
    {reverse(acc), def__, []}
  end

  defp def_add_yreg(dst, def__, regs) do
    case is_yreg(dst, regs) do
      true ->
        :ordsets.add_element(dst, def__)

      false ->
        def__
    end
  end

  defp def_successors(r_cg_br(bool: r_b_var(), succ: succ, fail: fail), def__, maybeDef, defMap0) do
    defMap = def_successors([fail], :ordsets.subtract(def__, maybeDef), defMap0)
    def_successors([succ], def__, defMap)
  end

  defp def_successors(last, def__, [], defMap) do
    def_successors(successors(last), def__, defMap)
  end

  defp def_successors([s | ss], def0, defMap) do
    case defMap do
      %{^s => def1} ->
        def__ = :ordsets.intersection(def0, def1)
        def_successors(ss, def0, %{defMap | s => def__})

      %{} ->
        def_successors(ss, def0, Map.put(defMap, s, def0))
    end
  end

  defp def_successors([], _, defMap) do
    defMap
  end

  defp need_y_init(r_cg_set(anno: %{clobbers: clobbers})) do
    clobbers
  end

  defp need_y_init(r_cg_set(op: :bs_get)) do
    true
  end

  defp need_y_init(r_cg_set(op: :bs_get_position)) do
    true
  end

  defp need_y_init(r_cg_set(op: :bs_get_tail)) do
    true
  end

  defp need_y_init(r_cg_set(op: :bs_init)) do
    true
  end

  defp need_y_init(r_cg_set(op: :bs_skip, args: [r_b_literal(val: type) | _])) do
    case type do
      :utf8 ->
        true

      :utf16 ->
        true

      :utf32 ->
        true

      _ ->
        false
    end
  end

  defp need_y_init(r_cg_set(op: :bs_start_match)) do
    true
  end

  defp need_y_init(r_cg_set(op: :put_map)) do
    true
  end

  defp need_y_init(r_cg_set()) do
    false
  end

  defp opt_allocate(linear, r_cg(regs: regs)) do
    opt_allocate_1(linear, regs)
  end

  defp opt_allocate_1(
         [
           {l, r_cg_blk(is: [r_cg_alloc(stack: stk) = i0 | is]) = blk0}
           | bs
         ] = bs0,
         regs
       )
       when is_integer(stk) do
    case :ordsets.from_list(
           opt_allocate_defs(
             is,
             regs
           )
         ) do
      yregs when length(yregs) === stk ->
        i = r_cg_alloc(i0, def_yregs: yregs)
        [{l, r_cg_blk(blk0, is: [i | is])} | opt_allocate_1(bs, regs)]

      yregs0 ->
        yregs1 = opt_alloc_def(bs0, :gb_sets.singleton(l), [])
        yregs = :ordsets.union(yregs0, yregs1)
        i = r_cg_alloc(i0, def_yregs: yregs)
        [{l, r_cg_blk(blk0, is: [i | is])} | opt_allocate_1(bs, regs)]
    end
  end

  defp opt_allocate_1([b | bs], regs) do
    [b | opt_allocate_1(bs, regs)]
  end

  defp opt_allocate_1([], _) do
    []
  end

  defp opt_allocate_defs([r_cg_set(op: :copy, dst: dst) | is], regs) do
    case is_yreg(dst, regs) do
      true ->
        [dst | opt_allocate_defs(is, regs)]

      false ->
        []
    end
  end

  defp opt_allocate_defs(_, _Regs) do
    []
  end

  defp opt_alloc_def([{l, r_cg_blk(is: is, last: last)} | bs], ws0, def0) do
    case :gb_sets.is_member(l, ws0) do
      false ->
        opt_alloc_def(bs, ws0, def0)

      true ->
        case opt_allocate_is(is) do
          :none ->
            succ = successors(last)
            ws = :gb_sets.union(ws0, :gb_sets.from_list(succ))
            opt_alloc_def(bs, ws, def0)

          def1 when is_list(def1) ->
            def__ = [def1 | def0]
            opt_alloc_def(bs, ws0, def__)
        end
    end
  end

  defp opt_alloc_def([], _, def__) do
    :ordsets.intersection(def__)
  end

  defp opt_allocate_is([r_cg_set(anno: anno) | is]) do
    case anno do
      %{def_yregs: yregs} ->
        yregs

      %{} ->
        opt_allocate_is(is)
    end
  end

  defp opt_allocate_is([
         r_cg_alloc(anno: %{def_yregs: yregs}, stack: :none)
         | _
       ]) do
    yregs
  end

  defp opt_allocate_is([r_cg_alloc() | is]) do
    opt_allocate_is(is)
  end

  defp opt_allocate_is([]) do
    :none
  end

  defp fix_wait_timeout([
         {l1, r_cg_blk(is: is0, last: r_cg_br(bool: r_b_var(), succ: l2)) = blk1},
         {l2, r_cg_blk(is: [], last: r_cg_br(bool: r_b_var()) = br) = blk2}
         | bs
       ]) do
    case fix_wait_timeout_is(is0, []) do
      :no ->
        [{l1, blk1}, {l2, blk2} | fix_wait_timeout(bs)]

      {:yes, is} ->
        [
          {l1, r_cg_blk(blk1, is: is, last: br)}
          | fix_wait_timeout(bs)
        ]
    end
  end

  defp fix_wait_timeout([b | bs]) do
    [b | fix_wait_timeout(bs)]
  end

  defp fix_wait_timeout([]) do
    []
  end

  defp fix_wait_timeout_is(
         [
           r_cg_set(op: :wait_timeout, dst: waitBool) = wT,
           r_cg_set(op: :succeeded, args: [waitBool])
         ],
         acc
       ) do
    {:yes, reverse(acc, [wT])}
  end

  defp fix_wait_timeout_is([i | is], acc) do
    fix_wait_timeout_is(is, [i | acc])
  end

  defp fix_wait_timeout_is([], _Acc) do
    :no
  end

  defp cg_linear(
         [
           {l, r_cg_blk(anno: %{recv_set: l} = anno0) = b0}
           | bs
         ],
         st0
       ) do
    anno = :maps.remove(:recv_set, anno0)
    b = r_cg_blk(b0, anno: anno)
    {is, st1} = cg_linear([{l, b} | bs], st0)
    {fail, st} = use_block_label(l, st1)
    {[{:recv_set, fail} | is], st}
  end

  defp cg_linear([{l, r_cg_blk(is: is0, last: last)} | bs], st0) do
    next = next_block(bs)
    st1 = new_block_label(l, st0)
    {is1, st2} = cg_block(is0, last, next, st1)
    {is2, st} = cg_linear(bs, st2)
    {def_block_label(l, st) ++ is1 ++ is2, st}
  end

  defp cg_linear([], st) do
    {[], st}
  end

  defp cg_block([r_cg_set(op: :recv_next)], r_cg_br(succ: lr0), _Next, st0) do
    {lr, st} = use_block_label(lr0, st0)
    {[{:loop_rec_end, lr}], st}
  end

  defp cg_block([r_cg_set(op: :wait)], r_cg_br(succ: lr0), _Next, st0) do
    {lr, st} = use_block_label(lr0, st0)
    {[{:wait, lr}], st}
  end

  defp cg_block(is0, last, next, st0) do
    case last do
      r_cg_br(succ: ^next, fail: ^next) ->
        cg_block(is0, :none, st0)

      r_cg_br(succ: same, fail: same) when same === 1 ->
        {is, st} = cg_block(is0, :none, st0)
        {is ++ [:if_end], st}

      r_cg_br(succ: same, fail: same) ->
        {fail, st1} = use_block_label(same, st0)
        {is, st} = cg_block(is0, :none, st1)
        {is ++ [jump(fail)], st}

      r_cg_br(bool: bool, succ: ^next, fail: fail0) ->
        {fail, st1} = use_block_label(fail0, st0)
        {is, st} = cg_block(is0, {bool, fail}, st1)
        {is, st}

      r_cg_br(bool: bool, succ: succ0, fail: fail0) ->
        {[succ, fail], st1} =
          use_block_labels(
            [succ0, fail0],
            st0
          )

        {is, st} = cg_block(is0, {bool, fail}, st1)
        {is ++ [jump(succ)], st}

      r_cg_ret(arg: src0, dealloc: n) ->
        src = beam_arg(src0, st0)
        cg_block(is0, {:return, src, n}, st0)

      r_cg_switch() ->
        cg_switch(is0, last, st0)
    end
  end

  defp cg_switch(is0, last, st0) do
    r_cg_switch(arg: src0, fail: fail0, list: list0) = last
    src = beam_arg(src0, st0)
    {fail1, st1} = use_block_label(fail0, st0)
    fail = ensure_label(fail1, st1)

    {list1, st2} =
      flatmapfoldl(
        fn {v, l}, s0 ->
          {lbl, s} = use_block_label(l, s0)
          {[beam_arg(v, s), lbl], s}
        end,
        st1,
        list0
      )

    {is1, st} = cg_block(is0, :none, st2)

    case reverse(is1) do
      [
        {:bif, :tuple_size, _, [tuple], {:z, _} = ^src}
        | more
      ] ->
        list =
          map(
            fn
              {:integer, arity} ->
                arity

              {:f, _} = f ->
                f
            end,
            list1
          )

        is =
          reverse(
            more,
            [{:select_tuple_arity, tuple, fail, {:list, list}}]
          )

        {is, st}

      _ ->
        selectVal = {:select_val, src, fail, {:list, list1}}
        {is1 ++ [selectVal], st}
    end
  end

  defp jump({:f, _} = fail) do
    {:jump, fail}
  end

  defp jump({:catch_tag, fail}) do
    {:jump, fail}
  end

  defp bif_fail({:f, _} = fail) do
    fail
  end

  defp bif_fail({:catch_tag, _}) do
    {:f, 0}
  end

  defp next_block([]) do
    :none
  end

  defp next_block([{next, _} | _]) do
    next
  end

  defp ensure_label(fail0, r_cg(ultimate_fail: lbl)) do
    case bif_fail(fail0) do
      {:f, 0} ->
        {:f, lbl}

      {:f, _} = fail ->
        fail
    end
  end

  defp cg_block([r_cg_set(anno: %{recv_mark: l} = anno0) = i0 | t], context, st0) do
    anno = :maps.remove(:recv_mark, anno0)
    i = r_cg_set(i0, anno: anno)
    {is, st1} = cg_block([i | t], context, st0)
    {fail, st} = use_block_label(l, st1)
    {[{:recv_mark, fail} | is], st}
  end

  defp cg_block([r_cg_set(op: :new_try_tag, dst: tag, args: args)], {tag, fail0}, st) do
    {:catch_tag, fail} = fail0
    [reg, {:atom, kind}] = beam_args([tag | args], st)
    {[{kind, reg, fail}], st}
  end

  defp cg_block(
         [
           r_cg_set(anno: anno, op: {:bif, name}, dst: dst0, args: args0) = i,
           r_cg_set(op: :succeeded, dst: bool)
         ],
         {bool, fail0},
         st
       ) do
    [dst | args] = beam_args([dst0 | args0], st)
    line0 = call_line(:body, {:extfunc, :erlang, name, length(args)}, anno)
    fail = bif_fail(fail0)

    line =
      case fail do
        {:f, 0} ->
          line0

        {:f, _} ->
          []
      end

    case is_gc_bif(name, args) do
      true ->
        live = get_live(i)
        kill = kill_yregs(anno, st)
        {kill ++ line ++ [{:gc_bif, name, fail, live, args, dst}], st}

      false ->
        {line ++ [{:bif, name, fail, args, dst}], st}
    end
  end

  defp cg_block(
         [
           r_cg_set(op: {:bif, :tuple_size}, dst: arity0, args: [tuple0]),
           r_cg_set(op: {:bif, :"=:="}, dst: bool, args: [arity0, r_b_literal(val: ar)]) = eq
         ],
         {bool, fail} = context,
         st0
       ) do
    tuple = beam_arg(tuple0, st0)

    case beam_arg(arity0, st0) do
      {:z, _} ->
        test = {:test, :test_arity, ensure_label(fail, st0), [tuple, ar]}
        {[test], st0}

      arity ->
        tupleSize = {:bif, :tuple_size, {:f, 0}, [tuple], arity}
        {is, st} = cg_block([eq], context, st0)
        {[tupleSize | is], st}
    end
  end

  defp cg_block([r_cg_set(op: {:bif, name}, dst: dst0, args: args0)] = is0, {dst0, fail}, st0) do
    [dst | args] = beam_args([dst0 | args0], st0)

    case dst do
      {:z, _} ->
        {test, st1} = bif_to_test(name, args, ensure_label(fail, st0), st0)
        {test, st1}

      _ ->
        {is1, st1} = cg_block(is0, :none, st0)
        {is2, st} = cg_block([], {dst0, fail}, st1)
        {is1 ++ is2, st}
    end
  end

  defp cg_block(
         [
           r_cg_set(anno: anno, op: {:bif, name}, dst: dst0, args: args0) = i
           | t
         ],
         context,
         st0
       ) do
    [dst | args] = beam_args([dst0 | args0], st0)
    {is0, st} = cg_block(t, context, st0)

    case is_gc_bif(name, args) do
      true ->
        line = call_line(:body, {:extfunc, :erlang, name, length(args)}, anno)
        live = get_live(i)
        kill = kill_yregs(anno, st)

        is =
          kill ++
            line ++
            [
              {:gc_bif, name, {:f, 0}, live, args, dst}
              | is0
            ]

        {is, st}

      false ->
        is = [{:bif, name, {:f, 0}, args, dst} | is0]
        {is, st}
    end
  end

  defp cg_block(
         [
           r_cg_set(op: :bs_init, dst: dst0, args: args0, anno: anno) = i,
           r_cg_set(op: :succeeded, dst: bool)
         ],
         {bool, fail0},
         st
       ) do
    fail = bif_fail(fail0)
    line = line(anno)
    alloc = :erlang.map_get(:alloc, anno)
    [r_b_literal(val: kind) | args1] = args0
    live = get_live(i)

    case kind do
      :new ->
        [dst, size, {:integer, unit}] =
          beam_args(
            [
              dst0
              | args1
            ],
            st
          )

        {[line | cg_bs_init(dst, size, alloc, unit, live, fail)], st}

      :private_append ->
        [dst, src, bits, {:integer, unit}] =
          beam_args(
            [
              dst0
              | args1
            ],
            st
          )

        flags = {:field_flags, []}
        testHeap = {:test_heap, alloc, live}
        bsPrivateAppend = {:bs_private_append, fail, bits, unit, src, flags, dst}
        is = [testHeap, line, bsPrivateAppend]
        {is, st}

      :append ->
        [dst, src, bits, {:integer, unit}] =
          beam_args(
            [
              dst0
              | args1
            ],
            st
          )

        flags = {:field_flags, []}
        is = [line, {:bs_append, fail, bits, alloc, live, unit, src, flags, dst}]
        {is, st}
    end
  end

  defp cg_block(
         [
           r_cg_set(
             anno: anno,
             op: :bs_start_match,
             dst: ctx0,
             args: [r_b_literal(val: :new), bin0]
           ) = i,
           r_cg_set(op: :succeeded, dst: bool)
         ],
         {bool, fail},
         st
       ) do
    [dst, bin1] = beam_args([ctx0, bin0], st)
    {bin, pre} = force_reg(bin1, dst)
    live = get_live(i)

    case :maps.find(:num_slots, anno) do
      {:ok, slots} ->
        is = pre ++ [{:test, :bs_start_match2, fail, live, [bin, slots], dst}]
        {is, st}

      :error ->
        is = pre ++ [{:test, :bs_start_match3, fail, live, [bin], dst}]
        {is, st}
    end
  end

  defp cg_block(
         [
           r_cg_set(op: :bs_get) = set,
           r_cg_set(
             op: :succeeded,
             dst: bool
           )
         ],
         {bool, fail},
         st
       ) do
    {cg_bs_get(fail, set, st), st}
  end

  defp cg_block(
         [
           r_cg_set(
             op: :bs_match_string,
             args: [ctxVar, r_b_literal(val: string0)]
           ),
           r_cg_set(op: :succeeded, dst: bool)
         ],
         {bool, fail},
         st
       ) do
    ctxReg = beam_arg(ctxVar, st)
    bits = bit_size(string0)

    string =
      case rem(bits, 8) do
        0 ->
          string0

        rem ->
          <<string0::bitstring, 0::size(8 - rem)>>
      end

    is = [{:test, :bs_match_string, fail, [ctxReg, bits, {:string, string}]}]
    {is, st}
  end

  defp cg_block(
         [
           r_cg_set(dst: dst0, op: :landingpad, args: args0)
           | t
         ],
         context,
         st0
       ) do
    [dst, {:atom, kind}, tag] =
      beam_args(
        [dst0 | args0],
        st0
      )

    case kind do
      :catch ->
        cg_catch(dst, t, context, st0)

      :try ->
        cg_try(dst, tag, t, context, st0)
    end
  end

  defp cg_block([r_cg_set(op: :kill_try_tag, args: args0) | is], context, st0) do
    [reg] = beam_args(args0, st0)
    {is0, st} = cg_block(is, context, st0)
    {[{:try_end, reg} | is0], st}
  end

  defp cg_block(
         [
           r_cg_set(op: :catch_end, dst: dst0, args: args0)
           | is
         ],
         context,
         st0
       ) do
    [dst, reg, {:x, 0}] = beam_args([dst0 | args0], st0)
    {is0, st} = cg_block(is, context, st0)
    {[{:catch_end, reg} | copy({:x, 0}, dst) ++ is0], st}
  end

  defp cg_block(
         [
           r_cg_set(op: :call) = i,
           r_cg_set(
             op: :succeeded,
             dst: bool
           )
         ],
         {bool, _Fail},
         st
       ) do
    cg_block([i], :none, st)
  end

  defp cg_block(
         [
           r_cg_set(op: :match_fail) = i,
           r_cg_set(
             op: :succeeded,
             dst: bool
           )
         ],
         {bool, _Fail},
         st
       ) do
    cg_block([i], :none, st)
  end

  defp cg_block(
         [
           r_cg_set(op: :get_map_element, dst: dst0, args: args0),
           r_cg_set(op: :succeeded, dst: bool)
         ],
         {bool, fail0},
         st
       ) do
    [dst, map, key] = beam_args([dst0 | args0], st)
    fail = ensure_label(fail0, st)
    {[{:get_map_elements, fail, map, {:list, [key, dst]}}], st}
  end

  defp cg_block(
         [r_cg_set(op: op, dst: dst0, args: args0) = i, r_cg_set(op: :succeeded, dst: bool)],
         {bool, fail},
         st
       ) do
    [dst | args] = beam_args([dst0 | args0], st)
    {cg_test(op, bif_fail(fail), args, dst, i), st}
  end

  defp cg_block([r_cg_set(op: :bs_put, dst: bool, args: args0)], {bool, fail}, st) do
    args = beam_args(args0, st)
    {cg_bs_put(bif_fail(fail), args), st}
  end

  defp cg_block([r_cg_set(op: :bs_test_tail, dst: bool, args: args0)], {bool, fail}, st) do
    [ctx, {:integer, bits}] = beam_args(args0, st)
    {[{:test, :bs_test_tail2, bif_fail(fail), [ctx, bits]}], st}
  end

  defp cg_block([r_cg_set(op: {:float, :checkerror}, dst: bool)], {bool, fail}, st) do
    {[{:fcheckerror, bif_fail(fail)}], st}
  end

  defp cg_block([r_cg_set(op: :is_tagged_tuple, dst: bool, args: args0)], {bool, fail}, st) do
    [src, {:integer, arity}, tag] = beam_args(args0, st)
    {[{:test, :is_tagged_tuple, ensure_label(fail, st), [src, arity, tag]}], st}
  end

  defp cg_block([r_cg_set(op: :is_nonempty_list, dst: bool, args: args0)], {bool, fail}, st) do
    args = beam_args(args0, st)
    {[{:test, :is_nonempty_list, ensure_label(fail, st), args}], st}
  end

  defp cg_block([r_cg_set(op: :has_map_field, dst: dst0, args: args0)], {dst0, fail0}, st) do
    fail = ensure_label(fail0, st)

    case beam_args([dst0 | args0], st) do
      [{:z, 0}, src, key] ->
        {[{:test, :has_map_fields, fail, src, {:list, [key]}}], st}

      [dst, src, key] ->
        {[
           {:bif, :is_map_key, fail0, [key, src], dst},
           {:test, :is_eq_exact, fail, [dst, {:atom, true}]}
         ], st}
    end
  end

  defp cg_block([r_cg_set(op: :call) = call], {_Bool, _Fail} = context, st0) do
    {is0, st1} = cg_call(call, :body, :none, st0)
    {is1, st} = cg_block([], context, st1)
    {is0 ++ is1, st}
  end

  defp cg_block([r_cg_set(op: :call, dst: dst0) = call], context, st) do
    dst = beam_arg(dst0, st)

    case context do
      {:return, ^dst, _} ->
        cg_call(call, :tail, context, st)

      _ ->
        cg_call(call, :body, context, st)
    end
  end

  defp cg_block([r_cg_set(op: :call) = call | t], context, st0) do
    {is0, st1} = cg_call(call, :body, :none, st0)
    {is1, st} = cg_block(t, context, st1)
    {is0 ++ is1, st}
  end

  defp cg_block(
         [
           r_cg_set(anno: anno, op: makeFun, dst: dst0, args: [local | args0])
           | t
         ],
         context,
         st0
       )
       when makeFun === :make_fun or
              makeFun === :old_make_fun do
    r_b_local(name: r_b_literal(val: func), arity: arity) = local
    [dst | args] = beam_args([dst0 | args0], st0)
    {funcLbl, st1} = local_func_label(func, arity, st0)

    is0 =
      case makeFun do
        :make_fun ->
          [{:make_fun3, {:f, funcLbl}, 0, 0, dst, {:list, args}}]

        :old_make_fun ->
          setup_args(args) ++
            [
              {:make_fun2, {:f, funcLbl}, 0, 0, length(args)}
              | copy({:x, 0}, dst)
            ]
      end

    is1 =
      case anno do
        %{result_type: type} ->
          info = {:var_info, dst, [{:fun_type, type}]}
          is0 ++ [{:%, info}]

        %{} ->
          is0
      end

    {is2, st} = cg_block(t, context, st1)
    {is1 ++ is2, st}
  end

  defp cg_block([r_cg_set(op: :copy) | _] = t0, context, st0) do
    {is0, t} = cg_copy(t0, st0)
    {is1, st} = cg_block(t, context, st0)
    is = is0 ++ is1

    case is_call(t) do
      {:yes, arity} ->
        {opt_call_moves(is, arity), st}

      :no ->
        {is, st}
    end
  end

  defp cg_block([r_cg_set(op: :match_fail, args: args0, anno: anno)], :none, st) do
    args = beam_args(args0, st)
    is = cg_match_fail(args, line(anno), :none)
    {is, st}
  end

  defp cg_block(
         [
           r_cg_set(op: :match_fail, args: args0, anno: anno)
           | t
         ],
         context,
         st0
       ) do
    fcLabel =
      case context do
        {:return, _, :none} ->
          r_cg(st0, :fc_label)

        _ ->
          :none
      end

    args = beam_args(args0, st0)
    is0 = cg_match_fail(args, line(anno), fcLabel)
    {is1, st} = cg_block(t, context, st0)
    {is0 ++ is1, st}
  end

  defp cg_block([r_cg_set(op: :wait_timeout, dst: bool, args: args0)], {bool, fail}, st) do
    case beam_args(args0, st) do
      [{:atom, :infinity}] ->
        {[{:wait, fail}], st}

      [timeout] ->
        {[{:wait_timeout, fail, timeout}], st}
    end
  end

  defp cg_block([r_cg_set(op: op, dst: dst0, args: args0) = set], :none, st) do
    [dst | args] = beam_args([dst0 | args0], st)
    is = cg_instr(op, args, dst, set)
    {is, st}
  end

  defp cg_block([r_cg_set(op: op, dst: dst0, args: args0) = set | t], context, st0) do
    [dst | args] = beam_args([dst0 | args0], st0)
    is0 = cg_instr(op, args, dst, set)
    {is1, st} = cg_block(t, context, st0)
    {is0 ++ is1, st}
  end

  defp cg_block([r_cg_alloc() = alloc | t], context, st0) do
    is0 = cg_alloc(alloc, st0)
    {is1, st} = cg_block(t, context, st0)
    {is0 ++ is1, st}
  end

  defp cg_block([], {:return, arg, :none}, st) do
    is = copy(arg, {:x, 0}) ++ [:return]
    {is, st}
  end

  defp cg_block([], {:return, arg, n}, st) do
    is = copy(arg, {:x, 0}) ++ [{:deallocate, n}, :return]
    {is, st}
  end

  defp cg_block([], :none, st) do
    {[], st}
  end

  defp cg_block([], {bool0, fail}, st) do
    [bool] = beam_args([bool0], st)
    {[{:test, :is_eq_exact, fail, [bool, {:atom, true}]}], st}
  end

  defp cg_copy(t0, st) do
    {copies, t} =
      splitwith(
        fn
          r_cg_set(op: :copy) ->
            true

          _ ->
            false
        end,
        t0
      )

    moves0 = cg_copy_1(copies, st)

    moves1 =
      for {:move, src, dst} = move <- moves0,
          src !== dst do
        move
      end

    moves = order_moves(moves1)
    {moves, t}
  end

  defp cg_copy_1([r_cg_set(dst: dst0, args: args) | t], st) do
    [dst, src] = beam_args([dst0 | args], st)
    copies = cg_copy_1(t, st)

    case keymember(dst, 3, copies) do
      true ->
        copies

      false ->
        [{:move, src, dst} | copies]
    end
  end

  defp cg_copy_1([], _St) do
    []
  end

  defp bif_to_test(:or, [v1, v2], {:f, lbl} = fail, st0)
       when lbl !== 0 do
    {succLabel, st} = new_label(st0)

    {[
       {:test, :is_eq_exact, {:f, succLabel}, [v1, {:atom, false}]},
       {:test, :is_eq_exact, fail, [v2, {:atom, true}]},
       {:label, succLabel}
     ], st}
  end

  defp bif_to_test(op, args, fail, st) do
    {bif_to_test(op, args, fail), st}
  end

  defp bif_to_test(:and, [v1, v2], fail) do
    [
      {:test, :is_eq_exact, fail, [v1, {:atom, true}]},
      {:test, :is_eq_exact, fail, [v2, {:atom, true}]}
    ]
  end

  defp bif_to_test(:not, [var], fail) do
    [{:test, :is_eq_exact, fail, [var, {:atom, false}]}]
  end

  defp bif_to_test(name, args, fail) do
    [bif_to_test_1(name, args, fail)]
  end

  defp bif_to_test_1(:is_atom, [_] = ops, fail) do
    {:test, :is_atom, fail, ops}
  end

  defp bif_to_test_1(:is_boolean, [_] = ops, fail) do
    {:test, :is_boolean, fail, ops}
  end

  defp bif_to_test_1(:is_binary, [_] = ops, fail) do
    {:test, :is_binary, fail, ops}
  end

  defp bif_to_test_1(:is_bitstring, [_] = ops, fail) do
    {:test, :is_bitstr, fail, ops}
  end

  defp bif_to_test_1(:is_float, [_] = ops, fail) do
    {:test, :is_float, fail, ops}
  end

  defp bif_to_test_1(:is_function, [_] = ops, fail) do
    {:test, :is_function, fail, ops}
  end

  defp bif_to_test_1(:is_function, [_, _] = ops, fail) do
    {:test, :is_function2, fail, ops}
  end

  defp bif_to_test_1(:is_integer, [_] = ops, fail) do
    {:test, :is_integer, fail, ops}
  end

  defp bif_to_test_1(:is_list, [_] = ops, fail) do
    {:test, :is_list, fail, ops}
  end

  defp bif_to_test_1(:is_map, [_] = ops, fail) do
    {:test, :is_map, fail, ops}
  end

  defp bif_to_test_1(:is_number, [_] = ops, fail) do
    {:test, :is_number, fail, ops}
  end

  defp bif_to_test_1(:is_pid, [_] = ops, fail) do
    {:test, :is_pid, fail, ops}
  end

  defp bif_to_test_1(:is_port, [_] = ops, fail) do
    {:test, :is_port, fail, ops}
  end

  defp bif_to_test_1(:is_reference, [_] = ops, fail) do
    {:test, :is_reference, fail, ops}
  end

  defp bif_to_test_1(:is_tuple, [_] = ops, fail) do
    {:test, :is_tuple, fail, ops}
  end

  defp bif_to_test_1(:"=<", [a, b], fail) do
    {:test, :is_ge, fail, [b, a]}
  end

  defp bif_to_test_1(:>, [a, b], fail) do
    {:test, :is_lt, fail, [b, a]}
  end

  defp bif_to_test_1(:<, [_, _] = ops, fail) do
    {:test, :is_lt, fail, ops}
  end

  defp bif_to_test_1(:>=, [_, _] = ops, fail) do
    {:test, :is_ge, fail, ops}
  end

  defp bif_to_test_1(:==, [c, a], fail)
       when c === nil or
              :erlang.element(
                1,
                c
              ) === :integer or
              :erlang.element(
                1,
                c
              ) === :float or
              :erlang.element(
                1,
                c
              ) === :atom or
              :erlang.element(
                1,
                c
              ) === :literal do
    {:test, :is_eq, fail, [a, c]}
  end

  defp bif_to_test_1(:==, [_, _] = ops, fail) do
    {:test, :is_eq, fail, ops}
  end

  defp bif_to_test_1(:"/=", [c, a], fail)
       when c === nil or
              :erlang.element(
                1,
                c
              ) === :integer or
              :erlang.element(
                1,
                c
              ) === :float or
              :erlang.element(
                1,
                c
              ) === :atom or
              :erlang.element(
                1,
                c
              ) === :literal do
    {:test, :is_ne, fail, [a, c]}
  end

  defp bif_to_test_1(:"/=", [_, _] = ops, fail) do
    {:test, :is_ne, fail, ops}
  end

  defp bif_to_test_1(:"=:=", [c, a], fail)
       when c === nil or
              :erlang.element(
                1,
                c
              ) === :integer or
              :erlang.element(
                1,
                c
              ) === :float or
              :erlang.element(
                1,
                c
              ) === :atom or
              :erlang.element(
                1,
                c
              ) === :literal do
    {:test, :is_eq_exact, fail, [a, c]}
  end

  defp bif_to_test_1(:"=:=", [_, _] = ops, fail) do
    {:test, :is_eq_exact, fail, ops}
  end

  defp bif_to_test_1(:"=/=", [c, a], fail)
       when c === nil or
              :erlang.element(
                1,
                c
              ) === :integer or
              :erlang.element(
                1,
                c
              ) === :float or
              :erlang.element(
                1,
                c
              ) === :atom or
              :erlang.element(
                1,
                c
              ) === :literal do
    {:test, :is_ne_exact, fail, [a, c]}
  end

  defp bif_to_test_1(:"=/=", [_, _] = ops, fail) do
    {:test, :is_ne_exact, fail, ops}
  end

  defp opt_call_moves(is0, arity) do
    {moves0, is} =
      splitwith(
        fn
          {:move, _, _} ->
            true

          {:init_yregs, _} ->
            true

          _ ->
            false
        end,
        is0
      )

    moves = opt_call_moves_1(moves0, arity)
    moves ++ is
  end

  defp opt_call_moves_1(
         [
           {:move, src, {:x, _} = tmp} = m1,
           {:init_yregs, {:list, yregs}} = init
           | is
         ],
         arity
       ) do
    case is do
      [{:move, {:x, _} = ^tmp, {:x, 0}} = m2] ->
        case member(src, yregs) do
          true ->
            opt_call_moves_1([m1, m2, init], arity)

          false ->
            opt_call_moves_1([init, m1, m2], arity)
        end

      _ ->
        [m1, init | is]
    end
  end

  defp opt_call_moves_1(
         [
           {:move, src, {:x, _} = tmp} = m1,
           {:move, tmp, dst} = m2
           | is
         ],
         arity
       ) do
    case is_killed(tmp, is, arity) do
      true ->
        [{:move, src, dst} | opt_call_moves_1(is, arity)]

      false ->
        [m1 | opt_call_moves_1([m2 | is], arity)]
    end
  end

  defp opt_call_moves_1([m | ms], arity) do
    [m | opt_call_moves_1(ms, arity)]
  end

  defp opt_call_moves_1([], _Arity) do
    []
  end

  defp is_killed(r, [{:move, r, _} | _], _) do
    false
  end

  defp is_killed(r, [{:move, _, r} | _], _) do
    true
  end

  defp is_killed(r, [{:move, _, _} | is], arity) do
    is_killed(r, is, arity)
  end

  defp is_killed({:x, _} = r, [{:init_yregs, _} | is], arity) do
    is_killed(r, is, arity)
  end

  defp is_killed({:x, x}, [], arity) do
    x >= arity
  end

  defp cg_alloc(
         r_cg_alloc(stack: :none, words: r_need(h: 0, l: 0, f: 0)),
         _St
       ) do
    []
  end

  defp cg_alloc(
         r_cg_alloc(stack: :none, words: need, live: live),
         _St
       ) do
    [{:test_heap, alloc(need), live}]
  end

  defp cg_alloc(
         r_cg_alloc(stack: stk, words: need, live: live, def_yregs: defYregs),
         r_cg(regs: regs)
       )
       when is_integer(stk) do
    alloc = alloc(need)

    all =
      for y <- :lists.seq(0, stk - 1) do
        {:y, y}
      end

    def__ =
      :ordsets.from_list(
        for v <- defYregs do
          :maps.get(v, regs)
        end
      )

    needInit = :ordsets.subtract(all, def__)

    i =
      case alloc do
        0 ->
          {:allocate, stk, live}

        _ ->
          {:allocate_heap, stk, alloc, live}
      end

    [i | init_yregs(needInit)]
  end

  defp init_yregs([_ | _] = yregs) do
    [{:init_yregs, {:list, yregs}}]
  end

  defp init_yregs([]) do
    []
  end

  defp alloc(r_need(h: words, l: 0, f: 0)) do
    words
  end

  defp alloc(r_need(h: words, l: lambdas, f: floats)) do
    {:alloc, [{:words, words}, {:floats, floats}, {:funs, lambdas}]}
  end

  defp is_call([r_cg_set(op: :call, args: [r_b_var() | args]) | _]) do
    {:yes, 1 + length(args)}
  end

  defp is_call([r_cg_set(op: :call, args: [_ | args]) | _]) do
    {:yes, length(args)}
  end

  defp is_call([r_cg_set(op: :old_make_fun, args: [_ | args]) | _]) do
    {:yes, length(args)}
  end

  defp is_call(_) do
    :no
  end

  defp cg_call(
         r_cg_set(anno: anno, op: :call, dst: dst0, args: [r_b_local() = func0 | args0]),
         where,
         context,
         st0
       ) do
    [dst | args] = beam_args([dst0 | args0], st0)
    r_b_local(name: name0, arity: arity) = func0
    {:atom, name} = beam_arg(name0, st0)
    {funcLbl, st} = local_func_label(name, arity, st0)
    line = call_line(where, :local, anno)
    call = build_call(:call, arity, {:f, funcLbl}, context, dst)
    is = setup_args(args, anno, context, st) ++ line ++ call

    case anno do
      %{result_type: type} ->
        info = {:var_info, dst, [{:type, type}]}
        {is ++ [{:%, info}], st}

      %{} ->
        {is, st}
    end
  end

  defp cg_call(
         r_cg_set(anno: anno0, op: :call, dst: dst0, args: [r_b_remote() = func0 | args0]),
         where,
         context,
         st
       ) do
    [dst | args] = beam_args([dst0 | args0], st)
    r_b_remote(mod: mod0, name: name0, arity: arity) = func0

    case {beam_arg(mod0, st), beam_arg(name0, st)} do
      {{:atom, mod}, {:atom, name}} ->
        func = {:extfunc, mod, name, arity}
        line = call_line(where, func, anno0)
        call = build_call(:call_ext, arity, func, context, dst)

        anno =
          case :erl_bifs.is_exit_bif(mod, name, arity) do
            true ->
              :maps.remove(:kill_yregs, anno0)

            false ->
              anno0
          end

        is = setup_args(args, anno, context, st) ++ line ++ call
        {is, st}

      {mod, name} ->
        apply = build_apply(arity, context, dst)
        is = setup_args(args ++ [mod, name], anno0, context, st) ++ [line(anno0)] ++ apply
        {is, st}
    end
  end

  defp cg_call(r_cg_set(anno: anno, op: :call, dst: dst0, args: args0), where, context, st) do
    [dst, func | args] = beam_args([dst0 | args0], st)
    line = call_line(where, func, anno)
    arity = length(args)
    call = build_call(:call_fun, arity, func, context, dst)
    is = setup_args(args ++ [func], anno, context, st) ++ line ++ call

    case anno do
      %{result_type: type} ->
        info = {:var_info, dst, [{:type, type}]}
        {is ++ [{:%, info}], st}

      %{} ->
        {is, st}
    end
  end

  defp cg_match_fail([{:atom, :function_clause} | args], line, fc) do
    case fc do
      :none ->
        make_fc(args, line)

      _ ->
        setup_args(args) ++ [{:jump, {:f, fc}}]
    end
  end

  defp cg_match_fail([{:atom, op}], line, _Fc) do
    [line, op]
  end

  defp cg_match_fail([{:atom, op}, val], line, _Fc) do
    [line, {op, val}]
  end

  defp make_fc(args, line) do
    live =
      foldl(
        fn
          {:x, x}, a ->
            max(x + 1, a)

          _, a ->
            a
        end,
        0,
        args
      )

    tmpReg = {:x, live}
    stkMoves = build_stk(reverse(args), tmpReg, nil)

    [
      {:test_heap, 2 * length(args), live}
      | stkMoves
    ] ++
      [
        {:move, {:atom, :function_clause}, {:x, 0}},
        line,
        {:call_ext, 2, {:extfunc, :erlang, :error, 2}}
      ]
  end

  defp build_stk([v], _TmpReg, tail) do
    [{:put_list, v, tail, {:x, 1}}]
  end

  defp build_stk([v | vs], tmpReg, tail) do
    i = {:put_list, v, tail, tmpReg}
    [i | build_stk(vs, tmpReg, tmpReg)]
  end

  defp build_stk([], _TmpReg, nil) do
    [{:move, nil, {:x, 1}}]
  end

  defp build_call(:call_fun, arity, _Func, :none, dst) do
    [{:call_fun, arity} | copy({:x, 0}, dst)]
  end

  defp build_call(:call_fun, arity, _Func, {:return, dst, n}, dst)
       when is_integer(n) do
    [{:call_fun, arity}, {:deallocate, n}, :return]
  end

  defp build_call(:call_fun, arity, _Func, {:return, val, n}, _Dst)
       when is_integer(n) do
    [{:call_fun, arity}, {:move, val, {:x, 0}}, {:deallocate, n}, :return]
  end

  defp build_call(:call_ext, 2, {:extfunc, :erlang, :!, 2}, :none, dst) do
    [:send | copy({:x, 0}, dst)]
  end

  defp build_call(:call_ext, 2, {:extfunc, :erlang, :!, 2}, {:return, dst, n}, dst)
       when is_integer(n) do
    [:send, {:deallocate, n}, :return]
  end

  defp build_call(prefix, arity, func, {:return, dst, :none}, dst) do
    i =
      case prefix do
        :call ->
          :call_only

        :call_ext ->
          :call_ext_only
      end

    [{i, arity, func}]
  end

  defp build_call(
         :call_ext,
         arity,
         {:extfunc, mod, name, arity} = func,
         {:return, _, :none},
         _Dst
       ) do
    true = :erl_bifs.is_exit_bif(mod, name, arity)
    [{:call_ext_only, arity, func}]
  end

  defp build_call(prefix, arity, func, {:return, dst, n}, dst)
       when is_integer(n) do
    i =
      case prefix do
        :call ->
          :call_last

        :call_ext ->
          :call_ext_last
      end

    [{i, arity, func, n}]
  end

  defp build_call(i, arity, func, {:return, val, n}, _Dst)
       when is_integer(n) do
    [{i, arity, func} | copy(val, {:x, 0}) ++ [{:deallocate, n}, :return]]
  end

  defp build_call(i, arity, func, :none, dst) do
    [{i, arity, func} | copy({:x, 0}, dst)]
  end

  defp build_apply(arity, {:return, dst, n}, dst)
       when is_integer(n) do
    [{:apply_last, arity, n}]
  end

  defp build_apply(arity, {:return, val, n}, _Dst)
       when is_integer(n) do
    [{:apply, arity} | copy(val, {:x, 0}) ++ [{:deallocate, n}, :return]]
  end

  defp build_apply(arity, :none, dst) do
    [{:apply, arity} | copy({:x, 0}, dst)]
  end

  defp cg_instr(:bs_start_match, [{:atom, :resume}, src], dst, set) do
    live = get_live(set)
    [{:bs_start_match4, {:atom, :resume}, live, src, dst}]
  end

  defp cg_instr(:bs_start_match, [{:atom, :new}, src0], dst, set) do
    {src, pre} = force_reg(src0, dst)
    live = get_live(set)
    pre ++ [{:bs_start_match4, {:atom, :no_fail}, live, src, dst}]
  end

  defp cg_instr(:bs_get_tail, [src], dst, set) do
    live = get_live(set)
    [{:bs_get_tail, src, dst, live}]
  end

  defp cg_instr(:bs_get_position, [ctx], dst, set) do
    live = get_live(set)
    [{:bs_get_position, ctx, dst, live}]
  end

  defp cg_instr(:put_map, [{:atom, :assoc}, srcMap | ss], dst, set) do
    live = get_live(set)
    [{:put_map_assoc, {:f, 0}, srcMap, dst, live, {:list, ss}}]
  end

  defp cg_instr(op, args, dst, _Set) do
    cg_instr(op, args, dst)
  end

  defp cg_instr(:bs_init_writable, args, dst) do
    setup_args(args) ++
      [
        :bs_init_writable
        | copy(
            {:x, 0},
            dst
          )
      ]
  end

  defp cg_instr(:bs_restore, [ctx, slot], _Dst) do
    case slot do
      {:integer, n} ->
        [{:bs_restore2, ctx, n}]

      {:atom, :start} ->
        [{:bs_restore2, ctx, slot}]
    end
  end

  defp cg_instr(:bs_save, [ctx, slot], _Dst) do
    {:integer, n} = slot
    [{:bs_save2, ctx, n}]
  end

  defp cg_instr(:bs_set_position, [ctx, pos], _Dst) do
    [{:bs_set_position, ctx, pos}]
  end

  defp cg_instr(:build_stacktrace, args, dst) do
    setup_args(args) ++
      [
        :build_stacktrace
        | copy(
            {:x, 0},
            dst
          )
      ]
  end

  defp cg_instr(:set_tuple_element = op, [new, tuple, {:integer, index}], _Dst) do
    [{op, new, tuple, index}]
  end

  defp cg_instr({:float, :clearerror}, [], _Dst) do
    [:fclearerror]
  end

  defp cg_instr({:float, :get}, [src], dst) do
    [{:fmove, src, dst}]
  end

  defp cg_instr({:float, :put}, [src], dst) do
    [{:fmove, src, dst}]
  end

  defp cg_instr(:get_hd = op, [src], dst) do
    [{op, src, dst}]
  end

  defp cg_instr(:get_tl = op, [src], dst) do
    [{op, src, dst}]
  end

  defp cg_instr(:get_tuple_element = op, [src, {:integer, n}], dst) do
    [{op, src, n, dst}]
  end

  defp cg_instr(:has_map_field, [map, key], dst) do
    [{:bif, :is_map_key, {:f, 0}, [key, map], dst}]
  end

  defp cg_instr(:put_list = op, [hd, tl], dst) do
    [{op, hd, tl, dst}]
  end

  defp cg_instr(:nop, [], _Dst) do
    []
  end

  defp cg_instr(:put_tuple, elements, dst) do
    [{:put_tuple2, dst, {:list, elements}}]
  end

  defp cg_instr(:put_tuple_arity, [{:integer, arity}], dst) do
    [{:put_tuple, arity, dst}]
  end

  defp cg_instr(:put_tuple_elements, elements, _Dst) do
    for e <- elements do
      {:put, e}
    end
  end

  defp cg_instr(:raw_raise, args, dst) do
    setup_args(args) ++ [:raw_raise | copy({:x, 0}, dst)]
  end

  defp cg_instr(:remove_message, [], _Dst) do
    [:remove_message]
  end

  defp cg_instr(:resume, [a, b], _Dst) do
    [{:bif, :raise, {:f, 0}, [a, b], {:x, 0}}]
  end

  defp cg_instr(:timeout, [], _Dst) do
    [:timeout]
  end

  defp cg_test(:bs_add = op, fail, [src1, src2, {:integer, unit}], dst, _I) do
    [{op, fail, [src1, src2, unit], dst}]
  end

  defp cg_test(:bs_skip, fail, args, _Dst, i) do
    cg_bs_skip(fail, args, i)
  end

  defp cg_test(:bs_utf8_size = op, fail, [src], dst, _I) do
    [{op, fail, src, dst}]
  end

  defp cg_test(:bs_utf16_size = op, fail, [src], dst, _I) do
    [{op, fail, src, dst}]
  end

  defp cg_test({:float, :convert}, fail, [src], dst, r_cg_set(anno: anno)) do
    {:f, 0} = fail
    [line(anno), {:fconv, src, dst}]
  end

  defp cg_test({:float, op0}, fail, args, dst, r_cg_set(anno: anno)) do
    op =
      case op0 do
        :+ ->
          :fadd

        :- when length(args) === 2 ->
          :fsub

        :- ->
          :fnegate

        :* ->
          :fmul

        :/ ->
          :fdiv
      end

    [line(anno), {:bif, op, fail, args, dst}]
  end

  defp cg_test(:peek_message, fail, [], dst, _I) do
    [{:loop_rec, fail, {:x, 0}} | copy({:x, 0}, dst)]
  end

  defp cg_test(:put_map, fail, [{:atom, :exact}, srcMap | ss], dst, r_cg_set(anno: anno) = set) do
    live = get_live(set)
    [line(anno), {:put_map_exact, fail, srcMap, dst, live, {:list, ss}}]
  end

  defp cg_bs_get(fail, r_cg_set(dst: dst0, args: [r_b_literal(val: type) | ss0]) = set, st) do
    op =
      case type do
        :integer ->
          :bs_get_integer2

        :float ->
          :bs_get_float2

        :binary ->
          :bs_get_binary2

        :utf8 ->
          :bs_get_utf8

        :utf16 ->
          :bs_get_utf16

        :utf32 ->
          :bs_get_utf32
      end

    [dst | ss1] = beam_args([dst0 | ss0], st)

    ss =
      case ss1 do
        [ctx, {:literal, flags}, size, {:integer, unit}] ->
          [ctx, size, unit, field_flags(flags, set)]

        [ctx, {:literal, flags}] ->
          [ctx, field_flags(flags, set)]
      end

    live = get_live(set)
    [{:test, op, fail, live, ss, dst}]
  end

  defp cg_bs_skip(fail, [{:atom, type} | ss0], set) do
    op =
      case type do
        :utf8 ->
          :bs_skip_utf8

        :utf16 ->
          :bs_skip_utf16

        :utf32 ->
          :bs_skip_utf32

        _ ->
          :bs_skip_bits2
      end

    live = get_live(set)

    ss =
      case ss0 do
        [ctx, {:literal, flags}, size, {:integer, unit}] ->
          [ctx, size, unit, field_flags(flags, set)]

        [ctx, {:literal, flags}] ->
          [ctx, live, field_flags(flags, set)]
      end

    case {type, ss} do
      {:binary, [_, {:atom, :all}, 1, _]} ->
        []

      {:binary, [r, {:atom, :all}, u, _]} ->
        [{:test, :bs_test_unit, fail, [r, u]}]

      {_, _} ->
        [{:test, op, fail, ss}]
    end
  end

  defp field_flags(flags, r_cg_set(anno: %{location: {file, line}})) do
    {:field_flags, [{:anno, [line, {:file, file}]} | flags]}
  end

  defp field_flags(flags, _) do
    {:field_flags, flags}
  end

  defp cg_bs_put(
         fail,
         [{:atom, type}, {:literal, flags} | args]
       ) do
    op =
      case type do
        :integer ->
          :bs_put_integer

        :float ->
          :bs_put_float

        :binary ->
          :bs_put_binary

        :utf8 ->
          :bs_put_utf8

        :utf16 ->
          :bs_put_utf16

        :utf32 ->
          :bs_put_utf32
      end

    case args do
      [src, size, {:integer, unit}] ->
        [{op, fail, size, unit, {:field_flags, flags}, src}]

      [src] ->
        [{op, fail, {:field_flags, flags}, src}]
    end
  end

  defp cg_bs_init(dst, size0, alloc, unit, live, fail) do
    op =
      case unit do
        1 ->
          :bs_init_bits

        8 ->
          :bs_init2
      end

    size = cg_bs_init_size(size0)
    [{op, fail, size, alloc, live, {:field_flags, []}, dst}]
  end

  defp cg_bs_init_size({:x, _} = r) do
    r
  end

  defp cg_bs_init_size({:y, _} = r) do
    r
  end

  defp cg_bs_init_size({:integer, int}) do
    int
  end

  defp cg_catch(agg, t0, context, st0) do
    {moves, t1} = cg_extract(t0, agg, st0)
    {t, st} = cg_block(t1, context, st0)
    {moves ++ t, st}
  end

  defp cg_try(agg, tag, t0, context, st0) do
    {moves0, t1} = cg_extract(t0, agg, st0)
    moves = order_moves(moves0)
    [r_cg_set(op: :kill_try_tag) | t2] = t1
    {t, st} = cg_block(t2, context, st0)
    {[{:try_case, tag} | moves ++ t], st}
  end

  defp cg_extract([r_cg_set(op: :extract, dst: dst0, args: args0) | is0], agg, st) do
    [dst, ^agg, {:integer, x}] =
      beam_args(
        [dst0 | args0],
        st
      )

    {ds, is} = cg_extract(is0, agg, st)

    case keymember(dst, 3, ds) do
      true ->
        {ds, is}

      false ->
        {copy({:x, x}, dst) ++ ds, is}
    end
  end

  defp cg_extract(is, _, _) do
    {[], is}
  end

  defp copy(src, src) do
    []
  end

  defp copy(src, dst) do
    [{:move, src, dst}]
  end

  defp force_reg({:literal, _} = lit, reg) do
    {reg, [{:move, lit, reg}]}
  end

  defp force_reg({:integer, _} = lit, reg) do
    {reg, [{:move, lit, reg}]}
  end

  defp force_reg({:atom, _} = lit, reg) do
    {reg, [{:move, lit, reg}]}
  end

  defp force_reg({:float, _} = lit, reg) do
    {reg, [{:move, lit, reg}]}
  end

  defp force_reg(nil = lit, reg) do
    {reg, [{:move, lit, reg}]}
  end

  defp force_reg({kind, _} = r, _)
       when kind === :x or
              kind === :y do
    {r, []}
  end

  defp successors(r_cg_br(succ: succ, fail: fail)) do
    :ordsets.from_list([succ, fail])
  end

  defp successors(r_cg_switch(fail: fail, list: list)) do
    :ordsets.from_list([
      fail
      | for {_, lbl} <- list do
          lbl
        end
    ])
  end

  defp successors(r_cg_ret()) do
    []
  end

  defp linearize(blocks) do
    linear = :beam_ssa.linearize(blocks)
    linearize_1(linear, blocks)
  end

  defp linearize_1([{1, _} | ls], blocks) do
    linearize_1(ls, blocks)
  end

  defp linearize_1([{l, block0} | ls], blocks) do
    block = translate_block(l, block0, blocks)
    [{l, block} | linearize_1(ls, blocks)]
  end

  defp linearize_1([], _Blocks) do
    []
  end

  defp translate_block(l, r_b_blk(anno: anno, is: is0, last: last0), blocks) do
    last = translate_terminator(last0)
    phiCopies = translate_phis(l, last, blocks)
    is1 = translate_is(is0, phiCopies)

    is =
      case anno do
        %{frame_size: size} ->
          alloc = r_cg_alloc(stack: size)
          [alloc | is1]

        %{} ->
          is1
      end

    r_cg_blk(anno: anno, is: is, last: last)
  end

  defp translate_is([r_b_set(op: :phi) | is], tail) do
    translate_is(is, tail)
  end

  defp translate_is(
         [
           r_b_set(anno: anno0, op: op, dst: dst, args: args) = i
           | is
         ],
         tail
       ) do
    anno =
      case :beam_ssa.clobbers_xregs(i) do
        true ->
          Map.put(anno0, :clobbers, true)

        false ->
          anno0
      end

    [
      r_cg_set(anno: anno, op: op, dst: dst, args: args)
      | translate_is(is, tail)
    ]
  end

  defp translate_is([], tail) do
    tail
  end

  defp translate_terminator(r_b_ret(anno: anno, arg: arg)) do
    dealloc =
      case anno do
        %{deallocate: n} ->
          n

        %{} ->
          :none
      end

    r_cg_ret(arg: arg, dealloc: dealloc)
  end

  defp translate_terminator(r_b_br(bool: r_b_literal(val: true), succ: succ)) do
    r_cg_br(bool: r_b_literal(val: true), succ: succ, fail: succ)
  end

  defp translate_terminator(r_b_br(bool: bool, succ: succ, fail: fail)) do
    r_cg_br(bool: bool, succ: succ, fail: fail)
  end

  defp translate_terminator(r_b_switch(arg: bool, fail: fail, list: list)) do
    r_cg_switch(arg: bool, fail: fail, list: list)
  end

  defp translate_phis(l, r_cg_br(succ: target, fail: target), blocks) do
    r_b_blk(is: is) = :maps.get(target, blocks)

    phis =
      takewhile(
        fn
          r_b_set(op: :phi) ->
            true

          r_b_set() ->
            false
        end,
        is
      )

    case phis do
      [] ->
        []

      [r_b_set(op: :phi, dst: nopDst) | _] = ^phis ->
        nop = r_cg_set(op: :nop, dst: nopDst, args: [])
        [nop | phi_copies(phis, l)]
    end
  end

  defp translate_phis(_, _, _) do
    []
  end

  defp phi_copies([r_b_set(dst: dst, args: phiArgs) | sets], l) do
    copyArgs =
      for {v, target} <- phiArgs, target === l do
        v
      end

    [
      r_cg_set(op: :copy, dst: dst, args: copyArgs)
      | phi_copies(sets, l)
    ]
  end

  defp phi_copies([], _) do
    []
  end

  defp opt_move_to_x0(moves) do
    opt_move_to_x0(moves, [])
  end

  defp opt_move_to_x0([{:move, _, {:x, 0}} = i | is0], acc0) do
    case move_past_kill(is0, i, acc0) do
      :impossible ->
        opt_move_to_x0(is0, [i | acc0])

      {is, acc} ->
        opt_move_to_x0(is, acc)
    end
  end

  defp opt_move_to_x0([i | is], acc) do
    opt_move_to_x0(is, [i | acc])
  end

  defp opt_move_to_x0([], acc) do
    reverse(acc)
  end

  defp move_past_kill([{:init_yregs, {:list, yregs}} = i | is], {:move, src, _} = move, acc) do
    case member(src, yregs) do
      true ->
        :impossible

      false ->
        move_past_kill(is, move, [i | acc])
    end
  end

  defp move_past_kill(is, move, acc) do
    {is, [move | acc]}
  end

  defp setup_args(args, anno, :none, st) do
    case {setup_args(args), kill_yregs(anno, st)} do
      {moves, []} ->
        moves

      {moves, kills} ->
        opt_move_to_x0(moves ++ kills)
    end
  end

  defp setup_args(args, _, _, _) do
    setup_args(args)
  end

  defp setup_args([]) do
    []
  end

  defp setup_args([_ | _] = args) do
    moves = gen_moves(args, 0, [])
    order_moves(moves)
  end

  defp kill_yregs(%{kill_yregs: kill}, r_cg(regs: regs)) do
    case :ordsets.from_list(
           for v <- kill do
             :erlang.map_get(v, regs)
           end
         ) do
      [] ->
        []

      [_ | _] = list ->
        [{:init_yregs, {:list, list}}]
    end
  end

  defp kill_yregs(%{}, r_cg()) do
    []
  end

  defp gen_moves([a | as], i, acc) do
    gen_moves(as, i + 1, copy(a, {:x, i}) ++ acc)
  end

  defp gen_moves([], _, acc) do
    keysort(3, acc)
  end

  defp order_moves(ms) do
    order_moves(ms, [])
  end

  defp order_moves([{:move, _, _} = m | ms0], acc0) do
    {chain, ms} = collect_chain(ms0, [m])
    acc = reverse(chain, acc0)
    order_moves(ms, acc)
  end

  defp order_moves([], acc) do
    acc
  end

  defp collect_chain(ms, path) do
    collect_chain(ms, path, [])
  end

  defp collect_chain([{:move, src, same} = m | ms0], [{:move, same, _} | _] = path, others) do
    case keymember(src, 3, path) do
      false ->
        collect_chain(reverse(others, ms0), [m | path], [])

      true ->
        {break_up_cycle(m, path), reverse(others, ms0)}
    end
  end

  defp collect_chain([m | ms], path, others) do
    collect_chain(ms, path, [m | others])
  end

  defp collect_chain([], path, others) do
    {path, others}
  end

  defp break_up_cycle({:move, src, _Dst} = m, path) do
    break_up_cycle_1(src, [m | path], [])
  end

  defp break_up_cycle_1(dst, [{:move, _Src, dst} | path], acc) do
    reverse(acc, path)
  end

  defp break_up_cycle_1(dst, [{:move, s, d} | path], acc) do
    break_up_cycle_1(dst, path, [{:swap, s, d} | acc])
  end

  defp verify_beam_register({:x, _} = reg) do
    reg
  end

  defp is_beam_register({:x, _}) do
    true
  end

  defp is_beam_register(_) do
    false
  end

  defp get_register(v, regs) do
    case is_beam_register(v) do
      true ->
        v

      false ->
        :maps.get(v, regs)
    end
  end

  defp beam_args(as, st) do
    for a <- as do
      beam_arg(a, st)
    end
  end

  defp beam_arg(r_b_var() = name, r_cg(regs: regs)) do
    :maps.get(name, regs)
  end

  defp beam_arg(r_b_literal(val: val), _) do
    cond do
      is_atom(val) ->
        {:atom, val}

      is_float(val) ->
        {:float, val}

      is_integer(val) ->
        {:integer, val}

      val === [] ->
        nil

      true ->
        {:literal, val}
    end
  end

  defp beam_arg(reg, _) do
    verify_beam_register(reg)
  end

  defp new_block_label(l, st0) do
    {_Lbl, st} = label_for_block(l, st0)
    st
  end

  defp def_block_label(l, r_cg(labels: labels, used_labels: used)) do
    lbl = :maps.get(l, labels)

    case :gb_sets.is_member(lbl, used) do
      false ->
        []

      true ->
        [{:label, lbl}]
    end
  end

  defp use_block_labels(ls, st) do
    mapfoldl(&use_block_label/2, st, ls)
  end

  defp use_block_label(
         l,
         r_cg(used_labels: used, catches: catches) = st0
       ) do
    {lbl, st} = label_for_block(l, st0)

    case :gb_sets.is_member(l, catches) do
      true ->
        {{:catch_tag, {:f, lbl}}, r_cg(st, used_labels: :gb_sets.add(lbl, used))}

      false ->
        {{:f, lbl}, r_cg(st, used_labels: :gb_sets.add(lbl, used))}
    end
  end

  defp label_for_block(l, r_cg(labels: labels0) = st0) do
    case labels0 do
      %{^l => lbl} ->
        {lbl, st0}

      %{} ->
        {lbl, st} = new_label(st0)
        labels = Map.put(labels0, l, lbl)
        {lbl, r_cg(st, labels: labels)}
    end
  end

  defp local_func_label(name, arity, st) do
    local_func_label({name, arity}, st)
  end

  defp local_func_label(key, r_cg(functable: map) = st0) do
    case map do
      %{^key => label} ->
        {label, st0}

      _ ->
        {label, st} = new_label(st0)
        {label, r_cg(st, functable: Map.put(map, key, label))}
    end
  end

  defp is_gc_bif(:hd, [_]) do
    false
  end

  defp is_gc_bif(:tl, [_]) do
    false
  end

  defp is_gc_bif(:self, []) do
    false
  end

  defp is_gc_bif(:node, []) do
    false
  end

  defp is_gc_bif(:node, [_]) do
    false
  end

  defp is_gc_bif(:element, [_, _]) do
    false
  end

  defp is_gc_bif(:get, [_]) do
    false
  end

  defp is_gc_bif(:is_map_key, [_, _]) do
    false
  end

  defp is_gc_bif(:map_get, [_, _]) do
    false
  end

  defp is_gc_bif(:tuple_size, [_]) do
    false
  end

  defp is_gc_bif(bif, args) do
    arity = length(args)

    not (:erl_internal.bool_op(
           bif,
           arity
         ) or
           :erl_internal.new_type_test(
             bif,
             arity
           ) or
           :erl_internal.comp_op(
             bif,
             arity
           ))
  end

  defp new_label(r_cg(lcount: next) = st) do
    {next, r_cg(st, lcount: next + 1)}
  end

  defp call_line(_Context, {:extfunc, mod, name, arity}, anno) do
    case :erl_bifs.is_safe(mod, name, arity) do
      false ->
        [line(anno)]

      true ->
        []
    end
  end

  defp call_line(:body, _, anno) do
    [line(anno)]
  end

  defp call_line(:tail, :local, _) do
    []
  end

  defp call_line(:tail, _, anno) do
    [line(anno)]
  end

  defp line(%{location: {file, line}}) do
    {:line, [{:location, file, line}]}
  end

  defp line(%{}) do
    {:line, []}
  end

  defp flatmapfoldl(f, accu0, [hd | tail]) do
    {r, accu1} = f.(hd, accu0)
    {rs, accu2} = flatmapfoldl(f, accu1, tail)
    {r ++ rs, accu2}
  end

  defp flatmapfoldl(_, accu, []) do
    {[], accu}
  end
end
