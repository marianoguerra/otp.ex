defmodule :m_beam_ssa_throw do
  use Bitwise
  import :lists, only: [foldl: 3]
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

  Record.defrecord(:r_gst, :gst, tlh_roots: :undefined, tlh_edges: %{}, throws: :cerl_sets.new())

  Record.defrecord(:r_lst, :lst,
    suitability: %{},
    handlers: %{},
    blocks: :undefined,
    predecessors: :undefined
  )

  def module(r_b_module(body: fs0) = module, _Opts) do
    case scan(module) do
      {throws, tLHs} ->
        fs = opt(fs0, throws, tLHs)
        {:ok, r_b_module(module, body: fs)}

      :no_throws ->
        {:ok, module}
    end
  end

  defp scan(r_b_module(body: fs0) = module) do
    scan_1(fs0, init_gst(module))
  end

  defp init_gst(r_b_module(exports: exports)) do
    unsuitable = :gb_sets.singleton(:unsuitable)

    roots =
      foldl(
        fn {name, arity}, acc ->
          id = r_b_local(name: r_b_literal(val: name), arity: arity)
          :gb_trees.insert(id, unsuitable, acc)
        end,
        :gb_trees.empty(),
        exports
      )

    r_gst(tlh_roots: roots)
  end

  defp scan_1([r_b_function(bs: blocks) = f | fs], gst0) do
    id = get_func_id(f)
    preds = :beam_ssa.predecessors(blocks)
    linear = :beam_ssa.linearize(blocks)
    lst = r_lst(blocks: blocks, predecessors: preds)
    {_, gst} = scan_bs(linear, id, lst, gst0)
    scan_1(fs, gst)
  end

  defp scan_1([], gst) do
    r_gst(tlh_roots: roots, tlh_edges: edges, throws: throws) = gst

    case :cerl_sets.size(throws) do
      0 ->
        :no_throws

      _ ->
        tLHs = propagate_tlhs(:gb_trees.to_list(roots), edges, %{})
        {throws, tLHs}
    end
  end

  defp propagate_tlhs([{id, handlersA} | roots], edges, acc0) do
    handlersB = :maps.get(id, acc0, :gb_sets.empty())

    case :gb_sets.is_subset(handlersA, handlersB) do
      true ->
        propagate_tlhs(roots, edges, acc0)

      false ->
        merged = :gb_sets.union(handlersA, handlersB)
        callees = pt_callees(id, merged, edges)
        acc = propagate_tlhs(callees, edges, Map.put(acc0, id, merged))
        propagate_tlhs(roots, edges, acc)
    end
  end

  defp propagate_tlhs([], _Edges, acc) do
    acc
  end

  defp pt_callees(id, merged, edges) do
    case edges do
      %{^id => callees} ->
        :gb_sets.fold(
          fn callee, acc ->
            [{callee, merged} | acc]
          end,
          [],
          callees
        )

      %{} ->
        []
    end
  end

  defp scan_bs([{1, _} | bs], id, lst, gst) do
    scan_bs(bs, id, lst, gst)
  end

  defp scan_bs([{lbl, r_b_blk(last: last, is: is)} | bs], id, lst0, gst0) do
    {lst, gst} = scan_bs(bs, id, lst0, gst0)
    si_is(is, id, lbl, last, lst, gst)
  end

  defp scan_bs([], _Id, lst, gst) do
    {lst, gst}
  end

  defp si_is([r_b_set(op: :landingpad, args: [_Kind, _Tag]) | is], id, lbl, last, lst, gst) do
    vars = {:none, :none, :none}
    si_handler_start(is, id, lbl, last, vars, lst, gst)
  end

  defp si_is(
         [
           r_b_set(op: :resume, args: [stacktrace, _Reason])
           | is
         ],
         id,
         lbl,
         last,
         lst,
         gst
       ) do
    si_handler_end(is, id, lbl, last, stacktrace, lst, gst)
  end

  defp si_is(
         [
           r_b_set(op: :raw_raise, args: [_, _, stacktrace])
           | is
         ],
         id,
         lbl,
         last,
         lst,
         gst
       ) do
    si_handler_end(is, id, lbl, last, stacktrace, lst, gst)
  end

  defp si_is(
         [
           r_b_set(op: :build_stacktrace, args: [stacktrace])
           | is
         ],
         id,
         lbl,
         last,
         lst,
         gst
       ) do
    si_handler_end(is, id, lbl, last, stacktrace, lst, gst)
  end

  defp si_is(
         [
           r_b_set(
             op: :call,
             dst: dst,
             args: [
               r_b_remote(
                 mod: r_b_literal(val: :erlang),
                 name: r_b_literal(val: :throw),
                 arity: 1
               ),
               _Term
             ]
           )
         ],
         id,
         _Lbl,
         r_b_ret(arg: dst),
         lst,
         gst
       ) do
    r_gst(throws: throws0) = gst
    throws = :cerl_sets.add_element(id, throws0)
    {lst, r_gst(gst, throws: throws)}
  end

  defp si_is(
         [r_b_set(op: :call, dst: dst, args: [r_b_local() = callee | _])],
         id,
         _Lbl,
         r_b_ret(arg: dst),
         lst,
         gst
       ) do
    {lst, inherit_tlh(id, callee, gst)}
  end

  defp si_is(
         [
           r_b_set(op: :call, dst: dst, args: [r_b_local() = callee | _]),
           r_b_set(op: {:succeeded, :body}, args: [dst])
         ],
         id,
         _Lbl,
         r_b_br(fail: 1),
         lst,
         gst
       ) do
    {lst, inherit_tlh(id, callee, gst)}
  end

  defp si_is(
         [
           r_b_set(op: :call, dst: dst, args: [r_b_local() = callee | _]),
           r_b_set(op: {:succeeded, :body}, args: [dst])
         ],
         id,
         _Lbl,
         r_b_br(fail: fail),
         lst,
         gst
       ) do
    handlerId = {id, fail}
    {lst, add_tlh(handlerId, callee, lst, gst)}
  end

  defp si_is([r_b_set() | is], id, lbl, last, lst, gst) do
    si_is(is, id, lbl, last, lst, gst)
  end

  defp si_is([], _Id, _Lbl, _Last, lst, gst) do
    {lst, gst}
  end

  defp si_handler_end(is, id, endLbl, last, stacktrace, lst0, gst) do
    r_lst(suitability: suitability0) = lst0

    marker =
      case suitability0 do
        %{^stacktrace => {:tentative, _}} ->
          :unsuitable

        %{} ->
          {:tentative, endLbl}
      end

    suitability = Map.put(suitability0, stacktrace, marker)
    lst = r_lst(lst0, suitability: suitability)
    si_is(is, id, endLbl, last, lst, gst)
  end

  defp si_handler_start(
         [
           r_b_set(op: :extract, dst: dst, args: [_, r_b_literal(val: idx)])
           | is
         ],
         id,
         startLbl,
         last,
         vars0,
         lst,
         gst
       ) do
    :none = :erlang.element(1 + idx, vars0)
    vars = :erlang.setelement(1 + idx, vars0, dst)
    si_handler_start(is, id, startLbl, last, vars, lst, gst)
  end

  defp si_handler_start(is, id, startLbl, last, vars, lst0, gst) do
    handlerId = {id, startLbl}

    r_lst(blocks: blocks, predecessors: preds, suitability: suitability, handlers: handlers0) =
      lst0

    {_, _, stacktrace} = vars

    handlers =
      case suitability do
        %{^stacktrace => {:tentative, endLbl}} ->
          path = :beam_ssa.between(startLbl, endLbl, preds, blocks)
          partition = :maps.with(path, blocks)
          handler = {:tentative, startLbl, vars, partition}
          Map.put(handlers0, handlerId, handler)

        %{} when stacktrace !== :none ->
          Map.put(handlers0, handlerId, :unsuitable)

        %{} when stacktrace === :none ->
          Map.put(handlers0, handlerId, :suitable)
      end

    lst = r_lst(lst0, handlers: handlers)
    si_is(is, id, startLbl, last, lst, gst)
  end

  defp inherit_tlh(caller, callee, r_gst(tlh_edges: edges0) = gst) do
    callees =
      case edges0 do
        %{^caller => callees0} ->
          :gb_sets.add_element(callee, callees0)

        %{} ->
          :gb_sets.singleton(callee)
      end

    edges = Map.put(edges0, caller, callees)
    r_gst(gst, tlh_edges: edges)
  end

  defp add_tlh(id, callee, lst, gst) do
    r_lst(handlers: handlers) = lst
    r_gst(tlh_roots: roots0) = gst
    %{^id => handler} = handlers

    tLHs0 =
      case :gb_trees.lookup(callee, roots0) do
        :none ->
          :gb_sets.singleton(handler)

        {:value, v} ->
          v
      end

    tLHs =
      case :gb_sets.is_element(
             :unsuitable,
             tLHs0
           ) do
        true ->
          tLHs0

        false ->
          :gb_sets.add_element(handler, tLHs0)
      end

    roots = :gb_trees.enter(callee, tLHs, roots0)
    r_gst(gst, tlh_roots: roots)
  end

  defp opt([r_b_function(bs: blocks0) = f | fs], throws, tLHs) do
    id = get_func_id(f)

    blocks =
      case {:cerl_sets.is_element(id, throws), tLHs} do
        {true, %{^id => handlers}} ->
          opt_function(handlers, blocks0)

        {_, _} ->
          blocks0
      end

    [r_b_function(f, bs: blocks) | opt(fs, throws, tLHs)]
  end

  defp opt([], _Throws, _TLHs) do
    []
  end

  defp get_func_id(r_b_function(anno: anno)) do
    {_, name, arity} = :maps.get(:func_info, anno)
    r_b_local(name: r_b_literal(val: name), arity: arity)
  end

  defp opt_function(handlers, blocks) do
    case :gb_sets.is_member(:unsuitable, handlers) do
      true ->
        blocks

      false ->
        linear0 = :beam_ssa.linearize(blocks)
        linear = opt_bs(linear0, :gb_sets.to_list(handlers))
        :maps.from_list(linear)
    end
  end

  defp opt_bs(
         [{lbl, r_b_blk(last: last, is: is0) = blk} | bs],
         hs
       ) do
    is = opt_is(is0, last, hs)
    [{lbl, r_b_blk(blk, is: is)} | opt_bs(bs, hs)]
  end

  defp opt_bs([], _Hs) do
    []
  end

  defp opt_is(
         [
           r_b_set(
             op: :call,
             dst: dst,
             args: [
               r_b_remote(
                 mod: r_b_literal(val: :erlang),
                 name: r_b_literal(val: :throw),
                 arity: 1
               ),
               _Term
             ]
           ) = i0
         ],
         r_b_ret(arg: dst),
         hs
       ) do
    thrownType = :beam_ssa.get_anno(:thrown_type, i0, :any)
    i = opt_throw(hs, thrownType, i0)
    [i]
  end

  defp opt_is([i | is], last, hs) do
    [i | opt_is(is, last, hs)]
  end

  defp opt_is([], _Last, _Hs) do
    []
  end

  defp opt_throw([:suitable | hs], thrownType, i) do
    opt_throw(hs, thrownType, i)
  end

  defp opt_throw([{:tentative, start, vars, blocks} | hs], thrownType, i) do
    case opt_is_suitable(start, blocks, vars, thrownType) do
      true ->
        opt_throw(hs, thrownType, i)

      false ->
        i
    end
  end

  defp opt_throw([], _ThrownType, r_b_set(args: [_, reason]) = i) do
    mFA = r_b_remote(mod: r_b_literal(val: :erlang), name: r_b_literal(val: :raise), arity: 3)
    stacktrace = r_b_literal(val: [])
    r_b_set(i, args: [mFA, r_b_literal(val: :throw), reason, stacktrace])
  end

  defp opt_is_suitable(start, blocks, vars, thrownType) do
    ts = ois_init_ts(vars, thrownType)
    ois_1([start], blocks, ts)
  end

  defp ois_1([lbl | lbls], blocks, ts0) do
    case blocks do
      %{^lbl => r_b_blk(last: last, is: is)} ->
        case ois_is(is, ts0) do
          {:ok, ts} ->
            next = ois_successors(last, ts)
            ois_1(next ++ lbls, blocks, ts)

          :error ->
            false
        end

      %{} ->
        ois_1(lbls, blocks, ts0)
    end
  end

  defp ois_1([], _Blocks, _Ts) do
    true
  end

  defp ois_successors(r_b_switch(fail: fail, list: list), _Ts) do
    lbls =
      for {_, lbl} <- list do
        lbl
      end

    [fail | lbls]
  end

  defp ois_successors(r_b_br(bool: bool, succ: succ, fail: fail), ts) do
    case :beam_types.get_singleton_value(
           ois_get_type(
             bool,
             ts
           )
         ) do
      {:ok, true} ->
        [succ]

      {:ok, false} ->
        [fail]

      :error ->
        [succ, fail]
    end
  end

  defp ois_init_ts({class, reason, stacktrace}, thrownType) do
    ts = %{class => :beam_types.make_atom(:throw), reason => thrownType, stacktrace => :any}
    :maps.remove(:none, ts)
  end

  defp ois_is([r_b_set(op: :build_stacktrace) | _], _Ts) do
    :error
  end

  defp ois_is([r_b_set(op: :raw_raise) | _], _Ts) do
    :error
  end

  defp ois_is([r_b_set(op: :resume) | _], _Ts) do
    :error
  end

  defp ois_is(
         [r_b_set(op: :get_hd, dst: dst, args: [src]) | is],
         ts0
       ) do
    srcType = ois_get_type(src, ts0)
    {type, _, _} = :beam_call_types.types(:erlang, :hd, [srcType])
    ts = Map.put(ts0, dst, type)
    ois_is(is, ts)
  end

  defp ois_is(
         [r_b_set(op: :get_tl, dst: dst, args: [src]) | is],
         ts0
       ) do
    srcType = ois_get_type(src, ts0)
    {type, _, _} = :beam_call_types.types(:erlang, :tl, [srcType])
    ts = Map.put(ts0, dst, type)
    ois_is(is, ts)
  end

  defp ois_is(
         [
           r_b_set(op: :get_tuple_element, dst: dst, args: [src, offset])
           | is
         ],
         ts0
       ) do
    type =
      case ts0 do
        %{^src => r_t_tuple(size: size, elements: es)} ->
          r_b_literal(val: n) = offset
          true = size > n
          :beam_types.get_tuple_element(n + 1, es)

        %{} ->
          :any
      end

    ts = Map.put(ts0, dst, type)
    ois_is(is, ts)
  end

  defp ois_is(
         [
           r_b_set(op: {:bif, :is_atom}, dst: dst, args: [src])
           | is
         ],
         ts
       ) do
    ois_type_test(src, dst, r_t_atom(), is, ts)
  end

  defp ois_is(
         [
           r_b_set(op: {:bif, :is_bitstring}, dst: dst, args: [src])
           | is
         ],
         ts
       ) do
    ois_type_test(src, dst, r_t_bitstring(), is, ts)
  end

  defp ois_is(
         [
           r_b_set(op: {:bif, :is_binary}, dst: dst, args: [src])
           | is
         ],
         ts
       ) do
    ois_type_test(src, dst, r_t_bitstring(size_unit: 8), is, ts)
  end

  defp ois_is(
         [
           r_b_set(op: {:bif, :is_float}, dst: dst, args: [src])
           | is
         ],
         ts
       ) do
    ois_type_test(src, dst, r_t_float(), is, ts)
  end

  defp ois_is(
         [
           r_b_set(op: {:bif, :is_integer}, dst: dst, args: [src])
           | is
         ],
         ts
       ) do
    ois_type_test(src, dst, r_t_integer(), is, ts)
  end

  defp ois_is(
         [
           r_b_set(op: {:bif, :is_list}, dst: dst, args: [src])
           | is
         ],
         ts
       ) do
    ois_type_test(src, dst, r_t_list(), is, ts)
  end

  defp ois_is(
         [
           r_b_set(op: {:bif, :is_map}, dst: dst, args: [src])
           | is
         ],
         ts
       ) do
    ois_type_test(src, dst, r_t_map(), is, ts)
  end

  defp ois_is(
         [
           r_b_set(op: {:bif, :is_number}, dst: dst, args: [src])
           | is
         ],
         ts
       ) do
    ois_type_test(src, dst, :number, is, ts)
  end

  defp ois_is(
         [
           r_b_set(op: {:bif, :is_tuple}, dst: dst, args: [src])
           | is
         ],
         ts
       ) do
    ois_type_test(src, dst, r_t_tuple(), is, ts)
  end

  defp ois_is(
         [
           r_b_set(op: :is_nonempty_list, dst: dst, args: [src])
           | is
         ],
         ts
       ) do
    ois_type_test(src, dst, r_t_cons(), is, ts)
  end

  defp ois_is(
         [
           r_b_set(op: :is_tagged_tuple, dst: dst, args: [src, size, tag])
           | is
         ],
         ts
       ) do
    es = :beam_types.set_tuple_element(1, ois_get_type(tag, ts), %{})
    r_b_literal(val: n) = size
    type = r_t_tuple(exact: true, size: n, elements: es)
    ois_type_test(src, dst, type, is, ts)
  end

  defp ois_is(
         [
           r_b_set(op: {:bif, :tuple_size}, dst: dst, args: [src])
           | is
         ],
         ts0
       ) do
    srcType = ois_get_type(src, ts0)
    {type, _, _} = :beam_call_types.types(:erlang, :tuple_size, [srcType])
    ts = Map.put(ts0, dst, type)
    ois_is(is, ts)
  end

  defp ois_is(
         [
           r_b_set(op: {:bif, :"=:="}, dst: dst, args: [lHS, r_b_literal(val: rHS)])
           | is
         ],
         ts0
       ) do
    type =
      case :beam_types.get_singleton_value(
             ois_get_type(
               lHS,
               ts0
             )
           ) do
        {:ok, ^rHS} ->
          :beam_types.make_atom(true)

        {:ok, _Other} ->
          :beam_types.make_atom(false)

        :error ->
          :beam_types.make_boolean()
      end

    ts = Map.put(ts0, dst, type)
    ois_is(is, ts)
  end

  defp ois_is([r_b_set() | is], ts) do
    ois_is(is, ts)
  end

  defp ois_is([], ts) do
    {:ok, ts}
  end

  defp ois_type_test(src, dst, requiredType, is, ts0) do
    givenType = ois_get_type(src, ts0)

    type =
      case :beam_types.meet(
             givenType,
             requiredType
           ) do
        ^givenType ->
          :beam_types.make_atom(true)

        :none ->
          :beam_types.make_atom(false)

        _Other ->
          :beam_types.make_boolean()
      end

    ts = Map.put(ts0, dst, type)
    ois_is(is, ts)
  end

  defp ois_get_type(r_b_literal(val: value), _Ts) do
    :beam_types.make_type_from_value(value)
  end

  defp ois_get_type(r_b_var() = arg, ts) do
    case ts do
      %{^arg => type} ->
        type

      %{} ->
        :any
    end
  end
end
