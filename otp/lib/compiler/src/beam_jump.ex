defmodule :m_beam_jump do
  use Bitwise
  import :lists, only: [foldl: 3, keymember: 3, mapfoldl: 3, reverse: 1, reverse: 2]
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

  def module({mod, exp, attr, fs0, lc0}, _Opt) do
    {fs, lc} = mapfoldl(&function/2, lc0, fs0)
    {:ok, {mod, exp, attr, fs, lc}}
  end

  defp function({:function, name, arity, cLabel, asm0}, lc0) do
    try do
      asm1 = eliminate_moves(asm0)
      {asm2, lc} = insert_labels(asm1, lc0, [])
      asm3 = share(asm2)
      asm4 = move(asm3)
      asm5 = opt(asm4, cLabel)
      asm6 = unshare(asm5)
      asm = remove_unused_labels(asm6)
      {{:function, name, arity, cLabel, asm}, lc}
    catch
      class, error ->
        :io.fwrite('Function: ~w/~w\n', [name, arity])
        :erlang.raise(class, error, __STACKTRACE__)
    end
  end

  defp eliminate_moves(is) do
    eliminate_moves(is, %{}, [])
  end

  defp eliminate_moves(
         [
           {:select, :select_val, reg, {:f, fail}, list} = i
           | is
         ],
         d0,
         acc
       ) do
    d1 = add_unsafe_label(fail, d0)
    d = update_value_dict(list, reg, d1)
    eliminate_moves(is, d, [i | acc])
  end

  defp eliminate_moves([{:test, :is_eq_exact, _, [reg, val]} = i, {:block, blkIs0} | is], d0, acc) do
    d = update_unsafe_labels(i, d0)
    regVal = {reg, val}
    blkIs = eliminate_moves_blk(blkIs0, regVal)
    eliminate_moves([{:block, blkIs} | is], d, [i | acc])
  end

  defp eliminate_moves(
         [
           {:test, :is_nonempty_list, fail, [reg]} = i
           | is
         ],
         d0,
         acc
       ) do
    case is_proper_list(reg, acc) do
      true ->
        d = update_value_dict([nil, fail], reg, d0)
        eliminate_moves(is, d, [i | acc])

      false ->
        d = update_unsafe_labels(i, d0)
        eliminate_moves(is, d, [i | acc])
    end
  end

  defp eliminate_moves([{:label, lbl}, {:block, blkIs0} = blk | is], d, acc0) do
    acc = [{:label, lbl} | acc0]

    case {no_fallthrough(acc0), d} do
      {true, %{^lbl => {_, _} = regVal}} ->
        blkIs = eliminate_moves_blk(blkIs0, regVal)
        eliminate_moves([{:block, blkIs} | is], d, acc)

      {_, _} ->
        eliminate_moves([blk | is], d, acc)
    end
  end

  defp eliminate_moves([{:call, _, _} = i | is], d, acc) do
    eliminate_moves_call(is, d, [i | acc])
  end

  defp eliminate_moves([{:call_ext, _, _} = i | is], d, acc) do
    eliminate_moves_call(is, d, [i | acc])
  end

  defp eliminate_moves([{:block, []} | is], d, acc) do
    eliminate_moves(is, d, acc)
  end

  defp eliminate_moves([i | is], d0, acc) do
    d = update_unsafe_labels(i, d0)
    eliminate_moves(is, d, [i | acc])
  end

  defp eliminate_moves([], _, acc) do
    reverse(acc)
  end

  defp eliminate_moves_call(
         [{:%, {:var_info, {:x, 0}, info}} = anno, {:block, blkIs0} = blk | is],
         d,
         acc0
       ) do
    acc = [anno | acc0]
    retType = :proplists.get_value(:type, info, :none)

    case :beam_types.get_singleton_value(retType) do
      {:ok, value} ->
        regVal = {{:x, 0}, value_to_literal(value)}
        blkIs = eliminate_moves_blk(blkIs0, regVal)
        eliminate_moves([{:block, blkIs} | is], d, acc)

      :error ->
        eliminate_moves(is, d, [blk | acc])
    end
  end

  defp eliminate_moves_call(is, d, acc) do
    eliminate_moves(is, d, acc)
  end

  defp eliminate_moves_blk(
         [{:set, [dst], [_], :move} | _] = is,
         {_, dst}
       ) do
    is
  end

  defp eliminate_moves_blk(
         [{:set, [dst], [lit], :move} | is],
         {dst, lit}
       ) do
    is
  end

  defp eliminate_moves_blk(
         [{:set, [dst], [_], :move} | _] = is,
         {dst, _}
       ) do
    is
  end

  defp eliminate_moves_blk(
         [{:set, [_], [_], :move} = i | is],
         {_, _} = regVal
       ) do
    [i | eliminate_moves_blk(is, regVal)]
  end

  defp eliminate_moves_blk(is, _) do
    is
  end

  defp no_fallthrough([{:%, _} | is]) do
    no_fallthrough(is)
  end

  defp no_fallthrough([i | _]) do
    is_unreachable_after(i)
  end

  defp is_proper_list(reg, [{:%, {:var_info, reg, info}} | _]) do
    case :proplists.get_value(:type, info) do
      r_t_list(terminator: nil) ->
        true

      _ ->
        false
    end
  end

  defp is_proper_list(reg, [{:%, {:var_info, _, _}} | is]) do
    is_proper_list(reg, is)
  end

  defp is_proper_list(_, _) do
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

  defp update_value_dict([lit, {:f, lbl} | t], reg, d0) do
    d =
      case d0 do
        %{^lbl => :unsafe} ->
          d0

        %{^lbl => {^reg, ^lit}} ->
          d0

        %{^lbl => _} ->
          %{d0 | lbl => :unsafe}

        %{} ->
          Map.put(d0, lbl, {reg, lit})
      end

    update_value_dict(t, reg, d)
  end

  defp update_value_dict([], _, d) do
    d
  end

  defp add_unsafe_label(l, d) do
    Map.put(d, l, :unsafe)
  end

  defp update_unsafe_labels(i, d) do
    ls = instr_labels(i)
    update_unsafe_labels_1(ls, d)
  end

  defp update_unsafe_labels_1([l | ls], d) do
    update_unsafe_labels_1(ls, Map.put(d, l, :unsafe))
  end

  defp update_unsafe_labels_1([], d) do
    d
  end

  defp insert_labels([{:test, op, _, _} = i | is], lc, acc) do
    useful =
      case op do
        :is_lt ->
          true

        :is_ge ->
          true

        :is_eq_exact ->
          true

        :is_ne_exact ->
          true

        _ ->
          false
      end

    case useful do
      false ->
        insert_labels(is, lc, [i | acc])

      true ->
        insert_labels(is, lc + 1, [{:label, lc}, i | acc])
    end
  end

  defp insert_labels([i | is], lc, acc) do
    insert_labels(is, lc, [i | acc])
  end

  defp insert_labels([], lc, acc) do
    {reverse(acc), lc}
  end

  defp share(is0) do
    is1 = eliminate_fallthroughs(is0, [])

    is2 =
      find_fixpoint(
        fn is ->
          share_1(is)
        end,
        is1
      )

    reverse(is2)
  end

  defp share_1(is) do
    safe = classify_labels(is)
    share_1(is, safe, %{}, %{}, [], [])
  end

  defp share_1([{:label, l} = lbl | is], safe, dict0, lbls0, [_ | _] = seq0, acc) do
    seq = maybe_add_scope(seq0, l, safe)

    case dict0 do
      %{^seq => label} ->
        lbls = Map.put(lbls0, l, label)
        share_1(is, safe, dict0, lbls, [], [[lbl, {:jump, {:f, label}}] | acc])

      %{} ->
        case map_size(safe) === 0 or is_shareable(seq) do
          true ->
            dict = Map.put(dict0, seq, l)
            share_1(is, safe, dict, lbls0, [], [[lbl | seq] | acc])

          false ->
            share_1(is, safe, dict0, lbls0, [], [[lbl | seq] | acc])
        end
    end
  end

  defp share_1([{:func_info, _, _, _} | _] = is0, _Safe, _, lbls, [], acc0) do
    f =
      case lbls === %{} do
        true ->
          &:lists.reverse/2

        false ->
          fn is, acc ->
            :beam_utils.replace_labels(is, acc, lbls, fn old ->
              old
            end)
          end
      end

    foldl(f, is0, acc0)
  end

  defp share_1([{:catch, _, _} = i | is], safe, dict, _Lbls0, seq, acc) do
    share_1(is, safe, dict, %{}, [i | seq], acc)
  end

  defp share_1([{:try, _, _} = i | is], safe, dict, _Lbls, seq, acc) do
    share_1(is, safe, dict, %{}, [i | seq], acc)
  end

  defp share_1(
         [
           {:jump, {:f, to}} = i,
           {:label, from} = lbl
           | is
         ],
         safe,
         dict0,
         lbls0,
         _Seq,
         acc
       ) do
    lbls = Map.put(lbls0, from, to)
    share_1(is, safe, dict0, lbls, [], [[lbl, i] | acc])
  end

  defp share_1([i | is], safe, dict, lbls, seq, acc) do
    case is_unreachable_after(i) do
      false ->
        share_1(is, safe, dict, lbls, [i | seq], acc)

      true ->
        share_1(is, safe, dict, lbls, [i], acc)
    end
  end

  defp maybe_add_scope(seq, l, safe) do
    case safe do
      %{^l => scope} ->
        add_scope(seq, scope)

      %{} ->
        seq
    end
  end

  defp add_scope([{:line, loc} = i | is], scope) do
    case keymember(:scope, 1, loc) do
      false ->
        [
          {:line, [{:scope, scope} | loc]}
          | add_scope(
              is,
              scope
            )
        ]

      true ->
        [i | add_scope(is, scope)]
    end
  end

  defp add_scope([i | is], scope) do
    [i | add_scope(is, scope)]
  end

  defp add_scope([], _Scope) do
    []
  end

  defp is_shareable([:build_stacktrace | _]) do
    false
  end

  defp is_shareable([{:case_end, _} | _]) do
    false
  end

  defp is_shareable([{:catch, _, _} | _]) do
    false
  end

  defp is_shareable([{:catch_end, _} | _]) do
    false
  end

  defp is_shareable([:if_end | _]) do
    false
  end

  defp is_shareable([{:try, _, _} | _]) do
    false
  end

  defp is_shareable([{:try_case, _} | _]) do
    false
  end

  defp is_shareable([{:try_end, _} | _]) do
    false
  end

  defp is_shareable([_ | is]) do
    is_shareable(is)
  end

  defp is_shareable([]) do
    true
  end

  defp classify_labels(is) do
    classify_labels(is, 0, %{})
  end

  defp classify_labels([{:catch, _, _} | is], scope, safe) do
    classify_labels(is, scope + 1, safe)
  end

  defp classify_labels([{:catch_end, _} | is], scope, safe) do
    classify_labels(is, scope + 1, safe)
  end

  defp classify_labels([{:try, _, _} | is], scope, safe) do
    classify_labels(is, scope + 1, safe)
  end

  defp classify_labels([{:try_end, _} | is], scope, safe) do
    classify_labels(is, scope + 1, safe)
  end

  defp classify_labels([{:try_case, _} | is], scope, safe) do
    classify_labels(is, scope + 1, safe)
  end

  defp classify_labels([{:try_case_end, _} | is], scope, safe) do
    classify_labels(is, scope + 1, safe)
  end

  defp classify_labels([i | is], scope, safe0) do
    labels = instr_labels(i)

    safe =
      foldl(
        fn l, a ->
          case a do
            %{^l => [^scope]} ->
              a

            %{^l => other} ->
              Map.put(a, l, :ordsets.add_element(scope, other))

            %{} ->
              Map.put(a, l, [scope])
          end
        end,
        safe0,
        labels
      )

    classify_labels(is, scope, safe)
  end

  defp classify_labels([], scope, safe) do
    case scope do
      0 ->
        %{}

      _ ->
        safe
    end
  end

  defp eliminate_fallthroughs([{:label, l} = lbl | is], [i | _] = acc) do
    case is_unreachable_after(i) do
      false ->
        eliminate_fallthroughs(
          is,
          [lbl, {:jump, {:f, l}} | acc]
        )

      true ->
        eliminate_fallthroughs(is, [lbl | acc])
    end
  end

  defp eliminate_fallthroughs([i | is], acc) do
    eliminate_fallthroughs(is, [i | acc])
  end

  defp eliminate_fallthroughs([], acc) do
    acc
  end

  defp move(is) do
    move_1(is, [], [])
  end

  defp move_1([i | is], ends, acc0) do
    case is_exit_instruction(i) do
      false ->
        move_1(is, ends, [i | acc0])

      true ->
        case extract_seq(acc0, [i]) do
          :no ->
            move_1(is, ends, [i | acc0])

          {:yes, end__, acc} ->
            move_1(is, [end__ | ends], acc)
        end
    end
  end

  defp move_1([], ends, acc) do
    reverse(acc, :lists.append(reverse(ends)))
  end

  defp extract_seq([{:line, _} = line | is], acc) do
    extract_seq(is, [line | acc])
  end

  defp extract_seq([{:block, _} = bl | is], acc) do
    extract_seq_1(is, [bl | acc])
  end

  defp extract_seq([{:label, _} | _] = is, acc) do
    extract_seq_1(is, acc)
  end

  defp extract_seq(_, _) do
    :no
  end

  defp extract_seq_1([{:line, _} = line | is], acc) do
    extract_seq_1(is, [line | acc])
  end

  defp extract_seq_1([{:label, _}, {:func_info, _, _, _} | _], _) do
    :no
  end

  defp extract_seq_1([{:label, lbl}, {:jump, {:f, lbl}} | _], _) do
    :no
  end

  defp extract_seq_1([{:label, _} = lbl | is], acc) do
    {:yes, [lbl | acc], is}
  end

  defp extract_seq_1(_, _) do
    :no
  end

  Record.defrecord(:r_st, :st, entry: :undefined, replace: :undefined, labels: :undefined)

  defp opt(is0, cLabel) do
    find_fixpoint(
      fn is ->
        lbls = initial_labels(is)
        st = r_st(entry: cLabel, replace: %{}, labels: lbls)
        opt(is, [], st)
      end,
      is0
    )
  end

  defp find_fixpoint(optFun, is0) do
    case optFun.(is0) do
      ^is0 ->
        is0

      is ->
        find_fixpoint(optFun, is)
    end
  end

  defp opt(
         [
           {:test, _, {:f, l} = lbl, _} = i
           | [
               {:jump, {:f, l}}
               | _
             ] = is
         ],
         acc,
         st
       ) do
    case :beam_utils.is_pure_test(i) do
      false ->
        opt(is, [i | acc], label_used(lbl, st))

      true ->
        opt(is, acc, st)
    end
  end

  defp opt(
         [
           {:test, test0, {:f, l} = lbl, ops} = i
           | [{:jump, to} | is] = is0
         ],
         acc,
         st
       ) do
    case is_label_defined(is, l) do
      false ->
        opt(is0, [i | acc], label_used(lbl, st))

      true ->
        case invert_test(test0) do
          :not_possible ->
            opt(is0, [i | acc], label_used(lbl, st))

          test ->
            opt([{:test, test, to, ops} | is], acc, st)
        end
    end
  end

  defp opt([{:test, _, {:f, _} = lbl, _} = i | is], acc, st) do
    opt(is, [i | acc], label_used(lbl, st))
  end

  defp opt([{:test, _, {:f, _} = lbl, _, _, _} = i | is], acc, st) do
    opt(is, [i | acc], label_used(lbl, st))
  end

  defp opt([{:select, _, _R, fail, vls} = i | is], acc, st) do
    skip_unreachable(is, [i | acc], label_used([fail | vls], st))
  end

  defp opt([{:label, from} = i, {:label, to} | is], acc, r_st(replace: replace) = st) do
    opt([i | is], acc, r_st(st, replace: Map.put(replace, to, from)))
  end

  defp opt(
         [
           {:jump, {:f, _} = x}
           | [
               {:label, _},
               {:jump, x}
               | _
             ] = is
         ],
         acc,
         st
       ) do
    opt(is, acc, st)
  end

  defp opt([{:jump, {:f, lbl}} | [{:label, lbl} | _] = is], acc, st) do
    opt(is, acc, st)
  end

  defp opt([{:jump, {:f, l} = lbl} = i | is], acc0, st0) do
    {acc, st} = collect_labels(acc0, l, st0)
    skip_unreachable(is, [i | acc], label_used(lbl, st))
  end

  defp opt([{:block, _} = i | is], acc, st) do
    opt(is, [i | acc], st)
  end

  defp opt([{:kill, _} = i | is], acc, st) do
    opt(is, [i | acc], st)
  end

  defp opt([{:call, _, _} = i | is], acc, st) do
    opt(is, [i | acc], st)
  end

  defp opt([{:deallocate, _} = i | is], acc, st) do
    opt(is, [i | acc], st)
  end

  defp opt([i | is], acc, r_st(labels: used0) = st0) do
    used = ulbl(i, used0)
    st = r_st(st0, labels: used)

    case is_unreachable_after(i) do
      true ->
        skip_unreachable(is, [i | acc], st)

      false ->
        opt(is, [i | acc], st)
    end
  end

  defp opt([], acc, r_st(replace: replace0))
       when replace0 !== %{} do
    replace = normalize_replace(:maps.to_list(replace0), replace0, [])

    :beam_utils.replace_labels(acc, [], replace, fn old ->
      old
    end)
  end

  defp opt([], acc, r_st(replace: replace))
       when replace === %{} do
    reverse(acc)
  end

  defp normalize_replace([{from, to0} | rest], replace, acc) do
    case replace do
      %{^to0 => to} ->
        normalize_replace([{from, to} | rest], replace, acc)

      _ ->
        normalize_replace(rest, replace, [{from, to0} | acc])
    end
  end

  defp normalize_replace([], _Replace, acc) do
    :maps.from_list(acc)
  end

  defp collect_labels(is, label, r_st(entry: entry, replace: replace) = st) do
    collect_labels_1(is, label, entry, replace, st)
  end

  defp collect_labels_1([{:label, entry} | _] = is, _Label, entry, acc, st) do
    {is, r_st(st, replace: acc)}
  end

  defp collect_labels_1([{:label, l} | is], label, entry, acc, st) do
    collect_labels_1(is, label, entry, Map.put(acc, l, label), st)
  end

  defp collect_labels_1(is, _Label, _Entry, acc, st) do
    {is, r_st(st, replace: acc)}
  end

  defp is_label_defined([{:label, l} | _], l) do
    true
  end

  defp is_label_defined([{:label, _} | is], l) do
    is_label_defined(is, l)
  end

  defp is_label_defined(_, _) do
    false
  end

  defp invert_test(:is_ge) do
    :is_lt
  end

  defp invert_test(:is_lt) do
    :is_ge
  end

  defp invert_test(:is_eq) do
    :is_ne
  end

  defp invert_test(:is_ne) do
    :is_eq
  end

  defp invert_test(:is_eq_exact) do
    :is_ne_exact
  end

  defp invert_test(:is_ne_exact) do
    :is_eq_exact
  end

  defp invert_test(_) do
    :not_possible
  end

  defp skip_unreachable([{:label, l} | _Is] = is0, [{:jump, {:f, l}} | acc], st) do
    opt(is0, acc, st)
  end

  defp skip_unreachable([{:label, l} | is] = is0, acc, st) do
    case is_label_used(l, st) do
      true ->
        opt(is0, acc, st)

      false ->
        skip_unreachable(is, acc, st)
    end
  end

  defp skip_unreachable([_ | is], acc, st) do
    skip_unreachable(is, acc, st)
  end

  defp skip_unreachable([], acc, st) do
    opt([], acc, st)
  end

  defp label_used({:f, l}, st) do
    r_st(st, labels: :cerl_sets.add_element(l, r_st(st, :labels)))
  end

  defp label_used([h | t], st0) do
    label_used(t, label_used(h, st0))
  end

  defp label_used([], st) do
    st
  end

  defp label_used(_Other, st) do
    st
  end

  defp is_label_used(l, st) do
    :cerl_sets.is_element(l, r_st(st, :labels))
  end

  defp is_unreachable_after({:func_info, _M, _F, _A}) do
    true
  end

  defp is_unreachable_after(:return) do
    true
  end

  defp is_unreachable_after({:jump, _Lbl}) do
    true
  end

  defp is_unreachable_after({:select, _What, _R, _Lbl, _Cases}) do
    true
  end

  defp is_unreachable_after({:loop_rec_end, _}) do
    true
  end

  defp is_unreachable_after({:wait, _}) do
    true
  end

  defp is_unreachable_after(i) do
    is_exit_instruction(i)
  end

  def is_exit_instruction(:if_end) do
    true
  end

  def is_exit_instruction({:case_end, _}) do
    true
  end

  def is_exit_instruction({:try_case_end, _}) do
    true
  end

  def is_exit_instruction({:badmatch, _}) do
    true
  end

  def is_exit_instruction(_) do
    false
  end

  def remove_unused_labels(is) do
    used0 = initial_labels(is)
    used = foldl(&ulbl/2, used0, is)
    rem_unused(is, used, [])
  end

  defp rem_unused([{:label, lbl} = i | is0], used, [prev | _] = acc) do
    case :cerl_sets.is_element(lbl, used) do
      false ->
        is =
          case is_unreachable_after(prev) do
            true ->
              drop_upto_label(is0)

            false ->
              is0
          end

        rem_unused(is, used, acc)

      true ->
        rem_unused(is0, used, [i | acc])
    end
  end

  defp rem_unused([i | is], used, acc) do
    rem_unused(is, used, [i | acc])
  end

  defp rem_unused([], _, acc) do
    reverse(acc)
  end

  defp initial_labels(is) do
    initial_labels(is, [])
  end

  defp initial_labels([{:line, _} | is], acc) do
    initial_labels(is, acc)
  end

  defp initial_labels([{:label, lbl} | is], acc) do
    initial_labels(is, [lbl | acc])
  end

  defp initial_labels(
         [{:func_info, _, _, _}, {:label, lbl} | _],
         acc
       ) do
    :cerl_sets.from_list([lbl | acc])
  end

  defp drop_upto_label([{:label, _} | _] = is) do
    is
  end

  defp drop_upto_label([_ | is]) do
    drop_upto_label(is)
  end

  defp drop_upto_label([]) do
    []
  end

  defp unshare(is) do
    short = unshare_collect_short(is, %{})
    unshare_short(is, short)
  end

  defp unshare_collect_short([{:label, l}, :return | is], map) do
    unshare_collect_short(is, Map.put(map, l, [:return]))
  end

  defp unshare_collect_short(
         [
           {:label, l},
           {:deallocate, _} = d,
           :return
           | is
         ],
         map
       ) do
    unshare_collect_short(is, Map.put(map, l, [d, :return]))
  end

  defp unshare_collect_short([_ | is], map) do
    unshare_collect_short(is, map)
  end

  defp unshare_collect_short([], map) do
    map
  end

  defp unshare_short([{:jump, {:f, f}} = i | is], map) do
    case map do
      %{^f => seq} ->
        seq ++ unshare_short(is, map)

      %{} ->
        [i | unshare_short(is, map)]
    end
  end

  defp unshare_short([i | is], map) do
    [i | unshare_short(is, map)]
  end

  defp unshare_short([], _Map) do
    []
  end

  defp ulbl(i, used) do
    case instr_labels(i) do
      [] ->
        used

      [lbl] ->
        :cerl_sets.add_element(lbl, used)

      [_ | _] = l ->
        ulbl_list(l, used)
    end
  end

  defp ulbl_list([l | ls], used) do
    ulbl_list(ls, :cerl_sets.add_element(l, used))
  end

  defp ulbl_list([], used) do
    used
  end

  defp instr_labels({:test, _, fail, _}) do
    do_instr_labels(fail)
  end

  defp instr_labels({:test, _, fail, _, _, _}) do
    do_instr_labels(fail)
  end

  defp instr_labels({:select, _, _, fail, vls}) do
    do_instr_labels_list(vls, do_instr_labels(fail))
  end

  defp instr_labels({:try, _, lbl}) do
    do_instr_labels(lbl)
  end

  defp instr_labels({:catch, _, lbl}) do
    do_instr_labels(lbl)
  end

  defp instr_labels({:jump, lbl}) do
    do_instr_labels(lbl)
  end

  defp instr_labels({:loop_rec, lbl, _}) do
    do_instr_labels(lbl)
  end

  defp instr_labels({:loop_rec_end, lbl}) do
    do_instr_labels(lbl)
  end

  defp instr_labels({:wait, lbl}) do
    do_instr_labels(lbl)
  end

  defp instr_labels({:wait_timeout, lbl, _To}) do
    do_instr_labels(lbl)
  end

  defp instr_labels({:bif, _Name, lbl, _As, _R}) do
    do_instr_labels(lbl)
  end

  defp instr_labels({:gc_bif, _Name, lbl, _Live, _As, _R}) do
    do_instr_labels(lbl)
  end

  defp instr_labels({:bs_init, lbl, _, _, _, _}) do
    do_instr_labels(lbl)
  end

  defp instr_labels({:bs_put, lbl, _, _}) do
    do_instr_labels(lbl)
  end

  defp instr_labels({:put_map, lbl, _Op, _Src, _Dst, _Live, _List}) do
    do_instr_labels(lbl)
  end

  defp instr_labels({:get_map_elements, lbl, _Src, _List}) do
    do_instr_labels(lbl)
  end

  defp instr_labels({:recv_mark, lbl}) do
    do_instr_labels(lbl)
  end

  defp instr_labels({:recv_set, lbl}) do
    do_instr_labels(lbl)
  end

  defp instr_labels({:fcheckerror, lbl}) do
    do_instr_labels(lbl)
  end

  defp instr_labels({:bs_start_match4, fail, _, _, _}) do
    case fail do
      {:f, l} ->
        [l]

      {:atom, _} ->
        []
    end
  end

  defp instr_labels(_) do
    []
  end

  defp do_instr_labels({:f, 0}) do
    []
  end

  defp do_instr_labels({:f, f}) do
    [f]
  end

  defp do_instr_labels_list([{:f, f} | t], acc) do
    do_instr_labels_list(t, [f | acc])
  end

  defp do_instr_labels_list([_ | t], acc) do
    do_instr_labels_list(t, acc)
  end

  defp do_instr_labels_list([], acc) do
    acc
  end
end
