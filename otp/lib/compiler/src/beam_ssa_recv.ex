defmodule :m_beam_ssa_recv do
  use Bitwise
  import :lists, only: [all: 2, reverse: 2]
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

  def module(r_b_module(body: fs0) = module, _Opts) do
    fs =
      for f <- fs0 do
        function(f)
      end

    {:ok, r_b_module(module, body: fs)}
  end

  defp function(r_b_function(anno: anno, bs: blocks0) = f) do
    try do
      blocks = opt(blocks0)
      r_b_function(f, bs: blocks)
    catch
      class, error ->
        %{:func_info => {_, name, arity}} = anno
        :io.fwrite('Function: ~w/~w\n', [name, arity])
        :erlang.raise(class, error, __STACKTRACE__)
    end
  end

  defp opt(blocks) do
    linear = :beam_ssa.linearize(blocks)
    opt(linear, blocks, [])
  end

  defp opt(
         [
           {l, r_b_blk(is: [r_b_set(op: :peek_message) | _]) = blk0}
           | bs
         ],
         blocks0,
         preds
       ) do
    case recv_opt(preds, l, blocks0) do
      {:yes, blocks1} ->
        blk = :beam_ssa.add_anno(:recv_set, l, blk0)
        blocks = %{blocks1 | l => blk}
        opt(bs, blocks, [])

      :no ->
        opt(bs, blocks0, [])
    end
  end

  defp opt([{l, _} | bs], blocks, preds) do
    opt(bs, blocks, [l | preds])
  end

  defp opt([], blocks, _) do
    blocks
  end

  defp recv_opt([l | ls], recvLbl, blocks) do
    r_b_blk(is: is0) = blk0 = :erlang.map_get(l, blocks)

    case recv_opt_is(l, {is0, []}, recvLbl, blocks) do
      {:yes, is} ->
        blk = r_b_blk(blk0, is: is)
        {:yes, %{blocks | l => blk}}

      :no ->
        recv_opt(ls, recvLbl, blocks)
    end
  end

  defp recv_opt([], _, _Blocks) do
    :no
  end

  defp recv_opt_is(l, {is0, preIs0}, recvLbl, blocks) do
    case recv_opt_makes_ref(is0, recvLbl, blocks, preIs0) do
      {:yes, ref, [i0 | is], preIs} ->
        tempBlk = r_b_blk(:erlang.map_get(l, blocks), is: is)
        tempBlocks = %{blocks | l => tempBlk}

        case opt_ref_used(l, ref, tempBlocks) do
          true ->
            i = :beam_ssa.add_anno(:recv_mark, recvLbl, i0)
            {:yes, reverse(preIs, [i | is])}

          false ->
            recv_opt_is(l, {is, [i0 | preIs]}, recvLbl, blocks)
        end

      :no ->
        :no
    end
  end

  defp recv_opt_makes_ref([r_b_set(op: :call) = i0 | is], recvLbl, blocks0, acc) do
    case makes_ref(i0, blocks0) do
      :no ->
        recv_opt_makes_ref(is, recvLbl, blocks0, [i0 | acc])

      {:yes, ref} ->
        {:yes, ref, [i0 | is], acc}
    end
  end

  defp recv_opt_makes_ref([i | is], recvLbl, blocks, acc) do
    recv_opt_makes_ref(is, recvLbl, blocks, [i | acc])
  end

  defp recv_opt_makes_ref([], _, _, _) do
    :no
  end

  defp makes_ref(r_b_set(dst: dst, args: [func0 | _]), blocks) do
    mFA =
      case func0 do
        r_b_remote(mod: r_b_literal(val: mod), name: r_b_literal(val: func), arity: a0) ->
          {mod, func, a0}

        _ ->
          :none
      end

    case mFA do
      {:erlang, :make_ref, 0} ->
        {:yes, dst}

      {:erlang, :monitor, 2} ->
        {:yes, dst}

      {:erlang, :spawn_request, a} when 1 <= a and a <= 5 ->
        {:yes, dst}

      {:erlang, :spawn_monitor, a} when 1 <= a and a <= 4 ->
        ref_in_tuple(dst, blocks)

      _ ->
        :no
    end
  end

  defp ref_in_tuple(tuple, blocks) do
    f = fn
      r_b_set(op: :get_tuple_element, dst: ref, args: [r_b_var() = tup, r_b_literal(val: 1)]), :no
      when tup === tuple ->
        {:yes, ref}

      _, a ->
        a
    end

    :beam_ssa.fold_instrs_rpo(f, [0], :no, blocks)
  end

  defp opt_ref_used(l, ref, blocks) do
    vs = %{ref => :ref, :ref => ref, :ref_matched => false}

    case opt_ref_used_1(l, vs, blocks) do
      :used ->
        true

      :not_used ->
        false

      :done ->
        false
    end
  end

  defp opt_ref_used_1(l, vs0, blocks) do
    r_b_blk(is: is) = blk = :erlang.map_get(l, blocks)

    case opt_ref_used_is(is, vs0) do
      %{} = vs ->
        opt_ref_used_last(blk, vs, blocks)

      result ->
        result
    end
  end

  defp opt_ref_used_is([r_b_set(op: :peek_message, dst: msg) | is], vs0) do
    vs = %{vs0 | msg => :message}
    opt_ref_used_is(is, vs)
  end

  defp opt_ref_used_is(
         [
           r_b_set(op: {:bif, bif}, args: args, dst: dst) = i
           | is
         ],
         vs0
       ) do
    s =
      case bif do
        :"=:=" ->
          true

        :== ->
          true

        _ ->
          :none
      end

    case s do
      :none ->
        vs = update_vars(i, vs0)
        opt_ref_used_is(is, vs)

      bool when is_boolean(bool) ->
        case is_ref_msg_comparison(args, vs0) do
          true ->
            vs = %{vs0 | dst => {:is_ref, bool}}
            opt_ref_used_is(is, vs)

          false ->
            opt_ref_used_is(is, vs0)
        end
    end
  end

  defp opt_ref_used_is([r_b_set(op: :remove_message) | _], vs) do
    case vs do
      %{:ref_matched => true} ->
        :used

      %{:ref_matched => false} ->
        :not_used
    end
  end

  defp opt_ref_used_is([r_b_set(op: :recv_next) | _], _Vs) do
    :done
  end

  defp opt_ref_used_is([r_b_set(op: :wait_timeout) | _], _Vs) do
    :done
  end

  defp opt_ref_used_is([r_b_set(op: :wait) | _], _Vs) do
    :done
  end

  defp opt_ref_used_is([r_b_set(op: :landingpad) | _], _Vs) do
    :done
  end

  defp opt_ref_used_is(
         [
           r_b_set(
             op: :call,
             args: [
               r_b_remote(mod: r_b_literal(val: mod), name: r_b_literal(val: name))
               | args
             ]
           ) = i
           | is
         ],
         vs0
       ) do
    case :erl_bifs.is_exit_bif(mod, name, length(args)) do
      true ->
        :done

      false ->
        vs = update_vars(i, vs0)
        opt_ref_used_is(is, vs)
    end
  end

  defp opt_ref_used_is([r_b_set(op: :timeout) | _], _Vs) do
    :done
  end

  defp opt_ref_used_is([r_b_set() = i | is], vs0) do
    vs = update_vars(i, vs0)
    opt_ref_used_is(is, vs)
  end

  defp opt_ref_used_is([], vs) do
    vs
  end

  defp opt_ref_used_last(r_b_blk(last: last) = blk, vs, blocks) do
    case last do
      r_b_br(bool: r_b_var() = bool, succ: succ, fail: fail) ->
        case vs do
          %{^bool => {:is_ref, matched}} ->
            ref_used_in(
              [
                {succ, %{vs | :ref_matched => matched}},
                {fail, %{vs | :ref_matched => not matched}}
              ],
              blocks
            )

          %{} ->
            ref_used_in([{succ, vs}, {fail, vs}], blocks)
        end

      r_b_ret() ->
        :not_used

      _ ->
        succVs =
          for succ <- :beam_ssa.successors(blk) do
            {succ, vs}
          end

        ref_used_in(succVs, blocks)
    end
  end

  defp ref_used_in([{l, vs0} | ls], blocks) do
    case opt_ref_used_1(l, vs0, blocks) do
      :not_used ->
        :not_used

      :used ->
        case ref_used_in(ls, blocks) do
          :done ->
            :used

          result ->
            result
        end

      :done ->
        ref_used_in(ls, blocks)
    end
  end

  defp ref_used_in([], _) do
    :done
  end

  defp update_vars(r_b_set(args: args, dst: dst), vs) do
    vars =
      for r_b_var() = v <- args do
        v
      end

    all =
      all(
        fn var ->
          case vs do
            %{^var => :message} ->
              true

            %{} ->
              false
          end
        end,
        vars
      )

    case {vars, all} do
      {[_ | _], true} ->
        %{vs | dst => :message}

      {_, _} ->
        vs
    end
  end

  defp is_ref_msg_comparison([r_b_var() = v1, r_b_var() = v2], vs) do
    case vs do
      %{^v1 => :ref, ^v2 => :message} ->
        true

      %{^v1 => :message, ^v2 => :ref} ->
        true

      %{} ->
        false
    end
  end

  defp is_ref_msg_comparison(_, _) do
    false
  end
end
