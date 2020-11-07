defmodule :m_beam_block do
  use Bitwise
  import :lists, only: [keysort: 2, reverse: 1, splitwith: 2]

  def module({mod, exp, attr, fs0, lc}, _Opts) do
    fs =
      for f <- fs0 do
        function(f)
      end

    {:ok, {mod, exp, attr, fs, lc}}
  end

  defp function({:function, name, arity, cLabel, is0}) do
    try do
      is1 = swap_opt(is0)
      is2 = blockify(is1)
      is = embed_lines(is2)
      {:function, name, arity, cLabel, is}
    catch
      class, error ->
        :io.fwrite('Function: ~w/~w\n', [name, arity])
        :erlang.raise(class, error, __STACKTRACE__)
    end
  end

  defp swap_opt([
         [
           {:move, reg1, {:x, x} = temp} = move1,
           {:move, reg2, reg1} = move2,
           {:move, temp, reg2} = move3
         ]
         | is
       ])
       when reg1 !== temp do
    case is_unused(x, is) do
      true ->
        [{:swap, reg1, reg2} | swap_opt(is)]

      false ->
        [move1 | swap_opt([[move2, move3] | is])]
    end
  end

  defp swap_opt([i | is]) do
    [i | swap_opt(is)]
  end

  defp swap_opt([]) do
    []
  end

  defp is_unused(x, [{:call, a, _} | _]) when a <= x do
    true
  end

  defp is_unused(x, [{:call_ext, a, _} | _]) when a <= x do
    true
  end

  defp is_unused(x, [{:make_fun2, _, _, _, a} | _])
       when a <= x do
    true
  end

  defp is_unused(
         x,
         [{:make_fun3, _, _, _, dst, {:list, env}} | is]
       ) do
    not :lists.member({:x, x}, env) and ({:x, x} === dst or is_unused(x, is))
  end

  defp is_unused(x, [{:move, src, dst} | is]) do
    case {src, dst} do
      {{:x, ^x}, _} ->
        false

      {_, {:x, ^x}} ->
        true

      {_, _} ->
        is_unused(x, is)
    end
  end

  defp is_unused(x, [{:line, _} | is]) do
    is_unused(x, is)
  end

  defp is_unused(_, _) do
    false
  end

  defp blockify(is) do
    blockify(is, [])
  end

  defp blockify([i | is0] = isAll, acc) do
    case collect(i) do
      :error ->
        blockify(is0, [i | acc])

      instr when is_tuple(instr) ->
        {block0, is} = collect_block(isAll)
        block = sort_moves(block0)
        blockify(is, [{:block, block} | acc])
    end
  end

  defp blockify([], acc) do
    reverse(acc)
  end

  defp collect_block(is) do
    collect_block(is, [])
  end

  defp collect_block([{:allocate, n, r} | is0], acc) do
    {inits, is} =
      splitwith(
        fn
          {:init, {:y, _}} ->
            true

          _ ->
            false
        end,
        is0
      )

    collect_block(
      is,
      [
        {:set, [], [], {:alloc, r, {:nozero, n, 0, inits}}}
        | acc
      ]
    )
  end

  defp collect_block([i | is] = is0, acc) do
    case collect(i) do
      :error ->
        {reverse(acc), is0}

      instr ->
        collect_block(is, [instr | acc])
    end
  end

  defp collect_block([], acc) do
    {reverse(acc), []}
  end

  defp collect({:allocate, n, r}) do
    {:set, [], [], {:alloc, r, {:nozero, n, 0, []}}}
  end

  defp collect({:allocate_heap, ns, nh, r}) do
    {:set, [], [], {:alloc, r, {:nozero, ns, nh, []}}}
  end

  defp collect({:init, d}) do
    {:set, [d], [], :init}
  end

  defp collect({:test_heap, n, r}) do
    {:set, [], [], {:alloc, r, {:nozero, :nostack, n, []}}}
  end

  defp collect({:bif, n, {:f, 0}, as, d}) do
    {:set, [d], as, {:bif, n, {:f, 0}}}
  end

  defp collect({:gc_bif, n, {:f, 0}, r, as, d}) do
    {:set, [d], as, {:alloc, r, {:gc_bif, n, {:f, 0}}}}
  end

  defp collect({:move, s, d}) do
    {:set, [d], [s], :move}
  end

  defp collect({:put_list, s1, s2, d}) do
    {:set, [d], [s1, s2], :put_list}
  end

  defp collect({:put_tuple, a, d}) do
    {:set, [d], [], {:put_tuple, a}}
  end

  defp collect({:put, s}) do
    {:set, [], [s], :put}
  end

  defp collect({:put_tuple2, d, {:list, els}}) do
    {:set, [d], els, :put_tuple2}
  end

  defp collect({:get_tuple_element, s, i, d}) do
    {:set, [d], [s], {:get_tuple_element, i}}
  end

  defp collect({:set_tuple_element, s, d, i}) do
    {:set, [], [s, d], {:set_tuple_element, i}}
  end

  defp collect({:get_hd, s, d}) do
    {:set, [d], [s], :get_hd}
  end

  defp collect({:get_tl, s, d}) do
    {:set, [d], [s], :get_tl}
  end

  defp collect(:remove_message) do
    {:set, [], [], :remove_message}
  end

  defp collect({:put_map, {:f, 0}, op, s, d, r, {:list, puts}}) do
    {:set, [d], [s | puts], {:alloc, r, {:put_map, op, {:f, 0}}}}
  end

  defp collect(:fclearerror) do
    {:set, [], [], :fclearerror}
  end

  defp collect({:fcheckerror, {:f, 0}}) do
    {:set, [], [], :fcheckerror}
  end

  defp collect({:fmove, s, d}) do
    {:set, [d], [s], :fmove}
  end

  defp collect({:fconv, s, d}) do
    {:set, [d], [s], :fconv}
  end

  defp collect(_) do
    :error
  end

  defp embed_lines(is) do
    embed_lines(reverse(is), [])
  end

  defp embed_lines(
         [
           [{:block, b2}, {:line, _} = line, {:block, b1}]
           | t
         ],
         acc
       ) do
    b = {:block, b1 ++ [{:set, [], [], line}] ++ b2}
    embed_lines([b | t], acc)
  end

  defp embed_lines([[{:block, b1}, {:line, _} = line] | t], acc) do
    b = {:block, [{:set, [], [], line} | b1]}
    embed_lines([b | t], acc)
  end

  defp embed_lines([i | is], acc) do
    embed_lines(is, [i | acc])
  end

  defp embed_lines([], acc) do
    acc
  end

  defp sort_moves([
         {:set, [{:x, _}], [{:y, _}], :move} = i
         | is0
       ]) do
    {moves, is} = sort_moves_1(is0, :x, :y, [i])
    moves ++ sort_moves(is)
  end

  defp sort_moves([
         {:set, [{:y, _}], [{:x, _}], :move} = i
         | is0
       ]) do
    {moves, is} = sort_moves_1(is0, :y, :x, [i])
    moves ++ sort_moves(is)
  end

  defp sort_moves([i | is]) do
    [i | sort_moves(is)]
  end

  defp sort_moves([]) do
    []
  end

  defp sort_moves_1([{:set, [{:x, 0}], [_], :move} = i | is], _DTag, _STag, acc) do
    {sort_on_yreg(acc) ++ [i], is}
  end

  defp sort_moves_1(
         [
           {:set, [{dTag, _}], [{sTag, _}], :move} = i
           | is
         ],
         dTag,
         sTag,
         acc
       ) do
    sort_moves_1(is, dTag, sTag, [i | acc])
  end

  defp sort_moves_1(is, _DTag, _STag, acc) do
    {sort_on_yreg(acc), is}
  end

  defp sort_on_yreg([{:set, [dst], [src], :move} | _] = moves) do
    case {dst, src} do
      {{:y, _}, {:x, _}} ->
        keysort(2, moves)

      {{:x, _}, {:y, _}} ->
        keysort(3, moves)
    end
  end
end
