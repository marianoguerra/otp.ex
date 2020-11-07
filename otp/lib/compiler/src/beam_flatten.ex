defmodule :m_beam_flatten do
  use Bitwise
  import :lists, only: [reverse: 1, reverse: 2]

  def module({mod, exp, attr, fs, lc}, _Opt) do
    {:ok,
     {mod, exp, attr,
      for f <- fs do
        function(f)
      end, lc}}
  end

  defp function({:function, name, arity, cLabel, is0}) do
    is = block(is0)
    {:function, name, arity, cLabel, is}
  end

  defp block(is) do
    block(is, [])
  end

  defp block([{:block, is0} | is1], acc) do
    block(is1, norm_block(is0, acc))
  end

  defp block([i | is], acc) do
    block(is, [i | acc])
  end

  defp block([], acc) do
    reverse(acc)
  end

  defp norm_block(
         [{:set, [], [], {:alloc, r, alloc}} | is],
         acc0
       ) do
    norm_block(is, reverse(norm_allocate(alloc, r), acc0))
  end

  defp norm_block([i | is], acc) do
    norm_block(is, [norm(i) | acc])
  end

  defp norm_block([], acc) do
    acc
  end

  defp norm({:set, [d], as, {:bif, n, f}}) do
    {:bif, n, f, as, d}
  end

  defp norm({:set, [d], as, {:alloc, r, {:gc_bif, n, f}}}) do
    {:gc_bif, n, f, r, as, d}
  end

  defp norm({:set, [d], [], :init}) do
    {:init, d}
  end

  defp norm({:set, [d], [s], :move}) do
    {:move, s, d}
  end

  defp norm({:set, [d], [s], :fmove}) do
    {:fmove, s, d}
  end

  defp norm({:set, [d], [s], :fconv}) do
    {:fconv, s, d}
  end

  defp norm({:set, [d], [s1, s2], :put_list}) do
    {:put_list, s1, s2, d}
  end

  defp norm({:set, [d], els, :put_tuple2}) do
    {:put_tuple2, d, {:list, els}}
  end

  defp norm({:set, [d], [], {:put_tuple, a}}) do
    {:put_tuple, a, d}
  end

  defp norm({:set, [], [s], :put}) do
    {:put, s}
  end

  defp norm({:set, [d], [s], {:get_tuple_element, i}}) do
    {:get_tuple_element, s, i, d}
  end

  defp norm({:set, [], [s, d], {:set_tuple_element, i}}) do
    {:set_tuple_element, s, d, i}
  end

  defp norm({:set, [d], [s], :get_hd}) do
    {:get_hd, s, d}
  end

  defp norm({:set, [d], [s], :get_tl}) do
    {:get_tl, s, d}
  end

  defp norm({:set, [d], [s | puts], {:alloc, r, {:put_map, op, f}}}) do
    {:put_map, f, op, s, d, r, {:list, puts}}
  end

  defp norm({:set, [], [], :remove_message}) do
    :remove_message
  end

  defp norm({:set, [], [], :fclearerror}) do
    :fclearerror
  end

  defp norm({:set, [], [], :fcheckerror}) do
    {:fcheckerror, {:f, 0}}
  end

  defp norm({:set, [], [], {:line, _} = line}) do
    line
  end

  defp norm_allocate({_Zero, :nostack, nh, []}, regs) do
    [{:test_heap, nh, regs}]
  end

  defp norm_allocate({:nozero, ns, 0, inits}, regs) do
    [{:allocate, ns, regs} | inits]
  end

  defp norm_allocate({:nozero, ns, nh, inits}, regs) do
    [{:allocate_heap, ns, nh, regs} | inits]
  end
end
