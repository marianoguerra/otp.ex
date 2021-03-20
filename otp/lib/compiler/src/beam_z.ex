defmodule :m_beam_z do
  use Bitwise
  import :lists, only: [dropwhile: 2, map: 2]

  def module({mod, exp, attr, fs0, lc}, opts) do
    noGetHdTl = :proplists.get_bool(:no_get_hd_tl, opts)
    noInitYregs = :proplists.get_bool(:no_init_yregs, opts)

    fs =
      for f <- fs0 do
        function(f, noGetHdTl, noInitYregs)
      end

    {:ok, {mod, exp, attr, fs, lc}}
  end

  defp function({:function, name, arity, cLabel, is0}, noGetHdTl, noInitYregs) do
    try do
      is1 = undo_renames(is0)
      is2 = maybe_eliminate_get_hd_tl(is1, noGetHdTl)
      is3 = maybe_eliminate_init_yregs(is2, noInitYregs)
      is = remove_redundant_lines(is3)
      {:function, name, arity, cLabel, is}
    catch
      class, error ->
        :io.fwrite('Function: ~w/~w\n', [name, arity])
        :erlang.raise(class, error, __STACKTRACE__)
    end
  end

  defp undo_renames([{:call_ext, 2, :send} | is]) do
    [:send | undo_renames(is)]
  end

  defp undo_renames([
         {:apply, a},
         {:deallocate, n},
         :return
         | is
       ]) do
    [{:apply_last, a, n} | undo_renames(is)]
  end

  defp undo_renames([{:call, a, f}, {:%, {:var_info, {:x, 0}, _}}, {:deallocate, n}, :return | is]) do
    [{:call_last, a, f, n} | undo_renames(is)]
  end

  defp undo_renames([
         {:call, a, f},
         {:deallocate, n},
         :return
         | is
       ]) do
    [{:call_last, a, f, n} | undo_renames(is)]
  end

  defp undo_renames([
         {:call_ext, a, f},
         {:%, {:var_info, {:x, 0}, _}},
         {:deallocate, n},
         :return | is
       ]) do
    [{:call_ext_last, a, f, n} | undo_renames(is)]
  end

  defp undo_renames([
         {:call_ext, a, f},
         {:deallocate, n},
         :return
         | is
       ]) do
    [{:call_ext_last, a, f, n} | undo_renames(is)]
  end

  defp undo_renames([{:call, a, f}, {:%, {:var_info, {:x, 0}, _}}, :return | is]) do
    [{:call_only, a, f} | undo_renames(is)]
  end

  defp undo_renames([{:call, a, f}, :return | is]) do
    [{:call_only, a, f} | undo_renames(is)]
  end

  defp undo_renames([{:call_ext, a, f}, {:%, {:var_info, {:x, 0}, _}}, :return | is]) do
    [{:call_ext_only, a, f} | undo_renames(is)]
  end

  defp undo_renames([{:call_ext, a, f}, :return | is]) do
    [{:call_ext_only, a, f} | undo_renames(is)]
  end

  defp undo_renames([{:bif, :raise, _, _, _} = i | is0]) do
    is =
      dropwhile(
        fn
          {:label, _} ->
            false

          _ ->
            true
        end,
        is0
      )

    [i | undo_renames(is)]
  end

  defp undo_renames([
         {:get_hd, src, dst1},
         {:get_tl, src, dst2}
         | is
       ]) do
    [{:get_list, src, dst1, dst2} | undo_renames(is)]
  end

  defp undo_renames([
         {:get_tl, src, dst2},
         {:get_hd, src, dst1}
         | is
       ]) do
    [{:get_list, src, dst1, dst2} | undo_renames(is)]
  end

  defp undo_renames([
         {:bs_put, _, {:bs_put_binary, 1, _}, [{:atom, :all}, {:literal, <<>>}]}
         | is
       ]) do
    undo_renames(is)
  end

  defp undo_renames([
         {:bs_put, fail, {:bs_put_binary, 1, _Flags}, [{:atom, :all}, {:literal, binString}]}
         | is0
       ])
       when is_bitstring(binString) do
    bits = bit_size(binString)
    bytes = div(bits, 8)

    case rem(bits, 8) do
      0 ->
        i = {:bs_put_string, byte_size(binString), {:string, binString}}
        [undo_rename(i) | undo_renames(is0)]

      rem ->
        <<binary::size(bytes)-bytes, int::size(rem)>> = binString

        putInt =
          {:bs_put_integer, fail, {:integer, rem}, 1, {:field_flags, [:unsigned, :big]},
           {:integer, int}}

        is = [putInt | undo_renames(is0)]

        case binary do
          <<>> ->
            is

          _ ->
            [
              {:bs_put_string, byte_size(binary), {:string, binary}}
              | is
            ]
        end
    end
  end

  defp undo_renames([i | is]) do
    [undo_rename(i) | undo_renames(is)]
  end

  defp undo_renames([]) do
    []
  end

  defp undo_rename({:bs_put, f, {i, u, fl}, [sz, src]}) do
    {i, f, sz, u, fl, src}
  end

  defp undo_rename({:bs_put, f, {i, fl}, [src]}) do
    {i, f, fl, src}
  end

  defp undo_rename({:bif, :bs_add = i, f, [src1, src2, {:integer, u}], dst}) do
    {i, f, [src1, src2, u], dst}
  end

  defp undo_rename({:bif, :bs_utf8_size = i, f, [src], dst}) do
    {i, f, src, dst}
  end

  defp undo_rename({:bif, :bs_utf16_size = i, f, [src], dst}) do
    {i, f, src, dst}
  end

  defp undo_rename({:bs_init, f, {i, u, flags}, :none, [sz, src], dst}) do
    {i, f, sz, u, src, flags, dst}
  end

  defp undo_rename({:bs_init, f, {i, extra, flags}, live, [sz], dst}) do
    {i, f, sz, extra, live, flags, dst}
  end

  defp undo_rename({:bs_init, f, {i, extra, u, flags}, live, [sz, src], dst}) do
    {i, f, sz, extra, live, u, src, flags, dst}
  end

  defp undo_rename({:bs_init, _, :bs_init_writable = i, _, _, _}) do
    i
  end

  defp undo_rename({:put_map, fail, :assoc, s, d, r, l}) do
    {:put_map_assoc, fail, s, d, r, l}
  end

  defp undo_rename({:put_map, fail, :exact, s, d, r, l}) do
    {:put_map_exact, fail, s, d, r, l}
  end

  defp undo_rename({:test, :has_map_fields, fail, [src | list]}) do
    {:test, :has_map_fields, fail, src, {:list, list}}
  end

  defp undo_rename({:get_map_elements, fail, src, {:list, list}}) do
    {:get_map_elements, fail, src, {:list, list}}
  end

  defp undo_rename({:test, :is_eq_exact, fail, [src, nil]}) do
    {:test, :is_nil, fail, [src]}
  end

  defp undo_rename({:select, i, reg, fail, list}) do
    {i, reg, fail, {:list, list}}
  end

  defp undo_rename(i) do
    i
  end

  defp maybe_eliminate_get_hd_tl(is, true) do
    map(
      fn
        {:get_hd, cons, hd} ->
          {:get_list, cons, hd, {:x, 1022}}

        {:get_tl, cons, tl} ->
          {:get_list, cons, {:x, 1022}, tl}

        i ->
          i
      end,
      is
    )
  end

  defp maybe_eliminate_get_hd_tl(is, false) do
    is
  end

  defp maybe_eliminate_init_yregs(is, true) do
    eliminate_init_yregs(is)
  end

  defp maybe_eliminate_init_yregs(is, false) do
    is
  end

  defp eliminate_init_yregs([
         {:allocate, ns, live},
         {:init_yregs, _}
         | is
       ]) do
    [{:allocate_zero, ns, live} | eliminate_init_yregs(is)]
  end

  defp eliminate_init_yregs([
         {:allocate_heap, ns, nh, live},
         {:init_yregs, _}
         | is
       ]) do
    [
      {:allocate_heap_zero, ns, nh, live}
      | eliminate_init_yregs(is)
    ]
  end

  defp eliminate_init_yregs([{:init_yregs, {:list, yregs}} | is]) do
    inits =
      for y <- yregs do
        {:init, y}
      end

    inits ++ eliminate_init_yregs(is)
  end

  defp eliminate_init_yregs([i | is]) do
    [i | eliminate_init_yregs(is)]
  end

  defp eliminate_init_yregs([]) do
    []
  end

  defp remove_redundant_lines(is) do
    remove_redundant_lines_1(is, :none)
  end

  defp remove_redundant_lines_1([{:line, loc} = i | is], prevLoc) do
    cond do
      loc === prevLoc ->
        remove_redundant_lines_1(is, loc)

      true ->
        [i | remove_redundant_lines_1(is, loc)]
    end
  end

  defp remove_redundant_lines_1([i | is], prevLoc) do
    [i | remove_redundant_lines_1(is, prevLoc)]
  end

  defp remove_redundant_lines_1([], _) do
    []
  end
end
