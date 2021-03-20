defmodule :m_beam_a do
  use Bitwise

  def module({mod, exp, attr, fs0, lc}, _Opt) do
    fs =
      for f <- fs0 do
        function(f)
      end

    {:ok, {mod, exp, attr, fs, lc}}
  end

  defp function({:function, name, arity, cLabel, is0}) do
    try do
      is1 = rename_instrs(is0)
      is2 = :beam_jump.remove_unused_labels(is1)
      is = coalesce_consecutive_labels(is2, [], [])
      {:function, name, arity, cLabel, is}
    catch
      class, error ->
        :io.fwrite('Function: ~w/~w\n', [name, arity])
        :erlang.raise(class, error, __STACKTRACE__)
    end
  end

  defp rename_instrs([{:test, :is_eq_exact, _, [dst, src]} = test, {:move, src, dst} | is]) do
    rename_instrs([test | is])
  end

  defp rename_instrs([
         {:test, :is_eq_exact, _, [same, same]}
         | is
       ]) do
    rename_instrs(is)
  end

  defp rename_instrs([{:apply_last, a, n} | is]) do
    [
      {:apply, a},
      {:deallocate, n},
      :return
      | rename_instrs(is)
    ]
  end

  defp rename_instrs([{:call_last, a, f, n} | is]) do
    [
      {:call, a, f},
      {:deallocate, n},
      :return
      | rename_instrs(is)
    ]
  end

  defp rename_instrs([{:call_ext_last, a, f, n} | is]) do
    [
      {:call_ext, a, f},
      {:deallocate, n},
      :return
      | rename_instrs(is)
    ]
  end

  defp rename_instrs([{:call_only, a, f} | is]) do
    [{:call, a, f}, :return | rename_instrs(is)]
  end

  defp rename_instrs([{:call_ext_only, a, f} | is]) do
    [{:call_ext, a, f}, :return | rename_instrs(is)]
  end

  defp rename_instrs([{:"%live", _} | is]) do
    rename_instrs(is)
  end

  defp rename_instrs([{:get_list, s, d1, d2} | is]) do
    cond do
      d1 === s ->
        [{:get_tl, s, d2}, {:get_hd, s, d1} | rename_instrs(is)]

      true ->
        [{:get_hd, s, d1}, {:get_tl, s, d2} | rename_instrs(is)]
    end
  end

  defp rename_instrs([i | is]) do
    [rename_instr(i) | rename_instrs(is)]
  end

  defp rename_instrs([]) do
    []
  end

  defp rename_instr({:bs_put_binary = i, f, sz, u, fl, src}) do
    {:bs_put, f, {i, u, fl}, [sz, src]}
  end

  defp rename_instr({:bs_put_float = i, f, sz, u, fl, src}) do
    {:bs_put, f, {i, u, fl}, [sz, src]}
  end

  defp rename_instr({:bs_put_integer = i, f, sz, u, fl, src}) do
    {:bs_put, f, {i, u, fl}, [sz, src]}
  end

  defp rename_instr({:bs_put_utf8 = i, f, fl, src}) do
    {:bs_put, f, {i, fl}, [src]}
  end

  defp rename_instr({:bs_put_utf16 = i, f, fl, src}) do
    {:bs_put, f, {i, fl}, [src]}
  end

  defp rename_instr({:bs_put_utf32 = i, f, fl, src}) do
    {:bs_put, f, {i, fl}, [src]}
  end

  defp rename_instr({:bs_put_string, _, {:string, string}}) do
    {:bs_put, {:f, 0}, {:bs_put_binary, 8, {:field_flags, [:unsigned, :big]}},
     [{:atom, :all}, {:literal, :erlang.iolist_to_binary([string])}]}
  end

  defp rename_instr({:bs_add = i, f, [src1, src2, u], dst})
       when is_integer(u) do
    {:bif, i, f, [src1, src2, {:integer, u}], dst}
  end

  defp rename_instr({:bs_utf8_size = i, f, src, dst}) do
    {:bif, i, f, [src], dst}
  end

  defp rename_instr({:bs_utf16_size = i, f, src, dst}) do
    {:bif, i, f, [src], dst}
  end

  defp rename_instr({:bs_init2 = i, f, sz, extra, live, flags, dst}) do
    {:bs_init, f, {i, extra, flags}, live, [sz], dst}
  end

  defp rename_instr({:bs_init_bits = i, f, sz, extra, live, flags, dst}) do
    {:bs_init, f, {i, extra, flags}, live, [sz], dst}
  end

  defp rename_instr({:bs_append = i, f, sz, extra, live, u, src, flags, dst}) do
    {:bs_init, f, {i, extra, u, flags}, live, [sz, src], dst}
  end

  defp rename_instr({:bs_private_append = i, f, sz, u, src, flags, dst}) do
    {:bs_init, f, {i, u, flags}, :none, [sz, src], dst}
  end

  defp rename_instr(:bs_init_writable = i) do
    {:bs_init, {:f, 0}, i, 1, [{:x, 0}], {:x, 0}}
  end

  defp rename_instr({:put_map_assoc, fail, s, d, r, l}) do
    {:put_map, fail, :assoc, s, d, r, l}
  end

  defp rename_instr({:put_map_exact, fail, s, d, r, l}) do
    {:put_map, fail, :exact, s, d, r, l}
  end

  defp rename_instr({:test, :has_map_fields, fail, src, {:list, list}}) do
    {:test, :has_map_fields, fail, [src | list]}
  end

  defp rename_instr({:test, :is_nil, fail, [src]}) do
    {:test, :is_eq_exact, fail, [src, nil]}
  end

  defp rename_instr({:select_val = i, reg, fail, {:list, list}}) do
    {:select, i, reg, fail, list}
  end

  defp rename_instr({:select_tuple_arity = i, reg, fail, {:list, list}}) do
    {:select, i, reg, fail, list}
  end

  defp rename_instr(:send) do
    {:call_ext, 2, :send}
  end

  defp rename_instr(i) do
    i
  end

  defp coalesce_consecutive_labels([{:label, l} = lbl, {:label, alias} | is], replace, acc) do
    coalesce_consecutive_labels([lbl | is], [{alias, l} | replace], acc)
  end

  defp coalesce_consecutive_labels([i | is], replace, acc) do
    coalesce_consecutive_labels(is, replace, [i | acc])
  end

  defp coalesce_consecutive_labels([], replace, acc) do
    d = :maps.from_list(replace)

    :beam_utils.replace_labels(acc, [], d, fn l ->
      l
    end)
  end
end
