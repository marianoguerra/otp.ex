defmodule :m_beam_peep do
  use Bitwise
  import :lists, only: [member: 2, reverse: 1, usort: 1]

  def module({mod, exp, attr, fs0, _}, _Opts) do
    {fs1, lc} = :beam_clean.clean_labels(fs0)

    fs =
      for f <- fs1 do
        function(f)
      end

    {:ok, {mod, exp, attr, fs, lc}}
  end

  defp function({:function, name, arity, cLabel, is0}) do
    try do
      is1 = peep(is0)
      is = :beam_jump.remove_unused_labels(is1)
      {:function, name, arity, cLabel, is}
    catch
      class, error ->
        :io.fwrite('Function: ~w/~w\n', [name, arity])
        :erlang.raise(class, error, __STACKTRACE__)
    end
  end

  defp peep(is) do
    peep(is, :gb_sets.empty(), [])
  end

  defp peep(
         [
           {:bif, :tuple_size, _, [_] = ops, dst} = i
           | is
         ],
         seenTests0,
         acc
       ) do
    seenTests1 = :gb_sets.add({:is_tuple, ops}, seenTests0)
    seenTests = kill_seen(dst, seenTests1)
    peep(is, seenTests, [i | acc])
  end

  defp peep([{:bif, :map_get, _, [key, map], dst} = i | is], seenTests0, acc) do
    seenTests1 =
      :gb_sets.add(
        {:has_map_fields, [map, key]},
        seenTests0
      )

    seenTests = kill_seen(dst, seenTests1)
    peep(is, seenTests, [i | acc])
  end

  defp peep([{:bif, _, _, _, dst} = i | is], seenTests0, acc) do
    seenTests = kill_seen(dst, seenTests0)
    peep(is, seenTests, [i | acc])
  end

  defp peep([{:gc_bif, _, _, _, _, dst} = i | is], seenTests0, acc) do
    seenTests = kill_seen(dst, seenTests0)
    peep(is, seenTests, [i | acc])
  end

  defp peep([[{:jump, {:f, l}}, {:label, l} = i] | is], _, acc) do
    peep(is, :gb_sets.empty(), [i | acc])
  end

  defp peep([{:select, :select_val, r, f, vls0} | is], seenTests0, acc0) do
    case prune_redundant_values(vls0, f) do
      [] ->
        i = {:jump, f}
        peep([i | is], :gb_sets.empty(), acc0)

      [{:atom, _} = value, lbl] ->
        is1 = [
          [{:test, :is_eq_exact, f, [r, value]}, {:jump, lbl}]
          | is
        ]

        peep(is1, seenTests0, acc0)

      [{:integer, _} = value, lbl] ->
        is1 = [
          [{:test, :is_eq_exact, f, [r, value]}, {:jump, lbl}]
          | is
        ]

        peep(is1, seenTests0, acc0)

      [{:atom, b1}, lbl, {:atom, b2}, lbl] when b1 === not b2 ->
        is1 = [
          [{:test, :is_boolean, f, [r]}, {:jump, lbl}]
          | is
        ]

        peep(is1, seenTests0, acc0)

      [_ | _] = vls ->
        i = {:select, :select_val, r, f, vls}
        peep(is, :gb_sets.empty(), [i | acc0])
    end
  end

  defp peep([{:get_map_elements, fail, src, list} = i | is], _SeenTests, acc0) do
    seenTests = :gb_sets.empty()

    case simplify_get_map_elements(fail, src, list, acc0) do
      {:ok, acc} ->
        peep(is, seenTests, acc)

      :error ->
        peep(is, seenTests, [i | acc0])
    end
  end

  defp peep([{:test, :has_map_fields, fail, ops} = i | is], seenTests, acc0) do
    case simplify_has_map_fields(fail, ops, acc0) do
      {:ok, acc} ->
        peep(is, seenTests, acc)

      :error ->
        peep(is, seenTests, [i | acc0])
    end
  end

  defp peep([{:test, op, _, ops} = i | is], seenTests0, acc) do
    case :beam_utils.is_pure_test(i) do
      false ->
        peep(is, :gb_sets.empty(), [i | acc])

      true ->
        case is_test_redundant(op, ops, seenTests0) do
          true ->
            peep(is, seenTests0, acc)

          false ->
            test = {op, ops}
            seenTests = :gb_sets.insert(test, seenTests0)
            peep(is, seenTests, [i | acc])
        end
    end
  end

  defp peep([i | is], _, acc) do
    peep(is, :gb_sets.empty(), [i | acc])
  end

  defp peep([], _, acc) do
    reverse(acc)
  end

  defp is_test_redundant(op, ops, seen) do
    :gb_sets.is_element(
      {op, ops},
      seen
    ) or is_test_redundant_1(op, ops, seen)
  end

  defp is_test_redundant_1(:is_boolean, [r], seen) do
    :gb_sets.is_element(
      {:is_eq_exact, [r, {:atom, false}]},
      seen
    ) or
      :gb_sets.is_element(
        {:is_eq_exact, [r, {:atom, true}]},
        seen
      )
  end

  defp is_test_redundant_1(_, _, _) do
    false
  end

  defp kill_seen(dst, seen0) do
    :gb_sets.from_ordset(
      kill_seen_1(
        :gb_sets.to_list(seen0),
        dst
      )
    )
  end

  defp kill_seen_1([{_, ops} = test | t], dst) do
    case member(dst, ops) do
      true ->
        kill_seen_1(t, dst)

      false ->
        [test | kill_seen_1(t, dst)]
    end
  end

  defp kill_seen_1([], _) do
    []
  end

  defp prune_redundant_values([[_Val, f] | vls], f) do
    prune_redundant_values(vls, f)
  end

  defp prune_redundant_values([[val, lbl] | vls], f) do
    [[val, lbl] | prune_redundant_values(vls, f)]
  end

  defp prune_redundant_values([], _) do
    []
  end

  defp simplify_get_map_elements(fail, src, {:list, [key, dst]}, [
         {:get_map_elements, fail, src, {:list, list1}}
         | acc
       ]) do
    case are_keys_literals([key]) and are_keys_literals(list1) and
           not is_source_overwritten(
             src,
             list1
           ) do
      true ->
        case member(key, list1) do
          true ->
            :error

          false ->
            list = [[key, dst] | list1]
            {:ok, [{:get_map_elements, fail, src, {:list, list}} | acc]}
        end

      false ->
        :error
    end
  end

  defp simplify_get_map_elements(_, _, _, _) do
    :error
  end

  defp simplify_has_map_fields(fail, [src | keys0], [
         {:test, :has_map_fields, fail, [src | keys1]}
         | acc
       ]) do
    case are_keys_literals(keys0) and are_keys_literals(keys1) do
      true ->
        keys = usort(keys0 ++ keys1)
        {:ok, [{:test, :has_map_fields, fail, [src | keys]} | acc]}

      false ->
        :error
    end
  end

  defp simplify_has_map_fields(_, _, _) do
    :error
  end

  defp are_keys_literals([{:x, _} | _]) do
    false
  end

  defp are_keys_literals([{:y, _} | _]) do
    false
  end

  defp are_keys_literals([_ | _]) do
    true
  end

  defp is_source_overwritten(src, [_Key, src]) do
    true
  end

  defp is_source_overwritten(_, _) do
    false
  end
end
