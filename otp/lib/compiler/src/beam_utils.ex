defmodule :m_beam_utils do
  use Bitwise
  import :lists, only: [map: 2, reverse: 1]

  def replace_labels(is, acc, d, fb) do
    replace_labels_1(is, acc, d, fb)
  end

  def is_pure_test({:test, :is_eq, _, [_, _]}) do
    true
  end

  def is_pure_test({:test, :is_ne, _, [_, _]}) do
    true
  end

  def is_pure_test({:test, :is_eq_exact, _, [_, _]}) do
    true
  end

  def is_pure_test({:test, :is_ne_exact, _, [_, _]}) do
    true
  end

  def is_pure_test({:test, :is_ge, _, [_, _]}) do
    true
  end

  def is_pure_test({:test, :is_lt, _, [_, _]}) do
    true
  end

  def is_pure_test({:test, :is_nonempty_list, _, [_]}) do
    true
  end

  def is_pure_test({:test, :is_tagged_tuple, _, [_, _, _]}) do
    true
  end

  def is_pure_test({:test, :test_arity, _, [_, _]}) do
    true
  end

  def is_pure_test({:test, :has_map_fields, _, [_ | _]}) do
    true
  end

  def is_pure_test({:test, :is_bitstr, _, [_]}) do
    true
  end

  def is_pure_test({:test, :is_function2, _, [_, _]}) do
    true
  end

  def is_pure_test({:test, op, _, ops}) do
    :erl_internal.new_type_test(op, length(ops))
  end

  def split_even(rs) do
    split_even(rs, [], [])
  end

  defp replace_labels_1([{:test, test, {:f, lbl}, ops} | is], acc, d, fb) do
    i = {:test, test, {:f, label(lbl, d, fb)}, ops}
    replace_labels_1(is, [i | acc], d, fb)
  end

  defp replace_labels_1([{:test, test, {:f, lbl}, live, ops, dst} | is], acc, d, fb) do
    i = {:test, test, {:f, label(lbl, d, fb)}, live, ops, dst}
    replace_labels_1(is, [i | acc], d, fb)
  end

  defp replace_labels_1([{:select, i, r, {:f, fail0}, vls0} | is], acc, d, fb) do
    vls =
      map(
        fn
          {:f, l} ->
            {:f, label(l, d, fb)}

          other ->
            other
        end,
        vls0
      )

    fail = label(fail0, d, fb)
    replace_labels_1(is, [{:select, i, r, {:f, fail}, vls} | acc], d, fb)
  end

  defp replace_labels_1([{:try, r, {:f, lbl}} | is], acc, d, fb) do
    replace_labels_1(is, [{:try, r, {:f, label(lbl, d, fb)}} | acc], d, fb)
  end

  defp replace_labels_1([{:catch, r, {:f, lbl}} | is], acc, d, fb) do
    replace_labels_1(is, [{:catch, r, {:f, label(lbl, d, fb)}} | acc], d, fb)
  end

  defp replace_labels_1([{:jump, {:f, lbl}} | is], acc, d, fb) do
    replace_labels_1(is, [{:jump, {:f, label(lbl, d, fb)}} | acc], d, fb)
  end

  defp replace_labels_1([{:loop_rec, {:f, lbl}, r} | is], acc, d, fb) do
    replace_labels_1(is, [{:loop_rec, {:f, label(lbl, d, fb)}, r} | acc], d, fb)
  end

  defp replace_labels_1([{:loop_rec_end, {:f, lbl}} | is], acc, d, fb) do
    replace_labels_1(is, [{:loop_rec_end, {:f, label(lbl, d, fb)}} | acc], d, fb)
  end

  defp replace_labels_1([{:wait, {:f, lbl}} | is], acc, d, fb) do
    replace_labels_1(is, [{:wait, {:f, label(lbl, d, fb)}} | acc], d, fb)
  end

  defp replace_labels_1([{:wait_timeout, {:f, lbl}, to} | is], acc, d, fb) do
    replace_labels_1(is, [{:wait_timeout, {:f, label(lbl, d, fb)}, to} | acc], d, fb)
  end

  defp replace_labels_1([{:recv_mark = op, {:f, lbl}} | is], acc, d, fb) do
    replace_labels_1(is, [{op, {:f, label(lbl, d, fb)}} | acc], d, fb)
  end

  defp replace_labels_1([{:recv_set = op, {:f, lbl}} | is], acc, d, fb) do
    replace_labels_1(is, [{op, {:f, label(lbl, d, fb)}} | acc], d, fb)
  end

  defp replace_labels_1([{:bif, name, {:f, lbl}, as, r} | is], acc, d, fb)
       when lbl !== 0 do
    replace_labels_1(is, [{:bif, name, {:f, label(lbl, d, fb)}, as, r} | acc], d, fb)
  end

  defp replace_labels_1([{:gc_bif, name, {:f, lbl}, live, as, r} | is], acc, d, fb)
       when lbl !== 0 do
    replace_labels_1(
      is,
      [
        {:gc_bif, name, {:f, label(lbl, d, fb)}, live, as, r}
        | acc
      ],
      d,
      fb
    )
  end

  defp replace_labels_1([{:call, ar, {:f, lbl}} | is], acc, d, fb) do
    replace_labels_1(is, [{:call, ar, {:f, label(lbl, d, fb)}} | acc], d, fb)
  end

  defp replace_labels_1([{:make_fun2, {:f, lbl}, u1, u2, u3} | is], acc, d, fb) do
    replace_labels_1(
      is,
      [
        {:make_fun2, {:f, label(lbl, d, fb)}, u1, u2, u3}
        | acc
      ],
      d,
      fb
    )
  end

  defp replace_labels_1([{:make_fun3, {:f, lbl}, u1, u2, u3, u4} | is], acc, d, fb) do
    replace_labels_1(
      is,
      [
        {:make_fun3, {:f, label(lbl, d, fb)}, u1, u2, u3, u4}
        | acc
      ],
      d,
      fb
    )
  end

  defp replace_labels_1(
         [
           {:bs_init, {:f, lbl}, info, live, ss, dst}
           | is
         ],
         acc,
         d,
         fb
       )
       when lbl !== 0 do
    replace_labels_1(
      is,
      [
        {:bs_init, {:f, label(lbl, d, fb)}, info, live, ss, dst}
        | acc
      ],
      d,
      fb
    )
  end

  defp replace_labels_1([{:bs_put, {:f, lbl}, info, ss} | is], acc, d, fb)
       when lbl !== 0 do
    replace_labels_1(is, [{:bs_put, {:f, label(lbl, d, fb)}, info, ss} | acc], d, fb)
  end

  defp replace_labels_1(
         [
           {:put_map = i, {:f, lbl}, op, src, dst, live, list}
           | is
         ],
         acc,
         d,
         fb
       )
       when lbl !== 0 do
    replace_labels_1(
      is,
      [
        {i, {:f, label(lbl, d, fb)}, op, src, dst, live, list}
        | acc
      ],
      d,
      fb
    )
  end

  defp replace_labels_1(
         [
           {:get_map_elements = i, {:f, lbl}, src, list}
           | is
         ],
         acc,
         d,
         fb
       )
       when lbl !== 0 do
    replace_labels_1(is, [{i, {:f, label(lbl, d, fb)}, src, list} | acc], d, fb)
  end

  defp replace_labels_1(
         [
           {:bs_start_match4, {:f, lbl}, live, src, dst}
           | is
         ],
         acc,
         d,
         fb
       ) do
    i = {:bs_start_match4, {:f, label(lbl, d, fb)}, live, src, dst}
    replace_labels_1(is, [i | acc], d, fb)
  end

  defp replace_labels_1([i | is], acc, d, fb) do
    replace_labels_1(is, [i | acc], d, fb)
  end

  defp replace_labels_1([], acc, _, _) do
    acc
  end

  defp label(old, d, fb) do
    case d do
      %{^old => new} ->
        new

      _ ->
        fb.(old)
    end
  end

  defp split_even([], ss, ds) do
    {reverse(ss), reverse(ds)}
  end

  defp split_even([[s, d] | rs], ss, ds) do
    split_even(rs, [s | ss], [d | ds])
  end
end
