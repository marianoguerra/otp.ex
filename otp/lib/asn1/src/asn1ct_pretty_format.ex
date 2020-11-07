defmodule :m_asn1ct_pretty_format do
  use Bitwise
  import :io_lib, only: [write: 1, write_string: 1]

  def term(term) do
    :erlang.element(2, term(term, 0))
  end

  defp term([], indent) do
    {indent, [?[, ?]]}
  end

  defp term(l, indent) when is_list(l) do
    case is_string(l) do
      true ->
        {indent, write_string(l)}

      false ->
        case complex_list(l) do
          true ->
            write_complex_list(l, indent)

          false ->
            write_simple_list(l, indent)
        end
    end
  end

  defp term(t, indent) when is_tuple(t) do
    case complex_tuple(t) do
      true ->
        write_complex_tuple(t, indent)

      false ->
        write_simple_tuple(t, indent)
    end
  end

  defp term(a, indent) do
    {indent, write(a)}
  end

  defp write_simple_list([h | t], indent) do
    {_, s1} = term(h, indent)
    {_, s2} = write_simple_list_tail(t, indent)
    {indent, [[?[, s1] | s2]}
  end

  defp write_simple_list_tail([h | t], indent) do
    {_, s1} = term(h, indent)
    {_, s2} = write_simple_list_tail(t, indent)
    {indent, [[?,, s1] | s2]}
  end

  defp write_simple_list_tail([], indent) do
    {indent, ']'}
  end

  defp write_simple_list_tail(other, indent) do
    {_, s} = term(other, indent)
    {indent, [?|, s, ?]]}
  end

  defp write_complex_list([h | t], indent) do
    {i1, s1} = term(h, indent + 1)
    {_, s2} = write_complex_list_tail(t, i1)
    {indent, [[?[, s1] | s2]}
  end

  defp write_complex_list_tail([h | t], indent) do
    {i1, s1} = term(h, indent)
    {_, s2} = write_complex_list_tail(t, i1)
    {indent, [?,, nl_indent(indent), s1, s2]}
  end

  defp write_complex_list_tail([], indent) do
    {indent, ']'}
  end

  defp write_complex_list_tail(other, indent) do
    {_, s} = term(other, indent)
    {indent, [?|, s, ?]]}
  end

  defp complex_list([]) do
    false
  end

  defp complex_list([h | t])
       when is_list(h) === false and
              is_tuple(h) === false do
    complex_list(t)
  end

  defp complex_list([h | t]) do
    case is_string(h) do
      true ->
        complex_list(t)

      false ->
        true
    end
  end

  defp complex_list(_) do
    true
  end

  defp complex_tuple(t) do
    complex_list(:erlang.tuple_to_list(t))
  end

  defp write_simple_tuple({}, indent) do
    {indent, '{}'}
  end

  defp write_simple_tuple(tuple, indent) do
    {_, s} =
      write_simple_tuple_args(
        :erlang.tuple_to_list(tuple),
        indent
      )

    {indent, [?{, s, ?}]}
  end

  defp write_simple_tuple_args([x], indent) do
    term(x, indent)
  end

  defp write_simple_tuple_args([h | t], indent) do
    {_, sH} = term(h, indent)
    {_, sT} = write_simple_tuple_args(t, indent)
    {indent, [sH, ?,, sT]}
  end

  defp write_complex_tuple(tuple, indent) do
    [h | t] = :erlang.tuple_to_list(tuple)
    {i1, sH} = term(h, indent + 2)
    {_, sT} = write_complex_tuple_args(t, i1)
    {indent, [?{, sH, sT, ?}]}
  end

  defp write_complex_tuple_args([x], indent) do
    {_, s} = term(x, indent)
    {indent, [?,, nl_indent(indent), s]}
  end

  defp write_complex_tuple_args([h | t], indent) do
    {i1, sH} = term(h, indent)
    {_, sT} = write_complex_tuple_args(t, i1)
    {indent, [?,, nl_indent(indent), sH, sT]}
  end

  defp write_complex_tuple_args([], indent) do
    {indent, []}
  end

  defp nl_indent(i) when i >= 0 do
    ['\n' | indent(i)]
  end

  defp nl_indent(_) do
    [?\s]
  end

  defp indent(i) when i >= 8 do
    [?\t | indent(i - 8)]
  end

  defp indent(i) when i > 0 do
    [?\s | indent(i - 1)]
  end

  defp indent(_) do
    []
  end

  defp is_string([9 | t]) do
    is_string(t)
  end

  defp is_string([10 | t]) do
    is_string(t)
  end

  defp is_string([h | t]) when h > 31 and h < 127 do
    is_string(t)
  end

  defp is_string([]) do
    true
  end

  defp is_string(_) do
    false
  end
end
