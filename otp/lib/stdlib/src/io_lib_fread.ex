defmodule :m_io_lib_fread do
  use Bitwise
  import :lists, only: [reverse: 1]

  def fread([], chars, format) do
    fread_collect(format, [], 0, [], chars)
  end

  def fread({format, stack, n, results} = _Continuation, chars, _) do
    fread_collect(format, stack, n, results, chars)
  end

  defp fread_collect(format, [?\r | stack], n, results, [?\n | chars]) do
    fread_line(format, reverse(stack), n, results, chars, [?\r, ?\n])
  end

  defp fread_collect(format, stack, n, results, [?\n | chars]) do
    fread_line(format, reverse(stack), n, results, chars, [?\n])
  end

  defp fread_collect(format, stack, n, results, []) do
    continuation = {format, stack, n, results}
    {:more, continuation}
  end

  defp fread_collect(format, [?\r | stack], n, results, chars) do
    fread_line(format, reverse(stack), n, results, chars, [?\r])
  end

  defp fread_collect(format, stack, n, results, [c | chars]) do
    fread_collect(format, [c | stack], n, results, chars)
  end

  defp fread_collect(format, stack, n, results, chars) do
    fread_line(format, reverse(stack), n, results, chars, [])
  end

  defp fread_line(format0, line, n0, results0, more, newline) do
    chars =
      cond do
        is_list(more) ->
          more

        true ->
          []
      end

    case fread(format0, line, n0, results0) do
      {:ok, results, []} ->
        {:done, {:ok, results}, chars}

      {:ok, results, rest} ->
        {:done, {:ok, results}, rest ++ newline ++ chars}

      {:more, format, n, results}
      when is_list(line) and
             is_list(more) ->
        fread_collect(format, [], n + length(newline), results, more)

      {:more, format, n, results} when is_list(line) ->
        fread_line(format, :eof, n + length(newline), results, more, [])

      other ->
        {:done, other, more}
    end
  end

  def fread(format, line) do
    fread(format, line, 0, [])
  end

  defp fread([?~ | format0] = allFormat, line, n, results) do
    {format, f, sup, unicode} = fread_field(format0)
    fread1(format, f, sup, unicode, line, n, results, allFormat)
  end

  defp fread([c | format], line, n, results)
       when c === ?\s or c === ?\t or c === ?\r or c === ?\n do
    fread_skip_white(format, line, n, results)
  end

  defp fread([c | format], [c | line], n, results) do
    fread(format, line, n + 1, results)
  end

  defp fread([_F | _Format], [_C | _Line], _N, _Results) do
    fread_error(:input)
  end

  defp fread([_ | _] = format, [], n, results) do
    {:more, format, n, results}
  end

  defp fread([_ | _], :eof, 0, []) do
    :eof
  end

  defp fread([_ | _], :eof, _N, _Results) do
    fread_error(:input)
  end

  defp fread([], line, _N, results) do
    {:ok, reverse(results), line}
  end

  defp fread_skip_white(format, [c | line], n, results)
       when c === ?\s or c === ?\t or c === ?\r or c === ?\n do
    fread_skip_white(format, line, n + 1, results)
  end

  defp fread_skip_white(format, line, n, results) do
    fread(format, line, n, results)
  end

  defp fread_field([?* | format]) do
    fread_field(format, true, false)
  end

  defp fread_field(format) do
    fread_field(format, false, false)
  end

  defp fread_field([c | format], sup, unic)
       when c >= ?0 and
              c <= ?9 do
    fread_field(format, c - ?0, sup, unic)
  end

  defp fread_field([?t | format], sup, _Unic) do
    {format, :none, sup, true}
  end

  defp fread_field(format, sup, unic) do
    {format, :none, sup, unic}
  end

  defp fread_field([c | format], f, sup, unic)
       when c >= ?0 and
              c <= ?9 do
    fread_field(format, 10 * f + c - ?0, sup, unic)
  end

  defp fread_field([?t | format], f, sup, _Unic) do
    {format, f, sup, true}
  end

  defp fread_field(format, f, sup, unic) do
    {format, f, sup, unic}
  end

  defp fread1([?l | format], _F, sup, _U, line, n, res, _AllFormat) do
    fread(format, line, n, fread_result(sup, n, res))
  end

  defp fread1(_Format, _F, _Sup, _U, [], n, res, allFormat) do
    {:more, allFormat, n, res}
  end

  defp fread1(_Format, _F, _Sup, _U, :eof, 0, [], _AllFormat) do
    :eof
  end

  defp fread1(_Format, _F, _Sup, _U, :eof, _N, _Res, _AllFormat) do
    fread_error(:input)
  end

  defp fread1(format, f, sup, u, line, n, res, _AllFormat) do
    fread1(format, f, sup, u, line, n, res)
  end

  defp fread1([?f | format], :none, sup, false, line0, n0, res) do
    {line, n, cs} = fread_float_cs(line0, n0)
    fread_float(cs, sup, format, line, n, res)
  end

  defp fread1([?f | format], f, sup, false, line0, n, res) do
    {line, cs} = fread_chars(line0, f, false)
    fread_float(cs, sup, format, line, n + f, res)
  end

  defp fread1([?d | format], :none, sup, false, line0, n0, res) do
    {line, n, cs} = fread_int_cs(line0, n0)
    fread_integer(cs, 10, sup, format, line, n, res)
  end

  defp fread1([?d | format], f, sup, false, line0, n, res) do
    {line, cs} = fread_chars(line0, f, false)
    fread_integer(cs, 10, sup, format, line, n + f, res)
  end

  defp fread1([?u | format], :none, sup, false, line0, n0, res) do
    {line, n, cs} = fread_digits(line0, n0, 10, [])
    fread_unsigned(cs, 10, sup, format, line, n, res)
  end

  defp fread1([?u | format], f, sup, false, line0, n0, res)
       when f >= 2 and f <= 1 + ?Z - ?A + 10 do
    {line, n, cs} = fread_digits(line0, n0, f, [])
    fread_unsigned(cs, f, sup, format, line, n, res)
  end

  defp fread1([?- | format], _F, sup, false, line, n, res) do
    fread_sign_char(sup, format, line, n, res)
  end

  defp fread1([?# | format], :none, sup, false, line0, n0, res) do
    case (try do
            {line1, n1, b1} = fread_base(line0, n0)
            b = abs(b1)
            true = :erlang.and(b >= 2, b <= 1 + ?Z - ?A + 10)
            {line2, n2, cs2} = fread_digits(line1, n1, b, [])
            fread_based(reverse(cs2), b1, sup, format, line2, n2, res)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        fread_error(:based)

      other ->
        other
    end
  end

  defp fread1([?# | format], f, sup, false, line0, n, res) do
    case (try do
            {line1, cs1} = fread_chars(line0, f, false)
            {line2, _, b2} = fread_base(reverse(cs1), n)
            true = :erlang.and(b2 >= 2, b2 <= 1 + ?Z - ?A + 10)
            fread_based(line2, b2, sup, format, line1, n + f, res)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        fread_error(:based)

      other ->
        other
    end
  end

  defp fread1([?s | format], :none, sup, u, line0, n0, res) do
    {line, n, cs} = fread_string_cs(line0, n0, u)
    fread_string(cs, sup, u, format, line, n, res)
  end

  defp fread1([?s | format], f, sup, u, line0, n, res) do
    {line, cs} = fread_chars(line0, f, u)
    fread_string(cs, sup, u, format, line, n + f, res)
  end

  defp fread1([?a | format], :none, sup, u, line0, n0, res) do
    {line, n, cs} = fread_string_cs(line0, n0, u)
    fread_atom(cs, sup, format, line, n, res)
  end

  defp fread1([?a | format], f, sup, false, line0, n, res) do
    {line, cs} = fread_chars(line0, f, false)
    fread_atom(cs, sup, format, line, n + f, res)
  end

  defp fread1([?c | format], :none, sup, u, line0, n, res) do
    {line, cs} = fread_chars(line0, 1, u)
    fread_chars(cs, sup, u, format, line, n + 1, res)
  end

  defp fread1([?c | format], f, sup, u, line0, n, res) do
    {line, cs} = fread_chars(line0, f, u)
    fread_chars(cs, sup, u, format, line, n + f, res)
  end

  defp fread1([?~ | format], _F, _Sup, _U, [?~ | line], n, res) do
    fread(format, line, n + 1, res)
  end

  defp fread1(_Format, _F, _Sup, _U, _Line, _N, _Res) do
    fread_error(:format)
  end

  defp fread_float(cs, sup, format, line, n, res) do
    case (try do
            :erlang.list_to_float(fread_skip_white(reverse(cs)))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        fread_error(:float)

      float ->
        fread(format, line, n, fread_result(sup, float, res))
    end
  end

  defp fread_integer(cs, base, sup, format, line, n, res) do
    case (try do
            :erlang.list_to_integer(
              fread_skip_white(reverse(cs)),
              base
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        fread_error(:integer)

      integer ->
        fread(format, line, n, fread_result(sup, integer, res))
    end
  end

  defp fread_unsigned(cs, base, sup, format, line, n, res) do
    case (try do
            :erlang.list_to_integer(
              fread_skip_white(reverse(cs)),
              base
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        fread_error(:unsigned)

      integer ->
        fread(format, line, n, fread_result(sup, integer, res))
    end
  end

  defp fread_based(cs0, b, sup, format, line, n, res) do
    {cs, base} =
      cond do
        b < 0 ->
          {[?- | cs0], -b}

        true ->
          {cs0, b}
      end

    i = :erlang.list_to_integer(cs, base)
    fread(format, line, n, fread_result(sup, i, res))
  end

  defp fread_sign_char(sup, format, [?- | line], n, res) do
    fread(format, line, n + 1, fread_result(sup, -1, res))
  end

  defp fread_sign_char(sup, format, [?+ | line], n, res) do
    fread(format, line, n + 1, fread_result(sup, +1, res))
  end

  defp fread_sign_char(sup, format, line, n, res) do
    fread(format, line, n, fread_result(sup, 1, res))
  end

  defp fread_string(:error, _Sup, _U, _Format, _Line, _N, _Res) do
    fread_error(:string)
  end

  defp fread_string(cs0, sup, u, format, line, n, res) do
    cs = fread_skip_white(reverse(fread_skip_white(cs0)))
    fread(format, line, n, fread_convert(fread_result(sup, cs, res), u))
  end

  defp fread_atom(:error, _Sup, _Format, _Line, _N, _Res) do
    fread_error(:atom)
  end

  defp fread_atom(cs0, sup, format, line, n, res) do
    cs = fread_skip_white(reverse(fread_skip_white(cs0)))
    fread(format, line, n, fread_result(sup, :erlang.list_to_atom(cs), res))
  end

  defp fread_chars(:error, _Sup, _U, _Format, _Line, _N, _Res) do
    fread_error(:character)
  end

  defp fread_chars(cs, sup, u, format, line, n, res) do
    fread(format, line, n, fread_convert(fread_result(sup, reverse(cs), res), u))
  end

  defp fread_chars(line, c, u) do
    fread_chars(c, line, u, [])
  end

  defp fread_chars(0, line, _U, cs) do
    {line, cs}
  end

  defp fread_chars(_N, [?\n | line], _U, _Cs) do
    {[?\n | line], :error}
  end

  defp fread_chars(n, [c | line], true, cs) do
    fread_chars(n - 1, line, true, [c | cs])
  end

  defp fread_chars(n, [c | line], false, cs)
       when c >= 0 and
              c <= 255 do
    fread_chars(n - 1, line, false, [c | cs])
  end

  defp fread_chars(_N, l, _U, _Cs) do
    {l, :error}
  end

  defp fread_int_cs(line0, n0) do
    {line1, n1} = fread_skip_white(line0, n0)
    {line, n, cs} = fread_sign(line1, n1, [])
    fread_digits(line, n, cs)
  end

  defp fread_float_cs(line0, n0) do
    {line1, n1} = fread_skip_white(line0, n0)
    {line2, n2, cs2} = fread_sign(line1, n1, [])
    {line, n, cs} = fread_digits(line2, n2, cs2)
    fread_float_cs_1(line, n, cs)
  end

  defp fread_float_cs_1([?. | line0], n0, cs0) do
    {line, n, cs} = fread_digits(line0, n0 + 1, [?. | cs0])
    fread_float_cs_2(line, n, cs)
  end

  defp fread_float_cs_1(line, n, cs) do
    {line, n, cs}
  end

  defp fread_float_cs_2([?e | line0], n0, cs0) do
    {line, n, cs} = fread_sign(line0, n0 + 1, [?e | cs0])
    fread_digits(line, n, cs)
  end

  defp fread_float_cs_2([?E | line0], n0, cs0) do
    {line, n, cs} = fread_sign(line0, n0 + 1, [?E | cs0])
    fread_digits(line, n, cs)
  end

  defp fread_float_cs_2(line, n, cs) do
    {line, n, cs}
  end

  defp fread_string_cs(line0, n0, false) do
    {line, n} = fread_skip_white(line0, n0)
    fread_skip_latin1_nonwhite(line, n, [])
  end

  defp fread_string_cs(line0, n0, true) do
    {line, n} = fread_skip_white(line0, n0)
    fread_skip_nonwhite(line, n, [])
  end

  defp fread_skip_white([c | line])
       when c === ?\s or c === ?\t or c === ?\r or c === ?\n do
    fread_skip_white(line)
  end

  defp fread_skip_white(line) do
    line
  end

  defp fread_skip_white([c | line], n)
       when c === ?\s or c === ?\t or c === ?\r or c === ?\n do
    fread_skip_white(line, n + 1)
  end

  defp fread_skip_white(line, n) do
    {line, n}
  end

  defp fread_skip_latin1_nonwhite([c | line], n, cs)
       when c === ?\s or c === ?\t or c === ?\r or c === ?\n do
    {[c | line], n, cs}
  end

  defp fread_skip_latin1_nonwhite([c | line], n, []) when c > 255 do
    {[c | line], n, :error}
  end

  defp fread_skip_latin1_nonwhite([c | line], n, cs) when c > 255 do
    {[c | line], n, cs}
  end

  defp fread_skip_latin1_nonwhite([c | line], n, cs) do
    fread_skip_latin1_nonwhite(line, n + 1, [c | cs])
  end

  defp fread_skip_latin1_nonwhite([], n, cs) do
    {[], n, cs}
  end

  defp fread_skip_nonwhite([c | line], n, cs)
       when c === ?\s or c === ?\t or c === ?\r or c === ?\n do
    {[c | line], n, cs}
  end

  defp fread_skip_nonwhite([c | line], n, cs) do
    fread_skip_nonwhite(line, n + 1, [c | cs])
  end

  defp fread_skip_nonwhite([], n, cs) do
    {[], n, cs}
  end

  defp fread_sign([?+ | line], n, cs) do
    {line, n + 1, [?+ | cs]}
  end

  defp fread_sign([?- | line], n, cs) do
    {line, n + 1, [?- | cs]}
  end

  defp fread_sign(line, n, cs) do
    {line, n, cs}
  end

  defp fread_base(line0, n0) do
    {[?# | line1], n1, cs1} = fread_int_cs(line0, n0)
    b = :erlang.list_to_integer(reverse(cs1))
    {line1, n1 + 1, b}
  end

  defp fread_digits([c | line], n, cs) when c >= ?0 and c <= ?9 do
    fread_digits(line, n + 1, [c | cs])
  end

  defp fread_digits(line, n, cs) do
    {line, n, cs}
  end

  defp fread_digits([c | line], n, base, cs)
       when c >= ?0 and
              c <= ?9 do
    fread_digits(line, n + 1, base, [c | cs])
  end

  defp fread_digits([c | line], n, base, cs)
       when c >= ?A and
              c < ?A + base - 10 do
    fread_digits(line, n + 1, base, [c | cs])
  end

  defp fread_digits([c | line], n, base, cs)
       when c >= ?a and
              c < ?a + base - 10 do
    fread_digits(line, n + 1, base, [c | cs])
  end

  defp fread_digits(line, n, _Base, cs) do
    {line, n, cs}
  end

  defp fread_result(true, _V, res) do
    res
  end

  defp fread_result(false, v, res) do
    [v | res]
  end

  defp fread_convert(any, _) do
    any
  end

  defp fread_error(in__) do
    {:error, {:fread, in__}}
  end
end
