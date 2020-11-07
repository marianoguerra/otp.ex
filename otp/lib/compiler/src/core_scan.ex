defmodule :m_core_scan do
  use Bitwise
  import :lists, only: [reverse: 1]

  def string(cs) do
    string(cs, 1)
  end

  def string(cs, sp) do
    case string_pre_scan(cs, [], sp) do
      {:done, _, soFar, ep} ->
        case scan(reverse(soFar), sp) do
          {:ok, toks} ->
            {:ok, toks, ep}

          {:error, e} ->
            {:error, e, ep}
        end

      other ->
        other
    end
  end

  defp string_pre_scan(cs, soFar0, sp) do
    case pre_scan(cs, soFar0, sp) do
      {:done, rest, soFar1, ep} ->
        {:done, rest, soFar1, ep}

      {:more, rest, soFar1, ep} ->
        string_pre_scan(rest ++ :eof, soFar1, ep)

      other ->
        other
    end
  end

  def format_error({:string, quote, head}) do
    [
      'unterminated ' ++
        string_thing(quote) ++
        ' starting with ' ++
        :io_lib.write_string(
          head,
          quote
        )
    ]
  end

  def format_error({:illegal, type}) do
    :io_lib.fwrite('illegal ~w', [type])
  end

  def format_error(:char) do
    'unterminated character'
  end

  def format_error(:scan) do
    'premature end'
  end

  def format_error({:base, base}) do
    :io_lib.fwrite('illegal base \'~w\'', [base])
  end

  def format_error(:float) do
    'bad float'
  end

  def format_error(other) do
    :io_lib.write(other)
  end

  defp string_thing(?') do
    'atom'
  end

  defp string_thing(?") do
    'string'
  end

  defp pre_scan([c | cs], soFar, pos) do
    pre_scan(c, cs, soFar, pos)
  end

  defp pre_scan([], soFar, pos) do
    {:more, [], soFar, pos}
  end

  defp pre_scan(:eof, soFar, pos) do
    {:done, :eof, soFar, pos}
  end

  defp pre_scan(?$, cs0, soFar0, pos) do
    case pre_char(cs0, [?$ | soFar0]) do
      {cs, soFar} ->
        pre_scan(cs, soFar, pos)

      :more ->
        {:more, [?$ | cs0], soFar0, pos}

      :error ->
        pre_error(:char, pos, pos)
    end
  end

  defp pre_scan(?', cs, soFar, pos) do
    pre_string(cs, ?', :"'", pos, [?' | soFar], pos)
  end

  defp pre_scan({:"'", sp}, cs, soFar, pos) do
    pre_string(cs, ?', :"'", sp, soFar, pos)
  end

  defp pre_scan(?", cs, soFar, pos) do
    pre_string(cs, ?", :"\"", pos, [?" | soFar], pos)
  end

  defp pre_scan({:"\"", sp}, cs, soFar, pos) do
    pre_string(cs, ?", :"\"", sp, soFar, pos)
  end

  defp pre_scan(?%, cs, soFar, pos) do
    pre_comment(cs, soFar, pos)
  end

  defp pre_scan(:%, cs, soFar, pos) do
    pre_comment(cs, soFar, pos)
  end

  defp pre_scan(?\n, cs, soFar, pos) do
    pre_scan(cs, [?\n | soFar], pos + 1)
  end

  defp pre_scan(c, cs, soFar, pos) do
    pre_scan(cs, [c | soFar], pos)
  end

  defp pre_string([q | cs], q, _, _, soFar, pos) do
    pre_scan(cs, [q | soFar], pos)
  end

  defp pre_string([?\n | cs], q, reent, sp, soFar, pos) do
    pre_string(cs, q, reent, sp, [?\n | soFar], pos + 1)
  end

  defp pre_string([?\\ | cs0], q, reent, sp, soFar0, pos) do
    case pre_escape(cs0, soFar0) do
      {cs, soFar} ->
        pre_string(cs, q, reent, sp, soFar, pos)

      :more ->
        {:more, [[{reent, sp}, ?\\] | cs0], soFar0, pos}

      :error ->
        pre_string_error(q, sp, soFar0, pos)
    end
  end

  defp pre_string([c | cs], q, reent, sp, soFar, pos) do
    pre_string(cs, q, reent, sp, [c | soFar], pos)
  end

  defp pre_string([], _, reent, sp, soFar, pos) do
    {:more, [{reent, sp}], soFar, pos}
  end

  defp pre_string(:eof, q, _, sp, soFar, pos) do
    pre_string_error(q, sp, soFar, pos)
  end

  defp pre_string_error(q, sp, soFar, pos) do
    [s, _] = :string.split(soFar, [q])
    pre_error({:string, q, :string.slice(:string.reverse(s), 0, 16)}, sp, pos)
  end

  defp pre_char([c | cs], soFar) do
    pre_char(c, cs, soFar)
  end

  defp pre_char([], _) do
    :more
  end

  defp pre_char(:eof, _) do
    :error
  end

  defp pre_char(?\\, cs, soFar) do
    pre_escape(cs, soFar)
  end

  defp pre_char(c, cs, soFar) do
    {cs, [c | soFar]}
  end

  defp pre_escape([?^ | cs0], soFar) do
    case cs0 do
      [c3 | cs] ->
        {cs, [[c3, ?^, ?\\] | soFar]}

      [] ->
        :more

      :eof ->
        :error
    end
  end

  defp pre_escape([c | cs], soFar) do
    {cs, [[c, ?\\] | soFar]}
  end

  defp pre_escape([], _) do
    :more
  end

  defp pre_escape(:eof, _) do
    :error
  end

  defp pre_comment([?\n | cs], soFar, pos) do
    pre_scan(cs, [[?\n, ?\s] | soFar], pos + 1)
  end

  defp pre_comment([_ | cs], soFar, pos) do
    pre_comment(cs, soFar, pos)
  end

  defp pre_comment([], soFar, pos) do
    {:more, [:%], soFar, pos}
  end

  defp pre_comment(:eof, sofar, pos) do
    pre_scan(:eof, [?\s | sofar], pos)
  end

  defp pre_error(e, epos, pos) do
    {:error, {epos, :core_scan, e}, pos}
  end

  defp scan(cs, pos) do
    scan1(cs, [], pos)
  end

  defp scan_key_word(c, cs0, toks, pos) do
    {wcs, cs} = scan_name(cs0, [])

    case (try do
            :erlang.list_to_atom([c | reverse(wcs)])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      name when is_atom(name) ->
        scan1(cs, [{name, pos} | toks], pos)

      _Error ->
        scan_error({:illegal, :atom}, pos)
    end
  end

  defp scan_variable(c, cs0, toks, pos) do
    {wcs, cs} = scan_name(cs0, [])

    case (try do
            :erlang.list_to_atom([c | reverse(wcs)])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      name when is_atom(name) ->
        scan1(cs, [{:var, pos, name} | toks], pos)

      _Error ->
        scan_error({:illegal, :var}, pos)
    end
  end

  defp scan_name([c | cs], ncs) do
    case name_char(c) do
      true ->
        scan_name(cs, [c | ncs])

      false ->
        {ncs, [c | cs]}
    end
  end

  defp scan_name([], ncs) do
    {ncs, []}
  end

  defp scan_string(cs, q, pos) do
    scan_string(cs, [], q, pos)
  end

  defp scan_string([q | cs], scs, q, pos) do
    {reverse(scs), cs, pos}
  end

  defp scan_string([?\n | cs], scs, q, pos) do
    scan_string(cs, [?\n | scs], q, pos + 1)
  end

  defp scan_string([?\\ | cs0], scs, q, pos) do
    {c, cs, pos1} = scan_escape(cs0, pos)
    scan_string(cs, [c | scs], q, pos1)
  end

  defp scan_string([c | cs], scs, q, pos) do
    scan_string(cs, [c | scs], q, pos)
  end

  defp scan_char([?\\ | cs], pos) do
    scan_escape(cs, pos)
  end

  defp scan_char([?\n | cs], pos) do
    {?\n, cs, pos + 1}
  end

  defp scan_char([c | cs], pos) do
    {c, cs, pos}
  end

  defp scan_escape([[o1, o2, o3] | cs], pos)
       when o1 >= ?0 and
              o1 <= ?7 and o2 >= ?0 and
              o2 <= ?7 and o3 >= ?0 and
              o3 <= ?7 do
    val = (o1 * 8 + o2) * 8 + o3 - 73 * ?0
    {val, cs, pos}
  end

  defp scan_escape([[o1, o2] | cs], pos)
       when o1 >= ?0 and
              o1 <= ?7 and o2 >= ?0 and o2 <= ?7 do
    val = o1 * 8 + o2 - 9 * ?0
    {val, cs, pos}
  end

  defp scan_escape([o1 | cs], pos) when o1 >= ?0 and o1 <= ?7 do
    {o1 - ?0, cs, pos}
  end

  defp scan_escape([[?^, c] | cs], pos) do
    val = c &&& 31
    {val, cs, pos}
  end

  defp scan_escape([?\n | cs], pos) do
    {?\n, cs, pos + 1}
  end

  defp scan_escape([c0 | cs], pos) do
    c = escape_char(c0)
    {c, cs, pos}
  end

  defp escape_char(?n) do
    ?\n
  end

  defp escape_char(?r) do
    ?\r
  end

  defp escape_char(?t) do
    ?\t
  end

  defp escape_char(?v) do
    ?\v
  end

  defp escape_char(?b) do
    ?\b
  end

  defp escape_char(?f) do
    ?\f
  end

  defp escape_char(?e) do
    ?\e
  end

  defp escape_char(?s) do
    ?\s
  end

  defp escape_char(?d) do
    ?\d
  end

  defp escape_char(c) do
    c
  end

  defp scan_number(c, cs0, toks, pos) do
    {ncs, cs, pos1} = scan_integer(cs0, [c], pos)
    scan_after_int(cs, ncs, toks, pos, pos1)
  end

  defp scan_signed_number(s, c, cs0, toks, pos) do
    {ncs, cs, pos1} = scan_integer(cs0, [c, s], pos)
    scan_after_int(cs, ncs, toks, pos, pos1)
  end

  defp scan_integer([c | cs], stack, pos)
       when c >= ?0 and
              c <= ?9 do
    scan_integer(cs, [c | stack], pos)
  end

  defp scan_integer(cs, stack, pos) do
    {stack, cs, pos}
  end

  defp scan_after_int([[?., c] | cs0], ncs0, toks, sPos, cPos)
       when c >= ?0 and c <= ?9 do
    {ncs, cs, cPos1} = scan_integer(cs0, [[c, ?.] | ncs0], cPos)
    scan_after_fraction(cs, ncs, toks, sPos, cPos1)
  end

  defp scan_after_int([?# | cs], ncs, toks, sPos, cPos) do
    case :erlang.list_to_integer(reverse(ncs)) do
      base when base >= 2 and base <= 16 ->
        scan_based_int(cs, 0, base, toks, sPos, cPos)

      base ->
        scan_error({:base, base}, cPos)
    end
  end

  defp scan_after_int(cs, ncs, toks, sPos, cPos) do
    n = :erlang.list_to_integer(reverse(ncs))
    scan1(cs, [{:integer, sPos, n} | toks], cPos)
  end

  defp scan_based_int([c | cs], soFar, base, toks, sPos, cPos)
       when c >= ?0 and c <= ?9 and c < base + ?0 do
    next = soFar * base + (c - ?0)
    scan_based_int(cs, next, base, toks, sPos, cPos)
  end

  defp scan_based_int([c | cs], soFar, base, toks, sPos, cPos)
       when c >= ?a and c <= ?f and c < base + ?a - 10 do
    next = soFar * base + (c - ?a + 10)
    scan_based_int(cs, next, base, toks, sPos, cPos)
  end

  defp scan_based_int([c | cs], soFar, base, toks, sPos, cPos)
       when c >= ?A and c <= ?F and c < base + ?A - 10 do
    next = soFar * base + (c - ?A + 10)
    scan_based_int(cs, next, base, toks, sPos, cPos)
  end

  defp scan_based_int(cs, soFar, _, toks, sPos, cPos) do
    scan1(cs, [{:integer, sPos, soFar} | toks], cPos)
  end

  defp scan_after_fraction([?E | cs], ncs, toks, sPos, cPos) do
    scan_exponent(cs, [?E | ncs], toks, sPos, cPos)
  end

  defp scan_after_fraction([?e | cs], ncs, toks, sPos, cPos) do
    scan_exponent(cs, [?E | ncs], toks, sPos, cPos)
  end

  defp scan_after_fraction(cs, ncs, toks, sPos, cPos) do
    case (try do
            :erlang.list_to_float(reverse(ncs))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      n when is_float(n) ->
        scan1(cs, [{:float, sPos, n} | toks], cPos)

      _Error ->
        scan_error({:illegal, :float}, sPos)
    end
  end

  defp scan_exponent([?+ | cs], ncs, toks, sPos, cPos) do
    scan_exponent1(cs, [?+ | ncs], toks, sPos, cPos)
  end

  defp scan_exponent([?- | cs], ncs, toks, sPos, cPos) do
    scan_exponent1(cs, [?- | ncs], toks, sPos, cPos)
  end

  defp scan_exponent(cs, ncs, toks, sPos, cPos) do
    scan_exponent1(cs, ncs, toks, sPos, cPos)
  end

  defp scan_exponent1([c | cs0], ncs0, toks, sPos, cPos)
       when c >= ?0 and c <= ?9 do
    {ncs, cs, cPos1} = scan_integer(cs0, [c | ncs0], cPos)

    case (try do
            :erlang.list_to_float(reverse(ncs))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      n when is_float(n) ->
        scan1(cs, [{:float, sPos, n} | toks], cPos1)

      _Error ->
        scan_error({:illegal, :float}, sPos)
    end
  end

  defp scan_exponent1(_, _, _, _, cPos) do
    scan_error(:float, cPos)
  end

  defp scan_error(in__, pos) do
    {:error, {pos, :core_scan, in__}}
  end
end
