defmodule :m_edoc_scanner do
  use Bitwise
  import :lists, only: [reverse: 1]

  def string(cs) do
    string(cs, 1)
  end

  def string(cs, startPos) do
    case scan(cs, startPos) do
      {:ok, toks} ->
        {:ok, toks, startPos}

      {:error, e} ->
        {:error, e, startPos}
    end
  end

  def format_error({:string, quote, head}) do
    ['unterminated string starting with ' ++ :io_lib.write_string(head, quote)]
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

  defp reserved(:where) do
    true
  end

  defp reserved(_) do
    false
  end

  defp scan(cs, pos) do
    scan1(cs, [], pos)
  end

  defp scan1([?\n | cs], toks, pos) do
    scan1(cs, toks, pos + 1)
  end

  defp scan1([c | cs], toks, pos)
       when c >= 0 and
              c <= ?\s do
    scan1(cs, toks, pos)
  end

  defp scan1([c | cs], toks, pos)
       when c >= ?a and
              c <= ?z do
    scan_atom(c, cs, toks, pos)
  end

  defp scan1([c | cs], toks, pos)
       when c >= 223 and
              c <= 255 and c != 247 do
    scan_atom(c, cs, toks, pos)
  end

  defp scan1([c | cs], toks, pos)
       when c >= ?0 and
              c <= ?9 do
    scan_number(c, cs, toks, pos)
  end

  defp scan1([[?-, c] | cs], toks, pos)
       when c >= ?0 and
              c <= ?9 do
    scan_signed_number(?-, c, cs, toks, pos)
  end

  defp scan1([[?+, c] | cs], toks, pos)
       when c >= ?0 and
              c <= ?9 do
    scan_signed_number(?+, c, cs, toks, pos)
  end

  defp scan1([c | cs], toks, pos)
       when c >= ?A and
              c <= ?Z do
    scan_variable(c, cs, toks, pos)
  end

  defp scan1([?_ | cs], toks, pos) do
    scan_variable(?_, cs, toks, pos)
  end

  defp scan1([c | cs], toks, pos)
       when c >= 192 and
              c <= 222 and c != 215 do
    scan_variable(c, cs, toks, pos)
  end

  defp scan1([?$ | cs], toks, pos) do
    case scan_char_const(cs, toks, pos) do
      {:ok, result} ->
        {:ok, result}

      {:error, :truncated_char} ->
        scan_error(:char, pos)

      {:error, :illegal_character} ->
        scan_error({:illegal, :char}, pos)
    end
  end

  defp scan1([?' | cs0], toks, pos) do
    case scan_string(cs0, ?', pos) do
      {s, cs1, pos1} ->
        case (try do
                :erlang.list_to_atom(s)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          a when is_atom(a) ->
            scan1(cs1, [{:atom, pos, a} | toks], pos1)

          _Error ->
            scan_error({:illegal, :atom}, pos)
        end

      {:error, :premature_end} ->
        scan_error({:string, ?', cs0}, pos)

      {:error, :truncated_char} ->
        scan_error(:char, pos)

      {:error, :illegal_character} ->
        scan_error({:illegal, :atom}, pos)
    end
  end

  defp scan1([?" | cs0], toks, pos) do
    case scan_string(cs0, ?", pos) do
      {s, cs1, pos1} ->
        case toks do
          [{:string, pos0, s0} | toks1] ->
            scan1(cs1, [{:string, pos0, s0 ++ s} | toks1], pos1)

          _ ->
            scan1(cs1, [{:string, pos, s} | toks], pos1)
        end

      {:error, :premature_end} ->
        scan_error({:string, ?", cs0}, pos)

      {:error, :truncated_char} ->
        scan_error(:char, pos)

      {:error, :illegal_character} ->
        scan_error({:illegal, :string}, pos)
    end
  end

  defp scan1([[?=, ?>] | cs], toks, pos) do
    scan1(cs, [{:"=>", pos} | toks], pos)
  end

  defp scan1([[?<, ?<] | cs], toks, pos) do
    scan1(cs, [{:"<<", pos} | toks], pos)
  end

  defp scan1([[?>, ?>] | cs], toks, pos) do
    scan1(cs, [{:">>", pos} | toks], pos)
  end

  defp scan1([[?-, ?>] | cs], toks, pos) do
    scan1(cs, [{:->, pos} | toks], pos)
  end

  defp scan1([[?:, ?=] | cs], toks, pos) do
    scan1(cs, [{:":=", pos} | toks], pos)
  end

  defp scan1([[?:, ?:] | cs], toks, pos) do
    scan1(cs, [{:"::", pos} | toks], pos)
  end

  defp scan1([[?/, ?/] | cs], toks, pos) do
    scan1(cs, [{:"//", pos} | toks], pos)
  end

  defp scan1([[?., ?., ?.] | cs], toks, pos) do
    scan1(cs, [{:..., pos} | toks], pos)
  end

  defp scan1([[?., ?.] | cs], toks, pos) do
    scan1(cs, [{:.., pos} | toks], pos)
  end

  defp scan1([c | cs], toks, pos) do
    p = :erlang.list_to_atom([c])
    scan1(cs, [{p, pos} | toks], pos)
  end

  defp scan1([], toks0, _Pos) do
    toks = reverse(toks0)
    {:ok, toks}
  end

  defp scan_variable(c, cs, toks, pos) do
    {wcs, cs1} = scan_name(cs, [])
    w = [c | reverse(wcs)]

    case w do
      '_' ->
        scan1(cs1, [{:an_var, pos, :_} | toks], pos)

      _ ->
        case (try do
                :erlang.list_to_atom(w)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          a when is_atom(a) ->
            scan1(cs1, [{:var, pos, a} | toks], pos)

          _ ->
            scan_error({:illegal, :variable}, pos)
        end
    end
  end

  defp scan_atom(c, cs, toks, pos) do
    {wcs, cs1} = scan_name(cs, [])
    w = [c | reverse(wcs)]

    case (try do
            :erlang.list_to_atom(w)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      a when is_atom(a) ->
        case reserved(a) do
          true ->
            scan1(cs1, [{a, pos} | toks], pos)

          false ->
            scan1(cs1, [{:atom, pos, a} | toks], pos)
        end

      _ ->
        scan_error({:illegal, :token}, pos)
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

  defp name_char(c) when c >= ?a and c <= ?z do
    true
  end

  defp name_char(c) when c >= 223 and c <= 255 and c != 247 do
    true
  end

  defp name_char(c) when c >= ?A and c <= ?Z do
    true
  end

  defp name_char(c) when c >= 192 and c <= 222 and c != 215 do
    true
  end

  defp name_char(c) when c >= ?0 and c <= ?9 do
    true
  end

  defp name_char(?_) do
    true
  end

  defp name_char(?@) do
    true
  end

  defp name_char(_) do
    false
  end

  defp scan_string(cs, quote, pos) do
    scan_string(cs, [], quote, pos)
  end

  defp scan_string([quote | cs], scs, quote, pos) do
    {reverse(scs), cs, pos}
  end

  defp scan_string([], _Scs, _Quote, _Pos) do
    {:error, :premature_end}
  end

  defp scan_string(cs0, scs, quote, pos) do
    case scan_char(cs0, pos) do
      {c, cs, pos1} ->
        scan_string(cs, [c | scs], quote, pos1)

      error ->
        error
    end
  end

  defp scan_char_const([?\s | _Cs0], _Toks, _Pos) do
    {:error, :illegal_character}
  end

  defp scan_char_const(cs0, toks, pos) do
    case scan_char(cs0, pos) do
      {c, cs, pos1} ->
        scan1(cs, [{:char, pos, c} | toks], pos1)

      error ->
        error
    end
  end

  defp scan_char([?\\ | cs], pos) do
    scan_escape(cs, pos)
  end

  defp scan_char([c | _Cs], _Pos) when c <= 31 do
    {:error, :illegal_character}
  end

  defp scan_char([c | cs], pos) do
    {c, cs, pos}
  end

  defp scan_char([], _Pos) do
    {:error, :truncated_char}
  end

  defp scan_escape([[o1, o2, o3] | cs], pos)
       when o1 >= ?0 and
              o1 <= ?3 and o2 >= ?0 and
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

  defp scan_escape([[?x, ?{] | cs], pos) do
    scan_hex(cs, pos, [])
  end

  defp scan_escape([[?x, h1, h2] | cs], pos)
       when (h1 >= ?0 and h1 <= ?9) or (h1 >= ?A and h1 <= ?F) or
              (h1 >= ?a and h1 <= ?f and
                 h2 >= ?0 and h2 <= ?9) or (h2 >= ?A and h2 <= ?F) or (h2 >= ?a and h2 <= ?f) do
    val = h1 * 16 + h2 - 17 * ?0
    {val, cs, pos}
  end

  defp scan_escape([[?^, c] | cs], pos) do
    cond do
      c >= ?@ and c <= ?_ ->
        {c - ?@, cs, pos}

      true ->
        {:error, :illegal_control_character}
    end
  end

  defp scan_escape([c | cs], pos) do
    case escape_char(c) do
      c1 when c1 > ?\0 ->
        {c1, cs, pos}

      _ ->
        {:error, :undefined_escape_sequence}
    end
  end

  defp scan_escape([], _Pos) do
    {:error, :truncated_char}
  end

  defp scan_hex([c | cs], pos, hCs)
       when (c >= ?0 and c <= ?9) or (c >= ?A and c <= ?F) or (c >= ?a and c <= ?f) do
    scan_hex(cs, pos, [c | hCs])
  end

  defp scan_hex([?} | cs], pos, hCs) do
    case (try do
            :erlang.list_to_integer(:lists.reverse(hCs), 16)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      val
      when (val >= 0 and val < 55296) or (val > 57343 and val < 65534) or
             (val > 65535 and val <= 1_114_111) ->
        {val, cs, pos}

      _ ->
        {:error, :undefined_escape_sequence}
    end
  end

  defp scan_hex(_Cs, _Pos, _HCs) do
    {:error, :undefined_escape_sequence}
  end

  defp escape_char(?b) do
    ?\b
  end

  defp escape_char(?d) do
    ?\d
  end

  defp escape_char(?e) do
    ?\e
  end

  defp escape_char(?f) do
    ?\f
  end

  defp escape_char(?n) do
    ?\n
  end

  defp escape_char(?r) do
    ?\r
  end

  defp escape_char(?s) do
    ?\s
  end

  defp escape_char(?t) do
    ?\t
  end

  defp escape_char(?v) do
    ?\v
  end

  defp escape_char(?\\) do
    ?\\
  end

  defp escape_char(?') do
    ?'
  end

  defp escape_char(?") do
    ?"
  end

  defp escape_char(_C) do
    ?\0
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

  defp scan_after_int(cs, ncs, toks, sPos, cPos) do
    n = :erlang.list_to_integer(reverse(ncs))
    scan1(cs, [{:integer, sPos, n} | toks], cPos)
  end

  defp scan_after_fraction([?E | cs], ncs, toks, sPos, cPos) do
    scan_exponent(cs, [?E | ncs], toks, sPos, cPos)
  end

  defp scan_after_fraction([?e | cs], ncs, toks, sPos, cPos) do
    scan_exponent(cs, [?e | ncs], toks, sPos, cPos)
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
    {:error, {pos, :edoc_scanner, in__}}
  end
end
