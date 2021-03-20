defmodule :m_io_lib_pretty do
  use Bitwise

  def print(term) do
    print(term, 1, 80, -1)
  end

  def print(term, options) when is_list(options) do
    col = get_option(:column, options, 1)
    ll = get_option(:line_length, options, 80)
    d = get_option(:depth, options, -1)
    m = get_option(:line_max_chars, options, -1)
    t = get_option(:chars_limit, options, -1)
    recDefFun = get_option(:record_print_fun, options, :no_fun)
    encoding = get_option(:encoding, options, :epp.default_encoding())
    strings = get_option(:strings, options, true)
    print(term, col, ll, d, m, t, recDefFun, encoding, strings)
  end

  def print(term, recDefFun) do
    print(term, -1, recDefFun)
  end

  def print(term, depth, recDefFun) do
    print(term, 1, 80, depth, recDefFun)
  end

  def print(term, col, ll, d) do
    print(term, col, ll, d, _M = -1, _T = -1, :no_fun, :latin1, true)
  end

  def print(term, col, ll, d, recDefFun) do
    print(term, col, ll, d, _M = -1, recDefFun)
  end

  def print(term, col, ll, d, m, recDefFun) do
    print(term, col, ll, d, m, _T = -1, recDefFun, :latin1, true)
  end

  defp print(_, _, _, 0, _M, _T, _RF, _Enc, _Str) do
    '...'
  end

  defp print(_, _, _, _D, _M, 0, _RF, _Enc, _Str) do
    '...'
  end

  defp print(term, col, ll, d, m, t, recDefFun, enc, str)
       when col <= 0 do
    print(term, 1, ll, d, m, t, recDefFun, enc, str)
  end

  defp print(atom, _Col, _Ll, _D, _M, _T, _RF, enc, _Str)
       when is_atom(atom) do
    write_atom(atom, enc)
  end

  defp print(term, col, ll, d, m0, t, recDefFun, enc, str)
       when is_tuple(term) or is_list(term) or is_map(term) or
              is_bitstring(term) do
    {_, len, _Dots, _} =
      if__ =
      case t < 0 do
        true ->
          print_length(term, d, t, recDefFun, enc, str)

        false ->
          intermediate(term, d, t, recDefFun, enc, str)
      end

    m = max_cs(m0, len)

    cond do
      ll === 0 ->
        write(if__)

      len < ll - col and len <= m ->
        write(if__)

      true ->
        tInd =
          while_fail(
            [-1, 4],
            fn i ->
              cind(if__, col, ll, m, i, 0, 0)
            end,
            1
          )

        pp(if__, col, ll, m, tInd, indent(col), 0, 0)
    end
  end

  defp print(term, _Col, _Ll, _D, _M, _T, _RF, _Enc, _Str) do
    :io_lib.write(term)
  end

  defp max_cs(m, len) when m < 0 do
    len
  end

  defp max_cs(m, _Len) do
    m
  end

  defp pp({_S, len, _, _} = if__, col, ll, m, _TInd, _Ind, lD, w)
       when len < ll - col - lD and len + w + lD <= m do
    write(if__)
  end

  defp pp({{:list, l}, _Len, _, _}, col, ll, m, tInd, ind, lD, w) do
    [?[, pp_list(l, col + 1, ll, m, tInd, indent(1, ind), lD, ?|, w + 1), ?]]
  end

  defp pp({{:tuple, true, l}, _Len, _, _}, col, ll, m, tInd, ind, lD, w) do
    [?{, pp_tag_tuple(l, col, ll, m, tInd, ind, lD, w + 1), ?}]
  end

  defp pp({{:tuple, false, l}, _Len, _, _}, col, ll, m, tInd, ind, lD, w) do
    [?{, pp_list(l, col + 1, ll, m, tInd, indent(1, ind), lD, ?,, w + 1), ?}]
  end

  defp pp({{:map, pairs}, _Len, _, _}, col, ll, m, tInd, ind, lD, w) do
    [?#, ?{, pp_map(pairs, col + 2, ll, m, tInd, indent(2, ind), lD, w + 1), ?}]
  end

  defp pp({{:record, [{name, nLen} | l]}, _Len, _, _}, col, ll, m, tInd, ind, lD, w) do
    [name, ?{, pp_record(l, nLen, col, ll, m, tInd, ind, lD, w + nLen + 1), ?}]
  end

  defp pp({{:bin, s}, _Len, _, _}, col, ll, m, _TInd, ind, lD, w) do
    pp_binary(s, col + 2, ll, m, indent(2, ind), lD, w)
  end

  defp pp({s, _Len, _, _}, _Col, _Ll, _M, _TInd, _Ind, _LD, _W) do
    s
  end

  defp pp_tag_tuple([{tag, tlen, _, _} | l], col, ll, m, tInd, ind, lD, w) do
    tagInd = tlen + 2
    tcol = col + tagInd
    s = ?,

    cond do
      tInd > 0 and tagInd > tInd ->
        col1 = col + tInd
        indent = indent(tInd, ind)
        [tag | pp_tail(l, col1, tcol, ll, m, tInd, indent, lD, s, w + tlen)]

      true ->
        indent = indent(tagInd, ind)
        [tag, s | pp_list(l, tcol, ll, m, tInd, indent, lD, s, w + tlen + 1)]
    end
  end

  defp pp_map([], _Col, _Ll, _M, _TInd, _Ind, _LD, _W) do
    ''
  end

  defp pp_map({:dots, _, _, _}, _Col, _Ll, _M, _TInd, _Ind, _LD, _W) do
    '...'
  end

  defp pp_map([p | ps], col, ll, m, tInd, ind, lD, w) do
    {pS, pW} = pp_pair(p, col, ll, m, tInd, ind, last_depth(ps, lD), w)
    [pS | pp_pairs_tail(ps, col, col + pW, ll, m, tInd, ind, lD, pW)]
  end

  defp pp_pairs_tail([], _Col0, _Col, _Ll, _M, _TInd, _Ind, _LD, _W) do
    ''
  end

  defp pp_pairs_tail({:dots, _, _, _}, _Col0, _Col, _M, _Ll, _TInd, _Ind, _LD, _W) do
    ',...'
  end

  defp pp_pairs_tail([{_, len, _, _} = p | ps], col0, col, ll, m, tInd, ind, lD, w) do
    lD1 = last_depth(ps, lD)
    eLen = 1 + len

    cond do
      (lD1 === 0 and eLen + 1 < ll - col and
         w + eLen + 1 <= m and
         is_list(
           :erlang.element(
             1,
             :erlang.element(
               2,
               :erlang.element(
                 1,
                 p
               )
             )
           )
         ) and
         is_list(
           :erlang.element(
             1,
             :erlang.element(
               3,
               :erlang.element(
                 1,
                 p
               )
             )
           )
         )) or
          (lD1 > 0 and eLen < ll - col - lD1 and
             w + eLen + lD1 <= m and
             is_list(
               :erlang.element(
                 1,
                 :erlang.element(
                   2,
                   :erlang.element(
                     1,
                     p
                   )
                 )
               )
             ) and
             is_list(
               :erlang.element(
                 1,
                 :erlang.element(
                   3,
                   :erlang.element(
                     1,
                     p
                   )
                 )
               )
             )) ->
        [?,, write_pair(p) | pp_pairs_tail(ps, col0, col + eLen, ll, m, tInd, ind, lD, w + eLen)]

      true ->
        {pS, pW} = pp_pair(p, col0, ll, m, tInd, ind, lD1, 0)
        [?,, ?\n, ind, pS | pp_pairs_tail(ps, col0, col0 + pW, ll, m, tInd, ind, lD, pW)]
    end
  end

  defp pp_pair({_, len, _, _} = pair, col, ll, m, _TInd, _Ind, lD, w)
       when len < ll - col - lD and len + w + lD <= m do
    {write_pair(pair),
     cond do
       is_list(
         :erlang.element(
           1,
           :erlang.element(
             2,
             :erlang.element(
               1,
               pair
             )
           )
         )
       ) and
           is_list(
             :erlang.element(
               1,
               :erlang.element(
                 3,
                 :erlang.element(
                   1,
                   pair
                 )
               )
             )
           ) ->
         len

       true ->
         ll
     end}
  end

  defp pp_pair({{:map_pair, k, v}, _Len, _, _}, col0, ll, m, tInd, ind0, lD, w) do
    i = map_value_indent(tInd)
    ind = indent(i, ind0)

    {[
       pp(k, col0, ll, m, tInd, ind0, lD, w),
       ' =>\n',
       ind | pp(v, col0 + i, ll, m, tInd, ind, lD, 0)
     ], ll}
  end

  defp pp_record([], _Nlen, _Col, _Ll, _M, _TInd, _Ind, _LD, _W) do
    ''
  end

  defp pp_record({:dots, _, _, _}, _Nlen, _Col, _Ll, _M, _TInd, _Ind, _LD, _W) do
    '...'
  end

  defp pp_record([f | fs], nlen, col0, ll, m, tInd, ind0, lD, w0) do
    nind = nlen + 1
    {col, ind, s, w} = rec_indent(nind, tInd, col0, ind0, w0)
    {fS, fW} = pp_field(f, col, ll, m, tInd, ind, last_depth(fs, lD), w)
    [s, fS | pp_fields_tail(fs, col, col + fW, ll, m, tInd, ind, lD, w + fW)]
  end

  defp pp_fields_tail([], _Col0, _Col, _Ll, _M, _TInd, _Ind, _LD, _W) do
    ''
  end

  defp pp_fields_tail({:dots, _, _, _}, _Col0, _Col, _M, _Ll, _TInd, _Ind, _LD, _W) do
    ',...'
  end

  defp pp_fields_tail([{_, len, _, _} = f | fs], col0, col, ll, m, tInd, ind, lD, w) do
    lD1 = last_depth(fs, lD)
    eLen = 1 + len

    cond do
      (lD1 === 0 and eLen + 1 < ll - col and
         w + eLen + 1 <= m and
         is_list(
           :erlang.element(
             1,
             :erlang.element(
               4,
               :erlang.element(
                 1,
                 f
               )
             )
           )
         )) or
          (lD1 > 0 and eLen < ll - col - lD1 and
             w + eLen + lD1 <= m and
             is_list(
               :erlang.element(
                 1,
                 :erlang.element(
                   4,
                   :erlang.element(
                     1,
                     f
                   )
                 )
               )
             )) ->
        [
          ?,,
          write_field(f) | pp_fields_tail(fs, col0, col + eLen, ll, m, tInd, ind, lD, w + eLen)
        ]

      true ->
        {fS, fW} = pp_field(f, col0, ll, m, tInd, ind, lD1, 0)
        [?,, ?\n, ind, fS | pp_fields_tail(fs, col0, col0 + fW, ll, m, tInd, ind, lD, fW)]
    end
  end

  defp pp_field({_, len, _, _} = fl, col, ll, m, _TInd, _Ind, lD, w)
       when len < ll - col - lD and len + w + lD <= m do
    {write_field(fl),
     cond do
       is_list(
         :erlang.element(
           1,
           :erlang.element(
             4,
             :erlang.element(
               1,
               fl
             )
           )
         )
       ) ->
         len

       true ->
         ll
     end}
  end

  defp pp_field({{:field, name, nameL, f}, _, _, _}, col0, ll, m, tInd, ind0, lD, w0) do
    {col, ind, s, w} = rec_indent(nameL, tInd, col0, ind0, w0 + nameL)

    sep =
      case s do
        [?\n | _] ->
          ' ='

        _ ->
          ' = '
      end

    {[name, sep, s | pp(f, col, ll, m, tInd, ind, lD, w)], ll}
  end

  defp rec_indent(rInd, tInd, col0, ind0, w0) do
    nl = :erlang.and(tInd > 0, rInd > tInd)

    dCol =
      case nl do
        true ->
          tInd

        false ->
          rInd
      end

    col = col0 + dCol
    ind = indent(dCol, ind0)

    s =
      case nl do
        true ->
          [?\n | ind]

        false ->
          ''
      end

    w =
      case nl do
        true ->
          0

        false ->
          w0
      end

    {col, ind, s, w}
  end

  defp pp_list({:dots, _, _, _}, _Col0, _Ll, _M, _TInd, _Ind, _LD, _S, _W) do
    '...'
  end

  defp pp_list([e | es], col0, ll, m, tInd, ind, lD, s, w) do
    {eS, wE} = pp_element(e, col0, ll, m, tInd, ind, last_depth(es, lD), w)
    [eS | pp_tail(es, col0, col0 + wE, ll, m, tInd, ind, lD, s, w + wE)]
  end

  defp pp_tail([], _Col0, _Col, _Ll, _M, _TInd, _Ind, _LD, _S, _W) do
    []
  end

  defp pp_tail([{_, len, _, _} = e | es], col0, col, ll, m, tInd, ind, lD, s, w) do
    lD1 = last_depth(es, lD)
    eLen = 1 + len

    cond do
      (lD1 === 0 and eLen + 1 < ll - col and
         w + eLen + 1 <= m and is_list(:erlang.element(1, e))) or
          (lD1 > 0 and eLen < ll - col - lD1 and
             w + eLen + lD1 <= m and
             is_list(:erlang.element(1, e))) ->
        [?,, write(e) | pp_tail(es, col0, col + eLen, ll, m, tInd, ind, lD, s, w + eLen)]

      true ->
        {eS, wE} = pp_element(e, col0, ll, m, tInd, ind, lD1, 0)
        [?,, ?\n, ind, eS | pp_tail(es, col0, col0 + wE, ll, m, tInd, ind, lD, s, wE)]
    end
  end

  defp pp_tail({:dots, _, _, _}, _Col0, _Col, _Ll, _M, _TInd, _Ind, _LD, s, _W) do
    [s | '...']
  end

  defp pp_tail({_, len, _, _} = e, _Col0, col, ll, m, _TInd, _Ind, lD, s, w)
       when len + 1 < ll - col - (lD + 1) and
              len + 1 + w + (lD + 1) <= m and
              is_list(:erlang.element(1, e)) do
    [s | write(e)]
  end

  defp pp_tail(e, col0, _Col, ll, m, tInd, ind, lD, s, _W) do
    [s, ?\n, ind | pp(e, col0, ll, m, tInd, ind, lD + 1, 0)]
  end

  defp pp_element({_, len, _, _} = e, col, ll, m, _TInd, _Ind, lD, w)
       when len < ll - col - lD and len + w + lD <= m and
              is_list(:erlang.element(1, e)) do
    {write(e), len}
  end

  defp pp_element(e, col, ll, m, tInd, ind, lD, w) do
    {pp(e, col, ll, m, tInd, ind, lD, w), ll}
  end

  defp pp_binary([lT, lT, s, gT, gT], col, ll, m, ind, lD, w) do
    n =
      :erlang.max(
        8,
        :erlang.min(ll - col, m - 4 - w) - lD
      )

    [lT, lT, pp_binary(s, n, n, ind), gT, gT]
  end

  defp pp_binary([bS, ?, | s], n, n0, ind) do
    len = length(bS) + 1

    case n - len do
      n1 when n1 < 0 ->
        [?\n, ind, bS, ?, | pp_binary(s, n0 - len, n0, ind)]

      n1 ->
        [bS, ?, | pp_binary(s, n1, n0, ind)]
    end
  end

  defp pp_binary([bS1, ?:, bS2] = s, n, _N0, ind)
       when length(bS1) + length(bS2) + 1 > n do
    [?\n, ind, s]
  end

  defp pp_binary(s, n, _N0, ind) do
    case :erlang.iolist_size(s) > n do
      true ->
        [?\n, ind, s]

      false ->
        s
    end
  end

  def write({{:tuple, _IsTagged, l}, _, _, _}) do
    [?{, write_list(l, ?,), ?}]
  end

  def write({{:list, l}, _, _, _}) do
    [?[, write_list(l, ?|), ?]]
  end

  def write({{:map, pairs}, _, _, _}) do
    [?#, ?{, write_list(pairs, ?,), ?}]
  end

  def write({{:map_pair, _K, _V}, _, _, _} = pair) do
    write_pair(pair)
  end

  def write({{:record, [{name, _} | l]}, _, _, _}) do
    [name, ?{, write_fields(l), ?}]
  end

  def write({{:bin, s}, _, _, _}) do
    s
  end

  def write({s, _, _, _}) do
    s
  end

  defp write_pair({{:map_pair, k, v}, _, _, _}) do
    [write(k), ' => ', write(v)]
  end

  defp write_fields([]) do
    ''
  end

  defp write_fields({:dots, _, _, _}) do
    '...'
  end

  defp write_fields([f | fs]) do
    [write_field(f) | write_fields_tail(fs)]
  end

  defp write_fields_tail([]) do
    ''
  end

  defp write_fields_tail({:dots, _, _, _}) do
    ',...'
  end

  defp write_fields_tail([f | fs]) do
    [?,, write_field(f) | write_fields_tail(fs)]
  end

  defp write_field({{:field, name, _NameL, f}, _, _, _}) do
    [name, ' = ' | write(f)]
  end

  defp write_list({:dots, _, _, _}, _S) do
    '...'
  end

  defp write_list([e | es], s) do
    [write(e) | write_tail(es, s)]
  end

  defp write_tail([], _S) do
    []
  end

  defp write_tail([e | es], s) do
    [?,, write(e) | write_tail(es, s)]
  end

  defp write_tail({:dots, _, _, _}, s) do
    [s | '...']
  end

  defp write_tail(e, s) do
    [s | write(e)]
  end

  def intermediate(term, d, t, rF, enc, str) when t > 0 do
    d0 = 1
    if__ = print_length(term, d0, t, rF, enc, str)

    case if__ do
      {_, len, dots, _} when dots === 0 or len > t or d === 1 ->
        if__

      _ ->
        find_upper(if__, term, t, d0, 2, d, rF, enc, str)
    end
  end

  defp find_upper(lower, term, t, dl, dd, d, rF, enc, str) do
    dd2 = dd * 2

    d1 =
      case d < 0 do
        true ->
          dl + dd2

        false ->
          min(dl + dd2, d)
      end

    if__ = expand(lower, t, d1 - dl)

    case if__ do
      {_, _, _Dots = 0, _} ->
        if__

      {_, _Len = ^t, _, _} ->
        if__

      {_, len, _, _} when (len < t and d1 < d) or d < 0 ->
        find_upper(if__, term, t, d1, dd2, d, rF, enc, str)

      _ ->
        search_depth(lower, if__, term, t, dl, d1, rF, enc, str)
    end
  end

  defp search_depth(lower, upper, _Term, t, dl, du, _RF, _Enc, _Str)
       when du - dl === 1 do
    case lower do
      {_, ^t, _, _} ->
        lower

      _ ->
        upper
    end
  end

  defp search_depth(lower, upper, term, t, dl, du, rF, enc, str) do
    d1 = div(dl + du, 2)
    if__ = expand(lower, t, d1 - dl)

    case if__ do
      {_, len, _, _} when len > t ->
        search_depth(lower, if__, term, t, dl, d1, rF, enc, str)

      _ ->
        search_depth(if__, upper, term, t, d1, du, rF, enc, str)
    end
  end

  defp print_length([], _D, _T, _RF, _Enc, _Str) do
    {'[]', 2, 0, :no_more}
  end

  defp print_length({}, _D, _T, _RF, _Enc, _Str) do
    {'{}', 2, 0, :no_more}
  end

  defp print_length(%{} = m, _D, _T, _RF, _Enc, _Str)
       when map_size(m) === 0 do
    {'\#{}', 3, 0, :no_more}
  end

  defp print_length(atom, _D, _T, _RF, enc, _Str)
       when is_atom(atom) do
    s = write_atom(atom, enc)
    {s, :io_lib.chars_length(s), 0, :no_more}
  end

  defp print_length(list, d, t, rF, enc, str) when is_list(list) do
    case str and printable_list(list, d, t, enc) do
      true ->
        s = write_string(list, enc)
        {s, :io_lib.chars_length(s), 0, :no_more}

      {true, prefix} ->
        s = write_string(prefix, enc)
        {[s | '...'], 3 + :io_lib.chars_length(s), 0, :no_more}

      false ->
        case print_length_list(list, d, t, rF, enc, str) do
          {what, len, dots, _More} when dots > 0 ->
            more = fn t1, dd ->
              print_length(list, d + dd, t1, rF, enc, str)
            end

            {what, len, dots, more}

          if__ ->
            if__
        end
    end
  end

  defp print_length(fun, _D, _T, _RF, _Enc, _Str)
       when is_function(fun) do
    s = :io_lib.write(fun)
    {s, :erlang.iolist_size(s), 0, :no_more}
  end

  defp print_length(r, d, t, rF, enc, str)
       when is_atom(:erlang.element(1, r)) and
              is_function(rF) do
    case rF.(:erlang.element(1, r), tuple_size(r) - 1) do
      :no ->
        print_length_tuple(r, d, t, rF, enc, str)

      rDefs ->
        print_length_record(r, d, t, rF, rDefs, enc, str)
    end
  end

  defp print_length(tuple, d, t, rF, enc, str)
       when is_tuple(tuple) do
    print_length_tuple(tuple, d, t, rF, enc, str)
  end

  defp print_length(map, d, t, rF, enc, str) when is_map(map) do
    print_length_map(map, d, t, rF, enc, str)
  end

  defp print_length(<<>>, _D, _T, _RF, _Enc, _Str) do
    {'<<>>', 4, 0, :no_more}
  end

  defp print_length(<<_::bitstring>> = bin, 1, _T, rF, enc, str) do
    more = fn t1, dd ->
      print_length(bin, 1 + dd, t1, rF, enc, str)
    end

    {'<<...>>', 7, 3, more}
  end

  defp print_length(<<_::bitstring>> = bin, d, t, rF, enc, str) do
    d1 = d - 1

    case str and rem(bit_size(bin), 8) === 0 and
           printable_bin0(
             bin,
             d1,
             tsub(t, 6),
             enc
           ) do
      {true, list} when is_list(list) ->
        s = :io_lib.write_string(list, ?")
        {[?<, ?<, s, ?>, ?>], 4 + length(s), 0, :no_more}

      {false, list} when is_list(list) ->
        s = :io_lib.write_string(list, ?")
        {[?<, ?<, s, '/utf8>>'], 9 + :io_lib.chars_length(s), 0, :no_more}

      {true, true, prefix} ->
        s = :io_lib.write_string(prefix, ?")

        more = fn t1, dd ->
          print_length(bin, d + dd, t1, rF, enc, str)
        end

        {[?<, ?<, s | '...>>'], 7 + length(s), 3, more}

      {false, true, prefix} ->
        s = :io_lib.write_string(prefix, ?")

        more = fn t1, dd ->
          print_length(bin, d + dd, t1, rF, enc, str)
        end

        {[?<, ?<, s | '/utf8...>>'], 12 + :io_lib.chars_length(s), 3, more}

      false ->
        case :io_lib.write_binary(bin, d, t) do
          {s, <<>>} ->
            {{:bin, s}, :erlang.iolist_size(s), 0, :no_more}

          {s, _Rest} ->
            more = fn t1, dd ->
              print_length(bin, d + dd, t1, rF, enc, str)
            end

            {{:bin, s}, :erlang.iolist_size(s), 3, more}
        end
    end
  end

  defp print_length(term, _D, _T, _RF, _Enc, _Str) do
    s = :io_lib.write(term)
    {s, :io_lib.chars_length(s), 0, :no_more}
  end

  defp print_length_map(map, 1, _T, rF, enc, str) do
    more = fn t1, dd ->
      print_length_map(map, 1 + dd, t1, rF, enc, str)
    end

    {'\#{...}', 6, 3, more}
  end

  defp print_length_map(map, d, t, rF, enc, str) when is_map(map) do
    next = :maps.next(:maps.iterator(map))
    pairsS = print_length_map_pairs(next, d, d - 1, tsub(t, 3), rF, enc, str)
    {len, dots} = list_length(pairsS, 3, 0)
    {{:map, pairsS}, len, dots, :no_more}
  end

  defp print_length_map_pairs(:none, _D, _D0, _T, _RF, _Enc, _Str) do
    []
  end

  defp print_length_map_pairs(term, d, d0, t, rF, enc, str)
       when d === 1 or
              t === 0 do
    more = fn t1, dd ->
      print_length_map_pairs(term, d + dd, d0, t1, rF, enc, str)
    end

    {:dots, 3, 3, more}
  end

  defp print_length_map_pairs({k, v, iter}, d, d0, t, rF, enc, str) do
    pair1 = print_length_map_pair(k, v, d0, tsub(t, 1), rF, enc, str)
    {_, len1, _, _} = pair1
    next = :maps.next(iter)
    [pair1 | print_length_map_pairs(next, d - 1, d0, tsub(t, len1 + 1), rF, enc, str)]
  end

  defp print_length_map_pair(k, v, d, t, rF, enc, str) do
    {_, kL, kD, _} = p1 = print_length(k, d, t, rF, enc, str)
    kL1 = kL + 4
    {_, vL, vD, _} = p2 = print_length(v, d, tsub(t, kL1), rF, enc, str)
    {{:map_pair, p1, p2}, kL1 + vL, kD + vD, :no_more}
  end

  defp print_length_tuple(tuple, 1, _T, rF, enc, str) do
    more = fn t1, dd ->
      print_length_tuple(tuple, 1 + dd, t1, rF, enc, str)
    end

    {'{...}', 5, 3, more}
  end

  defp print_length_tuple(tuple, d, t, rF, enc, str) do
    l = print_length_tuple1(tuple, 1, d, tsub(t, 2), rF, enc, str)

    isTagged =
      :erlang.and(
        is_atom(
          :erlang.element(
            1,
            tuple
          )
        ),
        tuple_size(tuple) > 1
      )

    {len, dots} = list_length(l, 2, 0)
    {{:tuple, isTagged, l}, len, dots, :no_more}
  end

  defp print_length_tuple1(tuple, i, _D, _T, _RF, _Enc, _Str)
       when i > tuple_size(tuple) do
    []
  end

  defp print_length_tuple1(tuple, i, d, t, rF, enc, str)
       when d === 1 or
              t === 0 do
    more = fn t1, dd ->
      print_length_tuple1(tuple, i, d + dd, t1, rF, enc, str)
    end

    {:dots, 3, 3, more}
  end

  defp print_length_tuple1(tuple, i, d, t, rF, enc, str) do
    e = :erlang.element(i, tuple)
    t1 = tsub(t, 1)
    {_, len1, _, _} = elem1 = print_length(e, d - 1, t1, rF, enc, str)
    t2 = tsub(t1, len1)
    [elem1 | print_length_tuple1(tuple, i + 1, d - 1, t2, rF, enc, str)]
  end

  defp print_length_record(tuple, 1, _T, rF, rDefs, enc, str) do
    more = fn t1, dd ->
      print_length_record(tuple, 1 + dd, t1, rF, rDefs, enc, str)
    end

    {'{...}', 5, 3, more}
  end

  defp print_length_record(tuple, d, t, rF, rDefs, enc, str) do
    name = [?# | write_atom(:erlang.element(1, tuple), enc)]
    nameL = :io_lib.chars_length(name)
    t1 = tsub(t, nameL + 2)
    l = print_length_fields(rDefs, d - 1, t1, tuple, 2, rF, enc, str)
    {len, dots} = list_length(l, nameL + 2, 0)
    {{:record, [{name, nameL} | l]}, len, dots, :no_more}
  end

  defp print_length_fields([], _D, _T, tuple, i, _RF, _Enc, _Str)
       when i > tuple_size(tuple) do
    []
  end

  defp print_length_fields(term, d, t, tuple, i, rF, enc, str)
       when d === 1 or t === 0 do
    more = fn t1, dd ->
      print_length_fields(term, d + dd, t1, tuple, i, rF, enc, str)
    end

    {:dots, 3, 3, more}
  end

  defp print_length_fields([def__ | defs], d, t, tuple, i, rF, enc, str) do
    e = :erlang.element(i, tuple)
    t1 = tsub(t, 1)
    field1 = print_length_field(def__, d - 1, t1, e, rF, enc, str)
    {_, len1, _, _} = field1
    t2 = tsub(t1, len1)
    [field1 | print_length_fields(defs, d - 1, t2, tuple, i + 1, rF, enc, str)]
  end

  defp print_length_field(def__, d, t, e, rF, enc, str) do
    name = write_atom(def__, enc)
    nameL = :io_lib.chars_length(name) + 3
    {_, len, dots, _} = field = print_length(e, d, tsub(t, nameL), rF, enc, str)
    {{:field, name, nameL, field}, nameL + len, dots, :no_more}
  end

  defp print_length_list(list, d, t, rF, enc, str) do
    l = print_length_list1(list, d, tsub(t, 2), rF, enc, str)
    {len, dots} = list_length(l, 2, 0)
    {{:list, l}, len, dots, :no_more}
  end

  defp print_length_list1([], _D, _T, _RF, _Enc, _Str) do
    []
  end

  defp print_length_list1(term, d, t, rF, enc, str)
       when d === 1 or
              t === 0 do
    more = fn t1, dd ->
      print_length_list1(term, d + dd, t1, rF, enc, str)
    end

    {:dots, 3, 3, more}
  end

  defp print_length_list1([e | es], d, t, rF, enc, str) do
    {_, len1, _, _} = elem1 = print_length(e, d - 1, tsub(t, 1), rF, enc, str)
    [elem1 | print_length_list1(es, d - 1, tsub(t, len1 + 1), rF, enc, str)]
  end

  defp print_length_list1(e, d, t, rF, enc, str) do
    print_length(e, d - 1, t, rF, enc, str)
  end

  defp list_length([], acc, dotsAcc) do
    {acc, dotsAcc}
  end

  defp list_length([{_, len, dots, _} | es], acc, dotsAcc) do
    list_length_tail(es, acc + len, dotsAcc + dots)
  end

  defp list_length({_, len, dots, _}, acc, dotsAcc) do
    {acc + len, dotsAcc + dots}
  end

  defp list_length_tail([], acc, dotsAcc) do
    {acc, dotsAcc}
  end

  defp list_length_tail([{_, len, dots, _} | es], acc, dotsAcc) do
    list_length_tail(es, acc + 1 + len, dotsAcc + dots)
  end

  defp list_length_tail({_, len, dots, _}, acc, dotsAcc) do
    {acc + 1 + len, dotsAcc + dots}
  end

  defp printable_list(_L, 1, _T, _Enc) do
    false
  end

  defp printable_list(l, _D, t, :latin1) when t < 0 do
    :io_lib.printable_latin1_list(l)
  end

  defp printable_list(l, _D, t, :latin1) when t >= 0 do
    n = tsub(t, 2)

    case printable_latin1_list(l, n) do
      :all ->
        true

      0 ->
        {l1, _} = :lists.split(n, l)
        {true, l1}

      _NC ->
        false
    end
  end

  defp printable_list(l, _D, t, _Unicode) when t >= 0 do
    n = tsub(t, 2)

    try do
      :string.slice(l, 0, n)
    catch
      _, _ ->
        false
    else
      '' ->
        false

      prefix ->
        case is_flat(l, :lists.flatlength(prefix)) do
          true ->
            case :string.equal(prefix, l) do
              true ->
                :io_lib.printable_list(l)

              false ->
                :io_lib.printable_list(prefix) and {true, prefix}
            end

          false ->
            false
        end
    end
  end

  defp printable_list(l, _D, t, _Uni) when t < 0 do
    :io_lib.printable_list(l)
  end

  defp is_flat(_L, 0) do
    true
  end

  defp is_flat([c | cs], n) when is_integer(c) do
    is_flat(cs, n - 1)
  end

  defp is_flat(_, _N) do
    false
  end

  defp printable_bin0(bin, d, t, enc) do
    len =
      case d >= 0 do
        true ->
          dChars = :erlang.min(4 * d, byte_size(bin))

          case t >= 0 do
            true ->
              :erlang.min(t, dChars)

            false ->
              dChars
          end

        false when t < 0 ->
          byte_size(bin)

        false when t >= 0 ->
          t
      end

    printable_bin(bin, len, d, enc)
  end

  defp printable_bin(_Bin, 0, _D, _Enc) do
    false
  end

  defp printable_bin(bin, len, d, :latin1) do
    n = :erlang.min(20, len)
    l = :erlang.binary_to_list(bin, 1, n)

    case printable_latin1_list(l, n) do
      :all when n === byte_size(bin) ->
        {true, l}

      :all when n === len ->
        {true, true, l}

      :all ->
        case printable_bin1(bin, 1 + n, len - n) do
          0 when byte_size(bin) === len ->
            {true, :erlang.binary_to_list(bin)}

          nC when d > 0 and len - nC >= d ->
            {true, true, :erlang.binary_to_list(bin, 1, len - nC)}

          nC when is_integer(nC) ->
            false
        end

      nC when is_integer(nC) and d > 0 and n - nC >= d ->
        {true, true, :erlang.binary_to_list(bin, 1, n - nC)}

      nC when is_integer(nC) ->
        false
    end
  end

  defp printable_bin(bin, len, d, _Uni) do
    case valid_utf8(bin, len) do
      true ->
        case printable_unicode(bin, len, [], :io.printable_range()) do
          {_, <<>>, l} ->
            {byte_size(bin) === length(l), l}

          {nC, bin1, l} when d > 0 and len - nC >= d ->
            {byte_size(bin) - byte_size(bin1) === length(l), true, l}

          {_NC, _Bin, _L} ->
            false
        end

      false ->
        printable_bin(bin, len, d, :latin1)
    end
  end

  defp printable_bin1(_Bin, _Start, 0) do
    0
  end

  defp printable_bin1(bin, start, len) do
    n = :erlang.min(10000, len)
    l = :erlang.binary_to_list(bin, start, start + n - 1)

    case printable_latin1_list(l, n) do
      :all ->
        printable_bin1(bin, start + n, len - n)

      nC when is_integer(nC) ->
        len - (n - nC)
    end
  end

  defp printable_latin1_list([_ | _], 0) do
    0
  end

  defp printable_latin1_list([c | cs], n) when c >= ?\s and c <= ?~ do
    printable_latin1_list(cs, n - 1)
  end

  defp printable_latin1_list([c | cs], n) when c >= 160 and c <= 255 do
    printable_latin1_list(cs, n - 1)
  end

  defp printable_latin1_list([?\n | cs], n) do
    printable_latin1_list(cs, n - 1)
  end

  defp printable_latin1_list([?\r | cs], n) do
    printable_latin1_list(cs, n - 1)
  end

  defp printable_latin1_list([?\t | cs], n) do
    printable_latin1_list(cs, n - 1)
  end

  defp printable_latin1_list([?\v | cs], n) do
    printable_latin1_list(cs, n - 1)
  end

  defp printable_latin1_list([?\b | cs], n) do
    printable_latin1_list(cs, n - 1)
  end

  defp printable_latin1_list([?\f | cs], n) do
    printable_latin1_list(cs, n - 1)
  end

  defp printable_latin1_list([?\e | cs], n) do
    printable_latin1_list(cs, n - 1)
  end

  defp printable_latin1_list([], _) do
    :all
  end

  defp printable_latin1_list(_, n) do
    n
  end

  defp valid_utf8(<<>>, _) do
    true
  end

  defp valid_utf8(_, 0) do
    true
  end

  defp valid_utf8(<<_::utf8, r::binary>>, n) do
    valid_utf8(r, n - 1)
  end

  defp valid_utf8(_, _) do
    false
  end

  defp printable_unicode(<<c::utf8, r::binary>> = bin, i, l, range)
       when i > 0 do
    case printable_char(c, range) do
      true ->
        printable_unicode(r, i - 1, [c | l], range)

      false ->
        {i, bin, :lists.reverse(l)}
    end
  end

  defp printable_unicode(bin, i, l, _) do
    {i, bin, :lists.reverse(l)}
  end

  defp printable_char(?\n, _) do
    true
  end

  defp printable_char(?\r, _) do
    true
  end

  defp printable_char(?\t, _) do
    true
  end

  defp printable_char(?\v, _) do
    true
  end

  defp printable_char(?\b, _) do
    true
  end

  defp printable_char(?\f, _) do
    true
  end

  defp printable_char(?\e, _) do
    true
  end

  defp printable_char(c, :latin1) do
    (c >= ?\s and c <= ?~) or (c >= 160 and c <= 255)
  end

  defp printable_char(c, :unicode) do
    (c >= ?\s and c <= ?~) or (c >= 160 and c < 55296) or (c > 57343 and c < 65534) or
      (c > 65535 and c <= 1_114_111)
  end

  defp write_atom(a, :latin1) do
    :io_lib.write_atom_as_latin1(a)
  end

  defp write_atom(a, _Uni) do
    :io_lib.write_atom(a)
  end

  defp write_string(s, :latin1) do
    :io_lib.write_latin1_string(s, ?")
  end

  defp write_string(s, _Uni) do
    :io_lib.write_string(s, ?")
  end

  defp expand({_, _, _Dots = 0, :no_more} = if__, _T, _Dd) do
    if__
  end

  defp expand({{:tuple, isTagged, l}, _Len, _, :no_more}, t, dd) do
    {nL, nLen, nDots} = expand_list(l, t, dd, 2)
    {{:tuple, isTagged, nL}, nLen, nDots, :no_more}
  end

  defp expand({{:map, pairs}, _Len, _, :no_more}, t, dd) do
    {nPairs, nLen, nDots} = expand_list(pairs, t, dd, 3)
    {{:map, nPairs}, nLen, nDots, :no_more}
  end

  defp expand({{:map_pair, k, v}, _Len, _, :no_more}, t, dd) do
    {_, kL, kD, _} = p1 = expand(k, tsub(t, 1), dd)
    kL1 = kL + 4
    {_, vL, vD, _} = p2 = expand(v, tsub(t, kL1), dd)
    {{:map_pair, p1, p2}, kL1 + vL, kD + vD, :no_more}
  end

  defp expand({{:record, [{name, nameL} | l]}, _Len, _, :no_more}, t, dd) do
    {nL, nLen, nDots} = expand_list(l, t, dd, nameL + 2)
    {{:record, [{name, nameL} | nL]}, nLen, nDots, :no_more}
  end

  defp expand({{:field, name, nameL, field}, _Len, _, :no_more}, t, dd) do
    f = {_S, l, dots, _} = expand(field, tsub(t, nameL), dd)
    {{:field, name, nameL, f}, nameL + l, dots, :no_more}
  end

  defp expand({_, _, _, more}, t, dd) do
    more.(t, dd)
  end

  defp expand_list(ifs, t, dd, l0) do
    l = expand_list(ifs, tsub(t, l0), dd)
    {len, dots} = list_length(l, l0, 0)
    {l, len, dots}
  end

  defp expand_list([], _T, _Dd) do
    []
  end

  defp expand_list([if__ | ifs], t, dd) do
    {_, len1, _, _} = elem1 = expand(if__, tsub(t, 1), dd)
    [elem1 | expand_list(ifs, tsub(t, len1 + 1), dd)]
  end

  defp expand_list({_, _, _, more}, t, dd) do
    more.(t, dd)
  end

  defp tsub(t, _) when t < 0 do
    t
  end

  defp tsub(t, e) when t >= e do
    t - e
  end

  defp tsub(_, _) do
    0
  end

  defp cind({_S, len, _, _}, col, ll, m, ind, lD, w)
       when len < ll - col - lD and len + w + lD <= m do
    ind
  end

  defp cind({{:list, l}, _Len, _, _}, col, ll, m, ind, lD, w) do
    cind_list(l, col + 1, ll, m, ind, lD, w + 1)
  end

  defp cind({{:tuple, true, l}, _Len, _, _}, col, ll, m, ind, lD, w) do
    cind_tag_tuple(l, col, ll, m, ind, lD, w + 1)
  end

  defp cind({{:tuple, false, l}, _Len, _, _}, col, ll, m, ind, lD, w) do
    cind_list(l, col + 1, ll, m, ind, lD, w + 1)
  end

  defp cind({{:map, pairs}, _Len, _, _}, col, ll, m, ind, lD, w) do
    cind_map(pairs, col + 2, ll, m, ind, lD, w + 2)
  end

  defp cind({{:record, [{_Name, nLen} | l]}, _Len, _, _}, col, ll, m, ind, lD, w) do
    cind_record(l, nLen, col, ll, m, ind, lD, w + nLen + 1)
  end

  defp cind({{:bin, _S}, _Len, _, _}, _Col, _Ll, _M, ind, _LD, _W) do
    ind
  end

  defp cind({_S, _Len, _, _}, _Col, _Ll, _M, ind, _LD, _W) do
    ind
  end

  defp cind_tag_tuple([{_Tag, tlen, _, _} | l], col, ll, m, ind, lD, w) do
    tagInd = tlen + 2
    tcol = col + tagInd

    cond do
      ind > 0 and tagInd > ind ->
        col1 = col + ind

        cond do
          m + col1 <= ll or col1 <= div(ll, 2) ->
            cind_tail(l, col1, tcol, ll, m, ind, lD, w + tlen)

          true ->
            throw(:no_good)
        end

      m + tcol < ll or tcol < div(ll, 2) ->
        cind_list(l, tcol, ll, m, ind, lD, w + tlen + 1)

      true ->
        throw(:no_good)
    end
  end

  defp cind_map([p | ps], col, ll, m, ind, lD, w) do
    pW = cind_pair(p, col, ll, m, ind, last_depth(ps, lD), w)
    cind_pairs_tail(ps, col, col + pW, ll, m, ind, lD, w + pW)
  end

  defp cind_map(_, _Col, _Ll, _M, ind, _LD, _W) do
    ind
  end

  defp cind_pairs_tail([{_, len, _, _} = p | ps], col0, col, ll, m, ind, lD, w) do
    lD1 = last_depth(ps, lD)
    eLen = 1 + len

    cond do
      (lD1 === 0 and eLen + 1 < ll - col and
         w + eLen + 1 <= m and
         is_list(
           :erlang.element(
             1,
             :erlang.element(
               2,
               :erlang.element(
                 1,
                 p
               )
             )
           )
         ) and
         is_list(
           :erlang.element(
             1,
             :erlang.element(
               3,
               :erlang.element(
                 1,
                 p
               )
             )
           )
         )) or
          (lD1 > 0 and eLen < ll - col - lD1 and
             w + eLen + lD1 <= m and
             is_list(
               :erlang.element(
                 1,
                 :erlang.element(
                   2,
                   :erlang.element(
                     1,
                     p
                   )
                 )
               )
             ) and
             is_list(
               :erlang.element(
                 1,
                 :erlang.element(
                   3,
                   :erlang.element(
                     1,
                     p
                   )
                 )
               )
             )) ->
        cind_pairs_tail(ps, col0, col + eLen, ll, m, ind, lD, w + eLen)

      true ->
        pW = cind_pair(p, col0, ll, m, ind, lD1, 0)
        cind_pairs_tail(ps, col0, col0 + pW, ll, m, ind, lD, pW)
    end
  end

  defp cind_pairs_tail(_, _Col0, _Col, _Ll, _M, ind, _LD, _W) do
    ind
  end

  defp cind_pair({{:map_pair, _Key, _Value}, len, _, _} = pair, col, ll, m, _Ind, lD, w)
       when len < ll - col - lD and len + w + lD <= m do
    cond do
      is_list(
        :erlang.element(
          1,
          :erlang.element(
            2,
            :erlang.element(
              1,
              pair
            )
          )
        )
      ) and
          is_list(
            :erlang.element(
              1,
              :erlang.element(
                3,
                :erlang.element(
                  1,
                  pair
                )
              )
            )
          ) ->
        len

      true ->
        ll
    end
  end

  defp cind_pair({{:map_pair, k, v}, _Len, _, _}, col0, ll, m, ind, lD, w0) do
    cind(k, col0, ll, m, ind, lD, w0)
    i = map_value_indent(ind)
    cind(v, col0 + i, ll, m, ind, lD, 0)
    ll
  end

  defp map_value_indent(tInd) do
    case tInd > 0 do
      true ->
        tInd

      false ->
        4
    end
  end

  defp cind_record([f | fs], nlen, col0, ll, m, ind, lD, w0) do
    nind = nlen + 1
    {col, w} = cind_rec(nind, col0, ll, m, ind, w0)
    fW = cind_field(f, col, ll, m, ind, last_depth(fs, lD), w)
    cind_fields_tail(fs, col, col + fW, ll, m, ind, lD, w + fW)
  end

  defp cind_record(_, _Nlen, _Col, _Ll, _M, ind, _LD, _W) do
    ind
  end

  defp cind_fields_tail([{_, len, _, _} = f | fs], col0, col, ll, m, ind, lD, w) do
    lD1 = last_depth(fs, lD)
    eLen = 1 + len

    cond do
      (lD1 === 0 and eLen + 1 < ll - col and
         w + eLen + 1 <= m and
         is_list(
           :erlang.element(
             1,
             :erlang.element(
               4,
               :erlang.element(
                 1,
                 f
               )
             )
           )
         )) or
          (lD1 > 0 and eLen < ll - col - lD1 and
             w + eLen + lD1 <= m and
             is_list(
               :erlang.element(
                 1,
                 :erlang.element(
                   4,
                   :erlang.element(
                     1,
                     f
                   )
                 )
               )
             )) ->
        cind_fields_tail(fs, col0, col + eLen, ll, m, ind, lD, w + eLen)

      true ->
        fW = cind_field(f, col0, ll, m, ind, lD1, 0)
        cind_fields_tail(fs, col0, col + fW, ll, m, ind, lD, fW)
    end
  end

  defp cind_fields_tail(_, _Col0, _Col, _Ll, _M, ind, _LD, _W) do
    ind
  end

  defp cind_field({{:field, _N, _NL, _F}, len, _, _} = fl, col, ll, m, _Ind, lD, w)
       when len < ll - col - lD and len + w + lD <= m do
    cond do
      is_list(
        :erlang.element(
          1,
          :erlang.element(4, :erlang.element(1, fl))
        )
      ) ->
        len

      true ->
        ll
    end
  end

  defp cind_field({{:field, _Name, nameL, f}, _Len, _, _}, col0, ll, m, ind, lD, w0) do
    {col, w} = cind_rec(nameL, col0, ll, m, ind, w0 + nameL)
    cind(f, col, ll, m, ind, lD, w)
    ll
  end

  defp cind_rec(rInd, col0, ll, m, ind, w0) do
    nl = :erlang.and(ind > 0, rInd > ind)

    dCol =
      case nl do
        true ->
          ind

        false ->
          rInd
      end

    col = col0 + dCol

    cond do
      m + col <= ll or col <= div(ll, 2) ->
        w =
          case nl do
            true ->
              0

            false ->
              w0
          end

        {col, w}

      true ->
        throw(:no_good)
    end
  end

  defp cind_list({:dots, _, _, _}, _Col0, _Ll, _M, ind, _LD, _W) do
    ind
  end

  defp cind_list([e | es], col0, ll, m, ind, lD, w) do
    wE = cind_element(e, col0, ll, m, ind, last_depth(es, lD), w)
    cind_tail(es, col0, col0 + wE, ll, m, ind, lD, w + wE)
  end

  defp cind_tail([], _Col0, _Col, _Ll, _M, ind, _LD, _W) do
    ind
  end

  defp cind_tail([{_, len, _, _} = e | es], col0, col, ll, m, ind, lD, w) do
    lD1 = last_depth(es, lD)
    eLen = 1 + len

    cond do
      (lD1 === 0 and eLen + 1 < ll - col and
         w + eLen + 1 <= m and is_list(:erlang.element(1, e))) or
          (lD1 > 0 and eLen < ll - col - lD1 and
             w + eLen + lD1 <= m and
             is_list(:erlang.element(1, e))) ->
        cind_tail(es, col0, col + eLen, ll, m, ind, lD, w + eLen)

      true ->
        wE = cind_element(e, col0, ll, m, ind, lD1, 0)
        cind_tail(es, col0, col0 + wE, ll, m, ind, lD, wE)
    end
  end

  defp cind_tail({:dots, _, _, _}, _Col0, _Col, _Ll, _M, ind, _LD, _W) do
    ind
  end

  defp cind_tail({_, len, _, _} = e, _Col0, col, ll, m, ind, lD, w)
       when len + 1 < ll - col - (lD + 1) and
              len + 1 + w + (lD + 1) <= m and
              is_list(:erlang.element(1, e)) do
    ind
  end

  defp cind_tail(e, _Col0, col, ll, m, ind, lD, _W) do
    cind(e, col, ll, m, ind, lD + 1, 0)
  end

  defp cind_element({_, len, _, _} = e, col, ll, m, _Ind, lD, w)
       when len < ll - col - lD and len + w + lD <= m and
              is_list(:erlang.element(1, e)) do
    len
  end

  defp cind_element(e, col, ll, m, ind, lD, w) do
    cind(e, col, ll, m, ind, lD, w)
    ll
  end

  defp last_depth([_ | _], _LD) do
    0
  end

  defp last_depth(_, lD) do
    lD + 1
  end

  defp while_fail([], _F, v) do
    v
  end

  defp while_fail([a | as], f, v) do
    try do
      f.(a)
    catch
      _ ->
        while_fail(as, f, v)
    end
  end

  defp indent(n) when is_integer(n) and n > 0 do
    chars(?\s, n - 1)
  end

  defp indent(1, ind) do
    [?\s | ind]
  end

  defp indent(4, ind) do
    s2 = [?\s, ?\s]
    [s2, s2 | ind]
  end

  defp indent(n, ind) when is_integer(n) and n > 0 do
    [chars(?\s, n) | ind]
  end

  defp chars(_C, 0) do
    []
  end

  defp chars(c, 2) do
    [c, c]
  end

  defp chars(c, 3) do
    [c, c, c]
  end

  defp chars(c, n) when n &&& 1 === 0 do
    s = chars(c, n >>> 1)
    [s | s]
  end

  defp chars(c, n) do
    s = chars(c, n >>> 1)
    [c, s | s]
  end

  defp get_option(key, tupleList, default) do
    case :lists.keyfind(key, 1, tupleList) do
      false ->
        default

      {^key, value} ->
        value

      _ ->
        default
    end
  end
end
