defmodule :m_xmerl_regexp do
  use Bitwise

  import :lists,
    only: [
      duplicate: 2,
      foldl: 3,
      keysearch: 3,
      keysort: 2,
      last: 1,
      map: 2,
      member: 2,
      reverse: 1,
      reverse: 2,
      seq: 2
    ]

  import :ordsets, only: [add_element: 2, is_element: 2, subtract: 2, union: 2]
  import :string, only: [substr: 2, substr: 3]

  def setup(rE0) do
    rE = setup(rE0, [?^])
    pid = spawn(:xmerl_regexp, :compile_proc, [self(), rE])

    receive do
      {:ok, result} ->
        result
    after
      2000 ->
        :erlang.exit(pid, :force)
        parse(rE)
    end
  end

  def compile_proc(from, rE) do
    res = compile(rE)
    send(from, {:ok, res})
  end

  defp setup([?\\, ?d | s], acc) do
    setup(s, ']9-0[' ++ acc)
  end

  defp setup([?\\, ?D | s], acc) do
    setup(s, ']9-0^[' ++ acc)
  end

  defp setup([?\\, ?s | s], acc) do
    setup(s, ']s\\t\\n\\r\\[' ++ acc)
  end

  defp setup([?\\, ?S | s], acc) do
    setup(s, ']\\s\\t\\n\\r^[' ++ acc)
  end

  defp setup([?\\, ?i | s], acc) do
    setup(s, ']z-aZ-A_:[' ++ acc)
  end

  defp setup([?\\, ?I | s], acc) do
    setup(s, ']z-aZ-A_:^[' ++ acc)
  end

  defp setup([?\\, ?c | s], acc) do
    setup(s, ']9-0z-aZ-A_:.' ++ [183] ++ '-[' ++ acc)
  end

  defp setup([?\\, ?C | s], acc) do
    setup(s, ']9-0z-aZ-A_:.' ++ [183] ++ '-^[' ++ acc)
  end

  defp setup([a | s], acc) do
    setup(s, [a | acc])
  end

  defp setup([], acc) do
    reverse([?$ | acc])
  end

  def sh_to_awk(sh) do
    '^(' ++ sh_to_awk_1(sh)
  end

  defp sh_to_awk_1([?* | sh]) do
    '.*' ++ sh_to_awk_1(sh)
  end

  defp sh_to_awk_1([?? | sh]) do
    [?. | sh_to_awk_1(sh)]
  end

  defp sh_to_awk_1([?[, ?^, ?] | sh]) do
    '\\^' ++ sh_to_awk_1(sh)
  end

  defp sh_to_awk_1('[^' ++ sh) do
    [?[ | sh_to_awk_2(sh, true)]
  end

  defp sh_to_awk_1('[!' ++ sh) do
    '[^' ++ sh_to_awk_2(sh, false)
  end

  defp sh_to_awk_1([?[ | sh]) do
    [?[ | sh_to_awk_2(sh, false)]
  end

  defp sh_to_awk_1([c | sh]) do
    case sh_special_char(c) do
      true ->
        [?\\, c | sh_to_awk_1(sh)]

      false ->
        [c | sh_to_awk_1(sh)]
    end
  end

  defp sh_to_awk_1([]) do
    ')$'
  end

  defp sh_to_awk_2([?] | sh], upArrow) do
    [?] | sh_to_awk_3(sh, upArrow)]
  end

  defp sh_to_awk_2(sh, upArrow) do
    sh_to_awk_3(sh, upArrow)
  end

  defp sh_to_awk_3([?] | sh], true) do
    '^]' ++ sh_to_awk_1(sh)
  end

  defp sh_to_awk_3([?] | sh], false) do
    [?] | sh_to_awk_1(sh)]
  end

  defp sh_to_awk_3([c | sh], upArrow) do
    [c | sh_to_awk_3(sh, upArrow)]
  end

  defp sh_to_awk_3([], true) do
    [?^ | sh_to_awk_1([])]
  end

  defp sh_to_awk_3([], false) do
    sh_to_awk_1([])
  end

  defp sh_special_char(?|) do
    true
  end

  defp sh_special_char(?*) do
    true
  end

  defp sh_special_char(?+) do
    true
  end

  defp sh_special_char(??) do
    true
  end

  defp sh_special_char(?() do
    true
  end

  defp sh_special_char(?)) do
    true
  end

  defp sh_special_char(?\\) do
    true
  end

  defp sh_special_char(?^) do
    true
  end

  defp sh_special_char(?$) do
    true
  end

  defp sh_special_char(?.) do
    true
  end

  defp sh_special_char(?[) do
    true
  end

  defp sh_special_char(?]) do
    true
  end

  defp sh_special_char(?") do
    true
  end

  defp sh_special_char(_C) do
    false
  end

  def parse(s) do
    case (try do
            reg(s, 0)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {r, sc, []} ->
        {:ok, {:regexp, {r, sc}}}

      {_R, _Sc, [c | _]} ->
        {:error, {:illegal, [c]}}

      {:error, e} ->
        {:error, e}
    end
  end

  def format_error({:interval_range, what}) do
    ['illegal interval range', :io_lib.write_string(what)]
  end

  def format_error({:illegal, what}) do
    ['illegal character `', what, '\'']
  end

  def format_error({:unterminated, what}) do
    ['unterminated `', what, '\'']
  end

  def format_error({:posix_cc, what}) do
    ['illegal POSIX character class ', :io_lib.write_string(what)]
  end

  def format_error({:char_class, what}) do
    ['illegal character class ', :io_lib.write_string(what)]
  end

  def match(s, regExp) when is_list(regExp) do
    case parse(regExp) do
      {:ok, rE} ->
        match(s, rE)

      {:error, e} ->
        {:error, e}
    end
  end

  def match(s, {:regexp, rE}) do
    case match_re(rE, s, 1, 0, -1) do
      {start, len} when len >= 0 ->
        {:match, start, len}

      {_Start, _Len} ->
        :nomatch
    end
  end

  def match(s, {:comp_regexp, rE}) do
    case match_comp(rE, s, 1, 0, -1) do
      {start, len} when len >= 0 ->
        {:match, start, len}

      {_Start, _Len} ->
        :nomatch
    end
  end

  defp match_re(rE, [_ | cs] = s0, p0, mst, mlen) do
    case re_apply(s0, p0, rE) do
      {:match, p1, _S1, _Subs} ->
        len = p1 - p0

        cond do
          len > mlen ->
            match_re(rE, cs, p0 + 1, p0, len)

          true ->
            match_re(rE, cs, p0 + 1, mst, mlen)
        end

      :nomatch ->
        match_re(rE, cs, p0 + 1, mst, mlen)

      :never_match ->
        {mst, mlen}
    end
  end

  defp match_re(_RE, _S, _P, mst, mlen) do
    {mst, mlen}
  end

  defp match_comp(rE, [_ | cs] = s0, p0, mst, mlen) do
    case comp_apply(s0, p0, rE) do
      {:match, p1, _S1} ->
        len = p1 - p0

        cond do
          len > mlen ->
            match_comp(rE, cs, p0 + 1, p0, len)

          true ->
            match_comp(rE, cs, p0 + 1, mst, mlen)
        end

      :nomatch ->
        match_comp(rE, cs, p0 + 1, mst, mlen)
    end
  end

  defp match_comp(_RE, _S, _P, mst, mlen) do
    {mst, mlen}
  end

  def first_match(s, regExp) when is_list(regExp) do
    case parse(regExp) do
      {:ok, rE} ->
        first_match(s, rE)

      {:error, e} ->
        {:error, e}
    end
  end

  def first_match(s, {:regexp, rE}) do
    case first_match_re(rE, s, 1) do
      {start, len, _} ->
        {:match, start, len}

      :nomatch ->
        :nomatch
    end
  end

  def first_match(s, {:comp_regexp, rE}) do
    case first_match_comp(rE, s, 1) do
      {start, len} ->
        {:match, start, len}

      :nomatch ->
        :nomatch
    end
  end

  defp first_match_re(rE, s, st) when s != [] do
    case re_apply(s, st, rE) do
      {:match, p, _Rest, subs} ->
        {st, p - st, subs}

      :nomatch ->
        first_match_re(rE, tl(s), st + 1)

      :never_match ->
        :nomatch
    end
  end

  defp first_match_re(_RE, [], _St) do
    :nomatch
  end

  defp first_match_comp(rE, s, st) when s != [] do
    case comp_apply(s, st, rE) do
      {:match, p, _Rest} ->
        {st, p - st}

      :nomatch ->
        first_match_comp(rE, tl(s), st + 1)
    end
  end

  defp first_match_comp(_RE, [], _St) do
    :nomatch
  end

  def matches(s, regExp) when is_list(regExp) do
    case parse(regExp) do
      {:ok, rE} ->
        matches(s, rE)

      {:error, e} ->
        {:error, e}
    end
  end

  def matches(s, {:regexp, rE}) do
    {:match, matches_re(s, rE, 1)}
  end

  def matches(s, {:comp_regexp, rE}) do
    {:match, matches_comp(s, rE, 1)}
  end

  defp matches_re([_ | cs] = s0, rE, p0) do
    case re_apply(s0, p0, rE) do
      {:match, ^p0, s1, _Subs} ->
        [{p0, 0} | matches_re(tl(s1), rE, p0 + 1)]

      {:match, p1, s1, _Subs} ->
        [{p0, p1 - p0} | matches_re(s1, rE, p1)]

      :nomatch ->
        matches_re(cs, rE, p0 + 1)

      :never_match ->
        []
    end
  end

  defp matches_re([], _RE, _P) do
    []
  end

  defp matches_comp([_ | cs] = s0, rE, p0) do
    case comp_apply(s0, p0, rE) do
      {:match, ^p0, s1} ->
        [{p0, 0} | matches_comp(tl(s1), rE, p0 + 1)]

      {:match, p1, s1} ->
        [{p0, p1 - p0} | matches_comp(s1, rE, p1)]

      :nomatch ->
        matches_comp(cs, rE, p0 + 1)
    end
  end

  defp matches_comp([], _RE, _P) do
    []
  end

  def sub(string, regExp, rep) when is_list(regExp) do
    case parse(regExp) do
      {:ok, rE} ->
        sub(string, rE, rep)

      {:error, e} ->
        {:error, e}
    end
  end

  def sub(string, {:regexp, rE}, rep) do
    case sub_re(string, 1, rE, [], rep) do
      {:yes, newStr} ->
        {:ok, newStr, 1}

      :no ->
        {:ok, string, 0}
    end
  end

  def sub(string, {:comp_regexp, rE}, rep) do
    case sub_comp(string, 1, rE, [], rep) do
      {:yes, newStr} ->
        {:ok, newStr, 1}

      :no ->
        {:ok, string, 0}
    end
  end

  defp sub_re([c | cs] = s0, p0, rE, bef, rep) do
    case re_apply(s0, p0, rE) do
      {:match, ^p0, _S1, _} ->
        sub_re(cs, p0 + 1, rE, [c | bef], rep)

      {:match, p1, rest, _Gps} ->
        {:yes,
         reverse(
           bef,
           sub_repl(rep, substr(s0, 1, p1 - p0), rest)
         )}

      :nomatch ->
        sub_re(cs, p0 + 1, rE, [c | bef], rep)

      :never_match ->
        :no
    end
  end

  defp sub_re([], _P, _RE, _Bef, _Rep) do
    :no
  end

  defp sub_comp([c | cs] = s0, p0, rE, bef, rep) do
    case comp_apply(s0, p0, rE) do
      {:match, ^p0, _S1} ->
        sub_comp(cs, p0 + 1, rE, [c | bef], rep)

      {:match, p1, rest} ->
        {:yes,
         reverse(
           bef,
           sub_repl(rep, substr(s0, 1, p1 - p0), rest)
         )}

      :nomatch ->
        sub_comp(cs, p0 + 1, rE, [c | bef], rep)
    end
  end

  defp sub_comp([], _P, _RE, _Bef, _Rep) do
    :no
  end

  defp sub_repl([?& | rep], m, rest) do
    m ++ sub_repl(rep, m, rest)
  end

  defp sub_repl('\\&' ++ rep, m, rest) do
    [?& | sub_repl(rep, m, rest)]
  end

  defp sub_repl([c | rep], m, rest) do
    [c | sub_repl(rep, m, rest)]
  end

  defp sub_repl([], _M, rest) do
    rest
  end

  def gsub(string, regExp, rep) when is_list(regExp) do
    case parse(regExp) do
      {:ok, rE} ->
        gsub(string, rE, rep)

      {:error, e} ->
        {:error, e}
    end
  end

  def gsub(string, {:regexp, rE}, rep) do
    case gsub_re(string, 1, rE, [], rep) do
      {newStr, n} ->
        {:ok, newStr, n}

      :no ->
        {:ok, string, 0}
    end
  end

  def gsub(string, {:comp_regexp, rE}, rep) do
    case gsub_comp(string, 1, rE, [], rep) do
      {newStr, n} ->
        {:ok, newStr, n}

      :no ->
        {:ok, string, 0}
    end
  end

  defp gsub_re([c | cs] = s0, p0, rE, bef, rep) do
    case re_apply(s0, p0, rE) do
      {:match, ^p0, _S1, _} ->
        gsub_re(cs, p0 + 1, rE, [c | bef], rep)

      {:match, p1, s1, _Gps} ->
        case gsub_re(s1, p1, rE, [], rep) do
          {newStr, n0} ->
            {reverse(
               bef,
               sub_repl(rep, substr(s0, 1, p1 - p0), newStr)
             ), n0 + 1}

          :no ->
            {reverse(
               bef,
               sub_repl(rep, substr(s0, 1, p1 - p0), s1)
             ), 1}
        end

      :nomatch ->
        gsub_re(cs, p0 + 1, rE, [c | bef], rep)

      :never_match ->
        :no
    end
  end

  defp gsub_re([], _P, _RE, _Bef, _Rep) do
    :no
  end

  defp gsub_comp([c | cs] = s0, p0, rE, bef, rep) do
    case comp_apply(s0, p0, rE) do
      {:match, ^p0, _S1} ->
        gsub_comp(cs, p0 + 1, rE, [c | bef], rep)

      {:match, p1, s1} ->
        case gsub_comp(s1, p1, rE, [], rep) do
          {newStr, n0} ->
            {reverse(
               bef,
               sub_repl(rep, substr(s0, 1, p1 - p0), newStr)
             ), n0 + 1}

          :no ->
            {reverse(
               bef,
               sub_repl(rep, substr(s0, 1, p1 - p0), s1)
             ), 1}
        end

      :nomatch ->
        gsub_comp(cs, p0 + 1, rE, [c | bef], rep)
    end
  end

  defp gsub_comp([], _P, _RE, _Bef, _Rep) do
    :no
  end

  def split(string, ' ') do
    {:ok, {:regexp, rE}} = parse('[ \t]+')

    case split_apply_re(string, rE, true) do
      [[] | ss] ->
        {:ok, ss}

      ss ->
        {:ok, ss}
    end
  end

  def split(string, regExp) when is_list(regExp) do
    case parse(regExp) do
      {:ok, {:regexp, rE}} ->
        {:ok, split_apply_re(string, rE, false)}

      {:error, e} ->
        {:error, e}
    end
  end

  def split(string, {:regexp, rE}) do
    {:ok, split_apply_re(string, rE, false)}
  end

  def split(string, {:comp_regexp, rE}) do
    {:ok, split_apply_comp(string, rE, false)}
  end

  defp split_apply_re(s, rE, trim) do
    split_apply_re(s, 1, rE, trim, [])
  end

  defp split_apply_re([], _P, _RE, true, []) do
    []
  end

  defp split_apply_re([], _P, _RE, _T, sub) do
    [reverse(sub)]
  end

  defp split_apply_re([c | cs] = s, p0, rE, t, sub) do
    case re_apply(s, p0, rE) do
      {:match, ^p0, _S1, _} ->
        split_apply_re(cs, p0 + 1, rE, t, [c | sub])

      {:match, p1, s1, _} ->
        [reverse(sub) | split_apply_re(s1, p1, rE, t, [])]

      :nomatch ->
        split_apply_re(cs, p0 + 1, rE, t, [c | sub])

      :never_match ->
        [reverse(sub, s)]
    end
  end

  defp split_apply_comp(s, rE, trim) do
    split_apply_comp(s, 1, rE, trim, [])
  end

  defp split_apply_comp([], _P, _RE, _T, sub) do
    [reverse(sub)]
  end

  defp split_apply_comp([c | cs] = s, p0, rE, t, sub) do
    case comp_apply(s, p0, rE) do
      {:match, ^p0, _S1} ->
        split_apply_comp(cs, p0 + 1, rE, t, [c | sub])

      {:match, p1, s1} ->
        [reverse(sub) | split_apply_comp(s1, p1, rE, t, [])]

      :nomatch ->
        split_apply_comp(cs, p0 + 1, rE, t, [c | sub])
    end
  end

  def sub_match(s, regExp) when is_list(regExp) do
    case parse(regExp) do
      {:ok, rE} ->
        sub_match(s, rE)

      {:error, e} ->
        {:error, e}
    end
  end

  def sub_match(s, {:regexp, rE}) do
    case sub_match_re(rE, s, 1, 0, -1, :none) do
      {start, len, subs} when len >= 0 ->
        {:match, start, len, subs}

      {_Start, _Len, _Subs} ->
        :nomatch
    end
  end

  defp sub_match_re(rE, s0, pos0, mst, mlen, msubs) do
    case first_match_re(rE, s0, pos0) do
      {st, len, subs} ->
        pos1 = st + 1
        s1 = :lists.nthtail(pos1 - pos0, s0)

        cond do
          len > mlen ->
            sub_match_re(rE, s1, pos1, st, len, subs)

          true ->
            sub_match_re(rE, s1, pos1, mst, mlen, msubs)
        end

      :nomatch ->
        {mst, mlen, msubs}
    end
  end

  def sub_first_match(s, regExp) when is_list(regExp) do
    {:ok, rE} = parse(regExp)
    sub_first_match(s, rE)
  end

  def sub_first_match(s, {:regexp, rE}) do
    case first_match_re(rE, s, 1) do
      {st, len, subs} ->
        {:match, st, len, subs}

      :nomatch ->
        :nomatch
    end
  end

  defp reg(s, sc) do
    reg1(s, sc)
  end

  defp reg1(s0, sc0) do
    {l, sc1, s1} = reg2(s0, sc0)
    reg1p(s1, l, sc1)
  end

  defp reg1p([?| | s0], l, sc0) do
    {r, sc1, s1} = reg2(s0, sc0)
    reg1p(s1, {:or, l, r}, sc1)
  end

  defp reg1p(s, l, sc) do
    {l, sc, s}
  end

  defp reg2(s0, sc0) do
    {l, sc1, s1} = reg3(s0, sc0)
    reg2p(s1, l, sc1)
  end

  defp reg2p([c | s0], l, sc0) when c != ?| and c != ?) do
    {r, sc1, s1} = reg3([c | s0], sc0)

    case is_integer(r) do
      true ->
        case l do
          {:literal, lit} ->
            reg2p(s1, {:literal, lit ++ [r]}, sc1)

          {:concat, s2, char} when is_integer(char) ->
            reg2p(s1, {:concat, s2, {:literal, [char, r]}}, sc1)

          {:concat, s2, {:literal, lit}} ->
            reg2p(s1, {:concat, s2, {:literal, lit ++ [r]}}, sc1)

          char when is_integer(char) ->
            reg2p(s1, {:literal, [char, r]}, sc1)

          _ ->
            reg2p(s1, {:concat, l, r}, sc1)
        end

      false ->
        reg2p(s1, {:concat, l, r}, sc1)
    end
  end

  defp reg2p(s, l, sc) do
    {l, sc, s}
  end

  defp reg3(s0, sc0) do
    {l, sc1, s1} = reg4(s0, sc0)
    reg3p(s1, l, sc1)
  end

  defp reg3p([?* | s], l, sc) do
    reg3p(s, {:kclosure, l}, sc)
  end

  defp reg3p([?+ | s], l, sc) do
    reg3p(s, {:pclosure, l}, sc)
  end

  defp reg3p([?? | s], l, sc) do
    reg3p(s, {:optional, l}, sc)
  end

  defp reg3p([?{ | cs0], l, sc) do
    case interval_range(cs0) do
      {:none, :none, _Cs1} ->
        parse_error({:interval_range, [?{ | cs0]})

      {n, m, [?} | cs1]} ->
        reg3p(cs1, {:iclosure, l, n, m}, sc)

      {_N, _M, _Cs1} ->
        parse_error({:unterminated, '{'})
    end
  end

  defp reg3p(s, l, sc) do
    {l, sc, s}
  end

  defp reg4([?( | s0], sc0) do
    sc1 = sc0 + 1

    case reg(s0, sc1) do
      {r, sc2, [?) | s1]} ->
        {{:subexpr, sc1, r}, sc2, s1}

      {_R, _Sc, _S} ->
        parse_error({:unterminated, '('})
    end
  end

  defp reg4([?^ | s], sc) do
    {:bos, sc, s}
  end

  defp reg4([?$ | s], sc) do
    {:eos, sc, s}
  end

  defp reg4([?. | s], sc) do
    {{:comp_class, '\n'}, sc, s}
  end

  defp reg4('[^' ++ s0, sc) do
    case char_class(s0) do
      {cc, [?] | s1]} ->
        {{:comp_class, cc}, sc, s1}

      {_Cc, _S} ->
        parse_error({:unterminated, '['})
    end
  end

  defp reg4([?[ | s0], sc) do
    case char_class(s0) do
      {cc, [?] | s1]} ->
        {{:char_class, cc}, sc, s1}

      {_Cc, _S1} ->
        parse_error({:unterminated, '['})
    end
  end

  defp reg4([c0 | s0], sc)
       when is_integer(c0) and
              c0 != ?* and c0 != ?+ and c0 != ?? and
              c0 != ?] and c0 != ?) and c0 != ?} do
    {c1, s1} = char(c0, s0)
    {c1, sc, s1}
  end

  defp reg4(s = [?) | _], sc) do
    {:epsilon, sc, s}
  end

  defp reg4([c | _S], _Sc) do
    parse_error({:illegal, [c]})
  end

  defp reg4([], sc) do
    {:epsilon, sc, []}
  end

  defp char(?\\, [o1, o2, o3 | s])
       when o1 >= ?0 and
              o1 <= ?7 and o2 >= ?0 and o2 <= ?7 and
              o3 >= ?0 and o3 <= ?7 do
    {(o1 * 8 + o2) * 8 + o3 - 73 * ?0, s}
  end

  defp char(?\\, [c | s]) do
    {escape_char(c), s}
  end

  defp char(?\\, []) do
    parse_error({:unterminated, '\\'})
  end

  defp char(c, s) do
    {c, s}
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

  defp char_class([?] | s0]) do
    {cc, s1} = char_class(s0, [?]])
    {pack_cc(cc), s1}
  end

  defp char_class(s0) do
    {cc, s1} = char_class(s0, [])
    {pack_cc(cc), s1}
  end

  defp pack_cc(cc0) do
    cc1 =
      :lists.usort(
        fn
          {cf1, _}, {cf2, _} ->
            cf1 < cf2

          {cf1, _}, c ->
            cf1 < c

          c, {cf, _} ->
            c < cf

          c1, c2 ->
            c1 <= c2
        end,
        cc0
      )

    pack_cc1(cc1)
  end

  defp pack_cc1([{cf1, cl1}, {cf2, cl2} | cc])
       when cl1 >= cf2 and cl1 <= cl2 do
    pack_cc1([{cf1, cl2} | cc])
  end

  defp pack_cc1([{cf1, cl1}, {cf2, cl2} | cc])
       when cl1 >= cf2 and cl1 >= cl2 do
    pack_cc1([{cf1, cl1} | cc])
  end

  defp pack_cc1([{cf1, cl1}, {cf2, cl2} | cc])
       when cl1 + 1 == cf2 do
    pack_cc1([{cf1, cl2} | cc])
  end

  defp pack_cc1([{cf, cl}, c | cc]) when cl >= c do
    pack_cc1([{cf, cl} | cc])
  end

  defp pack_cc1([{cf, cl}, c | cc]) when cl + 1 == c do
    pack_cc1([{cf, c} | cc])
  end

  defp pack_cc1([c, {cf, cl} | cc]) when c == cf - 1 do
    pack_cc1([{c, cl} | cc])
  end

  defp pack_cc1([c1, c2 | cc]) when c1 + 1 == c2 do
    pack_cc1([{c1, c2} | cc])
  end

  defp pack_cc1([c | cc]) do
    [c | pack_cc1(cc)]
  end

  defp pack_cc1([]) do
    []
  end

  defp char_class('[:' ++ s0, cc0) do
    case posix_cc(s0, cc0) do
      {cc1, ':]' ++ s1} ->
        char_class(s1, cc1)

      {_, _S1} ->
        parse_error({:posix_cc, '[:' ++ s0})
    end
  end

  defp char_class([c1 | s0], cc) when c1 != ?] do
    case char(c1, s0) do
      {cf, [?-, c2 | s1]} when c2 != ?] ->
        case char(c2, s1) do
          {cl, s2} when cf < cl ->
            char_class(s2, [{cf, cl} | cc])

          {_Cl, _S2} ->
            parse_error({:char_class, [c1 | s0]})
        end

      {c, s1} ->
        char_class(s1, [c | cc])
    end
  end

  defp char_class(s, cc) do
    {cc, s}
  end

  defp posix_cc('alnum' ++ s, cc) do
    {[
       {?0, ?9},
       {?A, ?Z},
       {192, 214},
       {216, 223},
       {?a, ?z},
       {224, 246},
       {248, 255}
       | cc
     ], s}
  end

  defp posix_cc('alpha' ++ s, cc) do
    {[{?A, ?Z}, {192, 214}, {216, 223}, {?a, ?z}, {224, 246}, {248, 255} | cc], s}
  end

  defp posix_cc('blank' ++ s, cc) do
    {[?\s, ?\t, 160 | cc], s}
  end

  defp posix_cc('cntrl' ++ s, cc) do
    {[{0, 31}, {127, 159} | cc], s}
  end

  defp posix_cc('digit' ++ s, cc) do
    {[{?0, ?9} | cc], s}
  end

  defp posix_cc('graph' ++ s, cc) do
    {[{33, 126}, {161, 255} | cc], s}
  end

  defp posix_cc('lower' ++ s, cc) do
    {[{?a, ?z}, {224, 246}, {248, 255} | cc], s}
  end

  defp posix_cc('print' ++ s, cc) do
    {[{32, 126}, {160, 255} | cc], s}
  end

  defp posix_cc('punct' ++ s, cc) do
    {[{?!, ?/}, {?:, ??}, {?{, ?~}, {161, 191} | cc], s}
  end

  defp posix_cc('space' ++ s, cc) do
    {[?\s, ?\t, ?\f, ?\r, ?\v, 160 | cc], s}
  end

  defp posix_cc('upper' ++ s, cc) do
    {[{?A, ?Z}, {192, 214}, {216, 223} | cc], s}
  end

  defp posix_cc('xdigit' ++ s, cc) do
    {[{?a, ?f}, {?A, ?F}, {?0, ?9} | cc], s}
  end

  defp posix_cc(s, _Cc) do
    parse_error({:posix_cc, '[:' ++ s})
  end

  defp interval_range(cs0) do
    case number(cs0) do
      {:none, cs1} ->
        {:none, :none, cs1}

      {n, [?, | cs1]} ->
        case number(cs1) do
          {:none, cs2} ->
            {n, :any, cs2}

          {m, cs2} ->
            {n, m, cs2}
        end

      {n, cs1} ->
        {n, :none, cs1}
    end
  end

  defp number([c | cs]) when c >= ?0 and c <= ?9 do
    number(cs, c - ?0)
  end

  defp number(cs) do
    {:none, cs}
  end

  defp number([c | cs], acc) when c >= ?0 and c <= ?9 do
    number(cs, 10 * acc + (c - ?0))
  end

  defp number(cs, acc) do
    {acc, cs}
  end

  defp parse_error(e) do
    throw({:error, e})
  end

  defp re_apply(s, st, {rE, sc}) do
    subs = :erlang.make_tuple(sc, :none)
    res = re_apply(rE, [], s, st, subs)
    res
  end

  defp re_apply(:epsilon, more, s, p, subs) do
    re_apply_more(more, s, p, subs)
  end

  defp re_apply({:or, rE1, rE2}, more, s, p, subs) do
    re_apply_or(
      re_apply(rE1, more, s, p, subs),
      re_apply(rE2, more, s, p, subs)
    )
  end

  defp re_apply({:concat, rE1, rE2}, more, s0, p, subs) do
    re_apply(rE1, [rE2 | more], s0, p, subs)
  end

  defp re_apply({:literal, [c | lcs]}, more, [c | s], p, subs) do
    re_apply_lit(lcs, more, s, p + 1, subs)
  end

  defp re_apply({:kclosure, rE}, more, s0, p0, subs0) do
    loop =
      case re_apply(rE, [], s0, p0, subs0) do
        {:match, ^p0, _S1, _Subs1} ->
          :nomatch

        {:match, p1, s1, subs1} ->
          re_apply_more([{:kclosure, rE} | more], s1, p1, subs1)

        :nomatch ->
          :nomatch

        :never_match ->
          :never_match
      end

    re_apply_or(loop, re_apply_more(more, s0, p0, subs0))
  end

  defp re_apply({:pclosure, rE}, more, s, p, subs) do
    re_apply(rE, [{:kclosure, rE} | more], s, p, subs)
  end

  defp re_apply({:optional, rE}, more, s, p, subs) do
    re_apply_or(
      re_apply(rE, more, s, p, subs),
      re_apply_more(more, s, p, subs)
    )
  end

  defp re_apply({:iclosure, rE, n, m}, more, s, p, subs)
       when n > 0 do
    re_apply(rE, [{:iclosure, rE, n - 1, m} | more], s, p, subs)
  end

  defp re_apply({:iclosure, rE, 0, m}, more, s, p, subs) do
    exp = expand_opt(rE, m)
    re_apply(exp, more, s, p, subs)
  end

  defp re_apply({:subexpr, n, rE}, more, s, p, subs) do
    re_apply(rE, [{:endsub, n, p} | more], s, p, subs)
  end

  defp re_apply({:endsub, n, st}, more, s, p, subs0) do
    subs1 = :erlang.setelement(n, subs0, {st, p - st})
    re_apply_more(more, s, p, subs1)
  end

  defp re_apply(:bos, more, s, 1, subs) do
    re_apply_more(more, s, 1, subs)
  end

  defp re_apply(:bos, _More, _S, _, _) do
    :never_match
  end

  defp re_apply(:eos, more, [?\n], p, subs) do
    re_apply_more(more, [], p, subs)
  end

  defp re_apply(:eos, more, [], p, subs) do
    re_apply_more(more, [], p, subs)
  end

  defp re_apply({:char_class, cc}, more, [c | s], p, subs) do
    case in_char_class(c, cc) do
      true ->
        re_apply_more(more, s, p + 1, subs)

      false ->
        :nomatch
    end
  end

  defp re_apply({:comp_class, cc}, more, [c | s], p, subs) do
    case in_char_class(c, cc) do
      true ->
        :nomatch

      false ->
        re_apply_more(more, s, p + 1, subs)
    end
  end

  defp re_apply(c, more, [c | s], p, subs) when is_integer(c) do
    re_apply_more(more, s, p + 1, subs)
  end

  defp re_apply(_RE, _More, _S, _P, _Subs) do
    :nomatch
  end

  defp re_apply_more([rE | more], s, p, subs) do
    re_apply(rE, more, s, p, subs)
  end

  defp re_apply_more([], s, p, subs) do
    {:match, p, s, subs}
  end

  defp re_apply_lit([c | lit], more, [c | cs], p, subs) do
    re_apply_lit(lit, more, cs, p + 1, subs)
  end

  defp re_apply_lit([], more, cs, p, subs) do
    re_apply_more(more, cs, p, subs)
  end

  defp re_apply_lit(_Lit, _More, _Cs, _P, _Subs) do
    :nomatch
  end

  defp expand_iclosure(rE, 0, m) do
    expand_opt(rE, m)
  end

  defp expand_iclosure(rE, n, m) do
    {:concat, rE, expand_iclosure(rE, n - 1, m)}
  end

  defp expand_opt(_RE, :none) do
    :epsilon
  end

  defp expand_opt(rE, :any) do
    {:kclosure, rE}
  end

  defp expand_opt(_RE, 0) do
    :epsilon
  end

  defp expand_opt(rE, 1) do
    {:optional, rE}
  end

  defp expand_opt(rE, n) do
    {:optional, {:concat, rE, expand_opt(rE, n - 1)}}
  end

  defp in_char_class(c, [{c1, c2} | _Cc])
       when c >= c1 and
              c <= c2 do
    true
  end

  defp in_char_class(c, [c | _Cc]) do
    true
  end

  defp in_char_class(c, [_ | cc]) do
    in_char_class(c, cc)
  end

  defp in_char_class(_C, []) do
    false
  end

  defp re_apply_or(m1 = {:match, p1, _, _}, {:match, p2, _, _})
       when p1 >= p2 do
    m1
  end

  defp re_apply_or({:match, _, _, _}, m2 = {:match, _, _, _}) do
    m2
  end

  defp re_apply_or(:never_match, r2) do
    r2
  end

  defp re_apply_or(r1, :never_match) do
    r1
  end

  defp re_apply_or(:nomatch, r2) do
    r2
  end

  defp re_apply_or(r1, :nomatch) do
    r1
  end

  require Record
  Record.defrecord(:r_nfa_state, :nfa_state, no: :undefined, edges: [], accept: :no)
  Record.defrecord(:r_dfa_state, :dfa_state, no: :undefined, nfa: [], trans: [], accept: :no)

  Record.defrecord(:r_c_state, :c_state,
    no: :undefined,
    trans: [],
    tmin: 0,
    smin: :none,
    tmax: 0,
    smax: :none,
    accept: false,
    spec: []
  )

  def make_nfa(rEAs0) do
    case parse_reas(rEAs0) do
      {:ok, rEAs1} ->
        {nFA, start} = build_combined_nfa(rEAs1)
        {:ok, {nFA, start}}

      {:error, e} ->
        {:error, e}
    end
  end

  def make_dfa(rEAs) do
    make_dfa(rEAs, 0)
  end

  def make_dfa(rEAs0, low) do
    case parse_reas(rEAs0) do
      {:ok, rEAs1} ->
        {nFA, start0} = build_combined_nfa(rEAs1)
        {dFA0, start1} = build_dfa(nFA, start0)
        {dFA, start} = minimise_dfa(dFA0, start1, low)
        {:ok, {dFA, start}}

      {:error, e} ->
        {:error, e}
    end
  end

  defp parse_reas(rEAs) do
    parse_reas(rEAs, [])
  end

  defp parse_reas([{{:regexp, {r, _Sc}}, a} | rEAs], s) do
    parse_reas(rEAs, [{r, a} | s])
  end

  defp parse_reas([{regExp, a} | rEAs], s) do
    case parse(regExp) do
      {:ok, {:regexp, {r, _Sc}}} ->
        parse_reas(rEAs, [{r, a} | s])

      {:error, e} ->
        {:error, e}
    end
  end

  defp parse_reas([], stack) do
    {:ok, reverse(stack)}
  end

  defp build_combined_nfa(rEAs) do
    {nFA, starts, next} = build_nfa_list(rEAs, [], [], 1)
    f = r_nfa_state(no: next, edges: epsilon_trans(starts), accept: :no)
    {[f | nFA], next}
  end

  defp build_nfa_list([{rE, action} | rEAs], nFA0, starts, next0) do
    {nFA1, next1, start} = build_nfa(rE, next0, action)
    build_nfa_list(rEAs, nFA1 ++ nFA0, [start | starts], next1)
  end

  defp build_nfa_list([], nFA, starts, next) do
    {nFA, reverse(starts), next}
  end

  defp epsilon_trans(sts) do
    for s <- sts do
      {:epsilon, s}
    end
  end

  defp build_nfa(rE, next, action) do
    {nFA, n, e} = build_nfa(rE, next + 1, next, [])
    {[r_nfa_state(no: e, accept: {:yes, action}) | nFA], n, next}
  end

  defp build_nfa({:or, rE1, rE2}, n0, s, nFA0) do
    {nFA1, n1, e1} = build_nfa(rE1, n0 + 1, n0, nFA0)
    {nFA2, n2, e2} = build_nfa(rE2, n1 + 1, n1, nFA1)
    e = n2

    {[
       r_nfa_state(no: s, edges: [{:epsilon, n0}, {:epsilon, n1}]),
       r_nfa_state(no: e1, edges: [{:epsilon, e}]),
       r_nfa_state(
         no: e2,
         edges: [{:epsilon, e}]
       )
       | nFA2
     ], n2 + 1, e}
  end

  defp build_nfa({:literal, []}, n, s, nFA) do
    {nFA, n, s}
  end

  defp build_nfa({:literal, [c | cs]}, n0, s, nFA0) do
    {nFA1, n1, e1} = build_nfa(c, n0, s, nFA0)
    build_nfa({:literal, cs}, n1, e1, nFA1)
  end

  defp build_nfa({:concat, rE1, rE2}, n0, s, nFA0) do
    {nFA1, n1, e1} = build_nfa(rE1, n0, s, nFA0)
    {nFA2, n2, e2} = build_nfa(rE2, n1, e1, nFA1)
    {nFA2, n2, e2}
  end

  defp build_nfa({:kclosure, rE}, n0, s, nFA0) do
    {nFA1, n1, e1} = build_nfa(rE, n0 + 1, n0, nFA0)
    e = n1

    {[
       r_nfa_state(no: s, edges: [{:epsilon, n0}, {:epsilon, e}]),
       r_nfa_state(no: e1, edges: [{:epsilon, n0}, {:epsilon, e}])
       | nFA1
     ], n1 + 1, e}
  end

  defp build_nfa({:pclosure, rE}, n0, s, nFA0) do
    {nFA1, n1, e1} = build_nfa(rE, n0 + 1, n0, nFA0)
    e = n1

    {[
       r_nfa_state(no: s, edges: [{:epsilon, n0}]),
       r_nfa_state(
         no: e1,
         edges: [{:epsilon, n0}, {:epsilon, e}]
       )
       | nFA1
     ], n1 + 1, e}
  end

  defp build_nfa({:optional, rE}, n0, s, nFA0) do
    {nFA1, n1, e1} = build_nfa(rE, n0 + 1, n0, nFA0)
    e = n1

    {[
       r_nfa_state(no: s, edges: [{:epsilon, n0}, {:epsilon, e}]),
       r_nfa_state(no: e1, edges: [{:epsilon, e}]) | nFA1
     ], n1 + 1, e}
  end

  defp build_nfa({:iclosure, rE, i1, i2}, n, s, nFA) do
    exp = expand_iclosure(rE, i1, i2)
    build_nfa(exp, n, s, nFA)
  end

  defp build_nfa({:char_class, cc}, n, s, nFA) do
    {[r_nfa_state(no: s, edges: [{nfa_char_class(cc), n}]) | nFA], n + 1, n}
  end

  defp build_nfa({:comp_class, cc}, n, s, nFA) do
    {[r_nfa_state(no: s, edges: [{nfa_comp_class(cc), n}]) | nFA], n + 1, n}
  end

  defp build_nfa(:epsilon, n, s, nFA) do
    {nFA, n, s}
  end

  defp build_nfa({:group, rE}, n, s, nFA) do
    build_nfa(rE, n, s, nFA)
  end

  defp build_nfa({:subexpr, _N, rE}, n, s, nFA) do
    build_nfa(rE, n, s, nFA)
  end

  defp build_nfa(:bos, n, s, nFA) do
    {[r_nfa_state(no: s, edges: [{[:bos], n}]) | nFA], n + 1, n}
  end

  defp build_nfa(:eos, n, s, nFA) do
    {[r_nfa_state(no: s, edges: [{[:eos], n}]) | nFA], n + 1, n}
  end

  defp build_nfa(c, n, s, nFA) when is_integer(c) do
    {[r_nfa_state(no: s, edges: [{[{c, c}], n}]) | nFA], n + 1, n}
  end

  defp nfa_char_class(cc) do
    crs =
      :lists.foldl(
        fn
          {c1, c2}, set ->
            add_element({c1, c2}, set)

          c, set ->
            add_element({c, c}, set)
        end,
        [],
        cc
      )

    pack_crs(crs)
  end

  defp pack_crs([{c1, c2} = cr, {c3, c4} | crs])
       when c1 <= c3 and c2 >= c4 do
    pack_crs([cr | crs])
  end

  defp pack_crs([{c1, c2}, {c3, c4} | crs])
       when c2 >= c3 and
              c2 < c4 do
    pack_crs([{c1, c4} | crs])
  end

  defp pack_crs([{c1, c2}, {c3, c4} | crs]) when c2 + 1 == c3 do
    pack_crs([{c1, c4} | crs])
  end

  defp pack_crs([cr | crs]) do
    [cr | pack_crs(crs)]
  end

  defp pack_crs([]) do
    []
  end

  defp nfa_comp_class(cc) do
    crs = nfa_char_class(cc)
    comp_crs(crs, 0)
  end

  defp comp_crs([{c1, c2} | crs], last) do
    [{last, c1 - 1} | comp_crs(crs, c2 + 1)]
  end

  defp comp_crs([], last) do
    [{last, :maxchar}]
  end

  defp build_dfa(nFA0, start) do
    nFA1 = :erlang.list_to_tuple(keysort(r_nfa_state(:no), nFA0))
    d = r_dfa_state(no: 0, nfa: eclosure([start], nFA1), accept: :no)
    {build_dfa([d], 1, [], nFA1), 0}
  end

  defp build_dfa([u | us0], n0, ms, nFA) do
    {ts, us1, n1} = build_dfa(r_dfa_state(u, :nfa), us0, n0, [], [u | ms], nFA)
    m = r_dfa_state(u, trans: ts, accept: accept(r_dfa_state(u, :nfa), nFA))
    build_dfa(us1, n1, [m | ms], nFA)
  end

  defp build_dfa([], _N, ms, _NFA) do
    ms
  end

  defp build_dfa(set, us, n, ts, ms, nFA) do
    crs0 =
      for s <- set,
          {crs, _St} <- r_nfa_state(:erlang.element(s, nFA), :edges),
          is_list(crs),
          cr <- crs do
        cr
      end

    crs1 = :lists.usort(crs0)
    test = disjoint_crs(crs1)
    build_dfa(test, set, us, n, ts, ms, nFA)
  end

  defp disjoint_crs([{_C1, c2} = cr1, {c3, _C4} = cr2 | crs])
       when c2 < c3 do
    [cr1 | disjoint_crs([cr2 | crs])]
  end

  defp disjoint_crs([{c1, c2}, {c3, c4} | crs]) when c1 == c3 do
    [
      {c1, c2}
      | disjoint_crs(
          add_element(
            {c2 + 1, c4},
            crs
          )
        )
    ]
  end

  defp disjoint_crs([{c1, c2}, {c3, c4} | crs])
       when c1 < c3 and
              c2 >= c3 and c2 < c4 do
    [
      {c1, c3 - 1}
      | disjoint_crs(
          union(
            [{c3, c2}, {c2 + 1, c4}],
            crs
          )
        )
    ]
  end

  defp disjoint_crs([{c1, c2}, {c3, c4} | crs])
       when c1 < c3 and
              c2 == c4 do
    [
      {c1, c3 - 1}
      | disjoint_crs(
          add_element(
            {c3, c4},
            crs
          )
        )
    ]
  end

  defp disjoint_crs([{c1, c2}, {c3, c4} | crs])
       when c1 < c3 and
              c2 > c4 do
    [
      {c1, c3 - 1}
      | disjoint_crs(
          union(
            [{c3, c4}, {c4 + 1, c2}],
            crs
          )
        )
    ]
  end

  defp disjoint_crs([cr | crs]) do
    [cr | disjoint_crs(crs)]
  end

  defp disjoint_crs([]) do
    []
  end

  defp build_dfa([cr | crs], set, us, n, ts, ms, nFA) do
    case eclosure(move(set, cr, nFA), nFA) do
      s when s != [] ->
        case keysearch(s, r_dfa_state(:nfa), us) do
          {:value, r_dfa_state(no: t)} ->
            build_dfa(crs, set, us, n, [{cr, t} | ts], ms, nFA)

          false ->
            case keysearch(s, r_dfa_state(:nfa), ms) do
              {:value, r_dfa_state(no: t)} ->
                build_dfa(crs, set, us, n, [{cr, t} | ts], ms, nFA)

              false ->
                u = r_dfa_state(no: n, nfa: s)
                build_dfa(crs, set, [u | us], n + 1, [{cr, n} | ts], ms, nFA)
            end
        end

      [] ->
        build_dfa(crs, set, us, n, ts, ms, nFA)
    end
  end

  defp build_dfa([], _Set, us, n, ts, _Ms, _NFA) do
    {ts, us, n}
  end

  defp eclosure(sts, nFA) do
    eclosure(sts, nFA, [])
  end

  defp eclosure([st | sts], nFA, ec) do
    r_nfa_state(edges: es) = :erlang.element(st, nFA)

    eclosure(
      for {:epsilon, n} <- es,
          not is_element(n, ec) do
        n
      end ++ sts,
      nFA,
      add_element(st, ec)
    )
  end

  defp eclosure([], _NFA, ec) do
    ec
  end

  defp move(sts, cr, nFA) do
    for n <- sts,
        {crs, st} <- r_nfa_state(:erlang.element(n, nFA), :edges),
        is_list(crs),
        in_crs(cr, crs) do
      st
    end
  end

  defp in_crs({c1, c2}, [{c3, c4} | _Crs])
       when c1 >= c3 and
              c2 <= c4 do
    true
  end

  defp in_crs(cr, [cr | _Crs]) do
    true
  end

  defp in_crs(cr, [_ | crs]) do
    in_crs(cr, crs)
  end

  defp in_crs(_Cr, []) do
    false
  end

  defp accept([st | sts], nFA) do
    case :erlang.element(st, nFA) do
      r_nfa_state(accept: {:yes, a}) ->
        {:yes, a}

      r_nfa_state(accept: :no) ->
        accept(sts, nFA)
    end
  end

  defp accept([], _NFA) do
    :no
  end

  defp minimise_dfa(dFA0, start, n) do
    case min_dfa(dFA0) do
      {dFA1, []} ->
        {dFA2, rs} = pack_dfa(dFA1, n)
        {min_update(dFA2, rs), min_new_state(start, rs)}

      {dFA1, rs} ->
        minimise_dfa(min_update(dFA1, rs), min_new_state(start, rs), n)
    end
  end

  defp min_dfa(dFA) do
    min_dfa(dFA, [], [])
  end

  defp min_dfa([d | dFA0], rs0, mDFA) do
    {dFA1, rs1} =
      min_delete(
        dFA0,
        r_dfa_state(d, :trans),
        r_dfa_state(d, :accept),
        r_dfa_state(d, :no),
        rs0,
        []
      )

    min_dfa(dFA1, rs1, [d | mDFA])
  end

  defp min_dfa([], rs, mDFA) do
    {mDFA, rs}
  end

  defp min_delete([r_dfa_state(no: n, trans: t, accept: a) | dFA], t, a, newN, rs, mDFA) do
    min_delete(dFA, t, a, newN, [{n, newN} | rs], mDFA)
  end

  defp min_delete([d | dFA], t, a, newN, rs, mDFA) do
    min_delete(dFA, t, a, newN, rs, [d | mDFA])
  end

  defp min_delete([], _T, _A, _NewN, rs, mDFA) do
    {mDFA, rs}
  end

  defp min_update(dFA, rs) do
    for d <- dFA do
      r_dfa_state(d, trans: min_update_trans(r_dfa_state(d, :trans), rs))
    end
  end

  defp min_update_trans(tr, rs) do
    for {c, s} <- tr do
      {c, min_new_state(s, rs)}
    end
  end

  defp min_new_state(old, [{old, new} | _Reds]) do
    new
  end

  defp min_new_state(old, [_R | reds]) do
    min_new_state(old, reds)
  end

  defp min_new_state(old, []) do
    old
  end

  defp pack_dfa(dFA, n) do
    pack_dfa(dFA, n, [], [])
  end

  defp pack_dfa([d | dFA], newN, rs, pDFA) do
    pack_dfa(dFA, newN + 1, [{r_dfa_state(d, :no), newN} | rs], [r_dfa_state(d, no: newN) | pDFA])
  end

  defp pack_dfa([], _NewN, rs, pDFA) do
    {pDFA, rs}
  end

  defp comp_apply(cs, p, {dFA, start, _Fail}) do
    comp_apply(:erlang.element(start, dFA), cs, p, dFA, :nomatch)
  end

  defp comp_apply(r_c_state(spec: []) = st, cs, p, dFA, accept) do
    comp_apply_tr(st, cs, p, dFA, accept)
  end

  defp comp_apply(r_c_state(spec: sp) = st, cs, p, dFA, accept) do
    comp_apply_sp(st, cs, p, dFA, accept, sp)
  end

  defp comp_apply_tr(r_c_state(trans: :none, accept: a), cs, p, _DFA, accept) do
    accept_value(a, cs, p, accept)
  end

  defp comp_apply_tr(
         r_c_state(trans: tr, tmin: tmin, smin: smin, tmax: tmax, smax: smax, accept: a),
         [c | cs] = cs0,
         p,
         dFA,
         accept
       ) do
    nextSt =
      cond do
        c <= tmin ->
          smin

        c >= tmax ->
          smax

        true ->
          :erlang.element(c - tmin, tr)
      end

    comp_apply(:erlang.element(nextSt, dFA), cs, p + 1, dFA, accept_value(a, cs0, p, accept))
  end

  defp comp_apply_tr(r_c_state(trans: _Tr, accept: a), [], p, _DFA, accept) do
    accept_value(a, [], p, accept)
  end

  defp comp_apply_sp(_St, cs, 1, dFA, accept, [{:bos, s} | _]) do
    comp_apply(:erlang.element(s, dFA), cs, 1, dFA, accept)
  end

  defp comp_apply_sp(_St, [?\n], p, dFA, accept, [{:eos, s} | _]) do
    comp_apply(:erlang.element(s, dFA), [], p, dFA, accept)
  end

  defp comp_apply_sp(_St, [], p, dFA, accept, [{:eos, s} | _]) do
    comp_apply(:erlang.element(s, dFA), [], p, dFA, accept)
  end

  defp comp_apply_sp(st, cs, p, dFA, accept, [_ | sp]) do
    comp_apply_sp(st, cs, p, dFA, accept, sp)
  end

  defp comp_apply_sp(st, cs, p, dFA, accept, []) do
    comp_apply_tr(st, cs, p, dFA, accept)
  end

  defp accept_value(true, cs, p, _Accept) do
    {:match, p, cs}
  end

  defp accept_value(false, _Cs, _P, accept) do
    accept
  end

  def compile(regExp) do
    case make_dfa([{regExp, :yes}], 2) do
      {:ok, {dFA0, start}} ->
        fail = 1
        dFA1 = [r_dfa_state(no: fail, accept: :no, trans: []) | dFA0]
        dFA = tuplelise_dfa(dFA1, 1)
        {:ok, {:comp_regexp, {dFA, start, fail}}}

      {:error, e} ->
        {:error, e}
    end
  end

  defp tuplelise_dfa(dFA0, noAccept) do
    dFA1 =
      map(
        fn r_dfa_state(no: n, trans: ts, accept: a) ->
          {tr, tmin, smin, tmax, smax, sp} =
            build_trans(
              ts,
              noAccept
            )

          r_c_state(
            no: n,
            trans: tr,
            tmin: tmin,
            smin: smin,
            tmax: tmax,
            smax: smax,
            accept: fix_accept(a),
            spec: sp
          )
        end,
        dFA0
      )

    :erlang.list_to_tuple(keysort(r_dfa_state(:no), dFA1))
  end

  defp build_trans(ts0, noAccept) do
    {ts1, sp1} =
      foldl(
        fn
          {{_, _}, _} = t, {ts, sp} ->
            {[t | ts], sp}

          {_, _} = t, {ts, sp} ->
            {ts, [t | sp]}
        end,
        {[], []},
        ts0
      )

    cond do
      ts1 == [] ->
        {:none, :none, :none, :none, :none, sp1}

      true ->
        ts2 = keysort(1, ts1)
        {tmin, smin, ts3} = min_trans(ts2, noAccept)
        {trans, tmax, smax} = expand_trans(ts3, tmin, noAccept)
        {:erlang.list_to_tuple(trans), tmin, smin, tmax, smax, sp1}
    end
  end

  defp min_trans([{{0, c2}, s} | crs], _Def) do
    {c2, s, crs}
  end

  defp min_trans([{{c1, _C2}, _S} | _] = crs, def__) do
    {c1 - 1, def__, crs}
  end

  defp expand_trans([{{c1, :maxchar}, s}], last, def__) do
    trs = duplicate(c1 - (last + 1), def__)
    {trs, c1, s}
  end

  defp expand_trans([{{c1, c2}, s}], last, def__) do
    trs =
      duplicate(
        c1 - (last + 1),
        def__
      ) ++ duplicate(c2 - c1 + 1, s)

    {trs, c2 + 1, def__}
  end

  defp expand_trans([{{c1, c2}, s} | crs], last, def__) do
    {trs0, tmax, smax} = expand_trans(crs, c2, def__)

    trs1 =
      duplicate(
        c1 - (last + 1),
        def__
      ) ++ duplicate(c2 - c1 + 1, s) ++ trs0

    {trs1, tmax, smax}
  end

  defp fix_accept({:yes, _}) do
    true
  end

  defp fix_accept(:no) do
    false
  end
end
