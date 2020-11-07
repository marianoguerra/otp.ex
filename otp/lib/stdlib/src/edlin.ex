defmodule :m_edlin do
  use Bitwise
  import :lists, only: [reverse: 1, reverse: 2]

  def init() do
    :erlang.put(:kill_buffer, [])
  end

  def init(pid) do
    copiedKillBuf =
      case :erlang.process_info(
             pid,
             :dictionary
           ) do
        {:dictionary, dict} ->
          case :proplists.get_value(:kill_buffer, dict) do
            :undefined ->
              []

            buf ->
              buf
          end

        :undefined ->
          []
      end

    :erlang.put(:kill_buffer, copiedKillBuf)
  end

  def start(pbs) do
    start(pbs, :none)
  end

  def start(pbs, mode) do
    {:more_chars, {:line, pbs, {[], []}, mode}, [{:put_chars, :unicode, pbs}]}
  end

  def edit_line(cs, {:line, p, l, {:blink, n}}) do
    edit(cs, p, l, :none, [{:move_rel, n}])
  end

  def edit_line(cs, {:line, p, l, m}) do
    edit(cs, p, l, m, [])
  end

  def edit_line1(cs, {:line, p, l, {:blink, n}}) do
    edit(cs, p, l, :none, [{:move_rel, n}])
  end

  def edit_line1(cs, {:line, p, {[], []}, :none}) do
    {:more_chars, {:line, p, {:string.reverse(cs), []}, :none}, [{:put_chars, :unicode, cs}]}
  end

  def edit_line1(cs, {:line, p, l, m}) do
    edit(cs, p, l, m, [])
  end

  defp edit([c | cs], p, line, {:blink, _}, [_ | rs]) do
    edit([c | cs], p, line, :none, rs)
  end

  defp edit([c | cs], p, {bef, aft}, prefix, rs0) do
    case key_map(c, prefix) do
      :meta ->
        edit(cs, p, {bef, aft}, :meta, rs0)

      :meta_o ->
        edit(cs, p, {bef, aft}, :meta_o, rs0)

      :meta_csi ->
        edit(cs, p, {bef, aft}, :meta_csi, rs0)

      :meta_meta ->
        edit(cs, p, {bef, aft}, :meta_meta, rs0)

      {:csi, _} = csi ->
        edit(cs, p, {bef, aft}, csi, rs0)

      :meta_left_sq_bracket ->
        edit(cs, p, {bef, aft}, :meta_left_sq_bracket, rs0)

      :search_meta ->
        edit(cs, p, {bef, aft}, :search_meta, rs0)

      :search_meta_left_sq_bracket ->
        edit(cs, p, {bef, aft}, :search_meta_left_sq_bracket, rs0)

      :ctlx ->
        edit(cs, p, {bef, aft}, :ctlx, rs0)

      :new_line ->
        {:done, get_line(bef, aft ++ '\n'), cs,
         reverse(
           rs0,
           [{:move_rel, cp_len(aft)}, {:put_chars, :unicode, '\n'}]
         )}

      :redraw_line ->
        rs1 = erase(p, bef, aft, rs0)
        rs = redraw(p, bef, aft, rs1)
        edit(cs, p, {bef, aft}, :none, rs)

      :tab_expand ->
        {:expand, bef, cs, {:line, p, {bef, aft}, :none}, reverse(rs0)}

      {:undefined, ^c} ->
        {:undefined, {:none, prefix, c}, cs, {:line, p, {bef, aft}, :none}, reverse(rs0)}

      op ->
        case do_op(op, bef, aft, rs0) do
          {:blink, n, line, rs} ->
            edit(cs, p, line, {:blink, n}, rs)

          {line, rs, mode} ->
            edit(cs, p, line, mode, rs)

          {line, rs} ->
            edit(cs, p, line, :none, rs)
        end
    end
  end

  defp edit([], p, l, {:blink, n}, rs) do
    {:blink, {:line, p, l, {:blink, n}}, reverse(rs)}
  end

  defp edit([], p, l, prefix, rs) do
    {:more_chars, {:line, p, l, prefix}, reverse(rs)}
  end

  defp edit(:eof, _, {bef, aft}, _, rs) do
    {:done, get_line(bef, aft), [], reverse(rs, [{:move_rel, cp_len(aft)}])}
  end

  def prefix_arg(:none) do
    1
  end

  def prefix_arg({:ctlu, n}) do
    n
  end

  def prefix_arg(n) do
    n
  end

  defp key_map(a, _) when is_atom(a) do
    a
  end

  defp key_map(1, :none) do
    :beginning_of_line
  end

  defp key_map(2, :none) do
    :backward_char
  end

  defp key_map(4, :none) do
    :forward_delete_char
  end

  defp key_map(5, :none) do
    :end_of_line
  end

  defp key_map(6, :none) do
    :forward_char
  end

  defp key_map(?\b, :none) do
    :backward_delete_char
  end

  defp key_map(?\t, :none) do
    :tab_expand
  end

  defp key_map(?\f, :none) do
    :redraw_line
  end

  defp key_map(?\n, :none) do
    :new_line
  end

  defp key_map(?\v, :none) do
    :kill_line
  end

  defp key_map(?\r, :none) do
    :new_line
  end

  defp key_map(20, :none) do
    :transpose_char
  end

  defp key_map(21, :none) do
    :ctlu
  end

  defp key_map(29, :none) do
    :auto_blink
  end

  defp key_map(24, :none) do
    :ctlx
  end

  defp key_map(25, :none) do
    :yank
  end

  defp key_map(23, :none) do
    :backward_kill_word
  end

  defp key_map(?\e, :none) do
    :meta
  end

  defp key_map(?), prefix)
       when prefix !== :meta and
              prefix !== :search and prefix !== :search_meta do
    {:blink, ?), ?(}
  end

  defp key_map(?}, prefix)
       when prefix !== :meta and
              prefix !== :search and prefix !== :search_meta do
    {:blink, ?}, ?{}
  end

  defp key_map(?], prefix)
       when prefix !== :meta and
              prefix !== :search and prefix !== :search_meta do
    {:blink, ?], ?[}
  end

  defp key_map(?B, :meta) do
    :backward_word
  end

  defp key_map(?D, :meta) do
    :kill_word
  end

  defp key_map(?F, :meta) do
    :forward_word
  end

  defp key_map(?T, :meta) do
    :transpose_word
  end

  defp key_map(?Y, :meta) do
    :yank_pop
  end

  defp key_map(?b, :meta) do
    :backward_word
  end

  defp key_map(?d, :meta) do
    :kill_word
  end

  defp key_map(?f, :meta) do
    :forward_word
  end

  defp key_map(?t, :meta) do
    :transpose_word
  end

  defp key_map(?y, :meta) do
    :yank_pop
  end

  defp key_map(?O, :meta) do
    :meta_o
  end

  defp key_map(?H, :meta_o) do
    :beginning_of_line
  end

  defp key_map(?F, :meta_o) do
    :end_of_line
  end

  defp key_map(?\d, :none) do
    :backward_delete_char
  end

  defp key_map(?\d, :meta) do
    :backward_kill_word
  end

  defp key_map(?[, :meta) do
    :meta_left_sq_bracket
  end

  defp key_map(?H, :meta_left_sq_bracket) do
    :beginning_of_line
  end

  defp key_map(?F, :meta_left_sq_bracket) do
    :end_of_line
  end

  defp key_map(?D, :meta_left_sq_bracket) do
    :backward_char
  end

  defp key_map(?C, :meta_left_sq_bracket) do
    :forward_char
  end

  defp key_map(?\e, :meta) do
    :meta_meta
  end

  defp key_map(?[, :meta_meta) do
    :meta_csi
  end

  defp key_map(?C, :meta_csi) do
    :forward_word
  end

  defp key_map(?D, :meta_csi) do
    :backward_word
  end

  defp key_map(?1, :meta_left_sq_bracket) do
    {:csi, '1'}
  end

  defp key_map(?3, :meta_left_sq_bracket) do
    {:csi, '3'}
  end

  defp key_map(?5, :meta_left_sq_bracket) do
    {:csi, '5'}
  end

  defp key_map(?5, {:csi, '1;'}) do
    {:csi, '1;5'}
  end

  defp key_map(?~, {:csi, '3'}) do
    :forward_delete_char
  end

  defp key_map(?C, {:csi, '5'}) do
    :forward_word
  end

  defp key_map(?C, {:csi, '1;5'}) do
    :forward_word
  end

  defp key_map(?D, {:csi, '5'}) do
    :backward_word
  end

  defp key_map(?D, {:csi, '1;5'}) do
    :backward_word
  end

  defp key_map(?;, {:csi, '1'}) do
    {:csi, '1;'}
  end

  defp key_map(c, :none) when c >= ?\s do
    {:insert, c}
  end

  defp key_map(?\b, :search) do
    {:search, :backward_delete_char}
  end

  defp key_map(?\d, :search) do
    {:search, :backward_delete_char}
  end

  defp key_map(18, :search) do
    {:search, :skip_up}
  end

  defp key_map(19, :search) do
    {:search, :skip_down}
  end

  defp key_map(?\n, :search) do
    {:search, :search_found}
  end

  defp key_map(?\r, :search) do
    {:search, :search_found}
  end

  defp key_map(1, :search) do
    {:search, :search_quit}
  end

  defp key_map(2, :search) do
    {:search, :search_quit}
  end

  defp key_map(4, :search) do
    {:search, :search_quit}
  end

  defp key_map(5, :search) do
    {:search, :search_quit}
  end

  defp key_map(6, :search) do
    {:search, :search_quit}
  end

  defp key_map(?\t, :search) do
    {:search, :search_quit}
  end

  defp key_map(?\f, :search) do
    {:search, :search_quit}
  end

  defp key_map(20, :search) do
    {:search, :search_quit}
  end

  defp key_map(21, :search) do
    {:search, :search_quit}
  end

  defp key_map(29, :search) do
    {:search, :search_quit}
  end

  defp key_map(24, :search) do
    {:search, :search_quit}
  end

  defp key_map(25, :search) do
    {:search, :search_quit}
  end

  defp key_map(?\e, :search) do
    :search_meta
  end

  defp key_map(?[, :search_meta) do
    :search_meta_left_sq_bracket
  end

  defp key_map(_, :search_meta) do
    {:search, :search_quit}
  end

  defp key_map(_C, :search_meta_left_sq_bracket) do
    {:search, :search_quit}
  end

  defp key_map(c, :search) do
    {:insert_search, c}
  end

  defp key_map(c, _) do
    {:undefined, c}
  end

  defp do_op({:insert, c}, [], [], rs) do
    {{[c], []}, [{:put_chars, :unicode, [c]} | rs]}
  end

  defp do_op({:insert, c}, [bef | bef0], [], rs) do
    case :string.to_graphemes([bef, c]) do
      [gC] ->
        {{[gC | bef0], []}, [{:put_chars, :unicode, [c]} | rs]}

      _ ->
        {{[[c, bef] | bef0], []}, [{:put_chars, :unicode, [c]} | rs]}
    end
  end

  defp do_op({:insert, c}, [], aft, rs) do
    {{[c], aft}, [{:insert_chars, :unicode, [c]} | rs]}
  end

  defp do_op({:insert, c}, [bef | bef0], aft, rs) do
    case :string.to_graphemes([bef, c]) do
      [gC] ->
        {{[gC | bef0], aft}, [{:insert_chars, :unicode, [c]} | rs]}

      _ ->
        {{[[c, bef] | bef0], aft}, [{:insert_chars, :unicode, [c]} | rs]}
    end
  end

  defp do_op({:insert_search, c}, bef, [], rs) do
    aft = '\': '

    {{[c | bef], aft},
     [
       [{:insert_chars, :unicode, [c] ++ aft}, {:delete_chars, -3}]
       | rs
     ], :search}
  end

  defp do_op({:insert_search, c}, bef, aft, rs) do
    offset = cp_len(aft)
    nAft = '\': '

    {{[c | bef], nAft},
     [
       [{:insert_chars, :unicode, [c] ++ nAft}, {:delete_chars, -offset}]
       | rs
     ], :search}
  end

  defp do_op({:search, :backward_delete_char}, [_ | bef], aft, rs) do
    offset = cp_len(aft) + 1
    nAft = '\': '

    {{bef, nAft},
     [
       [{:insert_chars, :unicode, nAft}, {:delete_chars, -offset}]
       | rs
     ], :search}
  end

  defp do_op({:search, :backward_delete_char}, [], _Aft, rs) do
    aft = '\': '
    {{[], aft}, rs, :search}
  end

  defp do_op({:search, :skip_up}, bef, aft, rs) do
    offset = cp_len(aft)
    nAft = '\': '

    {{[18 | bef], nAft},
     [
       [{:insert_chars, :unicode, nAft}, {:delete_chars, -offset}]
       | rs
     ], :search}
  end

  defp do_op({:search, :skip_down}, bef, aft, rs) do
    offset = cp_len(aft)
    nAft = '\': '

    {{[19 | bef], nAft},
     [
       [{:insert_chars, :unicode, nAft}, {:delete_chars, -offset}]
       | rs
     ], :search}
  end

  defp do_op({:search, :search_found}, _Bef, aft, rs) do
    '\': ' ++ nAft = aft

    {{[], nAft},
     [
       [{:put_chars, :unicode, '\n'}, {:move_rel, -cp_len(aft)}]
       | rs
     ], :search_found}
  end

  defp do_op({:search, :search_quit}, _Bef, aft, rs) do
    '\': ' ++ nAft = aft

    {{[], nAft},
     [
       [{:put_chars, :unicode, '\n'}, {:move_rel, -cp_len(aft)}]
       | rs
     ], :search_quit}
  end

  defp do_op({:blink, c, m}, bef = [[?$, ?$] | _], aft, rs) do
    n = over_paren(bef, c, m)

    {:blink, n + 1, {[c | bef], aft},
     [
       [{:move_rel, -(n + 1)}, {:insert_chars, :unicode, [c]}]
       | rs
     ]}
  end

  defp do_op({:blink, c, _}, bef = [?$ | _], aft, rs) do
    do_op({:insert, c}, bef, aft, rs)
  end

  defp do_op({:blink, c, m}, bef, aft, rs) do
    case over_paren(bef, c, m) do
      :beep ->
        {{[c | bef], aft}, [[:beep, {:insert_chars, :unicode, [c]}] | rs]}

      n ->
        {:blink, n + 1, {[c | bef], aft},
         [
           [{:move_rel, -(n + 1)}, {:insert_chars, :unicode, [c]}]
           | rs
         ]}
    end
  end

  defp do_op(:auto_blink, bef, aft, rs) do
    case over_paren_auto(bef) do
      {n, paren} ->
        {:blink, n + 1, {[paren | bef], aft},
         [
           [{:move_rel, -(n + 1)}, {:insert_chars, :unicode, [paren]}]
           | rs
         ]}

      n ->
        {:blink, n + 1, {bef, aft}, [{:move_rel, -(n + 1)} | rs]}
    end
  end

  defp do_op(:forward_delete_char, bef, [gC | aft], rs) do
    {{bef, aft}, [{:delete_chars, gc_len(gC)} | rs]}
  end

  defp do_op(:backward_delete_char, [gC | bef], aft, rs) do
    {{bef, aft}, [{:delete_chars, -gc_len(gC)} | rs]}
  end

  defp do_op(:transpose_char, [[c1, c2] | bef], [], rs) do
    len = gc_len(c1) + gc_len(c2)

    {{[[c2, c1] | bef], []},
     [
       [{:put_chars, :unicode, [c1, c2]}, {:move_rel, -len}]
       | rs
     ]}
  end

  defp do_op(:transpose_char, [c2 | bef], [c1 | aft], rs) do
    len = gc_len(c2)

    {{[[c2, c1] | bef], aft},
     [
       [{:put_chars, :unicode, [c1, c2]}, {:move_rel, -len}]
       | rs
     ]}
  end

  defp do_op(:kill_word, bef, aft0, rs) do
    {aft1, kill0, n0} = over_non_word(aft0, [], 0)
    {aft, kill, n} = over_word(aft1, kill0, n0)
    :erlang.put(:kill_buffer, reverse(kill))
    {{bef, aft}, [{:delete_chars, n} | rs]}
  end

  defp do_op(:backward_kill_word, bef0, aft, rs) do
    {bef1, kill0, n0} = over_non_word(bef0, [], 0)
    {bef, kill, n} = over_word(bef1, kill0, n0)
    :erlang.put(:kill_buffer, kill)
    {{bef, aft}, [{:delete_chars, -n} | rs]}
  end

  defp do_op(:kill_line, bef, aft, rs) do
    :erlang.put(:kill_buffer, aft)
    {{bef, []}, [{:delete_chars, cp_len(aft)} | rs]}
  end

  defp do_op(:yank, bef, [], rs) do
    kill = :erlang.get(:kill_buffer)
    {{reverse(kill, bef), []}, [{:put_chars, :unicode, kill} | rs]}
  end

  defp do_op(:yank, bef, aft, rs) do
    kill = :erlang.get(:kill_buffer)
    {{reverse(kill, bef), aft}, [{:insert_chars, :unicode, kill} | rs]}
  end

  defp do_op(:forward_char, bef, [c | aft], rs) do
    {{[c | bef], aft}, [{:move_rel, gc_len(c)} | rs]}
  end

  defp do_op(:backward_char, [c | bef], aft, rs) do
    {{bef, [c | aft]}, [{:move_rel, -gc_len(c)} | rs]}
  end

  defp do_op(:forward_word, bef0, aft0, rs) do
    {aft1, bef1, n0} = over_non_word(aft0, bef0, 0)
    {aft, bef, n} = over_word(aft1, bef1, n0)
    {{bef, aft}, [{:move_rel, n} | rs]}
  end

  defp do_op(:backward_word, bef0, aft0, rs) do
    {bef1, aft1, n0} = over_non_word(bef0, aft0, 0)
    {bef, aft, n} = over_word(bef1, aft1, n0)
    {{bef, aft}, [{:move_rel, -n} | rs]}
  end

  defp do_op(:beginning_of_line, [_ | _] = bef, aft, rs) do
    {{[], reverse(bef, aft)}, [{:move_rel, -cp_len(bef)} | rs]}
  end

  defp do_op(:beginning_of_line, [], aft, rs) do
    {{[], aft}, rs}
  end

  defp do_op(:end_of_line, bef, [_ | _] = aft, rs) do
    {{reverse(aft, bef), []}, [{:move_rel, cp_len(aft)} | rs]}
  end

  defp do_op(:end_of_line, bef, [], rs) do
    {{bef, []}, rs}
  end

  defp do_op(:ctlu, bef, aft, rs) do
    :erlang.put(:kill_buffer, reverse(bef))
    {{[], aft}, [{:delete_chars, -cp_len(bef)} | rs]}
  end

  defp do_op(:beep, bef, aft, rs) do
    {{bef, aft}, [:beep | rs]}
  end

  defp do_op(_, bef, aft, rs) do
    {{bef, aft}, [:beep | rs]}
  end

  def over_word(cs, stack, n) do
    l =
      length(
        for ?' <- cs do
          1
        end
      )

    case rem(l, 2) do
      0 ->
        over_word1(cs, stack, n)

      1 ->
        until_quote(cs, stack, n)
    end
  end

  defp until_quote([?' | cs], stack, n) do
    {cs, [?' | stack], n + 1}
  end

  defp until_quote([c | cs], stack, n) do
    until_quote(cs, [c | stack], n + gc_len(c))
  end

  defp over_word1([?' = c | cs], stack, n) do
    until_quote(cs, [c | stack], n + 1)
  end

  defp over_word1(cs, stack, n) do
    over_word2(cs, stack, n)
  end

  defp over_word2([c | cs], stack, n) do
    case word_char(c) do
      true ->
        over_word2(cs, [c | stack], n + gc_len(c))

      false ->
        {[c | cs], stack, n}
    end
  end

  defp over_word2([], stack, n) when is_integer(n) do
    {[], stack, n}
  end

  defp over_non_word([c | cs], stack, n) do
    case word_char(c) do
      true ->
        {[c | cs], stack, n}

      false ->
        over_non_word(cs, [c | stack], n + gc_len(c))
    end
  end

  defp over_non_word([], stack, n) do
    {[], stack, n}
  end

  defp over_paren(chars, paren, match) do
    over_paren(chars, paren, match, 1, 1, [])
  end

  defp over_paren([[c, ?$, ?$] | cs], paren, match, d, n, l) do
    over_paren([c | cs], paren, match, d, n + 2, l)
  end

  defp over_paren([[gC, ?$] | cs], paren, match, d, n, l) do
    over_paren(cs, paren, match, d, n + 1 + gc_len(gC), l)
  end

  defp over_paren([match | _], _Paren, match, 1, n, _) do
    n
  end

  defp over_paren([match | cs], paren, match, d, n, [match | l]) do
    over_paren(cs, paren, match, d - 1, n + 1, l)
  end

  defp over_paren([paren | cs], paren, match, d, n, l) do
    over_paren(cs, paren, match, d + 1, n + 1, [match | l])
  end

  defp over_paren([?) | cs], paren, match, d, n, l) do
    over_paren(cs, paren, match, d, n + 1, [?( | l])
  end

  defp over_paren([?] | cs], paren, match, d, n, l) do
    over_paren(cs, paren, match, d, n + 1, [?[ | l])
  end

  defp over_paren([?} | cs], paren, match, d, n, l) do
    over_paren(cs, paren, match, d, n + 1, [?{ | l])
  end

  defp over_paren([?( | cs], paren, match, d, n, [?( | l]) do
    over_paren(cs, paren, match, d, n + 1, l)
  end

  defp over_paren([?[ | cs], paren, match, d, n, [?[ | l]) do
    over_paren(cs, paren, match, d, n + 1, l)
  end

  defp over_paren([?{ | cs], paren, match, d, n, [?{ | l]) do
    over_paren(cs, paren, match, d, n + 1, l)
  end

  defp over_paren([?( | _], _, _, _, _, _) do
    :beep
  end

  defp over_paren([?[ | _], _, _, _, _, _) do
    :beep
  end

  defp over_paren([?{ | _], _, _, _, _, _) do
    :beep
  end

  defp over_paren([gC | cs], paren, match, d, n, l) do
    over_paren(cs, paren, match, d, n + gc_len(gC), l)
  end

  defp over_paren([], _, _, _, _, _) do
    0
  end

  defp over_paren_auto(chars) do
    over_paren_auto(chars, 1, 1, [])
  end

  defp over_paren_auto([[c, ?$, ?$] | cs], d, n, l) do
    over_paren_auto([c | cs], d, n + 2, l)
  end

  defp over_paren_auto([[gC, ?$] | cs], d, n, l) do
    over_paren_auto(cs, d, n + 1 + gc_len(gC), l)
  end

  defp over_paren_auto([?( | _], _, n, []) do
    {n, ?)}
  end

  defp over_paren_auto([?[ | _], _, n, []) do
    {n, ?]}
  end

  defp over_paren_auto([?{ | _], _, n, []) do
    {n, ?}}
  end

  defp over_paren_auto([?) | cs], d, n, l) do
    over_paren_auto(cs, d, n + 1, [?( | l])
  end

  defp over_paren_auto([?] | cs], d, n, l) do
    over_paren_auto(cs, d, n + 1, [?[ | l])
  end

  defp over_paren_auto([?} | cs], d, n, l) do
    over_paren_auto(cs, d, n + 1, [?{ | l])
  end

  defp over_paren_auto([?( | cs], d, n, [?( | l]) do
    over_paren_auto(cs, d, n + 1, l)
  end

  defp over_paren_auto([?[ | cs], d, n, [?[ | l]) do
    over_paren_auto(cs, d, n + 1, l)
  end

  defp over_paren_auto([?{ | cs], d, n, [?{ | l]) do
    over_paren_auto(cs, d, n + 1, l)
  end

  defp over_paren_auto([gC | cs], d, n, l) do
    over_paren_auto(cs, d, n + gc_len(gC), l)
  end

  defp over_paren_auto([], _, _, _) do
    0
  end

  def erase_line({:line, pbs, {bef, aft}, _}) do
    reverse(erase(pbs, bef, aft, []))
  end

  def erase_inp({:line, _, {bef, aft}, _}) do
    reverse(erase([], bef, aft, []))
  end

  defp erase(pbs, bef, aft, rs) do
    [
      [{:delete_chars, -cp_len(pbs) - cp_len(bef)}, {:delete_chars, cp_len(aft)}]
      | rs
    ]
  end

  def redraw_line({:line, pbs, {bef, aft}, _}) do
    reverse(redraw(pbs, bef, aft, []))
  end

  defp redraw(pbs, bef, aft, rs) do
    [
      [
        {:move_rel, -cp_len(aft)},
        {:put_chars, :unicode, reverse(bef, aft)},
        {:put_chars, :unicode, pbs}
      ]
      | rs
    ]
  end

  def length_before({:line, pbs, {bef, _Aft}, _}) do
    cp_len(pbs) + cp_len(bef)
  end

  def length_after({:line, _, {_Bef, aft}, _}) do
    cp_len(aft)
  end

  def prompt({:line, pbs, _, _}) do
    pbs
  end

  def current_line({:line, _, {bef, aft}, _}) do
    get_line(bef, aft ++ '\n')
  end

  def current_chars({:line, _, {bef, aft}, _}) do
    get_line(bef, aft)
  end

  defp get_line(bef, aft) do
    :unicode.characters_to_list(reverse(bef, aft))
  end

  defp gc_len(cP) when is_integer(cP) do
    1
  end

  defp gc_len(cPs) when is_list(cPs) do
    length(cPs)
  end

  defp cp_len(str) do
    cp_len(str, 0)
  end

  defp cp_len([gC | r], len) do
    cp_len(r, len + gc_len(gC))
  end

  defp cp_len([], len) do
    len
  end
end
