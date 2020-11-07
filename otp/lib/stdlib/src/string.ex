defmodule :m_string do
  use Bitwise
  import Kernel, except: [length: 1]
  import :lists, only: [member: 2]

  def list_to_float(_) do
    :erlang.nif_error(:undef)
  end

  def list_to_integer(_) do
    :erlang.nif_error(:undef)
  end

  def is_empty([]) do
    true
  end

  def is_empty(<<>>) do
    true
  end

  def is_empty([l | r]) do
    is_empty(l) and is_empty(r)
  end

  def is_empty(_) do
    false
  end

  def length(<<cP1::utf8, bin::binary>>) do
    length_b(bin, cP1, 0)
  end

  def length(cD) do
    length_1(cD, 0)
  end

  def to_graphemes(cD0) do
    case :unicode_util.gc(cD0) do
      [gC | cD] ->
        [gC | to_graphemes(cD)]

      [] ->
        []

      {:error, err} ->
        :erlang.error({:badarg, err})
    end
  end

  def equal(a, b) when is_binary(a) and is_binary(b) do
    a === b
  end

  def equal(a, b) do
    equal_1(a, b)
  end

  def equal(a, b, false) do
    equal(a, b)
  end

  def equal(a, b, true) do
    equal_nocase(a, b)
  end

  def equal(a, b, case__, :none) do
    equal(a, b, case__)
  end

  def equal(a, b, false, norm) do
    equal_norm(a, b, norm)
  end

  def equal(a, b, true, norm) do
    equal_norm_nocase(a, b, norm)
  end

  def reverse(<<cP1::utf8, rest::binary>>) do
    reverse_b(rest, cP1, [])
  end

  def reverse(cD) do
    reverse_1(cD, [])
  end

  def slice(cD, n) when is_integer(n) and n >= 0 do
    case slice_l0(cD, n) do
      [] when is_binary(cD) ->
        <<>>

      res ->
        res
    end
  end

  def slice(cD, n, length)
      when is_integer(n) and n >= 0 and
             is_integer(length) and length > 0 do
    case slice_l0(cD, n) do
      [] when is_binary(cD) ->
        <<>>

      l ->
        slice_trail(l, length)
    end
  end

  def slice(cD, n, :infinity) do
    case slice_l0(cD, n) do
      [] when is_binary(cD) ->
        <<>>

      res ->
        res
    end
  end

  def slice(cD, _, 0) do
    case is_binary(cD) do
      true ->
        <<>>

      false ->
        []
    end
  end

  def pad(cD, length) do
    pad(cD, length, :trailing, ?\s)
  end

  def pad(cD, length, dir) do
    pad(cD, length, dir, ?\s)
  end

  def pad(cD, length, :leading, char)
      when is_integer(length) do
    len = length(cD)
    [:lists.duplicate(max(0, length - len), char), cD]
  end

  def pad(cD, length, :trailing, char)
      when is_integer(length) do
    len = length(cD)
    [cD | :lists.duplicate(max(0, length - len), char)]
  end

  def pad(cD, length, :both, char)
      when is_integer(length) do
    len = length(cD)
    size = max(0, length - len)
    pre = :lists.duplicate(div(size, 2), char)

    post =
      case rem(size, 2) do
        1 ->
          [char]

        _ ->
          []
      end

    [[pre, cD, pre] | post]
  end

  def trim(str) do
    trim(str, :both, :unicode_util.whitespace())
  end

  def trim(str, dir) do
    trim(str, dir, :unicode_util.whitespace())
  end

  def trim(str, _, []) do
    str
  end

  def trim(str, :leading, [sep])
      when is_list(str) and
             sep < 256 do
    trim_ls(str, sep)
  end

  def trim(str, :leading, sep) when is_list(sep) do
    trim_l(str, sep)
  end

  def trim(str, :trailing, [sep])
      when is_list(str) and
             sep < 256 do
    trim_ts(str, sep)
  end

  def trim(str, :trailing, seps0) when is_list(seps0) do
    seps = search_pattern(seps0)
    trim_t(str, 0, seps)
  end

  def trim(str, :both, sep) when is_list(sep) do
    trim(trim(str, :leading, sep), :trailing, sep)
  end

  def chomp(str) do
    trim(str, :trailing, [[?\r, ?\n], ?\n])
  end

  def take(str, sep) do
    take(str, sep, false, :leading)
  end

  def take(str, sep, complement) do
    take(str, sep, complement, :leading)
  end

  def take(str, [], complement, dir) do
    empty =
      case is_binary(str) do
        true ->
          <<>>

        false ->
          []
      end

    case {complement, dir} do
      {false, :leading} ->
        {empty, str}

      {false, :trailing} ->
        {str, empty}

      {true, :leading} ->
        {str, empty}

      {true, :trailing} ->
        {empty, str}
    end
  end

  def take(str, sep, false, :leading) do
    take_l(str, sep, [])
  end

  def take(str, sep0, true, :leading) do
    sep = search_pattern(sep0)
    take_lc(str, sep, [])
  end

  def take(str, sep0, false, :trailing) do
    sep = search_pattern(sep0)
    take_t(str, 0, sep)
  end

  def take(str, sep0, true, :trailing) do
    sep = search_pattern(sep0)
    take_tc(str, 0, sep)
  end

  def uppercase(cD) when is_list(cD) do
    try do
      uppercase_list(cD, false)
    catch
      :unchanged ->
        cD
    end
  end

  def uppercase(<<cP1::utf8, rest::binary>> = orig) do
    try do
      uppercase_bin(cP1, rest, false)
    catch
      :unchanged ->
        orig
    else
      list ->
        :unicode.characters_to_binary(list)
    end
  end

  def uppercase(<<>>) do
    <<>>
  end

  def uppercase(bin) do
    :erlang.error({:badarg, bin})
  end

  def lowercase(cD) when is_list(cD) do
    try do
      lowercase_list(cD, false)
    catch
      :unchanged ->
        cD
    end
  end

  def lowercase(<<cP1::utf8, rest::binary>> = orig) do
    try do
      lowercase_bin(cP1, rest, false)
    catch
      :unchanged ->
        orig
    else
      list ->
        :unicode.characters_to_binary(list)
    end
  end

  def lowercase(<<>>) do
    <<>>
  end

  def lowercase(bin) do
    :erlang.error({:badarg, bin})
  end

  def titlecase(cD) when is_list(cD) do
    case :unicode_util.titlecase(cD) do
      [gC | tail] ->
        append(gC, tail)

      empty ->
        empty
    end
  end

  def titlecase(cD) when is_binary(cD) do
    case :unicode_util.titlecase(cD) do
      [cP | chars] when is_integer(cP) ->
        <<cP::utf8, chars::binary>>

      [cPs | chars] ->
        <<for cP <- cPs, into: <<>> do
            <<cP::utf8>>
          end::binary, chars::binary>>

      [] ->
        <<>>
    end
  end

  def casefold(cD) when is_list(cD) do
    try do
      casefold_list(cD, false)
    catch
      :unchanged ->
        cD
    end
  end

  def casefold(<<cP1::utf8, rest::binary>> = orig) do
    try do
      casefold_bin(cP1, rest, false)
    catch
      :unchanged ->
        orig
    else
      list ->
        :unicode.characters_to_binary(list)
    end
  end

  def casefold(<<>>) do
    <<>>
  end

  def casefold(bin) do
    :erlang.error({:badarg, bin})
  end

  def to_integer(string) do
    try do
      take(string, '+-0123456789')
    catch
      _, _ ->
        {:error, :badarg}
    else
      {head, tail} ->
        case is_empty(head) do
          true ->
            {:error, :no_integer}

          false ->
            list = :unicode.characters_to_list(head)

            case :string.list_to_integer(list) do
              {:error, _} = err ->
                err

              {int, rest} ->
                to_number(string, int, rest, list, tail)
            end
        end
    end
  end

  def to_float(string) do
    try do
      take(string, '+-0123456789eE.,')
    catch
      _, _ ->
        {:error, :badarg}
    else
      {head, tail} ->
        case is_empty(head) do
          true ->
            {:error, :no_float}

          false ->
            list = :unicode.characters_to_list(head)

            case :string.list_to_float(list) do
              {:error, _} = err ->
                err

              {float, rest} ->
                to_number(string, float, rest, list, tail)
            end
        end
    end
  end

  defp to_number(string, number, rest, list, _Tail)
       when is_binary(string) do
    bSz = :erlang.length(list) - :erlang.length(rest)
    <<_::size(bSz)-binary, cont::binary>> = string
    {number, cont}
  end

  defp to_number(_, number, rest, _, tail) do
    {number, concat(rest, tail)}
  end

  def prefix(str, prefix0) do
    result =
      case :unicode.characters_to_list(prefix0) do
        [] ->
          str

        prefix ->
          prefix_1(str, prefix)
      end

    case result do
      [] when is_binary(str) ->
        <<>>

      res ->
        res
    end
  end

  def split(string, searchPattern) do
    split(string, searchPattern, :leading)
  end

  def split(string, searchPattern, where) do
    case is_empty(searchPattern) do
      true ->
        [string]

      false ->
        searchPatternCPs = :unicode.characters_to_list(searchPattern)

        case split_1(string, searchPatternCPs, 0, where, [], []) do
          {_Curr, []} ->
            [string]

          {_Curr, acc} when where === :trailing ->
            acc

          {curr, acc} when where === :all ->
            :lists.reverse([curr | acc])

          acc when is_list(acc) ->
            acc
        end
    end
  end

  def replace(string, searchPattern, replacement) do
    :lists.join(replacement, split(string, searchPattern))
  end

  def replace(string, searchPattern, replacement, where) do
    :lists.join(
      replacement,
      split(string, searchPattern, where)
    )
  end

  def lexemes([], _) do
    []
  end

  def lexemes(str, []) do
    [str]
  end

  def lexemes(str, seps0) when is_list(seps0) do
    seps = search_pattern(seps0)
    lexemes_m(str, seps, [])
  end

  def nth_lexeme(str, 1, []) do
    str
  end

  def nth_lexeme(str, n, seps0)
      when is_list(seps0) and
             is_integer(n) and n > 0 do
    seps = search_pattern(seps0)
    nth_lexeme_m(str, seps, n)
  end

  def find(string, searchPattern) do
    find(string, searchPattern, :leading)
  end

  def find(string, '', _) do
    string
  end

  def find(string, <<>>, _) do
    string
  end

  def find(string, searchPattern, :leading) do
    find_l(
      string,
      :unicode.characters_to_list(searchPattern)
    )
  end

  def find(string, searchPattern, :trailing) do
    find_r(string, :unicode.characters_to_list(searchPattern), :nomatch)
  end

  def next_grapheme(cD) do
    :unicode_util.gc(cD)
  end

  def next_codepoint(cD) do
    :unicode_util.cp(cD)
  end

  defp length_1([cP1 | [cP2 | _] = cont], n)
       when cP1 < 256 and
              cP2 < 256 and cP1 !== ?\r do
    length_1(cont, n + 1)
  end

  defp length_1(str, n) do
    case :unicode_util.gc(str) do
      [] ->
        n

      [_ | rest] ->
        length_1(rest, n + 1)

      {:error, err} ->
        :erlang.error({:badarg, err})
    end
  end

  defp length_b(<<cP2::utf8, rest::binary>>, cP1, n)
       when cP1 < 256 and cP2 < 256 and cP1 !== ?\r do
    length_b(rest, cP2, n + 1)
  end

  defp length_b(bin0, cP1, n) do
    [_ | bin1] = :unicode_util.gc([cP1 | bin0])

    case :unicode_util.cp(bin1) do
      [] ->
        n + 1

      [cP3 | bin] ->
        length_b(bin, cP3, n + 1)

      {:error, err} ->
        :erlang.error({:badarg, err})
    end
  end

  defp equal_1([a | aR], [b | bR])
       when is_integer(a) and
              is_integer(b) do
    a === b and equal_1(aR, bR)
  end

  defp equal_1([], bR) do
    is_empty(bR)
  end

  defp equal_1(a0, b0) do
    case {:unicode_util.cp(a0), :unicode_util.cp(b0)} do
      {[cP | a], [cP | b]} ->
        equal_1(a, b)

      {[], []} ->
        true

      {l1, l2} when is_list(l1) and is_list(l2) ->
        false
    end
  end

  defp equal_nocase(a, a) do
    true
  end

  defp equal_nocase(a0, b0) do
    case {:unicode_util.cp(:unicode_util.casefold(a0)),
          :unicode_util.cp(:unicode_util.casefold(b0))} do
      {[cP | a], [cP | b]} ->
        equal_nocase(a, b)

      {[], []} ->
        true

      {l1, l2} when is_list(l1) and is_list(l2) ->
        false
    end
  end

  defp equal_norm(a, a, _Norm) do
    true
  end

  defp equal_norm(a0, b0, norm) do
    case {:unicode_util.cp(apply(:unicode_util, norm, [a0])),
          :unicode_util.cp(apply(:unicode_util, norm, [b0]))} do
      {[cP | a], [cP | b]} ->
        equal_norm(a, b, norm)

      {[], []} ->
        true

      {l1, l2} when is_list(l1) and is_list(l2) ->
        false
    end
  end

  defp equal_norm_nocase(a, a, _Norm) do
    true
  end

  defp equal_norm_nocase(a0, b0, norm) do
    case {:unicode_util.cp(:unicode_util.casefold(apply(:unicode_util, norm, [a0]))),
          :unicode_util.cp(:unicode_util.casefold(apply(:unicode_util, norm, [b0])))} do
      {[cP | a], [cP | b]} ->
        equal_norm_nocase(a, b, norm)

      {[], []} ->
        true

      {l1, l2} when is_list(l1) and is_list(l2) ->
        false
    end
  end

  defp reverse_1([cP1 | [cP2 | _] = cont], acc)
       when cP1 < 256 and cP2 < 256 and cP1 !== ?\r do
    reverse_1(cont, [cP1 | acc])
  end

  defp reverse_1(cD, acc) do
    case :unicode_util.gc(cD) do
      [gC | rest] ->
        reverse_1(rest, [gC | acc])

      [] ->
        acc

      {:error, err} ->
        :erlang.error({:badarg, err})
    end
  end

  defp reverse_b(<<cP2::utf8, rest::binary>>, cP1, acc)
       when cP1 < 256 and cP2 < 256 and cP1 !== ?\r do
    reverse_b(rest, cP2, [cP1 | acc])
  end

  defp reverse_b(bin0, cP1, acc) do
    [gC | bin1] = :unicode_util.gc([cP1 | bin0])

    case :unicode_util.cp(bin1) do
      [] ->
        [gC | acc]

      [cP3 | bin] ->
        reverse_b(bin, cP3, [gC | acc])

      {:error, err} ->
        :erlang.error({:badarg, err})
    end
  end

  defp slice_l0(<<cP1::utf8, bin::binary>>, n) when n > 0 do
    slice_lb(bin, cP1, n)
  end

  defp slice_l0(l, n) do
    slice_l(l, n)
  end

  defp slice_l([cP1 | [cP2 | _] = cont], n)
       when cP1 < 256 and
              cP2 < 256 and cP1 !== ?\r and
              n > 0 do
    slice_l(cont, n - 1)
  end

  defp slice_l(cD, n) when n > 0 do
    case :unicode_util.gc(cD) do
      [_ | cont] ->
        slice_l(cont, n - 1)

      [] ->
        []

      {:error, err} ->
        :erlang.error({:badarg, err})
    end
  end

  defp slice_l(cont, 0) do
    cont
  end

  defp slice_lb(<<cP2::utf8, bin::binary>>, cP1, n)
       when cP1 < 256 and cP2 < 256 and cP1 !== ?\r and
              n > 1 do
    slice_lb(bin, cP2, n - 1)
  end

  defp slice_lb(bin, cP1, n) do
    [_ | rest] = :unicode_util.gc([cP1 | bin])

    cond do
      n > 1 ->
        case :unicode_util.cp(rest) do
          [cP2 | cont] ->
            slice_lb(cont, cP2, n - 1)

          [] ->
            <<>>

          {:error, err} ->
            :erlang.error({:badarg, err})
        end

      n === 1 ->
        rest
    end
  end

  defp slice_trail(orig, n) when is_binary(orig) do
    case orig do
      <<cP1::utf8, bin::binary>> when n > 0 ->
        length = slice_bin(bin, cP1, n)
        sz = byte_size(orig) - length
        <<keep::size(sz)-binary, _::binary>> = orig
        keep

      <<_, _::binary>> when n > 0 ->
        :erlang.error({:badarg, orig})

      _ ->
        <<>>
    end
  end

  defp slice_trail(cD, n) when is_list(cD) do
    slice_list(cD, n)
  end

  defp slice_list([cP1 | [cP2 | _] = cont], n)
       when cP1 < 256 and
              cP2 < 256 and cP1 !== ?\r and
              n > 0 do
    [cP1 | slice_list(cont, n - 1)]
  end

  defp slice_list(cD, n) when n > 0 do
    case :unicode_util.gc(cD) do
      [gC | cont] ->
        append(gC, slice_list(cont, n - 1))

      [] ->
        []

      {:error, err} ->
        :erlang.error({:badarg, err})
    end
  end

  defp slice_list(_, 0) do
    []
  end

  defp slice_bin(<<cP2::utf8, bin::binary>>, cP1, n)
       when cP1 < 256 and cP2 < 256 and cP1 !== ?\r and
              n > 0 do
    slice_bin(bin, cP2, n - 1)
  end

  defp slice_bin(cD, cP1, n) when n > 0 do
    [_ | bin] = :unicode_util.gc([cP1 | cD])

    case :unicode_util.cp(bin) do
      [cP2 | cont] ->
        slice_bin(cont, cP2, n - 1)

      [] ->
        0

      {:error, err} ->
        :erlang.error({:badarg, err})
    end
  end

  defp slice_bin(cD, cP1, 0) do
    byte_size(cD) + byte_size(<<cP1::utf8>>)
  end

  defp uppercase_list([cP1 | [cP2 | _] = cont], _Changed)
       when ?a <= cP1 and cP1 <= ?z and cP2 < 256 do
    [cP1 - 32 | uppercase_list(cont, true)]
  end

  defp uppercase_list([cP1 | [cP2 | _] = cont], changed)
       when cP1 < 128 and cP2 < 256 do
    [cP1 | uppercase_list(cont, changed)]
  end

  defp uppercase_list([], true) do
    []
  end

  defp uppercase_list([], false) do
    throw(:unchanged)
  end

  defp uppercase_list(cPs0, changed) do
    case :unicode_util.uppercase(cPs0) do
      [char | cPs] when char === hd(cPs0) ->
        [char | uppercase_list(cPs, changed)]

      [char | cPs] ->
        append(char, uppercase_list(cPs, true))

      [] ->
        uppercase_list([], changed)
    end
  end

  defp uppercase_bin(cP1, <<cP2::utf8, bin::binary>>, _Changed)
       when ?a <= cP1 and cP1 <= ?z and cP2 < 256 do
    [cP1 - 32 | uppercase_bin(cP2, bin, true)]
  end

  defp uppercase_bin(cP1, <<cP2::utf8, bin::binary>>, changed)
       when cP1 < 128 and cP2 < 256 do
    [cP1 | uppercase_bin(cP2, bin, changed)]
  end

  defp uppercase_bin(cP1, bin, changed) do
    case :unicode_util.uppercase([cP1 | bin]) do
      [^cP1 | cPs] ->
        case :unicode_util.cp(cPs) do
          [next | rest] ->
            [cP1 | uppercase_bin(next, rest, changed)]

          [] when changed ->
            [cP1]

          [] ->
            throw(:unchanged)

          {:error, err} ->
            :erlang.error({:badarg, err})
        end

      [char | cPs] ->
        case :unicode_util.cp(cPs) do
          [next | rest] ->
            [char | uppercase_bin(next, rest, true)]

          [] ->
            [char]

          {:error, err} ->
            :erlang.error({:badarg, err})
        end
    end
  end

  defp lowercase_list([cP1 | [cP2 | _] = cont], _Changed)
       when ?A <= cP1 and cP1 <= ?Z and cP2 < 256 do
    [cP1 + 32 | lowercase_list(cont, true)]
  end

  defp lowercase_list([cP1 | [cP2 | _] = cont], changed)
       when cP1 < 128 and cP2 < 256 do
    [cP1 | lowercase_list(cont, changed)]
  end

  defp lowercase_list([], true) do
    []
  end

  defp lowercase_list([], false) do
    throw(:unchanged)
  end

  defp lowercase_list(cPs0, changed) do
    case :unicode_util.lowercase(cPs0) do
      [char | cPs] when char === hd(cPs0) ->
        [char | lowercase_list(cPs, changed)]

      [char | cPs] ->
        append(char, lowercase_list(cPs, true))

      [] ->
        lowercase_list([], changed)
    end
  end

  defp lowercase_bin(cP1, <<cP2::utf8, bin::binary>>, _Changed)
       when ?A <= cP1 and cP1 <= ?Z and cP2 < 256 do
    [cP1 + 32 | lowercase_bin(cP2, bin, true)]
  end

  defp lowercase_bin(cP1, <<cP2::utf8, bin::binary>>, changed)
       when cP1 < 128 and cP2 < 256 do
    [cP1 | lowercase_bin(cP2, bin, changed)]
  end

  defp lowercase_bin(cP1, bin, changed) do
    case :unicode_util.lowercase([cP1 | bin]) do
      [^cP1 | cPs] ->
        case :unicode_util.cp(cPs) do
          [next | rest] ->
            [cP1 | lowercase_bin(next, rest, changed)]

          [] when changed ->
            [cP1]

          [] ->
            throw(:unchanged)

          {:error, err} ->
            :erlang.error({:badarg, err})
        end

      [char | cPs] ->
        case :unicode_util.cp(cPs) do
          [next | rest] ->
            [char | lowercase_bin(next, rest, true)]

          [] ->
            [char]

          {:error, err} ->
            :erlang.error({:badarg, err})
        end
    end
  end

  defp casefold_list([cP1 | [cP2 | _] = cont], _Changed)
       when ?A <= cP1 and cP1 <= ?Z and cP2 < 256 do
    [cP1 + 32 | casefold_list(cont, true)]
  end

  defp casefold_list([cP1 | [cP2 | _] = cont], changed)
       when cP1 < 128 and cP2 < 256 do
    [cP1 | casefold_list(cont, changed)]
  end

  defp casefold_list([], true) do
    []
  end

  defp casefold_list([], false) do
    throw(:unchanged)
  end

  defp casefold_list(cPs0, changed) do
    case :unicode_util.casefold(cPs0) do
      [char | cPs] when char === hd(cPs0) ->
        [char | casefold_list(cPs, changed)]

      [char | cPs] ->
        append(char, casefold_list(cPs, true))

      [] ->
        casefold_list([], changed)
    end
  end

  defp casefold_bin(cP1, <<cP2::utf8, bin::binary>>, _Changed)
       when ?A <= cP1 and cP1 <= ?Z and cP2 < 256 do
    [cP1 + 32 | casefold_bin(cP2, bin, true)]
  end

  defp casefold_bin(cP1, <<cP2::utf8, bin::binary>>, changed)
       when cP1 < 128 and cP2 < 256 do
    [cP1 | casefold_bin(cP2, bin, changed)]
  end

  defp casefold_bin(cP1, bin, changed) do
    case :unicode_util.casefold([cP1 | bin]) do
      [^cP1 | cPs] ->
        case :unicode_util.cp(cPs) do
          [next | rest] ->
            [cP1 | casefold_bin(next, rest, changed)]

          [] when changed ->
            [cP1]

          [] ->
            throw(:unchanged)

          {:error, err} ->
            :erlang.error({:badarg, err})
        end

      [char | cPs] ->
        case :unicode_util.cp(cPs) do
          [next | rest] ->
            [char | casefold_bin(next, rest, true)]

          [] ->
            [char]

          {:error, err} ->
            :erlang.error({:badarg, err})
        end
    end
  end

  defp trim_ls([cP1 | [cP2 | _] = cont] = str, sep)
       when cP1 < 256 and cP2 < 256 and cP1 !== ?\r do
    case sep do
      ^cP1 ->
        trim_ls(cont, sep)

      _ ->
        str
    end
  end

  defp trim_ls(str, sep) do
    trim_l(str, [sep])
  end

  defp trim_l([cP1 | [cP2 | _] = cont] = str, sep)
       when cP1 < 256 and cP2 < 256 and cP1 !== ?\r do
    case :lists.member(cP1, sep) do
      true ->
        trim_l(cont, sep)

      false ->
        str
    end
  end

  defp trim_l([bin | cont0], sep) when is_binary(bin) do
    case bin_search_inv(bin, cont0, sep) do
      {:nomatch, cont} ->
        trim_l(cont, sep)

      keep ->
        keep
    end
  end

  defp trim_l(str, sep) when is_list(str) do
    case :unicode_util.gc(str) do
      [c | cs] ->
        case :lists.member(c, sep) do
          true ->
            trim_l(cs, sep)

          false ->
            str
        end

      [] ->
        []
    end
  end

  defp trim_l(bin, sep) when is_binary(bin) do
    case bin_search_inv(bin, [], sep) do
      {:nomatch, _} ->
        <<>>

      [keep] ->
        keep
    end
  end

  defp trim_ts([sep | cs1] = str, sep) do
    case cs1 do
      [] ->
        []

      [cP2 | _] when sep < 256 and cP2 < 256 and sep !== ?\r ->
        tail = trim_ts(cs1, sep)

        case is_empty(tail) do
          true ->
            []

          false ->
            [sep | tail]
        end

      _ ->
        trim_t(str, 0, search_pattern([sep]))
    end
  end

  defp trim_ts([cP | cont], sep) when is_integer(cP) do
    [cP | trim_ts(cont, sep)]
  end

  defp trim_ts(str, sep) do
    trim_t(str, 0, search_pattern([sep]))
  end

  defp trim_t([cP1 | cont] = cs0, _, {gCs, cPs, _} = seps)
       when is_integer(cP1) do
    case :lists.member(cP1, cPs) do
      true ->
        [gC | cs1] = :unicode_util.gc(cs0)

        case :lists.member(gC, gCs) do
          true ->
            tail = trim_t(cs1, 0, seps)

            case is_empty(tail) do
              true ->
                []

              false ->
                append(gC, tail)
            end

          false ->
            append(gC, trim_t(cs1, 0, seps))
        end

      false ->
        [cP1 | trim_t(cont, 0, seps)]
    end
  end

  defp trim_t([bin | cont0], n, {gCs, _, _} = seps0)
       when is_binary(bin) do
    <<_::size(n)-binary, rest::binary>> = bin
    seps = search_compile(seps0)

    case bin_search(rest, cont0, seps) do
      {:nomatch, _} ->
        stack(bin, trim_t(cont0, 0, seps))

      [sepStart | cont1] ->
        case bin_search_inv(sepStart, cont1, gCs) do
          {:nomatch, cont} ->
            tail = trim_t(cont, 0, seps)

            case is_empty(tail) do
              true ->
                keepSz = byte_size(bin) - byte_size(sepStart)
                <<keep::size(keepSz)-binary, _::binary>> = bin
                keep

              false ->
                used = cp_prefix(cont0, cont)
                stack(bin, stack(used, tail))
            end

          [nonSep | cont] when is_binary(nonSep) ->
            keepSz = byte_size(bin) - byte_size(nonSep)
            trim_t([bin | cont], keepSz, seps)
        end
    end
  end

  defp trim_t(str, 0, {gCs, _, _} = seps) when is_list(str) do
    case :unicode_util.gc(str) do
      [gC | cs1] ->
        case :lists.member(gC, gCs) do
          true ->
            tail = trim_t(cs1, 0, seps)

            case is_empty(tail) do
              true ->
                []

              false ->
                append(gC, tail)
            end

          false ->
            append(gC, trim_t(cs1, 0, seps))
        end

      [] ->
        []
    end
  end

  defp trim_t(bin, n, {gCs, _, _} = seps0)
       when is_binary(bin) do
    <<_::size(n)-binary, rest::binary>> = bin
    seps = search_compile(seps0)

    case bin_search(rest, [], seps) do
      {:nomatch, _} ->
        bin

      [sepStart] ->
        case bin_search_inv(sepStart, [], gCs) do
          {:nomatch, _} ->
            keepSz = byte_size(bin) - byte_size(sepStart)
            <<keep::size(keepSz)-binary, _::binary>> = bin
            keep

          [nonSep] ->
            keepSz = byte_size(bin) - byte_size(nonSep)
            trim_t(bin, keepSz, seps)
        end
    end
  end

  defp take_l([cP1 | [cP2 | _] = cont] = str, seps, acc)
       when cP1 < 256 and cP2 < 256 and cP1 !== ?\r do
    case :lists.member(cP1, seps) do
      true ->
        take_l(cont, seps, [cP1 | acc])

      false ->
        {rev(acc), str}
    end
  end

  defp take_l([bin | cont0], seps, acc) when is_binary(bin) do
    case bin_search_inv(bin, cont0, seps) do
      {:nomatch, cont} ->
        used = cp_prefix(cont0, cont)
        take_l(cont, seps, [:unicode.characters_to_binary([bin | used]) | acc])

      [bin1 | _] = after__ when is_binary(bin1) ->
        first = byte_size(bin) - byte_size(bin1)
        <<keep::size(first)-binary, _::binary>> = bin
        {btoken(keep, acc), after__}
    end
  end

  defp take_l(str, seps, acc) when is_list(str) do
    case :unicode_util.gc(str) do
      [c | cs] ->
        case :lists.member(c, seps) do
          true ->
            take_l(cs, seps, append(rev(c), acc))

          false ->
            {rev(acc), str}
        end

      [] ->
        {rev(acc), []}
    end
  end

  defp take_l(bin, seps, acc) when is_binary(bin) do
    case bin_search_inv(bin, [], seps) do
      {:nomatch, _} ->
        {btoken(bin, acc), <<>>}

      [after__] ->
        first = byte_size(bin) - byte_size(after__)
        <<keep::size(first)-binary, _::binary>> = bin
        {btoken(keep, acc), after__}
    end
  end

  defp take_lc([cP1 | cont] = str0, {gCs, cPs, _} = seps, acc)
       when is_integer(cP1) do
    case :lists.member(cP1, cPs) do
      true ->
        [gC | str] = :unicode_util.gc(str0)

        case :lists.member(gC, gCs) do
          false ->
            take_lc(str, seps, append(rev(gC), acc))

          true ->
            {rev(acc), str0}
        end

      false ->
        take_lc(cont, seps, append(cP1, acc))
    end
  end

  defp take_lc([bin | cont0], seps0, acc)
       when is_binary(bin) do
    seps = search_compile(seps0)

    case bin_search(bin, cont0, seps) do
      {:nomatch, cont} ->
        used = cp_prefix(cont0, cont)
        take_lc(cont, seps, [:unicode.characters_to_binary([bin | used]) | acc])

      [bin1 | _] = after__ when is_binary(bin1) ->
        first = byte_size(bin) - byte_size(bin1)
        <<keep::size(first)-binary, _::binary>> = bin
        {btoken(keep, acc), after__}
    end
  end

  defp take_lc(str, {gCs, _, _} = seps, acc)
       when is_list(str) do
    case :unicode_util.gc(str) do
      [c | cs] ->
        case :lists.member(c, gCs) do
          false ->
            take_lc(cs, seps, append(rev(c), acc))

          true ->
            {rev(acc), str}
        end

      [] ->
        {rev(acc), []}
    end
  end

  defp take_lc(bin, seps0, acc) when is_binary(bin) do
    seps = search_compile(seps0)

    case bin_search(bin, [], seps) do
      {:nomatch, _} ->
        {btoken(bin, acc), <<>>}

      [after__] ->
        first = byte_size(bin) - byte_size(after__)
        <<keep::size(first)-binary, _::binary>> = bin
        {btoken(keep, acc), after__}
    end
  end

  defp take_t([cP1 | cont] = str0, _, {gCs, cPs, _} = seps)
       when is_integer(cP1) do
    case :lists.member(cP1, cPs) do
      true ->
        [gC | str] = :unicode_util.gc(str0)

        case :lists.member(gC, gCs) do
          true ->
            {head, tail} = take_t(str, 0, seps)

            case is_empty(head) do
              true ->
                {head, append(gC, tail)}

              false ->
                {append(gC, head), tail}
            end

          false ->
            {head, tail} = take_t(str, 0, seps)
            {append(gC, head), tail}
        end

      false ->
        {head, tail} = take_t(cont, 0, seps)
        {[cP1 | head], tail}
    end
  end

  defp take_t([bin | cont0], n, {gCs, _, _} = seps0)
       when is_binary(bin) do
    <<_::size(n)-binary, rest::binary>> = bin
    seps = search_compile(seps0)

    case bin_search(rest, cont0, seps) do
      {:nomatch, cont} ->
        used = cp_prefix(cont0, cont)
        {head, tail} = take_t(cont, 0, seps)

        {stack(
           :unicode.characters_to_binary([bin | used]),
           head
         ), tail}

      [sepStart | cont1] ->
        case bin_search_inv(sepStart, cont1, gCs) do
          {:nomatch, cont} ->
            {head, tail} = take_t(cont, 0, seps)
            used = cp_prefix(cont0, cont)

            case is_empty(head) do
              true ->
                keepSz = byte_size(bin) - byte_size(sepStart)
                <<keep::size(keepSz)-binary, end__::binary>> = bin
                {keep, stack(stack(end__, used), tail)}

              false ->
                {stack(
                   :unicode.characters_to_binary([bin | used]),
                   head
                 ), tail}
            end

          [nonSep | cont] when is_binary(nonSep) ->
            keepSz = byte_size(bin) - byte_size(nonSep)
            take_t([bin | cont], keepSz, seps)
        end
    end
  end

  defp take_t(str, 0, {gCs, _, _} = seps) when is_list(str) do
    case :unicode_util.gc(str) do
      [gC | cs1] ->
        case :lists.member(gC, gCs) do
          true ->
            {head, tail} = take_t(cs1, 0, seps)

            case is_empty(head) do
              true ->
                {head, append(gC, tail)}

              false ->
                {append(gC, head), tail}
            end

          false ->
            {head, tail} = take_t(cs1, 0, seps)
            {append(gC, head), tail}
        end

      [] ->
        {[], []}
    end
  end

  defp take_t(bin, n, {gCs, _, _} = seps0)
       when is_binary(bin) do
    <<_::size(n)-binary, rest::binary>> = bin
    seps = search_compile(seps0)

    case bin_search(rest, [], seps) do
      {:nomatch, _} ->
        {bin, <<>>}

      [sepStart] ->
        case bin_search_inv(sepStart, [], gCs) do
          {:nomatch, _} ->
            keepSz = byte_size(bin) - byte_size(sepStart)
            <<before::size(keepSz)-binary, end__::binary>> = bin
            {before, end__}

          [nonSep] ->
            keepSz = byte_size(bin) - byte_size(nonSep)
            take_t(bin, keepSz, seps)
        end
    end
  end

  defp take_tc([cP1 | [cP2 | _] = cont], _, {gCs, _, _} = seps)
       when cP1 < 256 and cP2 < 256 and cP1 !== ?\r do
    case :lists.member(cP1, gCs) do
      false ->
        {head, tail} = take_tc(cont, 0, seps)

        case is_empty(head) do
          true ->
            {head, append(cP1, tail)}

          false ->
            {append(cP1, head), tail}
        end

      true ->
        {head, tail} = take_tc(cont, 0, seps)
        {append(cP1, head), tail}
    end
  end

  defp take_tc([bin | cont0], n, {gCs, _, _} = seps0)
       when is_binary(bin) do
    <<_::size(n)-binary, rest::binary>> = bin

    case bin_search_inv(rest, cont0, gCs) do
      {:nomatch, cont} ->
        used = cp_prefix(cont0, cont)
        {head, tail} = take_tc(cont, 0, seps0)

        {stack(
           :unicode.characters_to_binary([bin | used]),
           head
         ), tail}

      [sepStart | cont1] ->
        seps = search_compile(seps0)

        case bin_search(sepStart, cont1, seps) do
          {:nomatch, cont} ->
            {head, tail} = take_tc(cont, 0, seps)
            used = cp_prefix(cont0, cont)

            case is_empty(head) do
              true ->
                keepSz = byte_size(bin) - byte_size(sepStart)
                <<keep::size(keepSz)-binary, end__::binary>> = bin
                {keep, stack(stack(end__, used), tail)}

              false ->
                {stack(
                   :unicode.characters_to_binary([bin | used]),
                   head
                 ), tail}
            end

          [nonSep | cont] when is_binary(nonSep) ->
            keepSz = byte_size(bin) - byte_size(nonSep)
            take_tc([bin | cont], keepSz, seps)
        end
    end
  end

  defp take_tc(str, 0, {gCs, _, _} = seps) when is_list(str) do
    case :unicode_util.gc(str) do
      [gC | cs1] ->
        case :lists.member(gC, gCs) do
          false ->
            {head, tail} = take_tc(cs1, 0, seps)

            case is_empty(head) do
              true ->
                {head, append(gC, tail)}

              false ->
                {append(gC, head), tail}
            end

          true ->
            {head, tail} = take_tc(cs1, 0, seps)
            {append(gC, head), tail}
        end

      [] ->
        {[], []}
    end
  end

  defp take_tc(bin, n, {gCs, _, _} = seps0)
       when is_binary(bin) do
    <<_::size(n)-binary, rest::binary>> = bin

    case bin_search_inv(rest, [], gCs) do
      {:nomatch, _} ->
        {bin, <<>>}

      [sepStart] ->
        seps = search_compile(seps0)

        case bin_search(sepStart, [], seps) do
          {:nomatch, _} ->
            keepSz = byte_size(bin) - byte_size(sepStart)
            <<before::size(keepSz)-binary, end__::binary>> = bin
            {before, end__}

          [nonSep] ->
            keepSz = byte_size(bin) - byte_size(nonSep)
            take_tc(bin, keepSz, seps)
        end
    end
  end

  defp prefix_1(cs0, [gC]) do
    case :unicode_util.gc(cs0) do
      [^gC | cs] ->
        cs

      _ ->
        :nomatch
    end
  end

  defp prefix_1([cP | cs], [pre | preR]) when is_integer(cP) do
    case cP === pre do
      true ->
        prefix_1(cs, preR)

      false ->
        :nomatch
    end
  end

  defp prefix_1(<<cP::utf8, cs::binary>>, [pre | preR]) do
    case cP === pre do
      true ->
        prefix_1(cs, preR)

      false ->
        :nomatch
    end
  end

  defp prefix_1(cs0, [pre | preR]) do
    case :unicode_util.cp(cs0) do
      [^pre | cs] ->
        prefix_1(cs, preR)

      _ ->
        :nomatch
    end
  end

  defp split_1([cP1 | cs] = cs0, [c | _] = needle, _, where, curr, acc)
       when is_integer(cP1) do
    case cP1 === c do
      true ->
        case prefix_1(cs0, needle) do
          :nomatch ->
            split_1(cs, needle, 0, where, append(c, curr), acc)

          rest when where === :leading ->
            [rev(curr), rest]

          rest when where === :trailing ->
            split_1(cs, needle, 0, where, [c | curr], [rev(curr), rest])

          rest when where === :all ->
            split_1(rest, needle, 0, where, [], [rev(curr) | acc])
        end

      false ->
        split_1(cs, needle, 0, where, append(cP1, curr), acc)
    end
  end

  defp split_1([bin | cont0], needle, start, where, curr0, acc)
       when is_binary(bin) do
    case bin_search_str(bin, start, cont0, needle) do
      {:nomatch, sz, cont} ->
        <<keep::size(sz)-binary, _::binary>> = bin
        split_1(cont, needle, 0, where, [keep | curr0], acc)

      {before, [cs0 | cont], after__} ->
        curr = add_non_empty(before, curr0)

        case where do
          :leading ->
            [rev(curr), after__]

          :trailing ->
            <<_::utf8, cs::binary>> = cs0
            next = byte_size(bin) - byte_size(cs)
            split_1([bin | cont], needle, next, where, curr0, [rev(curr), after__])

          :all ->
            split_1(after__, needle, 0, where, [], [rev(curr) | acc])
        end
    end
  end

  defp split_1(cs0, [c | _] = needle, _, where, curr, acc)
       when is_list(cs0) do
    case :unicode_util.cp(cs0) do
      [^c | cs] ->
        case prefix_1(cs0, needle) do
          :nomatch ->
            split_1(cs, needle, 0, where, append(c, curr), acc)

          rest when where === :leading ->
            [rev(curr), rest]

          rest when where === :trailing ->
            split_1(cs, needle, 0, where, [c | curr], [rev(curr), rest])

          rest when where === :all ->
            split_1(rest, needle, 0, where, [], [rev(curr) | acc])
        end

      [other | cs] ->
        split_1(cs, needle, 0, where, append(other, curr), acc)

      [] ->
        {rev(curr), acc}
    end
  end

  defp split_1(bin, [_C | _] = needle, start, where, curr0, acc) do
    case bin_search_str(bin, start, [], needle) do
      {:nomatch, _, _} ->
        <<_::size(start)-binary, keep::binary>> = bin
        {rev([keep | curr0]), acc}

      {before, [cs0], after__} ->
        case where do
          :leading ->
            [rev([before | curr0]), after__]

          :trailing ->
            <<_::utf8, cs::binary>> = cs0
            next = byte_size(bin) - byte_size(cs)
            split_1(bin, needle, next, where, curr0, [btoken(before, curr0), after__])

          :all ->
            next = byte_size(bin) - byte_size(after__)
            <<_::size(start)-binary, keep::binary>> = before
            curr = [keep | curr0]
            split_1(bin, needle, next, where, [], [rev(curr) | acc])
        end
    end
  end

  defp lexemes_m([cP | _] = cs0, {gCs, cPs, _} = seps0, ts)
       when is_integer(cP) do
    case :lists.member(cP, cPs) do
      true ->
        [gC | cs2] = :unicode_util.gc(cs0)

        case :lists.member(gC, gCs) do
          true ->
            lexemes_m(cs2, seps0, ts)

          false ->
            seps = search_compile(seps0)
            {lexeme, rest} = lexeme_pick(cs0, seps, [])
            lexemes_m(rest, seps, [lexeme | ts])
        end

      false ->
        seps = search_compile(seps0)
        {lexeme, rest} = lexeme_pick(cs0, seps, [])
        lexemes_m(rest, seps, [lexeme | ts])
    end
  end

  defp lexemes_m([bin | cont0], {gCs, _, _} = seps0, ts)
       when is_binary(bin) do
    case bin_search_inv(bin, cont0, gCs) do
      {:nomatch, cont} ->
        lexemes_m(cont, seps0, ts)

      cs ->
        seps = search_compile(seps0)
        {lexeme, rest} = lexeme_pick(cs, seps, [])
        lexemes_m(rest, seps, [lexeme | ts])
    end
  end

  defp lexemes_m(cs0, {gCs, _, _} = seps0, ts)
       when is_list(cs0) do
    case :unicode_util.gc(cs0) do
      [c | cs] ->
        case :lists.member(c, gCs) do
          true ->
            lexemes_m(cs, seps0, ts)

          false ->
            seps = search_compile(seps0)
            {lexeme, rest} = lexeme_pick(cs0, seps, [])
            lexemes_m(rest, seps, [lexeme | ts])
        end

      [] ->
        :lists.reverse(ts)
    end
  end

  defp lexemes_m(bin, {gCs, _, _} = seps0, ts)
       when is_binary(bin) do
    case bin_search_inv(bin, [], gCs) do
      {:nomatch, _} ->
        :lists.reverse(ts)

      [cs] ->
        seps = search_compile(seps0)
        {lexeme, rest} = lexeme_pick(cs, seps, [])
        lexemes_m(rest, seps, add_non_empty(lexeme, ts))
    end
  end

  defp lexeme_pick([cP | cs1] = cs0, {gCs, cPs, _} = seps, tkn)
       when is_integer(cP) do
    case :lists.member(cP, cPs) do
      true ->
        [gC | cs2] = :unicode_util.gc(cs0)

        case :lists.member(gC, gCs) do
          true ->
            {rev(tkn), cs2}

          false ->
            lexeme_pick(cs2, seps, append(rev(gC), tkn))
        end

      false ->
        lexeme_pick(cs1, seps, [cP | tkn])
    end
  end

  defp lexeme_pick([bin | cont0], seps, tkn) when is_binary(bin) do
    case bin_search(bin, cont0, seps) do
      {:nomatch, _} ->
        lexeme_pick(cont0, seps, [bin | tkn])

      [left | _Cont] = cs ->
        bytes = byte_size(bin) - byte_size(left)
        <<lexeme::size(bytes)-binary, _::binary>> = bin
        {btoken(lexeme, tkn), cs}
    end
  end

  defp lexeme_pick(cs0, {gCs, cPs, _} = seps, tkn)
       when is_list(cs0) do
    case :unicode_util.cp(cs0) do
      [cP | cs] ->
        case :lists.member(cP, cPs) do
          true ->
            [gC | cs2] = :unicode_util.gc(cs0)

            case :lists.member(gC, gCs) do
              true ->
                {rev(tkn), cs2}

              false ->
                lexeme_pick(cs2, seps, append(rev(gC), tkn))
            end

          false ->
            lexeme_pick(cs, seps, append(cP, tkn))
        end

      [] ->
        {rev(tkn), []}
    end
  end

  defp lexeme_pick(bin, seps, tkn) when is_binary(bin) do
    case bin_search(bin, [], seps) do
      {:nomatch, _} ->
        {btoken(bin, tkn), []}

      [left] ->
        bytes = byte_size(bin) - byte_size(left)
        <<lexeme::size(bytes)-binary, _::binary>> = bin
        {btoken(lexeme, tkn), left}
    end
  end

  defp nth_lexeme_m([bin | cont0], {gCs, _, _} = seps0, n)
       when is_binary(bin) do
    case bin_search_inv(bin, cont0, gCs) do
      {:nomatch, cont} ->
        nth_lexeme_m(cont, seps0, n)

      cs when n > 1 ->
        rest = lexeme_skip(cs, seps0)
        nth_lexeme_m(rest, seps0, n - 1)

      cs ->
        seps = search_compile(seps0)
        {lexeme, _} = lexeme_pick(cs, seps, [])
        lexeme
    end
  end

  defp nth_lexeme_m(cs0, {gCs, _, _} = seps0, n)
       when is_list(cs0) do
    case :unicode_util.gc(cs0) do
      [c | cs] ->
        case :lists.member(c, gCs) do
          true ->
            nth_lexeme_m(cs, seps0, n)

          false when n > 1 ->
            cs1 = lexeme_skip(cs, seps0)
            nth_lexeme_m(cs1, seps0, n - 1)

          false ->
            seps = search_compile(seps0)
            {lexeme, _} = lexeme_pick(cs0, seps, [])
            lexeme
        end

      [] ->
        []
    end
  end

  defp nth_lexeme_m(bin, {gCs, _, _} = seps0, n)
       when is_binary(bin) do
    seps = search_compile(seps0)

    case bin_search_inv(bin, [], gCs) do
      [cs] when n > 1 ->
        cs1 = lexeme_skip(cs, seps)
        nth_lexeme_m(cs1, seps, n - 1)

      [cs] ->
        {lexeme, _} = lexeme_pick(cs, seps, [])
        lexeme

      {:nomatch, _} ->
        <<>>
    end
  end

  defp lexeme_skip([cP | cs1] = cs0, {gCs, cPs, _} = seps)
       when is_integer(cP) do
    case :lists.member(cP, cPs) do
      true ->
        [gC | cs2] = :unicode_util.gc(cs0)

        case :lists.member(gC, gCs) do
          true ->
            cs2

          false ->
            lexeme_skip(cs2, seps)
        end

      false ->
        lexeme_skip(cs1, seps)
    end
  end

  defp lexeme_skip([bin | cont0], seps0) when is_binary(bin) do
    seps = search_compile(seps0)

    case bin_search(bin, cont0, seps) do
      {:nomatch, _} ->
        lexeme_skip(cont0, seps)

      cs ->
        tl(:unicode_util.gc(cs))
    end
  end

  defp lexeme_skip(cs0, {gCs, cPs, _} = seps) when is_list(cs0) do
    case :unicode_util.cp(cs0) do
      [cP | cs] ->
        case :lists.member(cP, cPs) do
          true ->
            [gC | cs2] = :unicode_util.gc(cs0)

            case :lists.member(gC, gCs) do
              true ->
                cs2

              false ->
                lexeme_skip(cs2, seps)
            end

          false ->
            lexeme_skip(cs, seps)
        end

      [] ->
        []
    end
  end

  defp lexeme_skip(bin, seps0) when is_binary(bin) do
    seps = search_compile(seps0)

    case bin_search(bin, [], seps) do
      {:nomatch, _} ->
        <<>>

      [left] ->
        tl(:unicode_util.gc(left))
    end
  end

  defp find_l([c1 | cs] = cs0, [c | _] = needle)
       when is_integer(c1) do
    case c1 do
      ^c ->
        case prefix_1(cs0, needle) do
          :nomatch ->
            find_l(cs, needle)

          _ ->
            cs0
        end

      _ ->
        find_l(cs, needle)
    end
  end

  defp find_l([bin | cont0], needle) when is_binary(bin) do
    case bin_search_str(bin, 0, cont0, needle) do
      {:nomatch, _, cont} ->
        find_l(cont, needle)

      {_Before, cs, _After} ->
        cs
    end
  end

  defp find_l(cs0, [c | _] = needle) when is_list(cs0) do
    case :unicode_util.cp(cs0) do
      [^c | cs] ->
        case prefix_1(cs0, needle) do
          :nomatch ->
            find_l(cs, needle)

          _ ->
            cs0
        end

      [_C | cs] ->
        find_l(cs, needle)

      [] ->
        :nomatch
    end
  end

  defp find_l(bin, needle) do
    case bin_search_str(bin, 0, [], needle) do
      {:nomatch, _, _} ->
        :nomatch

      {_Before, [cs], _After} ->
        cs
    end
  end

  defp find_r([cp | cs] = cs0, [c | _] = needle, res)
       when is_integer(cp) do
    case cp do
      ^c ->
        case prefix_1(cs0, needle) do
          :nomatch ->
            find_r(cs, needle, res)

          _ ->
            find_r(cs, needle, cs0)
        end

      _ ->
        find_r(cs, needle, res)
    end
  end

  defp find_r([bin | cont0], needle, res)
       when is_binary(bin) do
    case bin_search_str(bin, 0, cont0, needle) do
      {:nomatch, _, cont} ->
        find_r(cont, needle, res)

      {_, cs0, _} ->
        [_ | cs] = :unicode_util.gc(cs0)
        find_r(cs, needle, cs0)
    end
  end

  defp find_r(cs0, [c | _] = needle, res) when is_list(cs0) do
    case :unicode_util.cp(cs0) do
      [^c | cs] ->
        case prefix_1(cs0, needle) do
          :nomatch ->
            find_r(cs, needle, res)

          _ ->
            find_r(cs, needle, cs0)
        end

      [_C | cs] ->
        find_r(cs, needle, res)

      [] ->
        res
    end
  end

  defp find_r(bin, needle, res) do
    case bin_search_str(bin, 0, [], needle) do
      {:nomatch, _, _} ->
        res

      {_Before, [cs0], _After} ->
        <<_::utf8, cs::binary>> = cs0
        find_r(cs, needle, cs0)
    end
  end

  defp btoken(token, []) do
    token
  end

  defp btoken(binPart, [c]) when is_integer(c) do
    <<c::utf8, binPart::binary>>
  end

  defp btoken(<<>>, tkn) do
    :lists.reverse(tkn)
  end

  defp btoken(binPart, cs) do
    [:lists.reverse(cs), binPart]
  end

  defp rev([b]) when is_binary(b) do
    b
  end

  defp rev(l) when is_list(l) do
    :lists.reverse(l)
  end

  defp rev(c) when is_integer(c) do
    c
  end

  defp append(char, <<>>) when is_integer(char) do
    [char]
  end

  defp append(char, <<>>) when is_list(char) do
    char
  end

  defp append(char, bin) when is_binary(bin) do
    [char, bin]
  end

  defp append(char, str) when is_integer(char) do
    [char | str]
  end

  defp append(gC, str) when is_list(gC) do
    gC ++ str
  end

  defp stack(bin, []) do
    bin
  end

  defp stack(<<>>, st) do
    st
  end

  defp stack([], st) do
    st
  end

  defp stack(bin, st) do
    [bin | st]
  end

  defp add_non_empty(<<>>, l) do
    l
  end

  defp add_non_empty(token, l) do
    [token | l]
  end

  defp cp_prefix(orig, cont) do
    case :unicode_util.cp(cont) do
      [] ->
        orig

      [cp | rest] ->
        cp_prefix_1(orig, cp, rest)
    end
  end

  defp cp_prefix_1(orig, until, cont) do
    case :unicode_util.cp(orig) do
      [^until | rest] ->
        case equal(rest, cont) do
          true ->
            []

          false ->
            [until | cp_prefix_1(rest, until, cont)]
        end

      [cP | rest] ->
        [cP | cp_prefix_1(rest, until, cont)]
    end
  end

  defp bin_search(bin, cont, {seps, _, bP}) do
    bin_search_loop(bin, 0, bP, cont, seps)
  end

  defp search_pattern({_, _, _} = p) do
    p
  end

  defp search_pattern(seps) do
    cPs = search_cp(seps)
    {seps, cPs, :undefined}
  end

  defp search_compile({sep, cPs, :undefined}) do
    {sep, cPs, :binary.compile_pattern(bin_pattern(cPs))}
  end

  defp search_compile({_, _, _} = compiled) do
    compiled
  end

  defp search_cp([cP | seps]) when is_integer(cP) do
    [cP | search_cp(seps)]
  end

  defp search_cp([pattern | seps]) do
    [cP | _] = :unicode_util.cp(pattern)
    [cP | search_cp(seps)]
  end

  defp search_cp([]) do
    []
  end

  defp bin_pattern([cP | seps]) do
    [<<cP::utf8>> | bin_pattern(seps)]
  end

  defp bin_pattern([]) do
    []
  end

  defp bin_search_loop(bin0, start, _, cont, _Seps)
       when byte_size(bin0) <= start or start < 0 do
    {:nomatch, cont}
  end

  defp bin_search_loop(bin0, start, binSeps, cont, seps) do
    <<_::size(start)-binary, bin::binary>> = bin0

    case :binary.match(bin, binSeps) do
      :nomatch ->
        {:nomatch, cont}

      {where, _CL} when cont === [] ->
        <<_::size(where)-binary, cont1::binary>> = bin
        [gC | cont2] = :unicode_util.gc(cont1)

        case :lists.member(gC, seps) do
          false when cont2 === [] ->
            {:nomatch, []}

          false ->
            next = byte_size(bin0) - byte_size(cont2)
            bin_search_loop(bin0, next, binSeps, cont, seps)

          true ->
            [cont1]
        end

      {where, _CL} ->
        <<_::size(where)-binary, cont0::binary>> = bin
        cont1 = [cont0 | cont]
        [gC | cont2] = :unicode_util.gc(cont1)

        case :lists.member(gC, seps) do
          false ->
            case cont2 do
              [binR | ^cont] when is_binary(binR) ->
                next = byte_size(bin0) - byte_size(binR)
                bin_search_loop(bin0, next, binSeps, cont, seps)

              _ ->
                {:nomatch, cont2}
            end

          true ->
            cont1
        end
    end
  end

  defp bin_search_inv(<<>>, cont, _) do
    {:nomatch, cont}
  end

  defp bin_search_inv(bin, cont, [sep]) do
    bin_search_inv_1(bin, cont, sep)
  end

  defp bin_search_inv(bin, cont, seps) do
    bin_search_inv_n(bin, cont, seps)
  end

  defp bin_search_inv_1(<<cP1::utf8, binRest::binary>> = bin0, cont, sep) do
    case binRest do
      <<cP2::utf8, _::binary>>
      when cP1 < 256 and
             cP2 < 256 and cP1 !== ?\r ->
        case cP1 do
          ^sep ->
            bin_search_inv_1(binRest, cont, sep)

          _ ->
            [bin0 | cont]
        end

      _ when cont === [] ->
        case :unicode_util.gc(bin0) do
          [^sep | bin] ->
            bin_search_inv_1(bin, cont, sep)

          _ ->
            [bin0 | cont]
        end

      _ ->
        case :unicode_util.gc([bin0 | cont]) do
          [[^sep, bin] | ^cont] when is_binary(bin) ->
            bin_search_inv_1(bin, cont, sep)

          [^sep | cs] ->
            {:nomatch, cs}

          _ ->
            [bin0 | cont]
        end
    end
  end

  defp bin_search_inv_1(<<>>, cont, _Sep) do
    {:nomatch, cont}
  end

  defp bin_search_inv_1([], cont, _Sep) do
    {:nomatch, cont}
  end

  defp bin_search_inv_1(bin, _, _) do
    :erlang.error({:badarg, bin})
  end

  defp bin_search_inv_n(<<cP1::utf8, binRest::binary>> = bin0, cont, seps) do
    case binRest do
      <<cP2::utf8, _::binary>>
      when cP1 < 256 and
             cP2 < 256 and cP1 !== ?\r ->
        case :lists.member(cP1, seps) do
          true ->
            bin_search_inv_n(binRest, cont, seps)

          false ->
            [bin0 | cont]
        end

      _ when cont === [] ->
        [gC | bin] = :unicode_util.gc(bin0)

        case :lists.member(gC, seps) do
          true ->
            bin_search_inv_n(bin, cont, seps)

          false ->
            [bin0 | cont]
        end

      _ ->
        [gC | cs0] = :unicode_util.gc([bin0 | cont])

        case :lists.member(gC, seps) do
          false ->
            [bin0 | cont]

          true ->
            case cs0 do
              [bin | ^cont] when is_binary(bin) ->
                bin_search_inv_n(bin, cont, seps)

              _ ->
                {:nomatch, cs0}
            end
        end
    end
  end

  defp bin_search_inv_n(<<>>, cont, _Sep) do
    {:nomatch, cont}
  end

  defp bin_search_inv_n([], cont, _Sep) do
    {:nomatch, cont}
  end

  defp bin_search_inv_n(bin, _, _) do
    :erlang.error({:badarg, bin})
  end

  defp bin_search_str(bin0, start, [], searchCPs) do
    compiled = :binary.compile_pattern(:unicode.characters_to_binary(searchCPs))
    bin_search_str_1(bin0, start, compiled, searchCPs)
  end

  defp bin_search_str(bin0, start, cont, [cP | _] = searchCPs) do
    first = :binary.compile_pattern(<<cP::utf8>>)
    bin_search_str_2(bin0, start, cont, first, searchCPs)
  end

  defp bin_search_str_1(bin0, start, first, searchCPs) do
    <<_::size(start)-binary, bin::binary>> = bin0

    case :binary.match(bin, first) do
      :nomatch ->
        {:nomatch, byte_size(bin0), []}

      {where0, _} ->
        where = start + where0
        <<keep::size(where)-binary, cs0::binary>> = bin0

        case prefix_1(cs0, searchCPs) do
          :nomatch ->
            <<_::utf8, cs::binary>> = cs0
            keepSz = byte_size(bin0) - byte_size(cs)
            bin_search_str_1(bin0, keepSz, first, searchCPs)

          [] ->
            {keep, [cs0], <<>>}

          rest ->
            {keep, [cs0], rest}
        end
    end
  end

  defp bin_search_str_2(bin0, start, cont, first, searchCPs) do
    <<_::size(start)-binary, bin::binary>> = bin0

    case :binary.match(bin, first) do
      :nomatch ->
        {:nomatch, byte_size(bin0), cont}

      {where0, _} ->
        where = start + where0
        <<keep::size(where)-binary, cs0::binary>> = bin0
        [gC | cs] = :unicode_util.gc(cs0)

        case prefix_1(stack(cs0, cont), searchCPs) do
          :nomatch when is_binary(cs) ->
            keepSz = byte_size(bin0) - byte_size(cs)
            bin_search_str_2(bin0, keepSz, cont, first, searchCPs)

          :nomatch ->
            {:nomatch, where, stack([gC | cs], cont)}

          [] ->
            {keep, [cs0 | cont], <<>>}

          rest ->
            {keep, [cs0 | cont], rest}
        end
    end
  end

  def len(s) do
    :erlang.length(s)
  end

  def concat(s1, s2) do
    s1 ++ s2
  end

  def chr(s, c) when is_integer(c) do
    chr(s, c, 1)
  end

  defp chr([c | _Cs], c, i) do
    i
  end

  defp chr([_ | cs], c, i) do
    chr(cs, c, i + 1)
  end

  defp chr([], _C, _I) do
    0
  end

  def rchr(s, c) when is_integer(c) do
    rchr(s, c, 1, 0)
  end

  defp rchr([c | cs], c, i, _L) do
    rchr(cs, c, i + 1, i)
  end

  defp rchr([_ | cs], c, i, l) do
    rchr(cs, c, i + 1, l)
  end

  defp rchr([], _C, _I, l) do
    l
  end

  def str(s, sub) when is_list(sub) do
    str(s, sub, 1)
  end

  defp str([c | s], [c | sub], i) do
    case l_prefix(sub, s) do
      true ->
        i

      false ->
        str(s, [c | sub], i + 1)
    end
  end

  defp str([_ | s], sub, i) do
    str(s, sub, i + 1)
  end

  defp str([], _Sub, _I) do
    0
  end

  def rstr(s, sub) when is_list(sub) do
    rstr(s, sub, 1, 0)
  end

  defp rstr([c | s], [c | sub], i, l) do
    case l_prefix(sub, s) do
      true ->
        rstr(s, [c | sub], i + 1, i)

      false ->
        rstr(s, [c | sub], i + 1, l)
    end
  end

  defp rstr([_ | s], sub, i, l) do
    rstr(s, sub, i + 1, l)
  end

  defp rstr([], _Sub, _I, l) do
    l
  end

  defp l_prefix([c | pre], [c | string]) do
    l_prefix(pre, string)
  end

  defp l_prefix([], string) when is_list(string) do
    true
  end

  defp l_prefix(pre, string)
       when is_list(pre) and
              is_list(string) do
    false
  end

  def span(s, cs) when is_list(cs) do
    span(s, cs, 0)
  end

  defp span([c | s], cs, i) do
    case member(c, cs) do
      true ->
        span(s, cs, i + 1)

      false ->
        i
    end
  end

  defp span([], _Cs, i) do
    i
  end

  def cspan(s, cs) when is_list(cs) do
    cspan(s, cs, 0)
  end

  defp cspan([c | s], cs, i) do
    case member(c, cs) do
      true ->
        i

      false ->
        cspan(s, cs, i + 1)
    end
  end

  defp cspan([], _Cs, i) do
    i
  end

  def substr(string, 1) when is_list(string) do
    string
  end

  def substr(string, s) when is_integer(s) and s > 1 do
    substr2(string, s)
  end

  def substr(string, s, l)
      when is_integer(s) and s >= 1 and
             is_integer(l) and l >= 0 do
    substr1(substr2(string, s), l)
  end

  defp substr1([c | string], l) when l > 0 do
    [c | substr1(string, l - 1)]
  end

  defp substr1(string, _L) when is_list(string) do
    []
  end

  defp substr2(string, 1) when is_list(string) do
    string
  end

  defp substr2([_ | string], s) do
    substr2(string, s - 1)
  end

  def tokens(s, seps) do
    case seps do
      [] ->
        case s do
          [] ->
            []

          [_ | _] ->
            [s]
        end

      [c] ->
        tokens_single_1(:lists.reverse(s), c, [])

      [_ | _] ->
        tokens_multiple_1(:lists.reverse(s), seps, [])
    end
  end

  defp tokens_single_1([sep | s], sep, toks) do
    tokens_single_1(s, sep, toks)
  end

  defp tokens_single_1([c | s], sep, toks) do
    tokens_single_2(s, sep, toks, [c])
  end

  defp tokens_single_1([], _, toks) do
    toks
  end

  defp tokens_single_2([sep | s], sep, toks, tok) do
    tokens_single_1(s, sep, [tok | toks])
  end

  defp tokens_single_2([c | s], sep, toks, tok) do
    tokens_single_2(s, sep, toks, [c | tok])
  end

  defp tokens_single_2([], _Sep, toks, tok) do
    [tok | toks]
  end

  defp tokens_multiple_1([c | s], seps, toks) do
    case member(c, seps) do
      true ->
        tokens_multiple_1(s, seps, toks)

      false ->
        tokens_multiple_2(s, seps, toks, [c])
    end
  end

  defp tokens_multiple_1([], _Seps, toks) do
    toks
  end

  defp tokens_multiple_2([c | s], seps, toks, tok) do
    case member(c, seps) do
      true ->
        tokens_multiple_1(s, seps, [tok | toks])

      false ->
        tokens_multiple_2(s, seps, toks, [c | tok])
    end
  end

  defp tokens_multiple_2([], _Seps, toks, tok) do
    [tok | toks]
  end

  def chars(c, n) do
    chars(c, n, [])
  end

  def chars(c, n, tail) when n > 0 do
    chars(c, n - 1, [c | tail])
  end

  def chars(c, 0, tail) when is_integer(c) do
    tail
  end

  def copies(charList, num)
      when is_list(charList) and
             is_integer(num) and num >= 0 do
    copies(charList, num, [])
  end

  defp copies(_CharList, 0, r) do
    r
  end

  defp copies(charList, num, r) do
    copies(charList, num - 1, charList ++ r)
  end

  def words(string) do
    words(string, ?\s)
  end

  def words(string, char) when is_integer(char) do
    w_count(strip(string, :both, char), char, 0)
  end

  defp w_count([], _, num) do
    num + 1
  end

  defp w_count([h | t], h, num) do
    w_count(strip(t, :left, h), h, num + 1)
  end

  defp w_count([_H | t], char, num) do
    w_count(t, char, num)
  end

  def sub_word(string, index) do
    sub_word(string, index, ?\s)
  end

  def sub_word(string, index, char)
      when is_integer(index) and
             is_integer(char) do
    case words(string, char) do
      num when num < index ->
        []

      _Num ->
        s_word(strip(string, :left, char), index, char, 1, [])
    end
  end

  defp s_word([], _, _, _, res) do
    :lists.reverse(res)
  end

  defp s_word([char | _], index, char, index, res) do
    :lists.reverse(res)
  end

  defp s_word([h | t], index, char, index, res) do
    s_word(t, index, char, index, [h | res])
  end

  defp s_word([char | t], stop, char, index, res)
       when index < stop do
    s_word(strip(t, :left, char), stop, char, index + 1, res)
  end

  defp s_word([_ | t], stop, char, index, res)
       when index < stop do
    s_word(t, stop, char, index, res)
  end

  def strip(string) do
    strip(string, :both)
  end

  def strip(string, :left) do
    strip_left(string, ?\s)
  end

  def strip(string, :right) do
    strip_right(string, ?\s)
  end

  def strip(string, :both) do
    strip_right(strip_left(string, ?\s), ?\s)
  end

  def strip(string, :right, char) do
    strip_right(string, char)
  end

  def strip(string, :left, char) do
    strip_left(string, char)
  end

  def strip(string, :both, char) do
    strip_right(strip_left(string, char), char)
  end

  defp strip_left([sc | s], sc) do
    strip_left(s, sc)
  end

  defp strip_left([_ | _] = s, sc) when is_integer(sc) do
    s
  end

  defp strip_left([], sc) when is_integer(sc) do
    []
  end

  defp strip_right([sc | s], sc) do
    case strip_right(s, sc) do
      [] ->
        []

      t ->
        [sc | t]
    end
  end

  defp strip_right([c | s], sc) do
    [c | strip_right(s, sc)]
  end

  defp strip_right([], sc) when is_integer(sc) do
    []
  end

  def left(string, len) when is_integer(len) do
    left(string, len, ?\s)
  end

  def left(string, len, char) when is_integer(char) do
    slen = :erlang.length(string)

    cond do
      slen > len ->
        substr(string, 1, len)

      slen < len ->
        l_pad(string, len - slen, char)

      slen === len ->
        string
    end
  end

  defp l_pad(string, num, char) do
    string ++ chars(char, num)
  end

  def right(string, len) when is_integer(len) do
    right(string, len, ?\s)
  end

  def right(string, len, char) when is_integer(char) do
    slen = :erlang.length(string)

    cond do
      slen > len ->
        substr(string, slen - len + 1)

      slen < len ->
        r_pad(string, len - slen, char)

      slen === len ->
        string
    end
  end

  defp r_pad(string, num, char) do
    chars(char, num, string)
  end

  def centre(string, len) when is_integer(len) do
    centre(string, len, ?\s)
  end

  def centre(string, 0, char)
      when is_list(string) and
             is_integer(char) do
    []
  end

  def centre(string, len, char) when is_integer(char) do
    slen = :erlang.length(string)

    cond do
      slen > len ->
        substr(string, div(slen - len, 2) + 1, len)

      slen < len ->
        n = div(len - slen, 2)
        r_pad(l_pad(string, len - (slen + n), char), n, char)

      slen === len ->
        string
    end
  end

  def sub_string(string, start) do
    substr(string, start)
  end

  def sub_string(string, start, stop) do
    substr(string, start, stop - start + 1)
  end

  defp to_lower_char(c)
       when is_integer(c) and ?A <= c and
              c <= ?Z do
    c + 32
  end

  defp to_lower_char(c)
       when is_integer(c) and 192 <= c and
              c <= 214 do
    c + 32
  end

  defp to_lower_char(c)
       when is_integer(c) and 216 <= c and
              c <= 222 do
    c + 32
  end

  defp to_lower_char(c) do
    c
  end

  defp to_upper_char(c)
       when is_integer(c) and ?a <= c and
              c <= ?z do
    c - 32
  end

  defp to_upper_char(c)
       when is_integer(c) and 224 <= c and
              c <= 246 do
    c - 32
  end

  defp to_upper_char(c)
       when is_integer(c) and 248 <= c and
              c <= 254 do
    c - 32
  end

  defp to_upper_char(c) do
    c
  end

  def to_lower(s) when is_list(s) do
    for c <- s do
      to_lower_char(c)
    end
  end

  def to_lower(c) when is_integer(c) do
    to_lower_char(c)
  end

  def to_upper(s) when is_list(s) do
    for c <- s do
      to_upper_char(c)
    end
  end

  def to_upper(c) when is_integer(c) do
    to_upper_char(c)
  end

  def join([], sep) when is_list(sep) do
    []
  end

  def join([h | t], sep) do
    h ++
      :lists.append(
        for x <- t do
          sep ++ x
        end
      )
  end
end
