defmodule :m_xmerl_ucs do
  use Bitwise

  def is_iso10646(ch) when is_integer(ch) and ch >= 0 do
    cond do
      ch < 55296 ->
        true

      ch < 57344 ->
        false

      ch < 65534 ->
        true

      ch <= 65535 ->
        false

      ch <= 2_147_483_647 ->
        true

      true ->
        false
    end
  end

  def is_iso10646(_) do
    false
  end

  def is_unicode(ch) when ch < 1_114_112 do
    is_iso10646(ch)
  end

  def is_unicode(_) do
    false
  end

  def is_bmpchar(ch) when is_integer(ch) and ch >= 0 do
    cond do
      ch < 55296 ->
        true

      ch < 57344 ->
        false

      ch < 65534 ->
        true

      true ->
        false
    end
  end

  def is_bmpchar(_) do
    false
  end

  def is_latin1(ch)
      when is_integer(ch) and ch >= 0 and
             ch <= 255 do
    true
  end

  def is_latin1(_) do
    false
  end

  def is_ascii(ch)
      when is_integer(ch) and ch >= 0 and
             ch <= 127 do
    true
  end

  def is_ascii(_) do
    false
  end

  def is_iso646_basic(ch) when is_integer(ch) and ch >= ?\s do
    cond do
      ch <= ?Z ->
        cond do
          ch > ?$ ->
            ch !== ?@

          true ->
            ch < ?#
        end

      ch > ?z ->
        false

      ch >= ?a ->
        true

      true ->
        ch === ?_
    end
  end

  def is_iso646_basic(_) do
    false
  end

  def is_visible_latin1(ch) when is_integer(ch) and ch >= ?\s do
    cond do
      ch <= ?~ ->
        true

      ch >= 161 ->
        ch <= 255
    end
  end

  def is_visible_latin1(_) do
    false
  end

  def is_visible_ascii(ch) when is_integer(ch) and ch >= ?\s do
    ch <= ?~
  end

  def is_visible_ascii(_) do
    false
  end

  def to_ucs4be(list) when is_list(list) do
    :lists.flatmap(&to_ucs4be/1, list)
  end

  def to_ucs4be(ch) do
    char_to_ucs4be(ch)
  end

  def from_ucs4be(bin) when is_binary(bin) do
    from_ucs4be(bin, [], [])
  end

  def from_ucs4be(list) do
    from_ucs4be(:erlang.list_to_binary(list), [], [])
  end

  def from_ucs4be(bin, tail) when is_binary(bin) do
    from_ucs4be(bin, [], tail)
  end

  def from_ucs4be(list, tail) do
    from_ucs4be(:erlang.list_to_binary(list), [], tail)
  end

  def to_ucs4le(list) when is_list(list) do
    :lists.flatmap(&to_ucs4le/1, list)
  end

  def to_ucs4le(ch) do
    char_to_ucs4le(ch)
  end

  def from_ucs4le(bin) when is_binary(bin) do
    from_ucs4le(bin, [], [])
  end

  def from_ucs4le(list) do
    from_ucs4le(:erlang.list_to_binary(list), [], [])
  end

  def from_ucs4le(bin, tail) when is_binary(bin) do
    from_ucs4le(bin, [], tail)
  end

  def from_ucs4le(list, tail) do
    from_ucs4le(:erlang.list_to_binary(list), [], tail)
  end

  def to_ucs2be(list) when is_list(list) do
    :lists.flatmap(&to_ucs2be/1, list)
  end

  def to_ucs2be(ch) do
    char_to_ucs2be(ch)
  end

  def from_ucs2be(bin) when is_binary(bin) do
    from_ucs2be(bin, [], [])
  end

  def from_ucs2be(list) do
    from_ucs2be(:erlang.list_to_binary(list), [], [])
  end

  def from_ucs2be(bin, tail) when is_binary(bin) do
    from_ucs2be(bin, [], tail)
  end

  def from_ucs2be(list, tail) do
    from_ucs2be(:erlang.list_to_binary(list), [], tail)
  end

  def to_ucs2le(list) when is_list(list) do
    :lists.flatmap(&to_ucs2le/1, list)
  end

  def to_ucs2le(ch) do
    char_to_ucs2le(ch)
  end

  def from_ucs2le(bin) when is_binary(bin) do
    from_ucs2le(bin, [], [])
  end

  def from_ucs2le(list) do
    from_ucs2le(:erlang.list_to_binary(list), [], [])
  end

  def from_ucs2le(bin, tail) when is_binary(bin) do
    from_ucs2le(bin, [], tail)
  end

  def from_ucs2le(list, tail) do
    from_ucs2le(:erlang.list_to_binary(list), [], tail)
  end

  def to_utf16be(list) when is_list(list) do
    :lists.flatmap(&to_utf16be/1, list)
  end

  def to_utf16be(ch) do
    char_to_utf16be(ch)
  end

  def from_utf16be(bin) when is_binary(bin) do
    from_utf16be(bin, [], [])
  end

  def from_utf16be(list) do
    from_utf16be(:erlang.list_to_binary(list), [], [])
  end

  def from_utf16be(bin, tail) when is_binary(bin) do
    from_utf16be(bin, [], tail)
  end

  def from_utf16be(list, tail) do
    from_utf16be(:erlang.list_to_binary(list), [], tail)
  end

  def to_utf16le(list) when is_list(list) do
    :lists.flatmap(&to_utf16le/1, list)
  end

  def to_utf16le(ch) do
    char_to_utf16le(ch)
  end

  def from_utf16le(bin) when is_binary(bin) do
    from_utf16le(bin, [], [])
  end

  def from_utf16le(list) do
    from_utf16le(:erlang.list_to_binary(list), [], [])
  end

  def from_utf16le(bin, tail) when is_binary(bin) do
    from_utf16le(bin, [], tail)
  end

  def from_utf16le(list, tail) do
    from_utf16le(:erlang.list_to_binary(list), [], tail)
  end

  def to_utf8(list) when is_list(list) do
    :lists.flatmap(&to_utf8/1, list)
  end

  def to_utf8(ch) do
    char_to_utf8(ch)
  end

  def from_utf8(bin) when is_binary(bin) do
    from_utf8(:erlang.binary_to_list(bin))
  end

  def from_utf8(list) do
    case expand_utf8(list) do
      {result, 0} ->
        result

      {_Res, _NumBadChar} ->
        exit({:ucs, {:bad_utf8_character_code}})
    end
  end

  def from_latin9(bin) when is_binary(bin) do
    from_latin9(:erlang.binary_to_list(bin))
  end

  def from_latin9(list) do
    for char <- list do
      latin9_to_ucs4(char)
    end
  end

  defp latin9_to_ucs4(164) do
    8364
  end

  defp latin9_to_ucs4(166) do
    352
  end

  defp latin9_to_ucs4(168) do
    353
  end

  defp latin9_to_ucs4(180) do
    381
  end

  defp latin9_to_ucs4(184) do
    382
  end

  defp latin9_to_ucs4(188) do
    338
  end

  defp latin9_to_ucs4(189) do
    339
  end

  defp latin9_to_ucs4(190) do
    376
  end

  defp latin9_to_ucs4(other) do
    other
  end

  defp char_to_ucs4be(ch) do
    true = is_iso10646(ch)
    [ch >>> 24, ch >>> 16 &&& 255, ch >>> 8 &&& 255, ch &&& 255]
  end

  defp from_ucs4be(<<ch::size(32)-big-signed-integer, rest::binary>>, acc, tail) do
    cond do
      ch < 0 or (ch >= 55296 and ch < 57344) or
        ch === 65534 or ch === 65535 ->
        exit({:bad_character_code, ch})

      true ->
        from_ucs4be(rest, [ch | acc], tail)
    end
  end

  defp from_ucs4be(<<>>, acc, tail) do
    :lists.reverse(acc, tail)
  end

  defp from_ucs4be(bin, acc, tail) do
    ucs_error(bin, acc, tail)
    {:error, :not_ucs4be}
  end

  defp char_to_ucs4le(ch) do
    true = is_iso10646(ch)
    [ch &&& 255, ch >>> 8 &&& 255, ch >>> 16 &&& 255, ch >>> 24]
  end

  defp from_ucs4le(<<ch::size(32)-little-signed-integer, rest::binary>>, acc, tail) do
    cond do
      ch < 0 or (ch >= 55296 and ch < 57344) or
        ch === 65534 or ch === 65535 ->
        exit({:bad_character_code, ch})

      true ->
        from_ucs4le(rest, [ch | acc], tail)
    end
  end

  defp from_ucs4le(<<>>, acc, tail) do
    :lists.reverse(acc, tail)
  end

  defp from_ucs4le(bin, acc, tail) do
    ucs_error(bin, acc, tail)
    {:error, :not_ucs4le}
  end

  defp char_to_ucs2be(ch) do
    true = is_iso10646(ch)
    [ch >>> 8 &&& 255, ch &&& 255]
  end

  defp from_ucs2be(<<ch::size(16)-big-signed-integer, rest::binary>>, acc, tail) do
    cond do
      ch < 0 or (ch >= 55296 and ch < 57344) or
        ch === 65534 or ch === 65535 ->
        exit({:bad_character_code, ch})

      true ->
        from_ucs2be(rest, [ch | acc], tail)
    end
  end

  defp from_ucs2be(<<>>, acc, tail) do
    :lists.reverse(acc, tail)
  end

  defp from_ucs2be(bin, acc, tail) do
    ucs_error(bin, acc, tail)
    {:error, :not_ucs2be}
  end

  defp char_to_ucs2le(ch) do
    true = is_iso10646(ch)
    [ch &&& 255, ch >>> 8 &&& 255]
  end

  defp from_ucs2le(<<ch::size(16)-little-signed-integer, rest::binary>>, acc, tail) do
    cond do
      ch < 0 or (ch >= 55296 and ch < 57344) or
        ch === 65534 or ch === 65535 ->
        exit({:bad_character_code, ch})

      true ->
        from_ucs2le(rest, [ch | acc], tail)
    end
  end

  defp from_ucs2le(<<>>, acc, tail) do
    :lists.reverse(acc, tail)
  end

  defp from_ucs2le(bin, acc, tail) do
    ucs_error(bin, acc, tail)
    {:error, :not_ucs2le}
  end

  defp char_to_utf16be(ch) when is_integer(ch) and ch >= 0 do
    cond do
      ch <= 65535 ->
        cond do
          ch < 55296 or (ch >= 57344 and ch < 65534) ->
            [ch >>> 8, ch &&& 255]
        end

      ch < 1_114_112 ->
        x = ch - 65536
        [216 + (x >>> 18), x >>> 10 &&& 255, 220 + (x >>> 8) &&& 3, x &&& 255]

      ch <= 2_147_483_647 ->
        [255, 253]
    end
  end

  defp from_utf16be(<<ch::size(16)-big-unsigned-integer, rest::binary>>, acc, tail)
       when ch < 55296 or ch > 57343 do
    cond do
      ch < 65534 ->
        from_utf16be(rest, [ch | acc], tail)
    end
  end

  defp from_utf16be(
         <<hi::size(16)-big-unsigned-integer, lo::size(16)-big-unsigned-integer, rest::binary>>,
         acc,
         tail
       )
       when hi >= 55296 and hi < 56320 and lo >= 56320 and
              lo <= 57343 do
    ch = hi &&& 1023 <<< (10 + lo) &&& 1023 + 65536
    from_utf16be(rest, [ch | acc], tail)
  end

  defp from_utf16be(<<>>, acc, tail) do
    :lists.reverse(acc, tail)
  end

  defp from_utf16be(bin, acc, tail) do
    ucs_error(bin, acc, tail)
    {:error, :not_utf16be}
  end

  defp char_to_utf16le(ch) when is_integer(ch) and ch >= 0 do
    cond do
      ch <= 65535 ->
        cond do
          ch < 55296 or (ch >= 57344 and ch < 65534) ->
            [ch &&& 255, ch >>> 8]
        end

      ch < 1_114_112 ->
        x = ch - 65536
        [x >>> 10 &&& 255, 216 + (x >>> 18), x &&& 255, 220 + (x >>> 8) &&& 3]

      ch <= 2_147_483_647 ->
        [253, 255]
    end
  end

  defp from_utf16le(<<ch::size(16)-little-unsigned-integer, rest::binary>>, acc, tail)
       when ch < 55296 or ch > 57343 do
    cond do
      ch < 65534 ->
        from_utf16le(rest, [ch | acc], tail)
    end
  end

  defp from_utf16le(
         <<hi::size(16)-little-unsigned-integer, lo::size(16)-little-unsigned-integer,
           rest::binary>>,
         acc,
         tail
       )
       when hi >= 55296 and hi < 56320 and lo >= 56320 and
              lo <= 57343 do
    ch = hi &&& 1023 <<< (10 + lo) &&& 1023 + 65536
    from_utf16le(rest, [ch | acc], tail)
  end

  defp from_utf16le(<<>>, acc, tail) do
    :lists.reverse(acc, tail)
  end

  defp from_utf16le(bin, acc, tail) do
    ucs_error(bin, acc, tail)
    {:error, :not_utf16le}
  end

  defp char_to_utf8(ch) when is_integer(ch) and ch >= 0 do
    cond do
      ch < 128 ->
        [ch]

      ch < 2048 ->
        [192 + (ch >>> 6), 128 + ch &&& 63]

      ch < 65536 ->
        cond do
          ch < 55296 or (ch > 57343 and ch < 65534) ->
            [224 + (ch >>> 12), 128 + (ch >>> 6) &&& 63, 128 + ch &&& 63]
        end

      ch < 2_097_152 ->
        [240 + (ch >>> 18), 128 + (ch >>> 12) &&& 63, 128 + (ch >>> 6) &&& 63, 128 + ch &&& 63]

      ch < 67_108_864 ->
        [
          248 + (ch >>> 24),
          128 + (ch >>> 18) &&& 63,
          128 + (ch >>> 12) &&& 63,
          128 + (ch >>> 6) &&& 63,
          128 + ch &&& 63
        ]

      ch < 2_147_483_648 ->
        [
          252 + (ch >>> 30),
          128 + (ch >>> 24) &&& 63,
          128 + (ch >>> 18) &&& 63,
          128 + (ch >>> 12) &&& 63,
          128 + (ch >>> 6) &&& 63,
          128 + ch &&& 63
        ]
    end
  end

  defp expand_utf8(str) do
    expand_utf8_1(str, [], 0)
  end

  defp expand_utf8_1([c | cs], acc, bad) when c < 128 do
    expand_utf8_1(cs, [c | acc], bad)
  end

  defp expand_utf8_1([[c1, c2] | cs], acc, bad)
       when c1 &&& 224 === 192 and c2 &&& 192 === 128 do
    case (c1 &&& 31 <<< 6) ||| (c2 &&& 63) do
      c when 128 <= c ->
        expand_utf8_1(cs, [c | acc], bad)

      _ ->
        expand_utf8_1(cs, acc, bad + 1)
    end
  end

  defp expand_utf8_1([[c1, c2, c3] | cs], acc, bad)
       when c1 &&& 240 === 224 and c2 &&& 192 === 128 and
              c3 &&& 192 === 128 do
    case (c1 &&& 15 <<< 6) ||| (c2 &&& 63 <<< 6) ||| (c3 &&& 63) do
      c when 2048 <= c ->
        expand_utf8_1(cs, [c | acc], bad)

      _ ->
        expand_utf8_1(cs, acc, bad + 1)
    end
  end

  defp expand_utf8_1([[c1, c2, c3, c4] | cs], acc, bad)
       when c1 &&& 248 === 240 and c2 &&& 192 === 128 and
              c3 &&& 192 === 128 and c4 &&& 192 === 128 do
    case (c1 &&& 15 <<< 6) ||| (c2 &&& 63 <<< 6) ||| (c3 &&& 63 <<< 6) ||| (c4 &&& 63) do
      c when 65536 <= c ->
        expand_utf8_1(cs, [c | acc], bad)

      _ ->
        expand_utf8_1(cs, acc, bad + 1)
    end
  end

  defp expand_utf8_1([_ | cs], acc, bad) do
    expand_utf8_1(cs, acc, bad + 1)
  end

  defp expand_utf8_1([], acc, bad) do
    {:lists.reverse(acc), bad}
  end

  def to_unicode(input, cs)
      when cs == :"ansi_x3.4-1968" or cs == :"iso-ir-6" or
             cs == :"ansi_x3.4-1986" or cs == :"iso_646.irv:1991" or cs == :ascii or
             cs == :"iso646-us" or
             cs == :"us-ascii" or cs == :us or cs == :ibm367 or
             cs == :cp367 or cs == :csascii do
    input
  end

  def to_unicode(input, cs)
      when cs == :"iso-10646-utf-1" or
             cs == :csiso10646utf1 do
    input
  end

  def to_unicode(input, cs)
      when cs == :"iso_646.basic:1983" or cs == :ref or
             cs == :csiso646basic1983 do
    input
  end

  def to_unicode(input, cs)
      when cs == :"iso_8859-1:1987" or cs == :"iso-ir-100" or
             cs == :"iso_8859-1" or cs == :"iso-8859-1" or cs == :latin1 or cs == :l1 or
             cs == :ibm819 or cs == :cp819 or
             cs == :csisolatin1 do
    input
  end

  def to_unicode(input, cs)
      when cs == :"iso_8859-15" or cs == :"iso-8859-15" or
             cs == :latin9 do
    from_latin9(input)
  end

  def to_unicode(input, cs) when cs == :"iso-10646-ucs-2" or cs == :csunicode do
    from_ucs2be(input)
  end

  def to_unicode(input, cs) when cs == :"iso-10646-ucs-4" or cs == :csucs4 do
    from_ucs4be(input)
  end

  def to_unicode(input, cs) when cs == :"utf-16be" or cs == :"utf-16" do
    from_utf16be(input)
  end

  def to_unicode(input, :"utf-16le") do
    from_utf16le(input)
  end

  def to_unicode(input, :"utf-8") do
    from_utf8(input)
  end

  def to_unicode(input, charset) do
    exit({:bad_character_code, input, charset})
  end

  def is_incharset(in__, cs)
      when cs == :"ansi_x3.4-1968" or cs == :"iso-ir-6" or
             cs == :"ansi_x3.4-1986" or cs == :"iso_646.irv:1991" or cs == :ascii or
             cs == :"iso646-us" or
             cs == :"us-ascii" or cs == :us or cs == :ibm367 or
             cs == :cp367 or cs == :csascii do
    cond do
      is_integer(in__) ->
        is_ascii(in__)

      is_list(in__) ->
        test_charset(&is_ascii/1, in__)
    end
  end

  def is_incharset(in__, cs)
      when cs == :"iso-10646-utf-1" or
             cs == :csiso10646utf1 do
    cond do
      is_integer(in__) ->
        is_unicode(in__)

      is_list(in__) ->
        test_charset(&is_unicode/1, in__)
    end
  end

  def is_incharset(in__, cs)
      when cs == :"iso_646.basic:1983" or cs == :ref or
             cs == :csiso646basic1983 do
    cond do
      is_integer(in__) ->
        is_iso646_basic(in__)

      is_list(in__) ->
        test_charset(&is_iso646_basic/1, in__)
    end
  end

  def is_incharset(in__, cs)
      when cs == :"iso_8859-1:1987" or cs == :"iso-ir-100" or
             cs == :"iso_8859-1" or cs == :"iso-8859-1" or cs == :latin1 or cs == :l1 or
             cs == :ibm819 or cs == :cp819 or cs == :csisolatin1 do
    cond do
      is_integer(in__) ->
        is_latin1(in__)

      is_list(in__) ->
        test_charset(&is_latin1/1, in__)
    end
  end

  def is_incharset(in__, charset) when is_integer(in__) do
    case to_unicode([in__], charset) do
      {:error, :unsupported_charset} ->
        {:error, :unsupported_charset}

      {:error, _} ->
        false

      [int] when is_integer(int) ->
        true
    end
  end

  def is_incharset(in__, charset) when is_list(in__) do
    case to_unicode(in__, charset) do
      {:error, :unsupported_charset} ->
        {:error, :unsupported_charset}

      {:error, _} ->
        false

      [int] when is_integer(int) ->
        true
    end
  end

  defp test_charset(fun, input) do
    case :lists.all(fun, input) do
      true ->
        true

      _ ->
        false
    end
  end

  defp ucs_error(bin, acc, tail) do
    :error_logger.error_msg('~w: Bin=~p~n     Acc=~p~n     Tail=~p~n', [
      :xmerl_ucs,
      bin,
      acc,
      tail
    ])
  end
end
