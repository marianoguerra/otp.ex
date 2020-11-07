defmodule :m_asn1ct_tok do
  use Bitwise

  def file(file0) do
    case :file.open(file0, [:read]) do
      {:error, reason} ->
        {:error, {file0, :file.format_error(reason)}}

      {:ok, stream} ->
        try do
          process(stream, 1, [])
        catch
          {:error, line, reason} ->
            file = :filename.basename(file0)
            error = {:structured_error, {file, line}, :asn1ct_tok, reason}
            {:error, [error]}
        end
    end
  end

  defp process(stream, lno, r) do
    process(:io.get_line(stream, :""), stream, lno, r)
  end

  defp process(:eof, stream, lno, acc) do
    :ok = :file.close(stream)
    :lists.reverse([{:"$end", lno} | acc])
  end

  defp process(l, stream, lno0, acc) when is_list(l) do
    try do
      tokenise(stream, l, lno0, [])
    catch
      {:error, reason} ->
        throw({:error, lno0, reason})
    else
      {lno, []} ->
        process(stream, lno, acc)

      {lno, ts} ->
        process(stream, lno, ts ++ acc)
    end
  end

  def format_error(:eof_in_comment) do
    'premature end of file in multi-line comment'
  end

  def format_error(:eol_in_token) do
    'end of line in token'
  end

  def format_error({:invalid_binary_number, str}) do
    :io_lib.format('invalid binary number: \'~s\'', [str])
  end

  def format_error({:invalid_hex_number, str}) do
    :io_lib.format('invalid hex number: \'~s\'', [str])
  end

  def format_error(other) do
    :io_lib.format('~p', [other])
  end

  defp tokenise(stream, [[?&, h] | t], lno, r)
       when ?A <= h and
              h <= ?Z do
    {x, t1} = get_name(t, [h])
    tokenise(stream, t1, lno, [{:typefieldreference, lno, x} | r])
  end

  defp tokenise(stream, [[?&, h] | t], lno, r)
       when ?a <= h and
              h <= ?z do
    {x, t1} = get_name(t, [h])
    tokenise(stream, t1, lno, [{:valuefieldreference, lno, x} | r])
  end

  defp tokenise(stream, '--' ++ t, lno, r) do
    tokenise(stream, skip_comment(t), lno, r)
  end

  defp tokenise(stream, [[?-, h] | t], lno, r)
       when ?0 <= h and
              h <= ?9 do
    {x, t1} = get_number(t, [h])
    tokenise(stream, t1, lno, [{:number, lno, -:erlang.list_to_integer(x)} | r])
  end

  defp tokenise(stream, '/*' ++ t, lno0, r) do
    {lno, t1} = skip_multiline_comment(stream, t, lno0, 0)
    tokenise(stream, t1, lno, r)
  end

  defp tokenise(stream, '::=' ++ t, lno, r) do
    tokenise(stream, t, lno, [{:"::=", lno} | r])
  end

  defp tokenise(stream, ':' ++ t, lno, r) do
    tokenise(stream, t, lno, [{:":", lno} | r])
  end

  defp tokenise(stream, '\'' ++ t0, lno, r) do
    {thing, t1} = collect_quoted(t0, lno, [])
    tokenise(stream, t1, lno, [thing | r])
  end

  defp tokenise(stream, [?" | t], lno, r) do
    {str, t1} = collect_string(t, lno)
    tokenise(stream, t1, lno, [str | r])
  end

  defp tokenise(stream, '{' ++ t, lno, r) do
    tokenise(stream, t, lno, [{:"{", lno} | r])
  end

  defp tokenise(stream, '}' ++ t, lno, r) do
    tokenise(stream, t, lno, [{:"}", lno} | r])
  end

  defp tokenise(stream, ']' ++ t, lno, r) do
    tokenise(stream, t, lno, [{:"]", lno} | r])
  end

  defp tokenise(stream, '[' ++ t, lno, r) do
    tokenise(stream, t, lno, [{:"[", lno} | r])
  end

  defp tokenise(stream, ',' ++ t, lno, r) do
    tokenise(stream, t, lno, [{:",", lno} | r])
  end

  defp tokenise(stream, '(' ++ t, lno, r) do
    tokenise(stream, t, lno, [{:"(", lno} | r])
  end

  defp tokenise(stream, ')' ++ t, lno, r) do
    tokenise(stream, t, lno, [{:")", lno} | r])
  end

  defp tokenise(stream, '...' ++ t, lno, r) do
    tokenise(stream, t, lno, [{:..., lno} | r])
  end

  defp tokenise(stream, '..' ++ t, lno, r) do
    tokenise(stream, t, lno, [{:.., lno} | r])
  end

  defp tokenise(stream, '.' ++ t, lno, r) do
    tokenise(stream, t, lno, [{:., lno} | r])
  end

  defp tokenise(stream, '|' ++ t, lno, r) do
    tokenise(stream, t, lno, [{:|, lno} | r])
  end

  defp tokenise(stream, [h | t], lno, r)
       when ?A <= h and
              h <= ?Z do
    {x, t1} = get_name(t, [h])

    case reserved_word(x) do
      true ->
        tokenise(stream, t1, lno, [{x, lno} | r])

      false ->
        tokenise(stream, t1, lno, [{:typereference, lno, x} | r])

      :rstrtype ->
        tokenise(stream, t1, lno, [{:restrictedcharacterstringtype, lno, x} | r])
    end
  end

  defp tokenise(stream, [h | t], lno, r)
       when ?a <= h and
              h <= ?z do
    {x, t1} = get_name(t, [h])
    tokenise(stream, t1, lno, [{:identifier, lno, x} | r])
  end

  defp tokenise(stream, [h | t], lno, r)
       when ?0 <= h and
              h <= ?9 do
    {x, t1} = get_number(t, [h])
    tokenise(stream, t1, lno, [{:number, lno, :erlang.list_to_integer(x)} | r])
  end

  defp tokenise(stream, [h | t], lno, r) when h <= ?\s do
    tokenise(stream, t, lno, r)
  end

  defp tokenise(stream, [h | t], lno, r) do
    tokenise(stream, t, lno, [{:erlang.list_to_atom([h]), lno} | r])
  end

  defp tokenise(_Stream, [], lno, r) do
    {lno + 1, r}
  end

  defp collect_string(l, lno) do
    collect_string(l, lno, [])
  end

  defp collect_string([?" | t], _Lno, str) do
    {{:cstring, 1, :lists.reverse(str)}, t}
  end

  defp collect_string([h | t], lno, str) do
    collect_string(t, lno, [h | str])
  end

  defp collect_string([], _, _) do
    throw({:error, :missing_quote_at_eof})
  end

  defp get_name([[?-, char] | t] = t0, acc) do
    case isalnum(char) do
      true ->
        get_name(t, [[char, ?-] | acc])

      false ->
        {:erlang.list_to_atom(:lists.reverse(acc)), t0}
    end
  end

  defp get_name([?- | _] = t, acc) do
    {:erlang.list_to_atom(:lists.reverse(acc)), t}
  end

  defp get_name([char | t] = t0, acc) do
    case isalnum(char) do
      true ->
        get_name(t, [char | acc])

      false ->
        {:erlang.list_to_atom(:lists.reverse(acc)), t0}
    end
  end

  defp get_name([], acc) do
    {:erlang.list_to_atom(:lists.reverse(acc)), []}
  end

  defp isalnum(h) when ?A <= h and h <= ?Z do
    true
  end

  defp isalnum(h) when ?a <= h and h <= ?z do
    true
  end

  defp isalnum(h) when ?0 <= h and h <= ?9 do
    true
  end

  defp isalnum(_) do
    false
  end

  defp isdigit(h) when ?0 <= h and h <= ?9 do
    true
  end

  defp isdigit(_) do
    false
  end

  defp get_number([h | t] = t0, l) do
    case isdigit(h) do
      true ->
        get_number(t, [h | l])

      false ->
        {:lists.reverse(l), t0}
    end
  end

  defp get_number([], l) do
    {:lists.reverse(l), []}
  end

  defp skip_comment([]) do
    []
  end

  defp skip_comment('--' ++ t) do
    t
  end

  defp skip_comment([_ | t]) do
    skip_comment(t)
  end

  defp skip_multiline_comment(stream, [], lno, level) do
    case :io.get_line(stream, :"") do
      :eof ->
        throw({:error, :eof_in_comment})

      line ->
        skip_multiline_comment(stream, line, lno + 1, level)
    end
  end

  defp skip_multiline_comment(_Stream, '*/' ++ t, lno, 0) do
    {lno, t}
  end

  defp skip_multiline_comment(stream, '*/' ++ t, lno, level) do
    skip_multiline_comment(stream, t, lno, level - 1)
  end

  defp skip_multiline_comment(stream, '/*' ++ t, lno, level) do
    skip_multiline_comment(stream, t, lno, level + 1)
  end

  defp skip_multiline_comment(stream, [_ | t], lno, level) do
    skip_multiline_comment(stream, t, lno, level)
  end

  defp collect_quoted('\'B' ++ t, lno, l) do
    case validate_bin(l) do
      {:ok, bin} ->
        {{:bstring, lno, bin}, t}

      false ->
        throw({:error, {:invalid_binary_number, :lists.reverse(l)}})
    end
  end

  defp collect_quoted('\'H' ++ t, lno, l) do
    case validate_hex(l) do
      {:ok, hex} ->
        {{:hstring, lno, hex}, t}

      false ->
        throw({:error, {:invalid_hex_number, :lists.reverse(l)}})
    end
  end

  defp collect_quoted([h | t], lno, l) do
    collect_quoted(t, lno, [h | l])
  end

  defp collect_quoted([], _, _) do
    throw({:error, :eol_in_token})
  end

  defp validate_bin(l) do
    validate_bin(l, [])
  end

  defp validate_bin([h | t], a) when h === ?0 or h === ?1 do
    validate_bin(t, [h | a])
  end

  defp validate_bin([?\s | t], a) do
    validate_bin(t, a)
  end

  defp validate_bin([_ | _], _) do
    false
  end

  defp validate_bin([], a) do
    {:ok, a}
  end

  defp validate_hex(l) do
    validate_hex(l, [])
  end

  defp validate_hex([h | t], a) when ?0 <= h and h <= ?9 do
    validate_hex(t, [h | a])
  end

  defp validate_hex([h | t], a) when ?A <= h and h <= ?F do
    validate_hex(t, [h | a])
  end

  defp validate_hex([?\s | t], a) do
    validate_hex(t, a)
  end

  defp validate_hex([_ | _], _) do
    false
  end

  defp validate_hex([], a) do
    {:ok, a}
  end

  defp reserved_word(:ABSENT) do
    true
  end

  defp reserved_word(:ALL) do
    true
  end

  defp reserved_word(:ANY) do
    true
  end

  defp reserved_word(:APPLICATION) do
    true
  end

  defp reserved_word(:AUTOMATIC) do
    true
  end

  defp reserved_word(:BEGIN) do
    true
  end

  defp reserved_word(:BIT) do
    true
  end

  defp reserved_word(:BMPString) do
    :rstrtype
  end

  defp reserved_word(:BOOLEAN) do
    true
  end

  defp reserved_word(:BY) do
    true
  end

  defp reserved_word(:CHARACTER) do
    true
  end

  defp reserved_word(:CHOICE) do
    true
  end

  defp reserved_word(:CLASS) do
    true
  end

  defp reserved_word(:COMPONENT) do
    true
  end

  defp reserved_word(:COMPONENTS) do
    true
  end

  defp reserved_word(:CONSTRAINED) do
    true
  end

  defp reserved_word(:CONTAINING) do
    true
  end

  defp reserved_word(:DEFAULT) do
    true
  end

  defp reserved_word(:DEFINED) do
    true
  end

  defp reserved_word(:DEFINITIONS) do
    true
  end

  defp reserved_word(:EMBEDDED) do
    true
  end

  defp reserved_word(:ENCODED) do
    true
  end

  defp reserved_word(:END) do
    true
  end

  defp reserved_word(:ENUMERATED) do
    true
  end

  defp reserved_word(:EXCEPT) do
    true
  end

  defp reserved_word(:EXPLICIT) do
    true
  end

  defp reserved_word(:EXPORTS) do
    true
  end

  defp reserved_word(:EXTENSIBILITY) do
    true
  end

  defp reserved_word(:EXTERNAL) do
    true
  end

  defp reserved_word(:FALSE) do
    true
  end

  defp reserved_word(:FROM) do
    true
  end

  defp reserved_word(:GeneralizedTime) do
    true
  end

  defp reserved_word(:GeneralString) do
    :rstrtype
  end

  defp reserved_word(:GraphicString) do
    :rstrtype
  end

  defp reserved_word(:IA5String) do
    :rstrtype
  end

  defp reserved_word(:IDENTIFIER) do
    true
  end

  defp reserved_word(:IMPLICIT) do
    true
  end

  defp reserved_word(:IMPLIED) do
    true
  end

  defp reserved_word(:IMPORTS) do
    true
  end

  defp reserved_word(:INCLUDES) do
    true
  end

  defp reserved_word(:INSTANCE) do
    true
  end

  defp reserved_word(:INTEGER) do
    true
  end

  defp reserved_word(:INTERSECTION) do
    true
  end

  defp reserved_word(:MAX) do
    true
  end

  defp reserved_word(:MIN) do
    true
  end

  defp reserved_word(:"MINUS-INFINITY") do
    true
  end

  defp reserved_word(:NULL) do
    true
  end

  defp reserved_word(:NumericString) do
    :rstrtype
  end

  defp reserved_word(:OBJECT) do
    true
  end

  defp reserved_word(:ObjectDescriptor) do
    true
  end

  defp reserved_word(:OCTET) do
    true
  end

  defp reserved_word(:OF) do
    true
  end

  defp reserved_word(:OPTIONAL) do
    true
  end

  defp reserved_word(:PATTERN) do
    true
  end

  defp reserved_word(:PDV) do
    true
  end

  defp reserved_word(:"PLUS-INFINITY") do
    true
  end

  defp reserved_word(:PRESENT) do
    true
  end

  defp reserved_word(:PrintableString) do
    :rstrtype
  end

  defp reserved_word(:PRIVATE) do
    true
  end

  defp reserved_word(:REAL) do
    true
  end

  defp reserved_word(:"RELATIVE-OID") do
    true
  end

  defp reserved_word(:SEQUENCE) do
    true
  end

  defp reserved_word(:SET) do
    true
  end

  defp reserved_word(:SIZE) do
    true
  end

  defp reserved_word(:STRING) do
    true
  end

  defp reserved_word(:SYNTAX) do
    true
  end

  defp reserved_word(:T61String) do
    :rstrtype
  end

  defp reserved_word(:TAGS) do
    true
  end

  defp reserved_word(:TeletexString) do
    :rstrtype
  end

  defp reserved_word(:TRUE) do
    true
  end

  defp reserved_word(:UNION) do
    true
  end

  defp reserved_word(:UNIQUE) do
    true
  end

  defp reserved_word(:UNIVERSAL) do
    true
  end

  defp reserved_word(:UniversalString) do
    :rstrtype
  end

  defp reserved_word(:UTCTime) do
    true
  end

  defp reserved_word(:UTF8String) do
    :rstrtype
  end

  defp reserved_word(:VideotexString) do
    :rstrtype
  end

  defp reserved_word(:VisibleString) do
    :rstrtype
  end

  defp reserved_word(:WITH) do
    true
  end

  defp reserved_word(_) do
    false
  end
end
