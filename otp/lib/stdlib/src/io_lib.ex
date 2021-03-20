defmodule :m_io_lib do
  use Bitwise

  def fwrite(format, args) do
    format(format, args)
  end

  def fwrite(format, args, options) do
    format(format, args, options)
  end

  def fread(chars, format) do
    :io_lib_fread.fread(chars, format)
  end

  def fread(cont, chars, format) do
    :io_lib_fread.fread(cont, chars, format)
  end

  def format(format, args) do
    try do
      :io_lib_format.fwrite(format, args)
    catch
      c, r ->
        test_modules_loaded(c, r, __STACKTRACE__)
        :erlang.error(:badarg, [format, args])
    end
  end

  def format(format, args, options) do
    try do
      :io_lib_format.fwrite(format, args, options)
    catch
      c, r ->
        test_modules_loaded(c, r, __STACKTRACE__)
        :erlang.error(:badarg, [format, args])
    end
  end

  def scan_format(format, args) do
    try do
      :io_lib_format.scan(format, args)
    catch
      c, r ->
        test_modules_loaded(c, r, __STACKTRACE__)
        :erlang.error(:badarg, [format, args])
    end
  end

  def unscan_format(formatList) do
    :io_lib_format.unscan(formatList)
  end

  def build_text(formatList) do
    try do
      :io_lib_format.build(formatList)
    catch
      c, r ->
        test_modules_loaded(c, r, __STACKTRACE__)
        :erlang.error(:badarg, [formatList])
    end
  end

  def build_text(formatList, options) do
    try do
      :io_lib_format.build(formatList, options)
    catch
      c, r ->
        test_modules_loaded(c, r, __STACKTRACE__)
        :erlang.error(:badarg, [formatList, options])
    end
  end

  defp test_modules_loaded(_C, _R, _S) do
    modules = [:io_lib_format, :io_lib_pretty, :string, :unicode]

    case :code.ensure_modules_loaded(modules) do
      :ok ->
        :ok

      error ->
        :erlang.error(error)
    end
  end

  def print(term) do
    :io_lib_pretty.print(term)
  end

  def print(term, column, lineLength, depth) do
    :io_lib_pretty.print(term, column, lineLength, depth)
  end

  def indentation(chars, current) do
    :io_lib_format.indentation(chars, current)
  end

  def format_prompt(prompt) do
    format_prompt(prompt, :latin1)
  end

  def format_prompt({:format, format, args}, _Encoding) do
    do_format_prompt(format, args)
  end

  def format_prompt(prompt, encoding)
      when is_list(prompt) or
             is_atom(prompt) or is_binary(prompt) do
    do_format_prompt(add_modifier(encoding, 's'), [prompt])
  end

  def format_prompt(prompt, encoding) do
    do_format_prompt(add_modifier(encoding, 'p'), [prompt])
  end

  defp do_format_prompt(format, args) do
    case (try do
            format(format, args)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        '???'

      list ->
        list
    end
  end

  defp add_modifier(:latin1, c) do
    '~' ++ c
  end

  defp add_modifier(_, c) do
    '~t' ++ c
  end

  def write(term) do
    write1(term, -1, :latin1)
  end

  def write(term, d, true) do
    :io_lib_pretty.print(term, 1, 80, d)
  end

  def write(term, d, false) do
    write(term, d)
  end

  def write(term, options) when is_list(options) do
    depth = get_option(:depth, options, -1)
    encoding = get_option(:encoding, options, :epp.default_encoding())
    charsLimit = get_option(:chars_limit, options, -1)

    cond do
      depth === 0 or charsLimit === 0 ->
        '...'

      charsLimit < 0 ->
        write1(term, depth, encoding)

      charsLimit > 0 ->
        recDefFun = fn _, _ ->
          :no
        end

        if__ =
          :io_lib_pretty.intermediate(term, depth, charsLimit, recDefFun, encoding, _Str = false)

        :io_lib_pretty.write(if__)
    end
  end

  def write(term, depth) do
    write(term, [{:depth, depth}, {:encoding, :latin1}])
  end

  defp write1(_Term, 0, _E) do
    '...'
  end

  defp write1(term, _D, _E) when is_integer(term) do
    :erlang.integer_to_list(term)
  end

  defp write1(term, _D, _E) when is_float(term) do
    :io_lib_format.fwrite_g(term)
  end

  defp write1(atom, _D, :latin1) when is_atom(atom) do
    write_atom_as_latin1(atom)
  end

  defp write1(atom, _D, _E) when is_atom(atom) do
    write_atom(atom)
  end

  defp write1(term, _D, _E) when is_port(term) do
    write_port(term)
  end

  defp write1(term, _D, _E) when is_pid(term) do
    :erlang.pid_to_list(term)
  end

  defp write1(term, _D, _E) when is_reference(term) do
    write_ref(term)
  end

  defp write1(<<_::bitstring>> = term, d, _E) do
    write_binary(term, d)
  end

  defp write1([], _D, _E) do
    '[]'
  end

  defp write1({}, _D, _E) do
    '{}'
  end

  defp write1([h | t], d, e) do
    cond do
      d === 1 ->
        '[...]'

      true ->
        [?[, [write1(h, d - 1, e) | write_tail(t, d - 1, e)], ?]]
    end
  end

  defp write1(f, _D, _E) when is_function(f) do
    :erlang.fun_to_list(f)
  end

  defp write1(term, d, e) when is_map(term) do
    write_map(term, d, e)
  end

  defp write1(t, d, e) when is_tuple(t) do
    cond do
      d === 1 ->
        '{...}'

      true ->
        [
          ?{,
          [
            write1(:erlang.element(1, t), d - 1, e)
            | write_tuple(t, 2, d - 1, e)
          ],
          ?}
        ]
    end
  end

  defp write_tail([], _D, _E) do
    ''
  end

  defp write_tail(_, 1, _E) do
    [?| | '...']
  end

  defp write_tail([h | t], d, e) do
    [?,, write1(h, d - 1, e) | write_tail(t, d - 1, e)]
  end

  defp write_tail(other, d, e) do
    [?|, write1(other, d - 1, e)]
  end

  defp write_tuple(t, i, _D, _E) when i > tuple_size(t) do
    ''
  end

  defp write_tuple(_, _I, 1, _E) do
    [?, | '...']
  end

  defp write_tuple(t, i, d, e) do
    [
      ?,,
      write1(:erlang.element(i, t), d - 1, e)
      | write_tuple(t, i + 1, d - 1, e)
    ]
  end

  defp write_port(port) do
    :erlang.port_to_list(port)
  end

  defp write_ref(ref) do
    :erlang.ref_to_list(ref)
  end

  defp write_map(_, 1, _E) do
    '\#{}'
  end

  defp write_map(map, d, e) when is_integer(d) do
    i = :maps.iterator(map)

    case :maps.next(i) do
      {k, v, nextI} ->
        d0 = d - 1
        w = write_map_assoc(k, v, d0, e)
        [?#, ?{, [w | write_map_body(nextI, d0, d0, e)], ?}]

      :none ->
        '\#{}'
    end
  end

  defp write_map_body(_, 1, _D0, _E) do
    ',...'
  end

  defp write_map_body(i, d, d0, e) do
    case :maps.next(i) do
      {k, v, nextI} ->
        w = write_map_assoc(k, v, d0, e)
        [?,, w | write_map_body(nextI, d - 1, d0, e)]

      :none ->
        ''
    end
  end

  defp write_map_assoc(k, v, d, e) do
    [write1(k, d, e), ' => ', write1(v, d, e)]
  end

  defp write_binary(b, d) when is_integer(d) do
    {s, _} = write_binary(b, d, -1)
    s
  end

  def write_binary(b, d, t) do
    {s, rest} = write_binary_body(b, d, tsub(t, 4), [])
    {[?<, ?<, :lists.reverse(s), ?>, ?>], rest}
  end

  defp write_binary_body(<<>> = b, _D, _T, acc) do
    {acc, b}
  end

  defp write_binary_body(b, d, t, acc) when d === 1 or t === 0 do
    {['...' | acc], b}
  end

  defp write_binary_body(<<x::size(8)>>, _D, _T, acc) do
    {[:erlang.integer_to_list(x) | acc], <<>>}
  end

  defp write_binary_body(<<x::size(8), rest::bitstring>>, d, t, acc) do
    s = :erlang.integer_to_list(x)
    write_binary_body(rest, d - 1, tsub(t, length(s) + 1), [?,, s | acc])
  end

  defp write_binary_body(b, _D, _T, acc) do
    l = bit_size(b)
    <<x::size(l)>> = b
    {[:erlang.integer_to_list(l), ?:, :erlang.integer_to_list(x) | acc], <<>>}
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

  def write_atom(atom) do
    write_possibly_quoted_atom(atom, &write_string/2)
  end

  def write_atom_as_latin1(atom) do
    write_possibly_quoted_atom(
      atom,
      &write_string_as_latin1/2
    )
  end

  defp write_possibly_quoted_atom(atom, pFun) do
    chars = :erlang.atom_to_list(atom)

    case quote_atom(atom, chars) do
      true ->
        pFun.(chars, ?')

      false ->
        chars
    end
  end

  defp name_chars([c | cs]) do
    case name_char(c) do
      true ->
        name_chars(cs)

      false ->
        false
    end
  end

  defp name_chars([]) do
    true
  end

  def write_string(s) do
    write_string(s, ?")
  end

  def write_string(s, q) do
    [q | write_string1(:unicode_as_unicode, s, q)]
  end

  def write_unicode_string(s) do
    write_string(s)
  end

  def write_latin1_string(s) do
    write_latin1_string(s, ?")
  end

  def write_latin1_string(s, q) do
    [q | write_string1(:latin1, s, q)]
  end

  def write_string_as_latin1(s) do
    write_string_as_latin1(s, ?")
  end

  def write_string_as_latin1(s, q) do
    [q | write_string1(:unicode_as_latin1, s, q)]
  end

  defp write_string1(_, [], q) do
    [q]
  end

  defp write_string1(enc, [c | cs], q) do
    string_char(enc, c, q, write_string1(enc, cs, q))
  end

  defp string_char(_, q, q, tail) do
    [?\\, q | tail]
  end

  defp string_char(_, ?\\, _, tail) do
    [?\\, ?\\ | tail]
  end

  defp string_char(_, c, _, tail) when c >= ?\s and c <= ?~ do
    [c | tail]
  end

  defp string_char(:latin1, c, _, tail)
       when c >= 160 and
              c <= 255 do
    [c | tail]
  end

  defp string_char(:unicode_as_unicode, c, _, tail)
       when c >= 160 do
    [c | tail]
  end

  defp string_char(:unicode_as_latin1, c, _, tail)
       when c >= 160 and c <= 255 do
    [c | tail]
  end

  defp string_char(:unicode_as_latin1, c, _, tail) when c >= 255 do
    '\\x{' ++ :erlang.integer_to_list(c, 16) ++ '}' ++ tail
  end

  defp string_char(_, ?\n, _, tail) do
    [?\\, ?n | tail]
  end

  defp string_char(_, ?\r, _, tail) do
    [?\\, ?r | tail]
  end

  defp string_char(_, ?\t, _, tail) do
    [?\\, ?t | tail]
  end

  defp string_char(_, ?\v, _, tail) do
    [?\\, ?v | tail]
  end

  defp string_char(_, ?\b, _, tail) do
    [?\\, ?b | tail]
  end

  defp string_char(_, ?\f, _, tail) do
    [?\\, ?f | tail]
  end

  defp string_char(_, ?\e, _, tail) do
    [?\\, ?e | tail]
  end

  defp string_char(_, ?\d, _, tail) do
    [?\\, ?d | tail]
  end

  defp string_char(_, c, _, tail) when c < 160 do
    c1 = c >>> (6 + ?0)
    c2 = c >>> 3 &&& 7 + ?0
    c3 = c &&& 7 + ?0
    [?\\, c1, c2, c3 | tail]
  end

  def write_char(?\s) do
    '$\\s'
  end

  def write_char(c) when is_integer(c) and c >= ?\0 do
    [?$ | string_char(:unicode_as_unicode, c, -1, [])]
  end

  def write_unicode_char(c) do
    write_char(c)
  end

  def write_latin1_char(lat1)
      when is_integer(lat1) and lat1 >= ?\0 and
             lat1 <= 255 do
    [?$ | string_char(:latin1, lat1, -1, [])]
  end

  def write_char_as_latin1(uni) when is_integer(uni) and uni >= ?\0 do
    [?$ | string_char(:unicode_as_latin1, uni, -1, [])]
  end

  def latin1_char_list([c | cs])
      when is_integer(c) and c >= ?\0 and
             c <= 255 do
    latin1_char_list(cs)
  end

  def latin1_char_list([]) do
    true
  end

  def latin1_char_list(_) do
    false
  end

  def char_list([c | cs])
      when (is_integer(c) and c >= 0 and
              c < 55296) or
             (is_integer(c) and c > 57343 and c < 65534) or
             (is_integer(c) and c > 65535 and c <= 1_114_111) do
    char_list(cs)
  end

  def char_list([]) do
    true
  end

  def char_list(_) do
    false
  end

  def deep_latin1_char_list(cs) do
    deep_latin1_char_list(cs, [])
  end

  defp deep_latin1_char_list([c | cs], more) when is_list(c) do
    deep_latin1_char_list(c, [cs | more])
  end

  defp deep_latin1_char_list([c | cs], more)
       when is_integer(c) and
              c >= ?\0 and c <= 255 do
    deep_latin1_char_list(cs, more)
  end

  defp deep_latin1_char_list([], [cs | more]) do
    deep_latin1_char_list(cs, more)
  end

  defp deep_latin1_char_list([], []) do
    true
  end

  defp deep_latin1_char_list(_, _More) do
    false
  end

  def deep_char_list(cs) do
    deep_char_list(cs, [])
  end

  defp deep_char_list([c | cs], more) when is_list(c) do
    deep_char_list(c, [cs | more])
  end

  defp deep_char_list([c | cs], more)
       when (is_integer(c) and
               c >= 0 and c < 55296) or
              (is_integer(c) and c > 57343 and c < 65534) or
              (is_integer(c) and c > 65535 and
                 c <= 1_114_111) do
    deep_char_list(cs, more)
  end

  defp deep_char_list([], [cs | more]) do
    deep_char_list(cs, more)
  end

  defp deep_char_list([], []) do
    true
  end

  defp deep_char_list(_, _More) do
    false
  end

  def deep_unicode_char_list(term) do
    deep_char_list(term)
  end

  def printable_latin1_list([c | cs])
      when is_integer(c) and c >= ?\s and
             c <= ?~ do
    printable_latin1_list(cs)
  end

  def printable_latin1_list([c | cs])
      when is_integer(c) and c >= 160 and
             c <= 255 do
    printable_latin1_list(cs)
  end

  def printable_latin1_list([?\n | cs]) do
    printable_latin1_list(cs)
  end

  def printable_latin1_list([?\r | cs]) do
    printable_latin1_list(cs)
  end

  def printable_latin1_list([?\t | cs]) do
    printable_latin1_list(cs)
  end

  def printable_latin1_list([?\v | cs]) do
    printable_latin1_list(cs)
  end

  def printable_latin1_list([?\b | cs]) do
    printable_latin1_list(cs)
  end

  def printable_latin1_list([?\f | cs]) do
    printable_latin1_list(cs)
  end

  def printable_latin1_list([?\e | cs]) do
    printable_latin1_list(cs)
  end

  def printable_latin1_list([]) do
    true
  end

  def printable_latin1_list(_) do
    false
  end

  def printable_list(l) do
    case :io.printable_range() do
      :latin1 ->
        printable_latin1_list(l)

      :unicode ->
        printable_unicode_list(l)
    end
  end

  def printable_unicode_list([c | cs])
      when is_integer(c) and c >= ?\s and
             c <= ?~ do
    printable_unicode_list(cs)
  end

  def printable_unicode_list([c | cs])
      when (is_integer(c) and c >= 160 and
              c < 55296) or
             (is_integer(c) and c > 57343 and c < 65534) or
             (is_integer(c) and c > 65535 and c <= 1_114_111) do
    printable_unicode_list(cs)
  end

  def printable_unicode_list([?\n | cs]) do
    printable_unicode_list(cs)
  end

  def printable_unicode_list([?\r | cs]) do
    printable_unicode_list(cs)
  end

  def printable_unicode_list([?\t | cs]) do
    printable_unicode_list(cs)
  end

  def printable_unicode_list([?\v | cs]) do
    printable_unicode_list(cs)
  end

  def printable_unicode_list([?\b | cs]) do
    printable_unicode_list(cs)
  end

  def printable_unicode_list([?\f | cs]) do
    printable_unicode_list(cs)
  end

  def printable_unicode_list([?\e | cs]) do
    printable_unicode_list(cs)
  end

  def printable_unicode_list([]) do
    true
  end

  def printable_unicode_list(_) do
    false
  end

  def nl() do
    '\n'
  end

  defp count_and_find_utf8(bin, n) do
    cafu(bin, n, 0, 0, :none)
  end

  defp cafu(<<>>, _N, count, _ByteCount, savePos) do
    {count, savePos}
  end

  defp cafu(<<_::utf8, rest::binary>>, 0, count, byteCount, _SavePos) do
    cafu(rest, -1, count + 1, 0, byteCount)
  end

  defp cafu(<<_::utf8, rest::binary>>, n, count, _ByteCount, savePos)
       when n < 0 do
    cafu(rest, -1, count + 1, 0, savePos)
  end

  defp cafu(<<_::utf8, rest::binary>> = whole, n, count, byteCount, savePos) do
    delta = byte_size(whole) - byte_size(rest)
    cafu(rest, n - 1, count + 1, byteCount + delta, savePos)
  end

  defp cafu(_Other, _N, count, _ByteCount, savePos) do
    {count, savePos}
  end

  def collect_chars(tag, data, n) do
    collect_chars(tag, data, :latin1, n)
  end

  def collect_chars(:start, data, :unicode, n)
      when is_binary(data) do
    {size, npos} = count_and_find_utf8(data, n)

    cond do
      size > n ->
        {b1, b2} = :erlang.split_binary(data, npos)
        {:stop, b1, b2}

      size < n ->
        {:binary, [data], n - size}

      true ->
        {:stop, data, :eof}
    end
  end

  def collect_chars(:start, data, :latin1, n) when is_binary(data) do
    size = byte_size(data)

    cond do
      size > n ->
        {b1, b2} = :erlang.split_binary(data, n)
        {:stop, b1, b2}

      size < n ->
        {:binary, [data], n - size}

      true ->
        {:stop, data, :eof}
    end
  end

  def collect_chars(:start, data, _, n) when is_list(data) do
    collect_chars_list([], n, data)
  end

  def collect_chars(:start, :eof, _, _) do
    {:stop, :eof, :eof}
  end

  def collect_chars({:binary, stack, _N}, :eof, _, _) do
    {:stop, binrev(stack), :eof}
  end

  def collect_chars({:binary, stack, n}, data, :unicode, _) do
    {size, npos} = count_and_find_utf8(data, n)

    cond do
      size > n ->
        {b1, b2} = :erlang.split_binary(data, npos)
        {:stop, binrev(stack, [b1]), b2}

      size < n ->
        {:binary, [data | stack], n - size}

      true ->
        {:stop, binrev(stack, [data]), :eof}
    end
  end

  def collect_chars({:binary, stack, n}, data, :latin1, _) do
    size = byte_size(data)

    cond do
      size > n ->
        {b1, b2} = :erlang.split_binary(data, n)
        {:stop, binrev(stack, [b1]), b2}

      size < n ->
        {:binary, [data | stack], n - size}

      true ->
        {:stop, binrev(stack, [data]), :eof}
    end
  end

  def collect_chars({:list, stack, n}, data, _, _) do
    collect_chars_list(stack, n, data)
  end

  def collect_chars([], chars, _, n) do
    collect_chars1(n, chars, [])
  end

  def collect_chars({left, sofar}, chars, _, _N) do
    collect_chars1(left, chars, sofar)
  end

  defp collect_chars1(n, chars, stack) when n <= 0 do
    {:done, :lists.reverse(stack, []), chars}
  end

  defp collect_chars1(n, [c | rest], stack) do
    collect_chars1(n - 1, rest, [c | stack])
  end

  defp collect_chars1(_N, :eof, []) do
    {:done, :eof, []}
  end

  defp collect_chars1(_N, :eof, stack) do
    {:done, :lists.reverse(stack, []), []}
  end

  defp collect_chars1(n, [], stack) do
    {:more, {n, stack}}
  end

  defp collect_chars_list(stack, 0, data) do
    {:stop, :lists.reverse(stack, []), data}
  end

  defp collect_chars_list(stack, _N, :eof) do
    {:stop, :lists.reverse(stack, []), :eof}
  end

  defp collect_chars_list(stack, n, []) do
    {:list, stack, n}
  end

  defp collect_chars_list(stack, n, [h | t]) do
    collect_chars_list([h | stack], n - 1, t)
  end

  def collect_line(tag, data, any) do
    collect_line(tag, data, :latin1, any)
  end

  def collect_line(:start, data, encoding, _)
      when is_binary(data) do
    collect_line_bin(data, data, [], encoding)
  end

  def collect_line(:start, data, _, _) when is_list(data) do
    collect_line_list(data, [])
  end

  def collect_line(:start, :eof, _, _) do
    {:stop, :eof, :eof}
  end

  def collect_line(stack, data, encoding, _) when is_binary(data) do
    collect_line_bin(data, data, stack, encoding)
  end

  def collect_line(stack, data, _, _) when is_list(data) do
    collect_line_list(data, stack)
  end

  def collect_line([b | _] = stack, :eof, _, _) when is_binary(b) do
    {:stop, binrev(stack), :eof}
  end

  def collect_line(stack, :eof, _, _) do
    {:stop, :lists.reverse(stack, []), :eof}
  end

  defp collect_line_bin(<<?\n, t::binary>>, data, stack0, _) do
    n = byte_size(data) - byte_size(t)
    <<line::size(n)-binary, _::binary>> = data

    case stack0 do
      [] ->
        {:stop, line, t}

      [<<?\r>> | stack] when n === 1 ->
        {:stop, binrev(stack, [?\n]), t}

      _ ->
        {:stop, binrev(stack0, [line]), t}
    end
  end

  defp collect_line_bin(<<?\r, ?\n, t::binary>>, data, stack, _) do
    n = byte_size(data) - byte_size(t) - 2
    <<line::size(n)-binary, _::binary>> = data
    {:stop, binrev(stack, [line, ?\n]), t}
  end

  defp collect_line_bin(<<?\r>>, data0, stack, _) do
    n = byte_size(data0) - 1
    <<data::size(n)-binary, _::binary>> = data0
    [<<?\r>>, data | stack]
  end

  defp collect_line_bin(<<_, t::binary>>, data, stack, enc) do
    collect_line_bin(t, data, stack, enc)
  end

  defp collect_line_bin(<<>>, data, stack, _) do
    [data | stack]
  end

  defp collect_line_list([?\n | t], [?\r | stack]) do
    {:stop, :lists.reverse(stack, [?\n]), t}
  end

  defp collect_line_list([?\n | t], stack) do
    {:stop, :lists.reverse(stack, [?\n]), t}
  end

  defp collect_line_list([h | t], stack) do
    collect_line_list(t, [h | stack])
  end

  defp collect_line_list([], stack) do
    stack
  end

  def get_until(any, data, arg) do
    get_until(any, data, :latin1, arg)
  end

  def get_until(:start, data, encoding, xtraArg) do
    get_until([], data, encoding, xtraArg)
  end

  def get_until(cont, data, encoding, {mod, func, xtraArgs}) do
    chars =
      cond do
        is_binary(data) and encoding === :unicode ->
          :unicode.characters_to_list(data, :utf8)

        is_binary(data) ->
          :erlang.binary_to_list(data)

        true ->
          data
      end

    case apply(mod, func, [cont, chars | xtraArgs]) do
      {:done, result, buf} ->
        {:stop,
         cond do
           is_binary(data) and is_list(result) and
               encoding === :unicode ->
             :unicode.characters_to_binary(result, :unicode, :unicode)

           is_binary(data) and is_list(result) ->
             :erlang.iolist_to_binary(result)

           true ->
             result
         end, buf}

      {:more, newCont} ->
        newCont
    end
  end

  defp binrev(l) do
    :erlang.list_to_binary(:lists.reverse(l, []))
  end

  defp binrev(l, t) do
    :erlang.list_to_binary(:lists.reverse(l, t))
  end

  def limit_term(term, depth) do
    try do
      test_limit(term, depth)
    catch
      :limit ->
        limit(term, depth)
    else
      :ok ->
        term
    end
  end

  defp limit(_, 0) do
    :...
  end

  defp limit([h | t] = l, d) do
    cond do
      d === 1 ->
        [:...]

      true ->
        case printable_list(l) do
          true ->
            l

          false ->
            [limit(h, d - 1) | limit_tail(t, d - 1)]
        end
    end
  end

  defp limit(term, d) when is_map(term) do
    limit_map(term, d)
  end

  defp limit({} = t, _D) do
    t
  end

  defp limit(t, d) when is_tuple(t) do
    cond do
      d === 1 ->
        {:...}

      true ->
        :erlang.list_to_tuple([
          limit(
            :erlang.element(1, t),
            d - 1
          )
          | limit_tuple(t, 2, d - 1)
        ])
    end
  end

  defp limit(<<_::bitstring>> = term, d) do
    limit_bitstring(term, d)
  end

  defp limit(term, _D) do
    term
  end

  defp limit_tail([], _D) do
    []
  end

  defp limit_tail(_, 1) do
    [:...]
  end

  defp limit_tail([h | t], d) do
    [limit(h, d - 1) | limit_tail(t, d - 1)]
  end

  defp limit_tail(other, d) do
    limit(other, d - 1)
  end

  defp limit_tuple(t, i, _D) when i > tuple_size(t) do
    []
  end

  defp limit_tuple(_, _I, 1) do
    [:...]
  end

  defp limit_tuple(t, i, d) do
    [limit(:erlang.element(i, t), d - 1) | limit_tuple(t, i + 1, d - 1)]
  end

  defp limit_map(map, d) do
    limit_map_body(:maps.iterator(map), d + 1, d, [])
  end

  defp limit_map_body(_I, 0, _D0, acc) do
    :maps.from_list(acc)
  end

  defp limit_map_body(i, d, d0, acc) do
    case :maps.next(i) do
      {k, v, nextI} ->
        limit_map_body(nextI, d - 1, d0, [limit_map_assoc(k, v, d0) | acc])

      :none ->
        :maps.from_list(acc)
    end
  end

  defp limit_map_assoc(k, v, d) do
    {k, limit(v, d - 1)}
  end

  defp limit_bitstring(b, _D) do
    b
  end

  defp test_limit(_, 0) do
    throw(:limit)
  end

  defp test_limit([h | t] = l, d) when is_integer(d) do
    cond do
      d === 1 ->
        throw(:limit)

      true ->
        case printable_list(l) do
          true ->
            :ok

          false ->
            test_limit(h, d - 1)
            test_limit_tail(t, d - 1)
        end
    end
  end

  defp test_limit(term, d) when is_map(term) do
    test_limit_map(term, d)
  end

  defp test_limit({}, _D) do
    :ok
  end

  defp test_limit(t, d) when is_tuple(t) do
    test_limit_tuple(t, 1, tuple_size(t), d)
  end

  defp test_limit(<<_::bitstring>> = term, d) do
    test_limit_bitstring(term, d)
  end

  defp test_limit(_Term, _D) do
    :ok
  end

  defp test_limit_tail([], _D) do
    :ok
  end

  defp test_limit_tail(_, 1) do
    throw(:limit)
  end

  defp test_limit_tail([h | t], d) do
    test_limit(h, d - 1)
    test_limit_tail(t, d - 1)
  end

  defp test_limit_tail(other, d) do
    test_limit(other, d - 1)
  end

  defp test_limit_tuple(_T, i, sz, _D) when i > sz do
    :ok
  end

  defp test_limit_tuple(_, _, _, 1) do
    throw(:limit)
  end

  defp test_limit_tuple(t, i, sz, d) do
    test_limit(:erlang.element(i, t), d - 1)
    test_limit_tuple(t, i + 1, sz, d - 1)
  end

  defp test_limit_map(map, d) do
    test_limit_map_body(:maps.iterator(map), d)
  end

  defp test_limit_map_body(_I, 0) do
    throw(:limit)
  end

  defp test_limit_map_body(i, d) do
    case :maps.next(i) do
      {k, v, nextI} ->
        test_limit_map_assoc(k, v, d)
        test_limit_map_body(nextI, d - 1)

      :none ->
        :ok
    end
  end

  defp test_limit_map_assoc(k, v, d) do
    test_limit(k, d - 1)
    test_limit(v, d - 1)
  end

  defp test_limit_bitstring(_, _) do
    :ok
  end

  def chars_length(s) do
    try do
      :erlang.iolist_size(s)
    catch
      _, _ ->
        :string.length(s)
    end
  end
end
