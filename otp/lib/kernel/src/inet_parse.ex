defmodule :m_inet_parse do
  use Bitwise
  import :lists, only: [reverse: 1]
  require Record

  Record.defrecord(:r_file_info, :file_info,
    size: :undefined,
    type: :undefined,
    access: :undefined,
    atime: :undefined,
    mtime: :undefined,
    ctime: :undefined,
    mode: :undefined,
    links: :undefined,
    major_device: :undefined,
    minor_device: :undefined,
    inode: :undefined,
    uid: :undefined,
    gid: :undefined
  )

  Record.defrecord(:r_file_descriptor, :file_descriptor,
    module: :undefined,
    data: :undefined
  )

  def services(file) do
    services(:noname, file)
  end

  def services(fname, file) do
    fn__ = fn [name, portProto | aliases] ->
      {proto, port} = port_proto(portProto, 0)
      {name, proto, port, aliases}
    end

    parse_file(fname, file, fn__)
  end

  def rpc(file) do
    rpc(:noname, file)
  end

  def rpc(fname, file) do
    fn__ = fn [name, program | aliases] ->
      prog = :erlang.list_to_integer(program)
      {name, prog, aliases}
    end

    parse_file(fname, file, fn__)
  end

  def hosts(file) do
    hosts(:noname, file)
  end

  def hosts(fname, file) do
    fn__ = fn [address, name | aliases] ->
      case :string.lexemes(address, '%') do
        [addr, _] ->
          {:ok, _} = address(addr)
          :skip

        _ ->
          {:ok, iP} = address(address)
          {iP, name, aliases}
      end
    end

    parse_file(fname, file, fn__)
  end

  def resolv(file) do
    resolv(:noname, file)
  end

  def resolv(fname, file) do
    fn__ = fn
      ['domain', domain] ->
        {:domain, domain}

      ['nameserver', address] ->
        {:ok, iP} = address(address)
        {:nameserver, iP}

      ['search' | list] ->
        {:search, list}

      ['lookup' | types] ->
        {:lookup, types}

      _ ->
        :skip
    end

    parse_file(fname, file, fn__)
  end

  def host_conf_linux(file) do
    host_conf_linux(:noname, file)
  end

  def host_conf_linux(fname, file) do
    fn__ = fn
      ['order' | order] ->
        {:lookup, split_comma(order)}

      _ ->
        :skip
    end

    parse_file(fname, file, fn__)
  end

  def host_conf_freebsd(file) do
    host_conf_freebsd(:noname, file)
  end

  def host_conf_freebsd(fname, file) do
    fn__ = fn [type] ->
      type
    end

    case parse_file(fname, file, fn__) do
      {:ok, ls} ->
        {:ok, [{:lookup, ls}]}

      error ->
        error
    end
  end

  def host_conf_bsdos(file) do
    host_conf_bsdos(:noname, file)
  end

  def host_conf_bsdos(fname, file) do
    fn__ = fn
      ['hosts' | list] ->
        delete_options(split_comma(list))

      _ ->
        :skip
    end

    case parse_file(fname, file, fn__) do
      {:ok, ls} ->
        {:ok, [{:lookup, :lists.append(ls)}]}

      error ->
        error
    end
  end

  defp delete_options(['continue' | t]) do
    delete_options(t)
  end

  defp delete_options(['merge' | t]) do
    delete_options(t)
  end

  defp delete_options([h | t]) do
    [h | delete_options(t)]
  end

  defp delete_options([]) do
    []
  end

  def nsswitch_conf(file) do
    nsswitch_conf(:noname, file)
  end

  def nsswitch_conf(fname, file) do
    fn__ = fn
      ['hosts:' | types] ->
        {:lookup, types}

      _ ->
        :skip
    end

    parse_file(fname, file, fn__)
  end

  def protocols(file) do
    protocols(:noname, file)
  end

  def protocols(fname, file) do
    fn__ = fn [name, number, dName] ->
      {:erlang.list_to_atom(name), :erlang.list_to_integer(number), dName}
    end

    parse_file(fname, file, fn__)
  end

  def netmasks(file) do
    netmasks(:noname, file)
  end

  def netmasks(fname, file) do
    fn__ = fn [net, subnetmask] ->
      {:ok, netIP} = address(net)
      {:ok, mask} = address(subnetmask)
      {netIP, mask}
    end

    parse_file(fname, file, fn__)
  end

  def networks(file) do
    networks(:noname, file)
  end

  def networks(fname, file) do
    fn__ = fn [netName, netNumber] ->
      number = :erlang.list_to_integer(netNumber)
      {netName, number}
    end

    parse_file(fname, file, fn__)
  end

  defp parse_file(fname, {:fd, fd}, fn__) do
    parse_fd(fname, fd, 1, fn__, [])
  end

  defp parse_file(fname, {:chars, cs}, fn__) when is_list(cs) do
    parse_cs(fname, cs, 1, fn__, [])
  end

  defp parse_file(fname, {:chars, cs}, fn__) when is_binary(cs) do
    parse_cs(fname, :erlang.binary_to_list(cs), 1, fn__, [])
  end

  defp parse_file(_, file, fn__) do
    case :file.open(file, [:read]) do
      {:ok, fd} ->
        result = parse_fd(file, fd, 1, fn__, [])
        _ = :file.close(fd)
        result

      error ->
        error
    end
  end

  defp parse_fd(fname, fd, line, fun, ls) do
    case read_line(fd) do
      :eof ->
        {:ok, reverse(ls)}

      cs ->
        case split_line(cs) do
          [] ->
            parse_fd(fname, fd, line + 1, fun, ls)

          toks ->
            case (try do
                    fun.(toks)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              {:EXIT, _} ->
                :erlang.error('~p:~p: erroneous line, SKIPPED~n', [fname, line])
                parse_fd(fname, fd, line + 1, fun, ls)

              {:warning, wlist, val} ->
                warning('~p:~p: warning! strange domain name(s) ~p ~n', [fname, line, wlist])
                parse_fd(fname, fd, line + 1, fun, [val | ls])

              :skip ->
                parse_fd(fname, fd, line + 1, fun, ls)

              val ->
                parse_fd(fname, fd, line + 1, fun, [val | ls])
            end
        end
    end
  end

  defp parse_cs(fname, chars, line, fun, ls) do
    case get_line(chars) do
      :eof ->
        {:ok, reverse(ls)}

      {cs, chars1} ->
        case split_line(cs) do
          [] ->
            parse_cs(fname, chars1, line + 1, fun, ls)

          toks ->
            case (try do
                    fun.(toks)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              {:EXIT, _} ->
                :erlang.error('~p:~p: erroneous line, SKIPPED~n', [fname, line])
                parse_cs(fname, chars1, line + 1, fun, ls)

              {:warning, wlist, val} ->
                warning('~p:~p: warning! strange domain name(s) ~p ~n', [fname, line, wlist])
                parse_cs(fname, chars1, line + 1, fun, [val | ls])

              :skip ->
                parse_cs(fname, chars1, line + 1, fun, ls)

              val ->
                parse_cs(fname, chars1, line + 1, fun, [val | ls])
            end
        end
    end
  end

  defp get_line([]) do
    :eof
  end

  defp get_line(chars) do
    get_line(chars, [])
  end

  defp get_line([], acc) do
    {reverse(acc), []}
  end

  defp get_line([?\r, ?\n | cs], acc) do
    {reverse([?\n | acc]), cs}
  end

  defp get_line([?\n | cs], acc) do
    {reverse([?\n | acc]), cs}
  end

  defp get_line([c | cs], acc) do
    get_line(cs, [c | acc])
  end

  defp read_line(fd) when is_pid(fd) do
    :io.get_line(fd, :"")
  end

  defp read_line(fd = r_file_descriptor()) do
    collect_line(fd, [])
  end

  defp collect_line(fd, cs) do
    case :file.read(fd, 80) do
      {:ok, line} when is_binary(line) ->
        collect_line(fd, byte_size(line), :erlang.binary_to_list(line), cs)

      {:ok, line} ->
        collect_line(fd, length(line), line, cs)

      :eof when cs === [] ->
        :eof

      :eof ->
        reverse(cs)
    end
  end

  defp collect_line(fd, n, [?\r, ?\n | _], cs) do
    {:ok, _} = :file.position(fd, {:cur, -(n - 2)})
    reverse([?\n | cs])
  end

  defp collect_line(fd, n, [?\n | _], cs) do
    {:ok, _} = :file.position(fd, {:cur, -(n - 1)})
    reverse([?\n | cs])
  end

  defp collect_line(fd, _, [], cs) do
    collect_line(fd, cs)
  end

  defp collect_line(fd, n, [x | xs], cs) do
    collect_line(fd, n - 1, xs, [x | cs])
  end

  defp port_proto([x | xs], n) when x >= ?0 and x <= ?9 do
    port_proto(xs, n * 10 + (x - ?0))
  end

  defp port_proto([?/ | proto], port) when port !== 0 do
    {:erlang.list_to_atom(proto), port}
  end

  def visible_string([h | t]) do
    is_vis1([h | t])
  end

  def visible_string(_) do
    false
  end

  defp is_vis1([c | cs]) when c >= 33 and c <= 126 do
    is_vis1(cs)
  end

  defp is_vis1([]) do
    true
  end

  defp is_vis1(_) do
    false
  end

  def domain([h | t]) do
    is_dom1([h | t])
  end

  def domain(_) do
    false
  end

  defp is_dom1([c | cs]) when c >= ?a and c <= ?z do
    is_dom_ldh(cs)
  end

  defp is_dom1([c | cs]) when c >= ?A and c <= ?Z do
    is_dom_ldh(cs)
  end

  defp is_dom1([c | cs]) when c >= ?0 and c <= ?9 do
    case is_dom_ldh(cs) do
      true ->
        is_dom2(:string.lexemes([c | cs], '.'))

      false ->
        false
    end
  end

  defp is_dom1(_) do
    false
  end

  defp is_dom_ldh([c | cs]) when c >= ?a and c <= ?z do
    is_dom_ldh(cs)
  end

  defp is_dom_ldh([c | cs]) when c >= ?A and c <= ?Z do
    is_dom_ldh(cs)
  end

  defp is_dom_ldh([c | cs]) when c >= ?0 and c <= ?9 do
    is_dom_ldh(cs)
  end

  defp is_dom_ldh([?-, ?. | _]) do
    false
  end

  defp is_dom_ldh([?_, ?. | _]) do
    false
  end

  defp is_dom_ldh([?_ | cs]) do
    is_dom_ldh(cs)
  end

  defp is_dom_ldh([?- | cs]) do
    is_dom_ldh(cs)
  end

  defp is_dom_ldh([?. | cs]) do
    is_dom1(cs)
  end

  defp is_dom_ldh([]) do
    true
  end

  defp is_dom_ldh(_) do
    false
  end

  defp is_dom2([a, b, c, d]) do
    case (try do
            :erlang.list_to_integer(d)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      di when is_integer(di) ->
        case {try do
                :erlang.list_to_integer(a)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end,
              try do
                :erlang.list_to_integer(b)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end,
              try do
                :erlang.list_to_integer(c)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end} do
          {ai, bi, ci}
          when is_integer(ai) and is_integer(bi) and
                 is_integer(ci) ->
            false

          _ ->
            true
        end

      _ ->
        true
    end
  end

  defp is_dom2(_) do
    true
  end

  def address(cs) when is_list(cs) do
    case ipv4_address(cs) do
      {:ok, iP} ->
        {:ok, iP}

      _ ->
        ipv6strict_address(cs)
    end
  end

  def address(_) do
    {:error, :einval}
  end

  def strict_address(cs) when is_list(cs) do
    case ipv4strict_address(cs) do
      {:ok, iP} ->
        {:ok, iP}

      _ ->
        ipv6strict_address(cs)
    end
  end

  def strict_address(_) do
    {:error, :einval}
  end

  def ipv4_address(cs) do
    try do
      ipv4_addr(cs)
    catch
      :error, :badarg ->
        {:error, :einval}
    else
      addr ->
        {:ok, addr}
    end
  end

  defp ipv4_addr(cs) do
    case ipv4_addr(cs, []) do
      [d] when d < 1 <<< 32 ->
        <<d1, d2, d3, d4>> = <<d::size(32)>>
        {d1, d2, d3, d4}

      [d, d1] when d < 1 <<< 24 and d1 < 256 ->
        <<d2, d3, d4>> = <<d::size(24)>>
        {d1, d2, d3, d4}

      [d, d2, d1] when (d < 1 <<< 16 and d2) ||| d1 < 256 ->
        <<d3, d4>> = <<d::size(16)>>
        {d1, d2, d3, d4}

      [d4, d3, d2, d1] when d4 ||| d3 ||| d2 ||| d1 < 256 ->
        {d1, d2, d3, d4}

      _ ->
        :erlang.error(:badarg)
    end
  end

  defp ipv4_addr([_ | _], [_, _, _, _]) do
    :erlang.error(:badarg)
  end

  defp ipv4_addr('0x' ++ cs, ds) do
    ipv4_addr(strip0(cs), ds, [], 16, 8)
  end

  defp ipv4_addr('0X' ++ cs, ds) do
    ipv4_addr(strip0(cs), ds, [], 16, 8)
  end

  defp ipv4_addr('0' ++ cs, ds) do
    ipv4_addr(strip0(cs), ds, [?0], 8, 11)
  end

  defp ipv4_addr(cs, ds) when is_list(cs) do
    ipv4_addr(cs, ds, [], 10, 10)
  end

  defp ipv4_addr(cs0, ds, rs, base, n) do
    case ipv4_field(cs0, n, rs, base) do
      {d, ''} ->
        [d | ds]

      {d, [?. | [_ | _] = cs]} ->
        ipv4_addr(cs, [d | ds])

      {_, _} ->
        :erlang.error(:badarg)
    end
  end

  defp strip0('0' ++ cs) do
    strip0(cs)
  end

  defp strip0(cs) when is_list(cs) do
    cs
  end

  def ipv4strict_address(cs) do
    try do
      ipv4strict_addr(cs)
    catch
      :error, :badarg ->
        {:error, :einval}
    else
      addr ->
        {:ok, addr}
    end
  end

  defp ipv4strict_addr(cs) do
    case ipv4strict_addr(cs, []) do
      [d4, d3, d2, d1] when d4 ||| d3 ||| d2 ||| d1 < 256 ->
        {d1, d2, d3, d4}

      _ ->
        :erlang.error(:badarg)
    end
  end

  defp ipv4strict_addr([_ | _], [_, _, _, _]) do
    :erlang.error(:badarg)
  end

  defp ipv4strict_addr('0', ds) do
    [0 | ds]
  end

  defp ipv4strict_addr('0.' ++ cs, ds) do
    ipv4strict_addr(cs, [0 | ds])
  end

  defp ipv4strict_addr(cs0, ds) when is_list(cs0) do
    case ipv4_field(cs0, 3, [], 10) do
      {d, ''} ->
        [d | ds]

      {d, [?. | [_ | _] = cs]} ->
        ipv4strict_addr(cs, [d | ds])

      {_, _} ->
        :erlang.error(:badarg)
    end
  end

  defp ipv4_field('', _, rs, base) do
    {ipv4_field(rs, base), ''}
  end

  defp ipv4_field('.' ++ _ = cs, _, rs, base) do
    {ipv4_field(rs, base), cs}
  end

  defp ipv4_field('0' ++ _, _, [], _) do
    :erlang.error(:badarg)
  end

  defp ipv4_field([c | cs], n, rs, base) when n > 0 do
    ipv4_field(cs, n - 1, [c | rs], base)
  end

  defp ipv4_field(cs, _, _, _) when is_list(cs) do
    :erlang.error(:badarg)
  end

  defp ipv4_field(rs, base) do
    v = :erlang.list_to_integer(:lists.reverse(rs), base)

    cond do
      v < 0 ->
        :erlang.error(:badarg)

      true ->
        v
    end
  end

  def ipv6_address(cs) do
    case ipv4_address(cs) do
      {:ok, {d1, d2, d3, d4}} ->
        {:ok, {0, 0, 0, 0, 0, 65535, d1 <<< 8 ||| d2, d3 <<< 8 ||| d4}}

      _ ->
        ipv6strict_address(cs)
    end
  end

  def ipv6strict_address(cs) do
    try do
      ipv6_addr(cs)
    catch
      :error, :badarg ->
        {:error, :einval}
    else
      addr ->
        {:ok, addr}
    end
  end

  defp ipv6_addr('::') do
    ipv6_addr_done([], [], 0)
  end

  defp ipv6_addr('::' ++ cs) do
    ipv6_addr(hex(cs), [], [], 0)
  end

  defp ipv6_addr(cs) do
    ipv6_addr(hex(cs), [], 0)
  end

  defp ipv6_addr({cs0, '%' ++ cs1}, a, n) when n == 7 do
    ipv6_addr_scope(cs1, [hex_to_int(cs0) | a], [], n + 1, [])
  end

  defp ipv6_addr({cs0, []}, a, n) when n == 7 do
    ipv6_addr_done([hex_to_int(cs0) | a])
  end

  defp ipv6_addr({cs0, '::%' ++ cs1}, a, n) when n <= 6 do
    ipv6_addr_scope(cs1, [hex_to_int(cs0) | a], [], n + 1, [])
  end

  defp ipv6_addr({cs0, '::'}, a, n) when n <= 6 do
    ipv6_addr_done([hex_to_int(cs0) | a], [], n + 1)
  end

  defp ipv6_addr({cs0, '::' ++ cs1}, a, n) when n <= 5 do
    ipv6_addr(hex(cs1), [hex_to_int(cs0) | a], [], n + 1)
  end

  defp ipv6_addr({cs0, ':' ++ cs1}, a, n) when n <= 6 do
    ipv6_addr(hex(cs1), [hex_to_int(cs0) | a], n + 1)
  end

  defp ipv6_addr({cs0, '.' ++ _ = cs1}, a, n) when n == 6 do
    ipv6_addr_done(a, [], n, ipv4strict_addr(cs0 ++ cs1))
  end

  defp ipv6_addr(_, _, _) do
    :erlang.error(:badarg)
  end

  defp ipv6_addr({cs0, '%' ++ cs1}, a, b, n) when n <= 6 do
    ipv6_addr_scope(cs1, a, [hex_to_int(cs0) | b], n + 1, [])
  end

  defp ipv6_addr({cs0, []}, a, b, n) when n <= 6 do
    ipv6_addr_done(a, [hex_to_int(cs0) | b], n + 1)
  end

  defp ipv6_addr({cs0, ':' ++ cs1}, a, b, n) when n <= 5 do
    ipv6_addr(hex(cs1), a, [hex_to_int(cs0) | b], n + 1)
  end

  defp ipv6_addr({cs0, '.' ++ _ = cs1}, a, b, n) when n <= 5 do
    ipv6_addr_done(a, b, n, ipv4strict_addr(cs0 ++ cs1))
  end

  defp ipv6_addr(_, _, _, _) do
    :erlang.error(:badarg)
  end

  defp ipv6_addr_scope([], ar, br, n, sr) do
    scopeId =
      case :lists.reverse(sr) do
        '' ->
          0

        '0' ++ s ->
          dec16(s)

        _ ->
          0
      end

    ipv6_addr_scope(scopeId, ar, br, n)
  end

  defp ipv6_addr_scope([c | cs], ar, br, n, sr) do
    ipv6_addr_scope(cs, ar, br, n, [c | sr])
  end

  defp ipv6_addr_scope(scopeId, [p], br, n)
       when (n <= 7 and
               p === 65152) or
              (n <= 7 and p === 65282) do
    ipv6_addr_done([scopeId, p], br, n + 1)
  end

  defp ipv6_addr_scope(scopeId, ar, br, n) do
    case :lists.reverse(br ++ dup(8 - n, 0, ar)) do
      [p, 0 | xs] when p === 65152 or p === 65282 ->
        :erlang.list_to_tuple([p, scopeId | xs])

      _ ->
        :erlang.error(:badarg)
    end
  end

  defp ipv6_addr_done(ar, br, n, {d1, d2, d3, d4}) do
    ipv6_addr_done(ar, [d3 <<< 8 ||| d4, d1 <<< 8 ||| d2 | br], n + 2)
  end

  defp ipv6_addr_done(ar, br, n) do
    ipv6_addr_done(br ++ dup(8 - n, 0, ar))
  end

  defp ipv6_addr_done(ar) do
    :erlang.list_to_tuple(:lists.reverse(ar))
  end

  defp hex(cs) do
    hex(cs, [], 4)
  end

  defp hex([c | cs], r, n)
       when c >= ?0 and c <= ?9 and
              n > 0 do
    hex(cs, [c | r], n - 1)
  end

  defp hex([c | cs], r, n)
       when c >= ?a and c <= ?f and
              n > 0 do
    hex(cs, [c | r], n - 1)
  end

  defp hex([c | cs], r, n)
       when c >= ?A and c <= ?F and
              n > 0 do
    hex(cs, [c | r], n - 1)
  end

  defp hex(cs, [_ | _] = r, _) when is_list(cs) do
    {:lists.reverse(r), cs}
  end

  defp hex(_, _, _) do
    :erlang.error(:badarg)
  end

  defp dec16(cs) do
    dec16(cs, 0)
  end

  defp dec16([], i) do
    i
  end

  defp dec16([c | cs], i) when c >= ?0 and c <= ?9 do
    case 10 * i + (c - ?0) do
      j when 65535 < j ->
        :erlang.error(:badarg)

      j ->
        dec16(cs, j)
    end
  end

  defp dec16(_, _) do
    :erlang.error(:badarg)
  end

  defp hex_to_int(cs) do
    :erlang.list_to_integer(cs, 16)
  end

  defp dup(0, _, l) do
    l
  end

  defp dup(n, e, l) when is_integer(n) and n >= 1 do
    dup(n - 1, e, [e | l])
  end

  def ntoa({a, b, c, d}) when a &&& b &&& c &&& d &&& ~~~255 === 0 do
    :erlang.integer_to_list(a) ++
      '.' ++
      :erlang.integer_to_list(b) ++
      '.' ++ :erlang.integer_to_list(c) ++ '.' ++ :erlang.integer_to_list(d)
  end

  def ntoa({0, 0, 0, 0, 0, 0, 0, 0}) do
    '::'
  end

  def ntoa({0, 0, 0, 0, 0, 0, 0, 1}) do
    '::1'
  end

  def ntoa({0, 0, 0, 0, 0, 0, a, b}) when a &&& b &&& ~~~65535 === 0 do
    '::' ++ dig_to_dec(a) ++ '.' ++ dig_to_dec(b)
  end

  def ntoa({0, 0, 0, 0, 0, 65535, a, b})
      when a &&& b &&& ~~~65535 === 0 do
    '::ffff:' ++ dig_to_dec(a) ++ '.' ++ dig_to_dec(b)
  end

  def ntoa({a, b, c, d, e, f, g, h})
      when a &&& b &&& c &&& d &&& e &&& f &&& g &&& h &&& ~~~65535 === 0 do
    cond do
      (a === 65152 and b !== 0) or
          (a === 65282 and b !== 0) ->
        ntoa(
          [a, 0, c, d, e, f, g, h],
          []
        ) ++ '%0' ++ :erlang.integer_to_list(b)

      true ->
        ntoa([a, b, c, d, e, f, g, h], [])
    end
  end

  def ntoa(_) do
    {:error, :einval}
  end

  defp ntoa([], r) do
    ntoa_done(r)
  end

  defp ntoa([0, 0 | t], r) do
    ntoa(t, r, 2)
  end

  defp ntoa([d | t], r) do
    ntoa(t, [d | r])
  end

  defp ntoa([], r, _) do
    ntoa_done(r, [])
  end

  defp ntoa([0 | t], r, n) do
    ntoa(t, r, n + 1)
  end

  defp ntoa([d | t], r, n) do
    ntoa(t, r, n, [d])
  end

  defp ntoa([], r1, _N1, r2) do
    ntoa_done(r1, r2)
  end

  defp ntoa([0, 0 | t], r1, n1, r2) do
    ntoa(t, r1, n1, r2, 2)
  end

  defp ntoa([d | t], r1, n1, r2) do
    ntoa(t, r1, n1, [d | r2])
  end

  defp ntoa(t, r1, n1, r2, n2) when n2 > n1 do
    ntoa(t, r2 ++ dup(n1, 0, r1), n2)
  end

  defp ntoa([], r1, _N1, r2, n2) do
    ntoa_done(r1, dup(n2, 0, r2))
  end

  defp ntoa([0 | t], r1, n1, r2, n2) do
    ntoa(t, r1, n1, r2, n2 + 1)
  end

  defp ntoa([d | t], r1, n1, r2, n2) do
    ntoa(t, r1, n1, [d | dup(n2, 0, r2)])
  end

  defp ntoa_done(r1, r2) do
    :lists.append(
      separate(
        ':',
        :lists.map(
          &dig_to_hex/1,
          :lists.reverse(r1)
        )
      ) ++
        [
          '::'
          | separate(
              ':',
              :lists.map(
                &dig_to_hex/1,
                :lists.reverse(r2)
              )
            )
        ]
    )
  end

  defp ntoa_done(r) do
    :lists.append(
      separate(
        ':',
        :lists.map(&dig_to_hex/1, :lists.reverse(r))
      )
    )
  end

  defp separate(_E, []) do
    []
  end

  defp separate(e, [_ | _] = l) do
    separate(e, l, [])
  end

  defp separate(e, [h | [_ | _] = t], r) do
    separate(e, t, [e, h | r])
  end

  defp separate(_E, [h], r) do
    :lists.reverse(r, [h])
  end

  defp dig_to_dec(0) do
    '0.0'
  end

  defp dig_to_dec(x) do
    :erlang.integer_to_list(x >>> 8 &&& 255) ++ '.' ++ :erlang.integer_to_list(x &&& 255)
  end

  defp dig_to_hex(0) do
    '0'
  end

  defp dig_to_hex(x) when is_integer(x) and 0 < x do
    dig_to_hex(x, '')
  end

  defp dig_to_hex(0, acc) do
    acc
  end

  defp dig_to_hex(x, acc) do
    dig_to_hex(
      x >>> 4,
      [
        case x &&& 15 do
          d when d < 10 ->
            d + ?0

          d ->
            d - 10 + ?a
        end
        | acc
      ]
    )
  end

  def dots(name) do
    dots(name, 0)
  end

  defp dots([?.], n) do
    {n, true}
  end

  defp dots([?. | t], n) do
    dots(t, n + 1)
  end

  defp dots([_C | t], n) do
    dots(t, n)
  end

  defp dots([], n) do
    {n, false}
  end

  def split_line(line) do
    split_line(line, [])
  end

  defp split_line([?# | _], tokens) do
    reverse(tokens)
  end

  defp split_line([?\s | l], tokens) do
    split_line(l, tokens)
  end

  defp split_line([?\t | l], tokens) do
    split_line(l, tokens)
  end

  defp split_line([?\n | l], tokens) do
    split_line(l, tokens)
  end

  defp split_line([], tokens) do
    reverse(tokens)
  end

  defp split_line([c | cs], tokens) do
    split_mid(cs, [c], tokens)
  end

  defp split_mid([?# | _Cs], acc, tokens) do
    split_end(acc, tokens)
  end

  defp split_mid([?\s | cs], acc, tokens) do
    split_line(cs, [reverse(acc) | tokens])
  end

  defp split_mid([?\t | cs], acc, tokens) do
    split_line(cs, [reverse(acc) | tokens])
  end

  defp split_mid([?\r, ?\n | cs], acc, tokens) do
    split_line(cs, [reverse(acc) | tokens])
  end

  defp split_mid([?\n | cs], acc, tokens) do
    split_line(cs, [reverse(acc) | tokens])
  end

  defp split_mid([], acc, tokens) do
    split_end(acc, tokens)
  end

  defp split_mid([c | cs], acc, tokens) do
    split_mid(cs, [c | acc], tokens)
  end

  defp split_end(acc, tokens) do
    reverse([reverse(acc) | tokens])
  end

  defp split_comma([]) do
    []
  end

  defp split_comma([token | tokens]) do
    split_comma(token, []) ++ split_comma(tokens)
  end

  defp split_comma([], tokens) do
    reverse(tokens)
  end

  defp split_comma([?, | l], tokens) do
    split_comma(l, tokens)
  end

  defp split_comma([c | cs], tokens) do
    split_mid_comma(cs, [c], tokens)
  end

  defp split_mid_comma([?, | cs], acc, tokens) do
    split_comma(cs, [reverse(acc) | tokens])
  end

  defp split_mid_comma([], acc, tokens) do
    split_end(acc, tokens)
  end

  defp split_mid_comma([c | cs], acc, tokens) do
    split_mid_comma(cs, [c | acc], tokens)
  end

  defp warning(fmt, args) do
    case :application.get_env(:kernel, :inet_warnings) do
      {:ok, :on} ->
        :error_logger.info_msg('inet_parse:' ++ fmt, args)

      _ ->
        :ok
    end
  end

  defp error(fmt, args) do
    :error_logger.info_msg('inet_parse:' ++ fmt, args)
  end
end
