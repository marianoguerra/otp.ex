defmodule :m_xmerl_uri do
  use Bitwise

  def resolve(_Root, _Rel) do
    :ok
  end

  def parse(uRI) do
    case parse_scheme(uRI) do
      {:http, cont} ->
        parse_http(cont, :http)

      {:https, cont} ->
        parse_http(cont, :https)

      {:ftp, cont} ->
        parse_ftp(cont, :ftp)

      {:sip, cont} ->
        parse_sip(cont, :sip)

      {:sips, cont} ->
        parse_sip(cont, :sips)

      {:sms, cont} ->
        parse_sms(cont, :sms)

      {:error, error} ->
        {:error, error}

      {scheme, cont} ->
        {scheme, cont}
    end
  end

  defp parse_scheme(uRI) do
    parse_scheme(uRI, [])
  end

  defp parse_scheme([h | uRI], acc)
       when (?a <= h and h <= ?z) or
              (?A <= h and h <= ?Z) do
    parse_scheme2(uRI, [h | acc])
  end

  defp parse_scheme(_, _) do
    {:error, :no_scheme}
  end

  defp parse_scheme2([h | uRI], acc)
       when (?a <= h and h <= ?z) or
              (?A <= h and h <= ?Z) or
              (?0 <= h and h <= ?9) or h == ?- or h == ?+ or
              h == ?. do
    parse_scheme2(uRI, [h | acc])
  end

  defp parse_scheme2([?: | uRI], acc) do
    {:erlang.list_to_atom(:lists.reverse(acc)), uRI}
  end

  defp parse_scheme2(_, _) do
    {:error, :no_scheme}
  end

  defp parse_http('//' ++ c0, scheme) do
    case scan_hostport(c0, scheme) do
      {c1, host, port} ->
        case scan_pathquery(c1) do
          {:error, error} ->
            {:error, error}

          {path, query} ->
            {scheme, host, port, path, query}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp parse_http(_, _) do
    {:error, :invalid_url}
  end

  defp scan_pathquery(c0) do
    case scan_abspath(c0) do
      {:error, error} ->
        {:error, error}

      {[], []} ->
        {'/', ''}

      {'?' ++ c1, path} ->
        case scan_query(c1, []) do
          {:error, error} ->
            {:error, error}

          query ->
            {path, '?' ++ query}
        end

      {'#' ++ c1, path} ->
        case scan_query(c1, []) do
          {:error, error} ->
            {:error, error}

          fragment ->
            {path, '#' ++ fragment}
        end

      {[], path} ->
        {path, ''}
    end
  end

  defp parse_ftp('//' ++ c0, scheme) do
    case ftp_userinfo(c0) do
      {:error, error} ->
        {:error, error}

      {c1, creds} ->
        case scan_hostport(c1, scheme) do
          {c2, host, port} ->
            case scan_abspath(c2) do
              {:error, error} ->
                {:error, error}

              {[], []} ->
                {scheme, creds, host, port, '/'}

              {[], path} ->
                {scheme, creds, host, port, path}
            end

          {:error, error} ->
            {:error, error}
        end
    end
  end

  defp ftp_userinfo(c0) do
    ftp_userinfo(c0, [])
  end

  defp ftp_userinfo([], acc) do
    {:lists.reverse(acc), {'', ''}}
  end

  defp ftp_userinfo(c0 = [?/ | _], acc) do
    {:lists.reverse(acc) ++ c0, {'', ''}}
  end

  defp ftp_userinfo([?@ | c0], acc) do
    {c0, ftp_userinfo_1(:lists.reverse(acc), 0, '', '')}
  end

  defp ftp_userinfo([c | c0], acc) do
    ftp_userinfo(c0, [c | acc])
  end

  defp ftp_userinfo_1([], 0, acc, []) do
    {:lists.reverse(acc), ''}
  end

  defp ftp_userinfo_1([], 1, acc, user) do
    {user, :lists.reverse(acc)}
  end

  defp ftp_userinfo_1([?: | _], 0, [], []) do
    {:error, :no_user}
  end

  defp ftp_userinfo_1([?: | c0], 0, acc, []) do
    ftp_userinfo_1(c0, 1, [], :lists.reverse(acc))
  end

  defp ftp_userinfo_1([c | c0], stage, acc, user) do
    ftp_userinfo_1(c0, stage, [c | acc], user)
  end

  defp parse_sip(c0, scheme) do
    case :string.tokens(c0, '@') do
      [userinfo, hostport] ->
        {user, pass} = sip_userinfo(userinfo)
        {c1, host, port} = scan_hostport(hostport, scheme)
        {c2, parameters} = scan_parameters(c1)
        headers = scan_headers(c2)
        {scheme, user, pass, host, port, parameters, headers}

      [hostport] ->
        {c1, host, port} = scan_hostport(hostport, scheme)
        {c2, parameters} = scan_parameters(c1)
        headers = scan_headers(c2)
        {scheme, :none, :none, host, port, parameters, headers}
    end
  end

  defp sip_userinfo(userinfo) do
    case :string.tokens(userinfo, ':') do
      [user, pass] ->
        {user, pass}

      [user] ->
        {user, :none}
    end
  end

  defp scan_parameters(c1) do
    parList = :string.tokens(c1, ';')
    scan_parameters2(parList, [], [])
  end

  defp scan_parameters2([], out, foo) do
    {:lists.reverse(foo), :lists.reverse(out)}
  end

  defp scan_parameters2(['transport' ++ val | rest], out, foo) do
    scan_parameters2(rest, [{:transport, val} | out], foo)
  end

  defp scan_parameters2(['user' ++ val | rest], out, foo) do
    scan_parameters2(rest, [{:user, val} | out], foo)
  end

  defp scan_parameters2(['method' ++ val | rest], out, foo) do
    scan_parameters2(rest, [{:method, val} | out], foo)
  end

  defp scan_parameters2(['ttl' ++ val | rest], out, foo) do
    scan_parameters2(rest, [{:ttl, val} | out], foo)
  end

  defp scan_parameters2(['maddr' ++ val | rest], out, foo) do
    scan_parameters2(rest, [{:maddr, val} | out], foo)
  end

  defp scan_parameters2(['lr' | rest], out, foo) do
    scan_parameters2(rest, [{:lr, ''} | out], foo)
  end

  defp scan_parameters2([other | rest], out, foo) do
    scan_parameters2(rest, [out], [other | foo])
  end

  defp scan_headers(c2) do
    c2
  end

  defp parse_sms(cont, scheme) do
    {scheme, cont}
  end

  defp scan_hostport(c0, scheme) do
    case scan_host(c0) do
      {:error, error} ->
        {:error, error}

      {':' ++ c1, host} ->
        {c2, port} = scan_port(c1, [])
        {c2, host, :erlang.list_to_integer(port)}

      {c1, host} when scheme == :http ->
        {c1, host, 80}

      {c1, host} when scheme == :https ->
        {c1, host, 443}

      {c1, host} when scheme == :ftp ->
        {c1, host, 21}

      {c1, host} when scheme == :sip ->
        {c1, host, 5060}
    end
  end

  defp scan_host(c0) do
    case scan_host2(c0, [], 0, [], []) do
      {c1, iPv4address, [1, 1, 1, 1]} ->
        {c1, :lists.reverse(:lists.append(iPv4address))}

      {c1, hostname, [_A | _HostF]} ->
        {c1, :lists.reverse(:lists.append(hostname))}
    end
  end

  defp scan_host2([h | c0], acc, curF, host, hostF)
       when ?0 <= h and h <= ?9 do
    scan_host2(c0, [h | acc], curF ||| 1, host, hostF)
  end

  defp scan_host2([h | c0], acc, curF, host, hostF)
       when (?a <= h and h <= ?z) or (?A <= h and h <= ?Z) do
    scan_host2(c0, [h | acc], curF ||| 6, host, hostF)
  end

  defp scan_host2([?- | c0], acc, curF, host, hostF)
       when curF !== 0 do
    scan_host2(c0, [?- | acc], curF, host, hostF)
  end

  defp scan_host2([?. | c0], acc, curF, host, hostF)
       when curF !== 0 do
    scan_host2(c0, [], 0, ['.', acc | host], [curF | hostF])
  end

  defp scan_host2(c0, acc, curF, host, hostF) do
    {c0, [acc | host], [curF | hostF]}
  end

  defp scan_port([h | c0], acc) when ?0 <= h and h <= ?9 do
    scan_port(c0, [h | acc])
  end

  defp scan_port(c0, acc) do
    {c0, :lists.reverse(acc)}
  end

  defp scan_abspath([]) do
    {[], []}
  end

  defp scan_abspath('/' ++ c0) do
    scan_pathsegments(c0, ['/'])
  end

  defp scan_abspath(_) do
    {:error, :no_abspath}
  end

  defp scan_pathsegments(c0, acc) do
    case scan_segment(c0, []) do
      {'/' ++ c1, segment} ->
        scan_pathsegments(c1, ['/', segment | acc])

      {c1, segment} ->
        {c1, :lists.reverse(:lists.append([segment | acc]))}
    end
  end

  defp scan_segment(';' ++ c0, acc) do
    {c1, paramAcc} = scan_pchars(c0, ';' ++ acc)
    scan_segment(c1, paramAcc)
  end

  defp scan_segment(c0, acc) do
    case scan_pchars(c0, acc) do
      {';' ++ c1, segment} ->
        {c2, paramAcc} = scan_pchars(c1, ';' ++ segment)
        scan_segment(c2, paramAcc)

      {c1, segment} ->
        {c1, segment}
    end
  end

  defp scan_query([], acc) do
    :lists.reverse(acc)
  end

  defp scan_query([?%, h1, h2 | c0], acc) do
    scan_query([hex2dec(h1) * 16 + hex2dec(h2) | c0], acc)
  end

  defp scan_query([h | c0], acc)
       when (?a <= h and h <= ?z) or
              (?A <= h and h <= ?Z) or
              (?0 <= h and h <= ?9) do
    scan_query(c0, [h | acc])
  end

  defp scan_query([h | c0], acc)
       when h == ?; or h == ?/ or
              h == ?? or h == ?: or h == ?@ or h == ?[ or
              h == ?] or h == ?& or h == ?= or h == ?+ or
              h == ?$ or h == ?, do
    scan_query(c0, [h | acc])
  end

  defp scan_query([h | c0], acc)
       when h == ?- or h == ?_ or
              h == ?. or h == ?! or h == ?~ or h == ?* or
              h == ?' or h == ?( or h == ?) do
    scan_query(c0, [h | acc])
  end

  defp scan_query([h | c0], acc) when 0 <= h and h <= 127 do
    {h1, h2} = dec2hex(h)
    scan_query(c0, [h2, h1, ?% | acc])
  end

  defp scan_query([_H | _C0], _Acc) do
    {:error, :no_query}
  end

  defp scan_pchars([], acc) do
    {[], acc}
  end

  defp scan_pchars([?%, h1, h2 | c0], acc) do
    scan_pchars([hex2dec(h1) * 16 + hex2dec(h2) | c0], acc)
  end

  defp scan_pchars([h | c0], acc)
       when (?a <= h and h <= ?z) or
              (?A <= h and h <= ?Z) or
              (?0 <= h and h <= ?9) do
    scan_pchars(c0, [h | acc])
  end

  defp scan_pchars([h | c0], acc)
       when h == ?- or h == ?_ or
              h == ?. or h == ?! or h == ?~ or h == ?* or
              h == ?' or h == ?( or h == ?) do
    scan_pchars(c0, [h | acc])
  end

  defp scan_pchars([h | c0], acc)
       when h == ?: or h == ?@ or
              h == ?& or h == ?= or h == ?+ or h == ?$ or
              h == ?, do
    scan_pchars(c0, [h | acc])
  end

  defp scan_pchars([h | c0], acc)
       when 0 <= h and h <= 127 and
              h !== ?? and h !== ?; and h !== ?/ and
              h !== ?# do
    {h1, h2} = dec2hex(h)
    scan_pchars(c0, [h2, h1, ?% | acc])
  end

  defp scan_pchars(c0, acc) do
    {c0, acc}
  end

  defp hex2dec(x) when x >= ?0 and x <= ?9 do
    x - ?0
  end

  defp hex2dec(x) when x >= ?A and x <= ?F do
    x - ?A + 10
  end

  defp hex2dec(x) when x >= ?a and x <= ?f do
    x - ?a + 10
  end

  defp dec2hex(h) when h < 256 do
    <<h1::size(4), h2::size(4)>> = <<h>>
    {nibble2hex(h1), nibble2hex(h2)}
  end

  defp nibble2hex(x) when 0 <= x and x <= 9 do
    x + ?0
  end

  defp nibble2hex(10) do
    ?a
  end

  defp nibble2hex(11) do
    ?b
  end

  defp nibble2hex(12) do
    ?c
  end

  defp nibble2hex(13) do
    ?d
  end

  defp nibble2hex(14) do
    ?e
  end

  defp nibble2hex(15) do
    ?f
  end
end
