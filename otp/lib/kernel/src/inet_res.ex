defmodule :m_inet_res do
  use Bitwise
  require Record

  Record.defrecord(:r_hostent, :hostent,
    h_name: :undefined,
    h_aliases: [],
    h_addrtype: :undefined,
    h_length: :undefined,
    h_addr_list: []
  )

  Record.defrecord(:r_dns_header, :dns_header,
    id: 0,
    qr: 0,
    opcode: 0,
    aa: 0,
    tc: 0,
    rd: 0,
    ra: 0,
    pr: 0,
    rcode: 0
  )

  Record.defrecord(:r_dns_rec, :dns_rec,
    header: :undefined,
    qdlist: [],
    anlist: [],
    nslist: [],
    arlist: []
  )

  Record.defrecord(:r_dns_rr, :dns_rr,
    domain: '',
    type: :any,
    class: :in,
    cnt: 0,
    ttl: 0,
    data: [],
    tm: :undefined,
    bm: [],
    func: false
  )

  Record.defrecord(:r_dns_rr_opt, :dns_rr_opt,
    domain: '',
    type: :opt,
    udp_payload_size: 1280,
    ext_rcode: 0,
    version: 0,
    z: 0,
    data: []
  )

  Record.defrecord(:r_dns_query, :dns_query,
    domain: :undefined,
    type: :undefined,
    class: :undefined
  )

  Record.defrecord(:r_connect_opts, :connect_opts, ifaddr: :any, port: 0, fd: -1, opts: [])

  Record.defrecord(:r_listen_opts, :listen_opts,
    ifaddr: :any,
    port: 0,
    backlog: 5,
    fd: -1,
    opts: []
  )

  Record.defrecord(:r_udp_opts, :udp_opts, ifaddr: :any, port: 0, fd: -1, opts: [{:active, true}])

  Record.defrecord(:r_sctp_opts, :sctp_opts,
    ifaddr: :undefined,
    port: 0,
    fd: -1,
    type: :seqpacket,
    opts: [
      {:mode, :binary},
      {:buffer, 65536},
      {:sndbuf, 65536},
      {:recbuf, 1024},
      {:sctp_events, :undefined}
    ]
  )

  def resolve(name, class, type) do
    resolve(name, class, type, [], :infinity)
  end

  def resolve(name, class, type, opts) do
    resolve(name, class, type, opts, :infinity)
  end

  def resolve(name, class, type, opts, timeout) do
    case nsdname(name) do
      {:ok, nm} ->
        timer = :inet.start_timer(timeout)
        res = res_query(nm, class, type, opts, timer)
        _ = :inet.stop_timer(timer)
        res

      error ->
        error
    end
  end

  def lookup(name, class, type) do
    lookup(name, class, type, [])
  end

  def lookup(name, class, type, opts) do
    lookup(name, class, type, opts, :infinity)
  end

  def lookup(name, class, type, opts, timeout) do
    lookup_filter(resolve(name, class, type, opts, timeout), class, type)
  end

  defp lookup_filter({:ok, r_dns_rec(anlist: answers)}, class, type) do
    for a <- answers,
        class === :any or r_dns_rr(a, :class) === class,
        type === :any or r_dns_rr(a, :type) === type do
      r_dns_rr(a, :data)
    end
  end

  defp lookup_filter({:error, _}, _, _) do
    []
  end

  def nslookup(name, class, type) do
    do_nslookup(name, class, type, [], :infinity)
  end

  def nslookup(name, class, type, timeout)
      when is_integer(timeout) and timeout >= 0 do
    do_nslookup(name, class, type, [], timeout)
  end

  def nslookup(name, class, type, nSs) do
    nnslookup(name, class, type, nSs)
  end

  def nnslookup(name, class, type, nSs) do
    nnslookup(name, class, type, nSs, :infinity)
  end

  def nnslookup(name, class, type, nSs, timeout) do
    do_nslookup(name, class, type, [{:nameservers, nSs}], timeout)
  end

  defp do_nslookup(name, class, type, opts, timeout) do
    case resolve(name, class, type, opts, timeout) do
      {:error, {:qfmterror, _}} ->
        {:error, :einval}

      {:error, {reason, _}} ->
        {:error, reason}

      result ->
        result
    end
  end

  Record.defrecord(:r_options, :options,
    alt_nameservers: :undefined,
    edns: :undefined,
    inet6: :undefined,
    nameservers: :undefined,
    recurse: :undefined,
    retry: :undefined,
    timeout: :undefined,
    udp_payload_size: :undefined,
    usevc: :undefined,
    verbose: :undefined
  )

  defp make_options(opts0) do
    opts =
      for opt <- opts0 do
        cond do
          is_atom(opt) ->
            case :erlang.atom_to_list(opt) do
              'no' ++ x ->
                {:erlang.list_to_atom(x), false}

              _ ->
                {opt, true}
            end

          true ->
            opt
        end
      end

    sortedOpts =
      :lists.ukeysort(
        1,
        case :lists.keymember(:nameservers, 1, opts) do
          true ->
            case :lists.keymember(:alt_nameservers, 1, opts) do
              false ->
                [{:alt_nameservers, []} | opts]

              true ->
                opts
            end

          false ->
            opts
        end
      )

    sortedNames = Keyword.keys(r_options(r_options()))
    :inet_db.res_update_conf()

    :erlang.list_to_tuple([
      :options
      | make_options(sortedOpts, sortedNames)
    ])
  end

  defp make_options([_ | _] = opts0, [] = names0) do
    :erlang.error(:badarg, [opts0, names0])
  end

  defp make_options([], []) do
    []
  end

  defp make_options(
         [{:verbose, val} | opts] = opts0,
         [:verbose | names] = names0
       ) do
    cond do
      is_boolean(val) ->
        [val | make_options(opts, names)]

      true ->
        :erlang.error(:badarg, [opts0, names0])
    end
  end

  defp make_options(
         [{opt, val} | opts] = opts0,
         [opt | names] = names0
       ) do
    case :inet_db.res_check_option(opt, val) do
      true ->
        [val | make_options(opts, names)]

      false ->
        :erlang.error(:badarg, [opts0, names0])
    end
  end

  defp make_options(opts, [:verbose | names]) do
    [false | make_options(opts, names)]
  end

  defp make_options(opts, [name | names]) do
    [:inet_db.res_option(name) | make_options(opts, names)]
  end

  def gethostbyaddr(iP) do
    gethostbyaddr_tm(iP, false)
  end

  def gethostbyaddr(iP, timeout) do
    timer = :inet.start_timer(timeout)
    res = gethostbyaddr_tm(iP, timer)
    _ = :inet.stop_timer(timer)
    res
  end

  def gethostbyaddr_tm({a, b, c, d} = iP, timer)
      when (a ||| b ||| c ||| d) &&& ~~~255 === 0 do
    :inet_db.res_update_conf()

    case :inet_db.gethostbyaddr(iP) do
      {:ok, hEnt} ->
        {:ok, hEnt}

      _ ->
        res_gethostbyaddr(dn_in_addr_arpa(a, b, c, d), iP, timer)
    end
  end

  def gethostbyaddr_tm({a, b, c, d, e, f, g, h} = iP, timer)
      when (a ||| b ||| c ||| d ||| e ||| f ||| g ||| h) &&& ~~~65535 === 0 do
    :inet_db.res_update_conf()

    case :inet_db.gethostbyaddr(iP) do
      {:ok, hEnt} ->
        {:ok, hEnt}

      _ ->
        res_gethostbyaddr(dn_ip6_int(a, b, c, d, e, f, g, h), iP, timer)
    end
  end

  def gethostbyaddr_tm(addr, timer) when is_list(addr) do
    case :inet_parse.address(addr) do
      {:ok, iP} ->
        gethostbyaddr_tm(iP, timer)

      _Error ->
        {:error, :formerr}
    end
  end

  def gethostbyaddr_tm(addr, timer) when is_atom(addr) do
    gethostbyaddr_tm(:erlang.atom_to_list(addr), timer)
  end

  def gethostbyaddr_tm(_, _) do
    {:error, :formerr}
  end

  defp res_gethostbyaddr(addr, iP, timer) do
    case res_query(addr, :in, :ptr, [], timer) do
      {:ok, rec} ->
        :inet_db.res_gethostbyaddr(iP, rec)

      {:error, {:qfmterror, _}} ->
        {:error, :einval}

      {:error, {reason, _}} ->
        {:error, reason}

      error ->
        error
    end
  end

  def gethostbyname(name) do
    case :inet_db.res_option(:inet6) do
      true ->
        gethostbyname_tm(name, :inet6, false)

      false ->
        gethostbyname_tm(name, :inet, false)
    end
  end

  def gethostbyname(name, family) do
    gethostbyname_tm(name, family, false)
  end

  def gethostbyname(name, family, timeout) do
    timer = :inet.start_timer(timeout)
    res = gethostbyname_tm(name, family, timer)
    _ = :inet.stop_timer(timer)
    res
  end

  def gethostbyname_tm(name, :inet, timer) do
    getbyname_tm(name, :a, timer)
  end

  def gethostbyname_tm(name, :inet6, timer) do
    getbyname_tm(name, :aaaa, timer)
  end

  def gethostbyname_tm(_Name, _Family, _Timer) do
    {:error, :einval}
  end

  def getbyname(name, type) do
    getbyname_tm(name, type, false)
  end

  def getbyname(name, type, timeout) do
    timer = :inet.start_timer(timeout)
    res = getbyname_tm(name, type, timer)
    _ = :inet.stop_timer(timer)
    res
  end

  def getbyname_tm(name, type, timer) when is_list(name) do
    case type_p(type) do
      true ->
        case :inet_parse.visible_string(name) do
          false ->
            {:error, :formerr}

          true ->
            :inet_db.res_update_conf()

            case :inet_db.getbyname(name, type) do
              {:ok, hEnt} ->
                {:ok, hEnt}

              _ ->
                res_getbyname(name, type, timer)
            end
        end

      false ->
        {:error, :formerr}
    end
  end

  def getbyname_tm(name, type, timer) when is_atom(name) do
    getbyname_tm(:erlang.atom_to_list(name), type, timer)
  end

  def getbyname_tm(_, _, _) do
    {:error, :formerr}
  end

  defp type_p(type) do
    :lists.member(
      type,
      [
        :a,
        :aaaa,
        :mx,
        :ns,
        :md,
        :mf,
        :cname,
        :soa,
        :mb,
        :mg,
        :mr,
        :null,
        :wks,
        :hinfo,
        :txt,
        :srv,
        :naptr,
        :spf,
        :uinfo,
        :uid,
        :gid
      ]
    )
  end

  defp res_getbyname(name, type, timer) do
    {embeddedDots, trailingDot} = :inet_parse.dots(name)

    dot =
      cond do
        trailingDot ->
          ''

        true ->
          '.'
      end

    cond do
      trailingDot ->
        res_getby_query(name, type, timer)

      embeddedDots === 0 ->
        res_getby_search(name, dot, :inet_db.get_searchlist(), :nxdomain, type, timer)

      true ->
        case res_getby_query(name, type, timer) do
          {:error, _Reason} = error ->
            res_getby_search(name, dot, :inet_db.get_searchlist(), error, type, timer)

          other ->
            other
        end
    end
  end

  defp res_getby_search(name, dot, [dom | ds], _Reason, type, timer) do
    case res_getby_query(name ++ dot ++ dom, type, timer, :inet_db.res_option(:nameservers)) do
      {:ok, hEnt} ->
        {:ok, hEnt}

      {:error, newReason} ->
        res_getby_search(name, dot, ds, newReason, type, timer)
    end
  end

  defp res_getby_search(_Name, _, [], reason, _, _) do
    {:error, reason}
  end

  defp res_getby_query(name, type, timer) do
    case res_query(name, :in, type, [], timer) do
      {:ok, rec} ->
        :inet_db.res_hostent_by_domain(name, type, rec)

      {:error, {:qfmterror, _}} ->
        {:error, :einval}

      {:error, {reason, _}} ->
        {:error, reason}

      error ->
        error
    end
  end

  defp res_getby_query(name, type, timer, nSs) do
    case res_query(name, :in, type, [], timer, nSs) do
      {:ok, rec} ->
        :inet_db.res_hostent_by_domain(name, type, rec)

      {:error, {:qfmterror, _}} ->
        {:error, :einval}

      {:error, {reason, _}} ->
        {:error, reason}

      error ->
        error
    end
  end

  Record.defrecord(:r_q, :q, options: :undefined, edns: :undefined, dns: :undefined)

  defp res_query(name, class, type, opts, timer) do
    r_q(options: r_options(nameservers: nSs)) = q = make_query(name, class, type, opts)

    case do_query(q, nSs, timer) do
      {:error, :nxdomain} = error ->
        res_query_alt(q, error, timer)

      {:error, {:nxdomain, _}} = error ->
        res_query_alt(q, error, timer)

      {:ok, r_dns_rec(anlist: [])} = reply ->
        res_query_alt(q, reply, timer)

      reply ->
        reply
    end
  end

  defp res_query(name, class, type, opts, timer, nSs) do
    q = make_query(name, class, type, opts)
    do_query(q, nSs, timer)
  end

  defp res_query_alt(r_q(options: r_options(alt_nameservers: nSs)) = q, reply, timer) do
    case nSs do
      [] ->
        reply

      _ ->
        do_query(q, nSs, timer)
    end
  end

  defp make_query(dname, class, type, opts) do
    options = make_options(opts)

    case r_options(options, :edns) do
      false ->
        r_q(
          options: options,
          edns: :undefined,
          dns: make_query(dname, class, type, options, false)
        )

      edns ->
        r_q(
          options: options,
          edns: make_query(dname, class, type, options, edns),
          dns: fn ->
            make_query(dname, class, type, options, false)
          end
        )
    end
  end

  defp make_query(dname, class, type, options, edns) do
    id = :inet_db.res_option(:next_id)
    recurse = r_options(options, :recurse)

    aRList =
      case edns do
        false ->
          []

        _ ->
          pSz = r_options(options, :udp_payload_size)
          [r_dns_rr_opt(udp_payload_size: pSz, version: edns)]
      end

    msg =
      r_dns_rec(
        header: r_dns_header(id: id, opcode: :query, rd: recurse, rcode: 0),
        qdlist: [r_dns_query(domain: dname, type: type, class: class)],
        arlist: aRList
      )

    case r_options(options, :verbose) do
      true ->
        :io.format(
          'Query: ~p~n',
          [dns_msg(msg)]
        )

      false ->
        :ok
    end

    buffer = :inet_dns.encode(msg)
    {id, buffer}
  end

  Record.defrecord(:r_sock, :sock,
    inet: :undefined,
    inet6: :undefined
  )

  defp udp_open(r_sock(inet6: i) = s, {a, b, c, d, e, f, g, h})
       when (a ||| b ||| c ||| d ||| e ||| f ||| g ||| h) &&& ~~~65535 === 0 do
    case i do
      :undefined ->
        case :gen_udp.open(
               0,
               [{:active, false}, :binary, :inet6]
             ) do
          {:ok, j} ->
            {:ok, r_sock(s, inet6: j)}

          error ->
            error
        end

      _ ->
        {:ok, s}
    end
  end

  defp udp_open(r_sock(inet: i) = s, {a, b, c, d})
       when (a ||| b ||| c ||| d) &&& ~~~255 === 0 do
    case i do
      :undefined ->
        case :gen_udp.open(
               0,
               [{:active, false}, :binary, :inet]
             ) do
          {:ok, j} ->
            {:ok, r_sock(s, inet: j)}

          error ->
            error
        end

      _ ->
        {:ok, s}
    end
  end

  defp udp_connect(r_sock(inet6: i), {a, b, c, d, e, f, g, h} = iP, port)
       when (a ||| b ||| c ||| d ||| e ||| f ||| g ||| h) &&& ~~~65535 === 0 and
              port &&& ~~~65535 === 0 do
    :gen_udp.connect(i, iP, port)
  end

  defp udp_connect(r_sock(inet: i), {a, b, c, d} = iP, port)
       when (a ||| b ||| c ||| d) &&& ~~~255 === 0 do
    :gen_udp.connect(i, iP, port)
  end

  defp udp_send(r_sock(inet6: i), {a, b, c, d, e, f, g, h} = iP, port, buffer)
       when (a ||| b ||| c ||| d ||| e ||| f ||| g ||| h) &&& ~~~65535 === 0 and
              port &&& ~~~65535 === 0 do
    :gen_udp.send(i, iP, port, buffer)
  end

  defp udp_send(r_sock(inet: i), {a, b, c, d} = iP, port, buffer)
       when (a ||| b ||| c ||| d) &&& ~~~255 === 0 and
              port &&& ~~~65535 === 0 do
    :gen_udp.send(i, iP, port, buffer)
  end

  defp udp_recv(r_sock(inet6: i), {a, b, c, d, e, f, g, h} = iP, port, timeout, decode)
       when (a ||| b ||| c ||| d ||| e ||| f ||| g ||| h) &&& ~~~65535 === 0 and
              port &&& ~~~65535 === 0 and 0 <= timeout do
    do_udp_recv(i, iP, port, timeout, decode, deadline(timeout), timeout)
  end

  defp udp_recv(r_sock(inet: i), {a, b, c, d} = iP, port, timeout, decode)
       when (a ||| b ||| c ||| d) &&& ~~~255 === 0 and
              port &&& ~~~65535 === 0 and 0 <= timeout do
    do_udp_recv(i, iP, port, timeout, decode, deadline(timeout), timeout)
  end

  defp do_udp_recv(_I, _IP, _Port, 0, _Decode, _Deadline, pollCnt)
       when pollCnt <= 0 do
    :timeout
  end

  defp do_udp_recv(i, iP, port, timeout, decode, deadline, pollCnt) do
    case :gen_udp.recv(i, 0, timeout) do
      {:ok, reply} ->
        case decode.(reply) do
          false when timeout === 0 ->
            do_udp_recv(i, iP, port, timeout, decode, deadline, pollCnt - 50)

          false ->
            t = timeout(deadline)
            do_udp_recv(i, iP, port, t, decode, deadline, pollCnt)

          result ->
            result
        end

      error ->
        error
    end
  end

  defp udp_close(r_sock(inet: i, inet6: i6)) do
    cond do
      i !== :undefined ->
        :gen_udp.close(i)

      true ->
        :ok
    end

    cond do
      i6 !== :undefined ->
        :gen_udp.close(i6)

      true ->
        :ok
    end

    :ok
  end

  defp do_query(_Q, [], _Timer) do
    {:error, :nxdomain}
  end

  defp do_query(r_q(options: r_options(retry: retry)) = q, nSs, timer) do
    reason = :timeout
    query_retries(q, nSs, timer, retry, 0, r_sock(), reason)
  end

  defp query_retries(_Q, _NSs, _Timer, retry, retry, s, reason) do
    query_retries_error(s, reason)
  end

  defp query_retries(_Q, [], _Timer, _Retry, _I, s, reason) do
    query_retries_error(s, reason)
  end

  defp query_retries(q, nSs, timer, retry, i, s_0, reason) do
    query_nss(q, nSs, timer, retry, i, s_0, reason, nSs)
  end

  defp query_nss(q, nSs, timer, retry, i, s, reason, []) do
    query_retries(q, nSs, timer, retry, i + 1, s, reason)
  end

  defp query_nss(r_q(edns: :undefined) = q, nSs, timer, retry, i, s, reason, tryNSs) do
    query_nss_dns(q, nSs, timer, retry, i, s, reason, tryNSs)
  end

  defp query_nss(q, nSs, timer, retry, i, s, reason, tryNSs) do
    query_nss_edns(q, nSs, timer, retry, i, s, reason, tryNSs)
  end

  defp query_nss_edns(
         r_q(
           options: r_options(udp_payload_size: pSz) = options,
           edns: {id, buffer}
         ) = q,
         nSs,
         timer,
         retry,
         i,
         s_0,
         reason,
         [{iP, port} = nS | tryNSs] = tryNSs_0
       ) do
    {s, result} = query_ns(s_0, id, buffer, iP, port, timer, retry, i, options, pSz)

    case result do
      {:error, {e, _}}
      when e === :qfmterror or
             e === :notimp or e === :servfail or e === :badvers ->
        query_nss_dns(q, nSs, timer, retry, i, s, reason, tryNSs_0)

      _ ->
        query_nss_result(q, nSs, timer, retry, i, s, reason, tryNSs, nS, result)
    end
  end

  defp query_nss_dns(r_q(dns: qdns) = q_0, nSs, timer, retry, i, s_0, reason, [
         {iP, port} = nS | tryNSs
       ]) do
    r_q(options: options, dns: {id, buffer}) =
      q =
      cond do
        is_function(qdns, 0) ->
          r_q(q_0, dns: qdns.())

        true ->
          q_0
      end

    {s, result} = query_ns(s_0, id, buffer, iP, port, timer, retry, i, options, 512)
    query_nss_result(q, nSs, timer, retry, i, s, reason, tryNSs, nS, result)
  end

  defp query_nss_result(q, nSs, timer, retry, i, s, reason, tryNSs, nS, result) do
    case result do
      {:ok, _} ->
        _ = udp_close(s)
        result

      :timeout ->
        query_retries_error(s, reason)

      {:error, :timeout} ->
        query_nss(q, nSs, timer, retry, i, s, reason, tryNSs)

      {:error, {:nxdomain, _} = newReason} ->
        query_retries_error(s, newReason)

      {:error, {e, _} = newReason}
      when e === :qfmterror or
             e === :notimp or e === :refused or
             e === :badvers ->
        newNSs = :lists.delete(nS, nSs)
        query_nss(q, newNSs, timer, retry, i, s, newReason, tryNSs)

      {:error, e = newReason}
      when e === :formerr or
             e === :enetunreach or e === :econnrefused ->
        newNSs = :lists.delete(nS, nSs)
        query_nss(q, newNSs, timer, retry, i, s, newReason, tryNSs)

      {:error, newReason} ->
        query_nss(q, nSs, timer, retry, i, s, newReason, tryNSs)
    end
  end

  defp query_retries_error(s, reason) do
    _ = udp_close(s)

    case reason do
      {:nxdomain, _} ->
        {:error, :nxdomain}

      _ ->
        {:error, reason}
    end
  end

  defp query_ns(
         s0,
         id,
         buffer,
         iP,
         port,
         timer,
         retry,
         i,
         r_options(timeout: tm, usevc: useVC, verbose: verbose),
         pSz
       ) do
    case useVC or :erlang.iolist_size(buffer) > pSz do
      true ->
        tcpTimeout = :inet.timeout(tm * 5, timer)
        {s0, query_tcp(tcpTimeout, id, buffer, iP, port, verbose)}

      false ->
        case udp_open(s0, iP) do
          {:ok, s} ->
            timeout =
              :inet.timeout(
                div(tm * (1 <<< i), retry),
                timer
              )

            case query_udp(s, id, buffer, iP, port, timeout, verbose) do
              {:ok, r_dns_rec(header: h)} when r_dns_header(h, :tc) ->
                tcpTimeout = :inet.timeout(tm * 5, timer)
                {s, query_tcp(tcpTimeout, id, buffer, iP, port, verbose)}

              {:error, :econnrefused} = err ->
                :ok = udp_close(s)
                {r_sock(), err}

              reply ->
                {s, reply}
            end

          error ->
            {s0, error}
        end
    end
  end

  defp query_udp(_S, _Id, _Buffer, _IP, _Port, 0, _Verbose) do
    :timeout
  end

  defp query_udp(s, id, buffer, iP, port, timeout, verbose) do
    case verbose do
      true ->
        :io.format(
          'Try UDP server : ~p:~p (timeout=~w)\n',
          [iP, port, timeout]
        )

      false ->
        :ok
    end

    case (case udp_connect(s, iP, port) do
            :ok ->
              udp_send(s, iP, port, buffer)

            e1 ->
              e1
          end) do
      :ok ->
        decode = fn
          {recIP, recPort, answer}
          when recIP === iP and recPort === port ->
            case decode_answer(answer, id, verbose) do
              {:error, :badid} ->
                false

              reply ->
                reply
            end

          {_, _, _} ->
            false
        end

        case udp_recv(s, iP, port, timeout, decode) do
          {:ok, _} = result ->
            result

          e2 ->
            case verbose do
              true ->
                :io.format(
                  'UDP server error: ~p\n',
                  [e2]
                )

              false ->
                :ok
            end

            e2
        end

      e3 ->
        case verbose do
          true ->
            :io.format(
              'UDP send failed: ~p\n',
              [e3]
            )

          false ->
            :ok
        end

        {:error, :econnrefused}
    end
  end

  defp query_tcp(0, _Id, _Buffer, _IP, _Port, _Verbose) do
    :timeout
  end

  defp query_tcp(timeout, id, buffer, iP, port, verbose) do
    case verbose do
      true ->
        :io.format(
          'Try TCP server : ~p:~p (timeout=~w)\n',
          [iP, port, timeout]
        )

      false ->
        :ok
    end

    family =
      case iP do
        {a, b, c, d} when (a ||| b ||| c ||| d) &&& ~~~255 === 0 ->
          :inet

        {a, b, c, d, e, f, g, h}
        when (a ||| b ||| c ||| d ||| e ||| f ||| g ||| h) &&& ~~~65535 === 0 ->
          :inet6
      end

    try do
      :gen_tcp.connect(iP, port, [{:active, false}, {:packet, 2}, :binary, family], timeout)
    catch
      _, _ ->
        {:error, :einval}
    else
      {:ok, s} ->
        case :gen_tcp.send(s, buffer) do
          :ok ->
            case :gen_tcp.recv(s, 0, timeout) do
              {:ok, answer} ->
                :gen_tcp.close(s)

                case decode_answer(answer, id, verbose) do
                  {:ok, _} = oK ->
                    oK

                  {:error, :badid} ->
                    {:error, :servfail}

                  error ->
                    error
                end

              error ->
                :gen_tcp.close(s)

                case verbose do
                  true ->
                    :io.format(
                      'TCP server recv error: ~p\n',
                      [error]
                    )

                  false ->
                    :ok
                end

                error
            end

          error ->
            :gen_tcp.close(s)

            case verbose do
              true ->
                :io.format(
                  'TCP server send error: ~p\n',
                  [error]
                )

              false ->
                :ok
            end

            error
        end

      error ->
        case verbose do
          true ->
            :io.format(
              'TCP server error: ~p\n',
              [error]
            )

          false ->
            :ok
        end

        error
    end
  end

  defp decode_answer(answer, id, verbose) do
    case :inet_dns.decode(answer) do
      {:ok, msg} ->
        case verbose do
          true ->
            :io.format(
              'Got reply: ~p~n',
              [dns_msg(msg)]
            )

          false ->
            :ok
        end

        e =
          case :lists.keyfind(:dns_rr_opt, 1, r_dns_rec(msg, :arlist)) do
            false ->
              0

            r_dns_rr_opt(ext_rcode: extRCode) ->
              extRCode
          end

        h = r_dns_rec(msg, :header)
        rCode = e <<< 4 ||| r_dns_header(h, :rcode)

        case rCode do
          0 ->
            cond do
              r_dns_header(h, :id) !== id ->
                {:error, :badid}

              length(r_dns_rec(msg, :qdlist)) !== 1 ->
                {:error, {:noquery, msg}}

              true ->
                {:ok, msg}
            end

          1 ->
            {:error, {:qfmterror, msg}}

          2 ->
            {:error, {:servfail, msg}}

          3 ->
            {:error, {:nxdomain, msg}}

          4 ->
            {:error, {:notimp, msg}}

          5 ->
            {:error, {:refused, msg}}

          16 ->
            {:error, {:badvers, msg}}

          _ ->
            {:error, {:unknown, msg}}
        end

      error ->
        case verbose do
          true ->
            :io.format(
              'Got reply: ~p~n',
              [error]
            )

          false ->
            :ok
        end

        error
    end
  end

  defp nsdname({a, b, c, d}) do
    {:ok, dn_in_addr_arpa(a, b, c, d)}
  end

  defp nsdname({a, b, c, d, e, f, g, h}) do
    {:ok, dn_ip6_int(a, b, c, d, e, f, g, h)}
  end

  defp nsdname(name) when is_list(name) do
    case :inet_parse.visible_string(name) do
      true ->
        case :inet_parse.address(name) do
          {:ok, addr} ->
            nsdname(addr)

          _ ->
            {:ok, name}
        end

      _ ->
        {:error, :formerr}
    end
  end

  defp nsdname(name) when is_atom(name) do
    nsdname(:erlang.atom_to_list(name))
  end

  defp nsdname(_) do
    {:error, :formerr}
  end

  defp dn_in_addr_arpa(a, b, c, d) do
    :erlang.integer_to_list(d) ++
      '.' ++
      :erlang.integer_to_list(c) ++
      '.' ++ :erlang.integer_to_list(b) ++ '.' ++ :erlang.integer_to_list(a) ++ '.IN-ADDR.ARPA'
  end

  defp dn_ip6_int(a, b, c, d, e, f, g, h) do
    dnib(h) ++
      dnib(g) ++ dnib(f) ++ dnib(e) ++ dnib(d) ++ dnib(c) ++ dnib(b) ++ dnib(a) ++ 'IP6.ARPA'
  end

  defp dnib(x) do
    l = :erlang.integer_to_list(x, 16)
    dnib(4 - length(l), l, [])
  end

  defp dnib(0, [], acc) do
    acc
  end

  defp dnib(0, [c | cs], acc) do
    dnib(0, cs, [c, ?. | acc])
  end

  defp dnib(n, cs, acc) do
    dnib(n - 1, cs, [?0, ?. | acc])
  end

  def dns_msg([]) do
    []
  end

  def dns_msg([{field, msg} | fields]) do
    [{field, dns_msg(msg)} | dns_msg(fields)]
  end

  def dns_msg([msg | msgs]) do
    [dns_msg(msg) | dns_msg(msgs)]
  end

  def dns_msg(msg) do
    case :inet_dns.record_type(msg) do
      :undefined ->
        msg

      type ->
        fields = apply(:inet_dns, type, [msg])
        {type, dns_msg(fields)}
    end
  end

  defp deadline(timeout) do
    :erlang.monotonic_time(1000) + timeout
  end

  defp timeout(deadline) do
    case deadline - :erlang.monotonic_time(1000) do
      timeout when 0 <= timeout ->
        timeout

      _ ->
        0
    end
  end
end
