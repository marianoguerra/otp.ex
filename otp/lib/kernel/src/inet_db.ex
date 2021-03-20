defmodule :m_inet_db do
  use Bitwise
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

  Record.defrecord(:r_state, :state,
    db: :undefined,
    cache: :undefined,
    hosts_byname: :undefined,
    hosts_byaddr: :undefined,
    hosts_file_byname: :undefined,
    hosts_file_byaddr: :undefined,
    cache_timer: :undefined
  )

  Record.defrecord(:r_hostent, :hostent,
    h_name: :undefined,
    h_aliases: [],
    h_addrtype: :undefined,
    h_length: :undefined,
    h_addr_list: []
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

  def start() do
    case :gen_server.start({:local, :inet_db}, :inet_db, [], []) do
      {:ok, _Pid} = ok ->
        :inet_config.init()
        ok

      error ->
        error
    end
  end

  def start_link() do
    case :gen_server.start_link({:local, :inet_db}, :inet_db, [], []) do
      {:ok, _Pid} = ok ->
        :inet_config.init()
        ok

      error ->
        error
    end
  end

  defp call(req) do
    :gen_server.call(:inet_db, req, :infinity)
  end

  def stop() do
    call(:stop)
  end

  def reset() do
    call(:reset)
  end

  def add_resolv(file) do
    case :inet_parse.resolv(file) do
      {:ok, res} ->
        add_rc_list(res)

      error ->
        error
    end
  end

  def add_hosts(file) do
    case :inet_parse.hosts(file) do
      {:ok, res} ->
        :lists.foreach(
          fn {iP, name, aliases} ->
            add_host(iP, [name | aliases])
          end,
          res
        )

      error ->
        error
    end
  end

  def add_host(iP, names) do
    call({:add_host, iP, names})
  end

  def del_host(iP) do
    call({:del_host, iP})
  end

  def clear_hosts() do
    call(:clear_hosts)
  end

  def add_ns(iP) do
    add_ns(iP, 53)
  end

  def add_ns(iP, port) do
    call({:listop, :nameservers, :add, {iP, port}})
  end

  def ins_ns(iP) do
    ins_ns(iP, 53)
  end

  def ins_ns(iP, port) do
    call({:listop, :nameservers, :ins, {iP, port}})
  end

  def del_ns(iP) do
    del_ns(iP, 53)
  end

  def del_ns(iP, port) do
    call({:listop, :nameservers, :del, {iP, port}})
  end

  def del_ns() do
    call({:listdel, :nameservers})
  end

  def add_alt_ns(iP) do
    add_alt_ns(iP, 53)
  end

  def add_alt_ns(iP, port) do
    call({:listop, :alt_nameservers, :add, {iP, port}})
  end

  def ins_alt_ns(iP) do
    ins_alt_ns(iP, 53)
  end

  def ins_alt_ns(iP, port) do
    call({:listop, :alt_nameservers, :ins, {iP, port}})
  end

  def del_alt_ns(iP) do
    del_alt_ns(iP, 53)
  end

  def del_alt_ns(iP, port) do
    call({:listop, :alt_nameservers, :del, {iP, port}})
  end

  def del_alt_ns() do
    call({:listdel, :alt_nameservers})
  end

  def add_search(domain) when is_list(domain) do
    call({:listop, :search, :add, domain})
  end

  def ins_search(domain) when is_list(domain) do
    call({:listop, :search, :ins, domain})
  end

  def del_search(domain) do
    call({:listop, :search, :del, domain})
  end

  def del_search() do
    call({:listdel, :search})
  end

  def set_hostname(name) do
    call({:set_hostname, name})
  end

  def set_domain(domain) do
    res_option(:domain, domain)
  end

  def set_lookup(methods) do
    res_option(:lookup, methods)
  end

  def set_recurse(flag) do
    res_option(:recurse, flag)
  end

  def set_timeout(time) do
    res_option(:timeout, time)
  end

  def set_retry(n) do
    res_option(:retry, n)
  end

  def set_inet6(bool) do
    res_option(:inet6, bool)
  end

  def set_usevc(bool) do
    res_option(:usevc, bool)
  end

  def set_edns(version) do
    res_option(:edns, version)
  end

  def set_udp_payload_size(size) do
    res_option(:udp_payload_size, size)
  end

  def set_resolv_conf(fname) when is_list(fname) do
    res_option(:resolv_conf, fname)
  end

  def set_hosts_file(fname) when is_list(fname) do
    res_option(:hosts_file, fname)
  end

  def get_hosts_file() do
    get_rc_hosts([], [], :inet_hosts_file_byaddr)
  end

  def set_socks_server(server) do
    call({:set_socks_server, server})
  end

  def set_socks_port(port) do
    call({:set_socks_port, port})
  end

  def add_socks_methods(ms) do
    call({:add_socks_methods, ms})
  end

  def del_socks_methods(ms) do
    call({:del_socks_methods, ms})
  end

  def del_socks_methods() do
    call(:del_socks_methods)
  end

  def add_socks_noproxy({net, mask}) do
    call({:add_socks_noproxy, {net, mask}})
  end

  def del_socks_noproxy(net) do
    call({:del_socks_noproxy, net})
  end

  def set_cache_size(limit) do
    call({:set_cache_size, limit})
  end

  def set_cache_refresh(time) do
    call({:set_cache_refresh, time})
  end

  def clear_cache() do
    call(:clear_cache)
  end

  def set_tcp_module(module) do
    call({:set_tcp_module, module})
  end

  def tcp_module() do
    db_get(:tcp_module)
  end

  def set_udp_module(module) do
    call({:set_udp_module, module})
  end

  def udp_module() do
    db_get(:udp_module)
  end

  def set_sctp_module(family) do
    call({:set_sctp_module, family})
  end

  def sctp_module() do
    db_get(:sctp_module)
  end

  def add_rc(file) do
    case :file.consult(file) do
      {:ok, list} ->
        add_rc_list(list)

      error ->
        error
    end
  end

  def add_rc_bin(bin) do
    case (try do
            :erlang.binary_to_term(bin)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      list when is_list(list) ->
        add_rc_list(list)

      _ ->
        {:error, :badarg}
    end
  end

  def add_rc_list(list) do
    call({:add_rc_list, list})
  end

  defp translate_lookup(['bind' | ls]) do
    [:dns | translate_lookup(ls)]
  end

  defp translate_lookup(['dns' | ls]) do
    [:dns | translate_lookup(ls)]
  end

  defp translate_lookup(['hosts' | ls]) do
    [:file | translate_lookup(ls)]
  end

  defp translate_lookup(['files' | ls]) do
    [:file | translate_lookup(ls)]
  end

  defp translate_lookup(['file' | ls]) do
    [:file | translate_lookup(ls)]
  end

  defp translate_lookup(['yp' | ls]) do
    [:yp | translate_lookup(ls)]
  end

  defp translate_lookup(['nis' | ls]) do
    [:nis | translate_lookup(ls)]
  end

  defp translate_lookup(['nisplus' | ls]) do
    [:nisplus | translate_lookup(ls)]
  end

  defp translate_lookup(['native' | ls]) do
    [:native | translate_lookup(ls)]
  end

  defp translate_lookup([m | ls]) when is_atom(m) do
    translate_lookup([:erlang.atom_to_list(m) | ls])
  end

  defp translate_lookup([_ | ls]) do
    translate_lookup(ls)
  end

  defp translate_lookup([]) do
    []
  end

  defp valid_lookup() do
    [:dns, :file, :yp, :nis, :nisplus, :native]
  end

  def get_rc() do
    get_rc(
      [
        :hosts,
        :domain,
        :nameservers,
        :search,
        :alt_nameservers,
        :timeout,
        :retry,
        :inet6,
        :usevc,
        :edns,
        :udp_payload_size,
        :resolv_conf,
        :hosts_file,
        :socks5_server,
        :socks5_port,
        :socks5_methods,
        :socks5_noproxy,
        :udp,
        :sctp,
        :tcp,
        :host,
        :cache_size,
        :cache_refresh,
        :lookup
      ],
      []
    )
  end

  defp get_rc([k | ks], ls) do
    case k do
      :hosts ->
        get_rc_hosts(ks, ls, :inet_hosts_byaddr)

      :domain ->
        get_rc(:domain, :res_domain, '', ks, ls)

      :nameservers ->
        get_rc_ns(db_get(:res_ns), :nameservers, ks, ls)

      :alt_nameservers ->
        get_rc_ns(db_get(:res_alt_ns), :alt_nameservers, ks, ls)

      :search ->
        get_rc(:search, :res_search, [], ks, ls)

      :timeout ->
        get_rc(:timeout, :res_timeout, 2000, ks, ls)

      :retry ->
        get_rc(:retry, :res_retry, 3, ks, ls)

      :inet6 ->
        get_rc(:inet6, :res_inet6, false, ks, ls)

      :usevc ->
        get_rc(:usevc, :res_usevc, false, ks, ls)

      :edns ->
        get_rc(:edns, :res_edns, false, ks, ls)

      :udp_payload_size ->
        get_rc(:udp_payload_size, :res_udp_payload_size, 1280, ks, ls)

      :resolv_conf ->
        get_rc(:resolv_conf, :res_resolv_conf, :undefined, ks, ls)

      :hosts_file ->
        get_rc(:hosts_file, :res_hosts_file, :undefined, ks, ls)

      :tcp ->
        get_rc(:tcp, :tcp_module, :inet_tcp, ks, ls)

      :udp ->
        get_rc(:udp, :udp_module, :inet_udp, ks, ls)

      :sctp ->
        get_rc(:sctp, :sctp_module, :inet_sctp, ks, ls)

      :lookup ->
        get_rc(:lookup, :res_lookup, [:native, :file], ks, ls)

      :cache_size ->
        get_rc(:cache_size, :cache_size, 100, ks, ls)

      :cache_refresh ->
        get_rc(:cache_refresh, :cache_refresh_interval, 60 * 60 * 1000, ks, ls)

      :socks5_server ->
        get_rc(:socks5_server, :socks5_server, '', ks, ls)

      :socks5_port ->
        get_rc(:socks5_port, :socks5_port, 1080, ks, ls)

      :socks5_methods ->
        get_rc(:socks5_methods, :socks5_methods, [:none], ks, ls)

      :socks5_noproxy ->
        case db_get(:socks5_noproxy) do
          [] ->
            get_rc(ks, ls)

          noProxy ->
            get_rc_noproxy(noProxy, ks, ls)
        end

      _ ->
        get_rc(ks, ls)
    end
  end

  defp get_rc([], ls) do
    :lists.reverse(ls)
  end

  defp get_rc(name, key, default, ks, ls) do
    case db_get(key) do
      ^default ->
        get_rc(ks, ls)

      value ->
        get_rc(ks, [{name, value} | ls])
    end
  end

  defp get_rc_noproxy([{net, mask} | ms], ks, ls) do
    get_rc_noproxy(ms, ks, [{:socks5_noproxy, net, mask} | ls])
  end

  defp get_rc_noproxy([], ks, ls) do
    get_rc(ks, ls)
  end

  defp get_rc_ns([{iP, 53} | ns], tag, ks, ls) do
    get_rc_ns(ns, tag, ks, [{tag, iP} | ls])
  end

  defp get_rc_ns([{iP, port} | ns], tag, ks, ls) do
    get_rc_ns(ns, tag, ks, [{tag, iP, port} | ls])
  end

  defp get_rc_ns([], _Tag, ks, ls) do
    get_rc(ks, ls)
  end

  defp get_rc_hosts(ks, ls, tab) do
    case :ets.tab2list(tab) do
      [] ->
        get_rc(ks, ls)

      hosts ->
        get_rc(
          ks,
          [
            for {{_Fam, iP}, names} <- hosts do
              {:host, iP, names}
            end
            | ls
          ]
        )
    end
  end

  def res_option(:next_id) do
    cnt = :ets.update_counter(:inet_db, :res_id, 1)

    case cnt &&& 65535 do
      0 ->
        :ets.update_counter(:inet_db, :res_id, -cnt)
        0

      id ->
        id
    end
  end

  def res_option(option) do
    case res_optname(option) do
      :undefined ->
        :erlang.error(:badarg, [option])

      resOptname ->
        db_get(resOptname)
    end
  end

  def res_option(option, value) do
    case res_optname(option) do
      :undefined ->
        :erlang.error(:badarg, [option, value])

      _ ->
        call({:res_set, option, value})
    end
  end

  defp res_optname(:nameserver) do
    :res_ns
  end

  defp res_optname(:alt_nameserver) do
    :res_alt_ns
  end

  defp res_optname(:nameservers) do
    :res_ns
  end

  defp res_optname(:alt_nameservers) do
    :res_alt_ns
  end

  defp res_optname(:domain) do
    :res_domain
  end

  defp res_optname(:lookup) do
    :res_lookup
  end

  defp res_optname(:recurse) do
    :res_recurse
  end

  defp res_optname(:search) do
    :res_search
  end

  defp res_optname(:retry) do
    :res_retry
  end

  defp res_optname(:timeout) do
    :res_timeout
  end

  defp res_optname(:inet6) do
    :res_inet6
  end

  defp res_optname(:usevc) do
    :res_usevc
  end

  defp res_optname(:edns) do
    :res_edns
  end

  defp res_optname(:udp_payload_size) do
    :res_udp_payload_size
  end

  defp res_optname(:resolv_conf) do
    :res_resolv_conf
  end

  defp res_optname(:resolv_conf_name) do
    :res_resolv_conf
  end

  defp res_optname(:hosts_file) do
    :res_hosts_file
  end

  defp res_optname(:hosts_file_name) do
    :res_hosts_file
  end

  defp res_optname(_) do
    :undefined
  end

  def res_check_option(:nameserver, nSs) do
    res_check_list(nSs, &res_check_ns/1)
  end

  def res_check_option(:alt_nameserver, nSs) do
    res_check_list(nSs, &res_check_ns/1)
  end

  def res_check_option(:nameservers, nSs) do
    res_check_list(nSs, &res_check_ns/1)
  end

  def res_check_option(:alt_nameservers, nSs) do
    res_check_list(nSs, &res_check_ns/1)
  end

  def res_check_option(:domain, dom) do
    :inet_parse.visible_string(dom)
  end

  def res_check_option(:lookup, methods) do
    try do
      lists_subtract(methods, valid_lookup())
    catch
      :error, _ ->
        false
    else
      [] ->
        true

      _ ->
        false
    end
  end

  def res_check_option(:recurse, r) when r === 0 or r === 1 do
    true
  end

  def res_check_option(:recurse, r) when is_boolean(r) do
    true
  end

  def res_check_option(:search, searchList) do
    res_check_list(searchList, &res_check_search/1)
  end

  def res_check_option(:retry, n) when is_integer(n) and n > 0 do
    true
  end

  def res_check_option(:timeout, t) when is_integer(t) and t > 0 do
    true
  end

  def res_check_option(:inet6, bool) when is_boolean(bool) do
    true
  end

  def res_check_option(:usevc, bool) when is_boolean(bool) do
    true
  end

  def res_check_option(:edns, v) when v === false or v === 0 do
    true
  end

  def res_check_option(:udp_payload_size, s)
      when is_integer(s) and
             s >= 512 do
    true
  end

  def res_check_option(:resolv_conf, '') do
    true
  end

  def res_check_option(:resolv_conf, f) do
    res_check_option_absfile(f)
  end

  def res_check_option(:resolv_conf_name, '') do
    true
  end

  def res_check_option(:resolv_conf_name, f) do
    res_check_option_absfile(f)
  end

  def res_check_option(:hosts_file, '') do
    true
  end

  def res_check_option(:hosts_file, f) do
    res_check_option_absfile(f)
  end

  def res_check_option(:hosts_file_name, '') do
    true
  end

  def res_check_option(:hosts_file_name, f) do
    res_check_option_absfile(f)
  end

  def res_check_option(_, _) do
    false
  end

  defp res_check_option_absfile(f) do
    try do
      :filename.pathtype(f)
    catch
      _, _ ->
        false
    else
      :absolute ->
        true

      _ ->
        false
    end
  end

  defp res_check_list([], _Fun) do
    true
  end

  defp res_check_list([h | t], fun) do
    fun.(h) and res_check_list(t, fun)
  end

  defp res_check_list(_, _Fun) do
    false
  end

  defp res_check_ns({{a, b, c, d, e, f, g, h}, port})
       when (a ||| b ||| c ||| d ||| e ||| f ||| g ||| h) &&& ~~~65535 === 0 and
              port &&& 65535 === port do
    true
  end

  defp res_check_ns({{a, b, c, d}, port})
       when (a ||| b ||| c ||| d) &&& ~~~255 === 0 and
              port &&& 65535 === port do
    true
  end

  defp res_check_ns(_) do
    false
  end

  defp res_check_search('') do
    true
  end

  defp res_check_search(dom) do
    :inet_parse.visible_string(dom)
  end

  def socks_option(:server) do
    db_get(:socks5_server)
  end

  def socks_option(:port) do
    db_get(:socks5_port)
  end

  def socks_option(:methods) do
    db_get(:socks5_methods)
  end

  def socks_option(:noproxy) do
    db_get(:socks5_noproxy)
  end

  def gethostname() do
    db_get(:hostname)
  end

  def res_update_conf() do
    res_update(:resolv_conf, :res_resolv_conf_tm)
  end

  def res_update_hosts() do
    res_update(:hosts_file, :res_hosts_file_tm)
  end

  defp res_update(option, tagTm) do
    case db_get(tagTm) do
      :undefined ->
        :ok

      tm ->
        case times() do
          now when now >= tm + 5 ->
            res_option(option, tm)

          _ ->
            :ok
        end
    end
  end

  defp db_get(name) do
    try do
      :ets.lookup_element(:inet_db, name, 2)
    catch
      :error, :badarg ->
        :undefined
    end
  end

  def add_rr(rR) do
    call({:add_rr, rR})
  end

  def add_rr(domain, class, type, tTL, data) do
    call({:add_rr, r_dns_rr(domain: domain, class: class, type: type, ttl: tTL, data: data)})
  end

  def del_rr(domain, class, type, data) do
    call(
      {:del_rr,
       r_dns_rr(
         domain: domain,
         class: class,
         type: type,
         cnt: :_,
         tm: :_,
         ttl: :_,
         bm: :_,
         func: :_,
         data: data
       )}
    )
  end

  defp res_cache_answer(rec) do
    :lists.foreach(
      fn rR ->
        add_rr(rR)
      end,
      r_dns_rec(rec, :anlist)
    )
  end

  def getbyname(name, type) do
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
        hostent_by_domain(name, type)

      embeddedDots === 0 ->
        getbysearch(name, dot, get_searchlist(), type, {:error, :nxdomain})

      true ->
        case hostent_by_domain(name, type) do
          {:error, _} = error ->
            getbysearch(name, dot, get_searchlist(), type, error)

          other ->
            other
        end
    end
  end

  defp getbysearch(name, dot, [dom | ds], type, _) do
    case hostent_by_domain(name ++ dot ++ dom, type) do
      {:ok, _HEnt} = ok ->
        ok

      error ->
        getbysearch(name, dot, ds, type, error)
    end
  end

  defp getbysearch(_Name, _Dot, [], _Type, error) do
    error
  end

  def get_searchlist() do
    case res_option(:search) do
      [] ->
        [res_option(:domain)]

      l ->
        l
    end
  end

  defp make_hostent(name, addrs, aliases, :a) do
    r_hostent(
      h_name: name,
      h_addrtype: :inet,
      h_length: 4,
      h_addr_list: addrs,
      h_aliases: aliases
    )
  end

  defp make_hostent(name, addrs, aliases, :aaaa) do
    r_hostent(
      h_name: name,
      h_addrtype: :inet6,
      h_length: 16,
      h_addr_list: addrs,
      h_aliases: aliases
    )
  end

  defp make_hostent(name, datas, aliases, type) do
    r_hostent(
      h_name: name,
      h_addrtype: type,
      h_length: length(datas),
      h_addr_list: datas,
      h_aliases: aliases
    )
  end

  defp hostent_by_domain(domain, type) do
    :ok
    hostent_by_domain(stripdot(domain), [], [], type)
  end

  defp hostent_by_domain(domain, aliases, lAliases, type) do
    case lookup_type(domain, type) do
      [] ->
        case lookup_cname(domain) do
          [] ->
            {:error, :nxdomain}

          [cName | _] ->
            lDomain = tolower(domain)

            case :lists.member(cName, [lDomain | lAliases]) do
              true ->
                {:error, :nxdomain}

              false ->
                hostent_by_domain(cName, [domain | aliases], [lDomain | lAliases], type)
            end
        end

      addrs ->
        {:ok, make_hostent(domain, addrs, aliases, type)}
    end
  end

  defp lookup_type(domain, type) do
    for r <- lookup_rr(domain, :in, type) do
      r_dns_rr(r, :data)
    end
  end

  defp lookup_cname(domain) do
    for r <- lookup_rr(domain, :in, :cname) do
      r_dns_rr(r, :data)
    end
  end

  defp lookup_rr(domain, class, type) do
    call({:lookup_rr, domain, class, type})
  end

  def res_hostent_by_domain(domain, type, rec) do
    rRs = :lists.map(&lower_rr/1, r_dns_rec(rec, :anlist))
    res_cache_answer(r_dns_rec(rec, anlist: rRs))
    :ok
    res_hostent_by_domain(stripdot(domain), [], [], type, rRs)
  end

  defp res_hostent_by_domain(domain, aliases, lAliases, type, rRs) do
    lDomain = tolower(domain)

    case res_lookup_type(lDomain, type, rRs) do
      [] ->
        case res_lookup_type(lDomain, :cname, rRs) do
          [] ->
            {:error, :nxdomain}

          [cName | _] ->
            case :lists.member(
                   tolower(cName),
                   [lDomain | lAliases]
                 ) do
              true ->
                {:error, :nxdomain}

              false ->
                res_hostent_by_domain(cName, [domain | aliases], [lDomain | lAliases], type, rRs)
            end
        end

      addrs ->
        {:ok, make_hostent(domain, addrs, aliases, type)}
    end
  end

  defp res_lookup_type(domain, type, rRs) do
    for r <- rRs, r_dns_rr(r, :domain) === domain, r_dns_rr(r, :type) === type do
      r_dns_rr(r, :data)
    end
  end

  def gethostbyaddr(iP) do
    case dnip(iP) do
      {:ok, {iP1, hType, hLen, dnIP}} ->
        rRs =
          match_rr(
            r_dns_rr(
              domain: dnIP,
              class: :in,
              type: :ptr,
              cnt: :_,
              tm: :_,
              ttl: :_,
              bm: :_,
              func: :_,
              data: :_
            )
          )

        ent_gethostbyaddr(rRs, iP1, hType, hLen)

      error ->
        error
    end
  end

  def res_gethostbyaddr(iP, rec) do
    {:ok, {iP1, hType, hLen}} = dnt(iP)
    rRs = :lists.map(&lower_rr/1, r_dns_rec(rec, :anlist))
    res_cache_answer(r_dns_rec(rec, anlist: rRs))
    ent_gethostbyaddr(r_dns_rec(rec, :anlist), iP1, hType, hLen)
  end

  defp ent_gethostbyaddr(rRs, iP, addrType, length) do
    case rRs do
      [] ->
        {:error, :nxdomain}

      [rR | tR] ->
        cond do
          tR !== [] ->
            :ok

          true ->
            :ok
        end

        domain = r_dns_rr(rR, :data)

        h =
          r_hostent(
            h_name: domain,
            h_aliases: lookup_cname(domain),
            h_addr_list: [iP],
            h_addrtype: addrType,
            h_length: length
          )

        {:ok, h}
    end
  end

  defp dnip(iP) do
    case dnt(iP) do
      {:ok, {iP1 = {a, b, c, d}, :inet, hLen}} ->
        {:ok, {iP1, :inet, hLen, dn_in_addr_arpa(a, b, c, d)}}

      {:ok, {iP1 = {a, b, c, d, e, f, g, h}, :inet6, hLen}} ->
        {:ok, {iP1, :inet6, hLen, dn_ip6_int(a, b, c, d, e, f, g, h)}}

      _ ->
        {:error, :formerr}
    end
  end

  defp dnt(iP = {a, b, c, d})
       when (a ||| b ||| c ||| d) &&& ~~~255 === 0 do
    {:ok, {iP, :inet, 4}}
  end

  defp dnt({0, 0, 0, 0, 0, 65535, g, h})
       when is_integer(g + h) do
    a = div(g, 256)
    b = rem(g, 256)
    c = div(h, 256)
    d = rem(h, 256)
    {:ok, {{a, b, c, d}, :inet, 4}}
  end

  defp dnt(iP = {a, b, c, d, e, f, g, h})
       when (a ||| b ||| c ||| d ||| e ||| f ||| g ||| h) &&& ~~~65535 === 0 do
    {:ok, {iP, :inet6, 16}}
  end

  defp dnt(_) do
    {:error, :formerr}
  end

  def register_socket(socket, module)
      when is_port(socket) and
             is_atom(module) do
    try do
      :erlang.port_set_data(socket, module)
    catch
      :error, :badarg ->
        false
    end
  end

  def unregister_socket(socket) when is_port(socket) do
    :ok
  end

  def lookup_socket(socket) when is_port(socket) do
    try do
      :erlang.port_get_data(socket)
    catch
      :error, :badarg ->
        {:error, :closed}
    else
      module when is_atom(module) ->
        {:ok, module}

      _ ->
        {:error, :closed}
    end
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)

    case :application.get_env(:kernel, :inet_backend) do
      {:ok, flag} when flag === :inet or flag === :socket ->
        :persistent_term.put({:kernel, :inet_backend}, flag)

      _ ->
        :ok
    end

    db = :ets.new(:inet_db, [:public, :named_table])
    reset_db(db)
    cacheOpts = [:public, :bag, {:keypos, r_dns_rr(:domain)}, :named_table]
    cache = :ets.new(:inet_cache, cacheOpts)

    hostsByname =
      :ets.new(
        :inet_hosts_byname,
        [:named_table]
      )

    hostsByaddr =
      :ets.new(
        :inet_hosts_byaddr,
        [:named_table]
      )

    hostsFileByname =
      :ets.new(
        :inet_hosts_file_byname,
        [:named_table]
      )

    hostsFileByaddr =
      :ets.new(
        :inet_hosts_file_byaddr,
        [:named_table]
      )

    {:ok,
     r_state(
       db: db,
       cache: cache,
       hosts_byname: hostsByname,
       hosts_byaddr: hostsByaddr,
       hosts_file_byname: hostsFileByname,
       hosts_file_byaddr: hostsFileByaddr,
       cache_timer: init_timer()
     )}
  end

  defp reset_db(db) do
    :ets.insert(
      db,
      [
        {:hostname, []},
        {:res_ns, []},
        {:res_alt_ns, []},
        {:res_search, []},
        {:res_domain, ''},
        {:res_lookup, []},
        {:res_recurse, true},
        {:res_usevc, false},
        {:res_id, 0},
        {:res_retry, 3},
        {:res_timeout, 2000},
        {:res_inet6, false},
        {:res_edns, false},
        {:res_udp_payload_size, 1280},
        {:cache_size, 100},
        {:cache_refresh_interval, 60 * 60 * 1000},
        {:socks5_server, ''},
        {:socks5_port, 1080},
        {:socks5_methods, [:none]},
        {:socks5_noproxy, []},
        {:tcp_module, :inet_tcp},
        {:udp_module, :inet_udp},
        {:sctp_module, :inet_sctp}
      ]
    )
  end

  def handle_call(request, from, r_state(db: db) = state) do
    case request do
      {:load_hosts_file, iPNmAs} when is_list(iPNmAs) ->
        load_hosts_list(
          iPNmAs,
          r_state(state, :hosts_file_byname),
          r_state(state, :hosts_file_byaddr)
        )

        {:reply, :ok, state}

      {:add_host, {a, b, c, d} = iP, [n | as] = names}
      when (a ||| b ||| c ||| d) &&& ~~~255 === 0 and
             is_list(n) and is_list(as) ->
        do_add_host(
          r_state(state, :hosts_byname),
          r_state(state, :hosts_byaddr),
          names,
          :inet,
          iP
        )

        {:reply, :ok, state}

      {:add_host, {a, b, c, d, e, f, g, h} = iP, [n | as] = names}
      when (a ||| b ||| c ||| d ||| e ||| f ||| g ||| h) &&& ~~~65535 === 0 and
             is_list(n) and is_list(as) ->
        do_add_host(
          r_state(state, :hosts_byname),
          r_state(state, :hosts_byaddr),
          names,
          :inet6,
          iP
        )

        {:reply, :ok, state}

      {:del_host, {a, b, c, d} = iP}
      when (a ||| b ||| c ||| d) &&& ~~~255 === 0 ->
        do_del_host(r_state(state, :hosts_byname), r_state(state, :hosts_byaddr), iP)
        {:reply, :ok, state}

      {:del_host, {a, b, c, d, e, f, g, h} = iP}
      when (a ||| b ||| c ||| d ||| e ||| f ||| g ||| h) &&& ~~~65535 === 0 ->
        do_del_host(r_state(state, :hosts_byname), r_state(state, :hosts_byaddr), iP)
        {:reply, :ok, state}

      {:add_rr, rR} when elem(rR, 0) === :dns_rr ->
        :ok
        do_add_rr(rR, db, state)
        {:reply, :ok, state}

      {:del_rr, rR} when elem(rR, 0) === :dns_rr ->
        cache = r_state(state, :cache)
        :ets.match_delete(cache, rR)
        {:reply, :ok, state}

      {:lookup_rr, domain, class, type} ->
        {:reply, do_lookup_rr(domain, class, type), state}

      {:listop, opt, op, e} ->
        el = [e]

        case res_check_option(opt, el) do
          true ->
            optname = res_optname(opt)
            es = :ets.lookup_element(db, optname, 2)

            newEs =
              case op do
                :ins ->
                  [e | lists_delete(e, es)]

                :add ->
                  lists_delete(e, es) ++ el

                :del ->
                  lists_delete(e, es)
              end

            :ets.insert(db, {optname, newEs})
            {:reply, :ok, state}

          false ->
            {:reply, :error, state}
        end

      {:listdel, opt} ->
        :ets.insert(db, {res_optname(opt), []})
        {:reply, :ok, state}

      {:set_hostname, name} ->
        case :inet_parse.visible_string(name) do
          true ->
            :ets.insert(db, {:hostname, name})
            {:reply, :ok, state}

          false ->
            {:reply, :error, state}
        end

      {:res_set, :hosts_file_name = option, fname} ->
        handle_set_file(
          option,
          fname,
          :res_hosts_file_tm,
          :res_hosts_file_info,
          :undefined,
          from,
          state
        )

      {:res_set, :resolv_conf_name = option, fname} ->
        handle_set_file(
          option,
          fname,
          :res_resolv_conf_tm,
          :res_resolv_conf_info,
          :undefined,
          from,
          state
        )

      {:res_set, :hosts_file = option, fname_or_Tm} ->
        handle_set_file(
          option,
          fname_or_Tm,
          :res_hosts_file_tm,
          :res_hosts_file_info,
          fn file, bin ->
            case :inet_parse.hosts(file, {:chars, bin}) do
              {:ok, opts} ->
                [{:load_hosts_file, opts}]

              _ ->
                :error
            end
          end,
          from,
          state
        )

      {:res_set, :resolv_conf = option, fname_or_Tm} ->
        handle_set_file(
          option,
          fname_or_Tm,
          :res_resolv_conf_tm,
          :res_resolv_conf_info,
          fn file, bin ->
            case :inet_parse.resolv(file, {:chars, bin}) do
              {:ok, opts} ->
                search =
                  :lists.foldl(
                    fn
                      {:search, l}, _ ->
                        l

                      {:domain, ''}, s ->
                        s

                      {:domain, d}, _ ->
                        [d]

                      _, s ->
                        s
                    end,
                    [],
                    opts
                  )

                [
                  :del_ns,
                  :clear_search,
                  :clear_cache,
                  {:search, search}
                  | for {:nameserver, _} = opt <- opts do
                      opt
                    end
                ]

              _ ->
                :error
            end
          end,
          from,
          state
        )

      {:res_set, opt, value} ->
        case res_optname(opt) do
          :undefined ->
            {:reply, :error, state}

          optname ->
            case res_check_option(opt, value) do
              true ->
                :ets.insert(db, {optname, value})
                {:reply, :ok, state}

              false ->
                {:reply, :error, state}
            end
        end

      {:set_resolv_conf_tm, tM} ->
        :ets.insert(db, {:res_resolv_conf_tm, tM})
        {:reply, :ok, state}

      {:set_hosts_file_tm, tM} ->
        :ets.insert(db, {:res_hosts_file_tm, tM})
        {:reply, :ok, state}

      {:set_socks_server, {a, b, c, d}}
      when (a ||| b ||| c ||| d) &&& ~~~255 === 0 ->
        :ets.insert(db, {:socks5_server, {a, b, c, d}})
        {:reply, :ok, state}

      {:set_socks_port, port} when is_integer(port) ->
        :ets.insert(db, {:socks5_port, port})
        {:reply, :ok, state}

      {:add_socks_methods, ls} ->
        as = :ets.lookup_element(db, :socks5_methods, 2)
        as1 = lists_subtract(as, ls)
        :ets.insert(db, {:socks5_methods, as1 ++ ls})
        {:reply, :ok, state}

      {:del_socks_methods, ls} ->
        as = :ets.lookup_element(db, :socks5_methods, 2)
        as1 = lists_subtract(as, ls)

        case :lists.member(:none, as1) do
          false ->
            :ets.insert(db, {:socks5_methods, as1 ++ [:none]})

          true ->
            :ets.insert(db, {:socks5_methods, as1})
        end

        {:reply, :ok, state}

      :del_socks_methods ->
        :ets.insert(db, {:socks5_methods, [:none]})
        {:reply, :ok, state}

      {:add_socks_noproxy, {{a, b, c, d}, {mA, mB, mC, mD}}}
      when (a ||| b ||| c ||| d) &&& ~~~255 === 0 and
             (mA ||| mB ||| mC ||| mD) &&& ~~~255 === 0 ->
        as = :ets.lookup_element(db, :socks5_noproxy, 2)

        :ets.insert(
          db,
          {:socks5_noproxy, as ++ [{{a, b, c, d}, {mA, mB, mC, mD}}]}
        )

        {:reply, :ok, state}

      {:del_socks_noproxy, {a, b, c, d} = iP}
      when (a ||| b ||| c ||| d) &&& ~~~255 === 0 ->
        as = :ets.lookup_element(db, :socks5_noproxy, 2)

        :ets.insert(
          db,
          {:socks5_noproxy, lists_keydelete(iP, 1, as)}
        )

        {:reply, :ok, state}

      {:set_tcp_module, mod} when is_atom(mod) ->
        :ets.insert(db, {:tcp_module, mod})
        {:reply, :ok, state}

      {:set_udp_module, mod} when is_atom(mod) ->
        :ets.insert(db, {:udp_module, mod})
        {:reply, :ok, state}

      {:set_sctp_module, fam} when is_atom(fam) ->
        :ets.insert(db, {:sctp_module, fam})
        {:reply, :ok, state}

      {:set_cache_size, size}
      when is_integer(size) and
             size >= 0 ->
        :ets.insert(db, {:cache_size, size})
        {:reply, :ok, state}

      {:set_cache_refresh, time}
      when is_integer(time) and
             time > 0 ->
        time1 = div(time + 999, 1000) * 1000
        :ets.insert(db, {:cache_refresh_interval, time1})
        _ = stop_timer(r_state(state, :cache_timer))
        {:reply, :ok, r_state(state, cache_timer: init_timer())}

      :clear_hosts ->
        :ets.delete_all_objects(r_state(state, :hosts_byname))
        :ets.delete_all_objects(r_state(state, :hosts_byaddr))
        {:reply, :ok, state}

      :clear_cache ->
        :ets.match_delete(r_state(state, :cache), :_)
        {:reply, :ok, state}

      :reset ->
        reset_db(db)
        _ = stop_timer(r_state(state, :cache_timer))
        {:reply, :ok, r_state(state, cache_timer: init_timer())}

      {:add_rc_list, list} ->
        handle_rc_list(list, from, state)

      :stop ->
        {:stop, :normal, :ok, state}

      _ ->
        {:reply, :error, state}
    end
  end

  def handle_cast(_Msg, state) do
    {:noreply, state}
  end

  def handle_info(:refresh_timeout, state) do
    do_refresh_cache(r_state(state, :cache))
    {:noreply, r_state(state, cache_timer: init_timer())}
  end

  def handle_info(_Info, state) do
    {:noreply, state}
  end

  def terminate(_Reason, state) do
    _ = stop_timer(r_state(state, :cache_timer))
    :ok
  end

  defp handle_set_file(option, tm, tagTm, tagInfo, parseFun, from, r_state(db: db) = state)
       when is_integer(tm) do
    try do
      :ets.lookup_element(db, tagTm, 2)
    catch
      :error, :badarg ->
        {:reply, :ok, state}
    else
      ^tm ->
        file = :ets.lookup_element(db, res_optname(option), 2)
        finfo = :ets.lookup_element(db, tagInfo, 2)
        handle_update_file(finfo, file, tagTm, tagInfo, parseFun, from, state)

      _ ->
        {:reply, :ok, state}
    end
  end

  defp handle_set_file(option, fname, tagTm, tagInfo, parseFun, from, r_state(db: db) = state) do
    case res_check_option(option, fname) do
      true when fname === '' ->
        :ets.insert(db, {res_optname(option), fname})
        :ets.delete(db, tagInfo)
        :ets.delete(db, tagTm)
        handle_set_file(parseFun, fname, <<>>, from, state)

      true when parseFun === :undefined ->
        file = :filename.flatten(fname)
        :ets.insert(db, {res_optname(option), file})
        :ets.insert(db, {tagInfo, :undefined})
        timeZero = times() - (5 + 1)
        :ets.insert(db, {tagTm, timeZero})
        {:reply, :ok, state}

      true ->
        file = :filename.flatten(fname)
        :ets.insert(db, {res_optname(option), file})
        handle_update_file(:undefined, file, tagTm, tagInfo, parseFun, from, state)

      false ->
        {:reply, :error, state}
    end
  end

  defp handle_set_file(parseFun, file, bin, from, state) do
    case parseFun.(file, bin) do
      :error ->
        {:reply, :error, state}

      opts ->
        handle_rc_list(opts, from, state)
    end
  end

  defp handle_update_file(finfo, file, tagTm, tagInfo, parseFun, from, r_state(db: db) = state) do
    case :erl_prim_loader.read_file_info(file) do
      {:ok, ^finfo} ->
        {:reply, :ok, state}

      {:ok, finfo_1} ->
        :ets.insert(db, {tagInfo, finfo_1})
        :ets.insert(db, {tagTm, times()})

        bin =
          case :erl_prim_loader.get_file(file) do
            {:ok, b, _} ->
              b

            _ ->
              <<>>
          end

        handle_set_file(parseFun, file, bin, from, state)

      _ ->
        :ets.insert(db, {tagInfo, :undefined})
        :ets.insert(db, {tagTm, times()})
        handle_set_file(parseFun, file, <<>>, from, state)
    end
  end

  defp do_add_host(byname, byaddr, names, type, iP) do
    nms =
      for nm <- names do
        tolower(nm)
      end

    add_ip_bynms(byname, type, iP, nms, names)
    key = {type, iP}

    try do
      :ets.lookup_element(byaddr, key, 2)
    catch
      :error, :badarg ->
        :ok
    else
      names_0 ->
        nmsSet =
          :lists.foldl(
            fn nm, set ->
              :maps.put(nm, [], set)
            end,
            %{},
            nms
          )

        del_ip_bynms(
          byname,
          type,
          iP,
          for nm <-
                (for name <- names_0 do
                   tolower(name)
                 end),
              not :maps.is_key(nm, nmsSet) do
            nm
          end
        )
    end

    :ets.insert(byaddr, {key, names})
    :ok
  end

  defp do_del_host(byname, byaddr, iP) do
    fam = inet_family(iP)
    key = {fam, iP}

    try do
      :ets.lookup_element(byaddr, key, 2)
    catch
      :error, :badarg ->
        :ok
    else
      names ->
        del_ip_bynms(
          byname,
          fam,
          iP,
          for name <- names do
            tolower(name)
          end
        )

        true = :ets.delete(byaddr, key)
        :ok
    end
  end

  defp add_ip_bynms(byname, fam, iP, nms, names) do
    :lists.foreach(
      fn nm ->
        key = {fam, nm}

        case :ets.lookup(byname, key) do
          [{_Key, [^iP | _] = iPs, _Names_1}] ->
            true = :ets.insert(byname, {key, iPs, names})

          [{_Key, iPs, names_0}] ->
            case :lists.member(iP, iPs) do
              true ->
                :ok

              false ->
                true =
                  :ets.insert(
                    byname,
                    {key, iPs ++ [iP], names_0}
                  )
            end

          [] ->
            true = :ets.insert(byname, {key, [iP], names})
        end
      end,
      nms
    )
  end

  defp del_ip_bynms(byname, fam, iP, nms) do
    :lists.foreach(
      fn nm ->
        key = {fam, nm}

        case :ets.lookup(byname, key) do
          [{_Key, [^iP], _Names}] ->
            true = :ets.delete(byname, key)

          [{_Key, iPs_0, names_0}] ->
            case :lists.member(iP, iPs_0) do
              true ->
                iPs = :lists.delete(iP, iPs_0)
                true = :ets.insert(byname, {key, iPs, names_0})

              false ->
                :ok
            end

          [] ->
            :ok
        end
      end,
      nms
    )
  end

  defp inet_family(t) when tuple_size(t) === 4 do
    :inet
  end

  defp inet_family(t) when tuple_size(t) === 8 do
    :inet6
  end

  defp load_hosts_list(hosts, byname, byaddr) do
    {byaddrMap, bynameMap} = load_hosts_list(hosts)

    :ets.insert(
      byaddr,
      for {addr, namesR} <- :maps.to_list(byaddrMap) do
        {addr, :lists.reverse(namesR)}
      end
    )

    :ets.insert(
      byname,
      for {fam_Nm, {iPsR, names}} <- :maps.to_list(bynameMap) do
        {fam_Nm, :lists.reverse(iPsR), names}
      end
    )

    ets_clean_map_keys(byaddr, byaddrMap)
    ets_clean_map_keys(byname, bynameMap)
  end

  defp load_hosts_list(hosts) do
    load_hosts_list_byaddr(hosts, %{}, [])
  end

  defp load_hosts_list_byaddr([], byaddrMap, addrs) do
    load_hosts_list_byname(:lists.reverse(addrs), byaddrMap, %{})
  end

  defp load_hosts_list_byaddr([{iP, name, aliases} | hosts], byaddrMap, addrs) do
    addr = {inet_family(iP), iP}

    case byaddrMap do
      %{^addr => namesR} ->
        load_hosts_list_byaddr(
          hosts,
          %{byaddrMap | addr => :lists.reverse(aliases, [name | namesR])},
          addrs
        )

      %{} ->
        load_hosts_list_byaddr(
          hosts,
          Map.put(
            byaddrMap,
            addr,
            :lists.reverse(
              aliases,
              [name]
            )
          ),
          [addr | addrs]
        )
    end
  end

  defp load_hosts_list_byname([], byaddrMap, bynameMap) do
    {byaddrMap, bynameMap}
  end

  defp load_hosts_list_byname([{fam, iP} = addr | addrs], byaddrMap, bynameMap) do
    names = :lists.reverse(:maps.get(addr, byaddrMap))

    load_hosts_list_byname(
      addrs,
      byaddrMap,
      load_hosts_list_byname(fam, iP, bynameMap, names, names)
    )
  end

  defp load_hosts_list_byname(_Fam, _IP, bynameMap, _Names_0, []) do
    bynameMap
  end

  defp load_hosts_list_byname(fam, iP, bynameMap, names_0, [name | names]) do
    key = {fam, tolower(name)}

    case bynameMap do
      %{^key => {iPsR, names_1}} ->
        load_hosts_list_byname(
          fam,
          iP,
          %{bynameMap | key => {[iP | iPsR], names_1}},
          names_0,
          names
        )

      %{} ->
        load_hosts_list_byname(fam, iP, Map.put(bynameMap, key, {[iP], names_0}), names_0, names)
    end
  end

  defp ets_clean_map_keys(tab, map) do
    true = :ets.safe_fixtable(tab, true)
    ets_clean_map_keys(tab, map, :ets.first(tab))
    true = :ets.safe_fixtable(tab, false)
    :ok
  end

  defp ets_clean_map_keys(_Tab, _Map, :"$end_of_table") do
    :ok
  end

  defp ets_clean_map_keys(tab, map, key) do
    case :maps.is_key(key, map) do
      true ->
        ets_clean_map_keys(tab, map, :ets.next(tab, key))

      false ->
        true = :ets.delete(tab, key)
        ets_clean_map_keys(tab, map, :ets.next(tab, key))
    end
  end

  defp handle_rc_list([], _From, state) do
    {:reply, :ok, state}
  end

  defp handle_rc_list([opt | opts], from, state) do
    case rc_opt_req(opt) do
      :undefined ->
        {:reply, {:error, {:badopt, opt}}, state}

      req ->
        case handle_calls(req, from, state) do
          {:reply, :ok, newState} ->
            handle_rc_list(opts, from, newState)

          result ->
            result
        end
    end
  end

  defp handle_rc_list(_, _From, state) do
    {:reply, :error, state}
  end

  defp handle_calls([], _From, state) do
    {:reply, :ok, state}
  end

  defp handle_calls([req | reqs], from, state) do
    case handle_call(req, from, state) do
      {:reply, :ok, newState} ->
        handle_calls(reqs, from, newState)

      {:reply, _, newState} ->
        {:reply, :error, newState}
    end
  end

  defp handle_calls(req, from, state) do
    handle_call(req, from, state)
  end

  defp rc_opt_req({:nameserver, ns}) do
    {:listop, :nameservers, :add, {ns, 53}}
  end

  defp rc_opt_req({:nameserver, ns, port}) do
    {:listop, :nameservers, :add, {ns, port}}
  end

  defp rc_opt_req({:alt_nameserver, ns}) do
    {:listop, :alt_nameservers, :add, {ns, 53}}
  end

  defp rc_opt_req({:alt_nameserver, ns, port}) do
    {:listop, :alt_nameservers, :add, {ns, port}}
  end

  defp rc_opt_req({:socks5_noproxy, iP, mask}) do
    {:add_socks_noproxy, {iP, mask}}
  end

  defp rc_opt_req({:search, ds}) when is_list(ds) do
    try do
      for d <- ds do
        {:listop, :search, :add, d}
      end
    catch
      :error, _ ->
        :undefined
    end
  end

  defp rc_opt_req({:host, iP, aliases}) do
    {:add_host, iP, aliases}
  end

  defp rc_opt_req({:load_hosts_file, _} = req) do
    req
  end

  defp rc_opt_req({:lookup, ls}) do
    try do
      {:res_set, :lookup, translate_lookup(ls)}
    catch
      :error, _ ->
        :undefined
    end
  end

  defp rc_opt_req({name, arg}) do
    case rc_reqname(name) do
      :undefined ->
        case is_res_set(name) do
          true ->
            {:res_set, name, arg}

          false ->
            :undefined
        end

      req ->
        {req, arg}
    end
  end

  defp rc_opt_req(:del_ns) do
    {:listdel, :nameservers}
  end

  defp rc_opt_req(:del_alt_ns) do
    {:listdel, :alt_nameservers}
  end

  defp rc_opt_req(:clear_ns) do
    [{:listdel, :nameservers}, {:listdel, :alt_nameservers}]
  end

  defp rc_opt_req(:clear_search) do
    {:listdel, :search}
  end

  defp rc_opt_req(opt) when is_atom(opt) do
    case is_reqname(opt) do
      true ->
        opt

      false ->
        :undefined
    end
  end

  defp rc_opt_req(_) do
    :undefined
  end

  defp rc_reqname(:socks5_server) do
    :set_socks_server
  end

  defp rc_reqname(:socks5_port) do
    :set_socks_port
  end

  defp rc_reqname(:socks5_methods) do
    :set_socks_methods
  end

  defp rc_reqname(:cache_refresh) do
    :set_cache_refresh
  end

  defp rc_reqname(:cache_size) do
    :set_cache_size
  end

  defp rc_reqname(:udp) do
    :set_udp_module
  end

  defp rc_reqname(:sctp) do
    :set_sctp_module
  end

  defp rc_reqname(:tcp) do
    :set_tcp_module
  end

  defp rc_reqname(_) do
    :undefined
  end

  defp is_res_set(:domain) do
    true
  end

  defp is_res_set(:lookup) do
    true
  end

  defp is_res_set(:timeout) do
    true
  end

  defp is_res_set(:retry) do
    true
  end

  defp is_res_set(:inet6) do
    true
  end

  defp is_res_set(:usevc) do
    true
  end

  defp is_res_set(:edns) do
    true
  end

  defp is_res_set(:udp_payload_size) do
    true
  end

  defp is_res_set(:resolv_conf) do
    true
  end

  defp is_res_set(:hosts_file) do
    true
  end

  defp is_res_set(_) do
    false
  end

  defp is_reqname(:reset) do
    true
  end

  defp is_reqname(:clear_cache) do
    true
  end

  defp is_reqname(:clear_hosts) do
    true
  end

  defp is_reqname(_) do
    false
  end

  defp do_add_rr(rR, db, state) do
    cacheDb = r_state(state, :cache)
    tM = times()

    case alloc_entry(db, cacheDb, tM) do
      true ->
        cache_rr(db, cacheDb, r_dns_rr(rR, tm: tM, cnt: tM))

      _ ->
        false
    end
  end

  defp cache_rr(_Db, cache, rR) do
    :ets.match_delete(
      cache,
      r_dns_rr(rR, cnt: :_, tm: :_, ttl: :_, bm: :_, func: :_)
    )

    :ets.insert(cache, rR)
  end

  defp times() do
    :erlang.monotonic_time(:second)
  end

  defp do_lookup_rr(domain, class, type) do
    match_rr(
      r_dns_rr(
        domain: tolower(domain),
        class: class,
        type: type,
        cnt: :_,
        tm: :_,
        ttl: :_,
        bm: :_,
        func: :_,
        data: :_
      )
    )
  end

  defp match_rr(rR) do
    filter_rr(:ets.match_object(:inet_cache, rR), times())
  end

  defp filter_rr([rR | rRs], time) when r_dns_rr(rR, :ttl) === 0 do
    :ets.match_delete(:inet_cache, rR)
    [rR | filter_rr(rRs, time)]
  end

  defp filter_rr([rR | rRs], time)
       when r_dns_rr(rR, :tm) + r_dns_rr(rR, :ttl) < time do
    :ets.match_delete(:inet_cache, rR)
    filter_rr(rRs, time)
  end

  defp filter_rr([rR | rRs], time) do
    :ets.match_delete(:inet_cache, rR)
    :ets.insert(:inet_cache, r_dns_rr(rR, cnt: time))
    [rR | filter_rr(rRs, time)]
  end

  defp filter_rr([], _Time) do
    []
  end

  defp lower_rr(r_dns_rr(domain: domain) = rR) when is_list(domain) do
    r_dns_rr(rR, domain: tolower(domain))
  end

  defp lower_rr(rR) do
    rR
  end

  def tolower([]) do
    []
  end

  def tolower([c | cs]) when is_integer(c) do
    cond do
      c >= ?A and c <= ?Z ->
        [c - ?A + ?a | tolower(cs)]

      true ->
        [c | tolower(cs)]
    end
  end

  defp dn_ip6_int(a, b, c, d, e, f, g, h) do
    dnib(h) ++
      dnib(g) ++ dnib(f) ++ dnib(e) ++ dnib(d) ++ dnib(c) ++ dnib(b) ++ dnib(a) ++ 'ip6.int'
  end

  defp dn_in_addr_arpa(a, b, c, d) do
    :erlang.integer_to_list(d) ++
      '.' ++
      :erlang.integer_to_list(c) ++
      '.' ++ :erlang.integer_to_list(b) ++ '.' ++ :erlang.integer_to_list(a) ++ '.in-addr.arpa'
  end

  defp dnib(x) do
    [hex(x), ?., hex(x >>> 4), ?., hex(x >>> 8), ?., hex(x >>> 12), ?.]
  end

  defp hex(x) do
    x4 = x &&& 15

    cond do
      x4 < 10 ->
        x4 + ?0

      true ->
        x4 - 10 + ?a
    end
  end

  defp stripdot(name) do
    case stripdot_1(name) do
      false ->
        name

      n ->
        n
    end
  end

  defp stripdot_1([?.]) do
    []
  end

  defp stripdot_1([]) do
    false
  end

  defp stripdot_1([h | t]) do
    case stripdot_1(t) do
      false ->
        false

      n ->
        [h | n]
    end
  end

  defp init_timer() do
    :erlang.send_after(cache_refresh(), self(), :refresh_timeout)
  end

  defp stop_timer(:undefined) do
    :undefined
  end

  defp stop_timer(timer) do
    :erlang.cancel_timer(timer)
  end

  defp cache_refresh() do
    case db_get(:cache_refresh_interval) do
      :undefined ->
        60 * 60 * 1000

      val ->
        val
    end
  end

  defp do_refresh_cache(cacheDb) do
    now = times()
    do_refresh_cache(:ets.first(cacheDb), cacheDb, now, now)
  end

  defp do_refresh_cache(:"$end_of_table", _, _, oldestT) do
    oldestT
  end

  defp do_refresh_cache(key, cacheDb, now, oldestT) do
    fun = fn
      rR, t when r_dns_rr(rR, :tm) + r_dns_rr(rR, :ttl) < now ->
        :ets.match_delete(cacheDb, rR)
        t

      r_dns_rr(cnt: c), t when c < t ->
        c

      _, t ->
        t
    end

    next = :ets.next(cacheDb, key)
    oldT = :lists.foldl(fun, oldestT, :ets.lookup(cacheDb, key))
    do_refresh_cache(next, cacheDb, now, oldT)
  end

  defp alloc_entry(db, cacheDb, tM) do
    curSize = :ets.info(cacheDb, :size)

    case :ets.lookup_element(db, :cache_size, 2) do
      size when size <= curSize and size > 0 ->
        alloc_entry(cacheDb, curSize, tM, trunc(size * 0.1) + 1)

      size when size <= 0 ->
        false

      _Size ->
        true
    end
  end

  defp alloc_entry(cacheDb, oldSize, tM, n) do
    oldestTM = do_refresh_cache(cacheDb)

    case :ets.info(cacheDb, :size) do
      ^oldSize ->
        delete_n_oldest(cacheDb, tM, oldestTM, n)

      _ ->
        true
    end
  end

  defp delete_n_oldest(cacheDb, tM, oldestTM, n) do
    delTM = trunc((tM - oldestTM) * 0.3) + oldestTM
    delete_older(cacheDb, delTM, n) !== 0
  end

  defp delete_older(cacheDb, tM, n) do
    delete_older(:ets.first(cacheDb), cacheDb, tM, n, 0)
  end

  defp delete_older(:"$end_of_table", _, _, _, m) do
    m
  end

  defp delete_older(_, _, _, n, m) when n <= m do
    m
  end

  defp delete_older(domain, cacheDb, tM, n, m) do
    next = :ets.next(cacheDb, domain)

    fun = fn
      rR, mM when r_dns_rr(rR, :cnt) <= tM ->
        :ets.match_delete(cacheDb, rR)
        mM + 1

      _, mM ->
        mM
    end

    m1 = :lists.foldl(fun, m, :ets.lookup(cacheDb, domain))
    delete_older(next, cacheDb, tM, n, m1)
  end

  defp lists_delete(_, []) do
    []
  end

  defp lists_delete(e, [e | es]) do
    lists_delete(e, es)
  end

  defp lists_delete(e, [x | es]) do
    [x | lists_delete(e, es)]
  end

  defp lists_subtract(as0, bs) do
    :lists.foldl(
      fn e, as ->
        lists_delete(e, as)
      end,
      as0,
      bs
    )
  end

  defp lists_keydelete(_, _, []) do
    []
  end

  defp lists_keydelete(k, n, [t | ts])
       when :erlang.element(
              n,
              t
            ) === k do
    lists_keydelete(k, n, ts)
  end

  defp lists_keydelete(k, n, [x | ts]) do
    [x | lists_keydelete(k, n, ts)]
  end
end
