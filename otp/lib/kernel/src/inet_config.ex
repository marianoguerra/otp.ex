defmodule :m_inet_config do
  use Bitwise
  import :lists, only: [foreach: 2, member: 2, reverse: 1]
  require Record

  Record.defrecord(:r_hostent, :hostent,
    h_name: :undefined,
    h_aliases: [],
    h_addrtype: :undefined,
    h_length: :undefined,
    h_addr_list: []
  )

  def init() do
    set_hostname()
    osType = :os.type()
    do_load_resolv(osType, erl_dist_mode())

    case osType do
      {:unix, type} ->
        cond do
          type === :linux ->
            case :inet_db.res_option(:domain) do
              '' ->
                case :inet.gethostbyname(:inet_db.gethostname()) do
                  {:ok, r_hostent(h_name: [])} ->
                    :ok

                  {:ok, r_hostent(h_name: hostName)} ->
                    set_hostname({:ok, hostName})

                  _ ->
                    :ok
                end

              _ ->
                :ok
            end

          true ->
            :ok
        end

        add_dns_lookup(:inet_db.res_option(:lookup))

      _ ->
        :ok
    end

    {rcFile, cfgFiles, cfgList} = read_rc()

    :lists.foreach(
      fn
        {:file, :hosts, file} ->
          load_hosts(file, :unix)

        {:file, func, file} ->
          load_resolv(file, func)

        {:registry, :win32} ->
          case osType do
            {:win32, winType} ->
              win32_load_from_registry(winType)

            _ ->
              :erlang.error('cannot read win32 system registry~n', [])
          end
      end,
      cfgFiles
    )

    case :inet_db.add_rc_list(cfgList) do
      :ok ->
        :ok

      _ ->
        :erlang.error('syntax error in ~ts~n', [rcFile])
    end

    case osType do
      {:unix, _} ->
        etc = :os.getenv('ERL_INET_ETC_DIR', '/etc')

        case :inet_db.res_option(:resolv_conf) do
          :undefined ->
            :inet_db.res_option(
              :resolv_conf_name,
              :filename.join(etc, 'resolv.conf')
            )

          _ ->
            :ok
        end

        case :inet_db.res_option(:hosts_file) do
          :undefined ->
            :inet_db.res_option(
              :hosts_file_name,
              :filename.join(etc, 'hosts')
            )

          _ ->
            :ok
        end

      _ ->
        :ok
    end
  end

  defp erl_dist_mode() do
    case :init.get_argument(:sname) do
      {:ok, [[_SName]]} ->
        :shortnames

      _ ->
        case :init.get_argument(:name) do
          {:ok, [[_Name]]} ->
            :longnames

          _ ->
            :nonames
        end
    end
  end

  def do_load_resolv({:unix, type}, :longnames) do
    etc = :os.getenv('ERL_INET_ETC_DIR', '/etc')
    load_resolv(:filename.join(etc, 'resolv.conf'), :resolv)

    case type do
      :freebsd ->
        load_resolv(:filename.join(etc, 'host.conf'), :host_conf_freebsd)

      :"bsd/os" ->
        load_resolv(:filename.join(etc, 'irs.conf'), :host_conf_bsdos)

      :sunos ->
        case :os.version() do
          {major, _, _} when major >= 5 ->
            load_resolv(:filename.join(etc, 'nsswitch.conf'), :nsswitch_conf)

          _ ->
            :ok
        end

      :netbsd ->
        case :os.version() do
          {major, minor, _} when major >= 1 and minor >= 4 ->
            load_resolv(:filename.join(etc, 'nsswitch.conf'), :nsswitch_conf)

          _ ->
            :ok
        end

      :linux ->
        case load_resolv(
               :filename.join(etc, 'host.conf'),
               :host_conf_linux
             ) do
          :ok ->
            :ok

          _ ->
            load_resolv(:filename.join(etc, 'nsswitch.conf'), :nsswitch_conf)
        end

      _ ->
        :ok
    end

    :inet_db.set_lookup([:native])
  end

  def do_load_resolv({:win32, type}, :longnames) do
    win32_load_from_registry(type)
    :inet_db.set_lookup([:native])
  end

  def do_load_resolv(_, _) do
    :inet_db.set_lookup([:native])
  end

  defp add_dns_lookup(l) do
    case :lists.member(:dns, l) do
      true ->
        :ok

      _ ->
        case :application.get_env(
               :kernel,
               :inet_dns_when_nis
             ) do
          {:ok, true} ->
            add_dns_lookup(l, [])

          _ ->
            :ok
        end
    end
  end

  defp add_dns_lookup([:yp | t], acc) do
    add_dns_lookup(t, [:yp, :dns | acc])
  end

  defp add_dns_lookup([h | t], acc) do
    add_dns_lookup(t, [h | acc])
  end

  defp add_dns_lookup([], acc) do
    :inet_db.set_lookup(reverse(acc))
  end

  defp set_hostname() do
    case :inet_udp.open(0, []) do
      {:ok, u} ->
        res = :inet.gethostname(u)
        :inet_udp.close(u)
        set_hostname(res)

      _ ->
        set_hostname({:ok, []})
    end
  end

  defp set_hostname({:ok, name}) when length(name) > 0 do
    {host, domain} =
      :lists.splitwith(
        fn
          ?. ->
            false

          _ ->
            true
        end,
        name
      )

    :inet_db.set_hostname(host)
    set_search_dom(domain)
  end

  defp set_hostname({:ok, []}) do
    :inet_db.set_hostname('nohost')
    set_search_dom('nodomain')
  end

  defp set_search_dom([?. | domain]) do
    :inet_db.set_domain(domain)
    :inet_db.ins_search(domain)
    :ok
  end

  defp set_search_dom([]) do
    :ok
  end

  defp set_search_dom(domain) do
    :inet_db.set_domain(domain)
    :inet_db.ins_search(domain)
    :ok
  end

  defp load_resolv(file, func) do
    case get_file(file) do
      {:ok, bin} ->
        case apply(:inet_parse, func, [file, {:chars, bin}]) do
          {:ok, ls} ->
            :inet_db.add_rc_list(ls)

          {:error, reason} ->
            :erlang.error('parse error in file ~ts: ~p', [file, reason])
        end

      error ->
        warning('file not found ~ts: ~p~n', [file, error])
    end
  end

  defp load_hosts(file, os) do
    case get_file(file) do
      {:ok, bin} ->
        case :inet_parse.hosts(file, {:chars, bin}) do
          {:ok, ls} ->
            foreach(
              fn {iP, name, aliases} ->
                :inet_db.add_host(iP, [name | aliases])
              end,
              ls
            )

          {:error, reason} ->
            :erlang.error('parse error in file ~ts: ~p', [file, reason])
        end

      error ->
        case os do
          :unix ->
            :erlang.error('file not found ~ts: ~p~n', [file, error])

          _ ->
            :ok
        end
    end
  end

  defp win32_load_from_registry(type) do
    tcpReg = :os.getenv('ERL_INET_ETC_DIR', '')
    {:ok, reg} = :win32reg.open([:read])

    {tcpIp, hFileKey} =
      case type do
        :nt ->
          case tcpReg do
            [] ->
              {'\\hklm\\system\\CurrentControlSet\\Services\\TcpIp\\Parameters', 'DataBasePath'}

            other ->
              {other, 'DataBasePath'}
          end

        :windows ->
          case tcpReg do
            [] ->
              {'\\hklm\\system\\CurrentControlSet\\Services\\VxD\\MSTCP', 'LMHostFile'}

            other ->
              {other, 'LMHostFile'}
          end
      end

    result =
      case :win32reg.change_key(reg, tcpIp) do
        :ok ->
          win32_load1(reg, type, hFileKey)

        {:error, _Reason} ->
          :erlang.error('Failed to locate TCP/IP parameters (is TCP/IP installed)?', [])
      end

    :win32reg.close(reg)
    result
  end

  defp win32_load1(reg, type, hFileKey) do
    names = [hFileKey, 'Domain', 'DhcpDomain', 'EnableDNS', 'NameServer', 'SearchList']

    case win32_get_strings(reg, names) do
      [dBPath0, domain, dhcpDomain, _EnableDNS, nameServers0, search] ->
        :inet_db.set_domain(
          case domain do
            '' ->
              dhcpDomain

            _ ->
              domain
          end
        )

        nameServers = win32_split_line(nameServers0, type)

        addNs = fn addr ->
          case :inet_parse.address(addr) do
            {:ok, address} ->
              :inet_db.add_ns(address)

            {:error, _} ->
              :erlang.error('Bad TCP/IP address in registry', [])
          end
        end

        foreach(addNs, nameServers)
        searches0 = win32_split_line(search, type)

        searches =
          case member(domain, searches0) do
            true ->
              searches0

            false ->
              [domain | searches0]
          end

        foreach(
          fn d ->
            :inet_db.add_search(d)
          end,
          searches
        )

        cond do
          type === :nt ->
            dBPath = :win32reg.expand(dBPath0)
            load_hosts(:filename.join(dBPath, 'hosts'), :nt)

          type === :windows ->
            load_hosts(:filename.join(dBPath0, ''), :windows)
        end

        true

      {:error, _Reason} ->
        :erlang.error('Failed to read TCP/IP parameters from registry', [])
    end
  end

  defp win32_split_line(line, :nt) do
    :inet_parse.split_line(line)
  end

  defp win32_split_line(line, :windows) do
    :string.lexemes(line, ',')
  end

  defp win32_get_strings(reg, names) do
    win32_get_strings(reg, names, [])
  end

  defp win32_get_strings(reg, [name | rest], result) do
    case :win32reg.value(reg, name) do
      {:ok, value} when is_list(value) ->
        win32_get_strings(reg, rest, [value | result])

      {:ok, _NotString} ->
        {:error, :not_string}

      {:error, _Reason} ->
        win32_get_strings(reg, rest, ['' | result])
    end
  end

  defp win32_get_strings(_, [], result) do
    :lists.reverse(result)
  end

  defp read_rc() do
    {rcFile, cfgList} = read_inetrc()

    case extract_cfg_files(cfgList, [], []) do
      {cfgFiles, cfgList1} ->
        {rcFile, cfgFiles, cfgList1}

      :error ->
        {:error, [], []}
    end
  end

  defp extract_cfg_files([e = {:file, type, _File} | es], cfgFiles, cfgList) do
    extract_cfg_files1(type, e, es, cfgFiles, cfgList)
  end

  defp extract_cfg_files([e = {:registry, type} | es], cfgFiles, cfgList) do
    extract_cfg_files1(type, e, es, cfgFiles, cfgList)
  end

  defp extract_cfg_files([e | es], cfgFiles, cfgList) do
    extract_cfg_files(es, cfgFiles, [e | cfgList])
  end

  defp extract_cfg_files([], cfgFiles, cfgList) do
    {reverse(cfgFiles), reverse(cfgList)}
  end

  defp extract_cfg_files1(type, e, es, cfgFiles, cfgList) do
    case valid_type(type) do
      true ->
        extract_cfg_files(es, [e | cfgFiles], cfgList)

      false ->
        :erlang.error('invalid config value ~w in inetrc~n', [type])
        :error
    end
  end

  defp valid_type(:resolv) do
    true
  end

  defp valid_type(:host_conf_freebsd) do
    true
  end

  defp valid_type(:host_conf_bsdos) do
    true
  end

  defp valid_type(:host_conf_linux) do
    true
  end

  defp valid_type(:nsswitch_conf) do
    true
  end

  defp valid_type(:hosts) do
    true
  end

  defp valid_type(:win32) do
    true
  end

  defp valid_type(_) do
    false
  end

  defp read_inetrc() do
    case :application.get_env(:inetrc) do
      {:ok, file} ->
        try_get_rc(file)

      _ ->
        case :os.getenv('ERL_INETRC') do
          false ->
            {:nofile, []}

          file ->
            try_get_rc(file)
        end
    end
  end

  defp try_get_rc(file) do
    case get_rc(file) do
      :error ->
        {:nofile, []}

      ls ->
        {file, ls}
    end
  end

  defp get_rc(file) do
    case get_file(file) do
      {:ok, bin} ->
        case parse_inetrc(bin) do
          {:ok, ls} ->
            ls

          _Error ->
            :erlang.error('parse error in ~ts~n', [file])
            :error
        end

      _Error ->
        :erlang.error('file ~ts not found~n', [file])
        :error
    end
  end

  defp get_file(file) do
    case :erl_prim_loader.get_file(file) do
      {:ok, bin, _} ->
        {:ok, bin}

      error ->
        error
    end
  end

  defp error(fmt, args) do
    :error_logger.error_msg('inet_config: ' ++ fmt, args)
  end

  defp warning(fmt, args) do
    case :application.get_env(:kernel, :inet_warnings) do
      {:ok, :on} ->
        :error_logger.info_msg('inet_config:' ++ fmt, args)

      _ ->
        :ok
    end
  end

  defp parse_inetrc(bin) do
    case file_binary_to_list(bin) do
      {:ok, string} ->
        parse_inetrc(string ++ '\n', 1, [])

      :error ->
        {:error, :bad_encoding}
    end
  end

  defp parse_inetrc_skip_line([], _Line, ack) do
    {:ok, reverse(ack)}
  end

  defp parse_inetrc_skip_line([?\n | str], line, ack) do
    parse_inetrc(str, line + 1, ack)
  end

  defp parse_inetrc_skip_line([_ | str], line, ack) do
    parse_inetrc_skip_line(str, line, ack)
  end

  defp parse_inetrc([?% | str], line, ack) do
    parse_inetrc_skip_line(str, line, ack)
  end

  defp parse_inetrc([?\s | str], line, ack) do
    parse_inetrc(str, line, ack)
  end

  defp parse_inetrc([?\n | str], line, ack) do
    parse_inetrc(str, line + 1, ack)
  end

  defp parse_inetrc([?\t | str], line, ack) do
    parse_inetrc(str, line, ack)
  end

  defp parse_inetrc([], _, ack) do
    {:ok, reverse(ack)}
  end

  defp parse_inetrc(str, line, ack) do
    case :erl_scan.tokens([], str, line) do
      {:done, {:ok, tokens, endLine}, moreChars} ->
        case :erl_parse.parse_term(tokens) do
          {:ok, term} ->
            parse_inetrc(moreChars, endLine, [term | ack])

          error ->
            {:error, {:parse_inetrc, error}}
        end

      {:done, {:eof, _}, _} ->
        {:ok, reverse(ack)}

      {:done, error, _} ->
        {:error, {:scan_inetrc, error}}

      {:more, _} ->
        {:error, {:scan_inetrc, {:eof, line}}}
    end
  end

  defp file_binary_to_list(bin) do
    enc =
      case :epp.read_encoding_from_binary(bin) do
        :none ->
          :epp.default_encoding()

        encoding ->
          encoding
      end

    case (try do
            :unicode.characters_to_list(bin, enc)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      string when is_list(string) ->
        {:ok, string}

      _ ->
        :error
    end
  end
end
