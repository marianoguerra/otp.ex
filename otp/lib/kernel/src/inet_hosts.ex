defmodule :m_inet_hosts do
  use Bitwise
  require Record

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

  def gethostbyname(name) when is_list(name) do
    gethostbyname(
      name,
      case :inet_db.res_option(:inet6) do
        true ->
          :inet6

        false ->
          :inet
      end
    )
  end

  def gethostbyname(name) when is_atom(name) do
    gethostbyname(:erlang.atom_to_list(name))
  end

  def gethostbyname(_) do
    {:error, :formerr}
  end

  def gethostbyname(name, type)
      when is_list(name) and
             is_atom(type) do
    nm = :inet_db.tolower(name)

    case gethostbyname(nm, type, :inet_hosts_byname) do
      false ->
        case gethostbyname(nm, type, :inet_hosts_file_byname) do
          false ->
            {:error, :nxdomain}

          hostent ->
            {:ok, hostent}
        end

      hostent ->
        {:ok, hostent}
    end
  end

  def gethostbyname(name, type)
      when is_atom(name) and
             is_atom(type) do
    gethostbyname(:erlang.atom_to_list(name), type)
  end

  def gethostbyname(_, _) do
    {:error, :formerr}
  end

  defp gethostbyname(nm, type, byname) do
    :inet_db.res_update_hosts()

    case :ets.lookup(byname, {type, nm}) do
      [] ->
        false

      [{_, iPs, [primary | aliases]}] ->
        make_hostent(primary, iPs, aliases, type)
    end
  end

  def gethostbyaddr({a, b, c, d} = iP)
      when (a ||| b ||| c ||| d) &&& ~~~255 === 0 do
    gethostbyaddr(iP, :inet)
  end

  def gethostbyaddr({a, b, c, d, e, f, g, h} = iP)
      when (a ||| b ||| c ||| d ||| e ||| f ||| g ||| h) &&& ~~~65535 === 0 do
    gethostbyaddr(iP, :inet6)
  end

  def gethostbyaddr(addr) when is_list(addr) do
    case :inet_parse.address(addr) do
      {:ok, iP} ->
        gethostbyaddr(iP)

      _Error ->
        {:error, :formerr}
    end
  end

  def gethostbyaddr(addr) when is_atom(addr) do
    gethostbyaddr(:erlang.atom_to_list(addr))
  end

  def gethostbyaddr(_) do
    {:error, :formerr}
  end

  defp gethostbyaddr(iP, type) do
    case gethostbyaddr(iP, type, :inet_hosts_byaddr) do
      false ->
        case gethostbyaddr(iP, type, :inet_hosts_file_byaddr) do
          false ->
            {:error, :nxdomain}

          hostent ->
            {:ok, hostent}
        end

      hostent ->
        {:ok, hostent}
    end
  end

  defp gethostbyaddr(iP, type, byaddr) do
    :inet_db.res_update_hosts()

    case :ets.lookup(byaddr, {type, iP}) do
      [] ->
        false

      [{_, [primary | aliases]}] ->
        make_hostent(primary, [iP], aliases, type)
    end
  end

  defp make_hostent(name, addrs, aliases, :inet) do
    r_hostent(
      h_name: name,
      h_addrtype: :inet,
      h_length: 4,
      h_addr_list: addrs,
      h_aliases: aliases
    )
  end

  defp make_hostent(name, addrs, aliases, :inet6) do
    r_hostent(
      h_name: name,
      h_addrtype: :inet6,
      h_length: 16,
      h_addr_list: addrs,
      h_aliases: aliases
    )
  end
end
