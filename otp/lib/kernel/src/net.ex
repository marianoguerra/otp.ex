defmodule :m_net do
  use Bitwise

  def call(n, m, f, a) do
    :rpc.call(n, m, f, a)
  end

  def cast(n, m, f, a) do
    :rpc.cast(n, m, f, a)
  end

  def broadcast(m, f, a) do
    :rpc.eval_everywhere(m, f, a)
  end

  def ping(node) do
    :net_adm.ping(node)
  end

  def sleep(t) do
    receive do
    after
      t ->
        :ok
    end
  end

  def relay(x) do
    :slave.relay(x)
  end

  def info() do
    :erlang.error(:notsup)
  end

  def command(_Cmd) do
    :erlang.error(:notsup)
  end

  def gethostname() do
    :erlang.error(:notsup)
  end

  def getnameinfo(sockAddr) do
    getnameinfo(sockAddr, :undefined)
  end

  def getnameinfo(sockAddr, flags)
      when (is_map(sockAddr) and
              is_list(flags)) or
             (is_map(sockAddr) and flags === :undefined) do
    :erlang.error(:notsup)
  end

  def getaddrinfo(host) when is_list(host) do
    getaddrinfo(host, :undefined)
  end

  def getaddrinfo(host, service)
      when (is_list(host) or host === :undefined) and (is_list(service) or service === :undefined) and
             not (service === :undefined and host === :undefined) do
    :erlang.error(:notsup)
  end

  def getifaddrs() do
    :erlang.error(:notsup)
  end

  def getifaddrs(filter)
      when is_atom(filter) or is_map(filter) or is_function(filter) do
    :erlang.error(:notsup)
  end

  def getifaddrs(namespace) when is_list(namespace) do
    :erlang.error(:notsup)
  end

  def getifaddrs(filter, namespace)
      when (is_atom(filter) or is_map(filter)) and is_list(namespace) do
    do_getifaddrs(
      getifaddrs_filter_map(filter),
      fn ->
        getifaddrs(namespace)
      end
    )
  end

  def getifaddrs(filter, namespace)
      when is_function(
             filter,
             1
           ) and is_list(namespace) do
    do_getifaddrs(
      filter,
      fn ->
        getifaddrs(namespace)
      end
    )
  end

  defp do_getifaddrs(filter, getIfAddrs) do
    case getIfAddrs.() do
      {:ok, ifAddrs0} when is_function(filter) ->
        {:ok, :lists.filtermap(filter, ifAddrs0)}

      {:ok, ifAddrs0} when is_map(filter) ->
        filterFun = fn elem ->
          getifaddrs_filter(filter, elem)
        end

        {:ok, :lists.filtermap(filterFun, ifAddrs0)}

      {:error, _} = eRROR ->
        eRROR
    end
  end

  defp getifaddrs_filter_map(:all) do
    getifaddrs_filter_map_all()
  end

  defp getifaddrs_filter_map(:default) do
    getifaddrs_filter_map_default()
  end

  defp getifaddrs_filter_map(:inet) do
    getifaddrs_filter_map_inet()
  end

  defp getifaddrs_filter_map(:inet6) do
    getifaddrs_filter_map_inet6()
  end

  defp getifaddrs_filter_map(:packet) do
    getifaddrs_filter_map_packet()
  end

  defp getifaddrs_filter_map(filterMap) when is_map(filterMap) do
    :maps.merge(getifaddrs_filter_map_default(), filterMap)
  end

  defp getifaddrs_filter_map_all() do
    %{family: :all, flags: :any}
  end

  defp getifaddrs_filter_map_default() do
    %{family: :default, flags: :any}
  end

  defp getifaddrs_filter_map_inet() do
    %{family: :inet, flags: :any}
  end

  defp getifaddrs_filter_map_inet6() do
    %{family: :inet6, flags: :any}
  end

  defp getifaddrs_filter_map_packet() do
    %{family: :packet, flags: :any}
  end

  defp getifaddrs_filter(
         %{family: fFamily, flags: fFlags},
         %{addr: %{family: family}, flags: flags} = _Entry
       )
       when fFamily === :default and (family === :inet or family === :inet6) do
    getifaddrs_filter_flags(fFlags, flags)
  end

  defp getifaddrs_filter(
         %{family: fFamily, flags: fFlags},
         %{addr: %{family: family}, flags: flags} = _Entry
       )
       when fFamily === :inet and family === :inet do
    getifaddrs_filter_flags(fFlags, flags)
  end

  defp getifaddrs_filter(
         %{family: fFamily, flags: fFlags},
         %{addr: %{family: family}, flags: flags} = _Entry
       )
       when fFamily === :inet6 and family === :inet6 do
    getifaddrs_filter_flags(fFlags, flags)
  end

  defp getifaddrs_filter(
         %{family: fFamily, flags: fFlags},
         %{addr: %{family: family}, flags: flags} = _Entry
       )
       when fFamily === :packet and family === :packet do
    getifaddrs_filter_flags(fFlags, flags)
  end

  defp getifaddrs_filter(
         %{family: fFamily, flags: fFlags},
         %{flags: flags} = _Entry
       )
       when fFamily === :all do
    getifaddrs_filter_flags(fFlags, flags)
  end

  defp getifaddrs_filter(_Filter, _Entry) do
    false
  end

  defp getifaddrs_filter_flags(:any, _Flags) do
    true
  end

  defp getifaddrs_filter_flags(filterFlags, flags) do
    [] === filterFlags -- flags
  end

  def if_name2index(if__) when is_list(if__) do
    :erlang.error(:notsup)
  end

  def if_index2name(idx) when is_integer(idx) do
    :erlang.error(:notsup)
  end

  def if_names() do
    :erlang.error(:notsup)
  end
end
