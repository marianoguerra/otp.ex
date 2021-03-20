defmodule :m_prim_net do
  use Bitwise

  def on_load() do
    on_load(%{})
  end

  def on_load(extra) do
    :ok =
      :erlang.load_nif(
        :erlang.atom_to_list(:net),
        extra
      )
  end

  def info() do
    nif_info()
  end

  def command(cmd) do
    nif_command(cmd)
  end

  def gethostname() do
    nif_gethostname()
  end

  def getnameinfo(sockAddr, flags) do
    try do
      :prim_socket.enc_sockaddr(sockAddr)
    catch
      eRROR ->
        eRROR
    else
      eSockAddr ->
        nif_getnameinfo(eSockAddr, flags)
    end
  end

  def getaddrinfo(host, service)
      when (is_list(host) or host === :undefined) and (is_list(service) or service === :undefined) and
             not (service === :undefined and host === :undefined) do
    result = nif_getaddrinfo(host, service, :undefined)

    case result do
      {:ok, []} ->
        result

      {:ok, addrs} ->
        protocols = :prim_socket.p_get(:protocols)

        {:ok,
         for addr <- addrs do
           case addr do
             %{protocol: num} ->
               case protocols do
                 %{^num => protocol} ->
                   %{addr | protocol: protocol}

                 %{} ->
                   addr
               end

             %{} ->
               addr
           end
         end}

      error ->
        error
    end
  end

  def getifaddrs(extra) when is_map(extra) do
    nif_getifaddrs(extra)
  end

  def if_name2index(if__) when is_list(if__) do
    nif_if_name2index(if__)
  end

  def if_index2name(idx) when is_integer(idx) do
    nif_if_index2name(idx)
  end

  def if_names() do
    nif_if_names()
  end

  defp nif_info() do
    :erlang.nif_error(:undef)
  end

  defp nif_command(_Cmd) do
    :erlang.nif_error(:undef)
  end

  defp nif_gethostname() do
    :erlang.nif_error(:undef)
  end

  defp nif_getnameinfo(_Addr, _Flags) do
    :erlang.nif_error(:undef)
  end

  defp nif_getaddrinfo(_Host, _Service, _Hints) do
    :erlang.nif_error(:undef)
  end

  defp nif_getifaddrs(_Extra) do
    :erlang.nif_error(:undef)
  end

  defp nif_if_name2index(_Name) do
    :erlang.nif_error(:undef)
  end

  defp nif_if_index2name(_Id) do
    :erlang.nif_error(:undef)
  end

  defp nif_if_names() do
    :erlang.nif_error(:undef)
  end
end
