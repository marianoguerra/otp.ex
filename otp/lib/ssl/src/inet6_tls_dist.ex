defmodule :m_inet6_tls_dist do
  use Bitwise

  def childspecs() do
    :inet_tls_dist.childspecs()
  end

  def select(node) do
    :inet_tls_dist.gen_select(:inet6_tcp, node)
  end

  def address() do
    :inet_tls_dist.gen_address(:inet6_tcp)
  end

  def listen(name, host) do
    :inet_tls_dist.gen_listen(:inet6_tcp, name, host)
  end

  def accept(listen) do
    :inet_tls_dist.gen_accept(:inet6_tcp, listen)
  end

  def accept_connection(acceptPid, socket, myNode, allowed, setupTime) do
    :inet_tls_dist.gen_accept_connection(
      :inet6_tcp,
      acceptPid,
      socket,
      myNode,
      allowed,
      setupTime
    )
  end

  def setup(node, type, myNode, longOrShortNames, setupTime) do
    :inet_tls_dist.gen_setup(:inet6_tcp, node, type, myNode, longOrShortNames, setupTime)
  end

  def close(socket) do
    :inet_tls_dist.gen_close(:inet6_tcp, socket)
  end
end
