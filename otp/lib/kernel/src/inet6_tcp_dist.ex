defmodule :m_inet6_tcp_dist do
  use Bitwise

  def select(node) do
    :inet_tcp_dist.gen_select(:inet6_tcp, node)
  end

  def address() do
    :inet_tcp_dist.gen_address(:inet6_tcp)
  end

  def listen(name, host) do
    :inet_tcp_dist.gen_listen(:inet6_tcp, name, host)
  end

  def accept(listen) do
    :inet_tcp_dist.gen_accept(:inet6_tcp, listen)
  end

  def accept_connection(acceptPid, socket, myNode, allowed, setupTime) do
    :inet_tcp_dist.gen_accept_connection(
      :inet6_tcp,
      acceptPid,
      socket,
      myNode,
      allowed,
      setupTime
    )
  end

  def setup(node, type, myNode, longOrShortNames, setupTime) do
    :inet_tcp_dist.gen_setup(:inet6_tcp, node, type, myNode, longOrShortNames, setupTime)
  end

  def close(socket) do
    :inet6_tcp.close(socket)
  end

  def is_node_name(node) when is_atom(node) do
    :inet_tcp_dist.is_node_name(node)
  end

  def setopts(s, opts) do
    :inet_tcp_dist.setopts(s, opts)
  end

  def getopts(s, opts) do
    :inet_tcp_dist.getopts(s, opts)
  end
end
