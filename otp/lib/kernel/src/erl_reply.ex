defmodule :m_erl_reply do
  use Bitwise

  def reply([addr, port, msg]) do
    ip = ip_string_to_tuple(:erlang.atom_to_list(addr))
    p = :erlang.list_to_integer(:erlang.atom_to_list(port))
    m = :erlang.atom_to_list(msg)
    {:ok, s} = :gen_tcp.connect(ip, p, [])
    :ok = :gen_tcp.send(s, m)
    :gen_tcp.close(s)
    :reply_done
  end

  def reply(_) do
    :error_logger.error_msg('erl_reply: Can\'t find address and port to reply to~n')
  end

  defp ip_string_to_tuple(ip) do
    [ip1, ip2, ip3, ip4] = :string.lexemes(ip, '.')

    {:erlang.list_to_integer(ip1), :erlang.list_to_integer(ip2), :erlang.list_to_integer(ip3),
     :erlang.list_to_integer(ip4)}
  end
end
