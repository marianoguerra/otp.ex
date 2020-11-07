defmodule :m_ssh_tcpip_forward_client do
  use Bitwise
  @behaviour :ssh_client_channel
  require Record
  Record.defrecord(:r_state, :state, id: :undefined, cm: :undefined, fwd_socket: :undefined)

  def init([fwdSocket]) do
    {:ok, r_state(fwd_socket: fwdSocket)}
  end

  def handle_msg(
        {:ssh_channel_up, channelId, connectionManager},
        state
      ) do
    {:ok, r_state(state, id: channelId, cm: connectionManager)}
  end

  def handle_msg(
        {:tcp, sock, data},
        r_state(fwd_socket: sock, cm: cM, id: chId) = state
      ) do
    :ssh_connection.send(cM, chId, data)
    :inet.setopts(sock, [{:active, :once}])
    {:ok, state}
  end

  def handle_msg(
        {:tcp_closed, sock},
        r_state(fwd_socket: sock, cm: cM, id: chId) = state
      ) do
    :ssh_connection.send_eof(cM, chId)
    {:stop, chId, r_state(state, fwd_socket: :undefined)}
  end

  def handle_ssh_msg(
        {:ssh_cm, _CM, {:data, _ChannelId, _Type, data}},
        r_state(fwd_socket: sock) = state
      ) do
    :gen_tcp.send(sock, data)
    {:ok, state}
  end

  def handle_ssh_msg({:ssh_cm, _CM, {:eof, chId}}, state) do
    {:stop, chId, state}
  end

  def handle_ssh_msg({:ssh_cm, _CM, {:signal, _, _}}, state) do
    {:ok, state}
  end

  def handle_ssh_msg(
        {:ssh_cm, _CM, {:exit_signal, chId, _, _Error, _}},
        state
      ) do
    {:stop, chId, state}
  end

  def handle_ssh_msg(
        {:ssh_cm, _, {:exit_status, chId, _Status}},
        state
      ) do
    {:stop, chId, state}
  end

  def terminate(_Reason, r_state(fwd_socket: sock)) do
    :gen_tcp.close(sock)
    :ok
  end

  def handle_call(req, _, s) do
    {:reply, {:unknown, req}, s}
  end

  def handle_cast(_, s) do
    {:noreply, s}
  end

  def code_change(_, s, _) do
    {:ok, s}
  end
end
