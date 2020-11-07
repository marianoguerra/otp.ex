defmodule :m_ssh_server_channel do
  use Bitwise

  def start_link(connectionManager, channelId, callBack, cbInitArgs, exec) do
    :ssh_client_channel.start_link(connectionManager, channelId, callBack, cbInitArgs, exec)
  end

  def get_print_info(pid) do
    :ssh_client_channel.get_print_info(pid)
  end
end
