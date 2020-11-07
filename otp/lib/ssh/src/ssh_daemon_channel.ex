defmodule :m_ssh_daemon_channel do
  use Bitwise

  def start_link(connectionManager, channelId, callBack, cbInitArgs, exec) do
    :ssh_server_channel.start_link(connectionManager, channelId, callBack, cbInitArgs, exec)
  end

  def get_print_info(pid) do
    :ssh_server_channel.get_print_info(pid)
  end
end
