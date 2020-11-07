defmodule :m_ftp_app do
  use Bitwise
  @behaviour :application
  def start(_StartType, _StartArgs) do
    :ftp_sup.start_link()
  end

  def stop(_State) do
    :ok
  end
end
