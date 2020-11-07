defmodule :m_odbc_app do
  use Bitwise

  def start(_Type, name) do
    :supervisor.start_link({:local, :odbc_sup}, :odbc_sup, [name])
  end

  def stop([]) do
    :ok
  end
end
