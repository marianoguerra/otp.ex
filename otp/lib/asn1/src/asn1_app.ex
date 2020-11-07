defmodule :m_asn1_app do
  use Bitwise
  @behaviour :application
  def start(_Type, _StartArgs) do
    {:ok, self()}
  end

  def stop(_State) do
    :ok
  end
end
