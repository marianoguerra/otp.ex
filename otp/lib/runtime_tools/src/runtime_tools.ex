defmodule :m_runtime_tools do
  use Bitwise
  @behaviour :application
  def start(_, autoModArgs) do
    case :supervisor.start_link({:local, :runtime_tools_sup}, :runtime_tools_sup, autoModArgs) do
      {:ok, pid} ->
        {:ok, pid, []}

      error ->
        error
    end
  end

  def stop(_) do
    :ok
  end
end
