defmodule :m_mnesia_app do
  use Bitwise
  @behaviour :application
  def start(:normal, args) do
    case :mnesia_sup.start_link(args) do
      {:ok, pid} ->
        {:ok, pid, {:normal, args}}

      error ->
        error
    end
  end

  def start(_, _) do
    {:error, :badarg}
  end

  def stop(_StartArgs) do
    :ok
  end
end
