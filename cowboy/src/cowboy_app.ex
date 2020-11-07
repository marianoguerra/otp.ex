defmodule :cowboy_app do
  use Bitwise
  @behaviour :application
  def start(_, _) do
    :cowboy_sup.start_link()
  end

  def stop(_) do
    :ok
  end
end
