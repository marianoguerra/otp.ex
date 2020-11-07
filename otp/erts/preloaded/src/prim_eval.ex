defmodule :m_prim_eval do
  use Bitwise

  def receive(_, _) do
    :erlang.nif_error(:stub)
  end
end
