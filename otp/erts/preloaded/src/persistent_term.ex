defmodule :m_persistent_term do
  use Bitwise

  def erase(_Key) do
    :erlang.nif_error(:undef)
  end

  def get() do
    :erlang.nif_error(:undef)
  end

  def get(_Key) do
    :erlang.nif_error(:undef)
  end

  def get(_Key, _Default) do
    :erlang.nif_error(:undef)
  end

  def info() do
    :erlang.nif_error(:undef)
  end

  def put(_Key, _Value) do
    :erlang.nif_error(:undef)
  end
end
