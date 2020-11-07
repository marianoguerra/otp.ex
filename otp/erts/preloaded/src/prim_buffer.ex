defmodule :m_prim_buffer do
  use Bitwise

  def on_load() do
    case :erlang.load_nif(
           :erlang.atom_to_list(:prim_buffer),
           0
         ) do
      :ok ->
        :ok
    end
  end

  def new() do
    :erlang.nif_error(:undef)
  end

  def size(_Buffer) do
    :erlang.nif_error(:undef)
  end

  def read(buffer, size) when size <= 64 do
    copying_read(buffer, size)
  end

  def read(buffer, size) when size > 64 do
    :erlang.iolist_to_binary(read_iovec(buffer, size))
  end

  def read_iovec(buffer, size) when size <= 64 do
    [copying_read(buffer, size)]
  end

  def read_iovec(buffer, size) when size > 64 do
    head = peek_head(buffer)
    headSize = byte_size(head)

    cond do
      headSize - size > 512 and size <= 512 ->
        [copying_read(buffer, size)]

      headSize > size ->
        skip(buffer, size)
        {first, _Rest} = :erlang.split_binary(head, size)
        [first]

      headSize < size ->
        skip(buffer, headSize)
        [head | read_iovec(buffer, size - headSize)]

      headSize === size ->
        skip(buffer, size)
        [head]
    end
  end

  def write(_Buffer, _IOVec) do
    :erlang.nif_error(:undef)
  end

  def skip(_Buffer, _Size) do
    :erlang.nif_error(:undef)
  end

  def wipe(buffer) do
    skip(buffer, :prim_buffer.size(buffer))
  end

  def find_byte_index(_Buffer, _Needle) do
    :erlang.nif_error(:undef)
  end

  def try_lock(_Buffer) do
    :erlang.nif_error(:undef)
  end

  def unlock(_Buffer) do
    :erlang.nif_error(:undef)
  end

  defp copying_read(_Buffer, _Size) do
    :erlang.nif_error(:undef)
  end

  defp peek_head(_Buffer) do
    :erlang.nif_error(:undef)
  end
end
