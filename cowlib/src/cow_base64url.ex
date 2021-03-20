defmodule :cow_base64url do
  use Bitwise
  def decode(enc) do
    decode(enc, %{})
  end

  def decode(enc0, opts) do
    enc1 = (for << <<c>> <- enc0 >>, into: <<>> do
              <<case (c) do
                  ?- ->
                    ?+
                  ?_ ->
                    ?/
                  _ ->
                    c
                end>>
            end)
    enc = (case (opts) do
             %{padding: false} ->
               case (rem(byte_size(enc1), 4)) do
                 0 ->
                   enc1
                 2 ->
                   <<enc1 :: binary, "==">>
                 3 ->
                   <<enc1 :: binary, "=">>
               end
             _ ->
               enc1
           end)
    :base64.decode(enc)
  end

  def encode(dec) do
    encode(dec, %{})
  end

  def encode(dec, opts) do
    encode(:base64.encode(dec), opts, <<>>)
  end

  defp encode(<<?+, r :: bits>>, opts, acc) do
    encode(r, opts, <<acc :: binary, ?->>)
  end

  defp encode(<<?/, r :: bits>>, opts, acc) do
    encode(r, opts, <<acc :: binary, ?_>>)
  end

  defp encode(<<?=, _ :: bits>>, %{padding: false}, acc) do
    acc
  end

  defp encode(<<c, r :: bits>>, opts, acc) do
    encode(r, opts, <<acc :: binary, c>>)
  end

  defp encode(<<>>, _, acc) do
    acc
  end

end