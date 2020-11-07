defmodule :cow_http_te do
  use Bitwise

  def stream_identity(data, {streamed, total}) do
    streamed2 = streamed + byte_size(data)

    cond do
      streamed2 < total ->
        {:more, data, total - streamed2, {streamed2, total}}

      true ->
        size = total - streamed
        <<data2::size(size)-binary, rest::bits>> = data
        {:done, data2, total, rest}
    end
  end

  def identity(data) do
    data
  end

  def stream_chunked(data, state) do
    stream_chunked(data, state, <<>>)
  end

  defp stream_chunked(data = <<c, _::bits>>, {0, streamed}, acc)
       when c !== ?\r do
    case chunked_len(data, streamed, acc, 0) do
      {:next, rest, state, acc2} ->
        stream_chunked(rest, state, acc2)

      {:more, state, acc2} ->
        {:more, acc2, data, state}

      ret ->
        ret
    end
  end

  defp stream_chunked(<<"\r\n", rest::bits>>, {2, streamed}, acc) do
    stream_chunked(rest, {0, streamed}, acc)
  end

  defp stream_chunked("\r", {2, streamed}, acc) do
    {:more, acc, {1, streamed}}
  end

  defp stream_chunked(<<"\n", rest::bits>>, {1, streamed}, acc) do
    stream_chunked(rest, {0, streamed}, acc)
  end

  defp stream_chunked(<<>>, state = {rem, _}, acc) do
    {:more, acc, rem, state}
  end

  defp stream_chunked(data, {rem, streamed}, acc) when rem > 2 do
    dataSize = byte_size(data)
    remSize = rem - 2

    case data do
      <<chunk::size(remSize)-binary, "\r\n", rest::bits>> ->
        stream_chunked(rest, {0, streamed + remSize}, <<acc::binary, chunk::binary>>)

      <<chunk::size(remSize)-binary, "\r">> ->
        {:more, <<acc::binary, chunk::binary>>, {1, streamed + remSize}}

      _ when dataSize <= remSize ->
        rem2 = rem - dataSize
        {:more, <<acc::binary, data::binary>>, rem2, {rem2, streamed + dataSize}}
    end
  end

  defp chunked_len(<<?0, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16)
  end

  defp chunked_len(<<?1, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 1)
  end

  defp chunked_len(<<?2, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 2)
  end

  defp chunked_len(<<?3, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 3)
  end

  defp chunked_len(<<?4, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 4)
  end

  defp chunked_len(<<?5, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 5)
  end

  defp chunked_len(<<?6, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 6)
  end

  defp chunked_len(<<?7, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 7)
  end

  defp chunked_len(<<?8, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 8)
  end

  defp chunked_len(<<?9, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 9)
  end

  defp chunked_len(<<?A, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 10)
  end

  defp chunked_len(<<?B, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 11)
  end

  defp chunked_len(<<?C, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 12)
  end

  defp chunked_len(<<?D, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 13)
  end

  defp chunked_len(<<?E, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 14)
  end

  defp chunked_len(<<?F, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 15)
  end

  defp chunked_len(<<?a, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 10)
  end

  defp chunked_len(<<?b, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 11)
  end

  defp chunked_len(<<?c, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 12)
  end

  defp chunked_len(<<?d, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 13)
  end

  defp chunked_len(<<?e, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 14)
  end

  defp chunked_len(<<?f, r::bits>>, s, a, len) do
    chunked_len(r, s, a, len * 16 + 15)
  end

  defp chunked_len(<<c, r::bits>>, s, a, len)
       when c === ?\s or c === ?\t or c === ?; do
    skip_chunk_ext(r, s, a, len, 0)
  end

  defp chunked_len(<<"\r\n\r\n", r::bits>>, _, <<>>, 0) do
    {:done, :no_trailers, r}
  end

  defp chunked_len(<<"\r\n\r\n", r::bits>>, _, a, 0) do
    {:done, a, :no_trailers, r}
  end

  defp chunked_len(<<"\r\n", r::bits>>, _, <<>>, 0)
       when byte_size(r) > 2 do
    {:done, :trailers, r}
  end

  defp chunked_len(<<"\r\n", r::bits>>, _, a, 0)
       when byte_size(r) > 2 do
    {:done, a, :trailers, r}
  end

  defp chunked_len(_, _, _, 0) do
    :more
  end

  defp chunked_len(<<"\r\n", r::bits>>, s, a, len) do
    {:next, r, {len + 2, s}, a}
  end

  defp chunked_len("\r", _, <<>>, _) do
    :more
  end

  defp chunked_len("\r", s, a, _) do
    {:more, {0, s}, a}
  end

  defp chunked_len(<<>>, _, <<>>, _) do
    :more
  end

  defp chunked_len(<<>>, s, a, _) do
    {:more, {0, s}, a}
  end

  defp skip_chunk_ext(r = <<"\r", _::bits>>, s, a, len, _) do
    chunked_len(r, s, a, len)
  end

  defp skip_chunk_ext(r = <<>>, s, a, len, _) do
    chunked_len(r, s, a, len)
  end

  defp skip_chunk_ext(<<c, r::bits>>, s, a, len, skipped)
       when c !== ?\n and skipped < 128 do
    skip_chunk_ext(r, s, a, len, skipped + 1)
  end

  def chunk(data) do
    [:erlang.integer_to_list(:erlang.iolist_size(data), 16), "\r\n", data, "\r\n"]
  end

  def last_chunk() do
    "0\r\n\r\n"
  end
end
