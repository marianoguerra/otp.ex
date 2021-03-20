defmodule :cow_iolists do
  use Bitwise
  def split(n, iolist) do
    case (split(n, iolist, [])) do
      {:ok, before, after__} ->
        {before, after__}
      {:more, _, before} ->
        {:lists.reverse(before), <<>>}
    end
  end

  defp split(0, rest, acc) do
    {:ok, :lists.reverse(acc), rest}
  end

  defp split(n, [], acc) do
    {:more, n, acc}
  end

  defp split(n, binary, acc) when byte_size(binary) <= n do
    {:more, n - byte_size(binary), [binary | acc]}
  end

  defp split(n, binary, acc) when is_binary(binary) do
    <<before :: size(n) - binary, after__ :: bits>> = binary
    {:ok, :lists.reverse([before | acc]), after__}
  end

  defp split(n, [binary | tail], acc)
      when byte_size(binary) <= n do
    split(n - byte_size(binary), tail, [binary | acc])
  end

  defp split(n, [binary | tail], acc)
      when is_binary(binary) do
    <<before :: size(n) - binary, after__ :: bits>> = binary
    {:ok, :lists.reverse([before | acc]), [after__ | tail]}
  end

  defp split(n, [char | tail], acc) when is_integer(char) do
    split(n - 1, tail, [char | acc])
  end

  defp split(n, [list | tail], acc0) do
    case (split(n, list, acc0)) do
      {:ok, before, after__} ->
        {:ok, before, [after__ | tail]}
      {:more, more, acc} ->
        split(more, tail, acc)
    end
  end

end