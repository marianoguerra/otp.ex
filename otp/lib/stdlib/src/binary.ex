defmodule :m_binary do
  use Bitwise

  def at(_, _) do
    :erlang.nif_error(:undef)
  end

  def bin_to_list(subject) do
    :erlang.binary_to_list(subject)
  end

  def bin_to_list(subject, {pos, len}) do
    bin_to_list(subject, pos, len)
  end

  def bin_to_list(_Subject, _BadArg) do
    :erlang.error(:badarg)
  end

  def bin_to_list(subject, pos, len)
      when not is_binary(subject) or
             not is_integer(pos) or not is_integer(len) do
    :erlang.error(:badarg)
  end

  def bin_to_list(subject, pos, 0)
      when pos >= 0 and
             pos <= byte_size(subject) do
    []
  end

  def bin_to_list(_Subject, _Pos, 0) do
    :erlang.error(:badarg)
  end

  def bin_to_list(subject, pos, len) when len < 0 do
    bin_to_list(subject, pos + len, -len)
  end

  def bin_to_list(subject, pos, len) when len > 0 do
    :erlang.binary_to_list(subject, pos + 1, pos + len)
  end

  def compile_pattern(_) do
    :erlang.nif_error(:undef)
  end

  def copy(_) do
    :erlang.nif_error(:undef)
  end

  def copy(_, _) do
    :erlang.nif_error(:undef)
  end

  def decode_unsigned(_) do
    :erlang.nif_error(:undef)
  end

  def decode_unsigned(_, _) do
    :erlang.nif_error(:undef)
  end

  def encode_unsigned(_) do
    :erlang.nif_error(:undef)
  end

  def encode_unsigned(_, _) do
    :erlang.nif_error(:undef)
  end

  def first(_) do
    :erlang.nif_error(:undef)
  end

  def last(_) do
    :erlang.nif_error(:undef)
  end

  def list_to_bin(_) do
    :erlang.nif_error(:undef)
  end

  def longest_common_prefix(_) do
    :erlang.nif_error(:undef)
  end

  def longest_common_suffix(_) do
    :erlang.nif_error(:undef)
  end

  def match(_, _) do
    :erlang.nif_error(:undef)
  end

  def match(_, _, _) do
    :erlang.nif_error(:undef)
  end

  def matches(_, _) do
    :erlang.nif_error(:undef)
  end

  def matches(_, _, _) do
    :erlang.nif_error(:undef)
  end

  def part(_, _) do
    :erlang.nif_error(:undef)
  end

  def part(_, _, _) do
    :erlang.nif_error(:undef)
  end

  def referenced_byte_size(_) do
    :erlang.nif_error(:undef)
  end

  def split(_, _) do
    :erlang.nif_error(:undef)
  end

  def split(_, _, _) do
    :erlang.nif_error(:undef)
  end

  def replace(h, n, r) do
    replace(h, n, r, [])
  end

  def replace(haystack, needles, replacement, options) do
    try do
      true = is_binary(replacement)

      {part, global, insert} =
        get_opts_replace(
          options,
          {:no, false, []}
        )

      moptlist =
        case part do
          :no ->
            []

          {a, b} ->
            [{:scope, {a, b}}]
        end

      mList =
        cond do
          global ->
            :binary.matches(haystack, needles, moptlist)

          true ->
            case :binary.match(haystack, needles, moptlist) do
              :nomatch ->
                []

              match ->
                [match]
            end
        end

      replList =
        case insert do
          [] ->
            replacement

          y when is_integer(y) ->
            splitat(replacement, 0, [y])

          li when is_list(li) ->
            splitat(replacement, 0, :lists.sort(li))
        end

      :erlang.iolist_to_binary(do_replace(haystack, mList, replList, 0))
    catch
      _, _ ->
        :erlang.error(:badarg)
    end
  end

  defp do_replace(h, [], _, n) do
    [:binary.part(h, {n, byte_size(h) - n})]
  end

  defp do_replace(h, [{a, b} | t], replacement, n) do
    [
      [
        :binary.part(h, {n, a - n}),
        cond do
          is_list(replacement) ->
            do_insert(replacement, :binary.part(h, {a, b}))

          true ->
            replacement
        end
      ]
      | do_replace(h, t, replacement, a + b)
    ]
  end

  defp do_insert([x], _) do
    [x]
  end

  defp do_insert([h | t], r) do
    [[h, r] | do_insert(t, r)]
  end

  defp splitat(h, n, []) do
    [:binary.part(h, {n, byte_size(h) - n})]
  end

  defp splitat(h, n, [i | t]) do
    [:binary.part(h, {n, i - n}) | splitat(h, i, t)]
  end

  defp get_opts_replace([], {part, global, insert}) do
    {part, global, insert}
  end

  defp get_opts_replace(
         [{:scope, {a, b}} | t],
         {_Part, global, insert}
       ) do
    get_opts_replace(t, {{a, b}, global, insert})
  end

  defp get_opts_replace([:global | t], {part, _Global, insert}) do
    get_opts_replace(t, {part, true, insert})
  end

  defp get_opts_replace(
         [{:insert_replaced, n} | t],
         {part, global, _Insert}
       ) do
    get_opts_replace(t, {part, global, n})
  end

  defp get_opts_replace(_, _) do
    throw(:badopt)
  end
end
