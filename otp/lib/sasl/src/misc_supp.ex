defmodule :m_misc_supp do
  use Bitwise

  def format_pdict(:normal, _PDict, _Exclude) do
    []
  end

  def format_pdict(:all, pDict, exclude) do
    case format_tuples(pDict, [:"$sys_dict$" | exclude]) do
      [] ->
        []

      data ->
        [{:newline, 1} | data]
    end
  end

  def format_tuples(keyValues, exclude) do
    case format_tuples(keyValues, exclude, []) do
      [] ->
        []

      data ->
        [{:data, data}]
    end
  end

  defp format_tuples([], _Exclude, res) do
    res
  end

  defp format_tuples([{key, value} | t], exclude, res) do
    case :lists.member(key, exclude) do
      true ->
        format_tuples(t, exclude, res)

      false ->
        format_tuples(t, exclude, [{key, value} | res])
    end
  end

  def assq(key, list) do
    case :lists.keysearch(key, 1, list) do
      {:value, {^key, val}} ->
        {:value, val}

      _ ->
        false
    end
  end

  def passq(key, list) do
    case :lists.keysearch(key, 1, list) do
      {:value, {^key, val}} ->
        val

      _ ->
        :undefined
    end
  end

  def is_string([]) do
    false
  end

  def is_string(x) do
    is_string_2(x)
  end

  defp is_string_2([]) do
    true
  end

  defp is_string_2([h | t])
       when is_integer(h) and h >= ?\s and
              h <= 255 do
    is_string_2(t)
  end

  defp is_string_2(_) do
    false
  end

  def multi_map(_Func, [[] | _ListOfLists]) do
    []
  end

  def multi_map(func, listOfLists) do
    [
      apply(
        func,
        :lists.map(
          fn list ->
            hd(list)
          end,
          listOfLists
        )
      )
      | multi_map(
          func,
          :lists.map(
            fn list ->
              tl(list)
            end,
            listOfLists
          )
        )
    ]
  end

  def modifier(device) do
    encoding =
      case :io.getopts(device) do
        list when is_list(list) ->
          :proplists.get_value(:encoding, list, :latin1)

        _ ->
          :latin1
      end

    encoding_to_modifier(encoding)
  end

  defp encoding_to_modifier(:latin1) do
    ''
  end

  defp encoding_to_modifier(_) do
    't'
  end
end
