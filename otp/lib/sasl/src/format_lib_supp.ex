defmodule :m_format_lib_supp do
  use Bitwise

  def print_info(device, format) do
    print_info(device, 79, format)
  end

  def print_info(device, line, format) do
    print_header(device, line, format)
    print_format(device, line, format)
  end

  defp print_header(device, line, [{:header, header} | _]) do
    print_header2(device, line, header)
  end

  defp print_header(device, line, _) do
    print_header2(device, line, '')
  end

  defp print_header2(device, line, header) do
    format1 = :lists.concat(['~n~', line, '.', line, 's~n'])
    format2 = :lists.concat(['~', line, 'c~n'])
    :io.format(device, format1, [header])
    :io.format(device, format2, [?=])
  end

  defp print_format(device, _Line, []) do
    :io.format(device, :"~n", [])
  end

  defp print_format(device, line, [{:data, data} | t]) do
    print_data(device, line, data)
    print_format(device, line, t)
  end

  defp print_format(device, line, [{:items, items} | t]) do
    print_items(device, line, items)
    print_format(device, line, t)
  end

  defp print_format(device, line, [{:newline, n} | t]) do
    print_newlines(device, n)
    print_format(device, line, t)
  end

  defp print_format(device, line, [_ | t]) do
    print_format(device, line, t)
  end

  defp print_data(_Device, _Line, []) do
    :ok
  end

  defp print_data(device, line, [{key, value} | t]) do
    print_one_line(device, line, key, value)
    print_data(device, line, t)
  end

  defp print_data(device, line, [value | t]) do
    modifier = :misc_supp.modifier(device)
    :io.format(device, '~' ++ modifier ++ 'p~n', [value])
    print_data(device, line, t)
  end

  defp print_data(device, _Line, value) do
    modifier = :misc_supp.modifier(device)
    :io.format(device, '~' ++ modifier ++ 'p~n', [value])
  end

  defp print_items(device, line, {name, items}) do
    print_items(device, line, name, items)
  end

  defp print_newlines(_Device, 0) do
    :ok
  end

  defp print_newlines(device, n) when n > 0 do
    :io.format(device, :"~n", [])
    print_newlines(device, n - 1)
  end

  defp print_one_line(device, line, key, value) do
    modifier = :misc_supp.modifier(device)
    strKey = term_to_string(key, modifier)
    keyLen = :lists.min([:string.length(strKey), line])
    valueLen = line - keyLen
    format1 = :lists.concat(['~-', keyLen, modifier, 's'])
    format2 = :lists.concat(['~', valueLen, modifier, 's~n'])
    :io.format(device, format1, [strKey])
    try = term_to_string(value, modifier)
    length = :string.length(try)

    cond do
      length < valueLen ->
        :io.format(device, format2, [try])

      true ->
        :io.format(device, '~n         ', [])
        format3 = :lists.concat(['~', line, '.9', modifier, 'p~n'])
        :io.format(device, format3, [value])
    end
  end

  defp term_to_string(value, modifier) do
    :io_lib.format(get_format(value, modifier), [value])
  end

  defp get_format([], _) do
    '~p'
  end

  defp get_format(value, modifier) do
    case :io_lib.printable_list(value) do
      true ->
        '~' ++ modifier ++ 's'

      false ->
        '~' ++ modifier ++ 'p'
    end
  end

  defp print_items(device, line, name, items) do
    print_one_line(device, line, name, ' ')
    print_item_elements(device, line, items)
  end

  defp print_item_elements(_Device, _Line, []) do
    :ok
  end

  defp print_item_elements(device, line, [{key, value} | t]) do
    print_one_line(device, line, :lists.concat(['   ', key]), value)
    print_item_elements(device, line, t)
  end
end
