defmodule :m_raw_file_io do
  use Bitwise

  def open(filename, modes) do
    moduleOrder = [
      {:raw_file_io_list, &match_list/1},
      {:raw_file_io_compressed, &match_compressed/1},
      {:raw_file_io_delayed, &match_delayed/1}
    ]

    open_1(moduleOrder, filename, add_implicit_modes(modes))
  end

  defp open_1([], filename, modes) do
    :prim_file.open(filename, modes)
  end

  defp open_1([{module, match} | rest], filename, modes) do
    case :lists.any(match, modes) do
      true ->
        {options, childModes} =
          :lists.partition(
            fn mode ->
              match.(mode)
            end,
            modes
          )

        module.open_layer(filename, childModes, options)

      false ->
        open_1(rest, filename, modes)
    end
  end

  defp add_implicit_modes(modes0) do
    modes1 = add_unless_matched(modes0, &match_writable/1, :read)
    add_unless_matched(modes1, &match_binary/1, :list)
  end

  defp add_unless_matched(modes, match, default) do
    case :lists.any(match, modes) do
      false ->
        [default | modes]

      true ->
        modes
    end
  end

  defp match_list(:list) do
    true
  end

  defp match_list(_Other) do
    false
  end

  defp match_compressed(:compressed) do
    true
  end

  defp match_compressed(_Other) do
    false
  end

  defp match_delayed({:delayed_write, _Size, _Timeout}) do
    true
  end

  defp match_delayed(:delayed_write) do
    true
  end

  defp match_delayed(_Other) do
    false
  end

  defp match_writable(:write) do
    true
  end

  defp match_writable(:append) do
    true
  end

  defp match_writable(:exclusive) do
    true
  end

  defp match_writable(_Other) do
    false
  end

  defp match_binary(:binary) do
    true
  end

  defp match_binary(_Other) do
    false
  end
end
