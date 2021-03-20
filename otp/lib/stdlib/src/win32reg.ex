defmodule :m_win32reg do
  use Bitwise

  def open(modes) do
    case :os.type() do
      {:win32, _} ->
        case open_mode(modes, []) do
          {:error, reason} ->
            {:error, reason}

          modeStr ->
            p = :erlang.open_port({:spawn, 'registry__drv__ ' ++ modeStr}, [])
            {:ok, {:win32reg, p}}
        end

      _ ->
        {:error, :enotsup}
    end
  end

  def close({:win32reg, reg}) when is_port(reg) do
    :erlang.unlink(reg)
    :erlang.exit(reg, :die)
    :ok
  end

  def current_key({:win32reg, reg}) when is_port(reg) do
    cmd = [0]
    send(reg, {self(), {:command, cmd}})
    {:state, hkey, name} = get_result(reg)
    root = hkey_to_string(hkey)

    {:ok,
     case name do
       [] ->
         root

       _ ->
         root ++ [?\\ | name]
     end}
  end

  def change_key({:win32reg, reg}, key) when is_port(reg) do
    change_key(reg, 1, key)
  end

  def change_key_create({:win32reg, reg}, key) when is_port(reg) do
    change_key(reg, 2, key)
  end

  defp change_key(reg, cmd, key) do
    case parse_key(key, reg) do
      {:ok, hkey, path} ->
        send(reg, {self(), {:command, [cmd, i32(hkey), path, 0]}})
        get_result(reg)

      {:error, reason} ->
        {:error, reason}
    end
  end

  def sub_keys({:win32reg, reg}) when is_port(reg) do
    cmd = [3]
    send(reg, {self(), {:command, cmd}})
    collect_keys(reg, [])
  end

  def delete_key({:win32reg, reg}) when is_port(reg) do
    cmd = [7]
    send(reg, {self(), {:command, cmd}})
    get_result(reg)
  end

  def set_value({:win32reg, reg}, name0, value)
      when is_port(reg) do
    name =
      case name0 do
        :default ->
          []

        _ ->
          name0
      end

    {type, v} = term_to_value(value)
    cmd = [6, type, name, 0, v]
    send(reg, {self(), {:command, cmd}})
    get_result(reg)
  end

  def value({:win32reg, reg}, name) when is_port(reg) do
    cmd = [4, name, 0]
    send(reg, {self(), {:command, cmd}})

    case get_result(reg) do
      {:value, {^name, value}} ->
        {:ok, value}

      {:error, reason} ->
        {:error, reason}
    end
  end

  def values({:win32reg, reg}) when is_port(reg) do
    cmd = [5]
    send(reg, {self(), {:command, cmd}})
    collect_values(reg, [])
  end

  def delete_value({:win32reg, reg}, name0) when is_port(reg) do
    name =
      case name0 do
        :default ->
          []

        _ ->
          name0
      end

    cmd = [8, name, 0]
    send(reg, {self(), {:command, cmd}})
    get_result(reg)
  end

  def expand(value) do
    expand(value, [], [])
  end

  defp expand([?%, ?% | rest], [], result) do
    expand(rest, [], [?% | result])
  end

  defp expand([?%, c | rest], [], result) do
    expand(rest, [c], result)
  end

  defp expand([c | rest], [], result) do
    expand(rest, [], [c | result])
  end

  defp expand([?% | rest], env0, result) do
    env = :lists.reverse(env0)
    expand(rest, [], :lists.reverse(:os.getenv(env, '')) ++ result)
  end

  defp expand([c | rest], env, result) do
    expand(rest, [c | env], result)
  end

  defp expand([], [], result) do
    :lists.reverse(result)
  end

  def format_error(errorId) do
    :erl_posix_msg.message(errorId)
  end

  defp collect_values(p, result) do
    case get_result(p) do
      :ok ->
        {:ok, :lists.reverse(result)}

      {:value, valueData} ->
        collect_values(p, [valueData | result])

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp collect_keys(p, result) do
    case get_result(p) do
      :ok ->
        {:ok, :lists.reverse(result)}

      {:key, keyData} ->
        collect_keys(p, [keyData | result])

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp get_result(p) do
    receive do
      {^p, {:data, data}} ->
        get_result1(data)
    end
  end

  defp get_result1([?e | reason]) do
    {:error, :erlang.list_to_atom(reason)}
  end

  defp get_result1([?o]) do
    :ok
  end

  defp get_result1([?k | name]) do
    {:key, name}
  end

  defp get_result1([?v | rest0]) do
    {:ok, type, rest1} = i32_on_head(rest0)
    {:ok, name0, value} = get_cstring(rest1)

    name =
      case name0 do
        [] ->
          :default

        _ ->
          name0
      end

    {:value, {name, encode_value(type, value)}}
  end

  defp get_result1([?s | rest0]) do
    {:ok, hkey, name} = i32_on_head(rest0)
    {:state, hkey, name}
  end

  defp encode_value(1, value) do
    value
  end

  defp encode_value(2, value) do
    value
  end

  defp encode_value(4, value) do
    i32(value)
  end

  defp encode_value(_, value) do
    :erlang.list_to_binary(value)
  end

  defp term_to_value(int) when is_integer(int) do
    {i32(4), i32(int)}
  end

  defp term_to_value(string) when is_list(string) do
    {i32(1), [string, 0]}
  end

  defp term_to_value(bin) when is_binary(bin) do
    {i32(3), bin}
  end

  defp term_to_value(_) do
    exit(:badarg)
  end

  defp get_cstring(list) do
    get_cstring(list, [])
  end

  defp get_cstring([0 | rest], result) do
    {:ok, :lists.reverse(result), rest}
  end

  defp get_cstring([c | rest], result) do
    get_cstring(rest, [c | result])
  end

  defp get_cstring([], result) do
    {:ok, :lists.reverse(result), []}
  end

  defp i32(int) when is_integer(int) do
    [int >>> 24 &&& 255, int >>> 16 &&& 255, int >>> 8 &&& 255, int &&& 255]
  end

  defp i32([x1, x2, x3, x4]) do
    x1 <<< 24 ||| x2 <<< 16 ||| x3 <<< 8 ||| x4
  end

  defp i32_on_head([x1, x2, x3, x4 | rest]) do
    {:ok, x1 <<< 24 ||| x2 <<< 16 ||| x3 <<< 8 ||| x4, rest}
  end

  defp parse_key([?\\ | rest], _) do
    parse_root(rest, [])
  end

  defp parse_key(key, reg) do
    parse_relative(key, reg)
  end

  defp parse_relative(path, reg) do
    cmd = [0]
    send(reg, {self(), {:command, cmd}})
    {:state, rootHandle, name} = get_result(reg)
    original = split_key(name)
    relative = :lists.reverse(split_key(path))

    case parse_relative1(relative, original) do
      newPath ->
        {:ok, rootHandle, newPath}
    end
  end

  defp parse_relative1(['..' | t1], [_ | t2]) do
    parse_relative1(t1, t2)
  end

  defp parse_relative1([comp | rest], result) do
    parse_relative1(rest, [comp | result])
  end

  defp parse_relative1([], result) do
    reverse_and_join(result, [])
  end

  defp reverse_and_join([x | rest], []) do
    reverse_and_join(rest, [x])
  end

  defp reverse_and_join([x | rest], result) do
    reverse_and_join(rest, [x, '\\' | result])
  end

  defp reverse_and_join([], result) do
    result
  end

  defp split_key(key) do
    split_key(key, [], [])
  end

  defp split_key([?\\ | rest], current, result) do
    split_key(rest, [], [:lists.reverse(current) | result])
  end

  defp split_key([c | rest], current, result) do
    split_key(rest, [c | current], result)
  end

  defp split_key([], [], result) do
    result
  end

  defp split_key([], current, result) do
    [:lists.reverse(current) | result]
  end

  defp parse_root([?\\ | rest], result) do
    root =
      case :lists.reverse(result) do
        [?h, ?k, ?e, ?y, ?_ | root0] ->
          root0

        root0 ->
          root0
      end

    case root_to_handle(:erlang.list_to_atom(root)) do
      false ->
        {:error, :enoent}

      handle ->
        {:ok, handle, rest}
    end
  end

  defp parse_root([c | rest], result) do
    parse_root(rest, [c | result])
  end

  defp parse_root([], result) do
    parse_root([?\\], result)
  end

  defp root_to_handle(:classes_root) do
    2_147_483_648
  end

  defp root_to_handle(:hkcr) do
    2_147_483_648
  end

  defp root_to_handle(:current_user) do
    2_147_483_649
  end

  defp root_to_handle(:hkcu) do
    2_147_483_649
  end

  defp root_to_handle(:local_machine) do
    2_147_483_650
  end

  defp root_to_handle(:hklm) do
    2_147_483_650
  end

  defp root_to_handle(:users) do
    2_147_483_651
  end

  defp root_to_handle(:hku) do
    2_147_483_651
  end

  defp root_to_handle(:current_config) do
    2_147_483_653
  end

  defp root_to_handle(:hkcc) do
    2_147_483_653
  end

  defp root_to_handle(:dyn_data) do
    2_147_483_654
  end

  defp root_to_handle(:hkdd) do
    2_147_483_654
  end

  defp root_to_handle(:performance_data) do
    2_147_483_652
  end

  defp root_to_handle(_) do
    false
  end

  defp hkey_to_string(2_147_483_648) do
    '\\hkey_classes_root'
  end

  defp hkey_to_string(2_147_483_649) do
    '\\hkey_current_user'
  end

  defp hkey_to_string(2_147_483_650) do
    '\\hkey_local_machine'
  end

  defp hkey_to_string(2_147_483_651) do
    '\\hkey_users'
  end

  defp hkey_to_string(2_147_483_652) do
    '\\hkey_performance_data'
  end

  defp hkey_to_string(2_147_483_653) do
    '\\hkey_current_config'
  end

  defp hkey_to_string(2_147_483_654) do
    '\\hkey_dyn_data'
  end

  defp open_mode([:read | rest], result) do
    open_mode(rest, [?r | result])
  end

  defp open_mode([:write | rest], result) do
    open_mode(rest, [?w | result])
  end

  defp open_mode([], result) do
    result
  end

  defp open_mode(_, _) do
    {:error, :einval}
  end
end
