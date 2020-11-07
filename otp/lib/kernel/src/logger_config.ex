defmodule :m_logger_config do
  use Bitwise

  def new(name) do
    _ =
      :ets.new(
        name,
        [:set, :protected, :named_table, {:read_concurrency, true}, {:write_concurrency, true}]
      )

    :ets.whereis(name)
  end

  def delete(tid, what) do
    :persistent_term.put(
      {:logger_config, table_key(what)},
      :undefined
    )

    :ets.delete(tid, table_key(what))
  end

  def allow(level, module) do
    modLevel =
      case :persistent_term.get(
             {:logger_config, module},
             :undefined
           ) do
        :undefined ->
          intLevel = get_primary_level()
          :persistent_term.put({:logger_config, module}, intLevel)
          intLevel

        intLevel ->
          cond do
            intLevel <= 10 ->
              intLevel

            true ->
              intLevel - 16
          end
      end

    less_or_equal_level(level, modLevel)
  end

  def allow(level) do
    primaryLevelInt = get_primary_level()
    less_or_equal_level(level, primaryLevelInt)
  end

  defp less_or_equal_level(:emergency, modLevel) do
    0 <= modLevel
  end

  defp less_or_equal_level(:alert, modLevel) do
    1 <= modLevel
  end

  defp less_or_equal_level(:critical, modLevel) do
    2 <= modLevel
  end

  defp less_or_equal_level(:error, modLevel) do
    3 <= modLevel
  end

  defp less_or_equal_level(:warning, modLevel) do
    4 <= modLevel
  end

  defp less_or_equal_level(:notice, modLevel) do
    5 <= modLevel
  end

  defp less_or_equal_level(:info, modLevel) do
    6 <= modLevel
  end

  defp less_or_equal_level(:debug, modLevel) do
    7 <= modLevel
  end

  def exist(tid, what) do
    :ets.member(tid, table_key(what))
  end

  defp get_primary_level() do
    :persistent_term.get({:logger_config, :"$primary_config$"}, 5)
  end

  def get(tid, what) do
    case :ets.lookup(tid, table_key(what)) do
      [{_, config}] ->
        {:ok, config}

      [] ->
        {:error, {:not_found, what}}
    end
  end

  def get(tid, what, level) do
    tableKey = table_key(what)

    case :persistent_term.get(
           {:logger_config, tableKey},
           :undefined
         ) do
      :undefined ->
        {:error, {:not_found, what}}

      confLevel ->
        case less_or_equal_level(level, confLevel) do
          true ->
            get(tid, what)

          false ->
            :error
        end
    end
  end

  def create(tid, :proxy, config) do
    :ets.insert(tid, {table_key(:proxy), config})
  end

  def create(tid, what, config) do
    levelInt = level_to_int(:maps.get(:level, config))

    :ok =
      :persistent_term.put(
        {:logger_config, table_key(what)},
        levelInt
      )

    :ets.insert(tid, {table_key(what), config})
  end

  def set(tid, :proxy, config) do
    :ets.insert(tid, {table_key(:proxy), config})
    :ok
  end

  def set(tid, what, config) do
    levelInt = level_to_int(:maps.get(:level, config))

    :ok =
      :persistent_term.put(
        {:logger_config, table_key(what)},
        levelInt
      )

    case what do
      :primary ->
        for {{:logger_config, module} = key, level} <- :persistent_term.get(),
            is_atom(module) and module !== :"$primary_config$",
            level <= 10 do
          :persistent_term.put(key, levelInt)
        end

        :ok

      _ ->
        :ok
    end

    :ets.insert(tid, {table_key(what), config})
    :ok
  end

  def set_module_level(modules, level) do
    levelInt = level_to_int(level)

    for module <- modules do
      :persistent_term.put(
        {:logger_config, module},
        levelInt + 16
      )
    end

    :ok
  end

  def unset_module_level(:all) do
    primaryLevel = get_primary_level()

    for {{:logger_config, module} = key, _} <- :persistent_term.get(),
        is_atom(module) and module !== :"$primary_config$" do
      :persistent_term.put(key, primaryLevel)
    end

    :ok
  end

  def unset_module_level(modules) do
    primaryLevel = get_primary_level()

    for module <- modules do
      :persistent_term.put(
        {:logger_config, module},
        primaryLevel
      )
    end

    :ok
  end

  def get_module_level() do
    :lists.sort(
      for {{:logger_config, module}, level} <- :persistent_term.get(),
          is_atom(module) and module !== :"$primary_config$",
          not (level <= 10) do
        {module,
         int_to_level(
           cond do
             level <= 10 ->
               level

             true ->
               level - 16
           end
         )}
      end
    )
  end

  def level_to_int(:none) do
    -1
  end

  def level_to_int(:emergency) do
    0
  end

  def level_to_int(:alert) do
    1
  end

  def level_to_int(:critical) do
    2
  end

  def level_to_int(:error) do
    3
  end

  def level_to_int(:warning) do
    4
  end

  def level_to_int(:notice) do
    5
  end

  def level_to_int(:info) do
    6
  end

  def level_to_int(:debug) do
    7
  end

  def level_to_int(:all) do
    10
  end

  defp int_to_level(-1) do
    :none
  end

  defp int_to_level(0) do
    :emergency
  end

  defp int_to_level(1) do
    :alert
  end

  defp int_to_level(2) do
    :critical
  end

  defp int_to_level(3) do
    :error
  end

  defp int_to_level(4) do
    :warning
  end

  defp int_to_level(5) do
    :notice
  end

  defp int_to_level(6) do
    :info
  end

  defp int_to_level(7) do
    :debug
  end

  defp int_to_level(10) do
    :all
  end

  defp table_key(:proxy) do
    :"$proxy_config$"
  end

  defp table_key(:primary) do
    :"$primary_config$"
  end

  defp table_key(handlerId) do
    {:"$handler_config$", handlerId}
  end
end
