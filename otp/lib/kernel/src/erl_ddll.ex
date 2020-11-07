defmodule :m_erl_ddll do
  use Bitwise

  def demonitor(_) do
    :erlang.nif_error(:undef)
  end

  def info(_, _) do
    :erlang.nif_error(:undef)
  end

  def format_error_int(_) do
    :erlang.nif_error(:undef)
  end

  def monitor(_, _) do
    :erlang.nif_error(:undef)
  end

  def try_load(_, _, _) do
    :erlang.nif_error(:undef)
  end

  def try_unload(_, _) do
    :erlang.nif_error(:undef)
  end

  def loaded_drivers() do
    :erlang.nif_error(:undef)
  end

  def start() do
    {:error, {:already_started, :undefined}}
  end

  def stop() do
    :ok
  end

  def load_driver(path, driver) do
    do_load_driver(path, driver, [{:driver_options, [:kill_ports]}])
  end

  def load(path, driver) do
    do_load_driver(path, driver, [])
  end

  defp do_load_driver(path, driver, driverFlags) do
    case :erl_ddll.try_load(path, driver, [{:monitor, :pending_driver}] ++ driverFlags) do
      {:error, :inconsistent} ->
        {:error, :bad_driver_name}

      {:error, what} ->
        {:error, what}

      {:ok, :already_loaded} ->
        :ok

      {:ok, :loaded} ->
        :ok

      {:ok, :pending_driver, ref} ->
        receive do
          {:DOWN, ^ref, :driver, _, :load_cancelled} ->
            {:error, :load_cancelled}

          {:UP, ^ref, :driver, _, :permanent} ->
            {:error, :permanent}

          {:DOWN, ^ref, :driver, _, {:load_failure, failure}} ->
            {:error, failure}

          {:UP, ^ref, :driver, _, :loaded} ->
            :ok
        end
    end
  end

  defp do_unload_driver(driver, flags) do
    case :erl_ddll.try_unload(driver, flags) do
      {:error, what} ->
        {:error, what}

      {:ok, :pending_process} ->
        :ok

      {:ok, :unloaded} ->
        :ok

      {:ok, :pending_driver} ->
        :ok

      {:ok, :pending_driver, ref} ->
        receive do
          {:UP, ^ref, :driver, _, :permanent} ->
            {:error, :permanent}

          {:UP, ^ref, :driver, _, :unload_cancelled} ->
            :ok

          {:DOWN, ^ref, :driver, _, :unloaded} ->
            :ok
        end
    end
  end

  def unload_driver(driver) do
    do_unload_driver(
      driver,
      [{:monitor, :pending_driver}, :kill_ports]
    )
  end

  def unload(driver) do
    do_unload_driver(driver, [])
  end

  def reload(path, driver) do
    do_load_driver(path, driver, [{:reload, :pending_driver}])
  end

  def reload_driver(path, driver) do
    do_load_driver(path, driver, [{:reload, :pending_driver}, {:driver_options, [:kill_ports]}])
  end

  def format_error(code) do
    case code do
      :load_cancelled ->
        'Loading was cancelled from other process'

      _ ->
        :erl_ddll.format_error_int(code)
    end
  end

  def info(driver) do
    [
      {:processes, :erl_ddll.info(driver, :processes)},
      {:driver_options, :erl_ddll.info(driver, :driver_options)},
      {:port_count, :erl_ddll.info(driver, :port_count)},
      {:linked_in_driver, :erl_ddll.info(driver, :linked_in_driver)},
      {:permanent, :erl_ddll.info(driver, :permanent)},
      {:awaiting_load, :erl_ddll.info(driver, :awaiting_load)},
      {:awaiting_unload, :erl_ddll.info(driver, :awaiting_unload)}
    ]
  end

  def info() do
    {:ok, driverList} = :erl_ddll.loaded_drivers()

    for x <- driverList,
        y <- [
          try do
            info(x)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
        ],
        is_list(y),
        not :lists.member({:linked_in_driver, true}, y) do
      {x, y}
    end
  end
end
