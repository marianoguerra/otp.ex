defmodule :m_application do
  use Bitwise

  def load(application) do
    load1(application, [])
  end

  def load(application, distNodes) do
    load1(application, distNodes)
  end

  defp load1(application, distNodes) do
    case :application_controller.load_application(application) do
      :ok when distNodes !== [] ->
        appName = get_appl_name(application)

        case :dist_ac.load_application(appName, distNodes) do
          :ok ->
            :ok

          {:error, r} ->
            :application_controller.unload_application(appName)
            {:error, r}
        end

      else__ ->
        else__
    end
  end

  def unload(application) do
    :application_controller.unload_application(application)
  end

  def ensure_all_started(application) do
    ensure_all_started(application, :temporary)
  end

  def ensure_all_started(application, type) do
    case ensure_all_started(application, type, []) do
      {:ok, started} ->
        {:ok, :lists.reverse(started)}

      {:error, reason, started} ->
        _ =
          for app <- started do
            stop(app)
          end

        {:error, reason}
    end
  end

  defp ensure_all_started(application, type, started) do
    case start(application, type) do
      :ok ->
        {:ok, [application | started]}

      {:error, {:already_started, ^application}} ->
        {:ok, started}

      {:error, {:not_started, dependency}} ->
        case ensure_all_started(dependency, type, started) do
          {:ok, newStarted} ->
            ensure_all_started(application, type, newStarted)

          error ->
            error
        end

      {:error, reason} ->
        {:error, {application, reason}, started}
    end
  end

  def start(application) do
    start(application, :temporary)
  end

  def start(application, restartType) do
    case load(application) do
      :ok ->
        name = get_appl_name(application)

        :application_controller.start_application(
          name,
          restartType
        )

      {:error, {:already_loaded, name}} ->
        :application_controller.start_application(
          name,
          restartType
        )

      error ->
        error
    end
  end

  def ensure_started(application) do
    ensure_started(application, :temporary)
  end

  def ensure_started(application, restartType) do
    case start(application, restartType) do
      :ok ->
        :ok

      {:error, {:already_started, ^application}} ->
        :ok

      error ->
        error
    end
  end

  def start_boot(application) do
    start_boot(application, :temporary)
  end

  def start_boot(application, restartType) do
    :application_controller.start_boot_application(
      application,
      restartType
    )
  end

  def takeover(application, restartType) do
    :dist_ac.takeover_application(application, restartType)
  end

  def permit(application, bool) do
    case bool do
      true ->
        :ok

      false ->
        :ok

      bad ->
        exit({:badarg, {:application, :permit, [application, bad]}})
    end

    case :application_controller.permit_application(
           application,
           bool
         ) do
      :distributed_application ->
        :dist_ac.permit_application(application, bool)

      {:distributed_application, :only_loaded} ->
        :dist_ac.permit_only_loaded_application(
          application,
          bool
        )

      localResult ->
        localResult
    end
  end

  def stop(application) do
    :application_controller.stop_application(application)
  end

  def which_applications() do
    :application_controller.which_applications()
  end

  def which_applications(:infinity) do
    :application_controller.which_applications(:infinity)
  end

  def which_applications(timeout)
      when is_integer(timeout) and
             timeout >= 0 do
    :application_controller.which_applications(timeout)
  end

  def loaded_applications() do
    :application_controller.loaded_applications()
  end

  def info() do
    :application_controller.info()
  end

  def set_env(config) when is_list(config) do
    set_env(config, [])
  end

  def set_env(config, opts)
      when is_list(config) and
             is_list(opts) do
    case :application_controller.set_env(config, opts) do
      :ok ->
        :ok

      {:error, msg} ->
        :erlang.error({:badarg, msg}, [config, opts])
    end
  end

  def set_env(application, key, val) do
    :application_controller.set_env(application, key, val)
  end

  def set_env(application, key, val, :infinity) do
    set_env(application, key, val, [{:timeout, :infinity}])
  end

  def set_env(application, key, val, timeout)
      when is_integer(timeout) and timeout >= 0 do
    set_env(application, key, val, [{:timeout, timeout}])
  end

  def set_env(application, key, val, opts)
      when is_list(opts) do
    :application_controller.set_env(application, key, val, opts)
  end

  def unset_env(application, key) do
    :application_controller.unset_env(application, key)
  end

  def unset_env(application, key, :infinity) do
    unset_env(application, key, [{:timeout, :infinity}])
  end

  def unset_env(application, key, timeout)
      when is_integer(timeout) and timeout >= 0 do
    unset_env(application, key, [{:timeout, timeout}])
  end

  def unset_env(application, key, opts) when is_list(opts) do
    :application_controller.unset_env(application, key, opts)
  end

  def get_env(key) do
    :application_controller.get_pid_env(
      :erlang.group_leader(),
      key
    )
  end

  def get_env(application, key) do
    :application_controller.get_env(application, key)
  end

  def get_env(application, key, def__) do
    case get_env(application, key) do
      {:ok, val} ->
        val

      :undefined ->
        def__
    end
  end

  def get_all_env() do
    :application_controller.get_pid_all_env(:erlang.group_leader())
  end

  def get_all_env(application) do
    :application_controller.get_all_env(application)
  end

  def get_key(key) do
    :application_controller.get_pid_key(
      :erlang.group_leader(),
      key
    )
  end

  def get_key(application, key) do
    :application_controller.get_key(application, key)
  end

  def get_all_key() do
    :application_controller.get_pid_all_key(:erlang.group_leader())
  end

  def get_all_key(application) do
    :application_controller.get_all_key(application)
  end

  def get_application() do
    :application_controller.get_application(:erlang.group_leader())
  end

  def get_application(pid) when is_pid(pid) do
    case :erlang.process_info(pid, :group_leader) do
      {:group_leader, gl} ->
        :application_controller.get_application(gl)

      :undefined ->
        :undefined
    end
  end

  def get_application(module) when is_atom(module) do
    :application_controller.get_application_module(module)
  end

  def start_type() do
    :application_controller.start_type(:erlang.group_leader())
  end

  defp get_appl_name(name) when is_atom(name) do
    name
  end

  defp get_appl_name({:application, name, _}) when is_atom(name) do
    name
  end
end
