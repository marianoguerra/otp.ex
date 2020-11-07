defmodule :m_application_starter do
  use Bitwise

  def start([], _Type, _Apps) do
    :ok
  end

  def start([{phase, _PhaseArgs} | phases], type, apps) do
    case start_apps(phase, type, apps) do
      {:error, _} = error ->
        error

      _ ->
        start(phases, type, apps)
    end
  end

  defp start_apps(_Phase, _Type, []) do
    :ok
  end

  defp start_apps(phase, type, [app | apps]) do
    case (try do
            run_start_phase(phase, type, app)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, _} = error ->
        error

      _ ->
        start_apps(phase, type, apps)
    end
  end

  defp run_start_phase(phase, type, app) do
    {:ok, {mod, arg}} = :application.get_key(app, :mod)

    case mod do
      :application_starter ->
        [startMod, _StartArgs] = arg
        run_the_phase(phase, type, app, startMod)

        {:ok, incApps} =
          :application.get_key(
            app,
            :included_applications
          )

        start_apps(phase, type, incApps)

      _ ->
        run_the_phase(phase, type, app, mod)
    end
  end

  defp run_the_phase(phase, type, app, mod) do
    start_phases =
      case :application_controller.get_key(
             app,
             :start_phases
           ) do
        {:ok, :undefined} ->
          throw({:error, {:start_phases_undefined, app}})

        {:ok, sp} ->
          sp
      end

    case :lists.keyfind(phase, 1, start_phases) do
      false ->
        :ok

      {^phase, phaseArgs} ->
        case (try do
                mod.start_phase(phase, type, phaseArgs)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          :ok ->
            :ok

          {:error, reason} ->
            throw({:error, {reason, {mod, :start_phase, [phase, type, phaseArgs]}}})

          other ->
            throw(
              {:error,
               {:bad_return_value, {{mod, :start_phase, [phase, type, phaseArgs]}, other}}}
            )
        end
    end
  end
end
