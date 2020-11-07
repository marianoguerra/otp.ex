defmodule :m_tftp_app do
  use Bitwise
  @behaviour :application
  def start(_StartType, _StartArgs) do
    config = get_configuration()
    :tftp_sup.start_link(config)
  end

  def stop(_State) do
    :ok
  end

  defp get_configuration() do
    case (try do
            :application.get_env(:tftp, :services)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, services} ->
        services

      _ ->
        []
    end
  end
end
