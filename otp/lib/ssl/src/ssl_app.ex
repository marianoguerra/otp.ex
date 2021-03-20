defmodule :m_ssl_app do
  use Bitwise
  @behaviour :application
  def start(_Type, _StartArgs) do
    start_logger()
    :ssl_sup.start_link()
  end

  def stop(_State) do
    stop_logger()
    :ok
  end

  defp start_logger() do
    config = %{level: :debug, filter_default: :stop, formatter: {:ssl_logger, %{}}}
    filter = {&:logger_filters.domain/2, {:log, :sub, [:otp, :ssl]}}
    :logger.add_handler(:ssl_handler, :logger_std_h, config)
    :logger.add_handler_filter(:ssl_handler, :filter_non_ssl, filter)
    :logger.set_module_level([:ssl_logger], :debug)
  end

  defp stop_logger() do
    :logger.unset_application_level(:ssl)
    :logger.remove_handler(:ssl_handler)
  end
end
