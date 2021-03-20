defmodule :cowboy do
  use Bitwise
  def start_clear(ref, transOpts0, protoOpts0) do
    transOpts1 = :ranch.normalize_opts(transOpts0)
    {transOpts,
       connectionType} = ensure_connection_type(transOpts1)
    protoOpts = Map.put(protoOpts0, :connection_type,
                                      connectionType)
    :ranch.start_listener(ref, :ranch_tcp, transOpts,
                            :cowboy_clear, protoOpts)
  end

  def start_tls(ref, transOpts0, protoOpts0) do
    transOpts1 = :ranch.normalize_opts(transOpts0)
    socketOpts = :maps.get(:socket_opts, transOpts1, [])
    transOpts2 = Map.put(transOpts1, :socket_opts,
                                       [{:next_protocols_advertised, ["h2", "http/1.1"]},
                                            {:alpn_preferred_protocols,
                                               ["h2", "http/1.1"]} |
                                                socketOpts])
    {transOpts,
       connectionType} = ensure_connection_type(transOpts2)
    protoOpts = Map.put(protoOpts0, :connection_type,
                                      connectionType)
    :ranch.start_listener(ref, :ranch_ssl, transOpts,
                            :cowboy_tls, protoOpts)
  end

  defp ensure_connection_type(transOpts = %{connection_type:
                        connectionType}) do
    {transOpts, connectionType}
  end

  defp ensure_connection_type(transOpts) do
    {Map.put(transOpts, :connection_type, :supervisor),
       :supervisor}
  end

  def stop_listener(ref) do
    :ranch.stop_listener(ref)
  end

  def set_env(ref, name, value) do
    opts = :ranch.get_protocol_options(ref)
    env = :maps.get(:env, opts, %{})
    opts2 = :maps.put(:env, :maps.put(name, value, env),
                        opts)
    :ok = :ranch.set_protocol_options(ref, opts2)
  end

  def log({:log, level, format, args}, opts) do
    log(level, format, args, opts)
  end

  def log(level, format, args, %{logger: logger})
      when logger !== :error_logger do
    _ = apply(logger, level, [format, args])
    :ok
  end

  def log(level, format, args, _) do
    function = (case (level) do
                  :emergency ->
                    :error_msg
                  :alert ->
                    :error_msg
                  :critical ->
                    :error_msg
                  :error ->
                    :error_msg
                  :warning ->
                    :warning_msg
                  :notice ->
                    :warning_msg
                  :info ->
                    :info_msg
                  :debug ->
                    :info_msg
                end)
    apply(:error_logger, function, [format, args])
  end

end