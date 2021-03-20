defmodule :cowboy_clear do
  use Bitwise
  @behaviour :ranch_protocol
  def start_link(ref, _Socket, transport, opts) do
    start_link(ref, transport, opts)
  end

  def start_link(ref, transport, opts) do
    pid = :proc_lib.spawn_link(:cowboy_clear,
                                 :connection_process,
                                 [self(), ref, transport, opts])
    {:ok, pid}
  end

  def connection_process(parent, ref, transport, opts) do
    proxyInfo = (case (:maps.get(:proxy_header, opts,
                                   false)) do
                   true ->
                     {:ok, proxyInfo0} = :ranch.recv_proxy_header(ref, 1000)
                     proxyInfo0
                   false ->
                     :undefined
                 end)
    {:ok, socket} = :ranch.handshake(ref)
    protocol = (case (:maps.get(:protocols, opts,
                                  [:http2, :http])) do
                  [:http2] ->
                    :cowboy_http2
                  [_ | _] ->
                    :cowboy_http
                end)
    init(parent, ref, socket, transport, proxyInfo, opts,
           protocol)
  end

  defp init(parent, ref, socket, transport, proxyInfo, opts,
            protocol) do
    _ = (case (:maps.get(:connection_type, opts,
                           :supervisor)) do
           :worker ->
             :ok
           :supervisor ->
             :erlang.process_flag(:trap_exit, true)
         end)
    protocol.init(parent, ref, socket, transport, proxyInfo,
                    opts)
  end

end