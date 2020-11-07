defmodule :m_dtls_listener_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link() do
    :supervisor.start_link({:local, :dtls_listener_sup}, :dtls_listener_sup, [])
  end

  def start_child(args) do
    :supervisor.start_child(:dtls_listener_sup, args)
  end

  def lookup_listener(iP, port) do
    try do
      :ets.lookup(:dtls_listener_sup, {iP, port})
    catch
      _, _ ->
        :undefined
    else
      [] ->
        :undefined

      [{{^iP, ^port}, {owner, handler}}] ->
        case :erlang.is_process_alive(handler) do
          true ->
            case owner !== :undefined and :erlang.is_process_alive(owner) do
              true ->
                {:error, :already_listening}

              false ->
                {:ok, handler}
            end

          false ->
            :ets.delete(:dtls_listener_sup, {iP, port})
            :undefined
        end
    end
  end

  def register_listener(ownerAndListner, iP, port) do
    :ets.insert(
      :dtls_listener_sup,
      {{iP, port}, ownerAndListner}
    )
  end

  def init(_O) do
    :ets.new(
      :dtls_listener_sup,
      [:named_table, :public, :set]
    )

    restartStrategy = :simple_one_for_one
    maxR = 0
    maxT = 3600
    name = :undefined
    startFunc = {:dtls_packet_demux, :start_link, []}
    restart = :temporary
    shutdown = 4000
    modules = [:dtls_packet_demux]
    type = :worker
    childSpec = {name, startFunc, restart, shutdown, type, modules}
    {:ok, {{restartStrategy, maxR, maxT}, [childSpec]}}
  end
end
