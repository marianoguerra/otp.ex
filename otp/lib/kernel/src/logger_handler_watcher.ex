defmodule :m_logger_handler_watcher do
  use Bitwise
  @behaviour :gen_server
  require Record
  Record.defrecord(:r_state, :state, handlers: :undefined)

  def start_link() do
    :gen_server.start_link({:local, :logger_handler_watcher}, :logger_handler_watcher, [], [])
  end

  def register_handler(id, pid) do
    :gen_server.call(
      :logger_handler_watcher,
      {:register, id, pid}
    )
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)
    {:ok, r_state(handlers: [])}
  end

  def handle_call({:register, id, pid}, _From, r_state(handlers: hs) = state) do
    ref = :erlang.monitor(:process, pid)
    hs1 = :lists.keystore(id, 1, hs, {id, ref})
    {:reply, :ok, r_state(state, handlers: hs1)}
  end

  def handle_cast(_Request, state) do
    {:noreply, state}
  end

  def handle_info(
        {:DOWN, ref, :process, _, :shutdown},
        r_state(handlers: hs) = state
      ) do
    case :lists.keytake(ref, 2, hs) do
      {:value, {id, ^ref}, hs1} ->
        _ =
          case :logger.get_handler_config(id) do
            {:ok, _} ->
              :logger.remove_handler(id)

            _ ->
              :ok
          end

        {:noreply, r_state(state, handlers: hs1)}

      false ->
        {:noreply, state}
    end
  end

  def handle_info(
        {:DOWN, ref, :process, _, _OtherReason},
        r_state(handlers: hs) = state
      ) do
    {:noreply, r_state(state, handlers: :lists.keydelete(ref, 2, hs))}
  end

  def handle_info(_Other, state) do
    {:noreply, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end
end
