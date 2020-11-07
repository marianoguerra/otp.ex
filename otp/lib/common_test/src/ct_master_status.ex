defmodule :m_ct_master_status do
  use Bitwise
  @behaviour :gen_event
  require Record
  Record.defrecord(:r_event, :event, name: :undefined, node: :undefined, data: :undefined)
  Record.defrecord(:r_state, :state, status: [])

  def init(_) do
    {:ok, r_state()}
  end

  def handle_event(r_event(name: name, node: node, data: data), state) do
    print('~n=== ~w ===~n', [:ct_master_status])
    print('~tw on ~w: ~tp~n', [name, node, data])
    {:ok, state}
  end

  def handle_call(_Req, state) do
    reply = :ok
    {:ok, reply, state}
  end

  def handle_info(_Info, state) do
    {:ok, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp print(_Str, _Args) do
    :ok
  end
end
