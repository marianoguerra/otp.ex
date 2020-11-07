defmodule :m_erl_signal_handler do
  use Bitwise
  @behaviour :gen_event
  require Record
  Record.defrecord(:r_state, :state, [])

  def start() do
    case :erlang.whereis(:erl_signal_server) do
      :undefined ->
        :ok

      _ ->
        :gen_event.add_handler(:erl_signal_server, :erl_signal_handler, [])
    end
  end

  def init(_Args) do
    {:ok, r_state()}
  end

  def handle_event(:sigusr1, s) do
    :erlang.halt('Received SIGUSR1')
    {:ok, s}
  end

  def handle_event(:sigquit, s) do
    :erlang.halt()
    {:ok, s}
  end

  def handle_event(:sigterm, s) do
    :error_logger.info_msg('SIGTERM received - shutting down~n')
    :ok = :init.stop()
    {:ok, s}
  end

  def handle_event(_SignalMsg, s) do
    {:ok, s}
  end

  def handle_info(_Info, s) do
    {:ok, s}
  end

  def handle_call(_Request, s) do
    {:ok, :ok, s}
  end

  def format_status(_Opt, [_Pdict, _S]) do
    :ok
  end

  def code_change(_OldVsn, s, _Extra) do
    {:ok, s}
  end

  def terminate(_Args, _S) do
    :ok
  end
end
