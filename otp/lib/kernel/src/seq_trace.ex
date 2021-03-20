defmodule :m_seq_trace do
  use Bitwise

  def set_token([]) do
    :erlang.seq_trace(:sequential_trace_token, [])
  end

  def set_token({flags, label, serial, _From, lastcnt}) do
    f = decode_flags(flags)

    set_token2([
      {:label, label},
      {:serial, {lastcnt, serial}}
      | f
    ])
  end

  def set_token(type, val) do
    :erlang.seq_trace(type, val)
  end

  def get_token() do
    :erlang.element(
      2,
      :erlang.process_info(self(), :sequential_trace_token)
    )
  end

  def get_token(type) do
    :erlang.seq_trace_info(type)
  end

  def print(term) do
    :erlang.seq_trace_print(term)
    :ok
  end

  def print(label, term) when is_atom(label) do
    :erlang.error(:badarg, [label, term])
  end

  def print(label, term) do
    :erlang.seq_trace_print(label, term)
    :ok
  end

  def reset_trace() do
    :erlang.system_flag(:reset_seq_trace, true)
  end

  def set_system_tracer({module, state} = tracer) do
    case :erlang.module_loaded(module) do
      false ->
        module.enabled(:trace_status, :erlang.self(), state)

      true ->
        :ok
    end

    :erlang.system_flag(:sequential_tracer, tracer)
  end

  def set_system_tracer(tracer) do
    :erlang.system_flag(:sequential_tracer, tracer)
  end

  def get_system_tracer() do
    :erlang.element(
      2,
      :erlang.system_info(:sequential_tracer)
    )
  end

  defp set_token2([{type, val} | t]) do
    _ = :erlang.seq_trace(type, val)
    set_token2(t)
  end

  defp set_token2([]) do
    :ok
  end

  defp decode_flags(flags) do
    print = flags &&& 4 > 0
    send = flags &&& 1 > 0
    rec = flags &&& 2 > 0
    nowTs = flags &&& 8 > 0
    strictMonTs = flags &&& 16 > 0
    monTs = flags &&& 32 > 0

    [
      {:print, print},
      {:send, send},
      {:receive, rec},
      {:timestamp, nowTs},
      {:strict_monotonic_timestamp, strictMonTs},
      {:monotonic_timestamp, monTs}
    ]
  end
end
