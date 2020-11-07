defmodule :m_erl_tracer do
  use Bitwise

  def on_load() do
    case :erlang.load_nif(
           :erlang.atom_to_list(:erl_tracer),
           0
         ) do
      :ok ->
        :ok
    end
  end

  def enabled(_, _, _) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def trace(_, _, _, _, _) do
    :erlang.nif_error(:nif_not_loaded)
  end
end
