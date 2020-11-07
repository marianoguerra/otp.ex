defmodule :m_mnesia_ext_sup do
  use Bitwise
  @behaviour :supervisor
  def start() do
    :supervisor.start_link({:local, :mnesia_ext_sup}, :mnesia_ext_sup, [])
  end

  def start_proc(name, m, f, a) do
    start_proc(name, m, f, a, [])
  end

  def start_proc(name, m, f, a, opts) do
    [restart, shutdown, type, modules] =
      for {k, default} <- [
            {:restart, :transient},
            {:shutdown, 120_000},
            {:type, :worker},
            {:modules, [m]}
          ] do
        :proplists.get_value(k, opts, default)
      end

    case :supervisor.start_child(
           :mnesia_ext_sup,
           {name, {m, f, a}, restart, shutdown, type, modules}
         ) do
      {:error, :already_present} ->
        :supervisor.restart_child(:mnesia_ext_sup, name)

      other ->
        other
    end
  end

  def stop_proc(name) do
    :supervisor.terminate_child(:mnesia_ext_sup, name)
  end

  def init(_) do
    {:ok, {{:one_for_all, 10, 60}, []}}
  end
end
