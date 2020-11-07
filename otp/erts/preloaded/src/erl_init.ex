defmodule :m_erl_init do
  use Bitwise

  def start(mod, bootArgs) do
    :zlib.on_load()
    :erl_tracer.on_load()
    :prim_buffer.on_load()
    :prim_file.on_load()

    conditional_load(
      :prim_socket,
      [:prim_socket, :prim_net]
    )

    run(mod, :boot, bootArgs)
  end

  defp run(m, f, a) do
    case :erlang.function_exported(m, f, 1) do
      false ->
        :erlang.display({:fatal, :error, :module, m, 'does not export', f, '/1'})
        :erlang.halt(1)

      true ->
        apply(m, f, [a])
    end
  end

  defp conditional_load(condMod, mods2Load) do
    loaded = :erlang.loaded()
    conditional_load(condMod, loaded, mods2Load)
  end

  defp conditional_load(_CondMod, [], _Mods2LOad) do
    :ok
  end

  defp conditional_load(condMod, [condMod | _], mods2Load) do
    on_load(mods2Load)
  end

  defp conditional_load(condMod, [_ | t], mods2Load) do
    conditional_load(condMod, t, mods2Load)
  end

  defp on_load([]) do
    :ok
  end

  defp on_load([mod | mods]) do
    mod.on_load()
    on_load(mods)
  end
end
