defmodule :m_dbg_debugged do
  use Bitwise

  def eval(mod, func, args) do
    meta = :dbg_ieval.eval(mod, func, args)
    mref = :erlang.monitor(:process, meta)
    msg_loop(meta, mref)
  end

  defp msg_loop(meta, mref) do
    receive do
      {:sys, ^meta, {:ready, val}} ->
        :erlang.demonitor(mref, [:flush])

        case val do
          {:dbg_apply, m, f, a} ->
            apply(m, f, a)

          _ ->
            val
        end

      {:sys, ^meta, {:exception, {class, reason, stacktrace}}} ->
        :erlang.demonitor(mref, [:flush])

        :erlang.error(
          :erlang.raise(class, reason, stacktrace),
          [class, reason, stacktrace]
        )

      {:sys, ^meta, {:receive, msg}} ->
        receive do
          ^msg ->
            send(meta, {self(), :rec_acked})
            :ok
        end

        msg_loop(meta, mref)

      {:sys, ^meta, {:command, command}} ->
        reply = handle_command(command)
        send(meta, {:sys, self(), reply})
        msg_loop(meta, mref)

      {:DOWN, ^mref, _, _, reason} ->
        {:interpreter_terminated, reason}
    end
  end

  defp handle_command(command) do
    try do
      reply(command)
    catch
      class, reason ->
        {:exception, {class, reason, stacktrace_f(__STACKTRACE__)}}
    end
  end

  defp reply({:apply, m, f, as}) do
    {:value, :erlang.apply(m, f, as)}
  end

  defp reply({:eval, expr, bs}) do
    :erl_eval.expr(expr, :lists.sort(bs))
  end

  defp stacktrace_f([]) do
    []
  end

  defp stacktrace_f([{:dbg_debugged, _, _, _} | _]) do
    []
  end

  defp stacktrace_f([f | s]) do
    [f | stacktrace_f(s)]
  end
end
