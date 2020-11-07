defmodule :m_mnesia_sp do
  use Bitwise

  def init_proc(who, mod, fun, args) do
    :mnesia_lib.verbose('~p starting: ~p~n', [who, self()])

    try do
      apply(mod, fun, args)
    catch
      :exit, reason
      when reason === :shutdown or
             reason === :kill or reason === :normal ->
        :mnesia_monitor.terminate_proc(who, reason, args)
        exit(reason)

      _, reason ->
        :mnesia_monitor.terminate_proc(who, {reason, __STACKTRACE__}, args)
        exit(reason)
    end
  end
end
