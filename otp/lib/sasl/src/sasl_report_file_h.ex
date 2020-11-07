defmodule :m_sasl_report_file_h do
  use Bitwise

  def init({file, modes0, type}) when is_list(modes0) do
    :erlang.process_flag(:trap_exit, true)

    modes1 =
      case :lists.keymember(:encoding, 1, modes0) do
        true ->
          modes0

        false ->
          [{:encoding, :utf8} | modes0]
      end

    modes =
      case (for m <- modes1,
                :lists.member(m, [:write, :append, :exclusive]) do
              m
            end) do
        [] ->
          [:write | modes1]

        _ ->
          modes1
      end

    case :file.open(file, modes) do
      {:ok, fd} ->
        {:ok, {fd, file, type}}

      what ->
        what
    end
  end

  def handle_event({_Type, gL, _Msg}, state)
      when node(gL) != node() do
    {:ok, state}
  end

  def handle_event(event, {fd, file, type}) do
    _ = :sasl_report.write_report(fd, type, tag_event(event))
    {:ok, {fd, file, type}}
  end

  def handle_event(_, state) do
    {:ok, state}
  end

  def handle_info({:EXIT, fd, _Reason}, {fd, _File, _Type}) do
    :remove_handler
  end

  def handle_info(_, state) do
    {:ok, state}
  end

  def handle_call(_Query, _State) do
    {:error, :bad_query}
  end

  def terminate(_, {fd, _File, _Type}) do
    _ = :file.close(fd)
    []
  end

  defp tag_event(event) do
    {:calendar.local_time(), event}
  end
end
