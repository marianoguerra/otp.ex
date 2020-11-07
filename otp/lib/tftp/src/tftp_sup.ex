defmodule :m_tftp_sup do
  use Bitwise
  @behaviour :supervisor
  def start_link(tftpServices) do
    :supervisor.start_link({:local, :tftp_sup}, :tftp_sup, [tftpServices])
  end

  def start_child(options) do
    killAfter = default_kill_after()
    childSpec = worker_spec(killAfter, options)
    :supervisor.start_child(:tftp_sup, childSpec)
  end

  def stop_child(pid) when is_pid(pid) do
    children = :supervisor.which_children(:tftp_sup)

    case (for {id, p, _Type, _Modules} <- children,
              p === pid do
            id
          end) do
      [] ->
        {:error, :not_found}

      [id] ->
        case :supervisor.terminate_child(:tftp_sup, id) do
          :ok ->
            :supervisor.delete_child(:tftp_sup, id)

          {:error, :not_found} ->
            :supervisor.delete_child(:tftp_sup, id)

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  def which_children() do
    children = :supervisor.which_children(:tftp_sup)

    for {_Id, pid, _Type, _Modules} <- children,
        pid !== :undefined do
      {:tftpd, pid}
    end
  end

  def init([services]) when is_list(services) do
    restartStrategy = :one_for_one
    maxR = 10
    maxT = 3600
    killAfter = default_kill_after()

    children =
      for {:tftpd, options} <- services do
        worker_spec(killAfter, options)
      end

    {:ok, {{restartStrategy, maxR, maxT}, children}}
  end

  defp worker_spec(killAfter, options) do
    modules = [:proc_lib, :tftp, :tftp_engine]
    kA = supervisor_timeout(killAfter)
    name = unique_name(options)
    {name, {:tftp, :start, [options]}, :permanent, kA, :worker, modules}
  end

  defp unique_name(options) do
    case :lists.keysearch(:port, 1, options) do
      {:value, {_, port}} when is_integer(port) and port > 0 ->
        {:tftpd, port}

      _ ->
        {:tftpd, :erlang.unique_integer([:positive])}
    end
  end

  defp default_kill_after() do
    :timer.seconds(3)
  end

  defp supervisor_timeout(killAfter) do
    killAfter
  end
end
