defmodule :m_file_server do
  use Bitwise
  @behaviour :gen_server
  def format_error({_Line, :file_server, reason}) do
    :io_lib.format('~w', [reason])
  end

  def format_error({_Line, mod, reason}) do
    mod.format_error(reason)
  end

  def format_error(errorId) do
    :erl_posix_msg.message(errorId)
  end

  def start() do
    do_start(:start)
  end

  def start_link() do
    do_start(:start_link)
  end

  def stop() do
    :gen_server.call(:file_server_2, :stop, :infinity)
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)

    :file_io_servers =
      :ets.new(
        :file_io_servers,
        [:named_table]
      )

    {:ok, :undefined}
  end

  def handle_call({:open, name, modeList}, {pid, _Tag} = _From, state)
      when is_list(modeList) do
    child = :file_io_server.start_link(pid, name, modeList)

    case child do
      {:ok, p} when is_pid(p) ->
        :ets.insert(:file_io_servers, {p, name})

      _ ->
        :ok
    end

    {:reply, child, state}
  end

  def handle_call({:open, _Name, _Mode}, _From, state) do
    {:reply, {:error, :einval}, state}
  end

  def handle_call({:read_file, name}, _From, state) do
    {:reply, :prim_file.read_file(name), state}
  end

  def handle_call({:write_file, name, bin}, _From, state) do
    {:reply, :prim_file.write_file(name, bin), state}
  end

  def handle_call({:set_cwd, name}, _From, state) do
    {:reply, :prim_file.set_cwd(name), state}
  end

  def handle_call({:delete, name}, _From, state) do
    {:reply, :prim_file.delete(name), state}
  end

  def handle_call({:rename, fr, to}, _From, state) do
    {:reply, :prim_file.rename(fr, to), state}
  end

  def handle_call({:make_dir, name}, _From, state) do
    {:reply, :prim_file.make_dir(name), state}
  end

  def handle_call({:del_dir, name}, _From, state) do
    {:reply, :prim_file.del_dir(name), state}
  end

  def handle_call({:list_dir, name}, _From, state) do
    {:reply, :prim_file.list_dir(name), state}
  end

  def handle_call({:list_dir_all, name}, _From, state) do
    {:reply, :prim_file.list_dir_all(name), state}
  end

  def handle_call(:get_cwd, _From, state) do
    {:reply, :prim_file.get_cwd(), state}
  end

  def handle_call({:get_cwd}, _From, state) do
    {:reply, :prim_file.get_cwd(), state}
  end

  def handle_call({:get_cwd, name}, _From, state) do
    {:reply, :prim_file.get_cwd(name), state}
  end

  def handle_call({:read_file_info, name}, _From, state) do
    {:reply, :prim_file.read_file_info(name), state}
  end

  def handle_call({:read_file_info, name, opts}, _From, state) do
    {:reply, :prim_file.read_file_info(name, opts), state}
  end

  def handle_call({:altname, name}, _From, state) do
    {:reply, :prim_file.altname(name), state}
  end

  def handle_call({:write_file_info, name, info}, _From, state) do
    {:reply, :prim_file.write_file_info(name, info), state}
  end

  def handle_call({:write_file_info, name, info, opts}, _From, state) do
    {:reply, :prim_file.write_file_info(name, info, opts), state}
  end

  def handle_call({:read_link_info, name}, _From, state) do
    {:reply, :prim_file.read_link_info(name), state}
  end

  def handle_call({:read_link_info, name, opts}, _From, state) do
    {:reply, :prim_file.read_link_info(name, opts), state}
  end

  def handle_call({:read_link, name}, _From, state) do
    {:reply, :prim_file.read_link(name), state}
  end

  def handle_call({:read_link_all, name}, _From, state) do
    {:reply, :prim_file.read_link_all(name), state}
  end

  def handle_call({:make_link, old, new}, _From, state) do
    {:reply, :prim_file.make_link(old, new), state}
  end

  def handle_call({:make_symlink, old, new}, _From, state) do
    {:reply, :prim_file.make_symlink(old, new), state}
  end

  def handle_call({:copy, sourceName, sourceOpts, destName, destOpts, length}, _From, state) do
    reply =
      case :prim_file.open(
             sourceName,
             [[:read, :binary] | sourceOpts]
           ) do
        {:ok, source} ->
          sourceReply =
            case :prim_file.open(
                   destName,
                   [
                     [:write, :binary]
                     | destOpts
                   ]
                 ) do
              {:ok, dest} ->
                destReply = :prim_file.copy(source, dest, length)
                :prim_file.close(dest)
                destReply

              {:error, _} = error ->
                error
            end

          :prim_file.close(source)
          sourceReply

        {:error, _} = error ->
          error
      end

    {:reply, reply, state}
  end

  def handle_call(:stop, _From, state) do
    {:stop, :normal, :stopped, state}
  end

  def handle_call(request, from, state) do
    :error_logger.error_msg('handle_call(~tp, ~tp, _)', [request, from])
    {:noreply, state}
  end

  def handle_cast(msg, state) do
    :error_logger.error_msg('handle_cast(~tp, _)', [msg])
    {:noreply, state}
  end

  def handle_info({:EXIT, pid, _Reason}, state) when is_pid(pid) do
    :ets.delete(:file_io_servers, pid)
    {:noreply, state}
  end

  def handle_info(info, state) do
    :error_logger.error_msg('handle_Info(~tp, _)', [info])
    {:noreply, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp do_start(start) do
    case :init.get_argument(:master) do
      :error ->
        apply(:gen_server, start, [{:local, :file_server_2}, :file_server, [], []])

      {:ok, [[node]]} ->
        do_start(start, :erlang.list_to_atom(node), :file_server_2)

      x ->
        {:error, {:get_argument, :master, x}}
    end
  end

  defp do_start(start, node, name) do
    case :rpc.call(node, :erlang, :whereis, [name]) do
      filer when is_pid(filer) or filer === :undefined ->
        case (try do
                do_start_slave(start, filer, name)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, reason} ->
            {:error, reason}

          result ->
            result
        end

      other ->
        {:error, {:no_master, other}}
    end
  end

  defp do_start_slave(:start_link, filer, name) do
    self = self()
    token = make_ref()

    slave =
      spawn_link(fn ->
        relay_start(self, token, filer, name)
      end)

    receive do
      {:started, ^token} ->
        {:ok, slave}
    end
  end

  defp do_start_slave(:start, filer, name) do
    self = self()
    token = make_ref()

    slave =
      spawn(fn ->
        relay_start(self, token, filer, name)
      end)

    slaveMonitor = :erlang.monitor(:process, slave)

    receive do
      {:started, ^token} ->
        :erlang.demonitor(slaveMonitor, [:flush])
        {:ok, slave}

      {:DOWN, ^slaveMonitor, _, _, reason} ->
        exit(reason)
    end
  end

  defp relay_start(parent, token, filer, name)
       when is_pid(filer) do
    case (try do
            :erlang.register(name, self())
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      true ->
        :ok

      _ ->
        exit({:already_started, :erlang.whereis(name)})
    end

    filerMonitor = :erlang.monitor(:process, filer)
    :erlang.process_flag(:trap_exit, true)
    send(parent, {:started, token})
    relay_loop(parent, filer, filerMonitor)
  end

  defp relay_start(parent, token, :undefined, _Name) do
    :erlang.process_flag(:trap_exit, true)
    send(parent, {:started, token})

    receive do
      {:EXIT, ^parent, reason} ->
        exit(reason)
    end
  end

  defp relay_loop(parent, filer, filerMonitor) do
    receive do
      {:DOWN, ^filerMonitor, _, _, reason} ->
        exit(reason)

      {:EXIT, ^parent, reason} ->
        exit(reason)

      msg ->
        send(filer, msg)
    end

    relay_loop(parent, filer, filerMonitor)
  end
end
