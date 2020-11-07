defmodule :m_disk_log_server do
  use Bitwise
  @behaviour :gen_server
  require Record

  Record.defrecord(:r_file_info, :file_info,
    size: :undefined,
    type: :undefined,
    access: :undefined,
    atime: :undefined,
    mtime: :undefined,
    ctime: :undefined,
    mode: :undefined,
    links: :undefined,
    major_device: :undefined,
    minor_device: :undefined,
    inode: :undefined,
    uid: :undefined,
    gid: :undefined
  )

  Record.defrecord(:r_file_descriptor, :file_descriptor,
    module: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_arg, :arg,
    name: 0,
    version: :undefined,
    file: :none,
    repair: true,
    size: :infinity,
    type: :halt,
    format: :internal,
    linkto: self(),
    head: :none,
    mode: :read_write,
    notify: false,
    quiet: false,
    options: []
  )

  Record.defrecord(:r_cache, :cache, fd: :undefined, sz: 0, c: [])
  Record.defrecord(:r_halt, :halt, fdc: :undefined, curB: :undefined, size: :undefined)

  Record.defrecord(:r_handle, :handle,
    filename: :undefined,
    maxB: :undefined,
    maxF: :undefined,
    curB: :undefined,
    curF: :undefined,
    cur_fdc: :undefined,
    cur_name: :undefined,
    cur_cnt: :undefined,
    acc_cnt: :undefined,
    firstPos: :undefined,
    noFull: :undefined,
    accFull: :undefined
  )

  Record.defrecord(:r_log, :log,
    status: :ok,
    name: :undefined,
    blocked_by: :none,
    users: 0,
    filename: :undefined,
    owners: [],
    type: :undefined,
    format: :undefined,
    format_type: :undefined,
    head: :none,
    mode: :undefined,
    size: :undefined,
    extra: :undefined,
    version: :undefined
  )

  Record.defrecord(:r_continuation, :continuation, pid: self(), pos: :undefined, b: :undefined)

  Record.defrecord(:r_pending, :pending,
    log: :undefined,
    pid: :undefined,
    req: :undefined,
    from: :undefined,
    attach: :undefined,
    clients: :undefined
  )

  Record.defrecord(:r_state, :state, pending: [])

  def start_link() do
    :gen_server.start_link({:local, :disk_log_server}, :disk_log_server, [], [])
  end

  def start() do
    ensure_started()
  end

  def open({:ok, a}) do
    ensure_started()
    :gen_server.call(:disk_log_server, {:open, a}, :infinity)
  end

  def open(other) do
    other
  end

  def close(pid) do
    :gen_server.call(:disk_log_server, {:close, pid}, :infinity)
  end

  def get_log_pid(logName) do
    do_get_log_pid(logName)
  end

  def all() do
    ensure_started()
    do_all()
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)
    _ = :ets.new(:disk_log_names, [:named_table, :set])
    _ = :ets.new(:disk_log_pids, [:named_table, :set])
    {:ok, r_state()}
  end

  def handle_call({:open, a}, from, state) do
    open([{{:open, a}, from}], state)
  end

  def handle_call({:close, pid}, _From, state) do
    reply = do_close(pid)
    {:reply, reply, state}
  end

  def handle_info({:pending_reply, pid, result0}, state) do
    {:value,
     r_pending(log: name, pid: ^pid, from: from, req: request, attach: attach, clients: clients)} =
      :lists.keysearch(pid, r_pending(:pid), r_state(state, :pending))

    nP = :lists.keydelete(pid, r_pending(:pid), r_state(state, :pending))
    state1 = r_state(state, pending: nP)

    cond do
      attach and result0 === {:error, :no_such_log} ->
        open([{request, from} | clients], state1)

      true ->
        case result0 do
          _ when attach ->
            :ok

          {:error, _} ->
            :ok

          _ ->
            :erlang.put(pid, name)
            :erlang.link(pid)
            :ets.insert(:disk_log_pids, {pid, name})
            :ets.insert(:disk_log_names, {name, pid})
        end

        :gen_server.reply(from, result0)
        open(clients, state1)
    end
  end

  def handle_info({:EXIT, pid, _Reason}, state) do
    case :erlang.get(pid) do
      :undefined ->
        :ok

      name ->
        erase_log(name, pid)
    end

    {:noreply, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  def terminate(_Reason, _) do
    :ok
  end

  defp ensure_started() do
    case :erlang.whereis(:disk_log_server) do
      :undefined ->
        logSup =
          {:disk_log_sup, {:disk_log_sup, :start_link, []}, :permanent, 1000, :supervisor,
           [:disk_log_sup]}

        {:ok, _} =
          ensure_child_started(
            :kernel_safe_sup,
            logSup
          )

        logServer =
          {:disk_log_server, {:disk_log_server, :start_link, []}, :permanent, 2000, :worker,
           [:disk_log_server]}

        {:ok, _} =
          ensure_child_started(
            :kernel_safe_sup,
            logServer
          )

        :ok

      _ ->
        :ok
    end
  end

  defp ensure_child_started(sup, child) do
    case :supervisor.start_child(sup, child) do
      {:ok, pid} ->
        {:ok, pid}

      {:error, {:already_started, pid}} ->
        {:ok, pid}

      error ->
        error
    end
  end

  defp open([{req, from} | l], state) do
    state2 =
      case do_open(req, from, state) do
        {:pending, state1} ->
          state1

        {reply, state1} ->
          :gen_server.reply(from, reply)
          state1
      end

    open(l, state2)
  end

  defp open([], state) do
    {:noreply, state}
  end

  defp do_open({:open, r_arg(name: name)} = req, from, state) do
    case check_pending(name, from, state, req) do
      {:pending, newState} ->
        {:pending, newState}

      false ->
        case do_get_log_pid(name) do
          :undefined ->
            start_log(name, req, from, state)

          pid ->
            do_internal_open(name, pid, from, req, true, state)
        end
    end
  end

  defp start_log(name, req, from, state) do
    server = self()

    case :supervisor.start_child(
           :disk_log_sup,
           [server]
         ) do
      {:ok, pid} ->
        do_internal_open(name, pid, from, req, false, state)

      error ->
        {error, state}
    end
  end

  defp do_internal_open(name, pid, from, {:open, a} = req, attach, state) do
    server = self()

    f = fn ->
      res = :disk_log.internal_open(pid, a)
      send(server, {:pending_reply, pid, res})
    end

    _ = spawn(f)
    pD = r_pending(log: name, pid: pid, req: req, from: from, attach: attach, clients: [])
    p = [pD | r_state(state, :pending)]
    {:pending, r_state(state, pending: p)}
  end

  defp check_pending(name, from, state, req) do
    case :lists.keysearch(name, r_pending(:log), r_state(state, :pending)) do
      {:value, r_pending(log: ^name, clients: clients) = p} ->
        nP =
          :lists.keyreplace(
            name,
            r_pending(:log),
            r_state(state, :pending),
            r_pending(p, clients: clients ++ [{req, from}])
          )

        {:pending, r_state(state, pending: nP)}

      false ->
        false
    end
  end

  defp do_close(pid) do
    case :erlang.get(pid) do
      :undefined ->
        :ok

      name ->
        erase_log(name, pid)
        :erlang.unlink(pid)
        :ok
    end
  end

  defp erase_log(name, pid) do
    case do_get_log_pid(name) do
      :undefined ->
        :ok

      ^pid ->
        true = :ets.delete(:disk_log_names, name)
        true = :ets.delete(:disk_log_pids, pid)
    end

    :erlang.erase(pid)
  end

  defp do_all() do
    localSpec = {:"$1", :_, :local}

    local0 =
      for l <-
            :ets.match(
              :disk_log_names,
              localSpec
            ) do
        hd(l)
      end

    :lists.sort(local0)
  end

  defp do_get_log_pid(logName) do
    try do
      :ets.lookup(:disk_log_names, logName)
    catch
      _, _ ->
        :undefined
    else
      [{^logName, pid}] ->
        pid

      [] ->
        :undefined
    end
  end
end
