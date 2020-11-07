defmodule :m_dets_server do
  use Bitwise
  @behaviour :gen_server
  require Record

  Record.defrecord(:r_pending, :pending,
    tab: :undefined,
    ref: :undefined,
    pid: :undefined,
    from: :undefined,
    reqtype: :undefined,
    clients: :undefined
  )

  Record.defrecord(:r_state, :state, store: :undefined, parent: :undefined, pending: :undefined)

  Record.defrecord(:r_head, :head,
    m: :undefined,
    m2: :undefined,
    next: :undefined,
    fptr: :undefined,
    no_objects: :undefined,
    no_keys: :undefined,
    maxobjsize: :undefined,
    n: :undefined,
    type: :undefined,
    keypos: :undefined,
    freelists: :undefined,
    freelists_p: :undefined,
    no_collections: :undefined,
    auto_save: :undefined,
    update_mode: :undefined,
    fixed: false,
    hash_bif: :undefined,
    has_md5: :undefined,
    min_no_slots: :undefined,
    max_no_slots: :undefined,
    cache: :undefined,
    filename: :undefined,
    access: :read_write,
    ram_file: false,
    name: :undefined,
    parent: :undefined,
    server: :undefined,
    bump: :undefined,
    base: :undefined
  )

  Record.defrecord(:r_fileheader, :fileheader,
    freelist: :undefined,
    fl_base: :undefined,
    cookie: :undefined,
    closed_properly: :undefined,
    type: :undefined,
    version: :undefined,
    m: :undefined,
    next: :undefined,
    keypos: :undefined,
    no_objects: :undefined,
    no_keys: :undefined,
    min_no_slots: :undefined,
    max_no_slots: :undefined,
    no_colls: :undefined,
    hash_method: :undefined,
    read_md5: :undefined,
    has_md5: :undefined,
    md5: :undefined,
    trailer: :undefined,
    eof: :undefined,
    n: :undefined
  )

  Record.defrecord(:r_cache, :cache,
    cache: :undefined,
    csize: :undefined,
    inserts: :undefined,
    wrtime: :undefined,
    tsize: :undefined,
    delay: :undefined
  )

  def start_link() do
    :gen_server.start_link({:local, :dets}, :dets_server, [self()], [])
  end

  def start() do
    ensure_started()
  end

  def stop() do
    case :erlang.whereis(:dets) do
      :undefined ->
        :stopped

      _Pid ->
        :gen_server.call(:dets, :stop, :infinity)
    end
  end

  def all() do
    call(:all)
  end

  def close(tab) do
    call({:close, tab})
  end

  def get_pid(tab) do
    :ets.lookup_element(:dets_registry, tab, 3)
  end

  def open_file(file) do
    call({:open, file})
  end

  def open_file(tab, openArgs) do
    call({:open, tab, openArgs})
  end

  def pid2name(pid) do
    ensure_started()
    pid2name_1(pid)
  end

  def users(tab) do
    call({:users, tab})
  end

  def verbose(what) do
    call({:set_verbose, what})
  end

  defp call(message) do
    ensure_started()
    :gen_server.call(:dets, message, :infinity)
  end

  def init(parent) do
    store = init()
    {:ok, r_state(store: store, parent: parent, pending: [])}
  end

  def handle_call(:all, _From, state) do
    f = fn x, a ->
      [:erlang.element(1, x) | a]
    end

    {:reply, :ets.foldl(f, [], :dets_registry), state}
  end

  def handle_call({:close, tab}, from, state) do
    request([{{:close, tab}, from}], state)
  end

  def handle_call({:open, file}, from, state) do
    request([{{:open, file}, from}], state)
  end

  def handle_call({:open, tab, openArgs}, from, state) do
    request([{{:open, tab, openArgs}, from}], state)
  end

  def handle_call(:stop, _From, state) do
    {:stop, :normal, :stopped, state}
  end

  def handle_call({:set_verbose, what}, _From, state) do
    set_verbose(what)
    {:reply, :ok, state}
  end

  def handle_call({:users, tab}, _From, state) do
    users =
      :ets.select(
        r_state(state, :store),
        [{{:"$1", tab}, [], [:"$1"]}]
      )

    {:reply, users, state}
  end

  def handle_cast(_Msg, state) do
    {:noreply, state}
  end

  def handle_info({:pending_reply, {ref, result0}}, state) do
    {:value,
     r_pending(tab: tab, pid: pid, from: {fromPid, _Tag} = from, reqtype: reqT, clients: clients)} =
      :lists.keysearch(ref, r_pending(:ref), r_state(state, :pending))

    store = r_state(state, :store)

    result =
      case {result0, reqT} do
        {:ok, :add_user} ->
          do_link(store, fromPid)
          true = :ets.insert(store, {fromPid, tab})
          :ets.update_counter(:dets_registry, tab, 1)
          {:ok, tab}

        {:ok, :internal_open} ->
          :erlang.link(pid)
          do_link(store, fromPid)
          true = :ets.insert(store, {fromPid, tab})
          {:ok, tab}

        {reply, :internal_open} ->
          true = :ets.delete(:dets_registry, tab)
          true = :ets.delete(:dets_owners, pid)
          reply

        {reply, _} ->
          reply
      end

    :gen_server.reply(from, result)
    nP = :lists.keydelete(pid, r_pending(:pid), r_state(state, :pending))
    state1 = r_state(state, pending: nP)
    request(clients, state1)
  end

  def handle_info({:EXIT, pid, _Reason}, state) do
    store = r_state(state, :store)

    case pid2name_1(pid) do
      {:ok, tab} ->
        true = :ets.delete(:dets_registry, tab)
        true = :ets.delete(:dets_owners, pid)

        users =
          :ets.select(
            r_state(state, :store),
            [{{:"$1", tab}, [], [:"$1"]}]
          )

        true = :ets.match_delete(store, {:_, tab})

        :lists.foreach(
          fn user ->
            do_unlink(store, user)
          end,
          users
        )

        {:noreply, state}

      :undefined ->
        f = fn {fromPid, tab}, s ->
          {_, s1} = handle_close(s, {:close, tab}, {fromPid, :notag}, tab)
          s1
        end

        state1 = :lists.foldl(f, state, :ets.lookup(store, pid))
        {:noreply, state1}
    end
  end

  def handle_info(_Message, state) do
    {:noreply, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp ensure_started() do
    case :erlang.whereis(:dets) do
      :undefined ->
        detsSup =
          {:dets_sup, {:dets_sup, :start_link, []}, :permanent, 1000, :supervisor, [:dets_sup]}

        _ = :supervisor.start_child(:kernel_safe_sup, detsSup)

        detsServer =
          {:dets, {:dets_server, :start_link, []}, :permanent, 2000, :worker, [:dets_server]}

        _ =
          :supervisor.start_child(
            :kernel_safe_sup,
            detsServer
          )

        :ok

      _ ->
        :ok
    end
  end

  defp init() do
    set_verbose(verbose_flag())
    :erlang.process_flag(:trap_exit, true)

    :dets_registry =
      :ets.new(
        :dets_registry,
        [:set, :named_table]
      )

    :dets_owners =
      :ets.new(
        :dets_owners,
        [:set, :named_table]
      )

    :ets.new(:dets, [:duplicate_bag])
  end

  defp verbose_flag() do
    case :init.get_argument(:dets) do
      {:ok, args} ->
        :lists.member(['verbose'], args)

      _ ->
        false
    end
  end

  defp set_verbose(true) do
    :erlang.put(:verbose, :yes)
  end

  defp set_verbose(_) do
    :erlang.erase(:verbose)
  end

  defp pid2name_1(pid) do
    case :ets.lookup(:dets_owners, pid) do
      [] ->
        :undefined

      [{_Pid, tab}] ->
        {:ok, tab}
    end
  end

  defp request([{req, from} | l], state) do
    res =
      case req do
        {:close, tab} ->
          handle_close(state, req, from, tab)

        {:open, file} ->
          do_internal_open(state, from, [file, :erlang.get(:verbose)])

        {:open, tab, openArgs} ->
          do_open(state, req, from, openArgs, tab)
      end

    state2 =
      case res do
        {:pending, state1} ->
          state1

        {reply, state1} ->
          :gen_server.reply(from, reply)
          state1
      end

    request(l, state2)
  end

  defp request([], state) do
    {:noreply, state}
  end

  defp do_open(state, req, from, args, tab) do
    case check_pending(tab, from, state, req) do
      {:pending, newState} ->
        {:pending, newState}

      false ->
        case :ets.lookup(:dets_registry, tab) do
          [] ->
            a = [tab, args, :erlang.get(:verbose)]
            do_internal_open(state, from, a)

          [{^tab, _Counter, pid}] ->
            pending_call(tab, pid, make_ref(), from, args, :add_user, state)
        end
    end
  end

  defp do_internal_open(state, from, args) do
    case :supervisor.start_child(:dets_sup, [self()]) do
      {:ok, pid} ->
        ref = make_ref()

        tab =
          case args do
            [t, _, _] ->
              t

            [_, _] ->
              ref
          end

        true = :ets.insert(:dets_registry, {tab, 1, pid})
        true = :ets.insert(:dets_owners, {pid, tab})
        pending_call(tab, pid, ref, from, args, :internal_open, state)

      error ->
        {error, state}
    end
  end

  defp handle_close(state, req, {fromPid, _Tag} = from, tab) do
    case check_pending(tab, from, state, req) do
      {:pending, newState} ->
        {:pending, newState}

      false ->
        store = r_state(state, :store)

        case :ets.match_object(store, {fromPid, tab}) do
          [] ->
            :void
            {{:error, :not_owner}, state}

          [_ | keep] ->
            case :ets.lookup(:dets_registry, tab) do
              [{^tab, 1, pid}] ->
                do_unlink(store, fromPid)
                true = :ets.delete(:dets_registry, tab)
                true = :ets.delete(:dets_owners, pid)
                true = :ets.match_delete(store, {fromPid, tab})
                :erlang.unlink(pid)
                pending_call(tab, pid, make_ref(), from, [], :internal_close, state)

              [{^tab, _Counter, pid}] ->
                do_unlink(store, fromPid)
                true = :ets.match_delete(store, {fromPid, tab})
                true = :ets.insert(store, keep)
                :ets.update_counter(:dets_registry, tab, -1)
                pending_call(tab, pid, make_ref(), from, [], :remove_user, state)
            end
        end
    end
  end

  defp do_link(store, pid) do
    key = {:links, pid}

    case :ets.lookup(store, key) do
      [] ->
        true = :ets.insert(store, {key, 1})
        :erlang.link(pid)

      [{_, c}] ->
        true = :ets.delete(store, key)
        true = :ets.insert(store, {key, c + 1})
    end
  end

  defp do_unlink(store, pid) do
    key = {:links, pid}

    case :ets.lookup(store, key) do
      [{_, c}] when c > 1 ->
        true = :ets.delete(store, key)
        true = :ets.insert(store, {key, c - 1})

      _ ->
        true = :ets.delete(store, key)
        :erlang.unlink(pid)
    end
  end

  defp pending_call(tab, pid, ref, {fromPid, _Tag} = from, args, reqT, state) do
    server = self()

    f = fn ->
      res =
        case reqT do
          :add_user ->
            :dets.add_user(pid, tab, args)

          :internal_open ->
            :dets.internal_open(pid, ref, args)

          :internal_close ->
            :dets.internal_close(pid)

          :remove_user ->
            :dets.remove_user(pid, fromPid)
        end

      send(server, {:pending_reply, {ref, res}})
    end

    _ = spawn(f)
    pD = r_pending(tab: tab, ref: ref, pid: pid, reqtype: reqT, from: from, clients: [])
    p = [pD | r_state(state, :pending)]
    {:pending, r_state(state, pending: p)}
  end

  defp check_pending(tab, from, state, req) do
    case :lists.keysearch(tab, r_pending(:tab), r_state(state, :pending)) do
      {:value, r_pending(tab: ^tab, clients: clients) = p} ->
        nP =
          :lists.keyreplace(
            tab,
            r_pending(:tab),
            r_state(state, :pending),
            r_pending(p, clients: clients ++ [{req, from}])
          )

        {:pending, r_state(state, pending: nP)}

      false ->
        false
    end
  end
end
