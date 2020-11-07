defmodule :m_global do
  use Bitwise
  import Kernel, except: [send: 2]
  @behaviour :gen_server
  require Record

  Record.defrecord(:r_state, :state,
    connect_all: :undefined,
    known: [],
    synced: [],
    resolvers: [],
    syncers: [],
    node_name: node(),
    the_locker: :undefined,
    the_registrar: :undefined,
    trace: :undefined,
    global_lock_down: false
  )

  def start() do
    :gen_server.start({:local, :global_name_server}, :global, [], [])
  end

  def start_link() do
    :gen_server.start_link({:local, :global_name_server}, :global, [], [])
  end

  def stop() do
    :gen_server.call(:global_name_server, :stop, :infinity)
  end

  def sync() do
    case check_sync_nodes() do
      {:error, _} = error ->
        error

      syncNodes ->
        :gen_server.call(:global_name_server, {:sync, syncNodes}, :infinity)
    end
  end

  def sync(nodes) do
    case check_sync_nodes(nodes) do
      {:error, _} = error ->
        error

      syncNodes ->
        :gen_server.call(:global_name_server, {:sync, syncNodes}, :infinity)
    end
  end

  def send(name, msg) do
    case whereis_name(name) do
      pid when is_pid(pid) ->
        send(pid, msg)
        pid

      :undefined ->
        exit({:badarg, {name, msg}})
    end
  end

  def whereis_name(name) do
    where(name)
  end

  def node_disconnected(node) do
    send(:global_name_server, {:nodedown, node})
  end

  def register_name(name, pid) when is_pid(pid) do
    register_name(name, pid, &random_exit_name/3)
  end

  def register_name(name, pid, method0) when is_pid(pid) do
    method = allow_tuple_fun(method0)

    fun = fn nodes ->
      case where(name) === :undefined and
             check_dupname(
               name,
               pid
             ) do
        true ->
          :gen_server.multi_call(nodes, :global_name_server, {:register, name, pid, method})
          :yes

        _ ->
          :no
      end
    end

    :ok
    :gen_server.call(:global_name_server, {:registrar, fun}, :infinity)
  end

  defp check_dupname(name, pid) do
    case :ets.lookup(:global_pid_names, pid) do
      [] ->
        true

      pidNames ->
        case :application.get_env(
               :kernel,
               :global_multi_name_action
             ) do
          {:ok, :allow} ->
            true

          _ ->
            s = 'global: ~w registered under several names: ~tw\n'

            names = [
              name
              | for {_Pid, name1} <- pidNames do
                  name1
                end
            ]

            :error_logger.error_msg(s, [pid, names])
            false
        end
    end
  end

  def unregister_name(name) do
    case where(name) do
      :undefined ->
        :ok

      _ ->
        fun = fn nodes ->
          :gen_server.multi_call(nodes, :global_name_server, {:unregister, name})
          :ok
        end

        :ok
        :gen_server.call(:global_name_server, {:registrar, fun}, :infinity)
    end
  end

  def re_register_name(name, pid) when is_pid(pid) do
    re_register_name(name, pid, &random_exit_name/3)
  end

  def re_register_name(name, pid, method0) when is_pid(pid) do
    method = allow_tuple_fun(method0)

    fun = fn nodes ->
      :gen_server.multi_call(nodes, :global_name_server, {:register, name, pid, method})
      :yes
    end

    :ok
    :gen_server.call(:global_name_server, {:registrar, fun}, :infinity)
  end

  def registered_names() do
    mS =
      :ets.fun2ms(fn {name, _Pid, _M, _R} ->
        name
      end)

    :ets.select(:global_names, mS)
  end

  def register_name_external(name, pid) when is_pid(pid) do
    register_name_external(name, pid, &random_exit_name/3)
  end

  def register_name_external(name, pid, method) when is_pid(pid) do
    fun = fn nodes ->
      case where(name) do
        :undefined ->
          :gen_server.multi_call(
            nodes,
            :global_name_server,
            {:register_ext, name, pid, method, node()}
          )

          :yes

        _Pid ->
          :no
      end
    end

    :ok
    :gen_server.call(:global_name_server, {:registrar, fun}, :infinity)
  end

  def unregister_name_external(name) do
    unregister_name(name)
  end

  def set_lock(id) do
    set_lock(id, [node() | :erlang.nodes()], :infinity, 1)
  end

  def set_lock(id, nodes) do
    set_lock(id, nodes, :infinity, 1)
  end

  def set_lock(id, nodes, retries)
      when is_integer(retries) and
             retries >= 0 do
    set_lock(id, nodes, retries, 1)
  end

  def set_lock(id, nodes, :infinity) do
    set_lock(id, nodes, :infinity, 1)
  end

  defp set_lock({_ResourceId, _LockRequesterId}, [], _Retries, _Times) do
    true
  end

  defp set_lock({_ResourceId, _LockRequesterId} = id, nodes, retries, times) do
    :ok

    case set_lock_on_nodes(id, nodes) do
      true ->
        :ok
        true

      false = reply when retries === 0 ->
        reply

      false ->
        random_sleep(times)
        set_lock(id, nodes, dec(retries), times + 1)
    end
  end

  def del_lock(id) do
    del_lock(id, [node() | :erlang.nodes()])
  end

  def del_lock({_ResourceId, _LockRequesterId} = id, nodes) do
    :ok
    :gen_server.multi_call(nodes, :global_name_server, {:del_lock, id})
    true
  end

  def trans(id, fun) do
    trans(id, fun, [node() | :erlang.nodes()], :infinity)
  end

  def trans(id, fun, nodes) do
    trans(id, fun, nodes, :infinity)
  end

  def trans(id, fun, nodes, retries) do
    case set_lock(id, nodes, retries) do
      true ->
        try do
          fun.()
        after
          del_lock(id, nodes)
        end

      false ->
        :aborted
    end
  end

  def info() do
    :gen_server.call(:global_name_server, :info, :infinity)
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)

    _ =
      :ets.new(
        :global_locks,
        [:set, :named_table, :protected]
      )

    _ =
      :ets.new(
        :global_names,
        [:set, :named_table, :protected, {:read_concurrency, true}]
      )

    _ =
      :ets.new(
        :global_names_ext,
        [:set, :named_table, :protected]
      )

    _ =
      :ets.new(
        :global_pid_names,
        [:bag, :named_table, :protected]
      )

    _ =
      :ets.new(
        :global_pid_ids,
        [:bag, :named_table, :protected]
      )

    doTrace = :os.getenv('GLOBAL_HIGH_LEVEL_TRACE') === 'TRUE'

    t0 =
      case doTrace do
        true ->
          send_high_level_trace()
          []

        false ->
          :no_trace
      end

    ca =
      case :init.get_argument(:connect_all) do
        {:ok, [['false']]} ->
          false

        _ ->
          true
      end

    s =
      r_state(
        the_locker: start_the_locker(doTrace),
        trace: t0,
        the_registrar: start_the_registrar(),
        connect_all: ca
      )

    {:ok, trace_message(s, {:init, node()}, [])}
  end

  def handle_call({:registrar, fun}, from, s) do
    send(r_state(s, :the_registrar), {:trans_all_known, fun, from})
    {:noreply, s}
  end

  def handle_call({:register, name, pid, method}, {fromPid, _Tag}, s0) do
    s = ins_name(name, pid, method, fromPid, [], s0)
    {:reply, :yes, s}
  end

  def handle_call({:unregister, name}, _From, s0) do
    s = delete_global_name2(name, s0)
    {:reply, :ok, s}
  end

  def handle_call({:register_ext, name, pid, method, regNode}, {fromPid, _Tag}, s0) do
    s = ins_name_ext(name, pid, method, regNode, fromPid, [], s0)
    {:reply, :yes, s}
  end

  def handle_call({:set_lock, lock}, {pid, _Tag}, s0) do
    {reply, s} = handle_set_lock(lock, pid, s0)
    {:reply, reply, s}
  end

  def handle_call({:del_lock, lock}, {pid, _Tag}, s0) do
    s = handle_del_lock(lock, pid, s0)
    {:reply, true, s}
  end

  def handle_call(:get_known, _From, s) do
    {:reply, r_state(s, :known), s}
  end

  def handle_call(:get_synced, _From, s) do
    {:reply, r_state(s, :synced), s}
  end

  def handle_call({:sync, nodes}, from, s) do
    pid =
      start_sync(
        :lists.delete(
          node(),
          nodes
        ) -- r_state(s, :synced),
        from
      )

    {:noreply, r_state(s, syncers: [pid | r_state(s, :syncers)])}
  end

  def handle_call(:get_protocol_version, _From, s) do
    {:reply, 5, s}
  end

  def handle_call(:get_names_ext, _From, s) do
    {:reply, get_names_ext(), s}
  end

  def handle_call(:info, _From, s) do
    {:reply, s, s}
  end

  def handle_call(:high_level_trace_start, _From, s) do
    send(r_state(s, :the_locker), {:do_trace, true})
    send_high_level_trace()
    {:reply, :ok, trace_message(r_state(s, trace: []), {:init, node()}, [])}
  end

  def handle_call(:high_level_trace_stop, _From, s) do
    r_state(the_locker: theLocker, trace: trace) = s
    send(theLocker, {:do_trace, false})
    wait_high_level_trace()
    {:reply, trace, r_state(s, trace: :no_trace)}
  end

  def handle_call(:high_level_trace_get, _From, r_state(trace: trace) = s) do
    {:reply, trace, r_state(s, trace: [])}
  end

  def handle_call(:stop, _From, s) do
    {:stop, :normal, :stopped, s}
  end

  def handle_call(request, from, s) do
    :error_logger.warning_msg(
      'The global_name_server received an unexpected message:\nhandle_call(~tp, ~tp, _)\n',
      [request, from]
    )

    {:noreply, s}
  end

  def handle_cast({:init_connect, vsn, node, initMsg}, s) do
    :ok

    case vsn do
      {hisVsn, hisTag} when hisVsn > 5 ->
        init_connect(5, node, initMsg, hisTag, r_state(s, :resolvers), s)

      {hisVsn, hisTag} ->
        init_connect(hisVsn, node, initMsg, hisTag, r_state(s, :resolvers), s)

      tuple when is_tuple(tuple) ->
        list = :erlang.tuple_to_list(tuple)
        [[_HisVsn, hisTag] | _] = list
        init_connect(5, node, initMsg, hisTag, r_state(s, :resolvers), s)

      _ ->
        txt = :io_lib.format('Illegal global protocol version ~p Node: ~p\n', [vsn, node])
        :error_logger.info_report(:lists.flatten(txt))
    end

    {:noreply, s}
  end

  def handle_cast({:lock_is_set, node, myTag, lockId}, s) do
    :ok

    case :erlang.get({:sync_tag_my, node}) do
      ^myTag ->
        lock_is_set(node, r_state(s, :resolvers), lockId)
        {:noreply, s}

      _ ->
        newS = cancel_locker(node, s, myTag)
        {:noreply, newS}
    end
  end

  def handle_cast(
        {:exchange, node, nameList, _NameExtList, myTag},
        s
      ) do
    case :erlang.get({:sync_tag_my, node}) do
      ^myTag ->
        exchange(node, nameList, r_state(s, :resolvers))
        {:noreply, s}

      _ ->
        newS = cancel_locker(node, s, myTag)
        {:noreply, newS}
    end
  end

  def handle_cast(
        {:exchange_ops, node, myTag, ops, resolved},
        s0
      ) do
    :ok
    s = trace_message(s0, {:exit_resolver, node}, [myTag])

    case :erlang.get({:sync_tag_my, node}) do
      ^myTag ->
        known = r_state(s, :known)

        :gen_server.cast(
          {:global_name_server, node},
          {:resolved, node(), resolved, known, known, get_names_ext(),
           :erlang.get({:sync_tag_his, node})}
        )

        case :erlang.get({:save_ops, node}) do
          {:resolved, hisKnown, names_ext, hisResolved} ->
            :erlang.put({:save_ops, node}, ops)
            newS = resolved(node, hisResolved, hisKnown, names_ext, s)
            {:noreply, newS}

          :undefined ->
            :erlang.put({:save_ops, node}, ops)
            {:noreply, s}
        end

      _ ->
        newS = cancel_locker(node, s, myTag)
        {:noreply, newS}
    end
  end

  def handle_cast(
        {:resolved, node, hisResolved, hisKnown, _HisKnown_v2, names_ext, myTag},
        s
      ) do
    :ok

    case :erlang.get({:sync_tag_my, node}) do
      ^myTag ->
        case :erlang.get({:save_ops, node}) do
          ops when is_list(ops) ->
            newS = resolved(node, hisResolved, hisKnown, names_ext, s)
            {:noreply, newS}

          :undefined ->
            resolved = {:resolved, hisKnown, names_ext, hisResolved}
            :erlang.put({:save_ops, node}, resolved)
            {:noreply, s}
        end

      _ ->
        newS = cancel_locker(node, s, myTag)
        {:noreply, newS}
    end
  end

  def handle_cast(
        {:new_nodes, node, ops, names_ext, nodes, extraInfo},
        s
      ) do
    :ok
    newS = new_nodes(ops, node, names_ext, nodes, extraInfo, s)
    {:noreply, newS}
  end

  def handle_cast({:in_sync, node, _IsKnown}, s) do
    :ok

    :lists.foreach(
      fn pid ->
        send(pid, {:synced, [node]})
      end,
      r_state(s, :syncers)
    )

    newS = cancel_locker(node, s, :erlang.get({:sync_tag_my, node}))
    reset_node_state(node)

    nSynced =
      case :lists.member(
             node,
             synced = r_state(newS, :synced)
           ) do
        true ->
          synced

        false ->
          [node | synced]
      end

    {:noreply, r_state(newS, synced: nSynced)}
  end

  def handle_cast({:async_del_name, _Name, _Pid}, s) do
    {:noreply, s}
  end

  def handle_cast({:async_del_lock, _ResourceId, _Pid}, s) do
    {:noreply, s}
  end

  def handle_cast(request, s) do
    :error_logger.warning_msg(
      'The global_name_server received an unexpected message:\nhandle_cast(~tp, _)\n',
      [request]
    )

    {:noreply, s}
  end

  def handle_info(
        {:EXIT, locker, _Reason} = exit,
        r_state(the_locker: locker) = s
      ) do
    {:stop, {:locker_died, exit}, r_state(s, the_locker: :undefined)}
  end

  def handle_info(
        {:EXIT, registrar, _} = exit,
        r_state(the_registrar: registrar) = s
      ) do
    {:stop, {:registrar_died, exit}, r_state(s, the_registrar: :undefined)}
  end

  def handle_info({:EXIT, pid, _Reason}, s) when is_pid(pid) do
    :ok
    syncers = :lists.delete(pid, r_state(s, :syncers))
    {:noreply, r_state(s, syncers: syncers)}
  end

  def handle_info({:nodedown, node}, s)
      when node === r_state(s, :node_name) do
    {:noreply, change_our_node_name(node(), s)}
  end

  def handle_info({:nodedown, node}, s0) do
    :ok
    s1 = trace_message(s0, {:nodedown, node}, [])
    s = handle_nodedown(node, s1)
    {:noreply, s}
  end

  def handle_info({:extra_nodedown, node}, s0) do
    :ok
    s1 = trace_message(s0, {:extra_nodedown, node}, [])
    s = handle_nodedown(node, s1)
    {:noreply, s}
  end

  def handle_info({:nodeup, node}, s) when node === node() do
    :ok
    {:noreply, change_our_node_name(node, s)}
  end

  def handle_info({:nodeup, _Node}, s) when not r_state(s, :connect_all) do
    {:noreply, s}
  end

  def handle_info({:nodeup, node}, s0) when r_state(s0, :connect_all) do
    isKnown =
      :erlang.or(
        :lists.member(node, r_state(s0, :known)),
        :lists.keymember(node, 1, r_state(s0, :resolvers))
      )

    :ok
    s1 = trace_message(s0, {:nodeup, node}, [])

    case isKnown do
      true ->
        {:noreply, s1}

      false ->
        resend_pre_connect(node)
        myTag = :erlang.unique_integer([:monotonic])
        :erlang.put({:sync_tag_my, node}, myTag)
        :ok
        send(r_state(s1, :the_locker), {:nodeup, node, myTag})
        notAPid = :no_longer_a_pid
        locker = {:locker, notAPid, r_state(s1, :known), r_state(s1, :the_locker)}
        initC = {:init_connect, {5, myTag}, node(), locker}
        rs = r_state(s1, :resolvers)
        :ok
        :gen_server.cast({:global_name_server, node}, initC)
        resolver = start_resolver(node, myTag)
        s = trace_message(s1, {:new_resolver, node}, [myTag, resolver])
        {:noreply, r_state(s, resolvers: [{node, myTag, resolver} | rs])}
    end
  end

  def handle_info({:whereis, name, from}, s) do
    _ = do_whereis(name, from)
    {:noreply, s}
  end

  def handle_info(:known, s) do
    :io.format('>>>> ~p\n', [r_state(s, :known)])
    {:noreply, s}
  end

  def handle_info(:high_level_trace, s) do
    case s do
      r_state(trace: [{node, _Time, _M, nodes, _X} | _]) ->
        send_high_level_trace()
        cNode = node()
        cNodes = :erlang.nodes()

        case {cNode, cNodes} do
          {^node, ^nodes} ->
            {:noreply, s}

          _ ->
            {new, _, old} =
              :sofs.symmetric_partition(
                :sofs.set([
                  cNode
                  | cNodes
                ]),
                :sofs.set([node | nodes])
              )

            m = {:nodes_changed, {:sofs.to_external(new), :sofs.to_external(old)}}
            {:noreply, trace_message(s, m, [])}
        end

      _ ->
        {:noreply, s}
    end
  end

  def handle_info({:trace_message, m}, s) do
    {:noreply, trace_message(s, m, [])}
  end

  def handle_info({:trace_message, m, x}, s) do
    {:noreply, trace_message(s, m, x)}
  end

  def handle_info(
        {:DOWN, monitorRef, :process, _Pid, _Info},
        s0
      ) do
    s1 = delete_lock(monitorRef, s0)
    s = del_name(monitorRef, s1)
    {:noreply, s}
  end

  def handle_info(message, s) do
    :error_logger.warning_msg(
      'The global_name_server received an unexpected message:\nhandle_info(~tp, _)\n',
      [message]
    )

    {:noreply, s}
  end

  defp wait_high_level_trace() do
    receive do
      :high_level_trace ->
        :ok
    after
      500 + 1 ->
        :ok
    end
  end

  defp send_high_level_trace() do
    :erlang.send_after(500, self(), :high_level_trace)
  end

  defp trans_all_known(fun) do
    id = {:global, self()}
    nodes = set_lock_known(id, 0)

    try do
      fun.(nodes)
    after
      delete_global_lock(id, nodes)
    end
  end

  defp set_lock_known(id, times) do
    known = get_known()
    nodes = [node() | known]
    boss = the_boss(nodes)

    case set_lock_on_nodes(id, [boss]) do
      true ->
        case lock_on_known_nodes(id, known, nodes) do
          true ->
            nodes

          false ->
            del_lock(id, [boss])
            random_sleep(times)
            set_lock_known(id, times + 1)
        end

      false ->
        random_sleep(times)
        set_lock_known(id, times + 1)
    end
  end

  defp lock_on_known_nodes(id, known, nodes) do
    case set_lock_on_nodes(id, nodes) do
      true ->
        get_known() -- known === []

      false ->
        false
    end
  end

  defp set_lock_on_nodes(_Id, []) do
    true
  end

  defp set_lock_on_nodes(id, nodes) do
    case local_lock_check(id, nodes) do
      true ->
        msg = {:set_lock, id}
        {replies, _} = :gen_server.multi_call(nodes, :global_name_server, msg)
        :ok
        check_replies(replies, id, replies)

      false = reply ->
        reply
    end
  end

  defp local_lock_check(_Id, [_] = _Nodes) do
    true
  end

  defp local_lock_check(id, nodes) do
    not :lists.member(
      node(),
      nodes
    ) or can_set_lock(id) !== false
  end

  defp check_replies([{_Node, true} | t], id, replies) do
    check_replies(t, id, replies)
  end

  defp check_replies([{_Node, false = reply} | _T], _Id, [_]) do
    reply
  end

  defp check_replies([{_Node, false = reply} | _T], id, replies) do
    trueReplyNodes =
      for {n, true} <- replies do
        n
      end

    :ok
    :gen_server.multi_call(trueReplyNodes, :global_name_server, {:del_lock, id})
    reply
  end

  defp check_replies([], _Id, _Replies) do
    true
  end

  defp init_connect(vsn, node, initMsg, hisTag, resolvers, s) do
    :erlang.put({:prot_vsn, node}, vsn)
    :erlang.put({:sync_tag_his, node}, hisTag)

    case :lists.keyfind(node, 1, resolvers) do
      {^node, myTag, _Resolver} ->
        ^myTag = :erlang.get({:sync_tag_my, node})
        {:locker, _NoLongerAPid, _HisKnown0, hisTheLocker} = initMsg
        :ok
        hisKnown = []

        send(
          r_state(s, :the_locker),
          {:his_the_locker, hisTheLocker, {vsn, hisKnown}, r_state(s, :known)}
        )

      false ->
        :ok

        :erlang.put(
          {:pre_connect, node},
          {vsn, initMsg, hisTag}
        )
    end
  end

  defp lock_is_set(node, resolvers, lockId) do
    :gen_server.cast(
      {:global_name_server, node},
      {:exchange, node(), get_names(), _ExtNames = [], :erlang.get({:sync_tag_his, node})}
    )

    :erlang.put({:lock_id, node}, lockId)

    case :erlang.get({:wait_lock, node}) do
      {:exchange, nameList} ->
        :erlang.put({:wait_lock, node}, :lock_is_set)
        exchange(node, nameList, resolvers)

      :undefined ->
        :erlang.put({:wait_lock, node}, :lock_is_set)
    end
  end

  defp exchange(node, nameList, resolvers) do
    :ok

    case :erlang.erase({:wait_lock, node}) do
      :lock_is_set ->
        {^node, _Tag, resolver} = :lists.keyfind(node, 1, resolvers)
        send(resolver, {:resolve, nameList, node})

      :undefined ->
        :erlang.put({:wait_lock, node}, {:exchange, nameList})
    end
  end

  defp resolved(node, hisResolved, hisKnown, names_ext, s0) do
    ops = :erlang.erase({:save_ops, node}) ++ hisResolved
    known = r_state(s0, :known)
    synced = r_state(s0, :synced)
    newNodes = [node | hisKnown]
    sync_others(hisKnown)
    extraInfo = [{:vsn, :erlang.get({:prot_vsn, node})}, {:lock, :erlang.get({:lock_id, node})}]
    s = do_ops(ops, node(), names_ext, extraInfo, s0)

    :lists.foreach(
      fn pid ->
        send(pid, {:synced, [node]})
      end,
      r_state(s, :syncers)
    )

    s3 =
      :lists.foldl(
        fn node1, s1 ->
          f = fn tag ->
            cancel_locker(node1, s1, tag)
          end

          cancel_resolved_locker(node1, f)
        end,
        s,
        hisKnown
      )

    newNodesF = fn ->
      :gen_server.abcast(
        known,
        :global_name_server,
        {:new_nodes, node(), ops, names_ext, newNodes, extraInfo}
      )
    end

    f = fn tag ->
      cancel_locker(node, s3, tag, newNodesF)
    end

    s4 = cancel_resolved_locker(node, f)
    addedNodes = newNodes -- known
    newKnown = known ++ addedNodes
    send(r_state(s4, :the_locker), {:add_to_known, addedNodes})

    newS =
      trace_message(s4, {:added, addedNodes}, [
        {:new_nodes, newNodes},
        {:abcast, known},
        {:ops, ops}
      ])

    r_state(newS, known: newKnown, synced: [node | synced])
  end

  defp cancel_resolved_locker(node, cancelFun) do
    tag = :erlang.get({:sync_tag_my, node})
    :ok
    s = cancelFun.(tag)
    reset_node_state(node)
    s
  end

  defp new_nodes(ops, connNode, names_ext, nodes, extraInfo, s0) do
    known = r_state(s0, :known)
    addedNodes = :lists.delete(node(), nodes -- known)
    sync_others(addedNodes)
    s = do_ops(ops, connNode, names_ext, extraInfo, s0)
    :ok
    send(r_state(s, :the_locker), {:add_to_known, addedNodes})
    s1 = trace_message(s, {:added, addedNodes}, [{:ops, ops}])
    r_state(s1, known: known ++ addedNodes)
  end

  defp do_whereis(name, from) do
    case is_global_lock_set() do
      false ->
        :gen_server.reply(from, where(name))

      true ->
        send_again({:whereis, name, from})
    end
  end

  def terminate(_Reason, _S) do
    true = :ets.delete(:global_names)
    true = :ets.delete(:global_names_ext)
    true = :ets.delete(:global_locks)
    true = :ets.delete(:global_pid_names)
    true = :ets.delete(:global_pid_ids)
    :ok
  end

  def code_change(_OldVsn, s, _Extra) do
    {:ok, s}
  end

  defp start_resolver(node, myTag) do
    spawn(fn ->
      resolver(node, myTag)
    end)
  end

  defp resolver(node, tag) do
    receive do
      {:resolve, nameList, ^node} ->
        :ok
        {ops, resolved} = exchange_names(nameList, node, [], [])
        exchange = {:exchange_ops, node, tag, ops, resolved}
        :gen_server.cast(:global_name_server, exchange)
        exit(:normal)

      _ ->
        resolver(node, tag)
    end
  end

  defp resend_pre_connect(node) do
    case :erlang.erase({:pre_connect, node}) do
      {vsn, initMsg, hisTag} ->
        :gen_server.cast(
          self(),
          {:init_connect, {vsn, hisTag}, node, initMsg}
        )

      _ ->
        :ok
    end
  end

  defp ins_name(name, pid, method, fromPidOrNode, extraInfo, s0) do
    :ok
    s1 = delete_global_name_keep_pid(name, s0)
    s = trace_message(s1, {:ins_name, node(pid)}, [name, pid])
    insert_global_name(name, pid, method, fromPidOrNode, extraInfo, s)
  end

  defp ins_name_ext(name, pid, method, regNode, fromPidOrNode, extraInfo, s0) do
    :ok
    s1 = delete_global_name_keep_pid(name, s0)
    dolink_ext(pid, regNode)
    s = trace_message(s1, {:ins_name_ext, node(pid)}, [name, pid])

    true =
      :ets.insert(
        :global_names_ext,
        {name, pid, regNode}
      )

    insert_global_name(name, pid, method, fromPidOrNode, extraInfo, s)
  end

  defp where(name) do
    case :ets.lookup(:global_names, name) do
      [{_Name, pid, _Method, _Ref}] ->
        cond do
          node(pid) == node() ->
            case :erlang.is_process_alive(pid) do
              true ->
                pid

              false ->
                :undefined
            end

          true ->
            pid
        end

      [] ->
        :undefined
    end
  end

  defp handle_set_lock(id, pid, s) do
    :ok

    case can_set_lock(id) do
      {true, pidRefs} ->
        case pid_is_locking(pid, pidRefs) do
          true ->
            {true, s}

          false ->
            {true, insert_lock(id, pid, pidRefs, s)}
        end

      false = reply ->
        {reply, s}
    end
  end

  defp can_set_lock({resourceId, lockRequesterId}) do
    case :ets.lookup(:global_locks, resourceId) do
      [{^resourceId, ^lockRequesterId, pidRefs}] ->
        {true, pidRefs}

      [{^resourceId, _LockRequesterId2, _PidRefs}] ->
        false

      [] ->
        {true, []}
    end
  end

  defp insert_lock({resourceId, lockRequesterId} = id, pid, pidRefs, s) do
    ref = :erlang.monitor(:process, pid)
    true = :ets.insert(:global_pid_ids, {pid, resourceId})
    true = :ets.insert(:global_pid_ids, {ref, resourceId})
    lock = {resourceId, lockRequesterId, [{pid, ref} | pidRefs]}
    true = :ets.insert(:global_locks, lock)
    trace_message(s, {:ins_lock, node(pid)}, [id, pid])
  end

  defp is_global_lock_set() do
    is_lock_set(:global)
  end

  defp is_lock_set(resourceId) do
    :ets.member(:global_locks, resourceId)
  end

  defp handle_del_lock({resourceId, lockReqId}, pid, s0) do
    :ok

    case :ets.lookup(:global_locks, resourceId) do
      [{^resourceId, ^lockReqId, pidRefs}] ->
        remove_lock(resourceId, lockReqId, pid, pidRefs, false, s0)

      _ ->
        s0
    end
  end

  defp remove_lock(resourceId, lockRequesterId, pid, [{pid, ref}], down, s0) do
    :ok
    true = :erlang.demonitor(ref, [:flush])
    true = :ets.delete(:global_locks, resourceId)

    true =
      :ets.delete_object(
        :global_pid_ids,
        {pid, resourceId}
      )

    true =
      :ets.delete_object(
        :global_pid_ids,
        {ref, resourceId}
      )

    s =
      case resourceId do
        :global ->
          r_state(s0, global_lock_down: down)

        _ ->
          s0
      end

    trace_message(s, {:rem_lock, node(pid)}, [{resourceId, lockRequesterId}, pid])
  end

  defp remove_lock(resourceId, lockRequesterId, pid, pidRefs0, _Down, s) do
    :ok

    pidRefs =
      case :lists.keyfind(pid, 1, pidRefs0) do
        {^pid, ref} ->
          true = :erlang.demonitor(ref, [:flush])

          true =
            :ets.delete_object(
              :global_pid_ids,
              {ref, resourceId}
            )

          :lists.keydelete(pid, 1, pidRefs0)

        false ->
          pidRefs0
      end

    lock = {resourceId, lockRequesterId, pidRefs}
    true = :ets.insert(:global_locks, lock)

    true =
      :ets.delete_object(
        :global_pid_ids,
        {pid, resourceId}
      )

    trace_message(s, {:rem_lock, node(pid)}, [{resourceId, lockRequesterId}, pid])
  end

  defp do_ops(ops, connNode, names_ext, extraInfo, s0) do
    :ok

    xInserts =
      for {name2, pid2, regNode} <- names_ext,
          {:insert, {name, pid, method}} <- ops,
          name === name2,
          pid === pid2 do
        {name, pid, regNode, method}
      end

    s1 =
      :lists.foldl(
        fn {name, pid, regNode, method}, s1 ->
          ins_name_ext(name, pid, method, regNode, connNode, extraInfo, s1)
        end,
        s0,
        xInserts
      )

    xNames =
      for {name, _Pid, _RegNode, _Method} <- xInserts do
        name
      end

    inserts =
      for {:insert, {name, pid, method}} <- ops,
          not :lists.member(name, xNames) do
        {name, pid, node(pid), method}
      end

    s2 =
      :lists.foldl(
        fn {name, pid, _RegNode, method}, s2 ->
          ins_name(name, pid, method, connNode, extraInfo, s2)
        end,
        s1,
        inserts
      )

    delNames =
      for {:delete, name} <- ops do
        name
      end

    :lists.foldl(
      fn name, s ->
        delete_global_name2(name, s)
      end,
      s2,
      delNames
    )
  end

  defp sync_others(nodes) do
    n =
      case :application.get_env(
             :kernel,
             :global_connect_retries
           ) do
        {:ok, nRetries}
        when is_integer(nRetries) and
               nRetries >= 0 ->
          nRetries

        _ ->
          0
      end

    :lists.foreach(
      fn node ->
        spawn(fn ->
          sync_other(node, n)
        end)
      end,
      nodes
    )
  end

  defp sync_other(node, n) do
    :erlang.monitor_node(node, true, [:allow_passive_connect])

    receive do
      {:nodedown, ^node} when n > 0 ->
        sync_other(node, n - 1)

      {:nodedown, ^node} ->
        :ok
        :error_logger.warning_msg('global: ~w failed to connect to ~w\n', [node(), node])
        send(:global_name_server, {:extra_nodedown, node})
    after
      0 ->
        :gen_server.cast(
          {:global_name_server, node},
          {:in_sync, node(), true}
        )
    end
  end

  defp insert_global_name(name, pid, method, fromPidOrNode, extraInfo, s) do
    ref = :erlang.monitor(:process, pid)

    true =
      :ets.insert(
        :global_names,
        {name, pid, method, ref}
      )

    true = :ets.insert(:global_pid_names, {pid, name})
    true = :ets.insert(:global_pid_names, {ref, name})

    case lock_still_set(fromPidOrNode, extraInfo, s) do
      true ->
        s

      false ->
        delete_global_name2(name, s)
    end
  end

  defp lock_still_set(pidOrNode, extraInfo, s) do
    case :ets.lookup(:global_locks, :global) do
      [{:global, _LockReqId, pidRefs}] when is_pid(pidOrNode) ->
        :lists.keymember(pidOrNode, 1, pidRefs)

      [{:global, lockReqId, _PidRefs}] when is_atom(pidOrNode) ->
        {:global, lockId} = extra_info(:lock, extraInfo)
        lockReqId === lockId

      [] ->
        not r_state(s, :global_lock_down)
    end
  end

  defp extra_info(tag, extraInfo) do
    case (try do
            :lists.keyfind(tag, 1, extraInfo)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {^tag, info} ->
        info

      _ ->
        :undefined
    end
  end

  defp del_name(ref, s) do
    nameL =
      for {_, name} <-
            :ets.lookup(
              :global_pid_names,
              ref
            ),
          {_, _Pid, _Method, ref1} <-
            :ets.lookup(
              :global_names,
              name
            ),
          ref1 === ref do
        name
      end

    case nameL do
      [name] ->
        delete_global_name2(name, s)

      [] ->
        s
    end
  end

  defp delete_global_name_keep_pid(name, s) do
    case :ets.lookup(:global_names, name) do
      [{^name, pid, _Method, ref}] ->
        delete_global_name2(name, pid, ref, s)

      [] ->
        s
    end
  end

  defp delete_global_name2(name, s) do
    case :ets.lookup(:global_names, name) do
      [{^name, pid, _Method, ref}] ->
        true = :ets.delete(:global_names, name)
        delete_global_name2(name, pid, ref, s)

      [] ->
        s
    end
  end

  defp delete_global_name2(name, pid, ref, s) do
    true = :erlang.demonitor(ref, [:flush])
    delete_global_name(name, pid)
    :ok

    true =
      :ets.delete_object(
        :global_pid_names,
        {pid, name}
      )

    true =
      :ets.delete_object(
        :global_pid_names,
        {ref, name}
      )

    case :ets.lookup(:global_names_ext, name) do
      [{^name, ^pid, regNode}] ->
        true = :ets.delete(:global_names_ext, name)
        :ok
        dounlink_ext(pid, regNode)

      [] ->
        :ok
        :ok
    end

    trace_message(s, {:del_name, node(pid)}, [name, pid])
  end

  defp delete_global_name(_Name, _Pid) do
    :ok
  end

  Record.defrecord(:r_multi, :multi,
    local: [],
    remote: [],
    known: [],
    the_boss: :undefined,
    just_synced: false,
    do_trace: :undefined
  )

  Record.defrecord(:r_him, :him,
    node: :undefined,
    locker: :undefined,
    vsn: :undefined,
    my_tag: :undefined
  )

  defp start_the_locker(doTrace) do
    spawn_link(init_the_locker_fun(doTrace))
  end

  defp init_the_locker_fun(doTrace) do
    fn ->
      :erlang.process_flag(:trap_exit, true)
      s0 = r_multi(do_trace: doTrace)
      s1 = update_locker_known({:add, get_known()}, s0)
      loop_the_locker(s1)
      :erlang.error(:locker_exited)
    end
  end

  defp loop_the_locker(s) do
    :ok

    receive do
      message when :erlang.element(1, message) !== :nodeup ->
        the_locker_message(message, s)
    after
      0 ->
        timeout =
          case {r_multi(s, :local), r_multi(s, :remote)} do
            {[], []} ->
              :infinity

            _ ->
              cond do
                r_multi(s, :just_synced) ->
                  0

                r_multi(s, :known) === [] ->
                  200

                true ->
                  :erlang.min(1000 + 100 * length(r_multi(s, :known)), 3000)
              end
          end

        s1 = r_multi(s, just_synced: false)

        receive do
          message when :erlang.element(1, message) !== :nodeup ->
            the_locker_message(message, s1)
        after
          timeout ->
            case is_global_lock_set() do
              true ->
                loop_the_locker(s1)

              false ->
                select_node(s1)
            end
        end
    end
  end

  defp the_locker_message(
         {:his_the_locker, hisTheLocker, hisKnown0, _MyKnown},
         s
       ) do
    :ok
    {hisVsn, _HisKnown} = hisKnown0
    true = hisVsn > 4

    receive do
      {:nodeup, node, myTag} when node(hisTheLocker) === node ->
        :ok
        him = r_him(node: node(hisTheLocker), my_tag: myTag, locker: hisTheLocker, vsn: hisVsn)
        loop_the_locker(add_node(him, s))

      {:cancel, node, _Tag, :no_fun}
      when node(hisTheLocker) === node ->
        loop_the_locker(s)
    after
      60000 ->
        :ok
        :error_logger.error_msg('global: nodeup never came ~w ~w\n', [node(), node(hisTheLocker)])
        loop_the_locker(r_multi(s, just_synced: false))
    end
  end

  defp the_locker_message({:cancel, _Node, :undefined, :no_fun}, s) do
    :ok
    loop_the_locker(s)
  end

  defp the_locker_message({:cancel, node, tag, :no_fun}, s) do
    :ok

    receive do
      {:nodeup, ^node, ^tag} ->
        :ok
        :ok
    after
      0 ->
        :ok
    end

    loop_the_locker(remove_node(node, s))
  end

  defp the_locker_message({:lock_set, _Pid, false, _}, s) do
    :ok
    loop_the_locker(s)
  end

  defp the_locker_message({:lock_set, pid, true, _HisKnown}, s) do
    node = node(pid)
    :ok

    case find_node_tag(node, s) do
      {true, myTag, hisVsn} ->
        lockId = locker_lock_id(pid, hisVsn)
        {isLockSet, s1} = lock_nodes_safely(lockId, [], s)
        send(pid, {:lock_set, self(), isLockSet, r_multi(s1, :known)})
        known2 = [node() | r_multi(s1, :known)]
        :ok

        case isLockSet do
          true ->
            :gen_server.cast(
              :global_name_server,
              {:lock_is_set, node, myTag, lockId}
            )

            :ok

            receive do
              {:cancel, ^node, _Tag, fun} ->
                :ok
                call_fun(fun)
                delete_global_lock(lockId, known2)
            end

            s2 = r_multi(s1, just_synced: true)
            loop_the_locker(remove_node(node, s2))

          false ->
            loop_the_locker(r_multi(s1, just_synced: false))
        end

      false ->
        :ok
        send(pid, {:lock_set, self(), false, r_multi(s, :known)})
        loop_the_locker(s)
    end
  end

  defp the_locker_message({:add_to_known, nodes}, s) do
    s1 = update_locker_known({:add, nodes}, s)
    loop_the_locker(s1)
  end

  defp the_locker_message({:remove_from_known, node}, s) do
    s1 = update_locker_known({:remove, node}, s)
    loop_the_locker(s1)
  end

  defp the_locker_message({:do_trace, doTrace}, s) do
    loop_the_locker(r_multi(s, do_trace: doTrace))
  end

  defp the_locker_message(other, s) do
    unexpected_message(other, :locker)
    :ok
    loop_the_locker(s)
  end

  defp select_node(s) do
    useRemote = r_multi(s, :local) === []

    others1 =
      cond do
        useRemote ->
          r_multi(s, :remote)

        true ->
          r_multi(s, :local)
      end

    others2 = exclude_known(others1, r_multi(s, :known))

    s1 =
      cond do
        useRemote ->
          r_multi(s, remote: others2)

        true ->
          r_multi(s, local: others2)
      end

    cond do
      others2 === [] ->
        loop_the_locker(s1)

      true ->
        him = random_element(others2)
        r_him(locker: hisTheLocker, vsn: hisVsn, node: node, my_tag: myTag) = him
        hisNode = [node]
        us = [node() | hisNode]
        lockId = locker_lock_id(hisTheLocker, hisVsn)
        :ok
        {isLockSet, s2} = lock_nodes_safely(lockId, hisNode, s1)

        case isLockSet do
          true ->
            known1 = us ++ r_multi(s2, :known)
            :ok
            send(hisTheLocker, {:lock_set, self(), true, r_multi(s2, :known)})
            s3 = lock_is_set(s2, him, myTag, known1, lockId)
            loop_the_locker(s3)

          false ->
            loop_the_locker(s2)
        end
    end
  end

  defp locker_lock_id(pid, vsn) when vsn > 4 do
    {:global, :lists.sort([self(), pid])}
  end

  defp lock_nodes_safely(lockId, extra, s0) do
    first = delete_nonode([r_multi(s0, :the_boss)])

    case [node()] === first or can_set_lock(lockId) !== false do
      true ->
        case set_lock(lockId, first, 0) do
          true ->
            s = update_locker_known(s0)
            second = delete_nonode([node() | extra] -- first)

            case set_lock(lockId, second, 0) do
              true ->
                known = r_multi(s, :known)

                case set_lock(lockId, known -- first, 0) do
                  true ->
                    _ = locker_trace(s, :ok, {first, known})
                    {true, s}

                  false ->
                    soFar = first ++ second
                    del_lock(lockId, soFar)
                    _ = locker_trace(s, :not_ok, {known, soFar})
                    {false, s}
                end

              false ->
                del_lock(lockId, first)
                _ = locker_trace(s, :not_ok, {second, first})
                {false, s}
            end

          false ->
            _ = locker_trace(s0, :not_ok, {first, []})
            {false, s0}
        end

      false ->
        {false, s0}
    end
  end

  defp delete_nonode(l) do
    :lists.delete(:nonode@nohost, l)
  end

  defp locker_trace(r_multi(do_trace: false), _, _Nodes) do
    :ok
  end

  defp locker_trace(r_multi(do_trace: true), :ok, ns) do
    send(:global_name_server, {:trace_message, {:locker_succeeded, node()}, ns})
  end

  defp locker_trace(r_multi(do_trace: true), :not_ok, ns) do
    send(:global_name_server, {:trace_message, {:locker_failed, node()}, ns})
  end

  defp locker_trace(r_multi(do_trace: true), :rejected, ns) do
    send(:global_name_server, {:trace_message, {:lock_rejected, node()}, ns})
  end

  defp update_locker_known(s) do
    receive do
      {:add_to_known, nodes} ->
        s1 = update_locker_known({:add, nodes}, s)
        update_locker_known(s1)

      {:remove_from_known, node} ->
        s1 = update_locker_known({:remove, node}, s)
        update_locker_known(s1)
    after
      0 ->
        s
    end
  end

  defp update_locker_known(upd, s) do
    known =
      case upd do
        {:add, nodes} ->
          nodes ++ r_multi(s, :known)

        {:remove, node} ->
          :lists.delete(node, r_multi(s, :known))
      end

    theBoss = the_boss([node() | known])
    r_multi(s, known: known, the_boss: theBoss)
  end

  defp random_element(l) do
    e = rem(abs(:erlang.monotonic_time() ^^^ :erlang.unique_integer()), length(l))
    :lists.nth(e + 1, l)
  end

  defp exclude_known(others, known) do
    for n <- others,
        not :lists.member(r_him(n, :node), known) do
      n
    end
  end

  defp lock_is_set(s, him, myTag, known1, lockId) do
    node = r_him(him, :node)

    receive do
      {:lock_set, p, true, _} when node(p) === node ->
        :gen_server.cast(
          :global_name_server,
          {:lock_is_set, node, myTag, lockId}
        )

        :ok

        receive do
          {:cancel, ^node, _, fun} ->
            :ok
            call_fun(fun)
            delete_global_lock(lockId, known1)
        end

        r_multi(s,
          just_synced: true,
          local: :lists.delete(him, r_multi(s, :local)),
          remote: :lists.delete(him, r_multi(s, :remote))
        )

      {:lock_set, p, false, _} when node(p) === node ->
        :ok
        _ = locker_trace(s, :rejected, known1)
        delete_global_lock(lockId, known1)
        s

      {:cancel, ^node, _, fun} ->
        :ok
        call_fun(fun)
        _ = locker_trace(s, :rejected, known1)
        delete_global_lock(lockId, known1)
        remove_node(node, s)

      {:EXIT, _, _} ->
        :ok
        _ = locker_trace(s, :rejected, known1)
        delete_global_lock(lockId, known1)
        s
    end
  end

  defp call_fun(:no_fun) do
    :ok
  end

  defp call_fun(fun) do
    fun.()
  end

  defp delete_global_lock(lockId, nodes) do
    theBoss = the_boss(nodes)
    del_lock(lockId, :lists.delete(theBoss, nodes))
    del_lock(lockId, [theBoss])
  end

  defp the_boss(nodes) do
    :lists.max(nodes)
  end

  defp find_node_tag(node, s) do
    case find_node_tag2(node, r_multi(s, :local)) do
      false ->
        find_node_tag2(node, r_multi(s, :remote))

      reply ->
        reply
    end
  end

  defp find_node_tag2(_Node, []) do
    false
  end

  defp find_node_tag2(
         node,
         [r_him(node: node, my_tag: myTag, vsn: hisVsn) | _]
       ) do
    {true, myTag, hisVsn}
  end

  defp find_node_tag2(node, [_E | rest]) do
    find_node_tag2(node, rest)
  end

  defp remove_node(node, s) do
    r_multi(s,
      local: remove_node2(node, r_multi(s, :local)),
      remote: remove_node2(node, r_multi(s, :remote))
    )
  end

  defp remove_node2(_Node, []) do
    []
  end

  defp remove_node2(node, [r_him(node: node) | rest]) do
    rest
  end

  defp remove_node2(node, [e | rest]) do
    [e | remove_node2(node, rest)]
  end

  defp add_node(him, s) do
    case is_node_local(r_him(him, :node)) do
      true ->
        r_multi(s, local: [him | r_multi(s, :local)])

      false ->
        r_multi(s, remote: [him | r_multi(s, :remote)])
    end
  end

  defp is_node_local(node) do
    {:ok, host} = :inet.gethostname()

    case (try do
            split_node(:erlang.atom_to_list(node), ?@, [])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      [_, ^host] ->
        true

      _ ->
        false
    end
  end

  defp split_node([chr | t], chr, ack) do
    [:lists.reverse(ack) | split_node(t, chr, [])]
  end

  defp split_node([h | t], chr, ack) do
    split_node(t, chr, [h | ack])
  end

  defp split_node([], _, ack) do
    [:lists.reverse(ack)]
  end

  defp cancel_locker(node, s, tag) do
    cancel_locker(node, s, tag, :no_fun)
  end

  defp cancel_locker(node, s, tag, toBeRunOnLockerF) do
    send(r_state(s, :the_locker), {:cancel, node, tag, toBeRunOnLockerF})
    resolvers = r_state(s, :resolvers)
    :ok

    case :lists.keyfind(node, 1, resolvers) do
      {_, ^tag, resolver} ->
        :ok
        :erlang.exit(resolver, :kill)
        s1 = trace_message(s, {:kill_resolver, node}, [tag, resolver])
        r_state(s1, resolvers: :lists.keydelete(node, 1, resolvers))

      _ ->
        s
    end
  end

  defp reset_node_state(node) do
    :ok
    :erlang.erase({:wait_lock, node})
    :erlang.erase({:save_ops, node})
    :erlang.erase({:pre_connect, node})
    :erlang.erase({:prot_vsn, node})
    :erlang.erase({:sync_tag_my, node})
    :erlang.erase({:sync_tag_his, node})
    :erlang.erase({:lock_id, node})
  end

  defp exchange_names([{name, pid, method} | tail], node, ops, res) do
    case :ets.lookup(:global_names, name) do
      [{^name, ^pid, _Method, _Ref2}] ->
        exchange_names(tail, node, ops, res)

      [{^name, pid2, method2, _Ref2}] when node() < node ->
        node2 = node(pid2)

        case :rpc.call(node2, :global, :resolve_it, [method2, name, pid, pid2]) do
          ^pid ->
            op = {:insert, {name, pid, method}}
            exchange_names(tail, node, [op | ops], res)

          ^pid2 ->
            op = {:insert, {name, pid2, method2}}
            exchange_names(tail, node, ops, [op | res])

          :none ->
            op = {:delete, name}
            exchange_names(tail, node, [op | ops], [op | res])

          {:badrpc, badrpc} ->
            :error_logger.info_msg(
              'global: badrpc ~w received when conflicting name ~tw was found\n',
              [badrpc, name]
            )

            op = {:insert, {name, pid, method}}
            exchange_names(tail, node, [op | ops], res)

          else__ ->
            :error_logger.info_msg(
              'global: Resolve method ~w for conflicting name ~tw returned ~tw\n',
              [method, name, else__]
            )

            op = {:delete, name}
            exchange_names(tail, node, [op | ops], [op | res])
        end

      [{^name, _Pid2, _Method, _Ref}] ->
        exchange_names(tail, node, ops, res)

      _ ->
        exchange_names(tail, node, [{:insert, {name, pid, method}} | ops], res)
    end
  end

  defp exchange_names([], _, ops, res) do
    :ok
    {ops, res}
  end

  def resolve_it(method, name, pid1, pid2) do
    try do
      method.(name, pid1, pid2)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp minmax(p1, p2) do
    cond do
      node(p1) < node(p2) ->
        {p1, p2}

      true ->
        {p2, p1}
    end
  end

  def random_exit_name(name, pid, pid2) do
    {min, max} = minmax(pid, pid2)
    :error_logger.info_msg('global: Name conflict terminating ~tw\n', [{name, max}])
    :erlang.exit(max, :kill)
    min
  end

  def random_notify_name(name, pid, pid2) do
    {min, max} = minmax(pid, pid2)
    send(max, {:global_name_conflict, name})
    min
  end

  def notify_all_name(name, pid, pid2) do
    send(pid, {:global_name_conflict, name, pid2})
    send(pid2, {:global_name_conflict, name, pid})
    :none
  end

  defp dolink_ext(pid, regNode) when regNode === node() do
    :erlang.link(pid)
  end

  defp dolink_ext(_, _) do
    :ok
  end

  defp dounlink_ext(pid, regNode) when regNode === node() do
    unlink_pid(pid)
  end

  defp dounlink_ext(_Pid, _RegNode) do
    :ok
  end

  defp unlink_pid(pid) do
    case :ets.member(:global_pid_names, pid) do
      false ->
        case :ets.member(:global_pid_ids, pid) do
          false ->
            :erlang.unlink(pid)

          true ->
            :ok
        end

      true ->
        :ok
    end
  end

  defp pid_is_locking(pid, pidRefs) do
    :lists.keyfind(pid, 1, pidRefs) !== false
  end

  defp delete_lock(ref, s0) do
    locks = pid_locks(ref)

    f = fn {resourceId, lockRequesterId, pidRefs}, s ->
      {pid, ^ref} = :lists.keyfind(ref, 2, pidRefs)
      remove_lock(resourceId, lockRequesterId, pid, pidRefs, true, s)
    end

    :lists.foldl(f, s0, locks)
  end

  defp pid_locks(ref) do
    l =
      :lists.flatmap(
        fn {_, resourceId} ->
          :ets.lookup(:global_locks, resourceId)
        end,
        :ets.lookup(:global_pid_ids, ref)
      )

    for lock = {_Id, _Req, pidRefs} <- l,
        ref_is_locking(ref, pidRefs) do
      lock
    end
  end

  defp ref_is_locking(ref, pidRefs) do
    :lists.keyfind(ref, 2, pidRefs) !== false
  end

  defp handle_nodedown(node, s) do
    r_state(known: known, synced: syncs) = s
    newS = cancel_locker(node, s, :erlang.get({:sync_tag_my, node}))
    send(r_state(newS, :the_locker), {:remove_from_known, node})
    reset_node_state(node)

    r_state(newS,
      known: :lists.delete(node, known),
      synced: :lists.delete(node, syncs)
    )
  end

  defp get_names() do
    :ets.select(
      :global_names,
      :ets.fun2ms(fn {name, pid, method, _Ref} ->
        {name, pid, method}
      end)
    )
  end

  defp get_names_ext() do
    :ets.tab2list(:global_names_ext)
  end

  defp get_known() do
    :gen_server.call(:global_name_server, :get_known, :infinity)
  end

  defp random_sleep(times) do
    _ =
      case rem(times, 10) do
        0 ->
          _ = :rand.seed(:exsplus)

        _ ->
          :ok
      end

    tmax =
      cond do
        times > 5 ->
          8000

        true ->
          div((1 <<< times) * 1000, 8)
      end

    t = :rand.uniform(tmax)
    :ok

    receive do
    after
      t ->
        :ok
    end
  end

  defp dec(:infinity) do
    :infinity
  end

  defp dec(n) do
    n - 1
  end

  defp send_again(msg) do
    me = self()

    spawn(fn ->
      timer(me, msg)
    end)
  end

  defp timer(pid, msg) do
    random_sleep(5)
    send(pid, msg)
  end

  defp change_our_node_name(newNode, s) do
    s1 = trace_message(s, {:new_node_name, newNode}, [])
    r_state(s1, node_name: newNode)
  end

  defp trace_message(r_state(trace: :no_trace) = s, _M, _X) do
    s
  end

  defp trace_message(s, m, x) do
    r_state(s, trace: [trace_message(m, x) | r_state(s, :trace)])
  end

  defp trace_message(m, x) do
    {node(), :erlang.timestamp(), m, :erlang.nodes(), x}
  end

  defp start_sync(nodes, from) do
    spawn_link(fn ->
      sync_init(nodes, from)
    end)
  end

  defp sync_init(nodes, from) do
    :lists.foreach(
      fn node ->
        :erlang.monitor_node(node, true)
      end,
      nodes
    )

    sync_loop(nodes, from)
  end

  defp sync_loop([], from) do
    :gen_server.reply(from, :ok)
  end

  defp sync_loop(nodes, from) do
    receive do
      {:nodedown, node} ->
        :erlang.monitor_node(node, false)
        sync_loop(:lists.delete(node, nodes), from)

      {:synced, sNodes} ->
        :lists.foreach(
          fn n ->
            :erlang.monitor_node(n, false)
          end,
          sNodes
        )

        sync_loop(nodes -- sNodes, from)
    end
  end

  defp check_sync_nodes() do
    case get_own_nodes() do
      {:ok, :all} ->
        :erlang.nodes()

      {:ok, nodesNG} ->
        intersection(:erlang.nodes(), nodesNG)

      {:error, _} = error ->
        error
    end
  end

  defp check_sync_nodes(syncNodes) do
    case get_own_nodes() do
      {:ok, :all} ->
        syncNodes

      {:ok, nodesNG} ->
        ownNodeGroup = intersection(:erlang.nodes(), nodesNG)
        illegalSyncNodes = syncNodes -- [node() | ownNodeGroup]

        case illegalSyncNodes do
          [] ->
            syncNodes

          _ ->
            {:error,
             {'Trying to sync nodes not defined in the own global group', illegalSyncNodes}}
        end

      {:error, _} = error ->
        error
    end
  end

  defp get_own_nodes() do
    case :global_group.get_own_nodes_with_errors() do
      {:error, error} ->
        {:error, {'global_groups definition error', error}}

      okTup ->
        okTup
    end
  end

  defp start_the_registrar() do
    spawn_link(fn ->
      loop_the_registrar()
    end)
  end

  defp loop_the_registrar() do
    receive do
      {:trans_all_known, fun, from} ->
        :ok
        :gen_server.reply(from, trans_all_known(fun))

      other ->
        unexpected_message(other, :register)
    end

    loop_the_registrar()
  end

  defp unexpected_message({:EXIT, _Pid, _Reason}, _What) do
    :ok
  end

  defp unexpected_message(message, what) do
    :error_logger.warning_msg(
      'The global_name_server ~w process received an unexpected message:\n~tp\n',
      [what, message]
    )
  end

  defp intersection(_, []) do
    []
  end

  defp intersection(l1, l2) do
    l1 -- l1 -- l2
  end

  defp allow_tuple_fun({m, f}) when is_atom(m) and is_atom(f) do
    Function.capture(m, f, 3)
  end

  defp allow_tuple_fun(fun) when is_function(fun, 3) do
    fun
  end
end
