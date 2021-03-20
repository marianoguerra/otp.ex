defmodule :m_dbg do
  use Bitwise

  def fun2ms(shellFun) when is_function(shellFun) do
    case :erl_eval.fun_data(shellFun) do
      {:fun_data, importList, clauses} ->
        case :ms_transform.transform_from_shell(:dbg, clauses, importList) do
          {:error, [{_, [{_, _, code} | _]} | _], _} ->
            modifier = modifier()

            :io.format(
              'Error: ~' ++ modifier ++ 's~n',
              [:ms_transform.format_error(code)]
            )

            {:error, :transform_error}

          else__ ->
            else__
        end

      false ->
        exit(
          {:badarg,
           {:dbg, :fun2ms,
            [
              :function,
              :called,
              :with,
              :real,
              :fun,
              :should,
              :be,
              :transformed,
              :with,
              :parse_transform,
              :or,
              :called,
              :with,
              :a,
              :fun,
              :generated,
              :in,
              :the,
              :shell
            ]}}
        )
    end
  end

  def n(node) when node === node() do
    {:error, :cant_add_local_node}
  end

  def n(node) do
    case (try do
            :net_adm.ping(node)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:bad_node, node}}

      :pang ->
        {:error, {:nodedown, node}}

      :pong ->
        req({:add_node, node})

      other ->
        {:error, other}
    end
  end

  def cn(node) do
    req({:remove_node, node})
  end

  def ln() do
    :lists.foreach(
      fn x ->
        :io.format('~p~n', [x])
      end,
      req(:get_nodes)
    )

    :ok
  end

  def tp(module, function, pattern) do
    do_tp({module, function, :_}, pattern, [])
  end

  def tp(module, function, arity, pattern) do
    do_tp({module, function, arity}, pattern, [])
  end

  def tp(module, pattern) when is_atom(module) do
    do_tp({module, :_, :_}, pattern, [])
  end

  def tp({_Module, _Function, _Arity} = x, pattern) do
    do_tp(x, pattern, [])
  end

  def tpl(module, function, pattern) do
    do_tp({module, function, :_}, pattern, [:local])
  end

  def tpl(module, function, arity, pattern) do
    do_tp({module, function, arity}, pattern, [:local])
  end

  def tpl(module, pattern) when is_atom(module) do
    do_tp({module, :_, :_}, pattern, [:local])
  end

  def tpl({_Module, _Function, _Arity} = x, pattern) do
    do_tp(x, pattern, [:local])
  end

  def tpe(event, pattern)
      when event === :send or
             event === :receive do
    do_tp(event, pattern, [])
  end

  defp do_tp(x, pattern, flags)
       when is_integer(pattern) or
              is_atom(pattern) do
    case :ets.lookup(get_pattern_table(), pattern) do
      [{_, nPattern}] ->
        do_tp(x, :erlang.binary_to_term(nPattern), flags)

      _ ->
        {:error, :unknown_pattern}
    end
  end

  defp do_tp(x, pattern, flags) when is_list(pattern) do
    nodes = req(:get_nodes)

    case x do
      {m, _, _} when is_atom(m) ->
        :lists.foreach(
          fn node ->
            :rpc.call(node, m, :module_info, [])
          end,
          nodes
        )

      _ ->
        :ok
    end

    case lint_tp(pattern) do
      {:ok, _} ->
        saveInfo =
          case save_pattern(pattern) do
            n when (is_integer(n) and n > 0) or is_atom(n) ->
              [{:saved, n}]

            _ ->
              []
          end

        {:ok, do_tp_on_nodes(nodes, x, pattern, flags) ++ saveInfo}

      other ->
        other
    end
  end

  defp do_tp_on_nodes(nodes, x, p, flags) do
    :lists.map(
      fn node ->
        case :rpc.call(node, :erlang, :trace_pattern, [x, p, flags]) do
          n when is_integer(n) ->
            {:matched, node, n}

          else__ ->
            {:matched, node, 0, else__}
        end
      end,
      nodes
    )
  end

  def ctp() do
    do_ctp({:_, :_, :_}, [])
  end

  def ctp(module, function) do
    do_ctp({module, function, :_}, [])
  end

  def ctp(module, function, arity) do
    do_ctp({module, function, arity}, [])
  end

  def ctp(module) when is_atom(module) do
    do_ctp({module, :_, :_}, [])
  end

  def ctp({_Module, _Function, _Arity} = x) do
    do_ctp(x, [])
  end

  def ctpl() do
    do_ctp({:_, :_, :_}, [:local])
  end

  def ctpl(module, function) do
    do_ctp({module, function, :_}, [:local])
  end

  def ctpl(module, function, arity) do
    do_ctp({module, function, arity}, [:local])
  end

  def ctpl(module) when is_atom(module) do
    do_ctp({module, :_, :_}, [:local])
  end

  def ctpl({_Module, _Function, _Arity} = x) do
    do_ctp(x, [:local])
  end

  def ctpg() do
    do_ctp({:_, :_, :_}, [:global])
  end

  def ctpg(module, function) do
    do_ctp({module, function, :_}, [:global])
  end

  def ctpg(module, function, arity) do
    do_ctp({module, function, arity}, [:global])
  end

  def ctpg(module) when is_atom(module) do
    do_ctp({module, :_, :_}, [:global])
  end

  def ctpg({_Module, _Function, _Arity} = x) do
    do_ctp(x, [:global])
  end

  defp do_ctp({module, function, arity}, []) do
    {:ok, _} = do_ctp({module, function, arity}, [:global])
    do_ctp({module, function, arity}, [:local])
  end

  defp do_ctp({_Module, _Function, _Arity} = mFA, flags) do
    nodes = req(:get_nodes)
    {:ok, do_tp_on_nodes(nodes, mFA, false, flags)}
  end

  def ctpe(event)
      when event === :send or
             event === :receive do
    nodes = req(:get_nodes)
    {:ok, do_tp_on_nodes(nodes, event, true, [])}
  end

  def ltp() do
    modifier = modifier()
    format = '~p: ~' ++ modifier ++ 'p~n'

    pt_doforall(
      fn {x, el}, _Ignore ->
        :io.format(format, [x, el])
      end,
      []
    )
  end

  def dtp() do
    pt_doforall(
      fn
        {key, _}, _ when is_integer(key) ->
          dtp(key)

        {_, _}, _ ->
          :ok
      end,
      []
    )
  end

  def dtp(n) when is_integer(n) do
    :ets.delete(get_pattern_table(), n)
    :ok
  end

  def dtp(_) do
    :ok
  end

  def wtp(fileName) do
    case :file.open(
           fileName,
           [:write, {:encoding, :utf8}]
         ) do
      {:error, reason} ->
        {:error, reason}

      {:ok, file} ->
        :io.format(file, '%% ~s\n', [:epp.encoding_to_string(:utf8)])

        pt_doforall(
          fn
            {_, val}, _ when is_list(val) ->
              :io.format(file, '~tp.~n', [val])

            {_, _}, _ ->
              :ok
          end,
          []
        )

        :ok = :file.close(file)
    end
  end

  def rtp(fileName) do
    t = get_pattern_table()

    case :file.consult(fileName) do
      {:error, reason1} ->
        {:error, {:read_error, reason1}}

      {:ok, data} ->
        case check_list(data) do
          :ok ->
            :lists.foreach(
              fn x ->
                save_pattern(x, t)
              end,
              data
            )

            :ok

          {:error, reason2} ->
            {:error, {:file_format_error, reason2}}
        end
    end
  end

  def tracer() do
    tracer(:process, {&dhandler/2, :user})
  end

  def tracer(:port, fun) when is_function(fun) do
    start(fun)
  end

  def tracer(:port, port) when is_port(port) do
    start(fn ->
      port
    end)
  end

  def tracer(:process, {handler, handlerData}) do
    start(fn ->
      start_tracer_process(handler, handlerData)
    end)
  end

  def tracer(:module, fun) when is_function(fun) do
    start(fun)
  end

  def tracer(:module, {module, state}) do
    start(fn ->
      {module, state}
    end)
  end

  defp remote_tracer(:port, fun) when is_function(fun) do
    remote_start(fun)
  end

  defp remote_tracer(:port, port) when is_port(port) do
    remote_start(fn ->
      port
    end)
  end

  defp remote_tracer(:process, {handler, handlerData}) do
    remote_start(fn ->
      start_tracer_process(handler, handlerData)
    end)
  end

  defp remote_tracer(:module, fun) when is_function(fun) do
    remote_start(fun)
  end

  defp remote_tracer(:module, {module, state}) do
    remote_start(fn ->
      {module, state}
    end)
  end

  defp remote_start(startTracer) do
    case (try do
            startTracer.()
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, reason} ->
        {:error, reason}

      tracer ->
        {:ok, tracer}
    end
  end

  def tracer(node, type, data) when node === node() do
    case tracer(type, data) do
      {:ok, _Dbg} ->
        {:ok, node}

      error ->
        error
    end
  end

  def tracer(node, type, data) do
    case (try do
            :net_adm.ping(node)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:bad_node, node}}

      :pang ->
        {:error, {:nodedown, node}}

      :pong ->
        req({:add_node, node, type, data})

      other ->
        {:error, other}
    end
  end

  def flush_trace_port() do
    trace_port_control(:flush)
  end

  def flush_trace_port(node) do
    trace_port_control(node, :flush)
  end

  def trace_port_control(operation) do
    trace_port_control(node(), operation)
  end

  def trace_port_control(node, :flush) do
    case get_tracer(node) do
      {:ok, port} when is_port(port) ->
        case (try do
                :rpc.call(node, :dbg, :deliver_and_flush, [port])
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          [0] ->
            :ok

          _ ->
            {:error, :not_supported_by_trace_driver}
        end

      _ ->
        {:error, :no_trace_driver}
    end
  end

  def trace_port_control(node, :get_listen_port) do
    case trace_port_control(node, ?p, '') do
      {:ok, <<0, ipPort::size(16)>>} ->
        {:ok, ipPort}

      {:ok, _Other} ->
        {:error, :not_supported_by_trace_driver}

      other ->
        other
    end
  end

  defp trace_port_control(node, command, arg) do
    case get_tracer(node) do
      {:ok, port} when is_port(port) ->
        {:ok,
         try do
           :rpc.call(node, :erlang, :port_control, [port, command, arg])
         catch
           :error, e -> {:EXIT, {e, __STACKTRACE__}}
           :exit, e -> {:EXIT, e}
           e -> e
         end}

      _ ->
        {:error, :no_trace_driver}
    end
  end

  def deliver_and_flush(port) do
    ref = :erlang.trace_delivered(:all)

    receive do
      {:trace_delivered, :all, ^ref} ->
        :ok
    end

    :erlang.port_control(port, ?f, '')
  end

  def trace_port(:file, {filename, :wrap, tail}) do
    trace_port(:file, {filename, :wrap, tail, 128 * 1024})
  end

  def trace_port(:file, {filename, :wrap, tail, wrapSize}) do
    trace_port(:file, {filename, :wrap, tail, wrapSize, 8})
  end

  def trace_port(
        :file,
        {filename, :wrap, tail, wrapSize, wrapCnt}
      )
      when is_list(tail) and is_integer(wrapSize) and
             wrapSize >= 0 and wrapSize < 1 <<< 32 and
             is_integer(wrapCnt) and wrapCnt >= 1 and
             wrapCnt < 1 <<< 32 do
    trace_port1(:file, filename, {:wrap, tail, wrapSize, wrapCnt, 0})
  end

  def trace_port(
        :file,
        {filename, :wrap, tail, {:time, wrapTime}, wrapCnt}
      )
      when is_list(tail) and is_integer(wrapTime) and
             wrapTime >= 1 and wrapTime < 1 <<< 32 and
             is_integer(wrapCnt) and wrapCnt >= 1 and
             wrapCnt < 1 <<< 32 do
    trace_port1(:file, filename, {:wrap, tail, 0, wrapCnt, wrapTime})
  end

  def trace_port(:file, filename) do
    trace_port1(:file, filename, :nowrap)
  end

  def trace_port(:ip, portno) when is_integer(portno) do
    trace_port(:ip, {portno, 200})
  end

  def trace_port(:ip, {portno, qsiz})
      when is_integer(portno) and
             is_integer(qsiz) do
    fn ->
      driver = 'trace_ip_drv'
      dir1 = :filename.join(:code.priv_dir(:runtime_tools), 'lib')

      case (try do
              :erl_ddll.load_driver(dir1, driver)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        :ok ->
          :ok

        _ ->
          dir2 =
            :filename.join(
              dir1,
              :erlang.system_info(:system_architecture)
            )

          try do
            :erl_ddll.load_driver(dir2, driver)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
      end

      l =
        :lists.flatten(
          :io_lib.format(
            '~s ~p ~p 2',
            [driver, portno, qsiz]
          )
        )

      :erlang.open_port({:spawn, l}, [:eof])
    end
  end

  defp trace_port1(:file, filename, options) do
    driver = 'trace_file_drv'

    fn ->
      name = :filename.absname(filename)

      {wrap, tail} =
        case options do
          {:wrap, t, wrapSize, wrapCnt, wrapTime} ->
            {:lists.flatten(
               :io_lib.format(
                 'w ~p ~p ~p ~p ',
                 [wrapSize, wrapCnt, wrapTime, length(name)]
               )
             ), t}

          :nowrap ->
            {'', ''}
        end

      command = driver ++ ' ' ++ wrap ++ 'n ' ++ name ++ tail
      dir1 = :filename.join(:code.priv_dir(:runtime_tools), 'lib')

      case (try do
              :erl_ddll.load_driver(dir1, driver)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        :ok ->
          :ok

        _ ->
          dir2 =
            :filename.join(
              dir1,
              :erlang.system_info(:system_architecture)
            )

          try do
            :erl_ddll.load_driver(dir2, driver)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
      end

      cond do
        :erlang.element(1, options) == :wrap ->
          files = wrap_postsort(wrap_presort(name, tail))

          :lists.foreach(
            fn n ->
              :file.delete(n)
            end,
            files
          )

        true ->
          :ok
      end

      :erlang.open_port({:spawn, command}, [:eof])
    end
  end

  def trace_client(:file, filename) do
    trace_client(:file, filename, {&dhandler/2, :user})
  end

  def trace_client(:follow_file, filename) do
    trace_client(:follow_file, filename, {&dhandler/2, :user})
  end

  def trace_client(:ip, portno) when is_integer(portno) do
    trace_client1(:ip, {'localhost', portno}, {&dhandler/2, :user})
  end

  def trace_client(:ip, {host, portno}) when is_integer(portno) do
    trace_client1(:ip, {host, portno}, {&dhandler/2, :user})
  end

  def trace_client(:file, {filename, :wrap, tail}, fD) do
    trace_client(:file, {filename, :wrap, tail, 128 * 1024}, fD)
  end

  def trace_client(:file, {filename, :wrap, tail, wrapSize}, fD) do
    trace_client(:file, {filename, :wrap, tail, wrapSize, 8}, fD)
  end

  def trace_client(
        :file,
        {_Filename, :wrap, tail, _WrapSize, wrapCnt} = wrapSpec,
        {fun, _Data} = fD
      )
      when is_list(tail) and is_function(fun) and
             is_integer(wrapCnt) and wrapCnt >= 1 do
    trace_client1(:file, wrapSpec, fD)
  end

  def trace_client(:file, filename, {fun, data})
      when is_function(fun) do
    trace_client1(:file, filename, {fun, data})
  end

  def trace_client(:follow_file, filename, {fun, data})
      when is_function(fun) do
    trace_client1(:follow_file, filename, {fun, data})
  end

  def trace_client(:ip, portno, {fun, data})
      when is_integer(portno) and is_function(fun) do
    trace_client1(:ip, {'localhost', portno}, {fun, data})
  end

  def trace_client(:ip, {host, portno}, {fun, data})
      when is_integer(portno) and is_function(fun) do
    trace_client1(:ip, {host, portno}, {fun, data})
  end

  defp trace_client1(type, openData, {handler, hData}) do
    case req(
           {:link_to,
            spawn(fn ->
              tc_loop(gen_reader(type, openData), handler, hData)
            end)}
         ) do
      {:ok, pid} ->
        pid

      other ->
        other
    end
  end

  def stop_trace_client(pid) when is_pid(pid) do
    :erlang.process_flag(:trap_exit, true)
    :erlang.link(pid)
    :erlang.exit(to_pidspec(pid), :abnormal)

    res =
      receive do
        {:EXIT, ^pid, _} ->
          :ok
      after
        5000 ->
          {:error, :timeout}
      end

    :erlang.process_flag(:trap_exit, false)
    res
  end

  def p(pid) do
    p(pid, [:m])
  end

  def p(pid, flags) when is_atom(flags) do
    p(pid, [flags])
  end

  def p(pid, flags) do
    req({:p, pid, flags})
  end

  def i() do
    req(:i)
  end

  def c(m, f, a) do
    c(m, f, a, :all)
  end

  def c(m, f, a, flags) when is_atom(flags) do
    c(m, f, a, [flags])
  end

  def c(m, f, a, flags) do
    case transform_flags(flags) do
      {:error, reason} ->
        {:error, reason}

      flags1 ->
        tracer()
        s = self()

        pid =
          spawn(fn ->
            c(s, m, f, a, [get_tracer_flag() | flags1])
          end)

        mref = :erlang.monitor(:process, pid)

        receive do
          {:DOWN, ^mref, _, _, reason} ->
            stop_clear()
            {:error, reason}

          {^pid, res} ->
            :erlang.demonitor(mref, [:flush])
            :timer.sleep(1)
            stop_clear()
            res
        end
    end
  end

  defp c(parent, m, f, a, flags) do
    :erlang.trace(self(), true, flags)
    res = apply(m, f, a)
    :erlang.trace(self(), false, [:all])
    send(parent, {self(), res})
  end

  def stop() do
    mref = :erlang.monitor(:process, :dbg)

    try do
      send(:dbg, {self(), :stop})
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    receive do
      {:DOWN, ^mref, _, _, _} ->
        :ok
    end
  end

  def stop_clear() do
    {:ok, _} = ctp()
    {:ok, _} = ctpe(:receive)
    {:ok, _} = ctpe(:send)
    stop()
  end

  defp req(r) do
    p = ensure()
    mref = :erlang.monitor(:process, p)

    try do
      send(p, {self(), r})
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    receive do
      {:DOWN, ^mref, _, _, _} ->
        exit(:dbg_server_crash)

      {:dbg, reply} ->
        :erlang.demonitor(mref, [:flush])
        reply
    end
  end

  defp ensure() do
    case :erlang.whereis(:dbg) do
      :undefined ->
        case start() do
          {:ok, p} ->
            p

          {:error, :already_started} ->
            :dbg
        end

      pid ->
        pid
    end
  end

  def start() do
    start(:no_tracer)
  end

  defp start(tracerFun) do
    s = self()

    case :erlang.whereis(:dbg) do
      :undefined ->
        dbg =
          spawn(fn ->
            init(s)
          end)

        receive do
          {^dbg, :started} ->
            :ok
        end

        case tracerFun do
          :no_tracer ->
            {:ok, dbg}

          fun when is_function(fun) ->
            req({:tracer, tracerFun})
        end

      pid when is_pid(pid) and is_function(tracerFun) ->
        req({:tracer, tracerFun})
    end
  end

  defp init(parent) do
    :erlang.process_flag(:trap_exit, true)
    :erlang.register(:dbg, self())
    send(parent, {self(), :started})
    loop({[], []}, [])
  end

  defp loop({c, t} = surviveLinks, table) do
    receive do
      {from, :i} ->
        modifier = modifier()

        reply =
          display_info(
            :lists.map(
              fn {n, _} ->
                n
              end,
              :erlang.get()
            ),
            modifier
          )

        reply(from, reply)
        loop(surviveLinks, table)

      {from, {:p, pid, flags}} ->
        reply(from, trace_process(pid, flags))
        loop(surviveLinks, table)

      {from, {:tracer, tracerFun}} when is_function(tracerFun) ->
        case :erlang.get(node()) do
          :undefined ->
            case (try do
                    tracerFun.()
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              {:EXIT, reason} ->
                reply(from, {:error, reason})

              tracer when is_pid(tracer) or is_port(tracer) ->
                :erlang.put(node(), {self(), tracer})
                reply(from, {:ok, self()})

              {module, _State} = tracer when is_atom(module) ->
                :erlang.put(node(), {self(), tracer})
                reply(from, {:ok, self()})
            end

          {_Relay, _Tracer} ->
            reply(from, {:error, :already_started})
        end

        loop(surviveLinks, table)

      {from, {:get_tracer, node}} ->
        case :erlang.get(node) do
          :undefined ->
            reply(from, {:error, {:no_tracer_on_node, node}})

          {_Relay, tracer} ->
            reply(from, {:ok, tracer})
        end

        loop(surviveLinks, table)

      {from, :get_table} ->
        tab =
          case table do
            [] ->
              new_pattern_table()

            _exists ->
              table
          end

        reply(from, {:ok, tab})
        loop(surviveLinks, tab)

      {_From, :stop} ->
        :lists.foreach(
          fn {node, {_Relay, port}} ->
            :rpc.call(node, :dbg, :deliver_and_flush, [port])
          end,
          :erlang.get()
        )

        exit(:done)

      {from, {:link_to, pid}} ->
        case (try do
                :erlang.link(pid)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, reason} ->
            reply(from, {:error, reason})
            loop(surviveLinks, table)

          _ ->
            reply(from, {:ok, pid})
            loop({[pid | c], t}, table)
        end

      {from, {:add_node, node}} ->
        case :erlang.get(node()) do
          :undefined ->
            reply(from, {:error, :no_local_tracer})
            loop(surviveLinks, table)

          {_LocalRelay, tracer} when is_port(tracer) ->
            reply(
              from,
              {:error, :cant_trace_remote_pid_to_local_port}
            )

            loop(surviveLinks, table)

          {_LocalRelay, tracer} when is_tuple(tracer) ->
            reply(
              from,
              {:error, :cant_trace_remote_pid_to_local_module}
            )

            loop(surviveLinks, table)

          {_LocalRelay, tracer} when is_pid(tracer) ->
            case (try do
                    relay(node, tracer)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              {:ok, relay} ->
                reply(from, {:ok, node})
                loop({c, [relay | t]}, table)

              {:EXIT, something} ->
                reply(from, {:error, something})
                loop(surviveLinks, table)

              error ->
                reply(from, error)
                loop(surviveLinks, table)
            end
        end

      {from, {:add_node, node, type, data}} ->
        case (try do
                relay(node, {type, data})
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:ok, relay} ->
            reply(from, {:ok, node})
            loop({c, [relay | t]}, table)

          {:EXIT, something} ->
            reply(from, {:error, something})
            loop(surviveLinks, table)

          error ->
            reply(from, error)
            loop(surviveLinks, table)
        end

      {from, {:remove_node, node}} ->
        :erlang.erase(node)
        reply(from, :ok)
        loop(surviveLinks, table)

      {from, :get_nodes} ->
        reply(
          from,
          :lists.map(
            fn {n, _} ->
              n
            end,
            :erlang.get()
          )
        )

        loop(surviveLinks, table)

      {:EXIT, pid, reason} ->
        case :lists.delete(pid, c) do
          ^c ->
            case :lists.delete(pid, t) do
              ^t ->
                modifier = modifier(:user)

                :io.format(:user, '** dbg got EXIT - terminating: ~' ++ modifier ++ 'p~n', [
                  reason
                ])

                exit(:done)

              newT ->
                :erlang.erase(node(pid))
                loop({c, newT}, table)
            end

          newC ->
            loop({newC, t}, table)
        end

      other ->
        modifier = modifier(:user)

        :io.format(:user, '** dbg got garbage: ~' ++ modifier ++ 'p~n', [
          {other, surviveLinks, table}
        ])

        loop(surviveLinks, table)
    end
  end

  defp reply(pid, reply) do
    send(pid, {:dbg, reply})
    :ok
  end

  defp start_tracer_process(handler, handlerData) do
    :erlang.spawn_opt(
      fn ->
        tracer_init(handler, handlerData)
      end,
      [:link, {:priority, :max}]
    )
  end

  defp tracer_init(handler, handlerData) do
    :erlang.process_flag(:trap_exit, true)
    tracer_loop(handler, handlerData)
  end

  defp tracer_loop(handler, hdata) do
    {state, suspended, traces} = recv_all_traces()
    newHdata = handle_traces(suspended, traces, handler, hdata)

    case state do
      :done ->
        exit(:normal)

      :loop ->
        tracer_loop(handler, newHdata)
    end
  end

  defp recv_all_traces() do
    recv_all_traces([], [], :infinity)
  end

  defp recv_all_traces(suspended0, traces, timeout) do
    receive do
      trace
      when is_tuple(trace) and
             :erlang.element(1, trace) == :trace ->
        suspended = suspend(trace, suspended0)
        recv_all_traces(suspended, [trace | traces], 0)

      trace
      when is_tuple(trace) and
             :erlang.element(1, trace) == :trace_ts ->
        suspended = suspend(trace, suspended0)
        recv_all_traces(suspended, [trace | traces], 0)

      trace
      when is_tuple(trace) and
             :erlang.element(1, trace) == :seq_trace ->
        suspended = suspend(trace, suspended0)
        recv_all_traces(suspended, [trace | traces], 0)

      trace
      when is_tuple(trace) and
             :erlang.element(1, trace) == :drop ->
        suspended = suspend(trace, suspended0)
        recv_all_traces(suspended, [trace | traces], 0)

      {:EXIT, _Pid, _Reason} ->
        {:done, suspended0, traces}

      other ->
        modifier = modifier(:user)
        :io.format(:user, '** tracer received garbage: ~' ++ modifier ++ 'p~n', [other])
        recv_all_traces(suspended0, traces, timeout)
    after
      timeout ->
        {:loop, suspended0, traces}
    end
  end

  defp handle_traces(suspended, traces, handler, hdata) do
    case (try do
            invoke_handler(traces, handler, hdata)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, reason} ->
        resume(suspended)
        exit({:trace_handler_crashed, reason})

      newHdata ->
        resume(suspended)
        newHdata
    end
  end

  defp invoke_handler([tr | traces], handler, hdata0) do
    hdata = invoke_handler(traces, handler, hdata0)
    handler.(tr, hdata)
  end

  defp invoke_handler([], _Handler, hdata) do
    hdata
  end

  defp suspend({:trace, from, :call, _Func}, suspended)
       when node(from) == node() do
    case (try do
            :erlang.suspend_process(
              from,
              [:unless_suspending, :asynchronous]
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      true ->
        [from | suspended]

      _ ->
        suspended
    end
  end

  defp suspend(_Other, suspended) do
    suspended
  end

  defp resume([pid | pids]) when node(pid) == node() do
    try do
      :erlang.resume_process(pid)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    resume(pids)
  end

  defp resume([]) do
    :ok
  end

  defp trac(proc, how, flags) when is_atom(proc) do
    case :erlang.get() do
      [] ->
        {:error, :no_tracers}

      nodes ->
        matched =
          for {node, nodeInfo} <- nodes do
            trac(node, nodeInfo, proc, how, flags)
          end

        {:ok, matched}
    end
  end

  defp trac(proc, how, flags) do
    pid = to_pid(proc)

    case pid do
      {:badpid, _} ->
        {:error, pid}

      _ ->
        node =
          cond do
            is_pid(pid) ->
              node(pid)

            true ->
              node()
          end

        case :erlang.get(node) do
          :undefined ->
            {:error, {:no_tracer_on_node, node}}

          nodeInfo ->
            match = trac(node, nodeInfo, pid, how, flags)
            {:ok, [match]}
        end
    end
  end

  defp trac(node, {_Replay, tracer}, atomPid, how, flags) do
    case :rpc.call(node, :dbg, :erlang_trace, [atomPid, how, [get_tracer_flag(tracer) | flags]]) do
      n when is_integer(n) ->
        {:matched, node, n}

      {:badrpc, reason} ->
        {:matched, node, 0, reason}

      else__ ->
        {:matched, node, 0, else__}
    end
  end

  def erlang_trace(atomPid, how, flags) do
    case to_pidspec(atomPid) do
      {:badpid, _} ->
        {:no_proc, atomPid}

      p ->
        :erlang.trace(p, how, flags)
    end
  end

  defp relay(node, to) when node != node() do
    case :erlang.get(node) do
      :undefined ->
        s = self()

        pid =
          :erlang.spawn_link(
            node,
            fn ->
              do_relay(s, to)
            end
          )

        receive do
          {:started, remote} ->
            :erlang.put(node, {pid, remote})
        end

        {:ok, pid}

      {_Relay, portOrPid} ->
        {:error, {:already_started, portOrPid}}
    end
  end

  defp do_relay(parent, relP) do
    :erlang.process_flag(:trap_exit, true)

    case relP do
      {type, data} ->
        {:ok, tracer} = remote_tracer(type, data)
        send(parent, {:started, tracer})
        :ok

      pid when is_pid(pid) ->
        send(parent, {:started, self()})
        :ok
    end

    do_relay_1(relP)
  end

  defp do_relay_1(relP) do
    receive do
      {:EXIT, _P, _} ->
        exit(:normal)

      traceInfo when is_pid(relP) ->
        send(relP, traceInfo)
        do_relay_1(relP)

      other ->
        modifier = modifier(:user)
        :io.format(:user, '** relay got garbage: ~' ++ modifier ++ 'p~n', [other])
        do_relay_1(relP)
    end
  end

  def dhandler(:end_of_trace, out) do
    out
  end

  def dhandler(trace, out)
      when :erlang.element(
             1,
             trace
           ) == :trace and
             tuple_size(trace) >= 3 do
    dhandler1(trace, tuple_size(trace), out(out))
  end

  def dhandler(trace, out)
      when :erlang.element(
             1,
             trace
           ) == :trace_ts and
             tuple_size(trace) >= 4 do
    dhandler1(trace, tuple_size(trace) - 1, :erlang.element(tuple_size(trace), trace), out(out))
  end

  def dhandler(trace, out)
      when :erlang.element(
             1,
             trace
           ) == :drop and
             tuple_size(trace) === 2 do
    {device, modifier} = out(out)
    :io.format(device, '*** Dropped ~p messages.~n', [:erlang.element(2, trace)])
    {device, modifier}
  end

  def dhandler(trace, out)
      when :erlang.element(
             1,
             trace
           ) == :seq_trace and
             tuple_size(trace) >= 3 do
    {device, modifier} = out(out)

    seqTraceInfo =
      case trace do
        {:seq_trace, lbl, sTI, tS} ->
          :io.format(device, 'SeqTrace ~p [~p]: ', [tS, lbl])
          sTI

        {:seq_trace, lbl, sTI} ->
          :io.format(device, 'SeqTrace [~p]: ', [lbl])
          sTI
      end

    case seqTraceInfo do
      {:send, ser, fr, to, mes} ->
        :io.format(device, '(~p) ~p ! ~' ++ modifier ++ 'p [Serial: ~p]~n', [fr, to, mes, ser])

      {:receive, ser, fr, to, mes} ->
        :io.format(device, '(~p) << ~' ++ modifier ++ 'p [Serial: ~p, From: ~p]~n', [
          to,
          mes,
          ser,
          fr
        ])

      {:print, ser, fr, _, info} ->
        :io.format(device, '-> ~' ++ modifier ++ 'p [Serial: ~p, From: ~p]~n', [info, ser, fr])

      else__ ->
        :io.format(device, '~' ++ modifier ++ 'p~n', [else__])
    end

    {device, modifier}
  end

  def dhandler(_Trace, out) do
    out
  end

  defp dhandler1(trace, size, {device, modifier}) do
    from = :erlang.element(2, trace)

    case :erlang.element(3, trace) do
      :receive ->
        case :erlang.element(4, trace) do
          {:dbg, :ok} ->
            :ok

          message ->
            :io.format(device, '(~p) << ~' ++ modifier ++ 'p~n', [from, message])
        end

      :send ->
        message = :erlang.element(4, trace)
        to = :erlang.element(5, trace)
        :io.format(device, '(~p) ~p ! ~' ++ modifier ++ 'p~n', [from, to, message])

      :call ->
        case :erlang.element(4, trace) do
          mFA when size == 5 ->
            message = :erlang.element(5, trace)

            :io.format(device, '(~p) call ~' ++ modifier ++ 's (~' ++ modifier ++ 'p)~n', [
              from,
              ffunc(mFA, modifier),
              message
            ])

          mFA ->
            :io.format(device, '(~p) call ~' ++ modifier ++ 's~n', [from, ffunc(mFA, modifier)])
        end

      :return ->
        case :erlang.element(4, trace) do
          mFA when size == 5 ->
            ret = :erlang.element(5, trace)

            :io.format(device, '(~p) old_ret ~' ++ modifier ++ 's -> ~' ++ modifier ++ 'p~n', [
              from,
              ffunc(mFA, modifier),
              ret
            ])

          mFA ->
            :io.format(device, '(~p) old_ret ~' ++ modifier ++ 's~n', [from, ffunc(mFA, modifier)])
        end

      :return_from ->
        mFA = :erlang.element(4, trace)
        ret = :erlang.element(5, trace)

        :io.format(device, '(~p) returned from ~' ++ modifier ++ 's -> ~' ++ modifier ++ 'p~n', [
          from,
          ffunc(mFA, modifier),
          ret
        ])

      :return_to ->
        mFA = :erlang.element(4, trace)

        :io.format(device, '(~p) returning to ~' ++ modifier ++ 's~n', [
          from,
          ffunc(mFA, modifier)
        ])

      :spawn when size == 5 ->
        pid = :erlang.element(4, trace)
        mFA = :erlang.element(5, trace)

        :io.format(device, '(~p) spawn ~p as ~' ++ modifier ++ 's~n', [
          from,
          pid,
          ffunc(mFA, modifier)
        ])

      op ->
        :io.format(device, '(~p) ~p ~' ++ modifier ++ 's~n', [
          from,
          op,
          ftup(trace, 4, size, modifier)
        ])
    end

    {device, modifier}
  end

  defp dhandler1(trace, size, tS, {device, modifier}) do
    from = :erlang.element(2, trace)

    case :erlang.element(3, trace) do
      :receive ->
        case :erlang.element(4, trace) do
          {:dbg, :ok} ->
            :ok

          message ->
            :io.format(device, '(~p) << ~' ++ modifier ++ 'p (Timestamp: ~p)~n', [
              from,
              message,
              tS
            ])
        end

      :send ->
        message = :erlang.element(4, trace)
        to = :erlang.element(5, trace)

        :io.format(device, '(~p) ~p ! ~' ++ modifier ++ 'p (Timestamp: ~p)~n', [
          from,
          to,
          message,
          tS
        ])

      :call ->
        case :erlang.element(4, trace) do
          mFA when size == 5 ->
            message = :erlang.element(5, trace)

            :io.format(
              device,
              '(~p) call ~' ++ modifier ++ 's (~' ++ modifier ++ 'p) (Timestamp: ~p)~n',
              [from, ffunc(mFA, modifier), message, tS]
            )

          mFA ->
            :io.format(device, '(~p) call ~' ++ modifier ++ 's (Timestamp: ~p)~n', [
              from,
              ffunc(mFA, modifier),
              tS
            ])
        end

      :return ->
        case :erlang.element(4, trace) do
          mFA when size == 5 ->
            ret = :erlang.element(5, trace)

            :io.format(
              device,
              '(~p) old_ret ~' ++ modifier ++ 's -> ~' ++ modifier ++ 'p (Timestamp: ~p)~n',
              [from, ffunc(mFA, modifier), ret, tS]
            )

          mFA ->
            :io.format(device, '(~p) old_ret ~' ++ modifier ++ 's (Timestamp: ~p)~n', [
              from,
              ffunc(mFA, modifier),
              tS
            ])
        end

      :return_from ->
        mFA = :erlang.element(4, trace)
        ret = :erlang.element(5, trace)

        :io.format(
          device,
          '(~p) returned from ~' ++ modifier ++ 's -> ~' ++ modifier ++ 'p (Timestamp: ~p)~n',
          [from, ffunc(mFA, modifier), ret, tS]
        )

      :return_to ->
        mFA = :erlang.element(4, trace)

        :io.format(device, '(~p) returning to ~' ++ modifier ++ 's (Timestamp: ~p)~n', [
          from,
          ffunc(mFA, modifier),
          tS
        ])

      :spawn when size == 5 ->
        pid = :erlang.element(4, trace)
        mFA = :erlang.element(5, trace)

        :io.format(device, '(~p) spawn ~p as ~' ++ modifier ++ 's (Timestamp: ~p)~n', [
          from,
          pid,
          ffunc(mFA, modifier),
          tS
        ])

      op ->
        :io.format(device, '(~p) ~p ~' ++ modifier ++ 's (Timestamp: ~p)~n', [
          from,
          op,
          ftup(trace, 4, size, modifier),
          tS
        ])
    end

    {device, modifier}
  end

  defp ffunc({m, f, argl}, modifier) when is_list(argl) do
    :io_lib.format(
      '~p:~' ++ modifier ++ 'p(~' ++ modifier ++ 's)',
      [m, f, fargs(argl, modifier)]
    )
  end

  defp ffunc({m, f, arity}, modifier) do
    :io_lib.format('~p:~' ++ modifier ++ 'p/~p', [m, f, arity])
  end

  defp ffunc(x, modifier) do
    :io_lib.format('~' ++ modifier ++ 'p', [x])
  end

  defp fargs(arity, _) when is_integer(arity) do
    :erlang.integer_to_list(arity)
  end

  defp fargs([], _) do
    []
  end

  defp fargs([a], modifier) do
    :io_lib.format('~' ++ modifier ++ 'p', [a])
  end

  defp fargs([a | args], modifier) do
    [
      :io_lib.format('~' ++ modifier ++ 'p,', [a])
      | fargs(
          args,
          modifier
        )
    ]
  end

  defp fargs(a, modifier) do
    :io_lib.format('~' ++ modifier ++ 'p', [a])
  end

  defp ftup(trace, index, index, modifier) do
    :io_lib.format(
      '~' ++ modifier ++ 'p',
      [:erlang.element(index, trace)]
    )
  end

  defp ftup(trace, index, size, modifier) do
    [
      :io_lib.format(
        '~' ++ modifier ++ 'p ',
        [:erlang.element(index, trace)]
      )
      | ftup(trace, index + 1, size, modifier)
    ]
  end

  defp out({_, _} = out) do
    out
  end

  defp out(device) do
    {device, modifier(device)}
  end

  defp modifier() do
    modifier(:erlang.group_leader())
  end

  defp modifier(device) do
    encoding =
      case :io.getopts(device) do
        list when is_list(list) ->
          :proplists.get_value(:encoding, list, :latin1)

        _ ->
          :latin1
      end

    encoding_to_modifier(encoding)
  end

  defp encoding_to_modifier(:latin1) do
    ''
  end

  defp encoding_to_modifier(_) do
    't'
  end

  defp trace_process(pid, [:clear]) do
    trac(pid, false, all())
  end

  defp trace_process(pid, flags0) do
    case transform_flags(flags0) do
      {:error, reason} ->
        {:error, reason}

      flags ->
        trac(pid, true, flags)
    end
  end

  def transform_flags(flags0) do
    transform_flags(flags0, [])
  end

  defp transform_flags([], acc) do
    acc
  end

  defp transform_flags([:m | tail], acc) do
    transform_flags(tail, [:send, :receive | acc])
  end

  defp transform_flags([:s | tail], acc) do
    transform_flags(tail, [:send | acc])
  end

  defp transform_flags([:r | tail], acc) do
    transform_flags(tail, [:receive | acc])
  end

  defp transform_flags([:c | tail], acc) do
    transform_flags(tail, [:call | acc])
  end

  defp transform_flags([:call | tail], acc) do
    transform_flags(tail, [:call | acc])
  end

  defp transform_flags([:p | tail], acc) do
    transform_flags(tail, [:procs | acc])
  end

  defp transform_flags([:sos | tail], acc) do
    transform_flags(tail, [:set_on_spawn | acc])
  end

  defp transform_flags([:sol | tail], acc) do
    transform_flags(tail, [:set_on_link | acc])
  end

  defp transform_flags([:sofs | tail], acc) do
    transform_flags(tail, [:set_on_first_spawn | acc])
  end

  defp transform_flags([:sofl | tail], acc) do
    transform_flags(tail, [:set_on_first_link | acc])
  end

  defp transform_flags([:all | _], _Acc) do
    all() -- [:silent, :running]
  end

  defp transform_flags([f | tail] = list, acc) when is_atom(f) do
    case :lists.member(f, all()) do
      true ->
        transform_flags(tail, [f | acc])

      false ->
        {:error, {:bad_flags, list}}
    end
  end

  defp transform_flags(bad, _Acc) do
    {:error, {:bad_flags, bad}}
  end

  defp all() do
    [
      :send,
      :receive,
      :call,
      :procs,
      :ports,
      :garbage_collection,
      :running,
      :set_on_spawn,
      :set_on_first_spawn,
      :set_on_link,
      :set_on_first_link,
      :timestamp,
      :monotonic_timestamp,
      :strict_monotonic_timestamp,
      :arity,
      :return_to,
      :silent,
      :running_procs,
      :running_ports,
      :exiting
    ]
  end

  defp display_info([node | nodes], modifier) do
    :io.format('~nNode ~w:~n', [node])
    :io.format('~-12s ~-21s Trace ~n', ['Pid', 'Initial call'])
    list = :rpc.call(node, :dbg, :get_info, [])
    display_info1(list, modifier)
    display_info(nodes, modifier)
  end

  defp display_info([], _) do
    :ok
  end

  defp display_info1([{pid, call, flags} | t], modifier) do
    :io.format(
      '~-12s ~-21' ++ modifier ++ 's ~s~n',
      [
        :io_lib.format('~w', [pid]),
        :io_lib.format('~' ++ modifier ++ 'p', [call]),
        format_trace(flags)
      ]
    )

    display_info1(t, modifier)
  end

  defp display_info1([], _) do
    :ok
  end

  def get_info() do
    get_info(
      :erlang.processes(),
      get_info(:erlang.ports(), [])
    )
  end

  defp get_info([port | t], acc) when is_port(port) do
    case pinfo(port, :name) do
      :undefined ->
        get_info(t, acc)

      {:name, name} ->
        get_info(t, get_tinfo(port, name, acc))
    end
  end

  defp get_info([pid | t], acc) do
    case pinfo(pid, :initial_call) do
      :undefined ->
        get_info(t, acc)

      {:initial_call, call} ->
        get_info(t, get_tinfo(pid, call, acc))
    end
  end

  defp get_info([], acc) do
    acc
  end

  defp get_tinfo(p, id, acc) do
    case tinfo(p, :flags) do
      :undefined ->
        acc

      {:flags, []} ->
        acc

      {:flags, flags} ->
        [{p, id, flags} | acc]
    end
  end

  defp format_trace([]) do
    []
  end

  defp format_trace([item]) do
    [ts(item)]
  end

  defp format_trace([item | t]) do
    [ts(item), ' | ', format_trace(t)]
  end

  defp ts(:send) do
    's'
  end

  defp ts(:receive) do
    'r'
  end

  defp ts(:call) do
    'c'
  end

  defp ts(:procs) do
    'p'
  end

  defp ts(:set_on_spawn) do
    'sos'
  end

  defp ts(:set_on_first_spawn) do
    'sofs'
  end

  defp ts(:set_on_link) do
    'sol'
  end

  defp ts(:set_on_first_link) do
    'sofl'
  end

  defp ts(other) do
    :erlang.atom_to_list(other)
  end

  defp to_pidspec(x) when is_pid(x) do
    case :erlang.is_process_alive(x) do
      true ->
        x

      false ->
        {:badpid, x}
    end
  end

  defp to_pidspec(x) when is_port(x) do
    case :erlang.port_info(x) do
      :undefined ->
        {:badport, x}

      _ ->
        x
    end
  end

  defp to_pidspec(tag)
       when tag === :all or tag === :ports or
              tag === :processes or tag === :new or
              tag === :new_ports or tag === :new_processes or
              tag === :existing or tag === :existing_ports or
              tag === :existing_processes do
    tag
  end

  defp to_pidspec(x) when is_atom(x) do
    case :erlang.whereis(x) do
      :undefined ->
        {:badpid, x}

      pid ->
        pid
    end
  end

  defp to_pidspec(x) do
    {:badpid, x}
  end

  defp to_pid(x) when is_pid(x) do
    x
  end

  defp to_pid(x) when is_port(x) do
    x
  end

  defp to_pid(x) when is_integer(x) do
    to_pid({0, x, 0})
  end

  defp to_pid({x, y, z}) do
    to_pid(
      :lists.concat([
        '<',
        :erlang.integer_to_list(x),
        '.',
        :erlang.integer_to_list(y),
        '.',
        :erlang.integer_to_list(z),
        '>'
      ])
    )
  end

  defp to_pid(x) when is_list(x) do
    try do
      :erlang.list_to_pid(x)
    catch
      :error, :badarg ->
        {:badpid, x}
    else
      pid ->
        pid
    end
  end

  defp to_pid(x) do
    {:badpid, x}
  end

  defp pinfo(p, x) when node(p) == node() and is_port(p) do
    :erlang.port_info(p, x)
  end

  defp pinfo(p, x) when node(p) == node() do
    :erlang.process_info(p, x)
  end

  defp pinfo(p, x) when is_port(p) do
    check(:rpc.call(node(p), :erlang, :port_info, [p, x]))
  end

  defp pinfo(p, x) do
    check(:rpc.call(node(p), :erlang, :process_info, [p, x]))
  end

  defp tinfo(p, x) when node(p) == node() do
    :erlang.trace_info(p, x)
  end

  defp tinfo(p, x) do
    check(:rpc.call(node(p), :erlang, :trace_info, [p, x]))
  end

  defp check({:badrpc, _}) do
    :undefined
  end

  defp check(x) do
    x
  end

  defp tc_loop([term | tail], handler, hData0) do
    hData = handler.(term, hData0)
    tc_loop(tail, handler, hData)
  end

  defp tc_loop([], handler, hData) do
    handler.(:end_of_trace, hData)
    exit(:normal)
  end

  defp tc_loop(reader, handler, hData)
       when is_function(reader) do
    tc_loop(reader.(), handler, hData)
  end

  defp tc_loop(other, _Handler, _HData) do
    modifier = modifier()
    :io.format('~p:tc_loop ~' ++ modifier ++ 'p~n', [:dbg, other])
    exit({:unknown_term_from_reader, other})
  end

  defp gen_reader(:ip, {host, portno}) do
    case :gen_tcp.connect(host, portno, [{:active, false}, :binary]) do
      {:ok, sock} ->
        p(sock, :clear)
        mk_reader(&ip_read/2, sock)

      error ->
        exit(error)
    end
  end

  defp gen_reader(:file, {filename, :wrap, tail, _, wrapCnt}) do
    mk_reader_wrap(
      wrap_sort(
        wrap_presort(filename, tail),
        wrapCnt
      )
    )
  end

  defp gen_reader(:file, filename) do
    gen_reader_file(&file_read/2, filename)
  end

  defp gen_reader(:follow_file, filename) do
    gen_reader_file(&follow_read/2, filename)
  end

  defp gen_reader_file(readFun, filename) do
    case :file.open(
           filename,
           [:read, :raw, :binary, :read_ahead]
         ) do
      {:ok, file} ->
        mk_reader(readFun, file)

      error ->
        exit({:client_cannot_open, error})
    end
  end

  defp mk_reader(readFun, source) do
    fn ->
      case read_term(readFun, source) do
        {:ok, term} ->
          [term | mk_reader(readFun, source)]

        :eof ->
          []
      end
    end
  end

  defp mk_reader_wrap([]) do
    []
  end

  defp mk_reader_wrap([hd | _] = wrapFiles) do
    case :file.open(
           wrap_name(hd),
           [:read, :raw, :binary, :read_ahead]
         ) do
      {:ok, file} ->
        mk_reader_wrap(wrapFiles, file)

      error ->
        exit({:client_cannot_open, error})
    end
  end

  defp mk_reader_wrap([_Hd | tail] = wrapFiles, file) do
    fn ->
      case read_term(&file_read/2, file) do
        {:ok, term} ->
          [term | mk_reader_wrap(wrapFiles, file)]

        :eof ->
          :ok = :file.close(file)

          case tail do
            [_ | _] ->
              mk_reader_wrap(tail)

            [] ->
              []
          end
      end
    end
  end

  defp read_term(readFun, source) do
    case readFun.(source, 5) do
      bin when is_binary(bin) ->
        read_term(readFun, source, bin)

      list when is_list(list) ->
        read_term(readFun, source, :erlang.list_to_binary(list))

      :eof ->
        :eof
    end
  end

  defp read_term(readFun, source, <<op, size::size(32)>> = tag) do
    case op do
      0 ->
        case readFun.(source, size) do
          :eof ->
            exit({:"trace term missing", :erlang.binary_to_list(tag)})

          bin when is_binary(bin) ->
            {:ok, :erlang.binary_to_term(bin)}

          list when is_list(list) ->
            {:ok, :erlang.binary_to_term(:erlang.list_to_binary(list))}
        end

      1 ->
        {:ok, {:drop, size}}

      junk ->
        exit({:"bad trace tag", junk})
    end
  end

  defp file_read(file, n) do
    case :file.read(file, n) do
      {:ok, bin} when byte_size(bin) === n ->
        bin

      {:ok, bin} when is_binary(bin) ->
        exit({:"truncated file", :erlang.binary_to_list(bin)})

      :eof ->
        :eof

      {:error, reason} ->
        exit({:"file read error", reason})
    end
  end

  defp follow_read(file, n) do
    follow_read(file, n, :cur)
  end

  defp follow_read(file, n, pos) do
    case :file.position(file, pos) do
      {:ok, offset} ->
        case :file.read(file, n) do
          {:ok, bin} when byte_size(bin) === n ->
            bin

          {:ok, bin} when is_binary(bin) ->
            follow_read(file, n, offset)

          :eof ->
            follow_read(file, n, offset)

          {:error, reason} ->
            exit({:"file read error", reason})
        end

      {:error, reason} ->
        exit({:"file position error", reason})
    end
  end

  defp ip_read(socket, n) do
    case :gen_tcp.recv(socket, n) do
      {:ok, bin} when byte_size(bin) < n ->
        [bin | ip_read(socket, n - byte_size(bin))]

      {:ok, bin} when byte_size(bin) == n ->
        [bin]

      {:ok, bin} when is_binary(bin) ->
        exit({:"socket read too much data", bin})

      {:error, :closed} ->
        :eof

      {:error, _Reason} = error ->
        exit({:"socket read error", error})
    end
  end

  def get_tracer() do
    req({:get_tracer, node()})
  end

  def get_tracer(node) do
    req({:get_tracer, node})
  end

  defp get_tracer_flag() do
    {:ok, tracer} = get_tracer()
    get_tracer_flag(tracer)
  end

  defp get_tracer_flag({module, state}) do
    {:tracer, module, state}
  end

  defp get_tracer_flag(port = pid) when is_port(port) or is_pid(pid) do
    {:tracer, ^pid = port}
  end

  defp save_pattern([]) do
    0
  end

  defp save_pattern(p) do
    try do
      save_pattern(p, get_pattern_table())
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp save_pattern(pattern, pT) do
    last = last_pattern(:ets.last(pT), pT)
    bPattern = :erlang.term_to_binary(pattern)

    case :ets.match_object(pT, {:_, bPattern}) do
      [] ->
        :ets.insert(pT, {last + 1, bPattern})
        last + 1

      [{n, ^bPattern}] ->
        n
    end
  end

  defp last_pattern(:"$end_of_table", _PT) do
    0
  end

  defp last_pattern(i, pT) when is_atom(i) do
    last_pattern(:ets.prev(pT, i), pT)
  end

  defp last_pattern(i, _PT) when is_integer(i) do
    i
  end

  defp last_pattern(_, _) do
    throw({:error, :badtable})
  end

  defp get_pattern_table() do
    {:ok, ret} = req(:get_table)
    ret
  end

  defp new_pattern_table() do
    pT = :ets.new(:dbg_tab, [:ordered_set, :public])

    :ets.insert(
      pT,
      {:x, :erlang.term_to_binary([{:_, [], [{:exception_trace}]}])}
    )

    :ets.insert(
      pT,
      {:exception_trace, :erlang.term_to_binary(:x)}
    )

    :ets.insert(
      pT,
      {:c, :erlang.term_to_binary([{:_, [], [{:message, {:caller}}]}])}
    )

    :ets.insert(
      pT,
      {:caller_trace, :erlang.term_to_binary(:c)}
    )

    :ets.insert(
      pT,
      {:cx, :erlang.term_to_binary([{:_, [], [{:exception_trace}, {:message, {:caller}}]}])}
    )

    :ets.insert(
      pT,
      {:caller_exception_trace, :erlang.term_to_binary(:cx)}
    )

    pT
  end

  defp pt_doforall(fun, ld) do
    t = get_pattern_table()
    pt_doforall(t, fun, :ets.first(t), ld)
  end

  defp pt_doforall(_, _, :"$end_of_table", _Ld) do
    :ok
  end

  defp pt_doforall(t, fun, key, ld) do
    [{a, b}] = :ets.lookup(t, key)
    nLd = fun.({a, :erlang.binary_to_term(b)}, ld)
    pt_doforall(t, fun, :ets.next(t, key), nLd)
  end

  defp lint_tp([]) do
    {:ok, []}
  end

  defp lint_tp(pattern) do
    case :erlang.match_spec_test([], pattern, :trace) do
      {:ok, _Res, warnings, _Flags} ->
        {:ok, warnings}

      {:error, reasons} ->
        {:error, reasons}
    end
  end

  defp check_list(t) do
    case (try do
            :lists.foldl(
              fn val, _ ->
                {:ok, _, _, _} = :erlang.match_spec_test([], val, :trace)
                :ok
              end,
              :ok,
              t
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, :bad_match_spec}

      :ok ->
        :ok

      _Else ->
        {:error, :badfile}
    end
  end

  def wrap_presort(filename, tail) do
    name = :filename.basename(filename)
    dirname = :filename.dirname(filename)

    case :file.list_dir(dirname) do
      {:ok, files} ->
        :lists.zf(
          fn n ->
            case match_front(n, name) do
              false ->
                false

              x ->
                case match_rear(x, tail) do
                  false ->
                    false

                  c ->
                    case match_0_9(c) do
                      true ->
                        {true,
                         wrap_encode(
                           :filename.join(dirname, n),
                           c
                         )}

                      false ->
                        false
                    end
                end
            end
          end,
          files
        )

      _ ->
        []
    end
  end

  def wrap_sort(files, n) do
    wrap_sortfix(:lists.sort(files), n)
  end

  def wrap_sortfix([], n) when n >= 1 do
    []
  end

  def wrap_sortfix([], _N) do
    exit(:inconsistent_wrap_file_trace_set)
  end

  def wrap_sortfix([{0, _}] = files, n) when n >= 1 do
    files
  end

  def wrap_sortfix([{0, _}], _N) do
    exit(:inconsistent_wrap_file_trace_set)
  end

  def wrap_sortfix([{0, _} | _] = files, n) when n >= 1 do
    wrap_sortfix_1(files, n, [], files)
  end

  def wrap_sortfix([{1, _} | _] = files, n) when n >= 1 do
    wrap_sortfix_2(files, n, [], files)
  end

  def wrap_sortfix([{_C, _} | _], _N) do
    exit(:inconsistent_wrap_file_trace_set)
  end

  defp wrap_sortfix_1([{c, _}], n, _R, files) when c < n do
    files
  end

  defp wrap_sortfix_1([{c1, _} = f1 | [{c2, _} | _] = tail], n, r, files)
       when c1 + 1 == c2 and c2 < n do
    wrap_sortfix_1(tail, n, [f1 | r], files)
  end

  defp wrap_sortfix_1([{c1, _} = f1 | [{c2, _} | _] = tail], n, r, _Files)
       when c1 + 2 == c2 and c2 <= n do
    wrap_sortfix_2(tail, n, :lists.reverse([f1 | r]), tail)
  end

  defp wrap_sortfix_1([_F1, _F2 | _], _N, _R, _Files) do
    exit(:inconsistent_wrap_file_trace_set)
  end

  defp wrap_sortfix_2([{n, _}], n, r, files) do
    files ++ r
  end

  defp wrap_sortfix_2([{_C, _}], _N, _R, _Files) do
    exit(:inconsistent_wrap_file_trace_set)
  end

  defp wrap_sortfix_2([{c1, _} | [{c2, _} | _] = tail], n, r, files)
       when c1 + 1 == c2 and c2 <= n do
    wrap_sortfix_2(tail, n, r, files)
  end

  defp wrap_sortfix_2([{_C1, _}, {_C2, _} | _], _N, _R, _Files) do
    exit(:inconsistent_wrap_file_trace_set)
  end

  def wrap_postsort(files) do
    :lists.map(&wrap_name/1, files)
  end

  defp wrap_encode(n, c) do
    {:erlang.list_to_integer(c), n}
  end

  defp wrap_name({_C, n}) do
    n
  end

  def match_front(listA, []) when is_list(listA) do
    listA
  end

  def match_front([], listB) when is_list(listB) do
    false
  end

  def match_front([hd | tlA], [hd | tlB]) do
    match_front(tlA, tlB)
  end

  def match_front([_HdA | _], [_HdB | _]) do
    false
  end

  def match_rear(listA, listB)
      when is_list(listA) and
             is_list(listB) do
    case match_front(
           :lists.reverse(listA),
           :lists.reverse(listB)
         ) do
      false ->
        false

      list ->
        :lists.reverse(list)
    end
  end

  def match_0_9([]) do
    false
  end

  def match_0_9([h])
      when is_integer(h) and ?0 <= h and
             h <= ?9 do
    true
  end

  def match_0_9([h | t])
      when is_integer(h) and ?0 <= h and
             h <= ?9 do
    match_0_9(t)
  end

  def match_0_9(l) when is_list(l) do
    false
  end

  defp help_display([]) do
    :io.format('~n', [])
    :ok
  end

  defp help_display([h | t]) do
    :io.format('~s~n', [h])
    help_display(t)
  end

  def h() do
    help_display([
      'The following help items are available:',
      '   p, c',
      '       - Set trace flags for processes',
      '   tp, tpl, ctp, ctpl, ctpg, ltp, dtp, wtp, rtp',
      '       - Manipulate trace patterns for functions',
      '   n, cn, ln',
      '       - Add/remove traced nodes.',
      '   tracer, trace_port, trace_client, get_tracer, stop, stop_clear',
      '       - Manipulate tracer process/port',
      '   i',
      '       - Info',
      '',
      'call dbg:h(Item) for brief help a brief description',
      'of one of the items above.'
    ])
  end

  def h(:p) do
    help_display([
      'p(Item) -> {ok, MatchDesc} | {error, term()}',
      ' - Traces messages to and from Item.',
      'p(Item, Flags) -> {ok, MatchDesc} | {error, term()}',
      ' - Traces Item according to Flags.',
      '   Flags can be one of s,r,m,c,p,sos,sol,sofs,',
      '   sofl,all,clear or any flag accepted by erlang:trace/3'
    ])
  end

  def h(:c) do
    help_display([
      'c(Mod, Fun, Args)',
      ' - Evaluates apply(M,F,Args) with all trace flags set.',
      'c(Mod, Fun, Args, Flags)',
      ' - Evaluates apply(M,F,Args) with Flags trace flags set.'
    ])
  end

  def h(:i) do
    help_display(['i() -> ok', ' - Displays information about all traced processes.'])
  end

  def h(:tp) do
    help_display([
      'tp(Module,MatchSpec)',
      ' - Same as tp({Module, \'_\', \'_\'}, MatchSpec)',
      'tp(Module,Function,MatchSpec)',
      ' - Same as tp({Module, Function, \'_\'}, MatchSpec)',
      'tp(Module, Function, Arity, MatchSpec)',
      ' - Same as tp({Module, Function, Arity}, MatchSpec)',
      'tp({Module, Function, Arity}, MatchSpec) -> {ok, MatchDesc} | {error, term()}',
      ' - Set pattern for traced global function calls.'
    ])
  end

  def h(:tpl) do
    help_display([
      'tpl(Module,MatchSpec)',
      ' - Same as tpl({Module, \'_\', \'_\'}, MatchSpec)',
      'tpl(Module,Function,MatchSpec)',
      ' - Same as tpl({Module, Function, \'_\'}, MatchSpec)',
      'tpl(Module, Function, Arity, MatchSpec)',
      ' - Same as tpl({Module, Function, Arity}, MatchSpec)',
      'tpl({Module, Function, Arity}, MatchSpec) -> {ok, MatchDesc} | {error, term()}',
      ' - Set pattern for traced local (as well as global) function calls.'
    ])
  end

  def h(:ctp) do
    help_display([
      'ctp()',
      ' - Same as ctp({\'_\', \'_\', \'_\'})',
      'ctp(Module)',
      ' - Same as ctp({Module, \'_\', \'_\'})',
      'ctp(Module, Function)',
      ' - Same as ctp({Module, Function, \'_\'})',
      'ctp(Module, Function, Arity)',
      ' - Same as ctp({Module, Function, Arity})',
      'ctp({Module, Function, Arity}) -> {ok, MatchDesc} | {error, term()}',
      ' - Clear call trace pattern for the specified functions'
    ])
  end

  def h(:ctpl) do
    help_display([
      'ctpl()',
      ' - Same as ctpl({\'_\', \'_\', \'_\'})',
      'ctpl(Module)',
      ' - Same as ctpl({Module, \'_\', \'_\'})',
      'ctpl(Module, Function)',
      ' - Same as ctpl({Module, Function, \'_\'})',
      'ctpl(Module, Function, Arity)',
      ' - Same as ctpl({Module, Function, Arity})',
      'ctpl({Module, Function, Arity}) -> {ok, MatchDesc} | {error, term()}',
      ' - Clear local call trace pattern for the specified functions'
    ])
  end

  def h(:ctpg) do
    help_display([
      'ctpg()',
      ' - Same as ctpg({\'_\', \'_\', \'_\'})',
      'ctpg(Module)',
      ' - Same as ctpg({Module, \'_\', \'_\'})',
      'ctpg(Module, Function)',
      ' - Same as ctpg({Module, Function, \'_\'})',
      'ctpg(Module, Function, Arity)',
      ' - Same as ctpg({Module, Function, Arity})',
      'ctpg({Module, Function, Arity}) -> {ok, MatchDesc} | {error, term()}',
      ' - Clear global call trace pattern for the specified functions'
    ])
  end

  def h(:ltp) do
    help_display(['ltp() -> ok', ' - Lists saved and built-in match_spec\'s on the console.'])
  end

  def h(:dtp) do
    help_display([
      'dtp() -> ok',
      ' - Deletes all saved match_spec\'s.',
      'dtp(N) -> ok',
      ' - Deletes a specific saved match_spec.'
    ])
  end

  def h(:wtp) do
    help_display([
      'wtp(Name) -> ok | {error, IOError}',
      ' - Writes all saved match_spec\'s to a file'
    ])
  end

  def h(:rtp) do
    help_display([
      'rtp(Name) -> ok | {error, Error}',
      ' - Read saved match specifications from file.'
    ])
  end

  def h(:n) do
    help_display([
      'n(Nodename) -> {ok, Nodename} | {error, Reason}',
      ' - Starts a tracer server on the given node.',
      'n(Nodename,Type,Data) -> {ok, Nodename} | {error, Reason}',
      ' - Starts a tracer server with additional args on the given node.'
    ])
  end

  def h(:cn) do
    help_display(['cn(Nodename) -> ok', ' - Clears a node from the list of traced nodes.'])
  end

  def h(:ln) do
    help_display(['ln() -> ok', ' - Shows the list of traced nodes on the console.'])
  end

  def h(:tracer) do
    help_display([
      'tracer() -> {ok, pid()} | {error, already_started}',
      ' - Starts a tracer server that handles trace messages.',
      'tracer(Type, Data) -> {ok, pid()} | {error, Error}',
      ' - Starts a tracer server with additional parameters'
    ])
  end

  def h(:trace_port) do
    help_display([
      'trace_port(Type, Parameters) -> fun()',
      ' - Creates and returns a trace port generating fun'
    ])
  end

  def h(:trace_client) do
    help_display([
      'trace_client(Type, Parameters) -> pid()',
      ' - Starts a trace client that reads messages created by a trace port driver',
      'trace_client(Type, Parameters, HandlerSpec) -> pid()',
      ' - Starts a trace client that reads messages created by a',
      '   trace port driver, with a user defined handler'
    ])
  end

  def h(:get_tracer) do
    help_display([
      'get_tracer() -> {ok, Tracer}',
      ' - Returns the process or port to which all trace messages are sent.',
      'get_tracer(Node) -> {ok, Tracer}',
      ' - Returns the process or port to which all trace messages are sent.'
    ])
  end

  def h(:stop) do
    help_display([
      'stop() -> ok',
      ' - Stops the dbg server and the tracing of all processes.',
      '   Does not clear any trace patterns.'
    ])
  end

  def h(:stop_clear) do
    help_display([
      'stop_clear() -> ok',
      ' - Stops the dbg server and the tracing of all processes,',
      '   and clears all trace patterns.'
    ])
  end
end
