defmodule :m_ct_slave do
  use Bitwise
  require Record

  Record.defrecord(:r_options, :options,
    username: :undefined,
    password: :undefined,
    boot_timeout: :undefined,
    init_timeout: :undefined,
    startup_timeout: :undefined,
    startup_functions: :undefined,
    monitor_master: :undefined,
    kill_if_fail: :undefined,
    erl_flags: :undefined,
    env: :undefined,
    ssh_port: :undefined,
    ssh_opts: :undefined,
    stop_timeout: :undefined
  )

  def start(node) do
    start(gethostname(), node)
  end

  def start(_HostOrNode = node, _NodeOrOpts = opts)
      when is_list(opts) do
    start(gethostname(), node, opts)
  end

  def start(host, node) do
    start(host, node, [])
  end

  def start(host, node, opts) do
    eNode = enodename(host, node)

    case :erlang.is_alive() do
      false ->
        {:error, :not_alive, node()}

      true ->
        case is_started(eNode) do
          false ->
            optionsRec = fetch_options(opts)
            do_start(host, node, optionsRec)

          {true, :not_connected} ->
            {:error, :started_not_connected, eNode}

          {true, :connected} ->
            {:error, :already_started, eNode}
        end
    end
  end

  def stop(node) do
    stop(gethostname(), node)
  end

  def stop(_HostOrNode = node, _NodeOrOpts = opts)
      when is_list(opts) do
    stop(gethostname(), node, opts)
  end

  def stop(host, node) do
    stop(host, node, [])
  end

  defp stop(host, node, opts) do
    eNode = enodename(host, node)

    case is_started(eNode) do
      {true, :connected} ->
        optionsRec = fetch_options(opts)
        do_stop(eNode, optionsRec)

      {true, :not_connected} ->
        {:error, :not_connected, eNode}

      false ->
        {:error, :not_started, eNode}
    end
  end

  defp get_option_value(key, optionList, default) do
    case :lists.keyfind(key, 1, optionList) do
      false ->
        default

      {^key, value} ->
        value
    end
  end

  defp fetch_options(options) do
    userName = get_option_value(:username, options, [])
    password = get_option_value(:password, options, [])
    bootTimeout = get_option_value(:boot_timeout, options, 3)
    initTimeout = get_option_value(:init_timeout, options, 1)
    startupTimeout = get_option_value(:startup_timeout, options, 1)
    startupFunctions = get_option_value(:startup_functions, options, [])
    monitor = get_option_value(:monitor_master, options, false)
    killIfFail = get_option_value(:kill_if_fail, options, true)
    erlFlags = get_option_value(:erl_flags, options, [])
    envVars = get_option_value(:env, options, [])
    sSHPort = get_option_value(:ssh_port, options, [])
    sSHOpts = get_option_value(:ssh_opts, options, [])
    stopTimeout = get_option_value(:stop_timeout, options, 5)

    r_options(
      username: userName,
      password: password,
      boot_timeout: bootTimeout,
      init_timeout: initTimeout,
      startup_timeout: startupTimeout,
      startup_functions: startupFunctions,
      monitor_master: monitor,
      kill_if_fail: killIfFail,
      erl_flags: erlFlags,
      env: envVars,
      ssh_port: sSHPort,
      ssh_opts: sSHOpts,
      stop_timeout: stopTimeout
    )
  end

  def slave_started(eNode, masterPid) do
    send(masterPid, {:node_started, eNode})
    :ok
  end

  def slave_ready(eNode, masterPid) do
    send(masterPid, {:node_ready, eNode})
    :ok
  end

  def monitor_master(masterNode) do
    spawn(fn ->
      monitor_master_int(masterNode)
    end)
  end

  defp monitor_master_int(masterNode) do
    :ct_util.mark_process()
    :erlang.monitor_node(masterNode, true)

    receive do
      {:nodedown, ^masterNode} ->
        :init.stop()
    end
  end

  defp is_connected(eNode) do
    for n <- :erlang.nodes(), n == eNode do
      n
    end == [eNode]
  end

  defp is_started(eNode) do
    case is_connected(eNode) do
      true ->
        {true, :connected}

      false ->
        case :net_adm.ping(eNode) do
          :pang ->
            false

          :pong ->
            :erlang.disconnect_node(eNode)
            {true, :not_connected}
        end
    end
  end

  defp enodename(host, node) do
    case :lists.member(?@, :erlang.atom_to_list(node)) do
      true ->
        node

      false ->
        :erlang.list_to_atom(:erlang.atom_to_list(node) ++ '@' ++ :erlang.atom_to_list(host))
    end
  end

  defp do_start(host, node, options) do
    eNode = enodename(host, node)

    functions =
      :lists.append([
        [{:ct_slave, :slave_started, [eNode, self()]}],
        r_options(options, :startup_functions),
        [{:ct_slave, :slave_ready, [eNode, self()]}]
      ])

    functions2 =
      cond do
        r_options(options, :monitor_master) ->
          [{:ct_slave, :monitor_master, [node()]} | functions]

        true ->
          functions
      end

    masterHost = gethostname()

    _ =
      cond do
        masterHost == host ->
          spawn_local_node(node, options)

        true ->
          spawn_remote_node(host, node, options)
      end

    bootTimeout = r_options(options, :boot_timeout)
    initTimeout = r_options(options, :init_timeout)
    startupTimeout = r_options(options, :startup_timeout)

    result =
      case wait_for_node_alive(
             eNode,
             bootTimeout
           ) do
        :pong ->
          case :test_server.is_cover() do
            true ->
              mainCoverNode = :cover.get_main_node()
              :rpc.call(mainCoverNode, :cover, :start, [eNode])

            false ->
              :ok
          end

          call_functions(eNode, functions2)

          receive do
            {:node_started, ^eNode} ->
              receive do
                {:node_ready, ^eNode} ->
                  {:ok, eNode}
              after
                startupTimeout * 1000 ->
                  {:error, :startup_timeout, eNode}
              end
          after
            initTimeout * 1000 ->
              {:error, :init_timeout, eNode}
          end

        :pang ->
          {:error, :boot_timeout, eNode}
      end

    _ =
      case result do
        {:ok, ^eNode} ->
          :ok

        {:error, timeout, ^eNode}
        when (timeout == :init_timeout or timeout == :startup_timeout) and
               r_options(options, :kill_if_fail) ->
          do_stop(eNode)

        _ ->
          :ok
      end

    result
  end

  defp long_or_short() do
    case :net_kernel.longnames() do
      true ->
        ' -name '

      false ->
        ' -sname '
    end
  end

  defp gethostname() do
    hostname =
      case :net_kernel.longnames() do
        true ->
          :net_adm.localhost()

        _ ->
          {:ok, name} = :inet.gethostname()
          name
      end

    :erlang.list_to_atom(hostname)
  end

  defp get_cmd(node, flags) do
    cookie = :erlang.get_cookie()

    'erl -detached -noinput -setcookie ' ++
      :erlang.atom_to_list(cookie) ++
      long_or_short() ++ :erlang.atom_to_list(node) ++ ' ' ++ flags
  end

  defp spawn_local_node(node, options) do
    r_options(env: env, erl_flags: erlFlags) = options
    cmd = get_cmd(node, erlFlags)
    :erlang.open_port({:spawn, cmd}, [:stream, {:env, env}])
  end

  defp spawn_remote_node(host, node, options) do
    r_options(
      username: username,
      password: password,
      erl_flags: erlFlags,
      env: env,
      ssh_port: maybeSSHPort,
      ssh_opts: sSHOpts
    ) = options

    sSHPort =
      case maybeSSHPort do
        [] ->
          22

        a ->
          a
      end

    sSHOptions =
      case {username, password} do
        {[], []} ->
          []

        {_, []} ->
          [{:user, username}]

        {_, _} ->
          [{:user, username}, {:password, password}]
      end ++ [{:silently_accept_hosts, true}] ++ sSHOpts

    {:ok, _} = :application.ensure_all_started(:ssh)
    {:ok, sSHConnRef} = :ssh.connect(:erlang.atom_to_list(host), sSHPort, sSHOptions)

    {:ok, sSHChannelId} =
      :ssh_connection.session_channel(
        sSHConnRef,
        :infinity
      )

    ssh_setenv(sSHConnRef, sSHChannelId, env)
    :ssh_connection.exec(sSHConnRef, sSHChannelId, get_cmd(node, erlFlags), :infinity)
  end

  defp ssh_setenv(sSHConnRef, sSHChannelId, [{var, value} | vars])
       when is_list(var) and is_list(value) do
    :success = :ssh_connection.setenv(sSHConnRef, sSHChannelId, var, value, :infinity)
    ssh_setenv(sSHConnRef, sSHChannelId, vars)
  end

  defp ssh_setenv(_SSHConnRef, _SSHChannelId, []) do
    :ok
  end

  defp call_functions(_Node, []) do
    :ok
  end

  defp call_functions(node, [{m, f, a} | functions]) do
    :rpc.call(node, m, f, a)
    call_functions(node, functions)
  end

  defp wait_for_node_alive(_Node, 0) do
    :pang
  end

  defp wait_for_node_alive(node, n) do
    :timer.sleep(1000)

    case :net_adm.ping(node) do
      :pong ->
        :pong

      :pang ->
        wait_for_node_alive(node, n - 1)
    end
  end

  defp do_stop(eNode) do
    do_stop(eNode, fetch_options([]))
  end

  defp do_stop(eNode, options) do
    {cover, mainCoverNode} =
      case :test_server.is_cover() do
        true ->
          main = :cover.get_main_node()
          :rpc.call(main, :cover, :flush, [eNode])
          {true, main}

        false ->
          {false, :undefined}
      end

    :erlang.spawn(eNode, :init, :stop, [])
    stopTimeout = r_options(options, :stop_timeout)

    case wait_for_node_dead(eNode, stopTimeout) do
      {:ok, ^eNode} ->
        cond do
          cover ->
            :rpc.call(mainCoverNode, :cover, :stop, [eNode])

          true ->
            :ok
        end

        {:ok, eNode}

      error ->
        error
    end
  end

  defp wait_for_node_dead(node, 0) do
    {:error, :stop_timeout, node}
  end

  defp wait_for_node_dead(node, n) do
    :timer.sleep(1000)

    case :lists.member(node, :erlang.nodes()) do
      true ->
        wait_for_node_dead(node, n - 1)

      false ->
        {:ok, node}
    end
  end
end
