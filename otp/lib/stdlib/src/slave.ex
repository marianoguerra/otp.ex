defmodule :m_slave do
  use Bitwise
  import :error_logger, only: [error_msg: 2]

  def pseudo([master | serverList]) do
    pseudo(master, serverList)
  end

  def pseudo(_) do
    error_msg('No master node given to slave:pseudo/1~n', [])
  end

  def pseudo(_, []) do
    :ok
  end

  def pseudo(master, [s | tail]) do
    start_pseudo(s, :erlang.whereis(s), master)
    pseudo(master, tail)
  end

  defp start_pseudo(name, :undefined, master) do
    x = :rpc.call(master, :erlang, :whereis, [name])
    :erlang.register(name, spawn(:slave, :relay, [x]))
  end

  defp start_pseudo(_, _, _) do
    :ok
  end

  def relay({:badrpc, reason}) do
    error_msg(' ** exiting relay server ~w :~tw  **~n', [self(), reason])
    exit(reason)
  end

  def relay(:undefined) do
    error_msg(' ** exiting relay server ~w  **~n', [self()])
    exit(:undefined)
  end

  def relay(pid) when is_pid(pid) do
    relay1(pid)
  end

  defp relay1(pid) do
    receive do
      x ->
        send(pid, x)
    end

    relay1(pid)
  end

  def start(host) do
    l = :erlang.atom_to_list(node())
    name = upto(?@, l)
    start(host, name, [], :no_link)
  end

  def start(host, name) do
    start(host, name, [])
  end

  def start(host, name, args) do
    start(host, name, args, :no_link)
  end

  def start_link(host) do
    l = :erlang.atom_to_list(node())
    name = upto(?@, l)
    start(host, name, [], self())
  end

  def start_link(host, name) do
    start_link(host, name, [])
  end

  def start_link(host, name, args) do
    start(host, name, args, self())
  end

  defp start(host0, name, args, linkTo) do
    prog = progname()
    start(host0, name, args, linkTo, prog)
  end

  def start(host0, name, args, linkTo, prog) do
    host =
      case :net_kernel.longnames() do
        true ->
          dns(host0)

        false ->
          strip_host_name(to_list(host0))

        :ignored ->
          exit(:not_alive)
      end

    node = :erlang.list_to_atom(:lists.concat([name, '@', host]))

    case :net_adm.ping(node) do
      :pang ->
        start_it(host, name, node, args, linkTo, prog)

      :pong ->
        {:error, {:already_running, node}}
    end
  end

  def stop(node) do
    :rpc.call(node, :erlang, :halt, [])
    :ok
  end

  defp start_it(host, name, node, args, linkTo, prog) do
    spawn(:slave, :wait_for_slave, [self(), host, name, node, args, linkTo, prog])

    receive do
      {:result, result} ->
        result
    end
  end

  def wait_for_slave(parent, host, name, node, args, linkTo, prog) do
    waiter = register_unique_name(0)

    case mk_cmd(host, name, args, waiter, prog) do
      {:ok, cmd} ->
        :erlang.open_port({:spawn, cmd}, [:stream])

        receive do
          {slavePid, :slave_started} ->
            :erlang.unregister(waiter)
            slave_started(parent, linkTo, slavePid)
        after
          32000 ->
            ^node = :erlang.list_to_atom(:lists.concat([name, '@', host]))

            case :net_adm.ping(node) do
              :pong ->
                :erlang.spawn(node, :erlang, :halt, [])
                :ok

              _ ->
                :ok
            end

            send(parent, {:result, {:error, :timeout}})
        end

      other ->
        send(parent, {:result, other})
    end
  end

  defp slave_started(replyTo, :no_link, slave) when is_pid(slave) do
    send(replyTo, {:result, {:ok, node(slave)}})
  end

  defp slave_started(replyTo, master, slave)
       when is_pid(master) and
              is_pid(slave) do
    :erlang.process_flag(:trap_exit, true)
    :erlang.link(master)
    :erlang.link(slave)
    send(replyTo, {:result, {:ok, node(slave)}})
    one_way_link(master, slave)
  end

  defp one_way_link(master, slave) do
    receive do
      {:EXIT, ^master, _Reason} ->
        :erlang.unlink(slave)
        send(slave, {:nodedown, node()})

      {:EXIT, ^slave, _Reason} ->
        :erlang.unlink(master)

      _Other ->
        one_way_link(master, slave)
    end
  end

  defp register_unique_name(number) do
    name = :erlang.list_to_atom(:lists.concat(['slave_waiter_', number]))

    case (try do
            :erlang.register(name, self())
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      true ->
        name

      {:EXIT, {:badarg, _}} ->
        register_unique_name(number + 1)
    end
  end

  defp mk_cmd(host, name, args, waiter, prog0) do
    prog = quote_progname(prog0)

    basicCmd =
      :lists.concat([
        prog,
        ' -detached -noinput -master ',
        node(),
        ' ',
        long_or_short(),
        name,
        '@',
        host,
        ' -s slave slave_start ',
        node(),
        ' ',
        waiter,
        ' ',
        args
      ])

    case after_char(?@, :erlang.atom_to_list(node())) do
      ^host ->
        {:ok, basicCmd}

      _ ->
        case rsh() do
          {:ok, rsh} ->
            {:ok, :lists.concat([rsh, ' ', host, ' ', basicCmd])}

          other ->
            other
        end
    end
  end

  defp progname() do
    case :init.get_argument(:progname) do
      {:ok, [[prog]]} ->
        prog

      _Other ->
        'no_prog_name'
    end
  end

  defp quote_progname(progname) do
    do_quote_progname(:string.lexemes(to_list(progname), ' '))
  end

  defp do_quote_progname([prog]) do
    '"' ++ prog ++ '"'
  end

  defp do_quote_progname([[prog, arg] | args]) do
    case :os.find_executable(prog) do
      false ->
        do_quote_progname([prog ++ ' ' ++ arg | args])

      _ ->
        '"' ++
          prog ++
          '"' ++
          :lists.flatten(
            :lists.map(
              fn x ->
                [' ', x]
              end,
              [arg | args]
            )
          )
    end
  end

  defp rsh() do
    rsh =
      case :init.get_argument(:rsh) do
        {:ok, [[prog]]} ->
          prog

        _ ->
          'ssh'
      end

    case :os.find_executable(rsh) do
      false ->
        {:error, :no_rsh}

      path ->
        {:ok, path}
    end
  end

  defp long_or_short() do
    case :net_kernel.longnames() do
      true ->
        ' -name '

      false ->
        ' -sname '
    end
  end

  def slave_start([master, waiter]) do
    true
    spawn(:slave, :wait_for_master_to_die, [master, waiter])
  end

  def wait_for_master_to_die(master, waiter) do
    true
    :erlang.process_flag(:trap_exit, true)
    :erlang.monitor_node(master, true)
    send({waiter, master}, {self(), :slave_started})
    wloop(master)
  end

  defp wloop(master) do
    receive do
      {:nodedown, ^master} ->
        true
        :erlang.halt()

      _Other ->
        wloop(master)
    end
  end

  defp strip_host_name([]) do
    []
  end

  defp strip_host_name([?. | _]) do
    []
  end

  defp strip_host_name([h | t]) do
    [h | strip_host_name(t)]
  end

  defp dns(h) do
    {:ok, host} = :net_adm.dns_hostname(h)
    host
  end

  defp to_list(x) when is_list(x) do
    x
  end

  defp to_list(x) when is_atom(x) do
    :erlang.atom_to_list(x)
  end

  defp upto(_, []) do
    []
  end

  defp upto(char, [char | _]) do
    []
  end

  defp upto(char, [h | t]) do
    [h | upto(char, t)]
  end

  defp after_char(_, []) do
    []
  end

  defp after_char(char, [char | rest]) do
    rest
  end

  defp after_char(char, [_ | rest]) do
    after_char(char, rest)
  end
end
