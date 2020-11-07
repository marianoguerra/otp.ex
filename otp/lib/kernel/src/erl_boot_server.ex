defmodule :m_erl_boot_server do
  use Bitwise
  @behaviour :gen_server
  require Record

  Record.defrecord(:r_state, :state,
    priority: 0,
    version: '',
    udp_sock: :undefined,
    udp_port: :undefined,
    listen_sock: :undefined,
    listen_port: :undefined,
    slaves: :undefined,
    bootp: :undefined,
    prim_state: :undefined
  )

  def start(slaves) do
    case check_arg(slaves) do
      {:ok, aL} ->
        :gen_server.start({:local, :boot_server}, :erl_boot_server, aL, [])

      _ ->
        {:error, {:badarg, slaves}}
    end
  end

  def start_link(slaves) do
    case check_arg(slaves) do
      {:ok, aL} ->
        :gen_server.start_link({:local, :boot_server}, :erl_boot_server, aL, [])

      _ ->
        {:error, {:badarg, slaves}}
    end
  end

  defp check_arg(slaves) do
    check_arg(slaves, [])
  end

  defp check_arg([slave | rest], result) do
    case :inet.getaddr(slave, :inet) do
      {:ok, iP} ->
        check_arg(rest, [{{255, 255, 255, 255}, iP} | result])

      _ ->
        :error
    end
  end

  defp check_arg([], result) do
    {:ok, result}
  end

  defp check_arg(_, _Result) do
    :error
  end

  def add_slave(slave) do
    case :inet.getaddr(slave, :inet) do
      {:ok, iP} ->
        :gen_server.call(
          :boot_server,
          {:add, {{255, 255, 255, 255}, iP}}
        )

      _ ->
        {:error, {:badarg, slave}}
    end
  end

  def delete_slave(slave) do
    case :inet.getaddr(slave, :inet) do
      {:ok, iP} ->
        :gen_server.call(
          :boot_server,
          {:delete, {{255, 255, 255, 255}, iP}}
        )

      _ ->
        {:error, {:badarg, slave}}
    end
  end

  def add_subnet(mask, addr)
      when is_tuple(mask) and
             is_tuple(addr) do
    case member_address(addr, [{mask, addr}]) do
      true ->
        :gen_server.call(:boot_server, {:add, {mask, addr}})

      false ->
        {:error, :empty_subnet}
    end
  end

  def delete_subnet(mask, addr)
      when is_tuple(mask) and
             is_tuple(addr) do
    :gen_server.call(:boot_server, {:delete, {mask, addr}})
  end

  def which_slaves() do
    :gen_server.call(:boot_server, :which)
  end

  def would_be_booted(addr) do
    {:ok, iP} = :inet.getaddr(addr, :inet)
    member_address(iP, which_slaves())
  end

  defp int16(x) when is_integer(x) do
    [x >>> 8 &&& 255, x &&& 255]
  end

  defp member_address(
         iP,
         [{{mA, mB, mC, mD}, {eA, eB, eC, eD}} | rest]
       ) do
    {a, b, c, d} = iP

    cond do
      a &&& mA === eA and b &&& mB === eB and
        c &&& mC === eC and d &&& mD === eD ->
        true

      true ->
        member_address(iP, rest)
    end
  end

  defp member_address(_, []) do
    false
  end

  def init(slaves) do
    {:ok, u} = :gen_udp.open(4368, [])
    {:ok, l} = :gen_tcp.listen(0, [:binary, {:packet, 4}])
    {:ok, port} = :inet.port(l)
    {:ok, uPort} = :inet.port(u)
    ref = make_ref()
    pid = :proc_lib.spawn_link(:erl_boot_server, :boot_init, [ref])
    :ok = :gen_tcp.controlling_process(l, pid)
    send(pid, {ref, l})
    :erlang.process_flag(:trap_exit, true)

    {:ok,
     r_state(
       priority: 0,
       version: :erlang.system_info(:version),
       udp_sock: u,
       udp_port: uPort,
       listen_sock: l,
       listen_port: port,
       slaves: :ordsets.from_list(slaves),
       bootp: pid
     )}
  end

  def handle_call({:add, address}, _, s0) do
    slaves = :ordsets.add_element(address, r_state(s0, :slaves))
    send(r_state(s0, :bootp), {:slaves, slaves})
    {:reply, :ok, r_state(s0, slaves: slaves)}
  end

  def handle_call({:delete, address}, _, s0) do
    slaves = :ordsets.del_element(address, r_state(s0, :slaves))
    send(r_state(s0, :bootp), {:slaves, slaves})
    {:reply, :ok, r_state(s0, slaves: slaves)}
  end

  def handle_call(:which, _, s0) do
    {:reply, :ordsets.to_list(r_state(s0, :slaves)), s0}
  end

  def handle_cast(_, slaves) do
    {:noreply, slaves}
  end

  def handle_info({:udp, u, iP, port, data}, s0) do
    token = 'EBOOTQ' ++ r_state(s0, :version)

    valid =
      member_address(
        iP,
        :ordsets.to_list(r_state(s0, :slaves))
      )

    case {valid, data, token} do
      {true, ^token, ^token} ->
        case :gen_udp.send(u, iP, port, [
               'EBOOTR',
               r_state(s0, :priority),
               int16(r_state(s0, :listen_port)),
               r_state(s0, :version)
             ]) do
          :ok ->
            :ok

          {:error, :not_owner} ->
            :error_logger.error_msg(
              '** Illegal boot server connection attempt: not owner of ~w ** ~n',
              [u]
            )

          {:error, reason} ->
            err = :file.format_error(reason)

            :error_logger.error_msg(
              '** Illegal boot server connection attempt: ~w POSIX error ** ~n',
              [u, err]
            )
        end

        {:noreply, s0}

      {false, _, _} ->
        :error_logger.error_msg(
          '** Illegal boot server connection attempt: ~w is not a valid address ** ~n',
          [iP]
        )

        {:noreply, s0}

      {true, _, _} ->
        case (try do
                :string.slice(data, 0, length('EBOOTQ'))
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          'EBOOTQ' ->
            vsn = :string.slice(data, length('EBOOTQ'), length(data))

            :error_logger.error_msg(
              '** Illegal boot server connection attempt: client version is ~s ** ~n',
              [vsn]
            )

          _ ->
            :error_logger.error_msg(
              '** Illegal boot server connection attempt: unrecognizable request ** ~n',
              []
            )
        end

        {:noreply, s0}
    end
  end

  def handle_info(_Info, s0) do
    {:noreply, s0}
  end

  def terminate(_Reason, _S0) do
    :ok
  end

  def code_change(_Vsn, state, _Extra) do
    {:ok, state}
  end

  def boot_init(tag) do
    receive do
      {^tag, listen} ->
        :erlang.process_flag(:trap_exit, true)
        boot_main(listen)
    end
  end

  defp boot_main(listen) do
    tag = make_ref()
    pid = :proc_lib.spawn_link(:erl_boot_server, :boot_accept, [self(), listen, tag])
    boot_main(listen, tag, pid)
  end

  defp boot_main(listen, tag, pid) do
    receive do
      {^tag, _} ->
        boot_main(listen)

      {:EXIT, ^pid, _} ->
        boot_main(listen)

      {:EXIT, _, reason} ->
        :erlang.exit(pid, :kill)
        exit(reason)

      {:tcp_closed, ^listen} ->
        exit(:closed)
    end
  end

  def boot_accept(server, listen, tag) do
    reply = :gen_tcp.accept(listen)
    :erlang.unlink(server)
    send(server, {tag, :continue})

    case reply do
      {:ok, socket} ->
        {:ok, {iP, _Port}} = :inet.peername(socket)
        true = member_address(iP, which_slaves())
        pS = :erl_prim_loader.prim_init()
        boot_loop(socket, pS)
    end
  end

  defp boot_loop(socket, pS) do
    receive do
      {:tcp, ^socket, data} ->
        pS2 = handle_command(socket, pS, data)
        boot_loop(socket, pS2)

      {:tcp_closed, ^socket} ->
        true
    end
  end

  defp handle_command(s, pS, msg) do
    case (try do
            :erlang.binary_to_term(msg)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:get, file} ->
        {res, pS2} = :erl_prim_loader.prim_get_file(pS, file)
        send_file_result(s, :get, res)
        pS2

      {:list_dir, dir} ->
        {res, pS2} = :erl_prim_loader.prim_list_dir(pS, dir)
        send_file_result(s, :list_dir, res)
        pS2

      {:read_file_info, file} ->
        {res, pS2} = :erl_prim_loader.prim_read_file_info(pS, file, true)
        send_file_result(s, :read_file_info, res)
        pS2

      {:read_link_info, file} ->
        {res, pS2} = :erl_prim_loader.prim_read_file_info(pS, file, false)
        send_file_result(s, :read_link_info, res)
        pS2

      :get_cwd ->
        {res, pS2} = :erl_prim_loader.prim_get_cwd(pS, [])
        send_file_result(s, :get_cwd, res)
        pS2

      {:get_cwd, drive} ->
        {res, pS2} = :erl_prim_loader.prim_get_cwd(pS, [drive])
        send_file_result(s, :get_cwd, res)
        pS2

      {:EXIT, reason} ->
        send_result(s, {:error, reason})
        pS

      _Other ->
        send_result(s, {:error, :unknown_command})
        pS
    end
  end

  defp send_file_result(s, cmd, result) do
    send_result(s, {cmd, result})
  end

  defp send_result(s, term) do
    case :gen_tcp.send(s, :erlang.term_to_binary(term)) do
      :ok ->
        :ok

      error ->
        :error_logger.error_msg('** Boot server could not send result to socket: ~w** ~n', [error])

        :ok
    end
  end
end
