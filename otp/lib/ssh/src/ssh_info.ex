defmodule :m_ssh_info do
  use Bitwise
  require Record

  Record.defrecord(:r_ssh_msg_global_request, :ssh_msg_global_request,
    name: :undefined,
    want_reply: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_request_success, :ssh_msg_request_success, data: :undefined)
  Record.defrecord(:r_ssh_msg_request_failure, :ssh_msg_request_failure, [])

  Record.defrecord(:r_ssh_msg_channel_open, :ssh_msg_channel_open,
    channel_type: :undefined,
    sender_channel: :undefined,
    initial_window_size: :undefined,
    maximum_packet_size: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_open_confirmation, :ssh_msg_channel_open_confirmation,
    recipient_channel: :undefined,
    sender_channel: :undefined,
    initial_window_size: :undefined,
    maximum_packet_size: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_open_failure, :ssh_msg_channel_open_failure,
    recipient_channel: :undefined,
    reason: :undefined,
    description: :undefined,
    lang: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_window_adjust, :ssh_msg_channel_window_adjust,
    recipient_channel: :undefined,
    bytes_to_add: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_data, :ssh_msg_channel_data,
    recipient_channel: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_extended_data, :ssh_msg_channel_extended_data,
    recipient_channel: :undefined,
    data_type_code: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_eof, :ssh_msg_channel_eof, recipient_channel: :undefined)

  Record.defrecord(:r_ssh_msg_channel_close, :ssh_msg_channel_close, recipient_channel: :undefined)

  Record.defrecord(:r_ssh_msg_channel_request, :ssh_msg_channel_request,
    recipient_channel: :undefined,
    request_type: :undefined,
    want_reply: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_success, :ssh_msg_channel_success,
    recipient_channel: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_failure, :ssh_msg_channel_failure,
    recipient_channel: :undefined
  )

  Record.defrecord(:r_channel, :channel,
    type: :undefined,
    sys: :undefined,
    user: :undefined,
    flow_control: :undefined,
    local_id: :undefined,
    recv_window_size: :undefined,
    recv_window_pending: 0,
    recv_packet_size: :undefined,
    recv_close: false,
    remote_id: :undefined,
    send_window_size: :undefined,
    send_packet_size: :undefined,
    sent_close: false,
    send_buf: []
  )

  Record.defrecord(:r_connection, :connection,
    requests: [],
    channel_cache: :undefined,
    channel_id_seed: :undefined,
    cli_spec: :undefined,
    options: :undefined,
    exec: :undefined,
    system_supervisor: :undefined,
    sub_system_supervisor: :undefined,
    connection_supervisor: :undefined
  )

  def print() do
    :io.format('~s', [string()])
  end

  def print(file) when is_list(file) do
    {:ok, d} = :file.open(file, [:write])
    print(d)
    :file.close(d)
  end

  def print(d) do
    :io.format(d, '~s', [string()])
  end

  def string() do
    try do
      :supervisor.which_children(:ssh_sup)
    catch
      _, _ ->
        :io_lib.format('Ssh not found~n', [])
    else
      _ ->
        [
          :io_lib.nl(),
          print_general(),
          :io_lib.nl(),
          underline('Client part', ?=),
          print_clients(),
          :io_lib.nl(),
          underline('Server part', ?=),
          print_servers(),
          :io_lib.nl(),
          underline('Supervisors', ?=),
          walk_sups(:ssh_sup),
          :io_lib.nl()
        ]
    end
  end

  defp print_general() do
    {_Name, slogan, ver} = :lists.keyfind(:ssh, 1, :application.which_applications())

    [
      underline(:io_lib.format('~s  ~s', [slogan, ver]), ?=),
      :io_lib.format(:"This printout is generated ~s. ~n", [datetime()])
    ]
  end

  defp print_clients() do
    try do
      :lists.map(
        &print_client/1,
        :supervisor.which_children(:sshc_sup)
      )
    catch
      c, e ->
        :io_lib.format(:"***print_clients FAILED: ~p:~p,~n ~p~n", [c, e, __STACKTRACE__])
    end
  end

  defp print_client({:undefined, pid, :supervisor, [:ssh_connection_handler]}) do
    {{local, remote}, _Str} = :ssh_connection_handler.get_print_info(pid)

    [
      :io_lib.format(
        '    Local: ~s  Remote: ~s  ConnectionRef = ~p~n',
        [fmt_host_port(local), fmt_host_port(remote), pid]
      ),
      case channels(pid) do
        {:ok, channels = [_ | _]} ->
          for r_channel(user: chPid) <- channels do
            print_ch(chPid)
          end

        _ ->
          :io_lib.format('            No channels~n', [])
      end
    ]
  end

  defp print_client({{:client, :ssh_system_sup, _, _, _}, pid, :supervisor, [:ssh_system_sup]})
       when is_pid(pid) do
    :lists.map(
      &print_system_sup/1,
      :supervisor.which_children(pid)
    )
  end

  defp print_servers() do
    try do
      :lists.map(
        &print_server/1,
        :supervisor.which_children(:sshd_sup)
      )
    catch
      c, e ->
        :io_lib.format(:"***print_servers FAILED: ~p:~p,~n ~p~n", [c, e, __STACKTRACE__])
    end
  end

  defp print_server(
         {{:server, :ssh_system_sup, localHost, localPort, profile}, pid, :supervisor,
          [:ssh_system_sup]}
       )
       when is_pid(pid) do
    children = :supervisor.which_children(pid)

    [
      :io_lib.format(
        '    Listen: ~s (~p children) Profile ~p',
        [fmt_host_port({localHost, localPort}), :ssh_acceptor.number_of_connections(pid), profile]
      ),
      case (for {{:ssh_acceptor_sup, _LocalHost, _LocalPort, _Profile}, accPid, :supervisor,
                 [:ssh_acceptor_sup]} <- children do
              accPid
            end) do
        acceptorPids = [_ | _] ->
          [
            :io_lib.format('  [Acceptor Pid', []),
            for accPid <- acceptorPids do
              :io_lib.format(' ~p', [accPid])
            end,
            :io_lib.format(']~n', [])
          ]

        [] ->
          :io_lib.nl()
      end,
      :lists.map(
        &print_system_sup/1,
        :supervisor.which_children(pid)
      )
    ]
  end

  defp print_system_sup({ref, pid, :supervisor, [:ssh_subsystem_sup]})
       when is_reference(ref) and is_pid(pid) do
    :lists.map(
      &print_channels/1,
      :supervisor.which_children(pid)
    )
  end

  defp print_system_sup(
         {{:ssh_acceptor_sup, _LocalHost, _LocalPort, _Profile}, pid, :supervisor,
          [:ssh_acceptor_sup]}
       )
       when is_pid(pid) do
    []
  end

  defp print_channels({{role, :ssh_channel_sup, _, _}, pid, :supervisor, [:ssh_channel_sup]})
       when is_pid(pid) do
    chanBehaviour =
      case role do
        :server ->
          :ssh_server_channel

        :client ->
          :ssh_client_channel
      end

    children = :supervisor.which_children(pid)

    channelPids =
      for {r, p, :worker, [mod]} <- children, chanBehaviour == mod, is_pid(p), is_reference(r) do
        p
      end

    case channelPids do
      [] ->
        :io_lib.format('        No channels~n', [])

      [ch1Pid | _] ->
        {{connManager, _}, _Str} = chanBehaviour.get_print_info(ch1Pid)
        {{_, remote}, _} = :ssh_connection_handler.get_print_info(connManager)

        [
          :io_lib.format(
            '        Remote: ~s ConnectionRef = ~p~n',
            [fmt_host_port(remote), connManager]
          ),
          :lists.map(&print_ch/1, channelPids)
        ]
    end
  end

  defp print_channels(
         {{_Role, :ssh_connection_sup, _, _}, pid, :supervisor, [:ssh_connection_sup]}
       )
       when is_pid(pid) do
    []
  end

  defp print_channels({ref, pid, :supervisor, [:ssh_tcpip_forward_acceptor_sup]})
       when is_pid(pid) and is_reference(ref) do
    []
  end

  defp print_ch(pid) do
    try do
      {{connManager, channelID}, str} = :ssh_server_channel.get_print_info(pid)
      {_LocalRemote, strM} = :ssh_connection_handler.get_print_info(connManager)
      :io_lib.format('            ch ~p ~p: ~s ~s~n', [channelID, pid, strM, str])
    catch
      c, e ->
        :io_lib.format(:"****print_ch FAILED for ChanPid ~p: ~p:~p~n", [pid, c, e])
    end
  end

  defp walk_sups(startPid) do
    :io_lib.format('Start at ~p, ~s.~n', [startPid, dead_or_alive(startPid)])
    walk_sups(children(startPid), _Indent = 0 + 4)
  end

  defp walk_sups([h = {_, pid, _, _} | t], indent) do
    [
      indent(indent),
      :io_lib.format(:"~200p  ~p is ~s~n", [h, pid, dead_or_alive(pid)]),
      case h do
        {_, _, :supervisor, [:ssh_connection_handler]} ->
          ''

        {_, ^pid, :supervisor, _} ->
          walk_sups(children(pid), indent + 4)

        _ ->
          ''
      end,
      walk_sups(t, indent)
    ]
  end

  defp walk_sups([], _) do
    ''
  end

  defp dead_or_alive(name) when is_atom(name) do
    case :erlang.whereis(name) do
      :undefined ->
        '**UNDEFINED**'

      pid ->
        dead_or_alive(pid)
    end
  end

  defp dead_or_alive(pid) when is_pid(pid) do
    case :erlang.process_info(pid) do
      :undefined ->
        '**DEAD**'

      _ ->
        'alive'
    end
  end

  defp indent(i) do
    :io_lib.format(:"~*c", [i, ?\s])
  end

  defp children(pid) do
    parent = self()

    helper =
      spawn(fn ->
        send(parent, {self(), :supervisor.which_children(pid)})
      end)

    receive do
      {^helper, l} when is_list(l) ->
        l
    after
      2000 ->
        try do
          :erlang.exit(helper, :kill)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

        []
    end
  end

  defp is_connection_handler(pid) do
    try do
      {:ssh_connection_handler, :init, _} =
        :proplists.get_value(
          :"$initial_call",
          :proplists.get_value(
            :dictionary,
            :erlang.process_info(
              pid,
              [:dictionary]
            )
          )
        )
    catch
      _, _ ->
        false
    else
      _ ->
        true
    end
  end

  defp channels(pid) do
    case is_connection_handler(pid) do
      true ->
        :ssh_connection_handler.info(pid, :all)

      false ->
        false
    end
  end

  defp underline(str, lineChar) do
    :io_lib.format(
      :"~s~n~*c~n",
      [str, :lists.flatlength(str), lineChar]
    )
  end

  defp datetime() do
    {{yYYY, mM, dD}, {h, m, s}} = :calendar.now_to_universal_time(:erlang.timestamp())

    :lists.flatten(
      :io_lib.format(
        :"~4w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w UTC",
        [yYYY, mM, dD, h, m, s]
      )
    )
  end

  defp fmt_host_port({{a, b, c, d}, port}) do
    :io_lib.format(:"~p.~p.~p.~p:~p", [a, b, c, d, port])
  end

  defp fmt_host_port({host, port}) do
    :io_lib.format(:"~s:~p", [host, port])
  end

  def collect_pids() do
    collect_pids(:ssh_sup)
  end

  defp collect_pids(p) do
    collector = pcollect_pids(p, spawn(&init_collector/0))
    send(collector, {:get_values, self()})

    receive do
      {:values, values} ->
        values
    end
  end

  defp pcollect_pids(:undefined, collector) do
    collector
  end

  defp pcollect_pids(a, collector) when is_atom(a) do
    pcollect_pids(:erlang.whereis(a), collector)
  end

  defp pcollect_pids(pid, collector) when is_pid(pid) do
    send(collector, {:expect, pid})

    spawn(fn ->
      :lists.foreach(
        fn p2 ->
          pcollect_pids(p2, collector)
        end,
        children(pid)
      )

      send(collector, {:value, pid, pid})
    end)

    collector
  end

  defp pcollect_pids({ref, pid, :supervisor, _}, collector)
       when is_pid(pid) and is_reference(ref) do
    pcollect_pids(pid, collector)
  end

  defp pcollect_pids({:sshc_sup, pid, :supervisor, _}, collector)
       when is_pid(pid) do
    pcollect_pids(pid, collector)
  end

  defp pcollect_pids({:sshd_sup, pid, :supervisor, _}, collector)
       when is_pid(pid) do
    pcollect_pids(pid, collector)
  end

  defp pcollect_pids(
         {{:ssh_acceptor_sup, _, _, _}, pid, :supervisor, _},
         collector
       )
       when is_pid(pid) do
    pcollect_pids(pid, collector)
  end

  defp pcollect_pids(
         {{:server, _, _, _}, pid, :supervisor, _},
         collector
       )
       when is_pid(pid) do
    pcollect_pids(pid, collector)
  end

  defp pcollect_pids(
         {{:server, _, _, _, _}, pid, :supervisor, _},
         collector
       )
       when is_pid(pid) do
    pcollect_pids(pid, collector)
  end

  defp pcollect_pids(
         {:undefined, pid, :supervisor, [:ssh_connection_handler]},
         collector
       ) do
    send(collector, {:value, pid, pid})

    case channels(pid) do
      {:ok, l} ->
        for r_channel(user: p) <- l do
          send(collector, {:value, p, p})
        end

      _ ->
        :ok
    end

    collector
  end

  defp pcollect_pids({_, pid, _, _}, collector) when is_pid(pid) do
    send(collector, {:value, pid, pid})
    collector
  end

  defp pcollect_pids(_, collector) do
    collector
  end

  defp init_collector() do
    loop_collector([], [])
  end

  defp loop_collector(expects, values) do
    receive do
      {:expect, ref} ->
        loop_collector([ref | expects], values)

      {:value, ref, val} ->
        loop_collector(expects -- [ref], [val | values])

      {:get_values, from} when expects == [] ->
        send(from, {:values, values})
    end
  end
end
