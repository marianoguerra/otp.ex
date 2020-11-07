defmodule :m_cpu_sup do
  use Bitwise
  require Record
  Record.defrecord(:r_cpu_util, :cpu_util, cpu: :undefined, busy: [], non_busy: [])

  Record.defrecord(:r_state, :state,
    server: :undefined,
    os_type: :undefined
  )

  Record.defrecord(:r_internal, :internal, port: :not_used, util: [], os_type: :undefined)

  def start() do
    :gen_server.start({:local, :cpu_sup}, :cpu_sup, [], [])
  end

  def start_link() do
    :gen_server.start_link({:local, :cpu_sup}, :cpu_sup, [], [])
  end

  def stop() do
    :gen_server.call(:cpu_sup, 'q', :infinity)
  end

  def nprocs() do
    :os_mon.call(:cpu_sup, 'n', :infinity)
  end

  def avg1() do
    :os_mon.call(:cpu_sup, '1', :infinity)
  end

  def avg5() do
    :os_mon.call(:cpu_sup, '5', :infinity)
  end

  def avg15() do
    :os_mon.call(:cpu_sup, 'f', :infinity)
  end

  def util(args) when is_list(args) do
    case :lists.foldl(
           fn
             :detailed, {_, pC} ->
               {true, pC}

             :per_cpu, {d, _} ->
               {d, true}

             _, _ ->
               :badarg
           end,
           {false, false},
           args
         ) do
      :badarg ->
        :erlang.error(:badarg)

      {detailed, perCpu} ->
        :os_mon.call(:cpu_sup, {'u', detailed, perCpu}, :infinity)
    end
  end

  def util(_) do
    :erlang.error(:badarg)
  end

  def util() do
    case util([]) do
      {:all, busy, _, _} ->
        busy

      error ->
        error
    end
  end

  def dummy_reply('n') do
    0
  end

  def dummy_reply('1') do
    0
  end

  def dummy_reply('5') do
    0
  end

  def dummy_reply('f') do
    0
  end

  def dummy_reply({'u', _, _}) do
    {:all, 0, 0, []}
  end

  def ping() do
    :gen_server.call(:cpu_sup, 'p')
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)
    :erlang.process_flag(:priority, :low)

    {:ok,
     r_state(
       os_type: :os.type(),
       server: measurement_server_start()
     )}
  end

  def handle_call('q', _From, state) do
    {:stop, :normal, :ok, state}
  end

  def handle_call({'u', d, pC}, {client, _Tag}, r_state(os_type: {:unix, flavor}) = state)
      when flavor == :sunos or flavor == :linux or
             flavor == :freebsd or flavor == :darwin do
    case measurement_server_call(
           r_state(state, :server),
           {'u', d, pC, client}
         ) do
      {:error, reason} ->
        {:reply, {:error, reason},
         r_state(state, server: measurement_server_restart(r_state(state, :server)))}

      result ->
        {:reply, result, state}
    end
  end

  def handle_call({'u', detailed, perCpu}, _From, state) do
    string = 'OS_MON (cpu_sup), util/1 unavailable for this OS~n'
    :error_logger.warning_msg(string)
    {:reply, dummy_reply({'u', detailed, perCpu}), state}
  end

  def handle_call(request, _From, state)
      when request == 'n' or
             request == '1' or request == '5' or
             request == 'f' or request == 'p' do
    case measurement_server_call(
           r_state(state, :server),
           request
         ) do
      {:error, reason} ->
        {:reply, {:error, reason},
         r_state(state, server: measurement_server_restart(r_state(state, :server)))}

      result ->
        {:reply, result, state}
    end
  end

  def handle_cast(_Msg, state) do
    {:noreply, state}
  end

  def handle_info({:EXIT, _Port, reason}, state) do
    {:stop, {:server_died, reason}, r_state(state, server: :not_used)}
  end

  def handle_info(_Info, state) do
    {:noreply, state}
  end

  def terminate(_Reason, state) do
    :erlang.exit(r_state(state, :server), :normal)
  end

  defp get_uint32_measurement(
         request,
         r_internal(port: p, os_type: {:unix, :linux})
       ) do
    case :file.open('/proc/loadavg', [:read, :raw]) do
      {:ok, f} ->
        {:ok, d} = :file.read_line(f)
        :ok = :file.close(f)
        {:ok, [load1, load5, load15, _PRun, pTotal], _} = :io_lib.fread('~f ~f ~f ~d/~d', d)

        case request do
          '1' ->
            sunify(load1)

          '5' ->
            sunify(load5)

          'f' ->
            sunify(load15)

          'p' ->
            4711

          'n' ->
            pTotal
        end

      {:error, _} ->
        port_server_call(p, request)
    end
  end

  defp get_uint32_measurement(request, r_internal(port: p, os_type: {:unix, sys}))
       when sys == :sunos or sys == :dragonfly or
              sys == :openbsd or sys == :freebsd or sys == :darwin do
    port_server_call(p, request)
  end

  defp get_uint32_measurement(request, r_internal(os_type: {:unix, sys}))
       when sys == :irix64 or sys == :irix do
    d = :os.cmd('uptime') -- '\n'

    avg =
      :lists.reverse(
        hd(
          :string.lexemes(
            :lists.reverse(d),
            ':'
          )
        )
      )

    {:ok, [l1, l5, l15], _} = :io_lib.fread('~f, ~f, ~f', avg)

    case request do
      '1' ->
        sunify(l1)

      '5' ->
        sunify(l5)

      'f' ->
        sunify(l15)

      'p' ->
        4711

      'n' ->
        {:ok, procList} = :file.list_dir('/proc/pinfo')
        length(procList)
    end
  end

  defp get_uint32_measurement(_, _) do
    throw(:not_implemented)
  end

  defp get_util_measurement('u', r_internal(port: p)) do
    case port_server_call(p, 'u') do
      {:error, error} ->
        {:error, error}

      newCpuUtil ->
        newCpuUtil
    end
  end

  defp get_util_measurement(_, _) do
    throw(:not_implemented)
  end

  defp sunify(val) do
    round(val * 256)
  end

  defp keysearchdelete(_, _, []) do
    {false, []}
  end

  defp keysearchdelete(k, n, [t | ts])
       when :erlang.element(
              n,
              t
            ) == k do
    {{:value, t}, ts}
  end

  defp keysearchdelete(k, n, [t | ts]) do
    {x, nTs} = keysearchdelete(k, n, ts)
    {x, [t | nTs]}
  end

  defp cpu_util_diff(new, old) do
    cpu_util_diff(new, old, [])
  end

  defp cpu_util_diff([], [], acc) do
    acc
  end

  defp cpu_util_diff(
         [
           r_cpu_util(cpu: cpu, busy: newBusy, non_busy: newNonBusy)
           | newCpuUtils
         ],
         [
           r_cpu_util(cpu: cpu, busy: oldBusy, non_busy: oldNonBusy)
           | oldCpuUtils
         ],
         acc
       ) do
    {preBusy, gotBusy} = state_list_diff(newBusy, oldBusy)

    {nonBusy, gotNonBusy} =
      state_list_diff(
        newNonBusy,
        oldNonBusy
      )

    busy =
      case gotBusy or gotNonBusy do
        true ->
          preBusy

        false ->
          :lists.map(
            fn
              {:user, 0} ->
                {:user, 1}

              {_, 0} = stateTup ->
                stateTup
            end,
            preBusy
          )
      end

    cpu_util_diff(newCpuUtils, oldCpuUtils, [
      r_cpu_util(cpu: cpu, busy: busy, non_busy: nonBusy) | acc
    ])
  end

  defp cpu_util_diff([r_cpu_util(cpu: nC) | _] = new, [r_cpu_util(cpu: oC) | _] = old, acc)
       when nC < oC do
    cpu_util_diff(new, [r_cpu_util(cpu: nC) | old], acc)
  end

  defp cpu_util_diff([r_cpu_util(cpu: nC) | _] = new, [], acc) do
    cpu_util_diff(new, [r_cpu_util(cpu: nC)], acc)
  end

  defp cpu_util_diff([r_cpu_util(cpu: nC) | ns], [r_cpu_util(cpu: oC) | _] = old, acc)
       when nC > oC do
    cpu_util_diff(ns, old, acc)
  end

  defp cpu_util_diff([], _Old, acc) do
    cpu_util_diff([], [], acc)
  end

  defp cpu_util_rel(newCpuUtils, oldCpuUtils, detailed, perCpu) do
    cpu_util_rel(cpu_util_diff(newCpuUtils, oldCpuUtils), detailed, perCpu)
  end

  defp cpu_util_rel(cUDiff, false, false) do
    {b, t} =
      :lists.foldl(
        fn r_cpu_util(
             busy: busyList,
             non_busy: nonBusyList
           ),
           {busyAcc, totAcc} ->
          busy = state_list_sum(busyList)
          nonBusy = state_list_sum(nonBusyList)
          {busyAcc + busy, totAcc + busy + nonBusy}
        end,
        {0, 0},
        cUDiff
      )

    bRel = b / t * 100
    {:all, bRel, 100 - bRel, []}
  end

  defp cpu_util_rel(cUDiff, true, false) do
    cpu_util_rel_det(
      cUDiff,
      r_cpu_util(cpu: [], busy: [], non_busy: [])
    )
  end

  defp cpu_util_rel(cUDiff, false, true) do
    cpu_util_rel_pcpu(cUDiff, [])
  end

  defp cpu_util_rel(cUDiff, true, true) do
    cpu_util_rel_det_pcpu(cUDiff, [])
  end

  defp cpu_util_rel_pcpu([], acc) do
    acc
  end

  defp cpu_util_rel_pcpu(
         [
           r_cpu_util(cpu: c, busy: busyList, non_busy: nonBusyList)
           | rest
         ],
         acc
       ) do
    busy = state_list_sum(busyList)
    nonBusy = state_list_sum(nonBusyList)
    tot = busy + nonBusy

    cpu_util_rel_pcpu(
      rest,
      [{c, busy / tot * 100, nonBusy / tot * 100, []} | acc]
    )
  end

  defp cpu_util_rel_det(
         [],
         r_cpu_util(cpu: cpuAcc, busy: busyAcc, non_busy: nonBusyAcc)
       ) do
    total = state_list_sum(busyAcc) + state_list_sum(nonBusyAcc)
    {cpuAcc, mk_rel_states(busyAcc, total), mk_rel_states(nonBusyAcc, total), []}
  end

  defp cpu_util_rel_det(
         [
           r_cpu_util(cpu: cpu, busy: busy, non_busy: nonBusy)
           | rest
         ],
         r_cpu_util(cpu: cpuAcc, busy: busyAcc, non_busy: nonBusyAcc)
       ) do
    cpu_util_rel_det(
      rest,
      r_cpu_util(
        cpu: [cpu | cpuAcc],
        busy: state_list_add(busy, busyAcc),
        non_busy: state_list_add(nonBusy, nonBusyAcc)
      )
    )
  end

  defp cpu_util_rel_det_pcpu([], acc) do
    acc
  end

  defp cpu_util_rel_det_pcpu(
         [
           r_cpu_util(cpu: cpu, busy: busy, non_busy: nonBusy)
           | rest
         ],
         acc
       ) do
    total = state_list_sum(busy) + state_list_sum(nonBusy)

    cpu_util_rel_det_pcpu(
      rest,
      [
        {cpu, mk_rel_states(busy, total), mk_rel_states(nonBusy, total), []}
        | acc
      ]
    )
  end

  defp mk_rel_states(states, total) do
    :lists.map(
      fn {state, value} ->
        {state, 100 * value / total}
      end,
      states
    )
  end

  defp state_list_sum(stateList) do
    :lists.foldl(
      fn {_, x}, acc ->
        acc + x
      end,
      0,
      stateList
    )
  end

  defp state_list_diff([], []) do
    {[], false}
  end

  defp state_list_diff([{state, valueNew} | restNew], []) do
    state_list_diff(
      [{state, valueNew} | restNew],
      [{state, 0}]
    )
  end

  defp state_list_diff(
         [{state, valueNew} | restNew],
         [{state, valueOld} | restOld]
       ) do
    valDiff = val_diff(state, valueNew, valueOld)

    {restStateDiff, foundDiff} =
      state_list_diff(
        restNew,
        restOld
      )

    {[{state, valDiff} | restStateDiff], foundDiff or valDiff != 0}
  end

  defp state_list_add([], []) do
    []
  end

  defp state_list_add([{state, valueA} | restA], []) do
    [{state, valueA} | state_list_add(restA, [])]
  end

  defp state_list_add(
         [{state, valueA} | restA],
         [{state, valueB} | restB]
       ) do
    [
      {state, valueA + valueB}
      | state_list_add(
          restA,
          restB
        )
    ]
  end

  defp one_step_backwards(state, new, old) do
    case :os.type() do
      {:unix, :linux} ->
        0

      _ ->
        val_diff2(state, new, old)
    end
  end

  defp val_diff(state, new, old) when new == old - 1 do
    one_step_backwards(state, new, old)
  end

  defp val_diff(state, 4_294_967_295, 0) do
    one_step_backwards(state, 1 <<< (32 - 1), 0)
  end

  defp val_diff(state, new, old) do
    val_diff2(state, new, old)
  end

  defp val_diff2(state, new, old)
       when new > 1 <<< (32 - 1) or
              old > 1 <<< (32 - 1) do
    ensure_positive_diff(state, new - old)
  end

  defp val_diff2(state, new, old) when new < old do
    ensure_positive_diff(
      state,
      1 <<< (32 - 1 + 1 + new - old)
    )
  end

  defp val_diff2(_State, new, old) do
    new - old
  end

  defp ensure_positive_diff(_State, diff) when diff >= 0 do
    diff
  end

  defp ensure_positive_diff(state, diff) do
    throw({:error, {:negative_diff, state, diff}})
  end

  defp measurement_server_call(pid, request) do
    timeout = 5000
    send(pid, {self(), request})

    receive do
      {:data, data} ->
        data
    after
      timeout ->
        {:error, :timeout}
    end
  end

  defp measurement_server_restart(pid) do
    :erlang.exit(pid, :kill)
    measurement_server_start()
  end

  defp measurement_server_start() do
    spawn(fn ->
      measurement_server_init()
    end)
  end

  defp measurement_server_init() do
    :erlang.process_flag(:trap_exit, true)
    oS = :os.type()

    server =
      case oS do
        {:unix, flavor}
        when flavor == :sunos or
               flavor == :linux or flavor == :darwin or
               flavor == :freebsd or
               flavor == :dragonfly or
               flavor == :openbsd ->
          {:ok, pid} = port_server_start_link()
          pid

        {:unix, flavor}
        when flavor == :irix64 or
               flavor == :irix ->
          :not_used

        _ ->
          exit({:unsupported_os, oS})
      end

    measurement_server_loop(r_internal(port: server, os_type: oS))
  end

  defp measurement_server_loop(state) do
    receive do
      {_, :quit} ->
        send(r_internal(state, :port), {self(), 'q'})
        :ok

      {:DOWN, monitor, :process, _, _} ->
        measurement_server_loop(
          r_internal(state,
            util:
              :lists.keydelete(
                monitor,
                2,
                r_internal(state, :util)
              )
          )
        )

      {pid, {'u', d, pC, client}} ->
        {monitor, oldCpuUtil, utils2} =
          case keysearchdelete(client, 1, r_internal(state, :util)) do
            {{:value, {^client, mon, u}}, us} ->
              {mon, u, us}

            {false, us} ->
              {:erlang.monitor(:process, client), [], us}
          end

        try do
          get_util_measurement('u', state)
        catch
          error ->
            send(pid, {:error, error})
            measurement_server_loop(state)
        else
          newCpuUtil ->
            result = cpu_util_rel(newCpuUtil, oldCpuUtil, d, pC)
            send(pid, {:data, result})

            measurement_server_loop(
              r_internal(state,
                util: [
                  {client, monitor, newCpuUtil}
                  | utils2
                ]
              )
            )
        end

      {pid, request} ->
        _ =
          try do
            get_uint32_measurement(request, state)
          catch
            error ->
              send(pid, {:error, error})
          else
            result ->
              send(pid, {:data, result})
          end

        measurement_server_loop(state)

      {:EXIT, oldPid, _n} when r_internal(state, :port) == oldPid ->
        {:ok, newPid} = port_server_start_link()
        measurement_server_loop(r_internal(state, port: newPid))

      _Other ->
        measurement_server_loop(state)
    end
  end

  defp port_server_call(pid, command) do
    send(pid, {self(), command})

    receive do
      {^pid, {:data, result}} ->
        result

      {^pid, {:error, reason}} ->
        {:error, reason}
    end
  end

  defp port_server_start_link() do
    timeout = 6000

    pid =
      spawn_link(fn ->
        port_server_init(timeout)
      end)

    send(pid, {self(), 'p'})

    receive do
      {^pid, {:data, 4711}} ->
        {:ok, pid}

      {:error, reason} ->
        {:error, reason}
    after
      timeout ->
        {:error, :timeout}
    end
  end

  defp port_server_init(timeout) do
    port = start_portprogram()
    port_server_loop(port, timeout)
  end

  defp port_server_loop(port, timeout) do
    receive do
      {pid, {:timeout, ^timeout}} ->
        send(pid, {:data, timeout})
        port_server_loop(port, timeout)

      {pid, 'n'} ->
        :erlang.port_command(port, 'n')
        result = port_receive_uint32(port, timeout)
        send(pid, {self(), {:data, result}})
        port_server_loop(port, timeout)

      {pid, '1'} ->
        :erlang.port_command(port, '1')
        result = port_receive_uint32(port, timeout)
        send(pid, {self(), {:data, result}})
        port_server_loop(port, timeout)

      {pid, '5'} ->
        :erlang.port_command(port, '5')
        result = port_receive_uint32(port, timeout)
        send(pid, {self(), {:data, result}})
        port_server_loop(port, timeout)

      {pid, 'f'} ->
        :erlang.port_command(port, 'f')
        result = port_receive_uint32(port, timeout)
        send(pid, {self(), {:data, result}})
        port_server_loop(port, timeout)

      {pid, 'u'} ->
        :erlang.port_command(port, 'u')
        result = port_receive_util(port, timeout)
        send(pid, {self(), {:data, result}})
        port_server_loop(port, timeout)

      {pid, 'p'} ->
        :erlang.port_command(port, 'p')
        result = port_receive_uint32(port, timeout)
        send(pid, {self(), {:data, result}})
        port_server_loop(port, timeout)

      {pid, 'q'} ->
        :erlang.port_command(port, 'q')
        :erlang.port_close(port)
        send(pid, {self(), {:data, :quit}})
        :ok

      _ ->
        port_server_loop(port, timeout)
    end
  end

  defp port_receive_uint32(port, timeout) do
    port_receive_uint32(port, timeout, [])
  end

  defp port_receive_uint32(_Port, _Timeout, [d3, d2, d1, d0]) do
    d3 <<< 24 ||| d2 <<< 16 ||| d1 <<< 8 ||| d0
  end

  defp port_receive_uint32(_Port, _Timeout, [[_, _, _, _] | g]) do
    exit({:port_garbage, g})
  end

  defp port_receive_uint32(port, timeout, d) do
    receive do
      {:EXIT, ^port, reason} ->
        exit({:port_exit, reason})

      {^port, {:data, nD}} ->
        port_receive_uint32(port, timeout, d ++ nD)
    after
      timeout ->
        exit(:timeout_uint32)
    end
  end

  defp port_receive_util(port, timeout) do
    receive do
      {^port,
       {:data,
        [
          [nP3, nP2, nP1, nP0, nE3, nE2, nE1, nE0]
          | cpuData
        ]}} ->
        port_receive_cpu_util(
          nP3 <<< 24 ||| nP2 <<< 16 ||| nP1 <<< 8 ||| nP0,
          nE3 <<< 24 ||| nE2 <<< 16 ||| nE1 <<< 8 ||| nE0,
          cpuData,
          []
        )

      {:EXIT, ^port, reason} ->
        exit({:port_exit, reason})
    after
      timeout ->
        exit(:timeout_util)
    end
  end

  defp port_receive_cpu_util(0, _NE, [], cpuList) do
    :lists.reverse(cpuList)
  end

  defp port_receive_cpu_util(0, _NE, garbage, _) do
    exit({:port_garbage, garbage})
  end

  defp port_receive_cpu_util(nP, nE, cpuData, cpuList) do
    {cpuUtil, rest} = port_receive_cpu_util_entries(nE, r_cpu_util(), cpuData)
    port_receive_cpu_util(nP - 1, nE, rest, [cpuUtil | cpuList])
  end

  defp port_receive_cpu_util_entries(0, cU, rest) do
    {cU, rest}
  end

  defp port_receive_cpu_util_entries(nE, cU, [
         [cID3, cID2, cID1, cID0, val3, val2, val1, val0]
         | cpuData
       ]) do
    tagId = cID3 <<< 24 ||| cID2 <<< 16 ||| cID1 <<< 8 ||| cID0
    value = val3 <<< 24 ||| val2 <<< 16 ||| val1 <<< 8 ||| val0

    case tagId do
      0 ->
        newCU = r_cpu_util(cU, cpu: value)
        port_receive_cpu_util_entries(nE - 1, newCU, cpuData)

      1 ->
        newCU = r_cpu_util(cU, busy: [{:user, value} | r_cpu_util(cU, :busy)])
        port_receive_cpu_util_entries(nE - 1, newCU, cpuData)

      2 ->
        newCU =
          r_cpu_util(cU,
            busy: [
              {:nice_user, value}
              | r_cpu_util(cU, :busy)
            ]
          )

        port_receive_cpu_util_entries(nE - 1, newCU, cpuData)

      3 ->
        newCU = r_cpu_util(cU, busy: [{:kernel, value} | r_cpu_util(cU, :busy)])
        port_receive_cpu_util_entries(nE - 1, newCU, cpuData)

      4 ->
        newCU =
          r_cpu_util(cU,
            non_busy: [
              {:wait, value}
              | r_cpu_util(cU, :non_busy)
            ]
          )

        port_receive_cpu_util_entries(nE - 1, newCU, cpuData)

      5 ->
        newCU =
          r_cpu_util(cU,
            non_busy: [
              {:idle, value}
              | r_cpu_util(cU, :non_busy)
            ]
          )

        port_receive_cpu_util_entries(nE - 1, newCU, cpuData)

      6 ->
        newCU = r_cpu_util(cU, busy: [{:hard_irq, value} | r_cpu_util(cU, :busy)])
        port_receive_cpu_util_entries(nE - 1, newCU, cpuData)

      7 ->
        newCU = r_cpu_util(cU, busy: [{:soft_irq, value} | r_cpu_util(cU, :busy)])
        port_receive_cpu_util_entries(nE - 1, newCU, cpuData)

      8 ->
        newCU =
          r_cpu_util(cU,
            non_busy: [
              {:steal, value}
              | r_cpu_util(cU, :non_busy)
            ]
          )

        port_receive_cpu_util_entries(nE - 1, newCU, cpuData)

      unhandled ->
        exit({:unexpected_type_id, unhandled})
    end
  end

  defp port_receive_cpu_util_entries(_, _, data) do
    exit({:data_mismatch, data})
  end

  defp start_portprogram() do
    port = :os_mon.open_port('cpu_sup', [:stream])
    :erlang.port_command(port, 'p')
    4711 = port_receive_uint32(port, 5000)
    port
  end
end
