defmodule :m_memsup do
  use Bitwise
  @behaviour :gen_server
  require Record

  Record.defrecord(:r_state, :state,
    os: :undefined,
    port_mode: :undefined,
    mem_usage: :undefined,
    worst_mem_user: :undefined,
    sys_only: :undefined,
    timeout: :undefined,
    helper_timeout: :undefined,
    sys_mem_watermark: :undefined,
    proc_mem_watermark: :undefined,
    pid: :undefined,
    wd_timer: :undefined,
    ext_wd_timer: :undefined,
    pending: [],
    ext_pending: []
  )

  def start_link() do
    :gen_server.start_link({:local, :memsup}, :memsup, [], [])
  end

  def get_os_wordsize() do
    :os_mon.call(:memsup, :get_os_wordsize, :infinity)
  end

  def get_memory_data() do
    :os_mon.call(:memsup, :get_memory_data, :infinity)
  end

  def get_system_memory_data() do
    :os_mon.call(:memsup, :get_system_memory_data, :infinity)
  end

  def get_check_interval() do
    :os_mon.call(:memsup, :get_check_interval, :infinity)
  end

  def set_check_interval(minutes) do
    case param_type(:memory_check_interval, minutes) do
      true ->
        mS = minutes_to_ms(minutes)
        :os_mon.call(:memsup, {:set_check_interval, mS}, :infinity)

      false ->
        :erlang.error(:badarg)
    end
  end

  def get_procmem_high_watermark() do
    :os_mon.call(:memsup, :get_procmem_high_watermark, :infinity)
  end

  def set_procmem_high_watermark(float) do
    case param_type(
           :process_memory_high_watermark,
           float
         ) do
      true ->
        :os_mon.call(:memsup, {:set_procmem_high_watermark, float}, :infinity)

      false ->
        :erlang.error(:badarg)
    end
  end

  def get_sysmem_high_watermark() do
    :os_mon.call(:memsup, :get_sysmem_high_watermark, :infinity)
  end

  def set_sysmem_high_watermark(float) do
    case param_type(
           :system_memory_high_watermark,
           float
         ) do
      true ->
        :os_mon.call(:memsup, {:set_sysmem_high_watermark, float}, :infinity)

      false ->
        :erlang.error(:badarg)
    end
  end

  def get_helper_timeout() do
    :os_mon.call(:memsup, :get_helper_timeout, :infinity)
  end

  def set_helper_timeout(seconds) do
    case param_type(:memsup_helper_timeout, seconds) do
      true ->
        :os_mon.call(:memsup, {:set_helper_timeout, seconds}, :infinity)

      false ->
        :erlang.error(:badarg)
    end
  end

  def dummy_reply(:get_memory_data) do
    dummy_reply(
      :get_memory_data,
      :os_mon.get_env(:memsup, :memsup_system_only)
    )
  end

  def dummy_reply(:get_system_memory_data) do
    []
  end

  def dummy_reply(:get_os_wordsize) do
    0
  end

  def dummy_reply(:get_check_interval) do
    minutes_to_ms(
      :os_mon.get_env(
        :memsup,
        :memory_check_interval
      )
    )
  end

  def dummy_reply({:set_check_interval, _}) do
    :ok
  end

  def dummy_reply(:get_procmem_high_watermark) do
    trunc(
      100 *
        :os_mon.get_env(
          :memsup,
          :process_memory_high_watermark
        )
    )
  end

  def dummy_reply({:set_procmem_high_watermark, _}) do
    :ok
  end

  def dummy_reply(:get_sysmem_high_watermark) do
    trunc(
      100 *
        :os_mon.get_env(
          :memsup,
          :system_memory_high_watermark
        )
    )
  end

  def dummy_reply({:set_sysmem_high_watermark, _}) do
    :ok
  end

  def dummy_reply(:get_helper_timeout) do
    :os_mon.get_env(:memsup, :memsup_helper_timeout)
  end

  def dummy_reply({:set_helper_timeout, _}) do
    :ok
  end

  defp dummy_reply(:get_memory_data, true) do
    {0, 0, :undefined}
  end

  defp dummy_reply(:get_memory_data, false) do
    {0, 0, {self(), 0}}
  end

  def param_type(:memsup_improved_system_memory_data, val)
      when val == true or val == false do
    true
  end

  def param_type(:memsup_system_only, val)
      when val == true or
             val == false do
    true
  end

  def param_type(:memory_check_interval, val)
      when is_integer(val) and val > 0 do
    true
  end

  def param_type(:memsup_helper_timeout, val)
      when is_integer(val) and val > 0 do
    true
  end

  def param_type(:system_memory_high_watermark, val)
      when is_number(val) and 0 <= val and val <= 1 do
    true
  end

  def param_type(:process_memory_high_watermark, val)
      when is_number(val) and 0 <= val and val <= 1 do
    true
  end

  def param_type(_Param, _Val) do
    false
  end

  def param_default(:memsup_improved_system_memory_data) do
    false
  end

  def param_default(:memsup_system_only) do
    false
  end

  def param_default(:memory_check_interval) do
    1
  end

  def param_default(:memsup_helper_timeout) do
    30
  end

  def param_default(:system_memory_high_watermark) do
    0.8
  end

  def param_default(:process_memory_high_watermark) do
    0.05
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)
    :erlang.process_flag(:priority, :low)
    oS = :os.type()

    portMode =
      case oS do
        {:unix, :darwin} ->
          true

        {:unix, :freebsd} ->
          false

        {:unix, :dragonfly} ->
          false

        {:unix, :linux} ->
          true

        {:unix, :openbsd} ->
          true

        {:unix, :netbsd} ->
          true

        {:unix, :irix64} ->
          true

        {:unix, :irix} ->
          true

        {:unix, :sunos} ->
          true

        {:win32, _OSname} ->
          false

        _ ->
          exit({:unsupported_os, oS})
      end

    iSMD =
      :os_mon.get_env(
        :memsup,
        :memsup_improved_system_memory_data
      )

    pid =
      cond do
        portMode ->
          spawn_link(fn ->
            port_init(iSMD)
          end)

        not portMode ->
          :undefined
      end

    sysOnly = :os_mon.get_env(:memsup, :memsup_system_only)

    timeout =
      :os_mon.get_env(
        :memsup,
        :memory_check_interval
      )

    helperTimeout =
      :os_mon.get_env(
        :memsup,
        :memsup_helper_timeout
      )

    sysMem =
      :os_mon.get_env(
        :memsup,
        :system_memory_high_watermark
      )

    procMem =
      :os_mon.get_env(
        :memsup,
        :process_memory_high_watermark
      )

    :persistent_term.put(
      :memsup_ext_memory_type_map__,
      %{
        1 => :system_total_memory,
        2 => :total_memory,
        12 => :available_memory,
        3 => :free_memory,
        6 => :buffered_memory,
        7 => :cached_memory,
        11 => :cached_memory,
        8 => :shared_memory,
        4 => :largest_free,
        5 => :number_of_free,
        9 => :total_swap,
        10 => :free_swap
      }
    )

    send(self(), :time_to_collect)

    {:ok,
     r_state(
       os: oS,
       port_mode: portMode,
       sys_only: sysOnly,
       timeout: minutes_to_ms(timeout),
       helper_timeout: sec_to_ms(helperTimeout),
       sys_mem_watermark: sysMem,
       proc_mem_watermark: procMem,
       pid: pid
     )}
  end

  def handle_call(:get_os_wordsize, _From, state) do
    wordsize = get_os_wordsize(r_state(state, :os))
    {:reply, wordsize, state}
  end

  def handle_call(:get_memory_data, from, state) do
    case r_state(state, :mem_usage) do
      {alloc, total} ->
        worst = r_state(state, :worst_mem_user)
        {:reply, {total, alloc, worst}, state}

      :undefined ->
        case r_state(state, :wd_timer) do
          :undefined ->
            wDTimer =
              :erlang.send_after(r_state(state, :timeout), self(), :reg_collection_timeout)

            pending = [{:reg, from}]

            cond do
              r_state(state, :port_mode) ->
                send(r_state(state, :pid), {self(), :collect_sys})
                {:noreply, r_state(state, wd_timer: wDTimer, pending: pending)}

              true ->
                oS = r_state(state, :os)
                self = self()

                pid =
                  spawn_link(fn ->
                    mU = get_memory_usage(oS)
                    send(self, {:collected_sys, mU})
                  end)

                {:noreply, r_state(state, pid: pid, wd_timer: wDTimer, pending: pending)}
            end

          _TimerRef ->
            pending = [{:reg, from} | r_state(state, :pending)]
            {:noreply, r_state(state, pending: pending)}
        end
    end
  end

  def handle_call(:get_system_memory_data, from, r_state(port_mode: true) = state) do
    case r_state(state, :ext_wd_timer) do
      :undefined ->
        wDTimer =
          :erlang.send_after(r_state(state, :helper_timeout), self(), :ext_collection_timeout)

        send(r_state(state, :pid), {self(), :collect_ext_sys})

        {:noreply,
         r_state(state,
           ext_wd_timer: wDTimer,
           ext_pending: [{:ext, from}]
         )}

      _TimerRef ->
        pending = [{:ext, from} | r_state(state, :ext_pending)]
        {:noreply, r_state(state, ext_pending: pending)}
    end
  end

  def handle_call(:get_system_memory_data, from, state) do
    case r_state(state, :wd_timer) do
      :undefined ->
        wDTimer =
          :erlang.send_after(r_state(state, :helper_timeout), self(), :reg_collection_timeout)

        oS = r_state(state, :os)
        self = self()

        pid =
          spawn_link(fn ->
            memUsage = get_memory_usage(oS)
            send(self, {:collected_sys, memUsage})
          end)

        {:noreply, r_state(state, pid: pid, wd_timer: wDTimer, pending: [{:ext, from}])}

      _TimerRef ->
        pending = [{:ext, from} | r_state(state, :pending)]
        {:noreply, r_state(state, pending: pending)}
    end
  end

  def handle_call(:get_check_interval, _From, state) do
    {:reply, r_state(state, :timeout), state}
  end

  def handle_call({:set_check_interval, mS}, _From, state) do
    {:reply, :ok, r_state(state, timeout: mS)}
  end

  def handle_call(:get_procmem_high_watermark, _From, state) do
    {:reply, trunc(100 * r_state(state, :proc_mem_watermark)), state}
  end

  def handle_call({:set_procmem_high_watermark, float}, _From, state) do
    {:reply, :ok, r_state(state, proc_mem_watermark: float)}
  end

  def handle_call(:get_sysmem_high_watermark, _From, state) do
    {:reply, trunc(100 * r_state(state, :sys_mem_watermark)), state}
  end

  def handle_call({:set_sysmem_high_watermark, float}, _From, state) do
    {:reply, :ok, r_state(state, sys_mem_watermark: float)}
  end

  def handle_call(:get_helper_timeout, _From, state) do
    {:reply, ms_to_sec(r_state(state, :helper_timeout)), state}
  end

  def handle_call({:set_helper_timeout, seconds}, _From, state) do
    {:reply, :ok, r_state(state, helper_timeout: sec_to_ms(seconds))}
  end

  def handle_call({:set_sys_hw, hW}, _From, state) do
    {:reply, :ok, r_state(state, sys_mem_watermark: hW)}
  end

  def handle_call({:set_pid_hw, hW}, _From, state) do
    {:reply, :ok, r_state(state, proc_mem_watermark: hW)}
  end

  def handle_call(:get_state, _From, state) do
    {:reply, state, state}
  end

  def handle_cast(_Msg, state) do
    {:noreply, state}
  end

  def handle_info(:time_to_collect, state) do
    case r_state(state, :wd_timer) do
      :undefined ->
        wDTimer =
          :erlang.send_after(r_state(state, :helper_timeout), self(), :reg_collection_timeout)

        cond do
          r_state(state, :port_mode) ->
            send(r_state(state, :pid), {self(), :collect_sys})
            {:noreply, r_state(state, wd_timer: wDTimer, pending: [:reg])}

          true ->
            oS = r_state(state, :os)
            self = self()

            pid =
              spawn_link(fn ->
                mU = get_memory_usage(oS)
                send(self, {:collected_sys, mU})
              end)

            {:noreply, r_state(state, pid: pid, wd_timer: wDTimer, pending: [:reg])}
        end

      _TimerRef ->
        {:noreply, r_state(state, pending: [:reg | r_state(state, :pending)])}
    end
  end

  def handle_info({:collected_sys, {alloc, total}}, state) do
    timeSpent =
      case :erlang.cancel_timer(r_state(state, :wd_timer)) do
        false ->
          r_state(state, :helper_timeout)

        timeLeft ->
          r_state(state, :helper_timeout) - timeLeft
      end

    flush(:reg_collection_timeout)

    state2 =
      case :lists.member(
             :reg,
             r_state(state, :pending)
           ) do
        true ->
          cond do
            alloc > r_state(state, :sys_mem_watermark) * total ->
              set_alarm(:system_memory_high_watermark, [])

            true ->
              clear_alarm(:system_memory_high_watermark)
          end

          case r_state(state, :sys_only) do
            false ->
              {pid, bytes} = get_worst_memory_user()
              threshold = r_state(state, :proc_mem_watermark) * total

              cond do
                bytes > threshold ->
                  set_alarm(:process_memory_high_watermark, pid)

                true ->
                  clear_alarm(:process_memory_high_watermark)
              end

              r_state(state,
                mem_usage: {alloc, total},
                worst_mem_user: {pid, bytes}
              )

            true ->
              r_state(state, mem_usage: {alloc, total})
          end

        false ->
          state
      end

    worst = r_state(state2, :worst_mem_user)

    sysMemUsage =
      get_ext_memory_usage(
        r_state(state2, :os),
        {alloc, total}
      )

    reply(r_state(state2, :pending), {total, alloc, worst}, sysMemUsage)

    _ =
      case :lists.member(:reg, r_state(state, :pending)) do
        true ->
          time =
            case r_state(state2, :timeout) - timeSpent do
              mS when mS < 0 ->
                0

              mS ->
                mS
            end

          :erlang.send_after(time, self(), :time_to_collect)

        false ->
          :ignore
      end

    {:noreply, r_state(state2, wd_timer: :undefined, pending: [])}
  end

  def handle_info({:EXIT, pid, :normal}, state) when is_pid(pid) do
    {:noreply, state}
  end

  def handle_info(:reg_collection_timeout, state) do
    cond do
      r_state(state, :port_mode) ->
        send(r_state(state, :pid), :cancel)

      true ->
        :erlang.exit(r_state(state, :pid), :cancel)
    end

    flush(:collected_sys)
    str = 'OS_MON (memsup) timeout, no data collected~n'
    :error_logger.warning_msg(str)

    reply(
      r_state(state, :pending),
      dummy_reply(:get_memory_data, r_state(state, :sys_only)),
      dummy_reply(:get_system_memory_data)
    )

    _ =
      case :lists.member(:reg, r_state(state, :pending)) do
        true ->
          time =
            case r_state(state, :timeout) - r_state(state, :helper_timeout) do
              mS when mS < 0 ->
                0

              mS ->
                mS
            end

          :erlang.send_after(time, self(), :time_to_collect)

        false ->
          :ignore
      end

    {:noreply, r_state(state, wd_timer: :undefined, pending: [])}
  end

  def handle_info({:EXIT, pid, :cancel}, state) when is_pid(pid) do
    {:noreply, state}
  end

  def handle_info({:collected_ext_sys, sysMemUsage}, state) do
    :ok =
      :erlang.cancel_timer(
        r_state(state, :ext_wd_timer),
        [{:async, true}]
      )

    flush(:ext_collection_timeout)
    reply(r_state(state, :ext_pending), :undef, sysMemUsage)
    {:noreply, r_state(state, ext_wd_timer: :undefined, ext_pending: [])}
  end

  def handle_info(:ext_collection_timeout, state) do
    send(r_state(state, :pid), :ext_cancel)
    flush(:collected_ext_sys)
    str = 'OS_MON (memsup) timeout, no data collected~n'
    :error_logger.warning_msg(str)
    sysMemUsage = dummy_reply(:get_system_memory_data)
    reply(r_state(state, :ext_pending), :undef, sysMemUsage)
    {:noreply, r_state(state, ext_wd_timer: :undefined, ext_pending: [])}
  end

  def handle_info({:EXIT, pid, reason}, state) when is_pid(pid) do
    {:stop, reason, state}
  end

  def handle_info(_Info, state) do
    {:noreply, state}
  end

  def terminate(_Reason, state) do
    cond do
      r_state(state, :port_mode) ->
        send(r_state(state, :pid), :close)

      true ->
        :ok
    end

    clear_alarms()
    :ok
  end

  def format_status(
        _Opt,
        [_PDict, r_state(timeout: timeout, mem_usage: memUsage, worst_mem_user: worstMemUser)]
      ) do
    {allocated, total} = memUsage

    worstMemFormat =
      case worstMemUser do
        {pid, mem} ->
          [{'Pid', pid}, {'Memory', mem}]

        :undefined ->
          :undefined
      end

    [
      {:data, [{'Timeout', timeout}]},
      {:items, {'Memory Usage', [{'Allocated', allocated}, {'Total', total}]}},
      {:items, {'Worst Memory User', worstMemFormat}}
    ]
  end

  defp get_os_wordsize({:unix, :sunos}) do
    string = clean_string(:os.cmd('isainfo -b'))
    :erlang.list_to_integer(string)
  end

  defp get_os_wordsize({:unix, :irix64}) do
    64
  end

  defp get_os_wordsize({:unix, :irix}) do
    32
  end

  defp get_os_wordsize({:unix, :linux}) do
    get_os_wordsize_with_uname()
  end

  defp get_os_wordsize({:unix, :darwin}) do
    get_os_wordsize_with_uname()
  end

  defp get_os_wordsize({:unix, :netbsd}) do
    get_os_wordsize_with_uname()
  end

  defp get_os_wordsize({:unix, :freebsd}) do
    get_os_wordsize_with_uname()
  end

  defp get_os_wordsize({:unix, :dragonfly}) do
    get_os_wordsize_with_uname()
  end

  defp get_os_wordsize({:unix, :openbsd}) do
    get_os_wordsize_with_uname()
  end

  defp get_os_wordsize(_) do
    :unsupported_os
  end

  defp get_os_wordsize_with_uname() do
    string = clean_string(:os.cmd('uname -m'))

    case string do
      'x86_64' ->
        64

      'sparc64' ->
        64

      'amd64' ->
        64

      'ppc64' ->
        64

      'ppc64le' ->
        64

      's390x' ->
        64

      'aarch64' ->
        64

      _ ->
        32
    end
  end

  defp clean_string(string) do
    :lists.flatten(
      :string.lexemes(
        string,
        [[?\r, ?\n] | '\n\t ']
      )
    )
  end

  defp reply(pending, memUsage, sysMemUsage) do
    :lists.foreach(
      fn
        :reg ->
          :ignore

        {:reg, from} ->
          :gen_server.reply(from, memUsage)

        {:ext, from} ->
          :gen_server.reply(from, sysMemUsage)
      end,
      :lists.reverse(pending)
    )
  end

  defp get_memory_usage({:unix, oSname})
       when oSname == :freebsd or
              oSname == :dragonfly do
    pageSize = freebsd_sysctl('vm.stats.vm.v_page_size')
    pageCount = freebsd_sysctl('vm.stats.vm.v_page_count')
    freeCount = freebsd_sysctl('vm.stats.vm.v_free_count')
    nMemUsed = (pageCount - freeCount) * pageSize
    nMemTotal = pageCount * pageSize
    {nMemUsed, nMemTotal}
  end

  defp get_memory_usage({:win32, _OSname}) do
    [result | _] = :os_mon_sysinfo.get_mem_info()

    {:ok, [_MemLoad, totPhys, availPhys, _TotPage, _AvailPage, _TotV, _AvailV], _RestStr} =
      :io_lib.fread('~d~d~d~d~d~d~d', result)

    {totPhys - availPhys, totPhys}
  end

  defp freebsd_sysctl(def__) do
    :erlang.list_to_integer(:os.cmd('/sbin/sysctl -n ' ++ def__) -- '\n')
  end

  defp get_ext_memory_usage(oS, {alloc, total}) do
    case oS do
      {:win32, _} ->
        [{:total_memory, total}, {:free_memory, total - alloc}, {:system_total_memory, total}]

      {:unix, :linux} ->
        [{:total_memory, total}, {:free_memory, total - alloc}, {:system_total_memory, total}]

      {:unix, :freebsd} ->
        [{:total_memory, total}, {:free_memory, total - alloc}, {:system_total_memory, total}]

      {:unix, :dragonfly} ->
        [{:total_memory, total}, {:free_memory, total - alloc}, {:system_total_memory, total}]

      _ ->
        :dummy
    end
  end

  defp port_init(iSMD) do
    :erlang.process_flag(:trap_exit, true)
    port = start_portprogram()
    port_idle(port, iSMD)
  end

  defp start_portprogram() do
    :os_mon.open_port('memsup', [{:packet, 1}])
  end

  defp port_idle(port, iSMD) do
    receive do
      {memsup, :collect_sys} ->
        send(port, {self(), {:command, [1]}})
        get_memory_usage(port, :undefined, memsup, iSMD)

      {memsup, :collect_ext_sys} ->
        send(port, {self(), {:command, [2]}})
        get_ext_memory_usage(port, %{}, memsup, iSMD)

      :cancel ->
        port_idle(port, iSMD)

      :ext_cancel ->
        port_idle(port, iSMD)

      :close ->
        :erlang.port_close(port)

      {^port, {:data, data}} ->
        exit({:port_error, data})

      {:EXIT, ^port, reason} ->
        exit({:port_died, reason})

      {:EXIT, _Memsup, _Reason} ->
        :erlang.port_close(port)
    end
  end

  defp get_memory_usage(port, alloc, memsup, iSMD) do
    receive do
      {^port, {:data, data}} when alloc == :undefined ->
        get_memory_usage(port, :erlang.list_to_integer(data, 16), memsup, iSMD)

      {^port, {:data, data}} ->
        total = :erlang.list_to_integer(data, 16)
        send(memsup, {:collected_sys, {alloc, total}})
        port_idle(port, iSMD)

      :cancel ->
        get_memory_usage_cancelled(port, alloc, iSMD)

      :close ->
        :erlang.port_close(port)

      {:EXIT, ^port, reason} ->
        exit({:port_died, reason})

      {:EXIT, _Memsup, _Reason} ->
        :erlang.port_close(port)
    end
  end

  defp get_memory_usage_cancelled(port, alloc, iSMD) do
    receive do
      {^port, {:data, _Data}} when alloc == :undefined ->
        get_memory_usage_cancelled(port, 0, iSMD)

      {^port, {:data, _Data}} ->
        port_idle(port, iSMD)

      :close ->
        :erlang.port_close(port)

      {:EXIT, ^port, reason} ->
        exit({:port_died, reason})

      {:EXIT, _Memsup, _Reason} ->
        :erlang.port_close(port)
    end
  end

  defp tag2atag(port, tag) do
    tab = :persistent_term.get(:memsup_ext_memory_type_map__)

    try do
      :maps.get(tag, tab)
    catch
      :error, _ ->
        exit({:memsup_port_error, {port, [tag]}})
    end
  end

  defp get_ext_memory_usage(port, accum, memsup, iSMD) do
    receive do
      {^port, {:data, [0]}} ->
        send(memsup, {:collected_ext_sys, :maps.to_list(accum)})
        port_idle(port, iSMD)

      {^port, {:data, [11]}} when iSMD == false ->
        get_ext_memory_usage(tag2atag(port, 11), port, accum, memsup, iSMD, true)

      {^port, {:data, [12]}} when iSMD == false ->
        get_ext_memory_usage(tag2atag(port, 12), port, accum, memsup, iSMD, true)

      {^port, {:data, [tag]}} ->
        get_ext_memory_usage(tag2atag(port, tag), port, accum, memsup, iSMD, false)

      :ext_cancel ->
        get_ext_memory_usage_cancelled(port, iSMD)

      :close ->
        :erlang.port_close(port)

      {:EXIT, ^port, reason} ->
        exit({:port_died, reason})

      {:EXIT, _Memsup, _Reason} ->
        :erlang.port_close(port)
    end
  end

  defp get_ext_memory_usage_cancelled(port, iSMD) do
    receive do
      {^port, {:data, [0]}} ->
        port_idle(port, iSMD)

      {^port, {:data, [tag]}} ->
        get_ext_memory_usage_cancelled(tag2atag(port, tag), port, iSMD)

      :close ->
        :erlang.port_close(port)

      {:EXIT, ^port, reason} ->
        exit({:port_died, reason})

      {:EXIT, _Memsup, _Reason} ->
        :erlang.port_close(port)
    end
  end

  defp get_ext_memory_usage(aTag, port, accum0, memsup, iSMD, drop) do
    receive do
      {^port, {:data, _Data}} when drop == true ->
        get_ext_memory_usage(port, accum0, memsup, iSMD)

      {^port, {:data, data}} when drop == false ->
        value = :erlang.list_to_integer(data, 16)

        accum =
          case :maps.get(aTag, accum0, :undefined) do
            :undefined ->
              :maps.put(aTag, value, accum0)

            prevValue ->
              :maps.put(aTag, value + prevValue, accum0)
          end

        get_ext_memory_usage(port, accum, memsup, iSMD)

      :cancel ->
        get_ext_memory_usage_cancelled(aTag, port, iSMD)

      :close ->
        :erlang.port_close(port)

      {:EXIT, ^port, reason} ->
        exit({:port_died, reason})

      {:EXIT, _Memsup, _Reason} ->
        :erlang.port_close(port)
    end
  end

  defp get_ext_memory_usage_cancelled(_ATag, port, iSMD) do
    receive do
      {^port, {:data, _Data}} ->
        get_ext_memory_usage_cancelled(port, iSMD)

      :close ->
        :erlang.port_close(port)

      {:EXIT, ^port, reason} ->
        exit({:port_died, reason})

      {:EXIT, _Memsup, _Reason} ->
        :erlang.port_close(port)
    end
  end

  defp get_worst_memory_user() do
    get_worst_memory_user(:erlang.processes(), self(), 0)
  end

  defp get_worst_memory_user([pid | pids], maxPid, maxMemBytes) do
    case process_memory(pid) do
      :undefined ->
        get_worst_memory_user(pids, maxPid, maxMemBytes)

      memoryBytes when memoryBytes > maxMemBytes ->
        get_worst_memory_user(pids, pid, memoryBytes)

      _MemoryBytes ->
        get_worst_memory_user(pids, maxPid, maxMemBytes)
    end
  end

  defp get_worst_memory_user([], maxPid, maxMemBytes) do
    {maxPid, maxMemBytes}
  end

  defp process_memory(pid) do
    case :erlang.process_info(pid, :memory) do
      {:memory, bytes} ->
        bytes

      :undefined ->
        :undefined
    end
  end

  defp set_alarm(alarmId, alarmDescr) do
    case :erlang.get(alarmId) do
      :set ->
        :ok

      :undefined ->
        :alarm_handler.set_alarm({alarmId, alarmDescr})
        :erlang.put(alarmId, :set)
    end
  end

  defp clear_alarm(alarmId) do
    case :erlang.get(alarmId) do
      :set ->
        :alarm_handler.clear_alarm(alarmId)
        :erlang.erase(alarmId)

      _ ->
        :ok
    end
  end

  defp clear_alarms() do
    :lists.foreach(
      fn
        {:system_memory_high_watermark = id, :set} ->
          :alarm_handler.clear_alarm(id)

        {:process_memory_high_watermark = id, :set} ->
          :alarm_handler.clear_alarm(id)

        _Other ->
          :ignore
      end,
      :erlang.get()
    )
  end

  defp minutes_to_ms(minutes) do
    trunc(60000 * minutes)
  end

  defp sec_to_ms(sec) do
    trunc(1000 * sec)
  end

  defp ms_to_sec(mS) do
    div(mS, 1000)
  end

  defp flush(msg) do
    receive do
      {^msg, _} ->
        true

      ^msg ->
        true
    after
      0 ->
        true
    end
  end
end
