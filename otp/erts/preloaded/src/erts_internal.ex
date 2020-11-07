defmodule :m_erts_internal do
  use Bitwise

  def await_port_send_result(ref, busy, ok) do
    receive do
      {^ref, false} ->
        busy

      {^ref, _} ->
        ok
    end
  end

  def await_result(ref) when is_reference(ref) do
    receive do
      {^ref, result} ->
        result
    end
  end

  def gather_io_bytes(ref, no)
      when is_reference(ref) and
             is_integer(no) and no > 0 do
    gather_io_bytes(ref, no, 0, 0)
  end

  defp gather_io_bytes(_Ref, 0, inAcc, outAcc) do
    {{:input, inAcc}, {:output, outAcc}}
  end

  defp gather_io_bytes(ref, no, inAcc, outAcc) do
    receive do
      {^ref, _SchedId, in__, out} ->
        gather_io_bytes(ref, no - 1, inAcc + in__, outAcc + out)
    end
  end

  def open_port(_PortName, _PortSettings) do
    :erlang.nif_error(:undefined)
  end

  def port_command(_Port, _Data, _OptionList) do
    :erlang.nif_error(:undefined)
  end

  def port_connect(_Port, _Pid) do
    :erlang.nif_error(:undefined)
  end

  def port_close(_Port) do
    :erlang.nif_error(:undefined)
  end

  def port_control(_Port, _Operation, _Data) do
    :erlang.nif_error(:undefined)
  end

  def port_call(_Port, _Operation, _Data) do
    :erlang.nif_error(:undefined)
  end

  def port_info(_Result) do
    :erlang.nif_error(:undefined)
  end

  def port_info(_Result, _Item) do
    :erlang.nif_error(:undefined)
  end

  def request_system_task(_Pid, _Prio, _Request) do
    :erlang.nif_error(:undefined)
  end

  def request_system_task(_RequesterPid, _TargetPid, _Prio, _Request) do
    :erlang.nif_error(:undefined)
  end

  def garbage_collect(_Mode) do
    :erlang.nif_error(:undefined)
  end

  def check_process_code(_Module) do
    :erlang.nif_error(:undefined)
  end

  def check_process_code(pid, module, optionList) do
    async = get_cpc_opts(optionList, :sync)

    case async do
      {:async, reqId} ->
        {:priority, prio} =
          :erlang.process_info(
            :erlang.self(),
            :priority
          )

        :erts_internal.request_system_task(pid, prio, {:check_process_code, reqId, module})
        :async

      :sync ->
        case pid == :erlang.self() do
          true ->
            :erts_internal.check_process_code(module)

          false ->
            {:priority, prio} =
              :erlang.process_info(
                :erlang.self(),
                :priority
              )

            reqId = :erlang.make_ref()
            :erts_internal.request_system_task(pid, prio, {:check_process_code, reqId, module})

            receive do
              {:check_process_code, ^reqId, checkResult} ->
                checkResult
            end
        end
    end
  end

  defp get_cpc_opts(
         [{:async, _ReqId} = asyncTuple | options],
         _OldAsync
       ) do
    get_cpc_opts(options, asyncTuple)
  end

  defp get_cpc_opts([{:allow_gc, allowGC} | options], async)
       when allowGC == true or allowGC == false do
    get_cpc_opts(options, async)
  end

  defp get_cpc_opts([], async) do
    async
  end

  def check_dirty_process_code(_Pid, _Module) do
    :erlang.nif_error(:undefined)
  end

  def is_process_executing_dirty(_Pid) do
    :erlang.nif_error(:undefined)
  end

  def dirty_process_handle_signals(_Pid) do
    :erlang.nif_error(:undefined)
  end

  def release_literal_area_switch() do
    :erlang.nif_error(:undefined)
  end

  def wait_release_literal_area_switch(waitMsg) do
    receive do
      ^waitMsg ->
        :ok
    end

    :erts_internal.release_literal_area_switch()
  end

  def purge_module(_Module, _Op) do
    :erlang.nif_error(:undefined)
  end

  def system_check(_Type) do
    :erlang.nif_error(:undefined)
  end

  def gather_system_check_result(ref) when is_reference(ref) do
    gather_system_check_result(
      ref,
      :erlang.system_info(:schedulers)
    )
  end

  defp gather_system_check_result(_Ref, 0) do
    :ok
  end

  defp gather_system_check_result(ref, n) do
    receive do
      ^ref ->
        gather_system_check_result(ref, n - 1)
    end
  end

  def cmp_term(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def map_to_tuple_keys(_M) do
    :erlang.nif_error(:undefined)
  end

  def term_type(_T) do
    :erlang.nif_error(:undefined)
  end

  def map_hashmap_children(_M) do
    :erlang.nif_error(:undefined)
  end

  def map_next(_I, _M, _A) do
    :erlang.nif_error(:undefined)
  end

  def flush_monitor_messages(ref, multi, res) when is_reference(ref) do
    receive do
      {_, ^ref, _, _, _} ->
        case multi do
          false ->
            res

          _ ->
            flush_monitor_messages(ref, multi, res)
        end
    after
      0 ->
        res
    end
  end

  def time_unit() do
    :erlang.nif_error(:undefined)
  end

  def perf_counter_unit() do
    :erlang.nif_error(:undefined)
  end

  def is_system_process(_Pid) do
    :erlang.nif_error(:undefined)
  end

  def await_microstate_accounting_modifications(ref, result, threads) do
    _ = microstate_accounting(ref, threads)
    result
  end

  def gather_microstate_accounting_result(ref, threads) do
    microstate_accounting(ref, threads)
  end

  defp microstate_accounting(_Ref, 0) do
    []
  end

  defp microstate_accounting(ref, threads) do
    receive do
      ^ref ->
        microstate_accounting(ref, threads - 1)

      {^ref, res} ->
        [res | microstate_accounting(ref, threads - 1)]
    end
  end

  def trace(_PidSpec, _How, _FlagList) do
    :erlang.nif_error(:undefined)
  end

  def trace_pattern(_MFA, _MatchSpec, _FlagList) do
    :erlang.nif_error(:undefined)
  end

  def dist_ctrl_put_data(dHandle, ioList) do
    try do
      binary = :erlang.iolist_to_binary(ioList)
      :erlang.dist_ctrl_put_data(dHandle, binary)
    catch
      class, reason ->
        rootST =
          try do
            :erlang.error(reason)
          catch
            :error, ^reason ->
              case __STACKTRACE__ do
                [] ->
                  []

                [_ | t] ->
                  t
              end
          end

        stackTrace = [
          {:erlang, :dist_ctrl_put_data, [dHandle, ioList], []}
          | rootST
        ]

        :erlang.raise(class, reason, stackTrace)
    end
  end

  def get_dflags() do
    :erlang.nif_error(:undefined)
  end

  def get_creation() do
    :erlang.nif_error(:undefined)
  end

  def new_connection(_Node) do
    :erlang.nif_error(:undefined)
  end

  def abort_pending_connection(_Node, _ConnId) do
    :erlang.nif_error(:undefined)
  end

  def system_flag_scheduler_wall_time(bool) do
    :kernel_refc.scheduler_wall_time(bool)
  end

  def scheduler_wall_time(_Enable) do
    :erlang.nif_error(:undefined)
  end

  def await_sched_wall_time_modifications(ref, result) do
    sched_wall_time(ref, :erlang.system_info(:schedulers))
    result
  end

  def gather_sched_wall_time_result(ref) when :erlang.is_reference(ref) do
    sched_wall_time(ref, :erlang.system_info(:schedulers), [])
  end

  defp sched_wall_time(_Ref, 0) do
    :ok
  end

  defp sched_wall_time(ref, n) do
    receive do
      ^ref ->
        sched_wall_time(ref, n - 1)
    end
  end

  defp sched_wall_time(_Ref, 0, acc) do
    acc
  end

  defp sched_wall_time(ref, n, :undefined) do
    receive do
      {^ref, _} ->
        sched_wall_time(ref, n - 1, :undefined)
    end
  end

  defp sched_wall_time(ref, n, acc) do
    receive do
      {^ref, :undefined} ->
        sched_wall_time(ref, n - 1, :undefined)

      {^ref, sWTL} when :erlang.is_list(sWTL) ->
        sched_wall_time(ref, n - 1, acc ++ sWTL)

      {^ref, sWT} ->
        sched_wall_time(ref, n - 1, [sWT | acc])
    end
  end

  def group_leader(_GL, _Pid) do
    :erlang.nif_error(:undefined)
  end

  def group_leader(_GL, _Pid, _Ref) do
    :erlang.nif_error(:undefined)
  end

  def is_process_alive(_Pid, _Ref) do
    :erlang.nif_error(:undefined)
  end

  def is_process_alive(pid) do
    ref = make_ref()
    :erts_internal.is_process_alive(pid, ref)

    receive do
      {^ref, res} ->
        res
    end
  end

  def gather_alloc_histograms(_) do
    :erlang.nif_error(:undef)
  end

  def gather_carrier_info(_) do
    :erlang.nif_error(:undef)
  end

  def suspend_process(_Suspendee, _OptList) do
    :erlang.nif_error(:undefined)
  end

  def process_display(_Pid, _Type) do
    :erlang.nif_error(:undefined)
  end

  def process_flag(_Pid, _Flag, _Value) do
    :erlang.nif_error(:undefined)
  end

  def create_dist_channel(_Node, _DistCtrlr, _Tpl) do
    :erlang.nif_error(:undefined)
  end

  def erase_persistent_terms() do
    :erlang.nif_error(:undefined)
  end

  def atomics_new(_Arity, _EncOpts) do
    :erlang.nif_error(:undef)
  end

  def counters_new(_Size) do
    :erlang.nif_error(:undef)
  end

  def counters_get(_Ref, _Ix) do
    :erlang.nif_error(:undef)
  end

  def counters_add(_Ref, _Ix, _Incr) do
    :erlang.nif_error(:undef)
  end

  def counters_put(_Ref, _Ix, _Value) do
    :erlang.nif_error(:undef)
  end

  def counters_info(_Ref) do
    :erlang.nif_error(:undef)
  end

  def spawn_system_process(_Mod, _Func, _Args) do
    :erlang.nif_error(:undefined)
  end

  def ets_lookup_binary_info(_Tab, _Key) do
    :erlang.nif_error(:undef)
  end

  def ets_super_user(_Bool) do
    :erlang.nif_error(:undef)
  end

  def ets_raw_first(_Tab) do
    :erlang.nif_error(:undef)
  end

  def ets_raw_next(_Tab, _Key) do
    :erlang.nif_error(:undef)
  end

  def ets_info_binary(tab) do
    try do
      :erts_internal.ets_super_user(true)
      :ets.safe_fixtable(tab, true)
      ets_info_binary_iter(tab, :erts_internal.ets_raw_first(tab), [])
    catch
      c, r ->
        ets_info_binary_error(tab, c, r, __STACKTRACE__)
    after
      :ets.safe_fixtable(tab, false)
      :erts_internal.ets_super_user(false)
    end
  end

  defp ets_info_binary_error(tab, c, r, []) do
    :erlang.raise(c, r, [{:ets, :info, [tab, :binary], []}])
  end

  defp ets_info_binary_error(tab, c, r, [sF | sFs])
       when :erlang.element(
              1,
              sF
            ) == :erts_internal and
              :erlang.element(
                2,
                sF
              ) == :ets_info_binary do
    :erlang.raise(c, r, [{:ets, :info, [tab, :binary], []} | sFs])
  end

  defp ets_info_binary_error(tab, c, r, [_SF | sFs]) do
    ets_info_binary_error(tab, c, r, sFs)
  end

  defp ets_info_binary_iter(_Tab, :"$end_of_table", acc) do
    acc
  end

  defp ets_info_binary_iter(tab, key, acc) do
    newAcc =
      case :erts_internal.ets_lookup_binary_info(
             tab,
             key
           ) do
        [] ->
          acc

        [bI] ->
          [bI | acc]

        [_ | _] = bIL ->
          bIL ++ acc
      end

    ets_info_binary_iter(tab, :erts_internal.ets_raw_next(tab, key), newAcc)
  end

  def get_internal_state_blocked(arg) do
    :erlang.system_flag(:multi_scheduling, :block)

    result =
      try do
        :erts_debug.get_internal_state({arg, :blocked})
      after
        :erlang.system_flag(:multi_scheduling, :unblock)
      end

    result
  end

  def spawn_request(_Module, _Function, _Args, _Opts) do
    :erlang.nif_error(:undef)
  end

  def spawn_init({m, f, a}) do
    apply(m, f, a)
  end

  def dist_spawn_request(_Node, _MFA, _Opts, _Type) do
    :erlang.nif_error(:undef)
  end

  def dist_spawn_init(mFA) do
    {m, f, _NoA} = mFA

    receive do
      a ->
        :erlang.apply(m, f, a)
    end
  end

  def crasher(node, mod, fun, args, [], reason) do
    :error_logger.warning_msg('** Can not start ~w:~w,~w on ~w **~n', [mod, fun, args, node])
    :erlang.exit(reason)
  end

  def crasher(node, mod, fun, args, opts, reason) do
    :error_logger.warning_msg(
      '** Can not start ~w:~w,~w (~w) on ~w **~n',
      [mod, fun, args, opts, node]
    )

    :erlang.exit(reason)
  end

  def prepare_loading(_Module, _Code) do
    :erlang.nif_error(:undefined)
  end

  def beamfile_chunk(_Bin, _Chunk) do
    :erlang.nif_error(:undefined)
  end

  def beamfile_module_md5(_Bin) do
    :erlang.nif_error(:undefined)
  end
end
