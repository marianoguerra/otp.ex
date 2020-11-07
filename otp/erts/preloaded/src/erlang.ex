defmodule :m_erlang do
  use Bitwise

  import Kernel,
    except: [
      ++: 2,
      spawn_monitor: 1,
      spawn: 3,
      spawn_monitor: 3,
      --: 2,
      spawn_link: 3,
      max: 2,
      make_ref: 0,
      send: 2,
      apply: 3,
      length: 1,
      spawn: 1,
      apply: 2,
      throw: 1,
      min: 2,
      spawn_link: 1,
      exit: 1
    ]

  def adler32(_Data) do
    :erlang.nif_error(:undefined)
  end

  def adler32(_OldAdler, _Data) do
    :erlang.nif_error(:undefined)
  end

  def adler32_combine(_FirstAdler, _SecondAdler, _SecondSize) do
    :erlang.nif_error(:undefined)
  end

  def append_element(_Tuple1, _Term) do
    :erlang.nif_error(:undefined)
  end

  def atom_to_binary(atom) do
    :erlang.atom_to_binary(atom, :utf8)
  end

  def atom_to_binary(_Atom, _Encoding) do
    :erlang.nif_error(:undefined)
  end

  def atom_to_list(_Atom) do
    :erlang.nif_error(:undefined)
  end

  def binary_part(_Subject, _PosLen) do
    :erlang.nif_error(:undefined)
  end

  def binary_part(_Subject, _Start, _Length) do
    :erlang.nif_error(:undefined)
  end

  def binary_to_atom(binary) do
    :erlang.binary_to_atom(binary, :utf8)
  end

  def binary_to_atom(_Binary, _Encoding) do
    :erlang.nif_error(:undefined)
  end

  def binary_to_existing_atom(binary) do
    :erlang.binary_to_existing_atom(binary, :utf8)
  end

  def binary_to_existing_atom(_Binary, _Encoding) do
    :erlang.nif_error(:undefined)
  end

  def binary_to_float(_Binary) do
    :erlang.nif_error(:undefined)
  end

  def binary_to_integer(_Binary) do
    :erlang.nif_error(:undefined)
  end

  def binary_to_integer(_Binary, _Base) do
    :erlang.nif_error(:undefined)
  end

  def binary_to_list(_Binary) do
    :erlang.nif_error(:undefined)
  end

  def binary_to_list(_Binary, _Start, _Stop) do
    :erlang.nif_error(:undefined)
  end

  def binary_to_term(_Binary) do
    :erlang.nif_error(:undefined)
  end

  def binary_to_term(_Binary, _Opts) do
    :erlang.nif_error(:undefined)
  end

  def bit_size(_Bitstring) do
    :erlang.nif_error(:undefined)
  end

  def bitsize(_P1) do
    :erlang.nif_error(:undefined)
  end

  def bitstring_to_list(_Bitstring) do
    :erlang.nif_error(:undefined)
  end

  def bump_reductions(_Reductions) do
    :erlang.nif_error(:undefined)
  end

  def byte_size(_Bitstring) do
    :erlang.nif_error(:undefined)
  end

  def call_on_load_function(_P1) do
    :erlang.nif_error(:undefined)
  end

  def cancel_timer(_TimerRef) do
    :erlang.nif_error(:undefined)
  end

  def cancel_timer(_TimerRef, _Options) do
    :erlang.nif_error(:undefined)
  end

  def ceil(_) do
    :erlang.nif_error(:undef)
  end

  def check_old_code(_Module) do
    :erlang.nif_error(:undefined)
  end

  def check_process_code(pid, module) do
    try do
      :erts_internal.check_process_code(pid, module, [{:allow_gc, true}])
    catch
      :error, error ->
        :erlang.error(error, [pid, module])
    end
  end

  def check_process_code(pid, module, optionList) do
    try do
      :erts_internal.check_process_code(pid, module, optionList)
    catch
      :error, error ->
        :erlang.error(error, [pid, module, optionList])
    end
  end

  def crc32(_Data) do
    :erlang.nif_error(:undefined)
  end

  def crc32(_OldCrc, _Data) do
    :erlang.nif_error(:undefined)
  end

  def crc32_combine(_FirstCrc, _SecondCrc, _SecondSize) do
    :erlang.nif_error(:undefined)
  end

  def date() do
    :erlang.nif_error(:undefined)
  end

  def decode_packet(_Type, _Bin, _Options) do
    :erlang.nif_error(:undefined)
  end

  def delete_element(_Index, _Tuple1) do
    :erlang.nif_error(:undefined)
  end

  def delete_module(_Module) do
    :erlang.nif_error(:undefined)
  end

  def demonitor(_MonitorRef) do
    :erlang.nif_error(:undefined)
  end

  def demonitor(_MonitorRef, _OptionList) do
    :erlang.nif_error(:undefined)
  end

  def display(_Term) do
    :erlang.nif_error(:undefined)
  end

  def display_nl() do
    :erlang.nif_error(:undefined)
  end

  def display_string(_P1) do
    :erlang.nif_error(:undefined)
  end

  def dt_append_vm_tag_data(_IoData) do
    :erlang.nif_error(:undefined)
  end

  def dt_get_tag() do
    :erlang.nif_error(:undefined)
  end

  def dt_get_tag_data() do
    :erlang.nif_error(:undefined)
  end

  def dt_prepend_vm_tag_data(_IoData) do
    :erlang.nif_error(:undefined)
  end

  def dt_put_tag(_IoData) do
    :erlang.nif_error(:undefined)
  end

  def dt_restore_tag(_TagData) do
    :erlang.nif_error(:undefined)
  end

  def dt_spread_tag(_Bool) do
    :erlang.nif_error(:undefined)
  end

  def erase() do
    :erlang.nif_error(:undefined)
  end

  def erase(_Key) do
    :erlang.nif_error(:undefined)
  end

  def error(_Reason) do
    :erlang.nif_error(:undefined)
  end

  def error(_Reason, _Args) do
    :erlang.nif_error(:undefined)
  end

  def exit(_Reason) do
    :erlang.nif_error(:undefined)
  end

  def exit(_Pid, _Reason) do
    :erlang.nif_error(:undefined)
  end

  def exit_signal(_Pid, _Reason) do
    :erlang.nif_error(:undefined)
  end

  def external_size(_Term) do
    :erlang.nif_error(:undefined)
  end

  def external_size(_Term, _Options) do
    :erlang.nif_error(:undefined)
  end

  def finish_loading(_List) do
    :erlang.nif_error(:undefined)
  end

  def finish_after_on_load(_P1, _P2) do
    :erlang.nif_error(:undefined)
  end

  def float(_Number) do
    :erlang.nif_error(:undefined)
  end

  def float_to_binary(_Float) do
    :erlang.nif_error(:undefined)
  end

  def float_to_binary(_Float, _Options) do
    :erlang.nif_error(:undefined)
  end

  def float_to_list(_Float) do
    :erlang.nif_error(:undefined)
  end

  def float_to_list(_Float, _Options) do
    :erlang.nif_error(:undefined)
  end

  def floor(_) do
    :erlang.nif_error(:undef)
  end

  def fun_info(_Fun, _Item) do
    :erlang.nif_error(:undefined)
  end

  def fun_info_mfa(_Fun) do
    :erlang.nif_error(:undefined)
  end

  def fun_to_list(_Fun) do
    :erlang.nif_error(:undefined)
  end

  def function_exported(_Module, _Function, _Arity) do
    :erlang.nif_error(:undefined)
  end

  def garbage_collect() do
    :erts_internal.garbage_collect(:major)
  end

  def garbage_collect(pid) do
    try do
      :erlang.garbage_collect(pid, [])
    catch
      :error, error ->
        :erlang.error(error, [pid])
    end
  end

  require Record
  Record.defrecord(:r_gcopt, :gcopt, async: :sync, type: :major)

  def garbage_collect(pid, optionList) do
    try do
      gcOpts = get_gc_opts(optionList, r_gcopt())

      case r_gcopt(gcOpts, :async) do
        {:async, reqId} ->
          {:priority, prio} =
            :erlang.process_info(
              :erlang.self(),
              :priority
            )

          :erts_internal.request_system_task(
            pid,
            prio,
            {:garbage_collect, reqId, r_gcopt(gcOpts, :type)}
          )

          :async

        :sync ->
          case pid == :erlang.self() do
            true ->
              :erts_internal.garbage_collect(r_gcopt(gcOpts, :type))

            false ->
              {:priority, prio} =
                :erlang.process_info(
                  :erlang.self(),
                  :priority
                )

              reqId = :erlang.make_ref()

              :erts_internal.request_system_task(
                pid,
                prio,
                {:garbage_collect, reqId, r_gcopt(gcOpts, :type)}
              )

              receive do
                {:garbage_collect, ^reqId, gCResult} ->
                  gCResult
              end
          end
      end
    catch
      :error, error ->
        :erlang.error(error, [pid, optionList])
    end
  end

  defp get_gc_opts(
         [{:async, _ReqId} = asyncTuple | options],
         gcOpt = r_gcopt()
       ) do
    get_gc_opts(options, r_gcopt(gcOpt, async: asyncTuple))
  end

  defp get_gc_opts([{:type, t} | options], gcOpt = r_gcopt()) do
    get_gc_opts(options, r_gcopt(gcOpt, type: t))
  end

  defp get_gc_opts([], gcOpt) do
    gcOpt
  end

  def garbage_collect_message_area() do
    :erlang.nif_error(:undefined)
  end

  def get() do
    :erlang.nif_error(:undefined)
  end

  def get(_Key) do
    :erlang.nif_error(:undefined)
  end

  def get_keys() do
    :erlang.nif_error(:undefined)
  end

  def get_keys(_Val) do
    :erlang.nif_error(:undefined)
  end

  def get_module_info(_Module) do
    :erlang.nif_error(:undefined)
  end

  def group_leader() do
    :erlang.nif_error(:undefined)
  end

  def group_leader(groupLeader, pid) do
    case (case :erts_internal.group_leader(
                 groupLeader,
                 pid
               ) do
            false ->
              ref = :erlang.make_ref()
              :erts_internal.group_leader(groupLeader, pid, ref)

              receive do
                {^ref, msgRes} ->
                  msgRes
              end

            res ->
              res
          end) do
      true ->
        true

      error ->
        :erlang.error(error, [groupLeader, pid])
    end
  end

  def halt() do
    :erlang.halt(0, [])
  end

  def halt(status) do
    :erlang.halt(status, [])
  end

  def halt(_Status, _Options) do
    :erlang.nif_error(:undefined)
  end

  def has_prepared_code_on_load(_PreparedCode) do
    :erlang.nif_error(:undefined)
  end

  def hibernate(_Module, _Function, _Args) do
    :erlang.nif_error(:undefined)
  end

  def insert_element(_Index, _Tuple1, _Term) do
    :erlang.nif_error(:undefined)
  end

  def integer_to_binary(_Integer) do
    :erlang.nif_error(:undefined)
  end

  def integer_to_list(_Integer) do
    :erlang.nif_error(:undefined)
  end

  def iolist_size(_Item) do
    :erlang.nif_error(:undefined)
  end

  def iolist_to_binary(_IoListOrBinary) do
    :erlang.nif_error(:undefined)
  end

  def iolist_to_iovec(_IoListOrBinary) do
    :erlang.nif_error(:undefined)
  end

  def is_alive() do
    :erlang.nif_error(:undefined)
  end

  def is_builtin(_Module, _Function, _Arity) do
    :erlang.nif_error(:undefined)
  end

  def is_map_key(_, _) do
    :erlang.nif_error(:undef)
  end

  def is_process_alive(_Pid) do
    :erlang.nif_error(:undefined)
  end

  def length(_List) do
    :erlang.nif_error(:undefined)
  end

  def link(_PidOrPort) do
    :erlang.nif_error(:undefined)
  end

  def list_to_atom(_String) do
    :erlang.nif_error(:undefined)
  end

  def list_to_binary(_IoList) do
    :erlang.nif_error(:undefined)
  end

  def list_to_bitstring(_BitstringList) do
    :erlang.nif_error(:undefined)
  end

  def list_to_existing_atom(_String) do
    :erlang.nif_error(:undefined)
  end

  def list_to_float(_String) do
    :erlang.nif_error(:undefined)
  end

  def list_to_integer(_String) do
    :erlang.nif_error(:undefined)
  end

  def list_to_integer(_String, _Base) do
    :erlang.nif_error(:undefined)
  end

  def list_to_pid(_String) do
    :erlang.nif_error(:undefined)
  end

  def list_to_port(_String) do
    :erlang.nif_error(:undefined)
  end

  def list_to_ref(_String) do
    :erlang.nif_error(:undefined)
  end

  def list_to_tuple(_List) do
    :erlang.nif_error(:undefined)
  end

  def loaded() do
    :erlang.nif_error(:undefined)
  end

  def localtime() do
    :erlang.nif_error(:undefined)
  end

  def make_ref() do
    :erlang.nif_error(:undefined)
  end

  def map_size(_Map) do
    :erlang.nif_error(:undefined)
  end

  def map_get(_Key, _Map) do
    :erlang.nif_error(:undefined)
  end

  def match_spec_test(_P1, _P2, _P3) do
    :erlang.nif_error(:undefined)
  end

  def md5(_Data) do
    :erlang.nif_error(:undefined)
  end

  def md5_final(_Context) do
    :erlang.nif_error(:undefined)
  end

  def md5_init() do
    :erlang.nif_error(:undefined)
  end

  def md5_update(_Context, _Data) do
    :erlang.nif_error(:undefined)
  end

  def module_loaded(_Module) do
    :erlang.nif_error(:undefined)
  end

  def monitor(_Type, _Item) do
    :erlang.nif_error(:undefined)
  end

  def monitor_node(_Node, _Flag) do
    :erlang.nif_error(:undefined)
  end

  def monitor_node(_Node, _Flag, _Options) do
    :erlang.nif_error(:undefined)
  end

  def nif_error(_Reason) do
    :erlang.nif_error(:undefined)
  end

  def nif_error(_Reason, _Args) do
    :erlang.nif_error(:undefined)
  end

  def node() do
    :erlang.nif_error(:undefined)
  end

  def node(_Arg) do
    :erlang.nif_error(:undefined)
  end

  def now() do
    :erlang.nif_error(:undefined)
  end

  def phash(_Term, _Range) do
    :erlang.nif_error(:undefined)
  end

  def phash2(_Term) do
    :erlang.nif_error(:undefined)
  end

  def phash2(_Term, _Range) do
    :erlang.nif_error(:undefined)
  end

  def pid_to_list(_Pid) do
    :erlang.nif_error(:undefined)
  end

  def port_to_list(_Port) do
    :erlang.nif_error(:undefined)
  end

  def ports() do
    :erlang.nif_error(:undefined)
  end

  def posixtime_to_universaltime(_P1) do
    :erlang.nif_error(:undefined)
  end

  def unique_integer(_ModifierList) do
    :erlang.nif_error(:undefined)
  end

  def unique_integer() do
    :erlang.nif_error(:undefined)
  end

  def monotonic_time() do
    :erlang.nif_error(:undefined)
  end

  def monotonic_time(_Unit) do
    :erlang.nif_error(:undefined)
  end

  def system_time() do
    :erlang.nif_error(:undefined)
  end

  def system_time(_Unit) do
    :erlang.nif_error(:undefined)
  end

  def convert_time_unit(time, fromUnit, toUnit) do
    try do
      fU =
        case fromUnit do
          :native ->
            :erts_internal.time_unit()

          :perf_counter ->
            :erts_internal.perf_counter_unit()

          :nanosecond ->
            1000 * 1000 * 1000

          :microsecond ->
            1000 * 1000

          :millisecond ->
            1000

          :second ->
            1

          :nano_seconds ->
            1000 * 1000 * 1000

          :micro_seconds ->
            1000 * 1000

          :milli_seconds ->
            1000

          :seconds ->
            1

          _ when fromUnit > 0 ->
            fromUnit
        end

      tU =
        case toUnit do
          :native ->
            :erts_internal.time_unit()

          :perf_counter ->
            :erts_internal.perf_counter_unit()

          :nanosecond ->
            1000 * 1000 * 1000

          :microsecond ->
            1000 * 1000

          :millisecond ->
            1000

          :second ->
            1

          :nano_seconds ->
            1000 * 1000 * 1000

          :micro_seconds ->
            1000 * 1000

          :milli_seconds ->
            1000

          :seconds ->
            1

          _ when toUnit > 0 ->
            toUnit
        end

      div(
        case time < 0 do
          true ->
            tU * time - (fU - 1)

          false ->
            tU * time
        end,
        fU
      )
    catch
      _, _ ->
        :erlang.error(:badarg, [time, fromUnit, toUnit])
    end
  end

  def time_offset() do
    :erlang.nif_error(:undefined)
  end

  def time_offset(_Unit) do
    :erlang.nif_error(:undefined)
  end

  def timestamp() do
    :erlang.nif_error(:undefined)
  end

  def prepare_loading(module, <<"FOR1", _::bits>> = code) do
    prepare_loading_1(module, code)
  end

  def prepare_loading(module, code0) do
    code =
      try do
        :zlib.gunzip(code0)
      catch
        _, _ ->
          code0
      else
        decompressed ->
          decompressed
      end

    prepare_loading_1(module, code)
  end

  defp prepare_loading_1(module, code) do
    try do
      :erts_internal.prepare_loading(module, code)
    catch
      :error, reason ->
        {:EXIT, {:new_stacktrace, [{mod, _, l, loc} | rest]}} =
          try do
            :erlang.error(
              :new_stacktrace,
              [module, code]
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        :erlang.raise(:error, reason, [{mod, :prepare_loading, l, loc} | rest])
    else
      res ->
        res
    end
  end

  def pre_loaded() do
    :erlang.nif_error(:undefined)
  end

  def process_display(pid, type) do
    case (case :erts_internal.process_display(
                 pid,
                 type
               ) do
            ref when :erlang.is_reference(ref) ->
              receive do
                {^ref, res} ->
                  res
              end

            res ->
              res
          end) do
      :badarg ->
        :erlang.error(:badarg, [pid, type])

      result ->
        result
    end
  end

  def process_flag(pid, flag, value) do
    case (case :erts_internal.process_flag(pid, flag, value) do
            ref when :erlang.is_reference(ref) ->
              receive do
                {^ref, res} ->
                  res
              end

            res ->
              res
          end) do
      :badarg ->
        :erlang.error(:badarg, [pid, flag, value])

      result ->
        result
    end
  end

  def process_info(_Pid) do
    :erlang.nif_error(:undefined)
  end

  def processes() do
    :erlang.nif_error(:undefined)
  end

  def purge_module(module) when :erlang.is_atom(module) do
    case :erts_code_purger.purge(module) do
      {false, _} ->
        :erlang.error(:badarg, [module])

      {true, _} ->
        true
    end
  end

  def purge_module(arg) do
    :erlang.error(:badarg, [arg])
  end

  def put(_Key, _Val) do
    :erlang.nif_error(:undefined)
  end

  def raise(_Class, _Reason, _Stacktrace) do
    :erlang.nif_error(:undefined)
  end

  def read_timer(_TimerRef) do
    :erlang.nif_error(:undefined)
  end

  def read_timer(_TimerRef, _Options) do
    :erlang.nif_error(:undefined)
  end

  def ref_to_list(_Ref) do
    :erlang.nif_error(:undefined)
  end

  def register(_RegName, _PidOrPort) do
    :erlang.nif_error(:undefined)
  end

  def registered() do
    :erlang.nif_error(:undefined)
  end

  def resume_process(_Suspendee) do
    :erlang.nif_error(:undefined)
  end

  def round(_Number) do
    :erlang.nif_error(:undefined)
  end

  def self() do
    :erlang.nif_error(:undefined)
  end

  def send_after(_Time, _Dest, _Msg) do
    :erlang.nif_error(:undefined)
  end

  def send_after(_Time, _Dest, _Msg, _Options) do
    :erlang.nif_error(:undefined)
  end

  def seq_trace(_P1, _P2) do
    :erlang.nif_error(:undefined)
  end

  def seq_trace_print(_P1) do
    :erlang.nif_error(:undefined)
  end

  def seq_trace_print(_P1, _P2) do
    :erlang.nif_error(:undefined)
  end

  def setnode(_P1, _P2) do
    :erlang.nif_error(:undefined)
  end

  def setnode(node, distCtrlr, {_Flags, _Ver, _Creation} = opts) do
    case (case :erts_internal.create_dist_channel(node, distCtrlr, opts) do
            {:ok, dH} ->
              dH

            {:message, ref} ->
              receive do
                {^ref, res} ->
                  res
              end

            err ->
              err
          end) do
      error when :erlang.is_atom(error) ->
        :erlang.error(error, [node, distCtrlr, opts])

      dHandle ->
        dHandle
    end
  end

  def setnode(node, distCtrlr, opts) do
    :erlang.error(:badarg, [node, distCtrlr, opts])
  end

  def size(_Item) do
    :erlang.nif_error(:undefined)
  end

  def spawn(_Module, _Function, _Args) do
    :erlang.nif_error(:undefined)
  end

  def spawn_link(_Module, _Function, _Args) do
    :erlang.nif_error(:undefined)
  end

  def split_binary(_Bin, _Pos) do
    :erlang.nif_error(:undefined)
  end

  def start_timer(_Time, _Dest, _Msg) do
    :erlang.nif_error(:undefined)
  end

  def start_timer(_Time, _Dest, _Msg, _Options) do
    :erlang.nif_error(:undefined)
  end

  def suspend_process(suspendee, optList) do
    case (case :erts_internal.suspend_process(
                 suspendee,
                 optList
               ) do
            ref when :erlang.is_reference(ref) ->
              receive do
                {^ref, res} ->
                  res
              end

            res ->
              res
          end) do
      true ->
        true

      false ->
        false

      error ->
        :erlang.error(error, [suspendee, optList])
    end
  end

  def suspend_process(suspendee) do
    case (case :erts_internal.suspend_process(
                 suspendee,
                 []
               ) do
            ref when :erlang.is_reference(ref) ->
              receive do
                {^ref, res} ->
                  res
              end

            res ->
              res
          end) do
      true ->
        true

      false ->
        :erlang.error(:internal_error, [suspendee])

      error ->
        :erlang.error(error, [suspendee])
    end
  end

  def system_monitor() do
    :erlang.nif_error(:undefined)
  end

  def system_monitor(_Arg) do
    :erlang.nif_error(:undefined)
  end

  def system_monitor(_MonitorPid, _Options) do
    :erlang.nif_error(:undefined)
  end

  def system_profile() do
    :erlang.nif_error(:undefined)
  end

  def system_profile(_ProfilerPid, _Options) do
    :erlang.nif_error(:undefined)
  end

  def throw(_Any) do
    :erlang.nif_error(:undefined)
  end

  def time() do
    :erlang.nif_error(:undefined)
  end

  def trace(pidPortSpec, how, flagList) do
    case :lists.keyfind(:tracer, 1, flagList) do
      {:tracer, module, state} when :erlang.is_atom(module) ->
        case :erlang.module_loaded(module) do
          false ->
            module.enabled(:trace_status, :erlang.self(), state)

          true ->
            :ok
        end

      _ ->
        :ignore
    end

    try do
      :erts_internal.trace(pidPortSpec, how, flagList)
    catch
      e, r ->
        {_, [_ | cST]} =
          :erlang.process_info(
            :erlang.self(),
            :current_stacktrace
          )

        :erlang.raise(e, r, [
          {:erlang, :trace, [pidPortSpec, how, flagList], []}
          | cST
        ])
    else
      res ->
        res
    end
  end

  def trace_delivered(_Tracee) do
    :erlang.nif_error(:undefined)
  end

  def trace_info(_PidPortFuncEvent, _Item) do
    :erlang.nif_error(:undefined)
  end

  def trunc(_Number) do
    :erlang.nif_error(:undefined)
  end

  def tuple_size(_Tuple) do
    :erlang.nif_error(:undefined)
  end

  def universaltime() do
    :erlang.nif_error(:undefined)
  end

  def universaltime_to_posixtime(_P1) do
    :erlang.nif_error(:undefined)
  end

  def unlink(_Id) do
    :erlang.nif_error(:undefined)
  end

  def unregister(_RegName) do
    :erlang.nif_error(:undefined)
  end

  def whereis(_RegName) do
    :erlang.nif_error(:undefined)
  end

  def abs(_Number) do
    :erlang.nif_error(:undefined)
  end

  def append(_List, _Tail) do
    :erlang.nif_error(:undefined)
  end

  def element(_N, _Tuple) do
    :erlang.nif_error(:undefined)
  end

  def get_module_info(_Module, _Item) do
    :erlang.nif_error(:undefined)
  end

  def hd(_List) do
    :erlang.nif_error(:undefined)
  end

  def is_atom(_Term) do
    :erlang.nif_error(:undefined)
  end

  def is_binary(_Term) do
    :erlang.nif_error(:undefined)
  end

  def is_bitstring(_Term) do
    :erlang.nif_error(:undefined)
  end

  def is_boolean(_Term) do
    :erlang.nif_error(:undefined)
  end

  def is_float(_Term) do
    :erlang.nif_error(:undefined)
  end

  def is_function(_Term) do
    :erlang.nif_error(:undefined)
  end

  def is_function(_Term, _Arity) do
    :erlang.nif_error(:undefined)
  end

  def is_integer(_Term) do
    :erlang.nif_error(:undefined)
  end

  def is_list(_Term) do
    :erlang.nif_error(:undefined)
  end

  def is_number(_Term) do
    :erlang.nif_error(:undefined)
  end

  def is_pid(_Term) do
    :erlang.nif_error(:undefined)
  end

  def is_map(_Term) do
    :erlang.nif_error(:undefined)
  end

  def is_port(_Term) do
    :erlang.nif_error(:undefined)
  end

  def is_record(_Term, _RecordTag) do
    :erlang.nif_error(:undefined)
  end

  def is_record(_Term, _RecordTag, _Size) do
    :erlang.nif_error(:undefined)
  end

  def is_reference(_Term) do
    :erlang.nif_error(:undefined)
  end

  def is_tuple(_Term) do
    :erlang.nif_error(:undefined)
  end

  def load_module(mod, code) do
    case :erlang.prepare_loading(mod, code) do
      {:error, _} = error ->
        error

      prep when :erlang.is_reference(prep) ->
        case :erlang.finish_loading([prep]) do
          :ok ->
            {:module, mod}

          {error, [^mod]} ->
            {:error, error}
        end
    end
  end

  def load_nif(_Path, _LoadInfo) do
    :erlang.nif_error(:undefined)
  end

  def localtime_to_universaltime(_Localtime, _IsDst) do
    :erlang.nif_error(:undefined)
  end

  def make_fun(_Module, _Function, _Arity) do
    :erlang.nif_error(:undefined)
  end

  def make_tuple(_Arity, _InitialValue) do
    :erlang.nif_error(:undefined)
  end

  def make_tuple(_Arity, _DefaultValue, _InitList) do
    :erlang.nif_error(:undefined)
  end

  def nodes(_Arg) do
    :erlang.nif_error(:undefined)
  end

  def open_port(portName, portSettings) do
    case (case :erts_internal.open_port(
                 portName,
                 portSettings
               ) do
            ref when :erlang.is_reference(ref) ->
              receive do
                {^ref, res} ->
                  res
              end

            res ->
              res
          end) do
      port when :erlang.is_port(port) ->
        port

      error ->
        :erlang.error(error, [portName, portSettings])
    end
  end

  def process_flag(_Flag, _Value) do
    :erlang.nif_error(:undefined)
  end

  def process_info(_Pid, _ItemSpec) do
    :erlang.nif_error(:undefined)
  end

  def send(_Dest, _Msg) do
    :erlang.nif_error(:undefined)
  end

  def send(_Dest, _Msg, _Options) do
    :erlang.nif_error(:undefined)
  end

  def seq_trace_info(_What) do
    :erlang.nif_error(:undefined)
  end

  def setelement(_Index, _Tuple1, _Value) do
    :erlang.nif_error(:undefined)
  end

  def statistics(_Item) do
    :erlang.nif_error(:undefined)
  end

  def subtract(_, _) do
    :erlang.nif_error(:undefined)
  end

  def system_flag(_Flag, _Value) do
    :erlang.nif_error(:undefined)
  end

  def term_to_binary(_Term) do
    :erlang.nif_error(:undefined)
  end

  def term_to_binary(_Term, _Options) do
    :erlang.nif_error(:undefined)
  end

  def term_to_iovec(_Term) do
    :erlang.nif_error(:undefined)
  end

  def term_to_iovec(_Term, _Options) do
    :erlang.nif_error(:undefined)
  end

  def tl(_List) do
    :erlang.nif_error(:undefined)
  end

  def trace_pattern(mFA, matchSpec) do
    try do
      :erts_internal.trace_pattern(mFA, matchSpec, [])
    catch
      e, r ->
        {_, [_ | cST]} =
          :erlang.process_info(
            :erlang.self(),
            :current_stacktrace
          )

        :erlang.raise(e, r, [{:erlang, :trace_pattern, [mFA, matchSpec], []} | cST])
    else
      res ->
        res
    end
  end

  def trace_pattern(mFA, matchSpec, flagList) do
    case :lists.keyfind(:meta, 1, flagList) do
      {:meta, module, state} when :erlang.is_atom(module) ->
        case :erlang.module_loaded(module) do
          false ->
            module.enabled(:trace_status, :erlang.self(), state)

          true ->
            :ok
        end

      _ ->
        :ignore
    end

    try do
      :erts_internal.trace_pattern(mFA, matchSpec, flagList)
    catch
      e, r ->
        {_, [_ | cST]} =
          :erlang.process_info(
            :erlang.self(),
            :current_stacktrace
          )

        :erlang.raise(e, r, [
          {:erlang, :trace_pattern, [mFA, matchSpec, flagList], []}
          | cST
        ])
    else
      res ->
        res
    end
  end

  def tuple_to_list(_Tuple) do
    :erlang.nif_error(:undefined)
  end

  def system_info(_Item) do
    :erlang.nif_error(:undefined)
  end

  def universaltime_to_localtime(_Universaltime) do
    :erlang.nif_error(:undefined)
  end

  def apply(fun, args) do
    :erlang.apply(fun, args)
  end

  def apply(mod, name, args) do
    :erlang.apply(mod, name, args)
  end

  def spawn(f) when :erlang.is_function(f) do
    :erlang.spawn(:erlang, :apply, [f, []])
  end

  def spawn({m, f} = mF)
      when :erlang.is_atom(m) and
             :erlang.is_atom(f) do
    :erlang.spawn(:erlang, :apply, [mF, []])
  end

  def spawn(f) do
    :erlang.error(:badarg, [f])
  end

  def spawn(n, f) when n === :erlang.node() do
    :erlang.spawn(f)
  end

  def spawn(n, f) when :erlang.is_function(f) do
    :erlang.spawn(n, :erlang, :apply, [f, []])
  end

  def spawn(n, {m, f} = mF)
      when :erlang.is_atom(m) and
             :erlang.is_atom(f) do
    :erlang.spawn(n, :erlang, :apply, [mF, []])
  end

  def spawn(n, f) do
    :erlang.error(:badarg, [n, f])
  end

  def spawn_link(f) when :erlang.is_function(f) do
    :erlang.spawn_link(:erlang, :apply, [f, []])
  end

  def spawn_link({m, f} = mF)
      when :erlang.is_atom(m) and
             :erlang.is_atom(f) do
    :erlang.spawn_link(:erlang, :apply, [mF, []])
  end

  def spawn_link(f) do
    :erlang.error(:badarg, [f])
  end

  def spawn_link(n, f) when n === :erlang.node() do
    spawn_link(f)
  end

  def spawn_link(n, f) when :erlang.is_function(f) do
    :erlang.spawn_link(n, :erlang, :apply, [f, []])
  end

  def spawn_link(n, {m, f} = mF)
      when :erlang.is_atom(m) and
             :erlang.is_atom(f) do
    :erlang.spawn_link(n, :erlang, :apply, [mF, []])
  end

  def spawn_link(n, f) do
    :erlang.error(:badarg, [n, f])
  end

  def spawn_monitor(f) when :erlang.is_function(f, 0) do
    :erlang.spawn_opt(:erlang, :apply, [f, []], [:monitor])
  end

  def spawn_monitor(f) do
    :erlang.error(:badarg, [f])
  end

  def spawn_monitor(node, f)
      when :erlang.is_atom(node) and
             :erlang.is_function(f, 0) do
    try do
      :erlang.spawn_monitor(node, :erlang, :apply, [f, []])
    catch
      :error, err ->
        :erlang.error(err, [node, f])
    end
  end

  def spawn_monitor(node, f) do
    :erlang.error(:badarg, [node, f])
  end

  def spawn_monitor(m, f, a)
      when :erlang.is_atom(m) and
             :erlang.is_atom(f) and :erlang.is_list(a) do
    :erlang.spawn_opt(m, f, a, [:monitor])
  end

  def spawn_monitor(m, f, a) do
    :erlang.error(:badarg, [m, f, a])
  end

  def spawn_opt(f, o) when :erlang.is_function(f) do
    :erlang.spawn_opt(:erlang, :apply, [f, []], o)
  end

  def spawn_opt({m, f} = mF, o)
      when :erlang.is_atom(m) and
             :erlang.is_atom(f) do
    :erlang.spawn_opt(:erlang, :apply, [mF, []], o)
  end

  def spawn_opt(f, o) do
    :erlang.error(:badarg, [f, o])
  end

  def spawn_opt(n, f, o) when n === :erlang.node() do
    :erlang.spawn_opt(f, o)
  end

  def spawn_opt(n, f, o) when :erlang.is_function(f, 0) do
    :erlang.spawn_opt(n, :erlang, :apply, [f, []], o)
  end

  def spawn_opt(n, {m, f} = mF, o)
      when :erlang.is_atom(m) and
             :erlang.is_atom(f) do
    :erlang.spawn_opt(n, :erlang, :apply, [mF, []], o)
  end

  def spawn_opt(n, f, o) do
    :erlang.error(:badarg, [n, f, o])
  end

  def spawn(n, m, f, a)
      when n === :erlang.node() and
             :erlang.is_atom(m) and :erlang.is_atom(f) and
             :erlang.is_list(a) do
    :erlang.spawn(m, f, a)
  end

  def spawn(n, m, f, a)
      when :erlang.is_atom(n) and
             :erlang.is_atom(m) and :erlang.is_atom(f) do
    try do
      :erlang.spawn_opt(n, m, f, a, [])
    catch
      _, reason ->
        :erlang.error(reason, [n, m, f, a])
    end
  end

  def spawn(n, m, f, a) do
    :erlang.error(:badarg, [n, m, f, a])
  end

  def spawn_link(n, m, f, a)
      when n === :erlang.node() and
             :erlang.is_atom(m) and :erlang.is_atom(f) and
             :erlang.is_list(a) do
    :erlang.spawn_link(m, f, a)
  end

  def spawn_link(n, m, f, a)
      when :erlang.is_atom(n) and
             :erlang.is_atom(m) and :erlang.is_atom(f) do
    try do
      :erlang.spawn_opt(n, m, f, a, [:link])
    catch
      _, reason ->
        :erlang.error(reason, [n, m, f, a])
    end
  end

  def spawn_link(n, m, f, a) do
    :erlang.error(:badarg, [n, m, f, a])
  end

  def spawn_monitor(n, m, f, a)
      when n === :erlang.node() and
             :erlang.is_atom(m) and :erlang.is_atom(f) and
             :erlang.is_list(a) do
    try do
      :erlang.spawn_monitor(m, f, a)
    catch
      :error, err ->
        :erlang.error(err, [n, m, f, a])
    end
  end

  def spawn_monitor(n, m, f, a)
      when :erlang.is_atom(n) and
             :erlang.is_atom(m) and :erlang.is_atom(f) do
    ref =
      try do
        :erlang.spawn_request(n, m, f, a, [:monitor])
      catch
        :error, err0 ->
          :erlang.error(err0, [n, m, f, a])
      end

    receive do
      {:spawn_reply, ^ref, :ok, pid} when :erlang.is_pid(pid) ->
        {pid, ref}

      {:spawn_reply, ^ref, :error, :badopt} ->
        :erlang.error(:badarg, [n, m, f, a])

      {:spawn_reply, ^ref, :error, :noconnection} ->
        try do
          :erlang.spawn_opt(:erts_internal, :crasher, [n, m, f, a, [:monitor], :noconnection], [
            :monitor
          ])
        catch
          _, err1 ->
            :erlang.error(err1, [n, m, f, a])
        end

      {:spawn_reply, ^ref, :error, err2} ->
        :erlang.error(err2, [n, m, f, a])
    end
  end

  def spawn_monitor(n, m, f, a) do
    :erlang.error(:badarg, [n, m, f, a])
  end

  def spawn_opt(_Module, _Function, _Args, _Options) do
    :erlang.nif_error(:undefined)
  end

  def spawn_opt(n, m, f, a, o)
      when n === :erlang.node() and
             :erlang.is_atom(m) and :erlang.is_atom(f) and
             :erlang.is_list(a) and :erlang.is_list(o) do
    :erlang.spawn_opt(m, f, a, o)
  end

  def spawn_opt(n, m, f, a, o)
      when :erlang.is_atom(n) and
             :erlang.is_atom(m) and :erlang.is_atom(f) do
    {ref, monOpt} =
      case :erts_internal.dist_spawn_request(n, {m, f, a}, o, :spawn_opt) do
        {r, mO} when :erlang.is_reference(r) ->
          {r, mO}

        :badarg ->
          :erlang.error(:badarg, [n, m, f, a, o])
      end

    receive do
      {:spawn_reply, ^ref, :ok, pid} when :erlang.is_pid(pid) ->
        case monOpt do
          true ->
            {pid, ref}

          false ->
            pid
        end

      {:spawn_reply, ^ref, :error, :badopt} ->
        :erlang.error(:badarg, [n, m, f, a, o])

      {:spawn_reply, ^ref, :error, :noconnection} ->
        try do
          :erlang.spawn_opt(:erts_internal, :crasher, [n, m, f, a, o, :noconnection], o)
        catch
          _, err1 ->
            :erlang.error(err1, [n, m, f, a, o])
        end

      {:spawn_reply, ^ref, :error, :notsup} ->
        case old_remote_spawn_opt(n, m, f, a, o) do
          pid when :erlang.is_pid(pid) ->
            pid

          err2 ->
            :erlang.error(err2, [n, m, f, a, o])
        end

      {:spawn_reply, ^ref, :error, err3} ->
        :erlang.error(err3, [n, m, f, a, o])
    end
  end

  def spawn_opt(n, m, f, a, o) do
    :erlang.error(:badarg, [n, m, f, a, o])
  end

  defp old_remote_spawn_opt(n, m, f, a, o) do
    case :lists.member(:monitor, o) do
      true ->
        :badarg

      _ ->
        {l, nO} =
          :lists.foldl(
            fn
              :link, {_, newOpts} ->
                {:link, newOpts}

              opt, {lO, newOpts} ->
                {lO, [opt | newOpts]}
            end,
            {:no_link, []},
            o
          )

        case (try do
                :gen_server.call(
                  {:net_kernel, n},
                  {:spawn_opt, m, f, a, nO, l, :erlang.group_leader()},
                  :infinity
                )
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          pid when :erlang.is_pid(pid) ->
            pid

          error ->
            case remote_spawn_error(error, {l, n, m, f, a, nO}) do
              {:fault, fault} ->
                fault

              pid ->
                pid
            end
        end
    end
  end

  defp remote_spawn_error(
         {:EXIT, {{:nodedown, n}, _}},
         {l, n, m, f, a, o}
       ) do
    {opts, lL} =
      case l === :link do
        true ->
          {[:link | o], [:link]}

        false ->
          {o, []}
      end

    :erlang.spawn_opt(:erts_internal, :crasher, [n, m, f, a, opts, :noconnection], lL)
  end

  defp remote_spawn_error({:EXIT, {reason, _}}, _) do
    {:fault, reason}
  end

  defp remote_spawn_error({:EXIT, reason}, _) do
    {:fault, reason}
  end

  defp remote_spawn_error(other, _) do
    {:fault, other}
  end

  def spawn_request(f) when :erlang.is_function(f, 0) do
    try do
      :erlang.spawn_request(:erlang, :apply, [f, []], [])
    catch
      :error, err ->
        :erlang.error(err, [f])
    end
  end

  def spawn_request(f) do
    :erlang.error(:badarg, [f])
  end

  def spawn_request(f, o) when :erlang.is_function(f, 0) do
    try do
      :erlang.spawn_request(:erlang, :apply, [f, []], o)
    catch
      :error, err ->
        :erlang.error(err, [f, o])
    end
  end

  def spawn_request(n, f) when :erlang.is_function(f, 0) do
    try do
      :erlang.spawn_request(n, :erlang, :apply, [f, []], [])
    catch
      :error, err ->
        :erlang.error(err, [n, f])
    end
  end

  def spawn_request(a1, a2) do
    :erlang.error(:badarg, [a1, a2])
  end

  def spawn_request(n, f, o) when :erlang.is_function(f, 0) do
    try do
      :erlang.spawn_request(n, :erlang, :apply, [f, []], o)
    catch
      :error, err ->
        :erlang.error(err, [n, f, o])
    end
  end

  def spawn_request(m, f, a) do
    try do
      :erlang.spawn_request(m, f, a, [])
    catch
      :error, err ->
        :erlang.error(err, [m, f, a])
    end
  end

  def spawn_request(n, m, f, a) when :erlang.is_atom(f) do
    try do
      :erlang.spawn_request(n, m, f, a, [])
    catch
      :error, err ->
        :erlang.error(err, [n, m, f, a])
    end
  end

  def spawn_request(m, f, a, o) do
    case :erts_internal.spawn_request(m, f, a, o) do
      ref when :erlang.is_reference(ref) ->
        ref

      :badarg ->
        :erlang.error(:badarg, [m, f, a, o])
    end
  end

  def spawn_request(n, m, f, a, o) when n === :erlang.node() do
    try do
      :erlang.spawn_request(m, f, a, o)
    catch
      :error, err ->
        :erlang.error(err, [n, m, f, a, o])
    end
  end

  def spawn_request(n, m, f, a, o) do
    case :erts_internal.dist_spawn_request(n, {m, f, a}, o, :spawn_request) do
      ref when :erlang.is_reference(ref) ->
        ref

      :badarg ->
        :erlang.error(:badarg, [n, m, f, a, o])
    end
  end

  def spawn_request_abandon(_ReqId) do
    :erlang.nif_error(:undefined)
  end

  def yield() do
    :erlang.yield()
  end

  def nodes() do
    :erlang.nodes(:visible)
  end

  def disconnect_node(node) do
    :net_kernel.disconnect(node)
  end

  def fun_info(fun) when :erlang.is_function(fun) do
    keys = [:type, :env, :arity, :name, :uniq, :index, :new_uniq, :new_index, :module, :pid]
    fun_info_1(keys, fun, [])
  end

  defp fun_info_1([k | ks], fun, a) do
    case :erlang.fun_info(fun, k) do
      {^k, :undefined} ->
        fun_info_1(ks, fun, a)

      {^k, _} = p ->
        fun_info_1(ks, fun, [p | a])
    end
  end

  defp fun_info_1([], _, a) do
    a
  end

  def send_nosuspend(pid, msg) do
    send_nosuspend(pid, msg, [])
  end

  def send_nosuspend(pid, msg, opts) do
    case :erlang.send(pid, msg, [:nosuspend | opts]) do
      :ok ->
        true

      _ ->
        false
    end
  end

  def localtime_to_universaltime(localtime) do
    :erlang.localtime_to_universaltime(
      localtime,
      :undefined
    )
  end

  def port_command(port, data) do
    case (case :erts_internal.port_command(port, data, []) do
            ref when :erlang.is_reference(ref) ->
              receive do
                {^ref, res} ->
                  res
              end

            res ->
              res
          end) do
      true ->
        true

      error ->
        :erlang.error(error, [port, data])
    end
  end

  def port_command(port, data, flags) do
    case (case :erts_internal.port_command(port, data, flags) do
            ref when :erlang.is_reference(ref) ->
              receive do
                {^ref, res} ->
                  res
              end

            res ->
              res
          end) do
      bool when bool == true or bool == false ->
        bool

      error ->
        :erlang.error(error, [port, data, flags])
    end
  end

  def port_connect(port, pid) do
    case (case :erts_internal.port_connect(port, pid) do
            ref when :erlang.is_reference(ref) ->
              receive do
                {^ref, res} ->
                  res
              end

            res ->
              res
          end) do
      true ->
        true

      error ->
        :erlang.error(error, [port, pid])
    end
  end

  def port_close(port) do
    case (case :erts_internal.port_close(port) do
            ref when :erlang.is_reference(ref) ->
              receive do
                {^ref, res} ->
                  res
              end

            res ->
              res
          end) do
      true ->
        true

      error ->
        :erlang.error(error, [port])
    end
  end

  def port_control(port, operation, data) do
    case (case :erts_internal.port_control(port, operation, data) do
            ref when :erlang.is_reference(ref) ->
              receive do
                {^ref, res} ->
                  res
              end

            res ->
              res
          end) do
      :badarg ->
        :erlang.error(:badarg, [port, operation, data])

      result ->
        result
    end
  end

  def port_call(port, data) do
    case (case :erts_internal.port_call(port, 0, data) do
            ref when :erlang.is_reference(ref) ->
              receive do
                {^ref, res} ->
                  res
              end

            res ->
              res
          end) do
      {:ok, result} ->
        result

      error ->
        :erlang.error(error, [port, data])
    end
  end

  def port_call(port, operation, data) do
    case (case :erts_internal.port_call(port, operation, data) do
            ref when :erlang.is_reference(ref) ->
              receive do
                {^ref, res} ->
                  res
              end

            res ->
              res
          end) do
      {:ok, result} ->
        result

      error ->
        :erlang.error(error, [port, operation, data])
    end
  end

  def port_info(port) do
    case (case :erts_internal.port_info(port) do
            ref when :erlang.is_reference(ref) ->
              receive do
                {^ref, res} ->
                  res
              end

            res ->
              res
          end) do
      :badarg ->
        :erlang.error(:badarg, [port])

      result ->
        result
    end
  end

  def port_info(port, item) do
    case (case :erts_internal.port_info(port, item) do
            ref when :erlang.is_reference(ref) ->
              receive do
                {^ref, res} ->
                  res
              end

            res ->
              res
          end) do
      :badarg ->
        :erlang.error(:badarg, [port, item])

      result ->
        result
    end
  end

  def port_set_data(_Port, _Data) do
    :erlang.nif_error(:undefined)
  end

  def port_get_data(_Port) do
    :erlang.nif_error(:undefined)
  end

  def dist_ctrl_input_handler(_DHandle, _InputHandler) do
    :erlang.nif_error(:undefined)
  end

  def dist_ctrl_put_data(_DHandle, _Data) do
    :erlang.nif_error(:undefined)
  end

  def dist_ctrl_get_data(_DHandle) do
    :erlang.nif_error(:undefined)
  end

  def dist_ctrl_get_data_notification(_DHandle) do
    :erlang.nif_error(:undefined)
  end

  def dist_ctrl_set_opt(_DHandle, _Opt, _Val) do
    :erlang.nif_error(:undefined)
  end

  def dist_ctrl_get_opt(_DHandle, _Opt) do
    :erlang.nif_error(:undefined)
  end

  def dist_get_stat(_DHandle) do
    :erlang.nif_error(:undefined)
  end

  def dmonitor_node(node, _Flag, []) do
    send(:erlang.self(), {:nodedown, node})
    true
  end

  def dmonitor_node(node, flag, opts) do
    case :lists.member(:allow_passive_connect, opts) do
      true ->
        case :net_kernel.passive_cnct(node) do
          true ->
            :erlang.monitor_node(node, flag, opts)

          false ->
            send(:erlang.self(), {:nodedown, node})
            true
        end

      _ ->
        dmonitor_node(node, flag, [])
    end
  end

  def delay_trap(result, 0) do
    :erlang.yield()
    result
  end

  def delay_trap(result, timeout) do
    receive do
    after
      timeout ->
        result
    end
  end

  def set_cookie(node, c)
      when node !== :nonode@nohost and
             :erlang.is_atom(node) do
    case :erlang.is_atom(c) do
      true ->
        :auth.set_cookie(node, c)

      false ->
        :erlang.error(:badarg)
    end
  end

  def get_cookie() do
    :auth.get_cookie()
  end

  def integer_to_list(_I, _Base) do
    :erlang.nif_error(:undefined)
  end

  def integer_to_binary(_I, _Base) do
    :erlang.nif_error(:undefined)
  end

  Record.defrecord(:r_cpu, :cpu,
    node: -1,
    processor: -1,
    processor_node: -1,
    core: -1,
    thread: -1,
    logical: -1
  )

  def set_cpu_topology(cpuTopology) do
    try do
      format_cpu_topology(
        :erlang.system_flag(
          :internal_cpu_topology,
          cput_e2i(cpuTopology)
        )
      )
    catch
      class, exception
      when class !== :error or
             exception !== :internal_error ->
        :erlang.error(:badarg, [cpuTopology])
    end
  end

  defp cput_e2i_clvl({:logical, _}, _PLvl) do
    r_cpu(:logical)
  end

  defp cput_e2i_clvl([e | _], pLvl) do
    case :erlang.element(1, e) do
      :node ->
        case pLvl do
          0 ->
            r_cpu(:node)

          r_cpu(:processor) ->
            r_cpu(:processor_node)
        end

      :processor ->
        case pLvl do
          0 ->
            r_cpu(:node)

          r_cpu(:node) ->
            r_cpu(:processor)
        end

      :core ->
        r_cpu(:core)

      :thread ->
        r_cpu(:thread)
    end
  end

  defp cput_e2i(:undefined) do
    :undefined
  end

  defp cput_e2i(e) do
    rvrs(cput_e2i(e, -1, -1, r_cpu(), 0, cput_e2i_clvl(e, 0), []))
  end

  defp cput_e2i([], _NId, _PId, _IS, _PLvl, _Lvl, res) do
    res
  end

  defp cput_e2i([e | es], nId0, pId, iS, pLvl, lvl, res0) do
    case cput_e2i(e, nId0, pId, iS, pLvl, lvl, res0) do
      [] ->
        cput_e2i(es, nId0, pId, iS, pLvl, lvl, res0)

      [
        r_cpu(node: n, processor: p, processor_node: pN) = cPU
        | _
      ] = res1 ->
        nId1 =
          case n > pN do
            true ->
              n

            false ->
              pN
          end

        cput_e2i(es, nId1, p, cPU, pLvl, lvl, res1)
    end
  end

  defp cput_e2i({tag, [], tagList}, nid, pId, cPU, pLvl, lvl, res) do
    cput_e2i({tag, tagList}, nid, pId, cPU, pLvl, lvl, res)
  end

  defp cput_e2i({:node, nL}, nid0, pId, _CPU, 0, r_cpu(:node), res) do
    nid1 = nid0 + 1
    lvl = cput_e2i_clvl(nL, r_cpu(:node))
    cput_e2i(nL, nid1, pId, r_cpu(node: nid1), r_cpu(:node), lvl, res)
  end

  defp cput_e2i({:processor, pL}, nid, pId0, _CPU, 0, r_cpu(:node), res) do
    pId1 = pId0 + 1
    lvl = cput_e2i_clvl(pL, r_cpu(:processor))
    cput_e2i(pL, nid, pId1, r_cpu(processor: pId1), r_cpu(:processor), lvl, res)
  end

  defp cput_e2i({:processor, pL}, nid, pId0, cPU, pLvl, cLvl, res)
       when pLvl < r_cpu(:processor) and cLvl <= r_cpu(:processor) do
    pId1 = pId0 + 1
    lvl = cput_e2i_clvl(pL, r_cpu(:processor))

    cput_e2i(
      pL,
      nid,
      pId1,
      r_cpu(cPU, processor: pId1, processor_node: -1, core: -1, thread: -1),
      r_cpu(:processor),
      lvl,
      res
    )
  end

  defp cput_e2i({:node, nL}, nid0, pId, cPU, r_cpu(:processor), r_cpu(:processor_node), res) do
    nid1 = nid0 + 1
    lvl = cput_e2i_clvl(nL, r_cpu(:processor_node))
    cput_e2i(nL, nid1, pId, r_cpu(cPU, processor_node: nid1), r_cpu(:processor_node), lvl, res)
  end

  defp cput_e2i({:core, cL}, nid, pId, r_cpu(core: c0) = cPU, pLvl, r_cpu(:core), res)
       when pLvl < r_cpu(:core) do
    lvl = cput_e2i_clvl(cL, r_cpu(:core))
    cput_e2i(cL, nid, pId, r_cpu(cPU, core: c0 + 1, thread: -1), r_cpu(:core), lvl, res)
  end

  defp cput_e2i({:thread, tL}, nid, pId, r_cpu(thread: t0) = cPU, pLvl, r_cpu(:thread), res)
       when pLvl < r_cpu(:thread) do
    lvl = cput_e2i_clvl(tL, r_cpu(:thread))
    cput_e2i(tL, nid, pId, r_cpu(cPU, thread: t0 + 1), r_cpu(:thread), lvl, res)
  end

  defp cput_e2i(
         {:logical, iD},
         _Nid,
         pId,
         r_cpu(processor: p, core: c, thread: t) = cPU,
         pLvl,
         r_cpu(:logical),
         res
       )
       when pLvl < r_cpu(:logical) and :erlang.is_integer(iD) and
              0 <= iD and iD < 65536 do
    [
      r_cpu(cPU,
        processor:
          case p do
            -1 ->
              pId + 1

            _ ->
              p
          end,
        core:
          case c do
            -1 ->
              0

            _ ->
              c
          end,
        thread:
          case t do
            -1 ->
              0

            _ ->
              t
          end,
        logical: iD
      )
      | res
    ]
  end

  def format_cpu_topology(internalCpuTopology) do
    try do
      cput_i2e(internalCpuTopology)
    catch
      _, _ ->
        :erlang.error(:internal_error, [internalCpuTopology])
    end
  end

  defp cput_i2e(:undefined) do
    :undefined
  end

  defp cput_i2e(is) do
    cput_i2e(is, true, r_cpu(:node), cput_i2e_tag_map())
  end

  defp cput_i2e([], _Frst, _Lvl, _TM) do
    []
  end

  defp cput_i2e([r_cpu(logical: lID) | _], _Frst, lvl, _TM)
       when lvl == r_cpu(:logical) do
    {:logical, lID}
  end

  defp cput_i2e([r_cpu() = i | is], frst, lvl, tM) do
    cput_i2e(:erlang.element(lvl, i), frst, is, [i], lvl, tM)
  end

  defp cput_i2e(v, frst, [i | is], sameV, lvl, tM)
       when v === :erlang.element(lvl, i) do
    cput_i2e(v, frst, is, [i | sameV], lvl, tM)
  end

  defp cput_i2e(-1, true, [], sameV, lvl, tM) do
    cput_i2e(rvrs(sameV), true, lvl + 1, tM)
  end

  defp cput_i2e(_V, true, [], sameV, lvl, tM)
       when lvl !== r_cpu(:processor) and
              lvl !== r_cpu(:processor_node) do
    cput_i2e(rvrs(sameV), true, lvl + 1, tM)
  end

  defp cput_i2e(-1, _Frst, is, sameV, r_cpu(:node), tM) do
    cput_i2e(rvrs(sameV), true, r_cpu(:processor), tM) ++ cput_i2e(is, false, r_cpu(:node), tM)
  end

  defp cput_i2e(_V, _Frst, is, sameV, lvl, tM) do
    [
      {cput_i2e_tag(lvl, tM), cput_i2e(rvrs(sameV), true, lvl + 1, tM)}
      | cput_i2e(is, false, lvl, tM)
    ]
  end

  defp cput_i2e_tag_map() do
    :erlang.list_to_tuple([:cpu | Keyword.keys(r_cpu(r_cpu()))])
  end

  defp cput_i2e_tag(lvl, tM) do
    case :erlang.element(lvl, tM) do
      :processor_node ->
        :node

      other ->
        other
    end
  end

  defp rvrs([_] = l) do
    l
  end

  defp rvrs(xs) do
    rvrs(xs, [])
  end

  defp rvrs([], ys) do
    ys
  end

  defp rvrs([x | xs], ys) do
    rvrs(xs, [x | ys])
  end

  def min(a, b) when a > b do
    b
  end

  def min(a, _) do
    a
  end

  def max(a, b) when a < b do
    b
  end

  def max(a, _) do
    a
  end

  Record.defrecord(:r_memory, :memory,
    total: 0,
    processes: 0,
    processes_used: 0,
    system: 0,
    atom: 0,
    atom_used: 0,
    binary: 0,
    code: 0,
    ets: 0
  )

  def memory() do
    case aa_mem_data(au_mem_data(:erlang.system_info(:alloc_util_allocators) -- [:mseg_alloc])) do
      :notsup ->
        :erlang.error(:notsup)

      mem ->
        [
          {:total, r_memory(mem, :total)},
          {:processes, r_memory(mem, :processes)},
          {:processes_used, r_memory(mem, :processes_used)},
          {:system, r_memory(mem, :system)},
          {:atom, r_memory(mem, :atom)},
          {:atom_used, r_memory(mem, :atom_used)},
          {:binary, r_memory(mem, :binary)},
          {:code, r_memory(mem, :code)},
          {:ets, r_memory(mem, :ets)}
        ]
    end
  end

  def memory(type) when :erlang.is_atom(type) do
    try do
      case aa_mem_data(au_mem_data(:erlang.system_info(:alloc_util_allocators) -- [:mseg_alloc])) do
        :notsup ->
          :erlang.error(:notsup)

        mem ->
          get_memval(type, mem)
      end
    catch
      :error, :badarg ->
        :erlang.error(:badarg)
    end
  end

  def memory(types) when :erlang.is_list(types) do
    try do
      case aa_mem_data(au_mem_data(:erlang.system_info(:alloc_util_allocators) -- [:mseg_alloc])) do
        :notsup ->
          :erlang.error(:notsup)

        mem ->
          memory_1(types, mem)
      end
    catch
      :error, :badarg ->
        :erlang.error(:badarg)
    end
  end

  defp memory_1([type | types], mem) do
    [{type, get_memval(type, mem)} | memory_1(types, mem)]
  end

  defp memory_1([], _Mem) do
    []
  end

  defp get_memval(:total, r_memory(total: v)) do
    v
  end

  defp get_memval(:processes, r_memory(processes: v)) do
    v
  end

  defp get_memval(:processes_used, r_memory(processes_used: v)) do
    v
  end

  defp get_memval(:system, r_memory(system: v)) do
    v
  end

  defp get_memval(:atom, r_memory(atom: v)) do
    v
  end

  defp get_memval(:atom_used, r_memory(atom_used: v)) do
    v
  end

  defp get_memval(:binary, r_memory(binary: v)) do
    v
  end

  defp get_memval(:code, r_memory(code: v)) do
    v
  end

  defp get_memval(:ets, r_memory(ets: v)) do
    v
  end

  defp get_memval(_, r_memory()) do
    :erlang.error(:badarg)
  end

  defp get_fix_proc([{procType, a1, u1} | rest], {a0, u0})
       when procType == :proc or procType == :monitor or
              procType == :link or procType == :msg_ref or
              procType == :ll_ptimer or procType == :hl_ptimer or
              procType == :bif_timer or
              procType == :accessor_bif_timer do
    get_fix_proc(rest, {a0 + a1, u0 + u1})
  end

  defp get_fix_proc([_ | rest], acc) do
    get_fix_proc(rest, acc)
  end

  defp get_fix_proc([], acc) do
    acc
  end

  defp fix_proc([{:fix_types, sizeList} | _Rest], acc) do
    get_fix_proc(sizeList, acc)
  end

  defp fix_proc([{:fix_types, mask, sizeList} | _Rest], acc) do
    {a, u} = get_fix_proc(sizeList, acc)
    {mask, a, u}
  end

  defp fix_proc([_ | rest], acc) do
    fix_proc(rest, acc)
  end

  defp fix_proc([], acc) do
    acc
  end

  defp au_mem_fix(
         r_memory(processes: proc, processes_used: procU, system: sys) = mem,
         data
       ) do
    case fix_proc(data, {0, 0}) do
      {a, u} ->
        r_memory(mem, processes: proc + a, processes_used: procU + u, system: sys - a)

      {mask, a, u} ->
        r_memory(mem,
          processes: mask &&& proc + a,
          processes_used: mask &&& procU + u,
          system: mask &&& sys - a
        )
    end
  end

  defp au_mem_acc(
         r_memory(total: tot, processes: proc, processes_used: procU) = mem,
         :eheap_alloc,
         data
       ) do
    sz = acc_blocks_size(data, 0)
    r_memory(mem, total: tot + sz, processes: proc + sz, processes_used: procU + sz)
  end

  defp au_mem_acc(r_memory(total: tot, system: sys, ets: ets) = mem, :ets_alloc, data) do
    sz = acc_blocks_size(data, 0)
    r_memory(mem, total: tot + sz, system: sys + sz, ets: ets + sz)
  end

  defp au_mem_acc(r_memory(total: tot, system: sys, binary: bin) = mem, :binary_alloc, data) do
    sz = acc_blocks_size(data, 0)
    r_memory(mem, total: tot + sz, system: sys + sz, binary: bin + sz)
  end

  defp au_mem_acc(r_memory(total: tot, system: sys) = mem, _Type, data) do
    sz = acc_blocks_size(data, 0)
    r_memory(mem, total: tot + sz, system: sys + sz)
  end

  defp acc_blocks_size([{:size, sz, _, _} | rest], acc) do
    acc_blocks_size(rest, acc + sz)
  end

  defp acc_blocks_size([{:size, sz} | rest], acc) do
    acc_blocks_size(rest, acc + sz)
  end

  defp acc_blocks_size([_ | rest], acc) do
    acc_blocks_size(rest, acc)
  end

  defp acc_blocks_size([], acc) do
    acc
  end

  defp au_mem_blocks([{:blocks, l} | rest], mem0) do
    mem = au_mem_blocks_1(l, mem0)
    au_mem_blocks(rest, mem)
  end

  defp au_mem_blocks([_ | rest], mem) do
    au_mem_blocks(rest, mem)
  end

  defp au_mem_blocks([], mem) do
    mem
  end

  defp au_mem_blocks_1([{type, sizeList} | rest], mem) do
    au_mem_blocks_1(rest, au_mem_acc(mem, type, sizeList))
  end

  defp au_mem_blocks_1([], mem) do
    mem
  end

  defp au_mem_current(mem, type, [{:mbcs_pool, stats} | rest]) do
    au_mem_current(au_mem_blocks(stats, mem), type, rest)
  end

  defp au_mem_current(mem, type, [{:mbcs, stats} | rest]) do
    au_mem_current(au_mem_blocks(stats, mem), type, rest)
  end

  defp au_mem_current(mem, type, [{:sbcs, stats} | rest]) do
    au_mem_current(au_mem_blocks(stats, mem), type, rest)
  end

  defp au_mem_current(mem, type, [_ | rest]) do
    au_mem_current(mem, type, rest)
  end

  defp au_mem_current(mem, _Type, []) do
    mem
  end

  defp au_mem_data(:notsup, _) do
    :notsup
  end

  defp au_mem_data(_, [{_, false} | _]) do
    :notsup
  end

  defp au_mem_data(r_memory() = mem0, [{:fix_alloc, _, data} | rest]) do
    mem = au_mem_fix(mem0, data)
    au_mem_data(au_mem_current(mem, :fix_alloc, data), rest)
  end

  defp au_mem_data(r_memory() = mem, [{type, _, data} | rest]) do
    au_mem_data(au_mem_current(mem, type, data), rest)
  end

  defp au_mem_data(eMD, []) do
    eMD
  end

  defp au_mem_data(allocs) do
    ref = :erlang.make_ref()
    :erlang.system_info({:memory_internal, ref, allocs})
    receive_emd(ref)
  end

  defp receive_emd(_Ref, eMD, 0) do
    eMD
  end

  defp receive_emd(ref, eMD, n) do
    receive do
      {^ref, _, data} ->
        receive_emd(ref, au_mem_data(eMD, data), n - 1)
    end
  end

  defp receive_emd(ref) do
    receive_emd(ref, r_memory(), :erlang.system_info(:schedulers))
  end

  defp aa_mem_data(r_memory() = mem, [{:total, tot} | rest]) do
    aa_mem_data(r_memory(mem, total: tot, system: 0), rest)
  end

  defp aa_mem_data(
         r_memory(atom: atom, atom_used: atomU) = mem,
         [{:atom_space, alloced, used} | rest]
       ) do
    aa_mem_data(
      r_memory(mem,
        atom: atom + alloced,
        atom_used: atomU + used
      ),
      rest
    )
  end

  defp aa_mem_data(
         r_memory(atom: atom, atom_used: atomU) = mem,
         [{:atom_table, sz} | rest]
       ) do
    aa_mem_data(
      r_memory(mem,
        atom: atom + sz,
        atom_used: atomU + sz
      ),
      rest
    )
  end

  defp aa_mem_data(r_memory(ets: ets) = mem, [{:ets_misc, sz} | rest]) do
    aa_mem_data(r_memory(mem, ets: ets + sz), rest)
  end

  defp aa_mem_data(
         r_memory(processes: proc, processes_used: procU, system: sys) = mem,
         [{procData, sz} | rest]
       )
       when procData == :bif_timer or
              procData == :process_table do
    aa_mem_data(
      r_memory(mem, processes: proc + sz, processes_used: procU + sz, system: sys - sz),
      rest
    )
  end

  defp aa_mem_data(r_memory(code: code) = mem, [{codeData, sz} | rest])
       when codeData == :module_table or
              codeData == :export_table or codeData == :export_list or
              codeData == :fun_table or codeData == :module_refs or
              codeData == :loaded_code do
    aa_mem_data(r_memory(mem, code: code + sz), rest)
  end

  defp aa_mem_data(eMD, [{_, _} | rest]) do
    aa_mem_data(eMD, rest)
  end

  defp aa_mem_data(
         r_memory(total: tot, processes: proc, system: sys) = mem,
         []
       )
       when sys <= 0 do
    r_memory(mem, system: tot - proc)
  end

  defp aa_mem_data(eMD, []) do
    eMD
  end

  defp aa_mem_data(:notsup) do
    :notsup
  end

  defp aa_mem_data(eMD) do
    aa_mem_data(eMD, :erlang.system_info(:allocated_areas))
  end

  def alloc_info(allocs) do
    get_alloc_info(:allocator, allocs)
  end

  def alloc_sizes(allocs) do
    get_alloc_info(:allocator_sizes, allocs)
  end

  defp get_alloc_info(type, aAtom) when :erlang.is_atom(aAtom) do
    [{^aAtom, result}] = get_alloc_info(type, [aAtom])
    result
  end

  defp get_alloc_info(type, aList) when :erlang.is_list(aList) do
    ref = :erlang.make_ref()
    :erlang.system_info({type, ref, aList})
    receive_allocator(ref, :erlang.system_info(:schedulers), mk_res_list(aList))
  end

  defp mk_res_list([]) do
    []
  end

  defp mk_res_list([alloc | rest]) do
    [{alloc, []} | mk_res_list(rest)]
  end

  defp insert_instance(i, n, rest) when :erlang.is_atom(n) do
    [{n, i} | rest]
  end

  defp insert_instance(i, n, []) do
    [{:instance, n, i}]
  end

  defp insert_instance(i, n, [{:instance, m, _} | _] = rest)
       when n < m do
    [{:instance, n, i} | rest]
  end

  defp insert_instance(i, n, [prev | rest]) do
    [prev | insert_instance(i, n, rest)]
  end

  defp insert_info([], ys) do
    ys
  end

  defp insert_info([{a, false} | xs], [{a, _IList} | ys]) do
    insert_info(xs, [{a, false} | ys])
  end

  defp insert_info([{a, n, i} | xs], [{a, iList} | ys]) do
    insert_info(
      xs,
      [{a, insert_instance(i, n, iList)} | ys]
    )
  end

  defp insert_info([{a1, _} | _] = xs, [{a2, _} = y | ys])
       when a1 != a2 do
    [y | insert_info(xs, ys)]
  end

  defp insert_info([{a1, _, _} | _] = xs, [{a2, _} = y | ys])
       when a1 != a2 do
    [y | insert_info(xs, ys)]
  end

  defp receive_allocator(_Ref, 0, acc) do
    acc
  end

  defp receive_allocator(ref, n, acc) do
    receive do
      {^ref, _, infoList} ->
        receive_allocator(ref, n - 1, insert_info(infoList, acc))
    end
  end

  def gather_gc_info_result(ref) when :erlang.is_reference(ref) do
    gc_info(ref, :erlang.system_info(:schedulers), {0, 0})
  end

  defp gc_info(_Ref, 0, {colls, recl}) do
    {colls, recl, 0}
  end

  defp gc_info(ref, n, {origColls, origRecl}) do
    receive do
      {^ref, {_, colls, recl}} ->
        gc_info(ref, n - 1, {colls + origColls, recl + origRecl})
    end
  end

  def unquote(:==)(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def unquote(:"=:=")(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def unquote(:"/=")(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def unquote(:"=/=")(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def unquote(:"=<")(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def unquote(:>=)(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def unquote(:<)(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def unquote(:>)(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def unquote(:-)(_A) do
    :erlang.nif_error(:undefined)
  end

  def unquote(:+)(_A) do
    :erlang.nif_error(:undefined)
  end

  def unquote(:-)(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def unquote(:+)(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def unquote(:/)(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def unquote(:*)(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def div(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def rem(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def bsl(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def bsr(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def bor(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def band(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def bxor(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def bnot(_A) do
    :erlang.nif_error(:undefined)
  end

  def unquote(:--)(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def unquote(:++)(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def unquote(:and)(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def unquote(:or)(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def xor(_A, _B) do
    :erlang.nif_error(:undefined)
  end

  def unquote(:not)(_A) do
    :erlang.nif_error(:undefined)
  end

  def unquote(:!)(_Dst, _Msg) do
    :erlang.nif_error(:undefined)
  end
end
