defmodule :m_dyntrace do
  use Bitwise
  @on_load :on_load
  defp on_load() do
    privDir = :code.priv_dir(:runtime_tools)
    libName = 'dyntrace'
    lib = :filename.join([privDir, 'lib', libName])

    status =
      case :erlang.load_nif(lib, 0) do
        :ok ->
          :ok

        {:error, {:load_failed, _}} = error1 ->
          archLibDir = :filename.join([privDir, 'lib', :erlang.system_info(:system_architecture)])
          candidate = :filelib.wildcard(:filename.join([archLibDir, libName ++ '*']))

          case candidate do
            [] ->
              error1

            _ ->
              archLib = :filename.join([archLibDir, libName])
              :erlang.load_nif(archLib, 0)
          end

        error1 ->
          error1
      end

    case status do
      :ok ->
        :ok

      {:error, {e, str}} ->
        case :erlang.system_info(:dynamic_trace) do
          :none ->
            :ok

          _ ->
            :error_logger.error_msg(
              'Unable to load dyntrace library. Failed with error:~n\n"~p, ~s"~nDynamic tracing is enabled but the driver is not built correctly~n',
              [e, str]
            )

            status
        end
    end
  end

  def available() do
    :erlang.nif_error(:nif_not_loaded)
  end

  def user_trace_s1(_Message) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def user_trace_i4s4(_, _, _, _, _, _, _, _, _) do
    :erlang.nif_error(:nif_not_loaded)
  end

  defp user_trace_n(_, _, _, _, _, _, _, _, _, _) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def trace(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def trace_procs(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def trace_ports(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def trace_running_procs(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def trace_running_ports(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def trace_call(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def trace_send(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def trace_receive(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def trace_garbage_collection(_TraceTag, _TracerState, _Tracee, _TraceTerm, _Opts) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def enabled(_TraceTag, _TracerState, _Tracee) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def enabled_procs(_TraceTag, _TracerState, _Tracee) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def enabled_ports(_TraceTag, _TracerState, _Tracee) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def enabled_running_procs(_TraceTag, _TracerState, _Tracee) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def enabled_running_ports(_TraceTag, _TracerState, _Tracee) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def enabled_call(_TraceTag, _TracerState, _Tracee) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def enabled_send(_TraceTag, _TracerState, _Tracee) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def enabled_receive(_TraceTag, _TracerState, _Tracee) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def enabled_garbage_collection(_TraceTag, _TracerState, _Tracee) do
    :erlang.nif_error(:nif_not_loaded)
  end

  def p() do
    user_trace_int(:undef, :undef, :undef, :undef, :undef, :undef, :undef, :undef)
  end

  def p(i1) when is_integer(i1) do
    user_trace_int(i1, :undef, :undef, :undef, :undef, :undef, :undef, :undef)
  end

  def p(s1) do
    user_trace_int(:undef, :undef, :undef, :undef, s1, :undef, :undef, :undef)
  end

  def p(i1, i2)
      when is_integer(i1) and
             is_integer(i2) do
    user_trace_int(i1, i2, :undef, :undef, :undef, :undef, :undef, :undef)
  end

  def p(i1, s1) when is_integer(i1) do
    user_trace_int(i1, :undef, :undef, :undef, s1, :undef, :undef, :undef)
  end

  def p(s1, s2) do
    user_trace_int(:undef, :undef, :undef, :undef, s1, s2, :undef, :undef)
  end

  def p(i1, i2, i3)
      when is_integer(i1) and
             is_integer(i2) and is_integer(i3) do
    user_trace_int(i1, i2, i3, :undef, :undef, :undef, :undef, :undef)
  end

  def p(i1, i2, s1)
      when is_integer(i1) and
             is_integer(i2) do
    user_trace_int(i1, i2, :undef, :undef, s1, :undef, :undef, :undef)
  end

  def p(i1, s1, s2) when is_integer(i1) do
    user_trace_int(i1, :undef, :undef, :undef, s1, s2, :undef, :undef)
  end

  def p(s1, s2, s3) do
    user_trace_int(:undef, :undef, :undef, :undef, s1, s2, s3, :undef)
  end

  def p(i1, i2, i3, i4)
      when is_integer(i1) and
             is_integer(i2) and is_integer(i3) and
             is_integer(i4) do
    user_trace_int(i1, i2, i3, i4, :undef, :undef, :undef, :undef)
  end

  def p(i1, i2, i3, s1)
      when is_integer(i1) and
             is_integer(i2) and is_integer(i3) do
    user_trace_int(i1, i2, i3, :undef, s1, :undef, :undef, :undef)
  end

  def p(i1, i2, s1, s2)
      when is_integer(i1) and
             is_integer(i2) do
    user_trace_int(i1, i2, :undef, :undef, s1, s2, :undef, :undef)
  end

  def p(i1, s1, s2, s3) when is_integer(i1) do
    user_trace_int(i1, :undef, :undef, :undef, s1, s2, s3, :undef)
  end

  def p(s1, s2, s3, s4) do
    user_trace_int(:undef, :undef, :undef, :undef, s1, s2, s3, s4)
  end

  def p(i1, i2, i3, i4, s1)
      when is_integer(i1) and
             is_integer(i2) and is_integer(i3) and
             is_integer(i4) do
    user_trace_int(i1, i2, i3, i4, s1, :undef, :undef, :undef)
  end

  def p(i1, i2, i3, s1, s2)
      when is_integer(i1) and
             is_integer(i2) and is_integer(i3) do
    user_trace_int(i1, i2, i3, :undef, s1, s2, :undef, :undef)
  end

  def p(i1, i2, s1, s2, s3)
      when is_integer(i1) and
             is_integer(i2) do
    user_trace_int(i1, i2, :undef, :undef, s1, s2, s3, :undef)
  end

  def p(i1, s1, s2, s3, s4) when is_integer(i1) do
    user_trace_int(i1, :undef, :undef, :undef, s1, s2, s3, s4)
  end

  def p(i1, i2, i3, i4, s1, s2)
      when is_integer(i1) and
             is_integer(i2) and is_integer(i3) and
             is_integer(i4) do
    user_trace_int(i1, i2, i3, i4, s1, s2, :undef, :undef)
  end

  def p(i1, i2, i3, s1, s2, s3)
      when is_integer(i1) and
             is_integer(i2) and is_integer(i3) do
    user_trace_int(i1, i2, i3, :undef, s1, s2, s3, :undef)
  end

  def p(i1, i2, s1, s2, s3, s4)
      when is_integer(i1) and
             is_integer(i2) do
    user_trace_int(i1, i2, :undef, :undef, s1, s2, s3, s4)
  end

  def p(i1, i2, i3, i4, s1, s2, s3)
      when is_integer(i1) and is_integer(i2) and
             is_integer(i3) and is_integer(i4) do
    user_trace_int(i1, i2, i3, i4, s1, s2, s3, :undef)
  end

  def p(i1, i2, i3, s1, s2, s3, s4)
      when is_integer(i1) and is_integer(i2) and
             is_integer(i3) do
    user_trace_int(i1, i2, i3, :undef, s1, s2, s3, s4)
  end

  def p(i1, i2, i3, i4, s1, s2, s3, s4)
      when is_integer(i1) and is_integer(i2) and
             is_integer(i3) and is_integer(i4) do
    user_trace_int(i1, i2, i3, i4, s1, s2, s3, s4)
  end

  defp user_trace_int(i1, i2, i3, i4, s1, s2, s3, s4) do
    uTag = get_tag()

    try do
      user_trace_i4s4(uTag, i1, i2, i3, i4, s1, s2, s3, s4)
    catch
      :error, :nif_not_loaded ->
        false
    end
  end

  def pn(probeLabel) do
    user_trace_n_int(probeLabel, :undef, :undef, :undef, :undef, :undef, :undef, :undef, :undef)
  end

  def pn(probeLabel, i1) when is_integer(i1) do
    user_trace_n_int(probeLabel, i1, :undef, :undef, :undef, :undef, :undef, :undef, :undef)
  end

  def pn(probeLabel, s1) do
    user_trace_n_int(probeLabel, :undef, :undef, :undef, :undef, s1, :undef, :undef, :undef)
  end

  def pn(probeLabel, i1, i2)
      when is_integer(i1) and
             is_integer(i2) do
    user_trace_n_int(probeLabel, i1, i2, :undef, :undef, :undef, :undef, :undef, :undef)
  end

  def pn(probeLabel, i1, s1) when is_integer(i1) do
    user_trace_n_int(probeLabel, i1, :undef, :undef, :undef, s1, :undef, :undef, :undef)
  end

  def pn(probeLabel, s1, s2) do
    user_trace_n_int(probeLabel, :undef, :undef, :undef, :undef, s1, s2, :undef, :undef)
  end

  def pn(probeLabel, i1, i2, i3)
      when is_integer(i1) and
             is_integer(i2) and is_integer(i3) do
    user_trace_n_int(probeLabel, i1, i2, i3, :undef, :undef, :undef, :undef, :undef)
  end

  def pn(probeLabel, i1, i2, s1)
      when is_integer(i1) and
             is_integer(i2) do
    user_trace_n_int(probeLabel, i1, i2, :undef, :undef, s1, :undef, :undef, :undef)
  end

  def pn(probeLabel, i1, s1, s2) when is_integer(i1) do
    user_trace_n_int(probeLabel, i1, :undef, :undef, :undef, s1, s2, :undef, :undef)
  end

  def pn(probeLabel, s1, s2, s3) do
    user_trace_n_int(probeLabel, :undef, :undef, :undef, :undef, s1, s2, s3, :undef)
  end

  def pn(probeLabel, i1, i2, i3, i4)
      when is_integer(i1) and is_integer(i2) and
             is_integer(i3) and is_integer(i4) do
    user_trace_n_int(probeLabel, i1, i2, i3, i4, :undef, :undef, :undef, :undef)
  end

  def pn(probeLabel, i1, i2, i3, s1)
      when is_integer(i1) and is_integer(i2) and
             is_integer(i3) do
    user_trace_n_int(probeLabel, i1, i2, i3, :undef, s1, :undef, :undef, :undef)
  end

  def pn(probeLabel, i1, i2, s1, s2)
      when is_integer(i1) and is_integer(i2) do
    user_trace_n_int(probeLabel, i1, i2, :undef, :undef, s1, s2, :undef, :undef)
  end

  def pn(probeLabel, i1, s1, s2, s3)
      when is_integer(i1) do
    user_trace_n_int(probeLabel, i1, :undef, :undef, :undef, s1, s2, s3, :undef)
  end

  def pn(probeLabel, s1, s2, s3, s4) do
    user_trace_n_int(probeLabel, :undef, :undef, :undef, :undef, s1, s2, s3, s4)
  end

  def pn(probeLabel, i1, i2, i3, i4, s1)
      when is_integer(i1) and is_integer(i2) and
             is_integer(i3) and is_integer(i4) do
    user_trace_n_int(probeLabel, i1, i2, i3, i4, s1, :undef, :undef, :undef)
  end

  def pn(probeLabel, i1, i2, i3, s1, s2)
      when is_integer(i1) and is_integer(i2) and
             is_integer(i3) do
    user_trace_n_int(probeLabel, i1, i2, i3, :undef, s1, s2, :undef, :undef)
  end

  def pn(probeLabel, i1, i2, s1, s2, s3)
      when is_integer(i1) and is_integer(i2) do
    user_trace_n_int(probeLabel, i1, i2, :undef, :undef, s1, s2, s3, :undef)
  end

  def pn(probeLabel, i1, s1, s2, s3, s4)
      when is_integer(i1) do
    user_trace_n_int(probeLabel, i1, :undef, :undef, :undef, s1, s2, s3, s4)
  end

  def pn(probeLabel, i1, i2, i3, i4, s1, s2)
      when is_integer(i1) and is_integer(i2) and
             is_integer(i3) and is_integer(i4) do
    user_trace_n_int(probeLabel, i1, i2, i3, i4, s1, s2, :undef, :undef)
  end

  def pn(probeLabel, i1, i2, i3, s1, s2, s3)
      when is_integer(i1) and is_integer(i2) and
             is_integer(i3) do
    user_trace_n_int(probeLabel, i1, i2, i3, :undef, s1, s2, s3, :undef)
  end

  def pn(probeLabel, i1, i2, s1, s2, s3, s4)
      when is_integer(i1) and is_integer(i2) do
    user_trace_n_int(probeLabel, i1, i2, :undef, :undef, s1, s2, s3, s4)
  end

  def pn(probeLabel, i1, i2, i3, i4, s1, s2, s3)
      when is_integer(i1) and is_integer(i2) and
             is_integer(i3) and is_integer(i4) do
    user_trace_n_int(probeLabel, i1, i2, i3, i4, s1, s2, s3, :undef)
  end

  def pn(probeLabel, i1, i2, i3, s1, s2, s3, s4)
      when is_integer(i1) and is_integer(i2) and
             is_integer(i3) do
    user_trace_n_int(probeLabel, i1, i2, i3, :undef, s1, s2, s3, s4)
  end

  def pn(probeLabel, i1, i2, i3, i4, s1, s2, s3, s4)
      when is_integer(i1) and is_integer(i2) and
             is_integer(i3) and is_integer(i4) do
    user_trace_n_int(probeLabel, i1, i2, i3, i4, s1, s2, s3, s4)
  end

  defp user_trace_n_int(probeLabel, i1, i2, i3, i4, s1, s2, s3, s4) do
    uTag = get_tag()

    try do
      user_trace_n(probeLabel, uTag, i1, i2, i3, i4, s1, s2, s3, s4)
    catch
      :error, :nif_not_loaded ->
        false
    end
  end

  def put_tag(data) do
    :erlang.dt_put_tag(:unicode.characters_to_binary(data))
  end

  def get_tag() do
    :erlang.dt_get_tag()
  end

  def get_tag_data() do
    :erlang.dt_get_tag_data()
  end

  def spread_tag(b) do
    :erlang.dt_spread_tag(b)
  end

  def restore_tag(t) do
    :erlang.dt_restore_tag(t)
  end
end
