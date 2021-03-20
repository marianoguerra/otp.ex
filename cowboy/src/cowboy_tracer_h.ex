defmodule :cowboy_tracer_h do
  use Bitwise
  @behaviour :cowboy_stream
  def init(streamID, req, opts) do
    init_tracer(streamID, req, opts)
    :cowboy_stream.init(streamID, req, opts)
  end

  def data(streamID, isFin, data, next) do
    :cowboy_stream.data(streamID, isFin, data, next)
  end

  def info(streamID, info, next) do
    :cowboy_stream.info(streamID, info, next)
  end

  def terminate(streamID, reason, next) do
    :cowboy_stream.terminate(streamID, reason, next)
  end

  def early_error(streamID, reason, partialReq, resp, opts) do
    :cowboy_stream.early_error(streamID, reason, partialReq,
                                 resp, opts)
  end

  def set_trace_patterns() do
    :erlang.trace_pattern({:_, :_, :_},
                            [{:_, [], [{:return_trace}]}], [:local])
    :erlang.trace_pattern(:on_load,
                            [{:_, [], [{:return_trace}]}], [:local])
    :ok
  end

  defp init_tracer(streamID, req,
            opts = %{tracer_match_specs: list,
                       tracer_callback: _}) do
    case (match(list, streamID, req, opts)) do
      false ->
        :ok
      true ->
        start_tracer(streamID, req, opts)
    end
  end

  defp init_tracer(_, _, _) do
    :ok
  end

  defp match([], _, _, _) do
    true
  end

  defp match([predicate | tail], streamID, req, opts)
      when is_function(predicate) do
    case (predicate.(streamID, req, opts)) do
      true ->
        match(tail, streamID, req, opts)
      false ->
        false
    end
  end

  defp match([{:method, value} | tail], streamID,
            req = %{method: value}, opts) do
    match(tail, streamID, req, opts)
  end

  defp match([{:host, value} | tail], streamID,
            req = %{host: value}, opts) do
    match(tail, streamID, req, opts)
  end

  defp match([{:path, value} | tail], streamID,
            req = %{path: value}, opts) do
    match(tail, streamID, req, opts)
  end

  defp match([{:path_start, pathStart} | tail], streamID,
            req = %{path: path}, opts) do
    len = byte_size(pathStart)
    case (path) do
      <<^pathStart :: size(len) - binary, _ :: bits>> ->
        match(tail, streamID, req, opts)
      _ ->
        false
    end
  end

  defp match([{:header, name} | tail], streamID,
            req = %{headers: headers}, opts) do
    case (headers) do
      %{^name => _} ->
        match(tail, streamID, req, opts)
      _ ->
        false
    end
  end

  defp match([{:header, name, value} | tail], streamID,
            req = %{headers: headers}, opts) do
    case (headers) do
      %{^name => ^value} ->
        match(tail, streamID, req, opts)
      _ ->
        false
    end
  end

  defp match([{:peer_ip, iP} | tail], streamID,
            req = %{peer: {iP, _}}, opts) do
    match(tail, streamID, req, opts)
  end

  defp match(_, _, _, _) do
    false
  end

  defp start_tracer(streamID, req, opts) do
    case (:erlang.trace_info(self(), :tracer)) do
      {:tracer, []} ->
        tracerPid = :proc_lib.spawn_link(:cowboy_tracer_h,
                                           :tracer_process,
                                           [streamID, req, opts])
        flags = :maps.get(:tracer_flags, opts,
                            [:send, :receive, :call, :return_to, :procs, :ports,
                                                                             :monotonic_timestamp,
                                                                                 :set_on_spawn])
        :erlang.trace(self(), true,
                        [{:tracer, tracerPid} | flags])
        :ok
      _ ->
        :ok
    end
  end

  def tracer_process(streamID, req = %{pid: parent},
           opts = %{tracer_callback: fun}) do
    :erlang.process_flag(:trap_exit, true)
    state = fun.(:init, {streamID, req, opts})
    tracer_loop(parent, opts, state)
  end

  defp tracer_loop(parent, opts = %{tracer_callback: fun},
            state0) do
    receive do
      msg when :erlang.element(1, msg) === :trace_ts ->
        state = fun.(msg, state0)
        tracer_loop(parent, opts, state)
      {:EXIT, ^parent, reason} ->
        tracer_terminate(reason, opts, state0)
      {:system, from, request} ->
        :sys.handle_system_msg(request, from, parent,
                                 :cowboy_tracer_h, [], {opts, state0})
      msg ->
        :cowboy.log(:warning, '~p: Tracer process received stray message ~9999p~n', [:cowboy_tracer_h, msg], opts)
        tracer_loop(parent, opts, state0)
    end
  end

  defp tracer_terminate(reason, %{tracer_callback: fun}, state) do
    _ = fun.(:terminate, state)
    exit(reason)
  end

  def system_continue(parent, _, {opts, state}) do
    tracer_loop(parent, opts, state)
  end

  def system_terminate(reason, _, _, {opts, state}) do
    tracer_terminate(reason, opts, state)
  end

  def system_code_change(misc, _, _, _) do
    {:ok, misc}
  end

end