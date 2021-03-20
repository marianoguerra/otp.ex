defmodule :cowboy_stream_h do
  use Bitwise
  @behaviour :cowboy_stream
  require Record
  Record.defrecord(:r_state, :state, next: :undefined,
                                 ref: :undefined, pid: :undefined,
                                 expect: :undefined, read_body_pid: :undefined,
                                 read_body_ref: :undefined,
                                 read_body_timer_ref: :undefined,
                                 read_body_length: 0, read_body_is_fin: :nofin,
                                 read_body_buffer: <<>>, body_length: 0,
                                 stream_body_pid: :undefined,
                                 stream_body_status: :normal)
  def init(streamID, req = %{ref: ref}, opts) do
    env = :maps.get(:env, opts, %{})
    middlewares = :maps.get(:middlewares, opts,
                              [:cowboy_router, :cowboy_handler])
    shutdown = :maps.get(:shutdown_timeout, opts, 5000)
    pid = :proc_lib.spawn_link(:cowboy_stream_h,
                                 :request_process, [req, env, middlewares])
    expect = expect(req)
    {commands, next} = :cowboy_stream.init(streamID, req,
                                             opts)
    {[{:spawn, pid, shutdown} | commands],
       r_state(next: next, ref: ref, pid: pid, expect: expect)}
  end

  defp expect(%{version: :"HTTP/1.0"}) do
    :undefined
  end

  defp expect(req) do
    try do
      :cowboy_req.parse_header("expect", req)
    catch
      _, _ ->
        :undefined
    else
      expect ->
        expect
    end
  end

  def data(streamID, isFin, data,
           state = r_state(read_body_ref: :undefined,
                       read_body_buffer: buffer, body_length: bodyLen)) do
    do_data(streamID, isFin, data, [],
              r_state(state, expect: :undefined,  read_body_is_fin: isFin, 
                         read_body_buffer: <<buffer :: binary,
                                               data :: binary>>, 
                         body_length: bodyLen + byte_size(data)))
  end

  def data(streamID, isFin, data,
           state = r_state(read_body_pid: pid, read_body_ref: ref,
                       read_body_length: :auto, body_length: bodyLen)) do
    send_request_body(pid, ref, isFin, bodyLen, data)
    do_data(streamID, isFin, data,
              [{:flow, byte_size(data)}],
              r_state(state, read_body_ref: :undefined, 
                         body_length: bodyLen))
  end

  def data(streamID, isFin = :nofin, data,
           state = r_state(read_body_length: readLen,
                       read_body_buffer: buffer, body_length: bodyLen))
      when byte_size(data) + byte_size(buffer) < readLen do
    do_data(streamID, isFin, data, [],
              r_state(state, expect: :undefined, 
                         read_body_buffer: <<buffer :: binary,
                                               data :: binary>>, 
                         body_length: bodyLen + byte_size(data)))
  end

  def data(streamID, isFin, data,
           state = r_state(read_body_pid: pid, read_body_ref: ref,
                       read_body_timer_ref: tRef, read_body_buffer: buffer,
                       body_length: bodyLen0)) do
    bodyLen = bodyLen0 + byte_size(data)
    :ok = :erlang.cancel_timer(tRef,
                                 [{:async, true}, {:info, false}])
    send_request_body(pid, ref, isFin, bodyLen,
                        <<buffer :: binary, data :: binary>>)
    do_data(streamID, isFin, data, [],
              r_state(state, expect: :undefined, 
                         read_body_ref: :undefined, 
                         read_body_timer_ref: :undefined, 
                         read_body_buffer: <<>>,  body_length: bodyLen))
  end

  defp do_data(streamID, isFin, data, commands1,
            state = r_state(next: next0)) do
    {commands2, next} = :cowboy_stream.data(streamID, isFin,
                                              data, next0)
    {commands1 ++ commands2, r_state(state, next: next)}
  end

  def info(streamID, info = {:EXIT, pid, :normal},
           state = r_state(pid: pid)) do
    do_info(streamID, info, [:stop], state)
  end

  def info(streamID,
           info = {:EXIT, pid,
                     {{:request_error, reason, _HumanReadable}, _}},
           state = r_state(pid: pid)) do
    status = (case (reason) do
                :timeout ->
                  408
                :payload_too_large ->
                  413
                _ ->
                  400
              end)
    do_info(streamID, info,
              [{:error_response, status, %{"content-length" => "0"}, <<>>}, :stop],
              state)
  end

  def info(streamID,
           exit = {:EXIT, pid, {reason, stacktrace}},
           state = r_state(ref: ref, pid: pid)) do
    commands0 = [{:internal_error, exit, :"Stream process crashed."}]
    commands = (case (reason) do
                  :normal ->
                    commands0
                  :shutdown ->
                    commands0
                  {:shutdown, _} ->
                    commands0
                  _ ->
                    [{:log, :error, 'Ranch listener ~p, connection process ~p, stream ~p had its request process ~p exit with reason ~999999p and stacktrace ~999999p~n',
                        [ref, self(), streamID, pid, reason, stacktrace]} |
                         commands0]
                end)
    do_info(streamID, exit,
              [{:error_response, 500, %{"content-length" => "0"}, <<>>} | commands],
              state)
  end

  def info(streamID,
           info = {:read_body, pid, ref, :auto, :infinity},
           state = r_state(read_body_buffer: <<>>)) do
    do_info(streamID, info, [],
              r_state(state, read_body_pid: pid,  read_body_ref: ref, 
                         read_body_length: :auto))
  end

  def info(streamID,
           info = {:read_body, pid, ref, :auto, :infinity},
           state = r_state(read_body_is_fin: isFin,
                       read_body_buffer: buffer, body_length: bodyLen)) do
    send_request_body(pid, ref, isFin, bodyLen, buffer)
    do_info(streamID, info, [{:flow, byte_size(buffer)}],
              r_state(state, read_body_buffer: <<>>))
  end

  def info(streamID,
           info = {:read_body, pid, ref, length, _},
           state = r_state(read_body_is_fin: isFin,
                       read_body_buffer: buffer, body_length: bodyLen))
      when isFin === :fin or byte_size(buffer) >= length do
    send_request_body(pid, ref, isFin, bodyLen, buffer)
    do_info(streamID, info, [],
              r_state(state, read_body_buffer: <<>>))
  end

  def info(streamID,
           info = {:read_body, pid, ref, length, period},
           state = r_state(expect: expect)) do
    commands = (case (expect) do
                  :continue ->
                    [{:inform, 100, %{}}, {:flow, length}]
                  :undefined ->
                    [{:flow, length}]
                end)
    tRef = :erlang.send_after(period, self(),
                                {{self(), streamID}, {:read_body_timeout, ref}})
    do_info(streamID, info, commands,
              r_state(state, read_body_pid: pid,  read_body_ref: ref, 
                         read_body_timer_ref: tRef,  read_body_length: length))
  end

  def info(streamID, info = {:read_body_timeout, ref},
           state = r_state(read_body_pid: pid, read_body_ref: ref,
                       read_body_is_fin: isFin, read_body_buffer: buffer,
                       body_length: bodyLen)) do
    send_request_body(pid, ref, isFin, bodyLen, buffer)
    do_info(streamID, info, [],
              r_state(state, read_body_ref: :undefined, 
                         read_body_timer_ref: :undefined, 
                         read_body_buffer: <<>>))
  end

  def info(streamID, info = {:read_body_timeout, _},
           state) do
    do_info(streamID, info, [], state)
  end

  def info(streamID, inform = {:inform, status, _},
           state0) do
    state = (case (:cow_http.status_to_integer(status)) do
               100 ->
                 r_state(state0, expect: :undefined)
               _ ->
                 state0
             end)
    do_info(streamID, inform, [inform], state)
  end

  def info(streamID, response = {:response, _, _, _},
           state) do
    do_info(streamID, response, [response],
              r_state(state, expect: :undefined))
  end

  def info(streamID, headers = {:headers, _, _}, state) do
    do_info(streamID, headers, [headers],
              r_state(state, expect: :undefined))
  end

  def info(streamID, data = {:data, _, _}, state) do
    do_info(streamID, data, [data], state)
  end

  def info(streamID, data0 = {:data, pid, _, _},
           state0 = r_state(stream_body_status: status)) do
    state = (case (status) do
               :normal ->
                 send(pid, {:data_ack, self()})
                 state0
               :blocking ->
                 r_state(state0, stream_body_pid: pid, 
                             stream_body_status: :blocked)
               :blocked ->
                 state0
             end)
    data = :erlang.delete_element(2, data0)
    do_info(streamID, data, [data], state)
  end

  def info(streamID, alarm = {:alarm, name, :on},
           state0 = r_state(stream_body_status: status))
      when name === :connection_buffer_full or
             name === :stream_buffer_full do
    state = (case (status) do
               :normal ->
                 r_state(state0, stream_body_status: :blocking)
               _ ->
                 state0
             end)
    do_info(streamID, alarm, [], state)
  end

  def info(streamID, alarm = {:alarm, name, :off},
           state = r_state(stream_body_pid: pid,
                       stream_body_status: status))
      when name === :connection_buffer_full or
             name === :stream_buffer_full do
    _ = (case (status) do
           :normal ->
             :ok
           :blocking ->
             :ok
           :blocked ->
             send(pid, {:data_ack, self()})
         end)
    do_info(streamID, alarm, [],
              r_state(state, stream_body_pid: :undefined, 
                         stream_body_status: :normal))
  end

  def info(streamID, trailers = {:trailers, _}, state) do
    do_info(streamID, trailers, [trailers], state)
  end

  def info(streamID, push = {:push, _, _, _, _, _, _, _},
           state) do
    do_info(streamID, push, [push], state)
  end

  def info(streamID,
           switchProtocol = {:switch_protocol, _, _, _}, state) do
    do_info(streamID, switchProtocol, [switchProtocol],
              r_state(state, expect: :undefined))
  end

  def info(streamID, setOptions = {:set_options, _},
           state) do
    do_info(streamID, setOptions, [setOptions], state)
  end

  def info(streamID, info, state) do
    do_info(streamID, info, [], state)
  end

  defp do_info(streamID, info, commands1,
            state0 = r_state(next: next0)) do
    {commands2, next} = :cowboy_stream.info(streamID, info,
                                              next0)
    {commands1 ++ commands2, r_state(state0, next: next)}
  end

  def terminate(streamID, reason, r_state(next: next)) do
    :cowboy_stream.terminate(streamID, reason, next)
  end

  def early_error(streamID, reason, partialReq, resp, opts) do
    :cowboy_stream.early_error(streamID, reason, partialReq,
                                 resp, opts)
  end

  defp send_request_body(pid, ref, :nofin, _, data) do
    send(pid, {:request_body, ref, :nofin, data})
    :ok
  end

  defp send_request_body(pid, ref, :fin, bodyLen, data) do
    send(pid, {:request_body, ref, :fin, bodyLen, data})
    :ok
  end

  def request_process(req, env, middlewares) do
    try do
      execute(req, env, middlewares)
    catch
      :exit, reason ->
        :erlang.raise(:exit, {reason, __STACKTRACE__},
                        __STACKTRACE__)
    end
  end

  defp execute(_, _, []) do
    :ok
  end

  defp execute(req, env, [middleware | tail]) do
    case (middleware.execute(req, env)) do
      {:ok, req2, env2} ->
        execute(req2, env2, tail)
      {:suspend, module, function, args} ->
        :proc_lib.hibernate(:cowboy_stream_h, :resume,
                              [env, tail, module, function, args])
      {:stop, _Req2} ->
        :ok
    end
  end

  def resume(env, tail, module, function, args) do
    case (apply(module, function, args)) do
      {:ok, req2, env2} ->
        execute(req2, env2, tail)
      {:suspend, module2, function2, args2} ->
        :proc_lib.hibernate(:cowboy_stream_h, :resume,
                              [env, tail, module2, function2, args2])
      {:stop, _Req2} ->
        :ok
    end
  end

end