defmodule :cowboy_websocket do
  use Bitwise
  @behaviour :cowboy_sub_protocol
  require Record

  Record.defrecord(:r_state, :state,
    parent: :undefined,
    ref: :undefined,
    socket: :undefined,
    transport: :undefined,
    opts: %{},
    active: true,
    handler: :undefined,
    key: :undefined,
    timeout_ref: :undefined,
    messages: :undefined,
    hibernate: false,
    frag_state: :undefined,
    frag_buffer: <<>>,
    utf8_state: :undefined,
    deflate: true,
    extensions: %{},
    req: %{},
    shutdown_reason: :normal
  )

  def is_upgrade_request(%{:version => :"HTTP/2", :method => "CONNECT", :protocol => protocol}) do
    "websocket" === :cowboy_bstr.to_lower(protocol)
  end

  def is_upgrade_request(req = %{:version => :"HTTP/1.1", :method => "GET"}) do
    connTokens = :cowboy_req.parse_header("connection", req, [])

    case :lists.member("upgrade", connTokens) do
      false ->
        false

      true ->
        upgradeTokens = :cowboy_req.parse_header("upgrade", req)
        :lists.member("websocket", upgradeTokens)
    end
  end

  def is_upgrade_request(_) do
    false
  end

  def upgrade(req, env, handler, handlerState) do
    upgrade(req, env, handler, handlerState, %{})
  end

  def upgrade(req0 = %{:version => version}, env, handler, handlerState, opts) do
    filteredReq =
      case :maps.get(:req_filter, opts, :undefined) do
        :undefined ->
          :maps.with(
            [:method, :version, :scheme, :host, :port, :path, :qs, :peer],
            req0
          )

        filterFun ->
          filterFun.(req0)
      end

    utf8State =
      case :maps.get(:validate_utf8, opts, true) do
        true ->
          0

        false ->
          :undefined
      end

    state0 = r_state(opts: opts, handler: handler, utf8_state: utf8State, req: filteredReq)

    try do
      websocket_upgrade(state0, req0)
    catch
      _, _ ->
        {:ok, :cowboy_req.reply(400, req0), env}
    else
      {:ok, state, req} ->
        websocket_handshake(state, req, handlerState, env)

      {:error, :upgrade_required} when version === :"HTTP/1.1" ->
        {:ok,
         :cowboy_req.reply(426, %{"connection" => "upgrade", "upgrade" => "websocket"}, req0),
         env}

      {:error, :upgrade_required} ->
        {:ok, :cowboy_req.reply(400, req0), env}
    end
  end

  defp websocket_upgrade(state, req = %{:version => version}) do
    case is_upgrade_request(req) do
      false ->
        {:error, :upgrade_required}

      true when version === :"HTTP/1.1" ->
        key = :cowboy_req.header("sec-websocket-key", req)
        false = key === :undefined
        websocket_version(r_state(state, key: key), req)

      true ->
        websocket_version(state, req)
    end
  end

  defp websocket_version(state, req) do
    wsVersion = :cowboy_req.parse_header("sec-websocket-version", req)

    case wsVersion do
      7 ->
        :ok

      8 ->
        :ok

      13 ->
        :ok
    end

    websocket_extensions(
      state,
      %{req | :websocket_version => wsVersion}
    )
  end

  defp websocket_extensions(state = r_state(opts: opts), req) do
    compress = :maps.get(:compress, opts, false)

    case {compress, :cowboy_req.parse_header("sec-websocket-extensions", req)} do
      {true, extensions} when extensions !== :undefined ->
        websocket_extensions(state, req, extensions, [])

      _ ->
        {:ok, state, req}
    end
  end

  defp websocket_extensions(state, req, [], []) do
    {:ok, state, req}
  end

  defp websocket_extensions(state, req, [], [", " | respHeader]) do
    {:ok, state,
     :cowboy_req.set_resp_header("sec-websocket-extensions", :lists.reverse(respHeader), req)}
  end

  defp websocket_extensions(
         state = r_state(opts: opts, extensions: extensions),
         req = %{:pid => pid, :version => version},
         [{"permessage-deflate", params} | tail],
         respHeader
       ) do
    deflateOpts0 = :maps.get(:deflate_opts, opts, %{})

    deflateOpts =
      case version do
        :"HTTP/1.1" ->
          %{deflateOpts0 | :owner => pid}

        _ ->
          deflateOpts0
      end

    try do
      :cow_ws.negotiate_permessage_deflate(params, extensions, deflateOpts)
    catch
      :exit, {:error, :incompatible_zlib_version, _} ->
        websocket_extensions(state, req, tail, respHeader)
    else
      {:ok, respExt, extensions2} ->
        websocket_extensions(r_state(state, extensions: extensions2), req, tail, [
          [", ", respExt] | respHeader
        ])

      :ignore ->
        websocket_extensions(state, req, tail, respHeader)
    end
  end

  defp websocket_extensions(
         state = r_state(opts: opts, extensions: extensions),
         req = %{:pid => pid, :version => version},
         [{"x-webkit-deflate-frame", params} | tail],
         respHeader
       ) do
    deflateOpts0 = :maps.get(:deflate_opts, opts, %{})

    deflateOpts =
      case version do
        :"HTTP/1.1" ->
          %{deflateOpts0 | :owner => pid}

        _ ->
          deflateOpts0
      end

    try do
      :cow_ws.negotiate_x_webkit_deflate_frame(params, extensions, deflateOpts)
    catch
      :exit, {:error, :incompatible_zlib_version, _} ->
        websocket_extensions(state, req, tail, respHeader)
    else
      {:ok, respExt, extensions2} ->
        websocket_extensions(r_state(state, extensions: extensions2), req, tail, [
          [", ", respExt] | respHeader
        ])

      :ignore ->
        websocket_extensions(state, req, tail, respHeader)
    end
  end

  defp websocket_extensions(state, req, [_ | tail], respHeader) do
    websocket_extensions(state, req, tail, respHeader)
  end

  defp websocket_handshake(
         state = r_state(key: key),
         req = %{:version => :"HTTP/1.1", :pid => pid, :streamid => streamID},
         handlerState,
         env
       ) do
    challenge =
      :base64.encode(
        :crypto.hash(
          :sha,
          <<key::binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>
        )
      )

    headers =
      :cowboy_req.response_headers(
        %{
          "connection" => "Upgrade",
          "upgrade" => "websocket",
          "sec-websocket-accept" => challenge
        },
        req
      )

    send(
      pid,
      {{pid, streamID}, {:switch_protocol, headers, :cowboy_websocket, {state, handlerState}}}
    )

    {:ok, req, env}
  end

  defp websocket_handshake(
         state,
         req = %{:ref => ref, :pid => pid, :streamid => streamID},
         handlerState,
         _Env
       ) do
    headers = :cowboy_req.response_headers(%{}, req)

    send(
      pid,
      {{pid, streamID}, {:switch_protocol, headers, :cowboy_websocket, {state, handlerState}}}
    )

    takeover(pid, ref, {pid, streamID}, :undefined, :undefined, <<>>, {state, handlerState})
  end

  Record.defrecord(:r_ps_header, :ps_header, buffer: <<>>)

  Record.defrecord(:r_ps_payload, :ps_payload,
    type: :undefined,
    len: :undefined,
    mask_key: :undefined,
    rsv: :undefined,
    close_code: :undefined,
    unmasked: <<>>,
    unmasked_len: 0,
    buffer: <<>>
  )

  def takeover(
        parent,
        ref,
        socket,
        transport,
        _Opts,
        buffer,
        {state0 = r_state(handler: handler), handlerState}
      ) do
    :ranch.remove_connection(ref)

    messages =
      case transport do
        :undefined ->
          :undefined

        _ ->
          transport.messages()
      end

    state =
      loop_timeout(
        r_state(state0,
          parent: parent,
          ref: ref,
          socket: socket,
          transport: transport,
          key: :undefined,
          messages: messages
        )
      )

    case :erlang.function_exported(handler, :websocket_init, 1) do
      true ->
        handler_call(
          state,
          handlerState,
          r_ps_header(buffer: buffer),
          :websocket_init,
          :undefined,
          &after_init/3
        )

      false ->
        after_init(state, handlerState, r_ps_header(buffer: buffer))
    end
  end

  defp after_init(state = r_state(active: true), handlerState, parseState) do
    setopts_active(state)
    maybe_read_body(state)
    parse_header(state, handlerState, parseState)
  end

  defp after_init(state, handlerState, parseState) do
    parse_header(state, handlerState, parseState)
  end

  defp setopts_active(r_state(transport: :undefined)) do
    :ok
  end

  defp setopts_active(r_state(socket: socket, transport: transport, opts: opts)) do
    n = :maps.get(:active_n, opts, 100)
    transport.setopts(socket, [{:active, n}])
  end

  defp maybe_read_body(r_state(socket: stream = {pid, _}, transport: :undefined, active: true)) do
    readBodyRef = make_ref()
    send(pid, {stream, {:read_body, self(), readBodyRef, :auto, :infinity}})
    :ok
  end

  defp maybe_read_body(_) do
    :ok
  end

  defp active(state) do
    setopts_active(state)
    maybe_read_body(state)
    r_state(state, active: true)
  end

  defp passive(state = r_state(transport: :undefined)) do
    r_state(state, active: false)
  end

  defp passive(state = r_state(socket: socket, transport: transport, messages: messages)) do
    transport.setopts(socket, [{:active, false}])
    flush_passive(socket, messages)
    r_state(state, active: false)
  end

  defp flush_passive(socket, messages) do
    receive do
      {passive, ^socket}
      when passive ===
             :erlang.element(
               4,
               messages
             ) or
             passive === :tcp_passive or
             passive === :ssl_passive ->
        flush_passive(socket, messages)
    after
      0 ->
        :ok
    end
  end

  defp before_loop(state = r_state(hibernate: true), handlerState, parseState) do
    :proc_lib.hibernate(:cowboy_websocket, :loop, [
      r_state(state, hibernate: false),
      handlerState,
      parseState
    ])
  end

  defp before_loop(state, handlerState, parseState) do
    loop(state, handlerState, parseState)
  end

  defp loop_timeout(state = r_state(opts: opts, timeout_ref: prevRef)) do
    _ =
      case prevRef do
        :undefined ->
          :ignore

        ^prevRef ->
          :erlang.cancel_timer(prevRef)
      end

    case :maps.get(:idle_timeout, opts, 60000) do
      :infinity ->
        r_state(state, timeout_ref: :undefined)

      timeout ->
        tRef = :erlang.start_timer(timeout, self(), :cowboy_websocket)
        r_state(state, timeout_ref: tRef)
    end
  end

  def loop(
        state = r_state(parent: parent, socket: socket, messages: messages, timeout_ref: tRef),
        handlerState,
        parseState
      ) do
    receive do
      {oK, ^socket, data}
      when oK ===
             :erlang.element(
               1,
               messages
             ) ->
        state2 = loop_timeout(state)
        parse(state2, handlerState, parseState, data)

      {closed, ^socket}
      when closed ===
             :erlang.element(
               2,
               messages
             ) ->
        terminate(state, handlerState, {:error, :closed})

      {error, ^socket, reason}
      when error === :erlang.element(3, messages) ->
        terminate(state, handlerState, {:error, reason})

      {passive, ^socket}
      when passive ===
             :erlang.element(
               4,
               messages
             ) or
             passive === :tcp_passive or
             passive === :ssl_passive ->
        setopts_active(state)
        loop(state, handlerState, parseState)

      {:request_body, _Ref, :nofin, data} ->
        maybe_read_body(state)
        state2 = loop_timeout(state)
        parse(state2, handlerState, parseState, data)

      {:request_body, _Ref, :fin, _, data} ->
        maybe_read_body(state)
        state2 = loop_timeout(state)
        parse(state2, handlerState, parseState, data)

      {:timeout, ^tRef, :cowboy_websocket} ->
        websocket_close(state, handlerState, :timeout)

      {:timeout, olderTRef, :cowboy_websocket}
      when is_reference(olderTRef) ->
        before_loop(state, handlerState, parseState)

      {:EXIT, ^parent, reason} ->
        exit(reason)

      {:system, from, request} ->
        :sys.handle_system_msg(
          request,
          from,
          parent,
          :cowboy_websocket,
          [],
          {state, handlerState, parseState}
        )

      {:"$gen_call", from, call} ->
        :cowboy_children.handle_supervisor_call(call, from, [], :cowboy_websocket)
        before_loop(state, handlerState, parseState)

      message ->
        handler_call(state, handlerState, parseState, :websocket_info, message, &before_loop/3)
    end
  end

  defp parse(state, handlerState, pS = r_ps_header(buffer: buffer), data) do
    parse_header(state, handlerState, r_ps_header(pS, buffer: <<buffer::binary, data::binary>>))
  end

  defp parse(state, handlerState, pS = r_ps_payload(buffer: buffer), data) do
    parse_payload(
      state,
      handlerState,
      r_ps_payload(pS, buffer: <<>>),
      <<buffer::binary, data::binary>>
    )
  end

  defp parse_header(
         state = r_state(opts: opts, frag_state: fragState, extensions: extensions),
         handlerState,
         parseState = r_ps_header(buffer: data)
       ) do
    maxFrameSize = :maps.get(:max_frame_size, opts, :infinity)

    case :cow_ws.parse_header(data, extensions, fragState) do
      {_, _, _, _, :undefined, _} ->
        websocket_close(state, handlerState, {:error, :badframe})

      {_, _, _, len, _, _} when len > maxFrameSize ->
        websocket_close(state, handlerState, {:error, :badsize})

      {type, fragState2, rsv, len, maskKey, rest} ->
        parse_payload(
          r_state(state, frag_state: fragState2),
          handlerState,
          r_ps_payload(type: type, len: len, mask_key: maskKey, rsv: rsv),
          rest
        )

      :more ->
        before_loop(state, handlerState, parseState)

      :error ->
        websocket_close(state, handlerState, {:error, :badframe})
    end
  end

  defp parse_payload(
         state = r_state(frag_state: fragState, utf8_state: incomplete, extensions: extensions),
         handlerState,
         parseState =
           r_ps_payload(
             type: type,
             len: len,
             mask_key: maskKey,
             rsv: rsv,
             unmasked: unmasked,
             unmasked_len: unmaskedLen
           ),
         data
       ) do
    case :cow_ws.parse_payload(
           data,
           maskKey,
           incomplete,
           unmaskedLen,
           type,
           len,
           fragState,
           extensions,
           rsv
         ) do
      {:ok, closeCode, payload, utf8State, rest} ->
        dispatch_frame(
          r_state(state, utf8_state: utf8State),
          handlerState,
          r_ps_payload(parseState,
            unmasked: <<unmasked::binary, payload::binary>>,
            close_code: closeCode
          ),
          rest
        )

      {:ok, payload, utf8State, rest} ->
        dispatch_frame(
          r_state(state, utf8_state: utf8State),
          handlerState,
          r_ps_payload(parseState, unmasked: <<unmasked::binary, payload::binary>>),
          rest
        )

      {:more, closeCode, payload, utf8State} ->
        before_loop(
          r_state(state, utf8_state: utf8State),
          handlerState,
          r_ps_payload(parseState,
            len: len - byte_size(data),
            close_code: closeCode,
            unmasked: <<unmasked::binary, payload::binary>>,
            unmasked_len: unmaskedLen + byte_size(data)
          )
        )

      {:more, payload, utf8State} ->
        before_loop(
          r_state(state, utf8_state: utf8State),
          handlerState,
          r_ps_payload(parseState,
            len: len - byte_size(data),
            unmasked: <<unmasked::binary, payload::binary>>,
            unmasked_len: unmaskedLen + byte_size(data)
          )
        )

      error = {:error, _Reason} ->
        websocket_close(state, handlerState, error)
    end
  end

  defp dispatch_frame(
         state = r_state(opts: opts, frag_state: fragState, frag_buffer: soFar),
         handlerState,
         r_ps_payload(type: type0, unmasked: payload0, close_code: closeCode0),
         remainingData
       ) do
    maxFrameSize = :maps.get(:max_frame_size, opts, :infinity)

    case :cow_ws.make_frame(type0, payload0, closeCode0, fragState) do
      {:fragment, _, _, payload}
      when byte_size(payload) + byte_size(soFar) > maxFrameSize ->
        websocket_close(state, handlerState, {:error, :badsize})

      {:fragment, :nofin, _, payload} ->
        parse_header(
          r_state(state, frag_buffer: <<soFar::binary, payload::binary>>),
          handlerState,
          r_ps_header(buffer: remainingData)
        )

      {:fragment, :fin, type, payload} ->
        handler_call(
          r_state(state,
            frag_state: :undefined,
            frag_buffer: <<>>
          ),
          handlerState,
          r_ps_header(buffer: remainingData),
          :websocket_handle,
          {type, <<soFar::binary, payload::binary>>},
          &parse_header/3
        )

      :close ->
        websocket_close(state, handlerState, :remote)

      {:close, closeCode, payload} ->
        websocket_close(state, handlerState, {:remote, closeCode, payload})

      frame = :ping ->
        transport_send(state, :nofin, frame(:pong, state))

        handler_call(
          state,
          handlerState,
          r_ps_header(buffer: remainingData),
          :websocket_handle,
          frame,
          &parse_header/3
        )

      frame = {:ping, payload} ->
        transport_send(state, :nofin, frame({:pong, payload}, state))

        handler_call(
          state,
          handlerState,
          r_ps_header(buffer: remainingData),
          :websocket_handle,
          frame,
          &parse_header/3
        )

      frame ->
        handler_call(
          state,
          handlerState,
          r_ps_header(buffer: remainingData),
          :websocket_handle,
          frame,
          &parse_header/3
        )
    end
  end

  defp handler_call(
         state = r_state(handler: handler),
         handlerState,
         parseState,
         callback,
         message,
         nextState
       ) do
    try do
      case callback do
        :websocket_init ->
          handler.websocket_init(handlerState)

        _ ->
          apply(handler, callback, [message, handlerState])
      end
    catch
      class, reason ->
        websocket_send_close(state, {:crash, class, reason})
        handler_terminate(state, handlerState, {:crash, class, reason})
        :erlang.raise(class, reason, __STACKTRACE__)
    else
      {commands, handlerState2} when is_list(commands) ->
        handler_call_result(state, handlerState2, parseState, nextState, commands)

      {commands, handlerState2, :hibernate}
      when is_list(commands) ->
        handler_call_result(
          r_state(state, hibernate: true),
          handlerState2,
          parseState,
          nextState,
          commands
        )

      {:ok, handlerState2} ->
        nextState.(state, handlerState2, parseState)

      {:ok, handlerState2, :hibernate} ->
        nextState.(r_state(state, hibernate: true), handlerState2, parseState)

      {:reply, payload, handlerState2} ->
        case websocket_send(payload, state) do
          :ok ->
            nextState.(state, handlerState2, parseState)

          :stop ->
            terminate(state, handlerState2, :stop)

          error = {:error, _} ->
            terminate(state, handlerState2, error)
        end

      {:reply, payload, handlerState2, :hibernate} ->
        case websocket_send(payload, state) do
          :ok ->
            nextState.(r_state(state, hibernate: true), handlerState2, parseState)

          :stop ->
            terminate(state, handlerState2, :stop)

          error = {:error, _} ->
            terminate(state, handlerState2, error)
        end

      {:stop, handlerState2} ->
        websocket_close(state, handlerState2, :stop)
    end
  end

  defp handler_call_result(state0, handlerState, parseState, nextState, commands) do
    case commands(commands, state0, []) do
      {:ok, state} ->
        nextState.(state, handlerState, parseState)

      {:stop, state} ->
        terminate(state, handlerState, :stop)

      {error = {:error, _}, state} ->
        terminate(state, handlerState, error)
    end
  end

  defp commands([], state, []) do
    {:ok, state}
  end

  defp commands([], state, data) do
    result = transport_send(state, :nofin, :lists.reverse(data))
    {result, state}
  end

  defp commands([{:active, active} | tail], state0 = r_state(active: active0), data)
       when is_boolean(active) do
    state =
      cond do
        active and not active0 ->
          active(state0)

        active0 and not active ->
          passive(state0)

        true ->
          state0
      end

    commands(tail, r_state(state, active: active), data)
  end

  defp commands([{:deflate, deflate} | tail], state, data)
       when is_boolean(deflate) do
    commands(tail, r_state(state, deflate: deflate), data)
  end

  defp commands([{:set_options, setOpts} | tail], state0 = r_state(opts: opts), data) do
    state =
      case setOpts do
        %{:idle_timeout => idleTimeout} ->
          loop_timeout(r_state(state0, opts: %{opts | :idle_timeout => idleTimeout}))

        _ ->
          state0
      end

    commands(tail, state, data)
  end

  defp commands([{:shutdown_reason, shutdownReason} | tail], state, data) do
    commands(tail, r_state(state, shutdown_reason: shutdownReason), data)
  end

  defp commands([frame | tail], state, data0) do
    data = [frame(frame, state) | data0]

    case is_close_frame(frame) do
      true ->
        _ = transport_send(state, :fin, :lists.reverse(data))
        {:stop, state}

      false ->
        commands(tail, state, data)
    end
  end

  defp transport_send(
         r_state(
           socket: stream = {pid, _},
           transport: :undefined
         ),
         isFin,
         data
       ) do
    send(pid, {stream, {:data, isFin, data}})
    :ok
  end

  defp transport_send(r_state(socket: socket, transport: transport), _, data) do
    transport.send(socket, data)
  end

  defp websocket_send(frames, state) when is_list(frames) do
    websocket_send_many(frames, state, [])
  end

  defp websocket_send(frame, state) do
    data = frame(frame, state)

    case is_close_frame(frame) do
      true ->
        _ = transport_send(state, :fin, data)
        :stop

      false ->
        transport_send(state, :nofin, data)
    end
  end

  defp websocket_send_many([], state, acc) do
    transport_send(state, :nofin, :lists.reverse(acc))
  end

  defp websocket_send_many([frame | tail], state, acc0) do
    acc = [frame(frame, state) | acc0]

    case is_close_frame(frame) do
      true ->
        _ = transport_send(state, :fin, :lists.reverse(acc))
        :stop

      false ->
        websocket_send_many(tail, state, acc)
    end
  end

  defp is_close_frame(:close) do
    true
  end

  defp is_close_frame({:close, _}) do
    true
  end

  defp is_close_frame({:close, _, _}) do
    true
  end

  defp is_close_frame(_) do
    false
  end

  defp websocket_close(state, handlerState, reason) do
    websocket_send_close(state, reason)
    terminate(state, handlerState, reason)
  end

  defp websocket_send_close(state, reason) do
    _ =
      case reason do
        normal when normal === :stop or normal === :timeout ->
          transport_send(state, :fin, frame({:close, 1000, <<>>}, state))

        {:error, :badframe} ->
          transport_send(state, :fin, frame({:close, 1002, <<>>}, state))

        {:error, :badencoding} ->
          transport_send(state, :fin, frame({:close, 1007, <<>>}, state))

        {:error, :badsize} ->
          transport_send(state, :fin, frame({:close, 1009, <<>>}, state))

        {:crash, _, _} ->
          transport_send(state, :fin, frame({:close, 1011, <<>>}, state))

        :remote ->
          transport_send(state, :fin, frame(:close, state))

        {:remote, code, _} ->
          transport_send(state, :fin, frame({:close, code, <<>>}, state))
      end

    :ok
  end

  defp frame(
         frame,
         r_state(deflate: false, extensions: extensions)
       ) do
    :cow_ws.frame(frame, %{extensions | :deflate => false})
  end

  defp frame(frame, r_state(extensions: extensions)) do
    :cow_ws.frame(frame, extensions)
  end

  defp terminate(state = r_state(shutdown_reason: shutdown), handlerState, reason) do
    handler_terminate(state, handlerState, reason)

    case shutdown do
      :normal ->
        exit(:normal)

      _ ->
        exit({:shutdown, shutdown})
    end
  end

  defp handler_terminate(r_state(handler: handler, req: req), handlerState, reason) do
    :cowboy_handler.terminate(reason, req, handlerState, handler)
  end

  def system_continue(_, _, {state, handlerState, parseState}) do
    loop(state, handlerState, parseState)
  end

  def system_terminate(reason, _, _, {state, handlerState, _}) do
    terminate(state, handlerState, reason)
  end

  def system_code_change(misc, _, _, _) do
    {:ok, misc}
  end
end
