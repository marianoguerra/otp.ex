defmodule :cowboy_http2 do
  use Bitwise
  require Record
  Record.defrecord(:r_stream, :stream, status: :running, flow: 0, state: :undefined)

  Record.defrecord(:r_state, :state,
    parent: :undefined,
    ref: :undefined,
    socket: :undefined,
    transport: :undefined,
    proxy_header: :undefined,
    opts: %{},
    timer: :undefined,
    peer: :undefined,
    sock: :undefined,
    cert: :undefined,
    http2_status: :undefined,
    http2_machine: :undefined,
    frame_rate_num: :undefined,
    frame_rate_time: :undefined,
    reset_rate_num: :undefined,
    reset_rate_time: :undefined,
    flow: 0,
    streams: %{},
    children: :cowboy_children.init()
  )

  def init(parent, ref, socket, transport, proxyHeader, opts) do
    peer0 = transport.peername(socket)
    sock0 = transport.sockname(socket)

    cert1 =
      case transport.name() do
        :ssl ->
          case :ssl.peercert(socket) do
            {:error, :no_peercert} ->
              {:ok, :undefined}

            cert0 ->
              cert0
          end

        _ ->
          {:ok, :undefined}
      end

    case {peer0, sock0, cert1} do
      {{:ok, peer}, {:ok, sock}, {:ok, cert}} ->
        init(parent, ref, socket, transport, proxyHeader, opts, peer, sock, cert, <<>>)

      {{:error, reason}, _, _} ->
        terminate(
          :undefined,
          {:socket_error, reason, :"A socket error occurred when retrieving the peer name."}
        )

      {_, {:error, reason}, _} ->
        terminate(
          :undefined,
          {:socket_error, reason, :"A socket error occurred when retrieving the sock name."}
        )

      {_, _, {:error, reason}} ->
        terminate(
          :undefined,
          {:socket_error, reason,
           :"A socket error occurred when retrieving the client TLS certificate."}
        )
    end
  end

  def init(parent, ref, socket, transport, proxyHeader, opts, peer, sock, cert, buffer) do
    {:ok, preface, hTTP2Machine} = :cow_http2_machine.init(:server, opts)

    state =
      set_timeout(
        init_rate_limiting(
          r_state(
            parent: parent,
            ref: ref,
            socket: socket,
            transport: transport,
            proxy_header: proxyHeader,
            opts: opts,
            peer: peer,
            sock: sock,
            cert: cert,
            http2_status: :sequence,
            http2_machine: hTTP2Machine
          )
        )
      )

    transport.send(socket, preface)
    setopts_active(state)

    case buffer do
      <<>> ->
        loop(state, buffer)

      _ ->
        parse(state, buffer)
    end
  end

  defp init_rate_limiting(state = r_state(opts: opts)) do
    {frameRateNum, frameRatePeriod} = :maps.get(:max_received_frame_rate, opts, {10000, 10000})
    {resetRateNum, resetRatePeriod} = :maps.get(:max_reset_stream_rate, opts, {10, 10000})
    currentTime = :erlang.monotonic_time(:millisecond)

    r_state(state,
      frame_rate_num: frameRateNum,
      frame_rate_time:
        add_period(
          currentTime,
          frameRatePeriod
        ),
      reset_rate_num: resetRateNum,
      reset_rate_time:
        add_period(
          currentTime,
          resetRatePeriod
        )
    )
  end

  defp add_period(_, :infinity) do
    :infinity
  end

  defp add_period(time, period) do
    time + period
  end

  def init(
        parent,
        ref,
        socket,
        transport,
        proxyHeader,
        opts,
        peer,
        sock,
        cert,
        buffer,
        _Settings,
        req = %{:method => method}
      ) do
    {:ok, preface, hTTP2Machine0} = :cow_http2_machine.init(:server, opts)

    {:ok, streamID, hTTP2Machine} =
      :cow_http2_machine.init_upgrade_stream(
        method,
        hTTP2Machine0
      )

    state0 =
      r_state(
        parent: parent,
        ref: ref,
        socket: socket,
        transport: transport,
        proxy_header: proxyHeader,
        opts: opts,
        peer: peer,
        sock: sock,
        cert: cert,
        http2_status: :upgrade,
        http2_machine: hTTP2Machine
      )

    state1 = headers_frame(r_state(state0, http2_machine: hTTP2Machine), streamID, req)

    state2 =
      info(
        state1,
        1,
        {:switch_protocol, %{"connection" => "Upgrade", "upgrade" => "h2c"}, :cowboy_http2,
         :undefined}
      )

    state = set_timeout(init_rate_limiting(r_state(state2, http2_status: :sequence)))
    transport.send(socket, preface)
    setopts_active(state)

    case buffer do
      <<>> ->
        loop(state, buffer)

      _ ->
        parse(state, buffer)
    end
  end

  defp setopts_active(r_state(socket: socket, transport: transport, opts: opts)) do
    n = :maps.get(:active_n, opts, 100)
    transport.setopts(socket, [{:active, n}])
  end

  defp loop(
         state =
           r_state(
             parent: parent,
             socket: socket,
             transport: transport,
             opts: opts,
             timer: timerRef,
             children: children
           ),
         buffer
       ) do
    messages = transport.messages()
    inactivityTimeout = :maps.get(:inactivity_timeout, opts, 300_000)

    receive do
      {oK, ^socket, data}
      when oK ===
             :erlang.element(
               1,
               messages
             ) ->
        parse(
          set_timeout(state),
          <<buffer::binary, data::binary>>
        )

      {closed, ^socket}
      when closed ===
             :erlang.element(
               2,
               messages
             ) ->
        terminate(state, {:socket_error, :closed, :"The socket has been closed."})

      {error, ^socket, reason}
      when error === :erlang.element(3, messages) ->
        terminate(state, {:socket_error, reason, :"An error has occurred on the socket."})

      {passive, ^socket}
      when passive ===
             :erlang.element(
               4,
               messages
             ) or
             passive === :tcp_passive or
             passive === :ssl_passive ->
        setopts_active(state)
        loop(state, buffer)

      {:EXIT, ^parent, reason} ->
        terminate(state, {:stop, {:exit, reason}, :"Parent process terminated."})

      {:system, from, request} ->
        :sys.handle_system_msg(request, from, parent, :cowboy_http2, [], {state, buffer})

      {:timeout, ^timerRef, :idle_timeout} ->
        terminate(state, {:stop, :timeout, :"Connection idle longer than configuration allows."})

      {:timeout, ref, {:shutdown, pid}} ->
        :cowboy_children.shutdown_timeout(children, ref, pid)
        loop(state, buffer)

      {:timeout, tRef, {:cow_http2_machine, name}} ->
        loop(timeout(state, name, tRef), buffer)

      {{pid, streamID}, msg} when pid === self() ->
        loop(info(state, streamID, msg), buffer)

      msg = {:EXIT, pid, _} ->
        loop(down(state, pid, msg), buffer)

      {:"$gen_call", from, call} ->
        :cowboy_children.handle_supervisor_call(call, from, children, :cowboy_http2)
        loop(state, buffer)

      msg ->
        :cowboy.log(:warning, 'Received stray message ~p.', [msg], opts)
        loop(state, buffer)
    after
      inactivityTimeout ->
        terminate(
          state,
          {:internal_error, :timeout, :"No message or data received before timeout."}
        )
    end
  end

  defp set_timeout(state = r_state(opts: opts, timer: timerRef0)) do
    :ok =
      case timerRef0 do
        :undefined ->
          :ok

        _ ->
          :erlang.cancel_timer(
            timerRef0,
            [{:async, true}, {:info, false}]
          )
      end

    timerRef =
      case :maps.get(:idle_timeout, opts, 60000) do
        :infinity ->
          :undefined

        timeout ->
          :erlang.start_timer(timeout, self(), :idle_timeout)
      end

    r_state(state, timer: timerRef)
  end

  defp parse(state = r_state(http2_status: :sequence), data) do
    case :cow_http2.parse_sequence(data) do
      {:ok, rest} ->
        parse(r_state(state, http2_status: :settings), rest)

      :more ->
        loop(state, data)

      error = {:connection_error, _, _} ->
        terminate(state, error)
    end
  end

  defp parse(
         state = r_state(http2_status: status, http2_machine: hTTP2Machine, streams: streams),
         data
       ) do
    maxFrameSize =
      :cow_http2_machine.get_local_setting(
        :max_frame_size,
        hTTP2Machine
      )

    case :cow_http2.parse(data, maxFrameSize) do
      {:ok, frame, rest} ->
        parse(frame_rate(state, frame), rest)

      {:ignore, rest} ->
        parse(frame_rate(state, :ignore), rest)

      {:stream_error, streamID, reason, human, rest} ->
        parse(
          reset_stream(state, streamID, {:stream_error, reason, human}),
          rest
        )

      error = {:connection_error, _, _} ->
        terminate(state, error)

      :more when status === :closing and streams === %{} ->
        terminate(state, {:stop, :normal, :"The connection is going away."})

      :more ->
        loop(state, data)
    end
  end

  defp frame_rate(
         state0 = r_state(opts: opts, frame_rate_num: num0, frame_rate_time: time),
         frame
       ) do
    {result, state} =
      case num0 - 1 do
        0 ->
          currentTime = :erlang.monotonic_time(:millisecond)

          cond do
            currentTime < time ->
              {:error, state0}

            true ->
              {num, period} = :maps.get(:max_received_frame_rate, opts, {1000, 10000})

              {:ok,
               r_state(state0,
                 frame_rate_num: num,
                 frame_rate_time: currentTime + period
               )}
          end

        num ->
          {:ok, r_state(state0, frame_rate_num: num)}
      end

    case {result, frame} do
      {:ok, :ignore} ->
        ignored_frame(state)

      {:ok, _} ->
        frame(state, frame)

      {:error, _} ->
        terminate(
          state,
          {:connection_error, :enhance_your_calm,
           :"Frame rate larger than configuration allows. Flood? (CVE-2019-9512, CVE-2019-9515, CVE-2019-9518)"}
        )
    end
  end

  defp frame(
         state = r_state(http2_machine: hTTP2Machine0),
         frame
       ) do
    case :cow_http2_machine.frame(frame, hTTP2Machine0) do
      {:ok, hTTP2Machine} ->
        maybe_ack(r_state(state, http2_machine: hTTP2Machine), frame)

      {:ok, {:data, streamID, isFin, data}, hTTP2Machine} ->
        data_frame(r_state(state, http2_machine: hTTP2Machine), streamID, isFin, data)

      {:ok, {:headers, streamID, isFin, headers, pseudoHeaders, bodyLen}, hTTP2Machine} ->
        headers_frame(
          r_state(state, http2_machine: hTTP2Machine),
          streamID,
          isFin,
          headers,
          pseudoHeaders,
          bodyLen
        )

      {:ok, {:trailers, _StreamID, _Trailers}, hTTP2Machine} ->
        r_state(state, http2_machine: hTTP2Machine)

      {:ok, {:rst_stream, streamID, reason}, hTTP2Machine} ->
        rst_stream_frame(r_state(state, http2_machine: hTTP2Machine), streamID, reason)

      {:ok, goAway = {:goaway, _, _, _}, hTTP2Machine} ->
        goaway(r_state(state, http2_machine: hTTP2Machine), goAway)

      {:send, sendData, hTTP2Machine} ->
        :lists.foldl(
          fn {streamID, _, _}, s ->
            maybe_send_data_alarm(s, hTTP2Machine0, streamID)
          end,
          send_data(
            maybe_ack(
              r_state(state, http2_machine: hTTP2Machine),
              frame
            ),
            sendData,
            []
          ),
          sendData
        )

      {:error, {:stream_error, streamID, reason, human}, hTTP2Machine} ->
        reset_stream(
          r_state(state, http2_machine: hTTP2Machine),
          streamID,
          {:stream_error, reason, human}
        )

      {:error, error = {:connection_error, _, _}, hTTP2Machine} ->
        terminate(r_state(state, http2_machine: hTTP2Machine), error)
    end
  end

  defp maybe_ack(state = r_state(http2_status: :settings), frame) do
    maybe_ack(r_state(state, http2_status: :connected), frame)
  end

  defp maybe_ack(
         state = r_state(socket: socket, transport: transport),
         frame
       ) do
    case frame do
      {:settings, _} ->
        transport.send(socket, :cow_http2.settings_ack())

      {:ping, opaque} ->
        transport.send(socket, :cow_http2.ping_ack(opaque))

      _ ->
        :ok
    end

    state
  end

  defp data_frame(
         state0 = r_state(opts: opts, flow: flow, streams: streams),
         streamID,
         isFin,
         data
       ) do
    case streams do
      %{^streamID => stream = r_stream(status: :running, flow: streamFlow, state: streamState0)} ->
        try do
          :cowboy_stream.data(streamID, isFin, data, streamState0)
        catch
          class, exception ->
            :cowboy.log(
              :cowboy_stream.make_error_log(
                :data,
                [streamID, isFin, data, streamState0],
                class,
                exception,
                __STACKTRACE__
              ),
              opts
            )

            reset_stream(
              state0,
              streamID,
              {:internal_error, {class, exception},
               :"Unhandled exception in cowboy_stream:data/4."}
            )
        else
          {commands, streamState} ->
            size = byte_size(data)

            state =
              update_window(
                r_state(state0,
                  flow:
                    max(
                      0,
                      flow - size
                    ),
                  streams: %{
                    streams
                    | streamID =>
                        r_stream(stream,
                          flow:
                            max(
                              0,
                              streamFlow - size
                            ),
                          state: streamState
                        )
                  }
                ),
                streamID
              )

            commands(state, streamID, commands)
        end

      %{} ->
        state0
    end
  end

  defp headers_frame(state, streamID, isFin, headers, pseudoHeaders = %{:method => "CONNECT"}, _)
       when map_size(pseudoHeaders) === 2 do
    early_error(
      state,
      streamID,
      isFin,
      headers,
      pseudoHeaders,
      501,
      :"The CONNECT method is currently not implemented. (RFC7231 4.3.6)"
    )
  end

  defp headers_frame(state, streamID, isFin, headers, pseudoHeaders = %{:method => "TRACE"}, _) do
    early_error(
      state,
      streamID,
      isFin,
      headers,
      pseudoHeaders,
      501,
      :"The TRACE method is currently not implemented. (RFC7231 4.3.8)"
    )
  end

  defp headers_frame(
         state,
         streamID,
         isFin,
         headers,
         pseudoHeaders = %{:authority => authority},
         bodyLen
       ) do
    headers_frame_parse_host(state, streamID, isFin, headers, pseudoHeaders, bodyLen, authority)
  end

  defp headers_frame(state, streamID, isFin, headers, pseudoHeaders, bodyLen) do
    case :lists.keyfind("host", 1, headers) do
      {_, authority} ->
        headers_frame_parse_host(
          state,
          streamID,
          isFin,
          headers,
          pseudoHeaders,
          bodyLen,
          authority
        )

      _ ->
        reset_stream(
          state,
          streamID,
          {:stream_error, :protocol_error,
           :"Requests translated from HTTP/1.1 must include a host header. (RFC7540 8.1.2.3, RFC7230 5.4)"}
        )
    end
  end

  defp headers_frame_parse_host(
         state = r_state(ref: ref, peer: peer, sock: sock, cert: cert, proxy_header: proxyHeader),
         streamID,
         isFin,
         headers,
         pseudoHeaders = %{:method => method, :scheme => scheme, :path => pathWithQs},
         bodyLen,
         authority
       ) do
    try do
      :cow_http_hd.parse_host(authority)
    catch
      _, _ ->
        reset_stream(
          state,
          streamID,
          {:stream_error, :protocol_error,
           :"The :authority pseudo-header is invalid. (RFC7540 8.1.2.3)"}
        )
    else
      {host, port0} ->
        port = ensure_port(scheme, port0)

        try do
          :cow_http.parse_fullpath(pathWithQs)
        catch
          _, _ ->
            reset_stream(
              state,
              streamID,
              {:stream_error, :protocol_error,
               :"The :path pseudo-header is invalid. (RFC7540 8.1.2.3)"}
            )
        else
          {<<>>, _} ->
            reset_stream(
              state,
              streamID,
              {:stream_error, :protocol_error,
               :"The path component must not be empty. (RFC7540 8.1.2.3)"}
            )

          {path, qs} ->
            req0 = %{
              :ref => ref,
              :pid => self(),
              :streamid => streamID,
              :peer => peer,
              :sock => sock,
              :cert => cert,
              :method => method,
              :scheme => scheme,
              :host => host,
              :port => port,
              :path => path,
              :qs => qs,
              :version => :"HTTP/2",
              :headers => headers_to_map(headers, %{}),
              :has_body => isFin === :nofin,
              :body_length => bodyLen
            }

            req1 =
              case proxyHeader do
                :undefined ->
                  req0

                _ ->
                  %{req0 | :proxy_header => proxyHeader}
              end

            req =
              case pseudoHeaders do
                %{:protocol => protocol} ->
                  %{req1 | :protocol => protocol}

                _ ->
                  req1
              end

            headers_frame(state, streamID, req)
        end
    end
  end

  defp ensure_port("http", :undefined) do
    80
  end

  defp ensure_port("https", :undefined) do
    443
  end

  defp ensure_port(_, port) do
    port
  end

  defp headers_to_map([], acc) do
    acc
  end

  defp headers_to_map([{name, value} | tail], acc0) do
    acc =
      case acc0 do
        %{^name => value0} when name === "cookie" ->
          %{acc0 | name => <<value0::binary, "; ", value::binary>>}

        %{^name => value0} ->
          %{acc0 | name => <<value0::binary, ", ", value::binary>>}

        _ ->
          %{acc0 | name => value}
      end

    headers_to_map(tail, acc)
  end

  defp headers_frame(state = r_state(opts: opts, streams: streams), streamID, req) do
    try do
      :cowboy_stream.init(streamID, req, opts)
    catch
      class, exception ->
        :cowboy.log(
          :cowboy_stream.make_error_log(
            :init,
            [streamID, req, opts],
            class,
            exception,
            __STACKTRACE__
          ),
          opts
        )

        reset_stream(
          state,
          streamID,
          {:internal_error, {class, exception}, :"Unhandled exception in cowboy_stream:init/3."}
        )
    else
      {commands, streamState} ->
        commands(
          r_state(state, streams: %{streams | streamID => r_stream(state: streamState)}),
          streamID,
          commands
        )
    end
  end

  defp early_error(
         state0 = r_state(ref: ref, opts: opts, peer: peer),
         streamID,
         _IsFin,
         headers,
         %{:method => method},
         statusCode0,
         humanReadable
       ) do
    reason = {:stream_error, :no_error, humanReadable}

    partialReq = %{
      :ref => ref,
      :peer => peer,
      :method => method,
      :headers => headers_to_map(headers, %{})
    }

    resp = {:response, statusCode0, respHeaders0 = %{"content-length" => "0"}, <<>>}

    try do
      :cowboy_stream.early_error(streamID, reason, partialReq, resp, opts)
    catch
      class, exception ->
        :cowboy.log(
          :cowboy_stream.make_error_log(
            :early_error,
            [streamID, reason, partialReq, resp, opts],
            class,
            exception,
            __STACKTRACE__
          ),
          opts
        )

        send_headers(state0, streamID, :fin, statusCode0, respHeaders0)
    else
      {:response, statusCode, respHeaders, respBody} ->
        send_response(state0, streamID, statusCode, respHeaders, respBody)
    end
  end

  defp rst_stream_frame(
         state =
           r_state(
             streams: streams0,
             children: children0
           ),
         streamID,
         reason
       ) do
    case :maps.take(streamID, streams0) do
      {r_stream(state: streamState), streams} ->
        terminate_stream_handler(state, streamID, reason, streamState)

        children =
          :cowboy_children.shutdown(
            children0,
            streamID
          )

        r_state(state, streams: streams, children: children)

      :error ->
        state
    end
  end

  defp ignored_frame(state = r_state(http2_machine: hTTP2Machine0)) do
    case :cow_http2_machine.ignored_frame(hTTP2Machine0) do
      {:ok, hTTP2Machine} ->
        r_state(state, http2_machine: hTTP2Machine)

      {:error, error = {:connection_error, _, _}, hTTP2Machine} ->
        terminate(r_state(state, http2_machine: hTTP2Machine), error)
    end
  end

  defp timeout(state = r_state(http2_machine: hTTP2Machine0), name, tRef) do
    case :cow_http2_machine.timeout(name, tRef, hTTP2Machine0) do
      {:ok, hTTP2Machine} ->
        r_state(state, http2_machine: hTTP2Machine)

      {:error, error = {:connection_error, _, _}, hTTP2Machine} ->
        terminate(r_state(state, http2_machine: hTTP2Machine), error)
    end
  end

  defp down(state = r_state(opts: opts, children: children0), pid, msg) do
    case :cowboy_children.down(children0, pid) do
      {:ok, :undefined, children} ->
        r_state(state, children: children)

      {:ok, streamID, children} ->
        info(r_state(state, children: children), streamID, msg)

      :error ->
        :cowboy.log(
          :warning,
          'Received EXIT signal ~p for unknown process ~p.~n',
          [msg, pid],
          opts
        )

        state
    end
  end

  defp info(
         state = r_state(opts: opts, http2_machine: hTTP2Machine, streams: streams),
         streamID,
         msg
       ) do
    case streams do
      %{^streamID => stream = r_stream(state: streamState0)} ->
        try do
          :cowboy_stream.info(streamID, msg, streamState0)
        catch
          class, exception ->
            :cowboy.log(
              :cowboy_stream.make_error_log(
                :info,
                [streamID, msg, streamState0],
                class,
                exception,
                __STACKTRACE__
              ),
              opts
            )

            reset_stream(
              state,
              streamID,
              {:internal_error, {class, exception},
               :"Unhandled exception in cowboy_stream:info/3."}
            )
        else
          {commands, streamState} ->
            commands(
              r_state(state,
                streams: %{streams | streamID => r_stream(stream, state: streamState)}
              ),
              streamID,
              commands
            )
        end

      _ ->
        case :cow_http2_machine.is_lingering_stream(
               streamID,
               hTTP2Machine
             ) do
          true ->
            :ok

          false ->
            :cowboy.log(
              :warning,
              'Received message ~p for unknown stream ~p.',
              [msg, streamID],
              opts
            )
        end

        state
    end
  end

  defp commands(state, _, []) do
    state
  end

  defp commands(
         state = r_state(http2_machine: hTTP2Machine),
         streamID,
         [
           {:error_response, statusCode, headers, body}
           | tail
         ]
       ) do
    case :cow_http2_machine.get_stream_local_state(
           streamID,
           hTTP2Machine
         ) do
      {:ok, :idle, _} ->
        commands(state, streamID, [{:response, statusCode, headers, body} | tail])

      _ ->
        commands(state, streamID, tail)
    end
  end

  defp commands(state0, streamID, [{:inform, statusCode, headers} | tail]) do
    state = send_headers(state0, streamID, :idle, statusCode, headers)
    commands(state, streamID, tail)
  end

  defp commands(state0, streamID, [{:response, statusCode, headers, body} | tail]) do
    state = send_response(state0, streamID, statusCode, headers, body)
    commands(state, streamID, tail)
  end

  defp commands(state0, streamID, [{:headers, statusCode, headers} | tail]) do
    state = send_headers(state0, streamID, :nofin, statusCode, headers)
    commands(state, streamID, tail)
  end

  defp commands(state0, streamID, [{:data, isFin, data} | tail]) do
    state = maybe_send_data(state0, streamID, isFin, data, [])
    commands(state, streamID, tail)
  end

  defp commands(state0, streamID, [{:trailers, trailers} | tail]) do
    state = maybe_send_data(state0, streamID, :fin, {:trailers, :maps.to_list(trailers)}, [])
    commands(state, streamID, tail)
  end

  defp commands(
         state0 = r_state(socket: socket, transport: transport, http2_machine: hTTP2Machine0),
         streamID,
         [
           {:push, method, scheme, host, port, path, qs, headers0}
           | tail
         ]
       ) do
    authority =
      case {scheme, port} do
        {"http", 80} ->
          host

        {"https", 443} ->
          host

        _ ->
          :erlang.iolist_to_binary([host, ?:, :erlang.integer_to_binary(port)])
      end

    pathWithQs =
      :erlang.iolist_to_binary(
        case qs do
          <<>> ->
            path

          _ ->
            [path, ??, qs]
        end
      )

    pseudoHeaders = %{
      :method => method,
      :scheme => scheme,
      :authority => authority,
      :path => pathWithQs
    }

    headers =
      :maps.to_list(
        :maps.map(
          fn _, v ->
            :erlang.iolist_to_binary(v)
          end,
          headers0
        )
      )

    state =
      case :cow_http2_machine.prepare_push_promise(
             streamID,
             hTTP2Machine0,
             pseudoHeaders,
             headers
           ) do
        {:ok, promisedStreamID, headerBlock, hTTP2Machine} ->
          transport.send(
            socket,
            :cow_http2.push_promise(
              streamID,
              promisedStreamID,
              headerBlock
            )
          )

          headers_frame(
            r_state(state0, http2_machine: hTTP2Machine),
            promisedStreamID,
            :fin,
            headers,
            pseudoHeaders,
            0
          )

        {:error, :no_push} ->
          state0
      end

    commands(state, streamID, tail)
  end

  defp commands(state0 = r_state(flow: flow, streams: streams), streamID, [{:flow, size} | tail]) do
    %{^streamID => stream = r_stream(flow: streamFlow)} = streams

    state =
      update_window(
        r_state(state0,
          flow: flow + size,
          streams: %{streams | streamID => r_stream(stream, flow: streamFlow + size)}
        ),
        streamID
      )

    commands(state, streamID, tail)
  end

  defp commands(state = r_state(children: children), streamID, [{:spawn, pid, shutdown} | tail]) do
    commands(
      r_state(state, children: :cowboy_children.up(children, pid, streamID, shutdown)),
      streamID,
      tail
    )
  end

  defp commands(state, streamID, [error = {:internal_error, _, _} | _Tail]) do
    reset_stream(state, streamID, error)
  end

  defp commands(
         state = r_state(socket: socket, transport: transport, http2_status: :upgrade),
         streamID,
         [
           {:switch_protocol, headers, :cowboy_http2, _}
           | tail
         ]
       ) do
    transport.send(
      socket,
      :cow_http.response(101, :"HTTP/1.1", :maps.to_list(headers))
    )

    commands(state, streamID, tail)
  end

  defp commands(state0, streamID, [
         {:switch_protocol, headers, _Mod, _ModState}
         | tail
       ]) do
    state = info(state0, streamID, {:headers, 200, headers})
    commands(state, streamID, tail)
  end

  defp commands(state, streamID, [{:set_options, _Opts} | tail]) do
    commands(state, streamID, tail)
  end

  defp commands(state, streamID, [:stop | _Tail]) do
    stop_stream(state, streamID)
  end

  defp commands(state = r_state(opts: opts), streamID, [log = {:log, _, _, _} | tail]) do
    :cowboy.log(log, opts)
    commands(state, streamID, tail)
  end

  defp update_window(
         state =
           r_state(
             socket: socket,
             transport: transport,
             http2_machine: hTTP2Machine0,
             flow: flow,
             streams: streams
           ),
         streamID
       ) do
    %{^streamID => r_stream(flow: streamFlow)} = streams

    {data1, hTTP2Machine2} =
      case :cow_http2_machine.ensure_window(
             flow,
             hTTP2Machine0
           ) do
        :ok ->
          {<<>>, hTTP2Machine0}

        {:ok, increment1, hTTP2Machine1} ->
          {:cow_http2.window_update(increment1), hTTP2Machine1}
      end

    {data2, hTTP2Machine} =
      case :cow_http2_machine.ensure_window(
             streamID,
             streamFlow,
             hTTP2Machine2
           ) do
        :ok ->
          {<<>>, hTTP2Machine2}

        {:ok, increment2, hTTP2Machine3} ->
          {:cow_http2.window_update(streamID, increment2), hTTP2Machine3}
      end

    case {data1, data2} do
      {<<>>, <<>>} ->
        :ok

      _ ->
        transport.send(socket, [data1, data2])
    end

    r_state(state, http2_machine: hTTP2Machine)
  end

  defp send_response(
         state0 = r_state(http2_machine: hTTP2Machine0),
         streamID,
         statusCode,
         headers,
         body
       ) do
    size =
      case body do
        {:sendfile, _, bytes, _} ->
          bytes

        _ ->
          :erlang.iolist_size(body)
      end

    case size do
      0 ->
        state = send_headers(state0, streamID, :fin, statusCode, headers)
        maybe_terminate_stream(state, streamID, :fin)

      _ ->
        {:ok, _IsFin, headerBlock, hTTP2Machine} =
          :cow_http2_machine.prepare_headers(
            streamID,
            hTTP2Machine0,
            :nofin,
            %{:status => :cow_http.status_to_integer(statusCode)},
            headers_to_list(headers)
          )

        maybe_send_data(r_state(state0, http2_machine: hTTP2Machine), streamID, :fin, body, [
          :cow_http2.headers(streamID, :nofin, headerBlock)
        ])
    end
  end

  defp send_headers(
         state = r_state(socket: socket, transport: transport, http2_machine: hTTP2Machine0),
         streamID,
         isFin0,
         statusCode,
         headers
       ) do
    {:ok, isFin, headerBlock, hTTP2Machine} =
      :cow_http2_machine.prepare_headers(
        streamID,
        hTTP2Machine0,
        isFin0,
        %{:status => :cow_http.status_to_integer(statusCode)},
        headers_to_list(headers)
      )

    transport.send(
      socket,
      :cow_http2.headers(streamID, isFin, headerBlock)
    )

    r_state(state, http2_machine: hTTP2Machine)
  end

  defp headers_to_list(headers0 = %{"set-cookie" => setCookies}) do
    headers = :maps.to_list(:maps.remove("set-cookie", headers0))

    headers ++
      for value <- setCookies do
        {"set-cookie", value}
      end
  end

  defp headers_to_list(headers) do
    :maps.to_list(headers)
  end

  defp maybe_send_data(
         state0 = r_state(socket: socket, transport: transport, http2_machine: hTTP2Machine0),
         streamID,
         isFin,
         data0,
         prefix
       ) do
    data =
      case is_tuple(data0) do
        false ->
          {:data, data0}

        true ->
          data0
      end

    case :cow_http2_machine.send_or_queue_data(streamID, hTTP2Machine0, isFin, data) do
      {:ok, hTTP2Machine} ->
        case prefix do
          [] ->
            :ok

          _ ->
            transport.send(socket, prefix)
        end

        maybe_send_data_alarm(
          r_state(state0, http2_machine: hTTP2Machine),
          hTTP2Machine0,
          streamID
        )

      {:send, sendData, hTTP2Machine} ->
        state =
          r_state(
            http2_status: status,
            streams: streams
          ) = send_data(r_state(state0, http2_machine: hTTP2Machine), sendData, prefix)

        cond do
          status === :closing and streams === %{} ->
            terminate(state, {:stop, :normal, :"The connection is going away."})

          true ->
            maybe_send_data_alarm(state, hTTP2Machine0, streamID)
        end
    end
  end

  defp send_data(
         state0 = r_state(socket: socket, transport: transport, opts: opts),
         sendData,
         prefix
       ) do
    {acc, state} = prepare_data(state0, sendData, [], prefix)

    _ =
      for data <- acc do
        case data do
          {:sendfile, offset, bytes, path} ->
            _ =
              case :maps.get(:sendfile, opts, true) do
                true ->
                  transport.sendfile(socket, path, offset, bytes)

                false ->
                  :ranch_transport.sendfile(transport, socket, path, offset, bytes, [])
              end

          _ ->
            transport.send(socket, data)
        end
      end

    state
  end

  defp prepare_data(state, [], acc, []) do
    {:lists.reverse(acc), state}
  end

  defp prepare_data(state, [], acc, buffer) do
    {:lists.reverse([:lists.reverse(buffer) | acc]), state}
  end

  defp prepare_data(state0, [{streamID, isFin, sendData} | tail], acc0, buffer0) do
    {acc, buffer, state} = prepare_data(state0, streamID, isFin, sendData, acc0, buffer0)
    prepare_data(state, tail, acc, buffer)
  end

  defp prepare_data(state0, streamID, isFin, [], acc, buffer) do
    state = maybe_terminate_stream(state0, streamID, isFin)
    {acc, buffer, state}
  end

  defp prepare_data(state0, streamID, isFin, [frameData | tail], acc, buffer) do
    frameIsFin =
      case tail do
        [] ->
          isFin

        _ ->
          :nofin
      end

    case prepare_data_frame(state0, streamID, frameIsFin, frameData) do
      {{moreData, sendfile}, state} when is_tuple(sendfile) ->
        case buffer do
          [] ->
            prepare_data(state, streamID, isFin, tail, [[sendfile, moreData] | acc], [])

          _ ->
            prepare_data(
              state,
              streamID,
              isFin,
              tail,
              [
                [sendfile, :lists.reverse([moreData | buffer])]
                | acc
              ],
              []
            )
        end

      {moreData, state} ->
        prepare_data(state, streamID, isFin, tail, acc, [moreData | buffer])
    end
  end

  defp prepare_data_frame(state, streamID, isFin, {:data, data}) do
    {:cow_http2.data(streamID, isFin, data), state}
  end

  defp prepare_data_frame(state, streamID, isFin, sendfile = {:sendfile, _, bytes, _}) do
    {{:cow_http2.data_header(streamID, isFin, bytes), sendfile}, state}
  end

  defp prepare_data_frame(
         state = r_state(http2_machine: hTTP2Machine0),
         streamID,
         :nofin,
         {:trailers, trailers}
       ) do
    {:ok, headerBlock, hTTP2Machine} =
      :cow_http2_machine.prepare_trailers(
        streamID,
        hTTP2Machine0,
        trailers
      )

    {:cow_http2.headers(streamID, :fin, headerBlock), r_state(state, http2_machine: hTTP2Machine)}
  end

  defp maybe_send_data_alarm(
         state =
           r_state(
             opts: opts,
             http2_machine: hTTP2Machine
           ),
         hTTP2Machine0,
         streamID
       ) do
    connBufferSizeBefore = :cow_http2_machine.get_connection_local_buffer_size(hTTP2Machine0)
    connBufferSizeAfter = :cow_http2_machine.get_connection_local_buffer_size(hTTP2Machine)

    {:ok, streamBufferSizeBefore} =
      :cow_http2_machine.get_stream_local_buffer_size(
        streamID,
        hTTP2Machine0
      )

    streamBufferSizeAfter =
      case :cow_http2_machine.get_stream_local_buffer_size(
             streamID,
             hTTP2Machine
           ) do
        {:ok, bSA} ->
          bSA

        {:error, :closed} ->
          streamBufferSizeBefore
      end

    maxConnBufferSize = :maps.get(:max_connection_buffer_size, opts, 16_000_000)
    maxStreamBufferSize = :maps.get(:max_stream_buffer_size, opts, 8_000_000)

    cond do
      connBufferSizeBefore >= maxConnBufferSize and
          connBufferSizeAfter < maxConnBufferSize ->
        connection_alarm(state, :connection_buffer_full, :off)

      connBufferSizeBefore < maxConnBufferSize and
          connBufferSizeAfter >= maxConnBufferSize ->
        connection_alarm(state, :connection_buffer_full, :on)

      streamBufferSizeBefore >= maxStreamBufferSize and
          streamBufferSizeAfter < maxStreamBufferSize ->
        stream_alarm(state, streamID, :stream_buffer_full, :off)

      streamBufferSizeBefore < maxStreamBufferSize and
          streamBufferSizeAfter >= maxStreamBufferSize ->
        stream_alarm(state, streamID, :stream_buffer_full, :on)

      true ->
        state
    end
  end

  defp connection_alarm(state0 = r_state(streams: streams), name, value) do
    :lists.foldl(
      fn streamID, state ->
        stream_alarm(state, streamID, name, value)
      end,
      state0,
      :maps.keys(streams)
    )
  end

  defp stream_alarm(state, streamID, name, value) do
    info(state, streamID, {:alarm, name, value})
  end

  defp goaway(
         state0 =
           r_state(
             socket: socket,
             transport: transport,
             http2_machine: hTTP2Machine,
             http2_status: status,
             streams: streams0
           ),
         {:goaway, lastStreamID, reason, _}
       )
       when status === :connected or status === :closing do
    streams =
      goaway_streams(
        state0,
        :maps.to_list(streams0),
        lastStreamID,
        {:stop, {:goaway, reason}, :"The connection is going away."},
        []
      )

    state = r_state(state0, streams: :maps.from_list(streams))

    case status do
      :connected ->
        transport.send(
          socket,
          :cow_http2.goaway(:cow_http2_machine.get_last_streamid(hTTP2Machine), :no_error, <<>>)
        )

        r_state(state, http2_status: :closing)

      _ ->
        state
    end
  end

  defp goaway(state, {:goaway, _, reason, _}) do
    terminate(state, {:stop, {:goaway, reason}, :"The connection is going away."})
  end

  defp goaway_streams(_, [], _, _, acc) do
    acc
  end

  defp goaway_streams(
         state,
         [{streamID, r_stream(state: streamState)} | tail],
         lastStreamID,
         reason,
         acc
       )
       when streamID > lastStreamID and
              rem(streamID, 2) === 0 do
    terminate_stream_handler(state, streamID, reason, streamState)
    goaway_streams(state, tail, lastStreamID, reason, acc)
  end

  defp goaway_streams(state, [stream | tail], lastStreamID, reason, acc) do
    goaway_streams(state, tail, lastStreamID, reason, [stream | acc])
  end

  defp terminate(:undefined, reason) do
    exit({:shutdown, reason})
  end

  defp terminate(
         state =
           r_state(
             socket: socket,
             transport: transport,
             http2_status: status,
             http2_machine: hTTP2Machine,
             streams: streams,
             children: children
           ),
         reason
       )
       when status === :connected or status === :closing do
    case status do
      :connected ->
        transport.send(
          socket,
          :cow_http2.goaway(
            :cow_http2_machine.get_last_streamid(hTTP2Machine),
            terminate_reason(reason),
            <<>>
          )
        )

      :closing ->
        :ok
    end

    terminate_all_streams(state, :maps.to_list(streams), reason)
    :cowboy_children.terminate(children)
    terminate_linger(state)
    exit({:shutdown, reason})
  end

  defp terminate(
         r_state(socket: socket, transport: transport),
         reason
       ) do
    transport.close(socket)
    exit({:shutdown, reason})
  end

  defp terminate_reason({:connection_error, reason, _}) do
    reason
  end

  defp terminate_reason({:stop, _, _}) do
    :no_error
  end

  defp terminate_reason({:socket_error, _, _}) do
    :internal_error
  end

  defp terminate_reason({:internal_error, _, _}) do
    :internal_error
  end

  defp terminate_all_streams(_, [], _) do
    :ok
  end

  defp terminate_all_streams(state, [{streamID, r_stream(state: streamState)} | tail], reason) do
    terminate_stream_handler(state, streamID, reason, streamState)
    terminate_all_streams(state, tail, reason)
  end

  defp terminate_linger(state = r_state(socket: socket, transport: transport, opts: opts)) do
    case transport.shutdown(socket, :write) do
      :ok ->
        case :maps.get(:linger_timeout, opts, 1000) do
          0 ->
            :ok

          :infinity ->
            terminate_linger_before_loop(state, :undefined, transport.messages())

          timeout ->
            timerRef = :erlang.start_timer(timeout, self(), :linger_timeout)
            terminate_linger_before_loop(state, timerRef, transport.messages())
        end

      {:error, _} ->
        :ok
    end
  end

  defp terminate_linger_before_loop(state, timerRef, messages) do
    case setopts_active(state) do
      :ok ->
        terminate_linger_loop(state, timerRef, messages)

      {:error, _} ->
        :ok
    end
  end

  defp terminate_linger_loop(state = r_state(socket: socket), timerRef, messages) do
    receive do
      {oK, ^socket, _}
      when oK ===
             :erlang.element(
               1,
               messages
             ) ->
        terminate_linger_loop(state, timerRef, messages)

      {closed, ^socket}
      when closed ===
             :erlang.element(
               2,
               messages
             ) ->
        :ok

      {error, ^socket, _}
      when error ===
             :erlang.element(
               3,
               messages
             ) ->
        :ok

      {passive, ^socket}
      when passive === :tcp_passive or
             passive === :ssl_passive ->
        terminate_linger_before_loop(state, timerRef, messages)

      {:timeout, ^timerRef, :linger_timeout} ->
        :ok

      _ ->
        terminate_linger_loop(state, timerRef, messages)
    end
  end

  defp reset_stream(
         state0 = r_state(socket: socket, transport: transport, http2_machine: hTTP2Machine0),
         streamID,
         error
       ) do
    reason =
      case error do
        {:internal_error, _, _} ->
          :internal_error

        {:stream_error, reason0, _} ->
          reason0
      end

    transport.send(
      socket,
      :cow_http2.rst_stream(streamID, reason)
    )

    state1 =
      case :cow_http2_machine.reset_stream(
             streamID,
             hTTP2Machine0
           ) do
        {:ok, hTTP2Machine} ->
          terminate_stream(r_state(state0, http2_machine: hTTP2Machine), streamID, error)

        {:error, :not_found} ->
          terminate_stream(state0, streamID, error)
      end

    case reset_rate(state1) do
      {:ok, state} ->
        state

      :error ->
        terminate(
          state1,
          {:connection_error, :enhance_your_calm,
           :"Stream reset rate larger than configuration allows. Flood? (CVE-2019-9514)"}
        )
    end
  end

  defp reset_rate(state0 = r_state(opts: opts, reset_rate_num: num0, reset_rate_time: time)) do
    case num0 - 1 do
      0 ->
        currentTime = :erlang.monotonic_time(:millisecond)

        cond do
          currentTime < time ->
            :error

          true ->
            {num, period} = :maps.get(:max_reset_stream_rate, opts, {10, 10000})

            {:ok,
             r_state(state0,
               reset_rate_num: num,
               reset_rate_time: currentTime + period
             )}
        end

      num ->
        {:ok, r_state(state0, reset_rate_num: num)}
    end
  end

  defp stop_stream(
         state = r_state(http2_machine: hTTP2Machine),
         streamID
       ) do
    case :cow_http2_machine.get_stream_local_state(
           streamID,
           hTTP2Machine
         ) do
      {:ok, :idle, _} ->
        info(stopping(state, streamID), streamID, {:response, 204, %{}, <<>>})

      {:ok, :nofin, :fin} ->
        stopping(state, streamID)

      {:ok, :nofin, _} ->
        info(stopping(state, streamID), streamID, {:data, :fin, <<>>})

      _ ->
        terminate_stream(state, streamID)
    end
  end

  defp stopping(state = r_state(streams: streams), streamID) do
    %{^streamID => stream} = streams
    r_state(state, streams: %{streams | streamID => r_stream(stream, status: :stopping)})
  end

  defp maybe_terminate_stream(state = r_state(streams: streams), streamID, :fin) do
    case streams do
      %{^streamID => r_stream(status: :stopping)} ->
        terminate_stream(state, streamID)

      _ ->
        state
    end
  end

  defp maybe_terminate_stream(state, _, _) do
    state
  end

  defp terminate_stream(
         state0 = r_state(socket: socket, transport: transport, http2_machine: hTTP2Machine0),
         streamID
       ) do
    state =
      case :cow_http2_machine.get_stream_local_state(
             streamID,
             hTTP2Machine0
           ) do
        {:ok, :fin, _} ->
          transport.send(
            socket,
            :cow_http2.rst_stream(streamID, :no_error)
          )

          {:ok, hTTP2Machine} =
            :cow_http2_machine.reset_stream(
              streamID,
              hTTP2Machine0
            )

          r_state(state0, http2_machine: hTTP2Machine)

        {:error, :closed} ->
          state0
      end

    terminate_stream(state, streamID, :normal)
  end

  defp terminate_stream(
         state = r_state(flow: flow, streams: streams0, children: children0),
         streamID,
         reason
       ) do
    case :maps.take(streamID, streams0) do
      {r_stream(flow: streamFlow, state: streamState), streams} ->
        terminate_stream_handler(state, streamID, reason, streamState)

        children =
          :cowboy_children.shutdown(
            children0,
            streamID
          )

        r_state(state, flow: flow - streamFlow, streams: streams, children: children)

      :error ->
        state
    end
  end

  defp terminate_stream_handler(r_state(opts: opts), streamID, reason, streamState) do
    try do
      :cowboy_stream.terminate(streamID, reason, streamState)
    catch
      class, exception ->
        :cowboy.log(
          :cowboy_stream.make_error_log(
            :terminate,
            [streamID, reason, streamState],
            class,
            exception,
            __STACKTRACE__
          ),
          opts
        )
    end
  end

  def system_continue(_, _, {state, buffer}) do
    loop(state, buffer)
  end

  def system_terminate(reason, _, _, {state, _}) do
    terminate(state, {:stop, {:exit, reason}, :"sys:terminate/2,3 was called."})
  end

  def system_code_change(misc, _, _, _) do
    {:ok, misc}
  end
end
