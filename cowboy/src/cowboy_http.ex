defmodule :cowboy_http do
  use Bitwise
  require Record
  Record.defrecord(:r_ps_request_line, :ps_request_line, empty_lines: 0)

  Record.defrecord(:r_ps_header, :ps_header,
    method: :undefined,
    authority: :undefined,
    path: :undefined,
    qs: :undefined,
    version: :undefined,
    headers: :undefined,
    name: :undefined
  )

  Record.defrecord(:r_ps_body, :ps_body,
    length: :undefined,
    received: 0,
    transfer_decode_fun: :undefined,
    transfer_decode_state: :undefined
  )

  Record.defrecord(:r_stream, :stream,
    id: :undefined,
    state: :undefined,
    method: :undefined,
    version: :undefined,
    te: :undefined,
    local_expected_size: :undefined,
    local_sent_size: 0,
    queue: []
  )

  Record.defrecord(:r_state, :state,
    parent: :undefined,
    ref: :undefined,
    socket: :undefined,
    transport: :undefined,
    proxy_header: :undefined,
    opts: %{},
    buffer: <<>>,
    overriden_opts: %{},
    peer: :undefined,
    sock: :undefined,
    cert: :undefined,
    timer: :undefined,
    active: true,
    in_streamid: 1,
    in_state: :EFE_TODO_NESTED_RECORD,
    flow: :infinity,
    out_streamid: 1,
    out_state: :wait,
    last_streamid: :undefined,
    streams: [],
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
        state =
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
            last_streamid: :maps.get(:max_keepalive, opts, 1000)
          )

        setopts_active(state)
        loop(set_timeout(state, :request_timeout))

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

  defp setopts_active(r_state(socket: socket, transport: transport, opts: opts)) do
    n = :maps.get(:active_n, opts, 100)
    transport.setopts(socket, [{:active, n}])
  end

  defp active(state) do
    setopts_active(state)
    r_state(state, active: true)
  end

  defp passive(
         state =
           r_state(
             socket: socket,
             transport: transport
           )
       ) do
    transport.setopts(socket, [{:active, false}])
    messages = transport.messages()
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

  defp loop(
         state =
           r_state(
             parent: parent,
             socket: socket,
             transport: transport,
             opts: opts,
             buffer: buffer,
             timer: timerRef,
             children: children,
             in_streamid: inStreamID,
             last_streamid: lastStreamID
           )
       ) do
    messages = transport.messages()
    inactivityTimeout = :maps.get(:inactivity_timeout, opts, 300_000)

    receive do
      {oK, ^socket, _}
      when oK ===
             :erlang.element(
               1,
               messages
             ) and
             inStreamID > lastStreamID ->
        loop(state)

      {oK, ^socket, data}
      when oK ===
             :erlang.element(
               1,
               messages
             ) ->
        parse(<<buffer::binary, data::binary>>, state)

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
        loop(state)

      {:timeout, ref, {:shutdown, pid}} ->
        :cowboy_children.shutdown_timeout(children, ref, pid)
        loop(state)

      {:timeout, ^timerRef, reason} ->
        timeout(state, reason)

      {:timeout, _, _} ->
        loop(state)

      {:EXIT, ^parent, reason} ->
        terminate(state, {:stop, {:exit, reason}, :"Parent process terminated."})

      {:system, from, request} ->
        :sys.handle_system_msg(request, from, parent, :cowboy_http, [], state)

      {{pid, streamID}, msg} when pid === self() ->
        loop(info(state, streamID, msg))

      msg = {:EXIT, pid, _} ->
        loop(down(state, pid, msg))

      {:"$gen_call", from, call} ->
        :cowboy_children.handle_supervisor_call(call, from, children, :cowboy_http)
        loop(state)

      msg ->
        :cowboy.log(:warning, 'Received stray message ~p.~n', [msg], opts)
        loop(state)
    after
      inactivityTimeout ->
        terminate(
          state,
          {:internal_error, :timeout, :"No message or data received before timeout."}
        )
    end
  end

  defp set_timeout(
         state = r_state(streams: [_ | _]),
         :request_timeout
       ) do
    state
  end

  defp set_timeout(state = r_state(in_state: r_ps_body()), :request_timeout) do
    state
  end

  defp set_timeout(
         state = r_state(streams: [], in_state: inState),
         :idle_timeout
       )
       when :erlang.element(1, inState) !== :ps_body do
    state
  end

  defp set_timeout(
         state0 =
           r_state(
             opts: opts,
             overriden_opts: override
           ),
         name
       ) do
    state = cancel_timeout(state0)

    default =
      case name do
        :request_timeout ->
          5000

        :idle_timeout ->
          60000
      end

    timeout =
      case override do
        %{^name => timeout0} ->
          timeout0

        _ ->
          :maps.get(name, opts, default)
      end

    timerRef =
      case timeout do
        :infinity ->
          :undefined

        ^timeout ->
          :erlang.start_timer(timeout, self(), name)
      end

    r_state(state, timer: timerRef)
  end

  defp cancel_timeout(state = r_state(timer: timerRef)) do
    :ok =
      case timerRef do
        :undefined ->
          :ok

        _ ->
          _ = :erlang.cancel_timer(timerRef)

          receive do
            {:timeout, ^timerRef, _} ->
              :ok
          after
            0 ->
              :ok
          end
      end

    r_state(state, timer: :undefined)
  end

  defp timeout(state = r_state(in_state: r_ps_request_line()), :request_timeout) do
    terminate(state, {:connection_error, :timeout, :"No request-line received before timeout."})
  end

  defp timeout(state = r_state(in_state: r_ps_header()), :request_timeout) do
    error_terminate(
      408,
      state,
      {:connection_error, :timeout, :"Request headers not received before timeout."}
    )
  end

  defp timeout(state, :idle_timeout) do
    terminate(
      state,
      {:connection_error, :timeout, :"Connection idle longer than configuration allows."}
    )
  end

  defp parse(<<>>, state) do
    loop(r_state(state, buffer: <<>>))
  end

  defp parse(
         _,
         state =
           r_state(
             in_streamid: inStreamID,
             in_state: r_ps_request_line(),
             last_streamid: lastStreamID
           )
       )
       when inStreamID > lastStreamID do
    loop(r_state(state, buffer: <<>>))
  end

  defp parse(
         buffer,
         state = r_state(in_state: r_ps_request_line(empty_lines: emptyLines))
       ) do
    after_parse(parse_request(buffer, state, emptyLines))
  end

  defp parse(
         buffer,
         state =
           r_state(
             in_state:
               pS =
                 r_ps_header(
                   headers: headers,
                   name: :undefined
                 )
           )
       ) do
    after_parse(
      parse_header(
        buffer,
        r_state(state, in_state: r_ps_header(pS, headers: :undefined)),
        headers
      )
    )
  end

  defp parse(
         buffer,
         state =
           r_state(
             in_state:
               pS =
                 r_ps_header(
                   headers: headers,
                   name: name
                 )
           )
       ) do
    after_parse(
      parse_hd_before_value(
        buffer,
        r_state(state,
          in_state:
            r_ps_header(pS,
              headers: :undefined,
              name: :undefined
            )
        ),
        headers,
        name
      )
    )
  end

  defp parse(buffer, state = r_state(in_state: r_ps_body())) do
    after_parse(parse_body(buffer, state))
  end

  defp after_parse(
         {:request,
          req = %{
            :streamid => streamID,
            :method => method,
            :headers => headers,
            :version => version
          }, state0 = r_state(opts: opts, buffer: buffer, streams: streams0)}
       ) do
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

        early_error(
          500,
          state0,
          {:internal_error, {class, exception}, :"Unhandled exception in cowboy_stream:init/3."},
          req
        )

        parse(buffer, state0)
    else
      {commands, streamState} ->
        flow = :maps.get(:initial_stream_flow_size, opts, 65535)
        tE = :maps.get("te", headers, :undefined)

        streams = [
          r_stream(id: streamID, state: streamState, method: method, version: version, te: tE)
          | streams0
        ]

        state1 =
          case maybe_req_close(state0, headers, version) do
            :close ->
              r_state(state0, streams: streams, last_streamid: streamID, flow: flow)

            :keepalive ->
              r_state(state0, streams: streams, flow: flow)
          end

        state = set_timeout(state1, :idle_timeout)
        parse(buffer, commands(state, streamID, commands))
    end
  end

  defp after_parse(
         {:data, streamID, isFin, data,
          state0 =
            r_state(
              opts: opts,
              buffer: buffer,
              streams:
                streams0 = [
                  stream =
                    r_stream(
                      id: streamID,
                      state: streamState0
                    )
                  | _
                ]
            )}
       ) do
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

        stream_terminate(
          state0,
          streamID,
          {:internal_error, {class, exception}, :"Unhandled exception in cowboy_stream:data/4."}
        )
    else
      {commands, streamState} ->
        streams =
          :lists.keyreplace(
            streamID,
            r_stream(:id),
            streams0,
            r_stream(stream, state: streamState)
          )

        state1 =
          set_timeout(
            state0,
            case isFin do
              :fin ->
                :request_timeout

              :nofin ->
                :idle_timeout
            end
          )

        state = update_flow(isFin, data, r_state(state1, streams: streams))
        parse(buffer, commands(state, streamID, commands))
    end
  end

  defp after_parse({:data, _, isFin, _, state}) do
    loop(
      set_timeout(
        state,
        case isFin do
          :fin ->
            :request_timeout

          :nofin ->
            :idle_timeout
        end
      )
    )
  end

  defp after_parse({:more, state}) do
    loop(set_timeout(state, :idle_timeout))
  end

  defp update_flow(:fin, _, state) do
    r_state(state, flow: :infinity)
  end

  defp update_flow(:nofin, data, state0 = r_state(flow: flow0)) do
    flow = flow0 - byte_size(data)
    state = r_state(state0, flow: flow)

    cond do
      flow0 > 0 and flow <= 0 ->
        passive(state)

      true ->
        state
    end
  end

  defp parse_request(<<?\n, _::bits>>, state, _) do
    error_terminate(
      400,
      state,
      {:connection_error, :protocol_error,
       :"Empty lines between requests must use the CRLF line terminator. (RFC7230 3.5)"}
    )
  end

  defp parse_request(<<?\s, _::bits>>, state, _) do
    error_terminate(
      400,
      state,
      {:connection_error, :protocol_error,
       :"The request-line must not begin with a space. (RFC7230 3.1.1, RFC7230 3.5)"}
    )
  end

  defp parse_request(
         buffer,
         state = r_state(opts: opts, in_streamid: inStreamID),
         emptyLines
       ) do
    maxLength = :maps.get(:max_request_line_length, opts, 8000)
    maxEmptyLines = :maps.get(:max_empty_lines, opts, 5)

    case match_eol(buffer, 0) do
      :nomatch when byte_size(buffer) > maxLength ->
        error_terminate(
          414,
          state,
          {:connection_error, :limit_reached,
           :"The request-line length is larger than configuration allows. (RFC7230 3.1.1)"}
        )

      :nomatch ->
        {:more,
         r_state(state,
           buffer: buffer,
           in_state: r_ps_request_line(empty_lines: emptyLines)
         )}

      1 when emptyLines === maxEmptyLines ->
        error_terminate(
          400,
          state,
          {:connection_error, :limit_reached,
           :"More empty lines were received than configuration allows. (RFC7230 3.5)"}
        )

      1 ->
        <<_::size(16), rest::bits>> = buffer
        parse_request(rest, state, emptyLines + 1)

      _ ->
        case buffer do
          <<"OPTIONS * ", rest::bits>> ->
            parse_version(rest, state, "OPTIONS", :undefined, "*", <<>>)

          <<"CONNECT ", _::bits>> ->
            error_terminate(
              501,
              state,
              {:connection_error, :no_error,
               :"The CONNECT method is currently not implemented. (RFC7231 4.3.6)"}
            )

          <<"TRACE ", _::bits>> ->
            error_terminate(
              501,
              state,
              {:connection_error, :no_error,
               :"The TRACE method is currently not implemented. (RFC7231 4.3.8)"}
            )

          <<"PRI * HTTP/2.0\r\n", _::bits>> when inStreamID === 1 ->
            http2_upgrade(state, buffer)

          _ ->
            parse_method(buffer, state, <<>>, :maps.get(:max_method_length, opts, 32))
        end
    end
  end

  defp match_eol(<<?\n, _::bits>>, n) do
    n
  end

  defp match_eol(<<_, rest::bits>>, n) do
    match_eol(rest, n + 1)
  end

  defp match_eol(_, _) do
    :nomatch
  end

  defp parse_method(_, state, _, 0) do
    error_terminate(
      501,
      state,
      {:connection_error, :limit_reached,
       :"The method name is longer than configuration allows. (RFC7230 3.1.1)"}
    )
  end

  defp parse_method(<<c, rest::bits>>, state, soFar, remaining) do
    case c do
      ?\r ->
        error_terminate(
          400,
          state,
          {:connection_error, :protocol_error,
           :"The method name must not be followed with a line break. (RFC7230 3.1.1)"}
        )

      ?\s ->
        parse_uri(rest, state, soFar)

      _
      when c === ?a or c === ?b or c === ?c or c === ?d or c === ?e or c === ?f or c === ?g or
             c === ?h or c === ?i or c === ?j or c === ?k or c === ?l or c === ?m or c === ?n or
             c === ?o or c === ?p or c === ?q or c === ?r or c === ?s or c === ?t or c === ?u or
             c === ?v or c === ?w or c === ?x or c === ?y or c === ?z or c === ?A or c === ?B or
             c === ?C or c === ?D or c === ?E or c === ?F or c === ?G or c === ?H or c === ?I or
             c === ?J or c === ?K or c === ?L or c === ?M or c === ?N or c === ?O or c === ?P or
             c === ?Q or c === ?R or c === ?S or c === ?T or c === ?U or c === ?V or c === ?W or
             c === ?X or c === ?Y or c === ?Z or c === ?0 or c === ?1 or c === ?2 or c === ?3 or
             c === ?4 or c === ?5 or c === ?6 or c === ?7 or c === ?8 or c === ?9 or c === ?! or
             c === ?# or c === ?$ or c === ?% or c === ?& or c === ?' or c === ?* or c === ?+ or
             c === ?- or c === ?. or c === ?^ or c === ?_ or c === ?` or c === ?| or c === ?~ ->
        parse_method(rest, state, <<soFar::binary, c>>, remaining - 1)

      _ ->
        error_terminate(
          400,
          state,
          {:connection_error, :protocol_error,
           :"The method name must contain only valid token characters. (RFC7230 3.1.1)"}
        )
    end
  end

  defp parse_uri(<<h, t, t, p, "://", rest::bits>>, state, method)
       when h === ?h or (h === ?H and t === ?t) or t === ?T or
              p === ?p or p === ?P do
    parse_uri_authority(rest, state, method)
  end

  defp parse_uri(<<h, t, t, p, s, "://", rest::bits>>, state, method)
       when h === ?h or (h === ?H and t === ?t) or t === ?T or
              p === ?p or p === ?P or s === ?s or s === ?S do
    parse_uri_authority(rest, state, method)
  end

  defp parse_uri(<<?/, rest::bits>>, state, method) do
    parse_uri_path(rest, state, method, :undefined, <<?/>>)
  end

  defp parse_uri(_, state, _) do
    error_terminate(
      400,
      state,
      {:connection_error, :protocol_error,
       :"Invalid request-line or request-target. (RFC7230 3.1.1, RFC7230 5.3)"}
    )
  end

  defp parse_uri_authority(rest, state = r_state(opts: opts), method) do
    parse_uri_authority(rest, state, method, <<>>, :maps.get(:max_authority_length, opts, 255))
  end

  defp parse_uri_authority(_, state, _, _, 0) do
    error_terminate(
      414,
      state,
      {:connection_error, :limit_reached,
       :"The authority component of the absolute URI is longer than configuration allows. (RFC7230 2.7.1)"}
    )
  end

  defp parse_uri_authority(<<c, rest::bits>>, state, method, soFar, remaining) do
    case c do
      ?\r ->
        error_terminate(
          400,
          state,
          {:connection_error, :protocol_error,
           :"The request-target must not be followed by a line break. (RFC7230 3.1.1)"}
        )

      ?@ ->
        error_terminate(
          400,
          state,
          {:connection_error, :protocol_error,
           :"Absolute URIs must not include a userinfo component. (RFC7230 2.7.1)"}
        )

      ^c
      when soFar === <<>> and (c === ?/ or c === ?\s or c === ?? or c === ?#) ->
        error_terminate(
          400,
          state,
          {:connection_error, :protocol_error,
           :"Absolute URIs must include a non-empty host component. (RFC7230 2.7.1)"}
        )

      ?: when soFar === <<>> ->
        error_terminate(
          400,
          state,
          {:connection_error, :protocol_error,
           :"Absolute URIs must include a non-empty host component. (RFC7230 2.7.1)"}
        )

      ?/ ->
        parse_uri_path(rest, state, method, soFar, "/")

      ?\s ->
        parse_version(rest, state, method, soFar, "/", <<>>)

      ?? ->
        parse_uri_query(rest, state, method, soFar, "/", <<>>)

      ?# ->
        skip_uri_fragment(rest, state, method, soFar, "/", <<>>)

      ^c ->
        parse_uri_authority(rest, state, method, <<soFar::binary, c>>, remaining - 1)
    end
  end

  defp parse_uri_path(<<c, rest::bits>>, state, method, authority, soFar) do
    case c do
      ?\r ->
        error_terminate(
          400,
          state,
          {:connection_error, :protocol_error,
           :"The request-target must not be followed by a line break. (RFC7230 3.1.1)"}
        )

      ?\s ->
        parse_version(rest, state, method, authority, soFar, <<>>)

      ?? ->
        parse_uri_query(rest, state, method, authority, soFar, <<>>)

      ?# ->
        skip_uri_fragment(rest, state, method, authority, soFar, <<>>)

      _ ->
        parse_uri_path(rest, state, method, authority, <<soFar::binary, c>>)
    end
  end

  defp parse_uri_query(<<c, rest::bits>>, state, m, a, p, soFar) do
    case c do
      ?\r ->
        error_terminate(
          400,
          state,
          {:connection_error, :protocol_error,
           :"The request-target must not be followed by a line break. (RFC7230 3.1.1)"}
        )

      ?\s ->
        parse_version(rest, state, m, a, p, soFar)

      ?# ->
        skip_uri_fragment(rest, state, m, a, p, soFar)

      _ ->
        parse_uri_query(rest, state, m, a, p, <<soFar::binary, c>>)
    end
  end

  defp skip_uri_fragment(<<c, rest::bits>>, state, m, a, p, q) do
    case c do
      ?\r ->
        error_terminate(
          400,
          state,
          {:connection_error, :protocol_error,
           :"The request-target must not be followed by a line break. (RFC7230 3.1.1)"}
        )

      ?\s ->
        parse_version(rest, state, m, a, p, q)

      _ ->
        skip_uri_fragment(rest, state, m, a, p, q)
    end
  end

  defp parse_version(<<"HTTP/1.1\r\n", rest::bits>>, state, m, a, p, q) do
    before_parse_headers(rest, state, m, a, p, q, :"HTTP/1.1")
  end

  defp parse_version(<<"HTTP/1.0\r\n", rest::bits>>, state, m, a, p, q) do
    before_parse_headers(rest, state, m, a, p, q, :"HTTP/1.0")
  end

  defp parse_version(<<"HTTP/1.", _, c, _::bits>>, state, _, _, _, _)
       when c === ?\s or c === ?\t do
    error_terminate(
      400,
      state,
      {:connection_error, :protocol_error,
       :"Whitespace is not allowed after the HTTP version. (RFC7230 3.1.1)"}
    )
  end

  defp parse_version(<<c, _::bits>>, state, _, _, _, _)
       when c === ?\s or c === ?\t do
    error_terminate(
      400,
      state,
      {:connection_error, :protocol_error,
       :"The separator between request target and version must be a single SP. (RFC7230 3.1.1)"}
    )
  end

  defp parse_version(_, state, _, _, _, _) do
    error_terminate(
      505,
      state,
      {:connection_error, :protocol_error, :"Unsupported HTTP version. (RFC7230 2.6)"}
    )
  end

  defp before_parse_headers(rest, state, m, a, p, q, v) do
    parse_header(
      rest,
      r_state(state, in_state: r_ps_header(method: m, authority: a, path: p, qs: q, version: v)),
      %{}
    )
  end

  defp parse_header(rest, state = r_state(in_state: pS), headers)
       when byte_size(rest) < 2 do
    {:more,
     r_state(state,
       buffer: rest,
       in_state: r_ps_header(pS, headers: headers)
     )}
  end

  defp parse_header(<<?\r, ?\n, rest::bits>>, s, headers) do
    request(rest, s, headers)
  end

  defp parse_header(buffer, state = r_state(opts: opts, in_state: pS), headers) do
    maxHeaders = :maps.get(:max_headers, opts, 100)
    numHeaders = :maps.size(headers)

    cond do
      numHeaders >= maxHeaders ->
        error_terminate(
          431,
          r_state(state, in_state: r_ps_header(pS, headers: headers)),
          {:connection_error, :limit_reached,
           :"The number of headers is larger than configuration allows. (RFC7230 3.2.5, RFC6585 5)"}
        )

      true ->
        parse_header_colon(buffer, state, headers)
    end
  end

  defp parse_header_colon(buffer, state = r_state(opts: opts, in_state: pS), headers) do
    maxLength = :maps.get(:max_header_name_length, opts, 64)

    case match_colon(buffer, 0) do
      :nomatch when byte_size(buffer) > maxLength ->
        error_terminate(
          431,
          r_state(state, in_state: r_ps_header(pS, headers: headers)),
          {:connection_error, :limit_reached,
           :"A header name is larger than configuration allows. (RFC7230 3.2.5, RFC6585 5)"}
        )

      :nomatch ->
        case match_eol(buffer, 0) do
          :nomatch ->
            {:more,
             r_state(state,
               buffer: buffer,
               in_state: r_ps_header(pS, headers: headers)
             )}

          _ ->
            error_terminate(
              400,
              r_state(state, in_state: r_ps_header(pS, headers: headers)),
              {:connection_error, :protocol_error,
               :"A header line is missing a colon separator. (RFC7230 3.2.4)"}
            )
        end

      _ ->
        parse_hd_name(buffer, state, headers, <<>>)
    end
  end

  defp match_colon(<<?:, _::bits>>, n) do
    n
  end

  defp match_colon(<<_, rest::bits>>, n) do
    match_colon(rest, n + 1)
  end

  defp match_colon(_, _) do
    :nomatch
  end

  defp parse_hd_name(<<?:, rest::bits>>, state, h, soFar) do
    parse_hd_before_value(rest, state, h, soFar)
  end

  defp parse_hd_name(<<c, _::bits>>, state = r_state(in_state: pS), h, <<>>)
       when c === ?\s or c === ?\t do
    error_terminate(
      400,
      r_state(state, in_state: r_ps_header(pS, headers: h)),
      {:connection_error, :protocol_error,
       :"Whitespace is not allowed before the header name. (RFC7230 3.2)"}
    )
  end

  defp parse_hd_name(<<c, _::bits>>, state = r_state(in_state: pS), h, _)
       when c === ?\s or c === ?\t do
    error_terminate(
      400,
      r_state(state, in_state: r_ps_header(pS, headers: h)),
      {:connection_error, :protocol_error,
       :"Whitespace is not allowed between the header name and the colon. (RFC7230 3.2.4)"}
    )
  end

  defp parse_hd_name(<<c, rest::bits>>, state, h, soFar) do
    case c do
      ?A ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?a>>)

      ?B ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?b>>)

      ?C ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?c>>)

      ?D ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?d>>)

      ?E ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?e>>)

      ?F ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?f>>)

      ?G ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?g>>)

      ?H ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?h>>)

      ?I ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?i>>)

      ?J ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?j>>)

      ?K ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?k>>)

      ?L ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?l>>)

      ?M ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?m>>)

      ?N ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?n>>)

      ?O ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?o>>)

      ?P ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?p>>)

      ?Q ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?q>>)

      ?R ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?r>>)

      ?S ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?s>>)

      ?T ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?t>>)

      ?U ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?u>>)

      ?V ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?v>>)

      ?W ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?w>>)

      ?X ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?x>>)

      ?Y ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?y>>)

      ?Z ->
        parse_hd_name(rest, state, h, <<soFar::binary, ?z>>)

      ^c ->
        parse_hd_name(rest, state, h, <<soFar::binary, c>>)
    end
  end

  defp parse_hd_before_value(<<?\s, rest::bits>>, s, h, n) do
    parse_hd_before_value(rest, s, h, n)
  end

  defp parse_hd_before_value(<<?\t, rest::bits>>, s, h, n) do
    parse_hd_before_value(rest, s, h, n)
  end

  defp parse_hd_before_value(buffer, state = r_state(opts: opts, in_state: pS), h, n) do
    maxLength = :maps.get(:max_header_value_length, opts, 4096)

    case match_eol(buffer, 0) do
      :nomatch when byte_size(buffer) > maxLength ->
        error_terminate(
          431,
          r_state(state, in_state: r_ps_header(pS, headers: h)),
          {:connection_error, :limit_reached,
           :"A header value is larger than configuration allows. (RFC7230 3.2.5, RFC6585 5)"}
        )

      :nomatch ->
        {:more,
         r_state(state,
           buffer: buffer,
           in_state: r_ps_header(pS, headers: h, name: n)
         )}

      _ ->
        parse_hd_value(buffer, state, h, n, <<>>)
    end
  end

  defp parse_hd_value(<<?\r, ?\n, rest::bits>>, s, headers0, name, soFar) do
    value = clean_value_ws_end(soFar, byte_size(soFar) - 1)

    headers =
      case :maps.get(name, headers0, :undefined) do
        :undefined ->
          %{headers0 | name => value}

        value0 when name === "cookie" ->
          %{headers0 | name => <<value0::binary, "; ", value::binary>>}

        value0 ->
          %{headers0 | name => <<value0::binary, ", ", value::binary>>}
      end

    parse_header(rest, s, headers)
  end

  defp parse_hd_value(<<c, rest::bits>>, s, h, n, soFar) do
    parse_hd_value(rest, s, h, n, <<soFar::binary, c>>)
  end

  defp clean_value_ws_end(_, -1) do
    <<>>
  end

  defp clean_value_ws_end(value, n) do
    case :binary.at(value, n) do
      ?\s ->
        clean_value_ws_end(value, n - 1)

      ?\t ->
        clean_value_ws_end(value, n - 1)

      _ ->
        s = n + 1
        <<value2::size(s)-binary, _::bits>> = value
        value2
    end
  end

  defp request(
         buffer,
         state =
           r_state(
             transport: transport,
             in_state:
               pS =
                 r_ps_header(
                   authority: authority,
                   version: version
                 )
           ),
         headers
       ) do
    case :maps.get("host", headers, :undefined) do
      :undefined when version === :"HTTP/1.1" ->
        error_terminate(
          400,
          r_state(state, in_state: r_ps_header(pS, headers: headers)),
          {:stream_error, :protocol_error,
           :"HTTP/1.1 requests must include a host header. (RFC7230 5.4)"}
        )

      :undefined ->
        request(buffer, state, headers, <<>>, default_port(transport.secure()))

      rawHost
      when authority === :undefined or
             authority === rawHost ->
        request_parse_host(buffer, state, headers, rawHost)

      _ ->
        error_terminate(
          400,
          r_state(state, in_state: r_ps_header(pS, headers: headers)),
          {:stream_error, :protocol_error,
           :"The host header is different than the absolute-form authority component. (RFC7230 5.4)"}
        )
    end
  end

  defp request_parse_host(
         buffer,
         state = r_state(transport: transport, in_state: pS),
         headers,
         rawHost
       ) do
    try do
      :cow_http_hd.parse_host(rawHost)
    catch
      _, _ ->
        error_terminate(
          400,
          r_state(state, in_state: r_ps_header(pS, headers: headers)),
          {:stream_error, :protocol_error, :"The host header is invalid. (RFC7230 5.4)"}
        )
    else
      {host, :undefined} ->
        request(buffer, state, headers, host, default_port(transport.secure()))

      {host, port} when port > 0 and port <= 65535 ->
        request(buffer, state, headers, host, port)

      _ ->
        error_terminate(
          400,
          state,
          {:stream_error, :protocol_error,
           :"The port component of the absolute-form is not in the range 0..65535. (RFC7230 2.7.1)"}
        )
    end
  end

  defp default_port(true) do
    443
  end

  defp default_port(_) do
    80
  end

  defp request(
         buffer,
         state0 =
           r_state(
             ref: ref,
             transport: transport,
             peer: peer,
             sock: sock,
             cert: cert,
             proxy_header: proxyHeader,
             in_streamid: streamID,
             in_state: pS = r_ps_header(method: method, path: path, qs: qs, version: version)
           ),
         headers0,
         host,
         port
       ) do
    scheme =
      case transport.secure() do
        true ->
          "https"

        false ->
          "http"
      end

    {headers, hasBody, bodyLength, tDecodeFun, tDecodeState} =
      case headers0 do
        %{"transfer-encoding" => transferEncoding0} ->
          try do
            :cow_http_hd.parse_transfer_encoding(transferEncoding0)
          catch
            _, _ ->
              error_terminate(
                400,
                r_state(state0, in_state: r_ps_header(pS, headers: headers0)),
                {:stream_error, :protocol_error,
                 :"The transfer-encoding header is invalid. (RFC7230 3.3.1)"}
              )
          else
            ["chunked"] ->
              {:maps.remove("content-length", headers0), true, :undefined,
               &:cow_http_te.stream_chunked/2, {0, 0}}

            _ ->
              error_terminate(
                400,
                r_state(state0, in_state: r_ps_header(pS, headers: headers0)),
                {:stream_error, :protocol_error,
                 :"Cowboy only supports transfer-encoding: chunked. (RFC7230 3.3.1)"}
              )
          end

        %{"content-length" => "0"} ->
          {headers0, false, 0, :undefined, :undefined}

        %{"content-length" => binLength} ->
          length =
            try do
              :cow_http_hd.parse_content_length(binLength)
            catch
              _, _ ->
                error_terminate(
                  400,
                  r_state(state0, in_state: r_ps_header(pS, headers: headers0)),
                  {:stream_error, :protocol_error,
                   :"The content-length header is invalid. (RFC7230 3.3.2)"}
                )
            end

          {headers0, true, length, &:cow_http_te.stream_identity/2, {0, length}}

        _ ->
          {headers0, false, 0, :undefined, :undefined}
      end

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
      :version => version,
      :headers => :maps.remove("transfer-encoding", headers),
      :has_body => hasBody,
      :body_length => bodyLength
    }

    req =
      case proxyHeader do
        :undefined ->
          req0

        _ ->
          %{req0 | :proxy_header => proxyHeader}
      end

    case is_http2_upgrade(headers, version) do
      false ->
        state =
          case hasBody do
            true ->
              r_state(state0,
                in_state:
                  r_ps_body(
                    length: bodyLength,
                    transfer_decode_fun: tDecodeFun,
                    transfer_decode_state: tDecodeState
                  )
              )

            false ->
              r_state(state0, in_streamid: streamID + 1, in_state: r_ps_request_line())
          end

        {:request, req, r_state(state, buffer: buffer)}

      {true, hTTP2Settings} ->
        http2_upgrade(
          r_state(state0, in_state: r_ps_header(pS, headers: headers)),
          buffer,
          hTTP2Settings,
          req
        )
    end
  end

  defp is_http2_upgrade(
         %{"connection" => conn, "upgrade" => upgrade, "http2-settings" => hTTP2Settings},
         :"HTTP/1.1"
       ) do
    conns = :cow_http_hd.parse_connection(conn)

    case {:lists.member("upgrade", conns), :lists.member("http2-settings", conns)} do
      {true, true} ->
        protocols = :cow_http_hd.parse_upgrade(upgrade)

        case :lists.member("h2c", protocols) do
          true ->
            {true, hTTP2Settings}

          false ->
            false
        end

      _ ->
        false
    end
  end

  defp is_http2_upgrade(_, _) do
    false
  end

  defp http2_upgrade(
         state =
           r_state(
             parent: parent,
             ref: ref,
             socket: socket,
             transport: transport,
             proxy_header: proxyHeader,
             opts: opts,
             peer: peer,
             sock: sock,
             cert: cert
           ),
         buffer
       ) do
    case transport.secure() do
      false ->
        _ = cancel_timeout(state)

        :cowboy_http2.init(
          parent,
          ref,
          socket,
          transport,
          proxyHeader,
          opts,
          peer,
          sock,
          cert,
          buffer
        )

      true ->
        error_terminate(
          400,
          state,
          {:connection_error, :protocol_error,
           :"Clients that support HTTP/2 over TLS MUST use ALPN. (RFC7540 3.4)"}
        )
    end
  end

  defp http2_upgrade(
         state =
           r_state(
             parent: parent,
             ref: ref,
             socket: socket,
             transport: transport,
             proxy_header: proxyHeader,
             opts: opts,
             peer: peer,
             sock: sock,
             cert: cert
           ),
         buffer,
         hTTP2Settings,
         req
       ) do
    try do
      :cow_http_hd.parse_http2_settings(hTTP2Settings)
    catch
      _, _ ->
        error_terminate(
          400,
          state,
          {:connection_error, :protocol_error,
           :"The HTTP2-Settings header must contain a base64 SETTINGS payload. (RFC7540 3.2, RFC7540 3.2.1)"}
        )
    else
      settings ->
        _ = cancel_timeout(state)

        :cowboy_http2.init(
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
          settings,
          req
        )
    end
  end

  defp parse_body(
         buffer,
         state =
           r_state(
             in_streamid: streamID,
             in_state:
               pS =
                 r_ps_body(
                   received: received,
                   transfer_decode_fun: tDecode,
                   transfer_decode_state: tState0
                 )
           )
       ) do
    try do
      tDecode.(buffer, tState0)
    catch
      _, _ ->
        reason =
          {:connection_error, :protocol_error, :"Failure to decode the content. (RFC7230 4)"}

        terminate(
          stream_terminate(state, streamID, reason),
          reason
        )
    else
      :more ->
        {:more, r_state(state, buffer: buffer)}

      {:more, data, tState} ->
        {:data, streamID, :nofin, data,
         r_state(state,
           buffer: <<>>,
           in_state:
             r_ps_body(pS,
               received: received + byte_size(data),
               transfer_decode_state: tState
             )
         )}

      {:more, data, _Length, tState} when is_integer(_Length) ->
        {:data, streamID, :nofin, data,
         r_state(state,
           buffer: <<>>,
           in_state:
             r_ps_body(pS,
               received: received + byte_size(data),
               transfer_decode_state: tState
             )
         )}

      {:more, data, rest, tState} ->
        {:data, streamID, :nofin, data,
         r_state(state,
           buffer: rest,
           in_state:
             r_ps_body(pS,
               received: received + byte_size(data),
               transfer_decode_state: tState
             )
         )}

      {:done, _HasTrailers, rest} ->
        {:data, streamID, :fin, <<>>,
         r_state(state, buffer: rest, in_streamid: streamID + 1, in_state: r_ps_request_line())}

      {:done, data, _HasTrailers, rest} ->
        {:data, streamID, :fin, data,
         r_state(state, buffer: rest, in_streamid: streamID + 1, in_state: r_ps_request_line())}
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

  defp info(state = r_state(opts: opts, streams: streams0), streamID, msg) do
    case :lists.keyfind(streamID, r_stream(:id), streams0) do
      stream = r_stream(state: streamState0) ->
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

            stream_terminate(
              state,
              streamID,
              {:internal_error, {class, exception},
               :"Unhandled exception in cowboy_stream:info/3."}
            )
        else
          {commands, streamState} ->
            streams =
              :lists.keyreplace(
                streamID,
                r_stream(:id),
                streams0,
                r_stream(stream, state: streamState)
              )

            commands(r_state(state, streams: streams), streamID, commands)
        end

      false ->
        :cowboy.log(
          :warning,
          'Received message ~p for unknown stream ~p.~n',
          [msg, streamID],
          opts
        )

        state
    end
  end

  defp commands(state, _, []) do
    state
  end

  defp commands(state = r_state(children: children), streamID, [{:spawn, pid, shutdown} | tail]) do
    commands(
      r_state(state, children: :cowboy_children.up(children, pid, streamID, shutdown)),
      streamID,
      tail
    )
  end

  defp commands(state, streamID, [error = {:internal_error, _, _} | tail]) do
    commands(stream_terminate(state, streamID, error), streamID, tail)
  end

  defp commands(
         state =
           r_state(
             out_streamid: current,
             streams: streams0
           ),
         streamID,
         commands
       )
       when current !== streamID do
    stream = r_stream(queue: queue) = :lists.keyfind(streamID, r_stream(:id), streams0)

    streams =
      :lists.keyreplace(
        streamID,
        r_stream(:id),
        streams0,
        r_stream(stream, queue: queue ++ commands)
      )

    r_state(state, streams: streams)
  end

  defp commands(state = r_state(flow: :infinity), streamID, [{:flow, _} | tail]) do
    commands(state, streamID, tail)
  end

  defp commands(state0 = r_state(flow: flow0), streamID, [{:flow, size} | tail]) do
    flow =
      cond do
        flow0 < 0 ->
          size

        true ->
          flow0 + size
      end

    state =
      cond do
        flow0 <= 0 and flow > 0 ->
          active(state0)

        true ->
          state0
      end

    commands(r_state(state, flow: flow), streamID, tail)
  end

  defp commands(
         state =
           r_state(
             out_state: :wait,
             out_streamid: streamID
           ),
         streamID,
         [{:error_response, status, headers0, body} | tail]
       ) do
    headers =
      case status do
        408 ->
          %{headers0 | "connection" => "close"}

        <<"408", _::bits>> ->
          %{headers0 | "connection" => "close"}

        _ ->
          headers0
      end

    commands(state, streamID, [{:response, status, headers, body} | tail])
  end

  defp commands(state, streamID, [{:error_response, _, _, _} | tail]) do
    commands(state, streamID, tail)
  end

  defp commands(
         state =
           r_state(socket: socket, transport: transport, out_state: :wait, streams: streams),
         streamID,
         [{:inform, statusCode, headers} | tail]
       ) do
    r_stream(version: version) = :lists.keyfind(streamID, r_stream(:id), streams)

    _ =
      case version do
        :"HTTP/1.1" ->
          transport.send(
            socket,
            :cow_http.response(statusCode, :"HTTP/1.1", headers_to_list(headers))
          )

        :"HTTP/1.0" ->
          :ok
      end

    commands(state, streamID, tail)
  end

  defp commands(
         state0 =
           r_state(socket: socket, transport: transport, out_state: :wait, streams: streams),
         streamID,
         [{:response, statusCode, headers0, body} | tail]
       ) do
    r_stream(version: version) = :lists.keyfind(streamID, r_stream(:id), streams)
    {state1, headers} = connection(state0, headers0, streamID, version)
    state = r_state(state1, out_state: :done)
    response = :cow_http.response(statusCode, :"HTTP/1.1", headers_to_list(headers))

    case body do
      {:sendfile, _, _, _} ->
        transport.send(socket, response)
        sendfile(state, body)

      _ ->
        transport.send(socket, [response, body])
    end

    commands(state, streamID, tail)
  end

  defp commands(
         state0 =
           r_state(
             socket: socket,
             transport: transport,
             opts: opts,
             overriden_opts: override,
             streams: streams0,
             out_state: outState
           ),
         streamID,
         [{:headers, statusCode, headers0} | tail]
       ) do
    stream = r_stream(version: version) = :lists.keyfind(streamID, r_stream(:id), streams0)
    status = :cow_http.status_to_integer(statusCode)
    contentLength = :maps.get("content-length", headers0, :undefined)

    chunked =
      case override do
        %{:chunked => chunked0} ->
          chunked0

        _ ->
          :maps.get(:chunked, opts, true)
      end

    {state1, headers1} =
      case {status, contentLength, version} do
        {204, _, :"HTTP/1.1"} ->
          {r_state(state0, out_state: :done), headers0}

        {304, _, :"HTTP/1.1"} ->
          {r_state(state0, out_state: :done), headers0}

        {_, :undefined, :"HTTP/1.1"} when chunked ->
          {r_state(state0, out_state: :chunked), %{headers0 | "transfer-encoding" => "chunked"}}

        {_, :undefined, _} ->
          {r_state(state0,
             out_state: :streaming,
             last_streamid: streamID
           ), headers0}

        _ ->
          expectedSize = :cow_http_hd.parse_content_length(contentLength)

          streams =
            :lists.keyreplace(
              streamID,
              r_stream(:id),
              streams0,
              r_stream(stream, local_expected_size: expectedSize)
            )

          {r_state(state0,
             out_state: :streaming,
             streams: streams
           ), headers0}
      end

    headers2 =
      case stream_te(outState, stream) do
        :trailers ->
          headers1

        _ ->
          :maps.remove("trailer", headers1)
      end

    {state, headers} = connection(state1, headers2, streamID, version)

    transport.send(
      socket,
      :cow_http.response(statusCode, :"HTTP/1.1", headers_to_list(headers))
    )

    commands(state, streamID, tail)
  end

  defp commands(
         state0 =
           r_state(socket: socket, transport: transport, streams: streams0, out_state: outState),
         streamID,
         [{:data, isFin, data} | tail]
       ) do
    size =
      case data do
        {:sendfile, _, b, _} ->
          b

        _ ->
          :erlang.iolist_size(data)
      end

    stream =
      case :lists.keyfind(streamID, r_stream(:id), streams0) do
        stream0 = r_stream(method: "HEAD") ->
          stream0

        stream0
        when size === 0 and isFin === :fin and
               outState === :chunked ->
          transport.send(socket, "0\r\n\r\n")
          stream0

        stream0 when size === 0 ->
          stream0

        stream0 when is_tuple(data) and outState === :chunked ->
          transport.send(
            socket,
            [:erlang.integer_to_binary(size, 16), "\r\n"]
          )

          sendfile(state0, data)

          transport.send(
            socket,
            case isFin do
              :fin ->
                "\r\n0\r\n\r\n"

              :nofin ->
                "\r\n"
            end
          )

          stream0

        stream0 when outState === :chunked ->
          transport.send(
            socket,
            [
              :erlang.integer_to_binary(size, 16),
              "\r\n",
              data,
              case isFin do
                :fin ->
                  "\r\n0\r\n\r\n"

                :nofin ->
                  "\r\n"
              end
            ]
          )

          stream0

        stream0 when outState === :streaming ->
          r_stream(
            local_sent_size: sentSize0,
            local_expected_size: expectedSize
          ) = stream0

          sentSize = sentSize0 + size

          cond do
            sentSize > expectedSize ->
              terminate(state0, :response_body_too_large)

            is_tuple(data) ->
              sendfile(state0, data)

            true ->
              transport.send(socket, data)
          end

          r_stream(stream0, local_sent_size: sentSize)
      end

    state =
      case isFin do
        :fin ->
          r_state(state0, out_state: :done)

        :nofin ->
          state0
      end

    streams = :lists.keyreplace(streamID, r_stream(:id), streams0, stream)
    commands(r_state(state, streams: streams), streamID, tail)
  end

  defp commands(
         state =
           r_state(socket: socket, transport: transport, streams: streams, out_state: outState),
         streamID,
         [{:trailers, trailers} | tail]
       ) do
    case stream_te(
           outState,
           :lists.keyfind(streamID, r_stream(:id), streams)
         ) do
      :trailers ->
        transport.send(
          socket,
          ["0\r\n", :cow_http.headers(:maps.to_list(trailers)), "\r\n"]
        )

      :no_trailers ->
        transport.send(socket, "0\r\n\r\n")

      :not_chunked ->
        :ok
    end

    commands(r_state(state, out_state: :done), streamID, tail)
  end

  defp commands(
         state0 =
           r_state(
             ref: ref,
             parent: parent,
             socket: socket,
             transport: transport,
             out_state: outState,
             opts: opts,
             buffer: buffer,
             children: children
           ),
         streamID,
         [
           {:switch_protocol, headers, protocol, initialState}
           | _Tail
         ]
       ) do
    state1 = cancel_timeout(state0)
    state = passive(state1)

    r_state(streams: streams) =
      case outState do
        :wait ->
          info(state, streamID, {:inform, 101, headers})

        _ ->
          state
      end

    r_stream(state: streamState) = :lists.keyfind(streamID, r_stream(:id), streams)
    stream_call_terminate(streamID, :switch_protocol, streamState, state)
    :cowboy_children.terminate(children)
    flush(parent)
    protocol.takeover(parent, ref, socket, transport, opts, buffer, initialState)
  end

  defp commands(state0 = r_state(overriden_opts: opts), streamID, [{:set_options, setOpts} | tail]) do
    state1 =
      case setOpts do
        %{:idle_timeout => idleTimeout} ->
          set_timeout(
            r_state(state0, overriden_opts: %{opts | :idle_timeout => idleTimeout}),
            :idle_timeout
          )

        _ ->
          state0
      end

    state =
      case setOpts do
        %{:chunked => chunked} ->
          r_state(state1, overriden_opts: %{opts | :chunked => chunked})

        _ ->
          state1
      end

    commands(state, streamID, tail)
  end

  defp commands(state, streamID, [:stop | tail]) do
    maybe_terminate(state, streamID, tail)
  end

  defp commands(state = r_state(opts: opts), streamID, [log = {:log, _, _, _} | tail]) do
    :cowboy.log(log, opts)
    commands(state, streamID, tail)
  end

  defp commands(state, streamID, [{:push, _, _, _, _, _, _, _} | tail]) do
    commands(state, streamID, tail)
  end

  defp headers_to_list(headers0 = %{"set-cookie" => setCookies}) do
    headers1 = :maps.to_list(:maps.remove("set-cookie", headers0))

    headers1 ++
      for value <- setCookies do
        {"set-cookie", value}
      end
  end

  defp headers_to_list(headers) do
    :maps.to_list(headers)
  end

  defp sendfile(
         state = r_state(socket: socket, transport: transport, opts: opts),
         {:sendfile, offset, bytes, path}
       ) do
    try do
      _ =
        case :maps.get(:sendfile, opts, true) do
          true ->
            transport.sendfile(socket, path, offset, bytes)

          false ->
            :ranch_transport.sendfile(transport, socket, path, offset, bytes, [])
        end

      :ok
    catch
      _, _ ->
        terminate(
          state,
          {:socket_error, :sendfile_crash, :"An error occurred when using the sendfile function."}
        )
    end
  end

  defp flush(parent) do
    receive do
      {:timeout, _, _} ->
        flush(parent)

      {{pid, _}, _} when pid === self() ->
        flush(parent)

      {:EXIT, pid, _} when pid !== parent ->
        flush(parent)
    after
      0 ->
        :ok
    end
  end

  defp maybe_terminate(state = r_state(last_streamid: streamID), streamID, _Tail) do
    terminate(
      stream_terminate(state, streamID, :normal),
      :normal
    )
  end

  defp maybe_terminate(state, streamID, _Tail) do
    stream_terminate(state, streamID, :normal)
  end

  defp stream_terminate(
         state0 =
           r_state(
             opts: opts,
             in_streamid: inStreamID,
             in_state: inState,
             out_streamid: outStreamID,
             out_state: outState,
             streams: streams0,
             children: children0
           ),
         streamID,
         reason
       ) do
    r_stream(version: version, local_expected_size: expectedSize, local_sent_size: sentSize) =
      :lists.keyfind(streamID, r_stream(:id), streams0)

    state1 =
      r_state(streams: streams1) =
      case outState do
        :wait
        when :erlang.element(
               1,
               reason
             ) === :internal_error ->
          info(state0, streamID, {:response, 500, %{"content-length" => "0"}, <<>>})

        :wait
        when :erlang.element(
               1,
               reason
             ) === :connection_error ->
          info(state0, streamID, {:response, 400, %{"content-length" => "0"}, <<>>})

        :wait ->
          info(state0, streamID, {:response, 204, %{}, <<>>})

        :chunked when version === :"HTTP/1.1" ->
          info(state0, streamID, {:data, :fin, <<>>})

        :streaming when sentSize < expectedSize ->
          terminate(
            state0,
            :response_body_too_small
          )

        _ ->
          state0
      end

    {:value, r_stream(state: streamState), streams} =
      :lists.keytake(streamID, r_stream(:id), streams1)

    stream_call_terminate(streamID, reason, streamState, state1)

    children =
      :cowboy_children.shutdown(
        children0,
        streamID
      )

    state = r_state(state1, overriden_opts: %{}, streams: streams, children: children)
    maxSkipBodyLength = :maps.get(:max_skip_body_length, opts, 1_000_000)

    case inState do
      r_ps_body(length: :undefined) when inStreamID === outStreamID ->
        terminate(state, :skip_body_unknown_length)

      r_ps_body(length: len, received: received)
      when inStreamID === outStreamID and
             received + maxSkipBodyLength < len ->
        terminate(state, :skip_body_too_large)

      r_ps_body() when inStreamID === outStreamID ->
        stream_next(r_state(state, flow: :infinity))

      _ ->
        stream_next(state)
    end
  end

  defp stream_next(
         state0 = r_state(opts: opts, active: active, out_streamid: outStreamID, streams: streams)
       ) do
    nextOutStreamID = outStreamID + 1

    case :lists.keyfind(nextOutStreamID, r_stream(:id), streams) do
      false ->
        r_state(state0,
          out_streamid: nextOutStreamID,
          out_state: :wait
        )

      r_stream(queue: commands) ->
        state =
          case active do
            true ->
              state0

            false ->
              active(state0)
          end

        flow = :maps.get(:initial_stream_flow_size, opts, 65535)

        commands(
          r_state(state, flow: flow, out_streamid: nextOutStreamID, out_state: :wait),
          nextOutStreamID,
          commands
        )
    end
  end

  defp stream_call_terminate(streamID, reason, streamState, r_state(opts: opts)) do
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

  defp maybe_req_close(r_state(opts: %{:http10_keepalive => false}), _, :"HTTP/1.0") do
    :close
  end

  defp maybe_req_close(_, %{"connection" => conn}, :"HTTP/1.0") do
    conns = :cow_http_hd.parse_connection(conn)

    case :lists.member("keep-alive", conns) do
      true ->
        :keepalive

      false ->
        :close
    end
  end

  defp maybe_req_close(_, _, :"HTTP/1.0") do
    :close
  end

  defp maybe_req_close(_, %{"connection" => conn}, :"HTTP/1.1") do
    case connection_hd_is_close(conn) do
      true ->
        :close

      false ->
        :keepalive
    end
  end

  defp maybe_req_close(_, _, _) do
    :keepalive
  end

  defp connection(
         state = r_state(last_streamid: streamID),
         headers = %{"connection" => conn},
         streamID,
         _
       ) do
    case connection_hd_is_close(conn) do
      true ->
        {state, headers}

      false ->
        {state, %{headers | "connection" => ["close, ", conn]}}
    end
  end

  defp connection(state = r_state(last_streamid: streamID), headers, streamID, _) do
    {state, %{headers | "connection" => "close"}}
  end

  defp connection(state, headers = %{"connection" => conn}, streamID, _) do
    case connection_hd_is_close(conn) do
      true ->
        {r_state(state, last_streamid: streamID), headers}

      false ->
        {state, headers}
    end
  end

  defp connection(state, headers, _, :"HTTP/1.0") do
    {state, %{headers | "connection" => "keep-alive"}}
  end

  defp connection(state, headers, _, _) do
    {state, headers}
  end

  defp connection_hd_is_close(conn) do
    conns = :cow_http_hd.parse_connection(:erlang.iolist_to_binary(conn))
    :lists.member("close", conns)
  end

  defp stream_te(:streaming, _) do
    :not_chunked
  end

  defp stream_te(_, r_stream(te: :undefined)) do
    :no_trailers
  end

  defp stream_te(_, r_stream(te: tE0)) do
    try do
      :cow_http_hd.parse_te(tE0)
    catch
      _, _ ->
        :no_trailers
    else
      {tE1, _} ->
        tE1
    end
  end

  defp error_terminate(
         statusCode,
         state = r_state(ref: ref, peer: peer, in_state: streamState),
         reason
       ) do
    partialReq =
      case streamState do
        r_ps_request_line() ->
          %{:ref => ref, :peer => peer}

        r_ps_header(method: method, path: path, qs: qs, version: version, headers: reqHeaders) ->
          %{
            :ref => ref,
            :peer => peer,
            :method => method,
            :path => path,
            :qs => qs,
            :version => version,
            :headers =>
              case reqHeaders do
                :undefined ->
                  %{}

                _ ->
                  reqHeaders
              end
          }
      end

    early_error(statusCode, state, reason, partialReq, %{"connection" => "close"})
    terminate(state, reason)
  end

  defp early_error(statusCode, state, reason, partialReq) do
    early_error(statusCode, state, reason, partialReq, %{})
  end

  defp early_error(
         statusCode0,
         r_state(socket: socket, transport: transport, opts: opts, in_streamid: streamID),
         reason,
         partialReq,
         respHeaders0
       ) do
    respHeaders1 = %{respHeaders0 | "content-length" => "0"}
    resp = {:response, statusCode0, respHeaders1, <<>>}

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

        transport.send(
          socket,
          :cow_http.response(statusCode0, :"HTTP/1.1", :maps.to_list(respHeaders1))
        )
    else
      {:response, statusCode, respHeaders, respBody} ->
        transport.send(
          socket,
          [:cow_http.response(statusCode, :"HTTP/1.1", :maps.to_list(respHeaders)), respBody]
        )
    end

    :ok
  end

  defp terminate(:undefined, reason) do
    exit({:shutdown, reason})
  end

  defp terminate(
         state = r_state(streams: streams, children: children),
         reason
       ) do
    terminate_all_streams(state, streams, reason)
    :cowboy_children.terminate(children)
    terminate_linger(state)
    exit({:shutdown, reason})
  end

  defp terminate_all_streams(_, [], _) do
    :ok
  end

  defp terminate_all_streams(state, [r_stream(id: streamID, state: streamState) | tail], reason) do
    stream_call_terminate(streamID, reason, streamState, state)
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

  def system_continue(_, _, state) do
    loop(state)
  end

  def system_terminate(reason, _, _, state) do
    terminate(state, {:stop, {:exit, reason}, :"sys:terminate/2,3 was called."})
  end

  def system_code_change(misc, _, _, _) do
    {:ok, misc}
  end
end
