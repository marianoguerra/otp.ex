defmodule :cow_http2_machine do
  use Bitwise
  require Record
  Record.defrecord(:r_sendfile, :sendfile, offset: :undefined,
                                    bytes: :undefined, path: :undefined)
  Record.defrecord(:r_stream, :stream, id: :undefined,
                                  method: :undefined, local: :idle,
                                  local_window: :undefined,
                                  local_buffer: :queue.new(),
                                  local_buffer_size: 0,
                                  local_trailers: :undefined, remote: :idle,
                                  remote_window: :undefined,
                                  remote_expected_size: :undefined,
                                  remote_read_size: 0, te: :undefined)
  Record.defrecord(:r_http2_machine, :http2_machine, mode: :undefined,
                                         opts: %{}, state: :settings,
                                         preface_timer: :undefined,
                                         settings_timer: :undefined,
                                         local_settings: %{initial_window_size:
                                                           65535},
                                         next_settings: :undefined,
                                         remote_settings: %{initial_window_size:
                                                            65535},
                                         local_window: 65535,
                                         remote_window: 65535,
                                         local_streamid: :undefined,
                                         remote_streamid: 0, streams: %{},
                                         local_lingering_streams: [],
                                         remote_lingering_streams: [],
                                         decode_state: :cow_hpack.init(),
                                         encode_state: :cow_hpack.init())
  def init(:client, opts) do
    nextSettings = settings_init(opts)
    client_preface(r_http2_machine(mode: :client, opts: opts,
                       preface_timer: start_timer(:preface_timeout, opts),
                       settings_timer: start_timer(:settings_timeout, opts),
                       next_settings: nextSettings, local_streamid: 1))
  end

  def init(:server, opts) do
    nextSettings = settings_init(opts)
    common_preface(r_http2_machine(mode: :server, opts: opts,
                       preface_timer: start_timer(:preface_timeout, opts),
                       settings_timer: start_timer(:settings_timeout, opts),
                       next_settings: nextSettings, local_streamid: 2))
  end

  defp start_timer(name, opts = %{message_tag: messageTag}) do
    case (:maps.get(name, opts, 5000)) do
      :infinity ->
        :undefined
      timeout ->
        :erlang.start_timer(timeout, self(),
                              {:cow_http2_machine, messageTag, name})
    end
  end

  defp start_timer(name, opts) do
    case (:maps.get(name, opts, 5000)) do
      :infinity ->
        :undefined
      timeout ->
        :erlang.start_timer(timeout, self(),
                              {:cow_http2_machine, name})
    end
  end

  defp client_preface(state0) do
    {:ok, commonPreface, state} = common_preface(state0)
    {:ok, ["PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", commonPreface], state}
  end

  defp common_preface(state = r_http2_machine(opts: opts,
                      next_settings: nextSettings)) do
    case (:maps.get(:initial_connection_window_size, opts,
                      65535)) do
      65535 ->
        {:ok, :cow_http2.settings(nextSettings), state}
      size ->
        {:ok,
           [:cow_http2.settings(nextSettings),
                :cow_http2.window_update(size - 65535)],
           update_window(size - 65535, state)}
    end
  end

  defp settings_init(opts) do
    s0 = setting_from_opt(%{}, opts, :max_decode_table_size,
                            :header_table_size, 4096)
    s1 = setting_from_opt(s0, opts, :max_concurrent_streams,
                            :max_concurrent_streams, :infinity)
    s2 = setting_from_opt(s1, opts,
                            :initial_stream_window_size, :initial_window_size,
                            65535)
    s3 = setting_from_opt(s2, opts,
                            :max_frame_size_received, :max_frame_size, 16384)
    setting_from_opt(s3, opts, :enable_connect_protocol,
                       :enable_connect_protocol, false)
  end

  defp setting_from_opt(settings, opts, optName, settingName,
            default) do
    case (:maps.get(optName, opts, default)) do
      ^default ->
        settings
      value ->
        Map.put(settings, settingName, value)
    end
  end

  def init_stream(method,
           state = r_http2_machine(mode: :client, local_streamid: localStreamID,
                       local_settings: %{initial_window_size: remoteWindow},
                       remote_settings: %{initial_window_size:
                                          localWindow})) do
    stream = r_stream(id: localStreamID, method: method,
                 local_window: localWindow, remote_window: remoteWindow)
    {:ok, localStreamID,
       stream_store(stream,
                      r_http2_machine(state, local_streamid: localStreamID + 2))}
  end

  def init_upgrade_stream(method,
           state = r_http2_machine(mode: :server, remote_streamid: 0,
                       local_settings: %{initial_window_size: remoteWindow},
                       remote_settings: %{initial_window_size:
                                          localWindow})) do
    stream = r_stream(id: 1, method: method, remote: :fin,
                 remote_expected_size: 0, local_window: localWindow,
                 remote_window: remoteWindow, te: :undefined)
    {:ok, 1,
       stream_store(stream, r_http2_machine(state, remote_streamid: 1))}
  end

  def frame(frame,
           state = r_http2_machine(state: :settings, preface_timer: tRef)) do
    :ok = (case (tRef) do
             :undefined ->
               :ok
             _ ->
               :erlang.cancel_timer(tRef,
                                      [{:async, true}, {:info, false}])
           end)
    settings_frame(frame,
                     r_http2_machine(state, state: :normal,  preface_timer: :undefined))
  end

  def frame(frame,
           state = r_http2_machine(state: {:continuation, _, _})) do
    continuation_frame(frame, state)
  end

  def frame(:settings_ack, state = r_http2_machine(state: :normal)) do
    settings_ack_frame(state)
  end

  def frame(frame, state = r_http2_machine(state: :normal)) do
    case (:erlang.element(1, frame)) do
      :data ->
        data_frame(frame, state)
      :headers ->
        headers_frame(frame, state)
      :priority ->
        priority_frame(frame, state)
      :rst_stream ->
        rst_stream_frame(frame, state)
      :settings ->
        settings_frame(frame, state)
      :push_promise ->
        push_promise_frame(frame, state)
      :ping ->
        ping_frame(frame, state)
      :ping_ack ->
        ping_ack_frame(frame, state)
      :goaway ->
        goaway_frame(frame, state)
      :window_update ->
        window_update_frame(frame, state)
      :continuation ->
        unexpected_continuation_frame(frame, state)
      _ ->
        ignored_frame(state)
    end
  end

  defp data_frame({:data, streamID, _, _},
            state = r_http2_machine(mode: mode, local_streamid: localStreamID,
                        remote_streamid: remoteStreamID))
      when (mode === :server and rem(streamID, 2) === 0 or mode === :client and rem(streamID, 2) === 1) and streamID >= localStreamID or not
                                                                                                                                         (mode === :server and rem(streamID, 2) === 0 or mode === :client and rem(streamID, 2) === 1) and streamID > remoteStreamID do
    {:error, {:connection_error, :protocol_error, :"DATA frame received on a stream in idle state. (RFC7540 5.1)"},
       state}
  end

  defp data_frame({:data, _, _, data},
            state = r_http2_machine(remote_window: connWindow))
      when byte_size(data) > connWindow do
    {:error, {:connection_error, :flow_control_error, :"DATA frame overflowed the connection flow control window. (RFC7540 6.9, RFC7540 6.9.1)"},
       state}
  end

  defp data_frame(frame = {:data, streamID, _, data},
            state0 = r_http2_machine(remote_window: connWindow,
                         local_lingering_streams: lingering)) do
    dataLen = byte_size(data)
    state = r_http2_machine(state0, remote_window: connWindow - dataLen)
    case (stream_get(streamID, state)) do
      r_stream(remote_window: streamWindow)
          when streamWindow < dataLen ->
        stream_reset(streamID, state, :flow_control_error, :"DATA frame overflowed the stream flow control window. (RFC7540 6.9, RFC7540 6.9.1)")
      stream = r_stream(remote: :nofin) ->
        data_frame(frame, state, stream, dataLen)
      r_stream(remote: :idle) ->
        stream_reset(streamID, state, :protocol_error, :"DATA frame received before a HEADERS frame. (RFC7540 8.1, RFC7540 8.1.2.6)")
      r_stream(remote: :fin) ->
        stream_reset(streamID, state, :stream_closed, :"DATA frame received for a half-closed (remote) stream. (RFC7540 5.1)")
      :undefined ->
        case (:lists.member(streamID, lingering)) do
          true ->
            {:ok, state}
          false ->
            {:error, {:connection_error, :stream_closed, :"DATA frame received for a closed stream. (RFC7540 5.1)"}, state}
        end
    end
  end

  defp data_frame(frame = {:data, _, isFin, _}, state0,
            stream0 = r_stream(id: streamID, remote_window: streamWindow,
                          remote_read_size: streamRead),
            dataLen) do
    stream = r_stream(stream0, remote: isFin, 
                          remote_window: streamWindow - dataLen, 
                          remote_read_size: streamRead + dataLen)
    state = stream_store(stream, state0)
    case (is_body_size_valid(stream)) do
      true ->
        {:ok, frame, state}
      false ->
        stream_reset(streamID, state, :protocol_error, :"The total size of DATA frames is different than the content-length. (RFC7540 8.1.2.6)")
    end
  end

  defp is_body_size_valid(r_stream(remote_expected_size: :undefined)) do
    true
  end

  defp is_body_size_valid(r_stream(remote: :nofin,
              remote_expected_size: expected, remote_read_size: read))
      when read > expected do
    false
  end

  defp is_body_size_valid(r_stream(remote: :nofin)) do
    true
  end

  defp is_body_size_valid(r_stream(remote: :fin, remote_expected_size: expected,
              remote_read_size: expected)) do
    true
  end

  defp is_body_size_valid(_) do
    false
  end

  Record.defrecord(:r_headers, :headers, id: :undefined,
                                   fin: :undefined, head: :undefined,
                                   data: :undefined)
  defp headers_frame(frame = r_headers(), state = r_http2_machine(mode: mode)) do
    case (mode) do
      :server ->
        server_headers_frame(frame, state)
      :client ->
        client_headers_frame(frame, state)
    end
  end

  defp headers_frame({:headers, streamID, isFin, isHeadFin,
             _IsExclusive, _DepStreamID, _Weight, headerData},
            state = r_http2_machine(mode: mode)) do
    headersFrame = r_headers(id: streamID, fin: isFin,
                       head: isHeadFin, data: headerData)
    case (mode) do
      :server ->
        server_headers_frame(headersFrame, state)
      :client ->
        client_headers_frame(headersFrame, state)
    end
  end

  defp server_headers_frame(r_headers(id: streamID), state)
      when rem(streamID, 2) === 0 do
    {:error, {:connection_error, :protocol_error, :"HEADERS frame received with even-numbered streamid. (RFC7540 5.1.1)"},
       state}
  end

  defp server_headers_frame(frame = r_headers(id: streamID, head: isHeadFin),
            state = r_http2_machine(mode: :server,
                        remote_streamid: remoteStreamID))
      when streamID > remoteStreamID do
    case (isHeadFin) do
      :head_fin ->
        headers_decode(frame, state, :request, :undefined)
      :head_nofin ->
        {:ok, r_http2_machine(state, state: {:continuation, :request, frame})}
    end
  end

  defp server_headers_frame(frame = r_headers(id: streamID, fin: isFin,
                      head: isHeadFin),
            state) do
    case (stream_get(streamID, state)) do
      stream = r_stream(remote: :nofin) when isFin === :fin ->
        case (isHeadFin) do
          :head_fin ->
            headers_decode(frame, state, :trailers, stream)
          :head_nofin ->
            {:ok,
               r_http2_machine(state, state: {:continuation, :trailers, frame})}
        end
      r_stream(remote: :nofin) ->
        {:error, {:connection_error, :protocol_error, :"Trailing HEADERS frame received without the END_STREAM flag set. (RFC7540 8.1, RFC7540 8.1.2.6)"},
           state}
      _ ->
        {:error, {:connection_error, :stream_closed, :"HEADERS frame received on a stream in closed or half-closed state. (RFC7540 5.1)"}, state}
    end
  end

  defp client_headers_frame(frame = r_headers(id: streamID, fin: isFin,
                      head: isHeadFin),
            state = r_http2_machine(local_streamid: localStreamID,
                        remote_streamid: remoteStreamID))
      when rem(streamID, 2) === 1 and streamID < localStreamID or not
                                                                  (rem(streamID, 2) === 1) and streamID <= remoteStreamID do
    case (stream_get(streamID, state)) do
      stream = r_stream(remote: :idle) ->
        case (isHeadFin) do
          :head_fin ->
            headers_decode(frame, state, :response, stream)
          :head_nofin ->
            {:ok,
               r_http2_machine(state, state: {:continuation, :response, frame})}
        end
      stream = r_stream(remote: :nofin) when isFin === :fin ->
        case (isHeadFin) do
          :head_fin ->
            headers_decode(frame, state, :trailers, stream)
          :head_nofin ->
            {:ok,
               r_http2_machine(state, state: {:continuation, :trailers, frame})}
        end
      r_stream(remote: :nofin) ->
        {:error, {:connection_error, :protocol_error, :"Trailing HEADERS frame received without the END_STREAM flag set. (RFC7540 8.1, RFC7540 8.1.2.6)"},
           state}
      _ ->
        {:error, {:connection_error, :stream_closed, :"HEADERS frame received on a stream in closed or half-closed state. (RFC7540 5.1)"}, state}
    end
  end

  defp client_headers_frame(_, state) do
    {:error, {:connection_error, :protocol_error, :"HEADERS frame received on an idle stream. (RFC7540 5.1.1)"},
       state}
  end

  defp headers_decode(frame = r_headers(head: :head_fin, data: headerData),
            state = r_http2_machine(decode_state: decodeState0), type, stream) do
    try do
      :cow_hpack.decode(headerData, decodeState0)
    catch
      _, _ ->
        {:error, {:connection_error, :compression_error, :"Error while trying to decode HPACK-encoded header block. (RFC7540 4.3)"},
           state}
    else
      {headers, decodeState} when type === :request ->
        headers_enforce_concurrency_limit(frame,
                                            r_http2_machine(state, decode_state: decodeState),
                                            type, stream, headers)
      {headers, decodeState} ->
        headers_pseudo_headers(frame,
                                 r_http2_machine(state, decode_state: decodeState), type,
                                 stream, headers)
    end
  end

  defp headers_enforce_concurrency_limit(frame = r_headers(id: streamID),
            state = r_http2_machine(local_settings: localSettings,
                        streams: streams),
            type, stream, headers) do
    maxConcurrentStreams = :maps.get(:max_concurrent_streams,
                                       localSettings, :infinity)
    case (map_size(streams) < maxConcurrentStreams) do
      true ->
        headers_pseudo_headers(frame, state, type, stream,
                                 headers)
      false ->
        {:error, {:stream_error, streamID, :refused_stream, :"Maximum number of concurrent streams has been reached. (RFC7540 5.1.2)"},
           state}
    end
  end

  defp headers_pseudo_headers(frame, state = r_http2_machine(local_settings: localSettings),
            type, stream, headers0)
      when type === :request or type === :push_promise do
    isExtendedConnectEnabled = :maps.get(:enable_connect_protocol,
                                           localSettings, false)
    case (request_pseudo_headers(headers0, %{})) do
      {:ok,
         pseudoHeaders = %{method: "CONNECT", scheme: _, authority: _,
                             path: _, protocol: _},
         headers}
          when isExtendedConnectEnabled ->
        headers_regular_headers(frame, state, type, stream,
                                  pseudoHeaders, headers)
      {:ok, %{method: "CONNECT", scheme: _, authority: _, path: _}, _}
          when isExtendedConnectEnabled ->
        headers_malformed(frame, state, :"The :protocol pseudo-header MUST be sent with an extended CONNECT. (RFC8441 4)")
      {:ok, %{protocol: _}, _} ->
        headers_malformed(frame, state, :"The :protocol pseudo-header is only defined for the extended CONNECT. (RFC8441 4)")
      {:ok, pseudoHeaders = %{method: "CONNECT", authority: _},
         headers}
          when map_size(pseudoHeaders) === 2 ->
        headers_regular_headers(frame, state, type, stream,
                                  pseudoHeaders, headers)
      {:ok, %{method: "CONNECT"}, _} ->
        headers_malformed(frame, state, :"CONNECT requests only use the :method and :authority pseudo-headers. (RFC7540 8.3)")
      {:ok, pseudoHeaders = %{method: _, scheme: _, path: _},
         headers} ->
        headers_regular_headers(frame, state, type, stream,
                                  pseudoHeaders, headers)
      {:ok, _, _} ->
        headers_malformed(frame, state, :"A required pseudo-header was not found. (RFC7540 8.1.2.3)")
      {:error, humanReadable} ->
        headers_malformed(frame, state, humanReadable)
    end
  end

  defp headers_pseudo_headers(frame = r_headers(id: streamID), state,
            type = :response, stream, headers0) do
    case (response_pseudo_headers(headers0, %{})) do
      {:ok, pseudoHeaders = %{status: _}, headers} ->
        headers_regular_headers(frame, state, type, stream,
                                  pseudoHeaders, headers)
      {:ok, _, _} ->
        stream_reset(streamID, state, :protocol_error, :"A required pseudo-header was not found. (RFC7540 8.1.2.4)")
      {:error, humanReadable} ->
        stream_reset(streamID, state, :protocol_error,
                       humanReadable)
    end
  end

  defp headers_pseudo_headers(frame = r_headers(id: streamID), state,
            type = :trailers, stream, headers) do
    case (trailers_contain_pseudo_headers(headers)) do
      false ->
        headers_regular_headers(frame, state, type, stream, %{},
                                  headers)
      true ->
        stream_reset(streamID, state, :protocol_error, :"Trailer header blocks must not contain pseudo-headers. (RFC7540 8.1.2.1)")
    end
  end

  defp headers_malformed(r_headers(id: streamID), state, humanReadable) do
    {:error,
       {:stream_error, streamID, :protocol_error,
          humanReadable},
       state}
  end

  defp request_pseudo_headers([{":method", _} | _], %{method: _}) do
    {:error, :"Multiple :method pseudo-headers were found. (RFC7540 8.1.2.3)"}
  end

  defp request_pseudo_headers([{":method", method} | tail], pseudoHeaders) do
    request_pseudo_headers(tail,
                             Map.put(pseudoHeaders, :method, method))
  end

  defp request_pseudo_headers([{":scheme", _} | _], %{scheme: _}) do
    {:error, :"Multiple :scheme pseudo-headers were found. (RFC7540 8.1.2.3)"}
  end

  defp request_pseudo_headers([{":scheme", scheme} | tail], pseudoHeaders) do
    request_pseudo_headers(tail,
                             Map.put(pseudoHeaders, :scheme, scheme))
  end

  defp request_pseudo_headers([{":authority", _} | _], %{authority: _}) do
    {:error, :"Multiple :authority pseudo-headers were found. (RFC7540 8.1.2.3)"}
  end

  defp request_pseudo_headers([{":authority", authority} | tail], pseudoHeaders) do
    request_pseudo_headers(tail,
                             Map.put(pseudoHeaders, :authority, authority))
  end

  defp request_pseudo_headers([{":path", _} | _], %{path: _}) do
    {:error, :"Multiple :path pseudo-headers were found. (RFC7540 8.1.2.3)"}
  end

  defp request_pseudo_headers([{":path", path} | tail], pseudoHeaders) do
    request_pseudo_headers(tail,
                             Map.put(pseudoHeaders, :path, path))
  end

  defp request_pseudo_headers([{":protocol", _} | _], %{protocol: _}) do
    {:error, :"Multiple :protocol pseudo-headers were found. (RFC7540 8.1.2.3)"}
  end

  defp request_pseudo_headers([{":protocol", protocol} | tail], pseudoHeaders) do
    request_pseudo_headers(tail,
                             Map.put(pseudoHeaders, :protocol, protocol))
  end

  defp request_pseudo_headers([{<<":", _ :: bits>>, _} | _], _) do
    {:error, :"An unknown or invalid pseudo-header was found. (RFC7540 8.1.2.1)"}
  end

  defp request_pseudo_headers(headers, pseudoHeaders) do
    {:ok, pseudoHeaders, headers}
  end

  defp response_pseudo_headers([{":status", _} | _], %{status: _}) do
    {:error, :"Multiple :status pseudo-headers were found. (RFC7540 8.1.2.3)"}
  end

  defp response_pseudo_headers([{":status", status} | tail], pseudoHeaders) do
    try do
      :cow_http.status_to_integer(status)
    catch
      _, _ ->
        {:error, :"The :status pseudo-header value is invalid. (RFC7540 8.1.2.4)"}
    else
      intStatus ->
        response_pseudo_headers(tail,
                                  Map.put(pseudoHeaders, :status, intStatus))
    end
  end

  defp response_pseudo_headers([{<<":", _ :: bits>>, _} | _], _) do
    {:error, :"An unknown or invalid pseudo-header was found. (RFC7540 8.1.2.1)"}
  end

  defp response_pseudo_headers(headers, pseudoHeaders) do
    {:ok, pseudoHeaders, headers}
  end

  defp trailers_contain_pseudo_headers([]) do
    false
  end

  defp trailers_contain_pseudo_headers([{<<":", _ :: bits>>, _} | _]) do
    true
  end

  defp trailers_contain_pseudo_headers([_ | tail]) do
    trailers_contain_pseudo_headers(tail)
  end

  defp headers_regular_headers(frame = r_headers(id: streamID), state, type, stream,
            pseudoHeaders, headers) do
    case (regular_headers(headers, type)) do
      :ok when type === :request ->
        request_expected_size(frame, state, type, stream,
                                pseudoHeaders, headers)
      :ok when type === :push_promise ->
        push_promise_frame(frame, state, stream, pseudoHeaders,
                             headers)
      :ok when type === :response ->
        response_expected_size(frame, state, type, stream,
                                 pseudoHeaders, headers)
      :ok when type === :trailers ->
        trailers_frame(frame, state, stream, headers)
      {:error, humanReadable} when type === :request ->
        headers_malformed(frame, state, humanReadable)
      {:error, humanReadable} ->
        stream_reset(streamID, state, :protocol_error,
                       humanReadable)
    end
  end

  defp regular_headers([{<<>>, _} | _], _) do
    {:error, :"Empty header names are not valid regular headers. (CVE-2019-9516)"}
  end

  defp regular_headers([{<<":", _ :: bits>>, _} | _], _) do
    {:error, :"Pseudo-headers were found after regular headers. (RFC7540 8.1.2.1)"}
  end

  defp regular_headers([{"connection", _} | _], _) do
    {:error, :"The connection header is not allowed. (RFC7540 8.1.2.2)"}
  end

  defp regular_headers([{"keep-alive", _} | _], _) do
    {:error, :"The keep-alive header is not allowed. (RFC7540 8.1.2.2)"}
  end

  defp regular_headers([{"proxy-authenticate", _} | _], _) do
    {:error, :"The proxy-authenticate header is not allowed. (RFC7540 8.1.2.2)"}
  end

  defp regular_headers([{"proxy-authorization", _} | _], _) do
    {:error, :"The proxy-authorization header is not allowed. (RFC7540 8.1.2.2)"}
  end

  defp regular_headers([{"transfer-encoding", _} | _], _) do
    {:error, :"The transfer-encoding header is not allowed. (RFC7540 8.1.2.2)"}
  end

  defp regular_headers([{"upgrade", _} | _], _) do
    {:error, :"The upgrade header is not allowed. (RFC7540 8.1.2.2)"}
  end

  defp regular_headers([{"te", value} | _], :request) when value !== "trailers" do
    {:error, :"The te header with a value other than \"trailers\" is not allowed. (RFC7540 8.1.2.2)"}
  end

  defp regular_headers([{"te", _} | _], type) when type !== :request do
    {:error, :"The te header is only allowed in request headers. (RFC7540 8.1.2.2)"}
  end

  defp regular_headers([{name, _} | tail], type) do
    pattern = [<<?A>>, <<?B>>, <<?C>>, <<?D>>, <<?E>>,
                                                   <<?F>>, <<?G>>, <<?H>>,
                                                                       <<?I>>,
                                                                           <<?J>>,
                                                                               <<?K>>,
                                                                                   <<?L>>,
                                                                                       <<?M>>,
                                                                                           <<?N>>,
                                                                                               <<?O>>,
                                                                                                   <<?P>>,
                                                                                                       <<?Q>>,
                                                                                                           <<?R>>,
                                                                                                               <<?S>>,
                                                                                                                   <<?T>>,
                                                                                                                       <<?U>>,
                                                                                                                           <<?V>>,
                                                                                                                               <<?W>>,
                                                                                                                                   <<?X>>,
                                                                                                                                       <<?Y>>,
                                                                                                                                           <<?Z>>]
    case (:binary.match(name, pattern)) do
      :nomatch ->
        regular_headers(tail, type)
      _ ->
        {:error, :"Header names must be lowercase. (RFC7540 8.1.2)"}
    end
  end

  defp regular_headers([], _) do
    :ok
  end

  defp request_expected_size(frame = r_headers(fin: isFin), state, type, stream,
            pseudoHeaders, headers) do
    case (for {"content-length", cL} <- headers do
            cL
          end) do
      [] when isFin === :fin ->
        headers_frame(frame, state, type, stream, pseudoHeaders,
                        headers, 0)
      [] ->
        headers_frame(frame, state, type, stream, pseudoHeaders,
                        headers, :undefined)
      ["0"] when isFin === :fin ->
        headers_frame(frame, state, type, stream, pseudoHeaders,
                        headers, 0)
      [_] when isFin === :fin ->
        headers_malformed(frame, state, :"HEADERS frame with the END_STREAM flag contains a non-zero content-length. (RFC7540 8.1.2.6)")
      [binLen] ->
        headers_parse_expected_size(frame, state, type, stream,
                                      pseudoHeaders, headers, binLen)
      _ ->
        headers_malformed(frame, state, :"Multiple content-length headers were received. (RFC7230 3.3.2)")
    end
  end

  defp response_expected_size(frame = r_headers(id: streamID, fin: isFin), state,
            type, stream = r_stream(method: method),
            pseudoHeaders = %{status: status}, headers) do
    case (for {"content-length", cL} <- headers do
            cL
          end) do
      [] when isFin === :fin ->
        headers_frame(frame, state, type, stream, pseudoHeaders,
                        headers, 0)
      [] ->
        headers_frame(frame, state, type, stream, pseudoHeaders,
                        headers, :undefined)
      [_] when (status >= 100 and status <= 199) ->
        stream_reset(streamID, state, :protocol_error, :"Content-length header received in a 1xx response. (RFC7230 3.3.2)")
      [_] when status === 204 ->
        stream_reset(streamID, state, :protocol_error, :"Content-length header received in a 204 response. (RFC7230 3.3.2)")
      [_] when (status >= 200 and status <= 299 and
                  method === "CONNECT")
               ->
        stream_reset(streamID, state, :protocol_error, :"Content-length header received in a 2xx response to a CONNECT request. (RFC7230 3.3.2).")
      [_] when method === "HEAD" ->
        headers_frame(frame, state, type, stream, pseudoHeaders,
                        headers, 0)
      [_] when status === 304 ->
        headers_frame(frame, state, type, stream, pseudoHeaders,
                        headers, 0)
      ["0"] when isFin === :fin ->
        headers_frame(frame, state, type, stream, pseudoHeaders,
                        headers, 0)
      [_] when isFin === :fin ->
        stream_reset(streamID, state, :protocol_error, :"HEADERS frame with the END_STREAM flag contains a non-zero content-length. (RFC7540 8.1.2.6)")
      [binLen] ->
        headers_parse_expected_size(frame, state, type, stream,
                                      pseudoHeaders, headers, binLen)
      _ ->
        stream_reset(streamID, state, :protocol_error, :"Multiple content-length headers were received. (RFC7230 3.3.2)")
    end
  end

  defp headers_parse_expected_size(frame = r_headers(id: streamID), state, type, stream,
            pseudoHeaders, headers, binLen) do
    try do
      :cow_http_hd.parse_content_length(binLen)
    catch
      _, _ ->
        humanReadable = :"The content-length header is invalid. (RFC7230 3.3.2)"
        case (type) do
          :request ->
            headers_malformed(frame, state, humanReadable)
          :response ->
            stream_reset(streamID, state, :protocol_error,
                           humanReadable)
        end
    else
      len ->
        headers_frame(frame, state, type, stream, pseudoHeaders,
                        headers, len)
    end
  end

  defp headers_frame(r_headers(id: streamID, fin: isFin),
            state0 = r_http2_machine(local_settings: %{initial_window_size:
                                         remoteWindow},
                         remote_settings: %{initial_window_size: localWindow}),
            type, stream0, pseudoHeaders, headers, len) do
    {stream, state1} = (case (type) do
                          :request ->
                            tE = (case (:lists.keyfind("te", 1, headers)) do
                                    {_, tE0} ->
                                      tE0
                                    false ->
                                      :undefined
                                  end)
                            {r_stream(id: streamID,
                                 method: :maps.get(:method, pseudoHeaders),
                                 remote: isFin, remote_expected_size: len,
                                 local_window: localWindow,
                                 remote_window: remoteWindow, te: tE),
                               r_http2_machine(state0, remote_streamid: streamID)}
                          :response ->
                            stream1 = (case (pseudoHeaders) do
                                         %{status: status}
                                             when (status >= 100 and
                                                     status <= 199)
                                                  ->
                                           stream0
                                         _ ->
                                           r_stream(stream0, remote: isFin, 
                                                        remote_expected_size: len)
                                       end)
                            {stream1, state0}
                        end)
    state = stream_store(stream, state1)
    {:ok,
       {:headers, streamID, isFin, headers, pseudoHeaders,
          len},
       state}
  end

  defp trailers_frame(r_headers(id: streamID), state0, stream0, headers) do
    stream = r_stream(stream0, remote: :fin)
    state = stream_store(stream, state0)
    case (is_body_size_valid(stream)) do
      true ->
        {:ok, {:trailers, streamID, headers}, state}
      false ->
        stream_reset(streamID, state, :protocol_error, :"The total size of DATA frames is different than the content-length. (RFC7540 8.1.2.6)")
    end
  end

  defp priority_frame(_Frame, state) do
    {:ok, state}
  end

  defp rst_stream_frame({:rst_stream, streamID, _},
            state = r_http2_machine(mode: mode, local_streamid: localStreamID,
                        remote_streamid: remoteStreamID))
      when (mode === :server and rem(streamID, 2) === 0 or mode === :client and rem(streamID, 2) === 1) and streamID >= localStreamID or not
                                                                                                                                         (mode === :server and rem(streamID, 2) === 0 or mode === :client and rem(streamID, 2) === 1) and streamID > remoteStreamID do
    {:error, {:connection_error, :protocol_error, :"RST_STREAM frame received on a stream in idle state. (RFC7540 5.1)"},
       state}
  end

  defp rst_stream_frame({:rst_stream, streamID, reason},
            state = r_http2_machine(streams: streams0,
                        remote_lingering_streams: lingering0)) do
    streams = :maps.remove(streamID, streams0)
    lingering = [streamID | :lists.sublist(lingering0,
                                             10 - 1)]
    {:ok, {:rst_stream, streamID, reason},
       r_http2_machine(state, streams: streams, 
                  remote_lingering_streams: lingering)}
  end

  defp settings_frame({:settings, settings},
            state0 = r_http2_machine(opts: opts, remote_settings: settings0)) do
    state1 = r_http2_machine(state0, remote_settings: :maps.merge(settings0,
                                                      settings))
    state2 = :maps.fold(fn :header_table_size, newSize,
                             state = r_http2_machine(encode_state: encodeState0) ->
                             maxSize = :maps.get(:max_encode_table_size, opts,
                                                   4096)
                             encodeState = :cow_hpack.set_max_size(min(newSize,
                                                                         maxSize),
                                                                     encodeState0)
                             r_http2_machine(state, encode_state: encodeState)
                           :initial_window_size, newWindowSize, state ->
                             oldWindowSize = :maps.get(:initial_window_size,
                                                         settings0, 65535)
                             streams_update_local_window(state,
                                                           newWindowSize - oldWindowSize)
                           _, _, state ->
                             state
                        end,
                          state1, settings)
    case (settings) do
      %{initial_window_size: _} ->
        send_data(state2)
      _ ->
        {:ok, state2}
    end
  end

  defp settings_frame(_F, state = r_http2_machine(mode: :server)) do
    {:error, {:connection_error, :protocol_error, :"The preface sequence must be followed by a SETTINGS frame. (RFC7540 3.5)"},
       state}
  end

  defp settings_frame(_F, state) do
    {:error, {:connection_error, :protocol_error, :"The preface must begin with a SETTINGS frame. (RFC7540 3.5)"},
       state}
  end

  defp streams_update_local_window(state = r_http2_machine(streams: streams0), increment) do
    streams = :maps.map(fn _,
                             s = r_stream(local_window: streamWindow) ->
                             r_stream(s, local_window: streamWindow + increment)
                        end,
                          streams0)
    r_http2_machine(state, streams: streams)
  end

  defp settings_ack_frame(state0 = r_http2_machine(settings_timer: tRef,
                       local_settings: local0, next_settings: nextSettings)) do
    :ok = (case (tRef) do
             :undefined ->
               :ok
             _ ->
               :erlang.cancel_timer(tRef,
                                      [{:async, true}, {:info, false}])
           end)
    local = :maps.merge(local0, nextSettings)
    state1 = r_http2_machine(state0, settings_timer: :undefined, 
                         local_settings: local,  next_settings: %{})
    {:ok,
       :maps.fold(fn :header_table_size, maxSize,
                       state = r_http2_machine(decode_state: decodeState0) ->
                       decodeState = :cow_hpack.set_max_size(maxSize,
                                                               decodeState0)
                       r_http2_machine(state, decode_state: decodeState)
                     :initial_window_size, newWindowSize, state ->
                       oldWindowSize = :maps.get(:initial_window_size, local0,
                                                   65535)
                       streams_update_remote_window(state,
                                                      newWindowSize - oldWindowSize)
                     _, _, state ->
                       state
                  end,
                    state1, nextSettings)}
  end

  defp streams_update_remote_window(state = r_http2_machine(streams: streams0), increment) do
    streams = :maps.map(fn _,
                             s = r_stream(remote_window: streamWindow) ->
                             r_stream(s, remote_window: streamWindow + increment)
                        end,
                          streams0)
    r_http2_machine(state, streams: streams)
  end

  Record.defrecord(:r_push_promise, :push_promise, id: :undefined,
                                        head: :undefined,
                                        promised_id: :undefined,
                                        data: :undefined)
  defp push_promise_frame(_, state = r_http2_machine(mode: :server)) do
    {:error, {:connection_error, :protocol_error, :"PUSH_PROMISE frames MUST NOT be sent by the client. (RFC7540 6.6)"},
       state}
  end

  defp push_promise_frame(_,
            state = r_http2_machine(local_settings: %{enable_push: false})) do
    {:error, {:connection_error, :protocol_error, :"PUSH_PROMISE frame received despite SETTINGS_ENABLE_PUSH set to 0. (RFC7540 6.6)"},
       state}
  end

  defp push_promise_frame(r_push_promise(promised_id: promisedStreamID),
            state = r_http2_machine(remote_streamid: remoteStreamID))
      when promisedStreamID <= remoteStreamID do
    {:error, {:connection_error, :protocol_error, :"PUSH_PROMISE frame received for a promised stream in closed or half-closed state. (RFC7540 5.1, RFC7540 6.6)"},
       state}
  end

  defp push_promise_frame(r_push_promise(id: streamID), state) when not
                                       (rem(streamID, 2) === 1) do
    {:error, {:connection_error, :protocol_error, :"PUSH_PROMISE frame received on a server-initiated stream. (RFC7540 6.6)"},
       state}
  end

  defp push_promise_frame(frame = r_push_promise(id: streamID, head: isHeadFin,
                      promised_id: promisedStreamID, data: headerData),
            state) do
    case (stream_get(streamID, state)) do
      stream = r_stream(remote: :idle) ->
        case (isHeadFin) do
          :head_fin ->
            headers_decode(r_headers(id: promisedStreamID, fin: :fin,
                               head: isHeadFin, data: headerData),
                             state, :push_promise, stream)
          :head_nofin ->
            {:ok,
               r_http2_machine(state, state: {:continuation, :push_promise, frame})}
        end
      _ ->
        {:error, {:connection_error, :stream_closed, :"PUSH_PROMISE frame received on a stream in closed or half-closed state. (RFC7540 5.1, RFC7540 6.6)"}, state}
    end
  end

  defp push_promise_frame(r_headers(id: promisedStreamID),
            state0 = r_http2_machine(local_settings: %{initial_window_size:
                                         remoteWindow},
                         remote_settings: %{initial_window_size: localWindow}),
            r_stream(id: streamID), pseudoHeaders = %{method: method},
            headers) do
    tE = (case (:lists.keyfind("te", 1, headers)) do
            {_, tE0} ->
              tE0
            false ->
              :undefined
          end)
    promisedStream = r_stream(id: promisedStreamID, method: method,
                         local: :fin, local_window: localWindow,
                         remote_window: remoteWindow, te: tE)
    state = stream_store(promisedStream,
                           r_http2_machine(state0, remote_streamid: promisedStreamID))
    {:ok,
       {:push_promise, streamID, promisedStreamID, headers,
          pseudoHeaders},
       state}
  end

  defp ping_frame({:ping, _}, state) do
    {:ok, state}
  end

  defp ping_ack_frame({:ping_ack, _}, state) do
    {:ok, state}
  end

  defp goaway_frame(frame = {:goaway, _, _, _}, state) do
    {:ok, frame, state}
  end

  defp window_update_frame({:window_update, increment},
            state = r_http2_machine(local_window: connWindow))
      when connWindow + increment > 2147483647 do
    {:error, {:connection_error, :flow_control_error, :"The flow control window must not be greater than 2^31-1. (RFC7540 6.9.1)"},
       state}
  end

  defp window_update_frame({:window_update, increment},
            state = r_http2_machine(local_window: connWindow)) do
    send_data(r_http2_machine(state, local_window: connWindow + increment))
  end

  defp window_update_frame({:window_update, streamID, _},
            state = r_http2_machine(mode: mode, local_streamid: localStreamID,
                        remote_streamid: remoteStreamID))
      when (mode === :server and rem(streamID, 2) === 0 or mode === :client and rem(streamID, 2) === 1) and streamID >= localStreamID or not
                                                                                                                                         (mode === :server and rem(streamID, 2) === 0 or mode === :client and rem(streamID, 2) === 1) and streamID > remoteStreamID do
    {:error, {:connection_error, :protocol_error, :"WINDOW_UPDATE frame received on a stream in idle state. (RFC7540 5.1)"},
       state}
  end

  defp window_update_frame({:window_update, streamID, increment},
            state0 = r_http2_machine(remote_lingering_streams: lingering)) do
    case (stream_get(streamID, state0)) do
      r_stream(local_window: streamWindow)
          when streamWindow + increment > 2147483647 ->
        stream_reset(streamID, state0, :flow_control_error, :"The flow control window must not be greater than 2^31-1. (RFC7540 6.9.1)")
      stream0 = r_stream(local_window: streamWindow) ->
        send_data(r_stream(stream0, local_window: streamWindow + increment),
                    state0)
      :undefined ->
        case (:lists.member(streamID, lingering)) do
          false ->
            {:ok, state0}
          true ->
            stream_reset(streamID, state0, :stream_closed, :"WINDOW_UPDATE frame received after the stream was reset. (RFC7540 5.1)")
        end
    end
  end

  Record.defrecord(:r_continuation, :continuation, id: :undefined,
                                        head: :undefined, data: :undefined)
  defp unexpected_continuation_frame(r_continuation(), state) do
    {:error, {:connection_error, :protocol_error, :"CONTINUATION frames MUST be preceded by a HEADERS or PUSH_PROMISE frame. (RFC7540 6.10)"},
       state}
  end

  defp continuation_frame(r_continuation(id: streamID, head: :head_fin,
              data: headerFragment1),
            state = r_http2_machine(state: {:continuation, type,
                                frame = r_headers(id: streamID,
                                            data: headerFragment0)})) do
    headerData = <<headerFragment0 :: binary,
                     headerFragment1 :: binary>>
    headers_decode(r_headers(frame, head: :head_fin, 
                              data: headerData),
                     r_http2_machine(state, state: :normal), type,
                     stream_get(streamID, state))
  end

  defp continuation_frame(r_continuation(id: streamID, head: :head_fin,
              data: headerFragment1),
            state = r_http2_machine(state: {:continuation, type,
                                r_push_promise(id: streamID, promised_id: promisedStreamID,
                                    data: headerFragment0)})) do
    headerData = <<headerFragment0 :: binary,
                     headerFragment1 :: binary>>
    headers_decode(r_headers(id: promisedStreamID, fin: :fin,
                       head: :head_fin, data: headerData),
                     r_http2_machine(state, state: :normal), type, :undefined)
  end

  defp continuation_frame(r_continuation(id: streamID, data: headerFragment1),
            state = r_http2_machine(state: {:continuation, type,
                                continuedFrame0}))
      when :erlang.element(2, continuedFrame0) === streamID do
    continuedFrame = (case (continuedFrame0) do
                        r_headers(data: headerFragment0) ->
                          headerData = <<headerFragment0 :: binary,
                                           headerFragment1 :: binary>>
                          r_headers(continuedFrame0, data: headerData)
                        r_push_promise(data: headerFragment0) ->
                          headerData = <<headerFragment0 :: binary,
                                           headerFragment1 :: binary>>
                          r_push_promise(continuedFrame0, data: headerData)
                      end)
    {:ok,
       r_http2_machine(state, state: {:continuation, type, continuedFrame})}
  end

  defp continuation_frame(_F, state) do
    {:error, {:connection_error, :protocol_error, :"An invalid frame was received in the middle of a header block. (RFC7540 6.2)"},
       state}
  end

  def ignored_frame(state = r_http2_machine(state: {:continuation, _, _})) do
    {:error, {:connection_error, :protocol_error, :"An invalid frame was received in the middle of a header block. (RFC7540 6.2)"},
       state}
  end

  def ignored_frame(state) do
    {:ok, state}
  end

  def timeout(:preface_timeout, tRef,
           state = r_http2_machine(preface_timer: tRef)) do
    {:error, {:connection_error, :protocol_error, :"The preface was not received in a reasonable amount of time."},
       state}
  end

  def timeout(:settings_timeout, tRef,
           state = r_http2_machine(settings_timer: tRef)) do
    {:error, {:connection_error, :settings_timeout, :"The SETTINGS ack was not received within the configured time. (RFC7540 6.5.3)"},
       state}
  end

  def timeout(_, _, state) do
    {:ok, state}
  end

  def prepare_headers(streamID, state = r_http2_machine(encode_state: encodeState0),
           isFin0, pseudoHeaders, headers0) do
    stream = (r_stream(method: method,
                  local: :idle) = stream_get(streamID, state))
    isFin = (case ({isFin0, method}) do
               {:idle, _} ->
                 :nofin
               {_, "HEAD"} ->
                 :fin
               _ ->
                 isFin0
             end)
    headers = merge_pseudo_headers(pseudoHeaders,
                                     remove_http11_headers(headers0))
    {headerBlock, encodeState} = :cow_hpack.encode(headers,
                                                     encodeState0)
    {:ok, isFin, headerBlock,
       stream_store(r_stream(stream, local: isFin0),
                      r_http2_machine(state, encode_state: encodeState))}
  end

  def prepare_push_promise(_, r_http2_machine(remote_settings: %{enable_push: false}), _,
           _) do
    {:error, :no_push}
  end

  def prepare_push_promise(streamID,
           state = r_http2_machine(encode_state: encodeState0,
                       local_settings: %{initial_window_size: remoteWindow},
                       remote_settings: %{initial_window_size: localWindow},
                       local_streamid: localStreamID),
           pseudoHeaders, headers0) do
    r_stream(local: :idle) = stream_get(streamID, state)
    tE = (case (:lists.keyfind("te", 1, headers0)) do
            {_, tE0} ->
              tE0
            false ->
              :undefined
          end)
    headers = merge_pseudo_headers(pseudoHeaders,
                                     remove_http11_headers(headers0))
    {headerBlock, encodeState} = :cow_hpack.encode(headers,
                                                     encodeState0)
    {:ok, localStreamID, headerBlock,
       stream_store(r_stream(id: localStreamID,
                        method: :maps.get(:method, pseudoHeaders), remote: :fin,
                        remote_expected_size: 0, local_window: localWindow,
                        remote_window: remoteWindow, te: tE),
                      r_http2_machine(state, encode_state: encodeState, 
                                 local_streamid: localStreamID + 2))}
  end

  defp remove_http11_headers(headers) do
    removeHeaders0 = ["keep-alive", "proxy-connection", "transfer-encoding", "upgrade"]
    removeHeaders = (case (:lists.keyfind("connection", 1, headers)) do
                       false ->
                         removeHeaders0
                       {_, connHd} ->
                         connection = :cow_http_hd.parse_connection(connHd)
                         connection ++ ["connection" | removeHeaders0]
                     end)
    :lists.filter(fn {name, _} ->
                       not :lists.member(name, removeHeaders)
                  end,
                    headers)
  end

  defp merge_pseudo_headers(pseudoHeaders, headers0) do
    :lists.foldl(fn {:status, status}, acc
                        when is_integer(status) ->
                      [{":status", :erlang.integer_to_binary(status)} | acc]
                    {name, value}, acc ->
                      [{:erlang.iolist_to_binary([?:,
                                                      :erlang.atom_to_binary(name,
                                                                               :latin1)]),
                          value} |
                           acc]
                 end,
                   headers0, :maps.to_list(pseudoHeaders))
  end

  def prepare_trailers(streamID, state = r_http2_machine(encode_state: encodeState0),
           trailers) do
    stream = (r_stream(local: :nofin) = stream_get(streamID,
                                              state))
    {headerBlock, encodeState} = :cow_hpack.encode(trailers,
                                                     encodeState0)
    {:ok, headerBlock,
       stream_store(r_stream(stream, local: :fin),
                      r_http2_machine(state, encode_state: encodeState))}
  end

  def send_or_queue_data(streamID,
           state0 = r_http2_machine(opts: opts, local_window: connWindow),
           isFin0, dataOrFileOrTrailers0) do
    stream0 = (r_stream(local: :nofin, local_window: streamWindow,
                   local_buffer_size: bufferSize,
                   te: tE0) = stream_get(streamID, state0))
    dataOrFileOrTrailers = (case (dataOrFileOrTrailers0) do
                              {:trailers, _} ->
                                tE = (try do
                                        :cow_http_hd.parse_te(tE0)
                                      catch
                                        _, _ ->
                                          :no_trailers
                                      else
                                        {:trailers, []} ->
                                          :trailers
                                        _ ->
                                          :no_trailers
                                      end)
                                case (tE) do
                                  :trailers ->
                                    dataOrFileOrTrailers0
                                  :no_trailers ->
                                    {:data, <<>>}
                                end
                              _ ->
                                dataOrFileOrTrailers0
                            end)
    sendSize = (case (dataOrFileOrTrailers) do
                  {:data, d} ->
                    bufferSize + :erlang.iolist_size(d)
                  r_sendfile(bytes: b) ->
                    bufferSize + b
                  {:trailers, _} ->
                    0
                end)
    minSendSize = :maps.get(:stream_window_data_threshold,
                              opts, 16384)
    cond do
      streamWindow < minSendSize and (streamWindow < sendSize or connWindow < sendSize) ->
        {:ok,
           stream_store(queue_data(stream0, isFin0,
                                     dataOrFileOrTrailers, :in),
                          state0)}
      true ->
        case (send_or_queue_data(stream0, state0, [], isFin0,
                                   dataOrFileOrTrailers, :in)) do
          {:ok, stream, state, []} ->
            {:ok, stream_store(stream, state)}
          {:ok, stream = r_stream(local: isFin), state, sendData} ->
            {:send, [{streamID, isFin, :lists.reverse(sendData)}],
               stream_store(stream, state)}
        end
    end
  end

  defp send_data(state0 = r_http2_machine(streams: streams0)) do
    iterator = :maps.iterator(streams0)
    case (send_data_for_all_streams(:maps.next(iterator),
                                      streams0, state0, [])) do
      {:ok, streams, state, []} ->
        {:ok, r_http2_machine(state, streams: streams)}
      {:ok, streams, state, send} ->
        {:send, send, r_http2_machine(state, streams: streams)}
    end
  end

  defp send_data_for_all_streams(:none, streams, state, send) do
    {:ok, streams, state, send}
  end

  defp send_data_for_all_streams(_, streams, state = r_http2_machine(local_window: connWindow),
            send)
      when connWindow <= 0 do
    {:ok, streams, state, send}
  end

  defp send_data_for_all_streams({streamID, stream0, iterator}, streams, state0,
            send) do
    case (send_data_for_one_stream(stream0, state0, [])) do
      {:ok, stream, state, []} ->
        send_data_for_all_streams(:maps.next(iterator),
                                    Map.put(streams, streamID, stream), state,
                                    send)
      {:ok, r_stream(local: :fin, remote: :fin), state, sendData} ->
        send_data_for_all_streams(:maps.next(iterator),
                                    :maps.remove(streamID, streams), state,
                                    [{streamID, :fin, sendData} | send])
      {:ok, stream = r_stream(local: isFin), state, sendData} ->
        send_data_for_all_streams(:maps.next(iterator),
                                    Map.put(streams, streamID, stream), state,
                                    [{streamID, isFin, sendData} | send])
    end
  end

  defp send_data(stream0, state0) do
    case (send_data_for_one_stream(stream0, state0, [])) do
      {:ok, stream, state, []} ->
        {:ok, stream_store(stream, state)}
      {:ok, stream = r_stream(id: streamID, local: isFin), state,
         sendData} ->
        {:send, [{streamID, isFin, sendData}],
           stream_store(stream, state)}
    end
  end

  defp send_data_for_one_stream(stream = r_stream(local: :nofin, local_buffer_size: 0,
                       local_trailers: trailers),
            state, sendAcc)
      when trailers !== :undefined do
    {:ok, stream, state,
       :lists.reverse([{:trailers, trailers} | sendAcc])}
  end

  defp send_data_for_one_stream(stream = r_stream(local: :nofin, local_buffer: q0,
                       local_buffer_size: 0),
            state, sendAcc) do
    case (:queue.len(q0)) do
      0 ->
        {:ok, stream, state, :lists.reverse(sendAcc)}
      1 ->
        {{:value, {:fin, 0, _}}, q} = :queue.out(q0)
        {:ok, r_stream(stream, local: :fin,  local_buffer: q), state,
           :lists.reverse(sendAcc)}
    end
  end

  defp send_data_for_one_stream(stream = r_stream(local: isFin,
                       local_window: streamWindow,
                       local_buffer_size: bufferSize),
            state = r_http2_machine(local_window: connWindow), sendAcc)
      when connWindow <= 0 or isFin === :fin or
             streamWindow <= 0 or bufferSize === 0 do
    {:ok, stream, state, :lists.reverse(sendAcc)}
  end

  defp send_data_for_one_stream(stream0 = r_stream(local_window: streamWindow,
                        local_buffer: q0, local_buffer_size: bufferSize),
            state0 = r_http2_machine(opts: opts, local_window: connWindow),
            sendAcc0) do
    minSendSize = :maps.get(:stream_window_data_threshold,
                              opts, 16384)
    cond do
      sendAcc0 === [] and streamWindow < minSendSize and (streamWindow < bufferSize or connWindow < bufferSize) ->
        {:ok, stream0, state0, []}
      true ->
        {{:value, {isFin, dataSize, data}}, q} = :queue.out(q0)
        stream1 = r_stream(stream0, local_buffer: q, 
                               local_buffer_size: bufferSize - dataSize)
        {:ok, stream, state,
           sendAcc} = send_or_queue_data(stream1, state0, sendAcc0,
                                           isFin, data, :in_r)
        send_data_for_one_stream(stream, state, sendAcc)
    end
  end

  defp send_or_queue_data(stream = r_stream(local_buffer_size: 0), state,
            sendAcc, :fin, {:trailers, trailers}, _) do
    {:ok, stream, state, [{:trailers, trailers} | sendAcc]}
  end

  defp send_or_queue_data(stream, state, sendAcc, :fin,
            {:trailers, trailers}, _) do
    {:ok, r_stream(stream, local_trailers: trailers), state,
       sendAcc}
  end

  defp send_or_queue_data(stream = r_stream(local_window: streamWindow),
            state = r_http2_machine(local_window: connWindow), sendAcc, isFin,
            data, in__)
      when connWindow <= 0 or streamWindow <= 0 do
    {:ok, queue_data(stream, isFin, data, in__), state,
       sendAcc}
  end

  defp send_or_queue_data(stream = r_stream(local_window: streamWindow),
            state = r_http2_machine(opts: opts, remote_settings: remoteSettings,
                        local_window: connWindow),
            sendAcc, isFin, data, in__) do
    remoteMaxFrameSize = :maps.get(:max_frame_size,
                                     remoteSettings, 16384)
    configuredMaxFrameSize = :maps.get(:max_frame_size_sent,
                                         opts, :infinity)
    maxSendSize = min(min(connWindow, streamWindow),
                        min(remoteMaxFrameSize, configuredMaxFrameSize))
    case (data) do
      file = r_sendfile(bytes: bytes) when bytes <= maxSendSize ->
        {:ok,
           r_stream(stream, local: isFin, 
                       local_window: streamWindow - bytes),
           r_http2_machine(state, local_window: connWindow - bytes),
           [file | sendAcc]}
      file = r_sendfile(offset: offset, bytes: bytes) ->
        send_or_queue_data(r_stream(stream, local_window: streamWindow - maxSendSize),
                             r_http2_machine(state, local_window: connWindow - maxSendSize),
                             [r_sendfile(file, bytes: maxSendSize) | sendAcc], isFin,
                             r_sendfile(file, offset: offset + maxSendSize, 
                                       bytes: bytes - maxSendSize),
                             in__)
      {:data, iolist0} ->
        iolistSize = :erlang.iolist_size(iolist0)
        cond do
          iolistSize <= maxSendSize ->
            {:ok,
               r_stream(stream, local: isFin, 
                           local_window: streamWindow - iolistSize),
               r_http2_machine(state, local_window: connWindow - iolistSize),
               [{:data, iolist0} | sendAcc]}
          true ->
            {iolist, more} = :cow_iolists.split(maxSendSize,
                                                  iolist0)
            send_or_queue_data(r_stream(stream, local_window: streamWindow - maxSendSize),
                                 r_http2_machine(state, local_window: connWindow - maxSendSize),
                                 [{:data, iolist} | sendAcc], isFin,
                                 {:data, more}, in__)
        end
    end
  end

  defp queue_data(stream = r_stream(local_buffer: q0,
                       local_buffer_size: size0),
            isFin, data, in__) do
    dataSize = (case (data) do
                  {:sendfile, _, bytes, _} ->
                    bytes
                  {:data, iolist} ->
                    :erlang.iolist_size(iolist)
                end)
    case ({dataSize, isFin}) do
      {0, :nofin} ->
        stream
      _ ->
        q = apply(:queue, in__, [{isFin, dataSize, data}, q0])
        r_stream(stream, local_buffer: q, 
                    local_buffer_size: size0 + dataSize)
    end
  end

  def ensure_window(size,
           state = r_http2_machine(opts: opts, remote_window: remoteWindow)) do
    case (ensure_window(size, remoteWindow, :connection,
                          opts)) do
      :ok ->
        :ok
      {:ok, increment} ->
        {:ok, increment,
           r_http2_machine(state, remote_window: remoteWindow + increment)}
    end
  end

  def ensure_window(streamID, size, state = r_http2_machine(opts: opts)) do
    case (stream_get(streamID, state)) do
      :undefined ->
        :ok
      stream = r_stream(remote_window: remoteWindow) ->
        case (ensure_window(size, remoteWindow, :stream,
                              opts)) do
          :ok ->
            :ok
          {:ok, increment} ->
            {:ok, increment,
               stream_store(r_stream(stream, remote_window: remoteWindow + increment),
                              state)}
        end
    end
  end

  defp ensure_window(0, _, _, _) do
    :ok
  end

  defp ensure_window(size, window, _, _) when size <= window do
    :ok
  end

  defp ensure_window(size0, window, type, opts) do
    threshold = ensure_window_threshold(type, opts)
    cond do
      window > threshold ->
        :ok
      true ->
        margin = ensure_window_margin(type, opts)
        size = size0 + margin
        maxWindow = ensure_window_max(type, opts)
        increment = (cond do
                       size > maxWindow ->
                         maxWindow - window
                       true ->
                         size - window
                     end)
        case (increment) do
          0 ->
            :ok
          _ ->
            {:ok, increment}
        end
    end
  end

  defp ensure_window_margin(:connection, opts) do
    :maps.get(:connection_window_margin_size, opts, 65535)
  end

  defp ensure_window_margin(:stream, opts) do
    :maps.get(:stream_window_margin_size, opts, 65535)
  end

  defp ensure_window_max(:connection, opts) do
    :maps.get(:max_connection_window_size, opts, 2147483647)
  end

  defp ensure_window_max(:stream, opts) do
    :maps.get(:max_stream_window_size, opts, 2147483647)
  end

  defp ensure_window_threshold(:connection, opts) do
    :maps.get(:connection_window_update_threshold, opts,
                163840)
  end

  defp ensure_window_threshold(:stream, opts) do
    :maps.get(:stream_window_update_threshold, opts, 163840)
  end

  def update_window(size, state = r_http2_machine(remote_window: remoteWindow))
      when size > 0 do
    r_http2_machine(state, remote_window: remoteWindow + size)
  end

  def update_window(streamID, size, state) when size > 0 do
    stream = (r_stream(remote_window: remoteWindow) = stream_get(streamID,
                                                            state))
    stream_store(r_stream(stream, remote_window: remoteWindow + size),
                   state)
  end

  def reset_stream(streamID, state = r_http2_machine(streams: streams0)) do
    case (:maps.take(streamID, streams0)) do
      {_, streams} ->
        {:ok,
           stream_linger(streamID, r_http2_machine(state, streams: streams))}
      :error ->
        {:error, :not_found}
    end
  end

  def get_connection_local_buffer_size(r_http2_machine(streams: streams)) do
    :maps.fold(fn _, r_stream(local_buffer_size: size), acc ->
                    acc + size
               end,
                 0, streams)
  end

  def get_local_setting(key, r_http2_machine(local_settings: settings)) do
    :maps.get(key, settings, default_setting_value(key))
  end

  def get_remote_settings(r_http2_machine(mode: mode, remote_settings: settings)) do
    defaults0 = %{header_table_size:
                  default_setting_value(:header_table_size),
                    enable_push: default_setting_value(:enable_push),
                    max_concurrent_streams:
                    default_setting_value(:max_concurrent_streams),
                    initial_window_size:
                    default_setting_value(:initial_window_size),
                    max_frame_size: default_setting_value(:max_frame_size),
                    max_header_list_size:
                    default_setting_value(:max_header_list_size)}
    defaults = (case (mode) do
                  :server ->
                    Map.put(defaults0, :enable_connect_protocol,
                                         default_setting_value(:enable_connect_protocol))
                  :client ->
                    defaults0
                end)
    :maps.merge(defaults, settings)
  end

  defp default_setting_value(:header_table_size) do
    4096
  end

  defp default_setting_value(:enable_push) do
    true
  end

  defp default_setting_value(:max_concurrent_streams) do
    :infinity
  end

  defp default_setting_value(:initial_window_size) do
    65535
  end

  defp default_setting_value(:max_frame_size) do
    16384
  end

  defp default_setting_value(:max_header_list_size) do
    :infinity
  end

  defp default_setting_value(:enable_connect_protocol) do
    false
  end

  def get_last_streamid(r_http2_machine(remote_streamid: remoteStreamID)) do
    remoteStreamID
  end

  def get_stream_local_buffer_size(streamID,
           state = r_http2_machine(mode: mode, local_streamid: localStreamID,
                       remote_streamid: remoteStreamID)) do
    case (stream_get(streamID, state)) do
      r_stream(local_buffer_size: size) ->
        {:ok, size}
      :undefined
          when (mode === :server and rem(streamID, 2) === 0 or mode === :client and rem(streamID, 2) === 1) and streamID < localStreamID or not
                                                                                                                                            (mode === :server and rem(streamID, 2) === 0 or mode === :client and rem(streamID, 2) === 1) and streamID <= remoteStreamID
               ->
        {:error, :closed}
      :undefined ->
        {:error, :not_found}
    end
  end

  def get_stream_local_state(streamID,
           state = r_http2_machine(mode: mode, local_streamid: localStreamID,
                       remote_streamid: remoteStreamID)) do
    case (stream_get(streamID, state)) do
      r_stream(local: isFin, local_buffer: q,
          local_trailers: :undefined) ->
        isQueueFin = (case (:queue.peek_r(q)) do
                        :empty ->
                          :empty
                        {:value, {isQueueFin0, _, _}} ->
                          isQueueFin0
                      end)
        {:ok, isFin, isQueueFin}
      r_stream(local: isFin) ->
        {:ok, isFin, :fin}
      :undefined
          when (mode === :server and rem(streamID, 2) === 0 or mode === :client and rem(streamID, 2) === 1) and streamID < localStreamID or not
                                                                                                                                            (mode === :server and rem(streamID, 2) === 0 or mode === :client and rem(streamID, 2) === 1) and streamID <= remoteStreamID
               ->
        {:error, :closed}
      :undefined ->
        {:error, :not_found}
    end
  end

  def get_stream_remote_state(streamID,
           state = r_http2_machine(mode: mode, local_streamid: localStreamID,
                       remote_streamid: remoteStreamID)) do
    case (stream_get(streamID, state)) do
      r_stream(remote: isFin) ->
        {:ok, isFin}
      :undefined
          when (mode === :server and rem(streamID, 2) === 0 or mode === :client and rem(streamID, 2) === 1) and streamID < localStreamID or not
                                                                                                                                            (mode === :server and rem(streamID, 2) === 0 or mode === :client and rem(streamID, 2) === 1) and streamID <= remoteStreamID
               ->
        {:error, :closed}
      :undefined ->
        {:error, :not_found}
    end
  end

  def is_lingering_stream(streamID,
           r_http2_machine(local_lingering_streams: local,
               remote_lingering_streams: remote)) do
    case (:lists.member(streamID, local)) do
      true ->
        true
      false ->
        :lists.member(streamID, remote)
    end
  end

  defp stream_get(streamID, r_http2_machine(streams: streams)) do
    :maps.get(streamID, streams, :undefined)
  end

  defp stream_store(r_stream(id: streamID, local: :fin, remote: :fin),
            state = r_http2_machine(streams: streams0)) do
    streams = :maps.remove(streamID, streams0)
    r_http2_machine(state, streams: streams)
  end

  defp stream_store(stream = r_stream(id: streamID),
            state = r_http2_machine(streams: streams)) do
    r_http2_machine(state, streams: Map.put(streams, streamID, stream))
  end

  defp stream_reset(streamID, state, reason, humanReadable) do
    {:error,
       {:stream_error, streamID, reason, humanReadable},
       stream_linger(streamID, state)}
  end

  defp stream_linger(streamID,
            state = r_http2_machine(local_lingering_streams: lingering0)) do
    lingering = [streamID | :lists.sublist(lingering0,
                                             100 - 1)]
    r_http2_machine(state, local_lingering_streams: lingering)
  end

end