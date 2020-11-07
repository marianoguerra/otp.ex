defmodule :cowboy_compress_h do
  use Bitwise
  @behaviour :cowboy_stream
  require Record

  Record.defrecord(:r_state, :state,
    next: :undefined,
    threshold: :undefined,
    compress: :undefined,
    deflate: :undefined,
    deflate_flush: :sync
  )

  def init(streamID, req, opts) do
    state0 = check_req(req)
    compressThreshold = :maps.get(:compress_threshold, opts, 300)
    deflateFlush = buffering_to_zflush(:maps.get(:compress_buffering, opts, false))
    {commands0, next} = :cowboy_stream.init(streamID, req, opts)

    fold(
      commands0,
      r_state(state0, next: next, threshold: compressThreshold, deflate_flush: deflateFlush)
    )
  end

  def data(streamID, isFin, data, state0 = r_state(next: next0)) do
    {commands0, next} = :cowboy_stream.data(streamID, isFin, data, next0)
    fold(commands0, r_state(state0, next: next))
  end

  def info(streamID, info, state0 = r_state(next: next0)) do
    {commands0, next} = :cowboy_stream.info(streamID, info, next0)
    fold(commands0, r_state(state0, next: next))
  end

  def terminate(streamID, reason, r_state(next: next, deflate: z)) do
    case z do
      :undefined ->
        :ok

      _ ->
        :zlib.close(z)
    end

    :cowboy_stream.terminate(streamID, reason, next)
  end

  def early_error(streamID, reason, partialReq, resp, opts) do
    :cowboy_stream.early_error(streamID, reason, partialReq, resp, opts)
  end

  defp check_req(req) do
    try do
      :cowboy_req.parse_header("accept-encoding", req)
    catch
      _, _ ->
        r_state(compress: :undefined)
    else
      :undefined ->
        r_state(compress: :undefined)

      encodings ->
        case (for e = {"gzip", q} <- encodings, q !== 0 do
                e
              end) do
          [] ->
            r_state(compress: :undefined)

          _ ->
            r_state(compress: :gzip)
        end
    end
  end

  defp check_resp_headers(%{"content-encoding" => _}, state) do
    r_state(state, compress: :undefined)
  end

  defp check_resp_headers(_, state) do
    state
  end

  defp fold(commands, state = r_state(compress: :undefined)) do
    {commands, state}
  end

  defp fold(commands, state) do
    fold(commands, state, [])
  end

  defp fold([], state, acc) do
    {:lists.reverse(acc), state}
  end

  defp fold(
         [
           response = {:response, _, _, {:sendfile, _, _, _}}
           | tail
         ],
         state,
         acc
       ) do
    fold(tail, state, [response | acc])
  end

  defp fold(
         [
           response0 = {:response, _, headers, body}
           | tail
         ],
         state0 = r_state(threshold: compressThreshold),
         acc
       ) do
    case check_resp_headers(headers, state0) do
      state = r_state(compress: :undefined) ->
        fold(tail, state, [response0 | acc])

      state1 ->
        bodyLength = :erlang.iolist_size(body)

        cond do
          bodyLength <= compressThreshold ->
            fold(tail, state1, [response0 | acc])

          true ->
            {response, state} = gzip_response(response0, state1)
            fold(tail, state, [response | acc])
        end
    end
  end

  defp fold([response0 = {:headers, _, headers} | tail], state0, acc) do
    case check_resp_headers(headers, state0) do
      state = r_state(compress: :undefined) ->
        fold(tail, state, [response0 | acc])

      state1 ->
        {response, state} = gzip_headers(response0, state1)
        fold(tail, state, [response | acc])
    end
  end

  defp fold([data0 = {:data, _, _} | tail], state0 = r_state(compress: :gzip), acc) do
    {data, state} = gzip_data(data0, state0)
    fold(tail, state, [data | acc])
  end

  defp fold([trailers = {:trailers, _} | tail], state0 = r_state(compress: :gzip), acc) do
    {{:data, :fin, data}, state} =
      gzip_data(
        {:data, :fin, <<>>},
        state0
      )

    fold(tail, state, [[trailers, {:data, :nofin, data}] | acc])
  end

  defp fold(
         [setOptions = {:set_options, opts} | tail],
         state =
           r_state(
             threshold: compressThreshold0,
             deflate_flush: deflateFlush0
           ),
         acc
       ) do
    compressThreshold = :maps.get(:compress_threshold, opts, compressThreshold0)

    deflateFlush =
      case opts do
        %{:compress_buffering => compressBuffering} ->
          buffering_to_zflush(compressBuffering)

        _ ->
          deflateFlush0
      end

    fold(
      tail,
      r_state(state,
        threshold: compressThreshold,
        deflate_flush: deflateFlush
      ),
      [setOptions | acc]
    )
  end

  defp fold([command | tail], state, acc) do
    fold(tail, state, [command | acc])
  end

  defp buffering_to_zflush(true) do
    :none
  end

  defp buffering_to_zflush(false) do
    :sync
  end

  defp gzip_response({:response, status, headers, body}, state) do
    z = :zlib.open()

    gzBody =
      try do
        :zlib.deflateInit(z, :default, :deflated, 31, 8, :default)
        gz = :zlib.deflate(z, body, :finish)
        :zlib.deflateEnd(z)
        gz
      after
        :zlib.close(z)
      end

    {{:response, status,
      vary(%{
        headers
        | "content-length" => :erlang.integer_to_binary(:erlang.iolist_size(gzBody)),
          "content-encoding" => "gzip"
      }), gzBody}, state}
  end

  defp gzip_headers({:headers, status, headers0}, state) do
    z = :zlib.open()
    :zlib.deflateInit(z, :default, :deflated, 31, 8, :default)
    headers = :maps.remove("content-length", headers0)

    {{:headers, status, vary(%{headers | "content-encoding" => "gzip"})},
     r_state(state, deflate: z)}
  end

  defp vary(headers = %{"vary" => vary}) do
    try do
      :cow_http_hd.parse_vary(:erlang.iolist_to_binary(vary))
    catch
      _, _ ->
        %{headers | "vary" => "accept-encoding"}
    else
      :* ->
        headers

      list ->
        case :lists.member("accept-encoding", list) do
          true ->
            headers

          false ->
            %{headers | "vary" => [vary, ", accept-encoding"]}
        end
    end
  end

  defp vary(headers) do
    %{headers | "vary" => "accept-encoding"}
  end

  defp gzip_data(
         {:data, :nofin, sendfile = {:sendfile, _, _, _}},
         state = r_state(deflate: z, deflate_flush: flush)
       ) do
    {:ok, data0} = read_file(sendfile)
    data = :zlib.deflate(z, data0, flush)
    {{:data, :nofin, data}, state}
  end

  defp gzip_data(
         {:data, :fin, sendfile = {:sendfile, _, _, _}},
         state = r_state(deflate: z)
       ) do
    {:ok, data0} = read_file(sendfile)
    data = :zlib.deflate(z, data0, :finish)
    :zlib.deflateEnd(z)
    :zlib.close(z)
    {{:data, :fin, data}, r_state(state, deflate: :undefined)}
  end

  defp gzip_data(
         {:data, :nofin, data0},
         state = r_state(deflate: z, deflate_flush: flush)
       ) do
    data = :zlib.deflate(z, data0, flush)
    {{:data, :nofin, data}, state}
  end

  defp gzip_data({:data, :fin, data0}, state = r_state(deflate: z)) do
    data = :zlib.deflate(z, data0, :finish)
    :zlib.deflateEnd(z)
    :zlib.close(z)
    {{:data, :fin, data}, r_state(state, deflate: :undefined)}
  end

  defp read_file({:sendfile, offset, bytes, path}) do
    {:ok, ioDevice} =
      :file.open(
        path,
        [:read, :raw, :binary]
      )

    try do
      _ =
        case offset do
          0 ->
            :ok

          _ ->
            :file.position(ioDevice, {:bof, offset})
        end

      :file.read(ioDevice, bytes)
    after
      :file.close(ioDevice)
    end
  end
end
