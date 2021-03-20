defmodule :cow_ws do
  use Bitwise
  def key() do
    :base64.encode(:crypto.strong_rand_bytes(16))
  end

  def encode_key(key) do
    :base64.encode(:crypto.hash(:sha, [key, '258EAFA5-E914-47DA-95CA-C5AB0DC85B11']))
  end

  def negotiate_permessage_deflate(_, %{deflate: _}, _) do
    :ignore
  end

  def negotiate_permessage_deflate(params, extensions, opts) do
    case (:lists.usort(params)) do
      params2 when length(params) !== length(params2) ->
        :ignore
      params2 ->
        negotiate_permessage_deflate1(params2, extensions, opts)
    end
  end

  defp negotiate_permessage_deflate1(params, extensions, opts) do
    serverTakeover = :maps.get(:server_context_takeover,
                                 opts, :takeover)
    clientTakeover = :maps.get(:client_context_takeover,
                                 opts, :takeover)
    serverMaxWindowBits = :maps.get(:server_max_window_bits,
                                      opts, 15)
    clientMaxWindowBits = :maps.get(:client_max_window_bits,
                                      opts, 15)
    respParams0 = (case (serverTakeover) do
                     :takeover ->
                       []
                     :no_takeover ->
                       ["; server_no_context_takeover"]
                   end)
    respParams1 = (case (clientTakeover) do
                     :takeover ->
                       respParams0
                     :no_takeover ->
                       ["; client_no_context_takeover" | respParams0]
                   end)
    negotiated0 = %{server_context_takeover: serverTakeover,
                      client_context_takeover: clientTakeover,
                      server_max_window_bits: serverMaxWindowBits,
                      client_max_window_bits: clientMaxWindowBits}
    case (negotiate_params(params, negotiated0,
                             respParams1)) do
      :ignore ->
        :ignore
      {%{server_max_window_bits: sB}, _}
          when sB > serverMaxWindowBits ->
        :ignore
      {%{client_max_window_bits: cB}, _}
          when cB > clientMaxWindowBits ->
        :ignore
      {negotiated, respParams2} ->
        respParams = (case (negotiated) do
                        %{server_max_window_bits_set: true} ->
                          respParams2
                        _ when serverMaxWindowBits === 15 ->
                          respParams2
                        _ ->
                          ["; server_max_window_bits=", :erlang.integer_to_binary(serverMaxWindowBits) |
                                  respParams2]
                      end)
        {inflate,
           deflate} = init_permessage_deflate(:maps.get(:client_max_window_bits,
                                                          negotiated),
                                                :maps.get(:server_max_window_bits,
                                                            negotiated),
                                                opts)
        {:ok, ["permessage-deflate", respParams],
           Map.merge(extensions, %{deflate: deflate,
                                     deflate_takeover:
                                     :maps.get(:server_context_takeover,
                                                 negotiated),
                                     inflate: inflate,
                                     inflate_takeover:
                                     :maps.get(:client_context_takeover,
                                                 negotiated)})}
    end
  end

  defp negotiate_params([], negotiated, respParams) do
    {negotiated, respParams}
  end

  defp negotiate_params(["client_max_window_bits" | tail], negotiated, respParams) do
    cB = :maps.get(:client_max_window_bits, negotiated)
    negotiate_params(tail,
                       Map.put(negotiated, :client_max_window_bits_set, true),
                       ["; client_max_window_bits=", :erlang.integer_to_binary(cB) | respParams])
  end

  defp negotiate_params([{"client_max_window_bits", max} | tail], negotiated, respParams) do
    cB0 = :maps.get(:client_max_window_bits, negotiated,
                      :undefined)
    case (parse_max_window_bits(max)) do
      :error ->
        :ignore
      cB when cB <= cB0 ->
        negotiate_params(tail,
                           Map.put(negotiated, :client_max_window_bits, cB),
                           ["; client_max_window_bits=", max | respParams])
      _ ->
        negotiate_params(tail, negotiated,
                           ["; client_max_window_bits=", :erlang.integer_to_binary(cB0) | respParams])
    end
  end

  defp negotiate_params([{"server_max_window_bits", max} | tail], negotiated, respParams) do
    sB0 = :maps.get(:server_max_window_bits, negotiated,
                      :undefined)
    case (parse_max_window_bits(max)) do
      :error ->
        :ignore
      sB when sB <= sB0 ->
        negotiate_params(tail,
                           Map.merge(negotiated, %{server_max_window_bits: sB,
                                                     server_max_window_bits_set:
                                                     true}),
                           ["; server_max_window_bits=", max | respParams])
      _ ->
        negotiate_params(tail, negotiated, respParams)
    end
  end

  defp negotiate_params(["client_no_context_takeover" | tail], negotiated, respParams) do
    case (:maps.get(:client_context_takeover,
                      negotiated)) do
      :no_takeover ->
        negotiate_params(tail, negotiated, respParams)
      :takeover ->
        negotiate_params(tail,
                           Map.put(negotiated, :client_context_takeover,
                                                 :no_takeover),
                           ["; client_no_context_takeover" | respParams])
    end
  end

  defp negotiate_params(["server_no_context_takeover" | tail], negotiated, respParams) do
    case (:maps.get(:server_context_takeover,
                      negotiated)) do
      :no_takeover ->
        negotiate_params(tail, negotiated, respParams)
      :takeover ->
        negotiate_params(tail,
                           Map.put(negotiated, :server_context_takeover,
                                                 :no_takeover),
                           ["; server_no_context_takeover" | respParams])
    end
  end

  defp negotiate_params(_, _, _) do
    :ignore
  end

  defp parse_max_window_bits("8") do
    8
  end

  defp parse_max_window_bits("9") do
    9
  end

  defp parse_max_window_bits("10") do
    10
  end

  defp parse_max_window_bits("11") do
    11
  end

  defp parse_max_window_bits("12") do
    12
  end

  defp parse_max_window_bits("13") do
    13
  end

  defp parse_max_window_bits("14") do
    14
  end

  defp parse_max_window_bits("15") do
    15
  end

  defp parse_max_window_bits(_) do
    :error
  end

  defp init_permessage_deflate(inflateWindowBits, deflateWindowBits, opts) do
    inflate = :zlib.open()
    :ok = :zlib.inflateInit(inflate, - inflateWindowBits)
    deflate = :zlib.open()
    deflateWindowBits2 = (case (deflateWindowBits) do
                            8 ->
                              9
                            _ ->
                              deflateWindowBits
                          end)
    :ok = :zlib.deflateInit(deflate,
                              :maps.get(:level, opts, :best_compression),
                              :deflated, - deflateWindowBits2,
                              :maps.get(:mem_level, opts, 8),
                              :maps.get(:strategy, opts, :default))
    case (opts) do
      %{owner: pid} ->
        set_owner(pid, inflate, deflate)
      _ ->
        :ok
    end
    {inflate, deflate}
  end

  defp set_owner(pid, inflate, deflate) do
    :zlib.set_controlling_process(inflate, pid)
    :zlib.set_controlling_process(deflate, pid)
  end

  def negotiate_x_webkit_deflate_frame(_, %{deflate: _}, _) do
    :ignore
  end

  def negotiate_x_webkit_deflate_frame(_Params, extensions, opts) do
    {inflate, deflate} = init_permessage_deflate(15, 15,
                                                   opts)
    {:ok, "x-webkit-deflate-frame",
       Map.merge(extensions, %{deflate: deflate,
                                 deflate_takeover: :takeover, inflate: inflate,
                                 inflate_takeover: :takeover})}
  end

  def validate_permessage_deflate(_, %{deflate: _}, _) do
    :error
  end

  def validate_permessage_deflate(params, extensions, opts) do
    case (:lists.usort(params)) do
      params2 when length(params) !== length(params2) ->
        :error
      params2 ->
        case (parse_response_permessage_deflate_params(params2,
                                                         15, :takeover, 15,
                                                         :takeover)) do
          :error ->
            :error
          {clientWindowBits, clientTakeOver, serverWindowBits,
             serverTakeOver} ->
            {inflate,
               deflate} = init_permessage_deflate(serverWindowBits,
                                                    clientWindowBits, opts)
            {:ok,
               Map.merge(extensions, %{deflate: deflate,
                                         deflate_takeover: clientTakeOver,
                                         inflate: inflate,
                                         inflate_takeover: serverTakeOver})}
        end
    end
  end

  defp parse_response_permessage_deflate_params([], cB, cTO, sB, sTO) do
    {cB, cTO, sB, sTO}
  end

  defp parse_response_permessage_deflate_params([{"client_max_window_bits", max} | tail], _, cTO, sB, sTO) do
    case (parse_max_window_bits(max)) do
      :error ->
        :error
      cB ->
        parse_response_permessage_deflate_params(tail, cB, cTO,
                                                   sB, sTO)
    end
  end

  defp parse_response_permessage_deflate_params(["client_no_context_takeover" | tail], cB, _, sB, sTO) do
    parse_response_permessage_deflate_params(tail, cB,
                                               :no_takeover, sB, sTO)
  end

  defp parse_response_permessage_deflate_params([{"server_max_window_bits", max} | tail], cB, cTO, _, sTO) do
    case (parse_max_window_bits(max)) do
      :error ->
        :error
      sB ->
        parse_response_permessage_deflate_params(tail, cB, cTO,
                                                   sB, sTO)
    end
  end

  defp parse_response_permessage_deflate_params(["server_no_context_takeover" | tail], cB, cTO, sB, _) do
    parse_response_permessage_deflate_params(tail, cB, cTO,
                                               sB, :no_takeover)
  end

  defp parse_response_permessage_deflate_params(_, _, _, _, _) do
    :error
  end

  def parse_header(<<_ :: size(1), rsv :: size(3), _ :: bits>>,
           extensions, _)
      when (extensions === %{} and rsv !== 0) do
    :error
  end

  def parse_header(<<_ :: size(2), 1 :: size(1), _ :: bits>>,
           %{deflate: _}, _) do
    :error
  end

  def parse_header(<<_ :: size(3), 1 :: size(1), _ :: bits>>,
           %{deflate: _}, _) do
    :error
  end

  def parse_header(<<_ :: size(4), 3 :: size(4), _ :: bits>>, _,
           _) do
    :error
  end

  def parse_header(<<_ :: size(4), 4 :: size(4), _ :: bits>>, _,
           _) do
    :error
  end

  def parse_header(<<_ :: size(4), 5 :: size(4), _ :: bits>>, _,
           _) do
    :error
  end

  def parse_header(<<_ :: size(4), 6 :: size(4), _ :: bits>>, _,
           _) do
    :error
  end

  def parse_header(<<_ :: size(4), 7 :: size(4), _ :: bits>>, _,
           _) do
    :error
  end

  def parse_header(<<_ :: size(4), 11 :: size(4), _ :: bits>>, _,
           _) do
    :error
  end

  def parse_header(<<_ :: size(4), 12 :: size(4), _ :: bits>>, _,
           _) do
    :error
  end

  def parse_header(<<_ :: size(4), 13 :: size(4), _ :: bits>>, _,
           _) do
    :error
  end

  def parse_header(<<_ :: size(4), 14 :: size(4), _ :: bits>>, _,
           _) do
    :error
  end

  def parse_header(<<_ :: size(4), 15 :: size(4), _ :: bits>>, _,
           _) do
    :error
  end

  def parse_header(<<0 :: size(1), _ :: size(3), opcode :: size(4),
             _ :: bits>>,
           _, _)
      when opcode >= 8 do
    :error
  end

  def parse_header(<<_ :: size(4), 0 :: size(4), _ :: bits>>, _,
           :undefined) do
    :error
  end

  def parse_header(<<_ :: size(4), 1 :: size(4), _ :: bits>>, _,
           {_, _, _}) do
    :error
  end

  def parse_header(<<_ :: size(4), 2 :: size(4), _ :: bits>>, _,
           {_, _, _}) do
    :error
  end

  def parse_header(<<_ :: size(4), 3 :: size(4), _ :: bits>>, _,
           {_, _, _}) do
    :error
  end

  def parse_header(<<_ :: size(4), 4 :: size(4), _ :: bits>>, _,
           {_, _, _}) do
    :error
  end

  def parse_header(<<_ :: size(4), 5 :: size(4), _ :: bits>>, _,
           {_, _, _}) do
    :error
  end

  def parse_header(<<_ :: size(4), 6 :: size(4), _ :: bits>>, _,
           {_, _, _}) do
    :error
  end

  def parse_header(<<_ :: size(4), 7 :: size(4), _ :: bits>>, _,
           {_, _, _}) do
    :error
  end

  def parse_header(<<_ :: size(4), 8 :: size(4), _ :: size(1),
             1 :: size(7), _ :: bits>>,
           _, _) do
    :error
  end

  def parse_header(data = <<_ :: size(4), 8 :: size(4),
                    0 :: size(1), len :: size(7), _ :: bits>>,
           _, _)
      when (len > 1 and byte_size(data) < 4) do
    :more
  end

  def parse_header(data = <<_ :: size(4), 8 :: size(4),
                    1 :: size(1), len :: size(7), _ :: bits>>,
           _, _)
      when (len > 1 and byte_size(data) < 8) do
    :more
  end

  def parse_header(<<fin :: size(1), rsv :: size(3) - bits,
             opcode :: size(4), 0 :: size(1), len :: size(7),
             rest :: bits>>,
           _, fragState)
      when len < 126 do
    parse_header(opcode, fin, fragState, rsv, len,
                   :undefined, rest)
  end

  def parse_header(<<fin :: size(1), rsv :: size(3) - bits,
             opcode :: size(4), 1 :: size(1), len :: size(7),
             maskKey :: size(32), rest :: bits>>,
           _, fragState)
      when len < 126 do
    parse_header(opcode, fin, fragState, rsv, len, maskKey,
                   rest)
  end

  def parse_header(<<fin :: size(1), rsv :: size(3) - bits,
             opcode :: size(4), 0 :: size(1), 126 :: size(7),
             len :: size(16), rest :: bits>>,
           _, fragState)
      when (len > 125 and opcode < 8) do
    parse_header(opcode, fin, fragState, rsv, len,
                   :undefined, rest)
  end

  def parse_header(<<fin :: size(1), rsv :: size(3) - bits,
             opcode :: size(4), 1 :: size(1), 126 :: size(7),
             len :: size(16), maskKey :: size(32), rest :: bits>>,
           _, fragState)
      when (len > 125 and opcode < 8) do
    parse_header(opcode, fin, fragState, rsv, len, maskKey,
                   rest)
  end

  def parse_header(<<fin :: size(1), rsv :: size(3) - bits,
             opcode :: size(4), 0 :: size(1), 127 :: size(7),
             0 :: size(1), len :: size(63), rest :: bits>>,
           _, fragState)
      when (len > 65535 and opcode < 8) do
    parse_header(opcode, fin, fragState, rsv, len,
                   :undefined, rest)
  end

  def parse_header(<<fin :: size(1), rsv :: size(3) - bits,
             opcode :: size(4), 1 :: size(1), 127 :: size(7),
             0 :: size(1), len :: size(63), maskKey :: size(32),
             rest :: bits>>,
           _, fragState)
      when (len > 65535 and opcode < 8) do
    parse_header(opcode, fin, fragState, rsv, len, maskKey,
                   rest)
  end

  def parse_header(<<_ :: size(9), 127 :: size(7), 1 :: size(1),
             _ :: bits>>,
           _, _) do
    :error
  end

  def parse_header(<<_ :: size(8), 0 :: size(1), 126 :: size(7),
             _ :: size(16), _ :: bits>>,
           _, _) do
    :error
  end

  def parse_header(<<_ :: size(8), 1 :: size(1), 126 :: size(7),
             _ :: size(48), _ :: bits>>,
           _, _) do
    :error
  end

  def parse_header(<<_ :: size(8), 0 :: size(1), 127 :: size(7),
             _ :: size(64), _ :: bits>>,
           _, _) do
    :error
  end

  def parse_header(<<_ :: size(8), 1 :: size(1), 127 :: size(7),
             _ :: size(96), _ :: bits>>,
           _, _) do
    :error
  end

  def parse_header(_, _, _) do
    :more
  end

  defp parse_header(opcode, fin, fragState, rsv, len, maskKey,
            rest) do
    type = opcode_to_frame_type(opcode)
    type2 = (case (fin) do
               0 ->
                 :fragment
               1 ->
                 type
             end)
    {type2, frag_state(type, fin, rsv, fragState), rsv, len,
       maskKey, rest}
  end

  defp opcode_to_frame_type(0) do
    :fragment
  end

  defp opcode_to_frame_type(1) do
    :text
  end

  defp opcode_to_frame_type(2) do
    :binary
  end

  defp opcode_to_frame_type(8) do
    :close
  end

  defp opcode_to_frame_type(9) do
    :ping
  end

  defp opcode_to_frame_type(10) do
    :pong
  end

  defp frag_state(type, 0, rsv, :undefined) do
    {:nofin, type, rsv}
  end

  defp frag_state(:fragment, 0, _, fragState = {:nofin, _, _}) do
    fragState
  end

  defp frag_state(:fragment, 1, _, {:nofin, type, rsv}) do
    {:fin, type, rsv}
  end

  defp frag_state(_, 1, _, fragState) do
    fragState
  end

  def parse_payload(data, _, utf8State, _, _, 0,
           {:fin, _, <<1 :: size(1), 0 :: size(2)>>},
           %{inflate: inflate, inflate_takeover: takeOver}, _) do
    _ = :zlib.inflate(inflate, <<0, 0, 255, 255>>)
    case (takeOver) do
      :no_takeover ->
        :zlib.inflateReset(inflate)
      :takeover ->
        :ok
    end
    {:ok, <<>>, utf8State, data}
  end

  def parse_payload(data, maskKey, utf8State, parsedLen, type, len,
           fragState = {_, _, <<1 :: size(1), 0 :: size(2)>>},
           %{inflate: inflate, inflate_takeover: takeOver}, _) do
    {data2, rest, eof} = split_payload(data, len)
    payload = inflate_frame(unmask(data2, maskKey,
                                     parsedLen),
                              inflate, takeOver, fragState, eof)
    validate_payload(payload, rest, utf8State, parsedLen,
                       type, fragState, eof)
  end

  def parse_payload(data, maskKey, utf8State, parsedLen, type, len,
           fragState,
           %{inflate: inflate, inflate_takeover: takeOver},
           <<1 :: size(1), 0 :: size(2)>>)
      when type === :text or type === :binary do
    {data2, rest, eof} = split_payload(data, len)
    payload = inflate_frame(unmask(data2, maskKey,
                                     parsedLen),
                              inflate, takeOver, fragState, eof)
    validate_payload(payload, rest, utf8State, parsedLen,
                       type, fragState, eof)
  end

  def parse_payload(data, _, utf8State, 0, _, 0, _, _, _)
      when utf8State === 0 or utf8State === :undefined do
    {:ok, <<>>, utf8State, data}
  end

  def parse_payload(data, maskKey, utf8State, 0, type = :close, len,
           fragState, _, <<0 :: size(3)>>) do
    {<<maskedCode :: size(2) - binary, data2 :: bits>>,
       rest, eof} = split_payload(data, len)
    <<closeCode :: size(16)>> = unmask(maskedCode, maskKey,
                                         0)
    case (validate_close_code(closeCode)) do
      :ok ->
        payload = unmask(data2, maskKey, 2)
        case (validate_payload(payload, rest, utf8State, 2,
                                 type, fragState, eof)) do
          {:ok, _, utf8State2, _} ->
            {:ok, closeCode, payload, utf8State2, rest}
          {:more, _, utf8State2} ->
            {:more, closeCode, payload, utf8State2}
          error ->
            error
        end
      :error ->
        {:error, :badframe}
    end
  end

  def parse_payload(data, maskKey, utf8State, parsedLen, type, len,
           fragState, _, <<0 :: size(3)>>) do
    {data2, rest, eof} = split_payload(data, len)
    payload = unmask(data2, maskKey, parsedLen)
    validate_payload(payload, rest, utf8State, parsedLen,
                       type, fragState, eof)
  end

  defp split_payload(data, len) do
    case (byte_size(data)) do
      ^len ->
        {data, <<>>, true}
      dataLen when dataLen < len ->
        {data, <<>>, false}
      _ ->
        <<data2 :: size(len) - binary, rest :: bits>> = data
        {data2, rest, true}
    end
  end

  defp validate_close_code(code) do
    cond do
      code < 1000 ->
        :error
      code === 1004 ->
        :error
      code === 1005 ->
        :error
      code === 1006 ->
        :error
      (code > 1011 and code < 3000) ->
        :error
      code > 4999 ->
        :error
      true ->
        :ok
    end
  end

  defp unmask(data, :undefined, _) do
    data
  end

  defp unmask(data, maskKey, 0) do
    mask(data, maskKey, <<>>)
  end

  defp unmask(data, maskKey, unmaskedLen) do
    left = rem(unmaskedLen, 4)
    right = 4 - left
    maskKey2 = maskKey <<< left * 8 + (maskKey >>> right * 8)
    mask(data, maskKey2, <<>>)
  end

  defp mask(<<>>, _, unmasked) do
    unmasked
  end

  defp mask(<<o :: size(32), rest :: bits>>, maskKey,
            acc) do
    t = o ^^^ maskKey
    mask(rest, maskKey, <<acc :: binary, t :: size(32)>>)
  end

  defp mask(<<o :: size(24)>>, maskKey, acc) do
    <<maskKey2 :: size(24), _ :: size(8)>> = <<maskKey
                                               ::
                                               size(32)>>
    t = o ^^^ maskKey2
    <<acc :: binary, t :: size(24)>>
  end

  defp mask(<<o :: size(16)>>, maskKey, acc) do
    <<maskKey2 :: size(16), _ :: size(16)>> = <<maskKey
                                                ::
                                                size(32)>>
    t = o ^^^ maskKey2
    <<acc :: binary, t :: size(16)>>
  end

  defp mask(<<o :: size(8)>>, maskKey, acc) do
    <<maskKey2 :: size(8), _ :: size(24)>> = <<maskKey
                                               ::
                                               size(32)>>
    t = o ^^^ maskKey2
    <<acc :: binary, t :: size(8)>>
  end

  defp inflate_frame(data, inflate, takeOver, fragState, true)
      when fragState === :undefined or
             :erlang.element(1, fragState) === :fin do
    data2 = :zlib.inflate(inflate,
                            <<data :: binary, 0, 0, 255, 255>>)
    case (takeOver) do
      :no_takeover ->
        :zlib.inflateReset(inflate)
      :takeover ->
        :ok
    end
    :erlang.iolist_to_binary(data2)
  end

  defp inflate_frame(data, inflate, _T, _F, _E) do
    :erlang.iolist_to_binary(:zlib.inflate(inflate, data))
  end

  defp validate_payload(payload, _, :undefined, _, _, _, false) do
    {:more, payload, :undefined}
  end

  defp validate_payload(payload, rest, :undefined, _, _, _, true) do
    {:ok, payload, :undefined, rest}
  end

  defp validate_payload(payload, rest, utf8State, _, type, _, eof)
      when type === :text or type === :close do
    case (validate_utf8(payload, utf8State)) do
      1 ->
        {:error, :badencoding}
      utf8State2 when not eof ->
        {:more, payload, utf8State2}
      0 when eof ->
        {:ok, payload, 0, rest}
      _ ->
        {:error, :badencoding}
    end
  end

  defp validate_payload(payload, rest, utf8State, _, :fragment,
            {fin, :text, _}, eof) do
    case (validate_utf8(payload, utf8State)) do
      1 ->
        {:error, :badencoding}
      0 when eof ->
        {:ok, payload, 0, rest}
      utf8State2 when (eof and fin === :nofin) ->
        {:ok, payload, utf8State2, rest}
      utf8State2 when not eof ->
        {:more, payload, utf8State2}
      _ ->
        {:error, :badencoding}
    end
  end

  defp validate_payload(payload, _, utf8State, _, _, _, false) do
    {:more, payload, utf8State}
  end

  defp validate_payload(payload, rest, utf8State, _, _, _, true) do
    {:ok, payload, utf8State, rest}
  end

  defp validate_utf8(<<>>, state) do
    state
  end

  defp validate_utf8(<<c, rest :: bits>>, 0) when c < 128 do
    validate_utf8(rest, 0)
  end

  defp validate_utf8(<<c, rest :: bits>>, 2) when (c >= 128 and
                                          c < 144) do
    validate_utf8(rest, 0)
  end

  defp validate_utf8(<<c, rest :: bits>>, 3) when (c >= 128 and
                                          c < 144) do
    validate_utf8(rest, 2)
  end

  defp validate_utf8(<<c, rest :: bits>>, 5) when (c >= 128 and
                                          c < 144) do
    validate_utf8(rest, 2)
  end

  defp validate_utf8(<<c, rest :: bits>>, 7) when (c >= 128 and
                                          c < 144) do
    validate_utf8(rest, 3)
  end

  defp validate_utf8(<<c, rest :: bits>>, 8) when (c >= 128 and
                                          c < 144) do
    validate_utf8(rest, 3)
  end

  defp validate_utf8(<<c, rest :: bits>>, 2) when (c >= 144 and
                                          c < 160) do
    validate_utf8(rest, 0)
  end

  defp validate_utf8(<<c, rest :: bits>>, 3) when (c >= 144 and
                                          c < 160) do
    validate_utf8(rest, 2)
  end

  defp validate_utf8(<<c, rest :: bits>>, 5) when (c >= 144 and
                                          c < 160) do
    validate_utf8(rest, 2)
  end

  defp validate_utf8(<<c, rest :: bits>>, 6) when (c >= 144 and
                                          c < 160) do
    validate_utf8(rest, 3)
  end

  defp validate_utf8(<<c, rest :: bits>>, 7) when (c >= 144 and
                                          c < 160) do
    validate_utf8(rest, 3)
  end

  defp validate_utf8(<<c, rest :: bits>>, 2) when (c >= 160 and
                                          c < 192) do
    validate_utf8(rest, 0)
  end

  defp validate_utf8(<<c, rest :: bits>>, 3) when (c >= 160 and
                                          c < 192) do
    validate_utf8(rest, 2)
  end

  defp validate_utf8(<<c, rest :: bits>>, 4) when (c >= 160 and
                                          c < 192) do
    validate_utf8(rest, 2)
  end

  defp validate_utf8(<<c, rest :: bits>>, 6) when (c >= 160 and
                                          c < 192) do
    validate_utf8(rest, 3)
  end

  defp validate_utf8(<<c, rest :: bits>>, 7) when (c >= 160 and
                                          c < 192) do
    validate_utf8(rest, 3)
  end

  defp validate_utf8(<<c, rest :: bits>>, 0) when (c >= 194 and
                                          c < 224) do
    validate_utf8(rest, 2)
  end

  defp validate_utf8(<<224, rest :: bits>>, 0) do
    validate_utf8(rest, 4)
  end

  defp validate_utf8(<<c, rest :: bits>>, 0) when (c >= 225 and
                                          c < 237) do
    validate_utf8(rest, 3)
  end

  defp validate_utf8(<<237, rest :: bits>>, 0) do
    validate_utf8(rest, 5)
  end

  defp validate_utf8(<<c, rest :: bits>>, 0) when c === 238 or
                                         c === 239 do
    validate_utf8(rest, 3)
  end

  defp validate_utf8(<<240, rest :: bits>>, 0) do
    validate_utf8(rest, 6)
  end

  defp validate_utf8(<<c, rest :: bits>>, 0) when c === 241 or
                                         c === 242 or c === 243 do
    validate_utf8(rest, 7)
  end

  defp validate_utf8(<<244, rest :: bits>>, 0) do
    validate_utf8(rest, 8)
  end

  defp validate_utf8(_, _) do
    1
  end

  def make_frame(:fragment, payload, _, {fin, type, _}) do
    {:fragment, fin, type, payload}
  end

  def make_frame(:text, payload, _, _) do
    {:text, payload}
  end

  def make_frame(:binary, payload, _, _) do
    {:binary, payload}
  end

  def make_frame(:close, <<>>, :undefined, _) do
    :close
  end

  def make_frame(:close, payload, closeCode, _) do
    {:close, closeCode, payload}
  end

  def make_frame(:ping, <<>>, _, _) do
    :ping
  end

  def make_frame(:ping, payload, _, _) do
    {:ping, payload}
  end

  def make_frame(:pong, <<>>, _, _) do
    :pong
  end

  def make_frame(:pong, payload, _, _) do
    {:pong, payload}
  end

  def frame(:close, _) do
    <<1 :: size(1), 0 :: size(3), 8 :: size(4),
        0 :: size(8)>>
  end

  def frame(:ping, _) do
    <<1 :: size(1), 0 :: size(3), 9 :: size(4),
        0 :: size(8)>>
  end

  def frame(:pong, _) do
    <<1 :: size(1), 0 :: size(3), 10 :: size(4),
        0 :: size(8)>>
  end

  def frame({:close, payload}, extensions) do
    frame({:close, 1000, payload}, extensions)
  end

  def frame({:close, statusCode, payload}, _) do
    len = 2 + :erlang.iolist_size(payload)
    true = len <= 125
    [<<1 :: size(1), 0 :: size(3), 8 :: size(4),
         0 :: size(1), len :: size(7), statusCode :: size(16)>>,
         payload]
  end

  def frame({:ping, payload}, _) do
    len = :erlang.iolist_size(payload)
    true = len <= 125
    [<<1 :: size(1), 0 :: size(3), 9 :: size(4),
         0 :: size(1), len :: size(7)>>,
         payload]
  end

  def frame({:pong, payload}, _) do
    len = :erlang.iolist_size(payload)
    true = len <= 125
    [<<1 :: size(1), 0 :: size(3), 10 :: size(4),
         0 :: size(1), len :: size(7)>>,
         payload]
  end

  def frame({:text, payload},
           %{deflate: deflate, deflate_takeover: takeOver})
      when deflate !== false do
    payload2 = deflate_frame(payload, deflate, takeOver)
    len = payload_length(payload2)
    [<<1 :: size(1), 1 :: size(1), 0 :: size(2),
         1 :: size(4), 0 :: size(1), len :: bits>>,
         payload2]
  end

  def frame({:binary, payload},
           %{deflate: deflate, deflate_takeover: takeOver})
      when deflate !== false do
    payload2 = deflate_frame(payload, deflate, takeOver)
    len = payload_length(payload2)
    [<<1 :: size(1), 1 :: size(1), 0 :: size(2),
         2 :: size(4), 0 :: size(1), len :: bits>>,
         payload2]
  end

  def frame({:text, payload}, _) do
    len = payload_length(payload)
    [<<1 :: size(1), 0 :: size(3), 1 :: size(4),
         0 :: size(1), len :: bits>>,
         payload]
  end

  def frame({:binary, payload}, _) do
    len = payload_length(payload)
    [<<1 :: size(1), 0 :: size(3), 2 :: size(4),
         0 :: size(1), len :: bits>>,
         payload]
  end

  def masked_frame(:close, _) do
    <<1 :: size(1), 0 :: size(3), 8 :: size(4),
        1 :: size(1), 0 :: size(39)>>
  end

  def masked_frame(:ping, _) do
    <<1 :: size(1), 0 :: size(3), 9 :: size(4),
        1 :: size(1), 0 :: size(39)>>
  end

  def masked_frame(:pong, _) do
    <<1 :: size(1), 0 :: size(3), 10 :: size(4),
        1 :: size(1), 0 :: size(39)>>
  end

  def masked_frame({:close, payload}, extensions) do
    frame({:close, 1000, payload}, extensions)
  end

  def masked_frame({:close, statusCode, payload}, _) do
    len = 2 + :erlang.iolist_size(payload)
    true = len <= 125
    maskKeyBin = (<<maskKey
                    ::
                    size(32)>> = :crypto.strong_rand_bytes(4))
    [<<1 :: size(1), 0 :: size(3), 8 :: size(4),
         1 :: size(1), len :: size(7)>>,
         maskKeyBin, mask(:erlang.iolist_to_binary([<<statusCode
                                                      ::
                                                      size(16)>>,
                                                        payload]),
                            maskKey, <<>>)]
  end

  def masked_frame({:ping, payload}, _) do
    len = :erlang.iolist_size(payload)
    true = len <= 125
    maskKeyBin = (<<maskKey
                    ::
                    size(32)>> = :crypto.strong_rand_bytes(4))
    [<<1 :: size(1), 0 :: size(3), 9 :: size(4),
         1 :: size(1), len :: size(7)>>,
         maskKeyBin, mask(:erlang.iolist_to_binary(payload),
                            maskKey, <<>>)]
  end

  def masked_frame({:pong, payload}, _) do
    len = :erlang.iolist_size(payload)
    true = len <= 125
    maskKeyBin = (<<maskKey
                    ::
                    size(32)>> = :crypto.strong_rand_bytes(4))
    [<<1 :: size(1), 0 :: size(3), 10 :: size(4),
         1 :: size(1), len :: size(7)>>,
         maskKeyBin, mask(:erlang.iolist_to_binary(payload),
                            maskKey, <<>>)]
  end

  def masked_frame({:text, payload},
           %{deflate: deflate, deflate_takeover: takeOver})
      when deflate !== false do
    maskKeyBin = (<<maskKey
                    ::
                    size(32)>> = :crypto.strong_rand_bytes(4))
    payload2 = mask(deflate_frame(payload, deflate,
                                    takeOver),
                      maskKey, <<>>)
    len = payload_length(payload2)
    [<<1 :: size(1), 1 :: size(1), 0 :: size(2),
         1 :: size(4), 1 :: size(1), len :: bits>>,
         maskKeyBin, payload2]
  end

  def masked_frame({:binary, payload},
           %{deflate: deflate, deflate_takeover: takeOver})
      when deflate !== false do
    maskKeyBin = (<<maskKey
                    ::
                    size(32)>> = :crypto.strong_rand_bytes(4))
    payload2 = mask(deflate_frame(payload, deflate,
                                    takeOver),
                      maskKey, <<>>)
    len = payload_length(payload2)
    [<<1 :: size(1), 1 :: size(1), 0 :: size(2),
         2 :: size(4), 1 :: size(1), len :: bits>>,
         maskKeyBin, payload2]
  end

  def masked_frame({:text, payload}, _) do
    maskKeyBin = (<<maskKey
                    ::
                    size(32)>> = :crypto.strong_rand_bytes(4))
    len = payload_length(payload)
    [<<1 :: size(1), 0 :: size(3), 1 :: size(4),
         1 :: size(1), len :: bits>>,
         maskKeyBin, mask(:erlang.iolist_to_binary(payload),
                            maskKey, <<>>)]
  end

  def masked_frame({:binary, payload}, _) do
    maskKeyBin = (<<maskKey
                    ::
                    size(32)>> = :crypto.strong_rand_bytes(4))
    len = payload_length(payload)
    [<<1 :: size(1), 0 :: size(3), 2 :: size(4),
         1 :: size(1), len :: bits>>,
         maskKeyBin, mask(:erlang.iolist_to_binary(payload),
                            maskKey, <<>>)]
  end

  defp payload_length(payload) do
    case (:erlang.iolist_size(payload)) do
      n when n <= 125 ->
        <<n :: size(7)>>
      n when n <= 65535 ->
        <<126 :: size(7), n :: size(16)>>
      n when n <= 9223372036854775807 ->
        <<127 :: size(7), n :: size(64)>>
    end
  end

  defp deflate_frame(payload, deflate, takeOver) do
    deflated = :erlang.iolist_to_binary(:zlib.deflate(deflate,
                                                        payload, :sync))
    case (takeOver) do
      :no_takeover ->
        :zlib.deflateReset(deflate)
      :takeover ->
        :ok
    end
    len = byte_size(deflated) - 4
    case (deflated) do
      <<body :: size(len) - binary, 0 :: size(8),
          0 :: size(8), 255 :: size(8), 255 :: size(8)>> ->
        body
      _ ->
        deflated
    end
  end

end