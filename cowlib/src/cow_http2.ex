defmodule :cow_http2 do
  use Bitwise

  def parse_sequence(<<"PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n", rest::bits>>) do
    {:ok, rest}
  end

  def parse_sequence(data) when byte_size(data) >= 24 do
    {:connection_error, :protocol_error, :"The connection preface was invalid. (RFC7540 3.5)"}
  end

  def parse_sequence(data) do
    len = byte_size(data)
    <<preface::size(len)-binary, _::bits>> = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

    case data do
      ^preface ->
        :more

      _ ->
        {:connection_error, :protocol_error, :"The connection preface was invalid. (RFC7540 3.5)"}
    end
  end

  def parse(<<len::size(24), _::bits>>, maxFrameSize)
      when len > maxFrameSize do
    {:connection_error, :frame_size_error,
     :"The frame size exceeded SETTINGS_MAX_FRAME_SIZE. (RFC7540 4.2)"}
  end

  def parse(data, _) do
    parse(data)
  end

  def parse(<<_::size(24), 0::size(8), _::size(9), 0::size(31), _::bits>>) do
    {:connection_error, :protocol_error,
     :"DATA frames MUST be associated with a stream. (RFC7540 6.1)"}
  end

  def parse(<<0::size(24), 0::size(8), _::size(4), 1::size(1), _::size(35), _::bits>>) do
    {:connection_error, :frame_size_error,
     :"DATA frames with padding flag MUST have a length > 0. (RFC7540 6.1)"}
  end

  def parse(
        <<len0::size(24), 0::size(8), _::size(4), 1::size(1), _::size(35), padLen::size(8),
          _::bits>>
      )
      when padLen >= len0 do
    {:connection_error, :protocol_error,
     :"Length of padding MUST be less than length of payload. (RFC7540 6.1)"}
  end

  def parse(
        <<len::size(24), 0::size(8), _::size(4), 0::size(1), _::size(2), flagEndStream::size(1),
          _::size(1), streamID::size(31), data::size(len)-binary, rest::bits>>
      ) do
    {:ok, {:data, streamID, parse_fin(flagEndStream), data}, rest}
  end

  def parse(
        <<len0::size(24), 0::size(8), _::size(4), 1::size(1), _::size(2), flagEndStream::size(1),
          _::size(1), streamID::size(31), padLen::size(8), rest0::bits>>
      )
      when byte_size(rest0) >= len0 - 1 do
    len = len0 - padLen - 1

    case rest0 do
      <<data::size(len)-binary, 0::size(padLen)-unit(8), rest::bits>> ->
        {:ok, {:data, streamID, parse_fin(flagEndStream), data}, rest}

      _ ->
        {:connection_error, :protocol_error, :"Padding octets MUST be set to zero. (RFC7540 6.1)"}
    end
  end

  def parse(<<_::size(24), 1::size(8), _::size(9), 0::size(31), _::bits>>) do
    {:connection_error, :protocol_error,
     :"HEADERS frames MUST be associated with a stream. (RFC7540 6.2)"}
  end

  def parse(<<0::size(24), 1::size(8), _::size(4), 1::size(1), _::size(35), _::bits>>) do
    {:connection_error, :frame_size_error,
     :"HEADERS frames with padding flag MUST have a length > 0. (RFC7540 6.1)"}
  end

  def parse(<<len::size(24), 1::size(8), _::size(2), 1::size(1), _::size(37), _::bits>>)
      when len < 5 do
    {:connection_error, :frame_size_error,
     :"HEADERS frames with priority flag MUST have a length >= 5. (RFC7540 6.1)"}
  end

  def parse(
        <<len::size(24), 1::size(8), _::size(2), 1::size(1), _::size(1), 1::size(1), _::size(35),
          _::bits>>
      )
      when len < 6 do
    {:connection_error, :frame_size_error,
     :"HEADERS frames with padding and priority flags MUST have a length >= 6. (RFC7540 6.1)"}
  end

  def parse(
        <<len0::size(24), 1::size(8), _::size(4), 1::size(1), _::size(35), padLen::size(8),
          _::bits>>
      )
      when padLen >= len0 do
    {:connection_error, :protocol_error,
     :"Length of padding MUST be less than length of payload. (RFC7540 6.2)"}
  end

  def parse(
        <<len0::size(24), 1::size(8), _::size(2), 1::size(1), _::size(1), 1::size(1), _::size(35),
          padLen::size(8), _::bits>>
      )
      when padLen >= len0 - 5 do
    {:connection_error, :protocol_error,
     :"Length of padding MUST be less than length of payload. (RFC7540 6.2)"}
  end

  def parse(
        <<len::size(24), 1::size(8), _::size(2), 0::size(1), _::size(1), 0::size(1),
          flagEndHeaders::size(1), _::size(1), flagEndStream::size(1), _::size(1),
          streamID::size(31), headerBlockFragment::size(len)-binary, rest::bits>>
      ) do
    {:ok,
     {:headers, streamID, parse_fin(flagEndStream), parse_head_fin(flagEndHeaders),
      headerBlockFragment}, rest}
  end

  def parse(
        <<len0::size(24), 1::size(8), _::size(2), 0::size(1), _::size(1), 1::size(1),
          flagEndHeaders::size(1), _::size(1), flagEndStream::size(1), _::size(1),
          streamID::size(31), padLen::size(8), rest0::bits>>
      )
      when byte_size(rest0) >= len0 - 1 do
    len = len0 - padLen - 1

    case rest0 do
      <<headerBlockFragment::size(len)-binary, 0::size(padLen)-unit(8), rest::bits>> ->
        {:ok,
         {:headers, streamID, parse_fin(flagEndStream), parse_head_fin(flagEndHeaders),
          headerBlockFragment}, rest}

      _ ->
        {:connection_error, :protocol_error, :"Padding octets MUST be set to zero. (RFC7540 6.2)"}
    end
  end

  def parse(
        <<_::size(24), 1::size(8), _::size(2), 1::size(1), _::size(1), 0::size(1), _::size(4),
          streamID::size(31), _::size(1), streamID::size(31), _::bits>>
      ) do
    {:connection_error, :protocol_error,
     :"HEADERS frames cannot define a stream that depends on itself. (RFC7540 5.3.1)"}
  end

  def parse(
        <<len0::size(24), 1::size(8), _::size(2), 1::size(1), _::size(1), 0::size(1),
          flagEndHeaders::size(1), _::size(1), flagEndStream::size(1), _::size(1),
          streamID::size(31), e::size(1), depStreamID::size(31), weight::size(8), rest0::bits>>
      )
      when byte_size(rest0) >= len0 - 5 do
    len = len0 - 5
    <<headerBlockFragment::size(len)-binary, rest::bits>> = rest0

    {:ok,
     {:headers, streamID, parse_fin(flagEndStream), parse_head_fin(flagEndHeaders),
      parse_exclusive(e), depStreamID, weight + 1, headerBlockFragment}, rest}
  end

  def parse(
        <<_::size(24), 1::size(8), _::size(2), 1::size(1), _::size(1), 1::size(1), _::size(4),
          streamID::size(31), _::size(9), streamID::size(31), _::bits>>
      ) do
    {:connection_error, :protocol_error,
     :"HEADERS frames cannot define a stream that depends on itself. (RFC7540 5.3.1)"}
  end

  def parse(
        <<len0::size(24), 1::size(8), _::size(2), 1::size(1), _::size(1), 1::size(1),
          flagEndHeaders::size(1), _::size(1), flagEndStream::size(1), _::size(1),
          streamID::size(31), padLen::size(8), e::size(1), depStreamID::size(31), weight::size(8),
          rest0::bits>>
      )
      when byte_size(rest0) >= len0 - 6 do
    len = len0 - padLen - 6

    case rest0 do
      <<headerBlockFragment::size(len)-binary, 0::size(padLen)-unit(8), rest::bits>> ->
        {:ok,
         {:headers, streamID, parse_fin(flagEndStream), parse_head_fin(flagEndHeaders),
          parse_exclusive(e), depStreamID, weight + 1, headerBlockFragment}, rest}

      _ ->
        {:connection_error, :protocol_error, :"Padding octets MUST be set to zero. (RFC7540 6.2)"}
    end
  end

  def parse(<<5::size(24), 2::size(8), _::size(9), 0::size(31), _::bits>>) do
    {:connection_error, :protocol_error,
     :"PRIORITY frames MUST be associated with a stream. (RFC7540 6.3)"}
  end

  def parse(
        <<5::size(24), 2::size(8), _::size(9), streamID::size(31), _::size(1), streamID::size(31),
          _::size(8), rest::bits>>
      ) do
    {:stream_error, streamID, :protocol_error,
     :"PRIORITY frames cannot make a stream depend on itself. (RFC7540 5.3.1)", rest}
  end

  def parse(
        <<5::size(24), 2::size(8), _::size(9), streamID::size(31), e::size(1),
          depStreamID::size(31), weight::size(8), rest::bits>>
      ) do
    {:ok, {:priority, streamID, parse_exclusive(e), depStreamID, weight + 1}, rest}
  end

  def parse(
        <<badLen::size(24), 2::size(8), _::size(9), streamID::size(31), _::size(badLen)-binary,
          rest::bits>>
      ) do
    {:stream_error, streamID, :frame_size_error,
     :"PRIORITY frames MUST be 5 bytes wide. (RFC7540 6.3)", rest}
  end

  def parse(<<4::size(24), 3::size(8), _::size(9), 0::size(31), _::bits>>) do
    {:connection_error, :protocol_error,
     :"RST_STREAM frames MUST be associated with a stream. (RFC7540 6.4)"}
  end

  def parse(
        <<4::size(24), 3::size(8), _::size(9), streamID::size(31), errorCode::size(32),
          rest::bits>>
      ) do
    {:ok, {:rst_stream, streamID, parse_error_code(errorCode)}, rest}
  end

  def parse(<<_::size(24), 3::size(8), _::size(9), _::size(31), _::bits>>) do
    {:connection_error, :frame_size_error,
     :"RST_STREAM frames MUST be 4 bytes wide. (RFC7540 6.4)"}
  end

  def parse(
        <<0::size(24), 4::size(8), _::size(7), 1::size(1), _::size(1), 0::size(31), rest::bits>>
      ) do
    {:ok, :settings_ack, rest}
  end

  def parse(<<_::size(24), 4::size(8), _::size(7), 1::size(1), _::size(1), 0::size(31), _::bits>>) do
    {:connection_error, :frame_size_error,
     :"SETTINGS frames with the ACK flag set MUST have a length of 0. (RFC7540 6.5)"}
  end

  def parse(
        <<len::size(24), 4::size(8), _::size(7), 0::size(1), _::size(1), 0::size(31), _::bits>>
      )
      when rem(len, 6) !== 0 do
    {:connection_error, :frame_size_error,
     :"SETTINGS frames MUST have a length multiple of 6. (RFC7540 6.5)"}
  end

  def parse(
        <<len::size(24), 4::size(8), _::size(7), 0::size(1), _::size(1), 0::size(31), rest::bits>>
      )
      when byte_size(rest) >= len do
    parse_settings_payload(rest, len, %{})
  end

  def parse(<<_::size(24), 4::size(8), _::size(8), _::size(1), streamID::size(31), _::bits>>)
      when streamID !== 0 do
    {:connection_error, :protocol_error,
     :"SETTINGS frames MUST NOT be associated with a stream. (RFC7540 6.5)"}
  end

  def parse(<<len::size(24), 5::size(8), _::size(40), _::bits>>)
      when len < 4 do
    {:connection_error, :frame_size_error,
     :"PUSH_PROMISE frames MUST have a length >= 4. (RFC7540 4.2, RFC7540 6.6)"}
  end

  def parse(<<len::size(24), 5::size(8), _::size(4), 1::size(1), _::size(35), _::bits>>)
      when len < 5 do
    {:connection_error, :frame_size_error,
     :"PUSH_PROMISE frames with padding flag MUST have a length >= 5. (RFC7540 4.2, RFC7540 6.6)"}
  end

  def parse(<<_::size(24), 5::size(8), _::size(9), 0::size(31), _::bits>>) do
    {:connection_error, :protocol_error,
     :"PUSH_PROMISE frames MUST be associated with a stream. (RFC7540 6.6)"}
  end

  def parse(
        <<len0::size(24), 5::size(8), _::size(4), 1::size(1), _::size(35), padLen::size(8),
          _::bits>>
      )
      when padLen >= len0 - 4 do
    {:connection_error, :protocol_error,
     :"Length of padding MUST be less than length of payload. (RFC7540 6.6)"}
  end

  def parse(
        <<len0::size(24), 5::size(8), _::size(4), 0::size(1), flagEndHeaders::size(1), _::size(3),
          streamID::size(31), _::size(1), promisedStreamID::size(31), rest0::bits>>
      )
      when byte_size(rest0) >= len0 - 4 do
    len = len0 - 4
    <<headerBlockFragment::size(len)-binary, rest::bits>> = rest0

    {:ok,
     {:push_promise, streamID, parse_head_fin(flagEndHeaders), promisedStreamID,
      headerBlockFragment}, rest}
  end

  def parse(
        <<len0::size(24), 5::size(8), _::size(4), 1::size(1), flagEndHeaders::size(1), _::size(2),
          streamID::size(31), padLen::size(8), _::size(1), promisedStreamID::size(31),
          rest0::bits>>
      )
      when byte_size(rest0) >= len0 - 5 do
    len = len0 - 5

    case rest0 do
      <<headerBlockFragment::size(len)-binary, 0::size(padLen)-unit(8), rest::bits>> ->
        {:ok,
         {:push_promise, streamID, parse_head_fin(flagEndHeaders), promisedStreamID,
          headerBlockFragment}, rest}

      _ ->
        {:connection_error, :protocol_error, :"Padding octets MUST be set to zero. (RFC7540 6.6)"}
    end
  end

  def parse(
        <<8::size(24), 6::size(8), _::size(7), 1::size(1), _::size(1), 0::size(31),
          opaque::size(64), rest::bits>>
      ) do
    {:ok, {:ping_ack, opaque}, rest}
  end

  def parse(
        <<8::size(24), 6::size(8), _::size(7), 0::size(1), _::size(1), 0::size(31),
          opaque::size(64), rest::bits>>
      ) do
    {:ok, {:ping, opaque}, rest}
  end

  def parse(<<8::size(24), 6::size(8), _::size(104), _::bits>>) do
    {:connection_error, :protocol_error,
     :"PING frames MUST NOT be associated with a stream. (RFC7540 6.7)"}
  end

  def parse(<<len::size(24), 6::size(8), _::bits>>)
      when len !== 8 do
    {:connection_error, :frame_size_error, :"PING frames MUST be 8 bytes wide. (RFC7540 6.7)"}
  end

  def parse(
        <<len0::size(24), 7::size(8), _::size(9), 0::size(31), _::size(1), lastStreamID::size(31),
          errorCode::size(32), rest0::bits>>
      )
      when byte_size(rest0) >= len0 - 8 do
    len = len0 - 8
    <<debugData::size(len)-binary, rest::bits>> = rest0
    {:ok, {:goaway, lastStreamID, parse_error_code(errorCode), debugData}, rest}
  end

  def parse(<<len::size(24), 7::size(8), _::size(40), _::bits>>)
      when len < 8 do
    {:connection_error, :frame_size_error,
     :"GOAWAY frames MUST have a length >= 8. (RFC7540 4.2, RFC7540 6.8)"}
  end

  def parse(<<_::size(24), 7::size(8), _::size(40), _::bits>>) do
    {:connection_error, :protocol_error,
     :"GOAWAY frames MUST NOT be associated with a stream. (RFC7540 6.8)"}
  end

  def parse(
        <<4::size(24), 8::size(8), _::size(9), 0::size(31), _::size(1), 0::size(31), _::bits>>
      ) do
    {:connection_error, :protocol_error,
     :"WINDOW_UPDATE frames MUST have a non-zero increment. (RFC7540 6.9)"}
  end

  def parse(
        <<4::size(24), 8::size(8), _::size(9), 0::size(31), _::size(1), increment::size(31),
          rest::bits>>
      ) do
    {:ok, {:window_update, increment}, rest}
  end

  def parse(
        <<4::size(24), 8::size(8), _::size(9), streamID::size(31), _::size(1), 0::size(31),
          rest::bits>>
      ) do
    {:stream_error, streamID, :protocol_error,
     :"WINDOW_UPDATE frames MUST have a non-zero increment. (RFC7540 6.9)", rest}
  end

  def parse(
        <<4::size(24), 8::size(8), _::size(9), streamID::size(31), _::size(1),
          increment::size(31), rest::bits>>
      ) do
    {:ok, {:window_update, streamID, increment}, rest}
  end

  def parse(<<len::size(24), 8::size(8), _::bits>>)
      when len !== 4 do
    {:connection_error, :frame_size_error,
     :"WINDOW_UPDATE frames MUST be 4 bytes wide. (RFC7540 6.9)"}
  end

  def parse(<<_::size(24), 9::size(8), _::size(9), 0::size(31), _::bits>>) do
    {:connection_error, :protocol_error,
     :"CONTINUATION frames MUST be associated with a stream. (RFC7540 6.10)"}
  end

  def parse(
        <<len::size(24), 9::size(8), _::size(5), flagEndHeaders::size(1), _::size(3),
          streamID::size(31), headerBlockFragment::size(len)-binary, rest::bits>>
      ) do
    {:ok, {:continuation, streamID, parse_head_fin(flagEndHeaders), headerBlockFragment}, rest}
  end

  def parse(<<len::size(24), type::size(8), _::size(40), _::size(len)-binary, rest::bits>>)
      when type > 9 do
    {:ignore, rest}
  end

  def parse(_) do
    :more
  end

  defp parse_fin(0) do
    :nofin
  end

  defp parse_fin(1) do
    :fin
  end

  defp parse_head_fin(0) do
    :head_nofin
  end

  defp parse_head_fin(1) do
    :head_fin
  end

  defp parse_exclusive(0) do
    :shared
  end

  defp parse_exclusive(1) do
    :exclusive
  end

  defp parse_error_code(0) do
    :no_error
  end

  defp parse_error_code(1) do
    :protocol_error
  end

  defp parse_error_code(2) do
    :internal_error
  end

  defp parse_error_code(3) do
    :flow_control_error
  end

  defp parse_error_code(4) do
    :settings_timeout
  end

  defp parse_error_code(5) do
    :stream_closed
  end

  defp parse_error_code(6) do
    :frame_size_error
  end

  defp parse_error_code(7) do
    :refused_stream
  end

  defp parse_error_code(8) do
    :cancel
  end

  defp parse_error_code(9) do
    :compression_error
  end

  defp parse_error_code(10) do
    :connect_error
  end

  defp parse_error_code(11) do
    :enhance_your_calm
  end

  defp parse_error_code(12) do
    :inadequate_security
  end

  defp parse_error_code(13) do
    :http_1_1_required
  end

  defp parse_error_code(_) do
    :unknown_error
  end

  def parse_settings_payload(settingsPayload) do
    {:ok, {:settings, settings}, <<>>} =
      parse_settings_payload(settingsPayload, byte_size(settingsPayload), %{})

    settings
  end

  defp parse_settings_payload(rest, 0, settings) do
    {:ok, {:settings, settings}, rest}
  end

  defp parse_settings_payload(<<1::size(16), value::size(32), rest::bits>>, len, settings) do
    parse_settings_payload(rest, len - 6, %{settings | :header_table_size => value})
  end

  defp parse_settings_payload(<<2::size(16), 0::size(32), rest::bits>>, len, settings) do
    parse_settings_payload(rest, len - 6, %{settings | :enable_push => false})
  end

  defp parse_settings_payload(<<2::size(16), 1::size(32), rest::bits>>, len, settings) do
    parse_settings_payload(rest, len - 6, %{settings | :enable_push => true})
  end

  defp parse_settings_payload(<<2::size(16), _::size(32), _::bits>>, _, _) do
    {:connection_error, :protocol_error,
     :"The SETTINGS_ENABLE_PUSH value MUST be 0 or 1. (RFC7540 6.5.2)"}
  end

  defp parse_settings_payload(<<3::size(16), value::size(32), rest::bits>>, len, settings) do
    parse_settings_payload(rest, len - 6, %{settings | :max_concurrent_streams => value})
  end

  defp parse_settings_payload(<<4::size(16), value::size(32), _::bits>>, _, _)
       when value > 2_147_483_647 do
    {:connection_error, :flow_control_error,
     :"The maximum SETTINGS_INITIAL_WINDOW_SIZE value is 0x7fffffff. (RFC7540 6.5.2)"}
  end

  defp parse_settings_payload(<<4::size(16), value::size(32), rest::bits>>, len, settings) do
    parse_settings_payload(rest, len - 6, %{settings | :initial_window_size => value})
  end

  defp parse_settings_payload(<<5::size(16), value::size(32), _::bits>>, _, _)
       when value <= 16383 do
    {:connection_error, :protocol_error,
     :"The SETTINGS_MAX_FRAME_SIZE value must be > 0x3fff. (RFC7540 6.5.2)"}
  end

  defp parse_settings_payload(<<5::size(16), value::size(32), rest::bits>>, len, settings)
       when value <= 16_777_215 do
    parse_settings_payload(rest, len - 6, %{settings | :max_frame_size => value})
  end

  defp parse_settings_payload(<<5::size(16), _::size(32), _::bits>>, _, _) do
    {:connection_error, :protocol_error,
     :"The SETTINGS_MAX_FRAME_SIZE value must be =< 0xffffff. (RFC7540 6.5.2)"}
  end

  defp parse_settings_payload(<<6::size(16), value::size(32), rest::bits>>, len, settings) do
    parse_settings_payload(rest, len - 6, %{settings | :max_header_list_size => value})
  end

  defp parse_settings_payload(<<8::size(16), 0::size(32), rest::bits>>, len, settings) do
    parse_settings_payload(rest, len - 6, %{settings | :enable_connect_protocol => false})
  end

  defp parse_settings_payload(<<8::size(16), 1::size(32), rest::bits>>, len, settings) do
    parse_settings_payload(rest, len - 6, %{settings | :enable_connect_protocol => true})
  end

  defp parse_settings_payload(<<8::size(16), _::size(32), _::bits>>, _, _) do
    {:connection_error, :protocol_error,
     :"The SETTINGS_ENABLE_CONNECT_PROTOCOL value MUST be 0 or 1. (draft-h2-websockets-01 3)"}
  end

  defp parse_settings_payload(<<_::size(48), rest::bits>>, len, settings) do
    parse_settings_payload(rest, len - 6, settings)
  end

  def data(streamID, isFin, data) do
    [data_header(streamID, isFin, :erlang.iolist_size(data)), data]
  end

  def data_header(streamID, isFin, len) do
    flagEndStream = flag_fin(isFin)
    <<len::size(24), 0::size(15), flagEndStream::size(1), 0::size(1), streamID::size(31)>>
  end

  def headers(streamID, isFin, headerBlock) do
    len = :erlang.iolist_size(headerBlock)
    flagEndStream = flag_fin(isFin)
    flagEndHeaders = 1

    [
      <<len::size(24), 1::size(8), 0::size(5), flagEndHeaders::size(1), 0::size(1),
        flagEndStream::size(1), 0::size(1), streamID::size(31)>>,
      headerBlock
    ]
  end

  def priority(streamID, e, depStreamID, weight) do
    flagExclusive = exclusive(e)

    <<5::size(24), 2::size(8), 0::size(9), streamID::size(31), flagExclusive::size(1),
      depStreamID::size(31), weight::size(8)>>
  end

  def rst_stream(streamID, reason) do
    errorCode = error_code(reason)
    <<4::size(24), 3::size(8), 0::size(9), streamID::size(31), errorCode::size(32)>>
  end

  def settings(settings) do
    payload = settings_payload(settings)
    len = :erlang.iolist_size(payload)
    [<<len::size(24), 4::size(8), 0::size(40)>>, payload]
  end

  def settings_payload(settings) do
    for {key, value} <- :maps.to_list(settings) do
      case key do
        :header_table_size ->
          <<1::size(16), value::size(32)>>

        :enable_push when value ->
          <<2::size(16), 1::size(32)>>

        :enable_push ->
          <<2::size(16), 0::size(32)>>

        :max_concurrent_streams when value === :infinity ->
          <<>>

        :max_concurrent_streams ->
          <<3::size(16), value::size(32)>>

        :initial_window_size ->
          <<4::size(16), value::size(32)>>

        :max_frame_size ->
          <<5::size(16), value::size(32)>>

        :max_header_list_size when value === :infinity ->
          <<>>

        :max_header_list_size ->
          <<6::size(16), value::size(32)>>

        :enable_connect_protocol when value ->
          <<8::size(16), 1::size(32)>>

        :enable_connect_protocol ->
          <<8::size(16), 0::size(32)>>
      end
    end
  end

  def settings_ack() do
    <<0::size(24), 4::size(8), 1::size(8), 0::size(32)>>
  end

  def push_promise(streamID, promisedStreamID, headerBlock) do
    len = :erlang.iolist_size(headerBlock) + 4
    flagEndHeaders = 1

    [
      <<len::size(24), 5::size(8), 0::size(5), flagEndHeaders::size(1), 0::size(3),
        streamID::size(31), 0::size(1), promisedStreamID::size(31)>>,
      headerBlock
    ]
  end

  def ping(opaque) do
    <<8::size(24), 6::size(8), 0::size(40), opaque::size(64)>>
  end

  def ping_ack(opaque) do
    <<8::size(24), 6::size(8), 0::size(7), 1::size(1), 0::size(32), opaque::size(64)>>
  end

  def goaway(lastStreamID, reason, debugData) do
    errorCode = error_code(reason)
    len = :erlang.iolist_size(debugData) + 8

    [
      <<len::size(24), 7::size(8), 0::size(41), lastStreamID::size(31), errorCode::size(32)>>,
      debugData
    ]
  end

  def window_update(increment) do
    window_update(0, increment)
  end

  def window_update(streamID, increment)
      when increment <= 2_147_483_647 do
    <<4::size(24), 8::size(8), 0::size(8), streamID::size(32), 0::size(1), increment::size(31)>>
  end

  defp flag_fin(:nofin) do
    0
  end

  defp flag_fin(:fin) do
    1
  end

  defp exclusive(:shared) do
    0
  end

  defp exclusive(:exclusive) do
    1
  end

  defp error_code(:no_error) do
    0
  end

  defp error_code(:protocol_error) do
    1
  end

  defp error_code(:internal_error) do
    2
  end

  defp error_code(:flow_control_error) do
    3
  end

  defp error_code(:settings_timeout) do
    4
  end

  defp error_code(:stream_closed) do
    5
  end

  defp error_code(:frame_size_error) do
    6
  end

  defp error_code(:refused_stream) do
    7
  end

  defp error_code(:cancel) do
    8
  end

  defp error_code(:compression_error) do
    9
  end

  defp error_code(:connect_error) do
    10
  end

  defp error_code(:enhance_your_calm) do
    11
  end

  defp error_code(:inadequate_security) do
    12
  end

  defp error_code(:http_1_1_required) do
    13
  end
end
