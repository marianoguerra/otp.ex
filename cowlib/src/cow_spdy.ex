defmodule :cow_spdy do
  use Bitwise

  def deflate_init() do
    zdef = :zlib.open()
    :ok = :zlib.deflateInit(zdef)

    _ =
      :zlib.deflateSetDictionary(
        zdef,
        <<0, 0, 0, 7, 111, 112, 116, 105, 111, 110, 115, 0, 0, 0, 4, 104, 101, 97, 100, 0, 0, 0,
          4, 112, 111, 115, 116, 0, 0, 0, 3, 112, 117, 116, 0, 0, 0, 6, 100, 101, 108, 101, 116,
          101, 0, 0, 0, 5, 116, 114, 97, 99, 101, 0, 0, 0, 6, 97, 99, 99, 101, 112, 116, 0, 0, 0,
          14, 97, 99, 99, 101, 112, 116, 45, 99, 104, 97, 114, 115, 101, 116, 0, 0, 0, 15, 97, 99,
          99, 101, 112, 116, 45, 101, 110, 99, 111, 100, 105, 110, 103, 0, 0, 0, 15, 97, 99, 99,
          101, 112, 116, 45, 108, 97, 110, 103, 117, 97, 103, 101, 0, 0, 0, 13, 97, 99, 99, 101,
          112, 116, 45, 114, 97, 110, 103, 101, 115, 0, 0, 0, 3, 97, 103, 101, 0, 0, 0, 5, 97,
          108, 108, 111, 119, 0, 0, 0, 13, 97, 117, 116, 104, 111, 114, 105, 122, 97, 116, 105,
          111, 110, 0, 0, 0, 13, 99, 97, 99, 104, 101, 45, 99, 111, 110, 116, 114, 111, 108, 0, 0,
          0, 10, 99, 111, 110, 110, 101, 99, 116, 105, 111, 110, 0, 0, 0, 12, 99, 111, 110, 116,
          101, 110, 116, 45, 98, 97, 115, 101, 0, 0, 0, 16, 99, 111, 110, 116, 101, 110, 116, 45,
          101, 110, 99, 111, 100, 105, 110, 103, 0, 0, 0, 16, 99, 111, 110, 116, 101, 110, 116,
          45, 108, 97, 110, 103, 117, 97, 103, 101, 0, 0, 0, 14, 99, 111, 110, 116, 101, 110, 116,
          45, 108, 101, 110, 103, 116, 104, 0, 0, 0, 16, 99, 111, 110, 116, 101, 110, 116, 45,
          108, 111, 99, 97, 116, 105, 111, 110, 0, 0, 0, 11, 99, 111, 110, 116, 101, 110, 116, 45,
          109, 100, 53, 0, 0, 0, 13, 99, 111, 110, 116, 101, 110, 116, 45, 114, 97, 110, 103, 101,
          0, 0, 0, 12, 99, 111, 110, 116, 101, 110, 116, 45, 116, 121, 112, 101, 0, 0, 0, 4, 100,
          97, 116, 101, 0, 0, 0, 4, 101, 116, 97, 103, 0, 0, 0, 6, 101, 120, 112, 101, 99, 116, 0,
          0, 0, 7, 101, 120, 112, 105, 114, 101, 115, 0, 0, 0, 4, 102, 114, 111, 109, 0, 0, 0, 4,
          104, 111, 115, 116, 0, 0, 0, 8, 105, 102, 45, 109, 97, 116, 99, 104, 0, 0, 0, 17, 105,
          102, 45, 109, 111, 100, 105, 102, 105, 101, 100, 45, 115, 105, 110, 99, 101, 0, 0, 0,
          13, 105, 102, 45, 110, 111, 110, 101, 45, 109, 97, 116, 99, 104, 0, 0, 0, 8, 105, 102,
          45, 114, 97, 110, 103, 101, 0, 0, 0, 19, 105, 102, 45, 117, 110, 109, 111, 100, 105,
          102, 105, 101, 100, 45, 115, 105, 110, 99, 101, 0, 0, 0, 13, 108, 97, 115, 116, 45, 109,
          111, 100, 105, 102, 105, 101, 100, 0, 0, 0, 8, 108, 111, 99, 97, 116, 105, 111, 110, 0,
          0, 0, 12, 109, 97, 120, 45, 102, 111, 114, 119, 97, 114, 100, 115, 0, 0, 0, 6, 112, 114,
          97, 103, 109, 97, 0, 0, 0, 18, 112, 114, 111, 120, 121, 45, 97, 117, 116, 104, 101, 110,
          116, 105, 99, 97, 116, 101, 0, 0, 0, 19, 112, 114, 111, 120, 121, 45, 97, 117, 116, 104,
          111, 114, 105, 122, 97, 116, 105, 111, 110, 0, 0, 0, 5, 114, 97, 110, 103, 101, 0, 0, 0,
          7, 114, 101, 102, 101, 114, 101, 114, 0, 0, 0, 11, 114, 101, 116, 114, 121, 45, 97, 102,
          116, 101, 114, 0, 0, 0, 6, 115, 101, 114, 118, 101, 114, 0, 0, 0, 2, 116, 101, 0, 0, 0,
          7, 116, 114, 97, 105, 108, 101, 114, 0, 0, 0, 17, 116, 114, 97, 110, 115, 102, 101, 114,
          45, 101, 110, 99, 111, 100, 105, 110, 103, 0, 0, 0, 7, 117, 112, 103, 114, 97, 100, 101,
          0, 0, 0, 10, 117, 115, 101, 114, 45, 97, 103, 101, 110, 116, 0, 0, 0, 4, 118, 97, 114,
          121, 0, 0, 0, 3, 118, 105, 97, 0, 0, 0, 7, 119, 97, 114, 110, 105, 110, 103, 0, 0, 0,
          16, 119, 119, 119, 45, 97, 117, 116, 104, 101, 110, 116, 105, 99, 97, 116, 101, 0, 0, 0,
          6, 109, 101, 116, 104, 111, 100, 0, 0, 0, 3, 103, 101, 116, 0, 0, 0, 6, 115, 116, 97,
          116, 117, 115, 0, 0, 0, 6, 50, 48, 48, 32, 79, 75, 0, 0, 0, 7, 118, 101, 114, 115, 105,
          111, 110, 0, 0, 0, 8, 72, 84, 84, 80, 47, 49, 46, 49, 0, 0, 0, 3, 117, 114, 108, 0, 0,
          0, 6, 112, 117, 98, 108, 105, 99, 0, 0, 0, 10, 115, 101, 116, 45, 99, 111, 111, 107,
          105, 101, 0, 0, 0, 10, 107, 101, 101, 112, 45, 97, 108, 105, 118, 101, 0, 0, 0, 6, 111,
          114, 105, 103, 105, 110, 49, 48, 48, 49, 48, 49, 50, 48, 49, 50, 48, 50, 50, 48, 53, 50,
          48, 54, 51, 48, 48, 51, 48, 50, 51, 48, 51, 51, 48, 52, 51, 48, 53, 51, 48, 54, 51, 48,
          55, 52, 48, 50, 52, 48, 53, 52, 48, 54, 52, 48, 55, 52, 48, 56, 52, 48, 57, 52, 49, 48,
          52, 49, 49, 52, 49, 50, 52, 49, 51, 52, 49, 52, 52, 49, 53, 52, 49, 54, 52, 49, 55, 53,
          48, 50, 53, 48, 52, 53, 48, 53, 50, 48, 51, 32, 78, 111, 110, 45, 65, 117, 116, 104,
          111, 114, 105, 116, 97, 116, 105, 118, 101, 32, 73, 110, 102, 111, 114, 109, 97, 116,
          105, 111, 110, 50, 48, 52, 32, 78, 111, 32, 67, 111, 110, 116, 101, 110, 116, 51, 48,
          49, 32, 77, 111, 118, 101, 100, 32, 80, 101, 114, 109, 97, 110, 101, 110, 116, 108, 121,
          52, 48, 48, 32, 66, 97, 100, 32, 82, 101, 113, 117, 101, 115, 116, 52, 48, 49, 32, 85,
          110, 97, 117, 116, 104, 111, 114, 105, 122, 101, 100, 52, 48, 51, 32, 70, 111, 114, 98,
          105, 100, 100, 101, 110, 52, 48, 52, 32, 78, 111, 116, 32, 70, 111, 117, 110, 100, 53,
          48, 48, 32, 73, 110, 116, 101, 114, 110, 97, 108, 32, 83, 101, 114, 118, 101, 114, 32,
          69, 114, 114, 111, 114, 53, 48, 49, 32, 78, 111, 116, 32, 73, 109, 112, 108, 101, 109,
          101, 110, 116, 101, 100, 53, 48, 51, 32, 83, 101, 114, 118, 105, 99, 101, 32, 85, 110,
          97, 118, 97, 105, 108, 97, 98, 108, 101, 74, 97, 110, 32, 70, 101, 98, 32, 77, 97, 114,
          32, 65, 112, 114, 32, 77, 97, 121, 32, 74, 117, 110, 32, 74, 117, 108, 32, 65, 117, 103,
          32, 83, 101, 112, 116, 32, 79, 99, 116, 32, 78, 111, 118, 32, 68, 101, 99, 32, 48, 48,
          58, 48, 48, 58, 48, 48, 32, 77, 111, 110, 44, 32, 84, 117, 101, 44, 32, 87, 101, 100,
          44, 32, 84, 104, 117, 44, 32, 70, 114, 105, 44, 32, 83, 97, 116, 44, 32, 83, 117, 110,
          44, 32, 71, 77, 84, 99, 104, 117, 110, 107, 101, 100, 44, 116, 101, 120, 116, 47, 104,
          116, 109, 108, 44, 105, 109, 97, 103, 101, 47, 112, 110, 103, 44, 105, 109, 97, 103,
          101, 47, 106, 112, 103, 44, 105, 109, 97, 103, 101, 47, 103, 105, 102, 44, 97, 112, 112,
          108, 105, 99, 97, 116, 105, 111, 110, 47, 120, 109, 108, 44, 97, 112, 112, 108, 105, 99,
          97, 116, 105, 111, 110, 47, 120, 104, 116, 109, 108, 43, 120, 109, 108, 44, 116, 101,
          120, 116, 47, 112, 108, 97, 105, 110, 44, 116, 101, 120, 116, 47, 106, 97, 118, 97, 115,
          99, 114, 105, 112, 116, 44, 112, 117, 98, 108, 105, 99, 112, 114, 105, 118, 97, 116,
          101, 109, 97, 120, 45, 97, 103, 101, 61, 103, 122, 105, 112, 44, 100, 101, 102, 108, 97,
          116, 101, 44, 115, 100, 99, 104, 99, 104, 97, 114, 115, 101, 116, 61, 117, 116, 102, 45,
          56, 99, 104, 97, 114, 115, 101, 116, 61, 105, 115, 111, 45, 56, 56, 53, 57, 45, 49, 44,
          117, 116, 102, 45, 44, 42, 44, 101, 110, 113, 61, 48, 46>>
      )

    zdef
  end

  def inflate_init() do
    zinf = :zlib.open()
    :ok = :zlib.inflateInit(zinf)
    zinf
  end

  def split(data = <<_::size(40), length::size(24), _::bits>>)
      when byte_size(data) >= length + 8 do
    length2 = length + 8
    <<frame::size(length2)-binary, rest::bits>> = data
    {true, frame, rest}
  end

  def split(_) do
    false
  end

  def parse(
        <<0::size(1), streamID::size(31), 0::size(7), isFinFlag::size(1), _::size(24),
          data::bits>>,
        _
      ) do
    {:data, streamID, from_flag(isFinFlag), data}
  end

  def parse(
        <<1::size(1), 3::size(15), 1::size(16), 0::size(6), isUnidirectionalFlag::size(1),
          isFinFlag::size(1), _::size(25), streamID::size(31), _::size(1),
          assocToStreamID::size(31), priority::size(3), _::size(5), 0::size(8), rest::bits>>,
        zinf
      ) do
    case parse_headers(rest, zinf) do
      {:ok, headers,
       [
         {":host", host},
         {":method", method},
         {":path", path},
         {":scheme", scheme},
         {":version", version}
       ]} ->
        {:syn_stream, streamID, assocToStreamID, from_flag(isFinFlag),
         from_flag(isUnidirectionalFlag), priority, method, scheme, host, path, version, headers}

      _ ->
        {:error, :badprotocol}
    end
  end

  def parse(
        <<1::size(1), 3::size(15), 2::size(16), 0::size(7), isFinFlag::size(1), _::size(25),
          streamID::size(31), rest::bits>>,
        zinf
      ) do
    case parse_headers(rest, zinf) do
      {:ok, headers, [{":status", status}, {":version", version}]} ->
        {:syn_reply, streamID, from_flag(isFinFlag), status, version, headers}

      _ ->
        {:error, :badprotocol}
    end
  end

  def parse(
        <<1::size(1), 3::size(15), 3::size(16), 0::size(8), _::size(56), statusCode::size(32)>>,
        _
      )
      when statusCode === 0 or statusCode > 11 do
    {:error, :badprotocol}
  end

  def parse(
        <<1::size(1), 3::size(15), 3::size(16), 0::size(8), _::size(25), streamID::size(31),
          statusCode::size(32)>>,
        _
      ) do
    status =
      case statusCode do
        1 ->
          :protocol_error

        2 ->
          :invalid_stream

        3 ->
          :refused_stream

        4 ->
          :unsupported_version

        5 ->
          :cancel

        6 ->
          :internal_error

        7 ->
          :flow_control_error

        8 ->
          :stream_in_use

        9 ->
          :stream_already_closed

        10 ->
          :invalid_credentials

        11 ->
          :frame_too_large
      end

    {:rst_stream, streamID, status}
  end

  def parse(
        <<1::size(1), 3::size(15), 4::size(16), 0::size(7), clearSettingsFlag::size(1),
          _::size(24), nbEntries::size(32), rest::bits>>,
        _
      ) do
    try do
      settings =
        for <<(<<is0::size(6), wasPersistedFlag::size(1), persistFlag::size(1), iD::size(24),
                 value::size(32)>> <- rest)>> do
          ^is0 = 0

          key =
            case iD do
              1 ->
                :upload_bandwidth

              2 ->
                :download_bandwidth

              3 ->
                :round_trip_time

              4 ->
                :max_concurrent_streams

              5 ->
                :current_cwnd

              6 ->
                :download_retrans_rate

              7 ->
                :initial_window_size

              8 ->
                :client_certificate_vector_size
            end

          {key, value, from_flag(persistFlag), from_flag(wasPersistedFlag)}
        end

      ^nbEntries = length(settings)
      {:settings, from_flag(clearSettingsFlag), settings}
    catch
      _, _ ->
        {:error, :badprotocol}
    end
  end

  def parse(
        <<1::size(1), 3::size(15), 6::size(16), 0::size(8), _::size(24), pingID::size(32)>>,
        _
      ) do
    {:ping, pingID}
  end

  def parse(
        <<1::size(1), 3::size(15), 7::size(16), 0::size(8), _::size(56), statusCode::size(32)>>,
        _
      )
      when statusCode > 2 do
    {:error, :badprotocol}
  end

  def parse(
        <<1::size(1), 3::size(15), 7::size(16), 0::size(8), _::size(25),
          lastGoodStreamID::size(31), statusCode::size(32)>>,
        _
      ) do
    status =
      case statusCode do
        0 ->
          :ok

        1 ->
          :protocol_error

        2 ->
          :internal_error
      end

    {:goaway, lastGoodStreamID, status}
  end

  def parse(
        <<1::size(1), 3::size(15), 8::size(16), 0::size(7), isFinFlag::size(1), _::size(25),
          streamID::size(31), rest::bits>>,
        zinf
      ) do
    case parse_headers(rest, zinf) do
      {:ok, headers, []} ->
        {:headers, streamID, from_flag(isFinFlag), headers}

      _ ->
        {:error, :badprotocol}
    end
  end

  def parse(
        <<1::size(1), 3::size(15), 9::size(16), 0::size(8), _::size(57), 0::size(31)>>,
        _
      ) do
    {:error, :badprotocol}
  end

  def parse(
        <<1::size(1), 3::size(15), 9::size(16), 0::size(8), _::size(25), streamID::size(31),
          _::size(1), deltaWindowSize::size(31)>>,
        _
      ) do
    {:window_update, streamID, deltaWindowSize}
  end

  def parse(_, _) do
    {:error, :badprotocol}
  end

  defp parse_headers(data, zinf) do
    [<<nbHeaders::size(32), rest::bits>>] = inflate(zinf, data)
    parse_headers(rest, nbHeaders, [], [])
  end

  defp parse_headers(<<>>, 0, headers, spHeaders) do
    {:ok, :lists.reverse(headers), :lists.sort(spHeaders)}
  end

  defp parse_headers(<<>>, _, _, _) do
    :error
  end

  defp parse_headers(_, 0, _, _) do
    :error
  end

  defp parse_headers(<<0::size(32), _::bits>>, _, _, _) do
    :error
  end

  defp parse_headers(
         <<l1::size(32), key::size(l1)-binary, l2::size(32), value::size(l2)-binary, rest::bits>>,
         nbHeaders,
         acc,
         spAcc
       ) do
    case key do
      <<?:, _::bits>> ->
        parse_headers(rest, nbHeaders - 1, acc, :lists.keystore(key, 1, spAcc, {key, value}))

      _ ->
        parse_headers(rest, nbHeaders - 1, [{key, value} | acc], spAcc)
    end
  end

  defp inflate(zinf, data) do
    try do
      :zlib.inflate(zinf, data)
    catch
      _, _ ->
        :ok =
          :zlib.inflateSetDictionary(
            zinf,
            <<0, 0, 0, 7, 111, 112, 116, 105, 111, 110, 115, 0, 0, 0, 4, 104, 101, 97, 100, 0, 0,
              0, 4, 112, 111, 115, 116, 0, 0, 0, 3, 112, 117, 116, 0, 0, 0, 6, 100, 101, 108, 101,
              116, 101, 0, 0, 0, 5, 116, 114, 97, 99, 101, 0, 0, 0, 6, 97, 99, 99, 101, 112, 116,
              0, 0, 0, 14, 97, 99, 99, 101, 112, 116, 45, 99, 104, 97, 114, 115, 101, 116, 0, 0,
              0, 15, 97, 99, 99, 101, 112, 116, 45, 101, 110, 99, 111, 100, 105, 110, 103, 0, 0,
              0, 15, 97, 99, 99, 101, 112, 116, 45, 108, 97, 110, 103, 117, 97, 103, 101, 0, 0, 0,
              13, 97, 99, 99, 101, 112, 116, 45, 114, 97, 110, 103, 101, 115, 0, 0, 0, 3, 97, 103,
              101, 0, 0, 0, 5, 97, 108, 108, 111, 119, 0, 0, 0, 13, 97, 117, 116, 104, 111, 114,
              105, 122, 97, 116, 105, 111, 110, 0, 0, 0, 13, 99, 97, 99, 104, 101, 45, 99, 111,
              110, 116, 114, 111, 108, 0, 0, 0, 10, 99, 111, 110, 110, 101, 99, 116, 105, 111,
              110, 0, 0, 0, 12, 99, 111, 110, 116, 101, 110, 116, 45, 98, 97, 115, 101, 0, 0, 0,
              16, 99, 111, 110, 116, 101, 110, 116, 45, 101, 110, 99, 111, 100, 105, 110, 103, 0,
              0, 0, 16, 99, 111, 110, 116, 101, 110, 116, 45, 108, 97, 110, 103, 117, 97, 103,
              101, 0, 0, 0, 14, 99, 111, 110, 116, 101, 110, 116, 45, 108, 101, 110, 103, 116,
              104, 0, 0, 0, 16, 99, 111, 110, 116, 101, 110, 116, 45, 108, 111, 99, 97, 116, 105,
              111, 110, 0, 0, 0, 11, 99, 111, 110, 116, 101, 110, 116, 45, 109, 100, 53, 0, 0, 0,
              13, 99, 111, 110, 116, 101, 110, 116, 45, 114, 97, 110, 103, 101, 0, 0, 0, 12, 99,
              111, 110, 116, 101, 110, 116, 45, 116, 121, 112, 101, 0, 0, 0, 4, 100, 97, 116, 101,
              0, 0, 0, 4, 101, 116, 97, 103, 0, 0, 0, 6, 101, 120, 112, 101, 99, 116, 0, 0, 0, 7,
              101, 120, 112, 105, 114, 101, 115, 0, 0, 0, 4, 102, 114, 111, 109, 0, 0, 0, 4, 104,
              111, 115, 116, 0, 0, 0, 8, 105, 102, 45, 109, 97, 116, 99, 104, 0, 0, 0, 17, 105,
              102, 45, 109, 111, 100, 105, 102, 105, 101, 100, 45, 115, 105, 110, 99, 101, 0, 0,
              0, 13, 105, 102, 45, 110, 111, 110, 101, 45, 109, 97, 116, 99, 104, 0, 0, 0, 8, 105,
              102, 45, 114, 97, 110, 103, 101, 0, 0, 0, 19, 105, 102, 45, 117, 110, 109, 111, 100,
              105, 102, 105, 101, 100, 45, 115, 105, 110, 99, 101, 0, 0, 0, 13, 108, 97, 115, 116,
              45, 109, 111, 100, 105, 102, 105, 101, 100, 0, 0, 0, 8, 108, 111, 99, 97, 116, 105,
              111, 110, 0, 0, 0, 12, 109, 97, 120, 45, 102, 111, 114, 119, 97, 114, 100, 115, 0,
              0, 0, 6, 112, 114, 97, 103, 109, 97, 0, 0, 0, 18, 112, 114, 111, 120, 121, 45, 97,
              117, 116, 104, 101, 110, 116, 105, 99, 97, 116, 101, 0, 0, 0, 19, 112, 114, 111,
              120, 121, 45, 97, 117, 116, 104, 111, 114, 105, 122, 97, 116, 105, 111, 110, 0, 0,
              0, 5, 114, 97, 110, 103, 101, 0, 0, 0, 7, 114, 101, 102, 101, 114, 101, 114, 0, 0,
              0, 11, 114, 101, 116, 114, 121, 45, 97, 102, 116, 101, 114, 0, 0, 0, 6, 115, 101,
              114, 118, 101, 114, 0, 0, 0, 2, 116, 101, 0, 0, 0, 7, 116, 114, 97, 105, 108, 101,
              114, 0, 0, 0, 17, 116, 114, 97, 110, 115, 102, 101, 114, 45, 101, 110, 99, 111, 100,
              105, 110, 103, 0, 0, 0, 7, 117, 112, 103, 114, 97, 100, 101, 0, 0, 0, 10, 117, 115,
              101, 114, 45, 97, 103, 101, 110, 116, 0, 0, 0, 4, 118, 97, 114, 121, 0, 0, 0, 3,
              118, 105, 97, 0, 0, 0, 7, 119, 97, 114, 110, 105, 110, 103, 0, 0, 0, 16, 119, 119,
              119, 45, 97, 117, 116, 104, 101, 110, 116, 105, 99, 97, 116, 101, 0, 0, 0, 6, 109,
              101, 116, 104, 111, 100, 0, 0, 0, 3, 103, 101, 116, 0, 0, 0, 6, 115, 116, 97, 116,
              117, 115, 0, 0, 0, 6, 50, 48, 48, 32, 79, 75, 0, 0, 0, 7, 118, 101, 114, 115, 105,
              111, 110, 0, 0, 0, 8, 72, 84, 84, 80, 47, 49, 46, 49, 0, 0, 0, 3, 117, 114, 108, 0,
              0, 0, 6, 112, 117, 98, 108, 105, 99, 0, 0, 0, 10, 115, 101, 116, 45, 99, 111, 111,
              107, 105, 101, 0, 0, 0, 10, 107, 101, 101, 112, 45, 97, 108, 105, 118, 101, 0, 0, 0,
              6, 111, 114, 105, 103, 105, 110, 49, 48, 48, 49, 48, 49, 50, 48, 49, 50, 48, 50, 50,
              48, 53, 50, 48, 54, 51, 48, 48, 51, 48, 50, 51, 48, 51, 51, 48, 52, 51, 48, 53, 51,
              48, 54, 51, 48, 55, 52, 48, 50, 52, 48, 53, 52, 48, 54, 52, 48, 55, 52, 48, 56, 52,
              48, 57, 52, 49, 48, 52, 49, 49, 52, 49, 50, 52, 49, 51, 52, 49, 52, 52, 49, 53, 52,
              49, 54, 52, 49, 55, 53, 48, 50, 53, 48, 52, 53, 48, 53, 50, 48, 51, 32, 78, 111,
              110, 45, 65, 117, 116, 104, 111, 114, 105, 116, 97, 116, 105, 118, 101, 32, 73, 110,
              102, 111, 114, 109, 97, 116, 105, 111, 110, 50, 48, 52, 32, 78, 111, 32, 67, 111,
              110, 116, 101, 110, 116, 51, 48, 49, 32, 77, 111, 118, 101, 100, 32, 80, 101, 114,
              109, 97, 110, 101, 110, 116, 108, 121, 52, 48, 48, 32, 66, 97, 100, 32, 82, 101,
              113, 117, 101, 115, 116, 52, 48, 49, 32, 85, 110, 97, 117, 116, 104, 111, 114, 105,
              122, 101, 100, 52, 48, 51, 32, 70, 111, 114, 98, 105, 100, 100, 101, 110, 52, 48,
              52, 32, 78, 111, 116, 32, 70, 111, 117, 110, 100, 53, 48, 48, 32, 73, 110, 116, 101,
              114, 110, 97, 108, 32, 83, 101, 114, 118, 101, 114, 32, 69, 114, 114, 111, 114, 53,
              48, 49, 32, 78, 111, 116, 32, 73, 109, 112, 108, 101, 109, 101, 110, 116, 101, 100,
              53, 48, 51, 32, 83, 101, 114, 118, 105, 99, 101, 32, 85, 110, 97, 118, 97, 105, 108,
              97, 98, 108, 101, 74, 97, 110, 32, 70, 101, 98, 32, 77, 97, 114, 32, 65, 112, 114,
              32, 77, 97, 121, 32, 74, 117, 110, 32, 74, 117, 108, 32, 65, 117, 103, 32, 83, 101,
              112, 116, 32, 79, 99, 116, 32, 78, 111, 118, 32, 68, 101, 99, 32, 48, 48, 58, 48,
              48, 58, 48, 48, 32, 77, 111, 110, 44, 32, 84, 117, 101, 44, 32, 87, 101, 100, 44,
              32, 84, 104, 117, 44, 32, 70, 114, 105, 44, 32, 83, 97, 116, 44, 32, 83, 117, 110,
              44, 32, 71, 77, 84, 99, 104, 117, 110, 107, 101, 100, 44, 116, 101, 120, 116, 47,
              104, 116, 109, 108, 44, 105, 109, 97, 103, 101, 47, 112, 110, 103, 44, 105, 109, 97,
              103, 101, 47, 106, 112, 103, 44, 105, 109, 97, 103, 101, 47, 103, 105, 102, 44, 97,
              112, 112, 108, 105, 99, 97, 116, 105, 111, 110, 47, 120, 109, 108, 44, 97, 112, 112,
              108, 105, 99, 97, 116, 105, 111, 110, 47, 120, 104, 116, 109, 108, 43, 120, 109,
              108, 44, 116, 101, 120, 116, 47, 112, 108, 97, 105, 110, 44, 116, 101, 120, 116, 47,
              106, 97, 118, 97, 115, 99, 114, 105, 112, 116, 44, 112, 117, 98, 108, 105, 99, 112,
              114, 105, 118, 97, 116, 101, 109, 97, 120, 45, 97, 103, 101, 61, 103, 122, 105, 112,
              44, 100, 101, 102, 108, 97, 116, 101, 44, 115, 100, 99, 104, 99, 104, 97, 114, 115,
              101, 116, 61, 117, 116, 102, 45, 56, 99, 104, 97, 114, 115, 101, 116, 61, 105, 115,
              111, 45, 56, 56, 53, 57, 45, 49, 44, 117, 116, 102, 45, 44, 42, 44, 101, 110, 113,
              61, 48, 46>>
          )

        :zlib.inflate(zinf, <<>>)
    end
  end

  defp from_flag(0) do
    false
  end

  defp from_flag(1) do
    true
  end

  def data(streamID, isFin, data) do
    isFinFlag = to_flag(isFin)
    length = :erlang.iolist_size(data)
    [<<0::size(1), streamID::size(31), 0::size(7), isFinFlag::size(1), length::size(24)>>, data]
  end

  def syn_stream(
        zdef,
        streamID,
        assocToStreamID,
        isFin,
        isUnidirectional,
        priority,
        method,
        scheme,
        host,
        path,
        version,
        headers
      ) do
    isFinFlag = to_flag(isFin)
    isUnidirectionalFlag = to_flag(isUnidirectional)

    headerBlock =
      build_headers(
        zdef,
        [
          [
            {":method", method},
            {":scheme", scheme},
            {":host", host},
            {":path", path},
            {":version", version}
          ]
          | headers
        ]
      )

    length = 10 + :erlang.iolist_size(headerBlock)

    [
      <<1::size(1), 3::size(15), 1::size(16), 0::size(6), isUnidirectionalFlag::size(1),
        isFinFlag::size(1), length::size(24), 0::size(1), streamID::size(31), 0::size(1),
        assocToStreamID::size(31), priority::size(3), 0::size(5), 0::size(8)>>,
      headerBlock
    ]
  end

  def syn_reply(zdef, streamID, isFin, status, version, headers) do
    isFinFlag = to_flag(isFin)

    headerBlock =
      build_headers(
        zdef,
        [[{":status", status}, {":version", version}] | headers]
      )

    length = 4 + :erlang.iolist_size(headerBlock)

    [
      <<1::size(1), 3::size(15), 2::size(16), 0::size(7), isFinFlag::size(1), length::size(24),
        0::size(1), streamID::size(31)>>,
      headerBlock
    ]
  end

  def rst_stream(streamID, status) do
    statusCode =
      case status do
        :protocol_error ->
          1

        :invalid_stream ->
          2

        :refused_stream ->
          3

        :unsupported_version ->
          4

        :cancel ->
          5

        :internal_error ->
          6

        :flow_control_error ->
          7

        :stream_in_use ->
          8

        :stream_already_closed ->
          9

        :invalid_credentials ->
          10

        :frame_too_large ->
          11
      end

    <<1::size(1), 3::size(15), 3::size(16), 0::size(8), 8::size(24), 0::size(1),
      streamID::size(31), statusCode::size(32)>>
  end

  def settings(clearSettingsFlag, settings) do
    isClearSettingsFlag = to_flag(clearSettingsFlag)
    nbEntries = length(settings)

    entries =
      for {key, value, wasPersistedFlag, persistFlag} <- settings do
        isWasPersistedFlag = to_flag(wasPersistedFlag)
        isPersistFlag = to_flag(persistFlag)

        iD =
          case key do
            :upload_bandwidth ->
              1

            :download_bandwidth ->
              2

            :round_trip_time ->
              3

            :max_concurrent_streams ->
              4

            :current_cwnd ->
              5

            :download_retrans_rate ->
              6

            :initial_window_size ->
              7

            :client_certificate_vector_size ->
              8
          end

        <<0::size(6), isWasPersistedFlag::size(1), isPersistFlag::size(1), iD::size(24),
          value::size(32)>>
      end

    length = 4 + :erlang.iolist_size(entries)

    [
      <<1::size(1), 3::size(15), 4::size(16), 0::size(7), isClearSettingsFlag::size(1),
        length::size(24), nbEntries::size(32)>>,
      entries
    ]
  end

  def ping(pingID) do
    <<1::size(1), 3::size(15), 6::size(16), 0::size(8), 4::size(24), pingID::size(32)>>
  end

  def goaway(lastGoodStreamID, status) do
    statusCode =
      case status do
        :ok ->
          0

        :protocol_error ->
          1

        :internal_error ->
          2
      end

    <<1::size(1), 3::size(15), 7::size(16), 0::size(8), 8::size(24), 0::size(1),
      lastGoodStreamID::size(31), statusCode::size(32)>>
  end

  defp build_headers(zdef, headers) do
    headers1 = merge_headers(:lists.sort(headers), [])
    nbHeaders = length(headers1)

    headers2 =
      for {key, value} <- headers1 do
        l1 = :erlang.iolist_size(key)
        l2 = :erlang.iolist_size(value)
        [<<l1::size(32)>>, key, <<l2::size(32)>>, value]
      end

    :zlib.deflate(zdef, [<<nbHeaders::size(32)>>, headers2], :full)
  end

  defp merge_headers([], acc) do
    :lists.reverse(acc)
  end

  defp merge_headers(
         [[{name, value1}, {name, value2}] | tail],
         acc
       ) do
    merge_headers([{name, [value1, 0, value2]} | tail], acc)
  end

  defp merge_headers([head | tail], acc) do
    merge_headers(tail, [head | acc])
  end

  defp to_flag(false) do
    0
  end

  defp to_flag(true) do
    1
  end
end
