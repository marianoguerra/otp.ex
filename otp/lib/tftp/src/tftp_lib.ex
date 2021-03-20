defmodule :m_tftp_lib do
  use Bitwise
  require Record

  Record.defrecord(:r_tftp_msg_req, :tftp_msg_req,
    access: :undefined,
    filename: :undefined,
    mode: :undefined,
    options: :undefined,
    local_filename: :undefined
  )

  Record.defrecord(:r_tftp_msg_data, :tftp_msg_data,
    block_no: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_tftp_msg_ack, :tftp_msg_ack, block_no: :undefined)

  Record.defrecord(:r_tftp_msg_error, :tftp_msg_error,
    code: :undefined,
    text: :undefined,
    details: :undefined
  )

  Record.defrecord(:r_tftp_msg_oack, :tftp_msg_oack, options: :undefined)

  Record.defrecord(:r_config, :config,
    parent_pid: self(),
    udp_socket: :undefined,
    udp_options: [:binary, {:reuseaddr, true}, {:active, :once}],
    udp_host: 'localhost',
    udp_port: 69,
    port_policy: :random,
    use_tsize: false,
    max_tsize: :infinity,
    max_conn: :infinity,
    rejected: [],
    polite_ack: false,
    debug_level: :none,
    timeout: :undefined,
    user_options: [],
    callbacks: [],
    logger: :tftp_logger,
    max_retries: 5
  )

  Record.defrecord(:r_callback, :callback,
    regexp: :undefined,
    internal: :undefined,
    module: :undefined,
    state: :undefined,
    block_no: :undefined,
    count: :undefined
  )

  def parse_config(options) do
    parse_config(options, r_config())
  end

  def parse_config(options, config) do
    do_parse_config(options, config)
  end

  defp do_parse_config([{key, val} | tail], config)
       when elem(config, 0) === :config do
    case key do
      :debug ->
        cond do
          val === 0 or val === :none ->
            do_parse_config(tail, r_config(config, debug_level: :none))

          val === 1 or val === :error ->
            do_parse_config(tail, r_config(config, debug_level: :error))

          val === 2 or val === :warning ->
            do_parse_config(tail, r_config(config, debug_level: :warning))

          val === 3 or val === :brief ->
            do_parse_config(tail, r_config(config, debug_level: :brief))

          val === 4 or val === :normal ->
            do_parse_config(tail, r_config(config, debug_level: :normal))

          val === 5 or val === :verbose ->
            do_parse_config(tail, r_config(config, debug_level: :verbose))

          val === 6 or val === :all ->
            do_parse_config(tail, r_config(config, debug_level: :all))

          true ->
            exit({:badarg, {key, val}})
        end

      :host ->
        cond do
          is_list(val) ->
            do_parse_config(tail, r_config(config, udp_host: val))

          is_tuple(val) and :erlang.size(val) === 4 ->
            do_parse_config(tail, r_config(config, udp_host: val))

          is_tuple(val) and :erlang.size(val) === 8 ->
            do_parse_config(tail, r_config(config, udp_host: val))

          true ->
            exit({:badarg, {key, val}})
        end

      :port ->
        cond do
          is_integer(val) and val >= 0 ->
            config2 =
              r_config(config,
                udp_port: val,
                udp_options: r_config(config, :udp_options)
              )

            do_parse_config(tail, config2)

          true ->
            exit({:badarg, {key, val}})
        end

      :port_policy ->
        case val do
          :random ->
            do_parse_config(tail, r_config(config, port_policy: val))

          0 ->
            do_parse_config(tail, r_config(config, port_policy: :random))

          minMax when is_integer(minMax) and minMax > 0 ->
            do_parse_config(
              tail,
              r_config(config, port_policy: {:range, minMax, minMax})
            )

          {:range, min, max}
          when max >= min and
                 is_integer(min) and min > 0 and
                 is_integer(max) and max > 0 ->
            do_parse_config(tail, r_config(config, port_policy: val))

          true ->
            exit({:badarg, {key, val}})
        end

      :udp when is_list(val) ->
        fun = fn
          {k, v}, list when k != :active ->
            replace_val(k, v, list)

          v, list when v != :list and v != :binary ->
            list ++ [v]

          v, _List ->
            exit({:badarg, {:udp, [v]}})
        end

        udpOptions = :lists.foldl(fun, r_config(config, :udp_options), val)

        do_parse_config(
          tail,
          r_config(config, udp_options: udpOptions)
        )

      :use_tsize ->
        case val do
          true ->
            do_parse_config(tail, r_config(config, use_tsize: val))

          false ->
            do_parse_config(tail, r_config(config, use_tsize: val))

          _ ->
            exit({:badarg, {key, val}})
        end

      :max_tsize ->
        cond do
          val === :infinity ->
            do_parse_config(tail, r_config(config, max_tsize: val))

          is_integer(val) and val >= 0 ->
            do_parse_config(tail, r_config(config, max_tsize: val))

          true ->
            exit({:badarg, {key, val}})
        end

      :max_conn ->
        cond do
          val === :infinity ->
            do_parse_config(tail, r_config(config, max_conn: val))

          is_integer(val) and val > 0 ->
            do_parse_config(tail, r_config(config, max_conn: val))

          true ->
            exit({:badarg, {key, val}})
        end

      _ when is_list(key) and is_list(val) ->
        key2 = to_lower(key)
        val2 = to_lower(val)
        tftpOptions = replace_val(key2, val2, r_config(config, :user_options))

        do_parse_config(
          tail,
          r_config(config, user_options: tftpOptions)
        )

      :reject ->
        case val do
          :read ->
            rejected = [val | r_config(config, :rejected)]
            do_parse_config(tail, r_config(config, rejected: rejected))

          :write ->
            rejected = [val | r_config(config, :rejected)]
            do_parse_config(tail, r_config(config, rejected: rejected))

          _ when is_list(val) ->
            rejected = [val | r_config(config, :rejected)]
            do_parse_config(tail, r_config(config, rejected: rejected))

          _ ->
            exit({:badarg, {key, val}})
        end

      :callback ->
        case val do
          {regExp, mod, state}
          when is_list(regExp) and
                 is_atom(mod) ->
            case :re.compile(regExp) do
              {:ok, internal} ->
                callback =
                  r_callback(regexp: regExp, internal: internal, module: mod, state: state)

                callbacks = r_config(config, :callbacks) ++ [callback]
                do_parse_config(tail, r_config(config, callbacks: callbacks))

              {:error, reason} ->
                exit({:badarg, {key, val}, reason})
            end

          _ ->
            exit({:badarg, {key, val}})
        end

      :logger ->
        cond do
          is_atom(val) ->
            do_parse_config(tail, r_config(config, logger: val))

          true ->
            exit({:badarg, {key, val}})
        end

      :max_retries ->
        cond do
          is_integer(val) and val > 0 ->
            do_parse_config(tail, r_config(config, max_retries: val))

          true ->
            exit({:badarg, {key, val}})
        end

      _ ->
        exit({:badarg, {key, val}})
    end
  end

  defp do_parse_config(
         [],
         r_config(
           udp_host: host,
           udp_options: udpOptions,
           user_options: userOptions,
           callbacks: callbacks
         ) = config
       ) do
    isInet6 = :lists.member(:inet6, udpOptions)
    isInet = :lists.member(:inet, udpOptions)

    host2 =
      cond do
        (isInet and not isInet6) or (not isInet and not isInet6) ->
          case :inet.getaddr(host, :inet) do
            {:ok, addr} ->
              addr

            {:error, reason} ->
              exit({:badarg, {:host, reason}})
          end

        isInet6 and not isInet ->
          case :inet.getaddr(host, :inet6) do
            {:ok, addr} ->
              addr

            {:error, reason} ->
              exit({:badarg, {:host, reason}})
          end

        true ->
          exit({:badarg, {:udp, [:inet]}})
      end

    udpOptions2 = :lists.reverse(udpOptions)
    tftpOptions = :lists.reverse(userOptions)
    callbacks2 = add_default_callbacks(callbacks)

    r_config(config,
      udp_host: host2,
      udp_options: udpOptions2,
      user_options: tftpOptions,
      callbacks: callbacks2
    )
  end

  defp do_parse_config(options, config)
       when elem(config, 0) === :config do
    exit({:badarg, options})
  end

  def add_default_callbacks(callbacks) do
    regExp = ''
    {:ok, internal} = :re.compile(regExp)
    file = r_callback(regexp: regExp, internal: internal, module: :tftp_file, state: [])
    bin = r_callback(regexp: regExp, internal: internal, module: :tftp_binary, state: [])
    callbacks ++ [file, bin]
  end

  def host_to_string(host) do
    case host do
      string when is_list(string) ->
        string

      {a1, a2, a3, a4} ->
        :lists.concat([a1, '.', a2, '.', a3, '.', a4])

      {a1, a2, a3, a4, a5, a6, a7, a8} ->
        :lists.concat([
          int16_to_hex(a1),
          '::',
          int16_to_hex(a2),
          '::',
          int16_to_hex(a3),
          '::',
          int16_to_hex(a4),
          '::',
          int16_to_hex(a5),
          '::',
          int16_to_hex(a6),
          '::',
          int16_to_hex(a7),
          '::',
          int16_to_hex(a8)
        ])
    end
  end

  defp int16_to_hex(0) do
    [?0]
  end

  defp int16_to_hex(i) do
    n1 = i >>> 8 &&& 255
    n2 = i &&& 255

    [
      code_character(div(n1, 16)),
      code_character(rem(n1, 16)),
      code_character(div(n2, 16)),
      code_character(rem(n2, 16))
    ]
  end

  defp code_character(n) when n < 10 do
    ?0 + n
  end

  defp code_character(n) do
    ?A + (n - 10)
  end

  def decode_msg(bin) when is_binary(bin) do
    case bin do
      <<1::size(16)-integer, tail::binary>> ->
        case decode_strings(
               tail,
               [:keep_case, :lower_case]
             ) do
          [filename, mode | strings] ->
            options = decode_options(strings)

            r_tftp_msg_req(
              access: :read,
              filename: filename,
              mode: to_lower(mode),
              options: options
            )

          [_Filename | _Strings] ->
            exit(r_tftp_msg_error(code: :undef, text: 'Missing mode'))

          _ ->
            exit(r_tftp_msg_error(code: :undef, text: 'Missing filename'))
        end

      <<2::size(16)-integer, tail::binary>> ->
        case decode_strings(
               tail,
               [:keep_case, :lower_case]
             ) do
          [filename, mode | strings] ->
            options = decode_options(strings)

            r_tftp_msg_req(
              access: :write,
              filename: filename,
              mode: to_lower(mode),
              options: options
            )

          [_Filename | _Strings] ->
            exit(r_tftp_msg_error(code: :undef, text: 'Missing mode'))

          _ ->
            exit(r_tftp_msg_error(code: :undef, text: 'Missing filename'))
        end

      <<3::size(16)-integer, seqNo::size(16)-integer, data::binary>> ->
        r_tftp_msg_data(block_no: seqNo, data: data)

      <<4::size(16)-integer, seqNo::size(16)-integer>> ->
        r_tftp_msg_ack(block_no: seqNo)

      <<5::size(16)-integer, errorCode::size(16)-integer, tail::binary>> ->
        case decode_strings(tail, [:keep_case]) do
          [errorText] ->
            errorCode2 = decode_error_code(errorCode)
            r_tftp_msg_error(code: errorCode2, text: errorText)

          _ ->
            exit(r_tftp_msg_error(code: :undef, text: 'Trailing garbage'))
        end

      <<6::size(16)-integer, tail::binary>> ->
        strings = decode_strings(tail, [:lower_case])
        options = decode_options(strings)
        r_tftp_msg_oack(options: options)

      _ ->
        exit(r_tftp_msg_error(code: :undef, text: 'Invalid syntax'))
    end
  end

  defp decode_strings(bin, cases)
       when is_binary(bin) and
              is_list(cases) do
    do_decode_strings(bin, cases, [])
  end

  defp do_decode_strings(<<>>, _Cases, strings) do
    :lists.reverse(strings)
  end

  defp do_decode_strings(bin, [case__ | cases], strings) do
    {string, tail} = decode_string(bin, case__, [])

    cond do
      cases === [] ->
        do_decode_strings(tail, [case__], [string | strings])

      true ->
        do_decode_strings(tail, cases, [string | strings])
    end
  end

  defp decode_string(<<char::size(8)-integer, tail::binary>>, case__, string) do
    cond do
      char === 0 ->
        {:lists.reverse(string), tail}

      case__ === :keep_case ->
        decode_string(tail, case__, [char | string])

      case__ === :lower_case ->
        char2 =
          cond do
            char >= ?A and char <= ?Z ->
              char - (?A - ?a)

            true ->
              char
          end

        decode_string(tail, case__, [char2 | string])
    end
  end

  defp decode_string(<<>>, _Case, _String) do
    exit(r_tftp_msg_error(code: :undef, text: 'Trailing null missing'))
  end

  defp decode_options([key, value | strings]) do
    [{to_lower(key), value} | decode_options(strings)]
  end

  defp decode_options([]) do
    []
  end

  defp decode_error_code(int) do
    case int do
      0 ->
        :undef

      1 ->
        :enoent

      2 ->
        :eacces

      3 ->
        :enospc

      4 ->
        :badop

      5 ->
        :badblk

      6 ->
        :eexist

      7 ->
        :baduser

      8 ->
        :badopt

      ^int
      when is_integer(int) and int >= 0 and
             int <= 65535 ->
        int

      _ ->
        exit(r_tftp_msg_error(code: :undef, text: 'Error code outside range.'))
    end
  end

  def encode_msg(r_tftp_msg_req(access: access, filename: filename, mode: mode, options: options)) do
    opCode =
      case access do
        :read ->
          1

        :write ->
          2
      end

    [
      <<opCode::size(16)-integer>>,
      filename,
      0,
      mode,
      0,
      for {key, val} <- options do
        [key, 0, val, 0]
      end
    ]
  end

  def encode_msg(r_tftp_msg_data(block_no: blockNo, data: data))
      when blockNo <= 65535 do
    [<<3::size(16)-integer, blockNo::size(16)-integer>>, data]
  end

  def encode_msg(r_tftp_msg_ack(block_no: blockNo)) when blockNo <= 65535 do
    <<4::size(16)-integer, blockNo::size(16)-integer>>
  end

  def encode_msg(r_tftp_msg_error(code: code, text: text)) do
    intCode = encode_error_code(code)
    [<<5::size(16)-integer, intCode::size(16)-integer>>, text, 0]
  end

  def encode_msg(r_tftp_msg_oack(options: options)) do
    [
      <<6::size(16)-integer>>,
      for {key, val} <- options do
        [key, 0, val, 0]
      end
    ]
  end

  defp encode_error_code(code) do
    case code do
      :undef ->
        0

      :enoent ->
        1

      :eacces ->
        2

      :enospc ->
        3

      :badop ->
        4

      :badblk ->
        5

      :eexist ->
        6

      :baduser ->
        7

      :badopt ->
        8

      int when is_integer(int) and int >= 0 and int <= 65535 ->
        int
    end
  end

  def replace_val(key, val, list) do
    case :lists.keysearch(key, 1, list) do
      false ->
        list ++ [{key, val}]

      {:value, {_, oldVal}} when oldVal === val ->
        list

      {:value, {_, _}} ->
        :lists.keyreplace(key, 1, list, {key, val})
    end
  end

  def to_lower(chars) do
    for char <- chars do
      cond do
        char >= ?A and char <= ?Z ->
          char - (?A - ?a)

        true ->
          char
      end
    end
  end
end
