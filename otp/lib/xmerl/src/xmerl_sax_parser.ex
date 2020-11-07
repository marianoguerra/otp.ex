defmodule :m_xmerl_sax_parser do
  use Bitwise
  require Record

  Record.defrecord(:r_xmerl_sax_parser_state, :xmerl_sax_parser_state,
    event_state: :undefined,
    event_fun: :undefined,
    continuation_state: :undefined,
    continuation_fun: :undefined,
    encoding: :utf8,
    line_no: 1,
    ns: [],
    current_tag: [],
    end_tags: [],
    match_end_tags: true,
    ref_table: :undefined,
    standalone: :no,
    file_type: :normal,
    current_location: :undefined,
    entity: :undefined,
    skip_external_dtd: false,
    input_type: :undefined,
    attribute_values: []
  )

  def file(name, options) do
    case :file.open(
           name,
           [:raw, :read_ahead, :read, :binary]
         ) do
      {:error, reason} ->
        {:error, {name, :file.format_error(reason)}}

      {:ok, fD} ->
        dir = :filename.dirname(name)
        cL = :filename.absname(dir)
        file = :filename.basename(name)
        continuationFun = &default_continuation_cb/1

        res =
          stream(
            <<>>,
            [
              [
                {:continuation_fun, continuationFun},
                {:continuation_state, fD},
                {:current_location, cL},
                {:entity, file}
              ]
              | options
            ],
            :file
          )

        :ok = :file.close(fD)
        res
    end
  end

  def stream(xml, options) do
    stream(xml, options, :stream)
  end

  def stream(xml, options, inputType)
      when is_list(xml) and
             is_list(options) do
    state = parse_options(options, initial_state())

    case r_xmerl_sax_parser_state(state, :file_type) do
      :dtd ->
        :xmerl_sax_parser_list.parse_dtd(
          xml,
          r_xmerl_sax_parser_state(state,
            encoding: :list,
            input_type: inputType
          )
        )

      :normal ->
        :xmerl_sax_parser_list.parse(
          xml,
          r_xmerl_sax_parser_state(state,
            encoding: :list,
            input_type: inputType
          )
        )
    end
  end

  def stream(xml, options, inputType)
      when is_binary(xml) and
             is_list(options) do
    case parse_options(options, initial_state()) do
      {:error, reason} ->
        {:error, reason}

      state ->
        parseFunction =
          case r_xmerl_sax_parser_state(state, :file_type) do
            :dtd ->
              :parse_dtd

            :normal ->
              :parse
          end

        try do
          {xml1, state1} = detect_charset(xml, state)

          parse_binary(
            xml1,
            r_xmerl_sax_parser_state(state1, input_type: inputType),
            parseFunction
          )
        catch
          {:fatal_error, {state2, reason}} ->
            {:fatal_error,
             {r_xmerl_sax_parser_state(state2, :current_location),
              r_xmerl_sax_parser_state(state2, :entity), 1}, reason, [],
             r_xmerl_sax_parser_state(state2, :event_state)}
        end
    end
  end

  defp parse_binary(xml, r_xmerl_sax_parser_state(encoding: :utf8) = state, f) do
    apply(:xmerl_sax_parser_utf8, f, [xml, state])
  end

  defp parse_binary(xml, r_xmerl_sax_parser_state(encoding: {:utf16, :little}) = state, f) do
    apply(:xmerl_sax_parser_utf16le, f, [xml, state])
  end

  defp parse_binary(xml, r_xmerl_sax_parser_state(encoding: {:utf16, :big}) = state, f) do
    apply(:xmerl_sax_parser_utf16be, f, [xml, state])
  end

  defp parse_binary(xml, r_xmerl_sax_parser_state(encoding: :latin1) = state, f) do
    apply(:xmerl_sax_parser_latin1, f, [xml, state])
  end

  defp parse_binary(_, r_xmerl_sax_parser_state(encoding: enc), state) do
    throw(
      {:fatal_error,
       {state, :lists.flatten(:io_lib.format('Charcter set ~p not supported', [enc]))}}
    )
  end

  defp initial_state() do
    r_xmerl_sax_parser_state(
      event_fun: &default_event_cb/3,
      ns: [{'xml', 'http://www.w3.org/XML/1998/namespace'}],
      current_location: '.',
      entity: ''
    )
  end

  defp parse_options([], state) do
    state
  end

  defp parse_options([{:event_state, cbState} | options], state) do
    parse_options(options, r_xmerl_sax_parser_state(state, event_state: cbState))
  end

  defp parse_options([{:event_fun, cbF} | options], state) do
    parse_options(options, r_xmerl_sax_parser_state(state, event_fun: cbF))
  end

  defp parse_options(
         [{:continuation_state, cState} | options],
         state
       ) do
    parse_options(
      options,
      r_xmerl_sax_parser_state(state, continuation_state: cState)
    )
  end

  defp parse_options([{:continuation_fun, cF} | options], state) do
    parse_options(options, r_xmerl_sax_parser_state(state, continuation_fun: cF))
  end

  defp parse_options([{:file_type, fT} | options], state)
       when fT == :normal or fT == :dtd do
    parse_options(options, r_xmerl_sax_parser_state(state, file_type: fT))
  end

  defp parse_options([{:encoding, e} | options], state) do
    case check_encoding_option(e) do
      {:error, reason} ->
        {:error, reason}

      enc ->
        parse_options(options, r_xmerl_sax_parser_state(state, encoding: enc))
    end
  end

  defp parse_options([{:current_location, cL} | options], state) do
    parse_options(options, r_xmerl_sax_parser_state(state, current_location: cL))
  end

  defp parse_options([{:entity, entity} | options], state) do
    parse_options(options, r_xmerl_sax_parser_state(state, entity: entity))
  end

  defp parse_options([:skip_external_dtd | options], state) do
    parse_options(
      options,
      r_xmerl_sax_parser_state(state, skip_external_dtd: true)
    )
  end

  defp parse_options([o | _], _State) do
    {:error, :lists.flatten(:io_lib.format('Option: ~p not supported', [o]))}
  end

  defp check_encoding_option(e)
       when e == :utf8 or e == {:utf16, :little} or
              e == {:utf16, :big} or e == :latin1 or e == :list do
    e
  end

  defp check_encoding_option(:utf16) do
    {:utf16, :big}
  end

  defp check_encoding_option(e) do
    {:error, :io_lib.format('Character set ~p not supported', [e])}
  end

  defp detect_charset(
         <<>>,
         r_xmerl_sax_parser_state(continuation_fun: :undefined) = state
       ) do
    throw({:fatal_error, {state, 'Can\'t detect character encoding due to lack of indata'}})
  end

  defp detect_charset(<<>>, state) do
    cf(<<>>, state, &detect_charset/2)
  end

  defp detect_charset(bytes, state) do
    case :unicode.bom_to_encoding(bytes) do
      {:latin1, 0} ->
        detect_charset_1(bytes, state)

      {enc, length} ->
        <<_::size(length)-binary, realBytes::binary>> = bytes
        {realBytes, r_xmerl_sax_parser_state(state, encoding: enc)}
    end
  end

  defp detect_charset_1(<<0>> = xml, state) do
    cf(xml, state, &detect_charset_1/2)
  end

  defp detect_charset_1(<<0, 60>> = xml, state) do
    cf(xml, state, &detect_charset_1/2)
  end

  defp detect_charset_1(<<0, 60, 0>> = xml, state) do
    cf(xml, state, &detect_charset_1/2)
  end

  defp detect_charset_1(<<0, 60, 0, 63, _::binary>> = xml, state) do
    {xml, r_xmerl_sax_parser_state(state, encoding: {:utf16, :big})}
  end

  defp detect_charset_1(<<60>> = xml, state) do
    cf(xml, state, &detect_charset_1/2)
  end

  defp detect_charset_1(<<60, 0>> = xml, state) do
    cf(xml, state, &detect_charset_1/2)
  end

  defp detect_charset_1(<<60, 0, 63>> = xml, state) do
    cf(xml, state, &detect_charset_1/2)
  end

  defp detect_charset_1(<<60, 0, 63, 0, _::binary>> = xml, state) do
    {xml, r_xmerl_sax_parser_state(state, encoding: {:utf16, :little})}
  end

  defp detect_charset_1(<<60>> = xml, state) do
    cf(xml, state, &detect_charset_1/2)
  end

  defp detect_charset_1(<<60, 63>> = xml, state) do
    cf(xml, state, &detect_charset_1/2)
  end

  defp detect_charset_1(<<60, 63, 120>> = xml, state) do
    cf(xml, state, &detect_charset_1/2)
  end

  defp detect_charset_1(<<60, 63, 120, 109>> = xml, state) do
    cf(xml, state, &detect_charset_1/2)
  end

  defp detect_charset_1(
         <<60, 63, 120, 109, 108, xml2::binary>>,
         state
       ) do
    {xml3, state1} =
      read_until_end_of_xml_directive(
        xml2,
        state
      )

    attrList = parse_xml_directive(xml3, state)

    case :lists.keysearch('encoding', 1, attrList) do
      {:value, {_, e}} ->
        enc = convert_encoding(e, state)
        {<<60, 63, 120, 109, 108, xml3::binary>>, r_xmerl_sax_parser_state(state1, encoding: enc)}

      _ ->
        {<<60, 63, 120, 109, 108, xml3::binary>>, state1}
    end
  end

  defp detect_charset_1(xml, state) do
    {xml, state}
  end

  defp convert_encoding(enc, state) do
    case :string.to_lower(enc) do
      'utf-8' ->
        :utf8

      'us-ascii' ->
        :utf8

      'latin1' ->
        :latin1

      'iso-8859-1' ->
        :latin1

      'iso-8859-2' ->
        :latin1

      'iso-8859-3' ->
        :latin1

      'iso-8859-4' ->
        :latin1

      'iso-8859-5' ->
        :latin1

      'iso-8859-6' ->
        :latin1

      'iso-8859-7' ->
        :latin1

      'iso-8859-8' ->
        :latin1

      'iso-8859-9' ->
        :latin1

      _ ->
        throw({:fatal_error, {state, 'Unknown encoding: ' ++ enc}})
    end
  end

  defp parse_xml_directive(<<c, rest::binary>>, state)
       when c === 32 or
              c === 13 or c === 10 or
              c === 9 do
    parse_xml_directive_1(rest, [], state)
  end

  defp parse_xml_directive(_, state) do
    throw({:fatal_error, {state, 'Expected whitespace in directive'}})
  end

  defp parse_xml_directive_1(<<c, rest::binary>>, acc, state)
       when c === 32 or c === 13 or c === 10 or c === 9 do
    parse_xml_directive_1(rest, acc, state)
  end

  defp parse_xml_directive_1(<<"?>", _::binary>>, acc, _State) do
    acc
  end

  defp parse_xml_directive_1(<<c, rest::binary>>, acc, state)
       when 97 <= c and c <= 122 do
    {name, rest1} = parse_name(rest, [c])
    rest2 = parse_eq(rest1, state)
    {value, rest3} = parse_value(rest2, state)
    parse_xml_directive_1(rest3, [{name, value} | acc], state)
  end

  defp parse_xml_directive_1(_, _, state) do
    throw({:fatal_error, {state, 'Unknown attribute in xml directive'}})
  end

  defp parse_name(<<c, rest::binary>>, acc)
       when 97 <= c and
              c <= 122 do
    parse_name(rest, [c | acc])
  end

  defp parse_name(rest, acc) do
    {:lists.reverse(acc), rest}
  end

  defp parse_eq(<<c, rest::binary>>, state)
       when c === 32 or
              c === 13 or c === 10 or
              c === 9 do
    parse_eq(rest, state)
  end

  defp parse_eq(<<"=", rest::binary>>, _State) do
    rest
  end

  defp parse_eq(_, state) do
    throw({:fatal_error, {state, 'expecting = or whitespace'}})
  end

  defp parse_value(<<c, rest::binary>>, state)
       when c === 32 or
              c === 13 or c === 10 or
              c === 9 do
    parse_value(rest, state)
  end

  defp parse_value(<<c, rest::binary>>, state)
       when c == ?' or
              c == ?" do
    parse_value_1(rest, c, [], state)
  end

  defp parse_value(_, state) do
    throw({:fatal_error, {state, '\', " or whitespace expected'}})
  end

  defp parse_value_1(<<stop, rest::binary>>, stop, acc, _State) do
    {:lists.reverse(acc), rest}
  end

  defp parse_value_1(<<c, rest::binary>>, stop, acc, state) do
    parse_value_1(rest, stop, [c | acc], state)
  end

  defp parse_value_1(_, _Stop, _Acc, state) do
    throw({:fatal_error, {state, 'end of input and no \' or " found'}})
  end

  defp default_event_cb(_Event, _LineNo, state) do
    state
  end

  def default_continuation_cb(ioDevice) do
    case :file.read(ioDevice, 1024) do
      :eof ->
        {<<>>, ioDevice}

      {:ok, fileBin} ->
        {fileBin, ioDevice}
    end
  end

  defp read_until_end_of_xml_directive(rest, state) do
    case :binary.match(rest, "?>") do
      :nomatch ->
        case cf(rest, state) do
          {<<>>, _} ->
            throw(
              {:fatal_error, {state, 'Can\'t detect character encoding due to lack of indata'}}
            )

          {newBytes, newState} ->
            read_until_end_of_xml_directive(newBytes, newState)
        end

      _ ->
        {rest, state}
    end
  end

  defp cf(
         _Rest,
         r_xmerl_sax_parser_state(continuation_fun: :undefined) = state
       ) do
    throw({:fatal_error, {state, 'Continuation function undefined'}})
  end

  defp cf(
         rest,
         r_xmerl_sax_parser_state(
           continuation_fun: cFun,
           continuation_state: cState
         ) = state
       ) do
    result =
      try do
        cFun.(cState)
      catch
        errorTerm ->
          throw({:fatal_error, {state, errorTerm}})

        :exit, reason ->
          throw({:fatal_error, {state, {:EXIT, reason}}})
      end

    case result do
      {<<>>, _} ->
        throw({:fatal_error, {state, 'Can\'t detect character encoding due to lack of indata'}})

      {newBytes, newContState} ->
        {<<rest::binary, newBytes::binary>>,
         r_xmerl_sax_parser_state(state, continuation_state: newContState)}
    end
  end

  defp cf(_Rest, r_xmerl_sax_parser_state(continuation_fun: :undefined) = state, _) do
    throw({:fatal_error, {state, 'Continuation function undefined'}})
  end

  defp cf(
         rest,
         r_xmerl_sax_parser_state(
           continuation_fun: cFun,
           continuation_state: cState
         ) = state,
         nextCall
       ) do
    result =
      try do
        cFun.(cState)
      catch
        errorTerm ->
          throw({:fatal_error, {state, errorTerm}})

        :exit, reason ->
          throw({:fatal_error, {state, {:EXIT, reason}}})
      end

    case result do
      {<<>>, _} ->
        throw({:fatal_error, {state, 'Can\'t detect character encoding due to lack of indata'}})

      {newBytes, newContState} ->
        nextCall.(
          <<rest::binary, newBytes::binary>>,
          r_xmerl_sax_parser_state(state, continuation_state: newContState)
        )
    end
  end
end
