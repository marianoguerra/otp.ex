defmodule :m_logger_formatter do
  use Bitwise

  def format(%{level: level, msg: msg0, meta: meta}, config0)
      when is_map(config0) do
    config = add_default_config(config0)
    meta1 = maybe_add_legacy_header(level, meta, config)
    template = :maps.get(:template, config)

    {bT, aT0} =
      :lists.splitwith(
        fn
          :msg ->
            false

          _ ->
            true
        end,
        template
      )

    {doMsg, aT} =
      case aT0 do
        [:msg | rest] ->
          {true, rest}

        _ ->
          {false, aT0}
      end

    b = do_format(level, meta1, bT, config)
    a = do_format(level, meta1, aT, config)

    msgStr =
      cond do
        doMsg ->
          config1 =
            case :maps.get(:chars_limit, config) do
              :unlimited ->
                config

              size0 ->
                size =
                  case size0 - :io_lib.chars_length([b, a]) do
                    s when s >= 0 ->
                      s

                    _ ->
                      0
                  end

                Map.put(config, :chars_limit, size)
            end

          msgStr0 = format_msg(msg0, meta1, config1)

          case :maps.get(:single_line, config) do
            true ->
              t =
                :lists.reverse(
                  trim(
                    :lists.reverse(
                      trim(
                        msgStr0,
                        false
                      )
                    ),
                    true
                  )
                )

              :re.replace(t, ',?\r?\n *', ', ', [{:return, :list}, :global, :unicode])

            _false ->
              msgStr0
          end

        true ->
          ''
      end

    truncate(b, msgStr, a, :maps.get(:max_size, config))
  end

  defp trim([h | t], rev)
       when h == ?\s or h == ?\r or
              h == ?\n do
    trim(t, rev)
  end

  defp trim([h | t], false) when is_list(h) do
    case trim(h, false) do
      [] ->
        trim(t, false)

      trimmedH ->
        [trimmedH | t]
    end
  end

  defp trim([h | t], true) when is_list(h) do
    case trim(:lists.reverse(h), true) do
      [] ->
        trim(t, true)

      trimmedH ->
        [:lists.reverse(trimmedH) | t]
    end
  end

  defp trim(string, _) do
    string
  end

  defp do_format(level, data, [:level | format], config) do
    [to_string(:level, level, config) | do_format(level, data, format, config)]
  end

  defp do_format(level, data, [{key, ifExist, else__} | format], config) do
    string =
      case value(key, data) do
        {:ok, value} ->
          do_format(level, Map.put(data, key, value), ifExist, config)

        :error ->
          do_format(level, data, else__, config)
      end

    [string | do_format(level, data, format, config)]
  end

  defp do_format(level, data, [key | format], config)
       when is_atom(key) or (is_list(key) and is_atom(hd(key))) do
    string =
      case value(key, data) do
        {:ok, value} ->
          to_string(key, value, config)

        :error ->
          ''
      end

    [string | do_format(level, data, format, config)]
  end

  defp do_format(level, data, [str | format], config) do
    [str | do_format(level, data, format, config)]
  end

  defp do_format(_Level, _Data, [], _Config) do
    []
  end

  defp value(key, meta) when :erlang.is_map_key(key, meta) do
    {:ok, :maps.get(key, meta)}
  end

  defp value([key | keys], meta)
       when :erlang.is_map_key(
              key,
              meta
            ) do
    value(keys, :maps.get(key, meta))
  end

  defp value([], value) do
    {:ok, value}
  end

  defp value(_, _) do
    :error
  end

  defp to_string(:time, time, config) do
    format_time(time, config)
  end

  defp to_string(:mfa, mFA, config) do
    format_mfa(mFA, config)
  end

  defp to_string(_, value, config) do
    to_string(value, config)
  end

  defp to_string(x, _) when is_atom(x) do
    :erlang.atom_to_list(x)
  end

  defp to_string(x, _) when is_integer(x) do
    :erlang.integer_to_list(x)
  end

  defp to_string(x, _) when is_pid(x) do
    :erlang.pid_to_list(x)
  end

  defp to_string(x, _) when is_reference(x) do
    :erlang.ref_to_list(x)
  end

  defp to_string(x, config) when is_list(x) do
    case printable_list(:lists.flatten(x)) do
      true ->
        x

      _ ->
        :io_lib.format(p(config), [x])
    end
  end

  defp to_string(x, config) do
    :io_lib.format(p(config), [x])
  end

  defp printable_list([]) do
    false
  end

  defp printable_list(x) do
    :io_lib.printable_list(x)
  end

  defp format_msg({:string, chardata}, meta, config) do
    format_msg({'~ts', [chardata]}, meta, config)
  end

  defp format_msg({:report, _} = msg, meta, %{report_cb: fun} = config)
       when is_function(fun, 1) or is_function(fun, 2) do
    format_msg(msg, Map.put(meta, :report_cb, fun), :maps.remove(:report_cb, config))
  end

  defp format_msg({:report, report}, %{report_cb: fun} = meta, config)
       when is_function(fun, 1) do
    try do
      fun.(report)
    catch
      c, r ->
        p = p(config)

        format_msg(
          {'REPORT_CB/1 CRASH: ' ++ p ++ '; Reason: ' ++ p,
           [
             report,
             {c, r,
              :logger.filter_stacktrace(
                :logger_formatter,
                __STACKTRACE__
              )}
           ]},
          meta,
          config
        )
    else
      {format, args} when is_list(format) and is_list(args) ->
        format_msg({format, args}, :maps.remove(:report_cb, meta), config)

      other ->
        p = p(config)

        format_msg(
          {'REPORT_CB/1 ERROR: ' ++ p ++ '; Returned: ' ++ p, [report, other]},
          meta,
          config
        )
    end
  end

  defp format_msg({:report, report}, %{report_cb: fun} = meta, config)
       when is_function(fun, 2) do
    try do
      fun.(
        report,
        :maps.with(
          [:depth, :chars_limit, :single_line],
          config
        )
      )
    catch
      c, r ->
        p = p(config)

        format_msg(
          {'REPORT_CB/2 CRASH: ' ++ p ++ '; Reason: ' ++ p,
           [
             report,
             {c, r,
              :logger.filter_stacktrace(
                :logger_formatter,
                __STACKTRACE__
              )}
           ]},
          meta,
          config
        )
    else
      chardata when is_list(chardata) or is_binary(chardata) ->
        try do
          chardata_to_list(chardata)
        catch
          _, _ ->
            p = p(config)

            format_msg(
              {'REPORT_CB/2 ERROR: ' ++ p ++ '; Returned: ' ++ p, [report, chardata]},
              meta,
              config
            )
        end

      other ->
        p = p(config)

        format_msg(
          {'REPORT_CB/2 ERROR: ' ++ p ++ '; Returned: ' ++ p, [report, other]},
          meta,
          config
        )
    end
  end

  defp format_msg({:report, report}, meta, config) do
    format_msg(
      {:report, report},
      Map.put(meta, :report_cb, &:logger.format_report/1),
      config
    )
  end

  defp format_msg(msg, _Meta, %{depth: depth, chars_limit: charsLimit, single_line: single}) do
    opts = chars_limit_to_opts(charsLimit)
    format_msg(msg, depth, opts, single)
  end

  defp chars_limit_to_opts(:unlimited) do
    []
  end

  defp chars_limit_to_opts(charsLimit) do
    [{:chars_limit, charsLimit}]
  end

  defp format_msg({format0, args}, depth, opts, single) do
    try do
      format1 = :io_lib.scan_format(format0, args)
      format = reformat(format1, depth, single)
      :io_lib.build_text(format, opts)
    catch
      c, r ->
        p = p(single)
        formatError = 'FORMAT ERROR: ' ++ p ++ ' - ' ++ p

        case format0 do
          ^formatError ->
            :erlang.raise(c, r, __STACKTRACE__)

          _ ->
            format_msg({formatError, [format0, args]}, depth, opts, single)
        end
    end
  end

  defp reformat(format, :unlimited, false) do
    format
  end

  defp reformat([%{control_char: c} = m | t], depth, true)
       when c === ?p do
    [limit_depth(Map.put(m, :width, 0), depth) | reformat(t, depth, true)]
  end

  defp reformat([%{control_char: c} = m | t], depth, true)
       when c === ?P do
    [Map.put(m, :width, 0) | reformat(t, depth, true)]
  end

  defp reformat([%{control_char: c} = m | t], depth, single)
       when c === ?p or c === ?w do
    [limit_depth(m, depth) | reformat(t, depth, single)]
  end

  defp reformat([h | t], depth, single) do
    [h | reformat(t, depth, single)]
  end

  defp reformat([], _, _) do
    []
  end

  defp limit_depth(m0, :unlimited) do
    m0
  end

  defp limit_depth(%{control_char: c0, args: args} = m0, depth) do
    c = c0 - (?a - ?A)
    %{m0 | control_char: c, args: args ++ [depth]}
  end

  defp chardata_to_list(chardata) do
    case :unicode.characters_to_list(
           chardata,
           :unicode
         ) do
      list when is_list(list) ->
        list

      error ->
        throw(error)
    end
  end

  defp truncate(b, msg, a, :unlimited) do
    [b, msg, a]
  end

  defp truncate(b, msg, a, size) do
    string = [b, msg, a]
    length = :io_lib.chars_length(string)

    cond do
      length > size ->
        {last, flatString} =
          case a do
            [] ->
              case msg do
                [] ->
                  {get_last(b), :lists.flatten(b)}

                _ ->
                  {get_last(msg), :lists.flatten([b, msg])}
              end

            _ ->
              {get_last(a), :lists.flatten(string)}
          end

        case last do
          ?\n ->
            :lists.sublist(flatString, 1, size - 4) ++ '...\n'

          _ ->
            :lists.sublist(flatString, 1, size - 3) ++ '...'
        end

      true ->
        string
    end
  end

  defp get_last(l) do
    get_first(:lists.reverse(l))
  end

  defp get_first([]) do
    :error
  end

  defp get_first([c | _]) when is_integer(c) do
    c
  end

  defp get_first([l | rest]) when is_list(l) do
    case get_last(l) do
      :error ->
        get_first(rest)

      first ->
        first
    end
  end

  defp format_time(
         sysTime,
         %{time_offset: offset, time_designator: des}
       )
       when is_integer(sysTime) do
    :calendar.system_time_to_rfc3339(
      sysTime,
      [{:unit, :microsecond}, {:offset, offset}, {:time_designator, des}]
    )
  end

  defp timestamp_to_datetimemicro(sysTime, config) when is_integer(sysTime) do
    micro = rem(sysTime, 1_000_000)
    sec = div(sysTime, 1_000_000)
    universalTime = :erlang.posixtime_to_universaltime(sec)

    {{date, time}, utcStr} =
      case offset_to_utc(
             :maps.get(
               :time_offset,
               config
             )
           ) do
        true ->
          {universalTime, 'UTC '}

        _ ->
          {:erlang.universaltime_to_localtime(universalTime), ''}
      end

    {date, time, micro, utcStr}
  end

  defp format_mfa({m, f, a}, _)
       when is_atom(m) and
              is_atom(f) and is_integer(a) do
    :io_lib.fwrite('~tw:~tw/~w', [m, f, a])
  end

  defp format_mfa({m, f, a}, config)
       when is_atom(m) and
              is_atom(f) and is_list(a) do
    format_mfa({m, f, length(a)}, config)
  end

  defp format_mfa(mFA, config) do
    to_string(mFA, config)
  end

  defp maybe_add_legacy_header(level, %{time: timestamp} = meta, %{legacy_header: true} = config) do
    %{title: title} = myMeta = add_legacy_title(level, meta, config)
    {{y, mo, d}, {h, mi, s}, micro, utcStr} = timestamp_to_datetimemicro(timestamp, config)

    header =
      :io_lib.format(
        '=~ts==== ~w-~s-~4w::~2..0w:~2..0w:~2..0w.~6..0w ~s===',
        [title, d, month(mo), y, h, mi, s, micro, utcStr]
      )

    Map.put(meta, :logger_formatter, Map.put(myMeta, :header, header))
  end

  defp maybe_add_legacy_header(_, meta, _) do
    meta
  end

  defp add_legacy_title(_Level, %{logger_formatter: %{title: _} = myMeta}, _) do
    myMeta
  end

  defp add_legacy_title(level, meta, config) do
    case :maps.get(:logger_formatter, meta, %{}) do
      %{title: _} = myMeta ->
        myMeta

      myMeta ->
        titleLevel =
          case level === :notice and
                 :maps.find(
                   :error_logger,
                   meta
                 ) do
            {:ok, _} ->
              :maps.get(:error_logger_notice_header, config)

            _ ->
              level
          end

        title = :string.uppercase(:erlang.atom_to_list(titleLevel)) ++ ' REPORT'
        Map.put(myMeta, :title, title)
    end
  end

  defp month(1) do
    'Jan'
  end

  defp month(2) do
    'Feb'
  end

  defp month(3) do
    'Mar'
  end

  defp month(4) do
    'Apr'
  end

  defp month(5) do
    'May'
  end

  defp month(6) do
    'Jun'
  end

  defp month(7) do
    'Jul'
  end

  defp month(8) do
    'Aug'
  end

  defp month(9) do
    'Sep'
  end

  defp month(10) do
    'Oct'
  end

  defp month(11) do
    'Nov'
  end

  defp month(12) do
    'Dec'
  end

  defp add_default_config(config0) do
    default = %{
      chars_limit: :unlimited,
      error_logger_notice_header: :info,
      legacy_header: false,
      single_line: true,
      time_designator: ?T
    }

    maxSize = get_max_size(:maps.get(:max_size, config0, :undefined))
    depth = get_depth(:maps.get(:depth, config0, :undefined))
    offset = get_offset(:maps.get(:time_offset, config0, :undefined))

    add_default_template(
      :maps.merge(
        default,
        Map.merge(config0, %{max_size: maxSize, depth: depth, time_offset: offset})
      )
    )
  end

  defp add_default_template(%{template: _} = config) do
    config
  end

  defp add_default_template(config) do
    Map.put(config, :template, default_template(config))
  end

  defp default_template(%{legacy_header: true}) do
    [[:logger_formatter, :header], '\n', :msg, '\n']
  end

  defp default_template(%{single_line: true}) do
    [:time, ' ', :level, ': ', :msg, '\n']
  end

  defp default_template(_) do
    [:time, ' ', :level, ':\n', :msg, '\n']
  end

  defp get_max_size(:undefined) do
    :unlimited
  end

  defp get_max_size(s) do
    max(10, s)
  end

  defp get_depth(:undefined) do
    :error_logger.get_format_depth()
  end

  defp get_depth(s) do
    max(5, s)
  end

  defp get_offset(:undefined) do
    utc_to_offset(get_utc_config())
  end

  defp get_offset(offset) do
    offset
  end

  defp utc_to_offset(true) do
    'Z'
  end

  defp utc_to_offset(false) do
    ''
  end

  defp get_utc_config() do
    case :application.get_env(:sasl, :utc_log) do
      {:ok, val} when is_boolean(val) ->
        val

      _ ->
        case :application.get_env(:stdlib, :utc_log) do
          {:ok, val} when is_boolean(val) ->
            val

          _ ->
            false
        end
    end
  end

  defp offset_to_utc(z) when z === 0 or z === 'z' or z === 'Z' do
    true
  end

  defp offset_to_utc([?+ | tz]) do
    case :io_lib.fread('~d:~d', tz) do
      {:ok, [0, 0], []} ->
        true

      _ ->
        false
    end
  end

  defp offset_to_utc(_) do
    false
  end

  def check_config(config) when is_map(config) do
    do_check_config(:maps.to_list(config))
  end

  def check_config(config) do
    {:error, {:invalid_formatter_config, :logger_formatter, config}}
  end

  defp do_check_config([{type, l} | config])
       when type == :chars_limit or type == :depth or
              type == :max_size do
    case check_limit(l) do
      :ok ->
        do_check_config(config)

      :error ->
        {:error, {:invalid_formatter_config, :logger_formatter, {type, l}}}
    end
  end

  defp do_check_config([{:single_line, sL} | config])
       when is_boolean(sL) do
    do_check_config(config)
  end

  defp do_check_config([{:legacy_header, lH} | config])
       when is_boolean(lH) do
    do_check_config(config)
  end

  defp do_check_config([{:error_logger_notice_header, eLNH} | config])
       when eLNH == :info or eLNH == :notice do
    do_check_config(config)
  end

  defp do_check_config([{:report_cb, rCB} | config])
       when is_function(rCB, 1) or is_function(rCB, 2) do
    do_check_config(config)
  end

  defp do_check_config([{:template, t} | config]) do
    case check_template(t) do
      :ok ->
        do_check_config(config)

      :error ->
        {:error, {:invalid_formatter_template, :logger_formatter, t}}
    end
  end

  defp do_check_config([{:time_offset, offset} | config]) do
    case check_offset(offset) do
      :ok ->
        do_check_config(config)

      :error ->
        {:error, {:invalid_formatter_config, :logger_formatter, {:time_offset, offset}}}
    end
  end

  defp do_check_config([{:time_designator, char} | config])
       when char >= 0 and char <= 255 do
    case :io_lib.printable_latin1_list([char]) do
      true ->
        do_check_config(config)

      false ->
        {:error, {:invalid_formatter_config, :logger_formatter, {:time_designator, char}}}
    end
  end

  defp do_check_config([c | _]) do
    {:error, {:invalid_formatter_config, :logger_formatter, c}}
  end

  defp do_check_config([]) do
    :ok
  end

  defp check_limit(l) when is_integer(l) and l > 0 do
    :ok
  end

  defp check_limit(:unlimited) do
    :ok
  end

  defp check_limit(_) do
    :error
  end

  defp check_template([key | t]) when is_atom(key) do
    check_template(t)
  end

  defp check_template([key | t])
       when is_list(key) and
              is_atom(hd(key)) do
    case :lists.all(
           fn
             x when is_atom(x) ->
               true

             _ ->
               false
           end,
           key
         ) do
      true ->
        check_template(t)

      false ->
        :error
    end
  end

  defp check_template([{key, ifExist, else__} | t])
       when is_atom(key) or (is_list(key) and is_atom(hd(key))) do
    case check_template(ifExist) do
      :ok ->
        case check_template(else__) do
          :ok ->
            check_template(t)

          :error ->
            :error
        end

      :error ->
        :error
    end
  end

  defp check_template([str | t]) when is_list(str) do
    case :io_lib.printable_unicode_list(str) do
      true ->
        check_template(t)

      false ->
        :error
    end
  end

  defp check_template([]) do
    :ok
  end

  defp check_template(_) do
    :error
  end

  defp check_offset(i) when is_integer(i) do
    :ok
  end

  defp check_offset(tz) when tz === '' or tz === 'Z' or tz === 'z' do
    :ok
  end

  defp check_offset([sign | tz]) when sign === ?+ or sign === ?- do
    check_timezone(tz)
  end

  defp check_offset(_) do
    :error
  end

  defp check_timezone(tz) do
    try do
      :io_lib.fread('~d:~d', tz)
    catch
      _, _ ->
        :error
    else
      {:ok, [_, _], []} ->
        :ok

      _ ->
        :error
    end
  end

  defp p(%{single_line: single}) do
    p(single)
  end

  defp p(true) do
    '~0tp'
  end

  defp p(false) do
    '~tp'
  end
end
