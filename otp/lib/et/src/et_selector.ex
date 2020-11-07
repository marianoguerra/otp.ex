defmodule :m_et_selector do
  use Bitwise
  require Record

  Record.defrecord(:r_event, :event,
    detail_level: :undefined,
    trace_ts: :undefined,
    event_ts: :undefined,
    from: :undefined,
    to: :undefined,
    label: :undefined,
    contents: :undefined
  )

  def make_pattern(:undefined) do
    {:undefined, :undefined}
  end

  def make_pattern({mod, pattern}) when is_atom(mod) do
    case pattern do
      :min ->
        {mod, []}

      :max ->
        head = [:"$1", :_, :_, :_, :_]
        body = []
        cond__ = []
        {mod, [{head, cond__, body}]}

      detailLevel when is_integer(detailLevel) ->
        head = [:"$1", :_, :_, :_, :_]
        body = []
        cond__ = [{:<, :"$1", detailLevel}]
        {mod, [{head, cond__, body}]}

      :undefined ->
        {mod, :undefined}

      _ ->
        exit({:bad_pattern, pattern})
    end
  end

  def change_pattern({mod, pattern}) when is_atom(mod) do
    mFA = {mod, :trace_me, 5}

    case pattern do
      :undefined ->
        :ignore

      [] ->
        error_to_exit(old_ctp(mFA))
        error_to_exit(:dbg.ctp(mFA))
        error_to_exit(:dbg.p(:all, :clear))

      list when is_list(list) ->
        error_to_exit(old_ctp(mFA))
        error_to_exit(old_tp(mFA, pattern))
        error_to_exit(:dbg.ctp(mFA))
        error_to_exit(:dbg.tp(mFA, pattern))
        error_to_exit(:dbg.p(:all, [:call, :timestamp]))

      other ->
        change_pattern(make_pattern({mod, other}))
    end

    :ok
  end

  defp old_ctp({mod, _Fun, args}) do
    case mod do
      :et ->
        {:ok, :ignore}

      _ ->
        :dbg.ctp({mod, :report_event, args})
    end
  end

  defp old_tp({mod, _Fun, args}, pattern) do
    case mod do
      :et ->
        {:ok, :ignore}

      _ ->
        :dbg.tp({mod, :report_event, args}, pattern)
    end
  end

  defp error_to_exit({:error, reason}) do
    exit(reason)
  end

  defp error_to_exit({:ok, res}) do
    res
  end

  def parse_event(_Mod, e) when elem(e, 0) === :event do
    true
  end

  def parse_event(mod, trace) do
    parsedTS = :erlang.now()

    case trace do
      {:trace, pid, label, info} ->
        parse_event(mod, trace, parsedTS, parsedTS, pid, label, [info])

      {:trace, pid, label, info, extra} ->
        parse_event(mod, trace, parsedTS, parsedTS, pid, label, [info, extra])

      {:trace_ts, pid, label, info, reportedTS} ->
        parse_event(mod, trace, parsedTS, reportedTS, pid, label, [info])

      {:trace_ts, pid, label, info, extra, reportedTS} ->
        parse_event(mod, trace, parsedTS, reportedTS, pid, label, [info, extra])

      {:seq_trace, label, info} ->
        parse_seq_event(trace, parsedTS, parsedTS, label, info)

      {:seq_trace, label, info, reportedTS} ->
        parse_seq_event(trace, parsedTS, reportedTS, label, info)

      {:drop, numberOfDroppedItems} ->
        detailLevel = 20

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: parsedTS,
           event_ts: parsedTS,
           from: :undefined,
           to: :undefined,
           label: :drop,
           contents: [
             {:label, :drop},
             {:detail_level, detailLevel},
             {:from, :undefined},
             {:to, :undefined},
             {:drop, numberOfDroppedItems}
           ]
         )}

      _ ->
        :error_logger.format('~p(~p): Ignoring unknown trace type -> ~tp~n~n', [
          :et_selector,
          212,
          trace
        ])

        false
    end
  end

  defp parse_seq_event(trace, parsedTS, reportedTS, label, info) do
    case info do
      {:send, serial, from, to, msg} ->
        detailLevel = 15

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: to,
           label: {:seq_send, label},
           contents: [
             {:label, {:seq_send, label}},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, to},
             {:serial, serial},
             {:msg, msg}
           ]
         )}

      {:receive, serial, from, to, msg} ->
        detailLevel = 10

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: to,
           label: {:seq_receive, label},
           contents: [
             {:label, {:seq_receive, label}},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, to},
             {:serial, serial},
             {:msg, msg}
           ]
         )}

      {:print, serial, from, _, userInfo} ->
        detailLevel = 5

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: from,
           label: {:seq_print, label},
           contents: [
             {:label, {:seq_print, label}},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, from},
             {:serial, serial},
             {:user_info, userInfo}
           ]
         )}

      _ ->
        :error_logger.format('~p(~p): Ignoring unknown trace type -> ~tp~n~n', [
          :et_selector,
          262,
          trace
        ])

        false
    end
  end

  defp parse_event(mod, trace, parsedTS, reportedTS, from, label, contents) do
    case label do
      :receive ->
        detailLevel = 35
        [msg] = contents

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: from,
           label: label,
           contents: [
             {:label, label},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, from},
             {:msg, msg}
           ]
         )}

      :send ->
        detailLevel = 40
        [msg, to] = contents

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: to,
           label: label,
           contents: [
             {:label, label},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, to},
             {:msg, msg}
           ]
         )}

      :send_to_non_existing_process ->
        detailLevel = 40
        [msg, to] = contents

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: to,
           label: label,
           contents: [
             {:label, label},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, to},
             {:msg, msg}
           ]
         )}

      :call ->
        case contents do
          [{m, :trace_me, [userDetailLevel, userFrom, userTo, userLabel, userContents]}]
          when m == mod and mod != :undefined ->
            {true,
             r_event(
               detail_level: userDetailLevel,
               trace_ts: reportedTS,
               event_ts: parsedTS,
               from: userFrom,
               to: userTo,
               label: userLabel,
               contents: userContents
             )}

          [{m, :report_event, [userDetailLevel, userFrom, userTo, userLabel, userContents]}]
          when m == mod and mod != :undefined ->
            {true,
             r_event(
               detail_level: userDetailLevel,
               trace_ts: reportedTS,
               event_ts: parsedTS,
               from: userFrom,
               to: userTo,
               label: userLabel,
               contents: userContents
             )}

          [mFA] ->
            detailLevel = 45

            {true,
             r_event(
               detail_level: detailLevel,
               trace_ts: reportedTS,
               event_ts: parsedTS,
               from: from,
               to: from,
               label: label,
               contents: [
                 {:label, label},
                 {:detail_level, detailLevel},
                 {:from, from},
                 {:to, from},
                 {:mfa, mFA}
               ]
             )}

          [mFA, pamResult] ->
            detailLevel = 45

            {true,
             r_event(
               detail_level: detailLevel,
               trace_ts: reportedTS,
               event_ts: parsedTS,
               from: from,
               to: from,
               label: label,
               contents: [
                 {:label, label},
                 {:detail_level, detailLevel},
                 {:from, from},
                 {:to, from},
                 {:mfa, mFA},
                 {:pam_result, pamResult}
               ]
             )}
        end

      :return_to ->
        detailLevel = 50
        [mFA] = contents

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: from,
           label: label,
           contents: [
             {:label, label},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, from},
             {:mfa, mFA}
           ]
         )}

      :return_from ->
        detailLevel = 52
        [mFA, returnValue] = contents

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: from,
           label: label,
           contents: [
             {:label, label},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, from},
             {:mfa, mFA},
             {:return, returnValue}
           ]
         )}

      :exception_from ->
        detailLevel = 54
        [mFA, exception] = contents

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: from,
           label: label,
           contents: [
             {:label, label},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, from},
             {:mfa, mFA},
             {:exception, exception}
           ]
         )}

      :spawn ->
        detailLevel = 25
        [newPid, mFA] = contents

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: newPid,
           label: label,
           contents: [
             {:label, label},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, newPid},
             {:mfa, mFA}
           ]
         )}

      :exit ->
        detailLevel = 30
        [reason] = contents

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: from,
           label: label,
           contents: [
             {:label, label},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, from},
             {:reason, reason}
           ]
         )}

      :link ->
        detailLevel = 55
        [linkTo] = contents

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: linkTo,
           label: label,
           contents: [{:label, label}, {:detail_level, detailLevel}, {:from, from}, {:to, linkTo}]
         )}

      :unlink ->
        detailLevel = 60
        [unlinkFrom] = contents

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: unlinkFrom,
           label: label,
           contents: [
             {:label, label},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, unlinkFrom}
           ]
         )}

      :getting_linked ->
        detailLevel = 65
        [linkTo] = contents

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: linkTo,
           label: label,
           contents: [{:label, label}, {:detail_level, detailLevel}, {:from, from}, {:to, linkTo}]
         )}

      :getting_unlinked ->
        detailLevel = 67
        [unlinkFrom] = contents

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: unlinkFrom,
           label: label,
           contents: [
             {:label, label},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, unlinkFrom}
           ]
         )}

      :register ->
        detailLevel = 70
        [name] = contents

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: from,
           label: label,
           contents: [
             {:label, label},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, from},
             {:name, name}
           ]
         )}

      :unregister ->
        detailLevel = 75
        [name] = contents

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: from,
           label: label,
           contents: [
             {:label, label},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, from},
             {:name, name}
           ]
         )}

      :in ->
        detailLevel = 90
        [mFA] = contents

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: from,
           label: label,
           contents: [
             {:label, label},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, from},
             {:mfa, mFA}
           ]
         )}

      :out ->
        detailLevel = 95
        [mFA] = contents

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: from,
           label: label,
           contents: [
             {:label, label},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, from},
             {:mfa, mFA}
           ]
         )}

      :gc_minor_start ->
        detailLevel = 80
        [gcKeyValueList] = contents

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: from,
           label: label,
           contents: [
             {:label, label},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, from},
             {:gc_items, gcKeyValueList}
           ]
         )}

      :gc_minor_end ->
        detailLevel = 85
        [gcKeyValueList] = contents

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: from,
           label: label,
           contents: [
             {:label, label},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, from},
             {:gc_items, gcKeyValueList}
           ]
         )}

      :gc_major_start ->
        detailLevel = 80
        [gcKeyValueList] = contents

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: from,
           label: label,
           contents: [
             {:label, label},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, from},
             {:gc_items, gcKeyValueList}
           ]
         )}

      :gc_major_end ->
        detailLevel = 85
        [gcKeyValueList] = contents

        {true,
         r_event(
           detail_level: detailLevel,
           trace_ts: reportedTS,
           event_ts: parsedTS,
           from: from,
           to: from,
           label: label,
           contents: [
             {:label, label},
             {:detail_level, detailLevel},
             {:from, from},
             {:to, from},
             {:gc_items, gcKeyValueList}
           ]
         )}

      _ ->
        :error_logger.format('~p(~p): Ignoring unknown trace type -> ~tp~n~n', [
          :et_selector,
          594,
          trace
        ])

        false
    end
  end
end
