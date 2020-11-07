defmodule :cowboy_metrics_h do
  use Bitwise
  @behaviour :cowboy_stream
  require Record

  Record.defrecord(:r_state, :state,
    next: :undefined,
    callback: :undefined,
    resp_headers_filter: :undefined,
    req: :undefined,
    resp_status: :undefined,
    resp_headers: :undefined,
    ref: :undefined,
    req_start: :undefined,
    req_end: :undefined,
    req_body_start: :undefined,
    req_body_end: :undefined,
    resp_start: :undefined,
    resp_end: :undefined,
    procs: %{},
    informational: [],
    req_body_length: 0,
    resp_body_length: 0,
    user_data: %{}
  )

  def init(streamID, req = %{:ref => ref}, opts = %{:metrics_callback => fun}) do
    reqStart = :erlang.monotonic_time()
    {commands, next} = :cowboy_stream.init(streamID, req, opts)

    filteredReq =
      case :maps.get(:metrics_req_filter, opts, :undefined) do
        :undefined ->
          req

        reqFilter ->
          reqFilter.(req)
      end

    respHeadersFilter = :maps.get(:metrics_resp_headers_filter, opts, :undefined)

    {commands,
     fold(
       commands,
       r_state(
         next: next,
         callback: fun,
         resp_headers_filter: respHeadersFilter,
         req: filteredReq,
         ref: ref,
         req_start: reqStart
       )
     )}
  end

  def data(streamID, isFin = :fin, data, state = r_state(req_body_start: :undefined)) do
    reqBody = :erlang.monotonic_time()

    do_data(
      streamID,
      isFin,
      data,
      r_state(state,
        req_body_start: reqBody,
        req_body_end: reqBody,
        req_body_length: byte_size(data)
      )
    )
  end

  def data(streamID, isFin = :fin, data, state = r_state(req_body_length: reqBodyLen)) do
    reqBodyEnd = :erlang.monotonic_time()

    do_data(
      streamID,
      isFin,
      data,
      r_state(state,
        req_body_end: reqBodyEnd,
        req_body_length: reqBodyLen + byte_size(data)
      )
    )
  end

  def data(streamID, isFin, data, state = r_state(req_body_start: :undefined)) do
    reqBodyStart = :erlang.monotonic_time()

    do_data(
      streamID,
      isFin,
      data,
      r_state(state,
        req_body_start: reqBodyStart,
        req_body_length: byte_size(data)
      )
    )
  end

  def data(streamID, isFin, data, state = r_state(req_body_length: reqBodyLen)) do
    do_data(streamID, isFin, data, r_state(state, req_body_length: reqBodyLen + byte_size(data)))
  end

  defp do_data(streamID, isFin, data, state0 = r_state(next: next0)) do
    {commands, next} = :cowboy_stream.data(streamID, isFin, data, next0)
    {commands, fold(commands, r_state(state0, next: next))}
  end

  def info(streamID, info = {:EXIT, pid, reason}, state0 = r_state(procs: procs)) do
    procEnd = :erlang.monotonic_time()
    p = :maps.get(pid, procs)
    state = r_state(state0, procs: %{procs | pid => %{p | :exit => procEnd, :reason => reason}})
    do_info(streamID, info, state)
  end

  def info(streamID, info, state) do
    do_info(streamID, info, state)
  end

  defp do_info(streamID, info, state0 = r_state(next: next0)) do
    {commands, next} = :cowboy_stream.info(streamID, info, next0)
    {commands, fold(commands, r_state(state0, next: next))}
  end

  defp fold([], state) do
    state
  end

  defp fold(
         [{:spawn, pid, _} | tail],
         state0 = r_state(procs: procs)
       ) do
    procStart = :erlang.monotonic_time()
    state = r_state(state0, procs: %{procs | pid => %{:spawn => procStart}})
    fold(tail, state)
  end

  defp fold(
         [{:inform, status, headers} | tail],
         state = r_state(informational: infos)
       ) do
    time = :erlang.monotonic_time()

    fold(
      tail,
      r_state(state,
        informational: [
          %{:status => status, :headers => headers, :time => time}
          | infos
        ]
      )
    )
  end

  defp fold(
         [{:response, status, headers, body} | tail],
         state = r_state(resp_headers_filter: respHeadersFilter)
       ) do
    resp = :erlang.monotonic_time()

    fold(
      tail,
      r_state(state,
        resp_status: status,
        resp_headers:
          case respHeadersFilter do
            :undefined ->
              headers

            _ ->
              respHeadersFilter.(headers)
          end,
        resp_start: resp,
        resp_end: resp,
        resp_body_length: resp_body_length(body)
      )
    )
  end

  defp fold(
         [
           {:error_response, status, headers, body}
           | tail
         ],
         state = r_state(resp_status: respStatus)
       ) do
    case respStatus do
      :undefined ->
        fold([{:response, status, headers, body} | tail], state)

      _ ->
        fold(tail, state)
    end
  end

  defp fold(
         [{:headers, status, headers} | tail],
         state = r_state(resp_headers_filter: respHeadersFilter)
       ) do
    respStart = :erlang.monotonic_time()

    fold(
      tail,
      r_state(state,
        resp_status: status,
        resp_headers:
          case respHeadersFilter do
            :undefined ->
              headers

            _ ->
              respHeadersFilter.(headers)
          end,
        resp_start: respStart
      )
    )
  end

  defp fold(
         [{:data, :nofin, data} | tail],
         state = r_state(resp_body_length: respBodyLen)
       ) do
    fold(
      tail,
      r_state(state, resp_body_length: respBodyLen + resp_body_length(data))
    )
  end

  defp fold(
         [{:data, :fin, data} | tail],
         state = r_state(resp_body_length: respBodyLen)
       ) do
    respEnd = :erlang.monotonic_time()

    fold(
      tail,
      r_state(state,
        resp_end: respEnd,
        resp_body_length: respBodyLen + resp_body_length(data)
      )
    )
  end

  defp fold(
         [{:set_options, setOpts} | tail],
         state0 = r_state(user_data: oldUserData)
       ) do
    state =
      case setOpts do
        %{:metrics_user_data => newUserData} ->
          r_state(state0,
            user_data:
              :maps.merge(
                oldUserData,
                newUserData
              )
          )

        _ ->
          state0
      end

    fold(tail, state)
  end

  defp fold([_ | tail], state) do
    fold(tail, state)
  end

  def terminate(
        streamID,
        reason,
        r_state(
          next: next,
          callback: fun,
          req: req,
          resp_status: respStatus,
          resp_headers: respHeaders,
          ref: ref,
          req_start: reqStart,
          req_body_start: reqBodyStart,
          req_body_end: reqBodyEnd,
          resp_start: respStart,
          resp_end: respEnd,
          procs: procs,
          informational: infos,
          user_data: userData,
          req_body_length: reqBodyLen,
          resp_body_length: respBodyLen
        )
      ) do
    res = :cowboy_stream.terminate(streamID, reason, next)
    reqEnd = :erlang.monotonic_time()

    metrics = %{
      :ref => ref,
      :pid => self(),
      :streamid => streamID,
      :reason => reason,
      :req => req,
      :resp_status => respStatus,
      :resp_headers => respHeaders,
      :req_start => reqStart,
      :req_end => reqEnd,
      :req_body_start => reqBodyStart,
      :req_body_end => reqBodyEnd,
      :resp_start => respStart,
      :resp_end => respEnd,
      :procs => procs,
      :informational => :lists.reverse(infos),
      :req_body_length => reqBodyLen,
      :resp_body_length => respBodyLen,
      :user_data => userData
    }

    fun.(metrics)
    res
  end

  def early_error(
        streamID,
        reason,
        partialReq = %{:ref => ref},
        resp0,
        opts = %{:metrics_callback => fun}
      ) do
    time = :erlang.monotonic_time()

    resp =
      {:response, respStatus, respHeaders, respBody} =
      :cowboy_stream.early_error(streamID, reason, partialReq, resp0, opts)

    metrics = %{
      :ref => ref,
      :pid => self(),
      :streamid => streamID,
      :reason => reason,
      :partial_req => partialReq,
      :resp_status => respStatus,
      :resp_headers => respHeaders,
      :early_error_time => time,
      :resp_body_length => resp_body_length(respBody)
    }

    fun.(metrics)
    resp
  end

  defp resp_body_length({:sendfile, _, len, _}) do
    len
  end

  defp resp_body_length(data) do
    :erlang.iolist_size(data)
  end
end
