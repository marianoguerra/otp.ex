defmodule :cowboy_stream do
  use Bitwise

  def init(streamID, req, opts) do
    case :maps.get(:stream_handlers, opts, [:cowboy_stream_h]) do
      [] ->
        {[], :undefined}

      [handler | tail] ->
        {commands, state} = handler.init(streamID, req, %{opts | :stream_handlers => tail})
        {commands, {handler, state}}
    end
  end

  def data(_, _, _, :undefined) do
    {[], :undefined}
  end

  def data(streamID, isFin, data, {handler, state0}) do
    {commands, state} = handler.data(streamID, isFin, data, state0)
    {commands, {handler, state}}
  end

  def info(_, _, :undefined) do
    {[], :undefined}
  end

  def info(streamID, info, {handler, state0}) do
    {commands, state} = handler.info(streamID, info, state0)
    {commands, {handler, state}}
  end

  def terminate(_, _, :undefined) do
    :ok
  end

  def terminate(streamID, reason, {handler, state}) do
    _ = handler.terminate(streamID, reason, state)
    :ok
  end

  def early_error(streamID, reason, partialReq, resp, opts) do
    case :maps.get(:stream_handlers, opts, [:cowboy_stream_h]) do
      [] ->
        resp

      [handler | tail] ->
        handler.early_error(streamID, reason, partialReq, resp, %{opts | :stream_handlers => tail})
    end
  end

  def make_error_log(:init, [streamID, req, opts], class, exception, stacktrace) do
    {:log, :error,
     'Unhandled exception ~p:~p in cowboy_stream:init(~p, Req, Opts)~nStacktrace: ~p~nReq: ~p~nOpts: ~p~n',
     [class, exception, streamID, stacktrace, req, opts]}
  end

  def make_error_log(:data, [streamID, isFin, data, state], class, exception, stacktrace) do
    {:log, :error,
     'Unhandled exception ~p:~p in cowboy_stream:data(~p, ~p, Data, State)~nStacktrace: ~p~nData: ~p~nState: ~p~n',
     [class, exception, streamID, isFin, stacktrace, data, state]}
  end

  def make_error_log(:info, [streamID, msg, state], class, exception, stacktrace) do
    {:log, :error,
     'Unhandled exception ~p:~p in cowboy_stream:info(~p, Msg, State)~nStacktrace: ~p~nMsg: ~p~nState: ~p~n',
     [class, exception, streamID, stacktrace, msg, state]}
  end

  def make_error_log(:terminate, [streamID, reason, state], class, exception, stacktrace) do
    {:log, :error,
     'Unhandled exception ~p:~p in cowboy_stream:terminate(~p, Reason, State)~nStacktrace: ~p~nReason: ~p~nState: ~p~n',
     [class, exception, streamID, stacktrace, reason, state]}
  end

  def make_error_log(
        :early_error,
        [streamID, reason, partialReq, resp, opts],
        class,
        exception,
        stacktrace
      ) do
    {:log, :error,
     'Unhandled exception ~p:~p in cowboy_stream:early_error(~p, Reason, PartialReq, Resp, Opts)~nStacktrace: ~p~nReason: ~p~nPartialReq: ~p~nResp: ~p~nOpts: ~p~n',
     [class, exception, streamID, stacktrace, reason, partialReq, resp, opts]}
  end
end
