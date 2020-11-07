defmodule :cow_sse do
  use Bitwise
  require Record

  Record.defrecord(:r_state, :state,
    state_name: :bom,
    buffer: <<>>,
    last_event_id: <<>>,
    last_event_id_set: false,
    event_type: <<>>,
    data: [],
    retry: :undefined
  )

  def init() do
    r_state()
  end

  def parse(
        data0,
        state = r_state(state_name: :bom, buffer: buffer)
      ) do
    data1 =
      case buffer do
        <<>> ->
          data0

        _ ->
          <<buffer::binary, data0::binary>>
      end

    case data1 do
      <<254, 255, data::bits>> ->
        parse_event(
          data,
          r_state(state, state_name: :events, buffer: <<>>)
        )

      <<254>> ->
        {:more, r_state(state, buffer: data1)}

      <<>> ->
        {:more, state}

      _ ->
        parse_event(
          data1,
          r_state(state, state_name: :events, buffer: <<>>)
        )
    end
  end

  def parse(<<>>, state = r_state(buffer: buffer)) do
    parse_event(buffer, r_state(state, buffer: <<>>))
  end

  def parse(data0, state = r_state(buffer: buffer)) do
    data =
      case buffer do
        <<>> ->
          data0

        _ ->
          <<buffer::binary, data0::binary>>
      end

    parse_event(data, state)
  end

  defp parse_event(data, state0) do
    case :binary.split(data, ["\r\n", "\r", "\n"]) do
      [line, rest] ->
        case parse_line(line, state0) do
          {:ok, state} ->
            parse_event(rest, state)

          {:event, event, state} ->
            {:event, event, r_state(state, buffer: rest)}
        end

      [_] ->
        {:more, r_state(state0, buffer: data)}
    end
  end

  defp parse_line(<<>>, state) do
    dispatch_event(state)
  end

  defp parse_line(<<?:, _::bits>>, state) do
    {:ok, state}
  end

  defp parse_line(line, state) do
    case :binary.split(line, [": ", ":"]) do
      [field, value] ->
        process_field(field, value, state)

      [field] ->
        process_field(field, <<>>, state)
    end
  end

  defp process_field("event", value, state) do
    {:ok, r_state(state, event_type: value)}
  end

  defp process_field("data", value, state = r_state(data: data)) do
    {:ok, r_state(state, data: [[<<?\n>>, value] | data])}
  end

  defp process_field("id", value, state) do
    {:ok,
     r_state(state,
       last_event_id: value,
       last_event_id_set: true
     )}
  end

  defp process_field("retry", value, state) do
    try do
      {:ok, r_state(state, retry: :erlang.binary_to_integer(value))}
    catch
      _, _ ->
        {:ok, state}
    end
  end

  defp process_field(_, _, state) do
    {:ok, state}
  end

  defp dispatch_event(
         state =
           r_state(
             last_event_id_set: false,
             data: []
           )
       ) do
    {:ok, r_state(state, event_type: <<>>)}
  end

  defp dispatch_event(
         state =
           r_state(
             last_event_id: lastEventID,
             data: []
           )
       ) do
    {:event, %{:last_event_id => lastEventID},
     r_state(state, last_event_id_set: false, event_type: <<>>)}
  end

  defp dispatch_event(
         state = r_state(last_event_id: lastEventID, event_type: eventType, data: [_ | data])
       ) do
    {:event,
     %{
       :last_event_id => lastEventID,
       :event_type =>
         case eventType do
           <<>> ->
             "message"

           _ ->
             eventType
         end,
       :data => :lists.reverse(data)
     }, r_state(state, last_event_id_set: false, event_type: <<>>, data: [])}
  end

  def events(events) do
    for event <- events do
      event(event)
    end
  end

  def event(event) do
    [
      event_comment(event),
      event_id(event),
      event_name(event),
      event_data(event),
      event_retry(event),
      ?\n
    ]
  end

  defp event_comment(%{:comment => comment}) do
    prefix_lines(comment, <<>>)
  end

  defp event_comment(_) do
    []
  end

  defp event_id(%{:id => iD}) do
    :nomatch =
      :binary.match(
        :erlang.iolist_to_binary(iD),
        "\n"
      )

    ["id: ", iD, ?\n]
  end

  defp event_id(_) do
    []
  end

  defp event_name(%{:event => name0}) do
    name =
      cond do
        is_atom(name0) ->
          :erlang.atom_to_binary(name0, :utf8)

        true ->
          :erlang.iolist_to_binary(name0)
      end

    :nomatch = :binary.match(name, "\n")
    ["event: ", name, ?\n]
  end

  defp event_name(_) do
    []
  end

  defp event_data(%{:data => data}) do
    prefix_lines(data, "data")
  end

  defp event_data(_) do
    []
  end

  defp event_retry(%{:retry => retry}) do
    ["retry: ", :erlang.integer_to_binary(retry), ?\n]
  end

  defp event_retry(_) do
    []
  end

  defp prefix_lines(ioData, prefix) do
    lines = :binary.split(:erlang.iolist_to_binary(ioData), "\n", [:global])

    for line <- lines do
      [prefix, ": ", line, ?\n]
    end
  end
end
