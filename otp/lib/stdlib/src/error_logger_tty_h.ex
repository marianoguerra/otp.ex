defmodule :m_error_logger_tty_h do
  use Bitwise
  @behaviour :gen_event
  require Record

  Record.defrecord(:r_st, :st,
    user: :undefined,
    prev_handler: :undefined,
    io_mod: :io,
    depth: :unlimited,
    modifier: ''
  )

  def init({[], {:error_logger, buf}}) do
    user = set_group_leader()
    depth = :error_logger.get_format_depth()
    modifier = modifier()
    state = r_st(user: user, prev_handler: :error_logger, depth: depth, modifier: modifier)
    write_events(state, buf)
    {:ok, state}
  end

  def init({[], {:error_logger_tty_h, prevHandler}}) do
    user = set_group_leader()
    {:ok, r_st(user: user, prev_handler: prevHandler)}
  end

  def init([]) do
    user = set_group_leader()
    depth = :error_logger.get_format_depth()
    modifier = modifier()
    {:ok, r_st(user: user, prev_handler: [], depth: depth, modifier: modifier)}
  end

  def handle_event({_Type, gL, _Msg}, state)
      when node(gL) !== node() do
    {:ok, state}
  end

  def handle_event(event, state) do
    :ok = do_write_event(state, tag_event(event))
    {:ok, state}
  end

  def handle_info(
        {:EXIT, user, _Reason},
        r_st(user: user, prev_handler: prevHandler) = state
      ) do
    case prevHandler do
      [] ->
        :remove_handler

      _ ->
        {:swap_handler, :install_prev, state, prevHandler, :go_back}
    end
  end

  def handle_info(_, state) do
    {:ok, state}
  end

  def handle_call(_Query, state) do
    {:ok, {:error, :bad_query}, state}
  end

  def terminate(:install_prev, _State) do
    []
  end

  def terminate(_Reason, r_st(prev_handler: prevHandler)) do
    {:error_logger_tty_h, prevHandler}
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  def write_event(event, ioMod) do
    do_write_event(r_st(io_mod: ioMod), event)
  end

  def write_event(event, ioMod, {depth, enc}) do
    modifier = modifier(enc)

    do_write_event(
      r_st(io_mod: ioMod, depth: depth, modifier: modifier),
      event
    )
  end

  defp set_group_leader() do
    case :erlang.whereis(:user) do
      user when is_pid(user) ->
        :erlang.link(user)
        :erlang.group_leader(user, self())
        user

      _ ->
        false
    end
  end

  defp tag_event(event) do
    {:erlang.universaltime(), event}
  end

  defp write_events(state, [ev | es]) do
    _ = write_events(state, es)
    _ = do_write_event(state, ev)
    :ok
  end

  defp write_events(_State, []) do
    :ok
  end

  defp do_write_event(r_st(modifier: m) = state, {time, event}) do
    case parse_event(event, m) do
      :ignore ->
        :ok

      {title, pid, formatList} ->
        header = header(time, title, m)
        body = format_body(state, formatList)

        atNode =
          cond do
            node(pid) !== node() ->
              ['** at node ', :erlang.atom_to_list(node(pid)), ' **\n']

            true ->
              []
          end

        str = [header, atNode, body]

        case r_st(state, :io_mod) do
          :io_lib ->
            str

          :io ->
            :io.put_chars(:user, str)
        end
    end
  end

  defp do_write_event(_, _) do
    :ok
  end

  defp format_body(r_st(modifier: m) = state, [{format, args} | t]) do
    s =
      try do
        format(state, format, args)
      catch
        _, _ ->
          format(state, 'ERROR: ~' ++ m ++ 'p - ~' ++ m ++ 'p\n', [format, args])
      else
        s0 ->
          s0
      end

    [s | format_body(state, t)]
  end

  defp format_body(_State, []) do
    []
  end

  defp format(r_st(depth: :unlimited), format, args) do
    :io_lib.format(format, args)
  end

  defp format(r_st(depth: depth), format0, args) do
    format1 = :io_lib.scan_format(format0, args)
    format = limit_format(format1, depth)
    :io_lib.build_text(format)
  end

  defp limit_format([%{:control_char => c0} = m0 | t], depth)
       when c0 === ?p or c0 === ?w do
    c = c0 - (?a - ?A)
    %{:args => args} = m0
    m = %{m0 | :control_char => c, :args => args ++ [depth]}
    [m | limit_format(t, depth)]
  end

  defp limit_format([h | t], depth) do
    [h | limit_format(t, depth)]
  end

  defp limit_format([], _) do
    []
  end

  defp parse_event({:error, _GL, {pid, format, args}}, _) do
    {'ERROR REPORT', pid, [{format, args}]}
  end

  defp parse_event({:info_msg, _GL, {pid, format, args}}, _) do
    {'INFO REPORT', pid, [{format, args}]}
  end

  defp parse_event({:warning_msg, _GL, {pid, format, args}}, _) do
    {'WARNING REPORT', pid, [{format, args}]}
  end

  defp parse_event(
         {:error_report, _GL, {pid, :std_error, args}},
         m
       ) do
    {'ERROR REPORT', pid, format_term(args, m)}
  end

  defp parse_event(
         {:info_report, _GL, {pid, :std_info, args}},
         m
       ) do
    {'INFO REPORT', pid, format_term(args, m)}
  end

  defp parse_event(
         {:warning_report, _GL, {pid, :std_warning, args}},
         m
       ) do
    {'WARNING REPORT', pid, format_term(args, m)}
  end

  defp parse_event(_, _) do
    :ignore
  end

  defp format_term(term, m) when is_list(term) do
    case string_p(:lists.flatten(term)) do
      true ->
        [{'~' ++ m ++ 's\n', [term]}]

      false ->
        format_term_list(term, m)
    end
  end

  defp format_term(term, m) do
    [{'~' ++ m ++ 'p\n', [term]}]
  end

  defp format_term_list([{tag, data} | t], m) do
    [
      {'    ~' ++ m ++ 'p: ~' ++ m ++ 'p\n', [tag, data]}
      | format_term_list(t, m)
    ]
  end

  defp format_term_list([data | t], m) do
    [{'    ~' ++ m ++ 'p\n', [data]} | format_term_list(t, m)]
  end

  defp format_term_list([], _) do
    []
  end

  defp string_p([]) do
    false
  end

  defp string_p(flatList) do
    :io_lib.printable_list(flatList)
  end

  defp get_utc_config() do
    case :application.get_env(:sasl, :utc_log) do
      {:ok, val} ->
        val

      :undefined ->
        case :application.get_env(:stdlib, :utc_log) do
          {:ok, val} ->
            val

          :undefined ->
            false
        end
    end
  end

  defp header(time, title, m) do
    case get_utc_config() do
      true ->
        header(time, title, 'UTC ', m)

      _ ->
        header(:calendar.universal_time_to_local_time(time), title, '', m)
    end
  end

  defp header({{y, mo, d}, {h, mi, s}}, title, uTC, m) do
    :io_lib.format(
      '~n=~' ++ m ++ 's==== ~p-~s-~p::~s:~s:~s ~s===~n',
      [title, d, month(mo), y, t(h), t(mi), t(s), uTC]
    )
  end

  defp t(x) when is_integer(x) do
    t1(:erlang.integer_to_list(x))
  end

  defp t(_) do
    ''
  end

  defp t1([x]) do
    [?0, x]
  end

  defp t1(x) do
    x
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

  defp modifier() do
    modifier(encoding())
  end

  defp modifier(:latin1) do
    ''
  end

  defp modifier(_) do
    't'
  end

  defp encoding() do
    :proplists.get_value(:encoding, :io.getopts(), :latin1)
  end
end
