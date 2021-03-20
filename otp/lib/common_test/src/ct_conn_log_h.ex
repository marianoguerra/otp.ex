defmodule :m_ct_conn_log_h do
  use Bitwise
  require Record

  Record.defrecord(:r_conn, :conn,
    handle: :undefined,
    targetref: :undefined,
    address: :undefined,
    callback: :undefined
  )

  Record.defrecord(:r_testspec, :testspec,
    spec_dir: :undefined,
    nodes: [],
    init: [],
    label: [],
    profile: [],
    logdir: ['.'],
    logopts: [],
    basic_html: [],
    esc_chars: [],
    verbosity: [],
    silent_connections: [],
    cover: [],
    cover_stop: [],
    config: [],
    userconfig: [],
    event_handler: [],
    ct_hooks: [],
    enable_builtin_hooks: true,
    release_shell: false,
    include: [],
    auto_compile: [],
    abort_if_missing_suites: [],
    stylesheet: [],
    multiply_timetraps: [],
    scale_timetraps: [],
    create_priv_dir: [],
    alias: [],
    tests: [],
    unknown: [],
    merge_tests: true
  )

  Record.defrecord(:r_cover, :cover,
    app: :none,
    local_only: false,
    level: :details,
    excl_mods: [],
    incl_mods: [],
    cross: [],
    src: []
  )

  Record.defrecord(:r_conn_log, :conn_log,
    header: true,
    client: :undefined,
    name: :undefined,
    address: :undefined,
    conn_pid: :undefined,
    action: :undefined,
    module: :undefined
  )

  Record.defrecord(:r_state, :state,
    logs: [],
    default_gl: :undefined
  )

  def init({gL, connLogs}) do
    open_files(gL, connLogs, r_state(default_gl: gL))
  end

  defp open_files(gL, [{connMod, {logType, logFiles}} | t], state = r_state(logs: logs)) do
    case do_open_files(logFiles, []) do
      {:ok, fds} ->
        connInfo = :proplists.get_value(gL, logs, [])

        logs1 = [
          {gL, [{connMod, {logType, fds}} | connInfo]}
          | :proplists.delete(gL, logs)
        ]

        open_files(gL, t, r_state(state, logs: logs1))

      error ->
        error
    end
  end

  defp open_files(_GL, [], state) do
    {:ok, state}
  end

  defp do_open_files([{tag, file} | logFiles], acc) do
    case :file.open(
           file,
           [:write, :append, {:encoding, :utf8}]
         ) do
      {:ok, fd} ->
        do_open_files(logFiles, [{tag, fd} | acc])

      {:error, reason} ->
        {:error, {:could_not_open_log, file, reason}}
    end
  end

  defp do_open_files([], acc) do
    {:ok, :lists.reverse(acc)}
  end

  def handle_event(
        {:info_report, _, {from, :update, {gL, connLogs}}},
        state
      )
      when node(gL) == node() do
    result = open_files(gL, connLogs, state)
    send(from, {:updated, gL})
    result
  end

  def handle_event({_Type, gL, _Msg}, state)
      when node(gL) != node() do
    {:ok, state}
  end

  def handle_event(
        {_Type, gL, {pid, {:ct_connection, mod, action, connName}, report}},
        state
      ) do
    info =
      conn_info(
        pid,
        r_conn_log(name: connName, action: action, module: mod)
      )

    write_report(:os.timestamp(), info, report, gL, state)
    {:ok, state}
  end

  def handle_event({_Type, gL, {pid, info = r_conn_log(), report}}, state) do
    write_report(:os.timestamp(), conn_info(pid, info), report, gL, state)
    {:ok, state}
  end

  def handle_event(
        {:error_report, gL, {pid, _, [{:ct_connection, connName} | r]}},
        state
      ) do
    write_error(:os.timestamp(), conn_info(pid, r_conn_log(name: connName)), r, gL, state)
    {:ok, state}
  end

  def handle_event(_What, state) do
    {:ok, state}
  end

  def handle_info(_What, state) do
    {:ok, state}
  end

  def handle_call(_Query, state) do
    {:ok, {:error, :bad_query}, state}
  end

  def terminate(_, r_state(logs: logs)) do
    :lists.foreach(
      fn {_GL, connLogs} ->
        for {_, {_, fds}} <- connLogs, {_, fd} <- fds do
          :file.close(fd)
        end
      end,
      logs
    )

    :ok
  end

  defp write_report(_Time, r_conn_log(header: false, module: connMod) = info, data, gL, state) do
    case get_log(info, gL, state) do
      {:silent, _, _} ->
        :ok

      {logType, dest, fd} ->
        str =
          cond do
            logType == :html and dest == :gl ->
              ['$tc_html', '~n~ts']

            true ->
              '~n~ts'
          end

        :io.format(fd, str, [format_data(connMod, logType, data)])
    end
  end

  defp write_report(time, r_conn_log(module: connMod) = info, data, gL, state) do
    case get_log(info, gL, state) do
      {:silent, _, _} ->
        :ok

      {logType, dest, fd} ->
        case format_data(connMod, logType, data) do
          []
          when r_conn_log(info, :action) == :send or
                 r_conn_log(info, :action) == :recv ->
            :ok

          formattedData ->
            str =
              cond do
                logType == :html and dest == :gl ->
                  ['$tc_html', '~n~ts~ts~ts']

                true ->
                  '~n~ts~ts~ts'
              end

            :io.format(fd, str, [
              format_head(connMod, logType, time),
              format_title(logType, info),
              formattedData
            ])
        end
    end
  end

  defp write_error(time, r_conn_log(module: connMod) = info, report, gL, state) do
    case get_log(info, gL, state) do
      {logType, _, _}
      when logType == :html or
             logType == :silent ->
        :ok

      {logType, dest, fd} ->
        str =
          cond do
            logType == :html and dest == :gl ->
              ['$tc_html', '~n~ts~ts~ts']

            true ->
              '~n~ts~ts~ts'
          end

        :io.format(fd, str, [
          format_head(connMod, logType, time, ' ERROR'),
          format_title(logType, info),
          format_error(
            logType,
            report
          )
        ])
    end
  end

  defp get_log(info, gL, state) do
    case :proplists.get_value(gL, r_state(state, :logs)) do
      :undefined ->
        {:html, :gl, r_state(state, :default_gl)}

      connLogs ->
        case :proplists.get_value(
               r_conn_log(info, :module),
               connLogs
             ) do
          {:html, _} ->
            {:html, :gl, gL}

          {logType, fds} ->
            {logType, :file, get_fd(info, fds)}

          :undefined ->
            {:html, :gl, gL}
        end
    end
  end

  defp get_fd(r_conn_log(name: :undefined), fds) do
    :proplists.get_value(:default, fds)
  end

  defp get_fd(r_conn_log(name: connName), fds) do
    case :proplists.get_value(connName, fds) do
      :undefined ->
        :proplists.get_value(:default, fds)

      fd ->
        fd
    end
  end

  defp format_head(connMod, logType, time) do
    format_head(connMod, logType, time, '')
  end

  defp format_head(connMod, :raw, time, text) do
    :io_lib.format('~n~w, ~w~ts, ', [now_to_time(time), connMod, text])
  end

  defp format_head(connMod, _, time, text) do
    head = pad_char_end(80, pretty_head(now_to_time(time), connMod, text), ?=)
    :io_lib.format('~n~ts', [head])
  end

  defp format_title(:raw, r_conn_log(client: client) = info) do
    :io_lib.format(
      'Client ~tw ~s ~ts',
      [client, actionstr(info), serverstr(info)]
    )
  end

  defp format_title(_, info) do
    title = pad_char_end(80, pretty_title(info), ?=)
    :io_lib.format('~n~ts', [title])
  end

  defp format_data(_, _, noData)
       when noData == '' or
              noData == <<>> do
    ''
  end

  defp format_data(connMod, logType, data) do
    connMod.format_data(logType, data)
  end

  defp format_error(:raw, report) do
    :io_lib.format('~n~tp~n', [report])
  end

  defp format_error(:pretty, report) do
    for {k, v} <- report do
      :io_lib.format('~n    ~tp: ~tp', [k, v])
    end
  end

  defp conn_info(
         loggingProc,
         r_conn_log(client: :undefined) = connInfo
       ) do
    conn_info(r_conn_log(connInfo, client: loggingProc))
  end

  defp conn_info(_, connInfo) do
    conn_info(connInfo)
  end

  defp conn_info(
         r_conn_log(
           client: client,
           module: :undefined
         ) = connInfo
       ) do
    case :ets.lookup(:ct_connections, client) do
      [r_conn(address: address, callback: callback)] ->
        r_conn_log(connInfo, address: address, module: callback)

      [] ->
        connInfo
    end
  end

  defp conn_info(connInfo) do
    connInfo
  end

  defp now_to_time({_, _, microS} = now) do
    {:calendar.now_to_local_time(now), microS}
  end

  defp pretty_head({{{y, mo, d}, {h, mi, s}}, microS}, connMod, text0) do
    text = :string.uppercase(:erlang.atom_to_list(connMod) ++ text0)

    :io_lib.format(
      '= ~s ==== ~s-~s-~w::~s:~s:~s,~s ',
      [text, t(d), month(mo), y, t(h), t(mi), t(s), micro2milli(microS)]
    )
  end

  defp pretty_title(r_conn_log(client: client) = info) do
    :io_lib.format(
      '= Client ~tw ~s ~ts ',
      [client, actionstr(info), serverstr(info)]
    )
  end

  defp actionstr(r_conn_log(action: :send)) do
    '----->'
  end

  defp actionstr(r_conn_log(action: :cmd)) do
    '----->'
  end

  defp actionstr(r_conn_log(action: :recv)) do
    '<-----'
  end

  defp actionstr(r_conn_log(action: :open)) do
    'opened session to'
  end

  defp actionstr(r_conn_log(action: :close)) do
    'closed session to'
  end

  defp actionstr(r_conn_log(action: :connect)) do
    'connected to'
  end

  defp actionstr(r_conn_log(action: :disconnect)) do
    'disconnected from'
  end

  defp actionstr(_) do
    '<---->'
  end

  defp serverstr(
         r_conn_log(
           name: :undefined,
           address: {:undefined, _}
         )
       ) do
    :io_lib.format('server', [])
  end

  defp serverstr(r_conn_log(name: :undefined, address: address)) do
    :io_lib.format('~tp', [address])
  end

  defp serverstr(r_conn_log(name: alias, address: {:undefined, _})) do
    :io_lib.format('~tw', [alias])
  end

  defp serverstr(r_conn_log(name: alias, address: address)) do
    :io_lib.format('~tw(~tp)', [alias, address])
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

  defp micro2milli(x) do
    pad0(3, :erlang.integer_to_list(div(x, 1000)))
  end

  defp t(x) do
    pad0(2, :erlang.integer_to_list(x))
  end

  defp pad0(n, str) do
    m = length(str)
    :lists.duplicate(n - m, ?0) ++ str
  end

  defp pad_char_end(n, str, char) do
    case :string.length(str) do
      m when m < n ->
        str ++ :lists.duplicate(n - m, char)

      _ ->
        str
    end
  end
end
