defmodule :m_ct_netconfc do
  use Bitwise
  import Kernel, except: [send: 2]
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

  Record.defrecord(:r_xmlDecl, :xmlDecl,
    vsn: :undefined,
    encoding: :undefined,
    standalone: :undefined,
    attributes: :undefined
  )

  Record.defrecord(:r_xmlAttribute, :xmlAttribute,
    name: :undefined,
    expanded_name: [],
    nsinfo: [],
    namespace: [],
    parents: [],
    pos: :undefined,
    language: [],
    value: :undefined,
    normalized: :undefined
  )

  Record.defrecord(:r_xmlNamespace, :xmlNamespace,
    default: [],
    nodes: []
  )

  Record.defrecord(:r_xmlNsNode, :xmlNsNode,
    parents: [],
    pos: :undefined,
    prefix: :undefined,
    uri: []
  )

  Record.defrecord(:r_xmlElement, :xmlElement,
    name: :undefined,
    expanded_name: [],
    nsinfo: [],
    namespace: :EFE_TODO_NESTED_RECORD,
    parents: [],
    pos: :undefined,
    attributes: [],
    content: [],
    language: '',
    xmlbase: '',
    elementdef: :undeclared
  )

  Record.defrecord(:r_xmlText, :xmlText,
    parents: [],
    pos: :undefined,
    language: [],
    value: :undefined,
    type: :text
  )

  Record.defrecord(:r_xmlComment, :xmlComment,
    parents: [],
    pos: :undefined,
    language: [],
    value: :undefined
  )

  Record.defrecord(:r_xmlPI, :xmlPI,
    name: :undefined,
    parents: [],
    pos: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_xmlDocument, :xmlDocument, content: :undefined)

  Record.defrecord(:r_xmlContext, :xmlContext,
    axis_type: :forward,
    context_node: :undefined,
    context_position: 1,
    nodeset: [],
    bindings: [],
    functions: [],
    namespace: [],
    whole_document: :undefined
  )

  Record.defrecord(:r_xmlNode, :xmlNode, type: :element, node: :undefined, parents: [], pos: 1)

  Record.defrecord(:r_xmlObj, :xmlObj,
    type: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_xmerl_fun_states, :xmerl_fun_states,
    event: :undefined,
    hook: :undefined,
    rules: :undefined,
    fetch: :undefined,
    cont: :undefined
  )

  Record.defrecord(:r_xmerl_scanner, :xmerl_scanner,
    encoding: :undefined,
    standalone: :no,
    environment: :prolog,
    declarations: [],
    doctype_name: :undefined,
    doctype_DTD: :internal,
    comments: true,
    document: false,
    default_attrs: false,
    rules: :undefined,
    keep_rules: false,
    namespace_conformant: false,
    xmlbase: :undefined,
    xmlbase_cache: :undefined,
    fetch_path: [],
    filename: :file_name_unknown,
    validation: :off,
    schemaLocation: [],
    space: :preserve,
    event_fun: :undefined,
    hook_fun: :undefined,
    acc_fun: :undefined,
    fetch_fun: :undefined,
    close_fun: :undefined,
    continuation_fun: :undefined,
    rules_read_fun: :undefined,
    rules_write_fun: :undefined,
    rules_delete_fun: :undefined,
    user_state: :undefined,
    fun_states: :EFE_TODO_NESTED_RECORD,
    entity_references: [],
    text_decl: false,
    quiet: false,
    col: 1,
    line: 1,
    common_data: []
  )

  Record.defrecord(:r_xmerl_event, :xmerl_event,
    event: :undefined,
    line: :undefined,
    col: :undefined,
    pos: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_state, :state,
    host: :undefined,
    port: :undefined,
    connection: :undefined,
    capabilities: :undefined,
    session_id: :undefined,
    msg_id: 1,
    hello_status: :undefined,
    buf: false,
    pending: [],
    event_receiver: :undefined
  )

  Record.defrecord(:r_options, :options,
    ssh: [],
    host: :undefined,
    port: 830,
    timeout: :infinity,
    name: :undefined,
    type: :undefined
  )

  Record.defrecord(:r_connection, :connection,
    reference: :undefined,
    host: :undefined,
    port: :undefined,
    name: :undefined,
    type: :undefined
  )

  Record.defrecord(:r_pending, :pending,
    tref: :undefined,
    msg_id: :undefined,
    op: :undefined,
    caller: :undefined
  )

  def connect(options) do
    connect(options, r_options(type: :connection), [])
  end

  def connect(keyOrName, extraOptions) do
    connect(
      make_opts(keyOrName, extraOptions),
      r_options(name: keyOrName, type: :connection),
      [{:name, keyOrName}]
    )
  end

  defp connect(opts, initRec, nameOpt) do
    case make_options(opts, initRec) do
      r_options() = rec ->
        start(rec, nameOpt, false)

      {:error, _} = no ->
        no
    end
  end

  defp make_opts(keyOrName, extraOptions) do
    sortedExtra = :lists.keysort(1, extraOptions)

    sortedConfig =
      :lists.keysort(
        1,
        :ct.get_config(keyOrName, [])
      )

    :lists.ukeymerge(1, sortedConfig, sortedExtra)
  end

  def disconnect(conn) do
    case call(conn, :get_ssh_connection) do
      {:ok, _} ->
        :ct_gen_conn.stop(conn)

      error ->
        error
    end
  end

  def session(conn) do
    session(conn, [], r_options(type: :channel), [])
  end

  def session(conn, options) when is_list(options) do
    session(conn, options, r_options(type: :channel), [])
  end

  def session(keyOrName, conn) do
    session(conn, [], r_options(name: keyOrName, type: :channel), [{:name, keyOrName}])
  end

  def session(keyOrName, conn, extraOptions) do
    session(
      conn,
      make_opts(keyOrName, extraOptions),
      r_options(name: keyOrName, type: :channel),
      [{:name, keyOrName}]
    )
  end

  defp session(conn, opts, initRec, nameOpt) do
    t = make_ref()

    try do
      [_ | {:ok, sshConn}] = [
        t
        | call(
            conn,
            :get_ssh_connection
          )
      ]

      [_ | r_options() = rec] = [
        t
        | make_session_options(
            opts,
            initRec
          )
      ]

      [_ | {:ok, client} = ok] = [t | start(sshConn, rec, nameOpt, true)]
      [_ | :ok] = [t | hello(client, caps(opts), r_options(rec, :timeout))]
      ok
    catch
      :error, {:badmatch, [^t | error]} ->
        error
    end
  end

  defp caps(opts) do
    for {:capability, _} = t <- opts do
      t
    end
  end

  def open(options) do
    open(options, r_options(type: :connection_and_channel), [], true)
  end

  def open(keyOrName, extraOpts) do
    open(keyOrName, extraOpts, true)
  end

  defp open(keyOrName, extraOptions, hello) do
    open(
      make_opts(keyOrName, extraOptions),
      r_options(name: keyOrName, type: :connection_and_channel),
      [{:name, keyOrName}],
      hello
    )
  end

  defp open(opts, initRec, nameOpt, hello) do
    t = make_ref()

    try do
      [_, r_options() = rec] = [t, make_options(opts, initRec)]

      [_, {:ok, client} = ok | true] = [
        t,
        start(rec, nameOpt, true)
        | hello
      ]

      [_, :ok] = [t, hello(client, caps(opts), r_options(rec, :timeout))]
      ok
    catch
      :error, {:badmatch, [^t, res | _]} ->
        res
    end
  end

  defp start(r_options(host: :undefined), _, _) do
    {:error, :no_host_address}
  end

  defp start(r_options(port: :undefined), _, _) do
    {:error, :no_port}
  end

  defp start(r_options(host: host, port: port) = opts, nameOpt, fwd) do
    start({host, port}, opts, nameOpt, fwd)
  end

  defp start(ep, opts, nameOpt, fwd) do
    :ct_gen_conn.start(ep, opts, :ct_netconfc, [
      {:reconnect, false},
      {:use_existing_connection, false},
      {:forward_messages, fwd}
      | nameOpt
    ])
  end

  def only_open(options) do
    open(options, r_options(type: :connection_and_channel), [], false)
  end

  def only_open(keyOrName, extraOpts) do
    open(keyOrName, extraOpts, false)
  end

  def hello(client) do
    hello(client, [], :infinity)
  end

  def hello(client, timeout) do
    hello(client, [], timeout)
  end

  def hello(client, options, timeout) do
    call(client, {:hello, options, timeout})
  end

  def get_session_id(client) do
    get_session_id(client, :infinity)
  end

  def get_session_id(client, timeout) do
    call(client, :get_session_id, timeout)
  end

  def get_capabilities(client) do
    get_capabilities(client, :infinity)
  end

  def get_capabilities(client, timeout) do
    call(client, :get_capabilities, timeout)
  end

  def send(client, simpleXml) do
    send(client, simpleXml, :infinity)
  end

  def send(client, simpleXml, timeout) do
    call(client, {:send, timeout, simpleXml})
  end

  def send_rpc(client, simpleXml) do
    send_rpc(client, simpleXml, :infinity)
  end

  def send_rpc(client, simpleXml, timeout) do
    call(client, {:send_rpc, simpleXml, timeout})
  end

  def lock(client, target) do
    lock(client, target, :infinity)
  end

  def lock(client, target, timeout) do
    call(client, {:send_rpc_op, :lock, [target], timeout})
  end

  def unlock(client, target) do
    unlock(client, target, :infinity)
  end

  def unlock(client, target, timeout) do
    call(client, {:send_rpc_op, :unlock, [target], timeout})
  end

  def get(client, filter) do
    get(client, filter, :infinity)
  end

  def get(client, filter, timeout) do
    call(client, {:send_rpc_op, :get, [filter], timeout})
  end

  def get_config(client, source, filter) do
    get_config(client, source, filter, :infinity)
  end

  def get_config(client, source, filter, timeout) do
    call(
      client,
      {:send_rpc_op, :get_config, [source, filter], timeout}
    )
  end

  def edit_config(client, target, config) do
    edit_config(client, target, config, :infinity)
  end

  def edit_config(client, target, config, timeout)
      when is_integer(timeout) or timeout == :infinity do
    edit_config(client, target, config, [], timeout)
  end

  def edit_config(client, target, config, optParams)
      when is_list(optParams) do
    edit_config(client, target, config, optParams, :infinity)
  end

  def edit_config(client, target, config, optParams, timeout)
      when not is_list(config) do
    edit_config(client, target, [config], optParams, timeout)
  end

  def edit_config(client, target, config, optParams, timeout) do
    call(
      client,
      {:send_rpc_op, :edit_config, [target, config, optParams], timeout}
    )
  end

  def delete_config(client, target) do
    delete_config(client, target, :infinity)
  end

  def delete_config(client, target, timeout)
      when target == :startup or target == :candidate do
    call(
      client,
      {:send_rpc_op, :delete_config, [target], timeout}
    )
  end

  def copy_config(client, source, target) do
    copy_config(client, source, target, :infinity)
  end

  def copy_config(client, target, source, timeout) do
    call(
      client,
      {:send_rpc_op, :copy_config, [target, source], timeout}
    )
  end

  def action(client, action) do
    action(client, action, :infinity)
  end

  def action(client, action, timeout) do
    call(client, {:send_rpc_op, :action, [action], timeout})
  end

  def create_subscription(client, %{} = values) do
    create_subscription(client, values, :infinity)
  end

  def create_subscription(client, timeout)
      when is_integer(timeout) or timeout == :infinity do
    create_subscription(client, %{}, timeout)
  end

  def create_subscription(client, stream)
      when is_list(stream) and is_integer(hd(stream)) do
    create_subscription(client, %{stream: stream})
  end

  def create_subscription(client, filter)
      when is_atom(filter) or
             (is_tuple(filter) and
                is_atom(
                  :erlang.element(
                    1,
                    filter
                  )
                )) or filter == [] or
             (is_list(filter) and
                (is_atom(hd(filter)) or
                   (is_tuple(hd(filter)) and
                      is_atom(
                        :erlang.element(
                          1,
                          hd(filter)
                        )
                      )))) do
    create_subscription(client, %{filter: filter})
  end

  def create_subscription(client, %{} = values, timeout) do
    keys = [
      {:stream, 'NETCONF'},
      {:filter, :undefined},
      {:start, :undefined},
      {:stop, :undefined}
    ]

    call(
      client,
      {:send_rpc_op, {:create_subscription, self()},
       for {k, d} <- keys do
         :maps.get(k, values, d)
       end, timeout}
    )
  end

  def create_subscription(client, stream, timeout)
      when (is_list(stream) and is_integer(hd(stream)) and
              is_integer(timeout)) or timeout == :infinity do
    create_subscription(client, %{stream: stream}, timeout)
  end

  def create_subscription(client, startTime, stopTime)
      when is_list(startTime) and is_integer(hd(startTime)) and
             is_list(stopTime) and is_integer(hd(stopTime)) do
    create_subscription(
      client,
      %{start: startTime, stop: stopTime}
    )
  end

  def create_subscription(client, filter, timeout)
      when is_atom(filter) or
             (is_tuple(filter) and
                is_atom(
                  :erlang.element(
                    1,
                    filter
                  )
                )) or filter == [] or
             (is_list(filter) and
                (is_atom(hd(filter)) or
                   (is_tuple(hd(filter)) and
                      is_atom(
                        :erlang.element(
                          1,
                          hd(filter)
                        )
                      ))) and
                is_integer(timeout)) or timeout == :infinity do
    create_subscription(client, %{filter: filter}, timeout)
  end

  def create_subscription(client, stream, filter)
      when (is_list(stream) and is_integer(hd(stream)) and
              (is_atom(filter) or
                 (is_tuple(filter) and
                    is_atom(
                      :erlang.element(
                        1,
                        filter
                      )
                    )))) or filter == [] or
             (is_list(filter) and
                (is_atom(hd(filter)) or
                   (is_tuple(hd(filter)) and
                      is_atom(
                        :erlang.element(
                          1,
                          hd(filter)
                        )
                      )))) do
    create_subscription(
      client,
      %{stream: stream, filter: filter}
    )
  end

  def create_subscription(client) do
    create_subscription(client, %{})
  end

  def create_subscription(client, startTime, stopTime, timeout)
      when (is_list(startTime) and is_integer(hd(startTime)) and
              is_list(stopTime) and is_integer(hd(stopTime)) and
              is_integer(timeout)) or timeout == :infinity do
    values = %{start: startTime, stop: stopTime}
    create_subscription(client, values, timeout)
  end

  def create_subscription(client, stream, startTime, stopTime)
      when is_list(stream) and is_integer(hd(stream)) and
             is_list(startTime) and is_integer(hd(startTime)) and
             is_list(stopTime) and is_integer(hd(stopTime)) do
    create_subscription(
      client,
      %{stream: stream, start: startTime, stop: stopTime}
    )
  end

  def create_subscription(client, filter, startTime, stopTime)
      when is_atom(filter) or
             (is_tuple(filter) and
                is_atom(
                  :erlang.element(
                    1,
                    filter
                  )
                )) or filter == [] or
             (is_list(filter) and
                (is_atom(hd(filter)) or
                   (is_tuple(hd(filter)) and
                      is_atom(
                        :erlang.element(
                          1,
                          hd(filter)
                        )
                      ))) and
                is_list(startTime) and is_integer(hd(startTime)) and
                is_list(stopTime) and is_integer(hd(stopTime))) do
    create_subscription(
      client,
      %{filter: filter, start: startTime, stop: stopTime}
    )
  end

  def create_subscription(client, stream, filter, timeout)
      when (is_list(stream) and is_integer(hd(stream)) and
              (is_atom(filter) or
                 (is_tuple(filter) and
                    is_atom(
                      :erlang.element(
                        1,
                        filter
                      )
                    )))) or filter == [] or
             (is_list(filter) and
                (is_atom(hd(filter)) or
                   (is_tuple(hd(filter)) and
                      is_atom(
                        :erlang.element(
                          1,
                          hd(filter)
                        )
                      ))) and
                is_integer(timeout)) or timeout == :infinity do
    values = %{stream: stream, filter: filter}
    create_subscription(client, values, timeout)
  end

  def create_subscription(client, stream, startTime, stopTime, timeout)
      when (is_list(stream) and is_integer(hd(stream)) and
              is_list(startTime) and is_integer(hd(startTime)) and
              is_list(stopTime) and is_integer(hd(stopTime)) and
              is_integer(timeout)) or timeout == :infinity do
    values = %{stream: stream, start: startTime, stop: stopTime}
    create_subscription(client, values, timeout)
  end

  def create_subscription(client, stream, filter, startTime, stopTime)
      when (is_list(stream) and is_integer(hd(stream)) and
              (is_atom(filter) or
                 (is_tuple(filter) and
                    is_atom(
                      :erlang.element(
                        1,
                        filter
                      )
                    )))) or filter == [] or
             (is_list(filter) and
                (is_atom(hd(filter)) or
                   (is_tuple(hd(filter)) and
                      is_atom(
                        :erlang.element(
                          1,
                          hd(filter)
                        )
                      ))) and
                is_list(startTime) and is_integer(hd(startTime)) and
                is_list(stopTime) and is_integer(hd(stopTime))) do
    create_subscription(
      client,
      %{stream: stream, filter: filter, start: startTime, stop: stopTime}
    )
  end

  def create_subscription(client, stream, filter, startTime, stopTime, timeout) do
    values = %{stream: stream, filter: filter, start: startTime, stop: stopTime}
    create_subscription(client, values, timeout)
  end

  def get_event_streams(client) do
    get_event_streams(client, [], :infinity)
  end

  def get_event_streams(client, timeout)
      when is_integer(timeout) or
             timeout == :infinity do
    get_event_streams(client, [], timeout)
  end

  def get_event_streams(client, streams) when is_list(streams) do
    get_event_streams(client, streams, :infinity)
  end

  def get_event_streams(client, streams, timeout) do
    call(client, {:get_event_streams, streams, timeout})
  end

  def close_session(client) do
    close_session(client, :infinity)
  end

  def close_session(client, timeout) do
    call(client, {:send_rpc_op, :close_session, [], timeout}, true)
  end

  def kill_session(client, sessionId) do
    kill_session(client, sessionId, :infinity)
  end

  def kill_session(client, sessionId, timeout) do
    call(
      client,
      {:send_rpc_op, :kill_session, [sessionId], timeout}
    )
  end

  def init(_KeyOrName, {cM, {host, port}}, options) do
    case ssh_channel(
           r_connection(reference: cM, host: host, port: port),
           options
         ) do
      {:ok, connection} ->
        {:ok, cM, r_state(connection: connection)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  def init(_KeyOrName, {_Host, _Port}, options)
      when r_options(options, :type) == :connection do
    case ssh_connect(options) do
      {:ok, connection} ->
        connPid = r_connection(connection, :reference)
        {:ok, connPid, r_state(connection: connection)}

      error ->
        error
    end
  end

  def init(_KeyOrName, {_Host, _Port}, options) do
    case ssh_open(options) do
      {:ok, connection} ->
        {connPid, _} = r_connection(connection, :reference)
        {:ok, connPid, r_state(connection: connection)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  def terminate(_, r_state(connection: connection)) do
    ssh_close(connection)
    :ok
  end

  def handle_msg(
        {:hello, options, timeout},
        from,
        r_state(
          connection: connection,
          hello_status: helloStatus
        ) = state
      ) do
    case do_send(connection, client_hello(options)) do
      :ok when helloStatus == :undefined ->
        tRef = set_request_timer(timeout, :hello)
        {:noreply, r_state(state, hello_status: r_pending(tref: tRef, caller: from))}

      :ok ->
        handle_capx(state)

      error ->
        {:stop, error, state}
    end
  end

  def handle_msg(:get_ssh_connection, _From, r_state(connection: connection) = state) do
    reply =
      case r_connection(connection, :reference) do
        {_, _} ->
          {:error, :not_an_ssh_connection}

        cM ->
          {:ok, {cM, {r_connection(connection, :host), r_connection(connection, :port)}}}
      end

    {:reply, reply, state}
  end

  def handle_msg(_, _From, r_state(session_id: :undefined) = state) do
    {:reply, {:error, :waiting_for_hello}, state}
  end

  def handle_msg(:get_capabilities, _From, r_state(capabilities: caps) = state) do
    {:reply, caps, state}
  end

  def handle_msg(:get_session_id, _From, r_state(session_id: id) = state) do
    {:reply, id, state}
  end

  def handle_msg(
        {:send, timeout, simpleXml},
        from,
        r_state(connection: connection, pending: pending) = state
      ) do
    case do_send(connection, simpleXml) do
      :ok ->
        tRef = set_request_timer(timeout, :send)

        {:noreply,
         r_state(state,
           pending: [
             r_pending(tref: tRef, caller: from)
             | pending
           ]
         )}

      error ->
        {:reply, error, state}
    end
  end

  def handle_msg({:send_rpc, simpleXml, timeout}, from, state) do
    do_send_rpc(:undefined, simpleXml, timeout, from, state)
  end

  def handle_msg({:send_rpc_op, op, data, timeout}, from, state) do
    simpleXml = encode_rpc_operation(op, data)
    do_send_rpc(op, simpleXml, timeout, from, state)
  end

  def handle_msg({:get_event_streams = op, streams, timeout}, from, state) do
    filter =
      {:netconf, [{:xmlns, 'urn:ietf:params:xml:ns:netmod:notification'}],
       [
         {:streams,
          for name <- streams do
            {:stream, [{:name, [name]}]}
          end}
       ]}

    simpleXml = encode_rpc_operation(:get, [filter])
    do_send_rpc(op, simpleXml, timeout, from, state)
  end

  def handle_msg(
        {:ssh_cm, cM, {:data, ch, _Type, data}},
        state
      ) do
    :ssh_connection.adjust_window(cM, ch, :erlang.size(data))
    log(r_state(state, :connection), :recv, data)
    handle_data(data, state)
  end

  def handle_msg({:ssh_cm, _CM, _SshCloseMsg}, state) do
    {:stop, state}
  end

  def handle_msg(
        {:timeout, tRef, :hello},
        r_state(hello_status: r_pending(tref: tRef, caller: from)) = state
      ) do
    :ct_gen_conn.return(
      from,
      {:error, {:hello_session_failed, :timeout}}
    )

    {:stop, r_state(state, hello_status: {:error, :timeout})}
  end

  def handle_msg(
        {:timeout, tRef, op},
        r_state(pending: pending) = state
      ) do
    case :lists.keytake(tRef, r_pending(:tref), pending) do
      {:value, r_pending(caller: from), rest} ->
        :ct_gen_conn.return(from, {:error, :timeout})

        {cond do
           op == :close_session ->
             :stop

           true ->
             :noreply
         end,
         r_state(state,
           pending: rest,
           buf: is_binary(r_state(state, :buf))
         )}

      false ->
        {:noreply, state}
    end
  end

  def close(client) do
    case get_handle(client) do
      {:ok, pid} ->
        case :ct_gen_conn.stop(pid) do
          {:error, {:process_down, ^pid, :noproc}} ->
            {:error, :already_closed}

          result ->
            result
        end

      error ->
        error
    end
  end

  defp call(client, msg) do
    call(client, msg, :infinity, false)
  end

  defp call(client, msg, timeout)
       when is_integer(timeout) or timeout == :infinity do
    call(client, msg, timeout, false)
  end

  defp call(client, msg, waitStop)
       when is_boolean(waitStop) do
    call(client, msg, :infinity, waitStop)
  end

  defp call(client, msg, timeout, waitStop) do
    case get_handle(client) do
      {:ok, pid} ->
        case :ct_gen_conn.call(pid, msg, timeout) do
          {:error, {:process_down, ^pid, :noproc}} ->
            {:error, :no_such_client}

          {:error, {:process_down, ^pid, :normal}} when waitStop ->
            :ok

          {:error, {:process_down, ^pid, :normal}} ->
            {:error, :closed}

          {:error, {:process_down, ^pid, reason}} ->
            {:error, {:closed, reason}}

          other when waitStop ->
            mRef = :erlang.monitor(:process, pid)

            receive do
              {:DOWN, ^mRef, :process, ^pid, normal}
              when normal == :normal or normal == :noproc ->
                other

              {:DOWN, ^mRef, :process, ^pid, reason} ->
                {:error, {{:closed, reason}, other}}
            after
              timeout ->
                :erlang.demonitor(mRef, [:flush])
                {:error, {:timeout, other}}
            end

          other ->
            other
        end

      error ->
        error
    end
  end

  defp get_handle(client) when is_pid(client) do
    {:ok, client}
  end

  defp get_handle(client) do
    case :ct_util.get_connection(client, :ct_netconfc) do
      {:ok, {pid, _}} ->
        {:ok, pid}

      {:error, :no_registered_connection} ->
        {:error, {:no_connection_found, client}}

      error ->
        error
    end
  end

  defp make_options(opts, rec) do
    make_options(opts, r_options(rec, port: :undefined), &opt/2)
  end

  defp opt({t, host}, rec) when t == :ssh or t == :host do
    r_options(rec, host: host)
  end

  defp opt({:port, port}, rec) do
    r_options(rec, port: port)
  end

  defp opt({:timeout, tmo}, rec)
       when is_integer(tmo) or
              tmo == :infinity do
    r_options(rec, timeout: tmo)
  end

  defp opt({:timeout, _} = t, _) do
    throw(t)
  end

  defp opt({:capability, _}, rec) do
    rec
  end

  defp opt(opt, r_options(ssh: opts) = rec) do
    r_options(rec, ssh: [opt | opts])
  end

  defp make_session_options(opts, rec) do
    make_options(opts, rec, &session_opt/2)
  end

  defp session_opt({:capability, _}, rec) do
    rec
  end

  defp session_opt({:timeout, tmo}, rec)
       when is_integer(tmo) or
              tmo == :infinity do
    r_options(rec, timeout: tmo)
  end

  defp session_opt(t, _Rec) do
    throw(t)
  end

  defp make_options(opts, rec, f) do
    try do
      r_options() = :lists.foldl(f, rec, opts)
    catch
      t ->
        {:error, {:invalid_option, t}}
    end
  end

  defp set_request_timer(:infinity, _) do
    false
  end

  defp set_request_timer(tmo, op) do
    :erlang.start_timer(tmo, self(), op)
  end

  defp cancel_request_timer(false) do
    :ok
  end

  defp cancel_request_timer(tRef) do
    :erlang.cancel_timer(tRef)
  end

  defp client_hello(opts) when is_list(opts) do
    userCaps =
      for {:capability = t, cs} <- opts do
        {t, cap(:lists.flatten(cs))}
      end

    vsns = versions(userCaps)
    :erlang.put({:ct_netconfc, :protocol_vsn}, vsns)

    {:hello, [{:xmlns, 'urn:ietf:params:xml:ns:netconf:base:1.0'}],
     [
       {:capabilities,
        for _ <- [:EFE_DUMMY_GEN], [] == vsns do
          {:capability, ['urn:ietf:params:netconf:base:', '1.0']}
        end ++ userCaps}
     ]}
  end

  defp cap(':base:' ++ _ = str) do
    ['urn:ietf:params:netconf', str]
  end

  defp cap([?: | _] = str) do
    ['urn:ietf:params:netconf:capability', str]
  end

  defp cap(str) do
    [str]
  end

  defp versions(opts) do
    for {:capability, l} <- opts,
        s <- l,
        'urn:ietf:params:netconf:base:' ++ x <- [:lists.flatten(s)],
        v <- [
          :lists.takewhile(
            fn c ->
              c != ??
            end,
            x
          )
        ] do
      v
    end
  end

  defp handle_capx(
         r_state(
           hello_status: :received,
           capabilities: caps
         ) = s
       ) do
    remote =
      for 'urn:ietf:params:netconf:base:' ++ x <- caps,
          [v | _] <- [:string.lexemes(x, '? \t\r\n')] do
        v
      end

    local = :erlang.erase({:ct_netconfc, :protocol_vsn})

    case protocol_vsn(local, remote) do
      false when remote == [] ->
        reason = {:incorrect_hello, :no_base_capability_found}
        {:stop, {:error, reason}, s}

      false ->
        reason = {:incompatible_base_capability_vsn, :lists.min(remote)}
        {:stop, {:error, reason}, s}

      vsn ->
        :erlang.put({:ct_netconfc, :chunk}, vsn != '1.0')
        {:reply, :ok, rebuf(vsn, r_state(s, hello_status: vsn))}
    end
  end

  defp handle_capx(r_state(hello_status: {:error, _} = no) = s) do
    {:stop, no, s}
  end

  defp rebuf('1.0', s) do
    s
  end

  defp rebuf(_, r_state(buf: bin) = s) do
    r_state(s, buf: [bin, 3])
  end

  defp protocol_vsn([], vsns) do
    protocol_vsn(['1.0'], vsns)
  end

  defp protocol_vsn(local, remote) do
    :lists.max([
      false
      | for v <- remote,
            :lists.member(v, local) do
          v
        end
    ])
  end

  defp encode_rpc_operation(lock, [target])
       when lock == :lock or
              lock == :unlock do
    {lock, [{:target, [target]}]}
  end

  defp encode_rpc_operation(:get, [filter]) do
    {:get, filter(filter)}
  end

  defp encode_rpc_operation(:get_config, [source, filter]) do
    {:"get-config", [{:source, [source]}] ++ filter(filter)}
  end

  defp encode_rpc_operation(:edit_config, [target, config, optParams]) do
    {:"edit-config", [{:target, [target]}] ++ optParams ++ [{:config, config}]}
  end

  defp encode_rpc_operation(:delete_config, [target]) do
    {:"delete-config", [{:target, [target]}]}
  end

  defp encode_rpc_operation(:copy_config, [target, source]) do
    {:"copy-config", [{:target, [target]}, {:source, [source]}]}
  end

  defp encode_rpc_operation(:action, [action]) do
    {:action, [{:xmlns, 'urn:com:ericsson:ecim:1.0'}], [{:data, [action]}]}
  end

  defp encode_rpc_operation(:kill_session, [sessionId]) do
    {:"kill-session", [{:"session-id", [:erlang.integer_to_list(sessionId)]}]}
  end

  defp encode_rpc_operation(:close_session, []) do
    :"close-session"
  end

  defp encode_rpc_operation(
         {:create_subscription, _},
         [stream, filter, startTime, stopTime]
       ) do
    {:"create-subscription", [{:xmlns, 'urn:ietf:params:xml:ns:netconf:notification:1.0'}],
     [{:stream, [stream]}] ++
       filter(filter) ++
       maybe_element(
         :startTime,
         startTime
       ) ++
       maybe_element(
         :stopTime,
         stopTime
       )}
  end

  defp filter(:undefined) do
    []
  end

  defp filter({:xpath, filter})
       when is_list(filter) and is_integer(hd(filter)) do
    [{:filter, [{:type, 'xpath'}, {:select, filter}], []}]
  end

  defp filter(filter) when is_list(filter) do
    [{:filter, [{:type, 'subtree'}], filter}]
  end

  defp filter(filter) do
    filter([filter])
  end

  defp maybe_element(_, :undefined) do
    []
  end

  defp maybe_element(tag, value) do
    [{tag, [value]}]
  end

  defp do_send_rpc(
         op,
         simpleXml,
         timeout,
         caller,
         r_state(connection: connection, msg_id: msgId, pending: pending) = state
       ) do
    msg =
      {:rpc, [{:"message-id", msgId}, {:xmlns, 'urn:ietf:params:xml:ns:netconf:base:1.0'}],
       [simpleXml]}

    next = msgId + 1

    case do_send(connection, msg) do
      :ok ->
        tRef = set_request_timer(timeout, op)
        rec = r_pending(tref: tRef, msg_id: msgId, op: op, caller: caller)
        {:noreply, r_state(state, msg_id: next, pending: [rec | pending])}

      error ->
        {:reply, error, r_state(state, msg_id: next)}
    end
  end

  defp do_send(connection, simple) do
    ssh_send(connection, frame(to_xml(simple)))
  end

  defp to_xml(simple) do
    prolog = '<?xml version="1.0" encoding="UTF-8"?>'

    chars =
      :xmerl.export_simple([simple], :xmerl_xml, [r_xmlAttribute(name: :prolog, value: prolog)])

    :unicode.characters_to_binary(chars)
  end

  defp frame(bin) do
    case :erlang.get({:ct_netconfc, :chunk}) do
      true ->
        [chunk(bin) | '\n##\n']

      _ ->
        [bin | "]]>]]>"]
    end
  end

  defp chunk(<<>>) do
    []
  end

  defp chunk(bin) do
    sz = min(:rand.uniform(1024), :erlang.size(bin))
    <<b::size(sz)-binary, rest::binary>> = bin
    ['\n#', :erlang.integer_to_list(sz), ?\n, b | chunk(rest)]
  end

  defp handle_data(bin, r_state(buf: head) = s) do
    case recv(bin, head) do
      {:error, reason} ->
        conn = r_state(s, :connection)

        :error_logger.error_report([
          {:ct_connection, r_connection(conn, :name)},
          {:client, self()},
          {:module, :ct_netconfc},
          {:line, 1401},
          {:receive_error, reason},
          {:buffer, head},
          {:bytes, bin}
        ])

        {:stop, s}

      {bytes, rest} ->
        handle_more(rest, handle_xml(bytes, s))

      buf ->
        {:noreply, r_state(s, buf: buf)}
    end
  end

  defp handle_more(_, {:stop, _} = no) do
    no
  end

  defp handle_more(bin, {:noreply, state}) do
    handle_data(
      bin,
      r_state(state, buf: true == :erlang.get({:ct_netconfc, :chunk}))
    )
  end

  defp handle_xml(bytes, state) do
    case parse(bytes) do
      {:ok, simple, _Rest} ->
        decode(simple, state)

      {:fatal_error, _Loc, reason, _EndTags, _EventState} ->
        conn = r_state(state, :connection)

        :error_logger.error_report([
          {:ct_connection, r_connection(conn, :name)},
          {:client, self()},
          {:module, :ct_netconfc},
          {:line, 1427},
          {:parse_error, reason},
          {:message, bytes}
        ])

        {:noreply, handle_error(reason, state)}
    end
  end

  defp parse(bytes) do
    :xmerl_sax_parser.stream(
      <<>>,
      [
        {:event_fun, &sax_event/3},
        {:event_state, []},
        {:continuation_fun, &cont/1},
        {:continuation_state, bytes}
      ]
    )
  end

  defp cont([] = no) do
    {<<>>, no}
  end

  defp cont([bin | rest]) do
    {bin, rest}
  end

  defp cont(bin) do
    {bin, <<>>}
  end

  defp handle_error(_Reason, r_state(pending: []) = state) do
    state
  end

  defp handle_error(reason, r_state(pending: pending) = state) do
    rec =
      r_pending(
        tref: tRef,
        caller: caller
      ) = :lists.last(pending)

    cancel_request_timer(tRef)

    :ct_gen_conn.return(
      caller,
      {:error, {:failed_to_parse_received_data, reason}}
    )

    r_state(state, pending: :lists.delete(rec, pending))
  end

  defp sax_event(event, _Loc, state) do
    sax_event(event, state)
  end

  defp sax_event({:startPrefixMapping, prefix, uri}, acc) do
    [{:xmlns, {prefix, uri}} | acc]
  end

  defp sax_event({:startElement, _Uri, _Name, qN, attrs}, acc) do
    {nsAttrs, newAcc} = split_attrs_and_elements(acc, [])
    tag = qn_to_tag(qN)
    [{tag, nsAttrs ++ parse_attrs(attrs), []} | newAcc]
  end

  defp sax_event(
         {:endElement, _Uri, _Name, _QN},
         [{name, attrs, cont}, {parent, pA, pC} | acc]
       ) do
    [
      {parent, pA, [{name, attrs, :lists.reverse(cont)} | pC]}
      | acc
    ]
  end

  defp sax_event(:endDocument, [{tag, attrs, cont}]) do
    {tag, attrs, :lists.reverse(cont)}
  end

  defp sax_event(
         {:characters, string},
         [{name, attrs, cont} | acc]
       ) do
    [{name, attrs, [string | cont]} | acc]
  end

  defp sax_event(_Event, state) do
    state
  end

  defp split_attrs_and_elements([{:xmlns, {prefix, uri}} | rest], attrs) do
    split_attrs_and_elements(
      rest,
      [{xmlnstag(prefix), uri} | attrs]
    )
  end

  defp split_attrs_and_elements(elements, attrs) do
    {attrs, elements}
  end

  defp xmlnstag([]) do
    :xmlns
  end

  defp xmlnstag(prefix) do
    :erlang.list_to_atom('xmlns:' ++ prefix)
  end

  defp qn_to_tag({[], name}) do
    :erlang.list_to_atom(name)
  end

  defp qn_to_tag({prefix, name}) do
    :erlang.list_to_atom(prefix ++ ':' ++ name)
  end

  defp parse_attrs([{_Uri, [], name, value} | attrs]) do
    [
      {:erlang.list_to_atom(name), value}
      | parse_attrs(attrs)
    ]
  end

  defp parse_attrs([{_Uri, prefix, name, value} | attrs]) do
    [
      {:erlang.list_to_atom(prefix ++ ':' ++ name), value}
      | parse_attrs(attrs)
    ]
  end

  defp parse_attrs([]) do
    []
  end

  defp decode({tag, _, _} = e, r_state() = state) do
    case decode(get_local_name_atom(tag), e, state) do
      r_state() = s ->
        {:noreply, s}

      {:stop, r_state()} = t ->
        t
    end
  end

  defp decode(:"rpc-reply", {_, attrs, _} = e, state) do
    decode_rpc_reply(get_msg_id(attrs), e, state)
  end

  defp decode(:hello, e, r_state(hello_status: :undefined) = state) do
    case decode_hello(e) do
      {:ok, sessionId, capabilities} ->
        r_state(state, session_id: sessionId, capabilities: capabilities, hello_status: :received)

      {:error, _Reason} = no ->
        r_state(state, hello_status: no)
    end
  end

  defp decode(:hello, e, r_state(hello_status: r_pending(tref: tRef, caller: from)) = state) do
    cancel_request_timer(tRef)

    case decode_hello(e) do
      {:ok, sessionId, capabilities} ->
        reply(
          from,
          handle_capx(
            r_state(state,
              session_id: sessionId,
              capabilities: capabilities,
              hello_status: :received
            )
          )
        )

      {:error, _Reason} = no ->
        :ct_gen_conn.return(from, no)
        {:stop, r_state(state, hello_status: no)}
    end
  end

  defp decode(:hello, e, r_state(hello_status: other) = state) do
    connName = r_connection(r_state(state, :connection), :name)

    :error_logger.error_report([
      {:ct_connection, connName},
      {:client, self()},
      {:module, :ct_netconfc},
      {:line, 1561},
      {:got_unexpected_hello, e},
      {:hello_status, other}
    ])

    state
  end

  defp decode(:notification, e, state) do
    send(r_state(state, :event_receiver), e)
    state
  end

  defp decode(other, e, state) do
    decode_send({:got_unexpected_msg, other}, e, state)
  end

  defp reply(from, {t, res, state}) do
    :ct_gen_conn.return(from, res)

    case t do
      :reply ->
        state

      :stop ->
        {t, state}
    end
  end

  defp get_msg_id(attrs) do
    case find(:"message-id", attrs) do
      {_, str} ->
        :erlang.list_to_integer(str)

      false ->
        :undefined
    end
  end

  defp decode_rpc_reply(:undefined, e, r_state(pending: [r_pending(msg_id: msgId)]) = state)
       when msgId != :undefined do
    connName = r_connection(r_state(state, :connection), :name)

    :error_logger.error_report([
      {:ct_connection, connName},
      {:client, self()},
      {:module, :ct_netconfc},
      {:line, 1601},
      {:warning, :rpc_reply_missing_msg_id},
      {:assuming, msgId}
    ])

    decode_rpc_reply(msgId, e, state)
  end

  defp decode_rpc_reply(:undefined, _, state) do
    connName = r_connection(r_state(state, :connection), :name)

    :error_logger.error_report([
      {:ct_connection, connName},
      {:client, self()},
      {:module, :ct_netconfc},
      {:line, 1607},
      {:error, :rpc_reply_missing_msg_id}
    ])

    state
  end

  defp decode_rpc_reply(msgId, {_, attrs, content0} = e, r_state(pending: pending) = state) do
    case :lists.keytake(msgId, r_pending(:msg_id), pending) do
      {:value, rec, rest} ->
        r_pending(tref: tRef, op: op, caller: from) = rec
        cancel_request_timer(tRef)
        content = forward_xmlns_attr(attrs, content0)
        {reply, t} = do_decode_rpc_reply(op, content, r_state(state, pending: rest))
        :ct_gen_conn.return(from, reply)
        t

      false ->
        decode_send({:got_unexpected_msg_id, msgId}, e, state)
    end
  end

  defp decode_send(errorT, elem, r_state(pending: pending) = state) do
    case (for r_pending(msg_id: :undefined) = p <- pending do
            p
          end) do
      [rec] ->
        r_pending(tref: tRef, caller: from) = rec
        cancel_request_timer(tRef)
        :ct_gen_conn.return(from, elem)
        r_state(state, pending: :lists.delete(rec, pending))

      _ ->
        conn = r_state(state, :connection)

        :error_logger.error_report([
          {:ct_connection, r_connection(conn, :name)},
          {:client, self()},
          {:module, :ct_netconfc},
          {:line, 1646},
          errorT,
          {:expecting, pending}
        ])

        state
    end
  end

  defp do_decode_rpc_reply(op, result, state)
       when op == :lock or
              op == :unlock or op == :edit_config or
              op == :delete_config or
              op == :copy_config or op == :kill_session do
    {decode_ok(result), state}
  end

  defp do_decode_rpc_reply(op, result, state)
       when op == :get or
              op == :get_config or op == :action do
    {decode_data(result), state}
  end

  defp do_decode_rpc_reply(:close_session, result, state) do
    case decode_ok(result) do
      :ok ->
        {:ok, {:stop, state}}

      other ->
        {other, state}
    end
  end

  defp do_decode_rpc_reply({:create_subscription, from}, result, state) do
    case decode_ok(result) do
      :ok ->
        {:ok, r_state(state, event_receiver: from)}

      other ->
        {other, state}
    end
  end

  defp do_decode_rpc_reply(:get_event_streams, result, state) do
    {decode_streams(decode_data(result)), state}
  end

  defp do_decode_rpc_reply(:undefined, result, state) do
    {result, state}
  end

  defp decode_ok([{tag, attrs, content}]) do
    case get_local_name_atom(tag) do
      :ok ->
        :ok

      :"rpc-error" ->
        {:error, forward_xmlns_attr(attrs, content)}

      _Other ->
        {:error, {:unexpected_rpc_reply, [{tag, attrs, content}]}}
    end
  end

  defp decode_ok(other) do
    {:error, {:unexpected_rpc_reply, other}}
  end

  defp decode_data([{tag, attrs, content}]) do
    case get_local_name_atom(tag) do
      :ok ->
        :ok

      :data ->
        {:ok,
         forward_xmlns_attr(
           remove_xmlnsattr_for_tag(tag, attrs),
           content
         )}

      :"rpc-error" ->
        {:error, forward_xmlns_attr(attrs, content)}

      _Other ->
        {:error, {:unexpected_rpc_reply, [{tag, attrs, content}]}}
    end
  end

  defp decode_data(other) do
    {:error, {:unexpected_rpc_reply, other}}
  end

  defp get_qualified_name(tag) do
    case :string.lexemes(:erlang.atom_to_list(tag), ':') do
      [tagStr] ->
        {[], tagStr}

      [prefixStr, tagStr] ->
        {prefixStr, tagStr}
    end
  end

  defp get_local_name_atom(tag) do
    {_, tagStr} = get_qualified_name(tag)
    :erlang.list_to_atom(tagStr)
  end

  defp remove_xmlnsattr_for_tag(tag, attrs) do
    {prefix, _TagStr} = get_qualified_name(tag)
    xmlnsTag = xmlnstag(prefix)

    case :lists.keytake(xmlnsTag, 1, attrs) do
      {:value, _, noNsAttrs} ->
        noNsAttrs

      false ->
        attrs
    end
  end

  defp forward_xmlns_attr(parentAttrs, children) do
    do_forward_xmlns_attr(
      get_all_xmlns_attrs(
        parentAttrs,
        []
      ),
      children
    )
  end

  defp do_forward_xmlns_attr(xmlnsAttrs, [{chT, chA, chC} | children]) do
    chA1 = add_xmlns_attrs(xmlnsAttrs, chA)

    [
      {chT, chA1, chC}
      | do_forward_xmlns_attr(
          xmlnsAttrs,
          children
        )
    ]
  end

  defp do_forward_xmlns_attr(_XmlnsAttrs, []) do
    []
  end

  defp add_xmlns_attrs([{key, _} = a | xmlnsAttrs], chA) do
    case :lists.keymember(key, 1, chA) do
      true ->
        add_xmlns_attrs(xmlnsAttrs, chA)

      false ->
        add_xmlns_attrs(xmlnsAttrs, [a | chA])
    end
  end

  defp add_xmlns_attrs([], chA) do
    chA
  end

  defp get_all_xmlns_attrs([{:xmlns, _} = default | attrs], xmlnsAttrs) do
    get_all_xmlns_attrs(attrs, [default | xmlnsAttrs])
  end

  defp get_all_xmlns_attrs([{key, _} = attr | attrs], xmlnsAttrs) do
    case :erlang.atom_to_list(key) do
      'xmlns:' ++ _Prefix ->
        get_all_xmlns_attrs(attrs, [attr | xmlnsAttrs])

      _ ->
        get_all_xmlns_attrs(attrs, xmlnsAttrs)
    end
  end

  defp get_all_xmlns_attrs([], xmlnsAttrs) do
    xmlnsAttrs
  end

  defp decode_hello({:hello, _Attrs, hello}) do
    u = make_ref()

    try do
      [{:"session-id", _, [sessionId]}, _ | _] = [
        find(:"session-id", hello),
        :no_session_id_found | u
      ]

      [{:ok, id}, _ | _] = [
        try do
          {:ok, :erlang.list_to_integer(sessionId)}
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end,
        :invalid_session_id | u
      ]

      [true, _ | _] = [0 < id, :invalid_session_id | u]

      [
        {:capabilities, _, capabilities},
        _
        | _
      ] = [
        find(:capabilities, hello),
        :capabilities_not_found
        | u
      ]

      [{:ok, caps}, _ | _] = [decode_caps(capabilities, [], false), false | u]
      {:ok, id, caps}
    catch
      :error, {:badmatch, [error, false | ^u]} ->
        error

      :error, {:badmatch, [_, reason | ^u]} ->
        {:error, {:incorrect_hello, reason}}
    end
  end

  defp find(key, list) do
    :lists.keyfind(key, 1, list)
  end

  defp decode_caps(
         [{:capability, [], ['urn:ietf:params:netconf:base:' ++ _ = cap]} | caps],
         acc,
         _
       ) do
    decode_caps(caps, [cap | acc], true)
  end

  defp decode_caps([{:capability, [], [cap]} | caps], acc, base) do
    decode_caps(caps, [cap | acc], base)
  end

  defp decode_caps([h | _], _, _) do
    {:error, {:unexpected_capability_element, h}}
  end

  defp decode_caps([], _, false) do
    {:error, {:incorrect_hello, :no_base_capability_found}}
  end

  defp decode_caps([], acc, true) do
    {:ok, :lists.reverse(acc)}
  end

  defp decode_streams({:error, reason}) do
    {:error, reason}
  end

  defp decode_streams({:ok, [{:netconf, _, streams}]}) do
    {:ok, decode_streams(streams)}
  end

  defp decode_streams([{:streams, _, streams}]) do
    decode_streams(streams)
  end

  defp decode_streams([{:stream, _, stream} | streams]) do
    {:name, _, [name]} = find(:name, stream)

    [
      {name,
       for {tag, _, [value]} <- stream, tag != :name do
         {tag, value}
       end}
      | decode_streams(streams)
    ]
  end

  defp decode_streams([]) do
    []
  end

  defp log(connection, action) do
    log(connection, action, <<>>)
  end

  defp log(r_connection(reference: ref, host: host, port: port, name: name), action, data) do
    address =
      case ref do
        {_, ch} ->
          {host, port, ch}

        _ ->
          {host, port}
      end

    :error_logger.info_report(
      r_conn_log(
        client: self(),
        address: address,
        name: name,
        action: action,
        module: :ct_netconfc
      ),
      data
    )
  end

  def format_data(how, data) do
    do_format_data(how, :unicode.characters_to_binary(data))
  end

  defp do_format_data(:raw, data) do
    :io_lib.format('~n~ts~n', [hide_password(data)])
  end

  defp do_format_data(:pretty, data) do
    maybe_io_lib_format(indent(data))
  end

  defp do_format_data(:html, data) do
    maybe_io_lib_format(html_format(data))
  end

  defp maybe_io_lib_format(<<>>) do
    []
  end

  defp maybe_io_lib_format(string) do
    :io_lib.format('~n~ts~n', [string])
  end

  defp hide_password(bin) do
    :re.replace(bin, "(<password[^>]*>)[^<]*(</password>)", "\\1*****\\2", [
      :global,
      {:return, :binary},
      :unicode
    ])
  end

  defp html_format(bin) do
    :binary.replace(indent(bin), "<", "&lt;", [:global])
  end

  defp indent(bin) do
    string = normalize(hide_password(bin))

    indentedString =
      case :erlang.erase(:part_of_line) do
        :undefined ->
          indent1(string, [])

        part ->
          indent1(
            :lists.reverse(part) ++ string,
            :erlang.erase(:indent)
          )
      end

    :unicode.characters_to_binary(indentedString)
  end

  defp normalize(bin) do
    :re.replace(bin, ">[ \r\n\t]+<", "><", [:global, {:return, :list}, :unicode])
  end

  defp indent1('<?' ++ rest1, indent1) do
    {line, rest2, indent2} = indent_line(rest1, indent1, [??, ?<])
    line ++ indent1(rest2, indent2)
  end

  defp indent1('</' ++ rest1, indent1) do
    case indent_line1(rest1, indent1, [?/, ?<]) do
      {[], [], _} ->
        []

      {line, rest2, indent2} ->
        '\n' ++ line ++ indent1(rest2, indent2)
    end
  end

  defp indent1('<' ++ rest1, indent1) do
    :erlang.put(:tag, get_tag(rest1))

    case indent_line(rest1, indent1, [?<]) do
      {[], [], _} ->
        []

      {line, rest2, indent2} ->
        '\n' ++ line ++ indent1(rest2, indent2)
    end
  end

  defp indent1([h | t], indent) do
    [h | indent1(t, indent)]
  end

  defp indent1([], _Indent) do
    []
  end

  defp indent_line('?>' ++ rest, indent, line) do
    {:lists.reverse(line) ++ '?>', rest, indent}
  end

  defp indent_line('/></' ++ rest, indent, line) do
    {indent ++ :lists.reverse(line) ++ '/>', '</' ++ rest, indent -- '  '}
  end

  defp indent_line('/>' ++ rest, indent, line) do
    {indent ++ :lists.reverse(line) ++ '/>', rest, indent}
  end

  defp indent_line('></' ++ rest, indent, line) do
    lastTag = :erlang.erase(:tag)

    case get_tag(rest) do
      ^lastTag ->
        indent_line1(rest, indent, [?/, ?<, ?> | line])

      _ ->
        {indent ++ :lists.reverse(line) ++ '>', '</' ++ rest, indent -- '  '}
    end
  end

  defp indent_line('><' ++ rest, indent, line) do
    {indent ++ :lists.reverse(line) ++ '>', '<' ++ rest, '  ' ++ indent}
  end

  defp indent_line('</' ++ rest, indent, line) do
    indent_line1(rest, indent, [?/, ?< | line])
  end

  defp indent_line([h | t], indent, line) do
    indent_line(t, indent, [h | line])
  end

  defp indent_line([], indent, line) do
    :erlang.put(:part_of_line, line)
    :erlang.put(:indent, indent)
    {[], [], indent}
  end

  defp indent_line1('></' ++ rest, indent, line) do
    {indent ++ :lists.reverse(line) ++ '>', '</' ++ rest, indent -- '  '}
  end

  defp indent_line1('>' ++ rest, indent, line) do
    {indent ++ :lists.reverse(line) ++ '>', rest, indent}
  end

  defp indent_line1([h | t], indent, line) do
    indent_line1(t, indent, [h | line])
  end

  defp indent_line1([], indent, line) do
    :erlang.put(:part_of_line, line)
    :erlang.put(:indent, indent)
    {[], [], indent}
  end

  defp get_tag('/>' ++ _) do
    []
  end

  defp get_tag('>' ++ _) do
    []
  end

  defp get_tag([h | t]) do
    [h | get_tag(t)]
  end

  defp get_tag([]) do
    []
  end

  defp ssh_connect(
         r_options(host: host, timeout: timeout, port: port, ssh: sshOpts, name: name, type: type)
       ) do
    case :ssh.connect(
           host,
           port,
           [
             {:user_interaction, false},
             {:silently_accept_hosts, true}
             | sshOpts
           ],
           timeout
         ) do
      {:ok, cM} ->
        connection = r_connection(reference: cM, host: host, port: port, name: name, type: type)
        log(connection, :connect)
        {:ok, connection}

      {:error, reason} ->
        {:error, {:ssh, :could_not_connect_to_server, reason}}
    end
  end

  defp ssh_channel(
         r_connection(reference: cM) = connection0,
         r_options(timeout: timeout, name: name, type: type)
       ) do
    case :ssh_connection.session_channel(cM, timeout) do
      {:ok, ch} ->
        case :ssh_connection.subsystem(cM, ch, 'netconf', timeout) do
          :success ->
            connection = r_connection(connection0, reference: {cM, ch}, name: name, type: type)
            log(connection, :open)
            {:ok, connection}

          :failure ->
            :ssh_connection.close(cM, ch)
            {:error, {:ssh, :could_not_execute_netconf_subsystem}}

          {:error, :timeout} ->
            :ssh_connection.close(cM, ch)
            {:error, {:ssh, :could_not_execute_netconf_subsystem, :timeout}}
        end

      {:error, reason} ->
        {:error, {:ssh, :could_not_open_channel, reason}}
    end
  end

  defp ssh_open(options) do
    case ssh_connect(options) do
      {:ok, connection} ->
        case ssh_channel(connection, options) do
          {:ok, _} = ok ->
            ok

          error ->
            ssh_close(connection)
            error
        end

      error ->
        error
    end
  end

  defp ssh_send(r_connection(reference: {cM, ch}) = connection, data) do
    case :ssh_connection.send(cM, ch, data) do
      :ok ->
        log(connection, :send, data)
        :ok

      {:error, reason} ->
        {:error, {:ssh, :failed_to_send_data, reason}}
    end
  end

  defp ssh_close(
         connection =
           r_connection(
             reference: {cM, ch},
             type: type
           )
       ) do
    _ = :ssh_connection.close(cM, ch)
    log(connection, :close)

    case type do
      :connection_and_channel ->
        ssh_close(r_connection(connection, reference: cM))

      _ ->
        :ok
    end

    :ok
  end

  defp ssh_close(connection = r_connection(reference: cM)) do
    _ = :ssh.close(cM)
    log(connection, :disconnect)
    :ok
  end

  defp recv(bin, true) do
    recv(bin, [<<>>, 3])
  end

  defp recv(bin, false) do
    recv(bin, <<>>)
  end

  defp recv(bin, [head, len | chunks]) do
    chunk(<<head::binary, bin::binary>>, chunks, len)
  end

  defp recv(bin, head) when is_binary(head) do
    frame(
      <<head::binary, bin::binary>>,
      max(0, :erlang.size(head) - 5)
    )
  end

  defp frame(bin, start) do
    sz = :erlang.size(bin)
    scope = {start, sz - start}

    case :binary.match(bin, pattern(), [{:scope, scope}]) do
      {len, 6} ->
        <<msg::size(len)-binary, _::size(6)-binary, rest::binary>> = bin
        {trim(msg), rest}

      :nomatch ->
        bin
    end
  end

  defp pattern() do
    key = {:ct_netconfc, :pattern}

    case :erlang.get(key) do
      :undefined ->
        cP = :binary.compile_pattern("]]>]]>")
        :erlang.put(key, cP)
        cP

      cP ->
        cP
    end
  end

  defp trim(<<c, bin::binary>>)
       when c == ?\n or
              c == ?\r or c == ?\t or c == ?\s do
    trim(bin)
  end

  defp trim(bin) do
    bin
  end

  defp chunk(bin, [sz | chunks] = l, 0) do
    case bin do
      <<chunk::size(sz)-binary, rest::binary>> ->
        chunk(rest, acc(chunk, chunks), 3)

      _ ->
        [bin, 0 | l]
    end
  end

  defp chunk(bin, chunks, len) when :erlang.size(bin) < 4 do
    [bin, 3 = len | chunks]
  end

  defp chunk(<<"\n##\n", rest::binary>>, chunks, _) do
    case chunks do
      [] ->
        {:error, 'end-of-chunks unexpected'}

      bins ->
        {:lists.reverse(bins), rest}
    end
  end

  defp chunk(<<"\n#", head::size(1)-binary, ?\n, rest::binary>>, chunks, _) do
    acc(head, rest, chunks)
  end

  defp chunk(<<"\n#", head::size(2)-binary, ?\n, rest::binary>>, chunks, _) do
    acc(head, rest, chunks)
  end

  defp chunk(<<"\n#", head::size(3)-binary, ?\n, rest::binary>>, chunks, _) do
    acc(head, rest, chunks)
  end

  defp chunk(<<"\n#", head::size(4)-binary, ?\n, rest::binary>>, chunks, _) do
    acc(head, rest, chunks)
  end

  defp chunk(<<"\n#", head::size(5)-binary, ?\n, rest::binary>>, chunks, _) do
    acc(head, rest, chunks)
  end

  defp chunk(<<"\n#", head::size(6)-binary, ?\n, rest::binary>>, chunks, _) do
    acc(head, rest, chunks)
  end

  defp chunk(<<"\n#", head::size(7)-binary, ?\n, rest::binary>>, chunks, _) do
    acc(head, rest, chunks)
  end

  defp chunk(<<"\n#", head::size(8)-binary, ?\n, rest::binary>>, chunks, _) do
    acc(head, rest, chunks)
  end

  defp chunk(<<"\n#", head::size(9)-binary, ?\n, rest::binary>>, chunks, _) do
    acc(head, rest, chunks)
  end

  defp chunk(<<"\n#", head::size(10)-binary, ?\n, rest::binary>>, chunks, _) do
    acc(head, rest, chunks)
  end

  defp chunk(<<"\n#", bin::size(11)-binary, _::binary>>, _, _) do
    {:error, {'chunk-size too long', bin}}
  end

  defp chunk(<<"\n#", _::binary>> = bin, chunks, _) do
    [bin, :erlang.size(bin) | chunks]
  end

  defp chunk(bin, chunks, 3 = len) do
    case drop(bin) do
      <<>> ->
        [bin, len | chunks]

      <<"\n#", _::binary>> = b ->
        chunk(b, chunks, len)

      _ ->
        {:error, {'not a chunk', bin}}
    end
  end

  defp drop(<<"\n#", _::binary>> = bin) do
    bin
  end

  defp drop(<<c, bin::binary>>)
       when c == ?\n or
              c == ?\r or c == ?\t or c == ?\s do
    drop(bin)
  end

  defp drop(bin) do
    bin
  end

  defp acc(chunk, []) do
    for b <- [trim(chunk)], <<>> != b do
      b
    end
  end

  defp acc(chunk, chunks) do
    [chunk | chunks]
  end

  defp acc(head, rest, chunks) do
    case chunk_size(head) do
      {:error, _Reason} = no ->
        no

      sz ->
        chunk(rest, [sz | chunks], 0)
    end
  end

  defp chunk_size(<<c, _::binary>> = bin) do
    try do
      true = ?0 < c
      :erlang.binary_to_integer(bin)
    catch
      :error, _ ->
        {:error, {'chunk-size invalid', bin}}
    else
      sz when 0 < sz >>> 32 ->
        {:error, {'chunk-size too large', sz}}

      sz ->
        sz
    end
  end
end
