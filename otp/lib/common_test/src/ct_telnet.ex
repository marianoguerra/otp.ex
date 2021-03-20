defmodule :m_ct_telnet do
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

  Record.defrecord(:r_state, :state,
    host: :undefined,
    port: :undefined,
    teln_pid: :undefined,
    prx: :undefined,
    buffer: [],
    prompt: false,
    name: :undefined,
    type: :undefined,
    target_mod: :undefined,
    keep_alive: :undefined,
    poll_limit: 0,
    poll_interval: 1000,
    extra: :undefined,
    conn_to: 10000,
    com_to: 10000,
    reconns: 3,
    reconn_int: 5000,
    tcp_nodelay: false
  )

  def open(name) do
    open(name, :telnet)
  end

  def open(name, connType) do
    case :ct_util.get_key_from_name(name) do
      {:ok, :unix} ->
        open(name, connType, :unix_telnet, name)

      {:ok, key} ->
        open(name, connType, key, name)

      error ->
        error
    end
  end

  def open(keyOrName, connType, targetMod) do
    open(keyOrName, connType, targetMod, keyOrName)
  end

  def open(keyOrName, connType, targetMod, extra) do
    case :ct.get_config({keyOrName, connType}) do
      :undefined ->
        log(:undefined, :open, 'Failed: ~tp', [{:not_available, keyOrName}])
        {:error, {:not_available, keyOrName, connType}}

      addr ->
        addr1 =
          case addr do
            {_IP, _Port} ->
              addr

            iP ->
              case :ct.get_config({keyOrName, :port}) do
                :undefined ->
                  iP

                p ->
                  {iP, p}
              end
          end

        keepAlive =
          case :ct.get_config({keyOrName, :keep_alive}) do
            :undefined ->
              case :ct.get_config({:telnet_settings, :keep_alive}) do
                :undefined ->
                  true

                bool ->
                  bool
              end

            bool ->
              bool
          end

        log(:undefined, :open, 'Connecting to ~tp(~tp)', [keyOrName, addr1])

        reconnect =
          case :ct.get_config({:telnet_settings, :reconnection_attempts}) do
            0 ->
              false

            _ ->
              true
          end

        :ct_gen_conn.start(
          full_addr(addr1, connType),
          {targetMod, keepAlive, extra},
          :ct_telnet,
          [{:name, keyOrName}, {:reconnect, reconnect}, {:old, true}]
        )
    end
  end

  def close(connection) do
    case get_handle(connection) do
      {:ok, pid} ->
        log(:undefined, :close, 'Connection closed, handle: ~w', [pid])

        case :ct_gen_conn.stop(pid) do
          {:error, {:process_down, ^pid, _}} ->
            {:error, :already_closed}

          result ->
            result
        end

      error ->
        error
    end
  end

  def cmd(connection, cmd) do
    cmd(connection, cmd, [])
  end

  def cmd(connection, cmd, opts) when is_list(opts) do
    case check_cmd_opts(opts) do
      :ok ->
        case get_handle(connection) do
          {:ok, pid} ->
            call(pid, {:cmd, cmd, opts})

          error ->
            error
        end

      error ->
        error
    end
  end

  def cmd(connection, cmd, timeout)
      when is_integer(timeout) or timeout == :default do
    cmd(connection, cmd, [{:timeout, timeout}])
  end

  defp check_cmd_opts([{:timeout, timeout} | opts])
       when is_integer(timeout) or timeout == :default do
    check_cmd_opts(opts)
  end

  defp check_cmd_opts([]) do
    :ok
  end

  defp check_cmd_opts(opts) do
    check_send_opts(opts)
  end

  def cmdf(connection, cmdFormat, args) do
    cmdf(connection, cmdFormat, args, [])
  end

  def cmdf(connection, cmdFormat, args, opts)
      when is_list(args) do
    cmd = :lists.flatten(:io_lib.format(cmdFormat, args))
    cmd(connection, cmd, opts)
  end

  def get_data(connection) do
    case get_handle(connection) do
      {:ok, pid} ->
        call(pid, :get_data)

      error ->
        error
    end
  end

  def send(connection, cmd) do
    send(connection, cmd, [])
  end

  def send(connection, cmd, opts) do
    case check_send_opts(opts) do
      :ok ->
        case get_handle(connection) do
          {:ok, pid} ->
            call(pid, {:send, cmd, opts})

          error ->
            error
        end

      error ->
        error
    end
  end

  defp check_send_opts([{:newline, bool} | opts])
       when is_boolean(bool) do
    check_send_opts(opts)
  end

  defp check_send_opts([{:newline, string} | opts])
       when is_list(string) do
    case :lists.all(
           fn
             i
             when is_integer(i) and i >= 0 and
                    i <= 127 ->
               true

             _ ->
               false
           end,
           string
         ) do
      true ->
        check_send_opts(opts)

      false ->
        {:error, {:invalid_option, {:newline, string}}}
    end
  end

  defp check_send_opts([invalid | _]) do
    {:error, {:invalid_option, invalid}}
  end

  defp check_send_opts([]) do
    :ok
  end

  def sendf(connection, cmdFormat, args)
      when is_list(args) do
    sendf(connection, cmdFormat, args, [])
  end

  def sendf(connection, cmdFormat, args, opts)
      when is_list(args) do
    cmd = :lists.flatten(:io_lib.format(cmdFormat, args))
    send(connection, cmd, opts)
  end

  def expect(connection, patterns) do
    expect(connection, patterns, [])
  end

  def expect(connection, patterns, opts) do
    case get_handle(connection) do
      {:ok, pid} ->
        case call(pid, {:expect, patterns, opts}) do
          {:error, reason}
          when :erlang.element(
                 1,
                 reason
               ) == :bad_pattern ->
            exit({reason, {:ct_telnet, :expect, 3}})

          other ->
            other
        end

      error ->
        error
    end
  end

  def init(name, {ip, port, type}, {targetMod, keepAlive, extra}) do
    s0 =
      case :ct.get_config(:telnet_settings) do
        :undefined ->
          r_state()

        settings ->
          set_telnet_defaults(settings, r_state())
      end

    _ = :code.ensure_loaded(targetMod)

    try do
      case :erlang.function_exported(targetMod, :connect, 7) do
        true ->
          targetMod.connect(
            name,
            ip,
            port,
            r_state(s0, :conn_to),
            keepAlive,
            r_state(s0, :tcp_nodelay),
            extra
          )

        false ->
          targetMod.connect(name, ip, port, r_state(s0, :conn_to), keepAlive, extra)
      end
    catch
      _, reason ->
        {:error, reason}
    else
      {:ok, telnPid} ->
        :erlang.put({:ct_telnet_pid2name, telnPid}, name)

        s1 =
          r_state(s0,
            host: ip,
            port: port,
            teln_pid: telnPid,
            name: name,
            type: type(type),
            target_mod: targetMod,
            keep_alive: keepAlive,
            extra: extra,
            prx: targetMod.get_prompt_regexp()
          )

        log(
          s1,
          :open,
          'Opened telnet connection\nIP: ~p\nPort: ~p\nCommand timeout: ~p\nReconnection attempts: ~p\nReconnection interval: ~p\nConnection timeout: ~p\nKeep alive: ~w\nPoll limit: ~w\nPoll interval: ~w\nTCP nodelay: ~w',
          [
            ip,
            port,
            r_state(s1, :com_to),
            r_state(s1, :reconns),
            r_state(s1, :reconn_int),
            r_state(s1, :conn_to),
            keepAlive,
            r_state(s1, :poll_limit),
            r_state(s1, :poll_interval),
            r_state(s1, :tcp_nodelay)
          ]
        )

        {:ok, telnPid, s1}

      error ->
        error
    end
  end

  defp type(:telnet) do
    :ip
  end

  defp type(tS) when tS == :ts1 or tS == :ts2 do
    :ts
  end

  defp set_telnet_defaults([{:connect_timeout, cnTo} | ss], s) do
    set_telnet_defaults(ss, r_state(s, conn_to: cnTo))
  end

  defp set_telnet_defaults([{:command_timeout, cmTo} | ss], s) do
    set_telnet_defaults(ss, r_state(s, com_to: cmTo))
  end

  defp set_telnet_defaults([{:reconnection_attempts, rs} | ss], s) do
    set_telnet_defaults(ss, r_state(s, reconns: rs))
  end

  defp set_telnet_defaults([{:reconnection_interval, rInt} | ss], s) do
    set_telnet_defaults(ss, r_state(s, reconn_int: rInt))
  end

  defp set_telnet_defaults([{:keep_alive, _} | ss], s) do
    set_telnet_defaults(ss, s)
  end

  defp set_telnet_defaults([{:poll_limit, pL} | ss], s) do
    set_telnet_defaults(ss, r_state(s, poll_limit: pL))
  end

  defp set_telnet_defaults([{:poll_interval, pI} | ss], s) do
    set_telnet_defaults(ss, r_state(s, poll_interval: pI))
  end

  defp set_telnet_defaults([{:tcp_nodelay, noDelay} | ss], s) do
    set_telnet_defaults(ss, r_state(s, tcp_nodelay: noDelay))
  end

  defp set_telnet_defaults([unknown | ss], s) do
    force_log(s, :error, 'Bad element in telnet_settings: ~tp', [unknown])
    set_telnet_defaults(ss, s)
  end

  defp set_telnet_defaults([], s) do
    s
  end

  def handle_msg({:cmd, cmd, opts}, state) do
    start_gen_log(heading(:cmd, r_state(state, :name)))
    log(state, :cmd, 'Cmd: ~tp', [cmd])
    debug_cont_gen_log('Throwing Buffer:', [])
    debug_log_lines(r_state(state, :buffer))

    _ =
      case {r_state(state, :type), r_state(state, :prompt)} do
        {:ts, _} ->
          silent_teln_expect(
            r_state(state, :name),
            r_state(state, :teln_pid),
            r_state(state, :buffer),
            :prompt,
            r_state(state, :prx),
            [{:idle_timeout, 2000}]
          )

        {:ip, false} ->
          silent_teln_expect(
            r_state(state, :name),
            r_state(state, :teln_pid),
            r_state(state, :buffer),
            :prompt,
            r_state(state, :prx),
            [{:idle_timeout, 200}]
          )

        {:ip, true} ->
          :ok
      end

    tO =
      case :proplists.get_value(:timeout, opts, :default) do
        :default ->
          r_state(state, :com_to)

        timeout ->
          timeout
      end

    newline = :proplists.get_value(:newline, opts, true)

    {return, newBuffer, prompt} =
      case teln_cmd(r_state(state, :teln_pid), cmd, r_state(state, :prx), newline, tO) do
        {:ok, data, _PromptType, rest} ->
          log(state, :recv, 'Return: ~tp', [{:ok, data}])
          {{:ok, data}, rest, true}

        error ->
          retry =
            {:retry,
             {error, {r_state(state, :name), r_state(state, :type)}, r_state(state, :teln_pid),
              {:cmd, cmd, opts}}}

          log(state, :recv, 'Return: ~tp', [error])
          {retry, [], false}
      end

    end_gen_log()
    {return, r_state(state, buffer: newBuffer, prompt: prompt)}
  end

  def handle_msg({:send, cmd, opts}, state) do
    start_gen_log(heading(:send, r_state(state, :name)))
    log(state, :send, 'Sending: ~tp', [cmd])
    debug_cont_gen_log('Throwing Buffer:', [])
    debug_log_lines(r_state(state, :buffer))

    _ =
      case {r_state(state, :type), r_state(state, :prompt)} do
        {:ts, _} ->
          silent_teln_expect(
            r_state(state, :name),
            r_state(state, :teln_pid),
            r_state(state, :buffer),
            :prompt,
            r_state(state, :prx),
            [{:idle_timeout, 2000}]
          )

        {:ip, false} ->
          silent_teln_expect(
            r_state(state, :name),
            r_state(state, :teln_pid),
            r_state(state, :buffer),
            :prompt,
            r_state(state, :prx),
            [{:idle_timeout, 200}]
          )

        {:ip, true} ->
          :ok
      end

    newline = :proplists.get_value(:newline, opts, true)
    :ct_telnet_client.send_data(r_state(state, :teln_pid), cmd, newline)
    end_gen_log()
    {:ok, r_state(state, buffer: [], prompt: false)}
  end

  def handle_msg(:get_data, state) do
    start_gen_log(heading(:get_data, r_state(state, :name)))
    log(state, :cmd, 'Reading data...', [])

    {:ok, data, buffer} =
      teln_get_all_data(state, r_state(state, :buffer), [], [], r_state(state, :poll_limit))

    log(state, :recv, 'Return: ~tp', [{:ok, data}])
    end_gen_log()
    {{:ok, data}, r_state(state, buffer: buffer)}
  end

  def handle_msg({:expect, pattern, opts}, state) do
    start_gen_log(heading(:expect, r_state(state, :name)))
    log(state, :expect, 'Expect: ~tp\nOpts = ~tp\n', [pattern, opts])

    {return, newBuffer, prompt} =
      case teln_expect(
             r_state(state, :name),
             r_state(state, :teln_pid),
             r_state(state, :buffer),
             pattern,
             r_state(state, :prx),
             opts
           ) do
        {:ok, data, rest} ->
          p = check_if_prompt_was_reached(data, [])
          {{:ok, data}, rest, p}

        {:ok, data, haltReason, rest} ->
          force_log(state, :expect, 'HaltReason: ~tp', [haltReason])
          p = check_if_prompt_was_reached(data, haltReason)
          {{:ok, data, haltReason}, rest, p}

        {:error, reason, rest} ->
          force_log(state, :expect, 'Expect failed\n~tp', [{:error, reason}])
          p = check_if_prompt_was_reached([], reason)
          {{:error, reason}, rest, p}

        {:error, reason} ->
          force_log(state, :expect, 'Expect failed\n~tp', [{:error, reason}])
          p = check_if_prompt_was_reached([], reason)
          {{:error, reason}, [], p}
      end

    end_gen_log()

    return1 =
      case return do
        {:error, _} ->
          {:retry,
           {return, {r_state(state, :name), r_state(state, :type)}, r_state(state, :teln_pid),
            {:expect, pattern, opts}}}

        _ ->
          return
      end

    {return1, r_state(state, buffer: newBuffer, prompt: prompt)}
  end

  def reconnect({ip, port, _Type}, state) do
    reconnect(ip, port, r_state(state, :reconns), state)
  end

  defp reconnect(
         ip,
         port,
         n,
         state =
           r_state(
             name: name,
             target_mod: targetMod,
             keep_alive: keepAlive,
             extra: extra,
             conn_to: connTo,
             reconn_int: reconnInt,
             tcp_nodelay: noDelay
           )
       ) do
    connResult =
      case :erlang.function_exported(targetMod, :connect, 7) do
        true ->
          targetMod.connect(name, ip, port, connTo, keepAlive, noDelay, extra)

        false ->
          targetMod.connect(name, ip, port, connTo, keepAlive, extra)
      end

    case connResult do
      {:ok, newPid} ->
        :erlang.put({:ct_telnet_pid2name, newPid}, name)
        {:ok, newPid, r_state(state, teln_pid: newPid)}

      error when n == 0 ->
        error

      _Error ->
        log(state, :reconnect, 'Reconnect failed!', 'Retries left: ~w', [n])
        :timer.sleep(reconnInt)
        reconnect(ip, port, n - 1, state)
    end
  end

  def terminate(telnPid, state) do
    result = :ct_telnet_client.close(telnPid)
    log(state, :close, 'Telnet connection for ~w closed.', [telnPid])
    result
  end

  defp get_handle(pid) when is_pid(pid) do
    {:ok, pid}
  end

  defp get_handle({name, type})
       when type == :telnet or
              type == :ts1 or type == :ts2 do
    case :ct_util.get_connection(name, :ct_telnet) do
      {:ok, conn} ->
        case get_handle(type, conn) do
          {:ok, pid} ->
            {:ok, pid}

          _Error ->
            case :ct_util.get_key_from_name(name) do
              {:ok, :node} ->
                open(name, type, :ct_telnet_cello_node)

              {:ok, :unix} ->
                open(name, type, :unix_telnet, name)

              {:ok, key} ->
                open(name, type, key, name)

              error ->
                error
            end
        end

      error ->
        error
    end
  end

  defp get_handle(name) do
    get_handle({name, :telnet})
  end

  defp get_handle(type, {pid, {_, _, type}}) do
    {:ok, pid}
  end

  defp get_handle(type, _) do
    {:error, {:no_such_connection, type}}
  end

  defp full_addr({ip, port}, type) do
    {ip, port, type}
  end

  defp full_addr(ip, type) do
    {ip, 23, type}
  end

  defp call(pid, msg) do
    :ct_gen_conn.call(pid, msg)
  end

  defp check_if_prompt_was_reached({:prompt, _}, _) do
    true
  end

  defp check_if_prompt_was_reached(_, {:prompt, _}) do
    true
  end

  defp check_if_prompt_was_reached(data, _) when is_list(data) do
    :lists.keymember(:prompt, 1, data)
  end

  defp check_if_prompt_was_reached(_, _) do
    false
  end

  defp heading(action, :undefined) do
    :io_lib.format('~w ~w', [:ct_telnet, action])
  end

  defp heading(action, name) do
    :io_lib.format('~w ~w for ~tp', [:ct_telnet, action, name])
  end

  defp force_log(state, action, string, args) do
    log(state, action, string, args, true)
  end

  def log(state, action, string, args)
      when elem(state, 0) === :state do
    log(state, action, string, args, false)
  end

  def log(name, action, string, args) when is_atom(name) do
    log(r_state(name: name), action, string, args, false)
  end

  def log(telnPid, action, string, args)
      when is_pid(telnPid) do
    log(r_state(teln_pid: telnPid), action, string, args, false)
  end

  def log(:undefined, string, args) do
    log(r_state(), :undefined, string, args, false)
  end

  def log(name, string, args) when is_atom(name) do
    log(r_state(name: name), :undefined, string, args, false)
  end

  def log(telnPid, string, args) when is_pid(telnPid) do
    log(r_state(teln_pid: telnPid), :undefined, string, args)
  end

  defp log(
         r_state(name: name, teln_pid: telnPid, host: host, port: port),
         action,
         string,
         args,
         forcePrint
       ) do
    name1 =
      cond do
        name == :undefined ->
          :erlang.get({:ct_telnet_pid2name, telnPid})

        true ->
          name
      end

    silent = :erlang.get(:silent)

    cond do
      action == :general_io ->
        case :ct_util.get_testdata({:cth_conn_log, :ct_telnet}) do
          hookMode
          when hookMode != :undefined and
                 hookMode != :silent and silent != true ->
            :error_logger.info_report(
              r_conn_log(
                header: false,
                client: self(),
                conn_pid: telnPid,
                address: {host, port},
                name: name1,
                action: action,
                module: :ct_telnet
              ),
              {string, args}
            )

          _ ->
            :ok
        end

      true ->
        cond do
          action == :open or action == :close or
            action == :reconnect or action == :info or
              action == :error ->
            :ct_gen_conn.log(heading(action, name1), string, args)

          forcePrint == false ->
            case :ct_util.is_silenced(:telnet) do
              true ->
                :ok

              false ->
                :ct_gen_conn.cont_log_no_timestamp(string, args)
            end

          forcePrint == true ->
            case :ct_util.is_silenced(:telnet) do
              true ->
                :ct_gen_conn.log(heading(action, name1), string, args)

              false ->
                :ct_gen_conn.cont_log_no_timestamp(string, args)
            end
        end
    end
  end

  def start_gen_log(heading) do
    case :ct_util.is_silenced(:telnet) do
      true ->
        :ok

      false ->
        :ct_gen_conn.start_log(heading)
    end
  end

  def end_gen_log() do
    case :ct_util.is_silenced(:telnet) do
      true ->
        :ok

      false ->
        :ct_gen_conn.end_log()
    end
  end

  defp debug_cont_gen_log(str, args) do
    old = :erlang.put(:silent, true)
    :ct_gen_conn.cont_log(str, args)
    :erlang.put(:silent, old)
  end

  def format_data(_How, {string, args}) do
    :io_lib.format(string, args)
  end

  defp teln_cmd(pid, cmd, prx, newline, timeout) do
    :ct_telnet_client.send_data(pid, cmd, newline)
    teln_receive_until_prompt(pid, prx, timeout)
  end

  defp teln_get_all_data(state = r_state(teln_pid: pid, prx: prx), data, acc, lastLine, polls) do
    case check_for_prompt(prx, lastLine ++ data) do
      {:prompt, lines, _PromptType, rest} ->
        teln_get_all_data(state, rest, [lines | acc], [], r_state(state, :poll_limit))

      {:noprompt, lines, lastLine1} ->
        case :ct_telnet_client.get_data(pid) do
          {:ok, []} when lastLine1 != [] and polls > 0 ->
            :timer.sleep(r_state(state, :poll_interval))

            newPolls =
              cond do
                polls == :infinity ->
                  :infinity

                true ->
                  polls - 1
              end

            teln_get_all_data(state, [], [lines | acc], lastLine1, newPolls)

          {:ok, []} ->
            {:ok, :lists.reverse(:lists.append([lines | acc])), lastLine1}

          {:ok, data1} ->
            teln_get_all_data(state, data1, [lines | acc], lastLine1, r_state(state, :poll_limit))
        end
    end
  end

  Record.defrecord(:r_eo, :eo,
    teln_pid: :undefined,
    prx: :undefined,
    idle_timeout: :undefined,
    total_timeout: :undefined,
    haltpatterns: [],
    seq: false,
    repeat: false,
    found_prompt: false,
    prompt_check: true
  )

  def silent_teln_expect(name, pid, data, pattern, prx, opts) do
    old = :erlang.put(:silent, true)
    result = teln_expect(name, pid, data, pattern, prx, opts)
    :erlang.put(:silent, old)
    result
  end

  defp teln_expect(name, pid, data, pattern0, prx, opts) do
    haltPatterns0 =
      case get_ignore_prompt(opts) do
        true ->
          get_haltpatterns(opts)

        false ->
          [:prompt | get_haltpatterns(opts)]
      end

    case convert_pattern(haltPatterns0, false) do
      {:ok, haltPatterns} ->
        {waitForPrompt, pattern1, opts1} = wait_for_prompt(pattern0, opts)
        seq = get_seq(opts1)

        case convert_pattern(pattern1, seq) do
          {:ok, pattern2} ->
            {idleTimeout, totalTimeout} = get_timeouts(opts1)
            promptCheck = get_prompt_check(opts1)

            eO =
              r_eo(
                teln_pid: pid,
                prx: prx,
                idle_timeout: idleTimeout,
                total_timeout: totalTimeout,
                seq: seq,
                haltpatterns: haltPatterns,
                prompt_check: promptCheck
              )

            case get_repeat(opts1) do
              false ->
                case teln_expect1(name, pid, data, pattern2, [], eO) do
                  {:ok, matched, rest} when waitForPrompt ->
                    case :lists.reverse(matched) do
                      [{:prompt, _}, matched1] ->
                        {:ok, matched1, rest}

                      [{:prompt, _} | matched1] ->
                        {:ok, :lists.reverse(matched1), rest}
                    end

                  {:ok, matched, rest} ->
                    {:ok, matched, rest}

                  {:halt, why, rest} ->
                    {:error, why, rest}

                  {:error, reason} ->
                    {:error, reason}
                end

              n ->
                eO1 = r_eo(eO, repeat: n)
                repeat_expect(name, pid, data, pattern2, [], eO1)
            end

          error ->
            error
        end

      error ->
        error
    end
  end

  defp convert_pattern(pattern0, seq)
       when pattern0 == [] or (is_list(pattern0) and not is_integer(hd(pattern0))) do
    pattern =
      case seq do
        true ->
          pattern0

        false ->
          rm_dupl(pattern0, [])
      end

    compile_pattern(pattern, [])
  end

  defp convert_pattern(pattern, _Seq) do
    compile_pattern([pattern], [])
  end

  defp rm_dupl([p | ps], acc) do
    case :lists.member(p, acc) do
      true ->
        rm_dupl(ps, acc)

      false ->
        rm_dupl(ps, [p | acc])
    end
  end

  defp rm_dupl([], acc) do
    :lists.reverse(acc)
  end

  defp compile_pattern([:prompt | patterns], acc) do
    compile_pattern(patterns, [:prompt | acc])
  end

  defp compile_pattern([{:prompt, _} = p | patterns], acc) do
    compile_pattern(patterns, [p | acc])
  end

  defp compile_pattern([{tag, pattern} | patterns], acc) do
    try do
      :re.compile(pattern, [:unicode])
    catch
      :error, :badarg ->
        {:error, {:bad_pattern, {tag, pattern}}}
    else
      {:ok, mP} ->
        compile_pattern(patterns, [{tag, mP} | acc])

      {:error, error} ->
        {:error, {:bad_pattern, {tag, pattern}, error}}
    end
  end

  defp compile_pattern([pattern | patterns], acc) do
    try do
      :re.compile(pattern, [:unicode])
    catch
      :error, :badarg ->
        {:error, {:bad_pattern, pattern}}
    else
      {:ok, mP} ->
        compile_pattern(patterns, [mP | acc])

      {:error, error} ->
        {:error, {:bad_pattern, pattern, error}}
    end
  end

  defp compile_pattern([], acc) do
    {:ok, :lists.reverse(acc)}
  end

  defp get_timeouts(opts) do
    {case :lists.keysearch(:idle_timeout, 1, opts) do
       {:value, {_, t}} ->
         t

       false ->
         case :lists.keysearch(:timeout, 1, opts) do
           {:value, {_, t}} ->
             t

           false ->
             10000
         end
     end,
     case :lists.keysearch(:total_timeout, 1, opts) do
       {:value, {_, t}} ->
         t

       false ->
         :infinity
     end}
  end

  defp get_repeat(opts) do
    case :lists.keysearch(:repeat, 1, opts) do
      {:value, {:repeat, n}} when is_integer(n) ->
        n

      false ->
        case :lists.member(:repeat, opts) do
          true ->
            -1

          false ->
            false
        end
    end
  end

  defp get_seq(opts) do
    :lists.member(:sequence, opts)
  end

  defp get_haltpatterns(opts) do
    case :lists.keysearch(:halt, 1, opts) do
      {:value, {:halt, haltPatterns}} ->
        haltPatterns

      false ->
        []
    end
  end

  defp get_ignore_prompt(opts) do
    :lists.member(:ignore_prompt, opts)
  end

  defp get_prompt_check(opts) do
    not :lists.member(:no_prompt_check, opts)
  end

  defp wait_for_prompt(pattern, opts) do
    case :lists.member(:wait_for_prompt, opts) do
      true ->
        wait_for_prompt1(:prompt, pattern, :lists.delete(:wait_for_prompt, opts))

      false ->
        case :proplists.get_value(:wait_for_prompt, opts) do
          :undefined ->
            {false, pattern, opts}

          promptStr ->
            wait_for_prompt1(
              {:prompt, promptStr},
              pattern,
              :proplists.delete(:wait_for_prompt, opts)
            )
        end
    end
  end

  defp wait_for_prompt1(prompt, [ch | _] = pattern, opts)
       when is_integer(ch) do
    wait_for_prompt2(prompt, [pattern], opts)
  end

  defp wait_for_prompt1(prompt, pattern, opts) when is_list(pattern) do
    wait_for_prompt2(prompt, pattern, opts)
  end

  defp wait_for_prompt1(prompt, pattern, opts) do
    wait_for_prompt2(prompt, [pattern], opts)
  end

  defp wait_for_prompt2(prompt, pattern, opts) do
    pattern1 =
      case :lists.reverse(pattern) do
        [:prompt | _] ->
          pattern

        [{:prompt, _} | _] ->
          pattern

        _ ->
          pattern ++ [prompt]
      end

    opts1 =
      case :lists.member(:sequence, opts) do
        true ->
          opts

        false ->
          [:sequence | opts]
      end

    {true, pattern1, opts1}
  end

  defp repeat_expect(_Name, _Pid, rest, _Pattern, acc, r_eo(repeat: 0)) do
    {:ok, :lists.reverse(acc), :done, rest}
  end

  defp repeat_expect(name, pid, data, pattern, acc, eO) do
    case teln_expect1(name, pid, data, pattern, [], eO) do
      {:ok, matched, rest} ->
        eO1 = r_eo(eO, repeat: r_eo(eO, :repeat) - 1)
        repeat_expect(name, pid, rest, pattern, [matched | acc], eO1)

      {:halt, why, rest} ->
        {:ok, :lists.reverse(acc), why, rest}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp teln_expect1(
         name,
         pid,
         data,
         pattern,
         acc,
         eO = r_eo(idle_timeout: idleTO, total_timeout: totalTO)
       ) do
    eOMod =
      cond do
        totalTO != :infinity ->
          r_eo(eO, total_timeout: trunc(totalTO))

        true ->
          eO
      end

    expectFun =
      case r_eo(eOMod, :seq) do
        true ->
          fn ->
            seq_expect(name, pid, data, pattern, acc, eOMod)
          end

        false ->
          fn ->
            one_expect(name, pid, data, pattern, eOMod)
          end
      end

    case expectFun.() do
      {:match, match, rest} ->
        {:ok, match, rest}

      {:halt, why, rest} ->
        {:halt, why, rest}

      notFinished ->
        fun = fn ->
          get_data1(r_eo(eOMod, :teln_pid))
        end

        breakAfter =
          cond do
            totalTO < idleTO ->
              r_eo(eOMod, :total_timeout)

            true ->
              idleTO
          end

        {patOrPats1, acc1, rest1} =
          case notFinished do
            {:nomatch, rest0} ->
              {pattern, [], rest0}

            {:continue, pats0, acc0, rest0} ->
              {pats0, acc0, rest0}
          end

        case :timer.tc(:ct_gen_conn, :do_within_time, [fun, breakAfter]) do
          {_, {:error, reason}} ->
            cond do
              rest1 != [] ->
                log(name_or_pid(name, pid), '       ~ts', [rest1])

              true ->
                :ok
            end

            {:error, reason}

          {_, {:ok, data1}} when totalTO == :infinity ->
            teln_expect1(name, pid, rest1 ++ data1, patOrPats1, acc1, eOMod)

          {elapsed, {:ok, data1}} ->
            tVal = totalTO - elapsed / 1000

            cond do
              tVal <= 0 ->
                {:error, :timeout}

              true ->
                eO1 = r_eo(eO, total_timeout: tVal)
                teln_expect1(name, pid, rest1 ++ data1, patOrPats1, acc1, eO1)
            end
        end
    end
  end

  defp get_data1(pid) do
    case :ct_telnet_client.get_data(pid) do
      {:ok, []} ->
        get_data1(pid)

      {:ok, data} ->
        {:ok, data}
    end
  end

  defp one_expect(name, pid, data, pattern, eO)
       when r_eo(eO, :prompt_check) == false do
    one_expect1(name, pid, data, pattern, [], r_eo(eO, found_prompt: false))
  end

  defp one_expect(name, pid, data, pattern, eO) do
    case match_prompt(data, r_eo(eO, :prx)) do
      {:prompt, uptoPrompt, promptType, rest} ->
        case pattern do
          [prompt]
          when prompt == :prompt or
                 prompt == {:prompt, promptType} ->
            log_lines(name, pid, uptoPrompt)
            log(name_or_pid(name, pid), 'PROMPT: ~ts', [promptType])
            {:match, {:prompt, promptType}, rest}

          [{:prompt, _OtherPromptType}] ->
            log_lines(name, pid, uptoPrompt)
            {:nomatch, rest}

          _ ->
            one_expect1(name, pid, uptoPrompt, pattern, rest, r_eo(eO, found_prompt: promptType))
        end

      :noprompt ->
        case pattern do
          [prompt]
          when prompt == :prompt or
                 :erlang.element(1, prompt) == :prompt ->
            lastLine = log_lines_not_last(name, pid, data)
            {:nomatch, lastLine}

          _ ->
            one_expect1(name, pid, data, pattern, [], r_eo(eO, found_prompt: false))
        end
    end
  end

  defp one_expect1(name, pid, data, pattern, rest, eO) do
    case match_lines(name, pid, data, pattern, eO) do
      {:match, match, matchRest} ->
        {:match, match, matchRest ++ rest}

      {:nomatch, :prompt} ->
        one_expect(name, pid, rest, pattern, eO)

      {:nomatch, noMatchRest} ->
        {:nomatch, noMatchRest ++ rest}

      {:halt, why, haltRest} ->
        {:halt, why, haltRest ++ rest}
    end
  end

  defp seq_expect(_Name, _Pid, data, [], acc, _EO) do
    {:match, :lists.reverse(acc), data}
  end

  defp seq_expect(_Name, _Pid, [], patterns, acc, _EO) do
    {:continue, patterns, :lists.reverse(acc), []}
  end

  defp seq_expect(name, pid, data, patterns, acc, eO)
       when r_eo(eO, :prompt_check) == false do
    seq_expect1(name, pid, data, patterns, acc, [], r_eo(eO, found_prompt: false))
  end

  defp seq_expect(name, pid, data, patterns, acc, eO) do
    case match_prompt(data, r_eo(eO, :prx)) do
      {:prompt, uptoPrompt, promptType, rest} ->
        seq_expect1(
          name,
          pid,
          uptoPrompt,
          patterns,
          acc,
          rest,
          r_eo(eO, found_prompt: promptType)
        )

      :noprompt ->
        seq_expect1(name, pid, data, patterns, acc, [], r_eo(eO, found_prompt: false))
    end
  end

  defp seq_expect1(name, pid, data, [:prompt | patterns], acc, rest, eO) do
    case r_eo(eO, :found_prompt) do
      false ->
        lastLine = log_lines_not_last(name, pid, data)
        {:continue, [:prompt | patterns], acc, lastLine}

      promptType ->
        log_lines(name, pid, data)
        log(name_or_pid(name, pid), 'PROMPT: ~ts', [promptType])
        seq_expect(name, pid, rest, patterns, [{:prompt, promptType} | acc], eO)
    end
  end

  defp seq_expect1(name, pid, data, [{:prompt, promptType} | patterns], acc, rest, eO) do
    case r_eo(eO, :found_prompt) do
      false ->
        lastLine = log_lines_not_last(name, pid, data)
        {:continue, [{:prompt, promptType} | patterns], acc, lastLine}

      ^promptType ->
        log_lines(name, pid, data)
        log(name_or_pid(name, pid), 'PROMPT: ~ts', [promptType])
        seq_expect(name, pid, rest, patterns, [{:prompt, promptType} | acc], eO)

      _OtherPromptType ->
        log_lines(name, pid, data)
        seq_expect(name, pid, rest, [{:prompt, promptType} | patterns], acc, eO)
    end
  end

  defp seq_expect1(name, pid, data, [pattern | patterns], acc, rest, eO) do
    case match_lines(name, pid, data, [pattern], eO) do
      {:match, match, matchRest} ->
        seq_expect1(name, pid, matchRest, patterns, [match | acc], rest, eO)

      {:nomatch, :prompt} ->
        seq_expect(name, pid, rest, [pattern | patterns], acc, eO)

      {:nomatch, noMatchRest} when rest == [] ->
        {:continue, [pattern | patterns], acc, noMatchRest}

      {:halt, why, haltRest} ->
        {:halt, why, haltRest ++ rest}
    end
  end

  defp seq_expect1(_Name, _Pid, data, [], acc, rest, _EO) do
    {:match, :lists.reverse(acc), data ++ rest}
  end

  defp match_lines(name, pid, data, patterns, eO) do
    foundPrompt = r_eo(eO, :found_prompt)

    case one_line(data, []) do
      {:noline, rest} when foundPrompt !== false ->
        case match_line(name, pid, rest, patterns, foundPrompt, false, eO) do
          :nomatch ->
            {:nomatch, :prompt}

          {tag, match} ->
            {tag, match, []}
        end

      {:noline, rest} when r_eo(eO, :prompt_check) == false ->
        case match_line(name, pid, rest, patterns, false, false, eO) do
          :nomatch ->
            {:nomatch, rest}

          {tag, match} ->
            {tag, match, []}
        end

      {:noline, rest} ->
        {:nomatch, rest}

      {line, rest} ->
        case match_line(name, pid, line, patterns, false, true, eO) do
          :nomatch ->
            match_lines(name, pid, rest, patterns, eO)

          {tag, match} ->
            {tag, match, rest}
        end
    end
  end

  defp match_line(name, pid, line, patterns, foundPrompt, terminated, eO) do
    match_line(name, pid, line, patterns, foundPrompt, terminated, eO, :match)
  end

  defp match_line(name, pid, line, [:prompt | patterns], false, term, eO, retTag) do
    match_line(name, pid, line, patterns, false, term, eO, retTag)
  end

  defp match_line(name, pid, line, [:prompt | _Patterns], foundPrompt, _Term, _EO, retTag) do
    log(name_or_pid(name, pid), '       ~ts', [line])
    log(name_or_pid(name, pid), 'PROMPT: ~ts', [foundPrompt])
    {retTag, {:prompt, foundPrompt}}
  end

  defp match_line(
         name,
         pid,
         line,
         [{:prompt, promptType} | _Patterns],
         foundPrompt,
         _Term,
         _EO,
         retTag
       )
       when promptType == foundPrompt do
    log(name_or_pid(name, pid), '       ~ts', [line])
    log(name_or_pid(name, pid), 'PROMPT: ~ts', [foundPrompt])
    {retTag, {:prompt, foundPrompt}}
  end

  defp match_line(
         name,
         pid,
         line,
         [{:prompt, promptType} | patterns],
         foundPrompt,
         term,
         eO,
         retTag
       )
       when promptType !== foundPrompt do
    match_line(name, pid, line, patterns, foundPrompt, term, eO, retTag)
  end

  defp match_line(name, pid, line, [{tag, pattern} | patterns], foundPrompt, term, eO, retTag) do
    case :re.run(line, pattern, [{:capture, :all, :list}]) do
      :nomatch ->
        match_line(name, pid, line, patterns, foundPrompt, term, eO, retTag)

      {:match, match} ->
        log(name_or_pid(name, pid), 'MATCH: ~ts', [line])
        {retTag, {tag, match}}
    end
  end

  defp match_line(name, pid, line, [pattern | patterns], foundPrompt, term, eO, retTag) do
    case :re.run(line, pattern, [{:capture, :all, :list}]) do
      :nomatch ->
        match_line(name, pid, line, patterns, foundPrompt, term, eO, retTag)

      {:match, match} ->
        log(name_or_pid(name, pid), 'MATCH: ~ts', [line])
        {retTag, match}
    end
  end

  defp match_line(name, pid, line, [], foundPrompt, term, eO, :match) do
    match_line(name, pid, line, r_eo(eO, :haltpatterns), foundPrompt, term, eO, :halt)
  end

  defp match_line(name, pid, line, [], _FoundPrompt, true, _EO, :halt) do
    log(name_or_pid(name, pid), '       ~ts', [line])
    :nomatch
  end

  defp match_line(_Name, _Pid, _Line, [], _FoundPrompt, false, _EO, :halt) do
    :nomatch
  end

  defp one_line([?\n | rest], line) do
    {:lists.reverse(line), rest}
  end

  defp one_line([?\r | rest], line) do
    one_line(rest, line)
  end

  defp one_line([0 | rest], line) do
    one_line(rest, line)
  end

  defp one_line([char | rest], line) do
    one_line(rest, [char | line])
  end

  defp one_line([], line) do
    {:noline, :lists.reverse(line)}
  end

  defp debug_log_lines(string) do
    old = :erlang.put(:silent, true)
    log_lines(:undefined, :undefined, string)
    :erlang.put(:silent, old)
  end

  defp log_lines(name, pid, string) do
    case log_lines_not_last(name, pid, string) do
      [] ->
        :ok

      lastLine ->
        log(name_or_pid(name, pid), '       ~ts', [lastLine])
    end
  end

  defp log_lines_not_last(name, pid, string) do
    case add_tabs(string, [], []) do
      {[], lastLine} ->
        lastLine

      {string1, lastLine} ->
        log(name_or_pid(name, pid), '~ts', [string1])
        lastLine
    end
  end

  defp name_or_pid(:undefined, pid) do
    pid
  end

  defp name_or_pid(name, _) do
    name
  end

  defp add_tabs([0 | rest], acc, lastLine) do
    add_tabs(rest, acc, lastLine)
  end

  defp add_tabs([?\r | rest], acc, lastLine) do
    add_tabs(rest, acc, lastLine)
  end

  defp add_tabs([?\n | rest], acc, lastLine) do
    add_tabs(
      rest,
      [?\n | lastLine] ++
        [
          ?\s,
          ?\s,
          ?\s,
          ?\s,
          ?\s,
          ?\s,
          ?\s
          | acc
        ],
      []
    )
  end

  defp add_tabs([ch | rest], acc, lastLine) do
    add_tabs(rest, acc, [ch | lastLine])
  end

  defp add_tabs([], [?\n | acc], lastLine) do
    {:lists.reverse(acc), :lists.reverse(lastLine)}
  end

  defp add_tabs([], [], lastLine) do
    {[], :lists.reverse(lastLine)}
  end

  def teln_receive_until_prompt(pid, prx, timeout) do
    fun = fn ->
      teln_receive_until_prompt(pid, prx, [], [])
    end

    :ct_gen_conn.do_within_time(fun, timeout)
  end

  defp teln_receive_until_prompt(pid, prx, acc, lastLine) do
    {:ok, data} = :ct_telnet_client.get_data(pid)

    case check_for_prompt(prx, lastLine ++ data) do
      {:prompt, lines, promptType, rest} ->
        return = :lists.reverse(:lists.append([lines | acc]))
        {:ok, return, promptType, rest}

      {:noprompt, lines, lastLine1} ->
        teln_receive_until_prompt(pid, prx, [lines | acc], lastLine1)
    end
  end

  defp check_for_prompt(prx, data) do
    case match_prompt(data, prx) do
      {:prompt, uptoPrompt, promptType, rest} ->
        {revLines, lastLine} = split_lines(uptoPrompt)
        {:prompt, [lastLine | revLines], promptType, rest}

      :noprompt ->
        {revLines, rest} = split_lines(data)
        {:noprompt, revLines, rest}
    end
  end

  defp split_lines(string) do
    split_lines(string, [], [])
  end

  defp split_lines([?\n | rest], line, lines) when line != [] do
    split_lines(rest, [], [:lists.reverse(line) | lines])
  end

  defp split_lines([?\n | rest], [], lines) do
    split_lines(rest, [], lines)
  end

  defp split_lines([?\r | rest], line, lines) do
    split_lines(rest, line, lines)
  end

  defp split_lines([0 | rest], line, lines) do
    split_lines(rest, line, lines)
  end

  defp split_lines([char | rest], line, lines) do
    split_lines(rest, [char | line], lines)
  end

  defp split_lines([], line, lines) do
    {lines, :lists.reverse(line)}
  end

  defp match_prompt(str, prx) do
    match_prompt(str, prx, [])
  end

  defp match_prompt(str, prx, acc) do
    case :re.run(str, prx, [:unicode]) do
      :nomatch ->
        :noprompt

      {:match, [{start, len}]} ->
        case split_prompt_string(str, start + 1, start + len, 1, [], []) do
          {:noprompt, done, rest} ->
            match_prompt(rest, prx, done)

          {:prompt, uptoPrompt, prompt, rest} ->
            {:prompt, :lists.reverse(uptoPrompt ++ acc), :lists.reverse(prompt), rest}
        end
    end
  end

  defp split_prompt_string([ch | str], start, end__, n, uptoPrompt, prompt)
       when n < start do
    split_prompt_string(str, start, end__, n + 1, [ch | uptoPrompt], prompt)
  end

  defp split_prompt_string([ch | str], start, end__, n, uptoPrompt, prompt)
       when n >= start and n < end__ do
    split_prompt_string(str, start, end__, n + 1, uptoPrompt, [ch | prompt])
  end

  defp split_prompt_string([ch | rest], _Start, end__, n, uptoPrompt, prompt)
       when n == end__ do
    case uptoPrompt do
      [?", ?=, ?T, ?P, ?M, ?O, ?R, ?P | _] ->
        {:noprompt, [ch | prompt] ++ uptoPrompt, rest}

      [?\s, ?t, ?s, ?a | _] when prompt == ':nigol' ->
        {:noprompt, [ch | prompt] ++ uptoPrompt, rest}

      _ ->
        {:prompt, [ch | prompt] ++ uptoPrompt, [ch | prompt], rest}
    end
  end
end
