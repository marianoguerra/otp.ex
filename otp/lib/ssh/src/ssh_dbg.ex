defmodule :m_ssh_dbg do
  use Bitwise
  require Record

  Record.defrecord(:r_ssh, :ssh,
    role: :undefined,
    peer: :undefined,
    local: :undefined,
    c_vsn: :undefined,
    s_vsn: :undefined,
    c_version: :undefined,
    s_version: :undefined,
    c_keyinit: :undefined,
    s_keyinit: :undefined,
    send_ext_info: :undefined,
    recv_ext_info: :undefined,
    algorithms: :undefined,
    send_mac: :none,
    send_mac_key: :undefined,
    send_mac_size: 0,
    recv_mac: :none,
    recv_mac_key: :undefined,
    recv_mac_size: 0,
    encrypt: :none,
    encrypt_cipher: :undefined,
    encrypt_keys: :undefined,
    encrypt_block_size: 8,
    encrypt_ctx: :undefined,
    decrypt: :none,
    decrypt_cipher: :undefined,
    decrypt_keys: :undefined,
    decrypt_block_size: 8,
    decrypt_ctx: :undefined,
    compress: :none,
    compress_ctx: :undefined,
    decompress: :none,
    decompress_ctx: :undefined,
    c_lng: :none,
    s_lng: :none,
    user_ack: true,
    timeout: :infinity,
    shared_secret: :undefined,
    exchanged_hash: :undefined,
    session_id: :undefined,
    opts: [],
    send_sequence: 0,
    recv_sequence: 0,
    keyex_key: :undefined,
    keyex_info: :undefined,
    random_length_padding: 15,
    user: :undefined,
    service: :undefined,
    userauth_quiet_mode: :undefined,
    userauth_methods: :undefined,
    userauth_supported_methods: :undefined,
    userauth_pubkeys: :undefined,
    kb_tries_left: 0,
    userauth_preference: :undefined,
    available_host_keys: :undefined,
    pwdfun_user_state: :undefined,
    authenticated: false
  )

  Record.defrecord(:r_alg, :alg,
    kex: :undefined,
    hkey: :undefined,
    send_mac: :undefined,
    recv_mac: :undefined,
    encrypt: :undefined,
    decrypt: :undefined,
    compress: :undefined,
    decompress: :undefined,
    c_lng: :undefined,
    s_lng: :undefined,
    send_ext_info: :undefined,
    recv_ext_info: :undefined
  )

  Record.defrecord(:r_ssh_pty, :ssh_pty,
    term: '',
    width: 80,
    height: 25,
    pixel_width: 1024,
    pixel_height: 768,
    modes: <<>>
  )

  Record.defrecord(:r_circ_buf_entry, :circ_buf_entry,
    module: :undefined,
    line: :undefined,
    function: :undefined,
    pid: self(),
    value: :undefined
  )

  Record.defrecord(:r_ssh_msg_disconnect, :ssh_msg_disconnect,
    code: :undefined,
    description: :undefined,
    language: :undefined
  )

  Record.defrecord(:r_ssh_msg_ignore, :ssh_msg_ignore, data: :undefined)
  Record.defrecord(:r_ssh_msg_unimplemented, :ssh_msg_unimplemented, sequence: :undefined)

  Record.defrecord(:r_ssh_msg_debug, :ssh_msg_debug,
    always_display: :undefined,
    message: :undefined,
    language: :undefined
  )

  Record.defrecord(:r_ssh_msg_service_request, :ssh_msg_service_request, name: :undefined)
  Record.defrecord(:r_ssh_msg_service_accept, :ssh_msg_service_accept, name: :undefined)

  Record.defrecord(:r_ssh_msg_ext_info, :ssh_msg_ext_info,
    nr_extensions: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_kexinit, :ssh_msg_kexinit,
    cookie: :undefined,
    kex_algorithms: :undefined,
    server_host_key_algorithms: :undefined,
    encryption_algorithms_client_to_server: :undefined,
    encryption_algorithms_server_to_client: :undefined,
    mac_algorithms_client_to_server: :undefined,
    mac_algorithms_server_to_client: :undefined,
    compression_algorithms_client_to_server: :undefined,
    compression_algorithms_server_to_client: :undefined,
    languages_client_to_server: :undefined,
    languages_server_to_client: :undefined,
    first_kex_packet_follows: false,
    reserved: 0
  )

  Record.defrecord(:r_ssh_msg_kexdh_init, :ssh_msg_kexdh_init, e: :undefined)

  Record.defrecord(:r_ssh_msg_kexdh_reply, :ssh_msg_kexdh_reply,
    public_host_key: :undefined,
    f: :undefined,
    h_sig: :undefined
  )

  Record.defrecord(:r_ssh_msg_newkeys, :ssh_msg_newkeys, [])

  Record.defrecord(:r_ssh_msg_kex_dh_gex_request, :ssh_msg_kex_dh_gex_request,
    min: :undefined,
    n: :undefined,
    max: :undefined
  )

  Record.defrecord(:r_ssh_msg_kex_dh_gex_request_old, :ssh_msg_kex_dh_gex_request_old,
    n: :undefined
  )

  Record.defrecord(:r_ssh_msg_kex_dh_gex_group, :ssh_msg_kex_dh_gex_group,
    p: :undefined,
    g: :undefined
  )

  Record.defrecord(:r_ssh_msg_kex_dh_gex_init, :ssh_msg_kex_dh_gex_init, e: :undefined)

  Record.defrecord(:r_ssh_msg_kex_dh_gex_reply, :ssh_msg_kex_dh_gex_reply,
    public_host_key: :undefined,
    f: :undefined,
    h_sig: :undefined
  )

  Record.defrecord(:r_ssh_msg_kex_ecdh_init, :ssh_msg_kex_ecdh_init, q_c: :undefined)

  Record.defrecord(:r_ssh_msg_kex_ecdh_reply, :ssh_msg_kex_ecdh_reply,
    public_host_key: :undefined,
    q_s: :undefined,
    h_sig: :undefined
  )

  Record.defrecord(:r_ssh_msg_global_request, :ssh_msg_global_request,
    name: :undefined,
    want_reply: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_request_success, :ssh_msg_request_success, data: :undefined)
  Record.defrecord(:r_ssh_msg_request_failure, :ssh_msg_request_failure, [])

  Record.defrecord(:r_ssh_msg_channel_open, :ssh_msg_channel_open,
    channel_type: :undefined,
    sender_channel: :undefined,
    initial_window_size: :undefined,
    maximum_packet_size: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_open_confirmation, :ssh_msg_channel_open_confirmation,
    recipient_channel: :undefined,
    sender_channel: :undefined,
    initial_window_size: :undefined,
    maximum_packet_size: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_open_failure, :ssh_msg_channel_open_failure,
    recipient_channel: :undefined,
    reason: :undefined,
    description: :undefined,
    lang: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_window_adjust, :ssh_msg_channel_window_adjust,
    recipient_channel: :undefined,
    bytes_to_add: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_data, :ssh_msg_channel_data,
    recipient_channel: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_extended_data, :ssh_msg_channel_extended_data,
    recipient_channel: :undefined,
    data_type_code: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_eof, :ssh_msg_channel_eof, recipient_channel: :undefined)

  Record.defrecord(:r_ssh_msg_channel_close, :ssh_msg_channel_close, recipient_channel: :undefined)

  Record.defrecord(:r_ssh_msg_channel_request, :ssh_msg_channel_request,
    recipient_channel: :undefined,
    request_type: :undefined,
    want_reply: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_success, :ssh_msg_channel_success,
    recipient_channel: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_failure, :ssh_msg_channel_failure,
    recipient_channel: :undefined
  )

  Record.defrecord(:r_channel, :channel,
    type: :undefined,
    sys: :undefined,
    user: :undefined,
    flow_control: :undefined,
    local_id: :undefined,
    recv_window_size: :undefined,
    recv_window_pending: 0,
    recv_packet_size: :undefined,
    recv_close: false,
    remote_id: :undefined,
    send_window_size: :undefined,
    send_packet_size: :undefined,
    sent_close: false,
    send_buf: []
  )

  Record.defrecord(:r_connection, :connection,
    requests: [],
    channel_cache: :undefined,
    channel_id_seed: :undefined,
    cli_spec: :undefined,
    options: :undefined,
    exec: :undefined,
    system_supervisor: :undefined,
    sub_system_supervisor: :undefined,
    connection_supervisor: :undefined
  )

  Record.defrecord(:r_ssh_msg_userauth_request, :ssh_msg_userauth_request,
    user: :undefined,
    service: :undefined,
    method: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_userauth_failure, :ssh_msg_userauth_failure,
    authentications: :undefined,
    partial_success: :undefined
  )

  Record.defrecord(:r_ssh_msg_userauth_success, :ssh_msg_userauth_success, [])

  Record.defrecord(:r_ssh_msg_userauth_banner, :ssh_msg_userauth_banner,
    message: :undefined,
    language: :undefined
  )

  Record.defrecord(:r_ssh_msg_userauth_passwd_changereq, :ssh_msg_userauth_passwd_changereq,
    prompt: :undefined,
    languge: :undefined
  )

  Record.defrecord(:r_ssh_msg_userauth_pk_ok, :ssh_msg_userauth_pk_ok,
    algorithm_name: :undefined,
    key_blob: :undefined
  )

  Record.defrecord(:r_ssh_msg_userauth_info_request, :ssh_msg_userauth_info_request,
    name: :undefined,
    instruction: :undefined,
    language_tag: :undefined,
    num_prompts: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_userauth_info_response, :ssh_msg_userauth_info_response,
    num_responses: :undefined,
    data: :undefined
  )

  @behaviour :gen_server
  def start() do
    start(&:io.format/2)
  end

  def start(ioFmtFun)
      when is_function(ioFmtFun, 2) or
             is_function(ioFmtFun, 3) do
    start_server()

    try do
      :dbg.start()
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    start_tracer(ioFmtFun)
    :dbg.p(:all, get_all_trace_flags())
    get_all_dbg_types()
  end

  def stop() do
    try do
      :dbg.stop_clear()
      :gen_server.stop(:ssh_dbg)
    catch
      _, _ ->
        :ok
    end
  end

  def start_server() do
    :gen_server.start({:local, :ssh_dbg}, :ssh_dbg, [], [])
  end

  def start_tracer() do
    start_tracer(&:io.format/2)
  end

  def start_tracer(writeFun) when is_function(writeFun, 2) do
    start_tracer(fn f, a, s ->
      writeFun.(f, a)
      s
    end)
  end

  def start_tracer(writeFun) when is_function(writeFun, 3) do
    start_tracer(writeFun, :undefined)
  end

  defp start_tracer(writeFun, initAcc)
       when is_function(
              writeFun,
              3
            ) do
    handler = fn arg, acc0 ->
      try_all_types_in_all_modules(
        :gen_server.call(
          :ssh_dbg,
          :get_on,
          15000
        ),
        arg,
        writeFun,
        acc0
      )
    end

    :dbg.tracer(:process, {handler, initAcc})
  end

  def on() do
    on(get_all_dbg_types())
  end

  def on(type) do
    switch(:on, type)
  end

  def is_on() do
    :gen_server.call(:ssh_dbg, :get_on, 15000)
  end

  def off() do
    off(get_all_dbg_types())
  end

  def off(type) do
    switch(:off, type)
  end

  def is_off() do
    get_all_dbg_types() -- is_on()
  end

  def go_on() do
    isOn = :gen_server.call(:ssh_dbg, :get_on, 15000)
    on(isOn)
  end

  def shrink_bin(b)
      when is_binary(b) and
             :erlang.size(b) > 256 do
    {:"*** SHRINKED BIN", :erlang.size(b), :erlang.element(1, :erlang.split_binary(b, 64)), :...,
     :erlang.element(
       2,
       :erlang.split_binary(b, :erlang.size(b) - 64)
     )}
  end

  def shrink_bin(l) when is_list(l) do
    :lists.map(&shrink_bin/1, l)
  end

  def shrink_bin(t) when is_tuple(t) do
    :erlang.list_to_tuple(shrink_bin(:erlang.tuple_to_list(t)))
  end

  def shrink_bin(x) do
    x
  end

  def reduce_state(t, recordExample) do
    name = :erlang.element(1, recordExample)
    arity = :erlang.size(recordExample)
    reduce_state(t, name, arity)
  end

  def reduce_state(t, name, arity)
      when :erlang.element(
             1,
             t
           ) == name and
             :erlang.size(t) == arity do
    :lists.concat([:"#", name, :{}])
  end

  def reduce_state(l, name, arity) when is_list(l) do
    for e <- l do
      reduce_state(e, name, arity)
    end
  end

  def reduce_state(t, name, arity) when is_tuple(t) do
    :erlang.list_to_tuple(reduce_state(:erlang.tuple_to_list(t), name, arity))
  end

  def reduce_state(x, _, _) do
    x
  end

  Record.defrecord(:r_data, :data, types_on: [])

  def init(_) do
    new_table()
    {:ok, r_data()}
  end

  defp new_table() do
    try do
      :ets.new(:ssh_dbg, [:public, :named_table])
      :ok
    catch
      :exit, :badarg ->
        :ok
    end
  end

  defp get_proc_stack(pid) when is_pid(pid) do
    try do
      :ets.lookup_element(:ssh_dbg, pid, 2)
    catch
      :error, :badarg ->
        new_proc(pid)
        :ets.insert(:ssh_dbg, {pid, []})
        []
    end
  end

  defp put_proc_stack(pid, data)
       when is_pid(pid) and
              is_list(data) do
    :ets.insert(:ssh_dbg, {pid, data})
  end

  defp new_proc(pid) when is_pid(pid) do
    :gen_server.cast(:ssh_dbg, {:new_proc, pid})
  end

  def ets_delete(tab, key) do
    try do
      :ets.delete(tab, key)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  def handle_call({:switch, :on, types}, _From, d) do
    nowOn = :lists.usort(types ++ r_data(d, :types_on))
    call_modules(:on, types)
    {:reply, {:ok, nowOn}, r_data(d, types_on: nowOn)}
  end

  def handle_call({:switch, :off, types}, _From, d) do
    stillOn = r_data(d, :types_on) -- types
    call_modules(:off, types)
    call_modules(:on, stillOn)
    {:reply, {:ok, stillOn}, r_data(d, types_on: stillOn)}
  end

  def handle_call(:get_on, _From, d) do
    {:reply, r_data(d, :types_on), d}
  end

  def handle_call(c, _From, d) do
    :io.format(:"*** Unknown call: ~p~n", [c])
    {:reply, {:error, {:unknown_call, c}}, d}
  end

  def handle_cast({:new_proc, pid}, d) do
    :erlang.monitor(:process, pid)
    {:noreply, d}
  end

  def handle_cast(c, d) do
    :io.format(:"*** Unknown cast: ~p~n", [c])
    {:noreply, d}
  end

  def handle_info({:DOWN, _MonitorRef, :process, pid, _Info}, d) do
    :timer.apply_after(20000, :ssh_dbg, :ets_delete, [:ssh_dbg, pid])
    {:noreply, d}
  end

  def handle_info(c, d) do
    :io.format(:"*** Unknown info: ~p~n", [c])
    {:noreply, d}
  end

  defp ssh_modules_with_trace() do
    {:ok, allSshModules} =
      :application.get_key(
        :ssh,
        :modules
      )

    for m <- allSshModules,
        {:behaviour, bs} <- m.module_info(:attributes),
        :lists.member(:ssh_dbg, bs) do
      m
    end
  end

  defp get_all_trace_flags() do
    :lists.usort(
      :lists.flatten([
        :timestamp
        | call_modules(
            :flags,
            get_all_dbg_types()
          )
      ])
    )
  end

  defp get_all_dbg_types() do
    :lists.usort(:lists.flatten(call_modules(:points)))
  end

  defp call_modules(:points) do
    f = fn mod ->
      mod.ssh_dbg_trace_points()
    end

    fold_modules(f, [], ssh_modules_with_trace())
  end

  defp call_modules(cmnd, types) when is_list(types) do
    f =
      case cmnd do
        :flags ->
          fn type ->
            fn mod ->
              mod.ssh_dbg_flags(type)
            end
          end

        :on ->
          fn type ->
            fn mod ->
              mod.ssh_dbg_on(type)
            end
          end

        :off ->
          fn type ->
            fn mod ->
              mod.ssh_dbg_off(type)
            end
          end
      end

    :lists.foldl(
      fn t, acc ->
        fold_modules(f.(t), acc, ssh_modules_with_trace())
      end,
      [],
      types
    )
  end

  defp fold_modules(f, acc0, modules) do
    :lists.foldl(
      fn mod, acc ->
        try do
          f.(mod)
        catch
          _, _ ->
            acc
        else
          result ->
            [result | acc]
        end
      end,
      acc0,
      modules
    )
  end

  defp switch(x, type) when is_atom(type) do
    switch(x, [type])
  end

  defp switch(x, types) when is_list(types) do
    case :erlang.whereis(:ssh_dbg) do
      :undefined ->
        start()

      _ ->
        :ok
    end

    case :lists.usort(types) -- get_all_dbg_types() do
      [] ->
        :gen_server.call(:ssh_dbg, {:switch, x, types}, 15000)

      l ->
        {:error, {:unknown, l}}
    end
  end

  defp trace_pid(t)
       when :erlang.element(1, t) == :trace or
              :erlang.element(1, t) == :trace_ts do
    :erlang.element(2, t)
  end

  defp trace_ts(t) when :erlang.element(1, t) == :trace_ts do
    ts(:erlang.element(:erlang.size(t), t))
  end

  defp trace_info(t) do
    case :erlang.tuple_to_list(t) do
      [:trace, _Pid | info] ->
        :erlang.list_to_tuple(info)

      [:trace_ts, _Pid | infoTS] ->
        :erlang.list_to_tuple(:lists.droplast(infoTS))
    end
  end

  defp try_all_types_in_all_modules(typesOn, arg, writeFun, acc0) do
    sshModules = ssh_modules_with_trace()
    tS = trace_ts(arg)
    pID = trace_pid(arg)
    iNFO = trace_info(arg)

    acc =
      :lists.foldl(
        fn type, acc1 ->
          :lists.foldl(
            fn sshMod, acc ->
              try do
                sshMod.ssh_dbg_format(
                  type,
                  iNFO
                )
              catch
                :error, e
                when e == :undef or
                       e == :function_clause or
                       :erlang.element(
                         1,
                         e
                       ) == :case_clause ->
                  try do
                    sTACK = get_proc_stack(pID)

                    sshMod.ssh_dbg_format(
                      type,
                      iNFO,
                      sTACK
                    )
                  catch
                    _, _ ->
                      acc
                  else
                    {:skip, newStack} ->
                      put_proc_stack(
                        pID,
                        newStack
                      )

                      :written

                    {txt, newStack}
                    when is_list(txt) ->
                      put_proc_stack(
                        pID,
                        newStack
                      )

                      write_txt(writeFun, tS, pID, txt)
                  end
              else
                :skip ->
                  :written

                txt when is_list(txt) ->
                  write_txt(writeFun, tS, pID, txt)
              end
            end,
            acc1,
            sshModules
          )
        end,
        acc0,
        typesOn
      )

    case acc do
      ^acc0 ->
        writeFun.('~n~s ~p DEBUG~n~p~n', [:lists.flatten(tS), pID, iNFO], acc0)

      :written ->
        acc0
    end
  end

  defp write_txt(writeFun, tS, pID, txt) when is_list(txt) do
    writeFun.(
      '~n~s ~p ~ts~n',
      [:lists.flatten(tS), pID, :lists.flatten(txt)],
      :written
    )
  end

  def wr_record(t, fs, bL) when is_tuple(t) do
    wr_record(:erlang.tuple_to_list(t), fs, bL)
  end

  def wr_record([_Name | values], fields, blackL) do
    w =
      case fields do
        [] ->
          0

        _ ->
          :lists.max(
            for f <- fields do
              length(:erlang.atom_to_list(f))
            end
          )
      end

    for {tag, value} <- :lists.zip(fields, values),
        not :lists.member(tag, blackL) do
      :io_lib.format('  ~*p: ~p~n', [w, tag, value])
    end
  end

  defp ts({_, _, usec} = now) when is_integer(usec) do
    {_Date, {hH, mM, sS}} = :calendar.now_to_local_time(now)
    :io_lib.format('~.2.0w:~.2.0w:~.2.0w.~.6.0w', [hH, mM, sS, usec])
  end

  defp ts(_) do
    '-'
  end

  def cbuf_start() do
    cbuf_start(20)
  end

  def cbuf_start(cbufMaxLen) do
    :erlang.put(:circ_buf, {cbufMaxLen, :queue.new()})
    :ok
  end

  def cbuf_stop_clear() do
    case :erlang.erase(:circ_buf) do
      :undefined ->
        []

      {_CbufMaxLen, queue} ->
        :queue.to_list(queue)
    end
  end

  def cbuf_in(value) do
    case :erlang.get(:circ_buf) do
      :undefined ->
        :disabled

      {cbufMaxLen, queue} ->
        updatedQueue =
          try do
            :queue.head(queue)
          catch
            :error, :empty ->
              :queue.in_r({value, :erlang.timestamp(), 1}, queue)
          else
            {^value, tS0, cnt0} ->
              :queue.in_r(
                {value, tS0, cnt0 + 1},
                :queue.drop(queue)
              )

            _ ->
              :queue.in_r(
                {value, :erlang.timestamp(), 1},
                truncate_cbuf(queue, cbufMaxLen)
              )
          end

        :erlang.put(:circ_buf, {cbufMaxLen, updatedQueue})
        :ok
    end
  end

  def cbuf_list() do
    case :erlang.get(:circ_buf) do
      :undefined ->
        []

      {_CbufMaxLen, queue} ->
        :queue.to_list(queue)
    end
  end

  defp truncate_cbuf(q, cbufMaxLen) do
    case :queue.len(q) do
      n when n >= cbufMaxLen ->
        truncate_cbuf(
          :erlang.element(2, :queue.out_r(q)),
          cbufMaxLen
        )

      _ ->
        q
    end
  end

  def fmt_cbuf_items() do
    :lists.flatten(
      :io_lib.format(
        'Circular trace buffer. Latest item first.~n~s~n',
        [
          case :erlang.get(:circ_buf) do
            {max, _} ->
              l = cbuf_list()

              for {n, x} <-
                    :lists.zip(
                      :lists.seq(
                        1,
                        length(l)
                      ),
                      l
                    ) do
                :io_lib.format(
                  '==== ~.*w: ~s~n',
                  [num_digits(max), n, fmt_cbuf_item(x)]
                )
              end

            _ ->
              :io_lib.format('Not started.~n', [])
          end
        ]
      )
    )
  end

  defp num_digits(0) do
    1
  end

  defp num_digits(n) when n > 0 do
    1 + trunc(:math.log10(n))
  end

  def fmt_cbuf_item({value, timeStamp, n}) do
    :io_lib.format(
      '~s~s~n~s~n',
      [
        fmt_ts(timeStamp),
        for _ <- [:EFE_DUMMY_GEN], n > 1 do
          :io_lib.format(' (Repeated ~p times)', [n])
        end,
        fmt_value(value)
      ]
    )
  end

  defp fmt_ts(tS = {_, _, us}) do
    {{yY, mM, dD}, {h, m, s}} = :calendar.now_to_universal_time(tS)
    :io_lib.format('~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w.~.6.0w UTC', [yY, mM, dD, h, m, s, us])
  end

  defp fmt_value(r_circ_buf_entry(module: m, line: l, function: {f, a}, pid: pid, value: v)) do
    :io_lib.format('~p:~p  ~p/~p ~p~n~s', [m, l, f, a, pid, fmt_value(v)])
  end

  defp fmt_value(value) do
    :io_lib.format('~p', [value])
  end

  Record.defrecord(:r_h, :h, max_bytes: 65536, bytes_per_line: 16, address_len: 4)

  def hex_dump(data) do
    hex_dump1(data, hd_opts([]))
  end

  def hex_dump(x, max) when is_integer(max) do
    hex_dump(x, [{:max_bytes, max}])
  end

  def hex_dump(x, optList) when is_list(optList) do
    hex_dump1(x, hd_opts(optList))
  end

  defp hex_dump1(b, opts) when is_binary(b) do
    hex_dump1(:erlang.binary_to_list(b), opts)
  end

  defp hex_dump1(l, opts)
       when is_list(l) and
              length(l) > r_h(opts, :max_bytes) do
    :io_lib.format(
      '~s---- skip ~w bytes----~n',
      [
        hex_dump1(
          :lists.sublist(l, r_h(opts, :max_bytes)),
          opts
        ),
        length(l) - r_h(opts, :max_bytes)
      ]
    )
  end

  defp hex_dump1(l, opts0) when is_list(l) do
    opts = r_h(opts0, address_len: num_hex_digits(r_h(opts0, :max_bytes)))
    result = hex_dump(l, [{0, [], []}], opts)

    [
      :io_lib.format(
        '~*.s | ~*s | ~s~n~*.c-+-~*c-+-~*c~n',
        [
          r_h(opts, :address_len),
          :lists.sublist(
            'Address',
            r_h(opts, :address_len)
          ),
          -3 * r_h(opts, :bytes_per_line),
          :lists.sublist(
            'Hexdump',
            3 * r_h(opts, :bytes_per_line)
          ),
          'ASCII',
          r_h(opts, :address_len),
          ?-,
          3 * r_h(opts, :bytes_per_line),
          ?-,
          r_h(opts, :bytes_per_line),
          ?-
        ]
      )
      | for {n, hexs, chars} <- :lists.reverse(result) do
          :io_lib.format(
            '~*.16.0b | ~s~*c | ~s~n',
            [
              r_h(opts, :address_len),
              n * r_h(opts, :bytes_per_line),
              :lists.reverse(hexs),
              3 * (r_h(opts, :bytes_per_line) - length(hexs)),
              ?\s,
              :lists.reverse(chars)
            ]
          )
        end
    ]
  end

  defp hd_opts(l) do
    :lists.foldl(&hd_opt/2, r_h(), l)
  end

  defp hd_opt({:max_bytes, m}, o) do
    r_h(o, max_bytes: m)
  end

  defp hd_opt({:bytes_per_line, m}, o) do
    r_h(o, bytes_per_line: m)
  end

  defp num_hex_digits(n) when n < 16 do
    1
  end

  defp num_hex_digits(n) do
    trunc(:math.ceil(:math.log2(n) / 4))
  end

  defp hex_dump([l | cs], result0, opts) when is_list(l) do
    result = hex_dump(l, result0, opts)
    hex_dump(cs, result, opts)
  end

  defp hex_dump(cs, [{n0, _, chars} | _] = lines, opts)
       when length(chars) == r_h(opts, :bytes_per_line) do
    hex_dump(cs, [{n0 + 1, [], []} | lines], opts)
  end

  defp hex_dump([c | cs], [{n, hexs, chars} | lines], opts) do
    asc =
      cond do
        32 <= c and c <= 126 ->
          c

        true ->
          ?.
      end

    hex = :io_lib.format('~2.16.0b ', [c])
    hex_dump(cs, [{n, [hex | hexs], [asc | chars]} | lines], opts)
  end

  defp hex_dump([], result, _) do
    result
  end
end
