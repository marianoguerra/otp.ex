defmodule :m_ssh_tcpip_forward_acceptor do
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

  def supervised_start(
        fwdSup,
        {listenAddrStr, listenPort},
        connectToAddr,
        chanType,
        chanCB,
        connPid
      ) do
    case get_fwd_listen_opts(listenAddrStr) do
      {:ok, opts} ->
        case :gen_tcp.listen(
               listenPort,
               [
                 :binary,
                 {:reuseaddr, true},
                 {:active, false}
                 | opts
               ]
             ) do
          {:ok, lSock} ->
            {:ok, {_, trueListenPort}} = :inet.sockname(lSock)

            :ssh_tcpip_forward_acceptor_sup.start_child(
              fwdSup,
              lSock,
              {listenAddrStr, trueListenPort},
              connectToAddr,
              chanType,
              chanCB,
              connPid
            )

            {:ok, trueListenPort}

          {:error, error} ->
            {:error, error}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  def start_link(lSock, {listenAddrStr, listenPort}, connectToAddr, chanType, chanCB, connPid) do
    pid =
      :proc_lib.spawn_link(fn ->
        acceptor_loop(lSock, listenAddrStr, listenPort, connectToAddr, chanType, chanCB, connPid)
      end)

    {:ok, pid}
  end

  defp acceptor_loop(lSock, listenAddrStr, listenPort, connectToAddr, chanType, chanCB, connPid) do
    case :gen_tcp.accept(lSock) do
      {:ok, sock} ->
        {:ok, {remHost, remPort}} = :inet.peername(sock)
        remHostBin = :erlang.list_to_binary(encode_ip(remHost))

        data =
          case connectToAddr do
            :undefined ->
              <<:erlang.size(listenAddrStr)::size(32)-unsigned-big-integer, listenAddrStr::binary,
                listenPort::size(32)-unsigned-big-integer,
                :erlang.size(remHostBin)::size(32)-unsigned-big-integer, remHostBin::binary,
                remPort::size(32)-unsigned-big-integer>>

            {connectToHost, connectToPort} ->
              <<:erlang.size(connectToHost)::size(32)-unsigned-big-integer, connectToHost::binary,
                connectToPort::size(32)-unsigned-big-integer,
                :erlang.size(remHostBin)::size(32)-unsigned-big-integer, remHostBin::binary,
                remPort::size(32)-unsigned-big-integer>>
          end

        case :ssh_connection.open_channel(connPid, chanType, data, :infinity) do
          {:ok, chId} ->
            :gen_tcp.controlling_process(sock, connPid)
            send(connPid, {:fwd_connect_received, sock, chId, chanCB})

          _ ->
            :gen_tcp.close(sock)
        end

        acceptor_loop(lSock, listenAddrStr, listenPort, connectToAddr, chanType, chanCB, connPid)

      {:error, :closed} ->
        :ok
    end
  end

  defp get_fwd_listen_opts("") do
    {:ok, []}
  end

  defp get_fwd_listen_opts("0.0.0.0") do
    {:ok, [:inet]}
  end

  defp get_fwd_listen_opts("::") do
    {:ok, [:inet6]}
  end

  defp get_fwd_listen_opts("localhost") do
    {:ok, [{:ip, :loopback}]}
  end

  defp get_fwd_listen_opts(addrStr) do
    case :inet.getaddr(
           :erlang.binary_to_list(addrStr),
           :inet
         ) do
      {:ok, addr} ->
        {:ok, [{:ip, addr}]}

      {:error, error} ->
        {:error, error}
    end
  end

  defp encode_ip(addr) when is_tuple(addr) do
    case (try do
            :inet_parse.ntoa(addr)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        false

      a ->
        a
    end
  end
end
