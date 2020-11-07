defmodule :m_ssh_acceptor_sup do
  use Bitwise
  @behaviour :supervisor
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

  def start_link(address, port, profile, options) do
    :supervisor.start_link(
      :ssh_acceptor_sup,
      [address, port, profile, options]
    )
  end

  def start_child(accSup, address, port, profile, options) do
    spec = child_spec(address, port, profile, options)

    case :supervisor.start_child(accSup, spec) do
      {:error, :already_present} ->
        stop_child(accSup, address, port, profile)
        :supervisor.start_child(accSup, spec)

      reply ->
        reply
    end
  end

  def stop_child(accSup, address, port, profile) do
    name = id(address, port, profile)

    case :supervisor.terminate_child(accSup, name) do
      :ok ->
        :supervisor.delete_child(accSup, name)

      error ->
        error
    end
  end

  def init([address, port, profile, options]) do
    supFlags = %{:strategy => :one_for_one, :intensity => 10, :period => 3600}
    childSpecs = [child_spec(address, port, profile, options)]
    {:ok, {supFlags, childSpecs}}
  end

  defp child_spec(address, port, profile, options) do
    timeout =
      :ssh_options.get_value(
        :internal_options,
        :timeout,
        options,
        fn ->
          50000
        end,
        :ssh_acceptor_sup,
        84
      )

    %{
      :id => id(address, port, profile),
      :start => {:ssh_acceptor, :start_link, [port, address, options, timeout]},
      :restart => :transient
    }
  end

  defp id(address, port, profile) do
    {:ssh_acceptor_sup, address, port, profile}
  end
end
