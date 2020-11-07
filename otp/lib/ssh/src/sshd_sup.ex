defmodule :m_sshd_sup do
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

  def start_link() do
    :supervisor.start_link({:local, :sshd_sup}, :sshd_sup, [])
  end

  def start_child(address, port, profile, options) do
    case :ssh_system_sup.system_supervisor(address, port, profile) do
      :undefined ->
        spec = child_spec(address, port, profile, options)
        :supervisor.start_child(:sshd_sup, spec)

      pid ->
        accPid = :ssh_system_sup.acceptor_supervisor(pid)
        :ssh_acceptor_sup.start_child(accPid, address, port, profile, options)
        {:ok, pid}
    end
  end

  def stop_child(childId) when is_tuple(childId) do
    :supervisor.terminate_child(:sshd_sup, childId)
  end

  def stop_child(childPid) when is_pid(childPid) do
    stop_child(system_name(childPid))
  end

  def stop_child(address, port, profile) do
    id = id(address, port, profile)
    stop_child(id)
  end

  def init(_) do
    supFlags = %{:strategy => :one_for_one, :intensity => 10, :period => 3600}
    childSpecs = []
    {:ok, {supFlags, childSpecs}}
  end

  defp child_spec(address, port, profile, options) do
    %{
      :id => id(address, port, profile),
      :start => {:ssh_system_sup, :start_link, [:server, address, port, profile, options]},
      :restart => :temporary,
      :type => :supervisor
    }
  end

  defp id(address, port, profile) do
    {:server, :ssh_system_sup, address, port, profile}
  end

  defp system_name(sysSup) do
    case :lists.keyfind(sysSup, 2, :supervisor.which_children(:sshd_sup)) do
      {name, ^sysSup, _, _} ->
        name

      false ->
        :undefind
    end
  end
end
