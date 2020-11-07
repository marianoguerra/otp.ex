defmodule :m_ssh_subsystem_sup do
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

  def start_link(role, address, port, profile, options) do
    :supervisor.start_link(
      :ssh_subsystem_sup,
      [role, address, port, profile, options]
    )
  end

  def connection_supervisor(supPid) do
    children = :supervisor.which_children(supPid)
    ssh_connection_sup(children)
  end

  def channel_supervisor(supPid) when is_pid(supPid) do
    children = :supervisor.which_children(supPid)
    ssh_channel_sup(children)
  end

  def tcpip_fwd_supervisor(supPid) when is_pid(supPid) do
    children = :supervisor.which_children(supPid)
    tcpip_fwd_sup(children)
  end

  def start_channel(role, supPid, connRef, callback, id, args, exec, opts) do
    channelSup = channel_supervisor(supPid)
    :ssh_channel_sup.start_child(role, channelSup, connRef, callback, id, args, exec, opts)
  end

  def init([role, address, port, profile, options]) do
    supFlags = %{:strategy => :one_for_all, :intensity => 0, :period => 3600}
    childSpecs = child_specs(role, address, port, profile, options)
    {:ok, {supFlags, childSpecs}}
  end

  defp child_specs(role, address, port, profile, options) do
    [
      ssh_channel_child_spec(role, address, port, profile, options),
      ssh_connection_child_spec(role, address, port, profile, options),
      ssh_tcpip_forward_acceptor_child_spec()
    ]
  end

  defp ssh_connection_child_spec(role, address, port, _Profile, options) do
    %{
      :id => id(role, :ssh_connection_sup, address, port),
      :start => {:ssh_connection_sup, :start_link, [options]},
      :restart => :temporary,
      :type => :supervisor
    }
  end

  defp ssh_channel_child_spec(role, address, port, _Profile, options) do
    %{
      :id => id(role, :ssh_channel_sup, address, port),
      :start => {:ssh_channel_sup, :start_link, [options]},
      :restart => :temporary,
      :type => :supervisor
    }
  end

  defp ssh_tcpip_forward_acceptor_child_spec() do
    %{
      :id => make_ref(),
      :start => {:ssh_tcpip_forward_acceptor_sup, :start_link, []},
      :restart => :temporary,
      :type => :supervisor
    }
  end

  defp id(role, sup, address, port) do
    {role, sup, address, port}
  end

  defp ssh_connection_sup([{_, child, _, [:ssh_connection_sup]} | _]) do
    child
  end

  defp ssh_connection_sup([_ | rest]) do
    ssh_connection_sup(rest)
  end

  defp ssh_channel_sup([{_, child, _, [:ssh_channel_sup]} | _]) do
    child
  end

  defp ssh_channel_sup([_ | rest]) do
    ssh_channel_sup(rest)
  end

  defp tcpip_fwd_sup([
         {_, child, _, [:ssh_tcpip_forward_acceptor_sup]}
         | _
       ]) do
    child
  end

  defp tcpip_fwd_sup([_ | rest]) do
    tcpip_fwd_sup(rest)
  end
end
