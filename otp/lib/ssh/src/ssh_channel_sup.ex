defmodule :m_ssh_channel_sup do
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

  def start_link(args) do
    :supervisor.start_link(:ssh_channel_sup, [args])
  end

  def start_child(:client, channelSup, connRef, callback, id, args, exec, _Opts)
      when is_pid(connRef) do
    start_the_child(:ssh_client_channel, channelSup, connRef, callback, id, args, exec)
  end

  def start_child(:server, channelSup, connRef, callback, id, args, exec, opts)
      when is_pid(connRef) do
    case max_num_channels_not_exceeded(
           channelSup,
           opts
         ) do
      true ->
        start_the_child(:ssh_server_channel, channelSup, connRef, callback, id, args, exec)

      false ->
        {:error, :max_num_channels_exceeded}
    end
  end

  defp start_the_child(chanMod, channelSup, connRef, callback, id, args, exec) do
    childSpec = %{
      :id => make_ref(),
      :start => {chanMod, :start_link, [connRef, id, callback, args, exec]},
      :restart => :temporary,
      :type => :worker,
      :modules => [chanMod]
    }

    case :supervisor.start_child(channelSup, childSpec) do
      {:ok, pid} ->
        {:ok, pid}

      {:ok, pid, _Info} ->
        {:ok, pid}

      {:error, {error, _Info}} ->
        {:error, error}

      {:error, error} ->
        {:error, error}
    end
  end

  def init(_Args) do
    restartStrategy = :one_for_one
    maxR = 10
    maxT = 3600
    children = []
    {:ok, {{restartStrategy, maxR, maxT}, children}}
  end

  defp max_num_channels_not_exceeded(channelSup, opts) do
    maxNumChannels =
      :ssh_options.get_value(:user_options, :max_channels, opts, :ssh_channel_sup, 86)

    numChannels =
      length(
        for {_, _, :worker, [:ssh_server_channel]} <- :supervisor.which_children(channelSup) do
          :x
        end
      )

    numChannels < maxNumChannels
  end
end
