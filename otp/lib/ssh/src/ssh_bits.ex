defmodule :m_ssh_bits do
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

  def name_list(namesList) do
    :erlang.list_to_binary(:lists.join(?,, namesList))
  end

  def mpint(-1) do
    <<0, 0, 0, 1, 255>>
  end

  def mpint(0) do
    <<0, 0, 0, 0>>
  end

  def mpint(i) when i > 0 do
    <<b1, v::binary>> = :binary.encode_unsigned(i)

    case b1 &&& 128 do
      128 ->
        <<:erlang.size(v) + 2::size(32)-unsigned-big-integer, 0, b1, v::binary>>

      _ ->
        <<:erlang.size(v) + 1::size(32)-unsigned-big-integer, b1, v::binary>>
    end
  end

  def mpint(n) when n < 0 do
    sxn = 8 * :erlang.size(:binary.encode_unsigned(-n))
    sxn1 = sxn + 8
    <<w::size(sxn1)>> = <<1, 0::size(sxn)>>
    <<b1, v::binary>> = :binary.encode_unsigned(w + n)

    case b1 &&& 128 do
      128 ->
        <<:erlang.size(v) + 1::size(32)-unsigned-big-integer, b1, v::binary>>

      _ ->
        <<:erlang.size(v) + 2::size(32)-unsigned-big-integer, 255, b1, v::binary>>
    end
  end

  def random(n) do
    :crypto.strong_rand_bytes(n)
  end
end
