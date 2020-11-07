defmodule :m_ssh_io do
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

  def read_line(prompt, opts) do
    format('~s', [listify(prompt)])

    send(
      :ssh_options.get_value(:internal_options, :user_pid, opts, :ssh_io, 32),
      {self(), :question}
    )

    receive do
      answer when is_list(answer) or is_binary(answer) ->
        :unicode.characters_to_list(answer)
    end
  end

  def yes_no(prompt, opts) do
    format('~s [y/n]?', [prompt])

    send(
      :ssh_options.get_value(:internal_options, :user_pid, opts, :ssh_io, 40),
      {self(), :question}
    )

    receive do
      :y ->
        :yes

      :n ->
        :no

      answer when is_list(answer) or is_binary(answer) ->
        case trim(answer) do
          'y' ->
            :yes

          'n' ->
            :no

          'Y' ->
            :yes

          'N' ->
            :no

          _ ->
            format('please answer y or n\n', [])
            yes_no(prompt, opts)
        end
    end
  end

  def read_password(prompt, opts) do
    format('~s', [listify(prompt)])

    send(
      :ssh_options.get_value(:internal_options, :user_pid, opts, :ssh_io, 61),
      {self(), :user_password}
    )

    receive do
      answer when is_list(answer) or is_binary(answer) ->
        case trim(answer) do
          '' ->
            read_password(prompt, opts)

          pwd ->
            pwd
        end
    end
  end

  def format(fmt, args) do
    :io.format(fmt, args)
  end

  defp listify(a) when is_atom(a) do
    :erlang.atom_to_list(a)
  end

  defp listify(l) when is_list(l) do
    l
  end

  defp listify(b) when is_binary(b) do
    :erlang.binary_to_list(b)
  end

  defp trim(line) when is_list(line) do
    :lists.reverse(trim1(:lists.reverse(trim1(line))))
  end

  defp trim(line) when is_binary(line) do
    trim(:unicode.characters_to_list(line))
  end

  defp trim(other) do
    other
  end

  defp trim1([?\s | cs]) do
    trim(cs)
  end

  defp trim1([?\r | cs]) do
    trim(cs)
  end

  defp trim1([?\n | cs]) do
    trim(cs)
  end

  defp trim1([?\t | cs]) do
    trim(cs)
  end

  defp trim1(cs) do
    cs
  end
end
