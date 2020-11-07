defmodule :m_ssh_agent do
  use Bitwise
  import Kernel, except: [send: 2]
  @behaviour :ssh_client_key_api
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

  Record.defrecord(:r_ssh_agent_success, :ssh_agent_success, [])
  Record.defrecord(:r_ssh_agent_failure, :ssh_agent_failure, [])
  Record.defrecord(:r_ssh_agent_identities_request, :ssh_agent_identities_request, [])

  Record.defrecord(:r_ssh_agent_key, :ssh_agent_key,
    blob: :undefined,
    comment: :undefined
  )

  Record.defrecord(:r_ssh_agent_identities_response, :ssh_agent_identities_response,
    keys: :undefined
  )

  Record.defrecord(:r_ssh_agent_sign_request, :ssh_agent_sign_request,
    key_blob: :undefined,
    data: :undefined,
    flags: :undefined
  )

  Record.defrecord(:r_ssh_agent_signature, :ssh_agent_signature,
    format: :undefined,
    blob: :undefined
  )

  Record.defrecord(:r_ssh_agent_sign_response, :ssh_agent_sign_response, signature: :undefined)

  def add_host_key(host, publicKey, options) do
    keyCbOpts = :proplists.get_value(:key_cb_private, options, [])
    sshFileCb = :proplists.get_value(:call_ssh_file, keyCbOpts, :ssh_file)
    sshFileCb.add_host_key(host, publicKey, options)
  end

  def is_host_key(key, peerName, algorithm, opts) do
    keyCbOpts = :proplists.get_value(:key_cb_private, opts, [])
    sshFileCb = :proplists.get_value(:call_ssh_file, keyCbOpts, :ssh_file)
    sshFileCb.is_host_key(key, peerName, algorithm, opts)
  end

  def add_host_key(host, port, publicKey, options) do
    keyCbOpts = :proplists.get_value(:key_cb_private, options, [])
    sshFileCb = :proplists.get_value(:call_ssh_file, keyCbOpts, :ssh_file)
    sshFileCb.add_host_key(host, port, publicKey, options)
  end

  def is_host_key(key, peerName, port, algorithm, opts) do
    keyCbOpts = :proplists.get_value(:key_cb_private, opts, [])
    sshFileCb = :proplists.get_value(:call_ssh_file, keyCbOpts, :ssh_file)
    sshFileCb.is_host_key(key, peerName, port, algorithm, opts)
  end

  def user_key(algorithm, opts) do
    keyCbOpts = :proplists.get_value(:key_cb_private, opts, [])
    request = r_ssh_agent_identities_request()
    response = :ssh_agent.send(request, keyCbOpts)
    r_ssh_agent_identities_response(keys: keys) = response
    algorithmStr = :erlang.atom_to_list(algorithm)

    matchingKeys =
      :lists.filter(
        fn key ->
          has_key_type(key, algorithmStr)
        end,
        keys
      )

    case matchingKeys do
      [r_ssh_agent_key(blob: pubKeyBlob) | _OtherKeys] ->
        {:ok, {:ssh2_pubkey, pubKeyBlob}}

      _ ->
        {:error, :enoent}
    end
  end

  def sign(pubKeyBlob, sigData, opts) do
    keyCbOpts = :proplists.get_value(:key_cb_private, opts, [])
    signFlags = 2 ||| 4
    signRequest = r_ssh_agent_sign_request(key_blob: pubKeyBlob, data: sigData, flags: signFlags)
    signResponse = :ssh_agent.send(signRequest, keyCbOpts)
    r_ssh_agent_sign_response(signature: r_ssh_agent_signature(blob: blob)) = signResponse
    blob
  end

  defp has_key_type(r_ssh_agent_key(blob: keyBlob), type) do
    <<_KeyTypeLen::size(32)-unsigned-big-integer, keyType::size(_KeyTypeLen)-binary,
      _KeyBlobRest::binary>> = keyBlob

    :erlang.binary_to_list(keyType) == type
  end

  def send(request, opts) do
    socketPath = :proplists.get_value(:socket_path, opts, :os.getenv('SSH_AUTH_SOCK'))
    timeout = :proplists.get_value(:timeout, opts, 1000)
    connectOpts = [:binary, {:packet, 0}, {:active, false}]
    {:ok, socket} = :gen_tcp.connect({:local, socketPath}, 0, connectOpts, timeout)
    binRequest = pack(encode(request))
    :ok = :gen_tcp.send(socket, binRequest)
    {:ok, <<len::size(32)-unsigned-big-integer>>} = :gen_tcp.recv(socket, 4, timeout)
    {:ok, binResponse} = :gen_tcp.recv(socket, len, timeout)
    :ok = :gen_tcp.close(socket)
    response = decode(binResponse)
    response
  end

  defp pack(data) do
    <<:erlang.size(data)::size(32)-unsigned-big-integer, data::binary>>
  end

  defp encode(r_ssh_agent_identities_request()) do
    <<11::size(8)-unsigned-big-integer>>
  end

  defp encode(r_ssh_agent_sign_request(key_blob: keyBlob, data: data, flags: flags)) do
    <<13::size(8)-unsigned-big-integer,
      :erlang.size(
        cond do
          is_binary(keyBlob) ->
            keyBlob

          is_list(keyBlob) ->
            :erlang.list_to_binary(keyBlob)

          keyBlob == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(keyBlob) ->
          keyBlob

        is_list(keyBlob) ->
          :erlang.list_to_binary(keyBlob)

        keyBlob == :undefined ->
          <<>>
      end::binary,
      :erlang.size(
        cond do
          is_binary(data) ->
            data

          is_list(data) ->
            :erlang.list_to_binary(data)

          data == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(data) ->
          data

        is_list(data) ->
          :erlang.list_to_binary(data)

        data == :undefined ->
          <<>>
      end::binary, flags::size(32)-unsigned-big-integer>>
  end

  defp decode_keys(<<>>, acc, 0) do
    :lists.reverse(acc)
  end

  defp decode_keys(
         <<_KeyBlobLen::size(32)-unsigned-big-integer, keyBlob::size(_KeyBlobLen)-binary,
           _CommentLen::size(32)-unsigned-big-integer, comment::size(_CommentLen)-binary,
           rest::binary>>,
         acc,
         n
       ) do
    key = r_ssh_agent_key(blob: keyBlob, comment: comment)
    decode_keys(rest, [key | acc], n - 1)
  end

  defp decode_signature(
         <<_FormatLen::size(32)-unsigned-big-integer, format::size(_FormatLen)-binary,
           blob::binary>>
       ) do
    <<_SignatureBlobLen::size(32)-unsigned-big-integer,
      signatureBlob::size(_SignatureBlobLen)-binary>> = blob

    r_ssh_agent_signature(format: format, blob: signatureBlob)
  end

  defp decode(<<6::size(8)-unsigned-big-integer>>) do
    r_ssh_agent_success()
  end

  defp decode(<<5::size(8)-unsigned-big-integer>>) do
    r_ssh_agent_failure()
  end

  defp decode(
         <<12::size(8)-unsigned-big-integer, numKeys::size(32)-unsigned-big-integer,
           keyData::binary>>
       ) do
    r_ssh_agent_identities_response(keys: decode_keys(keyData, [], numKeys))
  end

  defp decode(
         <<14::size(8)-unsigned-big-integer, _SignatureLen::size(32)-unsigned-big-integer,
           signature::size(_SignatureLen)-binary>>
       ) do
    r_ssh_agent_sign_response(signature: decode_signature(signature))
  end
end
