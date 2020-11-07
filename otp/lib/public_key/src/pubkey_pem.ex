defmodule :m_pubkey_pem do
  use Bitwise
  require Record

  Record.defrecord(:r_SubjectPublicKeyInfoAlgorithm, :SubjectPublicKeyInfoAlgorithm,
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_path_validation_state, :path_validation_state,
    valid_policy_tree: :undefined,
    explicit_policy: :undefined,
    inhibit_any_policy: :undefined,
    policy_mapping: :undefined,
    cert_num: :undefined,
    last_cert: false,
    permitted_subtrees: :no_constraints,
    excluded_subtrees: [],
    working_public_key_algorithm: :undefined,
    working_public_key: :undefined,
    working_public_key_parameters: :undefined,
    working_issuer_name: :undefined,
    max_path_length: :undefined,
    verify_fun: :undefined,
    user_state: :undefined
  )

  Record.defrecord(:r_policy_tree_node, :policy_tree_node,
    valid_policy: :undefined,
    qualifier_set: :undefined,
    criticality_indicator: :undefined,
    expected_policy_set: :undefined
  )

  Record.defrecord(:r_revoke_state, :revoke_state,
    reasons_mask: :undefined,
    cert_status: :undefined,
    interim_reasons_mask: :undefined,
    valid_ext: :undefined,
    details: :undefined
  )

  Record.defrecord(:r_ECPoint, :ECPoint, point: :undefined)

  def decode(bin) do
    decode_pem_entries(
      :binary.split(bin, ["\r\n", "\r", "\n"], [:global]),
      []
    )
  end

  def encode(pemEntries) do
    encode_pem_entries(pemEntries)
  end

  def decipher(
        {_, decryptDer, {cipher, keyDevParams}},
        password
      ) do
    :pubkey_pbe.decode(decryptDer, password, cipher, keyDevParams)
  end

  def cipher(der, {cipher, keyDevParams}, password) do
    :pubkey_pbe.encode(der, password, cipher, keyDevParams)
  end

  defp encode_pem_entries(entries) do
    for entry <- entries do
      encode_pem_entry(entry)
    end
  end

  defp encode_pem_entry({type, der, :not_encrypted}) do
    startStr = pem_start(type)
    [startStr, '\n', b64encode_and_split(der), '\n', pem_end(startStr), '\n\n']
  end

  defp encode_pem_entry({:PrivateKeyInfo, der, encParams}) do
    encDer =
      encode_encrypted_private_keyinfo(
        der,
        encParams
      )

    startStr = pem_start(:EncryptedPrivateKeyInfo)
    [startStr, '\n', b64encode_and_split(encDer), '\n', pem_end(startStr), '\n\n']
  end

  defp encode_pem_entry({type, decrypted, {cipher, salt}}) do
    startStr = pem_start(type)

    [
      startStr,
      '\n',
      pem_decrypt(),
      '\n',
      pem_decrypt_info(cipher, salt),
      '\n\n',
      b64encode_and_split(decrypted),
      '\n',
      pem_end(startStr),
      '\n\n'
    ]
  end

  defp decode_pem_entries([], entries) do
    :lists.reverse(entries)
  end

  defp decode_pem_entries([<<>>], entries) do
    :lists.reverse(entries)
  end

  defp decode_pem_entries([<<>> | lines], entries) do
    decode_pem_entries(lines, entries)
  end

  defp decode_pem_entries([startLine | lines], entries) do
    start = strip_tail_whitespace(startLine)

    case pem_end(start) do
      :undefined ->
        decode_pem_entries(lines, entries)

      _End ->
        {entry, restLines} = join_entry(lines, [])

        decode_pem_entries(
          restLines,
          [decode_pem_entry(start, entry) | entries]
        )
    end
  end

  defp strip_tail_whitespace(bin) when is_binary(bin) do
    strip_tail_whitespace(:lists.reverse(:binary.bin_to_list(bin)))
  end

  defp strip_tail_whitespace([char | rest])
       when char == ?\s or
              char == ?\t or char == ?\v or char == ?\f or
              char == ?\r or char == ?\n do
    strip_tail_whitespace(rest)
  end

  defp strip_tail_whitespace(list) do
    :binary.list_to_bin(:lists.reverse(list))
  end

  defp decode_pem_entry(start, [[<<"Proc-Type: 4,ENCRYPTED", _::binary>>, line] | lines]) do
    type = asn1_type(start)
    cs = :erlang.iolist_to_binary(lines)
    decoded = :base64.mime_decode(cs)

    [_, dekInfo0] =
      :string.tokens(
        :erlang.binary_to_list(line),
        ': '
      )

    [cipher, salt] = :string.tokens(dekInfo0, ',')
    {type, decoded, {cipher, unhex(salt)}}
  end

  defp decode_pem_entry(start, lines) do
    type = asn1_type(start)
    cs = :erlang.iolist_to_binary(lines)
    decoded = :base64.mime_decode(cs)

    case type do
      :EncryptedPrivateKeyInfo ->
        decode_encrypted_private_keyinfo(decoded)

      _ ->
        {type, decoded, :not_encrypted}
    end
  end

  defp decode_encrypted_private_keyinfo(der) do
    r_EncryptedPrivateKeyInfo(
      encryptionAlgorithm: algorithmInfo,
      encryptedData: data
    ) =
      :public_key.der_decode(
        :EncryptedPrivateKeyInfo,
        der
      )

    decryptParams = :pubkey_pbe.decrypt_parameters(algorithmInfo)
    {:PrivateKeyInfo, data, decryptParams}
  end

  defp encode_encrypted_private_keyinfo(encData, encryptParmams) do
    algorithmInfo = :pubkey_pbe.encrypt_parameters(encryptParmams)

    :public_key.der_encode(
      :EncryptedPrivateKeyInfo,
      r_EncryptedPrivateKeyInfo(
        encryptionAlgorithm: algorithmInfo,
        encryptedData: encData
      )
    )
  end

  defp b64encode_and_split(bin) do
    split_lines(:base64.encode(bin))
  end

  defp split_lines(<<text::size(64)-binary>>) do
    [text]
  end

  defp split_lines(<<text::size(64)-binary, rest::binary>>) do
    [[text, ?\n] | split_lines(rest)]
  end

  defp split_lines(bin) do
    [bin]
  end

  defp join_entry([<<"-----END ", _::binary>> | lines], entry) do
    {:lists.reverse(entry), lines}
  end

  defp join_entry([<<"-----END X509 CRL-----", _::binary>> | lines], entry) do
    {:lists.reverse(entry), lines}
  end

  defp join_entry([line | lines], entry) do
    join_entry(lines, [line | entry])
  end

  defp unhex(s) do
    unhex(s, [])
  end

  defp unhex('', acc) do
    :erlang.list_to_binary(:lists.reverse(acc))
  end

  defp unhex([[d1, d2] | rest], acc) do
    unhex(
      rest,
      [:erlang.list_to_integer([d1, d2], 16) | acc]
    )
  end

  defp hexify(l) do
    for b <- :erlang.binary_to_list(l) do
      [hex_byte(b)]
    end
  end

  defp hex_byte(b) when b < 16 do
    ['0', :erlang.integer_to_list(b, 16)]
  end

  defp hex_byte(b) do
    :erlang.integer_to_list(b, 16)
  end

  defp pem_start(:Certificate) do
    "-----BEGIN CERTIFICATE-----"
  end

  defp pem_start(:RSAPrivateKey) do
    "-----BEGIN RSA PRIVATE KEY-----"
  end

  defp pem_start(:RSAPublicKey) do
    "-----BEGIN RSA PUBLIC KEY-----"
  end

  defp pem_start(:SubjectPublicKeyInfo) do
    "-----BEGIN PUBLIC KEY-----"
  end

  defp pem_start(:DSAPrivateKey) do
    "-----BEGIN DSA PRIVATE KEY-----"
  end

  defp pem_start(:DHParameter) do
    "-----BEGIN DH PARAMETERS-----"
  end

  defp pem_start(:PrivateKeyInfo) do
    "-----BEGIN PRIVATE KEY-----"
  end

  defp pem_start(:EncryptedPrivateKeyInfo) do
    "-----BEGIN ENCRYPTED PRIVATE KEY-----"
  end

  defp pem_start(:CertificationRequest) do
    "-----BEGIN CERTIFICATE REQUEST-----"
  end

  defp pem_start(:ContentInfo) do
    "-----BEGIN PKCS7-----"
  end

  defp pem_start(:CertificateList) do
    "-----BEGIN X509 CRL-----"
  end

  defp pem_start(:EcpkParameters) do
    "-----BEGIN EC PARAMETERS-----"
  end

  defp pem_start(:ECPrivateKey) do
    "-----BEGIN EC PRIVATE KEY-----"
  end

  defp pem_start({:no_asn1, :new_openssh}) do
    "-----BEGIN OPENSSH PRIVATE KEY-----"
  end

  defp pem_end("-----BEGIN CERTIFICATE-----") do
    "-----END CERTIFICATE-----"
  end

  defp pem_end("-----BEGIN RSA PRIVATE KEY-----") do
    "-----END RSA PRIVATE KEY-----"
  end

  defp pem_end("-----BEGIN RSA PUBLIC KEY-----") do
    "-----END RSA PUBLIC KEY-----"
  end

  defp pem_end("-----BEGIN PUBLIC KEY-----") do
    "-----END PUBLIC KEY-----"
  end

  defp pem_end("-----BEGIN DSA PRIVATE KEY-----") do
    "-----END DSA PRIVATE KEY-----"
  end

  defp pem_end("-----BEGIN DH PARAMETERS-----") do
    "-----END DH PARAMETERS-----"
  end

  defp pem_end("-----BEGIN PRIVATE KEY-----") do
    "-----END PRIVATE KEY-----"
  end

  defp pem_end("-----BEGIN ENCRYPTED PRIVATE KEY-----") do
    "-----END ENCRYPTED PRIVATE KEY-----"
  end

  defp pem_end("-----BEGIN CERTIFICATE REQUEST-----") do
    "-----END CERTIFICATE REQUEST-----"
  end

  defp pem_end("-----BEGIN PKCS7-----") do
    "-----END PKCS7-----"
  end

  defp pem_end("-----BEGIN X509 CRL-----") do
    "-----END X509 CRL-----"
  end

  defp pem_end("-----BEGIN EC PARAMETERS-----") do
    "-----END EC PARAMETERS-----"
  end

  defp pem_end("-----BEGIN EC PRIVATE KEY-----") do
    "-----END EC PRIVATE KEY-----"
  end

  defp pem_end("-----BEGIN OPENSSH PRIVATE KEY-----") do
    "-----END OPENSSH PRIVATE KEY-----"
  end

  defp pem_end(_) do
    :undefined
  end

  defp asn1_type("-----BEGIN CERTIFICATE-----") do
    :Certificate
  end

  defp asn1_type("-----BEGIN RSA PRIVATE KEY-----") do
    :RSAPrivateKey
  end

  defp asn1_type("-----BEGIN RSA PUBLIC KEY-----") do
    :RSAPublicKey
  end

  defp asn1_type("-----BEGIN PUBLIC KEY-----") do
    :SubjectPublicKeyInfo
  end

  defp asn1_type("-----BEGIN DSA PRIVATE KEY-----") do
    :DSAPrivateKey
  end

  defp asn1_type("-----BEGIN DH PARAMETERS-----") do
    :DHParameter
  end

  defp asn1_type("-----BEGIN PRIVATE KEY-----") do
    :PrivateKeyInfo
  end

  defp asn1_type("-----BEGIN ENCRYPTED PRIVATE KEY-----") do
    :EncryptedPrivateKeyInfo
  end

  defp asn1_type("-----BEGIN CERTIFICATE REQUEST-----") do
    :CertificationRequest
  end

  defp asn1_type("-----BEGIN PKCS7-----") do
    :ContentInfo
  end

  defp asn1_type("-----BEGIN X509 CRL-----") do
    :CertificateList
  end

  defp asn1_type("-----BEGIN EC PARAMETERS-----") do
    :EcpkParameters
  end

  defp asn1_type("-----BEGIN EC PRIVATE KEY-----") do
    :ECPrivateKey
  end

  defp asn1_type("-----BEGIN OPENSSH PRIVATE KEY-----") do
    {:no_asn1, :new_openssh}
  end

  defp pem_decrypt() do
    "Proc-Type: 4,ENCRYPTED"
  end

  defp pem_decrypt_info(cipher, salt) do
    :io_lib.format(
      'DEK-Info: ~s,~s',
      [cipher, :lists.flatten(hexify(salt))]
    )
  end
end
