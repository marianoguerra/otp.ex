defmodule :m_pubkey_pbe do
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

  def encode(data, password, 'DES-CBC' = cipher, keyDevParams) do
    {key, iV} = password_to_key_and_iv(password, cipher, keyDevParams)
    :crypto.crypto_one_time(:des_cbc, key, iV, pbe_pad(data, block_size(:des_cbc)), true)
  end

  def encode(data, password, 'DES-EDE3-CBC' = cipher, keyDevParams) do
    {key, iV} = password_to_key_and_iv(password, cipher, keyDevParams)

    :crypto.crypto_one_time(
      :des_ede3_cbc,
      key,
      iV,
      pbe_pad(data, block_size(:des_ede3_cbc)),
      true
    )
  end

  def encode(data, password, 'RC2-CBC' = cipher, keyDevParams) do
    {key, iV} = password_to_key_and_iv(password, cipher, keyDevParams)
    :crypto.crypto_one_time(:rc2_cbc, key, iV, pbe_pad(data, block_size(:rc2_cbc)), true)
  end

  def encode(data, password, 'AES-128-CBC' = cipher, keyDevParams) do
    {key, iV} = password_to_key_and_iv(password, cipher, keyDevParams)
    :crypto.crypto_one_time(:aes_128_cbc, key, iV, pbe_pad(data, block_size(:aes_128_cbc)), true)
  end

  def encode(data, password, 'AES-192-CBC' = cipher, keyDevParams) do
    {key, iV} = password_to_key_and_iv(password, cipher, keyDevParams)
    :crypto.crypto_one_time(:aes_192_cbc, key, iV, pbe_pad(data, block_size(:aes_192_cbc)), true)
  end

  def encode(data, password, 'AES-256-CBC' = cipher, keyDevParams) do
    {key, iV} = password_to_key_and_iv(password, cipher, keyDevParams)
    :crypto.crypto_one_time(:aes_256_cbc, key, iV, pbe_pad(data, block_size(:aes_256_cbc)), true)
  end

  def decode(data, password, 'DES-CBC' = cipher, keyDevParams) do
    {key, iV} = password_to_key_and_iv(password, cipher, keyDevParams)
    :crypto.crypto_one_time(:des_cbc, key, iV, data, false)
  end

  def decode(data, password, 'DES-EDE3-CBC' = cipher, keyDevParams) do
    {key, iV} = password_to_key_and_iv(password, cipher, keyDevParams)
    :crypto.crypto_one_time(:des_ede3_cbc, key, iV, data, false)
  end

  def decode(data, password, 'RC2-CBC' = cipher, keyDevParams) do
    {key, iV} = password_to_key_and_iv(password, cipher, keyDevParams)
    :crypto.crypto_one_time(:rc2_cbc, key, iV, data, false)
  end

  def decode(data, password, 'AES-128-CBC' = cipher, keyDevParams) do
    {key, iV} = password_to_key_and_iv(password, cipher, keyDevParams)
    :crypto.crypto_one_time(:aes_128_cbc, key, iV, data, false)
  end

  def decode(data, password, 'AES-192-CBC' = cipher, keyDevParams) do
    {key, iV} = password_to_key_and_iv(password, cipher, keyDevParams)
    :crypto.crypto_one_time(:aes_192_cbc, key, iV, data, false)
  end

  def decode(data, password, 'AES-256-CBC' = cipher, keyDevParams) do
    {key, iV} = password_to_key_and_iv(password, cipher, keyDevParams)
    :crypto.crypto_one_time(:aes_256_cbc, key, iV, data, false)
  end

  def pbdkdf1(_, _, 0, acc) do
    acc
  end

  def pbdkdf1(password, salt, count, hash) do
    result = :crypto.hash(hash, [password, salt])
    do_pbdkdf1(result, count - 1, result, hash)
  end

  def pbdkdf2(password, salt, count, derivedKeyLen, prf, prfHash, prfOutputLen) do
    numBlocks = ceiling(derivedKeyLen / prfOutputLen)
    numLastBlockOctets = derivedKeyLen - (numBlocks - 1) * prfOutputLen

    blocks(
      numBlocks,
      numLastBlockOctets,
      1,
      password,
      salt,
      count,
      prf,
      prfHash,
      prfOutputLen,
      <<>>
    )
  end

  def decrypt_parameters(
        r_EncryptedPrivateKeyInfo_encryptionAlgorithm(algorithm: oid, parameters: param)
      ) do
    decrypt_parameters(
      oid,
      decode_handle_open_type_wrapper(param)
    )
  end

  def encrypt_parameters({cipher, params}) do
    encrypt_parameters(cipher, params)
  end

  defp password_to_key_and_iv(password, _, r_PBES2_params() = params) do
    {salt, itrCount, keyLen, pseudoRandomFunction, pseudoHash, pseudoOtputLen, iV} =
      key_derivation_params(params)

    <<key::size(keyLen)-binary, _::binary>> =
      pbdkdf2(password, salt, itrCount, keyLen, pseudoRandomFunction, pseudoHash, pseudoOtputLen)

    {key, iV}
  end

  defp password_to_key_and_iv(
         password,
         _Cipher,
         {r_PBEParameter(salt: salt, iterationCount: count), hash}
       ) do
    <<key::size(8)-binary, iV::size(8)-binary, _::binary>> = pbdkdf1(password, salt, count, hash)
    {key, iV}
  end

  defp password_to_key_and_iv(password, cipher, keyDevParams) do
    <<salt::size(8)-binary, _::binary>> = keyDevParams
    keyLen = derived_key_length(cipher, :undefined)

    <<key::size(keyLen)-binary, _::binary>> =
      pem_encrypt(<<>>, password, salt, ceiling(div(keyLen, 16)), <<>>, :md5)

    {key, keyDevParams}
  end

  defp pem_encrypt(_, _, _, 0, acc, _) do
    acc
  end

  defp pem_encrypt(prev, password, salt, count, acc, hash) do
    result = :crypto.hash(hash, [prev, password, salt])
    pem_encrypt(result, password, salt, count - 1, <<acc::binary, result::binary>>, hash)
  end

  defp do_pbdkdf1(_, 0, acc, _) do
    acc
  end

  defp do_pbdkdf1(prev, count, acc, hash) do
    result = :crypto.hash(hash, prev)
    do_pbdkdf1(result, count - 1, <<result::binary, acc::binary>>, hash)
  end

  defp blocks(1, n, index, password, salt, count, prf, prfHash, prfLen, acc) do
    <<xorSum::size(n)-binary, _::binary>> =
      xor_sum(password, salt, count, index, prf, prfHash, prfLen)

    <<acc::binary, xorSum::binary>>
  end

  defp blocks(numBlocks, n, index, password, salt, count, prf, prfHash, prfLen, acc) do
    xorSum = xor_sum(password, salt, count, index, prf, prfHash, prfLen)

    blocks(
      numBlocks - 1,
      n,
      index + 1,
      password,
      salt,
      count,
      prf,
      prfHash,
      prfLen,
      <<acc::binary, xorSum::binary>>
    )
  end

  defp xor_sum(password, salt, count, index, prf, prfHash, prfLen) do
    result = prf.(prfHash, password, [salt, <<index::size(32)-unsigned-big-integer>>], prfLen)
    do_xor_sum(prf, prfHash, prfLen, result, password, count - 1, result)
  end

  defp do_xor_sum(_, _, _, _, _, 0, acc) do
    acc
  end

  defp do_xor_sum(prf, prfHash, prfLen, prev, password, count, acc) do
    result = prf.(prfHash, password, prev, prfLen)
    do_xor_sum(prf, prfHash, prfLen, result, password, count - 1, :crypto.exor(acc, result))
  end

  defp pbe_pad(data, blockSize) do
    n = blockSize - rem(:erlang.byte_size(data), blockSize)
    pad = :binary.copy(<<n>>, n)
    <<data::binary, pad::binary>>
  end

  defp hmac4(subType, key, data, macLength) do
    :crypto.macN(:hmac, subType, key, data, macLength)
  end

  defp block_size(cipher)
       when cipher == :rc2_cbc or
              cipher == :des_cbc or cipher == :des_ede3_cbc do
    8
  end

  defp block_size(cipher)
       when cipher == :aes_128_cbc or
              cipher == :aes_192_cbc or cipher == :aes_256_cbc do
    16
  end

  defp ceiling(float) do
    :erlang.round(float + 0.5)
  end

  defp decode_handle_open_type_wrapper({:asn1_OPENTYPE, type}) do
    type
  end

  defp encode_handle_open_type_wrapper(type) do
    {:asn1_OPENTYPE, type}
  end
end
