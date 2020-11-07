defmodule :m_dtls_v1 do
  use Bitwise
  require Record

  Record.defrecord(:r_stateless_ticket, :stateless_ticket,
    hash: :undefined,
    pre_shared_key: :undefined,
    ticket_age_add: :undefined,
    lifetime: :undefined,
    timestamp: :undefined
  )

  Record.defrecord(:r_change_cipher_spec, :change_cipher_spec, type: 1)

  Record.defrecord(:r_cipher_state, :cipher_state,
    iv: :undefined,
    key: :undefined,
    finished_key: :undefined,
    state: :undefined,
    nonce: :undefined,
    tag_len: :undefined
  )

  def suites(minor) do
    :lists.filter(
      fn cipher ->
        is_acceptable_cipher(:ssl_cipher_format.suite_bin_to_map(cipher))
      end,
      :tls_v1.suites(corresponding_minor_tls_version(minor))
    )
  end

  def all_suites(version) do
    :lists.filter(
      fn cipher ->
        is_acceptable_cipher(:ssl_cipher_format.suite_bin_to_map(cipher))
      end,
      :ssl_cipher.all_suites(corresponding_tls_version(version))
    )
  end

  def anonymous_suites(version) do
    :lists.filter(
      fn cipher ->
        is_acceptable_cipher(:ssl_cipher_format.suite_bin_to_map(cipher))
      end,
      :ssl_cipher.anonymous_suites(corresponding_tls_version(version))
    )
  end

  def hmac_hash(macAlg, macSecret, value) do
    :tls_v1.hmac_hash(macAlg, macSecret, value)
  end

  def ecc_curves({_Major, minor}) do
    :tls_v1.ecc_curves(corresponding_minor_tls_version(minor))
  end

  def corresponding_tls_version({254, minor}) do
    {3, corresponding_minor_tls_version(minor)}
  end

  def cookie_secret() do
    :crypto.strong_rand_bytes(32)
  end

  def cookie_timeout() do
    round(:rand.uniform() * 30000 / 2)
  end

  defp corresponding_minor_tls_version(255) do
    2
  end

  defp corresponding_minor_tls_version(253) do
    3
  end

  def corresponding_dtls_version({3, minor}) do
    {254, corresponding_minor_dtls_version(minor)}
  end

  defp corresponding_minor_dtls_version(2) do
    255
  end

  defp corresponding_minor_dtls_version(3) do
    253
  end

  defp is_acceptable_cipher(suite) do
    not :ssl_cipher.is_stream_ciphersuite(suite)
  end
end
