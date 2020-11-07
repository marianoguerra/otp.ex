defmodule :m_pubkey_cert_records do
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

  def decode_cert(derCert) do
    {:ok, cert} =
      :OTP - PUB -
        KEY.decode(
          :OTPCertificate,
          derCert
        )

    r_OTPCertificate(tbsCertificate: tBS) = cert
    {:ok, r_OTPCertificate(cert, tbsCertificate: decode_tbs(tBS))}
  end

  def transform(r_OTPCertificate(tbsCertificate: tBS) = cert, :encode) do
    r_OTPCertificate(cert, tbsCertificate: encode_tbs(tBS))
  end

  def transform(r_OTPCertificate(tbsCertificate: tBS) = cert, :decode) do
    r_OTPCertificate(cert, tbsCertificate: decode_tbs(tBS))
  end

  def transform(r_OTPTBSCertificate() = tBS, :encode) do
    encode_tbs(tBS)
  end

  def transform(r_OTPTBSCertificate() = tBS, :decode) do
    decode_tbs(tBS)
  end

  def transform(r_AttributeTypeAndValue(type: id, value: value0) = aTAV, func) do
    {:ok, value} =
      case attribute_type(id) do
        :X520countryName when func == :decode ->
          case apply(:"OTP-PUB-KEY", func, [:"OTP-X520countryname", value0]) do
            {:ok, {:utf8String, utf8Value}} ->
              {:ok, :unicode.characters_to_list(utf8Value)}

            {:ok, {:printableString, aSCCI}} ->
              {:ok, aSCCI}
          end

        :EmailAddress when func == :decode ->
          case apply(:"OTP-PUB-KEY", func, [:"OTP-emailAddress", value0]) do
            {:ok, {:utf8String, utf8Value}} ->
              {:ok, :unicode.characters_to_list(utf8Value)}

            {:ok, {:ia5String, ia5Value}} ->
              {:ok, ia5Value}
          end

        type when is_atom(type) ->
          apply(:"OTP-PUB-KEY", func, [type, value0])

        _UnknownType ->
          {:ok, value0}
      end

    r_AttributeTypeAndValue(aTAV, value: value)
  end

  def transform(aKI = r_AuthorityKeyIdentifier(authorityCertIssuer: aCI), func) do
    r_AuthorityKeyIdentifier(aKI, authorityCertIssuer: transform(aCI, func))
  end

  def transform(list = [{:directoryName, _}], func) do
    for {:directoryName, value} <- list do
      {:directoryName, transform(value, func)}
    end
  end

  def transform({:directoryName, value}, func) do
    {:directoryName, transform(value, func)}
  end

  def transform({:rdnSequence, seqList}, func)
      when is_list(seqList) do
    {:rdnSequence,
     :lists.map(
       fn seq ->
         :lists.map(
           fn element ->
             transform(element, func)
           end,
           seq
         )
       end,
       seqList
     )}
  end

  def transform(
        r_NameConstraints(
          permittedSubtrees: permitted,
          excludedSubtrees: excluded
        ),
        func
      ) do
    r_NameConstraints(
      permittedSubtrees:
        transform_sub_tree(
          permitted,
          func
        ),
      excludedSubtrees: transform_sub_tree(excluded, func)
    )
  end

  def transform(other, _) do
    other
  end

  defp decode_supportedPublicKey(
         r_OTPSubjectPublicKeyInfo(
           algorithm: pA = r_PublicKeyAlgorithm(algorithm: algo),
           subjectPublicKey: sPK0
         )
       ) do
    type = supportedPublicKeyAlgorithms(algo)

    sPK =
      case type do
        :ECPoint ->
          r_ECPoint(point: sPK0)

        _ ->
          {:ok, sPK1} = :OTP - PUB - KEY.decode(type, sPK0)
          sPK1
      end

    r_OTPSubjectPublicKeyInfo(subjectPublicKey: sPK, algorithm: pA)
  end

  defp encode_supportedPublicKey(
         r_OTPSubjectPublicKeyInfo(
           algorithm: pA = r_PublicKeyAlgorithm(algorithm: algo),
           subjectPublicKey: sPK0
         )
       ) do
    type = supportedPublicKeyAlgorithms(algo)

    sPK =
      case type do
        :ECPoint ->
          r_ECPoint(sPK0, :point)

        _ ->
          {:ok, sPK1} = :OTP - PUB - KEY.encode(type, sPK0)
          sPK1
      end

    r_OTPSubjectPublicKeyInfo(subjectPublicKey: sPK, algorithm: pA)
  end

  defp decode_extensions(:asn1_NOVALUE) do
    :asn1_NOVALUE
  end

  defp decode_extensions(exts) do
    :lists.map(
      fn ext = r_Extension(extnID: id, extnValue: value0) ->
        case extension_id(id) do
          :undefined ->
            ext

          type ->
            {:ok, value} =
              :OTP - PUB -
                KEY.decode(
                  type,
                  :erlang.iolist_to_binary(value0)
                )

            r_Extension(ext, extnValue: transform(value, :decode))
        end
      end,
      exts
    )
  end

  defp encode_extensions(:asn1_NOVALUE) do
    :asn1_NOVALUE
  end

  defp encode_extensions(exts) do
    :lists.map(
      fn ext = r_Extension(extnID: id, extnValue: value0) ->
        case extension_id(id) do
          :undefined ->
            ext

          type ->
            value1 = transform(value0, :encode)
            {:ok, value} = :OTP - PUB - KEY.encode(type, value1)
            r_Extension(ext, extnValue: value)
        end
      end,
      exts
    )
  end

  defp encode_tbs(
         tBS =
           r_OTPTBSCertificate(
             issuer: issuer0,
             subject: subject0,
             subjectPublicKeyInfo: spki0,
             extensions: exts0
           )
       ) do
    issuer = transform(issuer0, :encode)
    subject = transform(subject0, :encode)
    spki = encode_supportedPublicKey(spki0)
    exts = encode_extensions(exts0)

    r_OTPTBSCertificate(tBS,
      issuer: issuer,
      subject: subject,
      subjectPublicKeyInfo: spki,
      extensions: exts
    )
  end

  defp decode_tbs(
         tBS =
           r_OTPTBSCertificate(
             issuer: issuer0,
             subject: subject0,
             subjectPublicKeyInfo: spki0,
             extensions: exts0
           )
       ) do
    issuer = transform(issuer0, :decode)
    subject = transform(subject0, :decode)
    spki = decode_supportedPublicKey(spki0)
    exts = decode_extensions(exts0)

    r_OTPTBSCertificate(tBS,
      issuer: issuer,
      subject: subject,
      subjectPublicKeyInfo: spki,
      extensions: exts
    )
  end

  defp transform_sub_tree(:asn1_NOVALUE, _) do
    :asn1_NOVALUE
  end

  defp transform_sub_tree(treeList, func) do
    for tree = r_GeneralSubtree(base: name) <- treeList do
      r_GeneralSubtree(tree, base: transform(name, func))
    end
  end
end
