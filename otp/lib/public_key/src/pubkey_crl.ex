defmodule :m_pubkey_crl do
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

  Record.defrecord(:r_userstate, :userstate,
    dpcrls: :undefined,
    idp: :undefined
  )

  def init_revokation_state() do
    r_revoke_state(
      reasons_mask: :sets.new(),
      interim_reasons_mask: :sets.new(),
      cert_status: :unrevoked,
      details: []
    )
  end

  def fresh_crl(_, {:undefined, :undefined}, _) do
    :no_fresh_crl
  end

  def fresh_crl(dP, {_, r_CertificateList(tbsCertList: tBSCRL)} = cRL, callBack) do
    now = :calendar.datetime_to_gregorian_seconds(:calendar.universal_time())
    updateTime = :pubkey_cert.time_str_2_gregorian_sec(r_TBSCertList(tBSCRL, :nextUpdate))

    case now >= updateTime do
      true ->
        case callBack.(dP, cRL) do
          ^cRL ->
            :no_fresh_crl

          newCRL ->
            fresh_crl(dP, newCRL, callBack)
        end

      false ->
        {:fresh, cRL}
    end
  end

  def combines(cRL, deltaCRL) do
    check_crl_num(
      cRL,
      deltaCRL
    ) and
      check_delta_issuer_and_scope(
        cRL,
        deltaCRL
      )
  end

  defp crl_status(state) do
    crl_status(state, &all_reasons/0)
  end

  defp crl_status(
         {:skip, r_revoke_state(cert_status: status) = revokedState},
         _
       ) do
    {:undetermined, status(status), revokedState}
  end

  defp crl_status(
         r_revoke_state(
           cert_status: :unrevoked = status,
           valid_ext: false
         ) = revokedState,
         _
       ) do
    {:undetermined, status, revokedState}
  end

  defp crl_status(r_revoke_state(cert_status: status, valid_ext: false), _) do
    {:finished, status(status)}
  end

  defp crl_status(
         r_revoke_state(reasons_mask: mask, cert_status: status, valid_ext: true) = revokedState,
         fun
       ) do
    case is_all_reasons(mask, fun) do
      true ->
        {:finished, status(status)}

      false when status == :unrevoked ->
        {:undetermined, status, revokedState}

      _ ->
        {:finished, status(status)}
    end
  end

  defp verify_crl(otpCert, dP, cRL, derCRL, deltaCRL, derDeltaCRL, otherDPCRLs, options, state0) do
    r_CertificateList(
      tbsCertList:
        r_TBSCertList(
          crlExtensions: extensions,
          revokedCertificates: tmpRevoked
        )
    ) = cRL

    revoked = revoked(tmpRevoked)
    iDP = issuing_distribution_point(extensions)
    deltaRevoked = delta_revoked(deltaCRL)

    validExt =
      :erlang.and(
        verify_extensions(extensions),
        verify_extensions(revoked)
      )

    intMask = compute_interim_reasons_mask(dP, iDP)

    revokedState =
      r_revoke_state(state0,
        interim_reasons_mask: intMask,
        valid_ext: validExt
      )

    {fun, additionalArgs} =
      issuerFun =
      :proplists.get_value(
        :issuer_fun,
        options
      )

    try do
      verify_issuer_and_scope(otpCert, dP, iDP, cRL)
    catch
      {:bad_crl, :invalid_issuer} = reason ->
        details = r_revoke_state(revokedState, :details)
        {:invalid, {:skip, r_revoke_state(revokedState, details: [{reason, cRL} | details])}}

      reason ->
        details = r_revoke_state(revokedState, :details)
        {:invalid, r_revoke_state(revokedState, details: [{reason, cRL} | details])}
    else
      {:ok, issuer} ->
        case fun.(dP, cRL, issuer, additionalArgs) do
          {:ok, trustedOtpCert, path} ->
            verify_mask_and_signatures(
              revoked,
              deltaRevoked,
              revokedState,
              cRL,
              derCRL,
              deltaCRL,
              derDeltaCRL,
              issuerFun,
              trustedOtpCert,
              path,
              otherDPCRLs,
              iDP
            )

          _ ->
            details = r_revoke_state(revokedState, :details)

            {:invalid,
             r_revoke_state(revokedState,
               valid_ext: validExt,
               details: [
                 {{:bad_crl, :no_issuer_cert_chain}, cRL}
                 | details
               ]
             )}
        end

      {:error, :issuer_not_found} ->
        case fun.(dP, cRL, :issuer_not_found, additionalArgs) do
          {:ok, trustedOtpCert, path} ->
            verify_mask_and_signatures(
              revoked,
              deltaRevoked,
              revokedState,
              cRL,
              derCRL,
              deltaCRL,
              derDeltaCRL,
              issuerFun,
              trustedOtpCert,
              path,
              otherDPCRLs,
              iDP
            )

          _ ->
            details = r_revoke_state(state0, :details)

            {:invalid,
             {:skip,
              r_revoke_state(state0,
                details: [
                  {{:bad_crl, :no_issuer_cert_chain}, cRL}
                  | details
                ]
              )}}
        end
    end
  end

  defp verify_mask_and_signatures(
         revoked,
         deltaRevoked,
         revokedState,
         cRL,
         derCRL,
         deltaCRL,
         derDeltaCRL,
         issuerFun,
         trustedOtpCert,
         path,
         otherDPCRLs,
         iDP
       ) do
    reasonsMask =
      :sets.union(
        r_revoke_state(revokedState, :reasons_mask),
        r_revoke_state(revokedState, :interim_reasons_mask)
      )

    try do
      verify_interim_reasons_mask(revokedState)

      true =
        verify_crl_signatures(
          cRL,
          derCRL,
          deltaCRL,
          derDeltaCRL,
          trustedOtpCert,
          path,
          issuerFun,
          otherDPCRLs,
          iDP
        )

      {:valid, revoked, deltaRevoked, r_revoke_state(revokedState, reasons_mask: reasonsMask),
       iDP}
    catch
      reason ->
        details = r_revoke_state(revokedState, :details)
        {:invalid, r_revoke_state(revokedState, details: [{reason, cRL} | details])}

      :error, {:badmatch, _} ->
        details = r_revoke_state(revokedState, :details)

        {:invalid,
         r_revoke_state(revokedState,
           details: [
             {{:bad_crl, :invalid_signature}, cRL}
             | details
           ]
         )}
    end
  end

  defp verify_crl_signatures(
         cRL,
         derCRL,
         deltaCRL,
         derDeltaCRL,
         trustedOtpCert,
         path,
         issuerFun,
         otherDPCRLs,
         iDP
       ) do
    try do
      verifyFunAndState =
        {fn
           _, {:bad_cert, _} = reason, _UserState ->
             {:fail, reason}

           _, {:extension, _}, userState ->
             {:unknown, userState}

           _Cert, :valid, userState ->
             {:valid, userState}

           cert, :valid_peer, userState ->
             case verify_crl_keybit(cert, :cRLSign) do
               true ->
                 handle_crlsigner(cert, issuerFun, userState)

               false ->
                 {:fail, :crl_sign_bit_not_set}
             end
         end, r_userstate(dpcrls: otherDPCRLs, idp: iDP)}

      {:ok, {{_, key, keyParams}, _}} =
        :public_key.pkix_path_validation(
          trustedOtpCert,
          path,
          [{:verify_fun, verifyFunAndState}]
        )

      true = verify_crl_signature(cRL, derCRL, key, keyParams)
      true = verify_crl_signature(deltaCRL, derDeltaCRL, key, keyParams)
    catch
      :error, {:badmatch, _} ->
        false
    end
  end

  defp handle_crlsigner(otpCert, issuerFun, r_userstate(idp: iDP) = userState) do
    case verify_crl_keybit(otpCert, :keyCertSign) do
      true ->
        {:valid, userState}

      false ->
        case not is_indirect_crl(iDP) and not :public_key.pkix_is_self_signed(otpCert) do
          true ->
            validate_crl_signing_cert(otpCert, issuerFun, userState)

          false ->
            {:valid, userState}
        end
    end
  end

  defp validate_crl_signing_cert(_, _, r_userstate(dpcrls: []) = userState) do
    {:valid, userState}
  end

  defp validate_crl_signing_cert(otpCert, issuerFun, r_userstate(dpcrls: cRLInfo) = userState) do
    case :public_key.pkix_crls_validate(otpCert, cRLInfo, [{:issuer_fun, issuerFun}]) do
      :valid ->
        {:valid, userState}

      reason ->
        {:fail, reason}
    end
  end

  defp delta_revoked(:undefined) do
    []
  end

  defp delta_revoked(
         r_CertificateList(tbsCertList: r_TBSCertList(revokedCertificates: deltaRevoked))
       ) do
    revoked(deltaRevoked)
  end

  defp revoked(:asn1_NOVALUE) do
    []
  end

  defp revoked(revoked) do
    revoked
  end

  defp revoked_status(
         dP,
         iDP,
         cRLIssuer,
         names,
         serialNumber,
         revoked,
         deltaRevoked,
         revokedState0
       ) do
    defaultIssuer0 = default_issuer(cRLIssuer, deltaRevoked)

    revokedState1 =
      check_revoked(dP, iDP, defaultIssuer0, names, serialNumber, deltaRevoked, revokedState0)

    revokedState =
      case r_revoke_state(revokedState1, :cert_status) do
        :unrevoked
        when r_revoke_state(revokedState1, :cert_status) !== :removeFromCRL ->
          defaultIssuer = default_issuer(cRLIssuer, revoked)
          check_revoked(dP, iDP, defaultIssuer, names, serialNumber, revoked, revokedState1)

        _ ->
          revokedState1
      end

    case r_revoke_state(revokedState, :cert_status) do
      :removeFromCRL ->
        r_revoke_state(revokedState, cert_status: :unrevoked)

      _ ->
        revokedState
    end
  end

  defp is_all_reasons(mask, allReasonsFun) do
    allReasons = allReasonsFun.()

    case :sets.is_subset(allReasons, mask) do
      true ->
        true

      false ->
        :sets.is_subset(
          :sets.del_element(
            :unspecified,
            allReasons
          ),
          mask
        )
    end
  end

  defp all_reasons() do
    :sets.from_list([
      :unspecified,
      :keyCompromise,
      :cACompromise,
      :affiliationChanged,
      :superseded,
      :cessationOfOperation,
      :certificateHold,
      :privilegeWithdrawn,
      :aACompromise
    ])
  end

  defp verify_issuer_and_scope(
         r_OTPCertificate(tbsCertificate: tBSCert) = cert,
         r_DistributionPoint(cRLIssuer: dPIssuer) = dP,
         iDP,
         r_CertificateList(tbsCertList: tBSCRL) = cRL
       )
       when dPIssuer !== :asn1_NOVALUE do
    cRLIssuer =
      :pubkey_cert_records.transform(
        r_TBSCertList(tBSCRL, :issuer),
        :decode
      )

    issuer = dp_crlissuer_to_issuer(dPIssuer)

    case :erlang.and(
           :pubkey_cert.is_issuer(
             issuer,
             cRLIssuer
           ),
           is_indirect_crl(iDP)
         ) do
      true ->
        verify_scope(cert, dP, iDP)
        issuer_id(cert, cRL)

      false ->
        verify_issuer_and_scope(
          cert,
          r_DistributionPoint(dP,
            distributionPoint: [r_OTPTBSCertificate(tBSCert, :issuer)],
            cRLIssuer: :asn1_NOVALUE
          ),
          iDP,
          cRL
        )
    end
  end

  defp verify_issuer_and_scope(
         r_OTPCertificate(tbsCertificate: tBSCert) = cert,
         dP,
         iDP,
         r_CertificateList(tbsCertList: tBSCRL)
       ) do
    cRLIssuer =
      :pubkey_cert_records.transform(
        r_TBSCertList(tBSCRL, :issuer),
        :decode
      )

    certIssuer = r_OTPTBSCertificate(tBSCert, :issuer)

    case :pubkey_cert.is_issuer(certIssuer, cRLIssuer) do
      true ->
        verify_scope(cert, dP, iDP)
        issuer_id(cert)

      false ->
        throw({:bad_crl, :invalid_issuer})
    end
  end

  defp dp_crlissuer_to_issuer(dPCRLIssuer) do
    [{:directoryName, issuer}] =
      :pubkey_cert_records.transform(
        dPCRLIssuer,
        :decode
      )

    issuer
  end

  defp is_indirect_crl(r_IssuingDistributionPoint(indirectCRL: value)) do
    value
  end

  defp is_indirect_crl(_) do
    false
  end

  defp verify_scope(_, _, :undefined) do
    :ok
  end

  defp verify_scope(
         r_OTPCertificate(tbsCertificate: tBSCert),
         r_DistributionPoint(cRLIssuer: dPIssuer) = dP,
         iDP
       ) do
    certIssuer = r_OTPTBSCertificate(tBSCert, :issuer)

    names =
      case gen_names(dPIssuer) do
        [{:directoryName, tNames}] ->
          tNames

        other ->
          other
      end

    dPName = dp_names(r_DistributionPoint(dP, :distributionPoint), names, certIssuer)
    iDPName = dp_names(r_IssuingDistributionPoint(iDP, :distributionPoint), names, certIssuer)
    verify_scope(dPName, iDPName, names, tBSCert, iDP)
  end

  defp verify_scope(:asn1_NOVALUE, _, :asn1_NOVALUE, _, _) do
    throw({:bad_crl, :scope_error})
  end

  defp verify_scope(:asn1_NOVALUE, iDPName, dPIssuerNames, tBSCert, iDP) do
    verify_dp_name(iDPName, dPIssuerNames)
    verify_dp_bools(tBSCert, iDP)
  end

  defp verify_scope(dPName, iDPName, _, tBSCert, iDP) do
    verify_dp_name(iDPName, dPName)
    verify_dp_bools(tBSCert, iDP)
  end

  defp dp_names(:asn1_NOVALUE, _, _) do
    :asn1_NOVALUE
  end

  defp dp_names({:fullName, name}, _, _) do
    gen_names(name)
  end

  defp dp_names(
         {:nameRelativeToCRLIssuer, fragment},
         :asn1_NOVALUE,
         {:rdnSequence, relativeDestinguistNames}
       ) do
    [
      {:directoryName,
       {:rdnSequence,
        relativeDestinguistNames ++
          [
            :lists.map(
              fn attrAndValue ->
                :pubkey_cert_records.transform(
                  attrAndValue,
                  :decode
                )
              end,
              fragment
            )
          ]}}
    ]
  end

  defp dp_names({:nameRelativeToCRLIssuer, fragment}, {:rdnSequence, relativeDestinguistNames}, _) do
    [
      {:directoryName,
       {:rdnSequence,
        relativeDestinguistNames ++
          [
            :lists.map(
              fn attrAndValue ->
                :pubkey_cert_records.transform(
                  attrAndValue,
                  :decode
                )
              end,
              fragment
            )
          ]}}
    ]
  end

  defp dp_names([{:rdnSequence, _}] = name0, _, _) do
    [name] = :pubkey_cert_records.transform(name0, :decode)
    [{:directoryName, name}]
  end

  defp gen_names(:asn1_NOVALUE) do
    :asn1_NOVALUE
  end

  defp gen_names([]) do
    []
  end

  defp gen_names([{nameType, name} | rest]) do
    [
      {nameType, :pubkey_cert_records.transform(name, :decode)}
      | gen_names(rest)
    ]
  end

  defp verify_dp_name(:asn1_NOVALUE, _) do
    :ok
  end

  defp verify_dp_name(iDPNames, dPorIssuerNames) do
    case match_one(dPorIssuerNames, iDPNames) do
      true ->
        :ok

      false ->
        throw({:bad_crl, :scope_error})
    end
  end

  def match_one([], _) do
    false
  end

  def match_one([{type, name} | names], candidateNames) do
    candidates =
      for {nameType, nameName} <- candidateNames,
          nameType == type do
        nameName
      end

    case candidates do
      [] ->
        false

      [_ | _] ->
        case :pubkey_cert.match_name(type, name, candidates) do
          true ->
            true

          false ->
            match_one(names, candidateNames)
        end
    end
  end

  defp verify_onlyContainsUserCerts(
         r_Extension(extnValue: r_BasicConstraints(cA: true)),
         r_IssuingDistributionPoint(onlyContainsUserCerts: true)
       ) do
    false
  end

  defp verify_onlyContainsUserCerts(_, _) do
    true
  end

  defp verify_onlyContainsCACerts(
         r_Extension(extnValue: r_BasicConstraints(cA: true)),
         r_IssuingDistributionPoint(onlyContainsCACerts: true)
       ) do
    true
  end

  defp verify_onlyContainsCACerts(_, r_IssuingDistributionPoint(onlyContainsCACerts: true)) do
    false
  end

  defp verify_onlyContainsCACerts(_, _) do
    true
  end

  defp verify_onlyContainsAttributeCerts(
         r_IssuingDistributionPoint(onlyContainsAttributeCerts: bool)
       ) do
    not bool
  end

  defp extension_value(extension, extType, extensions) do
    case :pubkey_cert.select_extension(
           extension,
           extensions
         ) do
      r_Extension(extnValue: value) ->
        :public_key.der_decode(
          extType,
          :erlang.iolist_to_binary(value)
        )

      _ ->
        :undefined
    end
  end

  defp assert_extension_value(extension, extType, extensions) do
    case extension_value(extension, extType, extensions) do
      :undefined ->
        throw(:no_extension_present)

      value ->
        value
    end
  end

  defp check_delta_issuer_and_scope(_, :undefined) do
    true
  end

  defp check_delta_issuer_and_scope(
         r_CertificateList(tbsCertList: tBSCRL),
         r_CertificateList(tbsCertList: tBSDeltaCRL)
       ) do
    case :pubkey_cert.is_issuer(
           r_TBSCertList(tBSCRL, :issuer),
           r_TBSCertList(tBSDeltaCRL, :issuer)
         ) do
      true ->
        check_delta_scope(tBSCRL, tBSDeltaCRL)

      false ->
        false
    end
  end

  defp check_delta_scope(
         r_TBSCertList(crlExtensions: extensions),
         r_TBSCertList(crlExtensions: deltaExtensions)
       ) do
    iDP = issuing_distribution_point(extensions)
    deltaIDP = issuing_distribution_point(deltaExtensions)
    authKey = authority_key_identifier(extensions)
    deltaAuthKey = authority_key_identifier(deltaExtensions)

    is_match(iDP, deltaIDP) and
      is_match(
        authKey,
        deltaAuthKey
      )
  end

  defp is_match(x, x) do
    true
  end

  defp is_match(_, _) do
    false
  end

  defp compute_interim_reasons_mask(
         r_DistributionPoint(reasons: :asn1_NOVALUE),
         r_IssuingDistributionPoint(onlySomeReasons: :asn1_NOVALUE)
       ) do
    all_reasons()
  end

  defp compute_interim_reasons_mask(r_DistributionPoint(reasons: :asn1_NOVALUE), :undefined) do
    all_reasons()
  end

  defp compute_interim_reasons_mask(
         r_DistributionPoint(reasons: :asn1_NOVALUE),
         r_IssuingDistributionPoint(onlySomeReasons: iDPReasons)
       ) do
    :sets.from_list(iDPReasons)
  end

  defp compute_interim_reasons_mask(
         r_DistributionPoint(reasons: dPReasons),
         r_IssuingDistributionPoint(onlySomeReasons: :asn1_NOVALUE)
       ) do
    :sets.from_list(dPReasons)
  end

  defp compute_interim_reasons_mask(r_DistributionPoint(reasons: dPReasons), :undefined) do
    :sets.from_list(dPReasons)
  end

  defp compute_interim_reasons_mask(
         r_DistributionPoint(reasons: dPReasons),
         r_IssuingDistributionPoint(onlySomeReasons: iDPReasons)
       ) do
    :sets.intersection(
      :sets.from_list(dPReasons),
      :sets.from_list(iDPReasons)
    )
  end

  defp verify_interim_reasons_mask(
         r_revoke_state(
           reasons_mask: mask,
           interim_reasons_mask: intMask
         )
       ) do
    case :sets.fold(
           fn element, acc ->
             case :sets.is_element(element, mask) do
               true ->
                 acc

               false ->
                 true
             end
           end,
           false,
           intMask
         ) do
      true ->
        :ok

      false ->
        throw({:bad_crl, :mask_error})
    end
  end

  def verify_crl_signature(:undefined, :undefined, _, _) do
    true
  end

  def verify_crl_signature(cRL, derCRL, key, keyParams) do
    {digestType, plainText, signature} = extract_crl_verify_data(cRL, derCRL)

    case key do
      r_RSAPublicKey() ->
        :public_key.verify(plainText, digestType, signature, key)

      _ ->
        :public_key.verify(plainText, digestType, signature, {key, keyParams})
    end
  end

  defp extract_crl_verify_data(cRL, derCRL) do
    signature = r_CertificateList(cRL, :signature)
    r_AlgorithmIdentifier(algorithm: sigAlg) = r_CertificateList(cRL, :signatureAlgorithm)
    plainText = encoded_tbs_crl(derCRL)
    {digestType, _} = :public_key.pkix_sign_types(sigAlg)
    {digestType, plainText, signature}
  end

  defp encoded_tbs_crl(cRL) do
    {:ok, pKIXCRL} = :OTP - PUB - KEY.decode_TBSCertList_exclusive(cRL)
    {:CertificateList, {:CertificateList_tbsCertList, encodedTBSCertList}, _, _} = pKIXCRL
    encodedTBSCertList
  end

  defp handle_indirect_crl_check(
         dP,
         iDP,
         defaultIssuer0,
         names,
         serialNr,
         extensions,
         reason,
         rest,
         state
       ) do
    case check_crl_issuer_extension(names, extensions, defaultIssuer0) do
      {true, _} ->
        r_revoke_state(state, cert_status: reason)

      {false, defaultIssuer} ->
        check_revoked(dP, iDP, defaultIssuer, names, serialNr, rest, state)
    end
  end

  defp issuer_id(cert, r_CertificateList(tbsCertList: tBSCRL)) do
    extensions = :pubkey_cert.extensions_list(r_TBSCertList(tBSCRL, :crlExtensions))

    case authority_key_identifier(extensions) do
      :undefined ->
        issuer_id(cert)

      r_AuthorityKeyIdentifier(
        authorityCertIssuer: :asn1_NOVALUE,
        authorityCertSerialNumber: :asn1_NOVALUE
      ) ->
        issuer_id(cert)

      r_AuthorityKeyIdentifier(
        authorityCertIssuer: issuer,
        authorityCertSerialNumber: nr
      ) ->
        {:ok, {nr, issuer}}
    end
  end

  defp issuer_id(r_OTPCertificate() = cert) do
    case :public_key.pkix_is_self_signed(cert) do
      true ->
        :public_key.pkix_issuer_id(cert, :self)

      false ->
        :public_key.pkix_issuer_id(cert, :other)
    end
  end

  defp status(:unrevoked) do
    :unrevoked
  end

  defp status(reason) do
    {:revoked, reason}
  end
end
