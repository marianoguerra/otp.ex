defmodule :m_pubkey_cert do
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

  def verify_data(derCert) do
    {:ok, otpCert} = :pubkey_cert_records.decode_cert(derCert)
    extract_verify_data(otpCert, derCert)
  end

  def prepare_for_next_cert(
        otpCert,
        validationState =
          r_path_validation_state(
            working_public_key_algorithm: prevAlgo,
            working_public_key_parameters: prevParams
          )
      ) do
    tBSCert = r_OTPCertificate(otpCert, :tbsCertificate)
    issuer = r_OTPTBSCertificate(tBSCert, :subject)

    {algorithm, publicKey, publicKeyParams0} =
      public_key_info(
        r_OTPTBSCertificate(tBSCert, :subjectPublicKeyInfo),
        validationState
      )

    publicKeyParams =
      case publicKeyParams0 do
        :NULL when algorithm === prevAlgo ->
          prevParams

        :asn1_NOVALUE when algorithm === prevAlgo ->
          prevParams

        _ ->
          publicKeyParams0
      end

    r_path_validation_state(validationState,
      working_public_key_algorithm: algorithm,
      working_public_key: publicKey,
      working_public_key_parameters: publicKeyParams,
      working_issuer_name: issuer,
      cert_num: r_path_validation_state(validationState, :cert_num) + 1
    )
  end

  def validate_time(otpCert, userState, verifyFun) do
    tBSCert = r_OTPCertificate(otpCert, :tbsCertificate)
    {:Validity, notBeforeStr, notAfterStr} = r_OTPTBSCertificate(tBSCert, :validity)
    now = :calendar.datetime_to_gregorian_seconds(:calendar.universal_time())
    notBefore = time_str_2_gregorian_sec(notBeforeStr)
    notAfter = time_str_2_gregorian_sec(notAfterStr)

    case :erlang.and(notBefore <= now, now <= notAfter) do
      true ->
        userState

      false ->
        verify_fun(otpCert, {:bad_cert, :cert_expired}, userState, verifyFun)
    end
  end

  def validate_issuer(otpCert, issuer, userState, verifyFun) do
    tBSCert = r_OTPCertificate(otpCert, :tbsCertificate)

    case is_issuer(issuer, r_OTPTBSCertificate(tBSCert, :issuer)) do
      true ->
        userState

      _ ->
        verify_fun(otpCert, {:bad_cert, :invalid_issuer}, userState, verifyFun)
    end
  end

  def validate_signature(otpCert, derCert, key, keyParams, userState, verifyFun) do
    case verify_signature(otpCert, derCert, key, keyParams) do
      true ->
        userState

      false ->
        verify_fun(otpCert, {:bad_cert, :invalid_signature}, userState, verifyFun)
    end
  end

  def validate_extensions(otpCert, validationState, userState, verifyFun) do
    tBSCert = r_OTPCertificate(otpCert, :tbsCertificate)

    case r_OTPTBSCertificate(tBSCert, :version) do
      n when n >= 3 ->
        extensions = r_OTPTBSCertificate(tBSCert, :extensions)

        validate_extensions(
          otpCert,
          extensions,
          validationState,
          :no_basic_constraint,
          is_self_signed(otpCert),
          userState,
          verifyFun
        )

      _ ->
        {validationState, userState}
    end
  end

  def normalize_general_name({:rdnSequence, issuer}) do
    normIssuer = do_normalize_general_name(issuer)
    {:rdnSequence, normIssuer}
  end

  def is_self_signed(
        r_OTPCertificate(
          tbsCertificate:
            r_OTPTBSCertificate(
              issuer: issuer,
              subject: subject
            )
        )
      ) do
    is_issuer(issuer, subject)
  end

  def is_issuer(
        {:rdnSequence, _} = issuer,
        {:rdnSequence, _} = candidate
      ) do
    {:rdnSequence, issuerDirName} = normalize_general_name(issuer)
    {:rdnSequence, candidateDirName} = normalize_general_name(candidate)
    is_dir_name(issuerDirName, candidateDirName, true)
  end

  def subject_id(otpcert) do
    tBSCert = r_OTPCertificate(otpcert, :tbsCertificate)
    subject = r_OTPTBSCertificate(tBSCert, :subject)
    serialNr = r_OTPTBSCertificate(tBSCert, :serialNumber)
    {serialNr, normalize_general_name(subject)}
  end

  def is_fixed_dh_cert(
        r_OTPCertificate(
          tbsCertificate:
            r_OTPTBSCertificate(
              subjectPublicKeyInfo: subjectPublicKeyInfo,
              extensions: extensions
            )
        )
      ) do
    is_fixed_dh_cert(
      subjectPublicKeyInfo,
      extensions_list(extensions)
    )
  end

  def verify_fun(otpcert, result, userState0, verifyFun) do
    case verifyFun.(otpcert, result, userState0) do
      {:valid, userState} ->
        userState

      {:valid_peer, userState} ->
        userState

      {:fail, reason} ->
        case reason do
          {:bad_cert, _} ->
            throw(reason)

          _ ->
            throw({:bad_cert, reason})
        end

      {:unknown, userState} ->
        case result do
          {:extension, r_Extension(critical: true)} ->
            throw({:bad_cert, :unknown_critical_extension})

          _ ->
            userState
        end
    end
  end

  def select_extension(_, :asn1_NOVALUE) do
    :undefined
  end

  def select_extension(_, []) do
    :undefined
  end

  def select_extension(id, [r_Extension(extnID: id) = extension | _]) do
    extension
  end

  def select_extension(id, [_ | extensions]) do
    select_extension(id, extensions)
  end

  def match_name(:rfc822Name, name, [permittedName | rest]) do
    match_name(&is_valid_host_or_domain/2, name, permittedName, rest)
  end

  def match_name(:directoryName, dirName, [permittedName | rest]) do
    match_name(&is_rdnSeq/2, dirName, permittedName, rest)
  end

  def match_name(:uniformResourceIdentifier, uRI, [permittedName | rest]) do
    case :uri_string.normalize(uRI, [:return_map]) do
      %{:host => host} ->
        pN =
          case :uri_string.normalize(
                 permittedName,
                 [:return_map]
               ) do
            %{:host => pNhost} ->
              pNhost

            _X ->
              permittedName
          end

        match_name(&is_valid_host_or_domain/2, host, pN, rest)

      _ ->
        false
    end
  end

  def match_name(:emailAddress, name, [permittedName | rest]) do
    fun = fn email, permittedEmail ->
      is_valid_email_address(email, permittedEmail, :string.tokens(permittedEmail, '@'))
    end

    match_name(fun, name, permittedName, rest)
  end

  def match_name(:dNSName, name, [permittedName | rest]) do
    fun = fn
      domain, [?. | domain] ->
        true

      name1, name2 ->
        :lists.suffix(
          :string.to_lower(name2),
          :string.to_lower(name1)
        )
    end

    match_name(fun, name, [?. | permittedName], rest)
  end

  def match_name(:x400Address, orAddress, [permittedAddr | rest]) do
    match_name(&is_or_address/2, orAddress, permittedAddr, rest)
  end

  def match_name(:ipAdress, iP, [permittedIP | rest]) do
    fun = fn
      [iP1, iP2, iP3, iP4], [iP5, iP6, iP7, iP8, m1, m2, m3, m4] ->
        is_permitted_ip([iP1, iP2, iP3, iP4], [iP5, iP6, iP7, iP8], [m1, m2, m3, m4])

      [iP1, iP2, iP3, iP4, iP5, iP6, iP7, iP8, iP9, iP10, iP11, iP12, iP13, iP14, iP15, iP16],
      [
        iP17,
        iP18,
        iP19,
        iP20,
        iP21,
        iP22,
        iP23,
        iP24,
        iP25,
        iP26,
        iP27,
        iP28,
        iP29,
        iP30,
        iP31,
        iP32,
        m1,
        m2,
        m3,
        m4,
        m5,
        m6,
        m7,
        m8,
        m9,
        m10,
        m11,
        m12,
        m13,
        m14,
        m15,
        m16
      ] ->
        is_permitted_ip(
          [iP1, iP2, iP3, iP4, iP5, iP6, iP7, iP8, iP9, iP10, iP11, iP12, iP13, iP14, iP15, iP16],
          [
            iP17,
            iP18,
            iP19,
            iP20,
            iP21,
            iP22,
            iP23,
            iP24,
            iP25,
            iP26,
            iP27,
            iP28,
            iP29,
            iP30,
            iP31,
            iP32
          ],
          [m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16]
        )

      _, _ ->
        false
    end

    match_name(fun, iP, permittedIP, rest)
  end

  defp match_name(fun, name, permittedName, []) do
    fun.(name, permittedName)
  end

  defp match_name(fun, name, permittedName, [head | tail]) do
    case fun.(name, permittedName) do
      true ->
        true

      false ->
        match_name(fun, name, head, tail)
    end
  end

  def gen_test_certs(%{
        :client_chain => %{:root => clientRoot, :intermediates => clientCAs, :peer => clientPeer},
        :server_chain => %{:root => serverRoot, :intermediates => serverCAs, :peer => serverPeer}
      }) do
    %{:cert => serverRootCert, :key => serverRootKey} =
      case serverRoot do
        %{} ->
          serverRoot

        serverRootConf when is_list(serverRootConf) ->
          root_cert('SERVER ROOT CA', serverRootConf)
      end

    %{:cert => clientRootCert, :key => clientRootKey} =
      case clientRoot do
        %{} ->
          clientRoot

        clientRootConf when is_list(clientRootConf) ->
          root_cert('CLIENT ROOT CA', clientRootConf)
      end

    [
      {serverDERCert, serverDERKey}
      | serverCAsKeys
    ] =
      config(
        :server,
        serverRootCert,
        serverRootKey,
        :lists.reverse([
          serverPeer
          | :lists.reverse(serverCAs)
        ])
      )

    [
      {clientDERCert, clientDERKey}
      | clientCAsKeys
    ] =
      config(
        :client,
        clientRootCert,
        clientRootKey,
        :lists.reverse([
          clientPeer
          | :lists.reverse(clientCAs)
        ])
      )

    serverDERCA = ca_config(clientRootCert, serverCAsKeys)
    clientDERCA = ca_config(serverRootCert, clientCAsKeys)

    %{
      :server_config => [{:cert, serverDERCert}, {:key, serverDERKey}, {:cacerts, serverDERCA}],
      :client_config => [{:cert, clientDERCert}, {:key, clientDERKey}, {:cacerts, clientDERCA}]
    }
  end

  def gen_test_certs(%{:root => root, :intermediates => cAs, :peer => peer}) do
    %{:cert => rootCert, :key => rootKey} =
      case root do
        %{} ->
          root

        rootConf when is_list(rootConf) ->
          root_cert('SERVER ROOT CA', rootConf)
      end

    [{dERCert, dERKey} | cAsKeys] =
      config(
        :server,
        rootCert,
        rootKey,
        :lists.reverse([
          peer
          | :lists.reverse(cAs)
        ])
      )

    dERCAs = ca_config(rootCert, cAsKeys)
    [{:cert, dERCert}, {:key, dERKey}, {:cacerts, dERCAs}]
  end

  def root_cert(name, opts) do
    privKey = gen_key(:proplists.get_value(:key, opts, default_key_gen()))
    tBS = cert_template()
    issuer = subject('root', name)
    signatureId = sign_algorithm(privKey, opts)
    sPI = public_key(privKey, signatureId)

    oTPTBS =
      r_OTPTBSCertificate(tBS,
        signature: signatureId,
        issuer: issuer,
        validity: validity(opts),
        subject: issuer,
        subjectPublicKeyInfo: sPI,
        extensions: extensions(:undefined, :ca, opts)
      )

    %{:cert => :public_key.pkix_sign(oTPTBS, privKey), :key => privKey}
  end

  defp do_normalize_general_name(issuer) do
    normalize = fn
      [{description, type, {:printableString, value}}] ->
        newValue = :string.to_lower(strip_spaces(value))
        [{description, type, {:printableString, newValue}}]

      atter ->
        atter
    end

    :lists.map(normalize, issuer)
  end

  defp extract_email({:rdnSequence, list}) do
    extract_email2(list)
  end

  def extensions_list(:asn1_NOVALUE) do
    []
  end

  def extensions_list(extensions) do
    extensions
  end

  defp extract_verify_data(otpCert, derCert) do
    signature = r_OTPCertificate(otpCert, :signature)
    sigAlg = r_OTPCertificate(otpCert, :signatureAlgorithm)
    plainText = encoded_tbs_cert(derCert)
    {digestType, _, _} = x509_pkix_sign_types(sigAlg)
    {digestType, plainText, signature}
  end

  defp verify_signature(otpCert, derCert, key, keyParams) do
    {digestType, plainText, signature} = extract_verify_data(otpCert, derCert)

    case key do
      r_RSAPublicKey() ->
        case keyParams do
          r_RSASSA_PSS_params() ->
            :public_key.verify(plainText, digestType, signature, key, verify_options(keyParams))

          :NULL ->
            :public_key.verify(plainText, digestType, signature, key)
        end

      _ ->
        :public_key.verify(plainText, digestType, signature, {key, keyParams})
    end
  end

  defp encoded_tbs_cert(cert) do
    {:ok, pKIXCert} = :OTP - PUB - KEY.decode_TBSCert_exclusive(cert)
    {:Certificate, {:Certificate_tbsCertificate, encodedTBSCert}, _, _} = pKIXCert
    encodedTBSCert
  end

  defp public_key_info(
         publicKeyInfo,
         r_path_validation_state(
           working_public_key_algorithm: workingAlgorithm,
           working_public_key_parameters: workingParams
         )
       ) do
    publicKey = r_OTPSubjectPublicKeyInfo(publicKeyInfo, :subjectPublicKey)
    algInfo = r_OTPSubjectPublicKeyInfo(publicKeyInfo, :algorithm)
    publicKeyParams = r_PublicKeyAlgorithm(algInfo, :parameters)
    algorithm = r_PublicKeyAlgorithm(algInfo, :algorithm)

    newPublicKeyParams =
      case publicKeyParams do
        {:null, :NULL} when workingAlgorithm == algorithm ->
          workingParams

        {:params, params} ->
          params

        params ->
          params
      end

    {algorithm, publicKey, newPublicKeyParams}
  end

  def time_str_2_gregorian_sec({:utcTime, [y1, y2, m1, m2, d1, d2, h1, h2, m3, m4, s1, s2, z]}) do
    case :erlang.list_to_integer([y1, y2]) do
      n when n >= 50 ->
        time_str_2_gregorian_sec(
          {:generalTime, [?1, ?9, y1, y2, m1, m2, d1, d2, h1, h2, m3, m4, s1, s2, z]}
        )

      _ ->
        time_str_2_gregorian_sec(
          {:generalTime, [?2, ?0, y1, y2, m1, m2, d1, d2, h1, h2, m3, m4, s1, s2, z]}
        )
    end
  end

  def time_str_2_gregorian_sec({_, [y1, y2, y3, y4, m1, m2, d1, d2, h1, h2, m3, m4, s1, s2, ?Z]}) do
    year = :erlang.list_to_integer([y1, y2, y3, y4])
    month = :erlang.list_to_integer([m1, m2])
    day = :erlang.list_to_integer([d1, d2])
    hour = :erlang.list_to_integer([h1, h2])
    min = :erlang.list_to_integer([m3, m4])
    sec = :erlang.list_to_integer([s1, s2])
    :calendar.datetime_to_gregorian_seconds({{year, month, day}, {hour, min, sec}})
  end

  defp is_dir_name([], [], _Exact) do
    true
  end

  defp is_dir_name([h | r1], [h | r2], exact) do
    is_dir_name(r1, r2, exact)
  end

  defp is_dir_name(
         [
           [{:AttributeTypeAndValue, type, what1}]
           | rest1
         ],
         [[{:AttributeTypeAndValue, type, what2}] | rest2],
         exact
       ) do
    case is_dir_name2(what1, what2) do
      true ->
        is_dir_name(rest1, rest2, exact)

      false ->
        false
    end
  end

  defp is_dir_name(_, [], false) do
    true
  end

  defp is_dir_name(_, _, _) do
    false
  end

  defp is_dir_name2(value, value) do
    true
  end

  defp is_dir_name2(
         {:printableString, value1},
         {:printableString, value2}
       ) do
    :string.to_lower(strip_spaces(value1)) === :string.to_lower(strip_spaces(value2))
  end

  defp is_dir_name2({:utf8String, value1}, string) do
    is_dir_name2(
      {:printableString, :unicode.characters_to_list(value1)},
      string
    )
  end

  defp is_dir_name2(string, {:utf8String, value1}) do
    is_dir_name2(
      string,
      {:printableString, :unicode.characters_to_list(value1)}
    )
  end

  defp is_dir_name2(_, _) do
    false
  end

  def cert_auth_key_id(r_AuthorityKeyIdentifier(authorityCertIssuer: :asn1_NOVALUE)) do
    {:error, :issuer_not_found}
  end

  def cert_auth_key_id(
        r_AuthorityKeyIdentifier(
          authorityCertIssuer: authCertIssuer,
          authorityCertSerialNumber: serialNr
        )
      ) do
    {:ok, {serialNr, decode_general_name(authCertIssuer)}}
  end

  defp decode_general_name([{:directoryName, issuer}]) do
    normalize_general_name(issuer)
  end

  defp decode_general_name([{_, issuer}]) do
    issuer
  end

  defp strip_spaces(string) do
    newString =
      :lists.foldl(
        fn char, acc ->
          acc ++ char ++ ' '
        end,
        [],
        :string.tokens(string, ' ')
      )

    :string.strip(newString)
  end

  defp is_valid_key_usage(keyUse, use) do
    :lists.member(use, keyUse)
  end

  defp validate_subject_alt_names([]) do
    false
  end

  defp validate_subject_alt_names([altName | rest]) do
    case is_valid_subject_alt_name(altName) do
      true ->
        true

      false ->
        validate_subject_alt_names(rest)
    end
  end

  defp is_valid_subject_alt_name({name, value})
       when name == :rfc822Name or
              name == :dNSName do
    case value do
      '' ->
        false

      _ ->
        true
    end
  end

  defp is_valid_subject_alt_name({:iPAdress, addr}) do
    case length(addr) do
      4 ->
        true

      16 ->
        true

      _ ->
        false
    end
  end

  defp is_valid_subject_alt_name({:uniformResourceIdentifier, uRI}) do
    is_valid_uri(uRI)
  end

  defp is_valid_subject_alt_name({:directoryName, _}) do
    true
  end

  defp is_valid_subject_alt_name({_, [_ | _]}) do
    true
  end

  defp is_valid_subject_alt_name({:otherName, r_AnotherName()}) do
    false
  end

  defp is_valid_subject_alt_name({_, _}) do
    false
  end

  defp is_valid_uri(absURI) do
    case :uri_string.normalize(absURI, [:return_map]) do
      %{:scheme => _} ->
        true

      _ ->
        false
    end
  end

  defp is_rdnSeq({:rdnSequence, []}, {:rdnSequence, [:none]}) do
    true
  end

  defp is_rdnSeq(
         {:rdnSequence, dirName},
         {:rdnSequence, permitted}
       ) do
    is_dir_name(dirName, permitted, false)
  end

  defp is_permitted(_, :no_constraints) do
    true
  end

  defp is_permitted(names, constraints) do
    is_valid_name(names, constraints, true)
  end

  defp is_excluded([], _) do
    false
  end

  defp is_excluded(names, constraints) do
    is_valid_name(names, constraints, false)
  end

  defp is_valid_name([], _, default) do
    default
  end

  defp is_valid_name([{type, name} | rest], constraints, default) do
    case type_subtree_names(type, constraints) do
      [_ | _] = constraintNames ->
        case match_name(type, name, constraintNames) do
          ^default ->
            is_valid_name(rest, constraints, default)

          fail ->
            fail
        end

      [] ->
        is_valid_name(rest, constraints, default)
    end
  end

  defp add_name_constraints(
         newPermittedTrees,
         newExcludedTrees,
         r_path_validation_state(
           permitted_subtrees: permittedTrees,
           excluded_subtrees: excludedTrees
         ) = validationState
       ) do
    newPermitted =
      subtree_intersection(
        newPermittedTrees,
        permittedTrees
      )

    newExcluded =
      subtree_union(
        newExcludedTrees,
        excludedTrees
      )

    r_path_validation_state(validationState,
      permitted_subtrees: newPermitted,
      excluded_subtrees: newExcluded
    )
  end

  defp subtree_union(:asn1_NOVALUE, trees) do
    trees
  end

  defp subtree_union(trees1, trees2) do
    trees1 ++ trees2
  end

  defp subtree_intersection(:asn1_NOVALUE, trees) do
    trees
  end

  defp subtree_intersection(list, :no_constraints) do
    list
  end

  defp subtree_intersection([tree | trees1], trees2) do
    trees = is_in_intersection(tree, trees2)
    subtree_intersection(trees1, trees)
  end

  defp subtree_intersection([], treesInt) do
    treesInt
  end

  defp is_in_intersection(
         r_GeneralSubtree(base: {:directoryName, {:rdnSequence, name1}}) = name,
         [
           r_GeneralSubtree(base: {:directoryName, {:rdnSequence, name2}})
           | trees
         ]
       ) do
    case is_dir_name(name1, name2, false) do
      true ->
        [name | trees]

      false ->
        [
          r_GeneralSubtree(name, base: {:directoryName, {:rdnSequence, [:none]}})
          | trees
        ]
    end
  end

  defp is_in_intersection(
         r_GeneralSubtree(base: {:ipAdress, ip}),
         trees = [r_GeneralSubtree(base: {:ipAdress, ip}) | _]
       ) do
    trees
  end

  defp is_in_intersection(
         r_GeneralSubtree(base: {:x400Address, orAddr1}) = addr,
         [r_GeneralSubtree(base: {:x400Address, orAddr2}) | trees]
       ) do
    case is_or_address(orAddr1, orAddr2) do
      true ->
        [addr | trees]

      false ->
        [r_GeneralSubtree(base: {:x400Address, ''}) | trees]
    end
  end

  defp is_in_intersection(
         r_GeneralSubtree(base: {type, name1}) = name,
         [r_GeneralSubtree(base: {type, name2}) | trees]
       ) do
    case case_insensitive_match(name1, name2) do
      true ->
        [name | trees]

      false ->
        [r_GeneralSubtree(base: {type, ''}) | trees]
    end
  end

  defp is_in_intersection(new, []) do
    [new]
  end

  defp is_in_intersection(name, [other | intCandidates]) do
    [other | is_in_intersection(name, intCandidates)]
  end

  defp type_subtree_names(type, subTrees) do
    for r_GeneralSubtree(base: {treeType, name}) <- subTrees,
        treeType === type do
      name
    end
  end

  defp is_permitted_ip([], [], []) do
    true
  end

  defp is_permitted_ip([candidatIp | candidatIpRest], [permittedIp | permittedIpRest], [
         mask | maskRest
       ]) do
    case mask_cmp(candidatIp, permittedIp, mask) do
      true ->
        is_permitted_ip(candidatIpRest, permittedIpRest, maskRest)

      false ->
        false
    end
  end

  defp mask_cmp(canditate, permitted, mask) do
    canditate &&& mask == permitted
  end

  defp is_valid_host_or_domain(canditate, [?. | _] = permitted) do
    is_suffix(permitted, canditate)
  end

  defp is_valid_host_or_domain(canditate, permitted) do
    case :string.tokens(canditate, '@') do
      [canditateHost] ->
        case_insensitive_match(canditateHost, permitted)

      [_, canditateHost] ->
        case_insensitive_match(canditateHost, permitted)
    end
  end

  defp is_valid_email_address(canditate, [?. | permitted], [_]) do
    is_suffix(permitted, canditate)
  end

  defp is_valid_email_address(canditate, permittedHost, [_]) do
    [_, canditateHost] = :string.tokens(canditate, '@')
    case_insensitive_match(canditateHost, permittedHost)
  end

  defp is_valid_email_address(canditate, permitted, [_, _]) do
    case_insensitive_match(canditate, permitted)
  end

  defp is_suffix(suffix, str) do
    :lists.suffix(
      :string.to_lower(suffix),
      :string.to_lower(str)
    )
  end

  defp case_insensitive_match(str1, str2) do
    :string.to_lower(str1) == :string.to_lower(str2)
  end

  defp is_or_address(address, canditate) do
    is_double_quoted(address) and is_double_quoted(canditate) and
      case_insensitive_match(
        address,
        canditate
      )
  end

  defp is_double_quoted(['"' | tail]) do
    is_double_quote(:lists.last(tail))
  end

  defp is_double_quoted('%22' ++ tail) do
    case :lists.reverse(tail) do
      [[a, b, c] | _] ->
        is_double_quote([c, b, a])

      _ ->
        false
    end
  end

  defp is_double_quoted(_) do
    false
  end

  defp is_double_quote('%22') do
    true
  end

  defp is_double_quote('"') do
    true
  end

  defp is_double_quote(_) do
    false
  end

  defp add_policy_constraints(
         expPolicy,
         mapPolicy,
         r_path_validation_state(
           cert_num: certNum,
           explicit_policy: curExpPolicy,
           policy_mapping: curMapPolicy
         ) = validationState
       ) do
    newExpPolicy = policy_constraint(curExpPolicy, expPolicy, certNum)
    newMapPolicy = policy_constraint(curMapPolicy, mapPolicy, certNum)

    r_path_validation_state(validationState,
      explicit_policy: newExpPolicy,
      policy_mapping: newMapPolicy
    )
  end

  defp policy_constraint(current, :asn1_NOVALUE, _) do
    current
  end

  defp policy_constraint(current, new, certNum) do
    :erlang.min(current, new + certNum)
  end

  defp process_policy_tree(_, _, 0) do
    0
  end

  defp process_policy_tree(_Id, _Qualifier, tree) do
    tree
  end

  defp policy_indicator(_, true) do
    0
  end

  defp policy_indicator(n, false) do
    n + 1
  end

  defp missing_basic_constraints(otpCert, selfSigned, validationState, verifyFun, userState0, len) do
    userState = verify_fun(otpCert, {:bad_cert, :missing_basic_constraint}, userState0, verifyFun)

    case selfSigned do
      true ->
        {validationState, userState}

      false ->
        {r_path_validation_state(validationState, max_path_length: len - 1), userState}
    end
  end

  defp gen_key(keyGen) do
    case is_key(keyGen) do
      true ->
        keyGen

      false ->
        :public_key.generate_key(keyGen)
    end
  end

  defp is_key(r_DSAPrivateKey()) do
    true
  end

  defp is_key(r_RSAPrivateKey()) do
    true
  end

  defp is_key({r_RSAPrivateKey(), _}) do
    true
  end

  defp is_key(r_ECPrivateKey()) do
    true
  end

  defp is_key(_) do
    false
  end

  defp cert_template() do
    r_OTPTBSCertificate(
      version: :v3,
      serialNumber: :erlang.unique_integer([:positive, :monotonic]),
      issuerUniqueID: :asn1_NOVALUE,
      subjectUniqueID: :asn1_NOVALUE
    )
  end

  defp subject(contact, name) do
    opts = [
      {:email, contact ++ '@example.org'},
      {:name, name},
      {:city, 'Stockholm'},
      {:country, 'SE'},
      {:org, 'erlang'},
      {:org_unit, 'automated testing'}
    ]

    subject(opts)
  end

  defp subject(subjectOpts) when is_list(subjectOpts) do
    encode = fn opt ->
      {type, value} = subject_enc(opt)
      [r_AttributeTypeAndValue(type: type, value: value)]
    end

    {:rdnSequence,
     for opt <- subjectOpts do
       encode.(opt)
     end}
  end

  defp validity(opts) do
    defFrom0 =
      :calendar.gregorian_days_to_date(:calendar.date_to_gregorian_days(:erlang.date()) - 1)

    defTo0 =
      :calendar.gregorian_days_to_date(:calendar.date_to_gregorian_days(:erlang.date()) + 7)

    {defFrom, defTo} = :proplists.get_value(:validity, opts, {defFrom0, defTo0})

    format = fn {y, m, d} ->
      :lists.flatten(:io_lib.format('~4..0w~2..0w~2..0w130000Z', [y, m, d]))
    end

    r_Validity(
      notBefore: {:generalTime, format.(defFrom)},
      notAfter: {:generalTime, format.(defTo)}
    )
  end

  defp config(role, root, key, opts) do
    cert_chain(role, root, key, opts)
  end

  defp cert_chain(role, root, rootKey, opts) do
    cert_chain(role, root, rootKey, opts, 0, [])
  end

  defp cert_chain(role, issuerCert, issuerKey, [peerOpts], _, acc) do
    key = gen_key(:proplists.get_value(:key, peerOpts, default_key_gen()))

    cert =
      cert(
        role,
        :public_key.pkix_decode_cert(issuerCert, :otp),
        issuerKey,
        key,
        'admin',
        ' Peer cert',
        peerOpts,
        :peer
      )

    [
      [{cert, encode_key(key)}, {issuerCert, encode_key(issuerKey)}]
      | acc
    ]
  end

  defp cert_chain(role, issuerCert, issuerKey, [cAOpts | rest], n, acc) do
    key = gen_key(:proplists.get_value(:key, cAOpts, default_key_gen()))

    cert =
      cert(
        role,
        :public_key.pkix_decode_cert(issuerCert, :otp),
        issuerKey,
        key,
        'webadmin',
        ' Intermidiate CA ' ++ :erlang.integer_to_list(n),
        cAOpts,
        :ca
      )

    cert_chain(role, cert, key, rest, n + 1, [{issuerCert, encode_key(issuerKey)} | acc])
  end

  defp cert(
         role,
         r_OTPCertificate(tbsCertificate: r_OTPTBSCertificate(subject: issuer)),
         privKey,
         key,
         contact,
         name,
         opts,
         type
       ) do
    tBS = cert_template()
    signAlgoId = sign_algorithm(privKey, opts)

    oTPTBS =
      r_OTPTBSCertificate(tBS,
        signature: signAlgoId,
        issuer: issuer,
        validity: validity(opts),
        subject:
          subject(
            contact,
            :erlang.atom_to_list(role) ++ name
          ),
        subjectPublicKeyInfo: public_key(key, signAlgoId),
        extensions: extensions(role, type, opts)
      )

    :public_key.pkix_sign(oTPTBS, privKey)
  end

  defp ca_config(root, cAsKeys) do
    [
      root
      | for {cA, _} <- cAsKeys do
          cA
        end
    ]
  end

  defp default_key_gen() do
    case :crypto.ec_curves() do
      [] ->
        {:rsa, 2048, 17}

      [curve | _] ->
        oid = :pubkey_cert_records.namedCurves(curve)
        {:namedCurve, oid}
    end
  end

  defp extensions(role, type, opts) do
    exts = :proplists.get_value(:extensions, opts, [])
    add_default_extensions(role, type, exts)
  end

  defp add_default_extensions(defaults0, exts) do
    defaults =
      :lists.filtermap(
        fn r_Extension(extnID: iD) = ext ->
          case :lists.keymember(iD, 2, exts) do
            true ->
              false

            false ->
              {true, ext}
          end
        end,
        defaults0
      )

    exts ++ defaults
  end

  defp encode_key({r_RSAPrivateKey(), r_RSASSA_PSS_params()} = key) do
    {asn1Type, dER, _} = :public_key.pem_entry_encode(:PrivateKeyInfo, key)
    {asn1Type, dER}
  end

  defp encode_key(r_RSAPrivateKey() = key) do
    {:RSAPrivateKey, :public_key.der_encode(:RSAPrivateKey, key)}
  end

  defp encode_key(r_ECPrivateKey() = key) do
    {:ECPrivateKey, :public_key.der_encode(:ECPrivateKey, key)}
  end

  defp encode_key(r_DSAPrivateKey() = key) do
    {:DSAPrivateKey, :public_key.der_encode(:DSAPrivateKey, key)}
  end
end
