defmodule :m_ssh do
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

  Record.defrecord(:r_ssh_msg_global_request, :ssh_msg_global_request,
    name: :undefined,
    want_reply: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_request_success, :ssh_msg_request_success, data: :undefined)
  Record.defrecord(:r_ssh_msg_request_failure, :ssh_msg_request_failure, [])

  Record.defrecord(:r_ssh_msg_channel_open, :ssh_msg_channel_open,
    channel_type: :undefined,
    sender_channel: :undefined,
    initial_window_size: :undefined,
    maximum_packet_size: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_open_confirmation, :ssh_msg_channel_open_confirmation,
    recipient_channel: :undefined,
    sender_channel: :undefined,
    initial_window_size: :undefined,
    maximum_packet_size: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_open_failure, :ssh_msg_channel_open_failure,
    recipient_channel: :undefined,
    reason: :undefined,
    description: :undefined,
    lang: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_window_adjust, :ssh_msg_channel_window_adjust,
    recipient_channel: :undefined,
    bytes_to_add: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_data, :ssh_msg_channel_data,
    recipient_channel: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_extended_data, :ssh_msg_channel_extended_data,
    recipient_channel: :undefined,
    data_type_code: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_eof, :ssh_msg_channel_eof, recipient_channel: :undefined)

  Record.defrecord(:r_ssh_msg_channel_close, :ssh_msg_channel_close, recipient_channel: :undefined)

  Record.defrecord(:r_ssh_msg_channel_request, :ssh_msg_channel_request,
    recipient_channel: :undefined,
    request_type: :undefined,
    want_reply: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_success, :ssh_msg_channel_success,
    recipient_channel: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_failure, :ssh_msg_channel_failure,
    recipient_channel: :undefined
  )

  Record.defrecord(:r_channel, :channel,
    type: :undefined,
    sys: :undefined,
    user: :undefined,
    flow_control: :undefined,
    local_id: :undefined,
    recv_window_size: :undefined,
    recv_window_pending: 0,
    recv_packet_size: :undefined,
    recv_close: false,
    remote_id: :undefined,
    send_window_size: :undefined,
    send_packet_size: :undefined,
    sent_close: false,
    send_buf: []
  )

  Record.defrecord(:r_connection, :connection,
    requests: [],
    channel_cache: :undefined,
    channel_id_seed: :undefined,
    cli_spec: :undefined,
    options: :undefined,
    exec: :undefined,
    system_supervisor: :undefined,
    sub_system_supervisor: :undefined,
    connection_supervisor: :undefined
  )

  Record.defrecord(:r_AlgorithmIdentifier_PKCS1, :"AlgorithmIdentifier-PKCS1",
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_AttributePKCS_7, :"AttributePKCS-7",
    type: :undefined,
    values: :undefined
  )

  Record.defrecord(:r_AlgorithmIdentifierPKCS_7, :"AlgorithmIdentifierPKCS-7",
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_AlgorithmIdentifierPKCS_10, :"AlgorithmIdentifierPKCS-10",
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_AttributePKCS_10, :"AttributePKCS-10",
    type: :undefined,
    values: :undefined
  )

  Record.defrecord(:r_SubjectPublicKeyInfo_PKCS_10, :"SubjectPublicKeyInfo-PKCS-10",
    algorithm: :undefined,
    subjectPublicKey: :undefined
  )

  Record.defrecord(:r_ECPrivateKey, :ECPrivateKey,
    version: :undefined,
    privateKey: :undefined,
    parameters: :asn1_NOVALUE,
    publicKey: :asn1_NOVALUE
  )

  Record.defrecord(:r_DSAPrivateKey, :DSAPrivateKey,
    version: :undefined,
    p: :undefined,
    q: :undefined,
    g: :undefined,
    y: :undefined,
    x: :undefined
  )

  Record.defrecord(:r_DHParameter, :DHParameter,
    prime: :undefined,
    base: :undefined,
    privateValueLength: :asn1_NOVALUE
  )

  Record.defrecord(:r_DigestAlgorithm, :DigestAlgorithm,
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_DigestInfoPKCS_1, :"DigestInfoPKCS-1",
    digestAlgorithm: :undefined,
    digest: :undefined
  )

  Record.defrecord(:r_RSASSA_AlgorithmIdentifier, :"RSASSA-AlgorithmIdentifier",
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_RSASSA_PSS_params, :"RSASSA-PSS-params",
    hashAlgorithm: :asn1_DEFAULT,
    maskGenAlgorithm: :asn1_DEFAULT,
    saltLength: :asn1_DEFAULT,
    trailerField: :asn1_DEFAULT
  )

  Record.defrecord(:r_RSAES_AlgorithmIdentifier, :"RSAES-AlgorithmIdentifier",
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_RSAES_OAEP_params, :"RSAES-OAEP-params",
    hashAlgorithm: :asn1_DEFAULT,
    maskGenAlgorithm: :asn1_DEFAULT,
    pSourceAlgorithm: :asn1_DEFAULT
  )

  Record.defrecord(:r_OtherPrimeInfo, :OtherPrimeInfo,
    prime: :undefined,
    exponent: :undefined,
    coefficient: :undefined
  )

  Record.defrecord(:r_RSAPrivateKey, :RSAPrivateKey,
    version: :undefined,
    modulus: :undefined,
    publicExponent: :undefined,
    privateExponent: :undefined,
    prime1: :undefined,
    prime2: :undefined,
    exponent1: :undefined,
    exponent2: :undefined,
    coefficient: :undefined,
    otherPrimeInfos: :asn1_NOVALUE
  )

  Record.defrecord(:r_RSAPublicKey, :RSAPublicKey,
    modulus: :undefined,
    publicExponent: :undefined
  )

  Record.defrecord(:r_PSourceAlgorithm, :PSourceAlgorithm,
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_MaskGenAlgorithm, :MaskGenAlgorithm,
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_HashAlgorithm, :HashAlgorithm,
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_Curve, :Curve, a: :undefined, b: :undefined, seed: :asn1_NOVALUE)

  Record.defrecord(:r_ECParameters, :ECParameters,
    version: :undefined,
    fieldID: :undefined,
    curve: :undefined,
    base: :undefined,
    order: :undefined,
    cofactor: :asn1_NOVALUE
  )

  Record.defrecord(:r_Pentanomial, :Pentanomial, k1: :undefined, k2: :undefined, k3: :undefined)

  Record.defrecord(:r_Characteristic_two, :"Characteristic-two",
    m: :undefined,
    basis: :undefined,
    parameters: :undefined
  )

  Record.defrecord(:r_ECDSA_Sig_Value, :"ECDSA-Sig-Value", r: :undefined, s: :undefined)

  Record.defrecord(:r_FieldID, :FieldID,
    fieldType: :undefined,
    parameters: :undefined
  )

  Record.defrecord(:r_ValidationParms, :ValidationParms,
    seed: :undefined,
    pgenCounter: :undefined
  )

  Record.defrecord(:r_DomainParameters, :DomainParameters,
    p: :undefined,
    g: :undefined,
    q: :undefined,
    j: :asn1_NOVALUE,
    validationParms: :asn1_NOVALUE
  )

  Record.defrecord(:r_Dss_Sig_Value, :"Dss-Sig-Value", r: :undefined, s: :undefined)
  Record.defrecord(:r_Dss_Parms, :"Dss-Parms", p: :undefined, q: :undefined, g: :undefined)

  Record.defrecord(:r_ACClearAttrs, :ACClearAttrs,
    acIssuer: :undefined,
    acSerial: :undefined,
    attrs: :undefined
  )

  Record.defrecord(:r_AAControls, :AAControls,
    pathLenConstraint: :asn1_NOVALUE,
    permittedAttrs: :asn1_NOVALUE,
    excludedAttrs: :asn1_NOVALUE,
    permitUnSpecified: :asn1_DEFAULT
  )

  Record.defrecord(:r_SecurityCategory, :SecurityCategory,
    type: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_Clearance, :Clearance,
    policyId: :undefined,
    classList: :asn1_DEFAULT,
    securityCategories: :asn1_NOVALUE
  )

  Record.defrecord(:r_RoleSyntax, :RoleSyntax,
    roleAuthority: :asn1_NOVALUE,
    roleName: :undefined
  )

  Record.defrecord(:r_SvceAuthInfo, :SvceAuthInfo,
    service: :undefined,
    ident: :undefined,
    authInfo: :asn1_NOVALUE
  )

  Record.defrecord(:r_IetfAttrSyntax, :IetfAttrSyntax,
    policyAuthority: :asn1_NOVALUE,
    values: :undefined
  )

  Record.defrecord(:r_TargetCert, :TargetCert,
    targetCertificate: :undefined,
    targetName: :asn1_NOVALUE,
    certDigestInfo: :asn1_NOVALUE
  )

  Record.defrecord(:r_AttCertValidityPeriod, :AttCertValidityPeriod,
    notBeforeTime: :undefined,
    notAfterTime: :undefined
  )

  Record.defrecord(:r_IssuerSerial, :IssuerSerial,
    issuer: :undefined,
    serial: :undefined,
    issuerUID: :asn1_NOVALUE
  )

  Record.defrecord(:r_V2Form, :V2Form,
    issuerName: :asn1_NOVALUE,
    baseCertificateID: :asn1_NOVALUE,
    objectDigestInfo: :asn1_NOVALUE
  )

  Record.defrecord(:r_ObjectDigestInfo, :ObjectDigestInfo,
    digestedObjectType: :undefined,
    otherObjectTypeID: :asn1_NOVALUE,
    digestAlgorithm: :undefined,
    objectDigest: :undefined
  )

  Record.defrecord(:r_Holder, :Holder,
    baseCertificateID: :asn1_NOVALUE,
    entityName: :asn1_NOVALUE,
    objectDigestInfo: :asn1_NOVALUE
  )

  Record.defrecord(:r_AttributeCertificateInfo, :AttributeCertificateInfo,
    version: :undefined,
    holder: :undefined,
    issuer: :undefined,
    signature: :undefined,
    serialNumber: :undefined,
    attrCertValidityPeriod: :undefined,
    attributes: :undefined,
    issuerUniqueID: :asn1_NOVALUE,
    extensions: :asn1_NOVALUE
  )

  Record.defrecord(:r_AttributeCertificate, :AttributeCertificate,
    acinfo: :undefined,
    signatureAlgorithm: :undefined,
    signatureValue: :undefined
  )

  Record.defrecord(:r_IssuingDistributionPoint, :IssuingDistributionPoint,
    distributionPoint: :asn1_NOVALUE,
    onlyContainsUserCerts: :asn1_DEFAULT,
    onlyContainsCACerts: :asn1_DEFAULT,
    onlySomeReasons: :asn1_NOVALUE,
    indirectCRL: :asn1_DEFAULT,
    onlyContainsAttributeCerts: :asn1_DEFAULT
  )

  Record.defrecord(:r_AccessDescription, :AccessDescription,
    accessMethod: :undefined,
    accessLocation: :undefined
  )

  Record.defrecord(:r_DistributionPoint, :DistributionPoint,
    distributionPoint: :asn1_NOVALUE,
    reasons: :asn1_NOVALUE,
    cRLIssuer: :asn1_NOVALUE
  )

  Record.defrecord(:r_PolicyConstraints, :PolicyConstraints,
    requireExplicitPolicy: :asn1_NOVALUE,
    inhibitPolicyMapping: :asn1_NOVALUE
  )

  Record.defrecord(:r_GeneralSubtree, :GeneralSubtree,
    base: :undefined,
    minimum: :asn1_DEFAULT,
    maximum: :asn1_NOVALUE
  )

  Record.defrecord(:r_NameConstraints, :NameConstraints,
    permittedSubtrees: :asn1_NOVALUE,
    excludedSubtrees: :asn1_NOVALUE
  )

  Record.defrecord(:r_BasicConstraints, :BasicConstraints,
    cA: :asn1_DEFAULT,
    pathLenConstraint: :asn1_NOVALUE
  )

  Record.defrecord(:r_EDIPartyName, :EDIPartyName,
    nameAssigner: :asn1_NOVALUE,
    partyName: :undefined
  )

  Record.defrecord(:r_AnotherName, :AnotherName,
    "type-id": :undefined,
    value: :undefined
  )

  Record.defrecord(:r_PolicyMappings_SEQOF, :PolicyMappings_SEQOF,
    issuerDomainPolicy: :undefined,
    subjectDomainPolicy: :undefined
  )

  Record.defrecord(:r_NoticeReference, :NoticeReference,
    organization: :undefined,
    noticeNumbers: :undefined
  )

  Record.defrecord(:r_UserNotice, :UserNotice,
    noticeRef: :asn1_NOVALUE,
    explicitText: :asn1_NOVALUE
  )

  Record.defrecord(:r_PolicyQualifierInfo, :PolicyQualifierInfo,
    policyQualifierId: :undefined,
    qualifier: :undefined
  )

  Record.defrecord(:r_PolicyInformation, :PolicyInformation,
    policyIdentifier: :undefined,
    policyQualifiers: :asn1_NOVALUE
  )

  Record.defrecord(:r_PrivateKeyUsagePeriod, :PrivateKeyUsagePeriod,
    notBefore: :asn1_NOVALUE,
    notAfter: :asn1_NOVALUE
  )

  Record.defrecord(:r_AuthorityKeyIdentifier, :AuthorityKeyIdentifier,
    keyIdentifier: :asn1_NOVALUE,
    authorityCertIssuer: :asn1_NOVALUE,
    authorityCertSerialNumber: :asn1_NOVALUE
  )

  Record.defrecord(:r_EncryptedData, :EncryptedData,
    version: :undefined,
    encryptedContentInfo: :undefined
  )

  Record.defrecord(:r_DigestedData, :DigestedData,
    version: :undefined,
    digestAlgorithm: :undefined,
    contentInfo: :undefined,
    digest: :undefined
  )

  Record.defrecord(:r_SignedAndEnvelopedData, :SignedAndEnvelopedData,
    version: :undefined,
    recipientInfos: :undefined,
    digestAlgorithms: :undefined,
    encryptedContentInfo: :undefined,
    certificates: :asn1_NOVALUE,
    crls: :asn1_NOVALUE,
    signerInfos: :undefined
  )

  Record.defrecord(:r_RecipientInfo, :RecipientInfo,
    version: :undefined,
    issuerAndSerialNumber: :undefined,
    keyEncryptionAlgorithm: :undefined,
    encryptedKey: :undefined
  )

  Record.defrecord(:r_EncryptedContentInfo, :EncryptedContentInfo,
    contentType: :undefined,
    contentEncryptionAlgorithm: :undefined,
    encryptedContent: :asn1_NOVALUE
  )

  Record.defrecord(:r_EnvelopedData, :EnvelopedData,
    version: :undefined,
    recipientInfos: :undefined,
    encryptedContentInfo: :undefined
  )

  Record.defrecord(:r_DigestInfoPKCS_7, :"DigestInfoPKCS-7",
    digestAlgorithm: :undefined,
    digest: :undefined
  )

  Record.defrecord(:r_SignerInfo, :SignerInfo,
    version: :undefined,
    issuerAndSerialNumber: :undefined,
    digestAlgorithm: :undefined,
    authenticatedAttributes: :asn1_NOVALUE,
    digestEncryptionAlgorithm: :undefined,
    encryptedDigest: :undefined,
    unauthenticatedAttributes: :asn1_NOVALUE
  )

  Record.defrecord(
    :r_SignerInfo_unauthenticatedAttributes_uaSet_SETOF,
    :SignerInfo_unauthenticatedAttributes_uaSet_SETOF,
    type: :undefined,
    values: :undefined
  )

  Record.defrecord(
    :r_SignerInfo_unauthenticatedAttributes_uaSequence_SEQOF,
    :SignerInfo_unauthenticatedAttributes_uaSequence_SEQOF,
    type: :undefined,
    values: :undefined
  )

  Record.defrecord(:r_SignedData, :SignedData,
    version: :undefined,
    digestAlgorithms: :undefined,
    contentInfo: :undefined,
    certificates: :asn1_NOVALUE,
    crls: :asn1_NOVALUE,
    signerInfos: :undefined
  )

  Record.defrecord(:r_ContentInfo, :ContentInfo,
    contentType: :undefined,
    content: :asn1_NOVALUE
  )

  Record.defrecord(:r_KeyEncryptionAlgorithmIdentifier, :KeyEncryptionAlgorithmIdentifier,
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_IssuerAndSerialNumber, :IssuerAndSerialNumber,
    issuer: :undefined,
    serialNumber: :undefined
  )

  Record.defrecord(:r_DigestEncryptionAlgorithmIdentifier, :DigestEncryptionAlgorithmIdentifier,
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_DigestAlgorithmIdentifier, :DigestAlgorithmIdentifier,
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_ContentEncryptionAlgorithmIdentifier, :ContentEncryptionAlgorithmIdentifier,
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(
    :r_SignerInfoAuthenticatedAttributes_aaSet_SETOF,
    :SignerInfoAuthenticatedAttributes_aaSet_SETOF,
    type: :undefined,
    values: :undefined
  )

  Record.defrecord(
    :r_SignerInfoAuthenticatedAttributes_aaSequence_SEQOF,
    :SignerInfoAuthenticatedAttributes_aaSequence_SEQOF,
    type: :undefined,
    values: :undefined
  )

  Record.defrecord(:r_CertificationRequest, :CertificationRequest,
    certificationRequestInfo: :undefined,
    signatureAlgorithm: :undefined,
    signature: :undefined
  )

  Record.defrecord(
    :r_CertificationRequest_signatureAlgorithm,
    :CertificationRequest_signatureAlgorithm,
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_CertificationRequestInfo, :CertificationRequestInfo,
    version: :undefined,
    subject: :undefined,
    subjectPKInfo: :undefined,
    attributes: :undefined
  )

  Record.defrecord(
    :r_CertificationRequestInfo_subjectPKInfo,
    :CertificationRequestInfo_subjectPKInfo,
    algorithm: :undefined,
    subjectPublicKey: :undefined
  )

  Record.defrecord(
    :r_CertificationRequestInfo_subjectPKInfo_algorithm,
    :CertificationRequestInfo_subjectPKInfo_algorithm,
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(
    :r_CertificationRequestInfo_attributes_SETOF,
    :CertificationRequestInfo_attributes_SETOF,
    type: :undefined,
    values: :undefined
  )

  Record.defrecord(:r_PreferredSignatureAlgorithm, :PreferredSignatureAlgorithm,
    sigIdentifier: :undefined,
    certIdentifier: :asn1_NOVALUE
  )

  Record.defrecord(:r_CrlID, :CrlID,
    crlUrl: :asn1_NOVALUE,
    crlNum: :asn1_NOVALUE,
    crlTime: :asn1_NOVALUE
  )

  Record.defrecord(:r_ServiceLocator, :ServiceLocator,
    issuer: :undefined,
    locator: :undefined
  )

  Record.defrecord(:r_RevokedInfo, :RevokedInfo,
    revocationTime: :undefined,
    revocationReason: :asn1_NOVALUE
  )

  Record.defrecord(:r_SingleResponse, :SingleResponse,
    certID: :undefined,
    certStatus: :undefined,
    thisUpdate: :undefined,
    nextUpdate: :asn1_NOVALUE,
    singleExtensions: :asn1_NOVALUE
  )

  Record.defrecord(:r_ResponseData, :ResponseData,
    version: :asn1_DEFAULT,
    responderID: :undefined,
    producedAt: :undefined,
    responses: :undefined,
    responseExtensions: :asn1_NOVALUE
  )

  Record.defrecord(:r_BasicOCSPResponse, :BasicOCSPResponse,
    tbsResponseData: :undefined,
    signatureAlgorithm: :undefined,
    signature: :undefined,
    certs: :asn1_NOVALUE
  )

  Record.defrecord(:r_ResponseBytes, :ResponseBytes,
    responseType: :undefined,
    response: :undefined
  )

  Record.defrecord(:r_OCSPResponse, :OCSPResponse,
    responseStatus: :undefined,
    responseBytes: :asn1_NOVALUE
  )

  Record.defrecord(:r_CertID, :CertID,
    hashAlgorithm: :undefined,
    issuerNameHash: :undefined,
    issuerKeyHash: :undefined,
    serialNumber: :undefined
  )

  Record.defrecord(:r_Request, :Request,
    reqCert: :undefined,
    singleRequestExtensions: :asn1_NOVALUE
  )

  Record.defrecord(:r_Signature, :Signature,
    signatureAlgorithm: :undefined,
    signature: :undefined,
    certs: :asn1_NOVALUE
  )

  Record.defrecord(:r_TBSRequest, :TBSRequest,
    version: :asn1_DEFAULT,
    requestorName: :asn1_NOVALUE,
    requestList: :undefined,
    requestExtensions: :asn1_NOVALUE
  )

  Record.defrecord(:r_OCSPRequest, :OCSPRequest,
    tbsRequest: :undefined,
    optionalSignature: :asn1_NOVALUE
  )

  Record.defrecord(:r_TeletexDomainDefinedAttribute, :TeletexDomainDefinedAttribute,
    type: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_PresentationAddress, :PresentationAddress,
    pSelector: :asn1_NOVALUE,
    sSelector: :asn1_NOVALUE,
    tSelector: :asn1_NOVALUE,
    nAddresses: :undefined
  )

  Record.defrecord(
    :r_ExtendedNetworkAddress_e163_4_address,
    :"ExtendedNetworkAddress_e163-4-address",
    number: :undefined,
    "sub-address": :asn1_NOVALUE
  )

  Record.defrecord(:r_PDSParameter, :PDSParameter,
    "printable-string": :asn1_NOVALUE,
    "teletex-string": :asn1_NOVALUE
  )

  Record.defrecord(:r_UnformattedPostalAddress, :UnformattedPostalAddress,
    "printable-address": :asn1_NOVALUE,
    "teletex-string": :asn1_NOVALUE
  )

  Record.defrecord(:r_TeletexPersonalName, :TeletexPersonalName,
    surname: :undefined,
    "given-name": :asn1_NOVALUE,
    initials: :asn1_NOVALUE,
    "generation-qualifier": :asn1_NOVALUE
  )

  Record.defrecord(:r_ExtensionAttribute, :ExtensionAttribute,
    "extension-attribute-type": :undefined,
    "extension-attribute-value": :undefined
  )

  Record.defrecord(:r_BuiltInDomainDefinedAttribute, :BuiltInDomainDefinedAttribute,
    type: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_PersonalName, :PersonalName,
    surname: :undefined,
    "given-name": :asn1_NOVALUE,
    initials: :asn1_NOVALUE,
    "generation-qualifier": :asn1_NOVALUE
  )

  Record.defrecord(:r_BuiltInStandardAttributes, :BuiltInStandardAttributes,
    "country-name": :asn1_NOVALUE,
    "administration-domain-name": :asn1_NOVALUE,
    "network-address": :asn1_NOVALUE,
    "terminal-identifier": :asn1_NOVALUE,
    "private-domain-name": :asn1_NOVALUE,
    "organization-name": :asn1_NOVALUE,
    "numeric-user-identifier": :asn1_NOVALUE,
    "personal-name": :asn1_NOVALUE,
    "organizational-unit-names": :asn1_NOVALUE
  )

  Record.defrecord(:r_ORAddress, :ORAddress,
    "built-in-standard-attributes": :undefined,
    "built-in-domain-defined-attributes": :asn1_NOVALUE,
    "extension-attributes": :asn1_NOVALUE
  )

  Record.defrecord(:r_AlgorithmIdentifier, :AlgorithmIdentifier,
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_TBSCertList, :TBSCertList,
    version: :asn1_NOVALUE,
    signature: :undefined,
    issuer: :undefined,
    thisUpdate: :undefined,
    nextUpdate: :asn1_NOVALUE,
    revokedCertificates: :asn1_NOVALUE,
    crlExtensions: :asn1_NOVALUE
  )

  Record.defrecord(
    :r_TBSCertList_revokedCertificates_SEQOF,
    :TBSCertList_revokedCertificates_SEQOF,
    userCertificate: :undefined,
    revocationDate: :undefined,
    crlEntryExtensions: :asn1_NOVALUE
  )

  Record.defrecord(:r_CertificateList, :CertificateList,
    tbsCertList: :undefined,
    signatureAlgorithm: :undefined,
    signature: :undefined
  )

  Record.defrecord(:r_Extension, :Extension,
    extnID: :undefined,
    critical: :asn1_DEFAULT,
    extnValue: :undefined
  )

  Record.defrecord(:r_SubjectPublicKeyInfo, :SubjectPublicKeyInfo,
    algorithm: :undefined,
    subjectPublicKey: :undefined
  )

  Record.defrecord(:r_Validity, :Validity,
    notBefore: :undefined,
    notAfter: :undefined
  )

  Record.defrecord(:r_TBSCertificate, :TBSCertificate,
    version: :asn1_DEFAULT,
    serialNumber: :undefined,
    signature: :undefined,
    issuer: :undefined,
    validity: :undefined,
    subject: :undefined,
    subjectPublicKeyInfo: :undefined,
    issuerUniqueID: :asn1_NOVALUE,
    subjectUniqueID: :asn1_NOVALUE,
    extensions: :asn1_NOVALUE
  )

  Record.defrecord(:r_Certificate, :Certificate,
    tbsCertificate: :undefined,
    signatureAlgorithm: :undefined,
    signature: :undefined
  )

  Record.defrecord(:r_AttributeTypeAndValue, :AttributeTypeAndValue,
    type: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_Attribute, :Attribute,
    type: :undefined,
    values: :undefined
  )

  Record.defrecord(:r_Extension_Any, :"Extension-Any",
    extnID: :undefined,
    critical: :asn1_DEFAULT,
    extnValue: :undefined
  )

  Record.defrecord(:r_OTPExtension, :OTPExtension,
    extnID: :undefined,
    critical: :asn1_DEFAULT,
    extnValue: :undefined
  )

  Record.defrecord(:r_OTPExtensionAttribute, :OTPExtensionAttribute,
    extensionAttributeType: :undefined,
    extensionAttributeValue: :undefined
  )

  Record.defrecord(:r_OTPCharacteristic_two, :"OTPCharacteristic-two",
    m: :undefined,
    basis: :undefined,
    parameters: :undefined
  )

  Record.defrecord(:r_OTPFieldID, :OTPFieldID,
    fieldType: :undefined,
    parameters: :undefined
  )

  Record.defrecord(:r_PublicKeyAlgorithm, :PublicKeyAlgorithm,
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_SignatureAlgorithm_Any, :"SignatureAlgorithm-Any",
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_SignatureAlgorithm, :SignatureAlgorithm,
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_OTPSubjectPublicKeyInfo_Any, :"OTPSubjectPublicKeyInfo-Any",
    algorithm: :undefined,
    subjectPublicKey: :undefined
  )

  Record.defrecord(:r_OTPSubjectPublicKeyInfo, :OTPSubjectPublicKeyInfo,
    algorithm: :undefined,
    subjectPublicKey: :undefined
  )

  Record.defrecord(:r_OTPOLDSubjectPublicKeyInfo, :OTPOLDSubjectPublicKeyInfo,
    algorithm: :undefined,
    subjectPublicKey: :undefined
  )

  Record.defrecord(:r_OTPOLDSubjectPublicKeyInfo_algorithm, :OTPOLDSubjectPublicKeyInfo_algorithm,
    algo: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_OTPAttributeTypeAndValue, :OTPAttributeTypeAndValue,
    type: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_OTPTBSCertificate, :OTPTBSCertificate,
    version: :asn1_DEFAULT,
    serialNumber: :undefined,
    signature: :undefined,
    issuer: :undefined,
    validity: :undefined,
    subject: :undefined,
    subjectPublicKeyInfo: :undefined,
    issuerUniqueID: :asn1_NOVALUE,
    subjectUniqueID: :asn1_NOVALUE,
    extensions: :asn1_NOVALUE
  )

  Record.defrecord(:r_OTPCertificate, :OTPCertificate,
    tbsCertificate: :undefined,
    signatureAlgorithm: :undefined,
    signature: :undefined
  )

  Record.defrecord(:r_AlgorithmIdentifierPKCS5v2_0, :"AlgorithmIdentifierPKCS5v2-0",
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_PKAttribute, :PKAttribute,
    type: :undefined,
    values: :undefined,
    valuesWithContext: :asn1_NOVALUE
  )

  Record.defrecord(:r_PKAttribute_valuesWithContext_SETOF, :PKAttribute_valuesWithContext_SETOF,
    value: :undefined,
    contextList: :undefined
  )

  Record.defrecord(:r_AlgorithmIdentifierPKCS_8, :"AlgorithmIdentifierPKCS-8",
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_RC5_CBC_Parameters, :"RC5-CBC-Parameters",
    version: :undefined,
    rounds: :undefined,
    blockSizeInBits: :undefined,
    iv: :asn1_NOVALUE
  )

  Record.defrecord(:r_RC2_CBC_Parameter, :"RC2-CBC-Parameter",
    rc2ParameterVersion: :asn1_NOVALUE,
    iv: :undefined
  )

  Record.defrecord(:r_PBMAC1_params, :"PBMAC1-params",
    keyDerivationFunc: :undefined,
    messageAuthScheme: :undefined
  )

  Record.defrecord(:r_PBMAC1_params_keyDerivationFunc, :"PBMAC1-params_keyDerivationFunc",
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_PBMAC1_params_messageAuthScheme, :"PBMAC1-params_messageAuthScheme",
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_PBES2_params, :"PBES2-params",
    keyDerivationFunc: :undefined,
    encryptionScheme: :undefined
  )

  Record.defrecord(:r_PBES2_params_keyDerivationFunc, :"PBES2-params_keyDerivationFunc",
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_PBES2_params_encryptionScheme, :"PBES2-params_encryptionScheme",
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_PBEParameter, :PBEParameter,
    salt: :undefined,
    iterationCount: :undefined
  )

  Record.defrecord(:r_PBKDF2_params, :"PBKDF2-params",
    salt: :undefined,
    iterationCount: :undefined,
    keyLength: :asn1_NOVALUE,
    prf: :asn1_DEFAULT
  )

  Record.defrecord(:r_PBKDF2_params_salt_otherSource, :"PBKDF2-params_salt_otherSource",
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_PBKDF2_params_prf, :"PBKDF2-params_prf",
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_Context, :Context,
    contextType: :undefined,
    contextValues: :undefined,
    fallback: :asn1_DEFAULT
  )

  Record.defrecord(:r_EncryptedPrivateKeyInfo, :EncryptedPrivateKeyInfo,
    encryptionAlgorithm: :undefined,
    encryptedData: :undefined
  )

  Record.defrecord(
    :r_EncryptedPrivateKeyInfo_encryptionAlgorithm,
    :EncryptedPrivateKeyInfo_encryptionAlgorithm,
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

  Record.defrecord(:r_Attributes_SETOF, :Attributes_SETOF,
    type: :undefined,
    values: :undefined,
    valuesWithContext: :asn1_NOVALUE
  )

  Record.defrecord(
    :r_Attributes_SETOF_valuesWithContext_SETOF,
    :Attributes_SETOF_valuesWithContext_SETOF,
    value: :undefined,
    contextList: :undefined
  )

  Record.defrecord(:r_PrivateKeyInfo, :PrivateKeyInfo,
    version: :undefined,
    privateKeyAlgorithm: :undefined,
    privateKey: :undefined,
    attributes: :asn1_NOVALUE
  )

  Record.defrecord(:r_PrivateKeyInfo_privateKeyAlgorithm, :PrivateKeyInfo_privateKeyAlgorithm,
    algorithm: :undefined,
    parameters: :asn1_NOVALUE
  )

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

  Record.defrecord(:r_file_info, :file_info,
    size: :undefined,
    type: :undefined,
    access: :undefined,
    atime: :undefined,
    mtime: :undefined,
    ctime: :undefined,
    mode: :undefined,
    links: :undefined,
    major_device: :undefined,
    minor_device: :undefined,
    inode: :undefined,
    uid: :undefined,
    gid: :undefined
  )

  Record.defrecord(:r_file_descriptor, :file_descriptor,
    module: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_hostent, :hostent,
    h_name: :undefined,
    h_aliases: [],
    h_addrtype: :undefined,
    h_length: :undefined,
    h_addr_list: []
  )

  def start() do
    start(:temporary)
  end

  def start(type) do
    case :application.ensure_all_started(:ssh, type) do
      {:ok, _} ->
        :ssh_transport.clear_default_algorithms_env()
        :ssh_transport.default_algorithms()
        :ok

      other ->
        other
    end
  end

  def stop() do
    :application.stop(:ssh)
  end

  def connect(openTcpSocket, options)
      when is_port(openTcpSocket) and is_list(options) do
    connect(openTcpSocket, options, :infinity)
  end

  def connect(socket, userOptions, negotiationTimeout)
      when is_port(socket) and is_list(userOptions) do
    case :ssh_options.handle_options(
           :client,
           userOptions
         ) do
      {:error, error} ->
        {:error, error}

      options ->
        case valid_socket_to_use(
               socket,
               :ssh_options.get_value(:user_options, :transport, options, :ssh, 138)
             ) do
          :ok ->
            connect_socket(socket, options, negotiationTimeout)

          {:error, sockError} ->
            {:error, sockError}
        end
    end
  end

  def connect(host, port, options)
      when is_integer(port) and
             port > 0 and is_list(options) do
    connect(host, port, options, :infinity)
  end

  def connect(host0, port, userOptions, negotiationTimeout)
      when is_integer(port) and port > 0 and
             is_list(userOptions) do
    case :ssh_options.handle_options(
           :client,
           userOptions
         ) do
      {:error, _Reason} = error ->
        error

      options ->
        {_, transport, _} =
          transportOpts = :ssh_options.get_value(:user_options, :transport, options, :ssh, 165)

        connectionTimeout =
          :ssh_options.get_value(:user_options, :connect_timeout, options, :ssh, 166)

        socketOpts = [
          {:active, false}
          | :ssh_options.get_value(:user_options, :socket_options, options, :ssh, 167)
        ]

        host = mangle_connect_address(host0, socketOpts)

        try do
          transport.connect(host, port, socketOpts, connectionTimeout)
        catch
          :exit, {:function_clause, _F} ->
            {:error, {:options, {:transport, transportOpts}}}

          :exit, :badarg ->
            {:error, {:options, {:socket_options, socketOpts}}}
        else
          {:ok, socket} ->
            connect_socket(
              socket,
              :ssh_options.put_value(:internal_options, {:host, host}, options, :ssh, 172),
              negotiationTimeout
            )

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp connect_socket(socket, options0, negotiationTimeout) do
    {:ok, {host, port}} = :inet.sockname(socket)
    profile = :ssh_options.get_value(:user_options, :profile, options0, :ssh, 187)
    {:ok, systemSup} = :sshc_sup.start_child(host, port, profile, options0)

    {:ok, subSysSup} =
      :ssh_system_sup.start_subsystem(systemSup, :client, host, port, profile, options0)

    connectionSup = :ssh_system_sup.connection_supervisor(systemSup)

    opts =
      :ssh_options.put_value(
        :internal_options,
        [
          {:user_pid, self()},
          {:supervisors,
           [
             {:system_sup, systemSup},
             {:subsystem_sup, subSysSup},
             {:connection_sup, connectionSup}
           ]}
        ],
        options0,
        :ssh,
        195
      )

    :ssh_connection_handler.start_connection(:client, socket, opts, negotiationTimeout)
  end

  def close(connectionRef) do
    :ssh_connection_handler.stop(connectionRef)
  end

  def connection_info(connectionRef) do
    connection_info(connectionRef, [])
  end

  def connection_info(connectionRef, key) do
    :ssh_connection_handler.connection_info(
      connectionRef,
      key
    )
  end

  def channel_info(connectionRef, channelId, options) do
    :ssh_connection_handler.channel_info(connectionRef, channelId, options)
  end

  def daemon(port) do
    daemon(port, [])
  end

  def daemon(socket, userOptions) when is_port(socket) do
    try do
      %{} =
        options =
        :ssh_options.handle_options(
          :server,
          userOptions
        )

      case valid_socket_to_use(
             socket,
             :ssh_options.get_value(:user_options, :transport, options, :ssh, 279)
           ) do
        :ok ->
          {:ok, {iP, port}} = :inet.sockname(socket)

          finalize_start(
            iP,
            port,
            :ssh_options.get_value(:user_options, :profile, options, :ssh, 282),
            :ssh_options.put_value(
              :internal_options,
              {:connected_socket, socket},
              options,
              :ssh,
              283
            ),
            fn opts, defaultResult ->
              try do
                :ssh_acceptor.handle_established_connection(
                  iP,
                  port,
                  opts,
                  socket
                )
              catch
                c, r ->
                  {:error, {:could_not_start_connection, {c, r}}}
              else
                {:error, error} ->
                  {:error, error}

                _ ->
                  defaultResult
              end
            end
          )

        {:error, sockError} ->
          {:error, sockError}
      end
    catch
      :bad_fd ->
        {:error, :bad_fd}

      :bad_socket ->
        {:error, :bad_socket}

      :error, {:badmatch, {:error, error}} ->
        {:error, error}

      :error, error ->
        {:error, error}

      _C, _E ->
        {:error, {:cannot_start_daemon, _C, _E}}
    end
  end

  def daemon(port, userOptions)
      when 0 <= port and
             port <= 65535 do
    daemon(:any, port, userOptions)
  end

  def daemon(host0, port0, userOptions0)
      when (0 <= port0 and
              port0 <= 65535 and
              host0 == :any) or
             host0 == :loopback or
             is_tuple(host0) do
    try do
      {host1, userOptions} =
        handle_daemon_args(
          host0,
          userOptions0
        )

      %{} =
        options0 =
        :ssh_options.handle_options(
          :server,
          userOptions
        )

      {open_listen_socket(host1, port0, options0), options0}
    catch
      :bad_fd ->
        {:error, :bad_fd}

      :bad_socket ->
        {:error, :bad_socket}

      :error, {:badmatch, {:error, error}} ->
        {:error, error}

      :error, error ->
        {:error, error}

      _C, _E ->
        {:error, {:cannot_start_daemon, _C, _E}}
    else
      {{{host, port}, listenSocket}, options1} ->
        try do
          finalize_start(
            host,
            port,
            :ssh_options.get_value(:user_options, :profile, options1, :ssh, 333),
            :ssh_options.put_value(
              :internal_options,
              {:lsocket, {listenSocket, self()}},
              options1,
              :ssh,
              334
            ),
            fn opts, result ->
              {_, callback, _} =
                :ssh_options.get_value(:user_options, :transport, opts, :ssh, 336)

              receive do
                {:request_control, ^listenSocket, reqPid} ->
                  :ok =
                    callback.controlling_process(
                      listenSocket,
                      reqPid
                    )

                  send(reqPid, {:its_yours, listenSocket})
                  result
              end
            end
          )
        catch
          :error, error ->
            close_listen_socket(listenSocket, options1)
            :erlang.error(error)

          :exit, exit ->
            close_listen_socket(listenSocket, options1)
            exit(exit)
        else
          {:error, err} ->
            close_listen_socket(listenSocket, options1)
            {:error, err}

          oK ->
            oK
        end
    end
  end

  def daemon(_, _, _) do
    {:error, :badarg}
  end

  def daemon_info(daemonRef) do
    case (try do
            :ssh_system_sup.acceptor_supervisor(daemonRef)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      asupPid when is_pid(asupPid) ->
        [{host, port, profile}] =
          for {{:ssh_acceptor_sup, hst, prt, prf}, _Pid, :worker, [:ssh_acceptor]} <-
                :supervisor.which_children(asupPid) do
            {hst, prt, prf}
          end

        iP =
          case :inet.parse_strict_address(host) do
            {:ok, iP0} ->
              iP0

            _ ->
              host
          end

        opts =
          case :ssh_system_sup.get_options(daemonRef, host, port, profile) do
            {:ok, optMap} ->
              :lists.sort(
                :maps.to_list(
                  :ssh_options.keep_set_options(
                    :server,
                    :ssh_options.keep_user_options(
                      :server,
                      optMap
                    )
                  )
                )
              )

            _ ->
              []
          end

        {:ok, [{:port, port}, {:ip, iP}, {:profile, profile}, {:options, opts}]}

      _ ->
        {:error, :bad_daemon_ref}
    end
  end

  def daemon_info(daemonRef, key) when is_atom(key) do
    case daemon_info(daemonRef, [key]) do
      [{^key, val}] ->
        {key, val}

      other ->
        other
    end
  end

  def daemon_info(daemonRef, keys) do
    case daemon_info(daemonRef) do
      {:ok, kVs} ->
        for key <- keys, :lists.keymember(key, 1, kVs) do
          {key, :proplists.get_value(key, kVs)}
        end

      _ ->
        []
    end
  end

  def stop_listener(sysSup) do
    :ssh_system_sup.stop_listener(sysSup)
  end

  def stop_listener(address, port) do
    stop_listener(address, port, :default)
  end

  def stop_listener(:any, port, profile) do
    map_ip(
      fn iP ->
        :ssh_system_sup.stop_listener(iP, port, profile)
      end,
      [{0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0}]
    )
  end

  def stop_listener(address, port, profile) do
    map_ip(
      fn iP ->
        :ssh_system_sup.stop_listener(iP, port, profile)
      end,
      {:address, address}
    )
  end

  def stop_daemon(sysSup) do
    :ssh_system_sup.stop_system(:server, sysSup)
  end

  def stop_daemon(address, port) do
    stop_daemon(address, port, :default)
  end

  def stop_daemon(:any, port, profile) do
    map_ip(
      fn iP ->
        :ssh_system_sup.stop_system(:server, iP, port, profile)
      end,
      [{0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0}]
    )
  end

  def stop_daemon(address, port, profile) do
    map_ip(
      fn iP ->
        :ssh_system_sup.stop_system(:server, iP, port, profile)
      end,
      {:address, address}
    )
  end

  def shell(socket) when is_port(socket) do
    shell(socket, [])
  end

  def shell(connectionRef) when is_pid(connectionRef) do
    case :ssh_connection.session_channel(
           connectionRef,
           :infinity
         ) do
      {:ok, channelId} ->
        :success =
          :ssh_connection.ptty_alloc(
            connectionRef,
            channelId,
            [{:pty_opts, [{:echo, 0}]}]
          )

        :success =
          :ssh_connection.send_environment_vars(connectionRef, channelId, ['LANG', 'LC_ALL'])

        args = [
          {:channel_cb, :ssh_shell},
          {:init_args, [connectionRef, channelId]},
          {:cm, connectionRef},
          {:channel_id, channelId}
        ]

        {:ok, state} = :ssh_client_channel.init([args])

        try do
          :ssh_client_channel.enter_loop(state)
        catch
          :exit, :normal ->
            :ok
        end

      error ->
        error
    end
  end

  def shell(host) do
    shell(host, 22, [])
  end

  def shell(socket, options) when is_port(socket) do
    case connect(socket, options) do
      {:ok, connectionRef} ->
        shell(connectionRef)
        close(connectionRef)

      error ->
        error
    end
  end

  def shell(host, options) do
    shell(host, 22, options)
  end

  def shell(host, port, options) do
    case connect(host, port, options) do
      {:ok, connectionRef} ->
        shell(connectionRef)
        close(connectionRef)

      error ->
        error
    end
  end

  def default_algorithms() do
    :ssh_transport.default_algorithms()
  end

  def chk_algos_opts(opts) do
    case :lists.foldl(
           fn
             {:preferred_algorithms, _}, acc ->
               acc

             {:modify_algorithms, _}, acc ->
               acc

             kV, acc ->
               [kV | acc]
           end,
           [],
           opts
         ) do
      [] ->
        case :ssh_options.handle_options(:client, opts) do
          m when is_map(m) ->
            :maps.get(:preferred_algorithms, m)

          others ->
            others
        end

      otherOps ->
        {:error, {:non_algo_opts_found, otherOps}}
    end
  end

  def set_sock_opts(connectionRef, socketOptions) do
    :ssh_connection_handler.set_sock_opts(
      connectionRef,
      socketOptions
    )
  end

  def get_sock_opts(connectionRef, socketGetOptions) do
    :ssh_connection_handler.get_sock_opts(
      connectionRef,
      socketGetOptions
    )
  end

  def tcpip_tunnel_to_server(
        connectionHandler,
        listenHost,
        listenPort,
        connectToHost,
        connectToPort
      ) do
    tcpip_tunnel_to_server(
      connectionHandler,
      listenHost,
      listenPort,
      connectToHost,
      connectToPort,
      :infinity
    )
  end

  def tcpip_tunnel_to_server(
        connectionHandler,
        listenHost,
        listenPort,
        connectToHost0,
        connectToPort,
        timeout
      ) do
    sockOpts = []

    try do
      :erlang.list_to_binary(
        case mangle_connect_address(
               connectToHost0,
               sockOpts
             ) do
          iP when is_tuple(iP) ->
            :inet_parse.ntoa(iP)

          _ when is_list(connectToHost0) ->
            connectToHost0
        end
      )
    catch
      _, _ ->
        {:error, :bad_connect_to_address}
    else
      connectToHost ->
        :ssh_connection_handler.handle_direct_tcpip(
          connectionHandler,
          mangle_tunnel_address(listenHost),
          listenPort,
          connectToHost,
          connectToPort,
          timeout
        )
    end
  end

  def tcpip_tunnel_from_server(
        connectionRef,
        listenHost,
        listenPort,
        connectToHost,
        connectToPort
      ) do
    tcpip_tunnel_from_server(
      connectionRef,
      listenHost,
      listenPort,
      connectToHost,
      connectToPort,
      :infinity
    )
  end

  def tcpip_tunnel_from_server(
        connectionRef,
        listenHost0,
        listenPort,
        connectToHost0,
        connectToPort,
        timeout
      ) do
    sockOpts = []
    listenHost = mangle_tunnel_address(listenHost0)

    connectToHost =
      mangle_connect_address(
        connectToHost0,
        sockOpts
      )

    case :ssh_connection_handler.global_request(
           connectionRef,
           'tcpip-forward',
           true,
           {listenHost, listenPort, connectToHost, connectToPort},
           timeout
         ) do
      {:success, <<>>} ->
        {:ok, listenPort}

      {:success, <<truePort::size(32)-unsigned-integer>>}
      when listenPort == 0 ->
        {:ok, truePort}

      {:success, _} = res ->
        {:error, {:bad_result, res}}

      {:failure, <<>>} ->
        {:error, :not_accepted}

      {:failure, error} ->
        {:error, error}

      other ->
        other
    end
  end

  defp handle_daemon_args(:any, opts) do
    case :proplists.get_value(:ip, opts) do
      :undefined ->
        {:any, opts}

      iP ->
        {iP, opts}
    end
  end

  defp handle_daemon_args(iPaddr, opts)
       when is_tuple(iPaddr) or
              iPaddr == :loopback do
    case :proplists.get_value(:ip, opts) do
      :undefined ->
        {iPaddr, [{:ip, iPaddr} | opts]}

      ^iPaddr ->
        {iPaddr, opts}

      iP ->
        {iPaddr, [{:ip, iPaddr} | opts -- [{:ip, iP}]]}
    end
  end

  defp valid_socket_to_use(socket, {:tcp, _, _}) do
    try do
      {is_tcp_socket(socket),
       {:ok, [{:active, false}]} ==
         :inet.getopts(
           socket,
           [:active]
         )}
    catch
      _, _ ->
        {:error, :bad_socket}
    else
      {true, true} ->
        :ok

      {true, false} ->
        {:error, :not_passive_mode}

      _ ->
        {:error, :not_tcp_socket}
    end
  end

  defp valid_socket_to_use(_, {l4, _, _}) do
    {:error, {:unsupported, l4}}
  end

  defp is_tcp_socket(socket) do
    case :inet.getopts(socket, [:delay_send]) do
      {:ok, [_]} ->
        true

      _ ->
        false
    end
  end

  defp open_listen_socket(_Host0, port0, options0) do
    {:ok, lSock} =
      case :ssh_options.get_value(:socket_options, :fd, options0, :ssh, 760) do
        :undefined ->
          :ssh_acceptor.listen(port0, options0)

        fd when is_integer(fd) ->
          :ssh_acceptor.listen(0, options0)
      end

    {:ok, {lHost, lPort}} = :inet.sockname(lSock)
    {{lHost, lPort}, lSock}
  end

  defp close_listen_socket(listenSocket, options) do
    try do
      {_, callback, _} = :ssh_options.get_value(:user_options, :transport, options, :ssh, 773)
      callback.close(listenSocket)
    catch
      _C, _E ->
        :ok
    end
  end

  defp finalize_start(host, port, profile, options0, f) do
    try do
      :ssh_connection_handler.available_hkey_algorithms(
        :server,
        options0
      )

      :sshd_sup.start_child(host, port, profile, options0)
    catch
      :error, {:shutdown, err} ->
        {:error, err}

      :exit, {:noproc, _} ->
        {:error, :ssh_not_started}
    else
      {:error, {:already_started, _}} ->
        {:error, :eaddrinuse}

      {:error, error} ->
        {:error, error}

      result = {:ok, _} ->
        f.(options0, result)
    end
  end

  defp map_ip(fun, {:address, iP}) when is_tuple(iP) do
    fun.(iP)
  end

  defp map_ip(fun, {:address, address}) do
    iPs =
      try do
        {:ok, r_hostent(h_addr_list: iP0s)} = :inet.gethostbyname(address)
        iP0s
      catch
        _, _ ->
          []
      end

    map_ip(fun, iPs)
  end

  defp map_ip(fun, iPs) do
    :lists.map(fun, iPs)
  end

  defp mangle_connect_address(a, sockOpts) do
    mangle_connect_address1(
      a,
      :proplists.get_value(:inet6, sockOpts, false)
    )
  end

  defp loopback(true) do
    {0, 0, 0, 0, 0, 0, 0, 1}
  end

  defp loopback(false) do
    {127, 0, 0, 1}
  end

  defp mangle_connect_address1(:loopback, v6flg) do
    loopback(v6flg)
  end

  defp mangle_connect_address1(:any, v6flg) do
    loopback(v6flg)
  end

  defp mangle_connect_address1({0, 0, 0, 0}, _) do
    loopback(false)
  end

  defp mangle_connect_address1({0, 0, 0, 0, 0, 0, 0, 0}, _) do
    loopback(true)
  end

  defp mangle_connect_address1(iP, _) when is_tuple(iP) do
    iP
  end

  defp mangle_connect_address1(a, _) do
    case (try do
            :inet.parse_address(a)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, {0, 0, 0, 0}} ->
        loopback(false)

      {:ok, {0, 0, 0, 0, 0, 0, 0, 0}} ->
        loopback(true)

      _ ->
        a
    end
  end

  defp mangle_tunnel_address(:any) do
    ""
  end

  defp mangle_tunnel_address(:loopback) do
    "localhost"
  end

  defp mangle_tunnel_address({0, 0, 0, 0}) do
    ""
  end

  defp mangle_tunnel_address({0, 0, 0, 0, 0, 0, 0, 0}) do
    ""
  end

  defp mangle_tunnel_address(iP) when is_tuple(iP) do
    :erlang.list_to_binary(:inet_parse.ntoa(iP))
  end

  defp mangle_tunnel_address(a) when is_atom(a) do
    mangle_tunnel_address(:erlang.atom_to_list(a))
  end

  defp mangle_tunnel_address(x) when is_list(x) do
    case (try do
            :inet.parse_address(x)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, {0, 0, 0, 0}} ->
        ""

      {:ok, {0, 0, 0, 0, 0, 0, 0, 0}} ->
        ""

      _ ->
        :erlang.list_to_binary(x)
    end
  end
end
