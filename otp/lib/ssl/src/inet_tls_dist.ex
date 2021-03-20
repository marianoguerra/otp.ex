defmodule :m_inet_tls_dist do
  use Bitwise
  require Record

  Record.defrecord(:r_net_address, :net_address,
    address: :undefined,
    host: :undefined,
    protocol: :undefined,
    family: :undefined
  )

  Record.defrecord(:r_hs_data, :hs_data,
    kernel_pid: :undefined,
    other_node: :undefined,
    this_node: :undefined,
    socket: :undefined,
    timer: :undefined,
    this_flags: :undefined,
    allowed: :undefined,
    other_version: :undefined,
    other_flags: :undefined,
    other_started: :undefined,
    f_send: :undefined,
    f_recv: :undefined,
    f_setopts_pre_nodeup: :undefined,
    f_setopts_post_nodeup: :undefined,
    f_getll: :undefined,
    f_address: :undefined,
    mf_tick: :undefined,
    mf_getstat: :undefined,
    request_type: :normal,
    mf_setopts: :undefined,
    mf_getopts: :undefined,
    f_handshake_complete: :undefined,
    add_flags: :undefined,
    reject_flags: :undefined,
    require_flags: :undefined,
    this_creation: :undefined,
    other_creation: :undefined
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
  Record.defrecord(:r_sslsocket, :sslsocket, fd: nil, pid: nil)

  def childspecs() do
    {:ok,
     [
       {:ssl_dist_sup, {:ssl_dist_sup, :start_link, []}, :permanent, :infinity, :supervisor,
        [:ssl_dist_sup]}
     ]}
  end

  def select(node) do
    gen_select(:inet_tcp, node)
  end

  def gen_select(driver, node) do
    case :dist_util.split_node(node) do
      {:node, _, host} ->
        case driver.getaddr(host) do
          {:ok, _} ->
            true

          _ ->
            false
        end

      _ ->
        false
    end
  end

  def address() do
    gen_address(:inet_tcp)
  end

  def gen_address(driver) do
    :inet_tcp_dist.gen_address(driver)
  end

  def is_node_name(node) do
    :dist_util.is_node_name(node)
  end

  defp hs_data_common(r_sslsocket(pid: [_, distCtrl | _]) = sslSocket) do
    r_hs_data(
      f_send: fn _Ctrl, packet ->
        f_send(sslSocket, packet)
      end,
      f_recv: fn _, length, timeout ->
        f_recv(sslSocket, length, timeout)
      end,
      f_setopts_pre_nodeup: fn ctrl when ctrl == distCtrl ->
        f_setopts_pre_nodeup(sslSocket)
      end,
      f_setopts_post_nodeup: fn ctrl when ctrl == distCtrl ->
        f_setopts_post_nodeup(sslSocket)
      end,
      f_getll: fn ctrl when ctrl == distCtrl ->
        f_getll(distCtrl)
      end,
      f_address: fn ctrl, node when ctrl == distCtrl ->
        f_address(sslSocket, node)
      end,
      mf_tick: fn ctrl when ctrl == distCtrl ->
        mf_tick(distCtrl)
      end,
      mf_getstat: fn ctrl when ctrl == distCtrl ->
        mf_getstat(sslSocket)
      end,
      mf_setopts: fn ctrl, opts when ctrl == distCtrl ->
        mf_setopts(sslSocket, opts)
      end,
      mf_getopts: fn ctrl, opts when ctrl == distCtrl ->
        mf_getopts(sslSocket, opts)
      end,
      f_handshake_complete: fn ctrl, node, dHandle
                               when ctrl == distCtrl ->
        f_handshake_complete(distCtrl, node, dHandle)
      end
    )
  end

  defp f_send(sslSocket, packet) do
    :ssl.send(sslSocket, packet)
  end

  defp f_recv(sslSocket, length, timeout) do
    case :ssl.recv(sslSocket, length, timeout) do
      {:ok, bin} when is_binary(bin) ->
        {:ok, :erlang.binary_to_list(bin)}

      other ->
        other
    end
  end

  defp f_setopts_pre_nodeup(_SslSocket) do
    :ok
  end

  defp f_setopts_post_nodeup(sslSocket) do
    :ssl.setopts(sslSocket, [nodelay()])
  end

  defp f_getll(distCtrl) do
    {:ok, distCtrl}
  end

  defp f_address(sslSocket, node) do
    case :ssl.peername(sslSocket) do
      {:ok, address} ->
        case :dist_util.split_node(node) do
          {:node, _, host} ->
            r_net_address(address: address, host: host, protocol: :tls, family: :inet)

          _ ->
            {:error, :no_node}
        end
    end
  end

  defp mf_tick(distCtrl) do
    send(distCtrl, :tick)
    :ok
  end

  defp mf_getstat(sslSocket) do
    case :ssl.getstat(
           sslSocket,
           [:recv_cnt, :send_cnt, :send_pend]
         ) do
      {:ok, stat} ->
        split_stat(stat, 0, 0, 0)

      error ->
        error
    end
  end

  defp mf_setopts(sslSocket, opts) do
    case setopts_filter(opts) do
      [] ->
        :ssl.setopts(sslSocket, opts)

      opts1 ->
        {:error, {:badopts, opts1}}
    end
  end

  defp mf_getopts(sslSocket, opts) do
    :ssl.getopts(sslSocket, opts)
  end

  defp f_handshake_complete(distCtrl, node, dHandle) do
    :tls_sender.dist_handshake_complete(distCtrl, node, dHandle)
  end

  defp setopts_filter(opts) do
    for {k, _} = opt <- opts,
        k === :active or k === :deliver or k === :packet do
      opt
    end
  end

  defp split_stat([{:recv_cnt, r} | stat], _, w, p) do
    split_stat(stat, r, w, p)
  end

  defp split_stat([{:send_cnt, w} | stat], r, _, p) do
    split_stat(stat, r, w, p)
  end

  defp split_stat([{:send_pend, p} | stat], r, w, _) do
    split_stat(stat, r, w, p)
  end

  defp split_stat([], r, w, p) do
    {:ok, r, w, p}
  end

  def listen(name, host) do
    gen_listen(:inet_tcp, name, host)
  end

  def gen_listen(driver, name, host) do
    case :inet_tcp_dist.gen_listen(driver, name, host) do
      {:ok, {socket, address, creation}} ->
        :inet.setopts(socket, [{:packet, 4}, {:nodelay, true}])
        {:ok, {socket, r_net_address(address, protocol: :tls), creation}}

      other ->
        other
    end
  end

  def accept(listen) do
    gen_accept(:inet_tcp, listen)
  end

  def gen_accept(driver, listen) do
    kernel = self()

    monitor_pid(
      :erlang.spawn_opt(
        fn ->
          accept_loop(driver, listen, kernel)
        end,
        [:link, {:priority, :max}]
      )
    )
  end

  defp accept_loop(driver, listen, kernel) do
    case driver.accept(listen) do
      {:ok, socket} ->
        case check_ip(driver, socket) do
          true ->
            accept_loop(driver, listen, kernel, socket)

          {false, iP} ->
            case :logger.allow(:error, :inet_tls_dist) do
              true ->
                apply(:logger, :macro_log, [
                  %{
                    mfa: {:inet_tls_dist, :accept_loop, 3},
                    line: 237,
                    file: 'otp/lib/ssl/src/inet_tls_dist.erl'
                  },
                  :error,
                  '** Connection attempt from disallowed IP ~w ** ~n',
                  [iP]
                ])

              false ->
                :ok
            end

            :dist_util.shutdown(:inet_tls_dist, 240, :no_node, trace({:disallowed, iP}))
        end

      error ->
        exit(trace(error))
    end
  end

  defp accept_loop(driver, listen, kernel, socket) do
    opts =
      setup_verify_client(
        socket,
        get_ssl_options(:server)
      )

    wait_for_code_server()

    case :ssl.handshake(
           socket,
           trace([{:active, false}, {:packet, 4} | opts]),
           :net_kernel.connecttime()
         ) do
      {:ok, r_sslsocket(pid: [_, distCtrl | _]) = sslSocket} ->
        trace(send(kernel, {:accept, self(), distCtrl, driver.family(), :tls}))

        receive do
          {^kernel, :controller, pid} ->
            :ok = :ssl.controlling_process(sslSocket, pid)
            trace(send(pid, {self(), :controller}))

          {^kernel, :unsupported_protocol} ->
            exit(trace(:unsupported_protocol))
        end

        accept_loop(driver, listen, kernel)

      {:error, {:options, _}} = error ->
        case :logger.allow(:error, :inet_tls_dist) do
          true ->
            apply(:logger, :macro_log, [
              %{
                mfa: {:inet_tls_dist, :accept_loop, 4},
                line: 272,
                file: 'otp/lib/ssl/src/inet_tls_dist.erl'
              },
              :error,
              'Cannot accept TLS distribution connection: ~s~n',
              [:ssl.format_error(error)]
            ])

          false ->
            :ok
        end

        :gen_tcp.close(socket)
        exit(trace(error))

      other ->
        :gen_tcp.close(socket)
        exit(trace(other))
    end
  end

  defp setup_verify_client(socket, opts) do
    setup_verify_client(socket, opts, true, [])
  end

  defp setup_verify_client(_Socket, [], _, optsR) do
    :lists.reverse(optsR)
  end

  defp setup_verify_client(socket, [opt | opts], first, optsR) do
    case opt do
      {:verify_fun, {fun, _}} ->
        case fun === (&:inet_tls_dist.verify_client/3) do
          true ->
            cond do
              first ->
                case :inet.peername(socket) do
                  {:ok, {peerIP, _Port}} ->
                    {:ok, allowed} = :net_kernel.allowed()
                    allowedHosts = allowed_hosts(allowed)

                    setup_verify_client(socket, opts, false, [
                      {:verify_fun, {fun, {allowedHosts, peerIP}}}
                      | optsR
                    ])

                  {:error, reason} ->
                    exit(trace({:no_peername, reason}))
                end

              true ->
                setup_verify_client(socket, opts, first, optsR)
            end

          false ->
            setup_verify_client(socket, opts, first, [opt | optsR])
        end

      _ ->
        setup_verify_client(socket, opts, first, [opt | optsR])
    end
  end

  defp allowed_hosts(allowed) do
    :lists.usort(allowed_node_hosts(allowed))
  end

  defp allowed_node_hosts([]) do
    []
  end

  defp allowed_node_hosts([node | allowed]) do
    case :dist_util.split_node(node) do
      {:node, _, host} ->
        [host | allowed_node_hosts(allowed)]

      {:host, host} ->
        [host | allowed_node_hosts(allowed)]

      _ ->
        allowed_node_hosts(allowed)
    end
  end

  def verify_client(_, {:bad_cert, _} = reason, _) do
    {:fail, reason}
  end

  def verify_client(_, {:extension, _}, s) do
    {:unknown, s}
  end

  def verify_client(_, :valid, s) do
    {:valid, s}
  end

  def verify_client(_, :valid_peer, {[], _} = s) do
    {:valid, s}
  end

  def verify_client(peerCert, :valid_peer, {allowedHosts, peerIP} = s) do
    case :public_key.pkix_verify_hostname(
           peerCert,
           [
             {:ip, peerIP}
             | for host <- allowedHosts do
                 {:dns_id, host}
               end
           ]
         ) do
      true ->
        {:valid, s}

      false ->
        {:fail, :cert_no_hostname_nor_ip_match}
    end
  end

  defp wait_for_code_server() do
    case :erlang.whereis(:code_server) do
      :undefined ->
        :timer.sleep(10)
        wait_for_code_server()

      pid when is_pid(pid) ->
        :ok
    end
  end

  def accept_connection(acceptPid, distCtrl, myNode, allowed, setupTime) do
    gen_accept_connection(:inet_tcp, acceptPid, distCtrl, myNode, allowed, setupTime)
  end

  def gen_accept_connection(driver, acceptPid, distCtrl, myNode, allowed, setupTime) do
    kernel = self()

    monitor_pid(
      :erlang.spawn_opt(
        fn ->
          do_accept(driver, acceptPid, distCtrl, myNode, allowed, setupTime, kernel)
        end,
        [:link, {:priority, :max}]
      )
    )
  end

  defp do_accept(_Driver, acceptPid, distCtrl, myNode, allowed, setupTime, kernel) do
    {:ok, sslSocket} = :tls_sender.dist_tls_socket(distCtrl)

    receive do
      {^acceptPid, :controller} ->
        timer = :dist_util.start_timer(setupTime)
        newAllowed = allowed_nodes(sslSocket, allowed)
        hSData0 = hs_data_common(sslSocket)

        hSData =
          r_hs_data(hSData0,
            kernel_pid: kernel,
            this_node: myNode,
            socket: distCtrl,
            timer: timer,
            this_flags: 0,
            allowed: newAllowed
          )

        :erlang.link(distCtrl)
        :dist_util.handshake_other_started(trace(hSData))
    end
  end

  defp allowed_nodes(_SslSocket, []) do
    []
  end

  defp allowed_nodes(sslSocket, allowed) do
    case :ssl.peercert(sslSocket) do
      {:ok, peerCertDER} ->
        case :ssl.peername(sslSocket) do
          {:ok, {peerIP, _Port}} ->
            peerCert =
              :public_key.pkix_decode_cert(
                peerCertDER,
                :otp
              )

            case allowed_nodes(peerCert, allowed_hosts(allowed), peerIP) do
              [] ->
                case :logger.allow(:error, :inet_tls_dist) do
                  true ->
                    apply(:logger, :macro_log, [
                      %{
                        mfa: {:inet_tls_dist, :allowed_nodes, 2},
                        line: 448,
                        file: 'otp/lib/ssl/src/inet_tls_dist.erl'
                      },
                      :error,
                      '** Connection attempt from disallowed node(s) ~p ** ~n',
                      [peerIP]
                    ])

                  false ->
                    :ok
                end

                :dist_util.shutdown(
                  :inet_tls_dist,
                  451,
                  peerIP,
                  trace({:is_allowed, :not_allowed})
                )

              allowedNodes ->
                allowedNodes
            end

          error1 ->
            :dist_util.shutdown(:inet_tls_dist, 457, :no_peer_ip, trace(error1))
        end

      {:error, :no_peercert} ->
        allowed

      error2 ->
        :dist_util.shutdown(:inet_tls_dist, 462, :no_peer_cert, trace(error2))
    end
  end

  defp allowed_nodes(peerCert, [], peerIP) do
    case :public_key.pkix_verify_hostname(
           peerCert,
           [{:ip, peerIP}]
         ) do
      true ->
        host = :inet.ntoa(peerIP)
        true = is_list(host)
        [host]

      false ->
        []
    end
  end

  defp allowed_nodes(peerCert, [node | allowed], peerIP) do
    case :dist_util.split_node(node) do
      {:node, _, host} ->
        allowed_nodes(peerCert, allowed, peerIP, node, host)

      {:host, host} ->
        allowed_nodes(peerCert, allowed, peerIP, node, host)

      _ ->
        allowed_nodes(peerCert, allowed, peerIP)
    end
  end

  defp allowed_nodes(peerCert, allowed, peerIP, node, host) do
    case :public_key.pkix_verify_hostname(
           peerCert,
           [{:dns_id, host}]
         ) do
      true ->
        [node | allowed_nodes(peerCert, allowed, peerIP)]

      false ->
        allowed_nodes(peerCert, allowed, peerIP)
    end
  end

  def setup(node, type, myNode, longOrShortNames, setupTime) do
    gen_setup(:inet_tcp, node, type, myNode, longOrShortNames, setupTime)
  end

  def gen_setup(driver, node, type, myNode, longOrShortNames, setupTime) do
    kernel = self()

    monitor_pid(
      :erlang.spawn_opt(
        setup_fun(driver, kernel, node, type, myNode, longOrShortNames, setupTime),
        [:link, {:priority, :max}]
      )
    )
  end

  defp setup_fun(driver, kernel, node, type, myNode, longOrShortNames, setupTime) do
    fn ->
      do_setup(driver, kernel, node, type, myNode, longOrShortNames, setupTime)
    end
  end

  defp do_setup(driver, kernel, node, type, myNode, longOrShortNames, setupTime) do
    {name, address} = split_node(driver, node, longOrShortNames)
    erlEpmd = :net_kernel.epmd_module()
    {aRMod, aRFun} = get_address_resolver(erlEpmd, driver)
    timer = trace(:dist_util.start_timer(setupTime))

    case apply(aRMod, aRFun, [name, address, driver.family()]) do
      {:ok, ip, tcpPort, version} ->
        do_setup_connect(driver, kernel, node, address, ip, tcpPort, version, type, myNode, timer)

      {:ok, ip} ->
        case erlEpmd.port_please(name, ip) do
          {:port, tcpPort, version} ->
            do_setup_connect(
              driver,
              kernel,
              node,
              address,
              ip,
              tcpPort,
              version,
              type,
              myNode,
              timer
            )

          other ->
            :dist_util.shutdown(
              :inet_tls_dist,
              524,
              node,
              trace({:port_please_failed, erlEpmd, name, ip, other})
            )
        end

      other ->
        :dist_util.shutdown(
          :inet_tls_dist,
          530,
          node,
          trace({:getaddr_failed, driver, address, other})
        )
    end
  end

  defp do_setup_connect(driver, kernel, node, address, ip, tcpPort, version, type, myNode, timer) do
    opts = trace(connect_options(get_ssl_options(:client)))
    :dist_util.reset_timer(timer)

    case :ssl.connect(
           address,
           tcpPort,
           [:binary, {:active, false}, {:packet, 4}, driver.family(), {:nodelay, true}] ++ opts,
           :net_kernel.connecttime()
         ) do
      {:ok, r_sslsocket(pid: [_, distCtrl | _]) = sslSocket} ->
        _ = monitor_pid(distCtrl)
        :ok = :ssl.controlling_process(sslSocket, self())
        hSData0 = hs_data_common(sslSocket)

        hSData =
          r_hs_data(hSData0,
            kernel_pid: kernel,
            other_node: node,
            this_node: myNode,
            socket: distCtrl,
            timer: timer,
            this_flags: 0,
            other_version: version,
            request_type: type
          )

        :erlang.link(distCtrl)
        :dist_util.handshake_we_started(trace(hSData))

      other ->
        :dist_util.shutdown(
          :inet_tls_dist,
          564,
          node,
          trace({:ssl_connect_failed, ip, tcpPort, other})
        )
    end
  end

  def close(socket) do
    gen_close(:inet, socket)
  end

  def gen_close(driver, socket) do
    trace(driver.close(socket))
  end

  defp get_address_resolver(epmdModule, _Driver) do
    case :erlang.function_exported(epmdModule, :address_please, 3) do
      true ->
        {epmdModule, :address_please}

      _ ->
        {:erl_epmd, :address_please}
    end
  end

  defp check_ip(driver, socket) do
    case :application.get_env(:check_ip) do
      {:ok, true} ->
        case get_ifs(socket) do
          {:ok, iFs, iP} ->
            check_ip(driver, iFs, iP)

          other ->
            :dist_util.shutdown(
              :inet_tls_dist,
              598,
              :no_node,
              trace({:check_ip_failed, socket, other})
            )
        end

      _ ->
        true
    end
  end

  defp check_ip(driver, [{ownIP, _, netmask} | iFs], peerIP) do
    case {driver.mask(netmask, peerIP), driver.mask(netmask, ownIP)} do
      {m, m} ->
        true

      _ ->
        check_ip(iFs, peerIP)
    end
  end

  defp check_ip(_Driver, [], peerIP) do
    {false, peerIP}
  end

  defp get_ifs(socket) do
    case :inet.peername(socket) do
      {:ok, {iP, _}} ->
        case :inet.getif(socket) do
          {:ok, iFs} ->
            {:ok, iFs, iP}

          error ->
            error
        end

      error ->
        error
    end
  end

  def cert_nodes(r_OTPCertificate(tbsCertificate: r_OTPTBSCertificate(extensions: extensions))) do
    parse_extensions(extensions)
  end

  defp parse_extensions(extensions) when is_list(extensions) do
    parse_extensions(extensions, [], [])
  end

  defp parse_extensions(:asn1_NOVALUE) do
    :undefined
  end

  defp parse_extensions([], [], []) do
    :undefined
  end

  defp parse_extensions([], hosts, []) do
    :lists.reverse(hosts)
  end

  defp parse_extensions([], [], names) do
    for name <- :lists.reverse(names) do
      name ++ '@'
    end
  end

  defp parse_extensions([], hosts, names) do
    for host <- :lists.reverse(hosts),
        name <- :lists.reverse(names) do
      name ++ '@' ++ host
    end
  end

  defp parse_extensions(
         [
           r_Extension(
             extnID: {2, 5, 29, 17},
             extnValue: altNames
           )
           | extensions
         ],
         hosts,
         names
       ) do
    case parse_subject_altname(altNames) do
      :none ->
        parse_extensions(extensions, hosts, names)

      {:host, host} ->
        parse_extensions(extensions, [host | hosts], names)

      {:name, name} ->
        parse_extensions(extensions, hosts, [name | names])
    end
  end

  defp parse_extensions([_ | extensions], hosts, names) do
    parse_extensions(extensions, hosts, names)
  end

  defp parse_subject_altname([]) do
    :none
  end

  defp parse_subject_altname([{:dNSName, host} | _AltNames]) do
    {:host, host}
  end

  defp parse_subject_altname([
         {:directoryName, {:rdnSequence, [rdn | _]}}
         | altNames
       ]) do
    case parse_rdn(rdn) do
      :none ->
        parse_subject_altname(altNames)

      name ->
        {:name, name}
    end
  end

  defp parse_subject_altname([_ | altNames]) do
    parse_subject_altname(altNames)
  end

  defp parse_rdn([]) do
    :none
  end

  defp parse_rdn([
         r_AttributeTypeAndValue(
           type: {2, 5, 4, 3},
           value: {:utf8String, commonName}
         )
         | _
       ]) do
    :unicode.characters_to_list(commonName)
  end

  defp parse_rdn([_ | rdn]) do
    parse_rdn(rdn)
  end

  defp split_node(driver, node, longOrShortNames) do
    case :dist_util.split_node(node) do
      {:node, name, host} ->
        check_node(driver, node, name, host, longOrShortNames)

      {:host, _} ->
        case :logger.allow(:error, :inet_tls_dist) do
          true ->
            apply(:logger, :macro_log, [
              %{
                mfa: {:inet_tls_dist, :split_node, 3},
                line: 707,
                file: 'otp/lib/ssl/src/inet_tls_dist.erl'
              },
              :error,
              '** Nodename ~p illegal, no \'@\' character **~n',
              [node]
            ])

          false ->
            :ok
        end

        :dist_util.shutdown(:inet_tls_dist, 710, node, trace({:illegal_node_n@me, node}))

      _ ->
        case :logger.allow(:error, :inet_tls_dist) do
          true ->
            apply(:logger, :macro_log, [
              %{
                mfa: {:inet_tls_dist, :split_node, 3},
                line: 712,
                file: 'otp/lib/ssl/src/inet_tls_dist.erl'
              },
              :error,
              '** Nodename ~p illegal **~n',
              [node]
            ])

          false ->
            :ok
        end

        :dist_util.shutdown(:inet_tls_dist, 714, node, trace({:illegal_node_name, node}))
    end
  end

  defp check_node(driver, node, name, host, longOrShortNames) do
    case :string.split(host, '.', :all) do
      [_] when longOrShortNames === :longnames ->
        case driver.parse_address(host) do
          {:ok, _} ->
            {name, host}

          _ ->
            case :logger.allow(:error, :inet_tls_dist) do
              true ->
                apply(:logger, :macro_log, [
                  %{
                    mfa: {:inet_tls_dist, :check_node, 5},
                    line: 724,
                    file: 'otp/lib/ssl/src/inet_tls_dist.erl'
                  },
                  :error,
                  '** System running to use fully qualified hostnames **~n** Hostname ~s is illegal **~n',
                  [host]
                ])

              false ->
                :ok
            end

            :dist_util.shutdown(:inet_tls_dist, 729, node, trace({:not_longnames, host}))
        end

      [_, _ | _] when longOrShortNames === :shortnames ->
        case :logger.allow(:error, :inet_tls_dist) do
          true ->
            apply(:logger, :macro_log, [
              %{
                mfa: {:inet_tls_dist, :check_node, 5},
                line: 732,
                file: 'otp/lib/ssl/src/inet_tls_dist.erl'
              },
              :error,
              '** System NOT running to use fully qualified hostnames **~n** Hostname ~s is illegal **~n',
              [host]
            ])

          false ->
            :ok
        end

        :dist_util.shutdown(:inet_tls_dist, 737, node, trace({:not_shortnames, host}))

      _ ->
        {name, host}
    end
  end

  defp connect_options(opts) do
    case :application.get_env(
           :kernel,
           :inet_dist_connect_options
         ) do
      {:ok, connectOpts} ->
        :lists.ukeysort(1, connectOpts ++ opts)

      _ ->
        opts
    end
  end

  def nodelay() do
    case :application.get_env(:kernel, :dist_nodelay) do
      :undefined ->
        {:nodelay, true}

      {:ok, true} ->
        {:nodelay, true}

      {:ok, false} ->
        {:nodelay, false}

      _ ->
        {:nodelay, true}
    end
  end

  defp get_ssl_options(type) do
    try do
      :ets.lookup(:ssl_dist_opts, type)
    catch
      :error, :badarg ->
        get_ssl_dist_arguments(type)
    else
      [{^type, opts}] ->
        [{:erl_dist, true}, {:versions, [:"tlsv1.2"]} | opts]

      _ ->
        get_ssl_dist_arguments(type)
    end
  end

  defp get_ssl_dist_arguments(type) do
    case :init.get_argument(:ssl_dist_opt) do
      {:ok, args} ->
        [
          {:erl_dist, true},
          {:versions, [:"tlsv1.2"]}
          | ssl_options(type, :lists.append(args))
        ]

      _ ->
        [{:erl_dist, true}, {:versions, [:"tlsv1.2"]}]
    end
  end

  defp ssl_options(_Type, []) do
    []
  end

  defp ssl_options(:client, ['client_' ++ opt, value | t] = opts) do
    ssl_options(:client, t, opts, opt, value)
  end

  defp ssl_options(:server, ['server_' ++ opt, value | t] = opts) do
    ssl_options(:server, t, opts, opt, value)
  end

  defp ssl_options(type, [_Opt, _Value | t]) do
    ssl_options(type, t)
  end

  defp ssl_options(type, t, opts, opt, value) do
    case ssl_option(type, opt) do
      :error ->
        :erlang.error(:malformed_ssl_dist_opt, [type, opts])

      fun ->
        [
          {:erlang.list_to_atom(opt), fun.(value)}
          | ssl_options(type, t)
        ]
    end
  end

  defp ssl_option(:server, opt) do
    case opt do
      'dhfile' ->
        &listify/1

      'fail_if_no_peer_cert' ->
        &atomize/1

      _ ->
        ssl_option(:client, opt)
    end
  end

  defp ssl_option(:client, opt) do
    case opt do
      'certfile' ->
        &listify/1

      'cacertfile' ->
        &listify/1

      'keyfile' ->
        &listify/1

      'password' ->
        &listify/1

      'verify' ->
        &atomize/1

      'verify_fun' ->
        &verify_fun/1

      'crl_check' ->
        &atomize/1

      'crl_cache' ->
        &termify/1

      'reuse_sessions' ->
        &atomize/1

      'secure_renegotiate' ->
        &atomize/1

      'depth' ->
        &:erlang.list_to_integer/1

      'hibernate_after' ->
        &:erlang.list_to_integer/1

      'ciphers' ->
        &listify/1

      _ ->
        :error
    end
  end

  defp listify(list) when is_list(list) do
    list
  end

  defp atomize(list) when is_list(list) do
    :erlang.list_to_atom(list)
  end

  defp atomize(atom) when is_atom(atom) do
    atom
  end

  defp termify(string) when is_list(string) do
    {:ok, tokens, _} = :erl_scan.string(string ++ '.')
    {:ok, term} = :erl_parse.parse_term(tokens)
    term
  end

  defp verify_fun(value) do
    case termify(value) do
      {mod, func, state} when is_atom(mod) and is_atom(func) ->
        fun = Function.capture(mod, func, 3)
        {fun, state}

      _ ->
        :erlang.error(:malformed_ssl_dist_opt, [value])
    end
  end

  defp trace(term) do
    term
  end

  defp monitor_pid(pid) do
    pid
  end

  def dbg() do
    :dbg.stop()
    :dbg.tracer()
    :dbg.p(:all, :c)
    :dbg.tpl(:inet_tls_dist, :cx)
    :dbg.tpl(:erlang, :dist_ctrl_get_data_notification, :cx)
    :dbg.tpl(:erlang, :dist_ctrl_get_data, :cx)
    :dbg.tpl(:erlang, :dist_ctrl_put_data, :cx)
    :ok
  end
end
