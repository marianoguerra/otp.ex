defmodule :m_dtls_record do
  use Bitwise
  require Record

  Record.defrecord(:r_security_parameters, :security_parameters,
    cipher_suite: :undefined,
    connection_end: :undefined,
    bulk_cipher_algorithm: :undefined,
    cipher_type: :undefined,
    iv_size: :undefined,
    key_size: :undefined,
    key_material_length: :undefined,
    expanded_key_material_length: :undefined,
    mac_algorithm: :undefined,
    prf_algorithm: :undefined,
    hash_size: :undefined,
    compression_algorithm: :undefined,
    master_secret: :undefined,
    resumption_master_secret: :undefined,
    application_traffic_secret: :undefined,
    client_random: :undefined,
    server_random: :undefined,
    exportable: :undefined
  )

  Record.defrecord(:r_compression_state, :compression_state,
    method: :undefined,
    state: :undefined
  )

  Record.defrecord(:r_generic_stream_cipher, :generic_stream_cipher,
    content: :undefined,
    mac: :undefined
  )

  Record.defrecord(:r_generic_block_cipher, :generic_block_cipher,
    iv: :undefined,
    content: :undefined,
    mac: :undefined,
    padding: :undefined,
    padding_length: :undefined,
    next_iv: :undefined
  )

  Record.defrecord(:r_ssl_tls, :ssl_tls,
    type: :undefined,
    version: :undefined,
    fragment: :undefined,
    epoch: :undefined,
    sequence_number: :undefined
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

  Record.defrecord(:r_socket_options, :socket_options,
    mode: :list,
    packet: 0,
    packet_size: 0,
    header: 0,
    active: true
  )

  Record.defrecord(:r_config, :config,
    ssl: :undefined,
    inet_user: :undefined,
    emulated: :undefined,
    trackers: :undefined,
    dtls_handler: :undefined,
    inet_ssl: :undefined,
    transport_info: :undefined,
    connection_cb: :undefined
  )

  Record.defrecord(:r_alert, :alert,
    level: :undefined,
    description: :undefined,
    where: :undefined,
    role: :undefined,
    reason: :undefined
  )

  Record.defrecord(:r_session, :session,
    session_id: :undefined,
    peer_certificate: :undefined,
    own_certificate: :undefined,
    compression_method: :undefined,
    cipher_suite: :undefined,
    master_secret: :undefined,
    srp_username: :undefined,
    is_resumable: :undefined,
    time_stamp: :undefined,
    ecc: :undefined,
    sign_alg: :undefined,
    dh_public_value: :undefined
  )

  Record.defrecord(:r_random, :random,
    gmt_unix_time: :undefined,
    random_bytes: :undefined
  )

  Record.defrecord(:r_hello_extensions, :hello_extensions,
    renegotiation_info: :undefined,
    signature_algs: :undefined,
    alpn: :undefined,
    next_protocol_negotiation: :undefined,
    srp: :undefined,
    ec_point_formats: :undefined,
    elliptic_curves: :undefined,
    sni: :undefined,
    client_hello_versions: :undefined,
    server_hello_selected_version: :undefined,
    signature_algs_cert: :undefined,
    key_share: :undefined
  )

  Record.defrecord(:r_server_hello, :server_hello,
    server_version: :undefined,
    random: :undefined,
    session_id: :undefined,
    cipher_suite: :undefined,
    compression_method: :undefined,
    extensions: :undefined
  )

  Record.defrecord(:r_certificate, :certificate, asn1_certificates: :undefined)

  Record.defrecord(:r_server_rsa_params, :server_rsa_params,
    rsa_modulus: :undefined,
    rsa_exponent: :undefined
  )

  Record.defrecord(:r_server_dh_params, :server_dh_params,
    dh_p: :undefined,
    dh_g: :undefined,
    dh_y: :undefined
  )

  Record.defrecord(:r_server_ecdh_params, :server_ecdh_params,
    curve: :undefined,
    public: :undefined
  )

  Record.defrecord(:r_server_psk_params, :server_psk_params, hint: :undefined)

  Record.defrecord(:r_server_dhe_psk_params, :server_dhe_psk_params,
    hint: :undefined,
    dh_params: :undefined
  )

  Record.defrecord(:r_server_ecdhe_psk_params, :server_ecdhe_psk_params,
    hint: :undefined,
    dh_params: :undefined
  )

  Record.defrecord(:r_server_srp_params, :server_srp_params,
    srp_n: :undefined,
    srp_g: :undefined,
    srp_s: :undefined,
    srp_b: :undefined
  )

  Record.defrecord(:r_server_key_exchange, :server_key_exchange, exchange_keys: :undefined)

  Record.defrecord(:r_server_key_params, :server_key_params,
    params: :undefined,
    params_bin: :undefined,
    hashsign: :undefined,
    signature: :undefined
  )

  Record.defrecord(:r_hello_request, :hello_request, [])
  Record.defrecord(:r_server_hello_done, :server_hello_done, [])

  Record.defrecord(:r_certificate_request, :certificate_request,
    certificate_types: :undefined,
    hashsign_algorithms: :undefined,
    certificate_authorities: :undefined
  )

  Record.defrecord(:r_client_key_exchange, :client_key_exchange, exchange_keys: :undefined)

  Record.defrecord(:r_pre_master_secret, :pre_master_secret,
    client_version: :undefined,
    random: :undefined
  )

  Record.defrecord(:r_encrypted_premaster_secret, :encrypted_premaster_secret,
    premaster_secret: :undefined
  )

  Record.defrecord(:r_client_diffie_hellman_public, :client_diffie_hellman_public,
    dh_public: :undefined
  )

  Record.defrecord(:r_client_ec_diffie_hellman_public, :client_ec_diffie_hellman_public,
    dh_public: :undefined
  )

  Record.defrecord(:r_client_psk_identity, :client_psk_identity, identity: :undefined)

  Record.defrecord(:r_client_dhe_psk_identity, :client_dhe_psk_identity,
    identity: :undefined,
    dh_public: :undefined
  )

  Record.defrecord(:r_client_ecdhe_psk_identity, :client_ecdhe_psk_identity,
    identity: :undefined,
    dh_public: :undefined
  )

  Record.defrecord(:r_client_rsa_psk_identity, :client_rsa_psk_identity,
    identity: :undefined,
    exchange_keys: :undefined
  )

  Record.defrecord(:r_client_srp_public, :client_srp_public, srp_a: :undefined)

  Record.defrecord(:r_certificate_verify, :certificate_verify,
    hashsign_algorithm: :undefined,
    signature: :undefined
  )

  Record.defrecord(:r_finished, :finished, verify_data: :undefined)

  Record.defrecord(:r_renegotiation_info, :renegotiation_info, renegotiated_connection: :undefined)

  Record.defrecord(:r_srp, :srp, username: :undefined)
  Record.defrecord(:r_hash_sign_algos, :hash_sign_algos, hash_sign_algos: :undefined)

  Record.defrecord(:r_signature_algorithms, :signature_algorithms,
    signature_scheme_list: :undefined
  )

  Record.defrecord(:r_alpn, :alpn, extension_data: :undefined)

  Record.defrecord(:r_next_protocol_negotiation, :next_protocol_negotiation,
    extension_data: :undefined
  )

  Record.defrecord(:r_next_protocol, :next_protocol, selected_protocol: :undefined)
  Record.defrecord(:r_elliptic_curves, :elliptic_curves, elliptic_curve_list: :undefined)
  Record.defrecord(:r_supported_groups, :supported_groups, supported_groups: :undefined)
  Record.defrecord(:r_ec_point_formats, :ec_point_formats, ec_point_format_list: :undefined)
  Record.defrecord(:r_sni, :sni, hostname: :undefined)
  Record.defrecord(:r_max_frag_enum, :max_frag_enum, enum: :undefined)

  Record.defrecord(:r_certificate_status_request, :certificate_status_request,
    status_type: :undefined,
    request: :undefined
  )

  Record.defrecord(:r_ocsp_status_request, :ocsp_status_request,
    responder_id_list: [],
    request_extensions: []
  )

  Record.defrecord(:r_certificate_status, :certificate_status,
    status_type: :undefined,
    response: :undefined
  )

  Record.defrecord(:r_client_hello_versions, :client_hello_versions, versions: :undefined)

  Record.defrecord(:r_server_hello_selected_version, :server_hello_selected_version,
    selected_version: :undefined
  )

  Record.defrecord(:r_signature_algorithms_cert, :signature_algorithms_cert,
    signature_scheme_list: :undefined
  )

  Record.defrecord(:r_client_hello, :client_hello,
    client_version: :undefined,
    random: :undefined,
    session_id: :undefined,
    cookie: :undefined,
    cipher_suites: :undefined,
    compression_methods: :undefined,
    extensions: :undefined
  )

  Record.defrecord(:r_sslsocket, :sslsocket, fd: nil, pid: nil)

  Record.defrecord(:r_hello_verify_request, :hello_verify_request,
    protocol_version: :undefined,
    cookie: :undefined
  )

  Record.defrecord(:r_handshake_fragment, :handshake_fragment,
    type: :undefined,
    length: :undefined,
    message_seq: :undefined,
    fragment_offset: :undefined,
    fragment_length: :undefined,
    fragment: :undefined
  )

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

  def init_connection_states(role, beastMitigation) do
    connectionEnd = :ssl_record.record_protocol_role(role)

    initial =
      initial_connection_state(
        connectionEnd,
        beastMitigation
      )

    current = %{initial | epoch: 0}

    initialPending =
      :ssl_record.empty_connection_state(
        connectionEnd,
        beastMitigation
      )

    pending = empty_connection_state(initialPending)

    %{
      saved_read: current,
      current_read: current,
      pending_read: pending,
      saved_write: current,
      current_write: current,
      pending_write: pending
    }
  end

  def empty_connection_state(empty) do
    Map.merge(empty, %{epoch: :undefined, replay_window: init_replay_window(64)})
  end

  def save_current_connection_state(%{current_read: current} = states, :read) do
    %{states | saved_read: current}
  end

  def save_current_connection_state(%{current_write: current} = states, :write) do
    %{states | saved_write: current}
  end

  def next_epoch(
        %{pending_read: pending, current_read: %{epoch: epoch}} = states,
        :read
      ) do
    %{states | pending_read: %{pending | epoch: epoch + 1, replay_window: init_replay_window(64)}}
  end

  def next_epoch(
        %{pending_write: pending, current_write: %{epoch: epoch}} = states,
        :write
      ) do
    %{
      states
      | pending_write: %{pending | epoch: epoch + 1, replay_window: init_replay_window(64)}
    }
  end

  def get_connection_state_by_epoch(epoch, %{current_write: %{epoch: epoch} = current}, :write) do
    current
  end

  def get_connection_state_by_epoch(epoch, %{saved_write: %{epoch: epoch} = saved}, :write) do
    saved
  end

  def get_connection_state_by_epoch(epoch, %{current_read: %{epoch: epoch} = current}, :read) do
    current
  end

  def get_connection_state_by_epoch(epoch, %{saved_read: %{epoch: epoch} = saved}, :read) do
    saved
  end

  defp set_connection_state_by_epoch(
         writeState,
         epoch,
         %{current_write: %{epoch: epoch}} = states,
         :write
       ) do
    %{states | current_write: writeState}
  end

  defp set_connection_state_by_epoch(
         writeState,
         epoch,
         %{saved_write: %{epoch: epoch}} = states,
         :write
       ) do
    %{states | saved_write: writeState}
  end

  defp set_connection_state_by_epoch(
         readState,
         epoch,
         %{current_read: %{epoch: epoch}} = states,
         :read
       ) do
    %{states | current_read: readState}
  end

  defp set_connection_state_by_epoch(
         readState,
         epoch,
         %{saved_read: %{epoch: epoch}} = states,
         :read
       ) do
    %{states | saved_read: readState}
  end

  def init_connection_state_seq(
        {254, _},
        %{current_read: %{epoch: 0, sequence_number: seq}, current_write: %{epoch: 0} = write} =
          connnectionStates0
      ) do
    Map.put(connnectionStates0, :current_write, Map.put(write, :sequence_number, seq))
  end

  def init_connection_state_seq(_, connnectionStates) do
    connnectionStates
  end

  def current_connection_state_epoch(%{current_read: %{epoch: epoch}}, :read) do
    epoch
  end

  def current_connection_state_epoch(%{current_write: %{epoch: epoch}}, :write) do
    epoch
  end

  def get_dtls_records(data, vinfo, buffer, sslOpts) do
    binData = :erlang.list_to_binary([buffer, data])
    get_dtls_records_aux(vinfo, binData, [], sslOpts)
  end

  def encode_handshake(frag, version, epoch, connectionStates) do
    encode_plain_text(22, version, epoch, frag, connectionStates)
  end

  def encode_alert_record(
        r_alert(level: level, description: description),
        version,
        connectionStates
      ) do
    %{epoch: epoch} =
      :ssl_record.current_connection_state(
        connectionStates,
        :write
      )

    encode_plain_text(
      21,
      version,
      epoch,
      <<level::size(8)-unsigned-big-integer, description::size(8)-unsigned-big-integer>>,
      connectionStates
    )
  end

  def encode_change_cipher_spec(version, epoch, connectionStates) do
    encode_plain_text(20, version, epoch, <<1::size(8)-unsigned-big-integer>>, connectionStates)
  end

  def encode_data(data, version, connectionStates) do
    %{epoch: epoch, max_fragment_length: maxFragmentLength} =
      :ssl_record.current_connection_state(
        connectionStates,
        :write
      )

    maxLength =
      cond do
        is_integer(maxFragmentLength) ->
          maxFragmentLength

        true ->
          16384
      end

    case :erlang.iolist_size(data) do
      n when n > maxLength ->
        frags =
          :tls_record.split_iovec(
            :erlang.iolist_to_iovec(data),
            maxLength
          )

        {revCipherText, connectionStates1} =
          :lists.foldl(
            fn frag, {acc, cS0} ->
              {cipherText, cS1} =
                encode_plain_text(
                  23,
                  version,
                  epoch,
                  frag,
                  cS0
                )

              {[cipherText | acc], cS1}
            end,
            {[], connectionStates},
            frags
          )

        {:lists.reverse(revCipherText), connectionStates1}

      _ ->
        encode_plain_text(23, version, epoch, data, connectionStates)
    end
  end

  def encode_plain_text(type, version, epoch, data, connectionStates) do
    write0 = get_connection_state_by_epoch(epoch, connectionStates, :write)
    {cipherFragment, write1} = encode_plain_text(type, version, data, write0)
    {cipherText, write} = encode_dtls_cipher_text(type, version, cipherFragment, write1)
    {cipherText, set_connection_state_by_epoch(write, epoch, connectionStates, :write)}
  end

  def decode_cipher_text(
        r_ssl_tls(epoch: epoch) = cipherText,
        connnectionStates0
      ) do
    readState = get_connection_state_by_epoch(epoch, connnectionStates0, :read)
    decode_cipher_text(cipherText, readState, connnectionStates0)
  end

  def protocol_version(:"dtlsv1.2") do
    {254, 253}
  end

  def protocol_version(:dtlsv1) do
    {254, 255}
  end

  def protocol_version({254, 253}) do
    :"dtlsv1.2"
  end

  def protocol_version({254, 255}) do
    :dtlsv1
  end

  def lowest_protocol_version(version = {m, n}, {m, o}) when n > o do
    version
  end

  def lowest_protocol_version({m, _}, version = {m, _}) do
    version
  end

  def lowest_protocol_version(version = {m, _}, {n, _}) when m > n do
    version
  end

  def lowest_protocol_version(_, version) do
    version
  end

  def lowest_protocol_version([]) do
    lowest_protocol_version()
  end

  def lowest_protocol_version(versions) do
    [ver | vers] = versions
    lowest_list_protocol_version(ver, vers)
  end

  def highest_protocol_version([]) do
    highest_protocol_version()
  end

  def highest_protocol_version(versions) do
    [ver | vers] = versions
    highest_list_protocol_version(ver, vers)
  end

  def highest_protocol_version(version = {m, n}, {m, o}) when n < o do
    version
  end

  def highest_protocol_version({m, _}, version = {m, _}) do
    version
  end

  def highest_protocol_version(version = {m, _}, {n, _}) when m < n do
    version
  end

  def highest_protocol_version(_, version) do
    version
  end

  def is_higher({m, n}, {m, o}) when n < o do
    true
  end

  def is_higher({m, _}, {n, _}) when m < n do
    true
  end

  def is_higher(_, _) do
    false
  end

  def supported_protocol_versions() do
    fun = fn version ->
      protocol_version(version)
    end

    case :application.get_env(
           :ssl,
           :dtls_protocol_version
         ) do
      :undefined ->
        :lists.map(fun, supported_protocol_versions([]))

      {:ok, []} ->
        :lists.map(fun, supported_protocol_versions([]))

      {:ok, vsns} when is_list(vsns) ->
        supported_protocol_versions(:lists.map(fun, vsns))

      {:ok, vsn} ->
        supported_protocol_versions([fun.(vsn)])
    end
  end

  defp supported_protocol_versions([]) do
    vsns =
      case sufficient_dtlsv1_2_crypto_support() do
        true ->
          [:"dtlsv1.2"]

        false ->
          [:dtlsv1]
      end

    :application.set_env(:ssl, :dtls_protocol_version, vsns)
    vsns
  end

  defp supported_protocol_versions([_ | _] = vsns) do
    case sufficient_dtlsv1_2_crypto_support() do
      true ->
        vsns

      false ->
        case vsns -- [:"dtlsv1.2"] do
          [] ->
            [:"tlsv1.1"]

          newVsns ->
            newVsns
        end
    end
  end

  def is_acceptable_version(version, versions) do
    :lists.member(version, versions)
  end

  def hello_version(version, versions) do
    case :dtls_v1.corresponding_tls_version(version) do
      tLSVersion when tLSVersion >= {3, 3} ->
        version

      _ ->
        lowest_protocol_version(versions)
    end
  end

  defp initial_connection_state(connectionEnd, beastMitigation) do
    %{
      security_parameters: :ssl_record.initial_security_params(connectionEnd),
      epoch: :undefined,
      sequence_number: 0,
      replay_window: init_replay_window(64),
      beast_mitigation: beastMitigation,
      compression_state: :undefined,
      cipher_state: :undefined,
      mac_secret: :undefined,
      secure_renegotiation: :undefined,
      client_verify_data: :undefined,
      server_verify_data: :undefined,
      max_fragment_length: :undefined
    }
  end

  defp get_dtls_records_aux(
         {dataTag, stateName, _, versions} = vinfo,
         <<type::size(8)-unsigned-big-integer, majVer::size(8)-unsigned-big-integer,
           minVer::size(8)-unsigned-big-integer, epoch::size(16)-unsigned-big-integer,
           sequenceNumber::size(48)-unsigned-big-integer, length::size(16)-unsigned-big-integer,
           data::size(length)-binary, rest::binary>> = rawDTLSRecord,
         acc,
         %{log_level: logLevel} = sslOpts
       )
       when (stateName == :hello or (stateName == :certify and dataTag == :udp) or
               (stateName == :abbreviated and dataTag == :udp)) and (type == 22 or type == 21) do
    :ssl_logger.debug(logLevel, :inbound, :record, [rawDTLSRecord])

    case is_acceptable_version(
           {majVer, minVer},
           versions
         ) do
      true ->
        get_dtls_records_aux(
          vinfo,
          rest,
          [
            r_ssl_tls(
              type: type,
              version: {majVer, minVer},
              epoch: epoch,
              sequence_number: sequenceNumber,
              fragment: data
            )
            | acc
          ],
          sslOpts
        )

      false ->
        r_alert(
          level: 2,
          description: 20,
          where: %{
            mfa: {:dtls_record, :get_dtls_records_aux, 4},
            line: 435,
            file: 'otp/lib/ssl/src/dtls_record.erl'
          }
        )
    end
  end

  defp get_dtls_records_aux(
         {_, _, version, _} = vinfo,
         <<type::size(8)-unsigned-big-integer, majVer::size(8)-unsigned-big-integer,
           minVer::size(8)-unsigned-big-integer, epoch::size(16)-unsigned-big-integer,
           sequenceNumber::size(48)-unsigned-big-integer, length::size(16)-unsigned-big-integer,
           data::size(length)-binary, rest::binary>> = rawDTLSRecord,
         acc,
         %{log_level: logLevel} = sslOpts
       )
       when type == 23 or type == 22 or type == 21 or type == 20 do
    :ssl_logger.debug(logLevel, :inbound, :record, [rawDTLSRecord])

    case {majVer, minVer} do
      ^version ->
        get_dtls_records_aux(
          vinfo,
          rest,
          [
            r_ssl_tls(
              type: type,
              version: {majVer, minVer},
              epoch: epoch,
              sequence_number: sequenceNumber,
              fragment: data
            )
            | acc
          ],
          sslOpts
        )

      _ ->
        r_alert(
          level: 2,
          description: 20,
          where: %{
            mfa: {:dtls_record, :get_dtls_records_aux, 4},
            line: 452,
            file: 'otp/lib/ssl/src/dtls_record.erl'
          }
        )
    end
  end

  defp get_dtls_records_aux(
         _,
         <<_::size(8)-unsigned-big-integer, _MajVer::size(8)-unsigned-big-integer,
           _MinVer::size(8)-unsigned-big-integer, length::size(16)-unsigned-big-integer,
           _::binary>>,
         _Acc,
         _
       )
       when length > 16384 + 2048 do
    r_alert(
      level: 2,
      description: 22,
      where: %{
        mfa: {:dtls_record, :get_dtls_records_aux, 4},
        line: 457,
        file: 'otp/lib/ssl/src/dtls_record.erl'
      }
    )
  end

  defp get_dtls_records_aux(_, data, acc, _) do
    case :erlang.size(data) <= 16384 + 2048 + 5 do
      true ->
        {:lists.reverse(acc), data}

      false ->
        r_alert(
          level: 2,
          description: 10,
          where: %{
            mfa: {:dtls_record, :get_dtls_records_aux, 4},
            line: 464,
            file: 'otp/lib/ssl/src/dtls_record.erl'
          }
        )
    end
  end

  defp init_replay_window(size) do
    %{size: size, top: size, bottom: 0, mask: 0 <<< 64}
  end

  def replay_detect(
        r_ssl_tls(sequence_number: sequenceNumber),
        %{replay_window: window}
      ) do
    is_replay(sequenceNumber, window)
  end

  defp is_replay(sequenceNumber, %{bottom: bottom})
       when sequenceNumber < bottom do
    true
  end

  defp is_replay(
         sequenceNumber,
         %{size: size, top: top, bottom: bottom, mask: mask}
       )
       when sequenceNumber >= bottom and sequenceNumber <= top do
    index = rem(sequenceNumber, size)
    index &&& mask == 1
  end

  defp is_replay(_, _) do
    false
  end

  defp update_replay_window(
         sequenceNumber,
         %{replay_window: %{size: size, top: top, bottom: bottom, mask: mask0} = window0} =
           connectionStates
       ) do
    noNewBits = sequenceNumber - top
    index = rem(sequenceNumber, size)
    mask = mask0 <<< noNewBits ||| index
    window = Map.merge(window0, %{top: sequenceNumber, bottom: bottom + noNewBits, mask: mask})
    %{connectionStates | replay_window: window}
  end

  defp encode_dtls_cipher_text(
         type,
         {majVer, minVer},
         fragment,
         %{epoch: epoch, sequence_number: seq} = writeState
       ) do
    length = :erlang.iolist_size(fragment)

    {[
       <<type::size(8)-unsigned-big-integer, majVer::size(8)-unsigned-big-integer,
         minVer::size(8)-unsigned-big-integer, epoch::size(16)-unsigned-big-integer,
         seq::size(48)-unsigned-big-integer, length::size(16)-unsigned-big-integer>>,
       fragment
     ], Map.put(writeState, :sequence_number, seq + 1)}
  end

  defp encode_plain_text(
         type,
         version,
         data,
         %{
           compression_state: compS0,
           cipher_state: cipherS0,
           epoch: epoch,
           sequence_number: seq,
           security_parameters:
             r_security_parameters(
               cipher_type: 2,
               bulk_cipher_algorithm: bCAlg,
               compression_algorithm: compAlg
             )
         } = writeState0
       ) do
    {comp, compS1} = :ssl_record.compress(compAlg, data, compS0)
    aAD = start_additional_data(type, version, epoch, seq)

    cipherS =
      :ssl_record.nonce_seed(
        bCAlg,
        <<epoch::size(16)-unsigned-big-integer, seq::size(48)-unsigned-big-integer>>,
        cipherS0
      )

    writeState = Map.merge(writeState0, %{compression_state: compS1, cipher_state: cipherS})
    tLSVersion = :dtls_v1.corresponding_tls_version(version)
    :ssl_record.cipher_aead(tLSVersion, comp, writeState, aAD)
  end

  defp encode_plain_text(
         type,
         version,
         fragment,
         %{
           compression_state: compS0,
           epoch: epoch,
           sequence_number: seq,
           cipher_state: cipherS0,
           security_parameters:
             r_security_parameters(
               compression_algorithm: compAlg,
               bulk_cipher_algorithm: bulkCipherAlgo
             )
         } = writeState0
       ) do
    {comp, compS1} = :ssl_record.compress(compAlg, fragment, compS0)
    writeState1 = Map.put(writeState0, :compression_state, compS1)
    mAC = calc_mac_hash(type, version, writeState1, epoch, seq, comp)
    tLSVersion = :dtls_v1.corresponding_tls_version(version)

    {cipherFragment, cipherS1} =
      :ssl_cipher.cipher(bulkCipherAlgo, cipherS0, mAC, fragment, tLSVersion)

    {cipherFragment, Map.put(writeState0, :cipher_state, cipherS1)}
  end

  defp decode_cipher_text(
         r_ssl_tls(
           type: type,
           version: version,
           epoch: epoch,
           sequence_number: seq,
           fragment: cipherFragment
         ) = cipherText,
         %{
           compression_state: compressionS0,
           cipher_state: cipherS0,
           security_parameters:
             r_security_parameters(
               cipher_type: 2,
               bulk_cipher_algorithm: bulkCipherAlgo,
               compression_algorithm: compAlg
             )
         } = readState0,
         connnectionStates0
       ) do
    aAD = start_additional_data(type, version, epoch, seq)

    cipherS =
      :ssl_record.nonce_seed(
        bulkCipherAlgo,
        <<epoch::size(16)-unsigned-big-integer, seq::size(48)-unsigned-big-integer>>,
        cipherS0
      )

    tLSVersion = :dtls_v1.corresponding_tls_version(version)

    case :ssl_record.decipher_aead(bulkCipherAlgo, cipherS, aAD, cipherFragment, tLSVersion) do
      plainFragment when is_binary(plainFragment) ->
        {plain, compressionS} =
          :ssl_record.uncompress(
            compAlg,
            plainFragment,
            compressionS0
          )

        readState1 = %{readState0 | compression_state: compressionS, cipher_state: cipherS}
        readState = update_replay_window(seq, readState1)

        connnectionStates =
          set_connection_state_by_epoch(
            readState,
            epoch,
            connnectionStates0,
            :read
          )

        {r_ssl_tls(cipherText, fragment: plain), connnectionStates}

      r_alert() = alert ->
        alert
    end
  end

  defp decode_cipher_text(
         r_ssl_tls(
           type: type,
           version: version,
           epoch: epoch,
           sequence_number: seq,
           fragment: cipherFragment
         ) = cipherText,
         %{
           compression_state: compressionS0,
           security_parameters: r_security_parameters(compression_algorithm: compAlg)
         } = readState0,
         connnectionStates0
       ) do
    {plainFragment, mac, readState1} =
      :ssl_record.decipher(
        :dtls_v1.corresponding_tls_version(version),
        cipherFragment,
        readState0,
        true
      )

    macHash = calc_mac_hash(type, version, readState1, epoch, seq, plainFragment)

    case :ssl_record.is_correct_mac(mac, macHash) do
      true ->
        {plain, compressionS1} =
          :ssl_record.uncompress(
            compAlg,
            plainFragment,
            compressionS0
          )

        readState2 = Map.put(readState1, :compression_state, compressionS1)
        readState = update_replay_window(seq, readState2)

        connnectionStates =
          set_connection_state_by_epoch(
            readState,
            epoch,
            connnectionStates0,
            :read
          )

        {r_ssl_tls(cipherText, fragment: plain), connnectionStates}

      false ->
        r_alert(
          level: 2,
          description: 20,
          where: %{
            mfa: {:dtls_record, :decode_cipher_text, 3},
            line: 597,
            file: 'otp/lib/ssl/src/dtls_record.erl'
          }
        )
    end
  end

  defp calc_mac_hash(
         type,
         version,
         %{
           mac_secret: macSecret,
           security_parameters: r_security_parameters(mac_algorithm: macAlg)
         },
         epoch,
         seqNo,
         fragment
       ) do
    length = :erlang.iolist_size(fragment)
    mac_hash(version, macAlg, macSecret, epoch, seqNo, type, length, fragment)
  end

  defp mac_hash({major, minor}, macAlg, macSecret, epoch, seqNo, type, length, fragment) do
    value = [
      <<epoch::size(16)-unsigned-big-integer, seqNo::size(48)-unsigned-big-integer,
        type::size(8)-unsigned-big-integer, major::size(8)-unsigned-big-integer,
        minor::size(8)-unsigned-big-integer, length::size(16)-unsigned-big-integer>>,
      fragment
    ]

    :dtls_v1.hmac_hash(macAlg, macSecret, value)
  end

  defp start_additional_data(type, {majVer, minVer}, epoch, seqNo) do
    <<epoch::size(16)-unsigned-big-integer, seqNo::size(48)-unsigned-big-integer,
      type::size(8)-unsigned-big-integer, majVer::size(8)-unsigned-big-integer,
      minVer::size(8)-unsigned-big-integer>>
  end

  defp lowest_list_protocol_version(ver, []) do
    ver
  end

  defp lowest_list_protocol_version(ver1, [ver2 | rest]) do
    lowest_list_protocol_version(
      lowest_protocol_version(
        ver1,
        ver2
      ),
      rest
    )
  end

  defp highest_list_protocol_version(ver, []) do
    ver
  end

  defp highest_list_protocol_version(ver1, [ver2 | rest]) do
    highest_list_protocol_version(
      highest_protocol_version(
        ver1,
        ver2
      ),
      rest
    )
  end

  defp highest_protocol_version() do
    highest_protocol_version(supported_protocol_versions())
  end

  defp lowest_protocol_version() do
    lowest_protocol_version(supported_protocol_versions())
  end

  defp sufficient_dtlsv1_2_crypto_support() do
    cryptoSupport = :crypto.supports()

    :proplists.get_bool(
      :sha256,
      :proplists.get_value(:hashs, cryptoSupport)
    )
  end
end
