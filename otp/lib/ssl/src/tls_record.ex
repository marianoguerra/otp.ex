defmodule :m_tls_record do
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
    fragment: :undefined
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

    current =
      initial_connection_state(
        connectionEnd,
        beastMitigation
      )

    pending =
      :ssl_record.empty_connection_state(
        connectionEnd,
        beastMitigation
      )

    %{
      :current_read => current,
      :pending_read => pending,
      :current_write => current,
      :pending_write => pending
    }
  end

  def get_tls_records(data, versions, buffer, maxFragLen, sslOpts)
      when is_binary(buffer) do
    parse_tls_records(versions, {[data], byte_size(data), []}, maxFragLen, sslOpts, :undefined)
  end

  def get_tls_records(data, versions, {hdr, {front, size, rear}}, maxFragLen, sslOpts) do
    parse_tls_records(
      versions,
      {front, size + byte_size(data), [data | rear]},
      maxFragLen,
      sslOpts,
      hdr
    )
  end

  def encode_handshake(frag, {3, 4}, connectionStates) do
    :tls_record_1_3.encode_handshake(frag, connectionStates)
  end

  def encode_handshake(
        frag,
        version,
        %{
          :current_write => %{
            :beast_mitigation => beastMitigation,
            :max_fragment_length => maxFragmentLength,
            :security_parameters => r_security_parameters(bulk_cipher_algorithm: bCA)
          }
        } = connectionStates
      ) do
    maxLength =
      cond do
        is_integer(maxFragmentLength) ->
          maxFragmentLength

        true ->
          16384
      end

    case :erlang.iolist_size(frag) do
      n when n > maxLength ->
        data =
          split_iovec(:erlang.iolist_to_iovec(frag), version, bCA, beastMitigation, maxLength)

        encode_fragments(22, version, data, connectionStates)

      _ ->
        encode_plain_text(22, version, frag, connectionStates)
    end
  end

  def encode_alert_record(alert, {3, 4}, connectionStates) do
    :tls_record_1_3.encode_alert_record(
      alert,
      connectionStates
    )
  end

  def encode_alert_record(
        r_alert(level: level, description: description),
        version,
        connectionStates
      ) do
    encode_plain_text(
      21,
      version,
      <<level::size(8)-unsigned-big-integer, description::size(8)-unsigned-big-integer>>,
      connectionStates
    )
  end

  def encode_change_cipher_spec(version, connectionStates) do
    encode_plain_text(20, version, <<1::size(8)-unsigned-big-integer>>, connectionStates)
  end

  def encode_data(data, {3, 4}, connectionStates) do
    :tls_record_1_3.encode_data(data, connectionStates)
  end

  def encode_data(
        data,
        version,
        %{
          :current_write => %{
            :beast_mitigation => beastMitigation,
            :max_fragment_length => maxFragmentLength,
            :security_parameters => r_security_parameters(bulk_cipher_algorithm: bCA)
          }
        } = connectionStates
      ) do
    maxLength =
      cond do
        is_integer(maxFragmentLength) ->
          maxFragmentLength

        true ->
          16384
      end

    fragments = split_iovec(data, version, bCA, beastMitigation, maxLength)
    encode_fragments(23, version, fragments, connectionStates)
  end

  def decode_cipher_text({3, 4}, cipherTextRecord, connectionStates, _) do
    :tls_record_1_3.decode_cipher_text(
      cipherTextRecord,
      connectionStates
    )
  end

  def decode_cipher_text(
        _,
        cipherTextRecord,
        %{
          :current_read => %{
            :sequence_number => seq,
            :security_parameters =>
              r_security_parameters(
                cipher_type: 2,
                bulk_cipher_algorithm: bulkCipherAlgo
              ),
            :cipher_state => cipherS0
          }
        } = connectionStates0,
        _
      ) do
    seqBin = <<seq::size(64)-unsigned-big-integer>>

    r_ssl_tls(type: type, version: {majVer, minVer} = version, fragment: fragment) =
      cipherTextRecord

    startAdditionalData =
      <<seqBin::binary, type::size(8)-unsigned-big-integer, majVer::size(8)-unsigned-big-integer,
        minVer::size(8)-unsigned-big-integer>>

    cipherS = :ssl_record.nonce_seed(bulkCipherAlgo, seqBin, cipherS0)

    case :ssl_record.decipher_aead(
           bulkCipherAlgo,
           cipherS,
           startAdditionalData,
           fragment,
           version
         ) do
      plainFragment when is_binary(plainFragment) ->
        %{
          :current_read =>
            %{:security_parameters => secParams, :compression_state => compressionS0} = readState0
        } = connectionStates0

        {plain, compressionS} =
          :ssl_record.uncompress(
            r_security_parameters(secParams, :compression_algorithm),
            plainFragment,
            compressionS0
          )

        connectionStates = %{
          connectionStates0
          | :current_read => %{
              readState0
              | :cipher_state => cipherS,
                :sequence_number => seq + 1,
                :compression_state => compressionS
            }
        }

        {r_ssl_tls(cipherTextRecord, fragment: plain), connectionStates}

      r_alert() = alert ->
        alert
    end
  end

  def decode_cipher_text(
        _,
        r_ssl_tls(
          version: version,
          fragment: cipherFragment
        ) = cipherTextRecord,
        %{:current_read => readState0} = connnectionStates0,
        paddingCheck
      ) do
    case :ssl_record.decipher(version, cipherFragment, readState0, paddingCheck) do
      {plainFragment, mac, readState1} ->
        macHash =
          :ssl_cipher.calc_mac_hash(
            r_ssl_tls(cipherTextRecord, :type),
            version,
            plainFragment,
            readState1
          )

        case :ssl_record.is_correct_mac(mac, macHash) do
          true ->
            %{
              :sequence_number => seq,
              :compression_state => compressionS0,
              :security_parameters => r_security_parameters(compression_algorithm: compAlg)
            } = readState0

            {plain, compressionS1} =
              :ssl_record.uncompress(
                compAlg,
                plainFragment,
                compressionS0
              )

            connnectionStates = %{
              connnectionStates0
              | :current_read => %{
                  readState1
                  | :sequence_number => seq + 1,
                    :compression_state => compressionS1
                }
            }

            {r_ssl_tls(cipherTextRecord, fragment: plain), connnectionStates}

          false ->
            r_alert(
              level: 2,
              description: 20,
              where: %{
                :mfa => {:tls_record, :decode_cipher_text, 4},
                :line => 242,
                :file => 'otp/lib/ssl/src/tls_record.erl'
              }
            )
        end

      r_alert() = alert ->
        alert
    end
  end

  def protocol_version(:"tlsv1.3") do
    {3, 4}
  end

  def protocol_version(:"tlsv1.2") do
    {3, 3}
  end

  def protocol_version(:"tlsv1.1") do
    {3, 2}
  end

  def protocol_version(:tlsv1) do
    {3, 1}
  end

  def protocol_version(:sslv3) do
    {3, 0}
  end

  def protocol_version(:sslv2) do
    {2, 0}
  end

  def protocol_version({3, 4}) do
    :"tlsv1.3"
  end

  def protocol_version({3, 3}) do
    :"tlsv1.2"
  end

  def protocol_version({3, 2}) do
    :"tlsv1.1"
  end

  def protocol_version({3, 1}) do
    :tlsv1
  end

  def protocol_version({3, 0}) do
    :sslv3
  end

  def lowest_protocol_version(version = {m, n}, {m, o}) when n < o do
    version
  end

  def lowest_protocol_version({m, _}, version = {m, _}) do
    version
  end

  def lowest_protocol_version(version = {m, _}, {n, _}) when m < n do
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

  def highest_protocol_version(version = {m, n}, {m, o}) when n > o do
    version
  end

  def highest_protocol_version({m, _}, version = {m, _}) do
    version
  end

  def highest_protocol_version(version = {m, _}, {n, _}) when m > n do
    version
  end

  def highest_protocol_version(_, version) do
    version
  end

  def is_higher({m, n}, {m, o}) when n > o do
    true
  end

  def is_higher({m, _}, {n, _}) when m > n do
    true
  end

  def is_higher(_, _) do
    false
  end

  def supported_protocol_versions() do
    fun = fn version ->
      protocol_version(version)
    end

    case :application.get_env(:ssl, :protocol_version) do
      :undefined ->
        :lists.map(fun, supported_protocol_versions([]))

      {:ok, []} ->
        :lists.map(fun, supported_protocol_versions([]))

      {:ok, vsns} when is_list(vsns) ->
        versions =
          :lists.filter(
            &is_acceptable_version/1,
            :lists.map(fun, vsns)
          )

        supported_protocol_versions(versions)

      {:ok, vsn} ->
        versions =
          :lists.filter(
            &is_acceptable_version/1,
            [fun.(vsn)]
          )

        supported_protocol_versions(versions)
    end
  end

  defp supported_protocol_versions([]) do
    vsns = sufficient_support([:"tlsv1.3", :"tlsv1.2"])
    :application.set_env(:ssl, :protocol_version, vsns)
    vsns
  end

  defp supported_protocol_versions([_ | _] = vsns) do
    sufficient_support(vsns)
  end

  def sufficient_crypto_support(version) do
    sufficient_crypto_support(:crypto.supports(), version)
  end

  defp sufficient_crypto_support(cryptoSupport, {_, _} = version) do
    sufficient_crypto_support(
      cryptoSupport,
      protocol_version(version)
    )
  end

  defp sufficient_crypto_support(cryptoSupport, version)
       when version == :tlsv1 or version == :"tlsv1.1" do
    hashes = :proplists.get_value(:hashs, cryptoSupport)

    pKeys =
      :proplists.get_value(
        :public_keys,
        cryptoSupport
      )

    :proplists.get_bool(
      :sha,
      hashes
    ) and
      :proplists.get_bool(
        :md5,
        hashes
      ) and
      :proplists.get_bool(
        :aes_cbc,
        :proplists.get_value(
          :ciphers,
          cryptoSupport
        )
      ) and
      (:proplists.get_bool(
         :ecdsa,
         pKeys
       ) or
         :proplists.get_bool(
           :rsa,
           pKeys
         ) or
         :proplists.get_bool(
           :dss,
           pKeys
         )) and
      (:proplists.get_bool(
         :ecdh,
         pKeys
       ) or
         :proplists.get_bool(
           :dh,
           pKeys
         ))
  end

  defp sufficient_crypto_support(cryptoSupport, :"tlsv1.2") do
    pKeys =
      :proplists.get_value(
        :public_keys,
        cryptoSupport
      )

    :proplists.get_bool(
      :sha256,
      :proplists.get_value(
        :hashs,
        cryptoSupport
      )
    ) and
      :proplists.get_bool(
        :aes_cbc,
        :proplists.get_value(
          :ciphers,
          cryptoSupport
        )
      ) and
      (:proplists.get_bool(
         :ecdsa,
         pKeys
       ) or
         :proplists.get_bool(
           :rsa,
           pKeys
         ) or
         :proplists.get_bool(
           :dss,
           pKeys
         )) and
      (:proplists.get_bool(
         :ecdh,
         pKeys
       ) or
         :proplists.get_bool(
           :dh,
           pKeys
         ))
  end

  defp sufficient_crypto_support(cryptoSupport, :"tlsv1.3") do
    fun = fn {group, algorithm} ->
      is_algorithm_supported(cryptoSupport, group, algorithm)
    end

    l = [
      {:ciphers, :aes_gcm},
      {:ciphers, :chacha20_poly1305},
      {:hashs, :sha256},
      {:hashs, :sha384},
      {:rsa_opts, :rsa_pkcs1_padding},
      {:rsa_opts, :rsa_pkcs1_pss_padding},
      {:rsa_opts, :rsa_pss_saltlen},
      {:public_keys, :ecdh},
      {:public_keys, :dh},
      {:public_keys, :rsa},
      {:public_keys, :ecdsa},
      {:curves, :secp256r1},
      {:curves, :x25519}
    ]

    :lists.all(fun, l)
  end

  defp is_algorithm_supported(cryptoSupport, group, algorithm) do
    :proplists.get_bool(
      algorithm,
      :proplists.get_value(group, cryptoSupport)
    )
  end

  def is_acceptable_version({n, _}) when n >= 3 do
    true
  end

  def is_acceptable_version(_) do
    false
  end

  def is_acceptable_version({n, _} = version, versions) when n >= 3 do
    :lists.member(version, versions)
  end

  def is_acceptable_version(_, _) do
    false
  end

  def hello_version([highest | _]) when highest >= {3, 3} do
    {3, 3}
  end

  def hello_version(versions) do
    lowest_protocol_version(versions)
  end

  def split_iovec([], _) do
    []
  end

  def split_iovec(data, maximumFragmentLength) do
    {part, rest} = split_iovec(data, maximumFragmentLength, [])
    [part | split_iovec(rest, maximumFragmentLength)]
  end

  defp initial_connection_state(connectionEnd, beastMitigation) do
    %{
      :security_parameters => :ssl_record.initial_security_params(connectionEnd),
      :sequence_number => 0,
      :beast_mitigation => beastMitigation,
      :compression_state => :undefined,
      :cipher_state => :undefined,
      :mac_secret => :undefined,
      :secure_renegotiation => :undefined,
      :client_verify_data => :undefined,
      :server_verify_data => :undefined,
      :max_fragment_length => :undefined
    }
  end

  def build_tls_record(r_ssl_tls(type: type, version: {majVer, minVer}, fragment: fragment)) do
    length = byte_size(fragment)

    <<type::size(8)-unsigned-big-integer, majVer::size(8)-unsigned-big-integer,
      minVer::size(8)-unsigned-big-integer, length::size(16)-unsigned-big-integer,
      fragment::binary>>
  end

  defp parse_tls_records(versions, q, maxFragLen, sslOpts, :undefined) do
    decode_tls_records(versions, q, maxFragLen, sslOpts, [], :undefined, :undefined, :undefined)
  end

  defp parse_tls_records(
         versions,
         q,
         maxFragLen,
         sslOpts,
         r_ssl_tls(type: type, version: version, fragment: length)
       ) do
    decode_tls_records(versions, q, maxFragLen, sslOpts, [], type, version, length)
  end

  defp decode_tls_records(
         versions,
         {_, size, _} = q0,
         maxFragLen,
         sslOpts,
         acc,
         :undefined,
         _Version,
         _Length
       ) do
    cond do
      5 <= size ->
        {<<type::size(8)-unsigned-big-integer, majVer::size(8)-unsigned-big-integer,
           minVer::size(8)-unsigned-big-integer, length::size(16)-unsigned-big-integer>>,
         q} = binary_from_front(5, q0)

        validate_tls_records_type(
          versions,
          q,
          maxFragLen,
          sslOpts,
          acc,
          type,
          {majVer, minVer},
          length
        )

      3 <= size ->
        {<<type::size(8)-unsigned-big-integer, majVer::size(8)-unsigned-big-integer,
           minVer::size(8)-unsigned-big-integer>>, q} = binary_from_front(3, q0)

        validate_tls_records_type(
          versions,
          q,
          maxFragLen,
          sslOpts,
          acc,
          type,
          {majVer, minVer},
          :undefined
        )

      1 <= size ->
        {<<type::size(8)-unsigned-big-integer>>, q} = binary_from_front(1, q0)

        validate_tls_records_type(
          versions,
          q,
          maxFragLen,
          sslOpts,
          acc,
          type,
          :undefined,
          :undefined
        )

      true ->
        validate_tls_records_type(
          versions,
          q0,
          maxFragLen,
          sslOpts,
          acc,
          :undefined,
          :undefined,
          :undefined
        )
    end
  end

  defp decode_tls_records(
         versions,
         {_, size, _} = q0,
         maxFragLen,
         sslOpts,
         acc,
         type,
         :undefined,
         _Length
       ) do
    cond do
      4 <= size ->
        {<<majVer::size(8)-unsigned-big-integer, minVer::size(8)-unsigned-big-integer,
           length::size(16)-unsigned-big-integer>>, q} = binary_from_front(4, q0)

        validate_tls_record_version(
          versions,
          q,
          maxFragLen,
          sslOpts,
          acc,
          type,
          {majVer, minVer},
          length
        )

      2 <= size ->
        {<<majVer::size(8)-unsigned-big-integer, minVer::size(8)-unsigned-big-integer>>, q} =
          binary_from_front(2, q0)

        validate_tls_record_version(
          versions,
          q,
          maxFragLen,
          sslOpts,
          acc,
          type,
          {majVer, minVer},
          :undefined
        )

      true ->
        validate_tls_record_version(
          versions,
          q0,
          maxFragLen,
          sslOpts,
          acc,
          type,
          :undefined,
          :undefined
        )
    end
  end

  defp decode_tls_records(
         versions,
         {_, size, _} = q0,
         maxFragLen,
         sslOpts,
         acc,
         type,
         version,
         :undefined
       ) do
    cond do
      2 <= size ->
        {<<length::size(16)-unsigned-big-integer>>, q} = binary_from_front(2, q0)
        validate_tls_record_length(versions, q, maxFragLen, sslOpts, acc, type, version, length)

      true ->
        validate_tls_record_length(
          versions,
          q0,
          maxFragLen,
          sslOpts,
          acc,
          type,
          version,
          :undefined
        )
    end
  end

  defp decode_tls_records(versions, q, maxFragLen, sslOpts, acc, type, version, length) do
    validate_tls_record_length(versions, q, maxFragLen, sslOpts, acc, type, version, length)
  end

  defp validate_tls_records_type(
         _Versions,
         q,
         _MaxFragLen,
         _SslOpts,
         acc,
         :undefined,
         _Version,
         _Length
       ) do
    {:lists.reverse(acc), {:undefined, q}}
  end

  defp validate_tls_records_type(versions, q, maxFragLen, sslOpts, acc, type, version, length) do
    cond do
      is_integer(type) and 20 <= type and type <= 23 ->
        validate_tls_record_version(versions, q, maxFragLen, sslOpts, acc, type, version, length)

      true ->
        r_alert(
          level: 2,
          description: 10,
          where: %{
            :mfa => {:tls_record, :validate_tls_records_type, 8},
            :line => 539,
            :file => 'otp/lib/ssl/src/tls_record.erl'
          },
          reason: {:unsupported_record_type, type}
        )
    end
  end

  defp validate_tls_record_version(
         _Versions,
         q,
         _MaxFragLen,
         _SslOpts,
         acc,
         type,
         :undefined,
         _Length
       ) do
    {:lists.reverse(acc), {r_ssl_tls(type: type, version: :undefined, fragment: :undefined), q}}
  end

  defp validate_tls_record_version(versions, q, maxFragLen, sslOpts, acc, type, version, length) do
    case versions do
      _ when is_list(versions) ->
        case is_acceptable_version(version, versions) do
          true ->
            validate_tls_record_length(
              versions,
              q,
              maxFragLen,
              sslOpts,
              acc,
              type,
              version,
              length
            )

          false ->
            r_alert(
              level: 2,
              description: 20,
              where: %{
                :mfa => {:tls_record, :validate_tls_record_version, 8},
                :line => 552,
                :file => 'otp/lib/ssl/src/tls_record.erl'
              },
              reason: {:unsupported_version, version}
            )
        end

      {3, 4} when version === {3, 3} ->
        validate_tls_record_length(versions, q, maxFragLen, sslOpts, acc, type, version, length)

      ^version ->
        validate_tls_record_length(versions, q, maxFragLen, sslOpts, acc, type, version, length)

      _ ->
        r_alert(
          level: 2,
          description: 20,
          where: %{
            :mfa => {:tls_record, :validate_tls_record_version, 8},
            :line => 560,
            :file => 'otp/lib/ssl/src/tls_record.erl'
          },
          reason: {:unsupported_version, version}
        )
    end
  end

  defp validate_tls_record_length(
         _Versions,
         q,
         _MaxFragLen,
         _SslOpts,
         acc,
         type,
         version,
         :undefined
       ) do
    {:lists.reverse(acc), {r_ssl_tls(type: type, version: version, fragment: :undefined), q}}
  end

  defp validate_tls_record_length(
         versions,
         {_, size0, _} = q0,
         maxFragLen,
         %{:log_level => logLevel} = sslOpts,
         acc,
         type,
         version,
         length
       ) do
    max =
      cond do
        is_integer(maxFragLen) ->
          maxFragLen + 256 + 32

        true ->
          max_len(versions)
      end

    cond do
      length <= max ->
        cond do
          length <= size0 ->
            {fragment, q} = binary_from_front(length, q0)
            record = r_ssl_tls(type: type, version: version, fragment: fragment)
            :ssl_logger.debug(logLevel, :inbound, :record, record)

            decode_tls_records(
              versions,
              q,
              maxFragLen,
              sslOpts,
              [record | acc],
              :undefined,
              :undefined,
              :undefined
            )

          true ->
            {:lists.reverse(acc), {r_ssl_tls(type: type, version: version, fragment: length), q0}}
        end

      true ->
        r_alert(
          level: 2,
          description: 22,
          where: %{
            :mfa => {:tls_record, :validate_tls_record_length, 8},
            :line => 588,
            :file => 'otp/lib/ssl/src/tls_record.erl'
          }
        )
    end
  end

  defp binary_from_front(0, q) do
    {<<>>, q}
  end

  defp binary_from_front(splitSize, {front, size, rear})
       when splitSize <= size do
    binary_from_front(splitSize, front, size, rear, [])
  end

  defp binary_from_front(splitSize, [], size, rear, acc) do
    case rear do
      [_] ->
        binary_from_front(splitSize, rear, size, [], acc)

      [bin2, bin1] ->
        binary_from_front(splitSize, [bin1, bin2], size, [], acc)

      [bin3, bin2, bin1] ->
        binary_from_front(splitSize, [bin1, bin2, bin3], size, [], acc)

      [[_, _, _] | _] ->
        binary_from_front(splitSize, :lists.reverse(rear), size, [], acc)
    end
  end

  defp binary_from_front(splitSize, [bin | front], size, rear, []) do
    binSize = byte_size(bin)

    cond do
      splitSize < binSize ->
        {retBin, rest} = :erlang.split_binary(bin, splitSize)
        {retBin, {[rest | front], size - splitSize, rear}}

      binSize < splitSize ->
        binary_from_front(splitSize - binSize, front, size, rear, [bin])

      true ->
        {bin, {front, size - splitSize, rear}}
    end
  end

  defp binary_from_front(splitSize, [bin | front], size, rear, acc) do
    binSize = byte_size(bin)

    cond do
      splitSize < binSize ->
        {last, rest} = :erlang.split_binary(bin, splitSize)

        retBin =
          :erlang.iolist_to_binary(
            :lists.reverse(
              acc,
              [last]
            )
          )

        {retBin, {[rest | front], size - byte_size(retBin), rear}}

      binSize < splitSize ->
        binary_from_front(splitSize - binSize, front, size, rear, [bin | acc])

      true ->
        retBin =
          :erlang.iolist_to_binary(
            :lists.reverse(
              acc,
              [bin]
            )
          )

        {retBin, {front, size - byte_size(retBin), rear}}
    end
  end

  def encode_plain_text(type, version, data, connectionStates0) do
    {[cipherText], connectionStates} = encode_fragments(type, version, [data], connectionStates0)
    {cipherText, connectionStates}
  end

  defp encode_fragments(
         type,
         version,
         data,
         %{
           :current_write => %{
             :compression_state => compS,
             :cipher_state => cipherS,
             :sequence_number => seq
           }
         } = connectionStates
       ) do
    encode_fragments(type, version, data, connectionStates, compS, cipherS, seq, [])
  end

  defp encode_fragments(
         _Type,
         _Version,
         [],
         %{:current_write => writeS} = cS,
         compS,
         cipherS,
         seq,
         cipherFragments
       ) do
    {:lists.reverse(cipherFragments),
     %{
       cS
       | :current_write => %{
           writeS
           | :compression_state => compS,
             :cipher_state => cipherS,
             :sequence_number => seq
         }
     }}
  end

  defp encode_fragments(
         type,
         version,
         [text | data],
         %{
           :current_write => %{
             :security_parameters =>
               r_security_parameters(
                 cipher_type: 2,
                 bulk_cipher_algorithm: bCAlg,
                 compression_algorithm: compAlg
               ) = secPars
           }
         } = cS,
         compS0,
         cipherS0,
         seq,
         cipherFragments
       ) do
    {compText, compS} = :ssl_record.compress(compAlg, text, compS0)
    seqBin = <<seq::size(64)-unsigned-big-integer>>
    cipherS1 = :ssl_record.nonce_seed(bCAlg, seqBin, cipherS0)
    {majVer, minVer} = version
    versionBin = <<majVer::size(8)-unsigned-big-integer, minVer::size(8)-unsigned-big-integer>>

    startAdditionalData =
      <<seqBin::binary, type::size(8)-unsigned-big-integer, versionBin::binary>>

    {cipherFragment, cipherS} =
      :ssl_record.cipher_aead(version, compText, cipherS1, startAdditionalData, secPars)

    length = byte_size(cipherFragment)

    cipherHeader =
      <<type::size(8)-unsigned-big-integer, versionBin::binary,
        length::size(16)-unsigned-big-integer>>

    encode_fragments(type, version, data, cS, compS, cipherS, seq + 1, [
      [cipherHeader, cipherFragment] | cipherFragments
    ])
  end

  defp encode_fragments(
         type,
         version,
         [text | data],
         %{
           :current_write => %{
             :security_parameters =>
               r_security_parameters(
                 compression_algorithm: compAlg,
                 mac_algorithm: macAlgorithm
               ) = secPars,
             :mac_secret => macSecret
           }
         } = cS,
         compS0,
         cipherS0,
         seq,
         cipherFragments
       ) do
    {compText, compS} = :ssl_record.compress(compAlg, text, compS0)
    macHash = :ssl_cipher.calc_mac_hash(type, version, compText, macAlgorithm, macSecret, seq)
    {cipherFragment, cipherS} = :ssl_record.cipher(version, compText, cipherS0, macHash, secPars)
    length = byte_size(cipherFragment)
    {majVer, minVer} = version

    cipherHeader =
      <<type::size(8)-unsigned-big-integer, majVer::size(8)-unsigned-big-integer,
        minVer::size(8)-unsigned-big-integer, length::size(16)-unsigned-big-integer>>

    encode_fragments(type, version, data, cS, compS, cipherS, seq + 1, [
      [cipherHeader, cipherFragment] | cipherFragments
    ])
  end

  defp encode_fragments(_Type, _Version, _Data, cS, _CompS, _CipherS, _Seq, _CipherFragments) do
    exit({:cs, cS})
  end

  defp split_iovec(data, version, bCA, :one_n_minus_one, maxLength)
       when bCA !== 1 and ({3, 1} == version or {3, 0} == version) do
    {part, restData} = split_iovec(data, 1, [])
    [part | split_iovec(restData, maxLength)]
  end

  defp split_iovec(data, version, bCA, :zero_n, maxLength)
       when bCA !== 1 and ({3, 1} == version or {3, 0} == version) do
    {part, restData} = split_iovec(data, 0, [])
    [part | split_iovec(restData, maxLength)]
  end

  defp split_iovec(data, _Version, _BCA, _BeatMitigation, maxLength) do
    split_iovec(data, maxLength)
  end

  defp split_iovec([bin | data] = bin_Data, splitSize, acc) do
    binSize = byte_size(bin)

    cond do
      binSize <= splitSize ->
        split_iovec(data, splitSize - binSize, [bin | acc])

      splitSize == 0 ->
        {:lists.reverse(acc), bin_Data}

      splitSize < binSize ->
        {last, rest} = :erlang.split_binary(bin, splitSize)
        {:lists.reverse(acc, [last]), [rest | data]}
    end
  end

  defp split_iovec([], _SplitSize, acc) do
    {:lists.reverse(acc), []}
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

  defp max_len([{3, 4} | _]) do
    16384 + 256
  end

  defp max_len(_) do
    16384 + 2048
  end

  defp sufficient_support(versions) do
    cryptoSupport = :crypto.supports()

    for ver <- versions,
        sufficient_crypto_support(cryptoSupport, ver) do
      ver
    end
  end
end
