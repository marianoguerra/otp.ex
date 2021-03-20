defmodule :m_ssl_cipher_format do
  use Bitwise
  require Record
  Record.defrecord(:r_sslsocket, :sslsocket, fd: nil, pid: nil)

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

  def suite_map_to_str(%{key_exchange: :null, cipher: :null, mac: :null, prf: :null}) do
    'TLS_EMPTY_RENEGOTIATION_INFO_SCSV'
  end

  def suite_map_to_str(%{key_exchange: :any, cipher: cipher, mac: :aead, prf: pRF}) do
    'TLS_' ++
      :string.to_upper(:erlang.atom_to_list(cipher)) ++
      '_' ++ :string.to_upper(:erlang.atom_to_list(pRF))
  end

  def suite_map_to_str(%{key_exchange: kex, cipher: cipher, mac: :aead, prf: pRF}) do
    'TLS_' ++
      :string.to_upper(:erlang.atom_to_list(kex)) ++
      '_WITH_' ++
      :string.to_upper(:erlang.atom_to_list(cipher)) ++
      '_' ++ :string.to_upper(:erlang.atom_to_list(pRF))
  end

  def suite_map_to_str(%{key_exchange: kex, cipher: cipher, mac: mac}) do
    'TLS_' ++
      :string.to_upper(:erlang.atom_to_list(kex)) ++
      '_WITH_' ++
      :string.to_upper(:erlang.atom_to_list(cipher)) ++
      '_' ++ :string.to_upper(:erlang.atom_to_list(mac))
  end

  def suite_str_to_map('TLS_EMPTY_RENEGOTIATION_INFO_SCSV') do
    %{key_exchange: :null, cipher: :null, mac: :null, prf: :null}
  end

  def suite_str_to_map(suiteStr) do
    str0 = :string.prefix(suiteStr, 'TLS_')

    case :string.split(str0, '_WITH_') do
      [rest] ->
        tls_1_3_suite_str_to_map(rest)

      [prefix, kex | rest]
      when prefix == 'SPR' or prefix == 'PSK' or
             prefix == 'DHE' or prefix == 'ECDHE' ->
        pre_tls_1_3_suite_str_to_map(prefix ++ '_' ++ kex, rest)

      [kex | rest] ->
        pre_tls_1_3_suite_str_to_map(kex, rest)
    end
  end

  def suite_map_to_openssl_str(%{key_exchange: :any, mac: :aead} = suite) do
    suite_map_to_str(suite)
  end

  def suite_map_to_openssl_str(%{key_exchange: :null} = suite) do
    suite_map_to_str(suite)
  end

  def suite_map_to_openssl_str(%{key_exchange: :rsa = kex, cipher: cipher, mac: mac})
      when cipher == 'des_cbc' or cipher == '3des_ede_cbc' do
    openssl_cipher_name(
      kex,
      :string.to_upper(:erlang.atom_to_list(cipher))
    ) ++ '-' ++ :string.to_upper(:erlang.atom_to_list(mac))
  end

  def suite_map_to_openssl_str(%{
        key_exchange: kex,
        cipher: :chacha20_poly1305 = cipher,
        mac: :aead
      }) do
    openssl_suite_start(:string.to_upper(:erlang.atom_to_list(kex))) ++
      openssl_cipher_name(
        kex,
        :string.to_upper(:erlang.atom_to_list(cipher))
      )
  end

  def suite_map_to_openssl_str(%{key_exchange: kex, cipher: cipher, mac: :aead, prf: pRF}) do
    openssl_suite_start(:string.to_upper(:erlang.atom_to_list(kex))) ++
      openssl_cipher_name(
        kex,
        :string.to_upper(:erlang.atom_to_list(cipher))
      ) ++ '-' ++ :string.to_upper(:erlang.atom_to_list(pRF))
  end

  def suite_map_to_openssl_str(%{key_exchange: kex, cipher: cipher, mac: mac}) do
    openssl_suite_start(:string.to_upper(:erlang.atom_to_list(kex))) ++
      openssl_cipher_name(
        kex,
        :string.to_upper(:erlang.atom_to_list(cipher))
      ) ++ '-' ++ :string.to_upper(:erlang.atom_to_list(mac))
  end

  def suite_openssl_str_to_map('TLS_' ++ _ = suiteStr) do
    suite_str_to_map(suiteStr)
  end

  def suite_openssl_str_to_map('DES-CBC-SHA') do
    suite_str_to_map('TLS_RSA_WITH_DES_CBC_SHA')
  end

  def suite_openssl_str_to_map('DES-CBC3-SHA') do
    suite_str_to_map('TLS_RSA_WITH_3DES_EDE_CBC_SHA')
  end

  def suite_openssl_str_to_map('SRP-DSS-DES-CBC3-SHA') do
    suite_str_to_map('TLS_SRP_SHA_DSS_WITH_3DES_EDE_CBC_SHA')
  end

  def suite_openssl_str_to_map('DHE-RSA-' ++ rest) do
    suite_openssl_str_to_map('DHE-RSA', rest)
  end

  def suite_openssl_str_to_map('DHE-DSS-' ++ rest) do
    suite_openssl_str_to_map('DHE-DSS', rest)
  end

  def suite_openssl_str_to_map('EDH-RSA-' ++ rest) do
    suite_openssl_str_to_map('DHE-RSA', rest)
  end

  def suite_openssl_str_to_map('EDH-DSS-' ++ rest) do
    suite_openssl_str_to_map('DHE-DSS', rest)
  end

  def suite_openssl_str_to_map('DES' ++ _ = rest) do
    suite_openssl_str_to_map('RSA', rest)
  end

  def suite_openssl_str_to_map('AES' ++ _ = rest) do
    suite_openssl_str_to_map('RSA', rest)
  end

  def suite_openssl_str_to_map('RC4' ++ _ = rest) do
    suite_openssl_str_to_map('RSA', rest)
  end

  def suite_openssl_str_to_map('ECDH-RSA-' ++ rest) do
    suite_openssl_str_to_map('ECDH-RSA', rest)
  end

  def suite_openssl_str_to_map('ECDH-ECDSA-' ++ rest) do
    suite_openssl_str_to_map('ECDH-ECDSA', rest)
  end

  def suite_openssl_str_to_map('ECDHE-RSA-' ++ rest) do
    suite_openssl_str_to_map('ECDHE-RSA', rest)
  end

  def suite_openssl_str_to_map('ECDHE-ECDSA-' ++ rest) do
    suite_openssl_str_to_map('ECDHE-ECDSA', rest)
  end

  def suite_openssl_str_to_map('RSA-PSK-' ++ rest) do
    suite_openssl_str_to_map('RSA-PSK', rest)
  end

  def suite_openssl_str_to_map('RSA-' ++ rest) do
    suite_openssl_str_to_map('RSA', rest)
  end

  def suite_openssl_str_to_map('DHE-PSK-' ++ rest) do
    suite_openssl_str_to_map('DHE-PSK', rest)
  end

  def suite_openssl_str_to_map('ECDHE-PSK-' ++ rest) do
    suite_openssl_str_to_map('ECDHE-PSK', rest)
  end

  def suite_openssl_str_to_map('PSK-' ++ rest) do
    suite_openssl_str_to_map('PSK', rest)
  end

  def suite_openssl_str_to_map('SRP-RSA-' ++ rest) do
    suite_openssl_str_to_map('SRP-RSA', rest)
  end

  def suite_openssl_str_to_map('SRP-DSS-' ++ rest) do
    suite_openssl_str_to_map('SRP-DSS', rest)
  end

  def suite_openssl_str_to_map('SRP-' ++ rest) do
    suite_openssl_str_to_map('SRP', rest)
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 0::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :null, cipher: :null, mac: :null, prf: :null}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 255::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :null, cipher: :null, mac: :null, prf: :null}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 4::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa, cipher: :rc4_128, mac: :md5, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 5::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa, cipher: :rc4_128, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 9::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa, cipher: :des_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 10::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa, cipher: :"3des_ede_cbc", mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 18::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_dss, cipher: :des_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 19::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_dss, cipher: :"3des_ede_cbc", mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 21::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_rsa, cipher: :des_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 22::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_rsa, cipher: :"3des_ede_cbc", mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 47::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa, cipher: :aes_128_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 50::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_dss, cipher: :aes_128_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 51::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_rsa, cipher: :aes_128_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 53::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa, cipher: :aes_256_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 56::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_dss, cipher: :aes_256_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 57::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_rsa, cipher: :aes_256_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 60::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa, cipher: :aes_128_cbc, mac: :sha256, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 61::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa, cipher: :aes_256_cbc, mac: :sha256, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 64::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_dss, cipher: :aes_128_cbc, mac: :sha256, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 103::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_rsa, cipher: :aes_128_cbc, mac: :sha256, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 106::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_dss, cipher: :aes_256_cbc, mac: :sha256, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 107::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_rsa, cipher: :aes_256_cbc, mac: :sha256, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 24::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dh_anon, cipher: :rc4_128, mac: :md5, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 26::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dh_anon, cipher: :des_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 27::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dh_anon, cipher: :"3des_ede_cbc", mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 52::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dh_anon, cipher: :aes_128_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 58::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dh_anon, cipher: :aes_256_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 108::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dh_anon, cipher: :aes_128_cbc, mac: :sha256, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 109::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dh_anon, cipher: :aes_256_cbc, mac: :sha256, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 138::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :psk, cipher: :rc4_128, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 139::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :psk, cipher: :"3des_ede_cbc", mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 140::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :psk, cipher: :aes_128_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 141::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :psk, cipher: :aes_256_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 142::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_psk, cipher: :rc4_128, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 143::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_psk, cipher: :"3des_ede_cbc", mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 144::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_psk, cipher: :aes_128_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 145::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_psk, cipher: :aes_256_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 146::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa_psk, cipher: :rc4_128, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 147::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa_psk, cipher: :"3des_ede_cbc", mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 148::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa_psk, cipher: :aes_128_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 149::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa_psk, cipher: :aes_256_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 44::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :psk, cipher: :null, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 45::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_psk, cipher: :null, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 46::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa_psk, cipher: :null, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 168::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :psk, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 169::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :psk, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 170::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_psk, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 171::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_psk, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 172::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa_psk, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 173::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa_psk, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 174::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :psk, cipher: :aes_128_cbc, mac: :sha256, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 175::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :psk, cipher: :aes_256_cbc, mac: :sha384, prf: :sha384}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 178::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_psk, cipher: :aes_128_cbc, mac: :sha256, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 179::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_psk, cipher: :aes_256_cbc, mac: :sha384, prf: :sha384}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 182::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa_psk, cipher: :aes_128_cbc, mac: :sha256, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 183::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa_psk, cipher: :aes_256_cbc, mac: :sha384, prf: :sha384}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 176::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :psk, cipher: :null, mac: :sha256, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 177::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :psk, cipher: :null, mac: :sha384, prf: :sha384}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 180::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_psk, cipher: :null, mac: :sha256, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 181::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_psk, cipher: :null, mac: :sha384, prf: :sha384}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 184::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa_psk, cipher: :null, mac: :sha256, prf: :default_prf}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 185::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa_psk, cipher: :null, mac: :sha384, prf: :sha384}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 51::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_psk, cipher: :rc4_128, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 52::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_psk, cipher: :"3des_ede_cbc", mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 53::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_psk, cipher: :aes_128_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 54::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_psk, cipher: :aes_256_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 55::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_psk, cipher: :aes_128_cbc, mac: :sha256, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 56::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_psk, cipher: :aes_256_cbc, mac: :sha384, prf: :sha384}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 58::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_psk, cipher: :null, mac: :sha256, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 59::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_psk, cipher: :null, mac: :sha384, prf: :sha384}
  end

  def suite_bin_to_map(<<208::size(8)-unsigned-big-integer, 1::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_psk, cipher: :aes_128_gcm, mac: :null, prf: :sha256}
  end

  def suite_bin_to_map(<<208::size(8)-unsigned-big-integer, 2::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_psk, cipher: :aes_256_gcm, mac: :null, prf: :sha384}
  end

  def suite_bin_to_map(<<208::size(8)-unsigned-big-integer, 5::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_psk, cipher: :aes_128_ccm, mac: :null, prf: :sha256}
  end

  def suite_bin_to_map(<<208::size(8)-unsigned-big-integer, 3::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_psk, cipher: :aes_128_ccm_8, mac: :null, prf: :sha256}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 26::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :srp_anon, cipher: :"3des_ede_cbc", mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 27::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :srp_rsa, cipher: :"3des_ede_cbc", mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 28::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :srp_dss, cipher: :"3des_ede_cbc", mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 29::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :srp_anon, cipher: :aes_128_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 30::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :srp_rsa, cipher: :aes_128_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 31::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :srp_dss, cipher: :aes_128_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 32::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :srp_anon, cipher: :aes_256_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 33::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :srp_rsa, cipher: :aes_256_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 34::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :srp_dss, cipher: :aes_256_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 1::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_ecdsa, cipher: :null, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 2::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_ecdsa, cipher: :rc4_128, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 3::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_ecdsa, cipher: :"3des_ede_cbc", mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 4::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_ecdsa, cipher: :aes_128_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 5::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_ecdsa, cipher: :aes_256_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 6::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_ecdsa, cipher: :null, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 7::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_ecdsa, cipher: :rc4_128, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 8::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_ecdsa, cipher: :"3des_ede_cbc", mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 9::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_ecdsa, cipher: :aes_128_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 10::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_ecdsa, cipher: :aes_256_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 11::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_rsa, cipher: :null, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 12::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_rsa, cipher: :rc4_128, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 13::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_rsa, cipher: :"3des_ede_cbc", mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 14::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_rsa, cipher: :aes_128_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 15::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_rsa, cipher: :aes_256_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 16::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_rsa, cipher: :null, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 17::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_rsa, cipher: :rc4_128, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 18::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_rsa, cipher: :"3des_ede_cbc", mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 19::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_rsa, cipher: :aes_128_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 20::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_rsa, cipher: :aes_256_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 21::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_anon, cipher: :null, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 22::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_anon, cipher: :rc4_128, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 23::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_anon, cipher: :"3des_ede_cbc", mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 24::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_anon, cipher: :aes_128_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 25::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_anon, cipher: :aes_256_cbc, mac: :sha, prf: :default_prf}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 35::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_ecdsa, cipher: :aes_128_cbc, mac: :sha256, prf: :sha256}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 36::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_ecdsa, cipher: :aes_256_cbc, mac: :sha384, prf: :sha384}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 37::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_ecdsa, cipher: :aes_128_cbc, mac: :sha256, prf: :sha256}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 38::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_ecdsa, cipher: :aes_256_cbc, mac: :sha384, prf: :sha384}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 39::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_rsa, cipher: :aes_128_cbc, mac: :sha256, prf: :sha256}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 40::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_rsa, cipher: :aes_256_cbc, mac: :sha384, prf: :sha384}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 41::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_rsa, cipher: :aes_128_cbc, mac: :sha256, prf: :sha256}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 42::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_rsa, cipher: :aes_256_cbc, mac: :sha384, prf: :sha384}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 156::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 157::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :rsa, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 158::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_rsa, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 159::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_rsa, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 160::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dh_rsa, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 161::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dh_rsa, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 162::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_dss, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 163::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_dss, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 164::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dh_dss, cipher: :aes_128_gcm, mac: :null, prf: :sha256}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 165::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dh_dss, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 166::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dh_anon, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<0::size(8)-unsigned-big-integer, 167::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dh_anon, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 43::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_ecdsa, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 44::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_ecdsa, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 45::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_ecdsa, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 46::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_ecdsa, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 47::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_rsa, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 48::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_rsa, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 49::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_rsa, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 50::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdh_rsa, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 164::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :psk, cipher: :aes_128_ccm, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 165::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :psk, cipher: :aes_256_ccm, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 166::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_psk, cipher: :aes_128_ccm, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 167::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_psk, cipher: :aes_256_ccm, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 168::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :psk, cipher: :aes_128_ccm_8, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 169::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :psk, cipher: :aes_256_ccm_8, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 170::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_psk, cipher: :aes_128_ccm_8, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<192::size(8)-unsigned-big-integer, 171::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_psk, cipher: :aes_256_ccm_8, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(%{key_exchange: :psk_dhe, cipher: :aes_256_ccm_8, mac: :aead, prf: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 171::size(8)-unsigned-big-integer>>
  end

  def suite_bin_to_map(<<204::size(8)-unsigned-big-integer, 168::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_rsa, cipher: :chacha20_poly1305, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<204::size(8)-unsigned-big-integer, 169::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :ecdhe_ecdsa, cipher: :chacha20_poly1305, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<204::size(8)-unsigned-big-integer, 170::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :dhe_rsa, cipher: :chacha20_poly1305, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<19::size(8)-unsigned-big-integer, 1::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :any, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<19::size(8)-unsigned-big-integer, 2::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :any, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}
  end

  def suite_bin_to_map(<<19::size(8)-unsigned-big-integer, 3::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :any, cipher: :chacha20_poly1305, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<19::size(8)-unsigned-big-integer, 4::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :any, cipher: :aes_128_ccm, mac: :aead, prf: :sha256}
  end

  def suite_bin_to_map(<<19::size(8)-unsigned-big-integer, 5::size(8)-unsigned-big-integer>>) do
    %{key_exchange: :any, cipher: :aes_128_ccm_8, mac: :aead, prf: :sha256}
  end

  def suite_legacy(bin) when is_binary(bin) do
    suite_legacy(suite_bin_to_map(bin))
  end

  def suite_legacy(%{key_exchange: keyExchange, cipher: cipher, mac: hash, prf: prf}) do
    case prf do
      :default_prf ->
        {keyExchange, cipher, hash}

      _ ->
        {keyExchange, cipher, hash, prf}
    end
  end

  def suite_map_to_bin(%{key_exchange: :rsa, cipher: :rc4_128, mac: :md5}) do
    <<0::size(8)-unsigned-big-integer, 4::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa, cipher: :rc4_128, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 5::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa, cipher: :des_cbc, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 9::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa, cipher: :"3des_ede_cbc", mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 10::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_dss, cipher: :des_cbc, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 18::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_dss, cipher: :"3des_ede_cbc", mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 19::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_rsa, cipher: :des_cbc, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 21::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_rsa, cipher: :"3des_ede_cbc", mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 22::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dh_anon, cipher: :rc4_128, mac: :md5}) do
    <<0::size(8)-unsigned-big-integer, 24::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dh_anon, cipher: :des_cbc, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 26::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dh_anon, cipher: :"3des_ede_cbc", mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 27::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa, cipher: :aes_128_cbc, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 47::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_dss, cipher: :aes_128_cbc, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 50::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_rsa, cipher: :aes_128_cbc, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 51::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dh_anon, cipher: :aes_128_cbc, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 52::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa, cipher: :aes_256_cbc, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 53::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_dss, cipher: :aes_256_cbc, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 56::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_rsa, cipher: :aes_256_cbc, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 57::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dh_anon, cipher: :aes_256_cbc, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 58::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa, cipher: :aes_128_cbc, mac: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 60::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa, cipher: :aes_256_cbc, mac: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 61::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_dss, cipher: :aes_128_cbc, mac: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 64::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_rsa, cipher: :aes_128_cbc, mac: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 103::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_dss, cipher: :aes_256_cbc, mac: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 106::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_rsa, cipher: :aes_256_cbc, mac: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 107::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dh_anon, cipher: :aes_128_cbc, mac: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 108::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dh_anon, cipher: :aes_256_cbc, mac: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 109::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :psk, cipher: :rc4_128, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 138::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :psk, cipher: :"3des_ede_cbc", mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 139::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :psk, cipher: :aes_128_cbc, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 140::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :psk, cipher: :aes_256_cbc, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 141::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_psk, cipher: :rc4_128, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 142::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_psk, cipher: :"3des_ede_cbc", mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 143::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_psk, cipher: :aes_128_cbc, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 144::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_psk, cipher: :aes_256_cbc, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 145::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa_psk, cipher: :rc4_128, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 146::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa_psk, cipher: :"3des_ede_cbc", mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 147::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa_psk, cipher: :aes_128_cbc, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 148::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa_psk, cipher: :aes_256_cbc, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 149::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :psk, cipher: :null, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 44::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_psk, cipher: :null, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 45::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa_psk, cipher: :null, mac: :sha}) do
    <<0::size(8)-unsigned-big-integer, 46::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :psk, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 168::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :psk, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}) do
    <<0::size(8)-unsigned-big-integer, 169::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_psk, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 170::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_psk, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}) do
    <<0::size(8)-unsigned-big-integer, 171::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa_psk, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 172::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa_psk, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}) do
    <<0::size(8)-unsigned-big-integer, 173::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :psk, cipher: :aes_128_cbc, mac: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 174::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :psk, cipher: :aes_256_cbc, mac: :sha384}) do
    <<0::size(8)-unsigned-big-integer, 175::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_psk, cipher: :aes_128_cbc, mac: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 178::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_psk, cipher: :aes_256_cbc, mac: :sha384}) do
    <<0::size(8)-unsigned-big-integer, 179::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa_psk, cipher: :aes_128_cbc, mac: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 182::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa_psk, cipher: :aes_256_cbc, mac: :sha384}) do
    <<0::size(8)-unsigned-big-integer, 183::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :psk, cipher: :null, mac: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 176::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :psk, cipher: :null, mac: :sha384}) do
    <<0::size(8)-unsigned-big-integer, 177::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_psk, cipher: :null, mac: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 180::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_psk, cipher: :null, mac: :sha384}) do
    <<0::size(8)-unsigned-big-integer, 181::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa_psk, cipher: :null, mac: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 184::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa_psk, cipher: :null, mac: :sha384}) do
    <<0::size(8)-unsigned-big-integer, 185::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_psk, cipher: :rc4_128, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 51::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_psk, cipher: :"3des_ede_cbc", mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 52::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_psk, cipher: :aes_128_cbc, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 53::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_psk, cipher: :aes_256_cbc, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 54::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_psk, cipher: :aes_128_cbc, mac: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 55::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_psk, cipher: :aes_256_cbc, mac: :sha384}) do
    <<192::size(8)-unsigned-big-integer, 56::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_psk, cipher: :null, mac: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 58::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_psk, cipher: :null, mac: :sha384}) do
    <<192::size(8)-unsigned-big-integer, 59::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_psk, cipher: :aes_128_gcm, mac: :null, prf: :sha256}) do
    <<208::size(8)-unsigned-big-integer, 1::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_psk, cipher: :aes_256_gcm, mac: :null, prf: :sha384}) do
    <<208::size(8)-unsigned-big-integer, 2::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{
        key_exchange: :ecdhe_psk,
        cipher: :aes_128_ccm_8,
        mac: :null,
        prf: :sha256
      }) do
    <<208::size(8)-unsigned-big-integer, 3::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_psk, cipher: :aes_128_ccm, mac: :null, prf: :sha256}) do
    <<208::size(8)-unsigned-big-integer, 5::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :srp_anon, cipher: :"3des_ede_cbc", mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 26::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :srp_rsa, cipher: :"3des_ede_cbc", mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 27::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :srp_dss, cipher: :"3des_ede_cbc", mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 28::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :srp_anon, cipher: :aes_128_cbc, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 29::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :srp_rsa, cipher: :aes_128_cbc, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 30::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :srp_dss, cipher: :aes_128_cbc, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 31::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :srp_anon, cipher: :aes_256_cbc, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 32::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :srp_rsa, cipher: :aes_256_cbc, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 33::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :srp_dss, cipher: :aes_256_cbc, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 34::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdh_ecdsa, cipher: :null, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 1::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdh_ecdsa, cipher: :rc4_128, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 2::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdh_ecdsa, cipher: :"3des_ede_cbc", mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 3::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdh_ecdsa, cipher: :aes_128_cbc, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 4::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdh_ecdsa, cipher: :aes_256_cbc, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 5::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_ecdsa, cipher: :null, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 6::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_ecdsa, cipher: :rc4_128, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 7::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_ecdsa, cipher: :"3des_ede_cbc", mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 8::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_ecdsa, cipher: :aes_128_cbc, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 9::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_ecdsa, cipher: :aes_256_cbc, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 10::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdh_rsa, cipher: :null, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 11::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdh_rsa, cipher: :rc4_128, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 12::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdh_rsa, cipher: :"3des_ede_cbc", mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 13::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdh_rsa, cipher: :aes_128_cbc, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 14::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdh_rsa, cipher: :aes_256_cbc, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 15::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_rsa, cipher: :null, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 16::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_rsa, cipher: :rc4_128, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 17::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_rsa, cipher: :"3des_ede_cbc", mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 18::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_rsa, cipher: :aes_128_cbc, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 19::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_rsa, cipher: :aes_256_cbc, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 20::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdh_anon, cipher: :null, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 21::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdh_anon, cipher: :rc4_128, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 22::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdh_anon, cipher: :"3des_ede_cbc", mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 23::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdh_anon, cipher: :aes_128_cbc, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 24::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdh_anon, cipher: :aes_256_cbc, mac: :sha}) do
    <<192::size(8)-unsigned-big-integer, 25::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{
        key_exchange: :ecdhe_ecdsa,
        cipher: :aes_128_cbc,
        mac: :sha256,
        prf: :sha256
      }) do
    <<192::size(8)-unsigned-big-integer, 35::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{
        key_exchange: :ecdhe_ecdsa,
        cipher: :aes_256_cbc,
        mac: :sha384,
        prf: :sha384
      }) do
    <<192::size(8)-unsigned-big-integer, 36::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{
        key_exchange: :ecdh_ecdsa,
        cipher: :aes_128_cbc,
        mac: :sha256,
        prf: :sha256
      }) do
    <<192::size(8)-unsigned-big-integer, 37::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{
        key_exchange: :ecdh_ecdsa,
        cipher: :aes_256_cbc,
        mac: :sha384,
        prf: :sha384
      }) do
    <<192::size(8)-unsigned-big-integer, 38::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{
        key_exchange: :ecdhe_rsa,
        cipher: :aes_128_cbc,
        mac: :sha256,
        prf: :sha256
      }) do
    <<192::size(8)-unsigned-big-integer, 39::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{
        key_exchange: :ecdhe_rsa,
        cipher: :aes_256_cbc,
        mac: :sha384,
        prf: :sha384
      }) do
    <<192::size(8)-unsigned-big-integer, 40::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{
        key_exchange: :ecdh_rsa,
        cipher: :aes_128_cbc,
        mac: :sha256,
        prf: :sha256
      }) do
    <<192::size(8)-unsigned-big-integer, 41::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{
        key_exchange: :ecdh_rsa,
        cipher: :aes_256_cbc,
        mac: :sha384,
        prf: :sha384
      }) do
    <<192::size(8)-unsigned-big-integer, 42::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 156::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}) do
    <<0::size(8)-unsigned-big-integer, 157::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_rsa, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 158::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_rsa, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}) do
    <<0::size(8)-unsigned-big-integer, 159::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dh_rsa, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 160::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dh_rsa, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}) do
    <<0::size(8)-unsigned-big-integer, 161::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_dss, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 162::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_dss, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}) do
    <<0::size(8)-unsigned-big-integer, 163::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dh_dss, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 164::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dh_dss, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}) do
    <<0::size(8)-unsigned-big-integer, 165::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dh_anon, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}) do
    <<0::size(8)-unsigned-big-integer, 166::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dh_anon, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}) do
    <<0::size(8)-unsigned-big-integer, 167::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{
        key_exchange: :ecdhe_ecdsa,
        cipher: :aes_128_gcm,
        mac: :aead,
        prf: :sha256
      }) do
    <<192::size(8)-unsigned-big-integer, 43::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{
        key_exchange: :ecdhe_ecdsa,
        cipher: :aes_256_gcm,
        mac: :aead,
        prf: :sha384
      }) do
    <<192::size(8)-unsigned-big-integer, 44::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{
        key_exchange: :ecdh_ecdsa,
        cipher: :aes_128_gcm,
        mac: :aead,
        prf: :sha256
      }) do
    <<192::size(8)-unsigned-big-integer, 45::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{
        key_exchange: :ecdh_ecdsa,
        cipher: :aes_256_gcm,
        mac: :aead,
        prf: :sha384
      }) do
    <<192::size(8)-unsigned-big-integer, 46::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_rsa, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 47::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdhe_rsa, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}) do
    <<192::size(8)-unsigned-big-integer, 48::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdh_rsa, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 49::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :ecdh_rsa, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}) do
    <<192::size(8)-unsigned-big-integer, 50::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{
        key_exchange: :ecdhe_rsa,
        cipher: :chacha20_poly1305,
        mac: :aead,
        prf: :sha256
      }) do
    <<204::size(8)-unsigned-big-integer, 168::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{
        key_exchange: :ecdhe_ecdsa,
        cipher: :chacha20_poly1305,
        mac: :aead,
        prf: :sha256
      }) do
    <<204::size(8)-unsigned-big-integer, 169::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{
        key_exchange: :dhe_rsa,
        cipher: :chacha20_poly1305,
        mac: :aead,
        prf: :sha256
      }) do
    <<204::size(8)-unsigned-big-integer, 170::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :psk, cipher: :aes_128_ccm, mac: :aead, prf: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 164::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :psk, cipher: :aes_256_ccm, mac: :aead, prf: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 165::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_psk, cipher: :aes_128_ccm, mac: :aead, prf: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 166::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_psk, cipher: :aes_256_ccm, mac: :aead, prf: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 167::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa, cipher: :aes_128_ccm, mac: :aead, prf: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 156::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa, cipher: :aes_256_ccm, mac: :aead, prf: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 157::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_rsa, cipher: :aes_128_ccm, mac: :aead, prf: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 159::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_rsa, cipher: :aes_256_ccm, mac: :aead, prf: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 158::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :psk, cipher: :aes_128_ccm_8, mac: :aead, prf: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 168::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :psk, cipher: :aes_256_ccm_8, mac: :aead, prf: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 169::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_psk, cipher: :aes_128_ccm_8, mac: :aead, prf: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 170::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_psk, cipher: :aes_256_ccm_8, mac: :aead, prf: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 171::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa, cipher: :aes_128_ccm_8, mac: :aead, prf: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 161::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :rsa, cipher: :aes_256_ccm_8, mac: :aead, prf: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 160::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_rsa, cipher: :aes_128_ccm_8, mac: :aead, prf: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 162::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :dhe_rsa, cipher: :aes_256_ccm_8, mac: :aead, prf: :sha256}) do
    <<192::size(8)-unsigned-big-integer, 163::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :any, cipher: :aes_128_gcm, mac: :aead, prf: :sha256}) do
    <<19::size(8)-unsigned-big-integer, 1::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :any, cipher: :aes_256_gcm, mac: :aead, prf: :sha384}) do
    <<19::size(8)-unsigned-big-integer, 2::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :any, cipher: :chacha20_poly1305, mac: :aead, prf: :sha256}) do
    <<19::size(8)-unsigned-big-integer, 3::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :any, cipher: :aes_128_ccm, mac: :aead, prf: :sha256}) do
    <<19::size(8)-unsigned-big-integer, 4::size(8)-unsigned-big-integer>>
  end

  def suite_map_to_bin(%{key_exchange: :any, cipher: :aes_128_ccm_8, mac: :aead, prf: :sha256}) do
    <<19::size(8)-unsigned-big-integer, 5::size(8)-unsigned-big-integer>>
  end

  defp tls_1_3_suite_str_to_map(cipherStr) do
    {cipher, mac, prf} = cipher_str_to_algs(:any, cipherStr, '')
    %{key_exchange: :any, mac: mac, cipher: cipher, prf: prf}
  end

  defp pre_tls_1_3_suite_str_to_map(kexStr, rest) do
    kex = algo_str_to_atom(kexStr)
    [cipherStr, algStr] = :string.split(rest, '_', :trailing)
    {cipher, mac, prf} = cipher_str_to_algs(kex, cipherStr, algStr)
    %{key_exchange: kex, mac: mac, cipher: cipher, prf: prf}
  end

  defp cipher_str_to_algs(_, cipherStr, 'CCM' = end__) do
    cipher = algo_str_to_atom(cipherStr ++ '_' ++ end__)
    {cipher, :aead, :sha256}
  end

  defp cipher_str_to_algs(_, cipherStr, '8' = end__) do
    cipher = algo_str_to_atom(cipherStr ++ '_' ++ end__)
    {cipher, :aead, :sha256}
  end

  defp cipher_str_to_algs(_, cipherStr, 'CHACHA20_POLY1305' = end__) do
    cipher = algo_str_to_atom(cipherStr ++ '_' ++ end__)
    {cipher, :aead, :sha256}
  end

  defp cipher_str_to_algs(_, cipherStr0, '') do
    [cipherStr, algStr] = :string.split(cipherStr0, '_', :trailing)
    hash = algo_str_to_atom(algStr)
    cipher = algo_str_to_atom(cipherStr)
    {cipher, :aead, hash}
  end

  defp cipher_str_to_algs(kex, cipherStr, hashStr) do
    hash = algo_str_to_atom(hashStr)
    cipher = algo_str_to_atom(cipherStr)

    case is_aead_cipher(cipherStr) do
      true ->
        {cipher, :aead, hash}

      false ->
        {cipher, hash, default_prf(kex, hash)}
    end
  end

  defp default_prf(_, :md5) do
    :default_prf
  end

  defp default_prf(_, :sha) do
    :default_prf
  end

  defp default_prf(:ecdhe_ecdsa, :sha256) do
    :sha256
  end

  defp default_prf(:ecdhe_rsa, :sha256) do
    :sha256
  end

  defp default_prf(:dhe_rsa, :sha256) do
    :default_prf
  end

  defp default_prf(:dhe_dss, :sha256) do
    :default_prf
  end

  defp default_prf(:rsa, :sha256) do
    :default_prf
  end

  defp default_prf(:rsa_psk, :sha256) do
    :default_prf
  end

  defp default_prf(_, hash) do
    hash
  end

  defp is_aead_cipher('CHACHA20_POLY1305') do
    true
  end

  defp is_aead_cipher(cipherStr) do
    [_, rest] = :string.split(cipherStr, '_', :trailing)
    rest == 'GCM' or rest == 'CCM' or rest == '8'
  end

  defp openssl_is_aead_cipher('CHACHA20-POLY1305') do
    true
  end

  defp openssl_is_aead_cipher(cipherStr) do
    case :string.split(cipherStr, '-', :trailing) do
      [_, rest] ->
        rest == 'GCM' or rest == 'CCM' or rest == '8'

      [_] ->
        false
    end
  end

  defp algo_str_to_atom('SRP_SHA_DSS') do
    :srp_dss
  end

  defp algo_str_to_atom(algoStr) do
    :erlang.list_to_existing_atom(:string.to_lower(algoStr))
  end

  defp openssl_cipher_name(_, '3DES_EDE_CBC' ++ _) do
    'DES-CBC3'
  end

  defp openssl_cipher_name(kex, 'AES_128_CBC' ++ _ = cipherStr)
       when kex == :rsa or
              kex == :dhe_rsa or
              kex == :ecdhe_rsa or
              kex == :ecdhe_ecdsa do
    openssl_name_concat(cipherStr)
  end

  defp openssl_cipher_name(kex, 'AES_256_CBC' ++ _ = cipherStr)
       when kex == :rsa or
              kex == :dhe_rsa or
              kex == :ecdhe_rsa or
              kex == :ecdhe_ecdsa do
    openssl_name_concat(cipherStr)
  end

  defp openssl_cipher_name(kex, 'AES_128_CBC' ++ _ = cipherStr)
       when kex == :srp or
              kex == :srp_rsa do
    :lists.append(:string.replace(cipherStr, '_', '-', :all))
  end

  defp openssl_cipher_name(kex, 'AES_256_CBC' ++ _ = cipherStr)
       when kex == :srp or
              kex == :srp_rsa do
    :lists.append(:string.replace(cipherStr, '_', '-', :all))
  end

  defp openssl_cipher_name(_, 'AES_128_CBC' ++ _ = cipherStr) do
    openssl_name_concat(cipherStr) ++ '-CBC'
  end

  defp openssl_cipher_name(_, 'AES_256_CBC' ++ _ = cipherStr) do
    openssl_name_concat(cipherStr) ++ '-CBC'
  end

  defp openssl_cipher_name(_, 'AES_128_GCM' ++ _ = cipherStr) do
    openssl_name_concat(cipherStr) ++ '-GCM'
  end

  defp openssl_cipher_name(_, 'AES_256_GCM' ++ _ = cipherStr) do
    openssl_name_concat(cipherStr) ++ '-GCM'
  end

  defp openssl_cipher_name(_, 'RC4' ++ _) do
    'RC4'
  end

  defp openssl_cipher_name(_, cipherStr) do
    :lists.append(:string.replace(cipherStr, '_', '-', :all))
  end

  defp openssl_suite_start(kex) do
    case openssl_kex_name(kex) do
      '' ->
        ''

      name ->
        name ++ '-'
    end
  end

  defp openssl_kex_name('RSA') do
    ''
  end

  defp openssl_kex_name('DHE_RSA') do
    'EDH-RSA'
  end

  defp openssl_kex_name(kex) do
    :lists.append(:string.replace(kex, '_', '-', :all))
  end

  defp kex_name_from_openssl(kex) do
    case :lists.append(:string.replace(kex, '-', '_', :all)) do
      'EDH_RSA' ->
        'DHE_RSA'

      str ->
        str
    end
  end

  defp cipher_name_from_openssl('AES128') do
    'AES_128_CBC'
  end

  defp cipher_name_from_openssl('AES256') do
    'AES_256_CBC'
  end

  defp cipher_name_from_openssl('AES128-CBC') do
    'AES_128_CBC'
  end

  defp cipher_name_from_openssl('AES256-CBC') do
    'AES_256_CBC'
  end

  defp cipher_name_from_openssl('AES-128-CBC') do
    'AES_128_CBC'
  end

  defp cipher_name_from_openssl('AES-256-CBC') do
    'AES_256_CBC'
  end

  defp cipher_name_from_openssl('AES128-GCM') do
    'AES_128_GCM'
  end

  defp cipher_name_from_openssl('AES256-GCM') do
    'AES_256_GCM'
  end

  defp cipher_name_from_openssl('DES-CBC') do
    'DES_CBC'
  end

  defp cipher_name_from_openssl('DES-CBC3') do
    '3DES_EDE_CBC'
  end

  defp cipher_name_from_openssl('RC4') do
    'RC4_128'
  end

  defp cipher_name_from_openssl(str) do
    str
  end

  defp openssl_name_concat(str0) do
    [str, _] = :string.split(str0, '_', :trailing)
    [part1, part2] = :string.split(str, '_', :trailing)
    part1 ++ part2
  end

  defp suite_openssl_str_to_map(kex0, rest) do
    kex = algo_str_to_atom(kex_name_from_openssl(kex0))
    [cipherStr, algStr] = :string.split(rest, '-', :trailing)
    {cipher, mac, prf} = openssl_cipher_str_to_algs(kex, cipherStr, algStr)
    %{key_exchange: kex, mac: mac, cipher: cipher, prf: prf}
  end

  defp openssl_cipher_str_to_algs(_, cipherStr, 'CCM' = end__) do
    cipher = algo_str_to_atom(cipherStr ++ '_' ++ end__)
    {cipher, :aead, :sha256}
  end

  defp openssl_cipher_str_to_algs(_, cipherStr, '8' = end__) do
    cipher = algo_str_to_atom(cipherStr ++ '_' ++ end__)
    {cipher, :aead, :sha256}
  end

  defp openssl_cipher_str_to_algs(_, cipherStr, 'POLY1305' = end__) do
    cipher = algo_str_to_atom(cipherStr ++ '_' ++ end__)
    {cipher, :aead, :sha256}
  end

  defp openssl_cipher_str_to_algs(kex, cipherStr, hashStr) do
    hash = algo_str_to_atom(hashStr)
    cipher = algo_str_to_atom(cipher_name_from_openssl(cipherStr))

    case openssl_is_aead_cipher(cipherStr) do
      true ->
        {cipher, :aead, hash}

      false ->
        {cipher, hash, default_prf(kex, hash)}
    end
  end
end
