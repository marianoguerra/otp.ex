defmodule :m_tls_v1 do
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

  def derive_secret(secret, label, messages, algo) do
    hash = :crypto.hash(mac_algo(algo), messages)
    hkdf_expand_label(secret, label, hash, :ssl_cipher.hash_size(algo), algo)
  end

  def hkdf_expand_label(secret, label0, context, length, algo) do
    hkdfLabel = create_info(label0, context, length)
    hkdf_expand(secret, hkdfLabel, length, algo)
  end

  def create_info(label0, context0, length) do
    label1 = <<"tls13 "::binary, label0::binary>>
    labelLen = :erlang.size(label1)
    label = <<labelLen::size(8)-unsigned-big-integer, label1::binary>>
    contextLen = :erlang.size(context0)
    context = <<contextLen::size(8)-unsigned-big-integer, context0::binary>>
    content = <<label::binary, context::binary>>
    <<length::size(16)-unsigned-big-integer, content::binary>>
  end

  def hkdf_extract(macAlg, salt, keyingMaterial) do
    hmac_hash(macAlg, salt, keyingMaterial)
  end

  def hkdf_expand(pseudoRandKey, contextInfo, length, algo) do
    iterations = :erlang.ceil(length / :ssl_cipher.hash_size(algo))
    hkdf_expand(algo, pseudoRandKey, contextInfo, length, 1, iterations, <<>>, <<>>)
  end

  def transcript_hash(messages, algo) do
    :crypto.hash(mac_algo(algo), messages)
  end

  def master_secret(prfAlgo, preMasterSecret, clientRandom, serverRandom) do
    prf(prfAlgo, preMasterSecret, "master secret", [clientRandom, serverRandom], 48)
  end

  def finished(role, version, prfAlgo, masterSecret, handshake)
      when version == 1 or version == 2 or prfAlgo == 4711 do
    mD5 = :crypto.hash(:md5, handshake)
    sHA = :crypto.hash(:sha, handshake)
    prf(4711, masterSecret, finished_label(role), [mD5, sHA], 12)
  end

  def finished(role, version, prfAlgo, masterSecret, handshake)
      when version == 3 do
    hash = :crypto.hash(mac_algo(prfAlgo), handshake)
    prf(prfAlgo, masterSecret, finished_label(role), hash, 12)
  end

  def certificate_verify(:md5sha, _Version, handshake) do
    mD5 = :crypto.hash(:md5, handshake)
    sHA = :crypto.hash(:sha, handshake)
    <<mD5::binary, sHA::binary>>
  end

  def certificate_verify(hashAlgo, _Version, handshake) do
    :crypto.hash(hashAlgo, handshake)
  end

  def setup_keys(
        version,
        _PrfAlgo,
        masterSecret,
        serverRandom,
        clientRandom,
        hashSize,
        keyMatLen,
        iVSize
      )
      when version == 1 do
    wantedLength = 2 * (hashSize + keyMatLen + iVSize)

    keyBlock =
      prf(4711, masterSecret, 'key expansion', [serverRandom, clientRandom], wantedLength)

    <<clientWriteMacSecret::size(hashSize)-binary, serverWriteMacSecret::size(hashSize)-binary,
      clientWriteKey::size(keyMatLen)-binary, serverWriteKey::size(keyMatLen)-binary,
      clientIV::size(iVSize)-binary, serverIV::size(iVSize)-binary>> = keyBlock

    {clientWriteMacSecret, serverWriteMacSecret, clientWriteKey, serverWriteKey, clientIV,
     serverIV}
  end

  def setup_keys(
        version,
        _PrfAlgo,
        masterSecret,
        serverRandom,
        clientRandom,
        hashSize,
        keyMatLen,
        iVSize
      )
      when version == 2 do
    wantedLength = 2 * (hashSize + keyMatLen + iVSize)

    keyBlock =
      prf(4711, masterSecret, 'key expansion', [serverRandom, clientRandom], wantedLength)

    <<clientWriteMacSecret::size(hashSize)-binary, serverWriteMacSecret::size(hashSize)-binary,
      clientWriteKey::size(keyMatLen)-binary, serverWriteKey::size(keyMatLen)-binary,
      clientIV::size(iVSize)-binary, serverIV::size(iVSize)-binary>> = keyBlock

    {clientWriteMacSecret, serverWriteMacSecret, clientWriteKey, serverWriteKey, clientIV,
     serverIV}
  end

  def setup_keys(
        version,
        prfAlgo,
        masterSecret,
        serverRandom,
        clientRandom,
        hashSize,
        keyMatLen,
        iVSize
      )
      when version == 3 or version == 4 do
    wantedLength = 2 * (hashSize + keyMatLen + iVSize)

    keyBlock =
      prf(prfAlgo, masterSecret, 'key expansion', [serverRandom, clientRandom], wantedLength)

    <<clientWriteMacSecret::size(hashSize)-binary, serverWriteMacSecret::size(hashSize)-binary,
      clientWriteKey::size(keyMatLen)-binary, serverWriteKey::size(keyMatLen)-binary,
      clientIV::size(iVSize)-binary, serverIV::size(iVSize)-binary>> = keyBlock

    {clientWriteMacSecret, serverWriteMacSecret, clientWriteKey, serverWriteKey, clientIV,
     serverIV}
  end

  def key_schedule(:early_secret, algo, {:psk, pSK}) do
    len = :ssl_cipher.hash_size(algo)

    salt =
      :binary.copy(
        <<0::size(8)-unsigned-big-integer>>,
        len
      )

    {:early_secret, hkdf_extract(algo, salt, pSK)}
  end

  def key_schedule(:master_secret, algo, {:handshake_secret, secret}) do
    len = :ssl_cipher.hash_size(algo)

    iKM =
      :binary.copy(
        <<0::size(8)-unsigned-big-integer>>,
        len
      )

    salt = derive_secret(secret, "derived", <<>>, algo)
    {:master_secret, hkdf_extract(algo, salt, iKM)}
  end

  def key_schedule(:handshake_secret, algo, iKM, {:early_secret, secret}) do
    salt = derive_secret(secret, "derived", <<>>, algo)
    {:handshake_secret, hkdf_extract(algo, salt, iKM)}
  end

  def external_binder_key(algo, {:early_secret, secret}) do
    derive_secret(secret, "ext binder", <<>>, algo)
  end

  def resumption_binder_key(algo, {:early_secret, secret}) do
    derive_secret(secret, "res binder", <<>>, algo)
  end

  def client_early_traffic_secret(algo, {:early_secret, secret}, m) do
    derive_secret(secret, "c e traffic", m, algo)
  end

  def early_exporter_master_secret(algo, {:early_secret, secret}, m) do
    derive_secret(secret, "e exp master", m, algo)
  end

  def client_handshake_traffic_secret(algo, {:handshake_secret, secret}, m) do
    derive_secret(secret, "c hs traffic", m, algo)
  end

  def server_handshake_traffic_secret(algo, {:handshake_secret, secret}, m) do
    derive_secret(secret, "s hs traffic", m, algo)
  end

  def client_application_traffic_secret_0(algo, {:master_secret, secret}, m) do
    derive_secret(secret, "c ap traffic", m, algo)
  end

  def server_application_traffic_secret_0(algo, {:master_secret, secret}, m) do
    derive_secret(secret, "s ap traffic", m, algo)
  end

  def exporter_master_secret(algo, {:master_secret, secret}, m) do
    derive_secret(secret, "exp master", m, algo)
  end

  def resumption_master_secret(algo, {:master_secret, secret}, m) do
    derive_secret(secret, "res master", m, algo)
  end

  def finished_key(baseKey, algo) do
    :ssl_cipher.hash_size(algo)
    hkdf_expand_label(baseKey, "finished", <<>>, :ssl_cipher.hash_size(algo), algo)
  end

  def finished_verify_data(finishedKey, hKDFAlgo, messages) do
    context = :lists.reverse(messages)
    tHash = :tls_v1.transcript_hash(context, hKDFAlgo)
    :tls_v1.hmac_hash(hKDFAlgo, finishedKey, tHash)
  end

  def pre_shared_key(rMS, nonce, algo) do
    :ssl_cipher.hash_size(algo)
    hkdf_expand_label(rMS, "resumption", nonce, :ssl_cipher.hash_size(algo), algo)
  end

  def update_traffic_secret(algo, secret) do
    hkdf_expand_label(secret, "traffic upd", <<>>, :ssl_cipher.hash_size(algo), algo)
  end

  def calculate_traffic_keys(hKDFAlgo, cipher, secret) do
    key = hkdf_expand_label(secret, "key", <<>>, :ssl_cipher.key_material(cipher), hKDFAlgo)
    iV = hkdf_expand_label(secret, "iv", <<>>, 12, hKDFAlgo)
    {key, iV}
  end

  def mac_hash(method, mac_write_secret, seq_num, type, {major, minor}, length, fragment) do
    mac =
      hmac_hash(method, mac_write_secret, [
        <<seq_num::size(64)-unsigned-big-integer, type::size(8)-unsigned-big-integer,
          major::size(8)-unsigned-big-integer, minor::size(8)-unsigned-big-integer,
          length::size(16)-unsigned-big-integer>>,
        fragment
      ])

    mac
  end

  def suites(minor) when minor == 1 or minor == 2 do
    [
      <<192::size(8)-unsigned-big-integer, 10::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 20::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 57::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 56::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 5::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 15::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 9::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 19::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 51::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 50::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 4::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 14::size(8)-unsigned-big-integer>>
    ]
  end

  def suites(3) do
    exclusive_suites(3) ++ suites(2)
  end

  def suites(4) do
    exclusive_suites(4) ++ suites(3)
  end

  def exclusive_suites(4) do
    [
      <<19::size(8)-unsigned-big-integer, 2::size(8)-unsigned-big-integer>>,
      <<19::size(8)-unsigned-big-integer, 1::size(8)-unsigned-big-integer>>,
      <<19::size(8)-unsigned-big-integer, 3::size(8)-unsigned-big-integer>>,
      <<19::size(8)-unsigned-big-integer, 4::size(8)-unsigned-big-integer>>,
      <<19::size(8)-unsigned-big-integer, 5::size(8)-unsigned-big-integer>>
    ]
  end

  def exclusive_suites(3) do
    [
      <<192::size(8)-unsigned-big-integer, 44::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 48::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 36::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 40::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 46::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 50::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 38::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 42::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 159::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 163::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 107::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 106::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 43::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 47::size(8)-unsigned-big-integer>>,
      <<204::size(8)-unsigned-big-integer, 169::size(8)-unsigned-big-integer>>,
      <<204::size(8)-unsigned-big-integer, 168::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 35::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 39::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 45::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 49::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 37::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 41::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 158::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 162::size(8)-unsigned-big-integer>>,
      <<204::size(8)-unsigned-big-integer, 170::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 103::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 64::size(8)-unsigned-big-integer>>
    ]
  end

  def exclusive_suites(minor) when minor == 1 or minor == 2 do
    [
      <<192::size(8)-unsigned-big-integer, 10::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 20::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 57::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 56::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 5::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 15::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 9::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 19::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 51::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 50::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 4::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 14::size(8)-unsigned-big-integer>>
    ]
  end

  def signature_algs({3, 4}, hashSigns) do
    signature_algs({3, 3}, hashSigns)
  end

  def signature_algs({3, 3}, hashSigns) do
    cryptoSupports = :crypto.supports()
    hashes = :proplists.get_value(:hashs, cryptoSupports)

    pubKeys =
      :proplists.get_value(
        :public_keys,
        cryptoSupports
      )

    supported =
      :lists.foldl(
        fn
          {hash, :dsa = sign} = alg, acc ->
            case :proplists.get_bool(
                   :dss,
                   pubKeys
                 ) and
                   :proplists.get_bool(
                     hash,
                     hashes
                   ) and
                   is_pair(
                     hash,
                     sign,
                     hashes
                   ) do
              true ->
                [alg | acc]

              false ->
                acc
            end

          {hash, sign} = alg, acc ->
            case :proplists.get_bool(
                   sign,
                   pubKeys
                 ) and
                   :proplists.get_bool(
                     hash,
                     hashes
                   ) and
                   is_pair(
                     hash,
                     sign,
                     hashes
                   ) do
              true ->
                [alg | acc]

              false ->
                acc
            end
        end,
        [],
        hashSigns
      )

    :lists.reverse(supported)
  end

  def default_signature_algs({3, 4} = version) do
    default_signature_schemes(version) ++ default_signature_algs({3, 3})
  end

  def default_signature_algs({3, 3} = version) do
    default = [
      {:sha512, :ecdsa},
      {:sha512, :rsa},
      {:sha384, :ecdsa},
      {:sha384, :rsa},
      {:sha256, :ecdsa},
      {:sha256, :rsa},
      {:sha224, :ecdsa},
      {:sha224, :rsa},
      {:sha, :ecdsa},
      {:sha, :rsa},
      {:sha, :dsa}
    ]

    signature_algs(version, default)
  end

  def default_signature_algs(_) do
    :undefined
  end

  def signature_schemes(version, signatureSchemes)
      when is_tuple(version) and version >= {3, 3} do
    cryptoSupports = :crypto.supports()
    hashes = :proplists.get_value(:hashs, cryptoSupports)

    pubKeys =
      :proplists.get_value(
        :public_keys,
        cryptoSupports
      )

    curves = :proplists.get_value(:curves, cryptoSupports)

    rSAPSSSupported =
      :lists.member(
        :rsa_pkcs1_pss_padding,
        :proplists.get_value(
          :rsa_opts,
          cryptoSupports
        )
      )

    fun = fn
      scheme, acc when is_atom(scheme) ->
        {hash0, sign0, curve} = :ssl_cipher.scheme_to_components(scheme)

        sign =
          case sign0 do
            :rsa_pkcs1 ->
              :rsa

            :rsa_pss_rsae when rSAPSSSupported ->
              :rsa

            :rsa_pss_pss when rSAPSSSupported ->
              :rsa

            s ->
              s
          end

        hash =
          case hash0 do
            :sha1 ->
              :sha

            h ->
              h
          end

        case :proplists.get_bool(
               sign,
               pubKeys
             ) and
               :proplists.get_bool(
                 hash,
                 hashes
               ) and
               (curve === :undefined or
                  :proplists.get_bool(
                    curve,
                    curves
                  )) and
               is_pair(
                 hash,
                 sign,
                 hashes
               ) do
          true ->
            [scheme | acc]

          false ->
            acc
        end

      {hash, :dsa = sign} = alg, acc ->
        case :proplists.get_bool(
               :dss,
               pubKeys
             ) and
               :proplists.get_bool(
                 hash,
                 hashes
               ) and
               is_pair(
                 hash,
                 sign,
                 hashes
               ) do
          true ->
            [alg | acc]

          false ->
            acc
        end

      {hash, sign} = alg, acc ->
        case :proplists.get_bool(
               sign,
               pubKeys
             ) and
               :proplists.get_bool(
                 hash,
                 hashes
               ) and
               is_pair(
                 hash,
                 sign,
                 hashes
               ) do
          true ->
            [alg | acc]

          false ->
            acc
        end
    end

    supported = :lists.foldl(fun, [], signatureSchemes)
    :lists.reverse(supported)
  end

  def signature_schemes(_, _) do
    []
  end

  def default_signature_schemes(version) do
    default = [
      :ecdsa_secp521r1_sha512,
      :ecdsa_secp384r1_sha384,
      :ecdsa_secp256r1_sha256,
      :rsa_pss_pss_sha512,
      :rsa_pss_pss_sha384,
      :rsa_pss_pss_sha256,
      :rsa_pss_rsae_sha512,
      :rsa_pss_rsae_sha384,
      :rsa_pss_rsae_sha256,
      :rsa_pkcs1_sha512,
      :rsa_pkcs1_sha384,
      :rsa_pkcs1_sha256,
      :ecdsa_sha1,
      :rsa_pkcs1_sha1
    ]

    signature_schemes(version, default)
  end

  defp hkdf_expand(algo, pseudoRandKey, contextInfo, length, n, n, prev, acc) do
    keyingmaterial =
      hmac_hash(
        algo,
        pseudoRandKey,
        <<prev::binary, contextInfo::binary, n::size(8)-unsigned-big-integer>>
      )

    :binary.part(
      <<acc::binary, keyingmaterial::binary>>,
      {0, length}
    )
  end

  defp hkdf_expand(algo, pseudoRandKey, contextInfo, length, m, n, prev, acc) do
    keyingmaterial =
      hmac_hash(
        algo,
        pseudoRandKey,
        <<prev::binary, contextInfo::binary, m::size(8)-unsigned-big-integer>>
      )

    hkdf_expand(
      algo,
      pseudoRandKey,
      contextInfo,
      length,
      m + 1,
      n,
      keyingmaterial,
      <<acc::binary, keyingmaterial::binary>>
    )
  end

  def hmac_hash(0, _, _) do
    <<>>
  end

  def hmac_hash(alg, key, value) do
    :crypto.mac(:hmac, mac_algo(alg), key, value)
  end

  defp mac_algo(alg) when is_atom(alg) do
    alg
  end

  defp mac_algo(1) do
    :md5
  end

  defp mac_algo(2) do
    :sha
  end

  defp mac_algo(4) do
    :sha256
  end

  defp mac_algo(5) do
    :sha384
  end

  defp mac_algo(6) do
    :sha512
  end

  defp p_hash(secret, seed, wantedLength, method) do
    p_hash(secret, seed, wantedLength, method, 0, [])
  end

  defp p_hash(_Secret, _Seed, wantedLength, _Method, _N, [])
       when wantedLength <= 0 do
    []
  end

  defp p_hash(_Secret, _Seed, wantedLength, _Method, _N, [last | acc])
       when wantedLength <= 0 do
    keep = byte_size(last) + wantedLength
    <<b::size(keep)-binary, _::binary>> = last
    :erlang.list_to_binary(:lists.reverse(acc, [b]))
  end

  defp p_hash(secret, seed, wantedLength, method, n, acc) do
    n1 = n + 1
    bin = hmac_hash(method, secret, [a(n1, secret, seed, method), seed])
    p_hash(secret, seed, wantedLength - byte_size(bin), method, n1, [bin | acc])
  end

  defp a(0, _Secret, seed, _Method) do
    seed
  end

  defp a(n, secret, seed0, method) do
    seed = hmac_hash(method, secret, seed0)
    a(n - 1, secret, seed, method)
  end

  defp split_secret(binSecret) do
    length = byte_size(binSecret)
    div = div(length, 2)
    evenLength = length - div
    <<secret1::size(evenLength)-binary, _::binary>> = binSecret
    <<_::size(div)-binary, secret2::size(evenLength)-binary>> = binSecret
    {secret1, secret2}
  end

  def prf(4711, secret, label, seed, wantedLength) do
    {s1, s2} = split_secret(secret)
    lS = :erlang.list_to_binary([label, seed])

    :crypto.exor(
      p_hash(s1, lS, wantedLength, 1),
      p_hash(s2, lS, wantedLength, 2)
    )
  end

  def prf(mAC, secret, label, seed, wantedLength) do
    lS = :erlang.list_to_binary([label, seed])
    p_hash(secret, lS, wantedLength, mAC)
  end

  defp finished_label(:client) do
    "client finished"
  end

  defp finished_label(:server) do
    "server finished"
  end

  defp is_pair(:sha, :dsa, _) do
    true
  end

  defp is_pair(_, :dsa, _) do
    false
  end

  defp is_pair(hash, :ecdsa, hashs) do
    atLeastSha = hashs -- [:md2, :md4, :md5]
    :lists.member(hash, atLeastSha)
  end

  defp is_pair(hash, :rsa, hashs) do
    atLeastMd5 = hashs -- [:md2, :md4]
    :lists.member(hash, atLeastMd5)
  end

  def ecc_curves(:all) do
    [
      :sect571r1,
      :sect571k1,
      :secp521r1,
      :brainpoolP512r1,
      :sect409k1,
      :sect409r1,
      :brainpoolP384r1,
      :secp384r1,
      :sect283k1,
      :sect283r1,
      :brainpoolP256r1,
      :secp256k1,
      :secp256r1,
      :sect239k1,
      :sect233k1,
      :sect233r1,
      :secp224k1,
      :secp224r1,
      :sect193r1,
      :sect193r2,
      :secp192k1,
      :secp192r1,
      :sect163k1,
      :sect163r1,
      :sect163r2,
      :secp160k1,
      :secp160r1,
      :secp160r2
    ]
  end

  def ecc_curves(minor) do
    tLSCurves = ecc_curves(:all)
    ecc_curves(minor, tLSCurves)
  end

  def ecc_curves(_Minor, tLSCurves) do
    cryptoCurves = :crypto.ec_curves()

    :lists.foldr(
      fn curve, curves ->
        case :proplists.get_bool(curve, cryptoCurves) do
          true ->
            [:pubkey_cert_records.namedCurves(curve) | curves]

          false ->
            curves
        end
      end,
      [],
      tLSCurves
    )
  end

  def groups(:all) do
    [
      :x25519,
      :x448,
      :secp256r1,
      :secp384r1,
      :secp521r1,
      :ffdhe2048,
      :ffdhe3072,
      :ffdhe4096,
      :ffdhe6144,
      :ffdhe8192
    ]
  end

  def groups(:default) do
    [:x25519, :x448, :secp256r1, :secp384r1]
  end

  def groups(minor) do
    tLSGroups = groups(:all)
    groups(minor, tLSGroups)
  end

  def groups(_Minor, tLSGroups) do
    cryptoGroups = supported_groups()

    :lists.filter(
      fn group ->
        :proplists.get_bool(group, cryptoGroups)
      end,
      tLSGroups
    )
  end

  def default_groups(minor) do
    tLSGroups = groups(:default)
    groups(minor, tLSGroups)
  end

  defp supported_groups() do
    :proplists.get_value(
      :curves,
      :crypto.supports()
    ) ++ [:ffdhe2048, :ffdhe3072, :ffdhe4096, :ffdhe6144, :ffdhe8192]
  end

  def group_to_enum(:secp256r1) do
    23
  end

  def group_to_enum(:secp384r1) do
    24
  end

  def group_to_enum(:secp521r1) do
    25
  end

  def group_to_enum(:x25519) do
    29
  end

  def group_to_enum(:x448) do
    30
  end

  def group_to_enum(:ffdhe2048) do
    256
  end

  def group_to_enum(:ffdhe3072) do
    257
  end

  def group_to_enum(:ffdhe4096) do
    258
  end

  def group_to_enum(:ffdhe6144) do
    259
  end

  def group_to_enum(:ffdhe8192) do
    260
  end

  def enum_to_group(23) do
    :secp256r1
  end

  def enum_to_group(24) do
    :secp384r1
  end

  def enum_to_group(25) do
    :secp521r1
  end

  def enum_to_group(29) do
    :x25519
  end

  def enum_to_group(30) do
    :x448
  end

  def enum_to_group(256) do
    :ffdhe2048
  end

  def enum_to_group(257) do
    :ffdhe3072
  end

  def enum_to_group(258) do
    :ffdhe4096
  end

  def enum_to_group(259) do
    :ffdhe6144
  end

  def enum_to_group(260) do
    :ffdhe8192
  end

  def enum_to_group(_) do
    :undefined
  end

  def oid_to_enum({1, 3, 132, 0, 1}) do
    1
  end

  def oid_to_enum({1, 3, 132, 0, 2}) do
    2
  end

  def oid_to_enum({1, 3, 132, 0, 15}) do
    3
  end

  def oid_to_enum({1, 3, 132, 0, 24}) do
    4
  end

  def oid_to_enum({1, 3, 132, 0, 25}) do
    5
  end

  def oid_to_enum({1, 3, 132, 0, 26}) do
    6
  end

  def oid_to_enum({1, 3, 132, 0, 27}) do
    7
  end

  def oid_to_enum({1, 3, 132, 0, 3}) do
    8
  end

  def oid_to_enum({1, 3, 132, 0, 16}) do
    9
  end

  def oid_to_enum({1, 3, 132, 0, 17}) do
    10
  end

  def oid_to_enum({1, 3, 132, 0, 36}) do
    11
  end

  def oid_to_enum({1, 3, 132, 0, 37}) do
    12
  end

  def oid_to_enum({1, 3, 132, 0, 38}) do
    13
  end

  def oid_to_enum({1, 3, 132, 0, 39}) do
    14
  end

  def oid_to_enum({1, 3, 132, 0, 9}) do
    15
  end

  def oid_to_enum({1, 3, 132, 0, 8}) do
    16
  end

  def oid_to_enum({1, 3, 132, 0, 30}) do
    17
  end

  def oid_to_enum({1, 3, 132, 0, 31}) do
    18
  end

  def oid_to_enum({1, 2, 840, 10045, 3, 1, 1}) do
    19
  end

  def oid_to_enum({1, 3, 132, 0, 32}) do
    20
  end

  def oid_to_enum({1, 3, 132, 0, 33}) do
    21
  end

  def oid_to_enum({1, 3, 132, 0, 10}) do
    22
  end

  def oid_to_enum({1, 2, 840, 10045, 3, 1, 7}) do
    23
  end

  def oid_to_enum({1, 3, 132, 0, 34}) do
    24
  end

  def oid_to_enum({1, 3, 132, 0, 35}) do
    25
  end

  def oid_to_enum({1, 3, 36, 3, 3, 2, 8, 1, 1, 7}) do
    26
  end

  def oid_to_enum({1, 3, 36, 3, 3, 2, 8, 1, 1, 11}) do
    27
  end

  def oid_to_enum({1, 3, 36, 3, 3, 2, 8, 1, 1, 13}) do
    28
  end

  def oid_to_enum({1, 3, 101, 110}) do
    29
  end

  def oid_to_enum({1, 3, 101, 111}) do
    30
  end

  def enum_to_oid(1) do
    {1, 3, 132, 0, 1}
  end

  def enum_to_oid(2) do
    {1, 3, 132, 0, 2}
  end

  def enum_to_oid(3) do
    {1, 3, 132, 0, 15}
  end

  def enum_to_oid(4) do
    {1, 3, 132, 0, 24}
  end

  def enum_to_oid(5) do
    {1, 3, 132, 0, 25}
  end

  def enum_to_oid(6) do
    {1, 3, 132, 0, 26}
  end

  def enum_to_oid(7) do
    {1, 3, 132, 0, 27}
  end

  def enum_to_oid(8) do
    {1, 3, 132, 0, 3}
  end

  def enum_to_oid(9) do
    {1, 3, 132, 0, 16}
  end

  def enum_to_oid(10) do
    {1, 3, 132, 0, 17}
  end

  def enum_to_oid(11) do
    {1, 3, 132, 0, 36}
  end

  def enum_to_oid(12) do
    {1, 3, 132, 0, 37}
  end

  def enum_to_oid(13) do
    {1, 3, 132, 0, 38}
  end

  def enum_to_oid(14) do
    {1, 3, 132, 0, 39}
  end

  def enum_to_oid(15) do
    {1, 3, 132, 0, 9}
  end

  def enum_to_oid(16) do
    {1, 3, 132, 0, 8}
  end

  def enum_to_oid(17) do
    {1, 3, 132, 0, 30}
  end

  def enum_to_oid(18) do
    {1, 3, 132, 0, 31}
  end

  def enum_to_oid(19) do
    {1, 2, 840, 10045, 3, 1, 1}
  end

  def enum_to_oid(20) do
    {1, 3, 132, 0, 32}
  end

  def enum_to_oid(21) do
    {1, 3, 132, 0, 33}
  end

  def enum_to_oid(22) do
    {1, 3, 132, 0, 10}
  end

  def enum_to_oid(23) do
    {1, 2, 840, 10045, 3, 1, 7}
  end

  def enum_to_oid(24) do
    {1, 3, 132, 0, 34}
  end

  def enum_to_oid(25) do
    {1, 3, 132, 0, 35}
  end

  def enum_to_oid(26) do
    {1, 3, 36, 3, 3, 2, 8, 1, 1, 7}
  end

  def enum_to_oid(27) do
    {1, 3, 36, 3, 3, 2, 8, 1, 1, 11}
  end

  def enum_to_oid(28) do
    {1, 3, 36, 3, 3, 2, 8, 1, 1, 13}
  end

  def enum_to_oid(29) do
    {1, 3, 101, 110}
  end

  def enum_to_oid(30) do
    {1, 3, 101, 111}
  end

  def enum_to_oid(_) do
    :undefined
  end
end
