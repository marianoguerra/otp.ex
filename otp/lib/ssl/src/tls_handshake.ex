defmodule :m_tls_handshake do
  use Bitwise
  require Record

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

  Record.defrecord(:r_key_share_entry, :key_share_entry,
    group: :undefined,
    key_exchange: :undefined
  )

  Record.defrecord(:r_key_share_client_hello, :key_share_client_hello, client_shares: :undefined)

  Record.defrecord(:r_key_share_hello_retry_request, :key_share_hello_retry_request,
    selected_group: :undefined
  )

  Record.defrecord(:r_key_share_server_hello, :key_share_server_hello, server_share: :undefined)

  Record.defrecord(:r_uncompressed_point_representation, :uncompressed_point_representation,
    legacy_form: 4,
    x: :undefined,
    y: :undefined
  )

  Record.defrecord(:r_psk_key_exchange_modes, :psk_key_exchange_modes, ke_modes: :undefined)
  Record.defrecord(:r_empty, :empty, [])
  Record.defrecord(:r_early_data_indication, :early_data_indication, indication: :undefined)

  Record.defrecord(:r_psk_identity, :psk_identity,
    identity: :undefined,
    obfuscated_ticket_age: :undefined
  )

  Record.defrecord(:r_offered_psks, :offered_psks,
    identities: :undefined,
    binders: :undefined
  )

  Record.defrecord(:r_pre_shared_key_client_hello, :pre_shared_key_client_hello,
    offered_psks: :undefined
  )

  Record.defrecord(:r_pre_shared_key_server_hello, :pre_shared_key_server_hello,
    selected_identity: :undefined
  )

  Record.defrecord(:r_cookie, :cookie, cookie: :undefined)
  Record.defrecord(:r_named_group_list, :named_group_list, named_group_list: :undefined)

  Record.defrecord(:r_certificate_authoritie_sextension, :certificate_authoritie_sextension,
    authorities: :undefined
  )

  Record.defrecord(:r_oid_filter, :oid_filter,
    certificate_extension_oid: :undefined,
    certificate_extension_values: :undefined
  )

  Record.defrecord(:r_oid_filter_extension, :oid_filter_extension, filters: :undefined)
  Record.defrecord(:r_post_handshake_auth, :post_handshake_auth, [])
  Record.defrecord(:r_encrypted_extensions, :encrypted_extensions, extensions: :undefined)

  Record.defrecord(:r_certificate_request_1_3, :certificate_request_1_3,
    certificate_request_context: :undefined,
    extensions: :undefined
  )

  Record.defrecord(:r_certificate_entry, :certificate_entry,
    data: :undefined,
    extensions: :undefined
  )

  Record.defrecord(:r_certificate_1_3, :certificate_1_3,
    certificate_request_context: :undefined,
    certificate_list: :undefined
  )

  Record.defrecord(:r_certificate_verify_1_3, :certificate_verify_1_3,
    algorithm: :undefined,
    signature: :undefined
  )

  Record.defrecord(:r_new_session_ticket, :new_session_ticket,
    ticket_lifetime: :undefined,
    ticket_age_add: :undefined,
    ticket_nonce: :undefined,
    ticket: :undefined,
    extensions: :undefined
  )

  Record.defrecord(:r_end_of_early_data, :end_of_early_data, [])
  Record.defrecord(:r_key_update, :key_update, request_update: :undefined)

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

  Record.defrecord(:r_alert, :alert,
    level: :undefined,
    description: :undefined,
    where: :undefined,
    role: :undefined,
    reason: :undefined
  )

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

  Record.defrecord(:r_sslsocket, :sslsocket, fd: nil, pid: nil)

  def client_hello(
        _Host,
        _Port,
        connectionStates,
        %{:versions => versions, :ciphers => userSuites, :fallback => fallback} = sslOpts,
        id,
        renegotiation,
        _OwnCert,
        keyShare,
        ticketData,
        ocspNonce
      ) do
    version = :tls_record.highest_protocol_version(versions)

    legacyVersion =
      case :tls_record.is_higher(
             version,
             {3, 2}
           ) do
        true ->
          {3, 3}

        false ->
          version
      end

    %{:security_parameters => secParams} =
      :ssl_record.pending_connection_state(
        connectionStates,
        :read
      )

    availableCipherSuites =
      :ssl_handshake.available_suites(
        userSuites,
        version
      )

    extensions =
      :ssl_handshake.client_hello_extensions(
        version,
        availableCipherSuites,
        sslOpts,
        connectionStates,
        renegotiation,
        keyShare,
        ticketData,
        ocspNonce
      )

    cipherSuites = :ssl_handshake.cipher_suites(availableCipherSuites, renegotiation, fallback)

    r_client_hello(
      session_id: id,
      client_version: legacyVersion,
      cipher_suites: cipherSuites,
      compression_methods: :ssl_record.compressions(),
      random: r_security_parameters(secParams, :client_random),
      extensions: extensions
    )
  end

  def hello(
        r_server_hello(
          server_version: {major, minor},
          random: <<_::size(24)-binary, down::size(8)-binary>>
        ),
        %{:versions => [{m, n} | _]},
        _,
        _,
        _
      )
      when ((m > 3 or (m === 3 and n >= 4)) and major === 3 and minor === 3 and
              down === <<68, 79, 87, 78, 71, 82, 68, 1>>) or
             ((m > 3 or (m === 3 and n >= 4)) and major === 3 and minor < 3 and
                down === <<68, 79, 87, 78, 71, 82, 68, 0>>) do
    r_alert(
      level: 2,
      description: 47,
      where: %{
        :mfa => {:tls_handshake, :hello, 5},
        :line => 130,
        :file => 'otp/lib/ssl/src/tls_handshake.erl'
      }
    )
  end

  def hello(
        r_server_hello(
          server_version: {major, minor},
          random: <<_::size(24)-binary, down::size(8)-binary>>
        ),
        %{:versions => [{m, n} | _]},
        _,
        _,
        _
      )
      when m === 3 and n === 3 and major === 3 and minor < 3 and
             down === <<68, 79, 87, 78, 71, 82, 68, 0>> do
    r_alert(
      level: 2,
      description: 47,
      where: %{
        :mfa => {:tls_handshake, :hello, 5},
        :line => 140,
        :file => 'otp/lib/ssl/src/tls_handshake.erl'
      }
    )
  end

  def hello(
        r_server_hello(
          server_version: legacyVersion,
          random: random,
          cipher_suite: cipherSuite,
          compression_method: compression,
          session_id: sessionId,
          extensions: %{
            :server_hello_selected_version =>
              r_server_hello_selected_version(selected_version: version) = helloExt
          }
        ),
        %{:versions => supportedVersions, :ocsp_stapling => stapling} = sslOpt,
        connectionStates0,
        renegotiation,
        oldId
      ) do
    case legacyVersion > {3, 3} or (legacyVersion === {3, 3} and version < {3, 3}) do
      true ->
        r_alert(
          level: 2,
          description: 47,
          where: %{
            :mfa => {:tls_handshake, :hello, 5},
            :line => 171,
            :file => 'otp/lib/ssl/src/tls_handshake.erl'
          }
        )

      false ->
        case :tls_record.is_acceptable_version(
               version,
               supportedVersions
             ) do
          true ->
            case version do
              {3, 3} ->
                isNew = :ssl_session.is_new(oldId, sessionId)

                handle_server_hello_extensions(
                  version,
                  sessionId,
                  random,
                  cipherSuite,
                  compression,
                  helloExt,
                  sslOpt,
                  connectionStates0,
                  renegotiation,
                  isNew
                )

              selectedVersion ->
                {:next_state, :wait_sh, selectedVersion,
                 %{:ocsp_stapling => stapling, :ocsp_expect => ocsp_expect(stapling)}}
            end

          false ->
            r_alert(
              level: 2,
              description: 47,
              where: %{
                :mfa => {:tls_handshake, :hello, 5},
                :line => 188,
                :file => 'otp/lib/ssl/src/tls_handshake.erl'
              }
            )
        end
    end
  end

  def hello(
        r_server_hello(
          server_version: version,
          random: random,
          cipher_suite: cipherSuite,
          compression_method: compression,
          session_id: sessionId,
          extensions: helloExt
        ),
        %{:versions => supportedVersions} = sslOpt,
        connectionStates0,
        renegotiation,
        oldId
      ) do
    isNew = :ssl_session.is_new(oldId, sessionId)

    case :tls_record.is_acceptable_version(
           version,
           supportedVersions
         ) do
      true ->
        handle_server_hello_extensions(
          version,
          sessionId,
          random,
          cipherSuite,
          compression,
          helloExt,
          sslOpt,
          connectionStates0,
          renegotiation,
          isNew
        )

      false ->
        r_alert(
          level: 2,
          description: 70,
          where: %{
            :mfa => {:tls_handshake, :hello, 5},
            :line => 207,
            :file => 'otp/lib/ssl/src/tls_handshake.erl'
          }
        )
    end
  end

  def hello(
        r_client_hello(
          client_version: _ClientVersion,
          cipher_suites: cipherSuites,
          extensions: %{
            :client_hello_versions => r_client_hello_versions(versions: clientVersions)
          }
        ) = hello,
        %{:versions => versions} = sslOpts,
        info,
        renegotiation
      ) do
    try do
      version =
        :ssl_handshake.select_supported_version(
          clientVersions,
          versions
        )

      do_hello(version, versions, cipherSuites, hello, sslOpts, info, renegotiation)
    catch
      _, _ ->
        r_alert(
          level: 2,
          description: 40,
          where: %{
            :mfa => {:tls_handshake, :hello, 4},
            :line => 248,
            :file => 'otp/lib/ssl/src/tls_handshake.erl'
          },
          reason: :malformed_handshake_data
        )
    end
  end

  def hello(
        r_client_hello(
          client_version: clientVersion,
          cipher_suites: cipherSuites
        ) = hello,
        %{:versions => versions} = sslOpts,
        info,
        renegotiation
      ) do
    try do
      version = :ssl_handshake.select_version(:tls_record, clientVersion, versions)
      do_hello(version, versions, cipherSuites, hello, sslOpts, info, renegotiation)
    catch
      :error, {:case_clause, {:asn1, asn1Reason}} ->
        r_alert(
          level: 2,
          description: 80,
          where: %{
            :mfa => {:tls_handshake, :hello, 4},
            :line => 261,
            :file => 'otp/lib/ssl/src/tls_handshake.erl'
          },
          reason: {:failed_to_decode_own_certificate, asn1Reason}
        )

      _, _ ->
        r_alert(
          level: 2,
          description: 40,
          where: %{
            :mfa => {:tls_handshake, :hello, 4},
            :line => 263,
            :file => 'otp/lib/ssl/src/tls_handshake.erl'
          },
          reason: :malformed_handshake_data
        )
    end
  end

  def encode_handshake(package, version) do
    {msgType, bin} = enc_handshake(package, version)
    len = byte_size(bin)
    [msgType, <<len::size(24)-unsigned-big-integer>>, bin]
  end

  def get_tls_handshake(version, data, <<>>, options) do
    get_tls_handshake_aux(version, data, options, [])
  end

  def get_tls_handshake(version, data, buffer, options) do
    get_tls_handshake_aux(version, :erlang.list_to_binary([buffer, data]), options, [])
  end

  def ocsp_nonce(true, true) do
    :public_key.der_encode(
      :Nonce,
      :crypto.strong_rand_bytes(8)
    )
  end

  def ocsp_nonce(_OcspNonceOpt, _OcspStaplingOpt) do
    :undefined
  end

  defp handle_client_hello(
         version,
         r_client_hello(
           session_id: sugesstedId,
           cipher_suites: cipherSuites,
           compression_methods: compressions,
           random: random,
           extensions: helloExt
         ),
         %{
           :versions => versions,
           :signature_algs => supportedHashSigns,
           :eccs => supportedECCs,
           :honor_ecc_order => eCCOrder
         } = sslOpts,
         {sessIdTracker, session0, connectionStates0, cert, _},
         renegotiation
       ) do
    case :tls_record.is_acceptable_version(
           version,
           versions
         ) do
      true ->
        curves = :maps.get(:elliptic_curves, helloExt, :undefined)
        clientHashSigns = :maps.get(:signature_algs, helloExt, :undefined)
        clientSignatureSchemes = :maps.get(:signature_algs_cert, helloExt, :undefined)

        availableHashSigns =
          :ssl_handshake.available_signature_algs(
            clientHashSigns,
            supportedHashSigns,
            cert,
            version
          )

        eCCCurve = :ssl_handshake.select_curve(curves, supportedECCs, eCCOrder)

        {type, r_session(cipher_suite: cipherSuite) = session1} =
          :ssl_handshake.select_session(
            sugesstedId,
            cipherSuites,
            availableHashSigns,
            compressions,
            sessIdTracker,
            r_session(session0, ecc: eCCCurve),
            version,
            sslOpts,
            cert
          )

        case cipherSuite do
          :no_suite ->
            r_alert(
              level: 2,
              description: 71,
              where: %{
                :mfa => {:tls_handshake, :handle_client_hello, 5},
                :line => 345,
                :file => 'otp/lib/ssl/src/tls_handshake.erl'
              },
              reason: :no_suitable_ciphers
            )

          _ ->
            %{:key_exchange => keyExAlg} = :ssl_cipher_format.suite_bin_to_map(cipherSuite)

            case :ssl_handshake.select_hashsign(
                   {clientHashSigns, clientSignatureSchemes},
                   cert,
                   keyExAlg,
                   supportedHashSigns,
                   version
                 ) do
              r_alert() = alert ->
                alert

              hashSign ->
                handle_client_hello_extensions(
                  version,
                  type,
                  random,
                  cipherSuites,
                  helloExt,
                  sslOpts,
                  session1,
                  connectionStates0,
                  renegotiation,
                  hashSign
                )
            end
        end

      false ->
        r_alert(
          level: 2,
          description: 70,
          where: %{
            :mfa => {:tls_handshake, :handle_client_hello, 5},
            :line => 363,
            :file => 'otp/lib/ssl/src/tls_handshake.erl'
          }
        )
    end
  end

  defp handle_client_hello_extensions(
         version,
         type,
         random,
         cipherSuites,
         helloExt,
         sslOpts,
         session0,
         connectionStates0,
         renegotiation,
         hashSign
       ) do
    try do
      :ssl_handshake.handle_client_hello_extensions(
        :tls_record,
        random,
        cipherSuites,
        helloExt,
        version,
        sslOpts,
        session0,
        connectionStates0,
        renegotiation,
        r_session(session0, :is_resumable)
      )
    catch
      alert ->
        alert
    else
      {session, connectionStates, protocol, serverHelloExt} ->
        {version, {type, session}, connectionStates, protocol, serverHelloExt, hashSign}
    end
  end

  defp handle_server_hello_extensions(
         version,
         sessionId,
         random,
         cipherSuite,
         compression,
         helloExt,
         sslOpt,
         connectionStates0,
         renegotiation,
         isNew
       ) do
    try do
      :ssl_handshake.handle_server_hello_extensions(
        :tls_record,
        random,
        cipherSuite,
        compression,
        helloExt,
        version,
        sslOpt,
        connectionStates0,
        renegotiation,
        isNew
      )
    catch
      alert ->
        alert
    else
      {connectionStates, protoExt, protocol, ocspState} ->
        {version, sessionId, connectionStates, protoExt, protocol, ocspState}
    end
  end

  defp do_hello(:undefined, _Versions, _CipherSuites, _Hello, _SslOpts, _Info, _Renegotiation) do
    r_alert(
      level: 2,
      description: 70,
      where: %{
        :mfa => {:tls_handshake, :do_hello, 7},
        :line => 396,
        :file => 'otp/lib/ssl/src/tls_handshake.erl'
      }
    )
  end

  defp do_hello(version, versions, cipherSuites, hello, sslOpts, info, renegotiation) do
    case :ssl_cipher.is_fallback(cipherSuites) do
      true ->
        highest = :tls_record.highest_protocol_version(versions)

        case :tls_record.is_higher(highest, version) do
          true ->
            r_alert(
              level: 2,
              description: 86,
              where: %{
                :mfa => {:tls_handshake, :do_hello, 7},
                :line => 403,
                :file => 'otp/lib/ssl/src/tls_handshake.erl'
              }
            )

          false ->
            handle_client_hello(version, hello, sslOpts, info, renegotiation)
        end

      false ->
        handle_client_hello(version, hello, sslOpts, info, renegotiation)
    end
  end

  defp enc_handshake(r_hello_request(), {3, n}) when n < 4 do
    {0, <<>>}
  end

  defp enc_handshake(
         r_client_hello(
           client_version: {major, minor} = version,
           random: random,
           session_id: sessionID,
           cipher_suites: cipherSuites,
           compression_methods: compMethods,
           extensions: helloExtensions
         ),
         _Version
       ) do
    sIDLength = byte_size(sessionID)
    binCompMethods = :erlang.list_to_binary(compMethods)
    cmLength = byte_size(binCompMethods)
    binCipherSuites = :erlang.list_to_binary(cipherSuites)
    csLength = byte_size(binCipherSuites)

    extensionsBin =
      :ssl_handshake.encode_hello_extensions(
        helloExtensions,
        version
      )

    {1,
     <<major::size(8)-unsigned-big-integer, minor::size(8)-unsigned-big-integer,
       random::size(32)-binary, sIDLength::size(8)-unsigned-big-integer, sessionID::binary,
       csLength::size(16)-unsigned-big-integer, binCipherSuites::binary,
       cmLength::size(8)-unsigned-big-integer, binCompMethods::binary, extensionsBin::binary>>}
  end

  defp enc_handshake(handshakeMsg, {3, 4}) do
    :tls_handshake_1_3.encode_handshake(handshakeMsg)
  end

  defp enc_handshake(handshakeMsg, version) do
    :ssl_handshake.encode_handshake(handshakeMsg, version)
  end

  defp get_tls_handshake_aux(
         version,
         <<type::size(8)-unsigned-big-integer, length::size(24)-unsigned-big-integer,
           body::size(length)-binary, rest::binary>>,
         %{:log_level => logLevel} = opts,
         acc
       ) do
    raw =
      <<type::size(8)-unsigned-big-integer, length::size(24)-unsigned-big-integer, body::binary>>

    try do
      decode_handshake(version, type, body)
    catch
      r_alert() = alert ->
        throw(alert)

      _, _ ->
        throw(
          r_alert(
            level: 2,
            description: 40,
            where: %{
              :mfa => {:tls_handshake, :get_tls_handshake_aux, 4},
              :line => 449,
              :file => 'otp/lib/ssl/src/tls_handshake.erl'
            },
            reason: :handshake_decode_error
          )
        )
    else
      handshake ->
        :ssl_logger.debug(logLevel, :inbound, :handshake, handshake)
        get_tls_handshake_aux(version, rest, opts, [{handshake, raw} | acc])
    end
  end

  defp get_tls_handshake_aux(_Version, data, _, acc) do
    {:lists.reverse(acc), data}
  end

  def decode_handshake({3, n}, 0, <<>>) when n < 4 do
    r_hello_request()
  end

  def decode_handshake(
        version,
        1,
        <<major::size(8)-unsigned-big-integer, minor::size(8)-unsigned-big-integer,
          random::size(32)-binary, sID_length::size(8)-unsigned-big-integer,
          session_ID::size(sID_length)-binary, cs_length::size(16)-unsigned-big-integer,
          cipherSuites::size(cs_length)-binary, cm_length::size(8)-unsigned-big-integer,
          comp_methods::size(cm_length)-binary, extensions::binary>>
      ) do
    exts = :ssl_handshake.decode_vector(extensions)

    decodedExtensions =
      :ssl_handshake.decode_hello_extensions(
        exts,
        version,
        {major, minor},
        :client_hello
      )

    r_client_hello(
      client_version: {major, minor},
      random: random,
      session_id: session_ID,
      cipher_suites:
        :ssl_handshake.decode_suites(
          :"2_bytes",
          cipherSuites
        ),
      compression_methods: :erlang.binary_to_list(comp_methods),
      extensions: decodedExtensions
    )
  end

  def decode_handshake({3, 4}, tag, msg) do
    :tls_handshake_1_3.decode_handshake(tag, msg)
  end

  def decode_handshake(version, tag, msg) do
    :ssl_handshake.decode_handshake(version, tag, msg)
  end

  defp ocsp_expect(true) do
    :staple
  end

  defp ocsp_expect(_) do
    :no_staple
  end
end
