defmodule :m_ssl_logger do
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

  Record.defrecord(:r_ssl_tls, :ssl_tls,
    type: :undefined,
    version: :undefined,
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

  def log(level, logLevel, reportMap, meta) do
    case :logger.compare_levels(logLevel, level) do
      :lt ->
        :logger.log(
          level,
          reportMap,
          Map.merge(meta, %{depth: 20, report_cb: &:ssl_logger.format/1})
        )

      :eq ->
        :logger.log(
          level,
          reportMap,
          Map.merge(meta, %{depth: 20, report_cb: &:ssl_logger.format/1})
        )

      _ ->
        :ok
    end
  end

  def debug(level, direction, protocol, message)
      when (direction === :inbound or direction === :outbound) and
             (protocol === :record or protocol === :handshake) do
    case :logger.compare_levels(level, :debug) do
      :lt ->
        case :logger.allow(:debug, :ssl_logger) do
          true ->
            apply(:logger, :macro_log, [
              %{mfa: {:ssl_logger, :debug, 4}, line: 65, file: 'otp/lib/ssl/src/ssl_logger.erl'},
              :debug,
              %{direction: direction, protocol: protocol, message: message},
              %{domain: [:otp, :ssl, protocol]}
            ])

          false ->
            :ok
        end

      :eq ->
        case :logger.allow(:debug, :ssl_logger) do
          true ->
            apply(:logger, :macro_log, [
              %{mfa: {:ssl_logger, :debug, 4}, line: 70, file: 'otp/lib/ssl/src/ssl_logger.erl'},
              :debug,
              %{direction: direction, protocol: protocol, message: message},
              %{domain: [:otp, :ssl, protocol]}
            ])

          false ->
            :ok
        end

      _ ->
        :ok
    end
  end

  def format(%{alert: alert, alerter: :own} = report) do
    %{protocol: protocolName, role: role, alert: ^alert, statename: stateName} = report
    :ssl_alert.own_alert_format(protocolName, role, stateName, alert)
  end

  def format(%{alert: alert, alerter: :peer} = report) do
    %{protocol: protocolName, role: role, alert: ^alert, statename: stateName} = report
    :ssl_alert.alert_format(protocolName, role, stateName, alert)
  end

  def format(%{alert: alert, alerter: :ignored} = report) do
    %{protocol: protocolName, role: role, alert: ^alert, statename: stateName} = report
    {fmt, args} = :ssl_alert.own_alert_format(protocolName, role, stateName, alert)
    {'~s ' ++ fmt, ['Ignored alert to mitigate DoS attacks', args]}
  end

  def format(%{description: desc} = report) do
    %{reason: reason} = report
    {'~s11:~p~n~s11:~p~n', ['Description', desc, 'Reason', reason]}
  end

  def format(%{msg: {:report, msg}}, _Config0) do
    %{direction: direction, protocol: protocol, message: content} = msg

    case protocol do
      :record ->
        binMsg =
          case content do
            r_ssl_tls() ->
              [:tls_record.build_tls_record(content)]

            _ when is_list(content) ->
              :lists.flatten(content)
          end

        format_tls_record(direction, binMsg)

      :handshake ->
        format_handshake(direction, content)

      _Other ->
        []
    end
  end

  defp format_handshake(direction, binMsg) do
    {header, message} = parse_handshake(direction, binMsg)
    :io_lib.format('~s~n~s~n', [header, message])
  end

  defp parse_handshake(
         direction,
         r_client_hello(
           client_version: version0,
           cipher_suites: cipherSuites0,
           extensions: extensions
         ) = clientHello
       ) do
    version = get_client_version(version0, extensions)

    header =
      :io_lib.format(
        '~s ~s Handshake, ClientHello',
        [header_prefix(direction), version(version)]
      )

    cipherSuites = parse_cipher_suites(cipherSuites0)

    message =
      :io_lib.format(
        '~p',
        [
          :lists.zip(
            Keyword.keys(r_client_hello(r_client_hello())),
            tl(:erlang.tuple_to_list(r_client_hello(clientHello, cipher_suites: cipherSuites)))
          )
        ]
      )

    {header, message}
  end

  defp parse_handshake(
         direction,
         r_server_hello(
           server_version: version0,
           cipher_suite: cipherSuite0,
           extensions: extensions
         ) = serverHello
       ) do
    version = get_server_version(version0, extensions)

    header =
      :io_lib.format(
        '~s ~s Handshake, ServerHello',
        [header_prefix(direction), version(version)]
      )

    cipherSuite = format_cipher(cipherSuite0)

    message =
      :io_lib.format(
        '~p',
        [
          :lists.zip(
            Keyword.keys(r_server_hello(r_server_hello())),
            tl(:erlang.tuple_to_list(r_server_hello(serverHello, cipher_suite: cipherSuite)))
          )
        ]
      )

    {header, message}
  end

  defp parse_handshake(direction, r_hello_verify_request() = helloVerifyRequest) do
    header = :io_lib.format('~s Handshake, HelloVerifyRequest', [header_prefix(direction)])

    message =
      :io_lib.format(
        '~p',
        [
          :lists.zip(
            Keyword.keys(r_hello_verify_request(r_hello_verify_request())),
            tl(:erlang.tuple_to_list(helloVerifyRequest))
          )
        ]
      )

    {header, message}
  end

  defp parse_handshake(direction, r_certificate() = certificate) do
    header = :io_lib.format('~s Handshake, Certificate', [header_prefix(direction)])

    message =
      :io_lib.format(
        '~p',
        [
          :lists.zip(
            Keyword.keys(r_certificate(r_certificate())),
            tl(:erlang.tuple_to_list(certificate))
          )
        ]
      )

    {header, message}
  end

  defp parse_handshake(direction, r_certificate_status() = certificateStatus) do
    header = :io_lib.format('~s Handshake, CertificateStatus', [header_prefix(direction)])

    message =
      :io_lib.format(
        '~p',
        [
          :lists.zip(
            Keyword.keys(r_certificate_status(r_certificate_status())),
            tl(:erlang.tuple_to_list(certificateStatus))
          )
        ]
      )

    {header, message}
  end

  defp parse_handshake(direction, r_server_key_exchange() = serverKeyExchange) do
    header = :io_lib.format('~s Handshake, ServerKeyExchange', [header_prefix(direction)])

    message =
      :io_lib.format(
        '~p',
        [
          :lists.zip(
            Keyword.keys(r_server_key_exchange(r_server_key_exchange())),
            tl(:erlang.tuple_to_list(serverKeyExchange))
          )
        ]
      )

    {header, message}
  end

  defp parse_handshake(direction, r_server_key_params() = serverKeyExchange) do
    header = :io_lib.format('~s Handshake, ServerKeyExchange', [header_prefix(direction)])

    message =
      :io_lib.format(
        '~p',
        [
          :lists.zip(
            Keyword.keys(r_server_key_params(r_server_key_params())),
            tl(:erlang.tuple_to_list(serverKeyExchange))
          )
        ]
      )

    {header, message}
  end

  defp parse_handshake(direction, r_certificate_request() = certificateRequest) do
    header = :io_lib.format('~s Handshake, CertificateRequest', [header_prefix(direction)])

    message =
      :io_lib.format(
        '~p',
        [
          :lists.zip(
            Keyword.keys(r_certificate_request(r_certificate_request())),
            tl(:erlang.tuple_to_list(certificateRequest))
          )
        ]
      )

    {header, message}
  end

  defp parse_handshake(direction, r_server_hello_done() = serverHelloDone) do
    header = :io_lib.format('~s Handshake, ServerHelloDone', [header_prefix(direction)])

    message =
      :io_lib.format(
        '~p',
        [
          :lists.zip(
            Keyword.keys(r_server_hello_done(r_server_hello_done())),
            tl(:erlang.tuple_to_list(serverHelloDone))
          )
        ]
      )

    {header, message}
  end

  defp parse_handshake(direction, r_client_key_exchange() = clientKeyExchange) do
    header = :io_lib.format('~s Handshake, ClientKeyExchange', [header_prefix(direction)])

    message =
      :io_lib.format(
        '~p',
        [
          :lists.zip(
            Keyword.keys(r_client_key_exchange(r_client_key_exchange())),
            tl(:erlang.tuple_to_list(clientKeyExchange))
          )
        ]
      )

    {header, message}
  end

  defp parse_handshake(direction, r_certificate_verify() = certificateVerify) do
    header = :io_lib.format('~s Handshake, CertificateVerify', [header_prefix(direction)])

    message =
      :io_lib.format(
        '~p',
        [
          :lists.zip(
            Keyword.keys(r_certificate_verify(r_certificate_verify())),
            tl(:erlang.tuple_to_list(certificateVerify))
          )
        ]
      )

    {header, message}
  end

  defp parse_handshake(direction, r_finished() = finished) do
    header = :io_lib.format('~s Handshake, Finished', [header_prefix(direction)])

    message =
      :io_lib.format(
        '~p',
        [
          :lists.zip(
            Keyword.keys(r_finished(r_finished())),
            tl(:erlang.tuple_to_list(finished))
          )
        ]
      )

    {header, message}
  end

  defp parse_handshake(direction, r_hello_request() = helloRequest) do
    header = :io_lib.format('~s Handshake, HelloRequest', [header_prefix(direction)])

    message =
      :io_lib.format(
        '~p',
        [
          :lists.zip(
            Keyword.keys(r_hello_request(r_hello_request())),
            tl(:erlang.tuple_to_list(helloRequest))
          )
        ]
      )

    {header, message}
  end

  defp parse_handshake(direction, r_certificate_request_1_3() = certificateRequest) do
    header = :io_lib.format('~s Handshake, CertificateRequest', [header_prefix(direction)])

    message =
      :io_lib.format(
        '~p',
        [
          :lists.zip(
            Keyword.keys(r_certificate_request_1_3(r_certificate_request_1_3())),
            tl(:erlang.tuple_to_list(certificateRequest))
          )
        ]
      )

    {header, message}
  end

  defp parse_handshake(direction, r_certificate_1_3() = certificate) do
    header = :io_lib.format('~s Handshake, Certificate', [header_prefix(direction)])

    message =
      :io_lib.format(
        '~p',
        [
          :lists.zip(
            Keyword.keys(r_certificate_1_3(r_certificate_1_3())),
            tl(:erlang.tuple_to_list(certificate))
          )
        ]
      )

    {header, message}
  end

  defp parse_handshake(direction, r_certificate_verify_1_3() = certificateVerify) do
    header = :io_lib.format('~s Handshake, CertificateVerify', [header_prefix(direction)])

    message =
      :io_lib.format(
        '~p',
        [
          :lists.zip(
            Keyword.keys(r_certificate_verify_1_3(r_certificate_verify_1_3())),
            tl(:erlang.tuple_to_list(certificateVerify))
          )
        ]
      )

    {header, message}
  end

  defp parse_handshake(direction, r_encrypted_extensions() = encryptedExtensions) do
    header = :io_lib.format('~s Handshake, EncryptedExtensions', [header_prefix(direction)])

    message =
      :io_lib.format(
        '~p',
        [
          :lists.zip(
            Keyword.keys(r_encrypted_extensions(r_encrypted_extensions())),
            tl(:erlang.tuple_to_list(encryptedExtensions))
          )
        ]
      )

    {header, message}
  end

  defp parse_handshake(direction, r_new_session_ticket() = newSessionTicket) do
    header = :io_lib.format('~s Post-Handshake, NewSessionTicket', [header_prefix(direction)])

    message =
      :io_lib.format(
        '~p',
        [
          :lists.zip(
            Keyword.keys(r_new_session_ticket(r_new_session_ticket())),
            tl(:erlang.tuple_to_list(newSessionTicket))
          )
        ]
      )

    {header, message}
  end

  defp parse_handshake(direction, r_key_update() = keyUpdate) do
    header = :io_lib.format('~s Post-Handshake, KeyUpdate', [header_prefix(direction)])

    message =
      :io_lib.format(
        '~p',
        [
          :lists.zip(
            Keyword.keys(r_key_update(r_key_update())),
            tl(:erlang.tuple_to_list(keyUpdate))
          )
        ]
      )

    {header, message}
  end

  defp parse_cipher_suites([_ | _] = ciphers) do
    for c <- ciphers do
      format_cipher(c)
    end
  end

  defp format_cipher(c0) do
    try do
      :ssl_cipher_format.suite_bin_to_map(c0)
    catch
      :error, :function_clause ->
        format_uknown_cipher_suite(c0)
    else
      map ->
        :ssl_cipher_format.suite_map_to_str(map)
    end
  end

  defp get_client_version(version, extensions) do
    cHVersions = :maps.get(:client_hello_versions, extensions, :undefined)

    case cHVersions do
      r_client_hello_versions(versions: [highest | _]) ->
        highest

      :undefined ->
        version
    end
  end

  defp get_server_version(version, extensions) do
    sHVersion = :maps.get(:server_hello_selected_version, extensions, :undefined)

    case sHVersion do
      r_server_hello_selected_version(selected_version: selectedVersion) ->
        selectedVersion

      :undefined ->
        version
    end
  end

  defp version({3, 4}) do
    'TLS 1.3'
  end

  defp version({3, 3}) do
    'TLS 1.2'
  end

  defp version({3, 2}) do
    'TLS 1.1'
  end

  defp version({3, 1}) do
    'TLS 1.0'
  end

  defp version({3, 0}) do
    'SSL 3.0'
  end

  defp version({254, 253}) do
    'DTLS 1.2'
  end

  defp version({254, 255}) do
    'DTLS 1.0'
  end

  defp version({m, n}) do
    :io_lib.format('TLS/DTLS [0x0~B0~B]', [m, n])
  end

  defp header_prefix(:inbound) do
    '<<<'
  end

  defp header_prefix(:outbound) do
    '>>>'
  end

  defp format_tls_record(direction, binMsg) do
    {message, size} = convert_to_hex(:tls_record, binMsg)

    header =
      :io_lib.format(
        '~s (~B bytes) ~s~n',
        [header_prefix_tls_record(direction), size, tls_record_version(binMsg)]
      )

    header ++ message
  end

  defp header_prefix_tls_record(:inbound) do
    'reading'
  end

  defp header_prefix_tls_record(:outbound) do
    'writing'
  end

  defp tls_record_version([
         <<b::size(8)-unsigned-big-integer, 3::size(8)-unsigned-big-integer,
           3::size(8)-unsigned-big-integer, _::binary>>
         | _
       ]) do
    :io_lib.format('TLS 1.2 Record Protocol, ~s', [msg_type(b)])
  end

  defp tls_record_version([
         <<b::size(8)-unsigned-big-integer, 3::size(8)-unsigned-big-integer,
           2::size(8)-unsigned-big-integer, _::binary>>
         | _
       ]) do
    :io_lib.format('TLS 1.1 Record Protocol, ~s', [msg_type(b)])
  end

  defp tls_record_version([
         <<b::size(8)-unsigned-big-integer, 3::size(8)-unsigned-big-integer,
           1::size(8)-unsigned-big-integer, _::binary>>
         | _
       ]) do
    :io_lib.format('TLS 1.0 Record Protocol, ~s', [msg_type(b)])
  end

  defp tls_record_version([
         <<b::size(8)-unsigned-big-integer, 3::size(8)-unsigned-big-integer,
           0::size(8)-unsigned-big-integer, _::binary>>
         | _
       ]) do
    :io_lib.format('SSL 3.0 Record Protocol, ~s', [msg_type(b)])
  end

  defp tls_record_version([
         <<b::size(8)-unsigned-big-integer, 254::size(8)-unsigned-big-integer,
           253::size(8)-unsigned-big-integer, _::binary>>
         | _
       ]) do
    :io_lib.format('DTLS 1.2 Record Protocol, ~s', [msg_type(b)])
  end

  defp tls_record_version([
         <<b::size(8)-unsigned-big-integer, 254::size(8)-unsigned-big-integer,
           255::size(8)-unsigned-big-integer, _::binary>>
         | _
       ]) do
    :io_lib.format('DTLS 1.0 Record Protocol, ~s', [msg_type(b)])
  end

  defp tls_record_version([
         <<b::size(8)-unsigned-big-integer, m::size(8)-unsigned-big-integer,
           n::size(8)-unsigned-big-integer, _::binary>>
         | _
       ]) do
    :io_lib.format('TLS/DTLS [0x0~B0~B] Record Protocol, ~s', [m, n, msg_type(b)])
  end

  defp msg_type(20) do
    'change_cipher_spec'
  end

  defp msg_type(21) do
    'alert'
  end

  defp msg_type(22) do
    'handshake'
  end

  defp msg_type(23) do
    'application_data'
  end

  defp msg_type(_) do
    :unknown
  end

  defp convert_to_hex(protocol, binMsg) do
    convert_to_hex(protocol, binMsg, [], [], 0)
  end

  defp convert_to_hex(p, [], row0, acc, c) when rem(c, 16) === 0 do
    row = :lists.reverse(end_row(p, row0))
    {:lists.reverse(acc) ++ row ++ :io_lib.nl(), c}
  end

  defp convert_to_hex(p, [], row0, acc, c) do
    row = :lists.reverse(end_row(p, row0))
    padding = calculate_padding(row0, acc)
    paddedRow = :string.pad(row, padding, :leading, ?\s)
    {:lists.reverse(acc) ++ paddedRow ++ :io_lib.nl(), c}
  end

  defp convert_to_hex(p, [h | t], row, acc, c) when is_list(h) do
    convert_to_hex(p, h ++ t, row, acc, c)
  end

  defp convert_to_hex(p, [<<>> | t], row, acc, c) do
    convert_to_hex(p, t, row, acc, c)
  end

  defp convert_to_hex(p, [<<a::size(4), b::size(4), r::binary>> | t], row, acc, c)
       when c === 0 do
    convert_to_hex(
      p,
      [<<r::binary>> | t],
      update_row(<<a::size(4), b::size(4)>>, row),
      prepend_first_row(p, a, b, acc, c),
      c + 1
    )
  end

  defp convert_to_hex(p, [<<a::size(4), b::size(4), r::binary>> | t], row, acc, c)
       when rem(c, 16) === 0 do
    convert_to_hex(
      p,
      [<<r::binary>> | t],
      update_row(<<a::size(4), b::size(4)>>, []),
      prepend_row(p, a, b, row, acc, c),
      c + 1
    )
  end

  defp convert_to_hex(p, [<<a::size(4), b::size(4), r::binary>> | t], row, acc, c)
       when rem(c, 8) === 7 do
    convert_to_hex(
      p,
      [<<r::binary>> | t],
      update_row(<<a::size(4), b::size(4)>>, row),
      prepend_eighths_hex(a, b, acc),
      c + 1
    )
  end

  defp convert_to_hex(p, [<<a::size(4), b::size(4), r::binary>> | t], row, acc, c) do
    convert_to_hex(
      p,
      [<<r::binary>> | t],
      update_row(<<a::size(4), b::size(4)>>, row),
      prepend_hex(a, b, acc),
      c + 1
    )
  end

  defp convert_to_hex(p, [h | t], row, acc, c)
       when is_integer(h) and
              c === 0 do
    convert_to_hex(p, t, update_row(h, row), prepend_first_row(p, h, acc, c), c + 1)
  end

  defp convert_to_hex(p, [h | t], row, acc, c)
       when is_integer(h) and
              rem(c, 16) === 0 do
    convert_to_hex(p, t, update_row(h, []), prepend_row(p, h, row, acc, c), c + 1)
  end

  defp convert_to_hex(p, [h | t], row, acc, c)
       when is_integer(h) and
              rem(c, 8) === 7 do
    convert_to_hex(p, t, update_row(h, row), prepend_eighths_hex(h, acc), c + 1)
  end

  defp convert_to_hex(p, [h | t], row, acc, c) when is_integer(h) do
    convert_to_hex(p, t, update_row(h, row), prepend_hex(h, acc), c + 1)
  end

  defp row_prefix(_, n) do
    s =
      :string.pad(
        :string.to_lower(
          :erlang.integer_to_list(
            n,
            16
          )
        ),
        4,
        :leading,
        ?0
      )

    :lists.reverse(:lists.flatten(s ++ ' - '))
  end

  defp end_row(_, row) do
    row ++ '  '
  end

  defp calculate_padding(row, acc) do
    nNL = div(length(acc), 75) * length(:io_lib.nl())
    length = rem(length(acc) - nNL, 75)
    paddedLength = 75 - (16 - length(row))
    paddedLength - length
  end

  defp update_row(b, row) when is_binary(b) do
    case :erlang.binary_to_list(b) do
      [c] when 32 <= c and c <= 126 ->
        [c | row]

      _Else ->
        [?. | row]
    end
  end

  defp update_row(c, row) when 32 <= c and c <= 126 do
    [c | row]
  end

  defp update_row(_, row) do
    [?. | row]
  end

  defp prepend_first_row(p, a, b, acc, c) do
    prepend_hex(a, b, row_prefix(p, c) ++ acc)
  end

  defp prepend_first_row(p, n, acc, c) do
    prepend_hex(n, row_prefix(p, c) ++ acc)
  end

  defp prepend_row(p, a, b, row, acc, c) do
    prepend_hex(
      a,
      b,
      row_prefix(p, c) ++
        :io_lib.nl() ++
        end_row(
          p,
          row
        ) ++ acc
    )
  end

  defp prepend_row(p, n, row, acc, c) do
    prepend_hex(
      n,
      row_prefix(p, c) ++
        :io_lib.nl() ++
        end_row(
          p,
          row
        ) ++ acc
    )
  end

  defp prepend_hex(a, b, acc) do
    [
      ?\s,
      cond do
        b >= 0 and b <= 9 ->
          b + ?0

        b >= 10 and b <= 15 ->
          b + ?a - 10
      end,
      cond do
        a >= 0 and a <= 9 ->
          a + ?0

        a >= 10 and a <= 15 ->
          a + ?a - 10
      end
      | acc
    ]
  end

  defp prepend_hex(n, acc) do
    ' ' ++ number_to_hex(n) ++ acc
  end

  defp prepend_eighths_hex(a, b, acc) do
    [
      ?\s,
      ?\s,
      cond do
        b >= 0 and b <= 9 ->
          b + ?0

        b >= 10 and b <= 15 ->
          b + ?a - 10
      end,
      cond do
        a >= 0 and a <= 9 ->
          a + ?0

        a >= 10 and a <= 15 ->
          a + ?a - 10
      end
      | acc
    ]
  end

  defp prepend_eighths_hex(n, acc) do
    '  ' ++ number_to_hex(n) ++ acc
  end

  defp number_to_hex(n) do
    case :string.to_lower(
           :erlang.integer_to_list(
             n,
             16
           )
         ) do
      h when length(h) < 2 ->
        :lists.append(h, '0')

      h ->
        :lists.reverse(h)
    end
  end

  defp format_uknown_cipher_suite(
         <<x::size(8)-unsigned-big-integer, y::size(8)-unsigned-big-integer>>
       ) do
    '0x' ++ number_to_hex(x) ++ '0x' ++ number_to_hex(y)
  end
end
