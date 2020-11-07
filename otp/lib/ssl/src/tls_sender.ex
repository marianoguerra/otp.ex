defmodule :m_tls_sender do
  use Bitwise
  @behaviour :gen_statem
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

  Record.defrecord(:r_sslsocket, :sslsocket, fd: nil, pid: nil)

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

  Record.defrecord(:r_static, :static,
    connection_pid: :undefined,
    role: :undefined,
    socket: :undefined,
    socket_options: :undefined,
    trackers: :undefined,
    transport_cb: :undefined,
    negotiated_version: :undefined,
    renegotiate_at: :undefined,
    key_update_at: :undefined,
    bytes_sent: :undefined,
    connection_monitor: :undefined,
    dist_handle: :undefined,
    log_level: :undefined
  )

  Record.defrecord(:r_data, :data,
    static: :EFE_TODO_NESTED_RECORD,
    connection_states: %{}
  )

  def start() do
    :gen_statem.start(:tls_sender, [], [])
  end

  def start(spawnOpts) do
    :gen_statem.start(:tls_sender, [], spawnOpts)
  end

  def initialize(pid, initMsg) do
    :gen_statem.call(pid, {self(), initMsg})
  end

  def send_data(pid, appData) do
    call(pid, {:application_data, appData})
  end

  def send_post_handshake(pid, handshakeData) do
    call(pid, {:post_handshake_data, handshakeData})
  end

  def send_alert(pid, alert) do
    :gen_statem.cast(pid, alert)
  end

  def send_and_ack_alert(pid, alert) do
    :gen_statem.call(pid, {:ack_alert, alert}, 5000)
  end

  def setopts(pid, opts) do
    call(pid, {:set_opts, opts})
  end

  def renegotiate(pid) do
    call(pid, :renegotiate)
  end

  def peer_renegotiate(pid) do
    :gen_statem.call(pid, :renegotiate, 5000)
  end

  def update_connection_state(pid, newState, version) do
    :gen_statem.cast(pid, {:new_write, newState, version})
  end

  def downgrade(pid, timeout) do
    try do
      :gen_statem.call(pid, :downgrade, timeout)
    catch
      _, _ ->
        {:error, :timeout}
    else
      result ->
        result
    end
  end

  def dist_handshake_complete(connectionPid, node, dHandle) do
    :gen_statem.call(
      connectionPid,
      {:dist_handshake_complete, node, dHandle}
    )
  end

  def dist_tls_socket(pid) do
    :gen_statem.call(pid, :dist_get_tls_socket)
  end

  def callback_mode() do
    :state_functions
  end

  def init(_) do
    {:ok, :init, r_data()}
  end

  def init(
        {:call, from},
        {pid,
         %{
           :current_write => writeState,
           :role => role,
           :socket => socket,
           :socket_options => sockOpts,
           :trackers => trackers,
           :transport_cb => transport,
           :negotiated_version => version,
           :renegotiate_at => renegotiateAt,
           :key_update_at => keyUpdateAt,
           :log_level => logLevel
         }},
        r_data(
          connection_states: connectionStates,
          static: static0
        ) = stateData0
      ) do
    monitor = :erlang.monitor(:process, pid)

    stateData =
      r_data(stateData0,
        connection_states: %{connectionStates | :current_write => writeState},
        static:
          r_static(static0,
            connection_pid: pid,
            connection_monitor: monitor,
            role: role,
            socket: socket,
            socket_options: sockOpts,
            trackers: trackers,
            transport_cb: transport,
            negotiated_version: version,
            renegotiate_at: renegotiateAt,
            key_update_at: keyUpdateAt,
            bytes_sent: 0,
            log_level: logLevel
          )
      )

    {:next_state, :handshake, stateData, [{:reply, from, :ok}]}
  end

  def init(_, _, _) do
    {:keep_state_and_data, [:postpone]}
  end

  def connection(
        {:call, from},
        {:application_data, appData},
        r_data(static: r_static(socket_options: r_socket_options(packet: packet))) = stateData
      ) do
    case encode_packet(packet, appData) do
      {:error, _} = error ->
        {:next_state, :connection, stateData, [{:reply, from, error}]}

      data ->
        send_application_data(data, from, :connection, stateData)
    end
  end

  def connection({:call, from}, {:post_handshake_data, hSData}, stateData) do
    send_post_handshake_data(hSData, from, :connection, stateData)
  end

  def connection({:call, from}, {:ack_alert, r_alert() = alert}, stateData0) do
    stateData = send_tls_alert(alert, stateData0)
    {:next_state, :connection, stateData, [{:reply, from, :ok}]}
  end

  def connection(
        {:call, from},
        :renegotiate,
        r_data(connection_states: %{:current_write => write}) = stateData
      ) do
    {:next_state, :handshake, stateData, [{:reply, from, {:ok, write}}]}
  end

  def connection(
        {:call, from},
        :downgrade,
        r_data(connection_states: %{:current_write => write}) = stateData
      ) do
    {:next_state, :death_row, stateData, [{:reply, from, {:ok, write}}]}
  end

  def connection({:call, from}, {:set_opts, opts}, stateData) do
    handle_set_opts(from, opts, stateData)
  end

  def connection(
        {:call, from},
        :dist_get_tls_socket,
        r_data(
          static:
            r_static(
              transport_cb: transport,
              socket: socket,
              connection_pid: pid,
              trackers: trackers
            )
        ) = stateData
      ) do
    tLSSocket = :tls_connection.socket([pid, self()], transport, socket, trackers)
    {:next_state, :connection, stateData, [{:reply, from, {:ok, tLSSocket}}]}
  end

  def connection(
        {:call, from},
        {:dist_handshake_complete, _Node, dHandle},
        r_data(static: r_static(connection_pid: pid) = static) = stateData
      ) do
    false = :erlang.dist_ctrl_set_opt(dHandle, :get_size, true)
    :ok = :erlang.dist_ctrl_input_handler(dHandle, pid)

    :ok =
      :ssl_connection.dist_handshake_complete(
        pid,
        dHandle
      )

    :erlang.process_flag(:priority, :normal)

    {:keep_state, r_data(stateData, static: r_static(static, dist_handle: dHandle)),
     [
       {:reply, from, :ok}
       | case dist_data(dHandle) do
           [] ->
             []

           data ->
             [{:next_event, :internal, {:application_packets, {self(), :undefined}, data}}]
         end
     ]}
  end

  def connection(:internal, {:application_packets, from, data}, stateData) do
    send_application_data(data, from, :connection, stateData)
  end

  def connection(:internal, {:post_handshake_data, from, hSData}, stateData) do
    send_post_handshake_data(hSData, from, :connection, stateData)
  end

  def connection(:cast, r_alert() = alert, stateData0) do
    stateData = send_tls_alert(alert, stateData0)
    {:next_state, :connection, stateData}
  end

  def connection(
        :cast,
        {:new_write, writesState, version},
        r_data(
          connection_states: connectionStates,
          static: static
        ) = stateData
      ) do
    {:next_state, :connection,
     r_data(stateData,
       connection_states: %{connectionStates | :current_write => writesState},
       static: r_static(static, negotiated_version: version)
     )}
  end

  def connection(:info, :dist_data, r_data(static: r_static(dist_handle: dHandle))) do
    {:keep_state_and_data,
     case dist_data(dHandle) do
       [] ->
         []

       data ->
         [{:next_event, :internal, {:application_packets, {self(), :undefined}, data}}]
     end}
  end

  def connection(:info, :tick, stateData) do
    consume_ticks()
    data = [<<0::size(32)>>]
    from = {self(), :undefined}
    send_application_data(data, from, :connection, stateData)
  end

  def connection(:info, {:send, from, ref, data}, _StateData) do
    send(from, {ref, :ok})

    {:keep_state_and_data,
     [
       {:next_event, {:call, {self(), :undefined}},
        {:application_data, :erlang.iolist_to_iovec(data)}}
     ]}
  end

  def connection(type, msg, stateData) do
    handle_common(type, msg, stateData)
  end

  def handshake({:call, from}, {:set_opts, opts}, stateData) do
    handle_set_opts(from, opts, stateData)
  end

  def handshake({:call, _}, _, _) do
    {:keep_state_and_data, [:postpone]}
  end

  def handshake(:internal, {:application_packets, _, _}, _) do
    {:keep_state_and_data, [:postpone]}
  end

  def handshake(
        :cast,
        {:new_write, writeState, version},
        r_data(
          connection_states: connectionStates,
          static: r_static(key_update_at: keyUpdateAt0) = static
        ) = stateData
      ) do
    keyUpdateAt = key_update_at(version, writeState, keyUpdateAt0)

    {:next_state, :connection,
     r_data(stateData,
       connection_states: %{connectionStates | :current_write => writeState},
       static:
         r_static(static,
           negotiated_version: version,
           key_update_at: keyUpdateAt
         )
     )}
  end

  def handshake(:info, :dist_data, _) do
    {:keep_state_and_data, [:postpone]}
  end

  def handshake(:info, :tick, _) do
    consume_ticks()
    :keep_state_and_data
  end

  def handshake(:info, {:send, _, _, _}, _) do
    {:keep_state_and_data, [:postpone]}
  end

  def handshake(type, msg, stateData) do
    handle_common(type, msg, stateData)
  end

  def death_row(:state_timeout, reason, _State) do
    {:stop, {:shutdown, reason}}
  end

  def death_row(_Type, _Msg, _State) do
    :keep_state_and_data
  end

  def terminate(_Reason, _State, _Data) do
    :void
  end

  def code_change(_OldVsn, state, data, _Extra) do
    {:ok, state, data}
  end

  defp handle_set_opts(
         from,
         opts,
         r_data(static: r_static(socket_options: sockOpts) = static) = stateData
       ) do
    {:keep_state,
     r_data(stateData,
       static:
         r_static(static,
           socket_options:
             set_opts(
               sockOpts,
               opts
             )
         )
     ), [{:reply, from, :ok}]}
  end

  defp handle_common(
         {:call, from},
         {:set_opts, opts},
         r_data(static: r_static(socket_options: sockOpts) = static) = stateData
       ) do
    {:keep_state,
     r_data(stateData,
       static:
         r_static(static,
           socket_options:
             set_opts(
               sockOpts,
               opts
             )
         )
     ), [{:reply, from, :ok}]}
  end

  defp handle_common(
         :info,
         {:DOWN, monitor, _, _, reason},
         r_data(
           static:
             r_static(
               connection_monitor: monitor,
               dist_handle: handle
             )
         ) = stateData
       )
       when handle !== :undefined do
    {:next_state, :death_row, stateData, [{:state_timeout, 5000, reason}]}
  end

  defp handle_common(
         :info,
         {:DOWN, monitor, _, _, _},
         r_data(static: r_static(connection_monitor: monitor)) = stateData
       ) do
    {:stop, :normal, stateData}
  end

  defp handle_common(:info, msg, r_data(static: r_static(log_level: level))) do
    :ssl_logger.log(
      :info,
      level,
      %{:event => 'TLS sender recived unexpected info', :reason => [{:message, msg}]},
      %{
        :mfa => {:tls_sender, :handle_common, 3},
        :line => 420,
        :file => 'otp/lib/ssl/src/tls_sender.erl'
      }
    )

    :keep_state_and_data
  end

  defp handle_common(type, msg, r_data(static: r_static(log_level: level))) do
    :ssl_logger.log(
      :error,
      level,
      %{
        :event => 'TLS sender recived unexpected event',
        :reason => [{:type, type}, {:message, msg}]
      },
      %{
        :mfa => {:tls_sender, :handle_common, 3},
        :line => 424,
        :file => 'otp/lib/ssl/src/tls_sender.erl'
      }
    )

    :keep_state_and_data
  end

  defp send_tls_alert(
         r_alert() = alert,
         r_data(
           static:
             r_static(
               negotiated_version: version,
               socket: socket,
               transport_cb: transport,
               log_level: logLevel
             ),
           connection_states: connectionStates0
         ) = stateData0
       ) do
    {binMsg, connectionStates} =
      :tls_record.encode_alert_record(
        alert,
        version,
        connectionStates0
      )

    :tls_socket.send(transport, socket, binMsg)
    :ssl_logger.debug(logLevel, :outbound, :record, binMsg)
    r_data(stateData0, connection_states: connectionStates)
  end

  defp send_application_data(
         data,
         from,
         stateName,
         r_data(
           static:
             r_static(
               connection_pid: pid,
               socket: socket,
               dist_handle: distHandle,
               negotiated_version: version,
               transport_cb: transport,
               renegotiate_at: renegotiateAt,
               key_update_at: keyUpdateAt,
               bytes_sent: bytesSent,
               log_level: logLevel
             ),
           connection_states: connectionStates0
         ) = stateData0
       ) do
    case time_to_rekey(version, data, connectionStates0, renegotiateAt, keyUpdateAt, bytesSent) do
      :key_update ->
        keyUpdate = :tls_handshake_1_3.key_update(:update_requested)

        {:keep_state_and_data,
         [
           {:next_event, :internal, {:post_handshake_data, from, keyUpdate}},
           {:next_event, :internal, {:key_update, from}},
           {:next_event, :internal, {:application_packets, from, data}}
         ]}

      :renegotiate ->
        :ssl_connection.internal_renegotiation(
          pid,
          connectionStates0
        )

        {:next_state, :handshake, stateData0,
         [{:next_event, :internal, {:application_packets, from, data}}]}

      :chunk_and_key_update ->
        keyUpdate = :tls_handshake_1_3.key_update(:update_requested)
        {chunk, rest} = chunk_data(data, keyUpdateAt)

        {:keep_state_and_data,
         [
           {:next_event, :internal, {:post_handshake_data, from, keyUpdate}},
           {:next_event, :internal, {:application_packets, from, chunk}},
           {:next_event, :internal, {:application_packets, from, rest}}
         ]}

      false ->
        {msgs, connectionStates} =
          :tls_record.encode_data(
            data,
            version,
            connectionStates0
          )

        stateData = r_data(stateData0, connection_states: connectionStates)

        case :tls_socket.send(transport, socket, msgs) do
          :ok when distHandle !== :undefined ->
            :ssl_logger.debug(logLevel, :outbound, :record, msgs)
            stateData1 = update_bytes_sent(version, stateData, data)
            {:next_state, stateName, stateData1, []}

          reason when distHandle !== :undefined ->
            {:next_state, :death_row, stateData, [{:state_timeout, 5000, reason}]}

          :ok ->
            :ssl_logger.debug(logLevel, :outbound, :record, msgs)
            stateData1 = update_bytes_sent(version, stateData, data)
            {:next_state, stateName, stateData1, [{:reply, from, :ok}]}

          result ->
            {:next_state, stateName, stateData, [{:reply, from, result}]}
        end
    end
  end

  defp send_post_handshake_data(
         handshake,
         from,
         stateName,
         r_data(
           static:
             r_static(
               socket: socket,
               dist_handle: distHandle,
               negotiated_version: version,
               transport_cb: transport,
               log_level: logLevel
             ),
           connection_states: connectionStates0
         ) = stateData0
       ) do
    binHandshake =
      :tls_handshake.encode_handshake(
        handshake,
        version
      )

    {encoded, connectionStates} =
      :tls_record.encode_handshake(
        binHandshake,
        version,
        connectionStates0
      )

    :ssl_logger.debug(logLevel, :outbound, :handshake, handshake)
    stateData1 = r_data(stateData0, connection_states: connectionStates)

    case :tls_socket.send(transport, socket, encoded) do
      :ok when distHandle !== :undefined ->
        :ssl_logger.debug(logLevel, :outbound, :record, encoded)

        stateData =
          maybe_update_cipher_key(
            stateData1,
            handshake
          )

        {:next_state, stateName, stateData, []}

      reason when distHandle !== :undefined ->
        {:next_state, :death_row, stateData1, [{:state_timeout, 5000, reason}]}

      :ok ->
        :ssl_logger.debug(logLevel, :outbound, :record, encoded)

        stateData =
          maybe_update_cipher_key(
            stateData1,
            handshake
          )

        {:next_state, stateName, stateData, [{:reply, from, :ok}]}

      result ->
        {:next_state, stateName, stateData1, [{:reply, from, result}]}
    end
  end

  defp maybe_update_cipher_key(
         r_data(
           connection_states: connectionStates0,
           static: static0
         ) = stateData,
         r_key_update()
       ) do
    connectionStates =
      :tls_connection.update_cipher_key(
        :current_write,
        connectionStates0
      )

    static = r_static(static0, bytes_sent: 0)

    r_data(stateData,
      connection_states: connectionStates,
      static: static
    )
  end

  defp maybe_update_cipher_key(stateData, _) do
    stateData
  end

  defp update_bytes_sent(version, stateData, _) when version < {3, 4} do
    stateData
  end

  defp update_bytes_sent(
         _,
         r_data(static: r_static(key_update_at: :seq_num_wrap)) = stateData,
         _
       ) do
    stateData
  end

  defp update_bytes_sent(
         _,
         r_data(static: r_static(bytes_sent: sent) = static) = stateData,
         data
       ) do
    r_data(stateData, static: r_static(static, bytes_sent: sent + :erlang.iolist_size(data)))
  end

  defp key_update_at(
         version,
         %{:security_parameters => r_security_parameters(bulk_cipher_algorithm: cipherAlgo)},
         keyUpdateAt
       )
       when version >= {3, 4} do
    case cipherAlgo do
      8 ->
        keyUpdateAt

      9 ->
        :seq_num_wrap

      10 ->
        keyUpdateAt

      11 ->
        keyUpdateAt
    end
  end

  defp key_update_at(_, _, keyUpdateAt) do
    keyUpdateAt
  end

  defp encode_packet(packet, data) do
    len = :erlang.iolist_size(data)

    case packet do
      1 when len < 1 <<< 8 ->
        [<<len::size(8)>> | data]

      2 when len < 1 <<< 16 ->
        [<<len::size(16)>> | data]

      4 when len < 1 <<< 32 ->
        [<<len::size(32)>> | data]

      n when n === 1 or n === 2 or n === 4 ->
        {:error, {:badarg, {:packet_to_large, len, 1 <<< ((packet <<< 3) - 1)}}}

      _ ->
        data
    end
  end

  defp set_opts(socketOptions, [{:packet, n}]) do
    r_socket_options(socketOptions, packet: n)
  end

  defp time_to_rekey(
         version,
         _Data,
         %{:current_write => %{:sequence_number => 18_446_744_073_709_551_615}},
         _,
         _,
         _
       )
       when version >= {3, 4} do
    :key_update
  end

  defp time_to_rekey(version, _Data, _, _, :seq_num_wrap, _)
       when version >= {3, 4} do
    false
  end

  defp time_to_rekey(version, data, _, _, keyUpdateAt, bytesSent)
       when version >= {3, 4} do
    dataSize = :erlang.iolist_size(data)

    case bytesSent + dataSize > keyUpdateAt do
      true ->
        case dataSize > keyUpdateAt do
          true ->
            :chunk_and_key_update

          false ->
            :key_update
        end

      false ->
        false
    end
  end

  defp time_to_rekey(
         _,
         _Data,
         %{:current_write => %{:sequence_number => num}},
         renegotiateAt,
         _,
         _
       ) do
    is_time_to_renegotiate(num, renegotiateAt)
  end

  defp chunk_data(data, size) do
    {chunk, rest} =
      :erlang.split_binary(
        :erlang.iolist_to_binary(data),
        size
      )

    {[chunk], [rest]}
  end

  defp is_time_to_renegotiate(n, m) when n < m do
    false
  end

  defp is_time_to_renegotiate(_, _) do
    :renegotiate
  end

  defp call(fsmPid, event) do
    try do
      :gen_statem.call(fsmPid, event)
    catch
      :exit, {:noproc, _} ->
        {:error, :closed}

      :exit, {:normal, _} ->
        {:error, :closed}

      :exit, {{:shutdown, _}, _} ->
        {:error, :closed}
    end
  end

  defp dist_data(dHandle) do
    dist_data(dHandle, 0)
  end

  defp dist_data(dHandle, curBytes) do
    case :erlang.dist_ctrl_get_data(dHandle) do
      :none ->
        :erlang.dist_ctrl_get_data_notification(dHandle)
        []

      {len, data} when len + curBytes >= 16 * 1024 * 1024 ->
        :erlang.dist_ctrl_get_data_notification(dHandle)
        [<<len::size(32)>> | data]

      {len, data} ->
        packet = [<<len::size(32)>> | data]

        case dist_data(dHandle, curBytes + len) do
          [] ->
            packet

          more ->
            packet ++ more
        end
    end
  end

  defp consume_ticks() do
    receive do
      :tick ->
        consume_ticks()
    after
      0 ->
        :ok
    end
  end
end
