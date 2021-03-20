defmodule :m_tls_connection do
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

  Record.defrecord(:r_srp_user, :srp_user,
    generator: :undefined,
    prime: :undefined,
    salt: :undefined,
    verifier: :undefined
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

  Record.defrecord(:r_static_env, :static_env,
    role: :undefined,
    transport_cb: :undefined,
    protocol_cb: :undefined,
    data_tag: :undefined,
    close_tag: :undefined,
    error_tag: :undefined,
    passive_tag: :undefined,
    host: :undefined,
    port: :undefined,
    socket: :undefined,
    cert_db: :undefined,
    session_cache: :undefined,
    session_cache_cb: :undefined,
    crl_db: :undefined,
    file_ref_db: :undefined,
    cert_db_ref: :undefined,
    trackers: :undefined
  )

  Record.defrecord(:r_handshake_env, :handshake_env,
    client_hello_version: :undefined,
    unprocessed_handshake_events: 0,
    tls_handshake_history: :undefined,
    expecting_finished: false,
    renegotiation: :undefined,
    resumption: false,
    change_cipher_spec_sent: false,
    sni_guided_cert_selection: false,
    allow_renegotiate: true,
    hello: :undefined,
    sni_hostname: :undefined,
    max_frag_enum: :undefined,
    expecting_next_protocol_negotiation: false,
    next_protocol: :undefined,
    alpn: :undefined,
    negotiated_protocol: :undefined,
    hashsign_algorithm: {:undefined, :undefined},
    cert_hashsign_algorithm: {:undefined, :undefined},
    kex_algorithm: :undefined,
    kex_keys: :undefined,
    diffie_hellman_params: :undefined,
    srp_params: :undefined,
    public_key_info: :undefined,
    premaster_secret: :undefined,
    server_psk_identity: :undefined,
    cookie_iv_shard: :undefined,
    ocsp_stapling_state: %{ocsp_stapling: false, ocsp_expect: :no_staple}
  )

  Record.defrecord(:r_connection_env, :connection_env,
    user_application: :undefined,
    downgrade: :undefined,
    terminated: false,
    negotiated_version: :undefined,
    erl_dist_handle: :undefined,
    private_key: :undefined
  )

  Record.defrecord(:r_state, :state,
    static_env: :undefined,
    connection_env: :undefined,
    ssl_options: :undefined,
    socket_options: :undefined,
    handshake_env: :undefined,
    flight_buffer: [],
    client_certificate_requested: false,
    protocol_specific: %{},
    session: :undefined,
    key_share: :undefined,
    connection_states: :undefined,
    protocol_buffers: :undefined,
    user_data_buffer: :undefined,
    bytes_to_read: :undefined,
    start_or_recv_from: :undefined,
    log_level: :undefined
  )

  Record.defrecord(:r_ssl_tls, :ssl_tls,
    type: :undefined,
    version: :undefined,
    fragment: :undefined
  )

  Record.defrecord(:r_protocol_buffers, :protocol_buffers,
    tls_record_buffer: <<>>,
    tls_handshake_buffer: <<>>,
    tls_cipher_texts: []
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

  Record.defrecord(:r_alert, :alert,
    level: :undefined,
    description: :undefined,
    where: :undefined,
    role: :undefined,
    reason: :undefined
  )

  Record.defrecord(:r_sslsocket, :sslsocket, fd: nil, pid: nil)

  def start_fsm(
        role,
        host,
        port,
        socket,
        {%{erl_dist: false}, _, trackers} = opts,
        user,
        {cbModule, _, _, _, _} = cbInfo,
        timeout
      ) do
    try do
      {:ok, sender} = :tls_sender.start()

      {:ok, pid} =
        :tls_connection_sup.start_child([role, sender, host, port, socket, opts, user, cbInfo])

      {:ok, sslSocket} =
        :ssl_connection.socket_control(:tls_connection, socket, [pid, sender], cbModule, trackers)

      :ssl_connection.handshake(sslSocket, timeout)
    catch
      :error, {:badmatch, {:error, _} = error} ->
        error
    end
  end

  def start_fsm(
        role,
        host,
        port,
        socket,
        {%{erl_dist: true}, _, trackers} = opts,
        user,
        {cbModule, _, _, _, _} = cbInfo,
        timeout
      ) do
    try do
      {:ok, sender} = :tls_sender.start([{:spawn_opt, [{:priority, :max}]}])

      {:ok, pid} =
        :tls_connection_sup.start_child_dist([
          role,
          sender,
          host,
          port,
          socket,
          opts,
          user,
          cbInfo
        ])

      {:ok, sslSocket} =
        :ssl_connection.socket_control(:tls_connection, socket, [pid, sender], cbModule, trackers)

      :ssl_connection.handshake(sslSocket, timeout)
    catch
      :error, {:badmatch, {:error, _} = error} ->
        error
    end
  end

  def start_link(role, sender, host, port, socket, options, user, cbInfo) do
    {:ok,
     :proc_lib.spawn_link(:tls_connection, :init, [
       [role, sender, host, port, socket, options, user, cbInfo]
     ])}
  end

  def init([
        role,
        sender,
        host,
        port,
        socket,
        {%{erl_dist: erlDist}, _, _} = options,
        user,
        cbInfo
      ]) do
    :erlang.process_flag(:trap_exit, true)
    :erlang.link(sender)

    case erlDist do
      true ->
        :erlang.process_flag(:priority, :max)

      _ ->
        :ok
    end

    state0 =
      r_state(protocol_specific: map) =
      initial_state(role, sender, host, port, socket, options, user, cbInfo)

    try do
      state = :ssl_connection.ssl_config(r_state(state0, :ssl_options), role, state0)
      initialize_tls_sender(state)
      :gen_statem.enter_loop(:tls_connection, [], :init, state)
    catch
      error ->
        eState = r_state(state0, protocol_specific: Map.put(map, :error, error))
        :gen_statem.enter_loop(:tls_connection, [], :error, eState)
    end
  end

  def pids(r_state(protocol_specific: %{sender: sender})) do
    [self(), sender]
  end

  defp next_record(
         _,
         r_state(handshake_env: r_handshake_env(unprocessed_handshake_events: n) = hsEnv) = state
       )
       when n > 0 do
    {:no_record,
     r_state(state, handshake_env: r_handshake_env(hsEnv, unprocessed_handshake_events: n - 1))}
  end

  defp next_record(
         _,
         r_state(
           protocol_buffers:
             r_protocol_buffers(
               tls_cipher_texts:
                 [
                   _
                   | _
                 ] = cipherTexts
             ),
           connection_states: connectionStates,
           ssl_options: %{padding_check: check}
         ) = state
       ) do
    next_record(state, cipherTexts, connectionStates, check)
  end

  defp next_record(
         :connection,
         r_state(
           protocol_buffers: r_protocol_buffers(tls_cipher_texts: []),
           protocol_specific: %{active_n_toggle: true}
         ) = state
       ) do
    flow_ctrl(state)
  end

  defp next_record(
         _,
         r_state(
           protocol_buffers: r_protocol_buffers(tls_cipher_texts: []),
           protocol_specific: %{active_n_toggle: true}
         ) = state
       ) do
    activate_socket(state)
  end

  defp next_record(_, state) do
    {:no_record, state}
  end

  defp flow_ctrl(
         r_state(
           user_data_buffer: {_, size, _},
           socket_options: r_socket_options(active: false),
           bytes_to_read: :undefined
         ) = state
       )
       when size !== 0 do
    {:no_record, state}
  end

  defp flow_ctrl(
         r_state(
           socket_options:
             r_socket_options(
               active: false,
               packet: packet
             )
         ) = state
       )
       when packet !== 0 and packet !== :raw do
    activate_socket(state)
  end

  defp flow_ctrl(
         r_state(
           user_data_buffer: {_, size, _},
           socket_options: r_socket_options(active: false),
           bytes_to_read: 0
         ) = state
       )
       when size == 0 do
    activate_socket(state)
  end

  defp flow_ctrl(
         r_state(
           user_data_buffer: {_, size, _},
           socket_options: r_socket_options(active: false),
           bytes_to_read: 0
         ) = state
       )
       when size !== 0 do
    {:no_record, state}
  end

  defp flow_ctrl(
         r_state(
           user_data_buffer: {_, size, _},
           socket_options: r_socket_options(active: false),
           bytes_to_read: bytesToRead
         ) = state
       )
       when bytesToRead > 0 do
    case size >= bytesToRead do
      true ->
        {:no_record, state}

      false ->
        activate_socket(state)
    end
  end

  defp flow_ctrl(state) do
    activate_socket(state)
  end

  defp activate_socket(
         r_state(
           protocol_specific: %{active_n_toggle: true, active_n: n} = protocolSpec,
           static_env: r_static_env(socket: socket, close_tag: closeTag, transport_cb: transport)
         ) = state
       ) do
    case :tls_socket.setopts(transport, socket, [{:active, n}]) do
      :ok ->
        {:no_record,
         r_state(state, protocol_specific: Map.put(protocolSpec, :active_n_toggle, false))}

      _ ->
        send(self(), {closeTag, socket})
        {:no_record, state}
    end
  end

  defp next_record(state, cipherTexts, connectionStates, check) do
    next_record(state, cipherTexts, connectionStates, check, [])
  end

  defp next_record(
         r_state(connection_env: r_connection_env(negotiated_version: {3, 4} = version)) = state,
         [cT | cipherTexts],
         connectionStates0,
         check,
         acc
       ) do
    case :tls_record.decode_cipher_text(version, cT, connectionStates0, check) do
      {r_ssl_tls(type: 23, fragment: fragment), connectionStates} ->
        case cipherTexts do
          [] ->
            next_record_done(
              state,
              [],
              connectionStates,
              r_ssl_tls(
                type: 23,
                fragment:
                  :erlang.iolist_to_binary(
                    :lists.reverse(
                      acc,
                      [fragment]
                    )
                  )
              )
            )

          [_ | _] ->
            next_record(state, cipherTexts, connectionStates, check, [fragment | acc])
        end

      {record, connectionStates} when acc === [] ->
        next_record_done(state, cipherTexts, connectionStates, record)

      {_Record, _ConnectionStates_to_forget} ->
        next_record_done(
          state,
          [cT | cipherTexts],
          connectionStates0,
          r_ssl_tls(
            type: 23,
            fragment: :erlang.iolist_to_binary(:lists.reverse(acc))
          )
        )

      r_alert() = alert ->
        alert
    end
  end

  defp next_record(
         r_state(connection_env: r_connection_env(negotiated_version: version)) = state,
         [r_ssl_tls(type: 23) = cT | cipherTexts],
         connectionStates0,
         check,
         acc
       ) do
    case :tls_record.decode_cipher_text(version, cT, connectionStates0, check) do
      {r_ssl_tls(type: 23, fragment: fragment), connectionStates} ->
        case cipherTexts do
          [] ->
            next_record_done(
              state,
              [],
              connectionStates,
              r_ssl_tls(
                type: 23,
                fragment:
                  :erlang.iolist_to_binary(
                    :lists.reverse(
                      acc,
                      [fragment]
                    )
                  )
              )
            )

          [_ | _] ->
            next_record(state, cipherTexts, connectionStates, check, [fragment | acc])
        end

      r_alert() = alert ->
        alert
    end
  end

  defp next_record(state, cipherTexts, connectionStates, _, [_ | _] = acc) do
    next_record_done(
      state,
      cipherTexts,
      connectionStates,
      r_ssl_tls(
        type: 23,
        fragment: :erlang.iolist_to_binary(:lists.reverse(acc))
      )
    )
  end

  defp next_record(
         r_state(connection_env: r_connection_env(negotiated_version: version)) = state,
         [cT | cipherTexts],
         connectionStates0,
         check,
         []
       ) do
    case :tls_record.decode_cipher_text(version, cT, connectionStates0, check) do
      {record, connectionStates} ->
        next_record_done(state, cipherTexts, connectionStates, record)

      r_alert() = alert ->
        alert
    end
  end

  defp next_record_done(
         r_state(protocol_buffers: buffers) = state,
         cipherTexts,
         connectionStates,
         record
       ) do
    {record,
     r_state(state,
       protocol_buffers: r_protocol_buffers(buffers, tls_cipher_texts: cipherTexts),
       connection_states: connectionStates
     )}
  end

  def next_event(stateName, record, state) do
    next_event(stateName, record, state, [])
  end

  def next_event(
        stateName,
        :no_record,
        r_state(static_env: r_static_env(role: role)) = state0,
        actions
      ) do
    case next_record(stateName, state0) do
      {:no_record, state} ->
        :ssl_connection.hibernate_after(stateName, state, actions)

      {record, state} ->
        next_event(stateName, record, state, actions)

      r_alert() = alert ->
        :ssl_connection.handle_normal_shutdown(r_alert(alert, role: role), stateName, state0)
        {:stop, {:shutdown, :own_alert}, state0}
    end
  end

  def next_event(stateName, r_ssl_tls() = record, state, actions) do
    {:next_state, stateName, state,
     [
       {:next_event, :internal, {:protocol_record, record}}
       | actions
     ]}
  end

  def next_event(stateName, r_alert() = alert, state, actions) do
    {:next_state, stateName, state, [{:next_event, :internal, alert} | actions]}
  end

  def handle_protocol_record(
        r_ssl_tls(type: 23, fragment: data),
        stateName,
        r_state(
          start_or_recv_from: from,
          socket_options: r_socket_options(active: false)
        ) = state0
      )
      when from !== :undefined do
    case :ssl_connection.read_application_data(
           data,
           state0
         ) do
      {:stop, _, _} = stop ->
        stop

      {record, r_state(start_or_recv_from: caller) = state} ->
        timerAction =
          case caller do
            :undefined ->
              [{{:timeout, :recv}, :infinity, :timeout}]

            _ ->
              []
          end

        next_event(stateName, record, state, timerAction)
    end
  end

  def handle_protocol_record(r_ssl_tls(type: 23, fragment: data), stateName, state0) do
    case :ssl_connection.read_application_data(
           data,
           state0
         ) do
      {:stop, _, _} = stop ->
        stop

      {record, state} ->
        next_event(stateName, record, state)
    end
  end

  def handle_protocol_record(
        r_ssl_tls(type: 22, fragment: data),
        stateName,
        r_state(
          protocol_buffers: r_protocol_buffers(tls_handshake_buffer: buf0) = buffers,
          connection_env: r_connection_env(negotiated_version: version),
          static_env: r_static_env(role: role),
          ssl_options: options
        ) = state0
      ) do
    try do
      effectiveVersion = effective_version(version, options, role)
      {packets, buf} = :tls_handshake.get_tls_handshake(effectiveVersion, data, buf0, options)

      state =
        r_state(state0, protocol_buffers: r_protocol_buffers(buffers, tls_handshake_buffer: buf))

      case packets do
        [] ->
          assert_buffer_sanity(buf, options)
          next_event(stateName, :no_record, state)

        _ ->
          events = tls_handshake_events(packets)

          case stateName do
            :connection ->
              :ssl_connection.hibernate_after(stateName, state, events)

            _ ->
              hsEnv = r_state(state, :handshake_env)

              {:next_state, stateName,
               r_state(state,
                 handshake_env:
                   r_handshake_env(hsEnv, unprocessed_handshake_events: unprocessed_events(events))
               ), events}
          end
      end
    catch
      r_alert() = alert ->
        :ssl_connection.handle_own_alert(alert, version, stateName, state0)
    end
  end

  def handle_protocol_record(r_ssl_tls(type: 20, fragment: data), stateName, state) do
    {:next_state, stateName, state, [{:next_event, :internal, r_change_cipher_spec(type: data)}]}
  end

  def handle_protocol_record(
        r_ssl_tls(type: 21, fragment: encAlerts),
        stateName,
        r_state(connection_env: r_connection_env(negotiated_version: version)) = state
      ) do
    try do
      decode_alerts(encAlerts)
    catch
      _, _ ->
        :ssl_connection.handle_own_alert(
          r_alert(
            level: 2,
            description: 40,
            where: %{
              mfa: {:tls_connection, :handle_protocol_record, 3},
              line: 402,
              file: 'otp/lib/ssl/src/tls_connection.erl'
            },
            reason: :alert_decode_error
          ),
          version,
          stateName,
          state
        )
    else
      alerts = [_ | _] ->
        handle_alerts(alerts, {:next_state, stateName, state})

      [] ->
        :ssl_connection.handle_own_alert(
          r_alert(
            level: 2,
            description: 40,
            where: %{
              mfa: {:tls_connection, :handle_protocol_record, 3},
              line: 396,
              file: 'otp/lib/ssl/src/tls_connection.erl'
            },
            reason: :empty_alert
          ),
          version,
          stateName,
          state
        )

      r_alert() = alert ->
        :ssl_connection.handle_own_alert(alert, version, stateName, state)
    end
  end

  def handle_protocol_record(r_ssl_tls(type: _Unknown), stateName, state) do
    {:next_state, stateName, state, []}
  end

  def renegotiation(pid, writeState) do
    :gen_statem.call(pid, {:user_renegotiate, writeState})
  end

  def renegotiate(
        r_state(
          static_env: r_static_env(role: :client),
          handshake_env: hsEnv
        ) = state,
        actions
      ) do
    hs0 = :ssl_handshake.init_handshake_history()

    {:next_state, :connection,
     r_state(state, handshake_env: r_handshake_env(hsEnv, tls_handshake_history: hs0)),
     [{:next_event, :internal, r_hello_request()} | actions]}
  end

  def renegotiate(
        r_state(
          static_env: r_static_env(role: :server, socket: socket, transport_cb: transport),
          handshake_env: hsEnv,
          connection_env: r_connection_env(negotiated_version: version),
          connection_states: connectionStates0
        ) = state0,
        actions
      ) do
    helloRequest = :ssl_handshake.hello_request()

    frag =
      :tls_handshake.encode_handshake(
        helloRequest,
        version
      )

    hs0 = :ssl_handshake.init_handshake_history()

    {binMsg, connectionStates} =
      :tls_record.encode_handshake(
        frag,
        version,
        connectionStates0
      )

    :tls_socket.send(transport, socket, binMsg)

    state =
      r_state(state0,
        connection_states: connectionStates,
        handshake_env: r_handshake_env(hsEnv, tls_handshake_history: hs0)
      )

    next_event(:hello, :no_record, state, actions)
  end

  def send_handshake(handshake, state) do
    send_handshake_flight(queue_handshake(handshake, state))
  end

  def queue_handshake(
        handshake,
        r_state(
          handshake_env: r_handshake_env(tls_handshake_history: hist0) = hsEnv,
          connection_env: r_connection_env(negotiated_version: version),
          flight_buffer: flight0,
          ssl_options: %{log_level: logLevel},
          connection_states: connectionStates0
        ) = state0
      ) do
    {binHandshake, connectionStates, hist} =
      encode_handshake(handshake, version, connectionStates0, hist0)

    :ssl_logger.debug(logLevel, :outbound, :handshake, handshake)
    :ssl_logger.debug(logLevel, :outbound, :record, binHandshake)

    r_state(state0,
      connection_states: connectionStates,
      handshake_env: r_handshake_env(hsEnv, tls_handshake_history: hist),
      flight_buffer: flight0 ++ [binHandshake]
    )
  end

  def send_handshake_flight(
        r_state(
          static_env:
            r_static_env(
              socket: socket,
              transport_cb: transport
            ),
          flight_buffer: flight
        ) = state0
      ) do
    :tls_socket.send(transport, socket, flight)
    {r_state(state0, flight_buffer: []), []}
  end

  def queue_change_cipher(
        msg,
        r_state(
          connection_env: r_connection_env(negotiated_version: version),
          flight_buffer: flight0,
          ssl_options: %{log_level: logLevel},
          connection_states: connectionStates0
        ) = state0
      ) do
    {binChangeCipher, connectionStates} = encode_change_cipher(msg, version, connectionStates0)
    :ssl_logger.debug(logLevel, :outbound, :record, binChangeCipher)

    r_state(state0,
      connection_states: connectionStates,
      flight_buffer: flight0 ++ [binChangeCipher]
    )
  end

  def reinit(
        r_state(
          protocol_specific: %{sender: sender},
          connection_env: r_connection_env(negotiated_version: version),
          connection_states: %{current_write: write}
        ) = state
      ) do
    :tls_sender.update_connection_state(sender, write, version)
    reinit_handshake_data(state)
  end

  def reinit_handshake_data(r_state(handshake_env: hsEnv) = state) do
    r_state(state,
      handshake_env:
        r_handshake_env(hsEnv,
          tls_handshake_history: :ssl_handshake.init_handshake_history(),
          public_key_info: :undefined,
          premaster_secret: :undefined
        )
    )
  end

  def select_sni_extension(r_client_hello(extensions: %{sni: sNI})) do
    sNI
  end

  def select_sni_extension(_) do
    :undefined
  end

  def empty_connection_state(connectionEnd, beastMitigation) do
    :ssl_record.empty_connection_state(
      connectionEnd,
      beastMitigation
    )
  end

  defp encode_alert(r_alert() = alert, version, connectionStates) do
    :tls_record.encode_alert_record(alert, version, connectionStates)
  end

  def send_alert(
        alert,
        r_state(
          static_env:
            r_static_env(
              socket: socket,
              transport_cb: transport
            ),
          connection_env: r_connection_env(negotiated_version: version),
          ssl_options: %{log_level: logLevel},
          connection_states: connectionStates0
        ) = stateData0
      ) do
    {binMsg, connectionStates} = encode_alert(alert, version, connectionStates0)
    :tls_socket.send(transport, socket, binMsg)
    :ssl_logger.debug(logLevel, :outbound, :record, binMsg)
    r_state(stateData0, connection_states: connectionStates)
  end

  def send_alert_in_connection(r_alert(level: 2) = alert, state) do
    send_sync_alert(alert, state)
  end

  def send_alert_in_connection(r_alert(description: 0) = alert, state) do
    send_sync_alert(alert, state)
  end

  def send_alert_in_connection(
        alert,
        r_state(protocol_specific: %{sender: sender})
      ) do
    :tls_sender.send_alert(sender, alert)
  end

  def send_sync_alert(
        alert,
        r_state(protocol_specific: %{sender: sender}) = state
      ) do
    try do
      :tls_sender.send_and_ack_alert(sender, alert)
    catch
      _, _ ->
        throw({:stop, {:shutdown, :own_alert}, state})
    end
  end

  def close({:close, timeout}, socket, transport = :gen_tcp, _, _) do
    :tls_socket.setopts(transport, socket, [{:active, false}])
    transport.shutdown(socket, :write)
    _ = transport.recv(socket, 0, timeout)
    :ok
  end

  def close({:shutdown, :transport_closed}, socket, transport = :gen_tcp, connectionStates, check) do
    close({:close, 0}, socket, transport, connectionStates, check)
  end

  def close({:shutdown, :own_alert}, socket, transport = :gen_tcp, connectionStates, check) do
    close({:close, 5000}, socket, transport, connectionStates, check)
  end

  def close(:downgrade, _, _, _, _) do
    :ok
  end

  def close(_, socket, transport, _, _) do
    :tls_socket.close(transport, socket)
  end

  def protocol_name() do
    'TLS'
  end

  def socket(pids, transport, socket, trackers) do
    :tls_socket.socket(pids, transport, socket, :tls_connection, trackers)
  end

  def setopts(transport, socket, other) do
    :tls_socket.setopts(transport, socket, other)
  end

  def getopts(transport, socket, tag) do
    :tls_socket.getopts(transport, socket, tag)
  end

  def init(
        {:call, from},
        {:start, timeout},
        r_state(
          static_env:
            r_static_env(
              role: :client,
              host: host,
              port: port,
              transport_cb: transport,
              socket: socket,
              session_cache: cache,
              session_cache_cb: cacheCb
            ),
          handshake_env:
            r_handshake_env(
              renegotiation: {renegotiation, _},
              ocsp_stapling_state: ocspState0
            ) = hsEnv,
          connection_env: cEnv,
          ssl_options:
            %{
              log_level: logLevel,
              versions: [helloVersion | _] = versions,
              session_tickets: sessionTickets,
              ocsp_stapling: ocspStaplingOpt,
              ocsp_nonce: ocspNonceOpt
            } = sslOpts,
          session: newSession,
          connection_states: connectionStates0
        ) = state0
      ) do
    keyShare = maybe_generate_client_shares(sslOpts)

    session =
      :ssl_session.client_select_session({host, port, sslOpts}, cache, cacheCb, newSession)

    {useTicket, state1} = :tls_handshake_1_3.maybe_automatic_session_resumption(state0)
    ticketData = :tls_handshake_1_3.get_ticket_data(self(), sessionTickets, useTicket)

    ocspNonce =
      :tls_handshake.ocsp_nonce(
        ocspNonceOpt,
        ocspStaplingOpt
      )

    hello =
      :tls_handshake.client_hello(
        host,
        port,
        connectionStates0,
        sslOpts,
        r_session(session, :session_id),
        renegotiation,
        r_session(session, :own_certificate),
        keyShare,
        ticketData,
        ocspNonce
      )

    handshake0 = :ssl_handshake.init_handshake_history()
    hello1 = :tls_handshake_1_3.maybe_add_binders(hello, ticketData, helloVersion)
    maxFragEnum = :maps.get(:max_frag_enum, r_client_hello(hello1, :extensions), :undefined)

    connectionStates1 =
      :ssl_record.set_max_fragment_length(
        maxFragEnum,
        connectionStates0
      )

    {binMsg, connectionStates, handshake} =
      encode_handshake(hello1, helloVersion, connectionStates1, handshake0)

    :tls_socket.send(transport, socket, binMsg)
    :ssl_logger.debug(logLevel, :outbound, :handshake, hello1)
    :ssl_logger.debug(logLevel, :outbound, :record, binMsg)
    requestedVersion = :tls_record.hello_version(versions)

    state =
      r_state(state1,
        connection_states: connectionStates,
        connection_env: r_connection_env(cEnv, negotiated_version: requestedVersion),
        session: session,
        handshake_env:
          r_handshake_env(hsEnv,
            tls_handshake_history: handshake,
            ocsp_stapling_state: Map.put(ocspState0, :ocsp_nonce, ocspNonce)
          ),
        start_or_recv_from: from,
        key_share: keyShare
      )

    next_event(:hello, :no_record, state, [{{:timeout, :handshake}, timeout, :close}])
  end

  def init(type, event, state) do
    gen_handshake(:init, type, event, state)
  end

  def error(
        {:call, from},
        {:start, _Timeout},
        r_state(protocol_specific: %{error: error}) = state
      ) do
    {:stop_and_reply, {:shutdown, :normal}, [{:reply, from, {:error, error}}], state}
  end

  def error({:call, _} = call, msg, state) do
    gen_handshake(:error, call, msg, state)
  end

  def error(_, _, _) do
    {:keep_state_and_data, [:postpone]}
  end

  def hello(
        :internal,
        r_client_hello(extensions: extensions) = hello,
        r_state(
          ssl_options: %{handshake: :hello},
          handshake_env: hsEnv,
          start_or_recv_from: from
        ) = state
      ) do
    {:next_state, :user_hello,
     r_state(state,
       start_or_recv_from: :undefined,
       handshake_env: r_handshake_env(hsEnv, hello: hello)
     ), [{:reply, from, {:ok, extensions}}]}
  end

  def hello(
        :internal,
        r_server_hello(extensions: extensions) = hello,
        r_state(
          ssl_options: %{handshake: :hello},
          handshake_env: hsEnv,
          start_or_recv_from: from
        ) = state
      ) do
    {:next_state, :user_hello,
     r_state(state,
       start_or_recv_from: :undefined,
       handshake_env: r_handshake_env(hsEnv, hello: hello)
     ), [{:reply, from, {:ok, extensions}}]}
  end

  def hello(
        :internal,
        r_client_hello(client_version: clientVersion) = hello,
        r_state(ssl_options: sslOpts0) = state0
      ) do
    case choose_tls_version(sslOpts0, hello) do
      :"tls_v1.3" ->
        {:next_state, :start, state0, [{:next_event, :internal, hello}]}

      :"tls_v1.2" ->
        case :ssl_connection.handle_sni_extension(
               state0,
               hello
             ) do
          r_state(
            connection_states: connectionStates0,
            static_env: r_static_env(trackers: trackers),
            handshake_env:
              r_handshake_env(
                kex_algorithm: keyExAlg,
                renegotiation: {renegotiation, _},
                negotiated_protocol: currentProtocol,
                sni_guided_cert_selection: sNICertSelection
              ) = hsEnv,
            connection_env: cEnv,
            session: r_session(own_certificate: cert) = session0,
            ssl_options: sslOpts
          ) = state ->
            sessionTracker =
              :proplists.get_value(
                :session_id_tracker,
                trackers
              )

            case :tls_handshake.hello(
                   hello,
                   sslOpts,
                   {sessionTracker, session0, connectionStates0, cert, keyExAlg},
                   renegotiation
                 ) do
              r_alert() = alert ->
                :ssl_connection.handle_own_alert(
                  alert,
                  clientVersion,
                  :hello,
                  r_state(state,
                    connection_env: r_connection_env(cEnv, negotiated_version: clientVersion)
                  )
                )

              {version, {type, session}, connectionStates, protocol0, serverHelloExt0, hashSign} ->
                protocol =
                  case protocol0 do
                    :undefined ->
                      currentProtocol

                    _ ->
                      protocol0
                  end

                serverHelloExt =
                  case sNICertSelection do
                    true ->
                      Map.put(serverHelloExt0, :sni, r_sni(hostname: ''))

                    false ->
                      serverHelloExt0
                  end

                gen_handshake(
                  :hello,
                  :internal,
                  {:common_client_hello, type, serverHelloExt},
                  r_state(state,
                    connection_states: connectionStates,
                    connection_env: r_connection_env(cEnv, negotiated_version: version),
                    handshake_env:
                      r_handshake_env(hsEnv,
                        hashsign_algorithm: hashSign,
                        client_hello_version: clientVersion,
                        negotiated_protocol: protocol
                      ),
                    session: session
                  )
                )
            end

          alert ->
            alert
        end
    end
  end

  def hello(
        :internal,
        r_server_hello() = hello,
        r_state(
          connection_states: connectionStates0,
          connection_env: r_connection_env(negotiated_version: reqVersion) = cEnv,
          static_env: r_static_env(role: :client),
          handshake_env:
            r_handshake_env(
              ocsp_stapling_state: ocspState0,
              renegotiation: {renegotiation, _}
            ) = hsEnv,
          session: r_session(session_id: oldId),
          ssl_options: sslOptions
        ) = state
      ) do
    case :tls_handshake.hello(hello, sslOptions, connectionStates0, renegotiation, oldId) do
      r_alert() = alert ->
        :ssl_connection.handle_own_alert(
          alert,
          reqVersion,
          :hello,
          r_state(state, connection_env: r_connection_env(cEnv, negotiated_version: reqVersion))
        )

      {version, newId, connectionStates, protoExt, protocol, ocspState} ->
        :ssl_connection.handle_session(
          hello,
          version,
          newId,
          connectionStates,
          protoExt,
          protocol,
          r_state(state,
            handshake_env:
              r_handshake_env(hsEnv,
                ocsp_stapling_state:
                  :maps.merge(
                    ocspState0,
                    ocspState
                  )
              )
          )
        )

      {:next_state, :wait_sh, selectedVersion, ocspState} ->
        {:next_state, :wait_sh,
         r_state(state,
           handshake_env:
             r_handshake_env(hsEnv,
               ocsp_stapling_state:
                 :maps.merge(
                   ocspState0,
                   ocspState
                 )
             ),
           connection_env: r_connection_env(cEnv, negotiated_version: selectedVersion)
         ), [{:next_event, :internal, hello}]}
    end
  end

  def hello(:info, event, state) do
    handle_info(event, :hello, state)
  end

  def hello(type, event, state) do
    gen_handshake(:hello, type, event, state)
  end

  def user_hello(type, event, state) do
    gen_handshake(:user_hello, type, event, state)
  end

  def abbreviated(:info, event, state) do
    gen_info(event, :abbreviated, state)
  end

  def abbreviated(type, event, state) do
    gen_handshake(:abbreviated, type, event, state)
  end

  def wait_ocsp_stapling(:info, event, state) do
    gen_info(event, :wait_ocsp_stapling, state)
  end

  def wait_ocsp_stapling(type, event, state) do
    gen_handshake(:wait_ocsp_stapling, type, event, state)
  end

  def certify(:info, event, state) do
    gen_info(event, :certify, state)
  end

  def certify(type, event, state) do
    gen_handshake(:certify, type, event, state)
  end

  def cipher(:info, event, state) do
    gen_info(event, :cipher, state)
  end

  def cipher(type, event, state) do
    gen_handshake(:cipher, type, event, state)
  end

  def connection(:info, event, state) do
    gen_info(event, :connection, state)
  end

  def connection(
        {:call, from},
        {:user_renegotiate, writeState},
        r_state(connection_states: connectionStates) = state
      ) do
    {:next_state, :connection,
     r_state(state, connection_states: Map.put(connectionStates, :current_write, writeState)),
     [{:next_event, {:call, from}, :renegotiate}]}
  end

  def connection(
        {:call, from},
        {:close, {pid, _Timeout}},
        r_state(
          connection_env: r_connection_env(terminated: :closed) = cEnv,
          protocol_specific: pS
        ) = state
      ) do
    {:next_state, :downgrade,
     r_state(state,
       connection_env:
         r_connection_env(cEnv,
           terminated: true,
           downgrade: {pid, from}
         ),
       protocol_specific: Map.merge(pS, %{active_n_toggle: true, active_n: 1})
     ),
     [
       {:next_event, :internal,
        r_alert(
          level: 1,
          description: 0,
          where: %{
            mfa: {:tls_connection, :connection, 3},
            line: 857,
            file: 'otp/lib/ssl/src/tls_connection.erl'
          }
        )}
     ]}
  end

  def connection(
        {:call, from},
        {:close, {pid, timeout}},
        r_state(
          connection_states: connectionStates,
          protocol_specific: %{sender: sender} = pS,
          connection_env: cEnv
        ) = state0
      ) do
    case :tls_sender.downgrade(sender, timeout) do
      {:ok, write} ->
        state =
          send_alert(
            r_alert(
              level: 1,
              description: 0,
              where: %{
                mfa: {:tls_connection, :connection, 3},
                line: 870,
                file: 'otp/lib/ssl/src/tls_connection.erl'
              }
            ),
            r_state(state0, connection_states: Map.put(connectionStates, :current_write, write))
          )

        {:next_state, :downgrade,
         r_state(state,
           connection_env:
             r_connection_env(cEnv,
               downgrade: {pid, from},
               terminated: true
             ),
           protocol_specific: Map.merge(pS, %{active_n_toggle: true, active_n: 1})
         ), [{:timeout, timeout, :downgrade}]}

      {:error, :timeout} ->
        {:stop_and_reply, {:shutdown, :downgrade_fail}, [{:reply, from, {:error, :timeout}}]}
    end
  end

  def connection(
        :internal,
        r_hello_request(),
        r_state(
          static_env:
            r_static_env(
              role: :client,
              host: host,
              port: port,
              session_cache: cache,
              session_cache_cb: cacheCb
            ),
          handshake_env:
            r_handshake_env(
              renegotiation: {renegotiation, :peer},
              ocsp_stapling_state: ocspState
            ),
          session: r_session(own_certificate: cert) = session0,
          ssl_options: sslOpts,
          protocol_specific: %{sender: pid},
          connection_states: connectionStates
        ) = state0
      ) do
    try do
      :tls_sender.peer_renegotiate(pid)
    catch
      _, _ ->
        {:stop, {:shutdown, :sender_blocked}, state0}
    else
      {:ok, write} ->
        session =
          :ssl_session.client_select_session({host, port, sslOpts}, cache, cacheCb, session0)

        hello =
          :tls_handshake.client_hello(
            host,
            port,
            connectionStates,
            sslOpts,
            r_session(session, :session_id),
            renegotiation,
            cert,
            :undefined,
            :undefined,
            :maps.get(:ocsp_nonce, ocspState, :undefined)
          )

        {state, actions} =
          send_handshake(
            hello,
            r_state(state0,
              connection_states: Map.put(connectionStates, :current_write, write),
              session: session
            )
          )

        next_event(:hello, :no_record, state, actions)
    end
  end

  def connection(
        :internal,
        r_hello_request(),
        r_state(
          static_env: r_static_env(role: :client, host: host, port: port),
          handshake_env:
            r_handshake_env(
              renegotiation: {renegotiation, _},
              ocsp_stapling_state: ocspState
            ),
          session: r_session(own_certificate: cert),
          ssl_options: sslOpts,
          connection_states: connectionStates
        ) = state0
      ) do
    hello =
      :tls_handshake.client_hello(
        host,
        port,
        connectionStates,
        sslOpts,
        <<>>,
        renegotiation,
        cert,
        :undefined,
        :undefined,
        :maps.get(:ocsp_nonce, ocspState, :undefined)
      )

    {state, actions} = send_handshake(hello, state0)
    next_event(:hello, :no_record, state, actions)
  end

  def connection(
        :internal,
        r_client_hello() = hello,
        r_state(
          static_env: r_static_env(role: :server),
          handshake_env: r_handshake_env(allow_renegotiate: true) = hsEnv,
          connection_states: cS,
          protocol_specific: %{sender: sender}
        ) = state
      ) do
    :erlang.send_after(12000, self(), :allow_renegotiate)
    {:ok, write} = :tls_sender.renegotiate(sender)

    next_event(
      :hello,
      :no_record,
      r_state(state,
        connection_states: Map.put(cS, :current_write, write),
        handshake_env:
          r_handshake_env(hsEnv,
            renegotiation: {true, :peer},
            allow_renegotiate: false
          )
      ),
      [{:next_event, :internal, hello}]
    )
  end

  def connection(
        :internal,
        r_client_hello(),
        r_state(
          static_env: r_static_env(role: :server),
          handshake_env: r_handshake_env(allow_renegotiate: false)
        ) = state0
      ) do
    alert =
      r_alert(
        level: 1,
        description: 100,
        where: %{
          mfa: {:tls_connection, :connection, 3},
          line: 947,
          file: 'otp/lib/ssl/src/tls_connection.erl'
        }
      )

    send_alert_in_connection(alert, state0)
    state = reinit_handshake_data(state0)
    next_event(:connection, :no_record, state)
  end

  def connection(:internal, r_new_session_ticket() = newSessionTicket, state) do
    handle_new_session_ticket(newSessionTicket, state)
    next_event(:connection, :no_record, state)
  end

  def connection(:internal, r_key_update() = keyUpdate, state0) do
    case handle_key_update(keyUpdate, state0) do
      {:ok, state} ->
        next_event(:connection, :no_record, state)

      {:error, state, alert} ->
        :ssl_connection.handle_own_alert(alert, {3, 4}, :connection, state)
        next_event(:connection, :no_record, state)
    end
  end

  def connection(type, event, state) do
    :ssl_connection.connection(type, event, state, :tls_connection)
  end

  def downgrade(
        :internal,
        r_alert(description: 0),
        r_state(
          static_env:
            r_static_env(
              transport_cb: transport,
              socket: socket
            ),
          connection_env: r_connection_env(downgrade: {pid, from})
        ) = state
      ) do
    :tls_socket.setopts(transport, socket, [{:active, false}, {:packet, 0}, {:mode, :binary}])
    transport.controlling_process(socket, pid)
    {:stop_and_reply, {:shutdown, :downgrade}, [{:reply, from, {:ok, socket}}], state}
  end

  def downgrade(
        :timeout,
        :downgrade,
        r_state(connection_env: r_connection_env(downgrade: {_, from})) = state
      ) do
    {:stop_and_reply, {:shutdown, :normal}, [{:reply, from, {:error, :timeout}}], state}
  end

  def downgrade(
        :info,
        {closeTag, socket},
        r_state(
          static_env: r_static_env(socket: socket, close_tag: closeTag),
          connection_env: r_connection_env(downgrade: {_, from})
        ) = state
      ) do
    {:stop_and_reply, {:shutdown, :normal}, [{:reply, from, {:error, closeTag}}], state}
  end

  def downgrade(:info, info, state) do
    handle_info(info, :downgrade, state)
  end

  def downgrade(type, event, state) do
    :ssl_connection.downgrade(type, event, state, :tls_connection)
  end

  def start(:info, event, state) do
    gen_info_1_3(event, :start, state)
  end

  def start(type, event, state) do
    gen_handshake_1_3(:start, type, event, state)
  end

  def negotiated(:info, event, state) do
    gen_info_1_3(event, :negotiated, state)
  end

  def negotiated(type, event, state) do
    gen_handshake_1_3(:negotiated, type, event, state)
  end

  def recvd_ch(:info, event, state) do
    gen_info_1_3(event, :recvd_ch, state)
  end

  def recvd_ch(type, event, state) do
    gen_handshake_1_3(:recvd_ch, type, event, state)
  end

  def wait_cert(:info, event, state) do
    gen_info_1_3(event, :wait_cert, state)
  end

  def wait_cert(type, event, state) do
    gen_handshake_1_3(:wait_cert, type, event, state)
  end

  def wait_cv(:info, event, state) do
    gen_info_1_3(event, :wait_cv, state)
  end

  def wait_cv(type, event, state) do
    gen_handshake_1_3(:wait_cv, type, event, state)
  end

  def wait_eoed(:info, event, state) do
    gen_info_1_3(event, :wait_eoed, state)
  end

  def wait_eoed(type, event, state) do
    gen_handshake_1_3(:wait_eoed, type, event, state)
  end

  def wait_finished(:info, event, state) do
    gen_info_1_3(event, :wait_finished, state)
  end

  def wait_finished(type, event, state) do
    gen_handshake_1_3(:wait_finished, type, event, state)
  end

  def wait_flight2(:info, event, state) do
    gen_info_1_3(event, :wait_flight2, state)
  end

  def wait_flight2(type, event, state) do
    gen_handshake_1_3(:wait_flight2, type, event, state)
  end

  def connected(:info, event, state) do
    gen_info_1_3(event, :connected, state)
  end

  def connected(type, event, state) do
    gen_handshake_1_3(:connected, type, event, state)
  end

  def wait_cert_cr(:info, event, state) do
    gen_info_1_3(event, :wait_cert_cr, state)
  end

  def wait_cert_cr(type, event, state) do
    gen_handshake_1_3(:wait_cert_cr, type, event, state)
  end

  def wait_ee(:info, event, state) do
    gen_info_1_3(event, :wait_ee, state)
  end

  def wait_ee(type, event, state) do
    gen_handshake_1_3(:wait_ee, type, event, state)
  end

  def wait_sh(:info, event, state) do
    gen_info_1_3(event, :wait_sh, state)
  end

  def wait_sh(type, event, state) do
    gen_handshake_1_3(:wait_sh, type, event, state)
  end

  def callback_mode() do
    :state_functions
  end

  def terminate(
        {:shutdown, {:sender_died, reason}},
        _StateName,
        r_state(
          static_env:
            r_static_env(
              socket: socket,
              transport_cb: transport
            )
        ) = state
      ) do
    :ssl_connection.handle_trusted_certs_db(state)
    close(reason, socket, transport, :undefined, :undefined)
  end

  def terminate(reason, stateName, state) do
    try do
      :ssl_connection.terminate(reason, stateName, state)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    ensure_sender_terminate(reason, state)
  end

  def format_status(type, data) do
    :ssl_connection.format_status(type, data)
  end

  def code_change(_OldVsn, stateName, state, _) do
    {:ok, stateName, state}
  end

  defp initial_state(
         role,
         sender,
         host,
         port,
         socket,
         {sSLOptions, socketOptions, trackers},
         user,
         {cbModule, dataTag, closeTag, errorTag, passiveTag}
       ) do
    %{
      beast_mitigation: beastMitigation,
      erl_dist: isErlDist,
      client_renegotiation: clientRenegotiation
    } = sSLOptions

    connectionStates =
      :tls_record.init_connection_states(
        role,
        beastMitigation
      )

    sessionCacheCb =
      case :application.get_env(
             :ssl,
             :session_cb
           ) do
        {:ok, cb} when is_atom(cb) ->
          cb

        _ ->
          :ssl_session_cache
      end

    internalActiveN =
      case :application.get_env(
             :ssl,
             :internal_active_n
           ) do
        {:ok, n} when is_integer(n) and not isErlDist ->
          n

        _ ->
          100
      end

    userMonitor = :erlang.monitor(:process, user)

    initStatEnv =
      r_static_env(
        role: role,
        transport_cb: cbModule,
        protocol_cb: :tls_connection,
        data_tag: dataTag,
        close_tag: closeTag,
        error_tag: errorTag,
        passive_tag: passiveTag,
        host: host,
        port: port,
        socket: socket,
        session_cache_cb: sessionCacheCb,
        trackers: trackers
      )

    r_state(
      static_env: initStatEnv,
      handshake_env:
        r_handshake_env(
          tls_handshake_history: :ssl_handshake.init_handshake_history(),
          renegotiation: {false, :first},
          allow_renegotiate: clientRenegotiation
        ),
      connection_env: r_connection_env(user_application: {userMonitor, user}),
      socket_options: socketOptions,
      ssl_options: sSLOptions,
      session: r_session(is_resumable: false),
      connection_states: connectionStates,
      protocol_buffers: r_protocol_buffers(),
      user_data_buffer: {[], 0, []},
      start_or_recv_from: :undefined,
      flight_buffer: [],
      protocol_specific: %{sender: sender, active_n: internalActiveN, active_n_toggle: true}
    )
  end

  defp initialize_tls_sender(
         r_state(
           static_env:
             r_static_env(role: role, transport_cb: transport, socket: socket, trackers: trackers),
           connection_env: r_connection_env(negotiated_version: version),
           socket_options: sockOpts,
           ssl_options: %{
             renegotiate_at: renegotiateAt,
             key_update_at: keyUpdateAt,
             log_level: logLevel
           },
           connection_states: %{current_write: connectionWriteState},
           protocol_specific: %{sender: sender}
         )
       ) do
    init = %{
      current_write: connectionWriteState,
      role: role,
      socket: socket,
      socket_options: sockOpts,
      trackers: trackers,
      transport_cb: transport,
      negotiated_version: version,
      renegotiate_at: renegotiateAt,
      key_update_at: keyUpdateAt,
      log_level: logLevel
    }

    :tls_sender.initialize(sender, init)
  end

  defp next_tls_record(
         data,
         stateName,
         r_state(
           protocol_buffers:
             r_protocol_buffers(
               tls_record_buffer: buf0,
               tls_cipher_texts: cT0
             ) = buffers,
           ssl_options: sslOpts
         ) = state0
       ) do
    versions =
      case stateName do
        state when state === :hello or state === :start ->
          for vsn <- [:"tlsv1.3", :"tlsv1.2", :"tlsv1.1", :tlsv1] do
            :tls_record.protocol_version(vsn)
          end

        _ ->
          r_connection_env(r_state(state0, :connection_env), :negotiated_version)
      end

    %{current_write: %{max_fragment_length: maxFragLen}} = r_state(state0, :connection_states)

    case :tls_record.get_tls_records(data, versions, buf0, maxFragLen, sslOpts) do
      {records, buf1} ->
        cT1 = cT0 ++ records

        next_record(
          stateName,
          r_state(state0,
            protocol_buffers:
              r_protocol_buffers(buffers,
                tls_record_buffer: buf1,
                tls_cipher_texts: cT1
              )
          )
        )

      r_alert() = alert ->
        handle_record_alert(alert, state0)
    end
  end

  defp handle_record_alert(alert, _) do
    alert
  end

  defp tls_handshake_events(packets) do
    :lists.map(
      fn packet ->
        {:next_event, :internal, {:handshake, packet}}
      end,
      packets
    )
  end

  defp handle_info(
         {protocol, _, data},
         stateName,
         r_state(
           static_env: r_static_env(data_tag: protocol),
           connection_env: r_connection_env(negotiated_version: version)
         ) = state0
       ) do
    case next_tls_record(data, stateName, state0) do
      {record, state} ->
        next_event(stateName, record, state)

      r_alert() = alert ->
        :ssl_connection.handle_own_alert(alert, version, stateName, state0)
    end
  end

  defp handle_info(
         {passiveTag, socket},
         stateName,
         r_state(
           static_env:
             r_static_env(
               socket: socket,
               passive_tag: passiveTag
             ),
           start_or_recv_from: from,
           protocol_buffers: r_protocol_buffers(tls_cipher_texts: cTs),
           protocol_specific: pS
         ) = state0
       ) do
    case from !== :undefined and cTs == [] do
      true ->
        {record, state} =
          activate_socket(r_state(state0, protocol_specific: Map.put(pS, :active_n_toggle, true)))

        next_event(stateName, record, state)

      false ->
        next_event(
          stateName,
          :no_record,
          r_state(state0, protocol_specific: Map.put(pS, :active_n_toggle, true))
        )
    end
  end

  defp handle_info(
         {closeTag, socket},
         stateName,
         r_state(
           static_env:
             r_static_env(role: role, host: host, port: port, socket: socket, close_tag: closeTag),
           handshake_env: r_handshake_env(renegotiation: type),
           connection_env: r_connection_env(negotiated_version: version),
           session: session
         ) = state
       )
       when stateName !== :connection do
    :ssl_connection.maybe_invalidate_session(version, type, role, host, port, session)

    alert =
      r_alert(
        level: 2,
        description: 0,
        where: %{
          mfa: {:tls_connection, :handle_info, 3},
          line: 1289,
          file: 'otp/lib/ssl/src/tls_connection.erl'
        },
        reason: :transport_closed
      )

    :ssl_connection.handle_normal_shutdown(r_alert(alert, role: role), stateName, state)
    {:stop, {:shutdown, :transport_closed}, state}
  end

  defp handle_info(
         {closeTag, socket},
         stateName,
         r_state(
           static_env: r_static_env(role: role, socket: socket, close_tag: closeTag),
           socket_options: r_socket_options(active: active),
           protocol_buffers: r_protocol_buffers(tls_cipher_texts: cTs),
           user_data_buffer: {_, bufferSize, _},
           protocol_specific: pS
         ) = state
       ) do
    case active == false and
           :erlang.or(
             cTs !== [],
             bufferSize !== 0
           ) do
      false ->
        alert =
          r_alert(
            level: 2,
            description: 0,
            where: %{
              mfa: {:tls_connection, :handle_info, 3},
              line: 1319,
              file: 'otp/lib/ssl/src/tls_connection.erl'
            },
            reason: :transport_closed
          )

        :ssl_connection.handle_normal_shutdown(r_alert(alert, role: role), stateName, state)
        {:stop, {:shutdown, :transport_closed}, state}

      true ->
        next_event(
          stateName,
          :no_record,
          r_state(state, protocol_specific: Map.put(pS, :active_n_toggle, true))
        )
    end
  end

  defp handle_info(
         {:EXIT, sender, reason},
         _,
         r_state(protocol_specific: %{sender: sender}) = state
       ) do
    {:stop, {:shutdown, {:sender_died, reason}}, state}
  end

  defp handle_info(msg, stateName, state) do
    apply(:ssl_connection, stateName, [:info, msg, state, :tls_connection])
  end

  defp handle_alerts([], result) do
    result
  end

  defp handle_alerts(_, {:stop, _, _} = stop) do
    stop
  end

  defp handle_alerts(
         [r_alert(level: 1, description: 0) | _Alerts],
         {:next_state, :connection = stateName,
          r_state(
            connection_env: cEnv,
            socket_options: r_socket_options(active: false),
            user_data_buffer: {_, bufferSize, _},
            protocol_buffers: r_protocol_buffers(tls_cipher_texts: cTs)
          ) = state}
       )
       when bufferSize !== 0 or cTs !== [] do
    {:next_state, stateName,
     r_state(state, connection_env: r_connection_env(cEnv, terminated: true))}
  end

  defp handle_alerts(
         [alert | alerts],
         {:next_state, stateName, state}
       ) do
    handle_alerts(
      alerts,
      :ssl_connection.handle_alert(alert, stateName, state)
    )
  end

  defp handle_alerts(
         [alert | alerts],
         {:next_state, stateName, state, _Actions}
       ) do
    handle_alerts(
      alerts,
      :ssl_connection.handle_alert(alert, stateName, state)
    )
  end

  def encode_handshake(handshake, version, connectionStates0, hist0) do
    frag =
      :tls_handshake.encode_handshake(
        handshake,
        version
      )

    hist =
      :ssl_handshake.update_handshake_history(
        hist0,
        frag
      )

    {encoded, connectionStates} =
      :tls_record.encode_handshake(
        frag,
        version,
        connectionStates0
      )

    {encoded, connectionStates, hist}
  end

  defp encode_change_cipher(r_change_cipher_spec(), version, connectionStates) do
    :tls_record.encode_change_cipher_spec(
      version,
      connectionStates
    )
  end

  defp decode_alerts(bin) do
    :ssl_alert.decode(bin)
  end

  defp gen_handshake(
         stateName,
         type,
         event,
         r_state(connection_env: r_connection_env(negotiated_version: version)) = state
       ) do
    try do
      apply(:ssl_connection, stateName, [type, event, state, :tls_connection])
    catch
      _, _ ->
        :ssl_connection.handle_own_alert(
          r_alert(
            level: 2,
            description: 40,
            where: %{
              mfa: {:tls_connection, :gen_handshake, 4},
              line: 1372,
              file: 'otp/lib/ssl/src/tls_connection.erl'
            },
            reason: :malformed_handshake_data
          ),
          version,
          stateName,
          state
        )
    else
      result ->
        result
    end
  end

  defp gen_handshake_1_3(
         stateName,
         type,
         event,
         r_state(connection_env: r_connection_env(negotiated_version: version)) = state
       ) do
    try do
      apply(:tls_connection_1_3, stateName, [type, event, state, :tls_connection])
    catch
      _, _ ->
        :ssl_connection.handle_own_alert(
          r_alert(
            level: 2,
            description: 40,
            where: %{
              mfa: {:tls_connection, :gen_handshake_1_3, 4},
              line: 1385,
              file: 'otp/lib/ssl/src/tls_connection.erl'
            },
            reason: :malformed_handshake_data
          ),
          version,
          stateName,
          state
        )
    else
      result ->
        result
    end
  end

  defp gen_info(
         event,
         :connection = stateName,
         r_state(connection_env: r_connection_env(negotiated_version: version)) = state
       ) do
    try do
      handle_info(event, stateName, state)
    catch
      _, _ ->
        :ssl_connection.handle_own_alert(
          r_alert(
            level: 2,
            description: 80,
            where: %{
              mfa: {:tls_connection, :gen_info, 3},
              line: 1397,
              file: 'otp/lib/ssl/src/tls_connection.erl'
            },
            reason: :malformed_data
          ),
          version,
          stateName,
          state
        )
    else
      result ->
        result
    end
  end

  defp gen_info(
         event,
         stateName,
         r_state(connection_env: r_connection_env(negotiated_version: version)) = state
       ) do
    try do
      handle_info(event, stateName, state)
    catch
      _, _ ->
        :ssl_connection.handle_own_alert(
          r_alert(
            level: 2,
            description: 40,
            where: %{
              mfa: {:tls_connection, :gen_info, 3},
              line: 1408,
              file: 'otp/lib/ssl/src/tls_connection.erl'
            },
            reason: :malformed_handshake_data
          ),
          version,
          stateName,
          state
        )
    else
      result ->
        result
    end
  end

  defp gen_info_1_3(
         event,
         :connected = stateName,
         r_state(connection_env: r_connection_env(negotiated_version: version)) = state
       ) do
    try do
      handle_info(event, stateName, state)
    catch
      _, _ ->
        :ssl_connection.handle_own_alert(
          r_alert(
            level: 2,
            description: 80,
            where: %{
              mfa: {:tls_connection, :gen_info_1_3, 3},
              line: 1419,
              file: 'otp/lib/ssl/src/tls_connection.erl'
            },
            reason: :malformed_data
          ),
          version,
          stateName,
          state
        )
    else
      result ->
        result
    end
  end

  defp gen_info_1_3(
         event,
         stateName,
         r_state(connection_env: r_connection_env(negotiated_version: version)) = state
       ) do
    try do
      handle_info(event, stateName, state)
    catch
      _, _ ->
        :ssl_connection.handle_own_alert(
          r_alert(
            level: 2,
            description: 40,
            where: %{
              mfa: {:tls_connection, :gen_info_1_3, 3},
              line: 1430,
              file: 'otp/lib/ssl/src/tls_connection.erl'
            },
            reason: :malformed_handshake_data
          ),
          version,
          stateName,
          state
        )
    else
      result ->
        result
    end
  end

  defp unprocessed_events(events) do
    :erlang.length(events) - 1
  end

  defp assert_buffer_sanity(
         <<_Type::size(8)-unsigned-big-integer, length::size(24)-unsigned-big-integer,
           rest::binary>>,
         %{max_handshake_size: max}
       )
       when length <= max do
    case :erlang.size(rest) do
      n when n < length ->
        true

      n when n > length ->
        throw(
          r_alert(
            level: 2,
            description: 40,
            where: %{
              mfa: {:tls_connection, :assert_buffer_sanity, 2},
              line: 1452,
              file: 'otp/lib/ssl/src/tls_connection.erl'
            },
            reason: :too_big_handshake_data
          )
        )

      _ ->
        throw(
          r_alert(
            level: 2,
            description: 40,
            where: %{
              mfa: {:tls_connection, :assert_buffer_sanity, 2},
              line: 1455,
              file: 'otp/lib/ssl/src/tls_connection.erl'
            },
            reason: :malformed_handshake_data
          )
        )
    end
  end

  defp assert_buffer_sanity(bin, _) do
    case :erlang.size(bin) do
      n when n < 3 ->
        true

      _ ->
        throw(
          r_alert(
            level: 2,
            description: 40,
            where: %{
              mfa: {:tls_connection, :assert_buffer_sanity, 2},
              line: 1463,
              file: 'otp/lib/ssl/src/tls_connection.erl'
            },
            reason: :malformed_handshake_data
          )
        )
    end
  end

  defp ensure_sender_terminate(:downgrade, _) do
    :ok
  end

  defp ensure_sender_terminate(_, r_state(protocol_specific: %{sender: sender})) do
    kill = fn ->
      receive do
      after
        5000 ->
          try do
            :erlang.exit(sender, :kill)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
      end
    end

    spawn(kill)
  end

  defp maybe_generate_client_shares(%{
         versions: [version | _],
         supported_groups: r_supported_groups(supported_groups: [group | _])
       })
       when version === {3, 4} do
    :ssl_cipher.generate_client_shares([group])
  end

  defp maybe_generate_client_shares(_) do
    :undefined
  end

  defp choose_tls_version(
         %{versions: versions},
         r_client_hello(
           extensions: %{client_hello_versions: r_client_hello_versions(versions: clientVersions)}
         )
       ) do
    case :ssl_handshake.select_supported_version(
           clientVersions,
           versions
         ) do
      {3, 4} ->
        :"tls_v1.3"

      _Else ->
        :"tls_v1.2"
    end
  end

  defp choose_tls_version(_, _) do
    :"tls_v1.2"
  end

  defp effective_version({3, 3}, %{versions: [version | _]}, :client)
       when version >= {3, 4} do
    version
  end

  defp effective_version(:undefined, %{versions: [version | _]}, _) do
    version
  end

  defp effective_version(version, _, _) do
    version
  end

  defp handle_new_session_ticket(
         _,
         r_state(ssl_options: %{session_tickets: :disabled})
       ) do
    :ok
  end

  defp handle_new_session_ticket(
         r_new_session_ticket(ticket_nonce: nonce) = newSessionTicket,
         r_state(
           connection_states: connectionStates,
           ssl_options: %{session_tickets: sessionTickets, server_name_indication: sNI},
           connection_env: r_connection_env(user_application: {_, user})
         )
       )
       when sessionTickets === :manual do
    %{security_parameters: secParams} =
      :ssl_record.current_connection_state(
        connectionStates,
        :read
      )

    hKDF = r_security_parameters(secParams, :prf_algorithm)
    rMS = r_security_parameters(secParams, :resumption_master_secret)
    pSK = :tls_v1.pre_shared_key(rMS, nonce, hKDF)
    send_ticket_data(user, newSessionTicket, hKDF, sNI, pSK)
  end

  defp handle_new_session_ticket(
         r_new_session_ticket(ticket_nonce: nonce) = newSessionTicket,
         r_state(
           connection_states: connectionStates,
           ssl_options: %{session_tickets: sessionTickets, server_name_indication: sNI}
         )
       )
       when sessionTickets === :auto do
    %{security_parameters: secParams} =
      :ssl_record.current_connection_state(
        connectionStates,
        :read
      )

    hKDF = r_security_parameters(secParams, :prf_algorithm)
    rMS = r_security_parameters(secParams, :resumption_master_secret)
    pSK = :tls_v1.pre_shared_key(rMS, nonce, hKDF)
    :tls_client_ticket_store.store_ticket(newSessionTicket, hKDF, sNI, pSK)
  end

  defp handle_key_update(
         r_key_update(request_update: :update_not_requested),
         state0
       ) do
    {:ok, update_cipher_key(:current_read, state0)}
  end

  defp handle_key_update(
         r_key_update(request_update: :update_requested),
         r_state(protocol_specific: %{sender: sender}) = state0
       ) do
    state1 = update_cipher_key(:current_read, state0)

    case send_key_update(sender, :update_not_requested) do
      :ok ->
        {:ok, state1}

      {:error, reason} ->
        {:error, state1,
         r_alert(
           level: 2,
           description: 80,
           where: %{
             mfa: {:tls_connection, :handle_key_update, 2},
             line: 1561,
             file: 'otp/lib/ssl/src/tls_connection.erl'
           },
           reason: reason
         )}
    end
  end

  def update_cipher_key(
        connStateName,
        r_state(connection_states: cS0) = state0
      ) do
    cS = update_cipher_key(connStateName, cS0)
    r_state(state0, connection_states: cS)
  end

  def update_cipher_key(connStateName, cS0) do
    %{security_parameters: secParams0, cipher_state: cipherState0} =
      connState0 =
      :maps.get(
        connStateName,
        cS0
      )

    hKDF = r_security_parameters(secParams0, :prf_algorithm)
    cipherSuite = r_security_parameters(secParams0, :cipher_suite)
    applicationTrafficSecret0 = r_security_parameters(secParams0, :application_traffic_secret)

    applicationTrafficSecret =
      :tls_v1.update_traffic_secret(
        hKDF,
        applicationTrafficSecret0
      )

    %{cipher: cipher} = :ssl_cipher_format.suite_bin_to_map(cipherSuite)
    {key, iV} = :tls_v1.calculate_traffic_keys(hKDF, cipher, applicationTrafficSecret)

    secParams =
      r_security_parameters(secParams0, application_traffic_secret: applicationTrafficSecret)

    cipherState = r_cipher_state(cipherState0, key: key, iv: iV)

    connState =
      Map.merge(connState0, %{
        security_parameters: secParams,
        cipher_state: cipherState,
        sequence_number: 0
      })

    Map.put(cS0, connStateName, connState)
  end

  def send_key_update(sender, type) do
    keyUpdate = :tls_handshake_1_3.key_update(type)
    :tls_sender.send_post_handshake(sender, keyUpdate)
  end

  defp send_ticket_data(user, newSessionTicket, hKDF, sNI, pSK) do
    timestamp = :erlang.system_time(:seconds)
    ticketData = %{hkdf: hKDF, sni: sNI, psk: pSK, timestamp: timestamp, ticket: newSessionTicket}
    send(user, {:ssl, :session_ticket, {sNI, :erlang.term_to_binary(ticketData)}})
  end
end
