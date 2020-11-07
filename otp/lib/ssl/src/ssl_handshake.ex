defmodule :m_ssl_handshake do
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

  Record.defrecord(:r_srp_user, :srp_user,
    generator: :undefined,
    prime: :undefined,
    salt: :undefined,
    verifier: :undefined
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

  def hello_request() do
    r_hello_request()
  end

  def server_hello(sessionId, version, connectionStates, extensions) do
    %{:security_parameters => secParams} =
      :ssl_record.pending_connection_state(
        connectionStates,
        :read
      )

    r_server_hello(
      server_version: version,
      cipher_suite: r_security_parameters(secParams, :cipher_suite),
      compression_method: r_security_parameters(secParams, :compression_algorithm),
      random: r_security_parameters(secParams, :server_random),
      session_id: sessionId,
      extensions: extensions
    )
  end

  def server_hello_done() do
    r_server_hello_done()
  end

  def certificate(ownCert, certDbHandle, certDbRef, :client) do
    chain =
      case :ssl_certificate.certificate_chain(
             ownCert,
             certDbHandle,
             certDbRef
           ) do
        {:ok, _, certChain} ->
          certChain

        {:error, _} ->
          []
      end

    r_certificate(asn1_certificates: chain)
  end

  def certificate(ownCert, certDbHandle, certDbRef, :server) do
    case :ssl_certificate.certificate_chain(ownCert, certDbHandle, certDbRef) do
      {:ok, _, chain} ->
        r_certificate(asn1_certificates: chain)

      {:error, error} ->
        r_alert(
          level: 2,
          description: 80,
          where: %{
            :mfa => {:ssl_handshake, :certificate, 4},
            :line => 150,
            :file => 'otp/lib/ssl/src/ssl_handshake.erl'
          },
          reason: {:server_has_no_suitable_certificates, error}
        )
    end
  end

  def client_certificate_verify(:undefined, _, _, _, _, _) do
    :ignore
  end

  def client_certificate_verify(_, _, _, _, :undefined, _) do
    :ignore
  end

  def client_certificate_verify(
        ownCert,
        masterSecret,
        version,
        {hashAlgo, signAlgo},
        privateKey,
        {handshake, _}
      ) do
    case :public_key.pkix_is_fixed_dh_cert(ownCert) do
      true ->
        r_alert(
          level: 2,
          description: 43,
          where: %{
            :mfa => {:ssl_handshake, :client_certificate_verify, 6},
            :line => 170,
            :file => 'otp/lib/ssl/src/ssl_handshake.erl'
          },
          reason: :fixed_diffie_hellman_prohibited
        )

      false ->
        hashes = calc_certificate_verify(version, hashAlgo, masterSecret, handshake)
        signed = digitally_signed(version, hashes, hashAlgo, privateKey, signAlgo)

        r_certificate_verify(
          signature: signed,
          hashsign_algorithm: {hashAlgo, signAlgo}
        )
    end
  end

  def certificate_request(cipherSuite, certDbHandle, certDbRef, hashSigns, version) do
    types =
      certificate_types(
        :ssl_cipher_format.suite_bin_to_map(cipherSuite),
        version
      )

    authorities =
      certificate_authorities(
        certDbHandle,
        certDbRef
      )

    r_certificate_request(
      certificate_types: types,
      hashsign_algorithms: hashSigns,
      certificate_authorities: authorities
    )
  end

  def key_exchange(:client, _Version, {:premaster_secret, secret, {_, publicKey, _}}) do
    encPremasterSecret =
      encrypted_premaster_secret(
        secret,
        publicKey
      )

    r_client_key_exchange(exchange_keys: encPremasterSecret)
  end

  def key_exchange(:client, _Version, {:dh, publicKey}) do
    r_client_key_exchange(exchange_keys: r_client_diffie_hellman_public(dh_public: publicKey))
  end

  def key_exchange(:client, _Version, {:ecdh, r_ECPrivateKey(publicKey: eCPublicKey)}) do
    r_client_key_exchange(
      exchange_keys: r_client_ec_diffie_hellman_public(dh_public: eCPublicKey)
    )
  end

  def key_exchange(:client, _Version, {:psk, identity}) do
    r_client_key_exchange(exchange_keys: r_client_psk_identity(identity: identity))
  end

  def key_exchange(:client, _Version, {:dhe_psk, identity, publicKey}) do
    r_client_key_exchange(
      exchange_keys:
        r_client_dhe_psk_identity(
          identity: identity,
          dh_public: publicKey
        )
    )
  end

  def key_exchange(
        :client,
        _Version,
        {:ecdhe_psk, identity, r_ECPrivateKey(publicKey: eCPublicKey)}
      ) do
    r_client_key_exchange(
      exchange_keys:
        r_client_ecdhe_psk_identity(
          identity: identity,
          dh_public: eCPublicKey
        )
    )
  end

  def key_exchange(
        :client,
        _Version,
        {:psk_premaster_secret, pskIdentity, secret, {_, publicKey, _}}
      ) do
    encPremasterSecret =
      encrypted_premaster_secret(
        secret,
        publicKey
      )

    r_client_key_exchange(
      exchange_keys:
        r_client_rsa_psk_identity(
          identity: pskIdentity,
          exchange_keys: encPremasterSecret
        )
    )
  end

  def key_exchange(:client, _Version, {:srp, publicKey}) do
    r_client_key_exchange(exchange_keys: r_client_srp_public(srp_a: publicKey))
  end

  def key_exchange(
        :server,
        version,
        {:dh, {publicKey, _}, r_DHParameter(prime: p, base: g), hashSign, clientRandom,
         serverRandom, privateKey}
      ) do
    serverDHParams = r_server_dh_params(dh_p: int_to_bin(p), dh_g: int_to_bin(g), dh_y: publicKey)

    enc_server_key_exchange(
      version,
      serverDHParams,
      hashSign,
      clientRandom,
      serverRandom,
      privateKey
    )
  end

  def key_exchange(
        :server,
        version,
        {:ecdh, r_ECPrivateKey(publicKey: eCPublicKey, parameters: eCCurve), hashSign,
         clientRandom, serverRandom, privateKey}
      ) do
    serverECParams = r_server_ecdh_params(curve: eCCurve, public: eCPublicKey)

    enc_server_key_exchange(
      version,
      serverECParams,
      hashSign,
      clientRandom,
      serverRandom,
      privateKey
    )
  end

  def key_exchange(
        :server,
        version,
        {:psk, pskIdentityHint, hashSign, clientRandom, serverRandom, privateKey}
      ) do
    serverPSKParams = r_server_psk_params(hint: pskIdentityHint)

    enc_server_key_exchange(
      version,
      serverPSKParams,
      hashSign,
      clientRandom,
      serverRandom,
      privateKey
    )
  end

  def key_exchange(
        :server,
        version,
        {:dhe_psk, pskIdentityHint, {publicKey, _}, r_DHParameter(prime: p, base: g), hashSign,
         clientRandom, serverRandom, privateKey}
      ) do
    serverEDHPSKParams =
      r_server_dhe_psk_params(
        hint: pskIdentityHint,
        dh_params:
          r_server_dh_params(
            dh_p: int_to_bin(p),
            dh_g: int_to_bin(g),
            dh_y: publicKey
          )
      )

    enc_server_key_exchange(
      version,
      serverEDHPSKParams,
      hashSign,
      clientRandom,
      serverRandom,
      privateKey
    )
  end

  def key_exchange(
        :server,
        version,
        {:ecdhe_psk, pskIdentityHint, r_ECPrivateKey(publicKey: eCPublicKey, parameters: eCCurve),
         hashSign, clientRandom, serverRandom, privateKey}
      ) do
    serverECDHEPSKParams =
      r_server_ecdhe_psk_params(
        hint: pskIdentityHint,
        dh_params:
          r_server_ecdh_params(
            curve: eCCurve,
            public: eCPublicKey
          )
      )

    enc_server_key_exchange(
      version,
      serverECDHEPSKParams,
      hashSign,
      clientRandom,
      serverRandom,
      privateKey
    )
  end

  def key_exchange(
        :server,
        version,
        {:srp, {publicKey, _}, r_srp_user(generator: generator, prime: prime, salt: salt),
         hashSign, clientRandom, serverRandom, privateKey}
      ) do
    serverSRPParams =
      r_server_srp_params(srp_n: prime, srp_g: generator, srp_s: salt, srp_b: publicKey)

    enc_server_key_exchange(
      version,
      serverSRPParams,
      hashSign,
      clientRandom,
      serverRandom,
      privateKey
    )
  end

  def finished(version, role, prfAlgo, masterSecret, {handshake, _}) do
    r_finished(verify_data: calc_finished(version, role, prfAlgo, masterSecret, handshake))
  end

  def next_protocol(selectedProtocol) do
    r_next_protocol(selected_protocol: selectedProtocol)
  end

  def certify(
        r_certificate(asn1_certificates: aSN1Certs),
        certDbHandle,
        certDbRef,
        %{
          :server_name_indication => serverNameIndication,
          :partial_chain => partialChain,
          :verify_fun => verifyFun,
          :customize_hostname_check => customizeHostnameCheck,
          :crl_check => crlCheck,
          :log_level => level,
          :signature_algs => signAlgos,
          :depth => depth
        } = opts,
        cRLDbHandle,
        role,
        host,
        version,
        %{
          :cert_ext => certExt,
          :ocsp_responder_certs => ocspResponderCerts,
          :ocsp_state => ocspState
        }
      ) do
    serverName = server_name(serverNameIndication, host, role)
    [peerCert | chainCerts] = aSN1Certs

    try do
      {trustedCert, certPath} =
        :ssl_certificate.trusted_cert_and_path(
          aSN1Certs,
          certDbHandle,
          certDbRef,
          partialChain
        )

      validationFunAndState =
        validation_fun_and_state(
          verifyFun,
          %{
            :role => role,
            :certdb => certDbHandle,
            :certdb_ref => certDbRef,
            :server_name => serverName,
            :customize_hostname_check => customizeHostnameCheck,
            :signature_algs => signAlgos,
            :signature_algs_cert => :undefined,
            :version => version,
            :crl_check => crlCheck,
            :crl_db => cRLDbHandle,
            :cert_ext => certExt,
            :issuer => trustedCert,
            :ocsp_responder_certs => ocspResponderCerts,
            :ocsp_state => ocspState
          },
          certPath,
          level
        )

      options = [{:max_path_length, depth}, {:verify_fun, validationFunAndState}]

      case :public_key.pkix_path_validation(trustedCert, certPath, options) do
        {:ok, {publicKeyInfo, _}} ->
          {peerCert, publicKeyInfo}

        {:error, reason} ->
          handle_path_validation_error(
            reason,
            peerCert,
            chainCerts,
            opts,
            options,
            certDbHandle,
            certDbRef
          )
      end
    catch
      :error, {_, {:error, {:asn1, asn1Reason}}} ->
        r_alert(
          level: 2,
          description: 46,
          where: %{
            :mfa => {:ssl_handshake, :certify, 9},
            :line => 391,
            :file => 'otp/lib/ssl/src/ssl_handshake.erl'
          },
          reason: {:failed_to_decode_certificate, asn1Reason}
        )

      :error, otherReason ->
        r_alert(
          level: 2,
          description: 80,
          where: %{
            :mfa => {:ssl_handshake, :certify, 9},
            :line => 393,
            :file => 'otp/lib/ssl/src/ssl_handshake.erl'
          },
          reason: {:unexpected_error, otherReason}
        )
    end
  end

  def certificate_verify(_, _, _, :undefined, _, _) do
    r_alert(
      level: 2,
      description: 40,
      where: %{
        :mfa => {:ssl_handshake, :certificate_verify, 6},
        :line => 402,
        :file => 'otp/lib/ssl/src/ssl_handshake.erl'
      },
      reason: :invalid_certificate_verify_message
    )
  end

  def certificate_verify(
        signature,
        publicKeyInfo,
        version,
        hashSign = {hashAlgo, _},
        masterSecret,
        {_, handshake}
      ) do
    hash = calc_certificate_verify(version, hashAlgo, masterSecret, handshake)

    case verify_signature(version, hash, hashSign, signature, publicKeyInfo) do
      true ->
        :valid

      _ ->
        r_alert(
          level: 2,
          description: 42,
          where: %{
            :mfa => {:ssl_handshake, :certificate_verify, 6},
            :line => 411,
            :file => 'otp/lib/ssl/src/ssl_handshake.erl'
          }
        )
    end
  end

  def verify_signature({3, 4}, hash, {hashAlgo, signAlgo}, signature, {_, pubKey, pubKeyParams})
      when signAlgo == :rsa_pss_rsae or
             signAlgo == :rsa_pss_pss do
    options = verify_options(signAlgo, hashAlgo, pubKeyParams)
    :public_key.verify(hash, hashAlgo, signature, pubKey, options)
  end

  def verify_signature({3, 3}, hash, {hashAlgo, signAlgo}, signature, {_, pubKey, pubKeyParams})
      when signAlgo == :rsa_pss_rsae or
             signAlgo == :rsa_pss_pss do
    options = verify_options(signAlgo, hashAlgo, pubKeyParams)
    :public_key.verify({:digest, hash}, hashAlgo, signature, pubKey, options)
  end

  def verify_signature(
        {3, minor},
        hash,
        {hashAlgo, signAlgo},
        signature,
        {{1, 2, 840, 113_549, 1, 1, 1}, pubKey, pubKeyParams}
      )
      when minor >= 3 do
    options = verify_options(signAlgo, hashAlgo, pubKeyParams)
    :public_key.verify({:digest, hash}, hashAlgo, signature, pubKey, options)
  end

  def verify_signature(
        {3, minor},
        hash,
        _HashAlgo,
        signature,
        {{1, 2, 840, 113_549, 1, 1, 1}, pubKey, _PubKeyParams}
      )
      when minor <= 2 do
    case :public_key.decrypt_public(signature, pubKey, [{:rsa_pad, :rsa_pkcs1_padding}]) do
      ^hash ->
        true

      _ ->
        false
    end
  end

  def verify_signature(
        {3, 4},
        hash,
        {hashAlgo, _SignAlgo},
        signature,
        {{1, 2, 840, 10045, 2, 1}, pubKey, pubKeyParams}
      ) do
    :public_key.verify(hash, hashAlgo, signature, {pubKey, pubKeyParams})
  end

  def verify_signature(
        _,
        hash,
        {hashAlgo, _SignAlg},
        signature,
        {{1, 2, 840, 10045, 2, 1}, publicKey, publicKeyParams}
      ) do
    :public_key.verify({:digest, hash}, hashAlgo, signature, {publicKey, publicKeyParams})
  end

  def verify_signature({3, minor}, _Hash, {_HashAlgo, :anon}, _Signature, _)
      when minor <= 3 do
    true
  end

  def verify_signature(
        {3, minor},
        hash,
        {hashAlgo, :dsa},
        signature,
        {{1, 2, 840, 10040, 4, 1}, publicKey, publicKeyParams}
      )
      when minor <= 3 do
    :public_key.verify({:digest, hash}, hashAlgo, signature, {publicKey, publicKeyParams})
  end

  def master_secret(version, r_session(master_secret: mastersecret), connectionStates, role) do
    %{:security_parameters => secParams} =
      :ssl_record.pending_connection_state(
        connectionStates,
        :read
      )

    try do
      master_secret(version, mastersecret, secParams, connectionStates, role)
    catch
      :exit, _ ->
        r_alert(
          level: 2,
          description: 40,
          where: %{
            :mfa => {:ssl_handshake, :master_secret, 4},
            :line => 465,
            :file => 'otp/lib/ssl/src/ssl_handshake.erl'
          },
          reason: :key_calculation_failure
        )
    end
  end

  def master_secret(version, premasterSecret, connectionStates, role) do
    %{:security_parameters => secParams} =
      :ssl_record.pending_connection_state(
        connectionStates,
        :read
      )

    r_security_parameters(
      prf_algorithm: prfAlgo,
      client_random: clientRandom,
      server_random: serverRandom
    ) = secParams

    try do
      master_secret(
        version,
        calc_master_secret(version, prfAlgo, premasterSecret, clientRandom, serverRandom),
        secParams,
        connectionStates,
        role
      )
    catch
      :exit, _ ->
        r_alert(
          level: 2,
          description: 40,
          where: %{
            :mfa => {:ssl_handshake, :master_secret, 4},
            :line => 481,
            :file => 'otp/lib/ssl/src/ssl_handshake.erl'
          },
          reason: :master_secret_calculation_failure
        )
    end
  end

  def server_key_exchange_hash(:md5sha, value) do
    mD5 = :crypto.hash(:md5, value)
    sHA = :crypto.hash(:sha, value)
    <<mD5::binary, sHA::binary>>
  end

  def server_key_exchange_hash(hash, value) do
    :crypto.hash(hash, value)
  end

  def verify_connection(
        version,
        r_finished(verify_data: data),
        role,
        prfAlgo,
        masterSecret,
        {_, handshake}
      ) do
    case calc_finished(version, role, prfAlgo, masterSecret, handshake) do
      ^data ->
        :verified

      _ ->
        r_alert(
          level: 2,
          description: 51,
          where: %{
            :mfa => {:ssl_handshake, :verify_connection, 6},
            :line => 511,
            :file => 'otp/lib/ssl/src/ssl_handshake.erl'
          }
        )
    end
  end

  def init_handshake_history() do
    {[], []}
  end

  def update_handshake_history({handshake0, _Prev}, data) do
    {[data | handshake0], handshake0}
  end

  def verify_server_key(
        r_server_key_params(params_bin: encParams, signature: signature),
        hashSign = {hashAlgo, _},
        connectionStates,
        version,
        pubKeyInfo
      ) do
    %{:security_parameters => secParams} =
      :ssl_record.pending_connection_state(
        connectionStates,
        :read
      )

    r_security_parameters(
      client_random: clientRandom,
      server_random: serverRandom
    ) = secParams

    hash =
      server_key_exchange_hash(
        hashAlgo,
        <<clientRandom::binary, serverRandom::binary, encParams::binary>>
      )

    verify_signature(version, hash, hashSign, signature, pubKeyInfo)
  end

  def select_version(recordCB, clientVersion, versions) do
    do_select_version(recordCB, clientVersion, versions)
  end

  def select_supported_version([], _ServerVersions) do
    :undefined
  end

  def select_supported_version([clientVersion | t], serverVersions) do
    case :lists.member(clientVersion, serverVersions) do
      true ->
        clientVersion

      false ->
        select_supported_version(t, serverVersions)
    end
  end

  def encode_handshake(
        r_next_protocol(selected_protocol: selectedProtocol),
        _Version
      ) do
    paddingLength = 32 - rem(byte_size(selectedProtocol) + 2, 32)

    {67,
     <<byte_size(selectedProtocol)::size(8)-unsigned-big-integer, selectedProtocol::binary,
       paddingLength::size(8)-unsigned-big-integer, 0::size(paddingLength * 8)>>}
  end

  def encode_handshake(
        r_server_hello(
          server_version: {major, minor} = version,
          random: random,
          session_id: session_ID,
          cipher_suite: cipherSuite,
          compression_method: comp_method,
          extensions: extensions
        ),
        _Version
      ) do
    sID_length = byte_size(session_ID)

    extensionsBin =
      encode_hello_extensions(
        extensions,
        version
      )

    {2,
     <<major::size(8)-unsigned-big-integer, minor::size(8)-unsigned-big-integer,
       random::size(32)-binary, sID_length::size(8)-unsigned-big-integer, session_ID::binary,
       cipherSuite::binary, comp_method::size(8)-unsigned-big-integer, extensionsBin::binary>>}
  end

  def encode_handshake(r_certificate(asn1_certificates: aSN1CertList), _Version) do
    aSN1Certs = certs_from_list(aSN1CertList)
    aCLen = :erlang.iolist_size(aSN1Certs)
    {11, <<aCLen::size(24)-unsigned-big-integer, aSN1Certs::size(aCLen)-binary>>}
  end

  def encode_handshake(r_server_key_exchange(exchange_keys: keys), _Version) do
    {12, keys}
  end

  def encode_handshake(
        r_server_key_params(params_bin: keys, hashsign: hashSign, signature: signature),
        version
      ) do
    encSign = enc_sign(hashSign, signature, version)
    {12, <<keys::binary, encSign::binary>>}
  end

  def encode_handshake(
        r_certificate_request(
          certificate_types: certTypes,
          hashsign_algorithms: r_hash_sign_algos(hash_sign_algos: hashSignAlgos),
          certificate_authorities: certAuths
        ),
        {major, minor}
      )
      when major == 3 and minor >= 3 do
    hashSigns =
      for {hash, sign} <- hashSignAlgos, into: <<>> do
        <<:ssl_cipher.hash_algorithm(hash)::size(8), :ssl_cipher.sign_algorithm(sign)::size(8)>>
      end

    certTypesLen = byte_size(certTypes)
    hashSignsLen = byte_size(hashSigns)
    certAuthsLen = byte_size(certAuths)

    {13,
     <<certTypesLen::size(8)-unsigned-big-integer, certTypes::binary,
       hashSignsLen::size(16)-unsigned-big-integer, hashSigns::binary,
       certAuthsLen::size(16)-unsigned-big-integer, certAuths::binary>>}
  end

  def encode_handshake(
        r_certificate_request(
          certificate_types: certTypes,
          certificate_authorities: certAuths
        ),
        _Version
      ) do
    certTypesLen = byte_size(certTypes)
    certAuthsLen = byte_size(certAuths)

    {13,
     <<certTypesLen::size(8)-unsigned-big-integer, certTypes::binary,
       certAuthsLen::size(16)-unsigned-big-integer, certAuths::binary>>}
  end

  def encode_handshake(r_server_hello_done(), _Version) do
    {14, <<>>}
  end

  def encode_handshake(r_client_key_exchange(exchange_keys: exchangeKeys), version) do
    {16, encode_client_key(exchangeKeys, version)}
  end

  def encode_handshake(
        r_certificate_verify(
          signature: binSig,
          hashsign_algorithm: hashSign
        ),
        version
      ) do
    encSig = enc_sign(hashSign, binSig, version)
    {15, encSig}
  end

  def encode_handshake(r_finished(verify_data: verifyData), _Version) do
    {20, verifyData}
  end

  def encode_hello_extensions(_, {3, 0}) do
    <<>>
  end

  def encode_hello_extensions(extensions, _) do
    encode_extensions(
      hello_extensions_list(extensions),
      <<>>
    )
  end

  def encode_extensions(exts) do
    encode_extensions(exts, <<>>)
  end

  def encode_extensions([], <<>>) do
    <<0::size(16)-unsigned-big-integer>>
  end

  def encode_extensions([], acc) do
    size = byte_size(acc)
    <<size::size(16)-unsigned-big-integer, acc::binary>>
  end

  def encode_extensions(
        [r_alpn(extension_data: extensionData) | rest],
        acc
      ) do
    len = byte_size(extensionData)
    extLen = len + 2

    encode_extensions(
      rest,
      <<16::size(16)-unsigned-big-integer, extLen::size(16)-unsigned-big-integer,
        len::size(16)-unsigned-big-integer, extensionData::binary, acc::binary>>
    )
  end

  def encode_extensions(
        [r_next_protocol_negotiation(extension_data: extensionData) | rest],
        acc
      ) do
    len = byte_size(extensionData)

    encode_extensions(
      rest,
      <<13172::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
        extensionData::binary, acc::binary>>
    )
  end

  def encode_extensions(
        [r_renegotiation_info(renegotiated_connection: :undefined) | rest],
        acc
      ) do
    encode_extensions(rest, acc)
  end

  def encode_extensions(
        [
          r_renegotiation_info(
            renegotiated_connection: <<0::size(8)-unsigned-big-integer>> = info
          )
          | rest
        ],
        acc
      ) do
    len = byte_size(info)

    encode_extensions(
      rest,
      <<65281::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer, info::binary,
        acc::binary>>
    )
  end

  def encode_extensions(
        [r_renegotiation_info(renegotiated_connection: info) | rest],
        acc
      ) do
    infoLen = byte_size(info)
    len = infoLen + 1

    encode_extensions(
      rest,
      <<65281::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
        infoLen::size(8)-unsigned-big-integer, info::binary, acc::binary>>
    )
  end

  def encode_extensions(
        [r_elliptic_curves(elliptic_curve_list: ellipticCurves) | rest],
        acc
      ) do
    ellipticCurveList =
      for x <- ellipticCurves, into: <<>> do
        <<:tls_v1.oid_to_enum(x)::size(16)>>
      end

    listLen = byte_size(ellipticCurveList)
    len = listLen + 2

    encode_extensions(
      rest,
      <<10::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
        listLen::size(16)-unsigned-big-integer, ellipticCurveList::binary, acc::binary>>
    )
  end

  def encode_extensions(
        [r_supported_groups(supported_groups: supportedGroups) | rest],
        acc
      ) do
    supportedGroupList =
      for x <- supportedGroups, into: <<>> do
        <<:tls_v1.group_to_enum(x)::size(16)>>
      end

    listLen = byte_size(supportedGroupList)
    len = listLen + 2

    encode_extensions(
      rest,
      <<10::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
        listLen::size(16)-unsigned-big-integer, supportedGroupList::binary, acc::binary>>
    )
  end

  def encode_extensions(
        [r_ec_point_formats(ec_point_format_list: eCPointFormats) | rest],
        acc
      ) do
    eCPointFormatList = :erlang.list_to_binary(eCPointFormats)
    listLen = byte_size(eCPointFormatList)
    len = listLen + 1

    encode_extensions(
      rest,
      <<11::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
        listLen::size(8)-unsigned-big-integer, eCPointFormatList::binary, acc::binary>>
    )
  end

  def encode_extensions([r_srp(username: userName) | rest], acc) do
    sRPLen = byte_size(userName)
    len = sRPLen + 1

    encode_extensions(
      rest,
      <<12::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
        sRPLen::size(8)-unsigned-big-integer, userName::binary, acc::binary>>
    )
  end

  def encode_extensions(
        [r_hash_sign_algos(hash_sign_algos: hashSignAlgos) | rest],
        acc
      ) do
    signAlgoList =
      for {hash, sign} <- hashSignAlgos, into: <<>> do
        <<:ssl_cipher.hash_algorithm(hash)::size(8), :ssl_cipher.sign_algorithm(sign)::size(8)>>
      end

    listLen = byte_size(signAlgoList)
    len = listLen + 2

    encode_extensions(
      rest,
      <<13::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
        listLen::size(16)-unsigned-big-integer, signAlgoList::binary, acc::binary>>
    )
  end

  def encode_extensions(
        [
          r_signature_algorithms(signature_scheme_list: signatureSchemes)
          | rest
        ],
        acc
      ) do
    signSchemeList =
      for signatureScheme <- signatureSchemes, into: <<>> do
        <<:ssl_cipher.signature_scheme(signatureScheme)::size(16)>>
      end

    listLen = byte_size(signSchemeList)
    len = listLen + 2

    encode_extensions(
      rest,
      <<13::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
        listLen::size(16)-unsigned-big-integer, signSchemeList::binary, acc::binary>>
    )
  end

  def encode_extensions(
        [
          r_signature_algorithms_cert(signature_scheme_list: signatureSchemes)
          | rest
        ],
        acc
      ) do
    signSchemeList =
      for signatureScheme <- signatureSchemes, into: <<>> do
        <<:ssl_cipher.signature_scheme(signatureScheme)::size(16)>>
      end

    listLen = byte_size(signSchemeList)
    len = listLen + 2

    encode_extensions(
      rest,
      <<50::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
        listLen::size(16)-unsigned-big-integer, signSchemeList::binary, acc::binary>>
    )
  end

  def encode_extensions([r_sni(hostname: '') | rest], acc) do
    hostnameBin = <<>>

    encode_extensions(
      rest,
      <<0::size(16)-unsigned-big-integer, 0::size(16)-unsigned-big-integer, hostnameBin::binary,
        acc::binary>>
    )
  end

  def encode_extensions([r_sni(hostname: hostname) | rest], acc) do
    hostLen = length(hostname)
    hostnameBin = :erlang.list_to_binary(hostname)
    serverNameLength = 1 + 2 + hostLen
    extLength = 2 + serverNameLength

    encode_extensions(
      rest,
      <<0::size(16)-unsigned-big-integer, extLength::size(16)-unsigned-big-integer,
        serverNameLength::size(16)-unsigned-big-integer, 0::size(8)-unsigned-big-integer,
        hostLen::size(16)-unsigned-big-integer, hostnameBin::binary, acc::binary>>
    )
  end

  def encode_extensions([r_max_frag_enum(enum: maxFragEnum) | rest], acc) do
    extLength = 1

    encode_extensions(
      rest,
      <<1::size(16)-unsigned-big-integer, extLength::size(16)-unsigned-big-integer,
        maxFragEnum::size(8)-unsigned-big-integer, acc::binary>>
    )
  end

  def encode_extensions([r_client_hello_versions(versions: versions0) | rest], acc) do
    versions = encode_versions(versions0)
    verLen = byte_size(versions)
    len = verLen + 1

    encode_extensions(
      rest,
      <<43::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
        verLen::size(8)-unsigned-big-integer, versions::binary, acc::binary>>
    )
  end

  def encode_extensions([r_server_hello_selected_version(selected_version: version0) | rest], acc) do
    version = encode_versions([version0])
    len = byte_size(version)

    encode_extensions(
      rest,
      <<43::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer, version::binary,
        acc::binary>>
    )
  end

  def encode_extensions([r_key_share_client_hello(client_shares: clientShares0) | rest], acc) do
    clientShares = encode_client_shares(clientShares0)
    clientSharesLen = byte_size(clientShares)
    len = clientSharesLen + 2

    encode_extensions(
      rest,
      <<51::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
        clientSharesLen::size(16)-unsigned-big-integer, clientShares::binary, acc::binary>>
    )
  end

  def encode_extensions([r_key_share_server_hello(server_share: serverShare0) | rest], acc) do
    serverShare = encode_key_share_entry(serverShare0)
    len = byte_size(serverShare)

    encode_extensions(
      rest,
      <<51::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
        serverShare::binary, acc::binary>>
    )
  end

  def encode_extensions([r_key_share_hello_retry_request(selected_group: group0) | rest], acc) do
    group = :tls_v1.group_to_enum(group0)

    encode_extensions(
      rest,
      <<51::size(16)-unsigned-big-integer, 2::size(16)-unsigned-big-integer,
        group::size(16)-unsigned-big-integer, acc::binary>>
    )
  end

  def encode_extensions([r_psk_key_exchange_modes(ke_modes: kEModes0) | rest], acc) do
    kEModes = encode_psk_key_exchange_modes(kEModes0)
    kEModesLen = byte_size(kEModes)
    extLen = kEModesLen + 1

    encode_extensions(
      rest,
      <<45::size(16)-unsigned-big-integer, extLen::size(16)-unsigned-big-integer,
        kEModesLen::size(8)-unsigned-big-integer, kEModes::binary, acc::binary>>
    )
  end

  def encode_extensions(
        [
          r_certificate_status_request(
            status_type: statusRequest,
            request: request
          )
          | rest
        ],
        acc
      ) do
    certStatusReq =
      encode_cert_status_req(
        statusRequest,
        request
      )

    len = byte_size(certStatusReq)

    encode_extensions(
      rest,
      <<5::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
        certStatusReq::binary, acc::binary>>
    )
  end

  def encode_extensions(
        [
          r_pre_shared_key_client_hello(
            offered_psks:
              r_offered_psks(
                identities: identities0,
                binders: binders0
              )
          )
          | rest
        ],
        acc
      ) do
    identities = encode_psk_identities(identities0)
    binders = encode_psk_binders(binders0)
    len = byte_size(identities) + byte_size(binders)

    encode_extensions(
      rest,
      <<acc::binary, 41::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
        identities::binary, binders::binary>>
    )
  end

  def encode_extensions([r_pre_shared_key_server_hello(selected_identity: identity) | rest], acc) do
    encode_extensions(
      rest,
      <<41::size(16)-unsigned-big-integer, 2::size(16)-unsigned-big-integer,
        identity::size(16)-unsigned-big-integer, acc::binary>>
    )
  end

  def encode_extensions([r_cookie(cookie: cookie) | rest], acc) do
    cookieLen = byte_size(cookie)
    len = cookieLen + 2

    encode_extensions(
      rest,
      <<44::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
        cookieLen::size(16)-unsigned-big-integer, cookie::binary, acc::binary>>
    )
  end

  defp encode_cert_status_req(
         statusType,
         r_ocsp_status_request(
           responder_id_list: responderIDList,
           request_extensions: reqExtns
         )
       ) do
    responderIDListBin = encode_responderID_list(responderIDList)
    reqExtnsBin = encode_request_extensions(reqExtns)
    <<statusType::size(8)-unsigned-big-integer, responderIDListBin::binary, reqExtnsBin::binary>>
  end

  defp encode_responderID_list([]) do
    <<0::size(16)-unsigned-big-integer>>
  end

  defp encode_responderID_list(list) do
    do_encode_responderID_list(list, <<>>)
  end

  defp do_encode_responderID_list([], acc) do
    len = byte_size(acc)
    <<len::size(16)-unsigned-big-integer, acc::binary>>
  end

  defp do_encode_responderID_list([responder | rest], acc)
       when is_binary(responder) do
    len = byte_size(responder)

    do_encode_responderID_list(
      rest,
      <<acc::binary, len::size(16)-unsigned-big-integer, responder::binary>>
    )
  end

  defp encode_request_extensions([]) do
    <<0::size(16)-unsigned-big-integer>>
  end

  defp encode_request_extensions(extns) when is_list(extns) do
    extnBin = :public_key.der_encode(:Extensions, extns)
    len = byte_size(extnBin)
    <<len::size(16)-unsigned-big-integer, extnBin::binary>>
  end

  def encode_client_protocol_negotiation(:undefined, _) do
    :undefined
  end

  def encode_client_protocol_negotiation(_, false) do
    r_next_protocol_negotiation(extension_data: <<>>)
  end

  def encode_client_protocol_negotiation(_, _) do
    :undefined
  end

  def encode_protocols_advertised_on_server(:undefined) do
    :undefined
  end

  def encode_protocols_advertised_on_server(protocols) do
    r_next_protocol_negotiation(extension_data: :lists.foldl(&encode_protocol/2, <<>>, protocols))
  end

  def decode_handshake(_, 0, <<>>) do
    r_hello_request()
  end

  def decode_handshake(
        _,
        67,
        <<selectedProtocolLength::size(8)-unsigned-big-integer,
          selectedProtocol::size(selectedProtocolLength)-binary,
          paddingLength::size(8)-unsigned-big-integer, _Padding::size(paddingLength)-binary>>
      ) do
    r_next_protocol(selected_protocol: selectedProtocol)
  end

  def decode_handshake(
        version,
        2,
        <<major::size(8)-unsigned-big-integer, minor::size(8)-unsigned-big-integer,
          random::size(32)-binary, sID_length::size(8)-unsigned-big-integer,
          session_ID::size(sID_length)-binary, cipher_suite::size(2)-binary,
          comp_method::size(8)-unsigned-big-integer>>
      ) do
    r_server_hello(
      server_version: {major, minor},
      random: random,
      session_id: session_ID,
      cipher_suite: cipher_suite,
      compression_method: comp_method,
      extensions: empty_extensions(version, :server_hello)
    )
  end

  def decode_handshake(
        version,
        2,
        <<major::size(8)-unsigned-big-integer, minor::size(8)-unsigned-big-integer,
          random::size(32)-binary, sID_length::size(8)-unsigned-big-integer,
          session_ID::size(sID_length)-binary, cipher_suite::size(2)-binary,
          comp_method::size(8)-unsigned-big-integer, extLen::size(16)-unsigned-big-integer,
          extensions::size(extLen)-binary>>
      ) do
    helloExtensions = decode_hello_extensions(extensions, version, {major, minor}, :server_hello)

    r_server_hello(
      server_version: {major, minor},
      random: random,
      session_id: session_ID,
      cipher_suite: cipher_suite,
      compression_method: comp_method,
      extensions: helloExtensions
    )
  end

  def decode_handshake(
        _Version,
        11,
        <<aCLen::size(24)-unsigned-big-integer, aSN1Certs::size(aCLen)-binary>>
      ) do
    r_certificate(asn1_certificates: certs_to_list(aSN1Certs))
  end

  def decode_handshake(
        _Version,
        22,
        <<1::size(8)-unsigned-big-integer, len::size(24)-unsigned-big-integer,
          aSN1OcspResponse::size(len)-binary>>
      ) do
    r_certificate_status(status_type: 1, response: aSN1OcspResponse)
  end

  def decode_handshake(_Version, 12, keys) do
    r_server_key_exchange(exchange_keys: keys)
  end

  def decode_handshake(
        {major, minor},
        13,
        <<certTypesLen::size(8)-unsigned-big-integer, certTypes::size(certTypesLen)-binary,
          hashSignsLen::size(16)-unsigned-big-integer, hashSigns::size(hashSignsLen)-binary,
          certAuthsLen::size(16)-unsigned-big-integer, certAuths::size(certAuthsLen)-binary>>
      )
      when major >= 3 and minor >= 3 do
    hashSignAlgos =
      for <<(<<hash::size(8)-unsigned-big-integer, sign::size(8)-unsigned-big-integer>> <-
               hashSigns)>> do
        {:ssl_cipher.hash_algorithm(hash), :ssl_cipher.sign_algorithm(sign)}
      end

    r_certificate_request(
      certificate_types: certTypes,
      hashsign_algorithms: r_hash_sign_algos(hash_sign_algos: hashSignAlgos),
      certificate_authorities: certAuths
    )
  end

  def decode_handshake(
        _Version,
        13,
        <<certTypesLen::size(8)-unsigned-big-integer, certTypes::size(certTypesLen)-binary,
          certAuthsLen::size(16)-unsigned-big-integer, certAuths::size(certAuthsLen)-binary>>
      ) do
    r_certificate_request(
      certificate_types: certTypes,
      certificate_authorities: certAuths
    )
  end

  def decode_handshake(_Version, 14, <<>>) do
    r_server_hello_done()
  end

  def decode_handshake(
        {major, minor},
        15,
        <<hashSign::size(2)-binary, signLen::size(16)-unsigned-big-integer,
          signature::size(signLen)-binary>>
      )
      when major == 3 and minor >= 3 do
    r_certificate_verify(
      hashsign_algorithm: dec_hashsign(hashSign),
      signature: signature
    )
  end

  def decode_handshake(
        _Version,
        15,
        <<signLen::size(16)-unsigned-big-integer, signature::size(signLen)-binary>>
      ) do
    r_certificate_verify(signature: signature)
  end

  def decode_handshake(_Version, 16, pKEPMS) do
    r_client_key_exchange(exchange_keys: pKEPMS)
  end

  def decode_handshake(_Version, 20, verifyData) do
    r_finished(verify_data: verifyData)
  end

  def decode_handshake(_, message, _) do
    throw(
      r_alert(
        level: 2,
        description: 40,
        where: %{
          :mfa => {:ssl_handshake, :decode_handshake, 3},
          :line => 916,
          :file => 'otp/lib/ssl/src/ssl_handshake.erl'
        },
        reason: {:unknown_or_malformed_handshake, message}
      )
    )
  end

  def decode_vector(<<>>) do
    <<>>
  end

  def decode_vector(<<len::size(16)-unsigned-big-integer, vector::size(len)-binary>>) do
    vector
  end

  def decode_hello_extensions(extensions, localVersion, legacyVersion, messageType0) do
    messageType =
      case messageType0 do
        :client ->
          :client_hello

        :server ->
          :server_hello

        t ->
          t
      end

    version =
      process_supported_versions_extension(
        extensions,
        localVersion,
        legacyVersion
      )

    decode_extensions(extensions, version, messageType, empty_extensions(version, messageType))
  end

  def decode_extensions(extensions, version, messageType) do
    decode_extensions(extensions, version, messageType, empty_extensions())
  end

  def decode_server_key(serverKey, type, version) do
    dec_server_key(serverKey, key_exchange_alg(type), version)
  end

  def decode_client_key(clientKey, type, version) do
    dec_client_key(clientKey, key_exchange_alg(type), version)
  end

  def decode_suites(:"2_bytes", dec) do
    from_2bytes(dec)
  end

  def decode_suites(:"3_bytes", dec) do
    from_3bytes(dec)
  end

  def available_suites(userSuites, version) do
    versionSuites = :ssl_cipher.all_suites(version) ++ :ssl_cipher.anonymous_suites(version)

    :lists.filtermap(
      fn suite ->
        :lists.member(suite, versionSuites)
      end,
      userSuites
    )
  end

  defp available_suites(serverCert, userSuites, version, :undefined, curve) do
    suites =
      :ssl_cipher.filter(
        serverCert,
        available_suites(userSuites, version),
        version
      )

    filter_unavailable_ecc_suites(curve, suites)
  end

  defp available_suites(serverCert, userSuites, version, hashSigns, curve) do
    suites = available_suites(serverCert, userSuites, version, :undefined, curve)

    filter_hashsigns(
      suites,
      for suite <- suites do
        :ssl_cipher_format.suite_bin_to_map(suite)
      end,
      hashSigns,
      version,
      []
    )
  end

  def available_signature_algs(:undefined, _) do
    :undefined
  end

  def available_signature_algs(supportedHashSigns, version) when version >= {3, 3} do
    r_hash_sign_algos(hash_sign_algos: supportedHashSigns)
  end

  def available_signature_algs(_, _) do
    :undefined
  end

  def available_signature_algs(:undefined, supportedHashSigns, _, version)
      when version >= {3, 3} do
    supportedHashSigns
  end

  def available_signature_algs(
        r_hash_sign_algos(hash_sign_algos: clientHashSigns),
        supportedHashSigns,
        _,
        version
      )
      when version >= {3, 3} do
    :sets.to_list(
      :sets.intersection(
        :sets.from_list(clientHashSigns),
        :sets.from_list(supportedHashSigns)
      )
    )
  end

  def available_signature_algs(_, _, _, _) do
    :undefined
  end

  def cipher_suites(suites, renegotiation, true) do
    cipher_suites(suites, renegotiation) ++
      [<<86::size(8)-unsigned-big-integer, 0::size(8)-unsigned-big-integer>>]
  end

  def cipher_suites(suites, renegotiation, false) do
    cipher_suites(suites, renegotiation)
  end

  defp cipher_suites(suites, false) do
    [
      <<0::size(8)-unsigned-big-integer, 255::size(8)-unsigned-big-integer>>
      | suites
    ]
  end

  defp cipher_suites(suites, true) do
    suites
  end

  def prf({3, _N}, pRFAlgo, secret, label, seed, wantedLength) do
    {:ok, :tls_v1.prf(pRFAlgo, secret, label, seed, wantedLength)}
  end

  def select_session(
        suggestedSessionId,
        cipherSuites,
        hashSigns,
        compressions,
        sessIdTracker,
        r_session(ecc: eCCCurve0) = session,
        version,
        %{:ciphers => userSuites, :honor_cipher_order => honorCipherOrder} = sslOpts,
        cert
      ) do
    {sessionId, resumed} =
      :ssl_session.server_select_session(
        version,
        sessIdTracker,
        suggestedSessionId,
        sslOpts,
        cert
      )

    case resumed do
      :undefined ->
        suites = available_suites(cert, userSuites, version, hashSigns, eCCCurve0)
        cipherSuite0 = select_cipher_suite(cipherSuites, suites, honorCipherOrder)
        {eCCCurve, cipherSuite} = cert_curve(cert, eCCCurve0, cipherSuite0)
        compression = select_compression(compressions)

        {:new,
         r_session(session,
           session_id: sessionId,
           ecc: eCCCurve,
           cipher_suite: cipherSuite,
           compression_method: compression
         )}

      _ ->
        {:resumed, resumed}
    end
  end

  def supported_ecc({major, minor})
      when (major == 3 and minor >= 1) or major > 3 do
    curves = :tls_v1.ecc_curves(minor)
    r_elliptic_curves(elliptic_curve_list: curves)
  end

  def supported_ecc(_) do
    r_elliptic_curves(elliptic_curve_list: [])
  end

  def premaster_secret(otherPublicDhKey, myPrivateKey, r_DHParameter() = params) do
    try do
      :public_key.compute_key(otherPublicDhKey, myPrivateKey, params)
    catch
      :error, :computation_failed ->
        throw(
          r_alert(
            level: 2,
            description: 47,
            where: %{
              :mfa => {:ssl_handshake, :premaster_secret, 3},
              :line => 1077,
              :file => 'otp/lib/ssl/src/ssl_handshake.erl'
            }
          )
        )
    end
  end

  def premaster_secret(publicDhKey, privateDhKey, r_server_dh_params(dh_p: prime, dh_g: base)) do
    try do
      :crypto.compute_key(:dh, publicDhKey, privateDhKey, [prime, base])
    catch
      :error, :computation_failed ->
        throw(
          r_alert(
            level: 2,
            description: 47,
            where: %{
              :mfa => {:ssl_handshake, :premaster_secret, 3},
              :line => 1084,
              :file => 'otp/lib/ssl/src/ssl_handshake.erl'
            }
          )
        )
    end
  end

  def premaster_secret(
        r_client_srp_public(srp_a: clientPublicKey),
        serverKey,
        r_srp_user(prime: prime, verifier: verifier)
      ) do
    try do
      :crypto.compute_key(:srp, clientPublicKey, serverKey, {:host, [verifier, prime, :"6a"]})
    catch
      :error, _ ->
        throw(
          r_alert(
            level: 2,
            description: 47,
            where: %{
              :mfa => {:ssl_handshake, :premaster_secret, 3},
              :line => 1093,
              :file => 'otp/lib/ssl/src/ssl_handshake.erl'
            }
          )
        )
    else
      premasterSecret ->
        premasterSecret
    end
  end

  def premaster_secret(
        r_server_srp_params(srp_n: prime, srp_g: generator, srp_s: salt, srp_b: public),
        clientKeys,
        {username, password}
      ) do
    case :ssl_srp_primes.check_srp_params(
           generator,
           prime
         ) do
      :ok ->
        derivedKey =
          :crypto.hash(
            :sha,
            [
              salt,
              :crypto.hash(
                :sha,
                [username, <<?:>>, password]
              )
            ]
          )

        try do
          :crypto.compute_key(
            :srp,
            public,
            clientKeys,
            {:user, [derivedKey, prime, generator, :"6a"]}
          )
        catch
          :error ->
            throw(
              r_alert(
                level: 2,
                description: 47,
                where: %{
                  :mfa => {:ssl_handshake, :premaster_secret, 3},
                  :line => 1105,
                  :file => 'otp/lib/ssl/src/ssl_handshake.erl'
                }
              )
            )
        else
          premasterSecret ->
            premasterSecret
        end

      _ ->
        throw(
          r_alert(
            level: 2,
            description: 47,
            where: %{
              :mfa => {:ssl_handshake, :premaster_secret, 3},
              :line => 1108,
              :file => 'otp/lib/ssl/src/ssl_handshake.erl'
            }
          )
        )
    end
  end

  def premaster_secret(
        r_client_rsa_psk_identity(
          identity: pSKIdentity,
          exchange_keys: r_encrypted_premaster_secret(premaster_secret: encPMS)
        ),
        r_RSAPrivateKey() = key,
        pSKLookup
      ) do
    premasterSecret = premaster_secret(encPMS, key)
    psk_secret(pSKIdentity, pSKLookup, premasterSecret)
  end

  def premaster_secret(
        r_server_dhe_psk_params(
          hint: identityHint,
          dh_params: r_server_dh_params(dh_y: publicDhKey) = params
        ),
        privateDhKey,
        lookupFun
      ) do
    premasterSecret = premaster_secret(publicDhKey, privateDhKey, params)
    psk_secret(identityHint, lookupFun, premasterSecret)
  end

  def premaster_secret(
        r_server_ecdhe_psk_params(
          hint: identityHint,
          dh_params: r_server_ecdh_params(public: eCServerPubKey)
        ),
        privateEcDhKey,
        lookupFun
      ) do
    premasterSecret =
      premaster_secret(
        r_ECPoint(point: eCServerPubKey),
        privateEcDhKey
      )

    psk_secret(identityHint, lookupFun, premasterSecret)
  end

  def premaster_secret({:rsa_psk, pSKIdentity}, pSKLookup, rSAPremasterSecret) do
    psk_secret(pSKIdentity, pSKLookup, rSAPremasterSecret)
  end

  def premaster_secret(
        r_client_ecdhe_psk_identity(
          identity: pSKIdentity,
          dh_public: publicEcDhPoint
        ),
        privateEcDhKey,
        pSKLookup
      ) do
    premasterSecret =
      premaster_secret(
        r_ECPoint(point: publicEcDhPoint),
        privateEcDhKey
      )

    psk_secret(pSKIdentity, pSKLookup, premasterSecret)
  end

  def premaster_secret(
        r_client_dhe_psk_identity(identity: pSKIdentity, dh_public: publicDhKey),
        privateKey,
        r_DHParameter() = params,
        pSKLookup
      ) do
    premasterSecret = premaster_secret(publicDhKey, privateKey, params)
    psk_secret(pSKIdentity, pSKLookup, premasterSecret)
  end

  def premaster_secret(r_client_psk_identity(identity: pSKIdentity), pSKLookup) do
    psk_secret(pSKIdentity, pSKLookup)
  end

  def premaster_secret({:psk, pSKIdentity}, pSKLookup) do
    psk_secret(pSKIdentity, pSKLookup)
  end

  def premaster_secret(r_ECPoint() = eCPoint, r_ECPrivateKey() = eCDHKeys) do
    :public_key.compute_key(eCPoint, eCDHKeys)
  end

  def premaster_secret(encSecret, r_RSAPrivateKey() = rSAPrivateKey) do
    try do
      :public_key.decrypt_private(encSecret, rSAPrivateKey, [{:rsa_pad, :rsa_pkcs1_padding}])
    catch
      _, _ ->
        throw(
          r_alert(
            level: 2,
            description: 51,
            where: %{
              :mfa => {:ssl_handshake, :premaster_secret, 2},
              :line => 1154,
              :file => 'otp/lib/ssl/src/ssl_handshake.erl'
            }
          )
        )
    end
  end

  def premaster_secret(encSecret, %{:algorithm => :rsa} = engine) do
    try do
      :crypto.private_decrypt(:rsa, encSecret, :maps.remove(:algorithm, engine), [
        {:rsa_pad, :rsa_pkcs1_padding}
      ])
    catch
      _, _ ->
        throw(
          r_alert(
            level: 2,
            description: 51,
            where: %{
              :mfa => {:ssl_handshake, :premaster_secret, 2},
              :line => 1161,
              :file => 'otp/lib/ssl/src/ssl_handshake.erl'
            }
          )
        )
    end
  end

  def client_hello_extensions(
        version,
        cipherSuites,
        sslOpts,
        connectionStates,
        renegotiation,
        keyShare,
        ticketData,
        ocspNonce
      ) do
    helloExtensions0 = add_tls12_extensions(version, sslOpts, connectionStates, renegotiation)
    helloExtensions1 = add_common_extensions(version, helloExtensions0, cipherSuites, sslOpts)

    helloExtensions2 =
      maybe_add_certificate_status_request(
        version,
        sslOpts,
        ocspNonce,
        helloExtensions1
      )

    maybe_add_tls13_extensions(version, helloExtensions2, sslOpts, keyShare, ticketData)
  end

  defp add_tls12_extensions(
         _Version,
         %{
           :alpn_advertised_protocols => alpnAdvertisedProtocols,
           :next_protocol_selector => nextProtocolSelector,
           :server_name_indication => serverNameIndication,
           :max_fragment_length => maxFragmentLength
         } = sslOpts,
         connectionStates,
         renegotiation
       ) do
    sRP = srp_user(sslOpts)

    %{
      :renegotiation_info =>
        renegotiation_info(:tls_record, :client, connectionStates, renegotiation),
      :srp => sRP,
      :alpn => encode_alpn(alpnAdvertisedProtocols, renegotiation),
      :next_protocol_negotiation =>
        encode_client_protocol_negotiation(
          nextProtocolSelector,
          renegotiation
        ),
      :sni => sni(serverNameIndication),
      :max_frag_enum => max_frag_enum(maxFragmentLength)
    }
  end

  defp add_common_extensions({3, 4}, helloExtensions, _CipherSuites, %{
         :eccs => supportedECCs,
         :supported_groups => groups,
         :signature_algs => signatureSchemes
       }) do
    {ecPointFormats, _} = client_ecc_extensions(supportedECCs)

    %{
      helloExtensions
      | :ec_point_formats => ecPointFormats,
        :elliptic_curves => groups,
        :signature_algs => signature_algs_ext(signatureSchemes)
    }
  end

  defp add_common_extensions(version, helloExtensions, cipherSuites, %{
         :eccs => supportedECCs,
         :signature_algs => supportedHashSigns
       }) do
    {ecPointFormats, ellipticCurves} =
      case advertises_ec_ciphers(
             :lists.map(
               &:ssl_cipher_format.suite_bin_to_map/1,
               cipherSuites
             )
           ) do
        true ->
          client_ecc_extensions(supportedECCs)

        false ->
          {:undefined, :undefined}
      end

    %{
      helloExtensions
      | :ec_point_formats => ecPointFormats,
        :elliptic_curves => ellipticCurves,
        :signature_algs => available_signature_algs(supportedHashSigns, version)
    }
  end

  defp maybe_add_tls13_extensions(
         {3, 4},
         helloExtensions0,
         %{:signature_algs_cert => signatureSchemes, :versions => supportedVersions},
         keyShare,
         ticketData
       ) do
    helloExtensions1 = %{
      helloExtensions0
      | :client_hello_versions => r_client_hello_versions(versions: supportedVersions),
        :signature_algs_cert => signature_algs_cert(signatureSchemes)
    }

    helloExtensions =
      maybe_add_key_share(
        helloExtensions1,
        keyShare
      )

    maybe_add_pre_shared_key(helloExtensions, ticketData)
  end

  defp maybe_add_tls13_extensions(_, helloExtensions, _, _, _) do
    helloExtensions
  end

  defp maybe_add_certificate_status_request(
         _Version,
         %{:ocsp_stapling => false},
         _OcspNonce,
         helloExtensions
       ) do
    helloExtensions
  end

  defp maybe_add_certificate_status_request(
         _Version,
         %{:ocsp_stapling => true, :ocsp_responder_certs => ocspResponderCerts},
         ocspNonce,
         helloExtensions
       ) do
    ocspResponderList = get_ocsp_responder_list(ocspResponderCerts)
    ocspRequestExtns = :public_key.ocsp_extensions(ocspNonce)

    req =
      r_ocsp_status_request(
        responder_id_list: ocspResponderList,
        request_extensions: ocspRequestExtns
      )

    certStatusReqExtn = r_certificate_status_request(status_type: 1, request: req)
    %{helloExtensions | :status_request => certStatusReqExtn}
  end

  defp get_ocsp_responder_list(responderCerts) do
    get_ocsp_responder_list(responderCerts, [])
  end

  defp get_ocsp_responder_list([], acc) do
    acc
  end

  defp get_ocsp_responder_list([responderCert | t], acc) do
    get_ocsp_responder_list(
      t,
      [
        :public_key.ocsp_responder_id(responderCert)
        | acc
      ]
    )
  end

  defp maybe_add_key_share(helloExtensions, :undefined) do
    helloExtensions
  end

  defp maybe_add_key_share(helloExtensions, keyShare) do
    r_key_share_client_hello(client_shares: clientShares0) = keyShare

    clientShares =
      :lists.map(
        &kse_remove_private_key/1,
        clientShares0
      )

    %{helloExtensions | :key_share => r_key_share_client_hello(client_shares: clientShares)}
  end

  defp maybe_add_pre_shared_key(helloExtensions, :undefined) do
    helloExtensions
  end

  defp maybe_add_pre_shared_key(helloExtensions, ticketData) do
    {identities, binders} = get_identities_binders(ticketData)

    %{
      helloExtensions
      | :pre_shared_key =>
          r_pre_shared_key_client_hello(
            offered_psks:
              r_offered_psks(
                identities: identities,
                binders: binders
              )
          ),
        :psk_key_exchange_modes => r_psk_key_exchange_modes(ke_modes: [:psk_ke, :psk_dhe_ke])
    }
  end

  defp get_identities_binders(ticketData) do
    get_identities_binders(ticketData, {[], []}, 0)
  end

  defp get_identities_binders([], {identities, binders}, _) do
    {:lists.reverse(identities), :lists.reverse(binders)}
  end

  defp get_identities_binders([{key, _, identity, _, _, hKDF} | t], {i0, b0}, n) do
    binder = dummy_binder(hKDF)
    :tls_client_ticket_store.update_ticket(key, n)
    get_identities_binders(t, {[identity | i0], [binder | b0]}, n + 1)
  end

  defp dummy_binder(hKDF) do
    :binary.copy(<<0>>, :ssl_cipher.hash_size(hKDF))
  end

  def add_server_share(:server_hello, extensions, keyShare) do
    r_key_share_server_hello(server_share: serverShare0) = keyShare
    serverShare = kse_remove_private_key(serverShare0)
    %{extensions | :key_share => r_key_share_server_hello(server_share: serverShare)}
  end

  def add_server_share(
        :hello_retry_request,
        extensions,
        r_key_share_server_hello(server_share: r_key_share_entry(group: group))
      ) do
    %{extensions | :key_share => r_key_share_hello_retry_request(selected_group: group)}
  end

  def add_alpn(extensions, aLPN0) do
    aLPN = encode_alpn([aLPN0], false)
    %{extensions | :alpn => aLPN}
  end

  def add_selected_version(extensions) do
    supportedVersions = r_server_hello_selected_version(selected_version: {3, 4})
    %{extensions | :server_hello_selected_version => supportedVersions}
  end

  defp kse_remove_private_key(
         r_key_share_entry(
           group: group,
           key_exchange: r_ECPrivateKey(publicKey: publicKey)
         )
       ) do
    r_key_share_entry(group: group, key_exchange: publicKey)
  end

  defp kse_remove_private_key(
         r_key_share_entry(
           group: group,
           key_exchange: {publicKey, _}
         )
       ) do
    r_key_share_entry(group: group, key_exchange: publicKey)
  end

  defp signature_algs_ext(:undefined) do
    :undefined
  end

  defp signature_algs_ext(signatureSchemes0) do
    r_signature_algorithms(signature_scheme_list: signatureSchemes0)
  end

  defp signature_algs_cert(:undefined) do
    :undefined
  end

  defp signature_algs_cert(signatureSchemes) do
    r_signature_algorithms_cert(signature_scheme_list: signatureSchemes)
  end

  def handle_client_hello_extensions(
        recordCB,
        random,
        clientCipherSuites,
        exts,
        version,
        %{
          :secure_renegotiate => secureRenegotation,
          :alpn_preferred_protocols => aLPNPreferredProtocols
        } = opts,
        r_session(
          cipher_suite: negotiatedCipherSuite,
          compression_method: compression
        ) = session0,
        connectionStates0,
        renegotiation,
        isResumed
      ) do
    session =
      handle_srp_extension(
        :maps.get(:srp, exts, :undefined),
        session0
      )

    maxFragEnum = handle_mfl_extension(:maps.get(:max_frag_enum, exts, :undefined))

    connectionStates1 =
      :ssl_record.set_max_fragment_length(
        maxFragEnum,
        connectionStates0
      )

    connectionStates =
      handle_renegotiation_extension(
        :server,
        recordCB,
        version,
        :maps.get(
          :renegotiation_info,
          exts,
          :undefined
        ),
        random,
        negotiatedCipherSuite,
        clientCipherSuites,
        compression,
        connectionStates1,
        renegotiation,
        secureRenegotation
      )

    empty = empty_extensions(version, :server_hello)

    serverMaxFragEnum =
      cond do
        isResumed ->
          :undefined

        true ->
          maxFragEnum
      end

    serverHelloExtensions = %{
      empty
      | :renegotiation_info =>
          renegotiation_info(recordCB, :server, connectionStates, renegotiation),
        :ec_point_formats =>
          server_ecc_extension(
            version,
            :maps.get(
              :ec_point_formats,
              exts,
              :undefined
            )
          ),
        :max_frag_enum => serverMaxFragEnum
    }

    aLPN = :maps.get(:alpn, exts, :undefined)

    cond do
      aLPN !== :undefined and
          aLPNPreferredProtocols !== :undefined ->
        protocol =
          handle_alpn_extension(
            aLPNPreferredProtocols,
            decode_alpn(aLPN)
          )

        {session, connectionStates, protocol,
         %{serverHelloExtensions | :alpn => encode_alpn([protocol], renegotiation)}}

      true ->
        nextProtocolNegotiation = :maps.get(:next_protocol_negotiation, exts, :undefined)

        protocolsToAdvertise =
          handle_next_protocol_extension(
            nextProtocolNegotiation,
            renegotiation,
            opts
          )

        {session, connectionStates, :undefined,
         %{
           serverHelloExtensions
           | :next_protocol_negotiation =>
               encode_protocols_advertised_on_server(protocolsToAdvertise)
         }}
    end
  end

  def handle_server_hello_extensions(
        recordCB,
        random,
        cipherSuite,
        compression,
        exts,
        version,
        %{
          :secure_renegotiate => secureRenegotation,
          :next_protocol_selector => nextProtoSelector,
          :ocsp_stapling => stapling
        },
        connectionStates0,
        renegotiation,
        isNew
      ) do
    connectionStates =
      handle_renegotiation_extension(
        :client,
        recordCB,
        version,
        :maps.get(
          :renegotiation_info,
          exts,
          :undefined
        ),
        random,
        cipherSuite,
        :undefined,
        compression,
        connectionStates0,
        renegotiation,
        secureRenegotation
      )

    cond do
      isNew ->
        serverMaxFragEnum = :maps.get(:max_frag_enum, exts, :undefined)
        %{:current_write => %{:max_fragment_length => connMaxFragLen}} = connectionStates
        clientMaxFragEnum = max_frag_enum(connMaxFragLen)

        cond do
          serverMaxFragEnum == clientMaxFragEnum ->
            :ok

          true ->
            throw(
              r_alert(
                level: 2,
                description: 47,
                where: %{
                  :mfa => {:ssl_handshake, :handle_server_hello_extensions, 10},
                  :line => 1460,
                  :file => 'otp/lib/ssl/src/ssl_handshake.erl'
                }
              )
            )
        end

      true ->
        :ok
    end

    case handle_ocsp_extension(stapling, exts) do
      r_alert() = alert ->
        alert

      ocspState ->
        aLPN = :maps.get(:alpn, exts, :undefined)

        case decode_alpn(aLPN) do
          [protocol] when not renegotiation ->
            {connectionStates, :alpn, protocol, ocspState}

          [_] when renegotiation ->
            {connectionStates, :alpn, :undefined, ocspState}

          :undefined ->
            nextProtocolNegotiation = :maps.get(:next_protocol_negotiation, exts, :undefined)

            protocol =
              handle_next_protocol(nextProtocolNegotiation, nextProtoSelector, renegotiation)

            {connectionStates, :npn, protocol, ocspState}

          {:error, reason} ->
            r_alert(
              level: 2,
              description: 40,
              where: %{
                :mfa => {:ssl_handshake, :handle_server_hello_extensions, 10},
                :line => 1485,
                :file => 'otp/lib/ssl/src/ssl_handshake.erl'
              },
              reason: reason
            )

          [] ->
            r_alert(
              level: 2,
              description: 40,
              where: %{
                :mfa => {:ssl_handshake, :handle_server_hello_extensions, 10},
                :line => 1487,
                :file => 'otp/lib/ssl/src/ssl_handshake.erl'
              },
              reason: :no_protocols_in_server_hello
            )

          [_ | _] ->
            r_alert(
              level: 2,
              description: 40,
              where: %{
                :mfa => {:ssl_handshake, :handle_server_hello_extensions, 10},
                :line => 1489,
                :file => 'otp/lib/ssl/src/ssl_handshake.erl'
              },
              reason: :too_many_protocols_in_server_hello
            )
        end
    end
  end

  def select_curve(client, server) do
    select_curve(client, server, false)
  end

  def select_curve(
        r_elliptic_curves(elliptic_curve_list: clientCurves),
        r_elliptic_curves(elliptic_curve_list: serverCurves),
        serverOrder
      ) do
    case serverOrder do
      false ->
        select_shared_curve(clientCurves, serverCurves)

      true ->
        select_shared_curve(serverCurves, clientCurves)
    end
  end

  def select_curve(:undefined, _, _) do
    {:namedCurve, {1, 2, 840, 10045, 3, 1, 7}}
  end

  def select_hashsign(_, _, keyExAlgo, _, _Version)
      when keyExAlgo == :dh_anon or keyExAlgo == :ecdh_anon or
             keyExAlgo == :srp_anon or keyExAlgo == :psk or
             keyExAlgo == :dhe_psk or keyExAlgo == :ecdhe_psk do
    {:null, :anon}
  end

  def select_hashsign(
        {clientHashSigns, clientSignatureSchemes},
        cert,
        keyExAlgo,
        :undefined,
        {major, minor} = version
      )
      when major >= 3 and minor >= 3 do
    select_hashsign(
      {clientHashSigns, clientSignatureSchemes},
      cert,
      keyExAlgo,
      :tls_v1.default_signature_algs(version),
      version
    )
  end

  def select_hashsign(
        {r_hash_sign_algos(hash_sign_algos: clientHashSigns), clientSignatureSchemes0},
        cert,
        keyExAlgo,
        supportedHashSigns,
        {major, minor}
      )
      when major >= 3 and minor >= 3 do
    clientSignatureSchemes = get_signature_scheme(clientSignatureSchemes0)
    {signAlgo0, param, publicKeyAlgo0, _} = get_cert_params(cert)
    signAlgo = sign_algo(signAlgo0)
    publicKeyAlgo = public_key_algo(publicKeyAlgo0)

    case is_supported_sign(signAlgo, param, clientHashSigns, clientSignatureSchemes) do
      true ->
        case :lists.filter(
               fn
                 {_, s} = algos
                 when s == publicKeyAlgo ->
                   is_acceptable_hash_sign(algos, keyExAlgo, supportedHashSigns)

                 _ ->
                   false
               end,
               clientHashSigns
             ) do
          [] ->
            r_alert(
              level: 2,
              description: 71,
              where: %{
                :mfa => {:ssl_handshake, :select_hashsign, 5},
                :line => 1565,
                :file => 'otp/lib/ssl/src/ssl_handshake.erl'
              },
              reason: :no_suitable_signature_algorithm
            )

          [hashSign | _] ->
            hashSign
        end

      false ->
        r_alert(
          level: 2,
          description: 71,
          where: %{
            :mfa => {:ssl_handshake, :select_hashsign, 5},
            :line => 1570,
            :file => 'otp/lib/ssl/src/ssl_handshake.erl'
          },
          reason: :no_suitable_signature_algorithm
        )
    end
  end

  def select_hashsign(_, cert, _, _, version) do
    r_OTPCertificate(tbsCertificate: tBSCert) =
      :public_key.pkix_decode_cert(
        cert,
        :otp
      )

    r_OTPSubjectPublicKeyInfo(algorithm: {_, algo, _}) =
      r_OTPTBSCertificate(tBSCert, :subjectPublicKeyInfo)

    select_hashsign_algs(:undefined, algo, version)
  end

  def select_hashsign(
        r_certificate_request(
          hashsign_algorithms: r_hash_sign_algos(hash_sign_algos: hashSigns),
          certificate_types: types
        ),
        cert,
        supportedHashSigns,
        {major, minor}
      )
      when major >= 3 and minor >= 3 do
    {signAlgo0, param, publicKeyAlgo0, _} = get_cert_params(cert)
    signAlgo = sign_algo(signAlgo0)
    publicKeyAlgo = public_key_algo(publicKeyAlgo0)

    case is_acceptable_cert_type(
           publicKeyAlgo,
           types
         ) and
           is_supported_sign(
             signAlgo,
             param,
             hashSigns,
             :undefined
           ) do
      true ->
        case :lists.filter(
               fn
                 {_, s} = algos
                 when s == publicKeyAlgo ->
                   is_acceptable_hash_sign(
                     algos,
                     supportedHashSigns
                   )

                 _ ->
                   false
               end,
               hashSigns
             ) do
          [] ->
            r_alert(
              level: 2,
              description: 71,
              where: %{
                :mfa => {:ssl_handshake, :select_hashsign, 4},
                :line => 1606,
                :file => 'otp/lib/ssl/src/ssl_handshake.erl'
              },
              reason: :no_suitable_signature_algorithm
            )

          [hashSign | _] ->
            hashSign
        end

      false ->
        r_alert(
          level: 2,
          description: 71,
          where: %{
            :mfa => {:ssl_handshake, :select_hashsign, 4},
            :line => 1611,
            :file => 'otp/lib/ssl/src/ssl_handshake.erl'
          },
          reason: :no_suitable_signature_algorithm
        )
    end
  end

  def select_hashsign(r_certificate_request(certificate_types: types), cert, _, version) do
    {_, _, publicKeyAlgo0, _} = get_cert_params(cert)
    publicKeyAlgo = public_key_algo(publicKeyAlgo0)

    case is_acceptable_cert_type(publicKeyAlgo, types) do
      true ->
        select_hashsign(:undefined, cert, :undefined, [], version)

      false ->
        r_alert(
          level: 2,
          description: 71,
          where: %{
            :mfa => {:ssl_handshake, :select_hashsign, 4},
            :line => 1622,
            :file => 'otp/lib/ssl/src/ssl_handshake.erl'
          },
          reason: :no_suitable_signature_algorithm
        )
    end
  end

  def get_cert_params(cert) do
    r_OTPCertificate(
      tbsCertificate: tBSCert,
      signatureAlgorithm: {_, signAlgo, param}
    ) =
      :public_key.pkix_decode_cert(
        cert,
        :otp
      )

    r_OTPSubjectPublicKeyInfo(
      algorithm: {_, publicKeyAlgo, _},
      subjectPublicKey: publicKey
    ) = r_OTPTBSCertificate(tBSCert, :subjectPublicKeyInfo)

    rSAKeySize =
      case publicKey do
        r_RSAPublicKey(modulus: modulus) ->
          byte_size(:binary.encode_unsigned(modulus))

        _ ->
          :undefined
      end

    {signAlgo, param, publicKeyAlgo, rSAKeySize}
  end

  defp get_signature_scheme(:undefined) do
    :undefined
  end

  defp get_signature_scheme(
         r_signature_algorithms_cert(signature_scheme_list: clientSignatureSchemes)
       ) do
    clientSignatureSchemes
  end

  def select_hashsign_algs(hashSign, _, {major, minor})
      when hashSign !== :undefined and major >= 3 and minor >= 3 do
    hashSign
  end

  def select_hashsign_algs(:undefined, {1, 2, 840, 113_549, 1, 1, 1}, {major, minor})
      when major >= 3 and minor >= 3 do
    {:sha, :rsa}
  end

  def select_hashsign_algs(:undefined, {1, 2, 840, 10045, 2, 1}, _) do
    {:sha, :ecdsa}
  end

  def select_hashsign_algs(:undefined, {1, 2, 840, 113_549, 1, 1, 1}, _) do
    {:md5sha, :rsa}
  end

  def select_hashsign_algs(:undefined, {1, 2, 840, 10040, 4, 1}, _) do
    {:sha, :dsa}
  end

  defp srp_user(%{:srp_identity => {userName, _}}) do
    r_srp(username: userName)
  end

  defp srp_user(_) do
    :undefined
  end

  def extension_value(:undefined) do
    :undefined
  end

  def extension_value(r_sni(hostname: hostName)) do
    hostName
  end

  def extension_value(r_ec_point_formats(ec_point_format_list: list)) do
    list
  end

  def extension_value(r_elliptic_curves(elliptic_curve_list: list)) do
    list
  end

  def extension_value(r_supported_groups(supported_groups: supportedGroups)) do
    supportedGroups
  end

  def extension_value(r_hash_sign_algos(hash_sign_algos: algos)) do
    algos
  end

  def extension_value(r_alpn(extension_data: data)) do
    data
  end

  def extension_value(r_max_frag_enum(enum: enum)) do
    enum
  end

  def extension_value(r_next_protocol_negotiation(extension_data: data)) do
    data
  end

  def extension_value(r_srp(username: name)) do
    name
  end

  def extension_value(r_renegotiation_info(renegotiated_connection: data)) do
    data
  end

  def extension_value(r_signature_algorithms(signature_scheme_list: schemes)) do
    schemes
  end

  def extension_value(r_signature_algorithms_cert(signature_scheme_list: schemes)) do
    schemes
  end

  def extension_value(r_key_share_client_hello(client_shares: clientShares)) do
    clientShares
  end

  def extension_value(r_key_share_server_hello(server_share: serverShare)) do
    serverShare
  end

  def extension_value(r_client_hello_versions(versions: versions)) do
    versions
  end

  def extension_value(r_server_hello_selected_version(selected_version: selectedVersion)) do
    selectedVersion
  end

  def extension_value(r_pre_shared_key_client_hello(offered_psks: pSKs)) do
    pSKs
  end

  def extension_value(r_pre_shared_key_server_hello(selected_identity: selectedIdentity)) do
    selectedIdentity
  end

  def extension_value(r_psk_key_exchange_modes(ke_modes: modes)) do
    modes
  end

  def extension_value(r_cookie(cookie: cookie)) do
    cookie
  end

  defp handle_ocsp_extension(true = stapling, extensions) do
    case :maps.get(:status_request, extensions, false) do
      :undefined ->
        %{:ocsp_stapling => stapling, :ocsp_expect => :staple}

      false ->
        %{:ocsp_stapling => stapling, :ocsp_expect => :no_staple}

      _Else ->
        r_alert(
          level: 2,
          description: 40,
          where: %{
            :mfa => {:ssl_handshake, :handle_ocsp_extension, 2},
            :line => 1746,
            :file => 'otp/lib/ssl/src/ssl_handshake.erl'
          },
          reason: :status_request_not_empty
        )
    end
  end

  defp handle_ocsp_extension(false = stapling, extensions) do
    case :maps.get(:status_request, extensions, false) do
      false ->
        %{:ocsp_stapling => stapling, :ocsp_expect => :no_staple}

      _Else ->
        r_alert(
          level: 2,
          description: 40,
          where: %{
            :mfa => {:ssl_handshake, :handle_ocsp_extension, 2},
            :line => 1754,
            :file => 'otp/lib/ssl/src/ssl_handshake.erl'
          },
          reason: :unexpected_status_request
        )
    end
  end

  defp int_to_bin(i) do
    l = div(length(:erlang.integer_to_list(i, 16)) + 1, 2)
    <<i::size(l * 8)>>
  end

  defp certificate_types(_, {n, m}) when n >= 3 and m >= 1 do
    eCDSA = supported_cert_type_or_empty(:ecdsa, 64)
    rSA = supported_cert_type_or_empty(:rsa, 1)
    dSS = supported_cert_type_or_empty(:dss, 2)
    <<eCDSA::binary, rSA::binary, dSS::binary>>
  end

  defp certificate_types(_, _) do
    rSA = supported_cert_type_or_empty(:rsa, 1)
    dSS = supported_cert_type_or_empty(:dss, 2)
    <<rSA::binary, dSS::binary>>
  end

  defp supported_cert_type_or_empty(algo, type) do
    case :proplists.get_bool(
           algo,
           :proplists.get_value(
             :public_keys,
             :crypto.supports()
           )
         ) do
      true ->
        <<type::size(8)-unsigned-big-integer>>

      false ->
        <<>>
    end
  end

  defp certificate_authorities(certDbHandle, certDbRef) do
    authorities =
      certificate_authorities_from_db(
        certDbHandle,
        certDbRef
      )

    enc = fn r_OTPCertificate(tbsCertificate: tBSCert) ->
      oTPSubj = r_OTPTBSCertificate(tBSCert, :subject)
      dNEncodedBin = :public_key.pkix_encode(:Name, oTPSubj, :otp)
      dNEncodedLen = byte_size(dNEncodedBin)
      <<dNEncodedLen::size(16)-unsigned-big-integer, dNEncodedBin::binary>>
    end

    :erlang.list_to_binary(
      for {_, cert} <- authorities do
        enc.(cert)
      end
    )
  end

  defp certificate_authorities_from_db(certDbHandle, certDbRef)
       when is_reference(certDbRef) do
    connectionCerts = fn
      {{ref, _, _}, cert}, acc
      when ref == certDbRef ->
        [cert | acc]

      _, acc ->
        acc
    end

    :ssl_pkix_db.foldl(connectionCerts, [], certDbHandle)
  end

  defp certificate_authorities_from_db(_CertDbHandle, {:extracted, certDbData}) do
    :lists.foldl(
      fn {:decoded, {_Key, cert}}, acc ->
        [cert | acc]
      end,
      [],
      certDbData
    )
  end

  def validation_fun_and_state({fun, userState0}, verifyState, certPath, logLevel) do
    {fn
       otpCert, {:extension, _} = extension, {sslState, userState} ->
         case :ssl_certificate.validate(otpCert, extension, sslState) do
           {:valid, newSslState} ->
             {:valid, {newSslState, userState}}

           {:fail, reason} ->
             apply_user_fun(fun, otpCert, reason, userState, sslState, certPath, logLevel)

           {:unknown, _} ->
             apply_user_fun(fun, otpCert, extension, userState, sslState, certPath, logLevel)
         end

       otpCert, verifyResult, {sslState, userState} ->
         apply_user_fun(fun, otpCert, verifyResult, userState, sslState, certPath, logLevel)
     end, {verifyState, userState0}}
  end

  def validation_fun_and_state(:undefined, verifyState, certPath, logLevel) do
    {fn
       otpCert, {:extension, _} = extension, sslState ->
         :ssl_certificate.validate(otpCert, extension, sslState)

       otpCert, verifyResult, sslState
       when verifyResult == :valid or verifyResult == :valid_peer ->
         case cert_status_check(otpCert, sslState, verifyResult, certPath, logLevel) do
           :valid ->
             :ssl_certificate.validate(otpCert, verifyResult, sslState)

           reason ->
             {:fail, reason}
         end

       otpCert, verifyResult, sslState ->
         :ssl_certificate.validate(otpCert, verifyResult, sslState)
     end, verifyState}
  end

  defp apply_user_fun(fun, otpCert, verifyResult0, userState0, sslState, certPath, logLevel)
       when verifyResult0 == :valid or verifyResult0 == :valid_peer do
    verifyResult = maybe_check_hostname(otpCert, verifyResult0, sslState)

    case fun.(otpCert, verifyResult, userState0) do
      {valid, userState}
      when valid == :valid or valid == :valid_peer ->
        case cert_status_check(otpCert, sslState, verifyResult, certPath, logLevel) do
          :valid ->
            {valid, {sslState, userState}}

          result ->
            apply_user_fun(fun, otpCert, result, userState, sslState, certPath, logLevel)
        end

      {:fail, _} = fail ->
        fail
    end
  end

  defp apply_user_fun(fun, otpCert, extensionOrError, userState0, sslState, _CertPath, _LogLevel) do
    case fun.(otpCert, extensionOrError, userState0) do
      {valid, userState}
      when valid == :valid or valid == :valid_peer ->
        {valid, {sslState, userState}}

      {:fail, _} = fail ->
        fail

      {:unknown, userState} ->
        {:unknown, {sslState, userState}}
    end
  end

  defp maybe_check_hostname(otpCert, :valid_peer, sslState) do
    case :ssl_certificate.validate(otpCert, :valid_peer, sslState) do
      {:valid, _} ->
        :valid_peer

      {:fail, reason} ->
        reason
    end
  end

  defp maybe_check_hostname(_, :valid, _) do
    :valid
  end

  def handle_path_validation_error(
        {:bad_cert, :unknown_ca} = reason,
        peerCert,
        chain,
        opts,
        options,
        certDbHandle,
        certsDbRef
      ) do
    handle_incomplete_chain(peerCert, chain, opts, options, certDbHandle, certsDbRef, reason)
  end

  def handle_path_validation_error(
        {:bad_cert, :invalid_issuer} = reason,
        peerCert,
        chain0,
        opts,
        options,
        certDbHandle,
        certsDbRef
      ) do
    handle_unordered_chain(peerCert, chain0, opts, options, certDbHandle, certsDbRef, reason)
  end

  def handle_path_validation_error(reason, _, _, _, _, _, _) do
    path_validation_alert(reason)
  end

  defp handle_incomplete_chain(
         peerCert,
         chain0,
         %{:partial_chain => partialChain} = opts,
         options,
         certDbHandle,
         certsDbRef,
         reason
       ) do
    case :ssl_certificate.certificate_chain(peerCert, certDbHandle, certsDbRef) do
      {:ok, _, [^peerCert | _] = chain} when chain !== chain0 ->
        case :ssl_certificate.trusted_cert_and_path(chain, certDbHandle, certsDbRef, partialChain) do
          {:unknown_ca, []} ->
            path_validation_alert(reason)

          {trusted, path} ->
            case :public_key.pkix_path_validation(trusted, path, options) do
              {:ok, {publicKeyInfo, _}} ->
                {peerCert, publicKeyInfo}

              {:error, pathError} ->
                handle_unordered_chain(
                  peerCert,
                  chain0,
                  opts,
                  options,
                  certDbHandle,
                  certsDbRef,
                  pathError
                )
            end
        end

      _ ->
        handle_unordered_chain(peerCert, chain0, opts, options, certDbHandle, certsDbRef, reason)
    end
  end

  defp handle_unordered_chain(
         peerCert,
         chain0,
         %{:partial_chain => partialChain},
         options,
         certDbHandle,
         certsDbRef,
         reason
       ) do
    {:ok, extractedCerts} = :ssl_pkix_db.extract_trusted_certs({:der, chain0})

    case :ssl_certificate.certificate_chain(peerCert, certDbHandle, extractedCerts, chain0) do
      {:ok, _, chain} when chain !== chain0 ->
        case :ssl_certificate.trusted_cert_and_path(chain, certDbHandle, certsDbRef, partialChain) do
          {:unknown_ca, []} ->
            path_validation_alert(reason)

          {trusted, path} ->
            case :public_key.pkix_path_validation(trusted, path, options) do
              {:ok, {publicKeyInfo, _}} ->
                {peerCert, publicKeyInfo}

              {:error, pathError} ->
                path_validation_alert(pathError)
            end
        end

      _ ->
        path_validation_alert(reason)
    end
  end

  defp path_validation_alert({:bad_cert, :cert_expired}) do
    r_alert(
      level: 2,
      description: 45,
      where: %{
        :mfa => {:ssl_handshake, :path_validation_alert, 1},
        :line => 1938,
        :file => 'otp/lib/ssl/src/ssl_handshake.erl'
      }
    )
  end

  defp path_validation_alert({:bad_cert, :invalid_issuer}) do
    r_alert(
      level: 2,
      description: 42,
      where: %{
        :mfa => {:ssl_handshake, :path_validation_alert, 1},
        :line => 1940,
        :file => 'otp/lib/ssl/src/ssl_handshake.erl'
      }
    )
  end

  defp path_validation_alert({:bad_cert, :invalid_signature}) do
    r_alert(
      level: 2,
      description: 42,
      where: %{
        :mfa => {:ssl_handshake, :path_validation_alert, 1},
        :line => 1942,
        :file => 'otp/lib/ssl/src/ssl_handshake.erl'
      }
    )
  end

  defp path_validation_alert({:bad_cert, :name_not_permitted}) do
    r_alert(
      level: 2,
      description: 42,
      where: %{
        :mfa => {:ssl_handshake, :path_validation_alert, 1},
        :line => 1944,
        :file => 'otp/lib/ssl/src/ssl_handshake.erl'
      }
    )
  end

  defp path_validation_alert({:bad_cert, :unknown_critical_extension}) do
    r_alert(
      level: 2,
      description: 43,
      where: %{
        :mfa => {:ssl_handshake, :path_validation_alert, 1},
        :line => 1946,
        :file => 'otp/lib/ssl/src/ssl_handshake.erl'
      }
    )
  end

  defp path_validation_alert({:bad_cert, {:revoked, _}}) do
    r_alert(
      level: 2,
      description: 44,
      where: %{
        :mfa => {:ssl_handshake, :path_validation_alert, 1},
        :line => 1948,
        :file => 'otp/lib/ssl/src/ssl_handshake.erl'
      }
    )
  end

  defp path_validation_alert({:bad_cert, {:revocation_status_undetermined, details}}) do
    alert =
      r_alert(
        level: 2,
        description: 42,
        where: %{
          :mfa => {:ssl_handshake, :path_validation_alert, 1},
          :line => 1950,
          :file => 'otp/lib/ssl/src/ssl_handshake.erl'
        }
      )

    r_alert(alert, reason: details)
  end

  defp path_validation_alert({:bad_cert, :selfsigned_peer}) do
    r_alert(
      level: 2,
      description: 42,
      where: %{
        :mfa => {:ssl_handshake, :path_validation_alert, 1},
        :line => 1953,
        :file => 'otp/lib/ssl/src/ssl_handshake.erl'
      }
    )
  end

  defp path_validation_alert({:bad_cert, :unknown_ca}) do
    r_alert(
      level: 2,
      description: 48,
      where: %{
        :mfa => {:ssl_handshake, :path_validation_alert, 1},
        :line => 1955,
        :file => 'otp/lib/ssl/src/ssl_handshake.erl'
      }
    )
  end

  defp path_validation_alert(reason) do
    r_alert(
      level: 2,
      description: 40,
      where: %{
        :mfa => {:ssl_handshake, :path_validation_alert, 1},
        :line => 1957,
        :file => 'otp/lib/ssl/src/ssl_handshake.erl'
      },
      reason: reason
    )
  end

  def digitally_signed(version, hashes, hashAlgo, privateKey, signAlgo) do
    try do
      do_digitally_signed(version, hashes, hashAlgo, privateKey, signAlgo)
    catch
      :error, :badkey ->
        throw(
          r_alert(
            level: 2,
            description: 40,
            where: %{
              :mfa => {:ssl_handshake, :digitally_signed, 5},
              :line => 1965,
              :file => 'otp/lib/ssl/src/ssl_handshake.erl'
            },
            reason: bad_key(privateKey)
          )
        )
    else
      signature ->
        signature
    end
  end

  defp do_digitally_signed({3, minor}, hash, _, %{:algorithm => :rsa} = engine, :rsa)
       when minor <= 2 do
    :crypto.private_encrypt(:rsa, hash, :maps.remove(:algorithm, engine), :rsa_pkcs1_padding)
  end

  defp do_digitally_signed({3, minor}, hash, hashAlgo, %{:algorithm => alg} = engine, signAlgo)
       when minor > 3 do
    options = signature_options(signAlgo, hashAlgo)
    :crypto.sign(alg, hashAlgo, hash, :maps.remove(:algorithm, engine), options)
  end

  defp do_digitally_signed({3, minor}, hash, hashAlgo, %{:algorithm => alg} = engine, signAlgo)
       when minor > 3 do
    options = signature_options(signAlgo, hashAlgo)
    :crypto.sign(alg, hashAlgo, hash, :maps.remove(:algorithm, engine), options)
  end

  defp do_digitally_signed({3, 3}, hash, hashAlgo, %{:algorithm => alg} = engine, signAlgo) do
    options = signature_options(signAlgo, hashAlgo)
    :crypto.sign(alg, hashAlgo, {:digest, hash}, :maps.remove(:algorithm, engine), options)
  end

  defp do_digitally_signed(
         {3, 4},
         hash,
         hashAlgo,
         {r_RSAPrivateKey() = key, r_RSASSA_PSS_params()},
         signAlgo
       ) do
    options = signature_options(signAlgo, hashAlgo)
    :public_key.sign(hash, hashAlgo, key, options)
  end

  defp do_digitally_signed({3, 4}, hash, hashAlgo, key, signAlgo) do
    options = signature_options(signAlgo, hashAlgo)
    :public_key.sign(hash, hashAlgo, key, options)
  end

  defp do_digitally_signed({3, minor}, hash, hashAlgo, key, signAlgo)
       when minor >= 3 do
    options = signature_options(hashAlgo, signAlgo)
    :public_key.sign({:digest, hash}, hashAlgo, key, options)
  end

  defp do_digitally_signed({3, minor}, hash, _HashAlgo, r_RSAPrivateKey() = key, :rsa)
       when minor <= 2 do
    :public_key.encrypt_private(hash, key, [{:rsa_pad, :rsa_pkcs1_padding}])
  end

  defp do_digitally_signed(_Version, hash, hashAlgo, key, _SignAlgo) do
    :public_key.sign({:digest, hash}, hashAlgo, key)
  end

  defp signature_options(signAlgo, hashAlgo)
       when signAlgo === :rsa_pss_rsae or signAlgo === :rsa_pss_pss do
    pss_options(hashAlgo)
  end

  defp signature_options(_, _) do
    []
  end

  defp verify_options(signAlgo, hashAlgo, _KeyParams)
       when signAlgo === :rsa_pss_rsae or signAlgo === :rsa_pss_pss do
    pss_options(hashAlgo)
  end

  defp verify_options(_, _, _) do
    []
  end

  defp pss_options(hashAlgo) do
    [{:rsa_padding, :rsa_pkcs1_pss_padding}, {:rsa_pss_saltlen, -1}, {:rsa_mgf1_md, hashAlgo}]
  end

  defp bad_key(r_DSAPrivateKey()) do
    :unacceptable_dsa_key
  end

  defp bad_key(r_RSAPrivateKey()) do
    :unacceptable_rsa_key
  end

  defp bad_key(r_ECPrivateKey()) do
    :unacceptable_ecdsa_key
  end

  defp bad_key(%{:algorithm => :rsa}) do
    :unacceptable_rsa_key
  end

  defp bad_key(%{:algorithm => :ecdsa}) do
    :unacceptable_ecdsa_key
  end

  defp cert_status_check(
         _,
         %{:ocsp_state => %{:ocsp_stapling => true, :ocsp_expect => :stapled}},
         _VerifyResult,
         _,
         _
       ) do
    :valid
  end

  defp cert_status_check(
         otpCert,
         %{:ocsp_state => %{:ocsp_stapling => false}} = sslState,
         verifyResult,
         certPath,
         logLevel
       ) do
    maybe_check_crl(otpCert, sslState, verifyResult, certPath, logLevel)
  end

  defp cert_status_check(
         otpCert,
         %{:ocsp_state => %{:ocsp_stapling => :best_effort, :ocsp_expect => :undetermined}} =
           sslState,
         verifyResult,
         certPath,
         logLevel
       ) do
    maybe_check_crl(otpCert, sslState, verifyResult, certPath, logLevel)
  end

  defp maybe_check_crl(_, %{:crl_check => false}, _, _, _) do
    :valid
  end

  defp maybe_check_crl(_, %{:crl_check => :peer}, _, :valid, _) do
    :valid
  end

  defp maybe_check_crl(
         otpCert,
         %{
           :crl_check => check,
           :certdb => certDbHandle,
           :certdb_ref => certDbRef,
           :crl_db => {callback, cRLDbHandle}
         },
         _,
         certPath,
         logLevel
       ) do
    options = [
      {:issuer_fun,
       {fn _DP, cRL, issuer, dBInfo ->
          :ssl_crl.trusted_cert_and_path(cRL, issuer, {certPath, dBInfo})
        end, {certDbHandle, certDbRef}}},
      {:update_crl,
       fn dP, cRL ->
         case callback.fresh_crl(dP, cRL) do
           {:logger, logInfo, fresh} ->
             handle_log(logLevel, logInfo)
             fresh

           fresh ->
             fresh
         end
       end},
      {:undetermined_details, true}
    ]

    case dps_and_crls(otpCert, callback, cRLDbHandle, :ext, logLevel) do
      :no_dps ->
        crl_check_same_issuer(
          otpCert,
          check,
          dps_and_crls(otpCert, callback, cRLDbHandle, :same_issuer, logLevel),
          options
        )

      dpsAndCRLs ->
        case :public_key.pkix_crls_validate(otpCert, dpsAndCRLs, options) do
          {:bad_cert, {:revocation_status_undetermined, _}} ->
            crl_check_same_issuer(
              otpCert,
              check,
              dps_and_crls(otpCert, callback, cRLDbHandle, :same_issuer, logLevel),
              options
            )

          other ->
            other
        end
    end
  end

  defp crl_check_same_issuer(otpCert, :best_effort, dps, options) do
    case :public_key.pkix_crls_validate(otpCert, dps, options) do
      {:bad_cert, {:revocation_status_undetermined, _}} ->
        :valid

      other ->
        other
    end
  end

  defp crl_check_same_issuer(otpCert, _, dps, options) do
    :public_key.pkix_crls_validate(otpCert, dps, options)
  end

  defp dps_and_crls(otpCert, callback, cRLDbHandle, :ext, logLevel) do
    case :public_key.pkix_dist_points(otpCert) do
      [] ->
        :no_dps

      distPoints ->
        issuer = r_OTPTBSCertificate(r_OTPCertificate(otpCert, :tbsCertificate), :issuer)
        cRLs = distpoints_lookup(distPoints, issuer, callback, cRLDbHandle, logLevel)
        dps_and_crls(distPoints, cRLs, [])
    end
  end

  defp dps_and_crls(otpCert, callback, cRLDbHandle, :same_issuer, logLevel) do
    dP =
      r_DistributionPoint(distributionPoint: {:fullName, genNames}) =
      :public_key.pkix_dist_point(otpCert)

    cRLs =
      :lists.flatmap(
        fn
          {:directoryName, issuer} ->
            case callback.select(issuer, cRLDbHandle) do
              {:logger, logInfo, return} ->
                handle_log(logLevel, logInfo)
                return

              return ->
                return
            end

          _ ->
            []
        end,
        genNames
      )

    for cRL <- cRLs do
      {dP, {cRL, :public_key.der_decode(:CertificateList, cRL)}}
    end
  end

  defp dps_and_crls([], _, acc) do
    acc
  end

  defp dps_and_crls([dP | rest], cRLs, acc) do
    dpCRL =
      for cRL <- cRLs do
        {dP, {cRL, :public_key.der_decode(:CertificateList, cRL)}}
      end

    dps_and_crls(rest, cRLs, dpCRL ++ acc)
  end

  defp distpoints_lookup([], _, _, _, _) do
    []
  end

  defp distpoints_lookup([distPoint | rest], issuer, callback, cRLDbHandle, logLevel) do
    result =
      try do
        callback.lookup(distPoint, issuer, cRLDbHandle)
      catch
        :error, :undef ->
          callback.lookup(distPoint, cRLDbHandle)
      end

    case result do
      :not_available ->
        distpoints_lookup(rest, issuer, callback, cRLDbHandle, logLevel)

      {:logger, logInfo, cRLs} ->
        handle_log(logLevel, logInfo)
        cRLs

      cRLs ->
        cRLs
    end
  end

  defp encrypted_premaster_secret(secret, rSAPublicKey) do
    try do
      preMasterSecret =
        :public_key.encrypt_public(
          secret,
          rSAPublicKey,
          [{:rsa_pad, :rsa_pkcs1_padding}]
        )

      r_encrypted_premaster_secret(premaster_secret: preMasterSecret)
    catch
      _, _ ->
        throw(
          r_alert(
            level: 2,
            description: 40,
            where: %{
              :mfa => {:ssl_handshake, :encrypted_premaster_secret, 2},
              :line => 2150,
              :file => 'otp/lib/ssl/src/ssl_handshake.erl'
            },
            reason: :premaster_encryption_failed
          )
        )
    end
  end

  defp calc_certificate_verify({3, n}, hashAlgo, _MasterSecret, handshake) do
    :tls_v1.certificate_verify(hashAlgo, n, :lists.reverse(handshake))
  end

  defp calc_finished({3, n}, role, prfAlgo, masterSecret, handshake) do
    :tls_v1.finished(role, n, prfAlgo, masterSecret, :lists.reverse(handshake))
  end

  defp master_secret(
         version,
         masterSecret,
         r_security_parameters(
           bulk_cipher_algorithm: bCA,
           client_random: clientRandom,
           server_random: serverRandom,
           hash_size: hashSize,
           prf_algorithm: prfAlgo,
           key_material_length: kML,
           expanded_key_material_length: eKML,
           iv_size: iVS
         ),
         connectionStates,
         role
       ) do
    {clientWriteMacSecret, serverWriteMacSecret, clientWriteKey, serverWriteKey, clientIV,
     serverIV} =
      setup_keys(
        version,
        prfAlgo,
        masterSecret,
        serverRandom,
        clientRandom,
        hashSize,
        kML,
        eKML,
        iVS
      )

    connStates1 =
      :ssl_record.set_master_secret(
        masterSecret,
        connectionStates
      )

    connStates2 =
      :ssl_record.set_mac_secret(clientWriteMacSecret, serverWriteMacSecret, role, connStates1)

    clientCipherState = :ssl_cipher.cipher_init(bCA, clientIV, clientWriteKey)
    serverCipherState = :ssl_cipher.cipher_init(bCA, serverIV, serverWriteKey)

    {masterSecret,
     :ssl_record.set_pending_cipher_state(connStates2, clientCipherState, serverCipherState, role)}
  end

  defp setup_keys(
         {3, n},
         prfAlgo,
         masterSecret,
         serverRandom,
         clientRandom,
         hashSize,
         kML,
         _EKML,
         iVS
       ) do
    :tls_v1.setup_keys(n, prfAlgo, masterSecret, serverRandom, clientRandom, hashSize, kML, iVS)
  end

  defp calc_master_secret({3, _}, prfAlgo, premasterSecret, clientRandom, serverRandom) do
    :tls_v1.master_secret(prfAlgo, premasterSecret, clientRandom, serverRandom)
  end

  defp hello_pending_connection_states(
         _RecordCB,
         role,
         version,
         cipherSuite,
         random,
         compression,
         connectionStates
       ) do
    readState =
      :ssl_record.pending_connection_state(
        connectionStates,
        :read
      )

    writeState =
      :ssl_record.pending_connection_state(
        connectionStates,
        :write
      )

    newReadSecParams =
      hello_security_parameters(role, version, readState, cipherSuite, random, compression)

    newWriteSecParams =
      hello_security_parameters(role, version, writeState, cipherSuite, random, compression)

    :ssl_record.set_security_params(newReadSecParams, newWriteSecParams, connectionStates)
  end

  defp hello_security_parameters(
         :client,
         version,
         %{:security_parameters => secParams},
         cipherSuite,
         random,
         compression
       ) do
    newSecParams = :ssl_cipher.security_parameters(version, cipherSuite, secParams)

    r_security_parameters(newSecParams,
      server_random: random,
      compression_algorithm: compression
    )
  end

  defp hello_security_parameters(
         :server,
         version,
         %{:security_parameters => secParams},
         cipherSuite,
         random,
         compression
       ) do
    newSecParams = :ssl_cipher.security_parameters(version, cipherSuite, secParams)

    r_security_parameters(newSecParams,
      client_random: random,
      compression_algorithm: compression
    )
  end

  defp select_compression(_CompressionMetodes) do
    0
  end

  defp do_select_version(_, clientVersion, []) do
    clientVersion
  end

  defp do_select_version(recordCB, clientVersion, [version | versions]) do
    case recordCB.is_higher(version, clientVersion) do
      true ->
        do_select_version(recordCB, clientVersion, versions)

      false ->
        do_select_version(recordCB, clientVersion, versions, version)
    end
  end

  defp do_select_version(_, _, [], goodVersion) do
    goodVersion
  end

  defp do_select_version(recordCB, clientVersion, [version | versions], goodVersion) do
    betterVersion =
      case recordCB.is_higher(
             version,
             clientVersion
           ) do
        true ->
          goodVersion

        false ->
          case recordCB.is_higher(version, goodVersion) do
            true ->
              version

            false ->
              goodVersion
          end
      end

    do_select_version(recordCB, clientVersion, versions, betterVersion)
  end

  defp encode_server_key(r_server_dh_params(dh_p: p, dh_g: g, dh_y: y)) do
    pLen = byte_size(p)
    gLen = byte_size(g)
    yLen = byte_size(y)

    <<pLen::size(16)-unsigned-big-integer, p::binary, gLen::size(16)-unsigned-big-integer,
      g::binary, yLen::size(16)-unsigned-big-integer, y::binary>>
  end

  defp encode_server_key(
         r_server_ecdh_params(
           curve: {:namedCurve, eCCurve},
           public: eCPubKey
         )
       ) do
    kLen = :erlang.size(eCPubKey)

    <<3::size(8)-unsigned-big-integer,
      :tls_v1.oid_to_enum(eCCurve)::size(16)-unsigned-big-integer,
      kLen::size(8)-unsigned-big-integer, eCPubKey::binary>>
  end

  defp encode_server_key(r_server_psk_params(hint: pskIdentityHint)) do
    len = byte_size(pskIdentityHint)
    <<len::size(16)-unsigned-big-integer, pskIdentityHint::binary>>
  end

  defp encode_server_key(params = r_server_dhe_psk_params(hint: :undefined)) do
    encode_server_key(r_server_dhe_psk_params(params, hint: <<>>))
  end

  defp encode_server_key(
         r_server_dhe_psk_params(
           hint: pskIdentityHint,
           dh_params: r_server_dh_params(dh_p: p, dh_g: g, dh_y: y)
         )
       ) do
    len = byte_size(pskIdentityHint)
    pLen = byte_size(p)
    gLen = byte_size(g)
    yLen = byte_size(y)

    <<len::size(16)-unsigned-big-integer, pskIdentityHint::binary,
      pLen::size(16)-unsigned-big-integer, p::binary, gLen::size(16)-unsigned-big-integer,
      g::binary, yLen::size(16)-unsigned-big-integer, y::binary>>
  end

  defp encode_server_key(params = r_server_ecdhe_psk_params(hint: :undefined)) do
    encode_server_key(r_server_ecdhe_psk_params(params, hint: <<>>))
  end

  defp encode_server_key(
         r_server_ecdhe_psk_params(
           hint: pskIdentityHint,
           dh_params:
             r_server_ecdh_params(
               curve: {:namedCurve, eCCurve},
               public: eCPubKey
             )
         )
       ) do
    len = byte_size(pskIdentityHint)
    kLen = :erlang.size(eCPubKey)

    <<len::size(16)-unsigned-big-integer, pskIdentityHint::binary,
      3::size(8)-unsigned-big-integer,
      :tls_v1.oid_to_enum(eCCurve)::size(16)-unsigned-big-integer,
      kLen::size(8)-unsigned-big-integer, eCPubKey::binary>>
  end

  defp encode_server_key(r_server_srp_params(srp_n: n, srp_g: g, srp_s: s, srp_b: b)) do
    nLen = byte_size(n)
    gLen = byte_size(g)
    sLen = byte_size(s)
    bLen = byte_size(b)

    <<nLen::size(16)-unsigned-big-integer, n::binary, gLen::size(16)-unsigned-big-integer,
      g::binary, sLen::size(8)-unsigned-big-integer, s::binary,
      bLen::size(16)-unsigned-big-integer, b::binary>>
  end

  defp encode_client_key(r_encrypted_premaster_secret(premaster_secret: pKEPMS), {3, 0}) do
    pKEPMS
  end

  defp encode_client_key(r_encrypted_premaster_secret(premaster_secret: pKEPMS), _) do
    pKEPMSLen = byte_size(pKEPMS)
    <<pKEPMSLen::size(16)-unsigned-big-integer, pKEPMS::binary>>
  end

  defp encode_client_key(r_client_diffie_hellman_public(dh_public: dHPublic), _) do
    len = byte_size(dHPublic)
    <<len::size(16)-unsigned-big-integer, dHPublic::binary>>
  end

  defp encode_client_key(r_client_ec_diffie_hellman_public(dh_public: dHPublic), _) do
    len = byte_size(dHPublic)
    <<len::size(8)-unsigned-big-integer, dHPublic::binary>>
  end

  defp encode_client_key(r_client_psk_identity(identity: :undefined), _) do
    id = "psk_identity"
    len = byte_size(id)
    <<len::size(16)-unsigned-big-integer, id::binary>>
  end

  defp encode_client_key(r_client_psk_identity(identity: id), _) do
    len = byte_size(id)
    <<len::size(16)-unsigned-big-integer, id::binary>>
  end

  defp encode_client_key(identity = r_client_dhe_psk_identity(identity: :undefined), version) do
    encode_client_key(r_client_dhe_psk_identity(identity, identity: "psk_identity"), version)
  end

  defp encode_client_key(r_client_dhe_psk_identity(identity: id, dh_public: dHPublic), _) do
    len = byte_size(id)
    dHLen = byte_size(dHPublic)

    <<len::size(16)-unsigned-big-integer, id::binary, dHLen::size(16)-unsigned-big-integer,
      dHPublic::binary>>
  end

  defp encode_client_key(identity = r_client_ecdhe_psk_identity(identity: :undefined), version) do
    encode_client_key(r_client_ecdhe_psk_identity(identity, identity: "psk_identity"), version)
  end

  defp encode_client_key(r_client_ecdhe_psk_identity(identity: id, dh_public: dHPublic), _) do
    len = byte_size(id)
    dHLen = byte_size(dHPublic)

    <<len::size(16)-unsigned-big-integer, id::binary, dHLen::size(8)-unsigned-big-integer,
      dHPublic::binary>>
  end

  defp encode_client_key(identity = r_client_rsa_psk_identity(identity: :undefined), version) do
    encode_client_key(r_client_rsa_psk_identity(identity, identity: "psk_identity"), version)
  end

  defp encode_client_key(
         r_client_rsa_psk_identity(identity: id, exchange_keys: exchangeKeys),
         version
       ) do
    encPMS = encode_client_key(exchangeKeys, version)
    len = byte_size(id)
    <<len::size(16)-unsigned-big-integer, id::binary, encPMS::binary>>
  end

  defp encode_client_key(r_client_srp_public(srp_a: a), _) do
    len = byte_size(a)
    <<len::size(16)-unsigned-big-integer, a::binary>>
  end

  defp enc_sign({_, :anon}, _Sign, _Version) do
    <<>>
  end

  defp enc_sign({hashAlg, signAlg}, signature, _Version = {major, minor})
       when major == 3 and minor >= 3 do
    signLen = byte_size(signature)
    hashSign = enc_hashsign(hashAlg, signAlg)
    <<hashSign::binary, signLen::size(16)-unsigned-big-integer, signature::binary>>
  end

  defp enc_sign(_HashSign, sign, _Version) do
    signLen = byte_size(sign)
    <<signLen::size(16)-unsigned-big-integer, sign::binary>>
  end

  defp enc_hashsign(hashAlgo, signAlgo) do
    hash = :ssl_cipher.hash_algorithm(hashAlgo)
    sign = :ssl_cipher.sign_algorithm(signAlgo)
    <<hash::size(8)-unsigned-big-integer, sign::size(8)-unsigned-big-integer>>
  end

  defp encode_protocol(protocol, acc) do
    len = byte_size(protocol)
    <<acc::binary, len::size(8)-unsigned-big-integer, protocol::binary>>
  end

  defp enc_server_key_exchange(
         version,
         params,
         {hashAlgo, signAlgo},
         clientRandom,
         serverRandom,
         privateKey
       ) do
    encParams = encode_server_key(params)

    case hashAlgo do
      :null ->
        r_server_key_params(
          params: params,
          params_bin: encParams,
          hashsign: {:null, :anon},
          signature: <<>>
        )

      _ ->
        hash =
          server_key_exchange_hash(
            hashAlgo,
            <<clientRandom::binary, serverRandom::binary, encParams::binary>>
          )

        signature = digitally_signed(version, hash, hashAlgo, privateKey, signAlgo)

        r_server_key_params(
          params: params,
          params_bin: encParams,
          hashsign: {hashAlgo, signAlgo},
          signature: signature
        )
    end
  end

  defp encode_alpn(_, true) do
    :undefined
  end

  defp encode_alpn(:undefined, _) do
    :undefined
  end

  defp encode_alpn(protocols, _) do
    r_alpn(extension_data: :lists.foldl(&encode_protocol/2, <<>>, protocols))
  end

  defp encode_versions(versions) do
    encode_versions(:lists.reverse(versions), <<>>)
  end

  defp encode_versions([], acc) do
    acc
  end

  defp encode_versions([{m, n} | t], acc) do
    encode_versions(
      t,
      <<m::size(8)-unsigned-big-integer, n::size(8)-unsigned-big-integer, acc::binary>>
    )
  end

  defp encode_client_shares(clientShares) do
    encode_client_shares(clientShares, <<>>)
  end

  defp encode_client_shares([], acc) do
    acc
  end

  defp encode_client_shares([keyShareEntry0 | t], acc) do
    keyShareEntry = encode_key_share_entry(keyShareEntry0)

    encode_client_shares(
      t,
      <<acc::binary, keyShareEntry::binary>>
    )
  end

  defp encode_key_share_entry(r_key_share_entry(group: group, key_exchange: keyExchange)) do
    len = byte_size(keyExchange)

    <<:tls_v1.group_to_enum(group)::size(16)-unsigned-big-integer,
      len::size(16)-unsigned-big-integer, keyExchange::binary>>
  end

  defp encode_psk_key_exchange_modes(kEModes) do
    encode_psk_key_exchange_modes(
      :lists.reverse(kEModes),
      <<>>
    )
  end

  defp encode_psk_key_exchange_modes([], acc) do
    acc
  end

  defp encode_psk_key_exchange_modes([:psk_ke | t], acc) do
    encode_psk_key_exchange_modes(
      t,
      <<0::size(8)-unsigned-big-integer, acc::binary>>
    )
  end

  defp encode_psk_key_exchange_modes([:psk_dhe_ke | t], acc) do
    encode_psk_key_exchange_modes(
      t,
      <<1::size(8)-unsigned-big-integer, acc::binary>>
    )
  end

  defp encode_psk_identities(identities) do
    encode_psk_identities(identities, <<>>)
  end

  defp encode_psk_identities([], acc) do
    len = byte_size(acc)
    <<len::size(16)-unsigned-big-integer, acc::binary>>
  end

  defp encode_psk_identities(
         [
           r_psk_identity(
             identity: identity,
             obfuscated_ticket_age: age
           )
           | t
         ],
         acc
       ) do
    idLen = byte_size(identity)

    encode_psk_identities(
      t,
      <<acc::binary, idLen::size(16)-unsigned-big-integer, identity::binary,
        age::size(32)-unsigned-big-integer>>
    )
  end

  defp encode_psk_binders(binders) do
    encode_psk_binders(binders, <<>>)
  end

  defp encode_psk_binders([], acc) do
    len = byte_size(acc)
    <<len::size(16)-unsigned-big-integer, acc::binary>>
  end

  defp encode_psk_binders([binder | t], acc) do
    len = byte_size(binder)

    encode_psk_binders(
      t,
      <<acc::binary, len::size(8)-unsigned-big-integer, binder::binary>>
    )
  end

  defp hello_extensions_list(helloExtensions) do
    for {_, ext} <- :maps.to_list(helloExtensions),
        ext !== :undefined do
      ext
    end
  end

  defp dec_server_key(
         <<pLen::size(16)-unsigned-big-integer, p::size(pLen)-binary,
           gLen::size(16)-unsigned-big-integer, g::size(gLen)-binary,
           yLen::size(16)-unsigned-big-integer, y::size(yLen)-binary, _::binary>> = keyStruct,
         1,
         version
       ) do
    params = r_server_dh_params(dh_p: p, dh_g: g, dh_y: y)

    {binMsg, hashSign, signature} =
      dec_server_key_params(pLen + gLen + yLen + 6, keyStruct, version)

    r_server_key_params(
      params: params,
      params_bin: binMsg,
      hashsign: hashSign,
      signature: signature
    )
  end

  defp dec_server_key(
         <<3::size(8)-unsigned-big-integer, curveID::size(16)-unsigned-big-integer,
           pointLen::size(8)-unsigned-big-integer, eCPoint::size(pointLen)-binary,
           _::binary>> = keyStruct,
         6,
         version
       ) do
    params =
      r_server_ecdh_params(
        curve: {:namedCurve, :tls_v1.enum_to_oid(curveID)},
        public: eCPoint
      )

    {binMsg, hashSign, signature} = dec_server_key_params(pointLen + 4, keyStruct, version)

    r_server_key_params(
      params: params,
      params_bin: binMsg,
      hashsign: hashSign,
      signature: signature
    )
  end

  defp dec_server_key(
         <<len::size(16)-unsigned-big-integer, pskIdentityHint::size(len)-binary, _::binary>> =
           keyStruct,
         keyExchange,
         version
       )
       when keyExchange == 2 or keyExchange == 4 do
    params = r_server_psk_params(hint: pskIdentityHint)
    {binMsg, hashSign, signature} = dec_server_key_params(len + 2, keyStruct, version)

    r_server_key_params(
      params: params,
      params_bin: binMsg,
      hashsign: hashSign,
      signature: signature
    )
  end

  defp dec_server_key(
         <<len::size(16)-unsigned-big-integer, identityHint::size(len)-binary,
           pLen::size(16)-unsigned-big-integer, p::size(pLen)-binary,
           gLen::size(16)-unsigned-big-integer, g::size(gLen)-binary,
           yLen::size(16)-unsigned-big-integer, y::size(yLen)-binary, _::binary>> = keyStruct,
         3,
         version
       ) do
    dHParams = r_server_dh_params(dh_p: p, dh_g: g, dh_y: y)
    params = r_server_dhe_psk_params(hint: identityHint, dh_params: dHParams)

    {binMsg, hashSign, signature} =
      dec_server_key_params(len + pLen + gLen + yLen + 8, keyStruct, version)

    r_server_key_params(
      params: params,
      params_bin: binMsg,
      hashsign: hashSign,
      signature: signature
    )
  end

  defp dec_server_key(
         <<len::size(16)-unsigned-big-integer, identityHint::size(len)-binary,
           3::size(8)-unsigned-big-integer, curveID::size(16)-unsigned-big-integer,
           pointLen::size(8)-unsigned-big-integer, eCPoint::size(pointLen)-binary,
           _::binary>> = keyStruct,
         7,
         version
       ) do
    dHParams =
      r_server_ecdh_params(
        curve: {:namedCurve, :tls_v1.enum_to_oid(curveID)},
        public: eCPoint
      )

    params = r_server_ecdhe_psk_params(hint: identityHint, dh_params: dHParams)

    {binMsg, hashSign, signature} =
      dec_server_key_params(len + 2 + pointLen + 4, keyStruct, version)

    r_server_key_params(
      params: params,
      params_bin: binMsg,
      hashsign: hashSign,
      signature: signature
    )
  end

  defp dec_server_key(
         <<nLen::size(16)-unsigned-big-integer, n::size(nLen)-binary,
           gLen::size(16)-unsigned-big-integer, g::size(gLen)-binary,
           sLen::size(8)-unsigned-big-integer, s::size(sLen)-binary,
           bLen::size(16)-unsigned-big-integer, b::size(bLen)-binary, _::binary>> = keyStruct,
         5,
         version
       ) do
    params = r_server_srp_params(srp_n: n, srp_g: g, srp_s: s, srp_b: b)

    {binMsg, hashSign, signature} =
      dec_server_key_params(nLen + gLen + sLen + bLen + 7, keyStruct, version)

    r_server_key_params(
      params: params,
      params_bin: binMsg,
      hashsign: hashSign,
      signature: signature
    )
  end

  defp dec_server_key(_, keyExchange, _) do
    throw(
      r_alert(
        level: 2,
        description: 40,
        where: %{
          :mfa => {:ssl_handshake, :dec_server_key, 3},
          :line => 2541,
          :file => 'otp/lib/ssl/src/ssl_handshake.erl'
        },
        reason: {:unknown_or_malformed_key_exchange, keyExchange}
      )
    )
  end

  defp dec_client_key(pKEPMS, 0, {3, 0}) do
    r_encrypted_premaster_secret(premaster_secret: pKEPMS)
  end

  defp dec_client_key(<<_::size(16)-unsigned-big-integer, pKEPMS::binary>>, 0, _) do
    r_encrypted_premaster_secret(premaster_secret: pKEPMS)
  end

  defp dec_client_key(<<>>, 1, _) do
    throw(
      r_alert(
        level: 2,
        description: 43,
        where: %{
          :mfa => {:ssl_handshake, :dec_client_key, 3},
          :line => 2548,
          :file => 'otp/lib/ssl/src/ssl_handshake.erl'
        },
        reason: :empty_dh_public
      )
    )
  end

  defp dec_client_key(
         <<dH_YLen::size(16)-unsigned-big-integer, dH_Y::size(dH_YLen)-binary>>,
         1,
         _
       ) do
    r_client_diffie_hellman_public(dh_public: dH_Y)
  end

  defp dec_client_key(<<>>, 6, _) do
    throw(
      r_alert(
        level: 2,
        description: 43,
        where: %{
          :mfa => {:ssl_handshake, :dec_client_key, 3},
          :line => 2553,
          :file => 'otp/lib/ssl/src/ssl_handshake.erl'
        },
        reason: :empty_dh_public
      )
    )
  end

  defp dec_client_key(<<dH_YLen::size(8)-unsigned-big-integer, dH_Y::size(dH_YLen)-binary>>, 6, _) do
    r_client_ec_diffie_hellman_public(dh_public: dH_Y)
  end

  defp dec_client_key(<<len::size(16)-unsigned-big-integer, id::size(len)-binary>>, 2, _) do
    r_client_psk_identity(identity: id)
  end

  defp dec_client_key(
         <<len::size(16)-unsigned-big-integer, id::size(len)-binary,
           dH_YLen::size(16)-unsigned-big-integer, dH_Y::size(dH_YLen)-binary>>,
         3,
         _
       ) do
    r_client_dhe_psk_identity(identity: id, dh_public: dH_Y)
  end

  defp dec_client_key(
         <<len::size(16)-unsigned-big-integer, id::size(len)-binary,
           dH_YLen::size(8)-unsigned-big-integer, dH_Y::size(dH_YLen)-binary>>,
         7,
         _
       ) do
    r_client_ecdhe_psk_identity(identity: id, dh_public: dH_Y)
  end

  defp dec_client_key(
         <<len::size(16)-unsigned-big-integer, id::size(len)-binary, pKEPMS::binary>>,
         4,
         {3, 0}
       ) do
    r_client_rsa_psk_identity(
      identity: id,
      exchange_keys: r_encrypted_premaster_secret(premaster_secret: pKEPMS)
    )
  end

  defp dec_client_key(
         <<len::size(16)-unsigned-big-integer, id::size(len)-binary,
           _::size(16)-unsigned-big-integer, pKEPMS::binary>>,
         4,
         _
       ) do
    r_client_rsa_psk_identity(
      identity: id,
      exchange_keys: r_encrypted_premaster_secret(premaster_secret: pKEPMS)
    )
  end

  defp dec_client_key(<<aLen::size(16)-unsigned-big-integer, a::size(aLen)-binary>>, 5, _) do
    r_client_srp_public(srp_a: a)
  end

  defp dec_server_key_params(len, keys, version) do
    <<params::size(len)-bytes, signature::binary>> = keys
    dec_server_key_signature(params, signature, version)
  end

  defp dec_server_key_signature(
         params,
         <<8::size(8)-unsigned-big-integer, signAlgo::size(8)-unsigned-big-integer,
           0::size(16)-unsigned-big-integer>>,
         {major, minor}
       )
       when major == 3 and minor >= 3 do
    <<scheme0::size(16)-unsigned-big-integer>> =
      <<8::size(8)-unsigned-big-integer, signAlgo::size(8)-unsigned-big-integer>>

    scheme = :ssl_cipher.signature_scheme(scheme0)
    {hash, sign, _} = :ssl_cipher.scheme_to_components(scheme)
    {params, {hash, sign}, <<>>}
  end

  defp dec_server_key_signature(
         params,
         <<8::size(8)-unsigned-big-integer, signAlgo::size(8)-unsigned-big-integer,
           len::size(16)-unsigned-big-integer, signature::size(len)-binary>>,
         {major, minor}
       )
       when major == 3 and minor >= 3 do
    <<scheme0::size(16)-unsigned-big-integer>> =
      <<8::size(8)-unsigned-big-integer, signAlgo::size(8)-unsigned-big-integer>>

    scheme = :ssl_cipher.signature_scheme(scheme0)
    {hash, sign, _} = :ssl_cipher.scheme_to_components(scheme)
    {params, {hash, sign}, signature}
  end

  defp dec_server_key_signature(
         params,
         <<hashAlgo::size(8)-unsigned-big-integer, signAlgo::size(8)-unsigned-big-integer,
           0::size(16)-unsigned-big-integer>>,
         {major, minor}
       )
       when major == 3 and minor >= 3 do
    hashSign = {:ssl_cipher.hash_algorithm(hashAlgo), :ssl_cipher.sign_algorithm(signAlgo)}
    {params, hashSign, <<>>}
  end

  defp dec_server_key_signature(
         params,
         <<hashAlgo::size(8)-unsigned-big-integer, signAlgo::size(8)-unsigned-big-integer,
           len::size(16)-unsigned-big-integer, signature::size(len)-binary>>,
         {major, minor}
       )
       when major == 3 and minor >= 3 do
    hashSign = {:ssl_cipher.hash_algorithm(hashAlgo), :ssl_cipher.sign_algorithm(signAlgo)}
    {params, hashSign, signature}
  end

  defp dec_server_key_signature(params, <<>>, _) do
    {params, {:null, :anon}, <<>>}
  end

  defp dec_server_key_signature(params, <<0::size(16)-unsigned-big-integer>>, _) do
    {params, {:null, :anon}, <<>>}
  end

  defp dec_server_key_signature(
         params,
         <<len::size(16)-unsigned-big-integer, signature::size(len)-binary>>,
         _
       ) do
    {params, :undefined, signature}
  end

  defp dec_server_key_signature(_, _, _) do
    throw(
      r_alert(
        level: 2,
        description: 40,
        where: %{
          :mfa => {:ssl_handshake, :dec_server_key_signature, 3},
          :line => 2615,
          :file => 'otp/lib/ssl/src/ssl_handshake.erl'
        },
        reason: :failed_to_decrypt_server_key_sign
      )
    )
  end

  defp process_supported_versions_extension(<<>>, localVersion, legacyVersion)
       when legacyVersion <= localVersion do
    legacyVersion
  end

  defp process_supported_versions_extension(<<>>, localVersion, _LegacyVersion) do
    localVersion
  end

  defp process_supported_versions_extension(
         <<43::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           extData::size(len)-binary, _Rest::binary>>,
         localVersion,
         _LegacyVersion
       )
       when len > 2 do
    <<_::size(8)-unsigned-big-integer, versions0::binary>> = extData
    [highest | _] = decode_versions(versions0)

    cond do
      highest <= localVersion ->
        highest

      true ->
        localVersion
    end
  end

  defp process_supported_versions_extension(
         <<43::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           major::size(8)-unsigned-big-integer, minor::size(8)-unsigned-big-integer,
           _Rest::binary>>,
         localVersion,
         _LegacyVersion
       )
       when len === 2 do
    selectedVersion = {major, minor}

    cond do
      selectedVersion <= localVersion ->
        selectedVersion

      true ->
        localVersion
    end
  end

  defp process_supported_versions_extension(
         <<_::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           _ExtData::size(len)-binary, rest::binary>>,
         localVersion,
         legacyVersion
       ) do
    process_supported_versions_extension(rest, localVersion, legacyVersion)
  end

  defp process_supported_versions_extension(_, localVersion, legacyVersion)
       when legacyVersion <= localVersion do
    legacyVersion
  end

  defp process_supported_versions_extension(_, localVersion, _) do
    localVersion
  end

  defp decode_extensions(<<>>, _Version, _MessageType, acc) do
    acc
  end

  defp decode_extensions(
         <<16::size(16)-unsigned-big-integer, extLen::size(16)-unsigned-big-integer,
           len::size(16)-unsigned-big-integer, extensionData::size(len)-binary, rest::binary>>,
         version,
         messageType,
         acc
       )
       when len + 2 === extLen do
    aLPN = r_alpn(extension_data: extensionData)
    decode_extensions(rest, version, messageType, %{acc | :alpn => aLPN})
  end

  defp decode_extensions(
         <<13172::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           extensionData::size(len)-binary, rest::binary>>,
         version,
         messageType,
         acc
       ) do
    nextP = r_next_protocol_negotiation(extension_data: extensionData)
    decode_extensions(rest, version, messageType, %{acc | :next_protocol_negotiation => nextP})
  end

  defp decode_extensions(
         <<65281::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           info::size(len)-binary, rest::binary>>,
         version,
         messageType,
         acc
       ) do
    renegotiateInfo =
      case len do
        1 ->
          info

        _ ->
          verifyLen = len - 1
          <<^verifyLen::size(8)-unsigned-big-integer, verifyInfo::binary>> = info
          verifyInfo
      end

    decode_extensions(rest, version, messageType, %{
      acc
      | :renegotiation_info => r_renegotiation_info(renegotiated_connection: renegotiateInfo)
    })
  end

  defp decode_extensions(
         <<12::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           sRPLen::size(8)-unsigned-big-integer, sRP::size(sRPLen)-binary, rest::binary>>,
         version,
         messageType,
         acc
       )
       when len == sRPLen + 1 do
    decode_extensions(rest, version, messageType, %{acc | :srp => r_srp(username: sRP)})
  end

  defp decode_extensions(
         <<13::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           extData::size(len)-binary, rest::binary>>,
         version,
         messageType,
         acc
       )
       when version < {3, 4} do
    signAlgoListLen = len - 2
    <<^signAlgoListLen::size(16)-unsigned-big-integer, signAlgoList::binary>> = extData

    hashSignAlgos =
      for <<(<<hash::size(8)-unsigned-big-integer, sign::size(8)-unsigned-big-integer>> <-
               signAlgoList)>> do
        {:ssl_cipher.hash_algorithm(hash), :ssl_cipher.sign_algorithm(sign)}
      end

    decode_extensions(rest, version, messageType, %{
      acc
      | :signature_algs => r_hash_sign_algos(hash_sign_algos: hashSignAlgos)
    })
  end

  defp decode_extensions(
         <<13::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           extData::size(len)-binary, rest::binary>>,
         version,
         messageType,
         acc
       )
       when version === {3, 4} do
    signSchemeListLen = len - 2
    <<^signSchemeListLen::size(16)-unsigned-big-integer, signSchemeList::binary>> = extData

    fun = fn elem ->
      case :ssl_cipher.signature_scheme(elem) do
        :unassigned ->
          false

        value ->
          {true, value}
      end
    end

    signSchemes =
      :lists.filtermap(
        fun,
        for <<(<<signScheme::size(16)-unsigned-big-integer>> <- signSchemeList)>> do
          signScheme
        end
      )

    decode_extensions(rest, version, messageType, %{
      acc
      | :signature_algs => r_signature_algorithms(signature_scheme_list: signSchemes)
    })
  end

  defp decode_extensions(
         <<50::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           extData::size(len)-binary, rest::binary>>,
         version,
         messageType,
         acc
       ) do
    signSchemeListLen = len - 2
    <<^signSchemeListLen::size(16)-unsigned-big-integer, signSchemeList::binary>> = extData

    fun = fn elem ->
      case :ssl_cipher.signature_scheme(elem) do
        :unassigned ->
          false

        value ->
          {true, value}
      end
    end

    signSchemes =
      :lists.filtermap(
        fun,
        for <<(<<signScheme::size(16)-unsigned-big-integer>> <- signSchemeList)>> do
          signScheme
        end
      )

    decode_extensions(rest, version, messageType, %{
      acc
      | :signature_algs_cert => r_signature_algorithms_cert(signature_scheme_list: signSchemes)
    })
  end

  defp decode_extensions(
         <<10::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           extData::size(len)-binary, rest::binary>>,
         version,
         messageType,
         acc
       )
       when version < {3, 4} do
    <<_::size(16)-unsigned-big-integer, ellipticCurveList::binary>> = extData

    pick = fn enum ->
      case :tls_v1.enum_to_oid(enum) do
        :undefined ->
          false

        oid ->
          {true, oid}
      end
    end

    ellipticCurves =
      :lists.filtermap(
        pick,
        for <<(<<eCC::size(16)>> <- ellipticCurveList)>> do
          eCC
        end
      )

    decode_extensions(rest, version, messageType, %{
      acc
      | :elliptic_curves => r_elliptic_curves(elliptic_curve_list: ellipticCurves)
    })
  end

  defp decode_extensions(
         <<10::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           extData::size(len)-binary, rest::binary>>,
         version,
         messageType,
         acc
       )
       when version === {3, 4} do
    <<_::size(16)-unsigned-big-integer, groupList::binary>> = extData

    pick = fn enum ->
      case :tls_v1.enum_to_group(enum) do
        :undefined ->
          false

        group ->
          {true, group}
      end
    end

    supportedGroups =
      :lists.filtermap(
        pick,
        for <<(<<group::size(16)>> <- groupList)>> do
          group
        end
      )

    decode_extensions(rest, version, messageType, %{
      acc
      | :elliptic_curves => r_supported_groups(supported_groups: supportedGroups)
    })
  end

  defp decode_extensions(
         <<11::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           extData::size(len)-binary, rest::binary>>,
         version,
         messageType,
         acc
       ) do
    <<_::size(8)-unsigned-big-integer, eCPointFormatList::binary>> = extData
    eCPointFormats = :erlang.binary_to_list(eCPointFormatList)

    decode_extensions(rest, version, messageType, %{
      acc
      | :ec_point_formats => r_ec_point_formats(ec_point_format_list: eCPointFormats)
    })
  end

  defp decode_extensions(
         <<0::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer, rest::binary>>,
         version,
         messageType,
         acc
       )
       when len == 0 do
    decode_extensions(rest, version, messageType, %{acc | :sni => r_sni(hostname: '')})
  end

  defp decode_extensions(
         <<0::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           extData::size(len)-binary, rest::binary>>,
         version,
         messageType,
         acc
       ) do
    <<_::size(16)-unsigned-big-integer, nameList::binary>> = extData
    decode_extensions(rest, version, messageType, %{acc | :sni => dec_sni(nameList)})
  end

  defp decode_extensions(
         <<1::size(16)-unsigned-big-integer, 1::size(16)-unsigned-big-integer,
           maxFragEnum::size(8)-unsigned-big-integer, rest::binary>>,
         version,
         messageType,
         acc
       ) do
    decode_extensions(rest, version, messageType, %{
      acc
      | :max_frag_enum => r_max_frag_enum(enum: maxFragEnum)
    })
  end

  defp decode_extensions(
         <<43::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           extData::size(len)-binary, rest::binary>>,
         version,
         messageType,
         acc
       )
       when len > 2 do
    <<_::size(8)-unsigned-big-integer, versions::binary>> = extData

    decode_extensions(rest, version, messageType, %{
      acc
      | :client_hello_versions => r_client_hello_versions(versions: decode_versions(versions))
    })
  end

  defp decode_extensions(
         <<43::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           selectedVersion::size(16)-unsigned-big-integer, rest::binary>>,
         version,
         messageType,
         acc
       )
       when len === 2 and selectedVersion === 772 do
    decode_extensions(rest, version, messageType, %{
      acc
      | :server_hello_selected_version =>
          r_server_hello_selected_version(selected_version: {3, 4})
    })
  end

  defp decode_extensions(
         <<51::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           extData::size(len)-binary, rest::binary>>,
         version,
         messageType = :client_hello,
         acc
       ) do
    <<_::size(16)-unsigned-big-integer, clientShares::binary>> = extData

    decode_extensions(rest, version, messageType, %{
      acc
      | :key_share => r_key_share_client_hello(client_shares: decode_client_shares(clientShares))
    })
  end

  defp decode_extensions(
         <<51::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           extData::size(len)-binary, rest::binary>>,
         version,
         messageType = :server_hello,
         acc
       ) do
    <<group::size(16)-unsigned-big-integer, keyLen::size(16)-unsigned-big-integer,
      keyExchange::size(keyLen)-binary>> = extData

    decode_extensions(rest, version, messageType, %{
      acc
      | :key_share =>
          r_key_share_server_hello(
            server_share:
              r_key_share_entry(
                group: :tls_v1.enum_to_group(group),
                key_exchange: keyExchange
              )
          )
    })
  end

  defp decode_extensions(
         <<51::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           extData::size(len)-binary, rest::binary>>,
         version,
         messageType = :hello_retry_request,
         acc
       ) do
    <<group::size(16)-unsigned-big-integer>> = extData

    decode_extensions(rest, version, messageType, %{
      acc
      | :key_share =>
          r_key_share_hello_retry_request(selected_group: :tls_v1.enum_to_group(group))
    })
  end

  defp decode_extensions(
         <<45::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           extData::size(len)-binary, rest::binary>>,
         version,
         messageType,
         acc
       ) do
    <<pLen::size(8)-unsigned-big-integer, kEModes::size(pLen)-binary>> = extData

    decode_extensions(rest, version, messageType, %{
      acc
      | :psk_key_exchange_modes =>
          r_psk_key_exchange_modes(ke_modes: decode_psk_key_exchange_modes(kEModes))
    })
  end

  defp decode_extensions(
         <<41::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           extData::size(len)-binary, rest::binary>>,
         version,
         messageType = :client_hello,
         acc
       ) do
    <<idLen::size(16)-unsigned-big-integer, identities::size(idLen)-binary,
      bLen::size(16)-unsigned-big-integer, binders::size(bLen)-binary>> = extData

    decode_extensions(rest, version, messageType, %{
      acc
      | :pre_shared_key =>
          r_pre_shared_key_client_hello(
            offered_psks:
              r_offered_psks(
                identities: decode_psk_identities(identities),
                binders: decode_psk_binders(binders)
              )
          )
    })
  end

  defp decode_extensions(
         <<41::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           extData::size(len)-binary, rest::binary>>,
         version,
         messageType = :server_hello,
         acc
       ) do
    <<identity::size(16)-unsigned-big-integer>> = extData

    decode_extensions(rest, version, messageType, %{
      acc
      | :pre_shared_key => r_pre_shared_key_server_hello(selected_identity: identity)
    })
  end

  defp decode_extensions(
         <<44::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           cookieLen::size(16)-unsigned-big-integer, cookie::size(cookieLen)-binary,
           rest::binary>>,
         version,
         messageType,
         acc
       )
       when len == cookieLen + 2 do
    decode_extensions(rest, version, messageType, %{acc | :cookie => r_cookie(cookie: cookie)})
  end

  defp decode_extensions(
         <<5::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           _ExtensionData::size(len)-binary, rest::binary>>,
         version,
         messageType = :server_hello,
         acc
       )
       when len === 0 do
    decode_extensions(rest, version, messageType, %{acc | :status_request => :undefined})
  end

  defp decode_extensions(
         <<5::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           certStatus::size(len)-binary, rest::binary>>,
         version,
         messageType,
         acc
       ) do
    case certStatus do
      <<1::size(8)-unsigned-big-integer, oCSPLen::size(24)-unsigned-big-integer,
        aSN1OCSPResponse::size(oCSPLen)-binary>> ->
        decode_extensions(rest, version, messageType, %{
          acc
          | :status_request => r_certificate_status(response: aSN1OCSPResponse)
        })

      _Other ->
        decode_extensions(rest, version, messageType, acc)
    end
  end

  defp decode_extensions(
         <<_::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           _Unknown::size(len)-binary, rest::binary>>,
         version,
         messageType,
         acc
       ) do
    decode_extensions(rest, version, messageType, acc)
  end

  defp decode_extensions(_, _, _, acc) do
    acc
  end

  defp dec_hashsign(
         <<hashAlgo::size(8)-unsigned-big-integer, signAlgo::size(8)-unsigned-big-integer>>
       ) do
    {:ssl_cipher.hash_algorithm(hashAlgo), :ssl_cipher.sign_algorithm(signAlgo)}
  end

  defp dec_sni(
         <<0::size(8)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           hostName::size(len)-binary, _::binary>>
       ) do
    r_sni(hostname: :erlang.binary_to_list(hostName))
  end

  defp dec_sni(
         <<_::size(8)-unsigned-big-integer, len::size(16)-unsigned-big-integer, _::size(len),
           rest::binary>>
       ) do
    dec_sni(rest)
  end

  defp dec_sni(_) do
    :undefined
  end

  def decode_alpn(:undefined) do
    :undefined
  end

  def decode_alpn(r_alpn(extension_data: data)) do
    decode_protocols(data, [])
  end

  defp decode_versions(versions) do
    decode_versions(versions, [])
  end

  defp decode_versions(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp decode_versions(
         <<m::size(8)-unsigned-big-integer, n::size(8)-unsigned-big-integer, rest::binary>>,
         acc
       ) do
    decode_versions(rest, [{m, n} | acc])
  end

  defp decode_client_shares(clientShares) do
    decode_client_shares(clientShares, [])
  end

  defp decode_client_shares(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp decode_client_shares(
         <<group0::size(16)-unsigned-big-integer, len::size(16)-unsigned-big-integer,
           keyExchange::size(len)-binary, rest::binary>>,
         acc
       ) do
    case :tls_v1.enum_to_group(group0) do
      :undefined ->
        decode_client_shares(rest, acc)

      group ->
        decode_client_shares(
          rest,
          [
            r_key_share_entry(group: group, key_exchange: keyExchange)
            | acc
          ]
        )
    end
  end

  defp decode_next_protocols({:next_protocol_negotiation, protocols}) do
    decode_protocols(protocols, [])
  end

  defp decode_protocols(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp decode_protocols(
         <<len::size(8)-unsigned-big-integer, protocol::size(len)-binary, rest::binary>>,
         acc
       ) do
    case len do
      0 ->
        {:error, :invalid_protocols}

      _ ->
        decode_protocols(rest, [protocol | acc])
    end
  end

  defp decode_protocols(_Bytes, _Acc) do
    {:error, :invalid_protocols}
  end

  defp decode_psk_key_exchange_modes(kEModes) do
    decode_psk_key_exchange_modes(kEModes, [])
  end

  defp decode_psk_key_exchange_modes(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp decode_psk_key_exchange_modes(
         <<0::size(8)-unsigned-big-integer, rest::binary>>,
         acc
       ) do
    decode_psk_key_exchange_modes(rest, [:psk_ke | acc])
  end

  defp decode_psk_key_exchange_modes(
         <<1::size(8)-unsigned-big-integer, rest::binary>>,
         acc
       ) do
    decode_psk_key_exchange_modes(rest, [:psk_dhe_ke | acc])
  end

  defp decode_psk_key_exchange_modes(
         <<_::size(8)-unsigned-big-integer, rest::binary>>,
         acc
       ) do
    decode_psk_key_exchange_modes(rest, acc)
  end

  defp decode_psk_identities(identities) do
    decode_psk_identities(identities, [])
  end

  defp decode_psk_identities(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp decode_psk_identities(
         <<len::size(16)-unsigned-big-integer, identity::size(len)-binary,
           age::size(32)-unsigned-big-integer, rest::binary>>,
         acc
       ) do
    decode_psk_identities(
      rest,
      [
        r_psk_identity(identity: identity, obfuscated_ticket_age: age)
        | acc
      ]
    )
  end

  defp decode_psk_binders(binders) do
    decode_psk_binders(binders, [])
  end

  defp decode_psk_binders(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp decode_psk_binders(
         <<len::size(8)-unsigned-big-integer, binder::size(len)-binary, rest::binary>>,
         acc
       ) do
    decode_psk_binders(rest, [binder | acc])
  end

  defp certs_to_list(aSN1Certs) do
    certs_to_list(aSN1Certs, [])
  end

  defp certs_to_list(
         <<certLen::size(24)-unsigned-big-integer, cert::size(certLen)-binary, rest::binary>>,
         acc
       ) do
    certs_to_list(rest, [cert | acc])
  end

  defp certs_to_list(<<>>, acc) do
    :lists.reverse(acc, [])
  end

  defp certs_from_list(aCList) do
    :erlang.list_to_binary(
      for cert <- aCList do
        certLen = byte_size(cert)
        <<certLen::size(24)-unsigned-big-integer, cert::binary>>
      end
    )
  end

  defp from_3bytes(bin3) do
    from_3bytes(bin3, [])
  end

  defp from_3bytes(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp from_3bytes(
         <<n::size(24)-unsigned-big-integer, rest::binary>>,
         acc
       ) do
    from_3bytes(
      rest,
      [<<n::size(16)-unsigned-big-integer>> | acc]
    )
  end

  defp from_2bytes(bin2) do
    from_2bytes(bin2, [])
  end

  defp from_2bytes(<<>>, acc) do
    :lists.reverse(acc)
  end

  defp from_2bytes(
         <<n::size(16)-unsigned-big-integer, rest::binary>>,
         acc
       ) do
    from_2bytes(
      rest,
      [<<n::size(16)-unsigned-big-integer>> | acc]
    )
  end

  defp key_exchange_alg(:rsa) do
    0
  end

  defp key_exchange_alg(alg)
       when alg == :dhe_rsa or alg == :dhe_dss or
              alg == :dh_dss or alg == :dh_rsa or alg == :dh_anon do
    1
  end

  defp key_exchange_alg(alg)
       when alg == :ecdhe_rsa or
              alg == :ecdh_rsa or alg == :ecdhe_ecdsa or
              alg == :ecdh_ecdsa or alg == :ecdh_anon do
    6
  end

  defp key_exchange_alg(:psk) do
    2
  end

  defp key_exchange_alg(:dhe_psk) do
    3
  end

  defp key_exchange_alg(:ecdhe_psk) do
    7
  end

  defp key_exchange_alg(:rsa_psk) do
    4
  end

  defp key_exchange_alg(alg)
       when alg == :srp_rsa or alg == :srp_dss or
              alg == :srp_anon do
    5
  end

  defp key_exchange_alg(_) do
    0
  end

  defp select_cipher_suite(cipherSuites, suites, false) do
    select_cipher_suite(cipherSuites, suites)
  end

  defp select_cipher_suite(cipherSuites, suites, true) do
    select_cipher_suite(suites, cipherSuites)
  end

  defp select_cipher_suite([], _) do
    :no_suite
  end

  defp select_cipher_suite([suite | clientSuites], supportedSuites) do
    case is_member(suite, supportedSuites) do
      true ->
        suite

      false ->
        select_cipher_suite(clientSuites, supportedSuites)
    end
  end

  defp is_member(suite, supportedSuites) do
    :lists.member(suite, supportedSuites)
  end

  defp psk_secret(pSKIdentity, pSKLookup) do
    case handle_psk_identity(pSKIdentity, pSKLookup) do
      {:ok, pSK} when is_binary(pSK) ->
        len = :erlang.byte_size(pSK)

        <<len::size(16)-unsigned-big-integer, 0::size(len * 8),
          len::size(16)-unsigned-big-integer, pSK::binary>>

      r_alert() = alert ->
        alert

      _ ->
        throw(
          r_alert(
            level: 2,
            description: 47,
            where: %{
              :mfa => {:ssl_handshake, :psk_secret, 2},
              :line => 3090,
              :file => 'otp/lib/ssl/src/ssl_handshake.erl'
            }
          )
        )
    end
  end

  defp psk_secret(pSKIdentity, pSKLookup, premasterSecret) do
    case handle_psk_identity(pSKIdentity, pSKLookup) do
      {:ok, pSK} when is_binary(pSK) ->
        len = :erlang.byte_size(premasterSecret)
        pSKLen = :erlang.byte_size(pSK)

        <<len::size(16)-unsigned-big-integer, premasterSecret::binary,
          pSKLen::size(16)-unsigned-big-integer, pSK::binary>>

      r_alert() = alert ->
        alert

      _ ->
        throw(
          r_alert(
            level: 2,
            description: 47,
            where: %{
              :mfa => {:ssl_handshake, :psk_secret, 3},
              :line => 3102,
              :file => 'otp/lib/ssl/src/ssl_handshake.erl'
            }
          )
        )
    end
  end

  defp handle_psk_identity(_PSKIdentity, lookupFun)
       when lookupFun == :undefined do
    :error
  end

  defp handle_psk_identity(pSKIdentity, {fun, userState}) do
    fun.(:psk, pSKIdentity, userState)
  end

  defp filter_hashsigns([], [], _, _, acc) do
    :lists.reverse(acc)
  end

  defp filter_hashsigns(
         [suite | suites],
         [%{:key_exchange => keyExchange} | algos],
         hashSigns,
         version,
         acc
       )
       when keyExchange == :dhe_ecdsa or
              keyExchange == :ecdhe_ecdsa do
    do_filter_hashsigns(:ecdsa, suite, suites, algos, hashSigns, version, acc)
  end

  defp filter_hashsigns(
         [suite | suites],
         [%{:key_exchange => keyExchange} | algos],
         hashSigns,
         version,
         acc
       )
       when keyExchange == :rsa or keyExchange == :dhe_rsa or
              keyExchange == :ecdhe_rsa or keyExchange == :srp_rsa or
              keyExchange == :rsa_psk do
    do_filter_hashsigns(:rsa, suite, suites, algos, hashSigns, version, acc)
  end

  defp filter_hashsigns(
         [suite | suites],
         [%{:key_exchange => keyExchange} | algos],
         hashSigns,
         version,
         acc
       )
       when keyExchange == :dhe_dss or
              keyExchange == :srp_dss do
    do_filter_hashsigns(:dsa, suite, suites, algos, hashSigns, version, acc)
  end

  defp filter_hashsigns(
         [suite | suites],
         [%{:key_exchange => keyExchange} | algos],
         hashSigns,
         verion,
         acc
       )
       when keyExchange == :dh_dss or keyExchange == :dh_rsa or
              keyExchange == :dh_ecdsa or keyExchange == :ecdh_rsa or
              keyExchange == :ecdh_ecdsa do
    filter_hashsigns(suites, algos, hashSigns, verion, [suite | acc])
  end

  defp filter_hashsigns(
         [suite | suites],
         [%{:key_exchange => keyExchange} | algos],
         hashSigns,
         version,
         acc
       )
       when keyExchange == :dh_anon or
              keyExchange == :ecdh_anon or keyExchange == :srp_anon or
              keyExchange == :psk or keyExchange == :dhe_psk or
              keyExchange == :ecdhe_psk do
    filter_hashsigns(suites, algos, hashSigns, version, [suite | acc])
  end

  defp do_filter_hashsigns(signAlgo, suite, suites, algos, hashSigns, version, acc) do
    case :lists.keymember(signAlgo, 2, hashSigns) do
      true ->
        filter_hashsigns(suites, algos, hashSigns, version, [suite | acc])

      false ->
        filter_hashsigns(suites, algos, hashSigns, version, acc)
    end
  end

  defp filter_unavailable_ecc_suites(:no_curve, suites) do
    eCCSuites =
      :ssl_cipher.filter_suites(
        suites,
        %{
          :key_exchange_filters => [
            fn
              :ecdh_ecdsa ->
                true

              :ecdhe_ecdsa ->
                true

              :ecdh_rsa ->
                true

              _ ->
                false
            end
          ],
          :cipher_filters => [],
          :mac_filters => [],
          :prf_filters => []
        }
      )

    suites -- eCCSuites
  end

  defp filter_unavailable_ecc_suites(_, suites) do
    suites
  end

  defp handle_renegotiation_extension(
         role,
         recordCB,
         version,
         info,
         random,
         negotiatedCipherSuite,
         clientCipherSuites,
         compression,
         connectionStates0,
         renegotiation,
         secureRenegotation
       ) do
    {:ok, connectionStates} =
      handle_renegotiation_info(
        version,
        recordCB,
        role,
        info,
        connectionStates0,
        renegotiation,
        secureRenegotation,
        clientCipherSuites
      )

    hello_pending_connection_states(
      recordCB,
      role,
      version,
      negotiatedCipherSuite,
      random,
      compression,
      connectionStates
    )
  end

  defp handle_alpn_extension(_, {:error, reason}) do
    throw(
      r_alert(
        level: 2,
        description: 40,
        where: %{
          :mfa => {:ssl_handshake, :handle_alpn_extension, 2},
          :line => 3188,
          :file => 'otp/lib/ssl/src/ssl_handshake.erl'
        },
        reason: reason
      )
    )
  end

  defp handle_alpn_extension([], _) do
    throw(
      r_alert(
        level: 2,
        description: 120,
        where: %{
          :mfa => {:ssl_handshake, :handle_alpn_extension, 2},
          :line => 3190,
          :file => 'otp/lib/ssl/src/ssl_handshake.erl'
        }
      )
    )
  end

  defp handle_alpn_extension([serverProtocol | tail], clientProtocols) do
    case :lists.member(serverProtocol, clientProtocols) do
      true ->
        serverProtocol

      false ->
        handle_alpn_extension(tail, clientProtocols)
    end
  end

  defp handle_mfl_extension(r_max_frag_enum(enum: enum) = maxFragEnum)
       when enum >= 1 and
              enum <= 4 do
    maxFragEnum
  end

  defp handle_mfl_extension(r_max_frag_enum()) do
    throw(
      r_alert(
        level: 2,
        description: 47,
        where: %{
          :mfa => {:ssl_handshake, :handle_mfl_extension, 1},
          :line => 3200,
          :file => 'otp/lib/ssl/src/ssl_handshake.erl'
        }
      )
    )
  end

  defp handle_mfl_extension(_) do
    :undefined
  end

  defp handle_next_protocol(:undefined, _NextProtocolSelector, _Renegotiating) do
    :undefined
  end

  defp handle_next_protocol(
         r_next_protocol_negotiation() = nextProtocols,
         nextProtocolSelector,
         renegotiating
       ) do
    case next_protocol_extension_allowed(
           nextProtocolSelector,
           renegotiating
         ) do
      true ->
        select_next_protocol(
          decode_next_protocols(nextProtocols),
          nextProtocolSelector
        )

      false ->
        throw(
          r_alert(
            level: 2,
            description: 40,
            where: %{
              :mfa => {:ssl_handshake, :handle_next_protocol, 3},
              :line => 3215,
              :file => 'otp/lib/ssl/src/ssl_handshake.erl'
            },
            reason: :unexpected_next_protocol_extension
          )
        )
    end
  end

  defp handle_next_protocol_extension(nextProtocolNegotiation, renegotiation, sslOpts) do
    case handle_next_protocol_on_server(nextProtocolNegotiation, renegotiation, sslOpts) do
      r_alert() = alert ->
        throw(alert)

      protocolsToAdvertise ->
        protocolsToAdvertise
    end
  end

  defp handle_next_protocol_on_server(:undefined, _Renegotiation, _SslOpts) do
    :undefined
  end

  defp handle_next_protocol_on_server(r_next_protocol_negotiation(extension_data: <<>>), false, %{
         :next_protocols_advertised => protocols
       }) do
    protocols
  end

  defp handle_next_protocol_on_server(_Hello, _Renegotiation, _SSLOpts) do
    r_alert(
      level: 2,
      description: 40,
      where: %{
        :mfa => {:ssl_handshake, :handle_next_protocol_on_server, 3},
        :line => 3235,
        :file => 'otp/lib/ssl/src/ssl_handshake.erl'
      },
      reason: :unexpected_next_protocol_extension
    )
  end

  defp next_protocol_extension_allowed(nextProtocolSelector, renegotiating) do
    nextProtocolSelector !== :undefined and not renegotiating
  end

  defp select_next_protocol({:error, reason}, _NextProtocolSelector) do
    r_alert(
      level: 2,
      description: 40,
      where: %{
        :mfa => {:ssl_handshake, :select_next_protocol, 2},
        :line => 3241,
        :file => 'otp/lib/ssl/src/ssl_handshake.erl'
      },
      reason: reason
    )
  end

  defp select_next_protocol(protocols, nextProtocolSelector) do
    case nextProtocolSelector.(protocols) do
      <<>> ->
        r_alert(
          level: 2,
          description: 40,
          where: %{
            :mfa => {:ssl_handshake, :select_next_protocol, 2},
            :line => 3245,
            :file => 'otp/lib/ssl/src/ssl_handshake.erl'
          },
          reason: :no_next_protocol
        )

      protocol when is_binary(protocol) ->
        protocol
    end
  end

  defp handle_srp_extension(:undefined, session) do
    session
  end

  defp handle_srp_extension(r_srp(username: username), session) do
    r_session(session, srp_username: username)
  end

  defp is_acceptable_hash_sign(_, keyExAlgo, _)
       when keyExAlgo == :psk or
              keyExAlgo == :dhe_psk or
              keyExAlgo == :ecdhe_psk or
              keyExAlgo == :srp_anon or
              keyExAlgo == :dh_anon or
              keyExAlgo == :ecdhe_anon do
    true
  end

  defp is_acceptable_hash_sign(algos, _, supportedHashSigns) do
    is_acceptable_hash_sign(algos, supportedHashSigns)
  end

  defp is_acceptable_hash_sign(algos, supportedHashSigns) do
    :lists.member(algos, supportedHashSigns)
  end

  defp is_acceptable_cert_type(sign, types) do
    :lists.member(
      sign_type(sign),
      :erlang.binary_to_list(types)
    )
  end

  defp is_supported_sign(signAlgo, _, hashSigns, :undefined) do
    :lists.member(signAlgo, hashSigns)
  end

  defp is_supported_sign({hash, sign}, :NULL, _, signatureSchemes) do
    fun = fn scheme, acc ->
      {h0, s0, _} = :ssl_cipher.scheme_to_components(scheme)

      s1 =
        case s0 do
          :rsa_pkcs1 ->
            :rsa

          s ->
            s
        end

      h1 =
        case h0 do
          :sha1 ->
            :sha

          h ->
            h
        end

      acc or (sign === s1 and hash === h1)
    end

    :lists.foldl(fun, false, signatureSchemes)
  end

  defp is_supported_sign({hash, sign}, _Param, _, signatureSchemes) do
    fun = fn scheme, acc ->
      {h0, s0, _} = :ssl_cipher.scheme_to_components(scheme)

      s1 =
        case s0 do
          :rsa_pkcs1 ->
            :rsa

          s ->
            s
        end

      h1 =
        case h0 do
          :sha1 ->
            :sha

          h ->
            h
        end

      acc or (sign === s1 and hash === h1)
    end

    :lists.foldl(fun, false, signatureSchemes)
  end

  defp public_key_algo({1, 2, 840, 113_549, 1, 1, 1}) do
    :rsa
  end

  defp public_key_algo({1, 2, 840, 10045, 2, 1}) do
    :ecdsa
  end

  defp public_key_algo({1, 2, 840, 10040, 4, 1}) do
    :dsa
  end

  defp sign_algo(alg) do
    :public_key.pkix_sign_types(alg)
  end

  defp sign_type(:rsa) do
    1
  end

  defp sign_type(:dsa) do
    2
  end

  defp sign_type(:ecdsa) do
    64
  end

  def server_name(_, _, :server) do
    :undefined
  end

  def server_name(:undefined, host, :client) do
    {:fallback, host}
  end

  def server_name(sNI, _, :client) do
    sNI
  end

  defp client_ecc_extensions(supportedECCs) do
    cryptoSupport =
      :proplists.get_value(
        :public_keys,
        :crypto.supports()
      )

    case :proplists.get_bool(:ecdh, cryptoSupport) do
      true ->
        ecPointFormats = r_ec_point_formats(ec_point_format_list: [0])
        ellipticCurves = supportedECCs
        {ecPointFormats, ellipticCurves}

      _ ->
        {:undefined, :undefined}
    end
  end

  defp server_ecc_extension(_Version, ecPointFormats) do
    cryptoSupport =
      :proplists.get_value(
        :public_keys,
        :crypto.supports()
      )

    case :proplists.get_bool(:ecdh, cryptoSupport) do
      true ->
        handle_ecc_point_fmt_extension(ecPointFormats)

      false ->
        :undefined
    end
  end

  defp handle_ecc_point_fmt_extension(:undefined) do
    :undefined
  end

  defp handle_ecc_point_fmt_extension(_) do
    r_ec_point_formats(ec_point_format_list: [0])
  end

  defp advertises_ec_ciphers([]) do
    false
  end

  defp advertises_ec_ciphers([%{:key_exchange => :ecdh_ecdsa} | _]) do
    true
  end

  defp advertises_ec_ciphers([%{:key_exchange => :ecdhe_ecdsa} | _]) do
    true
  end

  defp advertises_ec_ciphers([%{:key_exchange => :ecdh_rsa} | _]) do
    true
  end

  defp advertises_ec_ciphers([%{:key_exchange => :ecdhe_rsa} | _]) do
    true
  end

  defp advertises_ec_ciphers([%{:key_exchange => :ecdh_anon} | _]) do
    true
  end

  defp advertises_ec_ciphers([{:ecdhe_psk, _, _, _} | _]) do
    true
  end

  defp advertises_ec_ciphers([_ | rest]) do
    advertises_ec_ciphers(rest)
  end

  defp select_shared_curve([], _) do
    :no_curve
  end

  defp select_shared_curve([curve | rest], curves) do
    case :lists.member(curve, curves) do
      true ->
        {:namedCurve, curve}

      false ->
        select_shared_curve(rest, curves)
    end
  end

  defp sni(:undefined) do
    :undefined
  end

  defp sni(:disable) do
    :undefined
  end

  defp sni(hostname) do
    r_sni(hostname: hostname)
  end

  def max_frag_enum(512) do
    r_max_frag_enum(enum: 1)
  end

  def max_frag_enum(1024) do
    r_max_frag_enum(enum: 2)
  end

  def max_frag_enum(2048) do
    r_max_frag_enum(enum: 3)
  end

  def max_frag_enum(4096) do
    r_max_frag_enum(enum: 4)
  end

  def max_frag_enum(:undefined) do
    :undefined
  end

  defp renegotiation_info(_, :client, _, false) do
    r_renegotiation_info(renegotiated_connection: :undefined)
  end

  defp renegotiation_info(_RecordCB, :server, connectionStates, false) do
    connectionState =
      :ssl_record.current_connection_state(
        connectionStates,
        :read
      )

    case :maps.get(
           :secure_renegotiation,
           connectionState
         ) do
      true ->
        r_renegotiation_info(renegotiated_connection: <<0::size(8)-unsigned-big-integer>>)

      false ->
        r_renegotiation_info(renegotiated_connection: :undefined)
    end
  end

  defp renegotiation_info(_RecordCB, :client, connectionStates, true) do
    connectionState =
      :ssl_record.current_connection_state(
        connectionStates,
        :read
      )

    case :maps.get(
           :secure_renegotiation,
           connectionState
         ) do
      true ->
        data = :maps.get(:client_verify_data, connectionState)
        r_renegotiation_info(renegotiated_connection: data)

      false ->
        r_renegotiation_info(renegotiated_connection: :undefined)
    end
  end

  defp renegotiation_info(_RecordCB, :server, connectionStates, true) do
    connectionState =
      :ssl_record.current_connection_state(
        connectionStates,
        :read
      )

    case :maps.get(
           :secure_renegotiation,
           connectionState
         ) do
      true ->
        cData = :maps.get(:client_verify_data, connectionState)
        sData = :maps.get(:server_verify_data, connectionState)
        r_renegotiation_info(renegotiated_connection: <<cData::binary, sData::binary>>)

      false ->
        r_renegotiation_info(renegotiated_connection: :undefined)
    end
  end

  defp handle_renegotiation_info(
         _,
         _RecordCB,
         _,
         r_renegotiation_info(renegotiated_connection: <<0::size(8)-unsigned-big-integer>>),
         connectionStates,
         false,
         _,
         _
       ) do
    {:ok,
     :ssl_record.set_renegotiation_flag(
       true,
       connectionStates
     )}
  end

  defp handle_renegotiation_info(
         _,
         _RecordCB,
         :server,
         :undefined,
         connectionStates,
         _,
         _,
         cipherSuites
       ) do
    case is_member(
           <<0::size(8)-unsigned-big-integer, 255::size(8)-unsigned-big-integer>>,
           cipherSuites
         ) do
      true ->
        {:ok,
         :ssl_record.set_renegotiation_flag(
           true,
           connectionStates
         )}

      false ->
        {:ok,
         :ssl_record.set_renegotiation_flag(
           false,
           connectionStates
         )}
    end
  end

  defp handle_renegotiation_info(_, _RecordCB, _, :undefined, connectionStates, false, _, _) do
    {:ok,
     :ssl_record.set_renegotiation_flag(
       false,
       connectionStates
     )}
  end

  defp handle_renegotiation_info(
         _,
         _RecordCB,
         :client,
         r_renegotiation_info(renegotiated_connection: clientServerVerify),
         connectionStates,
         true,
         _,
         _
       ) do
    connectionState =
      :ssl_record.current_connection_state(
        connectionStates,
        :read
      )

    cData = :maps.get(:client_verify_data, connectionState)
    sData = :maps.get(:server_verify_data, connectionState)

    case <<cData::binary, sData::binary>> == clientServerVerify do
      true ->
        {:ok, connectionStates}

      false ->
        throw(
          r_alert(
            level: 2,
            description: 40,
            where: %{
              :mfa => {:ssl_handshake, :handle_renegotiation_info, 8},
              :line => 3489,
              :file => 'otp/lib/ssl/src/ssl_handshake.erl'
            },
            reason: :client_renegotiation
          )
        )
    end
  end

  defp handle_renegotiation_info(
         _,
         _RecordCB,
         :server,
         r_renegotiation_info(renegotiated_connection: clientVerify),
         connectionStates,
         true,
         _,
         cipherSuites
       ) do
    case is_member(
           <<0::size(8)-unsigned-big-integer, 255::size(8)-unsigned-big-integer>>,
           cipherSuites
         ) do
      true ->
        throw(
          r_alert(
            level: 2,
            description: 40,
            where: %{
              :mfa => {:ssl_handshake, :handle_renegotiation_info, 8},
              :line => 3496,
              :file => 'otp/lib/ssl/src/ssl_handshake.erl'
            },
            reason: {:server_renegotiation, :empty_renegotiation_info_scsv}
          )
        )

      false ->
        connectionState =
          :ssl_record.current_connection_state(
            connectionStates,
            :read
          )

        data = :maps.get(:client_verify_data, connectionState)

        case data == clientVerify do
          true ->
            {:ok, connectionStates}

          false ->
            throw(
              r_alert(
                level: 2,
                description: 40,
                where: %{
                  :mfa => {:ssl_handshake, :handle_renegotiation_info, 8},
                  :line => 3504,
                  :file => 'otp/lib/ssl/src/ssl_handshake.erl'
                },
                reason: :server_renegotiation
              )
            )
        end
    end
  end

  defp handle_renegotiation_info(
         _,
         recordCB,
         :client,
         :undefined,
         connectionStates,
         true,
         secureRenegotation,
         _
       ) do
    handle_renegotiation_info(recordCB, connectionStates, secureRenegotation)
  end

  defp handle_renegotiation_info(
         _,
         recordCB,
         :server,
         :undefined,
         connectionStates,
         true,
         secureRenegotation,
         cipherSuites
       ) do
    case is_member(
           <<0::size(8)-unsigned-big-integer, 255::size(8)-unsigned-big-integer>>,
           cipherSuites
         ) do
      true ->
        throw(
          r_alert(
            level: 2,
            description: 40,
            where: %{
              :mfa => {:ssl_handshake, :handle_renegotiation_info, 8},
              :line => 3513,
              :file => 'otp/lib/ssl/src/ssl_handshake.erl'
            },
            reason: {:server_renegotiation, :empty_renegotiation_info_scsv}
          )
        )

      false ->
        handle_renegotiation_info(recordCB, connectionStates, secureRenegotation)
    end
  end

  defp handle_renegotiation_info(_RecordCB, connectionStates, secureRenegotation) do
    connectionState =
      :ssl_record.current_connection_state(
        connectionStates,
        :read
      )

    case {secureRenegotation, :maps.get(:secure_renegotiation, connectionState)} do
      {_, true} ->
        throw(
          r_alert(
            level: 2,
            description: 40,
            where: %{
              :mfa => {:ssl_handshake, :handle_renegotiation_info, 3},
              :line => 3522,
              :file => 'otp/lib/ssl/src/ssl_handshake.erl'
            },
            reason: :already_secure
          )
        )

      {true, false} ->
        throw(
          r_alert(
            level: 2,
            description: 100,
            where: %{
              :mfa => {:ssl_handshake, :handle_renegotiation_info, 3},
              :line => 3524,
              :file => 'otp/lib/ssl/src/ssl_handshake.erl'
            }
          )
        )

      {false, false} ->
        {:ok, connectionStates}
    end
  end

  defp cert_curve(_, _, :no_suite) do
    {:no_curve, :no_suite}
  end

  defp cert_curve(cert, eCCCurve0, cipherSuite) do
    case :ssl_cipher_format.suite_bin_to_map(cipherSuite) do
      %{:key_exchange => kex}
      when kex == :ecdh_ecdsa or
             kex == :ecdh_rsa ->
        otpCert = :public_key.pkix_decode_cert(cert, :otp)
        tBSCert = r_OTPCertificate(otpCert, :tbsCertificate)

        r_OTPSubjectPublicKeyInfo(algorithm: algInfo) =
          r_OTPTBSCertificate(tBSCert, :subjectPublicKeyInfo)

        {:namedCurve, oid} = r_PublicKeyAlgorithm(algInfo, :parameters)
        {{:namedCurve, oid}, cipherSuite}

      _ ->
        {eCCCurve0, cipherSuite}
    end
  end

  defp empty_extensions() do
    %{}
  end

  def empty_extensions({3, 4}, :client_hello) do
    %{
      :sni => :undefined,
      :elliptic_curves => :undefined,
      :signature_algs => :undefined,
      :alpn => :undefined,
      :key_share => :undefined,
      :pre_shared_key => :undefined,
      :psk_key_exchange_modes => :undefined,
      :cookie => :undefined,
      :client_hello_versions => :undefined,
      :signature_algs_cert => :undefined
    }
  end

  def empty_extensions({3, 3}, :client_hello) do
    ext = empty_extensions({3, 2}, :client_hello)
    %{ext | :signature_algs => :undefined}
  end

  def empty_extensions(_, :client_hello) do
    %{
      :renegotiation_info => :undefined,
      :alpn => :undefined,
      :next_protocol_negotiation => :undefined,
      :srp => :undefined,
      :ec_point_formats => :undefined,
      :elliptic_curves => :undefined,
      :sni => :undefined
    }
  end

  def empty_extensions({3, 4}, :server_hello) do
    %{
      :server_hello_selected_version => :undefined,
      :key_share => :undefined,
      :pre_shared_key => :undefined
    }
  end

  def empty_extensions({3, 4}, :hello_retry_request) do
    %{
      :server_hello_selected_version => :undefined,
      :key_share => :undefined,
      :pre_shared_key => :undefined,
      :cookie => :undefined
    }
  end

  def empty_extensions(_, :server_hello) do
    %{
      :renegotiation_info => :undefined,
      :alpn => :undefined,
      :next_protocol_negotiation => :undefined,
      :ec_point_formats => :undefined
    }
  end

  defp handle_log(level, {logLevel, reportMap, meta}) do
    :ssl_logger.log(level, logLevel, reportMap, meta)
  end
end
