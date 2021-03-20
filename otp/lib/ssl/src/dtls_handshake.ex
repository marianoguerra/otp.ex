defmodule :m_dtls_handshake do
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

  Record.defrecord(:r_protocol_buffers, :protocol_buffers,
    dtls_record_buffer: <<>>,
    dtls_handshake_next_seq: 0,
    dtls_flight_last: :undefined,
    dtls_handshake_next_fragments: [],
    dtls_handshake_later_fragments: [],
    dtls_cipher_texts: []
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

  Record.defrecord(:r_ssl_tls, :ssl_tls,
    type: :undefined,
    version: :undefined,
    fragment: :undefined,
    epoch: :undefined,
    sequence_number: :undefined
  )

  Record.defrecord(:r_alert, :alert,
    level: :undefined,
    description: :undefined,
    where: :undefined,
    role: :undefined,
    reason: :undefined
  )

  def client_hello(host, port, connectionStates, sslOpts, id, renegotiation, ownCert) do
    client_hello(
      host,
      port,
      <<>>,
      connectionStates,
      sslOpts,
      id,
      renegotiation,
      ownCert,
      :undefined
    )
  end

  def client_hello(
        _Host,
        _Port,
        cookie,
        connectionStates,
        %{versions: versions, ciphers: userSuites, fallback: fallback} = sslOpts,
        id,
        renegotiation,
        _OwnCert,
        ocspNonce
      ) do
    version = :dtls_record.highest_protocol_version(versions)

    pending =
      :ssl_record.pending_connection_state(
        connectionStates,
        :read
      )

    secParams = :maps.get(:security_parameters, pending)
    tLSVersion = :dtls_v1.corresponding_tls_version(version)

    cipherSuites =
      :ssl_handshake.available_suites(
        userSuites,
        tLSVersion
      )

    extensions =
      :ssl_handshake.client_hello_extensions(
        tLSVersion,
        cipherSuites,
        sslOpts,
        connectionStates,
        renegotiation,
        :undefined,
        :undefined,
        ocspNonce
      )

    r_client_hello(
      session_id: id,
      client_version: version,
      cipher_suites: :ssl_handshake.cipher_suites(cipherSuites, renegotiation, fallback),
      compression_methods: :ssl_record.compressions(),
      random: r_security_parameters(secParams, :client_random),
      cookie: cookie,
      extensions: extensions
    )
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
        %{versions: supportedVersions} = sslOpt,
        connectionStates0,
        renegotiation,
        oldId
      ) do
    isNew = :ssl_session.is_new(oldId, sessionId)

    case :dtls_record.is_acceptable_version(
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
            mfa: {:dtls_handshake, :hello, 5},
            line: 108,
            file: 'otp/lib/ssl/src/dtls_handshake.erl'
          }
        )
    end
  end

  def hello(
        r_client_hello(client_version: clientVersion) = hello,
        %{versions: versions} = sslOpts,
        info,
        renegotiation
      ) do
    version = :ssl_handshake.select_version(:dtls_record, clientVersion, versions)
    handle_client_hello(version, hello, sslOpts, info, renegotiation)
  end

  def cookie(
        key,
        address,
        port,
        r_client_hello(
          client_version: {major, minor},
          random: random,
          session_id: sessionId,
          cipher_suites: cipherSuites,
          compression_methods: compressionMethods
        )
      ) do
    cookieData = [
      address_to_bin(address, port),
      <<major::size(8)-unsigned-big-integer, minor::size(8)-unsigned-big-integer>>,
      random,
      sessionId,
      cipherSuites,
      compressionMethods
    ]

    :crypto.mac(:hmac, :sha, key, cookieData)
  end

  def hello_verify_request(cookie, version) do
    r_hello_verify_request(protocol_version: version, cookie: cookie)
  end

  def fragment_handshake(bin, _) when is_binary(bin) do
    bin
  end

  def fragment_handshake([msgType, len, seq, _, len, bin], size) do
    bins = bin_fragments(bin, size)
    handshake_fragments(msgType, seq, len, bins, [])
  end

  def encode_handshake(handshake, version, seq) do
    {msgType, bin} = enc_handshake(handshake, version)
    len = byte_size(bin)

    [
      msgType,
      <<len::size(24)-unsigned-big-integer>>,
      <<seq::size(16)-unsigned-big-integer>>,
      <<0::size(24)-unsigned-big-integer>>,
      <<len::size(24)-unsigned-big-integer>>,
      bin
    ]
  end

  def get_dtls_handshake(version, fragment, protocolBuffers, options) do
    handle_fragments(version, fragment, protocolBuffers, options, [])
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
           versions: versions,
           signature_algs: supportedHashSigns,
           eccs: supportedECCs,
           honor_ecc_order: eCCOrder
         } = sslOpts,
         {sessIdTracker, session0, connectionStates0, cert, _},
         renegotiation
       ) do
    case :dtls_record.is_acceptable_version(
           version,
           versions
         ) do
      true ->
        curves = :maps.get(:elliptic_curves, helloExt, :undefined)
        clientHashSigns = :maps.get(:signature_algs, helloExt, :undefined)
        tLSVersion = :dtls_v1.corresponding_tls_version(version)

        availableHashSigns =
          :ssl_handshake.available_signature_algs(
            clientHashSigns,
            supportedHashSigns,
            cert,
            tLSVersion
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
            tLSVersion,
            sslOpts,
            cert
          )

        case cipherSuite do
          :no_suite ->
            r_alert(
              level: 2,
              description: 71,
              where: %{
                mfa: {:dtls_handshake, :handle_client_hello, 5},
                line: 194,
                file: 'otp/lib/ssl/src/dtls_handshake.erl'
              }
            )

          _ ->
            %{key_exchange: keyExAlg} = :ssl_cipher_format.suite_bin_to_map(cipherSuite)

            case :ssl_handshake.select_hashsign(
                   {clientHashSigns, :undefined},
                   cert,
                   keyExAlg,
                   supportedHashSigns,
                   tLSVersion
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
            mfa: {:dtls_handshake, :handle_client_hello, 5},
            line: 208,
            file: 'otp/lib/ssl/src/dtls_handshake.erl'
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
        :dtls_record,
        random,
        cipherSuites,
        helloExt,
        :dtls_v1.corresponding_tls_version(version),
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
        :dtls_record,
        random,
        cipherSuite,
        compression,
        helloExt,
        :dtls_v1.corresponding_tls_version(version),
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

  defp enc_handshake(
         r_hello_verify_request(
           protocol_version: {major, minor},
           cookie: cookie
         ),
         _Version
       ) do
    cookieLength = byte_size(cookie)

    {3,
     <<major::size(8)-unsigned-big-integer, minor::size(8)-unsigned-big-integer,
       cookieLength::size(8)-unsigned-big-integer, cookie::size(cookieLength)-binary>>}
  end

  defp enc_handshake(r_hello_request(), _Version) do
    {0, <<>>}
  end

  defp enc_handshake(
         r_client_hello(
           client_version: {major, minor},
           random: random,
           session_id: sessionID,
           cookie: cookie,
           cipher_suites: cipherSuites,
           compression_methods: compMethods,
           extensions: helloExtensions
         ),
         _Version
       ) do
    sIDLength = byte_size(sessionID)
    cookieLength = byte_size(cookie)
    binCompMethods = :erlang.list_to_binary(compMethods)
    cmLength = byte_size(binCompMethods)
    binCipherSuites = :erlang.list_to_binary(cipherSuites)
    csLength = byte_size(binCipherSuites)

    extensionsBin =
      :ssl_handshake.encode_hello_extensions(
        helloExtensions,
        :dtls_v1.corresponding_tls_version({major, minor})
      )

    {1,
     <<major::size(8)-unsigned-big-integer, minor::size(8)-unsigned-big-integer,
       random::size(32)-binary, sIDLength::size(8)-unsigned-big-integer, sessionID::binary,
       cookieLength::size(8)-unsigned-big-integer, cookie::binary,
       csLength::size(16)-unsigned-big-integer, binCipherSuites::binary,
       cmLength::size(8)-unsigned-big-integer, binCompMethods::binary, extensionsBin::binary>>}
  end

  defp enc_handshake(r_server_hello() = handshakeMsg, version) do
    {type,
     <<major::size(8)-unsigned-big-integer, minor::size(8)-unsigned-big-integer, rest::binary>>} =
      :ssl_handshake.encode_handshake(
        handshakeMsg,
        version
      )

    {dTLSMajor, dTLSMinor} = :dtls_v1.corresponding_dtls_version({major, minor})

    {type,
     <<dTLSMajor::size(8)-unsigned-big-integer, dTLSMinor::size(8)-unsigned-big-integer,
       rest::binary>>}
  end

  defp enc_handshake(handshakeMsg, version) do
    :ssl_handshake.encode_handshake(
      handshakeMsg,
      :dtls_v1.corresponding_tls_version(version)
    )
  end

  defp handshake_bin(
         r_handshake_fragment(
           type: type,
           length: len,
           message_seq: seq,
           fragment_length: len,
           fragment_offset: 0,
           fragment: fragment
         )
       ) do
    handshake_bin(type, len, seq, fragment)
  end

  defp handshake_bin(type, length, seq, fragmentData) do
    <<type::size(8)-unsigned-big-integer, length::size(24)-unsigned-big-integer,
      seq::size(16)-unsigned-big-integer, 0::size(24)-unsigned-big-integer,
      length::size(24)-unsigned-big-integer, fragmentData::size(length)-binary>>
  end

  defp bin_fragments(bin, size) do
    bin_fragments(bin, :erlang.size(bin), size, 0, [])
  end

  defp bin_fragments(bin, binSize, fragSize, offset, fragments) do
    case binSize - offset - fragSize > 0 do
      true ->
        frag = :binary.part(bin, {offset, fragSize})
        bin_fragments(bin, binSize, fragSize, offset + fragSize, [{frag, offset} | fragments])

      false ->
        frag = :binary.part(bin, {offset, binSize - offset})
        :lists.reverse([{frag, offset} | fragments])
    end
  end

  defp handshake_fragments(_, _, _, [], acc) do
    :lists.reverse(acc)
  end

  defp handshake_fragments(msgType, seq, len, [{bin, offset} | bins], acc) do
    fragLen = :erlang.size(bin)

    handshake_fragments(msgType, seq, len, bins, [
      <<msgType::size(8)-unsigned-big-integer, len::binary, seq::binary,
        offset::size(24)-unsigned-big-integer, fragLen::size(24)-unsigned-big-integer,
        bin::binary>>
      | acc
    ])
  end

  defp address_to_bin({a, b, c, d}, port) do
    <<0::size(80), 65535::size(16), a, b, c, d, port::size(16)>>
  end

  defp address_to_bin({a, b, c, d, e, f, g, h}, port) do
    <<a::size(16), b::size(16), c::size(16), d::size(16), e::size(16), f::size(16), g::size(16),
      h::size(16), port::size(16)>>
  end

  defp handle_fragments(version, fragmentData, buffers0, options, acc) do
    fragments = decode_handshake_fragments(fragmentData)
    do_handle_fragments(version, fragments, buffers0, options, acc)
  end

  defp do_handle_fragments(_, [], buffers, _Options, acc) do
    {:lists.reverse(acc), buffers}
  end

  defp do_handle_fragments(
         version,
         [fragment | fragments],
         buffers0,
         %{log_level: logLevel} = options,
         acc
       ) do
    case reassemble(version, fragment, buffers0) do
      {:more_data, buffers} when fragments == [] ->
        {:lists.reverse(acc), buffers}

      {:more_data, buffers} ->
        do_handle_fragments(version, fragments, buffers, options, acc)

      {{handshake, _} = hsPacket, buffers} ->
        :ssl_logger.debug(logLevel, :inbound, :handshake, handshake)
        do_handle_fragments(version, fragments, buffers, options, [hsPacket | acc])
    end
  end

  defp decode_handshake(
         version,
         <<type::size(8)-unsigned-big-integer, bin::binary>>
       ) do
    decode_handshake(version, type, bin)
  end

  defp decode_handshake(_, 0, <<>>) do
    r_hello_request()
  end

  defp decode_handshake(
         version,
         1,
         <<_::size(24)-unsigned-big-integer, _::size(16)-unsigned-big-integer,
           _::size(24)-unsigned-big-integer, _::size(24)-unsigned-big-integer,
           major::size(8)-unsigned-big-integer, minor::size(8)-unsigned-big-integer,
           random::size(32)-binary, sID_length::size(8)-unsigned-big-integer,
           session_ID::size(sID_length)-binary, cookieLength::size(8)-unsigned-big-integer,
           cookie::size(cookieLength)-binary, cs_length::size(16)-unsigned-big-integer,
           cipherSuites::size(cs_length)-binary, cm_length::size(8)-unsigned-big-integer,
           comp_methods::size(cm_length)-binary, extensions::binary>>
       ) do
    tLSVersion = :dtls_v1.corresponding_tls_version(version)
    legacyVersion = :dtls_v1.corresponding_tls_version({major, minor})
    exts = :ssl_handshake.decode_vector(extensions)

    decodedExtensions =
      :ssl_handshake.decode_hello_extensions(
        exts,
        tLSVersion,
        legacyVersion,
        :client
      )

    r_client_hello(
      client_version: {major, minor},
      random: random,
      cookie: cookie,
      session_id: session_ID,
      cipher_suites:
        :ssl_handshake.decode_suites(
          :"2_bytes",
          cipherSuites
        ),
      compression_methods: comp_methods,
      extensions: decodedExtensions
    )
  end

  defp decode_handshake(
         _Version,
         3,
         <<_::size(24)-unsigned-big-integer, _::size(16)-unsigned-big-integer,
           _::size(24)-unsigned-big-integer, _::size(24)-unsigned-big-integer,
           major::size(8)-unsigned-big-integer, minor::size(8)-unsigned-big-integer,
           cookieLength::size(8)-unsigned-big-integer, cookie::size(cookieLength)-binary>>
       ) do
    r_hello_verify_request(protocol_version: {major, minor}, cookie: cookie)
  end

  defp decode_handshake(
         version,
         tag,
         <<_::size(24)-unsigned-big-integer, _::size(16)-unsigned-big-integer,
           _::size(24)-unsigned-big-integer, _::size(24)-unsigned-big-integer, msg::binary>>
       ) do
    decode_tls_handshake(version, tag, msg)
  end

  defp decode_tls_handshake(version, tag, msg) do
    tLSVersion = :dtls_v1.corresponding_tls_version(version)
    :ssl_handshake.decode_handshake(tLSVersion, tag, msg)
  end

  defp decode_handshake_fragments(<<>>) do
    [<<>>]
  end

  defp decode_handshake_fragments(
         <<type::size(8)-unsigned-big-integer, length::size(24)-unsigned-big-integer,
           messageSeq::size(16)-unsigned-big-integer,
           fragmentOffset::size(24)-unsigned-big-integer,
           fragmentLength::size(24)-unsigned-big-integer, fragment::size(fragmentLength)-binary,
           rest::binary>>
       ) do
    [
      r_handshake_fragment(
        type: type,
        length: length,
        message_seq: messageSeq,
        fragment_offset: fragmentOffset,
        fragment_length: fragmentLength,
        fragment: fragment
      )
      | decode_handshake_fragments(rest)
    ]
  end

  defp reassemble(
         version,
         r_handshake_fragment(message_seq: seq) = fragment,
         r_protocol_buffers(
           dtls_handshake_next_seq: seq,
           dtls_handshake_next_fragments: fragments0,
           dtls_handshake_later_fragments: laterFragments0
         ) = buffers0
       ) do
    case reassemble_fragments(fragment, fragments0) do
      {:more_data, fragments} ->
        {:more_data, r_protocol_buffers(buffers0, dtls_handshake_next_fragments: fragments)}

      {:raw, rawHandshake} ->
        handshake = decode_handshake(version, rawHandshake)
        {nextFragments, laterFragments} = next_fragments(laterFragments0)

        {{handshake, rawHandshake},
         r_protocol_buffers(buffers0,
           dtls_handshake_next_seq: seq + 1,
           dtls_handshake_next_fragments: nextFragments,
           dtls_handshake_later_fragments: laterFragments
         )}
    end
  end

  defp reassemble(
         _,
         r_handshake_fragment(message_seq: fragSeq) = fragment,
         r_protocol_buffers(
           dtls_handshake_next_seq: seq,
           dtls_handshake_later_fragments: laterFragments
         ) = buffers0
       )
       when fragSeq > seq do
    {:more_data,
     r_protocol_buffers(buffers0,
       dtls_handshake_later_fragments: [
         fragment
         | laterFragments
       ]
     )}
  end

  defp reassemble(_, _, buffers) do
    {:more_data, buffers}
  end

  defp reassemble_fragments(current, fragments0) do
    [frag1 | frags] =
      :lists.keysort(
        r_handshake_fragment(:fragment_offset),
        [current | fragments0]
      )

    [fragment | _] =
      fragments =
      merge_fragment(
        frag1,
        frags
      )

    case is_complete_handshake(fragment) do
      true ->
        {:raw, handshake_bin(fragment)}

      false ->
        {:more_data, fragments}
    end
  end

  defp merge_fragment(frag0, []) do
    [frag0]
  end

  defp merge_fragment(frag0, [frag1 | rest]) do
    case merge_fragments(frag0, frag1) do
      [_ | _] = frags ->
        frags ++ rest

      frag ->
        merge_fragment(frag, rest)
    end
  end

  defp merge_fragments(
         r_handshake_fragment(
           fragment_offset: previousOffSet,
           fragment_length: previousLen,
           fragment: previousData
         ) = previous,
         r_handshake_fragment(
           fragment_offset: previousOffSet,
           fragment_length: previousLen,
           fragment: previousData
         )
       ) do
    previous
  end

  defp merge_fragments(
         r_handshake_fragment(
           fragment_offset: previousOffset,
           fragment_length: previousLen
         ) = previous,
         r_handshake_fragment(
           fragment_offset: currentOffset,
           fragment_length: currentLen
         )
       )
       when previousOffset <= currentOffset and currentOffset <= previousOffset + previousLen and
              currentOffset + currentLen <= previousOffset + previousLen do
    previous
  end

  defp merge_fragments(
         r_handshake_fragment(
           fragment_offset: previousOffset,
           fragment_length: previousLen
         ),
         r_handshake_fragment(
           fragment_offset: currentOffset,
           fragment_length: currentLen
         ) = current
       )
       when currentOffset <= previousOffset and
              currentOffset + currentLen >= previousOffset + previousLen do
    current
  end

  defp merge_fragments(
         r_handshake_fragment(
           fragment_offset: previousOffset,
           fragment_length: previousLen,
           fragment: previousData
         ) = previous,
         r_handshake_fragment(
           fragment_offset: currentOffset,
           fragment_length: currentLen,
           fragment: currentData
         )
       )
       when currentOffset < previousOffset and
              currentOffset + currentLen < previousOffset + previousLen do
    newDataLen = previousOffset - currentOffset
    <<newData::size(newDataLen)-binary, _::binary>> = currentData

    r_handshake_fragment(previous,
      fragment_length: previousLen + newDataLen,
      fragment: <<newData::binary, previousData::binary>>
    )
  end

  defp merge_fragments(
         r_handshake_fragment(
           fragment_offset: previousOffset,
           fragment_length: previousLen,
           fragment: previousData
         ) = previous,
         r_handshake_fragment(
           fragment_offset: currentOffset,
           fragment_length: currentLen,
           fragment: currentData
         )
       )
       when currentOffset > previousOffset and currentOffset < previousOffset + previousLen do
    newDataLen = currentOffset + currentLen - (previousOffset + previousLen)
    dropLen = currentLen - newDataLen
    <<_::size(dropLen)-binary, newData::binary>> = currentData

    r_handshake_fragment(previous,
      fragment_length: previousLen + newDataLen,
      fragment: <<previousData::binary, newData::binary>>
    )
  end

  defp merge_fragments(
         r_handshake_fragment(
           fragment_offset: previousOffset,
           fragment_length: previousLen,
           fragment: previousData
         ) = previous,
         r_handshake_fragment(
           fragment_offset: currentOffset,
           fragment_length: currentLen,
           fragment: currentData
         )
       )
       when currentOffset === previousOffset + previousLen do
    r_handshake_fragment(previous,
      fragment_length: previousLen + currentLen,
      fragment: <<previousData::binary, currentData::binary>>
    )
  end

  defp merge_fragments(
         r_handshake_fragment(
           fragment_offset: previousOffset,
           fragment_length: previousLen,
           fragment: previousData
         ) = previous,
         r_handshake_fragment(
           fragment_offset: currentOffset,
           fragment_length: currentLen,
           fragment: currentData
         )
       )
       when previousOffset === currentOffset + currentLen do
    r_handshake_fragment(previous,
      fragment_length: previousLen + currentLen,
      fragment: <<currentData::binary, previousData::binary>>
    )
  end

  defp merge_fragments(previous, current) do
    [previous, current]
  end

  defp next_fragments(laterFragments) do
    case :lists.keysort(
           r_handshake_fragment(:message_seq),
           laterFragments
         ) do
      [] ->
        {[], []}

      [r_handshake_fragment(message_seq: seq) | _] = fragments ->
        split_frags(fragments, seq, [])
    end
  end

  defp split_frags([r_handshake_fragment(message_seq: seq) = frag | rest], seq, acc) do
    split_frags(rest, seq, [frag | acc])
  end

  defp split_frags(frags, _, acc) do
    {:lists.reverse(acc), frags}
  end

  defp is_complete_handshake(r_handshake_fragment(length: length, fragment_length: length)) do
    true
  end

  defp is_complete_handshake(_) do
    false
  end
end
