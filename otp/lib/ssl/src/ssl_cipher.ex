defmodule :m_ssl_cipher do
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

  Record.defrecord(:r_alert, :alert,
    level: :undefined,
    description: :undefined,
    where: :undefined,
    role: :undefined,
    reason: :undefined
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

  def security_parameters(
        <<0::size(8)-unsigned-big-integer, 0::size(8)-unsigned-big-integer>> = cipherSuite,
        secParams
      ) do
    security_parameters(:undefined, cipherSuite, secParams)
  end

  def security_parameters(version, cipherSuite, secParams) do
    %{cipher: cipher, mac: hash, prf: prfHashAlg} =
      :ssl_cipher_format.suite_bin_to_map(cipherSuite)

    r_security_parameters(secParams,
      cipher_suite: cipherSuite,
      bulk_cipher_algorithm: bulk_cipher_algorithm(cipher),
      cipher_type: type(cipher),
      key_size: effective_key_bits(cipher),
      expanded_key_material_length: expanded_key_material(cipher),
      key_material_length: key_material(cipher),
      iv_size: iv_size(cipher),
      mac_algorithm: mac_algorithm(hash),
      prf_algorithm: prf_algorithm(prfHashAlg, version),
      hash_size: hash_size(hash)
    )
  end

  def security_parameters_1_3(secParams, cipherSuite) do
    %{cipher: cipher, prf: prfHashAlg} = :ssl_cipher_format.suite_bin_to_map(cipherSuite)

    r_security_parameters(secParams,
      cipher_suite: cipherSuite,
      bulk_cipher_algorithm: bulk_cipher_algorithm(cipher),
      prf_algorithm: prfHashAlg,
      cipher_type: 2
    )
  end

  def cipher_init(1, iV, key) do
    state = {:stream_init, :rc4, key, iV}
    r_cipher_state(iv: iV, key: key, state: state)
  end

  def cipher_init(type, iV, key) when type == 8 or type == 10 do
    <<nonce::size(64)>> = random_bytes(8)
    r_cipher_state(iv: iV, key: key, nonce: nonce, tag_len: 16)
  end

  def cipher_init(11, iV, key) do
    <<nonce::size(64)>> = random_bytes(8)
    r_cipher_state(iv: iV, key: key, nonce: nonce, tag_len: 8)
  end

  def cipher_init(9, iV, key) do
    r_cipher_state(iv: iV, key: key, tag_len: 16)
  end

  def cipher_init(_BCA, iV, key) do
    r_cipher_state(iv: iV, key: key, state: <<>>)
  end

  def nonce_seed(seed, cipherState) do
    r_cipher_state(cipherState, nonce: seed)
  end

  def cipher(0, cipherState, <<>>, fragment, _Version) do
    {:erlang.iolist_to_binary(fragment), cipherState}
  end

  def cipher(
        cipherEnum,
        cipherState = r_cipher_state(state: {:stream_init, :rc4, key, _IV}),
        mac,
        fragment,
        version
      ) do
    state = :crypto.crypto_init(:rc4, key, true)
    cipher(cipherEnum, r_cipher_state(cipherState, state: state), mac, fragment, version)
  end

  def cipher(1, cipherState = r_cipher_state(state: state), mac, fragment, _Version) do
    genStreamCipherList = [fragment, mac]
    t = :crypto.crypto_update(state, genStreamCipherList)
    {:erlang.iolist_to_binary(t), cipherState}
  end

  def cipher(3, cipherState, mac, fragment, version) do
    block_cipher(
      fn key, iV, t ->
        :crypto.crypto_one_time(:des_cbc, key, iV, t, true)
      end,
      block_size(:des_cbc),
      cipherState,
      mac,
      fragment,
      version
    )
  end

  def cipher(4, cipherState, mac, fragment, version) do
    block_cipher(
      fn key, iV, t ->
        :crypto.crypto_one_time(:des_ede3_cbc, key, iV, t, true)
      end,
      block_size(:des_ede3_cbc),
      cipherState,
      mac,
      fragment,
      version
    )
  end

  def cipher(7, cipherState, mac, fragment, version) do
    block_cipher(
      fn
        key, iV, t when byte_size(key) === 16 ->
          :crypto.crypto_one_time(:aes_128_cbc, key, iV, t, true)

        key, iV, t when byte_size(key) === 32 ->
          :crypto.crypto_one_time(:aes_256_cbc, key, iV, t, true)
      end,
      block_size(:aes_128_cbc),
      cipherState,
      mac,
      fragment,
      version
    )
  end

  def aead_encrypt(type, key, nonce, fragment, additionalData, tagLen) do
    :crypto.crypto_one_time_aead(
      aead_type(
        type,
        :erlang.size(key)
      ),
      key,
      nonce,
      fragment,
      additionalData,
      tagLen,
      true
    )
  end

  def aead_decrypt(type, key, nonce, cipherText, cipherTag, additionalData) do
    :crypto.crypto_one_time_aead(
      aead_type(
        type,
        :erlang.size(key)
      ),
      key,
      nonce,
      cipherText,
      additionalData,
      cipherTag,
      false
    )
  end

  defp aead_type(8, 16) do
    :aes_128_gcm
  end

  defp aead_type(8, 24) do
    :aes_192_gcm
  end

  defp aead_type(8, 32) do
    :aes_256_gcm
  end

  defp aead_type(10, 16) do
    :aes_128_ccm
  end

  defp aead_type(10, 24) do
    :aes_192_ccm
  end

  defp aead_type(10, 32) do
    :aes_256_ccm
  end

  defp aead_type(11, 16) do
    :aes_128_ccm
  end

  defp aead_type(11, 24) do
    :aes_192_ccm
  end

  defp aead_type(11, 32) do
    :aes_256_ccm
  end

  defp aead_type(9, _) do
    :chacha20_poly1305
  end

  defp build_cipher_block(blockSz, mac, fragment) do
    totSz = byte_size(mac) + :erlang.iolist_size(fragment) + 1
    [fragment, mac, padding_with_len(totSz, blockSz)]
  end

  defp block_cipher(fun, blockSz, r_cipher_state(key: key, iv: iV) = cS0, mac, fragment, {3, n})
       when n == 0 or n == 1 do
    l = build_cipher_block(blockSz, mac, fragment)
    t = fun.(key, iV, l)
    nextIV = next_iv(t, iV)
    {t, r_cipher_state(cS0, iv: nextIV)}
  end

  defp block_cipher(
         fun,
         blockSz,
         r_cipher_state(key: key, iv: iV, state: iV_Cache0) = cS0,
         mac,
         fragment,
         {3, n}
       )
       when n == 2 or n == 3 or n == 4 do
    iV_Size = byte_size(iV)

    <<nextIV::size(iV_Size)-binary, iV_Cache::binary>> =
      case iV_Cache0 do
        <<>> ->
          random_bytes(iV_Size <<< 5)

        _ ->
          iV_Cache0
      end

    l0 = build_cipher_block(blockSz, mac, fragment)
    l = [nextIV | l0]
    t = fun.(key, iV, l)
    {t, r_cipher_state(cS0, iv: nextIV, state: iV_Cache)}
  end

  def decipher(0, _HashSz, cipherState, fragment, _, _) do
    {fragment, <<>>, cipherState}
  end

  def decipher(
        cipherEnum,
        hashSz,
        cipherState = r_cipher_state(state: {:stream_init, :rc4, key, _IV}),
        fragment,
        version,
        paddingCheck
      ) do
    state = :crypto.crypto_init(:rc4, key, false)

    decipher(
      cipherEnum,
      hashSz,
      r_cipher_state(cipherState, state: state),
      fragment,
      version,
      paddingCheck
    )
  end

  def decipher(1, hashSz, cipherState = r_cipher_state(state: state), fragment, _, _) do
    try do
      :crypto.crypto_update(state, fragment)
    catch
      _, _ ->
        r_alert(
          level: 2,
          description: 20,
          where: %{
            mfa: {:ssl_cipher, :decipher, 6},
            line: 273,
            file: 'otp/lib/ssl/src/ssl_cipher.erl'
          },
          reason: :decryption_failed
        )
    else
      text ->
        gSC = generic_stream_cipher_from_bin(text, hashSz)
        r_generic_stream_cipher(content: content, mac: mac) = gSC
        {content, mac, cipherState}
    end
  end

  def decipher(3, hashSz, cipherState, fragment, version, paddingCheck) do
    block_decipher(
      fn key, iV, t ->
        :crypto.crypto_one_time(:des_cbc, key, iV, t, false)
      end,
      cipherState,
      hashSz,
      fragment,
      version,
      paddingCheck
    )
  end

  def decipher(4, hashSz, cipherState, fragment, version, paddingCheck) do
    block_decipher(
      fn key, iV, t ->
        :crypto.crypto_one_time(:des_ede3_cbc, key, iV, t, false)
      end,
      cipherState,
      hashSz,
      fragment,
      version,
      paddingCheck
    )
  end

  def decipher(7, hashSz, cipherState, fragment, version, paddingCheck) do
    block_decipher(
      fn
        key, iV, t when byte_size(key) === 16 ->
          :crypto.crypto_one_time(:aes_128_cbc, key, iV, t, false)

        key, iV, t when byte_size(key) === 32 ->
          :crypto.crypto_one_time(:aes_256_cbc, key, iV, t, false)
      end,
      cipherState,
      hashSz,
      fragment,
      version,
      paddingCheck
    )
  end

  defp block_decipher(
         fun,
         r_cipher_state(key: key, iv: iV) = cipherState0,
         hashSz,
         fragment,
         version,
         paddingCheck
       ) do
    try do
      text = fun.(key, iV, fragment)
      nextIV = next_iv(fragment, iV)
      gBC = generic_block_cipher_from_bin(version, text, nextIV, hashSz)
      content = r_generic_block_cipher(gBC, :content)
      mac = r_generic_block_cipher(gBC, :mac)
      cipherState1 = r_cipher_state(cipherState0, iv: r_generic_block_cipher(gBC, :next_iv))

      case is_correct_padding(gBC, version, paddingCheck) do
        true ->
          {content, mac, cipherState1}

        false ->
          {<<240, content::binary>>, mac, cipherState1}
      end
    catch
      _, _ ->
        r_alert(
          level: 2,
          description: 20,
          where: %{
            mfa: {:ssl_cipher, :block_decipher, 6},
            line: 318,
            file: 'otp/lib/ssl/src/ssl_cipher.erl'
          },
          reason: :decryption_failed
        )
    end
  end

  def suites({3, minor}) do
    :tls_v1.suites(minor)
  end

  def suites({_, minor}) do
    :dtls_v1.suites(minor)
  end

  def all_suites({3, _} = version) do
    suites(version) ++
      psk_suites(version) ++
      srp_suites(version) ++ rsa_suites(version) ++ des_suites(version) ++ rc4_suites(version)
  end

  def all_suites(version) do
    :dtls_v1.all_suites(version)
  end

  def anonymous_suites({3, n} = version) do
    srp_suites_anon(version) ++ anonymous_suites(n)
  end

  def anonymous_suites({254, _} = version) do
    :dtls_v1.anonymous_suites(version)
  end

  def anonymous_suites(4) do
    []
  end

  def anonymous_suites(3 = n) do
    psk_suites_anon(n) ++
      [
        <<0::size(8)-unsigned-big-integer, 166::size(8)-unsigned-big-integer>>,
        <<0::size(8)-unsigned-big-integer, 167::size(8)-unsigned-big-integer>>,
        <<0::size(8)-unsigned-big-integer, 108::size(8)-unsigned-big-integer>>,
        <<0::size(8)-unsigned-big-integer, 109::size(8)-unsigned-big-integer>>,
        <<192::size(8)-unsigned-big-integer, 24::size(8)-unsigned-big-integer>>,
        <<192::size(8)-unsigned-big-integer, 25::size(8)-unsigned-big-integer>>,
        <<192::size(8)-unsigned-big-integer, 23::size(8)-unsigned-big-integer>>,
        <<0::size(8)-unsigned-big-integer, 24::size(8)-unsigned-big-integer>>
      ]
  end

  def anonymous_suites(2 = n) do
    psk_suites_anon(n) ++
      [
        <<192::size(8)-unsigned-big-integer, 24::size(8)-unsigned-big-integer>>,
        <<192::size(8)-unsigned-big-integer, 25::size(8)-unsigned-big-integer>>,
        <<192::size(8)-unsigned-big-integer, 23::size(8)-unsigned-big-integer>>,
        <<0::size(8)-unsigned-big-integer, 26::size(8)-unsigned-big-integer>>,
        <<0::size(8)-unsigned-big-integer, 24::size(8)-unsigned-big-integer>>
      ]
  end

  def anonymous_suites(n) when n == 0 or n == 1 do
    psk_suites_anon(n) ++
      [
        <<0::size(8)-unsigned-big-integer, 24::size(8)-unsigned-big-integer>>,
        <<0::size(8)-unsigned-big-integer, 27::size(8)-unsigned-big-integer>>,
        <<0::size(8)-unsigned-big-integer, 26::size(8)-unsigned-big-integer>>
      ]
  end

  def psk_suites({3, n}) do
    psk_suites(n)
  end

  def psk_suites(4) do
    []
  end

  def psk_suites(3) do
    [
      <<0::size(8)-unsigned-big-integer, 173::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 183::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 172::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 182::size(8)-unsigned-big-integer>>
    ] ++ psk_suites(0)
  end

  def psk_suites(_) do
    [
      <<0::size(8)-unsigned-big-integer, 149::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 148::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 147::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 146::size(8)-unsigned-big-integer>>
    ]
  end

  def psk_suites_anon({3, n}) do
    psk_suites_anon(n)
  end

  def psk_suites_anon(3 = n) do
    [
      <<0::size(8)-unsigned-big-integer, 171::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 169::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 56::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 179::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 175::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 167::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 171::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 165::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 169::size(8)-unsigned-big-integer>>,
      <<208::size(8)-unsigned-big-integer, 1::size(8)-unsigned-big-integer>>,
      <<208::size(8)-unsigned-big-integer, 5::size(8)-unsigned-big-integer>>,
      <<208::size(8)-unsigned-big-integer, 3::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 170::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 168::size(8)-unsigned-big-integer>>,
      <<208::size(8)-unsigned-big-integer, 1::size(8)-unsigned-big-integer>>,
      <<208::size(8)-unsigned-big-integer, 3::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 55::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 178::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 174::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 166::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 170::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 164::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 168::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 51::size(8)-unsigned-big-integer>>
    ] ++ psk_suites_anon(n - 1)
  end

  def psk_suites_anon(n) when n > 0 do
    [
      <<0::size(8)-unsigned-big-integer, 145::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 141::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 53::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 144::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 140::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 52::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 143::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 139::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 51::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 142::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 138::size(8)-unsigned-big-integer>>
    ]
  end

  def psk_suites_anon(0) do
    []
  end

  def srp_suites(_) do
    [
      <<192::size(8)-unsigned-big-integer, 27::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 28::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 30::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 31::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 33::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 34::size(8)-unsigned-big-integer>>
    ]
  end

  def srp_suites_anon(_) do
    [
      <<192::size(8)-unsigned-big-integer, 26::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 29::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 32::size(8)-unsigned-big-integer>>
    ]
  end

  def rc4_suites({3, 0}) do
    rc4_suites(0)
  end

  def rc4_suites({3, minor}) do
    rc4_suites(minor) ++ rc4_suites(0)
  end

  def rc4_suites(0) do
    [
      <<0::size(8)-unsigned-big-integer, 5::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 4::size(8)-unsigned-big-integer>>
    ]
  end

  def rc4_suites(n) when n <= 4 do
    [
      <<192::size(8)-unsigned-big-integer, 7::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 17::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 2::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 12::size(8)-unsigned-big-integer>>
    ]
  end

  def des_suites(_) do
    [
      <<0::size(8)-unsigned-big-integer, 21::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 9::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 8::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 18::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 22::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 19::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 3::size(8)-unsigned-big-integer>>,
      <<192::size(8)-unsigned-big-integer, 13::size(8)-unsigned-big-integer>>
    ]
  end

  def rsa_suites({3, 0}) do
    rsa_suites(0)
  end

  def rsa_suites({3, minor}) do
    rsa_suites(minor) ++ rsa_suites(0)
  end

  def rsa_suites(0) do
    [
      <<0::size(8)-unsigned-big-integer, 53::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 47::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 10::size(8)-unsigned-big-integer>>
    ]
  end

  def rsa_suites(n) when n <= 4 do
    [
      <<0::size(8)-unsigned-big-integer, 157::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 61::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 156::size(8)-unsigned-big-integer>>,
      <<0::size(8)-unsigned-big-integer, 60::size(8)-unsigned-big-integer>>
    ]
  end

  def filter(:undefined, ciphers, _) do
    ciphers
  end

  def filter(derCert, ciphers0, version) do
    otpCert = :public_key.pkix_decode_cert(derCert, :otp)
    sigAlg = r_OTPCertificate(otpCert, :signatureAlgorithm)

    pubKeyInfo =
      r_OTPTBSCertificate(r_OTPCertificate(otpCert, :tbsCertificate), :subjectPublicKeyInfo)

    pubKeyAlg = r_OTPSubjectPublicKeyInfo(pubKeyInfo, :algorithm)

    ciphers =
      filter_suites_pubkey(
        :ssl_certificate.public_key_type(r_PublicKeyAlgorithm(pubKeyAlg, :algorithm)),
        ciphers0,
        version,
        otpCert
      )

    {_, sign} = :public_key.pkix_sign_types(r_SignatureAlgorithm(sigAlg, :algorithm))
    filter_suites_signature(sign, ciphers, version)
  end

  def filter_suites(suites, filters) do
    applyFilters = fn suite ->
      filter_suite(suite, filters)
    end

    :lists.filter(applyFilters, suites)
  end

  defp filter_suite(
         %{key_exchange: keyExchange, cipher: cipher, mac: hash, prf: prf},
         %{
           key_exchange_filters: keyFilters,
           cipher_filters: cipherFilters,
           mac_filters: hashFilters,
           prf_filters: prfFilters
         }
       ) do
    all_filters(
      keyExchange,
      keyFilters
    ) and
      all_filters(
        cipher,
        cipherFilters
      ) and
      all_filters(
        hash,
        hashFilters
      ) and
      all_filters(
        prf,
        prfFilters
      )
  end

  defp filter_suite(suite, filters) do
    filter_suite(
      :ssl_cipher_format.suite_bin_to_map(suite),
      filters
    )
  end

  def filter_suites(suites) do
    filters = crypto_support_filters()
    filter_suites(suites, filters)
  end

  defp all_filters(_, []) do
    true
  end

  defp all_filters(value, [filter | rest]) do
    case filter.(value) do
      true ->
        all_filters(value, rest)

      false ->
        false
    end
  end

  def crypto_support_filters() do
    algos = :crypto.supports()
    hashs = :proplists.get_value(:hashs, algos)

    %{
      key_exchange_filters: [
        fn keyExchange ->
          is_acceptable_keyexchange(
            keyExchange,
            :proplists.get_value(
              :public_keys,
              algos
            )
          )
        end
      ],
      cipher_filters: [
        fn cipher ->
          is_acceptable_cipher(
            cipher,
            :proplists.get_value(:ciphers, algos)
          )
        end
      ],
      mac_filters: [
        fn hash ->
          is_acceptable_hash(hash, hashs)
        end
      ],
      prf_filters: [
        fn prf ->
          is_acceptable_prf(
            prf,
            :proplists.get_value(:hashs, algos)
          )
        end
      ]
    }
  end

  defp is_acceptable_keyexchange(keyExchange, _Algos)
       when keyExchange == :psk or
              keyExchange == :null or
              keyExchange == :any do
    true
  end

  defp is_acceptable_keyexchange(keyExchange, algos)
       when keyExchange == :dh_anon or
              keyExchange == :dhe_psk do
    :proplists.get_bool(:dh, algos)
  end

  defp is_acceptable_keyexchange(:dhe_dss, algos) do
    :proplists.get_bool(
      :dh,
      algos
    ) and :proplists.get_bool(:dss, algos)
  end

  defp is_acceptable_keyexchange(:dhe_rsa, algos) do
    :proplists.get_bool(
      :dh,
      algos
    ) and :proplists.get_bool(:rsa, algos)
  end

  defp is_acceptable_keyexchange(keyExchange, algos)
       when keyExchange == :ecdh_anon or
              keyExchange == :ecdhe_psk do
    :proplists.get_bool(:ecdh, algos)
  end

  defp is_acceptable_keyexchange(keyExchange, algos)
       when keyExchange == :ecdh_ecdsa or
              keyExchange == :ecdhe_ecdsa do
    :proplists.get_bool(
      :ecdh,
      algos
    ) and :proplists.get_bool(:ecdsa, algos)
  end

  defp is_acceptable_keyexchange(keyExchange, algos)
       when keyExchange == :ecdh_rsa or
              keyExchange == :ecdhe_rsa do
    :proplists.get_bool(
      :ecdh,
      algos
    ) and :proplists.get_bool(:rsa, algos)
  end

  defp is_acceptable_keyexchange(keyExchange, algos)
       when keyExchange == :rsa or
              keyExchange == :rsa_psk do
    :proplists.get_bool(:rsa, algos)
  end

  defp is_acceptable_keyexchange(:srp_anon, algos) do
    :proplists.get_bool(:srp, algos)
  end

  defp is_acceptable_keyexchange(:srp_dss, algos) do
    :proplists.get_bool(
      :srp,
      algos
    ) and :proplists.get_bool(:dss, algos)
  end

  defp is_acceptable_keyexchange(:srp_rsa, algos) do
    :proplists.get_bool(
      :srp,
      algos
    ) and :proplists.get_bool(:rsa, algos)
  end

  defp is_acceptable_keyexchange(_KeyExchange, _Algos) do
    false
  end

  defp is_acceptable_cipher(:null, _Algos) do
    true
  end

  defp is_acceptable_cipher(:rc4_128, algos) do
    :proplists.get_bool(:rc4, algos)
  end

  defp is_acceptable_cipher(:"3des_ede_cbc", algos) do
    :proplists.get_bool(:des_ede3_cbc, algos)
  end

  defp is_acceptable_cipher(:aes_128_ccm_8, algos) do
    :proplists.get_bool(:aes_128_ccm, algos)
  end

  defp is_acceptable_cipher(:aes_256_ccm_8, algos) do
    :proplists.get_bool(:aes_256_ccm, algos)
  end

  defp is_acceptable_cipher(cipher, algos) do
    :proplists.get_bool(cipher, algos)
  end

  def is_acceptable_hash(:null, _Algos) do
    true
  end

  def is_acceptable_hash(:aead, _Algos) do
    true
  end

  def is_acceptable_hash(hash, algos) do
    :proplists.get_bool(hash, algos)
  end

  defp is_acceptable_prf(:default_prf, _) do
    true
  end

  defp is_acceptable_prf(prf, algos) do
    :proplists.get_bool(prf, algos)
  end

  def is_fallback(cipherSuites) do
    :lists.member(
      <<86::size(8)-unsigned-big-integer, 0::size(8)-unsigned-big-integer>>,
      cipherSuites
    )
  end

  def random_bytes(n) do
    :crypto.strong_rand_bytes(n)
  end

  def calc_mac_hash(type, version, plainFragment, %{
        sequence_number: seqNo,
        mac_secret: macSecret,
        security_parameters: r_security_parameters(mac_algorithm: macAlgorithm)
      }) do
    calc_mac_hash(type, version, plainFragment, macAlgorithm, macSecret, seqNo)
  end

  def calc_mac_hash(type, version, plainFragment, macAlgorithm, macSecret, seqNo) do
    length = :erlang.iolist_size(plainFragment)
    mac_hash(version, macAlgorithm, macSecret, seqNo, type, length, plainFragment)
  end

  def is_stream_ciphersuite(%{cipher: :rc4_128}) do
    true
  end

  def is_stream_ciphersuite(_) do
    false
  end

  def hash_size(:null) do
    0
  end

  def hash_size(:aead) do
    0
  end

  def hash_size(:md5) do
    16
  end

  def hash_size(:sha) do
    20
  end

  def hash_size(:sha256) do
    32
  end

  def hash_size(:sha384) do
    48
  end

  def hash_size(:sha512) do
    64
  end

  defp mac_hash({_, _}, 0, _MacSecret, _SeqNo, _Type, _Length, _Fragment) do
    <<>>
  end

  defp mac_hash({3, n} = version, macAlg, macSecret, seqNo, type, length, fragment)
       when n === 1 or n === 2 or n === 3 or n === 4 do
    :tls_v1.mac_hash(macAlg, macSecret, seqNo, type, version, length, fragment)
  end

  defp bulk_cipher_algorithm(:null) do
    0
  end

  defp bulk_cipher_algorithm(:rc4_128) do
    1
  end

  defp bulk_cipher_algorithm(:des_cbc) do
    3
  end

  defp bulk_cipher_algorithm(:"3des_ede_cbc") do
    4
  end

  defp bulk_cipher_algorithm(cipher)
       when cipher == :aes_128_cbc or
              cipher == :aes_256_cbc do
    7
  end

  defp bulk_cipher_algorithm(cipher)
       when cipher == :aes_128_gcm or
              cipher == :aes_256_gcm do
    8
  end

  defp bulk_cipher_algorithm(cipher)
       when cipher == :aes_128_ccm or
              cipher == :aes_256_ccm do
    10
  end

  defp bulk_cipher_algorithm(cipher)
       when cipher == :aes_128_ccm_8 or
              cipher == :aes_256_ccm_8 do
    11
  end

  defp bulk_cipher_algorithm(:chacha20_poly1305) do
    9
  end

  defp type(cipher)
       when cipher == :null or
              cipher == :rc4_128 do
    0
  end

  defp type(cipher)
       when cipher == :des_cbc or
              cipher == :"3des_ede_cbc" or cipher == :aes_128_cbc or
              cipher == :aes_256_cbc do
    1
  end

  defp type(cipher)
       when cipher == :aes_128_gcm or
              cipher == :aes_256_gcm or cipher == :aes_128_ccm or
              cipher == :aes_256_ccm or cipher == :aes_128_ccm_8 or
              cipher == :aes_256_ccm_8 or
              cipher == :chacha20_poly1305 do
    2
  end

  def key_material(:null) do
    0
  end

  def key_material(:rc4_128) do
    16
  end

  def key_material(:des_cbc) do
    8
  end

  def key_material(:"3des_ede_cbc") do
    24
  end

  def key_material(:aes_128_cbc) do
    16
  end

  def key_material(:aes_256_cbc) do
    32
  end

  def key_material(:aes_128_gcm) do
    16
  end

  def key_material(:aes_128_ccm) do
    16
  end

  def key_material(:aes_128_ccm_8) do
    16
  end

  def key_material(:aes_256_gcm) do
    32
  end

  def key_material(:aes_256_ccm_8) do
    32
  end

  def key_material(:aes_256_ccm) do
    32
  end

  def key_material(:chacha20_poly1305) do
    32
  end

  defp expanded_key_material(:null) do
    0
  end

  defp expanded_key_material(:rc4_128) do
    16
  end

  defp expanded_key_material(cipher) when cipher == :des_cbc do
    8
  end

  defp expanded_key_material(:"3des_ede_cbc") do
    24
  end

  defp expanded_key_material(cipher)
       when cipher == :aes_128_cbc or
              cipher == :aes_256_cbc or cipher == :aes_128_gcm or
              cipher == :aes_256_gcm or cipher == :aes_128_ccm or
              cipher == :aes_256_ccm or cipher == :aes_128_ccm_8 or
              cipher == :aes_256_ccm_8 or
              cipher == :chacha20_poly1305 do
    :unknown
  end

  def effective_key_bits(:null) do
    0
  end

  def effective_key_bits(:des_cbc) do
    56
  end

  def effective_key_bits(cipher)
      when cipher == :rc4_128 or
             cipher == :aes_128_cbc or cipher == :aes_128_gcm or
             cipher == :aes_128_ccm or cipher == :aes_128_ccm_8 do
    128
  end

  def effective_key_bits(:"3des_ede_cbc") do
    168
  end

  def effective_key_bits(cipher)
      when cipher == :aes_256_cbc or
             cipher == :aes_256_gcm or cipher == :aes_256_ccm or
             cipher == :aes_256_ccm_8 or
             cipher == :chacha20_poly1305 do
    256
  end

  defp iv_size(cipher)
       when cipher == :null or
              cipher == :rc4_128 do
    0
  end

  defp iv_size(cipher)
       when cipher == :aes_128_gcm or
              cipher == :aes_256_gcm or cipher == :aes_128_ccm or
              cipher == :aes_256_ccm or cipher == :aes_128_ccm_8 or
              cipher == :aes_256_ccm_8 do
    4
  end

  defp iv_size(:chacha20_poly1305) do
    12
  end

  defp iv_size(cipher) do
    block_size(cipher)
  end

  defp block_size(cipher)
       when cipher == :des_cbc or
              cipher == :des_ede3_cbc or cipher == :"3des_ede_cbc" do
    8
  end

  defp block_size(cipher)
       when cipher == :aes_128_cbc or
              cipher == :aes_256_cbc or cipher == :aes_128_gcm or
              cipher == :aes_256_gcm or cipher == :aes_128_ccm or
              cipher == :aes_256_ccm or cipher == :aes_128_ccm_8 or
              cipher == :aes_256_ccm_8 or
              cipher == :chacha20_poly1305 do
    16
  end

  defp prf_algorithm(:default_prf, {3, n}) when n >= 3 do
    4
  end

  defp prf_algorithm(:default_prf, {3, _}) do
    4711
  end

  defp prf_algorithm(algo, _) do
    hash_algorithm(algo)
  end

  defp mac_algorithm(:aead) do
    :aead
  end

  defp mac_algorithm(algo) do
    hash_algorithm(algo)
  end

  def hash_algorithm(:null) do
    0
  end

  def hash_algorithm(:md5) do
    1
  end

  def hash_algorithm(:sha) do
    2
  end

  def hash_algorithm(:sha224) do
    3
  end

  def hash_algorithm(:sha256) do
    4
  end

  def hash_algorithm(:sha384) do
    5
  end

  def hash_algorithm(:sha512) do
    6
  end

  def hash_algorithm(0) do
    :null
  end

  def hash_algorithm(1) do
    :md5
  end

  def hash_algorithm(2) do
    :sha
  end

  def hash_algorithm(3) do
    :sha224
  end

  def hash_algorithm(4) do
    :sha256
  end

  def hash_algorithm(5) do
    :sha384
  end

  def hash_algorithm(6) do
    :sha512
  end

  def hash_algorithm(other)
      when is_integer(other) and other >= 7 and other <= 223 do
    :unassigned
  end

  def hash_algorithm(other)
      when is_integer(other) and other >= 224 and other <= 255 do
    other
  end

  def sign_algorithm(:anon) do
    0
  end

  def sign_algorithm(:rsa) do
    1
  end

  def sign_algorithm(:dsa) do
    2
  end

  def sign_algorithm(:ecdsa) do
    3
  end

  def sign_algorithm(0) do
    :anon
  end

  def sign_algorithm(1) do
    :rsa
  end

  def sign_algorithm(2) do
    :dsa
  end

  def sign_algorithm(3) do
    :ecdsa
  end

  def sign_algorithm(other)
      when is_integer(other) and other >= 4 and other <= 223 do
    :unassigned
  end

  def sign_algorithm(other)
      when is_integer(other) and other >= 224 and other <= 255 do
    other
  end

  def signature_scheme(:rsa_pkcs1_sha256) do
    1025
  end

  def signature_scheme(:rsa_pkcs1_sha384) do
    1281
  end

  def signature_scheme(:rsa_pkcs1_sha512) do
    1537
  end

  def signature_scheme(:ecdsa_secp256r1_sha256) do
    1027
  end

  def signature_scheme(:ecdsa_secp384r1_sha384) do
    1283
  end

  def signature_scheme(:ecdsa_secp521r1_sha512) do
    1539
  end

  def signature_scheme(:rsa_pss_rsae_sha256) do
    2052
  end

  def signature_scheme(:rsa_pss_rsae_sha384) do
    2053
  end

  def signature_scheme(:rsa_pss_rsae_sha512) do
    2054
  end

  def signature_scheme(:ed25519) do
    2055
  end

  def signature_scheme(:ed448) do
    2056
  end

  def signature_scheme(:rsa_pss_pss_sha256) do
    2057
  end

  def signature_scheme(:rsa_pss_pss_sha384) do
    2058
  end

  def signature_scheme(:rsa_pss_pss_sha512) do
    2059
  end

  def signature_scheme(:rsa_pkcs1_sha1) do
    513
  end

  def signature_scheme(:ecdsa_sha1) do
    515
  end

  def signature_scheme({hash0, sign0}) do
    hash = hash_algorithm(hash0)
    sign = sign_algorithm(sign0)

    <<sigAlg::size(16)-unsigned-big-integer>> =
      <<hash::size(8)-unsigned-big-integer, sign::size(8)-unsigned-big-integer>>

    sigAlg
  end

  def signature_scheme(1025) do
    :rsa_pkcs1_sha256
  end

  def signature_scheme(1281) do
    :rsa_pkcs1_sha384
  end

  def signature_scheme(1537) do
    :rsa_pkcs1_sha512
  end

  def signature_scheme(1027) do
    :ecdsa_secp256r1_sha256
  end

  def signature_scheme(1283) do
    :ecdsa_secp384r1_sha384
  end

  def signature_scheme(1539) do
    :ecdsa_secp521r1_sha512
  end

  def signature_scheme(2052) do
    :rsa_pss_rsae_sha256
  end

  def signature_scheme(2053) do
    :rsa_pss_rsae_sha384
  end

  def signature_scheme(2054) do
    :rsa_pss_rsae_sha512
  end

  def signature_scheme(2055) do
    :ed25519
  end

  def signature_scheme(2056) do
    :ed448
  end

  def signature_scheme(2057) do
    :rsa_pss_pss_sha256
  end

  def signature_scheme(2058) do
    :rsa_pss_pss_sha384
  end

  def signature_scheme(2059) do
    :rsa_pss_pss_sha512
  end

  def signature_scheme(513) do
    :rsa_pkcs1_sha1
  end

  def signature_scheme(515) do
    :ecdsa_sha1
  end

  def signature_scheme(signAlgo) when is_integer(signAlgo) do
    <<hash::size(8)-unsigned-big-integer, sign::size(8)-unsigned-big-integer>> =
      <<signAlgo::size(16)-unsigned-big-integer>>

    {:ssl_cipher.hash_algorithm(hash), :ssl_cipher.sign_algorithm(sign)}
  end

  def signature_scheme(_) do
    :unassigned
  end

  def scheme_to_components(:rsa_pkcs1_sha256) do
    {:sha256, :rsa_pkcs1, :undefined}
  end

  def scheme_to_components(:rsa_pkcs1_sha384) do
    {:sha384, :rsa_pkcs1, :undefined}
  end

  def scheme_to_components(:rsa_pkcs1_sha512) do
    {:sha512, :rsa_pkcs1, :undefined}
  end

  def scheme_to_components(:ecdsa_secp256r1_sha256) do
    {:sha256, :ecdsa, :secp256r1}
  end

  def scheme_to_components(:ecdsa_secp384r1_sha384) do
    {:sha384, :ecdsa, :secp384r1}
  end

  def scheme_to_components(:ecdsa_secp521r1_sha512) do
    {:sha512, :ecdsa, :secp521r1}
  end

  def scheme_to_components(:rsa_pss_rsae_sha256) do
    {:sha256, :rsa_pss_rsae, :undefined}
  end

  def scheme_to_components(:rsa_pss_rsae_sha384) do
    {:sha384, :rsa_pss_rsae, :undefined}
  end

  def scheme_to_components(:rsa_pss_rsae_sha512) do
    {:sha512, :rsa_pss_rsae, :undefined}
  end

  def scheme_to_components(:ed25519) do
    {:undefined, :undefined, :undefined}
  end

  def scheme_to_components(:ed448) do
    {:undefined, :undefined, :undefined}
  end

  def scheme_to_components(:rsa_pss_pss_sha256) do
    {:sha256, :rsa_pss_pss, :undefined}
  end

  def scheme_to_components(:rsa_pss_pss_sha384) do
    {:sha384, :rsa_pss_pss, :undefined}
  end

  def scheme_to_components(:rsa_pss_pss_sha512) do
    {:sha512, :rsa_pss_pss, :undefined}
  end

  def scheme_to_components(:rsa_pkcs1_sha1) do
    {:sha1, :rsa_pkcs1, :undefined}
  end

  def scheme_to_components(:ecdsa_sha1) do
    {:sha1, :ecdsa, :undefined}
  end

  def scheme_to_components({hash, sign}) do
    {hash, sign, :undefined}
  end

  def signature_algorithm_to_scheme(
        r_SignatureAlgorithm(
          algorithm: {1, 2, 840, 113_549, 1, 1, 10},
          parameters:
            r_RSASSA_PSS_params(
              maskGenAlgorithm:
                r_MaskGenAlgorithm(
                  algorithm: {1, 2, 840, 113_549, 1, 1, 8},
                  parameters: hashAlgo
                )
            )
        )
      ) do
    r_HashAlgorithm(algorithm: hashOid) = hashAlgo

    case :public_key.pkix_hash_type(hashOid) do
      :sha256 ->
        :rsa_pss_pss_sha256

      :sha384 ->
        :rsa_pss_pss_sha384

      :sha512 ->
        :rsa_pss_pss_sha512
    end
  end

  def signature_algorithm_to_scheme(
        r_SignatureAlgorithm(algorithm: {1, 2, 840, 113_549, 1, 1, 11})
      ) do
    :rsa_pkcs1_sha256
  end

  def signature_algorithm_to_scheme(
        r_SignatureAlgorithm(algorithm: {1, 2, 840, 113_549, 1, 1, 12})
      ) do
    :rsa_pkcs1_sha384
  end

  def signature_algorithm_to_scheme(
        r_SignatureAlgorithm(algorithm: {1, 2, 840, 113_549, 1, 1, 13})
      ) do
    :rsa_pkcs1_sha512
  end

  def signature_algorithm_to_scheme(r_SignatureAlgorithm(algorithm: {1, 2, 840, 10045, 4, 3, 2})) do
    :ecdsa_secp256r1_sha256
  end

  def signature_algorithm_to_scheme(r_SignatureAlgorithm(algorithm: {1, 2, 840, 10045, 4, 3, 3})) do
    :ecdsa_secp384r1_sha384
  end

  def signature_algorithm_to_scheme(r_SignatureAlgorithm(algorithm: {1, 2, 840, 10045, 4, 3, 4})) do
    :ecdsa_secp512r1_sha512
  end

  def signature_algorithm_to_scheme(r_SignatureAlgorithm(algorithm: {1, 3, 14, 3, 2, 29})) do
    :rsa_pkcs1_sha1
  end

  def signature_algorithm_to_scheme(
        r_SignatureAlgorithm(algorithm: {1, 2, 840, 113_549, 1, 1, 5})
      ) do
    :rsa_pkcs1_sha1
  end

  def signature_algorithm_to_scheme(r_SignatureAlgorithm(algorithm: {1, 2, 840, 10045, 4, 1})) do
    :ecdsa_sha1
  end

  def signature_algorithm_to_scheme(r_SignatureAlgorithm(algorithm: {1, 3, 101, 112})) do
    :eddsa_ed25519
  end

  def signature_algorithm_to_scheme(r_SignatureAlgorithm(algorithm: {1, 3, 101, 113})) do
    :eddsa_ed448
  end

  def signature_algorithm_to_scheme(
        r_SignatureAlgorithm(
          algorithm: {1, 2, 840, 113_549, 1, 1, 1},
          parameters: 0
        )
      ) do
    :rsa_pkcs1_sha1
  end

  def signature_algorithm_to_scheme(
        r_SignatureAlgorithm(algorithm: {1, 2, 840, 113_549, 1, 1, 1})
      ) do
    :rsa_pss_rsae
  end

  def signature_algorithm_to_scheme(
        r_SignatureAlgorithm(algorithm: {1, 2, 840, 113_549, 1, 1, 10})
      ) do
    :rsa_pss_pss
  end

  defp generic_block_cipher_from_bin({3, n}, t, iV, hashSize)
       when n == 0 or
              n == 1 do
    sz1 = byte_size(t) - 1
    <<_::size(sz1)-binary, padLength0::size(8)-unsigned-big-integer>> = t

    padLength =
      cond do
        padLength0 >= sz1 ->
          0

        true ->
          padLength0
      end

    compressedLength = byte_size(t) - padLength - 1 - hashSize

    <<content::size(compressedLength)-binary, mac::size(hashSize)-binary,
      padding::size(padLength)-binary, ^padLength0::size(8)-unsigned-big-integer>> = t

    r_generic_block_cipher(
      content: content,
      mac: mac,
      padding: padding,
      padding_length: padLength0,
      next_iv: iV
    )
  end

  defp generic_block_cipher_from_bin({3, n}, t, iV, hashSize)
       when n == 2 or
              n == 3 or n == 4 do
    sz1 = byte_size(t) - 1
    <<_::size(sz1)-binary, padLength::size(8)-unsigned-big-integer>> = t
    iVLength = byte_size(iV)
    compressedLength = byte_size(t) - iVLength - padLength - 1 - hashSize

    <<nextIV::size(iVLength)-binary, content::size(compressedLength)-binary,
      mac::size(hashSize)-binary, padding::size(padLength)-binary,
      ^padLength::size(8)-unsigned-big-integer>> = t

    r_generic_block_cipher(
      content: content,
      mac: mac,
      padding: padding,
      padding_length: padLength,
      next_iv: nextIV
    )
  end

  defp generic_stream_cipher_from_bin(t, hashSz) do
    sz = byte_size(t)
    compressedLength = sz - hashSz
    <<content::size(compressedLength)-binary, mac::size(hashSz)-binary>> = t
    r_generic_stream_cipher(content: content, mac: mac)
  end

  defp is_correct_padding(
         r_generic_block_cipher(padding_length: len, padding: padding),
         {3, 0},
         _
       ) do
    len == byte_size(padding)
  end

  defp is_correct_padding(genBlockCipher, {3, 1}, false) do
    is_correct_padding(genBlockCipher, {3, 0}, false)
  end

  defp is_correct_padding(r_generic_block_cipher(padding_length: len, padding: padding), _, _) do
    len == byte_size(padding) and padding(len) == padding
  end

  defp padding(padLen) do
    case padLen do
      0 ->
        <<>>

      1 ->
        <<1>>

      2 ->
        <<2, 2>>

      3 ->
        <<3, 3, 3>>

      4 ->
        <<4, 4, 4, 4>>

      5 ->
        <<5, 5, 5, 5, 5>>

      6 ->
        <<6, 6, 6, 6, 6, 6>>

      7 ->
        <<7, 7, 7, 7, 7, 7, 7>>

      8 ->
        <<8, 8, 8, 8, 8, 8, 8, 8>>

      9 ->
        <<9, 9, 9, 9, 9, 9, 9, 9, 9>>

      10 ->
        <<10, 10, 10, 10, 10, 10, 10, 10, 10, 10>>

      11 ->
        <<11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11>>

      12 ->
        <<12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12>>

      13 ->
        <<13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13>>

      14 ->
        <<14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14>>

      15 ->
        <<15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15>>

      _ ->
        :binary.copy(<<padLen>>, padLen)
    end
  end

  defp padding_with_len(textLen, blockSize) do
    case blockSize - rem(textLen, blockSize) do
      0 ->
        <<0>>

      1 ->
        <<1, 1>>

      2 ->
        <<2, 2, 2>>

      3 ->
        <<3, 3, 3, 3>>

      4 ->
        <<4, 4, 4, 4, 4>>

      5 ->
        <<5, 5, 5, 5, 5, 5>>

      6 ->
        <<6, 6, 6, 6, 6, 6, 6>>

      7 ->
        <<7, 7, 7, 7, 7, 7, 7, 7>>

      8 ->
        <<8, 8, 8, 8, 8, 8, 8, 8, 8>>

      9 ->
        <<9, 9, 9, 9, 9, 9, 9, 9, 9, 9>>

      10 ->
        <<10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10>>

      11 ->
        <<11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11>>

      12 ->
        <<12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12>>

      13 ->
        <<13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13>>

      14 ->
        <<14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14>>

      15 ->
        <<15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15>>

      padLen ->
        :binary.copy(<<padLen>>, padLen + 1)
    end
  end

  defp next_iv(bin, iV) do
    binSz = byte_size(bin)
    iVSz = byte_size(iV)
    firstPart = binSz - iVSz
    <<_::size(firstPart)-binary, nextIV::size(iVSz)-binary>> = bin
    nextIV
  end

  defp filter_suites_pubkey(:rsa, ciphersSuites0, _Version, otpCert) do
    keyUses = key_uses(otpCert)

    notECDSAKeyed =
      (ciphersSuites0 -- ec_keyed_suites(ciphersSuites0)) -- dss_keyed_suites(ciphersSuites0)

    ciphersSuites =
      filter_keyuse_suites(
        :keyEncipherment,
        keyUses,
        notECDSAKeyed,
        rsa_suites_encipher(ciphersSuites0)
      )

    filter_keyuse_suites(
      :digitalSignature,
      keyUses,
      ciphersSuites,
      rsa_ecdhe_dhe_suites(ciphersSuites)
    )
  end

  defp filter_suites_pubkey(:dsa, ciphers, _, otpCert) do
    keyUses = key_uses(otpCert)
    notECRSAKeyed = (ciphers -- rsa_keyed_suites(ciphers)) -- ec_keyed_suites(ciphers)
    filter_keyuse_suites(:digitalSignature, keyUses, notECRSAKeyed, dss_dhe_suites(ciphers))
  end

  defp filter_suites_pubkey(:ec, ciphers, _, otpCert) do
    uses = key_uses(otpCert)
    notRSADSAKeyed = (ciphers -- rsa_keyed_suites(ciphers)) -- dss_keyed_suites(ciphers)

    ciphersSuites =
      filter_keyuse_suites(:digitalSignature, uses, notRSADSAKeyed, ec_ecdhe_suites(ciphers))

    filter_keyuse_suites(:keyAgreement, uses, ciphersSuites, ec_ecdh_suites(ciphers))
  end

  defp filter_suites_signature(_, ciphers, {3, n}) when n >= 3 do
    ciphers
  end

  defp filter_suites_signature(:rsa, ciphers, version) do
    (ciphers --
       ecdsa_signed_suites(
         ciphers,
         version
       )) --
      dsa_signed_suites(
        ciphers,
        version
      )
  end

  defp filter_suites_signature(:dsa, ciphers, version) do
    (ciphers --
       ecdsa_signed_suites(
         ciphers,
         version
       )) --
      rsa_signed_suites(
        ciphers,
        version
      )
  end

  defp filter_suites_signature(:ecdsa, ciphers, version) do
    (ciphers --
       rsa_signed_suites(
         ciphers,
         version
       )) --
      dsa_signed_suites(
        ciphers,
        version
      )
  end

  defp rsa_signed({3, n}) when n >= 3 do
    fn
      :rsa ->
        true

      :dhe_rsa ->
        true

      :ecdhe_rsa ->
        true

      :rsa_psk ->
        true

      :srp_rsa ->
        true

      _ ->
        false
    end
  end

  defp rsa_signed(_) do
    fn
      :rsa ->
        true

      :dhe_rsa ->
        true

      :ecdhe_rsa ->
        true

      :ecdh_rsa ->
        true

      :rsa_psk ->
        true

      :srp_rsa ->
        true

      _ ->
        false
    end
  end

  defp rsa_signed_suites(ciphers, version) do
    filter_suites(
      ciphers,
      %{
        key_exchange_filters: [rsa_signed(version)],
        cipher_filters: [],
        mac_filters: [],
        prf_filters: []
      }
    )
  end

  defp ecdsa_signed({3, n}) when n >= 3 do
    fn
      :ecdhe_ecdsa ->
        true

      _ ->
        false
    end
  end

  defp ecdsa_signed(_) do
    fn
      :ecdhe_ecdsa ->
        true

      :ecdh_ecdsa ->
        true

      _ ->
        false
    end
  end

  defp ecdsa_signed_suites(ciphers, version) do
    filter_suites(
      ciphers,
      %{
        key_exchange_filters: [ecdsa_signed(version)],
        cipher_filters: [],
        mac_filters: [],
        prf_filters: []
      }
    )
  end

  defp rsa_keyed(:dhe_rsa) do
    true
  end

  defp rsa_keyed(:ecdhe_rsa) do
    true
  end

  defp rsa_keyed(:rsa) do
    true
  end

  defp rsa_keyed(:rsa_psk) do
    true
  end

  defp rsa_keyed(:srp_rsa) do
    true
  end

  defp rsa_keyed(_) do
    false
  end

  defp rsa_keyed_suites(ciphers) do
    filter_suites(
      ciphers,
      %{
        key_exchange_filters: [
          fn kex ->
            rsa_keyed(kex)
          end
        ],
        cipher_filters: [],
        mac_filters: [],
        prf_filters: []
      }
    )
  end

  defp rsa_suites_encipher(ciphers) do
    filter_suites(
      ciphers,
      %{
        key_exchange_filters: [
          fn
            :rsa ->
              true

            :rsa_psk ->
              true

            _ ->
              false
          end
        ],
        cipher_filters: [],
        mac_filters: [],
        prf_filters: []
      }
    )
  end

  defp dss_keyed(:dhe_dss) do
    true
  end

  defp dss_keyed(:spr_dss) do
    true
  end

  defp dss_keyed(_) do
    false
  end

  defp dss_keyed_suites(ciphers) do
    filter_suites(
      ciphers,
      %{
        key_exchange_filters: [
          fn kex ->
            dss_keyed(kex)
          end
        ],
        cipher_filters: [],
        mac_filters: [],
        prf_filters: []
      }
    )
  end

  defp dsa_signed_suites(ciphers, version) do
    filter_suites(
      ciphers,
      %{
        key_exchange_filters: [dsa_signed(version)],
        cipher_filters: [],
        mac_filters: [],
        prf_filters: []
      }
    )
  end

  defp dsa_signed(_) do
    fn
      :dhe_dss ->
        true

      _ ->
        false
    end
  end

  defp dss_dhe_suites(ciphers) do
    filter_suites(
      ciphers,
      %{
        key_exchange_filters: [
          fn
            :dhe_dss ->
              true

            _ ->
              false
          end
        ],
        cipher_filters: [],
        mac_filters: [],
        prf_filters: []
      }
    )
  end

  defp ec_keyed(:ecdh_ecdsa) do
    true
  end

  defp ec_keyed(:ecdh_rsa) do
    true
  end

  defp ec_keyed(:ecdhe_ecdsa) do
    true
  end

  defp ec_keyed(_) do
    false
  end

  defp ec_keyed_suites(ciphers) do
    filter_suites(
      ciphers,
      %{
        key_exchange_filters: [
          fn kex ->
            ec_keyed(kex)
          end
        ],
        cipher_filters: [],
        mac_filters: [],
        prf_filters: []
      }
    )
  end

  defp ec_ecdh_suites(ciphers) do
    filter_suites(
      ciphers,
      %{
        key_exchange_filters: [
          fn
            :ecdh_ecdsa ->
              true

            _ ->
              false
          end
        ],
        cipher_filters: [],
        mac_filters: [],
        prf_filters: []
      }
    )
  end

  defp ec_ecdhe_suites(ciphers) do
    filter_suites(
      ciphers,
      %{
        key_exchange_filters: [
          fn
            :ecdhe_ecdsa ->
              true

            :ecdhe_rsa ->
              true

            _ ->
              false
          end
        ],
        cipher_filters: [],
        mac_filters: [],
        prf_filters: []
      }
    )
  end

  defp rsa_ecdhe_dhe_suites(ciphers) do
    filter_suites(
      ciphers,
      %{
        key_exchange_filters: [
          fn
            :dhe_rsa ->
              true

            :ecdhe_rsa ->
              true

            _ ->
              false
          end
        ],
        cipher_filters: [],
        mac_filters: [],
        prf_filters: []
      }
    )
  end

  defp key_uses(otpCert) do
    tBSCert = r_OTPCertificate(otpCert, :tbsCertificate)
    tBSExtensions = r_OTPTBSCertificate(tBSCert, :extensions)
    extensions = :ssl_certificate.extensions_list(tBSExtensions)

    case :ssl_certificate.select_extension(
           {2, 5, 29, 15},
           extensions
         ) do
      :undefined ->
        []

      r_Extension(extnValue: keyUses) ->
        keyUses
    end
  end

  defp filter_keyuse_suites(_, [], ciphersSuites, _) do
    ciphersSuites
  end

  defp filter_keyuse_suites(use, keyUse, cipherSuits, suites) do
    case :ssl_certificate.is_valid_key_usage(
           keyUse,
           use
         ) do
      true ->
        cipherSuits

      false ->
        cipherSuits -- suites
    end
  end

  def generate_server_share(group) do
    key = generate_key_exchange(group)
    r_key_share_server_hello(server_share: r_key_share_entry(group: group, key_exchange: key))
  end

  def generate_client_shares([]) do
    r_key_share_client_hello(client_shares: [])
  end

  def generate_client_shares(groups) do
    generate_client_shares(groups, [])
  end

  defp generate_client_shares([], acc) do
    r_key_share_client_hello(client_shares: :lists.reverse(acc))
  end

  defp generate_client_shares([group | groups], acc) do
    key = generate_key_exchange(group)
    keyShareEntry = r_key_share_entry(group: group, key_exchange: key)
    generate_client_shares(groups, [keyShareEntry | acc])
  end

  defp generate_key_exchange(:secp256r1) do
    :public_key.generate_key({:namedCurve, :secp256r1})
  end

  defp generate_key_exchange(:secp384r1) do
    :public_key.generate_key({:namedCurve, :secp384r1})
  end

  defp generate_key_exchange(:secp521r1) do
    :public_key.generate_key({:namedCurve, :secp521r1})
  end

  defp generate_key_exchange(:x25519) do
    :crypto.generate_key(:ecdh, :x25519)
  end

  defp generate_key_exchange(:x448) do
    :crypto.generate_key(:ecdh, :x448)
  end

  defp generate_key_exchange(fFDHE) do
    :public_key.generate_key(:ssl_dh_groups.dh_params(fFDHE))
  end

  def add_zero_padding(bin, primeSize)
      when byte_size(bin) === primeSize do
    bin
  end

  def add_zero_padding(bin, primeSize) do
    add_zero_padding(<<0, bin::binary>>, primeSize)
  end

  def encrypt_ticket(
        r_stateless_ticket(
          hash: hash,
          pre_shared_key: pSK,
          ticket_age_add: ticketAgeAdd,
          lifetime: lifetime,
          timestamp: timestamp
        ),
        shard,
        iV
      ) do
    plaintext =
      <<:ssl_cipher.hash_algorithm(hash)::size(8), pSK::binary,
        ticketAgeAdd::size(64)-unsigned-big-integer, lifetime::size(32)-unsigned-big-integer,
        timestamp::size(32)-unsigned-big-integer>>

    encrypt_ticket_data(plaintext, shard, iV)
  end

  def decrypt_ticket(cipherFragment, shard, iV) do
    case decrypt_ticket_data(cipherFragment, shard, iV) do
      :error ->
        :error

      plaintext ->
        <<hKDF::size(8)-unsigned-big-integer, t::binary>> = plaintext
        hash = hash_algorithm(hKDF)
        hashSize = hash_size(hash)

        <<pSK::size(hashSize)-binary, ticketAgeAdd::size(64)-unsigned-big-integer,
          lifetime::size(32)-unsigned-big-integer, timestamp::size(32)-unsigned-big-integer,
          _::binary>> = t

        r_stateless_ticket(
          hash: hash,
          pre_shared_key: pSK,
          ticket_age_add: ticketAgeAdd,
          lifetime: lifetime,
          timestamp: timestamp
        )
    end
  end

  defp encrypt_ticket_data(plaintext, shard, iV) do
    aAD =
      additional_data(
        "ticket",
        :erlang.iolist_size(plaintext) + 16
      )

    {oTP, key} = make_otp_key(shard)

    {content, cipherTag} =
      :crypto.crypto_one_time_aead(:aes_256_gcm, key, iV, plaintext, aAD, 16, true)

    <<content::binary, cipherTag::binary, oTP::binary>>
  end

  defp decrypt_ticket_data(cipherFragment, shard, iV) do
    size = byte_size(shard)

    aAD =
      additional_data(
        "ticket",
        :erlang.iolist_size(cipherFragment) - size
      )

    len = byte_size(cipherFragment) - size - 16

    case cipherFragment do
      <<encrypted::size(len)-binary, cipherTag::size(16)-binary, oTP::size(size)-binary>> ->
        key = :crypto.exor(oTP, shard)
        :crypto.crypto_one_time_aead(:aes_256_gcm, key, iV, encrypted, aAD, cipherTag, false)

      _ ->
        :error
    end
  end

  def encrypt_data(aDTag, plaintext, shard, iV) do
    aAD =
      additional_data(
        aDTag,
        :erlang.iolist_size(plaintext) + 16
      )

    {oTP, key} = make_otp_key(shard)

    {content, cipherTag} =
      :crypto.crypto_one_time_aead(:aes_256_gcm, key, iV, plaintext, aAD, 16, true)

    <<content::binary, cipherTag::binary, oTP::binary>>
  end

  def decrypt_data(aDTag, cipherFragment, shard, iV) do
    size = byte_size(shard)

    aAD =
      additional_data(
        aDTag,
        :erlang.iolist_size(cipherFragment) - size
      )

    len = byte_size(cipherFragment) - size - 16

    <<encrypted::size(len)-binary, cipherTag::size(16)-binary, oTP::size(size)-binary>> =
      cipherFragment

    key = :crypto.exor(oTP, shard)
    :crypto.crypto_one_time_aead(:aes_256_gcm, key, iV, encrypted, aAD, cipherTag, false)
  end

  defp additional_data(tag, length) do
    <<tag::binary, length::size(16)-unsigned-big-integer>>
  end

  defp make_otp_key(shard) do
    size = byte_size(shard)
    oTP = :crypto.strong_rand_bytes(size)
    key = :crypto.exor(oTP, shard)
    {oTP, key}
  end
end
