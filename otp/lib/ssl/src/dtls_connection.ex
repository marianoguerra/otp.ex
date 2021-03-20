defmodule :m_dtls_connection do
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

  Record.defrecord(:r_alert, :alert,
    level: :undefined,
    description: :undefined,
    where: :undefined,
    role: :undefined,
    reason: :undefined
  )

  Record.defrecord(:r_ssl_tls, :ssl_tls,
    type: :undefined,
    version: :undefined,
    fragment: :undefined,
    epoch: :undefined,
    sequence_number: :undefined
  )

  def start_fsm(
        role,
        host,
        port,
        socket,
        {%{erl_dist: false}, _, tracker} = opts,
        user,
        {cbModule, _, _, _, _} = cbInfo,
        timeout
      ) do
    try do
      {:ok, pid} =
        :dtls_connection_sup.start_child([role, host, port, socket, opts, user, cbInfo])

      {:ok, sslSocket} =
        :ssl_connection.socket_control(:dtls_connection, socket, [pid], cbModule, tracker)

      :ssl_connection.handshake(sslSocket, timeout)
    catch
      :error, {:badmatch, {:error, _} = error} ->
        error
    end
  end

  def start_link(role, host, port, socket, options, user, cbInfo) do
    {:ok,
     :proc_lib.spawn_link(:dtls_connection, :init, [
       [role, host, port, socket, options, user, cbInfo]
     ])}
  end

  def init([role, host, port, socket, options, user, cbInfo]) do
    :erlang.process_flag(:trap_exit, true)

    state0 =
      r_state(protocol_specific: map) =
      initial_state(role, host, port, socket, options, user, cbInfo)

    try do
      state = :ssl_connection.ssl_config(r_state(state0, :ssl_options), role, state0)
      :gen_statem.enter_loop(:dtls_connection, [], :init, state)
    catch
      error ->
        eState = r_state(state0, protocol_specific: Map.put(map, :error, error))
        :gen_statem.enter_loop(:dtls_connection, [], :error, eState)
    end
  end

  def pids(_) do
    [self()]
  end

  defp next_record(
         r_state(handshake_env: r_handshake_env(unprocessed_handshake_events: n) = hsEnv) = state
       )
       when n > 0 do
    {:no_record,
     r_state(state, handshake_env: r_handshake_env(hsEnv, unprocessed_handshake_events: n - 1))}
  end

  defp next_record(
         r_state(
           protocol_buffers:
             r_protocol_buffers(
               dtls_cipher_texts: [
                 r_ssl_tls(epoch: epoch) = cT
                 | rest
               ]
             ) = buffers,
           connection_states: %{current_read: %{epoch: epoch}} = connectionStates
         ) = state
       ) do
    currentRead =
      :dtls_record.get_connection_state_by_epoch(
        epoch,
        connectionStates,
        :read
      )

    case :dtls_record.replay_detect(cT, currentRead) do
      false ->
        decode_cipher_text(r_state(state, connection_states: connectionStates))

      true ->
        next_record(
          r_state(state,
            protocol_buffers: r_protocol_buffers(buffers, dtls_cipher_texts: rest),
            connection_states: connectionStates
          )
        )
    end
  end

  defp next_record(
         r_state(
           protocol_buffers:
             r_protocol_buffers(
               dtls_cipher_texts: [
                 r_ssl_tls(epoch: epoch)
                 | rest
               ]
             ) = buffers,
           connection_states: %{current_read: %{epoch: currentEpoch}} = connectionStates
         ) = state
       )
       when epoch > currentEpoch do
    next_record(
      r_state(state,
        protocol_buffers: r_protocol_buffers(buffers, dtls_cipher_texts: rest),
        connection_states: connectionStates
      )
    )
  end

  defp next_record(
         r_state(
           protocol_buffers:
             r_protocol_buffers(
               dtls_cipher_texts: [
                 _
                 | rest
               ]
             ) = buffers,
           connection_states: connectionStates
         ) = state
       ) do
    next_record(
      r_state(state,
        protocol_buffers: r_protocol_buffers(buffers, dtls_cipher_texts: rest),
        connection_states: connectionStates
      )
    )
  end

  defp next_record(
         r_state(
           static_env:
             r_static_env(
               role: :server,
               socket: {listener, {client, _}}
             )
         ) = state
       ) do
    :dtls_packet_demux.active_once(listener, client, self())
    {:no_record, state}
  end

  defp next_record(
         r_state(
           protocol_specific: %{active_n_toggle: true, active_n: n} = protocolSpec,
           static_env:
             r_static_env(
               role: :client,
               socket: {_Server, socket} = dTLSSocket,
               close_tag: closeTag,
               transport_cb: transport
             )
         ) = state
       ) do
    case :dtls_socket.setopts(transport, socket, [{:active, n}]) do
      :ok ->
        {:no_record,
         r_state(state, protocol_specific: Map.put(protocolSpec, :active_n_toggle, false))}

      _ ->
        send(self(), {closeTag, dTLSSocket})
        {:no_record, state}
    end
  end

  defp next_record(state) do
    {:no_record, state}
  end

  def next_event(stateName, record, state) do
    next_event(stateName, record, state, [])
  end

  def next_event(
        stateName,
        :no_record,
        r_state(connection_states: %{current_read: %{epoch: currentEpoch}}) = state0,
        actions
      ) do
    case next_record(state0) do
      {:no_record, state} ->
        :ssl_connection.hibernate_after(stateName, state, actions)

      {r_ssl_tls(epoch: ^currentEpoch, type: 22, version: version) = record, state1} ->
        state = dtls_version(stateName, version, state1)

        {:next_state, stateName, state,
         [
           {:next_event, :internal, {:protocol_record, record}}
           | actions
         ]}

      {r_ssl_tls(epoch: ^currentEpoch) = record, state} ->
        {:next_state, stateName, state,
         [
           {:next_event, :internal, {:protocol_record, record}}
           | actions
         ]}

      {r_ssl_tls(epoch: epoch, type: 22, version: _Version), state1} = _Record
      when epoch == currentEpoch - 1 ->
        {state, moreActions} =
          send_handshake_flight(
            state1,
            currentEpoch
          )

        next_event(stateName, :no_record, state, actions ++ moreActions)

      {r_ssl_tls(epoch: epoch, type: 20, version: _Version), state1} = _Record
      when epoch == currentEpoch - 1 ->
        {state, moreActions} =
          send_handshake_flight(
            state1,
            currentEpoch
          )

        next_event(stateName, :no_record, state, actions ++ moreActions)

      {r_ssl_tls(epoch: _Epoch, version: _Version), state} ->
        next_event(stateName, :no_record, state, actions)

      {r_alert() = alert, state} ->
        version = r_connection_env(r_state(state, :connection_env), :negotiated_version)
        handle_own_alert(alert, version, stateName, state)
    end
  end

  def next_event(
        :connection = stateName,
        record,
        r_state(connection_states: %{current_read: %{epoch: currentEpoch}}) = state0,
        actions
      ) do
    case record do
      r_ssl_tls(epoch: ^currentEpoch, type: 22, version: version) = ^record ->
        state = dtls_version(stateName, version, state0)

        {:next_state, stateName, state,
         [
           {:next_event, :internal, {:protocol_record, record}}
           | actions
         ]}

      r_ssl_tls(epoch: ^currentEpoch) ->
        {:next_state, stateName, state0,
         [
           {:next_event, :internal, {:protocol_record, record}}
           | actions
         ]}

      r_ssl_tls(epoch: epoch, type: 22, version: _Version)
      when epoch == currentEpoch - 1 ->
        {state, moreActions} =
          send_handshake_flight(
            state0,
            currentEpoch
          )

        next_event(stateName, :no_record, state, actions ++ moreActions)

      r_ssl_tls(epoch: epoch, type: 20, version: _Version)
      when epoch == currentEpoch - 1 ->
        {state, moreActions} =
          send_handshake_flight(
            state0,
            currentEpoch
          )

        next_event(stateName, :no_record, state, actions ++ moreActions)

      _ ->
        next_event(stateName, :no_record, state0, actions)
    end
  end

  def next_event(
        stateName,
        record,
        r_state(connection_states: %{current_read: %{epoch: currentEpoch}}) = state0,
        actions
      ) do
    case record do
      r_ssl_tls(epoch: ^currentEpoch, version: version) = ^record ->
        state = dtls_version(stateName, version, state0)

        {:next_state, stateName, state,
         [
           {:next_event, :internal, {:protocol_record, record}}
           | actions
         ]}

      r_ssl_tls(epoch: _Epoch, version: _Version) = _Record ->
        next_event(stateName, :no_record, state0, actions)

      r_alert() = alert ->
        version = r_connection_env(r_state(state0, :connection_env), :negotiated_version)
        handle_own_alert(alert, version, stateName, state0)
    end
  end

  def handle_protocol_record(r_ssl_tls(type: 23, fragment: data), stateName0, state0) do
    case :ssl_connection.read_application_data(
           data,
           state0
         ) do
      {:stop, _, _} = stop ->
        stop

      {record, state1} ->
        {:next_state, stateName, state, actions} = next_event(stateName0, record, state1)
        :ssl_connection.hibernate_after(stateName, state, actions)
    end
  end

  def handle_protocol_record(
        r_ssl_tls(type: 22, fragment: data),
        stateName,
        r_state(
          protocol_buffers: buffers0,
          connection_env: r_connection_env(negotiated_version: version),
          ssl_options: options
        ) = state
      ) do
    try do
      case :dtls_handshake.get_dtls_handshake(version, data, buffers0, options) do
        {[], buffers} ->
          next_event(stateName, :no_record, r_state(state, protocol_buffers: buffers))

        {packets, buffers} ->
          hsEnv = r_state(state, :handshake_env)
          events = dtls_handshake_events(packets)

          {:next_state, stateName,
           r_state(state,
             protocol_buffers: buffers,
             handshake_env:
               r_handshake_env(hsEnv, unprocessed_handshake_events: unprocessed_events(events))
           ), events}
      end
    catch
      r_alert() = alert ->
        handle_own_alert(alert, version, stateName, state)
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
    case decode_alerts(encAlerts) do
      alerts = [_ | _] ->
        handle_alerts(alerts, {:next_state, stateName, state})

      r_alert() = alert ->
        handle_own_alert(alert, version, stateName, state)
    end
  end

  def handle_protocol_record(r_ssl_tls(type: _Unknown), stateName, state) do
    {:next_state, stateName, state, []}
  end

  def renegotiate(
        r_state(static_env: r_static_env(role: :client)) = state0,
        actions
      ) do
    state = reinit_handshake_data(state0)
    {:next_state, :connection, state, [{:next_event, :internal, r_hello_request()} | actions]}
  end

  def renegotiate(
        r_state(static_env: r_static_env(role: :server)) = state0,
        actions
      ) do
    helloRequest = :ssl_handshake.hello_request()
    state1 = prepare_flight(state0)

    {state, moreActions} =
      send_handshake(
        helloRequest,
        state1
      )

    next_event(:hello, :no_record, state, actions ++ moreActions)
  end

  def send_handshake(
        handshake,
        r_state(connection_states: connectionStates) = state
      ) do
    %{epoch: epoch} =
      :ssl_record.current_connection_state(
        connectionStates,
        :write
      )

    send_handshake_flight(
      queue_handshake(handshake, state),
      epoch
    )
  end

  def queue_handshake(
        handshake0,
        r_state(
          handshake_env: r_handshake_env(tls_handshake_history: hist0) = hsEnv,
          connection_env: r_connection_env(negotiated_version: version),
          flight_buffer:
            %{handshakes: hsBuffer0, change_cipher_spec: :undefined, next_sequence: seq} = flight0,
          ssl_options: %{log_level: logLevel}
        ) = state
      ) do
    handshake = :dtls_handshake.encode_handshake(handshake0, version, seq)
    hist = update_handshake_history(handshake0, handshake, hist0)
    :ssl_logger.debug(logLevel, :outbound, :handshake, handshake0)

    r_state(state,
      flight_buffer:
        Map.merge(flight0, %{handshakes: [handshake | hsBuffer0], next_sequence: seq + 1}),
      handshake_env: r_handshake_env(hsEnv, tls_handshake_history: hist)
    )
  end

  def queue_handshake(
        handshake0,
        r_state(
          handshake_env: r_handshake_env(tls_handshake_history: hist0) = hsEnv,
          connection_env: r_connection_env(negotiated_version: version),
          flight_buffer:
            %{handshakes_after_change_cipher_spec: buffer0, next_sequence: seq} = flight0,
          ssl_options: %{log_level: logLevel}
        ) = state
      ) do
    handshake = :dtls_handshake.encode_handshake(handshake0, version, seq)
    hist = update_handshake_history(handshake0, handshake, hist0)
    :ssl_logger.debug(logLevel, :outbound, :handshake, handshake0)

    r_state(state,
      flight_buffer:
        Map.merge(flight0, %{
          handshakes_after_change_cipher_spec: [handshake | buffer0],
          next_sequence: seq + 1
        }),
      handshake_env: r_handshake_env(hsEnv, tls_handshake_history: hist)
    )
  end

  def queue_change_cipher(
        changeCipher,
        r_state(
          flight_buffer: flight,
          connection_states: connectionStates0
        ) = state
      ) do
    connectionStates =
      :dtls_record.next_epoch(
        connectionStates0,
        :write
      )

    r_state(state,
      flight_buffer: Map.put(flight, :change_cipher_spec, changeCipher),
      connection_states: connectionStates
    )
  end

  def reinit(state) do
    reinit_handshake_data(state)
  end

  def reinit_handshake_data(
        r_state(
          static_env: r_static_env(data_tag: dataTag),
          protocol_buffers: buffers,
          protocol_specific: pS,
          handshake_env: hsEnv
        ) = state
      ) do
    r_state(state,
      handshake_env:
        r_handshake_env(hsEnv,
          tls_handshake_history: :ssl_handshake.init_handshake_history(),
          public_key_info: :undefined,
          premaster_secret: :undefined
        ),
      protocol_specific: Map.put(pS, :flight_state, initial_flight_state(dataTag)),
      flight_buffer: new_flight(),
      protocol_buffers:
        r_protocol_buffers(buffers,
          dtls_handshake_next_seq: 0,
          dtls_handshake_next_fragments: [],
          dtls_handshake_later_fragments: []
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
    empty =
      :ssl_record.empty_connection_state(
        connectionEnd,
        beastMitigation
      )

    :dtls_record.empty_connection_state(empty)
  end

  def encode_alert(r_alert() = alert, version, connectionStates) do
    :dtls_record.encode_alert_record(alert, version, connectionStates)
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
          connection_states: connectionStates0,
          ssl_options: %{log_level: logLevel}
        ) = state0
      ) do
    {binMsg, connectionStates} = encode_alert(alert, version, connectionStates0)
    send(transport, socket, binMsg)
    :ssl_logger.debug(logLevel, :outbound, :record, binMsg)
    r_state(state0, connection_states: connectionStates)
  end

  def send_alert_in_connection(alert, state) do
    _ = send_alert(alert, state)
    :ok
  end

  def close(:downgrade, _, _, _, _) do
    :ok
  end

  def close(_, socket, transport, _, _) do
    :dtls_socket.close(transport, socket)
  end

  def protocol_name() do
    'DTLS'
  end

  defp send(transport, {listener, socket}, data)
       when is_pid(listener) do
    :dtls_socket.send(transport, socket, data)
  end

  defp send(transport, socket, data) do
    :dtls_socket.send(transport, socket, data)
  end

  def socket(pid, transport, socket, _Tracker) do
    :dtls_socket.socket(pid, transport, socket, :dtls_connection)
  end

  def setopts(transport, socket, other) do
    :dtls_socket.setopts(transport, socket, other)
  end

  def getopts(transport, socket, tag) do
    :dtls_socket.getopts(transport, socket, tag)
  end

  def init(:enter, _, state) do
    {:keep_state, state}
  end

  def init(
        {:call, from},
        {:start, timeout},
        r_state(
          static_env:
            r_static_env(
              host: host,
              port: port,
              role: :client,
              session_cache: cache,
              session_cache_cb: cacheCb
            ),
          handshake_env: r_handshake_env(renegotiation: {renegotiation, _}),
          connection_env: cEnv,
          ssl_options: %{versions: versions} = sslOpts,
          session: r_session(own_certificate: cert) = newSession,
          connection_states: connectionStates0
        ) = state0
      ) do
    session =
      :ssl_session.client_select_session({host, port, sslOpts}, cache, cacheCb, newSession)

    hello =
      :dtls_handshake.client_hello(
        host,
        port,
        connectionStates0,
        sslOpts,
        r_session(session, :session_id),
        renegotiation,
        cert
      )

    maxFragEnum = :maps.get(:max_frag_enum, r_client_hello(hello, :extensions), :undefined)

    connectionStates1 =
      :ssl_record.set_max_fragment_length(
        maxFragEnum,
        connectionStates0
      )

    version = r_client_hello(hello, :client_version)

    helloVersion =
      :dtls_record.hello_version(
        version,
        versions
      )

    state1 =
      prepare_flight(
        r_state(state0,
          connection_env: r_connection_env(cEnv, negotiated_version: version),
          connection_states: connectionStates1
        )
      )

    {state2, actions} =
      send_handshake(
        hello,
        r_state(state1, connection_env: r_connection_env(cEnv, negotiated_version: helloVersion))
      )

    state =
      r_state(state2,
        connection_env: r_connection_env(cEnv, negotiated_version: version),
        session: session,
        start_or_recv_from: from
      )

    next_event(:hello, :no_record, state, [{{:timeout, :handshake}, timeout, :close} | actions])
  end

  def init(
        {:call, _} = type,
        event,
        r_state(
          static_env: r_static_env(role: :server),
          protocol_specific: pS
        ) = state
      ) do
    result =
      gen_handshake(
        :init,
        type,
        event,
        r_state(state,
          protocol_specific:
            Map.merge(pS, %{
              current_cookie_secret: :dtls_v1.cookie_secret(),
              previous_cookie_secret: <<>>,
              ignored_alerts: 0,
              max_ignored_alerts: 10
            })
        )
      )

    :erlang.send_after(:dtls_v1.cookie_timeout(), self(), :new_cookie_secret)
    result
  end

  def init(type, event, state) do
    gen_handshake(:init, type, event, state)
  end

  def error(:enter, _, state) do
    {:keep_state, state}
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

  def hello(:enter, _, r_state(static_env: r_static_env(role: :server)) = state) do
    {:keep_state, state}
  end

  def hello(:enter, _, r_state(static_env: r_static_env(role: :client)) = state0) do
    {state, actions} = handle_flight_timer(state0)
    {:keep_state, state, actions}
  end

  def hello(
        :internal,
        r_client_hello(cookie: <<>>, client_version: version) = hello,
        r_state(
          static_env: r_static_env(role: :server, transport_cb: transport, socket: socket),
          handshake_env: hsEnv,
          connection_env: cEnv,
          protocol_specific: %{current_cookie_secret: secret}
        ) = state0
      ) do
    state1 =
      :ssl_connection.handle_sni_extension(
        state0,
        hello
      )

    {:ok, {iP, port}} =
      :dtls_socket.peername(
        transport,
        socket
      )

    cookie = :dtls_handshake.cookie(secret, iP, port, hello)

    verifyRequest =
      :dtls_handshake.hello_verify_request(
        cookie,
        {254, 255}
      )

    state2 =
      prepare_flight(
        r_state(state1, connection_env: r_connection_env(cEnv, negotiated_version: version))
      )

    {state, actions} = send_handshake(verifyRequest, state2)

    next_event(
      :hello,
      :no_record,
      r_state(state,
        handshake_env:
          r_handshake_env(hsEnv, tls_handshake_history: :ssl_handshake.init_handshake_history())
      ),
      actions
    )
  end

  def hello(
        :internal,
        r_hello_verify_request(cookie: cookie),
        r_state(
          static_env: r_static_env(role: :client, host: host, port: port),
          handshake_env:
            r_handshake_env(
              renegotiation: {renegotiation, _},
              ocsp_stapling_state: ocspState0
            ) = hsEnv,
          connection_env: cEnv,
          ssl_options: %{ocsp_stapling: ocspStaplingOpt, ocsp_nonce: ocspNonceOpt} = sslOpts,
          session: r_session(own_certificate: cert, session_id: id),
          connection_states: connectionStates0
        ) = state0
      ) do
    ocspNonce =
      :tls_handshake.ocsp_nonce(
        ocspNonceOpt,
        ocspStaplingOpt
      )

    hello =
      :dtls_handshake.client_hello(
        host,
        port,
        cookie,
        connectionStates0,
        sslOpts,
        id,
        renegotiation,
        cert,
        ocspNonce
      )

    version = r_client_hello(hello, :client_version)

    state1 =
      prepare_flight(
        r_state(state0,
          handshake_env:
            r_handshake_env(hsEnv,
              tls_handshake_history: :ssl_handshake.init_handshake_history(),
              ocsp_stapling_state: Map.put(ocspState0, :ocsp_nonce, ocspNonce)
            )
        )
      )

    {state2, actions} = send_handshake(hello, state1)
    state = r_state(state2, connection_env: r_connection_env(cEnv, negotiated_version: version))
    next_event(:hello, :no_record, state, actions)
  end

  def hello(
        :internal,
        r_client_hello(extensions: extensions) = hello,
        r_state(
          ssl_options: %{handshake: :hello},
          handshake_env: hsEnv,
          start_or_recv_from: from
        ) = state0
      ) do
    state =
      :ssl_connection.handle_sni_extension(
        state0,
        hello
      )

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
        r_client_hello(cookie: cookie) = hello,
        r_state(
          static_env: r_static_env(role: :server, transport_cb: transport, socket: socket),
          protocol_specific: %{current_cookie_secret: secret, previous_cookie_secret: pSecret}
        ) = state0
      ) do
    state =
      :ssl_connection.handle_sni_extension(
        state0,
        hello
      )

    {:ok, {iP, port}} =
      :dtls_socket.peername(
        transport,
        socket
      )

    case :dtls_handshake.cookie(secret, iP, port, hello) do
      ^cookie ->
        handle_client_hello(hello, state)

      _ ->
        case :dtls_handshake.cookie(pSecret, iP, port, hello) do
          ^cookie ->
            handle_client_hello(hello, state)

          _ ->
            hello(:internal, r_client_hello(hello, cookie: <<>>), state)
        end
    end
  end

  def hello(
        :internal,
        r_server_hello() = hello,
        r_state(
          static_env: r_static_env(role: :client),
          handshake_env:
            r_handshake_env(
              renegotiation: {renegotiation, _},
              ocsp_stapling_state: ocspState0
            ) = hsEnv,
          connection_env: r_connection_env(negotiated_version: reqVersion),
          connection_states: connectionStates0,
          session: r_session(session_id: oldId),
          ssl_options: sslOptions
        ) = state
      ) do
    case :dtls_handshake.hello(hello, sslOptions, connectionStates0, renegotiation, oldId) do
      r_alert() = alert ->
        handle_own_alert(alert, reqVersion, :hello, state)

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
    end
  end

  def hello(
        :internal,
        {:handshake, {r_client_hello(cookie: <<>>) = handshake, _}},
        state
      ) do
    {:next_state, :hello, state, [{:next_event, :internal, handshake}]}
  end

  def hello(:internal, {:handshake, {r_hello_verify_request() = handshake, _}}, state) do
    {:next_state, :hello, state, [{:next_event, :internal, handshake}]}
  end

  def hello(:internal, r_change_cipher_spec(type: <<1>>), state0) do
    {state1, actions0} =
      send_handshake_flight(
        state0,
        retransmit_epoch(
          :hello,
          state0
        )
      )

    {:next_state, :hello, state, actions} = next_event(:hello, :no_record, state1, actions0)
    {:repeat_state, state, actions}
  end

  def hello(:info, event, state) do
    gen_info(event, :hello, state)
  end

  def hello(:state_timeout, event, state) do
    handle_state_timeout(event, :hello, state)
  end

  def hello(type, event, state) do
    gen_handshake(:hello, type, event, state)
  end

  def user_hello(:enter, _, state) do
    {:keep_state, state}
  end

  def user_hello(type, event, state) do
    gen_handshake(:user_hello, type, event, state)
  end

  def abbreviated(:enter, _, state0) do
    {state, actions} = handle_flight_timer(state0)
    {:keep_state, state, actions}
  end

  def abbreviated(:info, event, state) do
    gen_info(event, :abbreviated, state)
  end

  def abbreviated(
        :internal = type,
        r_change_cipher_spec(type: <<1>>) = event,
        r_state(connection_states: connectionStates0) = state
      ) do
    connectionStates1 =
      :dtls_record.save_current_connection_state(
        connectionStates0,
        :read
      )

    connectionStates =
      :dtls_record.next_epoch(
        connectionStates1,
        :read
      )

    gen_handshake(:abbreviated, type, event, r_state(state, connection_states: connectionStates))
  end

  def abbreviated(
        :internal = type,
        r_finished() = event,
        r_state(
          connection_states: connectionStates,
          protocol_specific: pS
        ) = state
      ) do
    gen_handshake(
      :abbreviated,
      type,
      event,
      prepare_flight(
        r_state(state,
          connection_states: connectionStates,
          protocol_specific: Map.put(pS, :flight_state, :connection)
        )
      )
    )
  end

  def abbreviated(:state_timeout, event, state) do
    handle_state_timeout(event, :abbreviated, state)
  end

  def abbreviated(type, event, state) do
    gen_handshake(:abbreviated, type, event, state)
  end

  def wait_ocsp_stapling(:enter, _Event, state0) do
    {state, actions} = handle_flight_timer(state0)
    {:keep_state, state, actions}
  end

  def wait_ocsp_stapling(:info, event, state) do
    gen_info(event, :wait_ocsp_stapling, state)
  end

  def wait_ocsp_stapling(:state_timeout, event, state) do
    handle_state_timeout(event, :wait_ocsp_stapling, state)
  end

  def wait_ocsp_stapling(type, event, state) do
    gen_handshake(:wait_ocsp_stapling, type, event, state)
  end

  def certify(:enter, _, state0) do
    {state, actions} = handle_flight_timer(state0)
    {:keep_state, state, actions}
  end

  def certify(:info, event, state) do
    gen_info(event, :certify, state)
  end

  def certify(:internal = type, r_server_hello_done() = event, state) do
    :ssl_connection.certify(type, event, prepare_flight(state), :dtls_connection)
  end

  def certify(:internal, r_change_cipher_spec(type: <<1>>), state0) do
    {state1, actions0} =
      send_handshake_flight(
        state0,
        retransmit_epoch(
          :certify,
          state0
        )
      )

    {:next_state, :certify, state, actions} = next_event(:certify, :no_record, state1, actions0)
    {:repeat_state, state, actions}
  end

  def certify(:state_timeout, event, state) do
    handle_state_timeout(event, :certify, state)
  end

  def certify(type, event, state) do
    gen_handshake(:certify, type, event, state)
  end

  def cipher(:enter, _, state0) do
    {state, actions} = handle_flight_timer(state0)
    {:keep_state, state, actions}
  end

  def cipher(:info, event, state) do
    gen_info(event, :cipher, state)
  end

  def cipher(
        :internal = type,
        r_change_cipher_spec(type: <<1>>) = event,
        r_state(connection_states: connectionStates0) = state
      ) do
    connectionStates1 =
      :dtls_record.save_current_connection_state(
        connectionStates0,
        :read
      )

    connectionStates =
      :dtls_record.next_epoch(
        connectionStates1,
        :read
      )

    :ssl_connection.cipher(
      type,
      event,
      r_state(state, connection_states: connectionStates),
      :dtls_connection
    )
  end

  def cipher(
        :internal = type,
        r_finished() = event,
        r_state(
          connection_states: connectionStates,
          protocol_specific: pS
        ) = state
      ) do
    :ssl_connection.cipher(
      type,
      event,
      prepare_flight(
        r_state(state,
          connection_states: connectionStates,
          protocol_specific: Map.put(pS, :flight_state, :connection)
        )
      ),
      :dtls_connection
    )
  end

  def cipher(:state_timeout, event, state) do
    handle_state_timeout(event, :cipher, state)
  end

  def cipher(type, event, state) do
    :ssl_connection.cipher(type, event, state, :dtls_connection)
  end

  def connection(:enter, _, state) do
    {:keep_state, state}
  end

  def connection(:info, event, state) do
    gen_info(event, :connection, state)
  end

  def connection(
        :internal,
        r_hello_request(),
        r_state(
          static_env:
            r_static_env(
              host: host,
              port: port,
              data_tag: dataTag,
              session_cache: cache,
              session_cache_cb: cacheCb
            ),
          handshake_env: r_handshake_env(renegotiation: {renegotiation, _}),
          connection_env: cEnv,
          session: r_session(own_certificate: cert) = session0,
          ssl_options: %{versions: versions} = sslOpts,
          connection_states: connectionStates0,
          protocol_specific: pS
        ) = state0
      ) do
    session = :ssl_session.client_select_session({host, port, sslOpts}, cache, cacheCb, session0)

    hello =
      :dtls_handshake.client_hello(
        host,
        port,
        connectionStates0,
        sslOpts,
        r_session(session, :session_id),
        renegotiation,
        cert
      )

    version = r_client_hello(hello, :client_version)

    helloVersion =
      :dtls_record.hello_version(
        version,
        versions
      )

    state1 = prepare_flight(state0)

    {state2, actions} =
      send_handshake(
        hello,
        r_state(state1, connection_env: r_connection_env(cEnv, negotiated_version: helloVersion))
      )

    state =
      r_state(state2,
        protocol_specific: Map.put(pS, :flight_state, initial_flight_state(dataTag)),
        session: session
      )

    next_event(:hello, :no_record, state, actions)
  end

  def connection(
        :internal,
        r_client_hello() = hello,
        r_state(
          static_env: r_static_env(role: :server),
          handshake_env: r_handshake_env(allow_renegotiate: true) = hsEnv
        ) = state
      ) do
    :erlang.send_after(12000, self(), :allow_renegotiate)

    {:next_state, :hello,
     r_state(state,
       handshake_env:
         r_handshake_env(hsEnv,
           renegotiation: {true, :peer},
           allow_renegotiate: false
         )
     ), [{:next_event, :internal, hello}]}
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
          mfa: {:dtls_connection, :connection, 3},
          line: 761,
          file: 'otp/lib/ssl/src/dtls_connection.erl'
        }
      )

    state1 = send_alert(alert, state0)

    {record, state} =
      :ssl_connection.prepare_connection(
        state1,
        :dtls_connection
      )

    next_event(:connection, record, state)
  end

  def connection({:call, from}, {:application_data, data}, state) do
    try do
      send_application_data(data, from, :connection, state)
    catch
      error ->
        :ssl_connection.hibernate_after(:connection, state, [{:reply, from, error}])
    end
  end

  def connection(type, event, state) do
    :ssl_connection.connection(type, event, state, :dtls_connection)
  end

  def downgrade(:enter, _, state) do
    {:keep_state, state}
  end

  def downgrade(type, event, state) do
    :ssl_connection.downgrade(type, event, state, :dtls_connection)
  end

  def callback_mode() do
    [:state_functions, :state_enter]
  end

  def terminate(reason, stateName, state) do
    :ssl_connection.terminate(reason, stateName, state)
  end

  def code_change(_OldVsn, stateName, state, _Extra) do
    {:ok, stateName, state}
  end

  def format_status(type, data) do
    :ssl_connection.format_status(type, data)
  end

  defp initial_state(
         role,
         host,
         port,
         socket,
         {%{client_renegotiation: clientRenegotiation} = sSLOptions, socketOptions, trackers},
         user,
         {cbModule, dataTag, closeTag, errorTag, passiveTag}
       ) do
    %{beast_mitigation: beastMitigation} = sSLOptions

    connectionStates =
      :dtls_record.init_connection_states(
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
        {:ok, n} when is_integer(n) ->
          n

        _ ->
          100
      end

    monitor = :erlang.monitor(:process, user)

    initStatEnv =
      r_static_env(
        role: role,
        transport_cb: cbModule,
        protocol_cb: :dtls_connection,
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
      connection_env: r_connection_env(user_application: {monitor, user}),
      socket_options: socketOptions,
      ssl_options: Map.put(sSLOptions, :password, :undefined),
      session: r_session(is_resumable: false),
      connection_states: connectionStates,
      protocol_buffers: r_protocol_buffers(),
      user_data_buffer: {[], 0, []},
      start_or_recv_from: :undefined,
      flight_buffer: new_flight(),
      protocol_specific: %{
        active_n: internalActiveN,
        active_n_toggle: true,
        flight_state: initial_flight_state(dataTag)
      }
    )
  end

  defp initial_flight_state(:udp) do
    {:retransmit, 1000}
  end

  defp initial_flight_state(_) do
    :reliable
  end

  defp next_dtls_record(
         data,
         stateName,
         r_state(
           protocol_buffers:
             r_protocol_buffers(
               dtls_record_buffer: buf0,
               dtls_cipher_texts: cT0
             ) = buffers,
           connection_env: r_connection_env(negotiated_version: version),
           static_env: r_static_env(data_tag: dataTag),
           ssl_options: sslOpts
         ) = state0
       ) do
    case :dtls_record.get_dtls_records(
           data,
           {dataTag, stateName, version,
            for vsn <- [:"dtlsv1.2", :dtlsv1] do
              :dtls_record.protocol_version(vsn)
            end},
           buf0,
           sslOpts
         ) do
      {records, buf1} ->
        cT1 = cT0 ++ records

        next_record(
          r_state(state0,
            protocol_buffers:
              r_protocol_buffers(buffers,
                dtls_record_buffer: buf1,
                dtls_cipher_texts: cT1
              )
          )
        )

      r_alert() = alert ->
        alert
    end
  end

  defp dtls_handshake_events(packets) do
    :lists.map(
      fn packet ->
        {:next_event, :internal, {:handshake, packet}}
      end,
      packets
    )
  end

  defp decode_cipher_text(
         r_state(
           protocol_buffers:
             r_protocol_buffers(
               dtls_cipher_texts: [
                 cT
                 | rest
               ]
             ) = buffers,
           connection_states: connStates0
         ) = state
       ) do
    case :dtls_record.decode_cipher_text(
           cT,
           connStates0
         ) do
      {plain, connStates} ->
        {plain,
         r_state(state,
           protocol_buffers: r_protocol_buffers(buffers, dtls_cipher_texts: rest),
           connection_states: connStates
         )}

      r_alert() = alert ->
        {alert, state}
    end
  end

  defp dtls_version(
         :hello,
         version,
         r_state(
           static_env: r_static_env(role: :server),
           connection_env: cEnv
         ) = state
       ) do
    r_state(state, connection_env: r_connection_env(cEnv, negotiated_version: version))
  end

  defp dtls_version(_, _, state) do
    state
  end

  defp handle_client_hello(
         r_client_hello(client_version: clientVersion) = hello,
         r_state(
           connection_states: connectionStates0,
           static_env: r_static_env(trackers: trackers),
           handshake_env:
             r_handshake_env(
               kex_algorithm: keyExAlg,
               renegotiation: {renegotiation, _},
               negotiated_protocol: currentProtocol
             ) = hsEnv,
           connection_env: cEnv,
           session: r_session(own_certificate: cert) = session0,
           ssl_options: sslOpts
         ) = state0
       ) do
    sessionTracker =
      :proplists.get_value(
        :session_id_tracker,
        trackers
      )

    case :dtls_handshake.hello(
           hello,
           sslOpts,
           {sessionTracker, session0, connectionStates0, cert, keyExAlg},
           renegotiation
         ) do
      r_alert() = alert ->
        handle_own_alert(alert, clientVersion, :hello, state0)

      {version, {type, session}, connectionStates, protocol0, serverHelloExt, hashSign} ->
        protocol =
          case protocol0 do
            :undefined ->
              currentProtocol

            _ ->
              protocol0
          end

        state =
          prepare_flight(
            r_state(state0,
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

        :ssl_connection.hello(
          :internal,
          {:common_client_hello, type, serverHelloExt},
          state,
          :dtls_connection
        )
    end
  end

  defp handle_info(
         {protocol, _, _, _, data},
         stateName,
         r_state(
           static_env:
             r_static_env(
               role: role,
               data_tag: protocol
             )
         ) = state0
       ) do
    case next_dtls_record(data, stateName, state0) do
      {record, state} ->
        next_event(stateName, record, state)

      r_alert() = alert ->
        :ssl_connection.handle_normal_shutdown(r_alert(alert, role: role), stateName, state0)
        {:stop, {:shutdown, :own_alert}, state0}
    end
  end

  defp handle_info(
         {passiveTag, socket},
         stateName,
         r_state(
           static_env:
             r_static_env(
               socket: {_, socket},
               passive_tag: passiveTag
             ),
           protocol_specific: pS
         ) = state
       ) do
    next_event(
      stateName,
      :no_record,
      r_state(state, protocol_specific: Map.put(pS, :active_n_toggle, true))
    )
  end

  defp handle_info(
         {closeTag, socket},
         stateName,
         r_state(
           static_env: r_static_env(role: role, socket: socket, close_tag: closeTag),
           connection_env: r_connection_env(negotiated_version: version),
           socket_options: r_socket_options(active: active),
           protocol_buffers: r_protocol_buffers(dtls_cipher_texts: cTs),
           protocol_specific: pS
         ) = state
       ) do
    case active == false and cTs !== [] do
      false ->
        case version do
          {254, n} when n <= 253 ->
            :ok

          _ ->
            :ok
        end

        alert =
          r_alert(
            level: 2,
            description: 0,
            where: %{
              mfa: {:dtls_connection, :handle_info, 3},
              line: 983,
              file: 'otp/lib/ssl/src/dtls_connection.erl'
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
         :new_cookie_secret,
         stateName,
         r_state(protocol_specific: %{current_cookie_secret: secret} = cookieInfo) = state
       ) do
    :erlang.send_after(:dtls_v1.cookie_timeout(), self(), :new_cookie_secret)

    {:next_state, stateName,
     r_state(state,
       protocol_specific:
         Map.merge(cookieInfo, %{
           current_cookie_secret: :dtls_v1.cookie_secret(),
           previous_cookie_secret: secret
         })
     )}
  end

  defp handle_info(msg, stateName, state) do
    apply(:ssl_connection, stateName, [:info, msg, state, :dtls_connection])
  end

  defp handle_state_timeout(
         :flight_retransmission_timeout,
         stateName,
         r_state(protocol_specific: %{flight_state: {:retransmit, _NextTimeout}}) = state0
       ) do
    {state1, actions0} =
      send_handshake_flight(
        state0,
        retransmit_epoch(
          stateName,
          state0
        )
      )

    {:next_state, ^stateName, state, actions} =
      next_event(stateName, :no_record, state1, actions0)

    {:repeat_state, state, actions}
  end

  defp handle_alerts([], result) do
    result
  end

  defp handle_alerts(_, {:stop, _, _} = stop) do
    stop
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

  defp handle_own_alert(
         alert,
         version,
         stateName,
         r_state(
           static_env: r_static_env(data_tag: :udp, role: role),
           ssl_options: %{log_level: logLevel}
         ) = state0
       ) do
    case ignore_alert(alert, state0) do
      {true, state} ->
        log_ignore_alert(logLevel, stateName, alert, role)
        {:next_state, stateName, state}

      {false, state} ->
        :ssl_connection.handle_own_alert(alert, version, stateName, state)
    end
  end

  defp handle_own_alert(alert, version, stateName, state) do
    :ssl_connection.handle_own_alert(alert, version, stateName, state)
  end

  defp encode_handshake_flight(flight, version, maxFragmentSize, epoch, connectionStates) do
    fragments =
      :lists.map(
        fn handshake ->
          :dtls_handshake.fragment_handshake(
            handshake,
            maxFragmentSize
          )
        end,
        flight
      )

    :dtls_record.encode_handshake(fragments, version, epoch, connectionStates)
  end

  defp encode_change_cipher(r_change_cipher_spec(), version, epoch, connectionStates) do
    :dtls_record.encode_change_cipher_spec(version, epoch, connectionStates)
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
      apply(:ssl_connection, stateName, [type, event, state, :dtls_connection])
    catch
      _, _ ->
        :ssl_connection.handle_own_alert(
          r_alert(
            level: 2,
            description: 40,
            where: %{
              mfa: {:dtls_connection, :gen_handshake, 4},
              line: 1054,
              file: 'otp/lib/ssl/src/dtls_connection.erl'
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
              mfa: {:dtls_connection, :gen_info, 3},
              line: 1065,
              file: 'otp/lib/ssl/src/dtls_connection.erl'
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
              mfa: {:dtls_connection, :gen_info, 3},
              line: 1076,
              file: 'otp/lib/ssl/src/dtls_connection.erl'
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

  defp update_handshake_history(r_hello_verify_request(), _, hist) do
    hist
  end

  defp update_handshake_history(_, handshake, hist) do
    :ssl_handshake.update_handshake_history(
      hist,
      :erlang.iolist_to_binary(handshake)
    )
  end

  defp prepare_flight(
         r_state(
           flight_buffer: flight,
           connection_states: connectionStates0,
           protocol_buffers: r_protocol_buffers() = buffers
         ) = state
       ) do
    connectionStates =
      :dtls_record.save_current_connection_state(
        connectionStates0,
        :write
      )

    r_state(state,
      flight_buffer: next_flight(flight),
      connection_states: connectionStates,
      protocol_buffers:
        r_protocol_buffers(buffers,
          dtls_handshake_next_fragments: [],
          dtls_handshake_later_fragments: []
        )
    )
  end

  defp new_flight() do
    %{
      next_sequence: 0,
      handshakes: [],
      change_cipher_spec: :undefined,
      handshakes_after_change_cipher_spec: []
    }
  end

  defp next_flight(flight) do
    Map.merge(flight, %{
      handshakes: [],
      change_cipher_spec: :undefined,
      handshakes_after_change_cipher_spec: []
    })
  end

  defp handle_flight_timer(
         r_state(
           static_env: r_static_env(data_tag: :udp),
           protocol_specific: %{flight_state: {:retransmit, timeout}}
         ) = state
       ) do
    start_retransmision_timer(timeout, state)
  end

  defp handle_flight_timer(
         r_state(
           static_env: r_static_env(data_tag: :udp),
           protocol_specific: %{flight_state: :connection}
         ) = state
       ) do
    {state, []}
  end

  defp handle_flight_timer(r_state(protocol_specific: %{flight_state: :reliable}) = state) do
    {state, []}
  end

  defp start_retransmision_timer(timeout, r_state(protocol_specific: pS) = state) do
    {r_state(state,
       protocol_specific: Map.put(pS, :flight_state, {:retransmit, new_timeout(timeout)})
     ), [{:state_timeout, timeout, :flight_retransmission_timeout}]}
  end

  defp new_timeout(n) when n <= 30000 do
    n * 2
  end

  defp new_timeout(_) do
    60000
  end

  defp send_handshake_flight(
         r_state(
           static_env:
             r_static_env(
               socket: socket,
               transport_cb: transport
             ),
           connection_env: r_connection_env(negotiated_version: version),
           flight_buffer: %{handshakes: flight, change_cipher_spec: :undefined},
           connection_states: connectionStates0,
           ssl_options: %{log_level: logLevel}
         ) = state0,
         epoch
       ) do
    pMTUEstimate = 1400
    %{current_write: %{max_fragment_length: maxFragmentLength}} = connectionStates0
    maxSize = min(maxFragmentLength, pMTUEstimate)

    {encoded, connectionStates} =
      encode_handshake_flight(:lists.reverse(flight), version, maxSize, epoch, connectionStates0)

    send(transport, socket, encoded)
    :ssl_logger.debug(logLevel, :outbound, :record, encoded)
    {r_state(state0, connection_states: connectionStates), []}
  end

  defp send_handshake_flight(
         r_state(
           static_env:
             r_static_env(
               socket: socket,
               transport_cb: transport
             ),
           connection_env: r_connection_env(negotiated_version: version),
           flight_buffer: %{
             handshakes: [_ | _] = flight0,
             change_cipher_spec: changeCipher,
             handshakes_after_change_cipher_spec: []
           },
           connection_states: connectionStates0,
           ssl_options: %{log_level: logLevel}
         ) = state0,
         epoch
       ) do
    pMTUEstimate = 1400
    %{current_write: %{max_fragment_length: maxFragmentLength}} = connectionStates0
    maxSize = min(maxFragmentLength, pMTUEstimate)

    {hsBefore, connectionStates1} =
      encode_handshake_flight(:lists.reverse(flight0), version, maxSize, epoch, connectionStates0)

    {encChangeCipher, connectionStates} =
      encode_change_cipher(changeCipher, version, epoch, connectionStates1)

    send(transport, socket, [hsBefore, encChangeCipher])
    :ssl_logger.debug(logLevel, :outbound, :record, [hsBefore])
    :ssl_logger.debug(logLevel, :outbound, :record, [encChangeCipher])
    {r_state(state0, connection_states: connectionStates), []}
  end

  defp send_handshake_flight(
         r_state(
           static_env:
             r_static_env(
               socket: socket,
               transport_cb: transport
             ),
           connection_env: r_connection_env(negotiated_version: version),
           flight_buffer: %{
             handshakes: [_ | _] = flight0,
             change_cipher_spec: changeCipher,
             handshakes_after_change_cipher_spec: flight1
           },
           connection_states: connectionStates0,
           ssl_options: %{log_level: logLevel}
         ) = state0,
         epoch
       ) do
    pMTUEstimate = 1400
    %{current_write: %{max_fragment_length: maxFragmentLength}} = connectionStates0
    maxSize = min(maxFragmentLength, pMTUEstimate)

    {hsBefore, connectionStates1} =
      encode_handshake_flight(
        :lists.reverse(flight0),
        version,
        maxSize,
        epoch - 1,
        connectionStates0
      )

    {encChangeCipher, connectionStates2} =
      encode_change_cipher(changeCipher, version, epoch - 1, connectionStates1)

    {hsAfter, connectionStates} =
      encode_handshake_flight(:lists.reverse(flight1), version, maxSize, epoch, connectionStates2)

    send(transport, socket, [hsBefore, encChangeCipher, hsAfter])
    :ssl_logger.debug(logLevel, :outbound, :record, [hsBefore])
    :ssl_logger.debug(logLevel, :outbound, :record, [encChangeCipher])
    :ssl_logger.debug(logLevel, :outbound, :record, [hsAfter])
    {r_state(state0, connection_states: connectionStates), []}
  end

  defp send_handshake_flight(
         r_state(
           static_env:
             r_static_env(
               socket: socket,
               transport_cb: transport
             ),
           connection_env: r_connection_env(negotiated_version: version),
           flight_buffer: %{
             handshakes: [],
             change_cipher_spec: changeCipher,
             handshakes_after_change_cipher_spec: flight1
           },
           connection_states: connectionStates0,
           ssl_options: %{log_level: logLevel}
         ) = state0,
         epoch
       ) do
    pMTUEstimate = 1400
    %{current_write: %{max_fragment_length: maxFragmentLength}} = connectionStates0
    maxSize = min(maxFragmentLength, pMTUEstimate)

    {encChangeCipher, connectionStates1} =
      encode_change_cipher(changeCipher, version, epoch - 1, connectionStates0)

    {hsAfter, connectionStates} =
      encode_handshake_flight(:lists.reverse(flight1), version, maxSize, epoch, connectionStates1)

    send(transport, socket, [encChangeCipher, hsAfter])
    :ssl_logger.debug(logLevel, :outbound, :record, [encChangeCipher])
    :ssl_logger.debug(logLevel, :outbound, :record, [hsAfter])
    {r_state(state0, connection_states: connectionStates), []}
  end

  defp retransmit_epoch(
         _StateName,
         r_state(connection_states: connectionStates)
       ) do
    %{epoch: epoch} =
      :ssl_record.current_connection_state(
        connectionStates,
        :write
      )

    epoch
  end

  defp ignore_alert(
         r_alert(level: 2),
         r_state(protocol_specific: %{ignored_alerts: n, max_ignored_alerts: n}) = state
       ) do
    {false, state}
  end

  defp ignore_alert(
         r_alert(level: 2) = alert,
         r_state(protocol_specific: %{ignored_alerts: n} = pS) = state
       ) do
    case is_ignore_alert(alert) do
      true ->
        {true, r_state(state, protocol_specific: Map.put(pS, :ignored_alerts, n + 1))}

      false ->
        {false, state}
    end
  end

  defp ignore_alert(_, state) do
    {false, state}
  end

  defp is_ignore_alert(r_alert(description: 20)) do
    true
  end

  defp is_ignore_alert(r_alert(description: 22)) do
    true
  end

  defp is_ignore_alert(r_alert(description: 50)) do
    true
  end

  defp is_ignore_alert(r_alert(description: 51)) do
    true
  end

  defp is_ignore_alert(r_alert(description: 47)) do
    true
  end

  defp is_ignore_alert(_) do
    false
  end

  defp log_ignore_alert(level, stateName, r_alert(where: location) = alert, role) do
    :ssl_logger.log(
      :info,
      level,
      %{
        alert: alert,
        alerter: :ignored,
        statename: stateName,
        role: role,
        protocol: protocol_name()
      },
      location
    )
  end

  defp send_application_data(
         data,
         from,
         _StateName,
         r_state(
           static_env:
             r_static_env(
               socket: socket,
               transport_cb: transport
             ),
           connection_env: r_connection_env(negotiated_version: version),
           handshake_env: hsEnv,
           connection_states: connectionStates0,
           ssl_options: %{renegotiate_at: renegotiateAt, log_level: logLevel}
         ) = state0
       ) do
    case time_to_renegotiate(data, connectionStates0, renegotiateAt) do
      true ->
        renegotiate(
          r_state(state0, handshake_env: r_handshake_env(hsEnv, renegotiation: {true, :internal})),
          [{:next_event, {:call, from}, {:application_data, data}}]
        )

      false ->
        {msgs, connectionStates} =
          :dtls_record.encode_data(
            data,
            version,
            connectionStates0
          )

        state = r_state(state0, connection_states: connectionStates)

        case send(transport, socket, msgs) do
          :ok ->
            :ssl_logger.debug(logLevel, :outbound, :record, msgs)
            :ssl_connection.hibernate_after(:connection, state, [{:reply, from, :ok}])

          result ->
            :ssl_connection.hibernate_after(:connection, state, [{:reply, from, result}])
        end
    end
  end

  defp time_to_renegotiate(
         _Data,
         %{current_write: %{sequence_number: num}},
         renegotiateAt
       ) do
    is_time_to_renegotiate(num, renegotiateAt)
  end

  defp is_time_to_renegotiate(n, m) when n < m do
    false
  end

  defp is_time_to_renegotiate(_, _) do
    true
  end
end
