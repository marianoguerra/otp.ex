defmodule :m_tls_socket do
  use Bitwise
  @behaviour :gen_server
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

  Record.defrecord(:r_sslsocket, :sslsocket, fd: nil, pid: nil)

  Record.defrecord(:r_state, :state,
    emulated_opts: :undefined,
    port: :undefined,
    ssl_opts: :undefined
  )

  def send(transport, socket, data) do
    transport.send(socket, data)
  end

  def listen(
        transport,
        port,
        r_config(
          transport_info: {transport, _, _, _, _},
          inet_user: options,
          ssl: sslOpts,
          emulated: emOpts
        ) = config
      ) do
    case transport.listen(
           port,
           options ++ internal_inet_values()
         ) do
      {:ok, listenSocket} ->
        {:ok, tracker} = inherit_tracker(listenSocket, emOpts, sslOpts)
        lifeTime = get_ticket_lifetime()
        ticketStoreSize = get_ticket_store_size()
        {:ok, sessionHandler} = session_tickets_tracker(lifeTime, ticketStoreSize, sslOpts)
        {:ok, sessionIdHandle} = session_id_tracker(sslOpts)

        trackers = [
          {:option_tracker, tracker},
          {:session_tickets_tracker, sessionHandler},
          {:session_id_tracker, sessionIdHandle}
        ]

        socket = r_sslsocket(pid: {listenSocket, r_config(config, trackers: trackers)})
        check_active_n(emOpts, socket)
        {:ok, socket}

      err = {:error, _} ->
        err
    end
  end

  def accept(
        listenSocket,
        r_config(
          transport_info: {transport, _, _, _, _} = cbInfo,
          connection_cb: connectionCb,
          ssl: sslOpts,
          trackers: trackers
        ),
        timeout
      ) do
    case transport.accept(listenSocket, timeout) do
      {:ok, socket} ->
        tracker =
          :proplists.get_value(
            :option_tracker,
            trackers
          )

        {:ok, emOpts} = get_emulated_opts(tracker)
        {:ok, port} = :tls_socket.port(transport, socket)
        {:ok, sender} = :tls_sender.start()

        connArgs = [
          :server,
          sender,
          'localhost',
          port,
          socket,
          {sslOpts, emulated_socket_options(emOpts, r_socket_options()), trackers},
          self(),
          cbInfo
        ]

        case :tls_connection_sup.start_child(connArgs) do
          {:ok, pid} ->
            :ssl_connection.socket_control(
              connectionCb,
              socket,
              [pid, sender],
              transport,
              trackers
            )

          {:error, reason} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  def upgrade(
        socket,
        r_config(
          transport_info: {transport, _, _, _, _} = cbInfo,
          ssl: sslOptions,
          emulated: emOpts,
          connection_cb: connectionCb
        ),
        timeout
      ) do
    :ok = setopts(transport, socket, :tls_socket.internal_inet_values())

    case peername(transport, socket) do
      {:ok, {address, port}} ->
        :ssl_connection.connect(
          connectionCb,
          address,
          port,
          socket,
          {sslOptions, emulated_socket_options(emOpts, r_socket_options()), :undefined},
          self(),
          cbInfo,
          timeout
        )

      {:error, error} ->
        {:error, error}
    end
  end

  def connect(
        address,
        port,
        r_config(
          transport_info: cbInfo,
          inet_user: userOpts,
          ssl: sslOpts,
          emulated: emOpts,
          inet_ssl: socketOpts,
          connection_cb: connetionCb
        ),
        timeout
      ) do
    {transport, _, _, _, _} = cbInfo

    try do
      transport.connect(address, port, socketOpts, timeout)
    catch
      :exit, {:function_clause, _} ->
        {:error, {:options, {:cb_info, cbInfo}}}

      :exit, :badarg ->
        {:error, {:options, {:socket_options, userOpts}}}

      :exit, {:badarg, _} ->
        {:error, {:options, {:socket_options, userOpts}}}
    else
      {:ok, socket} ->
        :ssl_connection.connect(
          connetionCb,
          address,
          port,
          socket,
          {sslOpts, emulated_socket_options(emOpts, r_socket_options()), :undefined},
          self(),
          cbInfo,
          timeout
        )

      {:error, reason} ->
        {:error, reason}
    end
  end

  def socket(pids, transport, socket, connectionCb, trackers) do
    r_sslsocket(
      pid: pids,
      fd: {transport, socket, connectionCb, trackers}
    )
  end

  def setopts(
        :gen_tcp,
        socket = r_sslsocket(pid: {listenSocket, r_config(trackers: trackers)}),
        options
      ) do
    tracker =
      :proplists.get_value(
        :option_tracker,
        trackers
      )

    {sockOpts, emulatedOpts} = split_options(options)
    :ok = set_emulated_opts(tracker, emulatedOpts)
    check_active_n(emulatedOpts, socket)
    :inet.setopts(listenSocket, sockOpts)
  end

  def setopts(
        _,
        socket =
          r_sslsocket(
            pid:
              {listenSocket,
               r_config(
                 transport_info: {transport, _, _, _, _},
                 trackers: trackers
               )}
          ),
        options
      ) do
    tracker =
      :proplists.get_value(
        :option_tracker,
        trackers
      )

    {sockOpts, emulatedOpts} = split_options(options)
    :ok = set_emulated_opts(tracker, emulatedOpts)
    check_active_n(emulatedOpts, socket)
    transport.setopts(listenSocket, sockOpts)
  end

  def setopts(:gen_tcp, socket, options) do
    :inet.setopts(socket, options)
  end

  def setopts(transport, socket, options) do
    transport.setopts(socket, options)
  end

  defp check_active_n(
         emulatedOpts,
         socket = r_sslsocket(pid: {_, r_config(trackers: trackers)})
       ) do
    tracker =
      :proplists.get_value(
        :option_tracker,
        trackers
      )

    case :proplists.lookup(:active, emulatedOpts) do
      {_, n} when is_integer(n) and n < -32768 ->
        throw(:einval)

      {_, n} when is_integer(n) and n > 32767 ->
        throw(:einval)

      {_, n} when is_integer(n) ->
        case get_emulated_opts(tracker, [:active]) do
          [{_, false}] ->
            send(self(), {:ssl_passive, socket})
            :ok

          [{_, a}] when is_integer(a) and a < -32768 ->
            throw(:einval)

          [{_, a}] when is_integer(a) and a > 32767 ->
            throw(:einval)

          _ ->
            :ok
        end

      _ ->
        :ok
    end
  end

  def getopts(
        :gen_tcp,
        r_sslsocket(pid: {listenSocket, r_config(trackers: trackers)}),
        options
      ) do
    tracker =
      :proplists.get_value(
        :option_tracker,
        trackers
      )

    {sockOptNames, emulatedOptNames} = split_options(options)

    emulatedOpts =
      get_emulated_opts(
        tracker,
        emulatedOptNames
      )

    socketOpts = get_socket_opts(listenSocket, sockOptNames, :inet)
    {:ok, emulatedOpts ++ socketOpts}
  end

  def getopts(
        transport,
        r_sslsocket(pid: {listenSocket, r_config(trackers: trackers)}),
        options
      ) do
    tracker =
      :proplists.get_value(
        :option_tracker,
        trackers
      )

    {sockOptNames, emulatedOptNames} = split_options(options)

    emulatedOpts =
      get_emulated_opts(
        tracker,
        emulatedOptNames
      )

    socketOpts = get_socket_opts(listenSocket, sockOptNames, transport)
    {:ok, emulatedOpts ++ socketOpts}
  end

  def getopts(:gen_tcp, socket, options) do
    :inet.getopts(socket, options)
  end

  def getopts(transport, socket, options) do
    transport.getopts(socket, options)
  end

  def getstat(:gen_tcp, socket, options) do
    :inet.getstat(socket, options)
  end

  def getstat(transport, socket, options) do
    transport.getstat(socket, options)
  end

  def peername(:gen_tcp, socket) do
    :inet.peername(socket)
  end

  def peername(transport, socket) do
    transport.peername(socket)
  end

  def sockname(:gen_tcp, socket) do
    :inet.sockname(socket)
  end

  def sockname(transport, socket) do
    transport.sockname(socket)
  end

  def port(:gen_tcp, socket) do
    :inet.port(socket)
  end

  def port(transport, socket) do
    transport.port(socket)
  end

  def close(:gen_tcp, socket) do
    :inet.close(socket)
  end

  def close(transport, socket) do
    transport.close(socket)
  end

  def emulated_options() do
    [:mode, :packet, :active, :header, :packet_size]
  end

  def emulated_options(opts) do
    emulated_options(opts, internal_inet_values(), default_inet_values())
  end

  def internal_inet_values() do
    [{:packet_size, 0}, {:packet, 0}, {:header, 0}, {:active, false}, {:mode, :binary}]
  end

  def default_inet_values() do
    [{:packet_size, 0}, {:packet, 0}, {:header, 0}, {:active, true}, {:mode, :list}]
  end

  def inherit_tracker(listenSocket, emOpts, %{:erl_dist => false} = sslOpts) do
    :ssl_listen_tracker_sup.start_child([listenSocket, emOpts, sslOpts])
  end

  def inherit_tracker(listenSocket, emOpts, %{:erl_dist => true} = sslOpts) do
    :ssl_listen_tracker_sup.start_child_dist([listenSocket, emOpts, sslOpts])
  end

  defp session_tickets_tracker(_, _, %{:erl_dist => false, :session_tickets => :disabled}) do
    {:ok, :disabled}
  end

  defp session_tickets_tracker(lifetime, ticketStoreSize, %{
         :erl_dist => false,
         :session_tickets => mode,
         :anti_replay => antiReplay
       }) do
    :tls_server_session_ticket_sup.start_child([mode, lifetime, ticketStoreSize, antiReplay])
  end

  defp session_tickets_tracker(lifetime, ticketStoreSize, %{
         :erl_dist => true,
         :session_tickets => mode
       }) do
    :tls_server_session_ticket_sup.start_child_dist([mode, lifetime, ticketStoreSize])
  end

  def session_id_tracker(%{:versions => [{3, 4}]}) do
    {:ok, :not_relevant}
  end

  def session_id_tracker(%{:erl_dist => false}) do
    :ssl_server_session_cache_sup.start_child(:ssl_server_session_cache_sup.session_opts())
  end

  def session_id_tracker(%{:erl_dist => true}) do
    :ssl_server_session_cache_sup.start_child_dist(:ssl_server_session_cache_sup.session_opts())
  end

  def get_emulated_opts(trackerPid) do
    call(trackerPid, :get_emulated_opts)
  end

  def set_emulated_opts(trackerPid, inetValues) do
    call(trackerPid, {:set_emulated_opts, inetValues})
  end

  def get_all_opts(trackerPid) do
    call(trackerPid, :get_all_opts)
  end

  def start_link(port, sockOpts, sslOpts) do
    :gen_server.start_link(:tls_socket, [port, sockOpts, sslOpts], [])
  end

  def init([port, opts, sslOpts]) do
    :erlang.process_flag(:trap_exit, true)
    true = :erlang.link(port)
    {:ok, r_state(emulated_opts: do_set_emulated_opts(opts, []), port: port, ssl_opts: sslOpts)}
  end

  def handle_call({:set_emulated_opts, opts0}, _From, r_state(emulated_opts: opts1) = state) do
    opts = do_set_emulated_opts(opts0, opts1)
    {:reply, :ok, r_state(state, emulated_opts: opts)}
  end

  def handle_call(:get_emulated_opts, _From, r_state(emulated_opts: opts) = state) do
    {:reply, {:ok, opts}, state}
  end

  def handle_call(:get_all_opts, _From, r_state(emulated_opts: emOpts, ssl_opts: sslOpts) = state) do
    {:reply, {:ok, emOpts, sslOpts}, state}
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  def handle_info({:EXIT, port, _}, r_state(port: port) = state) do
    {:stop, :normal, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp call(pid, msg) do
    :gen_server.call(pid, msg, :infinity)
  end

  def split_options(opts) do
    split_options(opts, emulated_options(), [], [])
  end

  defp split_options([], _, socketOpts, emuOpts) do
    {socketOpts, emuOpts}
  end

  defp split_options([{name, _} = opt | opts], emu, socketOpts, emuOpts) do
    case :lists.member(name, emu) do
      true ->
        split_options(opts, emu, socketOpts, [opt | emuOpts])

      false ->
        split_options(opts, emu, [opt | socketOpts], emuOpts)
    end
  end

  defp split_options([name | opts], emu, socketOptNames, emuOptNames) do
    case :lists.member(name, emu) do
      true ->
        split_options(opts, emu, socketOptNames, [name | emuOptNames])

      false ->
        split_options(opts, emu, [name | socketOptNames], emuOptNames)
    end
  end

  defp do_set_emulated_opts([], opts) do
    opts
  end

  defp do_set_emulated_opts([{:active, n0} | rest], opts)
       when is_integer(n0) do
    n =
      update_active_n(
        n0,
        :proplists.get_value(:active, opts, false)
      )

    do_set_emulated_opts(
      rest,
      [{:active, n} | :proplists.delete(:active, opts)]
    )
  end

  defp do_set_emulated_opts([{name, _} = opt | rest], opts) do
    do_set_emulated_opts(
      rest,
      [opt | :proplists.delete(name, opts)]
    )
  end

  def update_active_n(new, current) do
    cond do
      is_integer(current) and new + current <= 0 ->
        false

      is_integer(current) ->
        new + current

      new <= 0 ->
        false

      true ->
        new
    end
  end

  def get_socket_opts(_, [], _) do
    []
  end

  def get_socket_opts(listenSocket, sockOptNames, cb) do
    {:ok, opts} = cb.getopts(listenSocket, sockOptNames)
    opts
  end

  defp get_emulated_opts(trackerPid, emOptNames) do
    {:ok, emOpts} = get_emulated_opts(trackerPid)

    :lists.map(
      fn name ->
        {:value, value} = :lists.keysearch(name, 1, emOpts)
        value
      end,
      emOptNames
    )
  end

  def emulated_socket_options(
        inetValues,
        r_socket_options(
          mode: mode,
          header: header,
          active: active,
          packet: packet,
          packet_size: size
        )
      ) do
    r_socket_options(
      mode: :proplists.get_value(:mode, inetValues, mode),
      header: :proplists.get_value(:header, inetValues, header),
      active: :proplists.get_value(:active, inetValues, active),
      packet: :proplists.get_value(:packet, inetValues, packet),
      packet_size: :proplists.get_value(:packet_size, inetValues, size)
    )
  end

  defp emulated_options([{:mode, value} = opt | opts], inet, emulated) do
    validate_inet_option(:mode, value)
    emulated_options(opts, inet, [opt | :proplists.delete(:mode, emulated)])
  end

  defp emulated_options([{:header, value} = opt | opts], inet, emulated) do
    validate_inet_option(:header, value)
    emulated_options(opts, inet, [opt | :proplists.delete(:header, emulated)])
  end

  defp emulated_options([{:active, value} = opt | opts], inet, emulated) do
    validate_inet_option(:active, value)
    emulated_options(opts, inet, [opt | :proplists.delete(:active, emulated)])
  end

  defp emulated_options([{:packet, value} = opt | opts], inet, emulated) do
    validate_inet_option(:packet, value)
    emulated_options(opts, inet, [opt | :proplists.delete(:packet, emulated)])
  end

  defp emulated_options([{:packet_size, value} = opt | opts], inet, emulated) do
    validate_inet_option(:packet_size, value)
    emulated_options(opts, inet, [opt | :proplists.delete(:packet_size, emulated)])
  end

  defp emulated_options([opt | opts], inet, emulated) do
    emulated_options(opts, [opt | inet], emulated)
  end

  defp emulated_options([], inet, emulated) do
    {inet, emulated}
  end

  defp validate_inet_option(:mode, value)
       when value !== :list and
              value !== :binary do
    throw({:error, {:options, {:mode, value}}})
  end

  defp validate_inet_option(:packet, value) when not (is_atom(value) or is_integer(value)) do
    throw({:error, {:options, {:packet, value}}})
  end

  defp validate_inet_option(:packet_size, value) when not is_integer(value) do
    throw({:error, {:options, {:packet_size, value}}})
  end

  defp validate_inet_option(:header, value) when not is_integer(value) do
    throw({:error, {:options, {:header, value}}})
  end

  defp validate_inet_option(:active, value)
       when value >= -32768 and
              value <= 32767 do
    :ok
  end

  defp validate_inet_option(:active, value)
       when value !== true and
              value !== false and value !== :once do
    throw({:error, {:options, {:active, value}}})
  end

  defp validate_inet_option(_, _) do
    :ok
  end

  defp get_ticket_lifetime() do
    case :application.get_env(
           :ssl,
           :server_session_ticket_lifetime
         ) do
      {:ok, seconds}
      when is_integer(seconds) and seconds <= 604_800 ->
        seconds

      _ ->
        7200
    end
  end

  defp get_ticket_store_size() do
    case :application.get_env(
           :ssl,
           :server_session_ticket_store_size
         ) do
      {:ok, size} when is_integer(size) ->
        size

      _ ->
        1000
    end
  end
end
