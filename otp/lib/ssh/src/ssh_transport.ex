defmodule :m_ssh_transport do
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

  Record.defrecord(:r_hostent, :hostent,
    h_name: :undefined,
    h_aliases: [],
    h_addrtype: :undefined,
    h_length: :undefined,
    h_addr_list: []
  )

  Record.defrecord(:r_ssh_msg_disconnect, :ssh_msg_disconnect,
    code: :undefined,
    description: :undefined,
    language: :undefined
  )

  Record.defrecord(:r_ssh_msg_ignore, :ssh_msg_ignore, data: :undefined)
  Record.defrecord(:r_ssh_msg_unimplemented, :ssh_msg_unimplemented, sequence: :undefined)

  Record.defrecord(:r_ssh_msg_debug, :ssh_msg_debug,
    always_display: :undefined,
    message: :undefined,
    language: :undefined
  )

  Record.defrecord(:r_ssh_msg_service_request, :ssh_msg_service_request, name: :undefined)
  Record.defrecord(:r_ssh_msg_service_accept, :ssh_msg_service_accept, name: :undefined)

  Record.defrecord(:r_ssh_msg_ext_info, :ssh_msg_ext_info,
    nr_extensions: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_kexinit, :ssh_msg_kexinit,
    cookie: :undefined,
    kex_algorithms: :undefined,
    server_host_key_algorithms: :undefined,
    encryption_algorithms_client_to_server: :undefined,
    encryption_algorithms_server_to_client: :undefined,
    mac_algorithms_client_to_server: :undefined,
    mac_algorithms_server_to_client: :undefined,
    compression_algorithms_client_to_server: :undefined,
    compression_algorithms_server_to_client: :undefined,
    languages_client_to_server: :undefined,
    languages_server_to_client: :undefined,
    first_kex_packet_follows: false,
    reserved: 0
  )

  Record.defrecord(:r_ssh_msg_kexdh_init, :ssh_msg_kexdh_init, e: :undefined)

  Record.defrecord(:r_ssh_msg_kexdh_reply, :ssh_msg_kexdh_reply,
    public_host_key: :undefined,
    f: :undefined,
    h_sig: :undefined
  )

  Record.defrecord(:r_ssh_msg_newkeys, :ssh_msg_newkeys, [])

  Record.defrecord(:r_ssh_msg_kex_dh_gex_request, :ssh_msg_kex_dh_gex_request,
    min: :undefined,
    n: :undefined,
    max: :undefined
  )

  Record.defrecord(:r_ssh_msg_kex_dh_gex_request_old, :ssh_msg_kex_dh_gex_request_old,
    n: :undefined
  )

  Record.defrecord(:r_ssh_msg_kex_dh_gex_group, :ssh_msg_kex_dh_gex_group,
    p: :undefined,
    g: :undefined
  )

  Record.defrecord(:r_ssh_msg_kex_dh_gex_init, :ssh_msg_kex_dh_gex_init, e: :undefined)

  Record.defrecord(:r_ssh_msg_kex_dh_gex_reply, :ssh_msg_kex_dh_gex_reply,
    public_host_key: :undefined,
    f: :undefined,
    h_sig: :undefined
  )

  Record.defrecord(:r_ssh_msg_kex_ecdh_init, :ssh_msg_kex_ecdh_init, q_c: :undefined)

  Record.defrecord(:r_ssh_msg_kex_ecdh_reply, :ssh_msg_kex_ecdh_reply,
    public_host_key: :undefined,
    q_s: :undefined,
    h_sig: :undefined
  )

  Record.defrecord(:r_ssh, :ssh,
    role: :undefined,
    peer: :undefined,
    local: :undefined,
    c_vsn: :undefined,
    s_vsn: :undefined,
    c_version: :undefined,
    s_version: :undefined,
    c_keyinit: :undefined,
    s_keyinit: :undefined,
    send_ext_info: :undefined,
    recv_ext_info: :undefined,
    algorithms: :undefined,
    send_mac: :none,
    send_mac_key: :undefined,
    send_mac_size: 0,
    recv_mac: :none,
    recv_mac_key: :undefined,
    recv_mac_size: 0,
    encrypt: :none,
    encrypt_cipher: :undefined,
    encrypt_keys: :undefined,
    encrypt_block_size: 8,
    encrypt_ctx: :undefined,
    decrypt: :none,
    decrypt_cipher: :undefined,
    decrypt_keys: :undefined,
    decrypt_block_size: 8,
    decrypt_ctx: :undefined,
    compress: :none,
    compress_ctx: :undefined,
    decompress: :none,
    decompress_ctx: :undefined,
    c_lng: :none,
    s_lng: :none,
    user_ack: true,
    timeout: :infinity,
    shared_secret: :undefined,
    exchanged_hash: :undefined,
    session_id: :undefined,
    opts: [],
    send_sequence: 0,
    recv_sequence: 0,
    keyex_key: :undefined,
    keyex_info: :undefined,
    random_length_padding: 15,
    user: :undefined,
    service: :undefined,
    userauth_quiet_mode: :undefined,
    userauth_methods: :undefined,
    userauth_supported_methods: :undefined,
    userauth_pubkeys: :undefined,
    kb_tries_left: 0,
    userauth_preference: :undefined,
    available_host_keys: :undefined,
    pwdfun_user_state: :undefined,
    authenticated: false
  )

  Record.defrecord(:r_alg, :alg,
    kex: :undefined,
    hkey: :undefined,
    send_mac: :undefined,
    recv_mac: :undefined,
    encrypt: :undefined,
    decrypt: :undefined,
    compress: :undefined,
    decompress: :undefined,
    c_lng: :undefined,
    s_lng: :undefined,
    send_ext_info: :undefined,
    recv_ext_info: :undefined
  )

  Record.defrecord(:r_ssh_pty, :ssh_pty,
    term: '',
    width: 80,
    height: 25,
    pixel_width: 1024,
    pixel_height: 768,
    modes: <<>>
  )

  Record.defrecord(:r_circ_buf_entry, :circ_buf_entry,
    module: :undefined,
    line: :undefined,
    function: :undefined,
    pid: self(),
    value: :undefined
  )

  @behaviour :ssh_dbg
  def clear_default_algorithms_env() do
    :application.unset_env(:ssh, :"$def-algs$")
  end

  def default_algorithms() do
    case :application.get_env(:ssh, :"$def-algs$") do
      :undefined ->
        opts = get_alg_conf()

        algs1 =
          case :proplists.get_value(
                 :preferred_algorithms,
                 opts
               ) do
            :undefined ->
              for k <- algo_classes() do
                {k, default_algorithms1(k)}
              end

            algs0 ->
              {true, algs01} = :ssh_options.check_preferred_algorithms(algs0)
              algs01
          end

        algs =
          case :proplists.get_value(
                 :modify_algorithms,
                 opts
               ) do
            :undefined ->
              algs1

            modifications ->
              :ssh_options.initial_default_algorithms(
                algs1,
                modifications
              )
          end

        :application.set_env(:ssh, :"$def-algs$", algs)
        algs

      {:ok, algs} ->
        algs
    end
  end

  defp get_alg_conf() do
    for t <- [:preferred_algorithms, :modify_algorithms],
        l <- [:application.get_env(:ssh, t, [])],
        l !== [] do
      {t, l}
    end
  end

  def algo_classes() do
    [:kex, :public_key, :cipher, :mac, :compression]
  end

  def algo_class(:kex) do
    true
  end

  def algo_class(:public_key) do
    true
  end

  def algo_class(:cipher) do
    true
  end

  def algo_class(:mac) do
    true
  end

  def algo_class(:compression) do
    true
  end

  def algo_class(_) do
    false
  end

  def algo_two_spec_classes() do
    [:cipher, :mac, :compression]
  end

  def algo_two_spec_class(:cipher) do
    true
  end

  def algo_two_spec_class(:mac) do
    true
  end

  def algo_two_spec_class(:compression) do
    true
  end

  def algo_two_spec_class(_) do
    false
  end

  def default_algorithms(tag) do
    case :application.get_env(:ssh, :"$def-algs$") do
      :undefined ->
        default_algorithms1(tag)

      {:ok, algs} ->
        :proplists.get_value(tag, algs, [])
    end
  end

  defp default_algorithms1(:kex) do
    supported_algorithms(:kex, [
      :"diffie-hellman-group1-sha1",
      :"diffie-hellman-group14-sha1",
      :"diffie-hellman-group-exchange-sha1"
    ])
  end

  defp default_algorithms1(:cipher) do
    supported_algorithms(
      :cipher,
      same([:AEAD_AES_128_GCM, :AEAD_AES_256_GCM])
    )
  end

  defp default_algorithms1(:mac) do
    supported_algorithms(
      :mac,
      same([:AEAD_AES_128_GCM, :AEAD_AES_256_GCM, :"hmac-sha1-96"])
    )
  end

  defp default_algorithms1(:public_key) do
    supported_algorithms(:public_key, [:"ssh-dss"])
  end

  defp default_algorithms1(alg) do
    supported_algorithms(alg, [])
  end

  def supported_algorithms() do
    for k <- algo_classes() do
      {k, supported_algorithms(k)}
    end
  end

  def supported_algorithms(:kex) do
    select_crypto_supported([
      {:"ecdh-sha2-nistp384", [{:public_keys, :ecdh}, {:curves, :secp384r1}, {:hashs, :sha384}]},
      {:"ecdh-sha2-nistp521", [{:public_keys, :ecdh}, {:curves, :secp521r1}, {:hashs, :sha512}]},
      {:"ecdh-sha2-nistp256", [{:public_keys, :ecdh}, {:curves, :secp256r1}, {:hashs, :sha256}]},
      {:"diffie-hellman-group-exchange-sha256", [{:public_keys, :dh}, {:hashs, :sha256}]},
      {:"diffie-hellman-group16-sha512", [{:public_keys, :dh}, {:hashs, :sha512}]},
      {:"diffie-hellman-group18-sha512", [{:public_keys, :dh}, {:hashs, :sha512}]},
      {:"diffie-hellman-group14-sha256", [{:public_keys, :dh}, {:hashs, :sha256}]},
      {:"curve25519-sha256", [{:public_keys, :ecdh}, {:curves, :x25519}, {:hashs, :sha256}]},
      {:"curve25519-sha256@libssh.org",
       [{:public_keys, :ecdh}, {:curves, :x25519}, {:hashs, :sha256}]},
      {:"curve448-sha512", [{:public_keys, :ecdh}, {:curves, :x448}, {:hashs, :sha512}]},
      {:"diffie-hellman-group14-sha1", [{:public_keys, :dh}, {:hashs, :sha}]},
      {:"diffie-hellman-group-exchange-sha1", [{:public_keys, :dh}, {:hashs, :sha}]},
      {:"diffie-hellman-group1-sha1", [{:public_keys, :dh}, {:hashs, :sha}]}
    ])
  end

  def supported_algorithms(:public_key) do
    select_crypto_supported([
      {:"ecdsa-sha2-nistp384",
       [{:public_keys, :ecdsa}, {:hashs, :sha384}, {:curves, :secp384r1}]},
      {:"ecdsa-sha2-nistp521",
       [{:public_keys, :ecdsa}, {:hashs, :sha512}, {:curves, :secp521r1}]},
      {:"ecdsa-sha2-nistp256",
       [{:public_keys, :ecdsa}, {:hashs, :sha256}, {:curves, :secp256r1}]},
      {:"ssh-ed25519", [{:public_keys, :eddsa}, {:curves, :ed25519}]},
      {:"ssh-ed448", [{:public_keys, :eddsa}, {:curves, :ed448}]},
      {:"rsa-sha2-256", [{:public_keys, :rsa}, {:hashs, :sha256}]},
      {:"rsa-sha2-512", [{:public_keys, :rsa}, {:hashs, :sha512}]},
      {:"ssh-rsa", [{:public_keys, :rsa}, {:hashs, :sha}]},
      {:"ssh-dss", [{:public_keys, :dss}, {:hashs, :sha}]}
    ])
  end

  def supported_algorithms(:cipher) do
    same(
      select_crypto_supported([
        {:"chacha20-poly1305@openssh.com", [{:ciphers, :chacha20}, {:macs, :poly1305}]},
        {:"aes256-gcm@openssh.com", [{:ciphers, :aes_256_gcm}]},
        {:"aes256-ctr", [{:ciphers, :aes_256_ctr}]},
        {:"aes192-ctr", [{:ciphers, :aes_192_ctr}]},
        {:"aes128-gcm@openssh.com", [{:ciphers, :aes_128_gcm}]},
        {:"aes128-ctr", [{:ciphers, :aes_128_ctr}]},
        {:AEAD_AES_256_GCM, [{:ciphers, :aes_256_gcm}]},
        {:AEAD_AES_128_GCM, [{:ciphers, :aes_128_gcm}]},
        {:"aes256-cbc", [{:ciphers, :aes_256_cbc}]},
        {:"aes192-cbc", [{:ciphers, :aes_192_cbc}]},
        {:"aes128-cbc", [{:ciphers, :aes_128_cbc}]},
        {:"3des-cbc", [{:ciphers, :des_ede3_cbc}]}
      ])
    )
  end

  def supported_algorithms(:mac) do
    same(
      select_crypto_supported([
        {:"hmac-sha2-256-etm@openssh.com", [{:macs, :hmac}, {:hashs, :sha256}]},
        {:"hmac-sha2-512-etm@openssh.com", [{:macs, :hmac}, {:hashs, :sha256}]},
        {:"hmac-sha2-256", [{:macs, :hmac}, {:hashs, :sha256}]},
        {:"hmac-sha2-512", [{:macs, :hmac}, {:hashs, :sha512}]},
        {:"hmac-sha1-etm@openssh.com", [{:macs, :hmac}, {:hashs, :sha256}]},
        {:"hmac-sha1", [{:macs, :hmac}, {:hashs, :sha}]},
        {:"hmac-sha1-96", [{:macs, :hmac}, {:hashs, :sha}]},
        {:AEAD_AES_128_GCM, [{:ciphers, :aes_128_gcm}]},
        {:AEAD_AES_256_GCM, [{:ciphers, :aes_256_gcm}]}
      ])
    )
  end

  def supported_algorithms(:compression) do
    same([:none, :"zlib@openssh.com", :zlib])
  end

  def versions(:client, options) do
    vsn =
      :ssh_options.get_value(
        :internal_options,
        :vsn,
        options,
        fn ->
          {2, 0}
        end,
        :ssh_transport,
        252
      )

    {vsn, format_version(vsn, software_version(options))}
  end

  def versions(:server, options) do
    vsn =
      :ssh_options.get_value(
        :internal_options,
        :vsn,
        options,
        fn ->
          {2, 0}
        end,
        :ssh_transport,
        255
      )

    {vsn, format_version(vsn, software_version(options))}
  end

  defp format_version({major, minor}, '') do
    :lists.concat(['SSH-', major, '.', minor])
  end

  defp format_version({major, minor}, softwareVersion) do
    :lists.concat(['SSH-', major, '.', minor, '-', softwareVersion])
  end

  defp software_version(options) do
    case :ssh_options.get_value(:user_options, :id_string, options, :ssh_transport, 264) do
      {:random, nlo, nup} ->
        random_id(nlo, nup)

      iD ->
        iD
    end
  end

  defp random_id(nlo, nup) do
    for _ <-
          :lists.duplicate(
            nlo + :rand.uniform(nup - nlo + 1) - 1,
            :x
          ) do
      ?a + :rand.uniform(?z - ?a + 1) - 1
    end
  end

  def hello_version_msg(data) do
    [data, '\r\n']
  end

  def next_seqnum(seqNum) do
    seqNum + 1 &&& 4_294_967_295
  end

  defp is_valid_mac(_, _, r_ssh(recv_mac_size: 0)) do
    true
  end

  defp is_valid_mac(
         mac,
         data,
         r_ssh(recv_mac: algorithm, recv_mac_key: key, recv_sequence: seqNum)
       ) do
    :crypto.equal_const_time(
      mac,
      mac(algorithm, key, seqNum, data)
    )
  end

  def handle_hello_version(version) do
    try do
      strVersion = trim_tail(version)

      case :string.tokens(version, '-') do
        [[_, '2.0'] | _] ->
          {{2, 0}, strVersion}

        [[_, '1.99'] | _] ->
          {{2, 0}, strVersion}

        [[_, '1.3'] | _] ->
          {{1, 3}, strVersion}

        [[_, '1.5'] | _] ->
          {{1, 5}, strVersion}
      end
    catch
      :error, _ ->
        {:undefined, 'unknown version'}
    end
  end

  def key_exchange_init_msg(ssh0) do
    msg = kex_init(ssh0)
    {sshPacket, ssh} = ssh_packet(msg, ssh0)
    {msg, sshPacket, ssh}
  end

  defp kex_init(r_ssh(role: role, opts: opts, available_host_keys: hostKeyAlgs) = ssh) do
    random = :ssh_bits.random(16)

    prefAlgs =
      adjust_algs_for_peer_version(
        role,
        :ssh_options.get_value(
          :user_options,
          :preferred_algorithms,
          opts,
          :ssh_transport,
          311
        ),
        ssh
      )

    kexinit_message(role, random, prefAlgs, hostKeyAlgs, opts)
  end

  def key_init(:client, ssh, value) do
    r_ssh(ssh, c_keyinit: value)
  end

  def key_init(:server, ssh, value) do
    r_ssh(ssh, s_keyinit: value)
  end

  defp adjust_algs_for_peer_version(:client, prefAlgs, r_ssh(s_version: v)) do
    adjust_algs_for_peer_version(v, prefAlgs)
  end

  defp adjust_algs_for_peer_version(:server, prefAlgs, r_ssh(c_version: v)) do
    adjust_algs_for_peer_version(v, prefAlgs)
  end

  def adjust_algs_for_peer_version('SSH-2.0-OpenSSH_6.2' ++ _, prefAlgs) do
    c0 = :proplists.get_value(:cipher, prefAlgs, same([]))

    c =
      for d <- [:client2server, :server2client],
          l <- [
            for k <- :proplists.get_value(d, c0, []),
                k !== :"aes256-gcm@openssh.com",
                k !== :"aes128-gcm@openssh.com" do
              k
            end
          ] do
        {d, l}
      end

    :lists.keyreplace(:cipher, 1, prefAlgs, {:cipher, c})
  end

  def adjust_algs_for_peer_version(_, prefAlgs) do
    prefAlgs
  end

  defp kexinit_message(role, random, algs, hostKeyAlgs, opts) do
    r_ssh_msg_kexinit(
      cookie: random,
      kex_algorithms:
        to_strings(
          get_algs(
            :kex,
            algs
          )
        ) ++
          kex_ext_info(
            role,
            opts
          ),
      server_host_key_algorithms: hostKeyAlgs,
      encryption_algorithms_client_to_server:
        c2s(
          :cipher,
          algs
        ),
      encryption_algorithms_server_to_client:
        s2c(
          :cipher,
          algs
        ),
      mac_algorithms_client_to_server: c2s(:mac, algs),
      mac_algorithms_server_to_client: s2c(:mac, algs),
      compression_algorithms_client_to_server:
        c2s(
          :compression,
          algs
        ),
      compression_algorithms_server_to_client:
        s2c(
          :compression,
          algs
        ),
      languages_client_to_server: [],
      languages_server_to_client: []
    )
  end

  defp c2s(key, algs) do
    x2y(:client2server, key, algs)
  end

  defp s2c(key, algs) do
    x2y(:server2client, key, algs)
  end

  defp x2y(directionKey, key, algs) do
    to_strings(
      :proplists.get_value(
        directionKey,
        get_algs(key, algs)
      )
    )
  end

  defp get_algs(key, algs) do
    :proplists.get_value(key, algs, default_algorithms(key))
  end

  defp to_strings(l) do
    :lists.map(&:erlang.atom_to_list/1, l)
  end

  def new_keys_message(ssh0) do
    {sshPacket, ssh1} = ssh_packet(r_ssh_msg_newkeys(), ssh0)
    ssh = install_alg(:snd, ssh1)
    {:ok, sshPacket, ssh}
  end

  def handle_kexinit_msg(
        r_ssh_msg_kexinit() = counterPart,
        r_ssh_msg_kexinit() = own,
        r_ssh(role: :client) = ssh
      ) do
    try do
      {:ok, algorithms} = select_algorithm(:client, own, counterPart, r_ssh(ssh, :opts))
      true = verify_algorithm(algorithms)
      algorithms
    catch
      class, error ->
        :ssh_connection_handler.disconnect(
          3,
          :io_lib.format('Kexinit failed in client: ~p:~p', [class, error]),
          :ssh_transport,
          380
        )
    else
      algos ->
        key_exchange_first_msg(
          r_alg(algos, :kex),
          r_ssh(ssh, algorithms: algos)
        )
    end
  end

  def handle_kexinit_msg(
        r_ssh_msg_kexinit() = counterPart,
        r_ssh_msg_kexinit() = own,
        r_ssh(role: :server) = ssh
      ) do
    try do
      {:ok, algorithms} = select_algorithm(:server, counterPart, own, r_ssh(ssh, :opts))
      true = verify_algorithm(algorithms)
      algorithms
    catch
      class, error ->
        :ssh_connection_handler.disconnect(
          3,
          :io_lib.format('Kexinit failed in server: ~p:~p', [class, error]),
          :ssh_transport,
          397
        )
    else
      algos ->
        {:ok, r_ssh(ssh, algorithms: algos)}
    end
  end

  defp verify_algorithm(r_alg(kex: :undefined)) do
    {false, 'kex'}
  end

  defp verify_algorithm(r_alg(hkey: :undefined)) do
    {false, 'hkey'}
  end

  defp verify_algorithm(r_alg(send_mac: :undefined)) do
    {false, 'send_mac'}
  end

  defp verify_algorithm(r_alg(recv_mac: :undefined)) do
    {false, 'recv_mac'}
  end

  defp verify_algorithm(r_alg(encrypt: :undefined)) do
    {false, 'encrypt'}
  end

  defp verify_algorithm(r_alg(decrypt: :undefined)) do
    {false, 'decrypt'}
  end

  defp verify_algorithm(r_alg(compress: :undefined)) do
    {false, 'compress'}
  end

  defp verify_algorithm(r_alg(decompress: :undefined)) do
    {false, 'decompress'}
  end

  defp verify_algorithm(r_alg(kex: kex)) do
    case :lists.member(kex, supported_algorithms(:kex)) do
      true ->
        true

      false ->
        {false, 'kex'}
    end
  end

  defp key_exchange_first_msg(kex, ssh0)
       when kex == :"diffie-hellman-group1-sha1" or kex == :"diffie-hellman-group14-sha1" or
              kex == :"diffie-hellman-group14-sha256" or kex == :"diffie-hellman-group16-sha512" or
              kex == :"diffie-hellman-group18-sha512" do
    {g, p} = dh_group(kex)
    sz = dh_bits(r_ssh(ssh0, :algorithms))
    {public, private} = generate_key(:dh, [p, g, 2 * sz])
    {sshPacket, ssh1} = ssh_packet(r_ssh_msg_kexdh_init(e: public), ssh0)
    {:ok, sshPacket, r_ssh(ssh1, keyex_key: {{private, public}, {g, p}})}
  end

  defp key_exchange_first_msg(kex, ssh0 = r_ssh(opts: opts))
       when kex == :"diffie-hellman-group-exchange-sha1" or
              kex == :"diffie-hellman-group-exchange-sha256" do
    {min, nBits0, max} =
      :ssh_options.get_value(:user_options, :dh_gex_limits, opts, :ssh_transport, 437)

    dhBits = dh_bits(r_ssh(ssh0, :algorithms))

    nBits1 =
      cond do
        dhBits <= 112 ->
          2048

        dhBits <= 128 ->
          3072

        dhBits <= 192 ->
          7680

        true ->
          8192
      end

    nBits = min(max(max(nBits0, nBits1), min), max)

    {sshPacket, ssh1} =
      ssh_packet(
        r_ssh_msg_kex_dh_gex_request(min: min, n: nBits, max: max),
        ssh0
      )

    {:ok, sshPacket, r_ssh(ssh1, keyex_info: {min, max, nBits})}
  end

  defp key_exchange_first_msg(kex, ssh0)
       when kex == :"ecdh-sha2-nistp256" or kex == :"ecdh-sha2-nistp384" or
              kex == :"ecdh-sha2-nistp521" or kex == :"curve25519-sha256" or
              kex == :"curve25519-sha256@libssh.org" or kex == :"curve448-sha512" do
    curve = ecdh_curve(kex)
    {public, private} = generate_key(:ecdh, curve)
    {sshPacket, ssh1} = ssh_packet(r_ssh_msg_kex_ecdh_init(q_c: public), ssh0)
    {:ok, sshPacket, r_ssh(ssh1, keyex_key: {{public, private}, curve})}
  end

  def handle_kexdh_init(
        r_ssh_msg_kexdh_init(e: e),
        ssh0 =
          r_ssh(
            algorithms: r_alg(kex: kex, hkey: signAlg) = algs,
            opts: opts
          )
      ) do
    {g, p} = dh_group(kex)

    cond do
      1 <= e and e <= p - 1 ->
        sz = dh_bits(algs)
        {public, private} = generate_key(:dh, [p, g, 2 * sz])
        k = compute_key(:dh, e, private, [p, g])
        myPrivHostKey = get_host_key(signAlg, opts)
        myPubHostKey = extract_public_key(myPrivHostKey)
        h = kex_hash(ssh0, myPubHostKey, sha(kex), {e, public, k})
        h_SIG = sign(h, sha(signAlg), myPrivHostKey)

        {sshPacket, ssh1} =
          ssh_packet(
            r_ssh_msg_kexdh_reply(
              public_host_key: {myPubHostKey, signAlg},
              f: public,
              h_sig: h_SIG
            ),
            ssh0
          )

        {:ok, sshPacket,
         r_ssh(ssh1,
           keyex_key: {{private, public}, {g, p}},
           shared_secret: :ssh_bits.mpint(k),
           exchanged_hash: h,
           session_id: sid(ssh1, h)
         )}

      true ->
        :ssh_connection_handler.disconnect(
          3,
          :io_lib.format('Kexdh init failed, received \'e\' out of bounds~n  E=~p~n  P=~p', [e, p]),
          :ssh_transport,
          505
        )
    end
  end

  def handle_kexdh_reply(
        r_ssh_msg_kexdh_reply(public_host_key: peerPubHostKey, f: f, h_sig: h_SIG),
        r_ssh(
          keyex_key: {{private, public}, {g, p}},
          algorithms: r_alg(kex: kex)
        ) = ssh0
      ) do
    cond do
      1 <= f and f <= p - 1 ->
        k = compute_key(:dh, f, private, [p, g])
        h = kex_hash(ssh0, peerPubHostKey, sha(kex), {public, f, k})

        case verify_host_key(ssh0, peerPubHostKey, h, h_SIG) do
          :ok ->
            {sshPacket, ssh} = ssh_packet(r_ssh_msg_newkeys(), ssh0)

            {:ok, sshPacket,
             install_alg(
               :snd,
               r_ssh(ssh,
                 shared_secret: :ssh_bits.mpint(k),
                 exchanged_hash: h,
                 session_id: sid(ssh, h)
               )
             )}

          error ->
            :ssh_connection_handler.disconnect(
              3,
              :io_lib.format('Kexdh init failed. Verify host key: ~p', [error]),
              :ssh_transport,
              527
            )
        end

      true ->
        :ssh_connection_handler.disconnect(
          3,
          :io_lib.format('Kexdh init failed, received \'f\' out of bounds~n  F=~p~n  P=~p', [f, p]),
          :ssh_transport,
          534
        )
    end
  end

  def handle_kex_dh_gex_request(
        r_ssh_msg_kex_dh_gex_request(min: min0, n: nBits, max: max0),
        ssh0 = r_ssh(opts: opts)
      )
      when min0 <= nBits and nBits <= max0 do
    {min, max} = adjust_gex_min_max(min0, max0, opts)

    case :public_key.dh_gex_group(
           min,
           nBits,
           max,
           :ssh_options.get_value(
             :user_options,
             :dh_gex_groups,
             opts,
             :ssh_transport,
             550
           )
         ) do
      {:ok, {_, {g, p}}} ->
        {sshPacket, ssh} = ssh_packet(r_ssh_msg_kex_dh_gex_group(p: p, g: g), ssh0)

        {:ok, sshPacket,
         r_ssh(ssh,
           keyex_key: {:x, {g, p}},
           keyex_info: {min0, max0, nBits}
         )}

      {:error, _} ->
        :ssh_connection_handler.disconnect(
          3,
          :io_lib.format('No possible diffie-hellman-group-exchange group found', []),
          :ssh_transport,
          560
        )
    end
  end

  def handle_kex_dh_gex_request(
        r_ssh_msg_kex_dh_gex_request_old(n: nBits),
        ssh0 = r_ssh(opts: opts)
      ) do
    min0 = nBits
    max0 = 8192
    {min, max} = adjust_gex_min_max(min0, max0, opts)

    case :public_key.dh_gex_group(
           min,
           nBits,
           max,
           :ssh_options.get_value(
             :user_options,
             :dh_gex_groups,
             opts,
             :ssh_transport,
             582
           )
         ) do
      {:ok, {_, {g, p}}} ->
        {sshPacket, ssh} = ssh_packet(r_ssh_msg_kex_dh_gex_group(p: p, g: g), ssh0)

        {:ok, sshPacket,
         r_ssh(ssh,
           keyex_key: {:x, {g, p}},
           keyex_info: {-1, -1, nBits}
         )}

      {:error, _} ->
        :ssh_connection_handler.disconnect(
          3,
          :io_lib.format('No possible diffie-hellman-group-exchange group found', []),
          :ssh_transport,
          592
        )
    end
  end

  def handle_kex_dh_gex_request(_, _) do
    :ssh_connection_handler.disconnect(
      3,
      'Key exchange failed, bad values in ssh_msg_kex_dh_gex_request',
      :ssh_transport,
      598
    )
  end

  defp adjust_gex_min_max(min0, max0, opts) do
    {min1, max1} =
      :ssh_options.get_value(:user_options, :dh_gex_limits, opts, :ssh_transport, 601)

    min2 = max(min0, min1)
    max2 = min(max0, max1)

    cond do
      min2 <= max2 ->
        {min2, max2}

      max2 < min2 ->
        :ssh_connection_handler.disconnect(
          2,
          'No possible diffie-hellman-group-exchange group possible',
          :ssh_transport,
          609
        )
    end
  end

  def handle_kex_dh_gex_group(r_ssh_msg_kex_dh_gex_group(p: p, g: g), ssh0) do
    sz = dh_bits(r_ssh(ssh0, :algorithms))
    {public, private} = generate_key(:dh, [p, g, 2 * sz])
    {sshPacket, ssh1} = ssh_packet(r_ssh_msg_kex_dh_gex_init(e: public), ssh0)
    {:ok, sshPacket, r_ssh(ssh1, keyex_key: {{private, public}, {g, p}})}
  end

  def handle_kex_dh_gex_init(
        r_ssh_msg_kex_dh_gex_init(e: e),
        r_ssh(
          keyex_key: {{private, public}, {g, p}},
          keyex_info: {min, max, nBits},
          algorithms: r_alg(kex: kex, hkey: signAlg),
          opts: opts
        ) = ssh0
      ) do
    cond do
      1 <= e and e <= p - 1 ->
        k = compute_key(:dh, e, private, [p, g])

        cond do
          1 < k and k < p - 1 ->
            myPrivHostKey = get_host_key(signAlg, opts)
            myPubHostKey = extract_public_key(myPrivHostKey)
            h = kex_hash(ssh0, myPubHostKey, sha(kex), {min, nBits, max, p, g, e, public, k})
            h_SIG = sign(h, sha(signAlg), myPrivHostKey)

            {sshPacket, ssh} =
              ssh_packet(
                r_ssh_msg_kex_dh_gex_reply(
                  public_host_key: {myPubHostKey, signAlg},
                  f: public,
                  h_sig: h_SIG
                ),
                ssh0
              )

            {:ok, sshPacket,
             r_ssh(ssh,
               shared_secret: :ssh_bits.mpint(k),
               exchanged_hash: h,
               session_id: sid(ssh, h)
             )}

          true ->
            :ssh_connection_handler.disconnect(
              3,
              'Kexdh init failed, received \'k\' out of bounds',
              :ssh_transport,
              649
            )
        end

      true ->
        :ssh_connection_handler.disconnect(
          3,
          :io_lib.format('Kexdh gex init failed, received \'e\' out of bounds~n  E=~p~n  P=~p', [
            e,
            p
          ]),
          :ssh_transport,
          655
        )
    end
  end

  def handle_kex_dh_gex_reply(
        r_ssh_msg_kex_dh_gex_reply(public_host_key: peerPubHostKey, f: f, h_sig: h_SIG),
        r_ssh(
          keyex_key: {{private, public}, {g, p}},
          keyex_info: {min, max, nBits},
          algorithms: r_alg(kex: kex)
        ) = ssh0
      ) do
    cond do
      1 <= f and f <= p - 1 ->
        k = compute_key(:dh, f, private, [p, g])

        cond do
          1 < k and k < p - 1 ->
            h = kex_hash(ssh0, peerPubHostKey, sha(kex), {min, nBits, max, p, g, public, f, k})

            case verify_host_key(ssh0, peerPubHostKey, h, h_SIG) do
              :ok ->
                {sshPacket, ssh} = ssh_packet(r_ssh_msg_newkeys(), ssh0)

                {:ok, sshPacket,
                 install_alg(
                   :snd,
                   r_ssh(ssh,
                     shared_secret: :ssh_bits.mpint(k),
                     exchanged_hash: h,
                     session_id: sid(ssh, h)
                   )
                 )}

              error ->
                :ssh_connection_handler.disconnect(
                  3,
                  :io_lib.format('Kexdh gex reply failed. Verify host key: ~p', [error]),
                  :ssh_transport,
                  681
                )
            end

          true ->
            :ssh_connection_handler.disconnect(
              3,
              'Kexdh gex init failed, \'K\' out of bounds',
              :ssh_transport,
              687
            )
        end

      true ->
        :ssh_connection_handler.disconnect(
          3,
          :io_lib.format('Kexdh gex init failed, received \'f\' out of bounds~n  F=~p~n  P=~p', [
            f,
            p
          ]),
          :ssh_transport,
          693
        )
    end
  end

  def handle_kex_ecdh_init(
        r_ssh_msg_kex_ecdh_init(q_c: peerPublic),
        ssh0 =
          r_ssh(
            algorithms: r_alg(kex: kex, hkey: signAlg),
            opts: opts
          )
      ) do
    curve = ecdh_curve(kex)
    {myPublic, myPrivate} = generate_key(:ecdh, curve)

    try do
      compute_key(:ecdh, peerPublic, myPrivate, curve)
    catch
      class, error ->
        :ssh_connection_handler.disconnect(
          3,
          :io_lib.format(
            'ECDH compute key failed in server: ~p:~p~nKex: ~p, Curve: ~p~nPeerPublic: ~p',
            [class, error, kex, curve, peerPublic]
          ),
          :ssh_transport,
          731
        )
    else
      k ->
        myPrivHostKey = get_host_key(signAlg, opts)
        myPubHostKey = extract_public_key(myPrivHostKey)
        h = kex_hash(ssh0, myPubHostKey, sha(curve), {peerPublic, myPublic, k})
        h_SIG = sign(h, sha(signAlg), myPrivHostKey)

        {sshPacket, ssh1} =
          ssh_packet(
            r_ssh_msg_kex_ecdh_reply(
              public_host_key: {myPubHostKey, signAlg},
              q_s: myPublic,
              h_sig: h_SIG
            ),
            ssh0
          )

        {:ok, sshPacket,
         r_ssh(ssh1,
           keyex_key: {{myPublic, myPrivate}, curve},
           shared_secret: :ssh_bits.mpint(k),
           exchanged_hash: h,
           session_id: sid(ssh1, h)
         )}
    end
  end

  def handle_kex_ecdh_reply(
        r_ssh_msg_kex_ecdh_reply(public_host_key: peerPubHostKey, q_s: peerPublic, h_sig: h_SIG),
        r_ssh(keyex_key: {{myPublic, myPrivate}, curve}) = ssh0
      ) do
    try do
      compute_key(:ecdh, peerPublic, myPrivate, curve)
    catch
      class, error ->
        :ssh_connection_handler.disconnect(
          3,
          :io_lib.format('Peer ECDH public key seem invalid: ~p:~p', [class, error]),
          :ssh_transport,
          762
        )
    else
      k ->
        h = kex_hash(ssh0, peerPubHostKey, sha(curve), {myPublic, peerPublic, k})

        case verify_host_key(ssh0, peerPubHostKey, h, h_SIG) do
          :ok ->
            {sshPacket, ssh} = ssh_packet(r_ssh_msg_newkeys(), ssh0)

            {:ok, sshPacket,
             install_alg(
               :snd,
               r_ssh(ssh,
                 shared_secret: :ssh_bits.mpint(k),
                 exchanged_hash: h,
                 session_id: sid(ssh, h)
               )
             )}

          error ->
            :ssh_connection_handler.disconnect(
              3,
              :io_lib.format('ECDH reply failed. Verify host key: ~p', [error]),
              :ssh_transport,
              755
            )
        end
    end
  end

  def handle_new_keys(r_ssh_msg_newkeys(), ssh0) do
    try do
      install_alg(:rcv, ssh0)
    catch
      class, error ->
        :ssh_connection_handler.disconnect(
          2,
          :io_lib.format('Install alg failed: ~p:~p', [class, error]),
          :ssh_transport,
          776
        )
    else
      r_ssh() = ssh ->
        {:ok, ssh}
    end
  end

  defp kex_ext_info(role, opts) do
    case :ssh_options.get_value(:user_options, :recv_ext_info, opts, :ssh_transport, 783) do
      true when role == :client ->
        ['ext-info-c']

      true when role == :server ->
        ['ext-info-s']

      false ->
        []
    end
  end

  def ext_info_message(r_ssh(role: :client, send_ext_info: true, opts: opts) = ssh0) do
    case :proplists.get_value(
           :ext_info_client,
           :ssh_options.get_value(:user_options, :tstflg, opts, :ssh_transport, 795)
         ) do
      true ->
        msg =
          r_ssh_msg_ext_info(
            nr_extensions: 1,
            data: [{'test@erlang.org', 'Testing,PleaseIgnore'}]
          )

        {sshPacket, ssh} = ssh_packet(msg, ssh0)
        {:ok, sshPacket, ssh}

      _ ->
        {:ok, '', ssh0}
    end
  end

  def ext_info_message(r_ssh(role: :server, send_ext_info: true, opts: opts) = ssh0) do
    algsList =
      :lists.map(
        &:erlang.atom_to_list/1,
        :ssh_options.get_value(:user_options, :pref_public_key_algs, opts, :ssh_transport, 810)
      )

    msg =
      r_ssh_msg_ext_info(
        nr_extensions: 1,
        data: [{'server-sig-algs', :string.join(algsList, ',')}]
      )

    {sshPacket, ssh} = ssh_packet(msg, ssh0)
    {:ok, sshPacket, ssh}
  end

  def ext_info_message(ssh0) do
    {:ok, '', ssh0}
  end

  defp sid(r_ssh(session_id: :undefined), h) do
    h
  end

  defp sid(r_ssh(session_id: id), _) do
    id
  end

  def get_host_key(signAlg, opts) do
    case call_KeyCb(:host_key, [signAlg], opts) do
      {:ok, privHostKey} ->
        case valid_key_sha_alg(:private, privHostKey, signAlg) do
          true ->
            privHostKey

          false ->
            exit({:error, :bad_hostkey})
        end

      result ->
        exit({:error, {result, :unsupported_key_type}})
    end
  end

  def call_KeyCb(f, args, opts) do
    {keyCb, keyCbOpts} = :ssh_options.get_value(:user_options, :key_cb, opts, :ssh_transport, 841)
    userOpts = :ssh_options.get_value(:user_options, :user_options, opts, :ssh_transport, 842)
    apply(keyCb, f, args ++ [[{:key_cb_private, keyCbOpts} | userOpts]])
  end

  def extract_public_key(r_RSAPrivateKey(modulus: n, publicExponent: e)) do
    r_RSAPublicKey(modulus: n, publicExponent: e)
  end

  def extract_public_key(r_DSAPrivateKey(y: y, p: p, q: q, g: g)) do
    {y, r_Dss_Parms(p: p, q: q, g: g)}
  end

  def extract_public_key(r_ECPrivateKey(parameters: {:namedCurve, oID}, publicKey: q))
      when is_tuple(oID) do
    {r_ECPoint(point: q), {:namedCurve, oID}}
  end

  def extract_public_key({:ed_pri, alg, pub, _Priv}) do
    {:ed_pub, alg, pub}
  end

  def extract_public_key(%{:engine => _, :key_id => _, :algorithm => alg} = m) do
    case {alg, :crypto.privkey_to_pubkey(alg, m)} do
      {:rsa, [e, n]} ->
        r_RSAPublicKey(modulus: n, publicExponent: e)

      {:dss, [p, q, g, y]} ->
        {y, r_Dss_Parms(p: p, q: q, g: g)}
    end
  end

  defp verify_host_key(r_ssh(algorithms: alg) = sSH, publicKey, digest, {algStr, signature}) do
    case :erlang.atom_to_list(r_alg(alg, :hkey)) do
      ^algStr ->
        case verify(digest, sha(r_alg(alg, :hkey)), signature, publicKey, sSH) do
          false ->
            {:error, :bad_signature}

          true ->
            known_host_key(sSH, publicKey, public_algo(publicKey))
        end

      _ ->
        {:error, :bad_signature_name}
    end
  end

  defp accepted_host(ssh, peerName, port, public, opts) do
    portStr =
      case port do
        22 ->
          ''

        _ ->
          :lists.concat([':', port])
      end

    case :ssh_options.get_value(:user_options, :silently_accept_hosts, opts, :ssh_transport, 884) do
      false ->
        :yes == yes_no(ssh, 'New host ' ++ peerName ++ portStr ++ ' accept')

      true ->
        true

      {false, alg} ->
        hostKeyAlg = r_alg(r_ssh(ssh, :algorithms), :hkey)

        prompt =
          :io_lib.format(
            'The authenticity of the host can\'t be established.~n~s host key fingerprint is ~s.~nNew host ~p~p accept',
            [
              fmt_hostkey(hostKeyAlg),
              :public_key.ssh_hostkey_fingerprint(
                alg,
                public
              ),
              peerName,
              portStr
            ]
          )

        :yes == yes_no(ssh, prompt)

      f when is_function(f, 2) ->
        case (try do
                f.(
                  peerName,
                  :public_key.ssh_hostkey_fingerprint(public)
                )
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          true ->
            true

          _ ->
            {:error, :fingerprint_check_failed}
        end

      f when is_function(f, 3) ->
        case (try do
                f.(peerName, port, :public_key.ssh_hostkey_fingerprint(public))
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          true ->
            true

          _ ->
            {:error, :fingerprint_check_failed}
        end

      {digestAlg, f} when is_function(f, 2) ->
        case (try do
                f.(
                  peerName,
                  :public_key.ssh_hostkey_fingerprint(digestAlg, public)
                )
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          true ->
            true

          _ ->
            {:error, {:fingerprint_check_failed, digestAlg}}
        end

      {digestAlg, f} when is_function(f, 3) ->
        case (try do
                f.(peerName, port, :public_key.ssh_hostkey_fingerprint(digestAlg, public))
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          true ->
            true

          _ ->
            {:error, {:fingerprint_check_failed, digestAlg}}
        end
    end
  end

  defp yes_no(r_ssh(opts: opts), prompt) do
    ioCb =
      :ssh_options.get_value(
        :internal_options,
        :io_cb,
        opts,
        fn ->
          :ssh_io
        end,
        :ssh_transport,
        929
      )

    ioCb.yes_no(prompt, opts)
  end

  defp fmt_hostkey(:"ssh-rsa") do
    'RSA'
  end

  defp fmt_hostkey(:"ssh-dss") do
    'DSA'
  end

  defp fmt_hostkey(:"ssh-ed25519") do
    'ED25519'
  end

  defp fmt_hostkey(:"ssh-ed448") do
    'ED448'
  end

  defp fmt_hostkey(a) when is_atom(a) do
    fmt_hostkey(:erlang.atom_to_list(a))
  end

  defp fmt_hostkey('ecdsa' ++ _) do
    'ECDSA'
  end

  defp fmt_hostkey(x) do
    x
  end

  defp known_host_key(
         r_ssh(
           opts: opts,
           peer: {peerName, {iP, port}}
         ) = ssh,
         public,
         alg
       ) do
    isHostKey =
      try do
        call_KeyCb(:is_host_key, [public, [peerName, iP], port, alg], opts)
      catch
        :error, :undef ->
          call_KeyCb(:is_host_key, [public, peerName, alg], opts)
      end

    case isHostKey do
      true ->
        :ok

      false ->
        doAdd =
          :ssh_options.get_value(:user_options, :save_accepted_host, opts, :ssh_transport, 959)

        case accepted_host(ssh, peerName, port, public, opts) do
          true when doAdd == true ->
            try do
              call_KeyCb(:add_host_key, [[peerName, iP], port, public], opts)
            catch
              :error, :undef ->
                call_KeyCb(:add_host_key, [peerName, public], opts)
            end

          true when doAdd == false ->
            :ok

          false ->
            {:error, :rejected_by_user}

          {:error, e} ->
            {:error, e}
        end

      {:error, error} ->
        {:error, error}
    end
  end

  defp select_algorithm(role, client, server, opts) do
    {encrypt0, decrypt0} = select_encrypt_decrypt(role, client, server)
    {sendMac0, recvMac0} = select_send_recv_mac(role, client, server)

    {encrypt, sendMac} =
      aead_gcm_simultan(
        encrypt0,
        sendMac0
      )

    {decrypt, recvMac} =
      aead_gcm_simultan(
        decrypt0,
        recvMac0
      )

    {compression, decompression} = select_compression_decompression(role, client, server)

    c_Lng =
      select(
        r_ssh_msg_kexinit(client, :languages_client_to_server),
        r_ssh_msg_kexinit(server, :languages_client_to_server)
      )

    s_Lng =
      select(
        r_ssh_msg_kexinit(client, :languages_server_to_client),
        r_ssh_msg_kexinit(server, :languages_server_to_client)
      )

    hKey =
      select_all(
        r_ssh_msg_kexinit(client, :server_host_key_algorithms),
        r_ssh_msg_kexinit(server, :server_host_key_algorithms)
      )

    hK =
      case hKey do
        [] ->
          :undefined

        [hK0 | _] ->
          hK0
      end

    kex =
      select(
        r_ssh_msg_kexinit(client, :kex_algorithms),
        r_ssh_msg_kexinit(server, :kex_algorithms)
      )

    sendExtInfo =
      :ssh_options.get_value(:user_options, :send_ext_info, opts, :ssh_transport, 1015) and
        case role do
          :server ->
            :lists.member(
              'ext-info-c',
              r_ssh_msg_kexinit(client, :kex_algorithms)
            )

          :client ->
            :lists.member(
              'ext-info-s',
              r_ssh_msg_kexinit(server, :kex_algorithms)
            )
        end

    recvExtInfo =
      :ssh_options.get_value(:user_options, :recv_ext_info, opts, :ssh_transport, 1026)

    {:ok,
     r_alg(
       kex: kex,
       hkey: hK,
       encrypt: encrypt,
       decrypt: decrypt,
       send_mac: sendMac,
       recv_mac: recvMac,
       compress: compression,
       decompress: decompression,
       c_lng: c_Lng,
       s_lng: s_Lng,
       send_ext_info: sendExtInfo,
       recv_ext_info: recvExtInfo
     )}
  end

  defp aead_gcm_simultan(:"aes128-gcm@openssh.com", _) do
    {:AEAD_AES_128_GCM, :AEAD_AES_128_GCM}
  end

  defp aead_gcm_simultan(:"aes256-gcm@openssh.com", _) do
    {:AEAD_AES_256_GCM, :AEAD_AES_256_GCM}
  end

  defp aead_gcm_simultan(:AEAD_AES_128_GCM = c, _) do
    {c, c}
  end

  defp aead_gcm_simultan(:AEAD_AES_256_GCM = c, _) do
    {c, c}
  end

  defp aead_gcm_simultan(_, :AEAD_AES_128_GCM = c) do
    {c, c}
  end

  defp aead_gcm_simultan(_, :AEAD_AES_256_GCM = c) do
    {c, c}
  end

  defp aead_gcm_simultan(:"chacha20-poly1305@openssh.com" = c, _) do
    {c, c}
  end

  defp aead_gcm_simultan(cipher, mac) do
    {cipher, mac}
  end

  defp select_encrypt_decrypt(:client, client, server) do
    encrypt =
      select(
        r_ssh_msg_kexinit(client, :encryption_algorithms_client_to_server),
        r_ssh_msg_kexinit(server, :encryption_algorithms_client_to_server)
      )

    decrypt =
      select(
        r_ssh_msg_kexinit(client, :encryption_algorithms_server_to_client),
        r_ssh_msg_kexinit(server, :encryption_algorithms_server_to_client)
      )

    {encrypt, decrypt}
  end

  defp select_encrypt_decrypt(:server, client, server) do
    decrypt =
      select(
        r_ssh_msg_kexinit(client, :encryption_algorithms_client_to_server),
        r_ssh_msg_kexinit(server, :encryption_algorithms_client_to_server)
      )

    encrypt =
      select(
        r_ssh_msg_kexinit(client, :encryption_algorithms_server_to_client),
        r_ssh_msg_kexinit(server, :encryption_algorithms_server_to_client)
      )

    {encrypt, decrypt}
  end

  defp select_send_recv_mac(:client, client, server) do
    sendMac =
      select(
        r_ssh_msg_kexinit(client, :mac_algorithms_client_to_server),
        r_ssh_msg_kexinit(server, :mac_algorithms_client_to_server)
      )

    recvMac =
      select(
        r_ssh_msg_kexinit(client, :mac_algorithms_server_to_client),
        r_ssh_msg_kexinit(server, :mac_algorithms_server_to_client)
      )

    {sendMac, recvMac}
  end

  defp select_send_recv_mac(:server, client, server) do
    recvMac =
      select(
        r_ssh_msg_kexinit(client, :mac_algorithms_client_to_server),
        r_ssh_msg_kexinit(server, :mac_algorithms_client_to_server)
      )

    sendMac =
      select(
        r_ssh_msg_kexinit(client, :mac_algorithms_server_to_client),
        r_ssh_msg_kexinit(server, :mac_algorithms_server_to_client)
      )

    {sendMac, recvMac}
  end

  defp select_compression_decompression(:client, client, server) do
    compression =
      select(
        r_ssh_msg_kexinit(client, :compression_algorithms_client_to_server),
        r_ssh_msg_kexinit(server, :compression_algorithms_client_to_server)
      )

    decompression =
      select(
        r_ssh_msg_kexinit(client, :compression_algorithms_server_to_client),
        r_ssh_msg_kexinit(server, :compression_algorithms_server_to_client)
      )

    {compression, decompression}
  end

  defp select_compression_decompression(:server, client, server) do
    decompression =
      select(
        r_ssh_msg_kexinit(client, :compression_algorithms_client_to_server),
        r_ssh_msg_kexinit(server, :compression_algorithms_client_to_server)
      )

    compression =
      select(
        r_ssh_msg_kexinit(client, :compression_algorithms_server_to_client),
        r_ssh_msg_kexinit(server, :compression_algorithms_server_to_client)
      )

    {compression, decompression}
  end

  defp install_alg(dir, sSH) do
    sSH1 = alg_final(dir, sSH)
    sSH2 = alg_setup(dir, sSH1)
    alg_init(dir, sSH2)
  end

  defp alg_setup(:snd, sSH) do
    aLG = r_ssh(sSH, :algorithms)

    r_ssh(sSH,
      encrypt: r_alg(aLG, :encrypt),
      send_mac: r_alg(aLG, :send_mac),
      send_mac_size: mac_digest_size(r_alg(aLG, :send_mac)),
      compress: r_alg(aLG, :compress),
      c_lng: r_alg(aLG, :c_lng),
      s_lng: r_alg(aLG, :s_lng),
      send_ext_info: r_alg(aLG, :send_ext_info),
      recv_ext_info: r_alg(aLG, :recv_ext_info)
    )
  end

  defp alg_setup(:rcv, sSH) do
    aLG = r_ssh(sSH, :algorithms)

    r_ssh(sSH,
      decrypt: r_alg(aLG, :decrypt),
      recv_mac: r_alg(aLG, :recv_mac),
      recv_mac_size: mac_digest_size(r_alg(aLG, :recv_mac)),
      decompress: r_alg(aLG, :decompress),
      c_lng: r_alg(aLG, :c_lng),
      s_lng: r_alg(aLG, :s_lng),
      send_ext_info: r_alg(aLG, :send_ext_info),
      recv_ext_info: r_alg(aLG, :recv_ext_info)
    )
  end

  defp alg_init(:snd, sSH0) do
    {:ok, sSH1} = send_mac_init(sSH0)
    {:ok, sSH2} = encrypt_init(sSH1)
    {:ok, sSH3} = compress_init(sSH2)
    sSH3
  end

  defp alg_init(:rcv, sSH0) do
    {:ok, sSH1} = recv_mac_init(sSH0)
    {:ok, sSH2} = decrypt_init(sSH1)
    {:ok, sSH3} = decompress_init(sSH2)
    sSH3
  end

  defp alg_final(:snd, sSH0) do
    {:ok, sSH1} = send_mac_final(sSH0)
    {:ok, sSH2} = encrypt_final(sSH1)
    {:ok, sSH3} = compress_final(sSH2)
    sSH3
  end

  defp alg_final(:rcv, sSH0) do
    {:ok, sSH1} = recv_mac_final(sSH0)
    {:ok, sSH2} = decrypt_final(sSH1)
    {:ok, sSH3} = decompress_final(sSH2)
    sSH3
  end

  defp select_all(cL, sL) when length(cL) + length(sL) < 200 do
    cLonly = cL -- sL

    :lists.foldr(
      fn aLG, acc ->
        try do
          [:erlang.list_to_existing_atom(aLG) | acc]
        catch
          _, _ ->
            acc
        end
      end,
      [],
      cL -- cLonly
    )
  end

  defp select_all(cL, sL) do
    error =
      :lists.concat([
        'Received too many algorithms (',
        length(cL),
        '+',
        length(sL),
        ' >= ',
        200,
        ').'
      ])

    :ssh_connection_handler.disconnect(2, error, :ssh_transport, 1198)
  end

  defp select([], []) do
    :none
  end

  defp select(cL, sL) do
    c =
      case select_all(cL, sL) do
        [] ->
          :undefined

        [aLG | _] ->
          aLG
      end

    c
  end

  def ssh_packet(r_ssh_msg_kexinit() = msg, ssh0) do
    binMsg = :ssh_message.encode(msg)
    ssh = key_init(r_ssh(ssh0, :role), ssh0, binMsg)
    pack(binMsg, ssh)
  end

  def ssh_packet(msg, ssh) do
    binMsg = :ssh_message.encode(msg)
    pack(binMsg, ssh)
  end

  def pack(data, ssh = r_ssh()) do
    pack(data, ssh, 0)
  end

  def pack(
        plainText,
        r_ssh(send_sequence: seqNum, send_mac: macAlg, encrypt: cryptoAlg) = ssh0,
        packetLenDeviationForTests
      )
      when is_binary(plainText) do
    {ssh1, compressedPlainText} = compress(ssh0, plainText)

    {finalPacket, ssh2} =
      pack(
        pkt_type(cryptoAlg),
        mac_type(macAlg),
        compressedPlainText,
        packetLenDeviationForTests,
        ssh1
      )

    ssh = r_ssh(ssh2, send_sequence: seqNum + 1 &&& 4_294_967_295)
    {finalPacket, ssh}
  end

  defp pack(
         :common,
         :rfc4253,
         plainText,
         deltaLenTst,
         r_ssh(send_sequence: seqNum, send_mac: macAlg, send_mac_key: macKey) = ssh0
       ) do
    padLen =
      padding_length(
        4 + 1 + :erlang.size(plainText),
        ssh0
      )

    pad = :ssh_bits.random(padLen)
    textLen = 1 + :erlang.size(plainText) + padLen + deltaLenTst

    plainPkt =
      <<textLen::size(32)-unsigned-big-integer, padLen::size(8)-unsigned-big-integer,
        plainText::binary, pad::binary>>

    {ssh1, cipherPkt} = encrypt(ssh0, plainPkt)
    mAC0 = mac(macAlg, macKey, seqNum, plainPkt)
    {<<cipherPkt::binary, mAC0::binary>>, ssh1}
  end

  defp pack(
         :common,
         :enc_then_mac,
         plainText,
         deltaLenTst,
         r_ssh(send_sequence: seqNum, send_mac: macAlg, send_mac_key: macKey) = ssh0
       ) do
    padLen =
      padding_length(
        1 + :erlang.size(plainText),
        ssh0
      )

    pad = :ssh_bits.random(padLen)
    plainLen = 1 + :erlang.size(plainText) + padLen + deltaLenTst
    plainPkt = <<padLen::size(8)-unsigned-big-integer, plainText::binary, pad::binary>>
    {ssh1, cipherPkt} = encrypt(ssh0, plainPkt)
    encPacketPkt = <<plainLen::size(32)-unsigned-big-integer, cipherPkt::binary>>
    mAC0 = mac(macAlg, macKey, seqNum, encPacketPkt)
    {<<plainLen::size(32)-unsigned-big-integer, cipherPkt::binary, mAC0::binary>>, ssh1}
  end

  defp pack(:aead, _, plainText, deltaLenTst, ssh0) do
    padLen =
      padding_length(
        1 + :erlang.size(plainText),
        ssh0
      )

    pad = :ssh_bits.random(padLen)
    plainLen = 1 + :erlang.size(plainText) + padLen + deltaLenTst
    plainPkt = <<padLen::size(8)-unsigned-big-integer, plainText::binary, pad::binary>>

    {ssh1, {cipherPkt, mAC0}} =
      encrypt(
        ssh0,
        <<plainLen::size(32)-unsigned-big-integer, plainPkt::binary>>
      )

    {<<cipherPkt::binary, mAC0::binary>>, ssh1}
  end

  def handle_packet_part(
        <<>>,
        encrypted0,
        aEAD0,
        :undefined,
        r_ssh(decrypt: cryptoAlg, recv_mac: macAlg) = ssh0
      ) do
    case get_length(pkt_type(cryptoAlg), mac_type(macAlg), encrypted0, ssh0) do
      :get_more ->
        {:get_more, <<>>, encrypted0, aEAD0, :undefined, ssh0}

      {:ok, packetLen, _, _, _, _} when packetLen > 256 * 1024 ->
        {:error, {:exceeds_max_size, packetLen}}

      {:ok, packetLen, decrypted, encrypted1, aEAD, r_ssh(recv_mac_size: macSize) = ssh1} ->
        totalNeeded = 4 + packetLen + macSize
        handle_packet_part(decrypted, encrypted1, aEAD, totalNeeded, ssh1)
    end
  end

  def handle_packet_part(decryptedPfx, encryptedBuffer, aEAD, totalNeeded, ssh0)
      when :erlang.size(decryptedPfx) + :erlang.size(encryptedBuffer) < totalNeeded do
    {:get_more, decryptedPfx, encryptedBuffer, aEAD, totalNeeded, ssh0}
  end

  def handle_packet_part(
        decryptedPfx,
        encryptedBuffer,
        aEAD,
        totalNeeded,
        r_ssh(decrypt: cryptoAlg, recv_mac: macAlg) = ssh0
      ) do
    case unpack(
           pkt_type(cryptoAlg),
           mac_type(macAlg),
           decryptedPfx,
           encryptedBuffer,
           aEAD,
           totalNeeded,
           ssh0
         ) do
      {:ok, payload, nextPacketBytes, ssh1} ->
        {ssh, decompressedPayload} = decompress(ssh1, payload)
        {:packet_decrypted, decompressedPayload, nextPacketBytes, ssh}

      other ->
        other
    end
  end

  defp unpack(
         :common,
         :rfc4253,
         decryptedPfx,
         encryptedBuffer,
         _AEAD,
         totalNeeded,
         r_ssh(recv_mac_size: macSize) = ssh0
       ) do
    moreNeeded = totalNeeded - :erlang.size(decryptedPfx) - macSize

    <<encryptedSfx::size(moreNeeded)-binary, mac::size(macSize)-binary, nextPacketBytes::binary>> =
      encryptedBuffer

    {ssh1, decryptedSfx} = decrypt(ssh0, encryptedSfx)
    plainPkt = <<decryptedPfx::binary, decryptedSfx::binary>>

    case is_valid_mac(mac, plainPkt, ssh1) do
      true ->
        {:ok, payload(plainPkt), nextPacketBytes, ssh1}

      false ->
        {:bad_mac, ssh1}
    end
  end

  defp unpack(
         :common,
         :enc_then_mac,
         <<plainLen::size(32)-unsigned-big-integer>>,
         encryptedBuffer,
         _AEAD,
         _TotalNeeded,
         r_ssh(recv_mac_size: macSize) = ssh0
       ) do
    <<payload::size(plainLen)-binary, mAC0::size(macSize)-binary, nextPacketBytes::binary>> =
      encryptedBuffer

    case is_valid_mac(
           mAC0,
           <<plainLen::size(32)-unsigned-big-integer, payload::binary>>,
           ssh0
         ) do
      true ->
        {ssh1, <<paddingLen::size(8)-unsigned-big-integer, plainRest::binary>>} =
          decrypt(ssh0, payload)

        compressedPlainTextLen = :erlang.size(plainRest) - paddingLen
        <<compressedPlainText::size(compressedPlainTextLen)-binary, _Padding::binary>> = plainRest
        {:ok, compressedPlainText, nextPacketBytes, ssh1}

      false ->
        {:bad_mac, ssh0}
    end
  end

  defp unpack(
         :aead,
         _,
         decryptedPfx,
         encryptedBuffer,
         aEAD,
         totalNeeded,
         r_ssh(recv_mac_size: macSize) = ssh0
       ) do
    moreNeeded = totalNeeded - :erlang.size(decryptedPfx) - macSize

    <<encryptedSfx::size(moreNeeded)-binary, mac::size(macSize)-binary, nextPacketBytes::binary>> =
      encryptedBuffer

    case decrypt(ssh0, {aEAD, encryptedSfx, mac}) do
      {ssh1, :error} ->
        {:bad_mac, ssh1}

      {ssh1, decryptedSfx} ->
        decryptedPacket = <<decryptedPfx::binary, decryptedSfx::binary>>
        {:ok, payload(decryptedPacket), nextPacketBytes, ssh1}
    end
  end

  defp get_length(:common, :rfc4253, encryptedBuffer, r_ssh(decrypt_block_size: blockSize) = ssh0) do
    case :erlang.size(encryptedBuffer) >=
           :erlang.max(
             8,
             blockSize
           ) do
      true ->
        <<encBlock::size(blockSize)-binary, encryptedRest::binary>> = encryptedBuffer

        {ssh, <<packetLen::size(32)-unsigned-big-integer, _::binary>> = decrypted} =
          decrypt(ssh0, encBlock)

        {:ok, packetLen, decrypted, encryptedRest, <<>>, ssh}

      false ->
        :get_more
    end
  end

  defp get_length(:common, :enc_then_mac, encryptedBuffer, ssh) do
    case encryptedBuffer do
      <<decrypted::size(4)-binary, encryptedRest::binary>> ->
        <<packetLen::size(32)-unsigned-big-integer>> = decrypted
        {:ok, packetLen, decrypted, encryptedRest, <<>>, ssh}

      _ ->
        :get_more
    end
  end

  defp get_length(:aead, _, encryptedBuffer, ssh) do
    case {:erlang.size(encryptedBuffer) >= 4, r_ssh(ssh, :decrypt)} do
      {true, :"chacha20-poly1305@openssh.com"} ->
        <<encryptedLen::size(4)-binary, encryptedRest::binary>> = encryptedBuffer

        {ssh1, packetLenBin} =
          decrypt(
            ssh,
            {:length, encryptedLen}
          )

        <<packetLen::size(32)-unsigned-big-integer>> = packetLenBin
        {:ok, packetLen, packetLenBin, encryptedRest, encryptedLen, ssh1}

      {true, _} ->
        <<packetLen::size(32)-unsigned-big-integer, encryptedRest::binary>> = encryptedBuffer

        {:ok, packetLen, <<packetLen::size(32)-unsigned-big-integer>>, encryptedRest,
         <<packetLen::size(32)-unsigned-big-integer>>, ssh}

      {false, _} ->
        :get_more
    end
  end

  defp padding_length(
         size,
         r_ssh(
           encrypt_block_size: blockSize,
           random_length_padding: randomLengthPad
         )
       ) do
    pL = rem(blockSize - rem(size, blockSize), blockSize)

    minPadLen =
      cond do
        pL < 4 ->
          pL + blockSize

        true ->
          pL
      end

    padBlockSize = max(blockSize, 4)

    maxExtraBlocks =
      div(
        max(
          randomLengthPad,
          minPadLen
        ) - minPadLen,
        padBlockSize
      )

    extraPadLen =
      try do
        (:rand.uniform(maxExtraBlocks + 1) - 1) * padBlockSize
      catch
        _, _ ->
          0
      end

    minPadLen + extraPadLen
  end

  defp payload(<<packetLen::size(32), paddingLen::size(8), payloadAndPadding::binary>>) do
    payloadLen = packetLen - paddingLen - 1
    <<payload::size(payloadLen)-binary, _::binary>> = payloadAndPadding
    payload
  end

  def sign(sigData, hashAlg, %{:algorithm => :dss} = key) do
    mk_dss_sig(:crypto.sign(:dss, hashAlg, sigData, key))
  end

  def sign(sigData, hashAlg, %{:algorithm => sigAlg} = key) do
    :crypto.sign(sigAlg, hashAlg, sigData, key)
  end

  def sign(sigData, hashAlg, r_DSAPrivateKey() = key) do
    mk_dss_sig(:public_key.sign(sigData, hashAlg, key))
  end

  def sign(sigData, hashAlg, key = r_ECPrivateKey()) do
    derEncodedSign = :public_key.sign(sigData, hashAlg, key)

    r_ECDSA_Sig_Value(r: r, s: s) =
      :public_key.der_decode(
        :"ECDSA-Sig-Value",
        derEncodedSign
      )

    <<:ssh_bits.mpint(r)::binary, :ssh_bits.mpint(s)::binary>>
  end

  def sign(sigData, hashAlg, key) do
    :public_key.sign(sigData, hashAlg, key)
  end

  defp mk_dss_sig(derSignature) do
    r_Dss_Sig_Value(r: r, s: s) = :public_key.der_decode(:"Dss-Sig-Value", derSignature)
    <<r::size(160)-big-unsigned-integer, s::size(160)-big-unsigned-integer>>
  end

  def verify(plainText, hashAlg, sig, {_, r_Dss_Parms()} = key, _) do
    case sig do
      <<r::size(160)-big-unsigned-integer, s::size(160)-big-unsigned-integer>> ->
        signature = :public_key.der_encode(:"Dss-Sig-Value", r_Dss_Sig_Value(r: r, s: s))
        :public_key.verify(plainText, hashAlg, signature, key)

      _ ->
        false
    end
  end

  def verify(plainText, hashAlg, sig, {r_ECPoint(), _} = key, _) do
    case sig do
      <<rlen::size(32)-unsigned-big-integer, r::size(rlen)-big-signed-integer-unit(8),
        slen::size(32)-unsigned-big-integer, s::size(slen)-big-signed-integer-unit(8)>> ->
        sval = r_ECDSA_Sig_Value(r: r, s: s)
        derEncodedSig = :public_key.der_encode(:"ECDSA-Sig-Value", sval)
        :public_key.verify(plainText, hashAlg, derEncodedSig, key)

      _ ->
        false
    end
  end

  def verify(
        plainText,
        hashAlg,
        sig,
        r_RSAPublicKey() = key,
        r_ssh(role: :server, c_version: 'SSH-2.0-OpenSSH_7.' ++ _)
      )
      when hashAlg == :sha256 or hashAlg == :sha512 do
    :public_key.verify(plainText, hashAlg, sig, key) or
      :public_key.verify(plainText, :sha, sig, key)
  end

  def verify(plainText, hashAlg, sig, key, _) do
    :public_key.verify(plainText, hashAlg, sig, key)
  end

  Record.defrecord(:r_cipher, :cipher,
    impl: :undefined,
    key_bytes: :undefined,
    iv_bytes: :undefined,
    block_bytes: :undefined,
    pkt_type: :common
  )

  defp cipher(:AEAD_AES_128_GCM) do
    r_cipher(impl: :aes_128_gcm, key_bytes: 16, iv_bytes: 12, block_bytes: 16, pkt_type: :aead)
  end

  defp cipher(:AEAD_AES_256_GCM) do
    r_cipher(impl: :aes_256_gcm, key_bytes: 32, iv_bytes: 12, block_bytes: 16, pkt_type: :aead)
  end

  defp cipher(:"3des-cbc") do
    r_cipher(impl: :des_ede3_cbc, key_bytes: 24, iv_bytes: 8, block_bytes: 8)
  end

  defp cipher(:"aes128-cbc") do
    r_cipher(impl: :aes_128_cbc, key_bytes: 16, iv_bytes: 16, block_bytes: 16)
  end

  defp cipher(:"aes192-cbc") do
    r_cipher(impl: :aes_192_cbc, key_bytes: 24, iv_bytes: 16, block_bytes: 16)
  end

  defp cipher(:"aes256-cbc") do
    r_cipher(impl: :aes_256_cbc, key_bytes: 32, iv_bytes: 16, block_bytes: 16)
  end

  defp cipher(:"aes128-ctr") do
    r_cipher(impl: :aes_128_ctr, key_bytes: 16, iv_bytes: 16, block_bytes: 16)
  end

  defp cipher(:"aes192-ctr") do
    r_cipher(impl: :aes_192_ctr, key_bytes: 24, iv_bytes: 16, block_bytes: 16)
  end

  defp cipher(:"aes256-ctr") do
    r_cipher(impl: :aes_256_ctr, key_bytes: 32, iv_bytes: 16, block_bytes: 16)
  end

  defp cipher(:"chacha20-poly1305@openssh.com") do
    r_cipher(
      impl: :chacha20_poly1305,
      key_bytes: 32,
      iv_bytes: 12,
      block_bytes: 8,
      pkt_type: :aead
    )
  end

  defp cipher(_) do
    r_cipher()
  end

  defp pkt_type(sshCipher) do
    r_cipher(cipher(sshCipher), :pkt_type)
  end

  defp mac_type(:"hmac-sha2-256-etm@openssh.com") do
    :enc_then_mac
  end

  defp mac_type(:"hmac-sha2-512-etm@openssh.com") do
    :enc_then_mac
  end

  defp mac_type(:"hmac-sha1-etm@openssh.com") do
    :enc_then_mac
  end

  defp mac_type(_) do
    :rfc4253
  end

  defp decrypt_magic(:server) do
    {'A', 'C'}
  end

  defp decrypt_magic(:client) do
    {'B', 'D'}
  end

  defp encrypt_magic(:client) do
    decrypt_magic(:server)
  end

  defp encrypt_magic(:server) do
    decrypt_magic(:client)
  end

  defp encrypt_init(r_ssh(encrypt: :none) = ssh) do
    {:ok, ssh}
  end

  defp encrypt_init(r_ssh(encrypt: :"chacha20-poly1305@openssh.com", role: role) = ssh) do
    {_, keyMagic} = encrypt_magic(role)
    <<k2::size(32)-binary, k1::size(32)-binary>> = hash(ssh, keyMagic, 8 * 64)
    {:ok, r_ssh(ssh, encrypt_keys: {k1, k2})}
  end

  defp encrypt_init(r_ssh(encrypt: sshCipher, role: role) = ssh)
       when sshCipher == :AEAD_AES_128_GCM or
              sshCipher == :AEAD_AES_256_GCM do
    {ivMagic, keyMagic} = encrypt_magic(role)

    r_cipher(impl: cryptoCipher, key_bytes: keyBytes, iv_bytes: ivBytes, block_bytes: blockBytes) =
      cipher(sshCipher)

    iV = hash(ssh, ivMagic, 8 * ivBytes)
    k = hash(ssh, keyMagic, 8 * keyBytes)

    {:ok,
     r_ssh(ssh,
       encrypt_cipher: cryptoCipher,
       encrypt_keys: k,
       encrypt_block_size: blockBytes,
       encrypt_ctx: iV
     )}
  end

  defp encrypt_init(r_ssh(encrypt: sshCipher, role: role) = ssh) do
    {ivMagic, keyMagic} = encrypt_magic(role)

    r_cipher(impl: cryptoCipher, key_bytes: keyBytes, iv_bytes: ivBytes, block_bytes: blockBytes) =
      cipher(sshCipher)

    iV = hash(ssh, ivMagic, 8 * ivBytes)
    k = hash(ssh, keyMagic, 8 * keyBytes)
    ctx0 = :crypto.crypto_init(cryptoCipher, k, iV, true)

    {:ok,
     r_ssh(ssh, encrypt_cipher: cryptoCipher, encrypt_block_size: blockBytes, encrypt_ctx: ctx0)}
  end

  defp encrypt_final(ssh) do
    {:ok,
     r_ssh(ssh,
       encrypt: :none,
       encrypt_keys: :undefined,
       encrypt_block_size: 8,
       encrypt_ctx: :undefined
     )}
  end

  defp encrypt(r_ssh(encrypt: :none) = ssh, data) do
    {ssh, data}
  end

  defp encrypt(
         r_ssh(
           encrypt: :"chacha20-poly1305@openssh.com",
           encrypt_keys: {k1, k2},
           send_sequence: seq
         ) = ssh,
         <<lenData::size(4)-binary, payloadData::binary>>
       ) do
    iV1 = <<0::size(8)-unit(8), seq::size(8)-unit(8)>>
    encLen = :crypto.crypto_one_time(:chacha20, k1, iV1, lenData, true)
    iV2 = <<1::size(8)-little-unit(8), seq::size(8)-unit(8)>>
    encPayloadData = :crypto.crypto_one_time(:chacha20, k2, iV2, payloadData, true)

    polyKey =
      :crypto.crypto_one_time(
        :chacha20,
        k2,
        <<0::size(8)-unit(8), seq::size(8)-unit(8)>>,
        <<0::size(32)-unit(8)>>,
        true
      )

    encBytes = <<encLen::binary, encPayloadData::binary>>
    ctag = :crypto.mac(:poly1305, polyKey, encBytes)
    {ssh, {encBytes, ctag}}
  end

  defp encrypt(
         r_ssh(
           encrypt: sshCipher,
           encrypt_cipher: cryptoCipher,
           encrypt_keys: k,
           encrypt_ctx: iV0
         ) = ssh,
         <<lenData::size(4)-binary, payloadData::binary>>
       )
       when sshCipher == :AEAD_AES_128_GCM or
              sshCipher == :AEAD_AES_256_GCM do
    {ctext, ctag} = :crypto.crypto_one_time_aead(cryptoCipher, k, iV0, payloadData, lenData, true)
    iV = next_gcm_iv(iV0)
    {r_ssh(ssh, encrypt_ctx: iV), {<<lenData::binary, ctext::binary>>, ctag}}
  end

  defp encrypt(r_ssh(encrypt_ctx: ctx0) = ssh, data) do
    enc = :crypto.crypto_update(ctx0, data)
    {ssh, enc}
  end

  defp decrypt_init(r_ssh(decrypt: :none) = ssh) do
    {:ok, ssh}
  end

  defp decrypt_init(r_ssh(decrypt: :"chacha20-poly1305@openssh.com", role: role) = ssh) do
    {_, keyMagic} = decrypt_magic(role)
    <<k2::size(32)-binary, k1::size(32)-binary>> = hash(ssh, keyMagic, 8 * 64)
    {:ok, r_ssh(ssh, decrypt_keys: {k1, k2})}
  end

  defp decrypt_init(r_ssh(decrypt: sshCipher, role: role) = ssh)
       when sshCipher == :AEAD_AES_128_GCM or
              sshCipher == :AEAD_AES_256_GCM do
    {ivMagic, keyMagic} = decrypt_magic(role)

    r_cipher(impl: cryptoCipher, key_bytes: keyBytes, iv_bytes: ivBytes, block_bytes: blockBytes) =
      cipher(sshCipher)

    iV = hash(ssh, ivMagic, 8 * ivBytes)
    k = hash(ssh, keyMagic, 8 * keyBytes)

    {:ok,
     r_ssh(ssh,
       decrypt_cipher: cryptoCipher,
       decrypt_keys: k,
       decrypt_block_size: blockBytes,
       decrypt_ctx: iV
     )}
  end

  defp decrypt_init(r_ssh(decrypt: sshCipher, role: role) = ssh) do
    {ivMagic, keyMagic} = decrypt_magic(role)

    r_cipher(impl: cryptoCipher, key_bytes: keyBytes, iv_bytes: ivBytes, block_bytes: blockBytes) =
      cipher(sshCipher)

    iV = hash(ssh, ivMagic, 8 * ivBytes)
    k = hash(ssh, keyMagic, 8 * keyBytes)
    ctx0 = :crypto.crypto_init(cryptoCipher, k, iV, false)

    {:ok,
     r_ssh(ssh, decrypt_cipher: cryptoCipher, decrypt_block_size: blockBytes, decrypt_ctx: ctx0)}
  end

  defp decrypt_final(ssh) do
    {:ok,
     r_ssh(ssh,
       decrypt: :none,
       decrypt_keys: :undefined,
       decrypt_ctx: :undefined,
       decrypt_block_size: 8
     )}
  end

  defp decrypt(ssh, <<>>) do
    {ssh, <<>>}
  end

  defp decrypt(
         r_ssh(
           decrypt: :"chacha20-poly1305@openssh.com",
           decrypt_keys: {k1, k2},
           recv_sequence: seq
         ) = ssh,
         data
       ) do
    case data do
      {:length, encryptedLen} ->
        packetLenBin =
          :crypto.crypto_one_time(
            :chacha20,
            k1,
            <<0::size(8)-unit(8), seq::size(8)-unit(8)>>,
            encryptedLen,
            false
          )

        {ssh, packetLenBin}

      {aAD, ctext, ctag} ->
        polyKey =
          :crypto.crypto_one_time(
            :chacha20,
            k2,
            <<0::size(8)-unit(8), seq::size(8)-unit(8)>>,
            <<0::size(32)-unit(8)>>,
            false
          )

        case :crypto.equal_const_time(
               ctag,
               :crypto.mac(:poly1305, polyKey, <<aAD::binary, ctext::binary>>)
             ) do
          true ->
            iV2 = <<1::size(8)-little-unit(8), seq::size(8)-unit(8)>>
            plainText = :crypto.crypto_one_time(:chacha20, k2, iV2, ctext, false)
            {ssh, plainText}

          false ->
            {ssh, :error}
        end
    end
  end

  defp decrypt(r_ssh(decrypt: :none) = ssh, data) do
    {ssh, data}
  end

  defp decrypt(
         r_ssh(
           decrypt: sshCipher,
           decrypt_cipher: cryptoCipher,
           decrypt_keys: k,
           decrypt_ctx: iV0
         ) = ssh,
         {aAD, ctext, ctag}
       )
       when sshCipher == :AEAD_AES_128_GCM or
              sshCipher == :AEAD_AES_256_GCM do
    dec = :crypto.crypto_one_time_aead(cryptoCipher, k, iV0, ctext, aAD, ctag, false)
    iV = next_gcm_iv(iV0)
    {r_ssh(ssh, decrypt_ctx: iV), dec}
  end

  defp decrypt(r_ssh(decrypt_ctx: ctx0) = ssh, data) do
    dec = :crypto.crypto_update(ctx0, data)
    {ssh, dec}
  end

  defp next_gcm_iv(<<fixed::size(32), invCtr::size(64)>>) do
    <<fixed::size(32), invCtr + 1::size(64)>>
  end

  defp compress_init(sSH) do
    compress_init(sSH, 1)
  end

  defp compress_init(r_ssh(compress: :none) = ssh, _) do
    {:ok, ssh}
  end

  defp compress_init(r_ssh(compress: :zlib) = ssh, level) do
    zlib = :zlib.open()
    :ok = :zlib.deflateInit(zlib, level)
    {:ok, r_ssh(ssh, compress_ctx: zlib)}
  end

  defp compress_init(r_ssh(compress: :"zlib@openssh.com") = ssh, level) do
    zlib = :zlib.open()
    :ok = :zlib.deflateInit(zlib, level)
    {:ok, r_ssh(ssh, compress_ctx: zlib)}
  end

  defp compress_final(r_ssh(compress: :none) = ssh) do
    {:ok, ssh}
  end

  defp compress_final(
         r_ssh(
           compress: :zlib,
           compress_ctx: context
         ) = ssh
       ) do
    :zlib.close(context)
    {:ok, r_ssh(ssh, compress: :none, compress_ctx: :undefined)}
  end

  defp compress_final(r_ssh(compress: :"zlib@openssh.com", authenticated: false) = ssh) do
    {:ok, ssh}
  end

  defp compress_final(
         r_ssh(compress: :"zlib@openssh.com", compress_ctx: context, authenticated: true) = ssh
       ) do
    :zlib.close(context)
    {:ok, r_ssh(ssh, compress: :none, compress_ctx: :undefined)}
  end

  defp compress(r_ssh(compress: :none) = ssh, data) do
    {ssh, data}
  end

  defp compress(
         r_ssh(compress: :zlib, compress_ctx: context) = ssh,
         data
       ) do
    compressed = :zlib.deflate(context, data, :sync)
    {ssh, :erlang.list_to_binary(compressed)}
  end

  defp compress(
         r_ssh(compress: :"zlib@openssh.com", authenticated: false) = ssh,
         data
       ) do
    {ssh, data}
  end

  defp compress(
         r_ssh(compress: :"zlib@openssh.com", compress_ctx: context, authenticated: true) = ssh,
         data
       ) do
    compressed = :zlib.deflate(context, data, :sync)
    {ssh, :erlang.list_to_binary(compressed)}
  end

  defp decompress_init(r_ssh(decompress: :none) = ssh) do
    {:ok, ssh}
  end

  defp decompress_init(r_ssh(decompress: :zlib) = ssh) do
    zlib = :zlib.open()
    :ok = :zlib.inflateInit(zlib)
    {:ok, r_ssh(ssh, decompress_ctx: zlib)}
  end

  defp decompress_init(r_ssh(decompress: :"zlib@openssh.com") = ssh) do
    zlib = :zlib.open()
    :ok = :zlib.inflateInit(zlib)
    {:ok, r_ssh(ssh, decompress_ctx: zlib)}
  end

  defp decompress_final(r_ssh(decompress: :none) = ssh) do
    {:ok, ssh}
  end

  defp decompress_final(
         r_ssh(
           decompress: :zlib,
           decompress_ctx: context
         ) = ssh
       ) do
    :zlib.close(context)
    {:ok, r_ssh(ssh, decompress: :none, decompress_ctx: :undefined)}
  end

  defp decompress_final(
         r_ssh(
           decompress: :"zlib@openssh.com",
           authenticated: false
         ) = ssh
       ) do
    {:ok, ssh}
  end

  defp decompress_final(
         r_ssh(decompress: :"zlib@openssh.com", decompress_ctx: context, authenticated: true) =
           ssh
       ) do
    :zlib.close(context)
    {:ok, r_ssh(ssh, decompress: :none, decompress_ctx: :undefined)}
  end

  defp decompress(r_ssh(decompress: :none) = ssh, data) do
    {ssh, data}
  end

  defp decompress(
         r_ssh(
           decompress: :zlib,
           decompress_ctx: context
         ) = ssh,
         data
       ) do
    decompressed = :zlib.inflate(context, data)
    {ssh, :erlang.list_to_binary(decompressed)}
  end

  defp decompress(
         r_ssh(decompress: :"zlib@openssh.com", authenticated: false) = ssh,
         data
       ) do
    {ssh, data}
  end

  defp decompress(
         r_ssh(decompress: :"zlib@openssh.com", decompress_ctx: context, authenticated: true) =
           ssh,
         data
       ) do
    decompressed = :zlib.inflate(context, data)
    {ssh, :erlang.list_to_binary(decompressed)}
  end

  defp send_mac_init(sSH) do
    case pkt_type(r_ssh(sSH, :send_mac)) do
      :common ->
        case r_ssh(sSH, :role) do
          :client ->
            keySize = 8 * mac_key_bytes(r_ssh(sSH, :send_mac))
            key = hash(sSH, 'E', keySize)
            {:ok, r_ssh(sSH, send_mac_key: key)}

          :server ->
            keySize = 8 * mac_key_bytes(r_ssh(sSH, :send_mac))
            key = hash(sSH, 'F', keySize)
            {:ok, r_ssh(sSH, send_mac_key: key)}
        end

      _ ->
        {:ok, sSH}
    end
  end

  defp send_mac_final(sSH) do
    {:ok, r_ssh(sSH, send_mac: :none, send_mac_key: :undefined)}
  end

  defp recv_mac_init(sSH) do
    case pkt_type(r_ssh(sSH, :recv_mac)) do
      :common ->
        case r_ssh(sSH, :role) do
          :client ->
            key = hash(sSH, 'F', 8 * mac_key_bytes(r_ssh(sSH, :recv_mac)))
            {:ok, r_ssh(sSH, recv_mac_key: key)}

          :server ->
            key = hash(sSH, 'E', 8 * mac_key_bytes(r_ssh(sSH, :recv_mac)))
            {:ok, r_ssh(sSH, recv_mac_key: key)}
        end

      _ ->
        {:ok, sSH}
    end
  end

  defp recv_mac_final(sSH) do
    {:ok, r_ssh(sSH, recv_mac: :none, recv_mac_key: :undefined)}
  end

  defp mac(:none, _, _, _) do
    <<>>
  end

  defp mac(:"hmac-sha1", key, seqNum, data) do
    :crypto.mac(:hmac, :sha, key, [<<seqNum::size(32)-unsigned-big-integer>>, data])
  end

  defp mac(:"hmac-sha1-96", key, seqNum, data) do
    :crypto.macN(
      :hmac,
      :sha,
      key,
      [<<seqNum::size(32)-unsigned-big-integer>>, data],
      mac_digest_size(:"hmac-sha1-96")
    )
  end

  defp mac(:"hmac-md5", key, seqNum, data) do
    :crypto.mac(:hmac, :md5, key, [<<seqNum::size(32)-unsigned-big-integer>>, data])
  end

  defp mac(:"hmac-md5-96", key, seqNum, data) do
    :crypto.macN(
      :hmac,
      :md5,
      key,
      [<<seqNum::size(32)-unsigned-big-integer>>, data],
      mac_digest_size(:"hmac-md5-96")
    )
  end

  defp mac(:"hmac-sha2-256", key, seqNum, data) do
    :crypto.mac(:hmac, :sha256, key, [<<seqNum::size(32)-unsigned-big-integer>>, data])
  end

  defp mac(:"hmac-sha2-512", key, seqNum, data) do
    :crypto.mac(:hmac, :sha512, key, [<<seqNum::size(32)-unsigned-big-integer>>, data])
  end

  defp mac(:"hmac-sha1-etm@openssh.com", key, seqNum, data) do
    mac(:"hmac-sha1", key, seqNum, data)
  end

  defp mac(:"hmac-sha2-256-etm@openssh.com", key, seqNum, data) do
    mac(:"hmac-sha2-256", key, seqNum, data)
  end

  defp mac(:"hmac-sha2-512-etm@openssh.com", key, seqNum, data) do
    mac(:"hmac-sha2-512", key, seqNum, data)
  end

  defp hash(_SSH, _Char, 0) do
    <<>>
  end

  defp hash(sSH, char, n) do
    hashAlg = sha(r_alg(r_ssh(sSH, :algorithms), :kex))
    k = r_ssh(sSH, :shared_secret)
    h = r_ssh(sSH, :exchanged_hash)

    k1 =
      :crypto.hash(
        hashAlg,
        [k, h, char, r_ssh(sSH, :session_id)]
      )

    sz = div(n, 8)
    <<key::size(sz)-binary, _::binary>> = hash(k, h, k1, n - 128, hashAlg)
    key
  end

  defp hash(_K, _H, ki, n, _HashAlg) when n <= 0 do
    ki
  end

  defp hash(k, h, ki, n, hashAlg) do
    kj = :crypto.hash(hashAlg, [k, h, ki])
    hash(k, h, <<ki::binary, kj::binary>>, n - 128, hashAlg)
  end

  defp kex_hash(sSH, key, hashAlg, args) do
    :crypto.hash(hashAlg, kex_plaintext(sSH, key, args))
  end

  defp kex_plaintext(sSH, key, args) do
    encodedKey = :ssh_message.ssh2_pubkey_encode(key)

    <<:erlang.size(
        cond do
          is_binary(r_ssh(sSH, :c_version)) ->
            r_ssh(sSH, :c_version)

          is_list(r_ssh(sSH, :c_version)) ->
            :erlang.list_to_binary(r_ssh(sSH, :c_version))

          r_ssh(sSH, :c_version) == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(r_ssh(sSH, :c_version)) ->
          r_ssh(sSH, :c_version)

        is_list(r_ssh(sSH, :c_version)) ->
          :erlang.list_to_binary(r_ssh(sSH, :c_version))

        r_ssh(sSH, :c_version) == :undefined ->
          <<>>
      end::binary,
      :erlang.size(
        cond do
          is_binary(r_ssh(sSH, :s_version)) ->
            r_ssh(sSH, :s_version)

          is_list(r_ssh(sSH, :s_version)) ->
            :erlang.list_to_binary(r_ssh(sSH, :s_version))

          r_ssh(sSH, :s_version) == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(r_ssh(sSH, :s_version)) ->
          r_ssh(sSH, :s_version)

        is_list(r_ssh(sSH, :s_version)) ->
          :erlang.list_to_binary(r_ssh(sSH, :s_version))

        r_ssh(sSH, :s_version) == :undefined ->
          <<>>
      end::binary, :erlang.size(r_ssh(sSH, :c_keyinit))::size(32)-unsigned-big-integer,
      r_ssh(sSH, :c_keyinit)::binary,
      :erlang.size(r_ssh(sSH, :s_keyinit))::size(32)-unsigned-big-integer,
      r_ssh(sSH, :s_keyinit)::binary, :erlang.size(encodedKey)::size(32)-unsigned-big-integer,
      encodedKey::binary, kex_alg_dependent(args)::binary>>
  end

  defp kex_alg_dependent({q_c, q_s, k})
       when is_binary(q_c) and
              is_binary(q_s) do
    <<:erlang.size(q_c)::size(32)-unsigned-big-integer, q_c::binary,
      :erlang.size(q_s)::size(32)-unsigned-big-integer, q_s::binary, :ssh_bits.mpint(k)::binary>>
  end

  defp kex_alg_dependent({e, f, k}) do
    <<:ssh_bits.mpint(e)::binary, :ssh_bits.mpint(f)::binary, :ssh_bits.mpint(k)::binary>>
  end

  defp kex_alg_dependent({-1, nBits, -1, prime, gen, e, f, k}) do
    <<nBits::size(32)-unsigned-big-integer, :ssh_bits.mpint(prime)::binary,
      :ssh_bits.mpint(gen)::binary, :ssh_bits.mpint(e)::binary, :ssh_bits.mpint(f)::binary,
      :ssh_bits.mpint(k)::binary>>
  end

  defp kex_alg_dependent({min, nBits, max, prime, gen, e, f, k}) do
    <<min::size(32)-unsigned-big-integer, nBits::size(32)-unsigned-big-integer,
      max::size(32)-unsigned-big-integer, :ssh_bits.mpint(prime)::binary,
      :ssh_bits.mpint(gen)::binary, :ssh_bits.mpint(e)::binary, :ssh_bits.mpint(f)::binary,
      :ssh_bits.mpint(k)::binary>>
  end

  def valid_key_sha_alg(_, %{:engine => _, :key_id => _}, _Alg) do
    true
  end

  def valid_key_sha_alg(:public, r_RSAPublicKey(), :"rsa-sha2-512") do
    true
  end

  def valid_key_sha_alg(:public, r_RSAPublicKey(), :"rsa-sha2-384") do
    true
  end

  def valid_key_sha_alg(:public, r_RSAPublicKey(), :"rsa-sha2-256") do
    true
  end

  def valid_key_sha_alg(:public, r_RSAPublicKey(), :"ssh-rsa") do
    true
  end

  def valid_key_sha_alg(:private, r_RSAPrivateKey(), :"rsa-sha2-512") do
    true
  end

  def valid_key_sha_alg(:private, r_RSAPrivateKey(), :"rsa-sha2-384") do
    true
  end

  def valid_key_sha_alg(:private, r_RSAPrivateKey(), :"rsa-sha2-256") do
    true
  end

  def valid_key_sha_alg(:private, r_RSAPrivateKey(), :"ssh-rsa") do
    true
  end

  def valid_key_sha_alg(:public, {_, r_Dss_Parms()}, :"ssh-dss") do
    true
  end

  def valid_key_sha_alg(:private, r_DSAPrivateKey(), :"ssh-dss") do
    true
  end

  def valid_key_sha_alg(:public, {:ed_pub, :ed25519, _}, :"ssh-ed25519") do
    true
  end

  def valid_key_sha_alg(:private, {:ed_pri, :ed25519, _, _}, :"ssh-ed25519") do
    true
  end

  def valid_key_sha_alg(:public, {:ed_pub, :ed448, _}, :"ssh-ed448") do
    true
  end

  def valid_key_sha_alg(:private, {:ed_pri, :ed448, _, _}, :"ssh-ed448") do
    true
  end

  def valid_key_sha_alg(:public, {r_ECPoint(), {:namedCurve, oID}}, alg)
      when is_tuple(oID) do
    valid_key_sha_alg_ec(oID, alg)
  end

  def valid_key_sha_alg(:private, r_ECPrivateKey(parameters: {:namedCurve, oID}), alg)
      when is_tuple(oID) do
    valid_key_sha_alg_ec(oID, alg)
  end

  def valid_key_sha_alg(_, _, _) do
    false
  end

  defp valid_key_sha_alg_ec(oID, alg) do
    try do
      curve = :public_key.oid2ssh_curvename(oID)
      alg == :erlang.list_to_existing_atom('ecdsa-sha2-' ++ :erlang.binary_to_list(curve))
    catch
      _, _ ->
        false
    end
  end

  def public_algo(r_RSAPublicKey()) do
    :"ssh-rsa"
  end

  def public_algo({_, r_Dss_Parms()}) do
    :"ssh-dss"
  end

  def public_algo({:ed_pub, :ed25519, _}) do
    :"ssh-ed25519"
  end

  def public_algo({:ed_pub, :ed448, _}) do
    :"ssh-ed448"
  end

  def public_algo({r_ECPoint(), {:namedCurve, oID}}) when is_tuple(oID) do
    sshName = :public_key.oid2ssh_curvename(oID)

    try do
      :erlang.list_to_existing_atom('ecdsa-sha2-' ++ :erlang.binary_to_list(sshName))
    catch
      _, _ ->
        :undefined
    end
  end

  def sha(:"ssh-rsa") do
    :sha
  end

  def sha(:"rsa-sha2-256") do
    :sha256
  end

  def sha(:"rsa-sha2-384") do
    :sha384
  end

  def sha(:"rsa-sha2-512") do
    :sha512
  end

  def sha(:"ssh-dss") do
    :sha
  end

  def sha(:"ecdsa-sha2-nistp256") do
    sha(:secp256r1)
  end

  def sha(:"ecdsa-sha2-nistp384") do
    sha(:secp384r1)
  end

  def sha(:"ecdsa-sha2-nistp521") do
    sha(:secp521r1)
  end

  def sha(:"ssh-ed25519") do
    :undefined
  end

  def sha(:"ssh-ed448") do
    :undefined
  end

  def sha(:secp256r1) do
    :sha256
  end

  def sha(:secp384r1) do
    :sha384
  end

  def sha(:secp521r1) do
    :sha512
  end

  def sha(:"diffie-hellman-group1-sha1") do
    :sha
  end

  def sha(:"diffie-hellman-group14-sha1") do
    :sha
  end

  def sha(:"diffie-hellman-group14-sha256") do
    :sha256
  end

  def sha(:"diffie-hellman-group16-sha512") do
    :sha512
  end

  def sha(:"diffie-hellman-group18-sha512") do
    :sha512
  end

  def sha(:"diffie-hellman-group-exchange-sha1") do
    :sha
  end

  def sha(:"diffie-hellman-group-exchange-sha256") do
    :sha256
  end

  def sha({1, 2, 840, 10045, 3, 1, 7}) do
    sha(:secp256r1)
  end

  def sha({1, 3, 132, 0, 34}) do
    sha(:secp384r1)
  end

  def sha({1, 3, 132, 0, 35}) do
    sha(:secp521r1)
  end

  def sha(:"ecdh-sha2-nistp256") do
    sha(:secp256r1)
  end

  def sha(:"ecdh-sha2-nistp384") do
    sha(:secp384r1)
  end

  def sha(:"ecdh-sha2-nistp521") do
    sha(:secp521r1)
  end

  def sha(:"curve25519-sha256") do
    :sha256
  end

  def sha(:"curve25519-sha256@libssh.org") do
    :sha256
  end

  def sha(:"curve448-sha512") do
    :sha512
  end

  def sha(:x25519) do
    :sha256
  end

  def sha(:x448) do
    :sha512
  end

  def sha(str) when is_list(str) and length(str) < 50 do
    sha(:erlang.list_to_existing_atom(str))
  end

  defp mac_key_bytes(:"hmac-sha1") do
    20
  end

  defp mac_key_bytes(:"hmac-sha1-etm@openssh.com") do
    20
  end

  defp mac_key_bytes(:"hmac-sha1-96") do
    20
  end

  defp mac_key_bytes(:"hmac-md5") do
    16
  end

  defp mac_key_bytes(:"hmac-md5-96") do
    16
  end

  defp mac_key_bytes(:"hmac-sha2-256") do
    32
  end

  defp mac_key_bytes(:"hmac-sha2-256-etm@openssh.com") do
    32
  end

  defp mac_key_bytes(:"hmac-sha2-512") do
    64
  end

  defp mac_key_bytes(:"hmac-sha2-512-etm@openssh.com") do
    64
  end

  defp mac_key_bytes(:AEAD_AES_128_GCM) do
    0
  end

  defp mac_key_bytes(:AEAD_AES_256_GCM) do
    0
  end

  defp mac_key_bytes(:"chacha20-poly1305@openssh.com") do
    0
  end

  defp mac_key_bytes(:none) do
    0
  end

  defp mac_digest_size(:"hmac-sha1") do
    20
  end

  defp mac_digest_size(:"hmac-sha1-etm@openssh.com") do
    20
  end

  defp mac_digest_size(:"hmac-sha1-96") do
    12
  end

  defp mac_digest_size(:"hmac-md5") do
    20
  end

  defp mac_digest_size(:"hmac-md5-96") do
    12
  end

  defp mac_digest_size(:"hmac-sha2-256") do
    32
  end

  defp mac_digest_size(:"hmac-sha2-256-etm@openssh.com") do
    32
  end

  defp mac_digest_size(:"hmac-sha2-512") do
    64
  end

  defp mac_digest_size(:"hmac-sha2-512-etm@openssh.com") do
    64
  end

  defp mac_digest_size(:AEAD_AES_128_GCM) do
    16
  end

  defp mac_digest_size(:AEAD_AES_256_GCM) do
    16
  end

  defp mac_digest_size(:"chacha20-poly1305@openssh.com") do
    16
  end

  defp mac_digest_size(:none) do
    0
  end

  defp dh_group(:"diffie-hellman-group1-sha1") do
    {2,
     179_769_313_486_231_590_770_839_156_793_787_453_197_860_296_048_756_011_706_444_423_684_197_180_216_158_519_368_947_833_795_864_925_541_502_180_565_485_980_503_646_440_548_199_239_100_050_792_877_003_355_816_639_229_553_136_239_076_508_735_759_914_822_574_862_575_007_425_302_077_447_712_589_550_957_937_778_424_442_426_617_334_727_629_299_387_668_709_205_606_050_270_810_842_907_692_932_019_128_194_467_627_007}
  end

  defp dh_group(:"diffie-hellman-group14-sha1") do
    {2,
     32_317_006_071_311_007_300_338_913_926_423_828_248_817_941_241_140_239_112_842_009_751_400_741_706_634_354_222_619_689_417_363_569_347_117_901_737_909_704_191_754_605_873_209_195_028_853_758_986_185_622_153_212_175_412_514_901_774_520_270_235_796_078_236_248_884_246_189_477_587_641_105_928_646_099_411_723_245_426_622_522_193_230_540_919_037_680_524_235_519_125_679_715_870_117_001_058_055_877_651_038_861_847_280_257_976_054_903_569_732_561_526_167_081_339_361_799_541_336_476_559_160_368_317_896_729_073_178_384_589_680_639_671_900_977_202_194_168_647_225_871_031_411_336_429_319_536_193_471_636_533_209_717_077_448_227_988_588_565_369_208_645_296_636_077_250_268_955_505_928_362_751_121_174_096_972_998_068_410_554_359_584_866_583_291_642_136_218_231_078_990_999_448_652_468_262_416_972_035_911_852_507_045_361_090_559}
  end

  defp dh_group(:"diffie-hellman-group14-sha256") do
    {2,
     32_317_006_071_311_007_300_338_913_926_423_828_248_817_941_241_140_239_112_842_009_751_400_741_706_634_354_222_619_689_417_363_569_347_117_901_737_909_704_191_754_605_873_209_195_028_853_758_986_185_622_153_212_175_412_514_901_774_520_270_235_796_078_236_248_884_246_189_477_587_641_105_928_646_099_411_723_245_426_622_522_193_230_540_919_037_680_524_235_519_125_679_715_870_117_001_058_055_877_651_038_861_847_280_257_976_054_903_569_732_561_526_167_081_339_361_799_541_336_476_559_160_368_317_896_729_073_178_384_589_680_639_671_900_977_202_194_168_647_225_871_031_411_336_429_319_536_193_471_636_533_209_717_077_448_227_988_588_565_369_208_645_296_636_077_250_268_955_505_928_362_751_121_174_096_972_998_068_410_554_359_584_866_583_291_642_136_218_231_078_990_999_448_652_468_262_416_972_035_911_852_507_045_361_090_559}
  end

  defp dh_group(:"diffie-hellman-group16-sha512") do
    {2,
     1_044_388_881_413_152_506_679_602_719_846_529_545_831_269_060_992_135_009_022_588_756_444_338_172_022_322_690_710_444_046_669_809_783_930_111_585_737_890_362_691_860_127_079_270_495_454_517_218_673_016_928_427_459_146_001_866_885_779_762_982_229_321_192_368_303_346_235_204_368_051_010_309_155_674_155_697_460_347_176_946_394_076_535_157_284_994_895_284_821_633_700_921_811_716_738_972_451_834_979_455_897_010_306_333_468_590_751_358_365_138_782_250_372_269_117_968_985_194_322_444_535_687_415_522_007_151_638_638_141_456_178_420_621_277_822_674_995_027_990_278_673_458_629_544_391_736_919_766_299_005_511_505_446_177_668_154_446_234_882_665_961_680_796_576_903_199_116_089_347_634_947_187_778_906_528_008_004_756_692_571_666_922_964_122_566_174_582_776_707_332_452_371_001_272_163_776_841_229_318_324_903_125_740_713_574_141_005_124_561_965_913_888_899_753_461_735_347_970_011_693_256_316_751_660_678_950_830_027_510_255_804_846_105_583_465_055_446_615_090_444_309_583_050_775_808_509_297_040_039_680_057_435_342_253_926_566_240_898_195_863_631_588_888_936_364_129_920_059_308_455_669_454_034_010_391_478_238_784_189_888_594_672_336_242_763_795_138_176_353_222_845_524_644_040_094_258_962_433_613_354_036_104_643_881_925_238_489_224_010_194_193_088_911_666_165_584_229_424_668_165_441_688_927_790_460_608_264_864_204_237_717_002_054_744_337_988_941_974_661_214_699_689_706_521_543_006_262_604_535_890_998_125_752_275_942_608_772_174_376_107_314_217_749_233_048_217_904_944_409_836_238_235_772_306_749_874_396_760_463_376_480_215_133_461_333_478_395_682_746_608_242_585_133_953_883_882_226_786_118_030_184_028_136_755_970_045_385_534_758_453_247}
  end

  defp dh_group(:"diffie-hellman-group18-sha512") do
    {2,
     1_090_748_135_619_415_929_450_294_929_359_784_500_348_155_124_953_172_211_774_101_106_966_150_168_922_785_639_028_532_473_848_836_817_769_712_164_169_076_432_969_224_698_752_674_677_662_739_994_265_785_437_233_596_157_045_970_922_338_040_698_100_507_861_033_047_312_331_823_982_435_279_475_700_199_860_971_612_732_540_528_796_554_502_867_919_746_776_983_759_391_475_987_142_521_315_878_719_577_519_148_811_830_879_919_426_939_958_487_087_540_965_716_419_167_467_499_326_156_226_529_675_209_172_277_001_377_591_248_147_563_782_880_558_861_083_327_174_154_014_975_134_893_125_116_015_776_318_890_295_960_698_011_614_157_721_282_527_539_468_816_519_319_333_337_503_114_777_192_360_412_281_721_018_955_834_377_615_480_468_479_252_748_867_320_362_385_355_596_601_795_122_806_756_217_713_579_819_870_634_321_561_907_813_255_153_703_950_795_271_232_652_404_894_983_869_492_174_481_652_303_803_498_881_366_210_508_647_263_668_376_514_131_031_102_336_837_488_999_775_744_046_733_651_827_239_395_353_540_348_414_872_854_639_719_294_694_323_450_186_884_189_822_544_540_647_226_987_292_160_693_184_734_654_941_906_936_646_576_130_260_972_193_280_317_171_696_418_971_553_954_161_446_191_759_093_719_524_951_116_705_577_362_073_481_319_296_041_201_283_516_154_269_044_389_257_727_700_289_684_119_460_283_480_452_306_204_130_024_913_879_981_135_908_026_983_868_205_969_318_167_819_680_850_998_649_694_416_907_952_712_904_962_404_937_775_789_698_917_207_356_355_227_455_066_183_815_847_669_135_530_549_755_439_819_480_321_732_925_869_069_136_146_085_326_382_334_628_745_456_398_071_603_058_051_634_209_386_708_703_306_545_903_199_608_523_824_513_729_625_136_659_128_221_100_967_735_450_519_952_404_248_198_262_813_831_097_374_261_650_380_017_277_916_975_324_134_846_574_681_307_337_017_380_830_353_680_623_216_336_949_471_306_191_686_438_249_305_686_413_380_231_046_096_450_953_594_089_375_540_285_037_292_470_929_395_114_028_305_547_452_584_962_074_309_438_151_825_437_902_976_012_891_749_355_198_678_420_603_722_034_900_311_364_893_046_495_761_404_333_938_686_140_037_848_030_916_292_543_273_684_533_640_032_637_639_100_774_502_371_542_479_302_473_698_388_692_892_420_946_478_947_733_800_387_782_741_417_786_484_770_190_108_867_879_778_991_633_218_628_640_533_982_619_322_466_154_883_011_452_291_890_252_336_487_236_086_654_396_093_853_898_628_805_813_177_559_162_076_363_154_436_494_477_507_871_294_119_841_637_867_701_722_166_609_831_201_845_484_078_070_518_041_336_869_808_398_454_625_586_921_201_308_185_638_888_082_699_408_686_536_045_192_649_569_198_110_353_659_943_111_802_300_636_106_509_865_023_943_661_829_436_426_563_007_917_282_050_894_429_388_841_748_885_398_290_707_743_052_973_605_359_277_515_749_619_730_823_773_215_894_755_121_761_467_887_865_327_707_115_573_804_264_519_206_349_215_850_195_195_364_813_387_526_811_742_474_131_549_802_130_246_506_341_207_020_335_797_706_780_705_406_945_275_438_806_265_978_516_209_706_795_702_579_244_075_380_490_231_741_030_862_614_968_783_306_207_869_687_868_108_423_639_971_983_209_077_624_758_080_499_988_275_591_392_787_267_627_182_442_892_809_646_874_228_263_172_435_642_368_588_260_139_161_962_836_121_481_966_092_745_325_488_641_054_238_839_295_138_992_979_335_446_110_090_325_230_955_276_870_524_611_359_124_918_392_740_353_154_294_858_383_359}
  end

  def parallell_gen_key(
        ssh =
          r_ssh(
            keyex_key: {:x, {g, p}},
            algorithms: algs
          )
      ) do
    sz = dh_bits(algs)
    {public, private} = generate_key(:dh, [p, g, 2 * sz])
    r_ssh(ssh, keyex_key: {{private, public}, {g, p}})
  end

  defp generate_key(:ecdh = algorithm, args) do
    :crypto.generate_key(algorithm, args)
  end

  defp generate_key(algorithm, args) do
    {public, private} =
      :crypto.generate_key(
        algorithm,
        args
      )

    {:crypto.bytes_to_integer(public), :crypto.bytes_to_integer(private)}
  end

  defp compute_key(algorithm, othersPublic, myPrivate, args) do
    shared = :crypto.compute_key(algorithm, othersPublic, myPrivate, args)
    :crypto.bytes_to_integer(shared)
  end

  defp dh_bits(r_alg(encrypt: encrypt, send_mac: sendMac)) do
    c = cipher(encrypt)

    8 *
      :lists.max([
        r_cipher(c, :key_bytes),
        r_cipher(c, :block_bytes),
        r_cipher(c, :iv_bytes),
        mac_key_bytes(sendMac)
      ])
  end

  defp ecdh_curve(:"ecdh-sha2-nistp256") do
    :secp256r1
  end

  defp ecdh_curve(:"ecdh-sha2-nistp384") do
    :secp384r1
  end

  defp ecdh_curve(:"ecdh-sha2-nistp521") do
    :secp521r1
  end

  defp ecdh_curve(:"curve448-sha512") do
    :x448
  end

  defp ecdh_curve(:"curve25519-sha256") do
    :x25519
  end

  defp ecdh_curve(:"curve25519-sha256@libssh.org") do
    :x25519
  end

  defp supported_algorithms(
         key,
         [{:client2server, bL1}, {:server2client, bL2}]
       ) do
    [{:client2server, as1}, {:server2client, as2}] = supported_algorithms(key)
    [{:client2server, as1 -- bL1}, {:server2client, as2 -- bL2}]
  end

  defp supported_algorithms(key, blackList) do
    supported_algorithms(key) -- blackList
  end

  defp select_crypto_supported(l) do
    sup = :crypto.supports()

    for {name, cryptoRequires} <- l,
        crypto_supported(cryptoRequires, sup) do
      name
    end
  end

  defp crypto_supported(conditions, supported) do
    :lists.all(
      fn {tag, cryptoName} when is_atom(cryptoName) ->
        crypto_name_supported(tag, cryptoName, supported)
      end,
      conditions
    )
  end

  defp crypto_name_supported(tag, cryptoName, supported) do
    vs = :proplists.get_value(tag, supported, [])
    :lists.member(cryptoName, vs)
  end

  defp same(algs) do
    [{:client2server, algs}, {:server2client, algs}]
  end

  defp trim_tail(str) do
    :lists.takewhile(
      fn c ->
        c !== ?\r and c !== ?\n
      end,
      str
    )
  end

  def ssh_dbg_trace_points() do
    [:alg, :ssh_messages, :raw_messages, :hello]
  end

  def ssh_dbg_flags(:alg) do
    [:c]
  end

  def ssh_dbg_flags(:hello) do
    [:c]
  end

  def ssh_dbg_flags(:raw_messages) do
    ssh_dbg_flags(:hello)
  end

  def ssh_dbg_flags(:ssh_messages) do
    ssh_dbg_flags(:hello)
  end

  def ssh_dbg_on(:alg) do
    :dbg.tpl(:ssh_transport, :select_algorithm, 4, :x)
  end

  def ssh_dbg_on(:hello) do
    :dbg.tp(:ssh_transport, :hello_version_msg, 1, :x)
    :dbg.tp(:ssh_transport, :handle_hello_version, 1, :x)
  end

  def ssh_dbg_on(:raw_messages) do
    ssh_dbg_on(:hello)
  end

  def ssh_dbg_on(:ssh_messages) do
    ssh_dbg_on(:hello)
  end

  def ssh_dbg_off(:alg) do
    :dbg.ctpl(:ssh_transport, :select_algorithm, 4)
  end

  def ssh_dbg_off(:hello) do
    :dbg.ctpg(:ssh_transport, :hello_version_msg, 1)
    :dbg.ctpg(:ssh_transport, :handle_hello_version, 1)
  end

  def ssh_dbg_off(:raw_messages) do
    ssh_dbg_off(:hello)
  end

  def ssh_dbg_off(:ssh_messages) do
    ssh_dbg_off(:hello)
  end

  def ssh_dbg_format(
        :hello,
        {:call, {:ssh_transport, :hello_version_msg, [_]}}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :hello,
        {:return_from, {:ssh_transport, :hello_version_msg, 1}, hello}
      ) do
    ['Going to send hello message:\n', hello]
  end

  def ssh_dbg_format(
        :hello,
        {:call, {:ssh_transport, :handle_hello_version, [hello]}}
      ) do
    ['Received hello message:\n', hello]
  end

  def ssh_dbg_format(
        :hello,
        {:return_from, {:ssh_transport, :handle_hello_version, 1}, _Ret}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :alg,
        {:call, {:ssh_transport, :select_algorithm, [_, _, _, _]}}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :alg,
        {:return_from, {:ssh_transport, :select_algorithm, 4}, {:ok, alg}}
      ) do
    ['Negotiated algorithms:\n', wr_record(alg)]
  end

  def ssh_dbg_format(:raw_messages, x) do
    ssh_dbg_format(:hello, x)
  end

  def ssh_dbg_format(:ssh_messages, x) do
    ssh_dbg_format(:hello, x)
  end

  defp wr_record(r = r_alg()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_alg(r_alg())), [])
  end
end
