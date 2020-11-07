defmodule :m_ssh_message do
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

  Record.defrecord(:r_ssh_msg_global_request, :ssh_msg_global_request,
    name: :undefined,
    want_reply: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_request_success, :ssh_msg_request_success, data: :undefined)
  Record.defrecord(:r_ssh_msg_request_failure, :ssh_msg_request_failure, [])

  Record.defrecord(:r_ssh_msg_channel_open, :ssh_msg_channel_open,
    channel_type: :undefined,
    sender_channel: :undefined,
    initial_window_size: :undefined,
    maximum_packet_size: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_open_confirmation, :ssh_msg_channel_open_confirmation,
    recipient_channel: :undefined,
    sender_channel: :undefined,
    initial_window_size: :undefined,
    maximum_packet_size: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_open_failure, :ssh_msg_channel_open_failure,
    recipient_channel: :undefined,
    reason: :undefined,
    description: :undefined,
    lang: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_window_adjust, :ssh_msg_channel_window_adjust,
    recipient_channel: :undefined,
    bytes_to_add: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_data, :ssh_msg_channel_data,
    recipient_channel: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_extended_data, :ssh_msg_channel_extended_data,
    recipient_channel: :undefined,
    data_type_code: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_eof, :ssh_msg_channel_eof, recipient_channel: :undefined)

  Record.defrecord(:r_ssh_msg_channel_close, :ssh_msg_channel_close, recipient_channel: :undefined)

  Record.defrecord(:r_ssh_msg_channel_request, :ssh_msg_channel_request,
    recipient_channel: :undefined,
    request_type: :undefined,
    want_reply: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_success, :ssh_msg_channel_success,
    recipient_channel: :undefined
  )

  Record.defrecord(:r_ssh_msg_channel_failure, :ssh_msg_channel_failure,
    recipient_channel: :undefined
  )

  Record.defrecord(:r_channel, :channel,
    type: :undefined,
    sys: :undefined,
    user: :undefined,
    flow_control: :undefined,
    local_id: :undefined,
    recv_window_size: :undefined,
    recv_window_pending: 0,
    recv_packet_size: :undefined,
    recv_close: false,
    remote_id: :undefined,
    send_window_size: :undefined,
    send_packet_size: :undefined,
    sent_close: false,
    send_buf: []
  )

  Record.defrecord(:r_connection, :connection,
    requests: [],
    channel_cache: :undefined,
    channel_id_seed: :undefined,
    cli_spec: :undefined,
    options: :undefined,
    exec: :undefined,
    system_supervisor: :undefined,
    sub_system_supervisor: :undefined,
    connection_supervisor: :undefined
  )

  Record.defrecord(:r_ssh_msg_userauth_request, :ssh_msg_userauth_request,
    user: :undefined,
    service: :undefined,
    method: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_userauth_failure, :ssh_msg_userauth_failure,
    authentications: :undefined,
    partial_success: :undefined
  )

  Record.defrecord(:r_ssh_msg_userauth_success, :ssh_msg_userauth_success, [])

  Record.defrecord(:r_ssh_msg_userauth_banner, :ssh_msg_userauth_banner,
    message: :undefined,
    language: :undefined
  )

  Record.defrecord(:r_ssh_msg_userauth_passwd_changereq, :ssh_msg_userauth_passwd_changereq,
    prompt: :undefined,
    languge: :undefined
  )

  Record.defrecord(:r_ssh_msg_userauth_pk_ok, :ssh_msg_userauth_pk_ok,
    algorithm_name: :undefined,
    key_blob: :undefined
  )

  Record.defrecord(:r_ssh_msg_userauth_info_request, :ssh_msg_userauth_info_request,
    name: :undefined,
    instruction: :undefined,
    language_tag: :undefined,
    num_prompts: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_ssh_msg_userauth_info_response, :ssh_msg_userauth_info_response,
    num_responses: :undefined,
    data: :undefined
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

  @behaviour :ssh_dbg
  defp ucl(b) do
    try do
      :unicode.characters_to_list(b)
    catch
      _, _ ->
        throw({:error, :bad_unicode})
    else
      l when is_list(l) ->
        l

      {:error, _Matched, rest} ->
        throw({:error, {:bad_unicode, rest}})
    end
  end

  def encode(r_ssh_msg_global_request(name: name, want_reply: bool, data: data)) do
    <<80::size(8)-unsigned-big-integer,
      :erlang.size(
        cond do
          is_binary(name) ->
            name

          is_list(name) ->
            :erlang.list_to_binary(name)

          name == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(name) ->
          name

        is_list(name) ->
          :erlang.list_to_binary(name)

        name == :undefined ->
          <<>>
      end::binary,
      case bool do
        true ->
          1

        false ->
          0
      end::size(8)-unsigned-big-integer,
      cond do
        is_binary(data) ->
          data

        is_list(data) ->
          :erlang.list_to_binary(data)

        data == :undefined ->
          <<>>
      end::binary>>
  end

  def encode(r_ssh_msg_request_success(data: data)) do
    <<81::size(8)-unsigned-big-integer, data::binary>>
  end

  def encode(r_ssh_msg_request_failure()) do
    <<82::size(8)-unsigned-big-integer>>
  end

  def encode(
        r_ssh_msg_channel_open(
          channel_type: type,
          sender_channel: sender,
          initial_window_size: window,
          maximum_packet_size: max,
          data: data
        )
      ) do
    <<90::size(8)-unsigned-big-integer,
      :erlang.size(
        cond do
          is_binary(type) ->
            type

          is_list(type) ->
            :erlang.list_to_binary(type)

          type == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(type) ->
          type

        is_list(type) ->
          :erlang.list_to_binary(type)

        type == :undefined ->
          <<>>
      end::binary, sender::size(32)-unsigned-big-integer, window::size(32)-unsigned-big-integer,
      max::size(32)-unsigned-big-integer,
      cond do
        is_binary(data) ->
          data

        is_list(data) ->
          :erlang.list_to_binary(data)

        data == :undefined ->
          <<>>
      end::binary>>
  end

  def encode(
        r_ssh_msg_channel_open_confirmation(
          recipient_channel: recipient,
          sender_channel: sender,
          initial_window_size: initWindowSize,
          maximum_packet_size: maxPacketSize,
          data: data
        )
      ) do
    <<91::size(8)-unsigned-big-integer, recipient::size(32)-unsigned-big-integer,
      sender::size(32)-unsigned-big-integer, initWindowSize::size(32)-unsigned-big-integer,
      maxPacketSize::size(32)-unsigned-big-integer,
      cond do
        is_binary(data) ->
          data

        is_list(data) ->
          :erlang.list_to_binary(data)

        data == :undefined ->
          <<>>
      end::binary>>
  end

  def encode(
        r_ssh_msg_channel_open_failure(
          recipient_channel: recipient,
          reason: reason,
          description: desc,
          lang: lang
        )
      ) do
    <<92::size(8)-unsigned-big-integer, recipient::size(32)-unsigned-big-integer,
      reason::size(32)-unsigned-big-integer,
      :erlang.size(
        cond do
          is_binary(desc) ->
            desc

          is_list(desc) ->
            :erlang.list_to_binary(desc)

          desc == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(desc) ->
          desc

        is_list(desc) ->
          :erlang.list_to_binary(desc)

        desc == :undefined ->
          <<>>
      end::binary,
      :erlang.size(
        cond do
          is_binary(lang) ->
            lang

          is_list(lang) ->
            :erlang.list_to_binary(lang)

          lang == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(lang) ->
          lang

        is_list(lang) ->
          :erlang.list_to_binary(lang)

        lang == :undefined ->
          <<>>
      end::binary>>
  end

  def encode(
        r_ssh_msg_channel_window_adjust(
          recipient_channel: recipient,
          bytes_to_add: bytes
        )
      ) do
    <<93::size(8)-unsigned-big-integer, recipient::size(32)-unsigned-big-integer,
      bytes::size(32)-unsigned-big-integer>>
  end

  def encode(r_ssh_msg_channel_data(recipient_channel: recipient, data: data)) do
    <<94::size(8)-unsigned-big-integer, recipient::size(32)-unsigned-big-integer,
      :erlang.size(data)::size(32)-unsigned-big-integer, data::binary>>
  end

  def encode(
        r_ssh_msg_channel_extended_data(
          recipient_channel: recipient,
          data_type_code: dataType,
          data: data
        )
      ) do
    <<95::size(8)-unsigned-big-integer, recipient::size(32)-unsigned-big-integer,
      dataType::size(32)-unsigned-big-integer, :erlang.size(data)::size(32)-unsigned-big-integer,
      data::binary>>
  end

  def encode(r_ssh_msg_channel_eof(recipient_channel: recipient)) do
    <<96::size(8)-unsigned-big-integer, recipient::size(32)-unsigned-big-integer>>
  end

  def encode(r_ssh_msg_channel_close(recipient_channel: recipient)) do
    <<97::size(8)-unsigned-big-integer, recipient::size(32)-unsigned-big-integer>>
  end

  def encode(
        r_ssh_msg_channel_request(
          recipient_channel: recipient,
          request_type: type,
          want_reply: bool,
          data: data
        )
      ) do
    <<98::size(8)-unsigned-big-integer, recipient::size(32)-unsigned-big-integer,
      :erlang.size(
        cond do
          is_binary(type) ->
            type

          is_list(type) ->
            :erlang.list_to_binary(type)

          type == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(type) ->
          type

        is_list(type) ->
          :erlang.list_to_binary(type)

        type == :undefined ->
          <<>>
      end::binary,
      case bool do
        true ->
          1

        false ->
          0
      end::size(8)-unsigned-big-integer,
      cond do
        is_binary(data) ->
          data

        is_list(data) ->
          :erlang.list_to_binary(data)

        data == :undefined ->
          <<>>
      end::binary>>
  end

  def encode(r_ssh_msg_channel_success(recipient_channel: recipient)) do
    <<99::size(8)-unsigned-big-integer, recipient::size(32)-unsigned-big-integer>>
  end

  def encode(r_ssh_msg_channel_failure(recipient_channel: recipient)) do
    <<100::size(8)-unsigned-big-integer, recipient::size(32)-unsigned-big-integer>>
  end

  def encode(r_ssh_msg_userauth_request(user: user, service: service, method: method, data: data)) do
    <<50::size(8)-unsigned-big-integer,
      <<:erlang.size(:unicode.characters_to_binary(user))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(user)::binary>>::binary,
      :erlang.size(
        cond do
          is_binary(service) ->
            service

          is_list(service) ->
            :erlang.list_to_binary(service)

          service == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(service) ->
          service

        is_list(service) ->
          :erlang.list_to_binary(service)

        service == :undefined ->
          <<>>
      end::binary,
      :erlang.size(
        cond do
          is_binary(method) ->
            method

          is_list(method) ->
            :erlang.list_to_binary(method)

          method == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(method) ->
          method

        is_list(method) ->
          :erlang.list_to_binary(method)

        method == :undefined ->
          <<>>
      end::binary,
      cond do
        is_binary(data) ->
          data

        is_list(data) ->
          :erlang.list_to_binary(data)

        data == :undefined ->
          <<>>
      end::binary>>
  end

  def encode(
        r_ssh_msg_userauth_failure(
          authentications: auths,
          partial_success: bool
        )
      ) do
    <<51::size(8)-unsigned-big-integer,
      :erlang.size(
        cond do
          is_binary(auths) ->
            auths

          is_list(auths) ->
            :erlang.list_to_binary(auths)

          auths == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(auths) ->
          auths

        is_list(auths) ->
          :erlang.list_to_binary(auths)

        auths == :undefined ->
          <<>>
      end::binary,
      case bool do
        true ->
          1

        false ->
          0
      end::size(8)-unsigned-big-integer>>
  end

  def encode(r_ssh_msg_userauth_success()) do
    <<52::size(8)-unsigned-big-integer>>
  end

  def encode(r_ssh_msg_userauth_banner(message: banner, language: lang)) do
    <<53::size(8)-unsigned-big-integer,
      <<:erlang.size(:unicode.characters_to_binary(banner))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(banner)::binary>>::binary,
      :erlang.size(
        cond do
          is_binary(lang) ->
            lang

          is_list(lang) ->
            :erlang.list_to_binary(lang)

          lang == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(lang) ->
          lang

        is_list(lang) ->
          :erlang.list_to_binary(lang)

        lang == :undefined ->
          <<>>
      end::binary>>
  end

  def encode(r_ssh_msg_userauth_pk_ok(algorithm_name: alg, key_blob: keyBlob)) do
    <<60::size(8)-unsigned-big-integer,
      :erlang.size(
        cond do
          is_binary(alg) ->
            alg

          is_list(alg) ->
            :erlang.list_to_binary(alg)

          alg == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(alg) ->
          alg

        is_list(alg) ->
          :erlang.list_to_binary(alg)

        alg == :undefined ->
          <<>>
      end::binary, :erlang.size(keyBlob)::size(32)-unsigned-big-integer, keyBlob::binary>>
  end

  def encode(r_ssh_msg_userauth_passwd_changereq(prompt: prompt, languge: lang)) do
    <<60::size(8)-unsigned-big-integer,
      <<:erlang.size(:unicode.characters_to_binary(prompt))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(prompt)::binary>>::binary,
      :erlang.size(
        cond do
          is_binary(lang) ->
            lang

          is_list(lang) ->
            :erlang.list_to_binary(lang)

          lang == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(lang) ->
          lang

        is_list(lang) ->
          :erlang.list_to_binary(lang)

        lang == :undefined ->
          <<>>
      end::binary>>
  end

  def encode(
        r_ssh_msg_userauth_info_request(
          name: name,
          instruction: inst,
          language_tag: lang,
          num_prompts: numPromtps,
          data: data
        )
      ) do
    <<60::size(8)-unsigned-big-integer,
      <<:erlang.size(:unicode.characters_to_binary(name))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(name)::binary>>::binary,
      <<:erlang.size(:unicode.characters_to_binary(inst))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(inst)::binary>>::binary,
      :erlang.size(
        cond do
          is_binary(lang) ->
            lang

          is_list(lang) ->
            :erlang.list_to_binary(lang)

          lang == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(lang) ->
          lang

        is_list(lang) ->
          :erlang.list_to_binary(lang)

        lang == :undefined ->
          <<>>
      end::binary, numPromtps::size(32)-unsigned-big-integer,
      cond do
        is_binary(data) ->
          data

        is_list(data) ->
          :erlang.list_to_binary(data)

        data == :undefined ->
          <<>>
      end::binary>>
  end

  def encode(r_ssh_msg_userauth_info_response(num_responses: num, data: data)) do
    :lists.foldl(
      fn response, acc ->
        <<acc::binary,
          <<:erlang.size(:unicode.characters_to_binary(response))::size(32)-unsigned-big-integer,
            :unicode.characters_to_binary(response)::binary>>::binary>>
      end,
      <<61::size(8)-unsigned-big-integer, num::size(32)-unsigned-big-integer>>,
      data
    )
  end

  def encode(r_ssh_msg_disconnect(code: code, description: desc, language: lang)) do
    <<1::size(8)-unsigned-big-integer, code::size(32)-unsigned-big-integer,
      <<:erlang.size(:unicode.characters_to_binary(desc))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(desc)::binary>>::binary,
      :erlang.size(
        cond do
          is_binary(lang) ->
            lang

          is_list(lang) ->
            :erlang.list_to_binary(lang)

          lang == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(lang) ->
          lang

        is_list(lang) ->
          :erlang.list_to_binary(lang)

        lang == :undefined ->
          <<>>
      end::binary>>
  end

  def encode(r_ssh_msg_service_request(name: service)) do
    <<5::size(8)-unsigned-big-integer,
      <<:erlang.size(:unicode.characters_to_binary(service))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(service)::binary>>::binary>>
  end

  def encode(r_ssh_msg_service_accept(name: service)) do
    <<6::size(8)-unsigned-big-integer,
      <<:erlang.size(:unicode.characters_to_binary(service))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(service)::binary>>::binary>>
  end

  def encode(r_ssh_msg_ext_info(nr_extensions: n, data: data)) do
    :lists.foldl(
      fn {extName, extVal}, acc ->
        <<acc::binary,
          :erlang.size(
            cond do
              is_binary(extName) ->
                extName

              is_list(extName) ->
                :erlang.list_to_binary(extName)

              extName == :undefined ->
                <<>>
            end
          )::size(32)-unsigned-big-integer,
          cond do
            is_binary(extName) ->
              extName

            is_list(extName) ->
              :erlang.list_to_binary(extName)

            extName == :undefined ->
              <<>>
          end::binary,
          :erlang.size(
            cond do
              is_binary(extVal) ->
                extVal

              is_list(extVal) ->
                :erlang.list_to_binary(extVal)

              extVal == :undefined ->
                <<>>
            end
          )::size(32)-unsigned-big-integer,
          cond do
            is_binary(extVal) ->
              extVal

            is_list(extVal) ->
              :erlang.list_to_binary(extVal)

            extVal == :undefined ->
              <<>>
          end::binary>>
      end,
      <<7::size(8)-unsigned-big-integer, n::size(32)-unsigned-big-integer>>,
      data
    )
  end

  def encode(r_ssh_msg_newkeys()) do
    <<21::size(8)-unsigned-big-integer>>
  end

  def encode(
        r_ssh_msg_kexinit(
          cookie: cookie,
          kex_algorithms: keyAlgs,
          server_host_key_algorithms: hostKeyAlgs,
          encryption_algorithms_client_to_server: encAlgC2S,
          encryption_algorithms_server_to_client: encAlgS2C,
          mac_algorithms_client_to_server: macAlgC2S,
          mac_algorithms_server_to_client: macAlgS2C,
          compression_algorithms_client_to_server: compAlgS2C,
          compression_algorithms_server_to_client: compAlgC2S,
          languages_client_to_server: langC2S,
          languages_server_to_client: langS2C,
          first_kex_packet_follows: bool,
          reserved: reserved
        )
      ) do
    <<20::size(8)-unsigned-big-integer, cookie::binary,
      :erlang.size(:ssh_bits.name_list(keyAlgs))::size(32)-unsigned-big-integer,
      :ssh_bits.name_list(keyAlgs)::binary,
      :erlang.size(:ssh_bits.name_list(hostKeyAlgs))::size(32)-unsigned-big-integer,
      :ssh_bits.name_list(hostKeyAlgs)::binary,
      :erlang.size(:ssh_bits.name_list(encAlgC2S))::size(32)-unsigned-big-integer,
      :ssh_bits.name_list(encAlgC2S)::binary,
      :erlang.size(:ssh_bits.name_list(encAlgS2C))::size(32)-unsigned-big-integer,
      :ssh_bits.name_list(encAlgS2C)::binary,
      :erlang.size(:ssh_bits.name_list(macAlgC2S))::size(32)-unsigned-big-integer,
      :ssh_bits.name_list(macAlgC2S)::binary,
      :erlang.size(:ssh_bits.name_list(macAlgS2C))::size(32)-unsigned-big-integer,
      :ssh_bits.name_list(macAlgS2C)::binary,
      :erlang.size(:ssh_bits.name_list(compAlgS2C))::size(32)-unsigned-big-integer,
      :ssh_bits.name_list(compAlgS2C)::binary,
      :erlang.size(:ssh_bits.name_list(compAlgC2S))::size(32)-unsigned-big-integer,
      :ssh_bits.name_list(compAlgC2S)::binary,
      :erlang.size(:ssh_bits.name_list(langC2S))::size(32)-unsigned-big-integer,
      :ssh_bits.name_list(langC2S)::binary,
      :erlang.size(:ssh_bits.name_list(langS2C))::size(32)-unsigned-big-integer,
      :ssh_bits.name_list(langS2C)::binary,
      case bool do
        true ->
          1

        false ->
          0
      end::size(8)-unsigned-big-integer, reserved::size(32)-unsigned-big-integer>>
  end

  def encode(r_ssh_msg_kexdh_init(e: e)) do
    <<30::size(8)-unsigned-big-integer, :ssh_bits.mpint(e)::binary>>
  end

  def encode(r_ssh_msg_kexdh_reply(public_host_key: {key, sigAlg}, f: f, h_sig: signature)) do
    encKey = ssh2_pubkey_encode(key)
    encSign = encode_signature(key, sigAlg, signature)

    <<31::size(8)-unsigned-big-integer, :erlang.size(encKey)::size(32)-unsigned-big-integer,
      encKey::binary, :ssh_bits.mpint(f)::binary,
      :erlang.size(encSign)::size(32)-unsigned-big-integer, encSign::binary>>
  end

  def encode(r_ssh_msg_kex_dh_gex_request(min: min, n: n, max: max)) do
    <<34::size(8)-unsigned-big-integer, min::size(32)-unsigned-big-integer,
      n::size(32)-unsigned-big-integer, max::size(32)-unsigned-big-integer>>
  end

  def encode(r_ssh_msg_kex_dh_gex_request_old(n: n)) do
    <<30::size(8)-unsigned-big-integer, n::size(32)-unsigned-big-integer>>
  end

  def encode(r_ssh_msg_kex_dh_gex_group(p: prime, g: generator)) do
    <<31::size(8)-unsigned-big-integer, :ssh_bits.mpint(prime)::binary,
      :ssh_bits.mpint(generator)::binary>>
  end

  def encode(r_ssh_msg_kex_dh_gex_init(e: public)) do
    <<32::size(8)-unsigned-big-integer, :ssh_bits.mpint(public)::binary>>
  end

  def encode(r_ssh_msg_kex_dh_gex_reply(public_host_key: {key, sigAlg}, f: f, h_sig: signature)) do
    encKey = ssh2_pubkey_encode(key)
    encSign = encode_signature(key, sigAlg, signature)

    <<33::size(8)-unsigned-big-integer, :erlang.size(encKey)::size(32)-unsigned-big-integer,
      encKey::binary, :ssh_bits.mpint(f)::binary,
      :erlang.size(encSign)::size(32)-unsigned-big-integer, encSign::binary>>
  end

  def encode(r_ssh_msg_kex_ecdh_init(q_c: q_c)) do
    <<30::size(8)-unsigned-big-integer, :erlang.size(q_c)::size(32)-unsigned-big-integer,
      q_c::binary>>
  end

  def encode(r_ssh_msg_kex_ecdh_reply(public_host_key: {key, sigAlg}, q_s: q_s, h_sig: sign)) do
    encKey = ssh2_pubkey_encode(key)
    encSign = encode_signature(key, sigAlg, sign)

    <<31::size(8)-unsigned-big-integer, :erlang.size(encKey)::size(32)-unsigned-big-integer,
      encKey::binary, :erlang.size(q_s)::size(32)-unsigned-big-integer, q_s::binary,
      :erlang.size(encSign)::size(32)-unsigned-big-integer, encSign::binary>>
  end

  def encode(r_ssh_msg_ignore(data: data)) do
    <<2::size(8)-unsigned-big-integer,
      <<:erlang.size(:unicode.characters_to_binary(data))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(data)::binary>>::binary>>
  end

  def encode(r_ssh_msg_unimplemented(sequence: seq)) do
    <<3::size(8)-unsigned-big-integer, seq::size(32)-unsigned-big-integer>>
  end

  def encode(r_ssh_msg_debug(always_display: bool, message: msg, language: lang)) do
    <<4::size(8)-unsigned-big-integer,
      case bool do
        true ->
          1

        false ->
          0
      end::size(8)-unsigned-big-integer,
      <<:erlang.size(:unicode.characters_to_binary(msg))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(msg)::binary>>::binary,
      :erlang.size(
        cond do
          is_binary(lang) ->
            lang

          is_list(lang) ->
            :erlang.list_to_binary(lang)

          lang == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(lang) ->
          lang

        is_list(lang) ->
          :erlang.list_to_binary(lang)

        lang == :undefined ->
          <<>>
      end::binary>>
  end

  def decode(
        <<80::size(8)-unsigned-big-integer, __0::size(32)-unsigned-big-integer,
          name::size(__0)-binary, bool::size(8)-unsigned-big-integer, data::binary>>
      ) do
    r_ssh_msg_global_request(name: name, want_reply: erl_boolean(bool), data: data)
  end

  def decode(<<81::size(8)-unsigned-big-integer, data::binary>>) do
    r_ssh_msg_request_success(data: data)
  end

  def decode(<<82::size(8)-unsigned-big-integer>>) do
    r_ssh_msg_request_failure()
  end

  def decode(
        <<90::size(8)-unsigned-big-integer, __0::size(32)-unsigned-big-integer,
          type::size(__0)-binary, sender::size(32)-unsigned-big-integer,
          window::size(32)-unsigned-big-integer, max::size(32)-unsigned-big-integer,
          data::binary>>
      ) do
    r_ssh_msg_channel_open(
      channel_type: :erlang.binary_to_list(type),
      sender_channel: sender,
      initial_window_size: window,
      maximum_packet_size: max,
      data: data
    )
  end

  def decode(
        <<91::size(8)-unsigned-big-integer, recipient::size(32)-unsigned-big-integer,
          sender::size(32)-unsigned-big-integer, initWindowSize::size(32)-unsigned-big-integer,
          maxPacketSize::size(32)-unsigned-big-integer, data::binary>>
      ) do
    r_ssh_msg_channel_open_confirmation(
      recipient_channel: recipient,
      sender_channel: sender,
      initial_window_size: initWindowSize,
      maximum_packet_size: maxPacketSize,
      data: data
    )
  end

  def decode(
        <<92::size(8)-unsigned-big-integer, recipient::size(32)-unsigned-big-integer,
          reason::size(32)-unsigned-big-integer, __0::size(32)-unsigned-big-integer,
          desc::size(__0)-binary, __1::size(32)-unsigned-big-integer, lang::size(__1)-binary>>
      ) do
    r_ssh_msg_channel_open_failure(
      recipient_channel: recipient,
      reason: reason,
      description: ucl(desc),
      lang: lang
    )
  end

  def decode(
        <<93::size(8)-unsigned-big-integer, recipient::size(32)-unsigned-big-integer,
          bytes::size(32)-unsigned-big-integer>>
      ) do
    r_ssh_msg_channel_window_adjust(recipient_channel: recipient, bytes_to_add: bytes)
  end

  def decode(
        <<94::size(8)-unsigned-big-integer, recipient::size(32)-unsigned-big-integer,
          __0::size(32)-unsigned-big-integer, data::size(__0)-binary>>
      ) do
    r_ssh_msg_channel_data(recipient_channel: recipient, data: data)
  end

  def decode(
        <<95::size(8)-unsigned-big-integer, recipient::size(32)-unsigned-big-integer,
          dataType::size(32)-unsigned-big-integer, __0::size(32)-unsigned-big-integer,
          data::size(__0)-binary>>
      ) do
    r_ssh_msg_channel_extended_data(
      recipient_channel: recipient,
      data_type_code: dataType,
      data: data
    )
  end

  def decode(<<96::size(8)-unsigned-big-integer, recipient::size(32)-unsigned-big-integer>>) do
    r_ssh_msg_channel_eof(recipient_channel: recipient)
  end

  def decode(<<97::size(8)-unsigned-big-integer, recipient::size(32)-unsigned-big-integer>>) do
    r_ssh_msg_channel_close(recipient_channel: recipient)
  end

  def decode(
        <<98::size(8)-unsigned-big-integer, recipient::size(32)-unsigned-big-integer,
          __0::size(32)-unsigned-big-integer, requestType::size(__0)-binary,
          bool::size(8)-unsigned-big-integer, data::binary>> = bytes
      ) do
    try do
      r_ssh_msg_channel_request(
        recipient_channel: recipient,
        request_type: ucl(requestType),
        want_reply: erl_boolean(bool),
        data: data
      )
    catch
      _, _ ->
        r_ssh_msg_channel_request(
          recipient_channel: recipient,
          request_type: :faulty_msg,
          data: bytes
        )
    end
  end

  def decode(<<99::size(8)-unsigned-big-integer, recipient::size(32)-unsigned-big-integer>>) do
    r_ssh_msg_channel_success(recipient_channel: recipient)
  end

  def decode(<<100::size(8)-unsigned-big-integer, recipient::size(32)-unsigned-big-integer>>) do
    r_ssh_msg_channel_failure(recipient_channel: recipient)
  end

  def decode(
        <<50::size(8)-unsigned-big-integer, __0::size(32)-unsigned-big-integer,
          user::size(__0)-binary, __1::size(32)-unsigned-big-integer, service::size(__1)-binary,
          __2::size(32)-unsigned-big-integer, method::size(__2)-binary, data::binary>>
      ) do
    r_ssh_msg_userauth_request(
      user: ucl(user),
      service: ucl(service),
      method: ucl(method),
      data: data
    )
  end

  def decode(
        <<51::size(8)-unsigned-big-integer, __0::size(32)-unsigned-big-integer,
          auths::size(__0)-binary, bool::size(8)-unsigned-big-integer>>
      ) do
    r_ssh_msg_userauth_failure(
      authentications: ucl(auths),
      partial_success: erl_boolean(bool)
    )
  end

  def decode(<<52::size(8)-unsigned-big-integer>>) do
    r_ssh_msg_userauth_success()
  end

  def decode(
        <<53::size(8)-unsigned-big-integer, __0::size(32)-unsigned-big-integer,
          banner::size(__0)-binary, __1::size(32)-unsigned-big-integer, lang::size(__1)-binary>>
      ) do
    r_ssh_msg_userauth_banner(message: banner, language: lang)
  end

  def decode(
        <<60::size(8)-unsigned-big-integer, __0::size(32)-unsigned-big-integer,
          name::size(__0)-binary, __1::size(32)-unsigned-big-integer, inst::size(__1)-binary,
          __2::size(32)-unsigned-big-integer, lang::size(__2)-binary,
          numPromtps::size(32)-unsigned-big-integer, data::binary>>
      ) do
    r_ssh_msg_userauth_info_request(
      name: name,
      instruction: inst,
      language_tag: lang,
      num_prompts: numPromtps,
      data: data
    )
  end

  def decode(
        <<60::size(8)-unsigned-big-integer, __0::size(32)-unsigned-big-integer,
          prompt::size(__0)-binary, __1::size(32)-unsigned-big-integer, lang::size(__1)-binary>>
      ) do
    r_ssh_msg_userauth_passwd_changereq(prompt: prompt, languge: lang)
  end

  def decode(
        <<60::size(8)-unsigned-big-integer, __0::size(32)-unsigned-big-integer,
          alg::size(__0)-binary, keyBlob::binary>>
      ) do
    r_ssh_msg_userauth_pk_ok(algorithm_name: alg, key_blob: keyBlob)
  end

  def decode(
        <<61::size(8)-unsigned-big-integer, num::size(32)-unsigned-big-integer, data::binary>>
      ) do
    r_ssh_msg_userauth_info_response(num_responses: num, data: data)
  end

  def decode(
        <<7::size(8)-unsigned-big-integer, n::size(32)-unsigned-big-integer, binData::binary>>
      ) do
    data =
      bin_foldr(
        fn
          bin, acc when length(acc) == n ->
            {bin, acc}

          <<__0::size(32)-unsigned-big-integer, v0::size(__0)-binary,
            __1::size(32)-unsigned-big-integer, v1::size(__1)-binary, rest::binary>>,
          acc ->
            {rest,
             [
               {:erlang.binary_to_list(v0), :erlang.binary_to_list(v1)}
               | acc
             ]}
        end,
        [],
        binData
      )

    r_ssh_msg_ext_info(nr_extensions: n, data: data)
  end

  def decode(<<20::size(8)-unsigned-big-integer, cookie::size(128), data::binary>>) do
    decode_kex_init(data, [cookie, :ssh_msg_kexinit], 10)
  end

  def decode(
        <<"dh", 30::size(8)-unsigned-big-integer, __0::size(32)-unsigned-big-integer,
          e::size(__0)-big-signed-integer-unit(8)>>
      ) do
    r_ssh_msg_kexdh_init(e: e)
  end

  def decode(
        <<"dh", 31::size(8)-unsigned-big-integer, __0::size(32)-unsigned-big-integer,
          key::size(__0)-binary, __1::size(32)-unsigned-big-integer,
          f::size(__1)-big-signed-integer-unit(8), __2::size(32)-unsigned-big-integer,
          hashsign::size(__2)-binary>>
      ) do
    r_ssh_msg_kexdh_reply(
      public_host_key: ssh2_pubkey_decode(key),
      f: f,
      h_sig: decode_signature(hashsign)
    )
  end

  def decode(
        <<34::size(8)-unsigned-big-integer, min::size(32)-unsigned-big-integer,
          n::size(32)-unsigned-big-integer, max::size(32)-unsigned-big-integer>>
      ) do
    r_ssh_msg_kex_dh_gex_request(min: min, n: n, max: max)
  end

  def decode(<<"dh_gex", 30::size(8)-unsigned-big-integer, n::size(32)-unsigned-big-integer>>) do
    r_ssh_msg_kex_dh_gex_request_old(n: n)
  end

  def decode(
        <<"dh_gex", 31::size(8)-unsigned-big-integer, __0::size(32)-unsigned-big-integer,
          prime::size(__0)-big-signed-integer-unit(8), __1::size(32)-unsigned-big-integer,
          generator::size(__1)-big-signed-integer-unit(8)>>
      ) do
    r_ssh_msg_kex_dh_gex_group(p: prime, g: generator)
  end

  def decode(
        <<32::size(8)-unsigned-big-integer, __0::size(32)-unsigned-big-integer,
          e::size(__0)-big-signed-integer-unit(8)>>
      ) do
    r_ssh_msg_kex_dh_gex_init(e: e)
  end

  def decode(
        <<33::size(8)-unsigned-big-integer, __0::size(32)-unsigned-big-integer,
          key::size(__0)-binary, __1::size(32)-unsigned-big-integer,
          f::size(__1)-big-signed-integer-unit(8), __2::size(32)-unsigned-big-integer,
          hashsign::size(__2)-binary>>
      ) do
    r_ssh_msg_kex_dh_gex_reply(
      public_host_key: ssh2_pubkey_decode(key),
      f: f,
      h_sig: decode_signature(hashsign)
    )
  end

  def decode(
        <<"ecdh", 30::size(8)-unsigned-big-integer, __0::size(32)-unsigned-big-integer,
          q_c::size(__0)-binary>>
      ) do
    r_ssh_msg_kex_ecdh_init(q_c: q_c)
  end

  def decode(
        <<"ecdh", 31::size(8)-unsigned-big-integer, __1::size(32)-unsigned-big-integer,
          key::size(__1)-binary, __2::size(32)-unsigned-big-integer, q_s::size(__2)-binary,
          __3::size(32)-unsigned-big-integer, sig::size(__3)-binary>>
      ) do
    r_ssh_msg_kex_ecdh_reply(
      public_host_key: ssh2_pubkey_decode(key),
      q_s: q_s,
      h_sig: decode_signature(sig)
    )
  end

  def decode(<<5, __0::size(32)-unsigned-big-integer, service::size(__0)-binary>>) do
    r_ssh_msg_service_request(name: ucl(service))
  end

  def decode(<<6, __0::size(32)-unsigned-big-integer, service::size(__0)-binary>>) do
    r_ssh_msg_service_accept(name: ucl(service))
  end

  def decode(
        <<1::size(8)-unsigned-big-integer, code::size(32)-unsigned-big-integer,
          __0::size(32)-unsigned-big-integer, desc::size(__0)-binary,
          __1::size(32)-unsigned-big-integer, lang::size(__1)-binary>>
      ) do
    r_ssh_msg_disconnect(code: code, description: ucl(desc), language: lang)
  end

  def decode(
        <<1::size(8)-unsigned-big-integer, code::size(32)-unsigned-big-integer,
          __0::size(32)-unsigned-big-integer, desc::size(__0)-binary>>
      ) do
    r_ssh_msg_disconnect(code: code, description: ucl(desc), language: "en")
  end

  def decode(<<21>>) do
    r_ssh_msg_newkeys()
  end

  def decode(
        <<2::size(8)-unsigned-big-integer, __0::size(32)-unsigned-big-integer,
          data::size(__0)-binary>>
      ) do
    r_ssh_msg_ignore(data: data)
  end

  def decode(<<3::size(8)-unsigned-big-integer, seq::size(32)-unsigned-big-integer>>) do
    r_ssh_msg_unimplemented(sequence: seq)
  end

  def decode(
        <<4::size(8)-unsigned-big-integer, bool::size(8)-unsigned-big-integer,
          __0::size(32)-unsigned-big-integer, msg::size(__0)-binary,
          __1::size(32)-unsigned-big-integer, lang::size(__1)-binary>>
      ) do
    r_ssh_msg_debug(always_display: erl_boolean(bool), message: msg, language: lang)
  end

  def ssh2_pubkey_encode(r_RSAPublicKey(modulus: n, publicExponent: e)) do
    <<:erlang.size("ssh-rsa")::size(32)-unsigned-big-integer, "ssh-rsa"::binary,
      :ssh_bits.mpint(e)::binary, :ssh_bits.mpint(n)::binary>>
  end

  def ssh2_pubkey_encode({y, r_Dss_Parms(p: p, q: q, g: g)}) do
    <<:erlang.size("ssh-dss")::size(32)-unsigned-big-integer, "ssh-dss"::binary,
      :ssh_bits.mpint(p)::binary, :ssh_bits.mpint(q)::binary, :ssh_bits.mpint(g)::binary,
      :ssh_bits.mpint(y)::binary>>
  end

  def ssh2_pubkey_encode({r_ECPoint(point: q), {:namedCurve, oID}}) do
    curve = :public_key.oid2ssh_curvename(oID)
    keyType = <<"ecdsa-sha2-", curve::binary>>

    <<:erlang.size(keyType)::size(32)-unsigned-big-integer, keyType::binary,
      :erlang.size(curve)::size(32)-unsigned-big-integer, curve::binary,
      :erlang.size(
        cond do
          is_binary(q) ->
            q

          is_list(q) ->
            :erlang.list_to_binary(q)

          q == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(q) ->
          q

        is_list(q) ->
          :erlang.list_to_binary(q)

        q == :undefined ->
          <<>>
      end::binary>>
  end

  def ssh2_pubkey_encode({:ed_pub, :ed25519, key}) do
    <<:erlang.size("ssh-ed25519")::size(32)-unsigned-big-integer, "ssh-ed25519"::binary,
      :erlang.size(
        cond do
          is_binary(key) ->
            key

          is_list(key) ->
            :erlang.list_to_binary(key)

          key == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(key) ->
          key

        is_list(key) ->
          :erlang.list_to_binary(key)

        key == :undefined ->
          <<>>
      end::binary>>
  end

  def ssh2_pubkey_encode({:ed_pub, :ed448, key}) do
    <<:erlang.size("ssh-ed448")::size(32)-unsigned-big-integer, "ssh-ed448"::binary,
      :erlang.size(
        cond do
          is_binary(key) ->
            key

          is_list(key) ->
            :erlang.list_to_binary(key)

          key == :undefined ->
            <<>>
        end
      )::size(32)-unsigned-big-integer,
      cond do
        is_binary(key) ->
          key

        is_list(key) ->
          :erlang.list_to_binary(key)

        key == :undefined ->
          <<>>
      end::binary>>
  end

  def ssh2_pubkey_decode(keyBlob) do
    {key, _RestBlob} = ssh2_pubkey_decode2(keyBlob)
    key
  end

  defp ssh2_pubkey_decode2(
         <<7::size(32)-unsigned-big-integer, "ssh-rsa", _EL::size(32)-unsigned-big-integer,
           e::size(_EL)-big-signed-integer-unit(8), _NL::size(32)-unsigned-big-integer,
           n::size(_NL)-big-signed-integer-unit(8), rest::binary>>
       ) do
    {r_RSAPublicKey(modulus: n, publicExponent: e), rest}
  end

  defp ssh2_pubkey_decode2(
         <<7::size(32)-unsigned-big-integer, "ssh-dss", _PL::size(32)-unsigned-big-integer,
           p::size(_PL)-big-signed-integer-unit(8), _QL::size(32)-unsigned-big-integer,
           q::size(_QL)-big-signed-integer-unit(8), _GL::size(32)-unsigned-big-integer,
           g::size(_GL)-big-signed-integer-unit(8), _YL::size(32)-unsigned-big-integer,
           y::size(_YL)-big-signed-integer-unit(8), rest::binary>>
       ) do
    {{y, r_Dss_Parms(p: p, q: q, g: g)}, rest}
  end

  defp ssh2_pubkey_decode2(<<tL::size(32)-unsigned-big-integer, "ecdsa-sha2-", keyRest::binary>>) do
    sz = tL - 11

    <<_Curve::size(sz)-binary, _IL::size(32)-unsigned-big-integer, sshName::size(_IL)-binary,
      _QL::size(32)-unsigned-big-integer, q::size(_QL)-binary, rest::binary>> = keyRest

    oID = :public_key.ssh_curvename2oid(sshName)
    {{r_ECPoint(point: q), {:namedCurve, oID}}, rest}
  end

  defp ssh2_pubkey_decode2(
         <<11::size(32)-unsigned-big-integer, "ssh-ed25519", _L::size(32)-unsigned-big-integer,
           key::size(_L)-binary, rest::binary>>
       ) do
    {{:ed_pub, :ed25519, key}, rest}
  end

  defp ssh2_pubkey_decode2(
         <<9::size(32)-unsigned-big-integer, "ssh-ed448", _L::size(32)-unsigned-big-integer,
           key::size(_L)-binary, rest::binary>>
       ) do
    {{:ed_pub, :ed448, key}, rest}
  end

  def ssh2_privkey_decode2(
        <<7::size(32)-unsigned-big-integer, "ssh-rsa", _NL::size(32)-unsigned-big-integer,
          n::size(_NL)-big-signed-integer-unit(8), _EL::size(32)-unsigned-big-integer,
          e::size(_EL)-big-signed-integer-unit(8), _DL::size(32)-unsigned-big-integer,
          d::size(_DL)-big-signed-integer-unit(8), _IQMPL::size(32)-unsigned-big-integer,
          iQMP::size(_IQMPL)-big-signed-integer-unit(8), _PL::size(32)-unsigned-big-integer,
          p::size(_PL)-big-signed-integer-unit(8), _QL::size(32)-unsigned-big-integer,
          q::size(_QL)-big-signed-integer-unit(8), rest::binary>>
      ) do
    {r_RSAPrivateKey(
       version: :"two-prime",
       modulus: n,
       publicExponent: e,
       privateExponent: d,
       prime1: p,
       prime2: q,
       coefficient: iQMP
     ), rest}
  end

  def ssh2_privkey_decode2(
        <<7::size(32)-unsigned-big-integer, "ssh-dss", _PL::size(32)-unsigned-big-integer,
          p::size(_PL)-big-signed-integer-unit(8), _QL::size(32)-unsigned-big-integer,
          q::size(_QL)-big-signed-integer-unit(8), _GL::size(32)-unsigned-big-integer,
          g::size(_GL)-big-signed-integer-unit(8), _YL::size(32)-unsigned-big-integer,
          y::size(_YL)-big-signed-integer-unit(8), _XL::size(32)-unsigned-big-integer,
          x::size(_XL)-big-signed-integer-unit(8), rest::binary>>
      ) do
    {r_DSAPrivateKey(version: 0, p: p, q: q, g: g, y: y, x: x), rest}
  end

  def ssh2_privkey_decode2(<<tL::size(32)-unsigned-big-integer, "ecdsa-sha2-", keyRest::binary>>) do
    sz = tL - 11

    <<_Curve::size(sz)-binary, _SNN::size(32)-unsigned-big-integer, curveName::size(_SNN)-binary,
      _QL::size(32)-unsigned-big-integer, q::size(_QL)-binary,
      _PrivL::size(32)-unsigned-big-integer, priv::size(_PrivL)-binary, rest::binary>> = keyRest

    oID = :public_key.ssh_curvename2oid(curveName)

    {r_ECPrivateKey(version: 1, parameters: {:namedCurve, oID}, privateKey: priv, publicKey: q),
     rest}
  end

  def ssh2_privkey_decode2(
        <<11::size(32)-unsigned-big-integer, "ssh-ed25519", _Lpub::size(32)-unsigned-big-integer,
          pub::size(_Lpub)-binary, _Lpriv::size(32)-unsigned-big-integer,
          priv::size(_Lpriv)-binary, rest::binary>>
      ) do
    {{:ed_pri, :ed25519, pub, priv}, rest}
  end

  def ssh2_privkey_decode2(
        <<9::size(32)-unsigned-big-integer, "ssh-ed448", _Lpub::size(32)-unsigned-big-integer,
          pub::size(_Lpub)-binary, _Lpriv::size(32)-unsigned-big-integer,
          priv::size(_Lpriv)-binary, rest::binary>>
      ) do
    {{:ed_pri, :ed448, pub, priv}, rest}
  end

  defp bin_foldr(fun, acc, bin) do
    :lists.reverse(bin_foldl(fun, acc, bin))
  end

  defp bin_foldl(_, acc, <<>>) do
    acc
  end

  defp bin_foldl(fun, acc0, bin0) do
    case fun.(bin0, acc0) do
      {^bin0, ^acc0} ->
        acc0

      {bin, acc} ->
        bin_foldl(fun, acc, bin)
    end
  end

  def decode_keyboard_interactive_prompts(<<>>, acc) do
    :lists.reverse(acc)
  end

  def decode_keyboard_interactive_prompts(<<0>>, acc) do
    :lists.reverse(acc)
  end

  def decode_keyboard_interactive_prompts(
        <<__0::size(32)-unsigned-big-integer, prompt::size(__0)-binary,
          bool::size(8)-unsigned-big-integer, bin::binary>>,
        acc
      ) do
    decode_keyboard_interactive_prompts(
      bin,
      [{prompt, erl_boolean(bool)} | acc]
    )
  end

  defp erl_boolean(0) do
    false
  end

  defp erl_boolean(1) do
    true
  end

  defp decode_kex_init(
         <<bool::size(8)-unsigned-big-integer, x::size(32)-unsigned-big-integer>>,
         acc,
         0
       ) do
    :erlang.list_to_tuple(
      :lists.reverse([
        [x, erl_boolean(bool)]
        | acc
      ])
    )
  end

  defp decode_kex_init(<<bool::size(8)-unsigned-big-integer>>, acc, 0) do
    x = 0

    :erlang.list_to_tuple(
      :lists.reverse([
        [x, erl_boolean(bool)]
        | acc
      ])
    )
  end

  defp decode_kex_init(
         <<__0::size(32)-unsigned-big-integer, data::size(__0)-binary, rest::binary>>,
         acc,
         n
       ) do
    names = :string.tokens(ucl(data), ',')
    decode_kex_init(rest, [names | acc], n - 1)
  end

  defp decode_signature(
         <<__0::size(32)-unsigned-big-integer, alg::size(__0)-binary,
           _::size(32)-unsigned-big-integer, signature::binary>>
       ) do
    {:erlang.binary_to_list(alg), signature}
  end

  defp encode_signature(r_RSAPublicKey(), sigAlg, signature) do
    signName = :erlang.list_to_binary(:erlang.atom_to_list(sigAlg))

    <<:erlang.size(signName)::size(32)-unsigned-big-integer, signName::binary,
      :erlang.size(signature)::size(32)-unsigned-big-integer, signature::binary>>
  end

  defp encode_signature({_, r_Dss_Parms()}, _SigAlg, signature) do
    <<:erlang.size("ssh-dss")::size(32)-unsigned-big-integer, "ssh-dss"::binary,
      :erlang.size(signature)::size(32)-unsigned-big-integer, signature::binary>>
  end

  defp encode_signature({r_ECPoint(), {:namedCurve, oID}}, _SigAlg, signature) do
    curve = :public_key.oid2ssh_curvename(oID)

    <<:erlang.size(<<"ecdsa-sha2-", curve::binary>>)::size(32)-unsigned-big-integer,
      <<"ecdsa-sha2-", curve::binary>>::binary,
      :erlang.size(signature)::size(32)-unsigned-big-integer, signature::binary>>
  end

  defp encode_signature({:ed_pub, :ed25519, _}, _SigAlg, signature) do
    <<:erlang.size("ssh-ed25519")::size(32)-unsigned-big-integer, "ssh-ed25519"::binary,
      :erlang.size(signature)::size(32)-unsigned-big-integer, signature::binary>>
  end

  defp encode_signature({:ed_pub, :ed448, _}, _SigAlg, signature) do
    <<:erlang.size("ssh-ed448")::size(32)-unsigned-big-integer, "ssh-ed448"::binary,
      :erlang.size(signature)::size(32)-unsigned-big-integer, signature::binary>>
  end

  def ssh_dbg_trace_points() do
    [:ssh_messages, :raw_messages]
  end

  def ssh_dbg_flags(:ssh_messages) do
    [:c]
  end

  def ssh_dbg_flags(:raw_messages) do
    [:c]
  end

  def ssh_dbg_on(p)
      when p == :ssh_messages or
             p == :raw_messages do
    :dbg.tp(:ssh_message, :encode, 1, :x)
    :dbg.tp(:ssh_message, :decode, 1, :x)
  end

  def ssh_dbg_off(p)
      when p == :ssh_messages or
             p == :raw_messages do
    :dbg.ctpg(:ssh_message, :encode, 1)
    :dbg.ctpg(:ssh_message, :decode, 1)
  end

  def ssh_dbg_format(
        :ssh_messages,
        {:call, {:ssh_message, :encode, [msg]}}
      ) do
    name =
      :string.to_upper(
        :erlang.atom_to_list(
          :erlang.element(
            1,
            msg
          )
        )
      )

    ['Going to send ', name, ':\n', wr_record(:ssh_dbg.shrink_bin(msg))]
  end

  def ssh_dbg_format(
        :ssh_messages,
        {:return_from, {:ssh_message, :encode, 1}, _Ret}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :ssh_messages,
        {:call, {:ssh_message, :decode, [_]}}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :ssh_messages,
        {:return_from, {:ssh_message, :decode, 1}, msg}
      ) do
    name =
      :string.to_upper(
        :erlang.atom_to_list(
          :erlang.element(
            1,
            msg
          )
        )
      )

    ['Received ', name, ':\n', wr_record(:ssh_dbg.shrink_bin(msg))]
  end

  def ssh_dbg_format(
        :raw_messages,
        {:call, {:ssh_message, :decode, [bytesPT]}}
      ) do
    ['Received plain text bytes (shown after decryption):\n', :io_lib.format('~p', [bytesPT])]
  end

  def ssh_dbg_format(
        :raw_messages,
        {:return_from, {:ssh_message, :decode, 1}, _Ret}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :raw_messages,
        {:call, {:ssh_message, :encode, [_]}}
      ) do
    :skip
  end

  def ssh_dbg_format(
        :raw_messages,
        {:return_from, {:ssh_message, :encode, 1}, bytesPT}
      ) do
    [
      'Going to send plain text bytes (shown before encryption):\n',
      :io_lib.format('~p', [bytesPT])
    ]
  end

  defp wr_record(r = r_ssh_msg_disconnect()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_ssh_msg_disconnect(r_ssh_msg_disconnect())), [])
  end

  defp wr_record(r = r_ssh_msg_ignore()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_ssh_msg_ignore(r_ssh_msg_ignore())), [])
  end

  defp wr_record(r = r_ssh_msg_unimplemented()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_ssh_msg_unimplemented(r_ssh_msg_unimplemented())), [])
  end

  defp wr_record(r = r_ssh_msg_debug()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_ssh_msg_debug(r_ssh_msg_debug())), [])
  end

  defp wr_record(r = r_ssh_msg_service_request()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_service_request(r_ssh_msg_service_request())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_service_accept()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_ssh_msg_service_accept(r_ssh_msg_service_accept())), [])
  end

  defp wr_record(r = r_ssh_msg_kexinit()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_ssh_msg_kexinit(r_ssh_msg_kexinit())), [])
  end

  defp wr_record(r = r_ssh_msg_kexdh_init()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_ssh_msg_kexdh_init(r_ssh_msg_kexdh_init())), [])
  end

  defp wr_record(r = r_ssh_msg_kexdh_reply()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_ssh_msg_kexdh_reply(r_ssh_msg_kexdh_reply())), [])
  end

  defp wr_record(r = r_ssh_msg_newkeys()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_ssh_msg_newkeys(r_ssh_msg_newkeys())), [])
  end

  defp wr_record(r = r_ssh_msg_ext_info()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_ssh_msg_ext_info(r_ssh_msg_ext_info())), [])
  end

  defp wr_record(r = r_ssh_msg_kex_dh_gex_request()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_kex_dh_gex_request(r_ssh_msg_kex_dh_gex_request())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_kex_dh_gex_request_old()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_kex_dh_gex_request_old(r_ssh_msg_kex_dh_gex_request_old())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_kex_dh_gex_group()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_kex_dh_gex_group(r_ssh_msg_kex_dh_gex_group())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_kex_dh_gex_init()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_kex_dh_gex_init(r_ssh_msg_kex_dh_gex_init())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_kex_dh_gex_reply()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_kex_dh_gex_reply(r_ssh_msg_kex_dh_gex_reply())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_kex_ecdh_init()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_ssh_msg_kex_ecdh_init(r_ssh_msg_kex_ecdh_init())), [])
  end

  defp wr_record(r = r_ssh_msg_kex_ecdh_reply()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_ssh_msg_kex_ecdh_reply(r_ssh_msg_kex_ecdh_reply())), [])
  end

  defp wr_record(r = r_ssh_msg_userauth_request()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_userauth_request(r_ssh_msg_userauth_request())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_userauth_failure()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_userauth_failure(r_ssh_msg_userauth_failure())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_userauth_success()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_userauth_success(r_ssh_msg_userauth_success())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_userauth_banner()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_userauth_banner(r_ssh_msg_userauth_banner())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_userauth_passwd_changereq()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_userauth_passwd_changereq(r_ssh_msg_userauth_passwd_changereq())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_userauth_pk_ok()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_ssh_msg_userauth_pk_ok(r_ssh_msg_userauth_pk_ok())), [])
  end

  defp wr_record(r = r_ssh_msg_userauth_info_request()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_userauth_info_request(r_ssh_msg_userauth_info_request())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_userauth_info_response()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_userauth_info_response(r_ssh_msg_userauth_info_response())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_global_request()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_ssh_msg_global_request(r_ssh_msg_global_request())), [])
  end

  defp wr_record(r = r_ssh_msg_request_success()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_request_success(r_ssh_msg_request_success())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_request_failure()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_request_failure(r_ssh_msg_request_failure())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_channel_open()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_ssh_msg_channel_open(r_ssh_msg_channel_open())), [])
  end

  defp wr_record(r = r_ssh_msg_channel_open_confirmation()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_channel_open_confirmation(r_ssh_msg_channel_open_confirmation())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_channel_open_failure()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_channel_open_failure(r_ssh_msg_channel_open_failure())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_channel_window_adjust()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_channel_window_adjust(r_ssh_msg_channel_window_adjust())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_channel_data()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_ssh_msg_channel_data(r_ssh_msg_channel_data())), [])
  end

  defp wr_record(r = r_ssh_msg_channel_extended_data()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_channel_extended_data(r_ssh_msg_channel_extended_data())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_channel_eof()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_ssh_msg_channel_eof(r_ssh_msg_channel_eof())), [])
  end

  defp wr_record(r = r_ssh_msg_channel_close()) do
    :ssh_dbg.wr_record(r, Keyword.keys(r_ssh_msg_channel_close(r_ssh_msg_channel_close())), [])
  end

  defp wr_record(r = r_ssh_msg_channel_request()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_channel_request(r_ssh_msg_channel_request())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_channel_success()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_channel_success(r_ssh_msg_channel_success())),
      []
    )
  end

  defp wr_record(r = r_ssh_msg_channel_failure()) do
    :ssh_dbg.wr_record(
      r,
      Keyword.keys(r_ssh_msg_channel_failure(r_ssh_msg_channel_failure())),
      []
    )
  end

  defp wr_record(r) do
    :io_lib.format(:"~p~n", [r])
  end
end
