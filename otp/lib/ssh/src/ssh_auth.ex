defmodule :m_ssh_auth do
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

  Record.defrecord(:r_ssh_agent_success, :ssh_agent_success, [])
  Record.defrecord(:r_ssh_agent_failure, :ssh_agent_failure, [])
  Record.defrecord(:r_ssh_agent_identities_request, :ssh_agent_identities_request, [])

  Record.defrecord(:r_ssh_agent_key, :ssh_agent_key,
    blob: :undefined,
    comment: :undefined
  )

  Record.defrecord(:r_ssh_agent_identities_response, :ssh_agent_identities_response,
    keys: :undefined
  )

  Record.defrecord(:r_ssh_agent_sign_request, :ssh_agent_sign_request,
    key_blob: :undefined,
    data: :undefined,
    flags: :undefined
  )

  Record.defrecord(:r_ssh_agent_signature, :ssh_agent_signature,
    format: :undefined,
    blob: :undefined
  )

  Record.defrecord(:r_ssh_agent_sign_response, :ssh_agent_sign_response, signature: :undefined)

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
  def userauth_request_msg(
        r_ssh(
          userauth_methods: serverMethods,
          userauth_supported_methods: userPrefMethods,
          userauth_preference: clientMethods0
        ) = ssh0
      ) do
    case sort_select_mthds(clientMethods0, userPrefMethods, serverMethods) do
      [] ->
        {:send_disconnect, 14, ssh0}

      [{pref, module, function, args} | prefs] ->
        ssh =
          case pref do
            'keyboard-interactive' ->
              ssh0

            _ ->
              r_ssh(ssh0, userauth_preference: prefs)
          end

        case apply(module, function, [args ++ [ssh]]) do
          {:not_ok, ssh1} ->
            userauth_request_msg(r_ssh(ssh1, userauth_preference: prefs))

          result ->
            {pref, result}
        end
    end
  end

  defp sort_select_mthds(clients, :undefined, servers) do
    sort_select_mthds1(
      clients,
      servers,
      :string.tokens('publickey,keyboard-interactive,password', ',')
    )
  end

  defp sort_select_mthds(clients, users0, servers0) do
    sort_select_mthds1(clients, :string.tokens(users0, ','), servers0)
  end

  defp sort_select_mthds1(clients, users0, servers0) do
    servers = unique(servers0)
    users = unique(users0)

    for key <- users, :lists.member(key, servers), c <- clients, :erlang.element(1, c) == key do
      c
    end
  end

  defp unique(l) do
    :lists.reverse(
      :lists.foldl(
        fn e, acc ->
          case :lists.member(e, acc) do
            true ->
              acc

            false ->
              [e | acc]
          end
        end,
        [],
        l
      )
    )
  end

  def password_msg([r_ssh(opts: opts, user: user, service: service) = ssh0]) do
    ioCb = :ssh_options.get_value(:internal_options, :io_cb, opts, :ssh_auth, 101)

    {password, ssh} =
      case :ssh_options.get_value(:user_options, :password, opts, :ssh_auth, 103) do
        :undefined when ioCb == :ssh_no_io ->
          {:not_ok, ssh0}

        :undefined ->
          {ioCb.read_password('ssh password: ', opts), ssh0}

        pW ->
          {pW,
           r_ssh(ssh0,
             opts:
               :ssh_options.put_value(:user_options, {:password, :not_ok}, opts, :ssh_auth, 110)
           )}
      end

    case password do
      :not_ok ->
        {:not_ok, ssh}

      _ ->
        {r_ssh_msg_userauth_request(
           user: user,
           service: service,
           method: 'password',
           data:
             <<0::size(8)-unsigned-big-integer,
               :erlang.size(:unicode.characters_to_binary(password))::size(32)-unsigned-big-integer,
               :unicode.characters_to_binary(password)::binary>>
         ), ssh}
    end
  end

  def keyboard_interactive_msg([r_ssh(user: user, opts: opts, service: service) = ssh]) do
    case :ssh_options.get_value(:user_options, :password, opts, :ssh_auth, 129) do
      :not_ok ->
        {:not_ok, ssh}

      _ ->
        {r_ssh_msg_userauth_request(
           user: user,
           service: service,
           method: 'keyboard-interactive',
           data:
             <<:erlang.size("")::size(32)-unsigned-big-integer, ""::binary,
               :erlang.size(<<>>)::size(32)-unsigned-big-integer, <<>>::binary>>
         ), ssh}
    end
  end

  def get_public_key(sigAlg, r_ssh(opts: opts)) do
    keyAlg = key_alg(sigAlg)

    case :ssh_transport.call_KeyCb(:user_key, [keyAlg], opts) do
      {:ok, {:ssh2_pubkey, pubKeyBlob}} ->
        {:ok, {:ssh2_pubkey, pubKeyBlob}}

      {:ok, privKey} ->
        try do
          true = :ssh_transport.valid_key_sha_alg(:private, privKey, keyAlg)
          key = :ssh_transport.extract_public_key(privKey)
          :ssh_message.ssh2_pubkey_encode(key)
        catch
          _, _ ->
            :not_ok
        else
          pubKeyBlob ->
            {:ok, {privKey, pubKeyBlob}}
        end

      _Error ->
        :not_ok
    end
  end

  def publickey_msg([
        sigAlg,
        r_ssh(user: user, session_id: sessionId, service: service, opts: opts) = ssh
      ]) do
    case get_public_key(sigAlg, ssh) do
      {:ok, {_, pubKeyBlob} = key} ->
        sigAlgStr = :erlang.atom_to_list(sigAlg)
        sigData = build_sig_data(sessionId, user, service, pubKeyBlob, sigAlgStr)

        sig =
          case key do
            {:ssh2_pubkey, ^pubKeyBlob} ->
              :ssh_transport.call_KeyCb(:sign, [pubKeyBlob, sigData], opts)

            {privKey, ^pubKeyBlob} ->
              hash = :ssh_transport.sha(sigAlg)
              :ssh_transport.sign(sigData, hash, privKey)
          end

        sigBlob =
          :erlang.list_to_binary([
            <<:erlang.size(:unicode.characters_to_binary(sigAlgStr))::size(32)-unsigned-big-integer,
              :unicode.characters_to_binary(sigAlgStr)::binary>>,
            <<:erlang.size(sig)::size(32)-unsigned-big-integer, sig::binary>>
          ])

        {r_ssh_msg_userauth_request(
           user: user,
           service: service,
           method: 'publickey',
           data: [
             1,
             <<:erlang.size(:unicode.characters_to_binary(sigAlgStr))::size(32)-unsigned-big-integer,
               :unicode.characters_to_binary(sigAlgStr)::binary>>,
             <<:erlang.size(pubKeyBlob)::size(32)-unsigned-big-integer, pubKeyBlob::binary>>,
             <<:erlang.size(sigBlob)::size(32)-unsigned-big-integer, sigBlob::binary>>
           ]
         ), ssh}

      _ ->
        {:not_ok, ssh}
    end
  end

  def service_request_msg(ssh) do
    {r_ssh_msg_service_request(name: 'ssh-userauth'), r_ssh(ssh, service: 'ssh-userauth')}
  end

  def init_userauth_request_msg(r_ssh(opts: opts) = ssh) do
    case :ssh_options.get_value(:user_options, :user, opts, :ssh_auth, 208) do
      :undefined ->
        :ssh_connection_handler.disconnect(
          15,
          'Could not determine the users name',
          :ssh_auth,
          211
        )

      user ->
        {r_ssh_msg_userauth_request(
           user: user,
           service: 'ssh-connection',
           method: 'none',
           data: <<>>
         ),
         r_ssh(ssh,
           user: user,
           userauth_preference: method_preference(r_ssh(ssh, :userauth_pubkeys)),
           userauth_methods: :none,
           service: 'ssh-connection'
         )}
    end
  end

  def handle_userauth_request(r_ssh_msg_service_request(name: name = 'ssh-userauth'), _, ssh) do
    {:ok, {r_ssh_msg_service_accept(name: name), r_ssh(ssh, service: 'ssh-connection')}}
  end

  def handle_userauth_request(
        r_ssh_msg_userauth_request(
          user: user,
          service: 'ssh-connection',
          method: 'password',
          data: <<0, sz::size(32)-unsigned-big-integer, binPwd::size(sz)-binary>>
        ),
        _,
        r_ssh(userauth_supported_methods: methods) = ssh
      ) do
    password = :unicode.characters_to_list(binPwd)

    case check_password(user, password, ssh) do
      {true, ssh1} ->
        {:authorized, user, {r_ssh_msg_userauth_success(), ssh1}}

      {false, ssh1} ->
        {:not_authorized, {user, {:error, 'Bad user or password'}},
         {r_ssh_msg_userauth_failure(authentications: methods, partial_success: false), ssh1}}
    end
  end

  def handle_userauth_request(
        r_ssh_msg_userauth_request(
          user: user,
          service: 'ssh-connection',
          method: 'password',
          data: <<1, _::binary>>
        ),
        _,
        r_ssh(userauth_supported_methods: methods) = ssh
      ) do
    {:not_authorized, {user, {:error, 'Password change not supported'}},
     {r_ssh_msg_userauth_failure(authentications: methods, partial_success: false), ssh}}
  end

  def handle_userauth_request(
        r_ssh_msg_userauth_request(user: user, service: 'ssh-connection', method: 'none'),
        _,
        r_ssh(userauth_supported_methods: methods) = ssh
      ) do
    {:not_authorized, {user, :undefined},
     {r_ssh_msg_userauth_failure(authentications: methods, partial_success: false), ssh}}
  end

  def handle_userauth_request(
        r_ssh_msg_userauth_request(
          user: user,
          service: 'ssh-connection',
          method: 'publickey',
          data:
            <<0::size(8)-unsigned-big-integer, aLen::size(32)-unsigned-big-integer,
              bAlg::size(aLen)-binary, kLen::size(32)-unsigned-big-integer,
              keyBlob::size(kLen)-binary, _::binary>>
        ),
        _SessionId,
        r_ssh(userauth_supported_methods: methods) = ssh0
      ) do
    ssh =
      case check_user(user, ssh0) do
        {true, ssh01} ->
          r_ssh(ssh01, user: user)

        {false, ssh01} ->
          r_ssh(ssh01, user: false)
      end

    case pre_verify_sig(user, keyBlob, ssh) do
      true ->
        {:not_authorized, {user, :undefined},
         {r_ssh_msg_userauth_pk_ok(
            algorithm_name: :erlang.binary_to_list(bAlg),
            key_blob: keyBlob
          ), ssh}}

      false ->
        {:not_authorized, {user, :undefined},
         {r_ssh_msg_userauth_failure(authentications: methods, partial_success: false), ssh}}
    end
  end

  def handle_userauth_request(
        r_ssh_msg_userauth_request(
          user: user,
          service: 'ssh-connection',
          method: 'publickey',
          data:
            <<1::size(8)-unsigned-big-integer, aLen::size(32)-unsigned-big-integer,
              bAlg::size(aLen)-binary, kLen::size(32)-unsigned-big-integer,
              keyBlob::size(kLen)-binary, sigWLen::binary>>
        ),
        sessionId,
        r_ssh(
          user: preVerifyUser,
          userauth_supported_methods: methods
        ) = ssh0
      ) do
    {userOk, ssh} = check_user(user, ssh0)

    case (preVerifyUser == user or preVerifyUser == :undefined) and userOk and
           verify_sig(
             sessionId,
             user,
             'ssh-connection',
             bAlg,
             keyBlob,
             sigWLen,
             ssh
           ) do
      true ->
        {:authorized, user, {r_ssh_msg_userauth_success(), ssh}}

      false ->
        {:not_authorized, {user, :undefined},
         {r_ssh_msg_userauth_failure(authentications: methods, partial_success: false), ssh}}
    end
  end

  def handle_userauth_request(
        r_ssh_msg_userauth_request(
          user: user,
          service: 'ssh-connection',
          method: 'keyboard-interactive',
          data: _
        ),
        _,
        r_ssh(opts: opts, kb_tries_left: kbTriesLeft, userauth_supported_methods: methods) = ssh
      ) do
    case kbTriesLeft do
      n when n < 1 ->
        {:not_authorized, {user, {:authmethod, 'keyboard-interactive'}},
         {r_ssh_msg_userauth_failure(authentications: methods, partial_success: false), ssh}}

      _ ->
        default = {'SSH server', 'Enter password for "' ++ user ++ '"', 'password: ', false}

        {name, instruction, prompt, echo} =
          case :ssh_options.get_value(
                 :user_options,
                 :auth_method_kb_interactive_data,
                 opts,
                 :ssh_auth,
                 372
               ) do
            :undefined ->
              default

            {_, _, _, _} = v ->
              v

            f when is_function(f, 4) ->
              {_, peerName} = r_ssh(ssh, :peer)
              f.(peerName, user, 'ssh-connection', r_ssh(ssh, :pwdfun_user_state))

            f when is_function(f) ->
              {_, peerName} = r_ssh(ssh, :peer)
              f.(peerName, user, 'ssh-connection')
          end

        echoEnc =
          case echo do
            true ->
              <<1>>

            false ->
              <<0>>
          end

        msg =
          r_ssh_msg_userauth_info_request(
            name: :unicode.characters_to_list(name),
            instruction: :unicode.characters_to_list(instruction),
            language_tag: '',
            num_prompts: 1,
            data:
              <<:erlang.size(:unicode.characters_to_binary(prompt))::size(32)-unsigned-big-integer,
                :unicode.characters_to_binary(prompt)::binary, echoEnc::binary>>
          )

        {:not_authorized, {user, :undefined}, {msg, r_ssh(ssh, user: user)}}
    end
  end

  def handle_userauth_request(
        r_ssh_msg_userauth_request(user: user, service: 'ssh-connection', method: other),
        _,
        r_ssh(userauth_supported_methods: methods) = ssh
      ) do
    {:not_authorized, {user, {:authmethod, other}},
     {r_ssh_msg_userauth_failure(authentications: methods, partial_success: false), ssh}}
  end

  def handle_userauth_info_request(
        r_ssh_msg_userauth_info_request(
          name: name,
          instruction: instr,
          num_prompts: numPrompts,
          data: data
        ),
        r_ssh(opts: opts) = ssh
      ) do
    promptInfos =
      decode_keyboard_interactive_prompts(
        numPrompts,
        data
      )

    case keyboard_interact_get_responses(opts, name, instr, promptInfos) do
      :not_ok ->
        :not_ok

      responses ->
        {:ok, {r_ssh_msg_userauth_info_response(num_responses: numPrompts, data: responses), ssh}}
    end
  end

  def handle_userauth_info_response(
        r_ssh_msg_userauth_info_response(
          num_responses: 1,
          data: <<sz::size(32)-unsigned-big-integer, password::size(sz)-binary>>
        ),
        r_ssh(
          opts: opts,
          kb_tries_left: kbTriesLeft,
          user: user,
          userauth_supported_methods: methods
        ) = ssh
      ) do
    sendOneEmpty =
      :ssh_options.get_value(:user_options, :tstflg, opts, :ssh_auth, 438) == :one_empty or
        :proplists.get_value(
          :one_empty,
          :ssh_options.get_value(
            :user_options,
            :tstflg,
            opts,
            :ssh_auth,
            440
          ),
          false
        )

    case check_password(user, :unicode.characters_to_list(password), ssh) do
      {true, ssh1} when sendOneEmpty == true ->
        {:authorized_but_one_more, user,
         {r_ssh_msg_userauth_info_request(
            name: '',
            instruction: '',
            language_tag: '',
            num_prompts: 0,
            data: <<0::size(8)-unsigned-big-integer>>
          ), ssh1}}

      {true, ssh1} ->
        {:authorized, user, {r_ssh_msg_userauth_success(), ssh1}}

      {false, ssh1} ->
        {:not_authorized, {user, {:error, 'Bad user or password'}},
         {r_ssh_msg_userauth_failure(authentications: methods, partial_success: false),
          r_ssh(ssh1, kb_tries_left: max(kbTriesLeft - 1, 0))}}
    end
  end

  def handle_userauth_info_response(
        {:extra, r_ssh_msg_userauth_info_response()},
        r_ssh(user: user) = ssh
      ) do
    {:authorized, user, {r_ssh_msg_userauth_success(), ssh}}
  end

  def handle_userauth_info_response(r_ssh_msg_userauth_info_response(), _Auth) do
    :ssh_connection_handler.disconnect(
      7,
      'Server does not support keyboard-interactive',
      :ssh_auth,
      472
    )
  end

  defp method_preference(sigKeyAlgs) do
    pubKeyDefs =
      for a <- sigKeyAlgs do
        {'publickey', :ssh_auth, :publickey_msg, [a]}
      end

    nonPKmethods = [
      {'password', :ssh_auth, :password_msg, []},
      {'keyboard-interactive', :ssh_auth, :keyboard_interactive_msg, []}
    ]

    pubKeyDefs ++ nonPKmethods
  end

  defp check_user(user, ssh) do
    case :ssh_options.get_value(:user_options, :pk_check_user, r_ssh(ssh, :opts), :ssh_auth, 487) do
      true ->
        check_password(user, :pubkey, ssh)

      _ ->
        {true, ssh}
    end
  end

  defp check_password(user, password, r_ssh(opts: opts) = ssh) do
    case :ssh_options.get_value(:user_options, :pwdfun, opts, :ssh_auth, 495) do
      :undefined when password == :pubkey ->
        case :lists.keysearch(
               user,
               1,
               :ssh_options.get_value(:user_options, :user_passwords, opts, :ssh_auth, 498)
             ) do
          {:value, {^user, _}} ->
            {true, ssh}

          false ->
            {false, ssh}
        end

      :undefined ->
        static = get_password_option(opts, user)
        {:crypto.equal_const_time(password, static), ssh}

      checker when is_function(checker, 2) ->
        {checker.(user, password), ssh}

      checker when is_function(checker, 4) ->
        r_ssh(
          pwdfun_user_state: privateState,
          peer: {_, peerAddr = {_, _}}
        ) = ssh

        case checker.(user, password, peerAddr, privateState) do
          true ->
            {true, ssh}

          false ->
            {false, ssh}

          {true, newState} ->
            {true, r_ssh(ssh, pwdfun_user_state: newState)}

          {false, newState} ->
            {false, r_ssh(ssh, pwdfun_user_state: newState)}

          :disconnect ->
            :ssh_connection_handler.disconnect(14, '', :ssh_auth, 525)
        end
    end
  end

  defp get_password_option(opts, user) do
    passwords = :ssh_options.get_value(:user_options, :user_passwords, opts, :ssh_auth, 530)

    case :lists.keysearch(user, 1, passwords) do
      {:value, {^user, pw}} ->
        pw

      false ->
        :ssh_options.get_value(:user_options, :password, opts, :ssh_auth, 533)
    end
  end

  defp pre_verify_sig(user, keyBlob, r_ssh(opts: opts)) do
    try do
      key = :ssh_message.ssh2_pubkey_decode(keyBlob)
      :ssh_transport.call_KeyCb(:is_auth_key, [key, user], opts)
    catch
      _, _ ->
        false
    end
  end

  defp verify_sig(sessionId, user, service, algBin, keyBlob, sigWLen, r_ssh(opts: opts) = ssh) do
    try do
      alg = :erlang.binary_to_list(algBin)
      key = :ssh_message.ssh2_pubkey_decode(keyBlob)
      true = :ssh_transport.call_KeyCb(:is_auth_key, [key, user], opts)
      plainText = build_sig_data(sessionId, user, service, keyBlob, alg)
      <<algSigLen::size(32)-unsigned-big-integer, algSig::size(algSigLen)-binary>> = sigWLen

      <<algLen::size(32)-unsigned-big-integer, _Alg::size(algLen)-binary,
        sigLen::size(32)-unsigned-big-integer, sig::size(sigLen)-binary>> = algSig

      :ssh_transport.verify(plainText, :ssh_transport.sha(alg), sig, key, ssh)
    catch
      _, _ ->
        false
    end
  end

  defp build_sig_data(sessionId, user, service, keyBlob, alg) do
    sig = [
      <<:erlang.size(sessionId)::size(32)-unsigned-big-integer, sessionId::binary>>,
      50,
      <<:erlang.size(:unicode.characters_to_binary(user))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(user)::binary>>,
      <<:erlang.size(:unicode.characters_to_binary(service))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(service)::binary>>,
      <<:erlang.size("publickey")::size(32)-unsigned-big-integer, "publickey"::binary>>,
      1,
      <<:erlang.size(:unicode.characters_to_binary(alg))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(alg)::binary>>,
      <<:erlang.size(keyBlob)::size(32)-unsigned-big-integer, keyBlob::binary>>
    ]

    :erlang.list_to_binary(sig)
  end

  defp key_alg(:"rsa-sha2-256") do
    :"ssh-rsa"
  end

  defp key_alg(:"rsa-sha2-512") do
    :"ssh-rsa"
  end

  defp key_alg(alg) do
    alg
  end

  defp decode_keyboard_interactive_prompts(_NumPrompts, data) do
    :ssh_message.decode_keyboard_interactive_prompts(
      data,
      []
    )
  end

  defp keyboard_interact_get_responses(opts, name, instr, promptInfos) do
    keyboard_interact_get_responses(
      :ssh_options.get_value(:user_options, :user_interaction, opts, :ssh_auth, 586),
      :ssh_options.get_value(:user_options, :keyboard_interact_fun, opts, :ssh_auth, 587),
      :ssh_options.get_value(:user_options, :password, opts, :ssh_auth, 588),
      name,
      instr,
      promptInfos,
      opts
    )
  end

  defp keyboard_interact_get_responses(_, _, :not_ok, _, _, _, _) do
    :not_ok
  end

  defp keyboard_interact_get_responses(_, :undefined, pwd, _, _, [_], _)
       when pwd !== :undefined do
    [pwd]
  end

  defp keyboard_interact_get_responses(_, _, _, _, _, [], _) do
    []
  end

  defp keyboard_interact_get_responses(false, :undefined, :undefined, _, _, [prompt | _], opts) do
    :ssh_no_io.read_line(prompt, opts)
  end

  defp keyboard_interact_get_responses(true, :undefined, _, name, instr, promptInfos, opts) do
    prompt_user_for_passwords(name, instr, promptInfos, opts)
  end

  defp keyboard_interact_get_responses(true, fun, _Pwd, name, instr, promptInfos, _Opts) do
    keyboard_interact_fun(fun, name, instr, promptInfos)
  end

  defp prompt_user_for_passwords(name, instr, promptInfos, opts) do
    ioCb = :ssh_options.get_value(:internal_options, :io_cb, opts, :ssh_auth, 626)
    write_if_nonempty(ioCb, name)
    write_if_nonempty(ioCb, instr)

    :lists.map(
      fn
        {prompt, true} ->
          ioCb.read_line(prompt, opts)

        {prompt, false} ->
          ioCb.read_password(prompt, opts)
      end,
      promptInfos
    )
  end

  defp keyboard_interact_fun(kbdInteractFun, name, instr, promptInfos) do
    case kbdInteractFun.(name, instr, promptInfos) do
      responses
      when is_list(responses) and
             length(responses) == length(promptInfos) ->
        responses

      _ ->
        :nok
    end
  end

  defp write_if_nonempty(_, '') do
    :ok
  end

  defp write_if_nonempty(_, <<>>) do
    :ok
  end

  defp write_if_nonempty(ioCb, text) do
    ioCb.format('~s~n', [text])
  end

  def ssh_msg_userauth_result(_R) do
    :ok
  end

  def ssh_dbg_trace_points() do
    [:authentication]
  end

  def ssh_dbg_flags(:authentication) do
    [:c]
  end

  def ssh_dbg_on(:authentication) do
    :dbg.tp(:ssh_auth, :handle_userauth_request, 3, :x)
    :dbg.tp(:ssh_auth, :init_userauth_request_msg, 1, :x)
    :dbg.tp(:ssh_auth, :ssh_msg_userauth_result, 1, :x)
    :dbg.tp(:ssh_auth, :userauth_request_msg, 1, :x)
  end

  def ssh_dbg_off(:authentication) do
    :dbg.ctpg(:ssh_auth, :handle_userauth_request, 3)
    :dbg.ctpg(:ssh_auth, :init_userauth_request_msg, 1)
    :dbg.ctpg(:ssh_auth, :ssh_msg_userauth_result, 1)
    :dbg.ctpg(:ssh_auth, :userauth_request_msg, 1)
  end

  def ssh_dbg_format(
        :authentication,
        {:call, {:ssh_auth, :handle_userauth_request, [req, _SessionID, ssh]}},
        stack
      ) do
    {:skip, [{req, ssh} | stack]}
  end

  def ssh_dbg_format(
        :authentication,
        {:return_from, {:ssh_auth, :handle_userauth_request, 3},
         {:ok, {r_ssh_msg_service_accept(name: name), _Ssh}}},
        [{r_ssh_msg_service_request(name: name), _} | stack]
      ) do
    {:skip, stack}
  end

  def ssh_dbg_format(
        :authentication,
        {:return_from, {:ssh_auth, :handle_userauth_request, 3}, {:authorized, user, _Repl}},
        [{r_ssh_msg_userauth_request() = req, ssh} | stack]
      ) do
    {[
       'AUTH srvr: Peer client authorized\n',
       :io_lib.format('user = ~p~n', [user]),
       fmt_req(req, ssh)
     ], stack}
  end

  def ssh_dbg_format(
        :authentication,
        {:return_from, {:ssh_auth, :handle_userauth_request, 3},
         {:not_authorized, {user, _X}, _Repl}},
        [{r_ssh_msg_userauth_request(method: 'none'), ssh} | stack]
      ) do
    methods = r_ssh(ssh, :userauth_supported_methods)

    {[
       'AUTH srvr: Peer queries auth methods\n',
       :io_lib.format('user = ~p~nsupported methods = ~p ?', [user, methods])
     ], stack}
  end

  def ssh_dbg_format(
        :authentication,
        {:return_from, {:ssh_auth, :handle_userauth_request, 3},
         {:not_authorized, {user, _X}, repl}},
        [
          {r_ssh_msg_userauth_request(
             method: 'publickey',
             data: <<0::size(8)-unsigned-big-integer, _::binary>>
           ) = req, ssh}
          | stack
        ]
      ) do
    {case repl do
       {r_ssh_msg_userauth_pk_ok(), _} ->
         ['AUTH srvr: Answer - pub key supported\n']

       {r_ssh_msg_userauth_failure(), _} ->
         ['AUTH srvr: Answer - pub key not supported\n']

       {other, _} ->
         [
           'AUTH srvr: Answer - strange answer\n',
           :io_lib.format('strange answer = ~p~n', [other])
         ]
     end ++ [:io_lib.format('user = ~p~n', [user]), fmt_req(req, ssh)], stack}
  end

  def ssh_dbg_format(
        :authentication,
        {:return_from, {:ssh_auth, :handle_userauth_request, 3},
         {:not_authorized, {user, _X}, {r_ssh_msg_userauth_info_request(), _Ssh}}},
        [{r_ssh_msg_userauth_request(method: 'keyboard-interactive') = req, ssh} | stack]
      ) do
    {[
       'AUTH srvr: Ask peer client for password\n',
       :io_lib.format('user = ~p~n', [user]),
       fmt_req(req, ssh)
     ], stack}
  end

  def ssh_dbg_format(
        :authentication,
        {:call, {:ssh_auth, :ssh_msg_userauth_result, [:success]}},
        stack
      ) do
    {['AUTH client: Success'], stack}
  end

  def ssh_dbg_format(
        :authentication,
        {:return_from, {:ssh_auth, :ssh_msg_userauth_result, 1}, _Result},
        stack
      ) do
    {:skip, stack}
  end

  def ssh_dbg_format(
        :authentication,
        {:return_from, {:ssh_auth, :handle_userauth_request, 3},
         {:not_authorized, {user, _X}, _Repl}},
        [{r_ssh_msg_userauth_request() = req, ssh} | stack]
      ) do
    {[
       'AUTH srvr: Peer client authorization failed\n',
       :io_lib.format('user = ~p~n', [user]),
       fmt_req(req, ssh)
     ], stack}
  end

  def ssh_dbg_format(
        :authentication,
        {:call, {:ssh_auth, :init_userauth_request_msg, [r_ssh(opts: opts)]}},
        stack
      ) do
    {[
       'AUTH client: Service ssh-userauth accepted\n',
       case :ssh_options.get_value(:user_options, :user, opts, :ssh_auth, 751) do
         :undefined ->
           :io_lib.format('user = undefined *** ERROR ***', [])

         user ->
           :io_lib.format('user = ~p', [user])
       end
     ], stack}
  end

  def ssh_dbg_format(
        :authentication,
        {:return_from, {:ssh_auth, :init_userauth_request_msg, 1},
         {repl =
            r_ssh_msg_userauth_request(user: user, service: 'ssh-connection', method: 'none'),
          _Ssh}},
        stack
      ) do
    {['AUTH client: Query for accepted methods\n', :io_lib.format('user = ~p', [user])],
     [repl | stack]}
  end

  def ssh_dbg_format(
        :authentication,
        {:call, {:ssh_auth, :userauth_request_msg, [r_ssh(userauth_methods: methods)]}},
        [
          r_ssh_msg_userauth_request(user: user, service: 'ssh-connection', method: 'none')
          | stack
        ]
      ) do
    {[
       'AUTH client: Server supports\n',
       :io_lib.format('user = ~p~nmethods = ~p', [user, methods])
     ], stack}
  end

  def ssh_dbg_format(
        :authentication,
        {:call, {:ssh_auth, :userauth_request_msg, [_Ssh]}},
        stack
      ) do
    {:skip, stack}
  end

  def ssh_dbg_format(
        :authentication,
        {:return_from, {:ssh_auth, :userauth_request_msg, 1}, {:send_disconnect, _Code, _Ssh}},
        stack
      ) do
    {:skip, stack}
  end

  def ssh_dbg_format(
        :authentication,
        {:return_from, {:ssh_auth, :userauth_request_msg, 1}, {method, {_Msg, _Ssh}}},
        stack
      ) do
    {['AUTH client: Try auth with\n', :io_lib.format('method = ~p', [method])], stack}
  end

  def ssh_dbg_format(:authentication, unhandled, stack) do
    case unhandled do
      {:call, {:ssh_auth, _F, _Args}} ->
        :ok

      {:return_from, {:ssh_auth, _F, _A}, _Resp} ->
        :ok
    end

    {[
       'UNHANDLED AUTH FORMAT\n',
       :io_lib.format('Unhandled = ~p~nStack = ~p', [unhandled, stack])
     ], stack}
  end

  defp fmt_req(
         r_ssh_msg_userauth_request(
           user: user,
           service: 'ssh-connection',
           method: method,
           data: data
         ),
         r_ssh(
           kb_tries_left: kbTriesLeft,
           userauth_supported_methods: methods
         )
       ) do
    [
      :io_lib.format('req user = ~p~nreq method = ~p~nsupported methods = ~p', [
        user,
        method,
        methods
      ]),
      case method do
        'none' ->
          ''

        'password' ->
          fmt_bool(data)

        'keyboard-interactive' ->
          fmt_kb_tries_left(kbTriesLeft)

        'publickey' ->
          [
            case data do
              <<_::size(8)-unsigned-big-integer, aLen::size(32)-unsigned-big-integer,
                alg::size(aLen)-binary, _::binary>> ->
                :io_lib.format('~nkey-type = ~p', [alg])

              _ ->
                ''
            end
          ]

        _ ->
          ''
      end
    ]
  end

  defp fmt_kb_tries_left(n) when is_integer(n) do
    :io_lib.format('~ntries left = ~p', [n - 1])
  end

  defp fmt_bool(<<bool::size(8)-unsigned-big-integer, _::binary>>) do
    :io_lib.format(
      '~nBool = ~s',
      [
        case bool do
          1 ->
            'true'

          0 ->
            'false'

          _ ->
            :io_lib.format('? (~p)', [bool])
        end
      ]
    )
  end

  defp fmt_bool(<<>>) do
    ''
  end
end
