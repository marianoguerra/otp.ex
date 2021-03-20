defmodule :m_ssl do
  use Bitwise
  import Kernel, except: [send: 2]
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

  Record.defrecord(:r_srp_user, :srp_user,
    generator: :undefined,
    prime: :undefined,
    salt: :undefined,
    verifier: :undefined
  )

  def start() do
    start(:temporary)
  end

  def start(type) do
    case :application.ensure_all_started(:ssl, type) do
      {:ok, _} ->
        :ok

      other ->
        other
    end
  end

  def stop() do
    :application.stop(:ssl)
  end

  def connect(socket, sslOptions) when is_port(socket) do
    connect(socket, sslOptions, :infinity)
  end

  def connect(socket, sslOptions0, timeout)
      when (is_port(socket) and
              is_integer(timeout) and timeout >= 0) or timeout == :infinity do
    cbInfo = handle_option_cb_info(sslOptions0, :tls)
    transport = :erlang.element(1, cbInfo)
    emulatedOptions = :tls_socket.emulated_options()
    {:ok, socketValues} = :tls_socket.getopts(transport, socket, emulatedOptions)

    try do
      handle_options(sslOptions0 ++ socketValues, :client)
    catch
      _, {:error, reason} ->
        {:error, reason}
    else
      {:ok, config} ->
        :tls_socket.upgrade(socket, config, timeout)
    end
  end

  def connect(host, port, options) do
    connect(host, port, options, :infinity)
  end

  def connect(host, port, options, timeout)
      when (is_integer(timeout) and timeout >= 0) or timeout == :infinity do
    try do
      {:ok, config} = handle_options(options, :client, host)

      case r_config(config, :connection_cb) do
        :tls_connection ->
          :tls_socket.connect(host, port, config, timeout)

        :dtls_connection ->
          :dtls_socket.connect(host, port, config, timeout)
      end
    catch
      error ->
        error
    end
  end

  def listen(_Port, []) do
    {:error, :nooptions}
  end

  def listen(port, options0) do
    try do
      {:ok, config} = handle_options(options0, :server)
      do_listen(port, config, r_config(config, :connection_cb))
    catch
      error = {:error, _} ->
        error
    end
  end

  def transport_accept(listenSocket) do
    transport_accept(listenSocket, :infinity)
  end

  def transport_accept(
        r_sslsocket(pid: {listenSocket, r_config(connection_cb: connectionCb) = config}),
        timeout
      )
      when (is_integer(timeout) and timeout >= 0) or timeout == :infinity do
    case connectionCb do
      :tls_connection ->
        :tls_socket.accept(listenSocket, config, timeout)

      :dtls_connection ->
        :dtls_socket.accept(listenSocket, config, timeout)
    end
  end

  def ssl_accept(listenSocket) do
    ssl_accept(listenSocket, [], :infinity)
  end

  def ssl_accept(socket, timeout)
      when (is_integer(timeout) and timeout >= 0) or timeout == :infinity do
    ssl_accept(socket, [], timeout)
  end

  def ssl_accept(listenSocket, sslOptions)
      when is_port(listenSocket) do
    ssl_accept(listenSocket, sslOptions, :infinity)
  end

  def ssl_accept(socket, timeout) do
    ssl_accept(socket, [], timeout)
  end

  def ssl_accept(socket, sslOptions, timeout)
      when is_port(socket) do
    handshake(socket, sslOptions, timeout)
  end

  def ssl_accept(socket, sslOptions, timeout) do
    case handshake(socket, sslOptions, timeout) do
      {:ok, _} ->
        :ok

      error ->
        error
    end
  end

  def handshake(listenSocket) do
    handshake(listenSocket, :infinity)
  end

  def handshake(r_sslsocket() = socket, timeout)
      when (is_integer(timeout) and timeout >= 0) or timeout == :infinity do
    :ssl_connection.handshake(socket, timeout)
  end

  def handshake(listenSocket, sslOptions)
      when is_port(listenSocket) do
    handshake(listenSocket, sslOptions, :infinity)
  end

  def handshake(r_sslsocket() = socket, [], timeout)
      when (is_integer(timeout) and timeout >= 0) or timeout == :infinity do
    handshake(socket, timeout)
  end

  def handshake(r_sslsocket(fd: {_, _, _, trackers}) = socket, sslOpts, timeout)
      when (is_integer(timeout) and timeout >= 0) or timeout == :infinity do
    try do
      tracker =
        :proplists.get_value(
          :option_tracker,
          trackers
        )

      {:ok, emOpts, _} = :tls_socket.get_all_opts(tracker)

      :ssl_connection.handshake(
        socket,
        {sslOpts,
         :tls_socket.emulated_socket_options(
           emOpts,
           r_socket_options()
         )},
        timeout
      )
    catch
      error = {:error, _Reason} ->
        error
    end
  end

  def handshake(r_sslsocket(pid: [pid | _], fd: {_, _, _}) = socket, sslOpts, timeout)
      when (is_integer(timeout) and timeout >= 0) or timeout == :infinity do
    try do
      {:ok, emOpts, _} = :dtls_packet_demux.get_all_opts(pid)

      :ssl_connection.handshake(
        socket,
        {sslOpts,
         :tls_socket.emulated_socket_options(
           emOpts,
           r_socket_options()
         )},
        timeout
      )
    catch
      error = {:error, _Reason} ->
        error
    end
  end

  def handshake(socket, sslOptions, timeout)
      when (is_port(socket) and
              is_integer(timeout) and timeout >= 0) or timeout == :infinity do
    cbInfo = handle_option_cb_info(sslOptions, :tls)
    transport = :erlang.element(1, cbInfo)
    emulatedOptions = :tls_socket.emulated_options()
    {:ok, socketValues} = :tls_socket.getopts(transport, socket, emulatedOptions)
    connetionCb = connection_cb(sslOptions)

    try do
      handle_options(sslOptions ++ socketValues, :server)
    catch
      error = {:error, _Reason} ->
        error
    else
      {:ok, r_config(transport_info: ^cbInfo, ssl: sslOpts, emulated: emOpts)} ->
        :ok = :tls_socket.setopts(transport, socket, :tls_socket.internal_inet_values())
        {:ok, port} = :tls_socket.port(transport, socket)
        {:ok, sessionIdHandle} = :tls_socket.session_id_tracker(sslOpts)

        :ssl_connection.handshake(
          connetionCb,
          port,
          socket,
          {sslOpts,
           :tls_socket.emulated_socket_options(
             emOpts,
             r_socket_options()
           ), [{:session_id_tracker, sessionIdHandle}]},
          self(),
          cbInfo,
          timeout
        )
    end
  end

  def handshake_continue(socket, sSLOptions) do
    handshake_continue(socket, sSLOptions, :infinity)
  end

  def handshake_continue(socket, sSLOptions, timeout) do
    :ssl_connection.handshake_continue(socket, sSLOptions, timeout)
  end

  def handshake_cancel(socket) do
    :ssl_connection.handshake_cancel(socket)
  end

  def close(r_sslsocket(pid: [pid | _])) when is_pid(pid) do
    :ssl_connection.close(pid, {:close, 5000})
  end

  def close(r_sslsocket(pid: {:dtls, r_config(dtls_handler: {_, _})}) = dTLSListen) do
    :dtls_socket.close(dTLSListen)
  end

  def close(r_sslsocket(pid: {listenSocket, r_config(transport_info: {transport, _, _, _, _})})) do
    transport.close(listenSocket)
  end

  def close(r_sslsocket(pid: [tLSPid | _]), {pid, timeout} = downGrade)
      when (is_pid(tLSPid) and is_pid(pid) and
              is_integer(timeout) and timeout >= 0) or timeout == :infinity do
    case :ssl_connection.close(
           tLSPid,
           {:close, downGrade}
         ) do
      :ok ->
        {:error, :closed}

      other ->
        other
    end
  end

  def close(r_sslsocket(pid: [tLSPid | _]), timeout)
      when (is_pid(tLSPid) and
              is_integer(timeout) and timeout >= 0) or timeout == :infinity do
    :ssl_connection.close(tLSPid, {:close, timeout})
  end

  def close(
        r_sslsocket(
          pid: {:dtls = listenSocket, r_config(transport_info: {transport, _, _, _, _})}
        ),
        _
      ) do
    :dtls_socket.close(transport, listenSocket)
  end

  def close(
        r_sslsocket(pid: {listenSocket, r_config(transport_info: {transport, _, _, _, _})}),
        _
      ) do
    :tls_socket.close(transport, listenSocket)
  end

  def send(r_sslsocket(pid: [pid]), data) when is_pid(pid) do
    :ssl_connection.send(pid, data)
  end

  def send(r_sslsocket(pid: [_, pid]), data) when is_pid(pid) do
    :tls_sender.send_data(
      pid,
      :erlang.iolist_to_iovec(data)
    )
  end

  def send(
        r_sslsocket(pid: {_, r_config(transport_info: {_, :udp, _, _})}),
        _
      ) do
    {:error, :enotconn}
  end

  def send(r_sslsocket(pid: {:dtls, _}), _) do
    {:error, :enotconn}
  end

  def send(
        r_sslsocket(pid: {listenSocket, r_config(transport_info: info)}),
        data
      ) do
    transport = :erlang.element(1, info)
    transport.send(listenSocket, data)
  end

  def recv(socket, length) do
    recv(socket, length, :infinity)
  end

  def recv(r_sslsocket(pid: [pid | _]), length, timeout)
      when (is_pid(pid) and
              is_integer(timeout) and timeout >= 0) or timeout == :infinity do
    :ssl_connection.recv(pid, length, timeout)
  end

  def recv(r_sslsocket(pid: {:dtls, _}), _, _) do
    {:error, :enotconn}
  end

  def recv(r_sslsocket(pid: {listen, r_config(transport_info: info)}), _, _)
      when is_port(listen) do
    transport = :erlang.element(1, info)
    transport.recv(listen, 0)
  end

  def controlling_process(r_sslsocket(pid: [pid | _]), newOwner)
      when is_pid(pid) and is_pid(newOwner) do
    :ssl_connection.new_user(pid, newOwner)
  end

  def controlling_process(r_sslsocket(pid: {:dtls, _}), newOwner)
      when is_pid(newOwner) do
    :ok
  end

  def controlling_process(
        r_sslsocket(pid: {listen, r_config(transport_info: {transport, _, _, _, _})}),
        newOwner
      )
      when is_port(listen) and is_pid(newOwner) do
    transport.controlling_process(listen, newOwner)
  end

  def connection_information(r_sslsocket(pid: [pid | _])) when is_pid(pid) do
    case :ssl_connection.connection_information(
           pid,
           false
         ) do
      {:ok, info} ->
        {:ok,
         for item = {_Key, value} <- info,
             value !== :undefined do
           item
         end}

      error ->
        error
    end
  end

  def connection_information(r_sslsocket(pid: {listen, _})) when is_port(listen) do
    {:error, :enotconn}
  end

  def connection_information(r_sslsocket(pid: {:dtls, _})) do
    {:error, :enotconn}
  end

  def connection_information(r_sslsocket(pid: [pid | _]), items) when is_pid(pid) do
    case :ssl_connection.connection_information(
           pid,
           include_security_info(items)
         ) do
      {:ok, info} ->
        {:ok,
         for item = {key, value} <- info, :lists.member(key, items), value !== :undefined do
           item
         end}

      error ->
        error
    end
  end

  def peername(r_sslsocket(pid: [pid | _], fd: {transport, socket, _}))
      when is_pid(pid) do
    :dtls_socket.peername(transport, socket)
  end

  def peername(r_sslsocket(pid: [pid | _], fd: {transport, socket, _, _}))
      when is_pid(pid) do
    :tls_socket.peername(transport, socket)
  end

  def peername(r_sslsocket(pid: {:dtls, r_config(dtls_handler: {_Pid, _})})) do
    :dtls_socket.peername(:dtls, :undefined)
  end

  def peername(
        r_sslsocket(pid: {listenSocket, r_config(transport_info: {transport, _, _, _, _})})
      ) do
    :tls_socket.peername(transport, listenSocket)
  end

  def peername(r_sslsocket(pid: {:dtls, _})) do
    {:error, :enotconn}
  end

  def peercert(r_sslsocket(pid: [pid | _])) when is_pid(pid) do
    case :ssl_connection.peer_certificate(pid) do
      {:ok, :undefined} ->
        {:error, :no_peercert}

      result ->
        result
    end
  end

  def peercert(r_sslsocket(pid: {:dtls, _})) do
    {:error, :enotconn}
  end

  def peercert(r_sslsocket(pid: {listen, _})) when is_port(listen) do
    {:error, :enotconn}
  end

  def negotiated_protocol(r_sslsocket(pid: [pid | _])) when is_pid(pid) do
    :ssl_connection.negotiated_protocol(pid)
  end

  def cipher_suites() do
    cipher_suites(:erlang)
  end

  def cipher_suites(:erlang) do
    for suite <- available_suites(:default) do
      :ssl_cipher_format.suite_legacy(suite)
    end
  end

  def cipher_suites(:openssl) do
    for suite <- available_suites(:default) do
      :ssl_cipher_format.suite_map_to_openssl_str(:ssl_cipher_format.suite_bin_to_map(suite))
    end
  end

  def cipher_suites(:all) do
    for suite <- available_suites(:all) do
      :ssl_cipher_format.suite_legacy(suite)
    end
  end

  def cipher_suites(description, version)
      when version == :"tlsv1.3" or
             version == :"tlsv1.2" or version == :"tlsv1.1" or
             version == :tlsv1 do
    cipher_suites(
      description,
      :tls_record.protocol_version(version)
    )
  end

  def cipher_suites(description, version)
      when version == :"dtlsv1.2" or
             version == :dtlsv1 do
    cipher_suites(
      description,
      :dtls_record.protocol_version(version)
    )
  end

  def cipher_suites(description, version) do
    for suite <- supported_suites(description, version) do
      :ssl_cipher_format.suite_bin_to_map(suite)
    end
  end

  def cipher_suites(description, version, stringType)
      when version == :"tlsv1.3" or version == :"tlsv1.2" or version == :"tlsv1.1" or
             version == :tlsv1 do
    cipher_suites(description, :tls_record.protocol_version(version), stringType)
  end

  def cipher_suites(description, version, stringType)
      when version == :"dtlsv1.2" or version == :dtlsv1 do
    cipher_suites(description, :dtls_record.protocol_version(version), stringType)
  end

  def cipher_suites(description, version, :rfc) do
    for suite <- supported_suites(description, version) do
      :ssl_cipher_format.suite_map_to_str(:ssl_cipher_format.suite_bin_to_map(suite))
    end
  end

  def cipher_suites(description, version, :openssl) do
    for suite <- supported_suites(description, version) do
      :ssl_cipher_format.suite_map_to_openssl_str(:ssl_cipher_format.suite_bin_to_map(suite))
    end
  end

  def filter_cipher_suites(suites, filters0) do
    %{key_exchange_filters: kexF, cipher_filters: cipherF, mac_filters: macF, prf_filters: prfF} =
      :ssl_cipher.crypto_support_filters()

    filters = %{
      key_exchange_filters:
        add_filter(
          :proplists.get_value(
            :key_exchange,
            filters0
          ),
          kexF
        ),
      cipher_filters:
        add_filter(
          :proplists.get_value(:cipher, filters0),
          cipherF
        ),
      mac_filters: add_filter(:proplists.get_value(:mac, filters0), macF),
      prf_filters: add_filter(:proplists.get_value(:prf, filters0), prfF)
    }

    :ssl_cipher.filter_suites(suites, filters)
  end

  def prepend_cipher_suites([first | _] = preferred, suites0)
      when is_map(first) do
    suites = preferred ++ (suites0 -- preferred)
    suites
  end

  def prepend_cipher_suites(filters, suites) do
    preferred = filter_cipher_suites(suites, filters)
    preferred ++ (suites -- preferred)
  end

  def append_cipher_suites([first | _] = deferred, suites0)
      when is_map(first) do
    suites = (suites0 -- deferred) ++ deferred
    suites
  end

  def append_cipher_suites(filters, suites) do
    deferred = filter_cipher_suites(suites, filters)
    (suites -- deferred) ++ deferred
  end

  def eccs() do
    curves = :tls_v1.ecc_curves(:all)
    eccs_filter_supported(curves)
  end

  def eccs(:dtlsv1) do
    eccs(:"tlsv1.1")
  end

  def eccs(:"dtlsv1.2") do
    eccs(:"tlsv1.2")
  end

  def eccs(version)
      when version == :"tlsv1.2" or version == :"tlsv1.1" or
             version == :tlsv1 do
    curves = :tls_v1.ecc_curves(:all)
    eccs_filter_supported(curves)
  end

  defp eccs_filter_supported(curves) do
    cryptoCurves = :crypto.ec_curves()

    :lists.filter(
      fn curve ->
        :proplists.get_bool(curve, cryptoCurves)
      end,
      curves
    )
  end

  def groups() do
    :tls_v1.groups(4)
  end

  def groups(:default) do
    :tls_v1.default_groups(4)
  end

  def getopts(r_sslsocket(pid: [pid | _]), optionTags)
      when is_pid(pid) and is_list(optionTags) do
    :ssl_connection.get_opts(pid, optionTags)
  end

  def getopts(
        r_sslsocket(pid: {:dtls, r_config(transport_info: {transport, _, _, _, _})}) =
          listenSocket,
        optionTags
      )
      when is_list(optionTags) do
    try do
      :dtls_socket.getopts(transport, listenSocket, optionTags)
    catch
      _, error ->
        {:error, {:options, {:socket_options, optionTags, error}}}
    else
      {:ok, _} = result ->
        result

      {:error, inetError} ->
        {:error, {:options, {:socket_options, optionTags, inetError}}}
    end
  end

  def getopts(
        r_sslsocket(pid: {_, r_config(transport_info: {transport, _, _, _, _})}) = listenSocket,
        optionTags
      )
      when is_list(optionTags) do
    try do
      :tls_socket.getopts(transport, listenSocket, optionTags)
    catch
      _, error ->
        {:error, {:options, {:socket_options, optionTags, error}}}
    else
      {:ok, _} = result ->
        result

      {:error, inetError} ->
        {:error, {:options, {:socket_options, optionTags, inetError}}}
    end
  end

  def getopts(r_sslsocket(), optionTags) do
    {:error, {:options, {:socket_options, optionTags}}}
  end

  def setopts(r_sslsocket(pid: [pid, sender]), options0)
      when is_pid(pid) and is_list(options0) do
    try do
      :proplists.expand(
        [{:binary, [{:mode, :binary}]}, {:list, [{:mode, :list}]}],
        options0
      )
    catch
      _, _ ->
        {:error, {:options, {:not_a_proplist, options0}}}
    else
      options ->
        case :proplists.get_value(:packet, options, :undefined) do
          :undefined ->
            :ssl_connection.set_opts(pid, options)

          packetOpt ->
            case :tls_sender.setopts(
                   sender,
                   [{:packet, packetOpt}]
                 ) do
              :ok ->
                :ssl_connection.set_opts(pid, options)

              error ->
                error
            end
        end
    end
  end

  def setopts(r_sslsocket(pid: [pid | _]), options0)
      when is_pid(pid) and is_list(options0) do
    try do
      :proplists.expand(
        [{:binary, [{:mode, :binary}]}, {:list, [{:mode, :list}]}],
        options0
      )
    catch
      _, _ ->
        {:error, {:options, {:not_a_proplist, options0}}}
    else
      options ->
        :ssl_connection.set_opts(pid, options)
    end
  end

  def setopts(
        r_sslsocket(pid: {:dtls, r_config(transport_info: {transport, _, _, _, _})}) =
          listenSocket,
        options
      )
      when is_list(options) do
    try do
      :dtls_socket.setopts(transport, listenSocket, options)
    catch
      _, error ->
        {:error, {:options, {:socket_options, options, error}}}
    else
      :ok ->
        :ok

      {:error, inetError} ->
        {:error, {:options, {:socket_options, options, inetError}}}
    end
  end

  def setopts(
        r_sslsocket(pid: {_, r_config(transport_info: {transport, _, _, _, _})}) = listenSocket,
        options
      )
      when is_list(options) do
    try do
      :tls_socket.setopts(transport, listenSocket, options)
    catch
      _, error ->
        {:error, {:options, {:socket_options, options, error}}}
    else
      :ok ->
        :ok

      {:error, inetError} ->
        {:error, {:options, {:socket_options, options, inetError}}}
    end
  end

  def setopts(r_sslsocket(), options) do
    {:error, {:options, {:not_a_proplist, options}}}
  end

  def getstat(socket) do
    getstat(socket, :inet.stats())
  end

  def getstat(
        r_sslsocket(
          pid:
            {:dtls,
             r_config(
               transport_info: {transport, _, _, _, _},
               dtls_handler: {listner, _}
             )}
        ),
        options
      )
      when is_list(options) do
    :dtls_socket.getstat(transport, listner, options)
  end

  def getstat(
        r_sslsocket(pid: {listen, r_config(transport_info: {transport, _, _, _, _})}),
        options
      )
      when is_port(listen) and is_list(options) do
    :tls_socket.getstat(transport, listen, options)
  end

  def getstat(
        r_sslsocket(pid: [pid | _], fd: {transport, socket, _, _}),
        options
      )
      when is_pid(pid) and is_list(options) do
    :tls_socket.getstat(transport, socket, options)
  end

  def getstat(
        r_sslsocket(pid: [pid | _], fd: {transport, socket, _}),
        options
      )
      when is_pid(pid) and is_list(options) do
    :dtls_socket.getstat(transport, socket, options)
  end

  def shutdown(r_sslsocket(pid: {listen, r_config(transport_info: info)}), how)
      when is_port(listen) do
    transport = :erlang.element(1, info)
    transport.shutdown(listen, how)
  end

  def shutdown(r_sslsocket(pid: {:dtls, _}), _) do
    {:error, :enotconn}
  end

  def shutdown(r_sslsocket(pid: [pid | _]), how) when is_pid(pid) do
    :ssl_connection.shutdown(pid, how)
  end

  def sockname(r_sslsocket(pid: {listen, r_config(transport_info: {transport, _, _, _, _})}))
      when is_port(listen) do
    :tls_socket.sockname(transport, listen)
  end

  def sockname(r_sslsocket(pid: {:dtls, r_config(dtls_handler: {pid, _})})) do
    :dtls_packet_demux.sockname(pid)
  end

  def sockname(r_sslsocket(pid: [pid | _], fd: {transport, socket, _}))
      when is_pid(pid) do
    :dtls_socket.sockname(transport, socket)
  end

  def sockname(r_sslsocket(pid: [pid | _], fd: {transport, socket, _, _}))
      when is_pid(pid) do
    :tls_socket.sockname(transport, socket)
  end

  def versions() do
    confTLSVsns = :tls_record.supported_protocol_versions()
    confDTLSVsns = :dtls_record.supported_protocol_versions()
    implementedTLSVsns = [:"tlsv1.3", :"tlsv1.2", :"tlsv1.1", :tlsv1]
    implementedDTLSVsns = [:"dtlsv1.2", :dtlsv1]

    tLSCryptoSupported = fn vsn ->
      :tls_record.sufficient_crypto_support(vsn)
    end

    dTLSCryptoSupported = fn vsn ->
      :tls_record.sufficient_crypto_support(:dtls_v1.corresponding_tls_version(vsn))
    end

    supportedTLSVsns =
      for vsn <- confTLSVsns,
          tLSCryptoSupported.(vsn) do
        :tls_record.protocol_version(vsn)
      end

    supportedDTLSVsns =
      for vsn <- confDTLSVsns,
          dTLSCryptoSupported.(vsn) do
        :dtls_record.protocol_version(vsn)
      end

    availableTLSVsns =
      for vsn <- implementedTLSVsns,
          tLSCryptoSupported.(:tls_record.protocol_version(vsn)) do
        vsn
      end

    availableDTLSVsns =
      for vsn <- implementedDTLSVsns,
          dTLSCryptoSupported.(:dtls_record.protocol_version(vsn)) do
        vsn
      end

    [
      {:ssl_app, :EFE_TODO_VSN_MACRO},
      {:supported, supportedTLSVsns},
      {:supported_dtls, supportedDTLSVsns},
      {:available, availableTLSVsns},
      {:available_dtls, availableDTLSVsns},
      {:implemented, implementedTLSVsns},
      {:implemented_dtls, implementedDTLSVsns}
    ]
  end

  def renegotiate(r_sslsocket(pid: [pid, sender | _]))
      when is_pid(pid) and
             is_pid(sender) do
    case :tls_sender.renegotiate(sender) do
      {:ok, write} ->
        :tls_connection.renegotiation(pid, write)

      error ->
        error
    end
  end

  def renegotiate(r_sslsocket(pid: [pid | _])) when is_pid(pid) do
    :ssl_connection.renegotiation(pid)
  end

  def renegotiate(r_sslsocket(pid: {:dtls, _})) do
    {:error, :enotconn}
  end

  def renegotiate(r_sslsocket(pid: {listen, _})) when is_port(listen) do
    {:error, :enotconn}
  end

  def update_keys(r_sslsocket(pid: [pid, sender | _]), type0)
      when is_pid(pid) and is_pid(sender) and (type0 === :write or type0 === :read_write) do
    type =
      case type0 do
        :write ->
          :update_not_requested

        :read_write ->
          :update_requested
      end

    :tls_connection.send_key_update(sender, type)
  end

  def update_keys(_, type) do
    {:error, {:illegal_parameter, type}}
  end

  def prf(r_sslsocket(pid: [pid | _]), secret, label, seed, wantedLength)
      when is_pid(pid) do
    :ssl_connection.prf(pid, secret, label, seed, wantedLength)
  end

  def prf(r_sslsocket(pid: {:dtls, _}), _, _, _, _) do
    {:error, :enotconn}
  end

  def prf(r_sslsocket(pid: {listen, _}), _, _, _, _)
      when is_port(listen) do
    {:error, :enotconn}
  end

  def clear_pem_cache() do
    :ssl_pem_cache.clear()
  end

  def format_error({:error, reason}) do
    format_error(reason)
  end

  def format_error(reason) when is_list(reason) do
    reason
  end

  def format_error(:closed) do
    'TLS connection is closed'
  end

  def format_error({:tls_alert, {_, description}}) do
    description
  end

  def format_error({:options, {fileType, file, reason}})
      when fileType == :cacertfile or fileType == :certfile or
             fileType == :keyfile or fileType == :dhfile do
    error = file_error_format(reason)
    file_desc(fileType) ++ file ++ ': ' ++ error
  end

  def format_error({:options, {:socket_options, option, error}}) do
    :lists.flatten(
      :io_lib.format(
        'Invalid transport socket option ~p: ~s',
        [option, format_error(error)]
      )
    )
  end

  def format_error({:options, {:socket_options, option}}) do
    :lists.flatten(:io_lib.format('Invalid socket option: ~p', [option]))
  end

  def format_error({:options, options}) do
    :lists.flatten(:io_lib.format('Invalid TLS option: ~p', [options]))
  end

  def format_error(error) do
    case :inet.format_error(error) do
      'unknown POSIX' ++ _ ->
        unexpected_format(error)

      other ->
        other
    end
  end

  def tls_version({3, _} = version) do
    version
  end

  def tls_version({254, _} = version) do
    :dtls_v1.corresponding_tls_version(version)
  end

  def suite_to_str(cipher) do
    :ssl_cipher_format.suite_map_to_str(cipher)
  end

  def suite_to_openssl_str(cipher) do
    :ssl_cipher_format.suite_map_to_openssl_str(cipher)
  end

  def str_to_suite(cipherSuiteName) do
    try do
      :ssl_cipher_format.suite_openssl_str_to_map(cipherSuiteName)
    catch
      _, _ ->
        {:error, {:not_recognized, cipherSuiteName}}
    end
  end

  defp available_suites(:default) do
    version = :tls_record.highest_protocol_version([])
    :ssl_cipher.filter_suites(:ssl_cipher.suites(version))
  end

  defp available_suites(:all) do
    version = :tls_record.highest_protocol_version([])
    :ssl_cipher.filter_suites(:ssl_cipher.all_suites(version))
  end

  defp supported_suites(:exclusive, {3, minor}) do
    :tls_v1.exclusive_suites(minor)
  end

  defp supported_suites(:default, version) do
    :ssl_cipher.suites(version)
  end

  defp supported_suites(:all, version) do
    :ssl_cipher.all_suites(version)
  end

  defp supported_suites(:anonymous, version) do
    :ssl_cipher.anonymous_suites(version)
  end

  defp do_listen(
         port,
         r_config(transport_info: {transport, _, _, _, _}) = config,
         :tls_connection
       ) do
    :tls_socket.listen(transport, port, config)
  end

  defp do_listen(port, config, :dtls_connection) do
    :dtls_socket.listen(port, config)
  end

  def handle_options(opts, role) do
    handle_options(opts, role, :undefined)
  end

  def handle_options(opts0, role, inheritedSslOpts)
      when is_map(inheritedSslOpts) do
    {sslOpts, _} =
      expand_options(
        opts0,
        %{
          alpn_advertised_protocols: {:undefined, [:versions]},
          alpn_preferred_protocols: {:undefined, [:versions]},
          anti_replay: {:undefined, [:versions, :session_tickets]},
          beast_mitigation: {:one_n_minus_one, [:versions]},
          cacertfile: {:undefined, [:versions, :verify_fun, :cacerts]},
          cacerts: {:undefined, [:versions]},
          cert: {:undefined, [:versions]},
          certfile: {<<>>, [:versions]},
          ciphers: {[], [:versions]},
          client_renegotiation: {:undefined, [:versions]},
          cookie: {true, [:versions]},
          crl_cache: {{:ssl_crl_cache, {:internal, []}}, [:versions]},
          crl_check: {false, [:versions]},
          customize_hostname_check: {[], [:versions]},
          depth: {1, [:versions]},
          dh: {:undefined, [:versions]},
          dhfile: {:undefined, [:versions]},
          eccs: {:undefined, [:versions]},
          erl_dist: {false, [:versions]},
          fail_if_no_peer_cert: {false, [:versions]},
          fallback: {false, [:versions]},
          handshake: {:full, [:versions]},
          hibernate_after: {:infinity, [:versions]},
          honor_cipher_order: {false, [:versions]},
          honor_ecc_order: {:undefined, [:versions]},
          key: {:undefined, [:versions]},
          keyfile: {:undefined, [:versions, :certfile]},
          key_update_at: {388_736_063_997, [:versions]},
          log_level: {:notice, [:versions]},
          max_handshake_size: {256 * 1024, [:versions]},
          middlebox_comp_mode: {true, [:versions]},
          max_fragment_length: {:undefined, [:versions]},
          next_protocol_selector: {:undefined, [:versions]},
          next_protocols_advertised: {:undefined, [:versions]},
          ocsp_stapling: {false, [:versions]},
          ocsp_responder_certs: {[], [:versions, :ocsp_stapling]},
          ocsp_nonce: {true, [:versions, :ocsp_stapling]},
          padding_check: {true, [:versions]},
          partial_chain:
            {fn _ ->
               :unknown_ca
             end, [:versions]},
          password: {'', [:versions]},
          protocol: {:tls, []},
          psk_identity: {:undefined, [:versions]},
          renegotiate_at: {268_435_456, [:versions]},
          reuse_session: {:undefined, [:versions]},
          reuse_sessions: {true, [:versions]},
          secure_renegotiate: {true, [:versions]},
          server_name_indication: {:undefined, [:versions]},
          session_tickets: {:disabled, [:versions]},
          signature_algs: {:undefined, [:versions]},
          signature_algs_cert: {:undefined, [:versions]},
          sni_fun: {:undefined, [:versions, :sni_hosts]},
          sni_hosts: {[], [:versions]},
          srp_identity: {:undefined, [:versions]},
          supported_groups: {:undefined, [:versions]},
          use_ticket: {:undefined, [:versions]},
          user_lookup_fun: {:undefined, [:versions]},
          verify: {:verify_none, [:versions, :fail_if_no_peer_cert, :partial_chain]},
          verify_fun:
            {{fn
                _, {:bad_cert, _}, userState ->
                  {:valid, userState}

                _, {:extension, r_Extension(critical: true)}, userState ->
                  {:valid, userState}

                _, {:extension, _}, userState ->
                  {:unknown, userState}

                _, :valid, userState ->
                  {:valid, userState}

                _, :valid_peer, userState ->
                  {:valid, userState}
              end, []}, [:versions, :verify]},
          versions: {[], [:protocol]}
        }
      )

    process_options(sslOpts, inheritedSslOpts, %{
      role: role,
      rules: %{
        alpn_advertised_protocols: {:undefined, [:versions]},
        alpn_preferred_protocols: {:undefined, [:versions]},
        anti_replay: {:undefined, [:versions, :session_tickets]},
        beast_mitigation: {:one_n_minus_one, [:versions]},
        cacertfile: {:undefined, [:versions, :verify_fun, :cacerts]},
        cacerts: {:undefined, [:versions]},
        cert: {:undefined, [:versions]},
        certfile: {<<>>, [:versions]},
        ciphers: {[], [:versions]},
        client_renegotiation: {:undefined, [:versions]},
        cookie: {true, [:versions]},
        crl_cache: {{:ssl_crl_cache, {:internal, []}}, [:versions]},
        crl_check: {false, [:versions]},
        customize_hostname_check: {[], [:versions]},
        depth: {1, [:versions]},
        dh: {:undefined, [:versions]},
        dhfile: {:undefined, [:versions]},
        eccs: {:undefined, [:versions]},
        erl_dist: {false, [:versions]},
        fail_if_no_peer_cert: {false, [:versions]},
        fallback: {false, [:versions]},
        handshake: {:full, [:versions]},
        hibernate_after: {:infinity, [:versions]},
        honor_cipher_order: {false, [:versions]},
        honor_ecc_order: {:undefined, [:versions]},
        key: {:undefined, [:versions]},
        keyfile: {:undefined, [:versions, :certfile]},
        key_update_at: {388_736_063_997, [:versions]},
        log_level: {:notice, [:versions]},
        max_handshake_size: {256 * 1024, [:versions]},
        middlebox_comp_mode: {true, [:versions]},
        max_fragment_length: {:undefined, [:versions]},
        next_protocol_selector: {:undefined, [:versions]},
        next_protocols_advertised: {:undefined, [:versions]},
        ocsp_stapling: {false, [:versions]},
        ocsp_responder_certs: {[], [:versions, :ocsp_stapling]},
        ocsp_nonce: {true, [:versions, :ocsp_stapling]},
        padding_check: {true, [:versions]},
        partial_chain:
          {fn _ ->
             :unknown_ca
           end, [:versions]},
        password: {'', [:versions]},
        protocol: {:tls, []},
        psk_identity: {:undefined, [:versions]},
        renegotiate_at: {268_435_456, [:versions]},
        reuse_session: {:undefined, [:versions]},
        reuse_sessions: {true, [:versions]},
        secure_renegotiate: {true, [:versions]},
        server_name_indication: {:undefined, [:versions]},
        session_tickets: {:disabled, [:versions]},
        signature_algs: {:undefined, [:versions]},
        signature_algs_cert: {:undefined, [:versions]},
        sni_fun: {:undefined, [:versions, :sni_hosts]},
        sni_hosts: {[], [:versions]},
        srp_identity: {:undefined, [:versions]},
        supported_groups: {:undefined, [:versions]},
        use_ticket: {:undefined, [:versions]},
        user_lookup_fun: {:undefined, [:versions]},
        verify: {:verify_none, [:versions, :fail_if_no_peer_cert, :partial_chain]},
        verify_fun:
          {{fn
              _, {:bad_cert, _}, userState ->
                {:valid, userState}

              _, {:extension, r_Extension(critical: true)}, userState ->
                {:valid, userState}

              _, {:extension, _}, userState ->
                {:unknown, userState}

              _, :valid, userState ->
                {:valid, userState}

              _, :valid_peer, userState ->
                {:valid, userState}
            end, []}, [:versions, :verify]},
        versions: {[], [:protocol]}
      }
    })
  end

  def handle_options(opts0, role, host) do
    {sslOpts0, sockOpts} =
      expand_options(
        opts0,
        %{
          alpn_advertised_protocols: {:undefined, [:versions]},
          alpn_preferred_protocols: {:undefined, [:versions]},
          anti_replay: {:undefined, [:versions, :session_tickets]},
          beast_mitigation: {:one_n_minus_one, [:versions]},
          cacertfile: {:undefined, [:versions, :verify_fun, :cacerts]},
          cacerts: {:undefined, [:versions]},
          cert: {:undefined, [:versions]},
          certfile: {<<>>, [:versions]},
          ciphers: {[], [:versions]},
          client_renegotiation: {:undefined, [:versions]},
          cookie: {true, [:versions]},
          crl_cache: {{:ssl_crl_cache, {:internal, []}}, [:versions]},
          crl_check: {false, [:versions]},
          customize_hostname_check: {[], [:versions]},
          depth: {1, [:versions]},
          dh: {:undefined, [:versions]},
          dhfile: {:undefined, [:versions]},
          eccs: {:undefined, [:versions]},
          erl_dist: {false, [:versions]},
          fail_if_no_peer_cert: {false, [:versions]},
          fallback: {false, [:versions]},
          handshake: {:full, [:versions]},
          hibernate_after: {:infinity, [:versions]},
          honor_cipher_order: {false, [:versions]},
          honor_ecc_order: {:undefined, [:versions]},
          key: {:undefined, [:versions]},
          keyfile: {:undefined, [:versions, :certfile]},
          key_update_at: {388_736_063_997, [:versions]},
          log_level: {:notice, [:versions]},
          max_handshake_size: {256 * 1024, [:versions]},
          middlebox_comp_mode: {true, [:versions]},
          max_fragment_length: {:undefined, [:versions]},
          next_protocol_selector: {:undefined, [:versions]},
          next_protocols_advertised: {:undefined, [:versions]},
          ocsp_stapling: {false, [:versions]},
          ocsp_responder_certs: {[], [:versions, :ocsp_stapling]},
          ocsp_nonce: {true, [:versions, :ocsp_stapling]},
          padding_check: {true, [:versions]},
          partial_chain:
            {fn _ ->
               :unknown_ca
             end, [:versions]},
          password: {'', [:versions]},
          protocol: {:tls, []},
          psk_identity: {:undefined, [:versions]},
          renegotiate_at: {268_435_456, [:versions]},
          reuse_session: {:undefined, [:versions]},
          reuse_sessions: {true, [:versions]},
          secure_renegotiate: {true, [:versions]},
          server_name_indication: {:undefined, [:versions]},
          session_tickets: {:disabled, [:versions]},
          signature_algs: {:undefined, [:versions]},
          signature_algs_cert: {:undefined, [:versions]},
          sni_fun: {:undefined, [:versions, :sni_hosts]},
          sni_hosts: {[], [:versions]},
          srp_identity: {:undefined, [:versions]},
          supported_groups: {:undefined, [:versions]},
          use_ticket: {:undefined, [:versions]},
          user_lookup_fun: {:undefined, [:versions]},
          verify: {:verify_none, [:versions, :fail_if_no_peer_cert, :partial_chain]},
          verify_fun:
            {{fn
                _, {:bad_cert, _}, userState ->
                  {:valid, userState}

                _, {:extension, r_Extension(critical: true)}, userState ->
                  {:valid, userState}

                _, {:extension, _}, userState ->
                  {:unknown, userState}

                _, :valid, userState ->
                  {:valid, userState}

                _, :valid_peer, userState ->
                  {:valid, userState}
              end, []}, [:versions, :verify]},
          versions: {[], [:protocol]}
        }
      )

    sslOpts1 =
      add_missing_options(
        sslOpts0,
        %{
          alpn_advertised_protocols: {:undefined, [:versions]},
          alpn_preferred_protocols: {:undefined, [:versions]},
          anti_replay: {:undefined, [:versions, :session_tickets]},
          beast_mitigation: {:one_n_minus_one, [:versions]},
          cacertfile: {:undefined, [:versions, :verify_fun, :cacerts]},
          cacerts: {:undefined, [:versions]},
          cert: {:undefined, [:versions]},
          certfile: {<<>>, [:versions]},
          ciphers: {[], [:versions]},
          client_renegotiation: {:undefined, [:versions]},
          cookie: {true, [:versions]},
          crl_cache: {{:ssl_crl_cache, {:internal, []}}, [:versions]},
          crl_check: {false, [:versions]},
          customize_hostname_check: {[], [:versions]},
          depth: {1, [:versions]},
          dh: {:undefined, [:versions]},
          dhfile: {:undefined, [:versions]},
          eccs: {:undefined, [:versions]},
          erl_dist: {false, [:versions]},
          fail_if_no_peer_cert: {false, [:versions]},
          fallback: {false, [:versions]},
          handshake: {:full, [:versions]},
          hibernate_after: {:infinity, [:versions]},
          honor_cipher_order: {false, [:versions]},
          honor_ecc_order: {:undefined, [:versions]},
          key: {:undefined, [:versions]},
          keyfile: {:undefined, [:versions, :certfile]},
          key_update_at: {388_736_063_997, [:versions]},
          log_level: {:notice, [:versions]},
          max_handshake_size: {256 * 1024, [:versions]},
          middlebox_comp_mode: {true, [:versions]},
          max_fragment_length: {:undefined, [:versions]},
          next_protocol_selector: {:undefined, [:versions]},
          next_protocols_advertised: {:undefined, [:versions]},
          ocsp_stapling: {false, [:versions]},
          ocsp_responder_certs: {[], [:versions, :ocsp_stapling]},
          ocsp_nonce: {true, [:versions, :ocsp_stapling]},
          padding_check: {true, [:versions]},
          partial_chain:
            {fn _ ->
               :unknown_ca
             end, [:versions]},
          password: {'', [:versions]},
          protocol: {:tls, []},
          psk_identity: {:undefined, [:versions]},
          renegotiate_at: {268_435_456, [:versions]},
          reuse_session: {:undefined, [:versions]},
          reuse_sessions: {true, [:versions]},
          secure_renegotiate: {true, [:versions]},
          server_name_indication: {:undefined, [:versions]},
          session_tickets: {:disabled, [:versions]},
          signature_algs: {:undefined, [:versions]},
          signature_algs_cert: {:undefined, [:versions]},
          sni_fun: {:undefined, [:versions, :sni_hosts]},
          sni_hosts: {[], [:versions]},
          srp_identity: {:undefined, [:versions]},
          supported_groups: {:undefined, [:versions]},
          use_ticket: {:undefined, [:versions]},
          user_lookup_fun: {:undefined, [:versions]},
          verify: {:verify_none, [:versions, :fail_if_no_peer_cert, :partial_chain]},
          verify_fun:
            {{fn
                _, {:bad_cert, _}, userState ->
                  {:valid, userState}

                _, {:extension, r_Extension(critical: true)}, userState ->
                  {:valid, userState}

                _, {:extension, _}, userState ->
                  {:unknown, userState}

                _, :valid, userState ->
                  {:valid, userState}

                _, :valid_peer, userState ->
                  {:valid, userState}
              end, []}, [:versions, :verify]},
          versions: {[], [:protocol]}
        }
      )

    sslOpts =
      %{protocol: protocol} =
      process_options(sslOpts1, %{}, %{
        role: role,
        host: host,
        rules: %{
          alpn_advertised_protocols: {:undefined, [:versions]},
          alpn_preferred_protocols: {:undefined, [:versions]},
          anti_replay: {:undefined, [:versions, :session_tickets]},
          beast_mitigation: {:one_n_minus_one, [:versions]},
          cacertfile: {:undefined, [:versions, :verify_fun, :cacerts]},
          cacerts: {:undefined, [:versions]},
          cert: {:undefined, [:versions]},
          certfile: {<<>>, [:versions]},
          ciphers: {[], [:versions]},
          client_renegotiation: {:undefined, [:versions]},
          cookie: {true, [:versions]},
          crl_cache: {{:ssl_crl_cache, {:internal, []}}, [:versions]},
          crl_check: {false, [:versions]},
          customize_hostname_check: {[], [:versions]},
          depth: {1, [:versions]},
          dh: {:undefined, [:versions]},
          dhfile: {:undefined, [:versions]},
          eccs: {:undefined, [:versions]},
          erl_dist: {false, [:versions]},
          fail_if_no_peer_cert: {false, [:versions]},
          fallback: {false, [:versions]},
          handshake: {:full, [:versions]},
          hibernate_after: {:infinity, [:versions]},
          honor_cipher_order: {false, [:versions]},
          honor_ecc_order: {:undefined, [:versions]},
          key: {:undefined, [:versions]},
          keyfile: {:undefined, [:versions, :certfile]},
          key_update_at: {388_736_063_997, [:versions]},
          log_level: {:notice, [:versions]},
          max_handshake_size: {256 * 1024, [:versions]},
          middlebox_comp_mode: {true, [:versions]},
          max_fragment_length: {:undefined, [:versions]},
          next_protocol_selector: {:undefined, [:versions]},
          next_protocols_advertised: {:undefined, [:versions]},
          ocsp_stapling: {false, [:versions]},
          ocsp_responder_certs: {[], [:versions, :ocsp_stapling]},
          ocsp_nonce: {true, [:versions, :ocsp_stapling]},
          padding_check: {true, [:versions]},
          partial_chain:
            {fn _ ->
               :unknown_ca
             end, [:versions]},
          password: {'', [:versions]},
          protocol: {:tls, []},
          psk_identity: {:undefined, [:versions]},
          renegotiate_at: {268_435_456, [:versions]},
          reuse_session: {:undefined, [:versions]},
          reuse_sessions: {true, [:versions]},
          secure_renegotiate: {true, [:versions]},
          server_name_indication: {:undefined, [:versions]},
          session_tickets: {:disabled, [:versions]},
          signature_algs: {:undefined, [:versions]},
          signature_algs_cert: {:undefined, [:versions]},
          sni_fun: {:undefined, [:versions, :sni_hosts]},
          sni_hosts: {[], [:versions]},
          srp_identity: {:undefined, [:versions]},
          supported_groups: {:undefined, [:versions]},
          use_ticket: {:undefined, [:versions]},
          user_lookup_fun: {:undefined, [:versions]},
          verify: {:verify_none, [:versions, :fail_if_no_peer_cert, :partial_chain]},
          verify_fun:
            {{fn
                _, {:bad_cert, _}, userState ->
                  {:valid, userState}

                _, {:extension, r_Extension(critical: true)}, userState ->
                  {:valid, userState}

                _, {:extension, _}, userState ->
                  {:unknown, userState}

                _, :valid, userState ->
                  {:valid, userState}

                _, :valid_peer, userState ->
                  {:valid, userState}
              end, []}, [:versions, :verify]},
          versions: {[], [:protocol]}
        }
      })

    {sock, emulated} = emulated_options(protocol, sockOpts)
    connetionCb = connection_cb(protocol)
    cbInfo = handle_option_cb_info(opts0, protocol)

    {:ok,
     r_config(
       ssl: sslOpts,
       emulated: emulated,
       inet_ssl: sock,
       inet_user: sock,
       transport_info: cbInfo,
       connection_cb: connetionCb
     )}
  end

  defp process_options({[], [], _}, optionsMap, _Env) do
    optionsMap
  end

  defp process_options({[], [_ | _] = skipped, counter}, optionsMap, env)
       when length(skipped) < counter do
    process_options({skipped, [], length(skipped)}, optionsMap, env)
  end

  defp process_options({[], [_ | _], _Counter}, _OptionsMap, _Env) do
    throw({:error, :faulty_configuration})
  end

  defp process_options({[{k0, v} = e | t], s, counter}, optionsMap0, env) do
    k = maybe_map_key_internal(k0)

    case check_dependencies(k, optionsMap0, env) do
      true ->
        optionsMap = handle_option(k, v, optionsMap0, env)
        process_options({t, s, counter}, optionsMap, env)

      false ->
        process_options({t, [e | s], counter}, optionsMap0, env)
    end
  end

  defp handle_option(:anti_replay = option, :unbound, optionsMap, %{rules: rules}) do
    value =
      validate_option(
        option,
        default_value(option, rules)
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(
         :anti_replay = option,
         value0,
         %{session_tickets: sessionTickets, versions: versions} = optionsMap,
         %{rules: rules}
       ) do
    assert_option_dependency(option, :versions, versions, [:"tlsv1.3"])
    assert_option_dependency(option, :session_tickets, [sessionTickets], [:stateless])

    case sessionTickets do
      :stateless ->
        value = validate_option(option, value0)
        Map.put(optionsMap, option, value)

      _ ->
        Map.put(optionsMap, option, default_value(option, rules))
    end
  end

  defp handle_option(:beast_mitigation = option, :unbound, optionsMap, %{rules: rules}) do
    value =
      validate_option(
        option,
        default_value(option, rules)
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(:beast_mitigation = option, value0, %{versions: versions} = optionsMap, _Env) do
    assert_option_dependency(option, :versions, versions, [:tlsv1])
    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(
         :cacertfile = option,
         :unbound,
         %{cacerts: caCerts, verify: verify, verify_fun: verifyFun} = optionsMap,
         _Env
       )
       when verify === :verify_none or verify === 0 do
    value =
      validate_option(
        option,
        ca_cert_default(:verify_none, verifyFun, caCerts)
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(
         :cacertfile = option,
         :unbound,
         %{cacerts: caCerts, verify: verify, verify_fun: verifyFun} = optionsMap,
         _Env
       )
       when verify === :verify_peer or verify === 1 or verify === 2 do
    value =
      validate_option(
        option,
        ca_cert_default(:verify_peer, verifyFun, caCerts)
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(:cacertfile = option, value0, optionsMap, _Env) do
    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:ciphers = option, :unbound, %{versions: versions} = optionsMap, %{
         rules: rules
       }) do
    value =
      handle_cipher_option(
        default_value(
          option,
          rules
        ),
        versions
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(:ciphers = option, value0, %{versions: versions} = optionsMap, _Env) do
    value = handle_cipher_option(value0, versions)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:client_renegotiation = option, :unbound, optionsMap, %{role: role}) do
    value = default_option_role(:server, true, role)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(
         :client_renegotiation = option,
         value0,
         %{versions: versions} = optionsMap,
         %{role: role}
       ) do
    assert_role(:server_only, role, option, value0)
    assert_option_dependency(option, :versions, versions, [:tlsv1, :"tlsv1.1", :"tlsv1.2"])
    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:eccs = option, :unbound, %{versions: [highestVersion | _]} = optionsMap, %{
         rules: _Rules
       }) do
    value = handle_eccs_option(eccs(), highestVersion)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:eccs = option, value0, %{versions: [highestVersion | _]} = optionsMap, _Env) do
    value = handle_eccs_option(value0, highestVersion)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:fallback = option, :unbound, optionsMap, %{role: role}) do
    value = default_option_role(:client, false, role)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:fallback = option, value0, optionsMap, %{role: role}) do
    assert_role(:client_only, role, option, value0)
    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:cookie = option, :unbound, optionsMap, %{role: role}) do
    value = default_option_role(:server, true, role)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:cookie = option, value0, %{versions: versions} = optionsMap, %{role: role}) do
    assert_option_dependency(option, :versions, versions, [:"tlsv1.3"])
    assert_role(:server_only, role, option, value0)
    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:honor_cipher_order = option, :unbound, optionsMap, %{role: role}) do
    value = default_option_role(:server, false, role)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:honor_cipher_order = option, value0, optionsMap, %{role: role}) do
    assert_role(:server_only, role, option, value0)
    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:honor_ecc_order = option, :unbound, optionsMap, %{role: role}) do
    value = default_option_role(:server, false, role)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:honor_ecc_order = option, value0, optionsMap, %{role: role}) do
    assert_role(:server_only, role, option, value0)
    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:keyfile = option, :unbound, %{certfile: certFile} = optionsMap, _Env) do
    value = validate_option(option, certFile)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:key_update_at = option, :unbound, optionsMap, %{rules: rules}) do
    value =
      validate_option(
        option,
        default_value(option, rules)
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(:key_update_at = option, value0, %{versions: versions} = optionsMap, _Env) do
    assert_option_dependency(option, :versions, versions, [:"tlsv1.3"])
    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:next_protocols_advertised = option, :unbound, optionsMap, %{rules: rules}) do
    value =
      validate_option(
        option,
        default_value(option, rules)
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(
         :next_protocols_advertised = option,
         value0,
         %{versions: versions} = optionsMap,
         _Env
       ) do
    assert_option_dependency(:next_protocols_advertised, :versions, versions, [
      :tlsv1,
      :"tlsv1.1",
      :"tlsv1.2"
    ])

    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:next_protocol_selector = option, :unbound, optionsMap, %{rules: rules}) do
    value = default_value(option, rules)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(
         :next_protocol_selector = option,
         value0,
         %{versions: versions} = optionsMap,
         _Env
       ) do
    assert_option_dependency(:client_preferred_next_protocols, :versions, versions, [
      :tlsv1,
      :"tlsv1.1",
      :"tlsv1.2"
    ])

    value =
      make_next_protocol_selector(
        validate_option(
          :client_preferred_next_protocols,
          value0
        )
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(:padding_check = option, :unbound, optionsMap, %{rules: rules}) do
    value =
      validate_option(
        option,
        default_value(option, rules)
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(:padding_check = option, value0, %{versions: versions} = optionsMap, _Env) do
    assert_option_dependency(option, :versions, versions, [:tlsv1])
    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:psk_identity = option, :unbound, optionsMap, %{rules: rules}) do
    value =
      validate_option(
        option,
        default_value(option, rules)
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(:psk_identity = option, value0, %{versions: versions} = optionsMap, _Env) do
    assert_option_dependency(option, :versions, versions, [:tlsv1, :"tlsv1.1", :"tlsv1.2"])
    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:secure_renegotiate = option, :unbound, optionsMap, %{rules: rules}) do
    value =
      validate_option(
        option,
        default_value(option, rules)
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(
         :secure_renegotiate = option,
         value0,
         %{versions: versions} = optionsMap,
         _Env
       ) do
    assert_option_dependency(:secure_renegotiate, :versions, versions, [
      :tlsv1,
      :"tlsv1.1",
      :"tlsv1.2"
    ])

    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:reuse_session = option, :unbound, optionsMap, %{role: role}) do
    value =
      case role do
        :client ->
          :undefined

        :server ->
          fn _, _, _, _ ->
            true
          end
      end

    Map.put(optionsMap, option, value)
  end

  defp handle_option(:reuse_session = option, value0, %{versions: versions} = optionsMap, _Env) do
    assert_option_dependency(:reuse_session, :versions, versions, [:tlsv1, :"tlsv1.1", :"tlsv1.2"])

    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:reuse_sessions = option, :unbound, optionsMap, %{rules: rules}) do
    value =
      validate_option(
        option,
        default_value(option, rules)
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(:reuse_sessions = option, value0, %{versions: versions} = optionsMap, _Env) do
    assert_option_dependency(:reuse_sessions, :versions, versions, [
      :tlsv1,
      :"tlsv1.1",
      :"tlsv1.2"
    ])

    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:server_name_indication = option, :unbound, optionsMap, %{
         host: host,
         role: role
       }) do
    value = default_option_role(:client, server_name_indication_default(host), role)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:server_name_indication = option, value0, optionsMap, _Env) do
    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:session_tickets = option, :unbound, optionsMap, %{rules: rules}) do
    value =
      validate_option(
        option,
        default_value(option, rules)
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(:session_tickets = option, value0, %{versions: versions} = optionsMap, %{
         role: role
       }) do
    assert_option_dependency(option, :versions, versions, [:"tlsv1.3"])

    assert_role_value(role, option, value0, [:disabled, :stateful, :stateless], [
      :disabled,
      :manual,
      :auto
    ])

    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(
         :signature_algs = option,
         :unbound,
         %{versions: [highestVersion | _]} = optionsMap,
         %{role: role}
       ) do
    value =
      handle_hashsigns_option(
        default_option_role_sign_algs(
          :server,
          :tls_v1.default_signature_algs(highestVersion),
          role,
          highestVersion
        ),
        tls_version(highestVersion)
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(
         :signature_algs = option,
         value0,
         %{versions: [highestVersion | _]} = optionsMap,
         _Env
       ) do
    value =
      handle_hashsigns_option(
        value0,
        tls_version(highestVersion)
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(
         :signature_algs_cert = option,
         :unbound,
         %{versions: [highestVersion | _]} = optionsMap,
         _Env
       ) do
    value =
      handle_signature_algorithms_option(
        :undefined,
        tls_version(highestVersion)
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(
         :signature_algs_cert = option,
         value0,
         %{versions: [highestVersion | _]} = optionsMap,
         _Env
       ) do
    value =
      handle_signature_algorithms_option(
        value0,
        tls_version(highestVersion)
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(:sni_fun = option, :unbound, optionsMap, %{rules: rules}) do
    value = default_value(option, rules)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:sni_fun = option, value0, optionsMap, _Env) do
    validate_option(option, value0)
    optHosts = :maps.get(:sni_hosts, optionsMap, :undefined)

    value =
      case {value0, optHosts} do
        {:undefined, _} ->
          value0

        {_, []} ->
          value0

        _ ->
          throw({:error, {:conflict_options, [:sni_fun, :sni_hosts]}})
      end

    Map.put(optionsMap, option, value)
  end

  defp handle_option(:srp_identity = option, :unbound, optionsMap, %{rules: rules}) do
    value =
      validate_option(
        option,
        default_value(option, rules)
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(:srp_identity = option, value0, %{versions: versions} = optionsMap, _Env) do
    assert_option_dependency(:srp_identity, :versions, versions, [:tlsv1, :"tlsv1.1", :"tlsv1.2"])
    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(
         :supported_groups = option,
         :unbound,
         %{versions: [highestVersion | _]} = optionsMap,
         %{rules: _Rules}
       ) do
    value =
      handle_supported_groups_option(
        groups(:default),
        highestVersion
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(
         :supported_groups = option,
         value0,
         %{versions: [highestVersion | _] = versions} = optionsMap,
         _Env
       ) do
    assert_option_dependency(option, :versions, versions, [:"tlsv1.3"])

    value =
      handle_supported_groups_option(
        value0,
        highestVersion
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(:use_ticket = option, :unbound, optionsMap, %{rules: rules}) do
    value =
      validate_option(
        option,
        default_value(option, rules)
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(:use_ticket = option, value0, %{versions: versions} = optionsMap, _Env) do
    assert_option_dependency(option, :versions, versions, [:"tlsv1.3"])
    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:user_lookup_fun = option, :unbound, optionsMap, %{rules: rules}) do
    value =
      validate_option(
        option,
        default_value(option, rules)
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(:user_lookup_fun = option, value0, %{versions: versions} = optionsMap, _Env) do
    assert_option_dependency(option, :versions, versions, [:tlsv1, :"tlsv1.1", :"tlsv1.2"])
    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:verify = option, :unbound, optionsMap, %{rules: rules}) do
    handle_verify_option(
      default_value(option, rules),
      optionsMap
    )
  end

  defp handle_option(:verify = _Option, value, optionsMap, _Env) do
    handle_verify_option(value, optionsMap)
  end

  defp handle_option(:verify_fun = option, :unbound, %{verify: verify} = optionsMap, %{
         rules: rules
       })
       when verify === :verify_none do
    Map.put(optionsMap, option, default_value(option, rules))
  end

  defp handle_option(:verify_fun = option, :unbound, %{verify: verify} = optionsMap, _Env)
       when verify === :verify_peer do
    Map.put(optionsMap, option, :undefined)
  end

  defp handle_option(:verify_fun = option, value0, optionsMap, _Env) do
    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:versions = option, :unbound, %{protocol: protocol} = optionsMap, _Env) do
    recordCb = record_cb(protocol)
    vsns0 = recordCb.supported_protocol_versions()

    value =
      :lists.sort(
        Function.capture(recordCb, :is_higher, 2),
        vsns0
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(:versions = option, vsns0, %{protocol: protocol} = optionsMap, _Env) do
    validate_option(:versions, vsns0)
    recordCb = record_cb(protocol)

    vsns1 =
      for vsn <- vsns0 do
        recordCb.protocol_version(vsn)
      end

    value =
      :lists.sort(
        Function.capture(recordCb, :is_higher, 2),
        vsns1
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(:cb_info = option, :unbound, %{protocol: protocol} = optionsMap, _Env) do
    default = default_cb_info(protocol)
    validate_option(option, default)
    value = handle_cb_info(default)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(:cb_info = option, value0, optionsMap, _Env) do
    validate_option(option, value0)
    value = handle_cb_info(value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option(option, :unbound, optionsMap, %{rules: rules}) do
    value =
      validate_option(
        option,
        default_value(option, rules)
      )

    Map.put(optionsMap, option, value)
  end

  defp handle_option(option, value0, optionsMap, _Env) do
    value = validate_option(option, value0)
    Map.put(optionsMap, option, value)
  end

  defp handle_option_cb_info(options, protocol) do
    value = :proplists.get_value(:cb_info, options, default_cb_info(protocol))
    %{cb_info: cbInfo} = handle_option(:cb_info, value, %{protocol: protocol}, %{})
    cbInfo
  end

  defp maybe_map_key_internal(:client_preferred_next_protocols) do
    :next_protocol_selector
  end

  defp maybe_map_key_internal(k) do
    k
  end

  defp maybe_map_key_external(:next_protocol_selector) do
    :client_preferred_next_protocols
  end

  defp maybe_map_key_external(k) do
    k
  end

  defp check_dependencies(k, optionsMap, env) do
    rules = :maps.get(:rules, env)
    deps = get_dependencies(k, rules)

    case deps do
      [] ->
        true

      l ->
        option_already_defined(
          k,
          optionsMap
        ) or
          dependecies_already_defined(
            l,
            optionsMap
          )
    end
  end

  defp get_dependencies(k, _) when k === :cb_info or k === :log_alert do
    []
  end

  defp get_dependencies(k, rules) do
    {_, deps} = :maps.get(k, rules)
    deps
  end

  defp option_already_defined(k, map) do
    :maps.get(k, map, :unbound) !== :unbound
  end

  defp dependecies_already_defined(l, optionsMap) do
    fun = fn e ->
      option_already_defined(e, optionsMap)
    end

    :lists.all(fun, l)
  end

  defp expand_options(opts0, rules) do
    opts1 =
      :proplists.expand(
        [{:binary, [{:mode, :binary}]}, {:list, [{:mode, :list}]}],
        opts0
      )

    assert_proplist(opts1)
    opts = :proplists.delete(:ssl_imp, opts1)
    allOpts = :maps.keys(rules)

    sockOpts =
      :lists.foldl(
        fn key, propList ->
          :proplists.delete(key, propList)
        end,
        opts,
        allOpts ++ [:ssl_imp, :cb_info, :client_preferred_next_protocols, :log_alert]
      )

    sslOpts = {opts -- sockOpts, [], length(opts -- sockOpts)}
    {sslOpts, sockOpts}
  end

  defp add_missing_options({l0, s, _C}, rules) do
    fun = fn k0, acc ->
      k = maybe_map_key_external(k0)

      case :proplists.is_defined(k, acc) do
        true ->
          acc

        false ->
          default = :unbound
          [{k, default} | acc]
      end
    end

    allOpts = :maps.keys(rules)
    l = :lists.foldl(fun, l0, allOpts)
    {l, s, length(l)}
  end

  defp default_value(key, rules) do
    {default, _} = :maps.get(key, rules, {:undefined, []})
    default
  end

  defp assert_role(:client_only, :client, _, _) do
    :ok
  end

  defp assert_role(:server_only, :server, _, _) do
    :ok
  end

  defp assert_role(:client_only, _, _, :undefined) do
    :ok
  end

  defp assert_role(:server_only, _, _, :undefined) do
    :ok
  end

  defp assert_role(type, _, key, _) do
    throw({:error, {:option, type, key}})
  end

  defp assert_role_value(:client, option, value, _, clientValues) do
    case :lists.member(value, clientValues) do
      true ->
        :ok

      false ->
        throw({:error, {:options, :role, {option, {value, {:client, clientValues}}}}})
    end
  end

  defp assert_role_value(:server, option, value, serverValues, _) do
    case :lists.member(value, serverValues) do
      true ->
        :ok

      false ->
        throw({:error, {:options, :role, {option, {value, {:server, serverValues}}}}})
    end
  end

  defp assert_option_dependency(option, optionDep, values0, allowedValues) do
    case is_dtls_configured(values0) do
      true ->
        :ok

      false ->
        values =
          case optionDep do
            :versions ->
              :lists.map(&:tls_record.protocol_version/1, values0)

            _ ->
              values0
          end

        set1 = :sets.from_list(values)
        set2 = :sets.from_list(allowedValues)

        case :sets.size(:sets.intersection(set1, set2)) > 0 do
          true ->
            :ok

          false ->
            throw({:error, {:options, :dependency, {option, {optionDep, allowedValues}}}})
        end
    end
  end

  defp is_dtls_configured(versions) do
    fun = fn
      version when version === {254, 253} or version === {254, 255} ->
        true

      _ ->
        false
    end

    :lists.any(fun, versions)
  end

  defp validate_option(:versions, versions) do
    validate_versions(versions, versions)
  end

  defp validate_option(:verify, value)
       when value == :verify_none or
              value == :verify_peer do
    value
  end

  defp validate_option(:verify_fun, :undefined) do
    :undefined
  end

  defp validate_option(:verify_fun, fun) when is_function(fun) do
    {fn
       _, {:bad_cert, _} = reason, oldFun ->
         case oldFun.([reason]) do
           true ->
             {:valid, oldFun}

           false ->
             {:fail, reason}
         end

       _, {:extension, _}, userState ->
         {:unknown, userState}

       _, :valid, userState ->
         {:valid, userState}

       _, :valid_peer, userState ->
         {:valid, userState}
     end, fun}
  end

  defp validate_option(:verify_fun, {fun, _} = value)
       when is_function(fun) do
    value
  end

  defp validate_option(:partial_chain, value)
       when is_function(value) do
    value
  end

  defp validate_option(:fail_if_no_peer_cert, value)
       when is_boolean(value) do
    value
  end

  defp validate_option(:depth, value)
       when is_integer(value) and
              value >= 0 and value <= 255 do
    value
  end

  defp validate_option(:cert, value)
       when value == :undefined or
              is_binary(value) do
    value
  end

  defp validate_option(:certfile, :undefined = value) do
    value
  end

  defp validate_option(:certfile, value) when is_binary(value) do
    value
  end

  defp validate_option(:certfile, value) when is_list(value) do
    binary_filename(value)
  end

  defp validate_option(:key, :undefined) do
    :undefined
  end

  defp validate_option(:key, {keyType, value})
       when (is_binary(value) and keyType == :rsa) or
              keyType == :dsa or keyType == :RSAPrivateKey or
              keyType == :DSAPrivateKey or keyType == :ECPrivateKey or
              keyType == :PrivateKeyInfo do
    {keyType, value}
  end

  defp validate_option(:key, %{algorithm: _} = value) do
    value
  end

  defp validate_option(:keyfile, :undefined) do
    <<>>
  end

  defp validate_option(:keyfile, value) when is_binary(value) do
    value
  end

  defp validate_option(:keyfile, value)
       when is_list(value) and
              value !== '' do
    binary_filename(value)
  end

  defp validate_option(:key_update_at, value)
       when is_integer(value) and value > 0 do
    value
  end

  defp validate_option(:password, value) when is_list(value) do
    value
  end

  defp validate_option(:cacerts, value)
       when value == :undefined or
              is_list(value) do
    value
  end

  defp validate_option(:cacertfile, :undefined) do
    <<>>
  end

  defp validate_option(:cacertfile, value) when is_binary(value) do
    value
  end

  defp validate_option(:cacertfile, value)
       when is_list(value) and
              value !== '' do
    binary_filename(value)
  end

  defp validate_option(:dh, value)
       when value == :undefined or
              is_binary(value) do
    value
  end

  defp validate_option(:dhfile, :undefined = value) do
    value
  end

  defp validate_option(:dhfile, value) when is_binary(value) do
    value
  end

  defp validate_option(:dhfile, value)
       when is_list(value) and
              value !== '' do
    binary_filename(value)
  end

  defp validate_option(:psk_identity, :undefined) do
    :undefined
  end

  defp validate_option(:psk_identity, identity)
       when is_list(identity) and identity !== '' and
              length(identity) <= 65535 do
    binary_filename(identity)
  end

  defp validate_option(:user_lookup_fun, :undefined) do
    :undefined
  end

  defp validate_option(:user_lookup_fun, {fun, _} = value)
       when is_function(fun, 3) do
    value
  end

  defp validate_option(:srp_identity, :undefined) do
    :undefined
  end

  defp validate_option(:srp_identity, {username, password})
       when is_list(username) and is_list(password) and
              username !== '' and length(username) <= 255 do
    {:unicode.characters_to_binary(username), :unicode.characters_to_binary(password)}
  end

  defp validate_option(:reuse_session, :undefined) do
    :undefined
  end

  defp validate_option(:reuse_session, value)
       when is_function(value) do
    value
  end

  defp validate_option(:reuse_session, value) when is_binary(value) do
    value
  end

  defp validate_option(:reuse_session, {id, data} = value)
       when is_binary(id) and is_binary(data) do
    value
  end

  defp validate_option(:reuse_sessions, value)
       when is_boolean(value) do
    value
  end

  defp validate_option(:reuse_sessions, :save = value) do
    value
  end

  defp validate_option(:secure_renegotiate, value)
       when is_boolean(value) do
    value
  end

  defp validate_option(:client_renegotiation, value)
       when is_boolean(value) do
    value
  end

  defp validate_option(:renegotiate_at, value)
       when is_integer(value) do
    :erlang.min(value, 268_435_456)
  end

  defp validate_option(:hibernate_after, :undefined) do
    :infinity
  end

  defp validate_option(:hibernate_after, :infinity) do
    :infinity
  end

  defp validate_option(:hibernate_after, value)
       when is_integer(value) and value >= 0 do
    value
  end

  defp validate_option(:erl_dist, value) when is_boolean(value) do
    value
  end

  defp validate_option(opt, value)
       when opt === :alpn_advertised_protocols or
              (opt === :alpn_preferred_protocols and
                 is_list(value)) do
    validate_binary_list(opt, value)
    value
  end

  defp validate_option(opt, value)
       when opt === :alpn_advertised_protocols or
              (opt === :alpn_preferred_protocols and
                 value === :undefined) do
    :undefined
  end

  defp validate_option(
         :client_preferred_next_protocols,
         {precedence, preferredProtocols}
       )
       when is_list(preferredProtocols) do
    validate_binary_list(
      :client_preferred_next_protocols,
      preferredProtocols
    )

    validate_npn_ordering(precedence)
    {precedence, preferredProtocols, <<>>}
  end

  defp validate_option(
         :client_preferred_next_protocols,
         {precedence, preferredProtocols, default} = value
       )
       when is_list(preferredProtocols) and
              is_binary(default) and byte_size(default) > 0 and
              byte_size(default) < 256 do
    validate_binary_list(
      :client_preferred_next_protocols,
      preferredProtocols
    )

    validate_npn_ordering(precedence)
    value
  end

  defp validate_option(:client_preferred_next_protocols, :undefined) do
    :undefined
  end

  defp validate_option(:log_alert, true) do
    :notice
  end

  defp validate_option(:log_alert, false) do
    :warning
  end

  defp validate_option(:log_level, value)
       when is_atom(value) and
              (value === :emergency or value === :alert or value === :critical or value === :error or
                 value === :warning or value === :notice or value === :info or value === :debug) do
    value
  end

  defp validate_option(:middlebox_comp_mode, value)
       when is_boolean(value) do
    value
  end

  defp validate_option(:next_protocols_advertised, value)
       when is_list(value) do
    validate_binary_list(:next_protocols_advertised, value)
    value
  end

  defp validate_option(:next_protocols_advertised, :undefined) do
    :undefined
  end

  defp validate_option(:server_name_indication, value)
       when is_list(value) do
    value
  end

  defp validate_option(:server_name_indication, :undefined) do
    :undefined
  end

  defp validate_option(:server_name_indication, :disable) do
    :disable
  end

  defp validate_option(:max_fragment_length, i)
       when i == 512 or
              i == 1024 or i == 2048 or i == 4096 do
    i
  end

  defp validate_option(:max_fragment_length, :undefined) do
    :undefined
  end

  defp validate_option(:sni_hosts, []) do
    []
  end

  defp validate_option(:sni_hosts, [{hostname, sSLOptions} | tail])
       when is_list(hostname) do
    recursiveSNIOptions = :proplists.get_value(:sni_hosts, sSLOptions, :undefined)

    case recursiveSNIOptions do
      :undefined ->
        [
          {hostname, validate_options(sSLOptions)}
          | validate_option(:sni_hosts, tail)
        ]

      _ ->
        throw({:error, {:options, {:sni_hosts, recursiveSNIOptions}}})
    end
  end

  defp validate_option(:sni_fun, :undefined) do
    :undefined
  end

  defp validate_option(:sni_fun, fun) when is_function(fun) do
    fun
  end

  defp validate_option(:honor_cipher_order, value)
       when is_boolean(value) do
    value
  end

  defp validate_option(:honor_ecc_order, value)
       when is_boolean(value) do
    value
  end

  defp validate_option(:padding_check, value) when is_boolean(value) do
    value
  end

  defp validate_option(:fallback, value) when is_boolean(value) do
    value
  end

  defp validate_option(:cookie, value) when is_boolean(value) do
    value
  end

  defp validate_option(:crl_check, value) when is_boolean(value) do
    value
  end

  defp validate_option(:crl_check, value)
       when value == :best_effort or value == :peer do
    value
  end

  defp validate_option(:crl_cache, {cb, {_Handle, options}} = value)
       when is_atom(cb) and is_list(options) do
    value
  end

  defp validate_option(:beast_mitigation, value)
       when value == :one_n_minus_one or value == :zero_n or value == :disabled do
    value
  end

  defp validate_option(:max_handshake_size, value)
       when is_integer(value) and value <= 8_388_607 do
    value
  end

  defp validate_option(:protocol, value = :tls) do
    value
  end

  defp validate_option(:protocol, value = :dtls) do
    value
  end

  defp validate_option(:handshake, :hello = value) do
    value
  end

  defp validate_option(:handshake, :full = value) do
    value
  end

  defp validate_option(:customize_hostname_check, value)
       when is_list(value) do
    value
  end

  defp validate_option(:cb_info, {v1, v2, v3, v4} = value)
       when is_atom(v1) and is_atom(v2) and is_atom(v3) and
              is_atom(v4) do
    value
  end

  defp validate_option(:cb_info, {v1, v2, v3, v4, v5} = value)
       when is_atom(v1) and is_atom(v2) and is_atom(v3) and
              is_atom(v4) and is_atom(v5) do
    value
  end

  defp validate_option(:use_ticket, value) when is_list(value) do
    value
  end

  defp validate_option(:session_tickets, value)
       when value === :disabled or value === :manual or value === :auto or value === :stateless or
              value === :stateful do
    value
  end

  defp validate_option(:anti_replay, :"10k") do
    {10, 5, 72985}
  end

  defp validate_option(:anti_replay, :"100k") do
    {10, 5, 729_845}
  end

  defp validate_option(:anti_replay, value)
       when is_tuple(value) and tuple_size(value) === 3 do
    value
  end

  defp validate_option(:ocsp_stapling, value)
       when value === true or value === false do
    value
  end

  defp validate_option(:ocsp_responder_certs, value)
       when is_list(value) do
    for certDer <- value, is_binary(certDer) do
      :public_key.pkix_decode_cert(certDer, :plain)
    end
  end

  defp validate_option(:ocsp_nonce, value)
       when value === true or value === false do
    value
  end

  defp validate_option(opt, :undefined = value) do
    allOpts =
      :maps.keys(%{
        alpn_advertised_protocols: {:undefined, [:versions]},
        alpn_preferred_protocols: {:undefined, [:versions]},
        anti_replay: {:undefined, [:versions, :session_tickets]},
        beast_mitigation: {:one_n_minus_one, [:versions]},
        cacertfile: {:undefined, [:versions, :verify_fun, :cacerts]},
        cacerts: {:undefined, [:versions]},
        cert: {:undefined, [:versions]},
        certfile: {<<>>, [:versions]},
        ciphers: {[], [:versions]},
        client_renegotiation: {:undefined, [:versions]},
        cookie: {true, [:versions]},
        crl_cache: {{:ssl_crl_cache, {:internal, []}}, [:versions]},
        crl_check: {false, [:versions]},
        customize_hostname_check: {[], [:versions]},
        depth: {1, [:versions]},
        dh: {:undefined, [:versions]},
        dhfile: {:undefined, [:versions]},
        eccs: {:undefined, [:versions]},
        erl_dist: {false, [:versions]},
        fail_if_no_peer_cert: {false, [:versions]},
        fallback: {false, [:versions]},
        handshake: {:full, [:versions]},
        hibernate_after: {:infinity, [:versions]},
        honor_cipher_order: {false, [:versions]},
        honor_ecc_order: {:undefined, [:versions]},
        key: {:undefined, [:versions]},
        keyfile: {:undefined, [:versions, :certfile]},
        key_update_at: {388_736_063_997, [:versions]},
        log_level: {:notice, [:versions]},
        max_handshake_size: {256 * 1024, [:versions]},
        middlebox_comp_mode: {true, [:versions]},
        max_fragment_length: {:undefined, [:versions]},
        next_protocol_selector: {:undefined, [:versions]},
        next_protocols_advertised: {:undefined, [:versions]},
        ocsp_stapling: {false, [:versions]},
        ocsp_responder_certs: {[], [:versions, :ocsp_stapling]},
        ocsp_nonce: {true, [:versions, :ocsp_stapling]},
        padding_check: {true, [:versions]},
        partial_chain:
          {fn _ ->
             :unknown_ca
           end, [:versions]},
        password: {'', [:versions]},
        protocol: {:tls, []},
        psk_identity: {:undefined, [:versions]},
        renegotiate_at: {268_435_456, [:versions]},
        reuse_session: {:undefined, [:versions]},
        reuse_sessions: {true, [:versions]},
        secure_renegotiate: {true, [:versions]},
        server_name_indication: {:undefined, [:versions]},
        session_tickets: {:disabled, [:versions]},
        signature_algs: {:undefined, [:versions]},
        signature_algs_cert: {:undefined, [:versions]},
        sni_fun: {:undefined, [:versions, :sni_hosts]},
        sni_hosts: {[], [:versions]},
        srp_identity: {:undefined, [:versions]},
        supported_groups: {:undefined, [:versions]},
        use_ticket: {:undefined, [:versions]},
        user_lookup_fun: {:undefined, [:versions]},
        verify: {:verify_none, [:versions, :fail_if_no_peer_cert, :partial_chain]},
        verify_fun:
          {{fn
              _, {:bad_cert, _}, userState ->
                {:valid, userState}

              _, {:extension, r_Extension(critical: true)}, userState ->
                {:valid, userState}

              _, {:extension, _}, userState ->
                {:unknown, userState}

              _, :valid, userState ->
                {:valid, userState}

              _, :valid_peer, userState ->
                {:valid, userState}
            end, []}, [:versions, :verify]},
        versions: {[], [:protocol]}
      })

    case :lists.member(opt, allOpts) do
      true ->
        value

      false ->
        throw({:error, {:options, {opt, value}}})
    end
  end

  defp validate_option(opt, value) do
    throw({:error, {:options, {opt, value}}})
  end

  defp handle_cb_info({v1, v2, v3, v4}) do
    {v1, v2, v3, v4, :erlang.list_to_atom(:erlang.atom_to_list(v2) ++ '_passive')}
  end

  defp handle_cb_info(cbInfo) do
    cbInfo
  end

  defp handle_hashsigns_option(value, version)
       when is_list(value) and version >= {3, 4} do
    case :tls_v1.signature_schemes(version, value) do
      [] ->
        throw({:error, {:options, :no_supported_signature_schemes, {:signature_algs, value}}})

      _ ->
        value
    end
  end

  defp handle_hashsigns_option(value, version)
       when is_list(value) and version === {3, 3} do
    case :tls_v1.signature_algs(version, value) do
      [] ->
        throw({:error, {:options, :no_supported_algorithms, {:signature_algs, value}}})

      _ ->
        value
    end
  end

  defp handle_hashsigns_option(_, version) when version === {3, 3} do
    handle_hashsigns_option(
      :tls_v1.default_signature_algs(version),
      version
    )
  end

  defp handle_hashsigns_option(_, _Version) do
    :undefined
  end

  defp handle_signature_algorithms_option(value, version)
       when is_list(value) and version >= {3, 4} do
    case :tls_v1.signature_schemes(version, value) do
      [] ->
        throw(
          {:error, {:options, :no_supported_signature_schemes, {:signature_algs_cert, value}}}
        )

      _ ->
        value
    end
  end

  defp handle_signature_algorithms_option(_, _Version) do
    :undefined
  end

  defp validate_options([]) do
    []
  end

  defp validate_options([{opt, value} | tail]) do
    [
      {opt, validate_option(opt, value)}
      | validate_options(tail)
    ]
  end

  defp validate_npn_ordering(:client) do
    :ok
  end

  defp validate_npn_ordering(:server) do
    :ok
  end

  defp validate_npn_ordering(value) do
    throw({:error, {:options, {:client_preferred_next_protocols, {:invalid_precedence, value}}}})
  end

  defp validate_binary_list(opt, list) do
    :lists.foreach(
      fn
        bin
        when is_binary(bin) and
               byte_size(bin) > 0 and byte_size(bin) < 256 ->
          :ok

        bin ->
          throw({:error, {:options, {opt, {:invalid_protocol, bin}}}})
      end,
      list
    )
  end

  defp validate_versions([], versions) do
    versions
  end

  defp validate_versions([version | rest], versions)
       when version == :"tlsv1.3" or version == :"tlsv1.2" or version == :"tlsv1.1" or
              version == :tlsv1 do
    case :tls_record.sufficient_crypto_support(version) do
      true ->
        tls_validate_versions(rest, versions)

      false ->
        throw(
          {:error, {:options, {:insufficient_crypto_support, {version, {:versions, versions}}}}}
        )
    end
  end

  defp validate_versions([version | rest], versions)
       when version == :dtlsv1 or version == :"dtlsv1.2" do
    dTLSVer = :dtls_record.protocol_version(version)

    case :tls_record.sufficient_crypto_support(:dtls_v1.corresponding_tls_version(dTLSVer)) do
      true ->
        dtls_validate_versions(rest, versions)

      false ->
        throw(
          {:error, {:options, {:insufficient_crypto_support, {version, {:versions, versions}}}}}
        )
    end
  end

  defp validate_versions([version | _], versions) do
    throw({:error, {:options, {version, {:versions, versions}}}})
  end

  defp tls_validate_versions([], versions) do
    tls_validate_version_gap(versions)
  end

  defp tls_validate_versions([version | rest], versions)
       when version == :"tlsv1.3" or version == :"tlsv1.2" or version == :"tlsv1.1" or
              version == :tlsv1 do
    tls_validate_versions(rest, versions)
  end

  defp tls_validate_versions([version | _], versions) do
    throw({:error, {:options, {version, {:versions, versions}}}})
  end

  defp tls_validate_version_gap(versions) do
    case :lists.member(:"tlsv1.3", versions) do
      true when length(versions) >= 2 ->
        case :lists.member(:"tlsv1.2", versions) do
          true ->
            versions

          false ->
            throw({:error, {:options, :missing_version, {:"tlsv1.2", {:versions, versions}}}})
        end

      _ ->
        versions
    end
  end

  defp dtls_validate_versions([], versions) do
    versions
  end

  defp dtls_validate_versions([version | rest], versions)
       when version == :dtlsv1 or version == :"dtlsv1.2" do
    dtls_validate_versions(rest, versions)
  end

  defp dtls_validate_versions([ver | _], versions) do
    throw({:error, {:options, {ver, {:versions, versions}}}})
  end

  defp ca_cert_default(_, _, [_ | _]) do
    :undefined
  end

  defp ca_cert_default(:verify_none, _, _) do
    :undefined
  end

  defp ca_cert_default(:verify_peer, {fun, _}, _)
       when is_function(fun) do
    :undefined
  end

  defp ca_cert_default(:verify_peer, :undefined, _) do
    ''
  end

  defp emulated_options(protocol, opts) do
    case protocol do
      :tls ->
        :tls_socket.emulated_options(opts)

      :dtls ->
        :dtls_socket.emulated_options(opts)
    end
  end

  defp handle_cipher_option(value, versions) when is_list(value) do
    try do
      binary_cipher_suites(versions, value)
    catch
      :exit, _ ->
        throw({:error, {:options, {:ciphers, value}}})

      :error, _ ->
        throw({:error, {:options, {:ciphers, value}}})
    else
      suites ->
        suites
    end
  end

  defp binary_cipher_suites([{3, 4} = version], []) do
    default_binary_suites(:exclusive, version)
  end

  defp binary_cipher_suites([version | _], []) do
    default_binary_suites(:default, version)
  end

  defp binary_cipher_suites(versions, [map | _] = ciphers0)
       when is_map(map) do
    ciphers =
      for c <- ciphers0 do
        :ssl_cipher_format.suite_map_to_bin(c)
      end

    binary_cipher_suites(versions, ciphers)
  end

  defp binary_cipher_suites(versions, [tuple | _] = ciphers0)
       when is_tuple(tuple) do
    ciphers =
      for c <- ciphers0 do
        :ssl_cipher_format.suite_map_to_bin(tuple_to_map(c))
      end

    binary_cipher_suites(versions, ciphers)
  end

  defp binary_cipher_suites(
         [version | _] = versions,
         [cipher0 | _] = ciphers0
       )
       when is_binary(cipher0) do
    all = :ssl_cipher.all_suites(version) ++ :ssl_cipher.anonymous_suites(version)

    case (for cipher <- ciphers0,
              :lists.member(cipher, all) do
            cipher
          end) do
      [] ->
        binary_cipher_suites(versions, [])

      ciphers ->
        ciphers
    end
  end

  defp binary_cipher_suites(versions, [head | _] = ciphers0)
       when is_list(head) do
    ciphers =
      for c <- ciphers0 do
        :ssl_cipher_format.suite_openssl_str_to_map(c)
      end

    binary_cipher_suites(versions, ciphers)
  end

  defp binary_cipher_suites(versions, ciphers0) do
    ciphers =
      for c <- :string.lexemes(ciphers0, ':') do
        :ssl_cipher_format.suite_openssl_str_to_map(c)
      end

    binary_cipher_suites(versions, ciphers)
  end

  defp default_binary_suites(:exclusive, {_, minor}) do
    :ssl_cipher.filter_suites(:tls_v1.exclusive_suites(minor))
  end

  defp default_binary_suites(:default, version) do
    :ssl_cipher.filter_suites(:ssl_cipher.suites(version))
  end

  defp tuple_to_map({kex, cipher, mac}) do
    %{key_exchange: kex, cipher: cipher, mac: mac, prf: :default_prf}
  end

  defp tuple_to_map({kex, cipher, mac, prf}) do
    %{key_exchange: kex, cipher: cipher, mac: tuple_to_map_mac(cipher, mac), prf: prf}
  end

  defp tuple_to_map_mac(:aes_128_gcm, _) do
    :aead
  end

  defp tuple_to_map_mac(:aes_256_gcm, _) do
    :aead
  end

  defp tuple_to_map_mac(:chacha20_poly1305, _) do
    :aead
  end

  defp tuple_to_map_mac(_, mAC) do
    mAC
  end

  defp handle_eccs_option(value, version) when is_list(value) do
    {_Major, minor} = tls_version(version)

    try do
      :tls_v1.ecc_curves(minor, value)
    catch
      :exit, _ ->
        throw({:error, {:options, {:eccs, value}}})

      :error, _ ->
        throw({:error, {:options, {:eccs, value}}})
    else
      curves ->
        r_elliptic_curves(elliptic_curve_list: curves)
    end
  end

  defp handle_supported_groups_option(value, version) when is_list(value) do
    {_Major, minor} = tls_version(version)

    try do
      :tls_v1.groups(minor, value)
    catch
      :exit, _ ->
        throw({:error, {:options, {:supported_groups, value}}})

      :error, _ ->
        throw({:error, {:options, {:supported_groups, value}}})
    else
      groups ->
        r_supported_groups(supported_groups: groups)
    end
  end

  defp unexpected_format(error) do
    :lists.flatten(:io_lib.format('Unexpected error: ~p', [error]))
  end

  defp file_error_format({:error, error}) do
    case :file.format_error(error) do
      'unknown POSIX error' ->
        'decoding error'

      str ->
        str
    end
  end

  defp file_error_format(_) do
    'decoding error'
  end

  defp file_desc(:cacertfile) do
    'Invalid CA certificate file '
  end

  defp file_desc(:certfile) do
    'Invalid certificate file '
  end

  defp file_desc(:keyfile) do
    'Invalid key file '
  end

  defp file_desc(:dhfile) do
    'Invalid DH params file '
  end

  defp detect(_Pred, []) do
    :undefined
  end

  defp detect(pred, [h | t]) do
    case pred.(h) do
      true ->
        h

      _ ->
        detect(pred, t)
    end
  end

  defp make_next_protocol_selector(:undefined) do
    :undefined
  end

  defp make_next_protocol_selector({:client, allProtocols, defaultProtocol}) do
    fn advertisedProtocols ->
      case detect(
             fn preferredProtocol ->
               :lists.member(preferredProtocol, advertisedProtocols)
             end,
             allProtocols
           ) do
        :undefined ->
          defaultProtocol

        preferredProtocol ->
          preferredProtocol
      end
    end
  end

  defp make_next_protocol_selector({:server, allProtocols, defaultProtocol}) do
    fn advertisedProtocols ->
      case detect(
             fn preferredProtocol ->
               :lists.member(preferredProtocol, allProtocols)
             end,
             advertisedProtocols
           ) do
        :undefined ->
          defaultProtocol

        preferredProtocol ->
          preferredProtocol
      end
    end
  end

  defp connection_cb(:tls) do
    :tls_connection
  end

  defp connection_cb(:dtls) do
    :dtls_connection
  end

  defp connection_cb(opts) do
    connection_cb(:proplists.get_value(:protocol, opts, :tls))
  end

  defp record_cb(:tls) do
    :tls_record
  end

  defp record_cb(:dtls) do
    :dtls_record
  end

  defp record_cb(opts) do
    record_cb(:proplists.get_value(:protocol, opts, :tls))
  end

  defp binary_filename(fileName) do
    enc = :file.native_name_encoding()
    :unicode.characters_to_binary(fileName, :unicode, enc)
  end

  defp assert_proplist([]) do
    true
  end

  defp assert_proplist([{key, _} | rest]) when is_atom(key) do
    assert_proplist(rest)
  end

  defp assert_proplist([{:raw, _, _, _} | rest]) do
    assert_proplist(rest)
  end

  defp assert_proplist([:inet | rest]) do
    assert_proplist(rest)
  end

  defp assert_proplist([:inet6 | rest]) do
    assert_proplist(rest)
  end

  defp assert_proplist([value | _]) do
    throw({:option_not_a_key_value_tuple, value})
  end

  defp handle_verify_option(
         :verify_none,
         %{fail_if_no_peer_cert: false} = optionsMap
       ) do
    Map.put(optionsMap, :verify, :verify_none)
  end

  defp handle_verify_option(:verify_none, %{fail_if_no_peer_cert: true}) do
    throw(
      {:error, {:options, :incompatible, {:verify, :verify_none}, {:fail_if_no_peer_cert, true}}}
    )
  end

  defp handle_verify_option(
         :verify_peer,
         %{verify: :verify_none} = optionsMap
       ) do
    Map.merge(optionsMap, %{verify: :verify_peer, verify_fun: :undefined})
  end

  defp handle_verify_option(:verify_peer, optionsMap) do
    Map.put(optionsMap, :verify, :verify_peer)
  end

  defp handle_verify_option(value, _) do
    throw({:error, {:options, {:verify, value}}})
  end

  defp default_option_role_sign_algs(_, value, _, version) when version >= {3, 4} do
    value
  end

  defp default_option_role_sign_algs(role, value, role, _) do
    value
  end

  defp default_option_role_sign_algs(_, _, _, _) do
    :undefined
  end

  defp default_option_role(role, value, role) do
    value
  end

  defp default_option_role(_, _, _) do
    :undefined
  end

  defp default_cb_info(:tls) do
    {:gen_tcp, :tcp, :tcp_closed, :tcp_error, :tcp_passive}
  end

  defp default_cb_info(:dtls) do
    {:gen_udp, :udp, :udp_closed, :udp_error, :udp_passive}
  end

  defp include_security_info([]) do
    false
  end

  defp include_security_info([item | items]) do
    case :lists.member(
           item,
           [:client_random, :server_random, :master_secret]
         ) do
      true ->
        true

      false ->
        include_security_info(items)
    end
  end

  defp server_name_indication_default(host) when is_list(host) do
    :string.strip(host, :right, ?.)
  end

  defp server_name_indication_default(_) do
    :undefined
  end

  defp add_filter(:undefined, filters) do
    filters
  end

  defp add_filter(filter, filters) do
    [filter | filters]
  end
end
