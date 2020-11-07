defmodule :m_ssl_connection do
  use Bitwise
  import Kernel, except: [send: 2]
  require Record
  Record.defrecord(:r_sslsocket, :sslsocket, fd: nil, pid: nil)

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
    ocsp_stapling_state: %{:ocsp_stapling => false, :ocsp_expect => :no_staple}
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

  Record.defrecord(:r_alert, :alert,
    level: :undefined,
    description: :undefined,
    where: :undefined,
    role: :undefined,
    reason: :undefined
  )

  def connect(connection, host, port, socket, options, user, cbInfo, timeout) do
    try do
      connection.start_fsm(:client, host, port, socket, options, user, cbInfo, timeout)
    catch
      :exit, {:noproc, _} ->
        {:error, :ssl_not_started}
    end
  end

  def handshake(connection, port, socket, opts, user, cbInfo, timeout) do
    try do
      connection.start_fsm(:server, 'localhost', port, socket, opts, user, cbInfo, timeout)
    catch
      :exit, {:noproc, _} ->
        {:error, :ssl_not_started}
    end
  end

  def handshake(r_sslsocket(pid: [pid | _]) = socket, timeout) do
    case call(pid, {:start, timeout}) do
      :connected ->
        {:ok, socket}

      {:ok, ext} ->
        {:ok, socket, no_records(ext)}

      error ->
        error
    end
  end

  def handshake(r_sslsocket(pid: [pid | _]) = socket, sslOptions, timeout) do
    case call(pid, {:start, sslOptions, timeout}) do
      :connected ->
        {:ok, socket}

      error ->
        error
    end
  end

  def handshake_continue(r_sslsocket(pid: [pid | _]) = socket, sslOptions, timeout) do
    case call(
           pid,
           {:handshake_continue, sslOptions, timeout}
         ) do
      :connected ->
        {:ok, socket}

      error ->
        error
    end
  end

  def handshake_cancel(r_sslsocket(pid: [pid | _])) do
    case call(pid, :cancel) do
      :closed ->
        :ok

      error ->
        error
    end
  end

  def socket_control(connection, socket, pid, transport) do
    socket_control(connection, socket, pid, transport, :undefined)
  end

  def socket_control(connection, socket, pids, transport, :udp_listener) do
    {:ok, connection.socket(pids, transport, socket, :undefined)}
  end

  def socket_control(:tls_connection = connection, socket, [pid | _] = pids, transport, trackers) do
    case transport.controlling_process(socket, pid) do
      :ok ->
        {:ok, connection.socket(pids, transport, socket, trackers)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  def socket_control(
        :dtls_connection = connection,
        {peerAddrPort, socket},
        [pid | _] = pids,
        transport,
        trackers
      ) do
    case transport.controlling_process(socket, pid) do
      :ok ->
        {:ok, connection.socket(pids, transport, {peerAddrPort, socket}, trackers)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  def send(pid, data) do
    call(
      pid,
      {:application_data, :erlang.iolist_to_iovec(data)}
    )
  end

  def recv(pid, length, timeout) do
    call(pid, {:recv, length, timeout})
  end

  def connection_information(pid, includeSecrityInfo) when is_pid(pid) do
    call(pid, {:connection_information, includeSecrityInfo})
  end

  def close(connectionPid, how) do
    case call(connectionPid, how) do
      {:error, :closed} ->
        :ok

      other ->
        other
    end
  end

  def shutdown(connectionPid, how) do
    call(connectionPid, {:shutdown, how})
  end

  def new_user(connectionPid, user) do
    call(connectionPid, {:new_user, user})
  end

  def negotiated_protocol(connectionPid) do
    call(connectionPid, :negotiated_protocol)
  end

  def get_opts(connectionPid, optTags) do
    call(connectionPid, {:get_opts, optTags})
  end

  def set_opts(connectionPid, options) do
    call(connectionPid, {:set_opts, options})
  end

  def peer_certificate(connectionPid) do
    call(connectionPid, :peer_certificate)
  end

  def renegotiation(connectionPid) do
    call(connectionPid, :renegotiate)
  end

  def internal_renegotiation(
        connectionPid,
        %{:current_write => writeState}
      ) do
    :gen_statem.cast(
      connectionPid,
      {:internal_renegotiate, writeState}
    )
  end

  def dist_handshake_complete(connectionPid, dHandle) do
    :gen_statem.cast(
      connectionPid,
      {:dist_handshake_complete, dHandle}
    )
  end

  def prf(connectionPid, secret, label, seed, wantedLength) do
    call(
      connectionPid,
      {:prf, secret, label, seed, wantedLength}
    )
  end

  def handle_own_alert(
        alert0,
        _,
        stateName,
        r_state(
          static_env: r_static_env(role: role, protocol_cb: connection),
          ssl_options: %{:log_level => logLevel}
        ) = state
      ) do
    try do
      send_alert(alert0, stateName, state)
    catch
      _, _ ->
        :ignore
    end

    try do
      alert = r_alert(alert0, role: role)
      log_alert(logLevel, role, connection.protocol_name(), stateName, alert)
      handle_normal_shutdown(alert, stateName, state)
    catch
      _, _ ->
        :ok
    end

    {:stop, {:shutdown, :own_alert}, state}
  end

  def handle_normal_shutdown(
        alert,
        stateName,
        r_state(
          static_env:
            r_static_env(
              role: role,
              socket: socket,
              transport_cb: transport,
              protocol_cb: connection,
              trackers: trackers
            ),
          handshake_env: r_handshake_env(renegotiation: {false, :first}),
          start_or_recv_from: startFrom
        ) = state
      ) do
    pids = connection.pids(state)
    alert_user(pids, transport, trackers, socket, startFrom, alert, role, stateName, connection)
  end

  def handle_normal_shutdown(
        alert,
        stateName,
        r_state(
          static_env:
            r_static_env(
              role: role,
              socket: socket,
              transport_cb: transport,
              protocol_cb: connection,
              trackers: trackers
            ),
          connection_env: r_connection_env(user_application: {_Mon, pid}),
          socket_options: opts,
          start_or_recv_from: recvFrom
        ) = state
      ) do
    pids = connection.pids(state)

    alert_user(
      pids,
      transport,
      trackers,
      socket,
      stateName,
      opts,
      pid,
      recvFrom,
      alert,
      role,
      stateName,
      connection
    )
  end

  def handle_alert(
        r_alert(level: 2) = alert0,
        stateName,
        r_state(
          static_env:
            r_static_env(
              role: role,
              socket: socket,
              host: host,
              port: port,
              trackers: trackers,
              transport_cb: transport,
              protocol_cb: connection
            ),
          connection_env: r_connection_env(user_application: {_Mon, pid}),
          ssl_options: %{:log_level => logLevel},
          start_or_recv_from: from,
          session: session,
          socket_options: opts
        ) = state
      ) do
    invalidate_session(role, host, port, session)
    alert = r_alert(alert0, role: opposite_role(role))
    log_alert(logLevel, role, connection.protocol_name(), stateName, alert)
    pids = connection.pids(state)

    alert_user(
      pids,
      transport,
      trackers,
      socket,
      stateName,
      opts,
      pid,
      from,
      alert,
      role,
      stateName,
      connection
    )

    {:stop, {:shutdown, :normal}, state}
  end

  def handle_alert(r_alert(level: 1, description: 0) = alert, :downgrade = stateName, state) do
    {:next_state, stateName, state, [{:next_event, :internal, alert}]}
  end

  def handle_alert(
        r_alert(level: 1, description: 0) = alert0,
        stateName,
        r_state(static_env: r_static_env(role: role)) = state
      ) do
    alert = r_alert(alert0, role: opposite_role(role))
    handle_normal_shutdown(alert, stateName, state)
    {:stop, {:shutdown, :peer_close}, state}
  end

  def handle_alert(
        r_alert(level: 1, description: 100) = alert0,
        stateName,
        r_state(
          static_env: r_static_env(role: role, protocol_cb: connection),
          handshake_env: r_handshake_env(renegotiation: {true, :internal}),
          ssl_options: %{:log_level => logLevel}
        ) = state
      ) do
    alert = r_alert(alert0, role: opposite_role(role))
    log_alert(logLevel, role, connection.protocol_name(), stateName, alert)
    handle_normal_shutdown(alert, stateName, state)
    {:stop, {:shutdown, :peer_close}, state}
  end

  def handle_alert(
        r_alert(level: 1, description: 100) = alert,
        :connection = stateName,
        r_state(
          static_env: r_static_env(role: role, protocol_cb: connection),
          handshake_env: r_handshake_env(renegotiation: {true, from}) = hsEnv,
          ssl_options: %{:log_level => logLevel}
        ) = state0
      ) do
    log_alert(
      logLevel,
      role,
      connection.protocol_name(),
      stateName,
      r_alert(alert, role: opposite_role(role))
    )

    :gen_statem.reply(
      from,
      {:error, :renegotiation_rejected}
    )

    state = connection.reinit_handshake_data(state0)

    connection.next_event(
      :connection,
      :no_record,
      r_state(state, handshake_env: r_handshake_env(hsEnv, renegotiation: :undefined))
    )
  end

  def handle_alert(
        r_alert(level: 1, description: 100) = alert,
        stateName,
        r_state(
          static_env: r_static_env(role: role, protocol_cb: connection),
          handshake_env: r_handshake_env(renegotiation: {true, from}) = hsEnv,
          ssl_options: %{:log_level => logLevel}
        ) = state0
      ) do
    log_alert(
      logLevel,
      role,
      connection.protocol_name(),
      stateName,
      r_alert(alert, role: opposite_role(role))
    )

    :gen_statem.reply(
      from,
      {:error, :renegotiation_rejected}
    )

    state =
      connection.reinit(
        r_state(state0, handshake_env: r_handshake_env(hsEnv, renegotiation: :undefined))
      )

    connection.next_event(:connection, :no_record, state)
  end

  def handle_alert(
        r_alert(level: 1) = alert,
        stateName,
        r_state(
          static_env: r_static_env(role: role, protocol_cb: connection),
          ssl_options: %{:log_level => logLevel}
        ) = state
      ) do
    log_alert(
      logLevel,
      role,
      connection.protocol_name(),
      stateName,
      r_alert(alert, role: opposite_role(role))
    )

    connection.next_event(stateName, :no_record, state)
  end

  def maybe_invalidate_session(:undefined, _, _, _, _, _) do
    :ok
  end

  def maybe_invalidate_session({3, 4}, _, _, _, _, _) do
    :ok
  end

  def maybe_invalidate_session({3, n}, type, role, host, port, session)
      when n < 4 do
    maybe_invalidate_session(type, role, host, port, session)
  end

  defp passive_receive(
         r_state(
           user_data_buffer: {front, bufferSize, rear},
           connection_env: r_connection_env(erl_dist_handle: :undefined)
         ) = state0,
         stateName,
         connection,
         startTimerAction
       ) do
    case bufferSize do
      0 ->
        connection.next_event(stateName, :no_record, state0, startTimerAction)

      _ ->
        case read_application_data(state0, front, bufferSize, rear) do
          {:stop, _, _} = shutdownError ->
            shutdownError

          {record, state} ->
            case r_state(state, :start_or_recv_from) do
              :undefined ->
                connection.next_event(stateName, record, state, [
                  {{:timeout, :recv}, :infinity, :timeout}
                ])

              _ ->
                connection.next_event(stateName, record, state, startTimerAction)
            end
        end
    end
  end

  def read_application_data(
        data,
        r_state(
          user_data_buffer: {front0, bufferSize0, rear0},
          connection_env: r_connection_env(erl_dist_handle: dHandle)
        ) = state
      ) do
    front = front0
    bufferSize = bufferSize0 + byte_size(data)
    rear = [data | rear0]

    case dHandle do
      :undefined ->
        read_application_data(state, front, bufferSize, rear)

      _ ->
        try do
          read_application_dist_data(dHandle, front, bufferSize, rear)
        catch
          :error, _ ->
            {:stop, :disconnect, r_state(state, user_data_buffer: {front, bufferSize, rear})}
        else
          buffer ->
            {:no_record, r_state(state, user_data_buffer: buffer)}
        end
    end
  end

  defp read_application_data(
         r_state(
           socket_options: socketOpts,
           bytes_to_read: bytesToRead,
           start_or_recv_from: recvFrom
         ) = state,
         front,
         bufferSize,
         rear
       ) do
    read_application_data(state, front, bufferSize, rear, socketOpts, recvFrom, bytesToRead)
  end

  defp read_application_data(
         state,
         [bin | front],
         bufferSize,
         rear,
         socketOpts,
         recvFrom,
         bytesToRead
       ) do
    read_application_data_bin(
      state,
      front,
      bufferSize,
      rear,
      socketOpts,
      recvFrom,
      bytesToRead,
      bin
    )
  end

  defp read_application_data(
         state,
         [] = front,
         bufferSize,
         [] = rear,
         socketOpts,
         recvFrom,
         bytesToRead
       ) do
    0 = bufferSize

    {:no_record,
     r_state(state,
       socket_options: socketOpts,
       bytes_to_read: bytesToRead,
       start_or_recv_from: recvFrom,
       user_data_buffer: {front, bufferSize, rear}
     )}
  end

  defp read_application_data(state, [], bufferSize, rear, socketOpts, recvFrom, bytesToRead) do
    [bin | front] = :lists.reverse(rear)

    read_application_data_bin(
      state,
      front,
      bufferSize,
      [],
      socketOpts,
      recvFrom,
      bytesToRead,
      bin
    )
  end

  defp read_application_data_bin(
         state,
         front,
         bufferSize,
         rear,
         socketOpts,
         recvFrom,
         bytesToRead,
         <<>>
       ) do
    read_application_data(state, front, bufferSize, rear, socketOpts, recvFrom, bytesToRead)
  end

  defp read_application_data_bin(
         state,
         front0,
         bufferSize0,
         rear0,
         socketOpts0,
         recvFrom,
         bytesToRead,
         bin0
       ) do
    case get_data(socketOpts0, bytesToRead, bin0) do
      {:ok, data, bin} ->
        bufferSize = bufferSize0 - (byte_size(bin0) - byte_size(bin))

        read_application_data_deliver(
          state,
          [bin | front0],
          bufferSize,
          rear0,
          socketOpts0,
          recvFrom,
          data
        )

      {:more, :undefined} ->
        cond do
          byte_size(bin0) < bufferSize0 ->
            bin =
              :erlang.iolist_to_binary([
                [bin0, front0]
                | :lists.reverse(rear0)
              ])

            read_application_data_bin(
              state,
              [],
              bufferSize0,
              [],
              socketOpts0,
              recvFrom,
              bytesToRead,
              bin
            )

          true ->
            {:no_record,
             r_state(state,
               socket_options: socketOpts0,
               bytes_to_read: bytesToRead,
               start_or_recv_from: recvFrom,
               user_data_buffer: {[bin0 | front0], bufferSize0, rear0}
             )}
        end

      {:more, size} when size <= bufferSize0 ->
        {data, front, rear} = iovec_from_front(size - byte_size(bin0), front0, rear0, [bin0])
        bin = :erlang.iolist_to_binary(data)

        read_application_data_bin(
          state,
          front,
          bufferSize0,
          rear,
          socketOpts0,
          recvFrom,
          bytesToRead,
          bin
        )

      {:more, _Size} ->
        {:no_record,
         r_state(state,
           socket_options: socketOpts0,
           bytes_to_read: bytesToRead,
           start_or_recv_from: recvFrom,
           user_data_buffer: {[bin0 | front0], bufferSize0, rear0}
         )}

      :passive ->
        {:no_record,
         r_state(state,
           socket_options: socketOpts0,
           bytes_to_read: bytesToRead,
           start_or_recv_from: recvFrom,
           user_data_buffer: {[bin0 | front0], bufferSize0, rear0}
         )}

      {:error, _Reason} ->
        r_state(
          static_env:
            r_static_env(
              socket: socket,
              protocol_cb: connection,
              transport_cb: transport,
              trackers: trackers
            ),
          connection_env: r_connection_env(user_application: {_Mon, pid})
        ) = state

        buffer =
          :erlang.iolist_to_binary([
            [bin0, front0]
            | :lists.reverse(rear0)
          ])

        deliver_packet_error(
          connection.pids(state),
          transport,
          socket,
          socketOpts0,
          buffer,
          pid,
          recvFrom,
          trackers,
          connection
        )

        {:stop, {:shutdown, :normal},
         r_state(state,
           socket_options: socketOpts0,
           bytes_to_read: bytesToRead,
           start_or_recv_from: recvFrom,
           user_data_buffer: {[buffer], bufferSize0, []}
         )}
    end
  end

  defp read_application_data_deliver(state, front, bufferSize, rear, socketOpts0, recvFrom, data) do
    r_state(
      static_env:
        r_static_env(
          socket: socket,
          protocol_cb: connection,
          transport_cb: transport,
          trackers: trackers
        ),
      connection_env: r_connection_env(user_application: {_Mon, pid})
    ) = state

    socketOpts =
      deliver_app_data(
        connection.pids(state),
        transport,
        socket,
        socketOpts0,
        data,
        pid,
        recvFrom,
        trackers,
        connection
      )

    cond do
      r_socket_options(socketOpts, :active) === false ->
        {:no_record,
         r_state(state,
           user_data_buffer: {front, bufferSize, rear},
           start_or_recv_from: :undefined,
           bytes_to_read: :undefined,
           socket_options: socketOpts
         )}

      true ->
        read_application_data(state, front, bufferSize, rear, socketOpts, :undefined, :undefined)
    end
  end

  defp read_application_dist_data(dHandle, [bin | front], bufferSize, rear) do
    read_application_dist_data(dHandle, front, bufferSize, rear, bin)
  end

  defp read_application_dist_data(_DHandle, [] = front, bufferSize, [] = rear) do
    ^bufferSize = 0
    {front, bufferSize, rear}
  end

  defp read_application_dist_data(dHandle, [], bufferSize, rear) do
    [bin | front] = :lists.reverse(rear)
    read_application_dist_data(dHandle, front, bufferSize, [], bin)
  end

  defp read_application_dist_data(dHandle, front0, bufferSize, rear0, bin0) do
    case bin0 do
      <<sizeA::size(32), dataA::size(sizeA)-binary, sizeB::size(32), dataB::size(sizeB)-binary,
        sizeC::size(32), dataC::size(sizeC)-binary, sizeD::size(32), dataD::size(sizeD)-binary,
        rest::binary>>
      when 0 < sizeA and 0 < sizeB and 0 < sizeC and
             0 < sizeD ->
        :erlang.dist_ctrl_put_data(dHandle, dataA)
        :erlang.dist_ctrl_put_data(dHandle, dataB)
        :erlang.dist_ctrl_put_data(dHandle, dataC)
        :erlang.dist_ctrl_put_data(dHandle, dataD)

        read_application_dist_data(
          dHandle,
          front0,
          bufferSize - (4 * 4 + sizeA + sizeB + sizeC + sizeD),
          rear0,
          rest
        )

      <<sizeA::size(32), dataA::size(sizeA)-binary, sizeB::size(32), dataB::size(sizeB)-binary,
        sizeC::size(32), dataC::size(sizeC)-binary, rest::binary>>
      when 0 < sizeA and 0 < sizeB and 0 < sizeC ->
        :erlang.dist_ctrl_put_data(dHandle, dataA)
        :erlang.dist_ctrl_put_data(dHandle, dataB)
        :erlang.dist_ctrl_put_data(dHandle, dataC)

        read_application_dist_data(
          dHandle,
          front0,
          bufferSize - (3 * 4 + sizeA + sizeB + sizeC),
          rear0,
          rest
        )

      <<sizeA::size(32), dataA::size(sizeA)-binary, sizeB::size(32), dataB::size(sizeB)-binary,
        rest::binary>>
      when 0 < sizeA and 0 < sizeB ->
        :erlang.dist_ctrl_put_data(dHandle, dataA)
        :erlang.dist_ctrl_put_data(dHandle, dataB)

        read_application_dist_data(
          dHandle,
          front0,
          bufferSize - (2 * 4 + sizeA + sizeB),
          rear0,
          rest
        )

      <<size::size(32), data::size(size)-binary, rest::binary>> ->
        0 < size and :erlang.dist_ctrl_put_data(dHandle, data)
        read_application_dist_data(dHandle, front0, bufferSize - (4 + size), rear0, rest)

      <<size::size(32), firstData::binary>>
      when 4 + size <= bufferSize ->
        {data, front, rear} =
          iovec_from_front(size - byte_size(firstData), front0, rear0, [firstData])

        0 < size and :erlang.dist_ctrl_put_data(dHandle, data)
        read_application_dist_data(dHandle, front, bufferSize - (4 + size), rear)

      <<bin::binary>> ->
        case bin do
          <<_Size::size(32), _InsufficientData::binary>> ->
            {[bin | front0], bufferSize, rear0}

          <<incompleteLengthField::binary>> when 4 < bufferSize ->
            {lengthField, front, rear} =
              case incompleteLengthField do
                <<>> ->
                  iovec_from_front(4, front0, rear0, [])

                _ ->
                  iovec_from_front(4 - byte_size(incompleteLengthField), front0, rear0, [
                    incompleteLengthField
                  ])
              end

            lengthBin = :erlang.iolist_to_binary(lengthField)
            read_application_dist_data(dHandle, front, bufferSize, rear, lengthBin)

          <<incompleteLengthField::binary>> ->
            case incompleteLengthField do
              <<>> ->
                {front0, bufferSize, rear0}

              _ ->
                {[incompleteLengthField | front0], bufferSize, rear0}
            end
        end
    end
  end

  defp iovec_from_front(0, front, rear, acc) do
    {:lists.reverse(acc), front, rear}
  end

  defp iovec_from_front(size, [], rear, acc) do
    case rear do
      [_] ->
        iovec_from_front(size, rear, [], acc)

      [bin2, bin1] ->
        iovec_from_front(size, [bin1, bin2], [], acc)

      [bin3, bin2, bin1] ->
        iovec_from_front(size, [bin1, bin2, bin3], [], acc)

      [[_, _, _] | _] = ^rear ->
        iovec_from_front(size, :lists.reverse(rear), [], acc)
    end
  end

  defp iovec_from_front(size, [bin | front], rear, []) do
    case bin do
      <<last::size(size)-binary>> ->
        {[last], front, rear}

      <<last::size(size)-binary, rest::binary>> ->
        {[last], [rest | front], rear}

      <<>> ->
        iovec_from_front(size, front, rear, [])

      <<_::binary>> ->
        binSize = byte_size(bin)
        iovec_from_front(size - binSize, front, rear, [bin])
    end
  end

  defp iovec_from_front(size, [bin | front], rear, acc) do
    case bin do
      <<last::size(size)-binary>> ->
        {:lists.reverse(acc, [last]), front, rear}

      <<last::size(size)-binary, rest::binary>> ->
        {:lists.reverse(acc, [last]), [rest | front], rear}

      <<>> ->
        iovec_from_front(size, front, rear, acc)

      <<_::binary>> ->
        binSize = byte_size(bin)
        iovec_from_front(size - binSize, front, rear, [bin | acc])
    end
  end

  def handle_session(
        r_server_hello(
          cipher_suite: cipherSuite,
          compression_method: compression
        ),
        version,
        newId,
        connectionStates,
        protoExt,
        protocol0,
        r_state(
          session: r_session(session_id: oldId),
          handshake_env: r_handshake_env(negotiated_protocol: currentProtocol) = hsEnv,
          connection_env: r_connection_env(negotiated_version: reqVersion) = cEnv
        ) = state0
      ) do
    %{:key_exchange => keyAlgorithm} = :ssl_cipher_format.suite_bin_to_map(cipherSuite)

    premasterSecret =
      make_premaster_secret(
        reqVersion,
        keyAlgorithm
      )

    {expectNPN, protocol} =
      case protocol0 do
        :undefined ->
          {false, currentProtocol}

        _ ->
          {protoExt === :npn, protocol0}
      end

    state =
      r_state(state0,
        connection_states: connectionStates,
        handshake_env:
          r_handshake_env(hsEnv,
            kex_algorithm: keyAlgorithm,
            premaster_secret: premasterSecret,
            expecting_next_protocol_negotiation: expectNPN,
            negotiated_protocol: protocol
          ),
        connection_env: r_connection_env(cEnv, negotiated_version: version)
      )

    case :ssl_session.is_new(oldId, newId) do
      true ->
        handle_new_session(
          newId,
          cipherSuite,
          compression,
          r_state(state, connection_states: connectionStates)
        )

      false ->
        handle_resumed_session(
          newId,
          r_state(state, connection_states: connectionStates)
        )
    end
  end

  def ssl_config(
        opts,
        role,
        r_state(static_env: initStatEnv0, handshake_env: hsEnv, connection_env: cEnv) = state0
      ) do
    {:ok,
     %{
       :cert_db_ref => ref,
       :cert_db_handle => certDbHandle,
       :fileref_db_handle => fileRefHandle,
       :session_cache => cacheHandle,
       :crl_db_info => cRLDbHandle,
       :private_key => key,
       :dh_params => dHParams,
       :own_certificate => ownCert
     }} =
      :ssl_config.init(
        opts,
        role
      )

    timeStamp = :erlang.monotonic_time()
    session = r_state(state0, :session)

    r_state(state0,
      session:
        r_session(session,
          own_certificate: ownCert,
          time_stamp: timeStamp
        ),
      static_env:
        r_static_env(initStatEnv0,
          file_ref_db: fileRefHandle,
          cert_db_ref: ref,
          cert_db: certDbHandle,
          crl_db: cRLDbHandle,
          session_cache: cacheHandle
        ),
      handshake_env: r_handshake_env(hsEnv, diffie_hellman_params: dHParams),
      connection_env: r_connection_env(cEnv, private_key: key),
      ssl_options: opts
    )
  end

  def init({:call, from}, {:start, timeout}, state0, connection) do
    connection.next_event(:hello, :no_record, r_state(state0, start_or_recv_from: from), [
      {{:timeout, :handshake}, timeout, :close}
    ])
  end

  def init(
        {:call, from},
        {:start, {opts, emOpts}, timeout},
        r_state(
          static_env: r_static_env(role: role),
          ssl_options: origSSLOptions,
          socket_options: sockOpts
        ) = state0,
        connection
      ) do
    try do
      sslOpts = :ssl.handle_options(opts, role, origSSLOptions)
      state = ssl_config(sslOpts, role, state0)

      init(
        {:call, from},
        {:start, timeout},
        r_state(state,
          ssl_options: sslOpts,
          socket_options: new_emulated(emOpts, sockOpts)
        ),
        connection
      )
    catch
      error ->
        {:stop_and_reply, {:shutdown, :normal}, {:reply, from, {:error, error}}, state0}
    end
  end

  def init({:call, from}, {:new_user, _} = msg, state, connection) do
    handle_call(msg, from, :init, state, connection)
  end

  def init({:call, from}, _Msg, _State, _Connection) do
    {:keep_state_and_data, [{:reply, from, {:error, :notsup_on_transport_accept_socket}}]}
  end

  def init(_Type, _Event, _State, _Connection) do
    {:keep_state_and_data, [:postpone]}
  end

  def error({:call, from}, {:close, _}, state, _Connection) do
    {:stop_and_reply, {:shutdown, :normal}, {:reply, from, :ok}, state}
  end

  def error({:call, from}, _Msg, state, _Connection) do
    {:next_state, :error, state, [{:reply, from, {:error, :closed}}]}
  end

  def hello({:call, from}, msg, state, connection) do
    handle_call(msg, from, :hello, state, connection)
  end

  def hello(:internal, {:common_client_hello, type, serverHelloExt}, state, connection) do
    do_server_hello(type, serverHelloExt, state, connection)
  end

  def hello(:info, msg, state, _) do
    handle_info(msg, :hello, state)
  end

  def hello(type, msg, state, connection) do
    handle_common_event(type, msg, :hello, state, connection)
  end

  def user_hello(
        {:call, from},
        :cancel,
        r_state(connection_env: r_connection_env(negotiated_version: version)) = state,
        _
      ) do
    :gen_statem.reply(from, :ok)

    handle_own_alert(
      r_alert(
        level: 2,
        description: 90,
        where: %{
          :mfa => {:ssl_connection, :user_hello, 4},
          :line => 881,
          :file => 'otp/lib/ssl/src/ssl_connection.erl'
        },
        reason: :user_canceled
      ),
      version,
      :user_hello,
      state
    )
  end

  def user_hello(
        {:call, from},
        {:handshake_continue, newOptions, timeout},
        r_state(
          static_env: r_static_env(role: role),
          handshake_env: r_handshake_env(hello: hello),
          ssl_options: options0
        ) = state0,
        _Connection
      ) do
    options = :ssl.handle_options(newOptions, role, %{options0 | :handshake => :full})
    state = ssl_config(options, role, state0)

    {:next_state, :hello, r_state(state, start_or_recv_from: from),
     [{:next_event, :internal, hello}, {{:timeout, :handshake}, timeout, :close}]}
  end

  def user_hello(_, _, _, _) do
    {:keep_state_and_data, [:postpone]}
  end

  def abbreviated({:call, from}, msg, state, connection) do
    handle_call(msg, from, :abbreviated, state, connection)
  end

  def abbreviated(
        :internal,
        r_finished(verify_data: data) = finished,
        r_state(
          static_env: r_static_env(role: :server),
          handshake_env:
            r_handshake_env(
              tls_handshake_history: hist,
              expecting_finished: true
            ) = hsEnv,
          connection_env: r_connection_env(negotiated_version: version),
          session: r_session(master_secret: masterSecret),
          connection_states: connectionStates0
        ) = state0,
        connection
      ) do
    case :ssl_handshake.verify_connection(
           :ssl.tls_version(version),
           finished,
           :client,
           get_current_prf(
             connectionStates0,
             :write
           ),
           masterSecret,
           hist
         ) do
      :verified ->
        connectionStates =
          :ssl_record.set_client_verify_data(
            :current_both,
            data,
            connectionStates0
          )

        {record, state} =
          prepare_connection(
            r_state(state0,
              connection_states: connectionStates,
              handshake_env: r_handshake_env(hsEnv, expecting_finished: false)
            ),
            connection
          )

        connection.next_event(:connection, record, state, [
          {{:timeout, :handshake}, :infinity, :close}
        ])

      r_alert() = alert ->
        handle_own_alert(alert, version, :abbreviated, state0)
    end
  end

  def abbreviated(
        :internal,
        r_finished(verify_data: data) = finished,
        r_state(
          static_env: r_static_env(role: :client),
          handshake_env: r_handshake_env(tls_handshake_history: hist0),
          connection_env: r_connection_env(negotiated_version: version),
          session: r_session(master_secret: masterSecret),
          connection_states: connectionStates0
        ) = state0,
        connection
      ) do
    case :ssl_handshake.verify_connection(
           :ssl.tls_version(version),
           finished,
           :server,
           get_pending_prf(
             connectionStates0,
             :write
           ),
           masterSecret,
           hist0
         ) do
      :verified ->
        connectionStates1 =
          :ssl_record.set_server_verify_data(
            :current_read,
            data,
            connectionStates0
          )

        {r_state(handshake_env: hsEnv) = state1, actions} =
          finalize_handshake(
            r_state(state0, connection_states: connectionStates1),
            :abbreviated,
            connection
          )

        {record, state} =
          prepare_connection(
            r_state(state1, handshake_env: r_handshake_env(hsEnv, expecting_finished: false)),
            connection
          )

        connection.next_event(:connection, record, state, [
          {{:timeout, :handshake}, :infinity, :close}
          | actions
        ])

      r_alert() = alert ->
        handle_own_alert(alert, version, :abbreviated, state0)
    end
  end

  def abbreviated(
        :internal,
        r_next_protocol(selected_protocol: selectedProtocol),
        r_state(
          static_env: r_static_env(role: :server),
          handshake_env: r_handshake_env(expecting_next_protocol_negotiation: true) = hsEnv
        ) = state,
        connection
      ) do
    connection.next_event(
      :abbreviated,
      :no_record,
      r_state(state,
        handshake_env:
          r_handshake_env(hsEnv,
            negotiated_protocol: selectedProtocol,
            expecting_next_protocol_negotiation: false
          )
      )
    )
  end

  def abbreviated(
        :internal,
        r_change_cipher_spec(type: <<1>>),
        r_state(
          connection_states: connectionStates0,
          handshake_env: hsEnv
        ) = state,
        connection
      ) do
    connectionStates1 =
      :ssl_record.activate_pending_connection_state(
        connectionStates0,
        :read,
        connection
      )

    connection.next_event(
      :abbreviated,
      :no_record,
      r_state(state,
        connection_states: connectionStates1,
        handshake_env: r_handshake_env(hsEnv, expecting_finished: true)
      )
    )
  end

  def abbreviated(:info, msg, state, _) do
    handle_info(msg, :abbreviated, state)
  end

  def abbreviated(type, msg, state, connection) do
    handle_common_event(type, msg, :abbreviated, state, connection)
  end

  def wait_ocsp_stapling(:internal, r_certificate(), state, connection) do
    connection.next_event(:wait_ocsp_stapling, :no_record, state, [{:postpone, true}])
  end

  def wait_ocsp_stapling(
        :internal,
        r_certificate_status() = certStatus,
        r_state(handshake_env: r_handshake_env(ocsp_stapling_state: ocspState) = hsEnv) = state,
        connection
      ) do
    connection.next_event(
      :certify,
      :no_record,
      r_state(state,
        handshake_env:
          r_handshake_env(hsEnv,
            ocsp_stapling_state: %{
              ocspState
              | :ocsp_expect => :stapled,
                :ocsp_response => certStatus
            }
          )
      )
    )
  end

  def wait_ocsp_stapling(
        :internal,
        msg,
        r_state(handshake_env: r_handshake_env(ocsp_stapling_state: ocspState) = hsEnv) = state,
        connection
      )
      when elem(msg, 0) === :server_key_exchange or elem(msg, 0) === :hello_request or
             elem(msg, 0) === :certificate_request or elem(msg, 0) === :server_hello_done or
             elem(msg, 0) === :client_key_exchange do
    connection.next_event(
      :certify,
      :no_record,
      r_state(state,
        handshake_env:
          r_handshake_env(hsEnv, ocsp_stapling_state: %{ocspState | :ocsp_expect => :undetermined})
      ),
      [{:postpone, true}]
    )
  end

  def wait_ocsp_stapling(type, msg, state, connection) do
    handle_common_event(type, msg, :wait_ocsp_stapling, state, connection)
  end

  def certify({:call, from}, msg, state, connection) do
    handle_call(msg, from, :certify, state, connection)
  end

  def certify(:info, msg, state, _) do
    handle_info(msg, :certify, state)
  end

  def certify(
        :internal,
        r_certificate(asn1_certificates: []),
        r_state(
          static_env: r_static_env(role: :server),
          connection_env: r_connection_env(negotiated_version: version),
          ssl_options: %{:verify => :verify_peer, :fail_if_no_peer_cert => true}
        ) = state,
        _
      ) do
    alert =
      r_alert(
        level: 2,
        description: 40,
        where: %{
          :mfa => {:ssl_connection, :certify, 4},
          :line => 1013,
          :file => 'otp/lib/ssl/src/ssl_connection.erl'
        },
        reason: :no_client_certificate_provided
      )

    handle_own_alert(alert, version, :certify, state)
  end

  def certify(
        :internal,
        r_certificate(asn1_certificates: []),
        r_state(
          static_env: r_static_env(role: :server),
          ssl_options: %{:verify => :verify_peer, :fail_if_no_peer_cert => false}
        ) = state0,
        connection
      ) do
    connection.next_event(
      :certify,
      :no_record,
      r_state(state0, client_certificate_requested: false)
    )
  end

  def certify(
        :internal,
        r_certificate(),
        r_state(
          static_env: r_static_env(role: :server),
          connection_env: r_connection_env(negotiated_version: version),
          ssl_options: %{:verify => :verify_none}
        ) = state,
        _
      ) do
    alert =
      r_alert(
        level: 2,
        description: 10,
        where: %{
          :mfa => {:ssl_connection, :certify, 4},
          :line => 1026,
          :file => 'otp/lib/ssl/src/ssl_connection.erl'
        },
        reason: :unrequested_certificate
      )

    handle_own_alert(alert, version, :certify, state)
  end

  def certify(
        :internal,
        r_certificate(),
        r_state(handshake_env: r_handshake_env(ocsp_stapling_state: %{:ocsp_expect => :staple})) =
          state,
        connection
      ) do
    connection.next_event(:wait_ocsp_stapling, :no_record, state, [{:postpone, true}])
  end

  def certify(
        :internal,
        r_certificate(asn1_certificates: [peer | _]) = cert,
        r_state(
          static_env:
            r_static_env(
              role: role,
              host: host,
              cert_db: certDbHandle,
              cert_db_ref: certDbRef,
              crl_db: cRLDbInfo
            ),
          handshake_env:
            r_handshake_env(ocsp_stapling_state: %{:ocsp_expect => status} = ocspState),
          connection_env: r_connection_env(negotiated_version: version),
          ssl_options: opts
        ) = state,
        connection
      )
      when status !== :staple do
    ocspInfo = ocsp_info(ocspState, opts, peer)

    case :ssl_handshake.certify(
           cert,
           certDbHandle,
           certDbRef,
           opts,
           cRLDbInfo,
           role,
           host,
           ensure_tls(version),
           ocspInfo
         ) do
      {peerCert, publicKeyInfo} ->
        handle_peer_cert(
          role,
          peerCert,
          publicKeyInfo,
          r_state(state, client_certificate_requested: false),
          connection,
          []
        )

      r_alert() = alert ->
        handle_own_alert(alert, version, :certify, state)
    end
  end

  def certify(
        :internal,
        r_server_key_exchange(exchange_keys: keys),
        r_state(
          static_env: r_static_env(role: :client),
          handshake_env:
            r_handshake_env(
              kex_algorithm: kexAlg,
              public_key_info: pubKeyInfo
            ) = hsEnv,
          connection_env: r_connection_env(negotiated_version: version),
          session: session,
          connection_states: connectionStates
        ) = state,
        connection
      )
      when kexAlg == :dhe_dss or kexAlg == :dhe_rsa or
             kexAlg == :ecdhe_rsa or kexAlg == :ecdhe_ecdsa or
             kexAlg == :dh_anon or kexAlg == :ecdh_anon or
             kexAlg == :psk or kexAlg == :dhe_psk or
             kexAlg == :ecdhe_psk or kexAlg == :rsa_psk or
             kexAlg == :srp_dss or kexAlg == :srp_rsa or
             kexAlg == :srp_anon do
    params = :ssl_handshake.decode_server_key(keys, kexAlg, :ssl.tls_version(version))

    hashSign =
      negotiated_hashsign(
        r_server_key_params(params, :hashsign),
        kexAlg,
        pubKeyInfo,
        :ssl.tls_version(version)
      )

    case is_anonymous(kexAlg) do
      true ->
        calculate_secret(
          r_server_key_params(params, :params),
          r_state(state, handshake_env: r_handshake_env(hsEnv, hashsign_algorithm: hashSign)),
          connection
        )

      false ->
        case :ssl_handshake.verify_server_key(
               params,
               hashSign,
               connectionStates,
               :ssl.tls_version(version),
               pubKeyInfo
             ) do
          true ->
            calculate_secret(
              r_server_key_params(params, :params),
              r_state(state,
                handshake_env: r_handshake_env(hsEnv, hashsign_algorithm: hashSign),
                session:
                  session_handle_params(
                    r_server_key_params(params, :params),
                    session
                  )
              ),
              connection
            )

          false ->
            handle_own_alert(
              r_alert(
                level: 2,
                description: 51,
                where: %{
                  :mfa => {:ssl_connection, :certify, 4},
                  :line => 1093,
                  :file => 'otp/lib/ssl/src/ssl_connection.erl'
                }
              ),
              version,
              :certify,
              state
            )
        end
    end
  end

  def certify(
        :internal,
        r_certificate_request(),
        r_state(
          static_env: r_static_env(role: :client),
          handshake_env: r_handshake_env(kex_algorithm: kexAlg),
          connection_env: r_connection_env(negotiated_version: version)
        ) = state,
        _
      )
      when kexAlg == :dh_anon or kexAlg == :ecdh_anon or
             kexAlg == :psk or kexAlg == :dhe_psk or
             kexAlg == :ecdhe_psk or kexAlg == :rsa_psk or
             kexAlg == :srp_dss or kexAlg == :srp_rsa or
             kexAlg == :srp_anon do
    handle_own_alert(
      r_alert(
        level: 2,
        description: 40,
        where: %{
          :mfa => {:ssl_connection, :certify, 4},
          :line => 1110,
          :file => 'otp/lib/ssl/src/ssl_connection.erl'
        }
      ),
      version,
      :certify,
      state
    )
  end

  def certify(
        :internal,
        r_certificate_request(),
        r_state(
          static_env: r_static_env(role: :client),
          session: r_session(own_certificate: :undefined)
        ) = state,
        connection
      ) do
    connection.next_event(
      :certify,
      :no_record,
      r_state(state, client_certificate_requested: true)
    )
  end

  def certify(
        :internal,
        r_certificate_request() = certRequest,
        r_state(
          static_env: r_static_env(role: :client),
          handshake_env: hsEnv,
          connection_env: r_connection_env(negotiated_version: version),
          session: r_session(own_certificate: cert),
          ssl_options: %{:signature_algs => supportedHashSigns}
        ) = state,
        connection
      ) do
    case :ssl_handshake.select_hashsign(
           certRequest,
           cert,
           supportedHashSigns,
           :ssl.tls_version(version)
         ) do
      r_alert() = alert ->
        handle_own_alert(alert, version, :certify, state)

      negotiatedHashSign ->
        connection.next_event(
          :certify,
          :no_record,
          r_state(state,
            client_certificate_requested: true,
            handshake_env: r_handshake_env(hsEnv, cert_hashsign_algorithm: negotiatedHashSign)
          )
        )
    end
  end

  def certify(
        :internal,
        r_server_hello_done(),
        r_state(
          static_env: r_static_env(role: :client),
          session: r_session(master_secret: :undefined),
          connection_env: r_connection_env(negotiated_version: version),
          handshake_env:
            r_handshake_env(
              kex_algorithm: kexAlg,
              premaster_secret: :undefined,
              server_psk_identity: pSKIdentity
            ) = hsEnv,
          ssl_options: %{:user_lookup_fun => pSKLookup}
        ) = state0,
        connection
      )
      when kexAlg == :psk do
    case :ssl_handshake.premaster_secret(
           {kexAlg, pSKIdentity},
           pSKLookup
         ) do
      r_alert() = alert ->
        handle_own_alert(alert, version, :certify, state0)

      premasterSecret ->
        state =
          master_secret(
            premasterSecret,
            r_state(state0,
              handshake_env: r_handshake_env(hsEnv, premaster_secret: premasterSecret)
            )
          )

        client_certify_and_key_exchange(state, connection)
    end
  end

  def certify(
        :internal,
        r_server_hello_done(),
        r_state(
          static_env: r_static_env(role: :client),
          connection_env: r_connection_env(negotiated_version: {major, minor}) = version,
          handshake_env:
            r_handshake_env(
              kex_algorithm: kexAlg,
              premaster_secret: :undefined,
              server_psk_identity: pSKIdentity
            ) = hsEnv,
          session: r_session(master_secret: :undefined),
          ssl_options: %{:user_lookup_fun => pSKLookup}
        ) = state0,
        connection
      )
      when kexAlg == :rsa_psk do
    rand = :ssl_cipher.random_bytes(48 - 2)

    rSAPremasterSecret =
      <<major::size(8)-unsigned-big-integer, minor::size(8)-unsigned-big-integer, rand::binary>>

    case :ssl_handshake.premaster_secret({kexAlg, pSKIdentity}, pSKLookup, rSAPremasterSecret) do
      r_alert() = alert ->
        handle_own_alert(alert, version, :certify, state0)

      premasterSecret ->
        state =
          master_secret(
            premasterSecret,
            r_state(state0,
              handshake_env: r_handshake_env(hsEnv, premaster_secret: rSAPremasterSecret)
            )
          )

        client_certify_and_key_exchange(state, connection)
    end
  end

  def certify(
        :internal,
        r_server_hello_done(),
        r_state(
          static_env: r_static_env(role: :client),
          connection_env: r_connection_env(negotiated_version: version),
          handshake_env: r_handshake_env(premaster_secret: :undefined),
          session: r_session(master_secret: masterSecret) = session,
          connection_states: connectionStates0
        ) = state0,
        connection
      ) do
    case :ssl_handshake.master_secret(
           :ssl.tls_version(version),
           session,
           connectionStates0,
           :client
         ) do
      {^masterSecret, connectionStates} ->
        state = r_state(state0, connection_states: connectionStates)
        client_certify_and_key_exchange(state, connection)

      r_alert() = alert ->
        handle_own_alert(alert, version, :certify, state0)
    end
  end

  def certify(
        :internal,
        r_server_hello_done(),
        r_state(
          static_env: r_static_env(role: :client),
          connection_env: r_connection_env(negotiated_version: version),
          handshake_env: r_handshake_env(premaster_secret: premasterSecret),
          session: session0,
          connection_states: connectionStates0
        ) = state0,
        connection
      ) do
    case :ssl_handshake.master_secret(
           :ssl.tls_version(version),
           premasterSecret,
           connectionStates0,
           :client
         ) do
      {masterSecret, connectionStates} ->
        session = r_session(session0, master_secret: masterSecret)

        state =
          r_state(state0,
            connection_states: connectionStates,
            session: session
          )

        client_certify_and_key_exchange(state, connection)

      r_alert() = alert ->
        handle_own_alert(alert, version, :certify, state0)
    end
  end

  def certify(
        :internal = type,
        r_client_key_exchange() = msg,
        r_state(
          static_env: r_static_env(role: :server),
          client_certificate_requested: true,
          ssl_options: %{:fail_if_no_peer_cert => true}
        ) = state,
        connection
      ) do
    handle_common_event(type, msg, :certify, state, connection)
  end

  def certify(
        :internal,
        r_client_key_exchange(exchange_keys: keys),
        state =
          r_state(
            handshake_env: r_handshake_env(kex_algorithm: keyAlg),
            connection_env: r_connection_env(negotiated_version: version)
          ),
        connection
      ) do
    try do
      certify_client_key_exchange(
        :ssl_handshake.decode_client_key(
          keys,
          keyAlg,
          :ssl.tls_version(version)
        ),
        state,
        connection
      )
    catch
      r_alert() = alert ->
        handle_own_alert(alert, version, :certify, state)
    end
  end

  def certify(type, msg, state, connection) do
    handle_common_event(type, msg, :certify, state, connection)
  end

  def cipher({:call, from}, msg, state, connection) do
    handle_call(msg, from, :cipher, state, connection)
  end

  def cipher(:info, msg, state, _) do
    handle_info(msg, :cipher, state)
  end

  def cipher(
        :internal,
        r_certificate_verify(
          signature: signature,
          hashsign_algorithm: certHashSign
        ),
        r_state(
          static_env: r_static_env(role: :server),
          handshake_env:
            r_handshake_env(
              tls_handshake_history: hist,
              kex_algorithm: kexAlg,
              public_key_info: pubKeyInfo
            ) = hsEnv,
          connection_env: r_connection_env(negotiated_version: version),
          session: r_session(master_secret: masterSecret)
        ) = state,
        connection
      ) do
    tLSVersion = :ssl.tls_version(version)
    hashSign = negotiated_hashsign(certHashSign, kexAlg, pubKeyInfo, tLSVersion)

    case :ssl_handshake.certificate_verify(
           signature,
           pubKeyInfo,
           tLSVersion,
           hashSign,
           masterSecret,
           hist
         ) do
      :valid ->
        connection.next_event(
          :cipher,
          :no_record,
          r_state(state, handshake_env: r_handshake_env(hsEnv, cert_hashsign_algorithm: hashSign))
        )

      r_alert() = alert ->
        handle_own_alert(alert, version, :cipher, state)
    end
  end

  def cipher(
        :internal,
        r_finished(),
        r_state(
          static_env: r_static_env(role: :server),
          handshake_env:
            r_handshake_env(
              expecting_next_protocol_negotiation: true,
              negotiated_protocol: :undefined
            ),
          connection_env: r_connection_env(negotiated_version: version)
        ) = state0,
        _Connection
      ) do
    handle_own_alert(
      r_alert(
        level: 2,
        description: 10,
        where: %{
          :mfa => {:ssl_connection, :cipher, 4},
          :line => 1264,
          :file => 'otp/lib/ssl/src/ssl_connection.erl'
        }
      ),
      version,
      :cipher,
      state0
    )
  end

  def cipher(
        :internal,
        r_finished(verify_data: data) = finished,
        r_state(
          static_env: r_static_env(role: role, host: host, port: port, trackers: trackers),
          handshake_env:
            r_handshake_env(
              tls_handshake_history: hist,
              expecting_finished: true
            ) = hsEnv,
          connection_env: r_connection_env(negotiated_version: version),
          session: r_session(master_secret: masterSecret) = session0,
          ssl_options: sslOpts,
          connection_states: connectionStates0
        ) = state,
        connection
      ) do
    case :ssl_handshake.verify_connection(
           :ssl.tls_version(version),
           finished,
           opposite_role(role),
           get_current_prf(
             connectionStates0,
             :read
           ),
           masterSecret,
           hist
         ) do
      :verified ->
        session = handle_session(role, sslOpts, host, port, trackers, session0)

        cipher_role(
          role,
          data,
          session,
          r_state(state, handshake_env: r_handshake_env(hsEnv, expecting_finished: false)),
          connection
        )

      r_alert() = alert ->
        handle_own_alert(alert, version, :cipher, state)
    end
  end

  def cipher(
        :internal,
        r_next_protocol(selected_protocol: selectedProtocol),
        r_state(
          static_env: r_static_env(role: :server),
          handshake_env:
            r_handshake_env(
              expecting_finished: true,
              expecting_next_protocol_negotiation: true
            ) = hsEnv
        ) = state,
        connection
      ) do
    connection.next_event(
      :cipher,
      :no_record,
      r_state(state,
        handshake_env:
          r_handshake_env(hsEnv,
            negotiated_protocol: selectedProtocol,
            expecting_next_protocol_negotiation: false
          )
      )
    )
  end

  def cipher(
        :internal,
        r_change_cipher_spec(type: <<1>>),
        r_state(
          handshake_env: hsEnv,
          connection_states: connectionStates0
        ) = state,
        connection
      ) do
    connectionStates =
      :ssl_record.activate_pending_connection_state(
        connectionStates0,
        :read,
        connection
      )

    connection.next_event(
      :cipher,
      :no_record,
      r_state(state,
        handshake_env: r_handshake_env(hsEnv, expecting_finished: true),
        connection_states: connectionStates
      )
    )
  end

  def cipher(type, msg, state, connection) do
    handle_common_event(type, msg, :cipher, state, connection)
  end

  def connection(
        {:call, recvFrom},
        {:recv, n, timeout},
        r_state(
          static_env: r_static_env(protocol_cb: connection),
          socket_options: r_socket_options(active: false)
        ) = state0,
        connection
      ) do
    passive_receive(
      r_state(state0,
        bytes_to_read: n,
        start_or_recv_from: recvFrom
      ),
      :connection,
      connection,
      [{{:timeout, :recv}, timeout, :timeout}]
    )
  end

  def connection(
        {:call, from},
        :renegotiate,
        r_state(
          static_env: r_static_env(protocol_cb: connection),
          handshake_env: hsEnv
        ) = state,
        connection
      ) do
    connection.renegotiate(
      r_state(state, handshake_env: r_handshake_env(hsEnv, renegotiation: {true, from})),
      []
    )
  end

  def connection(
        {:call, from},
        :peer_certificate,
        r_state(session: r_session(peer_certificate: cert)) = state,
        _
      ) do
    hibernate_after(:connection, state, [{:reply, from, {:ok, cert}}])
  end

  def connection({:call, from}, {:connection_information, true}, state, _) do
    info = connection_info(state) ++ security_info(state)
    hibernate_after(:connection, state, [{:reply, from, {:ok, info}}])
  end

  def connection({:call, from}, {:connection_information, false}, state, _) do
    info = connection_info(state)
    hibernate_after(:connection, state, [{:reply, from, {:ok, info}}])
  end

  def connection(
        {:call, from},
        :negotiated_protocol,
        r_state(
          handshake_env:
            r_handshake_env(
              alpn: :undefined,
              negotiated_protocol: :undefined
            )
        ) = state,
        _
      ) do
    hibernate_after(:connection, state, [{:reply, from, {:error, :protocol_not_negotiated}}])
  end

  def connection(
        {:call, from},
        :negotiated_protocol,
        r_state(
          handshake_env:
            r_handshake_env(
              alpn: :undefined,
              negotiated_protocol: selectedProtocol
            )
        ) = state,
        _
      ) do
    hibernate_after(:connection, state, [{:reply, from, {:ok, selectedProtocol}}])
  end

  def connection(
        {:call, from},
        :negotiated_protocol,
        r_state(
          handshake_env:
            r_handshake_env(
              alpn: selectedProtocol,
              negotiated_protocol: :undefined
            )
        ) = state,
        _
      ) do
    hibernate_after(:connection, state, [{:reply, from, {:ok, selectedProtocol}}])
  end

  def connection({:call, from}, msg, state, connection) do
    handle_call(msg, from, :connection, state, connection)
  end

  def connection(
        :cast,
        {:internal_renegotiate, writeState},
        r_state(
          static_env: r_static_env(protocol_cb: connection),
          handshake_env: hsEnv,
          connection_states: connectionStates
        ) = state,
        connection
      ) do
    connection.renegotiate(
      r_state(state,
        handshake_env: r_handshake_env(hsEnv, renegotiation: {true, :internal}),
        connection_states: %{connectionStates | :current_write => writeState}
      ),
      []
    )
  end

  def connection(
        :cast,
        {:dist_handshake_complete, dHandle},
        r_state(
          ssl_options: %{:erl_dist => true},
          connection_env: cEnv,
          socket_options: sockOpts
        ) = state0,
        connection
      ) do
    :erlang.process_flag(:priority, :normal)

    state1 =
      r_state(state0,
        socket_options: r_socket_options(sockOpts, active: true),
        connection_env: r_connection_env(cEnv, erl_dist_handle: dHandle),
        bytes_to_read: :undefined
      )

    {record, state} = read_application_data(<<>>, state1)
    connection.next_event(:connection, record, state)
  end

  def connection(:info, msg, state, _) do
    handle_info(msg, :connection, state)
  end

  def connection(
        :internal,
        {:recv, recvFrom},
        r_state(start_or_recv_from: recvFrom) = state,
        connection
      ) do
    passive_receive(state, :connection, connection, [])
  end

  def connection(type, msg, state, connection) do
    handle_common_event(type, msg, :connection, state, connection)
  end

  def downgrade(type, event, state, connection) do
    handle_common_event(type, event, :downgrade, state, connection)
  end

  def handle_common_event(
        :internal,
        {:handshake, {r_hello_request() = handshake, _}},
        :connection = stateName,
        r_state(
          static_env: r_static_env(role: :client),
          handshake_env: hsEnv
        ) = state,
        _
      ) do
    {:next_state, stateName,
     r_state(state, handshake_env: r_handshake_env(hsEnv, renegotiation: {true, :peer})),
     [{:next_event, :internal, handshake}]}
  end

  def handle_common_event(
        :internal,
        {:handshake, {r_hello_request(), _}},
        stateName,
        r_state(static_env: r_static_env(role: :client)),
        _
      )
      when stateName !== :connection do
    :keep_state_and_data
  end

  def handle_common_event(
        :internal,
        {:handshake, {handshake, raw}},
        stateName,
        r_state(
          handshake_env: r_handshake_env(tls_handshake_history: hist0) = hsEnv,
          connection_env: r_connection_env(negotiated_version: _Version)
        ) = state0,
        _Connection
      ) do
    hist =
      :ssl_handshake.update_handshake_history(
        hist0,
        raw
      )

    {:next_state, stateName,
     r_state(state0, handshake_env: r_handshake_env(hsEnv, tls_handshake_history: hist)),
     [{:next_event, :internal, handshake}]}
  end

  def handle_common_event(
        :internal,
        {:protocol_record, tLSorDTLSRecord},
        stateName,
        state,
        connection
      ) do
    connection.handle_protocol_record(tLSorDTLSRecord, stateName, state)
  end

  def handle_common_event(:timeout, :hibernate, _, _, _) do
    {:keep_state_and_data, [:hibernate]}
  end

  def handle_common_event(
        :internal,
        r_change_cipher_spec(type: <<1>>),
        stateName,
        r_state(connection_env: r_connection_env(negotiated_version: version)) = state,
        _
      ) do
    handle_own_alert(
      r_alert(
        level: 2,
        description: 40,
        where: %{
          :mfa => {:ssl_connection, :handle_common_event, 5},
          :line => 1410,
          :file => 'otp/lib/ssl/src/ssl_connection.erl'
        }
      ),
      version,
      stateName,
      state
    )
  end

  def handle_common_event(
        {:timeout, :handshake},
        :close,
        _StateName,
        r_state(start_or_recv_from: startFrom) = state,
        _
      ) do
    {:stop_and_reply, {:shutdown, :user_timeout}, {:reply, startFrom, {:error, :timeout}},
     r_state(state, start_or_recv_from: :undefined)}
  end

  def handle_common_event(
        {:timeout, :recv},
        :timeout,
        stateName,
        r_state(start_or_recv_from: recvFrom) = state,
        _
      ) do
    {:next_state, stateName,
     r_state(state,
       start_or_recv_from: :undefined,
       bytes_to_read: :undefined
     ), [{:reply, recvFrom, {:error, :timeout}}]}
  end

  def handle_common_event(
        :internal,
        {:recv, recvFrom},
        stateName,
        r_state(start_or_recv_from: recvFrom),
        _
      )
      when stateName !== :connection do
    {:keep_state_and_data, [:postpone]}
  end

  def handle_common_event(
        type,
        msg,
        stateName,
        r_state(connection_env: r_connection_env(negotiated_version: version)) = state,
        _
      ) do
    alert =
      r_alert(
        level: 2,
        description: 10,
        where: %{
          :mfa => {:ssl_connection, :handle_common_event, 5},
          :line => 1425,
          :file => 'otp/lib/ssl/src/ssl_connection.erl'
        },
        reason: {:unexpected_msg, {type, msg}}
      )

    handle_own_alert(alert, version, stateName, state)
  end

  defp handle_call({:application_data, _Data}, _, _, _, _) do
    {:keep_state_and_data, [:postpone]}
  end

  defp handle_call(
         {:close, _} = close,
         from,
         stateName,
         r_state(connection_env: cEnv) = state,
         _Connection
       ) do
    result = terminate(close, stateName, state)

    {:stop_and_reply, {:shutdown, :normal}, {:reply, from, result},
     r_state(state, connection_env: r_connection_env(cEnv, terminated: true))}
  end

  defp handle_call(
         {:shutdown, :read_write = how},
         from,
         stateName,
         r_state(
           static_env:
             r_static_env(
               transport_cb: transport,
               socket: socket
             ),
           connection_env: cEnv
         ) = state,
         _
       ) do
    try do
      send_alert(
        r_alert(
          level: 1,
          description: 0,
          where: %{
            :mfa => {:ssl_connection, :handle_call, 5},
            :line => 1442,
            :file => 'otp/lib/ssl/src/ssl_connection.erl'
          }
        ),
        stateName,
        state
      )
    catch
      return ->
        return
    else
      _ ->
        case transport.shutdown(socket, how) do
          :ok ->
            {:next_state, stateName,
             r_state(state, connection_env: r_connection_env(cEnv, terminated: true)),
             [{:reply, from, :ok}]}

          error ->
            {:stop_and_reply, {:shutdown, :normal}, {:reply, from, error},
             r_state(state, connection_env: r_connection_env(cEnv, terminated: true))}
        end
    end
  end

  defp handle_call(
         {:shutdown, how0},
         from,
         stateName,
         r_state(
           static_env:
             r_static_env(
               transport_cb: transport,
               socket: socket
             )
         ) = state,
         _
       ) do
    case transport.shutdown(socket, how0) do
      :ok ->
        {:next_state, stateName, state, [{:reply, from, :ok}]}

      error ->
        {:stop_and_reply, {:shutdown, :normal}, {:reply, from, error}, state}
    end
  end

  defp handle_call(
         {:recv, _N, _Timeout},
         from,
         _,
         r_state(socket_options: r_socket_options(active: active)),
         _
       )
       when active !== false do
    {:keep_state_and_data, [{:reply, from, {:error, :einval}}]}
  end

  defp handle_call({:recv, n, timeout}, recvFrom, stateName, state, _) do
    {:next_state, stateName,
     r_state(state,
       bytes_to_read: n,
       start_or_recv_from: recvFrom
     ), [{:next_event, :internal, {:recv, recvFrom}}, {{:timeout, :recv}, timeout, :timeout}]}
  end

  defp handle_call(
         {:new_user, user},
         from,
         stateName,
         state = r_state(connection_env: r_connection_env(user_application: {oldMon, _}) = cEnv),
         _
       ) do
    newMon = :erlang.monitor(:process, user)
    :erlang.demonitor(oldMon, [:flush])

    {:next_state, stateName,
     r_state(state, connection_env: r_connection_env(cEnv, user_application: {newMon, user})),
     [{:reply, from, :ok}]}
  end

  defp handle_call(
         {:get_opts, optTags},
         from,
         _,
         r_state(
           static_env:
             r_static_env(
               socket: socket,
               transport_cb: transport
             ),
           socket_options: sockOpts
         ),
         connection
       ) do
    optsReply = get_socket_opts(connection, transport, socket, optTags, sockOpts, [])
    {:keep_state_and_data, [{:reply, from, optsReply}]}
  end

  defp handle_call(
         {:set_opts, opts0},
         from,
         stateName,
         r_state(
           static_env: r_static_env(socket: socket, transport_cb: transport, trackers: trackers),
           connection_env: r_connection_env(user_application: {_Mon, pid}),
           socket_options: opts1
         ) = state0,
         connection
       ) do
    {reply, opts} = set_socket_opts(connection, transport, socket, opts0, opts1, [])

    case {:proplists.lookup(:active, opts0), opts} do
      {{_, n}, r_socket_options(active: false)} when is_integer(n) ->
        send_user(
          pid,
          format_passive(connection.pids(state0), transport, socket, trackers, connection)
        )

      _ ->
        :ok
    end

    state = r_state(state0, socket_options: opts)
    handle_active_option(r_socket_options(opts, :active), stateName, from, reply, state)
  end

  defp handle_call(:renegotiate, from, stateName, _, _)
       when stateName !== :connection do
    {:keep_state_and_data, [{:reply, from, {:error, :already_renegotiating}}]}
  end

  defp handle_call(
         {:prf, secret, label, seed, wantedLength},
         from,
         _,
         r_state(
           connection_states: connectionStates,
           connection_env: r_connection_env(negotiated_version: version)
         ),
         _
       ) do
    %{:security_parameters => secParams} =
      :ssl_record.current_connection_state(
        connectionStates,
        :read
      )

    r_security_parameters(
      master_secret: masterSecret,
      client_random: clientRandom,
      server_random: serverRandom,
      prf_algorithm: pRFAlgorithm
    ) = secParams

    reply =
      try do
        secretToUse =
          case secret do
            _ when is_binary(secret) ->
              secret

            :master_secret ->
              masterSecret
          end

        seedToUse =
          :lists.reverse(
            :lists.foldl(
              fn
                x, acc
                when is_binary(x) ->
                  [x | acc]

                :client_random, acc ->
                  [clientRandom | acc]

                :server_random, acc ->
                  [serverRandom | acc]
              end,
              [],
              seed
            )
          )

        :ssl_handshake.prf(
          :ssl.tls_version(version),
          pRFAlgorithm,
          secretToUse,
          label,
          seedToUse,
          wantedLength
        )
      catch
        :exit, _ ->
          {:error, :badarg}

        :error, reason ->
          {:error, reason}
      end

    {:keep_state_and_data, [{:reply, from, reply}]}
  end

  defp handle_call(_, _, _, _, _) do
    {:keep_state_and_data, [:postpone]}
  end

  defp handle_info(
         {errorTag, socket, :econnaborted},
         stateName,
         r_state(
           static_env:
             r_static_env(
               role: role,
               host: host,
               port: port,
               socket: socket,
               transport_cb: transport,
               error_tag: errorTag,
               trackers: trackers,
               protocol_cb: connection
             ),
           handshake_env: r_handshake_env(renegotiation: type),
           connection_env: r_connection_env(negotiated_version: version),
           session: session,
           start_or_recv_from: startFrom
         ) = state
       )
       when stateName !== :connection do
    maybe_invalidate_session(version, type, role, host, port, session)
    pids = connection.pids(state)

    alert_user(
      pids,
      transport,
      trackers,
      socket,
      startFrom,
      r_alert(
        level: 2,
        description: 0,
        where: %{
          :mfa => {:ssl_connection, :handle_info, 3},
          :line => 1558,
          :file => 'otp/lib/ssl/src/ssl_connection.erl'
        }
      ),
      role,
      stateName,
      connection
    )

    {:stop, {:shutdown, :normal}, state}
  end

  defp handle_info(
         {errorTag, socket, reason},
         stateName,
         r_state(
           static_env: r_static_env(role: role, socket: socket, error_tag: errorTag),
           ssl_options: %{:log_level => level}
         ) = state
       ) do
    :ssl_logger.log(
      :info,
      level,
      %{
        :description => 'Socket error',
        :reason => [{:error_tag, errorTag}, {:description, reason}]
      },
      %{
        :mfa => {:ssl_connection, :handle_info, 3},
        :line => 1567,
        :file => 'otp/lib/ssl/src/ssl_connection.erl'
      }
    )

    alert =
      r_alert(
        level: 2,
        description: 0,
        where: %{
          :mfa => {:ssl_connection, :handle_info, 3},
          :line => 1568,
          :file => 'otp/lib/ssl/src/ssl_connection.erl'
        },
        reason: {:transport_error, reason}
      )

    handle_normal_shutdown(r_alert(alert, role: role), stateName, state)
    {:stop, {:shutdown, :normal}, state}
  end

  defp handle_info(
         {:DOWN, monitorRef, _, _, reason},
         _,
         r_state(
           connection_env: r_connection_env(user_application: {monitorRef, _Pid}),
           ssl_options: %{:erl_dist => true}
         )
       ) do
    {:stop, {:shutdown, reason}}
  end

  defp handle_info(
         {:DOWN, monitorRef, _, _, _},
         _,
         r_state(connection_env: r_connection_env(user_application: {monitorRef, _Pid}))
       ) do
    {:stop, {:shutdown, :normal}}
  end

  defp handle_info(
         {:EXIT, pid, _Reason},
         stateName,
         r_state(connection_env: r_connection_env(user_application: {_MonitorRef, pid})) = state
       ) do
    {:next_state, stateName, state}
  end

  defp handle_info({:EXIT, _Sup, :shutdown}, _StateName, state) do
    {:stop, :shutdown, state}
  end

  defp handle_info(
         {:EXIT, socket, :normal},
         _StateName,
         r_state(static_env: r_static_env(socket: socket)) = state
       ) do
    {:stop, {:shutdown, :transport_closed}, state}
  end

  defp handle_info(
         {:EXIT, socket, reason},
         _StateName,
         r_state(static_env: r_static_env(socket: socket)) = state
       ) do
    {:stop, {:shutdown, reason}, state}
  end

  defp handle_info(:allow_renegotiate, stateName, r_state(handshake_env: hsEnv) = state) do
    {:next_state, stateName,
     r_state(state, handshake_env: r_handshake_env(hsEnv, allow_renegotiate: true))}
  end

  defp handle_info(
         msg,
         stateName,
         r_state(
           static_env: r_static_env(socket: socket, error_tag: errorTag),
           ssl_options: %{:log_level => level}
         ) = state
       ) do
    :ssl_logger.log(
      :notice,
      level,
      %{
        :description => 'Unexpected INFO message',
        :reason => [{:message, msg}, {:socket, socket}, {:error_tag, errorTag}]
      },
      %{
        :mfa => {:ssl_connection, :handle_info, 3},
        :line => 1600,
        :file => 'otp/lib/ssl/src/ssl_connection.erl'
      }
    )

    {:next_state, stateName, state}
  end

  def terminate(_, _, r_state(connection_env: r_connection_env(terminated: true))) do
    :ok
  end

  def terminate(
        {:shutdown, :transport_closed} = reason,
        _StateName,
        r_state(
          static_env:
            r_static_env(protocol_cb: connection, socket: socket, transport_cb: transport)
        ) = state
      ) do
    handle_trusted_certs_db(state)
    connection.close(reason, socket, transport, :undefined, :undefined)
  end

  def terminate(
        {:shutdown, :own_alert},
        _StateName,
        r_state(
          static_env:
            r_static_env(protocol_cb: connection, socket: socket, transport_cb: transport)
        ) = state
      ) do
    handle_trusted_certs_db(state)

    case :application.get_env(:ssl, :alert_timeout) do
      {:ok, timeout} when is_integer(timeout) ->
        connection.close({:timeout, timeout}, socket, transport, :undefined, :undefined)

      _ ->
        connection.close({:timeout, 5000}, socket, transport, :undefined, :undefined)
    end
  end

  def terminate(
        {:shutdown, :downgrade = reason},
        :downgrade,
        r_state(
          static_env:
            r_static_env(
              protocol_cb: connection,
              transport_cb: transport,
              socket: socket
            )
        ) = state
      ) do
    handle_trusted_certs_db(state)
    connection.close(reason, socket, transport, :undefined, :undefined)
  end

  def terminate(
        reason,
        :connection,
        r_state(
          static_env:
            r_static_env(protocol_cb: connection, transport_cb: transport, socket: socket),
          connection_states: connectionStates,
          ssl_options: %{:padding_check => check}
        ) = state
      ) do
    handle_trusted_certs_db(state)
    alert = terminate_alert(reason)

    try do
      :ok = connection.send_alert_in_connection(alert, state)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    connection.close({:timeout, 5000}, socket, transport, connectionStates, check)
  end

  def terminate(
        reason,
        _StateName,
        r_state(
          static_env:
            r_static_env(
              transport_cb: transport,
              protocol_cb: connection,
              socket: socket
            )
        ) = state
      ) do
    handle_trusted_certs_db(state)
    connection.close(reason, socket, transport, :undefined, :undefined)
  end

  def format_status(:normal, [_, stateName, state]) do
    [{:data, [{'State', {stateName, state}}]}]
  end

  def format_status(:terminate, [_, stateName, state]) do
    sslOptions = r_state(state, :ssl_options)

    newOptions = %{
      sslOptions
      | :password => '***',
        :cert => '***',
        :cacerts => '***',
        :key => '***',
        :dh => '***',
        :psk_identity => '***',
        :srp_identity => '***'
    }

    [
      {:data,
       [
         {'State',
          {stateName,
           r_state(state,
             connection_states: '***',
             protocol_buffers: '***',
             user_data_buffer: '***',
             handshake_env: '***',
             connection_env: '***',
             session: '***',
             ssl_options: newOptions,
             flight_buffer: '***'
           )}}
       ]}
    ]
  end

  defp send_alert(
         alert,
         :connection,
         r_state(static_env: r_static_env(protocol_cb: connection)) = state
       ) do
    connection.send_alert_in_connection(alert, state)
  end

  defp send_alert(alert, _, r_state(static_env: r_static_env(protocol_cb: connection)) = state) do
    connection.send_alert(alert, state)
  end

  defp connection_info(
         r_state(
           static_env: r_static_env(protocol_cb: connection),
           handshake_env:
             r_handshake_env(
               sni_hostname: sNIHostname,
               resumption: resumption
             ),
           session:
             r_session(
               session_id: sessionId,
               cipher_suite: cipherSuite,
               srp_username: srpUsername,
               ecc: eCCCurve
             ) = session,
           connection_states: %{:current_write => currentWrite},
           connection_env: r_connection_env(negotiated_version: {_, _} = version),
           ssl_options: opts
         )
       ) do
    recordCB = record_cb(connection)
    cipherSuiteDef = %{:key_exchange => kexAlg} = :ssl_cipher_format.suite_bin_to_map(cipherSuite)

    isNamedCurveSuite =
      :lists.member(
        kexAlg,
        [:ecdh_ecdsa, :ecdhe_ecdsa, :ecdh_rsa, :ecdhe_rsa, :ecdh_anon]
      )

    curveInfo =
      case eCCCurve do
        {:namedCurve, curve} when isNamedCurveSuite ->
          [{:ecc, {:named_curve, :pubkey_cert_records.namedCurves(curve)}}]

        _ ->
          []
      end

    mFLInfo =
      case :maps.get(:max_fragment_length, currentWrite, :undefined) do
        maxFragmentLength when is_integer(maxFragmentLength) ->
          [{:max_fragment_length, maxFragmentLength}]

        _ ->
          []
      end

    [
      [
        {:protocol, recordCB.protocol_version(version)},
        {:session_id, sessionId},
        {:session_data, :erlang.term_to_binary(session)},
        {:session_resumption, resumption},
        {:selected_cipher_suite, cipherSuiteDef},
        {:sni_hostname, sNIHostname},
        {:srp_username, srpUsername}
      ]
      | curveInfo
    ] ++ mFLInfo ++ ssl_options_list(opts)
  end

  defp security_info(r_state(connection_states: connectionStates)) do
    %{
      :security_parameters =>
        r_security_parameters(
          client_random: clientRand,
          server_random: serverRand,
          master_secret: masterSecret
        )
    } =
      :ssl_record.current_connection_state(
        connectionStates,
        :read
      )

    [{:client_random, clientRand}, {:server_random, serverRand}, {:master_secret, masterSecret}]
  end

  defp do_server_hello(
         type,
         %{:next_protocol_negotiation => nextProtocols} = serverHelloExt,
         r_state(
           connection_env: r_connection_env(negotiated_version: version),
           handshake_env: hsEnv,
           session: r_session(session_id: sessId),
           connection_states: connectionStates0,
           ssl_options: %{:versions => [highestVersion | _]}
         ) = state0,
         connection
       )
       when is_atom(type) do
    connectionStates1 = update_server_random(connectionStates0, version, highestVersion)
    state1 = r_state(state0, connection_states: connectionStates1)

    serverHello =
      :ssl_handshake.server_hello(
        sessId,
        :ssl.tls_version(version),
        connectionStates1,
        serverHelloExt
      )

    state =
      server_hello(
        serverHello,
        r_state(state1,
          handshake_env:
            r_handshake_env(hsEnv,
              expecting_next_protocol_negotiation: nextProtocols !== :undefined
            )
        ),
        connection
      )

    case type do
      :new ->
        new_server_hello(serverHello, state, connection)

      :resumed ->
        resumed_server_hello(state, connection)
    end
  end

  defp update_server_random(
         %{
           :pending_read => %{:security_parameters => readSecParams0} = readState0,
           :pending_write => %{:security_parameters => writeSecParams0} = writeState0
         } = connectionStates,
         version,
         highestVersion
       ) do
    readRandom =
      override_server_random(
        r_security_parameters(readSecParams0, :server_random),
        version,
        highestVersion
      )

    writeRandom =
      override_server_random(
        r_security_parameters(writeSecParams0, :server_random),
        version,
        highestVersion
      )

    readSecParams = r_security_parameters(readSecParams0, server_random: readRandom)
    writeSecParams = r_security_parameters(writeSecParams0, server_random: writeRandom)
    readState = %{readState0 | :security_parameters => readSecParams}
    writeState = %{writeState0 | :security_parameters => writeSecParams}
    %{connectionStates | :pending_read => readState, :pending_write => writeState}
  end

  defp override_server_random(
         <<random0::size(24)-binary, _::size(8)-binary>> = random,
         {m, n},
         {major, minor}
       )
       when major > 3 or (major === 3 and minor >= 4) do
    cond do
      m === 3 and n === 3 ->
        down = <<68, 79, 87, 78, 71, 82, 68, 1>>
        <<random0::binary, down::binary>>

      m === 3 and n < 3 ->
        down = <<68, 79, 87, 78, 71, 82, 68, 0>>
        <<random0::binary, down::binary>>

      true ->
        random
    end
  end

  defp override_server_random(
         <<random0::size(24)-binary, _::size(8)-binary>> = random,
         {m, n},
         {major, minor}
       )
       when major === 3 and minor === 3 do
    cond do
      m === 3 and n < 3 ->
        down = <<68, 79, 87, 78, 71, 82, 68, 0>>
        <<random0::binary, down::binary>>

      true ->
        random
    end
  end

  defp override_server_random(random, _, _) do
    random
  end

  defp new_server_hello(
         r_server_hello(
           cipher_suite: cipherSuite,
           compression_method: compression,
           session_id: sessionId
         ),
         r_state(
           session: session0,
           connection_env: r_connection_env(negotiated_version: version)
         ) = state0,
         connection
       ) do
    try do
      server_certify_and_key_exchange(state0, connection)
    catch
      r_alert() = alert ->
        handle_own_alert(alert, version, :hello, state0)
    else
      r_state() = state1 ->
        {state, actions} = server_hello_done(state1, connection)

        session =
          r_session(session0,
            session_id: sessionId,
            cipher_suite: cipherSuite,
            compression_method: compression
          )

        connection.next_event(:certify, :no_record, r_state(state, session: session), actions)
    end
  end

  defp resumed_server_hello(
         r_state(
           session: session,
           connection_states: connectionStates0,
           connection_env: r_connection_env(negotiated_version: version)
         ) = state0,
         connection
       ) do
    case :ssl_handshake.master_secret(
           :ssl.tls_version(version),
           session,
           connectionStates0,
           :server
         ) do
      {_, connectionStates1} ->
        state1 =
          r_state(state0,
            connection_states: connectionStates1,
            session: session
          )

        {state, actions} = finalize_handshake(state1, :abbreviated, connection)
        connection.next_event(:abbreviated, :no_record, state, actions)

      r_alert() = alert ->
        handle_own_alert(alert, version, :hello, state0)
    end
  end

  defp server_hello(serverHello, state0, connection) do
    cipherSuite = r_server_hello(serverHello, :cipher_suite)
    %{:key_exchange => keyAlgorithm} = :ssl_cipher_format.suite_bin_to_map(cipherSuite)

    r_state(handshake_env: hsEnv) =
      state =
      connection.queue_handshake(
        serverHello,
        state0
      )

    r_state(state, handshake_env: r_handshake_env(hsEnv, kex_algorithm: keyAlgorithm))
  end

  defp server_hello_done(state, connection) do
    helloDone = :ssl_handshake.server_hello_done()
    connection.send_handshake(helloDone, state)
  end

  defp handle_peer_cert(
         role,
         peerCert,
         publicKeyInfo,
         r_state(
           handshake_env: hsEnv,
           session: r_session(cipher_suite: cipherSuite) = session
         ) = state0,
         connection,
         actions
       ) do
    state1 =
      r_state(state0,
        handshake_env: r_handshake_env(hsEnv, public_key_info: publicKeyInfo),
        session: r_session(session, peer_certificate: peerCert)
      )

    %{:key_exchange => keyAlgorithm} = :ssl_cipher_format.suite_bin_to_map(cipherSuite)
    state = handle_peer_cert_key(role, peerCert, publicKeyInfo, keyAlgorithm, state1)
    connection.next_event(:certify, :no_record, state, actions)
  end

  defp handle_peer_cert_key(
         :client,
         _,
         {{1, 2, 840, 10045, 2, 1}, r_ECPoint(point: _ECPoint) = publicKey, publicKeyParams},
         keyAlg,
         r_state(handshake_env: hsEnv, session: session) = state
       )
       when keyAlg == :ecdh_rsa or keyAlg == :ecdh_ecdsa do
    eCDHKey = :public_key.generate_key(publicKeyParams)

    premasterSecret =
      :ssl_handshake.premaster_secret(
        publicKey,
        eCDHKey
      )

    master_secret(
      premasterSecret,
      r_state(state,
        handshake_env: r_handshake_env(hsEnv, kex_keys: eCDHKey),
        session: r_session(session, ecc: publicKeyParams)
      )
    )
  end

  defp handle_peer_cert_key(_, _, _, _, state) do
    state
  end

  defp certify_client(
         r_state(
           static_env: r_static_env(role: :client, cert_db: certDbHandle, cert_db_ref: certDbRef),
           client_certificate_requested: true,
           session: r_session(own_certificate: ownCert)
         ) = state,
         connection
       ) do
    certificate = :ssl_handshake.certificate(ownCert, certDbHandle, certDbRef, :client)
    connection.queue_handshake(certificate, state)
  end

  defp certify_client(
         r_state(client_certificate_requested: false) = state,
         _
       ) do
    state
  end

  defp verify_client_cert(
         r_state(
           static_env: r_static_env(role: :client),
           handshake_env:
             r_handshake_env(
               tls_handshake_history: hist,
               cert_hashsign_algorithm: hashSign
             ),
           connection_env:
             r_connection_env(
               negotiated_version: version,
               private_key: privateKey
             ),
           client_certificate_requested: true,
           session:
             r_session(
               master_secret: masterSecret,
               own_certificate: ownCert
             )
         ) = state,
         connection
       ) do
    case :ssl_handshake.client_certificate_verify(
           ownCert,
           masterSecret,
           :ssl.tls_version(version),
           hashSign,
           privateKey,
           hist
         ) do
      r_certificate_verify() = verified ->
        connection.queue_handshake(verified, state)

      :ignore ->
        state

      r_alert() = alert ->
        throw(alert)
    end
  end

  defp verify_client_cert(
         r_state(client_certificate_requested: false) = state,
         _
       ) do
    state
  end

  defp client_certify_and_key_exchange(
         r_state(connection_env: r_connection_env(negotiated_version: version)) = state0,
         connection
       ) do
    try do
      do_client_certify_and_key_exchange(state0, connection)
    catch
      r_alert() = alert ->
        handle_own_alert(alert, version, :certify, state0)
    else
      state1 = r_state() ->
        {state2, actions} = finalize_handshake(state1, :certify, connection)
        state = r_state(state2, client_certificate_requested: false)
        connection.next_event(:cipher, :no_record, state, actions)
    end
  end

  defp do_client_certify_and_key_exchange(state0, connection) do
    state1 = certify_client(state0, connection)
    state2 = key_exchange(state1, connection)
    verify_client_cert(state2, connection)
  end

  defp server_certify_and_key_exchange(state0, connection) do
    state1 = certify_server(state0, connection)
    state2 = key_exchange(state1, connection)
    request_client_cert(state2, connection)
  end

  defp certify_client_key_exchange(
         r_encrypted_premaster_secret(premaster_secret: encPMS),
         r_state(
           connection_env: r_connection_env(private_key: key),
           handshake_env: r_handshake_env(client_hello_version: {major, minor} = version)
         ) = state,
         connection
       ) do
    fakeSecret = make_premaster_secret(version, :rsa)

    premasterSecret =
      try do
        :ssl_handshake.premaster_secret(encPMS, key)
      catch
        r_alert(description: 51) ->
          fakeSecret
      else
        secret when :erlang.byte_size(secret) == 48 ->
          case secret do
            <<^major::size(8)-unsigned-big-integer, ^minor::size(8)-unsigned-big-integer,
              rest::binary>> ->
              <<major::size(8)-unsigned-big-integer, minor::size(8)-unsigned-big-integer,
                rest::binary>>

            <<_::size(8)-unsigned-big-integer, _::size(8)-unsigned-big-integer, rest::binary>> ->
              <<major::size(8)-unsigned-big-integer, minor::size(8)-unsigned-big-integer,
                rest::binary>>
          end

        _ ->
          fakeSecret
      end

    calculate_master_secret(premasterSecret, state, connection, :certify, :cipher)
  end

  defp certify_client_key_exchange(
         r_client_diffie_hellman_public(dh_public: clientPublicDhKey),
         r_state(
           handshake_env:
             r_handshake_env(
               diffie_hellman_params: r_DHParameter() = params,
               kex_keys: {_, serverDhPrivateKey}
             )
         ) = state,
         connection
       ) do
    premasterSecret =
      :ssl_handshake.premaster_secret(
        clientPublicDhKey,
        serverDhPrivateKey,
        params
      )

    calculate_master_secret(premasterSecret, state, connection, :certify, :cipher)
  end

  defp certify_client_key_exchange(
         r_client_ec_diffie_hellman_public(dh_public: clientPublicEcDhPoint),
         r_state(handshake_env: r_handshake_env(kex_keys: eCDHKey)) = state,
         connection
       ) do
    premasterSecret =
      :ssl_handshake.premaster_secret(
        r_ECPoint(point: clientPublicEcDhPoint),
        eCDHKey
      )

    calculate_master_secret(premasterSecret, state, connection, :certify, :cipher)
  end

  defp certify_client_key_exchange(
         r_client_psk_identity() = clientKey,
         r_state(ssl_options: %{:user_lookup_fun => pSKLookup}) = state0,
         connection
       ) do
    premasterSecret =
      :ssl_handshake.premaster_secret(
        clientKey,
        pSKLookup
      )

    calculate_master_secret(premasterSecret, state0, connection, :certify, :cipher)
  end

  defp certify_client_key_exchange(
         r_client_dhe_psk_identity() = clientKey,
         r_state(
           handshake_env:
             r_handshake_env(
               diffie_hellman_params: r_DHParameter() = params,
               kex_keys: {_, serverDhPrivateKey}
             ),
           ssl_options: %{:user_lookup_fun => pSKLookup}
         ) = state0,
         connection
       ) do
    premasterSecret =
      :ssl_handshake.premaster_secret(clientKey, serverDhPrivateKey, params, pSKLookup)

    calculate_master_secret(premasterSecret, state0, connection, :certify, :cipher)
  end

  defp certify_client_key_exchange(
         r_client_ecdhe_psk_identity() = clientKey,
         r_state(
           handshake_env: r_handshake_env(kex_keys: serverEcDhPrivateKey),
           ssl_options: %{:user_lookup_fun => pSKLookup}
         ) = state,
         connection
       ) do
    premasterSecret =
      :ssl_handshake.premaster_secret(
        clientKey,
        serverEcDhPrivateKey,
        pSKLookup
      )

    calculate_master_secret(premasterSecret, state, connection, :certify, :cipher)
  end

  defp certify_client_key_exchange(
         r_client_rsa_psk_identity() = clientKey,
         r_state(
           connection_env: r_connection_env(private_key: key),
           ssl_options: %{:user_lookup_fun => pSKLookup}
         ) = state0,
         connection
       ) do
    premasterSecret = :ssl_handshake.premaster_secret(clientKey, key, pSKLookup)
    calculate_master_secret(premasterSecret, state0, connection, :certify, :cipher)
  end

  defp certify_client_key_exchange(
         r_client_srp_public() = clientKey,
         r_state(
           handshake_env:
             r_handshake_env(
               srp_params: params,
               kex_keys: key
             )
         ) = state0,
         connection
       ) do
    premasterSecret = :ssl_handshake.premaster_secret(clientKey, key, params)
    calculate_master_secret(premasterSecret, state0, connection, :certify, :cipher)
  end

  defp certify_server(
         r_state(handshake_env: r_handshake_env(kex_algorithm: kexAlg)) = state,
         _
       )
       when kexAlg == :dh_anon or kexAlg == :ecdh_anon or
              kexAlg == :psk or kexAlg == :dhe_psk or
              kexAlg == :ecdhe_psk or kexAlg == :srp_anon do
    state
  end

  defp certify_server(
         r_state(
           static_env:
             r_static_env(
               cert_db: certDbHandle,
               cert_db_ref: certDbRef
             ),
           session: r_session(own_certificate: ownCert)
         ) = state,
         connection
       ) do
    case :ssl_handshake.certificate(ownCert, certDbHandle, certDbRef, :server) do
      cert = r_certificate() ->
        connection.queue_handshake(cert, state)

      alert = r_alert() ->
        throw(alert)
    end
  end

  defp key_exchange(
         r_state(
           static_env: r_static_env(role: :server),
           handshake_env: r_handshake_env(kex_algorithm: :rsa)
         ) = state,
         _
       ) do
    state
  end

  defp key_exchange(
         r_state(
           static_env: r_static_env(role: :server),
           handshake_env:
             r_handshake_env(
               kex_algorithm: kexAlg,
               diffie_hellman_params: r_DHParameter() = params,
               hashsign_algorithm: hashSignAlgo
             ),
           connection_env:
             r_connection_env(
               negotiated_version: version,
               private_key: privateKey
             ),
           connection_states: connectionStates0
         ) = state0,
         connection
       )
       when kexAlg == :dhe_dss or kexAlg == :dhe_rsa or
              kexAlg == :dh_anon do
    dHKeys = :public_key.generate_key(params)

    %{:security_parameters => secParams} =
      :ssl_record.pending_connection_state(
        connectionStates0,
        :read
      )

    r_security_parameters(
      client_random: clientRandom,
      server_random: serverRandom
    ) = secParams

    msg =
      :ssl_handshake.key_exchange(
        :server,
        :ssl.tls_version(version),
        {:dh, dHKeys, params, hashSignAlgo, clientRandom, serverRandom, privateKey}
      )

    r_state(handshake_env: hsEnv) =
      state =
      connection.queue_handshake(
        msg,
        state0
      )

    r_state(state, handshake_env: r_handshake_env(hsEnv, kex_keys: dHKeys))
  end

  defp key_exchange(
         r_state(
           static_env: r_static_env(role: :server),
           handshake_env: r_handshake_env(kex_algorithm: kexAlg) = hsEnv,
           connection_env:
             r_connection_env(private_key: r_ECPrivateKey(parameters: eCCurve) = key),
           session: session
         ) = state,
         _
       )
       when kexAlg == :ecdh_ecdsa or kexAlg == :ecdh_rsa do
    r_state(state,
      handshake_env: r_handshake_env(hsEnv, kex_keys: key),
      session: r_session(session, ecc: eCCurve)
    )
  end

  defp key_exchange(
         r_state(
           static_env: r_static_env(role: :server),
           handshake_env:
             r_handshake_env(
               kex_algorithm: kexAlg,
               hashsign_algorithm: hashSignAlgo
             ),
           connection_env:
             r_connection_env(
               negotiated_version: version,
               private_key: privateKey
             ),
           session: r_session(ecc: eCCCurve),
           connection_states: connectionStates0
         ) = state0,
         connection
       )
       when kexAlg == :ecdhe_ecdsa or kexAlg == :ecdhe_rsa or
              kexAlg == :ecdh_anon do
    eCDHKeys = :public_key.generate_key(eCCCurve)

    %{:security_parameters => secParams} =
      :ssl_record.pending_connection_state(
        connectionStates0,
        :read
      )

    r_security_parameters(
      client_random: clientRandom,
      server_random: serverRandom
    ) = secParams

    msg =
      :ssl_handshake.key_exchange(
        :server,
        :ssl.tls_version(version),
        {:ecdh, eCDHKeys, hashSignAlgo, clientRandom, serverRandom, privateKey}
      )

    r_state(handshake_env: hsEnv) =
      state =
      connection.queue_handshake(
        msg,
        state0
      )

    r_state(state, handshake_env: r_handshake_env(hsEnv, kex_keys: eCDHKeys))
  end

  defp key_exchange(
         r_state(
           static_env: r_static_env(role: :server),
           handshake_env: r_handshake_env(kex_algorithm: :psk),
           ssl_options: %{:psk_identity => :undefined}
         ) = state,
         _
       ) do
    state
  end

  defp key_exchange(
         r_state(
           static_env: r_static_env(role: :server),
           ssl_options: %{:psk_identity => pskIdentityHint},
           handshake_env:
             r_handshake_env(
               kex_algorithm: :psk,
               hashsign_algorithm: hashSignAlgo
             ),
           connection_env:
             r_connection_env(
               negotiated_version: version,
               private_key: privateKey
             ),
           connection_states: connectionStates0
         ) = state0,
         connection
       ) do
    %{:security_parameters => secParams} =
      :ssl_record.pending_connection_state(
        connectionStates0,
        :read
      )

    r_security_parameters(
      client_random: clientRandom,
      server_random: serverRandom
    ) = secParams

    msg =
      :ssl_handshake.key_exchange(
        :server,
        :ssl.tls_version(version),
        {:psk, pskIdentityHint, hashSignAlgo, clientRandom, serverRandom, privateKey}
      )

    connection.queue_handshake(msg, state0)
  end

  defp key_exchange(
         r_state(
           static_env: r_static_env(role: :server),
           ssl_options: %{:psk_identity => pskIdentityHint},
           handshake_env:
             r_handshake_env(
               kex_algorithm: :dhe_psk,
               diffie_hellman_params: r_DHParameter() = params,
               hashsign_algorithm: hashSignAlgo
             ),
           connection_env:
             r_connection_env(
               negotiated_version: version,
               private_key: privateKey
             ),
           connection_states: connectionStates0
         ) = state0,
         connection
       ) do
    dHKeys = :public_key.generate_key(params)

    %{:security_parameters => secParams} =
      :ssl_record.pending_connection_state(
        connectionStates0,
        :read
      )

    r_security_parameters(
      client_random: clientRandom,
      server_random: serverRandom
    ) = secParams

    msg =
      :ssl_handshake.key_exchange(
        :server,
        :ssl.tls_version(version),
        {:dhe_psk, pskIdentityHint, dHKeys, params, hashSignAlgo, clientRandom, serverRandom,
         privateKey}
      )

    r_state(handshake_env: hsEnv) =
      state =
      connection.queue_handshake(
        msg,
        state0
      )

    r_state(state, handshake_env: r_handshake_env(hsEnv, kex_keys: dHKeys))
  end

  defp key_exchange(
         r_state(
           static_env: r_static_env(role: :server),
           ssl_options: %{:psk_identity => pskIdentityHint},
           handshake_env:
             r_handshake_env(
               kex_algorithm: :ecdhe_psk,
               hashsign_algorithm: hashSignAlgo
             ),
           connection_env:
             r_connection_env(
               negotiated_version: version,
               private_key: privateKey
             ),
           session: r_session(ecc: eCCCurve),
           connection_states: connectionStates0
         ) = state0,
         connection
       ) do
    eCDHKeys = :public_key.generate_key(eCCCurve)

    %{:security_parameters => secParams} =
      :ssl_record.pending_connection_state(
        connectionStates0,
        :read
      )

    r_security_parameters(
      client_random: clientRandom,
      server_random: serverRandom
    ) = secParams

    msg =
      :ssl_handshake.key_exchange(
        :server,
        :ssl.tls_version(version),
        {:ecdhe_psk, pskIdentityHint, eCDHKeys, hashSignAlgo, clientRandom, serverRandom,
         privateKey}
      )

    r_state(handshake_env: hsEnv) =
      state =
      connection.queue_handshake(
        msg,
        state0
      )

    r_state(state, handshake_env: r_handshake_env(hsEnv, kex_keys: eCDHKeys))
  end

  defp key_exchange(
         r_state(
           static_env: r_static_env(role: :server),
           handshake_env: r_handshake_env(kex_algorithm: :rsa_psk),
           ssl_options: %{:psk_identity => :undefined}
         ) = state,
         _
       ) do
    state
  end

  defp key_exchange(
         r_state(
           static_env: r_static_env(role: :server),
           ssl_options: %{:psk_identity => pskIdentityHint},
           handshake_env:
             r_handshake_env(
               kex_algorithm: :rsa_psk,
               hashsign_algorithm: hashSignAlgo
             ),
           connection_env:
             r_connection_env(
               negotiated_version: version,
               private_key: privateKey
             ),
           connection_states: connectionStates0
         ) = state0,
         connection
       ) do
    %{:security_parameters => secParams} =
      :ssl_record.pending_connection_state(
        connectionStates0,
        :read
      )

    r_security_parameters(
      client_random: clientRandom,
      server_random: serverRandom
    ) = secParams

    msg =
      :ssl_handshake.key_exchange(
        :server,
        :ssl.tls_version(version),
        {:psk, pskIdentityHint, hashSignAlgo, clientRandom, serverRandom, privateKey}
      )

    connection.queue_handshake(msg, state0)
  end

  defp key_exchange(
         r_state(
           static_env: r_static_env(role: :server),
           ssl_options: %{:user_lookup_fun => lookupFun},
           handshake_env:
             r_handshake_env(
               kex_algorithm: kexAlg,
               hashsign_algorithm: hashSignAlgo
             ),
           connection_env:
             r_connection_env(
               negotiated_version: version,
               private_key: privateKey
             ),
           session: r_session(srp_username: username),
           connection_states: connectionStates0
         ) = state0,
         connection
       )
       when kexAlg == :srp_dss or kexAlg == :srp_rsa or
              kexAlg == :srp_anon do
    srpParams = handle_srp_identity(username, lookupFun)

    keys =
      case generate_srp_server_keys(srpParams, 0) do
        alert = r_alert() ->
          throw(alert)

        keys0 = {_, _} ->
          keys0
      end

    %{:security_parameters => secParams} =
      :ssl_record.pending_connection_state(
        connectionStates0,
        :read
      )

    r_security_parameters(
      client_random: clientRandom,
      server_random: serverRandom
    ) = secParams

    msg =
      :ssl_handshake.key_exchange(
        :server,
        :ssl.tls_version(version),
        {:srp, keys, srpParams, hashSignAlgo, clientRandom, serverRandom, privateKey}
      )

    r_state(handshake_env: hsEnv) =
      state =
      connection.queue_handshake(
        msg,
        state0
      )

    r_state(state,
      handshake_env:
        r_handshake_env(hsEnv,
          srp_params: srpParams,
          kex_keys: keys
        )
    )
  end

  defp key_exchange(
         r_state(
           static_env: r_static_env(role: :client),
           handshake_env:
             r_handshake_env(
               kex_algorithm: :rsa,
               public_key_info: publicKeyInfo,
               premaster_secret: premasterSecret
             ),
           connection_env: r_connection_env(negotiated_version: version)
         ) = state0,
         connection
       ) do
    msg = rsa_key_exchange(:ssl.tls_version(version), premasterSecret, publicKeyInfo)
    connection.queue_handshake(msg, state0)
  end

  defp key_exchange(
         r_state(
           static_env: r_static_env(role: :client),
           handshake_env:
             r_handshake_env(
               kex_algorithm: kexAlg,
               kex_keys: {dhPubKey, _}
             ),
           connection_env: r_connection_env(negotiated_version: version)
         ) = state0,
         connection
       )
       when kexAlg == :dhe_dss or kexAlg == :dhe_rsa or
              kexAlg == :dh_anon do
    msg =
      :ssl_handshake.key_exchange(
        :client,
        :ssl.tls_version(version),
        {:dh, dhPubKey}
      )

    connection.queue_handshake(msg, state0)
  end

  defp key_exchange(
         r_state(
           static_env: r_static_env(role: :client),
           handshake_env:
             r_handshake_env(
               kex_algorithm: kexAlg,
               kex_keys: r_ECPrivateKey(parameters: eCCurve) = key
             ),
           connection_env: r_connection_env(negotiated_version: version),
           session: session
         ) = state0,
         connection
       )
       when kexAlg == :ecdhe_ecdsa or kexAlg == :ecdhe_rsa or
              kexAlg == :ecdh_ecdsa or kexAlg == :ecdh_rsa or
              kexAlg == :ecdh_anon do
    msg = :ssl_handshake.key_exchange(:client, :ssl.tls_version(version), {:ecdh, key})

    connection.queue_handshake(
      msg,
      r_state(state0, session: r_session(session, ecc: eCCurve))
    )
  end

  defp key_exchange(
         r_state(
           static_env: r_static_env(role: :client),
           handshake_env: r_handshake_env(kex_algorithm: :psk),
           connection_env: r_connection_env(negotiated_version: version),
           ssl_options: %{:psk_identity => pSKIdentity}
         ) = state0,
         connection
       ) do
    msg =
      :ssl_handshake.key_exchange(
        :client,
        :ssl.tls_version(version),
        {:psk, pSKIdentity}
      )

    connection.queue_handshake(msg, state0)
  end

  defp key_exchange(
         r_state(
           static_env: r_static_env(role: :client),
           handshake_env:
             r_handshake_env(
               kex_algorithm: :dhe_psk,
               kex_keys: {dhPubKey, _}
             ),
           connection_env: r_connection_env(negotiated_version: version),
           ssl_options: %{:psk_identity => pSKIdentity}
         ) = state0,
         connection
       ) do
    msg =
      :ssl_handshake.key_exchange(
        :client,
        :ssl.tls_version(version),
        {:dhe_psk, pSKIdentity, dhPubKey}
      )

    connection.queue_handshake(msg, state0)
  end

  defp key_exchange(
         r_state(
           static_env: r_static_env(role: :client),
           handshake_env:
             r_handshake_env(
               kex_algorithm: :ecdhe_psk,
               kex_keys: eCDHKeys
             ),
           connection_env: r_connection_env(negotiated_version: version),
           ssl_options: %{:psk_identity => pSKIdentity}
         ) = state0,
         connection
       ) do
    msg =
      :ssl_handshake.key_exchange(
        :client,
        :ssl.tls_version(version),
        {:ecdhe_psk, pSKIdentity, eCDHKeys}
      )

    connection.queue_handshake(msg, state0)
  end

  defp key_exchange(
         r_state(
           static_env: r_static_env(role: :client),
           handshake_env:
             r_handshake_env(
               kex_algorithm: :rsa_psk,
               public_key_info: publicKeyInfo,
               premaster_secret: premasterSecret
             ),
           connection_env: r_connection_env(negotiated_version: version),
           ssl_options: %{:psk_identity => pSKIdentity}
         ) = state0,
         connection
       ) do
    msg =
      rsa_psk_key_exchange(:ssl.tls_version(version), pSKIdentity, premasterSecret, publicKeyInfo)

    connection.queue_handshake(msg, state0)
  end

  defp key_exchange(
         r_state(
           static_env: r_static_env(role: :client),
           handshake_env:
             r_handshake_env(
               kex_algorithm: kexAlg,
               kex_keys: {clientPubKey, _}
             ),
           connection_env: r_connection_env(negotiated_version: version)
         ) = state0,
         connection
       )
       when kexAlg == :srp_dss or kexAlg == :srp_rsa or
              kexAlg == :srp_anon do
    msg =
      :ssl_handshake.key_exchange(
        :client,
        :ssl.tls_version(version),
        {:srp, clientPubKey}
      )

    connection.queue_handshake(msg, state0)
  end

  defp rsa_key_exchange(version, premasterSecret, publicKeyInfo = {algorithm, _, _})
       when algorithm == {1, 2, 840, 113_549, 1, 1, 1} or
              algorithm == {1, 2, 840, 113_549, 1, 1, 2} or
              algorithm == {1, 2, 840, 113_549, 1, 1, 4} or
              algorithm == {1, 2, 840, 113_549, 1, 1, 5} or
              algorithm == {1, 2, 840, 113_549, 1, 1, 14} or
              algorithm == {1, 2, 840, 113_549, 1, 1, 11} or
              algorithm == {1, 2, 840, 113_549, 1, 1, 12} or
              algorithm == {1, 2, 840, 113_549, 1, 1, 13} do
    :ssl_handshake.key_exchange(
      :client,
      :ssl.tls_version(version),
      {:premaster_secret, premasterSecret, publicKeyInfo}
    )
  end

  defp rsa_key_exchange(_, _, _) do
    throw(
      r_alert(
        level: 2,
        description: 40,
        where: %{
          :mfa => {:ssl_connection, :rsa_key_exchange, 3},
          :line => 2288,
          :file => 'otp/lib/ssl/src/ssl_connection.erl'
        },
        reason: :pub_key_is_not_rsa
      )
    )
  end

  defp rsa_psk_key_exchange(
         version,
         pskIdentity,
         premasterSecret,
         publicKeyInfo = {algorithm, _, _}
       )
       when algorithm == {1, 2, 840, 113_549, 1, 1, 1} or
              algorithm == {1, 2, 840, 113_549, 1, 1, 2} or
              algorithm == {1, 2, 840, 113_549, 1, 1, 4} or
              algorithm == {1, 2, 840, 113_549, 1, 1, 5} or
              algorithm == {1, 2, 840, 113_549, 1, 1, 14} or
              algorithm == {1, 2, 840, 113_549, 1, 1, 11} or
              algorithm == {1, 2, 840, 113_549, 1, 1, 12} or
              algorithm == {1, 2, 840, 113_549, 1, 1, 13} do
    :ssl_handshake.key_exchange(
      :client,
      :ssl.tls_version(version),
      {:psk_premaster_secret, pskIdentity, premasterSecret, publicKeyInfo}
    )
  end

  defp rsa_psk_key_exchange(_, _, _, _) do
    throw(
      r_alert(
        level: 2,
        description: 40,
        where: %{
          :mfa => {:ssl_connection, :rsa_psk_key_exchange, 4},
          :line => 2305,
          :file => 'otp/lib/ssl/src/ssl_connection.erl'
        },
        reason: :pub_key_is_not_rsa
      )
    )
  end

  defp request_client_cert(
         r_state(handshake_env: r_handshake_env(kex_algorithm: alg)) = state,
         _
       )
       when alg == :dh_anon or alg == :ecdh_anon or
              alg == :psk or alg == :dhe_psk or alg == :ecdhe_psk or
              alg == :rsa_psk or alg == :srp_dss or alg == :srp_rsa or
              alg == :srp_anon do
    state
  end

  defp request_client_cert(
         r_state(
           static_env:
             r_static_env(
               cert_db: certDbHandle,
               cert_db_ref: certDbRef
             ),
           connection_env: r_connection_env(negotiated_version: version),
           ssl_options: %{:verify => :verify_peer, :signature_algs => supportedHashSigns},
           connection_states: connectionStates0
         ) = state0,
         connection
       ) do
    %{:security_parameters => r_security_parameters(cipher_suite: cipherSuite)} =
      :ssl_record.pending_connection_state(
        connectionStates0,
        :read
      )

    tLSVersion = :ssl.tls_version(version)

    hashSigns =
      :ssl_handshake.available_signature_algs(
        supportedHashSigns,
        tLSVersion
      )

    msg =
      :ssl_handshake.certificate_request(
        cipherSuite,
        certDbHandle,
        certDbRef,
        hashSigns,
        tLSVersion
      )

    state = connection.queue_handshake(msg, state0)
    r_state(state, client_certificate_requested: true)
  end

  defp request_client_cert(
         r_state(ssl_options: %{:verify => :verify_none}) = state,
         _
       ) do
    state
  end

  defp calculate_master_secret(
         premasterSecret,
         r_state(
           connection_env: r_connection_env(negotiated_version: version),
           connection_states: connectionStates0,
           session: session0
         ) = state0,
         connection,
         _Current,
         next
       ) do
    case :ssl_handshake.master_secret(
           :ssl.tls_version(version),
           premasterSecret,
           connectionStates0,
           :server
         ) do
      {masterSecret, connectionStates} ->
        session = r_session(session0, master_secret: masterSecret)

        state =
          r_state(state0,
            connection_states: connectionStates,
            session: session
          )

        connection.next_event(next, :no_record, state)

      r_alert() = alert ->
        handle_own_alert(alert, version, :certify, state0)
    end
  end

  defp finalize_handshake(state0, stateName, connection) do
    r_state(connection_states: connectionStates0) =
      state1 =
      cipher_protocol(
        state0,
        connection
      )

    connectionStates =
      :ssl_record.activate_pending_connection_state(
        connectionStates0,
        :write,
        connection
      )

    state2 = r_state(state1, connection_states: connectionStates)
    state = next_protocol(state2, connection)
    finished(state, stateName, connection)
  end

  defp next_protocol(r_state(static_env: r_static_env(role: :server)) = state, _) do
    state
  end

  defp next_protocol(
         r_state(handshake_env: r_handshake_env(negotiated_protocol: :undefined)) = state,
         _
       ) do
    state
  end

  defp next_protocol(
         r_state(handshake_env: r_handshake_env(expecting_next_protocol_negotiation: false)) =
           state,
         _
       ) do
    state
  end

  defp next_protocol(
         r_state(handshake_env: r_handshake_env(negotiated_protocol: nextProtocol)) = state0,
         connection
       ) do
    nextProtocolMessage = :ssl_handshake.next_protocol(nextProtocol)
    connection.queue_handshake(nextProtocolMessage, state0)
  end

  defp cipher_protocol(state, connection) do
    connection.queue_change_cipher(r_change_cipher_spec(), state)
  end

  defp finished(
         r_state(
           static_env: r_static_env(role: role),
           handshake_env: r_handshake_env(tls_handshake_history: hist),
           connection_env: r_connection_env(negotiated_version: version),
           session: session,
           connection_states: connectionStates0
         ) = state0,
         stateName,
         connection
       ) do
    masterSecret = r_session(session, :master_secret)

    finished =
      :ssl_handshake.finished(
        :ssl.tls_version(version),
        role,
        get_current_prf(
          connectionStates0,
          :write
        ),
        masterSecret,
        hist
      )

    connectionStates = save_verify_data(role, finished, connectionStates0, stateName)

    connection.send_handshake(
      finished,
      r_state(state0, connection_states: connectionStates)
    )
  end

  defp save_verify_data(:client, r_finished(verify_data: data), connectionStates, :certify) do
    :ssl_record.set_client_verify_data(:current_write, data, connectionStates)
  end

  defp save_verify_data(:server, r_finished(verify_data: data), connectionStates, :cipher) do
    :ssl_record.set_server_verify_data(:current_both, data, connectionStates)
  end

  defp save_verify_data(:client, r_finished(verify_data: data), connectionStates, :abbreviated) do
    :ssl_record.set_client_verify_data(:current_both, data, connectionStates)
  end

  defp save_verify_data(:server, r_finished(verify_data: data), connectionStates, :abbreviated) do
    :ssl_record.set_server_verify_data(:current_write, data, connectionStates)
  end

  defp calculate_secret(
         r_server_dh_params(dh_p: prime, dh_g: base, dh_y: serverPublicDhKey) = params,
         r_state(handshake_env: hsEnv) = state,
         connection
       ) do
    keys =
      {_, privateDhKey} =
      :crypto.generate_key(
        :dh,
        [prime, base]
      )

    premasterSecret = :ssl_handshake.premaster_secret(serverPublicDhKey, privateDhKey, params)

    calculate_master_secret(
      premasterSecret,
      r_state(state, handshake_env: r_handshake_env(hsEnv, kex_keys: keys)),
      connection,
      :certify,
      :certify
    )
  end

  defp calculate_secret(
         r_server_ecdh_params(curve: eCCurve, public: eCServerPubKey),
         r_state(handshake_env: hsEnv, session: session) = state,
         connection
       ) do
    eCDHKeys = :public_key.generate_key(eCCurve)

    premasterSecret =
      :ssl_handshake.premaster_secret(
        r_ECPoint(point: eCServerPubKey),
        eCDHKeys
      )

    calculate_master_secret(
      premasterSecret,
      r_state(state,
        handshake_env: r_handshake_env(hsEnv, kex_keys: eCDHKeys),
        session: r_session(session, ecc: eCCurve)
      ),
      connection,
      :certify,
      :certify
    )
  end

  defp calculate_secret(
         r_server_psk_params(hint: identityHint),
         r_state(handshake_env: hsEnv) = state,
         connection
       ) do
    connection.next_event(
      :certify,
      :no_record,
      r_state(state, handshake_env: r_handshake_env(hsEnv, server_psk_identity: identityHint))
    )
  end

  defp calculate_secret(
         r_server_dhe_psk_params(
           dh_params:
             r_server_dh_params(
               dh_p: prime,
               dh_g: base
             )
         ) = serverKey,
         r_state(
           handshake_env: hsEnv,
           ssl_options: %{:user_lookup_fun => pSKLookup}
         ) = state,
         connection
       ) do
    keys =
      {_, privateDhKey} =
      :crypto.generate_key(
        :dh,
        [prime, base]
      )

    premasterSecret = :ssl_handshake.premaster_secret(serverKey, privateDhKey, pSKLookup)

    calculate_master_secret(
      premasterSecret,
      r_state(state, handshake_env: r_handshake_env(hsEnv, kex_keys: keys)),
      connection,
      :certify,
      :certify
    )
  end

  defp calculate_secret(
         r_server_ecdhe_psk_params(dh_params: r_server_ecdh_params(curve: eCCurve)) = serverKey,
         r_state(ssl_options: %{:user_lookup_fun => pSKLookup}) =
           r_state(
             handshake_env: hsEnv,
             session: session
           ) = state,
         connection
       ) do
    eCDHKeys = :public_key.generate_key(eCCurve)
    premasterSecret = :ssl_handshake.premaster_secret(serverKey, eCDHKeys, pSKLookup)

    calculate_master_secret(
      premasterSecret,
      r_state(state,
        handshake_env: r_handshake_env(hsEnv, kex_keys: eCDHKeys),
        session: r_session(session, ecc: eCCurve)
      ),
      connection,
      :certify,
      :certify
    )
  end

  defp calculate_secret(
         r_server_srp_params(srp_n: prime, srp_g: generator) = serverKey,
         r_state(
           handshake_env: hsEnv,
           ssl_options: %{:srp_identity => sRPId}
         ) = state,
         connection
       ) do
    keys = generate_srp_client_keys(generator, prime, 0)
    premasterSecret = :ssl_handshake.premaster_secret(serverKey, keys, sRPId)

    calculate_master_secret(
      premasterSecret,
      r_state(state, handshake_env: r_handshake_env(hsEnv, kex_keys: keys)),
      connection,
      :certify,
      :certify
    )
  end

  defp master_secret(r_alert() = alert, _) do
    alert
  end

  defp master_secret(
         premasterSecret,
         r_state(
           static_env: r_static_env(role: role),
           connection_env: r_connection_env(negotiated_version: version),
           session: session,
           connection_states: connectionStates0
         ) = state
       ) do
    case :ssl_handshake.master_secret(
           :ssl.tls_version(version),
           premasterSecret,
           connectionStates0,
           role
         ) do
      {masterSecret, connectionStates} ->
        r_state(state,
          session: r_session(session, master_secret: masterSecret),
          connection_states: connectionStates
        )

      r_alert() = alert ->
        alert
    end
  end

  defp generate_srp_server_keys(_SrpParams, 10) do
    r_alert(
      level: 2,
      description: 47,
      where: %{
        :mfa => {:ssl_connection, :generate_srp_server_keys, 2},
        :line => 2484,
        :file => 'otp/lib/ssl/src/ssl_connection.erl'
      }
    )
  end

  defp generate_srp_server_keys(
         srpParams = r_srp_user(generator: generator, prime: prime, verifier: verifier),
         n
       ) do
    try do
      :crypto.generate_key(
        :srp,
        {:host, [verifier, generator, prime, :"6a"]}
      )
    catch
      :error, _ ->
        generate_srp_server_keys(srpParams, n + 1)
    else
      keys ->
        keys
    end
  end

  defp generate_srp_client_keys(_Generator, _Prime, 10) do
    r_alert(
      level: 2,
      description: 47,
      where: %{
        :mfa => {:ssl_connection, :generate_srp_client_keys, 3},
        :line => 2497,
        :file => 'otp/lib/ssl/src/ssl_connection.erl'
      }
    )
  end

  defp generate_srp_client_keys(generator, prime, n) do
    try do
      :crypto.generate_key(
        :srp,
        {:user, [generator, prime, :"6a"]}
      )
    catch
      :error, _ ->
        generate_srp_client_keys(generator, prime, n + 1)
    else
      keys ->
        keys
    end
  end

  defp handle_srp_identity(username, {fun, userState}) do
    case fun.(:srp, username, userState) do
      {:ok, {sRPParams, salt, derivedKey}}
      when is_atom(sRPParams) and is_binary(salt) and
             is_binary(derivedKey) ->
        {generator, prime} = :ssl_srp_primes.get_srp_params(sRPParams)
        verifier = :crypto.mod_pow(generator, derivedKey, prime)
        r_srp_user(generator: generator, prime: prime, salt: salt, verifier: verifier)

      r_alert() = alert ->
        throw(alert)

      _ ->
        throw(
          r_alert(
            level: 2,
            description: 47,
            where: %{
              :mfa => {:ssl_connection, :handle_srp_identity, 2},
              :line => 2519,
              :file => 'otp/lib/ssl/src/ssl_connection.erl'
            }
          )
        )
    end
  end

  defp cipher_role(
         :client,
         data,
         session,
         r_state(connection_states: connectionStates0) = state0,
         connection
       ) do
    connectionStates =
      :ssl_record.set_server_verify_data(
        :current_both,
        data,
        connectionStates0
      )

    {record, state} =
      prepare_connection(
        r_state(state0,
          session: session,
          connection_states: connectionStates
        ),
        connection
      )

    connection.next_event(:connection, record, state, [
      {{:timeout, :handshake}, :infinity, :close}
    ])
  end

  defp cipher_role(
         :server,
         data,
         session,
         r_state(connection_states: connectionStates0) = state0,
         connection
       ) do
    connectionStates1 =
      :ssl_record.set_client_verify_data(
        :current_read,
        data,
        connectionStates0
      )

    {state1, actions} =
      finalize_handshake(
        r_state(state0,
          connection_states: connectionStates1,
          session: session
        ),
        :cipher,
        connection
      )

    {record, state} = prepare_connection(state1, connection)

    connection.next_event(:connection, record, state, [
      {{:timeout, :handshake}, :infinity, :close}
      | actions
    ])
  end

  defp is_anonymous(kexAlg)
       when kexAlg == :dh_anon or
              kexAlg == :ecdh_anon or kexAlg == :psk or
              kexAlg == :dhe_psk or kexAlg == :ecdhe_psk or
              kexAlg == :rsa_psk or kexAlg == :srp_anon do
    true
  end

  defp is_anonymous(_) do
    false
  end

  defp get_current_prf(cStates, direction) do
    %{:security_parameters => secParams} =
      :ssl_record.current_connection_state(
        cStates,
        direction
      )

    r_security_parameters(secParams, :prf_algorithm)
  end

  defp get_pending_prf(cStates, direction) do
    %{:security_parameters => secParams} =
      :ssl_record.pending_connection_state(
        cStates,
        direction
      )

    r_security_parameters(secParams, :prf_algorithm)
  end

  defp opposite_role(:client) do
    :server
  end

  defp opposite_role(:server) do
    :client
  end

  defp record_cb(:tls_connection) do
    :tls_record
  end

  defp record_cb(:dtls_connection) do
    :dtls_record
  end

  defp call(fsmPid, event) do
    try do
      :gen_statem.call(fsmPid, event)
    catch
      :exit, {:noproc, _} ->
        {:error, :closed}

      :exit, {:normal, _} ->
        {:error, :closed}

      :exit, {{:shutdown, _}, _} ->
        {:error, :closed}
    end
  end

  defp get_socket_opts(_, _, _, [], _, acc) do
    {:ok, acc}
  end

  defp get_socket_opts(connection, transport, socket, [:mode | tags], sockOpts, acc) do
    get_socket_opts(connection, transport, socket, tags, sockOpts, [
      {:mode, r_socket_options(sockOpts, :mode)} | acc
    ])
  end

  defp get_socket_opts(connection, transport, socket, [:packet | tags], sockOpts, acc) do
    case r_socket_options(sockOpts, :packet) do
      {type, :headers} ->
        get_socket_opts(connection, transport, socket, tags, sockOpts, [{:packet, type} | acc])

      type ->
        get_socket_opts(connection, transport, socket, tags, sockOpts, [{:packet, type} | acc])
    end
  end

  defp get_socket_opts(connection, transport, socket, [:header | tags], sockOpts, acc) do
    get_socket_opts(connection, transport, socket, tags, sockOpts, [
      {:header, r_socket_options(sockOpts, :header)} | acc
    ])
  end

  defp get_socket_opts(connection, transport, socket, [:active | tags], sockOpts, acc) do
    get_socket_opts(connection, transport, socket, tags, sockOpts, [
      {:active, r_socket_options(sockOpts, :active)} | acc
    ])
  end

  defp get_socket_opts(connection, transport, socket, [tag | tags], sockOpts, acc) do
    case connection.getopts(transport, socket, [tag]) do
      {:ok, [opt]} ->
        get_socket_opts(connection, transport, socket, tags, sockOpts, [opt | acc])

      {:error, reason} ->
        {:error, {:options, {:socket_options, tag, reason}}}
    end
  end

  defp get_socket_opts(_, _, _, opts, _, _) do
    {:error, {:options, {:socket_options, opts, :function_clause}}}
  end

  defp set_socket_opts(_, _, _, [], sockOpts, []) do
    {:ok, sockOpts}
  end

  defp set_socket_opts(connectionCb, transport, socket, [], sockOpts, other) do
    try do
      connectionCb.setopts(transport, socket, other)
    catch
      _, error ->
        {{:error, {:options, {:socket_options, other, error}}}, sockOpts}
    else
      :ok ->
        {:ok, sockOpts}

      {:error, inetError} ->
        {{:error, {:options, {:socket_options, other, inetError}}}, sockOpts}
    end
  end

  defp set_socket_opts(connectionCb, transport, socket, [{:mode, mode} | opts], sockOpts, other)
       when mode == :list or mode == :binary do
    set_socket_opts(
      connectionCb,
      transport,
      socket,
      opts,
      r_socket_options(sockOpts, mode: mode),
      other
    )
  end

  defp set_socket_opts(_, _, _, [{:mode, _} = opt | _], sockOpts, _) do
    {{:error, {:options, {:socket_options, opt}}}, sockOpts}
  end

  defp set_socket_opts(
         connectionCb,
         transport,
         socket,
         [{:packet, packet} | opts],
         sockOpts,
         other
       )
       when packet == :raw or packet == 0 or packet == 1 or
              packet == 2 or packet == 4 or packet == :asn1 or
              packet == :cdr or packet == :sunrm or packet == :fcgi or
              packet == :tpkt or packet == :line or packet == :http or
              packet == :httph or packet == :http_bin or
              packet == :httph_bin do
    set_socket_opts(
      connectionCb,
      transport,
      socket,
      opts,
      r_socket_options(sockOpts, packet: packet),
      other
    )
  end

  defp set_socket_opts(_, _, _, [{:packet, _} = opt | _], sockOpts, _) do
    {{:error, {:options, {:socket_options, opt}}}, sockOpts}
  end

  defp set_socket_opts(
         connectionCb,
         transport,
         socket,
         [{:header, header} | opts],
         sockOpts,
         other
       )
       when is_integer(header) do
    set_socket_opts(
      connectionCb,
      transport,
      socket,
      opts,
      r_socket_options(sockOpts, header: header),
      other
    )
  end

  defp set_socket_opts(_, _, _, [{:header, _} = opt | _], sockOpts, _) do
    {{:error, {:options, {:socket_options, opt}}}, sockOpts}
  end

  defp set_socket_opts(
         connectionCb,
         transport,
         socket,
         [{:active, active} | opts],
         sockOpts,
         other
       )
       when active == :once or active == true or
              active == false do
    set_socket_opts(
      connectionCb,
      transport,
      socket,
      opts,
      r_socket_options(sockOpts, active: active),
      other
    )
  end

  defp set_socket_opts(
         connectionCb,
         transport,
         socket,
         [{:active, active1} = opt | opts],
         sockOpts = r_socket_options(active: active0),
         other
       )
       when active1 >= -32768 and active1 <= 32767 do
    active =
      cond do
        is_integer(active0) and active0 + active1 < -32768 ->
          :error

        is_integer(active0) and active0 + active1 <= 0 ->
          false

        is_integer(active0) and active0 + active1 > 32767 ->
          :error

        active1 <= 0 ->
          false

        is_integer(active0) ->
          active0 + active1

        true ->
          active1
      end

    case active do
      :error ->
        {{:error, {:options, {:socket_options, opt}}}, sockOpts}

      _ ->
        set_socket_opts(
          connectionCb,
          transport,
          socket,
          opts,
          r_socket_options(sockOpts, active: active),
          other
        )
    end
  end

  defp set_socket_opts(_, _, _, [{:active, _} = opt | _], sockOpts, _) do
    {{:error, {:options, {:socket_options, opt}}}, sockOpts}
  end

  defp set_socket_opts(connectionCb, transport, socket, [opt | opts], sockOpts, other) do
    set_socket_opts(connectionCb, transport, socket, opts, sockOpts, [opt | other])
  end

  def hibernate_after(
        :connection = stateName,
        r_state(ssl_options: %{:hibernate_after => hibernateAfter}) = state,
        actions
      ) do
    {:next_state, stateName, state, [{:timeout, hibernateAfter, :hibernate} | actions]}
  end

  def hibernate_after(stateName, state, actions) do
    {:next_state, stateName, state, actions}
  end

  defp terminate_alert(:normal) do
    r_alert(
      level: 1,
      description: 0,
      where: %{
        :mfa => {:ssl_connection, :terminate_alert, 1},
        :line => 2701,
        :file => 'otp/lib/ssl/src/ssl_connection.erl'
      }
    )
  end

  defp terminate_alert({reason, _})
       when reason == :close or
              reason == :shutdown do
    r_alert(
      level: 1,
      description: 0,
      where: %{
        :mfa => {:ssl_connection, :terminate_alert, 1},
        :line => 2704,
        :file => 'otp/lib/ssl/src/ssl_connection.erl'
      }
    )
  end

  defp terminate_alert(_) do
    r_alert(
      level: 2,
      description: 80,
      where: %{
        :mfa => {:ssl_connection, :terminate_alert, 1},
        :line => 2706,
        :file => 'otp/lib/ssl/src/ssl_connection.erl'
      }
    )
  end

  def handle_trusted_certs_db(r_state(ssl_options: %{:cacertfile => <<>>, :cacerts => []})) do
    :ok
  end

  def handle_trusted_certs_db(
        r_state(
          static_env:
            r_static_env(
              cert_db_ref: ref,
              cert_db: certDb
            ),
          ssl_options: %{:cacertfile => <<>>}
        )
      )
      when certDb !== :undefined do
    :ssl_pkix_db.remove_trusted_certs(ref, certDb)
  end

  def handle_trusted_certs_db(r_state(static_env: r_static_env(file_ref_db: :undefined))) do
    :ok
  end

  def handle_trusted_certs_db(
        r_state(
          static_env:
            r_static_env(
              cert_db_ref: ref,
              file_ref_db: refDb
            ),
          ssl_options: %{:cacertfile => file}
        )
      ) do
    case :ssl_pkix_db.ref_count(ref, refDb, -1) do
      0 ->
        :ssl_manager.clean_cert_db(ref, file)

      _ ->
        :ok
    end
  end

  def prepare_connection(
        r_state(
          handshake_env: r_handshake_env(renegotiation: renegotiate),
          start_or_recv_from: recvFrom
        ) = state0,
        connection
      )
      when renegotiate !== {false, :first} and
             recvFrom !== :undefined do
    state = connection.reinit(state0)
    {:no_record, ack_connection(state)}
  end

  def prepare_connection(state0, connection) do
    state = connection.reinit(state0)
    {:no_record, ack_connection(state)}
  end

  defp ack_connection(
         r_state(handshake_env: r_handshake_env(renegotiation: {true, initiater}) = hsEnv) = state
       )
       when initiater == :peer or initiater == :internal do
    r_state(state, handshake_env: r_handshake_env(hsEnv, renegotiation: :undefined))
  end

  defp ack_connection(
         r_state(handshake_env: r_handshake_env(renegotiation: {true, from}) = hsEnv) = state
       ) do
    :gen_statem.reply(from, :ok)
    r_state(state, handshake_env: r_handshake_env(hsEnv, renegotiation: :undefined))
  end

  defp ack_connection(
         r_state(
           handshake_env: r_handshake_env(renegotiation: {false, :first}) = hsEnv,
           start_or_recv_from: startFrom
         ) = state
       )
       when startFrom !== :undefined do
    :gen_statem.reply(startFrom, :connected)

    r_state(state,
      handshake_env: r_handshake_env(hsEnv, renegotiation: :undefined),
      start_or_recv_from: :undefined
    )
  end

  defp ack_connection(state) do
    state
  end

  defp session_handle_params(r_server_ecdh_params(curve: eCCurve), session) do
    r_session(session, ecc: eCCurve)
  end

  defp session_handle_params(_, session) do
    session
  end

  defp handle_session(
         :server,
         %{:reuse_sessions => true},
         _Host,
         _Port,
         trackers,
         r_session(is_resumable: false) = session
       ) do
    tracker =
      :proplists.get_value(
        :session_id_tracker,
        trackers
      )

    server_register_session(
      tracker,
      r_session(session, is_resumable: true)
    )
  end

  defp handle_session(
         role = :client,
         %{:verify => :verify_peer, :reuse_sessions => reuse} = sslOpts,
         host,
         port,
         _,
         r_session(is_resumable: false) = session
       )
       when reuse !== false do
    client_register_session(
      host_id(role, host, sslOpts),
      port,
      r_session(session, is_resumable: true),
      reg_type(reuse)
    )
  end

  defp handle_session(_, _, _, _, _, session) do
    session
  end

  defp reg_type(:save) do
    true
  end

  defp reg_type(true) do
    :unique
  end

  defp client_register_session(host, port, session, save) do
    :ssl_manager.register_session(host, port, session, save)
    session
  end

  defp server_register_session(tracker, session) do
    :ssl_server_session_cache.register_session(
      tracker,
      session
    )

    session
  end

  defp host_id(:client, _Host, %{:server_name_indication => hostname})
       when is_list(hostname) do
    hostname
  end

  defp host_id(_, host, _) do
    host
  end

  defp handle_new_session(
         newId,
         cipherSuite,
         compression,
         r_state(
           static_env: r_static_env(protocol_cb: connection),
           session: session0
         ) = state0
       ) do
    session =
      r_session(session0,
        session_id: newId,
        cipher_suite: cipherSuite,
        compression_method: compression
      )

    connection.next_event(:certify, :no_record, r_state(state0, session: session))
  end

  defp handle_resumed_session(
         sessId,
         r_state(
           static_env:
             r_static_env(
               host: host,
               port: port,
               protocol_cb: connection,
               session_cache: cache,
               session_cache_cb: cacheCb
             ),
           connection_env: r_connection_env(negotiated_version: version),
           connection_states: connectionStates0,
           ssl_options: opts
         ) = state
       ) do
    session =
      case :maps.get(:reuse_session, opts, :undefined) do
        {^sessId, sessionData}
        when is_binary(sessId) and
               is_binary(sessionData) ->
          :erlang.binary_to_term(sessionData, [:safe])

        _Else ->
          cacheCb.lookup(cache, {{host, port}, sessId})
      end

    case :ssl_handshake.master_secret(
           :ssl.tls_version(version),
           session,
           connectionStates0,
           :client
         ) do
      {_, connectionStates} ->
        connection.next_event(
          :abbreviated,
          :no_record,
          r_state(state,
            connection_states: connectionStates,
            session: session
          )
        )

      r_alert() = alert ->
        handle_own_alert(alert, version, :hello, state)
    end
  end

  defp make_premaster_secret({majVer, minVer}, :rsa) do
    rand = :ssl_cipher.random_bytes(48 - 2)
    <<majVer::size(8)-unsigned-big-integer, minVer::size(8)-unsigned-big-integer, rand::binary>>
  end

  defp make_premaster_secret(_, _) do
    :undefined
  end

  defp negotiated_hashsign(:undefined, kexAlg, pubKeyInfo, version) do
    case is_anonymous(kexAlg) do
      true ->
        {:null, :anon}

      false ->
        {pubAlg, _, _} = pubKeyInfo
        :ssl_handshake.select_hashsign_algs(:undefined, pubAlg, version)
    end
  end

  defp negotiated_hashsign(hashSign = {_, _}, _, _, _) do
    hashSign
  end

  defp ssl_options_list(sslOptions) do
    l = :maps.to_list(sslOptions)
    ssl_options_list(l, [])
  end

  defp ssl_options_list([], acc) do
    :lists.reverse(acc)
  end

  defp ssl_options_list([{:protocol, _} | t], acc) do
    ssl_options_list(t, acc)
  end

  defp ssl_options_list([{:erl_dist, _} | t], acc) do
    ssl_options_list(t, acc)
  end

  defp ssl_options_list([{:renegotiate_at, _} | t], acc) do
    ssl_options_list(t, acc)
  end

  defp ssl_options_list([{:max_fragment_length, _} | t], acc) do
    ssl_options_list(t, acc)
  end

  defp ssl_options_list([{:ciphers = key, value} | t], acc) do
    ssl_options_list(
      t,
      [
        {key,
         :lists.map(
           fn suite ->
             :ssl_cipher_format.suite_bin_to_map(suite)
           end,
           value
         )}
        | acc
      ]
    )
  end

  defp ssl_options_list([{key, value} | t], acc) do
    ssl_options_list(t, [{key, value} | acc])
  end

  defp handle_active_option(false, :connection = stateName, to, reply, state) do
    hibernate_after(stateName, state, [{:reply, to, reply}])
  end

  defp handle_active_option(
         _,
         :connection = stateName,
         to,
         _Reply,
         r_state(
           static_env: r_static_env(role: role),
           connection_env: r_connection_env(terminated: true),
           user_data_buffer: {_, 0, _}
         ) = state
       ) do
    alert =
      r_alert(
        level: 2,
        description: 0,
        where: %{
          :mfa => {:ssl_connection, :handle_active_option, 5},
          :line => 2875,
          :file => 'otp/lib/ssl/src/ssl_connection.erl'
        },
        reason: :all_data_deliverd
      )

    handle_normal_shutdown(
      r_alert(alert, role: role),
      stateName,
      r_state(state, start_or_recv_from: to)
    )

    {:stop, {:shutdown, :peer_close}, state}
  end

  defp handle_active_option(
         _,
         :connection = stateName0,
         to,
         reply,
         r_state(
           static_env: r_static_env(protocol_cb: connection),
           user_data_buffer: {_, 0, _}
         ) = state0
       ) do
    case connection.next_event(stateName0, :no_record, state0) do
      {:next_state, stateName, state} ->
        hibernate_after(stateName, state, [{:reply, to, reply}])

      {:next_state, stateName, state, actions} ->
        hibernate_after(stateName, state, [{:reply, to, reply} | actions])

      {:stop, _, _} = stop ->
        stop
    end
  end

  defp handle_active_option(_, stateName, to, reply, r_state(user_data_buffer: {_, 0, _}) = state) do
    {:next_state, stateName, state, [{:reply, to, reply}]}
  end

  defp handle_active_option(
         _,
         stateName0,
         to,
         reply,
         r_state(static_env: r_static_env(protocol_cb: connection)) = state0
       ) do
    case read_application_data(<<>>, state0) do
      {:stop, _, _} = stop ->
        stop

      {record, state1} ->
        case connection.next_event(stateName0, record, state1) do
          {:next_state, stateName, state} ->
            hibernate_after(stateName, state, [{:reply, to, reply}])

          {:next_state, stateName, state, actions} ->
            hibernate_after(stateName, state, [{:reply, to, reply} | actions])

          {:stop, _, _} = stop ->
            stop
        end
    end
  end

  defp get_data(r_socket_options(active: false), :undefined, _Bin) do
    :passive
  end

  defp get_data(r_socket_options(active: active, packet: raw), bytesToRead, bin)
       when raw === :raw or raw === 0 do
    case bin do
      <<_::binary>>
      when active !== false or bytesToRead === 0 ->
        {:ok, bin, <<>>}

      <<data::size(bytesToRead)-binary, rest::binary>> ->
        {:ok, data, rest}

      <<_::binary>> ->
        {:more, bytesToRead}
    end
  end

  defp get_data(r_socket_options(packet: type, packet_size: size), _, bin) do
    packetOpts = [{:packet_size, size}]
    decode_packet(type, bin, packetOpts)
  end

  defp decode_packet({:http, :headers}, buffer, packetOpts) do
    decode_packet(:httph, buffer, packetOpts)
  end

  defp decode_packet({:http_bin, :headers}, buffer, packetOpts) do
    decode_packet(:httph_bin, buffer, packetOpts)
  end

  defp decode_packet(type, buffer, packetOpts) do
    :erlang.decode_packet(type, buffer, packetOpts)
  end

  defp deliver_app_data(
         cPids,
         transport,
         socket,
         r_socket_options(active: active, packet: type) = sOpts,
         data,
         pid,
         from,
         trackers,
         connection
       ) do
    send_or_reply(
      active,
      pid,
      from,
      format_reply(cPids, transport, socket, sOpts, data, trackers, connection)
    )

    sO =
      case data do
        {p, _, _, _}
        when p === :http_request or
               (p === :http_response and
                  type === :http) or type === :http_bin ->
          r_socket_options(sOpts, packet: {type, :headers})

        :http_eoh when tuple_size(type) === 2 ->
          {type1, :headers} = type
          r_socket_options(sOpts, packet: type1)

        _ ->
          sOpts
      end

    case active do
      :once ->
        r_socket_options(sO, active: false)

      1 ->
        send_user(
          pid,
          format_passive(cPids, transport, socket, trackers, connection)
        )

        r_socket_options(sO, active: false)

      n when is_integer(n) ->
        r_socket_options(sO, active: n - 1)

      _ ->
        sO
    end
  end

  defp format_reply(
         _,
         _,
         _,
         r_socket_options(active: false, mode: mode, packet: packet, header: header),
         data,
         _,
         _
       ) do
    {:ok, do_format_reply(mode, packet, header, data)}
  end

  defp format_reply(
         cPids,
         transport,
         socket,
         r_socket_options(active: _, mode: mode, packet: packet, header: header),
         data,
         trackers,
         connection
       ) do
    {:ssl, connection.socket(cPids, transport, socket, trackers),
     do_format_reply(mode, packet, header, data)}
  end

  defp deliver_packet_error(
         cPids,
         transport,
         socket,
         sO = r_socket_options(active: active),
         data,
         pid,
         from,
         trackers,
         connection
       ) do
    send_or_reply(
      active,
      pid,
      from,
      format_packet_error(cPids, transport, socket, sO, data, trackers, connection)
    )
  end

  defp format_packet_error(_, _, _, r_socket_options(active: false, mode: mode), data, _, _) do
    {:error, {:invalid_packet, do_format_reply(mode, :raw, 0, data)}}
  end

  defp format_packet_error(
         cPids,
         transport,
         socket,
         r_socket_options(active: _, mode: mode),
         data,
         trackers,
         connection
       ) do
    {:ssl_error, connection.socket(cPids, transport, socket, trackers),
     {:invalid_packet, do_format_reply(mode, :raw, 0, data)}}
  end

  defp do_format_reply(:binary, _, n, data) when n > 0 do
    header(n, data)
  end

  defp do_format_reply(:binary, _, _, data) do
    data
  end

  defp do_format_reply(:list, packet, _, data)
       when packet == :http or
              packet == {:http, :headers} or
              packet == :http_bin or
              packet == {:http_bin, :headers} or
              packet == :httph or
              packet == :httph_bin do
    data
  end

  defp do_format_reply(:list, _, _, data) do
    :erlang.binary_to_list(data)
  end

  defp format_passive(cPids, transport, socket, trackers, connection) do
    {:ssl_passive, connection.socket(cPids, transport, socket, trackers)}
  end

  defp header(0, <<>>) do
    <<>>
  end

  defp header(_, <<>>) do
    []
  end

  defp header(0, binary) do
    binary
  end

  defp header(n, binary) do
    <<byteN::size(8)-unsigned-big-integer, newBinary::binary>> = binary
    [byteN | header(n - 1, newBinary)]
  end

  defp send_or_reply(false, _Pid, from, data)
       when from !== :undefined do
    :gen_statem.reply(from, data)
  end

  defp send_or_reply(false, pid, :undefined, _) when is_pid(pid) do
    :ok
  end

  defp send_or_reply(_, :no_pid, _, _) do
    :ok
  end

  defp send_or_reply(_, pid, _, data) do
    send_user(pid, data)
  end

  defp send_user(pid, msg) do
    send(pid, msg)
    :ok
  end

  defp alert_user(
         pids,
         transport,
         trackers,
         socket,
         :connection,
         opts,
         pid,
         from,
         alert,
         role,
         stateName,
         connection
       ) do
    alert_user(
      pids,
      transport,
      trackers,
      socket,
      r_socket_options(opts, :active),
      pid,
      from,
      alert,
      role,
      stateName,
      connection
    )
  end

  defp alert_user(
         pids,
         transport,
         trackers,
         socket,
         _,
         _,
         _,
         from,
         alert,
         role,
         stateName,
         connection
       ) do
    alert_user(pids, transport, trackers, socket, from, alert, role, stateName, connection)
  end

  defp alert_user(pids, transport, trackers, socket, from, alert, role, stateName, connection) do
    alert_user(
      pids,
      transport,
      trackers,
      socket,
      false,
      :no_pid,
      from,
      alert,
      role,
      stateName,
      connection
    )
  end

  defp alert_user(_, _, _, _, false = active, pid, from, alert, role, stateName, connection)
       when from !== :undefined do
    reasonCode = :ssl_alert.reason_code(alert, role, connection.protocol_name(), stateName)
    send_or_reply(active, pid, from, {:error, reasonCode})
  end

  defp alert_user(
         pids,
         transport,
         trackers,
         socket,
         active,
         pid,
         from,
         alert,
         role,
         stateName,
         connection
       ) do
    case :ssl_alert.reason_code(alert, role, connection.protocol_name(), stateName) do
      :closed ->
        send_or_reply(
          active,
          pid,
          from,
          {:ssl_closed, connection.socket(pids, transport, socket, trackers)}
        )

      reasonCode ->
        send_or_reply(
          active,
          pid,
          from,
          {:ssl_error, connection.socket(pids, transport, socket, trackers), reasonCode}
        )
    end
  end

  defp log_alert(level, role, protocolName, stateName, r_alert(role: role) = alert) do
    :ssl_logger.log(
      :notice,
      level,
      %{
        :protocol => protocolName,
        :role => role,
        :statename => stateName,
        :alert => alert,
        :alerter => :own
      },
      r_alert(alert, :where)
    )
  end

  defp log_alert(level, role, protocolName, stateName, alert) do
    :ssl_logger.log(
      :notice,
      level,
      %{
        :protocol => protocolName,
        :role => role,
        :statename => stateName,
        :alert => alert,
        :alerter => :peer
      },
      r_alert(alert, :where)
    )
  end

  defp maybe_invalidate_session({false, :first}, :server = role, host, port, session) do
    invalidate_session(role, host, port, session)
  end

  defp maybe_invalidate_session(_, _, _, _, _) do
    :ok
  end

  defp invalidate_session(:client, host, port, session) do
    :ssl_manager.invalidate_session(host, port, session)
  end

  defp invalidate_session(:server, _, _, _) do
    :ok
  end

  def handle_sni_extension_tls13(:undefined, state) do
    {:ok, state}
  end

  def handle_sni_extension_tls13(r_sni(hostname: hostname), state0) do
    case check_hostname(state0, hostname) do
      :valid ->
        state1 = handle_sni_hostname(hostname, state0)
        state = set_sni_guided_cert_selection(state1, true)
        {:ok, state}

      :unrecognized_name ->
        {:ok, handle_sni_hostname(hostname, state0)}

      r_alert() = alert ->
        {:error, alert}
    end
  end

  defp set_sni_guided_cert_selection(r_state(handshake_env: hsEnv0) = state, bool) do
    hsEnv = r_handshake_env(hsEnv0, sni_guided_cert_selection: bool)
    r_state(state, handshake_env: hsEnv)
  end

  defp check_hostname(r_state(ssl_options: sslOptions), hostname) do
    case is_sni_value(hostname) do
      true ->
        case is_hostname_recognized(sslOptions, hostname) do
          true ->
            :valid

          false ->
            :unrecognized_name
        end

      false ->
        r_alert(
          level: 2,
          description: 112,
          where: %{
            :mfa => {:ssl_connection, :check_hostname, 2},
            :line => 3123,
            :file => 'otp/lib/ssl/src/ssl_connection.erl'
          },
          reason: {:sni_included_trailing_dot, hostname}
        )
    end
  end

  defp is_hostname_recognized(
         %{:sni_fun => :undefined, :sni_hosts => sNIHosts},
         hostname
       ) do
    :proplists.is_defined(hostname, sNIHosts)
  end

  defp is_hostname_recognized(_, _) do
    true
  end

  def handle_sni_extension(
        r_state(static_env: r_static_env(protocol_cb: connection)) = state0,
        hello
      ) do
    possibleSNI = connection.select_sni_extension(hello)

    case do_handle_sni_extension(possibleSNI, state0) do
      r_state() = state1 ->
        state1

      r_alert() = alert0 ->
        :ssl_connection.handle_own_alert(alert0, :undefined, :hello, state0)
    end
  end

  defp do_handle_sni_extension(:undefined, state) do
    state
  end

  defp do_handle_sni_extension(
         r_sni(hostname: hostname),
         r_state(ssl_options: sslOptions) = state0
       ) do
    case is_sni_value(hostname) do
      true ->
        case is_hostname_recognized(sslOptions, hostname) do
          true ->
            state1 = handle_sni_hostname(hostname, state0)
            set_sni_guided_cert_selection(state1, true)

          false ->
            handle_sni_hostname(hostname, state0)
        end

      false ->
        r_alert(
          level: 2,
          description: 112,
          where: %{
            :mfa => {:ssl_connection, :do_handle_sni_extension, 2},
            :line => 3163,
            :file => 'otp/lib/ssl/src/ssl_connection.erl'
          },
          reason: {:sni_included_trailing_dot, hostname}
        )
    end
  end

  defp handle_sni_hostname(
         hostname,
         r_state(
           static_env: r_static_env(role: role) = initStatEnv0,
           handshake_env: hsEnv,
           connection_env: cEnv
         ) = state0
       ) do
    newOptions =
      update_ssl_options_from_sni(
        r_state(state0, :ssl_options),
        hostname
      )

    case newOptions do
      :undefined ->
        state0

      _ ->
        {:ok,
         %{
           :cert_db_ref => ref,
           :cert_db_handle => certDbHandle,
           :fileref_db_handle => fileRefHandle,
           :session_cache => cacheHandle,
           :crl_db_info => cRLDbHandle,
           :private_key => key,
           :dh_params => dHParams,
           :own_certificate => ownCert
         }} = :ssl_config.init(newOptions, role)

        r_state(state0,
          session: r_session(r_state(state0, :session), own_certificate: ownCert),
          static_env:
            r_static_env(initStatEnv0,
              file_ref_db: fileRefHandle,
              cert_db_ref: ref,
              cert_db: certDbHandle,
              crl_db: cRLDbHandle,
              session_cache: cacheHandle
            ),
          connection_env: r_connection_env(cEnv, private_key: key),
          ssl_options: newOptions,
          handshake_env:
            r_handshake_env(hsEnv,
              sni_hostname: hostname,
              diffie_hellman_params: dHParams
            )
        )
    end
  end

  defp update_ssl_options_from_sni(
         %{:sni_fun => sNIFun, :sni_hosts => sNIHosts} = origSSLOptions,
         sNIHostname
       ) do
    sSLOption =
      case sNIFun do
        :undefined ->
          :proplists.get_value(sNIHostname, sNIHosts)

        ^sNIFun ->
          sNIFun.(sNIHostname)
      end

    case sSLOption do
      :undefined ->
        :undefined

      _ ->
        :ssl.handle_options(sSLOption, :server, origSSLOptions)
    end
  end

  defp new_emulated([], emOpts) do
    emOpts
  end

  defp new_emulated(newEmOpts, _) do
    newEmOpts
  end

  defp no_records(extensions) do
    :maps.map(
      fn _, value ->
        :ssl_handshake.extension_value(value)
      end,
      extensions
    )
  end

  defp is_sni_value(hostname) do
    case hd(:lists.reverse(hostname)) do
      ?. ->
        false

      _ ->
        true
    end
  end

  defp ensure_tls({254, _} = version) do
    :dtls_v1.corresponding_tls_version(version)
  end

  defp ensure_tls(version) do
    version
  end

  defp ocsp_info(
         %{:ocsp_expect => :stapled, :ocsp_response => certStatus} = ocspState,
         %{:ocsp_responder_certs => ocspResponderCerts},
         peerCert
       ) do
    %{
      :cert_ext => %{:public_key.pkix_subject_id(peerCert) => [certStatus]},
      :ocsp_responder_certs => ocspResponderCerts,
      :ocsp_state => ocspState
    }
  end

  defp ocsp_info(%{:ocsp_expect => :no_staple} = ocspState, _, peerCert) do
    %{
      :cert_ext => %{:public_key.pkix_subject_id(peerCert) => []},
      :ocsp_responder_certs => [],
      :ocsp_state => ocspState
    }
  end
end
