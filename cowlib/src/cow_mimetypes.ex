defmodule :cow_mimetypes do
  use Bitwise

  def all(path) do
    case :filename.extension(path) do
      <<>> ->
        {"application", "octet-stream", []}

      <<?., ext::binary>> ->
        all_ext(:erlang.list_to_binary(:string.to_lower(:erlang.binary_to_list(ext))))
    end
  end

  def web(path) do
    case :filename.extension(path) do
      <<>> ->
        {"application", "octet-stream", []}

      <<?., ext::binary>> ->
        web_ext(:erlang.list_to_binary(:string.to_lower(:erlang.binary_to_list(ext))))
    end
  end

  defp all_ext("123") do
    {"application", "vnd.lotus-1-2-3", []}
  end

  defp all_ext("3dml") do
    {"text", "vnd.in3d.3dml", []}
  end

  defp all_ext("3ds") do
    {"image", "x-3ds", []}
  end

  defp all_ext("3g2") do
    {"video", "3gpp2", []}
  end

  defp all_ext("3gp") do
    {"video", "3gpp", []}
  end

  defp all_ext("7z") do
    {"application", "x-7z-compressed", []}
  end

  defp all_ext("aab") do
    {"application", "x-authorware-bin", []}
  end

  defp all_ext("aac") do
    {"audio", "x-aac", []}
  end

  defp all_ext("aam") do
    {"application", "x-authorware-map", []}
  end

  defp all_ext("aas") do
    {"application", "x-authorware-seg", []}
  end

  defp all_ext("abw") do
    {"application", "x-abiword", []}
  end

  defp all_ext("ac") do
    {"application", "pkix-attr-cert", []}
  end

  defp all_ext("acc") do
    {"application", "vnd.americandynamics.acc", []}
  end

  defp all_ext("ace") do
    {"application", "x-ace-compressed", []}
  end

  defp all_ext("acu") do
    {"application", "vnd.acucobol", []}
  end

  defp all_ext("acutc") do
    {"application", "vnd.acucorp", []}
  end

  defp all_ext("adp") do
    {"audio", "adpcm", []}
  end

  defp all_ext("aep") do
    {"application", "vnd.audiograph", []}
  end

  defp all_ext("afm") do
    {"application", "x-font-type1", []}
  end

  defp all_ext("afp") do
    {"application", "vnd.ibm.modcap", []}
  end

  defp all_ext("ahead") do
    {"application", "vnd.ahead.space", []}
  end

  defp all_ext("ai") do
    {"application", "postscript", []}
  end

  defp all_ext("aif") do
    {"audio", "x-aiff", []}
  end

  defp all_ext("aifc") do
    {"audio", "x-aiff", []}
  end

  defp all_ext("aiff") do
    {"audio", "x-aiff", []}
  end

  defp all_ext("air") do
    {"application", "vnd.adobe.air-application-installer-package+zip", []}
  end

  defp all_ext("ait") do
    {"application", "vnd.dvb.ait", []}
  end

  defp all_ext("ami") do
    {"application", "vnd.amiga.ami", []}
  end

  defp all_ext("apk") do
    {"application", "vnd.android.package-archive", []}
  end

  defp all_ext("appcache") do
    {"text", "cache-manifest", []}
  end

  defp all_ext("application") do
    {"application", "x-ms-application", []}
  end

  defp all_ext("apr") do
    {"application", "vnd.lotus-approach", []}
  end

  defp all_ext("arc") do
    {"application", "x-freearc", []}
  end

  defp all_ext("asc") do
    {"application", "pgp-signature", []}
  end

  defp all_ext("asf") do
    {"video", "x-ms-asf", []}
  end

  defp all_ext("asm") do
    {"text", "x-asm", []}
  end

  defp all_ext("aso") do
    {"application", "vnd.accpac.simply.aso", []}
  end

  defp all_ext("asx") do
    {"video", "x-ms-asf", []}
  end

  defp all_ext("atc") do
    {"application", "vnd.acucorp", []}
  end

  defp all_ext("atom") do
    {"application", "atom+xml", []}
  end

  defp all_ext("atomcat") do
    {"application", "atomcat+xml", []}
  end

  defp all_ext("atomsvc") do
    {"application", "atomsvc+xml", []}
  end

  defp all_ext("atx") do
    {"application", "vnd.antix.game-component", []}
  end

  defp all_ext("au") do
    {"audio", "basic", []}
  end

  defp all_ext("avi") do
    {"video", "x-msvideo", []}
  end

  defp all_ext("aw") do
    {"application", "applixware", []}
  end

  defp all_ext("azf") do
    {"application", "vnd.airzip.filesecure.azf", []}
  end

  defp all_ext("azs") do
    {"application", "vnd.airzip.filesecure.azs", []}
  end

  defp all_ext("azw") do
    {"application", "vnd.amazon.ebook", []}
  end

  defp all_ext("bat") do
    {"application", "x-msdownload", []}
  end

  defp all_ext("bcpio") do
    {"application", "x-bcpio", []}
  end

  defp all_ext("bdf") do
    {"application", "x-font-bdf", []}
  end

  defp all_ext("bdm") do
    {"application", "vnd.syncml.dm+wbxml", []}
  end

  defp all_ext("bed") do
    {"application", "vnd.realvnc.bed", []}
  end

  defp all_ext("bh2") do
    {"application", "vnd.fujitsu.oasysprs", []}
  end

  defp all_ext("bin") do
    {"application", "octet-stream", []}
  end

  defp all_ext("blb") do
    {"application", "x-blorb", []}
  end

  defp all_ext("blorb") do
    {"application", "x-blorb", []}
  end

  defp all_ext("bmi") do
    {"application", "vnd.bmi", []}
  end

  defp all_ext("bmp") do
    {"image", "bmp", []}
  end

  defp all_ext("book") do
    {"application", "vnd.framemaker", []}
  end

  defp all_ext("box") do
    {"application", "vnd.previewsystems.box", []}
  end

  defp all_ext("boz") do
    {"application", "x-bzip2", []}
  end

  defp all_ext("bpk") do
    {"application", "octet-stream", []}
  end

  defp all_ext("btif") do
    {"image", "prs.btif", []}
  end

  defp all_ext("bz2") do
    {"application", "x-bzip2", []}
  end

  defp all_ext("bz") do
    {"application", "x-bzip", []}
  end

  defp all_ext("c11amc") do
    {"application", "vnd.cluetrust.cartomobile-config", []}
  end

  defp all_ext("c11amz") do
    {"application", "vnd.cluetrust.cartomobile-config-pkg", []}
  end

  defp all_ext("c4d") do
    {"application", "vnd.clonk.c4group", []}
  end

  defp all_ext("c4f") do
    {"application", "vnd.clonk.c4group", []}
  end

  defp all_ext("c4g") do
    {"application", "vnd.clonk.c4group", []}
  end

  defp all_ext("c4p") do
    {"application", "vnd.clonk.c4group", []}
  end

  defp all_ext("c4u") do
    {"application", "vnd.clonk.c4group", []}
  end

  defp all_ext("cab") do
    {"application", "vnd.ms-cab-compressed", []}
  end

  defp all_ext("caf") do
    {"audio", "x-caf", []}
  end

  defp all_ext("cap") do
    {"application", "vnd.tcpdump.pcap", []}
  end

  defp all_ext("car") do
    {"application", "vnd.curl.car", []}
  end

  defp all_ext("cat") do
    {"application", "vnd.ms-pki.seccat", []}
  end

  defp all_ext("cb7") do
    {"application", "x-cbr", []}
  end

  defp all_ext("cba") do
    {"application", "x-cbr", []}
  end

  defp all_ext("cbr") do
    {"application", "x-cbr", []}
  end

  defp all_ext("cbt") do
    {"application", "x-cbr", []}
  end

  defp all_ext("cbz") do
    {"application", "x-cbr", []}
  end

  defp all_ext("cct") do
    {"application", "x-director", []}
  end

  defp all_ext("cc") do
    {"text", "x-c", []}
  end

  defp all_ext("ccxml") do
    {"application", "ccxml+xml", []}
  end

  defp all_ext("cdbcmsg") do
    {"application", "vnd.contact.cmsg", []}
  end

  defp all_ext("cdf") do
    {"application", "x-netcdf", []}
  end

  defp all_ext("cdkey") do
    {"application", "vnd.mediastation.cdkey", []}
  end

  defp all_ext("cdmia") do
    {"application", "cdmi-capability", []}
  end

  defp all_ext("cdmic") do
    {"application", "cdmi-container", []}
  end

  defp all_ext("cdmid") do
    {"application", "cdmi-domain", []}
  end

  defp all_ext("cdmio") do
    {"application", "cdmi-object", []}
  end

  defp all_ext("cdmiq") do
    {"application", "cdmi-queue", []}
  end

  defp all_ext("cdx") do
    {"chemical", "x-cdx", []}
  end

  defp all_ext("cdxml") do
    {"application", "vnd.chemdraw+xml", []}
  end

  defp all_ext("cdy") do
    {"application", "vnd.cinderella", []}
  end

  defp all_ext("cer") do
    {"application", "pkix-cert", []}
  end

  defp all_ext("cfs") do
    {"application", "x-cfs-compressed", []}
  end

  defp all_ext("cgm") do
    {"image", "cgm", []}
  end

  defp all_ext("chat") do
    {"application", "x-chat", []}
  end

  defp all_ext("chm") do
    {"application", "vnd.ms-htmlhelp", []}
  end

  defp all_ext("chrt") do
    {"application", "vnd.kde.kchart", []}
  end

  defp all_ext("cif") do
    {"chemical", "x-cif", []}
  end

  defp all_ext("cii") do
    {"application", "vnd.anser-web-certificate-issue-initiation", []}
  end

  defp all_ext("cil") do
    {"application", "vnd.ms-artgalry", []}
  end

  defp all_ext("cla") do
    {"application", "vnd.claymore", []}
  end

  defp all_ext("class") do
    {"application", "java-vm", []}
  end

  defp all_ext("clkk") do
    {"application", "vnd.crick.clicker.keyboard", []}
  end

  defp all_ext("clkp") do
    {"application", "vnd.crick.clicker.palette", []}
  end

  defp all_ext("clkt") do
    {"application", "vnd.crick.clicker.template", []}
  end

  defp all_ext("clkw") do
    {"application", "vnd.crick.clicker.wordbank", []}
  end

  defp all_ext("clkx") do
    {"application", "vnd.crick.clicker", []}
  end

  defp all_ext("clp") do
    {"application", "x-msclip", []}
  end

  defp all_ext("cmc") do
    {"application", "vnd.cosmocaller", []}
  end

  defp all_ext("cmdf") do
    {"chemical", "x-cmdf", []}
  end

  defp all_ext("cml") do
    {"chemical", "x-cml", []}
  end

  defp all_ext("cmp") do
    {"application", "vnd.yellowriver-custom-menu", []}
  end

  defp all_ext("cmx") do
    {"image", "x-cmx", []}
  end

  defp all_ext("cod") do
    {"application", "vnd.rim.cod", []}
  end

  defp all_ext("com") do
    {"application", "x-msdownload", []}
  end

  defp all_ext("conf") do
    {"text", "plain", []}
  end

  defp all_ext("cpio") do
    {"application", "x-cpio", []}
  end

  defp all_ext("cpp") do
    {"text", "x-c", []}
  end

  defp all_ext("cpt") do
    {"application", "mac-compactpro", []}
  end

  defp all_ext("crd") do
    {"application", "x-mscardfile", []}
  end

  defp all_ext("crl") do
    {"application", "pkix-crl", []}
  end

  defp all_ext("crt") do
    {"application", "x-x509-ca-cert", []}
  end

  defp all_ext("cryptonote") do
    {"application", "vnd.rig.cryptonote", []}
  end

  defp all_ext("csh") do
    {"application", "x-csh", []}
  end

  defp all_ext("csml") do
    {"chemical", "x-csml", []}
  end

  defp all_ext("csp") do
    {"application", "vnd.commonspace", []}
  end

  defp all_ext("css") do
    {"text", "css", []}
  end

  defp all_ext("cst") do
    {"application", "x-director", []}
  end

  defp all_ext("csv") do
    {"text", "csv", []}
  end

  defp all_ext("c") do
    {"text", "x-c", []}
  end

  defp all_ext("cu") do
    {"application", "cu-seeme", []}
  end

  defp all_ext("curl") do
    {"text", "vnd.curl", []}
  end

  defp all_ext("cww") do
    {"application", "prs.cww", []}
  end

  defp all_ext("cxt") do
    {"application", "x-director", []}
  end

  defp all_ext("cxx") do
    {"text", "x-c", []}
  end

  defp all_ext("dae") do
    {"model", "vnd.collada+xml", []}
  end

  defp all_ext("daf") do
    {"application", "vnd.mobius.daf", []}
  end

  defp all_ext("dart") do
    {"application", "vnd.dart", []}
  end

  defp all_ext("dataless") do
    {"application", "vnd.fdsn.seed", []}
  end

  defp all_ext("davmount") do
    {"application", "davmount+xml", []}
  end

  defp all_ext("dbk") do
    {"application", "docbook+xml", []}
  end

  defp all_ext("dcr") do
    {"application", "x-director", []}
  end

  defp all_ext("dcurl") do
    {"text", "vnd.curl.dcurl", []}
  end

  defp all_ext("dd2") do
    {"application", "vnd.oma.dd2+xml", []}
  end

  defp all_ext("ddd") do
    {"application", "vnd.fujixerox.ddd", []}
  end

  defp all_ext("deb") do
    {"application", "x-debian-package", []}
  end

  defp all_ext("def") do
    {"text", "plain", []}
  end

  defp all_ext("deploy") do
    {"application", "octet-stream", []}
  end

  defp all_ext("der") do
    {"application", "x-x509-ca-cert", []}
  end

  defp all_ext("dfac") do
    {"application", "vnd.dreamfactory", []}
  end

  defp all_ext("dgc") do
    {"application", "x-dgc-compressed", []}
  end

  defp all_ext("dic") do
    {"text", "x-c", []}
  end

  defp all_ext("dir") do
    {"application", "x-director", []}
  end

  defp all_ext("dis") do
    {"application", "vnd.mobius.dis", []}
  end

  defp all_ext("dist") do
    {"application", "octet-stream", []}
  end

  defp all_ext("distz") do
    {"application", "octet-stream", []}
  end

  defp all_ext("djv") do
    {"image", "vnd.djvu", []}
  end

  defp all_ext("djvu") do
    {"image", "vnd.djvu", []}
  end

  defp all_ext("dll") do
    {"application", "x-msdownload", []}
  end

  defp all_ext("dmg") do
    {"application", "x-apple-diskimage", []}
  end

  defp all_ext("dmp") do
    {"application", "vnd.tcpdump.pcap", []}
  end

  defp all_ext("dms") do
    {"application", "octet-stream", []}
  end

  defp all_ext("dna") do
    {"application", "vnd.dna", []}
  end

  defp all_ext("doc") do
    {"application", "msword", []}
  end

  defp all_ext("docm") do
    {"application", "vnd.ms-word.document.macroenabled.12", []}
  end

  defp all_ext("docx") do
    {"application", "vnd.openxmlformats-officedocument.wordprocessingml.document", []}
  end

  defp all_ext("dot") do
    {"application", "msword", []}
  end

  defp all_ext("dotm") do
    {"application", "vnd.ms-word.template.macroenabled.12", []}
  end

  defp all_ext("dotx") do
    {"application", "vnd.openxmlformats-officedocument.wordprocessingml.template", []}
  end

  defp all_ext("dp") do
    {"application", "vnd.osgi.dp", []}
  end

  defp all_ext("dpg") do
    {"application", "vnd.dpgraph", []}
  end

  defp all_ext("dra") do
    {"audio", "vnd.dra", []}
  end

  defp all_ext("dsc") do
    {"text", "prs.lines.tag", []}
  end

  defp all_ext("dssc") do
    {"application", "dssc+der", []}
  end

  defp all_ext("dtb") do
    {"application", "x-dtbook+xml", []}
  end

  defp all_ext("dtd") do
    {"application", "xml-dtd", []}
  end

  defp all_ext("dts") do
    {"audio", "vnd.dts", []}
  end

  defp all_ext("dtshd") do
    {"audio", "vnd.dts.hd", []}
  end

  defp all_ext("dump") do
    {"application", "octet-stream", []}
  end

  defp all_ext("dvb") do
    {"video", "vnd.dvb.file", []}
  end

  defp all_ext("dvi") do
    {"application", "x-dvi", []}
  end

  defp all_ext("dwf") do
    {"model", "vnd.dwf", []}
  end

  defp all_ext("dwg") do
    {"image", "vnd.dwg", []}
  end

  defp all_ext("dxf") do
    {"image", "vnd.dxf", []}
  end

  defp all_ext("dxp") do
    {"application", "vnd.spotfire.dxp", []}
  end

  defp all_ext("dxr") do
    {"application", "x-director", []}
  end

  defp all_ext("ecelp4800") do
    {"audio", "vnd.nuera.ecelp4800", []}
  end

  defp all_ext("ecelp7470") do
    {"audio", "vnd.nuera.ecelp7470", []}
  end

  defp all_ext("ecelp9600") do
    {"audio", "vnd.nuera.ecelp9600", []}
  end

  defp all_ext("ecma") do
    {"application", "ecmascript", []}
  end

  defp all_ext("edm") do
    {"application", "vnd.novadigm.edm", []}
  end

  defp all_ext("edx") do
    {"application", "vnd.novadigm.edx", []}
  end

  defp all_ext("efif") do
    {"application", "vnd.picsel", []}
  end

  defp all_ext("ei6") do
    {"application", "vnd.pg.osasli", []}
  end

  defp all_ext("elc") do
    {"application", "octet-stream", []}
  end

  defp all_ext("emf") do
    {"application", "x-msmetafile", []}
  end

  defp all_ext("eml") do
    {"message", "rfc822", []}
  end

  defp all_ext("emma") do
    {"application", "emma+xml", []}
  end

  defp all_ext("emz") do
    {"application", "x-msmetafile", []}
  end

  defp all_ext("eol") do
    {"audio", "vnd.digital-winds", []}
  end

  defp all_ext("eot") do
    {"application", "vnd.ms-fontobject", []}
  end

  defp all_ext("eps") do
    {"application", "postscript", []}
  end

  defp all_ext("epub") do
    {"application", "epub+zip", []}
  end

  defp all_ext("es3") do
    {"application", "vnd.eszigno3+xml", []}
  end

  defp all_ext("esa") do
    {"application", "vnd.osgi.subsystem", []}
  end

  defp all_ext("esf") do
    {"application", "vnd.epson.esf", []}
  end

  defp all_ext("et3") do
    {"application", "vnd.eszigno3+xml", []}
  end

  defp all_ext("etx") do
    {"text", "x-setext", []}
  end

  defp all_ext("eva") do
    {"application", "x-eva", []}
  end

  defp all_ext("evy") do
    {"application", "x-envoy", []}
  end

  defp all_ext("exe") do
    {"application", "x-msdownload", []}
  end

  defp all_ext("exi") do
    {"application", "exi", []}
  end

  defp all_ext("ext") do
    {"application", "vnd.novadigm.ext", []}
  end

  defp all_ext("ez2") do
    {"application", "vnd.ezpix-album", []}
  end

  defp all_ext("ez3") do
    {"application", "vnd.ezpix-package", []}
  end

  defp all_ext("ez") do
    {"application", "andrew-inset", []}
  end

  defp all_ext("f4v") do
    {"video", "x-f4v", []}
  end

  defp all_ext("f77") do
    {"text", "x-fortran", []}
  end

  defp all_ext("f90") do
    {"text", "x-fortran", []}
  end

  defp all_ext("fbs") do
    {"image", "vnd.fastbidsheet", []}
  end

  defp all_ext("fcdt") do
    {"application", "vnd.adobe.formscentral.fcdt", []}
  end

  defp all_ext("fcs") do
    {"application", "vnd.isac.fcs", []}
  end

  defp all_ext("fdf") do
    {"application", "vnd.fdf", []}
  end

  defp all_ext("fe_launch") do
    {"application", "vnd.denovo.fcselayout-link", []}
  end

  defp all_ext("fg5") do
    {"application", "vnd.fujitsu.oasysgp", []}
  end

  defp all_ext("fgd") do
    {"application", "x-director", []}
  end

  defp all_ext("fh4") do
    {"image", "x-freehand", []}
  end

  defp all_ext("fh5") do
    {"image", "x-freehand", []}
  end

  defp all_ext("fh7") do
    {"image", "x-freehand", []}
  end

  defp all_ext("fhc") do
    {"image", "x-freehand", []}
  end

  defp all_ext("fh") do
    {"image", "x-freehand", []}
  end

  defp all_ext("fig") do
    {"application", "x-xfig", []}
  end

  defp all_ext("flac") do
    {"audio", "x-flac", []}
  end

  defp all_ext("fli") do
    {"video", "x-fli", []}
  end

  defp all_ext("flo") do
    {"application", "vnd.micrografx.flo", []}
  end

  defp all_ext("flv") do
    {"video", "x-flv", []}
  end

  defp all_ext("flw") do
    {"application", "vnd.kde.kivio", []}
  end

  defp all_ext("flx") do
    {"text", "vnd.fmi.flexstor", []}
  end

  defp all_ext("fly") do
    {"text", "vnd.fly", []}
  end

  defp all_ext("fm") do
    {"application", "vnd.framemaker", []}
  end

  defp all_ext("fnc") do
    {"application", "vnd.frogans.fnc", []}
  end

  defp all_ext("for") do
    {"text", "x-fortran", []}
  end

  defp all_ext("fpx") do
    {"image", "vnd.fpx", []}
  end

  defp all_ext("frame") do
    {"application", "vnd.framemaker", []}
  end

  defp all_ext("fsc") do
    {"application", "vnd.fsc.weblaunch", []}
  end

  defp all_ext("fst") do
    {"image", "vnd.fst", []}
  end

  defp all_ext("ftc") do
    {"application", "vnd.fluxtime.clip", []}
  end

  defp all_ext("f") do
    {"text", "x-fortran", []}
  end

  defp all_ext("fti") do
    {"application", "vnd.anser-web-funds-transfer-initiation", []}
  end

  defp all_ext("fvt") do
    {"video", "vnd.fvt", []}
  end

  defp all_ext("fxp") do
    {"application", "vnd.adobe.fxp", []}
  end

  defp all_ext("fxpl") do
    {"application", "vnd.adobe.fxp", []}
  end

  defp all_ext("fzs") do
    {"application", "vnd.fuzzysheet", []}
  end

  defp all_ext("g2w") do
    {"application", "vnd.geoplan", []}
  end

  defp all_ext("g3") do
    {"image", "g3fax", []}
  end

  defp all_ext("g3w") do
    {"application", "vnd.geospace", []}
  end

  defp all_ext("gac") do
    {"application", "vnd.groove-account", []}
  end

  defp all_ext("gam") do
    {"application", "x-tads", []}
  end

  defp all_ext("gbr") do
    {"application", "rpki-ghostbusters", []}
  end

  defp all_ext("gca") do
    {"application", "x-gca-compressed", []}
  end

  defp all_ext("gdl") do
    {"model", "vnd.gdl", []}
  end

  defp all_ext("geo") do
    {"application", "vnd.dynageo", []}
  end

  defp all_ext("gex") do
    {"application", "vnd.geometry-explorer", []}
  end

  defp all_ext("ggb") do
    {"application", "vnd.geogebra.file", []}
  end

  defp all_ext("ggt") do
    {"application", "vnd.geogebra.tool", []}
  end

  defp all_ext("ghf") do
    {"application", "vnd.groove-help", []}
  end

  defp all_ext("gif") do
    {"image", "gif", []}
  end

  defp all_ext("gim") do
    {"application", "vnd.groove-identity-message", []}
  end

  defp all_ext("gml") do
    {"application", "gml+xml", []}
  end

  defp all_ext("gmx") do
    {"application", "vnd.gmx", []}
  end

  defp all_ext("gnumeric") do
    {"application", "x-gnumeric", []}
  end

  defp all_ext("gph") do
    {"application", "vnd.flographit", []}
  end

  defp all_ext("gpx") do
    {"application", "gpx+xml", []}
  end

  defp all_ext("gqf") do
    {"application", "vnd.grafeq", []}
  end

  defp all_ext("gqs") do
    {"application", "vnd.grafeq", []}
  end

  defp all_ext("gram") do
    {"application", "srgs", []}
  end

  defp all_ext("gramps") do
    {"application", "x-gramps-xml", []}
  end

  defp all_ext("gre") do
    {"application", "vnd.geometry-explorer", []}
  end

  defp all_ext("grv") do
    {"application", "vnd.groove-injector", []}
  end

  defp all_ext("grxml") do
    {"application", "srgs+xml", []}
  end

  defp all_ext("gsf") do
    {"application", "x-font-ghostscript", []}
  end

  defp all_ext("gtar") do
    {"application", "x-gtar", []}
  end

  defp all_ext("gtm") do
    {"application", "vnd.groove-tool-message", []}
  end

  defp all_ext("gtw") do
    {"model", "vnd.gtw", []}
  end

  defp all_ext("gv") do
    {"text", "vnd.graphviz", []}
  end

  defp all_ext("gxf") do
    {"application", "gxf", []}
  end

  defp all_ext("gxt") do
    {"application", "vnd.geonext", []}
  end

  defp all_ext("h261") do
    {"video", "h261", []}
  end

  defp all_ext("h263") do
    {"video", "h263", []}
  end

  defp all_ext("h264") do
    {"video", "h264", []}
  end

  defp all_ext("hal") do
    {"application", "vnd.hal+xml", []}
  end

  defp all_ext("hbci") do
    {"application", "vnd.hbci", []}
  end

  defp all_ext("hdf") do
    {"application", "x-hdf", []}
  end

  defp all_ext("hh") do
    {"text", "x-c", []}
  end

  defp all_ext("hlp") do
    {"application", "winhlp", []}
  end

  defp all_ext("hpgl") do
    {"application", "vnd.hp-hpgl", []}
  end

  defp all_ext("hpid") do
    {"application", "vnd.hp-hpid", []}
  end

  defp all_ext("hps") do
    {"application", "vnd.hp-hps", []}
  end

  defp all_ext("hqx") do
    {"application", "mac-binhex40", []}
  end

  defp all_ext("h") do
    {"text", "x-c", []}
  end

  defp all_ext("htke") do
    {"application", "vnd.kenameaapp", []}
  end

  defp all_ext("html") do
    {"text", "html", []}
  end

  defp all_ext("htm") do
    {"text", "html", []}
  end

  defp all_ext("hvd") do
    {"application", "vnd.yamaha.hv-dic", []}
  end

  defp all_ext("hvp") do
    {"application", "vnd.yamaha.hv-voice", []}
  end

  defp all_ext("hvs") do
    {"application", "vnd.yamaha.hv-script", []}
  end

  defp all_ext("i2g") do
    {"application", "vnd.intergeo", []}
  end

  defp all_ext("icc") do
    {"application", "vnd.iccprofile", []}
  end

  defp all_ext("ice") do
    {"x-conference", "x-cooltalk", []}
  end

  defp all_ext("icm") do
    {"application", "vnd.iccprofile", []}
  end

  defp all_ext("ico") do
    {"image", "x-icon", []}
  end

  defp all_ext("ics") do
    {"text", "calendar", []}
  end

  defp all_ext("ief") do
    {"image", "ief", []}
  end

  defp all_ext("ifb") do
    {"text", "calendar", []}
  end

  defp all_ext("ifm") do
    {"application", "vnd.shana.informed.formdata", []}
  end

  defp all_ext("iges") do
    {"model", "iges", []}
  end

  defp all_ext("igl") do
    {"application", "vnd.igloader", []}
  end

  defp all_ext("igm") do
    {"application", "vnd.insors.igm", []}
  end

  defp all_ext("igs") do
    {"model", "iges", []}
  end

  defp all_ext("igx") do
    {"application", "vnd.micrografx.igx", []}
  end

  defp all_ext("iif") do
    {"application", "vnd.shana.informed.interchange", []}
  end

  defp all_ext("imp") do
    {"application", "vnd.accpac.simply.imp", []}
  end

  defp all_ext("ims") do
    {"application", "vnd.ms-ims", []}
  end

  defp all_ext("ink") do
    {"application", "inkml+xml", []}
  end

  defp all_ext("inkml") do
    {"application", "inkml+xml", []}
  end

  defp all_ext("install") do
    {"application", "x-install-instructions", []}
  end

  defp all_ext("in") do
    {"text", "plain", []}
  end

  defp all_ext("iota") do
    {"application", "vnd.astraea-software.iota", []}
  end

  defp all_ext("ipfix") do
    {"application", "ipfix", []}
  end

  defp all_ext("ipk") do
    {"application", "vnd.shana.informed.package", []}
  end

  defp all_ext("irm") do
    {"application", "vnd.ibm.rights-management", []}
  end

  defp all_ext("irp") do
    {"application", "vnd.irepository.package+xml", []}
  end

  defp all_ext("iso") do
    {"application", "x-iso9660-image", []}
  end

  defp all_ext("itp") do
    {"application", "vnd.shana.informed.formtemplate", []}
  end

  defp all_ext("ivp") do
    {"application", "vnd.immervision-ivp", []}
  end

  defp all_ext("ivu") do
    {"application", "vnd.immervision-ivu", []}
  end

  defp all_ext("jad") do
    {"text", "vnd.sun.j2me.app-descriptor", []}
  end

  defp all_ext("jam") do
    {"application", "vnd.jam", []}
  end

  defp all_ext("jar") do
    {"application", "java-archive", []}
  end

  defp all_ext("java") do
    {"text", "x-java-source", []}
  end

  defp all_ext("jisp") do
    {"application", "vnd.jisp", []}
  end

  defp all_ext("jlt") do
    {"application", "vnd.hp-jlyt", []}
  end

  defp all_ext("jnlp") do
    {"application", "x-java-jnlp-file", []}
  end

  defp all_ext("joda") do
    {"application", "vnd.joost.joda-archive", []}
  end

  defp all_ext("jpeg") do
    {"image", "jpeg", []}
  end

  defp all_ext("jpe") do
    {"image", "jpeg", []}
  end

  defp all_ext("jpg") do
    {"image", "jpeg", []}
  end

  defp all_ext("jpgm") do
    {"video", "jpm", []}
  end

  defp all_ext("jpgv") do
    {"video", "jpeg", []}
  end

  defp all_ext("jpm") do
    {"video", "jpm", []}
  end

  defp all_ext("js") do
    {"application", "javascript", []}
  end

  defp all_ext("json") do
    {"application", "json", []}
  end

  defp all_ext("jsonml") do
    {"application", "jsonml+json", []}
  end

  defp all_ext("kar") do
    {"audio", "midi", []}
  end

  defp all_ext("karbon") do
    {"application", "vnd.kde.karbon", []}
  end

  defp all_ext("kfo") do
    {"application", "vnd.kde.kformula", []}
  end

  defp all_ext("kia") do
    {"application", "vnd.kidspiration", []}
  end

  defp all_ext("kml") do
    {"application", "vnd.google-earth.kml+xml", []}
  end

  defp all_ext("kmz") do
    {"application", "vnd.google-earth.kmz", []}
  end

  defp all_ext("kne") do
    {"application", "vnd.kinar", []}
  end

  defp all_ext("knp") do
    {"application", "vnd.kinar", []}
  end

  defp all_ext("kon") do
    {"application", "vnd.kde.kontour", []}
  end

  defp all_ext("kpr") do
    {"application", "vnd.kde.kpresenter", []}
  end

  defp all_ext("kpt") do
    {"application", "vnd.kde.kpresenter", []}
  end

  defp all_ext("kpxx") do
    {"application", "vnd.ds-keypoint", []}
  end

  defp all_ext("ksp") do
    {"application", "vnd.kde.kspread", []}
  end

  defp all_ext("ktr") do
    {"application", "vnd.kahootz", []}
  end

  defp all_ext("ktx") do
    {"image", "ktx", []}
  end

  defp all_ext("ktz") do
    {"application", "vnd.kahootz", []}
  end

  defp all_ext("kwd") do
    {"application", "vnd.kde.kword", []}
  end

  defp all_ext("kwt") do
    {"application", "vnd.kde.kword", []}
  end

  defp all_ext("lasxml") do
    {"application", "vnd.las.las+xml", []}
  end

  defp all_ext("latex") do
    {"application", "x-latex", []}
  end

  defp all_ext("lbd") do
    {"application", "vnd.llamagraphics.life-balance.desktop", []}
  end

  defp all_ext("lbe") do
    {"application", "vnd.llamagraphics.life-balance.exchange+xml", []}
  end

  defp all_ext("les") do
    {"application", "vnd.hhe.lesson-player", []}
  end

  defp all_ext("lha") do
    {"application", "x-lzh-compressed", []}
  end

  defp all_ext("link66") do
    {"application", "vnd.route66.link66+xml", []}
  end

  defp all_ext("list3820") do
    {"application", "vnd.ibm.modcap", []}
  end

  defp all_ext("listafp") do
    {"application", "vnd.ibm.modcap", []}
  end

  defp all_ext("list") do
    {"text", "plain", []}
  end

  defp all_ext("lnk") do
    {"application", "x-ms-shortcut", []}
  end

  defp all_ext("log") do
    {"text", "plain", []}
  end

  defp all_ext("lostxml") do
    {"application", "lost+xml", []}
  end

  defp all_ext("lrf") do
    {"application", "octet-stream", []}
  end

  defp all_ext("lrm") do
    {"application", "vnd.ms-lrm", []}
  end

  defp all_ext("ltf") do
    {"application", "vnd.frogans.ltf", []}
  end

  defp all_ext("lvp") do
    {"audio", "vnd.lucent.voice", []}
  end

  defp all_ext("lwp") do
    {"application", "vnd.lotus-wordpro", []}
  end

  defp all_ext("lzh") do
    {"application", "x-lzh-compressed", []}
  end

  defp all_ext("m13") do
    {"application", "x-msmediaview", []}
  end

  defp all_ext("m14") do
    {"application", "x-msmediaview", []}
  end

  defp all_ext("m1v") do
    {"video", "mpeg", []}
  end

  defp all_ext("m21") do
    {"application", "mp21", []}
  end

  defp all_ext("m2a") do
    {"audio", "mpeg", []}
  end

  defp all_ext("m2v") do
    {"video", "mpeg", []}
  end

  defp all_ext("m3a") do
    {"audio", "mpeg", []}
  end

  defp all_ext("m3u8") do
    {"application", "vnd.apple.mpegurl", []}
  end

  defp all_ext("m3u") do
    {"audio", "x-mpegurl", []}
  end

  defp all_ext("m4a") do
    {"audio", "mp4", []}
  end

  defp all_ext("m4u") do
    {"video", "vnd.mpegurl", []}
  end

  defp all_ext("m4v") do
    {"video", "x-m4v", []}
  end

  defp all_ext("ma") do
    {"application", "mathematica", []}
  end

  defp all_ext("mads") do
    {"application", "mads+xml", []}
  end

  defp all_ext("mag") do
    {"application", "vnd.ecowin.chart", []}
  end

  defp all_ext("maker") do
    {"application", "vnd.framemaker", []}
  end

  defp all_ext("man") do
    {"text", "troff", []}
  end

  defp all_ext("mar") do
    {"application", "octet-stream", []}
  end

  defp all_ext("mathml") do
    {"application", "mathml+xml", []}
  end

  defp all_ext("mb") do
    {"application", "mathematica", []}
  end

  defp all_ext("mbk") do
    {"application", "vnd.mobius.mbk", []}
  end

  defp all_ext("mbox") do
    {"application", "mbox", []}
  end

  defp all_ext("mc1") do
    {"application", "vnd.medcalcdata", []}
  end

  defp all_ext("mcd") do
    {"application", "vnd.mcd", []}
  end

  defp all_ext("mcurl") do
    {"text", "vnd.curl.mcurl", []}
  end

  defp all_ext("mdb") do
    {"application", "x-msaccess", []}
  end

  defp all_ext("mdi") do
    {"image", "vnd.ms-modi", []}
  end

  defp all_ext("mesh") do
    {"model", "mesh", []}
  end

  defp all_ext("meta4") do
    {"application", "metalink4+xml", []}
  end

  defp all_ext("metalink") do
    {"application", "metalink+xml", []}
  end

  defp all_ext("me") do
    {"text", "troff", []}
  end

  defp all_ext("mets") do
    {"application", "mets+xml", []}
  end

  defp all_ext("mfm") do
    {"application", "vnd.mfmp", []}
  end

  defp all_ext("mft") do
    {"application", "rpki-manifest", []}
  end

  defp all_ext("mgp") do
    {"application", "vnd.osgeo.mapguide.package", []}
  end

  defp all_ext("mgz") do
    {"application", "vnd.proteus.magazine", []}
  end

  defp all_ext("mid") do
    {"audio", "midi", []}
  end

  defp all_ext("midi") do
    {"audio", "midi", []}
  end

  defp all_ext("mie") do
    {"application", "x-mie", []}
  end

  defp all_ext("mif") do
    {"application", "vnd.mif", []}
  end

  defp all_ext("mime") do
    {"message", "rfc822", []}
  end

  defp all_ext("mj2") do
    {"video", "mj2", []}
  end

  defp all_ext("mjp2") do
    {"video", "mj2", []}
  end

  defp all_ext("mk3d") do
    {"video", "x-matroska", []}
  end

  defp all_ext("mka") do
    {"audio", "x-matroska", []}
  end

  defp all_ext("mks") do
    {"video", "x-matroska", []}
  end

  defp all_ext("mkv") do
    {"video", "x-matroska", []}
  end

  defp all_ext("mlp") do
    {"application", "vnd.dolby.mlp", []}
  end

  defp all_ext("mmd") do
    {"application", "vnd.chipnuts.karaoke-mmd", []}
  end

  defp all_ext("mmf") do
    {"application", "vnd.smaf", []}
  end

  defp all_ext("mmr") do
    {"image", "vnd.fujixerox.edmics-mmr", []}
  end

  defp all_ext("mng") do
    {"video", "x-mng", []}
  end

  defp all_ext("mny") do
    {"application", "x-msmoney", []}
  end

  defp all_ext("mobi") do
    {"application", "x-mobipocket-ebook", []}
  end

  defp all_ext("mods") do
    {"application", "mods+xml", []}
  end

  defp all_ext("movie") do
    {"video", "x-sgi-movie", []}
  end

  defp all_ext("mov") do
    {"video", "quicktime", []}
  end

  defp all_ext("mp21") do
    {"application", "mp21", []}
  end

  defp all_ext("mp2a") do
    {"audio", "mpeg", []}
  end

  defp all_ext("mp2") do
    {"audio", "mpeg", []}
  end

  defp all_ext("mp3") do
    {"audio", "mpeg", []}
  end

  defp all_ext("mp4a") do
    {"audio", "mp4", []}
  end

  defp all_ext("mp4s") do
    {"application", "mp4", []}
  end

  defp all_ext("mp4") do
    {"video", "mp4", []}
  end

  defp all_ext("mp4v") do
    {"video", "mp4", []}
  end

  defp all_ext("mpc") do
    {"application", "vnd.mophun.certificate", []}
  end

  defp all_ext("mpeg") do
    {"video", "mpeg", []}
  end

  defp all_ext("mpe") do
    {"video", "mpeg", []}
  end

  defp all_ext("mpg4") do
    {"video", "mp4", []}
  end

  defp all_ext("mpga") do
    {"audio", "mpeg", []}
  end

  defp all_ext("mpg") do
    {"video", "mpeg", []}
  end

  defp all_ext("mpkg") do
    {"application", "vnd.apple.installer+xml", []}
  end

  defp all_ext("mpm") do
    {"application", "vnd.blueice.multipass", []}
  end

  defp all_ext("mpn") do
    {"application", "vnd.mophun.application", []}
  end

  defp all_ext("mpp") do
    {"application", "vnd.ms-project", []}
  end

  defp all_ext("mpt") do
    {"application", "vnd.ms-project", []}
  end

  defp all_ext("mpy") do
    {"application", "vnd.ibm.minipay", []}
  end

  defp all_ext("mqy") do
    {"application", "vnd.mobius.mqy", []}
  end

  defp all_ext("mrc") do
    {"application", "marc", []}
  end

  defp all_ext("mrcx") do
    {"application", "marcxml+xml", []}
  end

  defp all_ext("mscml") do
    {"application", "mediaservercontrol+xml", []}
  end

  defp all_ext("mseed") do
    {"application", "vnd.fdsn.mseed", []}
  end

  defp all_ext("mseq") do
    {"application", "vnd.mseq", []}
  end

  defp all_ext("msf") do
    {"application", "vnd.epson.msf", []}
  end

  defp all_ext("msh") do
    {"model", "mesh", []}
  end

  defp all_ext("msi") do
    {"application", "x-msdownload", []}
  end

  defp all_ext("msl") do
    {"application", "vnd.mobius.msl", []}
  end

  defp all_ext("ms") do
    {"text", "troff", []}
  end

  defp all_ext("msty") do
    {"application", "vnd.muvee.style", []}
  end

  defp all_ext("mts") do
    {"model", "vnd.mts", []}
  end

  defp all_ext("mus") do
    {"application", "vnd.musician", []}
  end

  defp all_ext("musicxml") do
    {"application", "vnd.recordare.musicxml+xml", []}
  end

  defp all_ext("mvb") do
    {"application", "x-msmediaview", []}
  end

  defp all_ext("mwf") do
    {"application", "vnd.mfer", []}
  end

  defp all_ext("mxf") do
    {"application", "mxf", []}
  end

  defp all_ext("mxl") do
    {"application", "vnd.recordare.musicxml", []}
  end

  defp all_ext("mxml") do
    {"application", "xv+xml", []}
  end

  defp all_ext("mxs") do
    {"application", "vnd.triscape.mxs", []}
  end

  defp all_ext("mxu") do
    {"video", "vnd.mpegurl", []}
  end

  defp all_ext("n3") do
    {"text", "n3", []}
  end

  defp all_ext("nb") do
    {"application", "mathematica", []}
  end

  defp all_ext("nbp") do
    {"application", "vnd.wolfram.player", []}
  end

  defp all_ext("nc") do
    {"application", "x-netcdf", []}
  end

  defp all_ext("ncx") do
    {"application", "x-dtbncx+xml", []}
  end

  defp all_ext("nfo") do
    {"text", "x-nfo", []}
  end

  defp all_ext("n-gage") do
    {"application", "vnd.nokia.n-gage.symbian.install", []}
  end

  defp all_ext("ngdat") do
    {"application", "vnd.nokia.n-gage.data", []}
  end

  defp all_ext("nitf") do
    {"application", "vnd.nitf", []}
  end

  defp all_ext("nlu") do
    {"application", "vnd.neurolanguage.nlu", []}
  end

  defp all_ext("nml") do
    {"application", "vnd.enliven", []}
  end

  defp all_ext("nnd") do
    {"application", "vnd.noblenet-directory", []}
  end

  defp all_ext("nns") do
    {"application", "vnd.noblenet-sealer", []}
  end

  defp all_ext("nnw") do
    {"application", "vnd.noblenet-web", []}
  end

  defp all_ext("npx") do
    {"image", "vnd.net-fpx", []}
  end

  defp all_ext("nsc") do
    {"application", "x-conference", []}
  end

  defp all_ext("nsf") do
    {"application", "vnd.lotus-notes", []}
  end

  defp all_ext("ntf") do
    {"application", "vnd.nitf", []}
  end

  defp all_ext("nzb") do
    {"application", "x-nzb", []}
  end

  defp all_ext("oa2") do
    {"application", "vnd.fujitsu.oasys2", []}
  end

  defp all_ext("oa3") do
    {"application", "vnd.fujitsu.oasys3", []}
  end

  defp all_ext("oas") do
    {"application", "vnd.fujitsu.oasys", []}
  end

  defp all_ext("obd") do
    {"application", "x-msbinder", []}
  end

  defp all_ext("obj") do
    {"application", "x-tgif", []}
  end

  defp all_ext("oda") do
    {"application", "oda", []}
  end

  defp all_ext("odb") do
    {"application", "vnd.oasis.opendocument.database", []}
  end

  defp all_ext("odc") do
    {"application", "vnd.oasis.opendocument.chart", []}
  end

  defp all_ext("odf") do
    {"application", "vnd.oasis.opendocument.formula", []}
  end

  defp all_ext("odft") do
    {"application", "vnd.oasis.opendocument.formula-template", []}
  end

  defp all_ext("odg") do
    {"application", "vnd.oasis.opendocument.graphics", []}
  end

  defp all_ext("odi") do
    {"application", "vnd.oasis.opendocument.image", []}
  end

  defp all_ext("odm") do
    {"application", "vnd.oasis.opendocument.text-master", []}
  end

  defp all_ext("odp") do
    {"application", "vnd.oasis.opendocument.presentation", []}
  end

  defp all_ext("ods") do
    {"application", "vnd.oasis.opendocument.spreadsheet", []}
  end

  defp all_ext("odt") do
    {"application", "vnd.oasis.opendocument.text", []}
  end

  defp all_ext("oga") do
    {"audio", "ogg", []}
  end

  defp all_ext("ogg") do
    {"audio", "ogg", []}
  end

  defp all_ext("ogv") do
    {"video", "ogg", []}
  end

  defp all_ext("ogx") do
    {"application", "ogg", []}
  end

  defp all_ext("omdoc") do
    {"application", "omdoc+xml", []}
  end

  defp all_ext("onepkg") do
    {"application", "onenote", []}
  end

  defp all_ext("onetmp") do
    {"application", "onenote", []}
  end

  defp all_ext("onetoc2") do
    {"application", "onenote", []}
  end

  defp all_ext("onetoc") do
    {"application", "onenote", []}
  end

  defp all_ext("opf") do
    {"application", "oebps-package+xml", []}
  end

  defp all_ext("opml") do
    {"text", "x-opml", []}
  end

  defp all_ext("oprc") do
    {"application", "vnd.palm", []}
  end

  defp all_ext("org") do
    {"application", "vnd.lotus-organizer", []}
  end

  defp all_ext("osf") do
    {"application", "vnd.yamaha.openscoreformat", []}
  end

  defp all_ext("osfpvg") do
    {"application", "vnd.yamaha.openscoreformat.osfpvg+xml", []}
  end

  defp all_ext("otc") do
    {"application", "vnd.oasis.opendocument.chart-template", []}
  end

  defp all_ext("otf") do
    {"font", "otf", []}
  end

  defp all_ext("otg") do
    {"application", "vnd.oasis.opendocument.graphics-template", []}
  end

  defp all_ext("oth") do
    {"application", "vnd.oasis.opendocument.text-web", []}
  end

  defp all_ext("oti") do
    {"application", "vnd.oasis.opendocument.image-template", []}
  end

  defp all_ext("otp") do
    {"application", "vnd.oasis.opendocument.presentation-template", []}
  end

  defp all_ext("ots") do
    {"application", "vnd.oasis.opendocument.spreadsheet-template", []}
  end

  defp all_ext("ott") do
    {"application", "vnd.oasis.opendocument.text-template", []}
  end

  defp all_ext("oxps") do
    {"application", "oxps", []}
  end

  defp all_ext("oxt") do
    {"application", "vnd.openofficeorg.extension", []}
  end

  defp all_ext("p10") do
    {"application", "pkcs10", []}
  end

  defp all_ext("p12") do
    {"application", "x-pkcs12", []}
  end

  defp all_ext("p7b") do
    {"application", "x-pkcs7-certificates", []}
  end

  defp all_ext("p7c") do
    {"application", "pkcs7-mime", []}
  end

  defp all_ext("p7m") do
    {"application", "pkcs7-mime", []}
  end

  defp all_ext("p7r") do
    {"application", "x-pkcs7-certreqresp", []}
  end

  defp all_ext("p7s") do
    {"application", "pkcs7-signature", []}
  end

  defp all_ext("p8") do
    {"application", "pkcs8", []}
  end

  defp all_ext("pas") do
    {"text", "x-pascal", []}
  end

  defp all_ext("paw") do
    {"application", "vnd.pawaafile", []}
  end

  defp all_ext("pbd") do
    {"application", "vnd.powerbuilder6", []}
  end

  defp all_ext("pbm") do
    {"image", "x-portable-bitmap", []}
  end

  defp all_ext("pcap") do
    {"application", "vnd.tcpdump.pcap", []}
  end

  defp all_ext("pcf") do
    {"application", "x-font-pcf", []}
  end

  defp all_ext("pcl") do
    {"application", "vnd.hp-pcl", []}
  end

  defp all_ext("pclxl") do
    {"application", "vnd.hp-pclxl", []}
  end

  defp all_ext("pct") do
    {"image", "x-pict", []}
  end

  defp all_ext("pcurl") do
    {"application", "vnd.curl.pcurl", []}
  end

  defp all_ext("pcx") do
    {"image", "x-pcx", []}
  end

  defp all_ext("pdb") do
    {"application", "vnd.palm", []}
  end

  defp all_ext("pdf") do
    {"application", "pdf", []}
  end

  defp all_ext("pfa") do
    {"application", "x-font-type1", []}
  end

  defp all_ext("pfb") do
    {"application", "x-font-type1", []}
  end

  defp all_ext("pfm") do
    {"application", "x-font-type1", []}
  end

  defp all_ext("pfr") do
    {"application", "font-tdpfr", []}
  end

  defp all_ext("pfx") do
    {"application", "x-pkcs12", []}
  end

  defp all_ext("pgm") do
    {"image", "x-portable-graymap", []}
  end

  defp all_ext("pgn") do
    {"application", "x-chess-pgn", []}
  end

  defp all_ext("pgp") do
    {"application", "pgp-encrypted", []}
  end

  defp all_ext("pic") do
    {"image", "x-pict", []}
  end

  defp all_ext("pkg") do
    {"application", "octet-stream", []}
  end

  defp all_ext("pki") do
    {"application", "pkixcmp", []}
  end

  defp all_ext("pkipath") do
    {"application", "pkix-pkipath", []}
  end

  defp all_ext("plb") do
    {"application", "vnd.3gpp.pic-bw-large", []}
  end

  defp all_ext("plc") do
    {"application", "vnd.mobius.plc", []}
  end

  defp all_ext("plf") do
    {"application", "vnd.pocketlearn", []}
  end

  defp all_ext("pls") do
    {"application", "pls+xml", []}
  end

  defp all_ext("pml") do
    {"application", "vnd.ctc-posml", []}
  end

  defp all_ext("png") do
    {"image", "png", []}
  end

  defp all_ext("pnm") do
    {"image", "x-portable-anymap", []}
  end

  defp all_ext("portpkg") do
    {"application", "vnd.macports.portpkg", []}
  end

  defp all_ext("pot") do
    {"application", "vnd.ms-powerpoint", []}
  end

  defp all_ext("potm") do
    {"application", "vnd.ms-powerpoint.template.macroenabled.12", []}
  end

  defp all_ext("potx") do
    {"application", "vnd.openxmlformats-officedocument.presentationml.template", []}
  end

  defp all_ext("ppam") do
    {"application", "vnd.ms-powerpoint.addin.macroenabled.12", []}
  end

  defp all_ext("ppd") do
    {"application", "vnd.cups-ppd", []}
  end

  defp all_ext("ppm") do
    {"image", "x-portable-pixmap", []}
  end

  defp all_ext("pps") do
    {"application", "vnd.ms-powerpoint", []}
  end

  defp all_ext("ppsm") do
    {"application", "vnd.ms-powerpoint.slideshow.macroenabled.12", []}
  end

  defp all_ext("ppsx") do
    {"application", "vnd.openxmlformats-officedocument.presentationml.slideshow", []}
  end

  defp all_ext("ppt") do
    {"application", "vnd.ms-powerpoint", []}
  end

  defp all_ext("pptm") do
    {"application", "vnd.ms-powerpoint.presentation.macroenabled.12", []}
  end

  defp all_ext("pptx") do
    {"application", "vnd.openxmlformats-officedocument.presentationml.presentation", []}
  end

  defp all_ext("pqa") do
    {"application", "vnd.palm", []}
  end

  defp all_ext("prc") do
    {"application", "x-mobipocket-ebook", []}
  end

  defp all_ext("pre") do
    {"application", "vnd.lotus-freelance", []}
  end

  defp all_ext("prf") do
    {"application", "pics-rules", []}
  end

  defp all_ext("ps") do
    {"application", "postscript", []}
  end

  defp all_ext("psb") do
    {"application", "vnd.3gpp.pic-bw-small", []}
  end

  defp all_ext("psd") do
    {"image", "vnd.adobe.photoshop", []}
  end

  defp all_ext("psf") do
    {"application", "x-font-linux-psf", []}
  end

  defp all_ext("pskcxml") do
    {"application", "pskc+xml", []}
  end

  defp all_ext("p") do
    {"text", "x-pascal", []}
  end

  defp all_ext("ptid") do
    {"application", "vnd.pvi.ptid1", []}
  end

  defp all_ext("pub") do
    {"application", "x-mspublisher", []}
  end

  defp all_ext("pvb") do
    {"application", "vnd.3gpp.pic-bw-var", []}
  end

  defp all_ext("pwn") do
    {"application", "vnd.3m.post-it-notes", []}
  end

  defp all_ext("pya") do
    {"audio", "vnd.ms-playready.media.pya", []}
  end

  defp all_ext("pyv") do
    {"video", "vnd.ms-playready.media.pyv", []}
  end

  defp all_ext("qam") do
    {"application", "vnd.epson.quickanime", []}
  end

  defp all_ext("qbo") do
    {"application", "vnd.intu.qbo", []}
  end

  defp all_ext("qfx") do
    {"application", "vnd.intu.qfx", []}
  end

  defp all_ext("qps") do
    {"application", "vnd.publishare-delta-tree", []}
  end

  defp all_ext("qt") do
    {"video", "quicktime", []}
  end

  defp all_ext("qwd") do
    {"application", "vnd.quark.quarkxpress", []}
  end

  defp all_ext("qwt") do
    {"application", "vnd.quark.quarkxpress", []}
  end

  defp all_ext("qxb") do
    {"application", "vnd.quark.quarkxpress", []}
  end

  defp all_ext("qxd") do
    {"application", "vnd.quark.quarkxpress", []}
  end

  defp all_ext("qxl") do
    {"application", "vnd.quark.quarkxpress", []}
  end

  defp all_ext("qxt") do
    {"application", "vnd.quark.quarkxpress", []}
  end

  defp all_ext("ra") do
    {"audio", "x-pn-realaudio", []}
  end

  defp all_ext("ram") do
    {"audio", "x-pn-realaudio", []}
  end

  defp all_ext("rar") do
    {"application", "x-rar-compressed", []}
  end

  defp all_ext("ras") do
    {"image", "x-cmu-raster", []}
  end

  defp all_ext("rcprofile") do
    {"application", "vnd.ipunplugged.rcprofile", []}
  end

  defp all_ext("rdf") do
    {"application", "rdf+xml", []}
  end

  defp all_ext("rdz") do
    {"application", "vnd.data-vision.rdz", []}
  end

  defp all_ext("rep") do
    {"application", "vnd.businessobjects", []}
  end

  defp all_ext("res") do
    {"application", "x-dtbresource+xml", []}
  end

  defp all_ext("rgb") do
    {"image", "x-rgb", []}
  end

  defp all_ext("rif") do
    {"application", "reginfo+xml", []}
  end

  defp all_ext("rip") do
    {"audio", "vnd.rip", []}
  end

  defp all_ext("ris") do
    {"application", "x-research-info-systems", []}
  end

  defp all_ext("rl") do
    {"application", "resource-lists+xml", []}
  end

  defp all_ext("rlc") do
    {"image", "vnd.fujixerox.edmics-rlc", []}
  end

  defp all_ext("rld") do
    {"application", "resource-lists-diff+xml", []}
  end

  defp all_ext("rm") do
    {"application", "vnd.rn-realmedia", []}
  end

  defp all_ext("rmi") do
    {"audio", "midi", []}
  end

  defp all_ext("rmp") do
    {"audio", "x-pn-realaudio-plugin", []}
  end

  defp all_ext("rms") do
    {"application", "vnd.jcp.javame.midlet-rms", []}
  end

  defp all_ext("rmvb") do
    {"application", "vnd.rn-realmedia-vbr", []}
  end

  defp all_ext("rnc") do
    {"application", "relax-ng-compact-syntax", []}
  end

  defp all_ext("roa") do
    {"application", "rpki-roa", []}
  end

  defp all_ext("roff") do
    {"text", "troff", []}
  end

  defp all_ext("rp9") do
    {"application", "vnd.cloanto.rp9", []}
  end

  defp all_ext("rpss") do
    {"application", "vnd.nokia.radio-presets", []}
  end

  defp all_ext("rpst") do
    {"application", "vnd.nokia.radio-preset", []}
  end

  defp all_ext("rq") do
    {"application", "sparql-query", []}
  end

  defp all_ext("rs") do
    {"application", "rls-services+xml", []}
  end

  defp all_ext("rsd") do
    {"application", "rsd+xml", []}
  end

  defp all_ext("rss") do
    {"application", "rss+xml", []}
  end

  defp all_ext("rtf") do
    {"application", "rtf", []}
  end

  defp all_ext("rtx") do
    {"text", "richtext", []}
  end

  defp all_ext("s3m") do
    {"audio", "s3m", []}
  end

  defp all_ext("saf") do
    {"application", "vnd.yamaha.smaf-audio", []}
  end

  defp all_ext("sbml") do
    {"application", "sbml+xml", []}
  end

  defp all_ext("sc") do
    {"application", "vnd.ibm.secure-container", []}
  end

  defp all_ext("scd") do
    {"application", "x-msschedule", []}
  end

  defp all_ext("scm") do
    {"application", "vnd.lotus-screencam", []}
  end

  defp all_ext("scq") do
    {"application", "scvp-cv-request", []}
  end

  defp all_ext("scs") do
    {"application", "scvp-cv-response", []}
  end

  defp all_ext("scurl") do
    {"text", "vnd.curl.scurl", []}
  end

  defp all_ext("sda") do
    {"application", "vnd.stardivision.draw", []}
  end

  defp all_ext("sdc") do
    {"application", "vnd.stardivision.calc", []}
  end

  defp all_ext("sdd") do
    {"application", "vnd.stardivision.impress", []}
  end

  defp all_ext("sdkd") do
    {"application", "vnd.solent.sdkm+xml", []}
  end

  defp all_ext("sdkm") do
    {"application", "vnd.solent.sdkm+xml", []}
  end

  defp all_ext("sdp") do
    {"application", "sdp", []}
  end

  defp all_ext("sdw") do
    {"application", "vnd.stardivision.writer", []}
  end

  defp all_ext("see") do
    {"application", "vnd.seemail", []}
  end

  defp all_ext("seed") do
    {"application", "vnd.fdsn.seed", []}
  end

  defp all_ext("sema") do
    {"application", "vnd.sema", []}
  end

  defp all_ext("semd") do
    {"application", "vnd.semd", []}
  end

  defp all_ext("semf") do
    {"application", "vnd.semf", []}
  end

  defp all_ext("ser") do
    {"application", "java-serialized-object", []}
  end

  defp all_ext("setpay") do
    {"application", "set-payment-initiation", []}
  end

  defp all_ext("setreg") do
    {"application", "set-registration-initiation", []}
  end

  defp all_ext("sfd-hdstx") do
    {"application", "vnd.hydrostatix.sof-data", []}
  end

  defp all_ext("sfs") do
    {"application", "vnd.spotfire.sfs", []}
  end

  defp all_ext("sfv") do
    {"text", "x-sfv", []}
  end

  defp all_ext("sgi") do
    {"image", "sgi", []}
  end

  defp all_ext("sgl") do
    {"application", "vnd.stardivision.writer-global", []}
  end

  defp all_ext("sgml") do
    {"text", "sgml", []}
  end

  defp all_ext("sgm") do
    {"text", "sgml", []}
  end

  defp all_ext("sh") do
    {"application", "x-sh", []}
  end

  defp all_ext("shar") do
    {"application", "x-shar", []}
  end

  defp all_ext("shf") do
    {"application", "shf+xml", []}
  end

  defp all_ext("sid") do
    {"image", "x-mrsid-image", []}
  end

  defp all_ext("sig") do
    {"application", "pgp-signature", []}
  end

  defp all_ext("sil") do
    {"audio", "silk", []}
  end

  defp all_ext("silo") do
    {"model", "mesh", []}
  end

  defp all_ext("sis") do
    {"application", "vnd.symbian.install", []}
  end

  defp all_ext("sisx") do
    {"application", "vnd.symbian.install", []}
  end

  defp all_ext("sit") do
    {"application", "x-stuffit", []}
  end

  defp all_ext("sitx") do
    {"application", "x-stuffitx", []}
  end

  defp all_ext("skd") do
    {"application", "vnd.koan", []}
  end

  defp all_ext("skm") do
    {"application", "vnd.koan", []}
  end

  defp all_ext("skp") do
    {"application", "vnd.koan", []}
  end

  defp all_ext("skt") do
    {"application", "vnd.koan", []}
  end

  defp all_ext("sldm") do
    {"application", "vnd.ms-powerpoint.slide.macroenabled.12", []}
  end

  defp all_ext("sldx") do
    {"application", "vnd.openxmlformats-officedocument.presentationml.slide", []}
  end

  defp all_ext("slt") do
    {"application", "vnd.epson.salt", []}
  end

  defp all_ext("sm") do
    {"application", "vnd.stepmania.stepchart", []}
  end

  defp all_ext("smf") do
    {"application", "vnd.stardivision.math", []}
  end

  defp all_ext("smi") do
    {"application", "smil+xml", []}
  end

  defp all_ext("smil") do
    {"application", "smil+xml", []}
  end

  defp all_ext("smv") do
    {"video", "x-smv", []}
  end

  defp all_ext("smzip") do
    {"application", "vnd.stepmania.package", []}
  end

  defp all_ext("snd") do
    {"audio", "basic", []}
  end

  defp all_ext("snf") do
    {"application", "x-font-snf", []}
  end

  defp all_ext("so") do
    {"application", "octet-stream", []}
  end

  defp all_ext("spc") do
    {"application", "x-pkcs7-certificates", []}
  end

  defp all_ext("spf") do
    {"application", "vnd.yamaha.smaf-phrase", []}
  end

  defp all_ext("spl") do
    {"application", "x-futuresplash", []}
  end

  defp all_ext("spot") do
    {"text", "vnd.in3d.spot", []}
  end

  defp all_ext("spp") do
    {"application", "scvp-vp-response", []}
  end

  defp all_ext("spq") do
    {"application", "scvp-vp-request", []}
  end

  defp all_ext("spx") do
    {"audio", "ogg", []}
  end

  defp all_ext("sql") do
    {"application", "x-sql", []}
  end

  defp all_ext("src") do
    {"application", "x-wais-source", []}
  end

  defp all_ext("srt") do
    {"application", "x-subrip", []}
  end

  defp all_ext("sru") do
    {"application", "sru+xml", []}
  end

  defp all_ext("srx") do
    {"application", "sparql-results+xml", []}
  end

  defp all_ext("ssdl") do
    {"application", "ssdl+xml", []}
  end

  defp all_ext("sse") do
    {"application", "vnd.kodak-descriptor", []}
  end

  defp all_ext("ssf") do
    {"application", "vnd.epson.ssf", []}
  end

  defp all_ext("ssml") do
    {"application", "ssml+xml", []}
  end

  defp all_ext("st") do
    {"application", "vnd.sailingtracker.track", []}
  end

  defp all_ext("stc") do
    {"application", "vnd.sun.xml.calc.template", []}
  end

  defp all_ext("std") do
    {"application", "vnd.sun.xml.draw.template", []}
  end

  defp all_ext("s") do
    {"text", "x-asm", []}
  end

  defp all_ext("stf") do
    {"application", "vnd.wt.stf", []}
  end

  defp all_ext("sti") do
    {"application", "vnd.sun.xml.impress.template", []}
  end

  defp all_ext("stk") do
    {"application", "hyperstudio", []}
  end

  defp all_ext("stl") do
    {"application", "vnd.ms-pki.stl", []}
  end

  defp all_ext("str") do
    {"application", "vnd.pg.format", []}
  end

  defp all_ext("stw") do
    {"application", "vnd.sun.xml.writer.template", []}
  end

  defp all_ext("sub") do
    {"image", "vnd.dvb.subtitle", []}
  end

  defp all_ext("sus") do
    {"application", "vnd.sus-calendar", []}
  end

  defp all_ext("susp") do
    {"application", "vnd.sus-calendar", []}
  end

  defp all_ext("sv4cpio") do
    {"application", "x-sv4cpio", []}
  end

  defp all_ext("sv4crc") do
    {"application", "x-sv4crc", []}
  end

  defp all_ext("svc") do
    {"application", "vnd.dvb.service", []}
  end

  defp all_ext("svd") do
    {"application", "vnd.svd", []}
  end

  defp all_ext("svg") do
    {"image", "svg+xml", []}
  end

  defp all_ext("svgz") do
    {"image", "svg+xml", []}
  end

  defp all_ext("swa") do
    {"application", "x-director", []}
  end

  defp all_ext("swf") do
    {"application", "x-shockwave-flash", []}
  end

  defp all_ext("swi") do
    {"application", "vnd.aristanetworks.swi", []}
  end

  defp all_ext("sxc") do
    {"application", "vnd.sun.xml.calc", []}
  end

  defp all_ext("sxd") do
    {"application", "vnd.sun.xml.draw", []}
  end

  defp all_ext("sxg") do
    {"application", "vnd.sun.xml.writer.global", []}
  end

  defp all_ext("sxi") do
    {"application", "vnd.sun.xml.impress", []}
  end

  defp all_ext("sxm") do
    {"application", "vnd.sun.xml.math", []}
  end

  defp all_ext("sxw") do
    {"application", "vnd.sun.xml.writer", []}
  end

  defp all_ext("t3") do
    {"application", "x-t3vm-image", []}
  end

  defp all_ext("taglet") do
    {"application", "vnd.mynfc", []}
  end

  defp all_ext("tao") do
    {"application", "vnd.tao.intent-module-archive", []}
  end

  defp all_ext("tar") do
    {"application", "x-tar", []}
  end

  defp all_ext("tcap") do
    {"application", "vnd.3gpp2.tcap", []}
  end

  defp all_ext("tcl") do
    {"application", "x-tcl", []}
  end

  defp all_ext("teacher") do
    {"application", "vnd.smart.teacher", []}
  end

  defp all_ext("tei") do
    {"application", "tei+xml", []}
  end

  defp all_ext("teicorpus") do
    {"application", "tei+xml", []}
  end

  defp all_ext("tex") do
    {"application", "x-tex", []}
  end

  defp all_ext("texi") do
    {"application", "x-texinfo", []}
  end

  defp all_ext("texinfo") do
    {"application", "x-texinfo", []}
  end

  defp all_ext("text") do
    {"text", "plain", []}
  end

  defp all_ext("tfi") do
    {"application", "thraud+xml", []}
  end

  defp all_ext("tfm") do
    {"application", "x-tex-tfm", []}
  end

  defp all_ext("tga") do
    {"image", "x-tga", []}
  end

  defp all_ext("thmx") do
    {"application", "vnd.ms-officetheme", []}
  end

  defp all_ext("tiff") do
    {"image", "tiff", []}
  end

  defp all_ext("tif") do
    {"image", "tiff", []}
  end

  defp all_ext("tmo") do
    {"application", "vnd.tmobile-livetv", []}
  end

  defp all_ext("torrent") do
    {"application", "x-bittorrent", []}
  end

  defp all_ext("tpl") do
    {"application", "vnd.groove-tool-template", []}
  end

  defp all_ext("tpt") do
    {"application", "vnd.trid.tpt", []}
  end

  defp all_ext("tra") do
    {"application", "vnd.trueapp", []}
  end

  defp all_ext("trm") do
    {"application", "x-msterminal", []}
  end

  defp all_ext("tr") do
    {"text", "troff", []}
  end

  defp all_ext("tsd") do
    {"application", "timestamped-data", []}
  end

  defp all_ext("tsv") do
    {"text", "tab-separated-values", []}
  end

  defp all_ext("ttc") do
    {"font", "collection", []}
  end

  defp all_ext("t") do
    {"text", "troff", []}
  end

  defp all_ext("ttf") do
    {"font", "ttf", []}
  end

  defp all_ext("ttl") do
    {"text", "turtle", []}
  end

  defp all_ext("twd") do
    {"application", "vnd.simtech-mindmapper", []}
  end

  defp all_ext("twds") do
    {"application", "vnd.simtech-mindmapper", []}
  end

  defp all_ext("txd") do
    {"application", "vnd.genomatix.tuxedo", []}
  end

  defp all_ext("txf") do
    {"application", "vnd.mobius.txf", []}
  end

  defp all_ext("txt") do
    {"text", "plain", []}
  end

  defp all_ext("u32") do
    {"application", "x-authorware-bin", []}
  end

  defp all_ext("udeb") do
    {"application", "x-debian-package", []}
  end

  defp all_ext("ufd") do
    {"application", "vnd.ufdl", []}
  end

  defp all_ext("ufdl") do
    {"application", "vnd.ufdl", []}
  end

  defp all_ext("ulx") do
    {"application", "x-glulx", []}
  end

  defp all_ext("umj") do
    {"application", "vnd.umajin", []}
  end

  defp all_ext("unityweb") do
    {"application", "vnd.unity", []}
  end

  defp all_ext("uoml") do
    {"application", "vnd.uoml+xml", []}
  end

  defp all_ext("uris") do
    {"text", "uri-list", []}
  end

  defp all_ext("uri") do
    {"text", "uri-list", []}
  end

  defp all_ext("urls") do
    {"text", "uri-list", []}
  end

  defp all_ext("ustar") do
    {"application", "x-ustar", []}
  end

  defp all_ext("utz") do
    {"application", "vnd.uiq.theme", []}
  end

  defp all_ext("uu") do
    {"text", "x-uuencode", []}
  end

  defp all_ext("uva") do
    {"audio", "vnd.dece.audio", []}
  end

  defp all_ext("uvd") do
    {"application", "vnd.dece.data", []}
  end

  defp all_ext("uvf") do
    {"application", "vnd.dece.data", []}
  end

  defp all_ext("uvg") do
    {"image", "vnd.dece.graphic", []}
  end

  defp all_ext("uvh") do
    {"video", "vnd.dece.hd", []}
  end

  defp all_ext("uvi") do
    {"image", "vnd.dece.graphic", []}
  end

  defp all_ext("uvm") do
    {"video", "vnd.dece.mobile", []}
  end

  defp all_ext("uvp") do
    {"video", "vnd.dece.pd", []}
  end

  defp all_ext("uvs") do
    {"video", "vnd.dece.sd", []}
  end

  defp all_ext("uvt") do
    {"application", "vnd.dece.ttml+xml", []}
  end

  defp all_ext("uvu") do
    {"video", "vnd.uvvu.mp4", []}
  end

  defp all_ext("uvva") do
    {"audio", "vnd.dece.audio", []}
  end

  defp all_ext("uvvd") do
    {"application", "vnd.dece.data", []}
  end

  defp all_ext("uvvf") do
    {"application", "vnd.dece.data", []}
  end

  defp all_ext("uvvg") do
    {"image", "vnd.dece.graphic", []}
  end

  defp all_ext("uvvh") do
    {"video", "vnd.dece.hd", []}
  end

  defp all_ext("uvvi") do
    {"image", "vnd.dece.graphic", []}
  end

  defp all_ext("uvvm") do
    {"video", "vnd.dece.mobile", []}
  end

  defp all_ext("uvvp") do
    {"video", "vnd.dece.pd", []}
  end

  defp all_ext("uvvs") do
    {"video", "vnd.dece.sd", []}
  end

  defp all_ext("uvvt") do
    {"application", "vnd.dece.ttml+xml", []}
  end

  defp all_ext("uvvu") do
    {"video", "vnd.uvvu.mp4", []}
  end

  defp all_ext("uvv") do
    {"video", "vnd.dece.video", []}
  end

  defp all_ext("uvvv") do
    {"video", "vnd.dece.video", []}
  end

  defp all_ext("uvvx") do
    {"application", "vnd.dece.unspecified", []}
  end

  defp all_ext("uvvz") do
    {"application", "vnd.dece.zip", []}
  end

  defp all_ext("uvx") do
    {"application", "vnd.dece.unspecified", []}
  end

  defp all_ext("uvz") do
    {"application", "vnd.dece.zip", []}
  end

  defp all_ext("vcard") do
    {"text", "vcard", []}
  end

  defp all_ext("vcd") do
    {"application", "x-cdlink", []}
  end

  defp all_ext("vcf") do
    {"text", "x-vcard", []}
  end

  defp all_ext("vcg") do
    {"application", "vnd.groove-vcard", []}
  end

  defp all_ext("vcs") do
    {"text", "x-vcalendar", []}
  end

  defp all_ext("vcx") do
    {"application", "vnd.vcx", []}
  end

  defp all_ext("vis") do
    {"application", "vnd.visionary", []}
  end

  defp all_ext("viv") do
    {"video", "vnd.vivo", []}
  end

  defp all_ext("vob") do
    {"video", "x-ms-vob", []}
  end

  defp all_ext("vor") do
    {"application", "vnd.stardivision.writer", []}
  end

  defp all_ext("vox") do
    {"application", "x-authorware-bin", []}
  end

  defp all_ext("vrml") do
    {"model", "vrml", []}
  end

  defp all_ext("vsd") do
    {"application", "vnd.visio", []}
  end

  defp all_ext("vsf") do
    {"application", "vnd.vsf", []}
  end

  defp all_ext("vss") do
    {"application", "vnd.visio", []}
  end

  defp all_ext("vst") do
    {"application", "vnd.visio", []}
  end

  defp all_ext("vsw") do
    {"application", "vnd.visio", []}
  end

  defp all_ext("vtu") do
    {"model", "vnd.vtu", []}
  end

  defp all_ext("vxml") do
    {"application", "voicexml+xml", []}
  end

  defp all_ext("w3d") do
    {"application", "x-director", []}
  end

  defp all_ext("wad") do
    {"application", "x-doom", []}
  end

  defp all_ext("wav") do
    {"audio", "x-wav", []}
  end

  defp all_ext("wax") do
    {"audio", "x-ms-wax", []}
  end

  defp all_ext("wbmp") do
    {"image", "vnd.wap.wbmp", []}
  end

  defp all_ext("wbs") do
    {"application", "vnd.criticaltools.wbs+xml", []}
  end

  defp all_ext("wbxml") do
    {"application", "vnd.wap.wbxml", []}
  end

  defp all_ext("wcm") do
    {"application", "vnd.ms-works", []}
  end

  defp all_ext("wdb") do
    {"application", "vnd.ms-works", []}
  end

  defp all_ext("wdp") do
    {"image", "vnd.ms-photo", []}
  end

  defp all_ext("weba") do
    {"audio", "webm", []}
  end

  defp all_ext("webm") do
    {"video", "webm", []}
  end

  defp all_ext("webp") do
    {"image", "webp", []}
  end

  defp all_ext("wg") do
    {"application", "vnd.pmi.widget", []}
  end

  defp all_ext("wgt") do
    {"application", "widget", []}
  end

  defp all_ext("wks") do
    {"application", "vnd.ms-works", []}
  end

  defp all_ext("wma") do
    {"audio", "x-ms-wma", []}
  end

  defp all_ext("wmd") do
    {"application", "x-ms-wmd", []}
  end

  defp all_ext("wmf") do
    {"application", "x-msmetafile", []}
  end

  defp all_ext("wmlc") do
    {"application", "vnd.wap.wmlc", []}
  end

  defp all_ext("wmlsc") do
    {"application", "vnd.wap.wmlscriptc", []}
  end

  defp all_ext("wmls") do
    {"text", "vnd.wap.wmlscript", []}
  end

  defp all_ext("wml") do
    {"text", "vnd.wap.wml", []}
  end

  defp all_ext("wm") do
    {"video", "x-ms-wm", []}
  end

  defp all_ext("wmv") do
    {"video", "x-ms-wmv", []}
  end

  defp all_ext("wmx") do
    {"video", "x-ms-wmx", []}
  end

  defp all_ext("wmz") do
    {"application", "x-msmetafile", []}
  end

  defp all_ext("woff2") do
    {"font", "woff2", []}
  end

  defp all_ext("woff") do
    {"font", "woff", []}
  end

  defp all_ext("wpd") do
    {"application", "vnd.wordperfect", []}
  end

  defp all_ext("wpl") do
    {"application", "vnd.ms-wpl", []}
  end

  defp all_ext("wps") do
    {"application", "vnd.ms-works", []}
  end

  defp all_ext("wqd") do
    {"application", "vnd.wqd", []}
  end

  defp all_ext("wri") do
    {"application", "x-mswrite", []}
  end

  defp all_ext("wrl") do
    {"model", "vrml", []}
  end

  defp all_ext("wsdl") do
    {"application", "wsdl+xml", []}
  end

  defp all_ext("wspolicy") do
    {"application", "wspolicy+xml", []}
  end

  defp all_ext("wtb") do
    {"application", "vnd.webturbo", []}
  end

  defp all_ext("wvx") do
    {"video", "x-ms-wvx", []}
  end

  defp all_ext("x32") do
    {"application", "x-authorware-bin", []}
  end

  defp all_ext("x3db") do
    {"model", "x3d+binary", []}
  end

  defp all_ext("x3dbz") do
    {"model", "x3d+binary", []}
  end

  defp all_ext("x3d") do
    {"model", "x3d+xml", []}
  end

  defp all_ext("x3dv") do
    {"model", "x3d+vrml", []}
  end

  defp all_ext("x3dvz") do
    {"model", "x3d+vrml", []}
  end

  defp all_ext("x3dz") do
    {"model", "x3d+xml", []}
  end

  defp all_ext("xaml") do
    {"application", "xaml+xml", []}
  end

  defp all_ext("xap") do
    {"application", "x-silverlight-app", []}
  end

  defp all_ext("xar") do
    {"application", "vnd.xara", []}
  end

  defp all_ext("xbap") do
    {"application", "x-ms-xbap", []}
  end

  defp all_ext("xbd") do
    {"application", "vnd.fujixerox.docuworks.binder", []}
  end

  defp all_ext("xbm") do
    {"image", "x-xbitmap", []}
  end

  defp all_ext("xdf") do
    {"application", "xcap-diff+xml", []}
  end

  defp all_ext("xdm") do
    {"application", "vnd.syncml.dm+xml", []}
  end

  defp all_ext("xdp") do
    {"application", "vnd.adobe.xdp+xml", []}
  end

  defp all_ext("xdssc") do
    {"application", "dssc+xml", []}
  end

  defp all_ext("xdw") do
    {"application", "vnd.fujixerox.docuworks", []}
  end

  defp all_ext("xenc") do
    {"application", "xenc+xml", []}
  end

  defp all_ext("xer") do
    {"application", "patch-ops-error+xml", []}
  end

  defp all_ext("xfdf") do
    {"application", "vnd.adobe.xfdf", []}
  end

  defp all_ext("xfdl") do
    {"application", "vnd.xfdl", []}
  end

  defp all_ext("xht") do
    {"application", "xhtml+xml", []}
  end

  defp all_ext("xhtml") do
    {"application", "xhtml+xml", []}
  end

  defp all_ext("xhvml") do
    {"application", "xv+xml", []}
  end

  defp all_ext("xif") do
    {"image", "vnd.xiff", []}
  end

  defp all_ext("xla") do
    {"application", "vnd.ms-excel", []}
  end

  defp all_ext("xlam") do
    {"application", "vnd.ms-excel.addin.macroenabled.12", []}
  end

  defp all_ext("xlc") do
    {"application", "vnd.ms-excel", []}
  end

  defp all_ext("xlf") do
    {"application", "x-xliff+xml", []}
  end

  defp all_ext("xlm") do
    {"application", "vnd.ms-excel", []}
  end

  defp all_ext("xls") do
    {"application", "vnd.ms-excel", []}
  end

  defp all_ext("xlsb") do
    {"application", "vnd.ms-excel.sheet.binary.macroenabled.12", []}
  end

  defp all_ext("xlsm") do
    {"application", "vnd.ms-excel.sheet.macroenabled.12", []}
  end

  defp all_ext("xlsx") do
    {"application", "vnd.openxmlformats-officedocument.spreadsheetml.sheet", []}
  end

  defp all_ext("xlt") do
    {"application", "vnd.ms-excel", []}
  end

  defp all_ext("xltm") do
    {"application", "vnd.ms-excel.template.macroenabled.12", []}
  end

  defp all_ext("xltx") do
    {"application", "vnd.openxmlformats-officedocument.spreadsheetml.template", []}
  end

  defp all_ext("xlw") do
    {"application", "vnd.ms-excel", []}
  end

  defp all_ext("xm") do
    {"audio", "xm", []}
  end

  defp all_ext("xml") do
    {"application", "xml", []}
  end

  defp all_ext("xo") do
    {"application", "vnd.olpc-sugar", []}
  end

  defp all_ext("xop") do
    {"application", "xop+xml", []}
  end

  defp all_ext("xpi") do
    {"application", "x-xpinstall", []}
  end

  defp all_ext("xpl") do
    {"application", "xproc+xml", []}
  end

  defp all_ext("xpm") do
    {"image", "x-xpixmap", []}
  end

  defp all_ext("xpr") do
    {"application", "vnd.is-xpr", []}
  end

  defp all_ext("xps") do
    {"application", "vnd.ms-xpsdocument", []}
  end

  defp all_ext("xpw") do
    {"application", "vnd.intercon.formnet", []}
  end

  defp all_ext("xpx") do
    {"application", "vnd.intercon.formnet", []}
  end

  defp all_ext("xsl") do
    {"application", "xml", []}
  end

  defp all_ext("xslt") do
    {"application", "xslt+xml", []}
  end

  defp all_ext("xsm") do
    {"application", "vnd.syncml+xml", []}
  end

  defp all_ext("xspf") do
    {"application", "xspf+xml", []}
  end

  defp all_ext("xul") do
    {"application", "vnd.mozilla.xul+xml", []}
  end

  defp all_ext("xvm") do
    {"application", "xv+xml", []}
  end

  defp all_ext("xvml") do
    {"application", "xv+xml", []}
  end

  defp all_ext("xwd") do
    {"image", "x-xwindowdump", []}
  end

  defp all_ext("xyz") do
    {"chemical", "x-xyz", []}
  end

  defp all_ext("xz") do
    {"application", "x-xz", []}
  end

  defp all_ext("yang") do
    {"application", "yang", []}
  end

  defp all_ext("yin") do
    {"application", "yin+xml", []}
  end

  defp all_ext("z1") do
    {"application", "x-zmachine", []}
  end

  defp all_ext("z2") do
    {"application", "x-zmachine", []}
  end

  defp all_ext("z3") do
    {"application", "x-zmachine", []}
  end

  defp all_ext("z4") do
    {"application", "x-zmachine", []}
  end

  defp all_ext("z5") do
    {"application", "x-zmachine", []}
  end

  defp all_ext("z6") do
    {"application", "x-zmachine", []}
  end

  defp all_ext("z7") do
    {"application", "x-zmachine", []}
  end

  defp all_ext("z8") do
    {"application", "x-zmachine", []}
  end

  defp all_ext("zaz") do
    {"application", "vnd.zzazz.deck+xml", []}
  end

  defp all_ext("zip") do
    {"application", "zip", []}
  end

  defp all_ext("zir") do
    {"application", "vnd.zul", []}
  end

  defp all_ext("zirz") do
    {"application", "vnd.zul", []}
  end

  defp all_ext("zmm") do
    {"application", "vnd.handheld-entertainment+xml", []}
  end

  defp all_ext(_) do
    {"application", "octet-stream", []}
  end

  defp web_ext("css") do
    {"text", "css", []}
  end

  defp web_ext("gif") do
    {"image", "gif", []}
  end

  defp web_ext("html") do
    {"text", "html", []}
  end

  defp web_ext("htm") do
    {"text", "html", []}
  end

  defp web_ext("ico") do
    {"image", "x-icon", []}
  end

  defp web_ext("jpeg") do
    {"image", "jpeg", []}
  end

  defp web_ext("jpg") do
    {"image", "jpeg", []}
  end

  defp web_ext("js") do
    {"application", "javascript", []}
  end

  defp web_ext("mp3") do
    {"audio", "mpeg", []}
  end

  defp web_ext("mp4") do
    {"video", "mp4", []}
  end

  defp web_ext("ogg") do
    {"audio", "ogg", []}
  end

  defp web_ext("ogv") do
    {"video", "ogg", []}
  end

  defp web_ext("png") do
    {"image", "png", []}
  end

  defp web_ext("svg") do
    {"image", "svg+xml", []}
  end

  defp web_ext("wav") do
    {"audio", "x-wav", []}
  end

  defp web_ext("webm") do
    {"video", "webm", []}
  end

  defp web_ext(_) do
    {"application", "octet-stream", []}
  end
end
