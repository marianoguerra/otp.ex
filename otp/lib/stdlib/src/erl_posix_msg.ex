defmodule :m_erl_posix_msg do
  use Bitwise

  def message(t) do
    :erlang.binary_to_list(message_1(t))
  end

  defp message_1(:e2big) do
    "argument list too long"
  end

  defp message_1(:eacces) do
    "permission denied"
  end

  defp message_1(:eaddrinuse) do
    "address already in use"
  end

  defp message_1(:eaddrnotavail) do
    "can't assign requested address"
  end

  defp message_1(:eadv) do
    "advertise error"
  end

  defp message_1(:eafnosupport) do
    "address family not supported by protocol family"
  end

  defp message_1(:eagain) do
    "resource temporarily unavailable"
  end

  defp message_1(:ealign) do
    "EALIGN"
  end

  defp message_1(:ealready) do
    "operation already in progress"
  end

  defp message_1(:ebade) do
    "bad exchange descriptor"
  end

  defp message_1(:ebadf) do
    "bad file number"
  end

  defp message_1(:ebadfd) do
    "file descriptor in bad state"
  end

  defp message_1(:ebadmsg) do
    "not a data message"
  end

  defp message_1(:ebadr) do
    "bad request descriptor"
  end

  defp message_1(:ebadrpc) do
    "RPC structure is bad"
  end

  defp message_1(:ebadrqc) do
    "bad request code"
  end

  defp message_1(:ebadslt) do
    "invalid slot"
  end

  defp message_1(:ebfont) do
    "bad font file format"
  end

  defp message_1(:ebusy) do
    "file busy"
  end

  defp message_1(:echild) do
    "no children"
  end

  defp message_1(:echrng) do
    "channel number out of range"
  end

  defp message_1(:ecomm) do
    "communication error on send"
  end

  defp message_1(:econnaborted) do
    "software caused connection abort"
  end

  defp message_1(:econnrefused) do
    "connection refused"
  end

  defp message_1(:econnreset) do
    "connection reset by peer"
  end

  defp message_1(:edeadlk) do
    "resource deadlock avoided"
  end

  defp message_1(:edeadlock) do
    "resource deadlock avoided"
  end

  defp message_1(:edestaddrreq) do
    "destination address required"
  end

  defp message_1(:edirty) do
    "mounting a dirty fs w/o force"
  end

  defp message_1(:edom) do
    "math argument out of range"
  end

  defp message_1(:edotdot) do
    "cross mount point"
  end

  defp message_1(:edquot) do
    "disk quota exceeded"
  end

  defp message_1(:eduppkg) do
    "duplicate package name"
  end

  defp message_1(:eexist) do
    "file already exists"
  end

  defp message_1(:efault) do
    "bad address in system call argument"
  end

  defp message_1(:efbig) do
    "file too large"
  end

  defp message_1(:eftype) do
    "EFTYPE"
  end

  defp message_1(:ehostdown) do
    "host is down"
  end

  defp message_1(:ehostunreach) do
    "host is unreachable"
  end

  defp message_1(:eidrm) do
    "identifier removed"
  end

  defp message_1(:einit) do
    "initialization error"
  end

  defp message_1(:einprogress) do
    "operation now in progress"
  end

  defp message_1(:eintr) do
    "interrupted system call"
  end

  defp message_1(:einval) do
    "invalid argument"
  end

  defp message_1(:eio) do
    "I/O error"
  end

  defp message_1(:eisconn) do
    "socket is already connected"
  end

  defp message_1(:eisdir) do
    "illegal operation on a directory"
  end

  defp message_1(:eisnam) do
    "is a name file"
  end

  defp message_1(:elbin) do
    "ELBIN"
  end

  defp message_1(:el2hlt) do
    "level 2 halted"
  end

  defp message_1(:el2nsync) do
    "level 2 not synchronized"
  end

  defp message_1(:el3hlt) do
    "level 3 halted"
  end

  defp message_1(:el3rst) do
    "level 3 reset"
  end

  defp message_1(:elibacc) do
    "cannot access a needed shared library"
  end

  defp message_1(:elibbad) do
    "accessing a corrupted shared library"
  end

  defp message_1(:elibexec) do
    "cannot exec a shared library directly"
  end

  defp message_1(:elibmax) do
    "attempting to link in more shared libraries than system limit"
  end

  defp message_1(:elibscn) do
    ".lib section in a.out corrupted"
  end

  defp message_1(:elnrng) do
    "link number out of range"
  end

  defp message_1(:eloop) do
    "too many levels of symbolic links"
  end

  defp message_1(:emfile) do
    "too many open files"
  end

  defp message_1(:emlink) do
    "too many links"
  end

  defp message_1(:emsgsize) do
    "message too long"
  end

  defp message_1(:emultihop) do
    "multihop attempted"
  end

  defp message_1(:enametoolong) do
    "file name too long"
  end

  defp message_1(:enavail) do
    "not available"
  end

  defp message_1(:enet) do
    "ENET"
  end

  defp message_1(:enetdown) do
    "network is down"
  end

  defp message_1(:enetreset) do
    "network dropped connection on reset"
  end

  defp message_1(:enetunreach) do
    "network is unreachable"
  end

  defp message_1(:enfile) do
    "file table overflow"
  end

  defp message_1(:enoano) do
    "anode table overflow"
  end

  defp message_1(:enobufs) do
    "no buffer space available"
  end

  defp message_1(:enocsi) do
    "no CSI structure available"
  end

  defp message_1(:enodata) do
    "no data available"
  end

  defp message_1(:enodev) do
    "no such device"
  end

  defp message_1(:enoent) do
    "no such file or directory"
  end

  defp message_1(:enoexec) do
    "exec format error"
  end

  defp message_1(:enolck) do
    "no locks available"
  end

  defp message_1(:enolink) do
    "link has be severed"
  end

  defp message_1(:enomem) do
    "not enough memory"
  end

  defp message_1(:enomsg) do
    "no message of desired type"
  end

  defp message_1(:enonet) do
    "machine is not on the network"
  end

  defp message_1(:enopkg) do
    "package not installed"
  end

  defp message_1(:enoprotoopt) do
    "bad proocol option"
  end

  defp message_1(:enospc) do
    "no space left on device"
  end

  defp message_1(:enosr) do
    "out of stream resources or not a stream device"
  end

  defp message_1(:enostr) do
    "not a stream"
  end

  defp message_1(:enosym) do
    "unresolved symbol name"
  end

  defp message_1(:enosys) do
    "function not implemented"
  end

  defp message_1(:enotblk) do
    "block device required"
  end

  defp message_1(:enotconn) do
    "socket is not connected"
  end

  defp message_1(:enotdir) do
    "not a directory"
  end

  defp message_1(:enotempty) do
    "directory not empty"
  end

  defp message_1(:enotnam) do
    "not a name file"
  end

  defp message_1(:enotsock) do
    "socket operation on non-socket"
  end

  defp message_1(:enotsup) do
    "operation not supported"
  end

  defp message_1(:enotty) do
    "inappropriate device for ioctl"
  end

  defp message_1(:enotuniq) do
    "name not unique on network"
  end

  defp message_1(:enxio) do
    "no such device or address"
  end

  defp message_1(:eopnotsupp) do
    "operation not supported on socket"
  end

  defp message_1(:eoverflow) do
    "offset too large for file system"
  end

  defp message_1(:eperm) do
    "not owner"
  end

  defp message_1(:epfnosupport) do
    "protocol family not supported"
  end

  defp message_1(:epipe) do
    "broken pipe"
  end

  defp message_1(:eproclim) do
    "too many processes"
  end

  defp message_1(:eprocunavail) do
    "bad procedure for program"
  end

  defp message_1(:eprogmismatch) do
    "program version wrong"
  end

  defp message_1(:eprogunavail) do
    "RPC program not available"
  end

  defp message_1(:eproto) do
    "protocol error"
  end

  defp message_1(:eprotonosupport) do
    "protocol not suppored"
  end

  defp message_1(:eprototype) do
    "protocol wrong type for socket"
  end

  defp message_1(:erange) do
    "math result unrepresentable"
  end

  defp message_1(:erefused) do
    "EREFUSED"
  end

  defp message_1(:eremchg) do
    "remote address changed"
  end

  defp message_1(:eremdev) do
    "remote device"
  end

  defp message_1(:eremote) do
    "pathname hit remote file system"
  end

  defp message_1(:eremoteio) do
    "remote i/o error"
  end

  defp message_1(:eremoterelease) do
    "EREMOTERELEASE"
  end

  defp message_1(:erofs) do
    "read-only file system"
  end

  defp message_1(:erpcmismatch) do
    "RPC version is wrong"
  end

  defp message_1(:erremote) do
    "object is remote"
  end

  defp message_1(:eshutdown) do
    "can't send after socket shutdown"
  end

  defp message_1(:esocktnosupport) do
    "socket type not supported"
  end

  defp message_1(:espipe) do
    "invalid seek"
  end

  defp message_1(:esrch) do
    "no such process"
  end

  defp message_1(:esrmnt) do
    "srmount error"
  end

  defp message_1(:estale) do
    "stale remote file handle"
  end

  defp message_1(:esuccess) do
    "Error 0"
  end

  defp message_1(:etime) do
    "timer expired"
  end

  defp message_1(:etimedout) do
    "connection timed out"
  end

  defp message_1(:etoomanyrefs) do
    "too many references: can't splice"
  end

  defp message_1(:etxtbsy) do
    "text file or pseudo-device busy"
  end

  defp message_1(:euclean) do
    "structure needs cleaning"
  end

  defp message_1(:eunatch) do
    "protocol driver not attached"
  end

  defp message_1(:eusers) do
    "too many users"
  end

  defp message_1(:eversion) do
    "version mismatch"
  end

  defp message_1(:ewouldblock) do
    "operation would block"
  end

  defp message_1(:exdev) do
    "cross-domain link"
  end

  defp message_1(:exfull) do
    "message tables full"
  end

  defp message_1(:nxdomain) do
    "non-existing domain"
  end

  defp message_1(:exbadport) do
    "inet_drv bad port state"
  end

  defp message_1(:exbadseq) do
    "inet_drv bad request sequence"
  end

  defp message_1(_) do
    "unknown POSIX error"
  end
end
