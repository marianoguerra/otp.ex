defmodule :m_ssh_xfer do
  use Bitwise
  import :lists, only: [foldl: 3, reverse: 1]
  require Record

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

  Record.defrecord(:r_ssh_xfer_attr, :ssh_xfer_attr,
    type: :undefined,
    size: :undefined,
    owner: :undefined,
    group: :undefined,
    permissions: :undefined,
    atime: :undefined,
    atime_nseconds: :undefined,
    createtime: :undefined,
    createtime_nseconds: :undefined,
    mtime: :undefined,
    mtime_nseconds: :undefined,
    acl: :undefined,
    attrib_bits: :undefined,
    extensions: :undefined
  )

  Record.defrecord(:r_ssh_xfer_ace, :ssh_xfer_ace,
    type: :undefined,
    flag: :undefined,
    mask: :undefined,
    who: :undefined
  )

  Record.defrecord(:r_ssh_xfer, :ssh_xfer,
    vsn: :undefined,
    ext: :undefined,
    cm: :undefined,
    channel: :undefined
  )

  def protocol_version_request(xF, version) do
    xf_request(xF, 1, <<version::size(32)-unsigned-big-integer>>)
  end

  def open(xF, reqID, fileName, access, flags, attrs) do
    vsn = r_ssh_xfer(xF, :vsn)

    mBits =
      cond do
        vsn >= 5 ->
          m = encode_ace_mask(access)
          <<m::size(32)-unsigned-big-integer>>

        true ->
          <<>>
      end

    f = encode_open_flags(flags)

    xf_request(xF, 3, [
      <<reqID::size(32)-unsigned-big-integer>>,
      <<:erlang.size(:unicode.characters_to_binary(fileName))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(fileName)::binary>>,
      mBits,
      <<f::size(32)-unsigned-big-integer>>,
      encode_ATTR(vsn, attrs)
    ])
  end

  def opendir(xF, reqID, dirName) do
    xf_request(xF, 11, [
      <<reqID::size(32)-unsigned-big-integer>>,
      <<:erlang.size(:unicode.characters_to_binary(dirName))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(dirName)::binary>>
    ])
  end

  def close(xF, reqID, handle) do
    xf_request(xF, 4, [
      <<reqID::size(32)-unsigned-big-integer>>,
      <<:erlang.size(handle)::size(32)-unsigned-big-integer, handle::binary>>
    ])
  end

  def read(xF, reqID, handle, offset, length) do
    xf_request(xF, 5, [
      <<reqID::size(32)-unsigned-big-integer>>,
      <<:erlang.size(handle)::size(32)-unsigned-big-integer, handle::binary>>,
      <<offset::size(64)-unsigned-big-integer>>,
      <<length::size(32)-unsigned-big-integer>>
    ])
  end

  def readdir(xF, reqID, handle) do
    xf_request(xF, 12, [
      <<reqID::size(32)-unsigned-big-integer>>,
      <<:erlang.size(handle)::size(32)-unsigned-big-integer, handle::binary>>
    ])
  end

  def write(xF, reqID, handle, offset, data) do
    data1 =
      cond do
        is_binary(data) ->
          data

        is_list(data) ->
          try do
            :erlang.iolist_to_binary(data)
          catch
            _, _ ->
              :unicode.characters_to_binary(data)
          end
      end

    xf_request(xF, 6, [
      <<reqID::size(32)-unsigned-big-integer>>,
      <<:erlang.size(handle)::size(32)-unsigned-big-integer, handle::binary>>,
      <<offset::size(64)-unsigned-big-integer>>,
      <<:erlang.size(data1)::size(32)-unsigned-big-integer, data1::binary>>
    ])
  end

  def remove(xF, reqID, file) do
    xf_request(xF, 13, [
      <<reqID::size(32)-unsigned-big-integer>>,
      <<:erlang.size(:unicode.characters_to_binary(file))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(file)::binary>>
    ])
  end

  def rename(xF, reqID, oldPath, newPath, flags) do
    vsn = r_ssh_xfer(xF, :vsn)

    flagBits =
      cond do
        vsn >= 5 ->
          f0 = encode_rename_flags(flags)
          <<f0::size(32)-unsigned-big-integer>>

        true ->
          <<>>
      end

    ext = r_ssh_xfer(xF, :ext)
    extRename = 'posix-rename@openssh.com'

    case :lists.member({extRename, '1'}, ext) do
      true ->
        extended(xF, reqID, extRename, [
          <<:erlang.size(:unicode.characters_to_binary(oldPath))::size(32)-unsigned-big-integer,
            :unicode.characters_to_binary(oldPath)::binary>>,
          <<:erlang.size(:unicode.characters_to_binary(newPath))::size(32)-unsigned-big-integer,
            :unicode.characters_to_binary(newPath)::binary>>
        ])

      false ->
        xf_request(xF, 18, [
          <<reqID::size(32)-unsigned-big-integer>>,
          <<:erlang.size(:unicode.characters_to_binary(oldPath))::size(32)-unsigned-big-integer,
            :unicode.characters_to_binary(oldPath)::binary>>,
          <<:erlang.size(:unicode.characters_to_binary(newPath))::size(32)-unsigned-big-integer,
            :unicode.characters_to_binary(newPath)::binary>>,
          flagBits
        ])
    end
  end

  def mkdir(xF, reqID, path, attrs) do
    xf_request(xF, 14, [
      <<reqID::size(32)-unsigned-big-integer>>,
      <<:erlang.size(:unicode.characters_to_binary(path))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(path)::binary>>,
      encode_ATTR(r_ssh_xfer(xF, :vsn), attrs)
    ])
  end

  def rmdir(xF, reqID, dir) do
    xf_request(xF, 15, [
      <<reqID::size(32)-unsigned-big-integer>>,
      <<:erlang.size(:unicode.characters_to_binary(dir))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(dir)::binary>>
    ])
  end

  def stat(xF, reqID, path, flags) do
    vsn = r_ssh_xfer(xF, :vsn)

    attrFlags =
      cond do
        vsn >= 5 ->
          f = encode_attr_flags(vsn, flags)
          <<f::size(32)-unsigned-big-integer>>

        true ->
          []
      end

    xf_request(xF, 17, [
      <<reqID::size(32)-unsigned-big-integer>>,
      <<:erlang.size(:unicode.characters_to_binary(path))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(path)::binary>>,
      attrFlags
    ])
  end

  def lstat(xF, reqID, path, flags) do
    vsn = r_ssh_xfer(xF, :vsn)

    attrFlags =
      cond do
        vsn >= 5 ->
          f = encode_attr_flags(vsn, flags)
          <<f::size(32)-unsigned-big-integer>>

        true ->
          []
      end

    xf_request(xF, 7, [
      <<reqID::size(32)-unsigned-big-integer>>,
      <<:erlang.size(:unicode.characters_to_binary(path))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(path)::binary>>,
      attrFlags
    ])
  end

  def fstat(xF, reqID, handle, flags) do
    vsn = r_ssh_xfer(xF, :vsn)

    attrFlags =
      cond do
        vsn >= 5 ->
          f = encode_attr_flags(vsn, flags)
          <<f::size(32)-unsigned-big-integer>>

        true ->
          []
      end

    xf_request(xF, 8, [
      <<reqID::size(32)-unsigned-big-integer>>,
      <<:erlang.size(handle)::size(32)-unsigned-big-integer, handle::binary>>,
      attrFlags
    ])
  end

  def setstat(xF, reqID, path, attrs) do
    xf_request(xF, 9, [
      <<reqID::size(32)-unsigned-big-integer>>,
      <<:erlang.size(:unicode.characters_to_binary(path))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(path)::binary>>,
      encode_ATTR(r_ssh_xfer(xF, :vsn), attrs)
    ])
  end

  def fsetstat(xF, reqID, handle, attrs) do
    xf_request(xF, 10, [
      <<reqID::size(32)-unsigned-big-integer>>,
      <<:erlang.size(handle)::size(32)-unsigned-big-integer, handle::binary>>,
      encode_ATTR(r_ssh_xfer(xF, :vsn), attrs)
    ])
  end

  def readlink(xF, reqID, path) do
    xf_request(xF, 19, [
      <<reqID::size(32)-unsigned-big-integer>>,
      <<:erlang.size(:unicode.characters_to_binary(path))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(path)::binary>>
    ])
  end

  def symlink(xF, reqID, linkPath, targetPath) do
    linkPath1 = :unicode.characters_to_binary(linkPath)
    targetPath1 = :unicode.characters_to_binary(targetPath)

    xf_request(xF, 20, [
      <<reqID::size(32)-unsigned-big-integer>>,
      <<:erlang.size(linkPath1)::size(32)-unsigned-big-integer, linkPath1::binary>>,
      <<:erlang.size(targetPath1)::size(32)-unsigned-big-integer, targetPath1::binary>>
    ])
  end

  def realpath(xF, reqID, path) do
    xf_request(xF, 16, [
      <<reqID::size(32)-unsigned-big-integer>>,
      <<:erlang.size(:unicode.characters_to_binary(path))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(path)::binary>>
    ])
  end

  def extended(xF, reqID, request, data) do
    xf_request(xF, 200, [
      <<reqID::size(32)-unsigned-big-integer>>,
      <<:erlang.size(:unicode.characters_to_binary(request))::size(32)-unsigned-big-integer,
        :unicode.characters_to_binary(request)::binary>>,
      data
    ])
  end

  defp xf_request(xF, op, arg) do
    cM = r_ssh_xfer(xF, :cm)
    channel = r_ssh_xfer(xF, :channel)

    data =
      cond do
        is_binary(arg) ->
          arg

        is_list(arg) ->
          try do
            :erlang.iolist_to_binary(arg)
          catch
            _, _ ->
              :unicode.characters_to_binary(arg)
          end
      end

    size = 1 + :erlang.size(data)
    :ssh_connection.send(cM, channel, [<<size::size(32)-unsigned-big-integer, op, data::binary>>])
  end

  def xf_send_reply(r_ssh_xfer(cm: cM, channel: channel), op, arg) do
    data =
      cond do
        is_binary(arg) ->
          arg

        is_list(arg) ->
          try do
            :erlang.iolist_to_binary(arg)
          catch
            _, _ ->
              :unicode.characters_to_binary(arg)
          end
      end

    size = 1 + :erlang.size(data)
    :ssh_connection.send(cM, channel, [<<size::size(32)-unsigned-big-integer, op, data::binary>>])
  end

  def xf_send_name(xF, reqId, name, attr) do
    xf_send_names(xF, reqId, [{name, attr}])
  end

  def xf_send_handle(r_ssh_xfer(cm: cM, channel: channel), reqId, handle) do
    hLen = length(handle)
    size = 1 + 4 + 4 + hLen

    toSend = [
      <<size::size(32)-unsigned-big-integer, 102, reqId::size(32)-unsigned-big-integer,
        hLen::size(32)-unsigned-big-integer>>,
      handle
    ]

    :ssh_connection.send(cM, channel, toSend)
  end

  def xf_send_names(r_ssh_xfer(cm: cM, channel: channel, vsn: vsn), reqId, namesAndAttrs) do
    count = length(namesAndAttrs)
    {data, len} = encode_names(vsn, namesAndAttrs)
    size = 1 + 4 + 4 + len

    toSend = [
      <<size::size(32)-unsigned-big-integer, 104, reqId::size(32)-unsigned-big-integer,
        count::size(32)-unsigned-big-integer>>,
      data
    ]

    :ssh_connection.send(cM, channel, toSend)
  end

  def xf_send_status(xF, reqId, errorCode) do
    xf_send_status(xF, reqId, errorCode, '')
  end

  def xf_send_status(xF, reqId, errorCode, errorMsg) do
    xf_send_status(xF, reqId, errorCode, errorMsg, <<>>)
  end

  def xf_send_status(r_ssh_xfer(cm: cM, channel: channel), reqId, errorCode, errorMsg, data) do
    langTag = 'en'
    eLen = length(errorMsg)
    tLen = 2
    size = 1 + 4 + 4 + 4 + eLen + 4 + tLen + :erlang.size(data)

    toSend = [
      <<size::size(32)-unsigned-big-integer, 101, reqId::size(32)-unsigned-big-integer,
        errorCode::size(32)-unsigned-big-integer>>,
      <<eLen::size(32)-unsigned-big-integer>>,
      errorMsg,
      <<tLen::size(32)-unsigned-big-integer>>,
      langTag,
      data
    ]

    :ssh_connection.send(cM, channel, toSend)
  end

  def xf_send_attr(r_ssh_xfer(cm: cM, channel: channel, vsn: vsn), reqId, attr) do
    encAttr = encode_ATTR(vsn, attr)
    aLen = :erlang.size(encAttr)
    size = 1 + 4 + aLen

    toSend = [
      <<size::size(32)-unsigned-big-integer, 105, reqId::size(32)-unsigned-big-integer>>,
      encAttr
    ]

    :ssh_connection.send(cM, channel, toSend)
  end

  def xf_send_data(r_ssh_xfer(cm: cM, channel: channel), reqId, data) do
    dLen = :erlang.size(data)
    size = 1 + 4 + 4 + dLen

    toSend = [
      <<size::size(32)-unsigned-big-integer, 103, reqId::size(32)-unsigned-big-integer,
        dLen::size(32)-unsigned-big-integer>>,
      data
    ]

    :ssh_connection.send(cM, channel, toSend)
  end

  def xf_reply(
        _XF,
        <<101, reqID::size(32)-unsigned-big-integer, status::size(32)-unsigned-big-integer,
          eLen::size(32)-unsigned-big-integer, err::size(eLen)-binary,
          lLen::size(32)-unsigned-big-integer, lang::size(lLen)-binary, reply::binary>>
      ) do
    stat = decode_status(status)
    {:status, reqID, {stat, :erlang.binary_to_list(err), :erlang.binary_to_list(lang), reply}}
  end

  def xf_reply(
        _XF,
        <<101, reqID::size(32)-unsigned-big-integer, status::size(32)-unsigned-big-integer>>
      ) do
    stat = decode_status(status)
    {:status, reqID, {stat, '', '', <<>>}}
  end

  def xf_reply(
        _XF,
        <<102, reqID::size(32)-unsigned-big-integer, hLen::size(32)-unsigned-big-integer,
          handle::size(hLen)-binary>>
      ) do
    {:handle, reqID, handle}
  end

  def xf_reply(
        _XF,
        <<103, reqID::size(32)-unsigned-big-integer, dLen::size(32)-unsigned-big-integer,
          data::size(dLen)-binary>>
      ) do
    {:data, reqID, data}
  end

  def xf_reply(
        xF,
        <<104, reqID::size(32)-unsigned-big-integer, count::size(32)-unsigned-big-integer,
          aData::binary>>
      ) do
    {:name, reqID, decode_names(r_ssh_xfer(xF, :vsn), count, aData)}
  end

  def xf_reply(
        xF,
        <<105, reqID::size(32)-unsigned-big-integer, aData::binary>>
      ) do
    {a, _} = decode_ATTR(r_ssh_xfer(xF, :vsn), aData)
    {:attrs, reqID, a}
  end

  def xf_reply(
        _XF,
        <<201, reqID::size(32)-unsigned-big-integer, rData>>
      ) do
    {:extended_reply, reqID, rData}
  end

  defp decode_status(status) do
    case status do
      0 ->
        :ok

      1 ->
        :eof

      2 ->
        :no_such_file

      3 ->
        :permission_denied

      4 ->
        :failure

      5 ->
        :bad_message

      6 ->
        :no_connection

      7 ->
        :connection_lost

      8 ->
        :op_unsupported

      9 ->
        :invalid_handle

      10 ->
        :no_such_path

      11 ->
        :file_already_exists

      12 ->
        :write_protect

      13 ->
        :no_media

      14 ->
        :no_space_on_filesystem

      15 ->
        :quota_exceeded

      16 ->
        :unknown_principle

      17 ->
        :lock_conflict

      19 ->
        :not_a_directory

      24 ->
        :file_is_a_directory

      22 ->
        :cannot_delete

      _ ->
        {:error, status}
    end
  end

  def encode_erlang_status(status) do
    case status do
      :ok ->
        0

      :eof ->
        1

      :enoent ->
        2

      :eacces ->
        3

      :eisdir ->
        24

      :eperm ->
        22

      :eexist ->
        11

      _ ->
        4
    end
  end

  def decode_ext(
        <<nameLen::size(32)-unsigned-big-integer, name::size(nameLen)-binary,
          dataLen::size(32)-unsigned-big-integer, data::size(dataLen)-binary, tail::binary>>
      ) do
    [
      {:erlang.binary_to_list(name), :erlang.binary_to_list(data)}
      | decode_ext(tail)
    ]
  end

  def decode_ext(<<>>) do
    []
  end

  defp encode_rename_flags(flags) do
    encode_bits(
      fn
        :overwrite ->
          1

        :atomic ->
          2

        :native ->
          4
      end,
      flags
    )
  end

  def encode_open_flags(flags) do
    encode_bits(
      fn
        :read ->
          1

        :write ->
          2

        :append ->
          4

        :creat ->
          8

        :trunc ->
          16

        :excl ->
          32

        :create_new ->
          0

        :create_truncate ->
          1

        :open_existing ->
          2

        :open_or_create ->
          3

        :truncate_existing ->
          4

        :append_data ->
          8

        :append_data_atomic ->
          16

        :text_mode ->
          32

        :read_lock ->
          64

        :write_lock ->
          128

        :delete_lock ->
          256
      end,
      flags
    )
  end

  defp encode_ace_mask(access) do
    encode_bits(
      fn
        :read_data ->
          1

        :list_directory ->
          1

        :write_data ->
          2

        :add_file ->
          2

        :append_data ->
          4

        :add_subdirectory ->
          4

        :read_named_attrs ->
          8

        :write_named_attrs ->
          16

        :execute ->
          32

        :delete_child ->
          64

        :read_attributes ->
          128

        :write_attributes ->
          256

        :delete ->
          65536

        :read_acl ->
          131_072

        :write_acl ->
          262_144

        :write_owner ->
          524_288

        :synchronize ->
          1_048_576
      end,
      access
    )
  end

  def decode_ace_mask(f) do
    decode_bits(
      f,
      [
        {1, :read_data},
        {1, :list_directory},
        {2, :write_data},
        {2, :add_file},
        {4, :append_data},
        {4, :add_subdirectory},
        {8, :read_named_attrs},
        {16, :write_named_attrs},
        {32, :execute},
        {64, :delete_child},
        {128, :read_attributes},
        {256, :write_attributes},
        {65536, :delete},
        {131_072, :read_acl},
        {262_144, :write_acl},
        {524_288, :write_owner},
        {1_048_576, :synchronize}
      ]
    )
  end

  def decode_open_flags(vsn, f) when vsn <= 3 do
    decode_bits(
      f,
      [{1, :read}, {2, :write}, {4, :append}, {8, :creat}, {16, :trunc}, {32, :excl}]
    )
  end

  def decode_open_flags(vsn, f) when vsn >= 4 do
    r =
      decode_bits(
        f,
        [
          {8, :append_data},
          {16, :append_data_atomic},
          {32, :text_mode},
          {64, :read_lock},
          {128, :write_lock},
          {256, :delete_lock}
        ]
      )

    aD =
      case f &&& 7 do
        0 ->
          :create_new

        1 ->
          :create_truncate

        2 ->
          :open_existing

        3 ->
          :open_or_create

        4 ->
          :truncate_existing
      end

    [aD | r]
  end

  defp encode_ace_type(type) do
    case type do
      :access_allowed ->
        0

      :access_denied ->
        1

      :system_audit ->
        2

      :system_alarm ->
        3
    end
  end

  defp decode_ace_type(f) do
    case f do
      0 ->
        :access_allowed

      1 ->
        :access_denied

      2 ->
        :system_audit

      3 ->
        :system_alarm
    end
  end

  defp encode_ace_flag(flag) do
    encode_bits(
      fn
        :file_inherit ->
          1

        :directory_inherit ->
          2

        :no_propagte_inherit ->
          4

        :inherit_only ->
          8

        :successful_access ->
          16

        :failed_access ->
          32

        :identifier_group ->
          64
      end,
      flag
    )
  end

  defp decode_ace_flag(f) do
    decode_bits(
      f,
      [
        {1, :file_inherit},
        {2, :directory_inherit},
        {4, :no_propagte_inherit},
        {8, :inherit_only},
        {16, :successful_access},
        {32, :failed_access},
        {64, :identifier_group}
      ]
    )
  end

  defp encode_attr_flags(vsn, :all) do
    encode_attr_flags(
      vsn,
      [
        :size,
        :uidgid,
        :permissions,
        :acmodtime,
        :accesstime,
        :createtime,
        :modifytime,
        :acl,
        :ownergroup,
        :subsecond_times,
        :bits,
        :extended
      ]
    )
  end

  defp encode_attr_flags(vsn, flags) do
    encode_bits(
      fn
        :size ->
          1

        :uidgid when vsn <= 3 ->
          2

        :permissions ->
          4

        :acmodtime when vsn <= 3 ->
          8

        :accesstime when vsn >= 5 ->
          8

        :createtime when vsn >= 5 ->
          16

        :modifytime when vsn >= 5 ->
          32

        :acl when vsn >= 5 ->
          64

        :ownergroup when vsn >= 5 ->
          128

        :subsecond_times when vsn >= 5 ->
          256

        :bits when vsn >= 5 ->
          512

        :extended when vsn >= 5 ->
          2_147_483_648

        _ ->
          0
      end,
      flags
    )
  end

  defp encode_file_type(type) do
    case type do
      :regular ->
        1

      :directory ->
        2

      :symlink ->
        3

      :special ->
        4

      :unknown ->
        5

      :other ->
        5

      :socket ->
        6

      :char_device ->
        7

      :block_device ->
        8

      :fifo ->
        9

      :undefined ->
        5
    end
  end

  defp decode_file_type(type) do
    case type do
      1 ->
        :regular

      2 ->
        :directory

      3 ->
        :symlink

      4 ->
        :special

      5 ->
        :other

      6 ->
        :socket

      7 ->
        :char_device

      8 ->
        :block_device

      9 ->
        :fifo
    end
  end

  defp encode_attrib_bits(bits) do
    encode_bits(
      fn
        :readonly ->
          1

        :system ->
          2

        :hidden ->
          4

        :case_insensitive ->
          8

        :arcive ->
          16

        :encrypted ->
          32

        :compressed ->
          64

        :sparse ->
          128

        :append_only ->
          256

        :immutable ->
          512

        :sync ->
          1024
      end,
      bits
    )
  end

  defp decode_attrib_bits(f) do
    decode_bits(
      f,
      [
        {1, :readonly},
        {2, :system},
        {4, :hidden},
        {8, :case_insensitive},
        {16, :arcive},
        {32, :encrypted},
        {64, :compressed},
        {128, :sparse},
        {256, :append_only},
        {512, :immutable},
        {1024, :sync}
      ]
    )
  end

  def encode_ATTR(vsn, a) do
    {flags, as} =
      encode_As(
        vsn,
        [
          {:size, r_ssh_xfer_attr(a, :size)},
          {:ownergroup, r_ssh_xfer_attr(a, :owner)},
          {:ownergroup, r_ssh_xfer_attr(a, :group)},
          {:permissions, r_ssh_xfer_attr(a, :permissions)},
          {:acmodtime, r_ssh_xfer_attr(a, :atime)},
          {:acmodtime, r_ssh_xfer_attr(a, :mtime)},
          {:accesstime, r_ssh_xfer_attr(a, :atime)},
          {:subsecond_times, r_ssh_xfer_attr(a, :atime_nseconds)},
          {:createtime, r_ssh_xfer_attr(a, :createtime)},
          {:subsecond_times, r_ssh_xfer_attr(a, :createtime_nseconds)},
          {:modifytime, r_ssh_xfer_attr(a, :mtime)},
          {:subsecond_times, r_ssh_xfer_attr(a, :mtime_nseconds)},
          {:acl, r_ssh_xfer_attr(a, :acl)},
          {:bits, r_ssh_xfer_attr(a, :attrib_bits)},
          {:extended, r_ssh_xfer_attr(a, :extensions)}
        ],
        0,
        []
      )

    type = encode_file_type(r_ssh_xfer_attr(a, :type))

    result =
      :erlang.list_to_binary([
        <<flags::size(32)-unsigned-big-integer>>,
        cond do
          vsn >= 5 ->
            <<type::size(8)-unsigned-big-integer>>

          true ->
            <<>>
        end,
        as
      ])

    result
  end

  defp encode_As(vsn, [{_AName, :undefined} | as], flags, acc) do
    encode_As(vsn, as, flags, acc)
  end

  defp encode_As(vsn, [{aName, x} | as], flags, acc) do
    case aName do
      :size ->
        encode_As(vsn, as, flags ||| 1, [<<x::size(64)-unsigned-big-integer>> | acc])

      :ownergroup when vsn <= 4 ->
        encode_As(vsn, as, flags ||| 2, [<<x::size(32)-unsigned-big-integer>> | acc])

      :ownergroup when vsn >= 5 ->
        x1 = :erlang.list_to_binary(:erlang.integer_to_list(x))

        encode_As(vsn, as, flags ||| 128, [
          <<:erlang.size(x1)::size(32)-unsigned-big-integer, x1::binary>>
          | acc
        ])

      :permissions ->
        encode_As(vsn, as, flags ||| 4, [<<x::size(32)-unsigned-big-integer>> | acc])

      :acmodtime when vsn <= 3 ->
        encode_As(vsn, as, flags ||| 8, [<<x::size(32)-unsigned-big-integer>> | acc])

      :accesstime when vsn >= 5 ->
        encode_As(vsn, as, flags ||| 8, [<<x::size(64)-unsigned-big-integer>> | acc])

      :createtime when vsn >= 5 ->
        encode_As(vsn, as, flags ||| 16, [<<x::size(64)-unsigned-big-integer>> | acc])

      :modifytime when vsn >= 5 ->
        encode_As(vsn, as, flags ||| 32, [<<x::size(64)-unsigned-big-integer>> | acc])

      :subsecond_times when vsn >= 5 ->
        encode_As(vsn, as, flags ||| 256, [<<x::size(64)-unsigned-big-integer>> | acc])

      :acl when vsn >= 5 ->
        encode_As(vsn, as, flags ||| 64, [encode_acl(x) | acc])

      :bits when vsn >= 5 ->
        f = encode_attrib_bits(x)
        encode_As(vsn, as, flags ||| 512, [<<f::size(32)-unsigned-big-integer>> | acc])

      :extended ->
        encode_As(vsn, as, flags ||| 2_147_483_648, [encode_extensions(x) | acc])

      _ ->
        encode_As(vsn, as, flags, acc)
    end
  end

  defp encode_As(_Vsn, [], flags, acc) do
    {flags, reverse(acc)}
  end

  def decode_ATTR(
        vsn,
        <<flags::size(32)-unsigned-big-integer, tail::binary>>
      ) do
    {type, tail2} =
      cond do
        vsn <= 3 ->
          {5, tail}

        true ->
          <<t::size(8)-unsigned-big-integer, tL::binary>> = tail
          {t, tL}
      end

    decode_As(
      vsn,
      [
        {:size, r_ssh_xfer_attr(:size)},
        {:ownergroup, r_ssh_xfer_attr(:owner)},
        {:ownergroup, r_ssh_xfer_attr(:group)},
        {:permissions, r_ssh_xfer_attr(:permissions)},
        {:acmodtime, r_ssh_xfer_attr(:atime)},
        {:acmodtime, r_ssh_xfer_attr(:mtime)},
        {:accesstime, r_ssh_xfer_attr(:atime)},
        {:subsecond_times, r_ssh_xfer_attr(:atime_nseconds)},
        {:createtime, r_ssh_xfer_attr(:createtime)},
        {:subsecond_times, r_ssh_xfer_attr(:createtime_nseconds)},
        {:modifytime, r_ssh_xfer_attr(:mtime)},
        {:subsecond_times, r_ssh_xfer_attr(:mtime_nseconds)},
        {:acl, r_ssh_xfer_attr(:acl)},
        {:bits, r_ssh_xfer_attr(:attrib_bits)},
        {:extended, r_ssh_xfer_attr(:extensions)}
      ],
      r_ssh_xfer_attr(type: decode_file_type(type)),
      flags,
      tail2
    )
  end

  defp decode_As(vsn, [{aName, aField} | as], r, flags, tail) do
    case aName do
      :size when 1 &&& flags == 1 ->
        <<x::size(64)-unsigned-big-integer, tail2::binary>> = tail
        decode_As(vsn, as, :erlang.setelement(aField, r, x), flags, tail2)

      :ownergroup when 2 &&& flags == 2 and vsn <= 3 ->
        <<x::size(32)-unsigned-big-integer, tail2::binary>> = tail
        decode_As(vsn, as, :erlang.setelement(aField, r, x), flags, tail2)

      :ownergroup when 128 &&& flags == 128 and vsn >= 5 ->
        <<len::size(32)-unsigned-big-integer, bin::size(len)-binary, tail2::binary>> = tail
        x = :erlang.binary_to_list(bin)
        decode_As(vsn, as, :erlang.setelement(aField, r, x), flags, tail2)

      :permissions when 4 &&& flags == 4 and vsn >= 5 ->
        <<x::size(32)-unsigned-big-integer, tail2::binary>> = tail
        decode_As(vsn, as, :erlang.setelement(aField, r, x), flags, tail2)

      :permissions when 4 &&& flags == 4 and vsn <= 3 ->
        <<x::size(32)-unsigned-big-integer, tail2::binary>> = tail
        r1 = :erlang.setelement(aField, r, x)

        type =
          case x &&& 61440 do
            16384 ->
              :directory

            8192 ->
              :char_device

            24576 ->
              :block_device

            4096 ->
              :fifi

            32768 ->
              :regular

            49152 ->
              :socket

            40960 ->
              :symlink

            _ ->
              :unknown
          end

        decode_As(vsn, as, r_ssh_xfer_attr(r1, type: type), flags, tail2)

      :acmodtime when 8 &&& flags == 8 and vsn <= 3 ->
        <<x::size(32)-unsigned-big-integer, tail2::binary>> = tail
        decode_As(vsn, as, :erlang.setelement(aField, r, x), flags, tail2)

      :accesstime when 8 &&& flags == 8 and vsn >= 5 ->
        <<x::size(64)-unsigned-big-integer, tail2::binary>> = tail
        decode_As(vsn, as, :erlang.setelement(aField, r, x), flags, tail2)

      :modifytime when 32 &&& flags == 32 and vsn >= 5 ->
        <<x::size(64)-unsigned-big-integer, tail2::binary>> = tail
        decode_As(vsn, as, :erlang.setelement(aField, r, x), flags, tail2)

      :createtime when 16 &&& flags == 16 and vsn >= 5 ->
        <<x::size(64)-unsigned-big-integer, tail2::binary>> = tail
        decode_As(vsn, as, :erlang.setelement(aField, r, x), flags, tail2)

      :subsecond_times
      when 256 &&& flags == 256 and
             vsn >= 5 ->
        <<x::size(32)-unsigned-big-integer, tail2::binary>> = tail
        decode_As(vsn, as, :erlang.setelement(aField, r, x), flags, tail2)

      :acl when 64 &&& flags == 64 and vsn >= 5 ->
        {x, tail2} = decode_acl(tail)
        decode_As(vsn, as, :erlang.setelement(aField, r, x), flags, tail2)

      :bits when 512 &&& flags == 512 and vsn >= 5 ->
        <<y::size(32)-unsigned-big-integer, tail2::binary>> = tail
        x = decode_attrib_bits(y)
        decode_As(vsn, as, :erlang.setelement(aField, r, x), flags, tail2)

      :extended when 2_147_483_648 &&& flags == 2_147_483_648 ->
        {x, tail2} = decode_extended(tail)
        decode_As(vsn, as, :erlang.setelement(aField, r, x), flags, tail2)

      _ ->
        decode_As(vsn, as, r, flags, tail)
    end
  end

  defp decode_As(_Vsn, [], r, _, tail) do
    {r, tail}
  end

  defp decode_names(_Vsn, 0, _Data) do
    []
  end

  defp decode_names(
         vsn,
         i,
         <<len::size(32)-unsigned-big-integer, fileName::size(len)-binary,
           lLen::size(32)-unsigned-big-integer, _LongName::size(lLen)-binary, tail::binary>>
       )
       when vsn <= 3 do
    name = :unicode.characters_to_list(fileName)
    {a, tail2} = decode_ATTR(vsn, tail)
    [{name, a} | decode_names(vsn, i - 1, tail2)]
  end

  defp decode_names(
         vsn,
         i,
         <<len::size(32)-unsigned-big-integer, fileName::size(len)-binary, tail::binary>>
       )
       when vsn >= 4 do
    name = :unicode.characters_to_list(fileName)
    {a, tail2} = decode_ATTR(vsn, tail)
    [{name, a} | decode_names(vsn, i - 1, tail2)]
  end

  defp encode_names(vsn, namesAndAttrs) do
    :lists.mapfoldl(
      fn n, l ->
        encode_name(vsn, n, l)
      end,
      0,
      namesAndAttrs
    )
  end

  defp encode_name(vsn, {nameUC, attr}, len) when vsn <= 3 do
    name = :erlang.binary_to_list(:unicode.characters_to_binary(nameUC))
    nLen = length(name)
    encAttr = encode_ATTR(vsn, attr)
    aLen = :erlang.size(encAttr)
    newLen = len + nLen * 2 + 4 + 4 + aLen

    {[
       <<nLen::size(32)-unsigned-big-integer>>,
       name,
       <<nLen::size(32)-unsigned-big-integer>>,
       name,
       encAttr
     ], newLen}
  end

  defp encode_name(vsn, {nameUC, attr}, len) when vsn >= 4 do
    name = :erlang.binary_to_list(:unicode.characters_to_binary(nameUC))
    nLen = length(name)
    encAttr = encode_ATTR(vsn, attr)
    aLen = :erlang.size(encAttr)
    {[<<nLen::size(32)-unsigned-big-integer>>, name, encAttr], len + 4 + nLen + aLen}
  end

  defp encode_acl(aCLList) do
    count = length(aCLList)

    [
      <<count::size(32)-unsigned-big-integer>>
      | encode_acl_items(aCLList)
    ]
  end

  defp encode_acl_items([aCE | as]) do
    type = encode_ace_type(r_ssh_xfer_ace(aCE, :type))
    flag = encode_ace_flag(r_ssh_xfer_ace(aCE, :flag))
    mask = encode_ace_mask(r_ssh_xfer_ace(aCE, :mask))
    who = r_ssh_xfer_ace(aCE, :who)

    [
      [
        <<type::size(32)-unsigned-big-integer>>,
        <<flag::size(32)-unsigned-big-integer>>,
        <<mask::size(32)-unsigned-big-integer>>,
        <<:erlang.size(:unicode.characters_to_binary(who))::size(32)-unsigned-big-integer,
          :unicode.characters_to_binary(who)::binary>>
      ]
      | encode_acl_items(as)
    ]
  end

  defp encode_acl_items([]) do
    []
  end

  defp decode_acl(<<count::size(32)-unsigned-big-integer, tail::binary>>) do
    decode_acl_items(count, tail, [])
  end

  defp decode_acl_items(0, tail, acc) do
    {reverse(acc), tail}
  end

  defp decode_acl_items(
         i,
         <<type::size(32)-unsigned-big-integer, flag::size(32)-unsigned-big-integer,
           mask::size(32)-unsigned-big-integer, wLen::size(32)-unsigned-big-integer,
           bWho::size(wLen)-binary, tail::binary>>,
         acc
       ) do
    decode_acl_items(i - 1, tail, [
      r_ssh_xfer_ace(
        type: decode_ace_type(type),
        flag: decode_ace_flag(flag),
        mask: decode_ace_mask(mask),
        who: :unicode.characters_to_list(bWho)
      )
      | acc
    ])
  end

  defp encode_extensions(exts) do
    count = length(exts)

    [
      <<count::size(32)-unsigned-big-integer>>
      | encode_ext(exts)
    ]
  end

  defp encode_ext([{type, data} | exts]) do
    [
      [
        <<:erlang.size(:unicode.characters_to_binary(type))::size(32)-unsigned-big-integer,
          :unicode.characters_to_binary(type)::binary>>,
        <<:erlang.size(:unicode.characters_to_binary(data))::size(32)-unsigned-big-integer,
          :unicode.characters_to_binary(data)::binary>>
      ]
      | encode_ext(exts)
    ]
  end

  defp encode_ext([]) do
    []
  end

  defp decode_extended(<<count::size(32)-unsigned-big-integer, tail::binary>>) do
    decode_ext(count, tail, [])
  end

  defp decode_ext(0, tail, acc) do
    {reverse(acc), tail}
  end

  defp decode_ext(
         i,
         <<tLen::size(32)-unsigned-big-integer, type::size(tLen)-binary,
           dLen::size(32)-unsigned-big-integer, data::size(dLen)-binary, tail::binary>>,
         acc
       ) do
    decode_ext(i - 1, tail, [{:erlang.binary_to_list(type), data} | acc])
  end

  defp encode_bits(fun, bitNames) do
    encode_bits(fun, 0, bitNames)
  end

  defp encode_bits(fun, f, [bit | bitNames]) do
    encode_bits(fun, fun.(bit) ||| f, bitNames)
  end

  defp encode_bits(_Fun, f, []) do
    f
  end

  defp decode_bits(f, [{bit, bitName} | bits]) do
    cond do
      f &&& bit == bit ->
        [bitName | decode_bits(f, bits)]

      true ->
        decode_bits(f, bits)
    end
  end

  defp decode_bits(_F, []) do
    []
  end
end
