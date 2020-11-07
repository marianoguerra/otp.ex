defmodule :m_prim_zip do
  use Bitwise
  require Record

  Record.defrecord(:r_file_info, :file_info,
    size: :undefined,
    type: :undefined,
    access: :undefined,
    atime: :undefined,
    mtime: :undefined,
    ctime: :undefined,
    mode: :undefined,
    links: :undefined,
    major_device: :undefined,
    minor_device: :undefined,
    inode: :undefined,
    uid: :undefined,
    gid: :undefined
  )

  Record.defrecord(:r_file_descriptor, :file_descriptor,
    module: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_zip_file, :zip_file,
    name: :undefined,
    info: :undefined,
    comment: :undefined,
    offset: :undefined,
    comp_size: :undefined
  )

  Record.defrecord(:r_zip_comment, :zip_comment, comment: :undefined)

  Record.defrecord(:r_local_file_header, :local_file_header,
    version_needed: :undefined,
    gp_flag: :undefined,
    comp_method: :undefined,
    last_mod_time: :undefined,
    last_mod_date: :undefined,
    crc32: :undefined,
    comp_size: :undefined,
    uncomp_size: :undefined,
    file_name_length: :undefined,
    extra_field_length: :undefined
  )

  Record.defrecord(:r_cd_file_header, :cd_file_header,
    version_made_by: :undefined,
    version_needed: :undefined,
    gp_flag: :undefined,
    comp_method: :undefined,
    last_mod_time: :undefined,
    last_mod_date: :undefined,
    crc32: :undefined,
    comp_size: :undefined,
    uncomp_size: :undefined,
    file_name_length: :undefined,
    extra_field_length: :undefined,
    file_comment_length: :undefined,
    disk_num_start: :undefined,
    internal_attr: :undefined,
    external_attr: :undefined,
    local_header_offset: :undefined
  )

  Record.defrecord(:r_unix_extra_field, :unix_extra_field,
    atime: :undefined,
    mtime: :undefined,
    uid: :undefined,
    gid: :undefined
  )

  Record.defrecord(:r_extended_timestamp, :extended_timestamp,
    mtime: :undefined,
    atime: :undefined,
    ctime: :undefined
  )

  Record.defrecord(:r_eocd, :eocd,
    disk_num: :undefined,
    start_disk_num: :undefined,
    entries_on_disk: :undefined,
    entries: :undefined,
    size: :undefined,
    offset: :undefined,
    zip_comment_length: :undefined
  )

  Record.defrecord(:r_primzip_file, :primzip_file,
    name: :undefined,
    get_info: :undefined,
    get_bin: :undefined
  )

  Record.defrecord(:r_primzip, :primzip,
    files: [],
    zlib: :undefined,
    input: :undefined,
    in: :undefined
  )

  defp filter_fun() do
    continue = true
    include = true

    fn {_Name, _GetInfoFun, _GetBinFun}, acc ->
      {continue, include, acc}
    end
  end

  def open(f) do
    open(filter_fun(), :undefined, f)
  end

  def open(filterFun, filterAcc, f)
      when is_function(filterFun, 2) do
    try do
      do_open(filterFun, filterAcc, f)
    catch
      {:filter_fun_throw, reason} ->
        throw(reason)

      internalReason ->
        {:error, internalReason}

      class, reason ->
        :erlang.error(:erlang.raise(class, reason, __STACKTRACE__))
    end
  end

  def open(_, _, _) do
    {:error, :einval}
  end

  defp do_open(filterFun, filterAcc, f) do
    input = get_zip_input(f)
    in0 = input.({:open, f, [:read, :binary, :raw]}, [])
    z = :zlib.open()
    primZip = r_primzip(files: [], zlib: z, in: in0, input: input)

    try do
      {primZip2, filterAcc2} = get_central_dir(primZip, filterFun, filterAcc)
      {:ok, primZip2, filterAcc2}
    catch
      class, reason ->
        _ = close(primZip)
        :erlang.error(:erlang.raise(class, reason, __STACKTRACE__))
    end
  end

  def foldl(filterFun, filterAcc, r_primzip(files: files) = primZip)
      when is_function(filterFun, 2) do
    try do
      {:ok, filterAcc2, primZip2} = do_foldl(filterFun, filterAcc, files, [], primZip, primZip)
      {:ok, primZip2, filterAcc2}
    catch
      {:filter_fun_throw, reason} ->
        throw(reason)

      internalReason ->
        {:error, internalReason}

      class, reason ->
        :erlang.error(:erlang.raise(class, reason, __STACKTRACE__))
    end
  end

  def foldl(_, _, _) do
    {:error, :einval}
  end

  defp do_foldl(filterFun, filterAcc, [pF | tail], acc0, primZip, primZipOrig) do
    r_primzip_file(name: f, get_info: getInfo, get_bin: getBin) = pF

    try do
      filterFun.({f, getInfo, getBin}, filterAcc)
    catch
      reason ->
        throw({:filter_fun_throw, reason})
    else
      {continue, include, filterAcc2} ->
        acc1 = include_acc(include, pF, acc0)

        case continue do
          true ->
            do_foldl(filterFun, filterAcc2, tail, acc1, primZip, primZipOrig)

          false ->
            {:ok, filterAcc2, primZipOrig}
        end

      filterRes ->
        throw({:illegal_filter, filterRes})
    end
  end

  defp do_foldl(_FilterFun, filterAcc, [], acc, primZip, _PrimZipOrig) do
    {:ok, filterAcc, r_primzip(primZip, files: reverse(acc))}
  end

  defp include_acc(include, pF, acc) do
    case include do
      false ->
        acc

      true ->
        [pF | acc]

      {true, nick} ->
        [r_primzip_file(pF, name: nick) | acc]

      {true, nick, getInfo, getBin} ->
        pF2 = r_primzip_file(name: nick, get_info: getInfo, get_bin: getBin)
        [pF2 | acc]

      list when is_list(list) ->
        fun = fn i, a ->
          include_acc(i, pF, a)
        end

        lists_foldl(fun, acc, list)

      bad ->
        throw({:illegal_filter, bad})
    end
  end

  defp lists_foldl(f, accu, [hd | tail]) do
    lists_foldl(f, f.(hd, accu), tail)
  end

  defp lists_foldl(f, accu, []) when is_function(f, 2) do
    accu
  end

  def close(r_primzip(in: in0, input: input, zlib: z)) do
    input.(:close, in0)
    :zlib.close(z)
  end

  def close(_) do
    {:error, :einval}
  end

  defp get_zip_input({f, b}) when is_binary(b) and is_list(f) do
    &binary_io/2
  end

  defp get_zip_input(f) when is_list(f) do
    &prim_file_io/2
  end

  defp get_zip_input(_) do
    throw(:einval)
  end

  defp get_z_file(f, offset, chunkSize, r_primzip(zlib: z, in: in0, input: input)) do
    case input.({:pread, offset, chunkSize}, in0) do
      {<<67_324_752::size(32)-little,
         bLH::size(4 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 - 4)-binary, _::binary>> = b,
       _In1} ->
        r_local_file_header(
          gp_flag: gPFlag,
          file_name_length: fNLen,
          extra_field_length: eFLen,
          comp_method: compMethod
        ) =
          local_file_header_from_bin(
            bLH,
            f
          )

        dataOffs =
          4 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 + fNLen + eFLen +
            offset_over_z_data_descriptor(gPFlag)

        case b do
          <<_::size(dataOffs)-binary, data::binary>> ->
            out = get_z_all(compMethod, data, z, f)
            out

          _ ->
            throw({:bad_local_file_offset, f})
        end

      _ ->
        throw({:bad_local_file_header, f})
    end
  end

  defp get_z_all(8, compressed, z, _F) do
    :ok = :zlib.inflateInit(z, -15)
    uncompressed = :zlib.inflate(z, compressed)

    try do
      :zlib.inflateEnd(z)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :erlang.iolist_to_binary(uncompressed)
  end

  defp get_z_all(0, stored, _Z, _F) do
    stored
  end

  defp get_z_all(compMethod, _, _, f) do
    throw({:unsupported_compression, f, compMethod})
  end

  defp offset_over_z_data_descriptor(gPFlag) when gPFlag &&& 8 === 8 do
    12
  end

  defp offset_over_z_data_descriptor(_GPFlag) do
    0
  end

  defp get_central_dir(r_primzip(in: in0, input: input) = primZip, filterFun, filterAcc) do
    {b, in1} = get_end_of_central_dir(in0, 4 + 2 + 2 + 2 + 2 + 4 + 4 + 2, input)
    {eOCD, _BComment} = eocd_and_comment_from_bin(b)

    {bCD, in2} =
      input.(
        {:pread, r_eocd(eOCD, :offset), r_eocd(eOCD, :size)},
        in1
      )

    n = r_eocd(eOCD, :entries)
    endOffset = r_eocd(eOCD, :offset)
    primZip2 = r_primzip(primZip, in: in2)

    cond do
      n === 0 ->
        {primZip2, filterAcc}

      true ->
        {f, offset, cFH, bCDRest} = get_file_header(bCD)

        get_cd_loop(
          n,
          bCDRest,
          [],
          primZip2,
          f,
          offset,
          cFH,
          endOffset,
          filterFun,
          filterAcc,
          primZip
        )
    end
  end

  defp get_cd_loop(
         n,
         bCD,
         acc0,
         primZip,
         fileName,
         offset,
         cFH,
         endOffset,
         filterFun,
         filterAcc,
         primZipOrig
       ) do
    {nextF, nextOffset, nextCFH, bCDRest, size} =
      cond do
        n === 1 ->
          {:undefined, :undefined, :undefined, :undefined, endOffset - offset}

        true ->
          {nextF0, nextOffset0, nextCFH0, bCDRest0} = get_file_header(bCD)
          {nextF0, nextOffset0, nextCFH0, bCDRest0, nextOffset0 - offset}
      end

    getInfo = fn ->
      cd_file_header_to_file_info(fileName, cFH, <<>>)
    end

    getBin = fn ->
      get_z_file(fileName, offset, size, primZip)
    end

    pF = r_primzip_file(name: fileName, get_info: getInfo, get_bin: getBin)

    try do
      filterFun.({fileName, getInfo, getBin}, filterAcc)
    catch
      reason ->
        throw({:filter_fun_throw, reason})
    else
      {continue, include, filterAcc2} ->
        acc1 =
          case include do
            false ->
              acc0

            true ->
              [pF | acc0]

            {true, nick} ->
              [r_primzip_file(pF, name: nick) | acc0]

            {true, nick, gI, gB} ->
              pF2 = r_primzip_file(name: nick, get_info: gI, get_bin: gB)
              [pF2 | acc0]

            list when is_list(list) ->
              fun = fn i, a ->
                include_acc(i, pF, a)
              end

              lists_foldl(fun, acc0, list)
          end

        case continue do
          true when n > 1 ->
            get_cd_loop(
              n - 1,
              bCDRest,
              acc1,
              primZip,
              nextF,
              nextOffset,
              nextCFH,
              endOffset,
              filterFun,
              filterAcc2,
              primZipOrig
            )

          true ->
            primZip2 = r_primzip(primZip, files: reverse(acc1))
            {primZip2, filterAcc2}

          false ->
            {primZipOrig, filterAcc2}
        end

      filterRes ->
        throw({:illegal_filter, filterRes})
    end
  end

  defp get_file_header(bCD) do
    bCFH =
      case bCD do
        <<33_639_248::size(32)-little,
          b::size(4 + 2 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4 - 4)-binary,
          _::binary>> ->
          b

        _ ->
          throw(:bad_central_directory)
      end

    cFH = cd_file_header_from_bin(bCFH)
    fileNameLen = r_cd_file_header(cFH, :file_name_length)
    extraLen = r_cd_file_header(cFH, :extra_field_length)
    commentLen = r_cd_file_header(cFH, :file_comment_length)
    toGet = fileNameLen + extraLen + commentLen

    {b2, bCDRest} =
      case bCD do
        <<_::size(4 + 2 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4)-binary,
          g::size(toGet)-binary, rest::binary>> ->
          {g, rest}

        _ ->
          throw(:bad_central_directory)
      end

    fileName = get_filename_from_b2(b2, fileNameLen, extraLen, commentLen)
    offset = r_cd_file_header(cFH, :local_header_offset)
    {fileName, offset, cFH, bCDRest}
  end

  defp get_filename_from_b2(b, fileNameLen, extraLen, commentLen) do
    case b do
      <<bFileName::size(fileNameLen)-binary, _BExtra::size(extraLen)-binary,
        _BComment::size(commentLen)-binary>> ->
        :erlang.binary_to_list(bFileName)

      _ ->
        throw(:bad_central_directory)
    end
  end

  defp get_end_of_central_dir(_In, sz, _Input) when sz > 65535 do
    throw(:bad_eocd)
  end

  defp get_end_of_central_dir(in0, sz, input) do
    in1 = input.({:seek, :eof, -sz}, in0)
    {b, in2} = input.({:read, sz}, in1)

    case find_eocd_header(b) do
      :none ->
        get_end_of_central_dir(in2, sz + sz, input)

      header ->
        {header, in2}
    end
  end

  defp find_eocd_header(<<101_010_256::size(32)-little, rest::binary>>) do
    rest
  end

  defp find_eocd_header(<<_::size(8), rest::binary>>)
       when byte_size(rest) > 4 + 2 + 2 + 2 + 2 + 4 + 4 + 2 - 4 do
    find_eocd_header(rest)
  end

  defp find_eocd_header(_) do
    :none
  end

  defp prim_file_io({:file_info, f}, _) do
    case :prim_file.read_file_info(f) do
      {:ok, info} ->
        info

      {:error, e} ->
        throw(e)
    end
  end

  defp prim_file_io({:open, fN, opts}, _) do
    case (try do
            :prim_file.open(fN, opts ++ [:binary])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, h} ->
        h

      {:error, e} ->
        throw(e)
    end
  end

  defp prim_file_io({:read, n}, h) do
    case :prim_file.read(h, n) do
      {:ok, b} ->
        {b, h}

      :eof ->
        {:eof, h}

      {:error, e} ->
        throw(e)
    end
  end

  defp prim_file_io({:pread, pos, n}, h) do
    case :prim_file.pread(h, pos, n) do
      {:ok, b} ->
        {b, h}

      :eof ->
        {:eof, h}

      {:error, e} ->
        throw(e)
    end
  end

  defp prim_file_io({:seek, s, pos}, h) do
    case :prim_file.position(h, {s, pos}) do
      {:ok, _NewPos} ->
        h

      {:error, error} ->
        throw(error)
    end
  end

  defp prim_file_io({:write, data}, h) do
    case :prim_file.write(h, data) do
      :ok ->
        h

      {:error, error} ->
        throw(error)
    end
  end

  defp prim_file_io({:pwrite, pos, data}, h) do
    case :prim_file.pwrite(h, pos, data) do
      :ok ->
        h

      {:error, error} ->
        throw(error)
    end
  end

  defp prim_file_io({:close, fN}, h) do
    case :prim_file.close(h) do
      :ok ->
        fN

      {:error, error} ->
        throw(error)
    end
  end

  defp prim_file_io(:close, h) do
    prim_file_io({:close, :ok}, h)
  end

  defp prim_file_io({:set_file_info, f, fI}, h) do
    case :prim_file.write_file_info(f, fI) do
      :ok ->
        h

      {:error, error} ->
        throw(error)
    end
  end

  defp binary_io({:pread, newPos, n}, {oldPos, b}) do
    case b do
      <<_::size(newPos)-binary, read::size(n)-binary, _Rest::binary>> ->
        {read, {newPos + n, b}}

      _ ->
        {:eof, {oldPos, b}}
    end
  end

  defp binary_io({:read, n}, {pos, b})
       when pos >= byte_size(b) do
    {:eof, {pos + n, b}}
  end

  defp binary_io({:read, n}, {pos, b})
       when pos + n > byte_size(b) do
    case b do
      <<_::size(pos)-binary, read::binary>> ->
        {read, {byte_size(b), b}}

      _ ->
        {:eof, {pos, b}}
    end
  end

  defp binary_io({:read, n}, {pos, b}) do
    case b do
      <<_::size(pos)-binary, read::size(n)-binary, _::binary>> ->
        {read, {pos + n, b}}

      _ ->
        {:eof, {pos, b}}
    end
  end

  defp binary_io({:seek, :bof, pos}, {_OldPos, b}) do
    {pos, b}
  end

  defp binary_io({:seek, :cur, pos}, {oldPos, b}) do
    {oldPos + pos, b}
  end

  defp binary_io({:seek, :eof, pos}, {_OldPos, b}) do
    {byte_size(b) + pos, b}
  end

  defp binary_io({:file_info, {_Filename, b}}, a) do
    binary_io({:file_info, b}, a)
  end

  defp binary_io({:file_info, b}, _) do
    {type, size} =
      cond do
        is_binary(b) ->
          {:regular, byte_size(b)}

        b === :directory ->
          {:directory, 0}
      end

    now = :erlang.localtime()

    r_file_info(
      size: size,
      type: type,
      access: :read_write,
      atime: now,
      mtime: now,
      ctime: now,
      mode: 0,
      links: 1,
      major_device: 0,
      minor_device: 0,
      inode: 0,
      uid: 0,
      gid: 0
    )
  end

  defp binary_io({:pwrite, pos, data}, {oldPos, b}) do
    {oldPos, pwrite_binary(b, pos, data)}
  end

  defp binary_io({:write, data}, {pos, b}) do
    {pos + :erlang.iolist_size(data), pwrite_binary(b, pos, data)}
  end

  defp binary_io({:open, {_Filename, b}, _Opts}, _) do
    {0, b}
  end

  defp binary_io({:open, b, _Opts}, _) when is_binary(b) do
    {0, b}
  end

  defp binary_io({:open, filename, _Opts}, _)
       when is_list(filename) do
    {0, <<>>}
  end

  defp binary_io(:close, {_Pos, b}) do
    b
  end

  defp binary_io({:close, fN}, {_Pos, b}) do
    {fN, b}
  end

  defp eocd_and_comment_from_bin(
         <<diskNum::size(16)-little, startDiskNum::size(16)-little,
           entriesOnDisk::size(16)-little, entries::size(16)-little, size::size(32)-little,
           offset::size(32)-little, zipCommentLength::size(16)-little,
           comment::size(zipCommentLength)-binary>>
       ) do
    {r_eocd(
       disk_num: diskNum,
       start_disk_num: startDiskNum,
       entries_on_disk: entriesOnDisk,
       entries: entries,
       size: size,
       offset: offset,
       zip_comment_length: zipCommentLength
     ), comment}
  end

  defp eocd_and_comment_from_bin(_) do
    throw(:bad_eocd)
  end

  defp cd_file_header_to_file_info(
         fileName,
         r_cd_file_header(
           uncomp_size: uncompSize,
           last_mod_time: modTime,
           last_mod_date: modDate
         ),
         extraField
       )
       when is_binary(extraField) do
    t = dos_date_time_to_datetime(modDate, modTime)

    type =
      case last(fileName) do
        ?/ ->
          :directory

        _ ->
          :regular
      end

    fI =
      r_file_info(
        size: uncompSize,
        type: type,
        access: :read_write,
        atime: t,
        mtime: t,
        ctime: t,
        mode: 54,
        links: 1,
        major_device: 0,
        minor_device: 0,
        inode: 0,
        uid: 0,
        gid: 0
      )

    add_extra_info(fI, extraField)
  end

  defp add_extra_info(fI, _) do
    fI
  end

  defp dos_date_time_to_datetime(dosDate, dosTime) do
    <<hour::size(5), min::size(6), sec::size(5)>> = <<dosTime::size(16)>>
    <<yearFrom1980::size(7), month::size(4), day::size(5)>> = <<dosDate::size(16)>>
    {{yearFrom1980 + 1980, month, day}, {hour, min, sec}}
  end

  defp cd_file_header_from_bin(
         <<versionMadeBy::size(16)-little, versionNeeded::size(16)-little,
           gPFlag::size(16)-little, compMethod::size(16)-little, lastModTime::size(16)-little,
           lastModDate::size(16)-little, cRC32::size(32)-little, compSize::size(32)-little,
           uncompSize::size(32)-little, fileNameLength::size(16)-little,
           extraFieldLength::size(16)-little, fileCommentLength::size(16)-little,
           diskNumStart::size(16)-little, internalAttr::size(16)-little,
           externalAttr::size(32)-little, localHeaderOffset::size(32)-little>>
       ) do
    r_cd_file_header(
      version_made_by: versionMadeBy,
      version_needed: versionNeeded,
      gp_flag: gPFlag,
      comp_method: compMethod,
      last_mod_time: lastModTime,
      last_mod_date: lastModDate,
      crc32: cRC32,
      comp_size: compSize,
      uncomp_size: uncompSize,
      file_name_length: fileNameLength,
      extra_field_length: extraFieldLength,
      file_comment_length: fileCommentLength,
      disk_num_start: diskNumStart,
      internal_attr: internalAttr,
      external_attr: externalAttr,
      local_header_offset: localHeaderOffset
    )
  end

  defp cd_file_header_from_bin(_) do
    throw(:bad_cd_file_header)
  end

  defp local_file_header_from_bin(
         <<versionNeeded::size(16)-little, gPFlag::size(16)-little, compMethod::size(16)-little,
           lastModTime::size(16)-little, lastModDate::size(16)-little, cRC32::size(32)-little,
           compSize::size(32)-little, uncompSize::size(32)-little,
           fileNameLength::size(16)-little, extraFieldLength::size(16)-little>>,
         _F
       ) do
    r_local_file_header(
      version_needed: versionNeeded,
      gp_flag: gPFlag,
      comp_method: compMethod,
      last_mod_time: lastModTime,
      last_mod_date: lastModDate,
      crc32: cRC32,
      comp_size: compSize,
      uncomp_size: uncompSize,
      file_name_length: fileNameLength,
      extra_field_length: extraFieldLength
    )
  end

  defp local_file_header_from_bin(_, f) do
    throw({:bad_local_file_header, f})
  end

  defp split_iolist(b, pos) when is_binary(b) do
    :erlang.split_binary(b, pos)
  end

  defp split_iolist(l, pos) when is_list(l) do
    splitter([], l, pos)
  end

  def splitter(left, right, 0) do
    {left, right}
  end

  def splitter(<<>>, right, relPos) do
    split_iolist(right, relPos)
  end

  def splitter(left, [a | right], relPos)
      when is_list(a) or is_binary(a) do
    sz = :erlang.iolist_size(a)

    case sz > relPos do
      true ->
        {leftx, rightx} = split_iolist(a, relPos)
        {[left | leftx], [rightx, right]}

      _ ->
        splitter([left | a], right, relPos - sz)
    end
  end

  def splitter(left, [a | right], relPos) when is_integer(a) do
    splitter([left, a], right, relPos - 1)
  end

  def splitter(left, right, relPos) when is_binary(right) do
    splitter(left, [right], relPos)
  end

  defp skip_iolist(b, pos) when is_binary(b) do
    case b do
      <<_::size(pos)-binary, bin::binary>> ->
        bin

      _ ->
        <<>>
    end
  end

  defp skip_iolist(l, pos) when is_list(l) do
    skipper(l, pos)
  end

  defp skipper(right, 0) do
    right
  end

  defp skipper([a | right], relPos)
       when is_list(a) or is_binary(a) do
    sz = :erlang.iolist_size(a)

    case sz > relPos do
      true ->
        rightx = skip_iolist(a, relPos)
        [rightx, right]

      _ ->
        skip_iolist(right, relPos - sz)
    end
  end

  defp skipper([a | right], relPos) when is_integer(a) do
    skip_iolist(right, relPos - 1)
  end

  defp pwrite_iolist(iolist, pos, bin) do
    {left, right} = split_iolist(iolist, pos)
    sz = :erlang.iolist_size(bin)
    r = skip_iolist(right, sz)
    [[left, bin] | r]
  end

  defp pwrite_binary(b, pos, bin) do
    :erlang.iolist_to_binary(pwrite_iolist(b, pos, bin))
  end

  defp reverse(x) do
    reverse(x, [])
  end

  defp reverse([h | t], y) do
    reverse(t, [h | y])
  end

  defp reverse([], x) do
    x
  end

  defp last([e | es]) do
    last(e, es)
  end

  defp last(_, [e | es]) do
    last(e, es)
  end

  defp last(e, []) do
    e
  end
end
