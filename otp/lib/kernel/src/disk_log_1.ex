defmodule :m_disk_log_1 do
  use Bitwise
  import :lists, only: [concat: 1, reverse: 1, sum: 1]
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

  Record.defrecord(:r_arg, :arg,
    name: 0,
    version: :undefined,
    file: :none,
    repair: true,
    size: :infinity,
    type: :halt,
    format: :internal,
    linkto: self(),
    head: :none,
    mode: :read_write,
    notify: false,
    quiet: false,
    options: []
  )

  Record.defrecord(:r_cache, :cache, fd: :undefined, sz: 0, c: [])
  Record.defrecord(:r_halt, :halt, fdc: :undefined, curB: :undefined, size: :undefined)

  Record.defrecord(:r_handle, :handle,
    filename: :undefined,
    maxB: :undefined,
    maxF: :undefined,
    curB: :undefined,
    curF: :undefined,
    cur_fdc: :undefined,
    cur_name: :undefined,
    cur_cnt: :undefined,
    acc_cnt: :undefined,
    firstPos: :undefined,
    noFull: :undefined,
    accFull: :undefined
  )

  Record.defrecord(:r_log, :log,
    status: :ok,
    name: :undefined,
    blocked_by: :none,
    users: 0,
    filename: :undefined,
    owners: [],
    type: :undefined,
    format: :undefined,
    format_type: :undefined,
    head: :none,
    mode: :undefined,
    size: :undefined,
    extra: :undefined,
    version: :undefined
  )

  Record.defrecord(:r_continuation, :continuation, pid: self(), pos: :undefined, b: :undefined)

  defp log(fdC, fileName, x) do
    {bs, size} = logl(x, [], 0)

    case fwrite(fdC, fileName, bs, size) do
      {:ok, newFdC} ->
        {:ok, size, newFdC}

      error ->
        error
    end
  end

  def logl(x) do
    logl(x, [], 0)
  end

  defp logl([x | t], bs, size) do
    sz = byte_size(x)
    bSz = <<sz::size(4)-unit(8)>>

    nBs =
      case sz < 65528 do
        true ->
          [bs, bSz, <<98, 87, 76, 65>> | x]

        false ->
          mD5 = :erlang.md5(bSz)
          [bs, bSz, <<98, 87, 76, 65>>, mD5 | x]
      end

    logl(t, nBs, size + 8 + sz)
  end

  defp logl([], bs, size) do
    {bs, size}
  end

  def write_cache(r_cache(fd: fd, c: c), fName) do
    :erlang.erase(:write_cache_timer_is_running)
    write_cache(fd, fName, c)
  end

  def sync(fdC, fName) do
    fsync(fdC, fName)
  end

  def truncate(fdC, fileName, head) do
    reply = truncate_at(fdC, fileName, 8)

    case reply do
      {:ok, _} when head === :none ->
        reply

      {:ok, fdC1} ->
        {:ok, b} = head

        case log(fdC1, fileName, [b]) do
          {:ok, _NoBytes, newFdC} ->
            {:ok, newFdC}

          reply2 ->
            reply2
        end

      _ ->
        reply
    end
  end

  def chunk(fdC, fileName, pos, b, n) when is_binary(b) do
    true = byte_size(b) >= 8
    do_handle_chunk(fdC, fileName, pos, b, n)
  end

  def chunk(fdC, fileName, pos, noBytes, n) do
    maxNoBytes =
      case noBytes do
        [] ->
          65536

        _ ->
          :erlang.max(noBytes, 65536)
      end

    case read_chunk(fdC, fileName, pos, maxNoBytes) do
      {newFdC, {:ok, bin}} when byte_size(bin) < 8 ->
        {newFdC, {:error, {:corrupt_log_file, fileName}}}

      {newFdC, {:ok, bin}}
      when noBytes === [] or
             byte_size(bin) >= noBytes ->
        newPos = pos + byte_size(bin)
        do_handle_chunk(newFdC, fileName, newPos, bin, n)

      {newFdC, {:ok, _Bin}} ->
        {newFdC, {:error, {:corrupt_log_file, fileName}}}

      {newFdC, :eof} when is_integer(noBytes) ->
        {newFdC, {:error, {:corrupt_log_file, fileName}}}

      other ->
        other
    end
  end

  defp do_handle_chunk(fdC, fileName, pos, b, n) do
    case handle_chunk(b, pos, n, []) do
      :corrupt ->
        {fdC, {:error, {:corrupt_log_file, fileName}}}

      {c, []} ->
        chunk(fdC, fileName, r_continuation(c, :pos), r_continuation(c, :b), n)

      c_Ack ->
        {fdC, c_Ack}
    end
  end

  defp handle_chunk(b, pos, 0, ack) when byte_size(b) >= 8 do
    {r_continuation(pos: pos, b: b), ack}
  end

  defp handle_chunk(
         b = <<size::size(4)-unit(8), 1_649_888_321::size(4)-unit(8), tail::binary>>,
         pos,
         n,
         ack
       )
       when size < 65528 do
    case tail do
      <<binTerm::size(size)-binary, tail2::binary>> ->
        handle_chunk(tail2, pos, n - 1, [binTerm | ack])

      _ ->
        bytesToRead = size + 8
        {r_continuation(pos: pos - byte_size(b), b: bytesToRead), ack}
    end
  end

  defp handle_chunk(
         b = <<size::size(4)-unit(8), 1_649_888_321::size(4)-unit(8), tail::binary>>,
         pos,
         _N,
         ack
       ) do
    mD5 = :erlang.md5(<<size::size(4)-unit(8)>>)

    case tail do
      <<^mD5::size(16)-binary, bin::size(size)-binary>> ->
        {r_continuation(pos: pos, b: []), [bin | ack]}

      <<^mD5::size(16)-binary, _::binary>> ->
        bytesToRead = size + 8 + 16
        {r_continuation(pos: pos - byte_size(b), b: bytesToRead), ack}

      _ when byte_size(tail) >= 16 ->
        :corrupt

      _ ->
        {r_continuation(pos: pos - byte_size(b), b: []), ack}
    end
  end

  defp handle_chunk(
         b = <<size::size(4)-unit(8), 203_500_599::size(4)-unit(8), tail::binary>>,
         pos,
         n,
         ack
       ) do
    case tail do
      <<binTerm::size(size)-binary, tail2::binary>> ->
        handle_chunk(tail2, pos, n - 1, [binTerm | ack])

      _ ->
        bytesToRead = size + 8
        {r_continuation(pos: pos - byte_size(b), b: bytesToRead), ack}
    end
  end

  defp handle_chunk(b, _Pos, _N, _Ack) when byte_size(b) >= 8 do
    :corrupt
  end

  defp handle_chunk(b, pos, _N, ack) do
    {r_continuation(pos: pos - byte_size(b), b: []), ack}
  end

  defp read_chunk(fdC, fileName, pos, maxBytes) do
    {fdC1, r} = pread(fdC, fileName, pos + 8, maxBytes)

    case position(fdC1, fileName, :eof) do
      {:ok, newFdC, _Pos} ->
        {newFdC, r}

      {error, newFdC} ->
        {newFdC, error}
    end
  end

  def chunk_read_only(fdC = r_cache(), fileName, pos, b, n) do
    do_chunk_read_only(fdC, fileName, pos, b, n)
  end

  def chunk_read_only(fd, fileName, pos, b, n) do
    fdC = r_cache(fd: fd)
    {_NFdC, reply} = do_chunk_read_only(fdC, fileName, pos, b, n)
    reply
  end

  defp do_chunk_read_only(fdC, fileName, pos, b, n) when is_binary(b) do
    true = byte_size(b) >= 8
    do_handle_chunk_ro(fdC, fileName, pos, b, n)
  end

  defp do_chunk_read_only(fdC, fileName, pos, noBytes, n) do
    maxNoBytes =
      case noBytes do
        [] ->
          65536

        _ ->
          :erlang.max(noBytes, 65536)
      end

    case read_chunk_ro(fdC, fileName, pos, maxNoBytes) do
      {newFdC, {:ok, bin}} when byte_size(bin) < 8 ->
        newCont = r_continuation(pos: pos + byte_size(bin), b: [])
        {newFdC, {newCont, [], byte_size(bin)}}

      {newFdC, {:ok, bin}}
      when noBytes === [] or
             byte_size(bin) >= noBytes ->
        newPos = pos + byte_size(bin)
        do_handle_chunk_ro(newFdC, fileName, newPos, bin, n)

      {newFdC, {:ok, bin}} ->
        newCont = r_continuation(pos: pos + byte_size(bin), b: [])
        {newFdC, {newCont, [], byte_size(bin) - 8}}

      {newFdC, :eof} when is_integer(noBytes) ->
        {newFdC, :eof}

      other ->
        other
    end
  end

  defp do_handle_chunk_ro(fdC, fileName, pos, b, n) do
    case handle_chunk_ro(b, pos, n, [], 0) do
      {c, [], 0} ->
        r_continuation(pos: newPos, b: noBytes) = c
        do_chunk_read_only(fdC, fileName, newPos, noBytes, n)

      c_Ack_Bad ->
        {fdC, c_Ack_Bad}
    end
  end

  defp handle_chunk_ro(b, pos, 0, ack, bad) when byte_size(b) >= 8 do
    {r_continuation(pos: pos, b: b), ack, bad}
  end

  defp handle_chunk_ro(
         b = <<size::size(4)-unit(8), 1_649_888_321::size(4)-unit(8), tail::binary>>,
         pos,
         n,
         ack,
         bad
       )
       when size < 65528 do
    case tail do
      <<binTerm::size(size)-binary, tail2::binary>> ->
        handle_chunk_ro(tail2, pos, n - 1, [binTerm | ack], bad)

      _ ->
        bytesToRead = size + 8
        {r_continuation(pos: pos - byte_size(b), b: bytesToRead), ack, bad}
    end
  end

  defp handle_chunk_ro(
         b = <<size::size(4)-unit(8), 1_649_888_321::size(4)-unit(8), tail::binary>>,
         pos,
         n,
         ack,
         bad
       ) do
    mD5 = :erlang.md5(<<size::size(4)-unit(8)>>)

    case tail do
      <<^mD5::size(16)-binary, bin::size(size)-binary>> ->
        {r_continuation(pos: pos, b: []), [bin | ack], bad}

      <<^mD5::size(16)-binary, _::binary>> ->
        bytesToRead = size + 8 + 16
        {r_continuation(pos: pos - byte_size(b), b: bytesToRead), ack, bad}

      <<_BadMD5::size(16)-binary, _::size(1)-unit(8), tail2::binary>> ->
        handle_chunk_ro(tail2, pos, n - 1, ack, bad + 1)

      _ ->
        {r_continuation(pos: pos - byte_size(b), b: []), ack, bad}
    end
  end

  defp handle_chunk_ro(
         b = <<size::size(4)-unit(8), 203_500_599::size(4)-unit(8), tail::binary>>,
         pos,
         n,
         ack,
         bad
       ) do
    case tail do
      <<binTerm::size(size)-binary, tail2::binary>> ->
        handle_chunk_ro(tail2, pos, n - 1, [binTerm | ack], bad)

      _ ->
        bytesToRead = size + 8
        {r_continuation(pos: pos - byte_size(b), b: bytesToRead), ack, bad}
    end
  end

  defp handle_chunk_ro(b, pos, n, ack, bad) when byte_size(b) >= 8 do
    <<_::size(1)-unit(8), b2::binary>> = b
    handle_chunk_ro(b2, pos, n - 1, ack, bad + 1)
  end

  defp handle_chunk_ro(b, pos, _N, ack, bad) do
    {r_continuation(pos: pos - byte_size(b), b: []), ack, bad}
  end

  defp read_chunk_ro(fdC, fileName, pos, maxBytes) do
    pread(fdC, fileName, pos + 8, maxBytes)
  end

  def close(r_cache(fd: fd, c: []), fileName, :read_only) do
    case :file.close(fd) do
      :ok ->
        :ok

      error ->
        file_error(fileName, error)
    end
  end

  def close(r_cache(fd: fd, c: c), fileName, :read_write) do
    {reply, _NewFdC} = write_cache(fd, fileName, c)
    mark(fd, fileName, <<99, 88, 77, 11>>)

    case :file.close(fd) do
      :ok ->
        :ok

      error ->
        file_error(fileName, error)
    end

    cond do
      reply === :ok ->
        :ok

      true ->
        throw(reply)
    end
  end

  def int_open(fName, :truncate, :read_write, head) do
    new_int_file(fName, head)
  end

  def int_open(fName, repair, :read_write, head) do
    case open_read(fName) do
      {:ok, fd} ->
        case :file.read(fd, 8) do
          {:ok, fileHead} ->
            case is_head(fileHead) do
              :yes ->
                case :file.close(fd) do
                  :ok ->
                    :ok

                  error2 ->
                    file_error(fName, error2)
                end

                case open_update(fName) do
                  {:ok, fd2} ->
                    mark(fd2, fName, <<6, 7, 8, 9>>)
                    fdC1 = r_cache(fd: fd2)
                    {fdC, p} = position_close(fdC1, fName, :eof)
                    {:ok, {:existed, fdC, {0, 0}, p}}

                  error ->
                    file_error(fName, error)
                end

              :yes_not_closed when repair ->
                repair(fd, fName)

              :yes_not_closed when not repair ->
                _ = :file.close(fd)
                throw({:error, {:need_repair, fName}})

              :no ->
                _ = :file.close(fd)
                throw({:error, {:not_a_log_file, fName}})
            end

          :eof ->
            _ = :file.close(fd)
            throw({:error, {:not_a_log_file, fName}})

          error ->
            file_error_close(fd, fName, error)
        end

      _Other ->
        new_int_file(fName, head)
    end
  end

  def int_open(fName, _Repair, :read_only, _Head) do
    case open_read(fName) do
      {:ok, fd} ->
        case :file.read(fd, 8) do
          {:ok, head} ->
            case is_head(head) do
              :yes ->
                {:ok, p} = position_close2(fd, fName, :eof)
                fdC = r_cache(fd: fd)
                {:ok, {:existed, fdC, {0, 0}, p}}

              :yes_not_closed ->
                {:ok, p} = position_close2(fd, fName, :eof)
                fdC = r_cache(fd: fd)
                {:ok, {:existed, fdC, {0, 0}, p}}

              :no ->
                _ = :file.close(fd)
                throw({:error, {:not_a_log_file, fName}})
            end

          :eof ->
            _ = :file.close(fd)
            throw({:error, {:not_a_log_file, fName}})

          error ->
            file_error_close(fd, fName, error)
        end

      error ->
        file_error(fName, error)
    end
  end

  defp new_int_file(fName, head) do
    case open_update(fName) do
      {:ok, fd} ->
        :ok = truncate_at_close2(fd, fName, :bof)
        fwrite_close2(fd, fName, [<<1, 2, 3, 4>>, <<6, 7, 8, 9>>])
        {fdC1, nh, headSz} = int_log_head(fd, head)
        {fdC, fileSize} = position_close(fdC1, fName, :cur)
        {:ok, {:new, fdC, {nh, 8 + headSz}, fileSize}}

      error ->
        file_error(fName, error)
    end
  end

  defp int_log_head(fd, head) do
    case lh(head, :internal) do
      {:ok, binHead} ->
        {bs, size} = logl([binHead])
        {:ok, fdC} = fwrite_header(fd, bs, size)
        {fdC, 1, size}

      :none ->
        {r_cache(fd: fd), 0, 0}

      error ->
        _ = :file.close(fd)
        throw(error)
    end
  end

  def ext_open(fName, :truncate, :read_write, head) do
    new_ext_file(fName, head)
  end

  def ext_open(fName, _Repair, :read_write, head) do
    case :file.read_file_info(fName) do
      {:ok, _FileInfo} ->
        case open_update(fName) do
          {:ok, fd} ->
            {:ok, p} = position_close2(fd, fName, :eof)
            fdC = r_cache(fd: fd)
            {:ok, {:existed, fdC, {0, 0}, p}}

          error ->
            file_error(fName, error)
        end

      _Other ->
        new_ext_file(fName, head)
    end
  end

  def ext_open(fName, _Repair, :read_only, _Head) do
    case open_read(fName) do
      {:ok, fd} ->
        {:ok, p} = position_close2(fd, fName, :eof)
        fdC = r_cache(fd: fd)
        {:ok, {:existed, fdC, {0, 0}, p}}

      error ->
        file_error(fName, error)
    end
  end

  defp new_ext_file(fName, head) do
    case open_truncate(fName) do
      {:ok, fd} ->
        {fdC1, headSize} = ext_log_head(fd, head)
        {fdC, fileSize} = position_close(fdC1, fName, :cur)
        {:ok, {:new, fdC, headSize, fileSize}}

      error ->
        file_error(fName, error)
    end
  end

  defp ext_log_head(fd, head) do
    case lh(head, :external) do
      {:ok, binHead} ->
        size = byte_size(binHead)
        {:ok, fdC} = fwrite_header(fd, binHead, size)
        {fdC, {1, size}}

      :none ->
        {r_cache(fd: fd), {0, 0}}

      error ->
        _ = :file.close(fd)
        throw(error)
    end
  end

  defp mark(fd, fileName, what) do
    {:ok, _} = position_close2(fd, fileName, 4)
    fwrite_close2(fd, fileName, what)
  end

  defp lh({:ok, bin}, _Format) do
    {:ok, bin}
  end

  defp lh({m, f, a}, format) when is_list(a) do
    case (try do
            apply(m, f, a)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, head} when format === :internal ->
        {:ok, :erlang.term_to_binary(head)}

      {:ok, bin} when is_binary(bin) ->
        {:ok, bin}

      {:ok, bytes} ->
        case (try do
                :erlang.list_to_binary(bytes)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            {:error, {:invalid_header, {{m, f, a}, {:ok, bytes}}}}

          bin ->
            {:ok, bin}
        end

      {:EXIT, error} ->
        {:error, {:invalid_header, {{m, f, a}, error}}}

      error ->
        {:error, {:invalid_header, {{m, f, a}, error}}}
    end
  end

  defp lh({m, f, a}, _Format) do
    {:error, {:invalid_header, {m, f, a}}}
  end

  defp lh(:none, _Format) do
    :none
  end

  defp lh(h, _F) do
    {:error, {:invalid_header, h}}
  end

  defp repair(in__, file) do
    fSz = file_size(file)

    case is_quiet() do
      true ->
        :ok

      _ ->
        :error_logger.info_msg('disk_log: repairing ~tp ...\n', [file])
    end

    tmp = add_ext(file, 'TMP')

    {:ok, {_Alloc, out, {0, _}, _FileSize}} =
      new_int_file(
        tmp,
        :none
      )

    scan_f_read(<<>>, in__, out, file, fSz, tmp, 65536, 0, 0)
  end

  defp scan_f_read(b, in__, out, file, fSz, tmp, maxBytes, no, bad) do
    case :file.read(in__, maxBytes) do
      :eof ->
        done_scan(in__, out, tmp, file, no, bad + byte_size(b))

      {:ok, bin} ->
        newBin = :erlang.list_to_binary([b, bin])
        {nB, nMax, ack, nNo, nBad} = scan_f(newBin, fSz, [], no, bad)

        case log(out, tmp, :lists.reverse(ack)) do
          {:ok, _Size, newOut} ->
            scan_f_read(nB, in__, newOut, file, fSz, tmp, nMax, nNo, nBad)

          {{:error, {:file_error, _Filename, error}}, newOut} ->
            repair_err(in__, newOut, tmp, file, {:error, error})
        end

      error ->
        repair_err(in__, out, tmp, file, error)
    end
  end

  defp scan_f(
         b = <<size::size(4)-unit(8), 1_649_888_321::size(4)-unit(8), tail::binary>>,
         fSz,
         ack,
         no,
         bad
       )
       when size < 65528 do
    scan_f2(b, fSz, ack, no, bad, size, tail)
  end

  defp scan_f(
         b = <<size::size(4)-unit(8), 1_649_888_321::size(4)-unit(8), tail::binary>>,
         fSz,
         ack,
         no,
         bad
       ) do
    mD5 = :erlang.md5(<<size::size(4)-unit(8)>>)

    case tail do
      <<^mD5::size(16)-binary, binTerm::size(size)-binary, tail2::binary>> ->
        case (try do
                :erlang.binary_to_term(binTerm)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            scan_f(tail2, fSz, ack, no, bad + size)

          _Term ->
            scan_f(tail2, fSz, [binTerm | ack], no + 1, bad)
        end

      <<^mD5::size(16)-binary, _::binary>> ->
        {b, size - byte_size(tail) + 16, ack, no, bad}

      _ when byte_size(tail) < 16 ->
        {b, size - byte_size(tail) + 16, ack, no, bad}

      _ ->
        <<_::size(8), b2::binary>> = b
        scan_f(b2, fSz, ack, no, bad + 1)
    end
  end

  defp scan_f(
         b = <<size::size(4)-unit(8), 203_500_599::size(4)-unit(8), tail::binary>>,
         fSz,
         ack,
         no,
         bad
       )
       when size <= fSz do
    scan_f2(b, fSz, ack, no, bad, size, tail)
  end

  defp scan_f(b = <<_::size(8)-unit(8), _::binary>>, fSz, ack, no, bad) do
    <<_::size(8), b2::binary>> = b
    scan_f(b2, fSz, ack, no, bad + 1)
  end

  defp scan_f(b, _FSz, ack, no, bad) do
    {b, 65536, ack, no, bad}
  end

  defp scan_f2(b, fSz, ack, no, bad, size, tail) do
    case tail do
      <<binTerm::size(size)-binary, tail2::binary>> ->
        case (try do
                :erlang.binary_to_term(binTerm)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            <<_::size(8), b2::binary>> = b
            scan_f(b2, fSz, ack, no, bad + 1)

          _Term ->
            scan_f(tail2, fSz, [binTerm | ack], no + 1, bad)
        end

      _ ->
        {b, size - byte_size(tail), ack, no, bad}
    end
  end

  defp done_scan(in__, out, outName, fName, recoveredTerms, badChars) do
    _ = :file.close(in__)

    case (try do
            fclose(out, outName)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :ok ->
        case :file.rename(outName, fName) do
          :ok ->
            case open_update(fName) do
              {:ok, new} ->
                {:ok, p} = position_close2(new, fName, :eof)
                fdC = r_cache(fd: new)
                {:repaired, fdC, recoveredTerms, badChars, p}

              error ->
                file_error(fName, error)
            end

          error ->
            _ = :file.delete(outName)
            file_error(fName, error)
        end

      error ->
        _ = :file.delete(outName)
        throw(error)
    end
  end

  defp repair_err(in__, out, outName, errFileName, error) do
    _ = :file.close(in__)

    try do
      fclose(out, outName)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    _ = :file.delete(outName)
    file_error(errFileName, error)
  end

  def is_head(<<m::size(4)-binary, s::size(4)-binary>>)
      when <<1, 2, 3, 4>> === m and
             <<99, 88, 77, 11>> === s do
    :yes
  end

  def is_head(<<m::size(4)-binary, s::size(4)-binary>>)
      when <<1, 2, 3, 4>> === m and <<6, 7, 8, 9>> === s do
    :yes_not_closed
  end

  def is_head(bin) when is_binary(bin) do
    :no
  end

  def mf_int_open(fName, maxB, maxF, repair, mode, head, version) do
    {first, sz, totSz, nFiles} = read_index_file(repair, fName, maxF)
    write_size_file(mode, fName, maxB, maxF, version)

    newMaxF =
      cond do
        nFiles > maxF ->
          {maxF, nFiles}

        true ->
          maxF
      end

    case int_file_open(fName, first, 0, 0, head, repair, mode) do
      {:ok, fdC, fileName, lost, {noItems, noBytes}, fSz} ->
        curCnt = sz + noItems - lost

        {:ok,
         r_handle(
           filename: fName,
           maxB: maxB,
           maxF: newMaxF,
           curF: first,
           cur_fdc: fdC,
           cur_name: fileName,
           cur_cnt: curCnt,
           acc_cnt: -sz,
           curB: fSz,
           firstPos: noBytes,
           noFull: 0,
           accFull: 0
         ), totSz + curCnt}

      {:repaired, fdC, fileName, rec, bad, fSz} ->
        {:repaired,
         r_handle(
           filename: fName,
           maxB: maxB,
           cur_name: fileName,
           maxF: newMaxF,
           curF: first,
           cur_fdc: fdC,
           cur_cnt: rec,
           acc_cnt: -rec,
           curB: fSz,
           firstPos: 0,
           noFull: 0,
           accFull: 0
         ), rec, bad, totSz + rec}
    end
  end

  def mf_int_inc(handle, head) do
    r_handle(
      filename: fName,
      cur_cnt: curCnt,
      acc_cnt: accCnt,
      cur_name: fileName,
      curF: curF,
      maxF: maxF,
      cur_fdc: curFdC,
      noFull: noFull
    ) = handle

    case (try do
            wrap_int_log(fName, curF, maxF, curCnt, head)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {newF, newMaxF, newFdC, newFileName, nh, firstPos, lost} ->
        handle1 =
          r_handle(handle,
            cur_fdc: newFdC,
            curF: newF,
            cur_name: newFileName,
            cur_cnt: nh,
            acc_cnt: accCnt + curCnt,
            maxF: newMaxF,
            firstPos: firstPos,
            curB: firstPos,
            noFull: noFull + 1
          )

        case (try do
                close(curFdC, fileName, :read_write)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          :ok ->
            {:ok, handle1, lost}

          error ->
            {:error, error, handle1}
        end

      error ->
        {:error, error, handle}
    end
  end

  def mf_int_log(handle, bins, head) do
    mf_int_log(handle, bins, head, 0, [])
  end

  defp mf_int_log(handle, [], _Head, no, []) do
    {:ok, handle, no}
  end

  defp mf_int_log(handle, [], _Head, no, wraps0) do
    wraps = reverse(wraps0)
    {:ok, handle, no, sum(wraps), wraps}
  end

  defp mf_int_log(handle, bins, head, no0, wraps) do
    r_handle(
      curB: curB,
      maxB: maxB,
      cur_name: fileName,
      cur_fdc: curFdC,
      firstPos: firstPos0,
      cur_cnt: curCnt
    ) = handle

    {firstBins, lastBins, noBytes, n} = int_split_bins(curB, maxB, firstPos0, bins)

    case firstBins do
      [] ->
        r_handle(filename: fName, curF: curF, maxF: maxF, acc_cnt: accCnt, noFull: noFull) =
          handle

        case (try do
                wrap_int_log(fName, curF, maxF, curCnt, head)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {newF, newMaxF, newFdC, newFileName, nh, firstPos, lost} ->
            handle1 =
              r_handle(handle,
                cur_fdc: newFdC,
                curF: newF,
                cur_cnt: nh,
                cur_name: newFileName,
                acc_cnt: accCnt + curCnt,
                maxF: newMaxF,
                curB: firstPos,
                firstPos: firstPos,
                noFull: noFull + 1
              )

            case (try do
                    close(curFdC, fileName, :read_write)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              :ok ->
                mf_int_log(handle1, bins, head, no0 + nh, [lost | wraps])

              error ->
                lost1 = lost + sum(wraps)
                {:error, error, handle1, no0 + nh, lost1}
            end

          error ->
            {:error, error, handle, no0, sum(wraps)}
        end

      _ ->
        case fwrite(curFdC, fileName, firstBins, noBytes) do
          {:ok, newCurFdC} ->
            handle1 =
              r_handle(handle, cur_fdc: newCurFdC, curB: curB + noBytes, cur_cnt: curCnt + n)

            mf_int_log(handle1, lastBins, head, no0 + n, wraps)

          {error, newCurFdC} ->
            handle1 = r_handle(handle, cur_fdc: newCurFdC)
            {:error, error, handle1, no0, sum(wraps)}
        end
    end
  end

  defp wrap_int_log(fName, curF, maxF, curCnt, head) do
    {newF, newMaxF} = inc_wrap(fName, curF, maxF)

    {:ok, newFdC, newFileName, lost, {nh, firstPos}, _FileSize} =
      int_file_open(fName, newF, curF, curCnt, head)

    {newF, newMaxF, newFdC, newFileName, nh, firstPos, lost}
  end

  def mf_int_chunk(handle, 0, bin, n) do
    firstF = find_first_file(handle)
    mf_int_chunk(handle, {firstF, 0}, bin, n)
  end

  def mf_int_chunk(
        r_handle(curF: fileNo, cur_fdc: fdC, cur_name: fileName) = handle,
        {fileNo, pos},
        bin,
        n
      ) do
    {newFdC, reply} = chunk(fdC, fileName, pos, bin, n)
    {r_handle(handle, cur_fdc: newFdC), conv(reply, fileNo)}
  end

  def mf_int_chunk(handle, {fileNo, pos}, bin, n) do
    fName = add_ext(r_handle(handle, :filename), fileNo)
    nFileNo = inc(fileNo, r_handle(handle, :maxF))

    case (try do
            int_open(fName, true, :read_only, :any)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, _Reason} ->
        case is_quiet() do
          true ->
            :ok

          _ ->
            :error_logger.info_msg('disk_log: chunk error. File ~tp missing.\n\n', [fName])
        end

        mf_int_chunk(handle, {nFileNo, 0}, [], n)

      {:ok, {_Alloc, fdC, _HeadSize, _FileSize}} ->
        case chunk(fdC, fName, pos, bin, n) do
          {newFdC, :eof} ->
            _ = :file.close(r_cache(newFdC, :fd))
            mf_int_chunk(handle, {nFileNo, 0}, [], n)

          {newFdC, other} ->
            _ = :file.close(r_cache(newFdC, :fd))
            {handle, conv(other, fileNo)}
        end
    end
  end

  def mf_int_chunk_read_only(handle, 0, bin, n) do
    firstF = find_first_file(handle)
    mf_int_chunk_read_only(handle, {firstF, 0}, bin, n)
  end

  def mf_int_chunk_read_only(
        r_handle(curF: fileNo, cur_fdc: fdC, cur_name: fileName) = handle,
        {fileNo, pos},
        bin,
        n
      ) do
    {newFdC, reply} = do_chunk_read_only(fdC, fileName, pos, bin, n)
    {r_handle(handle, cur_fdc: newFdC), conv(reply, fileNo)}
  end

  def mf_int_chunk_read_only(handle, {fileNo, pos}, bin, n) do
    fName = add_ext(r_handle(handle, :filename), fileNo)
    nFileNo = inc(fileNo, r_handle(handle, :maxF))

    case (try do
            int_open(fName, true, :read_only, :any)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, _Reason} ->
        case is_quiet() do
          true ->
            :ok

          _ ->
            :error_logger.info_msg('disk_log: chunk error. File ~tp missing.\n\n', [fName])
        end

        mf_int_chunk_read_only(handle, {nFileNo, 0}, [], n)

      {:ok, {_Alloc, fdC, _HeadSize, _FileSize}} ->
        case do_chunk_read_only(fdC, fName, pos, bin, n) do
          {newFdC, :eof} ->
            _ = :file.close(r_cache(newFdC, :fd))
            mf_int_chunk_read_only(handle, {nFileNo, 0}, [], n)

          {newFdC, other} ->
            _ = :file.close(r_cache(newFdC, :fd))
            {handle, conv(other, fileNo)}
        end
    end
  end

  def mf_int_chunk_step(handle, 0, step) do
    firstF = find_first_file(handle)
    mf_int_chunk_step(handle, {firstF, 0}, step)
  end

  def mf_int_chunk_step(handle, {fileNo, _Pos}, step) do
    nFileNo = inc(fileNo, r_handle(handle, :maxF), step)
    fileName = add_ext(r_handle(handle, :filename), nFileNo)

    case :file.read_file_info(fileName) do
      {:ok, _FileInfo} ->
        {:ok, r_continuation(pos: {nFileNo, 0}, b: [])}

      _Error ->
        {:error, :end_of_log}
    end
  end

  def mf_write_cache(r_handle(filename: fName, cur_fdc: fdC) = handle) do
    :erlang.erase(:write_cache_timer_is_running)
    r_cache(fd: fd, c: c) = fdC
    {reply, newFdC} = write_cache(fd, fName, c)
    {reply, r_handle(handle, cur_fdc: newFdC)}
  end

  def mf_sync(r_handle(filename: fName, cur_fdc: fdC) = handle) do
    {reply, newFdC} = fsync(fdC, fName)
    {reply, r_handle(handle, cur_fdc: newFdC)}
  end

  def mf_int_close(
        r_handle(
          filename: fName,
          curF: curF,
          cur_name: fileName,
          cur_fdc: curFdC,
          cur_cnt: curCnt
        ),
        mode
      ) do
    close(curFdC, fileName, mode)
    write_index_file(mode, fName, curF, curF, curCnt)
    :ok
  end

  def mf_ext_open(fName, maxB, maxF, repair, mode, head, version) do
    {first, sz, totSz, nFiles} = read_index_file(repair, fName, maxF)
    write_size_file(mode, fName, maxB, maxF, version)

    newMaxF =
      cond do
        nFiles > maxF ->
          {maxF, nFiles}

        true ->
          maxF
      end

    {:ok, fdC, fileName, lost, {noItems, noBytes}, curB} =
      ext_file_open(fName, first, 0, 0, head, repair, mode)

    curCnt = sz + noItems - lost

    {:ok,
     r_handle(
       filename: fName,
       maxB: maxB,
       cur_name: fileName,
       maxF: newMaxF,
       cur_cnt: curCnt,
       acc_cnt: -sz,
       curF: first,
       cur_fdc: fdC,
       firstPos: noBytes,
       curB: curB,
       noFull: 0,
       accFull: 0
     ), totSz + curCnt}
  end

  def mf_ext_inc(handle, head) do
    r_handle(
      filename: fName,
      cur_cnt: curCnt,
      cur_name: fileName,
      acc_cnt: accCnt,
      curF: curF,
      maxF: maxF,
      cur_fdc: curFdC,
      noFull: noFull
    ) = handle

    case (try do
            wrap_ext_log(fName, curF, maxF, curCnt, head)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {newF, newMaxF, newFdC, newFileName, nh, firstPos, lost} ->
        handle1 =
          r_handle(handle,
            cur_fdc: newFdC,
            curF: newF,
            cur_name: newFileName,
            cur_cnt: nh,
            acc_cnt: accCnt + curCnt,
            maxF: newMaxF,
            firstPos: firstPos,
            curB: firstPos,
            noFull: noFull + 1
          )

        case (try do
                fclose(curFdC, fileName)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          :ok ->
            {:ok, handle1, lost}

          error ->
            {:error, error, handle1}
        end

      error ->
        {:error, error, handle}
    end
  end

  def mf_ext_log(handle, bins, head) do
    mf_ext_log(handle, bins, head, 0, [])
  end

  defp mf_ext_log(handle, [], _Head, no, []) do
    {:ok, handle, no}
  end

  defp mf_ext_log(handle, [], _Head, no, wraps0) do
    wraps = reverse(wraps0)
    {:ok, handle, no, sum(wraps), wraps}
  end

  defp mf_ext_log(handle, bins, head, no0, wraps) do
    r_handle(
      curB: curB,
      maxB: maxB,
      cur_name: fileName,
      cur_fdc: curFdC,
      firstPos: firstPos0,
      cur_cnt: curCnt
    ) = handle

    {firstBins, lastBins, noBytes, n} = ext_split_bins(curB, maxB, firstPos0, bins)

    case firstBins do
      [] ->
        r_handle(filename: fName, curF: curF, maxF: maxF, acc_cnt: accCnt, noFull: noFull) =
          handle

        case (try do
                wrap_ext_log(fName, curF, maxF, curCnt, head)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {newF, newMaxF, newFdC, newFileName, nh, firstPos, lost} ->
            handle1 =
              r_handle(handle,
                cur_fdc: newFdC,
                curF: newF,
                cur_cnt: nh,
                cur_name: newFileName,
                acc_cnt: accCnt + curCnt,
                maxF: newMaxF,
                curB: firstPos,
                firstPos: firstPos,
                noFull: noFull + 1
              )

            case (try do
                    fclose(curFdC, fileName)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              :ok ->
                mf_ext_log(handle1, bins, head, no0 + nh, [lost | wraps])

              error ->
                lost1 = lost + sum(wraps)
                {:error, error, handle1, no0 + nh, lost1}
            end

          error ->
            {:error, error, handle, no0, sum(wraps)}
        end

      _ ->
        case fwrite(curFdC, fileName, firstBins, noBytes) do
          {:ok, newCurFdC} ->
            handle1 =
              r_handle(handle, cur_fdc: newCurFdC, curB: curB + noBytes, cur_cnt: curCnt + n)

            mf_ext_log(handle1, lastBins, head, no0 + n, wraps)

          {error, newCurFdC} ->
            handle1 = r_handle(handle, cur_fdc: newCurFdC)
            {:error, error, handle1, no0, sum(wraps)}
        end
    end
  end

  defp wrap_ext_log(fName, curF, maxF, curCnt, head) do
    {newF, newMaxF} = inc_wrap(fName, curF, maxF)

    {:ok, newFdC, newFileName, lost, {nh, firstPos}, _FileSize} =
      ext_file_open(fName, newF, curF, curCnt, head)

    {newF, newMaxF, newFdC, newFileName, nh, firstPos, lost}
  end

  def mf_ext_close(
        r_handle(filename: fName, curF: curF, cur_fdc: curFdC, cur_cnt: curCnt),
        mode
      ) do
    res =
      try do
        fclose(curFdC, fName)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    write_index_file(mode, fName, curF, curF, curCnt)
    res
  end

  def change_size_wrap(handle, {newMaxB, newMaxF}, version) do
    fName = r_handle(handle, :filename)
    {_MaxB, maxF} = get_wrap_size(handle)
    write_size_file(:read_write, fName, newMaxB, newMaxF, version)

    cond do
      newMaxF > maxF ->
        remove_files(fName, maxF + 1, newMaxF)
        {:ok, r_handle(handle, maxB: newMaxB, maxF: newMaxF)}

      newMaxF < maxF ->
        {:ok, r_handle(handle, maxB: newMaxB, maxF: {newMaxF, maxF})}

      true ->
        {:ok, r_handle(handle, maxB: newMaxB, maxF: newMaxF)}
    end
  end

  defp int_file_open(fName, newFile, oldFile, oldCnt, head) do
    repair = :truncate
    mode = :read_write
    int_file_open(fName, newFile, oldFile, oldCnt, head, repair, mode)
  end

  defp int_file_open(fName, newFile, oldFile, oldCnt, head, repair, mode) do
    n = add_ext(fName, newFile)

    case int_open(n, repair, mode, head) do
      {:ok, {_Alloc, fdC, headSize, fileSize}} ->
        lost = write_index_file(mode, fName, newFile, oldFile, oldCnt)
        {:ok, fdC, n, lost, headSize, fileSize}

      {:repaired, fdC, recovered, badBytes, fileSize} ->
        write_index_file(mode, fName, newFile, oldFile, oldCnt)
        {:repaired, fdC, n, recovered, badBytes, fileSize}
    end
  end

  defp ext_file_open(fName, newFile, oldFile, oldCnt, head) do
    repair = :truncate
    mode = :read_write
    ext_file_open(fName, newFile, oldFile, oldCnt, head, repair, mode)
  end

  defp ext_file_open(fName, newFile, oldFile, oldCnt, head, repair, mode) do
    fileName = add_ext(fName, newFile)
    {:ok, {_Alloc, fdC, headSize, fileSize}} = ext_open(fileName, repair, mode, head)
    lost = write_index_file(mode, fName, newFile, oldFile, oldCnt)
    {:ok, fdC, fileName, lost, headSize, fileSize}
  end

  defp read_index_file(:truncate, fName, maxF) do
    remove_files(fName, 2, maxF)
    _ = :file.delete(add_ext(fName, 'idx'))
    {1, 0, 0, 0}
  end

  defp read_index_file(_, fName, _MaxF) do
    read_index_file(fName)
  end

  def read_index_file(fName) do
    fileName = add_ext(fName, 'idx')

    case open_read(fileName) do
      {:ok, fd} ->
        r =
          case :file.read(fd, 65536) do
            {:ok, <<0, 0::size(32), version, curF::size(32), tail::binary>>}
            when version === 2 and 0 < curF and curF < 65000 ->
              parse_index(curF, version, 1, tail, fd, 0, 0, 0)

            {:ok, <<0, curF::size(32), tail::binary>>}
            when 0 < curF and curF < 65000 ->
              parse_index(curF, 1, 1, tail, fd, 0, 0, 0)

            {:ok, <<curF, tail::binary>>} when 0 < curF ->
              parse_index(curF, 1, 1, tail, fd, 0, 0, 0)

            _ErrorOrEof ->
              {1, 0, 0, 0}
          end

        _ = :file.close(fd)
        r

      _Error ->
        {1, 0, 0, 0}
    end
  end

  defp parse_index(curF, v, curF, <<curSz::size(64), tail::binary>>, fd, _, totSz, nFiles)
       when v === 2 do
    parse_index(curF, v, curF + 1, tail, fd, curSz, totSz, nFiles + 1)
  end

  defp parse_index(curF, v, n, <<sz::size(64), tail::binary>>, fd, curSz, totSz, nFiles)
       when v === 2 do
    parse_index(curF, v, n + 1, tail, fd, curSz, totSz + sz, nFiles + 1)
  end

  defp parse_index(curF, v, curF, <<curSz::size(32), tail::binary>>, fd, _, totSz, nFiles)
       when v < 2 do
    parse_index(curF, v, curF + 1, tail, fd, curSz, totSz, nFiles + 1)
  end

  defp parse_index(curF, v, n, <<sz::size(32), tail::binary>>, fd, curSz, totSz, nFiles)
       when v < 2 do
    parse_index(curF, v, n + 1, tail, fd, curSz, totSz + sz, nFiles + 1)
  end

  defp parse_index(curF, v, n, b, fd, curSz, totSz, nFiles) do
    case :file.read(fd, 65536) do
      :eof when 0 === byte_size(b) ->
        {curF, curSz, totSz, nFiles}

      {:ok, bin} ->
        newB = :erlang.list_to_binary([b, bin])
        parse_index(curF, v, n, newB, fd, curSz, totSz, nFiles)

      _ErrorOrEof ->
        {1, 0, 0, 0}
    end
  end

  defp write_index_file(:read_only, _FName, _NewFile, _OldFile, _OldCnt) do
    0
  end

  defp write_index_file(:read_write, fName, newFile, oldFile, oldCnt) do
    fileName = add_ext(fName, 'idx')

    case open_update(fileName) do
      {:ok, fd} ->
        {offset, szSz} =
          case :file.read(fd, 6) do
            :eof ->
              bin = <<0, 0::size(32), 2, newFile::size(32)>>
              fwrite_close2(fd, fileName, bin)
              {10, 8}

            {:ok, <<0, 0::size(32), _Version>>} ->
              pwrite_close2(fd, fileName, 6, <<newFile::size(32)>>)
              {10, 8}

            {:ok, <<0, _::binary>>} ->
              pwrite_close2(fd, fileName, 1, <<newFile::size(32)>>)
              {5, 4}

            {:ok, <<_, _::binary>>} ->
              case :file.read_file(fileName) do
                {:ok, <<_CurF, tail::binary>>} ->
                  {:ok, _} = position_close2(fd, fileName, :bof)
                  bin = <<0, 0::size(32), 2, newFile::size(32)>>
                  newTail = to_8_bytes(tail, [], fileName, fd)
                  fwrite_close2(fd, fileName, [bin | newTail])
                  {10, 8}

                error ->
                  file_error_close(fd, fileName, error)
              end

            error ->
              file_error_close(fd, fileName, error)
          end

        newPos = offset + (newFile - 1) * szSz
        oldCntBin = <<oldCnt::size(szSz)-unit(8)>>

        cond do
          oldFile > 0 ->
            r = :file.pread(fd, newPos, szSz)
            oldPos = offset + (oldFile - 1) * szSz
            pwrite_close2(fd, fileName, oldPos, oldCntBin)
            _ = :file.close(fd)

            case r do
              {:ok, <<lost::size(szSz)-unit(8)>>} ->
                lost

              {:ok, _} ->
                throw({:error, {:invalid_index_file, fileName}})

              :eof ->
                0

              error2 ->
                file_error(fileName, error2)
            end

          true ->
            pwrite_close2(fd, fileName, newPos, oldCntBin)
            _ = :file.close(fd)
            0
        end

      e ->
        file_error(fileName, e)
    end
  end

  defp to_8_bytes(<<n::size(32), t::binary>>, nT, fileName, fd) do
    to_8_bytes(t, [nT | <<n::size(64)>>], fileName, fd)
  end

  defp to_8_bytes(b, nT, _FileName, _Fd)
       when byte_size(b) === 0 do
    nT
  end

  defp to_8_bytes(_B, _NT, fileName, fd) do
    _ = :file.close(fd)
    throw({:error, {:invalid_index_file, fileName}})
  end

  defp index_file_trunc(fName, n) do
    fileName = add_ext(fName, 'idx')

    case open_update(fileName) do
      {:ok, fd} ->
        case :file.read(fd, 6) do
          :eof ->
            _ = :file.close(fd)
            :ok

          {:ok, <<0, 0::size(32), version>>} when version === 2 ->
            truncate_index_file(fd, fileName, 10, 8, n)

          {:ok, <<0, _::binary>>} ->
            truncate_index_file(fd, fileName, 5, 4, n)

          {:ok, <<_, _::binary>>} ->
            truncate_index_file(fd, fileName, 1, 4, n)

          error ->
            file_error_close(fd, fileName, error)
        end

      error ->
        file_error(fileName, error)
    end
  end

  defp truncate_index_file(fd, fileName, offset, n, szSz) do
    pos = offset + n * szSz

    case pos > file_size(fileName) do
      true ->
        :ok = :file.close(fd)

      false ->
        truncate_at_close2(fd, fileName, {:bof, pos})
        :ok = :file.close(fd)
    end

    :ok
  end

  def print_index_file(file) do
    :io.format('-- Index begin --~n')

    case :file.read_file(file) do
      {:ok, <<0, 0::size(32), version, curF::size(32), tail::binary>>}
      when version === 2 and 0 < curF and curF < 65000 ->
        :io.format('cur file: ~w~n', [curF])
        loop_index(1, version, tail)

      {:ok, <<0, curF::size(32), tail::binary>>}
      when 0 < curF and curF < 65000 ->
        :io.format('cur file: ~w~n', [curF])
        loop_index(1, 1, tail)

      {:ok, <<curF, tail::binary>>} when 0 < curF ->
        :io.format('cur file: ~w~n', [curF])
        loop_index(1, 1, tail)

      _Else ->
        :ok
    end

    :io.format('-- end --~n')
  end

  defp loop_index(n, v, <<sz::size(64), tail::binary>>)
       when v === 2 do
    :io.format(' ~p  items: ~w~n', [n, sz])
    loop_index(n + 1, v, tail)
  end

  defp loop_index(n, v, <<sz::size(32), tail::binary>>)
       when v < 2 do
    :io.format(' ~p  items: ~w~n', [n, sz])
    loop_index(n + 1, v, tail)
  end

  defp loop_index(_, _, _) do
    :ok
  end

  defp write_size_file(:read_only, _FName, _NewSize, _NewMaxFiles, _Version) do
    :ok
  end

  defp write_size_file(:read_write, fName, newSize, newMaxFiles, version) do
    fileName = add_ext(fName, 'siz')

    bin =
      cond do
        version === 2 ->
          <<version, newSize::size(64), newMaxFiles::size(32)>>

        true ->
          <<newSize::size(32), newMaxFiles::size(32)>>
      end

    case :file.write_file(fileName, bin) do
      :ok ->
        :ok

      e ->
        file_error(fileName, e)
    end
  end

  def read_size_file(fName) do
    {size, _Version} = read_size_file_version(fName)
    size
  end

  def read_size_file_version(fName) do
    case :file.read_file(add_ext(fName, 'siz')) do
      {:ok, <<version, size::size(64), maxFiles::size(32)>>}
      when version === 2 ->
        {{size, maxFiles}, version}

      {:ok, <<size::size(32), maxFiles::size(32)>>} ->
        {{size, maxFiles}, 1}

      _ ->
        {{0, 0}, 2}
    end
  end

  defp conv({more, terms}, fileNo)
       when elem(more, 0) === :continuation do
    cont = r_continuation(more, pos: {fileNo, r_continuation(more, :pos)})
    {cont, terms}
  end

  defp conv({more, terms, bad}, fileNo)
       when elem(more, 0) === :continuation do
    cont = r_continuation(more, pos: {fileNo, r_continuation(more, :pos)})
    {cont, terms, bad}
  end

  defp conv(other, _) do
    other
  end

  defp find_first_file(r_handle(filename: fName, curF: curF, maxF: maxF)) do
    fff(fName, inc(curF, maxF), curF, maxF)
  end

  defp fff(_FName, curF, curF, _MaxF) do
    curF
  end

  defp fff(fName, maybeFirstF, curF, maxF) do
    n = add_ext(fName, maybeFirstF)

    case :file.read_file_info(n) do
      {:ok, _} ->
        maybeFirstF

      _ ->
        fff(fName, inc(maybeFirstF, maxF), curF, maxF)
    end
  end

  defp ext_split_bins(curB, maxB, firstPos, bins) do
    maxBs = maxB - curB
    isFirst = curB === firstPos
    ext_split_bins(maxBs, isFirst, [], bins, 0, 0)
  end

  defp ext_split_bins(maxBs, isFirst, first, [x | last], bs, n) do
    nBs = bs + byte_size(x)

    cond do
      nBs <= maxBs ->
        ext_split_bins(maxBs, isFirst, [first | x], last, nBs, n + 1)

      isFirst and first === [] ->
        {[x], last, nBs, n + 1}

      true ->
        {first, [x | last], bs, n}
    end
  end

  defp ext_split_bins(_, _, first, [], bs, n) do
    {first, [], bs, n}
  end

  defp int_split_bins(curB, maxB, firstPos, bins) do
    maxBs = maxB - curB
    isFirst = curB === firstPos
    int_split_bins(maxBs, isFirst, [], bins, 0, 0)
  end

  defp int_split_bins(maxBs, isFirst, first, [x | last], bs, n) do
    sz = byte_size(x)
    nBs = bs + sz + 8
    bSz = <<sz::size(4)-unit(8)>>

    xB =
      case sz < 65528 do
        true ->
          [bSz, <<98, 87, 76, 65>> | x]

        false ->
          mD5 = :erlang.md5(bSz)
          [bSz, <<98, 87, 76, 65>>, mD5 | x]
      end

    cond do
      nBs <= maxBs ->
        int_split_bins(maxBs, isFirst, [first | xB], last, nBs, n + 1)

      isFirst and first === [] ->
        {[xB], last, nBs, n + 1}

      true ->
        {first, [x | last], bs, n}
    end
  end

  defp int_split_bins(_, _, first, [], bs, n) do
    {first, [], bs, n}
  end

  defp inc_wrap(fName, curF, maxF) do
    case maxF do
      {newMaxF, oldMaxF} ->
        cond do
          curF >= newMaxF ->
            remove_files(fName, curF + 1, oldMaxF)

            cond do
              curF > newMaxF ->
                {1, {newMaxF, curF}}

              true ->
                index_file_trunc(fName, newMaxF)
                {1, newMaxF}
            end

          true ->
            newFt = inc(curF, newMaxF)
            {newFt, maxF}
        end

      ^maxF ->
        newFt = inc(curF, maxF)
        {newFt, maxF}
    end
  end

  defp inc(n, {_NewMax, oldMax}) do
    inc(n, oldMax, 1)
  end

  defp inc(n, max) do
    inc(n, max, 1)
  end

  defp inc(n, max, step) do
    nx = rem(n + step, max)

    cond do
      nx > 0 ->
        nx

      true ->
        nx + max
    end
  end

  defp file_size(fname) do
    {:ok, fi} = :file.read_file_info(fname)
    r_file_info(fi, :size)
  end

  defp remove_files(fName, n, max) do
    remove_files(fName, n, max, :ok)
  end

  defp remove_files(_FName, n, max, :ok) when n > max do
    :ok
  end

  defp remove_files(_FName, n, max, {fileName, error})
       when n > max do
    file_error(fileName, error)
  end

  defp remove_files(fName, n, max, reply) do
    fileName = add_ext(fName, n)

    newReply =
      case :file.delete(fileName) do
        :ok ->
          reply

        {:error, :enoent} ->
          reply

        error ->
          {fileName, error}
      end

    remove_files(fName, n + 1, max, newReply)
  end

  def get_wrap_size(r_handle(maxB: maxB, maxF: maxF)) do
    case maxF do
      {newMaxF, _} ->
        {maxB, newMaxF}

      ^maxF ->
        {maxB, maxF}
    end
  end

  defp add_ext(name, ext) do
    concat([name, '.', ext])
  end

  defp open_read(fileName) do
    :file.open(fileName, [:raw, :binary, :read])
  end

  defp open_update(fileName) do
    :file.open(fileName, [:raw, :binary, :read, :write])
  end

  defp open_truncate(fileName) do
    :file.open(fileName, [:raw, :binary, :write])
  end

  def fwrite(fdC, _FN, _B, 0) do
    {:ok, fdC}
  end

  def fwrite(r_cache(fd: fd, c: c, sz: sz) = fdC, fileName, b, size) do
    sz1 = sz + size
    c1 = cache_append(c, b)

    cond do
      sz1 > 65536 ->
        write_cache(fd, fileName, c1)

      true ->
        maybe_start_timer(c)
        {:ok, r_cache(fdC, sz: sz1, c: c1)}
    end
  end

  defp cache_append([], b) do
    b
  end

  defp cache_append(c, b) do
    [c | b]
  end

  defp maybe_start_timer([]) do
    case :erlang.get(:write_cache_timer_is_running) do
      true ->
        :ok

      _ ->
        :erlang.put(:write_cache_timer_is_running, true)
        :erlang.send_after(2000, self(), {self(), :write_cache})
        :ok
    end
  end

  defp maybe_start_timer(_C) do
    :ok
  end

  defp fwrite_header(fd, b, size) do
    {:ok, r_cache(fd: fd, sz: size, c: b)}
  end

  defp pread(r_cache(fd: fd, c: c), fileName, position, maxBytes) do
    reply = write_cache(fd, fileName, c)

    case reply do
      {:ok, newFdC} ->
        case :file.pread(fd, position, maxBytes) do
          {:error, error} ->
            {newFdC,
             try do
               file_error(fileName, {:error, error})
             catch
               :error, e -> {:EXIT, {e, __STACKTRACE__}}
               :exit, e -> {:EXIT, e}
               e -> e
             end}

          r ->
            {newFdC, r}
        end

      {error, newFdC} ->
        {newFdC, error}
    end
  end

  def position(r_cache(fd: fd, c: c), fileName, pos) do
    reply = write_cache(fd, fileName, c)

    case reply do
      {:ok, newFdC} ->
        case position2(fd, fileName, pos) do
          {:ok, loc} ->
            {:ok, newFdC, loc}

          error ->
            {error, newFdC}
        end

      _Error ->
        reply
    end
  end

  defp position_close(r_cache(fd: fd, c: c), fileName, pos) do
    newFdC = write_cache_close(fd, fileName, c)
    {:ok, loc} = position_close2(fd, fileName, pos)
    {newFdC, loc}
  end

  defp fsync(r_cache(fd: fd, c: c), fileName) do
    reply = write_cache(fd, fileName, c)

    case reply do
      {:ok, newFdC} ->
        case :file.sync(fd) do
          :ok ->
            reply

          error ->
            {try do
               file_error(fileName, error)
             catch
               :error, e -> {:EXIT, {e, __STACKTRACE__}}
               :exit, e -> {:EXIT, e}
               e -> e
             end, newFdC}
        end

      _Error ->
        reply
    end
  end

  def truncate_at(fdC, fileName, pos) do
    case position(fdC, fileName, pos) do
      {:ok, newFdC, _Pos} ->
        case :file.truncate(r_cache(newFdC, :fd)) do
          :ok ->
            {:ok, newFdC}

          error ->
            {try do
               file_error(fileName, error)
             catch
               :error, e -> {:EXIT, {e, __STACKTRACE__}}
               :exit, e -> {:EXIT, e}
               e -> e
             end, newFdC}
        end

      reply ->
        reply
    end
  end

  defp fwrite_close2(fd, fileName, b) do
    case :file.write(fd, b) do
      :ok ->
        :ok

      error ->
        file_error_close(fd, fileName, error)
    end
  end

  defp pwrite_close2(fd, fileName, position, b) do
    case :file.pwrite(fd, position, b) do
      :ok ->
        :ok

      {:error, error} ->
        file_error(fileName, {:error, error})
    end
  end

  defp position2(fd, fileName, pos) do
    case :file.position(fd, pos) do
      {:error, error} ->
        try do
          file_error(fileName, {:error, error})
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

      oK ->
        oK
    end
  end

  defp position_close2(fd, fileName, pos) do
    case :file.position(fd, pos) do
      {:error, error} ->
        file_error_close(fd, fileName, {:error, error})

      oK ->
        oK
    end
  end

  defp truncate_at_close2(fd, fileName, pos) do
    {:ok, _} = position_close2(fd, fileName, pos)

    case :file.truncate(fd) do
      :ok ->
        :ok

      error ->
        file_error_close(fd, fileName, error)
    end
  end

  def fclose(r_cache(fd: fd, c: c), fileName) do
    _ = write_cache_close(fd, fileName, c)
    :file.close(fd)
  end

  def set_quiet(bool) do
    :erlang.put(:quiet, bool)
  end

  def is_quiet() do
    :erlang.get(:quiet) === true
  end

  defp write_cache(fd, _FileName, []) do
    {:ok, r_cache(fd: fd)}
  end

  defp write_cache(fd, fileName, c) do
    case :file.write(fd, c) do
      :ok ->
        {:ok, r_cache(fd: fd)}

      error ->
        {try do
           file_error(fileName, error)
         catch
           :error, e -> {:EXIT, {e, __STACKTRACE__}}
           :exit, e -> {:EXIT, e}
           e -> e
         end, r_cache(fd: fd)}
    end
  end

  defp write_cache_close(fd, _FileName, []) do
    r_cache(fd: fd)
  end

  defp write_cache_close(fd, fileName, c) do
    case :file.write(fd, c) do
      :ok ->
        r_cache(fd: fd)

      error ->
        file_error_close(fd, fileName, error)
    end
  end

  defp file_error(fileName, {:error, error}) do
    throw({:error, {:file_error, fileName, error}})
  end

  defp file_error_close(fd, fileName, {:error, error}) do
    _ = :file.close(fd)
    throw({:error, {:file_error, fileName, error}})
  end
end
