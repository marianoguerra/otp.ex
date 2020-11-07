defmodule :m_wrap_log_reader do
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

  Record.defrecord(:r_wrap_reader, :wrap_reader,
    fd: :undefined,
    cont: :undefined,
    file: :undefined,
    file_no: :undefined,
    mod_time: :undefined,
    first_no: :undefined
  )

  def open(file) when is_atom(file) do
    open(:erlang.atom_to_list(file))
  end

  def open(file) when is_list(file) do
    case read_index_file(file) do
      {:ok, {curFileNo, _CurFileSz, _TotSz, noOfFiles}}
      when curFileNo === noOfFiles + 1 ->
        fileNo = 1
        :ok
        open_int(file, fileNo, fileNo)

      {:ok, {curFileNo, _CurFileSz, _TotSz, noOfFiles}} ->
        fileNo =
          case rem(curFileNo + 1, noOfFiles) do
            0 ->
              noOfFiles

            no ->
              no
          end

        :ok
        open_int(file, fileNo, fileNo)

      error ->
        error
    end
  end

  def open(file, fileNo)
      when is_atom(file) and
             is_integer(fileNo) do
    open(:erlang.atom_to_list(file), fileNo)
  end

  def open(file, fileNo)
      when is_list(file) and
             is_integer(fileNo) do
    case read_index_file(file) do
      {:ok, {_CurFileNo, _CurFileSz, _TotSz, noOfFiles}}
      when noOfFiles >= fileNo ->
        :ok
        open_int(file, fileNo, :one)

      {:ok, {curFileNo, _CurFileSz, _TotSz, noOfFiles}}
      when curFileNo === fileNo and
             curFileNo === noOfFiles + 1 ->
        :ok
        open_int(file, fileNo, :one)

      {:ok, {_CurFileNo, _CurFileSz, _TotSz, _NoOfFiles}} ->
        {:error, {:file_not_found, add_ext(file, fileNo)}}

      error ->
        error
    end
  end

  def close(r_wrap_reader(fd: fD)) do
    :file.close(fD)
  end

  def chunk(wR = r_wrap_reader()) do
    chunk(wR, 65536, 0)
  end

  def chunk(wR = r_wrap_reader(), :infinity) do
    chunk(wR, 65536, 0)
  end

  def chunk(wR = r_wrap_reader(), n) when is_integer(n) and n > 0 do
    chunk(wR, n, 0)
  end

  defp open_int(file, fileNo, firstFileNo) do
    fName = add_ext(file, fileNo)

    case :file.open(fName, [:raw, :binary, :read]) do
      {:ok, fd} ->
        case :file.read(fd, 8) do
          {:ok, head} ->
            case :disk_log_1.is_head(head) do
              :no ->
                _ = :file.close(fd)
                {:error, {:not_a_log_file, fName}}

              _ ->
                case last_mod_time(fName) do
                  {:ok, modTime} ->
                    wR =
                      r_wrap_reader(
                        fd: fd,
                        cont: :start,
                        file: file,
                        file_no: fileNo,
                        mod_time: modTime,
                        first_no: firstFileNo
                      )

                    {:ok, wR}

                  {:error, e} ->
                    _ = :file.close(fd)
                    {:error, {:file_error, fName, e}}
                end
            end

          _Other ->
            _ = :file.close(fd)
            {:error, {:not_a_log_file, fName}}
        end

      _Other ->
        {:error, {:not_a_log_file, fName}}
    end
  end

  defp chunk(wR, n, bad) do
    r_wrap_reader(fd: fd, cont: continue, file: file, file_no: curFileNo, first_no: firstFileNo) =
      wR

    case read_a_chunk(fd, n, continue, add_ext(file, curFileNo)) do
      :eof ->
        case firstFileNo do
          :one ->
            {wR, :eof}

          _Else ->
            chunk_at_eof(wR, n, bad)
        end

      {contOut, [], badBytes} ->
        :ok
        chunk(r_wrap_reader(wR, cont: contOut), n, bad + badBytes)

      {contOut, chunk, badBytes} when bad + badBytes === 0 ->
        {r_wrap_reader(wR, cont: contOut), chunk}

      {contOut, chunk, badBytes} ->
        :ok
        {r_wrap_reader(wR, cont: contOut), chunk, bad + badBytes}

      error ->
        error
    end
  end

  defp read_a_chunk(fd, n, :start, fileName) do
    read_a_chunk(fd, fileName, 0, [], n)
  end

  defp read_a_chunk(fd, n, more, fileName) do
    pos = r_continuation(more, :pos)
    b = r_continuation(more, :b)
    read_a_chunk(fd, fileName, pos, b, n)
  end

  defp read_a_chunk(fd, fileName, pos, b, n) do
    r = :disk_log_1.chunk_read_only(fd, fileName, pos, b, n)
    log = :foo

    case :disk_log.ichunk_end(r, log) do
      {c = r_continuation(), s} ->
        {c, s, 0}

      else__ ->
        else__
    end
  end

  defp chunk_at_eof(wR, n, bad) do
    r_wrap_reader(file: file, file_no: curFileNo, first_no: firstFileNo) = wR

    case read_index_file(file) do
      {:ok, indexFile} ->
        {_, _, _, noOfFiles} = indexFile

        newFileNo =
          case rem(curFileNo + 1, noOfFiles) do
            _ when curFileNo > noOfFiles ->
              1

            0 when noOfFiles > 1 ->
              noOfFiles

            no when curFileNo === noOfFiles ->
              fileName = add_ext(file, curFileNo + 1)

              case :file.read_file_info(fileName) do
                {:ok, _} ->
                  curFileNo + 1

                _ ->
                  no
              end

            no ->
              no
          end

        :ok

        case {firstFileNo, newFileNo} do
          {_, 0} ->
            {wR, :eof}

          {_, ^firstFileNo} ->
            {wR, :eof}

          _ ->
            read_next_file(wR, n, newFileNo, bad)
        end

      error ->
        error
    end
  end

  defp read_index_file(file) do
    case (try do
            :disk_log_1.read_index_file(file)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {1, 0, 0, 0} ->
        {:error, {:index_file_not_found, file}}

      {:error, _Reason} ->
        {:error, {:index_file_not_found, file}}

      fileData ->
        {:ok, fileData}
    end
  end

  defp read_next_file(wR, n, newFileNo, bad) do
    r_wrap_reader(file: file, file_no: curFileNo, mod_time: modTime, first_no: firstFileNo) = wR

    case last_mod_time(add_ext(file, newFileNo)) do
      {:ok, newModTime} ->
        oldMT = :calendar.datetime_to_gregorian_seconds(modTime)
        newMT = :calendar.datetime_to_gregorian_seconds(newModTime)
        diff = newMT - oldMT
        :ok

        cond do
          diff < 0 ->
            {:error, {:is_wrapped, add_ext(file, curFileNo)}}

          true ->
            case open_int(file, newFileNo, firstFileNo) do
              {:ok, nWR} ->
                _ = close(wR)
                chunk(nWR, n, bad)

              error ->
                error
            end
        end

      {:error, eN} ->
        {:error, {:file_error, add_ext(file, newFileNo), eN}}
    end
  end

  defp last_mod_time(file) do
    case :file.read_file_info(file) do
      {:ok, fileInfo} ->
        {:ok, r_file_info(fileInfo, :mtime)}

      e ->
        {:error, e}
    end
  end

  defp add_ext(file, ext) do
    :lists.concat([file, '.', ext])
  end
end
