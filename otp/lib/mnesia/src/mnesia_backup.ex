defmodule :m_mnesia_backup do
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

  Record.defrecord(:r_backup, :backup,
    tmp_file: :undefined,
    file: :undefined,
    file_desc: :undefined
  )

  def open_write(opaqueData) do
    file = opaqueData
    tmp = :lists.concat([file, '.BUPTMP'])
    :file.delete(tmp)
    :file.delete(file)

    case :disk_log.open([{:name, make_ref()}, {:file, tmp}, {:repair, false}, {:linkto, self()}]) do
      {:ok, fd} ->
        {:ok, r_backup(tmp_file: tmp, file: file, file_desc: fd)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  def write(opaqueData, backupItems) do
    b = opaqueData

    case :disk_log.log_terms(
           r_backup(b, :file_desc),
           backupItems
         ) do
      :ok ->
        {:ok, b}

      {:error, reason} ->
        abort_write(b)
        {:error, reason}
    end
  end

  def commit_write(opaqueData) do
    b = opaqueData

    case :disk_log.sync(r_backup(b, :file_desc)) do
      :ok ->
        case :disk_log.close(r_backup(b, :file_desc)) do
          :ok ->
            case :file.rename(r_backup(b, :tmp_file), r_backup(b, :file)) do
              :ok ->
                {:ok, r_backup(b, :file)}

              {:error, reason} ->
                {:error, reason}
            end

          {:error, reason} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  def abort_write(backupRef) do
    res = :disk_log.close(r_backup(backupRef, :file_desc))
    :file.delete(r_backup(backupRef, :tmp_file))

    case res do
      :ok ->
        {:ok, r_backup(backupRef, :file)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  Record.defrecord(:r_restore, :restore, file: :undefined, file_desc: :undefined, cont: :undefined)

  def open_read(opaqueData) do
    file = opaqueData

    case :file.read_file_info(file) do
      {:error, reason} ->
        {:error, reason}

      _FileInfo ->
        case :disk_log.open([
               {:file, file},
               {:name, make_ref()},
               {:repair, false},
               {:mode, :read_only},
               {:linkto, self()}
             ]) do
          {:ok, fd} ->
            {:ok, r_restore(file: file, file_desc: fd, cont: :start)}

          {:repaired, fd, _, {:badbytes, 0}} ->
            {:ok, r_restore(file: file, file_desc: fd, cont: :start)}

          {:repaired, fd, _, _} ->
            {:ok, r_restore(file: file, file_desc: fd, cont: :start)}

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  def read(opaqueData) do
    r = opaqueData
    fd = r_restore(r, :file_desc)

    case :disk_log.chunk(fd, r_restore(r, :cont)) do
      {:error, reason} ->
        {:error, {'Possibly truncated', reason}}

      :eof ->
        {:ok, r, []}

      {cont, []} ->
        read(r_restore(r, cont: cont))

      {cont, backupItems, _BadBytes} ->
        {:ok, r_restore(r, cont: cont), backupItems}

      {cont, backupItems} ->
        {:ok, r_restore(r, cont: cont), backupItems}
    end
  end

  def close_read(opaqueData) do
    r = opaqueData

    case :disk_log.close(r_restore(r, :file_desc)) do
      :ok ->
        {:ok, r_restore(r, :file)}

      {:error, reason} ->
        {:error, reason}
    end
  end
end
