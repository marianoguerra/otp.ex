defmodule :m_raw_file_io_list do
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

  def open_layer(filename, modes, [:list]) do
    case :raw_file_io.open(filename, [:binary | modes]) do
      {:ok, privateFd} ->
        {:ok, make_public_fd(privateFd, modes)}

      other ->
        other
    end
  end

  defp make_public_fd(privateFd, modes) do
    case :lists.member(:read, modes) do
      true ->
        r_file_descriptor(module: :raw_file_io_list, data: privateFd)

      false ->
        privateFd
    end
  end

  def close(fd) do
    privateFd = r_file_descriptor(fd, :data)
    apply(r_file_descriptor(privateFd, :module), :close, [privateFd])
  end

  def sync(fd) do
    privateFd = r_file_descriptor(fd, :data)
    apply(r_file_descriptor(privateFd, :module), :sync, [privateFd])
  end

  def datasync(fd) do
    privateFd = r_file_descriptor(fd, :data)
    apply(r_file_descriptor(privateFd, :module), :datasync, [privateFd])
  end

  def truncate(fd) do
    privateFd = r_file_descriptor(fd, :data)
    apply(r_file_descriptor(privateFd, :module), :truncate, [privateFd])
  end

  def advise(fd, offset, length, advise) do
    privateFd = r_file_descriptor(fd, :data)
    apply(r_file_descriptor(privateFd, :module), :advise, [privateFd, offset, length, advise])
  end

  def allocate(fd, offset, length) do
    privateFd = r_file_descriptor(fd, :data)
    apply(r_file_descriptor(privateFd, :module), :allocate, [privateFd, offset, length])
  end

  def position(fd, mark) do
    privateFd = r_file_descriptor(fd, :data)
    apply(r_file_descriptor(privateFd, :module), :position, [privateFd, mark])
  end

  def write(fd, iOData) do
    privateFd = r_file_descriptor(fd, :data)
    apply(r_file_descriptor(privateFd, :module), :write, [privateFd, iOData])
  end

  def pwrite(fd, offset, iOData) do
    privateFd = r_file_descriptor(fd, :data)
    apply(r_file_descriptor(privateFd, :module), :pwrite, [privateFd, offset, iOData])
  end

  def pwrite(fd, locBytes) do
    privateFd = r_file_descriptor(fd, :data)
    apply(r_file_descriptor(privateFd, :module), :pwrite, [privateFd, locBytes])
  end

  def read_line(fd) do
    privateFd = r_file_descriptor(fd, :data)

    case apply(r_file_descriptor(privateFd, :module), :read_line, [privateFd]) do
      {:ok, binary} ->
        {:ok, :erlang.binary_to_list(binary)}

      other ->
        other
    end
  end

  def read(fd, size) do
    privateFd = r_file_descriptor(fd, :data)

    case apply(r_file_descriptor(privateFd, :module), :read, [privateFd, size]) do
      {:ok, binary} ->
        {:ok, :erlang.binary_to_list(binary)}

      other ->
        other
    end
  end

  def pread(fd, offset, size) do
    privateFd = r_file_descriptor(fd, :data)

    case apply(r_file_descriptor(privateFd, :module), :pread, [privateFd, offset, size]) do
      {:ok, binary} ->
        {:ok, :erlang.binary_to_list(binary)}

      other ->
        other
    end
  end

  def pread(fd, locNums) do
    privateFd = r_file_descriptor(fd, :data)

    case apply(r_file_descriptor(privateFd, :module), :pread, [privateFd, locNums]) do
      {:ok, locResults} ->
        translatedResults =
          for result <- locResults do
            case result do
              ^result when is_binary(result) ->
                :erlang.binary_to_list(result)

              :eof ->
                :eof
            end
          end

        {:ok, translatedResults}

      other ->
        other
    end
  end

  def ipread_s32bu_p32bu(fd, offset, maxSize) do
    privateFd = r_file_descriptor(fd, :data)

    case apply(r_file_descriptor(privateFd, :module), :ipread_s32bu_p32bu, [
           privateFd,
           offset,
           maxSize
         ]) do
      {:ok, {size, pointer, binary}} when is_binary(binary) ->
        {:ok, {size, pointer, :erlang.binary_to_list(binary)}}

      other ->
        other
    end
  end

  def sendfile(fd, dest, offset, bytes, chunkSize, headers, trailers, flags) do
    args = [dest, offset, bytes, chunkSize, headers, trailers, flags]
    privateFd = r_file_descriptor(fd, :data)
    apply(r_file_descriptor(privateFd, :module), :sendfile, [privateFd | args])
  end

  def read_handle_info(fd, opts) do
    privateFd = r_file_descriptor(fd, :data)
    apply(r_file_descriptor(privateFd, :module), :read_handle_info, [privateFd, opts])
  end
end
