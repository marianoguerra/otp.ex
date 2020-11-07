defmodule :m_raw_file_io_compressed do
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

  def open_layer(filename, modes, options) do
    isAppend = :lists.member(:append, modes)
    isDeflate = :lists.member(:write, modes)
    isInflate = :lists.member(:read, modes)

    cond do
      (isDeflate and isInflate) or isAppend ->
        {:error, :einval}

      isDeflate and not isInflate ->
        start_server_module(:raw_file_io_deflate, filename, modes, options)

      isInflate ->
        start_server_module(:raw_file_io_inflate, filename, modes, options)
    end
  end

  defp start_server_module(module, filename, modes, options) do
    secret = make_ref()

    case :gen_statem.start(module, {self(), secret, options}, []) do
      {:ok, pid} ->
        open_next_layer(pid, secret, filename, modes)

      other ->
        other
    end
  end

  defp open_next_layer(pid, secret, filename, modes) do
    case :gen_statem.call(pid, {:"$open", secret, filename, modes}, :infinity) do
      :ok ->
        publicFd =
          r_file_descriptor(
            module: :raw_file_io_compressed,
            data: {self(), pid}
          )

        {:ok, publicFd}

      other ->
        other
    end
  end

  def close(fd) do
    wrap_call(fd, [:close])
  end

  def sync(fd) do
    wrap_call(fd, [:sync])
  end

  def datasync(fd) do
    wrap_call(fd, [:datasync])
  end

  def truncate(fd) do
    wrap_call(fd, [:truncate])
  end

  def advise(fd, offset, length, advise) do
    wrap_call(fd, [:advise, offset, length, advise])
  end

  def allocate(fd, offset, length) do
    wrap_call(fd, [:allocate, offset, length])
  end

  def position(fd, mark) do
    wrap_call(fd, [:position, mark])
  end

  def write(fd, iOData) do
    try do
      compactedData = :erlang.iolist_to_iovec(iOData)
      wrap_call(fd, [:write, compactedData])
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def pwrite(fd, offset, iOData) do
    try do
      compactedData = :erlang.iolist_to_iovec(iOData)
      wrap_call(fd, [:pwrite, offset, compactedData])
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def pwrite(fd, locBytes) do
    try do
      compactedLocBytes =
        for {offset, iOData} <- locBytes do
          {offset, :erlang.iolist_to_iovec(iOData)}
        end

      wrap_call(fd, [:pwrite, compactedLocBytes])
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  def read_line(fd) do
    wrap_call(fd, [:read_line])
  end

  def read(fd, size) do
    wrap_call(fd, [:read, size])
  end

  def pread(fd, offset, size) do
    wrap_call(fd, [:pread, offset, size])
  end

  def pread(fd, locNums) do
    wrap_call(fd, [:pread, locNums])
  end

  def ipread_s32bu_p32bu(fd, offset, maxSize) do
    wrap_call(fd, [:ipread_s32bu_p32bu, offset, maxSize])
  end

  def sendfile(_, _, _, _, _, _, _, _) do
    {:error, :enotsup}
  end

  def read_handle_info(fd, opts) do
    wrap_call(fd, [opts])
  end

  defp wrap_call(fd, command) do
    {_Owner, pid} = get_fd_data(fd)

    try do
      :gen_statem.call(pid, command, :infinity)
    catch
      :exit, {:normal, _StackTrace} ->
        {:error, :einval}

      :exit, {:noproc, _StackTrace} ->
        {:error, :einval}
    else
      result ->
        result
    end
  end

  defp get_fd_data(r_file_descriptor(data: data)) do
    {owner, _ServerPid} = data

    case self() do
      ^owner ->
        data

      _ ->
        :erlang.error(:not_on_controlling_process)
    end
  end
end
