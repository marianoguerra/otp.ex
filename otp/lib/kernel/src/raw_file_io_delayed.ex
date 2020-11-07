defmodule :m_raw_file_io_delayed do
  use Bitwise
  @behaviour :gen_statem
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
    secret = make_ref()

    case :gen_statem.start(:raw_file_io_delayed, {self(), secret, options}, []) do
      {:ok, pid} ->
        :gen_statem.call(pid, {:"$open", secret, filename, modes}, :infinity)

      other ->
        other
    end
  end

  def callback_mode() do
    :state_functions
  end

  def init({owner, secret, options}) do
    monitor = :erlang.monitor(:process, owner)

    defaults = %{
      :owner => owner,
      :monitor => monitor,
      :secret => secret,
      :timer => :none,
      :pid => self(),
      :buffer => :prim_buffer.new(),
      :delay_size => 64 <<< 10,
      :delay_time => 2000
    }

    data = fill_delay_values(defaults, options)
    {:ok, :opening, data}
  end

  defp fill_delay_values(data, []) do
    data
  end

  defp fill_delay_values(
         data,
         [{:delayed_write, size, time} | options]
       ) do
    fill_delay_values(
      %{data | :delay_size => size, :delay_time => time},
      options
    )
  end

  defp fill_delay_values(data, [_ | options]) do
    fill_delay_values(data, options)
  end

  def opening({:call, from}, {:"$open", secret, filename, modes}, %{:secret => secret} = data) do
    case :raw_file_io.open(filename, modes) do
      {:ok, privateFd} ->
        publicData =
          :maps.with(
            [:owner, :buffer, :delay_size, :pid],
            data
          )

        publicFd =
          r_file_descriptor(
            module: :raw_file_io_delayed,
            data: publicData
          )

        newData = %{data | :handle => privateFd}
        response = {:ok, publicFd}
        {:next_state, :opened, newData, [{:reply, from, response}]}

      other ->
        {:stop_and_reply, :normal, [{:reply, from, other}]}
    end
  end

  def opening(_Event, _Contents, _Data) do
    {:keep_state_and_data, [:postpone]}
  end

  def opened(:info, {:"$timed_out", secret}, %{:secret => secret} = data) do
    case try_flush_write_buffer(data) do
      :busy ->
        :gen_statem.cast(self(), :"$reset_timeout")

      :ok ->
        :ok
    end

    {:keep_state, %{data | :timer => :none}, []}
  end

  def opened(
        :info,
        {:DOWN, monitor, :process, _Owner, reason},
        %{:monitor => monitor} = data
      ) do
    cond do
      reason !== :kill ->
        try_flush_write_buffer(data)

      reason === :kill ->
        :ignored
    end

    {:stop, :shutdown}
  end

  def opened(:info, _Message, _Data) do
    :keep_state_and_data
  end

  def opened({:call, {owner, _Tag} = from}, [:close], %{:owner => owner} = data) do
    case flush_write_buffer(data) do
      :ok ->
        %{:handle => privateFd} = data
        response = apply(r_file_descriptor(privateFd, :module), :close, [privateFd])
        {:stop_and_reply, :normal, [{:reply, from, response}]}

      other ->
        {:stop_and_reply, :normal, [{:reply, from, other}]}
    end
  end

  def opened({:call, {owner, _Tag} = from}, :"$wait", %{:owner => owner}) do
    {:keep_state_and_data, [{:reply, from, :ok}]}
  end

  def opened({:call, {owner, _Tag} = from}, :"$synchronous_flush", %{:owner => owner} = data) do
    cancel_flush_timeout(data)
    response = flush_write_buffer(data)
    {:keep_state_and_data, [{:reply, from, response}]}
  end

  def opened({:call, {owner, _Tag} = from}, command, %{:owner => owner} = data) do
    response =
      case flush_write_buffer(data) do
        :ok ->
          dispatch_command(data, command)

        other ->
          other
      end

    {:keep_state_and_data, [{:reply, from, response}]}
  end

  def opened({:call, _From}, _Command, _Data) do
    {:shutdown, :protocol_violation}
  end

  def opened(:cast, :"$reset_timeout", %{:delay_time => timeout, :secret => secret} = data) do
    cancel_flush_timeout(data)
    timer = :erlang.send_after(timeout, self(), {:"$timed_out", secret})
    {:keep_state, %{data | :timer => timer}, []}
  end

  def opened(:cast, _Message, _Data) do
    {:keep_state_and_data, []}
  end

  defp dispatch_command(data, [function | args]) do
    %{:handle => handle} = data
    module = r_file_descriptor(handle, :module)
    apply(module, function, [handle | args])
  end

  defp cancel_flush_timeout(%{:timer => :none}) do
    :ok
  end

  defp cancel_flush_timeout(%{:timer => timer}) do
    _ = :erlang.cancel_timer(timer, [{:async, true}])
    :ok
  end

  defp try_flush_write_buffer(%{:buffer => buffer, :handle => privateFd}) do
    case :prim_buffer.try_lock(buffer) do
      :acquired ->
        flush_write_buffer_1(buffer, privateFd)
        :prim_buffer.unlock(buffer)
        :ok

      :busy ->
        :busy
    end
  end

  defp flush_write_buffer(%{:buffer => buffer, :handle => privateFd}) do
    :acquired = :prim_buffer.try_lock(buffer)
    result = flush_write_buffer_1(buffer, privateFd)
    :prim_buffer.unlock(buffer)
    result
  end

  defp flush_write_buffer_1(buffer, privateFd) do
    case :prim_buffer.size(buffer) do
      size when size > 0 ->
        apply(r_file_descriptor(privateFd, :module), :write, [
          privateFd,
          :prim_buffer.read_iovec(buffer, size)
        ])

      0 ->
        :ok
    end
  end

  def terminate(_Reason, _State, _Data) do
    :ok
  end

  def write(fd, iOData) do
    try do
      enqueue_write(fd, :erlang.iolist_to_iovec(iOData))
    catch
      :error, :badarg ->
        {:error, :badarg}
    end
  end

  defp enqueue_write(_Fd, []) do
    :ok
  end

  defp enqueue_write(fd, iOVec) do
    %{:delay_size => delaySize, :buffer => buffer, :pid => pid} = get_fd_data(fd)

    case :prim_buffer.try_lock(buffer) do
      :acquired ->
        enqueue_write_locked(pid, buffer, delaySize, iOVec)

      :busy ->
        :gen_statem.call(pid, :"$wait")
        enqueue_write(fd, iOVec)
    end
  end

  defp enqueue_write_locked(pid, buffer, delaySize, iOVec) do
    bufSize = :prim_buffer.size(buffer)

    case is_iovec_smaller_than(
           iOVec,
           delaySize - bufSize
         ) do
      true when bufSize > 0 ->
        :prim_buffer.write(buffer, iOVec)
        :prim_buffer.unlock(buffer)

      true ->
        :prim_buffer.write(buffer, iOVec)
        :prim_buffer.unlock(buffer)
        :gen_statem.cast(pid, :"$reset_timeout")

      false when bufSize > 0 ->
        :prim_buffer.write(buffer, iOVec)
        :prim_buffer.unlock(buffer)
        :gen_statem.call(pid, :"$synchronous_flush")

      false ->
        :prim_buffer.unlock(buffer)
        :gen_statem.call(pid, [:write, iOVec])
    end
  end

  defp is_iovec_smaller_than(iOVec, max) do
    is_iovec_smaller_than_1(iOVec, max, 0)
  end

  defp is_iovec_smaller_than_1(_IOVec, max, acc) when acc >= max do
    false
  end

  defp is_iovec_smaller_than_1([], _Max, _Acc) do
    true
  end

  defp is_iovec_smaller_than_1([binary | rest], max, acc)
       when is_binary(binary) do
    is_iovec_smaller_than_1(rest, max, acc + byte_size(binary))
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
    %{:pid => pid} = get_fd_data(fd)

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
    %{:owner => owner} = data

    case self() do
      ^owner ->
        data

      _ ->
        :erlang.error(:not_on_controlling_process)
    end
  end
end
