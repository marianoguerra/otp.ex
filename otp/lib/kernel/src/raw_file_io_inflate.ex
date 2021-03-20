defmodule :m_raw_file_io_inflate do
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

  def callback_mode() do
    :state_functions
  end

  def init({owner, secret, [:compressed]}) do
    monitor = :erlang.monitor(:process, owner)
    z = :zlib.open()
    :ok = :zlib.inflateInit(z, 16 + 15, :reset)

    data = %{
      owner: owner,
      monitor: monitor,
      secret: secret,
      position: 0,
      buffer: :prim_buffer.new(),
      zlib: z
    }

    {:ok, :opening, data}
  end

  defp choose_decompression_state(privateFd) do
    state =
      case apply(r_file_descriptor(privateFd, :module), :read, [privateFd, 2]) do
        {:ok, <<31, 139>>} ->
          :opened_gzip

        _Other ->
          :opened_passthrough
      end

    {:ok, 0} = apply(r_file_descriptor(privateFd, :module), :position, [privateFd, 0])
    state
  end

  def opening({:call, from}, {:"$open", secret, filename, modes}, %{secret: secret} = data) do
    case :raw_file_io.open(filename, modes) do
      {:ok, privateFd} ->
        nextState = choose_decompression_state(privateFd)
        newData = Map.put(data, :handle, privateFd)
        {:next_state, nextState, newData, [{:reply, from, :ok}]}

      other ->
        {:stop_and_reply, :normal, [{:reply, from, other}]}
    end
  end

  def opening(_Event, _Contents, _Data) do
    {:keep_state_and_data, [:postpone]}
  end

  defp internal_close(from, data) do
    %{handle: privateFd} = data
    response = apply(r_file_descriptor(privateFd, :module), :close, [privateFd])
    {:stop_and_reply, :normal, [{:reply, from, response}]}
  end

  def opened_passthrough(
        :info,
        {:DOWN, monitor, :process, _Owner, _Reason},
        %{monitor: monitor}
      ) do
    {:stop, :shutdown}
  end

  def opened_passthrough(:info, _Message, _Data) do
    :keep_state_and_data
  end

  def opened_passthrough({:call, {owner, _Tag} = from}, [:close], %{owner: owner} = data) do
    internal_close(from, data)
  end

  def opened_passthrough({:call, {owner, _Tag} = from}, [method | args], %{owner: owner} = data) do
    %{handle: privateFd} = data
    response = apply(r_file_descriptor(privateFd, :module), method, [privateFd | args])
    {:keep_state_and_data, [{:reply, from, response}]}
  end

  def opened_passthrough({:call, _From}, _Command, _Data) do
    {:shutdown, :protocol_violation}
  end

  def opened_passthrough(_Event, _Request, _Data) do
    :keep_state_and_data
  end

  def opened_gzip(
        :info,
        {:DOWN, monitor, :process, _Owner, _Reason},
        %{monitor: monitor}
      ) do
    {:stop, :shutdown}
  end

  def opened_gzip(:info, _Message, _Data) do
    :keep_state_and_data
  end

  def opened_gzip({:call, {owner, _Tag} = from}, [:close], %{owner: owner} = data) do
    internal_close(from, data)
  end

  def opened_gzip({:call, {owner, _Tag} = from}, [:position, mark], %{owner: owner} = data) do
    case position(data, mark) do
      {:ok, newData, result} ->
        response = {:ok, result}
        {:keep_state, newData, [{:reply, from, response}]}

      other ->
        {:keep_state_and_data, [{:reply, from, other}]}
    end
  end

  def opened_gzip({:call, {owner, _Tag} = from}, [:read, size], %{owner: owner} = data) do
    case read(data, size) do
      {:ok, newData, result} ->
        response = {:ok, result}
        {:keep_state, newData, [{:reply, from, response}]}

      other ->
        {:keep_state_and_data, [{:reply, from, other}]}
    end
  end

  def opened_gzip({:call, {owner, _Tag} = from}, [:read_line], %{owner: owner} = data) do
    case read_line(data) do
      {:ok, newData, result} ->
        response = {:ok, result}
        {:keep_state, newData, [{:reply, from, response}]}

      other ->
        {:keep_state_and_data, [{:reply, from, other}]}
    end
  end

  def opened_gzip({:call, {owner, _Tag} = from}, [:write, _IOData], %{owner: owner}) do
    response = {:error, :ebadf}
    {:keep_state_and_data, [{:reply, from, response}]}
  end

  def opened_gzip({:call, {owner, _Tag} = from}, _Request, %{owner: owner}) do
    response = {:error, :enotsup}
    {:keep_state_and_data, [{:reply, from, response}]}
  end

  def opened_gzip({:call, _From}, _Request, _Data) do
    {:shutdown, :protocol_violation}
  end

  def opened_gzip(_Event, _Request, _Data) do
    :keep_state_and_data
  end

  defp read(%{buffer: buffer} = data, size) do
    try do
      read_1(data, buffer, :prim_buffer.size(buffer), size)
    catch
      :error, :badarg ->
        {:error, :badarg}

      :error, _ ->
        {:error, :eio}
    else
      result ->
        result
    end
  end

  defp read_1(data, buffer, bufferSize, readSize)
       when bufferSize >= readSize do
    %{position: position} = data
    decompressed = :prim_buffer.read(buffer, readSize)
    {:ok, Map.put(data, :position, position + readSize), decompressed}
  end

  defp read_1(data, buffer, bufferSize, readSize)
       when bufferSize < readSize do
    %{handle: privateFd} = data

    case apply(r_file_descriptor(privateFd, :module), :read, [privateFd, 8 <<< 10]) do
      {:ok, compressed} ->
        %{zlib: z} = data

        uncompressed =
          :erlang.iolist_to_iovec(
            :zlib.inflate(
              z,
              compressed
            )
          )

        :prim_buffer.write(buffer, uncompressed)
        read_1(data, buffer, :prim_buffer.size(buffer), readSize)

      :eof when bufferSize > 0 ->
        read_1(data, buffer, bufferSize, bufferSize)

      other ->
        other
    end
  end

  defp read_line(%{buffer: buffer} = data) do
    try do
      read_line_1(data, buffer, :prim_buffer.find_byte_index(buffer, ?\n))
    catch
      :error, :badarg ->
        {:error, :badarg}

      :error, _ ->
        {:error, :eio}
    else
      {:ok, newData, decompressed} ->
        {:ok, newData, decompressed}

      other ->
        other
    end
  end

  defp read_line_1(data, buffer, :not_found) do
    %{handle: privateFd, zlib: z} = data

    case apply(r_file_descriptor(privateFd, :module), :read, [privateFd, 8 <<< 10]) do
      {:ok, compressed} ->
        uncompressed =
          :erlang.iolist_to_iovec(
            :zlib.inflate(
              z,
              compressed
            )
          )

        :prim_buffer.write(buffer, uncompressed)
        read_line_1(data, buffer, :prim_buffer.find_byte_index(buffer, ?\n))

      :eof ->
        case :prim_buffer.size(buffer) do
          size when size > 0 ->
            {:ok, :prim_buffer.read(buffer, size)}

          size when size === 0 ->
            :eof
        end

      error ->
        error
    end
  end

  defp read_line_1(data, buffer, {:ok, lFIndex}) do
    %{position: position} = data
    newData = Map.put(data, :position, position + lFIndex + 1)
    cRIndex = lFIndex - 1

    translatedLine =
      case :prim_buffer.read(
             buffer,
             lFIndex + 1
           ) do
        <<line::size(cRIndex)-binary, "\r\n">> ->
          <<line::binary, "\n">>

        line ->
          line
      end

    {:ok, newData, translatedLine}
  end

  defp position(data, mark) when is_atom(mark) do
    position(data, {mark, 0})
  end

  defp position(data, offset) when is_integer(offset) do
    position(data, {:bof, offset})
  end

  defp position(data, {:bof, offset}) when is_integer(offset) do
    position_1(data, offset)
  end

  defp position(data, {:cur, offset}) when is_integer(offset) do
    %{position: position} = data
    position_1(data, position + offset)
  end

  defp position(_Data, {:eof, offset})
       when is_integer(offset) do
    {:error, :einval}
  end

  defp position(_Data, _Other) do
    {:error, :badarg}
  end

  defp position_1(_Data, desired) when desired < 0 do
    {:error, :einval}
  end

  defp position_1(%{position: desired} = data, desired) do
    {:ok, data, desired}
  end

  defp position_1(%{position: current} = data, desired)
       when current < desired do
    case read(data, min(desired - current, 8 <<< 10)) do
      {:ok, newData, _Data} ->
        position_1(newData, desired)

      :eof ->
        {:ok, data, current}

      other ->
        other
    end
  end

  defp position_1(%{position: current} = data, desired)
       when current > desired do
    %{handle: privateFd, buffer: buffer, zlib: z} = data

    case apply(r_file_descriptor(privateFd, :module), :position, [privateFd, :bof]) do
      {:ok, 0} ->
        :ok = :zlib.inflateReset(z)
        :prim_buffer.wipe(buffer)
        position_1(Map.put(data, :position, 0), desired)

      other ->
        other
    end
  end

  def terminate(_Reason, _State, _Data) do
    :ok
  end
end
