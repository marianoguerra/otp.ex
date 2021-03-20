defmodule :m_raw_file_io_deflate do
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
    :ok = :zlib.deflateInit(z, :default, :deflated, 16 + 15, 8, :default)
    data = %{owner: owner, monitor: monitor, secret: secret, position: 0, zlib: z}
    {:ok, :opening, data}
  end

  def opening({:call, from}, {:"$open", secret, filename, modes}, %{secret: secret} = data) do
    case :raw_file_io.open(filename, modes) do
      {:ok, privateFd} ->
        newData = Map.put(data, :handle, privateFd)
        {:next_state, :opened, newData, [{:reply, from, :ok}]}

      other ->
        {:stop_and_reply, :normal, [{:reply, from, other}]}
    end
  end

  def opening(_Event, _Contents, _Data) do
    {:keep_state_and_data, [:postpone]}
  end

  def opened(
        :info,
        {:DOWN, monitor, :process, _Owner, reason},
        %{monitor: monitor} = data
      ) do
    cond do
      reason !== :kill ->
        flush_deflate_state(data)

      reason === :kill ->
        :ignored
    end

    {:stop, :shutdown}
  end

  def opened(:info, _Message, _Data) do
    :keep_state_and_data
  end

  def opened({:call, {owner, _Tag} = from}, [:close], %{owner: owner} = data) do
    %{handle: privateFd} = data

    response =
      case flush_deflate_state(data) do
        :ok ->
          apply(r_file_descriptor(privateFd, :module), :close, [privateFd])

        other ->
          other
      end

    {:stop_and_reply, :normal, [{:reply, from, response}]}
  end

  def opened({:call, {owner, _Tag} = from}, [:position, mark], %{owner: owner} = data) do
    case position(data, mark) do
      {:ok, newData, result} ->
        response = {:ok, result}
        {:keep_state, newData, [{:reply, from, response}]}

      other ->
        {:keep_state_and_data, [{:reply, from, other}]}
    end
  end

  def opened({:call, {owner, _Tag} = from}, [:write, iOVec], %{owner: owner} = data) do
    case write(data, iOVec) do
      {:ok, newData} ->
        {:keep_state, newData, [{:reply, from, :ok}]}

      other ->
        {:keep_state_and_data, [{:reply, from, other}]}
    end
  end

  def opened({:call, {owner, _Tag} = from}, [:read, _Size], %{owner: owner}) do
    response = {:error, :ebadf}
    {:keep_state_and_data, [{:reply, from, response}]}
  end

  def opened({:call, {owner, _Tag} = from}, [:read_line], %{owner: owner}) do
    response = {:error, :ebadf}
    {:keep_state_and_data, [{:reply, from, response}]}
  end

  def opened({:call, {owner, _Tag} = from}, _Command, %{owner: owner}) do
    response = {:error, :enotsup}
    {:keep_state_and_data, [{:reply, from, response}]}
  end

  def opened({:call, _From}, _Command, _Data) do
    {:shutdown, :protocol_violation}
  end

  def opened(_Event, _Request, _Data) do
    :keep_state_and_data
  end

  defp write(data, iOVec) do
    %{handle: privateFd, position: position, zlib: z} = data
    uncompressedSize = :erlang.iolist_size(iOVec)

    case apply(r_file_descriptor(privateFd, :module), :write, [privateFd, :zlib.deflate(z, iOVec)]) do
      :ok ->
        {:ok, %{data | position: position + uncompressedSize}}

      other ->
        other
    end
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

  defp position(_Data, _Any) do
    {:error, :badarg}
  end

  defp position_1(%{position: desired} = data, desired) do
    {:ok, data, desired}
  end

  defp position_1(%{position: current} = data, desired)
       when current < desired do
    bytesToWrite = min(desired - current, 4 <<< 20)

    case write(
           data,
           <<0::size(bytesToWrite)-unit(8)>>
         ) do
      {:ok, newData} ->
        position_1(newData, desired)

      other ->
        other
    end
  end

  defp position_1(%{position: current}, desired)
       when current > desired do
    {:error, :einval}
  end

  defp flush_deflate_state(%{handle: privateFd, zlib: z}) do
    case apply(r_file_descriptor(privateFd, :module), :write, [
           privateFd,
           :zlib.deflate(z, [], :finish)
         ]) do
      :ok ->
        :ok

      other ->
        other
    end
  end

  def terminate(_Reason, _State, _Data) do
    :ok
  end
end
