defmodule :m_ram_file do
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

  def open(data, modeList) when is_list(modeList) do
    case open_mode(modeList) do
      {mode, opts} when is_integer(mode) ->
        case ll_open(data, mode, opts) do
          {:ok, port} ->
            {:ok, r_file_descriptor(module: :ram_file, data: port)}

          error ->
            error
        end

      {:error, _} = error ->
        error
    end
  end

  def close(r_file_descriptor(module: :ram_file, data: port)) do
    ll_close(port)
  end

  def read(r_file_descriptor(module: :ram_file, data: port), sz)
      when is_integer(sz) and sz >= 0 do
    cond do
      is_integer(sz) and sz >= -(1 <<< 31) and
          sz < 1 <<< 31 ->
        cmd = <<2::size(8), sz::size(32)>>

        case call_port(port, cmd) do
          {:ok, {0, _Data}} when sz !== 0 ->
            :eof

          {:ok, {_Sz, data}} ->
            {:ok, data}

          {:error, :enomem} ->
            :erlang.garbage_collect()

            case call_port(port, cmd) do
              {:ok, {0, _Data}} when sz !== 0 ->
                :eof

              {:ok, {_Sz, data}} ->
                {:ok, data}

              error ->
                error
            end

          error ->
            error
        end

      true ->
        {:error, :einval}
    end
  end

  def write(r_file_descriptor(module: :ram_file, data: port), bytes) do
    case call_port(port, [4 | bytes]) do
      {:ok, _Sz} ->
        :ok

      error ->
        error
    end
  end

  def copy(
        r_file_descriptor(module: :ram_file) = source,
        r_file_descriptor(module: :ram_file) = dest,
        length
      )
      when (is_integer(length) and length >= 0) or
             is_atom(length) do
    :file.copy_opened(source, dest, length)
  end

  def datasync(r_file_descriptor(module: :ram_file, data: port)) do
    call_port(port, <<19>>)
  end

  def sync(r_file_descriptor(module: :ram_file, data: port)) do
    call_port(port, <<9>>)
  end

  def truncate(r_file_descriptor(module: :ram_file, data: port)) do
    call_port(port, <<14>>)
  end

  def position(r_file_descriptor(module: :ram_file, data: port), pos) do
    case lseek_position(pos) do
      {:ok, offs, whence}
      when is_integer(offs) and
             offs >= -(1 <<< 31) and offs < 1 <<< 31 ->
        call_port(
          port,
          <<3::size(8), offs::size(32), whence::size(32)>>
        )

      {:ok, _, _} ->
        {:error, :einval}

      error ->
        error
    end
  end

  def pread(r_file_descriptor(module: :ram_file, data: port), l)
      when is_list(l) do
    pread_1(port, l, [])
  end

  defp pread_1(port, [], cs) do
    pread_2(port, :lists.reverse(cs), [])
  end

  defp pread_1(port, [{at, sz} | t], cs)
       when is_integer(at) and is_integer(sz) and sz >= 0 do
    cond do
      is_integer(at) and at >= -(1 <<< 31) and
        at < 1 <<< 31 and is_integer(sz) and
        sz >= -(1 <<< 31) and sz < 1 <<< 31 ->
        pread_1(port, t, [
          {sz, <<17::size(8), at::size(32), sz::size(32)>>}
          | cs
        ])

      true ->
        {:error, :einval}
    end
  end

  defp pread_1(_, _, _243) do
    {:error, :badarg}
  end

  defp pread_2(_Port, [], r) do
    {:ok, :lists.reverse(r)}
  end

  defp pread_2(port, [{sz, command} | commands], r) do
    case call_port(port, command) do
      {:ok, {0, _Data}} when sz !== 0 ->
        pread_2(port, commands, [:eof | r])

      {:ok, {_Sz, data}} ->
        pread_2(port, commands, [data | r])

      error ->
        error
    end
  end

  def pread(r_file_descriptor(module: :ram_file, data: port), at, sz)
      when is_integer(at) and is_integer(sz) and sz >= 0 do
    cond do
      is_integer(at) and at >= -(1 <<< 31) and
        at < 1 <<< 31 and is_integer(sz) and
        sz >= -(1 <<< 31) and sz < 1 <<< 31 ->
        case call_port(
               port,
               <<17::size(8), at::size(32), sz::size(32)>>
             ) do
          {:ok, {0, _Data}} when sz !== 0 ->
            :eof

          {:ok, {_Sz, data}} ->
            {:ok, data}

          error ->
            error
        end

      true ->
        {:error, :einval}
    end
  end

  def pread(r_file_descriptor(module: :ram_file), _, _) do
    {:error, :badarg}
  end

  def pwrite(r_file_descriptor(module: :ram_file, data: port), l)
      when is_list(l) do
    pwrite_1(port, l, 0, [])
  end

  defp pwrite_1(port, [], _, cs) do
    pwrite_2(port, :lists.reverse(cs), 0)
  end

  defp pwrite_1(port, [{at, bytes} | t], r, cs)
       when is_integer(at) do
    cond do
      is_integer(at) and at >= -(1 <<< 31) and
        at < 1 <<< 31 and is_binary(bytes) ->
        pwrite_1(port, t, r + 1, [
          <<18::size(8), at::size(32), bytes::binary>>
          | cs
        ])

      is_integer(at) and at >= -(1 <<< 31) and
          at < 1 <<< 31 ->
        try do
          :erlang.iolist_to_binary(bytes)
        catch
          :error, reason ->
            {:error, reason}
        else
          bin ->
            pwrite_1(port, t, r + 1, [<<18::size(8), at::size(32), bin::binary>> | cs])
        end

      true ->
        {:error, {r, :einval}}
    end
  end

  defp pwrite_1(_, _, _, _) do
    {:error, :badarg}
  end

  defp pwrite_2(_Port, [], _R) do
    :ok
  end

  defp pwrite_2(port, [command | commands], r) do
    case call_port(port, command) do
      {:ok, _Sz} ->
        pwrite_2(port, commands, r + 1)

      {:error, :badarg} = error ->
        error

      {:error, reason} ->
        {:error, {r, reason}}
    end
  end

  def pwrite(r_file_descriptor(module: :ram_file, data: port), at, bytes)
      when is_integer(at) do
    cond do
      is_integer(at) and at >= -(1 <<< 31) and
          at < 1 <<< 31 ->
        case call_port(
               port,
               [<<18::size(8), at::size(32)>> | bytes]
             ) do
          {:ok, _Sz} ->
            :ok

          error ->
            error
        end

      true ->
        {:error, :einval}
    end
  end

  def pwrite(r_file_descriptor(module: :ram_file), _, _) do
    {:error, :badarg}
  end

  def ipread_s32bu_p32bu(r_file_descriptor(module: :ram_file) = handle, pos, maxSz) do
    :file.ipread_s32bu_p32bu_int(handle, pos, maxSz)
  end

  def get_file(r_file_descriptor(module: :ram_file, data: port)) do
    case call_port(port, [30]) do
      {:ok, {_Sz, data}} ->
        {:ok, data}

      error ->
        error
    end
  end

  def get_file(r_file_descriptor()) do
    {:error, :enotsup}
  end

  def get_size(r_file_descriptor(module: :ram_file, data: port)) do
    call_port(port, [37])
  end

  def get_size(r_file_descriptor()) do
    {:error, :enotsup}
  end

  def advise(r_file_descriptor(module: :ram_file, data: port), offset, length, advise) do
    cmd0 = <<38, offset::size(64)-signed, length::size(64)-signed>>

    case advise do
      :normal ->
        call_port(
          port,
          <<cmd0::binary, 0::size(32)-signed>>
        )

      :random ->
        call_port(
          port,
          <<cmd0::binary, 1::size(32)-signed>>
        )

      :sequential ->
        call_port(
          port,
          <<cmd0::binary, 2::size(32)-signed>>
        )

      :will_need ->
        call_port(
          port,
          <<cmd0::binary, 3::size(32)-signed>>
        )

      :dont_need ->
        call_port(
          port,
          <<cmd0::binary, 4::size(32)-signed>>
        )

      :no_reuse ->
        call_port(
          port,
          <<cmd0::binary, 5::size(32)-signed>>
        )

      _ ->
        {:error, :einval}
    end
  end

  def advise(r_file_descriptor(), _Offset, _Length, _Advise) do
    {:error, :enotsup}
  end

  def allocate(r_file_descriptor(module: :ram_file, data: port), offset, length) do
    call_port(
      port,
      <<39, offset::size(64)-signed, length::size(64)-signed>>
    )
  end

  def allocate(r_file_descriptor(), _Offset, _Length) do
    {:error, :enotsup}
  end

  defp ll_open(data, mode, opts) do
    try do
      :erlang.open_port({:spawn, 'ram_file_drv'}, opts)
    catch
      :error, reason ->
        {:error, reason}
    else
      port ->
        case call_port(
               port,
               [<<1::size(8), mode::size(32)>> | data]
             ) do
          {:error, _} = error ->
            ll_close(port)
            error

          {:ok, _} ->
            {:ok, port}
        end
    end
  end

  defp call_port(port, command)
       when is_port(port) and
              is_binary(command) do
    try do
      :erlang.port_command(port, command)
    catch
      :error, :badarg ->
        {:error, :einval}

      :error, reason ->
        {:error, reason}
    else
      true ->
        get_response(port)
    end
  end

  defp call_port(port, command) do
    try do
      :erlang.iolist_to_binary(command)
    catch
      :error, reason ->
        {:error, reason}
    else
      bin ->
        call_port(port, bin)
    end
  end

  defp get_response(port) do
    receive do
      {^port, {:data, [response | rest]}} ->
        translate_response(response, rest)

      {:EXIT, ^port, _Reason} ->
        {:error, :port_died}
    end
  end

  defp ll_close(port) do
    try do
      :erlang.port_close(port)
    catch
      :error, _ ->
        :ok
    end

    receive do
      {:EXIT, ^port, _} ->
        :ok
    after
      0 ->
        :ok
    end
  end

  defp open_mode(list) when is_list(list) do
    case open_mode(list, {0, []}) do
      {mode, opts} when mode &&& (1 ||| 2) === 0 ->
        {mode ||| 1, opts}

      other ->
        other
    end
  end

  defp open_mode([:ram | rest], {mode, opts}) do
    open_mode(rest, {mode, opts})
  end

  defp open_mode([:read | rest], {mode, opts}) do
    open_mode(rest, {mode ||| 1, opts})
  end

  defp open_mode([:write | rest], {mode, opts}) do
    open_mode(rest, {mode ||| 2, opts})
  end

  defp open_mode([:binary | rest], {mode, opts}) do
    open_mode(rest, {mode, [:binary | opts]})
  end

  defp open_mode([], {mode, opts}) do
    {mode, opts}
  end

  defp open_mode(_, _) do
    {:error, :badarg}
  end

  defp lseek_position(pos) when is_integer(pos) do
    lseek_position({:bof, pos})
  end

  defp lseek_position(:bof) do
    lseek_position({:bof, 0})
  end

  defp lseek_position(:cur) do
    lseek_position({:cur, 0})
  end

  defp lseek_position(:eof) do
    lseek_position({:eof, 0})
  end

  defp lseek_position({:bof, offset}) when is_integer(offset) do
    {:ok, offset, 0}
  end

  defp lseek_position({:cur, offset}) when is_integer(offset) do
    {:ok, offset, 1}
  end

  defp lseek_position({:eof, offset}) when is_integer(offset) do
    {:ok, offset, 2}
  end

  defp lseek_position(_) do
    {:error, :badarg}
  end

  defp translate_response(0, []) do
    :ok
  end

  defp translate_response(0, data) do
    {:ok, data}
  end

  defp translate_response(1, list) when is_list(list) do
    {:error, :erlang.list_to_atom(list)}
  end

  defp translate_response(3, [x1, x2, x3, x4]) do
    {:ok, i32(x1, x2, x3, x4)}
  end

  defp translate_response(2, [[x1, x2, x3, x4] | data]) do
    {:ok, {i32(x1, x2, x3, x4), data}}
  end

  defp translate_response(x, data) do
    {:error, {:bad_response_from_port, x, data}}
  end

  defp i32(x1, x2, x3, x4) do
    x1 <<< 24 ||| x2 <<< 16 ||| x3 <<< 8 ||| x4
  end
end
