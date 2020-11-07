defmodule :m_tftp_file do
  use Bitwise
  @behaviour :tftp
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

  Record.defrecord(:r_initial, :initial,
    filename: :undefined,
    is_native_ascii: :undefined
  )

  Record.defrecord(:r_state, :state,
    access: :undefined,
    filename: :undefined,
    is_native_ascii: :undefined,
    is_network_ascii: :undefined,
    root_dir: :undefined,
    options: :undefined,
    blksize: :undefined,
    fd: :undefined,
    count: :undefined,
    buffer: :undefined
  )

  def prepare(_Peer, access, filename, mode, suggestedOptions, initial)
      when is_list(initial) do
    case (try do
            handle_options(access, filename, mode, suggestedOptions, initial)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, filename2, isNativeAscii, isNetworkAscii, acceptedOptions} ->
        state =
          r_state(
            access: access,
            filename: filename2,
            is_native_ascii: isNativeAscii,
            is_network_ascii: isNetworkAscii,
            options: acceptedOptions,
            blksize: lookup_blksize(acceptedOptions),
            count: 0,
            buffer: []
          )

        {:ok, acceptedOptions, state}

      {:error, {code, text}} ->
        {:error, {code, text}}
    end
  end

  def open(peer, access, filename, mode, suggestedOptions, initial)
      when is_list(initial) do
    case prepare(peer, access, filename, mode, suggestedOptions, initial) do
      {:ok, acceptedOptions, state} ->
        open(peer, access, filename, mode, acceptedOptions, state)

      {:error, {code, text}} ->
        {:error, {code, text}}
    end
  end

  def open(_Peer, access, filename, mode, negotiatedOptions, state)
      when elem(state, 0) === :state do
    case (try do
            handle_options(access, filename, mode, negotiatedOptions, state)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, _Filename2, _IsNativeAscii, _IsNetworkAscii, options}
      when options === negotiatedOptions ->
        do_open(state)

      {:error, {code, text}} ->
        {:error, {code, text}}
    end
  end

  def open(peer, access, filename, mode, negotiatedOptions, state) do
    state2 = upgrade_state(state)
    open(peer, access, filename, mode, negotiatedOptions, state2)
  end

  defp do_open(state) when elem(state, 0) === :state do
    case :file.open(
           r_state(state, :filename),
           file_options(state)
         ) do
      {:ok, fd} ->
        {:ok, r_state(state, :options), r_state(state, fd: fd)}

      {:error, reason} when is_atom(reason) ->
        {:error, file_error(reason)}
    end
  end

  defp file_options(state) do
    case r_state(state, :access) do
      :read ->
        [:read, :read_ahead, :raw, :binary]

      :write ->
        [:write, :delayed_write, :raw, :binary]
    end
  end

  defp file_error(reason) when is_atom(reason) do
    details = :file.format_error(reason)

    case reason do
      :eexist ->
        {reason, details}

      :enoent ->
        {reason, details}

      :eacces ->
        {reason, details}

      :eperm ->
        {:eacces, details}

      :enospc ->
        {reason, details}

      _ ->
        {:undef, details ++ ' (' ++ :erlang.atom_to_list(reason) ++ ')'}
    end
  end

  def read(r_state(access: :read) = state) do
    blkSize = r_state(state, :blksize)

    case :file.read(r_state(state, :fd), blkSize) do
      {:ok, bin}
      when is_binary(bin) and
             :erlang.size(bin) === blkSize ->
        count = r_state(state, :count) + :erlang.size(bin)
        {:more, bin, r_state(state, count: count)}

      {:ok, bin}
      when is_binary(bin) and
             :erlang.size(bin) < blkSize ->
        _ = :file.close(r_state(state, :fd))
        count = r_state(state, :count) + :erlang.size(bin)
        {:last, bin, count}

      :eof ->
        {:last, <<>>, r_state(state, :count)}

      {:error, reason} ->
        _ = :file.close(r_state(state, :fd))
        {:error, file_error(reason)}
    end
  end

  def read(state) do
    state2 = upgrade_state(state)
    read(state2)
  end

  def write(bin, r_state(access: :write) = state)
      when is_binary(bin) do
    size = :erlang.size(bin)
    blkSize = r_state(state, :blksize)

    case :file.write(r_state(state, :fd), bin) do
      :ok when size === blkSize ->
        count = r_state(state, :count) + size
        {:more, r_state(state, count: count)}

      :ok when size < blkSize ->
        _ = :file.close(r_state(state, :fd))
        count = r_state(state, :count) + size
        {:last, count}

      {:error, reason} ->
        _ = :file.close(r_state(state, :fd))
        _ = :file.delete(r_state(state, :filename))
        {:error, file_error(reason)}
    end
  end

  def write(bin, state) do
    state2 = upgrade_state(state)
    write(bin, state2)
  end

  def abort(_Code, _Text, r_state(fd: fd, access: access) = state) do
    _ = :file.close(fd)

    case access do
      :write ->
        :ok = :file.delete(r_state(state, :filename))

      :read ->
        :ok
    end
  end

  defp handle_options(access, filename, mode, options, initial) do
    i =
      r_initial(
        filename: filename,
        is_native_ascii: is_native_ascii()
      )

    {filename2, isNativeAscii} = handle_initial(initial, i)
    isNetworkAscii = handle_mode(mode, isNativeAscii)
    options2 = do_handle_options(access, filename2, options)
    {:ok, filename2, isNativeAscii, isNetworkAscii, options2}
  end

  defp handle_mode(mode, isNativeAscii) do
    case mode do
      'netascii' when isNativeAscii === true ->
        true

      'octet' ->
        false

      _ ->
        throw({:error, {:badop, 'Illegal mode ' ++ mode}})
    end
  end

  defp handle_initial([{:root_dir, dir} | initial], i) do
    case (try do
            filename_join(dir, r_initial(i, :filename))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        throw({:error, {:badop, 'Internal error. root_dir is not a string'}})

      filename2 ->
        handle_initial(initial, r_initial(i, filename: filename2))
    end
  end

  defp handle_initial([{:native_ascii, bool} | initial], i) do
    case bool do
      true ->
        handle_initial(initial, r_initial(i, is_native_ascii: true))

      false ->
        handle_initial(initial, r_initial(i, is_native_ascii: false))
    end
  end

  defp handle_initial([], i) when elem(i, 0) === :initial do
    {r_initial(i, :filename), r_initial(i, :is_native_ascii)}
  end

  defp handle_initial(state, _) when elem(state, 0) === :state do
    {r_state(state, :filename), r_state(state, :is_native_ascii)}
  end

  defp filename_join(dir, filename) do
    case :filename.pathtype(filename) do
      :absolute ->
        [_ | relFilename] = :filename.split(filename)
        :filename.join([dir, relFilename])

      _ ->
        :filename.join([dir, filename])
    end
  end

  defp do_handle_options(access, filename, [{key, val} | t]) do
    case key do
      'tsize' ->
        case access do
          :read when val === '0' ->
            case :file.read_file_info(filename) do
              {:ok, fI} ->
                tsize = :erlang.integer_to_list(r_file_info(fI, :size))
                [{key, tsize} | do_handle_options(access, filename, t)]

              {:error, _} ->
                do_handle_options(access, filename, t)
            end

          _ ->
            handle_integer(access, filename, key, val, t, 0, :infinity)
        end

      'blksize' ->
        handle_integer(access, filename, key, val, t, 8, 65464)

      'timeout' ->
        handle_integer(access, filename, key, val, t, 1, 255)

      _ ->
        do_handle_options(access, filename, t)
    end
  end

  defp do_handle_options(_Access, _Filename, []) do
    []
  end

  defp handle_integer(access, filename, key, val, options, min, max) do
    case (try do
            :erlang.list_to_integer(val)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        do_handle_options(access, filename, options)

      int when int >= min and int <= max ->
        [{key, val} | do_handle_options(access, filename, options)]

      int when int >= min and max === :infinity ->
        [{key, val} | do_handle_options(access, filename, options)]

      _Int ->
        throw({:error, {:badopt, 'Illegal ' ++ key ++ ' value ' ++ val}})
    end
  end

  defp lookup_blksize(options) do
    case :lists.keysearch('blksize', 1, options) do
      {:value, {_, val}} ->
        :erlang.list_to_integer(val)

      false ->
        512
    end
  end

  defp is_native_ascii() do
    case :os.type() do
      {:win32, _} ->
        true

      _ ->
        false
    end
  end

  defp upgrade_state({:state, access, filename, rootDir, options, blkSize, fd, count, buffer}) do
    {:state, access, filename, false, false, rootDir, options, blkSize, fd, count, buffer}
  end
end
