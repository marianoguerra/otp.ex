defmodule :m_tftp_binary do
  use Bitwise
  @behaviour :tftp
  require Record

  Record.defrecord(:r_read_state, :read_state,
    options: :undefined,
    blksize: :undefined,
    bin: :undefined,
    is_native_ascii: :undefined,
    is_network_ascii: :undefined,
    count: :undefined
  )

  Record.defrecord(:r_write_state, :write_state,
    options: :undefined,
    blksize: :undefined,
    list: :undefined,
    is_native_ascii: :undefined,
    is_network_ascii: :undefined
  )

  def prepare(_Peer, access, filename, mode, suggestedOptions, initial)
      when is_list(initial) do
    isNativeAscii = is_native_ascii(initial)

    case (try do
            handle_options(access, filename, mode, suggestedOptions, isNativeAscii)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, isNetworkAscii, acceptedOptions}
      when access === :read and is_binary(filename) ->
        state =
          r_read_state(
            options: acceptedOptions,
            blksize: lookup_blksize(acceptedOptions),
            bin: filename,
            is_network_ascii: isNetworkAscii,
            count: :erlang.size(filename),
            is_native_ascii: isNativeAscii
          )

        {:ok, acceptedOptions, state}

      {:ok, isNetworkAscii, acceptedOptions}
      when access === :write and filename === :binary ->
        state =
          r_write_state(
            options: acceptedOptions,
            blksize: lookup_blksize(acceptedOptions),
            list: [],
            is_network_ascii: isNetworkAscii,
            is_native_ascii: isNativeAscii
          )

        {:ok, acceptedOptions, state}

      {:ok, _, _} ->
        {:error, {:undef, 'Illegal callback usage. Mode and filename is incompatible.'}}

      {:error, {code, text}} ->
        {:error, {code, text}}
    end
  end

  def prepare(_Peer, _Access, _Bin, _Mode, _SuggestedOptions, _Initial) do
    {:error, {:undef, 'Illegal callback options.'}}
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
      when elem(state, 0) === :read_state do
    case (try do
            handle_options(
              access,
              filename,
              mode,
              negotiatedOptions,
              r_read_state(state, :is_native_ascii)
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, isNetworkAscii, options}
      when options === negotiatedOptions and
             isNetworkAscii === r_read_state(state, :is_network_ascii) ->
        {:ok, negotiatedOptions, state}

      {:error, {code, text}} ->
        {:error, {code, text}}
    end
  end

  def open(_Peer, access, filename, mode, negotiatedOptions, state)
      when elem(state, 0) === :write_state do
    case (try do
            handle_options(
              access,
              filename,
              mode,
              negotiatedOptions,
              r_write_state(state, :is_native_ascii)
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, isNetworkAscii, options}
      when options === negotiatedOptions and
             isNetworkAscii === r_write_state(state, :is_network_ascii) ->
        {:ok, negotiatedOptions, state}

      {:error, {code, text}} ->
        {:error, {code, text}}
    end
  end

  def open(peer, access, filename, mode, negotiatedOptions, state) do
    state2 = upgrade_state(state)
    open(peer, access, filename, mode, negotiatedOptions, state2)
  end

  def read(r_read_state(bin: bin) = state) when is_binary(bin) do
    blkSize = r_read_state(state, :blksize)

    cond do
      :erlang.size(bin) >= blkSize ->
        <<block::size(blkSize)-binary, bin2::binary>> = bin
        state2 = r_read_state(state, bin: bin2)
        {:more, block, state2}

      :erlang.size(bin) < blkSize ->
        {:last, bin, r_read_state(state, :count)}
    end
  end

  def read(state) do
    state2 = upgrade_state(state)
    read(state2)
  end

  def write(bin, r_write_state(list: list) = state)
      when is_binary(bin) and is_list(list) do
    size = :erlang.size(bin)
    blkSize = r_write_state(state, :blksize)

    cond do
      size === blkSize ->
        {:more, r_write_state(state, list: [bin | list])}

      size < blkSize ->
        bin2 =
          :erlang.list_to_binary(
            :lists.reverse([
              bin
              | list
            ])
          )

        {:last, bin2}
    end
  end

  def write(bin, state) do
    state2 = upgrade_state(state)
    write(bin, state2)
  end

  def abort(_Code, _Text, r_read_state(bin: bin) = state)
      when elem(state, 0) === :read_state and
             is_binary(bin) do
    :ok
  end

  def abort(_Code, _Text, r_write_state(list: list) = state)
      when elem(state, 0) === :write_state and
             is_list(list) do
    :ok
  end

  def abort(code, text, state) do
    state2 = upgrade_state(state)
    abort(code, text, state2)
  end

  defp handle_options(access, bin, mode, options, isNativeAscii) do
    isNetworkAscii = handle_mode(mode, isNativeAscii)
    options2 = do_handle_options(access, bin, options)
    {:ok, isNetworkAscii, options2}
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

  defp do_handle_options(access, bin, [{key, val} | t]) do
    case key do
      'tsize' ->
        case access do
          :read when val === '0' and is_binary(bin) ->
            tsize = :erlang.integer_to_list(:erlang.size(bin))
            [{key, tsize} | do_handle_options(access, bin, t)]

          _ ->
            handle_integer(access, bin, key, val, t, 0, :infinity)
        end

      'blksize' ->
        handle_integer(access, bin, key, val, t, 8, 65464)

      'timeout' ->
        handle_integer(access, bin, key, val, t, 1, 255)

      _ ->
        do_handle_options(access, bin, t)
    end
  end

  defp do_handle_options(_Access, _Bin, []) do
    []
  end

  defp handle_integer(access, bin, key, val, options, min, max) do
    case (try do
            :erlang.list_to_integer(val)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        do_handle_options(access, bin, options)

      int when int >= min and int <= max ->
        [{key, val} | do_handle_options(access, bin, options)]

      int when int >= min and max === :infinity ->
        [{key, val} | do_handle_options(access, bin, options)]

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

  defp is_native_ascii([]) do
    is_native_ascii()
  end

  defp is_native_ascii([{:native_ascii, bool}]) do
    case bool do
      true ->
        true

      false ->
        false
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

  defp upgrade_state({:read_state, options, blksize, bin, isNetworkAscii, count}) do
    {:read_state, options, blksize, bin, false, isNetworkAscii, count}
  end

  defp upgrade_state({:write_state, options, blksize, list, isNetworkAscii}) do
    {:write_state, options, blksize, list, false, isNetworkAscii}
  end
end
