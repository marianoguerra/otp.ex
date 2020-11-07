defmodule :m_asn1rt_nif do
  use Bitwise
  @on_load :load_nif
  defp load_nif() do
    libBaseName = 'asn1rt_nif'
    privDir = :code.priv_dir(:asn1)

    libName =
      case :erlang.system_info(:build_type) do
        :opt ->
          libBaseName

        type ->
          libTypeName = libBaseName ++ '.' ++ :erlang.atom_to_list(type)

          case :filelib.wildcard(:filename.join([privDir, 'lib', libTypeName ++ '*'])) != [] or
                 :filelib.wildcard(
                   :filename.join([
                     privDir,
                     'lib',
                     :erlang.system_info(:system_architecture),
                     libTypeName ++ '*'
                   ])
                 ) != [] do
            true ->
              libTypeName

            false ->
              libBaseName
          end
      end

    lib = :filename.join([privDir, 'lib', libName])

    status =
      case :erlang.load_nif(lib, 1) do
        :ok ->
          :ok

        {:error, {:load_failed, _}} = error1 ->
          archLibDir = :filename.join([privDir, 'lib', :erlang.system_info(:system_architecture)])
          candidate = :filelib.wildcard(:filename.join([archLibDir, libName ++ '*']))

          case candidate do
            [] ->
              error1

            _ ->
              archLib = :filename.join([archLibDir, libName])
              :erlang.load_nif(archLib, 1)
          end

        error1 ->
          error1
      end

    case status do
      :ok ->
        :ok

      {:error, {e, str}} ->
        :error_logger.error_msg(
          'Unable to load asn1 nif library. Failed with error:~n"~p, ~s"~n',
          [e, str]
        )

        status
    end
  end

  def decode_ber_tlv(binary) do
    case decode_ber_tlv_raw(binary) do
      {:error, reason} ->
        exit({:error, {:asn1, reason}})

      other ->
        other
    end
  end

  def encode_per_complete(tagValueList) do
    case encode_per_complete_raw(tagValueList) do
      {:error, reason} ->
        handle_error(reason, tagValueList)

      other when is_binary(other) ->
        other
    end
  end

  defp handle_error([], _) do
    exit({:error, {:asn1, :enomem}})
  end

  defp handle_error(?1, l) do
    exit({:error, {:asn1, l}})
  end

  defp handle_error(errL, l) do
    exit({:error, {:asn1, errL, l}})
  end

  defp encode_per_complete_raw(_TagValueList) do
    :erlang.nif_error({:nif_not_loaded, :module, :asn1rt_nif, :line, 104})
  end

  defp decode_ber_tlv_raw(_Binary) do
    :erlang.nif_error({:nif_not_loaded, :module, :asn1rt_nif, :line, 107})
  end

  def encode_ber_tlv(_TagValueList) do
    :erlang.nif_error({:nif_not_loaded, :module, :asn1rt_nif, :line, 110})
  end
end
