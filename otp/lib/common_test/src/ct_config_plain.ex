defmodule :m_ct_config_plain do
  use Bitwise

  def read_config(configFile) do
    case :file.consult(configFile) do
      {:ok, config} ->
        {:ok, config}

      {:error, :enoent} ->
        {:error, {:config_file_error, :file.format_error(:enoent)}}

      {:error, reason} ->
        key =
          case :application.get_env(
                 :common_test,
                 :decrypt
               ) do
            {:ok, keyOrFile} ->
              case keyOrFile do
                {:key, k} ->
                  k

                {:file, f} ->
                  :ct_config.get_crypt_key_from_file(f)
              end

            _ ->
              :ct_config.get_crypt_key_from_file()
          end

        case key do
          {:error, :no_crypt_file} ->
            {:error,
             {:config_file_error,
              :lists.flatten(
                :io_lib.format(
                  '~ts',
                  [:file.format_error(reason)]
                )
              )}}

          {:error, cryptError} ->
            {:error, {:decrypt_file_error, cryptError}}

          _ when is_list(key) ->
            case :ct_config.decrypt_config_file(configFile, :undefined, {:key, key}) do
              {:ok, cfgBin} ->
                case read_config_terms(cfgBin) do
                  {:error, readFail} ->
                    {:error, {:config_file_error, readFail}}

                  config ->
                    {:ok, config}
                end

              {:error, decryptFail} ->
                {:error, {:decrypt_config_error, decryptFail}}
            end

          _ ->
            {:error, {:bad_decrypt_key, key}}
        end
    end
  end

  def check_parameter(file) do
    case :filelib.is_file(file) do
      true ->
        {:ok, {:file, file}}

      false ->
        {:error, {:nofile, file}}
    end
  end

  defp read_config_terms(bin) when is_binary(bin) do
    case (try do
            :erlang.binary_to_list(bin)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, :invalid_textfile}

      lines ->
        read_config_terms(lines)
    end
  end

  defp read_config_terms(lines) when is_list(lines) do
    read_config_terms1(:erl_scan.tokens([], lines, 0), 1, [], [])
  end

  defp read_config_terms1({:done, {:ok, ts, eL}, rest}, l, terms, _) do
    case :erl_parse.parse_term(ts) do
      {:ok, term} when rest == [] ->
        :lists.reverse([term | terms])

      {:ok, term} ->
        read_config_terms1(:erl_scan.tokens([], rest, 0), eL + 1, [term | terms], rest)

      _ ->
        {:error, {:bad_term, {l, eL}}}
    end
  end

  defp read_config_terms1({:done, {:eof, _}, _}, _, terms, rest)
       when rest == [] do
    :lists.reverse(terms)
  end

  defp read_config_terms1({:done, {:eof, eL}, _}, l, _, _) do
    {:error, {:bad_term, {l, eL}}}
  end

  defp read_config_terms1({:done, {:error, info, eL}, _}, l, _, _) do
    {:error, {info, {l, eL}}}
  end

  defp read_config_terms1({:more, _}, l, terms, rest) do
    case :string.lexemes(rest, [?\n, [?\r, ?\n], ?\t]) do
      [] ->
        :lists.reverse(terms)

      _ ->
        {:error, {:bad_term, l}}
    end
  end
end
