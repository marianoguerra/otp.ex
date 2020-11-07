defmodule :m_unicode do
  use Bitwise

  def bin_is_7bit(_) do
    :erlang.nif_error(:undef)
  end

  def characters_to_binary(_, _) do
    :erlang.nif_error(:undef)
  end

  def characters_to_list(_, _) do
    :erlang.nif_error(:undef)
  end

  def characters_to_list(mL) do
    :unicode.characters_to_list(mL, :unicode)
  end

  def characters_to_binary(mL) do
    try do
      :unicode.characters_to_binary(mL, :unicode)
    catch
      :error, anyError ->
        theError =
          case anyError do
            :system_limit ->
              :system_limit

            _ ->
              :badarg
          end

        {:EXIT, {:new_stacktrace, [{mod, _, l, _} | rest]}} =
          try do
            :erlang.error(
              :new_stacktrace,
              [mL]
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        :erlang.raise(:error, theError, [{mod, :characters_to_binary, l} | rest])
    end
  end

  def characters_to_binary(mL, :latin1, :latin1) when is_binary(mL) do
    mL
  end

  def characters_to_binary(mL, :latin1, uni)
      when is_binary(mL) and (uni === :utf8 or uni === :unicode) do
    case :unicode.bin_is_7bit(mL) do
      true ->
        mL

      false ->
        try do
          characters_to_binary_int(mL, :latin1, :utf8)
        catch
          :error, anyError ->
            theError =
              case anyError do
                :system_limit ->
                  :system_limit

                _ ->
                  :badarg
              end

            {:EXIT, {:new_stacktrace, [{mod, _, l, _} | rest]}} =
              try do
                :erlang.error(
                  :new_stacktrace,
                  [mL, :latin1, uni]
                )
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end

            :erlang.raise(:error, theError, [{mod, :characters_to_binary, l} | rest])
        end
    end
  end

  def characters_to_binary(mL, uni, :latin1)
      when is_binary(mL) and (uni === :utf8 or uni === :unicode) do
    case :unicode.bin_is_7bit(mL) do
      true ->
        mL

      false ->
        try do
          characters_to_binary_int(mL, :utf8, :latin1)
        catch
          :error, anyError ->
            theError =
              case anyError do
                :system_limit ->
                  :system_limit

                _ ->
                  :badarg
              end

            {:EXIT, {:new_stacktrace, [{mod, _, l, _} | rest]}} =
              try do
                :erlang.error(
                  :new_stacktrace,
                  [mL, uni, :latin1]
                )
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end

            :erlang.raise(:error, theError, [{mod, :characters_to_binary, l} | rest])
        end
    end
  end

  def characters_to_binary(mL, inEncoding, outEncoding) do
    try do
      characters_to_binary_int(mL, inEncoding, outEncoding)
    catch
      :error, anyError ->
        theError =
          case anyError do
            :system_limit ->
              :system_limit

            _ ->
              :badarg
          end

        {:EXIT, {:new_stacktrace, [{mod, _, l, _} | rest]}} =
          try do
            :erlang.error(
              :new_stacktrace,
              [mL, inEncoding, outEncoding]
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        :erlang.raise(:error, theError, [{mod, :characters_to_binary, l} | rest])
    end
  end

  def bom_to_encoding(<<239, 187, 191, _::binary>>) do
    {:utf8, 3}
  end

  def bom_to_encoding(<<0, 0, 254, 255, _::binary>>) do
    {{:utf32, :big}, 4}
  end

  def bom_to_encoding(<<255, 254, 0, 0, _::binary>>) do
    {{:utf32, :little}, 4}
  end

  def bom_to_encoding(<<254, 255, _::binary>>) do
    {{:utf16, :big}, 2}
  end

  def bom_to_encoding(<<255, 254, _::binary>>) do
    {{:utf16, :little}, 2}
  end

  def bom_to_encoding(bin) when is_binary(bin) do
    {:latin1, 0}
  end

  def encoding_to_bom(:unicode) do
    <<239, 187, 191>>
  end

  def encoding_to_bom(:utf8) do
    <<239, 187, 191>>
  end

  def encoding_to_bom(:utf16) do
    <<254, 255>>
  end

  def encoding_to_bom({:utf16, :big}) do
    <<254, 255>>
  end

  def encoding_to_bom({:utf16, :little}) do
    <<255, 254>>
  end

  def encoding_to_bom(:utf32) do
    <<0, 0, 254, 255>>
  end

  def encoding_to_bom({:utf32, :big}) do
    <<0, 0, 254, 255>>
  end

  def encoding_to_bom({:utf32, :little}) do
    <<255, 254, 0, 0>>
  end

  def encoding_to_bom(:latin1) do
    <<>>
  end

  def characters_to_nfd_list(cD) do
    characters_to_nfd_list(cD, [])
  end

  defp characters_to_nfd_list(cD, acc) do
    case :unicode_util.nfd(cD) do
      [gC | str] when is_list(gC) ->
        characters_to_nfd_list(str, :lists.reverse(gC, acc))

      [cP | str] ->
        characters_to_nfd_list(str, [cP | acc])

      [] ->
        :lists.reverse(acc)

      {:error, error} ->
        {:error, :lists.reverse(acc), error}
    end
  end

  def characters_to_nfd_binary(cD) do
    characters_to_nfd_binary(cD, 200, [], [])
  end

  defp characters_to_nfd_binary(cD, n, row, acc) when n > 0 do
    case :unicode_util.nfd(cD) do
      [gC | str] ->
        characters_to_nfd_binary(str, n - 1, [gC | row], acc)

      [] ->
        acc_to_binary(prepend_row_to_acc(row, acc))

      {:error, error} ->
        {:error, acc_to_binary(prepend_row_to_acc(row, acc)), error}
    end
  end

  defp characters_to_nfd_binary(cD, _, row, acc) do
    characters_to_nfd_binary(cD, 200, [], prepend_row_to_acc(row, acc))
  end

  def characters_to_nfkd_list(cD) do
    characters_to_nfkd_list(cD, [])
  end

  defp characters_to_nfkd_list(cD, acc) do
    case :unicode_util.nfkd(cD) do
      [gC | str] when is_list(gC) ->
        characters_to_nfkd_list(str, :lists.reverse(gC, acc))

      [cP | str] ->
        characters_to_nfkd_list(str, [cP | acc])

      [] ->
        :lists.reverse(acc)

      {:error, error} ->
        {:error, :lists.reverse(acc), error}
    end
  end

  def characters_to_nfkd_binary(cD) do
    characters_to_nfkd_binary(cD, 200, [], [])
  end

  defp characters_to_nfkd_binary(cD, n, row, acc) when n > 0 do
    case :unicode_util.nfkd(cD) do
      [gC | str] ->
        characters_to_nfkd_binary(str, n - 1, [gC | row], acc)

      [] ->
        acc_to_binary(prepend_row_to_acc(row, acc))

      {:error, error} ->
        {:error, acc_to_binary(prepend_row_to_acc(row, acc)), error}
    end
  end

  defp characters_to_nfkd_binary(cD, _, row, acc) do
    characters_to_nfkd_binary(cD, 200, [], prepend_row_to_acc(row, acc))
  end

  def characters_to_nfc_list(cD) do
    characters_to_nfc_list(cD, [])
  end

  defp characters_to_nfc_list(cD, acc) do
    case :unicode_util.nfc(cD) do
      [gC | str] when is_list(gC) ->
        characters_to_nfc_list(str, :lists.reverse(gC, acc))

      [cP | str] ->
        characters_to_nfc_list(str, [cP | acc])

      [] ->
        :lists.reverse(acc)

      {:error, error} ->
        {:error, :lists.reverse(acc), error}
    end
  end

  def characters_to_nfc_binary(cD) do
    characters_to_nfc_binary(cD, 200, [], [])
  end

  defp characters_to_nfc_binary(cD, n, row, acc) when n > 0 do
    case :unicode_util.nfc(cD) do
      [gC | str] ->
        characters_to_nfc_binary(str, n - 1, [gC | row], acc)

      [] ->
        acc_to_binary(prepend_row_to_acc(row, acc))

      {:error, error} ->
        {:error, acc_to_binary(prepend_row_to_acc(row, acc)), error}
    end
  end

  defp characters_to_nfc_binary(cD, _, row, acc) do
    characters_to_nfc_binary(cD, 200, [], prepend_row_to_acc(row, acc))
  end

  def characters_to_nfkc_list(cD) do
    characters_to_nfkc_list(cD, [])
  end

  defp characters_to_nfkc_list(cD, acc) do
    case :unicode_util.nfkc(cD) do
      [gC | str] when is_list(gC) ->
        characters_to_nfkc_list(str, :lists.reverse(gC, acc))

      [cP | str] ->
        characters_to_nfkc_list(str, [cP | acc])

      [] ->
        :lists.reverse(acc)

      {:error, error} ->
        {:error, :lists.reverse(acc), error}
    end
  end

  def characters_to_nfkc_binary(cD) do
    characters_to_nfkc_binary(cD, 200, [], [])
  end

  defp characters_to_nfkc_binary(cD, n, row, acc) when n > 0 do
    case :unicode_util.nfkc(cD) do
      [gC | str] ->
        characters_to_nfkc_binary(str, n - 1, [gC | row], acc)

      [] ->
        acc_to_binary(prepend_row_to_acc(row, acc))

      {:error, error} ->
        {:error, acc_to_binary(prepend_row_to_acc(row, acc)), error}
    end
  end

  defp characters_to_nfkc_binary(cD, _, row, acc) do
    characters_to_nfkc_binary(cD, 200, [], prepend_row_to_acc(row, acc))
  end

  defp acc_to_binary(acc) do
    :erlang.list_to_binary(:lists.reverse(acc))
  end

  defp prepend_row_to_acc(row, acc) do
    [characters_to_binary(:lists.reverse(row)) | acc]
  end

  def characters_to_list_int(mL, encoding) do
    try do
      do_characters_to_list(mL, encoding)
    catch
      :error, anyError ->
        theError =
          case anyError do
            :system_limit ->
              :system_limit

            _ ->
              :badarg
          end

        {:EXIT, {:new_stacktrace, [{mod, _, l, _} | rest]}} =
          try do
            :erlang.error(
              :new_stacktrace,
              [mL, encoding]
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        :erlang.raise(:error, theError, [{mod, :characters_to_list, l} | rest])
    end
  end

  defp do_characters_to_list(mL, encoding) do
    case :unicode.characters_to_binary(mL, encoding) do
      bin when is_binary(bin) ->
        :unicode.characters_to_list(bin, :utf8)

      {:error, encoded, rest} ->
        {:error, :unicode.characters_to_list(encoded, :utf8), rest}

      {:incomplete, encoded2, rest2} ->
        {:incomplete, :unicode.characters_to_list(encoded2, :utf8), rest2}
    end
  end

  def characters_to_binary_int(mL, inEncoding) do
    try do
      characters_to_binary_int(mL, inEncoding, :unicode)
    catch
      :error, anyError ->
        theError =
          case anyError do
            :system_limit ->
              :system_limit

            _ ->
              :badarg
          end

        {:EXIT, {:new_stacktrace, [{mod, _, l, _} | rest]}} =
          try do
            :erlang.error(
              :new_stacktrace,
              [mL, inEncoding]
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        :erlang.raise(:error, theError, [{mod, :characters_to_binary, l} | rest])
    end
  end

  defp characters_to_binary_int(mL, inEncoding, outEncoding)
       when (inEncoding === :latin1 and
               outEncoding === :unicode) or
              (inEncoding === :latin1 and outEncoding === :utf8) or
              (inEncoding === :unicode and
                 outEncoding === :unicode) or
              (inEncoding === :unicode and outEncoding === :utf8) or
              (inEncoding === :utf8 and outEncoding === :unicode) or
              (inEncoding === :utf8 and outEncoding === :utf8) do
    :unicode.characters_to_binary(mL, inEncoding)
  end

  defp characters_to_binary_int(mL, inEncoding, outEncoding) do
    {inTrans, limit} =
      case outEncoding do
        :latin1 ->
          {i_trans_chk(inEncoding), 255}

        _ ->
          {i_trans(inEncoding),
           case inEncoding do
             :latin1 ->
               255

             _ ->
               1_114_111
           end}
      end

    outTrans = o_trans(outEncoding)

    res =
      ml_map(
        mL,
        fn
          part, accum when is_binary(part) ->
            case inTrans.(part) do
              list when is_list(list) ->
                tail = outTrans.(list)
                <<accum::binary, tail::binary>>

              {:error, translated, rest} ->
                tail = outTrans.(translated)
                {:error, <<accum::binary, tail::binary>>, rest}

              {:incomplete, translated, rest, missing} ->
                tail = outTrans.(translated)
                {:incomplete, <<accum::binary, tail::binary>>, rest, missing}
            end

          part, accum when is_integer(part) and part <= limit ->
            case outTrans.([part]) do
              binary when is_binary(binary) ->
                <<accum::binary, binary::binary>>

              {:error, _, [^part]} ->
                {:error, accum, [part]}
            end

          part, accum ->
            {:error, accum, [part]}
        end,
        <<>>
      )

    case res do
      {:incomplete, a, b, _} ->
        {:incomplete, a, b}

      _ ->
        res
    end
  end

  defp cbv(
         :utf8,
         <<1::size(1), 1::size(1), 0::size(1), _::size(5)>>
       ) do
    1
  end

  defp cbv(
         :utf8,
         <<1::size(1), 1::size(1), 1::size(1), 0::size(1), _::size(4), r::binary>>
       ) do
    case r do
      <<>> ->
        2

      <<1::size(1), 0::size(1), _::size(6)>> ->
        1

      _ ->
        false
    end
  end

  defp cbv(
         :utf8,
         <<1::size(1), 1::size(1), 1::size(1), 1::size(1), 0::size(1), _::size(3), r::binary>>
       ) do
    case r do
      <<>> ->
        3

      <<1::size(1), 0::size(1), _::size(6)>> ->
        2

      <<1::size(1), 0::size(1), _::size(6), 1::size(1), 0::size(1), _::size(6)>> ->
        1

      _ ->
        false
    end
  end

  defp cbv(:utf8, _) do
    false
  end

  defp cbv({:utf16, :big}, <<a::size(8)>>)
       when a <= 215 or a >= 224 do
    1
  end

  defp cbv(
         {:utf16, :big},
         <<54::size(6), _::size(2)>>
       ) do
    3
  end

  defp cbv(
         {:utf16, :big},
         <<54::size(6), _::size(10)>>
       ) do
    2
  end

  defp cbv(
         {:utf16, :big},
         <<54::size(6), _::size(10), 55::size(6), _::size(2)>>
       ) do
    1
  end

  defp cbv({:utf16, :big}, _) do
    false
  end

  defp cbv({:utf16, :little}, <<_::size(8)>>) do
    1
  end

  defp cbv(
         {:utf16, :little},
         <<_::size(8), 54::size(6), _::size(2)>>
       ) do
    2
  end

  defp cbv(
         {:utf16, :little},
         <<_::size(8), 54::size(6), _::size(2), _::size(8)>>
       ) do
    1
  end

  defp cbv({:utf16, :little}, _) do
    false
  end

  defp cbv({:utf32, :big}, <<0::size(8)>>) do
    3
  end

  defp cbv({:utf32, :big}, <<0::size(8), x::size(8)>>)
       when x <= 16 do
    2
  end

  defp cbv(
         {:utf32, :big},
         <<0::size(8), x::size(8), y::size(8)>>
       )
       when (x <= 16 and x > 0) or y <= 215 or y >= 224 do
    1
  end

  defp cbv({:utf32, :big}, _) do
    false
  end

  defp cbv({:utf32, :little}, <<_::size(8)>>) do
    3
  end

  defp cbv(
         {:utf32, :little},
         <<_::size(8), _::size(8)>>
       ) do
    2
  end

  defp cbv(
         {:utf32, :little},
         <<x::size(8), 255::size(8), 0::size(8)>>
       )
       when x === 254 or x === 255 do
    false
  end

  defp cbv(
         {:utf32, :little},
         <<_::size(8), y::size(8), x::size(8)>>
       )
       when (x <= 16 and x > 0) or y <= 215 or y >= 224 do
    1
  end

  defp cbv({:utf32, :little}, _) do
    false
  end

  defp ml_map([], _, {{inc, x}, accum}) do
    {:incomplete, accum, inc, x}
  end

  defp ml_map([], _Fun, accum) do
    accum
  end

  defp ml_map([part | _] = whole, _, {{incomplete, _}, accum})
       when is_integer(part) do
    {:error, accum, [incomplete | whole]}
  end

  defp ml_map([part | t], fun, accum) when is_integer(part) do
    case fun.(part, accum) do
      bin when is_binary(bin) ->
        case ml_map(t, fun, bin) do
          bin2 when is_binary(bin2) ->
            bin2

          {:error, converted, rest} ->
            {:error, converted, rest}

          {:incomplete, converted, rest, x} ->
            {:incomplete, converted, rest, x}
        end

      {:error, converted, rest} ->
        {:error, converted, [rest | t]}
    end
  end

  defp ml_map([part | t], fun, {{incomplete, missing}, accum})
       when is_binary(part) do
    case byte_size(part) do
      n when n >= missing ->
        <<fillIn::size(missing)-binary, trailing::binary>> = part
        newPart = <<incomplete::binary, fillIn::binary>>
        ml_map([[newPart, trailing] | t], fun, accum)

      m ->
        newIncomplete = <<incomplete::binary, part::binary>>
        newMissing = missing - m
        ml_map(t, fun, {{newIncomplete, newMissing}, accum})
    end
  end

  defp ml_map([part | t], fun, accum)
       when is_binary(part) and byte_size(part) > 8192 do
    <<part1::size(8192)-binary, part2::binary>> = part
    ml_map([[part1, part2] | t], fun, accum)
  end

  defp ml_map([part | t], fun, accum) when is_binary(part) do
    case fun.(part, accum) do
      bin when is_binary(bin) ->
        ml_map(t, fun, bin)

      {:incomplete, converted, rest, missing} ->
        ml_map(t, fun, {{rest, missing}, converted})

      {:error, converted, rest} ->
        {:error, converted, [rest | t]}
    end
  end

  defp ml_map([list | t], fun, accum) when is_list(list) do
    case ml_map(list, fun, accum) do
      bin when is_binary(bin) ->
        ml_map(t, fun, bin)

      {:error, converted, rest} ->
        {:error, converted, [rest | t]}

      {:incomplete, converted, rest, n} ->
        ml_map(t, fun, {{rest, n}, converted})
    end
  end

  defp ml_map(bin, fun, {{incomplete, missing}, accum})
       when is_binary(bin) do
    case byte_size(bin) do
      n when n >= missing ->
        ml_map([incomplete, bin], fun, accum)

      m ->
        {:incomplete, accum, <<incomplete::binary, bin::binary>>, missing - m}
    end
  end

  defp ml_map(part, fun, accum)
       when is_binary(part) and
              byte_size(part) > 8192 do
    <<part1::size(8192)-binary, part2::binary>> = part
    ml_map([part1, part2], fun, accum)
  end

  defp ml_map(bin, fun, accum) when is_binary(bin) do
    fun.(bin, accum)
  end

  defp i_trans(:latin1) do
    fn bin ->
      :erlang.binary_to_list(bin)
    end
  end

  defp i_trans(:unicode) do
    i_trans(:utf8)
  end

  defp i_trans(:utf8) do
    &do_i_utf8/1
  end

  defp i_trans(:utf16) do
    &do_i_utf16_big/1
  end

  defp i_trans({:utf16, :big}) do
    &do_i_utf16_big/1
  end

  defp i_trans({:utf16, :little}) do
    &do_i_utf16_little/1
  end

  defp i_trans(:utf32) do
    &do_i_utf32_big/1
  end

  defp i_trans({:utf32, :big}) do
    &do_i_utf32_big/1
  end

  defp i_trans({:utf32, :little}) do
    &do_i_utf32_little/1
  end

  defp i_trans_chk(:latin1) do
    fn bin ->
      :erlang.binary_to_list(bin)
    end
  end

  defp i_trans_chk(:unicode) do
    i_trans_chk(:utf8)
  end

  defp i_trans_chk(:utf8) do
    &do_i_utf8_chk/1
  end

  defp i_trans_chk(:utf16) do
    &do_i_utf16_big_chk/1
  end

  defp i_trans_chk({:utf16, :big}) do
    &do_i_utf16_big_chk/1
  end

  defp i_trans_chk({:utf16, :little}) do
    &do_i_utf16_little_chk/1
  end

  defp i_trans_chk(:utf32) do
    &do_i_utf32_big_chk/1
  end

  defp i_trans_chk({:utf32, :big}) do
    &do_i_utf32_big_chk/1
  end

  defp i_trans_chk({:utf32, :little}) do
    &do_i_utf32_little_chk/1
  end

  defp o_trans(:latin1) do
    fn l ->
      :erlang.list_to_binary(l)
    end
  end

  defp o_trans(:unicode) do
    o_trans(:utf8)
  end

  defp o_trans(:utf8) do
    fn l ->
      do_o_binary(
        fn one ->
          <<one::utf8>>
        end,
        l
      )
    end
  end

  defp o_trans(:utf16) do
    fn l ->
      do_o_binary(
        fn one ->
          <<one::utf16>>
        end,
        l
      )
    end
  end

  defp o_trans({:utf16, :big}) do
    o_trans(:utf16)
  end

  defp o_trans({:utf16, :little}) do
    fn l ->
      do_o_binary(
        fn one ->
          <<one::utf16-little>>
        end,
        l
      )
    end
  end

  defp o_trans(:utf32) do
    fn l ->
      do_o_binary(
        fn one ->
          <<one::utf32>>
        end,
        l
      )
    end
  end

  defp o_trans({:utf32, :big}) do
    o_trans(:utf32)
  end

  defp o_trans({:utf32, :little}) do
    fn l ->
      do_o_binary(
        fn one ->
          <<one::utf32-little>>
        end,
        l
      )
    end
  end

  defp do_o_binary(f, l) do
    case do_o_binary2(f, l) do
      {tag, list, r} ->
        {tag, :erlang.iolist_to_binary(list), r}

      list ->
        :erlang.iolist_to_binary(list)
    end
  end

  defp do_o_binary2(_F, []) do
    <<>>
  end

  defp do_o_binary2(f, [h | t]) do
    case (try do
            f.(h)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, <<>>, [h | t]}

      bin when is_binary(bin) ->
        case do_o_binary2(f, t) do
          {:error, bin2, rest} ->
            {:error, [bin | bin2], rest}

          bin3 ->
            [bin | bin3]
        end
    end
  end

  defp do_i_utf8_chk(<<>>) do
    []
  end

  defp do_i_utf8_chk(<<u::utf8, r::binary>>) when u <= 255 do
    case do_i_utf8_chk(r) do
      {:error, trans, rest} ->
        {:error, [u | trans], rest}

      {:incomplete, trans, rest, n} ->
        {:incomplete, [u | trans], rest, n}

      l when is_list(l) ->
        [u | l]
    end
  end

  defp do_i_utf8_chk(<<_::utf8, _::binary>> = bin) do
    {:error, [], bin}
  end

  defp do_i_utf8_chk(bin) when is_binary(bin) do
    case cbv(:utf8, bin) do
      n when is_integer(n) ->
        {:incomplete, [], bin, n}

      false ->
        {:error, [], bin}
    end
  end

  defp do_i_utf16_big_chk(<<>>) do
    []
  end

  defp do_i_utf16_big_chk(<<u::utf16, r::binary>>) when u <= 255 do
    case do_i_utf16_big_chk(r) do
      {:error, trans, rest} ->
        {:error, [u | trans], rest}

      {:incomplete, trans, rest, n} ->
        {:incomplete, [u | trans], rest, n}

      l when is_list(l) ->
        [u | l]
    end
  end

  defp do_i_utf16_big_chk(<<_::utf16, _::binary>> = bin) do
    {:error, [], bin}
  end

  defp do_i_utf16_big_chk(bin) when is_binary(bin) do
    case cbv({:utf16, :big}, bin) do
      n when is_integer(n) ->
        {:incomplete, [], bin, n}

      false ->
        {:error, [], bin}
    end
  end

  defp do_i_utf16_little_chk(<<>>) do
    []
  end

  defp do_i_utf16_little_chk(<<u::utf16-little, r::binary>>)
       when u <= 255 do
    case do_i_utf16_little_chk(r) do
      {:error, trans, rest} ->
        {:error, [u | trans], rest}

      {:incomplete, trans, rest, n} ->
        {:incomplete, [u | trans], rest, n}

      l when is_list(l) ->
        [u | l]
    end
  end

  defp do_i_utf16_little_chk(<<_::utf16-little, _::binary>> = bin) do
    {:error, [], bin}
  end

  defp do_i_utf16_little_chk(bin) when is_binary(bin) do
    case cbv({:utf16, :little}, bin) do
      n when is_integer(n) ->
        {:incomplete, [], bin, n}

      false ->
        {:error, [], bin}
    end
  end

  defp do_i_utf32_big_chk(<<>>) do
    []
  end

  defp do_i_utf32_big_chk(<<u::utf32, r::binary>>) when u <= 255 do
    case do_i_utf32_big_chk(r) do
      {:error, trans, rest} ->
        {:error, [u | trans], rest}

      l when is_list(l) ->
        [u | l]
    end
  end

  defp do_i_utf32_big_chk(<<_::utf32, _::binary>> = bin) do
    {:error, [], bin}
  end

  defp do_i_utf32_big_chk(bin) when is_binary(bin) do
    case cbv({:utf32, :big}, bin) do
      n when is_integer(n) ->
        {:incomplete, [], bin, n}

      false ->
        {:error, [], bin}
    end
  end

  defp do_i_utf32_little_chk(<<>>) do
    []
  end

  defp do_i_utf32_little_chk(<<u::utf32-little, r::binary>>)
       when u <= 255 do
    case do_i_utf32_little_chk(r) do
      {:error, trans, rest} ->
        {:error, [u | trans], rest}

      l when is_list(l) ->
        [u | l]
    end
  end

  defp do_i_utf32_little_chk(<<_::utf32-little, _::binary>> = bin) do
    {:error, [], bin}
  end

  defp do_i_utf32_little_chk(bin) when is_binary(bin) do
    case cbv({:utf32, :little}, bin) do
      n when is_integer(n) ->
        {:incomplete, [], bin, n}

      false ->
        {:error, [], bin}
    end
  end

  defp do_i_utf8(<<>>) do
    []
  end

  defp do_i_utf8(<<u::utf8, r::binary>>) do
    case do_i_utf8(r) do
      {:error, trans, rest} ->
        {:error, [u | trans], rest}

      {:incomplete, trans, rest, n} ->
        {:incomplete, [u | trans], rest, n}

      l when is_list(l) ->
        [u | l]
    end
  end

  defp do_i_utf8(bin) when is_binary(bin) do
    case cbv(:utf8, bin) do
      n when is_integer(n) ->
        {:incomplete, [], bin, n}

      false ->
        {:error, [], bin}
    end
  end

  defp do_i_utf16_big(<<>>) do
    []
  end

  defp do_i_utf16_big(<<u::utf16, r::binary>>) do
    case do_i_utf16_big(r) do
      {:error, trans, rest} ->
        {:error, [u | trans], rest}

      {:incomplete, trans, rest, n} ->
        {:incomplete, [u | trans], rest, n}

      l when is_list(l) ->
        [u | l]
    end
  end

  defp do_i_utf16_big(bin) when is_binary(bin) do
    case cbv({:utf16, :big}, bin) do
      n when is_integer(n) ->
        {:incomplete, [], bin, n}

      false ->
        {:error, [], bin}
    end
  end

  defp do_i_utf16_little(<<>>) do
    []
  end

  defp do_i_utf16_little(<<u::utf16-little, r::binary>>) do
    case do_i_utf16_little(r) do
      {:error, trans, rest} ->
        {:error, [u | trans], rest}

      {:incomplete, trans, rest, n} ->
        {:incomplete, [u | trans], rest, n}

      l when is_list(l) ->
        [u | l]
    end
  end

  defp do_i_utf16_little(bin) when is_binary(bin) do
    case cbv({:utf16, :little}, bin) do
      n when is_integer(n) ->
        {:incomplete, [], bin, n}

      false ->
        {:error, [], bin}
    end
  end

  defp do_i_utf32_big(<<>>) do
    []
  end

  defp do_i_utf32_big(<<u::utf32, r::binary>>) do
    case do_i_utf32_big(r) do
      {:error, trans, rest} ->
        {:error, [u | trans], rest}

      {:incomplete, trans, rest, n} ->
        {:incomplete, [u | trans], rest, n}

      l when is_list(l) ->
        [u | l]
    end
  end

  defp do_i_utf32_big(bin) when is_binary(bin) do
    case cbv({:utf32, :big}, bin) do
      n when is_integer(n) ->
        {:incomplete, [], bin, n}

      false ->
        {:error, [], bin}
    end
  end

  defp do_i_utf32_little(<<>>) do
    []
  end

  defp do_i_utf32_little(<<u::utf32-little, r::binary>>) do
    case do_i_utf32_little(r) do
      {:error, trans, rest} ->
        {:error, [u | trans], rest}

      {:incomplete, trans, rest, n} ->
        {:incomplete, [u | trans], rest, n}

      l when is_list(l) ->
        [u | l]
    end
  end

  defp do_i_utf32_little(bin) when is_binary(bin) do
    case cbv({:utf32, :little}, bin) do
      n when is_integer(n) ->
        {:incomplete, [], bin, n}

      false ->
        {:error, [], bin}
    end
  end
end
