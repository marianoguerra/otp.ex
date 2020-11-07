defmodule :m_crypto do
  use Bitwise
  @on_load :on_load
  defp nif_stub_error(line) do
    :erlang.nif_error({:nif_not_loaded, :module, :crypto, :line, line})
  end

  def version() do
    '??'
  end

  def start() do
    :application.start(:crypto)
  end

  def stop() do
    :application.stop(:crypto)
  end

  def supports() do
    [
      [{:hashs, supports(:hashs)}, {:ciphers, prepend_old_aliases(supports(:ciphers))}]
      | for t <- [:public_keys, :macs, :curves, :rsa_opts] do
          {t, supports(t)}
        end
    ]
  end

  def supports(:hashs) do
    hash_algorithms()
  end

  def supports(:public_keys) do
    pubkey_algorithms()
  end

  def supports(:ciphers) do
    cipher_algorithms()
  end

  def supports(:macs) do
    mac_algorithms()
  end

  def supports(:curves) do
    curve_algorithms()
  end

  def supports(:rsa_opts) do
    rsa_opts_algorithms()
  end

  def info_lib() do
    nif_stub_error(712)
  end

  def info_fips() do
    nif_stub_error(716)
  end

  def enable_fips_mode(enable) do
    enable_fips_mode_nif(enable)
  end

  defp enable_fips_mode_nif(_) do
    nif_stub_error(723)
  end

  def equal_const_time(x1, x2) do
    equal_const_time(x1, x2, true)
  end

  defp equal_const_time(<<b1, r1::binary>>, <<b2, r2::binary>>, truth) do
    equal_const_time(r1, r2, :erlang.and(truth, b1 == b2))
  end

  defp equal_const_time(<<_, r1::binary>>, <<>>, truth) do
    equal_const_time(r1, <<>>, :erlang.and(truth, false))
  end

  defp equal_const_time(<<>>, <<>>, truth) do
    truth
  end

  defp equal_const_time([h1 | t1], [h2 | t2], truth) do
    equal_const_time(t1, t2, :erlang.and(truth, h1 == h2))
  end

  defp equal_const_time([_ | t1], [], truth) do
    equal_const_time(t1, [], :erlang.and(truth, false))
  end

  defp equal_const_time([], [], truth) do
    truth
  end

  defp equal_const_time(_, _, _) do
    false
  end

  def hash_info(type) do
    notsup_to_error(hash_info_nif(type))
  end

  def hash(type, data) do
    data1 = :erlang.iolist_to_binary(data)
    maxBytes = max_bytes()
    hash(type, data1, :erlang.byte_size(data1), maxBytes)
  end

  def hash_init(type) do
    notsup_to_error(hash_init_nif(type))
  end

  def hash_update(context, data) do
    data1 = :erlang.iolist_to_binary(data)
    maxBytes = max_bytes()
    hash_update(context, data1, :erlang.byte_size(data1), maxBytes)
  end

  def hash_final(context) do
    notsup_to_error(hash_final_nif(context))
  end

  def mac(:poly1305, key, data) do
    mac(:poly1305, :undefined, key, data)
  end

  def mac(type, subType, key, data) do
    mac_nif(type, subType, key, data)
  end

  def macN(type, key, data, macLength) do
    macN(type, :undefined, key, data, macLength)
  end

  def macN(type, subType, key, data, macLength) do
    :erlang.binary_part(mac(type, subType, key, data), 0, macLength)
  end

  def mac_init(:poly1305, key) do
    mac_init_nif(:poly1305, :undefined, key)
  end

  def mac_init(type, subType, key) do
    mac_init_nif(type, subType, key)
  end

  def mac_update(ref, data) do
    mac_update_nif(ref, data)
  end

  def mac_final(ref) do
    mac_final_nif(ref)
  end

  def mac_finalN(ref, macLength) do
    :erlang.binary_part(mac_final(ref), 0, macLength)
  end

  defp mac_nif(_Type, _SubType, _Key, _Data) do
    nif_stub_error(905)
  end

  defp mac_init_nif(_Type, _SubType, _Key) do
    nif_stub_error(907)
  end

  defp mac_update_nif(_Ref, _Data) do
    nif_stub_error(908)
  end

  defp mac_final_nif(_Ref) do
    nif_stub_error(909)
  end

  def hmac(type, key, data) do
    try do
      mac(:hmac, type, key, data)
    catch
      :error, {:error, {_File, _Line}, _Reason} ->
        :erlang.error(:badarg)

      :error, {e, {_File, _Line}, _Reason}
      when e == :notsup or e == :badarg ->
        :erlang.error(e)
    end
  end

  def hmac(type, key, data, macLength) do
    try do
      macN(:hmac, type, key, data, macLength)
    catch
      :error, {:error, {_File, _Line}, _Reason} ->
        :erlang.error(:badarg)

      :error, {e, {_File, _Line}, _Reason}
      when e == :notsup or e == :badarg ->
        :erlang.error(e)
    end
  end

  def hmac_init(type, key) do
    try do
      mac_init(:hmac, type, key)
    catch
      :error, {:error, {_File, _Line}, _Reason} ->
        :erlang.error(:badarg)

      :error, {e, {_File, _Line}, _Reason}
      when e == :notsup or e == :badarg ->
        :erlang.error(e)
    end
  end

  def hmac_update(state, data) do
    try do
      mac_update(state, data)
    catch
      :error, {:error, {_File, _Line}, _Reason} ->
        :erlang.error(:badarg)

      :error, {e, {_File, _Line}, _Reason}
      when e == :notsup or e == :badarg ->
        :erlang.error(e)
    end
  end

  def hmac_final(context) do
    try do
      mac_final(context)
    catch
      :error, {:error, {_File, _Line}, _Reason} ->
        :erlang.error(:badarg)

      :error, {e, {_File, _Line}, _Reason}
      when e == :notsup or e == :badarg ->
        :erlang.error(e)
    end
  end

  def hmac_final_n(context, hashLen) do
    try do
      mac_finalN(context, hashLen)
    catch
      :error, {:error, {_File, _Line}, _Reason} ->
        :erlang.error(:badarg)

      :error, {e, {_File, _Line}, _Reason}
      when e == :notsup or e == :badarg ->
        :erlang.error(e)
    end
  end

  def cmac(type, key, data) do
    try do
      mac(:cmac, __MODULE__.alias(type), key, data)
    catch
      :error, {:error, {_File, _Line}, _Reason} ->
        :erlang.error(:badarg)

      :error, {e, {_File, _Line}, _Reason}
      when e == :notsup or e == :badarg ->
        :erlang.error(e)
    end
  end

  def cmac(type, key, data, macLength) do
    try do
      macN(:cmac, __MODULE__.alias(type), key, data, macLength)
    catch
      :error, {:error, {_File, _Line}, _Reason} ->
        :erlang.error(:badarg)

      :error, {e, {_File, _Line}, _Reason}
      when e == :notsup or e == :badarg ->
        :erlang.error(e)
    end
  end

  def poly1305(key, data) do
    try do
      mac(:poly1305, key, data)
    catch
      :error, {:error, {_File, _Line}, _Reason} ->
        :erlang.error(:badarg)

      :error, {e, {_File, _Line}, _Reason}
      when e == :notsup or e == :badarg ->
        :erlang.error(e)
    end
  end

  def cipher_info(:aes_ctr) do
    %{
      :block_size => 1,
      :iv_length => 16,
      :key_length => 32,
      :mode => :ctr_mode,
      :type => :undefined
    }
  end

  def cipher_info(:aes_128_ctr) do
    %{
      :block_size => 1,
      :iv_length => 16,
      :key_length => 16,
      :mode => :ctr_mode,
      :type => :undefined
    }
  end

  def cipher_info(:aes_192_ctr) do
    %{
      :block_size => 1,
      :iv_length => 16,
      :key_length => 24,
      :mode => :ctr_mode,
      :type => :undefined
    }
  end

  def cipher_info(:aes_256_ctr) do
    %{
      :block_size => 1,
      :iv_length => 16,
      :key_length => 32,
      :mode => :ctr_mode,
      :type => :undefined
    }
  end

  def cipher_info(:aes_ige256) do
    %{
      :block_size => 16,
      :iv_length => 32,
      :key_length => 16,
      :mode => :ige_mode,
      :type => :undefined
    }
  end

  def cipher_info(type) do
    cipher_info_nif(__MODULE__.alias(type))
  end

  def block_encrypt(:aes_ige256, key, ivec, plainText) do
    notsup_to_error(aes_ige_crypt_nif(key, ivec, plainText, true))
  end

  def block_encrypt(type, key0, ivec, data) do
    key = :erlang.iolist_to_binary(key0)

    try do
      case data do
        {aAD, plainText} ->
          crypto_one_time_aead(__MODULE__.alias(type, key), key, ivec, plainText, aAD, true)

        {aAD, plainText, tagLength} ->
          crypto_one_time_aead(
            __MODULE__.alias(type, key),
            key,
            ivec,
            plainText,
            aAD,
            tagLength,
            true
          )

        plainText ->
          block_crypt(__MODULE__.alias(type, key), key, ivec, plainText, true)
      end
    catch
      :error, {:error, {_File, _Line}, _Reason} ->
        :erlang.error(:badarg)

      :error, {e, {_File, _Line}, _Reason}
      when e == :notsup or e == :badarg ->
        :erlang.error(e)
    end
  end

  def block_encrypt(type, key0, plainText) do
    key = :erlang.iolist_to_binary(key0)

    try do
      block_crypt(__MODULE__.alias(type, key), key, :undefined, plainText, true)
    catch
      :error, {:error, {_File, _Line}, _Reason} ->
        :erlang.error(:badarg)

      :error, {e, {_File, _Line}, _Reason}
      when e == :notsup or e == :badarg ->
        :erlang.error(e)
    end
  end

  def block_decrypt(:aes_ige256, key, ivec, data) do
    notsup_to_error(aes_ige_crypt_nif(key, ivec, data, false))
  end

  def block_decrypt(type, key0, ivec, data) do
    key = :erlang.iolist_to_binary(key0)

    try do
      case data do
        {aAD, cryptoText, tag} ->
          crypto_one_time_aead(
            __MODULE__.alias(type, key),
            key,
            ivec,
            cryptoText,
            aAD,
            tag,
            false
          )

        cryptoText ->
          block_crypt(__MODULE__.alias(type, key), key, ivec, cryptoText, false)
      end
    catch
      :error, {:error, {_File, _Line}, _Reason} ->
        :erlang.error(:badarg)

      :error, {e, {_File, _Line}, _Reason}
      when e == :notsup or e == :badarg ->
        :erlang.error(e)
    end
  end

  def block_decrypt(type, key0, cryptoText) do
    key = :erlang.iolist_to_binary(key0)

    try do
      block_crypt(__MODULE__.alias(type, key), key, :undefined, cryptoText, false)
    catch
      :error, {:error, {_File, _Line}, _Reason} ->
        :erlang.error(:badarg)

      :error, {e, {_File, _Line}, _Reason}
      when e == :notsup or e == :badarg ->
        :erlang.error(e)
    end
  end

  defp block_crypt(cipher, key, iV, data, encryptFlag) do
    ctx =
      case iV do
        :undefined ->
          crypto_init(cipher, key, encryptFlag)

        _ ->
          crypto_init(cipher, key, iV, encryptFlag)
      end

    crypto_update(ctx, data)
  end

  def stream_init(type, key0, iVec) when is_binary(iVec) do
    key = :erlang.iolist_to_binary(key0)

    ref =
      try do
        ng_crypto_init_nif(
          __MODULE__.alias(type, key),
          key,
          :erlang.iolist_to_binary(iVec),
          get_crypto_opts([{:encrypt, :undefined}])
        )
      catch
        :error, {:error, {_File, _Line}, _Reason} ->
          :erlang.error(:badarg)

        :error, {e, {_File, _Line}, _Reason}
        when e == :notsup or e == :badarg ->
          :erlang.error(e)
      end

    {type, {ref, :flg_undefined}}
  end

  def stream_init(:rc4 = type, key0) do
    key = :erlang.iolist_to_binary(key0)

    ref =
      try do
        ng_crypto_init_nif(
          __MODULE__.alias(type, key),
          key,
          <<>>,
          get_crypto_opts([{:encrypt, :undefined}])
        )
      catch
        :error, {:error, {_File, _Line}, _Reason} ->
          :erlang.error(:badarg)

        :error, {e, {_File, _Line}, _Reason}
        when e == :notsup or e == :badarg ->
          :erlang.error(e)
      end

    {type, {ref, :flg_undefined}}
  end

  def stream_encrypt(state, data) do
    crypto_stream_emulate(state, data, true)
  end

  def stream_decrypt(state, data) do
    crypto_stream_emulate(state, data, false)
  end

  defp crypto_stream_emulate({cipher, {ref0, :flg_undefined}}, data, encryptFlag)
       when is_reference(ref0) do
    try do
      ref = ng_crypto_init_nif(ref0, <<>>, <<>>, get_crypto_opts([{:encrypt, encryptFlag}]))
      {{cipher, ref}, crypto_update(ref, data)}
    catch
      :error, {:error, {_File, _Line}, _Reason} ->
        :erlang.error(:badarg)

      :error, {e, {_File, _Line}, _Reason}
      when e == :notsup or e == :badarg ->
        :erlang.error(e)
    end
  end

  defp crypto_stream_emulate({cipher, ref}, data, _)
       when is_reference(ref) do
    try do
      {{cipher, ref}, crypto_update(ref, data)}
    catch
      :error, {:error, {_File, _Line}, _Reason} ->
        :erlang.error(:badarg)

      :error, {e, {_File, _Line}, _Reason}
      when e == :notsup or e == :badarg ->
        :erlang.error(e)
    end
  end

  def next_iv(type, data) when is_binary(data) do
    iVecSize =
      case type do
        :des_cbc ->
          8

        :des3_cbc ->
          8

        :aes_cbc ->
          16

        :aes_ige ->
          32
      end

    {_, iVec} =
      :erlang.split_binary(
        data,
        :erlang.size(data) - iVecSize
      )

    iVec
  end

  def next_iv(type, data) when is_list(data) do
    next_iv(type, :erlang.list_to_binary(data))
  end

  def next_iv(:des_cfb, data, iVec) do
    iVecAndData = :erlang.list_to_binary([iVec, data])

    {_, newIVec} =
      :erlang.split_binary(
        iVecAndData,
        byte_size(iVecAndData) - 8
      )

    newIVec
  end

  def next_iv(type, data, _Ivec) do
    next_iv(type, data)
  end

  def crypto_init(cipher, key, flagOrOptions) do
    ng_crypto_init_nif(
      cipher,
      :erlang.iolist_to_binary(key),
      <<>>,
      get_crypto_opts(flagOrOptions)
    )
  end

  def crypto_init(cipher, key, iV, flagOrOptions) do
    ng_crypto_init_nif(
      cipher,
      :erlang.iolist_to_binary(key),
      :erlang.iolist_to_binary(iV),
      get_crypto_opts(flagOrOptions)
    )
  end

  defp get_crypto_opts(options) when is_list(options) do
    :lists.foldl(&chk_opt/2, %{:encrypt => true, :padding => :undefined}, options)
  end

  defp get_crypto_opts(flag) when is_boolean(flag) do
    %{:encrypt => flag, :padding => :undefined}
  end

  defp get_crypto_opts(x) do
    :erlang.error({:badarg, {:bad_option, x}})
  end

  defp chk_opt({tag, val}, a) do
    case ok_opt(tag, val) do
      true ->
        %{a | tag => val}

      false ->
        :erlang.error({:badarg, {:bad_option, {tag, val}}})
    end
  end

  defp chk_opt(x, _) do
    :erlang.error({:badarg, {:bad_option, x}})
  end

  defp ok_opt(:encrypt, v) do
    :lists.member(v, [true, false, :undefined])
  end

  defp ok_opt(:padding, v) do
    :lists.member(
      v,
      [:none, :pkcs_padding, :zero, :random, :undefined]
    )
  end

  defp ok_opt(_, _) do
    false
  end

  def crypto_dyn_iv_init(cipher, key, flagOrOptions) do
    ng_crypto_init_nif(
      cipher,
      :erlang.iolist_to_binary(key),
      :undefined,
      get_crypto_opts(flagOrOptions)
    )
  end

  def crypto_update(state, data) do
    ng_crypto_update_nif(
      state,
      :erlang.iolist_to_binary(data)
    )
  end

  def crypto_dyn_iv_update(state, data, iV) do
    ng_crypto_update_nif(
      state,
      :erlang.iolist_to_binary(data),
      :erlang.iolist_to_binary(iV)
    )
  end

  def crypto_final(state) do
    ng_crypto_final_nif(state)
  end

  def crypto_get_data(state) do
    ng_crypto_get_data_nif(state)
  end

  def crypto_one_time(cipher, key, data, flagOrOptions) do
    ng_crypto_one_time_nif(
      cipher,
      :erlang.iolist_to_binary(key),
      <<>>,
      :erlang.iolist_to_binary(data),
      get_crypto_opts(flagOrOptions)
    )
  end

  def crypto_one_time(cipher, key, iV, data, flagOrOptions) do
    ng_crypto_one_time_nif(
      cipher,
      :erlang.iolist_to_binary(key),
      :erlang.iolist_to_binary(iV),
      :erlang.iolist_to_binary(data),
      get_crypto_opts(flagOrOptions)
    )
  end

  def crypto_one_time_aead(cipher, key, iV, plainText, aAD, true) do
    crypto_one_time_aead(cipher, key, iV, plainText, aAD, aead_tag_len(cipher), true)
  end

  def crypto_one_time_aead(cipher, key, iV, textIn, aAD, tagOrTagLength, encFlg) do
    aead_cipher(cipher, key, iV, textIn, aAD, tagOrTagLength, encFlg)
  end

  defp aead_tag_len(:chacha20_poly1305) do
    16
  end

  defp aead_tag_len(:aes_ccm) do
    12
  end

  defp aead_tag_len(:aes_128_ccm) do
    12
  end

  defp aead_tag_len(:aes_192_ccm) do
    12
  end

  defp aead_tag_len(:aes_256_ccm) do
    12
  end

  defp aead_tag_len(:aes_gcm) do
    16
  end

  defp aead_tag_len(:aes_128_gcm) do
    16
  end

  defp aead_tag_len(:aes_192_gcm) do
    16
  end

  defp aead_tag_len(:aes_256_gcm) do
    16
  end

  defp aead_tag_len(_) do
    :erlang.error({:badarg, 'Not an AEAD cipher'})
  end

  defp ng_crypto_init_nif(cipher, key, iVec, %{:encrypt => encryptFlag, :padding => padding}) do
    ng_crypto_init_nif(cipher, key, iVec, encryptFlag, padding)
  end

  defp ng_crypto_init_nif(_Cipher, _Key, _IVec, _EncryptFlag, _Padding) do
    nif_stub_error(1464)
  end

  defp ng_crypto_update_nif(_State, _Data) do
    nif_stub_error(1467)
  end

  defp ng_crypto_update_nif(_State, _Data, _IV) do
    nif_stub_error(1468)
  end

  defp ng_crypto_final_nif(_State) do
    nif_stub_error(1470)
  end

  defp ng_crypto_get_data_nif(_State) do
    nif_stub_error(1472)
  end

  defp ng_crypto_one_time_nif(cipher, key, iVec, data, %{
         :encrypt => encryptFlag,
         :padding => padding
       }) do
    ng_crypto_one_time_nif(cipher, key, iVec, data, encryptFlag, padding)
  end

  defp ng_crypto_one_time_nif(_Cipher, _Key, _IVec, _Data, _EncryptFlag, _Padding) do
    nif_stub_error(1478)
  end

  defp prepend_old_aliases(l0) do
    l1 =
      case :lists.member(:des_ede3_cbc, l0) do
        true ->
          [
            [:des3_cbc, :des_ede3, :des_ede3_cbf, :des3_cbf, :des3_cfb]
            | l0
          ]

        false ->
          l0
      end

    l2 =
      case :lists.member(:aes_128_cbc, l1) do
        true ->
          [[:aes_cbc, :aes_cbc128, :aes_cbc256] | l1]

        false ->
          l1
      end

    l3 =
      case :lists.member(:aes_128_ctr, l2) do
        true ->
          [:aes_ctr | l2]

        false ->
          l2
      end

    l4 =
      case :lists.member(:aes_128_ccm, l3) do
        true ->
          [:aes_ccm | l3]

        false ->
          l3
      end

    l5 =
      case :lists.member(:aes_128_gcm, l4) do
        true ->
          [:aes_gcm | l4]

        false ->
          l4
      end

    l6 =
      case :lists.member(:aes_128_cfb8, l5) do
        true ->
          [:aes_cfb8 | l5]

        false ->
          l5
      end

    l7 =
      case :lists.member(:aes_128_cfb128, l6) do
        true ->
          [:aes_cfb128 | l6]

        false ->
          l6
      end

    l8 =
      case :lists.member(:aes_128_ecb, l7) do
        true ->
          [:aes_ecb | l7]

        false ->
          l7
      end

    l8
  end

  defp alias(:des3_cbc) do
    :des_ede3_cbc
  end

  defp alias(:des_ede3) do
    :des_ede3_cbc
  end

  defp alias(:des_ede3_cbf) do
    :des_ede3_cfb
  end

  defp alias(:des3_cbf) do
    :des_ede3_cfb
  end

  defp alias(:des3_cfb) do
    :des_ede3_cfb
  end

  defp alias(:aes_cbc128) do
    :aes_128_cbc
  end

  defp alias(:aes_cbc256) do
    :aes_256_cbc
  end

  defp alias(alg) do
    alg
  end

  defp alias(ciph, key) do
    alias2(__MODULE__.alias(ciph), key)
  end

  defp alias2(:aes_cbc, key) when :erlang.size(key) == 16 do
    :aes_128_cbc
  end

  defp alias2(:aes_cbc, key) when :erlang.size(key) == 24 do
    :aes_192_cbc
  end

  defp alias2(:aes_cbc, key) when :erlang.size(key) == 32 do
    :aes_256_cbc
  end

  defp alias2(:aes_cfb8, key) when :erlang.size(key) == 16 do
    :aes_128_cfb8
  end

  defp alias2(:aes_cfb8, key) when :erlang.size(key) == 24 do
    :aes_192_cfb8
  end

  defp alias2(:aes_cfb8, key) when :erlang.size(key) == 32 do
    :aes_256_cfb8
  end

  defp alias2(:aes_cfb128, key)
       when :erlang.size(key) == 16 do
    :aes_128_cfb128
  end

  defp alias2(:aes_cfb128, key)
       when :erlang.size(key) == 24 do
    :aes_192_cfb128
  end

  defp alias2(:aes_cfb128, key)
       when :erlang.size(key) == 32 do
    :aes_256_cfb128
  end

  defp alias2(:aes_ctr, key) when :erlang.size(key) == 16 do
    :aes_128_ctr
  end

  defp alias2(:aes_ctr, key) when :erlang.size(key) == 24 do
    :aes_192_ctr
  end

  defp alias2(:aes_ctr, key) when :erlang.size(key) == 32 do
    :aes_256_ctr
  end

  defp alias2(:aes_ecb, key) when :erlang.size(key) == 16 do
    :aes_128_ecb
  end

  defp alias2(:aes_ecb, key) when :erlang.size(key) == 24 do
    :aes_192_ecb
  end

  defp alias2(:aes_ecb, key) when :erlang.size(key) == 32 do
    :aes_256_ecb
  end

  defp alias2(:aes_gcm, key) when :erlang.size(key) == 16 do
    :aes_128_gcm
  end

  defp alias2(:aes_gcm, key) when :erlang.size(key) == 24 do
    :aes_192_gcm
  end

  defp alias2(:aes_gcm, key) when :erlang.size(key) == 32 do
    :aes_256_gcm
  end

  defp alias2(:aes_ccm, key) when :erlang.size(key) == 16 do
    :aes_128_ccm
  end

  defp alias2(:aes_ccm, key) when :erlang.size(key) == 24 do
    :aes_192_ccm
  end

  defp alias2(:aes_ccm, key) when :erlang.size(key) == 32 do
    :aes_256_ccm
  end

  defp alias2(alg, _) do
    alg
  end

  def strong_rand_bytes(bytes) do
    case strong_rand_bytes_nif(bytes) do
      false ->
        :erlang.error(:low_entropy)

      bin ->
        bin
    end
  end

  defp strong_rand_bytes_nif(_Bytes) do
    nif_stub_error(1573)
  end

  def rand_seed() do
    :rand.seed(rand_seed_s())
  end

  def rand_seed_s() do
    rand_seed_alg_s(:crypto)
  end

  def rand_seed_alg(alg) do
    :rand.seed(rand_seed_alg_s(alg))
  end

  def rand_seed_alg(alg, seed) do
    :rand.seed(rand_seed_alg_s(alg, seed))
  end

  def rand_seed_alg_s({algHandler, _AlgState} = state)
      when is_map(algHandler) do
    state
  end

  def rand_seed_alg_s({alg, algState}) when is_atom(alg) do
    {mk_alg_handler(alg), algState}
  end

  def rand_seed_alg_s(alg) when is_atom(alg) do
    {mk_alg_handler(alg), mk_alg_state(alg)}
  end

  def rand_seed_alg_s(alg, seed) when is_atom(alg) do
    {mk_alg_handler(alg), mk_alg_state({alg, seed})}
  end

  defp mk_alg_handler(:crypto = alg) do
    %{
      :type => alg,
      :bits => 64,
      :next => &:crypto.rand_plugin_next/1,
      :uniform => &:crypto.rand_plugin_uniform/1,
      :uniform_n => &:crypto.rand_plugin_uniform/2
    }
  end

  defp mk_alg_handler(:crypto_cache = alg) do
    %{:type => alg, :bits => 56, :next => &:crypto.rand_cache_plugin_next/1}
  end

  defp mk_alg_handler(:crypto_aes = alg) do
    %{
      :type => alg,
      :bits => 58,
      :next => &:crypto.rand_plugin_aes_next/1,
      :jump => &:crypto.rand_plugin_aes_jump/1
    }
  end

  defp mk_alg_state(:crypto) do
    :no_seed
  end

  defp mk_alg_state(:crypto_cache) do
    cacheBits = 56
    bytesPerWord = div(cacheBits + 7, 8)
    genBytes = div(rand_cache_size() + (2 * bytesPerWord - 1), bytesPerWord) * bytesPerWord
    {cacheBits, genBytes, <<>>}
  end

  defp mk_alg_state({:crypto_aes, seed}) do
    genWords = div(rand_cache_size() + 31, 16)
    key = :crypto.hash(:sha256, seed)
    {f, count} = longcount_seed(seed)
    {key, genWords, f, count}
  end

  defp rand_cache_size() do
    defaultCacheSize = 1024
    cacheSize = :application.get_env(:crypto, :rand_cache_size, defaultCacheSize)

    cond do
      is_integer(cacheSize) and 0 <= cacheSize ->
        cacheSize

      true ->
        defaultCacheSize
    end
  end

  def rand_plugin_next(seed) do
    {bytes_to_integer(strong_rand_range(1 <<< 64)), seed}
  end

  def rand_plugin_uniform(state) do
    {strong_rand_float(), state}
  end

  def rand_plugin_uniform(max, state) do
    {bytes_to_integer(strong_rand_range(max)) + 1, state}
  end

  def rand_cache_plugin_next({cacheBits, genBytes, <<>>}) do
    rand_cache_plugin_next({cacheBits, genBytes, strong_rand_bytes(genBytes)})
  end

  def rand_cache_plugin_next({cacheBits, genBytes, cache}) do
    <<i::size(cacheBits), newCache::binary>> = cache
    {i, {cacheBits, genBytes, newCache}}
  end

  def rand_plugin_aes_next([v | cache]) do
    {v, cache}
  end

  def rand_plugin_aes_next({key, genWords, f, count}) do
    rand_plugin_aes_next(key, genWords, f, count)
  end

  def rand_plugin_aes_next({key, genWords, f, _JumpBase, count}) do
    rand_plugin_aes_next(key, genWords, f, count)
  end

  defp rand_plugin_aes_next(key, genWords, f, count) do
    {cleartext, newCount} = aes_cleartext(<<>>, f, count, genWords)
    encrypted = block_encrypt(:aes_ecb, key, cleartext)

    [v | cache] =
      aes_cache(
        encrypted,
        {key, genWords, f, count, newCount}
      )

    {v, cache}
  end

  def rand_plugin_aes_jump({%{:type => :crypto_aes} = alg, cache}) do
    {alg, rand_plugin_aes_jump(&longcount_jump/1, 0, cache)}
  end

  defp rand_plugin_aes_jump(jump, j, [_ | cache]) do
    rand_plugin_aes_jump(jump, j + 1, cache)
  end

  defp rand_plugin_aes_jump(jump, j, {key, genWords, f, jumpBase, _Count}) do
    rand_plugin_aes_jump(jump, genWords - j, key, genWords, f, jumpBase)
  end

  defp rand_plugin_aes_jump(jump, 0, {key, genWords, f, jumpBase}) do
    rand_plugin_aes_jump(jump, 0, key, genWords, f, jumpBase)
  end

  defp rand_plugin_aes_jump(jump, skip, key, genWords, f, jumpBase) do
    count = longcount_next_count(skip, jump.(jumpBase))
    {key, genWords, f, count}
  end

  def rand_plugin_aes_jump_2pow20(cache) do
    rand_plugin_aes_jump(&longcount_jump_2pow20/1, 0, cache)
  end

  defp longcount_seed(seed) do
    <<x::size(64), _::size(6), f::size(12), s2::size(58), s1::size(58), s0::size(58)>> =
      :crypto.hash(:sha256, [seed, "Xoroshiro928"])

    {f,
     :rand.exro928_seed([
       [s0, s1, s2]
       | :rand.seed58(
           13,
           x
         )
     ])}
  end

  defp longcount_next_count(0, count) do
    count
  end

  defp longcount_next_count(n, count) do
    longcount_next_count(
      n - 1,
      :rand.exro928_next_state(count)
    )
  end

  defp longcount_next(count) do
    :rand.exro928_next(count)
  end

  defp longcount_jump(count) do
    :rand.exro928_jump_2pow512(count)
  end

  defp longcount_jump_2pow20(count) do
    :rand.exro928_jump_2pow20(count)
  end

  defp aes_cleartext(cleartext, _F, count, 0) do
    {cleartext, count}
  end

  defp aes_cleartext(cleartext, f, count, genWords) do
    {{s0, s1}, newCount} = longcount_next(count)

    aes_cleartext(
      <<cleartext::binary, f::size(12), s1::size(58), s0::size(58)>>,
      f,
      newCount,
      genWords - 1
    )
  end

  defp aes_cache(<<>>, cache) do
    cache
  end

  defp aes_cache(
         <<_::size(128 - 58), v::size(58), encrypted::binary>>,
         cache
       ) do
    [v | aes_cache(encrypted, cache)]
  end

  defp strong_rand_range(range) when is_integer(range) and range > 0 do
    binRange = int_to_bin(range)
    strong_rand_range(binRange)
  end

  defp strong_rand_range(binRange) when is_binary(binRange) do
    case strong_rand_range_nif(binRange) do
      false ->
        :erlang.error(:low_entropy)

      <<binResult::binary>> ->
        binResult
    end
  end

  defp strong_rand_range_nif(_BinRange) do
    nif_stub_error(1769)
  end

  defp strong_rand_float() do
    wholeRange = strong_rand_range(1 <<< 53)
    1.1102230246251565e-16 * bytes_to_integer(wholeRange)
  end

  def rand_uniform(from, to)
      when is_binary(from) and
             is_binary(to) do
    case rand_uniform_nif(from, to) do
      <<len::size(32)-integer, mSB, rest::binary>>
      when mSB > 127 ->
        <<len + 1::size(32)-integer, 0, mSB, rest::binary>>

      whatever ->
        whatever
    end
  end

  def rand_uniform(from, to)
      when is_integer(from) and
             is_integer(to) do
    cond do
      from < 0 ->
        rand_uniform_pos(0, to - from) + from

      true ->
        rand_uniform_pos(from, to)
    end
  end

  defp rand_uniform_pos(from, to) when from < to do
    binFrom = mpint(from)
    binTo = mpint(to)

    case rand_uniform(binFrom, binTo) do
      result when is_binary(result) ->
        erlint(result)

      other ->
        other
    end
  end

  defp rand_uniform_pos(_, _) do
    :erlang.error(:badarg)
  end

  defp rand_uniform_nif(_From, _To) do
    nif_stub_error(1803)
  end

  def rand_seed(seed) when is_binary(seed) do
    rand_seed_nif(seed)
  end

  defp rand_seed_nif(_Seed) do
    nif_stub_error(1810)
  end

  def sign(algorithm, type, data, key) do
    sign(algorithm, type, data, key, [])
  end

  def sign(algorithm0, type0, data, key, options) do
    {algorithm, type} = sign_verify_compatibility(algorithm0, type0, data)

    case pkey_sign_nif(algorithm, type, data, format_pkey(algorithm, key), options) do
      :error ->
        :erlang.error(
          :badkey,
          [algorithm, type, data, key, options]
        )

      :notsup ->
        :erlang.error(:notsup)

      signature ->
        signature
    end
  end

  defp pkey_sign_nif(_Algorithm, _Type, _Digest, _Key, _Options) do
    nif_stub_error(1875)
  end

  def verify(algorithm, type, data, signature, key) do
    verify(algorithm, type, data, signature, key, [])
  end

  def verify(algorithm0, type0, data, signature, key, options) do
    {algorithm, type} = sign_verify_compatibility(algorithm0, type0, data)

    case pkey_verify_nif(algorithm, type, data, signature, format_pkey(algorithm, key), options) do
      :notsup ->
        :erlang.error(:notsup)

      boolean ->
        boolean
    end
  end

  defp pkey_verify_nif(_Algorithm, _Type, _Data, _Signature, _Key, _Options) do
    nif_stub_error(1922)
  end

  defp sign_verify_compatibility(:dss, :none, digest) do
    {:sha, {:digest, digest}}
  end

  defp sign_verify_compatibility(algorithm0, type0, _Digest) do
    {algorithm0, type0}
  end

  def public_encrypt(algorithm, plainText, publicKey, options) do
    pkey_crypt(algorithm, plainText, publicKey, options, false, true)
  end

  def private_decrypt(algorithm, cipherText, privateKey, options) do
    pkey_crypt(algorithm, cipherText, privateKey, options, true, false)
  end

  def private_encrypt(algorithm, plainText, privateKey, options) do
    pkey_crypt(algorithm, plainText, privateKey, options, true, true)
  end

  def public_decrypt(algorithm, cipherText, publicKey, options) do
    pkey_crypt(algorithm, cipherText, publicKey, options, false, false)
  end

  defp pkey_crypt(:rsa, text, key, padding, pubPriv, encDec)
       when is_atom(padding) do
    pkey_crypt(:rsa, text, key, [{:rsa_padding, padding}], pubPriv, encDec)
  end

  defp pkey_crypt(alg, text, key, options, pubPriv, encDec) do
    case pkey_crypt_nif(alg, text, format_pkey(alg, key), options, pubPriv, encDec) do
      :error when encDec == true ->
        :erlang.error(
          :encrypt_failed,
          [alg, text, key, options]
        )

      :error when encDec == false ->
        :erlang.error(
          :decrypt_failed,
          [alg, text, key, options]
        )

      :notsup ->
        :erlang.error(:notsup)

      out ->
        out
    end
  end

  defp pkey_crypt_nif(_Algorithm, _In, _Key, _Options, _IsPrivate, _IsEncrypt) do
    nif_stub_error(2013)
  end

  def generate_key(type, params) do
    generate_key(type, params, :undefined)
  end

  def generate_key(:dh, dHParameters0, privateKey) do
    {dHParameters, len} =
      case dHParameters0 do
        [p, g, l] ->
          {[p, g], l}

        [p, g] ->
          {[p, g], 0}
      end

    dh_generate_key_nif(
      ensure_int_as_bin(privateKey),
      map_ensure_int_as_bin(dHParameters),
      0,
      len
    )
  end

  def generate_key(:srp, {:host, [verifier, generator, prime, version]}, privArg)
      when is_binary(verifier) and is_binary(generator) and
             is_binary(prime) and is_atom(version) do
    private =
      case privArg do
        :undefined ->
          strong_rand_bytes(32)

        _ ->
          ensure_int_as_bin(privArg)
      end

    host_srp_gen_key(private, verifier, generator, prime, version)
  end

  def generate_key(:srp, {:user, [generator, prime, version]}, privateArg)
      when is_binary(generator) and is_binary(prime) and
             is_atom(version) do
    private =
      case privateArg do
        :undefined ->
          strong_rand_bytes(32)

        _ ->
          privateArg
      end

    user_srp_gen_key(private, generator, prime)
  end

  def generate_key(:rsa, {modulusSize, publicExponent}, :undefined) do
    case rsa_generate_key_nif(
           modulusSize,
           ensure_int_as_bin(publicExponent)
         ) do
      :error ->
        :erlang.error(
          :computation_failed,
          [:rsa, {modulusSize, publicExponent}]
        )

      private ->
        {:lists.sublist(private, 2), private}
    end
  end

  def generate_key(:eddh, curve, privKey)
      when curve == :x448 or
             curve == :x25519 do
    evp_generate_key_nif(curve, ensure_int_as_bin(privKey))
  end

  def generate_key(:ecdh, curve, privKey)
      when curve == :x448 or
             curve == :x25519 do
    evp_generate_key_nif(curve, ensure_int_as_bin(privKey))
  end

  def generate_key(:ecdh, curve, privKey) do
    ec_key_generate(
      nif_curve_params(curve),
      ensure_int_as_bin(privKey)
    )
  end

  def generate_key(:eddsa, curve, privKey)
      when curve == :ed448 or
             curve == :ed25519 do
    evp_generate_key_nif(curve, ensure_int_as_bin(privKey))
  end

  defp evp_generate_key_nif(_Curve, _PrivKey) do
    nif_stub_error(2089)
  end

  def compute_key(:dh, othersPublicKey, myPrivateKey, dHParameters) do
    case dh_compute_key_nif(
           ensure_int_as_bin(othersPublicKey),
           ensure_int_as_bin(myPrivateKey),
           map_ensure_int_as_bin(dHParameters)
         ) do
      :error ->
        :erlang.error(
          :computation_failed,
          [:dh, othersPublicKey, myPrivateKey, dHParameters]
        )

      ret ->
        ret
    end
  end

  def compute_key(
        :srp,
        hostPublic,
        {userPublic, userPrivate},
        {:user,
         [
           [derivedKey, prime, generator, version]
           | scramblerArg
         ]}
      )
      when is_binary(prime) and is_binary(generator) and
             is_atom(version) do
    hostPubBin = ensure_int_as_bin(hostPublic)
    multiplier = srp_multiplier(version, generator, prime)

    scrambler =
      case scramblerArg do
        [] ->
          srp_scrambler(version, ensure_int_as_bin(userPublic), hostPubBin, prime)

        [s] ->
          s
      end

    notsup_to_error(
      srp_user_secret_nif(
        ensure_int_as_bin(userPrivate),
        scrambler,
        hostPubBin,
        multiplier,
        generator,
        derivedKey,
        prime
      )
    )
  end

  def compute_key(
        :srp,
        userPublic,
        {hostPublic, hostPrivate},
        {:host, [[verifier, prime, version] | scramblerArg]}
      )
      when is_binary(verifier) and is_binary(prime) and
             is_atom(version) do
    userPubBin = ensure_int_as_bin(userPublic)

    scrambler =
      case scramblerArg do
        [] ->
          srp_scrambler(version, userPubBin, ensure_int_as_bin(hostPublic), prime)

        [s] ->
          s
      end

    notsup_to_error(
      srp_host_secret_nif(verifier, ensure_int_as_bin(hostPrivate), scrambler, userPubBin, prime)
    )
  end

  def compute_key(:ecdh, others, my, curve)
      when curve == :x448 or
             curve == :x25519 do
    evp_compute_key_nif(curve, ensure_int_as_bin(others), ensure_int_as_bin(my))
  end

  def compute_key(:eddh, others, my, curve)
      when curve == :x448 or
             curve == :x25519 do
    evp_compute_key_nif(curve, ensure_int_as_bin(others), ensure_int_as_bin(my))
  end

  def compute_key(:ecdh, others, my, curve) do
    ecdh_compute_key_nif(
      ensure_int_as_bin(others),
      nif_curve_params(curve),
      ensure_int_as_bin(my)
    )
  end

  defp evp_compute_key_nif(_Curve, _OthersBin, _MyBin) do
    nif_stub_error(2154)
  end

  def exor(bin1, bin2) do
    data1 = :erlang.iolist_to_binary(bin1)
    data2 = :erlang.iolist_to_binary(bin2)
    maxBytes = max_bytes()
    exor(data1, data2, :erlang.byte_size(data1), maxBytes, [])
  end

  def mod_pow(base, exponent, prime) do
    case mod_exp_nif(
           ensure_int_as_bin(base),
           ensure_int_as_bin(exponent),
           ensure_int_as_bin(prime),
           0
         ) do
      <<0>> ->
        :error

      r ->
        r
    end
  end

  def engine_get_all_methods() do
    notsup_to_error(engine_get_all_methods_nif())
  end

  def engine_load(engineId, preCmds, postCmds)
      when is_list(preCmds) and is_list(postCmds) do
    engine_load(engineId, preCmds, postCmds, engine_get_all_methods())
  end

  def engine_load(engineId, preCmds, postCmds, engineMethods)
      when is_list(preCmds) and is_list(postCmds) do
    try do
      :ok = notsup_to_error(engine_load_dynamic_nif())

      case notsup_to_error(engine_by_id_nif(ensure_bin_chardata(engineId))) do
        {:ok, engine} ->
          engine_load_1(engine, preCmds, postCmds, engineMethods)

        {:error, error1} ->
          {:error, error1}
      end
    catch
      error2 ->
        error2
    end
  end

  defp engine_load_1(engine, preCmds, postCmds, engineMethods) do
    try do
      :ok =
        engine_nif_wrapper(
          engine_ctrl_cmd_strings_nif(
            engine,
            ensure_bin_cmds(preCmds),
            0
          )
        )

      :ok = engine_nif_wrapper(engine_init_nif(engine))
      engine_load_2(engine, postCmds, engineMethods)
      {:ok, engine}
    catch
      error ->
        :ok = engine_free_nif(engine)
        throw(error)

      :error, :badarg ->
        :ok = engine_free_nif(engine)
        :erlang.error(:badarg)
    end
  end

  defp engine_load_2(engine, postCmds, engineMethods) do
    try do
      :ok =
        engine_nif_wrapper(
          engine_ctrl_cmd_strings_nif(
            engine,
            ensure_bin_cmds(postCmds),
            0
          )
        )

      for method <- engineMethods do
        :ok =
          engine_nif_wrapper(
            engine_register_nif(
              engine,
              engine_method_atom_to_int(method)
            )
          )
      end

      :ok
    catch
      error ->
        :ok = engine_finish_nif(engine)
        throw(error)
    end
  end

  def engine_unload(engine) do
    engine_unload(engine, engine_get_all_methods())
  end

  defp engine_unload(engine, engineMethods) do
    try do
      for method <- engineMethods do
        :ok =
          engine_nif_wrapper(
            engine_unregister_nif(
              engine,
              engine_method_atom_to_int(method)
            )
          )
      end

      :ok = engine_nif_wrapper(engine_finish_nif(engine))
      :ok = engine_nif_wrapper(engine_free_nif(engine))
    catch
      error ->
        error
    end
  end

  def engine_by_id(engineId) do
    try do
      notsup_to_error(engine_by_id_nif(ensure_bin_chardata(engineId)))
    catch
      error ->
        error
    end
  end

  def engine_add(engine) do
    notsup_to_error(engine_add_nif(engine))
  end

  def engine_remove(engine) do
    notsup_to_error(engine_remove_nif(engine))
  end

  def engine_get_id(engine) do
    notsup_to_error(engine_get_id_nif(engine))
  end

  def engine_get_name(engine) do
    notsup_to_error(engine_get_name_nif(engine))
  end

  def engine_list() do
    case notsup_to_error(engine_get_first_nif()) do
      {:ok, <<>>} ->
        []

      {:ok, engine} ->
        case notsup_to_error(engine_get_id_nif(engine)) do
          <<>> ->
            engine_list(engine, [])

          engineId ->
            engine_list(engine, [engineId])
        end
    end
  end

  defp engine_list(engine0, idList) do
    case notsup_to_error(engine_get_next_nif(engine0)) do
      {:ok, <<>>} ->
        :lists.reverse(idList)

      {:ok, engine1} ->
        case notsup_to_error(engine_get_id_nif(engine1)) do
          <<>> ->
            engine_list(engine1, idList)

          engineId ->
            engine_list(engine1, [engineId | idList])
        end
    end
  end

  def engine_ctrl_cmd_string(engine, cmdName, cmdArg) do
    engine_ctrl_cmd_string(engine, cmdName, cmdArg, false)
  end

  def engine_ctrl_cmd_string(engine, cmdName, cmdArg, optional) do
    case engine_ctrl_cmd_strings_nif(
           engine,
           ensure_bin_cmds([{cmdName, cmdArg}]),
           bool_to_int(optional)
         ) do
      :ok ->
        :ok

      :notsup ->
        :erlang.error(:notsup)

      {:error, error} ->
        {:error, error}
    end
  end

  def ensure_engine_loaded(engineId, libPath) do
    ensure_engine_loaded(engineId, libPath, engine_get_all_methods())
  end

  def ensure_engine_loaded(engineId, libPath, engineMethods) do
    try do
      list = :crypto.engine_list()

      case :lists.member(engineId, list) do
        true ->
          notsup_to_error(engine_by_id_nif(ensure_bin_chardata(engineId)))

        false ->
          :ok = notsup_to_error(engine_load_dynamic_nif())

          case notsup_to_error(engine_by_id_nif(ensure_bin_chardata("dynamic"))) do
            {:ok, engine} ->
              preCommands = [
                {"SO_PATH", ensure_bin_chardata(libPath)},
                {"ID", ensure_bin_chardata(engineId)},
                "LOAD"
              ]

              ensure_engine_loaded_1(engine, preCommands, engineMethods)

            {:error, error1} ->
              {:error, error1}
          end
      end
    catch
      error2 ->
        error2
    end
  end

  defp ensure_engine_loaded_1(engine, preCmds, methods) do
    try do
      :ok =
        engine_nif_wrapper(
          engine_ctrl_cmd_strings_nif(
            engine,
            ensure_bin_cmds(preCmds),
            0
          )
        )

      :ok = engine_nif_wrapper(engine_add_nif(engine))
      :ok = engine_nif_wrapper(engine_init_nif(engine))
      ensure_engine_loaded_2(engine, methods)
      {:ok, engine}
    catch
      error ->
        :ok = engine_free_nif(engine)
        throw(error)
    end
  end

  defp ensure_engine_loaded_2(engine, methods) do
    try do
      for method <- methods do
        :ok =
          engine_nif_wrapper(
            engine_register_nif(
              engine,
              engine_method_atom_to_int(method)
            )
          )
      end

      :ok
    catch
      error ->
        :ok = engine_finish_nif(engine)
        throw(error)
    end
  end

  def ensure_engine_unloaded(engine) do
    ensure_engine_unloaded(engine, engine_get_all_methods())
  end

  def ensure_engine_unloaded(engine, engineMethods) do
    case engine_remove(engine) do
      :ok ->
        engine_unload(engine, engineMethods)

      {:error, e} ->
        {:error, e}
    end
  end

  defp on_load() do
    libBaseName = 'crypto'
    privDir = :code.priv_dir(:crypto)

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
    libBin = path2bin(lib)
    fipsMode = :application.get_env(:crypto, :fips_mode, false) == true

    status =
      case :erlang.load_nif(
             lib,
             {302, libBin, fipsMode}
           ) do
        :ok ->
          :ok

        {:error, {:load_failed, _}} = error1 ->
          archLibDir = :filename.join([privDir, 'lib', :erlang.system_info(:system_architecture)])

          candidate =
            :filelib.wildcard(
              :filename.join([archLibDir, libName ++ '*']),
              :erl_prim_loader
            )

          case candidate do
            [] ->
              error1

            _ ->
              archLib = :filename.join([archLibDir, libName])
              archBin = path2bin(archLib)
              :erlang.load_nif(archLib, {302, archBin, fipsMode})
          end

        error1 ->
          error1
      end

    case status do
      :ok ->
        :ok

      {:error, {e, str}} ->
        fmt = 'Unable to load crypto library. Failed with error:~n"~p, ~s"~n~s'

        extra =
          case e do
            :load_failed ->
              'OpenSSL might not be installed on this system.\n'

            _ ->
              ''
          end

        :error_logger.error_msg(fmt, [e, str, extra])
        status
    end
  end

  defp path2bin(path) when is_list(path) do
    encoding = :file.native_name_encoding()

    case :unicode.characters_to_binary(path, encoding, encoding) do
      bin when is_binary(bin) ->
        bin
    end
  end

  defp max_bytes() do
    20000
  end

  defp notsup_to_error(:notsup) do
    :erlang.error(:notsup)
  end

  defp notsup_to_error(other) do
    other
  end

  defp hash(hash, data, size, max) when size <= max do
    notsup_to_error(hash_nif(hash, data))
  end

  defp hash(hash, data, size, max) do
    state0 = hash_init(hash)
    state1 = hash_update(state0, data, size, max)
    hash_final(state1)
  end

  defp hash_update(state, data, size, maxBytes)
       when size <= maxBytes do
    notsup_to_error(hash_update_nif(state, data))
  end

  defp hash_update(state0, data, _, maxBytes) do
    <<increment::size(maxBytes)-binary, rest::binary>> = data

    state =
      notsup_to_error(
        hash_update_nif(
          state0,
          increment
        )
      )

    hash_update(state, rest, :erlang.byte_size(rest), maxBytes)
  end

  defp hash_info_nif(_Hash) do
    nif_stub_error(2608)
  end

  defp hash_nif(_Hash, _Data) do
    nif_stub_error(2609)
  end

  defp hash_init_nif(_Hash) do
    nif_stub_error(2610)
  end

  defp hash_update_nif(_State, _Data) do
    nif_stub_error(2611)
  end

  defp hash_final_nif(_State) do
    nif_stub_error(2612)
  end

  defp cipher_info_nif(_Type) do
    nif_stub_error(2616)
  end

  defp aead_cipher(_Type, _Key, _Ivec, _AAD, _In, _TagOrTagLength, _EncFlg) do
    nif_stub_error(2622)
  end

  defp aes_ige_crypt_nif(_Key, _IVec, _Data, _IsEncrypt) do
    nif_stub_error(2628)
  end

  defp user_srp_gen_key(private, generator, prime) do
    case info_fips() do
      :enabled ->
        :erlang.error(:notsup)

      _ ->
        :ok
    end

    case mod_pow(generator, private, prime) do
      :error ->
        :error

      public ->
        {public, private}
    end
  end

  defp host_srp_gen_key(private, verifier, generator, prime, version) do
    multiplier = srp_multiplier(version, generator, prime)

    case srp_value_B_nif(multiplier, verifier, generator, private, prime) do
      :error ->
        :error

      :notsup ->
        :erlang.error(:notsup)

      public ->
        {public, private}
    end
  end

  defp srp_multiplier(:"6a", generator, prime) do
    c0 = hash_init(:sha)
    c1 = hash_update(c0, prime)

    c2 =
      hash_update(
        c1,
        srp_pad_to(:erlang.byte_size(prime), generator)
      )

    hash_final(c2)
  end

  defp srp_multiplier(:"6", _, _) do
    <<3::integer>>
  end

  defp srp_multiplier(:"3", _, _) do
    <<1::integer>>
  end

  defp srp_scrambler(version, userPublic, hostPublic, prime)
       when version == :"6" or version == :"6a" do
    padLength = :erlang.byte_size(prime)
    c0 = hash_init(:sha)
    c1 = hash_update(c0, srp_pad_to(padLength, userPublic))
    c2 = hash_update(c1, srp_pad_to(padLength, hostPublic))
    hash_final(c2)
  end

  defp srp_scrambler(:"3", _, hostPublic, _Prime) do
    <<u::size(32)-bits, _::binary>> =
      hash(
        :sha,
        hostPublic
      )

    u
  end

  defp srp_pad_length(width, length) do
    rem(width - rem(length, width), width)
  end

  defp srp_pad_to(width, binary) do
    case srp_pad_length(width, :erlang.size(binary)) do
      0 ->
        binary

      n ->
        <<0::size(n * 8), binary::binary>>
    end
  end

  defp srp_host_secret_nif(_Verifier, _B, _U, _A, _Prime) do
    nif_stub_error(2691)
  end

  defp srp_user_secret_nif(_A, _U, _B, _Multiplier, _Generator, _Exponent, _Prime) do
    nif_stub_error(2693)
  end

  defp srp_value_B_nif(_Multiplier, _Verifier, _Generator, _Exponent, _Prime) do
    nif_stub_error(2695)
  end

  defp rsa_generate_key_nif(_Bits, _Exp) do
    nif_stub_error(2702)
  end

  defp dh_generate_key_nif(_PrivateKey, _DHParameters, _Mpint, _Length) do
    nif_stub_error(2709)
  end

  defp dh_compute_key_nif(_OthersPublicKey, _MyPrivateKey, _DHParameters) do
    nif_stub_error(2713)
  end

  defp ec_key_generate(_Curve, _Key) do
    nif_stub_error(2715)
  end

  defp ecdh_compute_key_nif(_Others, _Curve, _My) do
    nif_stub_error(2717)
  end

  def ec_curves() do
    :crypto_ec_curves.curves()
  end

  def ec_curve(x) do
    :crypto_ec_curves.curve(x)
  end

  def privkey_to_pubkey(alg, engineMap)
      when alg == :rsa or
             alg == :dss or alg == :ecdsa do
    try do
      privkey_to_pubkey_nif(alg, format_pkey(alg, engineMap))
    catch
      :error, :badarg when alg == :ecdsa ->
        {:error, :notsup}

      :error, :badarg ->
        {:error, :not_found}

      :error, :notsup ->
        {:error, :notsup}
    else
      [_ | _] = l ->
        map_ensure_bin_as_int(l)

      x ->
        x
    end
  end

  defp privkey_to_pubkey_nif(_Alg, _EngineMap) do
    nif_stub_error(2749)
  end

  defp term_to_nif_prime({:prime_field, prime}) do
    {:prime_field, ensure_int_as_bin(prime)}
  end

  defp term_to_nif_prime(primeField) do
    primeField
  end

  defp term_to_nif_curve({a, b, seed}) do
    {ensure_int_as_bin(a), ensure_int_as_bin(b), seed}
  end

  defp nif_curve_params({primeField, curve, basePoint, order, coFactor}) do
    {term_to_nif_prime(primeField), term_to_nif_curve(curve), ensure_int_as_bin(basePoint),
     ensure_int_as_bin(order), ensure_int_as_bin(coFactor)}
  end

  defp nif_curve_params(curve) when is_atom(curve) do
    case curve do
      :x448 ->
        {:evp, curve}

      :x25519 ->
        {:evp, curve}

      _ ->
        :crypto_ec_curves.curve(curve)
    end
  end

  defp exor(data1, data2, size, maxByts, [])
       when size <= maxByts do
    do_exor(data1, data2)
  end

  defp exor(data1, data2, size, maxByts, acc)
       when size <= maxByts do
    result = do_exor(data1, data2)
    :erlang.list_to_binary(:lists.reverse([result | acc]))
  end

  defp exor(data1, data2, _Size, maxByts, acc) do
    <<increment1::size(maxByts)-binary, rest1::binary>> = data1
    <<increment2::size(maxByts)-binary, rest2::binary>> = data2
    result = do_exor(increment1, increment2)
    exor(rest1, rest2, :erlang.byte_size(rest1), maxByts, [result | acc])
  end

  defp do_exor(_A, _B) do
    nif_stub_error(2792)
  end

  defp hash_algorithms() do
    nif_stub_error(2794)
  end

  defp pubkey_algorithms() do
    nif_stub_error(2795)
  end

  defp cipher_algorithms() do
    nif_stub_error(2796)
  end

  defp mac_algorithms() do
    nif_stub_error(2797)
  end

  defp curve_algorithms() do
    nif_stub_error(2798)
  end

  defp rsa_opts_algorithms() do
    nif_stub_error(2799)
  end

  defp int_to_bin(x) when x < 0 do
    int_to_bin_neg(x, [])
  end

  defp int_to_bin(x) do
    int_to_bin_pos(x, [])
  end

  defp int_to_bin_pos(0, ds = [_ | _]) do
    :erlang.list_to_binary(ds)
  end

  defp int_to_bin_pos(x, ds) do
    int_to_bin_pos(x >>> 8, [x &&& 255 | ds])
  end

  defp int_to_bin_neg(-1, ds = [mSB | _]) when mSB >= 128 do
    :erlang.list_to_binary(ds)
  end

  defp int_to_bin_neg(x, ds) do
    int_to_bin_neg(x >>> 8, [x &&& 255 | ds])
  end

  def bytes_to_integer(bin) do
    bin_to_int(bin)
  end

  defp bin_to_int(bin) when is_binary(bin) do
    bits = bit_size(bin)
    <<integer::size(bits)-integer>> = bin
    integer
  end

  defp bin_to_int(:undefined) do
    :undefined
  end

  defp map_ensure_int_as_bin([h | _] = list) when is_integer(h) do
    :lists.map(
      fn e ->
        int_to_bin(e)
      end,
      list
    )
  end

  defp map_ensure_int_as_bin(list) do
    list
  end

  defp ensure_int_as_bin(int) when is_integer(int) do
    int_to_bin(int)
  end

  defp ensure_int_as_bin(bin) do
    bin
  end

  defp map_ensure_bin_as_int(list) when is_list(list) do
    :lists.map(&ensure_bin_as_int/1, list)
  end

  defp ensure_bin_as_int(bin) when is_binary(bin) do
    bin_to_int(bin)
  end

  defp ensure_bin_as_int(e) do
    e
  end

  defp format_pkey(_Alg, %{:engine => _, :key_id => t} = m)
       when is_binary(t) do
    format_pwd(m)
  end

  defp format_pkey(_Alg, %{:engine => _, :key_id => t} = m)
       when is_list(t) do
    format_pwd(%{m | :key_id => :erlang.list_to_binary(t)})
  end

  defp format_pkey(_Alg, %{:engine => _} = m) do
    :erlang.error({:bad_key_id, m})
  end

  defp format_pkey(_Alg, %{} = m) do
    :erlang.error({:bad_engine_map, m})
  end

  defp format_pkey(:rsa, key) do
    map_ensure_int_as_bin(key)
  end

  defp format_pkey(:ecdsa, [key, curve]) do
    {nif_curve_params(curve), ensure_int_as_bin(key)}
  end

  defp format_pkey(:dss, key) do
    map_ensure_int_as_bin(key)
  end

  defp format_pkey(_, key) do
    key
  end

  defp format_pwd(%{:password => pwd} = m) when is_list(pwd) do
    %{m | :password => :erlang.list_to_binary(pwd)}
  end

  defp format_pwd(m) do
    m
  end

  defp mpint(x) when x < 0 do
    mpint_neg(x)
  end

  defp mpint(x) do
    mpint_pos(x)
  end

  defp mpint_neg(x) do
    bin = int_to_bin_neg(x, [])
    sz = byte_size(bin)
    <<sz::size(32)-unsigned-big-integer, bin::binary>>
  end

  defp mpint_pos(x) do
    bin = int_to_bin_pos(x, [])
    <<mSB, _::binary>> = bin
    sz = byte_size(bin)

    cond do
      mSB &&& 128 == 128 ->
        <<sz + 1::size(32)-unsigned-big-integer, 0, bin::binary>>

      true ->
        <<sz::size(32)-unsigned-big-integer, bin::binary>>
    end
  end

  defp erlint(<<mPIntSize::size(32)-integer, mPIntValue::binary>>) do
    bits = mPIntSize * 8
    <<integer::size(bits)-integer>> = mPIntValue
    integer
  end

  defp mod_exp_nif(_Base, _Exp, _Mod, _bin_hdr) do
    nif_stub_error(2896)
  end

  def packed_openssl_version(mAJ, mIN, fIX, p0) do
    p1 = :erlang.atom_to_list(p0)

    p =
      :lists.sum(
        for c <- p1 do
          c - ?a
        end
      )

    mAJ <<< 8 ||| mIN <<< 8 ||| fIX <<< 8 ||| (p + 1) <<< 4 ||| 15
  end

  defp engine_by_id_nif(_EngineId) do
    nif_stub_error(2911)
  end

  defp engine_init_nif(_Engine) do
    nif_stub_error(2912)
  end

  defp engine_finish_nif(_Engine) do
    nif_stub_error(2913)
  end

  defp engine_free_nif(_Engine) do
    nif_stub_error(2914)
  end

  defp engine_load_dynamic_nif() do
    nif_stub_error(2915)
  end

  defp engine_ctrl_cmd_strings_nif(_Engine, _Cmds, _Optional) do
    nif_stub_error(2916)
  end

  defp engine_add_nif(_Engine) do
    nif_stub_error(2917)
  end

  defp engine_remove_nif(_Engine) do
    nif_stub_error(2918)
  end

  defp engine_register_nif(_Engine, _EngineMethod) do
    nif_stub_error(2919)
  end

  defp engine_unregister_nif(_Engine, _EngineMethod) do
    nif_stub_error(2920)
  end

  defp engine_get_first_nif() do
    nif_stub_error(2921)
  end

  defp engine_get_next_nif(_Engine) do
    nif_stub_error(2922)
  end

  defp engine_get_id_nif(_Engine) do
    nif_stub_error(2923)
  end

  defp engine_get_name_nif(_Engine) do
    nif_stub_error(2924)
  end

  defp engine_get_all_methods_nif() do
    nif_stub_error(2925)
  end

  defp engine_nif_wrapper(:ok) do
    :ok
  end

  defp engine_nif_wrapper(:notsup) do
    :erlang.error(:notsup)
  end

  defp engine_nif_wrapper({:error, error}) do
    throw({:error, error})
  end

  defp ensure_bin_chardata(charData) when is_binary(charData) do
    charData
  end

  defp ensure_bin_chardata(charData) do
    :unicode.characters_to_binary(charData)
  end

  defp ensure_bin_cmds(cMDs) do
    ensure_bin_cmds(cMDs, [])
  end

  defp ensure_bin_cmds([], acc) do
    :lists.reverse(acc)
  end

  defp ensure_bin_cmds([{key, value} | cMDs], acc) do
    ensure_bin_cmds(
      cMDs,
      [
        {ensure_bin_chardata(key), ensure_bin_chardata(value)}
        | acc
      ]
    )
  end

  defp ensure_bin_cmds([key | cMDs], acc) do
    ensure_bin_cmds(
      cMDs,
      [{ensure_bin_chardata(key), ""} | acc]
    )
  end

  def engine_methods_convert_to_bitmask([], bitMask) do
    bitMask
  end

  def engine_methods_convert_to_bitmask(:engine_method_all, _BitMask) do
    65535
  end

  def engine_methods_convert_to_bitmask(:engine_method_none, _BitMask) do
    0
  end

  def engine_methods_convert_to_bitmask([m | ms], bitMask) do
    engine_methods_convert_to_bitmask(
      ms,
      bitMask ||| engine_method_atom_to_int(m)
    )
  end

  defp bool_to_int(true) do
    1
  end

  defp bool_to_int(false) do
    0
  end

  defp engine_method_atom_to_int(:engine_method_rsa) do
    1
  end

  defp engine_method_atom_to_int(:engine_method_dsa) do
    2
  end

  defp engine_method_atom_to_int(:engine_method_dh) do
    4
  end

  defp engine_method_atom_to_int(:engine_method_rand) do
    8
  end

  defp engine_method_atom_to_int(:engine_method_ecdh) do
    16
  end

  defp engine_method_atom_to_int(:engine_method_ecdsa) do
    32
  end

  defp engine_method_atom_to_int(:engine_method_ciphers) do
    64
  end

  defp engine_method_atom_to_int(:engine_method_digests) do
    128
  end

  defp engine_method_atom_to_int(:engine_method_store) do
    256
  end

  defp engine_method_atom_to_int(:engine_method_pkey_meths) do
    512
  end

  defp engine_method_atom_to_int(:engine_method_pkey_asn1_meths) do
    1024
  end

  defp engine_method_atom_to_int(:engine_method_ec) do
    2048
  end

  defp engine_method_atom_to_int(x) do
    :erlang.error(:badarg, [x])
  end

  def get_test_engine() do
    type = :erlang.system_info(:system_architecture)
    libDir = :filename.join([:code.priv_dir(:crypto), 'lib'])
    archDir = :filename.join([libDir, type])

    case :filelib.is_dir(archDir) do
      true ->
        check_otp_test_engine(archDir)

      false ->
        check_otp_test_engine(libDir)
    end
  end

  defp check_otp_test_engine(libDir) do
    case :filelib.wildcard('otp_test_engine*', libDir) do
      [] ->
        {:error, :notexist}

      [libName | _] ->
        libPath = :filename.join(libDir, libName)

        case :filelib.is_file(libPath) do
          true ->
            {:ok, :unicode.characters_to_binary(libPath)}

          false ->
            {:error, :notexist}
        end
    end
  end
end
