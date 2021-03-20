defmodule :m_hipe_unified_loader do
  use Bitwise

  def chunk_name(architecture) do
    case architecture do
      :amd64 ->
        'HA64'

      :arm ->
        'HARM'

      :powerpc ->
        'HPPC'

      :ppc64 ->
        'HP64'

      :ultrasparc ->
        'HS8P'

      :x86 ->
        'HX86'
    end
  end

  defp word_size(architecture) do
    case architecture do
      :amd64 ->
        8

      :ppc64 ->
        8

      _ ->
        4
    end
  end

  def load_native_code(_Mod, _Bin, :undefined) do
    :no_native
  end

  def load_native_code(mod, bin, architecture)
      when is_atom(mod) and
             is_binary(bin) do
    case :code.get_chunk(bin, chunk_name(architecture)) do
      :undefined ->
        :no_native

      nativeCode when is_binary(nativeCode) ->
        :erlang.system_flag(:multi_scheduling, :block_normal)

        try do
          :erlang.put(:hipe_patch_closures, false)

          case load_common(mod, nativeCode, bin, architecture) do
            :bad_crc ->
              :no_native

            result ->
              result
          end
        after
          :erlang.system_flag(:multi_scheduling, :unblock_normal)
        end
    end
  end

  def load_module(mod, bin, beam, architecture) do
    :erlang.system_flag(:multi_scheduling, :block_normal)

    try do
      :erlang.put(:hipe_patch_closures, false)
      load_common(mod, bin, beam, architecture)
    after
      :erlang.system_flag(:multi_scheduling, :unblock_normal)
    end
  end

  defp find_callee_mfas(patches, architecture) when is_list(patches) do
    case needs_trampolines(architecture) do
      true ->
        find_callee_mfas(patches, :gb_sets.empty(), no_erts_trampolines(architecture))

      _ ->
        []
    end
  end

  defp needs_trampolines(architecture) do
    case architecture do
      :arm ->
        true

      :powerpc ->
        true

      :ppc64 ->
        true

      :amd64 ->
        true

      _ ->
        false
    end
  end

  defp no_erts_trampolines(architecture) do
    case architecture do
      :powerpc ->
        true

      :ppc64 ->
        true

      _ ->
        false
    end
  end

  defp find_callee_mfas([{type, data} | patches], mFAs, skipErtsSyms) do
    newMFAs =
      case (case type do
              0 ->
                :load_atom

              1 ->
                :load_address

              4 ->
                :sdesc

              5 ->
                :x86_abs_pcrel

              2 ->
                :call_remote

              3 ->
                :call_local
            end) do
        :call_local ->
          add_callee_mfas(data, mFAs, skipErtsSyms)

        :call_remote ->
          add_callee_mfas(data, mFAs, skipErtsSyms)

        _ ->
          mFAs
      end

    find_callee_mfas(patches, newMFAs, skipErtsSyms)
  end

  defp find_callee_mfas([], mFAs, _SkipErtsSyms) do
    :erlang.list_to_tuple(:gb_sets.to_list(mFAs))
  end

  defp add_callee_mfas([{destMFA, _Offsets} | refs], mFAs, skipErtsSyms) do
    newMFAs =
      case skipErtsSyms do
        true ->
          case bif_address(destMFA) do
            false ->
              :gb_sets.add_element(destMFA, mFAs)

            bifAddress when is_integer(bifAddress) ->
              mFAs
          end

        false ->
          :gb_sets.add_element(destMFA, mFAs)
      end

    add_callee_mfas(refs, newMFAs, skipErtsSyms)
  end

  defp add_callee_mfas([], mFAs, _SkipErtsSyms) do
    mFAs
  end

  defp mk_trampoline_map([], [], _) do
    []
  end

  defp mk_trampoline_map(calleeMFAs, trampolines, architecture) do
    sizeofLong = word_size(architecture)

    mk_trampoline_map(
      tuple_size(calleeMFAs),
      calleeMFAs,
      trampolines,
      sizeofLong,
      :gb_trees.empty()
    )
  end

  defp mk_trampoline_map(i, calleeMFAs, trampolines, sizeofLong, map)
       when i >= 1 do
    mFA = :erlang.element(i, calleeMFAs)
    skip = (i - 1) * sizeofLong

    <<_::size(skip)-binary-unit(8), trampoline::size(sizeofLong)-integer-unsigned-native-unit(8),
      _::binary>> = trampolines

    newMap = :gb_trees.insert(mFA, trampoline, map)
    mk_trampoline_map(i - 1, calleeMFAs, trampolines, sizeofLong, newMap)
  end

  defp mk_trampoline_map(0, _, _, _, map) do
    map
  end

  defp trampoline_map_get(_, []) do
    []
  end

  defp trampoline_map_get(mFA, map) do
    :gb_trees.get(mFA, map)
  end

  defp trampoline_map_lookup(_, []) do
    []
  end

  defp trampoline_map_lookup(primop, map) do
    case :gb_trees.lookup(primop, map) do
      {:value, x} ->
        x

      _ ->
        []
    end
  end

  require Record

  Record.defrecord(:r_fundef, :fundef,
    address: :undefined,
    mfa: :undefined,
    is_closure: :undefined,
    is_exported: :undefined
  )

  defp exports(exportMap, baseAddress) do
    exports(exportMap, baseAddress, [])
  end

  defp exports([offset, m, f, a, isClosure, isExported | rest], baseAddress, funDefs) do
    case isExported and :erlang.is_builtin(m, f, a) do
      true ->
        exports(rest, baseAddress, funDefs)

      _false ->
        mFA = {m, f, a}
        address = baseAddress + offset

        funDef =
          r_fundef(address: address, mfa: mFA, is_closure: isClosure, is_exported: isExported)

        exports(rest, baseAddress, [funDef | funDefs])
    end
  end

  defp exports([], _, funDefs) do
    funDefs
  end

  defp mod({m, _F, _A}) do
    m
  end

  defp calculate_addresses(patchOffsets, base, funDefs) do
    remoteOrLocal = :local

    for {{destMFA, _, _} = data, offsets} <- patchOffsets do
      {data, offsets_to_addresses(offsets, base),
       get_native_address(destMFA, funDefs, remoteOrLocal)}
    end
  end

  defp offsets_to_addresses(os, base) do
    for o <- os do
      {o + base, :load_fe}
    end
  end

  defp find_closure_patches([{type, refs} | rest]) do
    case (case type do
            0 ->
              :load_atom

            1 ->
              :load_address

            4 ->
              :sdesc

            5 ->
              :x86_abs_pcrel

            2 ->
              :call_remote

            3 ->
              :call_local
          end) do
      :load_address ->
        find_closure_refs(refs, rest)

      _ ->
        find_closure_patches(rest)
    end
  end

  defp find_closure_patches([]) do
    []
  end

  defp find_closure_refs([{dest, offsets} | rest], refs) do
    case dest do
      {:closure, data} ->
        [{data, offsets} | find_closure_refs(rest, refs)]

      _ ->
        find_closure_refs(rest, refs)
    end
  end

  defp find_closure_refs([], refs) do
    find_closure_patches(refs)
  end

  defp make_beam_stub(mod, loaderState, mD5, beam, funDefs, closuresToPatch) do
    fs =
      for r_fundef(
            address: address,
            mfa: {_M, f, a}
          ) <- funDefs do
        {f, a, address}
      end

    ^mod = :code.make_stub_module(loaderState, beam, {fs, closuresToPatch, mD5})
    :ok
  end

  defp patch_call(
         [{destMFA, offsets} | sortedRefs],
         baseAddress,
         funDefs,
         remoteOrLocal,
         trampolineMap
       ) do
    case bif_address(destMFA) do
      false ->
        destAddress = get_native_address(destMFA, funDefs, remoteOrLocal)
        trampoline = trampoline_map_get(destMFA, trampolineMap)

        patch_mfa_call_list(
          offsets,
          baseAddress,
          destMFA,
          destAddress,
          funDefs,
          remoteOrLocal,
          trampoline
        )

      bifAddress when is_integer(bifAddress) ->
        trampoline =
          trampoline_map_lookup(
            destMFA,
            trampolineMap
          )

        patch_bif_call_list(offsets, baseAddress, bifAddress, trampoline)
    end

    patch_call(sortedRefs, baseAddress, funDefs, remoteOrLocal, trampolineMap)
  end

  defp patch_call([], _, _, _, _) do
    :ok
  end

  defp patch_all(type, [{dest, offsets} | rest], baseAddress, constAndZone, funDefs) do
    patch_all_offsets(type, dest, offsets, baseAddress, constAndZone, funDefs)
    patch_all(type, rest, baseAddress, constAndZone, funDefs)
  end

  defp patch_all(_, [], _, _, _) do
    :ok
  end

  defp patch_offset(type, data, address, constAndZone, funDefs) do
    case type do
      :load_address ->
        patch_load_address(data, address, constAndZone, funDefs)

      :load_atom ->
        atom = data
        patch_atom(address, atom)

      :sdesc ->
        patch_sdesc(data, address, constAndZone, funDefs)

      :x86_abs_pcrel ->
        patch_instr(address, data, :x86_abs_pcrel)
    end
  end

  defp patch_load_address(data, address, constAndZone, funDefs) do
    case data do
      {:local_function, destMFA} ->
        patch_load_mfa(address, destMFA, funDefs, :local)

      {:remote_function, destMFA} ->
        patch_load_mfa(address, destMFA, funDefs, :remote)

      {:constant, name} ->
        {constMap2, _CodeAddress} = constAndZone
        constAddress = find_const(name, constMap2)
        patch_instr(address, constAddress, :constant)

      {:closure, {destMFA, uniq, index}} ->
        patch_closure(destMFA, uniq, index, address, funDefs)

      {:c_const, cConst} ->
        patch_instr(address, bif_address(cConst), :c_const)
    end
  end

  defp patch_consts(labels, dataAddress, codeAddress, writeWord) do
    :lists.foreach(
      fn l ->
        patch_label_or_labels(l, dataAddress, codeAddress, writeWord)
      end,
      labels
    )
  end

  defp sort_on_representation(list) do
    :lists.sort(
      for {term, offset} <- list do
        {:hipe_bifs.term_to_word(term), offset}
      end
    )
  end

  defp patch_instr(address, value, type) do
    :hipe_bifs.patch_insn(address, value, type)
  end

  defp write_word_fun(wordSize) do
    case wordSize do
      8 ->
        fn dataAddress, dataWord ->
          :hipe_bifs.write_u64(dataAddress, dataWord)
          dataAddress + 8
        end

      4 ->
        fn dataAddress, dataWord ->
          :hipe_bifs.write_u32(dataAddress, dataWord)
          dataAddress + 4
        end
    end
  end

  defp bif_address({m, f, a}) do
    :hipe_bifs.bif_address(m, f, a)
  end

  defp bif_address(name) when is_atom(name) do
    :hipe_bifs.primop_address(name)
  end

  defp create_data_segment(dataAlign, dataSize, dataList, writeWord, loaderState) do
    dataAddress = :hipe_bifs.alloc_data(dataAlign, dataSize, loaderState)
    enter_data(dataList, [], dataAddress, dataSize, writeWord)
  end

  defp enter_datum(type, data, address, writeWord) do
    case (case type do
            0 ->
              :term

            2 ->
              :sorted_block

            1 ->
              :block
          end) do
      :term ->
        :hipe_bifs.term_to_word(:hipe_bifs.merge_term(data))

      :sorted_block ->
        l =
          :lists.sort(
            for term <- data do
              :hipe_bifs.term_to_word(term)
            end
          )

        write_words(l, address, writeWord)
        address

      :block ->
        case data do
          {lbls, []} ->
            write_bytes(lbls, address)

          {lbls, sortOrder} ->
            sortedLbls =
              for {_, lbl} <-
                    :lists.sort(
                      group(
                        lbls,
                        sortOrder
                      )
                    ) do
                lbl
              end

            write_words(sortedLbls, address, writeWord)

          lbls ->
            write_bytes(lbls, address)
        end

        address
    end
  end

  defp group([], []) do
    []
  end

  defp group([b1, b2, b3, b4 | ls], [o | os]) do
    [
      {:hipe_bifs.term_to_word(o), bytes_to_32(b4, b3, b2, b1)}
      | group(ls, os)
    ]
  end

  defp bytes_to_32(b4, b3, b2, b1) do
    b4 <<< 24 ||| b3 <<< 16 ||| b2 <<< 8 ||| b1
  end

  defp write_words([w | rest], addr, writeWord) do
    write_words(rest, writeWord.(addr, w), writeWord)
  end

  defp write_words([], addr, _) when is_integer(addr) do
    true
  end

  defp write_bytes([b | rest], addr) do
    :hipe_bifs.write_u8(addr, b)
    write_bytes(rest, addr + 1)
  end

  defp write_bytes([], addr) when is_integer(addr) do
    true
  end

  defp add_ref(calleeMFA, address, funDefs, refType, trampoline, remoteOrLocal) do
    callerMFA = address_to_mfa_lth(address, funDefs)

    case remoteOrLocal do
      :local ->
        {m, _, _} = calleeMFA
        {^m, _, _} = callerMFA
        :ok

      :remote ->
        :hipe_bifs.add_ref(
          calleeMFA,
          {callerMFA, address, refType, trampoline, :erlang.get(:hipe_loader_state)}
        )
    end
  end

  defp address_to_mfa_lth(address, [r_fundef(address: adr, mfa: mFA) | rest], prev) do
    cond do
      address < adr ->
        prev

      true ->
        address_to_mfa_lth(address, rest, mFA)
    end
  end

  defp address_to_mfa_lth(_Address, [], prev) do
    prev
  end

  defp mfa_to_address(
         mFA,
         [
           r_fundef(address: adr, mfa: mFA, is_exported: isExported)
           | _Rest
         ],
         remoteOrLocal
       ) do
    case remoteOrLocal do
      :local ->
        adr

      :remote ->
        case isExported do
          true ->
            adr

          false ->
            false
        end
    end
  end

  defp mfa_to_address(mFA, [_ | rest], remoteOrLocal) do
    mfa_to_address(mFA, rest, remoteOrLocal)
  end

  defp mfa_to_address(_, [], _) do
    false
  end

  defp assert_local_patch(address) when is_integer(address) do
    {first, last} = :erlang.get(:hipe_assert_code_area)
    address >= first and address < last
  end

  defp enter_code(codeSize, codeBinary, calleeMFAs, loaderState) do
    true = byte_size(codeBinary) === codeSize
    {codeAddress, trampolines} = :hipe_bifs.enter_code(codeBinary, calleeMFAs, loaderState)

    :erlang.put(
      :hipe_assert_code_area,
      {codeAddress, codeAddress + byte_size(codeBinary)}
    )

    {codeAddress, trampolines}
  end
end
