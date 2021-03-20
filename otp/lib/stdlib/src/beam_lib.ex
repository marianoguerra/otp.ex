defmodule :m_beam_lib do
  use Bitwise

  import :lists,
    only: [
      append: 1,
      delete: 2,
      foreach: 2,
      keysort: 2,
      member: 2,
      reverse: 1,
      sort: 1,
      splitwith: 2
    ]

  @behaviour :gen_server
  def info(file) do
    read_info(beam_filename(file))
  end

  def chunks(file, chunks) do
    read_chunk_data(file, chunks)
  end

  def chunks(file, chunks, options) do
    try do
      read_chunk_data(file, chunks, options)
    catch
      error ->
        error
    end
  end

  def all_chunks(file) do
    read_all_chunks(file)
  end

  def cmp(file1, file2) do
    try do
      cmp_files(file1, file2)
    catch
      error ->
        error
    end
  end

  def cmp_dirs(dir1, dir2) do
    try do
      compare_dirs(dir1, dir2)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  def diff_dirs(dir1, dir2) do
    try do
      diff_directories(dir1, dir2)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  def strip(fileName) do
    strip(fileName, [])
  end

  def strip(fileName, additionalChunks) do
    try do
      strip_file(fileName, additionalChunks)
    catch
      error ->
        error
    end
  end

  def strip_files(files) do
    strip_files(files, [])
  end

  def strip_files(files, additionalChunks) when is_list(files) do
    try do
      strip_fils(files, additionalChunks)
    catch
      error ->
        error
    end
  end

  def strip_release(root) do
    strip_release(root, [])
  end

  def strip_release(root, additionalChunks) do
    try do
      strip_rel(root, additionalChunks)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  def version(file) do
    case (try do
            read_chunk_data(file, [:attributes])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, {module, [{:attributes, attrs}]}} ->
        {:vsn, version} = :lists.keyfind(:vsn, 1, attrs)
        {:ok, {module, version}}

      error ->
        error
    end
  end

  def md5(file) do
    case (try do
            read_significant_chunks(file, md5_chunks())
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, {module, chunks0}} ->
        chunks = filter_funtab(chunks0)

        {:ok,
         {module,
          :erlang.md5(
            for {_Id, c} <- chunks do
              c
            end
          )}}

      error ->
        error
    end
  end

  def format_error({:error, error}) do
    format_error(error)
  end

  def format_error({:error, module, error}) do
    module.format_error(error)
  end

  def format_error({:unknown_chunk, file, chunkName}) do
    :io_lib.format('~tp: Cannot find chunk ~p~n', [file, chunkName])
  end

  def format_error({:invalid_chunk, file, chunkId}) do
    :io_lib.format('~tp: Invalid contents of chunk ~p~n', [file, chunkId])
  end

  def format_error({:not_a_beam_file, file}) do
    :io_lib.format('~tp: Not a BEAM file~n', [file])
  end

  def format_error({:file_error, file, reason}) do
    :io_lib.format('~tp: ~tp~n', [file, :file.format_error(reason)])
  end

  def format_error({:missing_chunk, file, chunkId}) do
    :io_lib.format('~tp: Not a BEAM file: no IFF "~s" chunk~n', [file, chunkId])
  end

  def format_error({:invalid_beam_file, file, pos}) do
    :io_lib.format('~tp: Invalid format of BEAM file near byte number ~p~n', [file, pos])
  end

  def format_error({:chunk_too_big, file, chunkId, size, len}) do
    :io_lib.format('~tp: Size of chunk "~s" is ~p bytes, but only ~p bytes could be read~n', [
      file,
      chunkId,
      size,
      len
    ])
  end

  def format_error({:chunks_different, id}) do
    :io_lib.format('Chunk "~s" differs in the two files~n', [id])
  end

  def format_error(:different_chunks) do
    'The two files have different chunks\n'
  end

  def format_error({:modules_different, module1, module2}) do
    :io_lib.format('Module names ~p and ~p differ in the two files~n', [module1, module2])
  end

  def format_error({:not_a_directory, name}) do
    :io_lib.format('~tp: Not a directory~n', [name])
  end

  def format_error({:key_missing_or_invalid, file, chunkId}) do
    :io_lib.format('~tp: Cannot decrypt ~ts because key is missing or invalid', [file, chunkId])
  end

  def format_error(:badfun) do
    'not a fun or the fun has the wrong arity'
  end

  def format_error(:exists) do
    'a fun has already been installed'
  end

  def format_error(e) do
    :io_lib.format('~tp~n', [e])
  end

  def crypto_key_fun(f) do
    call_crypto_server({:crypto_key_fun, f})
  end

  def clear_crypto_key_fun() do
    call_crypto_server(:clear_crypto_key_fun)
  end

  def make_crypto_key(:des3_cbc = type, string) do
    <<k1::size(8)-binary, k2::size(8)-binary>> = first = :erlang.md5(string)

    <<k3::size(8)-binary, iVec::size(8)-binary>> =
      :erlang.md5([
        first
        | reverse(string)
      ])

    {type, [k1, k2, k3], iVec, 8}
  end

  def build_module(chunks0) do
    chunks = :erlang.list_to_binary(build_chunks(chunks0))
    size = byte_size(chunks)
    0 = rem(size, 4)
    {:ok, <<"FOR1", size + 4::size(32), "BEAM", chunks::binary>>}
  end

  defp read_info(file) do
    try do
      {:ok, module, data} = scan_beam(file, :info)

      [
        cond do
          is_binary(file) ->
            {:binary, file}

          true ->
            {:file, file}
        end,
        {:module, module},
        {:chunks, data}
      ]
    catch
      error ->
        error
    end
  end

  defp diff_directories(dir1, dir2) do
    {onlyDir1, onlyDir2, diff} = compare_dirs(dir1, dir2)
    diff_only(dir1, onlyDir1)
    diff_only(dir2, onlyDir2)

    foreach(
      fn d ->
        :io.format('** different: ~tp~n', [d])
      end,
      diff
    )

    :ok
  end

  defp diff_only(_Dir, []) do
    :ok
  end

  defp diff_only(dir, only) do
    :io.format('Only in ~tp: ~tp~n', [dir, only])
  end

  defp compare_dirs(dir1, dir2) do
    r1 = :sofs.relation(beam_files(dir1))
    r2 = :sofs.relation(beam_files(dir2))
    f1 = :sofs.domain(r1)
    f2 = :sofs.domain(r2)
    {o1, both, o2} = :sofs.symmetric_partition(f1, f2)
    onlyL1 = :sofs.image(r1, o1)
    onlyL2 = :sofs.image(r2, o2)
    b1 = :sofs.to_external(:sofs.restriction(r1, both))
    b2 = :sofs.to_external(:sofs.restriction(r2, both))
    diff = compare_files(b1, b2, [])
    {:sofs.to_external(onlyL1), :sofs.to_external(onlyL2), diff}
  end

  defp compare_files([], [], acc) do
    :lists.reverse(acc)
  end

  defp compare_files([{_, f1} | r1], [{_, f2} | r2], acc) do
    nAcc =
      case (try do
              cmp_files(f1, f2)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:error, _Mod, _Reason} ->
          [{f1, f2} | acc]

        :ok ->
          acc
      end

    compare_files(r1, r2, nAcc)
  end

  defp beam_files(dir) do
    :ok = assert_directory(dir)
    l = :filelib.wildcard(:filename.join(dir, '*.beam'))

    for path <- l do
      {:filename.basename(path), path}
    end
  end

  defp cmp_files(file1, file2) do
    {:ok, {m1, l1}} = read_all_but_useless_chunks(file1)
    {:ok, {m2, l2}} = read_all_but_useless_chunks(file2)

    cond do
      m1 === m2 ->
        cmp_lists(l1, l2)

      true ->
        :erlang.error({:modules_different, m1, m2})
    end
  end

  defp cmp_lists([], []) do
    :ok
  end

  defp cmp_lists([{id, c1} | r1], [{id, c2} | r2]) do
    cond do
      c1 === c2 ->
        cmp_lists(r1, r2)

      true ->
        :erlang.error({:chunks_different, id})
    end
  end

  defp cmp_lists(_, _) do
    :erlang.error(:different_chunks)
  end

  defp strip_rel(root, additionalChunks) do
    :ok = assert_directory(root)

    strip_fils(
      :filelib.wildcard(:filename.join(root, 'lib/*/ebin/*.beam')),
      additionalChunks
    )
  end

  defp strip_fils(files, additionalChunks) do
    {:ok,
     for f <- files do
       {:ok, reply} = strip_file(f, additionalChunks)
       reply
     end}
  end

  defp strip_file(file, additionalChunks) do
    {:ok, {mod, chunks}} =
      read_significant_chunks(
        file,
        additionalChunks ++ significant_chunks()
      )

    {:ok, stripped0} = build_module(chunks)
    stripped = compress(stripped0)

    case file do
      _ when is_binary(file) ->
        {:ok, {mod, stripped}}

      _ ->
        fileName = beam_filename(file)

        case :file.open(fileName, [:raw, :binary, :write]) do
          {:ok, fd} ->
            case :file.write(fd, stripped) do
              :ok ->
                :ok = :file.close(fd)
                {:ok, {mod, fileName}}

              error ->
                :ok = :file.close(fd)
                file_error(fileName, error)
            end

          error ->
            file_error(fileName, error)
        end
    end
  end

  defp build_chunks([{id, data} | chunks]) do
    bId = :erlang.list_to_binary(id)
    size = byte_size(data)

    chunk = [
      <<bId::binary, size::size(32)>>,
      data
      | pad(size)
    ]

    [chunk | build_chunks(chunks)]
  end

  defp build_chunks([]) do
    []
  end

  defp pad(size) do
    case rem(size, 4) do
      0 ->
        []

      rem ->
        :lists.duplicate(4 - rem, 0)
    end
  end

  defp read_all_but_useless_chunks(file0)
       when is_atom(file0) or is_list(file0) or
              is_binary(file0) do
    file = beam_filename(file0)
    {:ok, module, chunkIds0} = scan_beam(file, :info)

    chunkIds =
      for {name, _, _} <- chunkIds0,
          not is_useless_chunk(name) do
        name
      end

    {:ok, ^module, chunks} = scan_beam(file, chunkIds)
    {:ok, {module, :lists.reverse(chunks)}}
  end

  defp is_useless_chunk('CInf') do
    true
  end

  defp is_useless_chunk(_) do
    false
  end

  defp read_significant_chunks(file, chunkList) do
    case read_chunk_data(file, chunkList, [:allow_missing_chunks]) do
      {:ok, {module, chunks0}} ->
        mandatory = mandatory_chunks()
        chunks = filter_significant_chunks(chunks0, mandatory, file, module)
        {:ok, {module, chunks}}
    end
  end

  defp filter_significant_chunks([{_, data} = pair | cs], mandatory, file, mod)
       when is_binary(data) do
    [pair | filter_significant_chunks(cs, mandatory, file, mod)]
  end

  defp filter_significant_chunks([{id, :missing_chunk} | cs], mandatory, file, mod) do
    case member(id, mandatory) do
      false ->
        filter_significant_chunks(cs, mandatory, file, mod)

      true ->
        :erlang.error({:missing_chunk, file, id})
    end
  end

  defp filter_significant_chunks([], _, _, _) do
    []
  end

  defp filter_funtab([
         {'FunT' = tag, <<l::size(4)-binary, data0::binary>>}
         | cs
       ]) do
    data = filter_funtab_1(data0, <<0::size(32)>>)
    funtab = <<l::binary, :erlang.iolist_to_binary(data)::binary>>
    [{tag, funtab} | filter_funtab(cs)]
  end

  defp filter_funtab([h | t]) do
    [h | filter_funtab(t)]
  end

  defp filter_funtab([]) do
    []
  end

  defp filter_funtab_1(
         <<important::size(20)-binary, _OldUniq::size(4)-binary, t::binary>>,
         zero
       ) do
    [important, zero | filter_funtab_1(t, zero)]
  end

  defp filter_funtab_1(tail, _) when is_binary(tail) do
    [tail]
  end

  defp read_all_chunks(file0)
       when is_atom(file0) or is_list(file0) or
              is_binary(file0) do
    try do
      file = beam_filename(file0)
      {:ok, module, chunkIds0} = scan_beam(file, :info)

      chunkIds =
        for {name, _, _} <- chunkIds0 do
          name
        end

      {:ok, ^module, chunks} = scan_beam(file, chunkIds)
      {:ok, module, :lists.reverse(chunks)}
    catch
      error ->
        error
    end
  end

  defp read_chunk_data(file0, chunkNames) do
    try do
      read_chunk_data(file0, chunkNames, [])
    catch
      error ->
        error
    end
  end

  defp read_chunk_data(file0, chunkNames0, options)
       when is_atom(file0) or is_list(file0) or
              is_binary(file0) do
    file = beam_filename(file0)
    {chunkIds, names, optional} = check_chunks(chunkNames0, file, [], [], [])

    allowMissingChunks =
      member(
        :allow_missing_chunks,
        options
      )

    {:ok, module, chunks} = scan_beam(file, chunkIds, allowMissingChunks, optional)
    aT = :ets.new(:beam_symbols, [])
    t = {:empty, aT}

    try do
      chunks_to_data(names, chunks, file, chunks, module, t, [])
    after
      :ets.delete(aT)
    end
  end

  defp check_chunks([:atoms | ids], file, iL, l, o) do
    check_chunks(ids, file, ['Atom', 'AtU8' | iL], [{:atom_chunk, :atoms} | l], [
      'Atom',
      'AtU8' | o
    ])
  end

  defp check_chunks([:abstract_code | ids], file, iL, l, o) do
    check_chunks(ids, file, ['Abst', 'Dbgi' | iL], [{:abst_chunk, :abstract_code} | l], [
      'Abst',
      'Dbgi' | o
    ])
  end

  defp check_chunks([chunkName | ids], file, iL, l, o)
       when is_atom(chunkName) do
    chunkId = chunk_name_to_id(chunkName, file)
    check_chunks(ids, file, [chunkId | iL], [{chunkId, chunkName} | l], o)
  end

  defp check_chunks([chunkId | ids], file, iL, l, o) do
    check_chunks(ids, file, [chunkId | iL], [{chunkId, chunkId} | l], o)
  end

  defp check_chunks([], _File, iL, l, o) do
    {:lists.usort(iL), reverse(l), o}
  end

  defp scan_beam(file, what) do
    scan_beam(file, what, false, [])
  end

  defp scan_beam(file, what0, allowMissingChunks, optionalChunks) do
    case scan_beam1(file, what0) do
      {:missing, _FD, mod, data, what} when allowMissingChunks ->
        {:ok, mod,
         for id <- what do
           {id, :missing_chunk}
         end ++ data}

      {:missing, fD, mod, data, what} ->
        case what -- optionalChunks do
          [] ->
            {:ok, mod, data}

          [missing | _] ->
            :erlang.error({:missing_chunk, filename(fD), missing})
        end

      r ->
        r
    end
  end

  defp scan_beam1(file, what) do
    fD = open_file(file)

    case (try do
            scan_beam2(fD, what)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      error when :error === :erlang.element(1, error) ->
        throw(error)

      r ->
        r
    end
  end

  defp scan_beam2(fD, what) do
    case pread(fD, 0, 12) do
      {nFD, {:ok, <<"FOR1", _Size::size(32), "BEAM">>}} ->
        start = 12
        scan_beam(nFD, start, what, 17, [])

      _Error ->
        :erlang.error({:not_a_beam_file, filename(fD)})
    end
  end

  defp scan_beam(_FD, _Pos, [], mod, data) when mod !== 17 do
    {:ok, mod, data}
  end

  defp scan_beam(fD, pos, what, mod, data) do
    case pread(fD, pos, 8) do
      {_NFD, :eof} when mod === 17 ->
        :erlang.error({:missing_chunk, filename(fD), 'Atom'})

      {_NFD, :eof} when what === :info ->
        {:ok, mod, reverse(data)}

      {nFD, :eof} ->
        {:missing, nFD, mod, data, what}

      {nFD, {:ok, <<idL::size(4)-binary, sz::size(32)>>}} ->
        id = :erlang.binary_to_list(idL)
        pos1 = pos + 8
        pos2 = 4 * trunc((sz + 3) / 4) + pos1
        get_data(what, id, nFD, sz, pos1, pos2, mod, data)

      {_NFD, {:ok, _ChunkHead}} ->
        :erlang.error({:invalid_beam_file, filename(fD), pos})
    end
  end

  defp get_atom_data(cs, id, fD, size, pos, pos2, data, encoding) do
    newCs = del_chunk(id, cs)
    {nFD, chunk} = get_chunk(id, pos, size, fD)
    <<_Num::size(32), chunk2::binary>> = chunk
    {module, _} = extract_atom(chunk2, encoding)

    c =
      case cs do
        :info ->
          {id, pos, size}

        _ ->
          {id, chunk}
      end

    scan_beam(nFD, pos2, newCs, module, [c | data])
  end

  defp get_data(cs, 'Atom' = id, fD, size, pos, pos2, _Mod, data) do
    get_atom_data(cs, id, fD, size, pos, pos2, data, :latin1)
  end

  defp get_data(cs, 'AtU8' = id, fD, size, pos, pos2, _Mod, data) do
    get_atom_data(cs, id, fD, size, pos, pos2, data, :utf8)
  end

  defp get_data(:info, id, fD, size, pos, pos2, mod, data) do
    scan_beam(fD, pos2, :info, mod, [{id, pos, size} | data])
  end

  defp get_data(chunks, id, fD, size, pos, pos2, mod, data) do
    {nFD, newData} =
      case member(id, chunks) do
        true ->
          {fD1, chunk} = get_chunk(id, pos, size, fD)
          {fD1, [{id, chunk} | data]}

        false ->
          {fD, data}
      end

    newChunks = del_chunk(id, chunks)
    scan_beam(nFD, pos2, newChunks, mod, newData)
  end

  defp del_chunk(_Id, :info) do
    :info
  end

  defp del_chunk(id, chunks) do
    delete(id, chunks)
  end

  defp get_chunk(id, pos, size, fD) do
    case pread(fD, pos, size) do
      {nFD, :eof} when size === 0 ->
        {nFD, <<>>}

      {_NFD, :eof} when size > 0 ->
        :erlang.error({:chunk_too_big, filename(fD), id, size, 0})

      {_NFD, {:ok, chunk}} when size > byte_size(chunk) ->
        :erlang.error({:chunk_too_big, filename(fD), id, size, byte_size(chunk)})

      {nFD, {:ok, chunk}} ->
        {nFD, chunk}
    end
  end

  defp chunks_to_data([{:atom_chunk, name} | cNs], chunks, file, cs, module, atoms, l) do
    {newAtoms, ret} = chunk_to_data(name, "", file, cs, atoms, module)
    chunks_to_data(cNs, chunks, file, cs, module, newAtoms, [ret | l])
  end

  defp chunks_to_data([{:abst_chunk, name} | cNs], chunks, file, cs, module, atoms, l) do
    dbgiChunk = :proplists.get_value('Dbgi', chunks, "")

    {newAtoms, ret} =
      case (try do
              chunk_to_data(:debug_info, dbgiChunk, file, cs, atoms, module)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {dbgiAtoms, {:debug_info, {:debug_info_v1, backend, metadata}}} ->
          case backend.debug_info(:erlang_v1, module, metadata, []) do
            {:ok, code} ->
              {dbgiAtoms, {:abstract_code, {:raw_abstract_v1, code}}}

            {:error, _} ->
              {dbgiAtoms, {:abstract_code, :no_abstract_code}}
          end

        {:error, :beam_lib, {:key_missing_or_invalid, path, :debug_info}} ->
          :erlang.error({:key_missing_or_invalid, path, :abstract_code})

        _ ->
          abstChunk = :proplists.get_value('Abst', chunks, "")
          chunk_to_data(name, abstChunk, file, cs, atoms, module)
      end

    chunks_to_data(cNs, chunks, file, cs, module, newAtoms, [ret | l])
  end

  defp chunks_to_data([{id, name} | cNs], chunks, file, cs, module, atoms, l) do
    {_Id, chunk} = :lists.keyfind(id, 1, chunks)
    {newAtoms, ret} = chunk_to_data(name, chunk, file, cs, atoms, module)
    chunks_to_data(cNs, chunks, file, cs, module, newAtoms, [ret | l])
  end

  defp chunks_to_data([], _Chunks, _File, _Cs, module, _Atoms, l) do
    {:ok, {module, reverse(l)}}
  end

  defp chunk_to_data(id, :missing_chunk, _File, _Cs, atomTable, _Mod) do
    {atomTable, {id, :missing_chunk}}
  end

  defp chunk_to_data(:attributes = id, chunk, file, _Cs, atomTable, _Mod) do
    try do
      term = :erlang.binary_to_term(chunk)
      {atomTable, {id, attributes(term)}}
    catch
      :error, :badarg ->
        :erlang.error({:invalid_chunk, file, chunk_name_to_id(id, file)})
    end
  end

  defp chunk_to_data(:compile_info = id, chunk, file, _Cs, atomTable, _Mod) do
    try do
      {atomTable, {id, :erlang.binary_to_term(chunk)}}
    catch
      :error, :badarg ->
        :erlang.error({:invalid_chunk, file, chunk_name_to_id(id, file)})
    end
  end

  defp chunk_to_data(:debug_info = id, chunk, file, _Cs, atomTable, mod) do
    case chunk do
      <<>> ->
        {atomTable, {id, :no_debug_info}}

      <<0::size(8), n::size(8), mode0::size(n)-binary, rest::binary>> ->
        mode = :erlang.binary_to_atom(mode0, :utf8)
        term = decrypt_chunk(mode, mod, file, id, rest)
        {atomTable, {id, anno_from_term(term)}}

      _ ->
        case (try do
                :erlang.binary_to_term(chunk)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            :erlang.error({:invalid_chunk, file, chunk_name_to_id(id, file)})

          term ->
            {atomTable, {id, anno_from_term(term)}}
        end
    end
  end

  defp chunk_to_data(:abstract_code = id, chunk, file, _Cs, atomTable, mod) do
    case chunk do
      <<>> ->
        {atomTable, {id, :no_abstract_code}}

      <<0::size(8), n::size(8), mode0::size(n)-binary, rest::binary>> ->
        mode = :erlang.binary_to_atom(mode0, :utf8)
        term = decrypt_chunk(mode, mod, file, id, rest)
        {atomTable, {id, old_anno_from_term(term)}}

      _ ->
        case (try do
                :erlang.binary_to_term(chunk)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            :erlang.error({:invalid_chunk, file, chunk_name_to_id(id, file)})

          term ->
            try do
              {atomTable, {id, old_anno_from_term(term)}}
            catch
              _, _ ->
                :erlang.error({:invalid_chunk, file, chunk_name_to_id(id, file)})
            end
        end
    end
  end

  defp chunk_to_data(:atoms = id, _Chunk, _File, cs, atomTable0, _Mod) do
    atomTable = ensure_atoms(atomTable0, cs)
    atoms = :ets.tab2list(atomTable)
    {atomTable, {id, :lists.sort(atoms)}}
  end

  defp chunk_to_data(chunkName, chunk, file, cs, atomTable, _Mod)
       when is_atom(chunkName) do
    case (try do
            symbols(chunk, atomTable, cs, chunkName)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, newAtomTable, s} ->
        {newAtomTable, {chunkName, s}}

      {:EXIT, _} ->
        :erlang.error({:invalid_chunk, file, chunk_name_to_id(chunkName, file)})
    end
  end

  defp chunk_to_data(chunkId, chunk, _File, _Cs, atomTable, _Module)
       when is_list(chunkId) do
    {atomTable, {chunkId, chunk}}
  end

  defp chunk_name_to_id(:indexed_imports, _) do
    'ImpT'
  end

  defp chunk_name_to_id(:imports, _) do
    'ImpT'
  end

  defp chunk_name_to_id(:exports, _) do
    'ExpT'
  end

  defp chunk_name_to_id(:labeled_exports, _) do
    'ExpT'
  end

  defp chunk_name_to_id(:locals, _) do
    'LocT'
  end

  defp chunk_name_to_id(:labeled_locals, _) do
    'LocT'
  end

  defp chunk_name_to_id(:attributes, _) do
    'Attr'
  end

  defp chunk_name_to_id(:abstract_code, _) do
    'Abst'
  end

  defp chunk_name_to_id(:debug_info, _) do
    'Dbgi'
  end

  defp chunk_name_to_id(:compile_info, _) do
    'CInf'
  end

  defp chunk_name_to_id(other, file) do
    :erlang.error({:unknown_chunk, file, other})
  end

  defp attributes(attrs) do
    attributes(keysort(1, attrs), [])
  end

  defp attributes([], r) do
    reverse(r)
  end

  defp attributes(l, r) do
    k = :erlang.element(1, hd(l))

    {l1, l2} =
      splitwith(
        fn t ->
          :erlang.element(1, t) === k
        end,
        l
      )

    v =
      append(
        for {_, a} <- l1 do
          a
        end
      )

    attributes(l2, [{k, v} | r])
  end

  defp symbols(<<_Num::size(32), b::binary>>, aT0, cs, name) do
    aT = ensure_atoms(aT0, cs)
    symbols1(b, aT, name, [], 1)
  end

  defp symbols1(<<i1::size(32), i2::size(32), i3::size(32), b::binary>>, aT, name, s, cnt) do
    symbol = symbol(name, aT, i1, i2, i3, cnt)
    symbols1(b, aT, name, [symbol | s], cnt + 1)
  end

  defp symbols1(<<>>, aT, _Name, s, _Cnt) do
    {:ok, aT, sort(s)}
  end

  defp symbol(:indexed_imports, aT, i1, i2, i3, cnt) do
    {cnt, atm(aT, i1), atm(aT, i2), i3}
  end

  defp symbol(:imports, aT, i1, i2, i3, _Cnt) do
    {atm(aT, i1), atm(aT, i2), i3}
  end

  defp symbol(:labeled_exports, aT, i1, i2, i3, _Cnt) do
    {atm(aT, i1), i2, i3}
  end

  defp symbol(:labeled_locals, aT, i1, i2, i3, _Cnt) do
    {atm(aT, i1), i2, i3}
  end

  defp symbol(_, aT, i1, i2, _I3, _Cnt) do
    {atm(aT, i1), i2}
  end

  defp atm(aT, n) do
    [{_N, s}] = :ets.lookup(aT, n)
    s
  end

  defp ensure_atoms({:empty, aT}, cs) do
    case :lists.keyfind('AtU8', 1, cs) do
      {_Id, atomChunk} when is_binary(atomChunk) ->
        extract_atoms(atomChunk, aT, :utf8)

      _ ->
        {_Id, atomChunk} = :lists.keyfind('Atom', 1, cs)
        extract_atoms(atomChunk, aT, :latin1)
    end

    aT
  end

  defp ensure_atoms(aT, _Cs) do
    aT
  end

  defp extract_atoms(<<_Num::size(32), b::binary>>, aT, encoding) do
    extract_atoms(b, 1, aT, encoding)
  end

  defp extract_atoms(<<>>, _I, _AT, _Encoding) do
    true
  end

  defp extract_atoms(b, i, aT, encoding) do
    {atom, b1} = extract_atom(b, encoding)
    true = :ets.insert(aT, {i, atom})
    extract_atoms(b1, i + 1, aT, encoding)
  end

  defp extract_atom(<<len, b::binary>>, encoding) do
    <<sB::size(len)-binary, tail::binary>> = b
    {:erlang.binary_to_atom(sB, encoding), tail}
  end

  require Record
  Record.defrecord(:r_bb, :bb, pos: 0, bin: :undefined, source: :undefined)

  defp open_file(binary0) when is_binary(binary0) do
    binary = maybe_uncompress(binary0)
    r_bb(bin: binary, source: binary)
  end

  defp open_file(fileName) do
    case :file.open(fileName, [:read, :raw, :binary]) do
      {:ok, fd} ->
        read_all(fd, fileName, [])

      error ->
        file_error(fileName, error)
    end
  end

  defp read_all(fd, fileName, bins) do
    case :file.read(fd, 1 <<< 18) do
      {:ok, bin} ->
        read_all(fd, fileName, [bin | bins])

      :eof ->
        :ok = :file.close(fd)

        r_bb(
          bin: maybe_uncompress(reverse(bins)),
          source: fileName
        )

      error ->
        :ok = :file.close(fd)
        file_error(fileName, error)
    end
  end

  defp pread(fD, atPos, size) do
    r_bb(pos: pos, bin: binary) = fD
    skip = atPos - pos

    case binary do
      <<_::size(skip)-binary, b::size(size)-binary, bin::binary>> ->
        nFD = r_bb(fD, pos: atPos + size, bin: bin)
        {nFD, {:ok, b}}

      <<_::size(skip)-binary, bin::binary>>
      when byte_size(bin) > 0 ->
        nFD = r_bb(fD, pos: atPos + byte_size(bin), bin: <<>>)
        {nFD, {:ok, bin}}

      _ ->
        {fD, :eof}
    end
  end

  defp filename(bB) when is_binary(r_bb(bB, :source)) do
    r_bb(bB, :source)
  end

  defp filename(bB) do
    :erlang.list_to_atom(r_bb(bB, :source))
  end

  defp beam_filename(bin) when is_binary(bin) do
    bin
  end

  defp beam_filename(file) do
    :filename.rootname(file, '.beam') ++ '.beam'
  end

  defp maybe_uncompress(<<"FOR1", _::binary>> = binary) do
    binary
  end

  defp maybe_uncompress([<<"FOR1", _::binary>> | _] = iOData) do
    :erlang.iolist_to_binary(iOData)
  end

  defp maybe_uncompress(iOData) do
    try do
      :zlib.gunzip(iOData)
    catch
      _, _ ->
        :erlang.iolist_to_binary(iOData)
    end
  end

  defp compress(iOData) do
    :zlib.gzip(iOData)
  end

  defp assert_directory(fileName) do
    case :filelib.is_dir(fileName) do
      true ->
        :ok

      false ->
        :erlang.error({:not_a_directory, fileName})
    end
  end

  defp file_error(fileName, {:error, reason}) do
    :erlang.error({:file_error, fileName, reason})
  end

  defp error(reason) do
    throw({:error, :beam_lib, reason})
  end

  def significant_chunks() do
    ['Line' | md5_chunks()]
  end

  defp md5_chunks() do
    ['Atom', 'AtU8', 'Code', 'StrT', 'ImpT', 'ExpT', 'FunT', 'LitT']
  end

  defp mandatory_chunks() do
    ['Code', 'ExpT', 'ImpT', 'StrT']
  end

  Record.defrecord(:r_state, :state, crypto_key_f: :undefined)

  defp decrypt_chunk(type, module, file, id, bin) do
    try do
      keyString = get_crypto_key({:debug_info, type, module, file})

      {^type, key, iVec, _BlockSize} =
        make_crypto_key(
          type,
          keyString
        )

      :ok = start_crypto()
      newBin = :crypto.crypto_one_time(:des_ede3_cbc, key, iVec, bin, false)
      :erlang.binary_to_term(newBin)
    catch
      _, _ ->
        :erlang.error({:key_missing_or_invalid, file, id})
    end
  end

  defp old_anno_from_term({:raw_abstract_v1, forms}) do
    {:raw_abstract_v1, anno_from_forms(forms)}
  end

  defp old_anno_from_term({tag, forms})
       when tag === :abstract_v1 or
              tag === :abstract_v2 do
    try do
      {tag, anno_from_forms(forms)}
    catch
      _, _ ->
        {tag, forms}
    end
  end

  defp old_anno_from_term(t) do
    t
  end

  defp anno_from_term({:debug_info_v1 = tag1, :erl_abstract_code = tag2, {forms, opts}}) do
    try do
      {tag1, tag2, {anno_from_forms(forms), opts}}
    catch
      _, _ ->
        {tag1, tag2, {forms, opts}}
    end
  end

  defp anno_from_term(t) do
    t
  end

  defp anno_from_forms(forms0) do
    forms = :epp.restore_typed_record_fields(forms0)

    for form <- forms do
      :erl_parse.anno_from_term(form)
    end
  end

  defp start_crypto() do
    case :crypto.start() do
      {:error, {:already_started, _}} ->
        :ok

      :ok ->
        :ok
    end
  end

  def get_crypto_key(what) do
    call_crypto_server({:get_crypto_key, what})
  end

  defp call_crypto_server(req) do
    try do
      :gen_server.call(:beam_lib__crypto_key_server, req, :infinity)
    catch
      :exit, {:noproc, _} ->
        call_crypto_server_1(req)

      :exit, {:normal, _} ->
        call_crypto_server_1(req)
    end
  end

  defp call_crypto_server_1(req) do
    case :gen_server.start({:local, :beam_lib__crypto_key_server}, :beam_lib, [], []) do
      {:ok, _} ->
        :ok

      {:error, {:already_started, _}} ->
        :ok
    end

    :erlang.yield()
    call_crypto_server(req)
  end

  def init([]) do
    {:ok, r_state()}
  end

  def handle_call({:get_crypto_key, _} = r, from, r_state(crypto_key_f: :undefined) = s) do
    case crypto_key_fun_from_file() do
      :error ->
        {:reply, :error, s}

      f when is_function(f) ->
        handle_call(r, from, r_state(s, crypto_key_f: f))
    end
  end

  def handle_call({:get_crypto_key, what}, from, r_state(crypto_key_f: f) = s) do
    try do
      result = f.(what)
      :gen_server.reply(from, result)
      :erlang.garbage_collect()
      {:noreply, s}
    catch
      _, _ ->
        {:reply, :error, s}
    end
  end

  def handle_call({:crypto_key_fun, f}, {_, _} = from, s) do
    case r_state(s, :crypto_key_f) do
      :undefined ->
        cond do
          is_function(f, 1) ->
            {result, fun, reply} =
              case (try do
                      f.(:init)
                    catch
                      :error, e -> {:EXIT, {e, __STACKTRACE__}}
                      :exit, e -> {:EXIT, e}
                      e -> e
                    end) do
                :ok ->
                  {true, f, :ok}

                {:ok, f1} when is_function(f1) ->
                  cond do
                    is_function(f1, 1) ->
                      {true, f1, :ok}

                    true ->
                      {false, :undefined, {:error, :badfun}}
                  end

                {:error, reason} ->
                  {false, :undefined, {:error, reason}}

                {:EXIT, reason} ->
                  {false, :undefined, {:error, reason}}
              end

            :gen_server.reply(from, reply)
            :erlang.garbage_collect()

            newS =
              case result do
                true ->
                  r_state(s, crypto_key_f: fun)

                false ->
                  s
              end

            {:noreply, newS}

          true ->
            {:reply, {:error, :badfun}, s}
        end

      otherF when is_function(otherF) ->
        {:reply, {:error, :exists}, s}
    end
  end

  def handle_call(:clear_crypto_key_fun, _From, s) do
    case r_state(s, :crypto_key_f) do
      :undefined ->
        {:stop, :normal, :undefined, s}

      f ->
        result =
          try do
            f.(:clear)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        {:stop, :normal, {:ok, result}, s}
    end
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  defp crypto_key_fun_from_file() do
    case :init.get_argument(:home) do
      {:ok, [[home]]} ->
        crypto_key_fun_from_file_1(['.', home])

      _ ->
        crypto_key_fun_from_file_1(['.'])
    end
  end

  defp crypto_key_fun_from_file_1(path) do
    case f_p_s(path, '.erlang.crypt') do
      {:ok, keyInfo, _} ->
        try_load_crypto_fun(keyInfo)

      _ ->
        :error
    end
  end

  defp f_p_s(p, f) do
    case :file.path_script(p, f) do
      {:error, :enoent} ->
        {:error, :enoent}

      {:error, {line, _Mod, _Term} = e} ->
        :erlang.error('file:path_script(~tp,~tp): error on line ~p: ~ts~n', [
          p,
          f,
          line,
          :file.format_error(e)
        ])

        :ok

      {:error, e} when is_atom(e) ->
        :erlang.error('file:path_script(~tp,~tp): ~ts~n', [p, f, :file.format_error(e)])
        :ok

      other ->
        other
    end
  end

  defp try_load_crypto_fun(keyInfo) when is_list(keyInfo) do
    t = :ets.new(:keys, [:private, :set])

    foreach(
      fn
        {:debug_info, mode, m, key} when is_atom(m) ->
          :ets.insert(t, {{:debug_info, mode, m, []}, key})

        {:debug_info, mode, [], key} ->
          :ets.insert(t, {{:debug_info, mode, [], []}, key})

        other ->
          :erlang.error('unknown key: ~p~n', [other])
      end,
      keyInfo
    )

    fn
      {:debug_info, mode, m, f} ->
        alt_lookup_key(
          [{:debug_info, mode, m, f}, {:debug_info, mode, m, []}, {:debug_info, mode, [], []}],
          t
        )

      :clear ->
        :ets.delete(t)

      _ ->
        :error
    end
  end

  defp try_load_crypto_fun(keyInfo) do
    :erlang.error('unrecognized crypto key info: ~p\n', [keyInfo])
  end

  defp alt_lookup_key([h | t], tab) do
    case :ets.lookup(tab, h) do
      [] ->
        alt_lookup_key(t, tab)

      [{_, val}] ->
        val
    end
  end

  defp alt_lookup_key([], _) do
    :error
  end

  defp error(fmt, args) do
    :error_logger.error_msg(fmt, args)
    :error
  end
end
