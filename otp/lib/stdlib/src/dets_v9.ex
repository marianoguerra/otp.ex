defmodule :m_dets_v9 do
  use Bitwise
  require Record

  Record.defrecord(:r_head, :head,
    m: :undefined,
    m2: :undefined,
    next: :undefined,
    fptr: :undefined,
    no_objects: :undefined,
    no_keys: :undefined,
    maxobjsize: :undefined,
    n: :undefined,
    type: :undefined,
    keypos: :undefined,
    freelists: :undefined,
    freelists_p: :undefined,
    no_collections: :undefined,
    auto_save: :undefined,
    update_mode: :undefined,
    fixed: false,
    hash_bif: :undefined,
    has_md5: :undefined,
    min_no_slots: :undefined,
    max_no_slots: :undefined,
    cache: :undefined,
    filename: :undefined,
    access: :read_write,
    ram_file: false,
    name: :undefined,
    parent: :undefined,
    server: :undefined,
    bump: :undefined,
    base: :undefined
  )

  Record.defrecord(:r_fileheader, :fileheader,
    freelist: :undefined,
    fl_base: :undefined,
    cookie: :undefined,
    closed_properly: :undefined,
    type: :undefined,
    version: :undefined,
    m: :undefined,
    next: :undefined,
    keypos: :undefined,
    no_objects: :undefined,
    no_keys: :undefined,
    min_no_slots: :undefined,
    max_no_slots: :undefined,
    no_colls: :undefined,
    hash_method: :undefined,
    read_md5: :undefined,
    has_md5: :undefined,
    md5: :undefined,
    trailer: :undefined,
    eof: :undefined,
    n: :undefined
  )

  Record.defrecord(:r_cache, :cache,
    cache: :undefined,
    csize: :undefined,
    inserts: :undefined,
    wrtime: :undefined,
    tsize: :undefined,
    delay: :undefined
  )

  Record.defrecord(:r__hash2, :"$hash2",
    file_format_version: :undefined,
    bchunk_format_version: :undefined,
    file: :undefined,
    type: :undefined,
    keypos: :undefined,
    hash_method: :undefined,
    n: :undefined,
    m: :undefined,
    next: :undefined,
    min: :undefined,
    max: :undefined,
    no_objects: :undefined,
    no_keys: :undefined,
    no_colls: :undefined
  )

  def mark_dirty(head) do
    dirty = [{8, <<0::size(32)>>}]
    {_H, :ok} = :dets_utils.pwrite(head, dirty)
    :ok = :dets_utils.sync(head)

    {:ok, _Pos} =
      :dets_utils.position(
        head,
        r_head(head, :freelists_p)
      )

    :dets_utils.truncate(head, :cur)
  end

  def prep_table_copy(fd, tab, fname, type, kp, ram, cacheSz, auto, parms) do
    case parms do
      r__hash2(
        file_format_version: 9,
        bchunk_format_version: 1,
        n: n,
        m: m,
        next: next,
        min: min,
        max: max,
        hash_method: hashMethodCode,
        no_objects: noObjects,
        no_keys: noKeys,
        no_colls: _NoColls
      )
      when is_integer(n) and is_integer(m) and
             is_integer(next) and is_integer(min) and
             is_integer(max) and is_integer(noObjects) and
             is_integer(noKeys) and noObjects >= noKeys ->
        hashMethod = code_to_hash_method(hashMethodCode)

        case hash_invars(n, m, next, min, max) do
          false ->
            throw(:badarg)

          true ->
            init_file(
              fd,
              tab,
              fname,
              type,
              kp,
              min,
              max,
              ram,
              cacheSz,
              auto,
              false,
              m,
              n,
              next,
              hashMethod,
              noObjects,
              noKeys
            )
        end

      _ ->
        throw(:badarg)
    end
  end

  def initiate_file(
        fd,
        tab,
        fname,
        type,
        kp,
        minSlots0,
        maxSlots0,
        ram,
        cacheSz,
        auto,
        doInitSegments
      ) do
    maxSlots1 = :erlang.min(maxSlots0, 256 * 512 * 256)
    minSlots1 = :erlang.min(minSlots0, maxSlots1)
    minSlots = slots2(minSlots1)
    maxSlots = slots2(maxSlots1)
    m = next = minSlots
    n = 0

    init_file(
      fd,
      tab,
      fname,
      type,
      kp,
      minSlots,
      maxSlots,
      ram,
      cacheSz,
      auto,
      doInitSegments,
      m,
      n,
      next,
      :phash2,
      0,
      0
    )
  end

  defp init_file(
         fd,
         tab,
         fname,
         type,
         kp,
         minSlots,
         maxSlots,
         ram,
         cacheSz,
         auto,
         doInitSegments,
         m,
         n,
         next,
         hashMethod,
         noObjects,
         noKeys
       ) do
    ftab = :dets_utils.init_alloc(56 + 28 * 4 + 16 + 4 + 124 + 4 * 256)

    head0 =
      r_head(
        m: m,
        m2: m * 2,
        next: next,
        fptr: fd,
        no_objects: noObjects,
        no_keys: noKeys,
        maxobjsize: 0,
        n: n,
        type: type,
        update_mode: :dirty,
        freelists: ftab,
        no_collections: :orddict.new(),
        auto_save: auto,
        hash_bif: hashMethod,
        has_md5: true,
        keypos: kp,
        min_no_slots: minSlots,
        max_no_slots: maxSlots,
        ram_file: ram,
        filename: fname,
        name: tab,
        cache: :dets_utils.new_cache(cacheSz),
        bump: 16,
        base: 56 + 28 * 4 + 16 + 4 + 124 + 4 * 256
      )

    freeListsPointer = 0
    noColls = <<0::size(28 * 4)-unit(8)>>
    fileHeader = file_header(head0, freeListsPointer, 0, noColls)
    w0 = {0, [fileHeader | <<0::size(4 * 256)-unit(8)>>]}

    :lists.foreach(
      fn
        {i1, i2}
        when is_integer(i1) and
               is_integer(i2) ->
          :ok

        {k, v} ->
          :erlang.put(k, v)
      end,
      :erlang.erase()
    )

    zero = seg_zero()
    {head1, ws1} = init_parts(head0, 0, no_parts(next), zero, [])
    noSegs = no_segs(next)
    {head2, wsI, wsP} = init_segments(head1, 0, noSegs, zero, [], [])

    ws2 =
      cond do
        doInitSegments ->
          wsP ++ wsI

        true ->
          wsP
      end

    :dets_utils.pwrite(fd, fname, [w0 | :lists.append(ws1) ++ ws2])
    true = hash_invars(head2)
    {_, where, _} = :dets_utils.alloc(head2, 16)
    newFtab = :dets_utils.init_alloc(where)
    head = r_head(head2, freelists: newFtab, base: where)
    {:ok, head}
  end

  defp slots2(noSlots) when noSlots >= 256 do
    1 <<< :dets_utils.log2(noSlots)
  end

  defp init_parts(head, partNo, noParts, zero, ws)
       when partNo < noParts do
    partPos = 56 + 28 * 4 + 16 + 4 + 124 + 4 * partNo
    {newHead, w, _Part} = alloc_part(head, zero, partPos)
    init_parts(newHead, partNo + 1, noParts, zero, [w | ws])
  end

  defp init_parts(head, _PartNo, _NoParts, _Zero, ws) do
    {head, ws}
  end

  defp init_segments(head, segNo, noSegs, segZero, wsP, wsI)
       when segNo < noSegs do
    {newHead, wI, ws} = allocate_segment(head, segZero, segNo)
    init_segments(newHead, segNo + 1, noSegs, segZero, ws ++ wsP, [wI | wsI])
  end

  defp init_segments(head, _SegNo, _NoSegs, _SegZero, wsP, wsI) do
    {head, wsI, wsP}
  end

  defp allocate_segment(head, segZero, segNo) do
    partPos = 56 + 28 * 4 + 16 + 4 + 124 + 4 * div(segNo, 512)

    case get_arrpart(partPos) do
      :undefined ->
        {head1, [initArrPart, arrPartPointer], part} = alloc_part(head, segZero, partPos)
        {newHead, initSegment, [segPointer]} = alloc_seg(head1, segZero, segNo, part)
        {newHead, initSegment, [initArrPart, segPointer, arrPartPointer]}

      part ->
        alloc_seg(head, segZero, segNo, part)
    end
  end

  defp alloc_part(head, partZero, partPos) do
    {newHead, part, _} =
      :dets_utils.alloc(
        head,
        adjsz(4 * 512)
      )

    arrpart_cache(partPos, part)
    initArrPart = {part, partZero}
    arrPartPointer = {partPos, <<part::size(32)>>}
    {newHead, [initArrPart, arrPartPointer], part}
  end

  defp alloc_seg(head, segZero, segNo, part) do
    {newHead, segment, _} =
      :dets_utils.alloc(
        head,
        adjsz(4 * 512)
      )

    initSegment = {segment, segZero}
    pos = part + 4 * (segNo &&& 512 - 1)
    segp_cache(pos, segment)
    :dets_utils.disk_map_segment(segment, segZero)
    segPointer = {pos, <<segment::size(32)>>}
    {newHead, initSegment, [segPointer]}
  end

  def init_freelist(head) do
    pos = r_head(head, :freelists_p)
    free_lists_from_file(head, pos)
  end

  def read_file_header(fd, fileName) do
    {:ok, bin} = :dets_utils.pread_close(fd, fileName, 0, 56 + 28 * 4 + 16 + 4)

    <<freeList::size(32), cookie::size(32), cP::size(32), type2::size(32), version::size(32),
      m::size(32), next::size(32), kp::size(32), noObjects::size(32), noKeys::size(32),
      minNoSlots::size(32), maxNoSlots::size(32), hashMethod::size(32), n::size(32),
      noCollsB::size(28 * 4)-binary, mD5::size(16)-binary, flBase::size(32)>> = bin

    <<_::size(12)-binary, mD5DigestedPart::size(56 + 28 * 4 + 16 + 4 - 16 - 4 - 12)-binary,
      _::binary>> = bin

    {:ok, eOF} = :dets_utils.position_close(fd, fileName, :eof)
    {:ok, <<fileSize::size(32)>>} = :dets_utils.pread_close(fd, fileName, eOF - 4, 4)

    {cL, <<>>} =
      :lists.foldl(
        fn lSz, {acc, <<nN::size(32), r::binary>>} ->
          cond do
            nN === 0 ->
              {acc, r}

            true ->
              {[{lSz, nN} | acc], r}
          end
        end,
        {[], noCollsB},
        :lists.seq(4, 32 - 1)
      )

    noColls =
      cond do
        cL === [] and noObjects > 0 ->
          :undefined

        true ->
          :lists.reverse(cL)
      end

    base =
      case flBase do
        0 ->
          56 + 28 * 4 + 16 + 4 + 124 + 4 * 256

        _ ->
          flBase
      end

    fH =
      r_fileheader(
        freelist: freeList,
        fl_base: base,
        cookie: cookie,
        closed_properly: cP,
        type: :dets_utils.code_to_type(type2),
        version: version,
        m: m,
        next: next,
        keypos: kp,
        no_objects: noObjects,
        no_keys: noKeys,
        min_no_slots: minNoSlots,
        max_no_slots: maxNoSlots,
        no_colls: noColls,
        hash_method: hashMethod,
        read_md5: mD5,
        has_md5: <<0::size(16)-unit(8)>> !== mD5,
        md5: :erlang.md5(mD5DigestedPart),
        trailer: fileSize + flBase,
        eof: eOF,
        n: n
      )

    {:ok, fd, fH}
  end

  def check_file_header(fH, fd) do
    hashBif = code_to_hash_method(r_fileheader(fH, :hash_method))

    test =
      cond do
        r_fileheader(fH, :cookie) !== 11_259_375 ->
          {:error, :not_a_dets_file}

        r_fileheader(fH, :type) === :badtype ->
          {:error, :invalid_type_code}

        r_fileheader(fH, :version) !== 9 ->
          {:error, :bad_version}

        r_fileheader(fH, :has_md5) and
            r_fileheader(fH, :read_md5) !== r_fileheader(fH, :md5) ->
          {:error, :not_a_dets_file}

        r_fileheader(fH, :trailer) !== r_fileheader(fH, :eof) ->
          {:error, :not_closed}

        hashBif === :undefined ->
          {:error, :bad_hash_bif}

        r_fileheader(fH, :closed_properly) === 1 ->
          :ok

        r_fileheader(fH, :closed_properly) === 0 ->
          {:error, :not_closed}

        true ->
          {:error, :not_a_dets_file}
      end

    case test do
      :ok ->
        maxObjSize = max_objsize(r_fileheader(fH, :no_colls))

        h =
          r_head(
            m: r_fileheader(fH, :m),
            m2: r_fileheader(fH, :m) * 2,
            next: r_fileheader(fH, :next),
            fptr: fd,
            no_objects: r_fileheader(fH, :no_objects),
            no_keys: r_fileheader(fH, :no_keys),
            maxobjsize: maxObjSize,
            n: r_fileheader(fH, :n),
            type: r_fileheader(fH, :type),
            update_mode: :saved,
            auto_save: :infinity,
            fixed: false,
            freelists_p: r_fileheader(fH, :freelist),
            hash_bif: hashBif,
            has_md5: r_fileheader(fH, :has_md5),
            keypos: r_fileheader(fH, :keypos),
            min_no_slots: r_fileheader(fH, :min_no_slots),
            max_no_slots: r_fileheader(fH, :max_no_slots),
            no_collections: r_fileheader(fH, :no_colls),
            bump: 16,
            base: r_fileheader(fH, :fl_base)
          )

        {:ok, h}

      error ->
        error
    end
  end

  defp max_objsize(noColls = :undefined) do
    noColls
  end

  defp max_objsize(noColls) do
    max_objsize(noColls, 0)
  end

  defp max_objsize([], max) do
    max
  end

  defp max_objsize([{_, 0} | l], max) do
    max_objsize(l, max)
  end

  defp max_objsize([{i, _} | l], _Max) do
    max_objsize(l, i)
  end

  def cache_segps(fd, fileName, m) do
    noParts = no_parts(m)
    arrStart = 56 + 28 * 4 + 16 + 4 + 124 + 4 * 0
    {:ok, bin} = :dets_utils.pread_close(fd, fileName, arrStart, 4 * noParts)
    cache_arrparts(bin, 56 + 28 * 4 + 16 + 4 + 124, fd, fileName)
  end

  defp cache_arrparts(<<arrPartPos::size(32), b::binary>>, pos, fd, fileName) do
    arrpart_cache(pos, arrPartPos)
    {:ok, arrPartBin} = :dets_utils.pread_close(fd, fileName, arrPartPos, 512 * 4)
    cache_segps1(fd, arrPartBin, arrPartPos)
    cache_arrparts(b, pos + 4, fd, fileName)
  end

  defp cache_arrparts(<<>>, _Pos, _Fd, _FileName) do
    :ok
  end

  defp cache_segps1(_Fd, <<0::size(32), _::binary>>, _P) do
    :ok
  end

  defp cache_segps1(fd, <<s::size(32), b::binary>>, p) do
    :dets_utils.disk_map_segment_p(fd, s)
    segp_cache(p, s)
    cache_segps1(fd, b, p + 4)
  end

  defp cache_segps1(_Fd, <<>>, _P) do
    :ok
  end

  defp no_parts(noSlots) do
    div(noSlots - 1, 256 * 512) + 1
  end

  defp no_segs(noSlots) do
    div(noSlots - 1, 256) + 1
  end

  def bulk_input(head, initFun, _Cntrs) do
    bulk_input(head, initFun, make_ref(), 0)
  end

  defp bulk_input(head, initFun, ref, seq) do
    fn
      :close ->
        _ =
          try do
            initFun.(:close)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

      :read ->
        case (try do
                {ref, initFun.(:read)}
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {^ref, :end_of_input} ->
            :end_of_input

          {^ref, {l0, newInitFun}}
          when is_list(l0) and
                 is_function(newInitFun) ->
            kp = r_head(head, :keypos)

            case (try do
                    bulk_objects(l0, head, kp, seq, [])
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              {:EXIT, _Error} ->
                _ =
                  try do
                    newInitFun.(:close)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end

                {:error, :invalid_objects_list}

              {l, nSeq} ->
                {l, bulk_input(head, newInitFun, ref, nSeq)}
            end

          {^ref, value} ->
            {:error, {:init_fun, value}}

          error ->
            throw({:thrown, error})
        end
    end
  end

  defp bulk_objects([t | ts], head, kp, seq, l) do
    bT = :erlang.term_to_binary(t)
    key = :erlang.element(kp, t)
    bulk_objects(ts, head, kp, seq + 1, [make_object(head, key, seq, bT) | l])
  end

  defp bulk_objects([], _Head, kp, seq, l)
       when is_integer(kp) and
              is_integer(seq) do
    {l, seq}
  end

  def output_objs(head, slotNums, cntrs) do
    fn
      :close ->
        cache = {}
        acc = []
        true = :ets.insert(cntrs, {1, 0, [], 0})
        true = :ets.insert(cntrs, {:no, 0, 0})
        fun = output_objs2(:foo, acc, head, cache, cntrs, slotNums, :bar)
        fun.(:close)

      [] ->
        output_objs(head, slotNums, cntrs)

      l ->
        true = :ets.delete_all_objects(cntrs)
        true = :ets.insert(cntrs, {:no, 0, 0})
        es = bin2term(l, r_head(head, :keypos))
        cache = {}
        {nE, nAcc, nCache} = output_slots(es, head, cache, cntrs, 0, 0)
        output_objs2(nE, nAcc, head, nCache, cntrs, slotNums, 1)
    end
  end

  defp output_objs2(e, acc, head, cache, sizeT, slotNums, 0) do
    nCache = write_all_sizes(cache, sizeT, head, :more)

    max =
      :erlang.max(
        1,
        :erlang.min(tuple_size(nCache), 10)
      )

    output_objs2(e, acc, head, nCache, sizeT, slotNums, max)
  end

  defp output_objs2(e, acc, head, cache, sizeT, slotNums, chunkI) do
    fn
      :close ->
        {_, [], cache1} =
          cond do
            acc === [] ->
              {:foo, [], cache}

            true ->
              output_slot(acc, head, cache, [], sizeT, 0, 0)
          end

        _NCache = write_all_sizes(cache1, sizeT, head, :no_more)
        segSz = 512 * 4
        {_, segEnd, _} = :dets_utils.alloc(head, adjsz(segSz))
        [{:no, noObjects, noKeys}] = :ets.lookup(sizeT, :no)
        head1 = r_head(head, no_objects: noObjects, no_keys: noKeys)
        true = :ets.delete(sizeT, :no)
        {newHead, nL, _MaxSz, _End} = allocate_all_objects(head1, sizeT)
        segment_file(sizeT, newHead, nL, segEnd)
        {minSlots, estNoSlots, maxSlots} = slotNums

        cond do
          estNoSlots === :bulk_init ->
            {:ok, 0, newHead}

          true ->
            estNoSegs = no_segs(estNoSlots)
            minNoSegs = no_segs(minSlots)
            maxNoSegs = no_segs(maxSlots)
            noSegs = no_segs(noKeys)
            diff = abs(noSegs - estNoSegs)

            cond do
              diff > 5 and noSegs <= maxNoSegs and
                  noSegs >= minNoSegs ->
                {:try_again, noKeys}

              true ->
                {:ok, 0, newHead}
            end
        end

      l ->
        es = bin2term(l, r_head(head, :keypos))
        {nE, nAcc, nCache} = output_slots(e, es, acc, head, cache, sizeT, 0, 0)
        output_objs2(nE, nAcc, head, nCache, sizeT, slotNums, chunkI - 1)
    end
  end

  def compact_init(readHead, writeHead, tableParameters) do
    sizeT = :ets.new(:dets_compact, [])
    r_head(no_keys: noKeys, no_objects: noObjects) = readHead
    noObjsPerSize = r__hash2(tableParameters, :no_colls)

    {newWriteHead, bases, segAddr, segEnd} =
      prepare_file_init(noObjects, noKeys, noObjsPerSize, sizeT, writeHead)

    input = compact_input(readHead, newWriteHead, sizeT, tuple_size(bases))
    output = fast_output(newWriteHead, sizeT, bases, segAddr, segEnd)
    tmpDir = :filename.dirname(r_head(newWriteHead, :filename))

    reply =
      try do
        :file_sorter.sort(input, output, [{:format, :binary}, {:tmpdir, tmpDir}, {:header, 1}])
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    :ets.delete(sizeT)
    reply
  end

  defp compact_input(head, wHead, sizeT, noSizes) do
    l = :dets_utils.all_allocated_as_list(head)

    cache =
      :erlang.list_to_tuple(
        :erlang.tuple_to_list({}) ++
          :lists.duplicate(
            noSizes - tuple_size({}),
            [0]
          )
      )

    compact_input(head, wHead, sizeT, cache, l)
  end

  defp compact_input(head, wHead, sizeT, cache, l) do
    fn
      :close ->
        :ok

      :read ->
        compact_read(head, wHead, sizeT, cache, l, 0, [], 0)
    end
  end

  defp compact_read(_Head, wHead, sizeT, cache, [], _Min, [], _ASz) do
    _ = fast_write_all_sizes(cache, sizeT, wHead)
    :end_of_input
  end

  defp compact_read(head, wHead, sizeT, cache, l, min, segBs, aSz)
       when aSz + min >= 60 * 8192 and aSz > 0 do
    nCache = fast_write_all_sizes(cache, sizeT, wHead)
    {segBs, compact_input(head, wHead, sizeT, nCache, l)}
  end

  defp compact_read(head, wHead, sizeT, cache, [[from | to] | l], min, segBs, aSz) do
    max = :erlang.max(8192 * 3, min)

    case check_pread_arg(max, head) do
      true ->
        case :dets_utils.pread_n(r_head(head, :fptr), from, max) do
          :eof ->
            :not_ok

          bin1 when byte_size(bin1) < min ->
            pad = min - byte_size(bin1)
            newBin = <<bin1::binary, 0::size(pad)-unit(8)>>
            compact_objs(head, wHead, sizeT, newBin, l, from, to, segBs, cache, aSz)

          newBin ->
            compact_objs(head, wHead, sizeT, newBin, l, from, to, segBs, cache, aSz)
        end

      false ->
        :not_ok
    end
  end

  defp compact_objs(head, wHead, sizeT, bin, l, from, to, segBs, cache, aSz)
       when from === to do
    case l do
      [] ->
        {segBs, compact_input(head, wHead, sizeT, cache, l)}

      [[from1 | to1] | l1] ->
        skip1 = from1 - from

        case bin do
          <<_::size(skip1)-binary, newBin::binary>> ->
            compact_objs(head, wHead, sizeT, newBin, l1, from1, to1, segBs, cache, aSz)

          _ when byte_size(bin) < skip1 ->
            compact_read(head, wHead, sizeT, cache, l, 0, segBs, aSz)
        end
    end
  end

  defp compact_objs(
         head,
         wHead,
         sizeT,
         <<size::size(32), st::size(32), _Sz::size(32), kO::binary>> = bin,
         l,
         from,
         to,
         segBs,
         cache,
         aSz
       )
       when st === 305_419_896 do
    lSize = sz2pos(size)
    size2 = 1 <<< (lSize - 1)

    cond do
      byte_size(bin) >= size2 ->
        nASz = aSz + size2
        <<slotObjs::size(size2)-binary, newBin::binary>> = bin

        term =
          cond do
            r_head(head, :type) === :set ->
              :erlang.binary_to_term(kO)

            true ->
              <<_KSz::size(32), b2::binary>> = kO
              :erlang.binary_to_term(b2)
          end

        key = :erlang.element(r_head(head, :keypos), term)
        slot = db_hash(key, head)
        from1 = from + size2
        [addr | aL] = :erlang.element(lSize, cache)
        nCache = :erlang.setelement(lSize, cache, [addr + size2, slotObjs | aL])

        nSegBs = [
          <<slot::size(32), size::size(32), addr::size(32), lSize::size(8)>>
          | segBs
        ]

        compact_objs(head, wHead, sizeT, newBin, l, from1, to, nSegBs, nCache, nASz)

      true ->
        compact_read(head, wHead, sizeT, cache, [[from | to] | l], size2, segBs, aSz)
    end
  end

  defp compact_objs(
         head,
         wHead,
         sizeT,
         <<_::size(32), _St::size(32), _::size(32), _::binary>> = bin,
         l,
         from,
         to,
         segBs,
         cache,
         aSz
       )
       when byte_size(bin) >= 512 * 4 do
    <<_::size(512 * 4)-binary, newBin::binary>> = bin
    compact_objs(head, wHead, sizeT, newBin, l, from + 512 * 4, to, segBs, cache, aSz)
  end

  defp compact_objs(
         head,
         wHead,
         sizeT,
         <<_::size(32), _St::size(32), _::size(32), _::binary>> = bin,
         l,
         from,
         to,
         segBs,
         cache,
         aSz
       )
       when byte_size(bin) < 512 * 4 do
    compact_read(head, wHead, sizeT, cache, [[from | to] | l], 512 * 4, segBs, aSz)
  end

  defp compact_objs(head, wHead, sizeT, _Bin, l, from, to, segBs, cache, aSz) do
    compact_read(head, wHead, sizeT, cache, [[from | to] | l], 0, segBs, aSz)
  end

  def read_bchunks(head, l) do
    read_bchunks(head, l, 0, [], 0)
  end

  defp read_bchunks(_Head, l, min, bs, aSz)
       when aSz + min >= 4 * 8192 and bs !== [] do
    {:lists.reverse(bs), l}
  end

  defp read_bchunks(head, {from, to, l}, min, bs, aSz) do
    max = :erlang.max(8192 * 2, min)

    case check_pread_arg(max, head) do
      true ->
        case :dets_utils.pread_n(r_head(head, :fptr), from, max) do
          :eof ->
            {:error, :premature_eof}

          newBin when byte_size(newBin) >= min ->
            bchunks(head, l, newBin, bs, aSz, from, to)

          bin1 when to - from === min and l === <<>> ->
            pad = min - byte_size(bin1)
            newBin = <<bin1::binary, 0::size(pad)-unit(8)>>
            bchunks(head, l, newBin, bs, aSz, from, to)

          _ ->
            {:error, :premature_eof}
        end

      false ->
        {:error,
         :dets_utils.bad_object(
           :bad_object,
           {:read_bchunks, max}
         )}
    end
  end

  defp bchunks(head, l, bin, bs, aSz, from, to)
       when from === to do
    cond do
      l === <<>> ->
        {:finished, :lists.reverse(bs)}

      true ->
        <<from1::size(32), to1::size(32), l1::binary>> = l
        skip1 = from1 - from

        case bin do
          <<_::size(skip1)-binary, newBin::binary>> ->
            bchunks(head, l1, newBin, bs, aSz, from1, to1)

          _ when byte_size(bin) < skip1 ->
            read_bchunks(head, {from1, to1, l1}, 0, bs, aSz)
        end
    end
  end

  defp bchunks(
         head,
         l,
         <<size::size(32), st::size(32), _Sz::size(32), kO::binary>> = bin,
         bs,
         aSz,
         from,
         to
       )
       when st === 305_419_896 or st === 61_591_023 do
    lSize = sz2pos(size)
    size2 = 1 <<< (lSize - 1)

    cond do
      byte_size(bin) >= size2 ->
        <<b0::size(size2)-binary, newBin::binary>> = bin

        term =
          cond do
            r_head(head, :type) === :set ->
              :erlang.binary_to_term(kO)

            true ->
              <<_KSz::size(32), b2::binary>> = kO
              :erlang.binary_to_term(b2)
          end

        key = :erlang.element(r_head(head, :keypos), term)
        slot = db_hash(key, head)
        b = {lSize, slot, b0}
        bchunks(head, l, newBin, [b | bs], aSz + size2, from + size2, to)

      true ->
        read_bchunks(head, {from, to, l}, size2, bs, aSz)
    end
  end

  defp bchunks(
         head,
         l,
         <<_::size(32), _St::size(32), _::size(32), _::binary>> = bin,
         bs,
         aSz,
         from,
         to
       )
       when byte_size(bin) >= 512 * 4 do
    <<_::size(512 * 4)-binary, newBin::binary>> = bin
    bchunks(head, l, newBin, bs, aSz, from + 512 * 4, to)
  end

  defp bchunks(
         head,
         l,
         <<_::size(32), _St::size(32), _::size(32), _::binary>> = bin,
         bs,
         aSz,
         from,
         to
       )
       when byte_size(bin) < 512 * 4 do
    read_bchunks(head, {from, to, l}, 512 * 4, bs, aSz)
  end

  defp bchunks(head, l, _Bin, bs, aSz, from, to) do
    read_bchunks(head, {from, to, l}, 0, bs, aSz)
  end

  def bchunk_init(head, initFun) do
    ref = make_ref()

    case (try do
            {ref, initFun.(:read)}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {^ref, :end_of_input} ->
        {:error, {:init_fun, :end_of_input}}

      {^ref, {[], nInitFun}} when is_function(nInitFun) ->
        bchunk_init(head, nInitFun)

      {^ref, {[parmsBin | l], nInitFun}}
      when is_list(l) and
             is_function(nInitFun) ->
        r_head(
          fptr: fd,
          type: type,
          keypos: kp,
          auto_save: auto,
          cache: cache,
          filename: fname,
          ram_file: ram,
          name: tab
        ) = head

        case try_bchunk_header(parmsBin, head) do
          {:ok, parms} ->
            r__hash2(no_objects: noObjects, no_keys: noKeys, no_colls: noObjsPerSize) = parms
            cacheSz = :dets_utils.cache_size(cache)
            {:ok, head1} = prep_table_copy(fd, tab, fname, type, kp, ram, cacheSz, auto, parms)
            sizeT = :ets.new(:dets_init, [])

            {newHead, bases, segAddr, segEnd} =
              prepare_file_init(noObjects, noKeys, noObjsPerSize, sizeT, head1)

            eCache =
              :erlang.list_to_tuple(
                :erlang.tuple_to_list({}) ++
                  :lists.duplicate(
                    tuple_size(bases) - tuple_size({}),
                    [0]
                  )
              )

            input = fn
              :close ->
                _ =
                  try do
                    nInitFun.(:close)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end

              :read ->
                do_make_slots(l, eCache, sizeT, newHead, ref, 0, nInitFun)
            end

            output = fast_output(newHead, sizeT, bases, segAddr, segEnd)
            tmpDir = :filename.dirname(r_head(head, :filename))

            reply =
              try do
                :file_sorter.sort(input, output, [
                  {:format, :binary},
                  {:tmpdir, tmpDir},
                  {:header, 1}
                ])
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end

            :ets.delete(sizeT)
            reply

          :not_ok ->
            {:error, {:init_fun, parmsBin}}
        end

      {^ref, value} ->
        {:error, {:init_fun, value}}

      error ->
        {:thrown, error}
    end
  end

  def try_bchunk_header(parmsBin, head) do
    r_head(type: type, keypos: kp, hash_bif: hashBif) = head
    hashMethod = hash_method_to_code(hashBif)

    case (try do
            :erlang.binary_to_term(parmsBin)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      parms
      when elem(parms, 0) === :"$hash2" and
             r__hash2(parms, :type) === type and
             r__hash2(parms, :keypos) === kp and
             r__hash2(parms, :hash_method) === hashMethod and
             r__hash2(parms, :bchunk_format_version) === 1 ->
        {:ok, parms}

      _ ->
        :not_ok
    end
  end

  defp bchunk_input(initFun, sizeT, head, ref, cache, aSz) do
    fn
      :close ->
        _ =
          try do
            initFun.(:close)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

      :read ->
        case (try do
                {ref, initFun.(:read)}
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {^ref, :end_of_input} ->
            _ = fast_write_all_sizes(cache, sizeT, head)
            :end_of_input

          {^ref, {l, nInitFun}}
          when is_list(l) and
                 is_function(nInitFun) ->
            do_make_slots(l, cache, sizeT, head, ref, aSz, nInitFun)

          {^ref, value} ->
            {:error, {:init_fun, value}}

          error ->
            throw({:thrown, error})
        end
    end
  end

  defp do_make_slots(l, cache, sizeT, head, ref, aSz, initFun) do
    case (try do
            make_slots(l, cache, [], aSz)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        _ =
          try do
            initFun.(:close)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        {:error, :invalid_objects_list}

      {cache1, segBs, nASz} when nASz > 60 * 8192 ->
        nCache = fast_write_all_sizes(cache1, sizeT, head)
        f = bchunk_input(initFun, sizeT, head, ref, nCache, 0)
        {segBs, f}

      {nCache, segBs, nASz} ->
        f = bchunk_input(initFun, sizeT, head, ref, nCache, nASz)
        {segBs, f}
    end
  end

  defp make_slots(
         [
           {lSize, slot, <<size::size(32), st::size(32), sz::size(32), kO::binary>> = bin0}
           | bins
         ],
         cache,
         segBs,
         aSz
       ) do
    bin =
      cond do
        st === 305_419_896 ->
          bin0

        st === 61_591_023 ->
          <<size::size(32), 305_419_896::size(32), sz::size(32), kO::binary>>
      end

    bSz = byte_size(bin0)
    true = bSz === 1 <<< (lSize - 1)
    nASz = aSz + bSz
    [addr | l] = :erlang.element(lSize, cache)

    nSegBs = [
      <<slot::size(32), size::size(32), addr::size(32), lSize::size(8)>>
      | segBs
    ]

    nCache = :erlang.setelement(lSize, cache, [addr + bSz, bin | l])
    make_slots(bins, nCache, nSegBs, nASz)
  end

  defp make_slots([], cache, segBs, aSz) do
    {cache, segBs, aSz}
  end

  defp fast_output(head, sizeT, bases, segAddr, segEnd) do
    fn
      :close ->
        fast_output_end(head, sizeT)

      l ->
        case :file.position(r_head(head, :fptr), segAddr) do
          {:ok, ^segAddr} ->
            newSegAddr = write_segment_file(l, bases, head, [], segAddr, segAddr)
            fast_output2(head, sizeT, bases, newSegAddr, segAddr, segEnd)

          error ->
            try do
              :dets_utils.file_error(error, r_head(head, :filename))
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end
        end
    end
  end

  defp fast_output2(head, sizeT, bases, segAddr, sS, segEnd) do
    fn
      :close ->
        finalZ = segEnd - segAddr
        :dets_utils.write(head, :dets_utils.make_zeros(finalZ))
        fast_output_end(head, sizeT)

      l ->
        newSegAddr = write_segment_file(l, bases, head, [], segAddr, sS)
        fast_output2(head, sizeT, bases, newSegAddr, sS, segEnd)
    end
  end

  defp fast_output_end(head, sizeT) do
    case :ets.foldl(
           fn {_Sz, _Pos, cnt, noC}, acc ->
             :erlang.and(cnt === noC, acc)
           end,
           true,
           sizeT
         ) do
      true ->
        {:ok, head}

      false ->
        {:error, :invalid_objects_list}
    end
  end

  defp write_segment_file(
         [
           <<slot::size(32), bSize::size(32), addrToBe::size(32), lSize::size(8)>>
           | bins
         ],
         bases,
         head,
         ws,
         segAddr,
         sS
       ) do
    pos = sS + 2 * 4 * slot
    write_segment_file(bins, bases, head, ws, segAddr, sS, pos, bSize, addrToBe, lSize)
  end

  defp write_segment_file([], _Bases, head, ws, segAddr, _SS) do
    :dets_utils.write(head, ws)
    segAddr
  end

  defp write_segment_file(bins, bases, head, ws, segAddr, sS, pos, bSize, addrToBe, lSize)
       when pos === segAddr do
    addr = addrToBe + :erlang.element(lSize, bases)
    nWs = [ws | <<bSize::size(32), addr::size(32)>>]
    write_segment_file(bins, bases, head, nWs, segAddr + 2 * 4, sS)
  end

  defp write_segment_file(bins, bases, head, ws, segAddr, sS, pos, bSize, addrToBe, lSize)
       when pos - segAddr < 100 do
    addr = addrToBe + :erlang.element(lSize, bases)
    noZeros = pos - segAddr
    nWs = [ws | <<0::size(noZeros)-unit(8), bSize::size(32), addr::size(32)>>]
    nSegAddr = segAddr + noZeros + 2 * 4
    write_segment_file(bins, bases, head, nWs, nSegAddr, sS)
  end

  defp write_segment_file(bins, bases, head, ws, segAddr, sS, pos, bSize, addrToBe, lSize) do
    addr = addrToBe + :erlang.element(lSize, bases)
    noZeros = pos - segAddr
    nWs = [ws, :dets_utils.make_zeros(noZeros) | <<bSize::size(32), addr::size(32)>>]
    nSegAddr = segAddr + noZeros + 2 * 4
    write_segment_file(bins, bases, head, nWs, nSegAddr, sS)
  end

  defp fast_write_all_sizes(cache, sizeT, head) do
    cacheL = :lists.reverse(:erlang.tuple_to_list(cache))
    fast_write_sizes(cacheL, tuple_size(cache), sizeT, head, [], [])
  end

  defp fast_write_sizes([], _Sz, _SizeT, head, nCL, pwriteList) do
    r_head(filename: fileName, fptr: fd) = head
    :ok = :dets_utils.pwrite(fd, fileName, pwriteList)
    :erlang.list_to_tuple(nCL)
  end

  defp fast_write_sizes([[_Addr] = c | cL], sz, sizeT, head, nCL, pwriteList) do
    fast_write_sizes(cL, sz - 1, sizeT, head, [c | nCL], pwriteList)
  end

  defp fast_write_sizes([[addr | c] | cL], sz, sizeT, head, nCL, pwriteList) do
    case :ets.lookup(sizeT, sz) do
      [] ->
        throw({:error, :invalid_objects_list})

      [{^sz, position, _ObjCounter, _NoCollections}] ->
        noColls = length(c)
        _ = :ets.update_counter(sizeT, sz, {3, noColls})
        pos = position + addr - noColls * (1 <<< (sz - 1))

        fast_write_sizes(cL, sz - 1, sizeT, head, [[addr] | nCL], [
          {pos, :lists.reverse(c)} | pwriteList
        ])
    end
  end

  defp prepare_file_init(noObjects, noKeys, noObjsPerSize, sizeT, head) do
    segSz = 512 * 4
    {_, segEnd, _} = :dets_utils.alloc(head, adjsz(segSz))
    head1 = r_head(head, no_objects: noObjects, no_keys: noKeys)
    true = :ets.insert(sizeT, {1, 0, [], 0})

    :lists.foreach(
      fn {logSz, noColls} ->
        true = :ets.insert(sizeT, {logSz + 1, 0, 0, noColls})
      end,
      noObjsPerSize
    )

    {newHead, nL0, maxSz, endOfFile} = allocate_all_objects(head1, sizeT)
    [{1, segAddr, [], 0} | nL] = nL0
    true = :ets.delete_all_objects(sizeT)

    :lists.foreach(
      fn x ->
        true = :ets.insert(sizeT, x)
      end,
      nL
    )

    bases =
      :lists.foldl(
        fn {lSz, p, _D, _N}, a ->
          :erlang.setelement(lSz, a, p)
        end,
        :erlang.make_tuple(maxSz, 0),
        nL
      )

    est =
      :lists.foldl(
        fn {lSz, _, _, n}, a ->
          a + (1 <<< (lSz - 1)) * n
        end,
        0,
        nL
      )

    :ok = write_bytes(newHead, endOfFile, est)
    {newHead, bases, segAddr, segEnd}
  end

  defp write_bytes(_Head, _EndOfFile, est) when est < 60 * 8192 do
    :ok
  end

  defp write_bytes(head, endOfFile, _Est) do
    fd = r_head(head, :fptr)
    {:ok, start} = :file.position(fd, :eof)
    bytesToWrite = endOfFile - start
    sizeInKB = 64

    bin =
      :erlang.list_to_binary(
        :lists.duplicate(
          sizeInKB * 4,
          :lists.seq(0, 255)
        )
      )

    write_loop(head, bytesToWrite, bin)
  end

  defp write_loop(head, bytesToWrite, bin)
       when bytesToWrite >= byte_size(bin) do
    case :file.write(r_head(head, :fptr), bin) do
      :ok ->
        write_loop(head, bytesToWrite - byte_size(bin), bin)

      error ->
        :dets_utils.file_error(error, r_head(head, :filename))
    end
  end

  defp write_loop(_Head, 0, _Bin) do
    :ok
  end

  defp write_loop(head, bytesToWrite, bin) do
    <<smallBin::size(bytesToWrite)-binary, _::binary>> = bin
    write_loop(head, bytesToWrite, smallBin)
  end

  defp allocate_all_objects(head, sizeT) do
    dTL =
      :lists.reverse(
        :lists.keysort(
          1,
          :ets.tab2list(sizeT)
        )
      )

    maxSz = :erlang.element(1, hd(dTL))
    {head1, nL} = allocate_all(head, dTL, [])
    {_Head, endOfFile, _} = :dets_utils.alloc(head1, 16)
    newHead = r_head(head1, maxobjsize: max_objsize(r_head(head1, :no_collections)))
    {newHead, nL, maxSz, endOfFile}
  end

  defp allocate_all(head, [{1, _, data, _}], l) do
    noParts = no_parts(r_head(head, :next))
    addr = 56 + 28 * 4 + 16 + 4 + 124 + 4 * 256 + noParts * 4 * 512
    {head, [{1, addr, data, 0} | l]}
  end

  defp allocate_all(head, [{lSize, _, data, noCollections} | dTL], l) do
    size = 1 <<< (lSize - 1)
    {_Head, addr, _} = :dets_utils.alloc(head, adjsz(size))
    head1 = :dets_utils.alloc_many(head, size, noCollections, addr)
    noColls = r_head(head1, :no_collections)
    newNoColls = :orddict.update_counter(lSize - 1, noCollections, noColls)
    newHead = r_head(head1, no_collections: newNoColls)
    e = {lSize, addr, data, noCollections}
    allocate_all(newHead, dTL, [e | l])
  end

  defp bin2term(bin, kp) do
    bin2term1(bin, kp, [])
  end

  defp bin2term1(
         [
           <<slot::size(32), seq::size(32), binTerm::binary>>
           | bTs
         ],
         kp,
         l
       ) do
    term = :erlang.binary_to_term(binTerm)
    key = :erlang.element(kp, term)
    bin2term1(bTs, kp, [{slot, key, seq, term, binTerm} | l])
  end

  defp bin2term1([], _Kp, l) do
    :lists.reverse(l)
  end

  defp write_all_sizes({} = cache, _SizeT, _Head, _More) do
    cache
  end

  defp write_all_sizes(cache, sizeT, head, more) do
    cacheL = :lists.reverse(:erlang.tuple_to_list(cache))
    sz = length(cacheL)

    nCL =
      case :ets.info(sizeT, :size) do
        1 when more === :no_more ->
          all_sizes(cacheL, sz, sizeT)

        _ ->
          write_sizes(cacheL, sz, sizeT, head)
      end

    :erlang.list_to_tuple(nCL)
  end

  defp all_sizes([] = cL, _Sz, _SizeT) do
    cL
  end

  defp all_sizes([[] = c | cL], sz, sizeT) do
    [c | all_sizes(cL, sz - 1, sizeT)]
  end

  defp all_sizes([c0 | cL], sz, sizeT) do
    c = :lists.reverse(c0)
    noCollections = length(c)
    true = :ets.insert(sizeT, {sz, 0, c, noCollections})
    [[] | all_sizes(cL, sz - 1, sizeT)]
  end

  defp write_sizes([] = cL, _Sz, _SizeT, _Head) do
    cL
  end

  defp write_sizes([[] = c | cL], sz, sizeT, head) do
    [c | write_sizes(cL, sz - 1, sizeT, head)]
  end

  defp write_sizes([c | cL], sz, sizeT, head) do
    {fileName, fd} =
      case :ets.lookup(sizeT, sz) do
        [] ->
          temp_file(head, sizeT, sz)

        [{_, _, {fN, f}, _}] ->
          {fN, f}
      end

    noCollections = length(c)
    _ = :ets.update_counter(sizeT, sz, {4, noCollections})

    case :file.write(fd, :lists.reverse(c)) do
      :ok ->
        [[] | write_sizes(cL, sz - 1, sizeT, head)]

      error ->
        :dets_utils.file_error(fileName, error)
    end
  end

  defp output_slots([e | es], head, cache, sizeT, noKeys, noObjs) do
    output_slots(e, es, [e], head, cache, sizeT, noKeys, noObjs)
  end

  defp output_slots([], _Head, cache, sizeT, noKeys, noObjs) do
    _ = :ets.update_counter(sizeT, :no, {2, noObjs})
    _ = :ets.update_counter(sizeT, :no, {3, noKeys})
    {:not_a_tuple, [], cache}
  end

  defp output_slots(e, [e1 | es], acc, head, cache, sizeT, noKeys, noObjs)
       when :erlang.element(1, e) === :erlang.element(1, e1) do
    output_slots(e1, es, [e1 | acc], head, cache, sizeT, noKeys, noObjs)
  end

  defp output_slots(e, [], acc, _Head, cache, sizeT, noKeys, noObjs) do
    _ = :ets.update_counter(sizeT, :no, {2, noObjs})
    _ = :ets.update_counter(sizeT, :no, {3, noKeys})
    {e, acc, cache}
  end

  defp output_slots(_E, l, acc, head, cache, sizeT, noKeys, noObjs) do
    output_slot(acc, head, cache, l, sizeT, noKeys, noObjs)
  end

  defp output_slot(es, head, cache, l, sizeT, noKeys, noObjs) do
    slot = :erlang.element(1, hd(es))
    {bins, size, no, kNo} = prep_slot(:lists.sort(es), head)
    nNoKeys = noKeys + kNo
    nNoObjs = noObjs + no
    bSize = size + 8
    lSize = sz2pos(bSize)
    size2 = 1 <<< (lSize - 1)
    pad = <<0::size(size2 - bSize)-unit(8)>>
    binObject = [<<bSize::size(32), 305_419_896::size(32)>>, bins | pad]

    cache1 =
      cond do
        lSize > tuple_size(cache) ->
          c1 =
            :erlang.list_to_tuple(
              :erlang.tuple_to_list(cache) ++
                :lists.duplicate(
                  lSize - tuple_size(cache),
                  []
                )
            )

          :erlang.setelement(lSize, c1, [binObject])

        true ->
          cL = :erlang.element(lSize, cache)
          :erlang.setelement(lSize, cache, [binObject | cL])
      end

    pBin = <<slot::size(32), bSize::size(32), lSize::size(8)>>
    pL = :erlang.element(1, cache1)
    nCache = :erlang.setelement(1, cache1, [pBin | pL])
    output_slots(l, head, nCache, sizeT, nNoKeys, nNoObjs)
  end

  defp prep_slot(l, head) when r_head(head, :type) !== :set do
    prep_slot(l, head, [])
  end

  defp prep_slot([{_Slot, key, _Seq, _T, bT} | l], _Head) do
    prep_set_slot(l, key, bT, 0, 0, 0, [])
  end

  defp prep_slot([{_Slot, key, seq, t, _BT} | l], head, w) do
    prep_slot(l, head, [{key, {seq, {:insert, t}}} | w])
  end

  defp prep_slot([], head, w) do
    wLs = :dets_utils.family(w)
    {[], bins, size, no, kNo, _} = eval_slot(wLs, [], r_head(head, :type), [], [], 0, 0, 0, false)
    {bins, size, no, kNo}
  end

  defp prep_set_slot([{_, k, _Seq, _T1, bT1} | l], k, _BT, sz, noKeys, noObjs, ws) do
    prep_set_slot(l, k, bT1, sz, noKeys, noObjs, ws)
  end

  defp prep_set_slot([{_, k1, _Seq, _T1, bT1} | l], _K, bT, sz, noKeys, noObjs, ws) do
    bSize = byte_size(bT) + 4
    nWs = [ws, <<bSize::size(32)>> | bT]
    prep_set_slot(l, k1, bT1, sz + bSize, noKeys + 1, noObjs + 1, nWs)
  end

  defp prep_set_slot([], _K, bT, sz, noKeys, noObjs, ws) do
    bSize = byte_size(bT) + 4
    {[ws, <<bSize::size(32)>> | bT], sz + bSize, noKeys + 1, noObjs + 1}
  end

  defp segment_file(sizeT, head, fileData, segEnd) do
    i = 2
    true = :ets.delete_all_objects(sizeT)

    :lists.foreach(
      fn x ->
        true = :ets.insert(sizeT, x)
      end,
      fileData
    )

    [{1, segAddr, data, 0} | fileData1] = fileData

    newData =
      case data do
        {inFile, in0} ->
          {outFile, out} = temp_file(head, sizeT, i)
          _ = :file.close(in0)

          {:ok, in__} =
            :dets_utils.open(
              inFile,
              [:raw, :binary, :read]
            )

          {:ok, 0} = :dets_utils.position(in__, inFile, :bof)
          seg_file(segAddr, segAddr, in__, inFile, out, outFile, sizeT, segEnd)
          _ = :file.close(in__)
          _ = :file.delete(inFile)
          {outFile, out}

        objects ->
          {lastAddr, b} = seg_file(objects, segAddr, segAddr, sizeT, [])
          :dets_utils.disk_map_segment(segAddr, b)
          finalZ = segEnd - lastAddr
          [b | :dets_utils.make_zeros(finalZ)]
      end

    true = :ets.delete_all_objects(sizeT)

    :lists.foreach(
      fn x ->
        true = :ets.insert(sizeT, x)
      end,
      [{10000, segAddr, newData, 0} | fileData1]
    )

    :ok
  end

  defp seg_file(addr, sS, in__, inFile, out, outFile, sizeT, segEnd) do
    case :dets_utils.read_n(in__, 4500) do
      :eof ->
        finalZ = segEnd - addr
        :dets_utils.fwrite(out, outFile, :dets_utils.make_zeros(finalZ))

      bin ->
        {newAddr, l} = seg_file(bin, addr, sS, sizeT, [])
        :dets_utils.disk_map_segment(addr, l)
        :ok = :dets_utils.fwrite(out, outFile, l)
        seg_file(newAddr, sS, in__, inFile, out, outFile, sizeT, segEnd)
    end
  end

  defp seg_file(
         <<slot::size(32), bSize::size(32), lSize::size(8), t::binary>>,
         addr,
         sS,
         sizeT,
         l
       ) do
    seg_file_item(t, addr, sS, sizeT, l, slot, bSize, lSize)
  end

  defp seg_file(
         [
           <<slot::size(32), bSize::size(32), lSize::size(8)>>
           | t
         ],
         addr,
         sS,
         sizeT,
         l
       ) do
    seg_file_item(t, addr, sS, sizeT, l, slot, bSize, lSize)
  end

  defp seg_file([], addr, _SS, _SizeT, l) do
    {addr, :lists.reverse(l)}
  end

  defp seg_file(<<>>, addr, _SS, _SizeT, l) do
    {addr, :lists.reverse(l)}
  end

  defp seg_file_item(t, addr, sS, sizeT, l, slot, bSize, lSize) do
    slotPos = sS + 2 * 4 * slot
    noZeros = slotPos - addr
    pSize = noZeros + 2 * 4
    inc = 1 <<< (lSize - 1)
    collP = :ets.update_counter(sizeT, lSize, inc) - inc

    pointerBin =
      cond do
        noZeros === 0 ->
          <<bSize::size(32), collP::size(32)>>

        noZeros > 100 ->
          [:dets_utils.make_zeros(noZeros) | <<bSize::size(32), collP::size(32)>>]

        true ->
          <<0::size(noZeros)-unit(8), bSize::size(32), collP::size(32)>>
      end

    seg_file(t, addr + pSize, sS, sizeT, [pointerBin | l])
  end

  defp temp_file(head, sizeT, n) do
    tmpName = :lists.concat([r_head(head, :filename), :., n])

    {:ok, fd} =
      :dets_utils.open(
        tmpName,
        [:raw, :binary, :write]
      )

    true = :ets.insert(sizeT, {n, 0, {tmpName, fd}, 0})
    {tmpName, fd}
  end

  def fsck_input(head, fd, cntrs, fileHeader) do
    maxSz0 =
      case r_fileheader(fileHeader, :has_md5) do
        true when is_list(r_fileheader(fileHeader, :no_colls)) ->
          1 <<< max_objsize(r_fileheader(fileHeader, :no_colls))

        _ ->
          case :file.position(fd, :eof) do
            {:ok, pos} ->
              pos

            _ ->
              1 <<< 32
          end
      end

    maxSz = :erlang.max(maxSz0, 8192)
    state0 = fsck_read(56 + 28 * 4 + 16 + 4 + 124 + 4 * 256, fd, [], 0)
    fsck_input(head, state0, fd, maxSz, cntrs)
  end

  defp fsck_input(head, state, fd, maxSz, cntrs) do
    fn
      :close ->
        :ok

      :read ->
        case state do
          :done ->
            :end_of_input

          {:done, l, _Seq} ->
            r = count_input(l)
            {r, fsck_input(head, :done, fd, maxSz, cntrs)}

          {:cont, l, bin, pos, seq} ->
            r = count_input(l)
            fR = fsck_objs(bin, r_head(head, :keypos), head, [], seq)
            newState = fsck_read(fR, pos, fd, maxSz, head)
            {r, fsck_input(head, newState, fd, maxSz, cntrs)}
        end
    end
  end

  defp count_input(l) do
    :lists.reverse(l)
  end

  defp fsck_read(pos, f, l, seq) do
    case :file.position(f, pos) do
      {:ok, _} ->
        read_more_bytes([], 0, pos, f, l, seq)

      _Error ->
        {:done, l, seq}
    end
  end

  defp fsck_read({:more, bin, sz, l, seq}, pos, f, maxSz, head)
       when sz > maxSz do
    fR = skip_bytes(bin, 16, r_head(head, :keypos), head, l, seq)
    fsck_read(fR, pos, f, maxSz, head)
  end

  defp fsck_read({:more, bin, sz, l, seq}, pos, f, _MaxSz, _Head) do
    read_more_bytes(bin, sz, pos, f, l, seq)
  end

  defp fsck_read({:new, skip, l, seq}, pos, f, _MaxSz, _Head) do
    newPos = pos + skip
    fsck_read(newPos, f, l, seq)
  end

  defp read_more_bytes(b, min, pos, f, l, seq) do
    max =
      cond do
        min < 8192 ->
          8192

        true ->
          min
      end

    case :dets_utils.read_n(f, max) do
      :eof ->
        {:done, l, seq}

      bin ->
        newPos = pos + byte_size(bin)
        {:cont, l, :erlang.list_to_binary([b, bin]), newPos, seq}
    end
  end

  defp fsck_objs(bin = <<sz::size(32), status::size(32), tail::binary>>, kp, head, l, seq) do
    cond do
      status === 305_419_896 ->
        sz1 = sz - 8

        case tail do
          <<binTerm::size(sz1)-binary, tail2::binary>> ->
            case (try do
                    bin2keybins(binTerm, head)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              {:EXIT, _Reason} ->
                skip_bytes(bin, 16, kp, head, l, seq)

              bOs ->
                {nL, nSeq} = make_objects(bOs, seq, kp, head, l)
                skip = 1 <<< (sz2pos(sz) - 1 - sz)
                skip_bytes(tail2, skip, kp, head, nL, nSeq)
            end

          _ when byte_size(tail) < sz1 ->
            {:more, bin, sz, l, seq}
        end

      true ->
        skip_bytes(bin, 16, kp, head, l, seq)
    end
  end

  defp fsck_objs(bin, _Kp, _Head, l, seq) do
    {:more, bin, 0, l, seq}
  end

  defp make_objects([{k, bT} | os], seq, kp, head, l) do
    obj = make_object(head, k, seq, bT)
    make_objects(os, seq + 1, kp, head, [obj | l])
  end

  defp make_objects([], seq, _Kp, _Head, l) do
    {l, seq}
  end

  defp make_object(head, key, seq, bT) do
    slot = db_hash(key, head)
    <<slot::size(32), seq::size(32), bT::binary>>
  end

  defp skip_bytes(bin, skip, kp, head, l, seq) do
    case bin do
      <<_::size(skip)-binary, tail::binary>> ->
        fsck_objs(tail, kp, head, l, seq)

      _ when byte_size(bin) < skip ->
        {:new, skip - byte_size(bin), l, seq}
    end
  end

  def do_perform_save(h) do
    {:ok, freeListsPointer} = :dets_utils.position(h, :eof)
    h1 = r_head(h, freelists_p: freeListsPointer)
    {fLW, fLSize} = free_lists_to_file(h1)
    fileSize = freeListsPointer + fLSize + 4

    adjustedFileSize =
      case r_head(h, :base) do
        1336 ->
          fileSize

        base ->
          fileSize - base
      end

    :ok =
      :dets_utils.write(
        h1,
        [fLW | <<adjustedFileSize::size(32)>>]
      )

    fileHeader = file_header(h1, freeListsPointer, 1)

    case :dets_utils.debug_mode() do
      true ->
        tmpHead0 = init_freelist(r_head(h1, fixed: false))
        tmpHead = r_head(tmpHead0, base: r_head(h1, :base))

        case (try do
                :dets_utils.all_allocated_as_list(tmpHead) ===
                  :dets_utils.all_allocated_as_list(h1)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          true ->
            :dets_utils.pwrite(h1, [{0, fileHeader}])

          _ ->
            throw(
              :dets_utils.corrupt_reason(
                h1,
                {:failed_to_save_free_lists, freeListsPointer, r_head(tmpHead, :freelists),
                 r_head(h1, :freelists)}
              )
            )
        end

      false ->
        :dets_utils.pwrite(h1, [{0, fileHeader}])
    end
  end

  defp file_header(head, freeListsPointer, closedProperly) do
    noColls =
      case r_head(head, :no_collections) do
        :undefined ->
          []

        nC ->
          nC
      end

    l =
      :orddict.merge(
        fn _K, v1, v2 ->
          v1 + v2
        end,
        noColls,
        :lists.map(
          fn x ->
            {x, 0}
          end,
          :lists.seq(4, 32 - 1)
        )
      )

    cW =
      :lists.map(
        fn {_LSz, n} ->
          <<n::size(32)>>
        end,
        l
      )

    file_header(head, freeListsPointer, closedProperly, cW)
  end

  defp file_header(head, freeListsPointer, closedProperly, noColls) do
    cookie = 11_259_375
    typeCode = :dets_utils.type_to_code(r_head(head, :type))
    version = 9
    hashMethod = hash_method_to_code(r_head(head, :hash_bif))
    h1 = <<freeListsPointer::size(32), cookie::size(32), closedProperly::size(32)>>

    h2 =
      <<typeCode::size(32), version::size(32), r_head(head, :m)::size(32),
        r_head(head, :next)::size(32), r_head(head, :keypos)::size(32),
        r_head(head, :no_objects)::size(32), r_head(head, :no_keys)::size(32),
        r_head(head, :min_no_slots)::size(32), r_head(head, :max_no_slots)::size(32),
        hashMethod::size(32), r_head(head, :n)::size(32)>>

    digH = [h2 | noColls]

    mD5 =
      case r_head(head, :has_md5) do
        true ->
          :erlang.md5(digH)

        false ->
          <<0::size(16)-unit(8)>>
      end

    base =
      case r_head(head, :base) do
        1336 ->
          <<0::size(32)>>

        flBase ->
          <<flBase::size(32)>>
      end

    [h1, digH, mD5, base | <<0::size(124)-unit(8)>>]
  end

  defp free_lists_to_file(h) do
    fL = :dets_utils.get_freelists(h)
    free_list_to_file(fL, h, 1, tuple_size(fL), [], 0)
  end

  defp free_list_to_file(_Ftab, _H, pos, sz, ws, wsSz) when pos > sz do
    {[ws | <<4 + 8::size(32), 61_591_023::size(32), 12345::size(32)>>], wsSz + 4 + 8}
  end

  defp free_list_to_file(ftab, h, pos, sz, ws, wsSz) do
    max = div(4096 - 4 - 8, 4)

    f = fn
      n, l, w, s when n === 0 ->
        {n, l, w, s}

      n, l, w, s ->
        {l1, n1, more} =
          cond do
            n > max ->
              {:lists.sublist(l, max), max, {n - max, :lists.nthtail(max, l)}}

            true ->
              {l, n, :no_more}
          end

        size = n1 * 4 + 4 + 8
        header = <<size::size(32), 61_591_023::size(32), pos::size(32)>>
        nW = [w, header | l1]

        case more do
          :no_more ->
            {0, [], nW, s + size}

          {nN, nL} ->
            :ok = :dets_utils.write(h, nW)
            {nN, nL, [], s + size}
        end
    end

    {nWs, nWsSz} =
      :dets_utils.tree_to_bin(
        :erlang.element(
          pos,
          ftab
        ),
        f,
        max,
        ws,
        wsSz
      )

    free_list_to_file(ftab, h, pos + 1, sz, nWs, nWsSz)
  end

  defp free_lists_from_file(h, pos) do
    {:ok, ^pos} = :dets_utils.position(r_head(h, :fptr), r_head(h, :filename), pos)
    fL = :dets_utils.empty_free_lists()

    case (try do
            bin_to_tree([], h, :start, fL, -1, [])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        throw({:error, {:bad_freelists, r_head(h, :filename)}})

      ftab ->
        r_head(h,
          freelists: ftab,
          base: 56 + 28 * 4 + 16 + 4 + 124 + 4 * 256
        )
    end
  end

  defp bin_to_tree(bin, h, lastPos, ftab, a0, l) do
    case bin do
      <<_Size::size(32), 61_591_023::size(32), 12345::size(32), _::binary>>
      when l === [] ->
        ftab

      <<_Size::size(32), 61_591_023::size(32), 12345::size(32), _::binary>> ->
        :erlang.setelement(lastPos, ftab, :dets_utils.list_to_tree(l))

      <<size::size(32), 61_591_023::size(32), pos::size(32), t::binary>>
      when byte_size(t) >= size - 4 - 8 ->
        {nFtab, l1, a1} =
          cond do
            pos !== lastPos and lastPos !== :start ->
              tree = :dets_utils.list_to_tree(l)
              {:erlang.setelement(lastPos, ftab, tree), [], -1}

            true ->
              {ftab, l, a0}
          end

        {nL, b2, a2} = bin_to_tree1(t, size - 8 - 4, a1, l1)
        bin_to_tree(b2, h, pos, nFtab, a2, nL)

      _ ->
        bin2 = :dets_utils.read_n(r_head(h, :fptr), 4096)
        bin_to_tree(:erlang.list_to_binary([bin | bin2]), h, lastPos, ftab, a0, l)
    end
  end

  defp bin_to_tree1(
         <<a1::size(32), a2::size(32), a3::size(32), a4::size(32), t::binary>>,
         size,
         a,
         l
       )
       when size >= 16 and a < a1 and a1 < a2 and a2 < a3 and
              a3 < a4 do
    bin_to_tree1(t, size - 16, a4, [a4, a3, a2, a1 | l])
  end

  defp bin_to_tree1(<<a1::size(32), t::binary>>, size, a, l)
       when size >= 4 and a < a1 do
    bin_to_tree1(t, size - 4, a1, [a1 | l])
  end

  defp bin_to_tree1(b, 0, a, l) do
    {l, b, a}
  end

  def slot_objs(h, slot) when slot >= r_head(h, :next) do
    :"$end_of_table"
  end

  def slot_objs(h, slot) do
    {:ok, _Pointer, objects} = slot_objects(h, slot)
    objects
  end

  defp h(i, :phash2) do
    :erlang.phash2(i)
  end

  defp h(i, :phash) do
    :erlang.phash(i, 67_108_863) - 1
  end

  def db_hash(key, head) when r_head(head, :hash_bif) === :phash2 do
    h = :erlang.phash2(key)
    hash = h &&& r_head(head, :m) - 1

    cond do
      hash < r_head(head, :n) ->
        h &&& r_head(head, :m2) - 1

      true ->
        hash
    end
  end

  def db_hash(key, head) do
    h = h(key, r_head(head, :hash_bif))
    hash = rem(h, r_head(head, :m))

    cond do
      hash < r_head(head, :n) ->
        rem(h, r_head(head, :m2))

      true ->
        hash
    end
  end

  defp hash_method_to_code(:phash2) do
    1
  end

  defp hash_method_to_code(:phash) do
    0
  end

  defp code_to_hash_method(1) do
    :phash2
  end

  defp code_to_hash_method(0) do
    :phash
  end

  defp code_to_hash_method(_) do
    :undefined
  end

  def no_slots(head) do
    {r_head(head, :min_no_slots), r_head(head, :next), r_head(head, :max_no_slots)}
  end

  def table_parameters(head) do
    case r_head(head, :no_collections) do
      :undefined ->
        :undefined

      cL ->
        noColls0 =
          :lists.foldl(
            fn
              {_, 0}, a ->
                a

              e, a ->
                [e | a]
            end,
            [],
            cL
          )

        noColls = :lists.reverse(noColls0)

        r__hash2(
          file_format_version: 9,
          bchunk_format_version: 1,
          file: :filename.basename(r_head(head, :filename)),
          type: r_head(head, :type),
          keypos: r_head(head, :keypos),
          hash_method: hash_method_to_code(r_head(head, :hash_bif)),
          n: r_head(head, :n),
          m: r_head(head, :m),
          next: r_head(head, :next),
          min: r_head(head, :min_no_slots),
          max: r_head(head, :max_no_slots),
          no_objects: r_head(head, :no_objects),
          no_keys: r_head(head, :no_keys),
          no_colls: noColls
        )
    end
  end

  defp re_hash(head, slotStart) do
    fromSlotPos = slot_position(slotStart)
    toSlotPos = slot_position(slotStart + r_head(head, :m))
    rSpec = [{fromSlotPos, 4 * 512}]
    {:ok, [fromBin]} = :dets_utils.pread(rSpec, head)
    split_bins(fromBin, head, fromSlotPos, toSlotPos, [], [], 0)
  end

  defp split_bins(<<>>, head, _Pos1, _Pos2, _ToRead, _L, 0) do
    {head, :ok}
  end

  defp split_bins(<<>>, head, pos1, pos2, toRead, l, _SoFar) do
    re_hash_write(head, toRead, l, pos1, pos2)
  end

  defp split_bins(fB, head, pos1, pos2, toRead, l, soFar) do
    <<sz1::size(32), p1::size(32), fT::binary>> = fB
    <<b1::size(8)-binary, _::binary>> = fB
    nSoFar = soFar + sz1
    nPos1 = pos1 + 2 * 4
    nPos2 = pos2 + 2 * 4

    cond do
      nSoFar > 10 * 8192 and toRead !== [] ->
        {newHead, :ok} = re_hash_write(head, toRead, l, pos1, pos2)
        split_bins(fB, newHead, pos1, pos2, [], [], 0)

      sz1 === 0 ->
        e = {:skip, b1}
        split_bins(fT, head, nPos1, nPos2, toRead, [e | l], nSoFar)

      true ->
        e = {sz1, p1, b1, pos1, pos2}
        newToRead = [{p1, sz1} | toRead]
        split_bins(fT, head, nPos1, nPos2, newToRead, [e | l], nSoFar)
    end
  end

  defp re_hash_write(head, toRead, l, pos1, pos2) do
    check_pread2_arg(toRead, head)
    {:ok, bins} = :dets_utils.pread(toRead, head)
    z = <<0::size(32), 0::size(32)>>
    {head1, binFS, binTS, wsB} = re_hash_slots(bins, l, head, z, [], [], [])
    wPos1 = pos1 - 2 * 4 * length(l)
    wPos2 = pos2 - 2 * 4 * length(l)
    toWrite = [{wPos1, binFS}, {wPos2, binTS} | wsB]
    :dets_utils.pwrite(head1, toWrite)
  end

  defp re_hash_slots(bins, [{:skip, b1} | l], head, z, binFS, binTS, wsB) do
    re_hash_slots(bins, l, head, z, [b1 | binFS], [z | binTS], wsB)
  end

  defp re_hash_slots([fB | bins], [e | l], head, z, binFS, binTS, wsB) do
    {sz1, p1, b1, pos1, pos2} = e

    keyObjs =
      case (try do
              per_key(head, fB)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:EXIT, _Error} ->
          bad = :dets_utils.bad_object(:re_hash_slots, {fB, e})
          throw(:dets_utils.corrupt_reason(head, bad))

        else__ ->
          else__
      end

    case re_hash_split(keyObjs, head, [], 0, [], 0) do
      {_KL, _KSz, [], 0} ->
        ^sz1 = _KSz + 8
        re_hash_slots(bins, l, head, z, [b1 | binFS], [z | binTS], wsB)

      {[], 0, _ML, _MSz} ->
        ^sz1 = _MSz + 8
        re_hash_slots(bins, l, head, z, [z | binFS], [b1 | binTS], wsB)

      {kL, kSz, mL, mSz}
      when kL !== [] and kSz > 0 and
             mL !== [] and mSz > 0 ->
        {head1, fS1, ws1} = updated(head, p1, sz1, kSz, pos1, kL, true, :foo, :bar)
        {newHead, [{^pos2, bin2}], ws2} = updated(head1, 0, 0, mSz, pos2, mL, true, :foo, :bar)

        newBinFS =
          case fS1 do
            [{^pos1, bin1}] ->
              [bin1 | binFS]

            [] ->
              [b1 | binFS]
          end

        newBinTS = [bin2 | binTS]
        newWsB = ws2 ++ ws1 ++ wsB
        re_hash_slots(bins, l, newHead, z, newBinFS, newBinTS, newWsB)
    end
  end

  defp re_hash_slots([], [], head, _Z, binFS, binTS, wsB) do
    {head, binFS, binTS, :lists.reverse(wsB)}
  end

  defp re_hash_split([e | keyObjs], head, kL, kSz, mL, mSz) do
    {key, sz, bin, _Item, _Objs} = e
    new = rem(h(key, r_head(head, :hash_bif)), r_head(head, :m2))

    cond do
      new >= r_head(head, :m) ->
        re_hash_split(keyObjs, head, kL, kSz, [bin | mL], mSz + sz)

      true ->
        re_hash_split(keyObjs, head, [bin | kL], kSz + sz, mL, mSz)
    end
  end

  defp re_hash_split([], _Head, kL, kSz, mL, mSz) do
    {:lists.reverse(kL), kSz, :lists.reverse(mL), mSz}
  end

  def write_cache(head) do
    c = r_head(head, :cache)

    case :dets_utils.is_empty_cache(c) do
      true ->
        {head, [], []}

      false ->
        {newC, maxInserts, perKey} = :dets_utils.reset_cache(c)

        maxNoInsertedKeys =
          :erlang.min(
            maxInserts,
            length(perKey)
          )

        head1 = r_head(head, cache: newC)

        case may_grow(head1, maxNoInsertedKeys, :once) do
          {head2, :ok} ->
            eval_work_list(head2, perKey)

          headError ->
            throw(headError)
        end
    end
  end

  def may_grow(head, 0, :once) do
    {head, :ok}
  end

  def may_grow(head, _N, _How) when r_head(head, :fixed) !== false do
    {head, :ok}
  end

  def may_grow(r_head(access: :read) = head, _N, _How) do
    {head, :ok}
  end

  def may_grow(head, _N, _How)
      when r_head(head, :next) >= r_head(head, :max_no_slots) do
    {head, :ok}
  end

  def may_grow(head, n, how) do
    extra =
      :erlang.min(
        2 * 256,
        r_head(head, :no_keys) + n - r_head(head, :next)
      )

    case (try do
            may_grow1(head, extra, how)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, _Reason} = error ->
        :dets_utils.corrupt(head, error)

      {newHead, reply} when elem(head, 0) === :head ->
        {newHead, reply}
    end
  end

  defp may_grow1(head, extra, :many_times) when extra > 256 do
    reply = grow(head, 1, :undefined)
    send(self(), {:"$dets_call", self(), :may_grow})
    reply
  end

  defp may_grow1(head, extra, _How) do
    grow(head, extra, :undefined)
  end

  defp grow(head, extra, _SegZero) when extra <= 0 do
    {head, :ok}
  end

  defp grow(head, extra, :undefined) do
    grow(head, extra, seg_zero())
  end

  defp grow(head, _Extra, _SegZero)
       when r_head(head, :next) >= r_head(head, :max_no_slots) do
    {head, :ok}
  end

  defp grow(head, extra, segZero) do
    r_head(n: n, next: next, m: m) = head
    segNum = div(next, 256)
    {head0, w, ws1} = allocate_segment(head, segZero, segNum)
    {head1, :ok} = :dets_utils.pwrite(head0, [w | ws1])
    {head2, :ok} = re_hash(head1, n)

    newHead =
      cond do
        n + 256 === m ->
          r_head(head2, n: 0, next: next + 256, m: 2 * m, m2: 4 * m)

        true ->
          r_head(head2, n: n + 256, next: next + 256)
      end

    true = hash_invars(newHead)
    grow(newHead, extra - 256, segZero)
  end

  defp hash_invars(h) do
    hash_invars(
      r_head(h, :n),
      r_head(h, :m),
      r_head(h, :next),
      r_head(h, :min_no_slots),
      r_head(h, :max_no_slots)
    )
  end

  defp hash_invars(n, m, next, min, max) do
    :erlang.and(
      :erlang.and(
        :erlang.and(
          :erlang.and(
            :erlang.and(
              :erlang.and(
                :erlang.and(
                  :erlang.and(
                    :erlang.and(
                      :erlang.and(
                        :erlang.and(
                          :erlang.and(
                            :erlang.and(
                              n &&& 256 - 1 === 0,
                              m &&& 256 - 1 === 0
                            ),
                            next &&& 256 - 1 === 0
                          ),
                          min &&& 256 - 1 === 0
                        ),
                        max &&& 256 - 1 === 0
                      ),
                      0 <= n
                    ),
                    n <= m
                  ),
                  n <= 2 * next
                ),
                m <= next
              ),
              next <= 2 * m
            ),
            0 <= min
          ),
          min <= next
        ),
        next <= max
      ),
      min <= m
    )
  end

  defp seg_zero() do
    <<0::size(4 * 512)-unit(8)>>
  end

  def find_object(head, object) do
    key = :erlang.element(r_head(head, :keypos), object)
    slot = db_hash(key, head)
    find_object(head, object, slot)
  end

  defp find_object(h, _Obj, slot) when slot >= r_head(h, :next) do
    false
  end

  defp find_object(h, obj, slot) do
    case (try do
            slot_objects(h, slot)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, pointer, objects} ->
        case :lists.member(obj, objects) do
          true ->
            {:ok, pointer}

          false ->
            false
        end

      _ ->
        false
    end
  end

  defp slot_objects(head, slot) do
    slotPos = slot_position(slot)
    maxSize = maxobjsize(head)

    case :dets_utils.ipread(head, slotPos, maxSize) do
      {:ok, {bucketSz, pointer, <<^bucketSz::size(32), _St::size(32), keysObjs::binary>>}} ->
        case (try do
                bin2objs(keysObjs, r_head(head, :type), [])
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _Error} ->
            bad =
              :dets_utils.bad_object(
                :slot_objects,
                {slotPos, keysObjs}
              )

            throw(:dets_utils.corrupt_reason(head, bad))

          objs when is_list(objs) ->
            {:ok, pointer, :lists.reverse(objs)}
        end

      [] ->
        {:ok, 0, []}

      badRead ->
        bad =
          :dets_utils.bad_object(
            :slot_objects,
            {slotPos, badRead}
          )

        throw(:dets_utils.corrupt_reason(head, bad))
    end
  end

  defp eval_work_list(head, [{key, [{_Seq, {:lookup, pid}}]}]) do
    slotPos = slot_position(db_hash(key, head))
    maxSize = maxobjsize(head)

    objs =
      case :dets_utils.ipread(head, slotPos, maxSize) do
        {:ok, {_BucketSz, _Pointer, bin}} ->
          case (try do
                  per_key(head, bin)
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end) do
            {:EXIT, _Error} ->
              bad =
                :dets_utils.bad_object(
                  :eval_work_list,
                  {slotPos, bin}
                )

              throw(:dets_utils.corrupt_reason(head, bad))

            keyObjs when is_list(keyObjs) ->
              case :dets_utils.mkeysearch(key, 1, keyObjs) do
                false ->
                  []

                {:value, {^key, _KS, _KB, o, os}} ->
                  case (try do
                          binobjs2terms(os)
                        catch
                          :error, e -> {:EXIT, {e, __STACKTRACE__}}
                          :exit, e -> {:EXIT, e}
                          e -> e
                        end) do
                    {:EXIT, _Error} ->
                      bad =
                        :dets_utils.bad_object(
                          :eval_work_list,
                          {slotPos, bin, keyObjs}
                        )

                      throw(:dets_utils.corrupt_reason(head, bad))

                    terms when is_list(terms) ->
                      get_objects([o | terms])
                  end
              end
          end

        [] ->
          []

        badRead ->
          bad =
            :dets_utils.bad_object(
              :eval_work_list,
              {slotPos, badRead}
            )

          throw(:dets_utils.corrupt_reason(head, bad))
      end

    {head, [{pid, objs}], []}
  end

  defp eval_work_list(head, perKey) do
    sWLs = tag_with_slot(perKey, head, [])
    p1 = :dets_utils.family(sWLs)
    {perSlot, slotPositions} = remove_slot_tag(p1, [], [])
    {:ok, bins} = :dets_utils.pread(slotPositions, head)
    read_buckets(perSlot, slotPositions, bins, head, [], [], [], [], 0, 0, 0)
  end

  defp tag_with_slot([{k, _} = wL | wLs], head, l) do
    tag_with_slot(wLs, head, [{db_hash(k, head), wL} | l])
  end

  defp tag_with_slot([], _Head, l) do
    l
  end

  defp remove_slot_tag([{s, sWLs} | sSWLs], ls, sPs) do
    remove_slot_tag(sSWLs, [sWLs | ls], [{slot_position(s), 4 * 2} | sPs])
  end

  defp remove_slot_tag([], ls, sPs) do
    {ls, sPs}
  end

  defp read_buckets(
         [wLs | sPs],
         [{p1, _8} | ss],
         [<<_Zero::size(32), p2::size(32)>> | bs],
         head,
         pWLs,
         toRead,
         lU,
         ws,
         noObjs,
         noKeys,
         soFar
       )
       when p2 === 0 do
    {newHead, nLU, nWs, no, kNo} = eval_bucket_keys(wLs, p1, 0, 0, [], head, ws, lU)
    newNoObjs = no + noObjs
    newNoKeys = kNo + noKeys
    read_buckets(sPs, ss, bs, newHead, pWLs, toRead, nLU, nWs, newNoObjs, newNoKeys, soFar)
  end

  defp read_buckets(
         [workLists | sPs],
         [{p1, _8} | ss],
         [<<size::size(32), p2::size(32)>> | bs],
         head,
         pWLs,
         toRead,
         lU,
         ws,
         noObjs,
         noKeys,
         soFar
       )
       when soFar + size < 10 * 8192 or toRead === [] do
    newToRead = [{p2, size} | toRead]
    newPWLs = [{p2, p1, workLists} | pWLs]
    newSoFar = soFar + size
    read_buckets(sPs, ss, bs, head, newPWLs, newToRead, lU, ws, noObjs, noKeys, newSoFar)
  end

  defp read_buckets(sPs, ss, bs, head, pWLs0, toRead0, lU, ws, noObjs, noKeys, soFar)
       when soFar > 0 do
    pWLs = :lists.keysort(1, pWLs0)
    toRead = :lists.keysort(1, toRead0)
    check_pread2_arg(toRead, head)
    {:ok, bins} = :dets_utils.pread(toRead, head)

    case (try do
            eval_buckets(bins, pWLs, head, lU, ws, 0, 0)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, newHead, nLU, [], 0, 0} ->
        read_buckets(sPs, ss, bs, newHead, [], [], nLU, [], noObjs, noKeys, 0)

      {:ok, head1, nLU, nWs, no, kNo} ->
        newNoObjs = noObjs + no
        newNoKeys = noKeys + kNo

        {newHead, :ok} =
          :dets_utils.pwrite(
            head1,
            :lists.reverse(nWs)
          )

        read_buckets(sPs, ss, bs, newHead, [], [], nLU, [], newNoObjs, newNoKeys, 0)

      error ->
        bad =
          :dets_utils.bad_object(
            :read_buckets,
            {bins, error}
          )

        throw(:dets_utils.corrupt_reason(head, bad))
    end
  end

  defp read_buckets([], [], [], head, [], [], lU, ws, noObjs, noKeys, 0) do
    {newHead, nWs} = update_no_keys(head, ws, noObjs, noKeys)
    {newHead, lU, :lists.reverse(nWs)}
  end

  defp eval_buckets([bin | bins], [sP | sPs], head, lU, ws, noObjs, noKeys) do
    {pos, p1, wLs} = sP
    keyObjs = per_key(head, bin)

    {newHead, nLU, nWs, no, kNo} =
      eval_bucket_keys(wLs, p1, pos, byte_size(bin), keyObjs, head, ws, lU)

    eval_buckets(bins, sPs, newHead, nLU, nWs, noObjs + no, noKeys + kNo)
  end

  defp eval_buckets([], [], head, lU, ws, noObjs, noKeys) do
    {:ok, head, lU, ws, noObjs, noKeys}
  end

  defp eval_bucket_keys(wLs, slotPos, pos, oldSize, keyObjs, head, ws, lU) do
    {nLU, bins, bSize, no, kNo, ch} =
      eval_slot(wLs, keyObjs, r_head(head, :type), lU, [], 0, 0, 0, false)

    {newHead, w1, w2} = updated(head, pos, oldSize, bSize, slotPos, bins, ch, no, kNo)
    {newHead, nLU, w2 ++ w1 ++ ws, no, kNo}
  end

  defp updated(head, pos, oldSize, bSize, slotPos, bins, ch, deltaNoOs, deltaNoKs) do
    binsSize = bSize + 8

    cond do
      pos === 0 and bSize === 0 ->
        {head, [], []}

      pos === 0 and bSize > 0 ->
        {head1, newPos, fPos} =
          :dets_utils.alloc(
            head,
            adjsz(binsSize)
          )

        newHead = one_bucket_added(head1, fPos - 1)

        w1 =
          {newPos,
           [
             <<binsSize::size(32), 305_419_896::size(32)>>
             | bins
           ]}

        w2 = {slotPos, <<binsSize::size(32), newPos::size(32)>>}
        {newHead, [w2], [w1]}

      pos !== 0 and bSize === 0 ->
        {head1, fPos} = :dets_utils.free(head, pos, adjsz(oldSize))
        newHead = one_bucket_removed(head1, fPos - 1)
        w1 = {pos + 4, <<61_591_023::size(32)>>}
        w2 = {slotPos, <<0::size(32), 0::size(32)>>}
        {newHead, [w2], [w1]}

      pos !== 0 and bSize > 0 and ch === false ->
        {head, [], []}

      pos !== 0 and bSize > 0 ->
        overwrite0 =
          cond do
            oldSize === binsSize ->
              :same

            true ->
              sz2pos(oldSize) === sz2pos(binsSize)
          end

        overwrite =
          cond do
            r_head(head, :fixed) !== false ->
              :erlang.and(
                :erlang.and(
                  overwrite0 !== false,
                  deltaNoOs === 0
                ),
                deltaNoKs === 0
              )

            true ->
              overwrite0
          end

        cond do
          overwrite === :same ->
            w1 = {pos + 8, bins}
            {head, [], [w1]}

          overwrite ->
            w1 =
              {pos,
               [
                 <<binsSize::size(32), 305_419_896::size(32)>>
                 | bins
               ]}

            w2 = {slotPos, <<binsSize::size(32), pos::size(32)>>}
            {head, [w2], [w1]}

          true ->
            {head1, fPosF} = :dets_utils.free(head, pos, adjsz(oldSize))

            {head2, newPos, fPosA} =
              :dets_utils.alloc(
                head1,
                adjsz(binsSize)
              )

            head3 = one_bucket_added(head2, fPosA - 1)
            newHead = one_bucket_removed(head3, fPosF - 1)

            w0 =
              {newPos,
               [
                 <<binsSize::size(32), 305_419_896::size(32)>>
                 | bins
               ]}

            w2 = {slotPos, <<binsSize::size(32), newPos::size(32)>>}

            w1 =
              cond do
                pos !== newPos ->
                  [w0, {pos + 4, <<61_591_023::size(32)>>}]

                true ->
                  [w0]
              end

            {newHead, [w2], w1}
        end
    end
  end

  defp one_bucket_added(h, _Log2)
       when r_head(h, :no_collections) === :undefined do
    h
  end

  defp one_bucket_added(h, log2) when r_head(h, :maxobjsize) >= log2 do
    newNoColls = :orddict.update_counter(log2, 1, r_head(h, :no_collections))
    r_head(h, no_collections: newNoColls)
  end

  defp one_bucket_added(h, log2) do
    newNoColls = :orddict.update_counter(log2, 1, r_head(h, :no_collections))
    r_head(h, no_collections: newNoColls, maxobjsize: log2)
  end

  defp one_bucket_removed(h, _FPos)
       when r_head(h, :no_collections) === :undefined do
    h
  end

  defp one_bucket_removed(h, log2) when r_head(h, :maxobjsize) > log2 do
    newNoColls = :orddict.update_counter(log2, -1, r_head(h, :no_collections))
    r_head(h, no_collections: newNoColls)
  end

  defp one_bucket_removed(h, log2) when r_head(h, :maxobjsize) === log2 do
    newNoColls = :orddict.update_counter(log2, -1, r_head(h, :no_collections))
    maxObjSize = max_objsize(newNoColls)

    r_head(h,
      no_collections: newNoColls,
      maxobjsize: maxObjSize
    )
  end

  defp eval_slot(
         [{key, commands} | wLs] = wLs0,
         [{k, kS, kB, o, os} | kOs1] = kOs,
         type,
         lU,
         ws,
         no,
         kNo,
         bSz,
         ch
       ) do
    case :dets_utils.cmp(k, key) do
      0 ->
        old = [o | binobjs2terms(os)]
        {nLU, nWs, sz, no1, kNo1, nCh} = eval_key(key, commands, old, type, kB, kS, lU, ws, ch)
        eval_slot(wLs, kOs1, type, nLU, nWs, no1 + no, kNo1 + kNo, sz + bSz, nCh)

      -1 ->
        eval_slot(wLs0, kOs1, type, lU, [ws | kB], no, kNo, kS + bSz, ch)

      1 ->
        {nLU, nWs, sz, no1, kNo1, nCh} = eval_key(key, commands, [], type, [], 0, lU, ws, ch)
        eval_slot(wLs, kOs, type, nLU, nWs, no1 + no, kNo1 + kNo, sz + bSz, nCh)
    end
  end

  defp eval_slot([{key, commands} | wLs], [], type, lU, ws, no, kNo, bSz, ch) do
    {nLU, nWs, sz, no1, kNo1, nCh} = eval_key(key, commands, [], type, [], 0, lU, ws, ch)
    eval_slot(wLs, [], type, nLU, nWs, no1 + no, kNo1 + kNo, sz + bSz, nCh)
  end

  defp eval_slot([], [{_Key, size, keyBin, _, _} | kOs], type, lU, ws, no, kNo, bSz, ch) do
    eval_slot([], kOs, type, lU, [ws | keyBin], no, kNo, size + bSz, ch)
  end

  defp eval_slot([], [], _Type, lU, ws, no, kNo, bSz, ch) do
    {lU, ws, bSz, no, kNo, ch}
  end

  defp eval_key(_K, [{_Seq, {:lookup, pid}}], [], _Type, _KeyBin, _KeySz, lU, ws, ch) do
    nLU = [{pid, []} | lU]
    {nLU, ws, 0, 0, 0, ch}
  end

  defp eval_key(_K, [{_Seq, {:lookup, pid}}], old0, _Type, keyBin, keySz, lU, ws, ch) do
    old = :lists.keysort(2, old0)
    objs = get_objects(old)
    nLU = [{pid, objs} | lU]
    {nLU, [ws | keyBin], keySz, 0, 0, ch}
  end

  defp eval_key(k, comms, orig, type, keyBin, keySz, lU, ws, ch) do
    old = :dets_utils.msort(orig)

    case eval_key1(comms, [], old, type, k, lU, ws, 0, orig) do
      {:ok, nLU} when old === [] ->
        {nLU, ws, 0, 0, 0, ch}

      {:ok, nLU} ->
        {nLU, [ws | keyBin], keySz, 0, 0, ch}

      {nLU, nWs, nSz, no} when old === [] and nSz > 0 ->
        {nLU, nWs, nSz, no, 1, true}

      {nLU, nWs, nSz, no} when old !== [] and nSz === 0 ->
        {nLU, nWs, nSz, no, -1, true}

      {nLU, nWs, nSz, no} ->
        {nLU, nWs, nSz, no, 0, true}
    end
  end

  defp eval_key1(
         [{_Seq, {:insert, term}} | l],
         cs,
         [{term, _, _}] = old,
         type = :set,
         k,
         lU,
         ws,
         no,
         orig
       ) do
    eval_key1(l, cs, old, type, k, lU, ws, no, orig)
  end

  defp eval_key1([{seq, {:insert, term}} | l], cs, old, type = :set, k, lU, ws, no, orig) do
    nNo = no + 1 - length(old)
    eval_key1(l, cs, [{term, seq, :insert}], type, k, lU, ws, nNo, orig)
  end

  defp eval_key1([{_Seq, {:lookup, pid}} | l], cs, old, type, key, lU, ws, no, orig) do
    {:ok, new0, newNo} = eval_comms(cs, old, type, no)
    new = :lists.keysort(2, new0)
    objs = get_objects(new)
    nLU = [{pid, objs} | lU]

    cond do
      l === [] ->
        eval_end(new, nLU, type, ws, newNo, orig)

      true ->
        newOld = :dets_utils.msort(new)
        eval_key1(l, [], newOld, type, key, nLU, ws, newNo, orig)
    end
  end

  defp eval_key1([{_Seq, :delete_key} | l], _Cs, old, type, k, lU, ws, no, orig) do
    newNo = no - length(old)
    eval_key1(l, [], [], type, k, lU, ws, newNo, orig)
  end

  defp eval_key1(
         [{_Seq, {:delete_object, term}} | l],
         cs,
         [{term, _, _}],
         type = :set,
         k,
         lU,
         ws,
         no,
         orig
       ) do
    eval_key1(l, cs, [], type, k, lU, ws, no - 1, orig)
  end

  defp eval_key1([{_Seq, {:delete_object, _T}} | l], cs, old1, type = :set, k, lU, ws, no, orig) do
    eval_key1(l, cs, old1, type, k, lU, ws, no, orig)
  end

  defp eval_key1([{seq, {comm, term}} | l], cs, old, type, k, lU, ws, no, orig)
       when type !== :set do
    eval_key1(l, [{term, seq, comm} | cs], old, type, k, lU, ws, no, orig)
  end

  defp eval_key1([], cs, old, type = :set, _Key, lU, ws, no, orig) do
    [] = cs
    eval_end(old, lU, type, ws, no, orig)
  end

  defp eval_key1([], cs, old, type, _Key, lU, ws, no, orig) do
    {:ok, new, newNo} = eval_comms(cs, old, type, no)
    eval_end(new, lU, type, ws, newNo, orig)
  end

  defp eval_comms([], l, _Type = :set, no) do
    {:ok, l, no}
  end

  defp eval_comms(cs, old, type, no) do
    commands = :dets_utils.msort(cs)

    case type do
      :bag ->
        eval_bag(commands, old, [], no)

      :duplicate_bag ->
        eval_dupbag(commands, old, [], no)
    end
  end

  defp eval_end(new0, lU, type, ws, newNo, orig) do
    new = :lists.keysort(2, new0)

    noChange =
      cond do
        length(new) !== length(orig) ->
          false

        true ->
          same_terms(orig, new)
      end

    cond do
      noChange ->
        {:ok, lU}

      new === [] ->
        {lU, ws, 0, newNo}

      true ->
        {ws1, sz} = make_bins(new, [], 0)

        cond do
          type === :set ->
            {lU, [ws | ws1], sz, newNo}

          true ->
            nSz = sz + 4
            {lU, [ws, <<nSz::size(32)>> | ws1], nSz, newNo}
        end
    end
  end

  defp same_terms([e1 | l1], [e2 | l2])
       when :erlang.element(
              1,
              e1
            ) ===
              :erlang.element(
                1,
                e2
              ) do
    same_terms(l1, l2)
  end

  defp same_terms([], []) do
    true
  end

  defp same_terms(_L1, _L2) do
    false
  end

  defp make_bins([{_Term, _Seq, b} | l], w, sz)
       when is_binary(b) do
    make_bins(l, [w | b], sz + byte_size(b))
  end

  defp make_bins([{term, _Seq, :insert} | l], w, sz) do
    b = :erlang.term_to_binary(term)
    bSize = byte_size(b) + 4
    make_bins(l, [w, [<<bSize::size(32)>> | b]], sz + bSize)
  end

  defp make_bins([], w, sz) do
    {w, sz}
  end

  defp get_objects([{t, _S, _BT} | l]) do
    [t | get_objects(l)]
  end

  defp get_objects([]) do
    []
  end

  defp eval_bag([{term1, _S1, op} = n | l] = l0, [{term2, _, _} = o | old] = old0, new, no) do
    case {op, :dets_utils.cmp(term1, term2)} do
      {:delete_object, -1} ->
        eval_bag(l, old0, new, no)

      {:insert, -1} ->
        bag_object(l, old0, new, no, [n], term1)

      {:delete_object, 0} ->
        bag_object(l, old, new, no - 1, [], term1)

      {:insert, 0} ->
        bag_object(l, old, new, no - 1, [n], term1)

      {_, 1} ->
        eval_bag(l0, old, [o | new], no)
    end
  end

  defp eval_bag([{_Term1, _Seq1, :delete_object} | l], [] = old, new, no) do
    eval_bag(l, old, new, no)
  end

  defp eval_bag([{term, _Seq1, :insert} = n | l], [] = old, new, no) do
    bag_object(l, old, new, no, [n], term)
  end

  defp eval_bag([] = l, [o | old], new, no) do
    eval_bag(l, old, [o | new], no)
  end

  defp eval_bag([], [], new, no) do
    {:ok, new, no}
  end

  defp bag_object([{term, _, :insert} = n | l], old, new, no, _N, term) do
    bag_object(l, old, new, no, [n], term)
  end

  defp bag_object([{term, _, :delete_object} | l], old, new, no, _N, term) do
    bag_object(l, old, new, no, [], term)
  end

  defp bag_object(l, old, new, no, [], _Term) do
    eval_bag(l, old, new, no)
  end

  defp bag_object(l, old, new, no, [n], _Term) do
    eval_bag(l, old, [n | new], no + 1)
  end

  defp eval_dupbag([{term1, _S1, op} = n | l] = l0, [{term2, _, _} = o | old] = old0, new, no) do
    case {op, :dets_utils.cmp(term1, term2)} do
      {:delete_object, -1} ->
        eval_dupbag(l, old0, new, no)

      {:insert, -1} ->
        dup_object(l, old0, new, no + 1, term1, [n])

      {_, 0} ->
        old_dup_object(l0, old, new, no, term1, [o])

      {_, 1} ->
        eval_dupbag(l0, old, [o | new], no)
    end
  end

  defp eval_dupbag([{_Term1, _Seq1, :delete_object} | l], [] = old, new, no) do
    eval_dupbag(l, old, new, no)
  end

  defp eval_dupbag([{term, _Seq1, :insert} = n | l], [] = old, new, no) do
    dup_object(l, old, new, no + 1, term, [n])
  end

  defp eval_dupbag([] = l, [o | old], new, no) do
    eval_dupbag(l, old, [o | new], no)
  end

  defp eval_dupbag([], [], new, no) do
    {:ok, new, no}
  end

  defp old_dup_object(l, [{term, _, _} = obj | old], new, no, term, n) do
    old_dup_object(l, old, new, no, term, [obj | n])
  end

  defp old_dup_object(l, old, new, no, term, n) do
    dup_object(l, old, new, no, term, n)
  end

  defp dup_object([{term, _, :insert} = obj | l], old, new, no, term, q) do
    dup_object(l, old, new, no + 1, term, [obj | q])
  end

  defp dup_object([{term, _Seq, :delete_object} | l], old, new, no, term, q) do
    newNo = no - length(q)
    dup_object(l, old, new, newNo, term, [])
  end

  defp dup_object(l, old, new, no, _Term, q) do
    eval_dupbag(l, old, q ++ new, no)
  end

  defp update_no_keys(head, ws, 0, 0) do
    {head, ws}
  end

  defp update_no_keys(head, ws, deltaObjects, deltaKeys) do
    noKeys = r_head(head, :no_keys)
    newNoKeys = noKeys + deltaKeys
    newNoObject = r_head(head, :no_objects) + deltaObjects

    newHead =
      r_head(head,
        no_objects: newNoObject,
        no_keys: newNoKeys
      )

    nWs =
      cond do
        newNoKeys > r_head(newHead, :max_no_slots) ->
          ws

        div(noKeys, 256) === div(newNoKeys, 256) ->
          ws

        true ->
          [{0, file_header(newHead, 0, 0)} | ws]
      end

    {newHead, nWs}
  end

  defp slot_position(s) do
    segNo = s >>> 8
    partPos = 56 + 28 * 4 + 16 + 4 + 124 + 4 * (segNo >>> 9)
    part = get_arrpart(partPos)
    pos = part + 4 * (segNo &&& 512 - 1)
    get_segp(pos) + 4 * 2 * (s &&& 256 - 1)
  end

  defp check_pread2_arg([{_Pos, sz}], head) when sz > 10 * 8192 do
    case check_pread_arg(sz, head) do
      true ->
        :ok

      false ->
        bad = :dets_utils.bad_object(:check_pread2_arg, sz)
        throw(:dets_utils.corrupt_reason(head, bad))
    end
  end

  defp check_pread2_arg(_ToRead, _Head) do
    :ok
  end

  defp check_pread_arg(sz, head) when sz > 10 * 8192 do
    maxobjsize(head) >= sz
  end

  defp check_pread_arg(_Sz, _Head) do
    true
  end

  defp segp_cache(pos, segment) do
    :erlang.put(pos, segment)
  end

  defp get_segp(pos) do
    :erlang.get(pos)
  end

  defp arrpart_cache(pos, arrPart) do
    :erlang.put(pos, arrPart)
  end

  defp get_arrpart(pos) do
    :erlang.get(pos)
  end

  defp sz2pos(n) do
    1 + :dets_utils.log2(n)
  end

  defp adjsz(n) do
    n - 1
  end

  defp maxobjsize(head)
       when r_head(head, :maxobjsize) === :undefined do
    1 <<< 32
  end

  defp maxobjsize(head) do
    1 <<< r_head(head, :maxobjsize)
  end

  def scan_objs(head, bin, from, to, l, ts, r, type) do
    case (try do
            scan_skip(bin, from, to, l, ts, r, type, 0)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _Reason} ->
        :bad_object

      reply = {:more, _From1, _To, _L, _Ts, _R, size}
      when size > 10 * 8192 ->
        case check_pread_arg(size, head) do
          true ->
            reply

          false ->
            :bad_object
        end

      reply ->
        reply
    end
  end

  defp scan_skip(bin, from, to, l, ts, r, type, skip) do
    from1 = from + skip

    case bin do
      _ when from1 >= to ->
        cond do
          from1 > to or l === <<>> ->
            {:more, from1, to, l, ts, r, 0}

          true ->
            <<from2::size(32), to1::size(32), l1::binary>> = l
            skip1 = from2 - from
            scan_skip(bin, from, to1, l1, ts, r, type, skip1)
        end

      <<_::size(skip)-binary, _Size::size(32), st::size(32), _Sz::size(32), kO::binary>>
      when st !== 305_419_896 and st !== 61_591_023 ->
        scan_skip(kO, from1 + 12, to, l, ts, r, type, 512 * 4 - 12)

      <<_::size(skip)-binary, size::size(32), _St::size(32), sz::size(32), kO::binary>>
      when size - 12 <= byte_size(kO) ->
        bin2bins(kO, from1 + 12, to, l, ts, r, type, size, sz)

      <<_::size(skip)-binary, size::size(32), _St::size(32), _Sz::size(32), _KO::binary>> ->
        {:more, from1, to, l, ts, r, size}

      _ when skip >= 0 ->
        {:more, from1, to, l, ts, r, 0}
    end
  end

  defp bin2bins(bin, from, to, l, ts, r, type = :set, size, objSz0) do
    objsSz1 = size - objSz0

    cond do
      objsSz1 === 8 ->
        slot_end(bin, from, to, l, [bin | ts], r, type, size, 1)

      true ->
        objSz = objSz0 - 4
        <<_::size(objSz)-binary, nObjSz::size(32), t::binary>> = bin
        bins_set(t, from, to, l, [bin | ts], r, type, size, 2, nObjSz, objsSz1 - nObjSz, bin)
    end
  end

  defp bin2bins(<<objSz::size(32), bin::binary>> = kO, from, to, l, ts, r, type, size, sz) do
    bins_bag(bin, from, to, l, ts, r, type, size, 1, sz - objSz - 4, objSz - 4, size - sz, kO)
  end

  defp bins_set(bin, from, to, l, ts, r, type, size, noObjs, _ObjSz0, 8, kO) do
    slot_end(kO, from, to, l, [bin | ts], r, type, size, noObjs)
  end

  defp bins_set(bin, from, to, l, ts, r, type, size, noObjs, objSz0, objsSz, kO) do
    objSz = objSz0 - 4
    <<_::size(objSz)-binary, nObjSz::size(32), t::binary>> = bin
    bins_set(t, from, to, l, [bin | ts], r, type, size, noObjs + 1, nObjSz, objsSz - nObjSz, kO)
  end

  defp bins_bag(bin, from, to, l, ts, r, type, size, noObjs, sz, objSz, objsSz, kO)
       when sz > 0 do
    <<_::size(objSz)-binary, nObjSz::size(32), t::binary>> = bin

    bins_bag(
      t,
      from,
      to,
      l,
      [bin | ts],
      r,
      type,
      size,
      noObjs + 1,
      sz - nObjSz,
      nObjSz - 4,
      objsSz,
      kO
    )
  end

  defp bins_bag(bin, from, to, l, ts, r, type, size, noObjs, _Z, _ObjSz, 8, kO) do
    slot_end(kO, from, to, l, [bin | ts], r, type, size, noObjs)
  end

  defp bins_bag(bin, from, to, l, ts, r, type, size, noObjs, _Z, objSz, objsSz, kO) do
    <<_::size(objSz)-binary, sz::size(32), nObjSz::size(32), t::binary>> = bin

    bins_bag(
      t,
      from,
      to,
      l,
      [bin | ts],
      r,
      type,
      size,
      noObjs + 1,
      sz - nObjSz - 4,
      nObjSz - 4,
      objsSz - sz,
      kO
    )
  end

  defp slot_end(kO, from, to, l, ts, r, type, size, noObjs) do
    skip = 1 <<< (:dets_utils.log2(size) - 12)

    cond do
      r >= 0 ->
        scan_skip(kO, from, to, l, ts, r + size, type, skip)

      true ->
        case r + noObjs do
          r1 when r1 >= -1 ->
            from1 = from + skip

            bin1 =
              case kO do
                <<_::size(skip)-binary, b::binary>> ->
                  b

                _ ->
                  <<>>
              end

            {:stop, bin1, from1, to, l, ts}

          r1 ->
            scan_skip(kO, from, to, l, ts, r1, type, skip)
        end
    end
  end

  def file_info(fH) do
    r_fileheader(
      closed_properly: cP,
      keypos: kp,
      m: m,
      next: next,
      n: n,
      version: version,
      type: type,
      no_objects: noObjects,
      no_keys: noKeys
    ) = fH

    cond do
      cP === 0 ->
        {:error, :not_closed}

      r_fileheader(fH, :cookie) !== 11_259_375 ->
        {:error, :not_a_dets_file}

      r_fileheader(fH, :version) !== 9 ->
        {:error, :bad_version}

      true ->
        {:ok,
         [
           {:closed_properly, cP},
           {:keypos, kp},
           {:m, m},
           {:n, n},
           {:next, next},
           {:no_objects, noObjects},
           {:no_keys, noKeys},
           {:type, type},
           {:version, version}
         ]}
    end
  end

  def v_segments(r_head() = h) do
    v_parts(h, 0, 0)
  end

  defp v_parts(_H, 256, _SegNo) do
    :done
  end

  defp v_parts(h, partNo, segNo) do
    fd = r_head(h, :fptr)

    partPos =
      :dets_utils.read_4(
        fd,
        56 + 28 * 4 + 16 + 4 + 124 + 4 * partNo
      )

    cond do
      partPos === 0 ->
        :done

      true ->
        partBin = :dets_utils.pread_n(fd, partPos, 512 * 4)
        v_segments(h, partBin, partNo + 1, segNo)
    end
  end

  defp v_segments(h, <<>>, partNo, segNo) do
    v_parts(h, partNo, segNo)
  end

  defp v_segments(_H, <<0::size(32), _::binary>>, _PartNo, _SegNo) do
    :done
  end

  defp v_segments(h, <<seg::size(32), t::binary>>, partNo, segNo) do
    :io.format('<~w>SEGMENT ~w~n', [seg, segNo])
    v_segment(h, segNo, seg, 0)
    v_segments(h, t, partNo, segNo + 1)
  end

  defp v_segment(_H, _, _SegPos, 256) do
    :done
  end

  defp v_segment(h, segNo, segPos, segSlot) do
    slot = segSlot + segNo * 256
    bucketP = segPos + 4 * 2 * segSlot

    case (try do
            read_bucket(h, bucketP, r_head(h, :type))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, reason} ->
        :dets_utils.vformat('** dets: Corrupt or truncated dets file~n', [])
        :io.format('~nERROR ~tp~n', [reason])

      [] ->
        true

      {size, collP, objects} ->
        :io.format('   <~w>~w: <~w:~p>~w~n', [bucketP, slot, collP, size, objects])
    end

    v_segment(h, segNo, segPos, segSlot + 1)
  end

  defp read_bucket(head, position, type) do
    maxSize = maxobjsize(head)

    case :dets_utils.ipread(head, position, maxSize) do
      {:ok, {size, pointer, <<^size::size(32), _Status::size(32), keysObjs::binary>>}} ->
        objs = bin2objs(keysObjs, type, [])
        {size, pointer, :lists.reverse(objs)}

      [] ->
        []
    end
  end

  defp per_key(
         head,
         <<binSize::size(32), 305_419_896::size(32), bin::binary>> = b
       ) do
    true = byte_size(b) === binSize

    cond do
      r_head(head, :type) === :set ->
        per_set_key(bin, r_head(head, :keypos), [])

      true ->
        per_bag_key(bin, r_head(head, :keypos), [])
    end
  end

  defp per_set_key(<<size::size(32), t::binary>> = b, keyPos, l) do
    <<keyBin::size(size)-binary, r::binary>> = b
    term = :erlang.binary_to_term(t)
    key = :erlang.element(keyPos, term)
    item = {term, -(1 <<< 26), keyBin}
    per_set_key(r, keyPos, [{key, size, keyBin, item, []} | l])
  end

  defp per_set_key(<<>>, keyPos, l) when is_integer(keyPos) do
    :lists.reverse(l)
  end

  defp per_bag_key(<<size::size(32), objSz::size(32), t::binary>> = b, keyPos, l) do
    <<keyBin::size(size)-binary, r::binary>> = b
    objSz1 = objSz - 4
    size1 = size - objSz - 4
    <<_::size(objSz1)-binary, keyObjs::size(size1)-binary, _::binary>> = t
    <<_Size::size(32), bin::size(objSz)-binary, _::binary>> = b
    term = :erlang.binary_to_term(t)
    key = :erlang.element(keyPos, term)
    item = {term, -(1 <<< 26), bin}
    per_bag_key(r, keyPos, [{key, size, keyBin, item, keyObjs} | l])
  end

  defp per_bag_key(<<>>, keyPos, l) when is_integer(keyPos) do
    :lists.reverse(l)
  end

  defp binobjs2terms(<<objSz::size(32), t::binary>> = b) do
    binobjs2terms(b, t, objSz, byte_size(b) - objSz, -(1 <<< 26) + 1, [])
  end

  defp binobjs2terms([] = b) do
    b
  end

  defp binobjs2terms(<<>>) do
    []
  end

  defp binobjs2terms(bin, obj, _ObjSz, _Size = 0, n, l) do
    :lists.reverse(
      l,
      [{:erlang.binary_to_term(obj), n, bin}]
    )
  end

  defp binobjs2terms(bin, bin1, objSz, size, n, l) do
    <<b::size(objSz)-binary, t::binary>> = bin
    <<nObjSz::size(32), t1::binary>> = t
    item = {:erlang.binary_to_term(bin1), n, b}
    binobjs2terms(t, t1, nObjSz, size - nObjSz, n + 1, [item | l])
  end

  defp bin2objs(keysObjs, :set, ts) do
    <<objSz::size(32), t::binary>> = keysObjs
    bin2objs(t, objSz - 4, byte_size(keysObjs) - objSz, ts)
  end

  defp bin2objs(keysObjs, _Type, ts) do
    bin2objs2(keysObjs, ts)
  end

  defp bin2objs2(
         <<size::size(32), objSz::size(32), t::binary>>,
         ts
       ) do
    bin2objs(t, objSz - 4, size - objSz - 4, ts)
  end

  defp bin2objs2(<<>>, ts) do
    ts
  end

  defp bin2objs(bin, objSz, _Size = 0, ts) do
    <<_::size(objSz)-binary, t::binary>> = bin
    bin2objs2(t, [:erlang.binary_to_term(bin) | ts])
  end

  defp bin2objs(bin, objSz, size, ts) do
    <<_::size(objSz)-binary, nObjSz::size(32), t::binary>> = bin
    bin2objs(t, nObjSz - 4, size - nObjSz, [:erlang.binary_to_term(bin) | ts])
  end

  defp bin2keybins(keysObjs, head) when r_head(head, :type) === :set do
    <<objSz::size(32), t::binary>> = keysObjs
    bin2keybins(t, r_head(head, :keypos), objSz - 4, byte_size(keysObjs) - objSz, [])
  end

  defp bin2keybins(keysObjs, head) do
    bin2keybins2(keysObjs, r_head(head, :keypos), [])
  end

  defp bin2keybins2(<<size::size(32), objSz::size(32), t::binary>>, kp, l) do
    bin2keybins(t, kp, objSz - 4, size - objSz - 4, l)
  end

  defp bin2keybins2(<<>>, kp, l) when is_integer(kp) do
    :lists.reverse(l)
  end

  defp bin2keybins(bin, kp, objSz, _Size = 0, l) do
    <<obj::size(objSz)-binary, t::binary>> = bin
    term = :erlang.binary_to_term(obj)
    bin2keybins2(t, kp, [{:erlang.element(kp, term), obj} | l])
  end

  defp bin2keybins(bin, kp, objSz, size, l) do
    <<obj::size(objSz)-binary, nObjSz::size(32), t::binary>> = bin
    term = :erlang.binary_to_term(obj)
    bin2keybins(t, kp, nObjSz - 4, size - nObjSz, [{:erlang.element(kp, term), obj} | l])
  end
end
