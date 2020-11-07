defmodule :m_dets_utils do
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

  def cmp(t, t) do
    0
  end

  def cmp([e1 | t1], [e2 | t2]) do
    case cmp(e1, e2) do
      0 ->
        cmp(t1, t2)

      r ->
        r
    end
  end

  def cmp(t1, t2) when tuple_size(t1) === tuple_size(t2) do
    tcmp(t1, t2, 1, tuple_size(t1))
  end

  def cmp(i, f) when is_integer(i) and is_float(f) do
    -1
  end

  def cmp(f, i) when is_float(f) and is_integer(i) do
    1
  end

  def cmp(t1, t2) when t1 < t2 do
    -1
  end

  def cmp(_T1, _T2) do
    1
  end

  defp tcmp(t1, t2, i, i) do
    cmp(:erlang.element(i, t1), :erlang.element(i, t2))
  end

  defp tcmp(t1, t2, i, n) do
    case cmp(
           :erlang.element(i, t1),
           :erlang.element(i, t2)
         ) do
      0 ->
        tcmp(t1, t2, i + 1, n)

      r ->
        r
    end
  end

  def msort(l) do
    f = fn x, y ->
      cmp(x, y) <= 0
    end

    :lists.sort(f, :lists.sort(l))
  end

  def mkeysort(i, l) do
    f = fn x, y ->
      cmp(:erlang.element(i, x), :erlang.element(i, y)) <= 0
    end

    :lists.sort(f, :lists.keysort(i, l))
  end

  def mkeysearch(key, i, l) do
    case :lists.keysearch(key, i, l) do
      {:value, value} = reply
      when :erlang.element(
             i,
             value
           ) === key ->
        reply

      false ->
        false

      _ ->
        mkeysearch2(key, i, l)
    end
  end

  defp mkeysearch2(_Key, _I, []) do
    false
  end

  defp mkeysearch2(key, i, [e | _L])
       when :erlang.element(
              i,
              e
            ) === key do
    {:value, e}
  end

  defp mkeysearch2(key, i, [_ | l]) do
    mkeysearch2(key, i, l)
  end

  def family([]) do
    []
  end

  def family(l) do
    [{k, v} | kVL] = mkeysort(1, l)
    per_key(kVL, k, [v], [])
  end

  defp per_key([], k, vs, kVs) do
    :lists.reverse(kVs, [{k, msort(vs)}])
  end

  defp per_key([{k, v} | l], k, vs, kVs) do
    per_key(l, k, [v | vs], kVs)
  end

  defp per_key([{k1, v} | l], k, vs, kVs) do
    per_key(l, k1, [v], [{k, msort(vs)} | kVs])
  end

  def rename(from, to) do
    case :file.rename(from, to) do
      :ok ->
        :ok

      {:error, reason} ->
        {:error, {:file_error, {from, to}, reason}}
    end
  end

  def pread(positions, head) do
    r =
      case :file.pread(r_head(head, :fptr), positions) do
        {:ok, bins} ->
          case :lists.member(:eof, bins) do
            true ->
              {:error, {:premature_eof, r_head(head, :filename)}}

            false ->
              {:ok, bins}
          end

        {:error, reason}
        when :enomem === reason or
               :einval === reason ->
          {:error, {:bad_object_header, r_head(head, :filename)}}

        {:error, reason} ->
          {:file_error, r_head(head, :filename), reason}
      end

    case r do
      {:ok, _Bins} ->
        r

      error ->
        throw(corrupt(head, error))
    end
  end

  def pread(head, pos, min, extra) do
    r =
      case :file.pread(r_head(head, :fptr), pos, min + extra) do
        {:error, reason}
        when :enomem === reason or
               :einval === reason ->
          {:error, {:bad_object_header, r_head(head, :filename)}}

        {:error, reason} ->
          {:file_error, r_head(head, :filename), reason}

        {:ok, bin} when byte_size(bin) < min ->
          {:error, {:premature_eof, r_head(head, :filename)}}

        oK ->
          oK
      end

    case r do
      {:ok, _Bin} ->
        r

      error ->
        throw(corrupt(head, error))
    end
  end

  def ipread(head, pos1, maxSize) do
    try do
      disk_map_pread(pos1)
    catch
      bad ->
        throw(corrupt_reason(head, {:disk_map, bad}))
    end

    case :file.ipread_s32bu_p32bu(r_head(head, :fptr), pos1, maxSize) do
      {:ok, {0, 0, :eof}} ->
        []

      {:ok, reply} ->
        {:ok, reply}

      _Else ->
        :eof
    end
  end

  def pwrite(head, []) do
    {head, :ok}
  end

  def pwrite(head, bins) do
    try do
      disk_map(bins)
    catch
      bad ->
        throw(corrupt_reason(head, {:disk_map, bad, bins}))
    end

    case :file.pwrite(r_head(head, :fptr), bins) do
      :ok ->
        {head, :ok}

      error ->
        corrupt_file(head, error)
    end
  end

  def write(_Head, []) do
    :ok
  end

  def write(head, bins) do
    case :file.write(r_head(head, :fptr), bins) do
      :ok ->
        :ok

      error ->
        corrupt_file(head, error)
    end
  end

  def write_file(head, bin) do
    r =
      case :file.open(
             r_head(head, :filename),
             [:binary, :raw, :write]
           ) do
        {:ok, fd} ->
          r1 = :file.write(fd, bin)
          r2 = :file.sync(fd)
          r3 = :file.close(fd)

          case {r1, r2, r3} do
            {:ok, :ok, ^r3} ->
              r3

            {:ok, ^r2, _} ->
              r2

            {^r1, _, _} ->
              r1
          end

        else__ ->
          else__
      end

    case r do
      :ok ->
        :ok

      error ->
        corrupt_file(head, error)
    end
  end

  def truncate(head, pos) do
    case (try do
            truncate(r_head(head, :fptr), r_head(head, :filename), pos)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :ok ->
        :ok

      error ->
        throw(corrupt(head, error))
    end
  end

  def position(head, pos) do
    case :file.position(r_head(head, :fptr), pos) do
      {:error, _Reason} = error ->
        corrupt_file(head, error)

      oK ->
        oK
    end
  end

  def sync(head) do
    case :file.sync(r_head(head, :fptr)) do
      :ok ->
        :ok

      error ->
        corrupt_file(head, error)
    end
  end

  def open(fileSpec, args) do
    case :file.open(fileSpec, args) do
      {:ok, fd} ->
        {:ok, fd}

      error ->
        file_error(fileSpec, error)
    end
  end

  def truncate(fd, fileName, pos) do
    _ =
      for _ <- [:EFE_DUMMY_GEN], pos !== :cur do
        position(fd, fileName, pos)
      end

    case :file.truncate(fd) do
      :ok ->
        :ok

      error ->
        file_error(fileName, {:error, error})
    end
  end

  def fwrite(fd, fileName, b) do
    case :file.write(fd, b) do
      :ok ->
        :ok

      error ->
        file_error_close(fd, fileName, error)
    end
  end

  def position(fd, fileName, pos) do
    case :file.position(fd, pos) do
      {:error, error} ->
        file_error(fileName, {:error, error})

      oK ->
        oK
    end
  end

  def position_close(fd, fileName, pos) do
    case :file.position(fd, pos) do
      {:error, error} ->
        file_error_close(fd, fileName, {:error, error})

      oK ->
        oK
    end
  end

  def pwrite(fd, fileName, bins) do
    case :file.pwrite(fd, bins) do
      :ok ->
        :ok

      {:error, {_NoWrites, reason}} ->
        file_error(fileName, {:error, reason})
    end
  end

  def pread_close(fd, fileName, pos, size) do
    case :file.pread(fd, pos, size) do
      {:error, error} ->
        file_error_close(fd, fileName, {:error, error})

      {:ok, bin} when byte_size(bin) < size ->
        _ = :file.close(fd)
        throw({:error, {:tooshort, fileName}})

      :eof ->
        _ = :file.close(fd)
        throw({:error, {:tooshort, fileName}})

      oK ->
        oK
    end
  end

  def file_error(fileName, {:error, reason}) do
    throw({:error, {:file_error, fileName, reason}})
  end

  defp file_error_close(fd, fileName, {:error, reason}) do
    _ = :file.close(fd)
    throw({:error, {:file_error, fileName, reason}})
  end

  def debug_mode() do
    :os.getenv('DETS_DEBUG') === 'true'
  end

  def bad_object(where, extra) do
    case debug_mode() do
      true ->
        {:bad_object, where, extra}

      false ->
        {:bad_object, where}
    end
  end

  def read_n(fd, max) do
    case :file.read(fd, max) do
      {:ok, bin} ->
        bin

      _Else ->
        :eof
    end
  end

  def pread_n(fd, position, max) do
    case :file.pread(fd, position, max) do
      {:ok, bin} ->
        bin

      _ ->
        :eof
    end
  end

  def read_4(fd, position) do
    {:ok, _} = :file.position(fd, position)
    <<four::size(32)>> = :dets_utils.read_n(fd, 4)
    four
  end

  def corrupt_file(head, {:error, reason}) do
    error = {:error, {:file_error, r_head(head, :filename), reason}}
    throw(corrupt(head, error))
  end

  def corrupt_reason(head, reason0) do
    reason =
      case get_disk_map() do
        :no_disk_map ->
          reason0

        dM ->
          {:current_stacktrace, sT} =
            :erlang.process_info(
              self(),
              :current_stacktrace
            )

          pD = :erlang.get()
          {reason0, sT, pD, dM}
      end

    error = {:error, {reason, r_head(head, :filename)}}
    corrupt(head, error)
  end

  def corrupt(head, error) do
    case :erlang.get(:verbose) do
      :yes ->
        :error_logger.format('** dets: Corrupt table ~tp: ~tp\n', [r_head(head, :name), error])

      _ ->
        :ok
    end

    case r_head(head, :update_mode) do
      {:error, _} ->
        {head, error}

      _ ->
        {r_head(head, update_mode: error), error}
    end
  end

  def vformat(f, as) do
    case :erlang.get(:verbose) do
      :yes ->
        :error_logger.format(f, as)

      _ ->
        :ok
    end
  end

  def code_to_type(1) do
    :set
  end

  def code_to_type(2) do
    :bag
  end

  def code_to_type(3) do
    :duplicate_bag
  end

  def code_to_type(_Type) do
    :badtype
  end

  def type_to_code(:set) do
    1
  end

  def type_to_code(:bag) do
    2
  end

  def type_to_code(:duplicate_bag) do
    3
  end

  def cache_size(c) do
    {r_cache(c, :delay), r_cache(c, :tsize)}
  end

  def cache_lookup(type, [key | keys], cL, lU) do
    case mkeysearch(key, 1, cL) do
      {:value, {^key, {_Seq, {:insert, object}}}}
      when type === :set ->
        cache_lookup(type, keys, cL, [object | lU])

      {:value, {^key, {_Seq, :delete_key}}} ->
        cache_lookup(type, keys, cL, lU)

      _ ->
        false
    end
  end

  def cache_lookup(_Type, [], _CL, lU) do
    lU
  end

  def reset_cache(c) do
    wrTime = r_cache(c, :wrtime)

    newWrTime =
      cond do
        wrTime === :undefined ->
          wrTime

        true ->
          :erlang.monotonic_time(1_000_000)
      end

    pK = family(r_cache(c, :cache))
    newC = r_cache(c, cache: [], csize: 0, inserts: 0, wrtime: newWrTime)
    {newC, r_cache(c, :inserts), pK}
  end

  def is_empty_cache(cache) do
    r_cache(cache, :cache) === []
  end

  def new_cache({delay, size}) do
    r_cache(cache: [], csize: 0, inserts: 0, tsize: size, wrtime: :undefined, delay: delay)
  end

  def init_alloc(base) do
    ftab = empty_free_lists()
    empty = bplus_empty_tree()
    :erlang.setelement(32, ftab, bplus_insert(empty, base))
  end

  def empty_free_lists() do
    empty = bplus_empty_tree()
    :erlang.make_tuple(32, empty)
  end

  def alloc_many(head, _Sz, 0, _A0) do
    head
  end

  def alloc_many(head, sz, n, a0) do
    ftab = r_head(head, :freelists)
    r_head(head, freelists: alloc_many1(ftab, 1, sz * n, a0, head))
  end

  defp alloc_many1(ftab, pos, size, a0, h) do
    {fPos, addr} = find_first_free(ftab, pos, pos, h)
    true = addr >= a0

    cond do
      1 <<< (fPos - 1) >= size ->
        alloc_many2(ftab, sz2pos(size), size, a0, h)

      true ->
        newFtab = reserve_buddy(ftab, fPos, fPos, addr)
        nSize = size - (1 <<< (fPos - 1))
        alloc_many1(newFtab, fPos, nSize, addr, h)
    end
  end

  defp alloc_many2(ftab, _Pos, 0, _A0, _H) do
    ftab
  end

  defp alloc_many2(ftab, pos, size, a0, h)
       when size &&& 1 <<< (pos - 1) > 0 do
    {fPos, addr} = find_first_free(ftab, pos, pos, h)
    true = addr >= a0
    newFtab = reserve_buddy(ftab, fPos, pos, addr)
    nSize = size - (1 <<< (pos - 1))
    alloc_many2(newFtab, pos - 1, nSize, addr, h)
  end

  defp alloc_many2(ftab, pos, size, a0, h) do
    alloc_many2(ftab, pos - 1, size, a0, h)
  end

  def alloc(head, sz) when r_head(head, :fixed) !== false do
    true
    pos = sz2pos(sz)
    {frozen, ftab} = r_head(head, :freelists)
    {fPos, addr} = find_first_free(frozen, pos, pos, head)
    newFrozen = reserve_buddy(frozen, fPos, pos, addr)
    ftab1 = undo_free(ftab, fPos, addr, r_head(head, :base))
    newFtab = move_down(ftab1, fPos, pos, addr)
    newFreelists = {newFrozen, newFtab}
    {r_head(head, freelists: newFreelists), addr, pos}
  end

  def alloc(head, sz) when r_head(head, :fixed) === false do
    true
    pos = sz2pos(sz)
    ftab = r_head(head, :freelists)
    {fPos, addr} = find_first_free(ftab, pos, pos, head)
    newFtab = reserve_buddy(ftab, fPos, pos, addr)
    {r_head(head, freelists: newFtab), addr, pos}
  end

  defp find_first_free(_Ftab, pos, _Pos0, head) when pos > 32 do
    throw({:error, {:no_more_space_on_file, r_head(head, :filename)}})
  end

  defp find_first_free(ftab, pos, pos0, head) do
    posTab = :erlang.element(pos, ftab)

    case bplus_lookup_first(posTab) do
      :undefined ->
        find_first_free(ftab, pos + 1, pos0, head)

      {:ok, addr}
      when addr + (1 <<< (pos0 - 1)) > 1 <<< (32 - 1 - 50_000_000) ->
        throw({:error, {:no_more_space_on_file, r_head(head, :filename)}})

      {:ok, addr} ->
        {pos, addr}
    end
  end

  defp undo_free(ftab, pos, addr, base) do
    posTab = :erlang.element(pos, ftab)

    case bplus_lookup(posTab, addr) do
      :undefined ->
        {buddyAddr, moveUpAddr} = my_buddy(addr, 1 <<< (pos - 1), base)
        newFtab = :erlang.setelement(pos, ftab, bplus_insert(posTab, buddyAddr))
        undo_free(newFtab, pos + 1, moveUpAddr, base)

      {:ok, ^addr} ->
        newPosTab = bplus_delete(posTab, addr)
        :erlang.setelement(pos, ftab, newPosTab)
    end
  end

  defp reserve_buddy(ftab, pos, pos0, addr) do
    posTab = :erlang.element(pos, ftab)
    newPosTab = bplus_delete(posTab, addr)
    newFtab = :erlang.setelement(pos, ftab, newPosTab)
    move_down(newFtab, pos, pos0, addr)
  end

  defp move_down(ftab, pos, pos, _Addr) do
    true
    ftab
  end

  defp move_down(ftab, pos, pos0, addr) do
    pos_1 = pos - 1
    size = 1 <<< pos_1
    highBuddy = addr + (size >>> 1)

    newPosTab_1 =
      bplus_insert(
        :erlang.element(pos_1, ftab),
        highBuddy
      )

    newFtab = :erlang.setelement(pos_1, ftab, newPosTab_1)
    move_down(newFtab, pos_1, pos0, addr)
  end

  def free(head, addr, sz) do
    true
    ftab = get_freelists(head)
    pos = sz2pos(sz)

    {set_freelists(
       head,
       free_in_pos(ftab, addr, pos, r_head(head, :base))
     ), pos}
  end

  defp free_in_pos(ftab, _Addr, pos, _Base) when pos > 32 do
    ftab
  end

  defp free_in_pos(ftab, addr, pos, base) do
    posTab = :erlang.element(pos, ftab)
    {buddyAddr, moveUpAddr} = my_buddy(addr, 1 <<< (pos - 1), base)

    case bplus_lookup(posTab, buddyAddr) do
      :undefined ->
        true
        :erlang.setelement(pos, ftab, bplus_insert(posTab, addr))

      {:ok, ^buddyAddr} ->
        posTab1 = bplus_delete(posTab, addr)
        posTab2 = bplus_delete(posTab1, buddyAddr)
        true
        newFtab = :erlang.setelement(pos, ftab, posTab2)
        free_in_pos(newFtab, moveUpAddr, pos + 1, base)
    end
  end

  def get_freelists(head) when r_head(head, :fixed) === false do
    r_head(head, :freelists)
  end

  def get_freelists(head) when r_head(head, :fixed) !== false do
    {_Frozen, current} = r_head(head, :freelists)
    current
  end

  defp set_freelists(head, ftab) when r_head(head, :fixed) === false do
    r_head(head, freelists: ftab)
  end

  defp set_freelists(head, ftab) when r_head(head, :fixed) !== false do
    {frozen, _} = r_head(head, :freelists)
    r_head(head, freelists: {frozen, ftab})
  end

  defp sz2pos(n) when n > 0 do
    1 + log2(n + 1)
  end

  def log2(n) when is_integer(n) and n >= 0 do
    cond do
      n > 1 <<< 8 ->
        cond do
          n > 1 <<< 10 ->
            cond do
              n > 1 <<< 11 ->
                cond do
                  n > 1 <<< 12 ->
                    12 +
                      cond do
                        n &&& 1 <<< (12 - 1) === 0 ->
                          log2(n >>> 12)

                        true ->
                          log2(1 + (n >>> 12))
                      end

                  true ->
                    12
                end

              true ->
                11
            end

          n > 1 <<< 9 ->
            10

          true ->
            9
        end

      n > 1 <<< 4 ->
        cond do
          n > 1 <<< 6 ->
            cond do
              n > 1 <<< 7 ->
                8

              true ->
                7
            end

          n > 1 <<< 5 ->
            6

          true ->
            5
        end

      n > 1 <<< 2 ->
        cond do
          n > 1 <<< 3 ->
            4

          true ->
            3
        end

      n > 1 <<< 1 ->
        2

      n >= 1 <<< 0 ->
        1

      true ->
        0
    end
  end

  def make_zeros(0) do
    []
  end

  def make_zeros(n) when rem(n, 2) === 0 do
    p = make_zeros(div(n, 2))
    [p | p]
  end

  def make_zeros(n) do
    p = make_zeros(div(n, 2))
    [[0, p] | p]
  end

  defp my_buddy(addr, sz, base) do
    case addr - base &&& sz do
      0 ->
        {addr + sz, addr}

      _ ->
        t = addr - sz
        {t, t}
    end
  end

  def all_free(head) do
    tab = get_freelists(head)
    base = r_head(head, :base)

    case all_free(all(tab), base, base, []) do
      [{^base, ^base} | l] ->
        l

      l ->
        l
    end
  end

  defp all_free([], x0, y0, f) do
    :lists.reverse([{x0, y0} | f])
  end

  defp all_free([{x, y} | l], x0, y0, f) when y0 === x do
    all_free(l, x0, y, f)
  end

  defp all_free([{x, y} | l], x0, y0, f) when y0 < x do
    all_free(l, x, y, [{x0, y0} | f])
  end

  def all_allocated(head) do
    all_allocated(all(get_freelists(head)), 0, r_head(head, :base), [])
  end

  defp all_allocated([], _X0, _Y0, []) do
    <<>>
  end

  defp all_allocated([], _X0, _Y0, a0) do
    [
      <<from::size(32), to::size(32)>>
      | a
    ] = :lists.reverse(a0)

    {from, to, :erlang.list_to_binary(a)}
  end

  defp all_allocated([{x, y} | l], x0, y0, a) when y0 === x do
    all_allocated(l, x0, y, a)
  end

  defp all_allocated([{x, y} | l], _X0, y0, a) when y0 < x do
    all_allocated(l, x, y, [<<y0::size(32), x::size(32)>> | a])
  end

  def all_allocated_as_list(head) do
    all_allocated_as_list(all(get_freelists(head)), 0, r_head(head, :base), [])
  end

  defp all_allocated_as_list([], _X0, _Y0, []) do
    []
  end

  defp all_allocated_as_list([], _X0, _Y0, a) do
    :lists.reverse(a)
  end

  defp all_allocated_as_list([{x, y} | l], x0, y0, a) when y0 === x do
    all_allocated_as_list(l, x0, y, a)
  end

  defp all_allocated_as_list([{x, y} | l], _X0, y0, a) when y0 < x do
    all_allocated_as_list(l, x, y, [[y0 | x] | a])
  end

  defp all(tab) do
    all(tab, tuple_size(tab), [])
  end

  defp all(_Tab, 0, l) do
    :lists.sort(l)
  end

  defp all(tab, i, l) do
    lL = collect_tree(:erlang.element(i, tab), i, l)
    all(tab, i - 1, lL)
  end

  def find_allocated(ftab, addr, length, base) do
    maxAddr = addr + length
    ints = collect_all_interval(ftab, addr, maxAddr, base)
    allocated(ints, addr, maxAddr, ftab, base)
  end

  defp allocated(some, addr, max, ftab, base) do
    case allocated1(some, addr, max, []) do
      [] ->
        case find_next_allocated(ftab, addr, base) do
          {from, _} ->
            find_allocated(ftab, from, 8192, base)

          :none ->
            <<>>
        end

      l ->
        :erlang.list_to_binary(:lists.reverse(l))
    end
  end

  defp allocated1([], y0, max, a) when y0 < max do
    [<<y0::size(32), max::size(32)>> | a]
  end

  defp allocated1([], _Y0, _Max, a) do
    a
  end

  defp allocated1([{x, y} | l], y0, max, a) when y0 >= x do
    allocated1(l, y, max, a)
  end

  defp allocated1([{x, y} | l], y0, max, a) do
    allocated1(l, y, max, [<<y0::size(32), x::size(32)>> | a])
  end

  def find_next_allocated(ftab, addr, base) do
    case find_next_free(ftab, addr, base) do
      :none ->
        :none

      {addr1, pos} when addr1 <= addr ->
        find_next_allocated(ftab, addr1 + (1 <<< (pos - 1)), base)

      {next, _Pos} ->
        {addr, next}
    end
  end

  defp find_next_free(ftab, addr, base) do
    maxBud = tuple_size(ftab)
    find_next_free(ftab, addr, 1, maxBud, -1, -1, base)
  end

  defp find_next_free(ftab, addr0, pos, maxBud, next, posN, base)
       when pos <= maxBud do
    addr = adjust_addr(addr0, pos, base)
    posTab = :erlang.element(pos, ftab)

    case bplus_lookup_next(posTab, addr - 1) do
      :undefined ->
        find_next_free(ftab, addr0, pos + 1, maxBud, next, posN, base)

      {:ok, next1} when posN === -1 or next1 < next ->
        find_next_free(ftab, addr0, pos + 1, maxBud, next1, pos, base)

      {:ok, _} ->
        find_next_free(ftab, addr0, pos + 1, maxBud, next, posN, base)
    end
  end

  defp find_next_free(_Ftab, _Addr, _Pos, _MaxBud, -1, _PosN, _Base) do
    :none
  end

  defp find_next_free(_Ftab, _Addr, _Pos, _MaxBud, next, posN, _Base) do
    {next, posN}
  end

  defp collect_all_interval(ftab, addr, maxAddr, base) do
    maxBud = tuple_size(ftab)
    collect_all_interval(ftab, addr, maxAddr, 1, maxBud, base, [])
  end

  defp collect_all_interval(ftab, l0, u, pos, maxBud, base, acc0)
       when pos <= maxBud do
    posTab = :erlang.element(pos, ftab)
    l = adjust_addr(l0, pos, base)
    acc = collect_interval(posTab, pos, l, u, acc0)
    collect_all_interval(ftab, l0, u, pos + 1, maxBud, base, acc)
  end

  defp collect_all_interval(_Ftab, _L, _U, _Pos, _MaxBud, _Base, acc) do
    :lists.sort(acc)
  end

  defp adjust_addr(addr, pos, base) do
    pow = 1 <<< (pos - 1)
    rem = rem(addr - base, pow)

    cond do
      rem === 0 ->
        addr

      addr < pow ->
        addr

      true ->
        addr - rem
    end
  end

  defp get_disk_map() do
    case :erlang.get(:disk_map) do
      :undefined ->
        :no_disk_map

      t ->
        {:disk_map, :ets.tab2list(t)}
    end
  end

  def init_disk_map(name) do
    :error_logger.info_msg('** dets: (debug) using disk map for ~p~n', [name])
    :erlang.put(:disk_map, :ets.new(:any, [:ordered_set]))
  end

  def stop_disk_map() do
    try do
      :ets.delete(:erlang.erase(:disk_map))
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  def disk_map_segment_p(fd, p) do
    case :erlang.get(:disk_map) do
      :undefined ->
        :ok

      _T ->
        disk_map_segment(p, pread_n(fd, p, 8 * 256))
    end
  end

  def disk_map_segment(p, segment) do
    case :erlang.get(:disk_map) do
      :undefined ->
        :ok

      t ->
        ps =
          segment_fragment_to_pointers(
            p,
            :erlang.iolist_to_binary(segment)
          )

        ss =
          for {_P1, <<sz::size(32), x::size(32)>>} <- ps,
              x > 0 do
            {x, <<sz::size(32), 305_419_896::size(32)>>}
          end

        dm(ps ++ ss, t)
    end
  end

  defp disk_map_pread(p) do
    case :erlang.get(:disk_map) do
      :undefined ->
        :ok

      t ->
        case :ets.lookup(t, p) do
          [] ->
            throw({:pread, p, 8})

          [{^p, {:pointer, 0, 0}}] ->
            :ok

          [{^p, {:pointer, pointer, sz}}] ->
            case :ets.lookup(t, pointer) do
              [{^pointer, {:slot, _P, ^sz}}] ->
                :ok

              got ->
                throw({:pread, p, pointer, got})
            end

          got ->
            throw({:pread, p, got})
        end
    end
  end

  defp disk_map(bins) do
    case :erlang.get(:disk_map) do
      :undefined ->
        :ok

      t ->
        bs =
          for {p, io} <- bins do
            {p, :erlang.iolist_to_binary(io)}
          end

        dm(bs, t)
    end
  end

  defp dm([{p, _Header} | bs], t) when p < 1336 do
    dm(bs, t)
  end

  defp dm([{p0, <<61_591_023::size(32)>>} | bs], t) do
    p = p0 - 4

    case :ets.lookup(t, p) do
      [] ->
        throw({:free, p0})

      [{^p, _OldSz}] ->
        true = :ets.delete(t, p)
    end

    dm(bs, t)
  end

  defp dm(
         [
           {slotP, <<sz::size(32), 305_419_896::size(32), _::binary>>}
           | bs
         ],
         t
       ) do
    ptr =
      case :ets.lookup(t, {:pointer, slotP}) do
        [{{:pointer, ^slotP}, pointer}] ->
          case :ets.lookup(t, pointer) do
            [{^pointer, {:pointer, ^slotP, sz2}}] ->
              case log2(sz) === log2(sz2) do
                true ->
                  pointer

                false ->
                  throw({:active, slotP, sz, pointer, sz2})
              end

            got ->
              throw({:active, slotP, sz, got})
          end

        [] ->
          throw({:active, slotP, sz})
      end

    true = :ets.insert(t, {slotP, {:slot, ptr, sz}})
    dm(bs, t)
  end

  defp dm(
         [{p, <<sz::size(32), x::size(32)>>} | bs],
         t
       ) do
    case prev(p, t) do
      {prev, prevSz} ->
        throw({:prev, p, sz, x, prev, prevSz})

      :ok ->
        :ok
    end

    case next(p, 8, t) do
      {:next, next} ->
        throw({:next, p, sz, x, next})

      :ok ->
        :ok
    end

    true = :ets.insert(t, {p, {:pointer, x, sz}})

    cond do
      sz === 0 ->
        ^x = 0
        true

      true ->
        true = :ets.insert(t, {{:pointer, x}, p})
    end

    dm(bs, t)
  end

  defp dm([{p, <<x::size(32)>>} | bs], t) do
    case :ets.lookup(t, x) do
      [] ->
        throw({:segment, p, x})

      [{^x, {:pointer, 0, 0}}] ->
        :ok

      [{^x, {:pointer, ^p, ^x}}] ->
        :ok
    end

    dm(bs, t)
  end

  defp dm(
         [
           {p, <<_Sz::size(32), b0::binary>> = b}
           | bs
         ],
         t
       ) do
    overwrite =
      case (try do
              :erlang.binary_to_term(b0)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:EXIT, _} ->
          <<_Sz1::size(32), b1::binary>> = b0

          case (try do
                  :erlang.binary_to_term(b1)
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end) do
            {:EXIT, _} ->
              false

            _ ->
              true
          end

        _ ->
          true
      end

    cond do
      overwrite ->
        dm(
          [
            {p - 8, <<byte_size(b) + 8::size(32), 305_419_896::size(32), b::binary>>}
            | bs
          ],
          t
        )

      true ->
        dm(segment_fragment_to_pointers(p, b) ++ bs, t)
    end
  end

  defp dm([], _T) do
    :ok
  end

  defp segment_fragment_to_pointers(_P, <<>>) do
    []
  end

  defp segment_fragment_to_pointers(p, <<szP::size(8)-binary, b::binary>>) do
    [{p, szP} | segment_fragment_to_pointers(p + 8, b)]
  end

  defp prev(p, t) do
    case :ets.prev(t, p) do
      :"$end_of_table" ->
        :ok

      prev ->
        case :ets.lookup(t, prev) do
          [{^prev, {:pointer, _Ptr, _}}] when prev + 8 > p ->
            {prev, 8}

          [{^prev, {:slot, _, sz}}] when prev + sz > p ->
            {prev, sz}

          _ ->
            :ok
        end
    end
  end

  defp next(p, pSz, t) do
    case :ets.next(t, p) do
      :"$end_of_table" ->
        :ok

      next when p + pSz > next ->
        {:next, next}

      _ ->
        :ok
    end
  end

  defp collect_tree(:v, _TI, acc) do
    acc
  end

  defp collect_tree(t, tI, acc) do
    pow = 1 <<< (tI - 1)
    collect_tree2(t, pow, acc)
  end

  defp collect_tree2(tree, pow, acc) do
    s = bplus_get_size(tree)

    case :erlang.element(1, tree) do
      :l ->
        collect_leaf(tree, s, pow, acc)

      :n ->
        collect_node(tree, s, pow, acc)
    end
  end

  defp collect_leaf(_Leaf, 0, _Pow, acc) do
    acc
  end

  defp collect_leaf(leaf, i, pow, acc) do
    key = :erlang.element(i + 1, leaf)
    v = {key, key + pow}
    collect_leaf(leaf, i - 1, pow, [v | acc])
  end

  defp collect_node(_Node, 0, _Pow, acc) do
    acc
  end

  defp collect_node(node, i, pow, acc) do
    acc1 = collect_tree2(bplus_get_tree(node, i), pow, acc)
    collect_node(node, i - 1, pow, acc1)
  end

  def tree_to_bin(:v, _F, _Max, ws, wsSz) do
    {ws, wsSz}
  end

  def tree_to_bin(t, f, max, ws, wsSz) do
    {n, l1, ws1, wsSz1} = tree_to_bin2(t, f, max, 0, [], ws, wsSz)
    {n1, l2, ws2, wsSz2} = f.(n, :lists.reverse(l1), ws1, wsSz1)
    {0, [], nWs, nWsSz} = f.(n1, l2, ws2, wsSz2)
    {nWs, nWsSz}
  end

  defp tree_to_bin2(tree, f, max, n, acc, ws, wsSz) when n >= max do
    {nN, nAcc, nWs, nWsSz} = f.(n, :lists.reverse(acc), ws, wsSz)
    tree_to_bin2(tree, f, max, nN, :lists.reverse(nAcc), nWs, nWsSz)
  end

  defp tree_to_bin2(tree, f, max, n, acc, ws, wsSz) do
    s = bplus_get_size(tree)

    case :erlang.element(1, tree) do
      :l ->
        {n + s, leaf_to_bin(bplus_leaf_to_list(tree), acc), ws, wsSz}

      :n ->
        node_to_bin(tree, f, max, n, acc, 1, s, ws, wsSz)
    end
  end

  defp node_to_bin(_Node, _F, _Max, n, acc, i, s, ws, wsSz)
       when i > s do
    {n, acc, ws, wsSz}
  end

  defp node_to_bin(node, f, max, n, acc, i, s, ws, wsSz) do
    {n1, acc1, ws1, wsSz1} = tree_to_bin2(bplus_get_tree(node, i), f, max, n, acc, ws, wsSz)
    node_to_bin(node, f, max, n1, acc1, i + 1, s, ws1, wsSz1)
  end

  defp leaf_to_bin([n | l], acc) do
    leaf_to_bin(l, [<<n::size(32)>> | acc])
  end

  defp leaf_to_bin([], acc) do
    acc
  end

  def list_to_tree(l) do
    leafs_to_nodes(l, length(l), &bplus_mk_leaf/1, [])
  end

  defp leafs_to_nodes([], 0, _F, [t]) do
    t
  end

  defp leafs_to_nodes([], 0, _F, l) do
    leafs_to_nodes(:lists.reverse(l), length(l), &mk_node/1, [])
  end

  defp leafs_to_nodes(ls, sz, f, l) do
    i =
      cond do
        sz <= 16 ->
          sz

        sz <= 32 ->
          div(sz, 2)

        true ->
          12
      end

    {l1, r} = split_list(ls, i, [])
    n = f.(l1)
    sz1 = sz - i
    leafs_to_nodes(r, sz1, f, [n | l])
  end

  defp mk_node([e | es]) do
    nL = [
      e
      | :lists.foldr(
          fn x, a ->
            [[get_first_key(x), x] | a]
          end,
          [],
          es
        )
    ]

    bplus_mk_node(nL)
  end

  defp split_list(l, 0, sL) do
    {sL, l}
  end

  defp split_list([e | es], i, sL) do
    split_list(es, i - 1, [e | sL])
  end

  defp get_first_key(t) do
    case :erlang.element(1, t) do
      :l ->
        :erlang.element(1 + 1, t)

      :n ->
        get_first_key(bplus_get_tree(t, 1))
    end
  end

  defp collect_interval(:v, _TI, _L, _U, acc) do
    acc
  end

  defp collect_interval(t, tI, l, u, acc) do
    pow = 1 <<< (tI - 1)
    collect_interval2(t, pow, l, u, acc)
  end

  defp collect_interval2(tree, pow, l, u, acc) do
    s = bplus_get_size(tree)

    case :erlang.element(1, tree) do
      :l ->
        collect_leaf_interval(tree, s, pow, l, u, acc)

      :n ->
        {max, _} = bplus_select_sub_tree(tree, u)
        {min, _} = bplus_select_sub_tree_2(tree, l, max)
        collect_node_interval(tree, min, max, pow, l, u, acc)
    end
  end

  defp collect_leaf_interval(_Leaf, 0, _Pow, _L, _U, acc) do
    acc
  end

  defp collect_leaf_interval(leaf, i, pow, l, u, acc) do
    key = :erlang.element(i + 1, leaf)

    cond do
      key < l ->
        acc

      key > u ->
        collect_leaf_interval(leaf, i - 1, pow, l, u, acc)

      true ->
        collect_leaf_interval(leaf, i - 1, pow, l, u, [{key, key + pow} | acc])
    end
  end

  defp collect_node_interval(_Node, i, uP, _Pow, _L, _U, acc) when i > uP do
    acc
  end

  defp collect_node_interval(node, i, uP, pow, l, u, acc) do
    acc1 = collect_interval2(bplus_get_tree(node, i), pow, l, u, acc)
    collect_node_interval(node, i + 1, uP, pow, l, u, acc1)
  end

  defp bplus_empty_tree() do
    :v
  end

  defp bplus_lookup(:v, _Key) do
    :undefined
  end

  defp bplus_lookup(tree, key) do
    case :erlang.element(1, tree) do
      :l ->
        bplus_lookup_leaf(key, tree)

      :n ->
        {_, subTree} = bplus_select_sub_tree(tree, key)
        bplus_lookup(subTree, key)
    end
  end

  defp bplus_lookup_leaf(key, leaf) do
    bplus_lookup_leaf_2(key, leaf, bplus_get_size(leaf))
  end

  defp bplus_lookup_leaf_2(_, _, 0) do
    :undefined
  end

  defp bplus_lookup_leaf_2(key, leaf, n) do
    case :erlang.element(n + 1, leaf) do
      ^key ->
        {:ok, key}

      _ ->
        bplus_lookup_leaf_2(key, leaf, n - 1)
    end
  end

  defp bplus_lookup_first(:v) do
    :undefined
  end

  defp bplus_lookup_first(tree) do
    case :erlang.element(1, tree) do
      :l ->
        {:ok, :erlang.element(1 + 1, tree)}

      :n ->
        bplus_lookup_first(bplus_get_tree(tree, 1))
    end
  end

  defp bplus_lookup_next(:v, _) do
    :undefined
  end

  defp bplus_lookup_next(tree, key) do
    case :erlang.element(1, tree) do
      :l ->
        lookup_next_leaf(key, tree)

      :n ->
        {pos, subTree} = bplus_select_sub_tree(tree, key)

        case bplus_lookup_next(subTree, key) do
          :undefined ->
            s = bplus_get_size(tree)

            cond do
              s > pos ->
                bplus_lookup_first(bplus_get_tree(tree, pos + 1))

              true ->
                :undefined
            end

          result ->
            result
        end
    end
  end

  defp lookup_next_leaf(key, leaf) do
    lookup_next_leaf_2(key, leaf, bplus_get_size(leaf), 1)
  end

  defp lookup_next_leaf_2(key, leaf, size, size) do
    k = :erlang.element(size + 1, leaf)

    cond do
      k > key ->
        {:ok, :erlang.element(size + 1, leaf)}

      true ->
        :undefined
    end
  end

  defp lookup_next_leaf_2(key, leaf, size, n) do
    k = :erlang.element(n + 1, leaf)

    cond do
      k < key ->
        lookup_next_leaf_2(key, leaf, size, n + 1)

      key == k ->
        {:ok, :erlang.element(n + 1 + 1, leaf)}

      true ->
        {:ok, :erlang.element(n + 1, leaf)}
    end
  end

  defp bplus_insert(:v, key) do
    bplus_mk_leaf([key])
  end

  defp bplus_insert(tree, key) do
    newTree = bplus_insert_in(tree, key)

    case bplus_get_size(newTree) > 16 do
      false ->
        newTree

      true ->
        {lTree, dKey, rTree} =
          case :erlang.element(
                 1,
                 newTree
               ) do
            :l ->
              bplus_split_leaf(newTree)

            :n ->
              bplus_split_node(newTree)
          end

        bplus_mk_node([lTree, dKey, rTree])
    end
  end

  defp bplus_delete(:v, _Key) do
    :v
  end

  defp bplus_delete(tree, key) do
    newTree = bplus_delete_in(tree, key)
    s = bplus_get_size(newTree)

    case :erlang.element(1, newTree) do
      :l ->
        cond do
          s === 0 ->
            :v

          true ->
            newTree
        end

      :n ->
        cond do
          s === 1 ->
            bplus_get_tree(newTree, 1)

          true ->
            newTree
        end
    end
  end

  defp bplus_insert_in(tree, key) do
    case :erlang.element(1, tree) do
      :l ->
        bplus_insert_in_leaf(tree, key)

      :n ->
        {pos, subTree} = bplus_select_sub_tree(tree, key)
        newSubTree = bplus_insert_in(subTree, key)

        case bplus_get_size(newSubTree) > 16 do
          false ->
            bplus_put_subtree(tree, [newSubTree, pos])

          true ->
            case bplus_reorganize_tree_ins(tree, newSubTree, pos) do
              {:left, {leftT, dKey, middleT}} ->
                bplus_put_subtree(
                  bplus_put_lkey(tree, dKey, pos),
                  [leftT, pos - 1, middleT, pos]
                )

              {:right, {middleT, dKey, rightT}} ->
                bplus_put_subtree(
                  bplus_put_rkey(tree, dKey, pos),
                  [middleT, pos, rightT, pos + 1]
                )

              {:split, {leftT, dKey, rightT}} ->
                bplus_extend_tree(tree, {leftT, dKey, rightT}, pos)
            end
        end
    end
  end

  defp bplus_insert_in_leaf(leaf, key) do
    bplus_insert_in_leaf_2(leaf, key, bplus_get_size(leaf), [])
  end

  defp bplus_insert_in_leaf_2(leaf, key, 0, accum) do
    bplus_insert_in_leaf_3(leaf, 0, [key | accum])
  end

  defp bplus_insert_in_leaf_2(leaf, key, n, accum) do
    k = :erlang.element(n + 1, leaf)

    cond do
      key < k ->
        bplus_insert_in_leaf_2(leaf, key, n - 1, [k | accum])

      k < key ->
        bplus_insert_in_leaf_3(leaf, n - 1, [[k, key] | accum])

      k == key ->
        bplus_insert_in_leaf_3(leaf, n - 1, [key | accum])
    end
  end

  defp bplus_insert_in_leaf_3(_Leaf, 0, leafList) do
    bplus_mk_leaf(leafList)
  end

  defp bplus_insert_in_leaf_3(leaf, n, leafList) do
    bplus_insert_in_leaf_3(leaf, n - 1, [:erlang.element(n + 1, leaf) | leafList])
  end

  defp bplus_delete_in(tree, key) do
    case :erlang.element(1, tree) do
      :l ->
        bplus_delete_in_leaf(tree, key)

      :n ->
        {pos, subTree} = bplus_select_sub_tree(tree, key)
        newSubTree = bplus_delete_in(subTree, key)

        case bplus_get_size(newSubTree) < 8 do
          false ->
            bplus_put_subtree(tree, [newSubTree, pos])

          true ->
            case bplus_reorganize_tree_del(tree, newSubTree, pos) do
              {:left, {leftT, dKey, middleT}} ->
                bplus_put_subtree(
                  bplus_put_lkey(tree, dKey, pos),
                  [leftT, pos - 1, middleT, pos]
                )

              {:right, {middleT, dKey, rightT}} ->
                bplus_put_subtree(
                  bplus_put_rkey(tree, dKey, pos),
                  [middleT, pos, rightT, pos + 1]
                )

              {:join_left, joinedTree} ->
                bplus_joinleft_tree(tree, joinedTree, pos)

              {:join_right, joinedTree} ->
                bplus_joinright_tree(tree, joinedTree, pos)
            end
        end
    end
  end

  defp bplus_delete_in_leaf(leaf, key) do
    bplus_delete_in_leaf_2(leaf, key, bplus_get_size(leaf), [])
  end

  defp bplus_delete_in_leaf_2(leaf, _, 0, _) do
    leaf
  end

  defp bplus_delete_in_leaf_2(leaf, key, n, accum) do
    k = :erlang.element(n + 1, leaf)

    cond do
      key == k ->
        bplus_delete_in_leaf_3(leaf, n - 1, accum)

      true ->
        bplus_delete_in_leaf_2(leaf, key, n - 1, [k | accum])
    end
  end

  defp bplus_delete_in_leaf_3(_Leaf, 0, leafList) do
    bplus_mk_leaf(leafList)
  end

  defp bplus_delete_in_leaf_3(leaf, n, leafList) do
    bplus_delete_in_leaf_3(leaf, n - 1, [:erlang.element(n + 1, leaf) | leafList])
  end

  defp bplus_select_sub_tree(tree, key) do
    bplus_select_sub_tree_2(tree, key, bplus_get_size(tree))
  end

  defp bplus_select_sub_tree_2(tree, _Key, 1) do
    {1, bplus_get_tree(tree, 1)}
  end

  defp bplus_select_sub_tree_2(tree, key, n) do
    k = bplus_get_lkey(tree, n)

    cond do
      k > key ->
        bplus_select_sub_tree_2(tree, key, n - 1)

      k <= key ->
        {n, bplus_get_tree(tree, n)}
    end
  end

  defp bplus_reorganize_tree_ins(tree, newSubTree, 1) do
    rTree = bplus_get_tree(tree, 2)

    case bplus_get_size(rTree) >= 16 do
      false ->
        bplus_reorganize_tree_r(tree, newSubTree, 1, rTree)

      true ->
        bplus_reorganize_tree_s(newSubTree)
    end
  end

  defp bplus_reorganize_tree_ins(tree, newSubTree, pos) do
    size = bplus_get_size(tree)

    cond do
      pos == size ->
        lTree = bplus_get_tree(tree, pos - 1)

        case bplus_get_size(lTree) >= 16 do
          false ->
            bplus_reorganize_tree_l(tree, newSubTree, pos, lTree)

          true ->
            bplus_reorganize_tree_s(newSubTree)
        end

      true ->
        lTree = bplus_get_tree(tree, pos - 1)
        rTree = bplus_get_tree(tree, pos + 1)
        sL = bplus_get_size(lTree)
        sR = bplus_get_size(rTree)

        cond do
          sL > sR ->
            bplus_reorganize_tree_r(tree, newSubTree, pos, rTree)

          sL < sR ->
            bplus_reorganize_tree_l(tree, newSubTree, pos, lTree)

          true ->
            case bplus_get_size(lTree) >= 16 do
              false ->
                bplus_reorganize_tree_l(tree, newSubTree, pos, lTree)

              true ->
                bplus_reorganize_tree_s(newSubTree)
            end
        end
    end
  end

  defp bplus_reorganize_tree_del(tree, newSubTree, 1) do
    rTree = bplus_get_tree(tree, 2)

    case bplus_get_size(rTree) <= 8 do
      false ->
        bplus_reorganize_tree_r(tree, newSubTree, 1, rTree)

      true ->
        bplus_reorganize_tree_jr(tree, newSubTree, 1, rTree)
    end
  end

  defp bplus_reorganize_tree_del(tree, newSubTree, pos) do
    size = bplus_get_size(tree)

    cond do
      pos == size ->
        lTree = bplus_get_tree(tree, pos - 1)

        case bplus_get_size(lTree) <= 8 do
          false ->
            bplus_reorganize_tree_l(tree, newSubTree, pos, lTree)

          true ->
            bplus_reorganize_tree_jl(tree, newSubTree, pos, lTree)
        end

      true ->
        lTree = bplus_get_tree(tree, pos - 1)
        rTree = bplus_get_tree(tree, pos + 1)
        sL = bplus_get_size(lTree)
        sR = bplus_get_size(rTree)

        cond do
          sL > sR ->
            bplus_reorganize_tree_l(tree, newSubTree, pos, lTree)

          sL < sR ->
            bplus_reorganize_tree_r(tree, newSubTree, pos, rTree)

          true ->
            case bplus_get_size(lTree) <= 8 do
              false ->
                bplus_reorganize_tree_l(tree, newSubTree, pos, lTree)

              true ->
                bplus_reorganize_tree_jl(tree, newSubTree, pos, lTree)
            end
        end
    end
  end

  defp bplus_reorganize_tree_l(tree, newSubTree, pos, lTree) do
    case :erlang.element(1, newSubTree) do
      :l ->
        {:left,
         bplus_split_leaf(
           bplus_mk_leaf(
             :lists.append(
               bplus_leaf_to_list(lTree),
               bplus_leaf_to_list(newSubTree)
             )
           )
         )}

      :n ->
        {:left,
         bplus_split_node(
           bplus_mk_node(
             :lists.append([
               bplus_node_to_list(lTree),
               [
                 bplus_get_lkey(
                   tree,
                   pos
                 )
               ],
               bplus_node_to_list(newSubTree)
             ])
           )
         )}
    end
  end

  defp bplus_reorganize_tree_r(tree, newSubTree, pos, rTree) do
    case :erlang.element(1, newSubTree) do
      :l ->
        {:right,
         bplus_split_leaf(
           bplus_mk_leaf(
             :lists.append([bplus_leaf_to_list(newSubTree), bplus_leaf_to_list(rTree)])
           )
         )}

      :n ->
        {:right,
         bplus_split_node(
           bplus_mk_node(
             :lists.append([
               bplus_node_to_list(newSubTree),
               [
                 bplus_get_rkey(
                   tree,
                   pos
                 )
               ],
               bplus_node_to_list(rTree)
             ])
           )
         )}
    end
  end

  defp bplus_reorganize_tree_s(newSubTree) do
    case :erlang.element(1, newSubTree) do
      :l ->
        {:split, bplus_split_leaf(newSubTree)}

      :n ->
        {:split, bplus_split_node(newSubTree)}
    end
  end

  defp bplus_reorganize_tree_jl(tree, newSubTree, pos, lTree) do
    case :erlang.element(1, newSubTree) do
      :l ->
        {:join_left,
         bplus_mk_leaf(:lists.append([bplus_leaf_to_list(lTree), bplus_leaf_to_list(newSubTree)]))}

      :n ->
        {:join_left,
         bplus_mk_node(
           :lists.append([
             bplus_node_to_list(lTree),
             [bplus_get_lkey(tree, pos)],
             bplus_node_to_list(newSubTree)
           ])
         )}
    end
  end

  defp bplus_reorganize_tree_jr(tree, newSubTree, pos, rTree) do
    case :erlang.element(1, newSubTree) do
      :l ->
        {:join_right,
         bplus_mk_leaf(:lists.append([bplus_leaf_to_list(newSubTree), bplus_leaf_to_list(rTree)]))}

      :n ->
        {:join_right,
         bplus_mk_node(
           :lists.append([
             bplus_node_to_list(newSubTree),
             [bplus_get_rkey(tree, pos)],
             bplus_node_to_list(rTree)
           ])
         )}
    end
  end

  defp bplus_split_leaf(leaf) do
    s = bplus_get_size(leaf)
    bplus_split_leaf_2(leaf, s, div(s, 2), [])
  end

  defp bplus_split_leaf_2(leaf, pos, 1, accum) do
    k = :erlang.element(pos + 1, leaf)
    bplus_split_leaf_3(leaf, pos - 1, [], k, [k | accum])
  end

  defp bplus_split_leaf_2(leaf, pos, n, accum) do
    bplus_split_leaf_2(leaf, pos - 1, n - 1, [:erlang.element(pos + 1, leaf) | accum])
  end

  defp bplus_split_leaf_3(_, 0, leftAcc, dKey, rightAcc) do
    {bplus_mk_leaf(leftAcc), dKey, bplus_mk_leaf(rightAcc)}
  end

  defp bplus_split_leaf_3(leaf, pos, leftAcc, dKey, rightAcc) do
    bplus_split_leaf_3(leaf, pos - 1, [:erlang.element(pos + 1, leaf) | leftAcc], dKey, rightAcc)
  end

  defp bplus_split_node(node) do
    s = bplus_get_size(node)
    bplus_split_node_2(node, s, div(s, 2), [])
  end

  defp bplus_split_node_2(node, pos, 1, accum) do
    bplus_split_node_3(node, pos - 1, [], bplus_get_lkey(node, pos), [
      bplus_get_tree(node, pos) | accum
    ])
  end

  defp bplus_split_node_2(node, pos, n, accum) do
    bplus_split_node_2(node, pos - 1, n - 1, [
      [bplus_get_lkey(node, pos), bplus_get_tree(node, pos)]
      | accum
    ])
  end

  defp bplus_split_node_3(node, 1, leftAcc, dKey, rightAcc) do
    {bplus_mk_node([bplus_get_tree(node, 1) | leftAcc]), dKey, bplus_mk_node(rightAcc)}
  end

  defp bplus_split_node_3(node, pos, leftAcc, dKey, rightAcc) do
    bplus_split_node_3(
      node,
      pos - 1,
      [
        [bplus_get_lkey(node, pos), bplus_get_tree(node, pos)]
        | leftAcc
      ],
      dKey,
      rightAcc
    )
  end

  defp bplus_joinleft_tree(tree, joinedTree, pos) do
    bplus_join_tree_2(tree, joinedTree, pos, bplus_get_size(tree), [])
  end

  defp bplus_joinright_tree(tree, joinedTree, pos) do
    bplus_join_tree_2(tree, joinedTree, pos + 1, bplus_get_size(tree), [])
  end

  defp bplus_join_tree_2(tree, joinedTree, pos, pos, accum) do
    bplus_join_tree_3(tree, pos - 2, [joinedTree | accum])
  end

  defp bplus_join_tree_2(tree, joinedTree, pos, n, accum) do
    bplus_join_tree_2(tree, joinedTree, pos, n - 1, [
      [bplus_get_lkey(tree, n), bplus_get_tree(tree, n)]
      | accum
    ])
  end

  defp bplus_join_tree_3(_Tree, 0, accum) do
    bplus_mk_node(accum)
  end

  defp bplus_join_tree_3(tree, pos, accum) do
    bplus_join_tree_3(tree, pos - 1, [
      [bplus_get_tree(tree, pos), bplus_get_rkey(tree, pos)]
      | accum
    ])
  end

  defp bplus_mk_node(nodeList) do
    :erlang.list_to_tuple([:n | nodeList])
  end

  defp bplus_node_to_list(node) do
    [_ | nodeList] = :erlang.tuple_to_list(node)
    nodeList
  end

  defp bplus_mk_leaf(keyList) do
    :erlang.list_to_tuple([:l | keyList])
  end

  defp bplus_leaf_to_list(leaf) do
    [_ | leafList] = :erlang.tuple_to_list(leaf)
    leafList
  end

  defp bplus_put_subtree(tree, []) do
    tree
  end

  defp bplus_put_subtree(tree, [[newSubTree, pos] | rest]) do
    bplus_put_subtree(
      :erlang.setelement(pos * 2, tree, newSubTree),
      rest
    )
  end

  defp bplus_extend_tree(tree, inserts, pos) do
    bplus_extend_tree_2(tree, inserts, pos, bplus_get_size(tree), [])
  end

  defp bplus_extend_tree_2(tree, {t1, dKey, t2}, pos, pos, accum) do
    bplus_extend_tree_3(tree, pos - 1, [[t1, dKey, t2] | accum])
  end

  defp bplus_extend_tree_2(tree, inserts, pos, n, accum) do
    bplus_extend_tree_2(tree, inserts, pos, n - 1, [
      [bplus_get_lkey(tree, n), bplus_get_tree(tree, n)]
      | accum
    ])
  end

  defp bplus_extend_tree_3(_, 0, accum) do
    bplus_mk_node(accum)
  end

  defp bplus_extend_tree_3(tree, n, accum) do
    bplus_extend_tree_3(tree, n - 1, [
      [bplus_get_tree(tree, n), bplus_get_rkey(tree, n)]
      | accum
    ])
  end

  defp bplus_put_lkey(tree, dKey, pos) do
    :erlang.setelement(pos * 2 - 1, tree, dKey)
  end

  defp bplus_put_rkey(tree, dKey, pos) do
    :erlang.setelement(pos * 2 + 1, tree, dKey)
  end

  defp bplus_get_size(tree) do
    case :erlang.element(1, tree) do
      :l ->
        tuple_size(tree) - 1

      :n ->
        div(tuple_size(tree), 2)
    end
  end

  defp bplus_get_tree(tree, pos) do
    :erlang.element(pos * 2, tree)
  end

  defp bplus_get_lkey(tree, pos) do
    :erlang.element(pos * 2 - 1, tree)
  end

  defp bplus_get_rkey(tree, pos) do
    :erlang.element(pos * 2 + 1, tree)
  end
end
