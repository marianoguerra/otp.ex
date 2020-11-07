defmodule :m_erts_debug do
  use Bitwise

  def breakpoint(_, _) do
    :erlang.nif_error(:undef)
  end

  def disassemble(_) do
    :erlang.nif_error(:undef)
  end

  def display(_) do
    :erlang.nif_error(:undef)
  end

  def dist_ext_to_term(_, _) do
    :erlang.nif_error(:undef)
  end

  def flat_size(_) do
    :erlang.nif_error(:undef)
  end

  def size_shared(_) do
    :erlang.nif_error(:undef)
  end

  def copy_shared(_) do
    :erlang.nif_error(:undef)
  end

  def get_internal_state(_) do
    :erlang.nif_error(:undef)
  end

  def instructions() do
    :erlang.nif_error(:undef)
  end

  def interpreter_size() do
    :erlang.nif_error(:undef)
  end

  def ic(f) when is_function(f) do
    is0 = :erlang.system_info(:instruction_counts)
    r = f.()
    is1 = :erlang.system_info(:instruction_counts)

    is =
      :lists.keysort(
        2,
        for {{i, c1}, {i, c0}} <- :lists.zip(is1, is0) do
          {i, c1 - c0}
        end
      )

    _ =
      for {i, c} <- is do
        :io.format('~12w ~w~n', [c, i])
      end

    :io.format(
      'Total: ~w~n',
      [
        :lists.sum(
          for {_I, c} <- is do
            c
          end
        )
      ]
    )

    r
  end

  def lcnt_control(_Option, _Value) do
    :erlang.nif_error(:undef)
  end

  def lcnt_control(_Option) do
    :erlang.nif_error(:undef)
  end

  def lcnt_collect() do
    :erlang.nif_error(:undef)
  end

  def lcnt_clear() do
    :erlang.nif_error(:undef)
  end

  def same(_, _) do
    :erlang.nif_error(:undef)
  end

  def set_internal_state(_, _) do
    :erlang.nif_error(:undef)
  end

  def dirty_cpu(_, _) do
    :erlang.nif_error(:undef)
  end

  def dirty_io(_, _) do
    :erlang.nif_error(:undef)
  end

  def dirty(_, _, _) do
    :erlang.nif_error(:undef)
  end

  require Record

  Record.defrecord(:r_s, :s,
    seen: :undefined,
    maps: :undefined
  )

  def size(term) do
    {sum, _} = size(term, r_s(seen: :gb_trees.empty(), maps: []), 0)
    sum
  end

  defp size([h | t] = term, seen0, sum0) do
    case remember_term(term, seen0) do
      :seen ->
        {sum0, seen0}

      seen1 ->
        {sum, seen} = size(h, seen1, sum0 + 2)
        size(t, seen, sum)
    end
  end

  defp size(tuple, seen0, sum0) when is_tuple(tuple) do
    case remember_term(tuple, seen0) do
      :seen ->
        {sum0, seen0}

      seen ->
        sum = sum0 + 1 + tuple_size(tuple)
        tuple_size(1, tuple_size(tuple), tuple, seen, sum)
    end
  end

  defp size(map, seen0, sum) when is_map(map) do
    case remember_term(map, seen0) do
      :seen ->
        {sum, seen0}

      seen ->
        map_size(map, seen, sum)
    end
  end

  defp size(fun, seen0, sum) when is_function(fun) do
    case remember_term(fun, seen0) do
      :seen ->
        {sum, seen0}

      seen ->
        fun_size(fun, seen, sum)
    end
  end

  defp size(term, seen0, sum) do
    case :erts_debug.flat_size(term) do
      0 ->
        {sum, seen0}

      sz ->
        case remember_term(term, seen0) do
          :seen ->
            {sum, seen0}

          seen ->
            {sum + sz, seen}
        end
    end
  end

  defp tuple_size(i, sz, _, seen, sum) when i > sz do
    {sum, seen}
  end

  defp tuple_size(i, sz, tuple, seen0, sum0) do
    {sum, seen} = size(:erlang.element(i, tuple), seen0, sum0)
    tuple_size(i + 1, sz, tuple, seen, sum)
  end

  defp map_size(map, seen0, sum0) do
    case :erts_internal.term_type(map) do
      :flatmap ->
        kt = :erts_internal.map_to_tuple_keys(map)
        vs = :maps.values(map)
        {sum1, seen1} = size(kt, seen0, sum0)
        fold_size(vs, seen1, sum1 + length(vs) + 3)

      :hashmap ->
        cs = :erts_internal.map_hashmap_children(map)
        fold_size(cs, seen0, sum0 + length(cs) + 2)

      :hashmap_node ->
        cs = :erts_internal.map_hashmap_children(map)
        fold_size(cs, seen0, sum0 + length(cs) + 1)
    end
  end

  defp fun_size(fun, seen, sum) do
    case :erlang.fun_info(fun, :type) do
      {:type, :external} ->
        {sum + :erts_debug.flat_size(fun), seen}

      {:type, :local} ->
        sz =
          :erts_debug.flat_size(fn ->
            :ok
          end)

        {:env, env} = :erlang.fun_info(fun, :env)
        fold_size(env, seen, sum + sz + length(env))
    end
  end

  defp fold_size([h | t], seen0, sum0) do
    {sum, seen} = size(h, seen0, sum0)
    fold_size(t, seen, sum)
  end

  defp fold_size([], seen, sum) do
    {sum, seen}
  end

  defp remember_term(term, r_s(maps: ms) = s) when is_map(term) do
    case is_term_seen(term, ms) do
      false ->
        r_s(s, maps: [term | ms])

      true ->
        :seen
    end
  end

  defp remember_term(term, r_s(seen: t) = s) do
    case :gb_trees.lookup(term, t) do
      :none ->
        r_s(s, seen: :gb_trees.insert(term, [term], t))

      {:value, terms} ->
        case is_term_seen(term, terms) do
          false ->
            r_s(s, seen: :gb_trees.update(term, [term | terms], t))

          true ->
            :seen
        end
    end
  end

  defp is_term_seen(term, [h | t]) do
    case :erts_debug.same(term, h) do
      true ->
        true

      false ->
        is_term_seen(term, t)
    end
  end

  defp is_term_seen(_, []) do
    false
  end

  def df(mod) when is_atom(mod) do
    try do
      mod.module_info(:functions)
    catch
      _, _ ->
        {:undef, mod}
    else
      fs0 when is_list(fs0) ->
        name = :lists.concat([mod, '.dis'])

        fs =
          for {func, arity} <- fs0 do
            {mod, func, arity}
          end

        dff(name, fs)
    end
  end

  def df(mod, func)
      when is_atom(mod) and
             is_atom(func) do
    try do
      mod.module_info(:functions)
    catch
      _, _ ->
        {:undef, mod}
    else
      fs0 when is_list(fs0) ->
        name = :lists.concat([mod, '_', func, '.dis'])

        fs =
          for {func1, arity} <- fs0, func1 === func do
            {mod, func1, arity}
          end

        dff(name, fs)
    end
  end

  def df(mod, func, arity)
      when is_atom(mod) and
             is_atom(func) do
    try do
      mod.module_info(:functions)
    catch
      _, _ ->
        {:undef, mod}
    else
      fs0 when is_list(fs0) ->
        name = :lists.concat([mod, '_', func, '_', arity, '.dis'])

        fs =
          for {func1, arity1} <- fs0, func1 === func, arity1 === arity do
            {mod, func1, arity1}
          end

        dff(name, fs)
    end
  end

  def dis_to_file(mod, name) when is_atom(mod) do
    try do
      mod.module_info(:functions)
    catch
      _, _ ->
        {:undef, mod}
    else
      fs0 when is_list(fs0) ->
        fs =
          for {func, arity} <- fs0 do
            {mod, func, arity}
          end

        dff(name, fs)
    end
  end

  defp dff(name, fs) do
    case :file.open(
           name,
           [:write, :raw, :delayed_write]
         ) do
      {:ok, f} ->
        try do
          dff_1(f, fs)
        after
          _ = :file.close(f)
        end

      {:error, reason} ->
        {:error, {:badopen, reason}}
    end
  end

  defp dff_1(file, fs) do
    :lists.foreach(
      fn mfa ->
        disassemble_function(file, mfa)
        :file.write(file, '\n')
      end,
      fs
    )
  end

  defp disassemble_function(file, {_, _, _} = mFA) do
    cont_dis(file, :erts_debug.disassemble(mFA), mFA)
  end

  defp cont_dis(_, false, _) do
    :ok
  end

  defp cont_dis(file, {addr, str, mFA}, mFA) do
    :ok = :file.write(file, str)
    cont_dis(file, :erts_debug.disassemble(addr), mFA)
  end

  defp cont_dis(_, {_, _, _}, _) do
    :ok
  end

  def map_info(_) do
    :erlang.nif_error(:undef)
  end

  def lc_graph() do
    :erts_debug.set_internal_state(
      :available_internal_state,
      true
    )

    :erts_debug.get_internal_state(:lc_graph)
  end

  def lc_graph_to_dot(outFile, inFile) do
    {:ok, [lL0]} = :file.consult(inFile)
    [{'NO LOCK', 0} | lL] = lL0

    map =
      :maps.from_list(
        for {name, id, _, _} <- lL do
          {id, name}
        end
      )

    case :file.open(outFile, [:exclusive]) do
      {:ok, out} ->
        :ok = :file.write(out, 'digraph G {\n')

        for lck <- lL do
          dot_print_lock(out, lck, map)
        end

        :ok = :file.write(out, '}\n')
        :ok = :file.close(out)

      {:error, :eexist} ->
        {'File already exists', outFile}
    end
  end

  defp dot_print_lock(out, {_Name, id, lst, _}, map) do
    for from <- lst do
      dot_print_edge(out, from, id, map)
    end

    :ok
  end

  defp dot_print_edge(_, 0, _, _) do
    :ignore
  end

  defp dot_print_edge(out, from, to, map) do
    :io.format(out, '~p -> ~p;\n', [:maps.get(from, map), :maps.get(to, map)])
  end

  def lc_graph_merge(outFile, inFiles) do
    lLs =
      :lists.map(
        fn inFile ->
          {:ok, [lL]} = :file.consult(inFile)
          lL
        end,
        inFiles
      )

    res =
      :lists.foldl(
        fn a, b ->
          lcg_merge(a, b)
        end,
        hd(lLs),
        tl(lLs)
      )

    case :file.open(outFile, [:exclusive]) do
      {:ok, out} ->
        try do
          lcg_print(out, res)
        after
          :file.close(out)
        end

        :ok

      {:error, :eexist} ->
        {'File already exists', outFile}
    end
  end

  defp lcg_merge(a, b) do
    :lists.zipwith(
      fn lA, lB ->
        lcg_merge_locks(lA, lB)
      end,
      a,
      b
    )
  end

  defp lcg_merge_locks(l, l) do
    l
  end

  defp lcg_merge_locks({name, id, dA, iA}, {name, id, dB, iB}) do
    direct = :lists.umerge(dA, dB)
    indirect = :lists.umerge(iA, iB)
    {name, id, direct, indirect -- direct}
  end

  defp lcg_print(out, lL) do
    :io.format(out, '[', [])
    lcg_print_locks(out, lL)
    :io.format(out, '].\n', [])
    :ok
  end

  defp lcg_print_locks(out, [{_, _} = noLock | rest]) do
    :io.format(out, '~p,\n', [noLock])
    lcg_print_locks(out, rest)
  end

  defp lcg_print_locks(out, [lastLock]) do
    :io.format(out, '~w', [lastLock])
  end

  defp lcg_print_locks(out, [lock | rest]) do
    :io.format(out, '~w,\n', [lock])
    lcg_print_locks(out, rest)
  end

  def alloc_blocks_size(type) do
    allocs = :erlang.system_info(:alloc_util_allocators)
    sizes = :erlang.system_info({:allocator_sizes, allocs})
    alloc_blocks_size_1(sizes, type, 0)
  end

  defp alloc_blocks_size_1([], _Type, 0) do
    :undefined
  end

  defp alloc_blocks_size_1([{_Type, false} | rest], type, acc) do
    alloc_blocks_size_1(rest, type, acc)
  end

  defp alloc_blocks_size_1([{_Type, instances} | rest], type, acc) do
    f = fn {:instance, _, l}, acc0 ->
      mBCSPool =
        case :lists.keyfind(:mbcs_pool, 1, l) do
          {_, pool} ->
            pool

          false ->
            []
        end

      {_, mBCS} = :lists.keyfind(:mbcs, 1, l)
      {_, sBCS} = :lists.keyfind(:sbcs, 1, l)
      acc1 = sum_block_sizes(mBCSPool, type, acc0)
      acc2 = sum_block_sizes(mBCS, type, acc1)
      sum_block_sizes(sBCS, type, acc2)
    end

    alloc_blocks_size_1(rest, type, :lists.foldl(f, acc, instances))
  end

  defp alloc_blocks_size_1([], _Type, acc) do
    acc
  end

  defp sum_block_sizes([{:blocks, list} | rest], type, acc) do
    sum_block_sizes(rest, type, sum_block_sizes_1(list, type, acc))
  end

  defp sum_block_sizes([_ | rest], type, acc) do
    sum_block_sizes(rest, type, acc)
  end

  defp sum_block_sizes([], _Type, acc) do
    acc
  end

  defp sum_block_sizes_1([{type, l} | rest], type, acc0) do
    acc =
      :lists.foldl(
        fn
          {:size, sz, _, _}, sz0 ->
            sz0 + sz

          {:size, sz}, sz0 ->
            sz0 + sz

          _, sz ->
            sz
        end,
        acc0,
        l
      )

    sum_block_sizes_1(rest, type, acc)
  end

  defp sum_block_sizes_1([_ | rest], type, acc) do
    sum_block_sizes_1(rest, type, acc)
  end

  defp sum_block_sizes_1([], _Type, acc) do
    acc
  end
end
