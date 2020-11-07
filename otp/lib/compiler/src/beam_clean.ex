defmodule :m_beam_clean do
  use Bitwise

  def module({mod, exp, attr, fs0, _}, opts) do
    order =
      for {:function, _, _, lbl, _} <- fs0 do
        lbl
      end

    all =
      :maps.from_list(
        for {:function, _, _, lbl, _} = func <- fs0 do
          {lbl, func}
        end
      )

    workList = rootset(fs0, exp, attr)
    used = find_all_used(workList, all, :cerl_sets.from_list(workList))
    fs1 = remove_unused(order, used, all)
    {fs2, lc} = clean_labels(fs1)
    fs3 = fix_swap(fs2, opts)
    fs = maybe_remove_lines(fs3, opts)
    {:ok, {mod, exp, attr, fs, lc}}
  end

  defp rootset(fs, root0, attr) do
    root1 =
      case :proplists.get_value(:on_load, attr) do
        :undefined ->
          root0

        [onLoad] ->
          [onLoad | root0]
      end

    root = :sofs.set(root1, [:function])

    map0 =
      for {:function, name, arity, lbl, _} <- fs do
        {{name, arity}, lbl}
      end

    map = :sofs.relation(map0, [{:function, :label}])
    :sofs.to_external(:sofs.image(map, root))
  end

  defp remove_unused([f | fs], used, all) do
    case :cerl_sets.is_element(f, used) do
      false ->
        remove_unused(fs, used, all)

      true ->
        [:erlang.map_get(f, all) | remove_unused(fs, used, all)]
    end
  end

  defp remove_unused([], _, _) do
    []
  end

  defp find_all_used([f | fs0], all, used0) do
    {:function, _, _, _, code} = :erlang.map_get(f, all)
    {fs, used} = update_work_list(code, {fs0, used0})
    find_all_used(fs, all, used)
  end

  defp find_all_used([], _All, used) do
    used
  end

  defp update_work_list([{:call, _, {:f, l}} | is], sets) do
    update_work_list(is, add_to_work_list(l, sets))
  end

  defp update_work_list([{:make_fun2, {:f, l}, _, _, _} | is], sets) do
    update_work_list(is, add_to_work_list(l, sets))
  end

  defp update_work_list(
         [{:make_fun3, {:f, l}, _, _, _, _} | is],
         sets
       ) do
    update_work_list(is, add_to_work_list(l, sets))
  end

  defp update_work_list([_ | is], sets) do
    update_work_list(is, sets)
  end

  defp update_work_list([], sets) do
    sets
  end

  defp add_to_work_list(f, {fs, used} = sets) do
    case :cerl_sets.is_element(f, used) do
      true ->
        sets

      false ->
        {[f | fs], :cerl_sets.add_element(f, used)}
    end
  end

  require Record
  Record.defrecord(:r_st, :st, lmap: :undefined, entry: :undefined, lc: :undefined)

  def clean_labels(fs0) do
    st0 = r_st(lmap: [], entry: 1, lc: 1)
    {fs1, r_st(lmap: lmap0, lc: lc)} = function_renumber(fs0, st0, [])
    lmap = :maps.from_list(lmap0)
    fs = function_replace(fs1, lmap, [])
    {fs, lc}
  end

  defp function_renumber([{:function, name, arity, _Entry, asm0} | fs], st0, acc) do
    {asm, st} = renumber_labels(asm0, [], st0)
    function_renumber(fs, st, [{:function, name, arity, r_st(st, :entry), asm} | acc])
  end

  defp function_renumber([], st, acc) do
    {acc, st}
  end

  defp renumber_labels([{:label, old} | is], [{:label, new} | _] = acc, r_st(lmap: d0) = st) do
    d = [{old, new} | d0]
    renumber_labels(is, acc, r_st(st, lmap: d))
  end

  defp renumber_labels([{:label, old} | is], acc, st0) do
    new = r_st(st0, :lc)
    d = [{old, new} | r_st(st0, :lmap)]
    renumber_labels(is, [{:label, new} | acc], r_st(st0, lmap: d, lc: new + 1))
  end

  defp renumber_labels([{:func_info, _, _, _} = fi | is], acc, st0) do
    renumber_labels(is, [fi | acc], r_st(st0, entry: r_st(st0, :lc)))
  end

  defp renumber_labels([i | is], acc, st0) do
    renumber_labels(is, [i | acc], st0)
  end

  defp renumber_labels([], acc, st) do
    {acc, st}
  end

  defp function_replace([{:function, name, arity, entry, asm0} | fs], dict, acc) do
    asm =
      try do
        fb = fn old ->
          throw({:error, {:undefined_label, old}})
        end

        :beam_utils.replace_labels(asm0, [], dict, fb)
      catch
        {:error, {:undefined_label, lbl} = reason} ->
          :io.format('Function ~s/~w refers to undefined label ~w\n', [name, arity, lbl])
          exit(reason)
      end

    function_replace(fs, dict, [{:function, name, arity, entry, asm} | acc])
  end

  defp function_replace([], _, acc) do
    acc
  end

  defp fix_swap(fs, opts) do
    case :proplists.get_bool(:no_swap, opts) do
      false ->
        fs

      true ->
        fold_functions(&swap_moves/1, fs)
    end
  end

  defp swap_moves([{:swap, reg1, reg2} | is]) do
    temp = {:x, 1022}

    [
      [{:move, reg1, temp}, {:move, reg2, reg1}, {:move, temp, reg2}]
      | swap_moves(is)
    ]
  end

  defp swap_moves([i | is]) do
    [i | swap_moves(is)]
  end

  defp swap_moves([]) do
    []
  end

  defp maybe_remove_lines(fs, opts) do
    case :proplists.get_bool(:no_line_info, opts) do
      false ->
        fs

      true ->
        fold_functions(&remove_lines/1, fs)
    end
  end

  defp remove_lines([{:line, _} | is]) do
    remove_lines(is)
  end

  defp remove_lines([{:block, bl0} | is]) do
    bl = remove_lines_block(bl0)
    [{:block, bl} | remove_lines(is)]
  end

  defp remove_lines([i | is]) do
    [i | remove_lines(is)]
  end

  defp remove_lines([]) do
    []
  end

  defp remove_lines_block([{:set, _, _, {:line, _}} | is]) do
    remove_lines_block(is)
  end

  defp remove_lines_block([i | is]) do
    [i | remove_lines_block(is)]
  end

  defp remove_lines_block([]) do
    []
  end

  defp fold_functions(f, [{:function, n, a, lbl, is0} | t]) do
    is = f.(is0)
    [{:function, n, a, lbl, is} | fold_functions(f, t)]
  end

  defp fold_functions(_F, []) do
    []
  end
end
