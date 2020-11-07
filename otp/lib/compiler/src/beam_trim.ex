defmodule :m_beam_trim do
  use Bitwise
  import :lists, only: [any: 2, member: 2, reverse: 1, reverse: 2, sort: 1]
  require Record
  Record.defrecord(:r_st, :st, safe: :undefined)

  def module({mod, exp, attr, fs0, lc}, _Opts) do
    fs =
      for f <- fs0 do
        function(f)
      end

    {:ok, {mod, exp, attr, fs, lc}}
  end

  defp function({:function, name, arity, cLabel, is0}) do
    try do
      st = r_st(safe: safe_labels(is0, []))
      is = trim(is0, st, [])
      {:function, name, arity, cLabel, is}
    catch
      class, error ->
        :io.fwrite('Function: ~w/~w\n', [name, arity])
        :erlang.raise(class, error, __STACKTRACE__)
    end
  end

  defp trim([{:init_yregs, {:list, kills0}} = i | is0], st, acc) do
    kills =
      for y <- kills0 do
        {:kill, y}
      end

    try do
      {frameSize, layout} = frame_layout(is0, kills, st)
      isNotRecursive = is_not_recursive(is0)
      recipes = trim_recipes(layout, isNotRecursive)
      try_remap(recipes, is0, frameSize)
    catch
      :not_possible ->
        trim(is0, st, [i | acc])
    else
      {is, trimInstr} ->
        trim(is, st, reverse(trimInstr) ++ acc)
    end
  end

  defp trim([i | is], st, acc) do
    trim(is, st, [i | acc])
  end

  defp trim([], _, acc) do
    reverse(acc)
  end

  defp is_not_recursive([{:call_ext, _, ext} | _]) do
    case ext do
      {:extfunc, m, f, a} ->
        :erl_bifs.is_pure(m, f, a)

      _ ->
        false
    end
  end

  defp is_not_recursive([{:block, _} | is]) do
    is_not_recursive(is)
  end

  defp is_not_recursive([{:line, _} | is]) do
    is_not_recursive(is)
  end

  defp is_not_recursive(_) do
    false
  end

  defp trim_recipes(layout, isNotRecursive) do
    recipes = construct_recipes(layout, 0, [], [])

    numOrigKills =
      length(
        for {:kill, _} = i <- layout do
          i
        end
      )

    isTooExpensive = is_too_expensive_fun(isNotRecursive)

    for r <- recipes,
        not is_too_expensive(r, numOrigKills, isTooExpensive) do
      r
    end
  end

  defp construct_recipes([{:kill, {:y, trim0}} | ks], trim0, moves, acc) do
    trim = trim0 + 1
    recipe = {ks, trim, moves}
    construct_recipes(ks, trim, moves, [recipe | acc])
  end

  defp construct_recipes([{:dead, {:y, trim0}} | ks], trim0, moves, acc) do
    trim = trim0 + 1
    recipe = {ks, trim, moves}
    construct_recipes(ks, trim, moves, [recipe | acc])
  end

  defp construct_recipes([{:live, {:y, trim0} = src} | ks0], trim0, moves0, acc) do
    case take_last_dead(ks0) do
      :none ->
        acc

      {dst, ks} ->
        trim = trim0 + 1
        moves = [{:move, src, dst} | moves0]
        recipe = {ks, trim, moves}
        construct_recipes(ks, trim, moves, [recipe | acc])
    end
  end

  defp construct_recipes([], _, _, acc) do
    acc
  end

  defp take_last_dead(l) do
    take_last_dead_1(reverse(l))
  end

  defp take_last_dead_1([{:kill, reg} | is]) do
    {reg, reverse(is)}
  end

  defp take_last_dead_1([{:dead, reg} | is]) do
    {reg, reverse(is)}
  end

  defp take_last_dead_1(_) do
    :none
  end

  defp is_too_expensive({ks, _, moves}, numOrigKills, isTooExpensive) do
    numKills = num_kills(ks, 0)
    numMoves = length(moves)
    isTooExpensive.(numKills, numMoves, numOrigKills)
  end

  defp num_kills([{:kill, _} | t], acc) do
    num_kills(t, acc + 1)
  end

  defp num_kills([_ | t], acc) do
    num_kills(t, acc)
  end

  defp num_kills([], acc) do
    acc
  end

  defp is_too_expensive_fun(true) do
    fn numKills, numMoves, numOrigKills ->
      penalty =
        cond do
          numMoves !== 0 ->
            1

          true ->
            0
        end

      1 + penalty + numKills + numMoves > numOrigKills
    end
  end

  defp is_too_expensive_fun(false) do
    fn numKills, numMoves, numOrigKills ->
      numKills + numMoves > numOrigKills
    end
  end

  defp try_remap([r | rs], is, frameSize) do
    {trimInstr, map} = expand_recipe(r, frameSize)

    try do
      {remap(is, map, []), trimInstr}
    catch
      :not_possible ->
        try_remap(rs, is, frameSize)
    end
  end

  defp try_remap([], _, _) do
    throw(:not_possible)
  end

  defp expand_recipe({layout, trim, moves}, frameSize) do
    is = reverse(moves, [{:trim, trim, frameSize - trim}])
    map = create_map(trim, moves)

    case (for {:kill, y} <- layout do
            y
          end) do
      [] ->
        {is, map}

      [_ | _] = yregs ->
        {[{:init_yregs, {:list, yregs}} | is], map}
    end
  end

  defp create_map(trim, []) do
    fn
      {:y, y} when y < trim ->
        throw(:not_possible)

      {:y, y} ->
        {:y, y - trim}

      {:frame_size, n} ->
        n - trim

      any ->
        any
    end
  end

  defp create_map(trim, moves) do
    map0 =
      for {:move, {:y, src}, {:y, dst}} <- moves do
        {src, dst - trim}
      end

    map = :maps.from_list(map0)

    illegalTargets =
      :cerl_sets.from_list(
        for {:move, _, {:y, dst}} <- moves do
          dst
        end
      )

    fn
      {:y, y0} when y0 < trim ->
        case map do
          %{^y0 => y} ->
            {:y, y}

          %{} ->
            throw(:not_possible)
        end

      {:y, y} ->
        case :cerl_sets.is_element(y, illegalTargets) do
          true ->
            throw(:not_possible)

          false ->
            {:y, y - trim}
        end

      {:frame_size, n} ->
        n - trim

      any ->
        any
    end
  end

  defp remap([{:%, comment} = i | is], map, acc) do
    case comment do
      {:var_info, var, type} ->
        remap(is, map, [{:%, {:var_info, map.(var), type}} | acc])

      _ ->
        remap(is, map, [i | acc])
    end
  end

  defp remap([{:block, bl0} | is], map, acc) do
    bl = remap_block(bl0, map, [])
    remap(is, map, [{:block, bl} | acc])
  end

  defp remap([{:bs_get_tail, src, dst, live} | is], map, acc) do
    i = {:bs_get_tail, map.(src), map.(dst), live}
    remap(is, map, [i | acc])
  end

  defp remap([{:bs_start_match4, fail, live, src, dst} | is], map, acc) do
    i = {:bs_start_match4, fail, live, map.(src), map.(dst)}
    remap(is, map, [i | acc])
  end

  defp remap([{:bs_set_position, src1, src2} | is], map, acc) do
    i = {:bs_set_position, map.(src1), map.(src2)}
    remap(is, map, [i | acc])
  end

  defp remap([{:call_fun, _} = i | is], map, acc) do
    remap(is, map, [i | acc])
  end

  defp remap([{:call, _, _} = i | is], map, acc) do
    remap(is, map, [i | acc])
  end

  defp remap([{:call_ext, _, _} = i | is], map, acc) do
    remap(is, map, [i | acc])
  end

  defp remap([{:apply, _} = i | is], map, acc) do
    remap(is, map, [i | acc])
  end

  defp remap([{:bif, name, fail, ss, d} | is], map, acc) do
    i =
      {:bif, name, fail,
       for s <- ss do
         map.(s)
       end, map.(d)}

    remap(is, map, [i | acc])
  end

  defp remap([{:gc_bif, name, fail, live, ss, d} | is], map, acc) do
    i =
      {:gc_bif, name, fail, live,
       for s <- ss do
         map.(s)
       end, map.(d)}

    remap(is, map, [i | acc])
  end

  defp remap(
         [
           {:get_map_elements, fail, m, {:list, l0}}
           | is
         ],
         map,
         acc
       ) do
    l =
      for e <- l0 do
        map.(e)
      end

    i = {:get_map_elements, fail, map.(m), {:list, l}}
    remap(is, map, [i | acc])
  end

  defp remap([{:bs_init, fail, info, live, ss0, dst0} | is], map, acc) do
    ss =
      for src <- ss0 do
        map.(src)
      end

    dst = map.(dst0)
    i = {:bs_init, fail, info, live, ss, dst}
    remap(is, map, [i | acc])
  end

  defp remap([{:bs_put = op, fail, info, ss} | is], map, acc) do
    i =
      {op, fail, info,
       for s <- ss do
         map.(s)
       end}

    remap(is, map, [i | acc])
  end

  defp remap([{:init_yregs, {:list, yregs0}} | is], map, acc) do
    yregs =
      sort(
        for y <- yregs0 do
          map.(y)
        end
      )

    i = {:init_yregs, {:list, yregs}}
    remap(is, map, [i | acc])
  end

  defp remap([{:make_fun2, _, _, _, _} = i | t], map, acc) do
    remap(t, map, [i | acc])
  end

  defp remap(
         [
           {:make_fun3, f, index, oldUniq, dst0, {:list, env0}}
           | t
         ],
         map,
         acc
       ) do
    env =
      for e <- env0 do
        map.(e)
      end

    dst = map.(dst0)
    i = {:make_fun3, f, index, oldUniq, dst, {:list, env}}
    remap(t, map, [i | acc])
  end

  defp remap([{:deallocate, n} | is], map, acc) do
    i = {:deallocate, map.({:frame_size, n})}
    remap(is, map, [i | acc])
  end

  defp remap([{:swap, reg1, reg2} | is], map, acc) do
    i = {:swap, map.(reg1), map.(reg2)}
    remap(is, map, [i | acc])
  end

  defp remap([{:test, name, fail, ss} | is], map, acc) do
    i =
      {:test, name, fail,
       for s <- ss do
         map.(s)
       end}

    remap(is, map, [i | acc])
  end

  defp remap([{:test, name, fail, live, ss, dst} | is], map, acc) do
    i =
      {:test, name, fail, live,
       for s <- ss do
         map.(s)
       end, map.(dst)}

    remap(is, map, [i | acc])
  end

  defp remap([:return | _] = is, _, acc) do
    reverse(acc, is)
  end

  defp remap([{:line, _} = i | is], map, acc) do
    remap(is, map, [i | acc])
  end

  defp remap_block([{:set, ds0, ss0, info} | is], map, acc) do
    ds =
      for d <- ds0 do
        map.(d)
      end

    ss =
      for s <- ss0 do
        map.(s)
      end

    remap_block(is, map, [{:set, ds, ss, info} | acc])
  end

  defp remap_block([], _, acc) do
    reverse(acc)
  end

  defp safe_labels([{:label, l} | is], acc) do
    case is_safe_label(is) do
      true ->
        safe_labels(is, [l | acc])

      false ->
        safe_labels(is, acc)
    end
  end

  defp safe_labels([_ | is], acc) do
    safe_labels(is, acc)
  end

  defp safe_labels([], acc) do
    :cerl_sets.from_list(acc)
  end

  defp is_safe_label([{:%, _} | is]) do
    is_safe_label(is)
  end

  defp is_safe_label([{:line, _} | is]) do
    is_safe_label(is)
  end

  defp is_safe_label([{:badmatch, {tag, _}} | _]) do
    tag !== :y
  end

  defp is_safe_label([{:case_end, {tag, _}} | _]) do
    tag !== :y
  end

  defp is_safe_label([{:try_case_end, {tag, _}} | _]) do
    tag !== :y
  end

  defp is_safe_label([:if_end | _]) do
    true
  end

  defp is_safe_label([{:block, bl} | is]) do
    is_safe_label_block(bl) and is_safe_label(is)
  end

  defp is_safe_label([{:call_ext, _, {:extfunc, m, f, a}} | _]) do
    :erl_bifs.is_exit_bif(m, f, a)
  end

  defp is_safe_label(_) do
    false
  end

  defp is_safe_label_block([{:set, ds, ss, _} | is]) do
    isYreg = fn
      {:y, _} ->
        true

      _ ->
        false
    end

    not (any(isYreg, ss) or
           any(
             isYreg,
             ds
           )) and is_safe_label_block(is)
  end

  defp is_safe_label_block([]) do
    true
  end

  defp frame_layout(is, kills, r_st(safe: safe)) do
    n = frame_size(is, safe)

    isKilled = fn r ->
      is_not_used(r, is)
    end

    {n, frame_layout_1(kills, 0, n, isKilled, [])}
  end

  defp frame_layout_1([{:kill, {:y, y}} = i | ks], y, n, isKilled, acc) do
    frame_layout_1(ks, y + 1, n, isKilled, [i | acc])
  end

  defp frame_layout_1(ks, y, n, isKilled, acc) when y < n do
    r = {:y, y}

    i =
      case isKilled.(r) do
        false ->
          {:live, r}

        true ->
          {:dead, r}
      end

    frame_layout_1(ks, y + 1, n, isKilled, [i | acc])
  end

  defp frame_layout_1([], y, y, _, acc) do
    frame_layout_2(acc)
  end

  defp frame_layout_2([{:live, _} | is]) do
    frame_layout_2(is)
  end

  defp frame_layout_2(is) do
    reverse(is)
  end

  defp frame_size([{:%, _} | is], safe) do
    frame_size(is, safe)
  end

  defp frame_size([{:block, _} | is], safe) do
    frame_size(is, safe)
  end

  defp frame_size([{:call_fun, _} | is], safe) do
    frame_size(is, safe)
  end

  defp frame_size([{:call, _, _} | is], safe) do
    frame_size(is, safe)
  end

  defp frame_size([{:call_ext, _, _} = i | is], safe) do
    case :beam_jump.is_exit_instruction(i) do
      true ->
        throw(:not_possible)

      false ->
        frame_size(is, safe)
    end
  end

  defp frame_size([{:apply, _} | is], safe) do
    frame_size(is, safe)
  end

  defp frame_size([{:bif, _, {:f, l}, _, _} | is], safe) do
    frame_size_branch(l, is, safe)
  end

  defp frame_size([{:gc_bif, _, {:f, l}, _, _, _} | is], safe) do
    frame_size_branch(l, is, safe)
  end

  defp frame_size([{:test, _, {:f, l}, _} | is], safe) do
    frame_size_branch(l, is, safe)
  end

  defp frame_size([{:test, _, {:f, l}, _, _, _} | is], safe) do
    frame_size_branch(l, is, safe)
  end

  defp frame_size([{:bs_init, {:f, l}, _, _, _, _} | is], safe) do
    frame_size_branch(l, is, safe)
  end

  defp frame_size([{:bs_put, {:f, l}, _, _} | is], safe) do
    frame_size_branch(l, is, safe)
  end

  defp frame_size([{:init_yregs, _} | is], safe) do
    frame_size(is, safe)
  end

  defp frame_size([{:make_fun2, _, _, _, _} | is], safe) do
    frame_size(is, safe)
  end

  defp frame_size([{:make_fun3, _, _, _, _, _} | is], safe) do
    frame_size(is, safe)
  end

  defp frame_size(
         [{:get_map_elements, {:f, l}, _, _} | is],
         safe
       ) do
    frame_size_branch(l, is, safe)
  end

  defp frame_size([{:deallocate, n} | _], _) do
    n
  end

  defp frame_size([{:line, _} | is], safe) do
    frame_size(is, safe)
  end

  defp frame_size(
         [{:bs_start_match4, fail, _, _, _} | is],
         safe
       ) do
    case fail do
      {:f, l} ->
        frame_size_branch(l, is, safe)

      _ ->
        frame_size(is, safe)
    end
  end

  defp frame_size([{:bs_set_position, _, _} | is], safe) do
    frame_size(is, safe)
  end

  defp frame_size([{:bs_get_tail, _, _, _} | is], safe) do
    frame_size(is, safe)
  end

  defp frame_size([{:swap, _, _} | is], safe) do
    frame_size(is, safe)
  end

  defp frame_size(_, _) do
    throw(:not_possible)
  end

  defp frame_size_branch(0, is, safe) do
    frame_size(is, safe)
  end

  defp frame_size_branch(l, is, safe) do
    case :cerl_sets.is_element(l, safe) do
      false ->
        throw(:not_possible)

      true ->
        frame_size(is, safe)
    end
  end

  defp is_not_used(y, [{:%, _} | is]) do
    is_not_used(y, is)
  end

  defp is_not_used(y, [{:apply, _} | is]) do
    is_not_used(y, is)
  end

  defp is_not_used(y, [{:bif, _, {:f, _}, ss, dst} | is]) do
    is_not_used_ss_dst(y, ss, dst, is)
  end

  defp is_not_used(y, [{:block, bl} | is]) do
    case is_not_used_block(y, bl) do
      :used ->
        false

      :killed ->
        true

      :transparent ->
        is_not_used(y, is)
    end
  end

  defp is_not_used(y, [{:bs_get_tail, src, dst, _} | is]) do
    is_not_used_ss_dst(y, [src], dst, is)
  end

  defp is_not_used(y, [{:bs_init, _, _, _, ss, dst} | is]) do
    is_not_used_ss_dst(y, ss, dst, is)
  end

  defp is_not_used(y, [{:bs_put, {:f, _}, _, ss} | is]) do
    not member(y, ss) and is_not_used(y, is)
  end

  defp is_not_used(
         y,
         [{:bs_start_match4, _Fail, _Live, src, dst} | is]
       ) do
    y !== src and y !== dst and is_not_used(y, is)
  end

  defp is_not_used(y, [{:bs_set_position, src1, src2} | is]) do
    y !== src1 and y !== src2 and is_not_used(y, is)
  end

  defp is_not_used(y, [{:call, _, _} | is]) do
    is_not_used(y, is)
  end

  defp is_not_used(y, [{:call_ext, _, _} = i | is]) do
    :beam_jump.is_exit_instruction(i) or is_not_used(y, is)
  end

  defp is_not_used(y, [{:call_fun, _} | is]) do
    is_not_used(y, is)
  end

  defp is_not_used(_Y, [{:deallocate, _} | _]) do
    true
  end

  defp is_not_used(
         y,
         [{:gc_bif, _, {:f, _}, _Live, ss, dst} | is]
       ) do
    is_not_used_ss_dst(y, ss, dst, is)
  end

  defp is_not_used(
         y,
         [
           {:get_map_elements, {:f, _}, s, {:list, list}}
           | is
         ]
       ) do
    {ss, ds} = :beam_utils.split_even(list)

    case member(y, [s | ss]) do
      true ->
        false

      false ->
        member(y, ds) or is_not_used(y, is)
    end
  end

  defp is_not_used(y, [{:init_yregs, {:list, yregs}} | is]) do
    member(y, yregs) or is_not_used(y, is)
  end

  defp is_not_used(y, [{:line, _} | is]) do
    is_not_used(y, is)
  end

  defp is_not_used(y, [{:make_fun2, _, _, _, _} | is]) do
    is_not_used(y, is)
  end

  defp is_not_used(
         y,
         [{:make_fun3, _, _, _, dst, {:list, env}} | is]
       ) do
    is_not_used_ss_dst(y, env, dst, is)
  end

  defp is_not_used(y, [{:swap, reg1, reg2} | is]) do
    y !== reg1 and y !== reg2 and is_not_used(y, is)
  end

  defp is_not_used(y, [{:test, _, _, ss} | is]) do
    not member(y, ss) and is_not_used(y, is)
  end

  defp is_not_used(
         y,
         [{:test, _Op, {:f, _}, _Live, ss, dst} | is]
       ) do
    is_not_used_ss_dst(y, ss, dst, is)
  end

  defp is_not_used_block(y, [{:set, ds, ss, _} | is]) do
    case member(y, ss) do
      true ->
        :used

      false ->
        case member(y, ds) do
          true ->
            :killed

          false ->
            is_not_used_block(y, is)
        end
    end
  end

  defp is_not_used_block(_Y, []) do
    :transparent
  end

  defp is_not_used_ss_dst(y, ss, dst, is) do
    not member(y, ss) and (y === dst or is_not_used(y, is))
  end
end
