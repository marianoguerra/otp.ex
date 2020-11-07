defmodule :m_beam_ssa_pp do
  use Bitwise
  require Record

  Record.defrecord(:r_b_module, :b_module,
    anno: %{},
    name: :undefined,
    exports: :undefined,
    attributes: :undefined,
    body: :undefined
  )

  Record.defrecord(:r_b_function, :b_function,
    anno: %{},
    args: :undefined,
    bs: :undefined,
    cnt: :undefined
  )

  Record.defrecord(:r_b_blk, :b_blk, anno: %{}, is: :undefined, last: :undefined)
  Record.defrecord(:r_b_set, :b_set, anno: %{}, dst: :none, op: :undefined, args: [])
  Record.defrecord(:r_b_ret, :b_ret, anno: %{}, arg: :undefined)

  Record.defrecord(:r_b_br, :b_br, anno: %{}, bool: :undefined, succ: :undefined, fail: :undefined)

  Record.defrecord(:r_b_switch, :b_switch,
    anno: %{},
    arg: :undefined,
    fail: :undefined,
    list: :undefined
  )

  Record.defrecord(:r_b_var, :b_var, name: :undefined)
  Record.defrecord(:r_b_literal, :b_literal, val: :undefined)
  Record.defrecord(:r_b_remote, :b_remote, mod: :undefined, name: :undefined, arity: :undefined)

  Record.defrecord(:r_b_local, :b_local,
    name: :undefined,
    arity: :undefined
  )

  def format_function(r_b_function(anno: anno0, args: args, bs: blocks, cnt: counter)) do
    %{:func_info => {m, f, _}} = anno0

    anno =
      :maps.without(
        [:func_info, :location, :live_intervals, :registers],
        anno0
      )

    funcAnno =
      case anno0 do
        %{:live_intervals => intervals} ->
          %{anno0 | :live_intervals => :maps.from_list(intervals)}

        %{} ->
          anno0
      end

    reachableBlocks = :beam_ssa.rpo(blocks)
    all = :maps.keys(blocks)

    unreachable =
      :ordsets.subtract(
        :ordsets.from_list(all),
        :ordsets.from_list(reachableBlocks)
      )

    [
      case anno0 do
        %{:location => {filename, line}} ->
          :io_lib.format('%% ~ts:~p\n', [filename, line])

        %{} ->
          []
      end,
      :io_lib.format('%% Counter = ~p\n', [counter]),
      for {key, value} <- :lists.sort(:maps.to_list(anno)) do
        format_anno(key, value)
      end,
      :io_lib.format('function `~p`:`~p`(~ts) {\n', [m, f, format_args(args, funcAnno)]),
      for var <- args do
        format_live_interval(var, funcAnno)
      end,
      format_blocks(reachableBlocks, blocks, funcAnno),
      case unreachable do
        [] ->
          []

        [_ | _] ->
          ['\n%% Unreachable blocks\n\n', format_blocks(unreachable, blocks, funcAnno)]
      end,
      '}\n'
    ]
  end

  def format_instr(r_b_set() = i) do
    cs = :lists.flatten(format_instr(r_b_set(i, anno: %{}), %{}, true))
    :string.trim(cs, :leading)
  end

  def format_instr(i0) do
    i = :erlang.setelement(2, i0, %{})
    cs = :lists.flatten(format_terminator(i, %{}))
    :string.trim(cs, :both)
  end

  def format_var(v) do
    cs = :lists.flatten(format_var(v, %{}))
    :string.trim(cs, :leading)
  end

  defp format_anno(:parameter_info, map) when is_map(map) do
    case map_size(map) do
      0 ->
        []

      _ ->
        params = :lists.sort(:maps.to_list(map))
        break = '\n%%     '

        [
          :io_lib.format('%% Parameters\n', []),
          for {v, i} <- params do
            :io_lib.format(
              '%%    ~s =>~s~s\n',
              [format_var(v), break, format_param_info(i, break)]
            )
          end
        ]
    end
  end

  defp format_anno(key, map) when is_map(map) do
    sorted = :lists.sort(:maps.to_list(map))

    [
      :io_lib.format('%% ~s:\n', [key]),
      for {k, v} <- sorted do
        :io_lib.format('%%    ~w => ~w\n', [k, v])
      end
    ]
  end

  defp format_anno(key, value) do
    :io_lib.format('%% ~s: ~p\n', [key, value])
  end

  defp format_param_info([{:type, t} | infos], break) do
    [
      format_type(t, break)
      | format_param_info(
          infos,
          break
        )
    ]
  end

  defp format_param_info([info | infos], break) do
    [
      :io_lib.format('~s~p', [break, info])
      | format_param_info(infos, break)
    ]
  end

  defp format_param_info([], _Break) do
    []
  end

  defp format_type(t, break) do
    indented = :lists.flatten(:io_lib.format('~p', [t]))
    :string.replace(indented, [?\n], break, :all)
  end

  defp format_blocks(ls, blocks, anno) do
    pP =
      for l <- ls do
        format_block(l, blocks, anno)
      end

    :lists.join(?\n, pP)
  end

  defp format_block(l, blocks, funcAnno) do
    r_b_blk(anno: anno, is: is, last: last) = :maps.get(l, blocks)

    [
      case map_size(anno) do
        0 ->
          []

        _ ->
          :io_lib.format('%% ~p\n', [anno])
      end,
      :io_lib.format('~p:', [l]),
      format_instrs(is, funcAnno, true),
      ?\n,
      format_terminator(last, funcAnno)
    ]
  end

  defp format_instrs([i | is], funcAnno, first) do
    [?\n, format_instr(i, funcAnno, first), format_instrs(is, funcAnno, false)]
  end

  defp format_instrs([], _FuncAnno, _First) do
    []
  end

  defp format_instr(r_b_set(anno: anno, op: op, dst: dst, args: args), funcAnno, first) do
    annoStr = format_anno(anno)
    liveIntervalStr = format_live_interval(dst, funcAnno)

    [
      cond do
        first ->
          []

        annoStr !== [] or liveIntervalStr !== [] ->
          ?\n

        true ->
          []
      end,
      annoStr,
      liveIntervalStr,
      :io_lib.format(
        '  ~s~ts = ~ts',
        [format_i_number(anno), format_var(dst, funcAnno), format_op(op)]
      ),
      case args do
        [] ->
          []

        [_ | _] ->
          :io_lib.format(' ~ts', [format_args(args, funcAnno)])
      end
    ]
  end

  defp format_i_number(%{:n => n}) do
    :io_lib.format('[~p] ', [n])
  end

  defp format_i_number(%{}) do
    []
  end

  defp format_terminator(
         r_b_br(anno: a, bool: r_b_literal(val: true), succ: same, fail: same),
         _
       ) do
    :io_lib.format(
      '  ~sbr ~ts\n',
      [format_i_number(a), format_label(same)]
    )
  end

  defp format_terminator(
         r_b_br(anno: a, bool: bool, succ: succ, fail: fail),
         funcAnno
       ) do
    :io_lib.format(
      '  ~sbr ~ts, ~ts, ~ts\n',
      [format_i_number(a), format_arg(bool, funcAnno), format_label(succ), format_label(fail)]
    )
  end

  defp format_terminator(
         r_b_switch(anno: a, arg: arg, fail: fail, list: list),
         funcAnno
       ) do
    :io_lib.format(
      '  ~sswitch ~ts, ~ts, ~ts\n',
      [
        format_i_number(a),
        format_arg(arg, funcAnno),
        format_label(fail),
        format_switch_list(list, funcAnno)
      ]
    )
  end

  defp format_terminator(r_b_ret(anno: a, arg: arg), funcAnno) do
    :io_lib.format(
      '  ~sret ~ts\n',
      [format_i_number(a), format_arg(arg, funcAnno)]
    )
  end

  defp format_op({prefix, name}) do
    :io_lib.format('~p:~p', [prefix, name])
  end

  defp format_op(name) do
    :io_lib.format('~p', [name])
  end

  defp format_register(r_b_var() = v, %{:registers => regs}) do
    {tag, n} = :maps.get(v, regs)
    :io_lib.format('~p~p', [tag, n])
  end

  defp format_register(_, %{}) do
    ''
  end

  defp format_var(var, funcAnno) do
    varString = format_var_1(var)

    case format_register(var, funcAnno) do
      [] ->
        varString

      [_ | _] = reg ->
        [reg, ?/, varString]
    end
  end

  defp format_var_1(r_b_var(name: {name, uniq})) do
    cond do
      is_atom(name) ->
        :io_lib.format('~ts:~p', [name, uniq])

      is_integer(name) ->
        :io_lib.format('_~p:~p', [name, uniq])
    end
  end

  defp format_var_1(r_b_var(name: name)) when is_atom(name) do
    :erlang.atom_to_list(name)
  end

  defp format_var_1(r_b_var(name: name)) when is_integer(name) do
    '_' ++ :erlang.integer_to_list(name)
  end

  defp format_args(args, funcAnno) do
    ss =
      for arg <- args do
        format_arg(arg, funcAnno)
      end

    :lists.join(', ', ss)
  end

  defp format_arg(r_b_var() = arg, funcAnno) do
    format_var(arg, funcAnno)
  end

  defp format_arg(r_b_literal(val: val), _FuncAnno) do
    :io_lib.format('`~p`', [val])
  end

  defp format_arg(
         r_b_remote(mod: mod, name: name, arity: arity),
         funcAnno
       ) do
    :io_lib.format(
      '(~ts:~ts/~p)',
      [format_arg(mod, funcAnno), format_arg(name, funcAnno), arity]
    )
  end

  defp format_arg(r_b_local(name: name, arity: arity), funcAnno) do
    :io_lib.format('(~ts/~p)', [format_arg(name, funcAnno), arity])
  end

  defp format_arg({value, label}, funcAnno)
       when is_integer(label) do
    :io_lib.format(
      '{ ~ts, ~ts }',
      [format_arg(value, funcAnno), format_label(label)]
    )
  end

  defp format_arg(other, _) do
    :io_lib.format('*** ~p ***', [other])
  end

  defp format_switch_list(list, funcAnno) do
    ss =
      for {val, l} <- list do
        :io_lib.format(
          '{ ~ts, ~ts }',
          [format_arg(val, funcAnno), format_label(l)]
        )
      end

    :io_lib.format('[\n    ~ts\n  ]', [:lists.join(',\n    ', ss)])
  end

  defp format_label(l) do
    :io_lib.format('^~w', [l])
  end

  defp format_anno(%{:n => _} = anno) do
    format_anno(:maps.remove(:n, anno))
  end

  defp format_anno(%{:location => {file, line}} = anno0) do
    anno = :maps.remove(:location, anno0)
    [:io_lib.format('  %% ~ts:~p\n', [file, line]) | format_anno(anno)]
  end

  defp format_anno(%{:result_type => t} = anno0) do
    anno = :maps.remove(:result_type, anno0)
    break = '\n  %%    '

    [
      :io_lib.format('  %% Result type:~s~s\n', [break, format_type(t, break)])
      | format_anno(anno)
    ]
  end

  defp format_anno(anno) do
    format_anno_1(anno)
  end

  defp format_anno_1(anno) do
    case map_size(anno) do
      0 ->
        []

      _ ->
        [:io_lib.format('  %% Anno: ~p\n', [anno])]
    end
  end

  defp format_live_interval(r_b_var() = dst, %{:live_intervals => intervals}) do
    case intervals do
      %{^dst => rs0} ->
        rs1 =
          for {start, end__} <- rs0 do
            :io_lib.format('~p..~p', [start, end__])
          end

        rs = :lists.join(' ', rs1)
        :io_lib.format('  %% ~ts: ~s\n', [format_var_1(dst), rs])

      %{} ->
        []
    end
  end

  defp format_live_interval(_, _) do
    []
  end
end
