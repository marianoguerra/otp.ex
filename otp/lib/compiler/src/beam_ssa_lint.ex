defmodule :m_beam_ssa_lint do
  use Bitwise
  import :lists, only: [append: 1, foldl: 3, foreach: 2]
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

  def module(r_b_module(body: fs, name: name) = mod0, _Options) do
    es0 =
      append(
        for f <- fs do
          validate_function(f)
        end
      )

    case (for e <- es0 do
            {:beam_ssa_lint, e}
          end) do
      [] ->
        {:ok, mod0}

      [_ | _] = es ->
        {:error, [{:erlang.atom_to_list(name), es}]}
    end
  end

  def format_error({{_M, f, a}, error}) do
    [:io_lib.format('~p/~p: ', [f, a]), format_error_1(error)]
  end

  defp format_instr(i) do
    [?', :beam_ssa_pp.format_instr(i), ?']
  end

  defp format_var(v) do
    :beam_ssa_pp.format_var(v)
  end

  defp validate_function(f) do
    try do
      validate_variables(f)
      []
    catch
      reason ->
        %{:func_info => mFA} = r_b_function(f, :anno)
        [{mFA, reason}]

      class, error ->
        :io.fwrite('Function: ~p\n', [r_b_function(f, :anno)])
        :erlang.raise(class, error, __STACKTRACE__)
    end
  end

  Record.defrecord(:r_vvars, :vvars,
    blocks: :undefined,
    branch_def_vars: :undefined,
    defined_vars: :undefined
  )

  defp validate_variables(r_b_function(args: args, bs: blocks)) do
    ^args = vvars_get_variables(args)
    defVars = :gb_sets.from_list(args)
    entry = 0

    state =
      r_vvars(
        blocks: blocks,
        branch_def_vars: %{entry => defVars},
        defined_vars: defVars
      )

    :ok = vvars_assert_unique(blocks, args)
    vvars_phi_nodes(vvars_block(entry, state))
  end

  defp vvars_assert_unique(blocks, args) do
    blockIs =
      for r_b_blk(is: is) <- :maps.values(blocks) do
        is
      end

    defined0 =
      :maps.from_list(
        for v <- args do
          {v, :argument}
        end
      )

    _ =
      foldl(
        fn is, defined ->
          vvars_assert_unique_1(is, defined)
        end,
        defined0,
        blockIs
      )

    :ok
  end

  defp vvars_assert_unique_1([r_b_set(dst: dst) = i | is], defined) do
    case defined do
      %{^dst => old} ->
        throw({:redefined_variable, dst, old, i})

      _ ->
        vvars_assert_unique_1(is, %{defined | dst => i})
    end
  end

  defp vvars_assert_unique_1([], defined) do
    defined
  end

  defp vvars_phi_nodes(r_vvars(blocks: blocks) = state) do
    _ =
      for {id, r_b_blk(is: is)} <- :maps.to_list(blocks) do
        vvars_phi_nodes_1(is, id, state)
      end

    :ok
  end

  defp vvars_phi_nodes_1([r_b_set(op: :phi, args: phis) = i | is], id, state) do
    :ok = vvars_assert_phi_paths(phis, i, id, state)
    :ok = vvars_assert_phi_vars(phis, i, id, state)
    vvars_phi_nodes_1(is, id, state)
  end

  defp vvars_phi_nodes_1([_ | is], id, _State) do
    case (for r_b_set(op: :phi, dst: dst) <- is do
            dst
          end) do
      [var | _] ->
        throw({:phi_inside_block, var, id})

      [] ->
        :ok
    end
  end

  defp vvars_phi_nodes_1([], _Id, _State) do
    :ok
  end

  defp vvars_assert_phi_paths(phis, i, id, state) do
    branchKeys = :maps.keys(r_vvars(state, :branch_def_vars))

    requiredPaths =
      :ordsets.from_list(
        for {from, to} <- branchKeys,
            to === id do
          from
        end
      )

    providedPaths =
      :ordsets.from_list(
        for {_Value, from} <- phis do
          from
        end
      )

    case :ordsets.subtract(
           requiredPaths,
           providedPaths
         ) do
      [_ | _] = missingPaths ->
        throw({:missing_phi_paths, missingPaths, i})

      [] ->
        :ok
    end
  end

  defp vvars_assert_phi_vars(phis, i, id, r_vvars(blocks: blocks, branch_def_vars: branchDefVars)) do
    vars =
      for {r_b_var() = var, from} <- phis do
        {var, from}
      end

    foreach(
      fn {var, from} ->
        branchKey = {from, id}

        case branchDefVars do
          %{^branchKey => defVars} ->
            case :gb_sets.is_member(var, defVars) do
              true ->
                :ok

              false ->
                throw({:unknown_variable, var, i})
            end

          %{} ->
            throw({:unknown_phi_variable, var, branchKey, i})
        end
      end,
      vars
    )

    labels =
      for {r_b_literal(), from} <- phis do
        from
      end

    foreach(
      fn label ->
        case blocks do
          %{^label => _} ->
            :ok

          %{} ->
            throw({:undefined_label_in_phi, label, i})
        end
      end,
      labels
    )
  end

  defp vvars_block(id, state0) do
    %{^id => r_b_blk(is: is, last: terminator)} = r_vvars(state0, :blocks)
    %{^id => defVars} = r_vvars(state0, :branch_def_vars)
    validate_normalized(terminator)
    state = r_vvars(state0, defined_vars: defVars)
    vvars_terminator(terminator, id, vvars_block_1(is, terminator, state))
  end

  defp validate_normalized(i) do
    case :beam_ssa.normalize(i) do
      ^i ->
        :ok

      _ ->
        throw({:not_normalized, i})
    end
  end

  defp vvars_block_1(
         [
           r_b_set(dst: opVar, args: opArgs) = i,
           r_b_set(op: {:succeeded, kind}, args: [opVar], dst: succVar)
         ],
         terminator,
         state
       ) do
    true = kind === :guard or kind === :body

    case terminator do
      r_b_br(bool: r_b_var()) ->
        :ok = vvars_assert_args(opArgs, i, state)
        vvars_save_var(succVar, vvars_save_var(opVar, state))

      _ when kind === :body ->
        :ok = vvars_assert_args(opArgs, i, state)
        vvars_save_var(succVar, vvars_save_var(opVar, state))

      _ ->
        throw({:succeeded_not_followed_by_two_way_br, i})
    end
  end

  defp vvars_block_1(
         [
           [r_b_set(op: {:succeeded, :guard}, args: args) = i, _]
           | _
         ],
         _Terminator,
         state
       ) do
    :ok = vvars_assert_args(args, i, state)
    throw({:succeeded_not_last, i})
  end

  defp vvars_block_1([r_b_set(op: {:succeeded, _}, args: args) = i], _Terminator, state) do
    :ok = vvars_assert_args(args, i, state)
    throw({:succeeded_not_preceded, i})
  end

  defp vvars_block_1([r_b_set(dst: dst, op: :phi) | is], terminator, state) do
    vvars_block_1(is, terminator, vvars_save_var(dst, state))
  end

  defp vvars_block_1([r_b_set(dst: dst, args: args) = i | is], terminator, state) do
    :ok = vvars_assert_args(args, i, state)
    vvars_block_1(is, terminator, vvars_save_var(dst, state))
  end

  defp vvars_block_1([], _Terminator, state) do
    state
  end

  defp vvars_terminator(r_b_ret(arg: arg) = i, _From, state) do
    :ok = vvars_assert_args([arg], i, state)
    state
  end

  defp vvars_terminator(r_b_switch(arg: arg, fail: fail, list: switch) = i, from, state) do
    :ok = vvars_assert_args([arg], i, state)

    :ok =
      vvars_assert_args(
        for {a, _Lbl} <- switch do
          a
        end,
        i,
        state
      )

    labels = [
      fail
      | for {_Arg, lbl} <- switch do
          lbl
        end
    ]

    :ok = vvars_assert_labels(labels, i, state)
    vvars_terminator_1(labels, from, state)
  end

  defp vvars_terminator(r_b_br(bool: r_b_literal(val: true), succ: succ) = i, from, state) do
    labels = [succ]
    :ok = vvars_assert_labels(labels, i, state)
    vvars_terminator_1(labels, from, state)
  end

  defp vvars_terminator(r_b_br(bool: r_b_literal(val: false), fail: fail) = i, from, state) do
    labels = [fail]
    :ok = vvars_assert_labels(labels, i, state)
    vvars_terminator_1(labels, from, state)
  end

  defp vvars_terminator(r_b_br(bool: arg, succ: succ, fail: fail) = i, from, state) do
    :ok = vvars_assert_args([arg], i, state)
    labels = [fail, succ]
    :ok = vvars_assert_labels(labels, i, state)
    vvars_terminator_1(labels, from, state)
  end

  defp vvars_terminator_1(labels0, from, state0) do
    labels =
      for to <- labels0,
          not :maps.is_key({from, to}, r_vvars(state0, :branch_def_vars)) do
        to
      end

    true = labels === labels0 or labels === []

    state1 =
      foldl(
        fn to, state ->
          vvars_save_branch(from, to, state)
        end,
        state0,
        labels
      )

    foldl(
      fn to, state ->
        vvars_block(to, state)
      end,
      state1,
      labels
    )
  end

  defp vvars_get_variables(args) do
    for r_b_var() = var <- args do
      var
    end
  end

  defp vvars_assert_args(args, i, r_vvars(defined_vars: defVars) = state) do
    foreach(
      fn
        r_b_remote(mod: mod, name: name) ->
          vvars_assert_args([mod, name], i, state)

        r_b_var() = var ->
          case :gb_sets.is_member(var, defVars) do
            true ->
              :ok

            false ->
              throw({:unknown_variable, var, i})
          end

        _ ->
          :ok
      end,
      args
    )
  end

  defp vvars_assert_labels(labels, i, r_vvars(blocks: blocks)) do
    foreach(
      fn label ->
        case :maps.is_key(label, blocks) do
          false ->
            throw({:unknown_block, label, i})

          true ->
            :ok
        end
      end,
      labels
    )
  end

  defp vvars_save_branch(from, to, state) do
    defVars = r_vvars(state, :defined_vars)
    branches0 = r_vvars(state, :branch_def_vars)

    case branches0 do
      %{^to => lblDefVars} ->
        mergedVars = vvars_merge_branches(defVars, lblDefVars)
        branches = %{branches0 | to => mergedVars, {from, to} => defVars}
        r_vvars(state, branch_def_vars: branches)

      _ ->
        branches = %{branches0 | to => defVars, {from, to} => defVars}
        r_vvars(state, branch_def_vars: branches)
    end
  end

  defp vvars_merge_branches(new, existing) do
    :gb_sets.intersection(new, existing)
  end

  defp vvars_save_var(var, state0) do
    defVars = :gb_sets.insert(var, r_vvars(state0, :defined_vars))
    r_vvars(state0, defined_vars: defVars)
  end

  defp format_error_1({:redefined_variable, name, old, i}) do
    :io_lib.format(
      'Variable ~ts (~ts) redefined by ~ts',
      [format_var(name), format_instr(old), format_instr(i)]
    )
  end

  defp format_error_1({:missing_phi_paths, paths, i}) do
    :io_lib.format('Phi node ~ts doesn\'t define a value for these branches: ~w', [
      format_instr(i),
      paths
    ])
  end

  defp format_error_1({:garbage_phi_paths, paths, i}) do
    :io_lib.format(
      'Phi node ~ts defines a value for these unreachable or non-existent branches: ~w',
      [format_instr(i), paths]
    )
  end

  defp format_error_1({:unknown_phi_variable, name, {from, _To}, i}) do
    :io_lib.format(
      'Variable ~ts used in phi node ~ts is undefined on branch ~w',
      [format_var(name), format_instr(i), from]
    )
  end

  defp format_error_1({:unknown_block, label, i}) do
    :io_lib.format('Unknown block ~p referenced in ~ts', [label, i])
  end

  defp format_error_1({:unknown_variable, name, i}) do
    :io_lib.format('Unbound variable ~ts used in ~ts', [format_var(name), format_instr(i)])
  end

  defp format_error_1({:phi_inside_block, name, id}) do
    :io_lib.format('Phi node defining ~ts is not at start of block ~p', [format_var(name), id])
  end

  defp format_error_1({:undefined_label_in_phi, label, i}) do
    :io_lib.format('Unknown block label ~p in phi node ~ts', [label, format_instr(i)])
  end

  defp format_error_1({:succeeded_not_preceded, i}) do
    :io_lib.format('~ts does not reference the preceding instruction', [format_instr(i)])
  end

  defp format_error_1({:succeeded_not_last, i}) do
    :io_lib.format('~ts is not the last instruction in its block', [format_instr(i)])
  end

  defp format_error_1({:not_normalized, i}) do
    :io_lib.format('~ts is not normalized by beam_ssa:normalize/1', [format_instr(i)])
  end

  defp format_error_1({:succeeded_not_followed_by_two_way_br, i}) do
    :io_lib.format('~ts not followed by a two-way branch', [format_instr(i)])
  end
end
