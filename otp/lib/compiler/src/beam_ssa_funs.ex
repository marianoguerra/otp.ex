defmodule :m_beam_ssa_funs do
  use Bitwise
  import :lists, only: [foldl: 3]
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

  def module(r_b_module(body: fs0) = module, _Opts) do
    trampolines = foldl(&find_trampolines/2, %{}, fs0)

    fs =
      for f <- fs0 do
        lfo(f, trampolines)
      end

    {:ok, r_b_module(module, body: fs)}
  end

  defp find_trampolines(r_b_function(args: args, bs: blocks) = f, trampolines) do
    case :erlang.map_get(0, blocks) do
      r_b_blk(
        is: [r_b_set(op: :call, args: [r_b_local() = actual | args], dst: dst)],
        last: r_b_ret(arg: dst)
      ) ->
        {_, name, arity} = :beam_ssa.get_anno(:func_info, f)
        trampoline = r_b_local(name: r_b_literal(val: name), arity: arity)
        %{trampolines | trampoline => actual}

      _ ->
        trampolines
    end
  end

  defp lfo(r_b_function(bs: blocks0) = f, trampolines) do
    linear0 = :beam_ssa.linearize(blocks0)
    linear = lfo_optimize(linear0, lfo_analyze(linear0, %{}), trampolines)
    r_b_function(f, bs: :maps.from_list(linear))
  end

  defp lfo_analyze([{_L, r_b_blk(is: is, last: last)} | bs], lFuns0) do
    lFuns =
      lfo_analyze_last(
        last,
        lfo_analyze_is(is, lFuns0)
      )

    lfo_analyze(bs, lFuns)
  end

  defp lfo_analyze([], lFuns) do
    lFuns
  end

  defp lfo_analyze_is(
         [
           r_b_set(op: :make_fun, dst: dst, args: [r_b_local() | freeVars]) = def__
           | is
         ],
         lFuns0
       ) do
    lFuns = :maps.put(dst, def__, :maps.without(freeVars, lFuns0))
    lfo_analyze_is(is, lFuns)
  end

  defp lfo_analyze_is(
         [r_b_set(op: :call, args: [fun | callArgs]) | is],
         lFuns
       )
       when :erlang.is_map_key(fun, lFuns) do
    r_b_set(
      args: [
        r_b_local(arity: arity)
        | freeVars
      ]
    ) = :erlang.map_get(fun, lFuns)

    case length(callArgs) + length(freeVars) do
      ^arity ->
        lfo_analyze_is(is, :maps.without(callArgs, lFuns))

      _ ->
        lfo_analyze_is(
          is,
          :maps.without([fun | callArgs], lFuns)
        )
    end
  end

  defp lfo_analyze_is([r_b_set(args: args) | is], lFuns)
       when map_size(lFuns) !== 0 do
    lfo_analyze_is(is, :maps.without(args, lFuns))
  end

  defp lfo_analyze_is([_ | is], lFuns) do
    lfo_analyze_is(is, lFuns)
  end

  defp lfo_analyze_is([], lFuns) do
    lFuns
  end

  defp lfo_analyze_last(r_b_switch(arg: arg), lFuns) do
    :maps.remove(arg, lFuns)
  end

  defp lfo_analyze_last(r_b_ret(arg: arg), lFuns) do
    :maps.remove(arg, lFuns)
  end

  defp lfo_analyze_last(_, lFuns) do
    lFuns
  end

  defp lfo_optimize(linear, lFuns, _Trampolines)
       when map_size(lFuns) === 0 do
    linear
  end

  defp lfo_optimize(linear, lFuns, trampolines) do
    lfo_optimize_1(linear, lFuns, trampolines)
  end

  defp lfo_optimize_1([{l, r_b_blk(is: is0) = blk} | bs], lFuns, trampolines) do
    is = lfo_optimize_is(is0, lFuns, trampolines)
    [{l, r_b_blk(blk, is: is)} | lfo_optimize_1(bs, lFuns, trampolines)]
  end

  defp lfo_optimize_1([], _LFuns, _Trampolines) do
    []
  end

  defp lfo_optimize_is(
         [
           r_b_set(op: :call, args: [fun | callArgs]) = call0
           | is
         ],
         lFuns,
         trampolines
       )
       when :erlang.is_map_key(fun, lFuns) do
    r_b_set(args: [local | freeVars]) =
      :erlang.map_get(
        fun,
        lFuns
      )

    args = [
      lfo_short_circuit(local, trampolines)
      | callArgs ++ freeVars
    ]

    call = :beam_ssa.add_anno(:local_fun_opt, fun, r_b_set(call0, args: args))
    [call | lfo_optimize_is(is, lFuns, trampolines)]
  end

  defp lfo_optimize_is([i | is], lFuns, trampolines) do
    [i | lfo_optimize_is(is, lFuns, trampolines)]
  end

  defp lfo_optimize_is([], _LFuns, _Trampolines) do
    []
  end

  defp lfo_short_circuit(call, trampolines) do
    lfo_short_circuit(call, trampolines, :cerl_sets.new())
  end

  defp lfo_short_circuit(call, trampolines, seen0) do
    case :cerl_sets.is_element(call, seen0) do
      true ->
        call

      false ->
        case trampolines do
          %{^call => other} ->
            seen = :cerl_sets.add_element(call, seen0)
            lfo_short_circuit(other, trampolines, seen)

          %{} ->
            call
        end
    end
  end
end
