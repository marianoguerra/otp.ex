defmodule :m_sys_core_bsm do
  use Bitwise
  require Record
  Record.defrecord(:r_c_alias, :c_alias, anno: [], var: :undefined, pat: :undefined)
  Record.defrecord(:r_c_apply, :c_apply, anno: [], op: :undefined, args: :undefined)

  Record.defrecord(:r_c_binary, :c_binary,
    anno: [],
    segments: :undefined
  )

  Record.defrecord(:r_c_bitstr, :c_bitstr,
    anno: [],
    val: :undefined,
    size: :undefined,
    unit: :undefined,
    type: :undefined,
    flags: :undefined
  )

  Record.defrecord(:r_c_call, :c_call,
    anno: [],
    module: :undefined,
    name: :undefined,
    args: :undefined
  )

  Record.defrecord(:r_c_case, :c_case, anno: [], arg: :undefined, clauses: :undefined)

  Record.defrecord(:r_c_catch, :c_catch,
    anno: [],
    body: :undefined
  )

  Record.defrecord(:r_c_clause, :c_clause,
    anno: [],
    pats: :undefined,
    guard: :undefined,
    body: :undefined
  )

  Record.defrecord(:r_c_cons, :c_cons, anno: [], hd: :undefined, tl: :undefined)
  Record.defrecord(:r_c_fun, :c_fun, anno: [], vars: :undefined, body: :undefined)

  Record.defrecord(:r_c_let, :c_let, anno: [], vars: :undefined, arg: :undefined, body: :undefined)

  Record.defrecord(:r_c_letrec, :c_letrec, anno: [], defs: :undefined, body: :undefined)

  Record.defrecord(:r_c_literal, :c_literal,
    anno: [],
    val: :undefined
  )

  Record.defrecord(:r_c_map, :c_map,
    anno: [],
    arg: :EFE_TODO_NESTED_RECORD,
    es: :undefined,
    is_pat: false
  )

  Record.defrecord(:r_c_map_pair, :c_map_pair,
    anno: [],
    op: :undefined,
    key: :undefined,
    val: :undefined
  )

  Record.defrecord(:r_c_module, :c_module,
    anno: [],
    name: :undefined,
    exports: :undefined,
    attrs: :undefined,
    defs: :undefined
  )

  Record.defrecord(:r_c_primop, :c_primop, anno: [], name: :undefined, args: :undefined)

  Record.defrecord(:r_c_receive, :c_receive,
    anno: [],
    clauses: :undefined,
    timeout: :undefined,
    action: :undefined
  )

  Record.defrecord(:r_c_seq, :c_seq, anno: [], arg: :undefined, body: :undefined)

  Record.defrecord(:r_c_try, :c_try,
    anno: [],
    arg: :undefined,
    vars: :undefined,
    body: :undefined,
    evars: :undefined,
    handler: :undefined
  )

  Record.defrecord(:r_c_tuple, :c_tuple, anno: [], es: :undefined)

  Record.defrecord(:r_c_values, :c_values,
    anno: [],
    es: :undefined
  )

  Record.defrecord(:r_c_var, :c_var, anno: [], name: :undefined)

  def module(r_c_module(defs: ds) = mod, _Opts) do
    {:ok, r_c_module(mod, defs: function(ds))}
  end

  defp function([{r_c_var(name: {f, arity}) = name, b0} | fs]) do
    try do
      :cerl_trees.map(&bsm_reorder/1, b0)
    catch
      class, error ->
        :io.fwrite('Function: ~w/~w\n', [f, arity])
        :erlang.raise(class, error, __STACKTRACE__)
    else
      b ->
        [{name, b} | function(fs)]
    end
  end

  defp function([]) do
    []
  end

  defp bsm_reorder(r_c_case(arg: r_c_var() = v) = case__) do
    bsm_reorder_1([v], case__)
  end

  defp bsm_reorder(r_c_case(arg: r_c_values(es: es)) = case__) do
    bsm_reorder_1(es, case__)
  end

  defp bsm_reorder(core) do
    core
  end

  defp bsm_reorder_1(vs0, r_c_case(clauses: cs0) = case__) do
    case bsm_leftmost(cs0) do
      pos when pos > 0 and pos !== :none ->
        vs = :core_lib.make_values(move_from_col(pos, vs0))

        cs =
          for r_c_clause(pats: ps) = c <- cs0 do
            r_c_clause(c, pats: move_from_col(pos, ps))
          end

        r_c_case(case__, arg: vs, clauses: cs)

      _ ->
        case__
    end
  end

  defp move_from_col(pos, l) do
    {first, [col | rest]} = :lists.split(pos - 1, l)
    [col | first] ++ rest
  end

  defp bsm_leftmost(cs) do
    bsm_leftmost_1(cs, :none)
  end

  defp bsm_leftmost_1([_ | _], 1) do
    1
  end

  defp bsm_leftmost_1([r_c_clause(pats: ps) | cs], pos) do
    bsm_leftmost_2(ps, cs, 1, pos)
  end

  defp bsm_leftmost_1([], pos) do
    pos
  end

  defp bsm_leftmost_2(_, cs, pos, pos) do
    bsm_leftmost_1(cs, pos)
  end

  defp bsm_leftmost_2([r_c_binary(segments: [_ | _]) | _], cs, n, _) do
    bsm_leftmost_1(cs, n)
  end

  defp bsm_leftmost_2([_ | ps], cs, n, pos) do
    bsm_leftmost_2(ps, cs, n + 1, pos)
  end

  defp bsm_leftmost_2([], cs, _, pos) do
    bsm_leftmost_1(cs, pos)
  end
end
