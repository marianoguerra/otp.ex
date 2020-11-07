defmodule :m_core_lib do
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

  def make_values([e]) do
    e
  end

  def make_values([h | _] = es) do
    r_c_values(anno: :cerl.get_ann(h), es: es)
  end

  def make_values([]) do
    r_c_values(es: [])
  end

  def make_values(e) do
    e
  end

  def is_var_used(v, b) do
    vu_expr(v, b)
  end

  defp vu_expr(v, r_c_values(es: es)) do
    vu_expr_list(v, es)
  end

  defp vu_expr(v, r_c_var(name: v2)) do
    v === v2
  end

  defp vu_expr(_, r_c_literal()) do
    false
  end

  defp vu_expr(v, r_c_cons(hd: h, tl: t)) do
    vu_expr(v, h) or vu_expr(v, t)
  end

  defp vu_expr(v, r_c_tuple(es: es)) do
    vu_expr_list(v, es)
  end

  defp vu_expr(v, r_c_map(arg: m, es: es)) do
    vu_expr(v, m) or vu_expr_list(v, es)
  end

  defp vu_expr(v, r_c_map_pair(key: key, val: val)) do
    vu_expr_list(v, [key, val])
  end

  defp vu_expr(v, r_c_binary(segments: ss)) do
    vu_seg_list(v, ss)
  end

  defp vu_expr(v, r_c_fun(vars: vs, body: b)) do
    case vu_var_list(v, vs) do
      true ->
        false

      false ->
        vu_expr(v, b)
    end
  end

  defp vu_expr(v, r_c_let(vars: vs, arg: arg, body: b)) do
    case vu_expr(v, arg) do
      true ->
        true

      false ->
        case vu_var_list(v, vs) do
          true ->
            false

          false ->
            vu_expr(v, b)
        end
    end
  end

  defp vu_expr(v, r_c_letrec(defs: fs, body: b)) do
    :lists.any(
      fn {_, fb} ->
        vu_expr(v, fb)
      end,
      fs
    ) or vu_expr(v, b)
  end

  defp vu_expr(v, r_c_seq(arg: arg, body: b)) do
    vu_expr(v, arg) or vu_expr(v, b)
  end

  defp vu_expr(v, r_c_case(arg: arg, clauses: cs)) do
    vu_expr(v, arg) or vu_clauses(v, cs)
  end

  defp vu_expr(v, r_c_apply(op: op, args: as)) do
    vu_expr_list(v, [op | as])
  end

  defp vu_expr(v, r_c_call(module: m, name: n, args: as)) do
    vu_expr_list(v, [[m, n] | as])
  end

  defp vu_expr(v, r_c_primop(args: as)) do
    vu_expr_list(v, as)
  end

  defp vu_expr(v, r_c_catch(body: b)) do
    vu_expr(v, b)
  end

  defp vu_expr(
         v,
         r_c_try(arg: e, vars: vs, body: b, evars: evs, handler: h)
       ) do
    case vu_expr(v, e) do
      true ->
        true

      false ->
        case (case vu_var_list(v, vs) do
                true ->
                  false

                false ->
                  vu_expr(v, b)
              end) do
          true ->
            true

          false ->
            case vu_var_list(v, evs) do
              true ->
                false

              false ->
                vu_expr(v, h)
            end
        end
    end
  end

  defp vu_expr_list(v, es) do
    :lists.any(
      fn e ->
        vu_expr(v, e)
      end,
      es
    )
  end

  defp vu_seg_list(v, ss) do
    :lists.any(
      fn r_c_bitstr(val: val, size: size) ->
        vu_expr(v, val) or vu_expr(v, size)
      end,
      ss
    )
  end

  defp vu_clause(v, r_c_clause(pats: ps, guard: g, body: b)) do
    vu_pattern_list(v, ps) or vu_expr(v, g) or vu_expr(v, b)
  end

  defp vu_clauses(v, cs) do
    :lists.any(
      fn c ->
        vu_clause(v, c)
      end,
      cs
    )
  end

  defp vu_pattern(v, r_c_var(name: v2)) do
    v === v2
  end

  defp vu_pattern(v, r_c_cons(hd: h, tl: t)) do
    vu_pattern(v, h) or vu_pattern(v, t)
  end

  defp vu_pattern(v, r_c_tuple(es: es)) do
    vu_pattern_list(v, es)
  end

  defp vu_pattern(v, r_c_binary(segments: ss)) do
    vu_pat_seg_list(v, ss)
  end

  defp vu_pattern(v, r_c_map(es: es)) do
    vu_map_pairs(v, es)
  end

  defp vu_pattern(v, r_c_alias(var: var, pat: p)) do
    vu_pattern(v, var) or vu_pattern(v, p)
  end

  defp vu_pattern(_V, r_c_literal()) do
    false
  end

  defp vu_pattern_list(v, ps) do
    :lists.any(
      fn p ->
        vu_pattern(v, p)
      end,
      ps
    )
  end

  defp vu_pat_seg_list(v, ss) do
    :lists.any(
      fn r_c_bitstr(size: size) ->
        vu_pattern(v, size)
      end,
      ss
    )
  end

  defp vu_map_pairs(v, [r_c_map_pair(key: key, val: pat) | t]) do
    vu_expr(v, key) or vu_pattern(v, pat) or
      vu_map_pairs(
        v,
        t
      )
  end

  defp vu_map_pairs(_, []) do
    false
  end

  defp vu_var_list(v, vs) do
    :lists.any(
      fn r_c_var(name: v2) ->
        v === v2
      end,
      vs
    )
  end
end
