defmodule :m_sys_core_inline do
  use Bitwise
  import :lists, only: [foldl: 3, map: 2, mapfoldl: 3, member: 2]
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
  Record.defrecord(:r_inline, :inline, exports: [], thresh: 0, inline: [])

  Record.defrecord(:r_fstat, :fstat,
    func: :undefined,
    arity: :undefined,
    def: :undefined,
    weight: 0,
    inline: false,
    modified: false
  )

  Record.defrecord(:r_ifun, :ifun,
    func: :undefined,
    arity: :undefined,
    vars: :undefined,
    body: :undefined,
    weight: :undefined
  )

  def module(r_c_module(exports: es, defs: ds0) = mod, opts) do
    case inline_option(opts) do
      {thresh, fs}
      when (is_integer(thresh) and thresh > 0) or
             fs !== [] ->
        case :proplists.get_bool(:verbose, opts) do
          true ->
            :io.format('Old inliner: threshold=~p functions=~p\n', [thresh, fs])

          false ->
            :ok
        end

        ds1 =
          inline(
            ds0,
            r_inline(exports: es, thresh: thresh, inline: fs)
          )

        {:ok, r_c_module(mod, defs: ds1)}

      _Other ->
        {:ok, mod}
    end
  end

  defp inline_option(opts) do
    foldl(
      fn
        {:inline, {_, _} = val}, {t, fs} ->
          {t, [val | fs]}

        {:inline, val}, {t, fs} when is_list(val) ->
          {t, val ++ fs}

        {:inline, val}, {_, fs} when is_integer(val) ->
          {val, fs}

        _Opt, {_, _} = def__ ->
          def__
      end,
      {0, []},
      opts
    )
  end

  defp inline(fs0, st0) do
    fs1 =
      map(
        fn {r_c_var(name: {f, a}), r_c_fun(body: b)} = def__ ->
          weight = :cerl_trees.fold(&weight_func/2, 0, b)
          r_fstat(func: f, arity: a, def: def__, weight: weight)
        end,
        fs0
      )

    {fs2, is0} =
      mapfoldl(
        fn fst, ifs ->
          case is_inlineable(fst, r_inline(st0, :thresh), r_inline(st0, :inline)) do
            true ->
              {_, ffun} = r_fstat(fst, :def)

              if__ =
                r_ifun(
                  func: r_fstat(fst, :func),
                  arity: r_fstat(fst, :arity),
                  vars: r_c_fun(ffun, :vars),
                  body: r_c_fun(ffun, :body),
                  weight: r_fstat(fst, :weight)
                )

              {r_fstat(fst, inline: true), [if__ | ifs]}

            false ->
              {fst, ifs}
          end
        end,
        [],
        fs1
      )

    is1 =
      for if__ <- is0 do
        inline_inline(if__, is0)
      end

    fs =
      for f <- fs2 do
        inline_func(f, is1)
      end

    for r_fstat(def: def__) <- fs do
      def__
    end
  end

  defp is_inlineable(r_fstat(weight: w), thresh, _Ofs) when w <= thresh do
    true
  end

  defp is_inlineable(r_fstat(func: f, arity: a), _Thresh, ofs) do
    member({f, a}, ofs)
  end

  defp inline_inline(r_ifun(body: b, weight: iw) = if__, is) do
    inline = fn
      r_c_apply(op: r_c_var(name: {f, a}), args: as) = call ->
        case find_inl(f, a, is) do
          r_ifun(vars: vs, body: b2, weight: w) when w < iw ->
            r_c_let(
              vars: vs,
              arg: kill_id_anns(:core_lib.make_values(as)),
              body: kill_id_anns(b2)
            )

          _Other ->
            call
        end

      core ->
        core
    end

    r_ifun(if__, body: :cerl_trees.map(inline, b))
  end

  defp inline_func(r_fstat(def: {name, f0}) = fstat, is) do
    inline = fn
      r_c_apply(op: r_c_var(name: {f, a}), args: as) = call, mod ->
        case find_inl(f, a, is) do
          r_ifun(vars: vs, body: b) ->
            {r_c_let(
               vars: vs,
               arg: kill_id_anns(:core_lib.make_values(as)),
               body: kill_id_anns(b)
             ), true}

          _Other ->
            {call, mod}
        end

      core, mod ->
        {core, mod}
    end

    {f1, mod} = :cerl_trees.mapfold(inline, false, f0)
    r_fstat(fstat, def: {name, f1}, modified: mod)
  end

  defp weight_func(_Core, acc) do
    acc + 1
  end

  defp find_inl(f, a, [r_ifun(func: f, arity: a) = if__ | _]) do
    if__
  end

  defp find_inl(f, a, [_ | is]) do
    find_inl(f, a, is)
  end

  defp find_inl(_, _, []) do
    :no
  end

  defp kill_id_anns(body) do
    :cerl_trees.map(
      fn
        r_c_fun(anno: a0) = cFun ->
          a = kill_id_anns_1(a0)
          r_c_fun(cFun, anno: a)

        r_c_var(anno: a0) = var ->
          a = kill_id_anns_1(a0)
          r_c_var(var, anno: a)

        expr ->
          a = compiler_generated(:cerl.get_ann(expr))
          :cerl.set_ann(expr, a)
      end,
      body
    )
  end

  defp kill_id_anns_1([{:id, _} | as]) do
    kill_id_anns_1(as)
  end

  defp kill_id_anns_1([a | as]) do
    [a | kill_id_anns_1(as)]
  end

  defp kill_id_anns_1([]) do
    []
  end

  defp compiler_generated([:compiler_generated | _] = anno) do
    anno
  end

  defp compiler_generated(anno) do
    [:compiler_generated | anno -- [:compiler_generated]]
  end
end
