defmodule :m_sys_core_fold_lists do
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

  def call(r_c_call(anno: anno), :lists, :all, [arg1, arg2]) do
    loop = r_c_var(name: {:"lists^all", 1})
    f = r_c_var(name: :F)
    xs = r_c_var(name: :Xs)
    x = r_c_var(name: :X)
    err1 = r_c_tuple(es: [r_c_literal(val: :case_clause), x])

    cC1 =
      r_c_clause(
        anno: anno,
        pats: [r_c_literal(val: true)],
        guard: r_c_literal(val: true),
        body: r_c_apply(anno: anno, op: loop, args: [xs])
      )

    cC2 =
      r_c_clause(
        anno: anno,
        pats: [r_c_literal(val: false)],
        guard: r_c_literal(val: true),
        body: r_c_literal(val: false)
      )

    cC3 =
      r_c_clause(
        anno: anno,
        pats: [x],
        guard: r_c_literal(val: true),
        body: match_fail(anno, err1)
      )

    c1 =
      r_c_clause(
        anno: anno,
        pats: [r_c_cons(hd: x, tl: xs)],
        guard: r_c_literal(val: true),
        body:
          r_c_case(
            arg: r_c_apply(anno: anno, op: f, args: [x]),
            clauses: [cC1, cC2, cC3]
          )
      )

    c2 =
      r_c_clause(
        anno: anno,
        pats: [r_c_literal(val: [])],
        guard:
          r_c_call(
            module: r_c_literal(val: :erlang),
            name: r_c_literal(val: :is_function),
            args: [f, r_c_literal(val: 1)]
          ),
        body: r_c_literal(val: true)
      )

    c3 =
      r_c_clause(
        anno: anno,
        pats: [xs],
        guard: r_c_literal(val: true),
        body: function_clause(anno, [f, xs])
      )

    fun =
      r_c_fun(
        vars: [xs],
        body: r_c_case(arg: xs, clauses: [c1, c2, c3])
      )

    l = r_c_var(name: :L)

    r_c_let(
      vars: [f, l],
      arg: r_c_values(es: [arg1, arg2]),
      body:
        r_c_letrec(
          defs: [{loop, fun}],
          body: r_c_apply(anno: anno, op: loop, args: [l])
        )
    )
  end

  def call(r_c_call(anno: anno), :lists, :any, [arg1, arg2]) do
    loop = r_c_var(name: {:"lists^any", 1})
    f = r_c_var(name: :F)
    xs = r_c_var(name: :Xs)
    x = r_c_var(name: :X)
    err1 = r_c_tuple(es: [r_c_literal(val: :case_clause), x])

    cC1 =
      r_c_clause(
        anno: anno,
        pats: [r_c_literal(val: true)],
        guard: r_c_literal(val: true),
        body: r_c_literal(val: true)
      )

    cC2 =
      r_c_clause(
        anno: anno,
        pats: [r_c_literal(val: false)],
        guard: r_c_literal(val: true),
        body: r_c_apply(anno: anno, op: loop, args: [xs])
      )

    cC3 =
      r_c_clause(
        anno: anno,
        pats: [x],
        guard: r_c_literal(val: true),
        body: match_fail(anno, err1)
      )

    c1 =
      r_c_clause(
        anno: anno,
        pats: [r_c_cons(hd: x, tl: xs)],
        guard: r_c_literal(val: true),
        body:
          r_c_case(
            arg: r_c_apply(anno: anno, op: f, args: [x]),
            clauses: [cC1, cC2, cC3]
          )
      )

    c2 =
      r_c_clause(
        anno: anno,
        pats: [r_c_literal(val: [])],
        guard:
          r_c_call(
            module: r_c_literal(val: :erlang),
            name: r_c_literal(val: :is_function),
            args: [f, r_c_literal(val: 1)]
          ),
        body: r_c_literal(val: false)
      )

    c3 =
      r_c_clause(pats: [xs], guard: r_c_literal(val: true), body: function_clause(anno, [f, xs]))

    fun =
      r_c_fun(
        vars: [xs],
        body: r_c_case(arg: xs, clauses: [c1, c2, c3])
      )

    l = r_c_var(name: :L)

    r_c_let(
      vars: [f, l],
      arg: r_c_values(es: [arg1, arg2]),
      body:
        r_c_letrec(
          defs: [{loop, fun}],
          body: r_c_apply(anno: anno, op: loop, args: [l])
        )
    )
  end

  def call(r_c_call(anno: anno), :lists, :foreach, [arg1, arg2]) do
    loop = r_c_var(name: {:"lists^foreach", 1})
    f = r_c_var(name: :F)
    xs = r_c_var(name: :Xs)
    x = r_c_var(name: :X)

    c1 =
      r_c_clause(
        anno: anno,
        pats: [r_c_cons(hd: x, tl: xs)],
        guard: r_c_literal(val: true),
        body:
          r_c_seq(
            arg: r_c_apply(anno: anno, op: f, args: [x]),
            body: r_c_apply(anno: anno, op: loop, args: [xs])
          )
      )

    c2 =
      r_c_clause(
        anno: anno,
        pats: [r_c_literal(val: [])],
        guard:
          r_c_call(
            module: r_c_literal(val: :erlang),
            name: r_c_literal(val: :is_function),
            args: [f, r_c_literal(val: 1)]
          ),
        body: r_c_literal(val: :ok)
      )

    c3 =
      r_c_clause(
        anno: anno,
        pats: [xs],
        guard: r_c_literal(val: true),
        body: function_clause(anno, [f, xs])
      )

    fun =
      r_c_fun(
        vars: [xs],
        body: r_c_case(arg: xs, clauses: [c1, c2, c3])
      )

    l = r_c_var(name: :L)

    r_c_let(
      vars: [f, l],
      arg: r_c_values(es: [arg1, arg2]),
      body:
        r_c_letrec(
          defs: [{loop, fun}],
          body: r_c_apply(anno: anno, op: loop, args: [l])
        )
    )
  end

  def call(r_c_call(anno: anno), :lists, :map, [arg1, arg2]) do
    loop = r_c_var(name: {:"lists^map", 1})
    f = r_c_var(name: :F)
    xs = r_c_var(name: :Xs)
    x = r_c_var(name: :X)
    h = r_c_var(name: :H)

    c1 =
      r_c_clause(
        anno: anno,
        pats: [r_c_cons(hd: x, tl: xs)],
        guard: r_c_literal(val: true),
        body:
          r_c_let(
            vars: [h],
            arg: r_c_apply(anno: anno, op: f, args: [x]),
            body:
              r_c_cons(
                hd: h,
                anno: [:compiler_generated],
                tl: r_c_apply(anno: anno, op: loop, args: [xs])
              )
          )
      )

    c2 =
      r_c_clause(
        anno: anno,
        pats: [r_c_literal(val: [])],
        guard:
          r_c_call(
            module: r_c_literal(val: :erlang),
            name: r_c_literal(val: :is_function),
            args: [f, r_c_literal(val: 1)]
          ),
        body: r_c_literal(val: [])
      )

    c3 =
      r_c_clause(pats: [xs], guard: r_c_literal(val: true), body: function_clause(anno, [f, xs]))

    fun =
      r_c_fun(
        vars: [xs],
        body: r_c_case(arg: xs, clauses: [c1, c2, c3])
      )

    l = r_c_var(name: :L)

    r_c_let(
      vars: [f, l],
      arg: r_c_values(es: [arg1, arg2]),
      body:
        r_c_letrec(
          defs: [{loop, fun}],
          body: r_c_apply(anno: anno, op: loop, args: [l])
        )
    )
  end

  def call(r_c_call(anno: anno), :lists, :flatmap, [arg1, arg2]) do
    loop = r_c_var(name: {:"lists^flatmap", 1})
    f = r_c_var(name: :F)
    xs = r_c_var(name: :Xs)
    x = r_c_var(name: :X)
    h = r_c_var(name: :H)

    c1 =
      r_c_clause(
        anno: anno,
        pats: [r_c_cons(hd: x, tl: xs)],
        guard: r_c_literal(val: true),
        body:
          r_c_let(
            vars: [h],
            arg: r_c_apply(anno: anno, op: f, args: [x]),
            body:
              r_c_call(
                anno: [:compiler_generated | anno],
                module: r_c_literal(val: :erlang),
                name: r_c_literal(val: :++),
                args: [h, r_c_apply(anno: anno, op: loop, args: [xs])]
              )
          )
      )

    c2 =
      r_c_clause(
        anno: anno,
        pats: [r_c_literal(val: [])],
        guard:
          r_c_call(
            module: r_c_literal(val: :erlang),
            name: r_c_literal(val: :is_function),
            args: [f, r_c_literal(val: 1)]
          ),
        body: r_c_literal(val: [])
      )

    c3 =
      r_c_clause(
        anno: anno,
        pats: [xs],
        guard: r_c_literal(val: true),
        body: function_clause(anno, [f, xs])
      )

    fun =
      r_c_fun(
        vars: [xs],
        body: r_c_case(arg: xs, clauses: [c1, c2, c3])
      )

    l = r_c_var(name: :L)

    r_c_let(
      vars: [f, l],
      arg: r_c_values(es: [arg1, arg2]),
      body:
        r_c_letrec(
          defs: [{loop, fun}],
          body: r_c_apply(anno: anno, op: loop, args: [l])
        )
    )
  end

  def call(r_c_call(anno: anno), :lists, :filter, [arg1, arg2]) do
    loop = r_c_var(name: {:"lists^filter", 1})
    f = r_c_var(name: :F)
    xs = r_c_var(name: :Xs)
    x = r_c_var(name: :X)
    b = r_c_var(name: :B)
    err1 = r_c_tuple(es: [r_c_literal(val: :case_clause), x])

    cC1 =
      r_c_clause(
        anno: anno,
        pats: [r_c_literal(val: true)],
        guard: r_c_literal(val: true),
        body: r_c_cons(anno: [:compiler_generated], hd: x, tl: xs)
      )

    cC2 =
      r_c_clause(
        anno: anno,
        pats: [r_c_literal(val: false)],
        guard: r_c_literal(val: true),
        body: xs
      )

    cC3 =
      r_c_clause(
        anno: anno,
        pats: [x],
        guard: r_c_literal(val: true),
        body: match_fail(anno, err1)
      )

    case__ = r_c_case(arg: b, clauses: [cC1, cC2, cC3])

    c1 =
      r_c_clause(
        pats: [r_c_cons(hd: x, tl: xs)],
        guard: r_c_literal(val: true),
        body:
          r_c_let(
            vars: [b],
            arg: r_c_apply(anno: anno, op: f, args: [x]),
            body:
              r_c_let(
                vars: [xs],
                arg: r_c_apply(anno: anno, op: loop, args: [xs]),
                body: case__
              )
          )
      )

    c2 =
      r_c_clause(
        anno: anno,
        pats: [r_c_literal(val: [])],
        guard:
          r_c_call(
            module: r_c_literal(val: :erlang),
            name: r_c_literal(val: :is_function),
            args: [f, r_c_literal(val: 1)]
          ),
        body: r_c_literal(val: [])
      )

    c3 =
      r_c_clause(
        anno: anno,
        pats: [xs],
        guard: r_c_literal(val: true),
        body: function_clause(anno, [f, xs])
      )

    fun =
      r_c_fun(
        vars: [xs],
        body: r_c_case(arg: xs, clauses: [c1, c2, c3])
      )

    l = r_c_var(name: :L)

    r_c_let(
      vars: [f, l],
      arg: r_c_values(es: [arg1, arg2]),
      body:
        r_c_letrec(
          defs: [{loop, fun}],
          body: r_c_apply(anno: anno, op: loop, args: [l])
        )
    )
  end

  def call(r_c_call(anno: anno), :lists, :foldl, [arg1, arg2, arg3]) do
    loop = r_c_var(name: {:"lists^foldl", 2})
    f = r_c_var(name: :F)
    xs = r_c_var(name: :Xs)
    x = r_c_var(name: :X)
    a = r_c_var(name: :A)

    c1 =
      r_c_clause(
        anno: anno,
        pats: [r_c_cons(hd: x, tl: xs)],
        guard: r_c_literal(val: true),
        body:
          r_c_apply(anno: anno, op: loop, args: [xs, r_c_apply(anno: anno, op: f, args: [x, a])])
      )

    c2 =
      r_c_clause(
        anno: anno,
        pats: [r_c_literal(val: [])],
        guard:
          r_c_call(
            module: r_c_literal(val: :erlang),
            name: r_c_literal(val: :is_function),
            args: [f, r_c_literal(val: 2)]
          ),
        body: a
      )

    c3 =
      r_c_clause(
        anno: anno,
        pats: [xs],
        guard: r_c_literal(val: true),
        body: function_clause(anno, [f, a, xs])
      )

    fun =
      r_c_fun(
        vars: [xs, a],
        body: r_c_case(arg: xs, clauses: [c1, c2, c3])
      )

    l = r_c_var(name: :L)

    r_c_let(
      vars: [f, a, l],
      arg: r_c_values(es: [arg1, arg2, arg3]),
      body:
        r_c_letrec(
          defs: [{loop, fun}],
          body: r_c_apply(anno: anno, op: loop, args: [l, a])
        )
    )
  end

  def call(r_c_call(anno: anno), :lists, :foldr, [arg1, arg2, arg3]) do
    loop = r_c_var(name: {:"lists^foldr", 2})
    f = r_c_var(name: :F)
    xs = r_c_var(name: :Xs)
    x = r_c_var(name: :X)
    a = r_c_var(name: :A)

    c1 =
      r_c_clause(
        anno: anno,
        pats: [r_c_cons(hd: x, tl: xs)],
        guard: r_c_literal(val: true),
        body:
          r_c_apply(anno: anno, op: f, args: [x, r_c_apply(anno: anno, op: loop, args: [xs, a])])
      )

    c2 =
      r_c_clause(
        anno: anno,
        pats: [r_c_literal(val: [])],
        guard:
          r_c_call(
            module: r_c_literal(val: :erlang),
            name: r_c_literal(val: :is_function),
            args: [f, r_c_literal(val: 2)]
          ),
        body: a
      )

    c3 =
      r_c_clause(
        anno: anno,
        pats: [xs],
        guard: r_c_literal(val: true),
        body: function_clause(anno, [f, a, xs])
      )

    fun =
      r_c_fun(
        vars: [xs, a],
        body: r_c_case(arg: xs, clauses: [c1, c2, c3])
      )

    l = r_c_var(name: :L)

    r_c_let(
      vars: [f, a, l],
      arg: r_c_values(es: [arg1, arg2, arg3]),
      body:
        r_c_letrec(
          defs: [{loop, fun}],
          body: r_c_apply(anno: anno, op: loop, args: [l, a])
        )
    )
  end

  def call(r_c_call(anno: anno), :lists, :mapfoldl, [arg1, arg2, arg3]) do
    loop = r_c_var(name: {:"lists^mapfoldl", 2})
    f = r_c_var(name: :F)
    xs = r_c_var(name: :Xs)
    x = r_c_var(name: :X)
    avar = r_c_var(name: :A)

    match = fn a, p, e ->
      c1 = r_c_clause(anno: anno, pats: [p], guard: r_c_literal(val: true), body: e)
      err = r_c_tuple(es: [r_c_literal(val: :badmatch), x])

      c2 =
        r_c_clause(
          anno: anno,
          pats: [x],
          guard: r_c_literal(val: true),
          body: match_fail(anno, err)
        )

      r_c_case(arg: a, clauses: [c1, c2])
    end

    c1 =
      r_c_clause(
        anno: anno,
        pats: [r_c_cons(hd: x, tl: xs)],
        guard: r_c_literal(val: true),
        body:
          match.(
            r_c_apply(anno: anno, op: f, args: [x, avar]),
            r_c_tuple(es: [x, avar]),
            match.(
              r_c_apply(anno: anno, op: loop, args: [xs, avar]),
              r_c_tuple(es: [xs, avar]),
              r_c_tuple(
                anno: [:compiler_generated],
                es: [r_c_cons(anno: [:compiler_generated], hd: x, tl: xs), avar]
              )
            )
          )
      )

    c2 =
      r_c_clause(
        anno: anno,
        pats: [r_c_literal(val: [])],
        guard:
          r_c_call(
            module: r_c_literal(val: :erlang),
            name: r_c_literal(val: :is_function),
            args: [f, r_c_literal(val: 2)]
          ),
        body:
          r_c_tuple(
            anno: [:compiler_generated],
            es: [r_c_literal(val: []), avar]
          )
      )

    c3 =
      r_c_clause(
        anno: anno,
        pats: [xs],
        guard: r_c_literal(val: true),
        body: function_clause(anno, [f, avar, xs])
      )

    fun =
      r_c_fun(
        vars: [xs, avar],
        body: r_c_case(arg: xs, clauses: [c1, c2, c3])
      )

    l = r_c_var(name: :L)

    r_c_let(
      vars: [f, avar, l],
      arg: r_c_values(es: [arg1, arg2, arg3]),
      body:
        r_c_letrec(
          defs: [{loop, fun}],
          body: r_c_apply(anno: anno, op: loop, args: [l, avar])
        )
    )
  end

  def call(r_c_call(anno: anno), :lists, :mapfoldr, [arg1, arg2, arg3]) do
    loop = r_c_var(name: {:"lists^mapfoldr", 2})
    f = r_c_var(name: :F)
    xs = r_c_var(name: :Xs)
    x = r_c_var(name: :X)
    avar = r_c_var(name: :A)

    match = fn a, p, e ->
      c1 = r_c_clause(anno: anno, pats: [p], guard: r_c_literal(val: true), body: e)
      err = r_c_tuple(es: [r_c_literal(val: :badmatch), x])

      c2 =
        r_c_clause(
          anno: anno,
          pats: [x],
          guard: r_c_literal(val: true),
          body: match_fail(anno, err)
        )

      r_c_case(arg: a, clauses: [c1, c2])
    end

    c1 =
      r_c_clause(
        anno: anno,
        pats: [r_c_cons(hd: x, tl: xs)],
        guard: r_c_literal(val: true),
        body:
          match.(
            r_c_apply(anno: anno, op: loop, args: [xs, avar]),
            r_c_tuple(es: [xs, avar]),
            match.(
              r_c_apply(anno: anno, op: f, args: [x, avar]),
              r_c_tuple(es: [x, avar]),
              r_c_tuple(
                anno: [:compiler_generated],
                es: [r_c_cons(anno: [:compiler_generated], hd: x, tl: xs), avar]
              )
            )
          )
      )

    c2 =
      r_c_clause(
        anno: anno,
        pats: [r_c_literal(val: [])],
        guard:
          r_c_call(
            module: r_c_literal(val: :erlang),
            name: r_c_literal(val: :is_function),
            args: [f, r_c_literal(val: 2)]
          ),
        body:
          r_c_tuple(
            anno: [:compiler_generated],
            es: [r_c_literal(val: []), avar]
          )
      )

    c3 =
      r_c_clause(
        anno: anno,
        pats: [xs],
        guard: r_c_literal(val: true),
        body: function_clause(anno, [f, avar, xs])
      )

    fun =
      r_c_fun(
        vars: [xs, avar],
        body: r_c_case(arg: xs, clauses: [c1, c2, c3])
      )

    l = r_c_var(name: :L)

    r_c_let(
      vars: [f, avar, l],
      arg: r_c_values(es: [arg1, arg2, arg3]),
      body:
        r_c_letrec(
          defs: [{loop, fun}],
          body: r_c_apply(anno: anno, op: loop, args: [l, avar])
        )
    )
  end

  def call(_, _, _, _) do
    :none
  end

  defp match_fail(ann, arg) do
    name = :cerl.abstract(:match_fail)
    args = [arg]
    :cerl.ann_c_primop(ann, name, args)
  end

  defp function_clause(anno, args) do
    r_c_call(
      anno: anno,
      module: r_c_literal(val: :erlang),
      name: r_c_literal(val: :error),
      args: [r_c_literal(val: :function_clause), :cerl.ann_make_list(anno, args)]
    )
  end
end
