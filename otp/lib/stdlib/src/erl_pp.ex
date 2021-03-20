defmodule :m_erl_pp do
  use Bitwise

  import :erl_parse,
    only: [
      func_prec: 0,
      inop_prec: 1,
      max_prec: 0,
      preop_prec: 1,
      type_inop_prec: 1,
      type_preop_prec: 1
    ]

  import :io_lib, only: [format: 2, write: 1]
  import :lists, only: [append: 1, foldr: 3, map: 2, mapfoldl: 3, reverse: 1, reverse: 2]
  require Record

  Record.defrecord(:r_pp, :pp,
    value_fun: :undefined,
    singleton_atom_type_fun: :undefined,
    string_fun: :undefined,
    char_fun: :undefined,
    linewidth: 72,
    indent: 4
  )

  Record.defrecord(:r_options, :options, hook: :undefined, encoding: :undefined, opts: :undefined)

  def form(thing) do
    form(thing, :none)
  end

  def form(thing, options) do
    :ok
    state = state(options)
    frmt(lform(thing, options(options)), state)
  end

  def attribute(thing) do
    attribute(thing, :none)
  end

  def attribute(thing, options) do
    :ok
    state = state(options)
    frmt(lattribute(thing, options(options)), state)
  end

  def function(f) do
    function(f, :none)
  end

  def function(f, options) do
    :ok
    frmt(lfunction(f, options(options)), state(options))
  end

  def guard(gs) do
    guard(gs, :none)
  end

  def guard(gs, options) do
    :ok
    frmt(lguard(gs, options(options)), state(options))
  end

  def exprs(es) do
    exprs(es, 0, :none)
  end

  def exprs(es, options) do
    exprs(es, 0, options)
  end

  def exprs(es, i, options) do
    :ok
    frmt({:seq, [], [], [?,], lexprs(es, options(options))}, i, state(options))
  end

  def expr(e) do
    :ok
    frmt(lexpr(e, 0, options(:none)), state(:none))
  end

  def expr(e, options) do
    :ok
    frmt(lexpr(e, 0, options(options)), state(options))
  end

  def expr(e, i, options) do
    :ok
    frmt(lexpr(e, 0, options(options)), i, state(options))
  end

  def expr(e, i, p, options) do
    :ok
    frmt(lexpr(e, p, options(options)), i, state(options))
  end

  defp options(options) when is_list(options) do
    hook = :proplists.get_value(:hook, options, :none)
    encoding = encoding(options)
    r_options(hook: hook, encoding: encoding, opts: options)
  end

  defp options(hook) do
    r_options(hook: hook, encoding: encoding([]), opts: hook)
  end

  defp state(options) when is_list(options) do
    quote =
      :proplists.get_bool(
        :quote_singleton_atom_types,
        options
      )

    state =
      case encoding(options) do
        :latin1 ->
          latin1_state(quote)

        :unicode ->
          unicode_state(quote)
      end

    indent = :proplists.get_value(:indent, options, 4)
    lineWidth = :proplists.get_value(:linewidth, options, 72)
    r_pp(state, indent: indent, linewidth: lineWidth)
  end

  defp state(_Hook) do
    latin1_state(false)
  end

  defp latin1_state(quote) do
    options = [{:encoding, :latin1}]

    valueFun = fn v ->
      :io_lib_pretty.print(v, options)
    end

    singletonFun =
      case quote do
        true ->
          fn a ->
            :io_lib.write_string_as_latin1(
              :erlang.atom_to_list(a),
              ?'
            )
          end

        false ->
          valueFun
      end

    r_pp(
      value_fun: valueFun,
      singleton_atom_type_fun: singletonFun,
      string_fun: &:io_lib.write_string_as_latin1/1,
      char_fun: &:io_lib.write_char_as_latin1/1
    )
  end

  defp unicode_state(quote) do
    options = [{:encoding, :unicode}]

    valueFun = fn v ->
      :io_lib_pretty.print(v, options)
    end

    singletonFun =
      case quote do
        true ->
          fn a ->
            :io_lib.write_string(:erlang.atom_to_list(a), ?')
          end

        false ->
          valueFun
      end

    r_pp(
      value_fun: valueFun,
      singleton_atom_type_fun: singletonFun,
      string_fun: &:io_lib.write_string/1,
      char_fun: &:io_lib.write_char/1
    )
  end

  defp encoding(options) do
    case :proplists.get_value(:encoding, options, :epp.default_encoding()) do
      :latin1 ->
        :latin1

      :utf8 ->
        :unicode

      :unicode ->
        :unicode
    end
  end

  defp lform({:attribute, line, name, arg}, opts) do
    lattribute({:attribute, line, name, arg}, opts)
  end

  defp lform(
         {:function, line, name, arity, clauses},
         opts
       ) do
    lfunction({:function, line, name, arity, clauses}, opts)
  end

  defp lform({:error, _} = e, opts) do
    message(e, opts)
  end

  defp lform({:warning, _} = w, opts) do
    message(w, opts)
  end

  defp lform({:eof, _Line}, _Opts) do
    ?\n
  end

  defp message(m, r_options(encoding: encoding)) do
    f =
      case encoding do
        :latin1 ->
          '~p\n'

        :unicode ->
          '~tp\n'
      end

    leaf(format(f, [m]))
  end

  defp lattribute({:attribute, _Line, :type, type}, opts) do
    [typeattr(:type, type, opts), leaf('.\n')]
  end

  defp lattribute({:attribute, _Line, :opaque, type}, opts) do
    [typeattr(:opaque, type, opts), leaf('.\n')]
  end

  defp lattribute({:attribute, _Line, :spec, arg}, _Opts) do
    [specattr(:spec, arg), leaf('.\n')]
  end

  defp lattribute({:attribute, _Line, :callback, arg}, _Opts) do
    [specattr(:callback, arg), leaf('.\n')]
  end

  defp lattribute({:attribute, _Line, name, arg}, opts) do
    [lattribute(name, arg, opts), leaf('.\n')]
  end

  defp lattribute(:module, {m, vs}, _Opts) do
    a = a0()

    attr(
      :module,
      [
        {:var, a, pname(m)},
        foldr(
          fn v, c ->
            {:cons, a, {:var, a, v}, c}
          end,
          {nil, a},
          vs
        )
      ]
    )
  end

  defp lattribute(:module, m, _Opts) do
    attr(:module, [{:var, a0(), pname(m)}])
  end

  defp lattribute(:export, falist, _Opts) do
    attrib(:export, falist(falist))
  end

  defp lattribute(:import, name, _Opts) when is_list(name) do
    attr(:import, [{:var, a0(), pname(name)}])
  end

  defp lattribute(:import, {from, falist}, _Opts) do
    attrib(:import, [leaf(pname(from)), falist(falist)])
  end

  defp lattribute(:export_type, talist, _Opts) do
    attrib(:export_type, falist(talist))
  end

  defp lattribute(:optional_callbacks, falist, opts) do
    try do
      attrib(:optional_callbacks, falist(falist))
    catch
      _, _ ->
        attr(:optional_callbacks, [abstract(falist, opts)])
    end
  end

  defp lattribute(:file, {name, line}, _Opts) do
    attr(
      :file,
      [{:string, a0(), name}, {:integer, a0(), line}]
    )
  end

  defp lattribute(:record, {name, is}, opts) do
    nl = [leaf('-record('), {:atom, name}, ?,]
    [{:first, nl, record_fields(is, opts)}, ?)]
  end

  defp lattribute(name, arg, options) do
    attr(name, [abstract(arg, options)])
  end

  defp abstract(arg, r_options(encoding: encoding)) do
    :erl_parse.abstract(arg, [{:encoding, encoding}])
  end

  defp typeattr(tag, {typeName, type, args}, _Opts) do
    {:first, leaf('-' ++ :erlang.atom_to_list(tag) ++ ' '),
     typed(
       call({:atom, a0(), typeName}, args, 0, options(:none)),
       type
     )}
  end

  defp ltype(t) do
    ltype(t, 0)
  end

  defp ltype({:ann_type, _Line, [v, t]}, prec) do
    {l, p, r} = type_inop_prec(:"::")
    vl = ltype(v, l)
    tr = ltype(t, r)
    el = {:list, [{:cstep, [vl, :" ::"], tr}]}
    maybe_paren(p, prec, el)
  end

  defp ltype({:paren_type, _Line, [t]}, p) do
    ltype(t, p)
  end

  defp ltype({:type, _Line, :union, ts}, prec) do
    {_L, p, r} = type_inop_prec(:|)
    e = {:seq, [], [], [:" |"], ltypes(ts, r)}
    maybe_paren(p, prec, e)
  end

  defp ltype({:type, _Line, :list, [t]}, _) do
    {:seq, ?[, ?], ?,, [ltype(t)]}
  end

  defp ltype({:type, _Line, :nonempty_list, [t]}, _) do
    {:seq, ?[, ?], [?,], [ltype(t), leaf('...')]}
  end

  defp ltype({:type, line, nil, []}, _) do
    lexpr({nil, line}, options(:none))
  end

  defp ltype({:type, line, :map, :any}, _) do
    simple_type({:atom, line, :map}, [])
  end

  defp ltype({:type, _Line, :map, pairs}, prec) do
    {p, _R} = type_preop_prec(:"#")
    e = map_type(pairs)
    maybe_paren(p, prec, e)
  end

  defp ltype({:type, line, :tuple, :any}, _) do
    simple_type({:atom, line, :tuple}, [])
  end

  defp ltype({:type, _Line, :tuple, ts}, _) do
    tuple_type(ts, &ltype/2)
  end

  defp ltype(
         {:type, _Line, :record, [{:atom, _, n} | fs]},
         prec
       ) do
    {p, _R} = type_preop_prec(:"#")
    e = record_type(n, fs)
    maybe_paren(p, prec, e)
  end

  defp ltype(
         {:type, _Line, :range, [_I1, _I2] = es},
         prec
       ) do
    {_L, p, r} = type_inop_prec(:..)

    f = fn e, opts ->
      lexpr(e, r, opts)
    end

    e = expr_list(es, :.., f, options(:none))
    maybe_paren(p, prec, e)
  end

  defp ltype({:type, _Line, :binary, [i1, i2]}, _) do
    binary_type(i1, i2)
  end

  defp ltype({:type, _Line, :fun, []}, _) do
    leaf('fun()')
  end

  defp ltype(
         {:type, _, :fun, [{:type, _, :any}, _]} = funType,
         _
       ) do
    [fun_type([:fun, ?(], funType), ?)]
  end

  defp ltype(
         {:type, _Line, :fun, [{:type, _, :product, _}, _]} = funType,
         _
       ) do
    [fun_type([:fun, ?(], funType), ?)]
  end

  defp ltype({:type, line, t, ts}, _) do
    simple_type({:atom, line, t}, ts)
  end

  defp ltype({:user_type, line, t, ts}, _) do
    simple_type({:atom, line, t}, ts)
  end

  defp ltype({:remote_type, line, [m, f, ts]}, _) do
    simple_type({:remote, line, m, f}, ts)
  end

  defp ltype({:atom, _, t}, _) do
    {:singleton_atom_type, t}
  end

  defp ltype(e, p) do
    lexpr(e, p, options(:none))
  end

  defp binary_type(i1, i2) do
    b =
      for {:integer, _, 0} <- [i1] do
        []
      end === []

    u =
      for {:integer, _, 0} <- [i2] do
        []
      end === []

    p = max_prec()

    e1 =
      for _ <- [:EFE_DUMMY_GEN], b do
        [leaf('_:'), lexpr(i1, p, options(:none))]
      end

    e2 =
      for _ <- [:EFE_DUMMY_GEN], u do
        [leaf('_:_*'), lexpr(i2, p, options(:none))]
      end

    case e1 ++ e2 do
      [] ->
        leaf('<<>>')

      es ->
        {:seq, :"<<", :">>", [?,], es}
    end
  end

  defp map_type(fs) do
    {:first, [?#], map_pair_types(fs)}
  end

  defp map_pair_types(fs) do
    tuple_type(fs, &map_pair_type/2)
  end

  defp map_pair_type(
         {:type, _Line, :map_field_assoc, [kType, vType]},
         prec
       ) do
    {:list, [{:cstep, [ltype(kType, prec), leaf(' =>')], ltype(vType, prec)}]}
  end

  defp map_pair_type(
         {:type, _Line, :map_field_exact, [kType, vType]},
         prec
       ) do
    {:list, [{:cstep, [ltype(kType, prec), leaf(' :=')], ltype(vType, prec)}]}
  end

  defp record_type(name, fields) do
    {:first, [record_name(name)], field_types(fields)}
  end

  defp field_types(fs) do
    tuple_type(fs, &field_type/2)
  end

  defp field_type(
         {:type, _Line, :field_type, [name, type]},
         _Prec
       ) do
    typed(lexpr(name, options(:none)), type)
  end

  defp typed(b, type) do
    {:list, [{:cstep, [b, :" ::"], ltype(type)}]}
  end

  defp tuple_type([], _) do
    leaf('{}')
  end

  defp tuple_type(ts, f) do
    {:seq, ?{, ?}, [?,], ltypes(ts, f, 0)}
  end

  defp specattr(specKind, {funcSpec, typeSpecs}) do
    func =
      case funcSpec do
        {f, _A} ->
          {:atom, f}

        {m, f, _A} ->
          [{:atom, m}, ?:, {:atom, f}]
      end

    {:first, leaf(:lists.concat(['-', specKind, ' '])),
     {:list, [{:first, func, spec_clauses(typeSpecs)}]}}
  end

  defp spec_clauses(typeSpecs) do
    {:prefer_nl, [?;],
     for t <- typeSpecs do
       sig_type(t)
     end}
  end

  defp sig_type({:type, _Line, :bounded_fun, [t, gs]}) do
    guard_type(fun_type([], t), gs)
  end

  defp sig_type(funType) do
    fun_type([], funType)
  end

  defp guard_type(before, gs) do
    opts = options(:none)
    gl = {:list, [{:step, :when, expr_list(gs, [?,], &constraint/2, opts)}]}
    {:list, [{:step, before, gl}]}
  end

  defp constraint(
         {:type, _Line, :constraint, [{:atom, _, :is_subtype}, [{:var, _, _} = v, type]]},
         _Opts
       ) do
    typed(lexpr(v, options(:none)), type)
  end

  defp constraint(
         {:type, _Line, :constraint, [tag, as]},
         _Opts
       ) do
    simple_type(tag, as)
  end

  defp fun_type(before, {:type, _, :fun, [fType, ret]}) do
    {:first, before, {:step, [type_args(fType), :" ->"], ltype(ret)}}
  end

  defp type_args({:type, _Line, :any}) do
    leaf('(...)')
  end

  defp type_args({:type, _line, :product, ts}) do
    targs(ts)
  end

  defp simple_type(tag, types) do
    {:first, lexpr(tag, options(:none)), targs(types)}
  end

  defp targs(ts) do
    {:seq, ?(, ?), [?,], ltypes(ts, 0)}
  end

  defp ltypes(ts, prec) do
    ltypes(ts, &ltype/2, prec)
  end

  defp ltypes(ts, f, prec) do
    for t <- ts do
      f.(t, prec)
    end
  end

  defp attr(name, args) do
    {:first, [?-, {:atom, name}], args(args, options(:none))}
  end

  defp attrib(name, args) do
    {:first, [?-, {:atom, name}], [{:seq, ?(, ?), [?,], args}]}
  end

  defp pname([:"" | as]) do
    [?. | pname(as)]
  end

  defp pname([a]) do
    write(a)
  end

  defp pname([a | as]) do
    [write(a), ?. | pname(as)]
  end

  defp pname(a) when is_atom(a) do
    write(a)
  end

  defp falist([]) do
    [:"[]"]
  end

  defp falist(falist) do
    l =
      for fa <- falist do
        {name, arity} = fa
        [{:atom, name}, leaf(format('/~w', [arity]))]
      end

    [{:seq, ?[, ?], ?,, l}]
  end

  defp lfunction({:function, _Line, name, _Arity, cs}, opts) do
    cll =
      nl_clauses(
        fn c, h ->
          func_clause(name, c, h)
        end,
        ?;,
        opts,
        cs
      )

    [cll, leaf('.\n')]
  end

  defp func_clause(name, {:clause, line, head, guard, body}, opts) do
    hl = call({:atom, line, name}, head, 0, opts)
    gl = guard_when(hl, guard, opts)
    bl = body(body, opts)
    {:step, gl, bl}
  end

  defp guard_when(before, guard, opts) do
    guard_when(before, guard, opts, :" ->")
  end

  defp guard_when(before, guard, opts, after__) do
    gl = lguard(guard, opts)
    [{:list, [{:step, before, gl}]}, after__]
  end

  defp lguard([e | es], opts) when is_list(e) do
    {:list, [{:step, :when, expr_list([e | es], [?;], &guard0/2, opts)}]}
  end

  defp lguard([e | es], opts) do
    lguard([[e | es]], opts)
  end

  defp lguard([], _) do
    []
  end

  defp guard0(es, opts) do
    expr_list(es, [?,], &lexpr/2, opts)
  end

  defp body([e], opts) do
    lexpr(e, opts)
  end

  defp body(es, opts) do
    {:prefer_nl, [?,], lexprs(es, opts)}
  end

  defp lexpr(e, opts) do
    lexpr(e, 0, opts)
  end

  defp lexpr({:var, _, v}, _, _) when is_integer(v) do
    leaf(format('_~w', [v]))
  end

  defp lexpr({:var, _, v}, _, _) do
    leaf(format('~ts', [v]))
  end

  defp lexpr({:char, _, c}, _, _) do
    {:char, c}
  end

  defp lexpr({:integer, _, n}, _, _) do
    leaf(write(n))
  end

  defp lexpr({:float, _, f}, _, _) do
    leaf(write(f))
  end

  defp lexpr({:atom, _, a}, _, _) do
    {:atom, a}
  end

  defp lexpr({:string, _, s}, _, _) do
    {:string, s}
  end

  defp lexpr({nil, _}, _, _) do
    :"[]"
  end

  defp lexpr({:cons, _, h, t}, _, opts) do
    list(t, [h], opts)
  end

  defp lexpr({:lc, _, e, qs}, _Prec, opts) do
    p = max_prec()
    lcl = {:list, [{:step, [lexpr(e, p, opts), leaf(' ||')], lc_quals(qs, opts)}]}
    {:list, [{:seq, ?[, [], [[]], [{:force_nl, leaf(' '), [lcl]}]}, ?]]}
  end

  defp lexpr({:bc, _, e, qs}, _Prec, opts) do
    p = max_prec()
    lcl = {:list, [{:step, [lexpr(e, p, opts), leaf(' ||')], lc_quals(qs, opts)}]}
    {:list, [{:seq, :"<<", [], [[]], [{:force_nl, leaf(' '), [lcl]}]}, :">>"]}
  end

  defp lexpr({:tuple, _, elts}, _, opts) do
    tuple(elts, opts)
  end

  defp lexpr({:record_index, _, name, f}, prec, opts) do
    {p, r} = preop_prec(:"#")
    nl = record_name(name)
    el = [nl, ?., lexpr(f, r, opts)]
    maybe_paren(p, prec, el)
  end

  defp lexpr({:record, _, name, fs}, prec, opts) do
    {p, _R} = preop_prec(:"#")
    nl = record_name(name)
    el = {:first, nl, record_fields(fs, opts)}
    maybe_paren(p, prec, el)
  end

  defp lexpr({:record_field, _, rec, name, f}, prec, opts) do
    {l, p, r} = inop_prec(:"#")
    rl = lexpr(rec, l, opts)
    sep = hash_after_integer(rec, [?#])
    nl = [sep, {:atom, name}, ?.]
    el = [rl, nl, lexpr(f, r, opts)]
    maybe_paren(p, prec, el)
  end

  defp lexpr({:record, _, rec, name, fs}, prec, opts) do
    {l, p, _R} = inop_prec(:"#")
    rl = lexpr(rec, l, opts)
    sep = hash_after_integer(rec, [])
    nl = record_name(name)
    el = {:first, [rl, sep, nl], record_fields(fs, opts)}
    maybe_paren(p, prec, el)
  end

  defp lexpr({:record_field, _, {:atom, _, :""}, f}, prec, opts) do
    {_L, p, r} = inop_prec(:.)
    el = [?., lexpr(f, r, opts)]
    maybe_paren(p, prec, el)
  end

  defp lexpr({:record_field, _, rec, f}, prec, opts) do
    {l, p, r} = inop_prec(:.)
    el = [lexpr(rec, l, opts), ?., lexpr(f, r, opts)]
    maybe_paren(p, prec, el)
  end

  defp lexpr({:map, _, fs}, prec, opts) do
    {p, _R} = preop_prec(:"#")
    el = {:first, ?#, map_fields(fs, opts)}
    maybe_paren(p, prec, el)
  end

  defp lexpr({:map, _, map, fs}, prec, opts) do
    {l, p, _R} = inop_prec(:"#")
    rl = lexpr(map, l, opts)
    sep = hash_after_integer(map, [?#])
    el = {:first, [rl | sep], map_fields(fs, opts)}
    maybe_paren(p, prec, el)
  end

  defp lexpr({:block, _, es}, _, opts) do
    {:list, [{:step, :begin, body(es, opts)}, {:reserved, :end}]}
  end

  defp lexpr({:if, _, cs}, _, opts) do
    {:list, [{:step, :if, if_clauses(cs, opts)}, {:reserved, :end}]}
  end

  defp lexpr({:case, _, expr, cs}, _, opts) do
    {:list,
     [
       {:step, {:list, [{:step, :case, lexpr(expr, opts)}, {:reserved, :of}]},
        cr_clauses(cs, opts)},
       {:reserved, :end}
     ]}
  end

  defp lexpr({:cond, _, cs}, _, opts) do
    {:list, [{:step, leaf('cond'), cond_clauses(cs, opts)}, {:reserved, :end}]}
  end

  defp lexpr({:receive, _, cs}, _, opts) do
    {:list, [{:step, :receive, cr_clauses(cs, opts)}, {:reserved, :end}]}
  end

  defp lexpr({:receive, _, cs, to, toOpt}, _, opts) do
    al = {:list, [{:step, [lexpr(to, opts), :" ->"], body(toOpt, opts)}]}
    {:list, [{:step, :receive, cr_clauses(cs, opts)}, {:step, :after, al}, {:reserved, :end}]}
  end

  defp lexpr({:fun, _, {:function, f, a}}, _Prec, _Opts) do
    [leaf('fun '), {:atom, f}, leaf(format('/~w', [a]))]
  end

  defp lexpr({:fun, l, {:function, _, _} = func, extra}, prec, opts) do
    {:force_nl, fun_info(extra), lexpr({:fun, l, func}, prec, opts)}
  end

  defp lexpr({:fun, _, {:function, m, f, a}}, _Prec, opts) do
    nameItem = lexpr(m, opts)
    callItem = lexpr(f, opts)
    arityItem = lexpr(a, opts)
    ['fun ', nameItem, ?:, callItem, ?/, arityItem]
  end

  defp lexpr({:fun, _, {:clauses, cs}}, _Prec, opts) do
    {:list, [{:first, :fun, fun_clauses(cs, opts, :unnamed)}, {:reserved, :end}]}
  end

  defp lexpr({:named_fun, _, name, cs}, _Prec, opts) do
    {:list, [{:first, [:fun, ' '], fun_clauses(cs, opts, {:named, name})}, {:reserved, :end}]}
  end

  defp lexpr({:fun, _, {:clauses, cs}, extra}, _Prec, opts) do
    {:force_nl, fun_info(extra),
     {:list, [{:first, :fun, fun_clauses(cs, opts, :unnamed)}, {:reserved, :end}]}}
  end

  defp lexpr({:named_fun, _, name, cs, extra}, _Prec, opts) do
    {:force_nl, fun_info(extra),
     {:list, [{:first, [:fun, ' '], fun_clauses(cs, opts, {:named, name})}, {:reserved, :end}]}}
  end

  defp lexpr({:call, _, {:remote, _, {:atom, _, m}, {:atom, _, f} = n} = name, args}, prec, opts) do
    case :erl_internal.bif(m, f, length(args)) do
      true when f !== :float ->
        call(n, args, prec, opts)

      true ->
        call(name, args, prec, opts)

      false ->
        call(name, args, prec, opts)
    end
  end

  defp lexpr({:call, _, name, args}, prec, opts) do
    call(name, args, prec, opts)
  end

  defp lexpr({:try, _, es, scs, ccs, as}, _, opts) do
    {:list,
     [
       cond do
         scs === [] ->
           {:step, :try, body(es, opts)}

         true ->
           {:step, {:list, [{:step, :try, body(es, opts)}, {:reserved, :of}]},
            cr_clauses(scs, opts)}
       end
     ] ++
       cond do
         ccs === [] ->
           []

         true ->
           [{:step, :catch, try_clauses(ccs, opts)}]
       end ++
       cond do
         as === [] ->
           []

         true ->
           [{:step, :after, body(as, opts)}]
       end ++ [{:reserved, :end}]}
  end

  defp lexpr({:catch, _, expr}, prec, opts) do
    {p, r} = preop_prec(:catch)
    el = {:list, [{:step, :catch, lexpr(expr, r, opts)}]}
    maybe_paren(p, prec, el)
  end

  defp lexpr({:match, _, lhs, rhs}, prec, opts) do
    {l, p, r} = inop_prec(:=)
    pl = lexpr(lhs, l, opts)
    rl = lexpr(rhs, r, opts)
    el = {:list, [{:cstep, [pl, :" ="], rl}]}
    maybe_paren(p, prec, el)
  end

  defp lexpr({:op, _, op, arg}, prec, opts) do
    {p, r} = preop_prec(op)
    ol = {:reserved, leaf(format('~s ', [op]))}
    el = [ol, lexpr(arg, r, opts)]
    maybe_paren(p, prec, el)
  end

  defp lexpr({:op, _, op, larg, rarg}, prec, opts)
       when op === :orelse or op === :andalso do
    {l, p, r} = inop_prec(op)
    ll = lexpr(larg, l, opts)
    ol = {:reserved, leaf(format('~s', [op]))}
    lr = lexpr(rarg, r, opts)
    el = {:prefer_nl, [[]], [ll, ol, lr]}
    maybe_paren(p, prec, el)
  end

  defp lexpr({:op, _, op, larg, rarg}, prec, opts) do
    {l, p, r} = inop_prec(op)
    ll = lexpr(larg, l, opts)
    ol = {:reserved, leaf(format('~s', [op]))}
    lr = lexpr(rarg, r, opts)
    el = {:list, [ll, ol, lr]}
    maybe_paren(p, prec, el)
  end

  defp lexpr({:remote, _, m, f}, prec, opts) do
    {l, p, r} = inop_prec(:":")
    nameItem = lexpr(m, l, opts)
    callItem = lexpr(f, r, opts)
    maybe_paren(p, prec, [nameItem, ?:, callItem])
  end

  defp lexpr({:bin, _, fs}, _, opts) do
    bit_grp(fs, opts)
  end

  defp lexpr({:value, _, val}, _, _) do
    {:value, val}
  end

  defp lexpr(other, _Precedence, r_options(hook: :none)) do
    leaf(format('INVALID-FORM:~w:', [other]))
  end

  defp lexpr(hookExpr, precedence, r_options(hook: {mod, func, eas}))
       when mod !== :fun do
    {:ehook, hookExpr, precedence, {mod, func, eas}}
  end

  defp lexpr(hookExpr, precedence, r_options(hook: func, opts: options)) do
    {:hook, hookExpr, precedence, func, options}
  end

  defp hash_after_integer({:integer, _, _}, c) do
    [?\s | c]
  end

  defp hash_after_integer({:fun, _, {:function, _, _}}, c) do
    [?\s | c]
  end

  defp hash_after_integer({:fun, _, {:function, _, _, _}}, c) do
    [?\s | c]
  end

  defp hash_after_integer(_, c) do
    c
  end

  defp call(name, args, prec, opts) do
    {f, p} = func_prec()
    item = {:first, lexpr(name, f, opts), args(args, opts)}
    maybe_paren(p, prec, item)
  end

  defp fun_info(extra) do
    [leaf('% fun-info: '), {:value, extra}]
  end

  defp bit_grp([], _Opts) do
    leaf('<<>>')
  end

  defp bit_grp(fs, opts) do
    append([[:"<<"], [bit_elems(fs, opts)], [:">>"]])
  end

  defp bit_elems(es, opts) do
    expr_list(es, ?,, &bit_elem/2, opts)
  end

  defp bit_elem({:bin_element, _, expr, sz, types}, opts) do
    p = max_prec()
    vChars = lexpr(expr, p, opts)

    sChars =
      cond do
        sz !== :default ->
          [vChars, ?:, lexpr(sz, p, opts)]

        true ->
          vChars
      end

    cond do
      types !== :default ->
        [sChars, ?/ | bit_elem_types(types)]

      true ->
        sChars
    end
  end

  defp bit_elem_types([t]) do
    [bit_elem_type(t)]
  end

  defp bit_elem_types([t | rest]) do
    [bit_elem_type(t), ?- | bit_elem_types(rest)]
  end

  defp bit_elem_type({a, b}) do
    [
      lexpr(:erl_parse.abstract(a), options(:none)),
      ?:,
      lexpr(
        :erl_parse.abstract(b),
        options(:none)
      )
    ]
  end

  defp bit_elem_type(t) do
    lexpr(:erl_parse.abstract(t), options(:none))
  end

  defp record_name(name) do
    [?#, {:atom, name}]
  end

  defp record_fields(fs, opts) do
    tuple(fs, &record_field/2, opts)
  end

  defp record_field({:record_field, _, f, val}, opts) do
    {l, _P, r} = inop_prec(:=)
    fl = lexpr(f, l, opts)
    vl = lexpr(val, r, opts)
    {:list, [{:cstep, [fl, :" ="], vl}]}
  end

  defp record_field(
         {:typed_record_field, {:record_field, _, f, val}, type},
         opts
       ) do
    {l, _P, r} = inop_prec(:=)
    fl = lexpr(f, l, opts)
    vl = typed(lexpr(val, r, opts), type)
    {:list, [{:cstep, [fl, :" ="], vl}]}
  end

  defp record_field({:typed_record_field, field, type}, opts) do
    typed(record_field(field, opts), type)
  end

  defp record_field({:record_field, _, f}, opts) do
    lexpr(f, 0, opts)
  end

  defp map_fields(fs, opts) do
    tuple(fs, &map_field/2, opts)
  end

  defp map_field({:map_field_assoc, _, k, v}, opts) do
    pl = lexpr(k, 0, opts)
    {:list, [{:step, [pl, leaf(' =>')], lexpr(v, 0, opts)}]}
  end

  defp map_field({:map_field_exact, _, k, v}, opts) do
    pl = lexpr(k, 0, opts)
    {:list, [{:step, [pl, leaf(' :=')], lexpr(v, 0, opts)}]}
  end

  defp list({:cons, _, h, t}, es, opts) do
    list(t, [h | es], opts)
  end

  defp list({nil, _}, es, opts) do
    proper_list(reverse(es), opts)
  end

  defp list(other, es, opts) do
    improper_list(reverse(es, [other]), opts)
  end

  defp if_clauses(cs, opts) do
    clauses(&if_clause/2, opts, cs)
  end

  defp if_clause({:clause, _, [], g, b}, opts) do
    gl = [guard_no_when(g, opts), :" ->"]
    {:step, gl, body(b, opts)}
  end

  defp guard_no_when([e | es], opts) when is_list(e) do
    expr_list([e | es], ?;, &guard0/2, opts)
  end

  defp guard_no_when([e | es], opts) do
    guard_no_when([[e | es]], opts)
  end

  defp guard_no_when([], _) do
    leaf('true')
  end

  defp cr_clauses(cs, opts) do
    clauses(&cr_clause/2, opts, cs)
  end

  defp cr_clause({:clause, _, [t], g, b}, opts) do
    el = lexpr(t, 0, opts)
    gl = guard_when(el, g, opts)
    bl = body(b, opts)
    {:step, gl, bl}
  end

  defp try_clauses(cs, opts) do
    clauses(&try_clause/2, opts, cs)
  end

  defp try_clause(
         {:clause, _, [{:tuple, _, [c, v, s]}], g, b},
         opts
       ) do
    cs = lexpr(c, 0, opts)
    el = lexpr(v, 0, opts)
    csEl = [cs, ?:, el]
    sl = stack_backtrace(s, csEl, opts)
    gl = guard_when(sl, g, opts)
    bl = body(b, opts)
    {:step, gl, bl}
  end

  defp stack_backtrace({:var, _, :_}, el, _Opts) do
    el
  end

  defp stack_backtrace(s, el, opts) do
    el ++ [?:, lexpr(s, 0, opts)]
  end

  defp fun_clauses(cs, opts, :unnamed) do
    nl_clauses(&fun_clause/2, [?;], opts, cs)
  end

  defp fun_clauses(cs, opts, {:named, name}) do
    nl_clauses(
      fn c, h ->
        {:step, gl, bl} = fun_clause(c, h)
        {:step, [:erlang.atom_to_list(name), gl], bl}
      end,
      [?;],
      opts,
      cs
    )
  end

  defp fun_clause({:clause, _, a, g, b}, opts) do
    el = args(a, opts)
    gl = guard_when(el, g, opts)
    bl = body(b, opts)
    {:step, gl, bl}
  end

  defp cond_clauses(cs, opts) do
    clauses(&cond_clause/2, opts, cs)
  end

  defp cond_clause({:clause, _, [], [[e]], b}, opts) do
    {:step, [lexpr(e, opts), :" ->"], body(b, opts)}
  end

  defp nl_clauses(type, sep, opts, cs) do
    {:prefer_nl, sep, lexprs(cs, type, opts)}
  end

  defp clauses(type, opts, cs) do
    {:prefer_nl, [?;], lexprs(cs, type, opts)}
  end

  defp lc_quals(qs, opts) do
    {:prefer_nl, [?,], lexprs(qs, &lc_qual/2, opts)}
  end

  defp lc_qual({:b_generate, _, pat, e}, opts) do
    pl = lexpr(pat, 0, opts)
    {:list, [{:step, [pl, leaf(' <=')], lexpr(e, 0, opts)}]}
  end

  defp lc_qual({:generate, _, pat, e}, opts) do
    pl = lexpr(pat, 0, opts)
    {:list, [{:step, [pl, leaf(' <-')], lexpr(e, 0, opts)}]}
  end

  defp lc_qual(q, opts) do
    lexpr(q, 0, opts)
  end

  defp proper_list(es, opts) do
    {:seq, ?[, ?], [?,], lexprs(es, opts)}
  end

  defp improper_list(es, opts) do
    {:seq, ?[, ?], [{?,, :" |"}], lexprs(es, opts)}
  end

  defp tuple(l, opts) do
    tuple(l, &lexpr/2, opts)
  end

  defp tuple([], _F, _Opts) do
    leaf('{}')
  end

  defp tuple(es, f, opts) do
    {:seq, ?{, ?}, [?,], lexprs(es, f, opts)}
  end

  defp args(as, opts) do
    {:seq, ?(, ?), [?,], lexprs(as, opts)}
  end

  defp expr_list(es, sep, f, opts) do
    {:seq, [], [], sep, lexprs(es, f, opts)}
  end

  defp lexprs(es, opts) do
    lexprs(es, &lexpr/2, opts)
  end

  defp lexprs(es, f, opts) do
    for e <- es do
      f.(e, opts)
    end
  end

  defp maybe_paren(p, prec, expr) when p < prec do
    [?(, expr, ?)]
  end

  defp maybe_paren(_P, _Prec, expr) do
    expr
  end

  defp leaf(s) do
    {:leaf, :string.length(s), s}
  end

  defp frmt(item, pP) do
    frmt(item, 0, pP)
  end

  defp frmt(item, i, pP) do
    sT = spacetab()
    wT = wordtable()
    {chars, _Length} = f(item, i, sT, wT, pP)
    [chars]
  end

  defp f([] = nil__, _I0, _ST, _WT, _PP) do
    {nil__, 0}
  end

  defp f(c, _I0, _ST, _WT, _PP) when is_integer(c) do
    {c, 1}
  end

  defp f({:leaf, length, chars}, _I0, _ST, _WT, _PP) do
    {chars, length}
  end

  defp f([item | items], i0, sT, wT, pP) do
    consecutive(items, f(item, i0, sT, wT, pP), i0, sT, wT, pP)
  end

  defp f({:list, items}, i0, sT, wT, pP) do
    f({:seq, [], [], [[]], items}, i0, sT, wT, pP)
  end

  defp f({:first, e, item}, i0, sT, wT, pP) do
    f({:seq, e, [], [[]], [item]}, i0, sT, wT, pP)
  end

  defp f({:seq, before, after__, sep, lItems}, i0, sT, wT, pP) do
    bCharsSize = f(before, i0, sT, wT, pP)
    i = indent(bCharsSize, i0)
    charsSizeL = fl(lItems, sep, i, after__, sT, wT, pP)
    {charsL, sizeL} = unz(charsSizeL)
    {bCharsL, bSizeL} = unz1([bCharsSize])
    sizes = bSizeL ++ sizeL

    nSepChars =
      cond do
        is_list(sep) and sep !== [] ->
          :erlang.max(0, length(charsL) - 1)

        true ->
          0
      end

    case same_line(i0, sizes, nSepChars, pP) do
      {:yes, size} ->
        chars =
          cond do
            nSepChars > 0 ->
              insert_sep(charsL, ?\s)

            true ->
              charsL
          end

        {bCharsL ++ chars, size}

      :no ->
        charsList = handle_step(charsSizeL, i, sT, pP)
        {lChars, lSize} = maybe_newlines(charsList, lItems, i, nSepChars, sT, pP)
        {[bCharsL, lChars], nsz(lSize, i0)}
    end
  end

  defp f({:force_nl, _ExtraInfoItem, item}, i, sT, wT, pP)
       when i < 0 do
    f(item, i, sT, wT, pP)
  end

  defp f({:force_nl, extraInfoItem, item}, i, sT, wT, pP) do
    f({:prefer_nl, [], [extraInfoItem, item]}, i, sT, wT, pP)
  end

  defp f({:prefer_nl, sep, lItems}, i, sT, wT, pP)
       when i < 0 do
    f({:seq, [], [], sep, lItems}, i, sT, wT, pP)
  end

  defp f({:prefer_nl, sep, lItems}, i0, sT, wT, pP) do
    charsSize2L = fl(lItems, sep, i0, [], sT, wT, pP)
    {_CharsL, sizes} = unz(charsSize2L)

    cond do
      sizes === [] ->
        {[], 0}

      true ->
        {insert_newlines(charsSize2L, i0, sT, pP), nsz(:lists.last(sizes), i0)}
    end
  end

  defp f({:value, v}, i, sT, wT, pP) do
    f(write_a_value(v, pP), i, sT, wT, pP)
  end

  defp f({:atom, a}, i, sT, wT, pP) do
    f(write_an_atom(a, pP), i, sT, wT, pP)
  end

  defp f({:singleton_atom_type, a}, i, sT, wT, pP) do
    f(write_a_singleton_atom_type(a, pP), i, sT, wT, pP)
  end

  defp f({:char, c}, i, sT, wT, pP) do
    f(write_a_char(c, pP), i, sT, wT, pP)
  end

  defp f({:string, s}, i, sT, wT, pP) do
    f(write_a_string(s, i, pP), i, sT, wT, pP)
  end

  defp f({:reserved, r}, i, sT, wT, pP) do
    f(r, i, sT, wT, pP)
  end

  defp f({:hook, hookExpr, precedence, func, options}, i, _ST, _WT, _PP) do
    chars = func.(hookExpr, i, precedence, options)
    {chars, indentation(chars, i)}
  end

  defp f({:ehook, hookExpr, precedence, {mod, func, eas} = modFuncEas}, i, _ST, _WT, _PP) do
    chars = apply(mod, func, [hookExpr, i, precedence, modFuncEas | eas])
    {chars, indentation(chars, i)}
  end

  defp f(wordName, _I, _ST, wT, _PP)
       when is_atom(wordName) do
    word(wordName, wT)
  end

  defp fl([], _Sep, i0, after__, sT, wT, pP) do
    [[f(after__, i0, sT, wT, pP), {[], 0}]]
  end

  defp fl(cItems, sep0, i0, after__, sT, wT, pP) do
    f = fn
      {:step, item1, item2}, s ->
        [f(item1, i0, sT, wT, pP), f([item2, s], incr(i0, r_pp(pP, :indent)), sT, wT, pP)]

      {:cstep, item1, item2}, s ->
        {_, sz1} = charSize1 = f(item1, i0, sT, wT, pP)

        cond do
          is_integer(sz1) and sz1 < r_pp(pP, :indent) ->
            item2p = [leaf(' '), item2, s]
            [consecutive(item2p, charSize1, i0, sT, wT, pP), {[], 0}]

          true ->
            [charSize1, f([item2, s], incr(i0, r_pp(pP, :indent)), sT, wT, pP)]
        end

      {:reserved, word}, s ->
        [f([word, s], i0, sT, wT, pP), {[], 0}]

      item, s ->
        [f([item, s], i0, sT, wT, pP), {[], 0}]
    end

    {sep, lastSep} = sep(sep0)
    fl1(cItems, f, sep, lastSep, after__)
  end

  defp sep([{s, lS}]) do
    {[s], [lS]}
  end

  defp sep({_, _} = sep) do
    sep
  end

  defp sep(s) do
    {s, s}
  end

  defp fl1([cItem], f, _Sep, _LastSep, after__) do
    [f.(cItem, after__)]
  end

  defp fl1([cItem1, cItem2], f, _Sep, lastSep, after__) do
    [f.(cItem1, lastSep), f.(cItem2, after__)]
  end

  defp fl1([cItem | cItems], f, sep, lastSep, after__) do
    [f.(cItem, sep) | fl1(cItems, f, sep, lastSep, after__)]
  end

  defp consecutive(items, charSize1, i0, sT, wT, pP) do
    {charsSizes, _Length} =
      mapfoldl(
        fn item, len ->
          charsSize = f(item, len, sT, wT, pP)
          {charsSize, indent(charsSize, len)}
        end,
        indent(charSize1, i0),
        items
      )

    {charsL, sizeL} = unz1([charSize1 | charsSizes])
    {charsL, line_size(sizeL)}
  end

  defp unz(charsSizesL) do
    unz1(append(charsSizesL))
  end

  defp unz1(charSizes) do
    :lists.unzip(nonzero(charSizes))
  end

  defp nonzero(charSizes) do
    :lists.filter(
      fn {_, sz} ->
        sz !== 0
      end,
      charSizes
    )
  end

  defp maybe_newlines([{chars, size}], [], _I, _NSepChars, _ST, _PP) do
    {chars, size}
  end

  defp maybe_newlines(charsSizeList, items, i, nSepChars, sT, pP)
       when i >= 0 do
    maybe_sep(charsSizeList, items, i, nSepChars, nl_indent(i, sT), pP)
  end

  defp maybe_sep([{chars1, size1} | charsSizeL], [item | items], i0, nSepChars, sep, pP) do
    i1 =
      case classify_item(item) do
        :atomic ->
          i0 + size1

        _ ->
          r_pp(pP, :linewidth) + 1
      end

    maybe_sep1(charsSizeL, items, i0, i1, sep, nSepChars, size1, [chars1], pP)
  end

  defp maybe_sep1([{chars, size} | charsSizeL], [item | items], i0, i, sep, nSepChars, sz0, a, pP) do
    case classify_item(item) do
      :atomic when is_integer(size) ->
        size1 = size + 1
        i1 = i + size1

        cond do
          i1 <= r_pp(pP, :linewidth) ->
            a1 =
              cond do
                nSepChars > 0 ->
                  [chars, ?\s | a]

                true ->
                  [chars | a]
              end

            maybe_sep1(charsSizeL, items, i0, i1, sep, nSepChars, sz0 + size1, a1, pP)

          true ->
            a1 = [chars, sep | a]
            maybe_sep1(charsSizeL, items, i0, i0 + size, sep, nSepChars, size1, a1, pP)
        end

      _ ->
        a1 = [chars, sep | a]
        maybe_sep1(charsSizeL, items, i0, r_pp(pP, :linewidth) + 1, sep, nSepChars, 0, a1, pP)
    end
  end

  defp maybe_sep1(_CharsSizeL, _Items, _Io, _I, _Sep, _NSepChars, sz, a, _PP) do
    {:lists.reverse(a), sz}
  end

  defp insert_newlines(charsSizesL, i, sT, pP) when i >= 0 do
    {charsL, _} = unz1(handle_step(charsSizesL, i, sT, pP))
    insert_nl(charsL, i, sT)
  end

  defp handle_step(charsSizesL, i, sT, pP) do
    map(
      fn
        [{_C1, 0}, {_C2, 0}] ->
          {[], 0}

        [{c1, sz1}, {_C2, 0}] ->
          {c1, sz1}

        [{c1, sz1}, {c2, sz2}] when sz2 > 0 ->
          {insert_nl([c1, c2], i + r_pp(pP, :indent), sT), line_size([sz1, sz2])}
      end,
      charsSizesL
    )
  end

  defp insert_nl(charsL, i, sT) do
    insert_sep(charsL, nl_indent(i, sT))
  end

  defp insert_sep([chars1 | charsL], sep) do
    [
      chars1
      | for chars <- charsL do
          [sep, chars]
        end
    ]
  end

  defp nl_indent(0, _T) do
    ?\n
  end

  defp nl_indent(i, t) when i > 0 do
    [?\n | spaces(i, t)]
  end

  defp classify_item({:atom, _}) do
    :atomic
  end

  defp classify_item({:singleton_atom_type, _}) do
    :atomic
  end

  defp classify_item(atom) when is_atom(atom) do
    :atomic
  end

  defp classify_item({:leaf, _, _}) do
    :atomic
  end

  defp classify_item(_) do
    :complex
  end

  defp same_line(i0, sizeL, nSepChars, pP) do
    try do
      size = :lists.sum(sizeL) + nSepChars
      true = incr(i0, size) <= r_pp(pP, :linewidth)
      {:yes, size}
    catch
      _, _ ->
        :no
    end
  end

  defp line_size(sizeL) do
    line_size(sizeL, 0, false)
  end

  defp line_size([], size, false) do
    size
  end

  defp line_size([], size, true) do
    {:line, size}
  end

  defp line_size([{:line, len} | sizeL], _, _) do
    line_size(sizeL, len, true)
  end

  defp line_size([sz | sizeL], sizeSoFar, lF) do
    line_size(sizeL, sizeSoFar + sz, lF)
  end

  defp nsz({:line, _Len} = sz, _I) do
    sz
  end

  defp nsz(size, i) when i >= 0 do
    {:line, size + i}
  end

  defp indent({_Chars, {:line, len}}, _I) do
    len
  end

  defp indent({_Chars, size}, i) do
    incr(i, size)
  end

  defp incr(i, _Incr) when i < 0 do
    i
  end

  defp incr(i, incr) do
    i + incr
  end

  defp indentation(e, i) when i < 0 do
    :string.length(e)
  end

  defp indentation(e, i0) do
    i = :io_lib_format.indentation(e, i0)

    case has_nl(e) do
      true ->
        {:line, i}

      false ->
        i
    end
  end

  defp has_nl([?\n | _]) do
    true
  end

  defp has_nl([c | cs]) when is_integer(c) do
    has_nl(cs)
  end

  defp has_nl([c | cs]) do
    has_nl(c) or has_nl(cs)
  end

  defp has_nl([]) do
    false
  end

  defp write_a_value(v, pP) do
    flat_leaf(write_value(v, pP))
  end

  defp write_an_atom(a, pP) do
    flat_leaf(write_atom(a, pP))
  end

  defp write_a_singleton_atom_type(a, pP) do
    flat_leaf(write_singleton_atom_type(a, pP))
  end

  defp write_a_char(c, pP) do
    flat_leaf(write_char(c, pP))
  end

  defp write_a_string(s, i, pP) when i < 0 or s === [] do
    flat_leaf(write_string(s, pP))
  end

  defp write_a_string(s, i, pP) do
    len = :erlang.max(r_pp(pP, :linewidth) - i, 5)
    {:list, write_a_string(s, len, len, pP)}
  end

  defp write_a_string([], _N, _Len, _PP) do
    []
  end

  defp write_a_string(s, n, len, pP) do
    sS = :string.slice(s, 0, n)
    sl = write_string(sS, pP)

    case :erlang.and(:string.length(sl) > len, n > 5) do
      true ->
        write_a_string(s, n - 1, len, pP)

      false ->
        [
          flat_leaf(sl)
          | write_a_string(
              :string.slice(
                s,
                :string.length(sS)
              ),
              len,
              len,
              pP
            )
        ]
    end
  end

  defp flat_leaf(s) do
    l = :lists.flatten(s)
    {:leaf, :string.length(l), l}
  end

  defp write_value(v, pP) do
    r_pp(pP, :value_fun).(v)
  end

  defp write_atom(a, pP) do
    r_pp(pP, :value_fun).(a)
  end

  defp write_singleton_atom_type(a, pP) do
    r_pp(pP, :singleton_atom_type_fun).(a)
  end

  defp write_string(s, pP) do
    r_pp(pP, :string_fun).(s)
  end

  defp write_char(c, pP) do
    r_pp(pP, :char_fun).(c)
  end

  defp a0() do
    :erl_anno.new(0)
  end

  defp spacetab() do
    {[_ | l], _} =
      mapfoldl(
        fn _, a ->
          {a, [?\s | a]}
        end,
        [],
        :lists.seq(0, 30)
      )

    :erlang.list_to_tuple(l)
  end

  defp spaces(n, t) when n <= 30 do
    :erlang.element(n, t)
  end

  defp spaces(n, t) do
    [:erlang.element(30, t) | spaces(n - 30, t)]
  end

  defp wordtable() do
    l =
      for w <- [
            ' ->',
            ' =',
            '<<',
            '>>',
            '[]',
            'after',
            'begin',
            'case',
            'catch',
            'end',
            'fun',
            'if',
            'of',
            'receive',
            'try',
            'when',
            ' ::',
            '..',
            ' |'
          ] do
        {:leaf, sz, s} = leaf(w)
        {s, sz}
      end

    :erlang.list_to_tuple(l)
  end

  defp word(:" ->", wT) do
    :erlang.element(1, wT)
  end

  defp word(:" =", wT) do
    :erlang.element(2, wT)
  end

  defp word(:"<<", wT) do
    :erlang.element(3, wT)
  end

  defp word(:">>", wT) do
    :erlang.element(4, wT)
  end

  defp word(:"[]", wT) do
    :erlang.element(5, wT)
  end

  defp word(:after, wT) do
    :erlang.element(6, wT)
  end

  defp word(:begin, wT) do
    :erlang.element(7, wT)
  end

  defp word(:case, wT) do
    :erlang.element(8, wT)
  end

  defp word(:catch, wT) do
    :erlang.element(9, wT)
  end

  defp word(:end, wT) do
    :erlang.element(10, wT)
  end

  defp word(:fun, wT) do
    :erlang.element(11, wT)
  end

  defp word(:if, wT) do
    :erlang.element(12, wT)
  end

  defp word(:of, wT) do
    :erlang.element(13, wT)
  end

  defp word(:receive, wT) do
    :erlang.element(14, wT)
  end

  defp word(:try, wT) do
    :erlang.element(15, wT)
  end

  defp word(:when, wT) do
    :erlang.element(16, wT)
  end

  defp word(:" ::", wT) do
    :erlang.element(17, wT)
  end

  defp word(:.., wT) do
    :erlang.element(18, wT)
  end

  defp word(:" |", wT) do
    :erlang.element(19, wT)
  end
end
