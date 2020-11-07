defmodule :m_xref_reader do
  use Bitwise
  import :lists, only: [keysearch: 3, member: 2, reverse: 1]
  require Record

  Record.defrecord(:r_xrefr, :xrefr,
    module: [],
    function: [],
    def_at: [],
    l_call_at: [],
    x_call_at: [],
    el: [],
    ex: [],
    x: [],
    df: :undefined,
    builtins_too: false,
    is_abstr: :undefined,
    funvars: [],
    matches: [],
    unresolved: [],
    lattrs: [],
    xattrs: [],
    battrs: [],
    on_load: :undefined
  )

  Record.defrecord(:r_xref, :xref,
    version: 1,
    mode: :functions,
    variables: :not_set_up,
    modules: :dict.new(),
    applications: :dict.new(),
    releases: :dict.new(),
    library_path: [],
    libraries: :dict.new(),
    builtins_default: false,
    recurse_default: false,
    verbose_default: false,
    warnings_default: true
  )

  Record.defrecord(:r_xref_mod, :xref_mod,
    name: :"",
    app_name: [],
    dir: '',
    mtime: :undefined,
    builtins: :undefined,
    info: :undefined,
    no_unresolved: 0,
    data: :undefined
  )

  Record.defrecord(:r_xref_app, :xref_app, name: :"", rel_name: [], vsn: [], dir: '')
  Record.defrecord(:r_xref_rel, :xref_rel, name: :"", dir: '')
  Record.defrecord(:r_xref_lib, :xref_lib, name: :"", dir: '')

  Record.defrecord(:r_xref_var, :xref_var,
    name: :"",
    value: :undefined,
    vtype: :undefined,
    otype: :undefined,
    type: :undefined
  )

  def module(module, forms, collectBuiltins, x, dF) do
    attrs =
      for {:attribute, _Line, attr, v} <- forms do
        {attr, v}
      end

    isAbstract = :xref_utils.is_abstract_module(attrs)
    s = r_xrefr(module: module, builtins_too: collectBuiltins, is_abstr: isAbstract, x: x, df: dF)
    forms(forms, s)
  end

  defp forms([f | fs], s) do
    s1 = form(f, s)
    forms(fs, s1)
  end

  defp forms([], s) do
    r_xrefr(
      module: m,
      def_at: defAt,
      l_call_at: lCallAt,
      x_call_at: xCallAt,
      el: lC,
      ex: xC,
      x: x,
      df: depr,
      on_load: onLoad,
      lattrs: aL,
      xattrs: aX,
      battrs: b,
      unresolved: u
    ) = s

    oL =
      case onLoad do
        :undefined ->
          []

        f ->
          [{m, f, 0}]
      end

    r_xrefr(
      def_at: ^defAt,
      l_call_at: ^lCallAt,
      x_call_at: ^xCallAt,
      el: ^lC,
      ex: ^xC,
      x: ^x,
      df: ^depr,
      on_load: ^onLoad,
      lattrs: ^aL,
      xattrs: ^aX,
      battrs: ^b,
      unresolved: ^u
    ) = s

    attrs = {:lists.reverse(aL), :lists.reverse(aX), :lists.reverse(b)}
    {:ok, m, {defAt, lCallAt, xCallAt, lC, xC, x, attrs, depr, oL}, u}
  end

  defp form({:attribute, line, :xref, calls}, s) do
    r_xrefr(module: m, function: fun, lattrs: l, xattrs: x, battrs: b) = s
    attr(calls, :erl_anno.line(line), m, fun, l, x, b, s)
  end

  defp form({:attribute, _, :on_load, {f, 0}}, s) do
    r_xrefr(s, on_load: f)
  end

  defp form({:attribute, _Line, _Attr, _Val}, s) do
    s
  end

  defp form({:function, _, :module_info, 0, _Clauses}, s) do
    s
  end

  defp form({:function, _, :module_info, 1, _Clauses}, s) do
    s
  end

  defp form(
         {:function, 0 = _Line, :behaviour_info, 1, _Clauses},
         s
       ) do
    s
  end

  defp form({:function, anno, name, arity, clauses}, s) do
    mFA0 = {r_xrefr(s, :module), name, arity}
    mFA = adjust_arity(s, mFA0)
    s1 = r_xrefr(s, function: mFA)
    line = :erl_anno.line(anno)
    s2 = r_xrefr(s1, def_at: [{mFA, line} | r_xrefr(s, :def_at)])
    s3 = clauses(clauses, s2)
    r_xrefr(s3, function: [])
  end

  defp form(_, s) do
    s
  end

  defp clauses(cls, s) do
    r_xrefr(funvars: funVars, matches: matches) = s
    clauses(cls, funVars, matches, s)
  end

  defp clauses([{:clause, _Line, _H, g, b} | cs], funVars, matches, s) do
    s1 =
      case r_xrefr(s, :builtins_too) do
        true ->
          expr(g, s)

        false ->
          s
      end

    s2 = expr(b, s1)
    s3 = r_xrefr(s2, funvars: funVars, matches: matches)
    clauses(cs, s3)
  end

  defp clauses([], _FunVars, _Matches, s) do
    s
  end

  defp attr(notList, ln, m, fun, aL, aX, b, s) when not is_list(notList) do
    attr([notList], ln, m, fun, aL, aX, b, s)
  end

  defp attr([e = {from, to} | as], ln, m, fun, aL, aX, b, s) do
    case mfa(from, m) do
      {_, _, mFA} when mFA === fun or [] === fun ->
        attr(from, to, ln, m, fun, aL, aX, b, s, as, e)

      {_, _, _} ->
        attr(as, ln, m, fun, aL, aX, [e | b], s)

      _ ->
        attr(fun, e, ln, m, fun, aL, aX, b, s, as, e)
    end
  end

  defp attr([to | as], ln, m, fun, aL, aX, b, s) do
    attr(fun, to, ln, m, fun, aL, aX, b, s, as, to)
  end

  defp attr([], _Ln, _M, _Fun, aL, aX, b, s) do
    r_xrefr(s, lattrs: aL, xattrs: aX, battrs: b)
  end

  defp attr(from, to, ln, m, fun, aL, aX, b, s, as, e) do
    case {mfa(from, m), mfa(to, m)} do
      {{true, _, f}, {_, :external, t}} ->
        attr(as, ln, m, fun, aL, [{{f, t}, ln} | aX], b, s)

      {{true, _, f}, {_, :local, t}} ->
        attr(as, ln, m, fun, [{{f, t}, ln} | aL], aX, b, s)

      _ ->
        attr(as, ln, m, fun, aL, aX, [e | b], s)
    end
  end

  defp mfa({f, a}, m)
       when is_atom(f) and
              is_integer(a) do
    {true, :local, {m, f, a}}
  end

  defp mfa(mFA = {m, f, a}, m1)
       when is_atom(m) and
              is_atom(f) and is_integer(a) do
    {m === m1, :external, mFA}
  end

  defp mfa(_, _M) do
    false
  end

  defp expr({:if, _Line, cs}, s) do
    clauses(cs, s)
  end

  defp expr({:case, _Line, e, cs}, s) do
    s1 = expr(e, s)
    clauses(cs, s1)
  end

  defp expr({:receive, _Line, cs}, s) do
    clauses(cs, s)
  end

  defp expr({:receive, _Line, cs, to, toEs}, s) do
    s1 = expr(to, s)
    s2 = expr(toEs, s1)
    clauses(cs, s2)
  end

  defp expr({:try, _Line, es, scs, ccs, as}, s) do
    s1 = expr(es, s)
    s2 = clauses(scs, s1)
    s3 = clauses(ccs, s2)
    expr(as, s3)
  end

  defp expr(
         {:fun, line, {:function, {:atom, _, mod}, {:atom, _, name}, {:integer, _, arity}}},
         s
       ) do
    as = :lists.duplicate(arity, {:atom, line, :foo})
    external_call(mod, name, as, line, false, s)
  end

  defp expr(
         {:fun, line, {:function, mod, name, {:integer, _, arity}}},
         s
       ) do
    as = :lists.duplicate(arity, {:atom, line, :foo})
    external_call(:erlang, :apply, [mod, name, list2term(as)], line, true, s)
  end

  defp expr(
         {:fun, line, {:function, mod, name, _Arity}},
         s
       ) do
    as = {:var, line, :_}
    external_call(:erlang, :apply, [mod, name, as], line, true, s)
  end

  defp expr(
         {:fun, line, {:function, name, arity}, _Extra},
         s
       ) do
    handle_call(:local, r_xrefr(s, :module), name, arity, line, s)
  end

  defp expr({:fun, _Line, {:clauses, cs}, _Extra}, s) do
    clauses(cs, s)
  end

  defp expr({:fun, line, {:function, name, arity}}, s) do
    handle_call(:local, r_xrefr(s, :module), name, arity, line, s)
  end

  defp expr({:fun, _Line, {:clauses, cs}}, s) do
    clauses(cs, s)
  end

  defp expr({:named_fun, _Line, :_, cs}, s) do
    clauses(cs, s)
  end

  defp expr({:named_fun, _Line, name, cs}, s) do
    s1 = r_xrefr(s, funvars: [name | r_xrefr(s, :funvars)])
    clauses(cs, s1)
  end

  defp expr({:call, line, {:atom, _, name}, as}, s) do
    s1 = handle_call(:local, r_xrefr(s, :module), name, length(as), line, s)
    expr(as, s1)
  end

  defp expr(
         {:call, line, {:remote, _Line, {:atom, _, mod}, {:atom, _, name}}, as},
         s
       ) do
    external_call(mod, name, as, line, false, s)
  end

  defp expr(
         {:call, line, {:remote, _Line, mod, name}, as},
         s
       ) do
    external_call(:erlang, :apply, [mod, name, list2term(as)], line, true, s)
  end

  defp expr({:call, line, f, as}, s) do
    external_call(:erlang, :apply, [f, list2term(as)], line, true, s)
  end

  defp expr(
         {:match, _Line, {:var, _, var}, {:fun, _, {:clauses, cs}, _Extra}},
         s
       ) do
    s1 = r_xrefr(s, funvars: [var | r_xrefr(s, :funvars)])
    clauses(cs, s1)
  end

  defp expr(
         {:match, _Line, {:var, _, var}, {:fun, _, {:clauses, cs}}},
         s
       ) do
    s1 = r_xrefr(s, funvars: [var | r_xrefr(s, :funvars)])
    clauses(cs, s1)
  end

  defp expr(
         {:match, _Line, {:var, _, var}, {:named_fun, _, _, _} = fun},
         s
       ) do
    s1 = r_xrefr(s, funvars: [var | r_xrefr(s, :funvars)])
    expr(fun, s1)
  end

  defp expr({:match, _Line, {:var, _, var}, e}, s) do
    s1 = r_xrefr(s, matches: [{var, e} | r_xrefr(s, :matches)])
    expr(e, s1)
  end

  defp expr({:op, _Line, :orelse, op1, op2}, s) do
    expr([op1, op2], s)
  end

  defp expr({:op, _Line, :andalso, op1, op2}, s) do
    expr([op1, op2], s)
  end

  defp expr({:op, line, op, operand1, operand2}, s) do
    external_call(:erlang, op, [operand1, operand2], line, false, s)
  end

  defp expr({:op, line, op, operand}, s) do
    external_call(:erlang, op, [operand], line, false, s)
  end

  defp expr(t, s) when is_tuple(t) do
    expr(:erlang.tuple_to_list(t), s)
  end

  defp expr([e | es], s) do
    expr(es, expr(e, s))
  end

  defp expr(_E, s) do
    s
  end

  defp external_call(mod, fun, argsList, line, x, s) do
    arity = length(argsList)

    w =
      case :xref_utils.is_funfun(mod, fun, arity) do
        true
        when :erlang === mod and :apply === fun and
               2 === arity ->
          :apply2

        true
        when :erts_debug === mod and :apply === fun and
               4 === arity ->
          :debug4

        true when :erlang === mod and :spawn_opt === fun ->
          arity - 1

        true ->
          arity

        false when mod === :erlang ->
          case :erl_internal.type_test(fun, arity) do
            true ->
              :type

            false ->
              false
          end

        false ->
          false
      end

    s1 =
      cond do
        w === :type or x ->
          s

        true ->
          handle_call(:external, mod, fun, arity, line, s)
      end

    case {w, argsList} do
      {false, _} ->
        expr(argsList, s1)

      {:type, _} ->
        expr(argsList, s1)

      {:apply2, [{:tuple, _, [m, f]}, argsTerm]} ->
        eval_args(m, f, argsTerm, line, s1, argsList, [])

      {1, [{:tuple, _, [m, f]} | r]} ->
        eval_args(m, f, list2term([]), line, s1, argsList, r)

      {2, [[node, {:tuple, _, [m, f]}] | r]} ->
        eval_args(m, f, list2term([]), line, s1, argsList, [node | r])

      {3, [[m, f, argsTerm] | r]} ->
        eval_args(m, f, argsTerm, line, s1, argsList, r)

      {4, [[node, m, f, argsTerm] | r]} ->
        eval_args(m, f, argsTerm, line, s1, argsList, [node | r])

      {:debug4, [m, f, argsTerm, _]} ->
        eval_args(m, f, argsTerm, line, s1, argsList, [])

      _Else ->
        check_funarg(w, argsList, line, s1)
    end
  end

  defp eval_args(mod, fun, argsTerm, line, s, argsList, extra) do
    {isSimpleCall, m, f} = mod_fun(mod, fun)

    case term2list(argsTerm, [], s) do
      :undefined ->
        s1 = unresolved(m, f, -1, line, s)
        expr(argsList, s1)

      argsList2 when not isSimpleCall ->
        s1 = unresolved(m, f, length(argsList2), line, s)
        expr(argsList, s1)

      argsList2 when isSimpleCall ->
        s1 = expr(extra, s)
        external_call(m, f, argsList2, line, false, s1)
    end
  end

  defp mod_fun({:atom, _, m1}, {:atom, _, f1}) do
    {true, m1, f1}
  end

  defp mod_fun({:atom, _, m1}, _) do
    {false, m1, :"$F_EXPR"}
  end

  defp mod_fun(_, {:atom, _, f1}) do
    {false, :"$M_EXPR", f1}
  end

  defp mod_fun(_, _) do
    {false, :"$M_EXPR", :"$F_EXPR"}
  end

  defp check_funarg(w, argsList, line, s) do
    {funArg, args} = fun_args(w, argsList)

    s1 =
      case funarg(funArg, s) do
        true ->
          s

        false when is_integer(w) ->
          unresolved(:"$M_EXPR", :"$F_EXPR", 0, line, s)

        false ->
          n =
            case term2list(args, [], s) do
              :undefined ->
                -1

              as ->
                length(as)
            end

          unresolved(:"$M_EXPR", :"$F_EXPR", n, line, s)
      end

    expr(argsList, s1)
  end

  defp funarg({:fun, _, _Clauses, _Extra}, _S) do
    true
  end

  defp funarg({:fun, _, {:clauses, _}}, _S) do
    true
  end

  defp funarg({:fun, _, {:function, _, _}}, _S) do
    true
  end

  defp funarg({:fun, _, {:function, _, _, _}}, _S) do
    true
  end

  defp funarg({:named_fun, _, _, _}, _S) do
    true
  end

  defp funarg({:var, _, var}, s) do
    member(var, r_xrefr(s, :funvars))
  end

  defp funarg(_, _S) do
    false
  end

  defp fun_args(:apply2, [funArg, args]) do
    {funArg, args}
  end

  defp fun_args(1, [funArg | args]) do
    {funArg, args}
  end

  defp fun_args(2, [[_Node, funArg] | args]) do
    {funArg, args}
  end

  defp list2term(l) do
    a = :erl_anno.new(0)
    list2term(l, a)
  end

  defp list2term([a | as], anno) do
    {:cons, anno, a, list2term(as)}
  end

  defp list2term([], anno) do
    {nil, anno}
  end

  defp term2list({:cons, _Line, h, t}, l, s) do
    term2list(t, [h | l], s)
  end

  defp term2list({nil, _Line}, l, _S) do
    reverse(l)
  end

  defp term2list({:var, _, var}, l, s) do
    case keysearch(var, 1, r_xrefr(s, :matches)) do
      {:value, {^var, e}} ->
        term2list(e, l, s)

      false ->
        :undefined
    end
  end

  defp term2list(_Else, _L, _S) do
    :undefined
  end

  defp unresolved(m, f, a, line, s) do
    handle_call(:external, {m, f, a}, line, s, true)
  end

  defp handle_call(locality, module, name, arity, line, s) do
    case :xref_utils.is_builtin(module, name, arity) do
      true when not r_xrefr(s, :builtins_too) ->
        s

      _Else ->
        to = {module, name, arity}
        handle_call(locality, to, line, s, false)
    end
  end

  defp handle_call(locality, to0, anno, s, isUnres) do
    from = r_xrefr(s, :function)
    to = adjust_arity(s, to0)
    call = {from, to}
    line = :erl_anno.line(anno)
    callAt = {call, line}

    s1 =
      cond do
        isUnres ->
          r_xrefr(s, unresolved: [callAt | r_xrefr(s, :unresolved)])

        true ->
          s
      end

    case locality do
      :local ->
        r_xrefr(s1,
          el: [call | r_xrefr(s1, :el)],
          l_call_at: [callAt | r_xrefr(s1, :l_call_at)]
        )

      :external ->
        r_xrefr(s1,
          ex: [call | r_xrefr(s1, :ex)],
          x_call_at: [callAt | r_xrefr(s1, :x_call_at)]
        )
    end
  end

  defp adjust_arity(
         r_xrefr(is_abstr: true, module: m),
         {m, f, a} = mFA
       ) do
    case :xref_utils.is_static_function(f, a) do
      true ->
        mFA

      false ->
        {m, f, a - 1}
    end
  end

  defp adjust_arity(_S, mFA) do
    mFA
  end
end
