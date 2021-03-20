defmodule :m_xref_compiler do
  use Bitwise
  import :lists, only: [concat: 1, foldl: 3, nthtail: 2, reverse: 1, sort: 1, sublist: 2]

  import :sofs,
    only: [
      composite: 2,
      difference: 2,
      empty_set: 0,
      from_term: 1,
      intersection: 2,
      is_empty_set: 1,
      multiple_relative_product: 2,
      projection: 2,
      relation: 1,
      relation_to_family: 1,
      restriction: 2,
      specification: 2,
      substitution: 2,
      to_external: 1,
      union: 2,
      union_of_family: 1
    ]

  require Record

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

  def compile(chars, table) do
    case :xref_scanner.scan(chars) do
      {:ok, tokens} ->
        case :xref_parser.parse(tokens) do
          {:ok, parseTree} ->
            :ok

            case (try do
                    statements(parseTree, table)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              e = {:error, _, _} ->
                e

              {:ok, uV, p} ->
                table1 = user_vars(uV, table)
                :ok
                reply = i(p, table1)
                :ok
                :ok
                reply
            end

          {:error, {line, _Module, error}} ->
            :erlang.error({:parse_error, line, error})
        end

      {:error, info, line} ->
        :erlang.error({:parse_error, line, info})
    end
  end

  def format_error({:error, module, error}) do
    module.format_error(error)
  end

  def format_error({:parse_error, line, error}) do
    format_parse_error(error, format_line(line))
  end

  def format_error({:variable_reassigned, expr}) do
    :io_lib.format('Variable assigned more than once: ~ts~n', [expr])
  end

  def format_error({:unknown_variable, name}) do
    :io_lib.format('Variable ~tp used before set~n', [name])
  end

  def format_error({:type_error, expr}) do
    :io_lib.format('Operator applied to argument(s) of different or invalid type(s): ~ts~n', [
      expr
    ])
  end

  def format_error({:type_mismatch, expr1, expr2}) do
    :io_lib.format('Constants of different types: ~ts, ~ts~n', [expr1, expr2])
  end

  def format_error({:unknown_constant, constant}) do
    :io_lib.format('Unknown constant ~ts~n', [constant])
  end

  def format_error(e) do
    :io_lib.format('~tp~n', [e])
  end

  defp user_vars([{{:user, name}, val} | uV], table) do
    user_vars(uV, :dict.store(name, val, table))
  end

  defp user_vars([_V | uV], table) do
    user_vars(uV, table)
  end

  defp user_vars([], table) do
    table
  end

  defp statements(stmts, table) do
    statements(stmts, table, [], [])
  end

  defp statements([stmt = {:assign, varType, name, e} | stmts0], table, l, uV) do
    case :dict.find(name, table) do
      {:ok, _} ->
        throw_error({:variable_reassigned, :xref_parser.t2s(stmt)})

      :error ->
        {type, oType, newE} = t_expr(e, table)
        val = r_xref_var(name: name, vtype: varType, otype: oType, type: type)
        newTable = :dict.store(name, val, table)

        stmts =
          cond do
            stmts0 === [] ->
              [{:variable, name}]

            true ->
              stmts0
          end

        variable = {varType, name}
        put = {:put, variable, newE}
        statements(stmts, newTable, [put | l], [{variable, val} | uV])
    end
  end

  defp statements([expr], table, l, uV) do
    {type, oType, newE} = t_expr(expr, table)
    e1 = un_familiarize(type, oType, newE)

    nE =
      case {type, oType} do
        {{:line, _}, :edge} ->
          {:relation_to_family, e1}

        {_Type, :edge_closure} ->
          e2 = {&graph_access/2, e1, e1}

          {fn _E ->
             :"closure()"
           end, e2}

        _Else ->
          e1
      end

    {:ok, uV, stats(l, nE)}
  end

  defp stats([{:put, v, x} | ss], e) do
    stats(ss, {:put, v, x, e})
  end

  defp stats([], e) do
    e
  end

  defp t_expr(e, table) do
    {:expr, type, oType, e1} = check_expr(e, table)
    :ok
    e2 = convert(e1)
    :ok
    {type, oType, e2}
  end

  defp check_expr({:list, l}, table) do
    check_constants(l, table)
  end

  defp check_expr({:tuple, l}, table) do
    {:expr, type, :vertex, _Consts} =
      check_constants(
        l,
        table
      )

    cs = reverse(constant_vertices(l, []))
    {:expr, type, :path, {:constants, cs}}
  end

  defp check_expr({:variable, name}, table) do
    case :dict.find(name, table) do
      {:ok, r_xref_var(vtype: varType, otype: oType, type: type)} ->
        v0 = {:variable, {varType, name}}

        v =
          case {varType, type, oType} do
            {:predef, :release, _} ->
              v0

            {:predef, :application, _} ->
              v0

            {:predef, :module, _} ->
              v0

            {:predef, :function, :vertex} ->
              v0

            {:predef, :function, :edge} ->
              {:call, :union_of_family, v0}

            _Else ->
              v0
          end

        {:expr, type, oType, v}

      :error ->
        throw_error({:unknown_variable, name})
    end
  end

  defp check_expr({:type, {:type, _Type}, e}, table) do
    check_expr(e, table)
  end

  defp check_expr(
         expr = {:type, {:convert, newType0}, e},
         table
       ) do
    newType = what_type(newType0)
    {:expr, oldType, oType, nE} = check_expr(e, table)
    :ok = check_conversion(oType, oldType, newType, expr)
    {:expr, newType, oType, {:convert, oType, oldType, newType, nE}}
  end

  defp check_expr(expr = {:set, sOp, e}, table) do
    {:expr, type, oType0, e1} = check_expr(e, table)

    oType =
      case {oType0, sOp} do
        {:edge, :range} ->
          :vertex

        {:edge, :domain} ->
          :vertex

        {:edge, :weak} ->
          :edge

        {:edge, :strict} ->
          :edge

        {:edge_set, :range} ->
          :vertex_set

        {:edge_set, :domain} ->
          :vertex_set

        {:edge_set, :weak} ->
          :edge_set

        {:edge_set, :strict} ->
          :edge_set

        _ ->
          throw_error({:type_error, :xref_parser.t2s(expr)})
      end

    op = set_op(sOp)
    nE = function_vertices_to_family(type, oType, {:call, op, e1})
    {:expr, type, oType, nE}
  end

  defp check_expr(expr = {:graph, op, e}, table) do
    {:expr, type, nOType, e1} = check_expr(e, table)

    case type do
      {:line, _LineType} ->
        throw_error({:type_error, :xref_parser.t2s(expr)})

      _Else ->
        :ok
    end

    oType =
      case {nOType, op} do
        {:edge, :components} ->
          :vertex_set

        {:edge, :condensation} ->
          :edge_set

        {:edge, :closure} ->
          :edge_closure

        {:edge_closure, :components} ->
          :vertex_set

        {:edge_closure, :condensation} ->
          :edge_set

        {:edge_closure, :closure} ->
          :edge_closure

        _ ->
          throw_error({:type_error, :xref_parser.t2s(expr)})
      end

    e2 = {:convert, nOType, :edge_closure, e1}

    nE =
      case op do
        :closure ->
          e2

        _Op ->
          use_of_closure(op, e2)
      end

    {:expr, type, oType, nE}
  end

  defp check_expr(expr = {:numeric, :"#", e}, table) do
    {:expr, type, oType, e1} = check_expr(e, table)

    case oType do
      :vertex ->
        :ok

      :vertex_set ->
        :ok

      :edge ->
        :ok

      :edge_set ->
        :ok

      _Else ->
        throw_error({:type_error, :xref_parser.t2s(expr)})
    end

    nE = {:convert, oType, type, :number, e1}
    {:expr, :number, :number, {:call, :no_elements, nE}}
  end

  defp check_expr(expr = {:set, sOp, e1, e2}, table) do
    {:expr, type1, oType1, nE1} = check_expr(e1, table)
    {:expr, type2, oType2, nE2} = check_expr(e2, table)

    oType =
      case {oType1, oType2} do
        {:vertex, :vertex} ->
          :vertex

        {:edge, :edge} ->
          :edge

        {:number, :number} ->
          :number

        _ ->
          throw_error({:type_error, :xref_parser.t2s(expr)})
      end

    case oType do
      :number ->
        {:expr, :number, :number, {:call, ari_op(sOp), nE1, nE2}}

      _Else ->
        {type, newE1, newE2} =
          case {type_ord(type1), type_ord(type2)} do
            {t1, t2} when t1 === t2 ->
              {type1, nE1, nE2}

            {t1, t2} when t1 < 2 or t2 < 2 ->
              throw_error({:type_error, :xref_parser.t2s(expr)})

            {t1, t2} when t1 > t2 ->
              {type2, {:convert, oType, type1, type2, nE1}, nE2}

            {t1, t2} when t1 < t2 ->
              {type1, nE1, {:convert, oType, type2, type1, nE2}}
          end

        op = set_op(sOp, type, oType)
        {:expr, type, oType, {:call, op, newE1, newE2}}
    end
  end

  defp check_expr(expr = {:restr, rOp, e1, e2}, table) do
    {:expr, type1, oType1, nE1} = check_expr(e1, table)
    {:expr, type2, oType2, nE2} = check_expr(e2, table)

    case {type1, type2} do
      {{:line, _LineType1}, _Type2} ->
        throw_error({:type_error, :xref_parser.t2s(expr)})

      {_Type1, {:line, _LineType2}} ->
        throw_error({:type_error, :xref_parser.t2s(expr)})

      _ ->
        :ok
    end

    case {oType1, oType2} do
      {:edge, :vertex} when rOp === :||| ->
        {:expr, _, _, r1} = restriction(:|, e1, type1, nE1, type2, nE2)
        {:expr, _, _, r2} = restriction(:||, e1, type1, nE1, type2, nE2)
        {:expr, type1, :edge, {:call, :intersection, r1, r2}}

      {:edge, :vertex} ->
        restriction(rOp, e1, type1, nE1, type2, nE2)

      {:edge_closure, :vertex} when rOp === :||| ->
        {:expr, _, _, r1} = closure_restriction(:|, type1, type2, oType2, nE1, nE2)
        {:expr, _, _, r2} = closure_restriction(:||, type1, type2, oType2, nE1, nE2)
        {:expr, type1, :edge, {:call, :intersection, r1, r2}}

      {:edge_closure, :vertex} ->
        closure_restriction(rOp, type1, type2, oType2, nE1, nE2)

      _ ->
        throw_error({:type_error, :xref_parser.t2s(expr)})
    end
  end

  defp check_expr(expr = {:path, e1, e2}, table) do
    {:expr, type1, oType1a, e1a} = check_expr(e1, table)
    {:expr, type2, oType2, e2a} = check_expr(e2, table)

    case {type1, type2} do
      {{:line, _LineType1}, _Type2} ->
        throw_error({:type_error, :xref_parser.t2s(expr)})

      {_Type1, {:line, _LineType2}} ->
        throw_error({:type_error, :xref_parser.t2s(expr)})

      _Else ->
        :ok
    end

    e2b = {:convert, oType2, type2, type1, e2a}
    {oType1, nE1} = path_arg(oType1a, e1a)

    nE2 =
      case {oType1, oType2} do
        {:path, :edge} ->
          {:convert, oType2, :edge_closure, e2b}

        {:path, :edge_closure} when type1 === type2 ->
          e2b

        _ ->
          throw_error({:type_error, :xref_parser.t2s(expr)})
      end

    {:expr, type1, :path, use_of_closure(:path, nE2, nE1)}
  end

  defp check_expr({:regexpr, rExpr, type0}, _Table) do
    type = what_type(type0)

    v =
      case type do
        :function ->
          :v

        :module ->
          :M

        :application ->
          :A

        :release ->
          :R
      end

    var = {:variable, {:predef, v}}

    call =
      {:call,
       fn e, v2 ->
         :xref_utils.regexpr(e, v2)
       end, {:constants, rExpr}, var}

    {:expr, type, :vertex, call}
  end

  defp check_expr(c = {:constant, _Type, _OType, _C}, table) do
    check_constants([c], table)
  end

  defp path_arg(:edge, e = {:constants, c}) do
    case to_external(c) do
      [{v1, v2}] ->
        {:path, {:constants, [v1, v2]}}

      _ ->
        {:edge, e}
    end
  end

  defp path_arg(oType, e) do
    {oType, e}
  end

  defp check_conversion(oType, type1, type2, expr) do
    case conversions(oType, type1, type2) do
      :ok ->
        :ok

      :not_ok ->
        throw_error({:type_error, :xref_parser.t2s(expr)})
    end
  end

  defp conversions(_OType, {:line, lineType}, {:line, lineType}) do
    :ok
  end

  defp conversions(:edge, {:line, _}, {:line, :all_line_call}) do
    :ok
  end

  defp conversions(:edge, from, {:line, line})
       when is_atom(from) and line !== :all_line_call do
    :ok
  end

  defp conversions(:vertex, from, {:line, :line})
       when is_atom(from) do
    :ok
  end

  defp conversions(:vertex, from, to)
       when is_atom(from) and
              is_atom(to) do
    :ok
  end

  defp conversions(:edge, from, to)
       when is_atom(from) and
              is_atom(to) do
    :ok
  end

  defp conversions(:edge, {:line, line}, to)
       when is_atom(to) and
              line !== :all_line_call do
    :ok
  end

  defp conversions(:vertex, {:line, :line}, to) when is_atom(to) do
    :ok
  end

  defp conversions(_OType, _From, _To) do
    :not_ok
  end

  defp set_op(:union, {:line, _LineType}, :edge) do
    :family_union
  end

  defp set_op(:intersection, {:line, _LineType}, :edge) do
    :family_intersection
  end

  defp set_op(:difference, {:line, _LineType}, :edge) do
    :family_difference
  end

  defp set_op(:union, :function, :vertex) do
    :family_union
  end

  defp set_op(:intersection, :function, :vertex) do
    :family_intersection
  end

  defp set_op(:difference, :function, :vertex) do
    :family_difference
  end

  defp set_op(sOp, _Type, _OType) do
    sOp
  end

  defp set_op(:weak) do
    :weak_relation
  end

  defp set_op(:strict) do
    :strict_relation
  end

  defp set_op(op) do
    op
  end

  defp ari_op(:union) do
    fn x, y ->
      x + y
    end
  end

  defp ari_op(:intersection) do
    fn x, y ->
      x * y
    end
  end

  defp ari_op(:difference) do
    fn x, y ->
      x - y
    end
  end

  defp restriction(rOp, e1, type1, nE1, type2, nE2) do
    {column, _} = restr_op(rOp)

    case nE1 do
      {:call, :union_of_family, _E} when rOp === :| ->
        restriction(column, type1, e1, type2, nE2)

      {:call, :union_of_family, _E} when rOp === :|| ->
        e1p = {:inverse, e1}
        restriction(column, type1, e1p, type2, nE2)

      _ ->
        nE2a = {:convert, :vertex, type2, type1, nE2}
        nE2b = family_to_function_vertices(type1, :vertex, nE2a)
        {:expr, type1, :edge, {:call, :restriction, column, nE1, nE2b}}
    end
  end

  defp restriction(column, type1, vE, type2, e2)
       when type1 === :function do
    m = {:convert, :vertex, type2, :module, e2}
    restr = {:call, :union_of_family, {:call, :restriction, vE, m}}
    c = {:convert, :vertex, type2, type1, e2}
    f = family_to_function_vertices(type1, :vertex, c)
    {:expr, type1, :edge, {:call, :restriction, column, restr, f}}
  end

  defp closure_restriction(op, type1, type2, oType2, e1, e2) do
    {_, fun} = restr_op(op)
    e2a = {:convert, oType2, type2, type1, e2}
    e2b = family_to_function_vertices(type1, :vertex, e2a)
    {:expr, type1, :edge, use_of_closure(fun, e1, e2b)}
  end

  defp restr_op(:|) do
    {1, :call}
  end

  defp restr_op(:||) do
    {2, :use}
  end

  defp use_of_closure(op, c) do
    access_of_closure(
      c,
      {:call,
       fn x ->
         apply(:xref_utils, op, [x])
       end, c}
    )
  end

  defp use_of_closure(op, c, e) do
    access_of_closure(
      c,
      {:call,
       fn x, y ->
         apply(:xref_utils, op, [x, y])
       end, c, e}
    )
  end

  defp access_of_closure(c, e) do
    {:call, &graph_access/2, c, e}
  end

  defp check_constants(
         cs = [
           c = {:constant, type0, oType, _Con}
           | cs1
         ],
         table
       ) do
    check_mix(cs1, type0, oType, c)

    types =
      case type0 do
        :unknown ->
          [:Rel, :App, :Mod]

        t ->
          [t]
      end

    case split(types, cs, table) do
      [{typeToBe, _Cs}] ->
        s =
          from_term(
            for {:constant, _T, _OT, con} <- cs do
              con
            end
          )

        type = what_type(typeToBe)
        e = function_vertices_to_family(type, oType, {:constants, s})
        {:expr, type, oType, e}

      [{type1, [c1 | _]}, {type2, [c2 | _]} | _] ->
        throw_error({:type_mismatch, make_vertex(type1, c1), make_vertex(type2, c2)})
    end
  end

  defp check_mix([c = {:constant, :Fun, oType, _Con} | cs], :Fun, oType, _C0) do
    check_mix(cs, :Fun, oType, c)
  end

  defp check_mix([c = {:constant, type, oType, _Con} | cs], type0, oType, _C0)
       when type !== :Fun and type0 !== :Fun do
    check_mix(cs, type, oType, c)
  end

  defp check_mix([c | _], _Type0, _OType0, c0) do
    throw_error({:type_mismatch, :xref_parser.t2s(c0), :xref_parser.t2s(c)})
  end

  defp check_mix([], _Type0, _OType0, _C0) do
    :ok
  end

  defp split(types, cs, table) do
    vs = from_term(constant_vertices(cs, []))
    split(types, vs, empty_set(), :unknown, table, [])
  end

  defp split([type | types], vs, allSoFar, _Type, table, l) do
    s0 = known_vertices(type, vs, table)
    s = difference(s0, allSoFar)

    case is_empty_set(s) do
      true ->
        split(types, vs, allSoFar, type, table, l)

      false ->
        all = union(allSoFar, s0)
        split(types, vs, all, type, table, [{type, to_external(s)} | l])
    end
  end

  defp split([], vs, all, type, _Table, l) do
    case to_external(difference(vs, all)) do
      [] ->
        l

      [c | _] ->
        throw_error({:unknown_constant, make_vertex(type, c)})
    end
  end

  defp make_vertex(type, c) do
    :xref_parser.t2s({:constant, type, :vertex, c})
  end

  defp constant_vertices([{:constant, _Type, :edge, {a, b}} | cs], l) do
    constant_vertices(cs, [a, b | l])
  end

  defp constant_vertices([{:constant, _Type, :vertex, v} | cs], l) do
    constant_vertices(cs, [v | l])
  end

  defp constant_vertices([], l) do
    l
  end

  defp known_vertices(:Fun, cs, t) do
    m = projection(1, cs)
    f = union_of_family(restriction(fetch_value(:v, t), m))
    union(bifs(cs), intersection(cs, f))
  end

  defp known_vertices(:Mod, cs, t) do
    intersection(cs, fetch_value(:M, t))
  end

  defp known_vertices(:App, cs, t) do
    intersection(cs, fetch_value(:A, t))
  end

  defp known_vertices(:Rel, cs, t) do
    intersection(cs, fetch_value(:R, t))
  end

  defp bifs(cs) do
    specification(
      {:external,
       fn {m, f, a} ->
         :xref_utils.is_builtin(m, f, a)
       end},
      cs
    )
  end

  defp function_vertices_to_family(:function, :vertex, e) do
    {:call, :partition_family, 1, e}
  end

  defp function_vertices_to_family(_Type, _OType, e) do
    e
  end

  defp family_to_function_vertices(:function, :vertex, e) do
    {:call, :union_of_family, e}
  end

  defp family_to_function_vertices(_Type, _OType, e) do
    e
  end

  defp convert({:inverse, {:variable, variable}}) do
    {:get, {:inverse, var_name(variable)}}
  end

  defp convert({:variable, variable}) do
    {:get, var_name(variable)}
  end

  defp convert({:convert, fromOType, toOType, e}) do
    convert(convert(e), fromOType, toOType)
  end

  defp convert({:convert, oType, fromType, toType, e}) do
    convert(convert(e), oType, fromType, toType)
  end

  defp convert({:call, op, e}) do
    {op, convert(e)}
  end

  defp convert({:call, op, e1, e2}) do
    {op, convert(e1), convert(e2)}
  end

  defp convert({:call, op, e1, e2, e3}) do
    {op, convert(e1), convert(e2), convert(e3)}
  end

  defp convert({:constants, constants}) do
    {:quote, constants}
  end

  defp convert(i) when is_integer(i) do
    {:quote, i}
  end

  defp var_name({:predef, varName}) do
    varName
  end

  defp var_name(variable) do
    variable
  end

  defp convert(e, oType, oType) do
    e
  end

  defp convert(e, :edge, :edge_closure) do
    {fn s ->
       :xref_utils.closure(s)
     end, e}
  end

  defp convert(e, oType, fromType, :number) do
    un_familiarize(fromType, oType, e)
  end

  defp convert(e, oType, fromType, toType) do
    case {type_ord(fromType), type_ord(toType)} do
      {fT, to} when fT === to ->
        e

      {fT, toT} when fT > toT ->
        special(oType, fromType, toType, e)

      {fT, toT} when fT < toT ->
        general(oType, fromType, toType, e)
    end
  end

  defp general(_ObjectType, fromType, toType, x)
       when fromType === toType do
    x
  end

  defp general(:edge, {:line, _LineType}, toType, lEs) do
    vEs =
      {:projection,
       {:quote,
        {:external,
         fn {v1V2, _Ls} ->
           v1V2
         end}}, lEs}

    general(:edge, :function, toType, vEs)
  end

  defp general(:edge, :function, toType, vEs) do
    mEs =
      {:projection,
       {:quote,
        {:external,
         fn {{m1, _, _}, {m2, _, _}} ->
           {m1, m2}
         end}}, vEs}

    general(:edge, :module, toType, mEs)
  end

  defp general(:edge, :module, toType, mEs) do
    aEs = {:image, {:get, :me2ae}, mEs}
    general(:edge, :application, toType, aEs)
  end

  defp general(:edge, :application, :release, aEs) do
    {:image, {:get, :ae}, aEs}
  end

  defp general(:vertex, {:line, _LineType}, toType, l) do
    v = {:partition_family, {:quote, 1}, {:domain, l}}
    general(:vertex, :function, toType, v)
  end

  defp general(:vertex, :function, toType, v) do
    m = {:domain, v}
    general(:vertex, :module, toType, m)
  end

  defp general(:vertex, :module, toType, m) do
    a = {:image, {:get, :m2a}, m}
    general(:vertex, :application, toType, a)
  end

  defp general(:vertex, :application, :release, a) do
    {:image, {:get, :a2r}, a}
  end

  defp special(_ObjectType, fromType, toType, x)
       when fromType === toType do
    x
  end

  defp special(:edge, {:line, _LineType}, {:line, :all_line_call}, calls) do
    {:put, {:tmp, :mods},
     {:projection,
      {:quote,
       {:external,
        fn {{{m1, _, _}, {m2, _, _}}, _} ->
          {m1, m2}
        end}}, calls},
     {:put, {:tmp, :def_at},
      {:union,
       {:image, {:get, :def_at},
        {:union, {:domain, {:get, {:tmp, :mods}}}, {:range, {:get, {:tmp, :mods}}}}}},
      {&funs_to_lines/2, {:get, {:tmp, :def_at}}, calls}}}
  end

  defp special(:edge, :function, {:line, lineType}, vEs) do
    var =
      cond do
        lineType === :line ->
          :call_at

        lineType === :export_call ->
          :e_call_at

        lineType === :local_call ->
          :l_call_at

        lineType === :external_call ->
          :x_call_at
      end

    line_edges(vEs, var)
  end

  defp special(:edge, :module, toType, mEs) do
    vEs =
      {:image,
       {:projection,
        {:quote,
         {:external,
          fn fE = {{m1, _, _}, {m2, _, _}} ->
            {{m1, m2}, fE}
          end}},
        {:union,
         {:image, {:get, :e},
          {:projection,
           {:quote,
            {:external,
             fn {m1, _M2} ->
               m1
             end}}, mEs}}}}, mEs}

    special(:edge, :function, toType, vEs)
  end

  defp special(:edge, :application, toType, aEs) do
    mEs = {:inverse_image, {:get, :me2ae}, aEs}
    special(:edge, :module, toType, mEs)
  end

  defp special(:edge, :release, toType, rEs) do
    aEs = {:inverse_image, {:get, :ae}, rEs}
    special(:edge, :application, toType, aEs)
  end

  defp special(:vertex, :function, {:line, _LineType}, v) do
    {:restriction, {:union_of_family, {:restriction, {:get, :def_at}, {:domain, v}}},
     {:union_of_family, v}}
  end

  defp special(:vertex, :module, toType, m) do
    v = {:restriction, {:get, :v}, m}
    special(:vertex, :function, toType, v)
  end

  defp special(:vertex, :application, toType, a) do
    m = {:inverse_image, {:get, :m2a}, a}
    special(:vertex, :module, toType, m)
  end

  defp special(:vertex, :release, toType, r) do
    a = {:inverse_image, {:get, :a2r}, r}
    special(:vertex, :application, toType, a)
  end

  defp line_edges(vEs, callAt) do
    {:put, {:tmp, :ves}, vEs,
     {:put, {:tmp, :m1},
      {:projection,
       {:quote,
        {:external,
         fn {{m1, _, _}, _} ->
           m1
         end}}, {:get, {:tmp, :ves}}},
      {:image,
       {:projection,
        {:quote,
         {:external,
          fn c = {vV, _L} ->
            {vV, c}
          end}}, {:union, {:image, {:get, callAt}, {:get, {:tmp, :m1}}}}}, {:get, {:tmp, :ves}}}}}
  end

  defp funs_to_lines(defAt, callAt) do
    t1 =
      multiple_relative_product(
        {defAt, defAt},
        projection(1, callAt)
      )

    t2 = composite(substitution(1, t1), callAt)

    fun = fn {{{v1, v2}, {l1, l2}}, ls} ->
      {{{v1, l1}, {v2, l2}}, ls}
    end

    projection({:external, fun}, t2)
  end

  defp what_type(:Rel) do
    :release
  end

  defp what_type(:App) do
    :application
  end

  defp what_type(:Mod) do
    :module
  end

  defp what_type(:Fun) do
    :function
  end

  defp what_type(:Lin) do
    {:line, :line}
  end

  defp what_type(:LLin) do
    {:line, :local_call}
  end

  defp what_type(:XLin) do
    {:line, :external_call}
  end

  defp what_type(:ELin) do
    {:line, :export_call}
  end

  defp what_type(:XXL) do
    {:line, :all_line_call}
  end

  defp type_ord({:line, :all_line_call}) do
    0
  end

  defp type_ord({:line, _LT}) do
    1
  end

  defp type_ord(:function) do
    2
  end

  defp type_ord(:module) do
    3
  end

  defp type_ord(:application) do
    4
  end

  defp type_ord(:release) do
    5
  end

  defp un_familiarize(:function, :vertex, e) do
    {:union_of_family, e}
  end

  defp un_familiarize({:line, _}, :edge, e) do
    {:family_to_relation, e}
  end

  defp un_familiarize(_Type, _OType, e) do
    e
  end

  defp i(e, table) do
    start = 1
    {n, _NE, _NI, nT} = find_nodes(e, start, :dict.new())
    {vs, uVs0, l} = save_vars(:dict.to_list(nT), nT, [], [], [])
    varsToSave = to_external(relation_to_family(relation(vs)))

    fun = fn {nN, s}, d ->
      :dict.store(nN, {:extra, s, :dict.fetch(nN, d)}, d)
    end

    d = foldl(fun, :dict.from_list(l), varsToSave)
    uVs = reverse(sort(uVs0))
    {_D, is0} = make_instructions(n, uVs, d)
    is = insert_unput(is0)
    :ok
    evaluate(is, table, [])
  end

  defp find_nodes(e = {:quote, _}, i, t) do
    find_node(e, i, t)
  end

  defp find_nodes({:get, var}, i, t) do
    find_node({:var, var}, i, t)
  end

  defp find_nodes({:put, var, e1, e2}, i, t) do
    {_NE1_N, nE1, i1, t1} = find_nodes(e1, i, t)
    nT = :dict.store({:var, var}, nE1, t1)
    find_nodes(e2, i1, nT)
  end

  defp find_nodes(tuple, i, t) when is_tuple(tuple) do
    [tag0 | l] = :erlang.tuple_to_list(tuple)

    fun = fn a, {l0, i0, t0} ->
      {nA, _E, nI, nT} = find_nodes(a, i0, t0)
      {[nA | l0], nI, nT}
    end

    {nL, nI, t1} = foldl(fun, {[], i, t}, l)

    tag =
      case tag0 do
        _ when is_function(tag0) ->
          tag0

        _ when is_atom(tag0) ->
          arity = length(nL)
          Function.capture(:sofs, tag0, arity)
      end

    find_node({:apply, tag, nL}, nI, t1)
  end

  defp find_node(e, i, t) do
    case :dict.find(e, t) do
      {:ok, {:reuse, n}} ->
        {n, e, i, t}

      {:ok, n} when is_integer(n) ->
        {n, e, i, :dict.store(e, {:reuse, n}, t)}

      {:ok, e1} ->
        find_node(e1, i, t)

      :error ->
        {i, e, i + 1, :dict.store(e, i, t)}
    end
  end

  defp save_vars([{i, {:reuse, n}} | dL], d, vs, uVs, l) do
    save_vars(dL, d, [{n, {:save, {:tmp, n}}} | vs], uVs, [{n, i} | l])
  end

  defp save_vars([{i, n} | dL], d, vs, uVs, l)
       when is_integer(n) do
    save_vars(dL, d, vs, uVs, [{n, i} | l])
  end

  defp save_vars([{{:var, v = {:user, _}}, i} | dL], d, vs, uVs, l) do
    n =
      case :dict.fetch(i, d) do
        {:reuse, n0} ->
          n0

        n0 ->
          n0
      end

    save_vars(dL, d, [{n, {:save, v}} | vs], [n | uVs], l)
  end

  defp save_vars([{{:var, {:tmp, _}}, _I} | dL], d, vs, uVs, l) do
    save_vars(dL, d, vs, uVs, l)
  end

  defp save_vars([], _D, vs, uVs, l) do
    {vs, uVs, l}
  end

  defp make_instructions(n, userVars, d) do
    {d1, is0} = make_instrs(n, d, [])
    make_more_instrs(userVars, d1, is0)
  end

  defp make_more_instrs([uV | uVs], d, is) do
    case :dict.find(uV, d) do
      :error ->
        make_more_instrs(uVs, d, is)

      _Else ->
        {nD, nIs} = make_instrs(uV, d, is)
        make_more_instrs(uVs, nD, [:pop | nIs])
    end
  end

  defp make_more_instrs([], d, is) do
    {d, is}
  end

  defp make_instrs(n, d, is) do
    case :dict.find(n, d) do
      {:ok, {:extra, save, val}} ->
        {d1, is1} = make_instr(val, d, is)
        {:dict.erase(n, d1), save ++ is1}

      {:ok, val} ->
        {d1, is1} = make_instr(val, d, is)
        {:dict.erase(n, d1), is1}

      :error ->
        {d, [{:get, {:tmp, n}} | is]}
    end
  end

  defp make_instr({:var, v}, d, is) do
    {d, [{:get, v} | is]}
  end

  defp make_instr(q = {:quote, _T}, d, is) do
    {d, [q | is]}
  end

  defp make_instr({:apply, mF, ns}, d, is) do
    fun = fn n, {d0, is0} ->
      make_instrs(n, d0, is0)
    end

    {d1, is1} = foldl(fun, {d, is}, ns)
    {d1, [{:apply, mF, length(ns)} | is1]}
  end

  defp insert_unput(l) do
    insert_unput(l, :dict.new(), [])
  end

  defp insert_unput([i = {:get, v = {:tmp, _}} | is], d, l) do
    case :dict.find(v, d) do
      {:ok, _} ->
        insert_unput(is, d, [i | l])

      :error ->
        insert_unput(is, :dict.store(v, [], d), [i, {:unput, v} | l])
    end
  end

  defp insert_unput([i = {:save, v = {:tmp, _}} | is], d, l) do
    case :dict.find(v, d) do
      {:ok, _} ->
        insert_unput(is, :dict.erase(v, d), [i | l])

      :error ->
        insert_unput(is, :dict.erase(v, d), l)
    end
  end

  defp insert_unput([i | is], d, l) do
    insert_unput(is, d, [i | l])
  end

  defp insert_unput([], _D, l) do
    l
  end

  defp graph_access(_G, v) do
    v
  end

  defp evaluate([{:apply, mF, noAs} | p], t, s) do
    args = sublist(s, noAs)
    newS = nthtail(noAs, s)
    :ok
    evaluate(p, t, [apply(mF, args) | newS])
  end

  defp evaluate([{:quote, val} | p], t, s) do
    evaluate(p, t, [val | s])
  end

  defp evaluate([{:get, var} | p], t, s) when is_atom(var) do
    value = fetch_value(var, t)

    val =
      case value do
        {r, _} ->
          r

        _ ->
          value
      end

    evaluate(p, t, [val | s])
  end

  defp evaluate([{:get, {:inverse, var}} | p], t, s) do
    {_, r} = fetch_value(var, t)
    evaluate(p, t, [r | s])
  end

  defp evaluate([{:get, {:user, var}} | p], t, s) do
    val = fetch_value(var, t)
    evaluate(p, t, [val | s])
  end

  defp evaluate([{:get, var} | p], t, s) do
    evaluate(p, t, [:dict.fetch(var, t) | s])
  end

  defp evaluate([{:save, var = {:tmp, _}} | p], t, s = [val | _]) do
    t1 = update_graph_counter(val, +1, t)
    evaluate(p, :dict.store(var, val, t1), s)
  end

  defp evaluate([{:save, {:user, name}} | p], t, s = [val | _]) do
    r_xref_var(vtype: :user, otype: oType, type: type) = :dict.fetch(name, t)
    newVar = r_xref_var(name: name, value: val, vtype: :user, otype: oType, type: type)
    t1 = update_graph_counter(val, +1, t)
    nT = :dict.store(name, newVar, t1)
    evaluate(p, nT, s)
  end

  defp evaluate([{:unput, var} | p], t, s) do
    t1 = update_graph_counter(:dict.fetch(var, t), -1, t)
    evaluate(p, :dict.erase(var, t1), s)
  end

  defp evaluate([:pop | p], t, [_ | s]) do
    evaluate(p, t, s)
  end

  defp evaluate([], t, [r]) do
    {t, r}
  end

  def update_graph_counter(value, inc, t) do
    case (try do
            :digraph.info(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      info when is_list(info) ->
        case :dict.find(value, t) do
          {:ok, 1} when inc === -1 ->
            true = :digraph.delete(value)
            :dict.erase(value, t)

          {:ok, c} ->
            :dict.store(value, c + inc, t)

          :error when inc === 1 ->
            :dict.store(value, 1, t)
        end

      _EXIT ->
        t
    end
  end

  defp fetch_value(v, d) do
    r_xref_var(value: value) = :dict.fetch(v, d)
    value
  end

  defp format_parse_error(['invalid_regexp', string, error], line) do
    :io_lib.format('Invalid regular expression "~ts"~s: ~ts~n', [
      string,
      line,
      :lists.flatten(error)
    ])
  end

  defp format_parse_error(['invalid_regexp_variable', var], line) do
    :io_lib.format('Invalid wildcard variable ~tp~s (only \'_\' is allowed)~n', [var, line])
  end

  defp format_parse_error(['missing_type', expr], line) do
    :io_lib.format('Missing type of regular expression ~ts~s~n', [expr, line])
  end

  defp format_parse_error(['type_mismatch', expr], line) do
    :io_lib.format('Type does not match structure of constant~s: ~ts~n', [line, expr])
  end

  defp format_parse_error(['invalid_operator', op], line) do
    :io_lib.format('Invalid operator ~tp~s~n', [op, line])
  end

  defp format_parse_error(error, line) do
    :io_lib.format('Parse error~s: ~ts~n', [line, :lists.flatten(error)])
  end

  defp format_line(8_388_608) do
    ' at end of string'
  end

  defp format_line(0) do
    ''
  end

  defp format_line(line) when is_integer(line) do
    concat([' on line ', line])
  end

  defp throw_error(reason) do
    throw(:erlang.error(reason))
  end

  defp error(reason) do
    {:error, :xref_compiler, reason}
  end
end
