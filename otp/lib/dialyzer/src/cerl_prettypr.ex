defmodule :m_cerl_prettypr do
  use Bitwise

  import :cerl,
    only: [
      abstract: 1,
      alias_pat: 1,
      alias_var: 1,
      apply_args: 1,
      apply_op: 1,
      atom_lit: 1,
      binary_segments: 1,
      bitstr_flags: 1,
      bitstr_size: 1,
      bitstr_type: 1,
      bitstr_unit: 1,
      bitstr_val: 1,
      c_atom: 1,
      c_binary: 1,
      c_bitstr: 5,
      c_int: 1,
      call_args: 1,
      call_module: 1,
      call_name: 1,
      case_arg: 1,
      case_clauses: 1,
      catch_body: 1,
      clause_body: 1,
      clause_guard: 1,
      clause_pats: 1,
      concrete: 1,
      cons_hd: 1,
      cons_tl: 1,
      float_lit: 1,
      fun_body: 1,
      fun_vars: 1,
      get_ann: 1,
      int_lit: 1,
      is_c_cons: 1,
      is_c_let: 1,
      is_c_map_empty: 1,
      is_c_nil: 1,
      is_c_seq: 1,
      is_print_string: 1,
      let_arg: 1,
      let_body: 1,
      let_vars: 1,
      letrec_body: 1,
      letrec_defs: 1,
      map_arg: 1,
      map_es: 1,
      map_pair_key: 1,
      map_pair_op: 1,
      map_pair_val: 1,
      module_attrs: 1,
      module_defs: 1,
      module_exports: 1,
      module_name: 1,
      primop_args: 1,
      primop_name: 1,
      receive_action: 1,
      receive_clauses: 1,
      receive_timeout: 1,
      seq_arg: 1,
      seq_body: 1,
      string_lit: 1,
      try_arg: 1,
      try_body: 1,
      try_evars: 1,
      try_handler: 1,
      try_vars: 1,
      tuple_es: 1,
      type: 1,
      values_es: 1,
      var_name: 1
    ]

  import :prettypr,
    only: [
      above: 2,
      beside: 2,
      empty: 0,
      floating: 1,
      follow: 2,
      follow: 3,
      nest: 2,
      par: 1,
      par: 2,
      sep: 1,
      text: 1
    ]

  require Record

  Record.defrecord(:r_ctxt, :ctxt,
    line: 0,
    body_indent: 4,
    sub_indent: 2,
    hook: :none,
    noann: false,
    paper: 76,
    ribbon: 45,
    user: :undefined
  )

  def format(node) do
    format(node, [])
  end

  def format(node, options) do
    w = :proplists.get_value(:paper, options, 76)
    l = :proplists.get_value(:ribbon, options, 45)
    :prettypr.format(layout(node, options), w, l)
  end

  def annotate(doc, as0, ctxt) do
    case strip_line(as0) do
      [] ->
        doc

      as ->
        case r_ctxt(ctxt, :noann) do
          false ->
            es = seq(as, floating(text(',')), ctxt, &lay_concrete/2)

            follow(
              beside(floating(text('(')), doc),
              beside(text('-| ['), beside(par(es), floating(text('])')))),
              r_ctxt(ctxt, :sub_indent)
            )

          true ->
            doc
        end
    end
  end

  defp layout(node, options) do
    lay(
      node,
      r_ctxt(
        hook: :proplists.get_value(:hook, options, :none),
        noann: :proplists.get_bool(:noann, options),
        paper: :proplists.get_value(:paper, options, 76),
        ribbon: :proplists.get_value(:ribbon, options, 45),
        user: :proplists.get_value(:user, options)
      )
    )
  end

  defp lay(node, ctxt) do
    case get_line(get_ann(node)) do
      :none ->
        lay_0(node, ctxt)

      line ->
        cond do
          line > r_ctxt(ctxt, :line) ->
            ctxt1 = r_ctxt(ctxt, line: line)
            txt = :io_lib.format('% Line ~w', [line])
            above(floating(text(txt)), lay_0(node, ctxt1))

          true ->
            lay_0(node, ctxt)
        end
    end
  end

  defp lay_0(node, ctxt) do
    case r_ctxt(ctxt, :hook) do
      :none ->
        lay_ann(node, ctxt)

      hook ->
        hook.(node, ctxt, &lay_ann/2)
    end
  end

  defp lay_ann(node, ctxt) do
    doc = lay_1(node, ctxt)
    as = get_ann(node)
    annotate(doc, as, ctxt)
  end

  defp lay_1(node, ctxt) do
    case type(node) do
      :literal ->
        lay_literal(node, ctxt)

      :var ->
        lay_var(node, ctxt)

      :values ->
        lay_values(node, ctxt)

      :cons ->
        lay_cons(node, ctxt)

      :tuple ->
        lay_tuple(node, ctxt)

      :map ->
        lay_map(node, ctxt)

      :map_pair ->
        lay_map_pair(node, ctxt)

      :let ->
        lay_let(node, ctxt)

      :seq ->
        lay_seq(node, ctxt)

      :apply ->
        lay_apply(node, ctxt)

      :call ->
        lay_call(node, ctxt)

      :primop ->
        lay_primop(node, ctxt)

      :case ->
        lay_case(node, ctxt)

      :clause ->
        lay_clause(node, ctxt)

      :alias ->
        lay_alias(node, ctxt)

      :fun ->
        lay_fun(node, ctxt)

      :receive ->
        lay_receive(node, ctxt)

      :try ->
        lay_try(node, ctxt)

      :catch ->
        lay_catch(node, ctxt)

      :letrec ->
        lay_letrec(node, ctxt)

      :module ->
        lay_module(node, ctxt)

      :binary ->
        lay_binary(node, ctxt)

      :bitstr ->
        lay_bitstr(node, ctxt)
    end
  end

  defp lay_literal(node, ctxt) do
    case concrete(node) do
      v when is_atom(v) ->
        text(atom_lit(node))

      v when is_float(v) ->
        text(tidy_float(float_lit(node)))

      v when is_integer(v) ->
        text(int_lit(node))

      v when is_bitstring(v) ->
        val = fn
          i when is_integer(i) ->
            i

          b when is_bitstring(b) ->
            bZ = bit_size(b)
            <<bV::size(bZ)>> = b
            bV
        end

        sz = fn
          i when is_integer(i) ->
            8

          b when is_bitstring(b) ->
            bit_size(b)
        end

        lay_binary(
          c_binary(
            for b <- :erlang.bitstring_to_list(v) do
              c_bitstr(
                abstract(val.(b)),
                abstract(sz.(b)),
                abstract(1),
                abstract(:integer),
                abstract([:unsigned, :big])
              )
            end
          ),
          ctxt
        )

      [] ->
        text('[]')

      [_ | _] ->
        lay_cons(node, ctxt)

      v when is_tuple(v) ->
        lay_tuple(node, ctxt)

      m when is_map(m) ->
        lay_map(node, ctxt)
    end
  end

  defp lay_var(node, ctxt) do
    case var_name(node) do
      v when is_atom(v) ->
        s = :erlang.atom_to_list(v)

        case s do
          [c | _] when c >= ?A and c <= ?Z ->
            text(s)

          [c | _] when c >= 192 and c <= 222 and c != 215 ->
            text(s)

          [?_ | _] ->
            text(s)

          _ ->
            text([?_ | s])
        end

      v when is_integer(v) ->
        text([?_ | :erlang.integer_to_list(v)])

      {n, a} when is_atom(n) and is_integer(a) ->
        beside(
          lay_noann(c_atom(:erlang.atom_to_list(n)), ctxt),
          beside(text('/'), lay_noann(c_int(a), ctxt))
        )
    end
  end

  defp lay_values(node, ctxt) do
    lay_value_list(values_es(node), ctxt)
  end

  defp lay_cons(node, ctxt) do
    case is_print_string(node) do
      true ->
        lay_string(string_lit(node), ctxt)

      false ->
        beside(
          floating(text('[')),
          beside(
            par(lay_list_elements(node, ctxt)),
            floating(text(']'))
          )
        )
    end
  end

  defp lay_string(s, ctxt) do
    w = div(r_ctxt(ctxt, :ribbon) * 2, 3)
    lay_string_1(s, length(s), w)
  end

  defp lay_string_1(s, l, w) when l > w and w > 0 do
    case split_string(s, w - 1, l) do
      {_, ''} ->
        text(s)

      {s1, s2} ->
        above(
          text(s1 ++ '"'),
          lay_string_1([?" | s2], l - w + 1, w)
        )
    end
  end

  defp lay_string_1(s, _L, _W) do
    text(s)
  end

  defp split_string(xs, n, l) do
    split_string_1(xs, n, l, [])
  end

  defp split_string_1([?\s | xs], n, l, as)
       when n <= 0 and
              l >= 5 do
    {:lists.reverse([?\s | as]), xs}
  end

  defp split_string_1([?\t | xs], n, l, as)
       when n <= 0 and
              l >= 5 do
    {:lists.reverse([[?t, ?\\] | as]), xs}
  end

  defp split_string_1([?\n | xs], n, l, as)
       when n <= 0 and
              l >= 5 do
    {:lists.reverse([[?n, ?\\] | as]), xs}
  end

  defp split_string_1([?\\ | xs], n, l, as) do
    split_string_2(xs, n - 1, l - 1, [?\\ | as])
  end

  defp split_string_1(xs, n, l, as) when n <= -10 and l >= 5 do
    {:lists.reverse(as), xs}
  end

  defp split_string_1([x | xs], n, l, as) do
    split_string_1(xs, n - 1, l - 1, [x | as])
  end

  defp split_string_1([], _N, _L, as) do
    {:lists.reverse(as), ''}
  end

  defp split_string_2([[?^, x] | xs], n, l, as) do
    split_string_1(xs, n - 2, l - 2, [[x, ?^] | as])
  end

  defp split_string_2([[x1, x2, x3] | xs], n, l, as)
       when x1 >= ?0 and x1 <= ?7 and x2 >= ?0 and
              x2 <= ?7 and x3 >= ?0 and x3 <= ?7 do
    split_string_1(xs, n - 3, l - 3, [[x3, x2, x1] | as])
  end

  defp split_string_2([[x1, x2] | xs], n, l, as)
       when x1 >= ?0 and
              x1 <= ?7 and x2 >= ?0 and
              x2 <= ?7 do
    split_string_1(xs, n - 2, l - 2, [[x2, x1] | as])
  end

  defp split_string_2([x | xs], n, l, as) do
    split_string_1(xs, n - 1, l - 1, [x | as])
  end

  defp lay_tuple(node, ctxt) do
    beside(
      floating(text('{')),
      beside(
        par(seq(tuple_es(node), floating(text(',')), ctxt, &lay/2)),
        floating(text('}'))
      )
    )
  end

  defp lay_map(node, ctxt) do
    arg = map_arg(node)

    after__ =
      case is_c_map_empty(arg) do
        true ->
          floating(text('}~'))

        false ->
          beside(
            floating(text(' | ')),
            beside(lay(arg, ctxt), floating(text('}~')))
          )
      end

    beside(
      floating(text('~{')),
      beside(
        par(seq(map_es(node), floating(text(',')), ctxt, &lay/2)),
        after__
      )
    )
  end

  defp lay_map_pair(node, ctxt) do
    k = map_pair_key(node)
    v = map_pair_val(node)

    opTxt =
      case concrete(map_pair_op(node)) do
        :assoc ->
          '=>'

        :exact ->
          ':='
      end

    beside(
      lay(k, ctxt),
      beside(floating(text(opTxt)), lay(v, ctxt))
    )
  end

  defp lay_let(node, ctxt) do
    v = lay_value_list(let_vars(node), ctxt)

    d1 =
      par(
        [
          follow(text('let'), beside(v, floating(text(' ='))), r_ctxt(ctxt, :sub_indent)),
          lay(let_arg(node), ctxt)
        ],
        r_ctxt(ctxt, :body_indent)
      )

    b = let_body(node)
    d2 = lay(b, ctxt)

    case is_c_let(b) do
      true ->
        sep([beside(d1, floating(text(' in'))), d2])

      false ->
        sep([d1, beside(text('in '), d2)])
    end
  end

  defp lay_seq(node, ctxt) do
    d1 = beside(text('do '), lay(seq_arg(node), ctxt))
    b = seq_body(node)
    d2 = lay(b, ctxt)

    case is_c_seq(b) do
      true ->
        sep([d1, d2])

      false ->
        sep([d1, nest(3, d2)])
    end
  end

  defp lay_apply(node, ctxt) do
    as = seq(apply_args(node), floating(text(',')), ctxt, &lay/2)

    beside(
      follow(text('apply'), lay(apply_op(node), ctxt)),
      beside(text('('), beside(par(as), floating(text(')'))))
    )
  end

  defp lay_call(node, ctxt) do
    as = seq(call_args(node), floating(text(',')), ctxt, &lay/2)

    beside(
      follow(
        text('call'),
        beside(
          beside(
            lay(call_module(node), ctxt),
            floating(text(':'))
          ),
          lay(call_name(node), ctxt)
        ),
        r_ctxt(ctxt, :sub_indent)
      ),
      beside(text('('), beside(par(as), floating(text(')'))))
    )
  end

  defp lay_primop(node, ctxt) do
    as = seq(primop_args(node), floating(text(',')), ctxt, &lay/2)

    beside(
      follow(text('primop'), lay(primop_name(node), ctxt), r_ctxt(ctxt, :sub_indent)),
      beside(text('('), beside(par(as), floating(text(')'))))
    )
  end

  defp lay_case(node, ctxt) do
    cs = seq(case_clauses(node), :none, ctxt, &lay/2)

    sep([
      par(
        [follow(text('case'), lay(case_arg(node), ctxt), r_ctxt(ctxt, :sub_indent)), text('of')],
        r_ctxt(ctxt, :sub_indent)
      ),
      nest(r_ctxt(ctxt, :sub_indent), vertical(cs)),
      text('end')
    ])
  end

  defp lay_clause(node, ctxt) do
    p = lay_value_list(clause_pats(node), ctxt)
    g = lay(clause_guard(node), ctxt)

    h =
      par(
        [
          p,
          follow(
            follow(text('when'), g, r_ctxt(ctxt, :sub_indent)),
            floating(text('->'))
          )
        ],
        r_ctxt(ctxt, :sub_indent)
      )

    par(
      [h, lay(clause_body(node), ctxt)],
      r_ctxt(ctxt, :body_indent)
    )
  end

  defp lay_alias(node, ctxt) do
    follow(
      beside(lay(alias_var(node), ctxt), text(' =')),
      lay(alias_pat(node), ctxt),
      r_ctxt(ctxt, :body_indent)
    )
  end

  defp lay_fun(node, ctxt) do
    vs = seq(fun_vars(node), floating(text(',')), ctxt, &lay/2)

    par(
      [
        follow(
          text('fun'),
          beside(text('('), beside(par(vs), floating(text(') ->')))),
          r_ctxt(ctxt, :sub_indent)
        ),
        lay(fun_body(node), ctxt)
      ],
      r_ctxt(ctxt, :body_indent)
    )
  end

  defp lay_receive(node, ctxt) do
    cs = seq(receive_clauses(node), :none, ctxt, &lay/2)

    sep([
      text('receive'),
      nest(r_ctxt(ctxt, :sub_indent), vertical(cs)),
      sep([
        follow(
          text('after'),
          beside(
            lay(receive_timeout(node), ctxt),
            floating(text(' ->'))
          ),
          r_ctxt(ctxt, :sub_indent)
        ),
        nest(
          r_ctxt(ctxt, :sub_indent),
          lay(receive_action(node), ctxt)
        )
      ])
    ])
  end

  defp lay_try(node, ctxt) do
    vs = lay_value_list(try_vars(node), ctxt)
    evs = lay_value_list(try_evars(node), ctxt)

    sep([
      follow(text('try'), lay(try_arg(node), ctxt), r_ctxt(ctxt, :body_indent)),
      follow(
        beside(beside(text('of '), vs), floating(text(' ->'))),
        lay(try_body(node), ctxt),
        r_ctxt(ctxt, :body_indent)
      ),
      follow(
        beside(beside(text('catch '), evs), floating(text(' ->'))),
        lay(try_handler(node), ctxt),
        r_ctxt(ctxt, :body_indent)
      )
    ])
  end

  defp lay_catch(node, ctxt) do
    follow(text('catch'), lay(catch_body(node), ctxt), r_ctxt(ctxt, :sub_indent))
  end

  defp lay_letrec(node, ctxt) do
    es = seq(letrec_defs(node), :none, ctxt, &lay_fdef/2)

    sep([
      text('letrec'),
      nest(r_ctxt(ctxt, :sub_indent), vertical(es)),
      beside(text('in '), lay(letrec_body(node), ctxt))
    ])
  end

  defp lay_module(node, ctxt) do
    xs = seq(module_exports(node), floating(text(',')), ctxt, &lay_noann/2)
    as = seq(module_attrs(node), floating(text(',')), ctxt, &lay_attrdef/2)
    es = seq(module_defs(node), :none, ctxt, &lay_fdef/2)

    sep([
      follow(
        text('module'),
        follow(
          lay_noann(module_name(node), ctxt),
          beside(beside(text('['), par(xs)), floating(text(']')))
        ),
        r_ctxt(ctxt, :sub_indent)
      ),
      nest(
        r_ctxt(ctxt, :sub_indent),
        follow(
          text('attributes'),
          beside(beside(text('['), par(as)), floating(text(']'))),
          r_ctxt(ctxt, :sub_indent)
        )
      ),
      nest(r_ctxt(ctxt, :sub_indent), vertical(es)),
      text('end')
    ])
  end

  defp lay_binary(node, ctxt) do
    beside(
      floating(text('\#{')),
      beside(
        sep(seq(binary_segments(node), floating(text(',')), ctxt, &lay_bitstr/2)),
        floating(text('}#'))
      )
    )
  end

  defp lay_bitstr(node, ctxt) do
    head =
      beside(
        floating(text('#<')),
        beside(lay(bitstr_val(node), ctxt), floating(text('>')))
      )

    as = [bitstr_size(node), bitstr_unit(node), bitstr_type(node), bitstr_flags(node)]

    beside(
      head,
      beside(
        floating(text('(')),
        beside(
          sep(seq(as, floating(text(',')), ctxt, &lay/2)),
          floating(text(')'))
        )
      )
    )
  end

  defp lay_value_list([e], ctxt) do
    lay(e, ctxt)
  end

  defp lay_value_list(es, ctxt) do
    beside(
      floating(text('<')),
      beside(
        par(seq(es, floating(text(',')), ctxt, &lay/2)),
        floating(text('>'))
      )
    )
  end

  defp lay_noann(node, ctxt) do
    lay(node, r_ctxt(ctxt, noann: true))
  end

  defp lay_concrete(t, ctxt) do
    lay(abstract(t), ctxt)
  end

  defp lay_attrdef({k, v}, ctxt) do
    follow(
      beside(lay_noann(k, ctxt), floating(text(' ='))),
      lay_noann(v, ctxt),
      r_ctxt(ctxt, :body_indent)
    )
  end

  defp lay_fdef({n, f}, ctxt) do
    par(
      [beside(lay(n, ctxt), floating(text(' ='))), lay(f, ctxt)],
      r_ctxt(ctxt, :body_indent)
    )
  end

  defp lay_list_elements(node, ctxt) do
    t = cons_tl(node)

    a =
      case r_ctxt(ctxt, :noann) do
        false ->
          get_ann(t)

        true ->
          []
      end

    h = lay(cons_hd(node), ctxt)

    case is_c_cons(t) do
      true when a === [] ->
        [
          beside(h, floating(text(',')))
          | lay_list_elements(
              t,
              ctxt
            )
        ]

      _ ->
        case is_c_nil(t) do
          true when a === [] ->
            [h]

          _ ->
            [h, beside(floating(text('| ')), lay(t, ctxt))]
        end
    end
  end

  defp seq([h | t], separator, ctxt, fun) do
    case t do
      [] ->
        [fun.(h, ctxt)]

      _ ->
        [maybe_append(separator, fun.(h, ctxt)) | seq(t, separator, ctxt, fun)]
    end
  end

  defp seq([], _, _, _) do
    [empty()]
  end

  defp maybe_append(:none, d) do
    d
  end

  defp maybe_append(suffix, d) do
    beside(d, suffix)
  end

  defp vertical([d]) do
    d
  end

  defp vertical([d | ds]) do
    above(d, vertical(ds))
  end

  defp vertical([]) do
    []
  end

  defp tidy_float([[?., c] | cs]) do
    [[?., c] | tidy_float_1(cs)]
  end

  defp tidy_float([?e | _] = cs) do
    tidy_float_2(cs)
  end

  defp tidy_float([c | cs]) do
    [c | tidy_float(cs)]
  end

  defp tidy_float([]) do
    []
  end

  defp tidy_float_1([[?0, ?0, ?0] | cs]) do
    tidy_float_2(cs)
  end

  defp tidy_float_1([?e | _] = cs) do
    tidy_float_2(cs)
  end

  defp tidy_float_1([c | cs]) do
    [c | tidy_float_1(cs)]
  end

  defp tidy_float_1([]) do
    []
  end

  defp tidy_float_2([?e, ?+, ?0]) do
    []
  end

  defp tidy_float_2([[?e, ?+, ?0] | cs]) do
    tidy_float_2([[?e, ?+] | cs])
  end

  defp tidy_float_2([[?e, ?+] | _] = cs) do
    cs
  end

  defp tidy_float_2([?e, ?-, ?0]) do
    []
  end

  defp tidy_float_2([[?e, ?-, ?0] | cs]) do
    tidy_float_2([[?e, ?-] | cs])
  end

  defp tidy_float_2([[?e, ?-] | _] = cs) do
    cs
  end

  defp tidy_float_2([?e | cs]) do
    tidy_float_2([[?e, ?+] | cs])
  end

  defp tidy_float_2([_ | cs]) do
    tidy_float_2(cs)
  end

  defp tidy_float_2([]) do
    []
  end

  defp get_line([l | _As]) when is_integer(l) do
    l
  end

  defp get_line([_ | as]) do
    get_line(as)
  end

  defp get_line([]) do
    :none
  end

  defp strip_line([a | as]) when is_integer(a) do
    strip_line(as)
  end

  defp strip_line([a | as]) do
    [a | strip_line(as)]
  end

  defp strip_line([]) do
    []
  end
end
