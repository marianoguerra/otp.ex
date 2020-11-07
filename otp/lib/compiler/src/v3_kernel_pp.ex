defmodule :m_v3_kernel_pp do
  use Bitwise
  require Record

  Record.defrecord(:r_k_literal, :k_literal,
    anno: [],
    val: :undefined
  )

  Record.defrecord(:r_k_tuple, :k_tuple, anno: [], es: :undefined)

  Record.defrecord(:r_k_map, :k_map,
    anno: [],
    var: :EFE_TODO_NESTED_RECORD,
    op: :undefined,
    es: :undefined
  )

  Record.defrecord(:r_k_map_pair, :k_map_pair, anno: [], key: :undefined, val: :undefined)
  Record.defrecord(:r_k_cons, :k_cons, anno: [], hd: :undefined, tl: :undefined)

  Record.defrecord(:r_k_binary, :k_binary,
    anno: [],
    segs: :undefined
  )

  Record.defrecord(:r_k_bin_seg, :k_bin_seg,
    anno: [],
    size: :undefined,
    unit: :undefined,
    type: :undefined,
    flags: :undefined,
    seg: :undefined,
    next: :undefined
  )

  Record.defrecord(:r_k_bin_int, :k_bin_int,
    anno: [],
    size: :undefined,
    unit: :undefined,
    flags: :undefined,
    val: :undefined,
    next: :undefined
  )

  Record.defrecord(:r_k_bin_end, :k_bin_end, anno: [])
  Record.defrecord(:r_k_var, :k_var, anno: [], name: :undefined)
  Record.defrecord(:r_k_local, :k_local, anno: [], name: :undefined, arity: :undefined)

  Record.defrecord(:r_k_remote, :k_remote,
    anno: [],
    mod: :undefined,
    name: :undefined,
    arity: :undefined
  )

  Record.defrecord(:r_k_internal, :k_internal, anno: [], name: :undefined, arity: :undefined)

  Record.defrecord(:r_k_mdef, :k_mdef,
    anno: [],
    name: :undefined,
    exports: :undefined,
    attributes: :undefined,
    body: :undefined
  )

  Record.defrecord(:r_k_fdef, :k_fdef,
    anno: [],
    func: :undefined,
    arity: :undefined,
    vars: :undefined,
    body: :undefined
  )

  Record.defrecord(:r_k_seq, :k_seq, anno: [], arg: :undefined, body: :undefined)
  Record.defrecord(:r_k_put, :k_put, anno: [], arg: :undefined, ret: [])
  Record.defrecord(:r_k_bif, :k_bif, anno: [], op: :undefined, args: :undefined, ret: [])
  Record.defrecord(:r_k_test, :k_test, anno: [], op: :undefined, args: :undefined)
  Record.defrecord(:r_k_call, :k_call, anno: [], op: :undefined, args: :undefined, ret: [])
  Record.defrecord(:r_k_enter, :k_enter, anno: [], op: :undefined, args: :undefined)

  Record.defrecord(:r_k_try, :k_try,
    anno: [],
    arg: :undefined,
    vars: :undefined,
    body: :undefined,
    evars: :undefined,
    handler: :undefined,
    ret: []
  )

  Record.defrecord(:r_k_try_enter, :k_try_enter,
    anno: [],
    arg: :undefined,
    vars: :undefined,
    body: :undefined,
    evars: :undefined,
    handler: :undefined
  )

  Record.defrecord(:r_k_catch, :k_catch, anno: [], body: :undefined, ret: [])

  Record.defrecord(:r_k_letrec_goto, :k_letrec_goto,
    anno: [],
    label: :undefined,
    first: :undefined,
    then: :undefined,
    ret: []
  )

  Record.defrecord(:r_k_goto, :k_goto,
    anno: [],
    label: :undefined
  )

  Record.defrecord(:r_k_match, :k_match, anno: [], body: :undefined, ret: [])
  Record.defrecord(:r_k_alt, :k_alt, anno: [], first: :undefined, then: :undefined)
  Record.defrecord(:r_k_select, :k_select, anno: [], var: :undefined, types: :undefined)

  Record.defrecord(:r_k_type_clause, :k_type_clause,
    anno: [],
    type: :undefined,
    values: :undefined
  )

  Record.defrecord(:r_k_val_clause, :k_val_clause, anno: [], val: :undefined, body: :undefined)

  Record.defrecord(:r_k_guard, :k_guard,
    anno: [],
    clauses: :undefined
  )

  Record.defrecord(:r_k_guard_clause, :k_guard_clause,
    anno: [],
    guard: :undefined,
    body: :undefined
  )

  Record.defrecord(:r_k_break, :k_break, anno: [], args: [])
  Record.defrecord(:r_k_return, :k_return, anno: [], args: [])
  Record.defrecord(:r_iset, :iset, anno: [], vars: :undefined, arg: :undefined, body: :undefined)
  Record.defrecord(:r_ifun, :ifun, anno: [], vars: :undefined, body: :undefined)
  Record.defrecord(:r_ctxt, :ctxt, indent: 0, item_indent: 2, body_indent: 2, tab_width: 8)

  defp canno(cthing) do
    :erlang.element(2, cthing)
  end

  def format(node) do
    format(node, r_ctxt())
  end

  defp format(node, ctxt) do
    case canno(node) do
      [] ->
        format_1(node, ctxt)

      [l, {:file, _}] when is_integer(l) ->
        format_1(node, ctxt)

      list ->
        format_anno(list, ctxt, fn ctxt1 ->
          format_1(node, ctxt1)
        end)
    end
  end

  defp format_anno(_Anno, ctxt, objFun) do
    objFun.(ctxt)
  end

  defp format_1(r_k_var(name: v), _Ctxt) do
    cond do
      is_atom(v) ->
        case :erlang.atom_to_list(v) do
          [?_ | cs] ->
            '_X' ++ cs

          [c | _Cs] = l when c >= ?A and c <= ?Z ->
            l

          cs ->
            [?_ | cs]
        end

      is_integer(v) ->
        [?_ | :erlang.integer_to_list(v)]
    end
  end

  defp format_1(r_k_cons(hd: h, tl: t), ctxt) do
    txt = ['[' | format(h, ctxt_bump_indent(ctxt, 1))]

    [
      txt
      | format_list_tail(
          t,
          ctxt_bump_indent(ctxt, width(txt, ctxt))
        )
    ]
  end

  defp format_1(r_k_tuple(es: es), ctxt) do
    [?{, format_hseq(es, ',', ctxt_bump_indent(ctxt, 1), &format/2), ?}]
  end

  defp format_1(r_k_map(var: r_k_literal(val: m), op: :assoc, es: es), ctxt)
       when is_map(m) and map_size(m) === 0 do
    ['~{', format_hseq(es, ',', ctxt_bump_indent(ctxt, 1), &format/2), '}~']
  end

  defp format_1(r_k_map(var: r_k_literal(val: m), op: :exact, es: es), ctxt)
       when is_map(m) and map_size(m) === 0 do
    ['::{', format_hseq(es, ',', ctxt_bump_indent(ctxt, 1), &format/2), '}::']
  end

  defp format_1(r_k_map(var: var, op: :assoc, es: es), ctxt) do
    [
      '~{',
      format_hseq(es, ',', ctxt_bump_indent(ctxt, 1), &format/2),
      ' | ',
      format_1(var, ctxt),
      '}~'
    ]
  end

  defp format_1(r_k_map(var: var, op: :exact, es: es), ctxt) do
    [
      '::{',
      format_hseq(es, ',', ctxt_bump_indent(ctxt, 1), &format/2),
      ' | ',
      format_1(var, ctxt),
      '}::'
    ]
  end

  defp format_1(r_k_map_pair(key: k, val: v), ctxt) do
    ['<', format(k, ctxt), ',', format(v, ctxt), '>']
  end

  defp format_1(r_k_binary(segs: s), ctxt) do
    ['#<', format(s, ctxt_bump_indent(ctxt, 2)), '>#']
  end

  defp format_1(r_k_bin_seg(next: next) = s, ctxt) do
    [format_bin_seg_1(s, ctxt), format_bin_seg(next, ctxt_bump_indent(ctxt, 2))]
  end

  defp format_1(
         r_k_bin_int(size: sz, unit: u, flags: fs, val: val, next: next),
         ctxt
       ) do
    s =
      r_k_bin_seg(
        size: sz,
        unit: u,
        type: :integer,
        flags: fs,
        seg: r_k_literal(val: val),
        next: next
      )

    [format_bin_seg_1(s, ctxt), format_bin_seg(next, ctxt_bump_indent(ctxt, 2))]
  end

  defp format_1(r_k_bin_end(), _Ctxt) do
    '#<>#'
  end

  defp format_1(r_k_literal(val: a), _Ctxt) when is_atom(a) do
    core_atom(a)
  end

  defp format_1(r_k_literal(val: term), _Ctxt) do
    :io_lib.format('~p', [term])
  end

  defp format_1(r_k_local(name: n, arity: a), ctxt) do
    'local ' ++ format_fa_pair({n, a}, ctxt)
  end

  defp format_1(r_k_remote(mod: m, name: n, arity: a), _Ctxt) do
    :io_lib.format('remote ~ts:~ts/~w', [format(m), format(n), a])
  end

  defp format_1(r_k_internal(name: n, arity: a), ctxt) do
    'internal ' ++ format_fa_pair({n, a}, ctxt)
  end

  defp format_1(r_k_seq(arg: a, body: b), ctxt) do
    ctxt1 = ctxt_bump_indent(ctxt, 2)

    [
      ['do', nl_indent(ctxt1), format(a, ctxt1), nl_indent(ctxt), 'then', nl_indent(ctxt)]
      | format(b, ctxt)
    ]
  end

  defp format_1(r_k_match(body: bs, ret: rs), ctxt) do
    ctxt1 = ctxt_bump_indent(ctxt, r_ctxt(ctxt, :item_indent))
    ['match', nl_indent(ctxt1), format(bs, ctxt1), nl_indent(ctxt), 'end', format_ret(rs, ctxt1)]
  end

  defp format_1(r_k_alt(first: o, then: t), ctxt) do
    ctxt1 = ctxt_bump_indent(ctxt, r_ctxt(ctxt, :item_indent))
    ['alt', nl_indent(ctxt1), format(o, ctxt1), nl_indent(ctxt1), format(t, ctxt1)]
  end

  defp format_1(
         r_k_letrec_goto(label: label, first: first, then: then, ret: rs),
         ctxt
       ) do
    ctxt1 = ctxt_bump_indent(ctxt, r_ctxt(ctxt, :item_indent))

    [
      'letrec_goto ',
      :erlang.atom_to_list(label),
      nl_indent(ctxt1),
      format(then, ctxt1),
      nl_indent(ctxt1),
      format(first, ctxt1),
      nl_indent(ctxt),
      'end',
      format_ret(rs, ctxt1)
    ]
  end

  defp format_1(r_k_goto(label: label), _Ctxt) do
    ['goto ', :erlang.atom_to_list(label)]
  end

  defp format_1(r_k_select(var: v, types: cs), ctxt) do
    ctxt1 = ctxt_bump_indent(ctxt, 2)
    ['select ', format(v, ctxt), nl_indent(ctxt1), format_vseq(cs, '', '', ctxt1, &format/2)]
  end

  defp format_1(r_k_type_clause(type: t, values: cs), ctxt) do
    ctxt1 = ctxt_bump_indent(ctxt, r_ctxt(ctxt, :body_indent))
    ['type ', :io_lib.write(t), nl_indent(ctxt1), format_vseq(cs, '', '', ctxt1, &format/2)]
  end

  defp format_1(r_k_val_clause(val: val, body: b), ctxt) do
    ctxt1 = ctxt_bump_indent(ctxt, r_ctxt(ctxt, :body_indent))

    [
      [format(val, ctxt), ' ->', nl_indent(ctxt1)]
      | format(
          b,
          ctxt1
        )
    ]
  end

  defp format_1(r_k_guard(clauses: gs), ctxt) do
    ctxt1 = ctxt_bump_indent(ctxt, 5)
    ['when ', nl_indent(ctxt1), format_vseq(gs, '', '', ctxt1, &format/2)]
  end

  defp format_1(r_k_guard_clause(guard: g, body: b), ctxt) do
    ctxt1 = ctxt_bump_indent(ctxt, r_ctxt(ctxt, :body_indent))

    [
      [format(g, ctxt), nl_indent(ctxt), '->', nl_indent(ctxt1)]
      | format(b, ctxt1)
    ]
  end

  defp format_1(r_k_call(op: op, args: as, ret: rs), ctxt) do
    txt = ['call (', format(op, ctxt_bump_indent(ctxt, 6)), ?)]
    ctxt1 = ctxt_bump_indent(ctxt, 2)
    [txt, format_args(as, ctxt1), format_ret(rs, ctxt1)]
  end

  defp format_1(r_k_enter(op: op, args: as), ctxt) do
    txt = ['enter (', format(op, ctxt_bump_indent(ctxt, 7)), ?)]
    ctxt1 = ctxt_bump_indent(ctxt, 2)
    [txt, format_args(as, ctxt1)]
  end

  defp format_1(r_k_bif(op: op, args: as, ret: rs), ctxt) do
    txt = ['bif (', format(op, ctxt_bump_indent(ctxt, 5)), ?)]
    ctxt1 = ctxt_bump_indent(ctxt, 2)
    [txt, format_args(as, ctxt1), format_ret(rs, ctxt1)]
  end

  defp format_1(r_k_test(op: op, args: as), ctxt) do
    txt = ['test (', format(op, ctxt_bump_indent(ctxt, 6)), ?)]
    ctxt1 = ctxt_bump_indent(ctxt, 2)
    [txt, format_args(as, ctxt1)]
  end

  defp format_1(r_k_put(arg: a, ret: rs), ctxt) do
    [format(a, ctxt), format_ret(rs, ctxt_bump_indent(ctxt, 1))]
  end

  defp format_1(
         r_k_try(arg: a, vars: vs, body: b, evars: evs, handler: h, ret: rs),
         ctxt
       ) do
    ctxt1 = ctxt_bump_indent(ctxt, r_ctxt(ctxt, :body_indent))

    [
      'try',
      nl_indent(ctxt1),
      format(a, ctxt1),
      nl_indent(ctxt),
      'of ',
      format_hseq(vs, ', ', ctxt_bump_indent(ctxt, 3), &format/2),
      nl_indent(ctxt1),
      format(b, ctxt1),
      nl_indent(ctxt),
      'catch ',
      format_hseq(evs, ', ', ctxt_bump_indent(ctxt, 6), &format/2),
      nl_indent(ctxt1),
      format(h, ctxt1),
      nl_indent(ctxt),
      'end',
      format_ret(rs, ctxt)
    ]
  end

  defp format_1(
         r_k_try_enter(arg: a, vars: vs, body: b, evars: evs, handler: h),
         ctxt
       ) do
    ctxt1 = ctxt_bump_indent(ctxt, r_ctxt(ctxt, :body_indent))

    [
      'try_enter',
      nl_indent(ctxt1),
      format(a, ctxt1),
      nl_indent(ctxt),
      'of ',
      format_hseq(vs, ', ', ctxt_bump_indent(ctxt, 3), &format/2),
      nl_indent(ctxt1),
      format(b, ctxt1),
      nl_indent(ctxt),
      'catch ',
      format_hseq(evs, ', ', ctxt_bump_indent(ctxt, 6), &format/2),
      nl_indent(ctxt1),
      format(h, ctxt1),
      nl_indent(ctxt),
      'end'
    ]
  end

  defp format_1(r_k_catch(body: b, ret: rs), ctxt) do
    ctxt1 = ctxt_bump_indent(ctxt, r_ctxt(ctxt, :body_indent))
    ['catch', nl_indent(ctxt1), format(b, ctxt1), nl_indent(ctxt), 'end', format_ret(rs, ctxt1)]
  end

  defp format_1(r_k_break(args: as), ctxt) do
    ['<', format_hseq(as, ',', ctxt_bump_indent(ctxt, 1), &format/2), '>']
  end

  defp format_1(r_k_return(args: as), ctxt) do
    ['<<', format_hseq(as, ',', ctxt_bump_indent(ctxt, 1), &format/2), '>>']
  end

  defp format_1(
         r_k_fdef(func: f, arity: a, vars: vs, body: b),
         ctxt
       ) do
    ctxt1 = ctxt_bump_indent(ctxt, r_ctxt(ctxt, :body_indent))

    [
      'fdef ',
      format_fa_pair({f, a}, ctxt_bump_indent(ctxt, 5)),
      format_args(vs, ctxt_bump_indent(ctxt, 14)),
      ' =',
      nl_indent(ctxt1),
      format(b, ctxt1)
    ]
  end

  defp format_1(
         r_k_mdef(name: n, exports: es, attributes: as, body: b),
         ctxt
       ) do
    [
      [
        'module ',
        format(r_k_literal(val: n), ctxt_bump_indent(ctxt, 7)),
        nl_indent(ctxt),
        'export [',
        format_vseq(es, '', ',', ctxt_bump_indent(ctxt, 8), &format_fa_pair/2),
        ']',
        nl_indent(ctxt),
        'attributes [',
        format_vseq(as, '', ',', ctxt_bump_indent(ctxt, 12), &format_attribute/2),
        ']',
        nl_indent(ctxt),
        format_vseq(b, '', '', ctxt, &format/2),
        nl_indent(ctxt)
      ]
      | 'end'
    ]
  end

  defp format_1(r_iset(vars: vs, arg: a, body: b), ctxt) do
    ctxt1 = ctxt_bump_indent(ctxt, r_ctxt(ctxt, :body_indent))

    [
      [
        'set <',
        format_hseq(vs, ', ', ctxt_bump_indent(ctxt, 5), &format/2),
        '> =',
        nl_indent(ctxt1),
        format(a, ctxt1),
        nl_indent(ctxt),
        'in  '
      ]
      | format(b, ctxt_bump_indent(ctxt, 2))
    ]
  end

  defp format_1(r_ifun(vars: vs, body: b), ctxt) do
    ctxt1 = ctxt_bump_indent(ctxt, r_ctxt(ctxt, :body_indent))

    [
      ['fun ', format_args(vs, ctxt_bump_indent(ctxt, 4)), ' ->', nl_indent(ctxt1)]
      | format(b, ctxt1)
    ]
  end

  defp format_1(type, _Ctxt) do
    [['** Unsupported type: ', :io_lib.write(type)] | ' **']
  end

  defp format_ret(rs, ctxt) do
    [' >> ', '<', format_hseq(rs, ',', ctxt_bump_indent(ctxt, 5), &format/2), '>']
  end

  defp format_args(as, ctxt) do
    [?(, format_hseq(as, ', ', ctxt_bump_indent(ctxt, 1), &format/2), ?)]
  end

  defp format_hseq([h], _Sep, ctxt, fun) do
    fun.(h, ctxt)
  end

  defp format_hseq([h | t], sep, ctxt, fun) do
    txt = [fun.(h, ctxt) | sep]
    ctxt1 = ctxt_bump_indent(ctxt, width(txt, ctxt))
    [txt | format_hseq(t, sep, ctxt1, fun)]
  end

  defp format_hseq([], _, _, _) do
    ''
  end

  defp format_vseq([h], _Pre, _Suf, ctxt, fun) do
    fun.(h, ctxt)
  end

  defp format_vseq([h | t], pre, suf, ctxt, fun) do
    [
      [fun.(h, ctxt), suf, nl_indent(ctxt), pre]
      | format_vseq(t, pre, suf, ctxt, fun)
    ]
  end

  defp format_vseq([], _, _, _, _) do
    ''
  end

  defp format_fa_pair({f, a}, _Ctxt) do
    [core_atom(f), ?/, :erlang.integer_to_list(a)]
  end

  defp format_attribute({name, val}, ctxt) when is_list(val) do
    txt = format(r_k_literal(val: name), ctxt)
    ctxt1 = ctxt_bump_indent(ctxt, width(txt, ctxt) + 4)

    [
      txt,
      ' = ',
      ?[,
      format_vseq(val, '', ',', ctxt1, fn a, _C ->
        :io_lib.write(a)
      end),
      ?]
    ]
  end

  defp format_attribute({name, val}, ctxt) do
    txt = format(r_k_literal(val: name), ctxt)
    [txt, ' = ', :io_lib.write(val)]
  end

  defp format_list_tail(r_k_literal(anno: [], val: []), _Ctxt) do
    ']'
  end

  defp format_list_tail(r_k_cons(anno: [], hd: h, tl: t), ctxt) do
    txt = [?, | format(h, ctxt)]
    ctxt1 = ctxt_bump_indent(ctxt, width(txt, ctxt))
    [txt | format_list_tail(t, ctxt1)]
  end

  defp format_list_tail(tail, ctxt) do
    ['|', format(tail, ctxt_bump_indent(ctxt, 1)), ']']
  end

  defp format_bin_seg([], _Ctx) do
    ''
  end

  defp format_bin_seg(r_k_bin_end(anno: []), _Ctxt) do
    ''
  end

  defp format_bin_seg(r_k_bin_seg(anno: [], next: n) = seg, ctxt) do
    txt = [?, | format_bin_seg_1(seg, ctxt)]

    [
      txt
      | format_bin_seg(
          n,
          ctxt_bump_indent(ctxt, width(txt, ctxt))
        )
    ]
  end

  defp format_bin_seg(seg, ctxt) do
    ['|', format(seg, ctxt_bump_indent(ctxt, 2))]
  end

  defp format_bin_seg_1(
         r_k_bin_seg(size: s, unit: u, type: t, flags: fs, seg: seg),
         ctxt
       ) do
    [
      format(seg, ctxt),
      ':',
      format(s, ctxt),
      '*',
      :io_lib.write(u),
      ':',
      :io_lib.write(t),
      for f <- fs do
        [?-, :io_lib.write(f)]
      end
    ]
  end

  defp indent(ctxt) do
    indent(r_ctxt(ctxt, :indent), ctxt)
  end

  defp indent(n, _Ctxt) when n <= 0 do
    ''
  end

  defp indent(n, ctxt) do
    t = r_ctxt(ctxt, :tab_width)

    :lists.duplicate(
      div(n, t),
      ?\t
    ) ++ :lists.duplicate(rem(n, t), ?\s)
  end

  defp nl_indent(ctxt) do
    [?\n | indent(ctxt)]
  end

  defp unindent(t, ctxt) do
    unindent(t, r_ctxt(ctxt, :indent), ctxt, [])
  end

  defp unindent(t, n, _Ctxt, c) when n <= 0 do
    [t | c]
  end

  defp unindent([?\s | t], n, ctxt, c) do
    unindent(t, n - 1, ctxt, c)
  end

  defp unindent([?\t | t], n, ctxt, c) do
    tab = r_ctxt(ctxt, :tab_width)

    cond do
      n >= tab ->
        unindent(t, n - tab, ctxt, c)

      true ->
        unindent([:lists.duplicate(tab - n, ?\s) | t], 0, ctxt, c)
    end
  end

  defp unindent([l | t], n, ctxt, c) when is_list(l) do
    unindent(l, n, ctxt, [t | c])
  end

  defp unindent([h | t], _N, _Ctxt, c) do
    [[h, t] | c]
  end

  defp unindent([], n, ctxt, [h | t]) do
    unindent(h, n, ctxt, t)
  end

  defp unindent([], _, _, []) do
    []
  end

  defp width(txt, ctxt) do
    width(txt, 0, ctxt, [])
  end

  defp width([?\t | t], a, ctxt, c) do
    width(t, a + r_ctxt(ctxt, :tab_width), ctxt, c)
  end

  defp width([?\n | t], _A, ctxt, c) do
    width(unindent([t | c], ctxt), ctxt)
  end

  defp width([h | t], a, ctxt, c) when is_list(h) do
    width(h, a, ctxt, [t | c])
  end

  defp width([_ | t], a, ctxt, c) do
    width(t, a + 1, ctxt, c)
  end

  defp width([], a, ctxt, [h | t]) do
    width(h, a, ctxt, t)
  end

  defp width([], a, _, []) do
    a
  end

  defp ctxt_bump_indent(ctxt, dx) do
    r_ctxt(ctxt, indent: r_ctxt(ctxt, :indent) + dx)
  end

  defp core_atom(a) do
    :io_lib.write_string(:erlang.atom_to_list(a), ?')
  end
end
