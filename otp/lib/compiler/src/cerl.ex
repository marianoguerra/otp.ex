defmodule :m_cerl do
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

  def type(r_c_alias()) do
    :alias
  end

  def type(r_c_apply()) do
    :apply
  end

  def type(r_c_binary()) do
    :binary
  end

  def type(r_c_bitstr()) do
    :bitstr
  end

  def type(r_c_call()) do
    :call
  end

  def type(r_c_case()) do
    :case
  end

  def type(r_c_catch()) do
    :catch
  end

  def type(r_c_clause()) do
    :clause
  end

  def type(r_c_cons()) do
    :cons
  end

  def type(r_c_fun()) do
    :fun
  end

  def type(r_c_let()) do
    :let
  end

  def type(r_c_letrec()) do
    :letrec
  end

  def type(r_c_literal()) do
    :literal
  end

  def type(r_c_map()) do
    :map
  end

  def type(r_c_map_pair()) do
    :map_pair
  end

  def type(r_c_module()) do
    :module
  end

  def type(r_c_primop()) do
    :primop
  end

  def type(r_c_receive()) do
    :receive
  end

  def type(r_c_seq()) do
    :seq
  end

  def type(r_c_try()) do
    :try
  end

  def type(r_c_tuple()) do
    :tuple
  end

  def type(r_c_values()) do
    :values
  end

  def type(r_c_var()) do
    :var
  end

  def is_leaf(node) do
    case type(node) do
      :literal ->
        true

      :var ->
        true

      _ ->
        false
    end
  end

  def get_ann(node) do
    :erlang.element(2, node)
  end

  def set_ann(node, list) do
    :erlang.setelement(2, node, list)
  end

  def add_ann(terms, node) do
    set_ann(node, terms ++ get_ann(node))
  end

  def copy_ann(source, target) do
    set_ann(target, get_ann(source))
  end

  def abstract(t) do
    r_c_literal(val: t)
  end

  def ann_abstract(as, t) do
    r_c_literal(val: t, anno: as)
  end

  def is_literal_term(t) when is_integer(t) do
    true
  end

  def is_literal_term(t) when is_float(t) do
    true
  end

  def is_literal_term(t) when is_atom(t) do
    true
  end

  def is_literal_term([]) do
    true
  end

  def is_literal_term([h | t]) do
    is_literal_term(h) and is_literal_term(t)
  end

  def is_literal_term(t) when is_tuple(t) do
    is_literal_term_list(:erlang.tuple_to_list(t))
  end

  def is_literal_term(b) when is_bitstring(b) do
    true
  end

  def is_literal_term(m) when is_map(m) do
    is_literal_term_list(:maps.to_list(m))
  end

  def is_literal_term(f) when is_function(f) do
    :erlang.fun_info(f, :type) === {:type, :external}
  end

  def is_literal_term(_) do
    false
  end

  defp is_literal_term_list([t | ts]) do
    case is_literal_term(t) do
      true ->
        is_literal_term_list(ts)

      false ->
        false
    end
  end

  defp is_literal_term_list([]) do
    true
  end

  def concrete(r_c_literal(val: v)) do
    v
  end

  def is_literal(r_c_literal()) do
    true
  end

  def is_literal(_) do
    false
  end

  def fold_literal(node) do
    case type(node) do
      :tuple ->
        update_c_tuple(node, fold_literal_list(tuple_es(node)))

      :cons ->
        update_c_cons(node, fold_literal(cons_hd(node)), fold_literal(cons_tl(node)))

      _ ->
        node
    end
  end

  defp fold_literal_list([e | es]) do
    [fold_literal(e) | fold_literal_list(es)]
  end

  defp fold_literal_list([]) do
    []
  end

  def unfold_literal(node) do
    case type(node) do
      :literal ->
        copy_ann(node, unfold_concrete(concrete(node)))

      _ ->
        node
    end
  end

  defp unfold_concrete(val) do
    case val do
      _ when is_tuple(val) ->
        c_tuple_skel(unfold_concrete_list(:erlang.tuple_to_list(val)))

      [h | t] ->
        c_cons_skel(unfold_concrete(h), unfold_concrete(t))

      _ ->
        abstract(val)
    end
  end

  defp unfold_concrete_list([e | es]) do
    [unfold_concrete(e) | unfold_concrete_list(es)]
  end

  defp unfold_concrete_list([]) do
    []
  end

  def c_module(name, exports, es) do
    r_c_module(name: name, exports: exports, attrs: [], defs: es)
  end

  def c_module(name, exports, attrs, es) do
    r_c_module(name: name, exports: exports, attrs: attrs, defs: es)
  end

  def ann_c_module(as, name, exports, es) do
    r_c_module(name: name, exports: exports, attrs: [], defs: es, anno: as)
  end

  def ann_c_module(as, name, exports, attrs, es) do
    r_c_module(name: name, exports: exports, attrs: attrs, defs: es, anno: as)
  end

  def update_c_module(node, name, exports, attrs, es) do
    r_c_module(name: name, exports: exports, attrs: attrs, defs: es, anno: get_ann(node))
  end

  def is_c_module(r_c_module()) do
    true
  end

  def is_c_module(_) do
    false
  end

  def module_name(node) do
    r_c_module(node, :name)
  end

  def module_exports(node) do
    r_c_module(node, :exports)
  end

  def module_attrs(node) do
    r_c_module(node, :attrs)
  end

  def module_defs(node) do
    r_c_module(node, :defs)
  end

  def module_vars(node) do
    for {f, _} <- module_defs(node) do
      f
    end
  end

  def c_int(value) do
    r_c_literal(val: value)
  end

  def ann_c_int(as, value) do
    r_c_literal(val: value, anno: as)
  end

  def is_c_int(r_c_literal(val: v)) when is_integer(v) do
    true
  end

  def is_c_int(_) do
    false
  end

  def int_val(node) do
    r_c_literal(node, :val)
  end

  def int_lit(node) do
    :erlang.integer_to_list(int_val(node))
  end

  def c_float(value) do
    r_c_literal(val: value)
  end

  def ann_c_float(as, value) do
    r_c_literal(val: value, anno: as)
  end

  def is_c_float(r_c_literal(val: v)) when is_float(v) do
    true
  end

  def is_c_float(_) do
    false
  end

  def float_val(node) do
    r_c_literal(node, :val)
  end

  def float_lit(node) do
    :erlang.float_to_list(float_val(node))
  end

  def c_atom(name) when is_atom(name) do
    r_c_literal(val: name)
  end

  def c_atom(name) do
    r_c_literal(val: :erlang.list_to_atom(name))
  end

  def ann_c_atom(as, name) when is_atom(name) do
    r_c_literal(val: name, anno: as)
  end

  def ann_c_atom(as, name) do
    r_c_literal(val: :erlang.list_to_atom(name), anno: as)
  end

  def is_c_atom(r_c_literal(val: v)) when is_atom(v) do
    true
  end

  def is_c_atom(_) do
    false
  end

  def atom_val(node) do
    r_c_literal(node, :val)
  end

  def atom_name(node) do
    :erlang.atom_to_list(atom_val(node))
  end

  def atom_lit(node) do
    :io_lib.write_string(atom_name(node), ?')
  end

  def c_char(value) when is_integer(value) and value >= 0 do
    r_c_literal(val: value)
  end

  def ann_c_char(as, value) do
    r_c_literal(val: value, anno: as)
  end

  def is_c_char(r_c_literal(val: v)) when is_integer(v) and v >= 0 do
    is_char_value(v)
  end

  def is_c_char(_) do
    false
  end

  def is_print_char(r_c_literal(val: v)) when is_integer(v) and v >= 0 do
    is_print_char_value(v)
  end

  def is_print_char(_) do
    false
  end

  def char_val(node) do
    r_c_literal(node, :val)
  end

  def char_lit(node) do
    :io_lib.write_char(char_val(node))
  end

  def c_string(value) do
    r_c_literal(val: value)
  end

  def ann_c_string(as, value) do
    r_c_literal(val: value, anno: as)
  end

  def is_c_string(r_c_literal(val: v)) do
    is_char_list(v)
  end

  def is_c_string(_) do
    false
  end

  def is_print_string(r_c_literal(val: v)) do
    is_print_char_list(v)
  end

  def is_print_string(_) do
    false
  end

  def string_val(node) do
    r_c_literal(node, :val)
  end

  def string_lit(node) do
    :io_lib.write_string(string_val(node))
  end

  def c_nil() do
    r_c_literal(val: [])
  end

  def ann_c_nil(as) do
    r_c_literal(val: [], anno: as)
  end

  def is_c_nil(r_c_literal(val: [])) do
    true
  end

  def is_c_nil(_) do
    false
  end

  def c_cons(r_c_literal(val: head), r_c_literal(val: tail)) do
    r_c_literal(val: [head | tail])
  end

  def c_cons(head, tail) do
    r_c_cons(hd: head, tl: tail)
  end

  def ann_c_cons(as, r_c_literal(val: head), r_c_literal(val: tail)) do
    r_c_literal(val: [head | tail], anno: as)
  end

  def ann_c_cons(as, head, tail) do
    r_c_cons(hd: head, tl: tail, anno: as)
  end

  def update_c_cons(node, r_c_literal(val: head), r_c_literal(val: tail)) do
    r_c_literal(val: [head | tail], anno: get_ann(node))
  end

  def update_c_cons(node, head, tail) do
    r_c_cons(hd: head, tl: tail, anno: get_ann(node))
  end

  def c_cons_skel(head, tail) do
    r_c_cons(hd: head, tl: tail)
  end

  def ann_c_cons_skel(as, head, tail) do
    r_c_cons(hd: head, tl: tail, anno: as)
  end

  def update_c_cons_skel(node, head, tail) do
    r_c_cons(hd: head, tl: tail, anno: get_ann(node))
  end

  def is_c_cons(r_c_cons()) do
    true
  end

  def is_c_cons(r_c_literal(val: [_ | _])) do
    true
  end

  def is_c_cons(_) do
    false
  end

  def cons_hd(r_c_cons(hd: head)) do
    head
  end

  def cons_hd(r_c_literal(val: [head | _])) do
    r_c_literal(val: head)
  end

  def cons_tl(r_c_cons(tl: tail)) do
    tail
  end

  def cons_tl(r_c_literal(val: [_ | tail])) do
    r_c_literal(val: tail)
  end

  def is_c_list(r_c_cons(tl: tail)) do
    is_c_list(tail)
  end

  def is_c_list(r_c_literal(val: v)) do
    is_proper_list(v)
  end

  def is_c_list(_) do
    false
  end

  defp is_proper_list([_ | tail]) do
    is_proper_list(tail)
  end

  defp is_proper_list([]) do
    true
  end

  defp is_proper_list(_) do
    false
  end

  def list_elements(r_c_cons(hd: head, tl: tail)) do
    [head | list_elements(tail)]
  end

  def list_elements(r_c_literal(val: v)) do
    abstract_list(v)
  end

  defp abstract_list([x | xs]) do
    [abstract(x) | abstract_list(xs)]
  end

  defp abstract_list([]) do
    []
  end

  def list_length(l) do
    list_length(l, 0)
  end

  defp list_length(r_c_cons(tl: tail), a) do
    list_length(tail, a + 1)
  end

  defp list_length(r_c_literal(val: v), a) do
    a + length(v)
  end

  def make_list(list) do
    ann_make_list([], list)
  end

  def make_list(list, tail) do
    ann_make_list([], list, tail)
  end

  def update_list(node, list) do
    ann_make_list(get_ann(node), list)
  end

  def update_list(node, list, tail) do
    ann_make_list(get_ann(node), list, tail)
  end

  def ann_make_list(as, list) do
    ann_make_list(as, list, :none)
  end

  def ann_make_list(as, [h | t], tail) do
    ann_c_cons(as, h, make_list(t, tail))
  end

  def ann_make_list(as, [], :none) do
    ann_c_nil(as)
  end

  def ann_make_list(_, [], node) do
    node
  end

  def is_c_map(r_c_map()) do
    true
  end

  def is_c_map(r_c_literal(val: v)) when is_map(v) do
    true
  end

  def is_c_map(_) do
    false
  end

  def map_es(r_c_literal(anno: as, val: m)) when is_map(m) do
    for {k, v} <- :maps.to_list(m) do
      ann_c_map_pair(
        as,
        r_c_literal(anno: as, val: :assoc),
        r_c_literal(anno: as, val: k),
        r_c_literal(anno: as, val: v)
      )
    end
  end

  def map_es(r_c_map(es: es)) do
    es
  end

  def map_arg(r_c_literal(anno: as, val: m)) when is_map(m) do
    r_c_literal(anno: as, val: %{})
  end

  def map_arg(r_c_map(arg: m)) do
    m
  end

  def c_map(pairs) do
    ann_c_map([], pairs)
  end

  def c_map_pattern(pairs) do
    r_c_map(es: pairs, is_pat: true)
  end

  def ann_c_map_pattern(as, pairs) do
    r_c_map(anno: as, es: pairs, is_pat: true)
  end

  def is_c_map_empty(r_c_map(es: [])) do
    true
  end

  def is_c_map_empty(r_c_literal(val: m))
      when is_map(m) and
             map_size(m) === 0 do
    true
  end

  def is_c_map_empty(_) do
    false
  end

  def is_c_map_pattern(r_c_map(is_pat: isPat)) do
    isPat
  end

  def ann_c_map(as, es) do
    ann_c_map(as, r_c_literal(val: %{}), es)
  end

  def ann_c_map(as, r_c_literal(val: m), es) when is_map(m) do
    fold_map_pairs(as, es, m)
  end

  def ann_c_map(as, m, es) do
    r_c_map(arg: m, es: es, anno: as)
  end

  defp fold_map_pairs(as, [], m) do
    r_c_literal(anno: as, val: m)
  end

  defp fold_map_pairs(
         as,
         [r_c_map_pair(op: r_c_literal(val: :assoc), key: ck, val: cv) = e | es],
         m
       ) do
    case is_lit_list([ck, cv]) do
      true ->
        [k, v] = lit_list_vals([ck, cv])
        fold_map_pairs(as, es, :maps.put(k, v, m))

      false ->
        r_c_map(arg: r_c_literal(val: m, anno: as), es: [e | es], anno: as)
    end
  end

  defp fold_map_pairs(
         as,
         [r_c_map_pair(op: r_c_literal(val: :exact), key: ck, val: cv) = e | es],
         m
       ) do
    case is_lit_list([ck, cv]) do
      true ->
        [k, v] = lit_list_vals([ck, cv])

        case :maps.is_key(k, m) do
          true ->
            fold_map_pairs(as, es, :maps.put(k, v, m))

          false ->
            r_c_map(arg: r_c_literal(val: m, anno: as), es: [e | es], anno: as)
        end

      false ->
        r_c_map(arg: r_c_literal(val: m, anno: as), es: [e | es], anno: as)
    end
  end

  def update_c_map(r_c_map(is_pat: true) = old, m, es) do
    r_c_map(old, arg: m, es: es)
  end

  def update_c_map(r_c_map(is_pat: false) = old, m, es) do
    ann_c_map(get_ann(old), m, es)
  end

  def map_pair_key(r_c_map_pair(key: k)) do
    k
  end

  def map_pair_val(r_c_map_pair(val: v)) do
    v
  end

  def map_pair_op(r_c_map_pair(op: op)) do
    op
  end

  def c_map_pair(key, val) do
    r_c_map_pair(op: r_c_literal(val: :assoc), key: key, val: val)
  end

  def c_map_pair_exact(key, val) do
    r_c_map_pair(op: r_c_literal(val: :exact), key: key, val: val)
  end

  def ann_c_map_pair(as, op, k, v) do
    r_c_map_pair(op: op, key: k, val: v, anno: as)
  end

  def update_c_map_pair(old, op, k, v) do
    r_c_map_pair(op: op, key: k, val: v, anno: get_ann(old))
  end

  def c_tuple(es) do
    case is_lit_list(es) do
      false ->
        r_c_tuple(es: es)

      true ->
        r_c_literal(val: :erlang.list_to_tuple(lit_list_vals(es)))
    end
  end

  def ann_c_tuple(as, es) do
    case is_lit_list(es) do
      false ->
        r_c_tuple(es: es, anno: as)

      true ->
        r_c_literal(
          val: :erlang.list_to_tuple(lit_list_vals(es)),
          anno: as
        )
    end
  end

  def update_c_tuple(node, es) do
    case is_lit_list(es) do
      false ->
        r_c_tuple(es: es, anno: get_ann(node))

      true ->
        r_c_literal(
          val: :erlang.list_to_tuple(lit_list_vals(es)),
          anno: get_ann(node)
        )
    end
  end

  def c_tuple_skel(es) do
    r_c_tuple(es: es)
  end

  def ann_c_tuple_skel(as, es) do
    r_c_tuple(es: es, anno: as)
  end

  def update_c_tuple_skel(old, es) do
    r_c_tuple(es: es, anno: get_ann(old))
  end

  def is_c_tuple(r_c_tuple()) do
    true
  end

  def is_c_tuple(r_c_literal(val: v)) when is_tuple(v) do
    true
  end

  def is_c_tuple(_) do
    false
  end

  def tuple_es(r_c_tuple(es: es)) do
    es
  end

  def tuple_es(r_c_literal(val: v)) do
    make_lit_list(:erlang.tuple_to_list(v))
  end

  def tuple_arity(r_c_tuple(es: es)) do
    length(es)
  end

  def tuple_arity(r_c_literal(val: v)) when is_tuple(v) do
    tuple_size(v)
  end

  def c_var(name) do
    r_c_var(name: name)
  end

  def ann_c_var(as, name) do
    r_c_var(name: name, anno: as)
  end

  def update_c_var(node, name) do
    r_c_var(name: name, anno: get_ann(node))
  end

  def is_c_var(r_c_var()) do
    true
  end

  def is_c_var(_) do
    false
  end

  def c_fname(atom, arity) do
    c_var({atom, arity})
  end

  def ann_c_fname(as, atom, arity) do
    ann_c_var(as, {atom, arity})
  end

  def update_c_fname(r_c_var(name: {_, arity}, anno: as), atom) do
    r_c_var(name: {atom, arity}, anno: as)
  end

  def update_c_fname(node, atom, arity) do
    update_c_var(node, {atom, arity})
  end

  def is_c_fname(r_c_var(name: {a, n}))
      when is_atom(a) and
             is_integer(n) and n >= 0 do
    true
  end

  def is_c_fname(_) do
    false
  end

  def var_name(node) do
    r_c_var(node, :name)
  end

  def fname_id(r_c_var(name: {a, _})) do
    a
  end

  def fname_arity(r_c_var(name: {_, n})) do
    n
  end

  def c_values(es) do
    r_c_values(es: es)
  end

  def ann_c_values(as, es) do
    r_c_values(es: es, anno: as)
  end

  def update_c_values(node, es) do
    r_c_values(es: es, anno: get_ann(node))
  end

  def is_c_values(r_c_values()) do
    true
  end

  def is_c_values(_) do
    false
  end

  def values_es(node) do
    r_c_values(node, :es)
  end

  def values_arity(node) do
    length(values_es(node))
  end

  def c_binary(segments) do
    r_c_binary(segments: segments)
  end

  def ann_c_binary(as, segments) do
    r_c_binary(segments: segments, anno: as)
  end

  def update_c_binary(node, segments) do
    r_c_binary(segments: segments, anno: get_ann(node))
  end

  def is_c_binary(r_c_binary()) do
    true
  end

  def is_c_binary(_) do
    false
  end

  def binary_segments(node) do
    r_c_binary(node, :segments)
  end

  def c_bitstr(val, size, unit, type, flags) do
    r_c_bitstr(val: val, size: size, unit: unit, type: type, flags: flags)
  end

  def c_bitstr(val, size, type, flags) do
    c_bitstr(val, size, abstract(1), type, flags)
  end

  def c_bitstr(val, type, flags) do
    c_bitstr(val, abstract(:all), abstract(1), type, flags)
  end

  def ann_c_bitstr(as, val, size, unit, type, flags) do
    r_c_bitstr(val: val, size: size, unit: unit, type: type, flags: flags, anno: as)
  end

  def ann_c_bitstr(as, value, size, type, flags) do
    ann_c_bitstr(as, value, size, abstract(1), type, flags)
  end

  def update_c_bitstr(node, val, size, unit, type, flags) do
    r_c_bitstr(val: val, size: size, unit: unit, type: type, flags: flags, anno: get_ann(node))
  end

  def update_c_bitstr(node, value, size, type, flags) do
    update_c_bitstr(node, value, size, abstract(1), type, flags)
  end

  def is_c_bitstr(r_c_bitstr()) do
    true
  end

  def is_c_bitstr(_) do
    false
  end

  def bitstr_val(node) do
    r_c_bitstr(node, :val)
  end

  def bitstr_size(node) do
    r_c_bitstr(node, :size)
  end

  def bitstr_bitsize(node) do
    size = r_c_bitstr(node, :size)

    case is_literal(size) do
      true ->
        case concrete(size) do
          :all ->
            :all

          :undefined ->
            'utf' ++ _ = :erlang.atom_to_list(concrete(r_c_bitstr(node, :type)))
            :utf

          s when is_integer(s) ->
            s * concrete(r_c_bitstr(node, :unit))
        end

      false ->
        :any
    end
  end

  def bitstr_unit(node) do
    r_c_bitstr(node, :unit)
  end

  def bitstr_type(node) do
    r_c_bitstr(node, :type)
  end

  def bitstr_flags(node) do
    r_c_bitstr(node, :flags)
  end

  def c_fun(variables, body) do
    r_c_fun(vars: variables, body: body)
  end

  def ann_c_fun(as, variables, body) do
    r_c_fun(vars: variables, body: body, anno: as)
  end

  def update_c_fun(node, variables, body) do
    r_c_fun(vars: variables, body: body, anno: get_ann(node))
  end

  def is_c_fun(r_c_fun()) do
    true
  end

  def is_c_fun(_) do
    false
  end

  def fun_vars(node) do
    r_c_fun(node, :vars)
  end

  def fun_body(node) do
    r_c_fun(node, :body)
  end

  def fun_arity(node) do
    length(fun_vars(node))
  end

  def c_seq(argument, body) do
    r_c_seq(arg: argument, body: body)
  end

  def ann_c_seq(as, argument, body) do
    r_c_seq(arg: argument, body: body, anno: as)
  end

  def update_c_seq(node, argument, body) do
    r_c_seq(arg: argument, body: body, anno: get_ann(node))
  end

  def is_c_seq(r_c_seq()) do
    true
  end

  def is_c_seq(_) do
    false
  end

  def seq_arg(node) do
    r_c_seq(node, :arg)
  end

  def seq_body(node) do
    r_c_seq(node, :body)
  end

  def c_let(variables, argument, body) do
    r_c_let(vars: variables, arg: argument, body: body)
  end

  def ann_c_let(as, variables, argument, body) do
    r_c_let(vars: variables, arg: argument, body: body, anno: as)
  end

  def update_c_let(node, variables, argument, body) do
    r_c_let(vars: variables, arg: argument, body: body, anno: get_ann(node))
  end

  def is_c_let(r_c_let()) do
    true
  end

  def is_c_let(_) do
    false
  end

  def let_vars(node) do
    r_c_let(node, :vars)
  end

  def let_arg(node) do
    r_c_let(node, :arg)
  end

  def let_body(node) do
    r_c_let(node, :body)
  end

  def let_arity(node) do
    length(let_vars(node))
  end

  def c_letrec(defs, body) do
    r_c_letrec(defs: defs, body: body)
  end

  def ann_c_letrec(as, defs, body) do
    r_c_letrec(defs: defs, body: body, anno: as)
  end

  def update_c_letrec(node, defs, body) do
    r_c_letrec(defs: defs, body: body, anno: get_ann(node))
  end

  def is_c_letrec(r_c_letrec()) do
    true
  end

  def is_c_letrec(_) do
    false
  end

  def letrec_defs(node) do
    r_c_letrec(node, :defs)
  end

  def letrec_body(node) do
    r_c_letrec(node, :body)
  end

  def letrec_vars(node) do
    for {f, _} <- letrec_defs(node) do
      f
    end
  end

  def c_case(expr, clauses) do
    r_c_case(arg: expr, clauses: clauses)
  end

  def ann_c_case(as, expr, clauses) do
    r_c_case(arg: expr, clauses: clauses, anno: as)
  end

  def update_c_case(node, expr, clauses) do
    r_c_case(arg: expr, clauses: clauses, anno: get_ann(node))
  end

  def is_c_case(r_c_case()) do
    true
  end

  def is_c_case(_) do
    false
  end

  def case_arg(node) do
    r_c_case(node, :arg)
  end

  def case_clauses(node) do
    r_c_case(node, :clauses)
  end

  def case_arity(node) do
    clause_arity(hd(case_clauses(node)))
  end

  def c_clause(patterns, body) do
    c_clause(patterns, c_atom(true), body)
  end

  def c_clause(patterns, guard, body) do
    r_c_clause(pats: patterns, guard: guard, body: body)
  end

  def ann_c_clause(as, patterns, body) do
    ann_c_clause(as, patterns, c_atom(true), body)
  end

  def ann_c_clause(as, patterns, guard, body) do
    r_c_clause(pats: patterns, guard: guard, body: body, anno: as)
  end

  def update_c_clause(node, patterns, guard, body) do
    r_c_clause(pats: patterns, guard: guard, body: body, anno: get_ann(node))
  end

  def is_c_clause(r_c_clause()) do
    true
  end

  def is_c_clause(_) do
    false
  end

  def clause_pats(node) do
    r_c_clause(node, :pats)
  end

  def clause_guard(node) do
    r_c_clause(node, :guard)
  end

  def clause_body(node) do
    r_c_clause(node, :body)
  end

  def clause_arity(node) do
    length(clause_pats(node))
  end

  def clause_vars(clause) do
    pat_list_vars(clause_pats(clause))
  end

  def pat_vars(node) do
    pat_vars(node, [])
  end

  defp pat_vars(node, vs) do
    case type(node) do
      :var ->
        [node | vs]

      :literal ->
        vs

      :cons ->
        pat_vars(cons_hd(node), pat_vars(cons_tl(node), vs))

      :tuple ->
        pat_list_vars(tuple_es(node), vs)

      :map ->
        pat_list_vars(map_es(node), vs)

      :map_pair ->
        pat_list_vars(
          [map_pair_op(node), map_pair_val(node)],
          vs
        )

      :binary ->
        pat_list_vars(binary_segments(node), vs)

      :bitstr ->
        pat_vars(bitstr_val(node), vs)

      :alias ->
        pat_vars(alias_pat(node), [alias_var(node) | vs])
    end
  end

  def pat_list_vars(ps) do
    pat_list_vars(ps, [])
  end

  defp pat_list_vars([p | ps], vs) do
    pat_list_vars(ps, pat_vars(p, vs))
  end

  defp pat_list_vars([], vs) do
    vs
  end

  def c_alias(var, pattern) do
    r_c_alias(var: var, pat: pattern)
  end

  def ann_c_alias(as, var, pattern) do
    r_c_alias(var: var, pat: pattern, anno: as)
  end

  def update_c_alias(node, var, pattern) do
    r_c_alias(var: var, pat: pattern, anno: get_ann(node))
  end

  def is_c_alias(r_c_alias()) do
    true
  end

  def is_c_alias(_) do
    false
  end

  def alias_var(node) do
    r_c_alias(node, :var)
  end

  def alias_pat(node) do
    r_c_alias(node, :pat)
  end

  def c_receive(clauses) do
    c_receive(clauses, c_atom(:infinity), c_atom(true))
  end

  def c_receive(clauses, timeout, action) do
    r_c_receive(clauses: clauses, timeout: timeout, action: action)
  end

  def ann_c_receive(as, clauses) do
    ann_c_receive(as, clauses, c_atom(:infinity), c_atom(true))
  end

  def ann_c_receive(as, clauses, timeout, action) do
    r_c_receive(clauses: clauses, timeout: timeout, action: action, anno: as)
  end

  def update_c_receive(node, clauses, timeout, action) do
    r_c_receive(clauses: clauses, timeout: timeout, action: action, anno: get_ann(node))
  end

  def is_c_receive(r_c_receive()) do
    true
  end

  def is_c_receive(_) do
    false
  end

  def receive_clauses(node) do
    r_c_receive(node, :clauses)
  end

  def receive_timeout(node) do
    r_c_receive(node, :timeout)
  end

  def receive_action(node) do
    r_c_receive(node, :action)
  end

  def c_apply(operator, arguments) do
    r_c_apply(op: operator, args: arguments)
  end

  def ann_c_apply(as, operator, arguments) do
    r_c_apply(op: operator, args: arguments, anno: as)
  end

  def update_c_apply(node, operator, arguments) do
    r_c_apply(op: operator, args: arguments, anno: get_ann(node))
  end

  def is_c_apply(r_c_apply()) do
    true
  end

  def is_c_apply(_) do
    false
  end

  def apply_op(node) do
    r_c_apply(node, :op)
  end

  def apply_args(node) do
    r_c_apply(node, :args)
  end

  def apply_arity(node) do
    length(apply_args(node))
  end

  def c_call(module, name, arguments) do
    r_c_call(module: module, name: name, args: arguments)
  end

  def ann_c_call(as, module, name, arguments) do
    r_c_call(module: module, name: name, args: arguments, anno: as)
  end

  def update_c_call(node, module, name, arguments) do
    r_c_call(module: module, name: name, args: arguments, anno: get_ann(node))
  end

  def is_c_call(r_c_call()) do
    true
  end

  def is_c_call(_) do
    false
  end

  def call_module(node) do
    r_c_call(node, :module)
  end

  def call_name(node) do
    r_c_call(node, :name)
  end

  def call_args(node) do
    r_c_call(node, :args)
  end

  def call_arity(node) do
    length(call_args(node))
  end

  def c_primop(name, arguments) do
    r_c_primop(name: name, args: arguments)
  end

  def ann_c_primop(as, name, arguments) do
    r_c_primop(name: name, args: arguments, anno: as)
  end

  def update_c_primop(node, name, arguments) do
    r_c_primop(name: name, args: arguments, anno: get_ann(node))
  end

  def is_c_primop(r_c_primop()) do
    true
  end

  def is_c_primop(_) do
    false
  end

  def primop_name(node) do
    r_c_primop(node, :name)
  end

  def primop_args(node) do
    r_c_primop(node, :args)
  end

  def primop_arity(node) do
    length(primop_args(node))
  end

  def c_try(expr, vs, body, evs, handler) do
    r_c_try(arg: expr, vars: vs, body: body, evars: evs, handler: handler)
  end

  def ann_c_try(as, expr, vs, body, evs, handler) do
    r_c_try(arg: expr, vars: vs, body: body, evars: evs, handler: handler, anno: as)
  end

  def update_c_try(node, expr, vs, body, evs, handler) do
    r_c_try(arg: expr, vars: vs, body: body, evars: evs, handler: handler, anno: get_ann(node))
  end

  def is_c_try(r_c_try()) do
    true
  end

  def is_c_try(_) do
    false
  end

  def try_arg(node) do
    r_c_try(node, :arg)
  end

  def try_vars(node) do
    r_c_try(node, :vars)
  end

  def try_body(node) do
    r_c_try(node, :body)
  end

  def try_evars(node) do
    r_c_try(node, :evars)
  end

  def try_handler(node) do
    r_c_try(node, :handler)
  end

  def c_catch(body) do
    r_c_catch(body: body)
  end

  def ann_c_catch(as, body) do
    r_c_catch(body: body, anno: as)
  end

  def update_c_catch(node, body) do
    r_c_catch(body: body, anno: get_ann(node))
  end

  def is_c_catch(r_c_catch()) do
    true
  end

  def is_c_catch(_) do
    false
  end

  def catch_body(node) do
    r_c_catch(node, :body)
  end

  def to_records(node) do
    node
  end

  def from_records(node) do
    node
  end

  def is_data(r_c_literal()) do
    true
  end

  def is_data(r_c_cons()) do
    true
  end

  def is_data(r_c_tuple()) do
    true
  end

  def is_data(_) do
    false
  end

  def data_type(r_c_literal(val: v)) do
    case v do
      [_ | _] ->
        :cons

      _ when is_tuple(v) ->
        :tuple

      _ ->
        {:atomic, v}
    end
  end

  def data_type(r_c_cons()) do
    :cons
  end

  def data_type(r_c_tuple()) do
    :tuple
  end

  def data_es(r_c_literal(val: v)) do
    case v do
      [head | tail] ->
        [r_c_literal(val: head), r_c_literal(val: tail)]

      _ when is_tuple(v) ->
        make_lit_list(:erlang.tuple_to_list(v))

      _ ->
        []
    end
  end

  def data_es(r_c_cons(hd: h, tl: t)) do
    [h, t]
  end

  def data_es(r_c_tuple(es: es)) do
    es
  end

  def data_arity(r_c_literal(val: v)) do
    case v do
      [_ | _] ->
        2

      _ when is_tuple(v) ->
        tuple_size(v)

      _ ->
        0
    end
  end

  def data_arity(r_c_cons()) do
    2
  end

  def data_arity(r_c_tuple(es: es)) do
    length(es)
  end

  def make_data(cType, es) do
    ann_make_data([], cType, es)
  end

  def ann_make_data(as, {:atomic, v}, []) do
    r_c_literal(val: v, anno: as)
  end

  def ann_make_data(as, :cons, [h, t]) do
    ann_c_cons(as, h, t)
  end

  def ann_make_data(as, :tuple, es) do
    ann_c_tuple(as, es)
  end

  def update_data(node, cType, es) do
    ann_make_data(get_ann(node), cType, es)
  end

  def make_data_skel(cType, es) do
    ann_make_data_skel([], cType, es)
  end

  def ann_make_data_skel(as, {:atomic, v}, []) do
    r_c_literal(val: v, anno: as)
  end

  def ann_make_data_skel(as, :cons, [h, t]) do
    ann_c_cons_skel(as, h, t)
  end

  def ann_make_data_skel(as, :tuple, es) do
    ann_c_tuple_skel(as, es)
  end

  def update_data_skel(node, cType, es) do
    ann_make_data_skel(get_ann(node), cType, es)
  end

  def subtrees(t) do
    case is_leaf(t) do
      true ->
        []

      false ->
        case type(t) do
          :values ->
            [values_es(t)]

          :binary ->
            [binary_segments(t)]

          :bitstr ->
            [
              [bitstr_val(t)],
              [bitstr_size(t)],
              [bitstr_unit(t)],
              [bitstr_type(t)],
              [bitstr_flags(t)]
            ]

          :cons ->
            [[cons_hd(t)], [cons_tl(t)]]

          :tuple ->
            [tuple_es(t)]

          :map ->
            [map_es(t)]

          :map_pair ->
            [[map_pair_op(t)], [map_pair_key(t)], [map_pair_val(t)]]

          :let ->
            [let_vars(t), [let_arg(t)], [let_body(t)]]

          :seq ->
            [[seq_arg(t)], [seq_body(t)]]

          :apply ->
            [[apply_op(t)], apply_args(t)]

          :call ->
            [[call_module(t)], [call_name(t)], call_args(t)]

          :primop ->
            [[primop_name(t)], primop_args(t)]

          :case ->
            [[case_arg(t)], case_clauses(t)]

          :clause ->
            [clause_pats(t), [clause_guard(t)], [clause_body(t)]]

          :alias ->
            [[alias_var(t)], [alias_pat(t)]]

          :fun ->
            [fun_vars(t), [fun_body(t)]]

          :receive ->
            [receive_clauses(t), [receive_timeout(t)], [receive_action(t)]]

          :try ->
            [[try_arg(t)], try_vars(t), [try_body(t)], try_evars(t), [try_handler(t)]]

          :catch ->
            [[catch_body(t)]]

          :letrec ->
            es = unfold_tuples(letrec_defs(t))
            [es, [letrec_body(t)]]

          :module ->
            as = unfold_tuples(module_attrs(t))
            es = unfold_tuples(module_defs(t))
            [[module_name(t)], module_exports(t), as, es]
        end
    end
  end

  def update_tree(node, gs) do
    ann_make_tree(get_ann(node), type(node), gs)
  end

  def update_tree(node, type, gs) do
    ann_make_tree(get_ann(node), type, gs)
  end

  def make_tree(type, gs) do
    ann_make_tree([], type, gs)
  end

  def ann_make_tree(as, :values, [es]) do
    ann_c_values(as, es)
  end

  def ann_make_tree(as, :binary, [ss]) do
    ann_c_binary(as, ss)
  end

  def ann_make_tree(as, :bitstr, [[v], [s], [u], [t], [fs]]) do
    ann_c_bitstr(as, v, s, u, t, fs)
  end

  def ann_make_tree(as, :cons, [[h], [t]]) do
    ann_c_cons(as, h, t)
  end

  def ann_make_tree(as, :tuple, [es]) do
    ann_c_tuple(as, es)
  end

  def ann_make_tree(as, :map, [es]) do
    ann_c_map(as, es)
  end

  def ann_make_tree(as, :map, [[a], es]) do
    ann_c_map(as, a, es)
  end

  def ann_make_tree(as, :map_pair, [[op], [k], [v]]) do
    ann_c_map_pair(as, op, k, v)
  end

  def ann_make_tree(as, :let, [vs, [a], [b]]) do
    ann_c_let(as, vs, a, b)
  end

  def ann_make_tree(as, :seq, [[a], [b]]) do
    ann_c_seq(as, a, b)
  end

  def ann_make_tree(as, :apply, [[op], es]) do
    ann_c_apply(as, op, es)
  end

  def ann_make_tree(as, :call, [[m], [n], es]) do
    ann_c_call(as, m, n, es)
  end

  def ann_make_tree(as, :primop, [[n], es]) do
    ann_c_primop(as, n, es)
  end

  def ann_make_tree(as, :case, [[a], cs]) do
    ann_c_case(as, a, cs)
  end

  def ann_make_tree(as, :clause, [ps, [g], [b]]) do
    ann_c_clause(as, ps, g, b)
  end

  def ann_make_tree(as, :alias, [[v], [p]]) do
    ann_c_alias(as, v, p)
  end

  def ann_make_tree(as, :fun, [vs, [b]]) do
    ann_c_fun(as, vs, b)
  end

  def ann_make_tree(as, :receive, [cs, [t], [a]]) do
    ann_c_receive(as, cs, t, a)
  end

  def ann_make_tree(as, :try, [[e], vs, [b], evs, [h]]) do
    ann_c_try(as, e, vs, b, evs, h)
  end

  def ann_make_tree(as, :catch, [[b]]) do
    ann_c_catch(as, b)
  end

  def ann_make_tree(as, :letrec, [es, [b]]) do
    ann_c_letrec(as, fold_tuples(es), b)
  end

  def ann_make_tree(as, :module, [[n], xs, es, ds]) do
    ann_c_module(as, n, xs, fold_tuples(es), fold_tuples(ds))
  end

  def meta(node) do
    case type(node) do
      :var ->
        case :lists.member(:meta_var, get_ann(node)) do
          false ->
            meta_0(:var, node)

          true ->
            set_ann(node, :lists.delete(:meta_var, get_ann(node)))
        end

      type ->
        meta_0(type, node)
    end
  end

  defp meta_0(type, node) do
    case get_ann(node) do
      [] ->
        meta_1(type, node)

      as ->
        meta_call(:set_ann, [meta_1(type, node), abstract(as)])
    end
  end

  defp meta_1(:literal, node) do
    case concrete(node) do
      v when is_atom(v) ->
        meta_call(:c_atom, [node])

      v when is_integer(v) ->
        meta_call(:c_int, [node])

      v when is_float(v) ->
        meta_call(:c_float, [node])

      [] ->
        meta_call(:c_nil, [])

      _ ->
        meta_call(:abstract, [node])
    end
  end

  defp meta_1(:var, node) do
    meta_call(:c_var, [abstract(var_name(node))])
  end

  defp meta_1(:values, node) do
    meta_call(
      :c_values,
      [make_list(meta_list(values_es(node)))]
    )
  end

  defp meta_1(:binary, node) do
    meta_call(
      :c_binary,
      [make_list(meta_list(binary_segments(node)))]
    )
  end

  defp meta_1(:bitstr, node) do
    meta_call(
      :c_bitstr,
      [
        meta(bitstr_val(node)),
        meta(bitstr_size(node)),
        meta(bitstr_unit(node)),
        meta(bitstr_type(node)),
        meta(bitstr_flags(node))
      ]
    )
  end

  defp meta_1(:cons, node) do
    case split_list(node) do
      {[h], node1} ->
        meta_call(:c_cons, [meta(h), meta(node1)])

      {l, node1} ->
        meta_call(
          :make_list,
          [make_list(meta_list(l)), meta(node1)]
        )
    end
  end

  defp meta_1(:tuple, node) do
    meta_call(
      :c_tuple,
      [make_list(meta_list(tuple_es(node)))]
    )
  end

  defp meta_1(:let, node) do
    meta_call(
      :c_let,
      [make_list(meta_list(let_vars(node))), meta(let_arg(node)), meta(let_body(node))]
    )
  end

  defp meta_1(:seq, node) do
    meta_call(
      :c_seq,
      [meta(seq_arg(node)), meta(seq_body(node))]
    )
  end

  defp meta_1(:apply, node) do
    meta_call(
      :c_apply,
      [meta(apply_op(node)), make_list(meta_list(apply_args(node)))]
    )
  end

  defp meta_1(:call, node) do
    meta_call(
      :c_call,
      [meta(call_module(node)), meta(call_name(node)), make_list(meta_list(call_args(node)))]
    )
  end

  defp meta_1(:primop, node) do
    meta_call(
      :c_primop,
      [meta(primop_name(node)), make_list(meta_list(primop_args(node)))]
    )
  end

  defp meta_1(:case, node) do
    meta_call(
      :c_case,
      [meta(case_arg(node)), make_list(meta_list(case_clauses(node)))]
    )
  end

  defp meta_1(:clause, node) do
    meta_call(
      :c_clause,
      [make_list(meta_list(clause_pats(node))), meta(clause_guard(node)), meta(clause_body(node))]
    )
  end

  defp meta_1(:alias, node) do
    meta_call(
      :c_alias,
      [meta(alias_var(node)), meta(alias_pat(node))]
    )
  end

  defp meta_1(:fun, node) do
    meta_call(
      :c_fun,
      [make_list(meta_list(fun_vars(node))), meta(fun_body(node))]
    )
  end

  defp meta_1(:receive, node) do
    meta_call(
      :c_receive,
      [
        make_list(meta_list(receive_clauses(node))),
        meta(receive_timeout(node)),
        meta(receive_action(node))
      ]
    )
  end

  defp meta_1(:try, node) do
    meta_call(
      :c_try,
      [
        meta(try_arg(node)),
        make_list(meta_list(try_vars(node))),
        meta(try_body(node)),
        make_list(meta_list(try_evars(node))),
        meta(try_handler(node))
      ]
    )
  end

  defp meta_1(:catch, node) do
    meta_call(:c_catch, [meta(catch_body(node))])
  end

  defp meta_1(:letrec, node) do
    meta_call(
      :c_letrec,
      [
        make_list(
          for {n, f} <- letrec_defs(node) do
            c_tuple([meta(n), meta(f)])
          end
        ),
        meta(letrec_body(node))
      ]
    )
  end

  defp meta_1(:module, node) do
    meta_call(
      :c_module,
      [
        meta(module_name(node)),
        make_list(meta_list(module_exports(node))),
        make_list(
          for {a, v} <- module_attrs(node) do
            c_tuple([meta(a), meta(v)])
          end
        ),
        make_list(
          for {n, f} <- module_defs(node) do
            c_tuple([meta(n), meta(f)])
          end
        )
      ]
    )
  end

  defp meta_call(f, as) do
    c_call(c_atom(:cerl), c_atom(f), as)
  end

  defp meta_list([t | ts]) do
    [meta(t) | meta_list(ts)]
  end

  defp meta_list([]) do
    []
  end

  defp split_list(node) do
    split_list(set_ann(node, []), [])
  end

  defp split_list(node, l) do
    a = get_ann(node)

    case type(node) do
      :cons when a === [] ->
        split_list(cons_tl(node), [cons_hd(node) | l])

      _ ->
        {:lists.reverse(l), node}
    end
  end

  defp is_lit_list([r_c_literal() | es]) do
    is_lit_list(es)
  end

  defp is_lit_list([_ | _]) do
    false
  end

  defp is_lit_list([]) do
    true
  end

  defp lit_list_vals([r_c_literal(val: v) | es]) do
    [v | lit_list_vals(es)]
  end

  defp lit_list_vals([]) do
    []
  end

  defp make_lit_list([v | vs]) do
    [r_c_literal(val: v) | make_lit_list(vs)]
  end

  defp make_lit_list([]) do
    []
  end

  defp is_char_value(v) when v >= ?\0 and v <= 255 do
    true
  end

  defp is_char_value(_) do
    false
  end

  defp is_print_char_value(v) when v >= ?\s and v <= ?~ do
    true
  end

  defp is_print_char_value(v) when v >= 160 and v <= 255 do
    true
  end

  defp is_print_char_value(v) when v === ?\b do
    true
  end

  defp is_print_char_value(v) when v === ?\d do
    true
  end

  defp is_print_char_value(v) when v === ?\e do
    true
  end

  defp is_print_char_value(v) when v === ?\f do
    true
  end

  defp is_print_char_value(v) when v === ?\n do
    true
  end

  defp is_print_char_value(v) when v === ?\r do
    true
  end

  defp is_print_char_value(v) when v === ?\s do
    true
  end

  defp is_print_char_value(v) when v === ?\t do
    true
  end

  defp is_print_char_value(v) when v === ?\v do
    true
  end

  defp is_print_char_value(v) when v === ?" do
    true
  end

  defp is_print_char_value(v) when v === ?' do
    true
  end

  defp is_print_char_value(v) when v === ?\\ do
    true
  end

  defp is_print_char_value(_) do
    false
  end

  defp is_char_list([v | vs]) when is_integer(v) do
    is_char_value(v) and is_char_list(vs)
  end

  defp is_char_list([]) do
    true
  end

  defp is_char_list(_) do
    false
  end

  defp is_print_char_list([v | vs]) when is_integer(v) do
    is_print_char_value(v) and is_print_char_list(vs)
  end

  defp is_print_char_list([]) do
    true
  end

  defp is_print_char_list(_) do
    false
  end

  defp unfold_tuples([{x, y} | ps]) do
    [x, y | unfold_tuples(ps)]
  end

  defp unfold_tuples([]) do
    []
  end

  defp fold_tuples([x, y | es]) do
    [{x, y} | fold_tuples(es)]
  end

  defp fold_tuples([]) do
    []
  end
end
