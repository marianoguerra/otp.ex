defmodule :m_beam_listing do
  use Bitwise
  import :lists, only: [foreach: 2]
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

  Record.defrecord(:r_b_module, :b_module,
    anno: %{},
    name: :undefined,
    exports: :undefined,
    attributes: :undefined,
    body: :undefined
  )

  Record.defrecord(:r_b_function, :b_function,
    anno: %{},
    args: :undefined,
    bs: :undefined,
    cnt: :undefined
  )

  Record.defrecord(:r_b_blk, :b_blk, anno: %{}, is: :undefined, last: :undefined)
  Record.defrecord(:r_b_set, :b_set, anno: %{}, dst: :none, op: :undefined, args: [])
  Record.defrecord(:r_b_ret, :b_ret, anno: %{}, arg: :undefined)

  Record.defrecord(:r_b_br, :b_br, anno: %{}, bool: :undefined, succ: :undefined, fail: :undefined)

  Record.defrecord(:r_b_switch, :b_switch,
    anno: %{},
    arg: :undefined,
    fail: :undefined,
    list: :undefined
  )

  Record.defrecord(:r_b_var, :b_var, name: :undefined)
  Record.defrecord(:r_b_literal, :b_literal, val: :undefined)
  Record.defrecord(:r_b_remote, :b_remote, mod: :undefined, name: :undefined, arity: :undefined)

  Record.defrecord(:r_b_local, :b_local,
    name: :undefined,
    arity: :undefined
  )

  Record.defrecord(:r_function, :function,
    name: :undefined,
    arity: :undefined,
    entry: :undefined,
    code: []
  )

  Record.defrecord(:r_beam_file, :beam_file,
    module: :undefined,
    labeled_exports: [],
    attributes: [],
    compile_info: [],
    code: []
  )

  def module(file, r_c_module() = core) do
    :io.put_chars(file, :core_pp.format(core))
  end

  def module(file, r_k_mdef() = kern) do
    :io.put_chars(file, :v3_kernel_pp.format(kern))
  end

  def module(
        file,
        r_b_module(name: mod, exports: exp, attributes: attr, body: fs)
      ) do
    :io.format(file, 'module ~p.\n', [mod])
    :io.format(file, 'exports ~p.\n', [exp])
    :io.format(file, 'attributes ~p.\n\n', [attr])

    pP =
      for f <- fs do
        :beam_ssa_pp.format_function(f)
      end

    :io.put_chars(file, :lists.join(?\n, pP))
  end

  def module(stream, {mod, exp, attr, code, numLabels}) do
    :io.format(stream, '{module, ~p}.  %% version = ~w\n', [mod, :beam_opcodes.format_number()])
    :io.format(stream, '\n{exports, ~p}.\n', [exp])
    :io.format(stream, '\n{attributes, ~p}.\n', [attr])
    :io.format(stream, '\n{labels, ~p}.\n', [numLabels])

    foreach(
      fn {:function, name, arity, entry, asm} ->
        :io.format(stream, '\n\n{function, ~w, ~w, ~w}.\n', [name, arity, entry])
        :io.put_chars(stream, format_asm(asm))
      end,
      code
    )
  end

  def module(stream, [_ | _] = fs) do
    foreach(
      fn f ->
        :io.format(stream, '~p.\n', [f])
      end,
      fs
    )
  end

  defp format_asm([{:label, l} | is]) do
    [:io_lib.format('  {label,~p}.\n', [l]) | format_asm(is)]
  end

  defp format_asm([i | is]) do
    [[:io_lib.format('    ~p', [i]), '.\n'] | format_asm(is)]
  end

  defp format_asm([]) do
    []
  end
end
