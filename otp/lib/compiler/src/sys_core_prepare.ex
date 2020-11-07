defmodule :m_sys_core_prepare do
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

  def module(mod0, _Opts) do
    count = :cerl_trees.next_free_variable_name(mod0)
    {mod, _} = :cerl_trees.mapfold(&rewrite_recv/2, count, mod0)
    {:ok, mod, []}
  end

  defp rewrite_recv(
         r_c_receive(clauses: [], timeout: timeout0, action: action),
         count0
       ) do
    false__ = r_c_literal(val: false)
    true__ = r_c_literal(val: true)
    {timeoutVal, count1} = new_var(count0)
    {loopName, count2} = new_func_varname(count1)
    loopFun = r_c_var(name: {loopName, 0})
    applyLoop = r_c_apply(op: loopFun, args: [])

    timeoutCs = [
      r_c_clause(
        pats: [true__],
        guard: true__,
        body: r_c_seq(arg: primop(:timeout), body: action)
      ),
      r_c_clause(pats: [false__], guard: true__, body: applyLoop)
    ]

    {timeoutBool, count4} = new_var(count2)
    timeoutCase = r_c_case(arg: timeoutBool, clauses: timeoutCs)

    timeoutLet =
      r_c_let(
        vars: [timeoutBool],
        arg: primop(:recv_wait_timeout, [timeoutVal]),
        body: timeoutCase
      )

    fun = r_c_fun(vars: [], body: timeoutLet)
    letrec = r_c_letrec(anno: [:letrec_goto], defs: [{loopFun, fun}], body: applyLoop)
    outerLet = r_c_let(vars: [timeoutVal], arg: timeout0, body: letrec)
    {outerLet, count4}
  end

  defp rewrite_recv(
         r_c_receive(clauses: cs0, timeout: timeout0, action: action),
         count0
       ) do
    false__ = r_c_literal(val: false)
    true__ = r_c_literal(val: true)
    {timeoutVal, count1} = new_var(count0)
    {loopName, count2} = new_func_varname(count1)
    loopFun = r_c_var(name: {loopName, 0})
    applyLoop = r_c_apply(op: loopFun, args: [])
    cs1 = rewrite_cs(cs0)
    recvNext = r_c_seq(arg: primop(:recv_next), body: applyLoop)

    recvNextC =
      r_c_clause(
        anno: [:compiler_generated],
        pats: [r_c_var(name: :Other)],
        guard: true__,
        body: recvNext
      )

    cs = cs1 ++ [recvNextC]
    {msg, count3} = new_var(count2)
    msgCase = r_c_case(arg: msg, clauses: cs)

    timeoutCs = [
      r_c_clause(
        pats: [true__],
        guard: true__,
        body: r_c_seq(arg: primop(:timeout), body: action)
      ),
      r_c_clause(pats: [false__], guard: true__, body: applyLoop)
    ]

    {timeoutBool, count4} = new_var(count3)
    timeoutCase = r_c_case(arg: timeoutBool, clauses: timeoutCs)

    timeoutLet =
      r_c_let(
        vars: [timeoutBool],
        arg: primop(:recv_wait_timeout, [timeoutVal]),
        body: timeoutCase
      )

    {peekSucceeded, count5} = new_var(count4)

    peekCs = [
      r_c_clause(pats: [true__], guard: true__, body: msgCase),
      r_c_clause(pats: [false__], guard: true__, body: timeoutLet)
    ]

    peekCase = r_c_case(arg: peekSucceeded, clauses: peekCs)
    peekLet = r_c_let(vars: [peekSucceeded, msg], arg: primop(:recv_peek_message), body: peekCase)
    fun = r_c_fun(vars: [], body: peekLet)
    letrec = r_c_letrec(anno: [:letrec_goto], defs: [{loopFun, fun}], body: applyLoop)
    outerLet = r_c_let(vars: [timeoutVal], arg: timeout0, body: letrec)
    {outerLet, count5}
  end

  defp rewrite_recv(tree, count) do
    {tree, count}
  end

  defp rewrite_cs([r_c_clause(body: b0) = c | cs]) do
    b = r_c_seq(arg: primop(:remove_message), body: b0)
    [r_c_clause(c, body: b) | rewrite_cs(cs)]
  end

  defp rewrite_cs([]) do
    []
  end

  defp primop(name) do
    primop(name, [])
  end

  defp primop(name, args) do
    r_c_primop(name: r_c_literal(val: name), args: args)
  end

  defp new_var(count) do
    {r_c_var(name: count), count + 1}
  end

  defp new_func_varname(count) do
    name = :erlang.list_to_atom('@pre' ++ :erlang.integer_to_list(count))
    {name, count + 1}
  end
end
