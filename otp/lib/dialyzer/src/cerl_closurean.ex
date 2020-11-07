defmodule :m_cerl_closurean do
  use Bitwise

  import :cerl,
    only: [
      ann_c_apply: 3,
      ann_c_fun: 3,
      ann_c_var: 2,
      apply_args: 1,
      apply_op: 1,
      atom_val: 1,
      binary_segments: 1,
      bitstr_size: 1,
      bitstr_val: 1,
      c_letrec: 2,
      c_nil: 0,
      c_seq: 2,
      c_tuple: 1,
      call_args: 1,
      call_module: 1,
      call_name: 1,
      case_arg: 1,
      case_clauses: 1,
      catch_body: 1,
      clause_body: 1,
      clause_guard: 1,
      clause_pats: 1,
      cons_hd: 1,
      cons_tl: 1,
      fun_body: 1,
      fun_vars: 1,
      get_ann: 1,
      is_c_atom: 1,
      let_arg: 1,
      let_body: 1,
      let_vars: 1,
      letrec_body: 1,
      letrec_defs: 1,
      module_defs: 1,
      module_exports: 1,
      pat_vars: 1,
      primop_args: 1,
      primop_name: 1,
      receive_action: 1,
      receive_clauses: 1,
      receive_timeout: 1,
      seq_arg: 1,
      seq_body: 1,
      set_ann: 2,
      try_arg: 1,
      try_body: 1,
      try_evars: 1,
      try_handler: 1,
      try_vars: 1,
      tuple_es: 1,
      type: 1,
      values_es: 1
    ]

  import :cerl_trees, only: [get_label: 1]

  def annotate(tree) do
    {xs, out, esc, deps, par} = analyze(tree)

    f = fn t ->
      case type(t) do
        :fun ->
          l = get_label(t)

          x =
            case :dict.find(l, deps) do
              {:ok, x1} ->
                x1

              :error ->
                set__new()
            end

          set_ann(
            t,
            append_ann(:callers, set__to_list(x), get_ann(t))
          )

        :apply ->
          l = get_label(t)

          x =
            case :dict.find(l, deps) do
              {:ok, x1} ->
                x1

              :error ->
                set__new()
            end

          set_ann(
            t,
            append_ann(:calls, set__to_list(x), get_ann(t))
          )

        _ ->
          t
      end
    end

    {:cerl_trees.map(f, tree), xs, out, esc, deps, par}
  end

  defp append_ann(tag, val, [x | xs]) do
    cond do
      tuple_size(x) >= 1 and
          :erlang.element(1, x) === tag ->
        append_ann(tag, val, xs)

      true ->
        [x | append_ann(tag, val, xs)]
    end
  end

  defp append_ann(tag, val, []) do
    [{tag, val}]
  end

  require Record

  Record.defrecord(:r_state, :state,
    vars: :undefined,
    out: :undefined,
    dep: :undefined,
    work: :undefined,
    funs: :undefined,
    par: :undefined
  )

  def analyze(tree) do
    external =
      ann_c_var(
        [{:label, :external}],
        {:external, 1}
      )

    escape = ann_c_var([{:label, :escape}], :Escape)

    extBody =
      c_seq(
        ann_c_apply([{:label, :loop}], external, [
          ann_c_apply([{:label, :external_call}], escape, [])
        ]),
        external
      )

    extFun = ann_c_fun([{:label, :external}], [escape], extBody)
    top = ann_c_var([{:label, :top}], {:top, 0})
    topFun = ann_c_fun([{:label, :top}], [], tree)

    startFun =
      ann_c_fun(
        [{:label, :start}],
        [],
        c_letrec(
          [{external, extFun}, {top, topFun}],
          c_nil()
        )
      )

    funs0 = :dict.new()
    vars0 = :dict.new()
    out0 = :dict.new()
    empty = empty()

    f = fn t, s = {fs, vs, os} ->
      case type(t) do
        :fun ->
          l = get_label(t)
          as = fun_vars(t)
          {:dict.store(l, t, fs), bind_vars_single(as, empty, vs), :dict.store(l, :none, os)}

        :letrec ->
          {fs, bind_defs(letrec_defs(t), vs), os}

        :module ->
          {fs, bind_defs(module_defs(t), vs), os}

        _ ->
          s
      end
    end

    {funs, vars, out} = :cerl_trees.fold(f, {funs0, vars0, out0}, startFun)
    vars1 = :dict.store(:escape, from_label_list([:top, :external]), vars)

    st =
      loop(
        startFun,
        :start,
        r_state(
          vars: vars1,
          out: out,
          dep: :dict.new(),
          work: init_work(),
          funs: funs,
          par: :dict.new()
        )
      )

    {:dict.fetch(:top, r_state(st, :out)),
     tidy_dict([:start, :top, :external], r_state(st, :out)),
     :dict.fetch(:escape, r_state(st, :vars)), tidy_dict([:loop], r_state(st, :dep)),
     r_state(st, :par)}
  end

  defp tidy_dict([x | xs], d) do
    tidy_dict(xs, :dict.erase(x, d))
  end

  defp tidy_dict([], d) do
    d
  end

  defp loop(t, l, st0) do
    xs0 = :dict.fetch(l, r_state(st0, :out))
    {xs, st1} = visit(fun_body(t), l, st0)

    {w, m} =
      case equal(xs0, xs) do
        true ->
          {r_state(st1, :work), r_state(st1, :out)}

        false ->
          m1 = :dict.store(l, xs, r_state(st1, :out))

          case :dict.find(l, r_state(st1, :dep)) do
            {:ok, s} ->
              {add_work(set__to_list(s), r_state(st1, :work)), m1}

            :error ->
              {r_state(st1, :work), m1}
          end
      end

    st2 = r_state(st1, out: m)

    case take_work(w) do
      {:ok, l1, w1} ->
        t1 = :dict.fetch(l1, r_state(st2, :funs))
        loop(t1, l1, r_state(st2, work: w1))

      :none ->
        st2
    end
  end

  defp visit(t, l, st) do
    case type(t) do
      :literal ->
        {[empty()], st}

      :var ->
        l1 = get_label(t)
        vars = r_state(st, :vars)

        case :dict.find(l1, vars) do
          {:ok, x} ->
            {[x], st}

          :error ->
            x = empty()
            st1 = r_state(st, vars: :dict.store(l1, x, vars))
            {[x], st1}
        end

      :fun ->
        l1 = get_label(t)

        st1 =
          r_state(st,
            work: add_work([l1], r_state(st, :work)),
            par: set_parent([l1], l, r_state(st, :par))
          )

        {[singleton(l1)], st1}

      :values ->
        visit_list(values_es(t), l, st)

      :cons ->
        {xs, st1} = visit_list([cons_hd(t), cons_tl(t)], l, st)
        {[join_single_list(xs)], st1}

      :tuple ->
        {xs, st1} = visit_list(tuple_es(t), l, st)
        {[join_single_list(xs)], st1}

      :let ->
        {xs, st1} = visit(let_arg(t), l, st)
        vars = bind_vars(let_vars(t), xs, r_state(st1, :vars))
        visit(let_body(t), l, r_state(st1, vars: vars))

      :seq ->
        {_, st1} = visit(seq_arg(t), l, st)
        visit(seq_body(t), l, st1)

      :apply ->
        {xs, st1} = visit(apply_op(t), l, st)
        {as, st2} = visit_list(apply_args(t), l, st1)

        case xs do
          [x] ->
            ls = set__to_list(x)
            out = r_state(st2, :out)

            xs1 =
              join_list(
                for lx <- ls do
                  :dict.fetch(lx, out)
                end
              )

            st3 = call_site(ls, l, as, st2)
            l1 = get_label(t)
            d = :dict.store(l1, x, r_state(st3, :dep))
            {xs1, r_state(st3, dep: d)}

          :none ->
            {:none, st2}
        end

      :call ->
        m = call_module(t)
        f = call_name(t)
        {_, st1} = visit(m, l, st)
        {_, st2} = visit(f, l, st1)
        {xs, st3} = visit_list(call_args(t), l, st2)
        remote_call(m, f, xs, st3)

      :primop ->
        as = primop_args(t)
        {xs, st1} = visit_list(as, l, st)
        primop_call(atom_val(primop_name(t)), length(xs), xs, st1)

      :case ->
        {xs, st1} = visit(case_arg(t), l, st)
        visit_clauses(xs, case_clauses(t), l, st1)

      :receive ->
        x = singleton(:external)
        {xs1, st1} = visit_clauses([x], receive_clauses(t), l, st)
        {_, st2} = visit(receive_timeout(t), l, st1)
        {xs2, st3} = visit(receive_action(t), l, st2)
        {join(xs1, xs2), st3}

      :try ->
        {xs1, st1} = visit(try_arg(t), l, st)
        x = singleton(:external)
        vars = bind_vars(try_vars(t), [x], r_state(st1, :vars))
        {xs2, st2} = visit(try_body(t), l, r_state(st1, vars: vars))
        evars = bind_vars(try_evars(t), [x, x, x], r_state(st2, :vars))
        {xs3, st3} = visit(try_handler(t), l, r_state(st2, vars: evars))
        {join(join(xs1, xs2), xs3), st3}

      :catch ->
        {_, st1} = visit(catch_body(t), l, st)
        {[singleton(:external)], st1}

      :binary ->
        {_, st1} = visit_list(binary_segments(t), l, st)
        {[empty()], st1}

      :bitstr ->
        {_, st1} = visit(bitstr_val(t), l, st)
        {_, st2} = visit(bitstr_size(t), l, st1)
        {:none, st2}

      :letrec ->
        ls =
          for {_, f} <- letrec_defs(t) do
            get_label(f)
          end

        st1 =
          r_state(st,
            work: add_work(ls, r_state(st, :work)),
            par: set_parent(ls, l, r_state(st, :par))
          )

        visit(letrec_body(t), l, st1)

      :module ->
        visit(
          c_letrec(
            module_defs(t),
            c_tuple(module_exports(t))
          ),
          l,
          st
        )
    end
  end

  defp visit_clause(t, xs, l, st) do
    vars = bind_pats(clause_pats(t), xs, r_state(st, :vars))
    {_, st1} = visit(clause_guard(t), l, r_state(st, vars: vars))
    visit(clause_body(t), l, st1)
  end

  defp visit_list([t | ts], l, st) do
    {xs, st1} = visit(t, l, st)
    {xs1, st2} = visit_list(ts, l, st1)

    x =
      case xs do
        [x1] ->
          x1

        :none ->
          :none
      end

    {[x | xs1], st2}
  end

  defp visit_list([], _L, st) do
    {[], st}
  end

  defp visit_clauses(xs, [t | ts], l, st) do
    {xs1, st1} = visit_clause(t, xs, l, st)
    {xs2, st2} = visit_clauses(xs, ts, l, st1)
    {join(xs1, xs2), st2}
  end

  defp visit_clauses(_, [], _L, st) do
    {:none, st}
  end

  defp bind_defs([{v, f} | ds], vars) do
    bind_defs(
      ds,
      :dict.store(get_label(v), singleton(get_label(f)), vars)
    )
  end

  defp bind_defs([], vars) do
    vars
  end

  defp bind_pats(ps, :none, vars) do
    bind_pats_single(ps, empty(), vars)
  end

  defp bind_pats(ps, xs, vars) do
    cond do
      length(xs) === length(ps) ->
        bind_pats_list(ps, xs, vars)

      true ->
        bind_pats_single(ps, empty(), vars)
    end
  end

  defp bind_pats_list([p | ps], [x | xs], vars) do
    bind_pats_list(ps, xs, bind_vars_single(pat_vars(p), x, vars))
  end

  defp bind_pats_list([], [], vars) do
    vars
  end

  defp bind_pats_single([p | ps], x, vars) do
    bind_pats_single(ps, x, bind_vars_single(pat_vars(p), x, vars))
  end

  defp bind_pats_single([], _X, vars) do
    vars
  end

  defp bind_vars(vs, :none, vars) do
    bind_vars_single(vs, empty(), vars)
  end

  defp bind_vars(vs, xs, vars) do
    cond do
      length(vs) === length(xs) ->
        bind_vars_list(vs, xs, vars)

      true ->
        bind_vars_single(vs, empty(), vars)
    end
  end

  defp bind_vars_list([v | vs], [x | xs], vars) do
    bind_vars_list(vs, xs, :dict.store(get_label(v), x, vars))
  end

  defp bind_vars_list([], [], vars) do
    vars
  end

  defp bind_vars_single([v | vs], x, vars) do
    bind_vars_single(vs, x, :dict.store(get_label(v), x, vars))
  end

  defp bind_vars_single([], _X, vars) do
    vars
  end

  defp call_site(ls, l, xs, st) do
    {d, w, v} =
      call_site(
        ls,
        l,
        xs,
        r_state(st, :dep),
        r_state(st, :work),
        r_state(st, :vars),
        r_state(st, :funs)
      )

    r_state(st, dep: d, work: w, vars: v)
  end

  defp call_site([:external | ls], t, xs, d, w, v, fs) do
    d1 = add_dep(:external, t, d)
    x = join_single_list(xs)

    case bind_arg(:escape, x, v) do
      {v1, true} ->
        {w1, v2} = update_esc(set__to_list(x), w, v1, fs)
        call_site(ls, t, xs, d1, add_work([:external], w1), v2, fs)

      {v1, false} ->
        call_site(ls, t, xs, d1, w, v1, fs)
    end
  end

  defp call_site([l | ls], t, xs, d, w, v, fs) do
    d1 = add_dep(l, t, d)
    vs = fun_vars(:dict.fetch(l, fs))

    case bind_args(vs, xs, v) do
      {v1, true} ->
        call_site(ls, t, xs, d1, add_work([l], w), v1, fs)

      {v1, false} ->
        call_site(ls, t, xs, d1, w, v1, fs)
    end
  end

  defp call_site([], _, _, d, w, v, _) do
    {d, w, v}
  end

  defp add_dep(source, target, deps) do
    case :dict.find(source, deps) do
      {:ok, x} ->
        case set__is_member(target, x) do
          true ->
            deps

          false ->
            :dict.store(source, set__add(target, x), deps)
        end

      :error ->
        :dict.store(source, set__singleton(target), deps)
    end
  end

  defp bind_args(vs, xs, vars) do
    cond do
      length(vs) === length(xs) ->
        bind_args(vs, xs, vars, false)

      true ->
        {vars, false}
    end
  end

  defp bind_args([v | vs], [x | xs], vars, ch) do
    l = get_label(v)
    {vars1, ch1} = bind_arg(l, x, vars, ch)
    bind_args(vs, xs, vars1, ch1)
  end

  defp bind_args([], [], vars, ch) do
    {vars, ch}
  end

  defp bind_args_single(vs, x, vars) do
    bind_args_single(vs, x, vars, false)
  end

  defp bind_args_single([v | vs], x, vars, ch) do
    l = get_label(v)
    {vars1, ch1} = bind_arg(l, x, vars, ch)
    bind_args_single(vs, x, vars1, ch1)
  end

  defp bind_args_single([], _, vars, ch) do
    {vars, ch}
  end

  defp bind_arg(l, x, vars) do
    bind_arg(l, x, vars, false)
  end

  defp bind_arg(l, x, vars, ch) do
    x0 = :dict.fetch(l, vars)
    x1 = join_single(x, x0)

    case equal_single(x0, x1) do
      true ->
        {vars, ch}

      false ->
        {:dict.store(l, x1, vars), true}
    end
  end

  defp escape([x], st) do
    vars = r_state(st, :vars)
    x0 = :dict.fetch(:escape, vars)
    x1 = join_single(x, x0)

    case equal_single(x0, x1) do
      true ->
        st

      false ->
        vars1 = :dict.store(:escape, x1, vars)

        {w, vars2} =
          update_esc(
            set__to_list(
              set__subtract(
                x,
                x0
              )
            ),
            r_state(st, :work),
            vars1,
            r_state(st, :funs)
          )

        r_state(st, work: add_work([:external], w), vars: vars2)
    end
  end

  defp update_esc(ls, w, v, fs) do
    update_esc(ls, singleton(:external), w, v, fs)
  end

  defp update_esc([:external | ls], x, w, v, fs) do
    update_esc(ls, x, w, v, fs)
  end

  defp update_esc([l | ls], x, w, v, fs) do
    vs = fun_vars(:dict.fetch(l, fs))

    case bind_args_single(vs, x, v) do
      {v1, true} ->
        update_esc(ls, x, add_work([l], w), v1, fs)

      {v1, false} ->
        update_esc(ls, x, w, v1, fs)
    end
  end

  defp update_esc([], _, w, v, _) do
    {w, v}
  end

  defp set_parent([l | ls], l1, d) do
    set_parent(ls, l1, :dict.store(l, l1, d))
  end

  defp set_parent([], _L1, d) do
    d
  end

  defp primop_call(f, a, xs, st0) do
    case is_pure_op(f, a) do
      false ->
        st1 =
          case is_escape_op(f, a) do
            true ->
              escape([join_single_list(xs)], st0)

            false ->
              st0
          end

        case is_literal_op(f, a) do
          true ->
            {:none, st1}

          false ->
            {[singleton(:external)], st1}
        end
    end
  end

  defp remote_call(m, f, xs, st) do
    case is_c_atom(m) and is_c_atom(f) do
      true ->
        remote_call_1(atom_val(m), atom_val(f), length(xs), xs, st)

      false ->
        {[singleton(:external)], escape([join_single_list(xs)], st)}
    end
  end

  defp remote_call_1(m, f, a, xs, st0) do
    case is_pure_op(m, f, a) do
      true ->
        case is_literal_op(m, f, a) do
          true ->
            {[empty()], st0}

          false ->
            {[join_single_list(xs)], st0}
        end

      false ->
        st1 =
          case is_escape_op(m, f, a) do
            true ->
              escape([join_single_list(xs)], st0)

            false ->
              st0
          end

        case is_literal_op(m, f, a) do
          true ->
            {[empty()], st1}

          false ->
            {[singleton(:external)], st1}
        end
    end
  end

  defp join(:none, xs2) do
    xs2
  end

  defp join(xs1, :none) do
    xs1
  end

  defp join(xs1, xs2) do
    cond do
      length(xs1) === length(xs2) ->
        join_1(xs1, xs2)

      true ->
        :none
    end
  end

  defp join_1([x1 | xs1], [x2 | xs2]) do
    [join_single(x1, x2) | join_1(xs1, xs2)]
  end

  defp join_1([], []) do
    []
  end

  defp empty() do
    set__new()
  end

  defp singleton(x) do
    set__singleton(x)
  end

  defp from_label_list(x) do
    set__from_list(x)
  end

  defp join_single(:none, y) do
    y
  end

  defp join_single(x, :none) do
    x
  end

  defp join_single(x, y) do
    set__union(x, y)
  end

  defp join_list([xs | xss]) do
    join(xs, join_list(xss))
  end

  defp join_list([]) do
    :none
  end

  defp join_single_list([x | xs]) do
    join_single(x, join_single_list(xs))
  end

  defp join_single_list([]) do
    empty()
  end

  defp equal(:none, :none) do
    true
  end

  defp equal(:none, _) do
    false
  end

  defp equal(_, :none) do
    false
  end

  defp equal(x1, x2) do
    equal_1(x1, x2)
  end

  defp equal_1([x1 | xs1], [x2 | xs2]) do
    equal_single(x1, x2) and equal_1(xs1, xs2)
  end

  defp equal_1([], []) do
    true
  end

  defp equal_1(_, _) do
    false
  end

  defp equal_single(x, y) do
    set__equal(x, y)
  end

  defp set__new() do
    []
  end

  defp set__singleton(x) do
    [x]
  end

  defp set__to_list(s) do
    s
  end

  defp set__from_list(s) do
    :ordsets.from_list(s)
  end

  defp set__union(x, y) do
    :ordsets.union(x, y)
  end

  defp set__add(x, s) do
    :ordsets.add_element(x, s)
  end

  defp set__is_member(x, s) do
    :ordsets.is_element(x, s)
  end

  defp set__subtract(x, y) do
    :ordsets.subtract(x, y)
  end

  defp set__equal(x, y) do
    x === y
  end

  defp queue__new() do
    {[], []}
  end

  defp queue__put(x, {in__, out}) do
    {[x | in__], out}
  end

  defp queue__get({in__, [x | out]}) do
    {:ok, x, {in__, out}}
  end

  defp queue__get({[], _}) do
    :empty
  end

  defp queue__get({in__, _}) do
    [x | in1] = :lists.reverse(in__)
    {:ok, x, {[], in1}}
  end

  defp init_work() do
    {queue__new(), :sets.new()}
  end

  defp add_work(ls, {q, set}) do
    add_work(ls, q, set)
  end

  defp add_work([l | ls], q, set) do
    case :sets.is_element(l, set) do
      true ->
        add_work(ls, q, set)

      false ->
        add_work(ls, queue__put(l, q), :sets.add_element(l, set))
    end
  end

  defp add_work([], q, set) do
    {q, set}
  end

  defp take_work({queue0, set0}) do
    case queue__get(queue0) do
      {:ok, l, queue1} ->
        set1 = :sets.del_element(l, set0)
        {:ok, l, {queue1, set1}}

      :empty ->
        :none
    end
  end

  def is_escape_op(:match_fail, 1) do
    false
  end

  def is_escape_op(:recv_wait_timeout, 1) do
    false
  end

  def is_escape_op(f, a) when is_atom(f) and is_integer(a) do
    true
  end

  def is_escape_op(:erlang, :error, 1) do
    false
  end

  def is_escape_op(:erlang, :error, 2) do
    false
  end

  def is_escape_op(m, f, a)
      when is_atom(m) and is_atom(f) and
             is_integer(a) do
    true
  end

  def is_literal_op(:recv_wait_timeout, 1) do
    true
  end

  def is_literal_op(:match_fail, 1) do
    true
  end

  def is_literal_op(f, a) when is_atom(f) and is_integer(a) do
    false
  end

  def is_literal_op(:erlang, :+, 2) do
    true
  end

  def is_literal_op(:erlang, :-, 2) do
    true
  end

  def is_literal_op(:erlang, :*, 2) do
    true
  end

  def is_literal_op(:erlang, :/, 2) do
    true
  end

  def is_literal_op(:erlang, :"=:=", 2) do
    true
  end

  def is_literal_op(:erlang, :==, 2) do
    true
  end

  def is_literal_op(:erlang, :"=/=", 2) do
    true
  end

  def is_literal_op(:erlang, :"/=", 2) do
    true
  end

  def is_literal_op(:erlang, :<, 2) do
    true
  end

  def is_literal_op(:erlang, :"=<", 2) do
    true
  end

  def is_literal_op(:erlang, :>, 2) do
    true
  end

  def is_literal_op(:erlang, :>=, 2) do
    true
  end

  def is_literal_op(:erlang, :and, 2) do
    true
  end

  def is_literal_op(:erlang, :or, 2) do
    true
  end

  def is_literal_op(:erlang, :not, 1) do
    true
  end

  def is_literal_op(:erlang, :length, 1) do
    true
  end

  def is_literal_op(:erlang, :size, 1) do
    true
  end

  def is_literal_op(:erlang, :fun_info, 1) do
    true
  end

  def is_literal_op(:erlang, :fun_info, 2) do
    true
  end

  def is_literal_op(:erlang, :fun_to_list, 1) do
    true
  end

  def is_literal_op(:erlang, :throw, 1) do
    true
  end

  def is_literal_op(:erlang, :exit, 1) do
    true
  end

  def is_literal_op(:erlang, :error, 1) do
    true
  end

  def is_literal_op(:erlang, :error, 2) do
    true
  end

  def is_literal_op(m, f, a)
      when is_atom(m) and is_atom(f) and
             is_integer(a) do
    false
  end

  defp is_pure_op(f, a) when is_atom(f) and is_integer(a) do
    false
  end

  defp is_pure_op(m, f, a) do
    :erl_bifs.is_pure(m, f, a)
  end
end
