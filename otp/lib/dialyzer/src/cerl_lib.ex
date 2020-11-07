defmodule :m_cerl_lib do
  use Bitwise

  def is_simple_clause(c) do
    case :cerl.clause_pats(c) do
      [_P] ->
        g = :cerl.clause_guard(c)

        case :cerl_clauses.eval_guard(g) do
          {:value, true} ->
            true

          _ ->
            false
        end

      _ ->
        false
    end
  end

  def is_bool_switch([c1, c2]) do
    case is_simple_clause(c1) and is_simple_clause(c2) do
      true ->
        [p1] = :cerl.clause_pats(c1)
        [p2] = :cerl.clause_pats(c2)

        case :cerl.is_c_atom(p1) and :cerl.is_c_atom(p2) do
          true ->
            a1 = :cerl.concrete(p1)
            a2 = :cerl.concrete(p2)
            is_boolean(a1) and is_boolean(a2) and a1 !== a2

          false ->
            false
        end

      false ->
        false
    end
  end

  def is_bool_switch(_) do
    false
  end

  def bool_switch_cases([c1, c2]) do
    b1 = :cerl.clause_body(c1)
    b2 = :cerl.clause_body(c2)
    [p1] = :cerl.clause_pats(c1)

    case :cerl.concrete(p1) do
      true ->
        {b1, b2}

      false ->
        {b2, b1}
    end
  end

  defp default_check(_Property, _Function) do
    false
  end

  def is_safe_expr(e, check) do
    case :cerl.type(e) do
      :literal ->
        true

      :var ->
        true

      :fun ->
        true

      :values ->
        is_safe_expr_list(:cerl.values_es(e), check)

      :tuple ->
        is_safe_expr_list(:cerl.tuple_es(e), check)

      :cons ->
        case is_safe_expr(:cerl.cons_hd(e), check) do
          true ->
            is_safe_expr(:cerl.cons_tl(e), check)

          false ->
            false
        end

      :let ->
        case is_safe_expr(:cerl.let_arg(e), check) do
          true ->
            is_safe_expr(:cerl.let_body(e), check)

          false ->
            false
        end

      :letrec ->
        is_safe_expr(:cerl.letrec_body(e), check)

      :seq ->
        case is_safe_expr(:cerl.seq_arg(e), check) do
          true ->
            is_safe_expr(:cerl.seq_body(e), check)

          false ->
            false
        end

      :catch ->
        is_safe_expr(:cerl.catch_body(e), check)

      :try ->
        case is_safe_expr(:cerl.try_arg(e), check) do
          true ->
            is_safe_expr(:cerl.try_body(e), check)

          false ->
            case is_pure_expr(:cerl.try_arg(e), check) do
              true ->
                case is_safe_expr(:cerl.try_body(e), check) do
                  true ->
                    is_safe_expr(:cerl.try_handler(e), check)

                  false ->
                    false
                end

              false ->
                false
            end
        end

      :primop ->
        name = :cerl.atom_val(:cerl.primop_name(e))
        as = :cerl.primop_args(e)

        case check.(:safe, {name, length(as)}) do
          true ->
            is_safe_expr_list(as, check)

          false ->
            false
        end

      :call ->
        module = :cerl.call_module(e)
        name = :cerl.call_name(e)

        case :erlang.and(
               :cerl.is_c_atom(module),
               :cerl.is_c_atom(name)
             ) do
          true ->
            m = :cerl.atom_val(module)
            f = :cerl.atom_val(name)
            as = :cerl.call_args(e)

            case check.(:safe, {m, f, length(as)}) do
              true ->
                is_safe_expr_list(as, check)

              false ->
                false
            end

          false ->
            false
        end

      _ ->
        false
    end
  end

  defp is_safe_expr_list([e | es], check) do
    case is_safe_expr(e, check) do
      true ->
        is_safe_expr_list(es, check)

      false ->
        false
    end
  end

  defp is_safe_expr_list([], _Check) do
    true
  end

  defp is_pure_expr(e, check) do
    case :cerl.type(e) do
      :literal ->
        true

      :var ->
        true

      :fun ->
        true

      :values ->
        is_pure_expr_list(:cerl.values_es(e), check)

      :tuple ->
        is_pure_expr_list(:cerl.tuple_es(e), check)

      :cons ->
        case is_pure_expr(:cerl.cons_hd(e), check) do
          true ->
            is_pure_expr(:cerl.cons_tl(e), check)

          false ->
            false
        end

      :let ->
        case is_pure_expr(:cerl.let_arg(e), check) do
          true ->
            is_pure_expr(:cerl.let_body(e), check)

          false ->
            false
        end

      :letrec ->
        is_pure_expr(:cerl.letrec_body(e), check)

      :seq ->
        case is_pure_expr(:cerl.seq_arg(e), check) do
          true ->
            is_pure_expr(:cerl.seq_body(e), check)

          false ->
            false
        end

      :catch ->
        is_pure_expr(:cerl.catch_body(e), check)

      :try ->
        case is_pure_expr(:cerl.try_arg(e), check) do
          true ->
            case is_pure_expr(:cerl.try_body(e), check) do
              true ->
                is_pure_expr(:cerl.try_handler(e), check)

              false ->
                false
            end

          false ->
            false
        end

      :primop ->
        name = :cerl.atom_val(:cerl.primop_name(e))
        as = :cerl.primop_args(e)

        case check.(:pure, {name, length(as)}) do
          true ->
            is_pure_expr_list(as, check)

          false ->
            false
        end

      :call ->
        module = :cerl.call_module(e)
        name = :cerl.call_name(e)

        case :erlang.and(
               :cerl.is_c_atom(module),
               :cerl.is_c_atom(name)
             ) do
          true ->
            m = :cerl.atom_val(module)
            f = :cerl.atom_val(name)
            as = :cerl.call_args(e)

            case check.(:pure, {m, f, length(as)}) do
              true ->
                is_pure_expr_list(as, check)

              false ->
                false
            end

          false ->
            false
        end

      _ ->
        false
    end
  end

  defp is_pure_expr_list([e | es], check) do
    case is_pure_expr(e, check) do
      true ->
        is_pure_expr_list(es, check)

      false ->
        false
    end
  end

  defp is_pure_expr_list([], _Check) do
    true
  end

  def reduce_expr(e) do
    check = &default_check/2
    reduce_expr(e, check)
  end

  defp reduce_expr(e, check) do
    case :cerl.type(e) do
      :values ->
        case :cerl.values_es(e) do
          [e1] ->
            reduce_expr(e1, check)

          _ ->
            e
        end

      :seq ->
        a = reduce_expr(:cerl.seq_arg(e), check)
        b = reduce_expr(:cerl.seq_body(e), check)

        case is_safe_expr(a, check) do
          true ->
            b

          false ->
            case :cerl.is_c_seq(b) do
              true ->
                b1 = :cerl.seq_arg(b)
                b2 = :cerl.seq_body(b)
                :cerl.c_seq(:cerl.c_seq(a, b1), b2)

              false ->
                :cerl.c_seq(a, b)
            end
        end

      :let ->
        a = reduce_expr(:cerl.let_arg(e), check)

        case :cerl.is_c_seq(a) do
          true ->
            a1 = :cerl.seq_arg(a)
            a2 = :cerl.seq_body(a)
            e1 = :cerl.update_c_let(e, :cerl.let_vars(e), a2, :cerl.let_body(e))
            :cerl.c_seq(a1, reduce_expr(e1, check))

          false ->
            b = reduce_expr(:cerl.let_body(e), check)
            vs = :cerl.let_vars(e)

            case :cerl.type(b) do
              :var when length(vs) === 1 ->
                [v] = vs
                n1 = :cerl.var_name(v)
                n2 = :cerl.var_name(b)

                cond do
                  n1 === n2 ->
                    a

                  true ->
                    reduce_expr(:cerl.c_seq(a, b), check)
                end

              :literal ->
                reduce_expr(:cerl.c_seq(a, b), check)

              _ ->
                :cerl.update_c_let(e, vs, a, b)
            end
        end

      :try ->
        a = reduce_expr(:cerl.try_arg(e), check)
        b = reduce_expr(:cerl.try_body(e), check)

        case is_safe_expr(a, check) do
          true ->
            b

          false ->
            :cerl.update_c_try(
              e,
              a,
              :cerl.try_vars(e),
              b,
              :cerl.try_evars(e),
              :cerl.try_handler(e)
            )
        end

      :catch ->
        b = reduce_expr(:cerl.catch_body(e), check)

        case is_safe_expr(b, check) do
          true ->
            b

          false ->
            :cerl.update_c_catch(e, b)
        end

      _ ->
        e
    end
  end
end
