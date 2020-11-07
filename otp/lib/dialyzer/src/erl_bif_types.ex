defmodule :m_erl_bif_types do
  use Bitwise

  import :erl_types,
    only: [
      number_max: 2,
      number_min: 2,
      t_any: 0,
      t_arity: 0,
      t_atom: 0,
      t_atom: 1,
      t_atom_vals: 2,
      t_atoms: 1,
      t_binary: 0,
      t_bitstr: 0,
      t_boolean: 0,
      t_byte: 0,
      t_cons: 0,
      t_cons: 2,
      t_cons_hd: 1,
      t_cons_tl: 1,
      t_fixnum: 0,
      t_float: 0,
      t_from_range: 2,
      t_from_term: 1,
      t_fun: 0,
      t_fun: 2,
      t_fun_args: 2,
      t_fun_range: 2,
      t_has_opaque_subtype: 2,
      t_identifier: 0,
      t_inf: 3,
      t_integer: 0,
      t_integer: 1,
      t_integers: 1,
      t_is_any: 1,
      t_is_atom: 2,
      t_is_binary: 2,
      t_is_bitstr: 2,
      t_is_boolean: 2,
      t_is_cons: 2,
      t_is_float: 2,
      t_is_fun: 2,
      t_is_integer: 2,
      t_is_map: 2,
      t_is_maybe_improper_list: 2,
      t_is_nil: 1,
      t_is_nil: 2,
      t_is_none: 1,
      t_is_none_or_unit: 1,
      t_is_number: 2,
      t_is_pid: 2,
      t_is_port: 2,
      t_is_reference: 2,
      t_is_subtype: 2,
      t_is_tuple: 2,
      t_list: 0,
      t_list: 1,
      t_list_elements: 2,
      t_list_termination: 2,
      t_map: 0,
      t_map: 3,
      t_map_def_key: 2,
      t_map_def_val: 2,
      t_map_entries: 2,
      t_map_get: 3,
      t_map_is_key: 3,
      t_map_pairwise_merge: 4,
      t_map_put: 3,
      t_map_remove: 3,
      t_map_update: 3,
      t_maybe_improper_list: 0,
      t_mfa: 0,
      t_module: 0,
      t_nil: 0,
      t_node: 0,
      t_non_neg_fixnum: 0,
      t_non_neg_integer: 0,
      t_none: 0,
      t_nonempty_list: 0,
      t_nonempty_list: 1,
      t_number: 0,
      t_number_vals: 2,
      t_pid: 0,
      t_port: 0,
      t_pos_fixnum: 0,
      t_pos_integer: 0,
      t_reference: 0,
      t_string: 0,
      t_subtract: 2,
      t_sup: 1,
      t_sup: 2,
      t_tuple: 0,
      t_tuple: 1,
      t_tuple_args: 2,
      t_tuple_size: 2,
      t_tuple_subtypes: 2
    ]

  def type(m, f, a) do
    type(m, f, a, any_list(a), [])
  end

  def type(m, f, a, xs) do
    type(m, f, a, xs, :universe)
  end

  def type(:erlang, :halt, 0, _, _) do
    t_none()
  end

  def type(:erlang, :halt, 1, _, _) do
    t_none()
  end

  def type(:erlang, :halt, 2, _, _) do
    t_none()
  end

  def type(:erlang, :exit, 1, _, _) do
    t_none()
  end

  def type(:erlang, :error, 1, _, _) do
    t_none()
  end

  def type(:erlang, :error, 2, _, _) do
    t_none()
  end

  def type(:erlang, :throw, 1, _, _) do
    t_none()
  end

  def type(:erlang, :==, 2, xs = [x1, x2], opaques) do
    case t_is_atom(x1, opaques) and
           t_is_atom(
             x2,
             opaques
           ) do
      true ->
        type(:erlang, :"=:=", 2, xs, opaques)

      false ->
        case t_is_integer(x1, opaques) and
               t_is_integer(
                 x2,
                 opaques
               ) do
          true ->
            type(:erlang, :"=:=", 2, xs, opaques)

          false ->
            strict2(xs, t_boolean())
        end
    end
  end

  def type(:erlang, :"/=", 2, xs = [x1, x2], opaques) do
    case t_is_atom(x1, opaques) and
           t_is_atom(
             x2,
             opaques
           ) do
      true ->
        type(:erlang, :"=/=", 2, xs, opaques)

      false ->
        case t_is_integer(x1, opaques) and
               t_is_integer(
                 x2,
                 opaques
               ) do
          true ->
            type(:erlang, :"=/=", 2, xs, opaques)

          false ->
            strict2(xs, t_boolean())
        end
    end
  end

  def type(:erlang, :"=:=", 2, xs = [lhs, rhs], opaques) do
    ans =
      case t_is_none(t_inf(lhs, rhs, opaques)) do
        true ->
          t_atom(false)

        false ->
          case t_is_atom(lhs, opaques) and
                 t_is_atom(
                   rhs,
                   opaques
                 ) do
            true ->
              case {t_atom_vals(lhs, opaques), t_atom_vals(rhs, opaques)} do
                {:unknown, _} ->
                  t_boolean()

                {_, :unknown} ->
                  t_boolean()

                {[x], [x]} ->
                  t_atom(true)

                {lhsVals, rhsVals} ->
                  case :lists.all(
                         fn {x, y} ->
                           x !== y
                         end,
                         for x <- lhsVals, y <- rhsVals do
                           {x, y}
                         end
                       ) do
                    true ->
                      t_atom(false)

                    false ->
                      t_boolean()
                  end
              end

            false ->
              case t_is_integer(lhs, opaques) and
                     t_is_integer(
                       rhs,
                       opaques
                     ) do
                false ->
                  t_boolean()

                true ->
                  case {t_number_vals(lhs, opaques), t_number_vals(rhs, opaques)} do
                    {[x], [x]} when is_integer(x) ->
                      t_atom(true)

                    _ ->
                      lhsMax = number_max(lhs, opaques)
                      lhsMin = number_min(lhs, opaques)
                      rhsMax = number_max(rhs, opaques)
                      rhsMin = number_min(rhs, opaques)
                      ans1 = is_integer(lhsMin) and is_integer(rhsMax) and lhsMin > rhsMax
                      ans2 = is_integer(lhsMax) and is_integer(rhsMin) and rhsMin > lhsMax

                      case ans1 or ans2 do
                        true ->
                          t_atom(false)

                        false ->
                          t_boolean()
                      end
                  end
              end
          end
      end

    strict2(xs, ans)
  end

  def type(:erlang, :"=/=", 2, xs = [lhs, rhs], opaques) do
    ans =
      case t_is_none(t_inf(lhs, rhs, opaques)) do
        true ->
          t_atom(true)

        false ->
          case t_is_atom(lhs, opaques) and
                 t_is_atom(
                   rhs,
                   opaques
                 ) do
            true ->
              case {t_atom_vals(lhs, opaques), t_atom_vals(rhs, opaques)} do
                {:unknown, _} ->
                  t_boolean()

                {_, :unknown} ->
                  t_boolean()

                {[val], [val]} ->
                  t_atom(false)

                {lhsVals, rhsVals} ->
                  t_sup(
                    for x <- lhsVals, y <- rhsVals do
                      t_from_term(x !== y)
                    end
                  )
              end

            false ->
              case t_is_integer(lhs, opaques) and
                     t_is_integer(
                       rhs,
                       opaques
                     ) do
                false ->
                  t_boolean()

                true ->
                  lhsMax = number_max(lhs, opaques)
                  lhsMin = number_min(lhs, opaques)
                  rhsMax = number_max(rhs, opaques)
                  rhsMin = number_min(rhs, opaques)
                  ans1 = is_integer(lhsMin) and is_integer(rhsMax) and lhsMin > rhsMax
                  ans2 = is_integer(lhsMax) and is_integer(rhsMin) and rhsMin > lhsMax

                  case ans1 or ans2 do
                    true ->
                      t_atom(true)

                    false ->
                      cond do
                        lhsMax === lhsMin and rhsMin === rhsMax and
                            rhsMax === lhsMax ->
                          t_atom(false)

                        true ->
                          t_boolean()
                      end
                  end
              end
          end
      end

    strict2(xs, ans)
  end

  def type(:erlang, :>, 2, xs = [lhs, rhs], opaques) do
    ans =
      case t_is_integer(
             lhs,
             opaques
           ) and t_is_integer(rhs, opaques) do
        true ->
          lhsMax = number_max(lhs, opaques)
          lhsMin = number_min(lhs, opaques)
          rhsMax = number_max(rhs, opaques)
          rhsMin = number_min(rhs, opaques)
          t = t_atom(true)
          f = t_atom(false)

          cond do
            is_integer(lhsMin) and is_integer(rhsMax) and
                lhsMin > rhsMax ->
              t

            is_integer(lhsMax) and is_integer(rhsMin) and
                rhsMin >= lhsMax ->
              f

            true ->
              t_boolean()
          end

        false ->
          compare(:>, lhs, rhs, opaques)
      end

    strict2(xs, ans)
  end

  def type(:erlang, :>=, 2, xs = [lhs, rhs], opaques) do
    ans =
      case t_is_integer(
             lhs,
             opaques
           ) and t_is_integer(rhs, opaques) do
        true ->
          lhsMax = number_max(lhs, opaques)
          lhsMin = number_min(lhs, opaques)
          rhsMax = number_max(rhs, opaques)
          rhsMin = number_min(rhs, opaques)
          t = t_atom(true)
          f = t_atom(false)

          cond do
            is_integer(lhsMin) and is_integer(rhsMax) and
                lhsMin >= rhsMax ->
              t

            is_integer(lhsMax) and is_integer(rhsMin) and
                rhsMin > lhsMax ->
              f

            true ->
              t_boolean()
          end

        false ->
          compare(:>=, lhs, rhs, opaques)
      end

    strict2(xs, ans)
  end

  def type(:erlang, :<, 2, xs = [lhs, rhs], opaques) do
    ans =
      case t_is_integer(
             lhs,
             opaques
           ) and t_is_integer(rhs, opaques) do
        true ->
          lhsMax = number_max(lhs, opaques)
          lhsMin = number_min(lhs, opaques)
          rhsMax = number_max(rhs, opaques)
          rhsMin = number_min(rhs, opaques)
          t = t_atom(true)
          f = t_atom(false)

          cond do
            is_integer(lhsMax) and is_integer(rhsMin) and
                lhsMax < rhsMin ->
              t

            is_integer(lhsMin) and is_integer(rhsMax) and
                rhsMax <= lhsMin ->
              f

            true ->
              t_boolean()
          end

        false ->
          compare(:<, lhs, rhs, opaques)
      end

    strict2(xs, ans)
  end

  def type(:erlang, :"=<", 2, xs = [lhs, rhs], opaques) do
    ans =
      case t_is_integer(
             lhs,
             opaques
           ) and t_is_integer(rhs, opaques) do
        true ->
          lhsMax = number_max(lhs, opaques)
          lhsMin = number_min(lhs, opaques)
          rhsMax = number_max(rhs, opaques)
          rhsMin = number_min(rhs, opaques)
          t = t_atom(true)
          f = t_atom(false)

          cond do
            is_integer(lhsMax) and is_integer(rhsMin) and
                lhsMax <= rhsMin ->
              t

            is_integer(lhsMin) and is_integer(rhsMax) and
                rhsMax < lhsMin ->
              f

            true ->
              t_boolean()
          end

        false ->
          compare(:"=<", lhs, rhs, opaques)
      end

    strict2(xs, ans)
  end

  def type(:erlang, :+, 1, xs, opaques) do
    strict(
      :erlang,
      :+,
      1,
      xs,
      fn [x] ->
        x
      end,
      opaques
    )
  end

  def type(:erlang, :-, 1, xs, opaques) do
    strict(
      :erlang,
      :-,
      1,
      xs,
      fn [x] ->
        case t_is_integer(x, opaques) do
          true ->
            type(:erlang, :-, 2, [t_integer(0), x])

          false ->
            x
        end
      end,
      opaques
    )
  end

  def type(:erlang, :!, 2, xs, opaques) do
    strict(
      :erlang,
      :!,
      2,
      xs,
      fn [_, x2] ->
        x2
      end,
      opaques
    )
  end

  def type(:erlang, :+, 2, xs, opaques) do
    strict(
      :erlang,
      :+,
      2,
      xs,
      fn [x1, x2] ->
        case arith(:+, x1, x2, opaques) do
          {:ok, t} ->
            t

          :error ->
            case t_is_float(x1, opaques) or
                   t_is_float(
                     x2,
                     opaques
                   ) do
              true ->
                t_float()

              false ->
                t_number()
            end
        end
      end,
      opaques
    )
  end

  def type(:erlang, :-, 2, xs, opaques) do
    strict(
      :erlang,
      :-,
      2,
      xs,
      fn [x1, x2] ->
        case arith(:-, x1, x2, opaques) do
          {:ok, t} ->
            t

          :error ->
            case t_is_float(x1, opaques) or
                   t_is_float(
                     x2,
                     opaques
                   ) do
              true ->
                t_float()

              false ->
                t_number()
            end
        end
      end,
      opaques
    )
  end

  def type(:erlang, :*, 2, xs, opaques) do
    strict(
      :erlang,
      :*,
      2,
      xs,
      fn [x1, x2] ->
        case arith(:*, x1, x2, opaques) do
          {:ok, t} ->
            t

          :error ->
            case t_is_float(x1, opaques) or
                   t_is_float(
                     x2,
                     opaques
                   ) do
              true ->
                t_float()

              false ->
                t_number()
            end
        end
      end,
      opaques
    )
  end

  def type(:erlang, :/, 2, xs, opaques) do
    strict(
      :erlang,
      :/,
      2,
      xs,
      fn _ ->
        t_float()
      end,
      opaques
    )
  end

  def type(:erlang, :div, 2, xs, opaques) do
    strict(
      :erlang,
      :div,
      2,
      xs,
      fn [x1, x2] ->
        case arith(:div, x1, x2, opaques) do
          :error ->
            t_integer()

          {:ok, t} ->
            t
        end
      end,
      opaques
    )
  end

  def type(:erlang, :rem, 2, xs, opaques) do
    strict(
      :erlang,
      :rem,
      2,
      xs,
      fn [x1, x2] ->
        case arith(:rem, x1, x2, opaques) do
          :error ->
            t_non_neg_integer()

          {:ok, t} ->
            t
        end
      end,
      opaques
    )
  end

  def type(:erlang, :++, 2, xs, opaques) do
    strict(
      :erlang,
      :++,
      2,
      xs,
      fn [x1, x2] ->
        case t_is_nil(x1, opaques) do
          true ->
            x2

          false ->
            case t_is_nil(x2, opaques) do
              true ->
                x1

              false ->
                e1 = t_list_elements(x1, opaques)

                case t_is_cons(x1, opaques) do
                  true ->
                    t_cons(e1, x2)

                  false ->
                    t_sup(x2, t_cons(e1, x2))
                end
            end
        end
      end,
      opaques
    )
  end

  def type(:erlang, :--, 2, xs, opaques) do
    strict(
      :erlang,
      :--,
      2,
      xs,
      fn [x1, x2] ->
        case t_is_nil(x1, opaques) do
          true ->
            t_nil()

          false ->
            case t_is_nil(x2, opaques) do
              true ->
                x1

              false ->
                t_list(t_list_elements(x1, opaques))
            end
        end
      end,
      opaques
    )
  end

  def type(:erlang, :and, 2, xs, opaques) do
    strict(
      :erlang,
      :and,
      2,
      xs,
      fn _ ->
        t_boolean()
      end,
      opaques
    )
  end

  def type(:erlang, :or, 2, xs, opaques) do
    strict(
      :erlang,
      :or,
      2,
      xs,
      fn _ ->
        t_boolean()
      end,
      opaques
    )
  end

  def type(:erlang, :xor, 2, xs, opaques) do
    strict(
      :erlang,
      :xor,
      2,
      xs,
      fn _ ->
        t_boolean()
      end,
      opaques
    )
  end

  def type(:erlang, :not, 1, xs, opaques) do
    strict(
      :erlang,
      :not,
      1,
      xs,
      fn _ ->
        t_boolean()
      end,
      opaques
    )
  end

  def type(:erlang, :band, 2, xs, opaques) do
    strict(
      :erlang,
      :band,
      2,
      xs,
      fn [x1, x2] ->
        case arith(:band, x1, x2, opaques) do
          :error ->
            t_integer()

          {:ok, t} ->
            t
        end
      end,
      opaques
    )
  end

  def type(:erlang, :bor, 2, xs, opaques) do
    strict(
      :erlang,
      :bor,
      2,
      xs,
      fn [x1, x2] ->
        case arith(:bor, x1, x2, opaques) do
          :error ->
            t_integer()

          {:ok, t} ->
            t
        end
      end,
      opaques
    )
  end

  def type(:erlang, :bxor, 2, xs, opaques) do
    strict(
      :erlang,
      :bxor,
      2,
      xs,
      fn [x1, x2] ->
        case arith(:bxor, x1, x2, opaques) do
          :error ->
            t_integer()

          {:ok, t} ->
            t
        end
      end,
      opaques
    )
  end

  def type(:erlang, :bsr, 2, xs, opaques) do
    strict(
      :erlang,
      :bsr,
      2,
      xs,
      fn [x1, x2] ->
        case arith(:bsr, x1, x2, opaques) do
          :error ->
            t_integer()

          {:ok, t} ->
            t
        end
      end,
      opaques
    )
  end

  def type(:erlang, :bsl, 2, xs, opaques) do
    strict(
      :erlang,
      :bsl,
      2,
      xs,
      fn [x1, x2] ->
        case arith(:bsl, x1, x2, opaques) do
          :error ->
            t_integer()

          {:ok, t} ->
            t
        end
      end,
      opaques
    )
  end

  def type(:erlang, :bnot, 1, xs, opaques) do
    strict(
      :erlang,
      :bnot,
      1,
      xs,
      fn [x1] ->
        case arith_bnot(x1, opaques) do
          :error ->
            t_integer()

          {:ok, t} ->
            t
        end
      end,
      opaques
    )
  end

  def type(:erlang, :abs, 1, xs, opaques) do
    strict(
      :erlang,
      :abs,
      1,
      xs,
      fn [x1] ->
        arith_abs(x1, opaques)
      end,
      opaques
    )
  end

  def type(:erlang, :append, 2, xs, _Opaques) do
    type(:erlang, :++, 2, xs)
  end

  def type(:erlang, :apply, 2, xs, opaques) do
    fun = fn [x, _Y] ->
      case t_is_fun(x, opaques) do
        true ->
          t_fun_range(x, opaques)

        false ->
          t_any()
      end
    end

    strict(:erlang, :apply, 2, xs, fun, opaques)
  end

  def type(:erlang, :apply, 3, xs, opaques) do
    strict(
      :erlang,
      :apply,
      3,
      xs,
      fn _ ->
        t_any()
      end,
      opaques
    )
  end

  def type(:erlang, :binary_part, 2, xs, opaques) do
    strict(
      :erlang,
      :binary_part,
      2,
      xs,
      fn _ ->
        t_binary()
      end,
      opaques
    )
  end

  def type(:erlang, :binary_part, 3, xs, opaques) do
    strict(
      :erlang,
      :binary_part,
      3,
      xs,
      fn _ ->
        t_binary()
      end,
      opaques
    )
  end

  def type(:erlang, :bit_size, 1, xs, opaques) do
    strict(
      :erlang,
      :bit_size,
      1,
      xs,
      fn _ ->
        t_non_neg_integer()
      end,
      opaques
    )
  end

  def type(:erlang, :byte_size, 1, xs, opaques) do
    strict(
      :erlang,
      :byte_size,
      1,
      xs,
      fn _ ->
        t_non_neg_integer()
      end,
      opaques
    )
  end

  def type(:erlang, :ceil, 1, xs, opaques) do
    strict(
      :erlang,
      :ceil,
      1,
      xs,
      fn _ ->
        t_integer()
      end,
      opaques
    )
  end

  def type(:erlang, :element, 2, xs, opaques) do
    strict(
      :erlang,
      :element,
      2,
      xs,
      fn [x1, x2] ->
        case t_tuple_subtypes(x2, opaques) do
          :unknown ->
            t_any()

          [_] ->
            sz = t_tuple_size(x2, opaques)
            as = t_tuple_args(x2, opaques)

            case t_number_vals(x1, opaques) do
              :unknown ->
                t_sup(as)

              ns when is_list(ns) ->
                fun = fn
                  n, x
                  when is_integer(n) and 1 <= n and
                         n <= sz ->
                    t_sup(x, :lists.nth(n, as))

                  _, x ->
                    x
                end

                :lists.foldl(fun, t_none(), ns)
            end

          ts when is_list(ts) ->
            t_sup(
              for y <- ts do
                type(:erlang, :element, 2, [x1, y])
              end
            )
        end
      end,
      opaques
    )
  end

  def type(:erlang, :float, 1, xs, opaques) do
    strict(
      :erlang,
      :float,
      1,
      xs,
      fn _ ->
        t_float()
      end,
      opaques
    )
  end

  def type(:erlang, :floor, 1, xs, opaques) do
    strict(
      :erlang,
      :floor,
      1,
      xs,
      fn _ ->
        t_integer()
      end,
      opaques
    )
  end

  def type(:erlang, :build_stacktrace, 0, _, _Opaques) do
    t_list(
      t_tuple([
        t_module(),
        t_atom(),
        t_sup([t_arity(), t_list()]),
        t_list(
          t_sup([t_tuple([t_atom(:file), t_string()]), t_tuple([t_atom(:line), t_pos_integer()])])
        )
      ])
    )
  end

  def type(:erlang, :hd, 1, xs, opaques) do
    strict(
      :erlang,
      :hd,
      1,
      xs,
      fn [x] ->
        t_cons_hd(x)
      end,
      opaques
    )
  end

  def type(:erlang, :info, 1, xs, _) do
    type(:erlang, :system_info, 1, xs)
  end

  def type(:erlang, :is_atom, 1, xs, opaques) do
    fun = fn x ->
      check_guard(
        x,
        fn y ->
          t_is_atom(y, opaques)
        end,
        t_atom(),
        opaques
      )
    end

    strict(:erlang, :is_atom, 1, xs, fun, opaques)
  end

  def type(:erlang, :is_binary, 1, xs, opaques) do
    fun = fn x ->
      check_guard(
        x,
        fn y ->
          t_is_binary(y, opaques)
        end,
        t_binary(),
        opaques
      )
    end

    strict(:erlang, :is_binary, 1, xs, fun, opaques)
  end

  def type(:erlang, :is_bitstring, 1, xs, opaques) do
    fun = fn x ->
      check_guard(
        x,
        fn y ->
          t_is_bitstr(y, opaques)
        end,
        t_bitstr(),
        opaques
      )
    end

    strict(:erlang, :is_bitstring, 1, xs, fun, opaques)
  end

  def type(:erlang, :is_boolean, 1, xs, opaques) do
    fun = fn x ->
      check_guard(
        x,
        fn y ->
          t_is_boolean(y, opaques)
        end,
        t_boolean(),
        opaques
      )
    end

    strict(:erlang, :is_boolean, 1, xs, fun, opaques)
  end

  def type(:erlang, :is_float, 1, xs, opaques) do
    fun = fn x ->
      check_guard(
        x,
        fn y ->
          t_is_float(y, opaques)
        end,
        t_float(),
        opaques
      )
    end

    strict(:erlang, :is_float, 1, xs, fun, opaques)
  end

  def type(:erlang, :is_function, 1, xs, opaques) do
    fun = fn x ->
      check_guard(
        x,
        fn y ->
          t_is_fun(y, opaques)
        end,
        t_fun(),
        opaques
      )
    end

    strict(:erlang, :is_function, 1, xs, fun, opaques)
  end

  def type(:erlang, :is_function, 2, xs, opaques) do
    fun = fn [funType, arityType] ->
      case t_number_vals(arityType, opaques) do
        :unknown ->
          t_boolean()

        [val] ->
          funConstr = t_fun(any_list(val), t_any())

          fun2 = fn x ->
            t_is_subtype(x, funConstr) and not t_is_none(x)
          end

          check_guard_single(funType, fun2, funConstr, opaques)

        intList when is_list(intList) ->
          t_boolean()
      end
    end

    strict(:erlang, :is_function, 2, xs, fun, opaques)
  end

  def type(:erlang, :is_integer, 1, xs, opaques) do
    fun = fn x ->
      check_guard(
        x,
        fn y ->
          t_is_integer(y, opaques)
        end,
        t_integer(),
        opaques
      )
    end

    strict(:erlang, :is_integer, 1, xs, fun, opaques)
  end

  def type(:erlang, :is_list, 1, xs, opaques) do
    fun = fn x ->
      fun2 = fn y ->
        t_is_maybe_improper_list(y, opaques)
      end

      check_guard(x, fun2, t_maybe_improper_list(), opaques)
    end

    strict(:erlang, :is_list, 1, xs, fun, opaques)
  end

  def type(:erlang, :is_map, 1, xs, opaques) do
    fun = fn x ->
      check_guard(
        x,
        fn y ->
          t_is_map(y, opaques)
        end,
        t_map(),
        opaques
      )
    end

    strict(:erlang, :is_map, 1, xs, fun, opaques)
  end

  def type(:erlang, :is_map_key, 2, xs, opaques) do
    type(:maps, :is_key, 2, xs, opaques)
  end

  def type(:erlang, :is_number, 1, xs, opaques) do
    fun = fn x ->
      check_guard(
        x,
        fn y ->
          t_is_number(y, opaques)
        end,
        t_number(),
        opaques
      )
    end

    strict(:erlang, :is_number, 1, xs, fun, opaques)
  end

  def type(:erlang, :is_pid, 1, xs, opaques) do
    fun = fn x ->
      check_guard(
        x,
        fn y ->
          t_is_pid(y, opaques)
        end,
        t_pid(),
        opaques
      )
    end

    strict(:erlang, :is_pid, 1, xs, fun, opaques)
  end

  def type(:erlang, :is_port, 1, xs, opaques) do
    fun = fn x ->
      check_guard(
        x,
        fn y ->
          t_is_port(y, opaques)
        end,
        t_port(),
        opaques
      )
    end

    strict(:erlang, :is_port, 1, xs, fun, opaques)
  end

  def type(:erlang, :is_record, 2, xs, opaques) do
    fun = fn [x, y] ->
      case t_is_tuple(x, opaques) do
        false ->
          case t_is_none(t_inf(t_tuple(), x, opaques)) do
            true ->
              case t_has_opaque_subtype(x, opaques) do
                true ->
                  t_none()

                false ->
                  t_atom(false)
              end

            false ->
              t_boolean()
          end

        true ->
          case t_tuple_subtypes(x, opaques) do
            :unknown ->
              t_boolean()

            [tuple] ->
              case t_tuple_args(tuple, opaques) do
                [tag | _] ->
                  check_record_tag(tag, y, opaques)
              end

            list when length(list) >= 2 ->
              t_sup(
                for t <- list do
                  type(:erlang, :is_record, 2, [t, y])
                end
              )
          end
      end
    end

    strict(:erlang, :is_record, 2, xs, fun, opaques)
  end

  def type(:erlang, :is_record, 3, xs, opaques) do
    fun = fn [x, y, z] ->
      arity = t_number_vals(z, opaques)

      case t_is_tuple(x, opaques) do
        false when length(arity) === 1 ->
          [realArity] = arity

          case t_is_none(t_inf(t_tuple(realArity), x, opaques)) do
            true ->
              case t_has_opaque_subtype(x, opaques) do
                true ->
                  t_none()

                false ->
                  t_atom(false)
              end

            false ->
              t_boolean()
          end

        false ->
          case t_is_none(t_inf(t_tuple(), x, opaques)) do
            true ->
              case t_has_opaque_subtype(x, opaques) do
                true ->
                  t_none()

                false ->
                  t_atom(false)
              end

            false ->
              t_boolean()
          end

        true when length(arity) === 1 ->
          [realArity] = arity

          case t_tuple_subtypes(x, opaques) do
            :unknown ->
              t_boolean()

            [tuple] ->
              case t_tuple_args(tuple, opaques) do
                args when length(args) === realArity ->
                  check_record_tag(hd(args), y, opaques)

                args when length(args) !== realArity ->
                  t_atom(false)
              end

            [[_, _] | _] ->
              t_boolean()
          end

        true ->
          t_boolean()
      end
    end

    strict(:erlang, :is_record, 3, xs, fun, opaques)
  end

  def type(:erlang, :is_reference, 1, xs, opaques) do
    fun = fn x ->
      check_guard(
        x,
        fn y ->
          t_is_reference(y, opaques)
        end,
        t_reference(),
        opaques
      )
    end

    strict(:erlang, :is_reference, 1, xs, fun, opaques)
  end

  def type(:erlang, :is_tuple, 1, xs, opaques) do
    fun = fn x ->
      check_guard(
        x,
        fn y ->
          t_is_tuple(y, opaques)
        end,
        t_tuple(),
        opaques
      )
    end

    strict(:erlang, :is_tuple, 1, xs, fun, opaques)
  end

  def type(:erlang, :length, 1, xs, opaques) do
    strict(
      :erlang,
      :length,
      1,
      xs,
      fn _ ->
        t_non_neg_fixnum()
      end,
      opaques
    )
  end

  def type(:erlang, :map_size, 1, xs, opaques) do
    type(:maps, :size, 1, xs, opaques)
  end

  def type(:erlang, :map_get, 2, xs, opaques) do
    type(:maps, :get, 2, xs, opaques)
  end

  def type(:erlang, :make_fun, 3, xs, opaques) do
    strict(
      :erlang,
      :make_fun,
      3,
      xs,
      fn [_, _, arity] ->
        case t_number_vals(arity, opaques) do
          [n] ->
            case is_integer(n) and 0 <= n and n <= 255 do
              true ->
                t_fun(n, t_any())

              false ->
                t_none()
            end

          _Other ->
            t_fun()
        end
      end,
      opaques
    )
  end

  def type(:erlang, :make_tuple, 2, xs, opaques) do
    strict(
      :erlang,
      :make_tuple,
      2,
      xs,
      fn [int, _] ->
        case t_number_vals(int, opaques) do
          [n] when is_integer(n) and n >= 0 ->
            t_tuple(n)

          _Other ->
            t_tuple()
        end
      end,
      opaques
    )
  end

  def type(:erlang, :make_tuple, 3, xs, opaques) do
    strict(
      :erlang,
      :make_tuple,
      3,
      xs,
      fn [int, _, _] ->
        case t_number_vals(int, opaques) do
          [n] when is_integer(n) and n >= 0 ->
            t_tuple(n)

          _Other ->
            t_tuple()
        end
      end,
      opaques
    )
  end

  def type(:erlang, :nif_error, 1, xs, opaques) do
    strict(
      :erlang,
      :nif_error,
      1,
      xs,
      fn _ ->
        t_any()
      end,
      opaques
    )
  end

  def type(:erlang, :nif_error, 2, xs, opaques) do
    strict(
      :erlang,
      :nif_error,
      2,
      xs,
      fn _ ->
        t_any()
      end,
      opaques
    )
  end

  def type(:erlang, :node, 0, _, _Opaques) do
    t_node()
  end

  def type(:erlang, :node, 1, xs, opaques) do
    strict(
      :erlang,
      :node,
      1,
      xs,
      fn _ ->
        t_node()
      end,
      opaques
    )
  end

  def type(:erlang, :round, 1, xs, opaques) do
    strict(
      :erlang,
      :round,
      1,
      xs,
      fn _ ->
        t_integer()
      end,
      opaques
    )
  end

  def type(:erlang, :self, 0, _, _Opaques) do
    t_pid()
  end

  def type(:erlang, :setelement, 3, xs, opaques) do
    strict(
      :erlang,
      :setelement,
      3,
      xs,
      fn [x1, x2, x3] ->
        case t_tuple_subtypes(x2, opaques) do
          :unknown ->
            t_tuple()

          [_] ->
            sz = t_tuple_size(x2, opaques)
            as = t_tuple_args(x2, opaques)

            case t_number_vals(x1, opaques) do
              :unknown ->
                t_tuple(
                  for x <- as do
                    t_sup(x, x3)
                  end
                )

              [n] when is_integer(n) and 1 <= n and n <= sz ->
                t_tuple(list_replace(n, x3, as))

              [n] when is_integer(n) and n < 1 ->
                t_none()

              [n] when is_integer(n) and n > sz ->
                t_none()

              ns ->
                fun = fn
                  n, xL
                  when is_integer(n) and 1 <= n and
                         n <= sz ->
                    x = :lists.nth(n, xL)
                    y = t_sup(x, x3)
                    list_replace(n, y, xL)

                  _, xL ->
                    xL
                end

                t_tuple(:lists.foldl(fun, as, ns))
            end

          ts when is_list(ts) ->
            t_sup(
              for y <- ts do
                type(:erlang, :setelement, 3, [x1, y, x3])
              end
            )
        end
      end,
      opaques
    )
  end

  def type(:erlang, :size, 1, xs, opaques) do
    strict(
      :erlang,
      :size,
      1,
      xs,
      fn _ ->
        t_non_neg_integer()
      end,
      opaques
    )
  end

  def type(:erlang, :subtract, 2, xs, _Opaques) do
    type(:erlang, :--, 2, xs)
  end

  def type(:erlang, :system_info, 1, xs, opaques) do
    strict(
      :erlang,
      :system_info,
      1,
      xs,
      fn [type] ->
        case t_is_atom(type, opaques) do
          true ->
            case t_atom_vals(type, opaques) do
              [:allocated_areas] ->
                t_list(
                  t_sup([
                    t_tuple([t_atom(), t_non_neg_integer()]),
                    t_tuple([t_atom(), t_non_neg_integer(), t_non_neg_integer()])
                  ])
                )

              [:allocator] ->
                t_tuple([
                  t_sup([t_atom(:undefined), t_atom(:glibc)]),
                  t_list(t_integer()),
                  t_list(t_atom()),
                  t_list(t_tuple([t_atom(), t_list(t_tuple([t_atom(), t_any()]))]))
                ])

              [:break_ignored] ->
                t_boolean()

              [:cpu_topology] ->
                t_system_cpu_topology()

              [:compat_rel] ->
                t_non_neg_fixnum()

              [:creation] ->
                t_fixnum()

              [:debug_compiled] ->
                t_boolean()

              [:dist] ->
                t_binary()

              [:dist_ctrl] ->
                t_list(t_tuple([t_atom(), t_sup([t_pid(), t_port()])]))

              [:endian] ->
                t_endian()

              [:fullsweep_after] ->
                t_tuple([t_atom(:fullsweep_after), t_non_neg_integer()])

              [:garbage_collection] ->
                t_list()

              [:heap_sizes] ->
                t_list(t_integer())

              [:heap_type] ->
                t_atom(:private)

              [:hipe_architecture] ->
                t_atoms([:amd64, :arm, :powerpc, :ppc64, :undefined, :ultrasparc, :x86])

              [:info] ->
                t_binary()

              [:internal_cpu_topology] ->
                t_internal_cpu_topology()

              [:loaded] ->
                t_binary()

              [:logical_processors] ->
                t_non_neg_fixnum()

              [:machine] ->
                t_string()

              [:multi_scheduling] ->
                t_system_multi_scheduling()

              [:multi_scheduling_blockers] ->
                t_list(t_pid())

              [:os_type] ->
                t_tuple([t_sup([t_atom(:unix), t_atom(:win32)]), t_atom()])

              [:os_version] ->
                t_sup(
                  t_tuple([t_non_neg_fixnum(), t_non_neg_fixnum(), t_non_neg_fixnum()]),
                  t_string()
                )

              [:otp_release] ->
                t_string()

              [:port_parallelism] ->
                t_boolean()

              [:port_count] ->
                t_non_neg_fixnum()

              [:port_limit] ->
                t_non_neg_fixnum()

              [:process_count] ->
                t_non_neg_fixnum()

              [:process_limit] ->
                t_non_neg_fixnum()

              [:procs] ->
                t_binary()

              [:scheduler_bindings] ->
                t_tuple()

              [:scheduler_bind_type] ->
                t_scheduler_bind_type_results()

              [:schedulers] ->
                t_pos_fixnum()

              [:schedulers_online] ->
                t_pos_fixnum()

              [:sequential_tracer] ->
                t_tuple([t_atom(:sequential_tracer), t_sequential_tracer()])

              [:smp_support] ->
                t_boolean()

              [:system_architecture] ->
                t_string()

              [:system_version] ->
                t_string()

              [:threads] ->
                t_boolean()

              [:thread_pool_size] ->
                t_non_neg_fixnum()

              [:trace_control_word] ->
                t_integer()

              [:version] ->
                t_string()

              [:wordsize] ->
                t_integers([4, 8])

              list when is_list(list) ->
                t_any()

              :unknown ->
                t_any()
            end

          false ->
            t_any()
        end
      end,
      opaques
    )
  end

  def type(:erlang, :tl, 1, xs, opaques) do
    strict(
      :erlang,
      :tl,
      1,
      xs,
      fn [x] ->
        t_cons_tl(x)
      end,
      opaques
    )
  end

  def type(:erlang, :trunc, 1, xs, opaques) do
    strict(
      :erlang,
      :trunc,
      1,
      xs,
      fn _ ->
        t_integer()
      end,
      opaques
    )
  end

  def type(:erlang, :tuple_size, 1, xs, opaques) do
    strict(
      :erlang,
      :tuple_size,
      1,
      xs,
      fn _ ->
        t_non_neg_integer()
      end,
      opaques
    )
  end

  def type(:erlang, :tuple_to_list, 1, xs, opaques) do
    strict(
      :erlang,
      :tuple_to_list,
      1,
      xs,
      fn [x] ->
        case t_tuple_subtypes(x, opaques) do
          :unknown ->
            t_list()

          subTypes ->
            args =
              :lists.append(
                for sT <- subTypes do
                  t_tuple_args(sT, opaques)
                end
              )

            case :lists.any(
                   fn t ->
                     t_tuple_size(t, opaques) === 0
                   end,
                   subTypes
                 ) do
              true ->
                t_sup(t_nonempty_list(t_sup(args)), t_nil())

              false ->
                t_nonempty_list(t_sup(args))
            end
        end
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :add_ref, 2, xs, opaques) do
    strict(
      :hipe_bifs,
      :add_ref,
      2,
      xs,
      fn _ ->
        t_atom(:ok)
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :alloc_data, 3, xs, opaques) do
    strict(
      :hipe_bifs,
      :alloc_data,
      3,
      xs,
      fn _ ->
        t_integer()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :array, 2, xs, opaques) do
    strict(
      :hipe_bifs,
      :array,
      2,
      xs,
      fn _ ->
        t_immarray()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :array_length, 1, xs, opaques) do
    strict(
      :hipe_bifs,
      :array_length,
      1,
      xs,
      fn _ ->
        t_non_neg_fixnum()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :array_sub, 2, xs, opaques) do
    strict(
      :hipe_bifs,
      :array_sub,
      2,
      xs,
      fn _ ->
        t_immediate()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :array_update, 3, xs, opaques) do
    strict(
      :hipe_bifs,
      :array_update,
      3,
      xs,
      fn _ ->
        t_immarray()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :atom_to_word, 1, xs, opaques) do
    strict(
      :hipe_bifs,
      :atom_to_word,
      1,
      xs,
      fn _ ->
        t_integer()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :bif_address, 3, xs, opaques) do
    strict(
      :hipe_bifs,
      :bif_address,
      3,
      xs,
      fn _ ->
        t_sup(t_integer(), t_atom(false))
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :bitarray, 2, xs, opaques) do
    strict(
      :hipe_bifs,
      :bitarray,
      2,
      xs,
      fn _ ->
        t_bitarray()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :bitarray_sub, 2, xs, opaques) do
    strict(
      :hipe_bifs,
      :bitarray_sub,
      2,
      xs,
      fn _ ->
        t_boolean()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :bitarray_update, 3, xs, opaques) do
    strict(
      :hipe_bifs,
      :bitarray_update,
      3,
      xs,
      fn _ ->
        t_bitarray()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :bytearray, 2, xs, opaques) do
    strict(
      :hipe_bifs,
      :bytearray,
      2,
      xs,
      fn _ ->
        t_bytearray()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :bytearray_sub, 2, xs, opaques) do
    strict(
      :hipe_bifs,
      :bytearray_sub,
      2,
      xs,
      fn _ ->
        t_byte()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :bytearray_update, 3, xs, opaques) do
    strict(
      :hipe_bifs,
      :bytearray_update,
      3,
      xs,
      fn _ ->
        t_bytearray()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :call_count_clear, 1, xs, opaques) do
    strict(
      :hipe_bifs,
      :call_count_clear,
      1,
      xs,
      fn _ ->
        t_sup(t_non_neg_integer(), t_atom(false))
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :call_count_get, 1, xs, opaques) do
    strict(
      :hipe_bifs,
      :call_count_get,
      1,
      xs,
      fn _ ->
        t_sup(t_non_neg_integer(), t_atom(false))
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :call_count_off, 1, xs, opaques) do
    strict(
      :hipe_bifs,
      :call_count_off,
      1,
      xs,
      fn _ ->
        t_sup(t_non_neg_integer(), t_atom(false))
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :call_count_on, 1, xs, opaques) do
    strict(
      :hipe_bifs,
      :call_count_on,
      1,
      xs,
      fn _ ->
        t_sup(t_atom(true), t_nil())
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :check_crc, 1, xs, opaques) do
    strict(
      :hipe_bifs,
      :check_crc,
      1,
      xs,
      fn _ ->
        t_boolean()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :enter_code, 3, xs, opaques) do
    strict(
      :hipe_bifs,
      :enter_code,
      3,
      xs,
      fn _ ->
        t_tuple([t_integer(), t_sup(t_nil(), t_binary())])
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :enter_sdesc, 2, xs, opaques) do
    strict(
      :hipe_bifs,
      :enter_sdesc,
      2,
      xs,
      fn _ ->
        t_nil()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :find_na_or_make_stub, 1, xs, opaques) do
    strict(
      :hipe_bifs,
      :find_na_or_make_stub,
      1,
      xs,
      fn _ ->
        t_integer()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :fun_to_address, 1, xs, opaques) do
    strict(
      :hipe_bifs,
      :fun_to_address,
      1,
      xs,
      fn _ ->
        t_integer()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :get_fe, 2, xs, opaques) do
    strict(
      :hipe_bifs,
      :get_fe,
      2,
      xs,
      fn _ ->
        t_integer()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :get_rts_param, 1, xs, opaques) do
    strict(
      :hipe_bifs,
      :get_rts_param,
      1,
      xs,
      fn _ ->
        t_sup(t_integer(), t_nil())
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :merge_term, 1, xs, opaques) do
    strict(
      :hipe_bifs,
      :merge_term,
      1,
      xs,
      fn [x] ->
        x
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :nstack_used_size, 0, _, _Opaques) do
    t_non_neg_fixnum()
  end

  def type(:hipe_bifs, :patch_call, 3, xs, opaques) do
    strict(
      :hipe_bifs,
      :patch_call,
      3,
      xs,
      fn _ ->
        t_nil()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :patch_insn, 3, xs, opaques) do
    strict(
      :hipe_bifs,
      :patch_insn,
      3,
      xs,
      fn _ ->
        t_nil()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :primop_address, 1, xs, opaques) do
    strict(
      :hipe_bifs,
      :primop_address,
      1,
      xs,
      fn _ ->
        t_sup(t_integer(), t_atom(false))
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :ref, 1, xs, opaques) do
    strict(
      :hipe_bifs,
      :ref,
      1,
      xs,
      fn _ ->
        t_immarray()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :ref_get, 1, xs, opaques) do
    strict(
      :hipe_bifs,
      :ref_get,
      1,
      xs,
      fn _ ->
        t_immediate()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :ref_set, 2, xs, opaques) do
    strict(
      :hipe_bifs,
      :ref_set,
      2,
      xs,
      fn _ ->
        t_nil()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :set_funinfo_native_address, 3, xs, opaques) do
    strict(
      :hipe_bifs,
      :set_funinfo_native_address,
      3,
      xs,
      fn _ ->
        t_nil()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :commit_patch_load, 1, xs, opaques) do
    strict(
      :hipe_bifs,
      :commit_patch_load,
      1,
      xs,
      fn _ ->
        t_atom()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :set_native_address, 3, xs, opaques) do
    strict(
      :hipe_bifs,
      :set_native_address,
      3,
      xs,
      fn _ ->
        t_nil()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :set_native_address_in_fe, 2, xs, opaques) do
    strict(
      :hipe_bifs,
      :set_native_address_in_fe,
      2,
      xs,
      fn _ ->
        t_atom(true)
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :system_crc, 0, _, _Opaques) do
    t_crc32()
  end

  def type(:hipe_bifs, :term_to_word, 1, xs, opaques) do
    strict(
      :hipe_bifs,
      :term_to_word,
      1,
      xs,
      fn _ ->
        t_integer()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :write_u8, 2, xs, opaques) do
    strict(
      :hipe_bifs,
      :write_u8,
      2,
      xs,
      fn _ ->
        t_nil()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :write_u32, 2, xs, opaques) do
    strict(
      :hipe_bifs,
      :write_u32,
      2,
      xs,
      fn _ ->
        t_nil()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :write_u64, 2, xs, opaques) do
    strict(
      :hipe_bifs,
      :write_u64,
      2,
      xs,
      fn _ ->
        t_nil()
      end,
      opaques
    )
  end

  def type(:hipe_bifs, :alloc_loader_state, 1, xs, opaques) do
    strict(
      :hipe_bifs,
      :alloc_loader_state,
      1,
      xs,
      fn _ ->
        t_binary()
      end,
      opaques
    )
  end

  def type(:lists, :all, 2, xs, opaques) do
    strict(
      :lists,
      :all,
      2,
      xs,
      fn [f, l] ->
        case t_is_nil(l, opaques) do
          true ->
            t_atom(true)

          false ->
            el = t_list_elements(l, opaques)

            case check_fun_application(f, [el], opaques) do
              :ok ->
                case t_is_cons(l, opaques) do
                  true ->
                    t_fun_range(f, opaques)

                  false ->
                    t_sup(t_atom(true), t_fun_range(f, opaques))
                end

              :error ->
                case t_is_cons(l, opaques) do
                  true ->
                    t_none()

                  false ->
                    t_fun_range(f, opaques)
                end
            end
        end
      end,
      opaques
    )
  end

  def type(:lists, :any, 2, xs, opaques) do
    strict(
      :lists,
      :any,
      2,
      xs,
      fn [f, l] ->
        case t_is_nil(l, opaques) do
          true ->
            t_atom(false)

          false ->
            el = t_list_elements(l, opaques)

            case check_fun_application(f, [el], opaques) do
              :ok ->
                case t_is_cons(l, opaques) do
                  true ->
                    t_fun_range(f, opaques)

                  false ->
                    t_sup(t_atom(false), t_fun_range(f, opaques))
                end

              :error ->
                case t_is_cons(l, opaques) do
                  true ->
                    t_none()

                  false ->
                    t_fun_range(f, opaques)
                end
            end
        end
      end,
      opaques
    )
  end

  def type(:lists, :append, 2, xs, _Opaques) do
    type(:erlang, :++, 2, xs)
  end

  def type(:lists, :delete, 2, xs, opaques) do
    strict(
      :lists,
      :delete,
      2,
      xs,
      fn [_, list] ->
        case t_is_cons(list, opaques) do
          true ->
            t_cons_tl(list)

          false ->
            list
        end
      end,
      opaques
    )
  end

  def type(:lists, :dropwhile, 2, xs, opaques) do
    strict(
      :lists,
      :dropwhile,
      2,
      xs,
      fn [f, x] ->
        case t_is_nil(x, opaques) do
          true ->
            t_nil()

          false ->
            x1 = t_list_elements(x, opaques)

            case check_fun_application(f, [x1], opaques) do
              :ok ->
                case t_atom_vals(
                       t_fun_range(f, opaques),
                       opaques
                     ) do
                  [true] ->
                    case t_is_none(t_inf(t_list(), x, opaques)) do
                      true ->
                        t_none()

                      false ->
                        t_nil()
                    end

                  [false] ->
                    case t_is_none(t_inf(t_list(), x, opaques)) do
                      true ->
                        t_none()

                      false ->
                        x
                    end

                  _ ->
                    t_inf(
                      t_cons_tl(t_inf(x, t_cons(), opaques)),
                      t_maybe_improper_list(),
                      opaques
                    )
                end

              :error ->
                case t_is_cons(x, opaques) do
                  true ->
                    t_none()

                  false ->
                    t_nil()
                end
            end
        end
      end,
      opaques
    )
  end

  def type(:lists, :filter, 2, xs, opaques) do
    strict(
      :lists,
      :filter,
      2,
      xs,
      fn [f, l] ->
        case t_is_nil(l, opaques) do
          true ->
            t_nil()

          false ->
            t = t_list_elements(l, opaques)

            case check_fun_application(f, [t], opaques) do
              :ok ->
                rangeVals =
                  t_atom_vals(
                    t_fun_range(f, opaques),
                    opaques
                  )

                case rangeVals === [false] do
                  true ->
                    t_nil()

                  false ->
                    case rangeVals === [true] do
                      true ->
                        l

                      false ->
                        t_list(t)
                    end
                end

              :error ->
                case t_is_cons(l, opaques) do
                  true ->
                    t_none()

                  false ->
                    t_nil()
                end
            end
        end
      end,
      opaques
    )
  end

  def type(:lists, :flatten, 1, xs, opaques) do
    strict(
      :lists,
      :flatten,
      1,
      xs,
      fn [l] ->
        case t_is_nil(l, opaques) do
          true ->
            l

          false ->
            x1 = t_list_elements(l, opaques)

            case t_is_any(x1) do
              true ->
                t_list()

              false ->
                x2 = type(:lists, :flatten, 1, [t_inf(x1, t_list(), opaques)])
                t_sup(t_list(t_subtract(x1, t_list())), x2)
            end
        end
      end,
      opaques
    )
  end

  def type(:lists, :flatmap, 2, xs, opaques) do
    strict(
      :lists,
      :flatmap,
      2,
      xs,
      fn [f, list] ->
        case t_is_nil(list, opaques) do
          true ->
            t_nil()

          false ->
            case check_fun_application(
                   f,
                   [
                     t_list_elements(
                       list,
                       opaques
                     )
                   ],
                   opaques
                 ) do
              :ok ->
                r = t_fun_range(f, opaques)

                case t_is_nil(r) do
                  true ->
                    t_nil()

                  false ->
                    elems = t_list_elements(r, opaques)

                    case t_is_cons(list, opaques) do
                      true ->
                        case t_is_subtype(t_nil(), r) do
                          true ->
                            t_list(elems)

                          false ->
                            t_nonempty_list(elems)
                        end

                      false ->
                        t_list(elems)
                    end
                end

              :error ->
                case t_is_cons(list, opaques) do
                  true ->
                    t_none()

                  false ->
                    t_nil()
                end
            end
        end
      end,
      opaques
    )
  end

  def type(:lists, :foreach, 2, xs, opaques) do
    strict(
      :lists,
      :foreach,
      2,
      xs,
      fn [f, list] ->
        case t_is_cons(list, opaques) do
          true ->
            case check_fun_application(
                   f,
                   [
                     t_list_elements(
                       list,
                       opaques
                     )
                   ],
                   opaques
                 ) do
              :ok ->
                t_atom(:ok)

              :error ->
                t_none()
            end

          false ->
            t_atom(:ok)
        end
      end,
      opaques
    )
  end

  def type(:lists, :foldl, 3, xs, opaques) do
    strict(
      :lists,
      :foldl,
      3,
      xs,
      fn [f, acc, list] ->
        case t_is_nil(list, opaques) do
          true ->
            acc

          false ->
            case check_fun_application(
                   f,
                   [
                     t_list_elements(
                       list,
                       opaques
                     ),
                     acc
                   ],
                   opaques
                 ) do
              :ok ->
                case t_is_cons(list, opaques) do
                  true ->
                    t_fun_range(f, opaques)

                  false ->
                    t_sup(t_fun_range(f, opaques), acc)
                end

              :error ->
                case t_is_cons(list, opaques) do
                  true ->
                    t_none()

                  false ->
                    acc
                end
            end
        end
      end,
      opaques
    )
  end

  def type(:lists, :foldr, 3, xs, _Opaques) do
    type(:lists, :foldl, 3, xs)
  end

  def type(:lists, :keydelete, 3, xs, opaques) do
    strict(
      :lists,
      :keydelete,
      3,
      xs,
      fn [_, _, l] ->
        term = t_list_termination(l, opaques)

        t_sup(
          term,
          :erl_types.lift_list_to_pos_empty(l, opaques)
        )
      end,
      opaques
    )
  end

  def type(:lists, :keyfind, 3, xs, opaques) do
    strict(
      :lists,
      :keyfind,
      3,
      xs,
      fn [x, y, z] ->
        listEs = t_list_elements(z, opaques)
        tuple = t_inf(t_tuple(), listEs, opaques)

        case t_is_none(tuple) do
          true ->
            t_atom(false)

          false ->
            ret = t_sup(tuple, t_atom(false))

            case t_is_any(x) do
              true ->
                ret

              false ->
                case t_tuple_subtypes(tuple, opaques) do
                  :unknown ->
                    ret

                  list ->
                    case key_comparisons_fail(x, y, list, opaques) do
                      true ->
                        t_atom(false)

                      false ->
                        ret
                    end
                end
            end
        end
      end,
      opaques
    )
  end

  def type(:lists, :keymap, 3, xs, opaques) do
    strict(
      :lists,
      :keymap,
      3,
      xs,
      fn [f, _I, l] ->
        case t_is_nil(l, opaques) do
          true ->
            l

          false ->
            t_list(
              t_sup(
                t_fun_range(f, opaques),
                t_list_elements(l, opaques)
              )
            )
        end
      end,
      opaques
    )
  end

  def type(:lists, :keymember, 3, xs, opaques) do
    strict(
      :lists,
      :keymember,
      3,
      xs,
      fn [x, y, z] ->
        listEs = t_list_elements(z, opaques)
        tuple = t_inf(t_tuple(), listEs, opaques)

        case t_is_none(tuple) do
          true ->
            t_atom(false)

          false ->
            case t_is_any(x) do
              true ->
                t_boolean()

              false ->
                case t_tuple_subtypes(tuple, opaques) do
                  :unknown ->
                    t_boolean()

                  list ->
                    case key_comparisons_fail(x, y, list, opaques) do
                      true ->
                        t_atom(false)

                      false ->
                        t_boolean()
                    end
                end
            end
        end
      end,
      opaques
    )
  end

  def type(:lists, :keymerge, 3, xs, opaques) do
    strict(
      :lists,
      :keymerge,
      3,
      xs,
      fn [_I, l1, l2] ->
        type(:lists, :merge, 2, [l1, l2])
      end,
      opaques
    )
  end

  def type(:lists, :keyreplace, 4, xs, opaques) do
    strict(
      :lists,
      :keyreplace,
      4,
      xs,
      fn [_K, _I, l, t] ->
        t_list(t_sup(t_list_elements(l, opaques), t))
      end,
      opaques
    )
  end

  def type(:lists, :keysearch, 3, xs, opaques) do
    strict(
      :lists,
      :keysearch,
      3,
      xs,
      fn [x, y, z] ->
        listEs = t_list_elements(z, opaques)
        tuple = t_inf(t_tuple(), listEs, opaques)

        case t_is_none(tuple) do
          true ->
            t_atom(false)

          false ->
            ret =
              t_sup(
                t_tuple([t_atom(:value), tuple]),
                t_atom(false)
              )

            case t_is_any(x) do
              true ->
                ret

              false ->
                case t_tuple_subtypes(tuple, opaques) do
                  :unknown ->
                    ret

                  list ->
                    case key_comparisons_fail(x, y, list, opaques) do
                      true ->
                        t_atom(false)

                      false ->
                        ret
                    end
                end
            end
        end
      end,
      opaques
    )
  end

  def type(:lists, :keysort, 2, xs, opaques) do
    strict(
      :lists,
      :keysort,
      2,
      xs,
      fn [_, l] ->
        l
      end,
      opaques
    )
  end

  def type(:lists, :last, 1, xs, opaques) do
    strict(
      :lists,
      :last,
      1,
      xs,
      fn [l] ->
        t_list_elements(l, opaques)
      end,
      opaques
    )
  end

  def type(:lists, :map, 2, xs, opaques) do
    strict(
      :lists,
      :map,
      2,
      xs,
      fn [f, l] ->
        case t_is_nil(l, opaques) do
          true ->
            l

          false ->
            el = t_list_elements(l, opaques)

            case t_is_cons(l, opaques) do
              true ->
                case check_fun_application(f, [el], opaques) do
                  :ok ->
                    t_nonempty_list(t_fun_range(f, opaques))

                  :error ->
                    t_none()
                end

              false ->
                case check_fun_application(f, [el], opaques) do
                  :ok ->
                    t_list(t_fun_range(f, opaques))

                  :error ->
                    t_nil()
                end
            end
        end
      end,
      opaques
    )
  end

  def type(:lists, :mapfoldl, 3, xs, opaques) do
    strict(
      :lists,
      :mapfoldl,
      3,
      xs,
      fn [f, acc, list] ->
        case t_is_nil(list, opaques) do
          true ->
            t_tuple([list, acc])

          false ->
            el = t_list_elements(list, opaques)
            r = t_fun_range(f, opaques)

            case t_is_cons(list, opaques) do
              true ->
                case check_fun_application(f, [el, acc], opaques) do
                  :ok ->
                    fun = fn rangeTuple ->
                      [t1, t2] =
                        t_tuple_args(
                          rangeTuple,
                          opaques
                        )

                      t_tuple([t_nonempty_list(t1), t2])
                    end

                    t_sup(
                      for sT <- t_tuple_subtypes(r, opaques) do
                        fun.(sT)
                      end
                    )

                  :error ->
                    t_none()
                end

              false ->
                case check_fun_application(f, [el, acc], opaques) do
                  :ok ->
                    fun = fn rangeTuple ->
                      [t1, t2] =
                        t_tuple_args(
                          rangeTuple,
                          opaques
                        )

                      t_tuple([t_list(t1), t_sup(acc, t2)])
                    end

                    t_sup(
                      for sT <- t_tuple_subtypes(r, opaques) do
                        fun.(sT)
                      end
                    )

                  :error ->
                    t_tuple([t_nil(), acc])
                end
            end
        end
      end,
      opaques
    )
  end

  def type(:lists, :mapfoldr, 3, xs, _Opaques) do
    type(:lists, :mapfoldl, 3, xs)
  end

  def type(:lists, :max, 1, xs, opaques) do
    strict(
      :lists,
      :max,
      1,
      xs,
      fn [l] ->
        t_list_elements(l, opaques)
      end,
      opaques
    )
  end

  def type(:lists, :member, 2, xs, opaques) do
    strict(
      :lists,
      :member,
      2,
      xs,
      fn [x, y] ->
        y1 = t_list_elements(y, opaques)

        case t_is_none(t_inf(y1, x, opaques)) do
          true ->
            t_atom(false)

          false ->
            t_boolean()
        end
      end,
      opaques
    )
  end

  def type(:lists, :merge, 2, xs, opaques) do
    strict(
      :lists,
      :merge,
      2,
      xs,
      fn [l1, l2] ->
        case t_is_none(l1) do
          true ->
            l2

          false ->
            case t_is_none(l2) do
              true ->
                l1

              false ->
                t_sup(l1, l2)
            end
        end
      end,
      opaques
    )
  end

  def type(:lists, :min, 1, xs, opaques) do
    strict(
      :lists,
      :min,
      1,
      xs,
      fn [l] ->
        t_list_elements(l, opaques)
      end,
      opaques
    )
  end

  def type(:lists, :nth, 2, xs, opaques) do
    strict(
      :lists,
      :nth,
      2,
      xs,
      fn [_, y] ->
        t_list_elements(y, opaques)
      end,
      opaques
    )
  end

  def type(:lists, :nthtail, 2, xs, opaques) do
    strict(
      :lists,
      :nthtail,
      2,
      xs,
      fn [_, y] ->
        t_sup(y, t_list())
      end,
      opaques
    )
  end

  def type(:lists, :partition, 2, xs, opaques) do
    strict(
      :lists,
      :partition,
      2,
      xs,
      fn [f, l] ->
        case t_is_nil(l, opaques) do
          true ->
            t_tuple([l, l])

          false ->
            el = t_list_elements(l, opaques)

            case check_fun_application(f, [el], opaques) do
              :error ->
                case t_is_cons(l, opaques) do
                  true ->
                    t_none()

                  false ->
                    t_tuple([t_nil(), t_nil()])
                end

              :ok ->
                case t_atom_vals(
                       t_fun_range(f, opaques),
                       opaques
                     ) do
                  [true] ->
                    t_tuple([l, t_nil()])

                  [false] ->
                    t_tuple([t_nil(), l])

                  [_, _] ->
                    l2 = t_list(el)
                    t_tuple([l2, l2])
                end
            end
        end
      end,
      opaques
    )
  end

  def type(:lists, :reverse, 1, xs, opaques) do
    strict(
      :lists,
      :reverse,
      1,
      xs,
      fn [x] ->
        x
      end,
      opaques
    )
  end

  def type(:lists, :reverse, 2, xs, _Opaques) do
    type(:erlang, :++, 2, xs)
  end

  def type(:lists, :sort, 1, xs, opaques) do
    strict(
      :lists,
      :sort,
      1,
      xs,
      fn [x] ->
        x
      end,
      opaques
    )
  end

  def type(:lists, :sort, 2, xs, opaques) do
    strict(
      :lists,
      :sort,
      2,
      xs,
      fn [f, l] ->
        r = t_fun_range(f, opaques)

        case t_is_boolean(r, opaques) do
          true ->
            l

          false ->
            case t_is_nil(l, opaques) do
              true ->
                t_nil()

              false ->
                t_none()
            end
        end
      end,
      opaques
    )
  end

  def type(:lists, :split, 2, xs, opaques) do
    strict(
      :lists,
      :split,
      2,
      xs,
      fn [_, l] ->
        case t_is_nil(l, opaques) do
          true ->
            t_tuple([l, l])

          false ->
            t = t_list_elements(l, opaques)
            t_tuple([t_list(t), t_list(t)])
        end
      end,
      opaques
    )
  end

  def type(:lists, :splitwith, 2, xs, _Opaques) do
    t1 = type(:lists, :takewhile, 2, xs)
    t2 = type(:lists, :dropwhile, 2, xs)

    case t_is_none(t1) or t_is_none(t2) do
      true ->
        t_none()

      false ->
        t_tuple([t1, t2])
    end
  end

  def type(:lists, :subtract, 2, xs, _Opaques) do
    type(:erlang, :--, 2, xs)
  end

  def type(:lists, :takewhile, 2, xs, opaques) do
    strict(
      :lists,
      :takewhile,
      2,
      xs,
      fn [f, l] ->
        case t_is_none(t_inf(t_list(), l, opaques)) do
          false ->
            type(:lists, :filter, 2, xs)

          true ->
            el = t_list_elements(l, opaques)
            type(:lists, :filter, 2, [f, t_list(el)])
        end
      end,
      opaques
    )
  end

  def type(:lists, :usort, 1, xs, _Opaques) do
    type(:lists, :sort, 1, xs)
  end

  def type(:lists, :usort, 2, xs, _Opaques) do
    type(:lists, :sort, 2, xs)
  end

  def type(:lists, :unzip, 1, xs, opaques) do
    strict(
      :lists,
      :unzip,
      1,
      xs,
      fn [ps] ->
        case t_is_nil(ps, opaques) do
          true ->
            t_tuple([t_nil(), t_nil()])

          false ->
            tupleTypes =
              t_tuple_subtypes(
                t_list_elements(
                  ps,
                  opaques
                ),
                opaques
              )

            :lists.foldl(
              fn tuple, acc ->
                [a, b] = t_tuple_args(tuple, opaques)

                t_sup(
                  t_tuple([t_list(a), t_list(b)]),
                  acc
                )
              end,
              t_none(),
              tupleTypes
            )
        end
      end,
      opaques
    )
  end

  def type(:lists, :unzip3, 1, xs, opaques) do
    strict(
      :lists,
      :unzip3,
      1,
      xs,
      fn [ts] ->
        case t_is_nil(ts, opaques) do
          true ->
            t_tuple([t_nil(), t_nil(), t_nil()])

          false ->
            tupleTypes =
              t_tuple_subtypes(
                t_list_elements(
                  ts,
                  opaques
                ),
                opaques
              )

            :lists.foldl(
              fn t, acc ->
                [a, b, c] = t_tuple_args(t, opaques)

                t_sup(
                  t_tuple([t_list(a), t_list(b), t_list(c)]),
                  acc
                )
              end,
              t_none(),
              tupleTypes
            )
        end
      end,
      opaques
    )
  end

  def type(:lists, :zip, 2, xs, opaques) do
    strict(
      :lists,
      :zip,
      2,
      xs,
      fn [as, bs] ->
        case t_is_nil(as, opaques) or t_is_nil(bs, opaques) do
          true ->
            t_nil()

          false ->
            a = t_list_elements(as, opaques)
            b = t_list_elements(bs, opaques)
            t_list(t_tuple([a, b]))
        end
      end,
      opaques
    )
  end

  def type(:lists, :zip3, 3, xs, opaques) do
    strict(
      :lists,
      :zip3,
      3,
      xs,
      fn [as, bs, cs] ->
        case t_is_nil(as, opaques) or
               t_is_nil(
                 bs,
                 opaques
               ) or
               t_is_nil(
                 cs,
                 opaques
               ) do
          true ->
            t_nil()

          false ->
            a = t_list_elements(as, opaques)
            b = t_list_elements(bs, opaques)
            c = t_list_elements(cs, opaques)
            t_list(t_tuple([a, b, c]))
        end
      end,
      opaques
    )
  end

  def type(:lists, :zipwith, 3, xs, opaques) do
    strict(
      :lists,
      :zipwith,
      3,
      xs,
      fn [f, _As, _Bs] ->
        t_sup(t_list(t_fun_range(f, opaques)), t_nil())
      end,
      opaques
    )
  end

  def type(:lists, :zipwith3, 4, xs, opaques) do
    strict(
      :lists,
      :zipwith3,
      4,
      xs,
      fn [f, _As, _Bs, _Cs] ->
        t_sup(t_list(t_fun_range(f, opaques)), t_nil())
      end,
      opaques
    )
  end

  def type(:maps, :from_list, 1, xs, opaques) do
    strict(
      :maps,
      :from_list,
      1,
      xs,
      fn [list] ->
        case t_is_nil(list, opaques) do
          true ->
            t_from_term(%{})

          false ->
            t = t_list_elements(list, opaques)

            case t_tuple_subtypes(t, opaques) do
              :unknown ->
                t_map()

              stypes when length(stypes) >= 1 ->
                t_sup(
                  for args <- stypes do
                    [k, v] = t_tuple_args(args, opaques)
                    t_map([], k, v)
                  end
                )
            end
        end
      end,
      opaques
    )
  end

  def type(:maps, :get, 2, xs, opaques) do
    strict(
      :maps,
      :get,
      2,
      xs,
      fn [key, map] ->
        t_map_get(key, map, opaques)
      end,
      opaques
    )
  end

  def type(:maps, :is_key, 2, xs, opaques) do
    strict(
      :maps,
      :is_key,
      2,
      xs,
      fn [key, map] ->
        t_map_is_key(key, map, opaques)
      end,
      opaques
    )
  end

  def type(:maps, :merge, 2, xs, opaques) do
    strict(
      :maps,
      :merge,
      2,
      xs,
      fn [mapA, mapB] ->
        aDefK = t_map_def_key(mapA, opaques)
        bDefK = t_map_def_key(mapB, opaques)
        aDefV = t_map_def_val(mapA, opaques)
        bDefV = t_map_def_val(mapB, opaques)

        t_map(
          t_map_pairwise_merge(
            fn
              k, _, _, :mandatory, v ->
                {k, :mandatory, v}

              k, mNess, vA, :optional, vB ->
                {k, mNess, t_sup(vA, vB)}
            end,
            mapA,
            mapB,
            opaques
          ),
          t_sup(aDefK, bDefK),
          t_sup(aDefV, bDefV)
        )
      end,
      opaques
    )
  end

  def type(:maps, :put, 3, xs, opaques) do
    strict(
      :maps,
      :put,
      3,
      xs,
      fn [key, value, map] ->
        t_map_put({key, value}, map, opaques)
      end,
      opaques
    )
  end

  def type(:maps, :remove, 2, xs, opaques) do
    strict(
      :maps,
      :remove,
      2,
      xs,
      fn [key, map] ->
        t_map_remove(key, map, opaques)
      end,
      opaques
    )
  end

  def type(:maps, :size, 1, xs, opaques) do
    strict(
      :maps,
      :size,
      1,
      xs,
      fn [map] ->
        mand =
          for e = {_, :mandatory, _} <- t_map_entries(map, opaques) do
            e
          end

        lowerBound = length(mand)

        case t_is_none(t_map_def_key(map, opaques)) do
          false ->
            t_from_range(lowerBound, :pos_inf)

          true ->
            opt =
              for e = {_, :optional, _} <-
                    t_map_entries(
                      map,
                      opaques
                    ) do
                e
              end

            upperBound = lowerBound + length(opt)
            t_from_range(lowerBound, upperBound)
        end
      end,
      opaques
    )
  end

  def type(:maps, :update, 3, xs, opaques) do
    strict(
      :maps,
      :update,
      3,
      xs,
      fn [key, value, map] ->
        t_map_update({key, value}, map, opaques)
      end,
      opaques
    )
  end

  def type(m, f, a, xs, _O)
      when is_atom(m) and
             is_atom(f) and is_integer(a) and 0 <= a and
             a <= 255 do
    strict(xs, t_any())
  end

  defp strict(m, f, a, xs, fun, opaques) do
    ts = arg_types(m, f, a)
    xs1 = inf_lists(xs, ts, opaques)

    case any_is_none_or_unit(xs1) do
      true ->
        t_none()

      false ->
        fun.(xs1)
    end
  end

  defp strict2(xs, x) do
    case any_is_none_or_unit(xs) do
      true ->
        t_none()

      false ->
        x
    end
  end

  defp strict(xs, x) do
    case any_is_none_or_unit(xs) do
      true ->
        t_none()

      false ->
        x
    end
  end

  defp inf_lists([x | xs], [t | ts], opaques) do
    [t_inf(x, t, opaques) | inf_lists(xs, ts, opaques)]
  end

  defp inf_lists([], [], _Opaques) do
    []
  end

  defp any_list(n) do
    any_list(n, t_any())
  end

  defp any_list(n, a) when n > 0 do
    [a | any_list(n - 1, a)]
  end

  defp any_list(0, _) do
    []
  end

  defp list_replace(n, e, [x | xs]) when n > 1 do
    [x | list_replace(n - 1, e, xs)]
  end

  defp list_replace(1, e, [_X | xs]) do
    [e | xs]
  end

  defp any_is_none_or_unit(ts) do
    :lists.any(&:erl_types.t_is_none_or_unit/1, ts)
  end

  defp check_guard([x], test, type, opaques) do
    check_guard_single(x, test, type, opaques)
  end

  defp check_guard_single(x, test, type, opaques) do
    case test.(x) do
      true ->
        t_atom(true)

      false ->
        case t_is_none(t_inf(type, x, opaques)) do
          true ->
            case t_has_opaque_subtype(x, opaques) do
              true ->
                t_none()

              false ->
                t_atom(false)
            end

          false ->
            t_boolean()
        end
    end
  end

  defp check_record_tag(tag, y, opaques) do
    case t_is_atom(tag, opaques) do
      false ->
        tagAtom = t_inf(tag, t_atom(), opaques)

        case t_is_none(tagAtom) do
          true ->
            case t_has_opaque_subtype(tag, opaques) do
              true ->
                t_none()

              false ->
                t_atom(false)
            end

          false ->
            t_boolean()
        end

      true ->
        case t_atom_vals(tag, opaques) do
          [realTag] ->
            case t_atom_vals(y, opaques) do
              [^realTag] ->
                t_atom(true)

              _ ->
                t_boolean()
            end

          _ ->
            t_boolean()
        end
    end
  end

  defp infinity_max([]) do
    :empty
  end

  defp infinity_max([h | t]) do
    cond do
      h === :empty ->
        infinity_max(t)

      true ->
        :lists.foldl(
          fn elem, max ->
            geq = infinity_geq(elem, max)

            cond do
              not geq or elem === :empty ->
                max

              true ->
                elem
            end
          end,
          h,
          t
        )
    end
  end

  defp infinity_min([]) do
    :empty
  end

  defp infinity_min([h | t]) do
    cond do
      h === :empty ->
        infinity_min(t)

      true ->
        :lists.foldl(
          fn elem, min ->
            geq = infinity_geq(elem, min)

            cond do
              geq or elem === :empty ->
                min

              true ->
                elem
            end
          end,
          h,
          t
        )
    end
  end

  defp infinity_abs(:pos_inf) do
    :pos_inf
  end

  defp infinity_abs(:neg_inf) do
    :pos_inf
  end

  defp infinity_abs(number) when is_integer(number) do
    abs(number)
  end

  defp infinity_inv(:pos_inf) do
    :neg_inf
  end

  defp infinity_inv(:neg_inf) do
    :pos_inf
  end

  defp infinity_inv(number) when is_integer(number) do
    -number
  end

  defp infinity_band(:neg_inf, type2) do
    type2
  end

  defp infinity_band(:pos_inf, type2) do
    type2
  end

  defp infinity_band(type1, type2)
       when is_integer(type1) and
              is_integer(type2) do
    type1 &&& type2
  end

  defp infinity_bor(:neg_inf, _Type2) do
    :neg_inf
  end

  defp infinity_bor(:pos_inf, _Type2) do
    :pos_inf
  end

  defp infinity_bor(type1, type2)
       when is_integer(type1) and
              is_integer(type2) do
    type1 ||| type2
  end

  defp infinity_div(:pos_inf, :pos_inf) do
    [0, :pos_inf]
  end

  defp infinity_div(:pos_inf, :neg_inf) do
    [:neg_inf, 0]
  end

  defp infinity_div(:neg_inf, :neg_inf) do
    [0, :pos_inf]
  end

  defp infinity_div(:neg_inf, :pos_inf) do
    [:neg_inf, 0]
  end

  defp infinity_div(:pos_inf, number)
       when is_integer(number) and
              number > 0 do
    :pos_inf
  end

  defp infinity_div(:pos_inf, number)
       when is_integer(number) and
              number < 0 do
    :neg_inf
  end

  defp infinity_div(:neg_inf, number)
       when is_integer(number) and
              number > 0 do
    :neg_inf
  end

  defp infinity_div(:neg_inf, number)
       when is_integer(number) and
              number < 0 do
    :pos_inf
  end

  defp infinity_div(number, :pos_inf)
       when is_integer(number) and
              number >= 0 do
    :pos_inf
  end

  defp infinity_div(number, :pos_inf)
       when is_integer(number) and
              number < 0 do
    :neg_inf
  end

  defp infinity_div(number, :neg_inf)
       when is_integer(number) and
              number >= 0 do
    :neg_inf
  end

  defp infinity_div(number, :neg_inf)
       when is_integer(number) and
              number < 0 do
    :pos_inf
  end

  defp infinity_div(number1, number2)
       when is_integer(number1) and
              is_integer(number2) do
    div(number1, number2)
  end

  defp infinity_bsl(:pos_inf, _) do
    :pos_inf
  end

  defp infinity_bsl(:neg_inf, _) do
    :neg_inf
  end

  defp infinity_bsl(0, :pos_inf) do
    0
  end

  defp infinity_bsl(number, :pos_inf)
       when is_integer(number) and
              number > 0 do
    :pos_inf
  end

  defp infinity_bsl(number, :pos_inf) when is_integer(number) do
    :neg_inf
  end

  defp infinity_bsl(number, :neg_inf)
       when is_integer(number) and
              number >= 0 do
    0
  end

  defp infinity_bsl(number, :neg_inf) when is_integer(number) do
    -1
  end

  defp infinity_bsl(number1, number2)
       when is_integer(number1) and
              is_integer(number2) do
    bits = 128

    cond do
      number2 > bits * 2 ->
        infinity_bsl(number1, :pos_inf)

      number2 < -bits * 2 ->
        infinity_bsl(number1, :neg_inf)

      true ->
        number1 <<< number2
    end
  end

  defp infinity_geq(:pos_inf, _) do
    true
  end

  defp infinity_geq(_, :pos_inf) do
    false
  end

  defp infinity_geq(_, :neg_inf) do
    true
  end

  defp infinity_geq(:neg_inf, _) do
    false
  end

  defp infinity_geq(a, b) when is_integer(a) and is_integer(b) do
    a >= b
  end

  def infinity_add(:pos_inf, _Number) do
    :pos_inf
  end

  def infinity_add(:neg_inf, _Number) do
    :neg_inf
  end

  def infinity_add(_Number, :pos_inf) do
    :pos_inf
  end

  def infinity_add(_Number, :neg_inf) do
    :neg_inf
  end

  def infinity_add(number1, number2)
      when is_integer(number1) and
             is_integer(number2) do
    try do
      number1 + number2
    catch
      :error, :system_limit when number1 < 0 ->
        :neg_inf

      :error, :system_limit ->
        :pos_inf
    end
  end

  defp infinity_mult(:neg_inf, number) do
    greater = infinity_geq(number, 0)

    cond do
      greater ->
        :neg_inf

      true ->
        :pos_inf
    end
  end

  defp infinity_mult(:pos_inf, number) do
    infinity_inv(infinity_mult(:neg_inf, number))
  end

  defp infinity_mult(number, :pos_inf) do
    infinity_inv(infinity_mult(:neg_inf, number))
  end

  defp infinity_mult(number, :neg_inf) do
    infinity_mult(:neg_inf, number)
  end

  defp infinity_mult(number1, number2)
       when is_integer(number1) and
              is_integer(number2) do
    try do
      number1 * number2
    catch
      :error, :system_limit ->
        cond do
          number1 >= 0 === number2 >= 0 ->
            :pos_inf

          true ->
            :neg_inf
        end
    end
  end

  defp width({min, max}) do
    infinity_max([width(min), width(max)])
  end

  defp width(:pos_inf) do
    :pos_inf
  end

  defp width(:neg_inf) do
    :pos_inf
  end

  defp width(x) when is_integer(x) and x >= 0 do
    poswidth(x, 0)
  end

  defp width(x) when is_integer(x) and x < 0 do
    negwidth(x, 0)
  end

  defp poswidth(x, n) do
    case x < 1 <<< n do
      true ->
        n

      false ->
        poswidth(x, n + 1)
    end
  end

  defp negwidth(x, n) do
    case x >= -1 <<< n do
      true ->
        n

      false ->
        negwidth(x, n + 1)
    end
  end

  defp arith_bnot(x1, opaques) do
    case t_is_integer(x1, opaques) do
      false ->
        :error

      true ->
        min1 = number_min(x1, opaques)
        max1 = number_max(x1, opaques)

        {:ok,
         t_from_range(
           infinity_add(infinity_inv(max1), -1),
           infinity_add(infinity_inv(min1), -1)
         )}
    end
  end

  defp arith_abs(x1, opaques) do
    case t_is_integer(x1, opaques) do
      false ->
        case t_is_float(x1, opaques) do
          true ->
            t_float()

          false ->
            t_number()
        end

      true ->
        min1 = number_min(x1, opaques)
        max1 = number_max(x1, opaques)

        {newMin, newMax} =
          case infinity_geq(min1, 0) do
            true ->
              {min1, max1}

            false ->
              negMin1 = infinity_inv(min1)
              negMax1 = infinity_inv(max1)

              case infinity_geq(max1, 0) do
                true ->
                  {0, max(negMin1, max1)}

                false ->
                  {negMax1, negMin1}
              end
          end

        t_from_range(newMin, newMax)
    end
  end

  defp arith_mult(min1, max1, min2, max2) do
    tmp_list = [
      infinity_mult(min1, min2),
      infinity_mult(min1, max2),
      infinity_mult(max1, min2),
      infinity_mult(max1, max2)
    ]

    {infinity_min(tmp_list), infinity_max(tmp_list)}
  end

  defp arith_div(_Min1, _Max1, 0, 0) do
    {:pos_inf, :neg_inf}
  end

  defp arith_div(min1, max1, min2, max2) do
    newMin2 =
      cond do
        min2 === 0 ->
          1

        true ->
          min2
      end

    newMax2 =
      cond do
        max2 === 0 ->
          -1

        true ->
          max2
      end

    tmp_list =
      :lists.flatten([
        infinity_div(min1, newMin2),
        infinity_div(min1, newMax2),
        infinity_div(max1, newMin2),
        infinity_div(max1, newMax2)
      ])

    {infinity_min(tmp_list), infinity_max(tmp_list)}
  end

  defp arith_rem(min1, max1, min2, max2) do
    min1_geq_zero = infinity_geq(min1, 0)
    max1_leq_zero = infinity_geq(0, max1)
    max_range2 = infinity_max([infinity_abs(min2), infinity_abs(max2)])

    new_min =
      cond do
        min1_geq_zero ->
          0

        max_range2 === 0 ->
          0

        true ->
          infinity_add(infinity_inv(max_range2), 1)
      end

    new_max =
      cond do
        max1_leq_zero ->
          0

        max_range2 === 0 ->
          0

        true ->
          infinity_add(max_range2, -1)
      end

    {new_min, new_max}
  end

  defp arith_bsl(min1, max1, min2, max2) do
    case infinity_geq(min1, 0) do
      true ->
        {infinity_bsl(min1, min2), infinity_bsl(max1, max2)}

      false ->
        case infinity_geq(max1, 0) do
          true ->
            {infinity_bsl(min1, max2), infinity_bsl(max1, max2)}

          false ->
            {infinity_bsl(min1, max2), infinity_bsl(max2, min2)}
        end
    end
  end

  defp arith_band_range_set({min, max}, [int | intList]) do
    safeAnd =
      :lists.foldl(
        fn intFromSet, safeAndAcc ->
          intFromSet ||| safeAndAcc
        end,
        int,
        intList
      )

    {infinity_band(min, safeAnd), infinity_band(max, safeAnd)}
  end

  defp arith_bor_range_set({min, max}, [int | intList]) do
    safeAnd =
      :lists.foldl(
        fn intFromSet, safeAndAcc ->
          intFromSet &&& safeAndAcc
        end,
        int,
        intList
      )

    {infinity_bor(min, safeAnd), infinity_bor(max, safeAnd)}
  end

  defp arith_band(x1, x2, opaques) do
    l1 = t_number_vals(x1, opaques)
    l2 = t_number_vals(x2, opaques)
    min1 = number_min(x1, opaques)
    max1 = number_max(x1, opaques)
    min2 = number_min(x2, opaques)
    max2 = number_max(x2, opaques)

    case {l1 === :unknown, l2 === :unknown} do
      {true, false} ->
        arith_band_range_set(
          arith_band_ranges(min1, max1, min2, max2),
          l2
        )

      {false, true} ->
        arith_band_range_set(
          arith_band_ranges(min1, max1, min2, max2),
          l1
        )

      {true, true} ->
        arith_band_ranges(min1, max1, min2, max2)
    end
  end

  defp arith_bor(x1, x2, opaques) do
    l1 = t_number_vals(x1, opaques)
    l2 = t_number_vals(x2, opaques)
    min1 = number_min(x1, opaques)
    max1 = number_max(x1, opaques)
    min2 = number_min(x2, opaques)
    max2 = number_max(x2, opaques)

    case {l1 === :unknown, l2 === :unknown} do
      {true, false} ->
        arith_bor_range_set(
          arith_bor_ranges(min1, max1, min2, max2),
          l2
        )

      {false, true} ->
        arith_bor_range_set(
          arith_bor_ranges(min1, max1, min2, max2),
          l1
        )

      {true, true} ->
        arith_bor_ranges(min1, max1, min2, max2)
    end
  end

  defp arith_band_ranges(min1, max1, min2, max2) do
    width = infinity_min([width({min1, max1}), width({min2, max2})])

    min =
      case infinity_geq(min1, 0) or
             infinity_geq(
               min2,
               0
             ) do
        true ->
          0

        false ->
          infinity_bsl(-1, width)
      end

    max =
      case infinity_geq(max1, 0) or
             infinity_geq(
               max2,
               0
             ) do
        true ->
          infinity_add(infinity_bsl(1, width), -1)

        false ->
          0
      end

    {min, max}
  end

  defp arith_bor_ranges(min1, max1, min2, max2) do
    width = infinity_max([width({min1, max1}), width({min2, max2})])

    min =
      case infinity_geq(
             min1,
             0
           ) and infinity_geq(min2, 0) do
        true ->
          0

        false ->
          infinity_bsl(-1, width)
      end

    max =
      case infinity_geq(
             max1,
             0
           ) and infinity_geq(max2, 0) do
        true ->
          infinity_add(infinity_bsl(1, width), -1)

        false ->
          -1
      end

    {min, max}
  end

  defp arith(op, x1, x2, opaques) do
    case t_is_integer(x1, opaques) and
           t_is_integer(
             x2,
             opaques
           ) do
      false ->
        :error

      true ->
        l1 = t_number_vals(x1, opaques)
        l2 = t_number_vals(x2, opaques)

        case l1 === :unknown or l2 === :unknown do
          true ->
            min1 = number_min(x1, opaques)
            max1 = number_max(x1, opaques)
            min2 = number_min(x2, opaques)
            max2 = number_max(x2, opaques)

            {newMin, newMax} =
              case op do
                :+ ->
                  {infinity_add(min1, min2), infinity_add(max1, max2)}

                :- ->
                  {infinity_add(min1, infinity_inv(max2)), infinity_add(max1, infinity_inv(min2))}

                :* ->
                  arith_mult(min1, max1, min2, max2)

                :div ->
                  arith_div(min1, max1, min2, max2)

                :rem ->
                  arith_rem(min1, max1, min2, max2)

                :bsl ->
                  arith_bsl(min1, max1, min2, max2)

                :bsr ->
                  newMin2 = infinity_inv(max2)
                  newMax2 = infinity_inv(min2)
                  arith_bsl(min1, max1, newMin2, newMax2)

                :band ->
                  arith_band(x1, x2, opaques)

                :bor ->
                  arith_bor(x1, x2, opaques)

                :bxor ->
                  arith_bor_ranges(min1, max1, min2, max2)
              end

            {:ok, t_from_range(newMin, newMax)}

          false ->
            try do
              case op do
                :+ ->
                  for x <- l1, y <- l2 do
                    x + y
                  end

                :- ->
                  for x <- l1, y <- l2 do
                    x - y
                  end

                :* ->
                  for x <- l1, y <- l2 do
                    x * y
                  end

                :div ->
                  for x <- l1, y <- l2, y !== 0 do
                    div(x, y)
                  end

                :rem ->
                  for x <- l1, y <- l2, y !== 0 do
                    rem(x, y)
                  end

                :bsl ->
                  for x <- l1, y <- l2 do
                    x <<< y
                  end

                :bsr ->
                  for x <- l1, y <- l2 do
                    x >>> y
                  end

                :band ->
                  for x <- l1, y <- l2 do
                    x &&& y
                  end

                :bor ->
                  for x <- l1, y <- l2 do
                    x ||| y
                  end

                :bxor ->
                  for x <- l1, y <- l2 do
                    x ^^^ y
                  end
              end
            catch
              :error, :system_limit ->
                :error
            else
              allVals ->
                {:ok, t_integers(:ordsets.from_list(allVals))}
            end
        end
    end
  end

  defp compare(op, lhs, rhs, opaques) do
    case t_is_none(t_inf(lhs, rhs, opaques)) do
      false ->
        t_boolean()

      true ->
        case opaque_args(:erlang, op, 2, [lhs, rhs], opaques) === [] do
          true ->
            case op do
              :< ->
                always_smaller(lhs, rhs, opaques)

              :> ->
                always_smaller(rhs, lhs, opaques)

              :"=<" ->
                always_smaller(lhs, rhs, opaques)

              :>= ->
                always_smaller(rhs, lhs, opaques)
            end

          false ->
            t_none()
        end
    end
  end

  defp always_smaller(type1, type2, opaques) do
    {min1, max1} = type_ranks(type1, opaques)
    {min2, max2} = type_ranks(type2, opaques)

    cond do
      max1 < min2 ->
        t_atom(true)

      min1 > max2 ->
        t_atom(false)

      true ->
        t_boolean()
    end
  end

  defp type_ranks(type, opaques) do
    type_ranks(type, 1, 0, 0, type_order(), opaques)
  end

  defp type_ranks(_Type, _I, min, max, [], _Opaques) do
    {min, max}
  end

  defp type_ranks(type, i, min, max, [typeClass | rest], opaques) do
    {newMin, newMax} =
      case t_is_none(t_inf(type, typeClass, opaques)) do
        true ->
          {min, max}

        false ->
          case min do
            0 ->
              {i, i}

            _ ->
              {min, i}
          end
      end

    type_ranks(type, i + 1, newMin, newMax, rest, opaques)
  end

  defp type_order() do
    [
      t_number(),
      t_atom(),
      t_reference(),
      t_fun(),
      t_port(),
      t_pid(),
      t_tuple(),
      t_map(),
      t_list(),
      t_bitstr()
    ]
  end

  defp key_comparisons_fail(x0, keyPos, tupleList, opaques) do
    x = :erl_types.t_widen_to_number(x0)

    :lists.all(
      fn tuple ->
        key = type(:erlang, :element, 2, [keyPos, tuple])
        t_is_none(t_inf(key, x, opaques))
      end,
      tupleList
    )
  end

  def arg_types(:erlang, :!, 2) do
    pid = t_sup([t_pid(), t_port(), t_atom(), t_tuple([t_atom(), t_node()])])
    [pid, t_any()]
  end

  def arg_types(:erlang, :==, 2) do
    [t_any(), t_any()]
  end

  def arg_types(:erlang, :"/=", 2) do
    [t_any(), t_any()]
  end

  def arg_types(:erlang, :"=:=", 2) do
    [t_any(), t_any()]
  end

  def arg_types(:erlang, :"=/=", 2) do
    [t_any(), t_any()]
  end

  def arg_types(:erlang, :>, 2) do
    [t_any(), t_any()]
  end

  def arg_types(:erlang, :>=, 2) do
    [t_any(), t_any()]
  end

  def arg_types(:erlang, :<, 2) do
    [t_any(), t_any()]
  end

  def arg_types(:erlang, :"=<", 2) do
    [t_any(), t_any()]
  end

  def arg_types(:erlang, :+, 1) do
    [t_number()]
  end

  def arg_types(:erlang, :+, 2) do
    [t_number(), t_number()]
  end

  def arg_types(:erlang, :++, 2) do
    [t_list(), t_any()]
  end

  def arg_types(:erlang, :-, 1) do
    [t_number()]
  end

  def arg_types(:erlang, :-, 2) do
    [t_number(), t_number()]
  end

  def arg_types(:erlang, :--, 2) do
    [t_list(), t_list()]
  end

  def arg_types(:erlang, :*, 2) do
    [t_number(), t_number()]
  end

  def arg_types(:erlang, :/, 2) do
    [t_number(), t_number()]
  end

  def arg_types(:erlang, :div, 2) do
    [t_integer(), t_integer()]
  end

  def arg_types(:erlang, :rem, 2) do
    [t_integer(), t_integer()]
  end

  def arg_types(:erlang, :and, 2) do
    [t_boolean(), t_boolean()]
  end

  def arg_types(:erlang, :or, 2) do
    [t_boolean(), t_boolean()]
  end

  def arg_types(:erlang, :xor, 2) do
    [t_boolean(), t_boolean()]
  end

  def arg_types(:erlang, :not, 1) do
    [t_boolean()]
  end

  def arg_types(:erlang, :band, 2) do
    [t_integer(), t_integer()]
  end

  def arg_types(:erlang, :bor, 2) do
    [t_integer(), t_integer()]
  end

  def arg_types(:erlang, :bxor, 2) do
    [t_integer(), t_integer()]
  end

  def arg_types(:erlang, :bsr, 2) do
    [t_integer(), t_integer()]
  end

  def arg_types(:erlang, :bsl, 2) do
    [t_integer(), t_integer()]
  end

  def arg_types(:erlang, :bnot, 1) do
    [t_integer()]
  end

  def arg_types(:erlang, :abs, 1) do
    [t_number()]
  end

  def arg_types(:erlang, :append, 2) do
    arg_types(:erlang, :++, 2)
  end

  def arg_types(:erlang, :apply, 2) do
    [t_sup(t_tuple([t_module(), t_atom()]), t_fun()), t_list()]
  end

  def arg_types(:erlang, :apply, 3) do
    [t_sup(t_atom(), t_tuple()), t_atom(), t_list()]
  end

  def arg_types(:erlang, :binary_part, 2) do
    [t_binary(), t_tuple([t_non_neg_integer(), t_integer()])]
  end

  def arg_types(:erlang, :binary_part, 3) do
    [t_binary(), t_non_neg_integer(), t_integer()]
  end

  def arg_types(:erlang, :bit_size, 1) do
    [t_bitstr()]
  end

  def arg_types(:erlang, :byte_size, 1) do
    [t_bitstr()]
  end

  def arg_types(:erlang, :ceil, 1) do
    [t_number()]
  end

  def arg_types(:erlang, :halt, 0) do
    []
  end

  def arg_types(:erlang, :halt, 1) do
    [t_sup([t_non_neg_fixnum(), t_atom(:abort), t_string()])]
  end

  def arg_types(:erlang, :halt, 2) do
    [
      t_sup([t_non_neg_fixnum(), t_atom(:abort), t_string()]),
      t_list(t_tuple([t_atom(:flush), t_boolean()]))
    ]
  end

  def arg_types(:erlang, :error, 1) do
    [t_any()]
  end

  def arg_types(:erlang, :error, 2) do
    [t_any(), t_list()]
  end

  def arg_types(:erlang, :exit, 1) do
    [t_any()]
  end

  def arg_types(:erlang, :element, 2) do
    [t_pos_fixnum(), t_tuple()]
  end

  def arg_types(:erlang, :float, 1) do
    [t_number()]
  end

  def arg_types(:erlang, :floor, 1) do
    [t_number()]
  end

  def arg_types(:erlang, :build_stacktrace, 0) do
    []
  end

  def arg_types(:erlang, :hd, 1) do
    [t_cons()]
  end

  def arg_types(:erlang, :info, 1) do
    arg_types(:erlang, :system_info, 1)
  end

  def arg_types(:erlang, :is_atom, 1) do
    [t_any()]
  end

  def arg_types(:erlang, :is_binary, 1) do
    [t_any()]
  end

  def arg_types(:erlang, :is_bitstring, 1) do
    [t_any()]
  end

  def arg_types(:erlang, :is_boolean, 1) do
    [t_any()]
  end

  def arg_types(:erlang, :is_float, 1) do
    [t_any()]
  end

  def arg_types(:erlang, :is_function, 1) do
    [t_any()]
  end

  def arg_types(:erlang, :is_function, 2) do
    [t_any(), t_arity()]
  end

  def arg_types(:erlang, :is_integer, 1) do
    [t_any()]
  end

  def arg_types(:erlang, :is_list, 1) do
    [t_any()]
  end

  def arg_types(:erlang, :is_map, 1) do
    [t_any()]
  end

  def arg_types(:erlang, :is_map_key, 2) do
    [t_any(), t_map()]
  end

  def arg_types(:erlang, :is_number, 1) do
    [t_any()]
  end

  def arg_types(:erlang, :is_pid, 1) do
    [t_any()]
  end

  def arg_types(:erlang, :is_port, 1) do
    [t_any()]
  end

  def arg_types(:erlang, :is_record, 2) do
    [t_any(), t_atom()]
  end

  def arg_types(:erlang, :is_record, 3) do
    [t_any(), t_atom(), t_non_neg_fixnum()]
  end

  def arg_types(:erlang, :is_reference, 1) do
    [t_any()]
  end

  def arg_types(:erlang, :is_tuple, 1) do
    [t_any()]
  end

  def arg_types(:erlang, :length, 1) do
    [t_list()]
  end

  def arg_types(:erlang, :map_size, 1) do
    [t_map()]
  end

  def arg_types(:erlang, :map_get, 2) do
    [t_any(), t_map()]
  end

  def arg_types(:erlang, :make_fun, 3) do
    [t_atom(), t_atom(), t_arity()]
  end

  def arg_types(:erlang, :make_tuple, 2) do
    [t_non_neg_fixnum(), t_any()]
  end

  def arg_types(:erlang, :make_tuple, 3) do
    [t_non_neg_fixnum(), t_any(), t_list(t_tuple([t_pos_integer(), t_any()]))]
  end

  def arg_types(:erlang, :nif_error, 1) do
    [t_any()]
  end

  def arg_types(:erlang, :nif_error, 2) do
    [t_any(), t_list()]
  end

  def arg_types(:erlang, :node, 0) do
    []
  end

  def arg_types(:erlang, :node, 1) do
    [t_identifier()]
  end

  def arg_types(:erlang, :round, 1) do
    [t_number()]
  end

  def arg_types(:erlang, :self, 0) do
    []
  end

  def arg_types(:erlang, :setelement, 3) do
    [t_pos_integer(), t_tuple(), t_any()]
  end

  def arg_types(:erlang, :size, 1) do
    [t_sup(t_tuple(), t_binary())]
  end

  def arg_types(:erlang, :subtract, 2) do
    arg_types(:erlang, :--, 2)
  end

  def arg_types(:erlang, :system_info, 1) do
    [
      t_sup([
        t_atom(),
        t_tuple([t_atom(), t_any()]),
        t_tuple([t_atom(), t_atom(), t_any()]),
        t_tuple([t_atom(:allocator_sizes), t_reference(), t_any()])
      ])
    ]
  end

  def arg_types(:erlang, :throw, 1) do
    [t_any()]
  end

  def arg_types(:erlang, :tl, 1) do
    [t_cons()]
  end

  def arg_types(:erlang, :trunc, 1) do
    [t_number()]
  end

  def arg_types(:erlang, :tuple_size, 1) do
    [t_tuple()]
  end

  def arg_types(:erlang, :tuple_to_list, 1) do
    [t_tuple()]
  end

  def arg_types(:hipe_bifs, :add_ref, 2) do
    [
      t_mfa(),
      t_tuple([
        t_mfa(),
        t_integer(),
        t_sup(t_atom(:call), t_atom(:load_mfa)),
        t_trampoline(),
        t_binary()
      ])
    ]
  end

  def arg_types(:hipe_bifs, :alloc_data, 3) do
    [t_integer(), t_integer(), t_binary()]
  end

  def arg_types(:hipe_bifs, :array, 2) do
    [t_non_neg_fixnum(), t_immediate()]
  end

  def arg_types(:hipe_bifs, :array_length, 1) do
    [t_immarray()]
  end

  def arg_types(:hipe_bifs, :array_sub, 2) do
    [t_immarray(), t_non_neg_fixnum()]
  end

  def arg_types(:hipe_bifs, :array_update, 3) do
    [t_immarray(), t_non_neg_fixnum(), t_immediate()]
  end

  def arg_types(:hipe_bifs, :atom_to_word, 1) do
    [t_atom()]
  end

  def arg_types(:hipe_bifs, :bif_address, 3) do
    [t_atom(), t_atom(), t_arity()]
  end

  def arg_types(:hipe_bifs, :bitarray, 2) do
    [t_non_neg_fixnum(), t_boolean()]
  end

  def arg_types(:hipe_bifs, :bitarray_sub, 2) do
    [t_bitarray(), t_non_neg_fixnum()]
  end

  def arg_types(:hipe_bifs, :bitarray_update, 3) do
    [t_bytearray(), t_non_neg_fixnum(), t_boolean()]
  end

  def arg_types(:hipe_bifs, :bytearray, 2) do
    [t_non_neg_fixnum(), t_byte()]
  end

  def arg_types(:hipe_bifs, :bytearray_sub, 2) do
    [t_bytearray(), t_non_neg_fixnum()]
  end

  def arg_types(:hipe_bifs, :bytearray_update, 3) do
    [t_bytearray(), t_non_neg_fixnum(), t_byte()]
  end

  def arg_types(:hipe_bifs, :call_count_clear, 1) do
    [t_mfa()]
  end

  def arg_types(:hipe_bifs, :call_count_get, 1) do
    [t_mfa()]
  end

  def arg_types(:hipe_bifs, :call_count_off, 1) do
    [t_mfa()]
  end

  def arg_types(:hipe_bifs, :call_count_on, 1) do
    [t_mfa()]
  end

  def arg_types(:hipe_bifs, :check_crc, 1) do
    [t_crc32()]
  end

  def arg_types(:hipe_bifs, :enter_code, 3) do
    [t_binary(), t_sup(t_nil(), t_tuple()), t_binary()]
  end

  def arg_types(:hipe_bifs, :enter_sdesc, 2) do
    [
      t_tuple([t_integer(), t_integer(), t_integer(), t_integer(), t_integer(), t_mfa()]),
      t_binary()
    ]
  end

  def arg_types(:hipe_bifs, :find_na_or_make_stub, 1) do
    [t_mfa()]
  end

  def arg_types(:hipe_bifs, :fun_to_address, 1) do
    [t_mfa()]
  end

  def arg_types(:hipe_bifs, :get_fe, 2) do
    [t_atom(), t_tuple([t_integer(), t_integer(), t_integer()])]
  end

  def arg_types(:hipe_bifs, :get_rts_param, 1) do
    [t_fixnum()]
  end

  def arg_types(:hipe_bifs, :merge_term, 1) do
    [t_any()]
  end

  def arg_types(:hipe_bifs, :nstack_used_size, 0) do
    []
  end

  def arg_types(:hipe_bifs, :patch_call, 3) do
    [t_integer(), t_integer(), t_trampoline()]
  end

  def arg_types(:hipe_bifs, :patch_insn, 3) do
    [t_integer(), t_integer(), t_insn_type()]
  end

  def arg_types(:hipe_bifs, :primop_address, 1) do
    [t_atom()]
  end

  def arg_types(:hipe_bifs, :ref, 1) do
    [t_immediate()]
  end

  def arg_types(:hipe_bifs, :ref_get, 1) do
    [t_hiperef()]
  end

  def arg_types(:hipe_bifs, :ref_set, 2) do
    [t_hiperef(), t_immediate()]
  end

  def arg_types(:hipe_bifs, :set_funinfo_native_address, 3) do
    arg_types(:hipe_bifs, :set_native_address, 3)
  end

  def arg_types(:hipe_bifs, :commit_patch_load, 1) do
    [t_binary()]
  end

  def arg_types(:hipe_bifs, :set_native_address, 3) do
    [t_mfa(), t_integer(), t_boolean()]
  end

  def arg_types(:hipe_bifs, :set_native_address_in_fe, 2) do
    [t_integer(), t_integer()]
  end

  def arg_types(:hipe_bifs, :system_crc, 0) do
    []
  end

  def arg_types(:hipe_bifs, :term_to_word, 1) do
    [t_any()]
  end

  def arg_types(:hipe_bifs, :write_u8, 2) do
    [t_integer(), t_byte()]
  end

  def arg_types(:hipe_bifs, :write_u32, 2) do
    [t_integer(), t_integer()]
  end

  def arg_types(:hipe_bifs, :write_u64, 2) do
    [t_integer(), t_integer()]
  end

  def arg_types(:hipe_bifs, :alloc_loader_state, 1) do
    [t_atom()]
  end

  def arg_types(:lists, :all, 2) do
    [t_fun([t_any()], t_boolean()), t_list()]
  end

  def arg_types(:lists, :any, 2) do
    [t_fun([t_any()], t_boolean()), t_list()]
  end

  def arg_types(:lists, :append, 2) do
    arg_types(:erlang, :++, 2)
  end

  def arg_types(:lists, :delete, 2) do
    [t_any(), t_maybe_improper_list()]
  end

  def arg_types(:lists, :dropwhile, 2) do
    [t_fun([t_any()], t_boolean()), t_maybe_improper_list()]
  end

  def arg_types(:lists, :filter, 2) do
    [t_fun([t_any()], t_boolean()), t_list()]
  end

  def arg_types(:lists, :flatten, 1) do
    [t_list()]
  end

  def arg_types(:lists, :flatmap, 2) do
    [t_fun([t_any()], t_list()), t_list()]
  end

  def arg_types(:lists, :foreach, 2) do
    [t_fun([t_any()], t_any()), t_list()]
  end

  def arg_types(:lists, :foldl, 3) do
    [t_fun([t_any(), t_any()], t_any()), t_any(), t_list()]
  end

  def arg_types(:lists, :foldr, 3) do
    arg_types(:lists, :foldl, 3)
  end

  def arg_types(:lists, :keydelete, 3) do
    [t_any(), t_pos_fixnum(), t_maybe_improper_list()]
  end

  def arg_types(:lists, :keyfind, 3) do
    arg_types(:lists, :keysearch, 3)
  end

  def arg_types(:lists, :keymap, 3) do
    [t_fun([t_any()], t_any()), t_pos_fixnum(), t_list(t_tuple())]
  end

  def arg_types(:lists, :keymember, 3) do
    [t_any(), t_pos_fixnum(), t_maybe_improper_list()]
  end

  def arg_types(:lists, :keymerge, 3) do
    [t_pos_fixnum(), t_list(t_tuple()), t_list(t_tuple())]
  end

  def arg_types(:lists, :keyreplace, 4) do
    [t_any(), t_pos_fixnum(), t_maybe_improper_list(), t_tuple()]
  end

  def arg_types(:lists, :keysearch, 3) do
    [t_any(), t_pos_fixnum(), t_maybe_improper_list()]
  end

  def arg_types(:lists, :keysort, 2) do
    [t_pos_fixnum(), t_list(t_tuple())]
  end

  def arg_types(:lists, :last, 1) do
    [t_nonempty_list()]
  end

  def arg_types(:lists, :map, 2) do
    [t_fun([t_any()], t_any()), t_list()]
  end

  def arg_types(:lists, :mapfoldl, 3) do
    [t_fun([t_any(), t_any()], t_tuple([t_any(), t_any()])), t_any(), t_list()]
  end

  def arg_types(:lists, :mapfoldr, 3) do
    arg_types(:lists, :mapfoldl, 3)
  end

  def arg_types(:lists, :max, 1) do
    [t_nonempty_list()]
  end

  def arg_types(:lists, :member, 2) do
    [t_any(), t_list()]
  end

  def arg_types(:lists, :merge, 2) do
    [t_list(), t_list()]
  end

  def arg_types(:lists, :min, 1) do
    [t_nonempty_list()]
  end

  def arg_types(:lists, :nth, 2) do
    [t_pos_fixnum(), t_nonempty_list()]
  end

  def arg_types(:lists, :nthtail, 2) do
    [t_non_neg_fixnum(), t_nonempty_list()]
  end

  def arg_types(:lists, :partition, 2) do
    arg_types(:lists, :filter, 2)
  end

  def arg_types(:lists, :reverse, 1) do
    [t_list()]
  end

  def arg_types(:lists, :reverse, 2) do
    [t_list(), t_any()]
  end

  def arg_types(:lists, :sort, 1) do
    [t_list()]
  end

  def arg_types(:lists, :sort, 2) do
    [t_fun([t_any(), t_any()], t_boolean()), t_list()]
  end

  def arg_types(:lists, :split, 2) do
    [t_non_neg_fixnum(), t_maybe_improper_list()]
  end

  def arg_types(:lists, :splitwith, 2) do
    [t_fun([t_any()], t_boolean()), t_maybe_improper_list()]
  end

  def arg_types(:lists, :subtract, 2) do
    arg_types(:erlang, :--, 2)
  end

  def arg_types(:lists, :takewhile, 2) do
    [t_fun([t_any()], t_boolean()), t_maybe_improper_list()]
  end

  def arg_types(:lists, :usort, 1) do
    arg_types(:lists, :sort, 1)
  end

  def arg_types(:lists, :usort, 2) do
    arg_types(:lists, :sort, 2)
  end

  def arg_types(:lists, :unzip, 1) do
    [t_list(t_tuple(2))]
  end

  def arg_types(:lists, :unzip3, 1) do
    [t_list(t_tuple(3))]
  end

  def arg_types(:lists, :zip, 2) do
    [t_list(), t_list()]
  end

  def arg_types(:lists, :zip3, 3) do
    [t_list(), t_list(), t_list()]
  end

  def arg_types(:lists, :zipwith, 3) do
    [t_fun([t_any(), t_any()], t_any()), t_list(), t_list()]
  end

  def arg_types(:lists, :zipwith3, 4) do
    [t_fun([t_any(), t_any(), t_any()], t_any()), t_list(), t_list(), t_list()]
  end

  def arg_types(:maps, :from_list, 1) do
    [t_list(t_tuple(2))]
  end

  def arg_types(:maps, :get, 2) do
    [t_any(), t_map()]
  end

  def arg_types(:maps, :is_key, 2) do
    [t_any(), t_map()]
  end

  def arg_types(:maps, :merge, 2) do
    [t_map(), t_map()]
  end

  def arg_types(:maps, :put, 3) do
    [t_any(), t_any(), t_map()]
  end

  def arg_types(:maps, :remove, 2) do
    [t_any(), t_map()]
  end

  def arg_types(:maps, :size, 1) do
    [t_map()]
  end

  def arg_types(:maps, :update, 3) do
    [t_any(), t_any(), t_map()]
  end

  def arg_types(m, f, a)
      when is_atom(m) and is_atom(f) and
             is_integer(a) and 0 <= a and a <= 255 do
    :unknown
  end

  def is_known(m, f, a) do
    arg_types(m, f, a) !== :unknown
  end

  def opaque_args(_M, _F, _A, _Xs, :universe) do
    []
  end

  def opaque_args(m, f, a, xs, opaques) do
    case kind_of_check(m, f, a) do
      :record ->
        [[x, y] | _] = xs

        for _ <- [:EFE_DUMMY_GEN],
            (case t_is_tuple(x, opaques) do
               true ->
                 case t_tuple_subtypes(x, opaques) do
                   :unknown ->
                     false

                   list when length(list) >= 1 ->
                     t_is_atom(y, opaques) and opaque_recargs(list, y, opaques)
                 end

               false ->
                 t_has_opaque_subtype(x, opaques)
             end) do
          1
        end

      :subtype ->
        for {n, x} <- :lists.zip(:lists.seq(1, length(xs)), xs),
            t_has_opaque_subtype(x, opaques) do
          n
        end

      :find_unknown ->
        [l, r] = xs
        :erl_types.t_find_unknown_opaque(l, r, opaques)

      :no_check ->
        []
    end
  end

  defp kind_of_check(:erlang, :is_record, 3) do
    :record
  end

  defp kind_of_check(:erlang, :is_record, 2) do
    :record
  end

  defp kind_of_check(:erlang, f, a) do
    case :erl_internal.guard_bif(
           f,
           a
         ) or :erl_internal.bool_op(f, a) do
      true ->
        :subtype

      false ->
        case :erl_internal.comp_op(f, a) do
          true ->
            :find_unknown

          false ->
            :no_check
        end
    end
  end

  defp kind_of_check(_M, _F, _A) do
    :no_check
  end

  defp opaque_recargs(tuples, y, opaques) do
    fun = fn tuple ->
      case t_tuple_args(tuple, opaques) do
        [tag | _] ->
          t_is_none(check_record_tag(tag, y, opaques))

        _ ->
          false
      end
    end

    :lists.all(fun, tuples)
  end

  defp check_fun_application(fun, args, opaques) do
    case t_is_fun(fun, opaques) do
      true ->
        case t_fun_args(fun, opaques) do
          :unknown ->
            case t_is_none_or_unit(t_fun_range(fun, opaques)) do
              true ->
                :error

              false ->
                :ok
            end

          funDom when length(funDom) === length(args) ->
            case any_is_none_or_unit(inf_lists(funDom, args, opaques)) do
              true ->
                :error

              false ->
                case t_is_none_or_unit(t_fun_range(fun, opaques)) do
                  true ->
                    :error

                  false ->
                    :ok
                end
            end

          _ ->
            :error
        end

      false ->
        :error
    end
  end

  defp t_endian() do
    t_sup(t_atom(:big), t_atom(:little))
  end

  defp t_crc32() do
    t_non_neg_integer()
  end

  defp t_sequential_tracer() do
    t_sup([t_atom(false), t_pid(), t_port()])
  end

  defp t_system_cpu_topology() do
    t_sup(
      t_atom(:undefined),
      t_system_cpu_topology_level_entry_list()
    )
  end

  defp t_system_cpu_topology_level_entry_list() do
    t_list(t_system_cpu_topology_level_entry())
  end

  defp t_system_cpu_topology_level_entry() do
    t_sup(
      t_tuple([t_system_cpu_topology_level_tag(), t_system_cpu_topology_sublevel_entry()]),
      t_tuple([
        t_system_cpu_topology_level_tag(),
        t_system_cpu_topology_info_list(),
        t_system_cpu_topology_sublevel_entry()
      ])
    )
  end

  defp t_system_cpu_topology_sublevel_entry() do
    t_sup(
      t_system_cpu_topology_logical_cpu_id(),
      t_list(t_tuple())
    )
  end

  defp t_system_cpu_topology_level_tag() do
    t_atoms([:core, :node, :processor, :thread])
  end

  defp t_system_cpu_topology_logical_cpu_id() do
    t_tuple([t_atom(:logical), t_non_neg_fixnum()])
  end

  defp t_system_cpu_topology_info_list() do
    t_nil()
  end

  defp t_internal_cpu_topology() do
    t_sup(
      t_list(
        t_tuple([
          t_atom(:cpu),
          t_non_neg_fixnum(),
          t_non_neg_fixnum(),
          t_non_neg_fixnum(),
          t_non_neg_fixnum(),
          t_non_neg_fixnum(),
          t_non_neg_fixnum()
        ])
      ),
      t_atom(:undefined)
    )
  end

  defp t_scheduler_bind_type_results() do
    t_sup([
      t_atom(:no_node_processor_spread),
      t_atom(:no_node_thread_spread),
      t_atom(:no_spread),
      t_atom(:processor_spread),
      t_atom(:spread),
      t_atom(:thread_spread),
      t_atom(:thread_no_node_processor_spread),
      t_atom(:unbound)
    ])
  end

  defp t_system_multi_scheduling() do
    t_sup([t_atom(:blocked), t_atom(:disabled), t_atom(:enabled)])
  end

  defp t_trampoline() do
    t_sup(t_nil(), t_integer())
  end

  defp t_immediate() do
    t_sup([t_nil(), t_atom(), t_fixnum()])
  end

  defp t_immarray() do
    t_integer()
  end

  defp t_hiperef() do
    t_immarray()
  end

  defp t_bitarray() do
    t_bitstr()
  end

  defp t_bytearray() do
    t_binary()
  end

  defp t_insn_type() do
    t_sup([
      t_atom(:load_mfa),
      t_atom(:x86_abs_pcrel),
      t_atom(:atom),
      t_atom(:constant),
      t_atom(:c_const),
      t_atom(:closure)
    ])
  end
end
