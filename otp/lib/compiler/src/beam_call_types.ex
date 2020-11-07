defmodule :m_beam_call_types do
  use Bitwise
  import :lists, only: [duplicate: 2, foldl: 3]
  require Record
  Record.defrecord(:r_t_atom, :t_atom, elements: :any)
  Record.defrecord(:r_t_bitstring, :t_bitstring, size_unit: 1)
  Record.defrecord(:r_t_bs_context, :t_bs_context, tail_unit: 1, slots: 0, valid: 0)
  Record.defrecord(:r_t_bs_matchable, :t_bs_matchable, tail_unit: 1)
  Record.defrecord(:r_t_float, :t_float, elements: :any)
  Record.defrecord(:r_t_fun, :t_fun, arity: :any, type: :any)
  Record.defrecord(:r_t_integer, :t_integer, elements: :any)

  Record.defrecord(:r_t_map, :t_map,
    super_key: :any,
    super_value: :any
  )

  Record.defrecord(:r_t_cons, :t_cons,
    type: :any,
    terminator: :any
  )

  Record.defrecord(:r_t_list, :t_list,
    type: :any,
    terminator: :any
  )

  Record.defrecord(:r_t_tuple, :t_tuple, size: 0, exact: false, elements: %{})

  Record.defrecord(:r_t_union, :t_union,
    atom: :none,
    list: :none,
    number: :none,
    tuple_set: :none,
    other: :none
  )

  def will_succeed(:erlang, :++, [lHS, _RHS]) do
    succeeds_if_type(lHS, proper_list())
  end

  def will_succeed(:erlang, :--, [lHS, rHS]) do
    case {succeeds_if_type(lHS, proper_list()), succeeds_if_type(rHS, proper_list())} do
      {:yes, :yes} ->
        :yes

      {:no, _} ->
        :no

      {_, :no} ->
        :no

      {_, _} ->
        :maybe
    end
  end

  def will_succeed(:erlang, boolOp, [lHS, rHS])
      when boolOp === :and or boolOp === :or do
    case {succeeds_if_type(
            lHS,
            :beam_types.make_boolean()
          ), succeeds_if_type(rHS, :beam_types.make_boolean())} do
      {:yes, :yes} ->
        :yes

      {:no, _} ->
        :no

      {_, :no} ->
        :no

      {_, _} ->
        :maybe
    end
  end

  def will_succeed(:erlang, :bit_size, [arg]) do
    succeeds_if_type(arg, r_t_bitstring())
  end

  def will_succeed(:erlang, :byte_size, [arg]) do
    succeeds_if_type(arg, r_t_bitstring())
  end

  def will_succeed(:erlang, :hd, [arg]) do
    succeeds_if_type(arg, r_t_cons())
  end

  def will_succeed(:erlang, :is_function, [_, r_t_integer(elements: {min, _})])
      when min >= 0 do
    :yes
  end

  def will_succeed(:erlang, :is_map_key, [_Key, map]) do
    succeeds_if_type(map, r_t_map())
  end

  def will_succeed(:erlang, :length, [arg]) do
    succeeds_if_type(arg, proper_list())
  end

  def will_succeed(:erlang, :map_size, [arg]) do
    succeeds_if_type(arg, r_t_map())
  end

  def will_succeed(:erlang, :not, [arg]) do
    succeeds_if_type(arg, :beam_types.make_boolean())
  end

  def will_succeed(:erlang, :setelement, [
        r_t_integer(elements: {min, max}),
        r_t_tuple(exact: exact, size: size),
        _
      ]) do
    case min >= 1 and max <= size do
      true ->
        :yes

      false when exact ->
        :no

      false ->
        :maybe
    end
  end

  def will_succeed(:erlang, :size, [arg]) do
    argType = :beam_types.join(r_t_tuple(), r_t_bitstring())
    succeeds_if_type(arg, argType)
  end

  def will_succeed(:erlang, :tuple_size, [arg]) do
    succeeds_if_type(arg, r_t_tuple())
  end

  def will_succeed(:erlang, :tl, [arg]) do
    succeeds_if_type(arg, r_t_cons())
  end

  def will_succeed(mod, func, args) do
    arity = length(args)

    case :erl_bifs.is_safe(mod, func, arity) do
      true ->
        :yes

      false ->
        case :erl_bifs.is_exit_bif(mod, func, arity) do
          true ->
            :no

          false ->
            case types(mod, func, args) do
              {:none, _, _} ->
                :no

              {_, argTypes, _} ->
                fails_on_conflict(args, argTypes)
            end
        end
    end
  end

  defp fails_on_conflict([argType | args], [required | types]) do
    case :beam_types.meet(argType, required) do
      :none ->
        :no

      _ ->
        fails_on_conflict(args, types)
    end
  end

  defp fails_on_conflict([], []) do
    :maybe
  end

  defp succeeds_if_type(argType, required) do
    case :beam_types.meet(argType, required) do
      ^argType ->
        :yes

      :none ->
        :no

      _ ->
        :maybe
    end
  end

  def types(:erlang, :map_size, [_]) do
    sub_safe(r_t_integer(), [r_t_map()])
  end

  def types(:erlang, :tuple_size, [_]) do
    sub_safe(r_t_integer(), [r_t_tuple()])
  end

  def types(:erlang, :bit_size, [_]) do
    sub_safe(r_t_integer(), [r_t_bitstring()])
  end

  def types(:erlang, :byte_size, [_]) do
    sub_safe(r_t_integer(), [r_t_bitstring()])
  end

  def types(:erlang, :hd, [src]) do
    retType = erlang_hd_type(src)
    sub_safe(retType, [r_t_cons()])
  end

  def types(:erlang, :tl, [src]) do
    retType = erlang_tl_type(src)
    sub_safe(retType, [r_t_cons()])
  end

  def types(:erlang, :not, [_]) do
    bool = :beam_types.make_boolean()
    sub_safe(bool, [bool])
  end

  def types(:erlang, :length, [_]) do
    sub_safe(r_t_integer(), [proper_list()])
  end

  def types(:erlang, :and, [_, _]) do
    bool = :beam_types.make_boolean()
    sub_unsafe(bool, [bool, bool])
  end

  def types(:erlang, :or, [_, _]) do
    bool = :beam_types.make_boolean()
    sub_unsafe(bool, [bool, bool])
  end

  def types(:erlang, :xor, [_, _]) do
    bool = :beam_types.make_boolean()
    sub_unsafe(bool, [bool, bool])
  end

  def types(:erlang, :band, [_, _] = args) do
    sub_unsafe(erlang_band_type(args), [r_t_integer(), r_t_integer()])
  end

  def types(:erlang, :bor, [_, _]) do
    sub_unsafe(r_t_integer(), [r_t_integer(), r_t_integer()])
  end

  def types(:erlang, :bxor, [_, _]) do
    sub_unsafe(r_t_integer(), [r_t_integer(), r_t_integer()])
  end

  def types(:erlang, :bsl, [_, _]) do
    sub_unsafe(r_t_integer(), [r_t_integer(), r_t_integer()])
  end

  def types(:erlang, :bsr, [_, _]) do
    sub_unsafe(r_t_integer(), [r_t_integer(), r_t_integer()])
  end

  def types(:erlang, :bnot, [_]) do
    sub_unsafe(r_t_integer(), [r_t_integer()])
  end

  def types(:erlang, :float, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:erlang, :round, [_]) do
    sub_unsafe(r_t_integer(), [:number])
  end

  def types(:erlang, :floor, [_]) do
    sub_unsafe(r_t_integer(), [:number])
  end

  def types(:erlang, :ceil, [_]) do
    sub_unsafe(r_t_integer(), [:number])
  end

  def types(:erlang, :trunc, [_]) do
    sub_unsafe(r_t_integer(), [:number])
  end

  def types(:erlang, :/, [_, _]) do
    sub_unsafe(r_t_float(), [:number, :number])
  end

  def types(:erlang, :div, [_, _]) do
    sub_unsafe(r_t_integer(), [r_t_integer(), r_t_integer()])
  end

  def types(:erlang, :rem, [_, _]) do
    sub_unsafe(r_t_integer(), [r_t_integer(), r_t_integer()])
  end

  def types(:erlang, :abs, [_] = args) do
    mixed_arith_types(args)
  end

  def types(:erlang, :++, [lHS, rHS]) do
    listType = copy_list(lHS, :same_length, :proper)
    retType = :beam_types.join(listType, rHS)
    sub_unsafe(retType, [proper_list(), :any])
  end

  def types(:erlang, :--, [lHS, _]) do
    retType = copy_list(lHS, :new_length, :proper)
    sub_unsafe(retType, [proper_list(), proper_list()])
  end

  def types(:erlang, :iolist_to_binary, [_]) do
    argType = :beam_types.join(r_t_list(), r_t_bitstring(size_unit: 8))
    sub_unsafe(r_t_bitstring(size_unit: 8), [argType])
  end

  def types(:erlang, :list_to_binary, [_]) do
    sub_unsafe(r_t_bitstring(size_unit: 8), [r_t_list()])
  end

  def types(:erlang, :list_to_bitstring, [_]) do
    sub_unsafe(r_t_bitstring(), [proper_list()])
  end

  def types(:erlang, :binary_part, [_, _]) do
    posLen = make_two_tuple(r_t_integer(), r_t_integer())
    binary = r_t_bitstring(size_unit: 8)
    sub_unsafe(binary, [binary, posLen])
  end

  def types(:erlang, :binary_part, [_, _, _]) do
    binary = r_t_bitstring(size_unit: 8)
    sub_unsafe(binary, [binary, r_t_integer(), r_t_integer()])
  end

  def types(:erlang, :is_map_key, [key, map]) do
    retType =
      case erlang_map_get_type(key, map) do
        :none ->
          :beam_types.make_atom(false)

        _ ->
          :beam_types.make_boolean()
      end

    sub_unsafe(retType, [:any, r_t_map()])
  end

  def types(:erlang, :map_get, [key, map]) do
    retType = erlang_map_get_type(key, map)
    sub_unsafe(retType, [:any, r_t_map()])
  end

  def types(:erlang, :node, [_]) do
    sub_unsafe(r_t_atom(), [:any])
  end

  def types(:erlang, :node, []) do
    sub_unsafe(r_t_atom(), [])
  end

  def types(:erlang, :size, [_]) do
    argType = :beam_types.join(r_t_tuple(), r_t_bitstring())
    sub_unsafe(r_t_integer(), [argType])
  end

  def types(:erlang, :element, [posType, tupleType]) do
    index =
      case posType do
        r_t_integer(elements: {same, same}) when is_integer(same) ->
          same

        _ ->
          0
      end

    retType =
      case tupleType do
        r_t_tuple(size: sz, elements: es)
        when index <= sz and
               index >= 1 ->
          :beam_types.get_tuple_element(index, es)

        _ ->
          :any
      end

    sub_unsafe(retType, [r_t_integer(), r_t_tuple(size: index)])
  end

  def types(:erlang, :setelement, [posType, tupleType, argType]) do
    retType =
      case {posType, tupleType} do
        {r_t_integer(elements: {index, index}), r_t_tuple(elements: es0, size: size) = t}
        when index >= 1 ->
          es = :beam_types.set_tuple_element(index, argType, es0)

          case r_t_tuple(t, :exact) do
            false ->
              r_t_tuple(t, size: max(index, size), elements: es)

            true when index <= size ->
              r_t_tuple(t, elements: es)

            true ->
              :none
          end

        {r_t_integer(elements: {min, max}), r_t_tuple(elements: es0, size: size) = t}
        when min >= 1 ->
          es = discard_tuple_element_info(min, max, es0)

          case r_t_tuple(t, :exact) do
            false ->
              r_t_tuple(t, elements: es, size: max(min, size))

            true when min <= size ->
              r_t_tuple(t, elements: es, size: size)

            true ->
              :none
          end

        {_, r_t_tuple() = t} ->
          r_t_tuple(t, elements: %{})

        {r_t_integer(elements: {min, _Max}), _} ->
          r_t_tuple(size: min)

        {_, _} ->
          r_t_tuple()
      end

    sub_unsafe(retType, [r_t_integer(), r_t_tuple(), :any])
  end

  def types(:erlang, :make_fun, [_, _, arity0]) do
    type =
      case arity0 do
        r_t_integer(elements: {arity, arity}) when arity >= 0 ->
          r_t_fun(arity: arity)

        _ ->
          r_t_fun()
      end

    sub_unsafe(type, [r_t_atom(), r_t_atom(), r_t_integer()])
  end

  def types(:erlang, name, args) do
    arity = length(args)

    case :erl_bifs.is_exit_bif(:erlang, name, arity) do
      true ->
        {:none, args, false}

      false ->
        case :erl_internal.arith_op(name, arity) do
          true ->
            mixed_arith_types(args)

          false ->
            isTest =
              :erl_internal.new_type_test(
                name,
                arity
              ) or
                :erl_internal.comp_op(
                  name,
                  arity
                )

            retType =
              case isTest do
                true ->
                  :beam_types.make_boolean()

                false ->
                  :any
              end

            sub_unsafe(retType, duplicate(arity, :any))
        end
    end
  end

  def types(:math, :cos, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :cosh, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :sin, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :sinh, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :tan, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :tanh, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :acos, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :acosh, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :asin, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :asinh, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :atan, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :atanh, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :erf, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :erfc, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :exp, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :log, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :log2, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :log10, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :sqrt, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :atan2, [_, _]) do
    sub_unsafe(r_t_float(), [:number, :number])
  end

  def types(:math, :pow, [_, _]) do
    sub_unsafe(r_t_float(), [:number, :number])
  end

  def types(:math, :ceil, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :floor, [_]) do
    sub_unsafe(r_t_float(), [:number])
  end

  def types(:math, :fmod, [_, _]) do
    sub_unsafe(r_t_float(), [:number, :number])
  end

  def types(:math, :pi, []) do
    sub_unsafe(r_t_float(), [])
  end

  def types(:lists, :append, [_, _] = args) do
    types(:erlang, :++, args)
  end

  def types(:lists, :append, [_]) do
    sub_unsafe(:any, [proper_list()])
  end

  def types(:lists, :subtract, [_, _] = args) do
    types(:erlang, :--, args)
  end

  def types(:lists, :all, [_, _]) do
    sub_unsafe(
      :beam_types.make_boolean(),
      [r_t_fun(arity: 1), r_t_list()]
    )
  end

  def types(:lists, :any, [_, _]) do
    sub_unsafe(
      :beam_types.make_boolean(),
      [r_t_fun(arity: 1), r_t_list()]
    )
  end

  def types(:lists, :keymember, [_, _, _]) do
    sub_unsafe(:beam_types.make_boolean(), [:any, r_t_integer(), r_t_list()])
  end

  def types(:lists, :member, [_, _]) do
    sub_unsafe(:beam_types.make_boolean(), [:any, r_t_list()])
  end

  def types(:lists, :prefix, [_, _]) do
    sub_unsafe(:beam_types.make_boolean(), [r_t_list(), r_t_list()])
  end

  def types(:lists, :suffix, [_, _]) do
    sub_unsafe(:beam_types.make_boolean(), [r_t_list(), r_t_list()])
  end

  def types(:lists, :foldl, [fun, init, list]) do
    retType = lists_fold_type(fun, init, list)
    sub_unsafe(retType, [r_t_fun(arity: 2), :any, proper_list()])
  end

  def types(:lists, :foldr, [fun, init, list]) do
    retType = lists_fold_type(fun, init, list)
    sub_unsafe(retType, [r_t_fun(arity: 2), :any, proper_list()])
  end

  def types(:lists, :droplast, [list]) do
    retType = copy_list(list, :new_length, :proper)
    sub_unsafe(retType, [proper_list()])
  end

  def types(:lists, :dropwhile, [_Fun, list]) do
    retType = copy_list(list, :new_length, :maybe_improper)
    sub_unsafe(retType, [r_t_fun(arity: 1), r_t_list()])
  end

  def types(:lists, :duplicate, [_Count, element]) do
    sub_unsafe(proper_list(element), [r_t_integer(), :any])
  end

  def types(:lists, :filter, [_Fun, list]) do
    retType = copy_list(list, :new_length, :proper)
    sub_unsafe(retType, [r_t_fun(arity: 1), proper_list()])
  end

  def types(:lists, :flatten, [_]) do
    sub_unsafe(proper_list(), [proper_list()])
  end

  def types(:lists, :map, [fun, list]) do
    retType = lists_map_type(fun, list)
    sub_unsafe(retType, [r_t_fun(arity: 1), proper_list()])
  end

  def types(:lists, :reverse, [list]) do
    retType = copy_list(list, :same_length, :proper)
    sub_unsafe(retType, [proper_list()])
  end

  def types(:lists, :sort, [list]) do
    retType = copy_list(list, :same_length, :proper)
    sub_unsafe(retType, [proper_list()])
  end

  def types(:lists, :takewhile, [_Fun, list]) do
    retType = copy_list(list, :new_length, :proper)
    sub_unsafe(retType, [r_t_fun(arity: 1), r_t_list()])
  end

  def types(:lists, :usort, [list]) do
    retType = copy_list(list, :same_length, :proper)
    sub_unsafe(retType, [proper_list()])
  end

  def types(:lists, :zip, [_, _] = lists) do
    {retType, argType} = lists_zip_types(lists)
    sub_unsafe(retType, [argType, argType])
  end

  def types(:lists, :zipwith, [fun | [_, _] = lists]) do
    {retType, argType} = lists_zipwith_types(fun, lists)
    sub_unsafe(retType, [r_t_fun(arity: 2), argType, argType])
  end

  def types(:lists, :keyfind, [keyType, posType, _]) do
    tupleType =
      case posType do
        r_t_integer(elements: {index, index})
        when is_integer(index) and
               index >= 1 ->
          es = :beam_types.set_tuple_element(index, keyType, %{})
          r_t_tuple(size: index, elements: es)

        _ ->
          r_t_tuple()
      end

    retType =
      :beam_types.join(
        tupleType,
        :beam_types.make_atom(false)
      )

    sub_unsafe(retType, [:any, r_t_integer(), r_t_list()])
  end

  def types(:lists, mapFold, [fun, init, list])
      when mapFold === :mapfoldl or mapFold === :mapfoldr do
    retType = lists_mapfold_type(fun, init, list)
    sub_unsafe(retType, [r_t_fun(arity: 2), :any, proper_list()])
  end

  def types(:lists, :partition, [_Fun, list]) do
    listType = copy_list(list, :new_length, :proper)
    retType = make_two_tuple(listType, listType)
    sub_unsafe(retType, [r_t_fun(arity: 1), proper_list()])
  end

  def types(:lists, :search, [_, _]) do
    tupleType =
      make_two_tuple(
        :beam_types.make_atom(:value),
        :any
      )

    retType =
      :beam_types.join(
        tupleType,
        :beam_types.make_atom(false)
      )

    sub_unsafe(retType, [r_t_fun(arity: 1), r_t_list()])
  end

  def types(:lists, :splitwith, [_Fun, list]) do
    left = copy_list(list, :new_length, :proper)
    right = copy_list(list, :new_length, :maybe_improper)

    sub_unsafe(
      make_two_tuple(left, right),
      [r_t_fun(arity: 1), r_t_list()]
    )
  end

  def types(:lists, :unzip, [list]) do
    retType = lists_unzip_type(2, list)
    sub_unsafe(retType, [proper_list()])
  end

  def types(:maps, :filter, [_Fun, map]) do
    retType =
      case map do
        r_t_map() = t ->
          t

        _ ->
          r_t_map()
      end

    sub_unsafe(retType, [r_t_fun(arity: 2), r_t_map()])
  end

  def types(:maps, :find, [key, map]) do
    tupleType =
      case erlang_map_get_type(key, map) do
        :none ->
          :none

        valueType ->
          make_two_tuple(:beam_types.make_atom(:ok), valueType)
      end

    retType =
      :beam_types.join(
        :beam_types.make_atom(:error),
        tupleType
      )

    sub_unsafe(retType, [:any, r_t_map()])
  end

  def types(:maps, :fold, [fun, init, _Map]) do
    retType =
      case fun do
        r_t_fun(type: type) ->
          :beam_types.join(type, init)

        _ ->
          :any
      end

    sub_unsafe(retType, [r_t_fun(arity: 3), :any, r_t_map()])
  end

  def types(:maps, :from_list, [pairs]) do
    pairType = erlang_hd_type(pairs)

    retType =
      case :beam_types.normalize(pairType) do
        r_t_tuple(elements: es) ->
          sKey = :beam_types.get_tuple_element(1, es)
          sValue = :beam_types.get_tuple_element(2, es)
          r_t_map(super_key: sKey, super_value: sValue)

        _ ->
          r_t_map()
      end

    sub_unsafe(retType, [proper_list()])
  end

  def types(:maps, :get, [_Key, _Map] = args) do
    types(:erlang, :map_get, args)
  end

  def types(:maps, :get, [key, map, default]) do
    retType =
      case erlang_map_get_type(key, map) do
        :none ->
          default

        valueType ->
          :beam_types.join(valueType, default)
      end

    sub_unsafe(retType, [:any, r_t_map(), :any])
  end

  def types(:maps, :is_key, [_Key, _Map] = args) do
    types(:erlang, :is_map_key, args)
  end

  def types(:maps, :keys, [map]) do
    retType =
      case map do
        r_t_map(super_key: :none) ->
          nil

        r_t_map(super_key: sKey) ->
          proper_list(sKey)

        _ ->
          proper_list()
      end

    sub_unsafe(retType, [r_t_map()])
  end

  def types(:maps, :map, [fun, map]) do
    retType =
      case {fun, map} do
        {r_t_fun(type: funRet), r_t_map(super_value: sValue0)} ->
          sValue = :beam_types.join(funRet, sValue0)
          r_t_map(map, super_value: sValue)

        _ ->
          r_t_map()
      end

    sub_unsafe(retType, [r_t_fun(arity: 2), r_t_map()])
  end

  def types(:maps, :merge, [a, b]) do
    retType =
      case {a, b} do
        {r_t_map(super_key: sKeyA, super_value: sValueA),
         r_t_map(super_key: sKeyB, super_value: sValueB)} ->
          sKey = :beam_types.join(sKeyA, sKeyB)
          sValue = :beam_types.join(sValueA, sValueB)
          r_t_map(super_key: sKey, super_value: sValue)

        _ ->
          r_t_map()
      end

    sub_unsafe(retType, [r_t_map(), r_t_map()])
  end

  def types(:maps, :new, []) do
    retType = r_t_map(super_key: :none, super_value: :none)
    sub_unsafe(retType, [])
  end

  def types(:maps, :put, [key, value, map]) do
    retType =
      case map do
        r_t_map(super_key: sKey0, super_value: sValue0) ->
          sKey = :beam_types.join(key, sKey0)
          sValue = :beam_types.join(value, sValue0)
          r_t_map(super_key: sKey, super_value: sValue)

        _ ->
          r_t_map()
      end

    sub_unsafe(retType, [:any, :any, r_t_map()])
  end

  def types(:maps, :remove, [key, map]) do
    retType = maps_remove_type(key, map)
    sub_unsafe(retType, [:any, r_t_map()])
  end

  def types(:maps, :take, [key, map]) do
    tupleType =
      case erlang_map_get_type(key, map) do
        :none ->
          :none

        valueType ->
          mapType = :beam_types.meet(map, r_t_map())
          make_two_tuple(valueType, mapType)
      end

    retType =
      :beam_types.join(
        :beam_types.make_atom(:error),
        tupleType
      )

    sub_unsafe(retType, [:any, r_t_map()])
  end

  def types(:maps, :to_list, [map]) do
    retType =
      case map do
        r_t_map(super_key: sKey, super_value: sValue) ->
          proper_list(make_two_tuple(sKey, sValue))

        _ ->
          proper_list()
      end

    sub_unsafe(retType, [r_t_map()])
  end

  def types(:maps, :update_with, [_Key, fun, map]) do
    retType =
      case {fun, map} do
        {r_t_fun(type: funRet), r_t_map(super_value: sValue0)} ->
          sValue = :beam_types.join(funRet, sValue0)
          r_t_map(map, super_value: sValue)

        _ ->
          r_t_map()
      end

    sub_unsafe(retType, [:any, r_t_fun(arity: 1), r_t_map()])
  end

  def types(:maps, :values, [map]) do
    retType =
      case map do
        r_t_map(super_value: :none) ->
          nil

        r_t_map(super_value: sValue) ->
          proper_list(sValue)

        _ ->
          proper_list()
      end

    sub_unsafe(retType, [r_t_map()])
  end

  def types(:maps, :with, [keys, map]) do
    retType =
      case map do
        r_t_map(super_key: sKey0) ->
          sKey = :beam_types.meet(erlang_hd_type(keys), sKey0)
          r_t_map(map, super_key: sKey)

        _ ->
          r_t_map()
      end

    sub_unsafe(retType, [proper_list(), r_t_map()])
  end

  def types(:maps, :without, [keys, map]) do
    retType = maps_remove_type(erlang_hd_type(keys), map)
    sub_unsafe(retType, [proper_list(), r_t_map()])
  end

  def types(_, _, args) do
    sub_unsafe(
      :any,
      for _ <- args do
        :any
      end
    )
  end

  defp mixed_arith_types([firstType | _] = args0) do
    retType =
      foldl(
        fn
          r_t_integer(), r_t_integer() ->
            r_t_integer()

          r_t_integer(), :number ->
            :number

          r_t_integer(), r_t_float() ->
            r_t_float()

          r_t_float(), r_t_integer() ->
            r_t_float()

          r_t_float(), :number ->
            r_t_float()

          r_t_float(), r_t_float() ->
            r_t_float()

          :number, r_t_integer() ->
            :number

          :number, r_t_float() ->
            r_t_float()

          :number, :number ->
            :number

          :any, _ ->
            :number

          _, _ ->
            :none
        end,
        firstType,
        args0
      )

    sub_unsafe(
      retType,
      for _ <- args0 do
        :number
      end
    )
  end

  defp erlang_hd_type(src) do
    case :beam_types.meet(src, r_t_cons()) do
      r_t_cons(type: type) ->
        type

      _ ->
        :any
    end
  end

  defp erlang_tl_type(src) do
    case :beam_types.meet(src, r_t_cons()) do
      r_t_cons(terminator: term) = cons ->
        :beam_types.join(cons, term)

      _ ->
        :any
    end
  end

  defp erlang_band_type([r_t_integer(elements: {int, int}), rHS])
       when is_integer(int) do
    erlang_band_type_1(rHS, int)
  end

  defp erlang_band_type([lHS, r_t_integer(elements: {int, int})])
       when is_integer(int) do
    erlang_band_type_1(lHS, int)
  end

  defp erlang_band_type(_) do
    r_t_integer()
  end

  defp erlang_band_type_1(lHS, int) do
    case lHS do
      r_t_integer(elements: {min0, max0}) when max0 - min0 < 1 <<< 256 ->
        {intersection, union} = range_masks(min0, max0)
        min = intersection &&& int
        max = min(max0, union &&& int)
        r_t_integer(elements: {min, max})

      r_t_integer() when int >= 0 ->
        :beam_types.meet(lHS, r_t_integer(elements: {0, int}))

      _ ->
        :beam_types.meet(lHS, r_t_integer())
    end
  end

  defp erlang_map_get_type(key, map) do
    case map do
      r_t_map(super_key: sKey, super_value: sValue) ->
        case :beam_types.meet(sKey, key) do
          :none ->
            :none

          _ ->
            sValue
        end

      _ ->
        :any
    end
  end

  defp lists_fold_type(_Fun, init, nil) do
    init
  end

  defp lists_fold_type(r_t_fun(type: type), _Init, r_t_cons()) do
    type
  end

  defp lists_fold_type(r_t_fun(type: type), init, r_t_list()) do
    :beam_types.join(type, init)
  end

  defp lists_fold_type(_Fun, _Init, _List) do
    :any
  end

  defp lists_map_type(r_t_fun(type: type), types) do
    lists_map_type_1(types, type)
  end

  defp lists_map_type(_Fun, types) do
    lists_map_type_1(types, :any)
  end

  defp lists_map_type_1(nil, _ElementType) do
    nil
  end

  defp lists_map_type_1(r_t_cons(), :none) do
    :none
  end

  defp lists_map_type_1(r_t_cons(), elementType) do
    proper_cons(elementType)
  end

  defp lists_map_type_1(_, :none) do
    nil
  end

  defp lists_map_type_1(_, elementType) do
    proper_list(elementType)
  end

  defp lists_mapfold_type(r_t_fun(type: r_t_tuple(size: 2, elements: es)), init, list) do
    elementType = :beam_types.get_tuple_element(1, es)
    accType = :beam_types.get_tuple_element(2, es)
    lists_mapfold_type_1(list, elementType, init, accType)
  end

  defp lists_mapfold_type(r_t_fun(type: :none), _Init, r_t_cons()) do
    :none
  end

  defp lists_mapfold_type(r_t_fun(type: :none), init, _List) do
    make_two_tuple(nil, init)
  end

  defp lists_mapfold_type(_Fun, init, list) do
    lists_mapfold_type_1(list, :any, init, :any)
  end

  defp lists_mapfold_type_1(nil, _ElementType, init, _AccType) do
    make_two_tuple(nil, init)
  end

  defp lists_mapfold_type_1(r_t_cons(), elementType, _Init, accType) do
    make_two_tuple(proper_cons(elementType), accType)
  end

  defp lists_mapfold_type_1(_, elementType, init, accType0) do
    accType = :beam_types.join(accType0, init)
    make_two_tuple(proper_list(elementType), accType)
  end

  defp lists_unzip_type(size, list) do
    es = lut_make_elements(lut_list_types(size, list), 1, %{})
    r_t_tuple(size: size, exact: true, elements: es)
  end

  defp lut_make_elements([type | types], index, es0) do
    es = :beam_types.set_tuple_element(index, type, es0)
    lut_make_elements(types, index + 1, es)
  end

  defp lut_make_elements([], _Index, es) do
    es
  end

  defp lut_list_types(size, r_t_cons(type: r_t_tuple(size: size, elements: es))) do
    types = lut_element_types(1, size, es)

    for t <- types do
      proper_cons(t)
    end
  end

  defp lut_list_types(size, r_t_list(type: r_t_tuple(size: size, elements: es))) do
    types = lut_element_types(1, size, es)

    for t <- types do
      proper_list(t)
    end
  end

  defp lut_list_types(size, nil) do
    :lists.duplicate(size, nil)
  end

  defp lut_list_types(size, _) do
    :lists.duplicate(size, proper_list())
  end

  defp lut_element_types(index, max, %{}) when index > max do
    []
  end

  defp lut_element_types(index, max, es) do
    elementType = :beam_types.get_tuple_element(index, es)
    [elementType | lut_element_types(index + 1, max, es)]
  end

  defp lists_zip_types(types) do
    lists_zip_types_1(types, false, %{}, 1)
  end

  defp lists_zip_types_1([nil | _], _AnyCons, _Es, _N) do
    {nil, nil}
  end

  defp lists_zip_types_1([r_t_cons(type: type, terminator: nil) | lists], _AnyCons, es0, n) do
    es = :beam_types.set_tuple_element(n, type, es0)
    lists_zip_types_1(lists, true, es, n + 1)
  end

  defp lists_zip_types_1([r_t_list(type: type, terminator: nil) | lists], anyCons, es0, n) do
    es = :beam_types.set_tuple_element(n, type, es0)
    lists_zip_types_1(lists, anyCons, es, n + 1)
  end

  defp lists_zip_types_1([_ | lists], anyCons, es, n) do
    lists_zip_types_1(lists, anyCons, es, n + 1)
  end

  defp lists_zip_types_1([], true, es, n) do
    elementType = r_t_tuple(exact: true, size: n - 1, elements: es)
    retType = proper_cons(elementType)
    argType = proper_cons()
    {retType, argType}
  end

  defp lists_zip_types_1([], false, es, n) do
    elementType = r_t_tuple(exact: true, size: n - 1, elements: es)
    retType = proper_list(elementType)
    argType = proper_list()
    {retType, argType}
  end

  defp lists_zipwith_types(r_t_fun(type: type), types) do
    lists_zipwith_type_1(types, type)
  end

  defp lists_zipwith_types(_Fun, types) do
    lists_zipwith_type_1(types, :any)
  end

  defp lists_zipwith_type_1([nil | _], _ElementType) do
    {nil, nil}
  end

  defp lists_zipwith_type_1([r_t_cons() | _Lists], :none) do
    {:none, :any}
  end

  defp lists_zipwith_type_1([r_t_cons() | _Lists], elementType) do
    retType = proper_cons(elementType)
    argType = proper_cons()
    {retType, argType}
  end

  defp lists_zipwith_type_1([_ | lists], elementType) do
    lists_zipwith_type_1(lists, elementType)
  end

  defp lists_zipwith_type_1([], :none) do
    {nil, nil}
  end

  defp lists_zipwith_type_1([], elementType) do
    retType = proper_list(elementType)
    argType = proper_list()
    {retType, argType}
  end

  defp maps_remove_type(key, r_t_map(super_key: sKey0) = map) do
    case :beam_types.is_singleton_type(key) do
      true ->
        sKey = :beam_types.subtract(sKey0, key)
        r_t_map(map, super_key: sKey)

      false ->
        map
    end
  end

  defp maps_remove_type(_Key, _Map) do
    r_t_map()
  end

  defp sub_unsafe(retType, argTypes) do
    {retType, argTypes, false}
  end

  defp sub_safe(retType, argTypes) do
    {retType, argTypes, true}
  end

  defp discard_tuple_element_info(min, max, es) do
    foldl(
      fn
        el, acc when min <= el and el <= max ->
          :maps.remove(el, acc)

        _El, acc ->
          acc
      end,
      es,
      :maps.keys(es)
    )
  end

  defp range_masks(from, to) when from <= to do
    range_masks_1(from, to, 0, -1, 0)
  end

  defp range_masks_1(from, to, bitPos, intersection, union)
       when from < to do
    range_masks_1(from + (1 <<< bitPos), to, bitPos + 1, intersection &&& from, union ||| from)
  end

  defp range_masks_1(_From, to, _BitPos, intersection0, union0) do
    intersection = to &&& intersection0
    union = to ||| union0
    {intersection, union}
  end

  defp proper_cons() do
    r_t_cons(terminator: nil)
  end

  defp proper_cons(elementType) do
    r_t_cons(type: elementType, terminator: nil)
  end

  defp proper_list() do
    r_t_list(terminator: nil)
  end

  defp proper_list(elementType) do
    r_t_list(type: elementType, terminator: nil)
  end

  defp copy_list(r_t_cons(terminator: term) = t, length, :maybe_improper) do
    copy_list_1(t, length, term)
  end

  defp copy_list(r_t_list(terminator: term) = t, length, :maybe_improper) do
    copy_list_1(t, length, term)
  end

  defp copy_list(t, length, :proper) do
    copy_list_1(t, length, nil)
  end

  defp copy_list(t, length, _Proper) do
    copy_list_1(t, length, :any)
  end

  defp copy_list_1(r_t_cons() = t, :same_length, terminator) do
    r_t_cons(t, terminator: terminator)
  end

  defp copy_list_1(r_t_cons(type: type), :new_length, terminator) do
    r_t_list(type: type, terminator: terminator)
  end

  defp copy_list_1(r_t_list() = t, _Length, terminator) do
    r_t_list(t, terminator: terminator)
  end

  defp copy_list_1(nil, _Length, _Terminator) do
    nil
  end

  defp copy_list_1(_, _Length, terminator) do
    r_t_list(terminator: terminator)
  end

  defp make_two_tuple(type1, type2) do
    es0 = :beam_types.set_tuple_element(1, type1, %{})
    es = :beam_types.set_tuple_element(2, type2, es0)
    r_t_tuple(size: 2, exact: true, elements: es)
  end
end
