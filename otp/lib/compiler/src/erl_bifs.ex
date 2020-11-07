defmodule :m_erl_bifs do
  use Bitwise

  def is_pure(:erlang, :*, 2) do
    true
  end

  def is_pure(:erlang, :+, 1) do
    true
  end

  def is_pure(:erlang, :+, 2) do
    true
  end

  def is_pure(:erlang, :++, 2) do
    true
  end

  def is_pure(:erlang, :-, 1) do
    true
  end

  def is_pure(:erlang, :-, 2) do
    true
  end

  def is_pure(:erlang, :--, 2) do
    true
  end

  def is_pure(:erlang, :/, 2) do
    true
  end

  def is_pure(:erlang, :"/=", 2) do
    true
  end

  def is_pure(:erlang, :<, 2) do
    true
  end

  def is_pure(:erlang, :"=/=", 2) do
    true
  end

  def is_pure(:erlang, :"=:=", 2) do
    true
  end

  def is_pure(:erlang, :"=<", 2) do
    true
  end

  def is_pure(:erlang, :==, 2) do
    true
  end

  def is_pure(:erlang, :>, 2) do
    true
  end

  def is_pure(:erlang, :>=, 2) do
    true
  end

  def is_pure(:erlang, :and, 2) do
    true
  end

  def is_pure(:erlang, :band, 2) do
    true
  end

  def is_pure(:erlang, :bnot, 1) do
    true
  end

  def is_pure(:erlang, :bor, 2) do
    true
  end

  def is_pure(:erlang, :bsl, 2) do
    true
  end

  def is_pure(:erlang, :bsr, 2) do
    true
  end

  def is_pure(:erlang, :bxor, 2) do
    true
  end

  def is_pure(:erlang, :div, 2) do
    true
  end

  def is_pure(:erlang, :not, 1) do
    true
  end

  def is_pure(:erlang, :or, 2) do
    true
  end

  def is_pure(:erlang, :rem, 2) do
    true
  end

  def is_pure(:erlang, :xor, 2) do
    true
  end

  def is_pure(:erlang, :abs, 1) do
    true
  end

  def is_pure(:erlang, :atom_to_binary, 1) do
    true
  end

  def is_pure(:erlang, :atom_to_binary, 2) do
    true
  end

  def is_pure(:erlang, :atom_to_list, 1) do
    true
  end

  def is_pure(:erlang, :binary_part, 2) do
    true
  end

  def is_pure(:erlang, :binary_part, 3) do
    true
  end

  def is_pure(:erlang, :binary_to_atom, 1) do
    true
  end

  def is_pure(:erlang, :binary_to_atom, 2) do
    true
  end

  def is_pure(:erlang, :binary_to_float, 1) do
    true
  end

  def is_pure(:erlang, :binary_to_integer, 1) do
    true
  end

  def is_pure(:erlang, :binary_to_list, 1) do
    true
  end

  def is_pure(:erlang, :binary_to_list, 3) do
    true
  end

  def is_pure(:erlang, :bit_size, 1) do
    true
  end

  def is_pure(:erlang, :byte_size, 1) do
    true
  end

  def is_pure(:erlang, :ceil, 1) do
    true
  end

  def is_pure(:erlang, :element, 2) do
    true
  end

  def is_pure(:erlang, :float, 1) do
    true
  end

  def is_pure(:erlang, :float_to_list, 1) do
    true
  end

  def is_pure(:erlang, :float_to_binary, 1) do
    true
  end

  def is_pure(:erlang, :floor, 1) do
    true
  end

  def is_pure(:erlang, :hd, 1) do
    true
  end

  def is_pure(:erlang, :integer_to_binary, 1) do
    true
  end

  def is_pure(:erlang, :integer_to_list, 1) do
    true
  end

  def is_pure(:erlang, :is_atom, 1) do
    true
  end

  def is_pure(:erlang, :is_boolean, 1) do
    true
  end

  def is_pure(:erlang, :is_binary, 1) do
    true
  end

  def is_pure(:erlang, :is_bitstring, 1) do
    true
  end

  def is_pure(:erlang, :is_float, 1) do
    true
  end

  def is_pure(:erlang, :is_function, 1) do
    true
  end

  def is_pure(:erlang, :is_function, 2) do
    true
  end

  def is_pure(:erlang, :is_integer, 1) do
    true
  end

  def is_pure(:erlang, :is_list, 1) do
    true
  end

  def is_pure(:erlang, :is_map, 1) do
    true
  end

  def is_pure(:erlang, :is_map_key, 2) do
    true
  end

  def is_pure(:erlang, :is_number, 1) do
    true
  end

  def is_pure(:erlang, :is_pid, 1) do
    true
  end

  def is_pure(:erlang, :is_port, 1) do
    true
  end

  def is_pure(:erlang, :is_record, 2) do
    true
  end

  def is_pure(:erlang, :is_record, 3) do
    true
  end

  def is_pure(:erlang, :is_reference, 1) do
    true
  end

  def is_pure(:erlang, :is_tuple, 1) do
    true
  end

  def is_pure(:erlang, :length, 1) do
    true
  end

  def is_pure(:erlang, :list_to_atom, 1) do
    true
  end

  def is_pure(:erlang, :list_to_binary, 1) do
    true
  end

  def is_pure(:erlang, :list_to_float, 1) do
    true
  end

  def is_pure(:erlang, :list_to_integer, 1) do
    true
  end

  def is_pure(:erlang, :list_to_integer, 2) do
    true
  end

  def is_pure(:erlang, :list_to_pid, 1) do
    true
  end

  def is_pure(:erlang, :list_to_tuple, 1) do
    true
  end

  def is_pure(:erlang, :max, 2) do
    true
  end

  def is_pure(:erlang, :make_fun, 3) do
    true
  end

  def is_pure(:erlang, :map_get, 2) do
    true
  end

  def is_pure(:erlang, :min, 2) do
    true
  end

  def is_pure(:erlang, :phash, 2) do
    false
  end

  def is_pure(:erlang, :pid_to_list, 1) do
    true
  end

  def is_pure(:erlang, :round, 1) do
    true
  end

  def is_pure(:erlang, :setelement, 3) do
    true
  end

  def is_pure(:erlang, :size, 1) do
    true
  end

  def is_pure(:erlang, :split_binary, 2) do
    true
  end

  def is_pure(:erlang, :term_to_binary, 1) do
    true
  end

  def is_pure(:erlang, :tl, 1) do
    true
  end

  def is_pure(:erlang, :trunc, 1) do
    true
  end

  def is_pure(:erlang, :tuple_size, 1) do
    true
  end

  def is_pure(:erlang, :tuple_to_list, 1) do
    true
  end

  def is_pure(:lists, :append, 2) do
    true
  end

  def is_pure(:lists, :subtract, 2) do
    true
  end

  def is_pure(:maps, :get, 2) do
    true
  end

  def is_pure(:maps, :is_key, 2) do
    true
  end

  def is_pure(:maps, :new, 0) do
    true
  end

  def is_pure(:math, :acos, 1) do
    true
  end

  def is_pure(:math, :acosh, 1) do
    true
  end

  def is_pure(:math, :asin, 1) do
    true
  end

  def is_pure(:math, :asinh, 1) do
    true
  end

  def is_pure(:math, :atan, 1) do
    true
  end

  def is_pure(:math, :atan2, 2) do
    true
  end

  def is_pure(:math, :atanh, 1) do
    true
  end

  def is_pure(:math, :ceil, 1) do
    true
  end

  def is_pure(:math, :cos, 1) do
    true
  end

  def is_pure(:math, :cosh, 1) do
    true
  end

  def is_pure(:math, :erf, 1) do
    true
  end

  def is_pure(:math, :erfc, 1) do
    true
  end

  def is_pure(:math, :exp, 1) do
    true
  end

  def is_pure(:math, :floor, 1) do
    true
  end

  def is_pure(:math, :fmod, 2) do
    true
  end

  def is_pure(:math, :log, 1) do
    true
  end

  def is_pure(:math, :log2, 1) do
    true
  end

  def is_pure(:math, :log10, 1) do
    true
  end

  def is_pure(:math, :pow, 2) do
    true
  end

  def is_pure(:math, :sin, 1) do
    true
  end

  def is_pure(:math, :sinh, 1) do
    true
  end

  def is_pure(:math, :sqrt, 1) do
    true
  end

  def is_pure(:math, :tan, 1) do
    true
  end

  def is_pure(:math, :tanh, 1) do
    true
  end

  def is_pure(:math, :pi, 0) do
    true
  end

  def is_pure(_, _, _) do
    false
  end

  def is_safe(:erlang, :"/=", 2) do
    true
  end

  def is_safe(:erlang, :<, 2) do
    true
  end

  def is_safe(:erlang, :"=/=", 2) do
    true
  end

  def is_safe(:erlang, :"=:=", 2) do
    true
  end

  def is_safe(:erlang, :"=<", 2) do
    true
  end

  def is_safe(:erlang, :==, 2) do
    true
  end

  def is_safe(:erlang, :>, 2) do
    true
  end

  def is_safe(:erlang, :>=, 2) do
    true
  end

  def is_safe(:erlang, :date, 0) do
    true
  end

  def is_safe(:erlang, :get, 0) do
    true
  end

  def is_safe(:erlang, :get, 1) do
    true
  end

  def is_safe(:erlang, :get_cookie, 0) do
    true
  end

  def is_safe(:erlang, :get_keys, 1) do
    true
  end

  def is_safe(:erlang, :group_leader, 0) do
    true
  end

  def is_safe(:erlang, :is_alive, 0) do
    true
  end

  def is_safe(:erlang, :is_atom, 1) do
    true
  end

  def is_safe(:erlang, :is_boolean, 1) do
    true
  end

  def is_safe(:erlang, :is_binary, 1) do
    true
  end

  def is_safe(:erlang, :is_bitstring, 1) do
    true
  end

  def is_safe(:erlang, :is_float, 1) do
    true
  end

  def is_safe(:erlang, :is_function, 1) do
    true
  end

  def is_safe(:erlang, :is_integer, 1) do
    true
  end

  def is_safe(:erlang, :is_list, 1) do
    true
  end

  def is_safe(:erlang, :is_map, 1) do
    true
  end

  def is_safe(:erlang, :is_number, 1) do
    true
  end

  def is_safe(:erlang, :is_pid, 1) do
    true
  end

  def is_safe(:erlang, :is_port, 1) do
    true
  end

  def is_safe(:erlang, :is_reference, 1) do
    true
  end

  def is_safe(:erlang, :is_tuple, 1) do
    true
  end

  def is_safe(:erlang, :make_ref, 0) do
    true
  end

  def is_safe(:erlang, :max, 2) do
    true
  end

  def is_safe(:erlang, :min, 2) do
    true
  end

  def is_safe(:erlang, :node, 0) do
    true
  end

  def is_safe(:erlang, :nodes, 0) do
    true
  end

  def is_safe(:erlang, :ports, 0) do
    true
  end

  def is_safe(:erlang, :pre_loaded, 0) do
    true
  end

  def is_safe(:erlang, :processes, 0) do
    true
  end

  def is_safe(:erlang, :registered, 0) do
    true
  end

  def is_safe(:erlang, :self, 0) do
    true
  end

  def is_safe(:erlang, :term_to_binary, 1) do
    true
  end

  def is_safe(:erlang, :time, 0) do
    true
  end

  def is_safe(_, _, _) do
    false
  end

  def is_exit_bif(:erlang, :exit, 1) do
    true
  end

  def is_exit_bif(:erlang, :throw, 1) do
    true
  end

  def is_exit_bif(:erlang, :error, 1) do
    true
  end

  def is_exit_bif(:erlang, :error, 2) do
    true
  end

  def is_exit_bif(_, _, _) do
    false
  end
end
