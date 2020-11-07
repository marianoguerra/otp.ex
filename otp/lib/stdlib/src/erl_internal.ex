defmodule :m_erl_internal do
  use Bitwise

  def guard_bif(:abs, 1) do
    true
  end

  def guard_bif(:binary_part, 2) do
    true
  end

  def guard_bif(:binary_part, 3) do
    true
  end

  def guard_bif(:bit_size, 1) do
    true
  end

  def guard_bif(:byte_size, 1) do
    true
  end

  def guard_bif(:ceil, 1) do
    true
  end

  def guard_bif(:element, 2) do
    true
  end

  def guard_bif(:float, 1) do
    true
  end

  def guard_bif(:floor, 1) do
    true
  end

  def guard_bif(:hd, 1) do
    true
  end

  def guard_bif(:is_map_key, 2) do
    true
  end

  def guard_bif(:length, 1) do
    true
  end

  def guard_bif(:map_size, 1) do
    true
  end

  def guard_bif(:map_get, 2) do
    true
  end

  def guard_bif(:node, 0) do
    true
  end

  def guard_bif(:node, 1) do
    true
  end

  def guard_bif(:round, 1) do
    true
  end

  def guard_bif(:self, 0) do
    true
  end

  def guard_bif(:size, 1) do
    true
  end

  def guard_bif(:tl, 1) do
    true
  end

  def guard_bif(:trunc, 1) do
    true
  end

  def guard_bif(:tuple_size, 1) do
    true
  end

  def guard_bif(name, a) do
    new_type_test(name, a)
  end

  def type_test(name, arity) do
    new_type_test(name, arity) or old_type_test(name, arity)
  end

  def new_type_test(:is_atom, 1) do
    true
  end

  def new_type_test(:is_binary, 1) do
    true
  end

  def new_type_test(:is_bitstring, 1) do
    true
  end

  def new_type_test(:is_boolean, 1) do
    true
  end

  def new_type_test(:is_float, 1) do
    true
  end

  def new_type_test(:is_function, 1) do
    true
  end

  def new_type_test(:is_function, 2) do
    true
  end

  def new_type_test(:is_integer, 1) do
    true
  end

  def new_type_test(:is_list, 1) do
    true
  end

  def new_type_test(:is_map, 1) do
    true
  end

  def new_type_test(:is_number, 1) do
    true
  end

  def new_type_test(:is_pid, 1) do
    true
  end

  def new_type_test(:is_port, 1) do
    true
  end

  def new_type_test(:is_record, 2) do
    true
  end

  def new_type_test(:is_record, 3) do
    true
  end

  def new_type_test(:is_reference, 1) do
    true
  end

  def new_type_test(:is_tuple, 1) do
    true
  end

  def new_type_test(name, a)
      when is_atom(name) and
             is_integer(a) do
    false
  end

  def old_type_test(:integer, 1) do
    true
  end

  def old_type_test(:float, 1) do
    true
  end

  def old_type_test(:number, 1) do
    true
  end

  def old_type_test(:atom, 1) do
    true
  end

  def old_type_test(:list, 1) do
    true
  end

  def old_type_test(:tuple, 1) do
    true
  end

  def old_type_test(:pid, 1) do
    true
  end

  def old_type_test(:reference, 1) do
    true
  end

  def old_type_test(:port, 1) do
    true
  end

  def old_type_test(:binary, 1) do
    true
  end

  def old_type_test(:record, 2) do
    true
  end

  def old_type_test(:function, 1) do
    true
  end

  def old_type_test(name, a)
      when is_atom(name) and
             is_integer(a) do
    false
  end

  def arith_op(:+, 1) do
    true
  end

  def arith_op(:-, 1) do
    true
  end

  def arith_op(:*, 2) do
    true
  end

  def arith_op(:/, 2) do
    true
  end

  def arith_op(:+, 2) do
    true
  end

  def arith_op(:-, 2) do
    true
  end

  def arith_op(:bnot, 1) do
    true
  end

  def arith_op(:div, 2) do
    true
  end

  def arith_op(:rem, 2) do
    true
  end

  def arith_op(:band, 2) do
    true
  end

  def arith_op(:bor, 2) do
    true
  end

  def arith_op(:bxor, 2) do
    true
  end

  def arith_op(:bsl, 2) do
    true
  end

  def arith_op(:bsr, 2) do
    true
  end

  def arith_op(op, a) when is_atom(op) and is_integer(a) do
    false
  end

  def bool_op(:not, 1) do
    true
  end

  def bool_op(:and, 2) do
    true
  end

  def bool_op(:or, 2) do
    true
  end

  def bool_op(:xor, 2) do
    true
  end

  def bool_op(op, a) when is_atom(op) and is_integer(a) do
    false
  end

  def comp_op(:==, 2) do
    true
  end

  def comp_op(:"/=", 2) do
    true
  end

  def comp_op(:"=<", 2) do
    true
  end

  def comp_op(:<, 2) do
    true
  end

  def comp_op(:>=, 2) do
    true
  end

  def comp_op(:>, 2) do
    true
  end

  def comp_op(:"=:=", 2) do
    true
  end

  def comp_op(:"=/=", 2) do
    true
  end

  def comp_op(op, a) when is_atom(op) and is_integer(a) do
    false
  end

  def list_op(:++, 2) do
    true
  end

  def list_op(:--, 2) do
    true
  end

  def list_op(op, a) when is_atom(op) and is_integer(a) do
    false
  end

  def send_op(:!, 2) do
    true
  end

  def send_op(op, a) when is_atom(op) and is_integer(a) do
    false
  end

  def op_type(:+, 1) do
    :arith
  end

  def op_type(:-, 1) do
    :arith
  end

  def op_type(:*, 2) do
    :arith
  end

  def op_type(:/, 2) do
    :arith
  end

  def op_type(:+, 2) do
    :arith
  end

  def op_type(:-, 2) do
    :arith
  end

  def op_type(:bnot, 1) do
    :arith
  end

  def op_type(:div, 2) do
    :arith
  end

  def op_type(:rem, 2) do
    :arith
  end

  def op_type(:band, 2) do
    :arith
  end

  def op_type(:bor, 2) do
    :arith
  end

  def op_type(:bxor, 2) do
    :arith
  end

  def op_type(:bsl, 2) do
    :arith
  end

  def op_type(:bsr, 2) do
    :arith
  end

  def op_type(:not, 1) do
    :bool
  end

  def op_type(:and, 2) do
    :bool
  end

  def op_type(:or, 2) do
    :bool
  end

  def op_type(:xor, 2) do
    :bool
  end

  def op_type(:==, 2) do
    :comp
  end

  def op_type(:"/=", 2) do
    :comp
  end

  def op_type(:"=<", 2) do
    :comp
  end

  def op_type(:<, 2) do
    :comp
  end

  def op_type(:>=, 2) do
    :comp
  end

  def op_type(:>, 2) do
    :comp
  end

  def op_type(:"=:=", 2) do
    :comp
  end

  def op_type(:"=/=", 2) do
    :comp
  end

  def op_type(:++, 2) do
    :list
  end

  def op_type(:--, 2) do
    :list
  end

  def op_type(:!, 2) do
    :send
  end

  def bif(:erlang, name, arity) do
    bif(name, arity)
  end

  def bif(m, f, a)
      when is_atom(m) and is_atom(f) and
             is_integer(a) do
    false
  end

  def bif(:abs, 1) do
    true
  end

  def bif(:apply, 2) do
    true
  end

  def bif(:apply, 3) do
    true
  end

  def bif(:atom_to_binary, 1) do
    true
  end

  def bif(:atom_to_binary, 2) do
    true
  end

  def bif(:atom_to_list, 1) do
    true
  end

  def bif(:binary_part, 2) do
    true
  end

  def bif(:binary_part, 3) do
    true
  end

  def bif(:binary_to_atom, 1) do
    true
  end

  def bif(:binary_to_atom, 2) do
    true
  end

  def bif(:binary_to_existing_atom, 1) do
    true
  end

  def bif(:binary_to_existing_atom, 2) do
    true
  end

  def bif(:binary_to_integer, 1) do
    true
  end

  def bif(:binary_to_integer, 2) do
    true
  end

  def bif(:binary_to_float, 1) do
    true
  end

  def bif(:binary_to_list, 1) do
    true
  end

  def bif(:binary_to_list, 3) do
    true
  end

  def bif(:binary_to_term, 1) do
    true
  end

  def bif(:binary_to_term, 2) do
    true
  end

  def bif(:bitsize, 1) do
    true
  end

  def bif(:bit_size, 1) do
    true
  end

  def bif(:bitstring_to_list, 1) do
    true
  end

  def bif(:byte_size, 1) do
    true
  end

  def bif(:ceil, 1) do
    true
  end

  def bif(:check_old_code, 1) do
    true
  end

  def bif(:check_process_code, 2) do
    true
  end

  def bif(:check_process_code, 3) do
    true
  end

  def bif(:date, 0) do
    true
  end

  def bif(:delete_module, 1) do
    true
  end

  def bif(:demonitor, 1) do
    true
  end

  def bif(:demonitor, 2) do
    true
  end

  def bif(:disconnect_node, 1) do
    true
  end

  def bif(:element, 2) do
    true
  end

  def bif(:erase, 0) do
    true
  end

  def bif(:erase, 1) do
    true
  end

  def bif(:error, 1) do
    true
  end

  def bif(:error, 2) do
    true
  end

  def bif(:exit, 1) do
    true
  end

  def bif(:exit, 2) do
    true
  end

  def bif(:float, 1) do
    true
  end

  def bif(:float_to_list, 1) do
    true
  end

  def bif(:float_to_list, 2) do
    true
  end

  def bif(:float_to_binary, 1) do
    true
  end

  def bif(:float_to_binary, 2) do
    true
  end

  def bif(:floor, 1) do
    true
  end

  def bif(:garbage_collect, 0) do
    true
  end

  def bif(:garbage_collect, 1) do
    true
  end

  def bif(:garbage_collect, 2) do
    true
  end

  def bif(:get, 0) do
    true
  end

  def bif(:get, 1) do
    true
  end

  def bif(:get_keys, 0) do
    true
  end

  def bif(:get_keys, 1) do
    true
  end

  def bif(:group_leader, 0) do
    true
  end

  def bif(:group_leader, 2) do
    true
  end

  def bif(:halt, 0) do
    true
  end

  def bif(:halt, 1) do
    true
  end

  def bif(:halt, 2) do
    true
  end

  def bif(:hd, 1) do
    true
  end

  def bif(:integer_to_binary, 1) do
    true
  end

  def bif(:integer_to_binary, 2) do
    true
  end

  def bif(:integer_to_list, 1) do
    true
  end

  def bif(:integer_to_list, 2) do
    true
  end

  def bif(:iolist_size, 1) do
    true
  end

  def bif(:iolist_to_binary, 1) do
    true
  end

  def bif(:is_alive, 0) do
    true
  end

  def bif(:is_process_alive, 1) do
    true
  end

  def bif(:is_atom, 1) do
    true
  end

  def bif(:is_boolean, 1) do
    true
  end

  def bif(:is_binary, 1) do
    true
  end

  def bif(:is_bitstring, 1) do
    true
  end

  def bif(:is_float, 1) do
    true
  end

  def bif(:is_function, 1) do
    true
  end

  def bif(:is_function, 2) do
    true
  end

  def bif(:is_integer, 1) do
    true
  end

  def bif(:is_list, 1) do
    true
  end

  def bif(:is_map, 1) do
    true
  end

  def bif(:is_map_key, 2) do
    true
  end

  def bif(:is_number, 1) do
    true
  end

  def bif(:is_pid, 1) do
    true
  end

  def bif(:is_port, 1) do
    true
  end

  def bif(:is_reference, 1) do
    true
  end

  def bif(:is_tuple, 1) do
    true
  end

  def bif(:is_record, 2) do
    true
  end

  def bif(:is_record, 3) do
    true
  end

  def bif(:length, 1) do
    true
  end

  def bif(:link, 1) do
    true
  end

  def bif(:list_to_atom, 1) do
    true
  end

  def bif(:list_to_binary, 1) do
    true
  end

  def bif(:list_to_bitstring, 1) do
    true
  end

  def bif(:list_to_existing_atom, 1) do
    true
  end

  def bif(:list_to_float, 1) do
    true
  end

  def bif(:list_to_integer, 1) do
    true
  end

  def bif(:list_to_integer, 2) do
    true
  end

  def bif(:list_to_pid, 1) do
    true
  end

  def bif(:list_to_port, 1) do
    true
  end

  def bif(:list_to_ref, 1) do
    true
  end

  def bif(:list_to_tuple, 1) do
    true
  end

  def bif(:load_module, 2) do
    true
  end

  def bif(:make_ref, 0) do
    true
  end

  def bif(:map_size, 1) do
    true
  end

  def bif(:map_get, 2) do
    true
  end

  def bif(:max, 2) do
    true
  end

  def bif(:min, 2) do
    true
  end

  def bif(:module_loaded, 1) do
    true
  end

  def bif(:monitor, 2) do
    true
  end

  def bif(:monitor_node, 2) do
    true
  end

  def bif(:node, 0) do
    true
  end

  def bif(:node, 1) do
    true
  end

  def bif(:nodes, 0) do
    true
  end

  def bif(:nodes, 1) do
    true
  end

  def bif(:now, 0) do
    true
  end

  def bif(:open_port, 2) do
    true
  end

  def bif(:pid_to_list, 1) do
    true
  end

  def bif(:port_to_list, 1) do
    true
  end

  def bif(:port_close, 1) do
    true
  end

  def bif(:port_command, 2) do
    true
  end

  def bif(:port_command, 3) do
    true
  end

  def bif(:port_connect, 2) do
    true
  end

  def bif(:port_control, 3) do
    true
  end

  def bif(:pre_loaded, 0) do
    true
  end

  def bif(:process_flag, 2) do
    true
  end

  def bif(:process_flag, 3) do
    true
  end

  def bif(:process_info, 1) do
    true
  end

  def bif(:process_info, 2) do
    true
  end

  def bif(:processes, 0) do
    true
  end

  def bif(:purge_module, 1) do
    true
  end

  def bif(:put, 2) do
    true
  end

  def bif(:ref_to_list, 1) do
    true
  end

  def bif(:register, 2) do
    true
  end

  def bif(:registered, 0) do
    true
  end

  def bif(:round, 1) do
    true
  end

  def bif(:self, 0) do
    true
  end

  def bif(:setelement, 3) do
    true
  end

  def bif(:size, 1) do
    true
  end

  def bif(:spawn, 1) do
    true
  end

  def bif(:spawn, 2) do
    true
  end

  def bif(:spawn, 3) do
    true
  end

  def bif(:spawn, 4) do
    true
  end

  def bif(:spawn_link, 1) do
    true
  end

  def bif(:spawn_link, 2) do
    true
  end

  def bif(:spawn_link, 3) do
    true
  end

  def bif(:spawn_link, 4) do
    true
  end

  def bif(:spawn_request, 1) do
    true
  end

  def bif(:spawn_request, 2) do
    true
  end

  def bif(:spawn_request, 3) do
    true
  end

  def bif(:spawn_request, 4) do
    true
  end

  def bif(:spawn_request, 5) do
    true
  end

  def bif(:spawn_request_abandon, 1) do
    true
  end

  def bif(:spawn_monitor, 1) do
    true
  end

  def bif(:spawn_monitor, 2) do
    true
  end

  def bif(:spawn_monitor, 3) do
    true
  end

  def bif(:spawn_monitor, 4) do
    true
  end

  def bif(:spawn_opt, 2) do
    true
  end

  def bif(:spawn_opt, 3) do
    true
  end

  def bif(:spawn_opt, 4) do
    true
  end

  def bif(:spawn_opt, 5) do
    true
  end

  def bif(:split_binary, 2) do
    true
  end

  def bif(:statistics, 1) do
    true
  end

  def bif(:term_to_binary, 1) do
    true
  end

  def bif(:term_to_binary, 2) do
    true
  end

  def bif(:term_to_iovec, 1) do
    true
  end

  def bif(:term_to_iovec, 2) do
    true
  end

  def bif(:throw, 1) do
    true
  end

  def bif(:time, 0) do
    true
  end

  def bif(:tl, 1) do
    true
  end

  def bif(:trunc, 1) do
    true
  end

  def bif(:tuple_size, 1) do
    true
  end

  def bif(:tuple_to_list, 1) do
    true
  end

  def bif(:unlink, 1) do
    true
  end

  def bif(:unregister, 1) do
    true
  end

  def bif(:whereis, 1) do
    true
  end

  def bif(name, a)
      when is_atom(name) and
             is_integer(a) do
    false
  end

  def old_bif(:abs, 1) do
    true
  end

  def old_bif(:apply, 2) do
    true
  end

  def old_bif(:apply, 3) do
    true
  end

  def old_bif(:atom_to_binary, 2) do
    true
  end

  def old_bif(:atom_to_list, 1) do
    true
  end

  def old_bif(:binary_to_atom, 2) do
    true
  end

  def old_bif(:binary_to_existing_atom, 2) do
    true
  end

  def old_bif(:binary_to_list, 1) do
    true
  end

  def old_bif(:binary_to_list, 3) do
    true
  end

  def old_bif(:binary_to_term, 1) do
    true
  end

  def old_bif(:bitsize, 1) do
    true
  end

  def old_bif(:bit_size, 1) do
    true
  end

  def old_bif(:bitstring_to_list, 1) do
    true
  end

  def old_bif(:byte_size, 1) do
    true
  end

  def old_bif(:check_process_code, 2) do
    true
  end

  def old_bif(:date, 0) do
    true
  end

  def old_bif(:delete_module, 1) do
    true
  end

  def old_bif(:disconnect_node, 1) do
    true
  end

  def old_bif(:element, 2) do
    true
  end

  def old_bif(:erase, 0) do
    true
  end

  def old_bif(:erase, 1) do
    true
  end

  def old_bif(:exit, 1) do
    true
  end

  def old_bif(:exit, 2) do
    true
  end

  def old_bif(:float, 1) do
    true
  end

  def old_bif(:float_to_list, 1) do
    true
  end

  def old_bif(:garbage_collect, 0) do
    true
  end

  def old_bif(:garbage_collect, 1) do
    true
  end

  def old_bif(:get, 0) do
    true
  end

  def old_bif(:get, 1) do
    true
  end

  def old_bif(:get_keys, 1) do
    true
  end

  def old_bif(:group_leader, 0) do
    true
  end

  def old_bif(:group_leader, 2) do
    true
  end

  def old_bif(:halt, 0) do
    true
  end

  def old_bif(:halt, 1) do
    true
  end

  def old_bif(:hd, 1) do
    true
  end

  def old_bif(:integer_to_list, 1) do
    true
  end

  def old_bif(:iolist_size, 1) do
    true
  end

  def old_bif(:iolist_to_binary, 1) do
    true
  end

  def old_bif(:is_alive, 0) do
    true
  end

  def old_bif(:is_process_alive, 1) do
    true
  end

  def old_bif(:is_atom, 1) do
    true
  end

  def old_bif(:is_boolean, 1) do
    true
  end

  def old_bif(:is_binary, 1) do
    true
  end

  def old_bif(:is_bitstring, 1) do
    true
  end

  def old_bif(:is_float, 1) do
    true
  end

  def old_bif(:is_function, 1) do
    true
  end

  def old_bif(:is_function, 2) do
    true
  end

  def old_bif(:is_integer, 1) do
    true
  end

  def old_bif(:is_list, 1) do
    true
  end

  def old_bif(:is_number, 1) do
    true
  end

  def old_bif(:is_pid, 1) do
    true
  end

  def old_bif(:is_port, 1) do
    true
  end

  def old_bif(:is_reference, 1) do
    true
  end

  def old_bif(:is_tuple, 1) do
    true
  end

  def old_bif(:is_record, 2) do
    true
  end

  def old_bif(:is_record, 3) do
    true
  end

  def old_bif(:length, 1) do
    true
  end

  def old_bif(:link, 1) do
    true
  end

  def old_bif(:list_to_atom, 1) do
    true
  end

  def old_bif(:list_to_binary, 1) do
    true
  end

  def old_bif(:list_to_bitstring, 1) do
    true
  end

  def old_bif(:list_to_existing_atom, 1) do
    true
  end

  def old_bif(:list_to_float, 1) do
    true
  end

  def old_bif(:list_to_integer, 1) do
    true
  end

  def old_bif(:list_to_pid, 1) do
    true
  end

  def old_bif(:list_to_tuple, 1) do
    true
  end

  def old_bif(:load_module, 2) do
    true
  end

  def old_bif(:make_ref, 0) do
    true
  end

  def old_bif(:module_loaded, 1) do
    true
  end

  def old_bif(:monitor_node, 2) do
    true
  end

  def old_bif(:node, 0) do
    true
  end

  def old_bif(:node, 1) do
    true
  end

  def old_bif(:nodes, 0) do
    true
  end

  def old_bif(:nodes, 1) do
    true
  end

  def old_bif(:now, 0) do
    true
  end

  def old_bif(:open_port, 2) do
    true
  end

  def old_bif(:pid_to_list, 1) do
    true
  end

  def old_bif(:port_close, 1) do
    true
  end

  def old_bif(:port_command, 2) do
    true
  end

  def old_bif(:port_connect, 2) do
    true
  end

  def old_bif(:port_control, 3) do
    true
  end

  def old_bif(:pre_loaded, 0) do
    true
  end

  def old_bif(:process_flag, 2) do
    true
  end

  def old_bif(:process_flag, 3) do
    true
  end

  def old_bif(:process_info, 1) do
    true
  end

  def old_bif(:process_info, 2) do
    true
  end

  def old_bif(:processes, 0) do
    true
  end

  def old_bif(:purge_module, 1) do
    true
  end

  def old_bif(:put, 2) do
    true
  end

  def old_bif(:register, 2) do
    true
  end

  def old_bif(:registered, 0) do
    true
  end

  def old_bif(:round, 1) do
    true
  end

  def old_bif(:self, 0) do
    true
  end

  def old_bif(:setelement, 3) do
    true
  end

  def old_bif(:size, 1) do
    true
  end

  def old_bif(:spawn, 1) do
    true
  end

  def old_bif(:spawn, 2) do
    true
  end

  def old_bif(:spawn, 3) do
    true
  end

  def old_bif(:spawn, 4) do
    true
  end

  def old_bif(:spawn_link, 1) do
    true
  end

  def old_bif(:spawn_link, 2) do
    true
  end

  def old_bif(:spawn_link, 3) do
    true
  end

  def old_bif(:spawn_link, 4) do
    true
  end

  def old_bif(:spawn_monitor, 1) do
    true
  end

  def old_bif(:spawn_monitor, 3) do
    true
  end

  def old_bif(:spawn_opt, 2) do
    true
  end

  def old_bif(:spawn_opt, 3) do
    true
  end

  def old_bif(:spawn_opt, 4) do
    true
  end

  def old_bif(:spawn_opt, 5) do
    true
  end

  def old_bif(:split_binary, 2) do
    true
  end

  def old_bif(:statistics, 1) do
    true
  end

  def old_bif(:term_to_binary, 1) do
    true
  end

  def old_bif(:term_to_binary, 2) do
    true
  end

  def old_bif(:throw, 1) do
    true
  end

  def old_bif(:time, 0) do
    true
  end

  def old_bif(:tl, 1) do
    true
  end

  def old_bif(:trunc, 1) do
    true
  end

  def old_bif(:tuple_size, 1) do
    true
  end

  def old_bif(:tuple_to_list, 1) do
    true
  end

  def old_bif(:unlink, 1) do
    true
  end

  def old_bif(:unregister, 1) do
    true
  end

  def old_bif(:whereis, 1) do
    true
  end

  def old_bif(name, a)
      when is_atom(name) and
             is_integer(a) do
    false
  end

  def is_type(:any, 0) do
    true
  end

  def is_type(:arity, 0) do
    true
  end

  def is_type(:atom, 0) do
    true
  end

  def is_type(:binary, 0) do
    true
  end

  def is_type(:bitstring, 0) do
    true
  end

  def is_type(:bool, 0) do
    true
  end

  def is_type(:boolean, 0) do
    true
  end

  def is_type(:byte, 0) do
    true
  end

  def is_type(:char, 0) do
    true
  end

  def is_type(:float, 0) do
    true
  end

  def is_type(:function, 0) do
    true
  end

  def is_type(:identifier, 0) do
    true
  end

  def is_type(:integer, 0) do
    true
  end

  def is_type(:iodata, 0) do
    true
  end

  def is_type(:iolist, 0) do
    true
  end

  def is_type(:list, 0) do
    true
  end

  def is_type(:list, 1) do
    true
  end

  def is_type(:map, 0) do
    true
  end

  def is_type(:maybe_improper_list, 0) do
    true
  end

  def is_type(:maybe_improper_list, 2) do
    true
  end

  def is_type(:mfa, 0) do
    true
  end

  def is_type(:module, 0) do
    true
  end

  def is_type(:neg_integer, 0) do
    true
  end

  def is_type(nil, 0) do
    true
  end

  def is_type(:no_return, 0) do
    true
  end

  def is_type(:node, 0) do
    true
  end

  def is_type(:non_neg_integer, 0) do
    true
  end

  def is_type(:none, 0) do
    true
  end

  def is_type(:nonempty_improper_list, 2) do
    true
  end

  def is_type(:nonempty_list, 0) do
    true
  end

  def is_type(:nonempty_list, 1) do
    true
  end

  def is_type(:nonempty_maybe_improper_list, 0) do
    true
  end

  def is_type(:nonempty_maybe_improper_list, 2) do
    true
  end

  def is_type(:nonempty_string, 0) do
    true
  end

  def is_type(:number, 0) do
    true
  end

  def is_type(:pid, 0) do
    true
  end

  def is_type(:port, 0) do
    true
  end

  def is_type(:pos_integer, 0) do
    true
  end

  def is_type(:reference, 0) do
    true
  end

  def is_type(:string, 0) do
    true
  end

  def is_type(:term, 0) do
    true
  end

  def is_type(:timeout, 0) do
    true
  end

  def is_type(:tuple, 0) do
    true
  end

  def is_type(_, _) do
    false
  end

  def add_predefined_functions(forms) do
    forms ++ predefined_functions(forms)
  end

  defp predefined_functions(forms) do
    attrs =
      for {:attribute, _, name, val} <- forms do
        {name, val}
      end

    {:module, mod} = :lists.keyfind(:module, 1, attrs)

    callbacks =
      for {:callback, callback} <- attrs do
        callback
      end

    optionalCallbacks = get_optional_callbacks(attrs)

    mpf1 =
      module_predef_func_beh_info(
        callbacks,
        optionalCallbacks
      )

    mpf2 = module_predef_funcs_mod_info(mod)

    mpf =
      for f <- mpf1 ++ mpf2 do
        :erl_parse.new_anno(f)
      end

    exp =
      for {:function, _, f, a, _} <- mpf do
        {f, a}
      end

    [{:attribute, 0, :export, exp} | mpf]
  end

  defp get_optional_callbacks(attrs) do
    l =
      for {:optional_callbacks, o} <- attrs,
          is_fa_list(o) do
        o
      end

    :lists.append(l)
  end

  defp is_fa_list([{funcName, arity} | l])
       when is_atom(funcName) and is_integer(arity) and
              arity >= 0 do
    is_fa_list(l)
  end

  defp is_fa_list([]) do
    true
  end

  defp is_fa_list(_) do
    false
  end

  defp module_predef_func_beh_info([], _) do
    []
  end

  defp module_predef_func_beh_info(callbacks0, optionalCallbacks) do
    callbacks =
      for {{_, _} = fA, _} <- callbacks0 do
        fA
      end

    list = make_list(callbacks)
    optionalList = make_list(optionalCallbacks)

    [
      {:function, 0, :behaviour_info, 1,
       [
         {:clause, 0, [{:atom, 0, :callbacks}], [], [list]},
         {:clause, 0, [{:atom, 0, :optional_callbacks}], [], [optionalList]}
       ]}
    ]
  end

  defp make_list([]) do
    {nil, 0}
  end

  defp make_list([{name, arity} | rest]) do
    {:cons, 0, {:tuple, 0, [{:atom, 0, name}, {:integer, 0, arity}]}, make_list(rest)}
  end

  defp module_predef_funcs_mod_info(mod) do
    modAtom = {:atom, 0, mod}

    [
      {:function, 0, :module_info, 0,
       [
         {:clause, 0, [], [],
          [{:call, 0, {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :get_module_info}}, [modAtom]}]}
       ]},
      {:function, 0, :module_info, 1,
       [
         {:clause, 0, [{:var, 0, :X}], [],
          [
            {:call, 0, {:remote, 0, {:atom, 0, :erlang}, {:atom, 0, :get_module_info}},
             [modAtom, {:var, 0, :X}]}
          ]}
       ]}
    ]
  end
end
