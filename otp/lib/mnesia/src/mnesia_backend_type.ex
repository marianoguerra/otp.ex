defmodule :m_mnesia_backend_type do
  use Bitwise

  def behaviour_info(:callbacks) do
    [
      {:add_aliases, 1},
      {:check_definition, 4},
      {:close_table, 2},
      {:create_table, 3},
      {:delete, 3},
      {:delete_table, 2},
      {:first, 2},
      {:fixtable, 3},
      {:last, 2},
      {:index_is_consistent, 3},
      {:init_backend, 0},
      {:info, 3},
      {:insert, 3},
      {:lookup, 3},
      {:is_index_consistent, 2},
      {:load_table, 4},
      {:match_delete, 3},
      {:next, 3},
      {:prev, 3},
      {:receiver_first_message, 4},
      {:receive_data, 5},
      {:receive_done, 4},
      {:real_suffixes, 0},
      {:remove_aliases, 1},
      {:repair_continuation, 2},
      {:select, 1},
      {:select, 3},
      {:select, 4},
      {:sender_init, 4},
      {:semantics, 2},
      {:slot, 3},
      {:sync_close_table, 2},
      {:tmp_suffixes, 0},
      {:update_counter, 4},
      {:validate_key, 6},
      {:validate_record, 6}
    ]
  end
end
