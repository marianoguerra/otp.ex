defmodule :m_mnesia_frag_hash do
  use Bitwise
  require Record

  Record.defrecord(:r_hash_state, :hash_state,
    n_fragments: :undefined,
    next_n_to_split: :undefined,
    n_doubles: :undefined,
    function: :undefined
  )

  def init_state(_Tab, state) when state == :undefined do
    r_hash_state(n_fragments: 1, next_n_to_split: 1, n_doubles: 0, function: :phash2)
  end

  defp convert_old_state({:hash_state, n, p, l}) do
    r_hash_state(n_fragments: n, next_n_to_split: p, n_doubles: l, function: :phash)
  end

  def add_frag(r_hash_state(next_n_to_split: splitN, n_doubles: l, n_fragments: n) = state) do
    p = splitN + 1
    newN = n + 1

    state2 =
      case power2(l) + 1 do
        p2 when p2 == p ->
          r_hash_state(state, n_fragments: newN, n_doubles: l + 1, next_n_to_split: 1)

        _ ->
          r_hash_state(state, n_fragments: newN, next_n_to_split: p)
      end

    {state2, [splitN], [newN]}
  end

  def add_frag(oldState) do
    state = convert_old_state(oldState)
    add_frag(state)
  end

  def del_frag(r_hash_state(next_n_to_split: splitN, n_doubles: l, n_fragments: n) = state) do
    p = splitN - 1

    cond do
      p < 1 ->
        l2 = l - 1
        mergeN = power2(l2)
        state2 = r_hash_state(state, n_fragments: n - 1, next_n_to_split: mergeN, n_doubles: l2)
        {state2, [n], [mergeN]}

      true ->
        mergeN = p

        state2 =
          r_hash_state(state,
            n_fragments: n - 1,
            next_n_to_split: mergeN
          )

        {state2, [n], [mergeN]}
    end
  end

  def del_frag(oldState) do
    state = convert_old_state(oldState)
    del_frag(state)
  end

  def key_to_frag_number(
        r_hash_state(function: :phash, n_fragments: n, n_doubles: l),
        key
      ) do
    a = :erlang.phash(key, power2(l + 1))

    cond do
      a > n ->
        a - power2(l)

      true ->
        a
    end
  end

  def key_to_frag_number(
        r_hash_state(function: :phash2, n_fragments: n, n_doubles: l),
        key
      ) do
    a = :erlang.phash2(key, power2(l + 1)) + 1

    cond do
      a > n ->
        a - power2(l)

      true ->
        a
    end
  end

  def key_to_frag_number(oldState, key) do
    state = convert_old_state(oldState)
    key_to_frag_number(state, key)
  end

  def match_spec_to_frag_numbers(r_hash_state(n_fragments: n) = state, matchSpec) do
    case matchSpec do
      [{headPat, _, _}]
      when is_tuple(headPat) and
             tuple_size(headPat) > 2 ->
        keyPat = :erlang.element(2, headPat)

        case has_var(keyPat) do
          false ->
            [key_to_frag_number(state, keyPat)]

          true ->
            :lists.seq(1, n)
        end

      _ ->
        :lists.seq(1, n)
    end
  end

  def match_spec_to_frag_numbers(oldState, matchSpec) do
    state = convert_old_state(oldState)
    match_spec_to_frag_numbers(state, matchSpec)
  end

  defp power2(y) do
    1 <<< y
  end

  defp has_var(pat) do
    :mnesia.has_var(pat)
  end
end
