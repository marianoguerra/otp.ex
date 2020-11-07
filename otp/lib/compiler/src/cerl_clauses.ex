defmodule :m_cerl_clauses do
  use Bitwise

  import :cerl,
    only: [
      alias_pat: 1,
      alias_var: 1,
      clause_guard: 1,
      clause_pats: 1,
      concrete: 1,
      data_arity: 1,
      data_es: 1,
      data_type: 1,
      is_c_var: 1,
      is_data: 1,
      let_body: 1,
      letrec_body: 1,
      seq_body: 1,
      try_arg: 1,
      type: 1,
      values_es: 1
    ]

  def is_catchall(c) do
    case all_vars(clause_pats(c)) do
      true ->
        case eval_guard(clause_guard(c)) do
          {:value, true} ->
            true

          _ ->
            false
        end

      false ->
        false
    end
  end

  defp all_vars([c | cs]) do
    case is_c_var(c) do
      true ->
        all_vars(cs)

      false ->
        false
    end
  end

  defp all_vars([]) do
    true
  end

  def any_catchall([c | cs]) do
    case is_catchall(c) do
      true ->
        true

      false ->
        any_catchall(cs)
    end
  end

  def any_catchall([]) do
    false
  end

  def eval_guard(e) do
    case type(e) do
      :literal ->
        {:value, concrete(e)}

      :values ->
        case values_es(e) do
          [e1] ->
            eval_guard(e1)

          _ ->
            :none
        end

      :try ->
        eval_guard(try_arg(e))

      :seq ->
        eval_guard(seq_body(e))

      :let ->
        eval_guard(let_body(e))

      :letrec ->
        eval_guard(letrec_body(e))

      _ ->
        :none
    end
  end

  def reduce(cs) do
    reduce(cs, [])
  end

  def reduce(cs, es) do
    reduce(cs, es, [])
  end

  defp reduce([c | cs], es, cs1) do
    ps = clause_pats(c)

    case match_list(ps, es) do
      :none ->
        reduce(cs, es, cs1)

      {false, _} ->
        reduce(cs, es, [c | cs1])

      {true, bs} ->
        case eval_guard(clause_guard(c)) do
          {:value, true} when cs1 === [] ->
            {true, {c, bs}}

          {:value, true} ->
            {false, :lists.reverse([c | cs1])}

          {:value, false} ->
            reduce(cs, es, cs1)

          _ ->
            reduce(cs, es, [c | cs1])
        end
    end
  end

  defp reduce([], _, cs) do
    {false, :lists.reverse(cs)}
  end

  def match(p, e) do
    match(p, e, [])
  end

  defp match(p, e, bs) do
    case type(p) do
      :var ->
        {true, [{p, e} | bs]}

      :alias ->
        match(alias_pat(p), e, [{alias_var(p), e} | bs])

      :binary ->
        cond do
          e === :any ->
            {false, bs}

          true ->
            case type(e) do
              :literal ->
                case is_bitstring(concrete(e)) do
                  false ->
                    :none

                  true ->
                    {false, bs}
                end

              :cons ->
                :none

              :tuple ->
                :none

              _ ->
                {false, bs}
            end
        end

      :map ->
        case e do
          :any ->
            {false, bs}

          _ ->
            case type(e) do
              :literal ->
                case is_map(concrete(e)) do
                  false ->
                    :none

                  true ->
                    {false, bs}
                end

              :cons ->
                :none

              :tuple ->
                :none

              _ ->
                {false, bs}
            end
        end

      _ ->
        match_1(p, e, bs)
    end
  end

  defp match_1(p, e, bs) do
    case is_data(p) do
      true when e === :any ->
        ps = data_es(p)

        es =
          for _ <- ps do
            :any
          end

        case match_list(ps, es, bs) do
          {_, bs1} ->
            {false, bs1}

          :none ->
            :none
        end

      true ->
        case is_data(e) do
          true ->
            t1 = {data_type(e), data_arity(e)}
            t2 = {data_type(p), data_arity(p)}

            cond do
              t1 === t2 ->
                match_list(data_es(p), data_es(e), bs)

              true ->
                :none
            end

          false ->
            match_1(p, :any, bs)
        end

      false ->
        {false, bs}
    end
  end

  def match_list([], []) do
    {true, []}
  end

  def match_list(ps, []) do
    match_list(
      ps,
      for _ <- ps do
        :any
      end,
      []
    )
  end

  def match_list(ps, es) do
    match_list(ps, es, [])
  end

  defp match_list([p | ps], [e | es], bs) do
    case match(p, e, bs) do
      {true, bs1} ->
        match_list(ps, es, bs1)

      {false, bs1} ->
        case match_list(ps, es, bs1) do
          {_, bs2} ->
            {false, bs2}

          :none ->
            :none
        end

      :none ->
        :none
    end
  end

  defp match_list([], [], bs) do
    {true, bs}
  end
end
