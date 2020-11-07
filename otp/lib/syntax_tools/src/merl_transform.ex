defmodule :m_merl_transform do
  use Bitwise

  def parse_transform(forms, _Options) do
    :erl_syntax.revert_forms(expand(:erl_syntax.form_list(forms)))
  end

  defp expand(tree0) do
    tree = pre(tree0)

    post(
      case :erl_syntax.subtrees(tree) do
        [] ->
          tree

        gs ->
          :erl_syntax.update_tree(
            tree,
            for g <- gs do
              for t <- g do
                expand(t)
              end
            end
          )
      end
    )
  end

  defp pre(t) do
    :merl.switch(
      t,
      [
        {:merl.quote(61, 'merl:quote(_@line, _@text) = _@expr'),
         fn [{:expr, _}, {:line, line}, {:text, text}] ->
           :erl_syntax.is_literal(text) and :erl_syntax.is_literal(line)
         end,
         fn [{:expr, expr}, {:line, line}, {:text, text}] ->
           pre_expand_match(expr, :erl_syntax.concrete(line), :erl_syntax.concrete(text))
         end},
        {:merl.quote(69, [
           'case _@expr of',
           '  merl:quote(_@_, _@text) when _@__@_ -> _@@_; _@_@_ -> 0',
           'end'
         ]), &case_guard/1,
         fn as ->
           case_body(as, t)
         end},
        fn ->
          t
        end
      ]
    )
  end

  defp case_guard([{:expr, _}, {:text, text}]) do
    :erl_syntax.is_literal(text)
  end

  defp case_body([{:expr, expr}, {:text, _Text}], t) do
    pre_expand_case(expr, :erl_syntax.case_expr_clauses(t), get_location(t))
  end

  defp post(t) do
    :merl.switch(
      t,
      [
        {:merl.quote(86, 'merl:_@function(_@@args)'),
         [
           {fn [{:args, as}, {:function, f}] ->
              :lists.all(&:erl_syntax.is_literal/1, [f | as])
            end,
            fn [{:args, as}, {:function, f}] ->
              line = get_location(f)

              [f1 | as1] =
                :lists.map(
                  &:erl_syntax.concrete/1,
                  [f | as]
                )

              eval_call(line, f1, as1, t)
            end},
           fn [{:args, as}, {:function, f}] ->
             :merl.switch(
               f,
               [
                 {:merl.quote(98, 'qquote'),
                  fn [] ->
                    expand_qquote(as, t, 1)
                  end},
                 {:merl.quote(99, 'subst'),
                  fn [] ->
                    expand_template(f, as, t)
                  end},
                 {:merl.quote(100, 'match'),
                  fn [] ->
                    expand_template(f, as, t)
                  end},
                 fn ->
                   t
                 end
               ]
             )
           end
         ]},
        fn ->
          t
        end
      ]
    )
  end

  defp expand_qquote([line, text, env], t, _) do
    case :erl_syntax.is_literal(line) do
      true ->
        expand_qquote([text, env], t, :erl_syntax.concrete(line))

      false ->
        t
    end
  end

  defp expand_qquote([text, env], t, line) do
    case :erl_syntax.is_literal(text) do
      true ->
        as = [line, :erl_syntax.concrete(text)]

        case eval_call(line, :quote, as, :failed) do
          :failed ->
            t

          t1 ->
            expand(:merl.qquote(line, 'merl:subst(_@tree, _@env)', [{:tree, t1}, {:env, env}]))
        end

      false ->
        t
    end
  end

  defp expand_qquote(_As, t, _StartPos) do
    t
  end

  defp expand_template(f, [pattern | args], t) do
    case :erl_syntax.is_literal(pattern) do
      true ->
        line = get_location(pattern)
        as = [:erl_syntax.concrete(pattern)]

        :merl.qquote(line, 'merl:_@function(_@pattern, _@args)', [
          {:function, f},
          {:pattern, eval_call(line, :template, as, t)},
          {:args, args}
        ])

      false ->
        t
    end
  end

  defp expand_template(_F, _As, t) do
    t
  end

  defp eval_call(line, f, as, t) do
    try do
      apply(:merl, f, as)
    catch
      _Reason ->
        t
    else
      t1 when f === :quote ->
        template = :merl.template(t1)
        vars = :merl.template_vars(template)

        case :lists.any(&is_inline_metavar/1, vars) do
          true when is_list(t1) ->
            :merl.qquote(line, 'merl:tree([_@template])', [
              {:template, :merl.meta_template(template)}
            ])

          true ->
            :merl.qquote(line, 'merl:tree(_@template)', [
              {:template, :merl.meta_template(template)}
            ])

          false ->
            :merl.term(t1)
        end

      t1 ->
        :merl.term(t1)
    end
  end

  defp pre_expand_match(expr, line, text) do
    {template, out, _Vars} = rewrite_pattern(line, text)

    :merl.qquote(line, '{ok, _@out} = merl:match(_@template, _@expr)', [
      {:expr, expr},
      {:out, out},
      {:template, :erl_syntax.abstract(template)}
    ])
  end

  defp rewrite_pattern(line, text) do
    t0 = :merl.template(:merl.quote(line, text))

    vars =
      for v <- :merl.template_vars(t0),
          is_inline_metavar(v) do
        v
      end

    {:merl.alpha(
       t0,
       for v <- vars do
         {v, var_to_tag(v)}
       end
     ),
     :erl_syntax.list(
       for v <- vars do
         :erl_syntax.tuple([
           :erl_syntax.abstract(var_to_tag(v)),
           :erl_syntax.variable(var_name(v))
         ])
       end
     ), vars}
  end

  defp var_name(v) when is_integer(v) do
    v1 =
      cond do
        v > 99 and rem(v, 100) === 99 ->
          div(v, 100)

        v > 9 and rem(v, 10) === 9 ->
          div(v, 10)

        true ->
          v
      end

    :erlang.list_to_atom('Q' ++ :erlang.integer_to_list(v1))
  end

  defp var_name(v) do
    v
  end

  defp var_to_tag(v) when is_integer(v) do
    v
  end

  defp var_to_tag(v) do
    :erlang.list_to_atom(:string.lowercase(:erlang.atom_to_list(v)))
  end

  defp pre_expand_case(expr, clauses, line) do
    :merl.qquote(line, 'merl:switch(_@expr, _@clauses)', [
      {:clauses,
       :erl_syntax.list(
         for c <- clauses do
           pre_expand_case_clause(c)
         end
       )},
      {:expr, expr}
    ])
  end

  defp pre_expand_case_clause(t) do
    :merl.switch(
      t,
      [
        {:merl.quote(211, '(merl:quote(_@line, _@text)) when _@__@guard -> _@@body'),
         fn [{:body, _}, {:guard, _}, {:line, line}, {:text, text}] ->
           :erl_syntax.is_literal(text) and :erl_syntax.is_literal(line)
         end,
         fn [{:body, body}, {:guard, guard}, {:line, line}, {:text, text}] ->
           pre_expand_case_clause(
             body,
             guard,
             :erl_syntax.concrete(line),
             :erl_syntax.concrete(text)
           )
         end},
        {:merl.quote(219, '_ -> _@@body'),
         fn env ->
           :merl.qquote('fun () -> _@body end', env)
         end}
      ]
    )
  end

  defp pre_expand_case_clause(body, guard, line, text) do
    {template, out, vars} = rewrite_pattern(line, text)
    guardExprs = rewrite_guard(guard)

    param = [
      {:body, body},
      {:guard, guardExprs},
      {:out, out},
      {:template, :erl_syntax.abstract(template)},
      {:unused, dummy_uses(vars)}
    ]

    case guardExprs do
      [] ->
        :merl.qquote(line, ['{_@template, ', ' fun (_@out) -> _@unused, _@body end}'], param)

      _ ->
        :merl.qquote(
          line,
          [
            '{_@template, ',
            ' fun (_@out) -> _@unused, _@guard end, ',
            ' fun (_@out) -> _@unused, _@body end}'
          ],
          param
        )
    end
  end

  defp dummy_uses(vars) do
    for v <- vars do
      :merl.qquote(250, '_ = _@var', [{:var, :erl_syntax.variable(var_name(v))}])
    end
  end

  defp rewrite_guard([]) do
    []
  end

  defp rewrite_guard([d]) do
    [make_orelse(:erl_syntax.disjunction_body(d))]
  end

  defp make_orelse([]) do
    []
  end

  defp make_orelse([c]) do
    make_andalso(:erl_syntax.conjunction_body(c))
  end

  defp make_orelse([c | cs]) do
    :merl.qquote(259, '_@expr orelse _@rest', [
      {:expr, make_andalso(:erl_syntax.conjunction_body(c))},
      {:rest, make_orelse(cs)}
    ])
  end

  defp make_andalso([e]) do
    e
  end

  defp make_andalso([e | es]) do
    :merl.qquote(265, '_@expr andalso _@rest', [{:expr, e}, {:rest, make_andalso(es)}])
  end

  defp is_inline_metavar(var) when is_atom(var) do
    is_erlang_var(:erlang.atom_to_list(var))
  end

  defp is_inline_metavar(var) when is_integer(var) do
    var > 9 and rem(var, 10) === 9
  end

  defp is_inline_metavar(_) do
    false
  end

  defp is_erlang_var([c | _])
       when (c >= ?A and c <= ?Z) or
              (c >= 195 and c <= 195 and c != 195) do
    true
  end

  defp is_erlang_var(_) do
    false
  end

  defp get_location(t) do
    pos = :erl_syntax.get_pos(t)

    case :erl_anno.is_anno(pos) do
      true ->
        :erl_anno.location(pos)

      false ->
        pos
    end
  end
end
