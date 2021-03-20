defmodule :m_erl_prettypr do
  use Bitwise

  import :erl_parse,
    only: [
      func_prec: 0,
      inop_prec: 1,
      max_prec: 0,
      preop_prec: 1,
      type_inop_prec: 1,
      type_preop_prec: 1
    ]

  import :prettypr,
    only: [
      above: 2,
      beside: 2,
      break: 1,
      empty: 0,
      floating: 1,
      floating: 3,
      follow: 2,
      follow: 3,
      nest: 2,
      par: 1,
      par: 2,
      sep: 1,
      text: 1
    ]

  require Record

  Record.defrecord(:r_ctxt, :ctxt,
    prec: 0,
    sub_indent: 2,
    break_indent: 4,
    clause: :undefined,
    hook: :none,
    paper: 80,
    ribbon: 56,
    user: :undefined,
    encoding: :epp.default_encoding(),
    empty_lines: :sets.new()
  )

  def get_ctxt_precedence(ctxt) do
    r_ctxt(ctxt, :prec)
  end

  def set_ctxt_precedence(ctxt, prec) do
    set_prec(ctxt, prec)
  end

  defp set_prec(ctxt, prec) do
    r_ctxt(ctxt, prec: prec)
  end

  defp reset_prec(ctxt) do
    set_prec(ctxt, 0)
  end

  def get_ctxt_paperwidth(ctxt) do
    r_ctxt(ctxt, :paper)
  end

  def set_ctxt_paperwidth(ctxt, w) do
    r_ctxt(ctxt, paper: w)
  end

  def get_ctxt_linewidth(ctxt) do
    r_ctxt(ctxt, :ribbon)
  end

  def set_ctxt_linewidth(ctxt, w) do
    r_ctxt(ctxt, ribbon: w)
  end

  def get_ctxt_hook(ctxt) do
    r_ctxt(ctxt, :hook)
  end

  def set_ctxt_hook(ctxt, hook) do
    r_ctxt(ctxt, hook: hook)
  end

  def get_ctxt_user(ctxt) do
    r_ctxt(ctxt, :user)
  end

  def set_ctxt_user(ctxt, x) do
    r_ctxt(ctxt, user: x)
  end

  def format(node) do
    format(node, [])
  end

  def format(node, options) do
    w = :proplists.get_value(:paper, options, 80)
    l = :proplists.get_value(:ribbon, options, 56)
    :prettypr.format(layout(node, options), w, l)
  end

  def best(node) do
    best(node, [])
  end

  def best(node, options) do
    w = :proplists.get_value(:paper, options, 80)
    l = :proplists.get_value(:ribbon, options, 56)
    :prettypr.best(layout(node, options), w, l)
  end

  def layout(node) do
    layout(node, [])
  end

  def layout(node, options) do
    lay(
      node,
      r_ctxt(
        hook: :proplists.get_value(:hook, options, :none),
        paper: :proplists.get_value(:paper, options, 80),
        ribbon: :proplists.get_value(:ribbon, options, 56),
        user: :proplists.get_value(:user, options),
        encoding: :proplists.get_value(:encoding, options, :epp.default_encoding()),
        empty_lines: :proplists.get_value(:empty_lines, options, :sets.new())
      )
    )
  end

  defp lay(node, ctxt) do
    case :erl_syntax.get_ann(node) do
      [] ->
        lay_1(node, ctxt)

      _As ->
        case r_ctxt(ctxt, :hook) do
          :none ->
            lay_1(node, ctxt)

          hook ->
            hook.(node, ctxt, &lay_1/2)
        end
    end
  end

  defp lay_1(node, ctxt) do
    case :erl_syntax.has_comments(node) do
      true ->
        d1 = lay_2(node, ctxt)

        d2 =
          lay_postcomments(
            :erl_syntax.get_postcomments(node),
            d1
          )

        lay_precomments(:erl_syntax.get_precomments(node), d2)

      false ->
        lay_2(node, ctxt)
    end
  end

  defp lay_precomments([], d) do
    d
  end

  defp lay_precomments(cs, d) do
    above(
      floating(break(stack_comments(cs, false)), -1, -1),
      d
    )
  end

  defp lay_postcomments([], d) do
    d
  end

  defp lay_postcomments(cs, d) do
    beside(
      d,
      floating(break(stack_comments(cs, true)), 1, 0)
    )
  end

  defp stack_comments([c | cs], pad) do
    d = stack_comment_lines(:erl_syntax.comment_text(c))

    d1 =
      case pad do
        true ->
          p =
            case :erl_syntax.comment_padding(c) do
              :none ->
                2

              p1 ->
                p1
            end

          beside(text(spaces(p)), d)

        false ->
          d
      end

    case cs do
      [] ->
        d1

      _ ->
        above(d1, stack_comments(cs, pad))
    end
  end

  defp stack_comment_lines([s | ss]) do
    d = text(add_comment_prefix(s))

    case ss do
      [] ->
        d

      _ ->
        above(d, stack_comment_lines(ss))
    end
  end

  defp stack_comment_lines([]) do
    empty()
  end

  defp add_comment_prefix(s) do
    [?% | s]
  end

  defp lay_2(node, ctxt) do
    case :erl_syntax.type(node) do
      :variable ->
        text(:erl_syntax.variable_literal(node))

      :atom ->
        text(:erl_syntax.atom_literal(node, r_ctxt(ctxt, :encoding)))

      :integer ->
        text(:erl_syntax.integer_literal(node))

      :float ->
        text(tidy_float(:erl_syntax.float_literal(node)))

      :char ->
        text(:erl_syntax.char_literal(node, r_ctxt(ctxt, :encoding)))

      :string ->
        lay_string(
          :erl_syntax.string_literal(
            node,
            r_ctxt(ctxt, :encoding)
          ),
          ctxt
        )

      nil ->
        text('[]')

      :tuple ->
        es = seq(:erl_syntax.tuple_elements(node), floating(text(',')), reset_prec(ctxt), &lay/2)

        beside(
          floating(text('{')),
          beside(sep(es), floating(text('}')))
        )

      :list ->
        ctxt1 = reset_prec(ctxt)
        node1 = :erl_syntax.compact_list(node)
        d1 = sep(seq(:erl_syntax.list_prefix(node1), floating(text(',')), ctxt1, &lay/2))

        d =
          case :erl_syntax.list_suffix(node1) do
            :none ->
              beside(d1, floating(text(']')))

            s ->
              follow(
                d1,
                beside(
                  floating(text('| ')),
                  beside(lay(s, ctxt1), floating(text(']')))
                )
              )
          end

        beside(floating(text('[')), d)

      :operator ->
        floating(text(:erl_syntax.operator_literal(node)))

      :infix_expr ->
        operator = :erl_syntax.infix_expr_operator(node)

        {precL, prec, precR} =
          case :erl_syntax.type(operator) do
            :operator ->
              inop_prec(:erl_syntax.operator_name(operator))

            _ ->
              {0, 0, 0}
          end

        d1 =
          lay(
            :erl_syntax.infix_expr_left(node),
            set_prec(ctxt, precL)
          )

        d2 = lay(operator, reset_prec(ctxt))

        d3 =
          lay(
            :erl_syntax.infix_expr_right(node),
            set_prec(ctxt, precR)
          )

        d4 = par([d1, d2, d3], r_ctxt(ctxt, :break_indent))
        maybe_parentheses(d4, prec, ctxt)

      :prefix_expr ->
        operator = :erl_syntax.prefix_expr_operator(node)

        {{prec, precR}, name} =
          case :erl_syntax.type(operator) do
            :operator ->
              n = :erl_syntax.operator_name(operator)
              {preop_prec(n), n}

            _ ->
              {{0, 0}, :any}
          end

        d1 = lay(operator, reset_prec(ctxt))

        d2 =
          lay(
            :erl_syntax.prefix_expr_argument(node),
            set_prec(ctxt, precR)
          )

        d3 =
          case name do
            :+ ->
              beside(d1, d2)

            :- ->
              beside(d1, d2)

            _ ->
              par([d1, d2], r_ctxt(ctxt, :break_indent))
          end

        maybe_parentheses(d3, prec, ctxt)

      :application ->
        {precL, prec} = func_prec()

        d =
          lay(
            :erl_syntax.application_operator(node),
            set_prec(ctxt, precL)
          )

        as =
          seq(
            :erl_syntax.application_arguments(node),
            floating(text(',')),
            reset_prec(ctxt),
            &lay/2
          )

        d1 =
          beside(
            d,
            beside(text('('), beside(sep(as), floating(text(')'))))
          )

        maybe_parentheses(d1, prec, ctxt)

      :match_expr ->
        {precL, prec, precR} = inop_prec(:=)

        d1 =
          lay(
            :erl_syntax.match_expr_pattern(node),
            set_prec(ctxt, precL)
          )

        d2 =
          lay(
            :erl_syntax.match_expr_body(node),
            set_prec(ctxt, precR)
          )

        d3 = follow(beside(d1, floating(text(' ='))), d2, r_ctxt(ctxt, :break_indent))
        maybe_parentheses(d3, prec, ctxt)

      :underscore ->
        text('_')

      :clause ->
        ctxt1 = r_ctxt(reset_prec(ctxt), clause: :undefined)
        d1 = par(seq(:erl_syntax.clause_patterns(node), floating(text(',')), ctxt1, &lay/2))

        d2 =
          case :erl_syntax.clause_guard(node) do
            :none ->
              :none

            g ->
              lay(g, ctxt1)
          end

        d3 =
          lay_clause_expressions(
            :erl_syntax.clause_body(node),
            ctxt1
          )

        case r_ctxt(ctxt, :clause) do
          :fun_expr ->
            make_fun_clause(d1, d2, d3, ctxt)

          {:function, n} ->
            make_fun_clause(n, d1, d2, d3, ctxt)

          :if_expr ->
            make_if_clause(d1, d2, d3, ctxt)

          :case_expr ->
            make_case_clause(d1, d2, d3, ctxt)

          :receive_expr ->
            make_case_clause(d1, d2, d3, ctxt)

          :try_expr ->
            make_case_clause(d1, d2, d3, ctxt)

          :undefined ->
            make_fun_clause(d1, d2, d3, ctxt)
        end

      :function ->
        ctxt1 = reset_prec(ctxt)
        d1 = lay(:erl_syntax.function_name(node), ctxt1)
        d2 = lay_clauses(:erl_syntax.function_clauses(node), {:function, d1}, ctxt1)
        beside(d2, floating(text('.')))

      :case_expr ->
        ctxt1 = reset_prec(ctxt)
        d1 = lay(:erl_syntax.case_expr_argument(node), ctxt1)
        d2 = lay_clauses(:erl_syntax.case_expr_clauses(node), :case_expr, ctxt1)

        sep([
          par(
            [follow(text('case'), d1, r_ctxt(ctxt1, :break_indent)), text('of')],
            r_ctxt(ctxt1, :break_indent)
          ),
          nest(r_ctxt(ctxt1, :break_indent), d2),
          text('end')
        ])

      :if_expr ->
        ctxt1 = reset_prec(ctxt)
        d = lay_clauses(:erl_syntax.if_expr_clauses(node), :if_expr, ctxt1)
        sep([follow(text('if'), d, r_ctxt(ctxt1, :break_indent)), text('end')])

      :fun_expr ->
        ctxt1 = reset_prec(ctxt)
        d = lay_clauses(:erl_syntax.fun_expr_clauses(node), :fun_expr, ctxt1)
        sep([follow(text('fun'), d, r_ctxt(ctxt1, :break_indent)), text('end')])

      :named_fun_expr ->
        ctxt1 = reset_prec(ctxt)
        d1 = lay(:erl_syntax.named_fun_expr_name(node), ctxt1)
        d = lay_clauses(:erl_syntax.named_fun_expr_clauses(node), {:function, d1}, ctxt1)
        sep([follow(text('fun'), d, r_ctxt(ctxt1, :break_indent)), text('end')])

      :module_qualifier ->
        {precL, _Prec, precR} = inop_prec(:":")

        d1 =
          lay(
            :erl_syntax.module_qualifier_argument(node),
            set_prec(ctxt, precL)
          )

        d2 =
          lay(
            :erl_syntax.module_qualifier_body(node),
            set_prec(ctxt, precR)
          )

        beside(d1, beside(text(':'), d2))

      :arity_qualifier ->
        ctxt1 = reset_prec(ctxt)
        d1 = lay(:erl_syntax.arity_qualifier_body(node), ctxt1)

        d2 =
          lay(
            :erl_syntax.arity_qualifier_argument(node),
            ctxt1
          )

        beside(d1, beside(text('/'), d2))

      :attribute ->
        ctxt1 = reset_prec(ctxt)
        args = :erl_syntax.attribute_arguments(node)

        n =
          case :erl_syntax.attribute_name(node) do
            {:atom, _, :if} ->
              :erl_syntax.variable(:if)

            n0 ->
              n0
          end

        d =
          case attribute_type(node) do
            :spec ->
              [specTuple] = args
              [funcName, funcTypes] = :erl_syntax.tuple_elements(specTuple)

              name =
                case :erl_syntax.type(funcName) do
                  :tuple ->
                    case :erl_syntax.tuple_elements(funcName) do
                      [f0, _] ->
                        f0

                      [m0, f0, _] ->
                        :erl_syntax.module_qualifier(m0, f0)

                      _ ->
                        funcName
                    end

                  _ ->
                    funcName
                end

              types = dodge_macros(funcTypes)
              d1 = lay_clauses(:erl_syntax.concrete(types), :spec, ctxt1)

              beside(
                follow(lay(n, ctxt1), lay(name, ctxt1), r_ctxt(ctxt1, :break_indent)),
                d1
              )

            :type ->
              [typeTuple] = args
              [name, type0, elements] = :erl_syntax.tuple_elements(typeTuple)
              typeName = dodge_macros(name)
              type = dodge_macros(type0)
              as0 = dodge_macros(elements)
              as = :erl_syntax.concrete(as0)
              d1 = lay_type_application(typeName, as, ctxt1)
              d2 = lay(:erl_syntax.concrete(type), ctxt1)

              beside(
                follow(
                  lay(n, ctxt1),
                  beside(d1, floating(text(' :: '))),
                  r_ctxt(ctxt1, :break_indent)
                ),
                d2
              )

            tag
            when tag === :export_type or
                   tag === :optional_callbacks ->
              [funcNs] = args
              funcNames = :erl_syntax.concrete(dodge_macros(funcNs))
              as = unfold_function_names(funcNames)

              beside(
                lay(n, ctxt1),
                beside(
                  text('('),
                  beside(lay(as, ctxt1), floating(text(')')))
                )
              )

            _ when args === :none ->
              lay(n, ctxt1)

            _ ->
              d1 = sep(seq(args, text(','), ctxt1, &lay/2))

              beside(
                lay(n, ctxt1),
                beside(text('('), beside(d1, floating(text(')'))))
              )
          end

        beside(floating(text('-')), beside(d, floating(text('.'))))

      :binary ->
        ctxt1 = reset_prec(ctxt)
        es = seq(:erl_syntax.binary_fields(node), floating(text(',')), ctxt1, &lay/2)

        beside(
          floating(text('<<')),
          beside(par(es), floating(text('>>')))
        )

      :binary_field ->
        ctxt1 = set_prec(ctxt, max_prec())
        d1 = lay(:erl_syntax.binary_field_body(node), ctxt1)

        d2 =
          case :erl_syntax.binary_field_types(node) do
            [] ->
              empty()

            ts ->
              beside(floating(text('/')), lay_bit_types(ts, ctxt1))
          end

        beside(d1, d2)

      :block_expr ->
        ctxt1 = reset_prec(ctxt)
        es = seq(:erl_syntax.block_expr_body(node), floating(text(',')), ctxt1, &lay/2)
        sep([text('begin'), nest(r_ctxt(ctxt1, :break_indent), sep(es)), text('end')])

      :catch_expr ->
        {prec, precR} = preop_prec(:catch)

        d =
          lay(
            :erl_syntax.catch_expr_body(node),
            set_prec(ctxt, precR)
          )

        d1 = follow(text('catch'), d, r_ctxt(ctxt, :break_indent))
        maybe_parentheses(d1, prec, ctxt)

      :class_qualifier ->
        ctxt1 = set_prec(ctxt, max_prec())

        d1 =
          lay(
            :erl_syntax.class_qualifier_argument(node),
            ctxt1
          )

        d2 = lay(:erl_syntax.class_qualifier_body(node), ctxt1)
        stacktrace = :erl_syntax.class_qualifier_stacktrace(node)

        case :erl_syntax.variable_name(stacktrace) do
          :_ ->
            beside(d1, beside(text(':'), d2))

          _ ->
            d3 = lay(stacktrace, ctxt1)

            beside(
              d1,
              beside(beside(text(':'), d2), beside(text(':'), d3))
            )
        end

      :comment ->
        d = stack_comment_lines(:erl_syntax.comment_text(node))

        case :erl_syntax.comment_padding(node) do
          :none ->
            floating(break(d))

          p ->
            floating(break(beside(text(spaces(p)), d)))
        end

      :conjunction ->
        par(
          seq(:erl_syntax.conjunction_body(node), floating(text(',')), reset_prec(ctxt), &lay/2)
        )

      :disjunction ->
        sep(
          seq(:erl_syntax.disjunction_body(node), floating(text(';')), reset_prec(ctxt), &lay/2)
        )

      :error_marker ->
        e = :erl_syntax.error_marker_info(node)

        beside(
          text('** '),
          beside(lay_error_info(e, reset_prec(ctxt)), text(' **'))
        )

      :eof_marker ->
        empty()

      :form_list ->
        es = seq(:erl_syntax.form_list_elements(node), :none, reset_prec(ctxt), &lay/2)
        vertical_sep(text(''), es)

      :generator ->
        ctxt1 = reset_prec(ctxt)
        d1 = lay(:erl_syntax.generator_pattern(node), ctxt1)
        d2 = lay(:erl_syntax.generator_body(node), ctxt1)
        par([d1, beside(text('<- '), d2)], r_ctxt(ctxt1, :break_indent))

      :binary_generator ->
        ctxt1 = reset_prec(ctxt)

        d1 =
          lay(
            :erl_syntax.binary_generator_pattern(node),
            ctxt1
          )

        d2 = lay(:erl_syntax.binary_generator_body(node), ctxt1)
        par([d1, beside(text('<= '), d2)], r_ctxt(ctxt1, :break_indent))

      :implicit_fun ->
        d =
          lay(
            :erl_syntax.implicit_fun_name(node),
            reset_prec(ctxt)
          )

        beside(floating(text('fun ')), d)

      :list_comp ->
        ctxt1 = reset_prec(ctxt)
        d1 = lay(:erl_syntax.list_comp_template(node), ctxt1)
        d2 = par(seq(:erl_syntax.list_comp_body(node), floating(text(',')), ctxt1, &lay/2))

        beside(
          floating(text('[')),
          par([
            d1,
            beside(
              floating(text('|| ')),
              beside(d2, floating(text(']')))
            )
          ])
        )

      :binary_comp ->
        ctxt1 = reset_prec(ctxt)
        d1 = lay(:erl_syntax.binary_comp_template(node), ctxt1)
        d2 = par(seq(:erl_syntax.binary_comp_body(node), floating(text(',')), ctxt1, &lay/2))

        beside(
          floating(text('<< ')),
          par([
            d1,
            beside(
              floating(text(' || ')),
              beside(d2, floating(text(' >>')))
            )
          ])
        )

      :macro ->
        ctxt1 = reset_prec(ctxt)
        n = :erl_syntax.macro_name(node)

        d =
          case :erl_syntax.macro_arguments(node) do
            :none ->
              lay(n, ctxt1)

            args ->
              as = seq(args, floating(text(',')), set_prec(ctxt1, max_prec()), &lay/2)

              beside(
                lay(n, ctxt1),
                beside(text('('), beside(par(as), floating(text(')'))))
              )
          end

        d1 = beside(floating(text('?')), d)
        maybe_parentheses(d1, 0, ctxt)

      :parentheses ->
        d =
          lay(
            :erl_syntax.parentheses_body(node),
            reset_prec(ctxt)
          )

        lay_parentheses(d, ctxt)

      :receive_expr ->
        ctxt1 = reset_prec(ctxt)
        d1 = lay_clauses(:erl_syntax.receive_expr_clauses(node), :receive_expr, ctxt1)

        d2 =
          case :erl_syntax.receive_expr_timeout(node) do
            :none ->
              d1

            t ->
              d3 = lay(t, ctxt1)
              a = :erl_syntax.receive_expr_action(node)
              d4 = sep(seq(a, floating(text(',')), ctxt1, &lay/2))

              sep([
                d1,
                follow(
                  floating(text('after')),
                  append_clause_body(d4, d3, ctxt1),
                  r_ctxt(ctxt1, :break_indent)
                )
              ])
          end

        sep([text('receive'), nest(r_ctxt(ctxt1, :break_indent), d2), text('end')])

      :record_access ->
        {precL, prec, precR} = inop_prec(:"#")

        d1 =
          lay(
            :erl_syntax.record_access_argument(node),
            set_prec(ctxt, precL)
          )

        d2 =
          beside(
            floating(text('.')),
            lay(
              :erl_syntax.record_access_field(node),
              set_prec(ctxt, precR)
            )
          )

        t = :erl_syntax.record_access_type(node)

        d3 =
          beside(
            beside(
              floating(text('#')),
              lay(t, reset_prec(ctxt))
            ),
            d2
          )

        maybe_parentheses(beside(d1, d3), prec, ctxt)

      :record_expr ->
        {precL, prec, _} = inop_prec(:"#")
        ctxt1 = reset_prec(ctxt)
        d1 = lay(:erl_syntax.record_expr_type(node), ctxt1)
        d2 = par(seq(:erl_syntax.record_expr_fields(node), floating(text(',')), ctxt1, &lay/2))

        d3 =
          beside(
            beside(floating(text('#')), d1),
            beside(text('{'), beside(d2, floating(text('}'))))
          )

        d4 =
          case :erl_syntax.record_expr_argument(node) do
            :none ->
              d3

            a ->
              beside(lay(a, set_prec(ctxt, precL)), d3)
          end

        maybe_parentheses(d4, prec, ctxt)

      :record_field ->
        ctxt1 = reset_prec(ctxt)
        d1 = lay(:erl_syntax.record_field_name(node), ctxt1)

        case :erl_syntax.record_field_value(node) do
          :none ->
            d1

          v ->
            par(
              [d1, floating(text('=')), lay(v, ctxt1)],
              r_ctxt(ctxt1, :break_indent)
            )
        end

      :record_index_expr ->
        {prec, precR} = preop_prec(:"#")

        d1 =
          lay(
            :erl_syntax.record_index_expr_type(node),
            reset_prec(ctxt)
          )

        d2 =
          lay(
            :erl_syntax.record_index_expr_field(node),
            set_prec(ctxt, precR)
          )

        d3 =
          beside(
            beside(floating(text('#')), d1),
            beside(floating(text('.')), d2)
          )

        maybe_parentheses(d3, prec, ctxt)

      :map_expr ->
        {precL, prec, _} = inop_prec(:"#")
        ctxt1 = reset_prec(ctxt)
        d1 = par(seq(:erl_syntax.map_expr_fields(node), floating(text(',')), ctxt1, &lay/2))
        d2 = beside(text('\#{'), beside(d1, floating(text('}'))))

        d3 =
          case :erl_syntax.map_expr_argument(node) do
            :none ->
              d2

            a ->
              beside(lay(a, set_prec(ctxt, precL)), d2)
          end

        maybe_parentheses(d3, prec, ctxt)

      :map_field_assoc ->
        ctxt1 = reset_prec(ctxt)
        d1 = lay(:erl_syntax.map_field_assoc_name(node), ctxt1)
        d2 = lay(:erl_syntax.map_field_assoc_value(node), ctxt1)

        par(
          [d1, floating(text('=>')), d2],
          r_ctxt(ctxt1, :break_indent)
        )

      :map_field_exact ->
        ctxt1 = reset_prec(ctxt)
        d1 = lay(:erl_syntax.map_field_exact_name(node), ctxt1)
        d2 = lay(:erl_syntax.map_field_exact_value(node), ctxt1)

        par(
          [d1, floating(text(':=')), d2],
          r_ctxt(ctxt1, :break_indent)
        )

      :size_qualifier ->
        ctxt1 = set_prec(ctxt, max_prec())
        d1 = lay(:erl_syntax.size_qualifier_body(node), ctxt1)

        d2 =
          lay(
            :erl_syntax.size_qualifier_argument(node),
            ctxt1
          )

        beside(d1, beside(text(':'), d2))

      :text ->
        text(:erl_syntax.text_string(node))

      :typed_record_field ->
        {_, prec, _} = type_inop_prec(:"::")
        ctxt1 = reset_prec(ctxt)

        d1 =
          lay(
            :erl_syntax.typed_record_field_body(node),
            ctxt1
          )

        d2 =
          lay(
            :erl_syntax.typed_record_field_type(node),
            set_prec(ctxt, prec)
          )

        d3 =
          par(
            [d1, floating(text('::')), d2],
            r_ctxt(ctxt1, :break_indent)
          )

        maybe_parentheses(d3, prec, ctxt)

      :try_expr ->
        ctxt1 = reset_prec(ctxt)
        d1 = sep(seq(:erl_syntax.try_expr_body(node), floating(text(',')), ctxt1, &lay/2))
        es0 = [text('end')]

        es1 =
          case :erl_syntax.try_expr_after(node) do
            [] ->
              es0

            as ->
              d2 = sep(seq(as, floating(text(',')), ctxt1, &lay/2))
              [text('after'), nest(r_ctxt(ctxt1, :break_indent), d2) | es0]
          end

        es2 =
          case :erl_syntax.try_expr_handlers(node) do
            [] ->
              es1

            hs ->
              d3 = lay_clauses(hs, :try_expr, ctxt1)
              [text('catch'), nest(r_ctxt(ctxt1, :break_indent), d3) | es1]
          end

        es3 =
          case :erl_syntax.try_expr_clauses(node) do
            [] ->
              es2

            cs ->
              d4 = lay_clauses(cs, :try_expr, ctxt1)
              [text('of'), nest(r_ctxt(ctxt1, :break_indent), d4) | es2]
          end

        sep([
          par([follow(text('try'), d1, r_ctxt(ctxt1, :break_indent)), hd(es3)])
          | tl(es3)
        ])

      :warning_marker ->
        e = :erl_syntax.warning_marker_info(node)
        beside(text('%% WARNING: '), lay_error_info(e, reset_prec(ctxt)))

      :annotated_type ->
        {_, prec, _} = type_inop_prec(:"::")

        d1 =
          lay(
            :erl_syntax.annotated_type_name(node),
            reset_prec(ctxt)
          )

        d2 =
          lay(
            :erl_syntax.annotated_type_body(node),
            set_prec(ctxt, prec)
          )

        d3 = follow(beside(d1, floating(text(' ::'))), d2, r_ctxt(ctxt, :break_indent))
        maybe_parentheses(d3, prec, ctxt)

      :type_application ->
        name = :erl_syntax.type_application_name(node)
        arguments = :erl_syntax.type_application_arguments(node)

        case :erl_syntax_lib.analyze_type_application(node) do
          {nil, 0} ->
            text('[]')

          {:list, 1} ->
            [a] = arguments
            d1 = lay(a, reset_prec(ctxt))
            beside(text('['), beside(d1, text(']')))

          {:nonempty_list, 1} ->
            [a] = arguments
            d1 = lay(a, reset_prec(ctxt))
            beside(text('['), beside(d1, text(', ...]')))

          _ ->
            lay_type_application(name, arguments, ctxt)
        end

      :bitstring_type ->
        ctxt1 = set_prec(ctxt, max_prec())
        m = :erl_syntax.bitstring_type_m(node)
        n = :erl_syntax.bitstring_type_n(node)

        d1 =
          for _ <- [:EFE_DUMMY_GEN],
              :erl_syntax.type(m) !== :integer or :erl_syntax.integer_value(m) !== 0 do
            beside(text('_:'), lay(m, ctxt1))
          end

        d2 =
          for _ <- [:EFE_DUMMY_GEN],
              :erl_syntax.type(n) !== :integer or :erl_syntax.integer_value(n) !== 0 do
            beside(text('_:_*'), lay(n, ctxt1))
          end

        f = fn d, _ ->
          d
        end

        d = seq(d1 ++ d2, floating(text(',')), ctxt1, f)

        beside(
          floating(text('<<')),
          beside(par(d), floating(text('>>')))
        )

      :fun_type ->
        text('fun()')

      :constrained_function_type ->
        ctxt1 = reset_prec(ctxt)

        d1 =
          lay(
            :erl_syntax.constrained_function_type_body(node),
            ctxt1
          )

        ctxt2 = r_ctxt(ctxt1, clause: :undefined)

        d2 =
          lay(
            :erl_syntax.constrained_function_type_argument(node),
            ctxt2
          )

        beside(d1, beside(floating(text(' when ')), d2))

      :function_type ->
        {before, after__} =
          case r_ctxt(ctxt, :clause) do
            :spec ->
              {'', ''}

            _ ->
              {'fun(', ')'}
          end

        ctxt1 = r_ctxt(reset_prec(ctxt), clause: :undefined)

        d1 =
          case :erl_syntax.function_type_arguments(node) do
            :any_arity ->
              text('(...)')

            arguments ->
              as = seq(arguments, floating(text(',')), ctxt1, &lay/2)
              beside(text('('), beside(par(as), floating(text(')'))))
          end

        d2 = lay(:erl_syntax.function_type_return(node), ctxt1)

        beside(
          floating(text(before)),
          beside(
            d1,
            beside(
              floating(text(' -> ')),
              beside(d2, floating(text(after__)))
            )
          )
        )

      :constraint ->
        name = :erl_syntax.constraint_argument(node)
        args = :erl_syntax.constraint_body(node)

        case is_subtype(name, args) do
          true ->
            [var, type] = args
            {precL, prec, precR} = type_inop_prec(:"::")
            d1 = lay(var, set_prec(ctxt, precL))
            d2 = lay(type, set_prec(ctxt, precR))
            d3 = follow(beside(d1, floating(text(' ::'))), d2, r_ctxt(ctxt, :break_indent))
            maybe_parentheses(d3, prec, ctxt)

          false ->
            lay_type_application(name, args, ctxt)
        end

      :map_type ->
        case :erl_syntax.map_type_fields(node) do
          :any_size ->
            text('map()')

          fs ->
            ctxt1 = reset_prec(ctxt)
            es = seq(fs, floating(text(',')), ctxt1, &lay/2)

            d =
              beside(
                floating(text('\#{')),
                beside(par(es), floating(text('}')))
              )

            {prec, _PrecR} = type_preop_prec(:"#")
            maybe_parentheses(d, prec, ctxt)
        end

      :map_type_assoc ->
        name = :erl_syntax.map_type_assoc_name(node)
        value = :erl_syntax.map_type_assoc_value(node)
        lay_type_assoc(name, value, ctxt)

      :map_type_exact ->
        ctxt1 = reset_prec(ctxt)
        d1 = lay(:erl_syntax.map_type_exact_name(node), ctxt1)
        d2 = lay(:erl_syntax.map_type_exact_value(node), ctxt1)

        par(
          [d1, floating(text(':=')), d2],
          r_ctxt(ctxt1, :break_indent)
        )

      :integer_range_type ->
        {precL, prec, precR} = type_inop_prec(:..)

        d1 =
          lay(
            :erl_syntax.integer_range_type_low(node),
            set_prec(ctxt, precL)
          )

        d2 =
          lay(
            :erl_syntax.integer_range_type_high(node),
            set_prec(ctxt, precR)
          )

        d3 = beside(d1, beside(text('..'), d2))
        maybe_parentheses(d3, prec, ctxt)

      :record_type ->
        {prec, _PrecR} = type_preop_prec(:"#")

        d1 =
          beside(
            text('#'),
            lay(
              :erl_syntax.record_type_name(node),
              reset_prec(ctxt)
            )
          )

        es =
          seq(:erl_syntax.record_type_fields(node), floating(text(',')), reset_prec(ctxt), &lay/2)

        d2 =
          beside(
            d1,
            beside(text('{'), beside(par(es), floating(text('}'))))
          )

        maybe_parentheses(d2, prec, ctxt)

      :record_type_field ->
        ctxt1 = reset_prec(ctxt)

        d1 =
          lay(
            :erl_syntax.record_type_field_name(node),
            ctxt1
          )

        d2 =
          lay(
            :erl_syntax.record_type_field_type(node),
            ctxt1
          )

        par(
          [d1, floating(text('::')), d2],
          r_ctxt(ctxt1, :break_indent)
        )

      :tuple_type ->
        case :erl_syntax.tuple_type_elements(node) do
          :any_size ->
            text('tuple()')

          elements ->
            es = seq(elements, floating(text(',')), reset_prec(ctxt), &lay/2)

            beside(
              floating(text('{')),
              beside(sep(es), floating(text('}')))
            )
        end

      :type_union ->
        {_, prec, precR} = type_inop_prec(:|)

        es =
          sep(
            seq(
              :erl_syntax.type_union_types(node),
              floating(text(' |')),
              set_prec(ctxt, precR),
              &lay/2
            )
          )

        maybe_parentheses(es, prec, ctxt)

      :user_type_application ->
        lay_type_application(
          :erl_syntax.user_type_application_name(node),
          :erl_syntax.user_type_application_arguments(node),
          ctxt
        )
    end
  end

  defp attribute_type(node) do
    n = :erl_syntax.attribute_name(node)

    case (try do
            :erl_syntax.concrete(n)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :opaque ->
        :type

      :spec ->
        :spec

      :callback ->
        :spec

      :type ->
        :type

      :export_type ->
        :export_type

      :optional_callbacks ->
        :optional_callbacks

      _ ->
        n
    end
  end

  defp is_subtype(name, [var, _]) do
    :erl_syntax.is_atom(
      name,
      :is_subtype
    ) and :erl_syntax.type(var) === :variable
  end

  defp is_subtype(_, _) do
    false
  end

  defp unfold_function_names(ns) do
    f = fn {atom, arity} ->
      :erl_syntax.arity_qualifier(
        :erl_syntax.atom(atom),
        :erl_syntax.integer(arity)
      )
    end

    :erl_syntax.list(
      for n <- ns do
        f.(n)
      end
    )
  end

  defp dodge_macros(type) do
    f = fn t ->
      case :erl_syntax.type(t) do
        :macro ->
          var = :erl_syntax.macro_name(t)
          varName0 = :erl_syntax.variable_name(var)
          varName = :erlang.list_to_atom('?' ++ :erlang.atom_to_list(varName0))
          atom = :erl_syntax.atom(varName)
          atom

        _ ->
          t
      end
    end

    :erl_syntax_lib.map(f, type)
  end

  defp lay_parentheses(d, _Ctxt) do
    beside(floating(text('(')), beside(d, floating(text(')'))))
  end

  defp maybe_parentheses(d, prec, ctxt) do
    case r_ctxt(ctxt, :prec) do
      p when p > prec ->
        lay_parentheses(d, ctxt)

      _ ->
        d
    end
  end

  defp lay_string(s, ctxt) do
    w = div(r_ctxt(ctxt, :ribbon) * 2, 3)
    lay_string_1(s, length(s), w)
  end

  defp lay_string_1(s, l, w) when l > w and w > 0 do
    case split_string(s, w - 1, l) do
      {_S1, ''} ->
        text(s)

      {s1, s2} ->
        above(
          text(s1 ++ '"'),
          lay_string_1([?" | s2], l - w + 1, w)
        )
    end
  end

  defp lay_string_1(s, _L, _W) do
    text(s)
  end

  defp split_string(xs, n, l) do
    split_string_1(xs, n, l, [])
  end

  defp split_string_1([?\s | xs], n, l, as)
       when n <= 0 and
              l >= 5 do
    {:lists.reverse([?\s | as]), xs}
  end

  defp split_string_1([?\t | xs], n, l, as)
       when n <= 0 and
              l >= 5 do
    {:lists.reverse([?t, ?\\ | as]), xs}
  end

  defp split_string_1([?\n | xs], n, l, as)
       when n <= 0 and
              l >= 5 do
    {:lists.reverse([?n, ?\\ | as]), xs}
  end

  defp split_string_1([?\\ | xs], n, l, as) do
    split_string_2(xs, n - 1, l - 1, [?\\ | as])
  end

  defp split_string_1(xs, n, l, as) when n <= -10 and l >= 5 do
    {:lists.reverse(as), xs}
  end

  defp split_string_1([x | xs], n, l, as) do
    split_string_1(xs, n - 1, l - 1, [x | as])
  end

  defp split_string_1([], _N, _L, as) do
    {:lists.reverse(as), ''}
  end

  defp split_string_2([?^, x | xs], n, l, as) do
    split_string_1(xs, n - 2, l - 2, [x, ?^ | as])
  end

  defp split_string_2([?x, ?{ | xs], n, l, as) do
    split_string_3(xs, n - 2, l - 2, [?{, ?x | as])
  end

  defp split_string_2([x1, x2, x3 | xs], n, l, as)
       when x1 >= ?0 and
              x1 <= ?7 and x2 >= ?0 and
              x2 <= ?7 and x3 >= ?0 and
              x3 <= ?7 do
    split_string_1(xs, n - 3, l - 3, [x3, x2, x1 | as])
  end

  defp split_string_2([x1, x2 | xs], n, l, as)
       when x1 >= ?0 and
              x1 <= ?7 and x2 >= ?0 and
              x2 <= ?7 do
    split_string_1(xs, n - 2, l - 2, [x2, x1 | as])
  end

  defp split_string_2([x | xs], n, l, as) do
    split_string_1(xs, n - 1, l - 1, [x | as])
  end

  defp split_string_3([?} | xs], n, l, as) do
    split_string_1(xs, n - 1, l - 1, [?} | as])
  end

  defp split_string_3([x | xs], n, l, as)
       when (x >= ?0 and
               x <= ?9) or
              (x >= ?a and x <= ?z) or
              (x >= ?A and x <= ?Z) do
    split_string_3(xs, n - 1, l - 1, [x | as])
  end

  defp split_string_3([x | xs], n, l, as)
       when x >= ?0 and
              x <= ?9 do
    split_string_1(xs, n - 1, l - 1, [x | as])
  end

  defp lay_clauses(cs, type, ctxt) do
    vertical(seq(cs, floating(text(';')), r_ctxt(ctxt, clause: type), &lay/2))
  end

  defp make_fun_clause(p, g, b, ctxt) do
    make_fun_clause(:none, p, g, b, ctxt)
  end

  defp make_fun_clause(n, p, g, b, ctxt) do
    d = make_fun_clause_head(n, p, ctxt)
    make_case_clause(d, g, b, ctxt)
  end

  defp make_fun_clause_head(n, p, ctxt) do
    d = lay_parentheses(p, ctxt)

    cond do
      n === :none ->
        d

      true ->
        beside(n, d)
    end
  end

  defp make_case_clause(p, g, b, ctxt) do
    append_clause_body(b, append_guard(g, p, ctxt), ctxt)
  end

  defp make_if_clause(_P, g, b, ctxt) do
    g1 =
      cond do
        g === :none ->
          text('true')

        true ->
          g
      end

    append_clause_body(b, g1, ctxt)
  end

  defp append_clause_body(b, d, ctxt) do
    append_clause_body(b, d, floating(text(' ->')), ctxt)
  end

  defp append_clause_body(b, d, s, ctxt) do
    sep([beside(d, s), nest(r_ctxt(ctxt, :break_indent), b)])
  end

  defp append_guard(:none, d, _) do
    d
  end

  defp append_guard(g, d, ctxt) do
    par(
      [d, follow(text('when'), g, r_ctxt(ctxt, :sub_indent))],
      r_ctxt(ctxt, :break_indent)
    )
  end

  defp lay_bit_types([t], ctxt) do
    lay(t, ctxt)
  end

  defp lay_bit_types([t | ts], ctxt) do
    beside(
      lay(t, ctxt),
      beside(floating(text('-')), lay_bit_types(ts, ctxt))
    )
  end

  defp lay_error_info({l, m, t} = t0, ctxt)
       when is_integer(l) and
              is_atom(m) do
    case (try do
            m.format_error(t)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      s when is_list(s) ->
        cond do
          l > 0 ->
            beside(text(:io_lib.format('~w: ', [l])), text(s))

          true ->
            text(s)
        end

      _ ->
        lay_concrete(t0, ctxt)
    end
  end

  defp lay_error_info(t, ctxt) do
    lay_concrete(t, ctxt)
  end

  defp lay_concrete(t, ctxt) do
    lay(:erl_syntax.abstract(t), ctxt)
  end

  defp lay_type_assoc(name, value, ctxt) do
    ctxt1 = reset_prec(ctxt)
    d1 = lay(name, ctxt1)
    d2 = lay(value, ctxt1)

    par(
      [d1, floating(text('=>')), d2],
      r_ctxt(ctxt1, :break_indent)
    )
  end

  defp lay_type_application(name, arguments, ctxt) do
    {precL, prec} = func_prec()
    d1 = lay(name, set_prec(ctxt, precL))
    as = seq(arguments, floating(text(',')), reset_prec(ctxt), &lay/2)

    d =
      beside(
        d1,
        beside(text('('), beside(par(as), floating(text(')'))))
      )

    maybe_parentheses(d, prec, ctxt)
  end

  defp seq([h | t], separator, ctxt, fun) do
    case t do
      [] ->
        [fun.(h, ctxt)]

      _ ->
        [maybe_append(separator, fun.(h, ctxt)) | seq(t, separator, ctxt, fun)]
    end
  end

  defp seq([], _, _, _) do
    [empty()]
  end

  defp maybe_append(:none, d) do
    d
  end

  defp maybe_append(suffix, d) do
    beside(d, suffix)
  end

  defp vertical([d]) do
    d
  end

  defp vertical([d | ds]) do
    above(d, vertical(ds))
  end

  defp vertical([]) do
    []
  end

  defp vertical_sep(_Sep, [d]) do
    d
  end

  defp vertical_sep(sep, [d | ds]) do
    above(above(d, sep), vertical_sep(sep, ds))
  end

  defp vertical_sep(_Sep, []) do
    []
  end

  defp spaces(n) when n > 0 do
    [?\s | spaces(n - 1)]
  end

  defp spaces(_) do
    []
  end

  defp tidy_float([?., c | cs]) do
    [?., c | tidy_float_1(cs)]
  end

  defp tidy_float([?e | _] = cs) do
    tidy_float_2(cs)
  end

  defp tidy_float([c | cs]) do
    [c | tidy_float(cs)]
  end

  defp tidy_float([]) do
    []
  end

  defp tidy_float_1([?0, ?0, ?0 | cs]) do
    tidy_float_2(cs)
  end

  defp tidy_float_1([?e | _] = cs) do
    tidy_float_2(cs)
  end

  defp tidy_float_1([c | cs]) do
    [c | tidy_float_1(cs)]
  end

  defp tidy_float_1([]) do
    []
  end

  defp tidy_float_2([?e, ?+, ?0]) do
    []
  end

  defp tidy_float_2([?e, ?+, ?0 | cs]) do
    tidy_float_2([?e, ?+ | cs])
  end

  defp tidy_float_2([?e, ?+ | _] = cs) do
    cs
  end

  defp tidy_float_2([?e, ?-, ?0]) do
    []
  end

  defp tidy_float_2([?e, ?-, ?0 | cs]) do
    tidy_float_2([?e, ?- | cs])
  end

  defp tidy_float_2([?e, ?- | _] = cs) do
    cs
  end

  defp tidy_float_2([?e | cs]) do
    tidy_float_2([?e, ?+ | cs])
  end

  defp tidy_float_2([_C | cs]) do
    tidy_float_2(cs)
  end

  defp tidy_float_2([]) do
    []
  end

  defp lay_clause_expressions([h], ctxt) do
    lay(h, ctxt)
  end

  defp lay_clause_expressions([h | t], ctxt) do
    clause = beside(lay(h, ctxt), floating(text(',')))
    next = lay_clause_expressions(t, ctxt)

    case is_last_and_before_empty_line(h, t, ctxt) do
      true ->
        above(above(clause, text('')), next)

      false ->
        above(clause, next)
    end
  end

  defp lay_clause_expressions([], _) do
    empty()
  end

  defp is_last_and_before_empty_line(h, [], r_ctxt(empty_lines: emptyLines)) do
    try do
      :sets.is_element(:erl_syntax.get_pos(h) + 1, emptyLines)
    catch
      :error, :badarith ->
        false
    end
  end

  defp is_last_and_before_empty_line(h, [h2 | _], r_ctxt(empty_lines: emptyLines)) do
    try do
      :erlang.and(
        :erl_syntax.get_pos(h2) - :erl_syntax.get_pos(h) >= 2,
        :sets.is_element(
          :erl_syntax.get_pos(h) + 1,
          emptyLines
        )
      )
    catch
      :error, :badarith ->
        false
    end
  end
end
