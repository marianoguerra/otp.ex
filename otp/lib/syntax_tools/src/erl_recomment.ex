defmodule :m_erl_recomment do
  use Bitwise

  def quick_recomment_forms(tree, cs) do
    recomment_forms(tree, cs, false)
  end

  def recomment_forms(tree, cs) do
    recomment_forms(tree, cs, true)
  end

  defp recomment_forms(tree, cs, insert) when is_list(tree) do
    recomment_forms(:erl_syntax.form_list(tree), cs, insert)
  end

  defp recomment_forms(tree, cs, insert) do
    case :erl_syntax.type(tree) do
      :form_list ->
        tree1 = :erl_syntax.flatten_form_list(tree)
        node = build_tree(tree1)
        [node1] = node_subtrees(node)
        list = filter_forms(node_subtrees(node1))
        list1 = recomment_forms_1(cs, list, insert)

        revert_tree(
          set_node_subtrees(
            node,
            [set_node_subtrees(node1, list1)]
          )
        )

      _ ->
        {tree1, cs1} = recomment_tree(tree, cs)
        revert_tree(append_comments(cs1, tree1))
    end
  end

  defp append_comments([c | cs], tree) do
    append_comments(cs, node_add_postcomment(c, tree))
  end

  defp append_comments([], tree) do
    tree
  end

  defp recomment_forms_1([c | cs], ns, insert) do
    ns1 = recomment_forms_2(c, ns, insert)
    recomment_forms_1(cs, ns1, insert)
  end

  defp recomment_forms_1([], ns, _Insert) do
    ns
  end

  defp recomment_forms_2(c, [n | ns] = nodes, insert) do
    {l, col, ind, text} = c
    min = node_min(n)
    max = node_max(n)
    delta = comment_delta(text)

    trailing =
      case ns do
        [] ->
          true

        [next | _] ->
          l + delta < node_min(next) - 2
      end

    cond do
      l > max + 1 or (l === max + 1 and not trailing) ->
        [n | recomment_forms_2(c, ns, insert)]

      l + delta < min - 1 ->
        [standalone_comment(c) | nodes]

      l < min ->
        [node_add_precomment(c, n) | ns]

      col <= 1 and l <= min and l + delta >= min ->
        n1 = standalone_comment(c)

        cond do
          l < min ->
            [n1 | nodes]

          true ->
            [[n, n1] | ns]
        end

      insert === true ->
        [insert(n, l, col, ind, c) | ns]

      true ->
        nodes
    end
  end

  defp recomment_forms_2(c, [], _Top) do
    [standalone_comment(c)]
  end

  defp standalone_comment({l, col, _Ind, text}) do
    leaf_node(
      l,
      l + comment_delta(text),
      :erl_syntax.set_pos(
        :erl_syntax.comment(col - 1, text),
        l
      )
    )
  end

  defp comment_delta(text) do
    case length(text) do
      n when n > 0 ->
        n - 1

      _ ->
        0
    end
  end

  require Record
  Record.defrecord(:r_filter, :filter, file: :undefined, line: 0)

  defp filter_forms(fs) do
    filter_forms(fs, false, r_filter())
  end

  defp filter_forms([f | fs], kill, s) do
    case check_file_attr(f) do
      {true, a1, a2} ->
        s1 =
          case r_filter(s, :file) do
            :undefined ->
              r_filter(s, file: a1, line: a2)

            _ ->
              s
          end

        cond do
          r_filter(s1, :file) === a1 and r_filter(s1, :line) <= a2 ->
            [f | filter_forms(fs, false, r_filter(s1, line: a2))]

          kill === true ->
            [node_kill_range(f) | filter_forms(fs, true, s1)]

          true ->
            [f | filter_forms(fs, true, s1)]
        end

      false ->
        case kill do
          true ->
            [node_kill_range(f) | filter_forms(fs, kill, s)]

          false ->
            [f | filter_forms(fs, kill, s)]
        end
    end
  end

  defp filter_forms([], _, _) do
    []
  end

  defp check_file_attr(f) do
    case node_type(f) do
      :tree_node ->
        case tree_node_type(f) do
          :attribute ->
            case node_subtrees(f) do
              [[l1, l2] | _] ->
                check_file_attr_1(l1, l2)

              _ ->
                false
            end

          _ ->
            false
        end

      _ ->
        false
    end
  end

  defp check_file_attr_1(l1, l2) do
    case node_subtrees(l1) do
      [n1 | _] ->
        n2 = leaf_node_value(n1)

        case :erl_syntax.type(n2) do
          :atom ->
            case :erl_syntax.atom_value(n2) do
              :file ->
                check_file_attr_2(l2)

              _ ->
                false
            end

          _ ->
            false
        end

      _ ->
        false
    end
  end

  defp check_file_attr_2(l) do
    case node_subtrees(l) do
      [[n1, n2] | _] ->
        t1 = :erl_syntax.concrete(revert_tree(n1))
        t2 = :erl_syntax.concrete(revert_tree(n2))
        {true, t1, t2}

      _ ->
        false
    end
  end

  def recomment_tree(tree, cs) do
    {tree1, cs1} = insert_comments(cs, build_tree(tree))
    {revert_tree(tree1), cs1}
  end

  defp insert_comments(cs, node) do
    insert_comments(cs, node, [])
  end

  defp insert_comments([c | cs], node, cs1) do
    {l, col, ind, _Text} = c
    max = node_max(node)

    cond do
      l <= max ->
        insert_comments(cs, insert(node, l, col, ind, c), cs1)

      true ->
        insert_comments(cs, node, [c | cs1])
    end
  end

  defp insert_comments([], node, cs) do
    {node, :lists.reverse(cs)}
  end

  defp insert(node, l, col, ind, c) do
    case node_type(node) do
      :list_node ->
        set_node_subtrees(
          node,
          insert_in_list(node_subtrees(node), l, col, ind, c)
        )

      _ ->
        min = node_min(node)
        max = node_max(node)

        cond do
          l < min ->
            node_add_precomment(c, node)

          min === max ->
            node_add_postcomment(c, node)

          true ->
            insert_1(node, l, col, ind, c)
        end
    end
  end

  defp insert_1(node, l, col, ind, c) do
    case node_type(node) do
      :tree_node ->
        set_node_subtrees(
          node,
          insert_in_list(node_subtrees(node), l, col, ind, c)
        )

      :leaf_node ->
        node_add_postcomment(c, node)
    end
  end

  defp insert_in_list([node | ns], l, col, ind, c) do
    max = node_max(node)
    nextMin = next_min_in_list(ns)

    cond do
      nextMin < 0 ->
        insert_here(node, l, col, ind, c, ns)

      l >= nextMin and nextMin >= max ->
        insert_later(node, l, col, ind, c, ns)

      l <= max ->
        insert_here(node, l, col, ind, c, ns)

      true ->
        insert_later(node, l, col, ind, c, ns)
    end
  end

  defp insert_in_list([], l, col, _, _) do
    exit({:bad_tree, l, col})
  end

  defp insert_here(node, l, col, ind, c, ns) do
    [insert(node, l, col, ind, c) | ns]
  end

  defp insert_later(node, l, col, ind, c, ns) do
    [node | insert_in_list(ns, l, col, ind, c)]
  end

  defp next_min_in_list(ts) do
    next_min_in_list(ts, [])
  end

  defp next_min_in_list([t | ts], ack) do
    next_min_in_node(t, [ts | ack])
  end

  defp next_min_in_list([], [t | ts]) do
    next_min_in_list(t, ts)
  end

  defp next_min_in_list([], []) do
    -1
  end

  defp next_min_in_node(node, ack) do
    case node_type(node) do
      :leaf_node ->
        node_min(node)

      :tree_node ->
        node_min(node)

      :list_node ->
        next_min_in_list(node_subtrees(node), ack)
    end
  end

  defp build_tree(node) do
    l = get_line(node)

    case :erl_syntax.subtrees(node) do
      [] ->
        leaf_node(l, l, node)

      ts ->
        {subtrees, min, max} = build_list_list(ts)

        tree_node(
          minpos(l, min),
          :erlang.max(l, max),
          :erl_syntax.type(node),
          :erl_syntax.get_attrs(node),
          subtrees
        )
    end
  end

  defp build_list(ts) do
    build_list(ts, 0, 0, [])
  end

  defp build_list([t | ts], min, max, ack) do
    node = build_tree(t)
    min1 = minpos(node_min(node), min)
    max1 = :erlang.max(node_max(node), max)
    build_list(ts, min1, max1, [node | ack])
  end

  defp build_list([], min, max, ack) do
    list_node(min, max, :lists.reverse(ack))
  end

  defp build_list_list(ls) do
    build_list_list(ls, 0, 0, [])
  end

  defp build_list_list([l | ls], min, max, ack) do
    node = build_list(l)
    min1 = minpos(node_min(node), min)
    max1 = :erlang.max(node_max(node), max)
    build_list_list(ls, min1, max1, [node | ack])
  end

  defp build_list_list([], min, max, ack) do
    {:lists.reverse(ack), min, max}
  end

  defp revert_tree(node) do
    case node_type(node) do
      :leaf_node ->
        add_comments(node, leaf_node_value(node))

      :tree_node ->
        add_comments(
          node,
          :erl_syntax.set_attrs(
            :erl_syntax.make_tree(
              tree_node_type(node),
              revert_list(node_subtrees(node))
            ),
            tree_node_attrs(node)
          )
        )

      :list_node ->
        revert_list(node_subtrees(node))
    end
  end

  defp revert_list([t | ts]) do
    [revert_tree(t) | revert_list(ts)]
  end

  defp revert_list([]) do
    []
  end

  defp add_comments(node, tree) do
    case node_precomments(node) do
      [] ->
        add_comments_1(node, tree)

      cs ->
        cs1 = :lists.reverse(expand_comments(cs))

        add_comments_1(
          node,
          :erl_syntax.add_precomments(cs1, tree)
        )
    end
  end

  defp add_comments_1(node, tree) do
    case node_postcomments(node) do
      [] ->
        tree

      cs ->
        cs1 = :lists.reverse(expand_comments(cs))
        :erl_syntax.add_postcomments(cs1, tree)
    end
  end

  defp expand_comments([c | cs]) do
    [expand_comment(c) | expand_comments(cs)]
  end

  defp expand_comments([]) do
    []
  end

  defp expand_comment(c) do
    {l, _Col, ind, text} = c
    :erl_syntax.set_pos(:erl_syntax.comment(ind, text), l)
  end

  Record.defrecord(:r_leaf, :leaf,
    min: 0,
    max: 0,
    precomments: [],
    postcomments: [],
    value: :undefined
  )

  Record.defrecord(:r_tree, :tree,
    min: 0,
    max: 0,
    type: :undefined,
    attrs: :undefined,
    precomments: [],
    postcomments: [],
    subtrees: []
  )

  Record.defrecord(:r_list, :list, min: 0, max: 0, subtrees: [])

  defp leaf_node(min, max, value) do
    r_leaf(min: min, max: max, value: value)
  end

  defp tree_node(min, max, type, attrs, subtrees) do
    r_tree(min: min, max: max, type: type, attrs: attrs, subtrees: subtrees)
  end

  defp list_node(min, max, subtrees) do
    r_list(min: min, max: max, subtrees: subtrees)
  end

  defp node_type(r_leaf()) do
    :leaf_node
  end

  defp node_type(r_tree()) do
    :tree_node
  end

  defp node_type(r_list()) do
    :list_node
  end

  defp node_min(r_leaf(min: min)) do
    min
  end

  defp node_min(r_tree(min: min)) do
    min
  end

  defp node_min(r_list(min: min)) do
    min
  end

  defp node_max(r_leaf(max: max)) do
    max
  end

  defp node_max(r_tree(max: max)) do
    max
  end

  defp node_max(r_list(max: max)) do
    max
  end

  defp node_kill_range(node) do
    case node do
      r_leaf() ->
        r_leaf(node, min: -1, max: -1)

      r_tree() ->
        r_tree(node, min: -1, max: -1)

      r_list() ->
        r_list(node, min: -1, max: -1)
    end
  end

  defp node_precomments(r_leaf(precomments: cs)) do
    cs
  end

  defp node_precomments(r_tree(precomments: cs)) do
    cs
  end

  defp node_add_precomment(c, node) do
    case node do
      r_leaf() ->
        r_leaf(node, precomments: [c | r_leaf(node, :precomments)])

      r_tree() ->
        r_tree(node, precomments: [c | r_tree(node, :precomments)])
    end
  end

  defp node_postcomments(r_leaf(postcomments: cs)) do
    cs
  end

  defp node_postcomments(r_tree(postcomments: cs)) do
    cs
  end

  defp node_add_postcomment(c, node) do
    case node do
      r_leaf() ->
        r_leaf(node, postcomments: [c | r_leaf(node, :postcomments)])

      r_tree() ->
        r_tree(node, postcomments: [c | r_tree(node, :postcomments)])
    end
  end

  defp node_subtrees(r_tree(subtrees: subtrees)) do
    subtrees
  end

  defp node_subtrees(r_list(subtrees: subtrees)) do
    subtrees
  end

  defp leaf_node_value(r_leaf(value: value)) do
    value
  end

  defp tree_node_type(r_tree(type: type)) do
    type
  end

  defp set_node_subtrees(node, subtrees) do
    case node do
      r_tree() ->
        r_tree(node, subtrees: subtrees)

      r_list() ->
        r_list(node, subtrees: subtrees)
    end
  end

  defp tree_node_attrs(r_tree(attrs: attrs)) do
    attrs
  end

  defp minpos(x, y) when x < y do
    minpos1(x, y)
  end

  defp minpos(x, y) do
    minpos1(y, x)
  end

  defp minpos1(x, y) when x < 1 do
    minpos2(y)
  end

  defp minpos1(x, _) do
    x
  end

  defp minpos2(x) when x < 1 do
    0
  end

  defp minpos2(x) do
    x
  end

  defp get_line(node) do
    case :erl_syntax.get_pos(node) do
      l when is_integer(l) ->
        l

      {l, _} when is_integer(l) ->
        l

      {_, l} when is_integer(l) ->
        l

      {l, _, _} when is_integer(l) ->
        l

      {_, l, _} when is_integer(l) ->
        l

      pos ->
        try do
          :erl_anno.line(pos)
        catch
          _, _ ->
            exit({:bad_position, pos})
        else
          line ->
            line
        end
    end
  end
end
