defmodule :m_xmerl_xpath do
  use Bitwise
  import Kernel, except: [to_string: 1]
  require Record

  Record.defrecord(:r_xmlDecl, :xmlDecl,
    vsn: :undefined,
    encoding: :undefined,
    standalone: :undefined,
    attributes: :undefined
  )

  Record.defrecord(:r_xmlAttribute, :xmlAttribute,
    name: :undefined,
    expanded_name: [],
    nsinfo: [],
    namespace: [],
    parents: [],
    pos: :undefined,
    language: [],
    value: :undefined,
    normalized: :undefined
  )

  Record.defrecord(:r_xmlNamespace, :xmlNamespace,
    default: [],
    nodes: []
  )

  Record.defrecord(:r_xmlNsNode, :xmlNsNode,
    parents: [],
    pos: :undefined,
    prefix: :undefined,
    uri: []
  )

  Record.defrecord(:r_xmlElement, :xmlElement,
    name: :undefined,
    expanded_name: [],
    nsinfo: [],
    namespace: :EFE_TODO_NESTED_RECORD,
    parents: [],
    pos: :undefined,
    attributes: [],
    content: [],
    language: '',
    xmlbase: '',
    elementdef: :undeclared
  )

  Record.defrecord(:r_xmlText, :xmlText,
    parents: [],
    pos: :undefined,
    language: [],
    value: :undefined,
    type: :text
  )

  Record.defrecord(:r_xmlComment, :xmlComment,
    parents: [],
    pos: :undefined,
    language: [],
    value: :undefined
  )

  Record.defrecord(:r_xmlPI, :xmlPI,
    name: :undefined,
    parents: [],
    pos: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_xmlDocument, :xmlDocument, content: :undefined)

  Record.defrecord(:r_xmlContext, :xmlContext,
    axis_type: :forward,
    context_node: :undefined,
    context_position: 1,
    nodeset: [],
    bindings: [],
    functions: [],
    namespace: [],
    whole_document: :undefined
  )

  Record.defrecord(:r_xmlNode, :xmlNode, type: :element, node: :undefined, parents: [], pos: 1)

  Record.defrecord(:r_xmlObj, :xmlObj,
    type: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_xmerl_fun_states, :xmerl_fun_states,
    event: :undefined,
    hook: :undefined,
    rules: :undefined,
    fetch: :undefined,
    cont: :undefined
  )

  Record.defrecord(:r_xmerl_scanner, :xmerl_scanner,
    encoding: :undefined,
    standalone: :no,
    environment: :prolog,
    declarations: [],
    doctype_name: :undefined,
    doctype_DTD: :internal,
    comments: true,
    document: false,
    default_attrs: false,
    rules: :undefined,
    keep_rules: false,
    namespace_conformant: false,
    xmlbase: :undefined,
    xmlbase_cache: :undefined,
    fetch_path: [],
    filename: :file_name_unknown,
    validation: :off,
    schemaLocation: [],
    space: :preserve,
    event_fun: :undefined,
    hook_fun: :undefined,
    acc_fun: :undefined,
    fetch_fun: :undefined,
    close_fun: :undefined,
    continuation_fun: :undefined,
    rules_read_fun: :undefined,
    rules_write_fun: :undefined,
    rules_delete_fun: :undefined,
    user_state: :undefined,
    fun_states: :EFE_TODO_NESTED_RECORD,
    entity_references: [],
    text_decl: false,
    quiet: false,
    col: 1,
    line: 1,
    common_data: []
  )

  Record.defrecord(:r_xmerl_event, :xmerl_event,
    event: :undefined,
    line: :undefined,
    col: :undefined,
    pos: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_state, :state,
    context: :EFE_TODO_NESTED_RECORD,
    acc: []
  )

  def string(str, doc) do
    string(str, doc, [])
  end

  def string(str, doc, options) do
    string(str, doc, [], doc, options)
  end

  def string(str, node, parents, doc, options) do
    fullParents =
      case parents do
        [] ->
          []

        [{h, p} | _] when is_atom(h) and is_integer(p) ->
          full_parents(parents, doc)
      end

    contextNode = r_xmlNode(type: node_type(node), node: node, parents: fullParents)
    wholeDoc = whole_document(doc)

    context =
      r_xmlContext(new_context(options),
        context_node: contextNode,
        whole_document: wholeDoc
      )

    r_state(context: newContext) = match(str, r_state(context: context))

    case r_xmlContext(newContext, :nodeset) do
      scalObj = r_xmlObj(type: scalar)
      when scalar == :boolean or
             scalar == :number or scalar == :string ->
        scalObj

      r_xmlObj(type: :nodeset, value: nodeSet) ->
        nodeSet

      _ ->
        for r_xmlNode(node: n) <- r_xmlContext(newContext, :nodeset) do
          n
        end
    end
  end

  defp whole_document(r_xmlDocument() = doc) do
    r_xmlNode(type: :root_node, node: doc, parents: [])
  end

  defp whole_document(other) do
    r_xmlNode(type: :root_node, node: r_xmlDocument(content: other), parents: [])
  end

  defp new_context(options) do
    new_context(options, r_xmlContext())
  end

  defp new_context([{:namespace, r_xmlNamespace(nodes: nodes)} | t], c) do
    new_context(t, r_xmlContext(c, namespace: ns_nodes(nodes)))
  end

  defp new_context([{:namespace, nodes} | t], c) do
    new_context(t, r_xmlContext(c, namespace: ns_nodes(nodes)))
  end

  defp new_context([{:bindings, bs} | t], c) do
    new_context(t, r_xmlContext(c, bindings: bs))
  end

  defp new_context([{:functions, fs} | t], c) do
    new_context(t, r_xmlContext(c, functions: fs))
  end

  defp new_context([], c) do
    c
  end

  defp ns_nodes([{prefix, uRI} | t]) do
    [{to_string(prefix), to_atom(uRI)} | ns_nodes(t)]
  end

  defp ns_nodes([]) do
    []
  end

  defp full_parents(ps, doc) do
    full_parents1(:lists.reverse(ps), [doc], [])
  end

  defp full_parents1([{name, pos} | ns], content, parents) do
    e = locate_element(name, pos, content)
    pN = r_xmlNode(type: :element, node: e, parents: parents)
    full_parents1(ns, get_content(e), [pN | parents])
  end

  defp full_parents1([], _E, parents) do
    parents
  end

  defp locate_element(name, pos, [e = r_xmlElement(name: name, pos: pos) | _]) do
    e
  end

  defp locate_element(_Name, pos, [r_xmlElement(pos: p) | _]) when p >= pos do
    exit(:invalid_parents)
  end

  defp locate_element(_Name, _Pos, []) do
    exit(:invalid_parents)
  end

  defp locate_element(name, pos, [_ | t]) do
    locate_element(name, pos, t)
  end

  defp match(str, s = r_state()) do
    tokens = :xmerl_xpath_scan.tokens(str)

    case :xmerl_xpath_parse.parse(tokens) do
      {:ok, expr} ->
        match_expr(expr, s)

      error ->
        error
    end
  end

  defp match_expr({:path, type, arg}, s) do
    eval_path(type, arg, r_state(s, :context))
  end

  defp match_expr(primExpr, s) do
    eval_primary_expr(primExpr, s)
  end

  defp path_expr({:refine, stepExpr1, stepExpr2}, s) do
    :no_debug
    :no_debug
    s1 = path_expr(stepExpr1, s)
    :no_debug
    path_expr(stepExpr2, s1)
  end

  defp path_expr(
         {:step, {axis, nodeTest, predExpr}},
         s = r_state(context: c, acc: acc)
       ) do
    :no_debug
    newContext = axis(axis, nodeTest, c, acc)
    pred_expr(predExpr, r_state(s, context: newContext))
  end

  defp path_expr(:/, s) do
    s
  end

  defp pred_expr([], s) do
    s
  end

  defp pred_expr([{:pred, pred} | preds], s = r_state()) do
    :no_debug
    newS = eval_pred(pred, s)
    pred_expr(preds, newS)
  end

  defp eval_pred(
         {:number, n0},
         s =
           r_state(
             context:
               c =
                 r_xmlContext(
                   nodeset: nS,
                   axis_type: axisType
                 )
           )
       ) do
    len = length(nS)

    case len >= n0 do
      true ->
        n =
          case axisType do
            :forward ->
              n0

            :reverse ->
              len + 1 - n0
          end

        newNodeSet = [:lists.nth(n, nS)]
        newContext = r_xmlContext(c, nodeset: newNodeSet)
        r_state(s, context: newContext)

      false ->
        r_state(s, context: r_xmlContext(c, nodeset: []))
    end
  end

  defp eval_pred(
         predicate,
         s = r_state(context: c = r_xmlContext(nodeset: nodeSet))
       ) do
    newNodeSet =
      :lists.filter(
        fn node ->
          thisContext = r_xmlContext(c, context_node: node)

          :xmerl_xpath_pred.eval(
            predicate,
            thisContext
          )
        end,
        nodeSet
      )

    newContext = r_xmlContext(c, nodeset: newNodeSet)
    r_state(s, context: newContext)
  end

  def write_node(r_xmlNode(pos: pos, node: r_xmlAttribute(name: name, parents: ps))) do
    {:attribute, pos, name, ps}
  end

  def write_node(r_xmlNode(pos: pos, node: r_xmlElement(name: name, parents: ps))) do
    {:element, pos, name, ps}
  end

  def write_node(r_xmlNode(pos: pos, node: r_xmlText(value: txt, parents: ps))) do
    {:text, pos, txt, ps}
  end

  def write_node(r_xmlNode(pos: pos, node: r_xmlComment(parents: ps))) do
    {:comment, pos, :"", ps}
  end

  def write_node(r_xmlNode(pos: pos, node: r_xmlPI(name: name, parents: ps))) do
    {:processing_instruction, pos, name, ps}
  end

  def write_node(
        r_xmlNode(
          pos: pos,
          node: r_xmlNsNode(parents: ps, prefix: prefix)
        )
      ) do
    {:namespace, pos, prefix, ps}
  end

  def write_node(_) do
    :other
  end

  def eval_path(:union, {pathExpr1, pathExpr2}, c = r_xmlContext()) do
    s = r_state(context: c)
    s1 = match_expr(pathExpr1, s)
    s2 = match_expr(pathExpr2, r_state(s1, context: c))
    nodeSet1 = r_xmlContext(r_state(s1, :context), :nodeset)
    nodeSet2 = r_xmlContext(r_state(s2, :context), :nodeset)

    newNodeSet =
      :ordsets.to_list(
        :ordsets.union(
          :ordsets.from_list(nodeSet1),
          :ordsets.from_list(nodeSet2)
        )
      )

    r_state(s2, context: r_xmlContext(r_state(s2, :context), nodeset: newNodeSet))
  end

  def eval_path(:abs, pathExpr, c = r_xmlContext()) do
    nodeSet = [r_xmlContext(c, :whole_document)]
    context = r_xmlContext(c, nodeset: nodeSet)
    s = r_state(context: context)
    path_expr(pathExpr, s)
  end

  def eval_path(:rel, pathExpr, c = r_xmlContext()) do
    nodeSet = [r_xmlContext(c, :context_node)]
    context = r_xmlContext(c, nodeset: nodeSet)
    s = r_state(context: context)
    path_expr(pathExpr, s)
  end

  def eval_path(:filter, {pathExpr, {:pred, pred}}, c = r_xmlContext()) do
    s = r_state(context: c)
    s1 = match_expr(pathExpr, s)
    eval_pred(pred, s1)
  end

  defp eval_primary_expr(primExpr, s = r_state(context: context)) do
    newNodeSet = :xmerl_xpath_lib.eval(:primary_expr, primExpr, context)
    newContext = r_xmlContext(context, nodeset: newNodeSet)
    r_state(s, context: newContext)
  end

  def axis(axis, nodeTest, context) do
    axis(axis, nodeTest, context, [])
  end

  def axis(axis, nodeTest, context = r_xmlContext(nodeset: nS0), acc) do
    newNodeSet =
      :lists.foldr(
        fn n, accX ->
          axis1(axis, nodeTest, n, accX, context)
        end,
        acc,
        nS0
      )

    update_nodeset(
      fwd_or_reverse(axis, context),
      newNodeSet
    )
  end

  defp axis1(:self, tok, n, acc, context) do
    match_self(tok, n, acc, context)
  end

  defp axis1(:descendant, tok, n, acc, context) do
    match_descendant(tok, n, acc, context)
  end

  defp axis1(:child, tok, n, acc, context) do
    match_child(tok, n, acc, context)
  end

  defp axis1(:parent, tok, n, acc, context) do
    match_parent(tok, n, acc, context)
  end

  defp axis1(:ancestor, tok, n, acc, context) do
    match_ancestor(tok, n, acc, context)
  end

  defp axis1(:following_sibling, tok, n, acc, context) do
    match_following_sibling(tok, n, acc, context)
  end

  defp axis1(:preceding_sibling, tok, n, acc, context) do
    match_preceding_sibling(tok, n, acc, context)
  end

  defp axis1(:following, tok, n, acc, context) do
    match_following(tok, n, acc, context)
  end

  defp axis1(:preceding, tok, n, acc, context) do
    match_preceding(tok, n, acc, context)
  end

  defp axis1(:attribute, tok, n, acc, context) do
    match_attribute(tok, n, acc, context)
  end

  defp axis1(:namespace, tok, n, acc, context) do
    match_namespace(tok, n, acc, context)
  end

  defp axis1(:ancestor_or_self, tok, n, acc, context) do
    match_ancestor_or_self(tok, n, acc, context)
  end

  defp axis1(:descendant_or_self, tok, n, acc, context) do
    match_descendant_or_self(tok, n, acc, context)
  end

  defp fwd_or_reverse(:ancestor, context) do
    reverse_axis(context)
  end

  defp fwd_or_reverse(:ancestor_or_self, context) do
    reverse_axis(context)
  end

  defp fwd_or_reverse(:preceding_sibling, context) do
    reverse_axis(context)
  end

  defp fwd_or_reverse(:preceding, context) do
    reverse_axis(context)
  end

  defp fwd_or_reverse(_, context) do
    forward_axis(context)
  end

  defp reverse_axis(context) do
    r_xmlContext(context, axis_type: :reverse)
  end

  defp forward_axis(context) do
    r_xmlContext(context, axis_type: :forward)
  end

  defp match_self(tok, n, acc, context) do
    case node_test(tok, n, context) do
      true ->
        [n | acc]

      false ->
        acc
    end
  end

  defp match_descendant(tok, n, acc, context) do
    r_xmlNode(parents: ps, node: node, type: type) = n

    case type do
      el when el == :element or el == :root_node ->
        newPs = [n | ps]
        match_desc(get_content(node), newPs, tok, acc, context)

      _Other ->
        acc
    end
  end

  defp match_desc([e = r_xmlElement() | t], parents, tok, acc, context) do
    acc1 = match_desc(t, parents, tok, acc, context)
    n = r_xmlNode(type: node_type(e), node: e, parents: parents)
    newParents = [n | parents]
    acc2 = match_desc(get_content(e), newParents, tok, acc1, context)
    match_self(tok, n, acc2, context)
  end

  defp match_desc([e | t], parents, tok, acc, context) do
    acc1 = match_desc(t, parents, tok, acc, context)
    n = r_xmlNode(node: e, type: node_type(e), parents: parents)
    match_self(tok, n, acc1, context)
  end

  defp match_desc([], _Parents, _Tok, acc, _Context) do
    acc
  end

  defp match_descendant_or_self(tok, n, acc, context) do
    acc1 = match_descendant(tok, n, acc, context)
    match_self(tok, n, acc1, context)
  end

  defp match_child(tok, n, acc, context) do
    r_xmlNode(parents: ps, node: node, type: type) = n

    case type do
      el when el == :element or el == :root_node ->
        newPs = [n | ps]

        :lists.foldr(
          fn e, accX ->
            thisN = r_xmlNode(type: node_type(e), node: e, parents: newPs)
            match_self(tok, thisN, accX, context)
          end,
          acc,
          get_content(node)
        )

      _Other ->
        acc
    end
  end

  defp match_parent(tok, n, acc, context) do
    case r_xmlNode(n, :parents) do
      [] ->
        acc

      [pN | _] ->
        match_self(tok, pN, acc, context)
    end
  end

  defp match_ancestor(tok, n, acc, context) do
    parents = r_xmlNode(n, :parents)

    :lists.foldl(
      fn pN, accX ->
        match_self(tok, pN, accX, context)
      end,
      acc,
      parents
    )
  end

  defp match_ancestor_or_self(tok, n, acc, context) do
    acc1 = match_self(tok, n, acc, context)
    match_ancestor(tok, n, acc1, context)
  end

  defp match_following_sibling(_Tok, r_xmlAttribute(), acc, _Context) do
    acc
  end

  defp match_following_sibling(_Tok, r_xmlNamespace(), acc, _Context) do
    acc
  end

  defp match_following_sibling(tok, n, acc, context) do
    r_xmlNode(parents: ps, node: node) = n

    case ps do
      [r_xmlNode(type: :element, node: r_xmlElement() = pNode) | _] ->
        followingSiblings =
          :lists.nthtail(
            get_position(node),
            get_content(pNode)
          )

        :lists.foldr(
          fn e, accX ->
            thisN = r_xmlNode(type: node_type(e), node: e, parents: ps)
            match_self(tok, thisN, accX, context)
          end,
          acc,
          followingSiblings
        )

      _Other ->
        acc
    end
  end

  defp match_following(tok, n, acc, context) do
    r_xmlNode(parents: ps, node: node) = n

    case ps do
      [r_xmlNode(type: :element, node: r_xmlElement() = pNode) = p | _] ->
        followingSiblings =
          :lists.nthtail(
            get_position(node),
            get_content(pNode)
          )

        acc0 = match_following(tok, p, acc, context)

        :lists.foldr(
          fn e, accX ->
            thisN = r_xmlNode(type: node_type(e), node: e, parents: ps)
            match_descendant_or_self(tok, thisN, accX, context)
          end,
          acc0,
          followingSiblings
        )

      _Other ->
        acc
    end
  end

  defp match_preceding_sibling(_Tok, r_xmlAttribute(), acc, _Context) do
    acc
  end

  defp match_preceding_sibling(_Tok, r_xmlNamespace(), acc, _Context) do
    acc
  end

  defp match_preceding_sibling(tok, n, acc, context) do
    r_xmlNode(parents: ps, node: node) = n

    case ps do
      [r_xmlNode(type: :element, node: r_xmlElement() = pNode) | _] ->
        precedingSiblings = :lists.sublist(get_content(pNode), 1, get_position(node) - 1)

        :lists.foldr(
          fn e, accX ->
            thisN = r_xmlNode(type: node_type(e), node: e, parents: ps)
            match_self(tok, thisN, accX, context)
          end,
          acc,
          precedingSiblings
        )

      _Other ->
        acc
    end
  end

  defp match_preceding(tok, n, acc, context) do
    r_xmlNode(parents: ps, node: node) = n

    case ps do
      [r_xmlNode(type: :element, node: r_xmlElement() = pNode) = p | _] ->
        precedingSiblings = :lists.sublist(get_content(pNode), 1, get_position(node) - 1)

        acc0 =
          :lists.foldr(
            fn e, accX ->
              thisN = r_xmlNode(type: node_type(e), node: e, parents: ps)
              match_descendant_or_self(tok, thisN, accX, context)
            end,
            acc,
            precedingSiblings
          )

        match_preceding(tok, p, acc0, context)

      _Other ->
        acc
    end
  end

  defp match_attribute(tok, n, acc, context) do
    case r_xmlNode(n, :type) do
      :element ->
        r_xmlNode(parents: ps, node: e) = n

        :lists.foldr(
          fn a, accX ->
            thisN = r_xmlNode(type: :attribute, node: a, parents: [n | ps])
            match_self(tok, thisN, accX, context)
          end,
          acc,
          r_xmlElement(e, :attributes)
        )

      _Other ->
        acc
    end
  end

  defp node_type(r_xmlAttribute()) do
    :attribute
  end

  defp node_type(r_xmlElement()) do
    :element
  end

  defp node_type(r_xmlText()) do
    :text
  end

  defp node_type(r_xmlPI()) do
    :processing_instruction
  end

  defp node_type(r_xmlNsNode()) do
    :namespace
  end

  defp node_type(r_xmlComment()) do
    :comment
  end

  defp node_type(r_xmlDocument()) do
    :root_node
  end

  defp match_namespace(tok, n, acc, context) do
    case r_xmlNode(n, :type) do
      :element ->
        r_xmlNode(parents: ps, node: e) = n
        r_xmlElement(name: name, namespace: nS, parents: ePs, pos: pos) = e
        r_xmlNamespace(default: default, nodes: nSPairs) = nS
        thisEPs = [{name, pos} | ePs]
        thisPs = [n | ps]

        acc0 =
          case default do
            d when d === [] or d === :"" ->
              {[], 1}

            uRI ->
              defaultNSNode = r_xmlNsNode(parents: thisEPs, pos: 1, prefix: [], uri: uRI)
              node = r_xmlNode(type: :namespace, node: defaultNSNode, parents: thisPs)
              {[node], 2}
          end

        {nodes, _I} =
          :lists.foldr(
            fn {prefix, uRI}, {accX, i} ->
              nSNode = r_xmlNsNode(parents: thisEPs, pos: i, prefix: prefix, uri: uRI)
              thisN = r_xmlNode(pos: i, type: :namespace, node: nSNode, parents: thisPs)
              {[thisN | accX], i + 1}
            end,
            acc0,
            nSPairs
          )

        :lists.foldr(
          fn thisN, accX ->
            match_self(tok, thisN, accX, context)
          end,
          acc,
          nodes
        )

      _Other ->
        acc
    end
  end

  defp update_nodeset(context = r_xmlContext(axis_type: axisType), nodeSet) do
    mapFold =
      case axisType do
        :forward ->
          :mapfoldl

        :reverse ->
          :mapfoldr
      end

    {result, _N} =
      apply(:lists, mapFold, [
        fn node, n ->
          {r_xmlNode(node, pos: n), n + 1}
        end,
        1,
        nodeSet
      ])

    r_xmlContext(context, nodeset: result)
  end

  defp node_test(f, n, context) when is_function(f) do
    f.(n, context)
  end

  defp node_test(_Test, r_xmlNode(type: :attribute, node: r_xmlAttribute(name: :xmlns)), _Context) do
    false
  end

  defp node_test(
         _Test,
         r_xmlNode(type: :attribute, node: r_xmlAttribute(nsinfo: {'xmlns', _Local})),
         _Context
       ) do
    false
  end

  defp node_test({:wildcard, _}, r_xmlNode(type: elAt), _Context)
       when elAt == :element or elAt == :attribute or
              elAt == :namespace do
    true
  end

  defp node_test({:prefix_test, prefix}, r_xmlNode(node: n), context) do
    case n do
      r_xmlElement(nsinfo: {^prefix, _}) ->
        true

      r_xmlElement(expanded_name: {uri, _}) ->
        case expanded_name(prefix, '_', context) do
          {^uri, _} ->
            true

          _ ->
            false
        end

      r_xmlAttribute(nsinfo: {^prefix, _}) ->
        true

      r_xmlAttribute(expanded_name: {uri, _}) ->
        case expanded_name(prefix, '_', context) do
          {^uri, _} ->
            true

          _ ->
            false
        end

      _ ->
        false
    end
  end

  defp node_test(
         {:name, {tag, _Prefix, _Local}},
         r_xmlNode(node: r_xmlElement(name: tag)) = _N,
         _Context
       ) do
    true
  end

  defp node_test(
         {:name, {tag, prefix, local}},
         r_xmlNode(
           node: r_xmlElement(name: name, expanded_name: eExpName, nsinfo: {_Prefix1, _})
         ),
         context
       ) do
    case expanded_name(prefix, local, context) do
      [] ->
        res = tag == name
        :no_debug
        res

      expName ->
        res = expName == eExpName
        :no_debug
        res
    end
  end

  defp node_test(
         {:name, {_Tag, prefix, local}},
         r_xmlNode(node: r_xmlElement(name: name, expanded_name: _EExpName, namespace: nS)),
         context
       ) do
    case expanded_name(prefix, local, context) do
      [] ->
        :no_debug
        false

      expName ->
        res = expName == {r_xmlNamespace(nS, :default), name}
        :no_debug
        res
    end
  end

  defp node_test(
         {:name, {tag, _Prefix, _Local}},
         r_xmlNode(node: r_xmlAttribute(name: tag)),
         _Context
       ) do
    true
  end

  defp node_test(
         {:name, {tag, prefix, local}},
         r_xmlNode(node: r_xmlAttribute(name: name, expanded_name: eExpName)),
         context
       ) do
    case expanded_name(prefix, local, context) do
      [] ->
        res = tag == name
        :no_debug
        res

      expName ->
        res = expName == eExpName
        :no_debug
        res
    end
  end

  defp node_test(
         {:name, {_Tag, [], local}},
         r_xmlNode(node: r_xmlNsNode(prefix: local)),
         _Context
       ) do
    true
  end

  defp node_test({:node_type, nT}, r_xmlNode(node: n), _Context) do
    case {nT, n} do
      {:text, r_xmlText()} ->
        true

      {:node, _} ->
        true

      {:attribute, r_xmlAttribute()} ->
        true

      {:namespace, r_xmlNsNode()} ->
        true

      {:comment, r_xmlComment()} ->
        true

      {:processing_instruction, r_xmlPI()} ->
        true

      _ ->
        false
    end
  end

  defp node_test(
         {:processing_instruction, name1},
         r_xmlNode(node: r_xmlPI(name: name2)),
         _Context
       ) do
    name1 == :erlang.atom_to_list(name2)
  end

  defp node_test(_Other, _N, _Context) do
    false
  end

  defp expanded_name(prefix, local, r_xmlContext(namespace: nS)) do
    case :lists.keysearch(prefix, 1, nS) do
      {:value, {_, uRI}} ->
        {uRI, :erlang.list_to_atom(local)}

      false ->
        []
    end
  end

  defp to_atom(a) when is_atom(a) do
    a
  end

  defp to_atom(s) when is_list(s) do
    :erlang.list_to_atom(s)
  end

  defp to_string(a) when is_atom(a) do
    :erlang.atom_to_list(a)
  end

  defp to_string(s) when is_list(s) do
    s
  end

  defp get_content(r_xmlElement(content: c)) when is_list(c) do
    c
  end

  defp get_content(r_xmlElement(content: f) = e) when is_function(f) do
    case f.() do
      c when is_list(c) ->
        c

      _Other ->
        exit({:bad_content, e})
    end
  end

  defp get_content(r_xmlDocument(content: c)) when is_list(c) do
    c
  end

  defp get_content(r_xmlDocument(content: c)) do
    [c]
  end

  defp get_position(r_xmlElement(pos: n)) do
    n
  end

  defp get_position(r_xmlText(pos: n)) do
    n
  end
end
