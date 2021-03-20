defmodule :m_xmerl_xpath_pred do
  use Bitwise
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

  def eval(expr, c = r_xmlContext(context_node: r_xmlNode(pos: pos))) do
    obj = expr(expr, c)

    res =
      case r_xmlObj(obj, :type) do
        :number when r_xmlObj(obj, :value) == pos ->
          true

        :number ->
          false

        :boolean ->
          r_xmlObj(obj, :value)

        _ ->
          mk_boolean(c, obj)
      end

    res
  end

  def string(x) do
    r_xmlObj(type: :string, value: x)
  end

  def nodeset(x) do
    r_xmlObj(type: :nodeset, value: x)
  end

  def number(x) do
    r_xmlObj(type: :number, value: x)
  end

  def boolean(x) do
    r_xmlObj(type: :boolean, value: x)
  end

  defp expr({:arith, op, e1, e2}, c) do
    arith_expr(op, e1, e2, c)
  end

  defp expr({:comp, op, e1, e2}, c) do
    comp_expr(op, e1, e2, c)
  end

  defp expr({:bool, op, e1, e2}, c) do
    bool_expr(op, e1, e2, c)
  end

  defp expr({:negative, e}, c) do
    n = mk_number(c, e)
    -n
  end

  defp expr({:number, n}, _C) do
    r_xmlObj(type: :number, value: n)
  end

  defp expr({:literal, s}, _C) do
    r_xmlObj(type: :string, value: s)
  end

  defp expr({:function_call, f, args}, c) do
    case core_function(f) do
      {true, f1} ->
        apply(:xmerl_xpath_pred, f1, [c, args])

      true ->
        apply(:xmerl_xpath_pred, f, [c, args])

      false ->
        exit({:not_a_core_function, f})
    end
  end

  defp expr({:path, type, pathExpr}, c) do
    r_state(context: r_xmlContext(nodeset: nS)) = :xmerl_xpath.eval_path(type, pathExpr, c)
    r_xmlObj(type: :nodeset, value: nS)
  end

  defp expr(expr, _C) do
    exit({:unknown_expr, expr})
  end

  defp arith_expr(:+, e1, e2, c) do
    r_xmlObj(
      type: :number,
      value: mk_number(c, e1) + mk_number(c, e2)
    )
  end

  defp arith_expr(:-, e1, e2, c) do
    r_xmlObj(
      type: :number,
      value: mk_number(c, e1) - mk_number(c, e2)
    )
  end

  defp arith_expr(:*, e1, e2, c) do
    r_xmlObj(
      type: :number,
      value: mk_number(c, e1) * mk_number(c, e2)
    )
  end

  defp arith_expr(:div, e1, e2, c) do
    r_xmlObj(
      type: :number,
      value: mk_number(c, e1) / mk_number(c, e2)
    )
  end

  defp arith_expr(:mod, e1, e2, c) do
    r_xmlObj(
      type: :number,
      value: rem(mk_number(c, e1), mk_number(c, e2))
    )
  end

  defp comp_expr(:>, e1, e2, c) do
    n1 = expr(e1, c)
    n2 = expr(e2, c)

    r_xmlObj(
      type: :boolean,
      value: compare_ineq_format(n1, n2, c) > compare_ineq_format(n2, n1, c)
    )
  end

  defp comp_expr(:<, e1, e2, c) do
    n1 = expr(e1, c)
    n2 = expr(e2, c)

    r_xmlObj(
      type: :boolean,
      value: compare_ineq_format(n1, n2, c) > compare_ineq_format(n2, n1, c)
    )
  end

  defp comp_expr(:>=, e1, e2, c) do
    n1 = expr(e1, c)
    n2 = expr(e2, c)

    r_xmlObj(
      type: :boolean,
      value: compare_ineq_format(n1, n2, c) > compare_ineq_format(n2, n1, c)
    )
  end

  defp comp_expr(:<=, e1, e2, c) do
    n1 = expr(e1, c)
    n2 = expr(e2, c)

    r_xmlObj(
      type: :boolean,
      value: compare_ineq_format(n1, n2, c) > compare_ineq_format(n2, n1, c)
    )
  end

  defp comp_expr(:=, e1, e2, c) do
    n1 = expr(e1, c)
    n2 = expr(e2, c)

    r_xmlObj(
      type: :boolean,
      value: compare_eq_format(n1, n2, c) == compare_eq_format(n2, n1, c)
    )
  end

  defp comp_expr(:!=, e1, e2, c) do
    n1 = expr(e1, c)
    n2 = expr(e2, c)

    r_xmlObj(
      type: :boolean,
      value: compare_eq_format(n1, n2, c) != compare_eq_format(n2, n1, c)
    )
  end

  defp bool_expr(:or, e1, e2, c) do
    r_xmlObj(
      type: :boolean,
      value: :erlang.or(mk_boolean(c, e1), mk_boolean(c, e2))
    )
  end

  defp bool_expr(:and, e1, e2, c) do
    r_xmlObj(
      type: :boolean,
      value:
        :erlang.and(
          mk_boolean(c, e1),
          mk_boolean(c, e2)
        )
    )
  end

  defp compare_eq_format(n1 = r_xmlObj(type: t1), n2 = r_xmlObj(type: t2), c)
       when t1 == :nodeset or t2 == :nodeset do
    compare_nseq_format(n1, n2, c)
  end

  defp compare_eq_format(n1 = r_xmlObj(type: t1), r_xmlObj(type: t2), c)
       when t1 == :boolean or t2 == :boolean do
    mk_boolean(c, n1)
  end

  defp compare_eq_format(n1 = r_xmlObj(type: t1), r_xmlObj(type: t2), c)
       when t1 == :number or t2 == :number do
    mk_number(c, n1)
  end

  defp compare_eq_format(n1, _, c) do
    mk_string(c, string_value(n1))
  end

  defp compare_ineq_format(n1 = r_xmlObj(type: t1), n2 = r_xmlObj(type: t2), c)
       when t1 == :nodeset or t2 == :nodeset do
    compare_nseq_format(n1, n2, c)
  end

  defp compare_ineq_format(n1, _N2, c) do
    mk_number(c, n1)
  end

  defp compare_nseq_format(n1 = r_xmlObj(type: :number), _N2, c) do
    mk_number(c, n1)
  end

  defp compare_nseq_format(n1 = r_xmlObj(type: :boolean), _N2, c) do
    mk_boolean(c, n1)
  end

  defp compare_nseq_format(n1 = r_xmlObj(type: :string), _N2, c) do
    mk_string(c, n1)
  end

  defp compare_nseq_format(n1 = r_xmlObj(type: :nodeset), _N2 = r_xmlObj(type: :number), c) do
    mk_number(c, string_value(n1))
  end

  defp compare_nseq_format(n1 = r_xmlObj(type: :nodeset), _N2 = r_xmlObj(type: :boolean), c) do
    mk_boolean(c, n1)
  end

  defp compare_nseq_format(n1 = r_xmlObj(type: :nodeset), _N2, c) do
    mk_string(c, string_value(n1))
  end

  def core_function(:last) do
    true
  end

  def core_function(:position) do
    true
  end

  def core_function(:count) do
    true
  end

  def core_function(:id) do
    true
  end

  def core_function(:"local-name") do
    true
  end

  def core_function(:"namespace-uri") do
    true
  end

  def core_function(:name) do
    true
  end

  def core_function(:string) do
    true
  end

  def core_function(:concat) do
    true
  end

  def core_function(:"starts-with") do
    true
  end

  def core_function(:contains) do
    true
  end

  def core_function(:"substring-before") do
    true
  end

  def core_function(:"substring-after") do
    true
  end

  def core_function(:"string-length") do
    true
  end

  def core_function(:"normalize-space") do
    true
  end

  def core_function(:translate) do
    true
  end

  def core_function(:boolean) do
    true
  end

  def core_function(:not) do
    {true, :fn_not}
  end

  def core_function(true) do
    {true, :fn_true}
  end

  def core_function(false) do
    {true, :fn_false}
  end

  def core_function(:lang) do
    true
  end

  def core_function(:number) do
    true
  end

  def core_function(:sum) do
    true
  end

  def core_function(:floor) do
    true
  end

  def core_function(:ceiling) do
    true
  end

  def core_function(:round) do
    true
  end

  def core_function(_) do
    false
  end

  def last(r_xmlContext(nodeset: set), []) do
    r_xmlObj(type: :number, value: length(set))
  end

  def position(r_xmlContext(context_node: r_xmlNode(pos: pos)), []) do
    r_xmlObj(type: :number, value: pos)
  end

  def count(c, [arg]) do
    r_xmlObj(type: :number, value: length(mk_nodeset(c, arg)))
  end

  def id(c, [arg]) do
    wD = r_xmlContext(c, :whole_document)
    nS0 = [wD]
    obj = mk_object(c, arg)

    case r_xmlObj(obj, :type) do
      :nodeset ->
        nodeSet = r_xmlObj(obj, :value)

        idTokens =
          :lists.foldl(
            fn n, accX ->
              strVal = string_value(n)
              tokensX = id_tokens(strVal)
              tokensX ++ accX
            end,
            [],
            nodeSet
          )

        newNodeSet =
          :xmerl_xpath.axis(
            :descendant_or_self,
            fn node ->
              attribute_test(node, :id, idTokens)
            end,
            r_xmlContext(c, nodeset: nS0)
          )

        r_xmlObj(type: :nodeset, value: newNodeSet)

      _ ->
        strVal = string_value(r_xmlObj(obj, :value))
        idTokens = id_tokens(strVal)
        nodeSet = [r_xmlDocument(r_xmlNode(wD, :node), :content)]

        newNodeSet =
          :lists.foldl(
            fn tok, accX ->
              select_on_attribute(nodeSet, :id, tok, accX)
            end,
            [],
            idTokens
          )

        r_xmlObj(type: :nodeset, value: newNodeSet)
    end
  end

  defp id_tokens(str = r_xmlObj(type: :string)) do
    :string.tokens(r_xmlObj(str, :value), ' \t\n\r')
  end

  defp attribute_test(r_xmlNode(node: r_xmlElement(attributes: attrs)), key, vals) do
    case :lists.keysearch(key, r_xmlAttribute(:name), attrs) do
      {:value, r_xmlAttribute(value: v)} ->
        :lists.member(v, vals)

      _ ->
        false
    end
  end

  defp attribute_test(_Node, _Key, _Vals) do
    false
  end

  def unquote(:"local-name")(c, []) do
    local_name1(default_nodeset(c))
  end

  def unquote(:"local-name")(c, [arg]) do
    local_name1(mk_nodeset(c, arg))
  end

  defp local_name1([]) do
    r_xmlObj(type: :string, value: [])
  end

  defp local_name1([r_xmlNode(type: :element, node: el) | _]) do
    r_xmlElement(name: name, nsinfo: nSI) = el
    local_name2(name, nSI)
  end

  defp local_name1([r_xmlNode(type: :attribute, node: att) | _]) do
    r_xmlAttribute(name: name, nsinfo: nSI) = att
    local_name2(name, nSI)
  end

  defp local_name1([r_xmlNode(type: :namespace, node: n) | _]) do
    r_xmlNsNode(prefix: prefix) = n
    r_xmlObj(type: :string, value: prefix)
  end

  defp local_name1([r_xmlElement(name: name, nsinfo: nSI) | _]) do
    local_name2(name, nSI)
  end

  defp local_name2(name, nSI) do
    case nSI do
      {_Prefix, local} ->
        r_xmlObj(type: :string, value: local)

      [] ->
        r_xmlObj(type: :string, value: :erlang.atom_to_list(name))
    end
  end

  def unquote(:"namespace-uri")(c, []) do
    ns_uri(default_nodeset(c))
  end

  def unquote(:"namespace-uri")(c, [arg]) do
    ns_uri(mk_nodeset(c, arg))
  end

  defp ns_uri([]) do
    r_xmlObj(type: :string, value: [])
  end

  defp ns_uri([r_xmlElement(nsinfo: nSI, namespace: nS) | _]) do
    ns_uri2(nSI, nS)
  end

  defp ns_uri([r_xmlNode(type: :element, node: el) | _]) do
    r_xmlElement(nsinfo: nSI, namespace: nS) = el
    ns_uri2(nSI, nS)
  end

  defp ns_uri([r_xmlNode(type: :attribute, node: att) | _]) do
    r_xmlAttribute(nsinfo: nSI, namespace: nS) = att
    ns_uri2(nSI, nS)
  end

  defp ns_uri(_) do
    r_xmlObj(type: :string, value: [])
  end

  defp ns_uri2(nSI, nS) do
    case nSI do
      {prefix, _} ->
        case :lists.keysearch(prefix, 1, r_xmlNamespace(nS, :nodes)) do
          false ->
            r_xmlObj(type: :string, value: [])

          {:value, {_K, v}} ->
            string_value(v)
        end

      [] ->
        r_xmlObj(type: :string, value: [])
    end
  end

  def name(c, []) do
    name1(default_nodeset(c))
  end

  def name(c, [arg]) do
    name1(mk_nodeset(c, arg))
  end

  defp name1([]) do
    r_xmlObj(type: :string, value: [])
  end

  defp name1(nodeSet) do
    nSVal =
      case ns_uri(nodeSet) do
        r_xmlObj(value: nSStr) when nSStr !== [] ->
          '{' ++ nSStr ++ '}'

        _ ->
          ''
      end

    r_xmlObj(value: localName) = local_name1(nodeSet)
    r_xmlObj(type: :string, value: nSVal ++ localName)
  end

  def string(c, []) do
    ns_string(default_nodeset(c))
  end

  def string(c, [arg]) do
    string_value(mk_object(c, arg))
  end

  defp ns_string([obj | _]) do
    string_value(obj)
  end

  defp string_value(r_xmlObj(type: :nodeset, value: [])) do
    r_xmlObj(type: :string, value: '')
  end

  defp string_value(n = r_xmlObj(type: :nodeset)) do
    string_value(hd(r_xmlObj(n, :value)))
  end

  defp string_value(n = r_xmlObj()) do
    string_value(r_xmlObj(n, :value))
  end

  defp string_value(a = r_xmlNode(type: :attribute)) do
    r_xmlAttribute(value: attVal) = r_xmlNode(a, :node)
    r_xmlObj(type: :string, value: attVal)
  end

  defp string_value(n = r_xmlNode(type: :namespace)) do
    r_xmlNsNode(uri: uRI) = r_xmlNode(n, :node)
    r_xmlObj(type: :string, value: :erlang.atom_to_list(uRI))
  end

  defp string_value(el = r_xmlNode(type: :element)) do
    r_xmlElement(content: c) = r_xmlNode(el, :node)

    textValue = fn
      r_xmlText(value: t), _Fun ->
        t

      r_xmlElement(content: cont), fun ->
        fun.(cont, fun)

      _, _ ->
        []
    end

    textDecendants = fn x ->
      textValue.(x, textValue)
    end

    r_xmlObj(
      type: :string,
      value: :lists.flatten(:lists.map(textDecendants, c))
    )
  end

  defp string_value(t = r_xmlNode(type: :text)) do
    r_xmlText(value: txt) = r_xmlNode(t, :node)
    r_xmlObj(type: :string, value: txt)
  end

  defp string_value(t = r_xmlNode(type: :comment)) do
    r_xmlComment(value: txt) = r_xmlNode(t, :node)
    r_xmlObj(type: :string, value: txt)
  end

  defp string_value(:infinity) do
    r_xmlObj(type: :string, value: 'Infinity')
  end

  defp string_value(:neg_infinity) do
    r_xmlObj(type: :string, value: '-Infinity')
  end

  defp string_value(a) when is_atom(a) do
    r_xmlObj(type: :string, value: :erlang.atom_to_list(a))
  end

  defp string_value(n) when is_integer(n) do
    r_xmlObj(type: :string, value: :erlang.integer_to_list(n))
  end

  defp string_value(n) when is_float(n) do
    n1 = round(n * 10_000_000_000_000_000)

    r_xmlObj(
      type: :string,
      value: strip_zeroes(:erlang.integer_to_list(n1))
    )
  end

  defp string_value(str) when is_list(str) do
    r_xmlObj(type: :string, value: str)
  end

  defp strip_zeroes(str) do
    strip_zs(:lists.reverse(str), 15)
  end

  defp strip_zs([h | t], 0) do
    :lists.reverse(t) ++ [?., h]
  end

  defp strip_zs('0' ++ t, n) do
    strip_zs(t, n - 1)
  end

  defp strip_zs([h | t], n) do
    strip_zs(t, n - 1, [h])
  end

  defp strip_zs([h | t], 0, acc) do
    :lists.reverse(t) ++ [?., h | acc]
  end

  defp strip_zs([h | t], n, acc) do
    strip_zs(t, n - 1, [h | acc])
  end

  def concat(c, args = [_, _ | _]) do
    strings =
      for a <- args do
        mk_string(c, a)
      end

    r_xmlObj(type: :string, value: :lists.concat(strings))
  end

  def unquote(:"starts-with")(c, [a1, a2]) do
    r_xmlObj(
      type: :boolean,
      value:
        :lists.prefix(
          mk_string(c, a2),
          mk_string(c, a1)
        )
    )
  end

  def contains(c, [a1, a2]) do
    pos = :string.str(mk_string(c, a1), mk_string(c, a2))
    r_xmlObj(type: :boolean, value: pos > 0)
  end

  def unquote(:"substring-before")(c, [a1, a2]) do
    s1 = mk_string(c, a1)
    s2 = mk_string(c, a2)
    pos = :string.str(s1, s2)
    r_xmlObj(type: :string, value: :string.substr(s1, 1, pos))
  end

  def unquote(:"substring-after")(c, [a1, a2]) do
    s1 = mk_string(c, a1)
    s2 = mk_string(c, a2)

    case :string.str(s1, s2) do
      0 ->
        r_xmlObj(type: :string, value: [])

      pos ->
        r_xmlObj(type: :string, value: :string.substr(s1, pos))
    end
  end

  def substring(c, [a1, a2]) do
    s = mk_string(c, a1)
    pos = mk_integer(c, a2)
    r_xmlObj(type: :string, value: :string.substr(s, pos))
  end

  def substring(c, [a1, a2, a3]) do
    s = mk_string(c, a1)
    pos = mk_integer(c, a2)
    length = mk_integer(c, a3)
    r_xmlObj(type: :string, value: :string.substr(s, pos, length))
  end

  def unquote(:"string-length")(c = r_xmlContext(context_node: n), []) do
    length(mk_string(c, string_value(n)))
  end

  def unquote(:"string-length")(c, [a]) do
    length(mk_string(c, a))
  end

  def unquote(:"normalize-space")(c = r_xmlContext(context_node: n), []) do
    normalize(mk_string(c, string_value(n)))
  end

  def unquote(:"normalize-space")(c, [a]) do
    normalize(mk_string(c, a))
  end

  def translate(c, [a1, a2, a3]) do
    s1 = mk_string(c, a1)
    s2 = mk_string(c, a2)
    s3 = mk_string(c, a3)

    r_xmlObj(
      type: :string,
      value: translate1(s1, translations(s2, s3))
    )
  end

  defp translate1([h | t], xls) do
    case :lists.keysearch(h, 1, xls) do
      {:value, {_, :remove}} ->
        translate1(t, xls)

      {:value, {_, :replace, h1}} ->
        [h1 | translate1(t, xls)]

      false ->
        [h | translate1(t, xls)]
    end
  end

  defp translate1([], _) do
    []
  end

  defp translations([h | t], [h1 | t1]) do
    [{h, :replace, h1} | translations(t, t1)]
  end

  defp translations(rest, []) do
    for x <- rest do
      {x, :remove}
    end
  end

  defp translations([], _Rest) do
    []
  end

  def boolean(c, [arg]) do
    r_xmlObj(type: :boolean, value: mk_boolean(c, arg))
  end

  def fn_not(c, [arg]) do
    r_xmlObj(type: :boolean, value: not mk_boolean(c, arg))
  end

  def fn_true(_C, []) do
    r_xmlObj(type: :boolean, value: true)
  end

  def fn_false(_C, []) do
    r_xmlObj(type: :boolean, value: false)
  end

  def lang(c = r_xmlContext(context_node: n), [arg]) do
    s = mk_string(c, arg)

    lang =
      case n do
        r_xmlElement(language: l) ->
          l

        r_xmlAttribute(language: l) ->
          l

        r_xmlText(language: l) ->
          l

        r_xmlComment(language: l) ->
          l

        _ ->
          []
      end

    case lang do
      [] ->
        r_xmlObj(type: :boolean, value: false)

      _ ->
        r_xmlObj(
          type: :boolean,
          value: match_lang(upcase(s), upcase(lang))
        )
    end
  end

  defp upcase([h | t]) when h >= ?a and h <= ?z do
    [h + (?A - ?a) | upcase(t)]
  end

  defp upcase([h | t]) do
    [h | upcase(t)]
  end

  defp upcase([]) do
    []
  end

  defp match_lang([h | t], [h | t1]) do
    match_lang(t, t1)
  end

  defp match_lang([], '-' ++ _) do
    true
  end

  defp match_lang([], []) do
    true
  end

  defp match_lang(_, _) do
    false
  end

  def number(c = r_xmlContext(context_node: n), []) do
    r_xmlObj(type: :number, value: mk_number(c, string(c, n)))
  end

  def number(c, [arg]) do
    r_xmlObj(type: :number, value: mk_number(c, arg))
  end

  def sum(c, [arg]) do
    nS = mk_nodeset(c, arg)

    :lists.foldl(
      fn n, sum ->
        sum + mk_number(c, string(c, n))
      end,
      0,
      nS
    )
  end

  def floor(c, [arg]) do
    num = mk_number(c, arg)

    case trunc(num) do
      num1 when num1 > num ->
        r_xmlObj(type: :number, value: num1 - 1)

      num1 ->
        r_xmlObj(type: :number, value: num1)
    end
  end

  def ceiling(c, [arg]) do
    num = mk_number(c, arg)

    case trunc(num) do
      num1 when num1 < num ->
        r_xmlObj(type: :number, value: num1 + 1)

      num1 ->
        r_xmlObj(type: :number, value: num1)
    end
  end

  def round(c, [arg]) do
    case mk_number(c, arg) do
      a when is_atom(a) ->
        a

      n when is_integer(n) ->
        n

      f when is_float(f) ->
        round(f)
    end
  end

  defp select_on_attribute([e = r_xmlElement(attributes: attrs) | t], k, v, acc) do
    case :lists.keysearch(k, r_xmlAttribute(:name), attrs) do
      {:value, r_xmlAttribute(value: ^v)} ->
        acc2 = select_on_attribute(r_xmlElement(e, :content), k, v, [e | acc])
        select_on_attribute(t, k, v, acc2)

      _ ->
        acc2 = select_on_attribute(r_xmlElement(e, :content), k, v, acc)
        select_on_attribute(t, k, v, acc2)
    end
  end

  defp select_on_attribute([h | t], k, v, acc)
       when elem(h, 0) === :xmlText do
    select_on_attribute(t, k, v, acc)
  end

  defp select_on_attribute([], _K, _V, acc) do
    acc
  end

  defp mk_nodeset(_C0, r_xmlContext(nodeset: nS)) do
    nS
  end

  defp mk_nodeset(_C0, r_xmlObj(type: :nodeset, value: nS)) do
    nS
  end

  defp mk_nodeset(c0, expr) do
    case expr(expr, c0) do
      r_xmlObj(type: :nodeset, value: nS) ->
        nS

      other ->
        exit({:expected_nodeset, other})
    end
  end

  defp default_nodeset(r_xmlContext(context_node: n)) do
    [n]
  end

  defp mk_object(_C0, obj = r_xmlObj()) do
    obj
  end

  defp mk_object(c0, expr) do
    expr(expr, c0)
  end

  defp mk_string(_C0, r_xmlObj(type: :string, value: v)) do
    v
  end

  defp mk_string(c0, obj = r_xmlObj()) do
    mk_string(c0, string_value(obj))
  end

  defp mk_string(c0, expr) do
    mk_string(c0, expr(expr, c0))
  end

  defp mk_integer(_C0, r_xmlObj(type: :number, value: v))
       when is_float(v) do
    round(v)
  end

  defp mk_integer(_C0, r_xmlObj(type: :number, value: v))
       when is_integer(v) do
    v
  end

  defp mk_integer(c, expr) do
    mk_integer(c, expr(expr, c))
  end

  defp mk_number(_C, r_xmlObj(type: :string, value: v)) do
    scan_number(v)
  end

  defp mk_number(_C, r_xmlObj(type: :number, value: v)) do
    v
  end

  defp mk_number(c, n = r_xmlObj(type: :nodeset)) do
    mk_number(c, string_value(n))
  end

  defp mk_number(_C, r_xmlObj(type: :boolean, value: false)) do
    0
  end

  defp mk_number(_C, r_xmlObj(type: :boolean, value: true)) do
    1
  end

  defp mk_number(c, expr) do
    mk_number(c, expr(expr, c))
  end

  defp mk_boolean(_C, r_xmlObj(type: :boolean, value: v)) do
    v
  end

  defp mk_boolean(_C, r_xmlObj(type: :number, value: 0)) do
    false
  end

  defp mk_boolean(_C, r_xmlObj(type: :number, value: v))
       when is_float(v) or is_integer(v) do
    true
  end

  defp mk_boolean(_C, r_xmlObj(type: :nodeset, value: [])) do
    false
  end

  defp mk_boolean(_C, r_xmlObj(type: :nodeset, value: _V)) do
    true
  end

  defp mk_boolean(_C, r_xmlObj(type: :string, value: [])) do
    false
  end

  defp mk_boolean(_C, r_xmlObj(type: :string, value: _V)) do
    true
  end

  defp mk_boolean(c, expr) do
    mk_boolean(c, expr(expr, c))
  end

  defp normalize([h | t])
       when h == 32 or h == 13 or h == 10 or
              h == 9 do
    normalize(t)
  end

  defp normalize(str) do
    contF = fn _ContF, retF, _S ->
      retF.()
    end

    normalize(
      str,
      r_xmerl_scanner(
        acc_fun: fn ->
          exit(:acc_fun)
        end,
        event_fun: fn ->
          exit(:event_fun)
        end,
        hook_fun: fn ->
          exit(:hook_fun)
        end,
        continuation_fun: contF
      ),
      []
    )
  end

  defp normalize(str = [h | _], s, acc)
       when h == 32 or
              h == 13 or h == 10 or h == 9 do
    case :xmerl_scan.accumulate_whitespace(str, s, :preserve, acc) do
      {' ' ++ acc1, [], _S1} ->
        :lists.reverse(acc1)

      {acc1, [], _S1} ->
        :lists.reverse(acc1)

      {acc1, t1, s1} ->
        normalize(t1, s1, acc1)
    end
  end

  defp normalize([h | t], s, acc) do
    normalize(t, s, [h | acc])
  end

  defp normalize([], _S, acc) do
    :lists.reverse(acc)
  end

  defp scan_number([h | t])
       when h == 32 or h == 13 or h == 10 or
              h == 9 do
    scan_number(t)
  end

  defp scan_number('-' ++ t) do
    case (try do
            :xmerl_xpath_scan.scan_number(t)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {{:number, n}, tail} ->
        case is_all_white(tail) do
          true ->
            n

          false ->
            :NaN
        end

      _Other ->
        :NaN
    end
  end

  defp scan_number(t) do
    case (try do
            :xmerl_xpath_scan.scan_number(t)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {{:number, n}, tail} ->
        case is_all_white(tail) do
          true ->
            n

          false ->
            :NaN
        end

      _Other ->
        :NaN
    end
  end

  defp is_all_white([h | t])
       when h == 32 or h == 13 or h == 10 or
              h == 9 do
    is_all_white(t)
  end

  defp is_all_white([_H | _T]) do
    false
  end

  defp is_all_white([]) do
    true
  end
end
