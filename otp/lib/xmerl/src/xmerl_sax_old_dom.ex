defmodule :m_xmerl_sax_old_dom do
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

  Record.defrecord(:r_xmlPI, :xmlPI, name: :undefined, pos: :undefined, value: :undefined)
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

  Record.defrecord(:r_xmerl_sax_old_dom_state, :xmerl_sax_old_dom_state,
    tags: [],
    cno: [],
    namespaces: [],
    dom: []
  )

  def initial_state() do
    r_xmerl_sax_old_dom_state()
  end

  def get_dom(r_xmerl_sax_old_dom_state(dom: dom)) do
    dom
  end

  def event(event, _LineNo, state) do
    build_dom(event, state)
  end

  defp build_dom(:startDocument, state) do
    r_xmerl_sax_old_dom_state(state, dom: [:startDocument])
  end

  defp build_dom(
         :endDocument,
         r_xmerl_sax_old_dom_state(dom: [r_xmlElement(content: c) = current | d]) = state
       ) do
    case d do
      [:startDocument] ->
        r_xmerl_sax_old_dom_state(state, dom: [r_xmlElement(current, content: :lists.reverse(c))])

      [r_xmlDecl() = decl, :startDocument] ->
        r_xmerl_sax_old_dom_state(state,
          dom: [decl, r_xmlElement(current, content: :lists.reverse(c))]
        )

      _ ->
        state
    end
  end

  defp build_dom(
         {:startElement, uri, localName, qName, attributes},
         r_xmerl_sax_old_dom_state(tags: t, cno: cN, namespaces: nS, dom: d) = state
       ) do
    a = parse_attributes(localName, attributes)

    {num, newCN} =
      case cN do
        [] ->
          {1, [1]}

        [n | cNs] ->
          {n, [[1, n + 1] | cNs]}
      end

    nsInfo =
      case qName do
        {[], _} ->
          []

        qN ->
          qN
      end

    nameAsAtom = convert_qname_to_atom(qName)

    r_xmerl_sax_old_dom_state(state,
      tags: [{nameAsAtom, num} | t],
      cno: newCN,
      dom: [
        r_xmlElement(
          name: nameAsAtom,
          expanded_name: nameAsAtom,
          nsinfo: nsInfo,
          namespace:
            r_xmlNamespace(
              default: :erlang.list_to_atom(uri),
              nodes: nS
            ),
          pos: num,
          parents: t,
          attributes: :lists.reverse(a),
          xmlbase: '.'
        )
        | d
      ]
    )
  end

  defp build_dom(
         {:endElement, _Uri, localName, qName},
         r_xmerl_sax_old_dom_state(
           tags: [_ | t],
           cno: [_ | cN],
           dom: [
             [r_xmlElement(name: cName, content: c) = current, r_xmlElement(content: pC) = parent]
             | d
           ]
         ) = state
       ) do
    case convert_qname_to_atom(qName) do
      ^cName ->
        r_xmerl_sax_old_dom_state(state,
          tags: t,
          cno: cN,
          dom: [
            r_xmlElement(parent,
              content: [
                r_xmlElement(current, content: :lists.reverse(c))
                | pC
              ]
            )
            | d
          ]
        )

      _ ->
        throw(
          {:xmerl_sax_old_dom_error,
           'Got end of element: ' ++
             localName ++ ' but expected: ' ++ r_xmlElement(current, :name)}
        )
    end
  end

  defp build_dom(
         {:characters, string},
         r_xmerl_sax_old_dom_state(
           tags: t,
           cno: [num | cN],
           dom: [r_xmlElement(content: c) = current | d]
         ) = state
       ) do
    r_xmerl_sax_old_dom_state(state,
      cno: [num + 1 | cN],
      dom: [
        r_xmlElement(current,
          content: [
            r_xmlText(value: string, parents: t, pos: num, type: :text)
            | c
          ]
        )
        | d
      ]
    )
  end

  defp build_dom(
         {:ignorableWhitespace, string},
         r_xmerl_sax_old_dom_state(
           tags: t,
           cno: [num | cN],
           dom: [r_xmlElement(content: c) = current | d]
         ) = state
       ) do
    r_xmerl_sax_old_dom_state(state,
      cno: [num + 1 | cN],
      dom: [
        r_xmlElement(current,
          content: [
            r_xmlText(value: string, parents: t, pos: num, type: :text)
            | c
          ]
        )
        | d
      ]
    )
  end

  defp build_dom(
         {:comment, string},
         r_xmerl_sax_old_dom_state(
           tags: t,
           cno: [num | cN],
           dom: [r_xmlElement(content: c) = current | d]
         ) = state
       ) do
    r_xmerl_sax_old_dom_state(state,
      cno: [num + 1 | cN],
      dom: [
        r_xmlElement(current,
          content: [
            r_xmlComment(parents: t, pos: num, value: string)
            | c
          ]
        )
        | d
      ]
    )
  end

  defp build_dom({:startPrefixMapping, [], _Uri}, state) do
    state
  end

  defp build_dom(
         {:startPrefixMapping, prefix, uri},
         r_xmerl_sax_old_dom_state(namespaces: nS) = state
       ) do
    r_xmerl_sax_old_dom_state(state,
      namespaces: [
        {prefix, :erlang.list_to_atom(uri)}
        | nS
      ]
    )
  end

  defp build_dom(
         {:endPrefixMapping, prefix},
         r_xmerl_sax_old_dom_state(namespaces: [{prefix, _} | nS]) = state
       ) do
    r_xmerl_sax_old_dom_state(state, namespaces: nS)
  end

  defp build_dom(
         {:processingInstruction, 'xml', piData},
         r_xmerl_sax_old_dom_state(dom: d) = state
       ) do
    {vsn, piData1} = find_and_remove_attribute('version', piData, [])
    {enc, piData2} = find_and_remove_attribute('encoding', piData1, [])
    {standalone, piData3} = find_and_remove_attribute('standalone', piData2, :yes)

    r_xmerl_sax_old_dom_state(state,
      dom: [
        r_xmlDecl(vsn: vsn, encoding: enc, standalone: standalone, attributes: piData3)
        | d
      ]
    )
  end

  defp build_dom(
         {:processingInstruction, piTarget, piData},
         r_xmerl_sax_old_dom_state(
           cno: [num | cN],
           dom: [r_xmlElement(content: c) = current | d]
         ) = state
       ) do
    r_xmerl_sax_old_dom_state(state,
      cno: [num + 1 | cN],
      dom: [
        r_xmlElement(current,
          content: [
            r_xmlPI(name: piTarget, pos: num, value: piData)
            | c
          ]
        )
        | d
      ]
    )
  end

  defp build_dom(_E, state) do
    state
  end

  defp parse_attributes(elName, attributes) do
    parse_attributes(elName, attributes, 1, [])
  end

  defp parse_attributes(_, [], _, acc) do
    acc
  end

  defp parse_attributes(elName, [{_Uri, prefix, localName, attrValue} | as], n, acc) do
    name = convert_qname_to_atom({prefix, localName})

    nsInfo =
      case prefix do
        [] ->
          []

        p ->
          {p, localName}
      end

    parse_attributes(elName, as, n + 1, [
      r_xmlAttribute(name: name, pos: n, nsinfo: nsInfo, value: attrValue, normalized: false)
      | acc
    ])
  end

  defp convert_qname_to_atom({[], n}) do
    :erlang.list_to_atom(n)
  end

  defp convert_qname_to_atom({p, n}) do
    :erlang.list_to_atom(p ++ ':' ++ n)
  end

  defp find_and_remove_attribute(key, data, default) do
    case :lists.keysearch(key, 1, data) do
      {:value, {^key, value}} ->
        data2 = :lists.keydelete(key, 1, data)
        {value, data2}

      false ->
        {default, data}
    end
  end
end
