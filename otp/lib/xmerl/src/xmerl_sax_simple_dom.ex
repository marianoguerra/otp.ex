defmodule :m_xmerl_sax_simple_dom do
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

  Record.defrecord(:r_xmerl_sax_simple_dom_state, :xmerl_sax_simple_dom_state,
    tags: [],
    cno: [],
    namespaces: [],
    dom: []
  )

  def initial_state() do
    r_xmerl_sax_simple_dom_state()
  end

  def get_dom(r_xmerl_sax_simple_dom_state(dom: dom)) do
    dom
  end

  def event(event, _LineNo, state) do
    build_dom(event, state)
  end

  defp build_dom(:startDocument, state) do
    r_xmerl_sax_simple_dom_state(state, dom: [:startDocument])
  end

  defp build_dom(
         :endDocument,
         r_xmerl_sax_simple_dom_state(dom: [{tag, attributes, content} | d]) = state
       ) do
    case d do
      [:startDocument] ->
        r_xmerl_sax_simple_dom_state(state, dom: [{tag, attributes, :lists.reverse(content)}])

      [r_xmlDecl() = decl, :startDocument] ->
        r_xmerl_sax_simple_dom_state(state,
          dom: [decl, {tag, attributes, :lists.reverse(content)}]
        )

      _ ->
        state
    end
  end

  defp build_dom(
         {:startElement, _Uri, localName, _QName, attributes},
         r_xmerl_sax_simple_dom_state(tags: t, cno: cN, dom: d) = state
       ) do
    a = parse_attributes(localName, attributes)

    {num, newCN} =
      case cN do
        [] ->
          {1, [1]}

        [n | cNs] ->
          {n, [[1, n + 1] | cNs]}
      end

    r_xmerl_sax_simple_dom_state(state,
      tags: [
        {:erlang.list_to_atom(localName), num}
        | t
      ],
      cno: newCN,
      dom: [
        {:erlang.list_to_atom(localName), :lists.reverse(a), []}
        | d
      ]
    )
  end

  defp build_dom(
         {:endElement, _Uri, localName, _QName},
         r_xmerl_sax_simple_dom_state(
           tags: [_ | t],
           cno: [_ | cN],
           dom: [
             [{cName, cAttributes, cContent}, {pName, pAttributes, pContent}]
             | d
           ]
         ) = state
       ) do
    case :erlang.list_to_atom(localName) do
      ^cName ->
        r_xmerl_sax_simple_dom_state(state,
          tags: t,
          cno: cN,
          dom: [
            {pName, pAttributes,
             [
               {cName, cAttributes, :lists.reverse(cContent)}
               | pContent
             ]}
            | d
          ]
        )

      _ ->
        throw(
          {:sax_simple_dom_error,
           'Got end of element: ' ++ localName ++ ' but expected: ' ++ cName}
        )
    end
  end

  defp build_dom(
         {:characters, string},
         r_xmerl_sax_simple_dom_state(
           tags: _T,
           cno: [num | cN],
           dom: [{name, attributes, content} | d]
         ) = state
       ) do
    r_xmerl_sax_simple_dom_state(state,
      cno: [num + 1 | cN],
      dom: [{name, attributes, [string | content]} | d]
    )
  end

  defp build_dom(
         {:ignorableWhitespace, string},
         r_xmerl_sax_simple_dom_state(
           tags: t,
           cno: [num | cN],
           dom: [{name, attributes, content} | d]
         ) = state
       ) do
    r_xmerl_sax_simple_dom_state(state,
      cno: [num + 1 | cN],
      dom: [
        {name, attributes,
         [
           r_xmlText(value: string, parents: t, pos: num, type: :text)
           | content
         ]}
        | d
      ]
    )
  end

  defp build_dom(
         {:comment, string},
         r_xmerl_sax_simple_dom_state(
           tags: t,
           cno: [num | cN],
           dom: [r_xmlElement(content: c) = current | d]
         ) = state
       ) do
    r_xmerl_sax_simple_dom_state(state,
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
         r_xmerl_sax_simple_dom_state(namespaces: nS) = state
       ) do
    r_xmerl_sax_simple_dom_state(state,
      namespaces: [
        {prefix, :erlang.list_to_atom(uri)}
        | nS
      ]
    )
  end

  defp build_dom(
         {:endPrefixMapping, prefix},
         r_xmerl_sax_simple_dom_state(namespaces: [{prefix, _} | nS]) = state
       ) do
    r_xmerl_sax_simple_dom_state(state, namespaces: nS)
  end

  defp build_dom(
         {:processingInstruction, 'xml', piData},
         r_xmerl_sax_simple_dom_state(dom: d) = state
       ) do
    {vsn, piData1} = find_and_remove_attribute('version', piData, [])
    {enc, piData2} = find_and_remove_attribute('encoding', piData1, [])
    {standalone, piData3} = find_and_remove_attribute('standalone', piData2, :yes)

    r_xmerl_sax_simple_dom_state(state,
      dom: [
        r_xmlDecl(vsn: vsn, encoding: enc, standalone: standalone, attributes: piData3)
        | d
      ]
    )
  end

  defp build_dom(
         {:processingInstruction, piTarget, piData},
         r_xmerl_sax_simple_dom_state(
           cno: [num | cN],
           dom: [r_xmlElement(content: c) = current | d]
         ) = state
       ) do
    r_xmerl_sax_simple_dom_state(state,
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

  defp parse_attributes(elName, [{_Uri, _Prefix, localName, attrValue} | as], n, acc) do
    parse_attributes(elName, as, n + 1, [{:erlang.list_to_atom(localName), attrValue} | acc])
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
