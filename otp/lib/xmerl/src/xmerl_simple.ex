defmodule :m_xmerl_simple do
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
    content_acc: [],
    attr_acc: [],
    content_stack: [],
    attr_stack: []
  )

  def file(fname, opts) do
    opts1 = scanner_options(opts)
    :xmerl_scan.file(fname, opts1)
  end

  def string(str, opts) do
    opts1 = scanner_options(opts)
    :xmerl_scan.string(str, opts1)
  end

  defp scanner_options(opts) do
    eventS = r_state()

    scanner_options(
      opts,
      [
        {:event_fun, &event/2, eventS},
        {:acc_fun,
         fn _, acc, s ->
           {acc, s}
         end},
        {:close_fun, &close/1}
      ]
    )
  end

  defp scanner_options([h | t], opts) do
    case (try do
            keyreplace(h, 1, opts)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      false ->
        scanner_options(t, [h | opts])

      newOpts ->
        scanner_options(t, newOpts)
    end
  end

  defp scanner_options([], opts) do
    opts
  end

  defp keyreplace(x, pos, [h | t])
       when :erlang.element(
              pos,
              x
            ) ==
              :erlang.element(
                pos,
                h
              ) do
    [x | t]
  end

  defp keyreplace(x, pos, [h | t]) do
    [h | keyreplace(x, pos, t)]
  end

  defp keyreplace(_X, _Pos, []) do
    throw(false)
  end

  defp close(s) do
    eS = :xmerl_scan.event_state(s)
    r_state(attr_stack: [], content_stack: [], content_acc: content) = eS
    :lists.reverse(content)
  end

  defp event(r_xmerl_event(event: :started, data: r_xmlElement()), s) do
    r_state(content_acc: cAcc, attr_acc: aAcc, content_stack: cSt, attr_stack: aSt) =
      eS = :xmerl_scan.event_state(s)

    :xmerl_scan.event_state(
      r_state(eS,
        content_acc: [],
        attr_acc: [],
        content_stack: [cAcc | cSt],
        attr_stack: [aAcc | aSt]
      ),
      s
    )
  end

  defp event(r_xmerl_event(event: :ended, data: r_xmlElement(name: name)), s) do
    r_state(
      content_acc: cAcc,
      attr_acc: aAcc,
      content_stack: [prevCAcc | cSt],
      attr_stack: [
        prevAAcc
        | aSt
      ]
    ) = eS = :xmerl_scan.event_state(s)

    simple = {name, :lists.reverse(aAcc), :lists.reverse(cAcc)}

    :xmerl_scan.event_state(
      r_state(eS,
        content_acc: [
          simple
          | prevCAcc
        ],
        attr_acc: prevAAcc,
        content_stack: cSt,
        attr_stack: aSt
      ),
      s
    )
  end

  defp event(
         r_xmerl_event(
           event: :ended,
           data: r_xmlAttribute(name: name, value: value)
         ),
         s
       ) do
    r_state(attr_acc: aAcc) = eS = :xmerl_scan.event_state(s)
    simple = {name, value}

    :xmerl_scan.event_state(
      r_state(eS,
        attr_acc: [
          simple
          | aAcc
        ]
      ),
      s
    )
  end

  defp event(r_xmerl_event(event: :ended, data: r_xmlText(value: text)), s) do
    r_state(content_acc: cAcc) = eS = :xmerl_scan.event_state(s)

    :xmerl_scan.event_state(
      r_state(eS,
        content_acc: [
          text
          | cAcc
        ]
      ),
      s
    )
  end

  defp event(_E, s) do
    s
  end
end
