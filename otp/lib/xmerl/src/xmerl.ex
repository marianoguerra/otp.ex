defmodule :m_xmerl do
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

  def export(content, callback) do
    export(content, callback, [])
  end

  def export(content, callback, rootAttributes)
      when is_atom(callback) do
    export1(content, callbacks(callback), rootAttributes)
  end

  def export(content, callbacks, rootAttrs)
      when is_list(callbacks) do
    export1(content, callbacks, rootAttrs)
  end

  def export_simple(content, callback) do
    export_simple(content, callback, [])
  end

  def export_simple(content, callback, rootAttrs)
      when is_atom(callback) do
    export_simple1(content, callbacks(callback), rootAttrs)
  end

  def export_simple(content, callbacks, rootAttrs)
      when is_list(callbacks) do
    export_simple1(content, callbacks, rootAttrs)
  end

  defp export_simple1(content, callback, rootAttrs) do
    export1(:xmerl_lib.expand_content(content), callback, rootAttrs)
  end

  defp export1(content, callbacks, rootAttrs)
       when is_list(content) do
    result = export_content(content, callbacks)
    attrs = :xmerl_lib.expand_attributes(rootAttrs, 1, [{:"#root#", 1}])
    root = r_xmlElement(name: :"#root#", pos: 1, parents: [], attributes: attrs)
    args = [result, r_xmlElement(root, :attributes), [], root]
    tagdef(:"#root#", 1, [], args, callbacks)
  end

  def export_simple_content(content, callback) when is_atom(callback) do
    export_content(
      :xmerl_lib.expand_content(content),
      callbacks(callback)
    )
  end

  def export_simple_content(content, callbacks) when is_list(callbacks) do
    export_content(
      :xmerl_lib.expand_content(content),
      callbacks
    )
  end

  def export_content([r_xmlText(value: text) | es], callbacks) do
    [
      apply_text_cb(callbacks, text)
      | export_content(
          es,
          callbacks
        )
    ]
  end

  def export_content([r_xmlPI() | es], callbacks) do
    export_content(es, callbacks)
  end

  def export_content([r_xmlComment() | es], callbacks) do
    export_content(es, callbacks)
  end

  def export_content([r_xmlDecl() | es], callbacks) do
    export_content(es, callbacks)
  end

  def export_content([e | es], callbacks) do
    [
      export_element(e, callbacks)
      | export_content(
          es,
          callbacks
        )
    ]
  end

  def export_content([], _Callbacks) do
    []
  end

  def export_simple_element(content, callback) when is_atom(callback) do
    export_element(
      :xmerl_lib.expand_element(content),
      callbacks(callback)
    )
  end

  def export_simple_element(content, callbacks) when is_list(callbacks) do
    export_element(
      :xmerl_lib.expand_element(content),
      callbacks
    )
  end

  def export_element(e, cB) when is_atom(cB) do
    export_element(e, callbacks(cB))
  end

  def export_element(r_xmlText(value: text), cBs) do
    apply_text_cb(cBs, text)
  end

  def export_element(
        e =
          r_xmlElement(
            name: tag,
            pos: pos,
            attributes: attributes,
            parents: parents,
            content: content
          ),
        cBs
      ) do
    data = export_content(content, cBs)
    args = [data, attributes, parents, e]
    tagdef(tag, pos, parents, args, cBs)
  end

  def export_element(r_xmlPI(), _CBs) do
    []
  end

  def export_element(r_xmlComment(), _CBs) do
    []
  end

  def export_element(r_xmlDecl(), _CBs) do
    []
  end

  def export_element(e, callbackModule, callbackState)
      when is_atom(callbackModule) do
    export_element(e, callbacks(callbackModule), callbackState)
  end

  def export_element(r_xmlText(value: text), callbackModule, _CallbackState) do
    apply_text_cb(callbackModule, text)
  end

  def export_element(
        e =
          r_xmlElement(
            name: tag,
            pos: pos,
            parents: parents,
            attributes: attributes,
            content: content
          ),
        callbacks,
        cBstate
      ) do
    args = [content, attributes, cBstate, e]
    tagdef(tag, pos, parents, args, callbacks)
  end

  def export_element(r_xmlPI(), _CallbackModule, callbackState) do
    callbackState
  end

  def export_element(r_xmlComment(), _CallbackModule, callbackState) do
    callbackState
  end

  def export_element(r_xmlDecl(), _CallbackModule, callbackState) do
    callbackState
  end

  defp tagdef(tag, pos, parents, args, cBs) do
    case apply_tag_cb(cBs, tag, args) do
      {:"#xml-alias#", newTag} ->
        tagdef(newTag, pos, parents, args, cBs)

      {:"#xml-redefine#", data} ->
        export_content(
          :xmerl_lib.expand_content(data, pos, parents),
          cBs
        )

      other ->
        other
    end
  end

  def callbacks(module) do
    result = check_inheritance(module, [])
    :lists.reverse(result)
  end

  defp callbacks([m | mods], visited) do
    case :lists.member(m, visited) do
      false ->
        newVisited = check_inheritance(m, visited)
        callbacks(mods, newVisited)

      true ->
        exit({:cyclic_inheritance, {m, hd(visited)}})
    end
  end

  defp callbacks([], visited) do
    visited
  end

  defp check_inheritance(m, visited) do
    case m."#xml-inheritance#"() do
      [] ->
        [m | visited]

      mods ->
        callbacks(mods, [m | visited])
    end
  end

  defp apply_text_cb(ms, text) do
    apply_cb(ms, :"#text#", :"#text#", [text])
  end

  defp apply_tag_cb(ms, f, args) do
    apply_cb(ms, f, :"#element#", args)
  end

  defp apply_cb(ms, f, df, args) do
    apply_cb(ms, f, df, args, length(args))
  end

  defp apply_cb(ms, f, df, args, a) do
    apply_cb(ms, f, df, args, a, ms)
  end

  defp apply_cb([m | ms], f, df, args, a, ms0) do
    case :erlang.function_exported(m, f, a) do
      true ->
        apply(m, f, args)

      false ->
        apply_cb(ms, f, df, args, a, ms0)
    end
  end

  defp apply_cb([], df, df, args, _A, _Ms0) do
    exit({:unknown_tag, {df, args}})
  end

  defp apply_cb([], f, df, args, a, ms0) do
    apply_cb(ms0, df, df, [f | args], a + 1)
  end
end
