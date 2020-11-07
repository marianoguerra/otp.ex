defmodule :m_xmerl_otpsgml do
  use Bitwise
  import :xmerl_lib, only: [export_text: 1, is_empty_data: 1, markup: 3, start_tag: 2]
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

  def unquote(:"#xml-inheritance#")() do
    [:xmerl_sgml]
  end

  def unquote(:"#text#")(text) do
    export_text(text)
  end

  def unquote(:"#root#")(data, _Attrs, [], _E) do
    ['<!doctype erlref PUBLIC "-//Stork//DTD erlref//EN">\n', data]
  end

  def unquote(:"#element#")(tag, data, attrs, _Parents, _E) do
    case convert_tag(tag, attrs) do
      {false, newTag, newAttrs} ->
        markup(newTag, newAttrs, data)

      {true, newTag, newAttrs} ->
        [start_tag(newTag, newAttrs), data]
    end
  end

  def p(data, attrs, _Parents, _E) do
    case is_empty_data(data) do
      true ->
        markup(:p, attrs, ' ')

      false ->
        markup(:p, attrs, data)
    end
  end

  defp convert_tag(:code, attrs) do
    convert_tag(:c, attrs)
  end

  defp convert_tag(:strong, attrs) do
    convert_tag(:em, attrs)
  end

  defp convert_tag(:b, attrs) do
    convert_tag(:em, attrs)
  end

  defp convert_tag(:underline, attrs) do
    convert_tag(:em, attrs)
  end

  defp convert_tag(:dl, attrs) do
    convert_tag(:taglist, attrs)
  end

  defp convert_tag(:dt, attrs) do
    convert_tag(:tag, attrs)
  end

  defp convert_tag(:dd, attrs) do
    convert_tag(:item, attrs)
  end

  defp convert_tag(:ul, attrs) do
    convert_tag(:list, attrs)
  end

  defp convert_tag(:li, attrs) do
    convert_tag(:item, attrs)
  end

  defp convert_tag(:tt, attrs) do
    convert_tag(:c, attrs)
  end

  defp convert_tag(:a, attrs) do
    convert_tag(
      convert_aref(attrs),
      convert_aref_attrs(convert_aref(attrs), attrs)
    )
  end

  defp convert_tag(tag, attrs) do
    {forbid_end(tag), tag, attrs}
  end

  defp convert_aref([r_xmlAttribute(name: :href, value: v) | _Rest]) do
    case html_content(v) do
      true ->
        :url

      _ ->
        :seealso
    end
  end

  defp convert_aref([r_xmlAttribute(name: k) | rest]) do
    :error_logger.warning_msg('ignoring attribute \'~p\' for tag \'a\'\n', [k])
    convert_aref(rest)
  end

  defp convert_aref_attrs(:url, attrs) do
    attrs
  end

  defp convert_aref_attrs(sA, [r_xmlAttribute(name: :href, value: v) = a | rest]) do
    [
      r_xmlAttribute(a, name: :marker, value: v)
      | convert_aref_attrs(
          sA,
          rest
        )
    ]
  end

  defp convert_aref_attrs(_, []) do
    []
  end

  defp html_content([]) do
    false
  end

  defp html_content([?. | rest]) do
    case rest do
      'htm' ++ _EmaNfeR ->
        true

      _ ->
        html_content(rest)
    end
  end

  defp html_content([_H | t]) do
    html_content(t)
  end

  defp forbid_end(:area) do
    true
  end

  defp forbid_end(:base) do
    true
  end

  defp forbid_end(:basefont) do
    true
  end

  defp forbid_end(:br) do
    true
  end

  defp forbid_end(:col) do
    true
  end

  defp forbid_end(:frame) do
    true
  end

  defp forbid_end(:hr) do
    true
  end

  defp forbid_end(:img) do
    true
  end

  defp forbid_end(:input) do
    true
  end

  defp forbid_end(:isindex) do
    true
  end

  defp forbid_end(:link) do
    true
  end

  defp forbid_end(:marker) do
    true
  end

  defp forbid_end(:meta) do
    true
  end

  defp forbid_end(:param) do
    true
  end

  defp forbid_end(_) do
    false
  end
end
