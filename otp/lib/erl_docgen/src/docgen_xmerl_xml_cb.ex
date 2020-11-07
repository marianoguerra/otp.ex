defmodule :m_docgen_xmerl_xml_cb do
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

  def unquote(:"#xml-inheritance#")() do
    [:xmerl_xml]
  end

  def unquote(:"#root#")(data, attrs, [], _E) do
    encoding =
      case (for r_xmlAttribute(
                  name: :encoding,
                  value: e
                ) <- attrs do
              e
            end) do
        [e] ->
          e

        _ ->
          :erlang.atom_to_list(:epp.default_encoding())
      end

    ['<', dTD, '>'] = hd(hd(data))

    [
      '<?xml version="1.0" encoding="',
      encoding,
      '" ?>\n',
      '<!DOCTYPE ' ++ dTD ++ ' SYSTEM "' ++ dTD ++ '.dtd">\n',
      data
    ]
  end

  def unquote(:"#element#")(tag, data, attrs, _Parents, _E) do
    {newTag, newAttrs} = convert_tag(tag, attrs)
    :xmerl_lib.markup(newTag, newAttrs, data)
  end

  def unquote(:"#text#")(text) do
    :xmerl_lib.export_text(text)
  end

  defp convert_tag(:a, [attr]) do
    case r_xmlAttribute(attr, :name) do
      :href ->
        val = r_xmlAttribute(attr, :value)

        case is_url(val) do
          true ->
            {:url, [attr]}

          false ->
            makesee(val)
        end

      :name ->
        {:marker, [r_xmlAttribute(attr, name: :id)]}
    end
  end

  defp convert_tag(:b, attrs) do
    {:em, attrs}
  end

  defp convert_tag(:blockquote, attrs) do
    {:quote, attrs}
  end

  defp convert_tag(:code, attrs) do
    {:c, attrs}
  end

  defp convert_tag(:dd, attrs) do
    {:item, attrs}
  end

  defp convert_tag(:dl, attrs) do
    {:taglist, attrs}
  end

  defp convert_tag(:dt, attrs) do
    {:tag, attrs}
  end

  defp convert_tag(:li, attrs) do
    {:item, attrs}
  end

  defp convert_tag(:ol, attrs) do
    {:list, attrs}
  end

  defp convert_tag(:strong, attrs) do
    {:em, attrs}
  end

  defp convert_tag(:td, attrs) do
    {:cell, attrs}
  end

  defp convert_tag(:tr, attrs) do
    {:row, attrs}
  end

  defp convert_tag(:tt, attrs) do
    {:c, attrs}
  end

  defp convert_tag(:ul, attrs) do
    {:list, attrs}
  end

  defp convert_tag(:underline, attrs) do
    {:em, attrs}
  end

  defp convert_tag(tag, attrs) do
    {tag, attrs}
  end

  defp is_url('http:' ++ _) do
    true
  end

  defp is_url('https:' ++ _) do
    true
  end

  defp is_url('../' ++ _) do
    true
  end

  defp is_url(fileRef) do
    case :filename.extension(fileRef) do
      '' ->
        false

      _Ext ->
        true
    end
  end

  defp makesee(ref) do
    {tag, marker} = :docgen_edoc_xml_cb.makesee(ref)
    {tag, [r_xmlAttribute(name: :marker, value: marker)]}
  end
end
