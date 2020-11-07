defmodule :m_xmerl_html do
  use Bitwise

  import :xmerl_lib,
    only: [end_tag: 1, export_text: 1, find_attribute: 2, is_empty_data: 1, start_tag: 2]

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
    []
  end

  def unquote(:"#text#")(text) do
    export_text(text)
  end

  def unquote(:"#root#")(data, attrs, [], _E) do
    ver =
      case find_attribute(:version, attrs) do
        {:value, v} ->
          v

        false ->
          '-//W3C//DTD HTML 4.01 Transitional//EN'
      end

    uRI =
      case find_attribute(:uri, attrs) do
        {:value, u} ->
          [' "', u, '"']

        false ->
          ''
      end

    ['<!DOCTYPE HTML PUBLIC "', ver, '"', uRI, '>\n', data]
  end

  defp markup(tag, attrs, data) do
    [start_tag(tag, attrs), data, end_tag(tag)]
  end

  defp markup_noend(tag, attrs, data) do
    [start_tag(tag, attrs), data]
  end

  def unquote(:"#element#")(tag, data, attrs, _Parents, _E) do
    case forbid_end(tag) do
      false ->
        markup(tag, attrs, data)

      true ->
        markup_noend(tag, attrs, data)
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
