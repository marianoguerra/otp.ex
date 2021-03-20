defmodule :m_xmerl_lib do
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

  Record.defrecord(:r_xsd_state, :xsd_state,
    schema_name: :undefined,
    vsn: :undefined,
    schema_preprocessed: false,
    external_xsd_base: false,
    xsd_base: :undefined,
    xml_options: [],
    scope: [],
    schemaLocations: [],
    elementFormDefault: :unqualified,
    attributeFormDefault: :unqualified,
    localElementsNamespace: :undefined,
    targetNamespace: :undefined,
    namespace_nodes: [{'xml', :"http://www.w3.org/XML/1998/namespace"}],
    global_namespace_nodes: [],
    checked_namespace_nodes: [{'xml', [], :"http://www.w3.org/XML/1998/namespace"}],
    table: :undefined,
    tab2file: false,
    redefine: false,
    finalDefault: :undefined,
    blockDefault: :undefined,
    fetch_fun: :undefined,
    fetch_path: [],
    num_el: 0,
    global_element_source: [],
    keyrefs: [],
    IDs: [],
    substitutionGroups: [],
    derived_types: [],
    unchecked_references: [],
    circularity_stack: [],
    circularity_disallowed: [],
    errors: []
  )

  Record.defrecord(:r_schema, :schema,
    elementFormDefault: :undefined,
    attributeFormDefault: :undefined,
    targetNamespace: :undefined,
    blockDefault: [],
    finalDefault: [],
    id: :undefined,
    content: []
  )

  Record.defrecord(:r_schema_element, :schema_element,
    name: :undefined,
    type: :undefined,
    resolved: false,
    substitutionGroup: :undefined,
    uniqueness: :undefined,
    key: :undefined,
    scope: :undefined,
    form: :undefined,
    id: :undefined,
    occurance: {1, 1},
    value_constraint: :undefined,
    nillable: false,
    abstract: false,
    block: [],
    final: []
  )

  Record.defrecord(:r_schema_simple_type, :schema_simple_type,
    name: :undefined,
    scope: :undefined,
    base_type: :undefined,
    resolved: false,
    final: [],
    id: :undefined,
    facets: [],
    variety: :atomic,
    content: :undefined
  )

  Record.defrecord(:r_schema_complex_type, :schema_complex_type,
    name: :undefined,
    base_type: :undefined,
    resolved: false,
    scope: :undefined,
    derivation: :undefined,
    final: [],
    id: :undefined,
    block: [],
    abstract: false,
    content_type: :"element-only",
    complexity: :undefined,
    attributes: [],
    content: [],
    prohibited: :undefined
  )

  Record.defrecord(:r_schema_attribute, :schema_attribute,
    name: :undefined,
    type: :undefined,
    resolved: false,
    scope: :undefined,
    use: :optional,
    default: :undefined,
    fixed: :undefined,
    form: :undefined,
    id: :undefined
  )

  Record.defrecord(:r_schema_attribute_group, :schema_attribute_group,
    name: :undefined,
    id: :undefined,
    ref: :undefined,
    content: []
  )

  Record.defrecord(:r_schema_anyAttribute, :schema_anyAttribute,
    id: :undefined,
    processContents: :strict,
    namespace: :undefined,
    scope: :undefined
  )

  Record.defrecord(:r_schema_group, :schema_group,
    name: :undefined,
    id: :undefined,
    ref: :undefined,
    content: [],
    occurance: {1, 1}
  )

  Record.defrecord(:r_schema_extension, :schema_extension,
    base: :undefined,
    id: :undefined,
    content: []
  )

  Record.defrecord(:r_schema_restriction, :schema_restriction,
    base: :undefined,
    id: :undefined,
    content: []
  )

  Record.defrecord(:r_schema_list, :schema_list,
    id: :undefined,
    itemType: :undefined
  )

  Record.defrecord(:r_id_constraint, :id_constraint,
    category: :undefined,
    id: :undefined,
    name: :undefined,
    refer: :undefined,
    type: :undefined,
    selector: :undefined,
    fields: :undefined,
    key_sequence: :undefined
  )

  Record.defrecord(:r_chain, :chain,
    content: :undefined,
    occurance: {1, 1}
  )

  Record.defrecord(:r_alternative, :alternative,
    content: :undefined,
    occurance: {0, 1}
  )

  def export_text(t) do
    export_text(t, [])
  end

  defp export_text([?< | t], cont) do
    '&lt;' ++ export_text(t, cont)
  end

  defp export_text([?> | t], cont) do
    '&gt;' ++ export_text(t, cont)
  end

  defp export_text([?& | t], cont) do
    '&amp;' ++ export_text(t, cont)
  end

  defp export_text([c | t], cont) when is_integer(c) do
    [c | export_text(t, cont)]
  end

  defp export_text([t | t1], cont) do
    export_text(t, [t1 | cont])
  end

  defp export_text([], [t | cont]) do
    export_text(t, cont)
  end

  defp export_text([], []) do
    []
  end

  defp export_text(bin, cont) do
    export_text(:erlang.binary_to_list(bin), cont)
  end

  def flatten_text(t) do
    flatten_text(t, [])
  end

  defp flatten_text([c | t], cont) when is_integer(c) do
    [c | flatten_text(t, cont)]
  end

  defp flatten_text([t | t1], cont) do
    flatten_text(t, [t1 | cont])
  end

  defp flatten_text([], [t | cont]) do
    flatten_text(t, cont)
  end

  defp flatten_text([], []) do
    []
  end

  defp flatten_text(bin, cont) do
    flatten_text(:erlang.binary_to_list(bin), cont)
  end

  def export_attribute(i) when is_integer(i) do
    :erlang.integer_to_list(i)
  end

  def export_attribute(a) when is_atom(a) do
    export_attribute(:erlang.atom_to_list(a), [])
  end

  def export_attribute(s) do
    export_attribute(s, [])
  end

  defp export_attribute([?< | t], cont) do
    '&lt;' ++ export_attribute(t, cont)
  end

  defp export_attribute([?& | t], cont) do
    '&amp;' ++ export_attribute(t, cont)
  end

  defp export_attribute([?" | t], cont) do
    '&quot;' ++ export_attribute(t, cont)
  end

  defp export_attribute([c | t], cont) when is_integer(c) do
    [c | export_attribute(t, cont)]
  end

  defp export_attribute([t | t1], cont) do
    export_attribute(t, [t1 | cont])
  end

  defp export_attribute([], [t | cont]) do
    export_attribute(t, cont)
  end

  defp export_attribute([], []) do
    []
  end

  defp export_attribute(bin, cont) do
    export_attribute(:erlang.binary_to_list(bin), cont)
  end

  def normalize_element(element) do
    normalize_element(element, 1, [])
  end

  def normalize_element(element, pos, parents) do
    expand_element(element, pos, parents, true)
  end

  def expand_element(element) do
    expand_element(element, 1, [])
  end

  def expand_element(element, pos, parents) do
    expand_element(element, pos, parents, false)
  end

  defp expand_element(e = r_xmlElement(name: n), pos, parents, norm) do
    newParents = [{n, pos} | parents]
    content = expand_content(r_xmlElement(e, :content), 1, newParents, norm)
    attrs = expand_attributes(r_xmlElement(e, :attributes), 1, newParents)
    r_xmlElement(e, pos: pos, parents: parents, attributes: attrs, content: content)
  end

  defp expand_element(e = r_xmlText(), pos, parents, norm) do
    r_xmlText(e, pos: pos, parents: parents, value: expand_text(r_xmlText(e, :value), norm))
  end

  defp expand_element(e = r_xmlPI(), pos, parents, norm) do
    r_xmlPI(e, pos: pos, parents: parents, value: expand_text(r_xmlPI(e, :value), norm))
  end

  defp expand_element(e = r_xmlComment(), pos, parents, norm) do
    r_xmlComment(e, pos: pos, parents: parents, value: expand_text(r_xmlComment(e, :value), norm))
  end

  defp expand_element(e = r_xmlDecl(), _Pos, _Parents, _Norm) do
    attrs = expand_attributes(r_xmlDecl(e, :attributes), 1, [])
    r_xmlDecl(e, attributes: attrs)
  end

  defp expand_element({tag, attrs, content}, pos, parents, norm)
       when is_atom(tag) do
    newParents = [{tag, pos} | parents]

    r_xmlElement(
      name: tag,
      pos: pos,
      parents: parents,
      attributes: expand_attributes(attrs, 1, newParents),
      content: expand_content(content, 1, newParents, norm)
    )
  end

  defp expand_element({tag, content}, pos, parents, norm)
       when is_atom(tag) do
    newParents = [{tag, pos} | parents]

    r_xmlElement(
      name: tag,
      pos: pos,
      parents: parents,
      attributes: [],
      content: expand_content(content, 1, newParents, norm)
    )
  end

  defp expand_element(tag, pos, parents, _Norm) when is_atom(tag) do
    r_xmlElement(name: tag, pos: pos, parents: parents, attributes: [], content: [])
  end

  defp expand_element(string, pos, parents, norm)
       when is_list(string) do
    r_xmlText(pos: pos, parents: parents, value: expand_text(string, norm))
  end

  defp expand_text(s, false) do
    s
  end

  defp expand_text(s, true) do
    flatten_text(s)
  end

  def normalize_content(content) do
    normalize_content(content, 1, [])
  end

  def normalize_content(content, pos, parents) do
    expand_content(content, pos, parents, true)
  end

  def expand_content(content) do
    expand_content(content, 1, [])
  end

  def expand_content(content, pos, parents) do
    expand_content(content, pos, parents, false)
  end

  defp expand_content([{h} | t], pos, parents, norm) do
    expand_content(h ++ t, pos, parents, norm)
  end

  defp expand_content([{f, s} | t], pos, parents, norm)
       when is_function(f) do
    case f.(s) do
      :done ->
        expand_content(t, pos, parents, norm)

      {c, s2} ->
        expand_content([{f, s2}, c | t], pos, parents, norm)
    end
  end

  defp expand_content([h | t], pos, parents, norm) do
    [
      expand_element(h, pos, parents, norm)
      | expand_content(t, pos + 1, parents, norm)
    ]
  end

  defp expand_content([], _Pos, _Parents, _Norm) do
    []
  end

  def expand_attributes(attrs) do
    expand_attributes(attrs, 1, [])
  end

  def expand_attributes([h = r_xmlAttribute() | t], pos, parents) do
    [
      r_xmlAttribute(h, pos: pos, value: expand_value(r_xmlAttribute(h, :value)))
      | expand_attributes(t, pos + 1, parents)
    ]
  end

  def expand_attributes([{p, s} | t], pos, parents)
      when is_function(p) do
    case p.(s) do
      :done ->
        expand_attributes(t, pos, parents)

      {a, s2} ->
        expand_attributes([{p, s2}, a | t], pos, parents)
    end
  end

  def expand_attributes([{k, v} | t], pos, parents) do
    [
      r_xmlAttribute(name: k, pos: pos, parents: parents, value: expand_value(v))
      | expand_attributes(t, pos + 1, parents)
    ]
  end

  def expand_attributes([], _Pos, _Parents) do
    []
  end

  defp expand_value(s) when is_atom(s) do
    :erlang.atom_to_list(s)
  end

  defp expand_value(s) when is_integer(s) do
    :erlang.integer_to_list(s)
  end

  defp expand_value(s) do
    flatten_text(s)
  end

  def simplify_element(
        r_xmlElement(expanded_name: [], name: tag, attributes: attrs, content: content)
      ) do
    {tag, simplify_attributes(attrs), simplify_content(content)}
  end

  def simplify_element(r_xmlElement(expanded_name: name, attributes: attrs, content: content)) do
    {name, simplify_attributes(attrs), simplify_content(content)}
  end

  def simplify_element(r_xmlText(value: text)) do
    text
  end

  def simplify_element({tag, attrs, content}) when is_atom(tag) do
    {tag, simplify_attributes(attrs), simplify_content(content)}
  end

  def simplify_element({tag, content}) when is_atom(tag) do
    {tag, [], simplify_content(content)}
  end

  def simplify_element(tag) when is_atom(tag) do
    {tag, [], []}
  end

  def simplify_element(text) when is_list(text) do
    text
  end

  def simplify_content([r_xmlPI() | t]) do
    simplify_content(t)
  end

  def simplify_content([r_xmlComment() | t]) do
    simplify_content(t)
  end

  def simplify_content([r_xmlDecl() | t]) do
    simplify_content(t)
  end

  def simplify_content([h | t]) do
    [simplify_element(h) | simplify_content(t)]
  end

  def simplify_content([]) do
    []
  end

  defp simplify_attributes([r_xmlAttribute(name: k, value: v) | t]) when is_atom(k) do
    [{k, expand_value(v)} | simplify_attributes(t)]
  end

  defp simplify_attributes([h = {k, _} | t]) when is_atom(k) do
    [h | simplify_attributes(t)]
  end

  defp simplify_attributes([]) do
    []
  end

  def find_attribute(name, attrs) do
    case :lists.keysearch(name, r_xmlAttribute(:name), attrs) do
      {:value, r_xmlAttribute(value: v)} ->
        {:value, v}

      false ->
        false
    end
  end

  def markup(tag, data) do
    markup(tag, [], data)
  end

  def markup(tag, attrs, []) do
    empty_tag(tag, attrs)
  end

  def markup(tag, attrs, data) do
    [start_tag(tag, attrs), data, end_tag(tag)]
  end

  def start_tag(tagStr) do
    start_tag(tagStr, [])
  end

  def start_tag(tag, attrs) when is_atom(tag) do
    start_tag(:erlang.atom_to_list(tag), attrs)
  end

  def start_tag(tagStr, []) do
    ['<', tagStr, '>']
  end

  def start_tag(tagStr, attrs) do
    ['<', tagStr, attributes(attrs), '>']
  end

  def empty_tag(tag) do
    empty_tag(tag, [])
  end

  def empty_tag(tag, attrs) when is_atom(tag) do
    empty_tag(:erlang.atom_to_list(tag), attrs)
  end

  def empty_tag(tagStr, []) do
    ['<', tagStr, '/>']
  end

  def empty_tag(tagStr, attrs) do
    ['<', tagStr, attributes(attrs), '/>']
  end

  def end_tag(tag) when is_atom(tag) do
    end_tag(:erlang.atom_to_list(tag))
  end

  def end_tag(tagStr) do
    ['</', tagStr, '>']
  end

  defp attributes(attrs) do
    for a <- attrs do
      attr_string(a)
    end
  end

  defp attr_string(r_xmlAttribute(name: k, value: v)) do
    [' ', :erlang.atom_to_list(k), '="', export_attribute(v), '"']
  end

  def is_empty_data([]) do
    true
  end

  def is_empty_data([x | xs]) do
    case is_empty_data(x) do
      false ->
        false

      true ->
        is_empty_data(xs)
    end
  end

  def is_empty_data(_) do
    false
  end

  def remove_whitespace([r_xmlText(value: ' ') | data]) do
    remove_whitespace(data)
  end

  def remove_whitespace([e = r_xmlElement(content: content) | data]) do
    [
      r_xmlElement(e, content: remove_whitespace(content))
      | remove_whitespace(data)
    ]
  end

  def remove_whitespace([other | data]) do
    [other | remove_whitespace(data)]
  end

  def remove_whitespace([]) do
    []
  end

  def mapxml(fun, r_xmlElement() = e) do
    c1 = fun.(e)
    c2 = mapxml(fun, :lists.flatten(r_xmlElement(c1, :content)))
    r_xmlElement(c1, content: c2)
  end

  def mapxml(fun, list) when is_list(list) do
    aFun = fn e ->
      mapxml(fun, e)
    end

    :lists.map(aFun, list)
  end

  def mapxml(fun, e) do
    fun.(e)
  end

  def foldxml(fun, accu0, r_xmlElement(content: c) = e) do
    accu1 = fun.(e, accu0)
    foldxml(fun, accu1, c)
  end

  def foldxml(fun, accu, list) when is_list(list) do
    aFun = fn e, a ->
      foldxml(fun, a, e)
    end

    :lists.foldl(aFun, accu, list)
  end

  def foldxml(fun, accu, e) do
    fun.(e, accu)
  end

  def mapfoldxml(fun, accu0, r_xmlElement() = e) do
    {c1, accu1} = fun.(e, accu0)
    {c2, accu2} = mapfoldxml(fun, accu1, :lists.flatten(r_xmlElement(c1, :content)))
    {r_xmlElement(c1, content: c2), accu2}
  end

  def mapfoldxml(fun, accu, list) when is_list(list) do
    aFun = fn e, a ->
      mapfoldxml(fun, a, e)
    end

    :lists.mapfoldl(aFun, accu, list)
  end

  def mapfoldxml(fun, accu, e) do
    fun.(e, accu)
  end

  def detect_charset(content) do
    detect_charset(:undefined, content)
  end

  def detect_charset(extCharset, content) when is_list(extCharset) do
    detect_charset(
      :erlang.list_to_atom(extCharset),
      content
    )
  end

  def detect_charset(extCharset, content) do
    case autodetect(extCharset, content) do
      {:auto, content1} ->
        {:auto, :"iso-10646-utf-1", content1}

      {:external, content1} ->
        {:external, :"iso-10646-utf-1", content1}

      {:undefined, _} ->
        {:undefined, :undefined, content}

      {^extCharset, ^content} ->
        {:external, extCharset, content}
    end
  end

  defp autodetect(:undefined, [0, 0, 254, 255 | input]) do
    {:auto, :xmerl_ucs.from_ucs4be(input)}
  end

  defp autodetect(:"iso-10646-utf-1", [0, 0, 254, 255 | input]) do
    {:external, :xmerl_ucs.from_ucs4be(input)}
  end

  defp autodetect(:undefined, [255, 254, 0, 0 | input]) do
    {:auto, :xmerl_ucs.from_ucs4le(input)}
  end

  defp autodetect(:"iso-10646-utf-1", [255, 254, 0, 0 | input]) do
    {:external, :xmerl_ucs.from_ucs4le(input)}
  end

  defp autodetect(:undefined, [254, 255 | input]) do
    {:auto, :xmerl_ucs.from_utf16be(input)}
  end

  defp autodetect(:"utf-16be", [254, 255 | input]) do
    {:external, :xmerl_ucs.from_utf16be(input)}
  end

  defp autodetect(:undefined, [255, 254 | input]) do
    {:auto, :xmerl_ucs.from_utf16le(input)}
  end

  defp autodetect(:"utf-16le", [255, 254 | input]) do
    {:external, :xmerl_ucs.from_utf16le(input)}
  end

  defp autodetect(:undefined, [239, 187, 191 | input]) do
    {:auto, :xmerl_ucs.from_utf8(input)}
  end

  defp autodetect(:"utf-8", [239, 187, 191 | input]) do
    {:external, :xmerl_ucs.from_utf8(input)}
  end

  defp autodetect(:"utf-8", [255, 254 | input]) do
    {:external, :xmerl_ucs.from_utf16le(input)}
  end

  defp autodetect(:"utf-8", [254, 255 | input]) do
    {:external, :xmerl_ucs.from_utf16be(input)}
  end

  defp autodetect(:undefined, [0, 0, 0, 60 | input]) do
    {:auto, :xmerl_ucs.from_ucs4be([0, 0, 0, 60 | input])}
  end

  defp autodetect(:"iso-10646-utf-1", [0, 0, 0, 60 | input]) do
    {:external, :xmerl_ucs.from_ucs4be([0, 0, 0, 60 | input])}
  end

  defp autodetect(:undefined, [60, 0, 0, 0 | input]) do
    {:auto, :xmerl_ucs.from_ucs4le([60, 0, 0, 0 | input])}
  end

  defp autodetect(:"iso-10646-utf-1", [60, 0, 0, 0 | input]) do
    {:external, :xmerl_ucs.from_ucs4le([60, 0, 0, 0 | input])}
  end

  defp autodetect(:undefined, [0, 60, 0, 63 | input]) do
    {:auto, :xmerl_ucs.from_utf16be([0, 60, 0, 63 | input])}
  end

  defp autodetect(:"utf-16be", [0, 60, 0, 63 | input]) do
    {:external, :xmerl_ucs.from_utf16be([0, 60, 0, 63 | input])}
  end

  defp autodetect(:undefined, [60, 0, 63, 0 | input]) do
    {:auto, :xmerl_ucs.from_utf16le([60, 0, 63, 0 | input])}
  end

  defp autodetect(:"utf-16le", [60, 0, 63, 0 | input]) do
    {:external, :xmerl_ucs.from_utf16le([60, 0, 63, 0 | input])}
  end

  defp autodetect(extCharset, content) do
    {extCharset, content}
  end

  def is_ncname(a) when is_atom(a) do
    is_ncname(:erlang.atom_to_list(a))
  end

  def is_ncname([?_ | t]) do
    is_name1(t)
  end

  def is_ncname([h | t]) do
    case is_letter(h) do
      true ->
        is_name1(t)

      _ ->
        false
    end
  end

  def is_name(a) when is_atom(a) do
    is_name(:erlang.atom_to_list(a))
  end

  def is_name([?_ | t]) do
    is_name1(t)
  end

  def is_name([?: | t]) do
    is_name1(t)
  end

  def is_name([h | t]) do
    case is_letter(h) do
      true ->
        is_name1(t)

      _ ->
        false
    end
  end

  defp is_name1([]) do
    true
  end

  defp is_name1([h | t]) do
    case is_namechar(h) do
      true ->
        is_name1(t)

      _ ->
        false
    end
  end

  def is_char(9) do
    true
  end

  def is_char(10) do
    true
  end

  def is_char(13) do
    true
  end

  def is_char(x) when x >= 32 and x <= 55295 do
    true
  end

  def is_char(x) when x >= 57344 and x <= 65533 do
    true
  end

  def is_char(x) when x >= 65536 and x <= 1_114_111 do
    true
  end

  def is_char(_) do
    false
  end

  def is_namechar(x) do
    try do
      :erlang.element(
        x,
        {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 0, 0,
         0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
         0, 0, 0, 0, 3, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
         1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
         1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
         1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1}
      ) > 0
    catch
      _, _ ->
        case is_letter(x) do
          true ->
            true

          false ->
            case is_digit(x) do
              true ->
                true

              false ->
                case is_combining_char(x) do
                  true ->
                    true

                  false ->
                    is_extender(x)
                end
            end
        end
    end
  end

  def is_letter(x) do
    try do
      :erlang.element(
        x,
        {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 0, 0,
         0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
         0, 0, 0, 0, 3, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
         1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
         1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
         1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1}
      ) === 1
    catch
      _, _ ->
        case is_base_char(x) do
          false ->
            is_ideographic(x)

          true ->
            true
        end
    end
  end

  defp is_base_char(x) when x >= 65 and x <= 90 do
    true
  end

  defp is_base_char(x) when x >= 97 and x <= 122 do
    true
  end

  defp is_base_char(x) when x >= 192 and x <= 214 do
    true
  end

  defp is_base_char(x) when x >= 216 and x <= 246 do
    true
  end

  defp is_base_char(x) when x >= 248 and x <= 255 do
    true
  end

  defp is_base_char(x) when x >= 256 and x <= 305 do
    true
  end

  defp is_base_char(x) when x >= 308 and x <= 318 do
    true
  end

  defp is_base_char(x) when x >= 321 and x <= 328 do
    true
  end

  defp is_base_char(x) when x >= 330 and x <= 382 do
    true
  end

  defp is_base_char(x) when x >= 384 and x <= 451 do
    true
  end

  defp is_base_char(x) when x >= 461 and x <= 496 do
    true
  end

  defp is_base_char(x) when x >= 500 and x <= 501 do
    true
  end

  defp is_base_char(x) when x >= 506 and x <= 535 do
    true
  end

  defp is_base_char(x) when x >= 592 and x <= 680 do
    true
  end

  defp is_base_char(x) when x >= 699 and x <= 705 do
    true
  end

  defp is_base_char(902) do
    true
  end

  defp is_base_char(x) when x >= 904 and x <= 906 do
    true
  end

  defp is_base_char(908) do
    true
  end

  defp is_base_char(x) when x >= 910 and x <= 929 do
    true
  end

  defp is_base_char(x) when x >= 931 and x <= 974 do
    true
  end

  defp is_base_char(x) when x >= 976 and x <= 982 do
    true
  end

  defp is_base_char(986) do
    true
  end

  defp is_base_char(988) do
    true
  end

  defp is_base_char(990) do
    true
  end

  defp is_base_char(992) do
    true
  end

  defp is_base_char(x) when x >= 994 and x <= 1011 do
    true
  end

  defp is_base_char(x) when x >= 1025 and x <= 1036 do
    true
  end

  defp is_base_char(x) when x >= 1038 and x <= 1103 do
    true
  end

  defp is_base_char(x) when x >= 1105 and x <= 1116 do
    true
  end

  defp is_base_char(x) when x >= 1118 and x <= 1153 do
    true
  end

  defp is_base_char(x) when x >= 1168 and x <= 1220 do
    true
  end

  defp is_base_char(x) when x >= 1223 and x <= 1224 do
    true
  end

  defp is_base_char(x) when x >= 1227 and x <= 1228 do
    true
  end

  defp is_base_char(x) when x >= 1232 and x <= 1259 do
    true
  end

  defp is_base_char(x) when x >= 1262 and x <= 1269 do
    true
  end

  defp is_base_char(x) when x >= 1272 and x <= 1273 do
    true
  end

  defp is_base_char(x) when x >= 1329 and x <= 1366 do
    true
  end

  defp is_base_char(1369) do
    true
  end

  defp is_base_char(x) when x >= 1377 and x <= 1414 do
    true
  end

  defp is_base_char(x) when x >= 1488 and x <= 1514 do
    true
  end

  defp is_base_char(x) when x >= 1520 and x <= 1522 do
    true
  end

  defp is_base_char(x) when x >= 1569 and x <= 1594 do
    true
  end

  defp is_base_char(x) when x >= 1601 and x <= 1610 do
    true
  end

  defp is_base_char(x) when x >= 1649 and x <= 1719 do
    true
  end

  defp is_base_char(x) when x >= 1722 and x <= 1726 do
    true
  end

  defp is_base_char(x) when x >= 1728 and x <= 1742 do
    true
  end

  defp is_base_char(x) when x >= 1744 and x <= 1747 do
    true
  end

  defp is_base_char(1749) do
    true
  end

  defp is_base_char(x) when x >= 1765 and x <= 1766 do
    true
  end

  defp is_base_char(x) when x >= 2309 and x <= 2361 do
    true
  end

  defp is_base_char(2365) do
    true
  end

  defp is_base_char(x) when x >= 2392 and x <= 2401 do
    true
  end

  defp is_base_char(x) when x >= 2437 and x <= 2444 do
    true
  end

  defp is_base_char(x) when x >= 2447 and x <= 2448 do
    true
  end

  defp is_base_char(x) when x >= 2451 and x <= 2472 do
    true
  end

  defp is_base_char(x) when x >= 2474 and x <= 2480 do
    true
  end

  defp is_base_char(2482) do
    true
  end

  defp is_base_char(x) when x >= 2486 and x <= 2489 do
    true
  end

  defp is_base_char(x) when x >= 2524 and x <= 2525 do
    true
  end

  defp is_base_char(x) when x >= 2527 and x <= 2529 do
    true
  end

  defp is_base_char(x) when x >= 2544 and x <= 2545 do
    true
  end

  defp is_base_char(x) when x >= 2565 and x <= 2570 do
    true
  end

  defp is_base_char(x) when x >= 2575 and x <= 2576 do
    true
  end

  defp is_base_char(x) when x >= 2579 and x <= 2600 do
    true
  end

  defp is_base_char(x) when x >= 2602 and x <= 2608 do
    true
  end

  defp is_base_char(x) when x >= 2610 and x <= 2611 do
    true
  end

  defp is_base_char(x) when x >= 2613 and x <= 2614 do
    true
  end

  defp is_base_char(x) when x >= 2616 and x <= 2617 do
    true
  end

  defp is_base_char(x) when x >= 2649 and x <= 2652 do
    true
  end

  defp is_base_char(2654) do
    true
  end

  defp is_base_char(x) when x >= 2674 and x <= 2676 do
    true
  end

  defp is_base_char(x) when x >= 2693 and x <= 2699 do
    true
  end

  defp is_base_char(2701) do
    true
  end

  defp is_base_char(x) when x >= 2703 and x <= 2705 do
    true
  end

  defp is_base_char(x) when x >= 2707 and x <= 2728 do
    true
  end

  defp is_base_char(x) when x >= 2730 and x <= 2736 do
    true
  end

  defp is_base_char(x) when x >= 2738 and x <= 2739 do
    true
  end

  defp is_base_char(x) when x >= 2741 and x <= 2745 do
    true
  end

  defp is_base_char(2749) do
    true
  end

  defp is_base_char(2784) do
    true
  end

  defp is_base_char(x) when x >= 2821 and x <= 2828 do
    true
  end

  defp is_base_char(x) when x >= 2831 and x <= 2832 do
    true
  end

  defp is_base_char(x) when x >= 2835 and x <= 2856 do
    true
  end

  defp is_base_char(x) when x >= 2858 and x <= 2864 do
    true
  end

  defp is_base_char(x) when x >= 2866 and x <= 2867 do
    true
  end

  defp is_base_char(x) when x >= 2870 and x <= 2873 do
    true
  end

  defp is_base_char(2877) do
    true
  end

  defp is_base_char(x) when x >= 2908 and x <= 2909 do
    true
  end

  defp is_base_char(x) when x >= 2911 and x <= 2913 do
    true
  end

  defp is_base_char(x) when x >= 2949 and x <= 2954 do
    true
  end

  defp is_base_char(x) when x >= 2958 and x <= 2960 do
    true
  end

  defp is_base_char(x) when x >= 2962 and x <= 2965 do
    true
  end

  defp is_base_char(x) when x >= 2969 and x <= 2970 do
    true
  end

  defp is_base_char(2972) do
    true
  end

  defp is_base_char(x) when x >= 2974 and x <= 2975 do
    true
  end

  defp is_base_char(x) when x >= 2979 and x <= 2980 do
    true
  end

  defp is_base_char(x) when x >= 2984 and x <= 2986 do
    true
  end

  defp is_base_char(x) when x >= 2990 and x <= 2997 do
    true
  end

  defp is_base_char(x) when x >= 2999 and x <= 3001 do
    true
  end

  defp is_base_char(x) when x >= 3077 and x <= 3084 do
    true
  end

  defp is_base_char(x) when x >= 3086 and x <= 3088 do
    true
  end

  defp is_base_char(x) when x >= 3090 and x <= 3112 do
    true
  end

  defp is_base_char(x) when x >= 3114 and x <= 3123 do
    true
  end

  defp is_base_char(x) when x >= 3125 and x <= 3129 do
    true
  end

  defp is_base_char(x) when x >= 3168 and x <= 3169 do
    true
  end

  defp is_base_char(x) when x >= 3205 and x <= 3212 do
    true
  end

  defp is_base_char(x) when x >= 3214 and x <= 3216 do
    true
  end

  defp is_base_char(x) when x >= 3218 and x <= 3240 do
    true
  end

  defp is_base_char(x) when x >= 3242 and x <= 3251 do
    true
  end

  defp is_base_char(x) when x >= 3253 and x <= 3257 do
    true
  end

  defp is_base_char(3294) do
    true
  end

  defp is_base_char(x) when x >= 3296 and x <= 3297 do
    true
  end

  defp is_base_char(x) when x >= 3333 and x <= 3340 do
    true
  end

  defp is_base_char(x) when x >= 3342 and x <= 3344 do
    true
  end

  defp is_base_char(x) when x >= 3346 and x <= 3368 do
    true
  end

  defp is_base_char(x) when x >= 3370 and x <= 3385 do
    true
  end

  defp is_base_char(x) when x >= 3424 and x <= 3425 do
    true
  end

  defp is_base_char(x) when x >= 3585 and x <= 3630 do
    true
  end

  defp is_base_char(3632) do
    true
  end

  defp is_base_char(x) when x >= 3634 and x <= 3635 do
    true
  end

  defp is_base_char(x) when x >= 3648 and x <= 3653 do
    true
  end

  defp is_base_char(x) when x >= 3713 and x <= 3714 do
    true
  end

  defp is_base_char(3716) do
    true
  end

  defp is_base_char(x) when x >= 3719 and x <= 3720 do
    true
  end

  defp is_base_char(3722) do
    true
  end

  defp is_base_char(3725) do
    true
  end

  defp is_base_char(x) when x >= 3732 and x <= 3735 do
    true
  end

  defp is_base_char(x) when x >= 3737 and x <= 3743 do
    true
  end

  defp is_base_char(x) when x >= 3745 and x <= 3747 do
    true
  end

  defp is_base_char(3749) do
    true
  end

  defp is_base_char(3751) do
    true
  end

  defp is_base_char(x) when x >= 3754 and x <= 3755 do
    true
  end

  defp is_base_char(x) when x >= 3757 and x <= 3758 do
    true
  end

  defp is_base_char(3760) do
    true
  end

  defp is_base_char(x) when x >= 3762 and x <= 3763 do
    true
  end

  defp is_base_char(3773) do
    true
  end

  defp is_base_char(x) when x >= 3776 and x <= 3780 do
    true
  end

  defp is_base_char(x) when x >= 3904 and x <= 3911 do
    true
  end

  defp is_base_char(x) when x >= 3913 and x <= 3945 do
    true
  end

  defp is_base_char(x) when x >= 4256 and x <= 4293 do
    true
  end

  defp is_base_char(x) when x >= 4304 and x <= 4342 do
    true
  end

  defp is_base_char(4352) do
    true
  end

  defp is_base_char(x) when x >= 4354 and x <= 4355 do
    true
  end

  defp is_base_char(x) when x >= 4357 and x <= 4359 do
    true
  end

  defp is_base_char(4361) do
    true
  end

  defp is_base_char(x) when x >= 4363 and x <= 4364 do
    true
  end

  defp is_base_char(x) when x >= 4366 and x <= 4370 do
    true
  end

  defp is_base_char(4412) do
    true
  end

  defp is_base_char(4414) do
    true
  end

  defp is_base_char(4416) do
    true
  end

  defp is_base_char(4428) do
    true
  end

  defp is_base_char(4430) do
    true
  end

  defp is_base_char(4432) do
    true
  end

  defp is_base_char(x) when x >= 4436 and x <= 4437 do
    true
  end

  defp is_base_char(4441) do
    true
  end

  defp is_base_char(x) when x >= 4447 and x <= 4449 do
    true
  end

  defp is_base_char(4451) do
    true
  end

  defp is_base_char(4453) do
    true
  end

  defp is_base_char(4455) do
    true
  end

  defp is_base_char(4457) do
    true
  end

  defp is_base_char(x) when x >= 4461 and x <= 4462 do
    true
  end

  defp is_base_char(x) when x >= 4466 and x <= 4467 do
    true
  end

  defp is_base_char(4469) do
    true
  end

  defp is_base_char(4510) do
    true
  end

  defp is_base_char(4520) do
    true
  end

  defp is_base_char(4523) do
    true
  end

  defp is_base_char(x) when x >= 4526 and x <= 4527 do
    true
  end

  defp is_base_char(x) when x >= 4535 and x <= 4536 do
    true
  end

  defp is_base_char(4538) do
    true
  end

  defp is_base_char(x) when x >= 4540 and x <= 4546 do
    true
  end

  defp is_base_char(4587) do
    true
  end

  defp is_base_char(4592) do
    true
  end

  defp is_base_char(4601) do
    true
  end

  defp is_base_char(x) when x >= 7680 and x <= 7835 do
    true
  end

  defp is_base_char(x) when x >= 7840 and x <= 7929 do
    true
  end

  defp is_base_char(x) when x >= 7936 and x <= 7957 do
    true
  end

  defp is_base_char(x) when x >= 7960 and x <= 7965 do
    true
  end

  defp is_base_char(x) when x >= 7968 and x <= 8005 do
    true
  end

  defp is_base_char(x) when x >= 8008 and x <= 8013 do
    true
  end

  defp is_base_char(x) when x >= 8016 and x <= 8023 do
    true
  end

  defp is_base_char(8025) do
    true
  end

  defp is_base_char(8027) do
    true
  end

  defp is_base_char(8029) do
    true
  end

  defp is_base_char(x) when x >= 8031 and x <= 8061 do
    true
  end

  defp is_base_char(x) when x >= 8064 and x <= 8116 do
    true
  end

  defp is_base_char(x) when x >= 8118 and x <= 8124 do
    true
  end

  defp is_base_char(8126) do
    true
  end

  defp is_base_char(x) when x >= 8130 and x <= 8132 do
    true
  end

  defp is_base_char(x) when x >= 8134 and x <= 8140 do
    true
  end

  defp is_base_char(x) when x >= 8144 and x <= 8147 do
    true
  end

  defp is_base_char(x) when x >= 8150 and x <= 8155 do
    true
  end

  defp is_base_char(x) when x >= 8160 and x <= 8172 do
    true
  end

  defp is_base_char(x) when x >= 8178 and x <= 8180 do
    true
  end

  defp is_base_char(x) when x >= 8182 and x <= 8188 do
    true
  end

  defp is_base_char(8486) do
    true
  end

  defp is_base_char(x) when x >= 8490 and x <= 8491 do
    true
  end

  defp is_base_char(8494) do
    true
  end

  defp is_base_char(x) when x >= 8576 and x <= 8578 do
    true
  end

  defp is_base_char(x) when x >= 12353 and x <= 12436 do
    true
  end

  defp is_base_char(x) when x >= 12449 and x <= 12538 do
    true
  end

  defp is_base_char(x) when x >= 12549 and x <= 12588 do
    true
  end

  defp is_base_char(x) when x >= 44032 and x <= 55203 do
    true
  end

  defp is_base_char(_) do
    false
  end

  defp is_ideographic(x) when x >= 19968 and x <= 40869 do
    true
  end

  defp is_ideographic(12295) do
    true
  end

  defp is_ideographic(x) when x >= 12321 and x <= 12329 do
    true
  end

  defp is_ideographic(_) do
    false
  end

  defp is_combining_char(x) when x >= 768 and x <= 837 do
    true
  end

  defp is_combining_char(x) when x >= 864 and x <= 865 do
    true
  end

  defp is_combining_char(x) when x >= 1155 and x <= 1158 do
    true
  end

  defp is_combining_char(x) when x >= 1425 and x <= 1441 do
    true
  end

  defp is_combining_char(x) when x >= 1443 and x <= 1465 do
    true
  end

  defp is_combining_char(x) when x >= 1467 and x <= 1469 do
    true
  end

  defp is_combining_char(1471) do
    true
  end

  defp is_combining_char(x) when x >= 1473 and x <= 1474 do
    true
  end

  defp is_combining_char(1476) do
    true
  end

  defp is_combining_char(x) when x >= 1611 and x <= 1618 do
    true
  end

  defp is_combining_char(1648) do
    true
  end

  defp is_combining_char(x) when x >= 1750 and x <= 1756 do
    true
  end

  defp is_combining_char(x) when x >= 1757 and x <= 1759 do
    true
  end

  defp is_combining_char(x) when x >= 1760 and x <= 1764 do
    true
  end

  defp is_combining_char(x) when x >= 1767 and x <= 1768 do
    true
  end

  defp is_combining_char(x) when x >= 1770 and x <= 1773 do
    true
  end

  defp is_combining_char(x) when x >= 2305 and x <= 2307 do
    true
  end

  defp is_combining_char(2364) do
    true
  end

  defp is_combining_char(x) when x >= 2366 and x <= 2380 do
    true
  end

  defp is_combining_char(2381) do
    true
  end

  defp is_combining_char(x) when x >= 2385 and x <= 2388 do
    true
  end

  defp is_combining_char(x) when x >= 2402 and x <= 2403 do
    true
  end

  defp is_combining_char(x) when x >= 2433 and x <= 2435 do
    true
  end

  defp is_combining_char(2492) do
    true
  end

  defp is_combining_char(2494) do
    true
  end

  defp is_combining_char(2495) do
    true
  end

  defp is_combining_char(x) when x >= 2496 and x <= 2500 do
    true
  end

  defp is_combining_char(x) when x >= 2503 and x <= 2504 do
    true
  end

  defp is_combining_char(x) when x >= 2507 and x <= 2509 do
    true
  end

  defp is_combining_char(2519) do
    true
  end

  defp is_combining_char(x) when x >= 2530 and x <= 2531 do
    true
  end

  defp is_combining_char(2562) do
    true
  end

  defp is_combining_char(2620) do
    true
  end

  defp is_combining_char(2622) do
    true
  end

  defp is_combining_char(2623) do
    true
  end

  defp is_combining_char(x) when x >= 2624 and x <= 2626 do
    true
  end

  defp is_combining_char(x) when x >= 2631 and x <= 2632 do
    true
  end

  defp is_combining_char(x) when x >= 2635 and x <= 2637 do
    true
  end

  defp is_combining_char(x) when x >= 2672 and x <= 2673 do
    true
  end

  defp is_combining_char(x) when x >= 2689 and x <= 2691 do
    true
  end

  defp is_combining_char(2748) do
    true
  end

  defp is_combining_char(x) when x >= 2750 and x <= 2757 do
    true
  end

  defp is_combining_char(x) when x >= 2759 and x <= 2761 do
    true
  end

  defp is_combining_char(x) when x >= 2763 and x <= 2765 do
    true
  end

  defp is_combining_char(x) when x >= 2817 and x <= 2819 do
    true
  end

  defp is_combining_char(2876) do
    true
  end

  defp is_combining_char(x) when x >= 2878 and x <= 2883 do
    true
  end

  defp is_combining_char(x) when x >= 2887 and x <= 2888 do
    true
  end

  defp is_combining_char(x) when x >= 2891 and x <= 2893 do
    true
  end

  defp is_combining_char(x) when x >= 2902 and x <= 2903 do
    true
  end

  defp is_combining_char(x) when x >= 2946 and x <= 2947 do
    true
  end

  defp is_combining_char(x) when x >= 3006 and x <= 3010 do
    true
  end

  defp is_combining_char(x) when x >= 3014 and x <= 3016 do
    true
  end

  defp is_combining_char(x) when x >= 3018 and x <= 3021 do
    true
  end

  defp is_combining_char(3031) do
    true
  end

  defp is_combining_char(x) when x >= 3073 and x <= 3075 do
    true
  end

  defp is_combining_char(x) when x >= 3134 and x <= 3140 do
    true
  end

  defp is_combining_char(x) when x >= 3142 and x <= 3144 do
    true
  end

  defp is_combining_char(x) when x >= 3146 and x <= 3149 do
    true
  end

  defp is_combining_char(x) when x >= 3157 and x <= 3158 do
    true
  end

  defp is_combining_char(x) when x >= 3202 and x <= 3203 do
    true
  end

  defp is_combining_char(x) when x >= 3262 and x <= 3268 do
    true
  end

  defp is_combining_char(x) when x >= 3270 and x <= 3272 do
    true
  end

  defp is_combining_char(x) when x >= 3274 and x <= 3277 do
    true
  end

  defp is_combining_char(x) when x >= 3285 and x <= 3286 do
    true
  end

  defp is_combining_char(x) when x >= 3330 and x <= 3331 do
    true
  end

  defp is_combining_char(x) when x >= 3390 and x <= 3395 do
    true
  end

  defp is_combining_char(x) when x >= 3398 and x <= 3400 do
    true
  end

  defp is_combining_char(x) when x >= 3402 and x <= 3405 do
    true
  end

  defp is_combining_char(3415) do
    true
  end

  defp is_combining_char(3633) do
    true
  end

  defp is_combining_char(x) when x >= 3636 and x <= 3642 do
    true
  end

  defp is_combining_char(x) when x >= 3655 and x <= 3662 do
    true
  end

  defp is_combining_char(3761) do
    true
  end

  defp is_combining_char(x) when x >= 3764 and x <= 3769 do
    true
  end

  defp is_combining_char(x) when x >= 3771 and x <= 3772 do
    true
  end

  defp is_combining_char(x) when x >= 3784 and x <= 3789 do
    true
  end

  defp is_combining_char(x) when x >= 3864 and x <= 3865 do
    true
  end

  defp is_combining_char(3893) do
    true
  end

  defp is_combining_char(3895) do
    true
  end

  defp is_combining_char(3897) do
    true
  end

  defp is_combining_char(3902) do
    true
  end

  defp is_combining_char(3903) do
    true
  end

  defp is_combining_char(x) when x >= 3953 and x <= 3972 do
    true
  end

  defp is_combining_char(x) when x >= 3974 and x <= 3979 do
    true
  end

  defp is_combining_char(x) when x >= 3984 and x <= 3989 do
    true
  end

  defp is_combining_char(3991) do
    true
  end

  defp is_combining_char(x) when x >= 3993 and x <= 4013 do
    true
  end

  defp is_combining_char(x) when x >= 4017 and x <= 4023 do
    true
  end

  defp is_combining_char(4025) do
    true
  end

  defp is_combining_char(x) when x >= 8400 and x <= 8412 do
    true
  end

  defp is_combining_char(8417) do
    true
  end

  defp is_combining_char(x) when x >= 12330 and x <= 12335 do
    true
  end

  defp is_combining_char(12441) do
    true
  end

  defp is_combining_char(12442) do
    true
  end

  defp is_combining_char(_) do
    false
  end

  defp is_digit(x) when x >= 48 and x <= 57 do
    true
  end

  defp is_digit(x) when x >= 1632 and x <= 1641 do
    true
  end

  defp is_digit(x) when x >= 1776 and x <= 1785 do
    true
  end

  defp is_digit(x) when x >= 2406 and x <= 2415 do
    true
  end

  defp is_digit(x) when x >= 2534 and x <= 2543 do
    true
  end

  defp is_digit(x) when x >= 2662 and x <= 2671 do
    true
  end

  defp is_digit(x) when x >= 2790 and x <= 2799 do
    true
  end

  defp is_digit(x) when x >= 2918 and x <= 2927 do
    true
  end

  defp is_digit(x) when x >= 3047 and x <= 3055 do
    true
  end

  defp is_digit(x) when x >= 3174 and x <= 3183 do
    true
  end

  defp is_digit(x) when x >= 3302 and x <= 3311 do
    true
  end

  defp is_digit(x) when x >= 3430 and x <= 3439 do
    true
  end

  defp is_digit(x) when x >= 3664 and x <= 3673 do
    true
  end

  defp is_digit(x) when x >= 3792 and x <= 3801 do
    true
  end

  defp is_digit(x) when x >= 3872 and x <= 3881 do
    true
  end

  defp is_digit(_) do
    false
  end

  defp is_extender(183) do
    true
  end

  defp is_extender(720) do
    true
  end

  defp is_extender(721) do
    true
  end

  defp is_extender(903) do
    true
  end

  defp is_extender(1600) do
    true
  end

  defp is_extender(3654) do
    true
  end

  defp is_extender(3782) do
    true
  end

  defp is_extender(12293) do
    true
  end

  defp is_extender(x) when x >= 12337 and x <= 12341 do
    true
  end

  defp is_extender(x) when x >= 12445 and x <= 12446 do
    true
  end

  defp is_extender(x) when x >= 12540 and x <= 12542 do
    true
  end

  defp is_extender(_) do
    false
  end

  def to_lower(str) do
    to_lower(str, [])
  end

  defp to_lower([c | cs], acc) when c >= ?A and c <= ?Z do
    to_lower(cs, [c + (?a - ?A) | acc])
  end

  defp to_lower([c | cs], acc) do
    to_lower(cs, [c | acc])
  end

  defp to_lower([], acc) do
    :lists.reverse(acc)
  end

  def is_facet(:length) do
    true
  end

  def is_facet(:minLength) do
    true
  end

  def is_facet(:maxLength) do
    true
  end

  def is_facet(:pattern) do
    true
  end

  def is_facet(:enumeration) do
    true
  end

  def is_facet(:whiteSpace) do
    true
  end

  def is_facet(:maxInclusive) do
    true
  end

  def is_facet(:maxExclusive) do
    true
  end

  def is_facet(:minInclusive) do
    true
  end

  def is_facet(:minExclusive) do
    true
  end

  def is_facet(:totalDigits) do
    true
  end

  def is_facet(:fractionDigits) do
    true
  end

  def is_facet(_) do
    false
  end

  def is_builtin_simple_type({type, _, :"http://www.w3.org/2001/XMLSchema"}) when is_atom(type) do
    is_builtin_simple_type(:erlang.atom_to_list(type))
  end

  def is_builtin_simple_type({type, _, :"http://www.w3.org/2001/XMLSchema"}) do
    is_builtin_simple_type(type)
  end

  def is_builtin_simple_type({_, _, _}) do
    false
  end

  def is_builtin_simple_type('string') do
    true
  end

  def is_builtin_simple_type('normalizedString') do
    true
  end

  def is_builtin_simple_type('token') do
    true
  end

  def is_builtin_simple_type('base64Binary') do
    true
  end

  def is_builtin_simple_type('hexBinary') do
    true
  end

  def is_builtin_simple_type('integer') do
    true
  end

  def is_builtin_simple_type('positiveInteger') do
    true
  end

  def is_builtin_simple_type('negativeInteger') do
    true
  end

  def is_builtin_simple_type('nonNegativeInteger') do
    true
  end

  def is_builtin_simple_type('nonPositiveInteger') do
    true
  end

  def is_builtin_simple_type('long') do
    true
  end

  def is_builtin_simple_type('unsignedLong') do
    true
  end

  def is_builtin_simple_type('int') do
    true
  end

  def is_builtin_simple_type('unsignedInt') do
    true
  end

  def is_builtin_simple_type('short') do
    true
  end

  def is_builtin_simple_type('unsignedShort') do
    true
  end

  def is_builtin_simple_type('decimal') do
    true
  end

  def is_builtin_simple_type('float') do
    true
  end

  def is_builtin_simple_type('double') do
    true
  end

  def is_builtin_simple_type('boolean') do
    true
  end

  def is_builtin_simple_type('duration') do
    true
  end

  def is_builtin_simple_type('dateTime') do
    true
  end

  def is_builtin_simple_type('date') do
    true
  end

  def is_builtin_simple_type('time') do
    true
  end

  def is_builtin_simple_type('gYear') do
    true
  end

  def is_builtin_simple_type('gYearMonth') do
    true
  end

  def is_builtin_simple_type('gMonth') do
    true
  end

  def is_builtin_simple_type('gMonthDay') do
    true
  end

  def is_builtin_simple_type('gDay') do
    true
  end

  def is_builtin_simple_type('Name') do
    true
  end

  def is_builtin_simple_type('QName') do
    true
  end

  def is_builtin_simple_type('NCName') do
    true
  end

  def is_builtin_simple_type('anyURI') do
    true
  end

  def is_builtin_simple_type('language') do
    true
  end

  def is_builtin_simple_type('ID') do
    true
  end

  def is_builtin_simple_type('IDREF') do
    true
  end

  def is_builtin_simple_type('IDREFS') do
    true
  end

  def is_builtin_simple_type('ENTITY') do
    true
  end

  def is_builtin_simple_type('ENTITIES') do
    true
  end

  def is_builtin_simple_type('NOTATION') do
    true
  end

  def is_builtin_simple_type('NMTOKEN') do
    true
  end

  def is_builtin_simple_type('NMTOKENS') do
    true
  end

  def is_builtin_simple_type('byte') do
    true
  end

  def is_builtin_simple_type('unsignedByte') do
    true
  end

  def is_builtin_simple_type(_) do
    false
  end

  def is_xsd_string({type, _, :"http://www.w3.org/2001/XMLSchema"}) when is_atom(type) do
    is_xsd_string(type)
  end

  def is_xsd_string({type, _, :"http://www.w3.org/2001/XMLSchema"}) do
    is_xsd_string(type)
  end

  def is_xsd_string({_, _, _}) do
    false
  end

  def is_xsd_string(atom) when is_atom(atom) do
    is_xsd_string(:erlang.atom_to_list(atom))
  end

  def is_xsd_string('string') do
    true
  end

  def is_xsd_string('normalizedString') do
    true
  end

  def is_xsd_string('token') do
    true
  end

  def is_xsd_string('language') do
    true
  end

  def is_xsd_string('Name') do
    true
  end

  def is_xsd_string('NMTOKEN') do
    true
  end

  def is_xsd_string('NMTOKENS') do
    true
  end

  def is_xsd_string('NCName') do
    true
  end

  def is_xsd_string('ID') do
    true
  end

  def is_xsd_string('IDREF') do
    true
  end

  def is_xsd_string('IDREFS') do
    true
  end

  def is_xsd_string('ENTITY') do
    true
  end

  def is_xsd_string('ENTITIES') do
    true
  end

  def is_xsd_string(_) do
    false
  end
end
