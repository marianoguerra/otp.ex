defmodule :m_xmerl_xsd do
  use Bitwise

  import :lists,
    only: [
      filter: 2,
      flatten: 1,
      foldl: 3,
      keydelete: 3,
      keymember: 3,
      keyreplace: 4,
      keysearch: 3,
      map: 2,
      mapfoldl: 3,
      member: 2,
      reverse: 1,
      reverse: 2,
      splitwith: 2
    ]

  import :xmerl_lib, only: [is_builtin_simple_type: 1, is_facet: 1, is_xsd_string: 1]
  import :xmerl_xsd_type, only: [facet_fun: 2]
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

  Record.defrecord(:r_file_info, :file_info,
    size: :undefined,
    type: :undefined,
    access: :undefined,
    atime: :undefined,
    mtime: :undefined,
    ctime: :undefined,
    mode: :undefined,
    links: :undefined,
    major_device: :undefined,
    minor_device: :undefined,
    inode: :undefined,
    uid: :undefined,
    gid: :undefined
  )

  Record.defrecord(:r_file_descriptor, :file_descriptor,
    module: :undefined,
    data: :undefined
  )

  def validate(xml, state) do
    validate(xml, state, [])
  end

  def validate(xml, state, opts)
      when elem(state, 0) === :xsd_state do
    s2 = initiate_state2(state, opts)
    s3 = validation_options(s2, opts)
    validate3(r_xsd_state(s3, :schema_name), xml, s3)
  end

  def state2file(s = r_xsd_state(schema_name: sN)) do
    state2file(s, :filename.rootname(sN))
  end

  def state2file(s, fileName) when elem(s, 0) === :xsd_state do
    save_xsd_state(s)

    case (try do
            :ets.tab2file(r_xsd_state(s, :table), :lists.append(fileName, '.xss'))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, reason} ->
        {:error, {[], :xmerl_xsd, reason}}

      ret ->
        ret
    end
  end

  def file2state(fileName) do
    case (try do
            :ets.file2tab(fileName)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, tab} ->
        case load_xsd_state(tab) do
          [{:state, s}] when elem(s, 0) === :xsd_state ->
            xmerl_xsd_vsn_check(s)

          other ->
            {:error, {[], :xmerl_xsd, {:incomplete_file, fileName, other}}}
        end

      {:error, reason} ->
        {:error, {[], :xmerl_xsd, reason}}

      other ->
        {:error, {[], :xmerl_xsd, other}}
    end
  end

  defp save_xsd_state(s) do
    try do
      :ets.insert(r_xsd_state(s, :table), {:state, s})
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp load_xsd_state(table) do
    try do
      :ets.lookup(table, :state)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp xmerl_xsd_vsn() do
    case :lists.keysearch(:vsn, 1, :xmerl_xsd.module_info(:attributes)) do
      {:value, {_, mD5_VSN}} ->
        mD5_VSN

      _ ->
        :undefined
    end
  end

  defp xmerl_xsd_vsn_check(s = r_xsd_state(vsn: mD5_VSN)) do
    case (for {:vsn, v} <- :xmerl_xsd.module_info(:attributes) do
            v
          end) do
      [^mD5_VSN] ->
        {:ok, s}

      _ ->
        {:error,
         {[], :xmerl_xsd, {:different_version_of_xmerl_xsd_module_used, :state_not_reliable}}}
    end
  end

  def process_validate(schema, xml) do
    process_validate(schema, xml, [])
  end

  def process_validate(schema, xml, opts) do
    targetNamespace = target_namespace(xml)

    case schema do
      [h | _] when is_list(h) or is_tuple(h) ->
        case process_schemas(
               schema,
               [
                 {:target_namespace, targetNamespace}
                 | opts
               ]
             ) do
          {:ok, s} ->
            s2 = validation_options(s, opts)
            validate3(r_xsd_state(s2, :schema_name), xml, s2)

          err ->
            err
        end

      _ ->
        process_validate2(:xmerl_scan.file(schema), schema, xml, opts)
    end
  end

  defp process_validate2(err = {:error, _}, _, _, _) do
    err
  end

  defp process_validate2({sE, _}, schema, xml, opts) do
    s = initiate_state(opts, schema)
    s1 = validate_schema(sE, s)
    s2 = validate_schema_ph2(s1)
    s3 = schema_concistence_checks(s2)
    s4 = validation_options(s3, opts)
    validate3(schema, xml, s4)
  end

  defp validate3(schema, xml, s = r_xsd_state(errors: [])) do
    ret =
      {_, s2} =
      case (try do
              validate_xml(xml, s)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        _Err = {:error, reason} ->
          {xml, acc_errs(s, reason)}

        {:EXIT, reason} ->
          {xml,
           acc_errs(
             s,
             {error_path(xml, r_xmlElement(xml, :name)), :xmerl_xsd,
              {:undefined, {:internal_error, reason}}}
           )}

        {xML2, rest, sx} ->
          case :lists.dropwhile(
                 fn
                   x
                   when elem(x, 0) === :xmlComment ->
                     true

                   _ ->
                     false
                 end,
                 rest
               ) do
            [] ->
              case xML2 do
                [xML3] ->
                  {xML3, sx}

                xML3 ->
                  {xML3, sx}
              end

            unValidated ->
              {xml,
               acc_errs(
                 sx,
                 {error_path(
                    unValidated,
                    r_xmlElement(xml, :name)
                  ), :xmerl_xsd, {:unvalidated_rest, unValidated}}
               )}
          end
      end

    save_to_file(s2, :filename.rootname(schema) ++ '.tab2')

    case r_xsd_state(s2, :errors) do
      [] ->
        ret

      l ->
        return_error(l)
    end
  end

  defp validate3(_, _, s) do
    return_schema_error(r_xsd_state(s, :errors))
  end

  def process_schema(schema) do
    process_schema(schema, [])
  end

  def process_schema(schema, options) when is_list(options) do
    state = initiate_state(options, schema)
    process_schema(schema, state)
  end

  def process_schema(schema, state = r_xsd_state(fetch_fun: fetch)) do
    case fetch.(schema, state) do
      {:ok, {:file, file}, _} ->
        process_schema2(:xmerl_scan.file(file), state, schema)

      {:ok, {:string, str}, _} ->
        process_schema2(:xmerl_scan.string(str), state, schema)

      {:ok, [], _} ->
        {:error, :enoent}

      err ->
        err
    end
  end

  defp process_schema2(err = {:error, _}, _, _) do
    err
  end

  defp process_schema2({sE, _}, state, _Schema) do
    s1 = validate_schema(sE, state)
    s2 = validate_schema_ph2(s1)

    case schema_concistence_checks(s2) do
      s3 = r_xsd_state(errors: []) ->
        {:ok, s3}

      s3 ->
        delete_table(s3)
        return_error(r_xsd_state(s3, :errors))
    end
  end

  def process_schemas(schemas) do
    process_schemas(schemas, [])
  end

  def process_schemas(schemas = [{_, schema} | _], options)
      when is_list(options) do
    state = initiate_state(options, schema)
    process_schemas(schemas, state)
  end

  def process_schemas(
        [{_NS, schema} | rest],
        state = r_xsd_state(fetch_fun: fetch)
      ) do
    res =
      case fetch.(schema, state) do
        {:ok, {:file, file}, _} ->
          process_schema2(:xmerl_scan.file(file), state, schema)

        {:ok, {:string, str}, _} ->
          process_schema2(:xmerl_scan.string(str), state, schema)

        {:ok, [], _} ->
          {:ok, state}

        err ->
          err
      end

    case res do
      {:ok, s2} ->
        process_schemas(rest, s2)

      _ ->
        res
    end
  end

  def process_schemas([], s) when elem(s, 0) === :xsd_state do
    {:ok, s}
  end

  defp initiate_state(opts, schema) do
    xSDBase = :filename.dirname(schema)
    {{:state, s}, restOpts} = new_state(opts)
    s2 = create_tables(s)

    initiate_state2(
      r_xsd_state(s2, schema_name: schema, xsd_base: xSDBase, fetch_fun: &fetch/2),
      restOpts
    )
  end

  defp initiate_state2(s, []) do
    s
  end

  defp initiate_state2(s, [{:tab2file, bool} | t]) do
    initiate_state2(r_xsd_state(s, tab2file: bool), t)
  end

  defp initiate_state2(s, [{:xsdbase, xSDBase} | t]) do
    initiate_state2(
      r_xsd_state(s,
        xsd_base: xSDBase,
        external_xsd_base: true
      ),
      t
    )
  end

  defp initiate_state2(s, [{:fetch_fun, fetchFun} | t]) do
    initiate_state2(r_xsd_state(s, fetch_fun: fetchFun), t)
  end

  defp initiate_state2(s, [{:fetch_path, fetchPath} | t]) do
    initiate_state2(r_xsd_state(s, fetch_path: fetchPath), t)
  end

  defp initiate_state2(s, [{:schema_preprocessed, bool} | t]) do
    initiate_state2(r_xsd_state(s, schema_preprocessed: bool), t)
  end

  defp initiate_state2(s, [{:target_namespace, _NS} | t]) do
    initiate_state2(s, t)
  end

  defp initiate_state2(s, [h | t]) do
    error_msg('~w: invalid option: ~p~n', [:xmerl_xsd, h])
    initiate_state2(s, t)
  end

  defp validation_options(s, [{:target_namespace, nS} | t]) do
    validation_options(
      r_xsd_state(s, targetNamespace: if_list_to_atom(nS)),
      t
    )
  end

  defp validation_options(s, [_H | t]) do
    validation_options(s, t)
  end

  defp validation_options(s, []) do
    s
  end

  defp new_state(opts) do
    xSD_VSN = xmerl_xsd_vsn()
    keysearch_delete(:state, 1, opts, {:state, r_xsd_state(vsn: xSD_VSN)})
  end

  defp validate_schema(e = r_xmlElement(), s) do
    s1 = r_xsd_state(s, targetNamespace: target_namespace(e))

    case is_already_processed(
           r_xsd_state(s1, :targetNamespace),
           s1
         ) do
      true ->
        save_namespace_definition(r_xsd_state(s1, :targetNamespace), s1)

      _ ->
        s2 = s1
        {cM, s3} = traverse_content(e, s2)
        save_schema_element(cM, s3)
        s3
    end
  end

  defp validate_schema_ph2(s = r_xsd_state(derived_types: [])) do
    s
  end

  defp validate_schema_ph2(s = r_xsd_state(derived_types: dT)) do
    deduce_derived_types(dT, s)
  end

  defp traverse_content(e = r_xmlElement(name: name), s) do
    case local_name(name) do
      :schema ->
        content = r_xmlElement(e, :content)
        thisNS = {'#this#', r_xsd_state(s, :schema_name), r_xsd_state(s, :targetNamespace)}

        s2 =
          r_xsd_state(s,
            checked_namespace_nodes:
              add_once(
                thisNS,
                r_xsd_state(s, :checked_namespace_nodes)
              )
          )

        s3 = namespace_nodes(e, s2)
        s4 = element_form_default(e, s3)
        s5 = attribute_form_default(e, s4)
        s6 = substitution_default(:finalDefault, e, s5)
        s7 = substitution_default(:blockDefault, e, s6)
        traverse_content2(content, s7, [])

      err ->
        exit({:error, {[], :xmerl_xsd, {:schema_error, err}}})
    end
  end

  defp traverse_content2([], s, acc) do
    {reverse(remove_annotation(acc)), reset_scope(s)}
  end

  defp traverse_content2([el | els], s, acc)
       when elem(el, 0) === :xmlElement do
    {object, s2} = element_content(kind(el, s), el, r_xsd_state(s, :scope))
    traverse_content2(els, s2, [object | acc])
  end

  defp traverse_content2([_T | els], s, acc) do
    traverse_content2(els, s, acc)
  end

  defp target_namespace(e) do
    case get_attribute_value(:targetNamespace, e, :undefined) do
      uRI when is_list(uRI) ->
        :erlang.list_to_atom(uRI)

      uRI ->
        uRI
    end
  end

  defp namespace_nodes(
         r_xmlElement(namespace: r_xmlNamespace(nodes: nS)),
         s =
           r_xsd_state(
             namespace_nodes: nSN,
             global_namespace_nodes: gNSN
           )
       ) do
    s2 = r_xsd_state(s, namespace_nodes: foldl(&add_once/2, nSN, nS))

    r_xsd_state(s2,
      global_namespace_nodes:
        add_key_once(
          r_xsd_state(s, :targetNamespace),
          1,
          {r_xsd_state(s, :targetNamespace), nS},
          gNSN
        )
    )
  end

  defp attribute_form_default(r_xmlElement(attributes: atts), s) do
    def__ = form_default(:attributeFormDefault, atts, s)
    r_xsd_state(s, attributeFormDefault: def__)
  end

  defp element_form_default(r_xmlElement(attributes: atts), s) do
    def__ = form_default(:elementFormDefault, atts, s)
    r_xsd_state(s, elementFormDefault: def__)
  end

  defp form_default(key, atts, _S) do
    case keyNsearch(key, r_xmlAttribute(:name), atts, :unqualified) do
      r_xmlAttribute(value: v) when is_list(v) ->
        :erlang.list_to_atom(v)

      r_xmlAttribute(value: v) ->
        v

      _ ->
        :unqualified
    end
  end

  defp substitution_default(subst = :finalDefault, el, s) do
    r_xsd_state(s, finalDefault: substitution(subst, el, s))
  end

  defp substitution_default(subst = :blockDefault, el, s) do
    r_xsd_state(s, blockDefault: substitution(subst, el, s))
  end

  defp substitution(subst, el, _S) do
    split_by_whitespace(
      get_attribute_value(subst, el, []),
      []
    )
  end

  defp element_content({:attribute, s = r_xsd_state(scope: scope)}, att, env) do
    case qualify_NCName(att, s) do
      :no_name ->
        ref = attribute_ref(att)
        attRef = {:attribute, get_QName(ref, r_xmlElement(att, :namespace), reset_scope(s))}
        {attRef, add_ref(s, attRef)}

      name ->
        {attrType, s2} = attribute_type(att, [name | env], s)
        s3 = check_cm(:attribute, allowed_content(:attribute, env), attrType, s2)

        {attr, s4} =
          attribute_properties(
            r_xmlElement(att, :attributes),
            r_schema_attribute(type: attrType),
            s3
          )

        object = {:attribute, r_schema_attribute(attr, name: name, scope: scope)}
        s5 = save_object(object, s4)
        {{:attribute, name}, s5}
    end
  end

  defp element_content({:element, s}, el, env) do
    case qualify_NCName(el, s) do
      :no_name ->
        ref = particle_ref(el)
        {occ, s2} = occurance(el, {1, 1}, s)
        s3 = element_forbidden_properties(el, s2)
        s4 = element_forbidden_content(r_xmlElement(el, :content), s3)
        elRef = {:element, {get_QName(ref, r_xmlElement(el, :namespace), reset_scope(s)), occ}}
        {elRef, add_ref(s4, elRef)}

      name ->
        {type, s2} = element_type(el, [name | env], s)
        s3 = check_cm(:element, allowed_content(:element, env), type, s2)
        type2 = remove_annotation(type)

        unique =
          for x = {:unique, _} <- type2 do
            x
          end

        key =
          for x = {k, _} <- type2,
              k == :key or k == :keyref do
            x
          end

        {occur, s4} = occurance(el, {1, 1}, s3)
        {sE, s5} = element_properties(r_xmlElement(el, :attributes), r_schema_element(), el, s4)

        cM =
          remove_attributes(
            for x = {y, _} <- type2,
                :unique !== y,
                :key !== y,
                :keyref !== y,
                :annotation !== y do
              x
            end
          )

        sE2 =
          r_schema_element(sE,
            name: name,
            type: cM,
            uniqueness: unique,
            key: key,
            occurance: occur,
            scope: r_xsd_state(s5, :scope)
          )

        s6 = insert_substitutionGroup(sE2, s5)
        s7 = save_object({:element, sE2}, s6)
        {{:element, {name, occur}}, s7}
    end
  end

  defp element_content({:complexType, s}, cT, env) do
    {sCT, s1} = c_t_properties(cT, r_schema_complex_type(), s)
    {mixed, s2} = mixed(cT, s1)
    complexity = complexity(r_xmlElement(cT, :content))

    {object, name, s7} =
      case qualify_NCName(cT, s2) do
        :no_name ->
          {cM, s3} =
            type(
              r_xmlElement(cT, :content),
              in_scope(:anonymous, s2),
              [:complexType | env]
            )

          s4 =
            check_cm(
              :complexType,
              allowed_content(
                :complexType,
                env
              ),
              cM,
              s3
            )

          name1 = get_QName(:_xmerl_no_name_, r_xmlElement(cT, :namespace), s4)
          s5 = set_scope(r_xsd_state(s, :scope), s4)
          {content, attributes} = split_content(remove_annotation(cM))
          sCT2 = base_type(content, sCT)

          cTObj =
            {:complexType,
             r_schema_complex_type(sCT2,
               name: name1,
               scope: r_xsd_state(s5, :scope),
               attributes: attributes,
               complexity: complexity,
               content:
                 mixify(
                   mixed,
                   content
                 )
             )}

          {cTObj, name1, s5}

        name2 ->
          s3 = in_scope(name2, s2)
          s3a = push_circularity_mark({:typeDef, name2}, s3)
          {cM, s4} = type(r_xmlElement(cT, :content), s3a, [:complexType | env])
          s4a = pop_circularity_mark({:typeDef, name2}, s4)

          s5 =
            check_cm(
              :complexType,
              allowed_content(
                :complexType,
                env
              ),
              cM,
              s4a
            )

          s6 = set_scope(r_xsd_state(s, :scope), s5)
          {content, attributes} = split_content(remove_annotation(cM))
          sCT2 = base_type(content, sCT)

          {{:complexType,
            r_schema_complex_type(sCT2,
              name: name2,
              scope: r_xsd_state(s6, :scope),
              attributes: attributes,
              complexity: complexity,
              content: mixify(mixed, content)
            )}, name2, s6}
      end

    s8 = save_object(object, s7)
    s9 = derived_type(object, s8)
    {{:complexType, name}, s9}
  end

  defp element_content({:attributeGroup, s}, aG, env) do
    case qualify_NCName(aG, s) do
      :no_name ->
        ref = attributeGroup_ref(aG)
        aGRef = {:attributeGroup, get_QName(ref, r_xmlElement(aG, :namespace), reset_scope(s))}
        {aGRef, add_ref(s, aGRef)}

      name ->
        {cM, s2} = type(r_xmlElement(aG, :content), in_scope(name, s), [:attributeGroup | env])
        s2_1 = out_scope(name, s2)
        s3 = check_cm(:attributeGroup, allowed_content(:attributeGroup, env), cM, s2_1)

        s4 =
          save_object(
            {:attributeGroup, r_schema_attribute_group(name: name, content: keep_attributes(cM))},
            s3
          )

        {{:attributeGroup, name}, s4}
    end
  end

  defp element_content({:group, s}, g, env) do
    case qualify_NCName(g, s) do
      :no_name ->
        ref = particle_ref(g)
        {occur, s2} = occurance(g, {1, 1}, s)
        gRef = {:group, {get_QName(ref, r_xmlElement(g, :namespace), reset_scope(s2)), occur}}
        {gRef, add_ref(s2, gRef)}

      name ->
        {cM, s2} = type(r_xmlElement(g, :content), in_scope(name, s), [:group | env])
        cM2 = recursive_redefine(name, cM, s2)
        s2_1 = out_scope(name, s2)
        s3 = check_cm(:group, allowed_content(:group, env), cM2, s2_1)

        s4 =
          save_object(
            {:group, r_schema_group(name: name, content: remove_annotation(cM2))},
            s3
          )

        {{:group, name}, s4}
    end
  end

  defp element_content({:all, s}, all, env) do
    {occur, s1} = occurance(all, {1, 1}, s)
    {cM, s2} = type(r_xmlElement(all, :content), s1, [:all | env])
    s3 = check_cm(:all, allowed_content(:all, env), cM, s2)

    {{:all,
      {for x = {:element, _} <- cM do
         x
       end, occur}}, s3}
  end

  defp element_content({:sequence, s}, seq, env) do
    {occur, s1} = occurance(seq, {1, 1}, s)
    {cM, s2} = type(r_xmlElement(seq, :content), s1, [:sequence | env])
    s3 = check_cm(:sequence, allowed_content(:sequence, env), cM, s2)
    {{:sequence, {remove_annotation(cM), occur}}, s3}
  end

  defp element_content({:choice, s}, choice, env) do
    {occur, s1} = occurance(choice, {1, 1}, s)
    {cM, s2} = type(r_xmlElement(choice, :content), s1, [:choice | env])
    s3 = check_cm(:choice, allowed_content(:choice, env), cM, s2)
    {{:choice, {remove_annotation(cM), occur}}, s3}
  end

  defp element_content({:any, s}, any, _Env) do
    {occur, s1} = occurance(any, {1, 1}, s)
    nameSpace = wildcard_namespace(any, s1)
    pC = processor_contents(any)
    :no_debug

    pred = fn
      e = r_xmlElement() ->
        case kind(e) do
          :annotation ->
            false

          _ ->
            true
        end

      _ ->
        false
    end

    s2 =
      case filter(pred, r_xmlElement(any, :content)) do
        [] ->
          s1

        err ->
          acc_errs(
            s1,
            {error_path(any, r_xmlElement(any, :name)), :xmerl_xsd,
             {:unexpected_content_in_any, err}}
          )
      end

    {{:any, {nameSpace, occur, pC}}, s2}
  end

  defp element_content({iDC, s}, el, env)
       when iDC == :unique or
              iDC == :key or iDC == :keyref do
    qName = qualify_NCName(el, reset_scope(s))
    ref = keyrefer(iDC, el, s)
    {selField, s2} = type(r_xmlElement(el, :content), s, [iDC | env])

    case {for x = {:selector, _} <- selField do
            x
          end,
          for x = {:field, _} <- selField do
            x
          end} do
      {[sel], fields = [_H | _T]} ->
        iDConstr =
          r_id_constraint(category: iDC, name: qName, refer: ref, selector: sel, fields: fields)

        s3 = save_idc(iDC, iDConstr, s2)
        {{iDC, iDConstr}, s3}

      err ->
        s3 =
          acc_errs(
            s2,
            {error_path(el, r_xmlElement(el, :name)), :xmerl_xsd,
             {:erroneous_content_in_identity_constraint, iDC, err}}
          )

        {{iDC, []}, s3}
    end
  end

  defp element_content({:selector, s}, sel, _Env) do
    case get_attribute_value(:xpath, sel, :error) do
      :error ->
        s2 =
          acc_errs(
            s,
            {error_path(sel, r_xmlElement(sel, :name)), :xmerl_xsd,
             {:missing_xpath_attribute, :selector}}
          )

        {{:selector, []}, s2}

      xPath ->
        {{:selector, xPath}, s}
    end
  end

  defp element_content({:field, s}, f, _Env) do
    case get_attribute_value(:xpath, f, :error) do
      :error ->
        s2 =
          acc_errs(
            s,
            {error_path(f, r_xmlElement(f, :name)), :xmerl_xsd,
             {:missing_xpath_attribute, :field}}
          )

        {{:field, []}, s2}

      xPath ->
        {{:field, xPath}, s}
    end
  end

  defp element_content({:notation, s}, _N, _Env) do
    {{:notation, []}, s}
  end

  defp element_content({:annotation, s}, _Ann, _Env) do
    {{:annotation, []}, s}
  end

  defp element_content({:appinfo, s}, _AI, _Env) do
    {{:appinfo, []}, s}
  end

  defp element_content({:documentation, s}, _D, _Env) do
    {{:documentation, []}, s}
  end

  defp element_content({:simpleType, s}, sT, env) do
    name =
      case qualify_NCName(sT, s) do
        :no_name ->
          get_QName(:_xmerl_no_name_, r_xmlElement(sT, :namespace), in_scope(:_xmerl_no_name_, s))

        qName ->
          qName
      end

    {type, s2} =
      type(
        r_xmlElement(sT, :content),
        push_circularity_mark(
          {:typeDef, name},
          in_scope(name, s)
        ),
        [:simpleType | env]
      )

    s2_1 = pop_circularity_mark({:typeDef, name}, s2)
    s3 = set_scope(r_xsd_state(s, :scope), s2_1)
    s4 = check_cm(:simpleType, allowed_content(:simpleType, env), type, s3)
    {baseType, facets} = facets(type, s4)
    variety = variety(type)
    final = simpleType_final(sT, s4)

    object =
      {:simpleType,
       r_schema_simple_type(
         name: name,
         base_type: baseType,
         final: final,
         facets: facets,
         variety: variety,
         content: remove_annotation(type),
         scope: r_xsd_state(s4, :scope)
       )}

    s5 = save_object(object, s4)
    s6 = derived_type(object, s5)
    {{:simpleType, name}, s6}
  end

  defp element_content({:restriction, s}, r, env) do
    {cM, s2} = type(r_xmlElement(r, :content), s, [:restriction | env])
    s3 = check_cm(:restriction, allowed_content(:restriction, env), cM, s2)
    {baseTypeName, cM2, s4} = restriction_base_type(r, cM, s3)
    baseTypeType = base_type_type(env)

    {{:restriction, {baseTypeName, remove_annotation(cM2)}},
     add_ref(s4, {baseTypeType, baseTypeName})}
  end

  defp element_content({:list, s = r_xsd_state(scope: scope)}, l, env) do
    {type, s2} = list_type(l, s, [:list | env])
    s3 = check_cm(:list, allowed_content(:list, scope), type, s2)
    {{:list, remove_annotation(type)}, s3}
  end

  defp element_content({:union, s = r_xsd_state(scope: scope)}, u, env) do
    {types, s2} = union_types(u, s, [:union | env])
    s3 = check_cm(:union, allowed_content(:union, scope), types, s2)
    {{:union, types}, s3}
  end

  defp element_content(
         {:include, s = r_xsd_state(schema_name: thisSchema, targetNamespace: tNS)},
         i,
         _Env
       ) do
    s2 = process_external_schema_once(i, r_xsd_state(s, :targetNamespace), s)
    {{:include, []}, r_xsd_state(s2, schema_name: thisSchema, targetNamespace: tNS)}
  end

  defp element_content(
         {:import,
          s =
            r_xsd_state(
              schema_name: thisSchema,
              targetNamespace: thisNameS
            )},
         i,
         _Env
       ) do
    namespace =
      case get_attribute_value(:namespace, i, :undefined) do
        l when is_list(l) ->
          :erlang.list_to_atom(l)

        a ->
          a
      end

    schemaLocation = get_attribute_value(:schemaLocation, i, :absent)
    s2 = process_external_schema_once(schemaLocation, namespace, s)

    {{:import, []},
     r_xsd_state(s2,
       schema_name: thisSchema,
       targetNamespace: thisNameS
     )}
  end

  defp element_content({:redefine, s = r_xsd_state(schema_name: thisSchema)}, rD, env) do
    s2 =
      process_external_schema_once(
        rD,
        r_xsd_state(s, :targetNamespace),
        r_xsd_state(s, errors: [])
      )

    case r_xsd_state(s2, :errors) do
      [] ->
        s3 =
          r_xsd_state(s2,
            schema_name: thisSchema,
            errors: r_xsd_state(s, :errors)
          )

        {cM, s4} =
          type(r_xmlElement(rD, :content), r_xsd_state(s3, redefine: true), [:redefine | env])

        s5 = r_xsd_state(s4, redefine: false)
        s6 = check_cm(:redefine, allowed_content(:redefine, env), cM, s5)
        s7 = redefine(cM, s6)
        {{:redefine, []}, s7}

      errs ->
        s3 =
          r_xsd_state(s2,
            schema_name: thisSchema,
            errors: errs ++ r_xsd_state(s, :errors)
          )

        {{:redefine, []}, s3}
    end
  end

  defp element_content({:anyAttribute, s}, aA, _Env) do
    nameSpace = wildcard_namespace(aA, s)
    pC = processor_contents(aA)

    pred = fn
      e = r_xmlElement() ->
        case kind(e) do
          :annotation ->
            false

          _ ->
            true
        end

      _ ->
        false
    end

    s2 =
      case filter(pred, r_xmlElement(aA, :content)) do
        [] ->
          s

        err ->
          acc_errs(
            s,
            {error_path(aA, r_xmlElement(aA, :name)), :xmerl_xsd, {:content_in_anyAttribute, err}}
          )
      end

    {{:anyAttribute, {nameSpace, pC}}, s2}
  end

  defp element_content({:simpleContent, s}, sC, env) do
    s2 =
      pre_check_cm(:simpleContent, r_xmlElement(sC, :content), mk_name(r_xsd_state(s, :scope)), s)

    case filter(
           fn
             x = r_xmlElement() ->
               case kind(x) do
                 :restriction ->
                   true

                 :extension ->
                   true

                 _ ->
                   false
               end

             _ ->
               false
           end,
           r_xmlElement(sC, :content)
         ) do
      [e] ->
        element_content(kind(e, s2), e, [:simpleContent | env])

      err ->
        {[],
         acc_errs(
           s2,
           {error_path(sC, r_xmlElement(sC, :name)), :xmerl_xsd, {:content_in_simpleContent, err}}
         )}
    end
  end

  defp element_content({:complexContent, s}, cC, env) do
    s2 =
      pre_check_cm(
        :complexContent,
        r_xmlElement(cC, :content),
        mk_name(r_xsd_state(s, :scope)),
        s
      )

    case filter(
           fn
             x = r_xmlElement() ->
               case kind(x) do
                 :restriction ->
                   true

                 :extension ->
                   true

                 _ ->
                   false
               end

             _ ->
               false
           end,
           r_xmlElement(cC, :content)
         ) do
      [e] ->
        element_content(kind(e, s2), e, [:complexContent | env])

      err ->
        {[],
         acc_errs(
           s2,
           {error_path(cC, r_xmlElement(cC, :name)), :xmerl_xsd,
            {:complexContent_content_failure, err}}
         )}
    end
  end

  defp element_content({:extension, s}, ext, env) do
    baseType = base_type(ext)
    {cM, s2} = type(r_xmlElement(ext, :content), s, [:extension | env])
    s3 = check_cm(:extension, allowed_content(:extension, r_xsd_state(s, :scope)), cM, s2)
    baseTypeName = get_QName(baseType, r_xmlElement(ext, :namespace), reset_scope(s))
    baseTypeType = base_type_type(env)
    {{:extension, {baseTypeName, cM}}, add_ref(s3, {baseTypeType, baseTypeName})}
  end

  defp element_content({:minExclusive, s}, cF, _Env) do
    value = get_value(cF)
    {{:minExclusive, value}, s}
  end

  defp element_content({:minInclusive, s}, cF, _Env) do
    value = get_value(cF)
    {{:minInclusive, value}, s}
  end

  defp element_content({:maxExclusive, s}, cF, _Env) do
    value = get_value(cF)
    {{:maxExclusive, value}, s}
  end

  defp element_content({:maxInclusive, s}, cF, _Env) do
    value = get_value(cF)
    {{:maxInclusive, value}, s}
  end

  defp element_content({:totalDigits, s}, cF, _Env) do
    value = get_value(cF)
    {{:totalDigits, value}, s}
  end

  defp element_content({:fractionDigits, s}, cF, _Env) do
    value = get_value(cF)
    {{:fractionDigits, value}, s}
  end

  defp element_content({:length, s}, cF, _Env) do
    value = get_value(cF)
    {{:length, value}, s}
  end

  defp element_content({:minLength, s}, cF, _Env) do
    value = get_value(cF)
    {{:minLength, value}, s}
  end

  defp element_content({:maxLength, s}, cF, _Env) do
    value = get_value(cF)
    {{:maxLength, value}, s}
  end

  defp element_content({:enumeration, s}, cF, _Env) do
    value = get_value(cF)
    {{:enumeration, value}, s}
  end

  defp element_content({:whiteSpace, s}, cF, _Env) do
    value = get_value(cF)
    {{:whiteSpace, value}, s}
  end

  defp element_content({:pattern, s}, cF, _Env) do
    value = get_value(cF)
    {{:pattern, value}, s}
  end

  defp element_content({other, s = r_xsd_state(errors: errs)}, c, _Env) do
    case errs do
      [] ->
        {[],
         acc_errs(
           s,
           {error_path(c, r_xmlElement(c, :name)), :xmerl_xsd, {:unknown_content, other}}
         )}

      _ ->
        {[], s}
    end
  end

  defp type(c, s, env) do
    type(c, s, env, [])
  end

  defp type([e = r_xmlElement() | els], s, env, acc) do
    {cM, s2} = element_content(kind(e, s), e, env)
    type(els, set_scope(r_xsd_state(s, :scope), s2), env, [cM | acc])
  end

  defp type([_H | els], s, env, acc) do
    type(els, s, env, acc)
  end

  defp type([], s, _Env, acc) do
    {flatten(reverse(acc)), s}
  end

  defp simpleUrType() do
    {:anySimpleType, []}
  end

  defp urType() do
    {:anyType, []}
  end

  defp attribute_type(att, env = [name | _], s) do
    {cM, s2} = type(r_xmlElement(att, :content), in_scope(name, s), env)

    case remove_annotation(cM) do
      [] ->
        case keyNsearch(:type, r_xmlAttribute(:name), r_xmlElement(att, :attributes), []) do
          r_xmlAttribute(value: simpleTypeName) ->
            typeRef =
              {:simpleType,
               get_QName(simpleTypeName, r_xmlElement(att, :namespace), reset_scope(s))}

            {[typeRef], set_scope(r_xsd_state(s, :scope), add_ref(s2, typeRef))}

          _ ->
            {[{:simpleType, simpleUrType()}], set_scope(r_xsd_state(s, :scope), s2)}
        end

      type ->
        {type, set_scope(r_xsd_state(s, :scope), s2)}
    end
  end

  defp element_type(el, env = [name | _], s) do
    {cM, s2} = type(r_xmlElement(el, :content), in_scope(name, s), env)

    case remove_annotation(cM) do
      [] ->
        case {get_attribute_value(:type, el, :no_name),
              get_attribute_value(:substitutionGroup, el, :undefined)} do
          {:no_name, sGName} when is_list(sGName) ->
            qN = get_QName(sGName, r_xmlElement(el, :namespace), reset_scope(s))

            case is_simple_type(qN, s2) do
              true ->
                exit(:this_can_never_happen)
                tRef = {:simpleType, qN}
                {[tRef], add_ref(set_scope(r_xsd_state(s, :scope), s2), tRef)}

              _ ->
                {[{:substitutionGroup, qN}], set_scope(r_xsd_state(s, :scope), s2)}
            end

          {tName, _} when is_list(tName) ->
            qN = get_QName(tName, r_xmlElement(el, :namespace), reset_scope(s2))

            case is_simple_type(qN, s2) do
              true ->
                tRef = {:simpleType, qN}
                {[tRef], add_ref(set_scope(r_xsd_state(s, :scope), s2), tRef)}

              _ ->
                tRef = {:simple_or_complex_Type, qN}
                {[tRef], add_ref(set_scope(r_xsd_state(s, :scope), s2), tRef)}
            end

          _ ->
            case {get_attribute_value(:ref, el, :no_name), is_global_env(env)} do
              {ref, false} when is_list(ref) ->
                {[{:element, get_QName(ref, r_xmlElement(el, :namespace), reset_scope(s))}],
                 set_scope(r_xsd_state(s, :scope), s2)}

              _ ->
                {[urType()], set_scope(r_xsd_state(s, :scope), s2)}
            end
        end

      _Type ->
        {cM, set_scope(r_xsd_state(s, :scope), s2)}
    end
  end

  defp list_type(l, s, env) do
    case keyNsearch(:itemType, r_xmlAttribute(:name), r_xmlElement(l, :attributes), []) do
      [] ->
        type(r_xmlElement(l, :content), s, env)

      r_xmlAttribute(value: v) ->
        typeRef = {:simpleType, get_QName(v, r_xmlElement(l, :namespace), reset_scope(s))}
        {[typeRef], add_ref(s, typeRef)}
    end
  end

  defp union_types(u, s, env) do
    {memberTypes, s2} =
      case keyNsearch(:memberTypes, r_xmlAttribute(:name), r_xmlElement(u, :attributes), []) do
        [] ->
          {[], s}

        r_xmlAttribute(value: nameString) ->
          names = namestring2namelist(nameString)

          uTypeRefs =
            for x <- names do
              {:simpleType, get_QName(x, r_xmlElement(u, :namespace), reset_scope(s))}
            end

          {uTypeRefs,
           foldl(
             fn x, s_in ->
               add_ref(s_in, x)
             end,
             s,
             uTypeRefs
           )}
      end

    {definedTypes, s3} = union_types1(r_xmlElement(u, :content), s2, env)
    {memberTypes ++ definedTypes, s3}
  end

  defp union_types1(c, s, env) do
    union_types1(c, s, env, [])
  end

  defp union_types1([], s, _Env, acc) do
    {acc, s}
  end

  defp union_types1([c = r_xmlElement() | cs], s, env, acc) do
    case element_content(kind(c, s), c, env) do
      {sT = {:simpleType, _}, s2} ->
        union_types1(cs, s2, env, [sT | acc])

      {{:annotation, _}, s2} ->
        union_types1(cs, s2, env, acc)

      {illegalType, s2} ->
        err =
          {error_path(c, r_xmlElement(c, :name)), :xmerl_xsd,
           {:union_member_type_not_simpleType, illegalType}}

        union_types1(cs, acc_errs(s2, err), env, acc)
    end
  end

  defp union_types1([_H | t], s, env, acc) do
    union_types1(t, s, env, acc)
  end

  defp recursive_redefine(name, cM, s = r_xsd_state(redefine: true)) do
    case remove_annotation(cM) do
      [{mG, {c, occ}}] ->
        [{mG, {recursive_redefine2(name, c, s), occ}}]

      _ ->
        cM
    end
  end

  defp recursive_redefine(_, cM, _) do
    cM
  end

  defp recursive_redefine2(name, [{:group, {name, occ}} | t], s) do
    case rename_redef_group(name, s) do
      :failed ->
        [{:group, {name, occ}} | t]

      newName ->
        [{:group, {newName, occ}} | t]
    end
  end

  defp recursive_redefine2(name, [{mG, {c, occ}} | t], s)
       when mG === :sequence or mG === :choice or
              mG === :all or mG === :group do
    c2 = recursive_redefine2(name, c, s)
    [{mG, {c2, occ}} | recursive_redefine2(name, t, s)]
  end

  defp recursive_redefine2(name, [h | t], s) do
    [h | recursive_redefine2(name, t, s)]
  end

  defp recursive_redefine2(_, [], _) do
    []
  end

  defp rename_redef_group(name = {lN, scope, nS}, s) do
    newName = {lN, [:"#redefine" | scope], nS}

    case resolve({:group, newName}, s) do
      {sG = r_schema_group(name: ^name), _} ->
        _ = save_object({:group, r_schema_group(sG, name: newName)}, s)
        newName

      _ ->
        :failed
    end
  end

  defp add_ref(
         s = r_xsd_state(unchecked_references: uR),
         sTRef = {:simpleType, ref}
       ) do
    case {is_builtin_simple_type(ref), ref} do
      {true, _} ->
        s

      {_, {:"", _, _}} ->
        s

      _ ->
        s2 = r_xsd_state(s, unchecked_references: add_once(sTRef, uR))
        add_circularity_ref(sTRef, s2)
    end
  end

  defp add_ref(
         s = r_xsd_state(unchecked_references: uR),
         sTRef = {:simple_or_complex_Type, ref}
       ) do
    case {is_builtin_simple_type(ref), ref} do
      {true, _} ->
        s

      {_, {:"", _, _}} ->
        s

      {_, {:anyType, _, :"http://www.w3.org/2001/XMLSchema"}} ->
        s

      {_, {:anySimpleType, _, :"http://www.w3.org/2001/XMLSchema"}} ->
        s

      _ ->
        s2 = r_xsd_state(s, unchecked_references: add_once(sTRef, uR))
        add_circularity_ref(sTRef, s2)
    end
  end

  defp add_ref(s, {:complexType, {:anyType, _, :"http://www.w3.org/2001/XMLSchema"}}) do
    s
  end

  defp add_ref(s = r_xsd_state(unchecked_references: uR), ref) do
    s2 = r_xsd_state(s, unchecked_references: add_once(ref, uR))
    add_circularity_ref(ref, s2)
  end

  defp add_circularity_ref(
         ref = {kind, to},
         s = r_xsd_state(circularity_disallowed: cD, redefine: false)
       )
       when kind == :simpleType or
              kind == :simple_or_complex_Type or
              kind == :complexType do
    case get_circularity_mark(ref, s) do
      [] ->
        s

      from ->
        r_xsd_state(s,
          circularity_disallowed:
            add_once(
              {from, {:typeDef, to}},
              cD
            )
        )
    end
  end

  defp add_circularity_ref(_, s) do
    s
  end

  defp get_circularity_mark({tD, _}, s)
       when tD == :simpleType or
              tD == :complexType or
              tD == :simple_or_complex_Type do
    case r_xsd_state(s, :circularity_stack) do
      [from = {:typeDef, _} | _] ->
        from

      _ ->
        []
    end
  end

  defp get_circularity_mark(_, _S) do
    []
  end

  defp push_circularity_mark(
         mark,
         s = r_xsd_state(circularity_stack: cS, redefine: false)
       ) do
    r_xsd_state(s, circularity_stack: [mark | cS])
  end

  defp push_circularity_mark(_, s) do
    s
  end

  defp pop_circularity_mark(mark, s = r_xsd_state(redefine: false)) do
    case r_xsd_state(s, :circularity_stack) do
      [^mark | rest] ->
        r_xsd_state(s, circularity_stack: rest)

      _ ->
        s
    end
  end

  defp pop_circularity_mark(_, s) do
    s
  end

  defp derived_type(
         {:complexType, r_schema_complex_type(name: name, content: c)},
         s = r_xsd_state(derived_types: dT)
       ) do
    case {keymember(:restriction, 1, c), keymember(:extension, 1, c)} do
      {false, false} ->
        s

      _ ->
        r_xsd_state(s, derived_types: [{:complexType, name} | dT])
    end
  end

  defp derived_type(
         {:simpleType, r_schema_simple_type(name: name, content: c)},
         s = r_xsd_state(derived_types: dT)
       ) do
    case keymember(:restriction, 1, c) do
      true ->
        r_xsd_state(s, derived_types: [{:simpleType, name} | dT])

      _ ->
        s
    end
  end

  defp facets([{:annotation, _} | rest], s) do
    facets(rest, s)
  end

  defp facets([{:restriction, {baseType, cM}}], _S) do
    facets =
      for x = {f, _} <- cM, is_facet(f) do
        x
      end

    groupFacets = group_facets(facets)
    {baseType, groupFacets}
  end

  defp facets(_, _S) do
    {:undefined, []}
  end

  defp group_facets(facets) do
    group_facets(facets, [])
  end

  defp group_facets(l = [{:enumeration, _} | _Rest], acc) do
    {enums, rest} =
      splitwith(
        fn
          {:enumeration, _} ->
            true

          _ ->
            false
        end,
        l
      )

    group_facets(
      rest,
      [
        {:enumeration,
         for {:enumeration, x} <- enums do
           x
         end}
        | acc
      ]
    )
  end

  defp group_facets([h | t], acc) do
    group_facets(t, [h | acc])
  end

  defp group_facets([], acc) do
    reverse(acc)
  end

  defp simpleType_final(sT, _S) do
    final = get_attribute_value(:final, sT, [])
    split_by_whitespace(final, [])
  end

  defp redefine([cM | rest], s) do
    s2 = redefine(cM, s)
    redefine(rest, s2)
  end

  defp redefine(sT = {type, _Name}, s)
       when type == :simpleType or type == :complexType do
    {originalType, s2} = resolve(sT, s)
    {redefinedType, s3} = load_redefine_object(sT, s2)
    {_MergedType, s4} = merge_derived_types(originalType, redefinedType, :redefine, s3)
    s4
  end

  defp redefine(_, s) do
    s
  end

  defp keyrefer(:keyref, el, s) do
    ref = get_attribute_value(:refer, el, :undefined)
    get_QName(ref, r_xmlElement(el, :namespace), reset_scope(s))
  end

  defp keyrefer(_, _, _) do
    :undefined
  end

  defp remove_annotation(cM) when is_list(cM) do
    for x = {k, _} <- cM, k !== :annotation do
      x
    end
  end

  defp remove_attributes(cM) when is_list(cM) do
    for x = {k, _} <- cM, k !== :attribute, k !== :anyAttribute, k !== :attributeGroup do
      x
    end
  end

  defp keep_attributes(cM) when is_list(cM) do
    for x = {k, _} <- cM,
        k == :attribute or k == :anyAttribute or k == :attributeGroup do
      x
    end
  end

  defp split_content([{:restriction, {baseT, cM}}]) do
    {[{:restriction, {baseT, remove_attributes(cM)}}], keep_attributes(cM)}
  end

  defp split_content([{:extension, {baseT, cM}}]) do
    {[{:extension, {baseT, remove_attributes(remove_annotation(cM))}}], keep_attributes(cM)}
  end

  defp split_content(cM) do
    {remove_attributes(cM), keep_attributes(cM)}
  end

  defp restriction_base_type(r, cM, s) do
    case base_type(r) do
      [] ->
        case (for x = {:simpleType, _} <- cM do
                x
              end) do
          [{:simpleType, typeName}] ->
            {typeName, keydelete(:simpleType, 1, cM), s}

          other ->
            err =
              {error_path(r, r_xmlElement(r, :name)), :xmerl_xsd,
               {:missing_base_type, :restriction, other}}

            {{[], [], []}, cM, acc_errs(s, err)}
        end

      bT ->
        {get_QName(bT, r_xmlElement(r, :namespace), reset_scope(s)), cM, s}
    end
  end

  defp base_type([{:restriction, {baseT, _}}], sCT) do
    r_schema_complex_type(sCT, base_type: baseT)
  end

  defp base_type([{:extension, {baseT, _}}], sCT) do
    r_schema_complex_type(sCT, base_type: baseT)
  end

  defp base_type(_, sCT) do
    sCT
  end

  defp variety([{:list, _ItemType}]) do
    :list
  end

  defp variety([{:union, _ItemType}]) do
    :union
  end

  defp variety(_) do
    :atomic
  end

  defp pre_check_cm(kind, cs = [c = r_xmlElement() | restC], name, s) do
    case kind(c, s) do
      {:annotation, _} ->
        pre_check_cm2(kind, restC, name, c, s, 0)

      {_, s2} ->
        pre_check_cm2(kind, cs, name, c, s2, 0)
    end
  end

  defp pre_check_cm(kind, [_C | cs], name, s) do
    pre_check_cm(kind, cs, name, s)
  end

  defp pre_check_cm(kind, [], name, s) do
    err = {[], :xmerl_xsd, {:content_failure, kind, [], name}}
    acc_errs(s, err)
  end

  defp pre_check_cm2(kind, [c = r_xmlElement() | cs], name, _El, s, n) do
    s2 =
      case kind(c, s) do
        {:restriction, _} ->
          s

        {:extension, _} ->
          s

        {other, s1} ->
          err =
            {error_path(c, r_xmlElement(c, :name)), :xmerl_xsd,
             {:illegal_element, kind, other, name}}

          acc_errs(s1, err)
      end

    pre_check_cm2(kind, cs, name, c, s2, n + 1)
  end

  defp pre_check_cm2(kind, [_H | t], name, el, s, n) do
    pre_check_cm2(kind, t, name, el, s, n)
  end

  defp pre_check_cm2(_, [], _, _, s, n) when n == 1 do
    s
  end

  defp pre_check_cm2(kind, [], name, el, s, n) do
    err =
      case n do
        0 ->
          {error_path(el, r_xmlElement(el, :name)), :xmerl_xsd,
           {:content_failure_expected_restriction_or_extension, kind, name}}

        _ ->
          {error_path(el, r_xmlElement(el, :name)), :xmerl_xsd,
           {:content_failure_only_one_restriction_or_extension_allowed, kind, name}}
      end

    acc_errs(s, err)
  end

  defp check_cm(kind, s4SCM, contentModel, s) do
    case check_cm2(kind, s4SCM, contentModel, s) do
      {[], _S} ->
        s

      {[_, [] | _], _S} ->
        s

      {_CM, s2} ->
        s2

      err ->
        exit({:error, {[], :xmerl_xsd, {:internal_error, err}}})
    end
  end

  defp check_cm2(kind, r_chain(content: s4SCM, occurance: occ), contentModel, s) do
    case occurance_loop(occ, &check_chain/1, [s4SCM, contentModel, kind, s], 0) do
      {:ok, []} ->
        {[], s}

      {:ok, [s4SCMRest, cMRest | _]} ->
        case all_optional(s4SCMRest) do
          true ->
            {cMRest, s}

          _ ->
            err = {[], :xmerl_xsd, {:mandatory_component_missing, s4SCMRest, kind}}
            acc_errs(s, err)
        end

      {:error, {_, _, reason}} ->
        err = {[], :xmerl_xsd, {:illegal_content, reason, kind}}
        {contentModel, acc_errs(s, err)}
    end
  end

  defp check_cm2(kind, r_alternative(content: s4SCM, occurance: occ), contentModel, s) do
    case occurance_loop(occ, &check_alternative/1, [s4SCM, contentModel, kind, s], 0) do
      {:ok, []} ->
        {[], s}

      {:ok, [_, cMRest | _]} ->
        {cMRest, s}

      {:error, reason} ->
        {contentModel, acc_errs(s, reason)}
    end
  end

  defp check_cm2(_, {kind, occ}, cM, s) do
    case occurance_loop(occ, &check_simple_cm/1, [kind, cM], 0) do
      {:ok, []} ->
        {[], s}

      {:ok, [_, cMRest | _]} ->
        {cMRest, s}

      {:error, reason} ->
        {cM, acc_errs(s, reason)}

      err ->
        {cM, acc_errs(s, err)}
    end
  end

  defp check_simple_cm([kind, cM]) do
    check_simple_cm(kind, cM)
  end

  defp check_simple_cm(kind, []) do
    {:error, {[], :xmerl_xsd, {:no_match, {kind, []}}}}
  end

  defp check_simple_cm(kind, [{kind, _} | rest]) do
    {:ok, [kind, rest]}
  end

  defp check_simple_cm(kind, [{other, _} | rest])
       when kind == :simpleType or kind == :complexType do
    case other do
      :simple_or_complex_Type ->
        {:ok, [kind, rest]}

      _ ->
        {:error, {[], :xmerl_xsd, {:no_match, other}}}
    end
  end

  defp check_simple_cm(_Kind, [{other, _} | _]) do
    {:error, {[], :xmerl_xsd, {:no_match, other}}}
  end

  defp check_chain([s4SCM, contentModel, kind, s]) do
    check_chain(kind, s4SCM, contentModel, s)
  end

  defp check_chain(kind, [s4SC | s4SCs], chainCM = [_H | _T], s = r_xsd_state(errors: errs)) do
    newKind =
      case s4SC do
        {nK, _} ->
          nK

        _ ->
          kind
      end

    case check_cm2(newKind, s4SC, chainCM, s) do
      {chainCMRest, r_xsd_state(errors: ^errs)} ->
        check_chain(kind, s4SCs, chainCMRest, s)

      {_ChainCMRest, _S2} ->
        case optional(s4SC) do
          true ->
            check_chain(kind, s4SCs, chainCM, s)

          _ ->
            {:error, {[], :xmerl_xsd, {:unmatched_mandatory_object, kind, s4SC}}}
        end
    end
  end

  defp check_chain(kind, [], cM, s) do
    {:ok, [[], cM, kind, s]}
  end

  defp check_chain(kind, rest, cM, s) do
    case all_optional(rest) do
      true ->
        {:ok, [rest, cM, kind, s]}

      _ ->
        {:error, {[], :xmerl_xsd, {:bad_match, rest, cM}}}
    end
  end

  defp check_alternative([s4SC, cM, kind, s]) do
    check_alternative(kind, s4SC, cM, s)
  end

  defp check_alternative(kind, [s4SC | s4SCs], altCM = [_H | _T], s = r_xsd_state(errors: err)) do
    newKind =
      case s4SC do
        {nK, _} ->
          nK

        _ ->
          kind
      end

    case check_cm2(newKind, s4SC, altCM, s) do
      {altCMRest, r_xsd_state(errors: ^err)} ->
        {:ok, [[s4SC], altCMRest, kind, s]}

      {altCMRest, _S2} ->
        check_alternative(kind, s4SCs, altCMRest, s)
    end
  end

  defp check_alternative(kind, [], _AltCM, _S) do
    {:error, {[], :xmerl_xsd, {:no_match, kind}}}
  end

  defp occurance_loop({min, max}, _CheckFun, [_, [] | _Rest], n)
       when min <= n and max >= n do
    {:ok, []}
  end

  defp occurance_loop(occ = {min, max}, checkFun, args, n) do
    nplus1 = n + 1

    case checkFun.(args) do
      {:error, {_, _, {:no_match, _}}}
      when min <= n and
             max >= n ->
        {:ok, args}

      err = {:error, _} ->
        err

      {:ok, ^args} ->
        {:error, {[], :xmerl_xsd, {:no_match, occurance_kind(args)}}}

      {:ok, newArgs} when nplus1 < max ->
        occurance_loop(occ, checkFun, newArgs, nplus1)

      ret = {:ok, _NewArgs} ->
        ret
    end
  end

  defp occurance_kind([kind, _]) do
    kind
  end

  defp occurance_kind([_, _, kind, _]) do
    kind
  end

  defp occurance_kind(_) do
    []
  end

  defp count_occur({min, max}) do
    {decrease(min), max}
  end

  defp count_occur(other) do
    other
  end

  defp decrease(i) when is_integer(i) and i > 0 do
    i - 1
  end

  defp decrease(i) do
    i
  end

  defp decrease_occurance({k, {iD, occ}}) do
    {k, {iD, count_occur(occ)}}
  end

  defp decrease_occurance(other) do
    other
  end

  defp get_occur({_, {_, occ = {min, _}}})
       when is_integer(min) do
    occ
  end

  defp get_occur({_, {_, occ = {min, _}, _}})
       when is_integer(min) do
    occ
  end

  defp get_occur(other) do
    other
  end

  defp optional(:optional_text) do
    true
  end

  defp optional({_, {0, _}}) do
    true
  end

  defp optional({_, {_, {0, _}}}) do
    true
  end

  defp optional({:any, {_, {0, _}, _}}) do
    true
  end

  defp optional(r_chain(occurance: {0, _})) do
    true
  end

  defp optional(r_alternative(occurance: {0, _})) do
    true
  end

  defp optional(r_chain(content: content)) do
    try do
      is_optional_content(content)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp optional(r_alternative(content: content)) do
    try do
      is_optional_content(content)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp optional({:all, {content, _}}) do
    try do
      is_optional_content(content)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp optional(_) do
    false
  end

  defp is_optional_content([h | t]) do
    case optional(h) do
      true ->
        is_optional_content(t)

      false ->
        throw(false)
    end
  end

  defp is_optional_content([]) do
    true
  end

  defp not_optional(x) do
    case optional(x) do
      true ->
        false

      _ ->
        true
    end
  end

  defp all_optional([]) do
    true
  end

  defp all_optional(l) do
    case filter(&not_optional/1, l) do
      [] ->
        true

      _ ->
        false
    end
  end

  defp allowed_content(:element, _Parents) do
    r_chain(
      content: [
        {:annotation, {0, 1}},
        r_chain(
          content: [
            r_alternative(
              content: [{:simpleType, {1, 1}}, {:complexType, {1, 1}}],
              occurance: {0, 1}
            ),
            r_alternative(
              content: [{:unique, {1, 1}}, {:key, {1, 1}}, {:keyref, {1, 1}}],
              occurance: {0, :unbounded}
            )
          ]
        )
      ]
    )
  end

  defp allowed_content(:attribute, _Parents) do
    r_chain(content: [{:annotation, {0, 1}}, {:simpleType, {0, 1}}])
  end

  defp allowed_content(:complexType, parents) do
    r_chain(
      content: [
        {:annotation, {0, 1}},
        r_alternative(
          content: [
            set_occurance(
              allowed_content(
                :simpleContent,
                parents
              ),
              {1, 1}
            ),
            set_occurance(
              allowed_content(
                :complexContent,
                parents
              ),
              {1, 1}
            ),
            r_chain(
              content: [
                r_alternative(
                  content: [
                    {:group, {1, 1}},
                    {:all, {1, 1}},
                    {:choice, {1, 1}},
                    {:sequence, {1, 1}}
                  ],
                  occurance: {0, 1}
                ),
                r_chain(
                  content: [
                    r_alternative(
                      content: [{:attribute, {1, 1}}, {:attributeGroup, {1, 1}}],
                      occurance: {0, :unbounded}
                    ),
                    {:anyAttribute, {0, 1}}
                  ]
                )
              ]
            )
          ]
        )
      ]
    )
  end

  defp allowed_content(:attributeGroup, parents) do
    case member(:simpleContent, parents) do
      true ->
        {:annotation, {0, 1}}

      _ ->
        r_chain(
          content: [
            {:annotation, {0, 1}},
            r_chain(
              content: [
                r_alternative(
                  content: [{:attribute, {1, 1}}, {:attributeGroup, {1, 1}}],
                  occurance: {0, :unbounded}
                ),
                {:anyAttribute, {0, 1}}
              ]
            )
          ]
        )
    end
  end

  defp allowed_content(:group, _Parents) do
    r_chain(
      content: [
        {:annotation, {0, 1}},
        r_alternative(
          content: [{:all, {1, 1}}, {:choice, {1, 1}}, {:sequence, {1, 1}}],
          occurance: {0, 1}
        )
      ]
    )
  end

  defp allowed_content(:all, _Parents) do
    r_chain(content: [{:annotation, {0, 1}}, {:element, {0, :unbounded}}])
  end

  defp allowed_content(sorC, _Parents)
       when sorC == :sequence or
              sorC == :choice do
    r_chain(
      content: [
        {:annotation, {0, 1}},
        r_alternative(
          content: [
            {:element, {1, 1}},
            {:group, {1, 1}},
            {:choice, {1, 1}},
            {:sequence, {1, 1}},
            {:any, {1, 1}}
          ],
          occurance: {0, :unbounded}
        )
      ]
    )
  end

  defp allowed_content(:simpleType, _Parents) do
    r_chain(
      content: [
        {:annotation, {0, 1}},
        r_alternative(content: [{:restriction, {1, 1}}, {:list, {1, 1}}, {:union, {1, 1}}])
      ]
    )
  end

  defp allowed_content(:restriction, parents) do
    case member(:simpleType, parents) do
      true ->
        allowed_content2(:restriction, :simpleType)

      _ ->
        case member(:simpleContent, parents) do
          true ->
            allowed_content2(:restriction, :simpleContent)

          _ ->
            allowed_content2(:restriction, :complexContent)
        end
    end
  end

  defp allowed_content(lU, _Parent) when lU == :list or lU == :union do
    r_chain(content: [{:annotation, {0, 1}}, {:simpleType, {0, 1}}])
  end

  defp allowed_content(:redefine, _Parents) do
    r_alternative(
      content: [
        {:annotation, {1, 1}},
        r_alternative(
          content: [
            {:simpleType, {1, 1}},
            {:complexType, {1, 1}},
            {:group, {1, 1}},
            {:attributeGroup, {1, 1}}
          ]
        )
      ],
      occurance: {0, :unbounded}
    )
  end

  defp allowed_content(e, _Parents)
       when e == :simpleContent or
              e == :complexContent do
    r_chain(
      content: [
        {:annotation, {0, 1}},
        r_alternative(content: [{:restriction, {1, 1}}, {:extension, {1, 1}}])
      ]
    )
  end

  defp allowed_content(:extension, parents) do
    case member(:simpleContent, parents) do
      true ->
        allowed_content2(:extension, :simpleContent)

      _ ->
        allowed_content2(:extension, :complexContent)
    end
  end

  defp allowed_content2(:restriction, :simpleType) do
    r_chain(
      content: [
        {:annotation, {0, 1}},
        r_chain(
          content: [
            {:simpleType, {0, 1}},
            r_alternative(
              content: [
                {:minExclusive, {1, 1}},
                {:minInclusive, {1, 1}},
                {:maxExclusive, {1, 1}},
                {:maxInclusive, {1, 1}},
                {:totalDigits, {1, 1}},
                {:fractionDigits, {1, 1}},
                {:length, {1, 1}},
                {:minLength, {1, 1}},
                {:maxLength, {1, 1}},
                {:enumeration, {1, 1}},
                {:whiteSpace, {1, 1}},
                {:pattern, {1, 1}}
              ],
              occurance: {0, :unbounded}
            )
          ]
        )
      ]
    )
  end

  defp allowed_content2(:restriction, :simpleContent) do
    r_chain(
      content: [
        {:annotation, {0, 1}},
        r_chain(
          content: [
            {:simpleType, {0, 1}},
            r_alternative(
              content: [
                {:minExclusive, {1, 1}},
                {:minInclusive, {1, 1}},
                {:maxExclusive, {1, 1}},
                {:maxInclusive, {1, 1}},
                {:totalDigits, {1, 1}},
                {:fractionDigits, {1, 1}},
                {:length, {1, 1}},
                {:minLength, {1, 1}},
                {:maxLength, {1, 1}},
                {:enumeration, {1, 1}},
                {:whiteSpace, {1, 1}},
                {:pattern, {1, 1}}
              ],
              occurance: {0, :unbounded}
            )
          ],
          occurance: {0, 1}
        ),
        r_chain(
          content: [
            r_alternative(
              content: [{:attribute, {1, 1}}, {:attributeGroup, {1, 1}}],
              occurance: {0, :unbounded}
            ),
            {:anyAttribute, {0, 1}}
          ]
        )
      ]
    )
  end

  defp allowed_content2(:restriction, :complexContent) do
    r_chain(
      content: [
        {:annotation, {0, 1}},
        r_alternative(
          content: [{:group, {1, 1}}, {:all, {1, 1}}, {:choice, {1, 1}}, {:sequence, {1, 1}}],
          occurance: {0, 1}
        ),
        r_chain(
          content: [
            r_alternative(
              content: [{:attribute, {1, 1}}, {:attributeGroup, {1, 1}}],
              occurance: {0, :unbounded}
            ),
            {:anyAttribute, {0, 1}}
          ]
        )
      ]
    )
  end

  defp allowed_content2(:extension, :simpleContent) do
    r_chain(
      content: [
        {:annotation, {0, 1}},
        r_chain(
          content: [
            r_alternative(
              content: [{:attribute, {1, 1}}, {:attributeGroup, {1, 1}}],
              occurance: {0, :unbounded}
            ),
            {:anyAttribute, {0, 1}}
          ]
        )
      ]
    )
  end

  defp allowed_content2(:extension, :complexContent) do
    r_chain(
      content: [
        {:annotation, {0, 1}},
        r_chain(
          content: [
            r_alternative(
              content: [{:group, {1, 1}}, {:all, {1, 1}}, {:choice, {1, 1}}, {:sequence, {1, 1}}],
              occurance: {0, 1}
            ),
            r_chain(
              content: [
                r_alternative(
                  content: [{:attribute, {1, 1}}, {:attributeGroup, {1, 1}}],
                  occurance: {0, 1}
                ),
                {:anyAttribute, {0, 1}}
              ]
            )
          ]
        )
      ]
    )
  end

  defp set_occurance(ch = r_chain(), occ) do
    r_chain(ch, occurance: occ)
  end

  defp set_occurance(alt = r_alternative(), occ) do
    r_alternative(alt, occurance: occ)
  end

  defp set_occurance({name, _}, occ) when is_atom(name) do
    {name, occ}
  end

  defp process_external_schema_once(e, namespace, s)
       when elem(e, 0) === :xmlElement do
    case get_attribute_value(:schemaLocation, e, []) do
      [] ->
        err = {:missing_schemalocation_attribute, r_xmlElement(e, :name)}
        acc_errs(s, err)

      path ->
        process_external_schema_once(path, namespace, s)
    end
  end

  defp process_external_schema_once(schemaLocation, namespace, s) do
    case fetch_external_schema(schemaLocation, s) do
      {e = r_xmlElement(), s2} ->
        case is_already_processed(namespace, s2) do
          true ->
            save_namespace_definition(namespace, s2)

          _ ->
            s3 = save_namespace_definition(namespace, s2)

            traverse_ext_schema(
              e,
              r_xsd_state(s3, targetNamespace: namespace)
            )
        end

      {_, s2} ->
        s2
    end
  end

  defp process_external_schema(path, s) when is_list(path) do
    case fetch_external_schema(path, s) do
      {e = r_xmlElement(), s2} ->
        traverse_ext_schema(e, s2)

      {_, s2} ->
        s2
    end
  end

  defp process_external_schema(:absent, s) do
    s
  end

  defp fetch_external_schema(path, s) when is_list(path) do
    fetchFun = r_xsd_state(s, :fetch_fun)

    case fetchFun.(path, s) do
      {:ok, {:file, file}, _} ->
        :no_debug

        case :xmerl_scan.file(file, r_xsd_state(s, :xml_options)) do
          {:error, reason} ->
            {:error,
             acc_errs(
               s,
               {[], :xmerl_xsd, {:parsing_external_schema_failed, file, reason}}
             )}

          {eXSD, _} ->
            {eXSD, r_xsd_state(s, schema_name: file)}
        end

      {_, {:string, string}, _} ->
        :no_debug

        case :xmerl_scan.string(string, r_xsd_state(s, :xml_options)) do
          {:error, reason} ->
            {:error,
             acc_errs(
               s,
               {[], :xmerl_xsd, {:parsing_external_schema_failed, path, reason}}
             )}

          {eXSD, _} ->
            {eXSD, r_xsd_state(s, schema_name: path)}
        end

      {:ok, [], _} ->
        {:ok, s}

      {_, other, _} ->
        {:error,
         acc_errs(
           s,
           {[], :xmerl_xsd, {:fetch_fun_failed, other}}
         )}
    end
  end

  defp fetch_external_schema(:absent, s) do
    {:ok, s}
  end

  defp is_already_processed(
         nameSpace,
         r_xsd_state(
           schema_name: schemaName,
           checked_namespace_nodes: cNS
         )
       ) do
    case keysearch(schemaName, 2, cNS) do
      {_, {_, _, ^nameSpace}} ->
        true

      _ ->
        false
    end
  end

  defp save_namespace_definition(
         nameSpace,
         s =
           r_xsd_state(
             targetNamespace: tNS,
             global_namespace_nodes: gNS,
             checked_namespace_nodes: cNS
           )
       ) do
    {prefix, s2} =
      case keysearch(tNS, 1, gNS) do
        {:value, {_, importedNodes}} ->
          case keysearch(nameSpace, 2, importedNodes) do
            {:value, {_P, _}} ->
              {_P, s}

            _ ->
              {:none, s}
          end

        _ ->
          err = {[], :xmerl_xsd, {:imported_namespace_wo_namespace_definition, nameSpace}}
          {:none, acc_errs(s, err)}
      end

    case prefix do
      :none ->
        s2

      _ ->
        r_xsd_state(s,
          checked_namespace_nodes:
            add_once(
              {prefix, r_xsd_state(s, :schema_name), nameSpace},
              cNS
            )
        )
    end
  end

  defp prefix_namespace_2global(
         namespace,
         r_xmlNamespace(nodes: nodes),
         s =
           r_xsd_state(
             targetNamespace: tNS,
             global_namespace_nodes: gNS
           )
       ) do
    case keysearch(namespace, 2, nodes) do
      {:value, {prefix, _}} ->
        case keysearch(tNS, 1, gNS) do
          {:value, {_, definedNamespaces}} ->
            r_xsd_state(s,
              global_namespace_nodes:
                keyreplace(
                  tNS,
                  1,
                  gNS,
                  {tNS,
                   add_once(
                     {prefix, namespace},
                     definedNamespaces
                   )}
                )
            )

          _ ->
            r_xsd_state(s,
              global_namespace_nodes: [
                {tNS,
                 [
                   {prefix, namespace}
                   | default_namespace_by_convention()
                 ]}
              ]
            )
        end

      _ ->
        s
    end
  end

  defp prefix_namespace_2global(_, _, s) do
    s
  end

  defp traverse_ext_schema(e, s) do
    targetNS = target_namespace(e)

    case {targetNS, r_xsd_state(s, :targetNamespace)} do
      {:undefined, _} ->
        traverse_ext_schema2(e, s)

      {tNS, tNS} ->
        traverse_ext_schema2(e, s)

      _ ->
        err =
          {error_path(e, :schema), :xmerl_xsd,
           {:illegal_target_namespace_external_schema, r_xmlElement(e, :name)}}

        acc_errs(s, err)
    end
  end

  defp traverse_ext_schema2(e, s) do
    s1 = namespace_nodes(e, s)
    s2 = element_form_default(e, s1)
    s3 = attribute_form_default(e, s2)
    s4 = substitution_default(:finalDefault, e, s3)
    s5 = substitution_default(:blockDefault, e, s4)
    {cM, s6} = traverse_content2(r_xmlElement(e, :content), s5, [])
    save_schema_element(cM, s6)
    s6
  end

  defp attribute_properties([r_xmlAttribute(name: :default, value: default) | rest], attr, s) do
    attribute_properties(rest, r_schema_attribute(attr, default: default), s)
  end

  defp attribute_properties([r_xmlAttribute(name: :fixed, value: fixed) | rest], attr, s) do
    attribute_properties(rest, r_schema_attribute(attr, fixed: fixed), s)
  end

  defp attribute_properties([r_xmlAttribute(name: :use, value: use) | rest], attr, s) do
    {use2, s2} = attribute_use(use, s)
    attribute_properties(rest, r_schema_attribute(attr, use: use2), s2)
  end

  defp attribute_properties([r_xmlAttribute(name: :form, value: form) | rest], attr, s) do
    {form2, s2} = attribute_form(form, s)
    attribute_properties(rest, r_schema_attribute(attr, form: form2), s2)
  end

  defp attribute_properties([r_xmlAttribute(name: :id, value: iD) | rest], attr, s) do
    s2 = check_and_save_ID(iD, s)
    attribute_properties(rest, r_schema_attribute(attr, id: iD), s2)
  end

  defp attribute_properties([_H | rest], attr, s) do
    attribute_properties(rest, attr, s)
  end

  defp attribute_properties([], attr, s) do
    {attr, s}
  end

  defp attribute_use(use, s) when use == 'optional' or use == 'prohibited' or use == 'required' do
    {:erlang.list_to_atom(use), s}
  end

  defp attribute_use(use, s) do
    {use,
     acc_errs(
       s,
       {[], :xmerl_xsd, {:illegal_use_value, use}}
     )}
  end

  defp attribute_form(form, s) when form == 'qualified' or form == 'unqualified' do
    {:erlang.list_to_atom(form), s}
  end

  defp attribute_form(form, s) do
    {form,
     acc_errs(
       s,
       {[], :xmerl_xsd, {:illegal_form_value, form}}
     )}
  end

  defp element_properties([r_xmlAttribute(name: :default, value: default) | rest], sE, el, s) do
    case r_schema_element(sE, :value_constraint) do
      {:fixed, _} ->
        err =
          {error_path(el, :schema), :xmerl_xsd,
           {'only one of final/default attributes allowed', r_xmlElement(el, :name)}}

        element_properties(rest, sE, el, acc_errs(s, err))

      _ ->
        element_properties(
          rest,
          r_schema_element(sE, value_constraint: {:default, default}),
          el,
          s
        )
    end
  end

  defp element_properties([r_xmlAttribute(name: :fixed, value: fixed) | rest], sE, el, s) do
    case r_schema_element(sE, :value_constraint) do
      {:default, _} ->
        err =
          {error_path(el, :schema), :xmerl_xsd,
           {'only one of final/default attributes allowed', r_xmlElement(el, :name)}}

        element_properties(rest, sE, el, acc_errs(s, err))

      _ ->
        element_properties(rest, r_schema_element(sE, value_constraint: {:fixed, fixed}), el, s)
    end
  end

  defp element_properties([r_xmlAttribute(name: :substitutionGroup, value: sG) | rest], sE, el, s) do
    sGName = get_QName(sG, r_xmlElement(el, :namespace), reset_scope(s))

    element_properties(
      rest,
      r_schema_element(sE, substitutionGroup: sGName),
      el,
      add_ref(s, {:element, sGName})
    )
  end

  defp element_properties([r_xmlAttribute(name: :form, value: f) | rest], sE, el, s) do
    {form, s2} = attribute_form(f, s)
    element_properties(rest, r_schema_element(sE, form: form), el, s2)
  end

  defp element_properties([r_xmlAttribute(name: :id, value: iD) | rest], sE, el, s) do
    s2 = check_and_save_ID(iD, s)
    element_properties(rest, r_schema_element(sE, id: iD), el, s2)
  end

  defp element_properties([r_xmlAttribute(name: :nillable, value: n) | rest], sE, el, s) do
    case boolean_to_atom(n) do
      :error ->
        element_properties(
          rest,
          sE,
          el,
          acc_errs(
            s,
            {error_path(el, :schema), :xmerl_xsd, {:illegal_nillable_value, n}}
          )
        )

      n_atom ->
        element_properties(rest, r_schema_element(sE, nillable: n_atom), el, s)
    end
  end

  defp element_properties([r_xmlAttribute(name: :abstract, value: a) | rest], sE, el, s) do
    case boolean_to_atom(a) do
      :error ->
        element_properties(
          rest,
          sE,
          el,
          acc_errs(
            s,
            {error_path(el, :schema), :xmerl_xsd, {:illegal_abstract_value, a}}
          )
        )

      a_atom ->
        element_properties(rest, r_schema_element(sE, abstract: a_atom), el, s)
    end
  end

  defp element_properties([r_xmlAttribute(name: :block, value: b) | rest], sE, el, s) do
    blockValues = split_by_whitespace(b, [])

    case legal_block_values(:element, blockValues) do
      {:error, reason} ->
        element_properties(
          rest,
          sE,
          el,
          acc_errs(
            s,
            {error_path(el, :schema), :xmerl_xsd, {:illegal_block_values, reason}}
          )
        )

      _ ->
        element_properties(rest, r_schema_element(sE, block: blockValues), el, s)
    end
  end

  defp element_properties([r_xmlAttribute(name: :final, value: f) | rest], sE, el, s) do
    finalValues = split_by_whitespace(f, [])

    case legal_final_values(:element, finalValues) do
      {:error, reason} ->
        element_properties(
          rest,
          sE,
          el,
          acc_errs(
            s,
            {error_path(el, :schema), :xmerl_xsd, {:illegal_final_values, reason}}
          )
        )

      _ ->
        element_properties(rest, r_schema_element(sE, final: finalValues), el, s)
    end
  end

  defp element_properties([_H | t], sE, el, s) do
    element_properties(t, sE, el, s)
  end

  defp element_properties([], sE, _El, s) do
    {sE, s}
  end

  defp element_forbidden_properties(el, s) do
    element_forbidden_properties(r_xmlElement(el, :attributes), el, s)
  end

  defp element_forbidden_properties([r_xmlAttribute(name: :nillable, value: v) | atts], el, s) do
    element_forbidden_properties(
      atts,
      el,
      acc_errs(
        s,
        {error_path(el, :schema), :xmerl_xsd, {:forbidden_property, :nillable, v}}
      )
    )
  end

  defp element_forbidden_properties([r_xmlAttribute(name: :default, value: v) | atts], el, s) do
    element_forbidden_properties(
      atts,
      el,
      acc_errs(
        s,
        {error_path(el, :schema), :xmerl_xsd, {:forbidden_property, :default, v}}
      )
    )
  end

  defp element_forbidden_properties([r_xmlAttribute(name: :fixed, value: v) | atts], el, s) do
    element_forbidden_properties(
      atts,
      el,
      acc_errs(
        s,
        {error_path(el, :schema), :xmerl_xsd, {:forbidden_property, :fixed, v}}
      )
    )
  end

  defp element_forbidden_properties([r_xmlAttribute(name: :form, value: v) | atts], el, s) do
    element_forbidden_properties(
      atts,
      el,
      acc_errs(
        s,
        {error_path(el, :schema), :xmerl_xsd, {:forbidden_property, :form, v}}
      )
    )
  end

  defp element_forbidden_properties([r_xmlAttribute(name: :block, value: v) | atts], el, s) do
    element_forbidden_properties(
      atts,
      el,
      acc_errs(
        s,
        {error_path(el, :schema), :xmerl_xsd, {:forbidden_property, :block, v}}
      )
    )
  end

  defp element_forbidden_properties([r_xmlAttribute(name: :type, value: v) | atts], el, s) do
    element_forbidden_properties(
      atts,
      el,
      acc_errs(
        s,
        {error_path(el, :schema), :xmerl_xsd, {:forbidden_property, :type, v}}
      )
    )
  end

  defp element_forbidden_properties([r_xmlAttribute() | atts], el, s) do
    element_forbidden_properties(atts, el, s)
  end

  defp element_forbidden_properties([], _, s) do
    s
  end

  defp element_forbidden_content([], s) do
    s
  end

  defp element_forbidden_content([el = r_xmlElement() | els], s) do
    case kind(el) do
      k
      when k == :complexType or k == :simpleType or
             k == :key or k == :keyref or k == :unique ->
        acc_errs(
          s,
          {error_path(el, :schema), :xmerl_xsd, {:element_content_must_not_contain, k, el}}
        )

      :annotation ->
        element_forbidden_content(els, s)

      other ->
        acc_errs(
          s,
          {error_path(el, :schema), :xmerl_xsd, {:illegal_element_content, other}}
        )
    end
  end

  defp element_forbidden_content([t = r_xmlText() | rest], s) do
    case is_whitespace(t) do
      true ->
        element_forbidden_content(rest, s)

      _ ->
        acc_errs(
          s,
          {error_path(t, :schema), :xmerl_xsd, {:illegal_element_content, t}}
        )
    end
  end

  defp c_t_properties(el, cT, s) do
    c_t_properties(r_xmlElement(el, :attributes), el, cT, s)
  end

  defp c_t_properties([r_xmlAttribute(name: :final, value: v) | rest], el, cT, s) do
    finalValues = split_by_whitespace(v, [])

    case legal_final_values(:complexType, finalValues) do
      {:error, reason} ->
        err = {error_path(el, :schema), :xmerl_xsd, {:illegal_final_values, reason}}
        c_t_properties(rest, el, cT, acc_errs(s, err))

      _ ->
        c_t_properties(rest, el, r_schema_complex_type(cT, final: finalValues), s)
    end
  end

  defp c_t_properties([r_xmlAttribute(name: :block, value: v) | rest], el, cT, s) do
    blockValues = split_by_whitespace(v, [])

    case legal_block_values(:complexType, blockValues) do
      {:error, reason} ->
        err = {error_path(el, :schema), :xmerl_xsd, {:illegal_block_values, reason}}
        c_t_properties(rest, el, cT, acc_errs(s, err))

      _ ->
        c_t_properties(rest, el, r_schema_complex_type(cT, block: blockValues), s)
    end
  end

  defp c_t_properties([r_xmlAttribute(name: :abstract, value: v) | rest], el, cT, s) do
    case boolean_to_atom(v) do
      :error ->
        err = {error_path(el, :schema), :xmerl_xsd, {:illegal_abstract_value, v}}
        c_t_properties(rest, el, cT, acc_errs(s, err))

      v_atom ->
        c_t_properties(rest, el, r_schema_complex_type(cT, abstract: v_atom), s)
    end
  end

  defp c_t_properties([_H | t], el, cT, s) do
    c_t_properties(t, el, cT, s)
  end

  defp c_t_properties([], _, cT, s) do
    {cT, s}
  end

  defp legal_block_values(_, [:"#all"]) do
    true
  end

  defp legal_block_values(:element, blockValues) do
    list_members(
      blockValues,
      [:extension, :restriction, :substitution]
    )
  end

  defp legal_block_values(:complexType, blockValues) do
    list_members(blockValues, [:extension, :restriction])
  end

  defp legal_final_values(_, [:"#all"]) do
    true
  end

  defp legal_final_values(_, finalValues) do
    list_members(finalValues, [:extension, :restriction])
  end

  defp boolean_to_atom(b) when b == '1' or b == 'true' do
    true
  end

  defp boolean_to_atom(b) when b == '0' or b == 'false' do
    false
  end

  defp boolean_to_atom(_) do
    :error
  end

  defp count_num_el(s = r_xsd_state(num_el: n)) do
    r_xsd_state(s, num_el: n + 1)
  end

  defp set_num_el(s = r_xsd_state(), i) when is_integer(i) do
    r_xsd_state(s, num_el: i)
  end

  defp set_num_el(s = r_xsd_state(), r_xsd_state(num_el: i)) do
    r_xsd_state(s, num_el: i)
  end

  defp occurance(el = r_xmlElement(attributes: atts), {min, max}, s) do
    attVal = fn
      r_xmlAttribute(value: v), sin ->
        case (try do
                mk_int_or_atom(v)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            err = {error_path(el, :schema), :xmerl_xsd, {:illegal_occurance_value, v}}
            {v, acc_errs(sin, err)}

          iAV ->
            {iAV, sin}
        end

      v1, sin ->
        {v1, sin}
    end

    {minVal, s2} =
      attVal.(
        keyNsearch(:minOccurs, r_xmlAttribute(:name), atts, min),
        s
      )

    {maxVal, s3} =
      attVal.(
        keyNsearch(:maxOccurs, r_xmlAttribute(:name), atts, max),
        s2
      )

    {{minVal, maxVal}, s3}
  end

  defp mk_int_or_atom(v = 'unbounded') do
    :erlang.list_to_atom(v)
  end

  defp mk_int_or_atom(v) when is_list(v) do
    :erlang.list_to_integer(v)
  end

  defp mk_int_or_atom(v) do
    v
  end

  defp mixed(e = r_xmlElement(content: c), s) do
    case {get_attribute_value(:mixed, e, :undefined),
          for y = r_xmlElement() <- c, kind(y) == :simpleContent do
            y
          end} do
      {_, [_SCE]} ->
        {false, s}

      {:undefined, _} ->
        case (for x = r_xmlElement() <- c, kind(x) == :complexContent do
                x
              end) do
          [e2] ->
            mixed(e2, s)

          _ ->
            {false, s}
        end

      {m, _} when m == '1' or m == 'true' ->
        {true, s}

      {m, _} when m == '0' or m == 'false' ->
        {false, s}

      {m, _} ->
        err = {error_path(e, :schema), :xmerl_xsd, {:invalid_mixed_value, m}}
        {false, acc_errs(s, err)}
    end
  end

  defp mixify(false, cM) do
    cM
  end

  defp mixify(true, cM) do
    mixify2(cM, [:optional_text])
  end

  defp mixify2([], acc) do
    reverse(acc)
  end

  defp mixify2([h | t], acc) do
    mixify2(t, [:optional_text, h | acc])
  end

  defp complexity([]) do
    :undefined
  end

  defp complexity([r_xmlText() | t]) do
    complexity(t)
  end

  defp complexity([r_xmlComment() | t]) do
    complexity(t)
  end

  defp complexity([h | t]) do
    case kind(h) do
      :simpleContent ->
        :simple

      :complexContent ->
        :complex

      _ ->
        complexity(t)
    end
  end

  defp validate_xml(
         el = r_xmlElement(name: name),
         s = r_xsd_state(table: tab, schemaLocations: schemaLocations)
       ) do
    elQName = {_, _, namespace} = mk_EII_QName(name, el, s)
    schemaCM = get_schema_cm(tab, namespace)

    case (for x = {:element, {qName, occ}} <- r_schema(schemaCM, :content),
              cmp_name(elQName, qName, s),
              at_least_one(occ) do
            x
          end) do
      [obj] ->
        {object, s2} = load_object(obj, s)
        validate_xml(el, object, s2)

      _ ->
        case is_already_processed(namespace, s) do
          true ->
            {:error,
             {error_path(el, name), :xmerl_xsd,
              {:element_not_in_schema, [name, elQName, schemaCM]}}}

          _ ->
            case keysearch(if_atom_to_list(namespace), 1, schemaLocations) do
              {:value, {_, location}} ->
                s1 = prefix_namespace_2global(namespace, r_xmlElement(el, :namespace), s)
                s2 = save_namespace_definition(namespace, s1)

                s3 =
                  process_external_schema(
                    location,
                    r_xsd_state(s2, targetNamespace: namespace)
                  )

                validate_xml(el, s3)

              _ ->
                {:error,
                 {error_path(el, name), :xmerl_xsd,
                  {:element_not_in_schema, [name, elQName, schemaCM]}}}
            end
        end
    end
  end

  defp validate_xml(xMLEl = r_xmlElement(), sEl = r_schema_element(), s) do
    case check_target_namespace(xMLEl, s) do
      :ok ->
        s2 = schemaLocations(xMLEl, s)
        :no_debug
        r_schema_element(name: _Name, type: _Type, block: bl) = sEl
        block = blocking(bl, r_xsd_state(s2, :blockDefault))
        ret = check_element_type([xMLEl], sEl, [], block, s2, [])

        case ret do
          {valXML, unvalRest, s3} ->
            {valXML, unvalRest, s3}

          _ ->
            ret
        end

      _ ->
        err =
          {error_path(xMLEl, r_xmlElement(xMLEl, :name)), :xmerl_xsd,
           {:target_namespace_missmatch}}

        {xMLEl, [], acc_errs(s, err)}
    end
  end

  defp check_element_type(
         xML = [xMLTxt = r_xmlText() | rest],
         cM = [cMEl | cMRest],
         env,
         block,
         s,
         checked
       ) do
    case is_whitespace(xMLTxt) do
      true ->
        check_element_type(rest, cM, env, block, s, [xMLTxt | checked])

      _ ->
        {resolvedT, s2} = resolve(cMEl, s)

        case check_text_type(xML, resolvedT, s2) do
          {:error, reason} ->
            case is_optional(cMEl, s) do
              true ->
                check_element_type(xML, cMRest, env, block, s, checked)

              _ ->
                check_element_type(rest, cM, env, block, acc_errs(s, reason), checked)
            end

          {ret, rest2, s3} ->
            check_element_type(rest2, cMRest, env, block, s3, reverse(ret, checked))
        end
    end
  end

  defp check_element_type(
         xML = [r_xmlElement() | _],
         [{:sequence, {cM, occ}} | _CMRest],
         env,
         _Block,
         s,
         checked
       ) do
    :no_debug
    check_sequence(xML, cM, occ, env, set_num_el(s, 0), checked)
  end

  defp check_element_type(
         xML = [r_xmlElement() | _],
         [{:choice, {cM, occ}} | _CMRest],
         env,
         _Block,
         s,
         checked
       ) do
    :no_debug
    check_choice(xML, cM, occ, env, set_num_el(s, 0), checked)
  end

  defp check_element_type(
         xML = [r_xmlElement() | _],
         [{:all, {cM, occ}} | _CMRest],
         env,
         _Block,
         s,
         checked
       ) do
    :no_debug
    check_all(xML, cM, occ, env, set_num_el(s, 0), checked, xML)
  end

  defp check_element_type(
         xML = [xMLEl = r_xmlElement() | _],
         [cMEl | cMRest],
         env,
         block,
         s,
         checked
       ) do
    {resolvedT, s2} = resolve(cMEl, s)

    case check_element_type(xML, resolvedT, env, block, s2, []) do
      {:error, reason} ->
        check_element_type(tl(xML), cMRest, env, block, acc_errs(s, reason), [xMLEl | checked])

      {[], _, _} ->
        check_element_type(xML, cMRest, env, block, s, checked)

      {xMLEl2, restXML, s3} ->
        check_element_type(
          restXML,
          [decrease_occurance(cMEl) | cMRest],
          env,
          block,
          s3,
          xMLEl2 ++ checked
        )
    end
  end

  defp check_element_type([], [], _Env, _Block, s, checked) do
    {checked, [], s}
  end

  defp check_element_type([], [cMEl | cMRest], env, block, s, checked) do
    case is_optional(cMEl, s) do
      true ->
        check_element_type([], cMRest, env, block, s, checked)

      _ ->
        err = {error_path(checked, :undefined), :xmerl_xsd, {:missing_mandatory_element, cMEl}}
        {checked, [], acc_errs(s, err)}
    end
  end

  defp check_element_type(
         _XML = [],
         r_schema_complex_type(name: _Name, base_type: bT, complexity: :simple, content: _C) = cT,
         _Env,
         _Block,
         s,
         checked
       ) do
    {resolvedType, _} =
      resolve(
        {:simple_or_complex_Type, bT},
        s
      )

    case resolvedType do
      r_schema_simple_type() ->
        {newVal, s2} = check_type(resolvedType, [], :unapplied, s)
        {newVal, [], s2}

      {:simpleType, _} ->
        {newVal, s2} = check_type(resolvedType, [], :unapplied, s)
        {newVal, [], s2}

      _ ->
        {:error, {error_path(checked, :undefined), :xmerl_xsd, {:empty_content_not_allowed, cT}}}
    end
  end

  defp check_element_type(
         [],
         r_schema_complex_type(name: _Name, block: _Bl, content: c),
         _Env,
         _Block,
         s,
         checked
       ) do
    case allow_empty_content(c) do
      true ->
        {[], [], s}

      false ->
        {:error, {error_path(checked, :undefined), :xmerl_xsd, {:empty_content_not_allowed, c}}}
    end
  end

  defp check_element_type(c, {:anyType, _}, _Env, _Block, s, _Checked) do
    {:lists.reverse(c), [], s}
  end

  defp check_element_type(
         xML = [r_xmlText() | _],
         type = r_schema_simple_type(),
         _Env,
         _Block,
         s,
         _Checked
       ) do
    check_text_type(xML, type, s)
  end

  defp check_element_type(
         xML = [r_xmlText() | _],
         type = {:simpleType, _NameNS},
         _Env,
         _Block,
         s,
         _Checked
       ) do
    check_text_type(xML, type, s)
  end

  defp check_element_type(
         xML = [r_xmlText() | _],
         r_schema_complex_type(name: _Name, base_type: bT, complexity: :simple, content: _C),
         env,
         block,
         s,
         checked
       ) do
    {resolvedType, _} =
      resolve(
        {:simple_or_complex_Type, bT},
        s
      )

    check_element_type(xML, resolvedType, env, block, s, checked)
  end

  defp check_element_type(
         xML = [_H | _],
         r_schema_complex_type(name: name, block: bl, content: c),
         env,
         _Block,
         s,
         checked
       ) do
    envName =
      case name do
        {lN, _Scope, _NS} ->
          lN

        _ ->
          :anonymous
      end

    block = blocking(bl, r_xsd_state(s, :blockDefault))
    check_element_type(xML, c, [envName | env], block, name_scope(name, s), checked)
  end

  defp check_element_type(
         xML = [xMLEl = r_xmlElement(name: name) | restXML],
         cMEl = r_schema_element(name: cMName, type: type),
         env,
         block,
         s,
         checked
       ) do
    elName = mk_EII_QName(name, xMLEl, r_xsd_state(s, scope: :erlang.element(2, cMName)))
    {min, max} = r_schema_element(cMEl, :occurance)

    case cmp_name(elName, cMName, s) do
      true when r_xsd_state(s, :num_el) <= max ->
        s1 = id_constraints(cMEl, xMLEl, s)

        {cMEl2, type2, s2} =
          cond do
            elName === cMName ->
              {cMEl, type, s1}

            true ->
              case resolve({:element, elName}, s1) do
                {sESub = r_schema_element(type: subType), ssub} ->
                  {sESub, subType, ssub}

                {_, ssub} ->
                  {cMEl, type, ssub}
              end
          end

        {resolvedType, s3} = resolve(type2, xMLEl, s2)
        xsiFactors = xsi_factors(cMEl2)
        {xMLEl2, s4} = check_attributes(xMLEl, resolvedType, xsiFactors, s3)
        s5 = check_abstract(elName, xMLEl, cMEl, s4)

        s6 =
          check_form(
            elName,
            name,
            xMLEl,
            actual_form_value(
              r_schema_element(cMEl, :form),
              r_xsd_state(s5, :elementFormDefault)
            ),
            s5
          )

        {content, _, s7} =
          case check_element_type(
                 r_xmlElement(xMLEl2, :content),
                 resolvedType,
                 env,
                 block,
                 s6,
                 checked
               ) do
            {:error, reason} ->
              {r_xmlElement(xMLEl2, :content), [], acc_errs(s6, reason)}

            result = {_, [], _} ->
              result

            {_, unexpectedRest, _} ->
              err = {error_path(xMLEl, name), :xmerl_xsd, {:unexpected_rest, unexpectedRest}}
              {r_xmlElement(xMLEl2, :content), [], acc_errs(s6, err)}
          end

        {[r_xmlElement(xMLEl2, content: reverse(content))], restXML,
         set_scope(r_xsd_state(s5, :scope), set_num_el(s7, s6))}

      true ->
        {:error,
         {error_path(xMLEl, name), :xmerl_xsd, {:element_not_suitable_with_schema, elName, s}}}

      _ when r_xsd_state(s, :num_el) >= min ->
        {[], xML, r_xsd_state(s, num_el: 0)}

      _ ->
        {:error,
         {error_path(xMLEl, name), :xmerl_xsd,
          {:element_not_suitable_with_schema, elName, cMName, cMEl, s}}}
    end
  end

  defp check_element_type(xML, r_schema_group(content: [cM]), env, block, s, checked) do
    check_element_type(xML, cM, env, block, s, checked)
  end

  defp check_element_type(xML, r_schema_group(content: []), _Env, _Block, _S, _Checked) do
    {:error, {error_path(xML, :undefined), :xmerl_xsd, {:no_element_expected_in_group, xML}}}
  end

  defp check_element_type(
         xML = [r_xmlElement(content: _Content) | _Rest],
         {:sequence, {els, occ}},
         env,
         _Block,
         s,
         checked
       ) do
    :no_debug

    case check_sequence(xML, els, occ, env, r_xsd_state(s, num_el: 0), checked) do
      err = {:error, _} ->
        err

      {validContent, rest2, s2} ->
        {validContent, rest2, s2}
    end
  end

  defp check_element_type(
         xML = [r_xmlElement() | _Rest],
         {:choice, {els, occ}},
         env,
         _Block,
         s,
         checked
       ) do
    :no_debug

    case check_choice(xML, els, occ, env, r_xsd_state(s, num_el: 0), checked) do
      err = {:error, _} ->
        err

      {validContent, rest2, s2} ->
        {validContent, rest2, s2}
    end
  end

  defp check_element_type(
         xML = [e = r_xmlElement(name: name) | rest],
         any = {:any, {namespace, _Occ = {min, _}, processorContents}},
         env,
         _Block,
         s,
         _Checked
       ) do
    :no_debug
    elName = mk_EII_QName(name, e, s)

    case cmp_any_namespace(elName, namespace, s) do
      true ->
        case processorContents do
          :skip ->
            {[e], rest, s}

          :lax ->
            {[e], rest, s}

          :strict ->
            case member(:absent, namespace) do
              true ->
                traverse = fn
                  r_xmlElement(nsinfo: [], attributes: atts, content: c), sin, fun ->
                    sin2 = fun.(atts, sin, fun)
                    fun.(c, sin2, fun)

                  r_xmlAttribute(namespace: []), sin, _Fun ->
                    sin

                  r_xmlText(), sin, _Fun ->
                    sin

                  [h | t], sin, fun ->
                    sin2 = fun.(h, sin, fun)
                    fun.(t, sin2, fun)

                  [], sin, _Fun ->
                    sin

                  el, sin, _Fun ->
                    err = {error_path(e, name), :xmerl_xsd, {:illegal_component_in_any, el}}
                    acc_errs(sin, err)
                end

                s2 = traverse.(e, s, traverse)
                {[e], rest, s2}

              _ ->
                {result, s2} = check_any(e, any, env, s)
                {[result], rest, s2}
            end
        end

      false when r_xsd_state(s, :num_el) >= min ->
        {[], xML, s}

      _ ->
        {:error, {error_path(e, name), :xmerl_xsd, {:element_bad_match, e, any, env}}}
    end
  end

  defp check_element_type([], cM, _Env, _Block, s, checked) do
    case cM do
      r_schema_simple_type() ->
        {newVal, s2} = check_type(cM, [], :unapplied, s)
        {newVal, [], s2}

      {:simpleType, _} ->
        {newVal, s2} = check_type(cM, [], :unapplied, s)
        {newVal, [], s2}

      _ ->
        {:error, {error_path(checked, :undefined), :xmerl_xsd, {:empty_content_not_allowed, cM}}}
    end
  end

  defp check_element_type([c = r_xmlComment() | rest], cM, env, block, s, checked) do
    check_element_type(rest, cM, env, block, s, [c | checked])
  end

  defp check_element_type(xML, cM, _Env, _Block, s, _Checked) do
    {:error, {error_path(xML, :undefined), :xmerl_xsd, {:match_failure, xML, cM, s}}}
  end

  defp check_text_type(xML = [r_xmlText() | _], :optional_text, s) do
    {xMLText, rest} = split_xmlText(xML)
    {xMLText, rest, s}
  end

  defp check_text_type(xML = [txt = r_xmlText() | _], type = {:simpleType, _}, s) do
    {xMLText, rest} = split_xmlText(xML)

    {newVal, s2} =
      check_type(
        type,
        flatten(
          for r_xmlText(value: x) <- xMLText do
            x
          end
        ),
        :unapplied,
        s
      )

    {[r_xmlText(txt, value: newVal)], rest, s2}
  end

  defp check_text_type(xML = [txt = r_xmlText() | _], type = r_schema_simple_type(), s) do
    {xMLText, rest} = split_xmlText(xML)

    {newVal, s2} =
      check_type(
        type,
        flatten(
          for r_xmlText(value: x) <- xMLText do
            x
          end
        ),
        :unapplied,
        s
      )

    {[r_xmlText(txt, value: newVal)], rest, s2}
  end

  defp check_text_type([xMLTxt = r_xmlText() | _], cMEl, _S) do
    {:error, {error_path(xMLTxt, :undefined), :xmerl_xsd, {:cannot_contain_text, xMLTxt, cMEl}}}
  end

  defp split_xmlText(xML) do
    splitwith(
      fn
        r_xmlText() ->
          true

        r_xmlComment() ->
          true

        _ ->
          false
      end,
      xML
    )
  end

  defp check_sequence([t = r_xmlText() | rest], els, occ, env, s, checked) do
    check_sequence(rest, els, occ, env, s, [t | checked])
  end

  defp check_sequence(
         seq = [_InstEl = r_xmlElement() | _],
         [el | els],
         occ = {_Min, _Max},
         env,
         s,
         checked
       ) do
    {resolvedT, s2} = resolve(el, s)

    case check_element_type(seq, resolvedT, env, [], count_num_el(s2), []) do
      {[], _, s3} ->
        case is_optional(el, s3) do
          true ->
            check_sequence(seq, els, occ, env, set_num_el(s3, 0), checked)

          _ ->
            {:error,
             {error_path(checked, :undefined), :xmerl_xsd, {:missing_mandatory_elements, el}}}
        end

      err = {:error, _Reason} ->
        case {is_optional(el, s), r_xsd_state(s, :num_el), get_occur(el)} do
          {true, _, _} ->
            check_sequence(seq, els, occ, env, set_num_el(s, 0), checked)

          {_, n, {_Min2, max}} when n >= max ->
            check_sequence(seq, els, occ, env, set_num_el(s, 0), checked)

          _ ->
            err
        end

      {ret, unValRest, s3} ->
        check_sequence(
          unValRest,
          [decrease_occurance(el) | els],
          occ,
          env,
          count_num_el(set_num_el(s3, s2)),
          ret ++ checked
        )
    end
  end

  defp check_sequence([c = r_xmlComment() | rest], els, occ, env, s, checked) do
    check_sequence(rest, els, occ, env, s, [c | checked])
  end

  defp check_sequence(rest, [], _Occ, _Env, s, checked) do
    {checked, rest, set_num_el(s, 0)}
  end

  defp check_sequence([], els, _Occ, _Env, s, checked) do
    case (for x = {_, y = {_, _}} <- els,
              optional(y) == false do
            x
          end) do
      [] ->
        {checked, [], set_num_el(s, 0)}

      mandatoryEls ->
        {:error,
         {error_path(checked, :undefined), :xmerl_xsd,
          {:missing_mandatory_elements, mandatoryEls}}}
    end
  end

  defp check_choice([t = r_xmlText() | rest], els, occ, env, s, checked) do
    case is_whitespace(t) do
      true ->
        check_choice(rest, els, occ, env, s, [t | checked])

      _ ->
        {:error, {error_path(t, :undefined), :xmerl_xsd, {:choice_missmatch, t, els}}}
    end
  end

  defp check_choice(ch = [r_xmlElement() | _], [el | els], occ, env, s, checked) do
    {resolvedT, s2} = resolve(el, s)

    case check_element_type(ch, resolvedT, env, [], count_num_el(s2), []) do
      {[], _, _S3} ->
        check_choice(ch, els, occ, env, s2, checked)

      {:error, _Reason} ->
        case (for x = r_xmlElement() <- checked do
                x
              end) do
          [] ->
            check_choice(ch, els, occ, env, s2, checked)

          _ ->
            {checked, ch, set_num_el(s, 0)}
        end

      {result, unValRest, s3} ->
        check_choice(
          unValRest,
          [el],
          occ,
          env,
          count_num_el(set_num_el(s3, s)),
          result ++ checked
        )
    end
  end

  defp check_choice([], _, _, _, s, checked) do
    {checked, [], set_num_el(s, 0)}
  end

  defp check_choice(xML, [], {0, _}, _, s, checked) do
    {checked, xML, set_num_el(s, 0)}
  end

  defp check_choice(xML, [], _, _, s, checked) do
    case r_xsd_state(s, :num_el) > 0 do
      true ->
        {checked, xML, set_num_el(s, 0)}

      _ ->
        {:error, {error_path(xML, :undefined), :xmerl_xsd, {:no_element_matching_choice, xML}}}
    end
  end

  defp check_all([t = r_xmlText() | restXML], cM, occ, env, s, checked, xML) do
    case is_whitespace(t) do
      true ->
        check_all(restXML, cM, occ, env, s, [t | checked], xML)

      _ ->
        {:error, {error_path(t, :undefined), :xmerl_xsd, {:all_missmatch, t, cM}}}
    end
  end

  defp check_all(
         xML = [e = r_xmlElement(name: name) | restXML],
         cM,
         occ,
         env,
         s,
         checked,
         prevXML
       ) do
    elName = mk_EII_QName(name, e, s)

    case search_delete_all_el(elName, cM, s) do
      {cMEl = {:element, _}, restCM} ->
        {resolvedT, s2} = resolve(cMEl, s)

        case check_element_type(xML, resolvedT, env, [], s2, []) do
          {[], _, _S3} ->
            err = {error_path(e, name), :xmerl_xsd, {:validation_error_all, elName, cM}}
            check_all(restXML, cM, occ, env, acc_errs(s, err), checked, prevXML)

          {:error, _} when :erlang.element(1, occ) == 0 ->
            {[], prevXML, s}

          {:error, reason} ->
            check_all(restXML, restCM, occ, env, acc_errs(s, reason), [e | checked], prevXML)

          {result, unValRest, s3} ->
            check_all(
              unValRest,
              restCM,
              occ,
              env,
              r_xsd_state(s3, scope: r_xsd_state(s, :scope)),
              result ++ checked,
              prevXML
            )
        end

      _ when :erlang.element(1, occ) == 0 ->
        {[], prevXML, s}

      _ ->
        err = {error_path(e, name), :xmerl_xsd, {:element_not_in_all, elName, e, cM}}
        check_all(restXML, cM, occ, env, acc_errs(s, err), [e | checked], prevXML)
    end
  end

  defp check_all([c = r_xmlComment() | restXML], cM, occ, env, s, checked, xML) do
    check_all(restXML, cM, occ, env, s, [c | checked], xML)
  end

  defp check_all(xML, [], _, _, s, checked, _) do
    {checked, xML, s}
  end

  defp check_all([], cM, _Occ, _, s, checked, _PrevXML) do
    case (for x = {_, y = {_, _}} <- cM,
              optional(y) == false do
            x
          end) do
      [] ->
        {checked, [], set_num_el(s, 0)}

      mandatoryEls ->
        {:error,
         {error_path(checked, :undefined), :xmerl_xsd,
          {:missing_mandatory_elements_in_all, mandatoryEls}}}
    end
  end

  defp check_any(e, any, _Env, s) do
    case (try do
            validate_xml(e, r_xsd_state(s, scope: []))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {[result], [], s2} ->
        {result, r_xsd_state(s2, scope: r_xsd_state(s, :scope))}

      {result, [], s2} ->
        {result, r_xsd_state(s2, scope: r_xsd_state(s, :scope))}

      {_, _Unvalidated, s2} ->
        err = {error_path(e, :undefined), :xmerl_xsd, {:failed_validating, e, any}}
        {e, acc_errs(r_xsd_state(s2, scope: r_xsd_state(s, :scope)), err)}

      {:error, reason} ->
        {e, acc_errs(s, reason)}

      {:EXIT, reason} ->
        err = {error_path(e, :undefined), :xmerl_xsd, {:internal_error, reason}}
        {e, acc_errs(s, err)}
    end
  end

  defp check_target_namespace(xMLEl, s) do
    case {r_xsd_state(s, :targetNamespace), r_xmlElement(xMLEl, :nsinfo)} do
      {:undefined, []} ->
        :ok

      {uRI, {prefix, _}} ->
        nS = r_xmlElement(xMLEl, :namespace)

        case namespace(prefix, nS, r_xmlNamespace(nS, :default)) do
          ^uRI ->
            :ok

          _ ->
            :failed
        end

      {uRI, _} ->
        case r_xmlNamespace(r_xmlElement(xMLEl, :namespace), :default) do
          ^uRI ->
            :ok

          _ ->
            :failed
        end
    end
  end

  defp schemaLocations(el = r_xmlElement(attributes: atts), s) do
    pred = fn
      r_xmlAttribute(name: :schemaLocation) ->
        false

      r_xmlAttribute(nsinfo: {_, 'schemaLocation'}) ->
        false

      _ ->
        true
    end

    case :lists.dropwhile(pred, atts) do
      [] ->
        s

      [r_xmlAttribute(value: paths) | _] ->
        case :string.tokens(paths, ' \n\t\r') do
          l when length(l) > 0 ->
            case rem(length(l), 2) do
              0 ->
                pairList = fn
                  [], _Fun ->
                    []

                  [sLNS, sLLoc | rest], fun ->
                    [{sLNS, sLLoc} | fun.(rest, fun)]
                end

                r_xsd_state(s, schemaLocations: pairList.(l, pairList))

              _ ->
                err =
                  {error_path(el, r_xmlElement(el, :name)), :xmerl_xsd,
                   {:schemaLocation_list_failure, paths}}

                acc_errs(s, err)
            end

          _ ->
            s
        end

      _ ->
        s
    end
  end

  defp blocking([], blockDefault) do
    blockDefault
  end

  defp blocking(block, _) do
    block
  end

  defp allow_empty_content([]) do
    true
  end

  defp allow_empty_content([{:restriction, {_BT, _CM = []}}]) do
    true
  end

  defp allow_empty_content([{:extension, {_BT, _CM = []}}]) do
    true
  end

  defp allow_empty_content([{_, {_, {0, _}}} | rest]) do
    allow_empty_content(rest)
  end

  defp allow_empty_content([{_, {content, _}} | rest]) do
    case allow_empty_content(content) do
      true ->
        allow_empty_content(rest)

      _ ->
        false
    end
  end

  defp allow_empty_content(_) do
    false
  end

  defp empty_xml_content([]) do
    true
  end

  defp empty_xml_content([h | t]) do
    case is_whitespace(h) do
      true ->
        empty_xml_content(t)

      _ ->
        false
    end
  end

  defp empty_xml_content(_) do
    false
  end

  defp xsi_factors(r_schema_element(nillable: n)) do
    [{:nillable, n}]
  end

  defp check_xsi_factors(
         {nil, _, :"http://www.w3.org/2001/XMLSchema-instance"},
         r_xmlAttribute(value: 'true'),
         xsiFactors,
         xMLEl,
         s
       ) do
    case key1search(:nillable, xsiFactors, false) do
      {_, true} ->
        case empty_xml_content(r_xmlElement(xMLEl, :content)) do
          true ->
            s

          _ ->
            err =
              {error_path(xMLEl, r_xmlElement(xMLEl, :name)), :xmerl_xsd,
               {:element_content_not_nil, xMLEl}}

            acc_errs(s, err)
        end

      _ ->
        s
    end
  end

  defp check_xsi_factors(_, _, _, _, s) do
    s
  end

  defp check_attributes(
         xMLEl = r_xmlElement(attributes: atts),
         r_schema_complex_type(name: name, attributes: schemaAtts),
         xsiFactors,
         s
       ) do
    oldScope = r_xsd_state(s, :scope)
    schemaAtts2 = resolve_attributeGroups(schemaAtts, xMLEl, s)
    {xMLEl2, s2} = check_attributes(atts, schemaAtts2, xMLEl, xsiFactors, name_scope(name, s), [])
    {xMLEl2, r_xsd_state(s2, scope: oldScope)}
  end

  defp check_attributes(xMLEl = r_xmlElement(attributes: []), _, _, s) do
    {xMLEl, s}
  end

  defp check_attributes(xMLEl = r_xmlElement(name: n, attributes: atts), _, xsiFactors, s) do
    fun = fn attX, s_in ->
      case reserved_attribute(attX, r_xmlElement(xMLEl, :namespace)) do
        true ->
          attQName = mk_EII_QName(r_xmlAttribute(attX, :name), xMLEl, s_in)
          check_xsi_factors(attQName, attX, xsiFactors, xMLEl, s_in)

        _ ->
          err = {error_path(xMLEl, n), :xmerl_xsd, {:attribute_in_simpleType, xMLEl, attX}}
          acc_errs(s_in, err)
      end
    end

    {xMLEl, foldl(fun, s, atts)}
  end

  defp check_attributes([], [sA | schemaAtts], xMLEl, xsiFactors, s, checkedAtts) do
    case resolve(sA, s) do
      {r_schema_attribute(name: name, use: use, default: def__, fixed: fix), s2} ->
        case {use, def__, fix} do
          {:required, _, _} ->
            err =
              {error_path(xMLEl, r_xmlElement(xMLEl, :name)), :xmerl_xsd,
               {:required_attribute_missed, xMLEl, name}}

            check_attributes([], schemaAtts, xMLEl, xsiFactors, acc_errs(s2, err), checkedAtts)

          {:optional, :undefined, :undefined} ->
            check_attributes([], schemaAtts, xMLEl, xsiFactors, s2, checkedAtts)

          {:optional, default, :undefined} ->
            newAtt = create_attribute(name, default)
            check_attributes([], schemaAtts, xMLEl, xsiFactors, s2, [newAtt | checkedAtts])

          {:optional, :undefined, ^fix} ->
            newAtt = create_attribute(name, def__)
            check_attributes([], schemaAtts, xMLEl, xsiFactors, s2, [newAtt | checkedAtts])

          {:optional, default, ^fix} ->
            err =
              {error_path(xMLEl, r_xmlElement(xMLEl, :name)), :xmerl_xsd,
               {:default_and_fixed_attributes_mutual_exclusive, name, default, fix}}

            check_attributes([], schemaAtts, xMLEl, xsiFactors, acc_errs(s2, err), checkedAtts)

          _ ->
            check_attributes([], schemaAtts, xMLEl, xsiFactors, s2, checkedAtts)
        end

      {{:anyAttribute, {_Namespaces, _PC}}, s2} ->
        check_attributes([], schemaAtts, xMLEl, xsiFactors, s2, checkedAtts)

      err ->
        errMsg =
          {error_path(xMLEl, r_xmlElement(xMLEl, :name)), :xmerl_xsd,
           {:schema_error, :unexpected_object, sA, err}}

        check_attributes([], schemaAtts, xMLEl, xsiFactors, acc_errs(s, errMsg), checkedAtts)
    end
  end

  defp check_attributes([], [], xMLEl, _XsiFactors, s, checkedAtts) do
    {r_xmlElement(xMLEl, attributes: reverse(checkedAtts)), s}
  end

  defp check_attributes([att | atts], schemaAtts, xMLEl, xsiFactors, s, checkedAtts) do
    {isQ, attQName} = mk_EII_Att_QName(r_xmlAttribute(att, :name), xMLEl, s)

    case search_attribute(isQ, attQName, schemaAtts) do
      {attObj = {:attribute, _}, schemaAtts2} ->
        {sA, s2} = load_object(attObj, s)
        r_schema_attribute(type: [attType]) = sA
        {val, s4} = check_type(attType, r_xmlAttribute(att, :value), :unapplied, s2)

        check_attributes(atts, schemaAtts2, xMLEl, xsiFactors, s4, [
          r_xmlAttribute(att, value: val) | checkedAtts
        ])

      {:undefined, schemaAtts2} ->
        case reserved_attribute(att, r_xmlElement(xMLEl, :namespace)) do
          true ->
            s2 = check_xsi_factors(attQName, att, xsiFactors, xMLEl, s)
            check_attributes(atts, schemaAtts2, xMLEl, xsiFactors, s2, [att | checkedAtts])

          _ ->
            case check_anyAttribute(att, schemaAtts2, xMLEl, s) do
              {:error, reason} ->
                check_attributes(
                  atts,
                  schemaAtts2,
                  xMLEl,
                  xsiFactors,
                  acc_errs(s, reason),
                  checkedAtts
                )

              {att2, s2} ->
                check_attributes(atts, schemaAtts2, xMLEl, xsiFactors, s2, [att2 | checkedAtts])
            end
        end

      other ->
        err = {[], :xmerl_xsd, {:internal_error, other}}
        check_attributes(atts, schemaAtts, xMLEl, xsiFactors, acc_errs(s, err), checkedAtts)
    end
  end

  defp check_anyAttribute(att, schemaAtts, el = r_xmlElement(name: name, namespace: nS), s) do
    case (for any = {:anyAttribute, _} <- schemaAtts do
            any
          end) do
      [] ->
        {:error,
         {error_path(el, name), :xmerl_xsd,
          {:attribute_not_defined_in_schema, r_xmlAttribute(att, :name)}}}

      [{_, {namespace, pC}} | _] ->
        case check_anyAttribute_namespace(namespace, nS) do
          :ok ->
            check_anyAttribute2(namespace, pC, att, nS, s)

          _ ->
            {:error,
             {error_path(el, name), :xmerl_xsd,
              {:disallowed_namespace, namespace, nS, r_xmlAttribute(att, :name)}}}
        end
    end
  end

  defp check_anyAttribute2(_, pC, att, _, s)
       when pC == :skip or
              pC == :lax do
    {att, s}
  end

  defp check_anyAttribute2(_Namespace, _, att, _NS, s) do
    {att, s}
  end

  defp check_anyAttribute_namespace([:"##any" | _], _NS) do
    :ok
  end

  defp check_anyAttribute_namespace([:absent], _NS) do
    :ok
  end

  defp check_anyAttribute_namespace([nS | _], nS) do
    :ok
  end

  defp check_anyAttribute_namespace([{:not, nS} | _], nS) do
    false
  end

  defp check_anyAttribute_namespace([_H | t], nS) do
    check_anyAttribute_namespace2(t, nS)
  end

  defp check_anyAttribute_namespace2([nS | _], nS) do
    :ok
  end

  defp check_anyAttribute_namespace2([_H | t], nS) do
    check_anyAttribute_namespace2(t, nS)
  end

  defp check_anyAttribute_namespace2([], _NS) do
    false
  end

  defp resolve_attributeGroups(schemaAtts, el, s) do
    resolve_attributeGroups(schemaAtts, el, s, [], [])
  end

  defp resolve_attributeGroups([aG = {:attributeGroup, _} | schemaAtts], el, s, parents, acc) do
    case resolve(aG, s) do
      {r_schema_attribute_group(name: name, content: aGC), _S2} ->
        case {member(name, parents), r_xsd_state(s, :redefine)} do
          {true, false} ->
            err =
              {error_path(el, r_xmlElement(el, :name)), :xmerl_xsd,
               {:cirkular_attributeGroup_reference, name}}

            resolve_attributeGroups(schemaAtts, el, acc_errs(s, err), parents, acc)

          {true, _} ->
            resolve_attributeGroups(schemaAtts, el, s, parents, acc)

          _ ->
            resolve_attributeGroups(aGC ++ [:marker | schemaAtts], el, s, [name | parents], acc)
        end

      err ->
        errMsg =
          {error_path(el, r_xmlElement(el, :name)), :xmerl_xsd,
           {:schema_error, :unexpected_object, aG, err}}

        resolve_attributeGroups(schemaAtts, el, acc_errs(s, errMsg), parents, acc)
    end
  end

  defp resolve_attributeGroups([:marker | t], el, s, [_P | ps], acc) do
    resolve_attributeGroups(t, el, s, ps, acc)
  end

  defp resolve_attributeGroups([h | t], el, s, parents, acc) do
    resolve_attributeGroups(t, el, s, parents, [h | acc])
  end

  defp resolve_attributeGroups([], _, _, _, acc) do
    acc
  end

  defp check_type(type = r_schema_simple_type(), value, facetS, s) do
    check_simpleType(type, value, facetS, s)
  end

  defp check_type({:simpleType, {:anySimpleType, _}}, value, _FacetS, s) do
    {value, s}
  end

  defp check_type({:union, types}, value, _FacetS, s) do
    check_union_types(types, value, s)
  end

  defp check_type(sT = {:simpleType, qName = {name, _Scope, _NS}}, value, facetS, s) do
    case is_builtin_simple_type(qName) do
      true ->
        {constrainedValue, s2} = constrained(qName, default_facets(facetS, name), value, s)

        case :xmerl_xsd_type.check_simpleType(name, constrainedValue, s2) do
          {:ok, _} when name == :IDREF or name == :IDREFS ->
            {constrainedValue, s2}

          {:ok, _} ->
            {constrainedValue, s2}

          {:error, reason} ->
            :no_debug
            {value, acc_errs(s2, reason)}
        end

      _ ->
        case resolve(sT, s) do
          {[], s2} ->
            err = {[], :xmerl_xsd, {:could_not_resolve_type, sT}}
            {value, acc_errs(s2, err)}

          {refedST, s2} ->
            check_type(refedST, value, :unapplied, s2)
        end
    end
  end

  defp check_type(type, value, _FacetS, s) do
    err = {[], :xmerl_xsd, {:could_not_check_value_for_type, type}}
    :no_debug
    {value, acc_errs(s, err)}
  end

  defp check_simpleType(
         r_schema_simple_type(base_type: bT, final: _Final, facets: facets, content: type),
         value,
         facetS,
         s
       ) do
    case {bT, type} do
      {{_ST, _, _}, _} ->
        case is_builtin_simple_type(bT) do
          true ->
            {constrainedValue, s2} =
              constrained(
                bT,
                merge_facets(
                  default_facets(
                    facetS,
                    bT
                  ),
                  facets
                ),
                value,
                s
              )

            {_, _S3} = check_type({:simpleType, bT}, constrainedValue, :applied, s2)

          _ ->
            case resolve({:simpleType, bT}, s) do
              {baseST = r_schema_simple_type(facets: facets2), _} ->
                check_simpleType(
                  r_schema_simple_type(baseST, facets: facets ++ facets2),
                  value,
                  :unapplied,
                  s
                )

              _ ->
                err = {[], :xmerl_xsd, {:unknown_simpleType, bT}}
                {value, acc_errs(s, err)}
            end
        end

      {_, [cT]} ->
        {_, _S2} = check_type(cT, value, :unapplied, s)
    end
  end

  defp check_union_types(types, value, s) do
    check_union_types(types, types, value, s)
  end

  defp check_union_types([], uT, value, s) do
    acc_errs(
      s,
      {[], :xmerl_xsd, {:value_not_valid, value, uT}}
    )
  end

  defp check_union_types([t | ts], uT, value, s = r_xsd_state(errors: errs)) do
    case check_type(t, value, :unapplied, s) do
      {val, s2 = r_xsd_state(errors: ^errs)} ->
        {val, s2}

      {_, _} ->
        check_union_types(ts, uT, value, s)
    end
  end

  defp reserved_attribute({rA, _, :"http://www.w3.org/2001/XMLSchema-instance"}, _)
       when rA == :type or rA == nil or
              rA == :schemaLocation or
              rA == :noNamespaceSchemaLocation do
    true
  end

  defp reserved_attribute(r_xmlAttribute(name: name), r_xmlNamespace(nodes: nSNodes)) do
    nameStr =
      cond do
        is_atom(name) ->
          :erlang.atom_to_list(name)

        true ->
          name
      end

    case :string.tokens(nameStr, ':') do
      ['xmlns' | _] ->
        true

      [prefix, instAtt]
      when instAtt == 'type' or instAtt == 'nil' or
             instAtt == 'schemaLocation' or instAtt == 'noNamespaceSchemaLocation' ->
        case keyNsearch(:"http://www.w3.org/2001/XMLSchema-instance", 2, nSNodes, []) do
          {^prefix, _} ->
            true

          _ ->
            false
        end

      _ ->
        false
    end
  end

  defp reserved_attribute(_, _) do
    false
  end

  defp default_facets(:applied, _) do
    []
  end

  defp default_facets(_, type) do
    default_facets(type)
  end

  defp default_facets({name, _, _}) when is_list(name) do
    default_facets(:erlang.list_to_atom(name))
  end

  defp default_facets({name, _, _}) do
    default_facets(name)
  end

  defp default_facets(typeName) do
    case is_xsd_string(typeName) do
      false ->
        [{:whiteSpace, 'collapse'}]

      _ ->
        []
    end
  end

  defp merge_facets([], definedF) do
    definedF
  end

  defp merge_facets([f = {name, _} | rest], definedF) do
    merge_facets(rest, keyreplace(name, 1, definedF, f))
  end

  defp constrained({t, _, _}, facets, value, s) do
    facetFuns =
      for f <- facets do
        facet_fun(t, f)
      end

    constrained2(facetFuns, value, s)
  end

  defp constrained2([], value, s) do
    {value, s}
  end

  defp constrained2([facet | restFacets], value, s) do
    case facet.(value) do
      {:error, reason} ->
        constrained2(restFacets, value, acc_errs(s, reason))

      {:ok, newValue} ->
        constrained2(restFacets, newValue, s)
    end
  end

  defp id_constraints(cMEl, xMLEl, s) do
    s1 = check_uniqueness(r_schema_element(cMEl, :uniqueness), xMLEl, s)

    s2 =
      check_keys(
        for {:key, x} <- r_schema_element(cMEl, :key) do
          x
        end,
        xMLEl,
        s1
      )

    prepare_keyrefs(
      for {:keyref, x} <- r_schema_element(cMEl, :key) do
        x
      end,
      xMLEl,
      s2
    )
  end

  defp check_abstract(elName, el, r_schema_element(name: elName, abstract: true), s) do
    acc_errs(
      s,
      {error_path(el, r_xmlElement(el, :name)), :xmerl_xsd, {:abstract_element_instance, elName}}
    )
  end

  defp check_abstract(elName, _El, r_schema_element(name: elName), s) do
    s
  end

  defp check_abstract(elName, el, r_schema_element(), s) do
    {xMLEl, _S2} = load_object({:element, elName}, s)
    check_abstract(elName, el, xMLEl, s)
  end

  defp check_form(
         {localName, _, namespace},
         localName,
         el = r_xmlElement(name: name, namespace: nS),
         :qualified,
         s
       ) do
    case r_xmlNamespace(nS, :default) do
      ^namespace ->
        s

      _ ->
        acc_errs(
          s,
          {error_path(el, name), :xmerl_xsd, {:qualified_name_required, localName}}
        )
    end
  end

  defp check_form({localName, _, _}, localName, _El, _ActualFormDefault, s) do
    s
  end

  defp check_form({_LocalName, [], _}, _QualifiedName, _El, _ActualFormDefault, s) do
    s
  end

  defp check_form({_LocalName, _, _}, qualifiedName, el, :unqualified, s) do
    acc_errs(
      s,
      {error_path(el, r_xmlElement(el, :name)), :xmerl_xsd,
       {:unqualified_name_required, qualifiedName}}
    )
  end

  defp check_form({_LocalName, _, _}, _QualifiedName, _El, _ActualFormDefault, s) do
    s
  end

  defp actual_form_value(:undefined, globalForm) do
    globalForm
  end

  defp actual_form_value(localForm, _) do
    localForm
  end

  defp check_uniqueness(:undefined, _, s) do
    s
  end

  defp check_uniqueness(unique, xMLEl, s) do
    case unique do
      [
        {:unique,
         r_id_constraint(
           selector: {:selector, selectorPath},
           fields: fields
         )}
      ] ->
        targetNodeSet = target_node_set(selectorPath, xMLEl, s)

        case qualified_node_set(fields, targetNodeSet, xMLEl, s) do
          {[], s1} ->
            s1

          {[_E], s1} ->
            s1

          {l, s1} when is_list(l) ->
            key_sequence_uniqueness(l, xMLEl, s1)
        end

      _ ->
        s
    end
  end

  defp target_node_set(selectorPath, xMLEl, s) do
    :xmerl_xpath.string(selectorPath, xMLEl, [{:namespace, r_xsd_state(s, :namespace_nodes)}])
  end

  defp qualified_node_set(fields, set, el, s) do
    qualified_node_set(
      for {:field, x} <- fields do
        x
      end,
      set,
      el,
      s,
      []
    )
  end

  defp qualified_node_set([], _Set, _El, s, acc) do
    {acc, s}
  end

  defp qualified_node_set(_, [], _El, s, acc) do
    {acc, s}
  end

  defp qualified_node_set(paths, [qN | qNs], el, s, acc) do
    fun = fn p, sx ->
      case apply_field(p, qN, sx) do
        l when length(l) <= 1 ->
          {l, sx}

        err ->
          retErr =
            {error_path(el, r_xmlElement(el, :name)), :xmerl_xsd,
             {:illegal_key_sequence_value, err}}

          {[], acc_errs(sx, retErr)}
      end
    end

    {keySequence, s2} = mapfoldl(fun, s, paths)

    case flatten(keySequence) do
      [] ->
        qualified_node_set(paths, qNs, el, s2, acc)

      kS ->
        qualified_node_set(paths, qNs, el, s2, [kS | acc])
    end
  end

  defp apply_field(f, el, s) do
    :xmerl_xpath.string(f, el, [{:namespace, r_xsd_state(s, :namespace_nodes)}])
  end

  defp check_keys([], _XMLEl, s) do
    s
  end

  defp check_keys(
         [
           key =
             r_id_constraint(
               selector: {:selector, selectorPath},
               fields: fields
             )
           | keys
         ],
         xMLEl,
         s
       ) do
    targetNodeSet = target_node_set(selectorPath, xMLEl, s)

    s3 =
      case qualified_node_set(fields, targetNodeSet, xMLEl, s) do
        {l, s1} when length(l) == length(targetNodeSet) ->
          s2 = key_sequence_uniqueness(l, xMLEl, s1)
          _ = save_key(r_id_constraint(key, key_sequence: l), s2)
          s2

        {err, s1} ->
          acc_errs(
            s1,
            {error_path(xMLEl, r_xmlElement(xMLEl, :name)), :xmerl_xsd,
             {:qualified_node_set_not_correct_for_key, err}}
          )
      end

    check_keys(keys, xMLEl, s3)
  end

  defp prepare_keyrefs([], _XMLEl, s) do
    s
  end

  defp prepare_keyrefs(
         [
           keyRef =
             r_id_constraint(
               selector: {:selector, selectorPath},
               fields: fields
             )
           | rest
         ],
         xMLEl,
         s
       ) do
    targetNodeSet = target_node_set(selectorPath, xMLEl, s)
    {l, s1} = qualified_node_set(fields, targetNodeSet, xMLEl, s)
    save_keyref(r_id_constraint(keyRef, key_sequence: l), s1)
    prepare_keyrefs(rest, xMLEl, s1)
  end

  defp key_sequence_uniqueness([], _, s) do
    s
  end

  defp key_sequence_uniqueness([_H], _, s) do
    s
  end

  defp key_sequence_uniqueness([kS = [f1 | fRest] | kSs], el, s) do
    case is_key_sequence_equal(f1, kSs) do
      {true, tailOfEquals} ->
        s1 =
          case k_s_u(fRest, tailOfEquals, s) do
            true ->
              acc_errs(
                s,
                {error_path(el, r_xmlElement(el, :name)), :xmerl_xsd, {:key_value_not_unique, kS}}
              )

            _ ->
              s
          end

        key_sequence_uniqueness(kSs, el, s1)

      false ->
        key_sequence_uniqueness(kSs, el, s)
    end
  end

  defp k_s_u([], _, _) do
    true
  end

  defp k_s_u([f | fs], kSs, s) do
    case is_key_sequence_equal(f, kSs) do
      {true, tailOfEquals} ->
        k_s_u(fs, tailOfEquals, s)

      _ ->
        false
    end
  end

  defp is_key_sequence_equal(f, kSs) do
    is_key_sequence_equal(f, kSs, [])
  end

  defp is_key_sequence_equal(_F, [], []) do
    false
  end

  defp is_key_sequence_equal(_F, [], acc) do
    {true, reverse(acc)}
  end

  defp is_key_sequence_equal(f, [[f1 | tlF1] | rest], acc) do
    case is_key_el_equal(f, f1) do
      true ->
        is_key_sequence_equal(f, rest, [tlF1 | acc])

      false ->
        is_key_sequence_equal(f, rest, acc)
    end
  end

  defp is_key_el_equal(r_xmlElement(content: c1), r_xmlElement(content: c2)) do
    is_equal_content(c1, c2)
  end

  defp is_key_el_equal(r_xmlAttribute(value: v1), r_xmlAttribute(value: v1)) do
    true
  end

  defp is_key_el_equal(_, _) do
    false
  end

  defp is_equal_content([t1 | rest1], [t2 | rest2])
       when elem(t1, 0) === :xmlText and
              elem(t2, 0) === :xmlText do
    case is_whitespace(t1) do
      true ->
        case is_whitespace(t2) do
          true ->
            is_equal_content(rest1, rest2)

          _ ->
            is_equal_content(rest1, [t2 | rest2])
        end

      _ ->
        case r_xmlText(t1, :value) == r_xmlText(t2, :value) do
          true ->
            is_equal_content(rest1, rest2)

          _ ->
            false
        end
    end
  end

  defp is_equal_content([], []) do
    true
  end

  defp is_equal_content(_, _) do
    false
  end

  defp schema_concistence_checks(s) do
    s2 = check_keyrefs(s)
    s3 = check_references(s2)

    s4 =
      check_substitutionGroups(
        r_xsd_state(s3, :substitutionGroups),
        s3
      )

    s5 = check_cyclic_defs(s4)
    reset_state(s5)
  end

  defp reset_state(s) do
    r_xsd_state(s,
      keyrefs: [],
      IDs: [],
      unchecked_references: [],
      substitutionGroups: [],
      derived_types: [],
      circularity_stack: [],
      circularity_disallowed: []
    )
  end

  defp check_keyrefs(s) do
    keyRefs = r_xsd_state(s, :keyrefs)

    keyExist = fn
      {:keyref, name, refer}, s_in ->
        case load_key(refer, s_in) do
          key = r_id_constraint() ->
            check_keyref_cardinality(name, load_keyref(name, s_in), key, s_in)

          _ ->
            acc_errs(
              s_in,
              {[], :xmerl_xsd, {:keyref_missed_matching_key, refer}}
            )
        end

      other, s_in ->
        acc_errs(
          s_in,
          {[], :xmerl_xsd, {:keyref_unexpected_object, other}}
        )
    end

    foldl(keyExist, s, keyRefs)
  end

  defp check_keyref_cardinality(
         _,
         kR = r_id_constraint(category: :keyref, fields: keyRefFs),
         k = r_id_constraint(fields: keyFs),
         s
       ) do
    case length(keyRefFs) == length(keyFs) do
      true ->
        s

      _ ->
        acc_errs(
          s,
          {[], :xmerl_xsd, {:cardinality_of_fields_not_equal, kR, k}}
        )
    end
  end

  defp check_keyref_cardinality(name, _, _, s) do
    acc_errs(
      s,
      {[], :xmerl_xsd, {:could_not_load_keyref, name}}
    )
  end

  defp check_references(s) when elem(s, 0) === :xsd_state do
    check_references(r_xsd_state(s, :unchecked_references), s)
  end

  defp check_references([], s) do
    s
  end

  defp check_references([h | t], s) do
    check_references(t, check_reference(h, s))
  end

  defp check_reference(ref = {:attribute, _}, s) do
    case load_object(ref, s) do
      {r_schema_attribute(), s2} ->
        s2

      _ ->
        acc_errs(
          s,
          {[], :xmerl_xsd, {:reference_undeclared, :attribute, ref}}
        )
    end
  end

  defp check_reference(ref = {:element, _}, s) do
    case load_object(ref, s) do
      {r_schema_element(), s2} ->
        s2

      _ ->
        acc_errs(
          s,
          {[], :xmerl_xsd, {:reference_undeclared, :element, ref}}
        )
    end
  end

  defp check_reference(ref = {:attributeGroup, _}, s) do
    case load_object(ref, s) do
      {r_schema_attribute_group(), s2} ->
        s2

      _ ->
        acc_errs(
          s,
          {[], :xmerl_xsd, {:reference_undeclared, :attributeGroup, ref}}
        )
    end
  end

  defp check_reference(ref = {:group, _}, s) do
    case load_object(ref, s) do
      {r_schema_group(), s2} ->
        s2

      _ ->
        acc_errs(
          s,
          {[], :xmerl_xsd, {:reference_undeclared, :group, ref}}
        )
    end
  end

  defp check_reference(ref = {:simpleType, _}, s) do
    case load_object(ref, s) do
      {r_schema_simple_type(), s2} ->
        s2

      _ ->
        acc_errs(
          s,
          {[], :xmerl_xsd, {:reference_undeclared, :simpleType, ref}}
        )
    end
  end

  defp check_reference(ref = {:complexType, _}, s) do
    case load_object(ref, s) do
      {r_schema_complex_type(), s2} ->
        s2

      _ ->
        acc_errs(
          s,
          {[], :xmerl_xsd, {:reference_undeclared, :complexType, ref}}
        )
    end
  end

  defp check_reference(
         {:simple_or_complex_Type, ref},
         s = r_xsd_state(errors: errs)
       ) do
    case check_reference({:complexType, ref}, s) do
      s2 = r_xsd_state(errors: ^errs) ->
        s2

      _ ->
        check_reference({:simpleType, ref}, s)
    end
  end

  defp check_reference(ref, s) do
    acc_errs(
      s,
      {[], :xmerl_xsd, {:internal_error, :unknown_reference, ref}}
    )
  end

  defp check_substitutionGroups([], s) do
    s
  end

  defp check_substitutionGroups(sGs, s) do
    s2 = check_substGr_acyclic(sGs, s)
    s3 = check_substGr_type_structure(sGs, s2)
    save_substitutionGroup(sGs, s3)
  end

  defp check_substGr_acyclic(sGs, s) do
    set = :sofs.family(sGs)

    case (try do
            :sofs.family_to_digraph(set, [:acyclic])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, {:cyclic, _}} ->
        acc_errs(
          s,
          {[], :xmerl_xsd, {:cyclic_substitutionGroup, sGs}}
        )

      dG ->
        :digraph.delete(dG)
        s
    end
  end

  defp check_substGr_type_structure([sG | sGs], s) do
    check_substGr_type_structure(
      sGs,
      check_substGr_type_structure2(sG, s)
    )
  end

  defp check_substGr_type_structure([], s) do
    s
  end

  defp check_substGr_type_structure2({head, sGMembers}, s) do
    typeCheck = fn sG, s_in ->
      case (try do
              cmp_substGr_types(head, sG, s_in)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:EXIT, _} ->
          acc_errs(
            s_in,
            {[], :xmerl_xsd, {:substitutionGroup_error, head, sG}}
          )

        s_out ->
          s_out
      end
    end

    foldl(typeCheck, s, sGMembers)
  end

  defp cmp_substGr_types(head, sG, s) do
    {headElement, s2} = load_object({:element, head}, s)
    {memberElement, s3} = load_object({:element, sG}, s2)

    case (try do
            derived_or_equal(
              r_schema_element(memberElement, :type),
              r_schema_element(headElement, :type),
              [],
              s3
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      s4 = r_xsd_state() ->
        s4

      _ ->
        acc_errs(
          s3,
          {[], :xmerl_xsd,
           {:internal_error, :derived_or_equal, r_schema_element(memberElement, :type),
            r_schema_element(headElement, :type)}}
        )
    end
  end

  defp check_cyclic_defs(s = r_xsd_state(circularity_disallowed: cA)) do
    set = :sofs.relation_to_family(:sofs.relation(cA))

    case (try do
            :sofs.family_to_digraph(set, [:acyclic])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, {:cyclic, _}} ->
        acc_errs(s, {[], :xmerl_xsd, {:cyclic_definition, cA}})

      dG ->
        :digraph.delete(dG)
        s
    end
  end

  defp derived_or_equal(type, type, _Block, s) do
    s
  end

  defp derived_or_equal([memberTypeRef], [headTypeRef], block, s) do
    {headType, _} = resolve(headTypeRef, s)
    {memberType, _} = resolve(memberTypeRef, s)
    derived_or_equal_types(memberType, headType, :schema, block, s)
  end

  defp derived_or_equal_types(memT, {:anyType, _}, env, block, s) do
    case memT do
      r_schema_simple_type(content: cntnt) ->
        is_derivation_blocked(env, block, cntnt, s)

      r_schema_complex_type(content: cntnt) ->
        is_derivation_blocked(env, block, cntnt, s)

      _ ->
        s
    end
  end

  defp derived_or_equal_types(
         memT = r_schema_simple_type(name: mem, base_type: memBase),
         r_schema_simple_type(name: head),
         env,
         block,
         s
       )
       when mem == head or memBase == head do
    is_derivation_blocked(env, block, r_schema_simple_type(memT, :content), s)
  end

  defp derived_or_equal_types({:simpleType, name}, {:simpleType, name}, _Env, _Block, s) do
    s
  end

  defp derived_or_equal_types(
         r_schema_simple_type(base_type: name, content: content),
         {:simpleType, name},
         env,
         block,
         s
       ) do
    is_derivation_blocked(env, block, content, s)
  end

  defp derived_or_equal_types(
         r_schema_simple_type(content: [{loU, [content]}]),
         simpleType,
         env,
         block,
         s
       )
       when loU == :list or loU == :union do
    {newMemType, s2} = resolve(content, s)
    derived_or_equal_types(newMemType, simpleType, env, block, s2)
  end

  defp derived_or_equal_types(
         memT = r_schema_complex_type(name: mem, base_type: memBase),
         r_schema_complex_type(name: head),
         env,
         block,
         s
       )
       when mem == head or memBase == head do
    is_derivation_blocked(env, block, r_schema_complex_type(memT, :content), s)
  end

  defp derived_or_equal_types(memT, headT, _Env, _Block, s) do
    acc_errs(
      s,
      {[], :xmerl_xsd, {:type_of_element_not_derived, memT, headT}}
    )
  end

  defp is_derivation_blocked(:schema, _, _, s) do
    s
  end

  defp is_derivation_blocked(:instance, [:"#all"], derivation, s) do
    acc_errs(s, {:derivation_blocked, :"#all", derivation})
  end

  defp is_derivation_blocked(:instance, [], _, s) do
    s
  end

  defp is_derivation_blocked(:instance, block, c = [{derivation, _}], s) do
    case member(derivation, block) do
      true ->
        acc_errs(
          s,
          {[], :xmerl_xsd, {:derivation_blocked, derivation, c}}
        )

      _ ->
        s
    end
  end

  defp is_derivation_blocked(:instance, _Block, _, s) do
    s
  end

  defp create_attribute(qName, value) do
    {name, _Scope, nSName} = qName
    r_xmlAttribute(name: name, namespace: {name, nSName}, value: value)
  end

  defp mk_name(l) do
    mk_name(l, [])
  end

  defp mk_name([], _Acc) do
    []
  end

  defp mk_name([h], []) do
    h
  end

  defp mk_name([h], acc) do
    :erlang.list_to_atom(:lists.concat([h, :_ | acc]))
  end

  defp mk_name([h | t], []) do
    mk_name(t, [h])
  end

  defp mk_name([h1 | t], acc) do
    mk_name(t, [h1, :_ | acc])
  end

  defp cmp_name({lName, scope, nS}, {lName, scope, nS}, _S) do
    true
  end

  defp cmp_name(xMLName = {_, scope, nS}, cMName = {_, scope, nS}, s) do
    {el, _S2} = load_object({:element, xMLName}, s)
    cmp_SG_name(el, cMName, s)
  end

  defp cmp_name(_, _, _) do
    false
  end

  defp cmp_SG_name(r_schema_element(substitutionGroup: name), name, _S) do
    true
  end

  defp cmp_SG_name(r_schema_element(substitutionGroup: sGName), cMName, s) do
    cmp_name(sGName, cMName, s)
  end

  defp cmp_SG_name(_, _, _) do
    false
  end

  defp cmp_any_namespace({_, _, eIINS}, namespace, _S) do
    case member(eIINS, namespace) do
      true ->
        true

      _ ->
        case keysearch(eIINS, 2, namespace) do
          {:value, {:not, ^eIINS}} ->
            false

          _ ->
            true
        end
    end
  end

  defp at_least_one({_Min, max}) when max > 0 do
    true
  end

  defp at_least_one(_) do
    false
  end

  defp is_optional({:element, {_, {0, _}}}, _S) do
    true
  end

  defp is_optional({:any, {_, {0, _}, _}}, _S) do
    true
  end

  defp is_optional({mG, {_CM, {0, _}}}, _S)
       when mG === :all or
              mG === :sequence or mG === :choice do
    true
  end

  defp is_optional({mG, {cM, _Occ}}, s)
       when mG === :all or
              mG === :sequence or mG === :choice do
    case member(
           false,
           for y <- cM do
             is_optional(y, s)
           end
         ) do
      true ->
        false

      _ ->
        true
    end
  end

  defp is_optional({:group, {_, {0, _}}}, _S) do
    true
  end

  defp is_optional(g = {:group, _}, s) do
    case resolve(g, s) do
      {r_schema_group(content: []), _} ->
        true

      {r_schema_group(content: [cM]), _} ->
        is_optional(cM, s)
    end
  end

  defp is_optional(_, _) do
    false
  end

  defp acc_errs(s = r_xsd_state(errors: errs), errMsg) do
    r_xsd_state(s, errors: [errMsg | errs])
  end

  defp error_path([h | _T], top) when h == r_xmlElement() or h == r_xmlText() do
    error_path(h, top)
  end

  defp error_path([_H | t], top) do
    error_path(t, top)
  end

  defp error_path(r_xmlElement(parents: ps, pos: pos), top) do
    error_path(ps, pos, top)
  end

  defp error_path(r_xmlAttribute(parents: ps, pos: pos), top) do
    error_path(ps, pos, top)
  end

  defp error_path(r_xmlText(parents: ps, pos: pos), top) do
    error_path(ps, pos, top)
  end

  defp error_path(_, _) do
    []
  end

  defp error_path([], pos, top) when is_integer(pos) do
    mk_xpath_path([{top, pos}])
  end

  defp error_path([], _, top) do
    top
  end

  defp error_path(nodes, _, _) do
    mk_xpath_path(nodes)
  end

  defp mk_xpath_path(nodes) do
    slash = fn
      [h1, h2 | t], fun, acc ->
        fun.([h2 | t], fun, ['/', h1 | acc])

      [h1], _, acc ->
        [h1 | acc]

      [], _, acc ->
        acc
    end

    flatten(
      slash.(
        for {a, b} <- nodes do
          :lists.concat([a, '[', b, ']'])
        end,
        slash,
        []
      )
    )
  end

  defp resolve(xSDType, instanceEl, s) do
    explicit_type(xSDType, instanceEl, s)
  end

  defp resolve([h], s) do
    resolve(h, s)
  end

  defp resolve(any = {:any, _}, s) do
    {any, s}
  end

  defp resolve(any = {:anyAttribute, _}, s) do
    {any, s}
  end

  defp resolve(any = {:anyType, _}, s) do
    {any, s}
  end

  defp resolve(seq = {:sequence, _}, s) do
    {seq, s}
  end

  defp resolve(choice = {:choice, _}, s) do
    {choice, s}
  end

  defp resolve({:simple_or_complex_Type, qN}, s) do
    case resolve({:simpleType, qN}, s) do
      res = {r_schema_simple_type(), _S1} ->
        res

      {[], _S} ->
        case load_object({:complexType, qN}, s) do
          {[], _} ->
            :no_debug
            {[], s}

          t ->
            t
        end

      t ->
        t
    end
  end

  defp resolve({:complexType, {:anyType, _, _}}, s) do
    {{:anyType, []}, s}
  end

  defp resolve({:simpleType, {:anyType, _, _}}, s) do
    {{:anyType, []}, s}
  end

  defp resolve(sT = {:simpleType, nameNS = {_, _, _}}, s) do
    case load_object(sT, s) do
      {[], _S} ->
        case is_builtin_simple_type(nameNS) do
          true ->
            {sT, s}

          _ ->
            {[], s}
        end

      obj ->
        obj
    end
  end

  defp resolve({:substitutionGroup, qName}, s) do
    case load_object({:element, qName}, s) do
      ret = {[], _S} ->
        ret

      {r_schema_element(type: [type]), s2} ->
        case type do
          {:simple_or_complex_Type, _} ->
            resolve(type, s2)

          _ ->
            {type, s2}
        end

      {r_schema_element(type: type), s2} ->
        {type, s2}
    end
  end

  defp resolve({:extension, {baseType, cM}}, s) do
    case is_builtin_simple_type(baseType) do
      true ->
        {{:simpleType, baseType}, s}

      _ ->
        case resolve(
               {:simple_or_complex_Type, baseType},
               s
             ) do
          {sT = r_schema_simple_type(), _} ->
            {sT, s}

          {cT = r_schema_complex_type(content: c), _} ->
            {newC, s2} = extend_type(c, cM, s)
            {r_schema_complex_type(cT, content: newC), s2}

          t ->
            t
        end
    end
  end

  defp resolve({:restriction, {baseType, cM}}, s) do
    case is_builtin_simple_type(baseType) do
      true ->
        {{:simpleType, baseType}, s}

      _ ->
        case resolve(
               {:simple_or_complex_Type, baseType},
               s
             ) do
          {sT = r_schema_simple_type(content: c), _} ->
            {newContent, s2} = restrict_simple_type(c, cM, baseType, s)
            {r_schema_simple_type(sT, content: newContent), s2}

          {cT = r_schema_complex_type(content: c), _} ->
            {newContent, s2} = restrict_type(c, cM, baseType, s)
            {r_schema_complex_type(cT, content: newContent), s2}

          t ->
            t
        end
    end
  end

  defp resolve(:optional_text, s) do
    {:optional_text, s}
  end

  defp resolve(e, s) do
    :no_debug
    load_object(e, s)
  end

  defp explicit_type(xSDType, instanceEl = r_xmlElement(namespace: nS, attributes: atts), s) do
    case get_instance_type(nS, atts) do
      false ->
        resolve(xSDType, s)

      {:ok, name} ->
        qName = mk_EII_QName(name, instanceEl, r_xsd_state(s, scope: []))

        {xsiType, s2} =
          resolve(
            {:simple_or_complex_Type, qName},
            s
          )

        {_Blocks, s3} = legal_substitution(instanceEl, xsiType, s2)
        {xsiType, s3}
    end
  end

  defp get_instance_type(r_xmlNamespace(nodes: nodes), atts) do
    case keyNsearch(:"http://www.w3.org/2001/XMLSchema-instance", 2, nodes, []) do
      {prefix, _} ->
        typeAtt = :erlang.list_to_atom(prefix ++ ':type')

        case keyNsearch(typeAtt, r_xmlAttribute(:name), atts, []) do
          r_xmlAttribute(value: value) ->
            {:ok, value}

          _ ->
            false
        end

      _ ->
        false
    end
  end

  defp merge_derived_types(type1, type2, mode, s) do
    merge_derived_types(type1, type2, [], mode, s)
  end

  defp merge_derived_types(type, type, _Blocks, _Mode, s) do
    {type, s}
  end

  defp merge_derived_types(xSDType, instType, blocks, mode, s) do
    case (try do
            merge_derived_types2(xSDType, instType, blocks, mode, s)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, reason} ->
        {instType,
         acc_errs(
           s,
           {[], :xmerl_xsd, {:internal_error, :merge_derived_types, reason}}
         )}

      {:error, s2} ->
        {instType, s2}

      {mergedType, s2} ->
        _ = save_merged_type(mergedType, s2)
        {mergedType, s2}
    end
  end

  defp merge_derived_types2(
         xSDType = r_schema_complex_type(),
         instType = r_schema_complex_type(),
         blocks,
         mode,
         s
       ) do
    case r_schema_complex_type(instType, :content) do
      [{:extension, {baseTypeName, cM}}] ->
        {extendedAtts, s2} =
          extend_attributes(
            r_schema_complex_type(xSDType, :attributes),
            r_schema_complex_type(instType, :attributes),
            baseTypeName,
            cM,
            mode,
            allowed_derivation(:extension, blocks, s)
          )

        case compare_base_types(baseTypeName, xSDType, s2) do
          :ok ->
            {newContent, s3} = extend_type(r_schema_complex_type(xSDType, :content), cM, s2)

            {r_schema_complex_type(instType,
               attributes: extendedAtts,
               content: newContent
             ), s3}

          err ->
            {:error, acc_errs(s2, err)}
        end

      [{:restriction, {baseTypeName, cM}}] ->
        {restrictedAtts, s2} =
          restrict_attributes(
            r_schema_complex_type(xSDType, :attributes),
            r_schema_complex_type(instType, :attributes),
            allowed_derivation(:restriction, blocks, s)
          )

        case compare_base_types(baseTypeName, xSDType, s2) do
          :ok ->
            {newContent, s3} =
              case r_schema_complex_type(instType, :complexity) do
                :simple ->
                  restrict_simple_type(
                    r_schema_complex_type(xSDType, :content),
                    cM,
                    baseTypeName,
                    s2
                  )

                _ ->
                  restrict_type(r_schema_complex_type(xSDType, :content), cM, baseTypeName, s2)
              end

            {r_schema_complex_type(instType,
               attributes: restrictedAtts,
               content: newContent
             ), s3}

          err ->
            {:error, acc_errs(s, err)}
        end

      other ->
        {:error,
         acc_errs(
           s,
           {[], :xmerl_xsd, {:unexpected_type, other}}
         )}
    end
  end

  defp merge_derived_types2(
         xSDType = r_schema_simple_type(),
         instType = r_schema_simple_type(),
         blocks,
         _Mode,
         s
       ) do
    case r_schema_simple_type(instType, :content) do
      [{:restriction, {baseTypeName, cM}}] ->
        case compare_base_types(baseTypeName, xSDType, s) do
          :ok ->
            {newContent, s2} =
              restrict_simple_type(r_schema_simple_type(xSDType, :content), cM, baseTypeName, s)

            {r_schema_simple_type(instType, content: newContent),
             allowed_derivation(:restriction, blocks, s2)}

          err ->
            {:error, allowed_derivation(:restriction, blocks, acc_errs(s, err))}
        end

      other ->
        {:error, acc_errs(s, {:unexpected_type, other})}
    end
  end

  defp merge_derived_types2(
         xSDType = r_schema_simple_type(content: xSDContent),
         instType = r_schema_complex_type(),
         blocks,
         _Mode,
         s
       ) do
    case r_schema_complex_type(instType, :content) do
      [{:extension, {baseTypeName, cM}}] ->
        case compare_base_types(baseTypeName, xSDType, s) do
          :ok ->
            {newContent, s2} =
              cond do
                cM == [] ->
                  {xSDContent, s}

                true ->
                  extend_type(xSDContent, cM, s)
              end

            {r_schema_complex_type(instType, content: newContent),
             allowed_derivation(:extension, blocks, s2)}

          err ->
            {:error, allowed_derivation(:extension, blocks, acc_errs(s, err))}
        end

      [{:restriction, {baseTypeName, _CM}}]
      when r_schema_complex_type(instType, :complexity) == :simple ->
        case compare_base_types(baseTypeName, xSDType, s) do
          :ok ->
            {instType, allowed_derivation(:restriction, blocks, s)}

          err ->
            {:error, allowed_derivation(:extension, blocks, acc_errs(s, err))}
        end

      other ->
        {:error,
         acc_errs(
           s,
           {[], :xmerl_xsd, {:unexpected_type, other}}
         )}
    end
  end

  defp merge_derived_types2(
         _XSDType = {:simpleType, builtInType},
         instType = r_schema_complex_type(content: content),
         blocks,
         _Mode,
         s
       ) do
    case content do
      [{:extension, {^builtInType, cM}}] ->
        {newContent, s2} = extend_type([], cM, s)

        {r_schema_complex_type(instType,
           base_type: builtInType,
           content: newContent
         ), allowed_derivation(:extension, blocks, s2)}

      [{:restriction, {^builtInType, cM}}] ->
        {newContent, s2} = restrict_simple_type([], cM, builtInType, s)

        {r_schema_complex_type(instType,
           base_type: builtInType,
           content: newContent
         ), allowed_derivation(:restriction, blocks, s2)}

      other ->
        {:error,
         acc_errs(
           s,
           {[], :xmerl_xsd, {:unexpected_content, other, instType}}
         )}
    end
  end

  defp merge_derived_types2(_XSDType = {:anyType, _}, instType, blocks, _Mode, s) do
    case type_content(instType) do
      [{:restriction, {_BaseTypeName, cM}}] ->
        {set_type_content(instType, cM), allowed_derivation(:restriction, blocks, s)}

      other ->
        {:error,
         acc_errs(
           s,
           {[], :xmerl_xsd, {:unexpected_content, other, instType}}
         )}
    end
  end

  defp merge_derived_types2(
         {:simpleType, builtInType},
         instType = r_schema_simple_type(content: content),
         blocks,
         _Mode,
         s
       ) do
    case content do
      [{:restriction, {^builtInType, cM}}] ->
        {r_schema_simple_type(instType, base_type: builtInType, content: cM),
         allowed_derivation(:restriction, blocks, s)}

      other ->
        {:error,
         acc_errs(
           s,
           {[], :xmerl_xsd, {:unexpected_content, other, instType}}
         )}
    end
  end

  defp merge_derived_types2(xSDType, instType, blocks, mode, s) do
    case {variety_type(xSDType, s), variety_type(instType, s)} do
      {^xSDType, ^instType} ->
        {:error,
         acc_errs(
           s,
           {[], :xmerl_xsd, {:unexpected_type, xSDType, instType}}
         )}

      {_XSDType2, instType2} ->
        case allowed_derivation(:substitution, blocks, s) do
          ^s ->
            merge_derived_types2(xSDType, instType2, blocks, mode, s)

          s2 ->
            {:error, s2}
        end
    end
  end

  defp variety_type(
         r_schema_simple_type(variety: :list, content: [{:list, [type]}]),
         s
       ) do
    {varietyType, _} = resolve(type, s)
    varietyType
  end

  defp variety_type(
         r_schema_simple_type(variety: :union, content: [{:union, types}]),
         s
       ) do
    for {t, _} <-
          (for varietyType <- types do
             resolve(varietyType, s)
           end) do
      t
    end
  end

  defp variety_type(type, _S) do
    type
  end

  defp allowed_derivation(_Derivation, _Blocks, s) do
    s
  end

  defp legal_substitution(el = r_xmlElement(name: elName), xsiType, s) do
    qName = mk_EII_QName(elName, el, s)
    {headElement, _} = load_object({:element, qName}, s)
    legal_substitution2(headElement, xsiType, s)
  end

  defp legal_substitution2(r_schema_element(type: type, block: bl), xsiType, s) do
    {headType, _} = resolve(type, s)
    block = blocking(bl, r_xsd_state(s, :blockDefault))
    s2 = derived_or_equal_types(xsiType, headType, :instance, block, s)
    {block, s2}
  end

  defp compare_base_types(qName, r_schema_complex_type(name: qName), _S) do
    :ok
  end

  defp compare_base_types(qName1, r_schema_complex_type(name: qName2), _S) do
    {[], :xmerl_xsd, {:names_not_equal, qName1, qName2}}
  end

  defp compare_base_types(qName, r_schema_simple_type(name: qName), _S) do
    :ok
  end

  defp compare_base_types(qName1, r_schema_simple_type(name: qName2), _S) do
    {[], :xmerl_xsd, {:names_not_equal, qName1, qName2}}
  end

  defp extend_type(base, extension, s) do
    extend_type(base, extension, [], s)
  end

  defp extend_type([], [], acc, s) do
    {reverse(acc), s}
  end

  defp extend_type([baseCM | baseRest], ext = [{seqCho, {extension, occ}} | extRest], acc, s)
       when seqCho == :sequence or seqCho == :choice do
    case baseCM do
      {^seqCho, {bC, _Occ}} ->
        extend_type(baseRest, extRest, [{seqCho, {bC ++ extension, occ}} | acc], s)

      g = {:group, {_Ref, _Occ}} ->
        {resG, s2} = resolve(g, s)

        case resG do
          r_schema_group(content: gC) ->
            case keysearch(seqCho, 1, gC) do
              {:value, sCC} ->
                extend_type([sCC | baseRest], ext, acc, s)

              _ ->
                s3 =
                  acc_errs(
                    s2,
                    {[], :xmerl_xsd, {:illegal_content_in_extension, ext}}
                  )

                {reverse(acc), s3}
            end

          _ ->
            s3 =
              acc_errs(
                s2,
                {[], :xmerl_xsd, {:illegal_content_in_extension, resG}}
              )

            {reverse(acc), s3}
        end

      _ ->
        extend_type([baseCM | baseRest], extRest, [{seqCho, {extension, occ}} | acc], s)
    end
  end

  defp extend_type(baseCM, extCM, acc, s)
       when is_list(baseCM) and
              is_list(extCM) do
    extend_type([], [], reverse(extCM) ++ reverse(baseCM) ++ acc, s)
  end

  defp restrict_type(content, cM, baseTypeName, s) do
    restrict_type(content, cM, baseTypeName, [], s)
  end

  defp restrict_type([], [], _TypeName, acc, s) do
    {reverse(acc), s}
  end

  defp restrict_type([{:restriction, {_Type, cM1}}], [], _TypeName, acc, s) do
    {cM1 ++ reverse(acc), s}
  end

  defp restrict_type([{:extension, {_Type, cM1}}], [], _TypeName, acc, s) do
    {cM1 ++ reverse(acc), s}
  end

  defp restrict_type(baseRest, [sT = {:simpleType, _Name} | restrRest], typeName, acc, s) do
    restrict_type(baseRest, restrRest, typeName, [sT | acc], s)
  end

  defp restrict_type([baseCM | baseRest], [{seqCho, {cM, occ}} | restrRest], typeName, acc, s)
       when seqCho == :sequence or seqCho == :choice do
    case baseCM do
      {^seqCho, {bCM, _}} ->
        case check_element_presence(cM, bCM) do
          {:error, reason} ->
            {reverse(acc), acc_errs(s, reason)}

          :ok ->
            restrict_type(baseRest, restrRest, typeName, [{seqCho, {cM, occ}} | acc], s)
        end

      other ->
        {reverse(acc),
         acc_errs(
           s,
           {[], :xmerl_xsd, {seqCho, :expected, other, :found}}
         )}
    end
  end

  defp restrict_type(baseRest, [facet = {f, _Val} | restrRest], typeName, acc, s) do
    case is_facet(f) do
      true ->
        restrict_type(baseRest, restrRest, typeName, [facet | acc], s)

      _ ->
        {reverse(acc),
         acc_errs(
           s,
           {[], :xmerl_xsd, {:does_not_support, facet, :in_restriction}}
         )}
    end
  end

  defp restrict_simple_type([{:restriction, {_Type, baseCM}}], restrCM, _TypeName, s) do
    restrict_simple_type(baseCM, restrCM, _TypeName, s)
  end

  defp restrict_simple_type(cM = [{:extension, {_Type, _BaseCM}}], _RestrCM, typeName, s) do
    {[],
     acc_errs(
       s,
       {[], :xmerl_xsd, {:illegal_content_simple_type, cM, typeName}}
     )}
  end

  defp restrict_simple_type(baseCM, restrCM, typeName, s) do
    {acc, s2} =
      case baseCM do
        [] ->
          {[], s}

        _ ->
          restrict_simple_type([], baseCM, typeName, s)
      end

    fun = fn
      x = {:simpleType, _}, {acc_in, s_in} ->
        {[x | acc_in], s_in}

      x = {lU, _}, {acc_in, s_in}
      when lU == :list or
             lU == :union ->
        {[x | acc_in], s_in}

      x = {f, _}, {acc_in, s_in} ->
        case is_facet(f) do
          true ->
            {[x | acc_in], s_in}

          _ ->
            {acc_in,
             acc_errs(
               s_in,
               {[], :xmerl_xsd, {:illegal_in_restriction_of_simpleType, x}}
             )}
        end

      x, {acc_in, s_in} ->
        {acc_in,
         acc_errs(
           s_in,
           {[], :xmerl_xsd, {:illegal_in_restriction_of_simpleType, x}}
         )}
    end

    foldl(fun, {acc, s2}, restrCM)
  end

  defp check_element_presence([], _BCM) do
    :ok
  end

  defp check_element_presence([{:element, {name, _}} | cM], bCM) do
    case check_element_presence2(name, bCM) do
      {:ok, bCM2} ->
        check_element_presence(cM, bCM2)

      _ ->
        {:error, {[], :xmerl_xsd, {:element, name, :not_present_in_restriction}}}
    end
  end

  defp check_element_presence([_C | cM], bCM) do
    check_element_presence(cM, bCM)
  end

  defp check_element_presence2(name, bCM) do
    check_element_presence2(name, bCM, [])
  end

  defp check_element_presence2(
         {localName, _, nS},
         [{:element, {{localName, _, nS}, _}} | bCM],
         acc
       ) do
    {:ok, reverse(acc) ++ bCM}
  end

  defp check_element_presence2(name, [e | bCM], acc) do
    check_element_presence2(name, bCM, [e | acc])
  end

  defp check_element_presence2(_Name, [], _Acc) do
    :error
  end

  defp extend_attributes(baseAtts, [eA = {:attribute, name} | extAtts], baseTypeName, cM, mode, s) do
    newAtts = key_replace_or_insert(name, 2, baseAtts, eA)
    extend_attributes(newAtts, extAtts, baseTypeName, cM, mode, s)
  end

  defp extend_attributes(
         baseAtts,
         [localWC = {:anyAttribute, _NS_PC} | extAtts],
         baseTypeName,
         cM,
         :deduce,
         s
       ) do
    {completeWC, s2} = complete_wildcard(localWC, cM, s)
    baseWC = base_wildcard(baseAtts)

    {newWC, s4} =
      case baseWC do
        [] ->
          {completeWC, s2}

        _ ->
          cond do
            completeWC == localWC ->
              {baseWC, s2}

            true ->
              {nS, s3} =
                attribute_wildcard_union(
                  wc_ns(completeWC),
                  wc_ns(baseWC),
                  s2
                )

              pC = wc_pc(completeWC)
              {[{:anyAttribute, {nS, pC}}], s3}
          end
      end

    newBaseAtts = keyreplace(:anyAttribute, 1, baseAtts, newWC)
    extend_attributes(newBaseAtts, extAtts, baseTypeName, cM, :deduce, s4)
  end

  defp extend_attributes(atts, [], _, _, _Mode, s) do
    {reverse(atts), s}
  end

  defp restrict_attributes(baseAtts, [rA | rAtts], s) do
    {newAtts, s2} = restrict_attribute_replace(baseAtts, rA, s)
    restrict_attributes(newAtts, rAtts, s2)
  end

  defp restrict_attributes(atts, [], s) do
    {reverse(atts), s}
  end

  defp restrict_attribute_replace(baseAtts, eA = {:attribute, name}, s) do
    {keyreplace(name, 2, baseAtts, eA), s}
  end

  defp restrict_attribute_replace(baseAtts, eA = {:anyAttribute, {nS, _}}, s) do
    case key1search(:anyAttribute, baseAtts, false) do
      false ->
        {baseAtts, acc_errs(s, {:invalid_derivation, eA, baseAtts})}

      {_, {baseNS, _}} ->
        s2 = wildcard_subset(baseNS, nS, s)
        {keyreplace(:anyAttribute, 1, baseAtts, eA), s2}
    end
  end

  defp wildcard_subset([:"##any"], _NS, s) do
    s
  end

  defp wildcard_subset([{:not, nS}], [{:not, nS}], s) do
    s
  end

  defp wildcard_subset(_, [], s) do
    s
  end

  defp wildcard_subset(baseNS, nS, s)
       when is_list(baseNS) and
              is_list(nS) do
    case (for x <- nS, member(x, baseNS) do
            x
          end) do
      ^nS ->
        s

      _ ->
        acc_errs(
          s,
          {[], :xmerl_xsd, {:wildcard_namespace, nS, :not_subset_of_base_namespace, baseNS}}
        )
    end
  end

  defp wildcard_subset(baseNS = [{:not, bNS}], nS, s)
       when is_list(nS) do
    case (for x <- bNS, member(x, nS) do
            x
          end) do
      [] ->
        s

      _ ->
        acc_errs(
          s,
          {[], :xmerl_xsd, {:wildcard_namespace, nS, :not_subset_of_base_namespace, baseNS}}
        )
    end
  end

  defp wildcard_subset(baseNS, nS, s) do
    acc_errs(
      s,
      {[], :xmerl_xsd, {:wildcard_namespace, nS, :not_subset_of_base_namespace, baseNS}}
    )
  end

  defp base_wildcard(baseAtts) do
    key1search(:anyAttribute, baseAtts, [])
  end

  defp complete_wildcard(localWC, cM, s) do
    case keysearch(:attributeGroup, 1, cM) do
      {:value, attG = {_, _Name}} ->
        case resolve(attG, s) do
          {r_schema_attribute_group(content: atts), _S} ->
            case keysearch(:anyAttribute, 1, atts) do
              {:value, aA} ->
                {pC, s2} = attribute_wildcard_intersection(wc_ns(localWC), wc_ns(aA), s)
                {{:anyAttribute, {wc_pc(localWC), pC}}, s2}

              _ ->
                {localWC, s}
            end

          _ ->
            {localWC, s}
        end

      _ ->
        {localWC, s}
    end
  end

  defp wc_ns({:anyAttribute, {nS, _}}) do
    nS
  end

  defp wc_ns(_) do
    []
  end

  defp wc_pc({:anyAttribute, {_, pC}}) do
    pC
  end

  defp wc_pc(_) do
    :strict
  end

  defp attribute_wildcard_union(nS, nS, s) do
    {nS, s}
  end

  defp attribute_wildcard_union(nS1, nS2, s) when nS1 == [:"##any"] or nS2 == [:"##any"] do
    {[:"##any"], s}
  end

  defp attribute_wildcard_union(nS1, nS2, s) do
    case {keysearch(:not, 1, nS1), keysearch(:not, 1, nS2)} do
      {false, false} ->
        {nS1 ++
           for x <- nS2, member(x, nS1) == false do
             x
           end, s}

      {{:value, {_, set1}}, {:value, {_, set2}}} ->
        case {:lists.sort(set1), :lists.sort(set2)} do
          {l, l} ->
            {[{:not, l}], s}

          _ ->
            {[{:not, [:absent]}], s}
        end

      _ ->
        case toggle_ns(nS1, nS2) do
          {_O1 = [:absent], nS3} ->
            case member(:absent, nS3) do
              true ->
                {[:"##any"], s}

              _ ->
                {[{:not, [:absent]}], s}
            end

          {o1 = [o1Name], nS4} ->
            case member(o1Name, nS4) do
              true ->
                case member(:absent, nS4) do
                  true ->
                    {[:"##any"], s}

                  _ ->
                    {[{:not, [:absent]}], s}
                end

              _ ->
                case member(:absent, nS4) do
                  true ->
                    err = {[], :xmerl_xsd, {:wildcard_namespace_union_not_expressible, nS1, nS2}}
                    {[], acc_errs(s, err)}

                  _ ->
                    {[{:not, o1}], s}
                end
            end
        end
    end
  end

  defp attribute_wildcard_intersection(o1, o1, s) do
    {o1, s}
  end

  defp attribute_wildcard_intersection([:"##any"], o2, s) do
    {o2, s}
  end

  defp attribute_wildcard_intersection(o1, [:"##any"], s) do
    {o1, s}
  end

  defp attribute_wildcard_intersection([{:not, [:absent]}], o2 = [{:not, _}], s) do
    {o2, s}
  end

  defp attribute_wildcard_intersection(o1 = [{:not, _}], [{:not, [:absent]}], s) do
    {o1, s}
  end

  defp attribute_wildcard_intersection([{:not, nS1}], [{:not, nS2}], s) do
    case (for x <- nS1, member(x, nS2) do
            x
          end) do
      [] ->
        {[],
         acc_errs(
           s,
           {[], :xmerl_xsd, {:wildcard_namespace_intersection_not_expressible, nS1, nS2}}
         )}

      nS3 ->
        {[{:not, nS3}], s}
    end
  end

  defp attribute_wildcard_intersection([{:not, nS}], o2, s) do
    {:lists.delete(
       :absent,
       for x <- o2, member(x, nS) == false do
         x
       end
     ), s}
  end

  defp attribute_wildcard_intersection(o1, [{:not, nS}], s) do
    {:lists.delete(
       :absent,
       for x <- o1, member(x, nS) == false do
         x
       end
     ), s}
  end

  defp attribute_wildcard_intersection(o1, o2, s) do
    case (for x <- o1, member(x, o2) do
            x
          end) do
      [] ->
        {[:absent], s}

      l ->
        {l, s}
    end
  end

  defp toggle_ns(nS1, nS2 = [{:not, _}]) do
    {nS2, nS1}
  end

  defp toggle_ns(nS1, nS2) do
    {nS1, nS2}
  end

  defp deduce_derived_types([dT | dTs], s) do
    deduce_derived_types(
      dTs,
      deduce_derived_type(dT, s, [])
    )
  end

  defp deduce_derived_types([], s) do
    s
  end

  defp deduce_derived_type(dT = {_Kind, tName}, s, refChain) do
    case keymember(tName, 2, refChain) do
      true ->
        acc_errs(
          s,
          {[], :xmerl_xsd, {:circular_reference_of_type, tName}}
        )

      _ ->
        deduce_derived_type2(dT, s, [dT | refChain])
    end
  end

  defp deduce_derived_type2(dT, s, refChain) do
    {derivedType, _} = resolve(dT, s)

    case is_unmerged_type(derivedType) do
      true ->
        baseTypeRef = get_base_type(derivedType)

        {baseType, _} =
          resolve(
            {:simple_or_complex_Type, baseTypeRef},
            s
          )

        baseTypeKind =
          (fn
             r_schema_complex_type() ->
               :complexType

             _ ->
               :simpleType
           end).(baseType)

        case is_unmerged_type(baseType) do
          true ->
            s2 = deduce_derived_type({baseTypeKind, baseTypeRef}, s, refChain)

            case r_xsd_state(s2, :errors) == r_xsd_state(s, :errors) do
              true ->
                deduce_derived_type2(dT, s2, refChain)

              _ ->
                s2
            end

          _ ->
            {_, s2} = merge_derived_types(baseType, derivedType, :deduce, s)
            s2
        end

      _ ->
        s
    end
  end

  defp is_unmerged_type(type) do
    case type_content(type) do
      [{:restriction, _}] ->
        true

      [{:extension, _}] ->
        true

      _ ->
        false
    end
  end

  defp type_content(r_schema_simple_type(content: c)) do
    c
  end

  defp type_content(r_schema_complex_type(content: c)) do
    c
  end

  defp type_content(_) do
    []
  end

  defp set_type_content(type = r_schema_simple_type(), cM) do
    r_schema_simple_type(type, content: cM)
  end

  defp set_type_content(type = r_schema_complex_type(), cM) do
    r_schema_complex_type(type, content: cM)
  end

  defp get_base_type(r_schema_simple_type(base_type: bT)) do
    bT
  end

  defp get_base_type(r_schema_complex_type(base_type: bT)) do
    bT
  end

  defp in_scope({local, _Scope, _NS}, s) do
    in_scope(local, s)
  end

  defp in_scope(name, s = r_xsd_state(scope: scope)) when is_atom(name) do
    r_xsd_state(s, scope: [name | scope])
  end

  defp in_scope(name, s = r_xsd_state(scope: scope)) when is_list(name) do
    r_xsd_state(s, scope: [atom_if_shortasciilist(name) | scope])
  end

  defp out_scope({local, _, _}, s) do
    out_scope(atom_if_shortasciilist(local), s)
  end

  defp out_scope(name, s = r_xsd_state(scope: [name | rest])) do
    r_xsd_state(s, scope: rest)
  end

  defp out_scope(_Name, s) do
    s
  end

  defp name_scope({:_xmerl_no_name_, scope, _NS}, s) do
    r_xsd_state(s, scope: scope)
  end

  defp name_scope({name, scope, _NS}, s) do
    r_xsd_state(s, scope: [name | scope])
  end

  defp reset_scope(s) do
    r_xsd_state(s, scope: [])
  end

  defp set_scope(scope, s) when is_list(scope) do
    r_xsd_state(s, scope: scope)
  end

  defp set_scope(_, s) do
    s
  end

  defp is_global_env([_Env]) do
    true
  end

  defp is_global_env(_) do
    false
  end

  defp kind(r_xmlElement(name: name), s) do
    localName = local_name(name)
    is_a(localName, s)
  end

  defp kind(r_xmlElement(name: name)) do
    localName = local_name(name)
    :erlang.element(1, is_a(localName, :dummy))
  end

  defp is_a(:element, s) do
    {:element, s}
  end

  defp is_a(:annotation, s) do
    {:annotation, s}
  end

  defp is_a(:simpleType, s) do
    {:simpleType, s}
  end

  defp is_a(:complexType, s) do
    {:complexType, s}
  end

  defp is_a(:simpleContent, s) do
    {:simpleContent, s}
  end

  defp is_a(:complexContent, s) do
    {:complexContent, s}
  end

  defp is_a(:include, s) do
    {:include, s}
  end

  defp is_a(:import, s) do
    {:import, s}
  end

  defp is_a(:redefine, s) do
    {:redefine, s}
  end

  defp is_a(:unique, s) do
    {:unique, s}
  end

  defp is_a(:key, s) do
    {:key, s}
  end

  defp is_a(:keyref, s) do
    {:keyref, s}
  end

  defp is_a(:attribute, s) do
    {:attribute, s}
  end

  defp is_a(:attributeGroup, s) do
    {:attributeGroup, s}
  end

  defp is_a(:group, s) do
    {:group, s}
  end

  defp is_a(:all, s) do
    {:all, s}
  end

  defp is_a(:sequence, s) do
    {:sequence, s}
  end

  defp is_a(:choice, s) do
    {:choice, s}
  end

  defp is_a(:any, s) do
    {:any, s}
  end

  defp is_a(:anyAttribute, s) do
    {:anyAttribute, s}
  end

  defp is_a(:selector, s) do
    {:selector, s}
  end

  defp is_a(:field, s) do
    {:field, s}
  end

  defp is_a(:notation, s) do
    {:notation, s}
  end

  defp is_a(:appinfo, s) do
    {:appinfo, s}
  end

  defp is_a(:documentation, s) do
    {:documentation, s}
  end

  defp is_a(:restriction, s) do
    {:restriction, s}
  end

  defp is_a(:extension, s) do
    {:extension, s}
  end

  defp is_a(:list, s) do
    {:list, s}
  end

  defp is_a(:union, s) do
    {:union, s}
  end

  defp is_a(:schema, s) do
    {:schema, s}
  end

  defp is_a(:minExclusive, s) do
    {:minExclusive, s}
  end

  defp is_a(:minInclusive, s) do
    {:minInclusive, s}
  end

  defp is_a(:maxExclusive, s) do
    {:maxExclusive, s}
  end

  defp is_a(:maxInclusive, s) do
    {:maxInclusive, s}
  end

  defp is_a(:totalDigits, s) do
    {:totalDigits, s}
  end

  defp is_a(:fractionDigits, s) do
    {:fractionDigits, s}
  end

  defp is_a(:length, s) do
    {:length, s}
  end

  defp is_a(:minLength, s) do
    {:minLength, s}
  end

  defp is_a(:maxLength, s) do
    {:maxLength, s}
  end

  defp is_a(:enumeration, s) do
    {:enumeration, s}
  end

  defp is_a(:whiteSpace, s) do
    {:whiteSpace, s}
  end

  defp is_a(:pattern, s) do
    {:pattern, s}
  end

  defp is_a(name, s) when elem(s, 0) === :xsd_state do
    {name, acc_errs(s, {[], :xmerl_xsd, {:unknown_content, name}})}
  end

  defp is_a(name, _) do
    exit({:error, {:internal_error, :not_implemented, name}})
  end

  defp wildcard_namespace(e, s) do
    attVal = get_attribute_value(:namespace, e, '##any')
    listOfVals = namestring2namelist(attVal)

    pred = fn
      :"##other" ->
        case r_xsd_state(s, :targetNamespace) do
          :undefined ->
            {:not, [:absent]}

          tN ->
            {:not, tN}
        end

      :"##targetNamespace" ->
        case r_xsd_state(s, :targetNamespace) do
          :undefined ->
            :absent

          tN ->
            tN
        end

      :"##local" ->
        :absent

      x ->
        x
    end

    for x <- map(pred, listOfVals), x !== [] do
      x
    end
  end

  defp processor_contents(any) do
    case get_attribute_value(:processContents, any, :strict) do
      v when is_list(v) ->
        :erlang.list_to_atom(v)

      a ->
        a
    end
  end

  defp base_type(e) do
    get_attribute_value(:base, e, [])
  end

  defp base_type_type(env) do
    case member(:simpleType, env) do
      true ->
        :simpleType

      _ ->
        :simple_or_complex_Type
    end
  end

  defp attribute_ref(a) do
    get_attribute_value(:ref, a, [])
  end

  defp particle_ref(el) do
    get_attribute_value(:ref, el, [])
  end

  defp attributeGroup_ref(el) do
    get_attribute_value(:ref, el, [])
  end

  defp get_value(el) do
    get_attribute_value(:value, el, :undefined)
  end

  defp get_attribute_value(key, r_xmlElement(attributes: atts), default) do
    case keyNsearch(key, r_xmlAttribute(:name), atts, default) do
      r_xmlAttribute(value: v) ->
        v

      _ ->
        default
    end
  end

  defp qualify_NCName(e = r_xmlElement(), s) do
    case get_local_name(e) do
      [] ->
        :no_name

      localName ->
        namespace =
          case r_xsd_state(s, :targetNamespace) do
            :undefined ->
              []

            tNS ->
              tNS
          end

        {atom_if_shortasciilist(localName), r_xsd_state(s, :scope), namespace}
    end
  end

  defp get_local_name(r_xmlElement(attributes: atts)) do
    case keyNsearch(:name, r_xmlAttribute(:name), atts, []) do
      r_xmlAttribute(value: v) ->
        v

      default ->
        default
    end
  end

  defp local_name(name) when is_atom(name) do
    local_name(:erlang.atom_to_list(name))
  end

  defp local_name(name) when is_list(name) do
    case splitwith(
           fn
             ?: ->
               false

             _ ->
               true
           end,
           name
         ) do
      {_, ':' ++ localName} ->
        :erlang.list_to_atom(localName)

      _ ->
        :erlang.list_to_atom(name)
    end
  end

  defp namestring2namelist(str) do
    split_by_whitespace(str, [])
  end

  defp split_by_whitespace(str, acc)
       when is_list(str) and
              length(str) > 0 do
    f = fn
      ?\s ->
        false

      _ ->
        true
    end

    {str1, rest} = splitwith(f, str)

    split_by_whitespace(
      :string.strip(rest),
      [:erlang.list_to_atom(str1) | acc]
    )
  end

  defp split_by_whitespace(_, acc) do
    reverse(acc)
  end

  defp get_QName(name, nS, s) when is_atom(name) do
    get_QName(:erlang.atom_to_list(name), nS, s)
  end

  defp get_QName(name, nS, r_xsd_state(scope: scope)) do
    qualified_name(name, nS, r_xmlNamespace(nS, :default), scope)
  end

  defp qualified_name(name, nS, default, scope) do
    case splitwith(
           fn
             ?: ->
               false

             _ ->
               true
           end,
           name
         ) do
      {globalName, ':' ++ localName} ->
        {atom_if_shortasciilist(localName), scope, namespace(globalName, nS, default)}

      _ ->
        {atom_if_shortasciilist(name), scope, default}
    end
  end

  defp atom_if_shortasciilist(n) when is_list(n) do
    case (try do
            :erlang.list_to_atom(n)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _Reason} ->
        n

      aN ->
        aN
    end
  end

  defp atom_if_shortasciilist(n) do
    n
  end

  defp namespace('xml', _, _) do
    :"http://www.w3.org/XML/1998/namespace"
  end

  defp namespace(prefix, nS, default) do
    case key1search(prefix, r_xmlNamespace(nS, :nodes), default) do
      {^prefix, namespace} ->
        namespace

      namespace ->
        namespace
    end
  end

  defp mk_EII_QName(name, r_xmlElement(name: me, namespace: nS, parents: p), s)
       when is_list(name) do
    mk_EII_QName(:erlang.list_to_atom(name), r_xmlElement(name: me, namespace: nS, parents: p), s)
  end

  defp mk_EII_QName(name, r_xmlElement(name: me, namespace: nS, parents: p), s) do
    scope = r_xsd_state(s, :scope)
    nameStr = :erlang.atom_to_list(name)

    case :string.tokens(nameStr, ':') do
      ['xmlns', prefixDef] ->
        {:xmlns, scope, namespace(prefixDef, nS, [])}

      [prefix, localName] ->
        {:erlang.list_to_atom(localName), scope, namespace(prefix, nS, [])}

      [_LocalName] ->
        {name, scope, mk_EII_namespace([{me, 0} | p], nS, s)}
    end
  end

  defp mk_EII_namespace([], r_xmlNamespace(default: defaultNS), _S) do
    defaultNS
  end

  defp mk_EII_namespace([{pName, _} | grandPs], nS, s) do
    nameStr = :erlang.atom_to_list(pName)

    case :string.tokens(nameStr, ':') do
      [prefix, _LocalName] ->
        namespace(prefix, nS, [])

      [_LocalName] ->
        mk_EII_namespace(grandPs, nS, s)
    end
  end

  defp mk_EII_namespace(_, nS, _S) do
    r_xmlNamespace(nS, :default)
  end

  defp mk_EII_Att_QName(attName, xMLEl, s) when is_list(attName) do
    mk_EII_Att_QName(:erlang.list_to_atom(attName), xMLEl, s)
  end

  defp mk_EII_Att_QName(attName, xMLEl, s) do
    nameStr = :erlang.atom_to_list(attName)
    {member(?:, nameStr), mk_EII_QName(attName, xMLEl, s)}
  end

  defp create_tables(s = r_xsd_state(table: :undefined)) do
    tid = :ets.new(:xmerl_schema_tab, [])
    r_xsd_state(s, table: tid)
  end

  defp create_tables(s) do
    s
  end

  defp delete_table(r_xsd_state(table: tab)) do
    try do
      :ets.delete(tab)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  def print_table(r_xsd_state(table: tab)) do
    case (try do
            :ets.tab2list(tab)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      res when is_list(res) ->
        res

      {:EXIT, reason} ->
        {:error, {:xmerl_xsd, [], reason}}
    end
  end

  def print_table(_) do
    :ok
  end

  defp save_object({kind, obj}, s = r_xsd_state(redefine: true))
       when kind == :simpleType or kind == :complexType do
    save_in_table({kind, :redefine, object_name(obj)}, obj, s)
  end

  defp save_object({kind, obj}, s = r_xsd_state(redefine: true))
       when kind == :group or kind == :attributeGroup do
    save_in_table({kind, object_name(obj)}, obj, s)
  end

  defp save_object({kind, obj}, s)
       when kind == :simpleType or
              kind == :complexType do
    save_unique_type({kind, object_name(obj)}, obj, s)
  end

  defp save_object({kind, obj}, s)
       when kind == :attributeGroup or
              kind == :group do
    save_uniquely({kind, object_name(obj)}, obj, s)
  end

  defp save_object({kind, obj}, s) do
    save_in_table({kind, object_name(obj)}, obj, s)
  end

  defp save_unique_type(key = {_, name}, obj, s) do
    case resolve({:simple_or_complex_Type, name}, s) do
      {r_schema_simple_type(), _} ->
        acc_errs(
          s,
          {[], :xmerl_xsd, {:type_not_uniquely_defined_in_schema, name}}
        )

      {r_schema_complex_type(), _} ->
        acc_errs(
          s,
          {[], :xmerl_xsd, {:type_not_uniquely_defined_in_schema, name}}
        )

      _ ->
        save_in_table(key, obj, s)
    end
  end

  defp save_uniquely(key, obj, s) do
    case load_object(key, s) do
      {[], _} ->
        save_in_table(key, obj, s)

      _ ->
        acc_errs(
          s,
          {[], :xmerl_xsd, {:not_uniquely_defined_in_schema, key}}
        )
    end
  end

  defp save_schema_element(
         cM,
         s =
           r_xsd_state(
             elementFormDefault: eFD,
             attributeFormDefault: aFD,
             targetNamespace: tN,
             finalDefault: fD,
             blockDefault: bD
           )
       ) do
    elementList =
      for x = {:element, _} <- cM do
        x
      end

    schema = get_schema_cm(r_xsd_state(s, :table), tN)

    schema2 =
      case schema == r_schema() do
        true ->
          r_schema(schema,
            elementFormDefault: eFD,
            attributeFormDefault: aFD,
            targetNamespace: tN,
            blockDefault: bD,
            finalDefault: fD,
            content: elementList
          )

        _ ->
          content = r_schema(schema, :content)

          r_schema(schema,
            content:
              for x <- content,
                  member(x, elementList) == false do
                x
              end ++ elementList
          )
      end

    tN2 =
      case tN do
        :undefined ->
          []

        _ ->
          tN
      end

    _ = save_in_table({:schema, tN2}, schema2, s)
    save_to_file(s)
  end

  defp save_to_file(s = r_xsd_state(tab2file: true), fileName) do
    save_to_file(r_xsd_state(s, tab2file: fileName))
  end

  defp save_to_file(_, _) do
    :ok
  end

  defp save_to_file(s = r_xsd_state(tab2file: tF)) do
    case tF do
      true ->
        {:ok, iO} =
          :file.open(
            :filename.rootname(r_xsd_state(s, :schema_name)) ++ '.tab',
            [:write]
          )

        :io.format(iO, '~p~n', [
          try do
            :ets.tab2list(r_xsd_state(s, :table))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
        ])

        :ok = :file.close(iO)

      false ->
        :ok

      iOFile ->
        {:ok, iO} = :file.open(iOFile, [:write])

        :io.format(iO, '~p~n', [
          try do
            :ets.tab2list(r_xsd_state(s, :table))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
        ])

        :ok = :file.close(iO)
    end
  end

  defp save_merged_type(type = r_schema_simple_type(), s) do
    resave_object({:simpleType, type}, s)
  end

  defp save_merged_type(type = r_schema_complex_type(), s) do
    resave_object({:complexType, type}, s)
  end

  defp resave_object({kind, obj}, s) do
    save_in_table({kind, object_name(obj)}, obj, s)
  end

  defp save_in_table(name, elDef, s = r_xsd_state(table: tab)) do
    try do
      :ets.insert(tab, {name, elDef})
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    s
  end

  defp save_idc(:key, iDConstr, s) do
    save_key(iDConstr, s)
  end

  defp save_idc(:keyref, iDConstr, s) do
    save_keyref(iDConstr, s)
  end

  defp save_idc(:unique, iDConstr, s) do
    save_unique(iDConstr, s)
  end

  defp save_key(key, s) do
    _ = save_object({:key, key}, s)
    s
  end

  defp save_keyref(keyRef = r_id_constraint(category: :keyref), s) do
    s1 = add_keyref(keyRef, s)
    _ = save_object({:keyref, keyRef}, s1)
    s1
  end

  defp save_keyref(_, s) do
    s
  end

  defp save_unique(unique, s) do
    _ = save_object({:unique, unique}, s)
    s
  end

  defp save_substitutionGroup([], s) do
    s
  end

  defp save_substitutionGroup([{head, members} | sGs], s) do
    _ = save_in_table({:substitutionGroup, head}, members, s)

    :lists.foreach(
      fn x ->
        save_in_table({:substitutionGroup_member, x}, head, s)
      end,
      members
    )

    save_substitutionGroup(sGs, s)
  end

  defp substitutionGroup_member(elName, s) do
    case load_object(
           {:substitutionGroup_member, elName},
           s
         ) do
      {[], _} ->
        false

      {res, _} ->
        res
    end
  end

  defp add_keyref(
         r_id_constraint(name: name, refer: refer),
         s = r_xsd_state(keyrefs: keyRefs)
       ) do
    r_xsd_state(s, keyrefs: add_once({:keyref, name, refer}, keyRefs))
  end

  defp load_redefine_object({kind, name}, s) do
    load_object({kind, :redefine, name}, s)
  end

  defp load_object({:element, {qN, occ = {min, _}}}, s)
       when is_integer(min) do
    case load_object({:element, qN}, s) do
      {sE = r_schema_element(), s1} ->
        {r_schema_element(sE, occurance: occ), s1}

      other ->
        other
    end
  end

  defp load_object({:group, {qN, _Occ = {min, _}}}, s)
       when is_integer(min) do
    load_object({:group, qN}, s)
  end

  defp load_object(key, s = r_xsd_state(table: tab)) do
    case :ets.lookup(tab, key) do
      [{^key, value}] ->
        {value, s}

      [] ->
        case :ets.lookup(tab, global_def(key)) do
          [{_, value}] ->
            {value, global_scope(s)}

          other ->
            {other, s}
        end

      other ->
        {other, s}
    end
  end

  defp load_keyref(name, s) do
    case load_object({:keyref, name}, s) do
      {keyRef = r_id_constraint(), _} ->
        keyRef

      _ ->
        []
    end
  end

  defp load_key(name, s) do
    case load_object({:key, name}, s) do
      {key = r_id_constraint(), _} ->
        key

      _ ->
        case load_object({:unique, name}, s) do
          {key = r_id_constraint(), _} ->
            key

          _ ->
            []
        end
    end
  end

  defp save_ID(iD, s) do
    case member(iD, r_xsd_state(s, :IDs)) do
      true ->
        acc_errs(s, {:ID_name_not_unique, iD})

      _ ->
        r_xsd_state(s, IDs: [iD | r_xsd_state(s, :IDs)])
    end
  end

  defp check_and_save_ID(iD, s) do
    case :xmerl_xsd_type.check_simpleType(:ID, iD, s) do
      {:ok, ^iD} ->
        save_ID(iD, s)

      _ ->
        acc_errs(s, {:illegal_ID_value, iD})
    end
  end

  defp insert_substitutionGroup(r_schema_element(substitutionGroup: :undefined), s) do
    s
  end

  defp insert_substitutionGroup(
         r_schema_element(name: name, substitutionGroup: sG),
         s = r_xsd_state(substitutionGroups: sGregister)
       ) do
    case key1search(sG, sGregister, []) do
      {_, sGList} ->
        r_xsd_state(s, substitutionGroups: keyreplace(sG, 1, sGregister, {sG, [name | sGList]}))

      _ ->
        r_xsd_state(s, substitutionGroups: [{sG, [name]} | sGregister])
    end
  end

  defp global_scope(s = r_xsd_state()) do
    r_xsd_state(s, scope: [])
  end

  defp global_def({kind, {local, _, nS}})
       when kind == :simpleType or kind == :complexType or
              kind == :group or kind == :attributeGroup or
              kind == :element or kind == :attribute or
              kind == :substitutionGroup or
              kind == :substitutionGroup_member do
    {kind, {local, [], nS}}
  end

  defp global_def(d) do
    d
  end

  defp get_schema_cm(tab, :undefined) do
    get_schema_cm(tab, [])
  end

  defp get_schema_cm(tab, []) do
    get_schema_cm1(tab, [])
  end

  defp get_schema_cm(tab, namespace) do
    noNamespaceC = get_no_namespace_content(tab)
    schema = get_schema_cm1(tab, namespace)
    nSC = r_schema(schema, :content)

    r_schema(schema,
      content:
        nSC ++
          for x <- noNamespaceC,
              member(x, nSC) == false do
            x
          end
    )
  end

  defp get_schema_cm1(tab, namespace) do
    case (try do
            :ets.lookup(tab, {:schema, namespace})
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      [{_, h}] ->
        h

      _ ->
        r_schema()
    end
  end

  defp get_no_namespace_content(tab) do
    case get_schema_cm1(tab, []) do
      r_schema(content: c) ->
        c

      _ ->
        []
    end
  end

  defp is_simple_type({lName, scope, nS}, s) when is_atom(lName) do
    is_simple_type(
      {:erlang.atom_to_list(lName), scope, nS},
      s
    )
  end

  defp is_simple_type(qName = {_, _, _}, s) do
    case is_builtin_simple_type(qName) do
      true ->
        true

      _ ->
        is_derived_simple_type(qName, s)
    end
  end

  defp is_derived_simple_type(qName, s) do
    case resolve({:simpleType, qName}, s) do
      {r_schema_simple_type(), _} ->
        true

      _ ->
        false
    end
  end

  defp object_name(r_schema_element(name: n)) do
    n
  end

  defp object_name(r_schema_simple_type(name: n)) do
    n
  end

  defp object_name(r_schema_complex_type(name: n)) do
    n
  end

  defp object_name(r_schema_attribute(name: n)) do
    n
  end

  defp object_name(r_schema_attribute_group(name: n)) do
    n
  end

  defp object_name(r_schema_group(name: n)) do
    n
  end

  defp object_name(r_id_constraint(name: n)) do
    n
  end

  defp is_whitespace(r_xmlText(value: v)) do
    case (for x <- v, whitespace(x) == false do
            x
          end) do
      [] ->
        true

      _ ->
        false
    end
  end

  defp is_whitespace(_) do
    false
  end

  defp fetch(uRI, s) do
    split = :filename.split(uRI)

    filename =
      (fn
         [] ->
           []

         x ->
           :lists.last(x)
       end).(split)

    fullname =
      case split do
        ['file:' | name] ->
          :filename.join(['/' | name])

        ['/' | rest] when rest != [] ->
          uRI

        ['http:' | _Rest] ->
          {:http, uRI}

        [] ->
          []

        _ ->
          case r_xsd_state(s, :external_xsd_base) do
            true ->
              :filename.join(r_xsd_state(s, :xsd_base), uRI)

            false ->
              :filename.join(r_xsd_state(s, :xsd_base), :filename.basename(uRI))
          end
      end

    path = path_locate(r_xsd_state(s, :fetch_path), filename, fullname)
    :no_debug
    {:ok, path, s}
  end

  defp path_locate(_, _, {:http, _} = uRI) do
    uRI
  end

  defp path_locate(_, _, []) do
    []
  end

  defp path_locate([dir | dirs], fN, fullName) do
    f = :filename.join(dir, fN)

    case :file.read_file_info(f) do
      {:ok, r_file_info(type: :regular)} ->
        {:file, f}

      _ ->
        path_locate(dirs, fN, fullName)
    end
  end

  defp path_locate([], _FN, fullName) do
    {:file, fullName}
  end

  defp return_error(errs) do
    {:error, reverse(errs)}
  end

  defp return_schema_error(errs) do
    {:error, {:schema_failure, reverse(errs)}}
  end

  defp if_atom_to_list(a) when is_atom(a) do
    :erlang.atom_to_list(a)
  end

  defp if_atom_to_list(l) do
    l
  end

  defp if_list_to_atom(l) when is_list(l) do
    :erlang.list_to_atom(l)
  end

  defp if_list_to_atom(a) do
    a
  end

  defp list_members(members, completeList) do
    case (for x <- members,
              member(x, completeList) == false do
            x
          end) do
      [] ->
        true

      l ->
        {:error, l}
    end
  end

  defp whitespace(x)
       when x == 32 or x == 13 or x == 10 or
              x == 9 do
    true
  end

  defp whitespace(_) do
    false
  end

  defp key1search(key, list, default) do
    case keysearch(key, 1, list) do
      {:value, v} ->
        v

      _ ->
        default
    end
  end

  defp keyNsearch(key, n, l, default) do
    case keysearch(key, n, l) do
      {:value, v} ->
        v

      _ ->
        default
    end
  end

  defp key_replace_or_insert(key, n, list, tuple) do
    case keyreplace(key, n, list, tuple) do
      ^list ->
        [tuple | list]

      newList ->
        newList
    end
  end

  defp keysearch_delete(key, n, list, default) do
    case keysearch(key, n, list) do
      {:value, res} ->
        {res, keydelete(key, n, list)}

      _ ->
        {default, list}
    end
  end

  defp search_delete_all_el(elName, elList, s) do
    case search_delete_all_el2(elName, elList, []) do
      false ->
        case substitutionGroup_member(elName, s) do
          false ->
            false

          head ->
            case search_delete_all_el(head, elList, s) do
              {_, rest} ->
                {name, _, nS} = elName
                {{:element, {name, [], nS}}, rest}

              _ ->
                false
            end
        end

      res ->
        res
    end
  end

  defp search_delete_all_el2(_ElName, [], _NoMatch) do
    false
  end

  defp search_delete_all_el2(
         {name, scope, nS},
         [el = {:element, {{name, scopeCM, nS}, _}} | rest],
         noMatch
       )
       when scope == scopeCM or scopeCM == [] do
    {el, reverse(noMatch) ++ rest}
  end

  defp search_delete_all_el2(elName, [h | t], noMatch) do
    search_delete_all_el2(elName, t, [h | noMatch])
  end

  defp search_attribute(true, {name, _, namespace}, schemaAtts) do
    case (for a = {_, {n, _, nS}} <- schemaAtts, n == name, nS == namespace do
            a
          end) do
      [] ->
        {:undefined, schemaAtts}

      [attr] ->
        {attr, :lists.delete(attr, schemaAtts)}
    end
  end

  defp search_attribute(_, {name, _, _}, schemaAtts) do
    case (for a = {_, {n, _, _}} <- schemaAtts,
              n == name do
            a
          end) do
      [] ->
        {:undefined, schemaAtts}

      [attr] ->
        {attr, :lists.delete(attr, schemaAtts)}
    end
  end

  defp error_msg(format, args) do
    :error_logger.error_msg(format, args)
  end

  defp add_once(el, l) do
    case member(el, l) do
      true ->
        l

      _ ->
        [el | l]
    end
  end

  defp add_key_once(key, n, el, l) do
    case keymember(key, n, l) do
      true ->
        l

      _ ->
        [el | l]
    end
  end

  def format_error(l) when is_list(l) do
    for x <- l do
      format_error(x)
    end
  end

  def format_error({:unexpected_rest, uR}) do
    :io_lib.format(
      'XML: The following content of an element didn\'t validate by the provided schema, ~n~p.',
      [uR]
    )
  end

  def format_error({:unvalidated_rest, uR}) do
    :io_lib.format(
      'XML: The following content of an element didn\'t validate by the provided schema, ~n~p.',
      [uR]
    )
  end

  def format_error({:no_schemas_provided}) do
    'Schema: Validator found no schema. A schema must be provided for validation.'
  end

  def format_error({:internal_error, reason}) do
    :io_lib.format('An error occured that was unforeseen, due to ~p.', [reason])
  end

  def format_error({:internal_error, reason, info}) do
    :io_lib.format('An error occured that was unforeseen, due to ~p: ~p.', [reason, info])
  end

  def format_error({:internal_error, function, info1, info2}) do
    :io_lib.format('An internal error occured in function ~p with args: ~p,~p.', [
      function,
      info1,
      info2
    ])
  end

  def format_error({:illegal_content, reason, kind}) do
    :io_lib.format(
      'Schema: The schema violates the content model allowed for schemas.~nReason: ~p,~nkind of schema element: ~p.',
      [reason, kind]
    )
  end

  def format_error({:no_match, kind}) do
    :io_lib.format(
      'Schema: The schema violates the content model allowed for schemas.~nKind of schema element: ~p.',
      [kind]
    )
  end

  def format_error({:bad_match, s4SC, cM}) do
    :io_lib.format('Schema: The schema missed mandatory elements ~p in ~p.', [s4SC, cM])
  end

  def format_error({:unmatched_mandatory_object, sequenceEl1, sequenceEl2}) do
    :io_lib.format(
      'Schema: The schema should have had an ~p object after the ~p, but it was missing.',
      [sequenceEl2, sequenceEl1]
    )
  end

  def format_error({:parsing_external_schema_failed, file, reason}) do
    :io_lib.format('Schema: Parsing the referenced external schema ~p, failed due to ~p.', [
      file,
      reason
    ])
  end

  def format_error({:fetch_fun_failed, other}) do
    :io_lib.format('Schema: Fetching this kind of external schema is not supported ~p.', [other])
  end

  def format_error({:element_not_in_schema, [eIIName, _ElQName, _CM]}) do
    :io_lib.format('XML: The XML element ~p are not present in the schema.', [eIIName])
  end

  def format_error({:missing_mandatory_element, cMEl}) do
    :io_lib.format('XML: The XML file missed mandatory element(s) ~p defined in schema.', [cMEl])
  end

  def format_error({:empty_content_not_allowed, c}) do
    :io_lib.format('XML: The XML file missed mandatory element(s): ~p defined in schema.', [c])
  end

  def format_error({:element_not_suitable_with_schema, elName, _S}) do
    :io_lib.format(
      'XML: The XML element: ~p violates the schema, probably to many of same element.',
      [elName]
    )
  end

  def format_error({:element_not_suitable_with_schema, elName, cMName, _CMEl, _S}) do
    :io_lib.format('XML: The XML element: ~p violates the schema. Schema expected element ~p.', [
      elName,
      cMName
    ])
  end

  def format_error({:no_element_expected_in_group, xML}) do
    :io_lib.format('XML: The XML element(s) ~p violates the schema. No element was expected.', [
      xML
    ])
  end

  def format_error({:element_bad_match, e, any, _Env}) do
    :io_lib.format(
      'XML: XML element ~p didn\'t match into the namespace of schema type any ~p.',
      [e, any]
    )
  end

  def format_error({:match_failure, _XML, _CM, _S}) do
    'XML: A combination of XML element(s) and schema definitions that is not known has occured. The implementation doesn\'t support this structure.'
  end

  def format_error({:cannot_contain_text, _XMLTxt, cMEl}) do
    :io_lib.format('XML: The schema structure: ~p doesn\'t allow text', [cMEl])
  end

  def format_error({:missing_mandatory_elements, mandatoryEls}) do
    :io_lib.format('XML: A schema sequence has mandatory elements ~p, that were unmatched.', [
      mandatoryEls
    ])
  end

  def format_error({:choice_missmatch, t, els}) do
    :io_lib.format(
      'XML: A schema choice structure with the alternatives: ~p doesn\'t allow the text: ~p.',
      [els, t]
    )
  end

  def format_error({:no_element_matching_choice, xML}) do
    :io_lib.format(
      'XML: The choice at location: ~p had no alternative that matched the XML structure(s): ~p.',
      [error_path(xML, :undefined), xML]
    )
  end

  def format_error({:all_missmatch, t, cM}) do
    :io_lib.format(
      'XML: The schema expected one of: ~p, but the XML content was text: ~p at the location: ~p.',
      [cM, t, error_path(t, :undefined)]
    )
  end

  def format_error({:element_not_in_all, elName, e, _CM}) do
    :io_lib.format(
      'XML: The element ~p at location ~p in the XML file was not allowed according to the schema.',
      [elName, error_path(e, :undefined)]
    )
  end

  def format_error({:missing_mandatory_elements_in_all, mandatoryEls}) do
    :io_lib.format('XML: The schema elements ~p were missed in the XML file.', [mandatoryEls])
  end

  def format_error({:failed_validating, e, any}) do
    :io_lib.format(
      'XML: The element ~p at location ~p failed validation. It should hav been matched by an any schema element ~p',
      [r_xmlElement(e, :name), error_path(e, :undefined), any]
    )
  end

  def format_error({:schemaLocation_list_failure, paths}) do
    :io_lib.format(
      'XML: schemaLocation values consists of one or more pairs of URI references, separated by white space. The first is a namespace name the second a reference to a schema: ~p.',
      [paths]
    )
  end

  def format_error({:element_content_not_nil, xMLEl}) do
    :io_lib.format(
      'XML: The element ~p at position ~p has content of text/elements despite the nillable attribute was true.',
      [r_xmlElement(xMLEl, :name), error_path(xMLEl, :undefined)]
    )
  end

  def format_error({:attribute_in_simpleType, el, att}) do
    :io_lib.format(
      'XML: The element ~p at location ~p must not have attributes like: ~p since it according to the schema has simpleType type.',
      [r_xmlElement(el, :name), error_path(el, :undefined), att]
    )
  end

  def format_error({:required_attribute_missed, el, name}) do
    :io_lib.format(
      'XML: The schema required an attribute ~p in element at location ~p that was missing.',
      [name, error_path(el, :undefined)]
    )
  end

  def format_error({:default_and_fixed_attributes_mutual_exclusive, name, default, fix}) do
    :io_lib.format(
      'Schema: It is an error in the schema to assign values for both default and fix for an attribute. Attribute: ~p, default: ~p, fix: ~p.',
      [name, default, fix]
    )
  end

  def format_error({:schema_error, :unexpected_object, _SA, _Err}) do
    'Schema: An unforeseen error case occured, maybee due to an unimplemented feature.'
  end

  def format_error({:attribute_not_defined_in_schema, name}) do
    :io_lib.format('XML: The attribute ~p is not defined in the provided schema.', [name])
  end

  def format_error({:disallowed_namespace, namespace, nS, name}) do
    :io_lib.format(
      'XML: The attribute ~p is not valid because the namespace ~p is forbidden by ~p',
      [name, nS, namespace]
    )
  end

  def format_error({:cirkular_attributeGroup_reference, name}) do
    :io_lib.format(
      'Schema: Cirkular references to attribute groups are forbidden. One was detected including ~p.',
      [name]
    )
  end

  def format_error({:could_not_resolve_type, sT}) do
    :io_lib.format(
      'Schema: The simpleType ~p could not be found among the types defined by the provided schema.',
      [sT]
    )
  end

  def format_error({:could_not_check_value_for_type, type}) do
    :io_lib.format('XML: Checking value for type ~p is not implemented.', [type])
  end

  def format_error({:unknown_simpleType, bT}) do
    :io_lib.format(
      'Schema: The simpleType ~p could not be found among the types defined by the provided schema',
      [bT]
    )
  end

  def format_error({:abstract_element_instance, elName}) do
    :io_lib.format(
      'XML: Elements defined as abstract in the schema must not be instantiated in XML: ~p.',
      [elName]
    )
  end

  def format_error({:qualified_name_required, localName}) do
    :io_lib.format(
      'XML: Element name ~p in XML instance is not qualified, though the schema requires that.',
      [localName]
    )
  end

  def format_error({:unqualified_name_required, qualifiedName}) do
    :io_lib.format(
      'XML: Element name ~p in XML instance must be unqualified, according to schema.',
      [qualifiedName]
    )
  end

  def format_error({:illegal_key_sequence_value, err}) do
    :io_lib.format(
      'XML: The \'key-sequence\', (se XML-spec 3.11.4), must be a node with at most one member: ~p',
      [err]
    )
  end

  def format_error({:qualified_node_set_not_correct_for_key, _Err}) do
    'Schema: The \'target node set\' and \'qualified node set\' (se XML-spec 3.11.4.2.1) must be equal.'
  end

  def format_error({:key_value_not_unique, kS}) do
    :io_lib.format('Schema: Key values must be unique within the schema. This is not ~p,', [kS])
  end

  def format_error({:keyref_missed_matching_key, refer}) do
    :io_lib.format('Schema: This keyref had no matching key ~p.', [refer])
  end

  def format_error({:keyref_unexpected_object, _Other}) do
    'Schema: An unforeseen error case occured, unknown failure cause.'
  end

  def format_error({:cardinality_of_fields_not_equal, kR, k}) do
    :io_lib.format(
      'Schema: keyref and the corresponding key must have same cardinality of their fields. Missmatch in this case keyref: ~p, key: ~p.',
      [kR, k]
    )
  end

  def format_error({:could_not_load_keyref, name}) do
    :io_lib.format('Schema: The schema didn\'t define a keyref with the name ~p.', [name])
  end

  def format_error({:reference_undeclared, kind, ref}) do
    :io_lib.format('Schema: The schema didn\'t define an ~p with the name ~p.', [kind, ref])
  end

  def format_error({:cyclic_substitutionGroup, sGs}) do
    :io_lib.format(
      'Schema: cyclic substitutionGroup was detected, substitutionGroup structure is ~p.',
      [sGs]
    )
  end

  def format_error({:substitutionGroup_error, head, sG}) do
    :io_lib.format(
      'Schema: Either of substitutionGroup members ~p or ~p is not defined in the provided schema.',
      [head, sG]
    )
  end

  def format_error({:cyclic_definition, cA}) do
    :io_lib.format('Schema: A forbidden cicular definition was detected ~p.', [cA])
  end

  def format_error({:type_of_element_not_derived, memT, headT}) do
    :io_lib.format(
      'Schema: Type in substitutionGroup members should be simpleType or complexType. In this case ~p and ~p were found.',
      [memT, headT]
    )
  end

  def format_error({:derivation_blocked, blockTag, derivation}) do
    :io_lib.format('Derivation by ~p is blocked by the blocking tag ~p.', [derivation, blockTag])
  end

  def format_error({:names_not_equal, qName1, qName2}) do
    :io_lib.format('The type ~p seems to be derived from another type than the base type ~p', [
      qName2,
      qName1
    ])
  end

  def format_error({:illegal_content_in_extension, ext}) do
    :io_lib.format(
      'The extension content ~p didn\'t match the content model of the provided schema.',
      [ext]
    )
  end

  def format_error({seqCho, :expected, other, :found})
      when seqCho == :sequence or seqCho == :choice do
    :io_lib.format(
      'Schema: The restriction content ~p didn\'t match the content model of the provided schema, ~p was expected.',
      [seqCho, other]
    )
  end

  def format_error({:does_not_support, f, :in_restriction}) do
    :io_lib.format('Schema: The structure ~p is not supported in the implementation.', [f])
  end

  def format_error({:illegal_content_simple_type, cM, typeName}) do
    :io_lib.format('Schema: ~p content is not allowed in a simpleType, as in ~p.', [cM, typeName])
  end

  def format_error({:illegal_in_restriction_of_simpleType, x}) do
    :io_lib.format('Schema: The ~p content is illegal in a simpleType.', [x])
  end

  def format_error({:element, name, :not_present_in_restriction}) do
    :io_lib.format(
      'Schema: In a restriction all element names of the restriction must be one of the elements of the base type. ~p is not.',
      [name]
    )
  end

  def format_error({:invalid_derivation, eA, baseAtts}) do
    :io_lib.format(
      'Schema: An anyAttribute ~p in a restricted derived type must be present among the base type attributes ~p.',
      [eA, baseAtts]
    )
  end

  def format_error({:wildcard_namespace, nS, :not_subset_of_base_namespace, baseNS}) do
    :io_lib.format(
      'Schema: See XML spec. section 3.10.6. This wildcard namespace ~p is not allowed by the base namespace restrictions ~p.',
      [nS, baseNS]
    )
  end

  def format_error({:wildcard_namespace_union_not_expressible, nS1, nS2}) do
    :io_lib.format(
      'Schema: See XML spec. section 3.10.6. The union of namespaces ~p and ~p is not expressible.',
      [nS1, nS2]
    )
  end

  def format_error({:wildcard_namespace_intersection_not_expressible, nS1, nS2}) do
    :io_lib.format(
      'Schema: See XML spec. section 3.10.6. The intersection of namespaces ~p and ~p is not expressible.',
      [nS1, nS2]
    )
  end

  def format_error({:circular_reference_of_type, tName}) do
    :io_lib.format(
      'Schema: An illicit circular reference involving simple/complex type ~p has been detected.',
      [tName]
    )
  end

  def format_error({:type_not_uniquely_defined_in_schema, name}) do
    :io_lib.format(
      'Schema: See XML spec. section 3.4.1. Type names whether simple or complex must be unique within the schema. ~p is not.',
      [name]
    )
  end

  def format_error({:not_uniquely_defined_in_schema, key}) do
    :io_lib.format(
      'Schema: All schema objects of the same kind identified by name must be unique within the schema. ~p is not.',
      [key]
    )
  end

  def format_error({:illegal_ID_value, iD}) do
    :io_lib.format('The ID value ~p is not allowed as an ID value.', [iD])
  end

  def format_error({:incomplete_file, _FileName, _Other}) do
    'Schema: The file containing a schema state must be produced by xmerl_xsd:state2file/[1,2].'
  end

  def format_error({:unexpected_content_in_any, a}) do
    :io_lib.format(
      'Schema: The any type is considered to have no content besides annotation. ~p was found.',
      [a]
    )
  end

  def format_error({:erroneous_content_in_identity_constraint, iDC, err}) do
    :io_lib.format(
      'Schema: An ~p identity constraint must have one selector and one or more field in content. This case ~p',
      [iDC, err]
    )
  end

  def format_error({:missing_xpath_attribute, iDCContent}) do
    :io_lib.format('Schema: A ~p in a identity constraint must have a xpath attribute.', [
      iDCContent
    ])
  end

  def format_error({:content_in_anyAttribute, err}) do
    :io_lib.format(
      'Schema: ~p is not allowed in anyAttribute. Content cannot be anything else than annotation.',
      [err]
    )
  end

  def format_error({:content_in_simpleContent, err}) do
    :io_lib.format(
      'Schema: Content of simpleContent can only be an optional annotation and one of restriction or extension. In this case ~p.',
      [err]
    )
  end

  def format_error({:complexContent_content_failure, err}) do
    :io_lib.format(
      'Schema: Besides an optional annotation complexContent should have one of restriction or extension. In this case ~p.',
      [err]
    )
  end

  def format_error({:union_member_type_not_simpleType, illegalType}) do
    :io_lib.format(
      'Schema: ~p is not allowed in a union. Content must be any nymber of simpleType.',
      [illegalType]
    )
  end

  def format_error({:missing_base_type, :restriction, _Other}) do
    'Schema: A restriction must have a base type, either assigned by the \'base\' attribute or as a simpleType defined in content.'
  end

  def format_error({:content_failure_expected_restriction_or_extension, kind, _}) do
    :io_lib.format('Schema: A ~p had no restriction or extension in content.', [kind])
  end

  def format_error({:content_failure_only_one_restriction_or_extension_allowed, kind, _}) do
    :io_lib.format('Schema: A ~p can only have one of restriction or extension in content.', [
      kind
    ])
  end

  def format_error({:mandatory_component_missing, s4SCMRest, kind}) do
    :io_lib.format('Schema: After matching a ~p the schema should have had content ~p.', [
      kind,
      s4SCMRest
    ])
  end

  def format_error(err) do
    :io_lib.format('~p~n', [err])
  end

  defp default_namespace_by_convention() do
    [{:xml, :"http://www.w3.org/XML/1998/namespace"}]
  end
end
