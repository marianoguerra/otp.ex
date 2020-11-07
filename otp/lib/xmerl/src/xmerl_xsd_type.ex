defmodule :m_xmerl_xsd_type do
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

  def check_simpleType(name, value, s) when is_list(name) do
    :no_debug
    check_simpleType(:erlang.list_to_atom(name), value, s)
  end

  def check_simpleType(:string, value, _S) do
    case (for x <- value, :xmerl_lib.is_char(x) do
            x
          end) do
      ^value ->
        {:ok, value}

      _ ->
        {:error, {:value_not_string, value}}
    end
  end

  def check_simpleType(:normalizedString, value, _S) do
    case (for x <- value, :xmerl_lib.is_char(x), ns_whitespace(x) == false do
            x
          end) do
      ^value ->
        {:ok, value}

      _ ->
        {:error, {:value_not_normalizedString, value}}
    end
  end

  def check_simpleType(:boolean, 'true', _S) do
    {:ok, 'true'}
  end

  def check_simpleType(:boolean, 'false', _S) do
    {:ok, 'false'}
  end

  def check_simpleType(:boolean, '1', _S) do
    {:ok, '1'}
  end

  def check_simpleType(:boolean, '0', _S) do
    {:ok, '0'}
  end

  def check_simpleType(:boolean, other, _S) do
    {:error, {:value_not_boolean, other}}
  end

  def check_simpleType(:decimal, value, _S) do
    case (try do
            check_decimal(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :invalid_decimal, value}}

      {:error, _} ->
        {:error, {:invalid_decimal, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:integer, value, _S) do
    case (try do
            check_integer(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :invalid_integer, value}}

      {:error, _} ->
        {:error, {:invalid_integer, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:float, value, _S) do
    case (try do
            check_float(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :invalid_float, value}}

      {:error, _} ->
        {:error, {:invalid_float, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:double, value, _S) do
    case (try do
            check_double(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :invalid_double, value}}

      {:error, _} ->
        {:error, {:invalid_double, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:duration, value, _S) do
    case (try do
            check_duration(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :invalid_duration, value}}

      {:error, _} ->
        {:error, {:invalid_duration, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:dateTime, value, _S) do
    case (try do
            check_dateTime(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :invalid_dateTime, value}}

      {:error, _} ->
        {:error, {:invalid_dateTime, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:time, value, _S) do
    case (try do
            check_time(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :invalid_time, value}}

      {:error, _} ->
        {:error, {:invalid_time, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:date, value, _S) do
    case (try do
            check_date(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :invalid_date, value}}

      {:error, _} ->
        {:error, {:invalid_date, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:gYearMonth, value, _S) do
    case (try do
            check_gYearMonth(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :invalid_gYearMonth, value}}

      {:error, _} ->
        {:error, {:invalid_gYearMonth, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:gYear, value, _S) do
    case (try do
            check_gYear(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :invalid_gYear, value}}

      {:error, _} ->
        {:error, {:invalid_gYear, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:gMonthDay, value, _S) do
    case (try do
            check_gMonthDay(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :invalid_gMonthDay, value}}

      {:error, _} ->
        {:error, {:invalid_gMonthDay, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:gDay, value, _S) do
    case (try do
            check_gDay(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :invalid_gDay, value}}

      {:error, _} ->
        {:error, {:invalid_gDay, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:gMonth, value, _S) do
    case (try do
            check_gMonth(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :invalid_gMonth, value}}

      {:error, _} ->
        {:error, {:invalid_gMonth, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:hexBinary, value, _S) do
    isEven = fn x ->
      case rem(x, 2) do
        0 ->
          true

        _ ->
          false
      end
    end

    isHex = fn
      x when x >= ?A and x <= ?F ->
        true

      x when x >= ?a and x <= ?f ->
        true

      x when x >= ?0 and x <= ?9 ->
        true

      _ ->
        false
    end

    case (for x <- value, isEven.(length(value)), isHex.(x) do
            x
          end) do
      ^value ->
        {:ok, value}

      _ ->
        {:error, {:value_not_hexBinary, value}}
    end
  end

  def check_simpleType(:base64Binary, value, _S) do
    check_base64Binary(value)
  end

  def check_simpleType(:anyURI, value, s) do
    case :xmerl_uri.parse(value) do
      {:error, _} ->
        case (try do
                :file.read_file_info(
                  :filename.join(
                    r_xsd_state(s, :xsd_base),
                    value
                  )
                )
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:ok, _} ->
            {:ok, value}

          _ ->
            {:error, {:value_not_anyURI, value}}
        end

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:QName, value, _S) do
    case :xmerl_lib.is_name(value) do
      true ->
        {:ok, value}

      _ ->
        {:error, {:value_not_QName, value}}
    end
  end

  def check_simpleType(:NOTATION, value, _S) do
    {:ok, value}
  end

  def check_simpleType(:token, value, _S) do
    case (try do
            check_token(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :invalid_token, value}}

      {:error, _} ->
        {:error, {:invalid_token, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:language, value, _S) do
    case (try do
            check_language(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_language, value}}

      {:error, _} ->
        {:error, {:illegal_language, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:NMTOKEN, value, _S) do
    case (try do
            check_NMTOKEN(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_NMTOKEN, value}}

      {:error, _} ->
        {:error, {:illegal_NMTOKEN, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:NMTOKENS, value, _S) do
    case (try do
            check_NMTOKENS(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_NMTOKENS, value}}

      {:error, _} ->
        {:error, {:illegal_NMTOKENS, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:Name, value, _S) do
    case (try do
            check_Name(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_name, value}}

      {:error, _} ->
        {:error, {:illegal_name, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:NCName, value, _S) do
    case (try do
            check_NCName(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_name, value}}

      {:error, _} ->
        {:error, {:illegal_name, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:ID, value, _S) do
    case (try do
            check_ID(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_ID, value}}

      {:error, _} ->
        {:error, {:illegal_ID, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:IDREF, value, _S) do
    case (try do
            check_IDREF(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_IDREF, value}}

      {:error, _} ->
        {:error, {:illegal_IDREF, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:IDREFS, value, _S) do
    case (try do
            check_IDREFS(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_IDREFS, value}}

      {:error, _} ->
        {:error, {:illegal_IDREFS, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:ENTITY, value, _S) do
    case (try do
            check_ENTITY(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_ENTITY, value}}

      {:error, _} ->
        {:error, {:illegal_ENTITY, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:ENTITIES, value, _S) do
    case (try do
            check_ENTITIES(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_ENTITIES, value}}

      {:error, _} ->
        {:error, {:illegal_ENTITIES, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:nonPositiveInteger, value, _S) do
    case (try do
            check_nonPositiveInteger(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_nonPositiveInteger, value}}

      {:error, _} ->
        {:error, {:illegal_nonPositiveInteger, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:negativeInteger, value, _S) do
    case (try do
            check_negativeInteger(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_negativeInteger, value}}

      {:error, _} ->
        {:error, {:illegal_negativeInteger, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:long, value, _S) do
    case (try do
            check_long(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_long, value}}

      {:error, _} ->
        {:error, {:illegal_long, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:int, value, _S) do
    case (try do
            check_int(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_int, value}}

      {:error, _} ->
        {:error, {:illegal_int, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:short, value, _S) do
    case (try do
            check_short(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_short, value}}

      {:error, _} ->
        {:error, {:illegal_short, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:byte, value, _S) do
    case (try do
            check_byte(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_byte, value}}

      {:error, _} ->
        {:error, {:illegal_byte, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:nonNegativeInteger, value, _S) do
    case (try do
            check_nonNegativeInteger(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_nonNegativeInteger, value}}

      {:error, _} ->
        {:error, {:illegal_nonNegativeInteger, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:unsignedLong, value, _S) do
    case (try do
            check_unsignedLong(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_unsignedLong, value}}

      {:error, _} ->
        {:error, {:illegal_unsignedLong, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:unsignedInt, value, _S) do
    case (try do
            check_unsignedInt(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_unsignedInt, value}}

      {:error, _} ->
        {:error, {:illegal_unsignedInt, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:unsignedShort, value, _S) do
    case (try do
            check_unsignedShort(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_unsignedShort, value}}

      {:error, _} ->
        {:error, {:illegal_unsignedShort, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:unsignedByte, value, _S) do
    case (try do
            check_unsignedByte(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_unsignedByte, value}}

      {:error, _} ->
        {:error, {:illegal_unsignedByte, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(:positiveInteger, value, _S) do
    case (try do
            check_positiveInteger(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:type, :illegal_positiveInteger, value}}

      {:error, _} ->
        {:error, {:illegal_positiveInteger, value}}

      _ ->
        {:ok, value}
    end
  end

  def check_simpleType(unknown, value, _S) do
    {:error, {:unknown_type, unknown, value}}
  end

  defp check_decimal(value) do
    case :string.tokens(value, '.') do
      l when length(l) == 1 or length(l) == 2 ->
        _ =
          for x <- l do
            :erlang.list_to_integer(x)
          end

        {:ok, value}

      _ ->
        {:error, {:value_not_decimal, value}}
    end
  end

  defp check_float(v = '-INF') do
    {:ok, v}
  end

  defp check_float(v = 'INF') do
    {:ok, v}
  end

  defp check_float(v = 'NaN') do
    {:ok, v}
  end

  defp check_float(value) do
    case :string.tokens(value, 'eE') do
      [mantissa, exponent] ->
        {:ok, _} = check_decimal(mantissa)
        {:ok, _} = check_integer(exponent)
        :ok

      [mantissa] ->
        {:ok, _} = check_decimal(mantissa)
        :ok
    end

    {:ok, value}
  end

  defp check_double(value) do
    check_float(value)
  end

  defp check_duration('-' ++ value) do
    check_duration(value)
  end

  defp check_duration('P' ++ value) do
    {date, time} =
      :lists.splitwith(
        fn
          ?T ->
            false

          _ ->
            true
        end,
        value
      )

    {:ok, _} = check_duration_date(date, ['Y', 'M', 'D'])
    {:ok, _} = check_duration_time(time, ['T', 'H', 'M', 'S'])
  end

  defp check_duration_date('', _) do
    {:ok, ''}
  end

  defp check_duration_date(date, [h | t]) do
    case :string.tokens(date, h) do
      [^date] ->
        check_duration_date(date, t)

      [dateItem] ->
        {:ok, _} = check_positive_integer(dateItem)

      [dateItem, rest] ->
        {:ok, _} = check_positive_integer(dateItem)
        check_duration_date(rest, t)
    end
  end

  defp check_duration_time('', [_H | _T]) do
    {:ok, ''}
  end

  defp check_duration_time(time, [s]) do
    [sec] = :string.tokens(time, s)
    {:ok, _} = check_decimal(sec)
  end

  defp check_duration_time('T' ++ time, tTokens) do
    [_H | _] = time
    check_duration_time(time, tl(tTokens))
  end

  defp check_duration_time(time, [h | t]) do
    case :string.tokens(time, h) do
      [^time] ->
        check_duration_time(time, t)

      [timeItem] ->
        {:ok, _} = check_positive_integer(timeItem)

      [timeItem, rest] ->
        {:ok, _} = check_positive_integer(timeItem)
        check_duration_time(rest, t)
    end
  end

  defp check_positive_integer(value) do
    case (try do
            :erlang.list_to_integer(value)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      int when is_integer(int) and int >= 0 ->
        {:ok, int}

      _ ->
        {:error, {:value_not_integer, value}}
    end
  end

  defp check_integer(value) do
    {:ok, :erlang.list_to_integer(value)}
  end

  defp check_nonPositiveInteger(value) do
    check_constr_int(value, :undefined, 0, :illegal_nonPositiveInteger)
  end

  defp check_negativeInteger(value) do
    check_constr_int(value, :undefined, -1, :illegal_negativeInteger)
  end

  defp check_long(value) do
    check_constr_int(value, -9_223_372_036_854_775_808, 9_223_372_036_854_775_807, :illegal_long)
  end

  defp check_int(value) do
    check_constr_int(value, -2_147_483_648, 2_147_483_647, :illegal_int)
  end

  defp check_short(value) do
    check_constr_int(value, -32768, 32767, :illegal_short)
  end

  defp check_byte(value) do
    check_constr_int(value, -128, 127, :illegal_byte)
  end

  defp check_nonNegativeInteger(value) do
    check_constr_int(value, 0, :undefined, :illegal_nonNegativeInteger)
  end

  defp check_unsignedLong(value) do
    check_constr_int(value, 0, 18_446_744_073_709_551_615, :illegal_unsignedLong)
  end

  defp check_unsignedInt(value) do
    check_constr_int(value, 0, 4_294_967_295, :illegal_unsignedInt)
  end

  defp check_unsignedShort(value) do
    check_constr_int(value, 0, 65535, :illegal_unsignedShort)
  end

  defp check_unsignedByte(value) do
    check_constr_int(value, 0, 255, :illegal_unsignedByte)
  end

  defp check_positiveInteger(value) do
    check_constr_int(value, 1, :undefined, :illegal_positiveInteger)
  end

  defp check_constr_int(value, :undefined, max, errMsg) do
    case check_integer(value) do
      {:ok, int} when int <= max ->
        {:ok, int}

      _ ->
        {:error, {errMsg}}
    end
  end

  defp check_constr_int(value, min, max, errMsg) do
    case check_integer(value) do
      {:ok, int} when int >= min and int <= max ->
        {:ok, int}

      _ ->
        {:error, {errMsg}}
    end
  end

  defp check_dateTime('-' ++ dateTime) do
    check_dateTime(dateTime)
  end

  defp check_dateTime('+' ++ _DateTime) do
    {:error, {:invalid_dateTime, :plus_sign}}
  end

  defp check_dateTime(dateTime) do
    [date, time] = :string.tokens(dateTime, 'T')
    [y, m, d] = :string.tokens(date, '-')
    {:ok, _} = check_year(y)
    {:ok, _} = check_positive_integer(m)
    {:ok, _} = check_positive_integer(d)
    check_time(time)
  end

  defp check_year(y) when length(y) > 4 do
    ^y = :string.strip(y, :left, ?0)
    {:ok, :erlang.list_to_integer(y)}
  end

  defp check_year(y) do
    case :erlang.list_to_integer(y) do
      year when year !== 0 ->
        {:ok, year}

      _ ->
        {:error, {:invalid_year, y}}
    end
  end

  defp check_month(str) do
    case check_positive_integer(str) do
      {:ok, int} when int >= 1 and int <= 12 ->
        {:ok, int}

      _ ->
        {:error, {:invalid_month, str}}
    end
  end

  defp check_day(str) do
    case check_positive_integer(str) do
      {:ok, int} when int >= 1 and int <= 31 ->
        {:ok, int}

      _ ->
        {:error, {:invalid_day, str}}
    end
  end

  defp check_time(time) do
    {hMS, tZ} =
      case :lists.split(8, time) do
        {t, '.' ++ secFractionsTZ} ->
          onlyDigits = fn
            x when x >= ?0 and x <= ?9 ->
              true

            _ ->
              false
          end

          {secFrac, tZone} =
            :lists.splitwith(
              onlyDigits,
              secFractionsTZ
            )

          {:ok, _} = check_positive_integer(secFrac)
          {t, tZone}

        {t, tZone} ->
          {t, tZone}
      end

    [h, m, s] = :string.tokens(hMS, ':')
    {:ok, _} = check_hour(h)
    {:ok, _} = check_minute(m)
    {:ok, _} = check_second(s)

    case tZ do
      [] ->
        {:ok, time}

      _ ->
        check_timezone(tZ)
    end
  end

  defp check_hour(str) do
    case check_positive_integer(str) do
      {:ok, h} when h >= 0 and h <= 24 ->
        {:ok, h}

      _ ->
        {:error, {:invalid_hour, str}}
    end
  end

  defp check_minute(str) do
    case check_positive_integer(str) do
      {:ok, h} when h >= 0 and h <= 60 ->
        {:ok, h}

      _ ->
        {:error, {:invalid_minute, str}}
    end
  end

  defp check_second(str) do
    case check_positive_integer(str) do
      {:ok, h} when h >= 0 and h <= 60 ->
        {:ok, h}

      _ ->
        {:error, {:invalid_second, str}}
    end
  end

  defp check_timezone('Z') do
    {:ok, 'Z'}
  end

  defp check_timezone(tZ) do
    [h, m] = :string.tokens(tZ, ':')

    case check_integer(h) do
      {:ok, h2} when h2 >= -13 and h2 <= 13 ->
        case check_positive_integer(m) do
          {:ok, m2} when m2 >= 0 and m2 <= 59 ->
            {:ok, {h2, m2}}

          _ ->
            {:error, {:invalid_timezone, tZ, m}}
        end

      {:ok, h2} when h2 == 14 or h2 == -14 ->
        case check_positive_integer(m) do
          {:ok, 0} ->
            {:ok, {h2, 0}}

          _ ->
            {:error, {:invalid_timezone, tZ}}
        end

      _ ->
        {:error, {:invalid_timezone, tZ}}
    end
  end

  defp check_date('-' ++ date) do
    check_date(date)
  end

  defp check_date('+' ++ _Date) do
    {:error, {:invalid_date, :plus_sign}}
  end

  defp check_date(date) do
    {year, month, day} =
      case :string.tokens(date, '-+Z') do
        [y, m, d, tZ] ->
          {:ok, _} = check_timezone(tZ)
          {y, m, d}

        [y, m, d] ->
          {y, m, d}
      end

    {:ok, _} = check_year(year)
    {:ok, _} = check_month(month)
    {:ok, _} = check_day(day)
  end

  defp check_gYearMonth('-' ++ value) do
    check_gYearMonth(value)
  end

  defp check_gYearMonth('+' ++ _Value) do
    {:error, {:invalid_gYearMonth, :plus_sign}}
  end

  defp check_gYearMonth(value) do
    {year, month} =
      case :string.tokens(value, '-+Z') do
        [y, m, tZ] ->
          {:ok, _} = check_timezone(tZ)
          {y, m}

        [y, m] ->
          {y, m}
      end

    {:ok, _} = check_year(year)
    {:ok, _} = check_month(month)
  end

  defp check_gYear('-' ++ value) do
    check_gYear(value)
  end

  defp check_gYear('+' ++ _Value) do
    {:error, {:invalid_gYear, :plus_sign}}
  end

  defp check_gYear(value) do
    year =
      case :string.tokens(value, '-+Z') do
        [y, tZ] ->
          {:ok, _} = check_timezone(tZ)
          y

        [y] ->
          y
      end

    {:ok, _} = check_year(year)
  end

  defp check_gMonthDay('--' ++ value) do
    {m, '-' ++ dTZ} = :lists.split(2, value)
    {:ok, _} = check_month(m)
    {:ok, _} = check_gDay2(dTZ)
  end

  defp check_gDay('---' ++ value) do
    check_gDay2(value)
  end

  defp check_gDay2(value) do
    {d, tZ} = :lists.split(2, value)
    {:ok, _} = check_day(d)

    case tZ do
      [] ->
        {:ok, value}

      _ ->
        {:ok, _} = check_timezone(tZ)
    end
  end

  defp check_gMonth('--' ++ value) do
    {m, tZ} = :lists.split(2, value)
    {:ok, _} = check_month(m)

    case tZ do
      [] ->
        {:ok, value}

      _ ->
        {:ok, _} = check_timezone(tZ)
    end
  end

  defp check_base64Binary(value) do
    case (try do
            :xmerl_b64Bin.parse(:xmerl_b64Bin_scan.scan(value))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, _} ->
        {:ok, value}

      err = {:error, _} ->
        err

      {:EXIT, {:error, reason}} ->
        {:error, reason}

      {:EXIT, reason} ->
        {:error, {:internal_error, reason}}
    end
  end

  defp check_token(v = [32 | _]) do
    {:error, {:invalid_token, :leading_space, v}}
  end

  defp check_token(value) do
    check_token(value, value)
  end

  defp check_token([], value) do
    {:ok, value}
  end

  defp check_token([32], v) do
    {:error, {:invalid_token, :trailing_space, v}}
  end

  defp check_token([9 | _T], v) do
    {:error, {:invalid_token, :tab, v}}
  end

  defp check_token([10 | _T], v) do
    {:error, {:invalid_token, :line_feed, v}}
  end

  defp check_token([13 | _T], v) do
    {:error, {:invalid_token, :carriage_return, v}}
  end

  defp check_token([[32, 32] | _T], v) do
    {:error, {:invalid_token, :double_space, v}}
  end

  defp check_token([_H | t], v) do
    check_token(t, v)
  end

  defp check_language(value) do
    check_language(value, 0)
  end

  defp check_language([h | t], n) when h >= ?A and h <= ?Z do
    check_language(t, n + 1)
  end

  defp check_language([h | t], n) when h >= ?a and h <= ?z do
    check_language(t, n + 1)
  end

  defp check_language([?- | t], n) when n >= 1 and n <= 8 do
    check_language2(t, 0)
  end

  defp check_language([], n) when n >= 1 and n <= 8 do
    {:ok, []}
  end

  defp check_language2([h | t], n) when h >= ?A and h <= ?Z do
    check_language2(t, n + 1)
  end

  defp check_language2([h | t], n) when h >= ?a and h <= ?z do
    check_language2(t, n + 1)
  end

  defp check_language2([h | t], n) when h >= ?0 and h <= ?9 do
    check_language2(t, n + 1)
  end

  defp check_language2([?- | t], n) when n >= 1 and n <= 8 do
    check_language2(t, 0)
  end

  defp check_language2([], n) when n >= 1 and n <= 8 do
    {:ok, []}
  end

  defp check_NMTOKEN([h | t]) do
    true = :xmerl_lib.is_namechar(h)
    check_NMTOKEN2(t)
  end

  defp check_NMTOKEN2([]) do
    {:ok, []}
  end

  defp check_NMTOKEN2([h | t]) do
    true = :xmerl_lib.is_namechar(h)
    check_NMTOKEN2(t)
  end

  defp check_NMTOKENS(value) do
    tokList = :string.tokens(value, ' \n\t\r')
    :lists.foreach(&check_NMTOKEN/1, tokList)
    {:ok, value}
  end

  defp check_Name(value) do
    true = :xmerl_lib.is_name(value)
    {:ok, value}
  end

  defp check_NCName(value) do
    true = :xmerl_lib.is_ncname(value)
    {:ok, value}
  end

  defp check_ID(value) do
    true = :xmerl_lib.is_ncname(value)
    {:ok, value}
  end

  defp check_IDREF(value) do
    true = :xmerl_lib.is_name(value)
    {:ok, value}
  end

  defp check_IDREFS(value) do
    check_list_type(value, &check_IDREF/1)
  end

  defp check_ENTITY(value) do
    true = :xmerl_lib.is_ncname(value)
    {:ok, value}
  end

  defp check_ENTITIES(value) do
    check_list_type(value, &check_ENTITY/1)
  end

  defp check_list_type(value, baseTypeFun) do
    tokens = :string.tokens(value, ' \n\t\r')
    :lists.foreach(baseTypeFun, tokens)
    {:ok, value}
  end

  defp ns_whitespace(wS) when wS == 9 or wS == 10 or wS == 13 do
    true
  end

  defp ns_whitespace(_) do
    false
  end

  def facet_fun(type, {:length, v}) do
    length_fun(type, :erlang.list_to_integer(v))
  end

  def facet_fun(type, {:minLength, v}) do
    minLength_fun(type, :erlang.list_to_integer(v))
  end

  def facet_fun(type, {:maxLength, v}) do
    maxLength_fun(type, :erlang.list_to_integer(v))
  end

  def facet_fun(type, {:pattern, v}) do
    pattern_fun(type, v)
  end

  def facet_fun(type, {:enumeration, v}) do
    enumeration_fun(type, v)
  end

  def facet_fun(type, {:whiteSpace, v}) do
    whiteSpace_fun(type, v)
  end

  def facet_fun(type, {:maxInclusive, v}) do
    maxInclusive_fun(type, v)
  end

  def facet_fun(type, {:maxExclusive, v}) do
    maxExclusive_fun(type, v)
  end

  def facet_fun(type, {:minExclusive, v}) do
    minExclusive_fun(type, v)
  end

  def facet_fun(type, {:minInclusive, v}) do
    minInclusive_fun(type, v)
  end

  def facet_fun(type, {:totalDigits, v}) do
    totalDigits_fun(type, :erlang.list_to_integer(v))
  end

  def facet_fun(type, {:fractionDigits, v}) do
    fractionDigits_fun(type, :erlang.list_to_integer(v))
  end

  def facet_fun(type, f) do
    fn _X_ ->
      :error_logger.warning_msg('~w: not valid facet on ~p ~p~n', [:xmerl_xsd_type, type, f])
    end
  end

  defp length_fun(t, v)
       when t == :string or
              t == :normalizedString or t == :token or t == :Name or
              t == :NCName or t == :language or t == :ID or
              t == :IDREF or t == :IDREFS or t == :ENTITY or
              t == :ENTITIES or t == :NMTOKEN or t == :NMTOKENS or
              t == :anyURI do
    fn val ->
      case :string.len(val) == v do
        true ->
          {:ok, val}

        false ->
          {:error, {:length, :string.len(val), :should_be, v}}
      end
    end
  end

  defp length_fun(t, _V) when t == :NOTATION or t == :QName do
    fn val ->
      {:ok, val}
    end
  end

  defp length_fun(t, v)
       when t == :base64Binary or
              t == :hexBinary do
    fn val ->
      case length(val) == v do
        true ->
          {:ok, val}

        false ->
          {:error, {:length, length(val), :xhould_be, v}}
      end
    end
  end

  defp length_fun(t, _V) do
    fn _Val ->
      {:error, {:length_not_applicable_on, t}}
    end
  end

  defp minLength_fun(t, v)
       when t == :string or
              t == :normalizedString or t == :token or t == :Name or
              t == :NCName or t == :language or t == :ID or
              t == :IDREF or t == :IDREFS or t == :ENTITY or
              t == :ENTITIES or t == :NMTOKEN or t == :NMTOKENS or
              t == :anyURI do
    fn val ->
      case :string.len(val) >= v do
        true ->
          {:ok, val}

        false ->
          {:error, {:minLength, :string.len(val), :should_at_least_be, v}}
      end
    end
  end

  defp minLength_fun(t, _V) when t == :NOTATION or t == :QName do
    fn val ->
      {:ok, val}
    end
  end

  defp minLength_fun(t, v)
       when t == :base64Binary or
              t == :hexBinary do
    fn val ->
      case length(val) >= v do
        true ->
          {:ok, val}

        false ->
          {:error, {:minLength, length(val), :should_at_least_be, v}}
      end
    end
  end

  defp minLength_fun(t, _V) do
    fn _Val ->
      {:error, {:minLength_not_applicable_on, t}}
    end
  end

  defp maxLength_fun(t, v)
       when t == :string or
              t == :normalizedString or t == :token or t == :Name or
              t == :NCName or t == :language or t == :ID or
              t == :IDREF or t == :IDREFS or t == :ENTITY or
              t == :ENTITIES or t == :NMTOKEN or t == :NMTOKENS or
              t == :anyURI do
    fn val ->
      case length(val) do
        len when len <= v ->
          {:ok, val}

        _ ->
          {:error, {:maxLength, :string.len(val), :should_not_be_more_than, v}}
      end
    end
  end

  defp maxLength_fun(t, _V) when t == :NOTATION or t == :QName do
    fn val ->
      {:ok, val}
    end
  end

  defp maxLength_fun(t, v)
       when t == :base64Binary or
              t == :hexBinary do
    fn val ->
      case length(val) <= v do
        true ->
          {:ok, val}

        false ->
          {:error, {:maxLength, length(val), :should_not_be_more_than, v}}
      end
    end
  end

  defp maxLength_fun(t, _V) do
    fn _Val ->
      {:error, {:maxLength_not_applicable_on, t}}
    end
  end

  defp pattern_fun(_Type, regExp) do
    case :xmerl_regexp.setup(regExp) do
      {:ok, rE} ->
        fn val ->
          case :xmerl_regexp.first_match(val, rE) do
            {:match, _, _} ->
              {:ok, val}

            _ ->
              {:error, {:pattern_mismatch, val, regExp}}
          end
        end

      _ ->
        fn val ->
          {:error, {:unsupported_pattern, val, regExp}}
        end
    end
  end

  defp enumeration_fun(_Type, v) do
    fn val ->
      case :lists.member(val, v) do
        true ->
          {:ok, val}

        false ->
          {:error, {:enumeration, val, :should_be_one_of, v}}
      end
    end
  end

  defp whiteSpace_fun(_Type, 'preserve') do
    fn val ->
      {:ok, val}
    end
  end

  defp whiteSpace_fun(_Type, 'replace') do
    fn val ->
      {:ok, :xmerl_xsd_type.replace_ws(val, [])}
    end
  end

  defp whiteSpace_fun(_Type, 'collapse') do
    fn val ->
      {:ok, :xmerl_xsd_type.collapse_ws(val)}
    end
  end

  def replace_ws([9 | t], acc) do
    replace_ws(t, [32 | acc])
  end

  def replace_ws([10 | t], acc) do
    replace_ws(t, [32 | acc])
  end

  def replace_ws([13 | t], acc) do
    replace_ws(t, [32 | acc])
  end

  def replace_ws([h | t], acc) do
    replace_ws(t, [h | acc])
  end

  def replace_ws([], acc) do
    :lists.reverse(acc)
  end

  def collapse_ws(val) do
    collapse_ws(
      :lists.dropwhile(
        fn
          wS
          when wS == 32 or
                 wS == 9 or wS == 10 or wS == 13 ->
            true

          _ ->
            false
        end,
        replace_ws(val, [])
      ),
      []
    )
  end

  defp collapse_ws([[32, 32] | t], acc) do
    collapse_ws([32 | t], acc)
  end

  defp collapse_ws([h | t], acc) do
    collapse_ws(t, [h | acc])
  end

  defp collapse_ws([], acc) do
    :lists.reverse(
      :lists.dropwhile(
        fn
          ?\s ->
            true

          _ ->
            false
        end,
        acc
      )
    )
  end

  defp maxInclusive_fun(t, v)
       when t == :integer or
              t == :positiveInteger or t == :negativeInteger or
              t == :nonNegativeInteger or t == :nonPositiveInteger or
              t == :long or t == :unsignedLong or t == :int or
              t == :unsignedInt or t == :short or
              t == :unsignedShort or t == :byte or
              t == :unsignedByte do
    fn val ->
      case (try do
              :erlang.list_to_integer(val) <= :erlang.list_to_integer(v)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        true ->
          {:ok, val}

        _ ->
          {:error, {:maxInclusive, val, :should_be_less_than_or_equal_with, v}}
      end
    end
  end

  defp maxInclusive_fun(t, v)
       when t == :decimal or t == :float or
              t == :double do
    fn val ->
      case :xmerl_xsd_type.compare_floats(val, v) do
        :gt ->
          {:error, {:maxInclusive, val, :should_be_less_than_or_equal_with, v}}

        err = {:error, _} ->
          err

        _ ->
          {:ok, val}
      end
    end
  end

  defp maxInclusive_fun(t, v) when t == :duration do
    fn val ->
      case :xmerl_xsd_type.compare_durations(val, v) do
        gT when gT == :gt or gT == :indefinite ->
          {:error, {:maxInclusive, val, :should_be_less_than_or_equal_with, v}}

        _ ->
          {:ok, val}
      end
    end
  end

  defp maxInclusive_fun(t, v) when t == :dateTime do
    fn val ->
      case :xmerl_xsd_type.compare_dateTime(val, v) do
        gT when gT == :gt or gT == :indefinite ->
          {:error, {:maxInclusive, val, :should_be_less_than_or_equal_with, v}}

        _ ->
          {:ok, val}
      end
    end
  end

  defp maxInclusive_fun(t, _V) do
    fn _ ->
      {:error, {:maxInclusive, :not_implemented_for, t}}
    end
  end

  defp maxExclusive_fun(t, v)
       when t == :integer or
              t == :positiveInteger or t == :negativeInteger or
              t == :nonNegativeInteger or t == :nonPositiveInteger or
              t == :long or t == :unsignedLong or t == :int or
              t == :unsignedInt or t == :short or
              t == :unsignedShort or t == :byte or
              t == :unsignedByte do
    fn val ->
      case (try do
              :erlang.list_to_integer(val) < :erlang.list_to_integer(v)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        true ->
          {:ok, val}

        _ ->
          {:error, {:maxExclusive, val, :not_less_than, v}}
      end
    end
  end

  defp maxExclusive_fun(t, v)
       when t == :decimal or t == :float or
              t == :double do
    fn val ->
      case :xmerl_xsd_type.compare_floats(val, v) do
        :lt ->
          {:ok, val}

        err = {:error, _} ->
          err

        _ ->
          {:error, {:maxExclusive, val, :not_less_than, v}}
      end
    end
  end

  defp maxExclusive_fun(t, v) when t == :duration do
    fn val ->
      case :xmerl_xsd_type.compare_durations(val, v) do
        :lt ->
          {:ok, val}

        _ ->
          {:error, {:maxExclusive, val, :not_less_than, v}}
      end
    end
  end

  defp maxExclusive_fun(t, v) when t == :dateTime do
    fn val ->
      case :xmerl_xsd_type.compare_dateTime(val, v) do
        :lt ->
          {:ok, val}

        _ ->
          {:error, {:maxExclusive, val, :not_less_than, v}}
      end
    end
  end

  defp maxExclusive_fun(t, _V) do
    fn _ ->
      {:error, {:maxExclusive, :not_implemented_for, t}}
    end
  end

  defp minExclusive_fun(t, v)
       when t == :integer or
              t == :positiveInteger or t == :negativeInteger or
              t == :nonNegativeInteger or t == :nonPositiveInteger or
              t == :long or t == :unsignedLong or t == :int or
              t == :unsignedInt or t == :short or
              t == :unsignedShort or t == :byte or
              t == :unsignedByte do
    fn val ->
      case (try do
              :erlang.list_to_integer(val) > :erlang.list_to_integer(v)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        true ->
          {:ok, val}

        _ ->
          {:error, {:minExclusive, val, :not_greater_than, v}}
      end
    end
  end

  defp minExclusive_fun(t, v)
       when t == :decimal or t == :float or
              t == :double do
    fn val ->
      case :xmerl_xsd_type.compare_floats(val, v) do
        :gt ->
          {:ok, val}

        err = {:error, _} ->
          err

        _ ->
          {:error, {:minExclusive, val, :not_greater_than, v}}
      end
    end
  end

  defp minExclusive_fun(t, v) when t == :duration do
    fn val ->
      case :xmerl_xsd_type.compare_durations(val, v) do
        :gt ->
          {:ok, val}

        _ ->
          {:error, {:minExclusive, val, :not_greater_than, v}}
      end
    end
  end

  defp minExclusive_fun(t, v) when t == :dateTime do
    fn val ->
      case :xmerl_xsd_type.compare_dateTime(val, v) do
        :gt ->
          {:ok, val}

        _ ->
          {:error, {:minExclusive, val, :not_greater_than, v}}
      end
    end
  end

  defp minExclusive_fun(t, _V) do
    fn _ ->
      {:error, {:minExclusive, :not_implemented_for, t}}
    end
  end

  defp minInclusive_fun(t, v)
       when t == :integer or
              t == :positiveInteger or t == :negativeInteger or
              t == :nonNegativeInteger or t == :nonPositiveInteger or
              t == :long or t == :unsignedLong or t == :int or
              t == :unsignedInt or t == :short or
              t == :unsignedShort or t == :byte or
              t == :unsignedByte do
    fn val ->
      case (try do
              :erlang.list_to_integer(val) >= :erlang.list_to_integer(v)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        true ->
          {:ok, val}

        _ ->
          {:error, {:minInclusive, val, :not_greater_than_or_equal_with, v}}
      end
    end
  end

  defp minInclusive_fun(t, v)
       when t == :decimal or t == :float or
              t == :double do
    fn val ->
      case :xmerl_xsd_type.compare_floats(val, v) do
        :lt ->
          {:error, {:minInclusive, val, :not_greater_than_or_equal_with, v}}

        err = {:error, _} ->
          err

        _ ->
          {:ok, val}
      end
    end
  end

  defp minInclusive_fun(t, v) when t == :duration do
    fn val ->
      case :xmerl_xsd_type.compare_durations(val, v) do
        :lt ->
          {:error, {:minInclusive, val, :not_greater_than_or_equal_with, v}}

        _ ->
          {:ok, val}
      end
    end
  end

  defp minInclusive_fun(t, v) when t == :dateTime do
    fn val ->
      case :xmerl_xsd_type.compare_dateTime(val, v) do
        :lt ->
          {:error, {:minInclusive, val, :not_greater_than_or_equal_with, v}}

        _ ->
          {:ok, val}
      end
    end
  end

  defp minInclusive_fun(t, _V) do
    fn _ ->
      {:error, {:minInclusive, :not_implemented_for, t}}
    end
  end

  defp totalDigits_fun(t, v)
       when t == :integer or
              t == :positiveInteger or t == :negativeInteger or
              t == :nonNegativeInteger or t == :nonPositiveInteger or
              t == :long or t == :unsignedLong or t == :int or
              t == :unsignedInt or t == :short or
              t == :unsignedShort or t == :byte or
              t == :unsignedByte or t == :decimal do
    fn val ->
      pred = fn
        ?0 ->
          true

        _ ->
          false
      end

      val2 = :lists.dropwhile(pred, val)

      length =
        case :lists.member(?., val2) do
          true ->
            length(:lists.dropwhile(pred, :lists.reverse(val2))) - 1

          _ ->
            length(val2)
        end

      cond do
        length <= v ->
          {:ok, val}

        true ->
          {:error, {:totalDigits, length, :to_many_digits}}
      end
    end
  end

  defp totalDigits_fun(t, _V) do
    fn _ ->
      {:error, {:totalDigits, :not_applicable, t}}
    end
  end

  defp fractionDigits_fun(t, v)
       when t == :integer or
              t == :positiveInteger or t == :negativeInteger or
              t == :nonNegativeInteger or t == :nonPositiveInteger or
              t == :long or t == :unsignedLong or t == :int or
              t == :unsignedInt or t == :short or
              t == :unsignedShort or t == :byte or
              t == :unsignedByte or t == :decimal do
    fn val ->
      len =
        case :string.tokens(val, '.') do
          [_I, frc] when t == :decimal ->
            pred = fn
              ?0 ->
                true

              _ ->
                false
            end

            length(:lists.dropwhile(pred, :lists.reverse(frc)))

          _ ->
            0
        end

      cond do
        len <= v ->
          {:ok, val}

        true ->
          {:error, {:fractionDigits, len, :to_many_digits_in, val}}
      end
    end
  end

  defp fractionDigits_fun(t, _V) do
    fn _ ->
      {:error, {:fractionDigits, :not_applicable, t}}
    end
  end

  def compare_floats(f1, f2) when f1 == 'NaN' or f2 == 'NaN' do
    {:error, {:not_comparable}}
  end

  def compare_floats(f1, f1) do
    :eq
  end

  def compare_floats(f1, f2) when f1 == 'INF' or f2 == '-INF' do
    :gt
  end

  def compare_floats(f1, f2) when f1 == '-INF' or f2 == 'INF' do
    :lt
  end

  def compare_floats(str1, str2) do
    f1 = {s1, _B1, _D1, _E1} = str_to_float(str1)
    f2 = {s2, _B2, _D2, _E2} = str_to_float(str2)

    cond do
      s1 == :- and s2 == :+ ->
        :lt

      s1 == :+ and s2 == :- ->
        :gt

      true ->
        compare_floats2(f1, f2)
    end
  end

  defp compare_floats2({s1, b1, d1, e1}, {_S2, b2, d2, e2})
       when b1 == 0 or b2 == 0 do
    i1 = pow(b1, d1, e1)
    i2 = pow(b2, d2, e2)

    cond do
      i1 > i2 ->
        sign(s1, :gt)

      i1 < i2 ->
        sign(s1, :lt)

      true ->
        :eq
    end
  end

  defp compare_floats2({s1, b1, d1, e1}, {_S2, b2, d2, e2}) do
    i1 = pow(b1, e1)
    i2 = pow(b2, e2)

    cond do
      i1 > i2 ->
        sign(s1, :gt)

      i1 < i2 ->
        sign(s1, :lt)

      true ->
        cond do
          d1 == d2 ->
            :eq

          d1 < d2 ->
            sign(s1, :lt)

          d1 > d2 ->
            sign(s1, :gt)
        end
    end
  end

  defp str_to_float(string) do
    {sign, str} =
      case string do
        '-' ++ str1 ->
          {:-, str1}

        _ ->
          {:+, string}
      end

    case :string.tokens(str, '.') do
      [b, dE] ->
        case :string.tokens(dE, 'Ee') do
          [d, e] ->
            {sign, :erlang.list_to_integer(b), remove_trailing_zeros(d),
             :erlang.list_to_integer(e)}

          [d] ->
            {sign, :erlang.list_to_integer(b), remove_trailing_zeros(d), 0}
        end

      [b] ->
        case :string.tokens(str, 'Ee') do
          [i, e] ->
            {sign, :erlang.list_to_integer(i), '0', :erlang.list_to_integer(e)}

          _ ->
            {sign, :erlang.list_to_integer(b), '0', 0}
        end
    end
  end

  defp pow(mantissa, exponent) do
    case mantissa * :math.pow(10, exponent) do
      i when i < 1 ->
        i

      i ->
        round(i)
    end
  end

  defp pow(mantissa, fraction, exponent) do
    mantissa *
      :math.pow(
        10,
        exponent
      ) +
      :erlang.list_to_integer(fraction) *
        :math.pow(
          10,
          exponent - length(fraction)
        )
  end

  defp sign(:-, :gt) do
    :lt
  end

  defp sign(:-, :lt) do
    :gt
  end

  defp sign(_, rel) do
    rel
  end

  defp remove_trailing_zeros(str) do
    pred = fn
      ?0 ->
        true

      _ ->
        false
    end

    case :lists.reverse(
           :lists.dropwhile(
             pred,
             :lists.reverse(str)
           )
         ) do
      [] ->
        '0'

      fr ->
        fr
    end
  end

  def compare_durations(v1, v2) do
    ref1_dateTime = {1696, 9, 1, 0, 0, 0, {:pos, 0, 0}}
    ref2_dateTime = {1697, 2, 1, 0, 0, 0, {:pos, 0, 0}}
    ref3_dateTime = {1903, 3, 1, 0, 0, 0, {:pos, 0, 0}}
    ref4_dateTime = {1903, 7, 1, 0, 0, 0, {:pos, 0, 0}}

    cmpRes1 =
      compare_dateTime(
        normalize_dateTime(
          add_duration2dateTime(
            ref1_dateTime,
            v1
          )
        ),
        normalize_dateTime(
          add_duration2dateTime(
            ref1_dateTime,
            v2
          )
        )
      )

    cmpRes2 =
      compare_dateTime(
        normalize_dateTime(
          add_duration2dateTime(
            ref2_dateTime,
            v1
          )
        ),
        normalize_dateTime(
          add_duration2dateTime(
            ref2_dateTime,
            v2
          )
        )
      )

    cmpRes3 =
      compare_dateTime(
        normalize_dateTime(
          add_duration2dateTime(
            ref3_dateTime,
            v1
          )
        ),
        normalize_dateTime(
          add_duration2dateTime(
            ref3_dateTime,
            v2
          )
        )
      )

    cmpRes4 =
      compare_dateTime(
        normalize_dateTime(
          add_duration2dateTime(
            ref4_dateTime,
            v1
          )
        ),
        normalize_dateTime(
          add_duration2dateTime(
            ref4_dateTime,
            v2
          )
        )
      )

    cond do
      cmpRes1 == cmpRes2 and cmpRes1 == cmpRes3 and
          cmpRes1 == cmpRes4 ->
        cmpRes1

      true ->
        :indefinite
    end
  end

  def compare_dateTime(
        dT1 = {_, _, _, _, _, _, z},
        dT2 = {_, _, _, _, _, _, z}
      ) do
    case dT1 < dT2 do
      true ->
        :lt

      _ ->
        case dT1 > dT2 do
          true ->
            :gt

          _ ->
            :eq
        end
    end
  end

  def compare_dateTime(
        p = {_, _, _, _, _, _, {_, _, _}},
        _Q = {y, m, d, h, min, s, :none}
      ) do
    case compare_dateTime(
           p,
           normalize_dateTime({y, m, d, h, min, s, {:pos, 14, 0}})
         ) do
      :lt ->
        :lt

      _ ->
        case compare_dateTime(
               p,
               normalize_dateTime({y, m, d, h, min, s, {:neg, 14, 0}})
             ) do
          :gt ->
            :gt

          _ ->
            :indefinite
        end
    end
  end

  def compare_dateTime(
        _P = {y, m, d, h, min, s, :none},
        q = {_, _, _, _, _, _, {_, _, _}}
      ) do
    case compare_dateTime(
           normalize_dateTime({y, m, d, h, min, s, {:neg, 14, 0}}),
           q
         ) do
      :lt ->
        :lt

      _ ->
        case compare_dateTime(
               normalize_dateTime({y, m, d, h, min, s, {:pos, 14, 0}}),
               q
             ) do
          :gt ->
            :gt

          _ ->
            :indefinite
        end
    end
  end

  def compare_dateTime(p, q) when is_list(p) do
    compare_dateTime(
      normalize_dateTime(dateTime_atoms(p)),
      q
    )
  end

  def compare_dateTime(p, q) when is_list(q) do
    compare_dateTime(
      p,
      normalize_dateTime(dateTime_atoms(q))
    )
  end

  def compare_dateTime(_P, _Q) do
    :indefinite
  end

  def fQuotient(a, b) when is_float(a) do
    fQuotient(:erlang.floor(a), b)
  end

  def fQuotient(a, b) when is_float(b) do
    fQuotient(a, :erlang.floor(b))
  end

  def fQuotient(a, b) when a >= 0 and b >= 0 do
    div(a, b)
  end

  def fQuotient(a, b) when a < 0 and b < 0 do
    div(a, b)
  end

  def fQuotient(a, b) do
    case rem(a, b) do
      0 ->
        div(a, b)

      _ ->
        div(a, b) - 1
    end
  end

  def fQuotient(a, low, high) do
    fQuotient(a - low, high - low)
  end

  def modulo(a, b) do
    a - fQuotient(a, b) * b
  end

  def modulo(a, low, high) do
    modulo(a - low, high - low) + low
  end

  def maximumDayInMonthFor(yearValue, monthValue) do
    m = modulo(monthValue, 1, 13)
    y = yearValue + fQuotient(monthValue, 1, 13)
    monthValue(m, y)
  end

  defp monthValue(m, _Y)
       when m == 1 or m == 3 or m == 5 or
              m == 7 or m == 8 or m == 10 or m == 12 do
    31
  end

  defp monthValue(m, _Y)
       when m == 4 or m == 6 or m == 9 or
              m == 11 do
    30
  end

  defp monthValue(_M, y) do
    case modulo(y, 400) do
      0 ->
        29

      _ ->
        case {modulo(y, 100) != 0, modulo(y, 4)} do
          {true, 0} ->
            29

          _ ->
            28
        end
    end
  end

  def add_duration2dateTime(s, d) when is_list(s) and is_list(d) do
    satoms = dateTime_atoms(s)

    case duration_atoms(d) do
      datoms = {_, _, _, _, _, _} ->
        add_duration2dateTime2(satoms, datoms)

      err ->
        {:error, err}
    end
  end

  def add_duration2dateTime(s = {_, _, _, _, _, _, _}, d) do
    case duration_atoms(d) do
      datoms = {_, _, _, _, _, _} ->
        add_duration2dateTime2(s, datoms)

      err ->
        {:error, err}
    end
  end

  defp add_duration2dateTime2(
         {syear, smonth, sday, shour, sminute, ssec, szone},
         {dyears, dmonths, ddays, dhours, dminutes, dsecs}
       ) do
    temp1 = smonth + dmonths
    emonth = modulo(temp1, 1, 13)
    carry1 = fQuotient(temp1, 1, 13)
    eyear = syear + dyears + carry1
    temp2 = ssec + dsecs
    esecs = modulo(temp2, 60)
    carry2 = fQuotient(temp2, 60)
    temp3 = sminute + dminutes + carry2
    eminute = modulo(temp3, 60)
    carry3 = fQuotient(temp3, 60)
    temp4 = shour + dhours + carry3
    ehour = modulo(temp4, 24)
    carry4 = fQuotient(temp4, 24)

    tempDays =
      case maximumDayInMonthFor(
             eyear,
             emonth
           ) do
        maxDay when sday > maxDay ->
          maxDay

        _ ->
          case sday < 1 do
            true ->
              1

            _ ->
              sday
          end
      end

    {eyear2, emonth2, eday} = carry_loop(tempDays + ddays + carry4, emonth, eyear)
    {eyear2, emonth2, eday, ehour, eminute, esecs, szone}
  end

  defp carry_loop(eday, emonth, eyear) when eday < 1 do
    carry_loop(
      eday +
        maximumDayInMonthFor(
          eyear,
          emonth - 1
        ),
      modulo(emonth - 1, 1, 13),
      eyear + fQuotient(emonth - 1, 1, 13)
    )
  end

  defp carry_loop(eday, emonth, eyear) do
    case maximumDayInMonthFor(eyear, emonth) do
      maxD when eday > maxD ->
        carry_loop(
          eday - maximumDayInMonthFor(eyear, emonth),
          modulo(emonth + 1, 1, 13),
          eyear + fQuotient(emonth + 1, 1, 13)
        )

      _ ->
        {eyear, emonth, eday}
    end
  end

  def dateTime_atoms('-' ++ dT) do
    dateTime_atoms(dT, :neg)
  end

  def dateTime_atoms(dT) do
    dateTime_atoms(dT, :pos)
  end

  defp dateTime_atoms(s, sign) do
    [date, timeZone] = :string.tokens(s, 'T')
    [yY, mM, dD] = :string.tokens(date, '-')

    {zone, zoneSign, [hour, min, sec]} =
      case :lists.reverse(timeZone) do
        'Z' ++ _ ->
          {'Z', :pos, :string.tokens(timeZone, 'Z:')}

        _ ->
          zS = zone_sign(timeZone)

          case :string.tokens(timeZone, '-+') do
            [time, z] ->
              {z, zS, :string.tokens(time, ':')}

            [time] ->
              {:none, zS, :string.tokens(time, ':')}
          end
      end

    {set_sign(sign, yY), :erlang.list_to_integer(mM), :erlang.list_to_integer(dD),
     :erlang.list_to_integer(hour), :erlang.list_to_integer(min), sign_sec(:pos, sec),
     zone_atoms(zoneSign, zone)}
  end

  defp zone_sign(timeZone) do
    case :lists.member(?-, timeZone) do
      true ->
        :neg

      _ ->
        :pos
    end
  end

  defp zone_atoms(_Sign, 'Z') do
    {:pos, 0, 0}
  end

  defp zone_atoms(sign, zone) when is_list(zone) do
    case :string.tokens(zone, ':') do
      [h, m] ->
        {sign, :erlang.list_to_integer(h), :erlang.list_to_integer(m)}

      _ ->
        :none
    end
  end

  defp zone_atoms(_Sign, zone) do
    zone
  end

  def duration_atoms('-P' ++ dur) do
    duration_atoms2(dur, :neg)
  end

  def duration_atoms('P' ++ dur) do
    duration_atoms2(dur, :pos)
  end

  def duration_atoms(dur) do
    {:illegal_duration, dur}
  end

  defp duration_atoms2(dur, sign) do
    case :lists.member(?T, dur) do
      true ->
        case :string.tokens(dur, 'T') do
          [date, time] ->
            case duration_atoms_date(date) do
              {y, m, d} ->
                case duration_atoms_time(time) do
                  {hour, min, sec} ->
                    {set_sign(sign, y), set_sign(sign, m), set_sign(sign, d),
                     set_sign(sign, hour), set_sign(sign, min), sign_sec(sign, sec)}

                  err ->
                    err
                end

              err ->
                err
            end

          [time] ->
            case duration_atoms_time(time) do
              {hour, min, sec} ->
                {0, 0, 0, set_sign(sign, hour), set_sign(sign, min), sign_sec(sign, sec)}

              err ->
                err
            end

          err ->
            {:illegal_duration, err}
        end

      _ ->
        {y, m, d} = duration_atoms_date(dur)
        {set_sign(sign, y), set_sign(sign, m), set_sign(sign, d), 0, 0, 0}
    end
  end

  defp duration_atoms_date(date) do
    {y, date2} = get_digit(date, ?Y)
    {m, date3} = get_digit(date2, ?M)
    {d, rest} = get_digit(date3, ?D)

    case rest do
      '' ->
        {y, m, d}

      err ->
        {:illegal_duration, err}
    end
  end

  defp duration_atoms_time(time) do
    {h, time2} = get_digit(time, ?H)
    {m, time3} = get_digit(time2, ?M)
    {s, rest} = get_sec(time3)

    case rest do
      '' ->
        {h, m, s}

      err ->
        {:illegal_duration, err}
    end
  end

  defp get_digit(str, delim) do
    get_digit(str, delim, [], str)
  end

  defp get_digit([delim | t], delim, acc, _Str) do
    {:lists.reverse(acc), t}
  end

  defp get_digit([h | t], delim, acc, str)
       when h >= ?0 and
              h <= ?9 do
    get_digit(t, delim, [h | acc], str)
  end

  defp get_digit([], _, [], _Str) do
    {'0', []}
  end

  defp get_digit([], _, _, str) do
    {'0', str}
  end

  defp get_digit(_, _, _, str) do
    {'0', str}
  end

  defp get_sec([]) do
    {'0', []}
  end

  defp get_sec(str) do
    get_sec(str, [], str)
  end

  defp get_sec([h | t], acc, str) when h >= ?0 and h <= ?9 do
    get_sec(t, [h | acc], str)
  end

  defp get_sec([?. | t], acc, str) do
    get_sec(t, [?. | acc], str)
  end

  defp get_sec([?S | t], acc, _) do
    {:lists.reverse(acc), t}
  end

  defp get_sec(_, _, str) do
    {'0', str}
  end

  defp set_sign(:pos, istr) do
    :erlang.list_to_integer(istr)
  end

  defp set_sign(_, istr) do
    :erlang.list_to_integer('-' ++ istr)
  end

  defp sign_sec(:pos, sec) do
    case :lists.member(?., sec) do
      true ->
        :erlang.list_to_float(sec)

      _ ->
        :erlang.list_to_integer(sec)
    end
  end

  defp sign_sec(_, sec) do
    sign_sec(:pos, '-' ++ sec)
  end

  defp invert_sign(:pos) do
    :neg
  end

  defp invert_sign(:neg) do
    :pos
  end

  defp invert_sign(s) do
    s
  end

  def normalize_dateTime({y, m, d, hour, min, sec, {sign, zH, zM}}) do
    tmpMin =
      min +
        set_sign(
          invert_sign(sign),
          :erlang.integer_to_list(zM)
        )

    nMin = modulo(tmpMin, 60)
    carry1 = fQuotient(tmpMin, 60)

    tmpHour =
      hour +
        set_sign(
          invert_sign(sign),
          :erlang.integer_to_list(zH)
        ) + carry1

    nHour = modulo(tmpHour, 24)
    carry2 = fQuotient(tmpHour, 24)
    {nY, nM, nD} = carry_loop(d + carry2, m, y)
    {nY, nM, nD, nHour, nMin, sec, {:pos, 0, 0}}
  end

  def normalize_dateTime(dT) do
    dT
  end
end
