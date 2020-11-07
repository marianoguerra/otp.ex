defmodule :m_xmerl_xpath_scan do
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

  def tokens(str) do
    tokens(strip_ws(str), [])
  end

  defp tokens([], acc) do
    :lists.reverse([{:"$end", 1, :"$end"} | acc])
  end

  defp tokens(str, acc) do
    case scan_token(str, acc) do
      {:rescan, newStr} ->
        tokens(newStr, acc)

      {token, t} ->
        tokens(strip_ws(t), [token | acc])
    end
  end

  defp scan_token('(' ++ t, _A) do
    {{:"(", 1, :"("}, t}
  end

  defp scan_token(')' ++ t, _A) do
    {{:")", 1, :")"}, t}
  end

  defp scan_token('[' ++ t, _A) do
    {{:"[", 1, :"["}, t}
  end

  defp scan_token(']' ++ t, _A) do
    {{:"]", 1, :"]"}, t}
  end

  defp scan_token('..' ++ t, _A) do
    {:rescan, 'parent::node()' ++ t}
  end

  defp scan_token('@' ++ t, _A) do
    {:rescan, 'attribute::' ++ t}
  end

  defp scan_token(',' ++ t, _A) do
    {{:",", 1, :","}, t}
  end

  defp scan_token('::' ++ t, _A) do
    {{:"::", 1, :"::"}, t}
  end

  defp scan_token('//' ++ t, _A) do
    {:rescan, '/descendant-or-self::node()/' ++ t}
  end

  defp scan_token('/' ++ t, _A) do
    {{:/, 1, :/}, t}
  end

  defp scan_token('|' ++ t, _A) do
    {{:|, 1, :|}, t}
  end

  defp scan_token('+' ++ t, _A) do
    {{:+, 1, :+}, t}
  end

  defp scan_token('-' ++ t, _A) do
    {{:-, 1, :-}, t}
  end

  defp scan_token('=' ++ t, _A) do
    {{:=, 1, :=}, t}
  end

  defp scan_token('!=' ++ t, _A) do
    {{:!=, 1, :!=}, t}
  end

  defp scan_token('<=' ++ t, _A) do
    {{:<=, 1, :<=}, t}
  end

  defp scan_token('<' ++ t, _A) do
    {{:<, 1, :<}, t}
  end

  defp scan_token('>=' ++ t, _A) do
    {{:>=, 1, :>=}, t}
  end

  defp scan_token('>' ++ t, _A) do
    {{:>, 1, :>}, t}
  end

  defp scan_token('*' ++ t, a) do
    tok =
      case a do
        [{x, _, _} | _] ->
          case special_token(x) do
            false ->
              {:*, 1, :*}

            true ->
              {:wildcard, 1, :wildcard}
          end

        _ ->
          {:wildcard, 1, :wildcard}
      end

    {tok, t}
  end

  defp scan_token(str = [h | _], _A) when h >= ?0 and h <= ?9 do
    scan_number(str)
  end

  defp scan_token(str = [[?., h] | _], a)
       when h >= ?0 and
              h <= ?9 do
    scan_number(str, a)
  end

  defp scan_token('.' ++ t, _A) do
    {:rescan, 'self::node()' ++ t}
  end

  defp scan_token([?$ | t], _A) do
    {{prefix, local}, t1} = scan_name(t)

    case prefix do
      [] ->
        {{:var_reference, 1, :erlang.list_to_atom(local)}, t1}

      _ ->
        {{:var_reference, 1, :erlang.list_to_atom(prefix ++ ':' ++ local)}, t1}
    end
  end

  defp scan_token([h | t], _A) when h == ?" or h == ?' do
    {literal, t1} = scan_literal(t, h, [])
    {{:literal, 1, literal}, t1}
  end

  defp scan_token(t, a) do
    {{prefix, local}, t1} = scan_name(t)

    case a do
      [{x, _, _} | _] ->
        case special_token(x) do
          false ->
            operator_name(prefix, local, t1)

          true ->
            other_name(prefix, local, strip_ws(t1))
        end

      _ ->
        other_name(prefix, local, t1)
    end
  end

  defp operator_name([], 'and', t) do
    {{:and, 1, :and}, t}
  end

  defp operator_name([], 'or', t) do
    {{:or, 1, :or}, t}
  end

  defp operator_name([], 'mod', t) do
    {{:mod, 1, :mod}, t}
  end

  defp operator_name([], 'div', t) do
    {{:div, 1, :div}, t}
  end

  defp other_name(prefix, [], '*' ++ t) do
    {{:prefix_test, 1, prefix}, t}
  end

  defp other_name(prefix, local, t = '(' ++ _) do
    node_type_or_function_name(prefix, local, t)
  end

  defp other_name(prefix, local, t = '::' ++ _) do
    axis(prefix, local, t)
  end

  defp other_name([], local, t) do
    {{:name, 1, {:erlang.list_to_atom(local), [], local}}, t}
  end

  defp other_name(prefix, local, t) do
    {{:name, 1, {:erlang.list_to_atom(prefix ++ ':' ++ local), prefix, local}}, t}
  end

  defp node_type_or_function_name([], 'comment', t) do
    {{:node_type, 1, :comment}, t}
  end

  defp node_type_or_function_name([], 'text', t) do
    {{:node_type, 1, :text}, t}
  end

  defp node_type_or_function_name([], 'processing-instruction', t) do
    {{:"processing-instruction", 1, :"processing-instruction"}, t}
  end

  defp node_type_or_function_name([], 'node', t) do
    {{:node_type, 1, :node}, t}
  end

  defp node_type_or_function_name(prefix, local, t) do
    {{:function_name, 1, :erlang.list_to_atom(prefix ++ local)}, t}
  end

  defp axis([], 'ancestor-or-self', t) do
    {{:axis, 1, :ancestor_or_self}, t}
  end

  defp axis([], 'ancestor', t) do
    {{:axis, 1, :ancestor}, t}
  end

  defp axis([], 'attribute', t) do
    {{:axis, 1, :attribute}, t}
  end

  defp axis([], 'child', t) do
    {{:axis, 1, :child}, t}
  end

  defp axis([], 'descendant-or-self', t) do
    {{:axis, 1, :descendant_or_self}, t}
  end

  defp axis([], 'descendant', t) do
    {{:axis, 1, :descendant}, t}
  end

  defp axis([], 'following-sibling', t) do
    {{:axis, 1, :following_sibling}, t}
  end

  defp axis([], 'following', t) do
    {{:axis, 1, :following}, t}
  end

  defp axis([], 'namespace', t) do
    {{:axis, 1, :namespace}, t}
  end

  defp axis([], 'parent', t) do
    {{:axis, 1, :parent}, t}
  end

  defp axis([], 'preceding-sibling', t) do
    {{:axis, 1, :preceding_sibling}, t}
  end

  defp axis([], 'preceding', t) do
    {{:axis, 1, :preceding}, t}
  end

  defp axis([], 'self', t) do
    {{:axis, 1, :self}, t}
  end

  defp scan_literal([h | t], h, acc) do
    {:lists.reverse(acc), t}
  end

  defp scan_literal([h | t], delim, acc) do
    scan_literal(t, delim, [h | acc])
  end

  defp scan_name([[h1, h2] | t]) when h1 == ?: or h1 == ?_ do
    cond do
      h2 == 32 or h2 == 13 or h2 == 10 or h2 == 9 ->
        exit({:invalid_name, [h1, h2, :...]})

      true ->
        scan_prefix(t, [h2, h1])
    end
  end

  defp scan_name([h | t]) do
    case :xmerl_lib.is_letter(h) do
      true ->
        scan_prefix(t, [h])

      false ->
        exit({:invalid_name, :lists.sublist([h | t], 1, 6)})
    end
  end

  defp scan_name(str) do
    exit({:invalid_name, :lists.sublist(str, 1, 6)})
  end

  defp scan_prefix([], acc) do
    {{[], :lists.reverse(acc)}, []}
  end

  defp scan_prefix(str = [h | _], acc)
       when h == 32 or h == 13 or
              h == 10 or h == 9 do
    {{[], :lists.reverse(acc)}, str}
  end

  defp scan_prefix(t = '::' ++ _, acc) do
    {{[], :lists.reverse(acc)}, t}
  end

  defp scan_prefix(':' ++ t, acc) do
    {localPart, t1} = scan_local_part(t, [])
    prefix = :lists.reverse(acc)
    {{prefix, localPart}, t1}
  end

  defp scan_prefix(str = [h | t], acc) do
    case :xmerl_lib.is_namechar(h) do
      true ->
        scan_prefix(t, [h | acc])

      false ->
        {{[], :lists.reverse(acc)}, str}
    end
  end

  defp scan_local_part([], acc) do
    {:lists.reverse(acc), []}
  end

  defp scan_local_part(str = [h | _], acc)
       when h == 32 or h == 13 or
              h == 10 or h == 9 do
    {:lists.reverse(acc), str}
  end

  defp scan_local_part(str = [h | t], acc) do
    case :xmerl_lib.is_namechar(h) do
      true ->
        scan_local_part(t, [h | acc])

      false ->
        {:lists.reverse(acc), str}
    end
  end

  def scan_number(t) do
    scan_number(t, [])
  end

  defp scan_number([], acc) do
    {{:number, 1, :erlang.list_to_integer(:lists.reverse(acc))}, []}
  end

  defp scan_number('.' ++ t, []) do
    {digits, t1} = scan_digits(t, '.0')
    number = :erlang.list_to_float(digits)
    {{:number, 1, number}, t1}
  end

  defp scan_number('.' ++ t, acc) do
    {digits, t1} = scan_digits(t, '.' ++ acc)
    number = :erlang.list_to_float(digits)
    {{:number, 1, number}, t1}
  end

  defp scan_number([h | t], acc) when h >= ?0 and h <= ?9 do
    scan_number(t, [h | acc])
  end

  defp scan_number(t, acc) do
    {{:number, 1, :erlang.list_to_integer(:lists.reverse(acc))}, t}
  end

  defp scan_digits([], acc) do
    {:lists.reverse(acc), []}
  end

  defp scan_digits([h | t], acc) when h >= ?0 and h <= ?9 do
    scan_digits(t, [h | acc])
  end

  defp scan_digits(t, acc) do
    {:lists.reverse(acc), t}
  end

  defp strip_ws([h | t])
       when h == 32 or h == 13 or h == 10 or
              h == 9 do
    strip_ws(t)
  end

  defp strip_ws(t) do
    t
  end

  defp special_token(:"::") do
    true
  end

  defp special_token(:",") do
    true
  end

  defp special_token(:"(") do
    true
  end

  defp special_token(:"[") do
    true
  end

  defp special_token(:/) do
    true
  end

  defp special_token(:|) do
    true
  end

  defp special_token(:+) do
    true
  end

  defp special_token(:-) do
    true
  end

  defp special_token(:=) do
    true
  end

  defp special_token(:!=) do
    true
  end

  defp special_token(:<) do
    true
  end

  defp special_token(:<=) do
    true
  end

  defp special_token(:>) do
    true
  end

  defp special_token(:>=) do
    true
  end

  defp special_token(:and) do
    true
  end

  defp special_token(:or) do
    true
  end

  defp special_token(:mod) do
    true
  end

  defp special_token(:div) do
    true
  end

  defp special_token(_) do
    false
  end
end
