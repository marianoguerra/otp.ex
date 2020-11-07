defmodule :m_xmerl_validate do
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

  def validate(
        r_xmerl_scanner(doctype_name: dTName, doctype_DTD: opProv),
        r_xmlElement(name: name)
      )
      when dTName !== name and
             opProv !== :option_provided do
    {:error, {:mismatched_root_element, name, dTName}}
  end

  def validate(r_xmerl_scanner(rules: rules) = s, xML = r_xmlElement(name: name)) do
    try do
      do_validation(read_rules(rules, name), xML, rules, s)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  def validate(_, xML) do
    {:error, {:no_xml_element, xML}}
  end

  defp do_validation(:undefined, r_xmlElement(name: name), _Rules, _S) do
    {:error, {:unknown_element, name}}
  end

  defp do_validation(el_Rule, xML, rules, s) do
    case (try do
            valid_attributes(
              r_xmlElement(el_Rule, :attributes),
              r_xmlElement(xML, :attributes),
              s
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, reason} ->
        {:error, reason}

      {:error, reason} ->
        {:error, reason}

      attr_2 ->
        el_Rule_Cont = r_xmlElement(el_Rule, :content)
        wSActionMode = ws_action_mode(r_xmlElement(el_Rule, :elementdef), el_Rule_Cont, s)
        xML_Cont = r_xmlElement(xML, :content)
        check_direct_ws_SDD(xML_Cont, wSActionMode)

        case valid_contents(el_Rule_Cont, xML_Cont, rules, s, wSActionMode) do
          {:error, reason} ->
            {:error, reason}

          {:error, reason, n} ->
            {:error, reason, n}

          xMLS ->
            r_xmlElement(xML, attributes: attr_2, content: xMLS)
        end
    end
  end

  defp check_direct_ws_SDD(xML, :always_preserve) do
    case xML do
      [r_xmlText() | _Rest] ->
        exit({:error, {:illegal_whitespace_standalone_doc, xML}})

      _ ->
        :ok
    end

    case :lists.reverse(xML) do
      [r_xmlText() | _Rest2] ->
        exit({:error, {:illegal_whitespace_standalone_doc, xML}})

      _ ->
        :ok
    end
  end

  defp check_direct_ws_SDD(_, _) do
    :ok
  end

  defp ws_action_mode({:external, _}, content, r_xmerl_scanner(standalone: :yes)) do
    case element_content(content) do
      :children ->
        :always_preserve

      _ ->
        :preserve
    end
  end

  defp ws_action_mode(_, _, _) do
    :preserve
  end

  defp element_content(a)
       when is_atom(a) and a != :any and
              a != :empty do
    :children
  end

  defp element_content({:choice, l}) when is_list(l) do
    element_content(l)
  end

  defp element_content({:seq, l}) when is_list(l) do
    element_content(l)
  end

  defp element_content([:"#PCDATA" | _T]) do
    :mixed
  end

  defp element_content(:"#PCDATA") do
    :mixed
  end

  defp element_content({:*, rest}) do
    element_content(rest)
  end

  defp element_content(_) do
    :children
  end

  defp read_rules(_, :pcdata) do
    :pcdata
  end

  defp read_rules(t, name) do
    case :ets.lookup(t, {:elem_def, name}) do
      [] ->
        :undefined

      [{_K, v}] ->
        v
    end
  end

  defp valid_attributes(all_Attr, [r_xmlAttribute() | _T] = attr, s) do
    single_ID_definition(all_Attr)
    vc_Name_Token_IDREFS(all_Attr, attr)

    :lists.foreach(
      fn r_xmlAttribute(name: name) ->
        case is_attribute_exist(name, all_Attr) do
          true ->
            :ok

          false ->
            exit({:error, {:attribute_unknown, name}})
        end
      end,
      attr
    )

    :lists.flatten(
      :lists.foldl(
        fn {name, dataType, iF, defDecl, env}, attr_2 ->
          attr_2 ++ [valid_attribute(name, dataType, iF, defDecl, attr, env, s)]
        end,
        [],
        all_Attr
      )
    )
  end

  defp valid_attributes([], [], _) do
    []
  end

  defp valid_attributes(all_Attr, [], s) do
    single_ID_definition(all_Attr)

    :lists.flatten(
      :lists.foldl(
        fn {name, dataType, iF, defDecl, env}, attr_2 ->
          attr_2 ++ [valid_attribute(name, dataType, iF, defDecl, [], env, s)]
        end,
        [],
        all_Attr
      )
    )
  end

  defp valid_attribute(name, dataType, iF, defaultDecl, list_of_Attributes, env, s) do
    sA = r_xmerl_scanner(s, :standalone)
    attr = search_attr(name, list_of_Attributes)
    check_SDD_validity(sA, env, attr, iF)

    case {defaultDecl, iF, attr} do
      {:"#REQUIRED", _, :no_attribute} ->
        exit({:error, {name, :is_required}})

      {:"#IMPLIED", _, :no_attribute} ->
        []

      {:"#FIXED", defVal, r_xmlAttribute(value: defVal) = ^attr} ->
        attr

      {:"#FIXED", a, :no_attribute} ->
        r_xmlAttribute(name: name, value: a)

      {:"#FIXED", a, b} ->
        exit({:error, {:fixed_default_value_missmatch, a, b}})

      {_, value, :no_attribute} when is_list(value) ->
        r_xmlAttribute(name: name, value: value)

      {_, _, r_xmlAttribute() = ^attr} ->
        test_attribute_value(dataType, attr, iF, s)

      {defDecl, else__, xML} ->
        exit({:error, {:unknow_attribute_type, defDecl, else__, xML}})
    end
  end

  defp vc_Name_Token_IDREFS([{name, type, _, _, _} | rest], attrs)
       when type == :NMTOKEN or type == :NMTOKENS do
    case :lists.keysearch(name, r_xmlAttribute(:name), attrs) do
      {:value, a} ->
        valid_nmtoken_value(r_xmlAttribute(a, :value), type)

      _ ->
        :ok
    end

    vc_Name_Token_IDREFS(rest, attrs)
  end

  defp vc_Name_Token_IDREFS([{name, type, _, _, _} | rest], attrs)
       when type == :IDREFS do
    case :lists.keysearch(name, r_xmlAttribute(:name), attrs) do
      {:value, a} ->
        valid_IDREFS(r_xmlAttribute(a, :value), type)

      _ ->
        :ok
    end

    vc_Name_Token_IDREFS(rest, attrs)
  end

  defp vc_Name_Token_IDREFS([_H | rest], attrs) do
    vc_Name_Token_IDREFS(rest, attrs)
  end

  defp vc_Name_Token_IDREFS([], _) do
    :ok
  end

  defp valid_nmtoken_value([], :NMTOKENS) do
    exit({:error, {:at_least_one_Nmtoken_required}})
  end

  defp valid_nmtoken_value(nmtok, _) do
    validChar = fn
      x
      when x == 32 or x == 13 or x == 10 or
             (x == 9 and nmtok == :NMTOKENS) ->
        :ok

      x ->
        case :xmerl_lib.is_namechar(x) do
          false ->
            exit({:error, {:invalid_character_in_Nmtoken, x}})

          _ ->
            :ok
        end
    end

    :lists.foreach(validChar, nmtok)
  end

  defp valid_IDREFS([], :IDREFS) do
    exit({:error, {:at_least_one_IDREF_Name_required}})
  end

  defp valid_IDREFS(_Str, :IDREFS) do
    :ok
  end

  defp single_ID_definition([{_, :ID, _, _, _} = att1 | rest]) do
    case :lists.keysearch(:ID, 2, rest) do
      {:value, att2} ->
        exit({:error, {:just_one_ID_definition_allowed, att1, att2}})

      _ ->
        :ok
    end
  end

  defp single_ID_definition([_H | t]) do
    single_ID_definition(t)
  end

  defp single_ID_definition([]) do
    :ok
  end

  defp check_SDD_validity(:yes, {:external, _}, r_xmlAttribute(name: name, normalized: true), _) do
    exit({:error, {:externally_defed_attribute_normalized_in_standalone_doc, name}})
  end

  defp check_SDD_validity(:yes, {:external, _}, :no_attribute, v)
       when v != :no_value do
    exit({:error, {:externally_defed_attribute_with_default_value_missing_in_standalone_doc}})
  end

  defp check_SDD_validity(_, _, _, _) do
    :ok
  end

  defp search_attr(name, [r_xmlAttribute(name: name) = h | _T]) do
    h
  end

  defp search_attr(name, [r_xmlAttribute() | t]) do
    search_attr(name, t)
  end

  defp search_attr(_Name, _T) do
    :no_attribute
  end

  defp is_attribute_exist(name, [{name, _, _, _, _} | _T]) do
    true
  end

  defp is_attribute_exist(name, [{_Attr, _, _, _, _} | t]) do
    is_attribute_exist(name, t)
  end

  defp is_attribute_exist(_Name, []) do
    false
  end

  defp test_attribute_value(:CDATA, r_xmlAttribute() = attr, _, _) do
    attr
  end

  defp test_attribute_value(:NMTOKEN, r_xmlAttribute(name: name, value: v) = attr, default, _S) do
    fun = fn x ->
      case :xmerl_lib.is_namechar(x) do
        true ->
          :ok

        false ->
          exit({:error, {:invalid_value_nmtoken, name, v}})
      end
    end

    :lists.foreach(fun, v)

    cond do
      is_list(default) ->
        :lists.foreach(fun, default)

      true ->
        :ok
    end

    attr
  end

  defp test_attribute_value(:NMTOKENS, r_xmlAttribute(name: name, value: v) = attr, default, _S) do
    fun = fn x ->
      case :xmerl_lib.is_namechar(x) do
        true ->
          :ok

        false when x == 32 or x == 13 or x == 10 or x == 9 ->
          :ok

        false ->
          exit({:error, {:invalid_value_nmtokens, name, v}})
      end
    end

    :lists.foreach(fun, v)

    cond do
      is_list(default) ->
        :lists.foreach(fun, default)

      true ->
        :ok
    end

    attr
  end

  defp test_attribute_value(
         ent,
         r_xmlAttribute(name: _Name, value: v) = attr,
         _Default,
         s = r_xmerl_scanner(rules_read_fun: read)
       )
       when ent == :ENTITY or ent == :ENTITIES do
    nameListFun = fn
      [], acc, _ ->
        :lists.reverse(acc)

      str, acc, fun ->
        {n, str2} = scan_name(str, [])
        fun.(str2, [n | acc], fun)
    end

    nameList = nameListFun.(v, [], nameListFun)

    vC_Entity_Name = fn x ->
      case read.(:entity, x, s) do
        {_, :external, {_, {:ndata, _}}} ->
          :ok

        _ ->
          exit({:error, {:vc_Entity_Name, x, v}})
      end
    end

    :lists.foreach(vC_Entity_Name, nameList)
    attr
  end

  defp test_attribute_value({type, l}, r_xmlAttribute(value: value) = attr, default, _S)
       when type == :enumeration or type == :notation do
    validDefault =
      cond do
        is_atom(default) ->
          true

        true ->
          :lists.member(:erlang.list_to_atom(default), l)
      end

    noDuplicatesFun = fn
      _, _, :notation ->
        true

      [], _, _ ->
        true

      [h | t], f, enum ->
        case :lists.member(h, t) do
          true ->
            false

          _ ->
            f.(t, f, enum)
        end
    end

    noDuplicates = noDuplicatesFun.(l, noDuplicatesFun, type)

    case {:lists.member(:erlang.list_to_atom(value), l), validDefault, noDuplicates} do
      {true, true, true} ->
        attr

      {false, _, _} ->
        exit({:error, {:attribute_value_unknow, value, {:list, l}}})

      {_, false, _} ->
        exit({:error, {:attribute_default_value_unknow, default, {:list, l}}})

      {_, _, false} ->
        exit({:error, {:duplicate_tokens_not_allowed, {:list, l}}})
    end
  end

  defp test_attribute_value(_Rule, attr, _, _) do
    attr
  end

  defp valid_contents(rule, xMLS, rules, s, wSActionMode) do
    case parse(rule, xMLS, rules, wSActionMode, s) do
      {:error, reason} ->
        {:error, reason}

      {:error, reason, n} ->
        {:error, reason, n}

      {xML_N, rest} ->
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
            :lists.flatten(xML_N)

          [r_xmlElement(name: name) | _T] ->
            exit({:error, {:element, name, :isnt_comprise_in_the_rule, rule}})

          [r_xmlText() = txt | _T] ->
            exit({:error, {:element, :text, txt, :isnt_comprise_in_the_rule, rule}})
        end
    end
  end

  defp parse({:*, subRule}, xMLS, rules, wSaction, s) do
    star(subRule, xMLS, rules, wSaction, [], s)
  end

  defp parse({:+, subRule}, xMLS, rules, wSaction, s) do
    plus(subRule, xMLS, rules, wSaction, s)
  end

  defp parse({:choice, cHOICE}, xMLS, rules, wSaction, s) do
    choice(cHOICE, xMLS, rules, wSaction, s)
  end

  defp parse(:empty, [], _Rules, _WSaction, _S) do
    {[], []}
  end

  defp parse({:"?", subRule}, xMLS, rules, _WSaction, s) do
    question(subRule, xMLS, rules, s)
  end

  defp parse({:seq, list}, xMLS, rules, wSaction, s) do
    seq(list, xMLS, rules, wSaction, s)
  end

  defp parse(el_Name, [r_xmlElement(name: el_Name) = xML | t], rules, _WSaction, s)
       when is_atom(el_Name) do
    case do_validation(read_rules(rules, el_Name), xML, rules, s) do
      {:error, r} ->
        exit(r)

      {:error, r, _N} ->
        exit(r)

      xML_ ->
        {[xML_], t}
    end
  end

  defp parse(:any, cont, rules, _WSaction, s) do
    case (try do
            parse_any(cont, rules, s)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      err = {:error, _} ->
        err

      validContents ->
        {validContents, []}
    end
  end

  defp parse(el_Name, [r_xmlElement(name: name) | _T] = xMLS, _Rules, _WSa, _S)
       when is_atom(el_Name) do
    {:error, {:element_seq_not_conform, {:wait, el_Name}, {:is, name}},
     {{:next, xMLS}, {:act, []}}}
  end

  defp parse(el_Name, [r_xmlComment() | t], rules, wSa, s) do
    parse(el_Name, t, rules, wSa, s)
  end

  defp parse(_El_Name, [r_xmlPI() = h | t], _Rules, _WSa, _S) do
    {[h], t}
  end

  defp parse(:"#PCDATA", xMLS, _Rules, _WSa, _S) do
    parse_pcdata(xMLS)
  end

  defp parse(el_Name, [r_xmlText() | _T] = xMLS, _Rules, _WSa, _S) do
    {:error, {:text_in_place_of, el_Name}, {{:next, xMLS}, {:act, []}}}
  end

  defp parse([], _, _, _, _) do
    {:error, :no_rule}
  end

  defp parse(rule, [], _, _, _) do
    {:error, {:no_xml_element, rule}}
  end

  defp parse_any([], _Rules, _S) do
    []
  end

  defp parse_any([h | t], rules, s) do
    case parse_any(h, rules, s) do
      [cont] ->
        [cont | parse_any(t, rules, s)]

      err ->
        throw(err)
    end
  end

  defp parse_any(r_xmlElement() = xML, rules, s) do
    case do_validation(read_rules(rules, el_name(xML)), xML, rules, s) do
      {:error, r} ->
        {:error, r}

      {:error, r, n} ->
        {:error, r, n}

      xML_ ->
        [xML_]
    end
  end

  defp parse_any(el, _Rules, _S) do
    [el]
  end

  defp choice([cH | cHS], [_XML | _T] = xMLS, rules, wSaction, s) do
    {wS, xMLS1} =
      whitespace_action(
        xMLS,
        ws_action(wSaction, :remove)
      )

    case parse(cH, xMLS1, rules, ws_action(wSaction, :remove), s) do
      {:error, _R} ->
        choice(cHS, xMLS, rules, wSaction, s)

      {:error, _R, _N} ->
        choice(cHS, xMLS, rules, wSaction, s)

      {[], ^xMLS1} ->
        case cHS do
          [] ->
            {[], xMLS1}

          _ ->
            choice(cHS, xMLS1, rules, wSaction, s)
        end

      {tree, xMLS2} ->
        {wS2, xMLS3} =
          whitespace_action(
            xMLS2,
            ws_action(wSaction, :remove)
          )

        {wS2 ++ [tree] ++ wS, xMLS3}
    end
  end

  defp choice([], xMLS, _, wSaction, _S) do
    case whitespace_action(
           xMLS,
           ws_action(wSaction, :remove)
         ) do
      res = {_, []} ->
        res

      _ ->
        {:error, :element_unauthorize_in_choice, {{:next, xMLS}, {:act, []}}}
    end
  end

  defp choice(_, [], _, _, _S) do
    {[], []}
  end

  defp plus(rule, xMLS, rules, wSaction, s) do
    {wS, xMLS1} = whitespace_action(xMLS, wSaction)

    case parse(rule, xMLS1, rules, wSaction, s) do
      {:error, reason, _XML} ->
        {:error, reason}

      {:error, x} ->
        {:error, x}

      {tree, xMLS2} ->
        case star(rule, xMLS2, rules, wSaction, [], s) do
          {[], _} ->
            {wS ++ [tree], xMLS2}

          {tree_1, xMLS3} ->
            {wS ++ [tree] ++ tree_1, xMLS3}
        end
    end
  end

  defp star(_Rule, xML, _Rules, _WSa, tree, _S)
       when length(xML) == 0 do
    {[tree], []}
  end

  defp star(rule, xMLS, rules, wSaction, tree, s) do
    {wS, xMLS1} = whitespace_action(xMLS, wSaction)

    case parse(rule, xMLS1, rules, wSaction, s) do
      {:error, _E, {{:next, n}, {:act, a}}} ->
        {wS ++ tree ++ a, n}

      {:error, _E} ->
        case whitespace_action(
               xMLS,
               ws_action(wSaction, :remove)
             ) do
          {[], _} ->
            {wS ++ [tree], xMLS}

          {wS2, xMLS2} ->
            {wS2 ++ [tree], xMLS2}
        end

      {tree1, xMLS2} ->
        star(rule, xMLS2, rules, wSaction, tree ++ wS ++ [tree1], s)
    end
  end

  defp question(_Rule, [], _Rules, _S) do
    {[], []}
  end

  defp question(rule, toks, rules, s) do
    case parse(rule, toks, rules, :preserve, s) do
      {:error, _E, _Next} ->
        {[], toks}

      {:error, _E} ->
        {[], toks}

      {t, toks1} ->
        {t, toks1}
    end
  end

  defp seq(h, toks, rules, wSaction, s) do
    case seq2(h, toks, rules, [], wSaction, s) do
      {:error, e} ->
        {:error, e}

      {:error, r, n} ->
        {:error, r, n}

      {tree, toks2} ->
        {tree, toks2}
    end
  end

  defp seq2([], [], _, tree, _WSa, _S) do
    {tree, []}
  end

  defp seq2([], [r_xmlText()] = xML, _, tree, _WSa, _S) do
    case whitespace_action(xML, :remove) do
      {[], _} ->
        {:error, :sequence_finish, {{:next, xML}, {:act, tree}}}

      {wS, rest} ->
        {wS ++ tree, rest}
    end
  end

  defp seq2([], rest, _, tree, _WSa, _S) do
    {wS, rest2} = whitespace_action(rest, :remove)
    {wS ++ tree, rest2}
  end

  defp seq2([h | t], toks, rules, tree, wSaction, s) do
    {wS, toks1} =
      whitespace_action(
        toks,
        ws_action(wSaction, :remove)
      )

    case parse(h, toks1, rules, :remove, s) do
      {:error, reason, _XML} ->
        {:error, reason}

      {:error, e} ->
        {:error, e}

      {[], toks2} ->
        seq2(t, toks2, rules, tree, wSaction, s)

      {tree1, toks2} when is_list(tree1) ->
        seq2(t, toks2, rules, tree ++ wS ++ tree1, wSaction, s)

      {tree1, toks2} ->
        seq2(t, toks2, rules, tree ++ wS ++ [tree1], wSaction, s)
    end
  end

  defp el_name(r_xmlElement(name: name)) do
    name
  end

  defp parse_pcdata([r_xmlText() = h | t]) do
    parse_pcdata(t, [h])
  end

  defp parse_pcdata([r_xmlComment() | t]) do
    parse_pcdata(t, [])
  end

  defp parse_pcdata(h) do
    {[], h}
  end

  defp parse_pcdata([r_xmlText() = h | t], acc) do
    parse_pcdata(t, acc ++ [h])
  end

  defp parse_pcdata([r_xmlComment() | t], acc) do
    parse_pcdata(t, acc)
  end

  defp parse_pcdata(h, acc) do
    {acc, h}
  end

  defp whitespace([]) do
    true
  end

  defp whitespace([h | t])
       when h == 32 or h == 13 or h == 10 or
              h == 9 do
    whitespace(t)
  end

  defp whitespace(_) do
    false
  end

  defp whitespace_action(xML, :remove) do
    whitespace_remove(xML, [])
  end

  defp whitespace_action(xML, _) do
    {[], xML}
  end

  defp whitespace_remove([r_xmlText(value: v, type: :text) = t | r] = l, acc) do
    case whitespace(v) do
      true ->
        whitespace_remove(r, [t | acc])

      _ ->
        {:lists.reverse(acc), l}
    end
  end

  defp whitespace_remove(l, acc) do
    {:lists.reverse(acc), l}
  end

  defp ws_action(:always_preserve = a, _) do
    a
  end

  defp ws_action(_, b) do
    b
  end

  defp scan_name(n, _) when is_atom(n) do
    n
  end

  defp scan_name([?\s | t], acc) do
    {:erlang.list_to_atom(:lists.reverse(acc)), t}
  end

  defp scan_name([h | t], acc) do
    scan_name(t, [h | acc])
  end

  defp scan_name('', acc) do
    {:erlang.list_to_atom(:lists.reverse(acc)), []}
  end
end
