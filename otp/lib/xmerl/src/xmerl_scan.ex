defmodule :m_xmerl_scan do
  use Bitwise
  @vsn :"0.20"
  @date :"03-09-16"
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

  def user_state(r_xmerl_scanner(user_state: s)) do
    s
  end

  def event_state(r_xmerl_scanner(fun_states: r_xmerl_fun_states(event: s))) do
    s
  end

  def hook_state(r_xmerl_scanner(fun_states: r_xmerl_fun_states(hook: s))) do
    s
  end

  def rules_state(r_xmerl_scanner(fun_states: r_xmerl_fun_states(rules: s))) do
    s
  end

  def fetch_state(r_xmerl_scanner(fun_states: r_xmerl_fun_states(fetch: s))) do
    s
  end

  def cont_state(r_xmerl_scanner(fun_states: r_xmerl_fun_states(cont: s))) do
    s
  end

  def user_state(x, s) do
    r_xmerl_scanner(s, user_state: x)
  end

  def event_state(x, s = r_xmerl_scanner(fun_states: fS)) do
    fS1 = r_xmerl_fun_states(fS, event: x)
    r_xmerl_scanner(s, fun_states: fS1)
  end

  def hook_state(x, s = r_xmerl_scanner(fun_states: fS)) do
    fS1 = r_xmerl_fun_states(fS, hook: x)
    r_xmerl_scanner(s, fun_states: fS1)
  end

  def rules_state(x, s = r_xmerl_scanner(fun_states: fS)) do
    fS1 = r_xmerl_fun_states(fS, rules: x)
    r_xmerl_scanner(s, fun_states: fS1)
  end

  def fetch_state(x, s = r_xmerl_scanner(fun_states: fS)) do
    fS1 = r_xmerl_fun_states(fS, fetch: x)
    r_xmerl_scanner(s, fun_states: fS1)
  end

  def cont_state(x, s = r_xmerl_scanner(fun_states: fS)) do
    fS1 = r_xmerl_fun_states(fS, cont: x)
    r_xmerl_scanner(s, fun_states: fS1)
  end

  def file(f) do
    file(f, [])
  end

  def file(f, options) do
    extCharset =
      case :lists.keysearch(:encoding, 1, options) do
        {:value, {_, val}} ->
          val

        false ->
          :undefined
      end

    case int_file(f, options, extCharset) do
      {res, tail, s = r_xmerl_scanner(close_fun: close)} ->
        close.(s)
        {res, tail}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp int_file(f, options, _ExtCharset) do
    case :file.read_file(f) do
      {:ok, bin} ->
        int_string(:erlang.binary_to_list(bin), options, :filename.dirname(f), f)

      error ->
        error
    end
  end

  defp int_file_decl(f, options, _ExtCharset) do
    case :file.read_file(f) do
      {:ok, bin} ->
        int_string_decl(:erlang.binary_to_list(bin), options, :filename.dirname(f), f)

      error ->
        error
    end
  end

  def string(str) do
    string(str, [])
  end

  def string(str, options) do
    {res, tail, s = r_xmerl_scanner(close_fun: close)} =
      int_string(
        str,
        options,
        :file_name_unknown
      )

    close.(s)
    {res, tail}
  end

  defp int_string(str, options, fileName) do
    {:ok, xMLBase} = :file.get_cwd()
    int_string(str, options, xMLBase, fileName)
  end

  defp int_string(str, options, xMLBase, fileName) do
    s0 = initial_state0(options, xMLBase)
    s = r_xmerl_scanner(s0, filename: fileName)

    case :xmerl_lib.detect_charset(
           r_xmerl_scanner(s, :encoding),
           str
         ) do
      {:auto, :"iso-10646-utf-1", str2} ->
        scan_document(str2, r_xmerl_scanner(s, encoding: 'iso-10646-utf-1'))

      {:external, :"iso-10646-utf-1", str2} ->
        scan_document(str2, r_xmerl_scanner(s, encoding: 'iso-10646-utf-1'))

      {:undefined, :undefined, str2} ->
        scan_document(str2, s)

      {:external, extCharset, str2} ->
        scan_document(
          str2,
          r_xmerl_scanner(s, encoding: :erlang.atom_to_list(extCharset))
        )
    end
  end

  defp int_string_decl(str, options, xMLBase, fileName) do
    s0 = initial_state0(options, xMLBase)
    s = r_xmerl_scanner(s0, filename: fileName)

    case :xmerl_lib.detect_charset(
           r_xmerl_scanner(s, :encoding),
           str
         ) do
      {:auto, :"iso-10646-utf-1", str2} ->
        scan_decl(str2, r_xmerl_scanner(s, encoding: 'iso-10646-utf-1'))

      {:external, :"iso-10646-utf-1", str2} ->
        scan_decl(str2, r_xmerl_scanner(s, encoding: 'iso-10646-utf-1'))

      {:undefined, :undefined, str2} ->
        scan_decl(str2, s)

      {:external, extCharset, str2} ->
        scan_decl(
          str2,
          r_xmerl_scanner(s, encoding: :erlang.atom_to_list(extCharset))
        )
    end
  end

  defp initial_state0(options, xMLBase) do
    commonData = common_data()

    initial_state(
      options,
      r_xmerl_scanner(
        event_fun: &event/2,
        hook_fun: &hook/2,
        acc_fun: &acc/3,
        fetch_fun: &fetch/2,
        close_fun: &close/1,
        continuation_fun: &cont/3,
        rules_read_fun: &rules_read/3,
        rules_write_fun: &rules_write/4,
        rules_delete_fun: &rules_delete/3,
        xmlbase: xMLBase,
        common_data: commonData
      )
    )
  end

  defp initial_state([{:event_fun, f} | t], s) do
    initial_state(t, r_xmerl_scanner(s, event_fun: f))
  end

  defp initial_state([{:event_fun, f, eS} | t], s) do
    s1 = event_state(eS, r_xmerl_scanner(s, event_fun: f))
    initial_state(t, s1)
  end

  defp initial_state([{:acc_fun, f} | t], s) do
    initial_state(t, r_xmerl_scanner(s, acc_fun: f))
  end

  defp initial_state([{:hook_fun, f} | t], s) do
    initial_state(t, r_xmerl_scanner(s, hook_fun: f))
  end

  defp initial_state([{:hook_fun, f, hS} | t], s) do
    s1 = hook_state(hS, r_xmerl_scanner(s, hook_fun: f))
    initial_state(t, s1)
  end

  defp initial_state([{:close_fun, f} | t], s) do
    initial_state(t, r_xmerl_scanner(s, close_fun: f))
  end

  defp initial_state([{:fetch_fun, f} | t], s) do
    initial_state(t, r_xmerl_scanner(s, fetch_fun: f))
  end

  defp initial_state([{:fetch_fun, f, fS} | t], s) do
    s1 = fetch_state(fS, r_xmerl_scanner(s, fetch_fun: f))
    initial_state(t, s1)
  end

  defp initial_state([{:fetch_path, p} | t], s) do
    initial_state(t, r_xmerl_scanner(s, fetch_path: p))
  end

  defp initial_state([{:continuation_fun, f} | t], s) do
    initial_state(t, r_xmerl_scanner(s, continuation_fun: f))
  end

  defp initial_state([{:continuation_fun, f, cS} | t], s) do
    s1 = cont_state(cS, r_xmerl_scanner(s, continuation_fun: f))
    initial_state(t, s1)
  end

  defp initial_state([{:rules, r} | t], s) do
    initial_state(t, r_xmerl_scanner(s, rules: r, keep_rules: true))
  end

  defp initial_state([{:rules, read, write, rS} | t], s) do
    s1 =
      rules_state(
        rS,
        r_xmerl_scanner(s, rules_read_fun: read, rules_write_fun: write, keep_rules: true)
      )

    initial_state(t, s1)
  end

  defp initial_state([{:user_state, f} | t], s) do
    initial_state(t, r_xmerl_scanner(s, user_state: f))
  end

  defp initial_state([{:space, l} | t], s) do
    initial_state(t, r_xmerl_scanner(s, space: l))
  end

  defp initial_state([{:line, l} | t], s) do
    initial_state(t, r_xmerl_scanner(s, line: l))
  end

  defp initial_state([{:namespace_conformant, f} | t], s)
       when f == true or f == false do
    initial_state(t, r_xmerl_scanner(s, namespace_conformant: f))
  end

  defp initial_state([{:validation, f} | t], s)
       when f == :off or
              f == :dtd or f == :schema or
              f == true or f == false do
    initial_state(t, r_xmerl_scanner(s, validation: validation_value(f)))
  end

  defp initial_state([{:schemaLocation, sL} | t], s)
       when is_list(sL) do
    initial_state(t, r_xmerl_scanner(s, schemaLocation: sL))
  end

  defp initial_state([{:quiet, f} | t], s)
       when f == true or
              f == false do
    initial_state(t, r_xmerl_scanner(s, quiet: f))
  end

  defp initial_state([{:doctype_DTD, dTD} | t], s) do
    initial_state(t, r_xmerl_scanner(s, doctype_DTD: dTD))
  end

  defp initial_state([{:document, f} | t], s) when is_boolean(f) do
    initial_state(t, r_xmerl_scanner(s, document: f))
  end

  defp initial_state([{:comments, f} | t], s) when is_boolean(f) do
    initial_state(t, r_xmerl_scanner(s, comments: f))
  end

  defp initial_state([{:default_attrs, f} | t], s)
       when is_boolean(f) do
    initial_state(t, r_xmerl_scanner(s, default_attrs: f))
  end

  defp initial_state([{:text_decl, bool} | t], s) do
    initial_state(t, r_xmerl_scanner(s, text_decl: bool))
  end

  defp initial_state([{:environment, env} | t], s) do
    initial_state(t, r_xmerl_scanner(s, environment: env))
  end

  defp initial_state([{:xmlbase, d} | t], s) do
    initial_state(t, r_xmerl_scanner(s, xmlbase: d))
  end

  defp initial_state([{:encoding, enc} | t], s) do
    initial_state(t, r_xmerl_scanner(s, encoding: enc))
  end

  defp initial_state([], s = r_xmerl_scanner(rules: :undefined)) do
    tab = :ets.new(:rules, [:set, :public])
    r_xmerl_scanner(s, rules: tab)
  end

  defp initial_state([], s) do
    s
  end

  defp validation_value(true) do
    :dtd
  end

  defp validation_value(false) do
    :off
  end

  defp validation_value(f) do
    f
  end

  defp common_data() do
    {comdata(:lists.duplicate(60, ?\s), []), comdata(:lists.duplicate(15, ?\t), []), '\n'}
  end

  defp comdata([], cD) do
    :erlang.list_to_tuple(cD)
  end

  defp comdata([_ | t] = l, cD) do
    comdata(t, [[?\n | l] | cD])
  end

  defp hook(x, state) do
    {x, state}
  end

  defp event(_X, s) do
    s
  end

  defp acc(r_xmlText(value: text), [x = r_xmlText(value: accText)], s) do
    {[r_xmlText(x, value: accText ++ text)], s}
  end

  defp acc(x, acc, s) do
    {[x | acc], s}
  end

  defp fetch({:system, uRI}, s) do
    fetch_URI(uRI, s)
  end

  defp fetch({:public, _PublicID, uRI}, s) do
    fetch_URI(uRI, s)
  end

  defp fetch_URI(uRI, s) do
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
          :filename.join(r_xmerl_scanner(s, :xmlbase), uRI)
      end

    path = path_locate(r_xmerl_scanner(s, :fetch_path), filename, fullname)
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

  defp cont(_F, exception, uS) do
    exception.(uS)
  end

  defp close(s) do
    s
  end

  defp scan_document(
         str0,
         s =
           r_xmerl_scanner(
             event_fun: event,
             line: l,
             col: c,
             environment: env,
             encoding: charset,
             document: document,
             validation: validateResult
           )
       ) do
    s1 =
      event.(
        r_xmerl_event(event: :started, line: l, col: c, data: :document),
        s
      )

    str =
      cond do
        charset == 'utf-8' ->
          str0

        charset !== :undefined ->
          :xmerl_ucs.to_unicode(
            str0,
            :erlang.list_to_atom(charset)
          )

        true ->
          str0
      end

    {prolog, pos, t1, s2} = scan_prolog(str, s1, _StartPos = 1)
    t2 = scan_mandatory('<', t1, 1, s2, :expected_element_start_tag)
    {res, t3, s3} = scan_element(t2, s2, pos)
    {misc, _Pos1, tail, s4} = scan_misc(t3, s3, pos + 1)

    s5 =
      r_xmerl_scanner() =
      event.(
        r_xmerl_event(
          event: :ended,
          line: r_xmerl_scanner(s4, :line),
          col: r_xmerl_scanner(s4, :col),
          data: :document
        ),
        s4
      )

    {res2, s6} =
      case validation_mode(validateResult) do
        :off ->
          {res, cleanup(s5)}

        :dtd when env == :element or env == :prolog ->
          check_decl2(s5)

          case :xmerl_validate.validate(s5, res) do
            {:EXIT, {:error, reason}} ->
              s5b = cleanup(s5)

              cond do
                r_xmerl_scanner(s5b, :quiet) ->
                  :ok

                true ->
                  :error_logger.error_msg(
                    '~p- fatal: ~p~n',
                    [595, {:failed_validation, reason}]
                  )

                  :ok
              end

              fatal({:failed_validation, reason}, s5b)

            {:EXIT, reason} ->
              s5b = cleanup(s5)

              cond do
                r_xmerl_scanner(s5b, :quiet) ->
                  :ok

                true ->
                  :error_logger.error_msg(
                    '~p- fatal: ~p~n',
                    [598, {:failed_validation, reason}]
                  )

                  :ok
              end

              fatal({:failed_validation, reason}, s5b)

            {:error, reason} ->
              s5b = cleanup(s5)

              cond do
                r_xmerl_scanner(s5b, :quiet) ->
                  :ok

                true ->
                  :error_logger.error_msg(
                    '~p- fatal: ~p~n',
                    [601, {:failed_validation, reason}]
                  )

                  :ok
              end

              fatal({:failed_validation, reason}, s5b)

            {:error, reason, _Next} ->
              s5b = cleanup(s5)

              cond do
                r_xmerl_scanner(s5b, :quiet) ->
                  :ok

                true ->
                  :error_logger.error_msg(
                    '~p- fatal: ~p~n',
                    [604, {:failed_validation, reason}]
                  )

                  :ok
              end

              fatal({:failed_validation, reason}, s5b)

            _XML ->
              {res, cleanup(s5)}
          end

        :schema ->
          case schemaLocations(res, s5) do
            {:ok, schemas} ->
              _ = cleanup(s5)
              xSDRes = :xmerl_xsd.process_validate(schemas, res, inherit_options(s5))
              handle_schema_result(xSDRes, s5)

            _ ->
              {res, cleanup(s5)}
          end

        _ ->
          {res, cleanup(s5)}
      end

    res3 =
      case document do
        true ->
          content =
            :lists.reverse(
              prolog,
              [res2 | :lists.reverse(misc)]
            )

          r_xmlDocument(content: content)

        false ->
          res2
      end

    {res3, tail, s6}
  end

  defp scan_decl(
         str,
         s =
           r_xmerl_scanner(
             event_fun: event,
             line: l,
             col: c,
             environment: _Env,
             encoding: _Charset,
             validation: _ValidateResult
           )
       ) do
    s1 =
      event.(
        r_xmerl_event(event: :started, line: l, col: c, data: :document),
        s
      )

    case scan_prolog(str, s1, _StartPos = 1) do
      {_, _, t2 = '<' ++ _, s2} ->
        {{r_xmerl_scanner(s2, :user_state), t2}, [], s2}

      {_, _, [], s2} ->
        {[], [], s2}

      {_, _, t2, s2} ->
        {_, _, s3} =
          scan_content(
            t2,
            s2,
            [],
            _Attrs = [],
            r_xmerl_scanner(s2, :space),
            _Lang = [],
            _Parents = [],
            r_xmlNamespace()
          )

        {t2, [], s3}
    end
  end

  defp scan_prolog(t, s, pos) do
    scan_prolog(t, s, pos, [])
  end

  defp scan_prolog([], s = r_xmerl_scanner(continuation_fun: f), pos, acc) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_prolog(moreBytes, s1, pos, acc)
      end,
      fn s1 ->
        {acc, pos, [], s1}
      end,
      s
    )
  end

  defp scan_prolog(
         '<?xml' ++ t,
         s0 = r_xmerl_scanner(encoding: charset0, col: col, line: l),
         pos,
         acc
       )
       when hd(t) == 32 or hd(t) == 13 or hd(t) == 10 or
              hd(t) == 9 do
    {charset, t3, s3} =
      cond do
        col == 1 and l == 1 and
            r_xmerl_scanner(s0, :text_decl) == true ->
          :no_debug
          :no_debug
          s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 5)
          {_, t1, s1} = mandatory_strip(t, s)
          {decl, t2, s2} = scan_text_decl(t1, s1)
          encoding = r_xmlDecl(decl, :encoding)
          {encoding, t2, r_xmerl_scanner(s2, encoding: encoding)}

        col == 1 and l == 1 ->
          :no_debug
          :no_debug
          s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 5)
          {decl, t2, s2} = scan_xml_decl(t, s)
          encoding = r_xmlDecl(decl, :encoding)
          {encoding, t2, r_xmerl_scanner(s2, encoding: encoding)}

        true ->
          cond do
            r_xmerl_scanner(s0, :quiet) ->
              :ok

            true ->
              :error_logger.error_msg(
                '~p- fatal: ~p~n',
                [687, {:xml_declaration_must_be_first_in_doc, col, l}]
              )

              :ok
          end

          fatal(
            {:xml_declaration_must_be_first_in_doc, col, l},
            s0
          )
      end

    cond do
      charset == charset0 ->
        scan_prolog(t3, s3, pos, acc)

      charset0 !== :undefined ->
        scan_prolog(t3, r_xmerl_scanner(s3, encoding: charset0), pos, acc)

      charset == 'utf-8' ->
        scan_prolog(t3, s3, pos, acc)

      charset !== :undefined ->
        t4 =
          :xmerl_ucs.to_unicode(
            t3,
            :erlang.list_to_atom(charset)
          )

        scan_prolog(t4, s3, pos, acc)

      true ->
        scan_prolog(t3, s3, pos, acc)
    end
  end

  defp scan_prolog(
         '<!DOCTYPE' ++ t,
         s0 = r_xmerl_scanner(environment: :prolog, encoding: _Charset),
         pos,
         acc
       ) do
    :no_debug
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 9)

    t1 =
      cond do
        true ->
          t
      end

    {t2, s1} = scan_doctype(t1, s)
    scan_misc(t2, s1, pos, acc)
  end

  defp scan_prolog(str = '%' ++ _T, s = r_xmerl_scanner(environment: {:external, _}), pos, acc) do
    {t, s1} = scan_ext_subset(str, s)
    {acc, pos, t, s1}
  end

  defp scan_prolog(str, s0 = r_xmerl_scanner(user_state: _US, encoding: _Charset), pos, acc) do
    :no_debug
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)

    t =
      cond do
        true ->
          str
      end

    {acc1, pos1, t1, s1} = scan_misc(t, s, pos, acc)
    scan_prolog2(t1, s1, pos1, acc1)
  end

  defp scan_prolog2([], s = r_xmerl_scanner(continuation_fun: f), pos, acc) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_prolog2(moreBytes, s1, pos, acc)
      end,
      fn s1 ->
        {acc, pos, [], s1}
      end,
      s
    )
  end

  defp scan_prolog2('<!DOCTYPE' ++ t, s0 = r_xmerl_scanner(environment: :prolog), pos, acc) do
    :no_debug
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 9)
    {t1, s1} = scan_doctype(t, s)
    scan_misc(t1, s1, pos, acc)
  end

  defp scan_prolog2(str = '<!' ++ _, s, pos, acc) do
    :no_debug
    {t, s1} = scan_ext_subset(str, s)
    {acc, pos, t, s1}
  end

  defp scan_prolog2(str, s0 = r_xmerl_scanner(user_state: _US), pos, acc) do
    :no_debug

    s1 =
      case s0 do
        r_xmerl_scanner(validation: :dtd, doctype_DTD: dTD) when is_list(dTD) ->
          s = fetch_DTD(:undefined, s0)
          check_decl(s)
          s

        _ ->
          s0
      end

    scan_misc(str, s1, pos, acc)
  end

  defp scan_misc(t, s, pos) do
    scan_misc(t, s, pos, [])
  end

  defp scan_misc([], s = r_xmerl_scanner(continuation_fun: f), pos, acc) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_misc(moreBytes, s1, pos, acc)
      end,
      fn s1 ->
        {acc, pos, [], s1}
      end,
      s
    )
  end

  defp scan_misc('<!--' ++ t, s0 = r_xmerl_scanner(acc_fun: f, comments: cF), pos, acc) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 4)
    {c, t1, s1} = scan_comment(t, s, pos, _Parents = [], _Lang = [])

    case cF do
      true ->
        {acc2, pos2, s3} =
          case f.(c, acc, s1) do
            {acc1, s2} ->
              {acc1, pos + 1, s2}

            {acc1, pos1, s2} ->
              {acc1, pos1, s2}
          end

        scan_misc(t1, s3, pos2, acc2)

      false ->
        scan_misc(t1, s1, pos, acc)
    end
  end

  defp scan_misc('<?' ++ t, s0 = r_xmerl_scanner(acc_fun: f), pos, acc) do
    :no_debug
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 2)
    {pI, t1, s1} = scan_pi(t, s, pos, [])

    {acc2, pos2, s3} =
      case f.(pI, acc, s1) do
        {acc1, s2} ->
          {acc1, pos + 1, s2}

        {acc1, pos1, s2} ->
          {acc1, pos1, s2}
      end

    scan_misc(t1, s3, pos2, acc2)
  end

  defp scan_misc(t = [h | _T], s, pos, acc)
       when h == 32 or
              h == 13 or h == 10 or h == 9 do
    :no_debug
    {_, t1, s1} = strip(t, s)
    scan_misc(t1, s1, pos, acc)
  end

  defp scan_misc(t, s, pos, acc) do
    {acc, pos, t, s}
  end

  defp cleanup(s = r_xmerl_scanner(keep_rules: false, rules: rules)) do
    :ets.delete(rules)
    r_xmerl_scanner(s, rules: :undefined)
  end

  defp cleanup(s) do
    s
  end

  defp scan_xml_decl(t, s) do
    {_, t1, s1} = mandatory_strip(t, s)

    {t2, s2} =
      case t1 do
        'version' ++ _T2 ->
          {_T2, r_xmerl_scanner(s1, col: r_xmerl_scanner(s1, :col) + 7)}

        _ ->
          cond do
            r_xmerl_scanner(s1, :quiet) ->
              :ok

            true ->
              :error_logger.error_msg(
                '~p- fatal: ~p~n',
                [848, :expected_version_attribute]
              )

              :ok
          end

          fatal(:expected_version_attribute, s1)
      end

    {t3, s3} = scan_eq(t2, s2)
    {vsn, t4, s4} = scan_xml_vsn(t3, s3)
    attr = r_xmlAttribute(name: :version, parents: [{:xml, _XMLPos = 1}], value: vsn)
    scan_xml_decl(t4, s4, r_xmlDecl(vsn: vsn, attributes: [attr]))
  end

  defp scan_xml_decl([], s = r_xmerl_scanner(continuation_fun: f), decl) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_xml_decl(moreBytes, s1, decl)
      end,
      fn s1 ->
        {[], [], s1}
      end,
      s
    )
  end

  defp scan_xml_decl('?>' ++ t, s0, decl) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 2)
    return_xml_decl(t, s, decl)
  end

  defp scan_xml_decl(t, s = r_xmerl_scanner(event_fun: _Event), decl)
       when hd(t) == 32 or hd(t) == 13 or hd(t) == 10 or
              hd(t) == 9 do
    {_, t1, s1} = mandatory_strip(t, s)
    scan_xml_decl2(t1, s1, decl)
  end

  defp scan_xml_decl(_T, s = r_xmerl_scanner(event_fun: _Event), _Decl) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [
            870,
            preformat(
              [:expected, :one, :"of:"],
              [:"?>", :whitespace_character],
              ','
            )
          ]
        )

        :ok
    end

    fatal(
      preformat([:expected, :one, :"of:"], [:"?>", :whitespace_character], ','),
      s
    )
  end

  defp scan_xml_decl2('?>' ++ t, s0, decl) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 2)
    return_xml_decl(t, s, decl)
  end

  defp scan_xml_decl2(
         'encoding' ++ t,
         s0 = r_xmerl_scanner(event_fun: event),
         decl0 = r_xmlDecl(attributes: attrs)
       ) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 8)
    {t1, s1} = scan_eq(t, s)
    {encName, t2, s2} = scan_enc_name(t1, s1)
    lowEncName = :xmerl_lib.to_lower(encName)
    attr = r_xmlAttribute(name: :encoding, parents: [{:xml, _XMLPos = 1}], value: lowEncName)

    decl =
      r_xmlDecl(decl0,
        encoding: lowEncName,
        attributes: [attr | attrs]
      )

    s3 =
      r_xmerl_scanner() =
      event.(
        r_xmerl_event(
          event: :ended,
          line: r_xmerl_scanner(s0, :line),
          col: r_xmerl_scanner(s0, :col),
          data: attr
        ),
        s2
      )

    case t2 do
      '?>' ++ _T3 ->
        scan_xml_decl3(t2, s3, decl)

      _ ->
        {_, t3, s4} = mandatory_strip(t2, s3)
        scan_xml_decl3(t3, s4, decl)
    end
  end

  defp scan_xml_decl2(t = 'standalone' ++ _T, s, decl) do
    scan_xml_decl3(t, s, decl)
  end

  defp scan_xml_decl2(_BadString, s, _Decl) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [
            901,
            preformat(
              [:expected, :one, :"of:"],
              [:"?>", :standalone, :encoding],
              ','
            )
          ]
        )

        :ok
    end

    fatal(
      preformat([:expected, :one, :"of:"], [:"?>", :standalone, :encoding], ','),
      s
    )
  end

  defp scan_xml_decl3('?>' ++ t, s0, decl) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 2)
    return_xml_decl(t, s, decl)
  end

  defp scan_xml_decl3(
         'standalone' ++ t,
         s0 = r_xmerl_scanner(event_fun: event),
         decl0 = r_xmlDecl(attributes: attrs)
       ) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 10)
    {t1, s1} = scan_eq(t, s)
    {stValue, t2, s2} = scan_standalone_value(t1, s1)
    attr = r_xmlAttribute(name: :standalone, parents: [{:xml, _XMLPos = 1}], value: stValue)

    decl =
      r_xmlDecl(decl0,
        standalone: stValue,
        attributes: [attr | attrs]
      )

    s3 =
      r_xmerl_scanner() =
      event.(
        r_xmerl_event(
          event: :ended,
          line: r_xmerl_scanner(s0, :line),
          col: r_xmerl_scanner(s0, :col),
          data: attr
        ),
        s2
      )

    {_, t3, s4} = strip(t2, s3)
    t4 = scan_mandatory('?>', t3, 2, s4, :expected_xml_decl_endtag)
    return_xml_decl(t4, r_xmerl_scanner(s4, col: r_xmerl_scanner(s4, :col) + 2), decl)
  end

  defp return_xml_decl(
         t,
         s = r_xmerl_scanner(hook_fun: _Hook, event_fun: event),
         decl0 = r_xmlDecl(attributes: attrs)
       ) do
    {_, t1, s1} = strip(t, s)
    decl = r_xmlDecl(decl0, attributes: :lists.reverse(attrs))

    s2 =
      r_xmerl_scanner() =
      event.(
        r_xmerl_event(
          event: :ended,
          line: r_xmerl_scanner(s, :line),
          col: r_xmerl_scanner(s, :col),
          data: decl
        ),
        s1
      )

    {decl, t1, s2}
  end

  defp scan_standalone_value('\'yes\'' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 5)
    {:yes, t, r_xmerl_scanner(s, standalone: :yes)}
  end

  defp scan_standalone_value('"yes"' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 5)
    {:yes, t, r_xmerl_scanner(s, standalone: :yes)}
  end

  defp scan_standalone_value('\'no\'' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 4)
    {:no, t, s}
  end

  defp scan_standalone_value('"no"' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 4)
    {:no, t, s}
  end

  defp scan_text_decl(t, s = r_xmerl_scanner(event_fun: event)) do
    {r_xmlDecl(attributes: attrs) = decl0, t1, s1} = scan_optional_version(t, s)

    t2 =
      case t1 do
        'encoding' ++ _T2 ->
          _T2

        _ ->
          cond do
            r_xmerl_scanner(s1, :quiet) ->
              :ok

            true ->
              :error_logger.error_msg(
                '~p- fatal: ~p~n',
                [965, :expected_encoding_attribute]
              )

              :ok
          end

          fatal(:expected_encoding_attribute, s1)
      end

    s2 = r_xmerl_scanner(s1, col: r_xmerl_scanner(s1, :col) + 8)
    {t3, s3} = scan_eq(t2, s2)
    {encName, t4, s4} = scan_enc_name(t3, s3)
    lowEncName = :xmerl_lib.to_lower(encName)
    {_, t5, s5} = strip(t4, s4)
    attr = r_xmlAttribute(name: :encoding, parents: [{:xml, 1}], value: lowEncName)

    decl =
      r_xmlDecl(decl0,
        encoding: lowEncName,
        attributes: [attr | attrs]
      )

    s6 =
      r_xmerl_scanner() =
      event.(
        r_xmerl_event(
          event: :ended,
          line: r_xmerl_scanner(s5, :line),
          col: r_xmerl_scanner(s5, :col),
          data: attr
        ),
        s5
      )

    scan_text_decl(t5, s6, decl)
  end

  defp scan_text_decl(
         '?>' ++ t,
         s0 = r_xmerl_scanner(hook_fun: _Hook, event_fun: event),
         decl0 = r_xmlDecl(attributes: attrs)
       ) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 2)
    {_, t1, s1} = strip(t, s)
    decl = r_xmlDecl(decl0, attributes: :lists.reverse(attrs))

    s2 =
      r_xmerl_scanner() =
      event.(
        r_xmerl_event(
          event: :ended,
          line: r_xmerl_scanner(s0, :line),
          col: r_xmerl_scanner(s0, :col),
          data: decl
        ),
        s1
      )

    {decl, t1, s2}
  end

  defp scan_text_decl([h | _T], s, _) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [997, {:unexpected_character_in_text_declaration, h}]
        )

        :ok
    end

    fatal({:unexpected_character_in_text_declaration, h}, s)
  end

  defp scan_optional_version('version' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 7)
    {_, t1, s1} = strip(t, s)
    {t2, s2} = scan_eq(t1, s1)
    {vsn, t3, s3} = scan_xml_vsn(t2, s2)
    {_, t4, s4} = mandatory_strip(t3, s3)
    attr = r_xmlAttribute(name: :version, parents: [{:xml, 1}], value: vsn)
    {r_xmlDecl(attributes: [attr]), t4, s4}
  end

  defp scan_optional_version(t, s) do
    {r_xmlDecl(attributes: []), t, s}
  end

  defp scan_enc_name([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_enc_name(moreBytes, s1)
      end,
      fatal_fun(:expected_encoding_name),
      s
    )
  end

  defp scan_enc_name([h | t], s0) when h >= ?" or h <= ?' do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_enc_name(t, s, h, [])
  end

  defp scan_enc_name([], s = r_xmerl_scanner(continuation_fun: f), delim, acc) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_enc_name(moreBytes, s1, delim, acc)
      end,
      fatal_fun(:expected_encoding_name),
      s
    )
  end

  defp scan_enc_name([h | t], s0, delim, acc)
       when h >= ?a and
              h <= ?z do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_enc_name2(t, s, delim, [h | acc])
  end

  defp scan_enc_name([h | t], s0, delim, acc)
       when h >= ?A and
              h <= ?Z do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_enc_name2(t, s, delim, [h | acc])
  end

  defp scan_enc_name([h | _T], s, _Delim, _Acc) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [1035, {:error, {:unexpected_character_in_Enc_Name, h}}]
        )

        :ok
    end

    fatal(
      {:error, {:unexpected_character_in_Enc_Name, h}},
      s
    )
  end

  defp scan_enc_name2([], s = r_xmerl_scanner(continuation_fun: f), delim, acc) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_enc_name2(moreBytes, s1, delim, acc)
      end,
      fatal_fun(:expected_encoding_name),
      s
    )
  end

  defp scan_enc_name2([h | t], s0, h, acc) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {:lists.reverse(acc), t, s}
  end

  defp scan_enc_name2([h | t], s0, delim, acc)
       when h >= ?a and
              h <= ?z do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_enc_name2(t, s, delim, [h | acc])
  end

  defp scan_enc_name2([h | t], s0, delim, acc)
       when h >= ?A and
              h <= ?Z do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_enc_name2(t, s, delim, [h | acc])
  end

  defp scan_enc_name2([h | t], s0, delim, acc)
       when h >= ?0 and
              h <= ?9 do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_enc_name2(t, s, delim, [h | acc])
  end

  defp scan_enc_name2([h | t], s0, delim, acc)
       when h == ?. or
              h == ?_ or h == ?- do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_enc_name2(t, s, delim, [h | acc])
  end

  defp scan_xml_vsn([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_xml_vsn(moreBytes, s1)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_xml_vsn([h | t], s) when h == ?" or h == ?' do
    xml_vsn(t, r_xmerl_scanner(s, col: r_xmerl_scanner(s, :col) + 1), h, [])
  end

  defp xml_vsn([], s = r_xmerl_scanner(continuation_fun: f), delim, acc) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        xml_vsn(moreBytes, s1, delim, acc)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp xml_vsn([h | t], s = r_xmerl_scanner(col: c), h, acc) do
    {:lists.reverse(acc), t, r_xmerl_scanner(s, col: c + 1)}
  end

  defp xml_vsn([h | t], s = r_xmerl_scanner(col: c), delim, acc)
       when h >= ?a and h <= ?z do
    xml_vsn(t, r_xmerl_scanner(s, col: c + 1), delim, [h | acc])
  end

  defp xml_vsn([h | t], s = r_xmerl_scanner(col: c), delim, acc)
       when h >= ?A and h <= ?Z do
    xml_vsn(t, r_xmerl_scanner(s, col: c + 1), delim, [h | acc])
  end

  defp xml_vsn([h | t], s = r_xmerl_scanner(col: c), delim, acc)
       when h >= ?0 and h <= ?9 do
    xml_vsn(t, r_xmerl_scanner(s, col: c + 1), delim, [h | acc])
  end

  defp xml_vsn([h | t], s = r_xmerl_scanner(col: c), delim, acc) do
    case :lists.member(h, '_.:-') do
      true ->
        xml_vsn(t, r_xmerl_scanner(s, col: c + 1), delim, [h | acc])

      false ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [1087, {:invalid_vsn_char, h}]
            )

            :ok
        end

        fatal({:invalid_vsn_char, h}, s)
    end
  end

  defp scan_pi([], s = r_xmerl_scanner(continuation_fun: f), pos, ps) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_pi(moreBytes, s1, pos, ps)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_pi(str = [[h1, h2, h3] | t], s0 = r_xmerl_scanner(line: l, col: c), pos, ps)
       when h1 == ?x or h1 == ?X do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 3)

    cond do
      (h2 == ?m or h2 == ?M) and (h3 == ?l or h3 == ?L) ->
        scan_wellknown_pi(t, s, pos, ps)

      true ->
        {target, _NamespaceInfo, t1, s1} = scan_name(str, s)
        scan_pi(t1, s1, target, l, c, pos, ps, [])
    end
  end

  defp scan_pi(str, s = r_xmerl_scanner(line: l, col: c), pos, ps) do
    {target, _NamespaceInfo, t1, s1} = scan_name(str, s)
    scan_pi(t1, s1, target, l, c, pos, ps, [])
  end

  defp scan_wellknown_pi('-stylesheet' ++ t, s0 = r_xmerl_scanner(line: l, col: c), pos, ps) do
    :no_debug
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 16)
    scan_pi(t, s, 'xml-stylesheet', l, c, pos, ps, [])
  end

  defp scan_wellknown_pi(str, s, _Pos, _Ps) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [1122, {:invalid_target_name, :lists.sublist(str, 1, 10)}]
        )

        :ok
    end

    fatal(
      {:invalid_target_name, :lists.sublist(str, 1, 10)},
      s
    )
  end

  defp scan_pi([], s = r_xmerl_scanner(continuation_fun: f), target, l, c, pos, ps, acc) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_pi(moreBytes, s1, target, l, c, pos, ps, acc)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_pi(
         '?>' ++ t,
         s0 = r_xmerl_scanner(hook_fun: hook, event_fun: event),
         target,
         l,
         c,
         pos,
         ps,
         acc
       ) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 2)
    pI = r_xmlPI(name: target, parents: ps, pos: pos, value: :lists.reverse(acc))

    s1 =
      r_xmerl_scanner() =
      event.(
        r_xmerl_event(event: :ended, line: l, col: c, data: pI),
        s
      )

    {ret, s2} = hook.(pI, s1)
    {ret, t, s2}
  end

  defp scan_pi([h | t], s, target, l, c, pos, ps, acc)
       when h == 32 or h == 13 or h == 10 or h == 9 do
    {_, t1, s1} = strip(t, s)
    scan_pi2(t1, s1, target, l, c, pos, ps, acc)
  end

  defp scan_pi([h | _T], s, _Target, _L, _C, _Pos, _Ps, _Acc) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [1151, {:expected_whitespace_OR_end_of_PI, {:char, h}}]
        )

        :ok
    end

    fatal(
      {:expected_whitespace_OR_end_of_PI, {:char, h}},
      s
    )
  end

  defp scan_pi2([], s = r_xmerl_scanner(continuation_fun: f), target, l, c, pos, ps, acc) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_pi2(moreBytes, s1, target, l, c, pos, ps, acc)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_pi2(
         '?>' ++ t,
         s0 = r_xmerl_scanner(hook_fun: hook, event_fun: event),
         target,
         l,
         c,
         pos,
         ps,
         acc
       ) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 2)
    pI = r_xmlPI(name: target, parents: ps, pos: pos, value: :lists.reverse(acc))

    s1 =
      r_xmerl_scanner() =
      event.(
        r_xmerl_event(event: :ended, line: l, col: c, data: pI),
        s
      )

    {ret, s2} = hook.(pI, s1)
    {ret, t, s2}
  end

  defp scan_pi2(str, s0, target, l, c, pos, ps, acc) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {ch, t} = wfc_legal_char(str, s)
    scan_pi2(t, s, target, l, c, pos, ps, [ch | acc])
  end

  defp scan_doctype([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_doctype(moreBytes, s1)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_doctype(t, s) do
    {_, t1, s1} = mandatory_strip(t, s)
    {dTName, _NamespaceInfo, t2, s2} = scan_name(t1, s1)
    {_, t3, s3} = strip(t2, s2)
    scan_doctype1(t3, r_xmerl_scanner(s3, doctype_name: dTName))
  end

  defp scan_doctype1([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_doctype1(moreBytes, s1)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_doctype1('PUBLIC' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 6)
    {_, t1, s1} = mandatory_strip(t, s)
    {pIDL, t2, s2} = scan_pubid_literal(t1, s1)
    {_, t3, s3} = mandatory_strip(t2, s2)
    {sL, t4, s4} = scan_system_literal(t3, s3)
    {_, t5, s5} = strip(t4, s4)
    scan_doctype2(t5, s5, {:public, pIDL, sL})
  end

  defp scan_doctype1('SYSTEM' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 6)
    {_, t1, s1} = mandatory_strip(t, s)
    {sL, t2, s2} = scan_system_literal(t1, s1)
    {_, t3, s3} = strip(t2, s2)
    scan_doctype2(t3, s3, {:system, sL})
  end

  defp scan_doctype1(t, s) do
    scan_doctype2(t, s, :undefined)
  end

  defp scan_doctype2([], s = r_xmerl_scanner(continuation_fun: f), dTD) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_doctype2(moreBytes, s1, dTD)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_doctype2('[' ++ t, s0, dTD) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {_, t1, s1} = strip(t, s)
    scan_doctype3(t1, s1, dTD)
  end

  defp scan_doctype2('>' ++ t, s0, dTD) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {_, t1, s1} = strip(t, s)
    s2 = fetch_DTD(dTD, s1)
    check_decl(s2)
    {t1, s2}
  end

  defp scan_doctype2(_T, s, _DTD) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [1236, :expected_end_of_DOCTYPE_declaration]
        )

        :ok
    end

    fatal(:expected_end_of_DOCTYPE_declaration, s)
  end

  defp scan_doctype3([], s = r_xmerl_scanner(continuation_fun: f), dTD) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_doctype3(moreBytes, s1, dTD)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_doctype3('%' ++ t, s0, dTD) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {pERefName, t1, s1} = scan_pe_reference(t, s)
    {_, t2, s2} = strip(t1, s1)

    case expand_pe_reference(pERefName, s2, :as_PE) do
      {:system, _} = name ->
        s3 = fetch_DTD(name, s2)
        check_decl(s3)
        scan_doctype3(t2, s3, dTD)

      {:public, _} = name ->
        s3 = fetch_DTD(name, s2)
        check_decl(s3)
        scan_doctype3(t2, s3, dTD)

      {:public, _, _} = name ->
        s3 = fetch_DTD(name, s2)
        check_decl(s3)
        scan_doctype3(t2, s3, dTD)

      expRef when is_list(expRef) ->
        {_, t3, s3} = strip(expRef ++ t2, s2)
        scan_doctype3(t3, s3, dTD)
    end
  end

  defp scan_doctype3(']' ++ t, s0, dTD) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {_, t1, s1} = strip(t, s)
    s2 = fetch_DTD(dTD, s1)
    check_decl(s2)
    t2 = scan_mandatory('>', t1, 1, s2, :expected_doctype_end_tag)
    {t2, s2}
  end

  defp scan_doctype3(t, s, dTD) do
    {_, t1, s1} = scan_markup_decl(t, s)
    scan_doctype3(t1, s1, dTD)
  end

  defp fetch_DTD(:undefined, s = r_xmerl_scanner(doctype_DTD: uRI))
       when is_list(uRI) do
    fetch_DTD(
      {:system, uRI},
      r_xmerl_scanner(s, doctype_DTD: :option_provided)
    )
  end

  defp fetch_DTD(:undefined, s) do
    s
  end

  defp fetch_DTD(dTDSpec, s) do
    case fetch_and_parse(dTDSpec, s, [{:text_decl, true}, {:environment, {:external, :subset}}]) do
      newS when elem(newS, 0) === :xmerl_scanner ->
        newS

      {_Res, _Tail, _Sx} ->
        s
    end
  end

  defp fetch_and_parse(
         extSpec,
         s = r_xmerl_scanner(fetch_fun: fetch, rules: rules, xmlbase: xMLBase),
         options0
       ) do
    retS =
      case fetch.(extSpec, s) do
        {:ok, newS} ->
          newS

        {:ok, :not_fetched, newS} ->
          newS

        {:ok, dataRet,
         newS =
             r_xmerl_scanner(
               fetch_path: fetchPath,
               user_state: uState,
               event_fun: event,
               hook_fun: hook,
               fetch_fun: fetch1,
               close_fun: close1,
               continuation_fun: cont,
               acc_fun: acc,
               rules_read_fun: read,
               rules_write_fun: write,
               validation: valid,
               quiet: quiet,
               encoding: charset
             )} ->
          evS = event_state(newS)
          hoS = hook_state(newS)
          feS = fetch_state(newS)
          coS = cont_state(newS)

          options =
            options0 ++
              [
                {:fetch_path, fetchPath},
                {:user_state, uState},
                {:rules, rules},
                {:event_fun, event, evS},
                {:hook_fun, hook, hoS},
                {:fetch_fun, fetch1, feS},
                {:close_fun, close1},
                {:continuation_fun, cont, coS},
                {:rules, read, write, ''},
                {:acc_fun, acc},
                {:validation, valid},
                {:quiet, quiet},
                {:encoding, charset}
              ]

          case dataRet do
            {:file, f} ->
              int_file_decl(f, options, charset)

            {:string, string} ->
              int_string_decl(string, options, xMLBase, :file_name_unknown)

            _ ->
              {dataRet, [], newS}
          end

        error ->
          cond do
            r_xmerl_scanner(s, :quiet) ->
              :ok

            true ->
              :error_logger.error_msg(
                '~p- fatal: ~p~n',
                [1350, {:error_fetching_DTD, {extSpec, error}}]
              )

              :ok
          end

          fatal({:error_fetching_DTD, {extSpec, error}}, s)
      end

    case retS do
      r_xmerl_scanner() ->
        r_xmerl_scanner(retS,
          text_decl: false,
          environment: r_xmerl_scanner(s, :environment)
        )

      _ ->
        retS
    end
  end

  defp fetch_not_parse(extSpec, s = r_xmerl_scanner(fetch_fun: fetch)) do
    case fetch.(extSpec, s) do
      {:ok, :not_fetched, _NewS} ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [1363, {:error_fetching_external_source, extSpec}]
            )

            :ok
        end

        fatal({:error_fetching_external_source, extSpec}, s)

      {:ok, dataRet, newS} ->
        {string, locationName} =
          case dataRet do
            {:file, f} ->
              {get_file(f, s), f}

            {:string, str} ->
              {:erlang.binary_to_list(str), :file_name_unknown}

            {:http, uRI} ->
              {{:http, uRI}, uRI}

            _ ->
              dataRet
          end

        {string, r_xmerl_scanner(newS, filename: locationName)}

      _ ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [1377, {:error_fetching_external_resource, extSpec}]
            )

            :ok
        end

        fatal({:error_fetching_external_resource, extSpec}, s)
    end
  end

  defp get_file(f, s) do
    case :file.read_file(f) do
      {:ok, bin} ->
        :erlang.binary_to_list(bin)

      err ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [1386, {:error_reading_file, f, err}]
            )

            :ok
        end

        fatal({:error_reading_file, f, err}, s)
    end
  end

  defp check_decl(r_xmerl_scanner(validation: v)) when v !== :dtd do
    :ok
  end

  defp check_decl(r_xmerl_scanner(rules: tab) = s) do
    check_notations(tab, s)
    check_elements(tab, s)
    check_entities(tab, s)
  end

  defp check_notations(tab, s) do
    case :ets.match(
           tab,
           {{:notation, :"$1"}, :undeclared}
         ) do
      [[]] ->
        :ok

      [] ->
        :ok

      [l] when is_list(l) ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [1404, {:error_missing_declaration_in_DTD, hd(l)}]
            )

            :ok
        end

        fatal({:error_missing_declaration_in_DTD, hd(l)}, s)

      err ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [1406, {:error_missing_declaration_in_DTD, err}]
            )

            :ok
        end

        fatal({:error_missing_declaration_in_DTD, err}, s)
    end
  end

  defp check_elements(tab, s) do
    case (try do
            :ets.match(tab, {{:elem_def, :_}, :"$2"}, 10)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {_, _} = m ->
        fun = fn
          {match, :"$end_of_table"}, _F ->
            :lists.foreach(
              fn x ->
                check_elements2(x, s)
              end,
              match
            )

            :ok

          :"$end_of_table", _ ->
            :ok

          {match, cont}, f ->
            :lists.foreach(
              fn x ->
                check_elements2(x, s)
              end,
              match
            )

            f.(:ets.match(cont), f)
        end

        fun.(m, fun)

      :"$end_of_table" ->
        :ok

      err ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [1425, {:error_missing_declaration_in_DTD, err}]
            )

            :ok
        end

        fatal({:error_missing_declaration_in_DTD, err}, s)
    end
  end

  defp check_elements2([r_xmlElement(attributes: attrs)], s) do
    check_attributes(attrs, s)
  end

  defp check_elements2(_, _) do
    :ok
  end

  defp check_attributes([{n1, :ID, _, _, _} = attr | rest], s) do
    case :lists.keysearch(:ID, 2, rest) do
      {:value, att2} ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [1438, {:error_more_than_one_ID_def, n1, :erlang.element(1, att2)}]
            )

            :ok
        end

        fatal(
          {:error_more_than_one_ID_def, n1, :erlang.element(1, att2)},
          s
        )

      _ ->
        :ok
    end

    vc_ID_Attribute_Default(attr, s)
    check_attributes(rest, s)
  end

  defp check_attributes(
         [{_, {:enumeration, _}, _, _, _} = attr | t],
         s
       ) do
    vc_Enumeration(attr, s)
    check_attributes(t, s)
  end

  defp check_attributes([{_, ent, _, _, _} = attr | t], s)
       when ent == :ENTITY or ent == :ENTITIES do
    vc_Entity_Name(attr, s)
    check_attributes(t, s)
  end

  defp check_attributes([_ | t], s) do
    check_attributes(t, s)
  end

  defp check_attributes([], _S) do
    :ok
  end

  defp check_entities(tab, s = r_xmerl_scanner(validation: :dtd)) do
    case :ets.match(tab, {{:entity, :"$1"}, :undeclared}) do
      [[]] ->
        :ok

      [] ->
        :ok

      [l] when is_list(l) ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [1461, {:error_missing_declaration_in_DTD, hd(l)}]
            )

            :ok
        end

        fatal({:error_missing_declaration_in_DTD, hd(l)}, s)

      err ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [1463, {:error_missing_declaration_in_DTD, err}]
            )

            :ok
        end

        fatal({:error_missing_declaration_in_DTD, err}, s)
    end
  end

  defp check_entities(_, _) do
    :ok
  end

  defp check_decl2(s = r_xmerl_scanner(rules: tab)) do
    check_referenced_ids(tab, s)
  end

  defp check_referenced_ids(tab, s) do
    case :ets.match(tab, {{:id, :"$1"}, :undeclared}) do
      [[]] ->
        :ok

      [] ->
        :ok

      [l] when is_list(l) ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [1479, {:error_missing_declaration_in_DTD, hd(l)}]
            )

            :ok
        end

        fatal({:error_missing_declaration_in_DTD, hd(l)}, s)

      err ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [1481, {:error_missing_declaration_in_DTD, err}]
            )

            :ok
        end

        fatal({:error_missing_declaration_in_DTD, err}, s)
    end
  end

  defp scan_ext_subset([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_ext_subset(moreBytes, s1)
      end,
      fn s1 ->
        {[], s1}
      end,
      s
    )
  end

  defp scan_ext_subset('%' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {t1, s1} = scan_decl_sep(t, s)
    scan_ext_subset(t1, s1)
  end

  defp scan_ext_subset('<![' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 3)
    {_, t1, s1} = strip(t, s)
    {_, t2, s2} = scan_conditional_sect(t1, s1)
    scan_ext_subset(t2, s2)
  end

  defp scan_ext_subset(t, s)
       when hd(t) == 32 or hd(t) == 13 or
              hd(t) == 10 or hd(t) == 9 do
    {_, t1, s1} = strip(t, s)
    scan_ext_subset(t1, s1)
  end

  defp scan_ext_subset(t, s) do
    {_, t1, s1} = scan_markup_decl(t, s)
    scan_ext_subset(t1, s1)
  end

  defp scan_decl_sep(t, s) do
    {pERefName, t1, s1} = scan_pe_reference(t, s)

    {expandedRef, s2} =
      case expand_pe_reference(pERefName, s1, :as_PE) do
        tuple when is_tuple(tuple) ->
          {expRef, _Sx} = fetch_not_parse(tuple, s1)
          {expRef, s1}

        expRef ->
          {expRef, s1}
      end

    {_, tRef, s3} = strip(expandedRef, s2)
    {_, s4} = scan_ext_subset(tRef, s3)
    {t1, s4}
  end

  defp scan_conditional_sect([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_conditional_sect(moreBytes, s1)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_conditional_sect('IGNORE' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 6)
    {_, t1, s1} = strip(t, s)
    t2 = scan_mandatory('[', t1, 1, s, :expected_IGNORE_bracket)
    {_, t3, s3} = strip(t2, s1)
    scan_ignore(t3, s3)
  end

  defp scan_conditional_sect('INCLUDE' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 7)
    {_, t1, s1} = strip(t, s)
    t2 = scan_mandatory('[', t1, 1, s, :expected_INCLUDE_bracket)
    {_, t3, s3} = strip(t2, s1)
    scan_include(t3, s3)
  end

  defp scan_conditional_sect('%' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {pERefName, t1, s1} = scan_pe_reference(t, s)
    expRef = expand_pe_reference(pERefName, s1, :as_PE)
    {_, t2, s2} = strip(expRef ++ t1, s1)
    scan_conditional_sect(t2, s2)
  end

  defp scan_ignore(str, s) do
    scan_ignore(str, s, 0)
  end

  defp scan_ignore([], s = r_xmerl_scanner(continuation_fun: f), level) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_ignore(moreBytes, s1, level)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_ignore('<![' ++ t, s0, level) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 3)
    scan_ignore(t, s, level + 1)
  end

  defp scan_ignore(']]>' ++ t, s0, 0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 3)
    {[], t, s}
  end

  defp scan_ignore(']]>' ++ t, s0, level) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 3)
    scan_ignore(t, s, level - 1)
  end

  defp scan_ignore([_H | t], s0, level) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_ignore(t, s, level)
  end

  defp scan_include([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_include(moreBytes, s1)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_include(']]>' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 3)
    {[], t, s}
  end

  defp scan_include('%' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {pERefName, t1, s1} = scan_pe_reference(t, s)
    expRef = expand_pe_reference(pERefName, s1, :as_PE)
    {_, t2, s2} = strip(expRef ++ t1, s1)
    scan_include(t2, s2)
  end

  defp scan_include('<![' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 3)
    {_, t1, s1} = strip(t, s)
    {_, t2, s2} = scan_conditional_sect(t1, s1)
    {_, t3, s3} = strip(t2, s2)
    scan_include(t3, s3)
  end

  defp scan_include(t, s) do
    {_, t1, s1} = scan_markup_decl(t, s)
    scan_include(t1, s1)
  end

  defp scan_markup_decl([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_markup_decl(moreBytes, s1)
      end,
      fn s1 ->
        {[], [], s1}
      end,
      s
    )
  end

  defp scan_markup_decl('<!--' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 4)
    scan_comment(t, s)
  end

  defp scan_markup_decl('<?' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 2)
    {_PI, t1, s1} = scan_pi(t, s, _Pos = :markup, [])
    strip(t1, s1)
  end

  defp scan_markup_decl(
         '<!ELEMENT' ++ t,
         r_xmerl_scanner(rules_read_fun: read, rules_write_fun: write, rules_delete_fun: delete) =
           s0
       ) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 9)
    {_, t1, s1} = mandatory_strip(t, s)
    {ename, _NamespaceInfo, t2, s2} = scan_name(t1, s1)

    element =
      case read.(:elem_def, ename, s2) do
        el = r_xmlElement(elementdef: decl) when decl !== :undeclared ->
          case r_xmerl_scanner(s2, :validation) do
            :dtd ->
              cond do
                r_xmerl_scanner(s2, :quiet) ->
                  :ok

                true ->
                  :error_logger.error_msg(
                    '~p- fatal: ~p~n',
                    [1662, {:already_defined, ename}]
                  )

                  :ok
              end

              fatal({:already_defined, ename}, s2)

            _ ->
              delete.(:elem_def, ename, s2)
              el
          end

        el = r_xmlElement() ->
          delete.(:elem_def, ename, s2)
          el

        :undefined ->
          r_xmlElement()
      end

    {_, t3, s3} = mandatory_strip(t2, s2)
    {edef, t4, s4} = scan_contentspec(t3, s3)
    {_, t5, s5} = strip(t4, s4)
    {'>' ++ t6, s6} = scan_element_completion(t5, s5)

    s7 =
      write.(
        :elem_def,
        ename,
        r_xmlElement(element,
          name: ename,
          content: edef,
          elementdef: r_xmerl_scanner(s6, :environment)
        ),
        r_xmerl_scanner(s6, col: r_xmerl_scanner(s6, :col) + 1)
      )

    strip(t6, s7)
  end

  defp scan_markup_decl('<!ENTITY' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 8)
    {_, t1, s1} = mandatory_strip(t, s)
    {t2, s2} = scan_entity(t1, s1)
    strip(t2, s2)
  end

  defp scan_markup_decl('<!NOTATION' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 10)
    {_, t1, s1} = mandatory_strip(t, s)
    {t2, s2} = scan_notation_decl(t1, s1)
    strip(t2, s2)
  end

  defp scan_markup_decl(
         '<!ATTLIST' ++ t,
         r_xmerl_scanner(rules_read_fun: read, rules_write_fun: write, rules_delete_fun: delete) =
           s0
       ) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 9)
    {_, t1, s1} = mandatory_strip(t, s)
    {ename, _NamespaceInfo, t2, s2} = scan_name(t1, s1)
    {attributes, t4, s4} = scan_attdef(t2, s2)

    {eDEF, mergedAttrs} =
      case read.(:elem_def, ename, s4) do
        :undefined ->
          {r_xmlElement(), update_attributes(attributes, [])}

        edef = r_xmlElement(attributes: oldAttrs) ->
          delete.(:elem_def, ename, s4)
          {edef, update_attributes(attributes, oldAttrs)}
      end

    newEdef = r_xmlElement(eDEF, name: ename, attributes: mergedAttrs)
    s5 = write.(:elem_def, ename, newEdef, s4)
    t5 = t4
    strip(t5, s5)
  end

  defp scan_markup_decl(_Str, s) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg('~p- fatal: ~p~n', [1726, :expected_markup])
        :ok
    end

    fatal(:expected_markup, s)
  end

  defp scan_element_completion(t, s) do
    scan_markup_completion_gt(t, s)
  end

  defp update_attributes(newAttrs, oldAttrs) do
    update_attributes1(newAttrs, :lists.reverse(oldAttrs))
  end

  defp update_attributes1(
         [
           a = {name, _Type, _DefaultV, _DefaultD, _Env}
           | attrs
         ],
         oldAttrs
       ) do
    case :lists.keymember(name, 1, oldAttrs) do
      true ->
        update_attributes1(attrs, oldAttrs)

      false ->
        update_attributes1(attrs, [a | oldAttrs])
    end
  end

  defp update_attributes1([], acc) do
    :lists.reverse(acc)
  end

  defp scan_attdef([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_attdef(moreBytes, s1)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_attdef(t, s) do
    scan_attdef(t, s, _AttrAcc = [])
  end

  defp scan_attdef([], s = r_xmerl_scanner(continuation_fun: f), attrs) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_attdef(moreBytes, s1, attrs)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_attdef('>' ++ t, s0, attrs) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {:lists.reverse(attrs), t, s}
  end

  defp scan_attdef('%' ++ _T, s = r_xmerl_scanner(environment: :prolog), _Attrs) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [1766, {:error, {:wfc_PEs_In_Internal_Subset}}]
        )

        :ok
    end

    fatal({:error, {:wfc_PEs_In_Internal_Subset}}, s)
  end

  defp scan_attdef('%' ++ t, s0, attrs) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {pERefName, t1, s1} = scan_pe_reference(t, s)
    expRef = expand_pe_reference(pERefName, s1, :as_PE)
    {_, t2, s2} = strip(expRef ++ t1, s1)
    scan_attdef(t2, s2, attrs)
  end

  defp scan_attdef(t, s, attrs) do
    {_, t1, s1} = mandatory_strip(t, s)
    scan_attdef2(t1, s1, attrs)
  end

  defp scan_attdef2('>' ++ t, s0, attrs) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {:lists.reverse(attrs), t, s}
  end

  defp scan_attdef2('%' ++ _T, s = r_xmerl_scanner(environment: :prolog), _Attrs) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [1781, {:error, {:wfc_PEs_In_Internal_Subset}}]
        )

        :ok
    end

    fatal({:error, {:wfc_PEs_In_Internal_Subset}}, s)
  end

  defp scan_attdef2('%' ++ t, s0, attrs) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {pERefName, t1, s1} = scan_pe_reference(t, s)
    expRef = expand_pe_reference(pERefName, s1, :as_PE)
    {_, t2, s2} = strip(expRef ++ t1, s1)
    scan_attdef2(t2, s2, attrs)
  end

  defp scan_attdef2(t, s, attrs) do
    {attName, _NamespaceInfo, t1, s1} = scan_name(t, s)
    {_, t2, s2} = mandatory_strip(t1, s1)
    {attType, t3, s3} = scan_att_type(t2, s2)
    {_, t4, s4} = mandatory_strip(t3, s3)
    {{defaultDecl, defaultValue}, t5, s5} = scan_default_decl(t4, s4, attType)
    {_, t6, s6} = strip(t5, s5)
    attr = {attName, attType, defaultValue, defaultDecl, r_xmerl_scanner(s, :environment)}
    scan_attdef2(t6, s6, [attr | attrs])
  end

  defp scan_att_type([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_att_type(moreBytes, s1)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_att_type('CDATA' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 5)
    {:CDATA, t, s}
  end

  defp scan_att_type('IDREFS' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 6)
    {:IDREFS, t, s}
  end

  defp scan_att_type('IDREF' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 5)
    {:IDREF, t, s}
  end

  defp scan_att_type('ID' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 2)
    {:ID, t, s}
  end

  defp scan_att_type('ENTITY' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 6)
    {:ENTITY, t, s}
  end

  defp scan_att_type('ENTITIES' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 8)
    {:ENTITIES, t, s}
  end

  defp scan_att_type('NMTOKENS' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 8)
    {:NMTOKENS, t, s}
  end

  defp scan_att_type('NMTOKEN' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 7)
    {:NMTOKEN, t, s}
  end

  defp scan_att_type('NOTATION' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 8)
    {_, t1, s1} = mandatory_strip(t, s)
    t2 = scan_mandatory('(', t1, 1, s1, :expected_parenthesis_after_NOTATION)
    s2 = s1
    {_, t3, s3} = strip(t2, s2)
    {name, _NamespaceInfo, t4, s4} = scan_name(t3, s3)
    notation_exists(name, s4)
    {_, t5, s5} = strip(t4, s4)
    scan_notation_type(t5, s5, [name])
  end

  defp scan_att_type('(' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {_, t1, s1} = strip(t, s)
    {nmToken, _NamespaceInfo, t2, s2} = scan_nmtoken(t1, s1)
    {_, t3, s3} = strip(t2, s2)
    scan_enumeration(t3, s3, [nmToken])
  end

  defp scan_att_type('%' ++ _T, s = r_xmerl_scanner(environment: :prolog)) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [1850, {:error, {:wfc_PEs_In_Internal_Subset}}]
        )

        :ok
    end

    fatal({:error, {:wfc_PEs_In_Internal_Subset}}, s)
  end

  defp scan_att_type('%' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {pERefName, t1, s1} = scan_pe_reference(t, s)
    expRef = expand_pe_reference(pERefName, s1, :in_literal)
    {expRef, t1, s1}
  end

  defp scan_notation_type([], s = r_xmerl_scanner(continuation_fun: f), acc) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_notation_type(moreBytes, s1, acc)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_notation_type(')' ++ t, s0, acc) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {{:notation, :lists.reverse(acc)}, t, s}
  end

  defp scan_notation_type('|' ++ t, s0, acc) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {_, t1, s1} = strip(t, s)
    {name, _NamespaceInfo, t2, s2} = scan_name(t1, s1)
    notation_exists(name, s2)
    {_, t3, s3} = strip(t2, s2)
    scan_notation_type(t3, s3, [name | acc])
  end

  defp notation_exists(
         name,
         r_xmerl_scanner(rules_read_fun: read, rules_write_fun: write) = s
       ) do
    case read.(:notation, name, s) do
      :undefined ->
        write.(:notation, name, :undeclared, s)

      _Value ->
        :ok
    end
  end

  defp scan_enumeration([], s = r_xmerl_scanner(continuation_fun: f), acc) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_enumeration(moreBytes, s1, acc)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_enumeration(')' ++ t, s0, acc) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {{:enumeration, :lists.reverse(acc)}, t, s}
  end

  defp scan_enumeration('|' ++ t, s0, acc) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {_, t1, s1} = strip(t, s)
    {nmToken, _NamespaceInfo, t2, s2} = scan_nmtoken(t1, s1)
    {_, t3, s3} = strip(t2, s2)
    scan_enumeration(t3, s3, [nmToken | acc])
  end

  defp scan_default_decl([], s = r_xmerl_scanner(continuation_fun: f), type) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_default_decl(moreBytes, s1, type)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_default_decl('#REQUIRED' ++ t, s0, _Type) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 9)
    {{:"#REQUIRED", :no_value}, t, s}
  end

  defp scan_default_decl('#IMPLIED' ++ t, s0, _Type) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 8)
    {{:"#IMPLIED", :no_value}, t, s}
  end

  defp scan_default_decl('#FIXED' ++ t, s0, type) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 6)
    {_, t1, s1} = mandatory_strip(t, s)
    {value, t2, s2, _} = default_value(t1, s1, type)
    {{:"#FIXED", value}, t2, s2}
  end

  defp scan_default_decl(str, s, type) do
    {value, t1, s1, _} = default_value(str, s, type)
    {{:no_decl, value}, t1, s1}
  end

  defp default_value(t, s, type) do
    {_Val, _T1, _S1, _} = scan_att_value(t, s, type)
  end

  defp scan_entity([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_entity(moreBytes, s1)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_entity('%' ++ t, r_xmerl_scanner(rules_write_fun: write) = s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {_, t1, s1} = mandatory_strip(t, s)
    {pEName, _NamespaceInfo, t2, s2} = scan_name_no_colons(t1, s1)
    {_, t3, s3} = mandatory_strip(t2, s2)
    {pEDef, t4, s4} = scan_pe_def(t3, s3, pEName)
    {_, t5, s5} = strip(t4, s4)
    {'>' ++ t6, s6} = scan_entity_completion(t5, s5)
    s7 = write.(:parameter_entity, pEName, pEDef, s6)
    {t6, s7}
  end

  defp scan_entity(
         t,
         r_xmerl_scanner(rules_write_fun: write, rules_read_fun: read, rules_delete_fun: delete) =
           s
       ) do
    {eName, _NamespaceInfo, t1, s1} =
      scan_name_no_colons(
        t,
        s
      )

    {_, t2, s2} = mandatory_strip(t1, s1)
    {eDef, entType, t3, s3} = scan_entity_def(t2, s2, eName)
    check_entity_recursion(eName, s3)
    {_, t4, s4} = strip(t3, s3)
    {'>' ++ t5, s5} = scan_entity_completion(t4, s4)

    case read.(:entity, eName, s5) do
      :undeclared ->
        delete.(:entity, eName, s5)

      _ ->
        :ok
    end

    s6 = write.(:entity, eName, {r_xmerl_scanner(s5, :environment), entType, eDef}, s5)
    {t5, s6}
  end

  defp scan_entity_completion(t, s) do
    scan_markup_completion_gt(t, s)
  end

  defp scan_entity_def([], s = r_xmerl_scanner(continuation_fun: f), eName) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_entity_def(moreBytes, s1, eName)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_entity_def('\'' ++ t, s0, eName) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {eVal, t1, s1} = scan_entity_value(t, s, ?', eName, :general)
    {eVal, :internal, t1, s1}
  end

  defp scan_entity_def('"' ++ t, s0, eName) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {eVal, t1, s1} = scan_entity_value(t, s, ?", eName, :general)
    {eVal, :internal, t1, s1}
  end

  defp scan_entity_def(str, s, eName) do
    {extID, t1, s1} = scan_external_id(str, s)
    {nData, t2, s2} = scan_ndata_decl(t1, s1)

    case nData do
      {:ndata, _} ->
        {{extID, nData}, :external, t2, s2}

      _ ->
        case fetch_and_parse(extID, s2, [
               {:text_decl, true},
               {:environment, {:external, {:entity, eName}}}
             ]) do
          {{_USret, entity}, _Tail, _Sx} ->
            {entity, :external, t2, s2}

          {entity, _Tail, sx} ->
            oldRef = r_xmerl_scanner(s2, :entity_references)
            newRef = r_xmerl_scanner(sx, :entity_references)
            {entity, :external, t2, r_xmerl_scanner(s2, entity_references: oldRef ++ newRef)}

          {:error, :enoent} ->
            {{:error, :enoent}, :external, t2, s2}
        end
    end
  end

  defp scan_ndata_decl([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_ndata_decl(moreBytes, s1)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_ndata_decl(str = '>' ++ _T, s) do
    {[], str, s}
  end

  defp scan_ndata_decl(t, s) do
    {_, t1, s1} = mandatory_strip(t, s)
    scan_ndata_decl2(t1, s1)
  end

  defp scan_ndata_decl2(str = '>' ++ _T, s) do
    {[], str, s}
  end

  defp scan_ndata_decl2(
         'NDATA' ++ t,
         s0 = r_xmerl_scanner(rules_read_fun: read, rules_write_fun: write)
       ) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 5)
    {_, t1, s1} = mandatory_strip(t, s)
    {name, _NamespaceInfo, t2, s2} = scan_name(t1, s1)

    case read.(:notation, name, s2) do
      :undefined ->
        write.(:notation, name, :undeclared, s2)
        {{:ndata, name}, t2, s2}

      _Value ->
        {{:ndata, name}, t2, s2}
    end
  end

  defp scan_element(t, s, pos) do
    scan_element(
      t,
      s,
      pos,
      r_xmerl_scanner(s, :space),
      _Lang = [],
      _Parents = [],
      r_xmlNamespace()
    )
  end

  defp scan_element(t, s = r_xmerl_scanner(line: l, col: c), pos, spaceDefault, lang, parents, nS) do
    {name, namespaceInfo, t1, s1} = scan_name(t, s)
    vc_Element_valid(name, namespaceInfo, s)
    {_, t2, s2} = strip(t1, s1)

    scan_element(
      t2,
      s2,
      pos,
      name,
      l,
      c,
      _Attrs = [],
      lang,
      parents,
      namespaceInfo,
      nS,
      spaceDefault
    )
  end

  defp scan_element(
         '/',
         s = r_xmerl_scanner(continuation_fun: f),
         pos,
         name,
         startL,
         startC,
         attrs,
         lang,
         parents,
         nSI,
         nS,
         spaceDefault
       ) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_element(
          '/' ++ moreBytes,
          s1,
          pos,
          name,
          startL,
          startC,
          attrs,
          lang,
          parents,
          nSI,
          nS,
          spaceDefault
        )
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_element(
         [],
         s = r_xmerl_scanner(continuation_fun: f),
         pos,
         name,
         startL,
         startC,
         attrs,
         lang,
         parents,
         nSI,
         nS,
         spaceDefault
       ) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_element(
          moreBytes,
          s1,
          pos,
          name,
          startL,
          startC,
          attrs,
          lang,
          parents,
          nSI,
          nS,
          spaceDefault
        )
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_element(
         '/>' ++ t,
         s0 =
           r_xmerl_scanner(
             hook_fun: hook,
             event_fun: event,
             line: l,
             col: c,
             xmlbase_cache: xMLBase
           ),
         pos,
         name,
         _StartL,
         _StartC,
         attrs0,
         lang,
         parents,
         nSI,
         namespace,
         _SpaceDefault
       ) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 2)
    attrs = :lists.reverse(attrs0)
    e = processed_whole_element(s, pos, name, attrs, lang, parents, nSI, namespace)
    r_xmlElement(attributes: attrs1) = e
    wfc_unique_att_spec(attrs1, s)

    s1 =
      r_xmerl_scanner() =
      event.(
        r_xmerl_event(event: :ended, line: l, col: c, data: e),
        s0
      )

    {ret, s2} = hook.(e, s1)
    s2b = r_xmerl_scanner(s2, xmlbase: xMLBase)
    {ret, t, s2b}
  end

  defp scan_element(
         '>',
         s = r_xmerl_scanner(continuation_fun: f),
         pos,
         name,
         startL,
         startC,
         attrs,
         lang,
         parents,
         nSI,
         nS,
         spaceDefault
       ) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_element(
          '>' ++ moreBytes,
          s1,
          pos,
          name,
          startL,
          startC,
          attrs,
          lang,
          parents,
          nSI,
          nS,
          spaceDefault
        )
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_element(
         '>' ++ t,
         s0 =
           r_xmerl_scanner(
             event_fun: event,
             hook_fun: hook,
             line: l,
             col: c,
             xmlbase_cache: xMLBase,
             space: spaceOption
           ),
         pos,
         name,
         startL,
         startC,
         attrs0,
         lang,
         parents,
         nSI,
         namespace,
         spaceDefault
       ) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    attrs = :lists.reverse(attrs0)
    e0 = processed_whole_element(s, pos, name, attrs, lang, parents, nSI, namespace)
    r_xmlElement(attributes: attrs1) = e0
    wfc_unique_att_spec(attrs1, s)

    xMLSpace =
      case :lists.keysearch(:"xml:space", r_xmlAttribute(:name), attrs1) do
        false ->
          spaceDefault

        {:value, r_xmlAttribute(value: 'default')} ->
          spaceOption

        {:value, r_xmlAttribute(value: 'preserve')} ->
          :preserve

        _ ->
          spaceDefault
      end

    ^e0 = processed_whole_element(s, pos, name, attrs1, lang, parents, nSI, namespace)

    s1 =
      r_xmerl_scanner() =
      event.(
        r_xmerl_event(event: :started, line: startL, col: startC, data: e0),
        s
      )

    {content, t1, s2} =
      scan_content(
        t,
        s1,
        name,
        attrs1,
        xMLSpace,
        r_xmlElement(e0, :language),
        [{name, pos} | parents],
        namespace
      )

    element =
      r_xmlElement(e0,
        content: content,
        xmlbase: r_xmlElement(e0, :xmlbase)
      )

    s3 =
      r_xmerl_scanner() =
      event.(
        r_xmerl_event(event: :ended, line: l, col: c, data: element),
        s2
      )

    {ret, s4} = hook.(element, s3)
    s4b = r_xmerl_scanner(s4, xmlbase: xMLBase)
    {ret, t1, s4b}
  end

  defp scan_element(t, s, pos, name, startL, startC, attrs, lang, parents, nSI, nS, spaceDefault) do
    {attName, namespaceInfo, t1, s1} = scan_name(t, s)
    {t2, s2} = scan_eq(t1, s1)
    {attType, _DefaultDecl} = get_att_type(s2, attName, name)
    {attValue, t3a, s3a, isNorm} = scan_att_value(t2, s2, attType)
    newNS = check_namespace(attName, namespaceInfo, attValue, nS)
    {t3, s3} = wfc_whitespace_betw_attrs(t3a, s3a)
    {_, t4, s4} = strip(t3, s3)

    attrPos =
      case attrs do
        [] ->
          1

        [r_xmlAttribute(pos: p) | _] ->
          p + 1
      end

    attr =
      r_xmlAttribute(
        name: attName,
        parents: [{name, pos} | parents],
        pos: attrPos,
        language: lang,
        nsinfo: namespaceInfo,
        value: attValue,
        normalized: isNorm
      )

    xMLBase =
      cond do
        attName == :"xml:base" ->
          resolve_relative_uri(attValue, r_xmerl_scanner(s4, :xmlbase))

        true ->
          r_xmerl_scanner(s4, :xmlbase)
      end

    r_xmerl_scanner(event_fun: event, line: line, col: col) = s4

    s5 =
      event.(
        r_xmerl_event(event: :ended, line: line, col: col, data: attr),
        r_xmerl_scanner(s4, xmlbase: xMLBase, xmlbase_cache: r_xmerl_scanner(s, :xmlbase))
      )

    scan_element(
      t4,
      s5,
      pos,
      name,
      startL,
      startC,
      [attr | attrs],
      lang,
      parents,
      nSI,
      newNS,
      spaceDefault
    )
  end

  defp get_default_attrs(s = r_xmerl_scanner(rules_read_fun: read), elemName) do
    case read.(:elem_def, elemName, s) do
      r_xmlElement(attributes: attrs) ->
        for {attName, _, attValue, _, _} <- attrs,
            attValue !== :no_value do
          {attName, attValue}
        end

      _ ->
        []
    end
  end

  defp get_att_type(s = r_xmerl_scanner(rules_read_fun: read), attName, elemName) do
    case read.(:elem_def, elemName, s) do
      r_xmlElement(attributes: attrs) ->
        case :lists.keysearch(attName, 1, attrs) do
          {:value, {_, attType, _, defaultDecl, _}} ->
            {attType, defaultDecl}

          _ ->
            {:CDATA, :no_value}
        end

      _ ->
        {:CDATA, :no_value}
    end
  end

  defp resolve_relative_uri(newBase = '/' ++ _, currentBase) do
    case :xmerl_uri.parse(currentBase) do
      {:error, _Reason} ->
        newBase

      {scheme, host, port, _Path, _Query} ->
        :erlang.atom_to_list(scheme) ++ host ++ ':' ++ :erlang.integer_to_list(port) ++ newBase
    end
  end

  defp resolve_relative_uri(newBase, currentBase) do
    :filename.join(currentBase, newBase)
  end

  defp processed_whole_element(
         s =
           r_xmerl_scanner(
             hook_fun: _Hook,
             xmlbase: xMLBase,
             line: _L,
             col: _C,
             event_fun: _Event
           ),
         pos,
         name,
         attrs,
         lang,
         parents,
         nSI,
         namespace
       ) do
    language = check_language(attrs, lang)

    allAttrs =
      case r_xmerl_scanner(s, :default_attrs) do
        true ->
          defaultAttrs =
            for {attName, attValue} <-
                  get_default_attrs(
                    s,
                    name
                  ),
                attValue !== :no_value,
                not :lists.keymember(attName, r_xmlAttribute(:name), attrs) do
              r_xmlAttribute(
                name: attName,
                parents: [{name, pos} | parents],
                language: lang,
                nsinfo: nSI,
                namespace: namespace,
                value: attValue,
                normalized: true
              )
            end

          :lists.append(attrs, defaultAttrs)

        false ->
          attrs
      end

    {expName, expAttrs} =
      case r_xmerl_scanner(s, :namespace_conformant) do
        true ->
          tempNamespace = r_xmlNamespace(namespace, default: [])

          expAttrsX =
            for a <- allAttrs do
              r_xmlAttribute(a,
                namespace: namespace,
                expanded_name:
                  expanded_name(
                    r_xmlAttribute(a, :name),
                    r_xmlAttribute(a, :nsinfo),
                    tempNamespace,
                    s
                  )
              )
            end

          {expanded_name(name, nSI, namespace, s), expAttrsX}

        false ->
          {name, allAttrs}
      end

    r_xmlElement(
      name: name,
      xmlbase: xMLBase,
      pos: pos,
      parents: parents,
      attributes: expAttrs,
      language: language,
      expanded_name: expName,
      nsinfo: nSI,
      namespace: namespace
    )
  end

  defp check_language([r_xmlAttribute(name: :"xml:lang", value: lang) | _], _) do
    lang
  end

  defp check_language([_ | t], lang) do
    check_language(t, lang)
  end

  defp check_language([], lang) do
    lang
  end

  defp check_namespace(:xmlns, _, value, nS) do
    r_xmlNamespace(nS, default: :erlang.list_to_atom(value))
  end

  defp check_namespace(_, {'xmlns', prefix}, value, nS = r_xmlNamespace(nodes: ns)) do
    r_xmlNamespace(nS, nodes: keyreplaceadd(prefix, 1, ns, {prefix, :erlang.list_to_atom(value)}))
  end

  defp check_namespace(_, _, _, nS) do
    nS
  end

  defp expanded_name(name, [], r_xmlNamespace(default: []), _S) do
    name
  end

  defp expanded_name(name, [], r_xmlNamespace(default: uRI), s) do
    case uRI do
      :"http://www.w3.org/XML/1998/namespace" ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [2303, :cannot_bind_default_namespace_to_xml_namespace_name]
            )

            :ok
        end

        fatal(
          :cannot_bind_default_namespace_to_xml_namespace_name,
          s
        )

      :"http://www.w3.org/2000/xmlns/" ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [2305, :cannot_bind_default_namespace_to_xmlns_namespace_name]
            )

            :ok
        end

        fatal(
          :cannot_bind_default_namespace_to_xmlns_namespace_name,
          s
        )

      _ ->
        {uRI, name}
    end
  end

  defp expanded_name(name, n = {'xmlns', local}, r_xmlNamespace(nodes: ns), s) do
    {_, value} = :lists.keyfind(local, 1, ns)

    case name do
      :"xmlns:xml" when value === :"http://www.w3.org/XML/1998/namespace" ->
        n

      :"xmlns:xml" when value !== :"http://www.w3.org/XML/1998/namespace" ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [2315, {:xml_prefix_cannot_be_redeclared, value}]
            )

            :ok
        end

        fatal({:xml_prefix_cannot_be_redeclared, value}, s)

      :"xmlns:xmlns" ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [2317, {:xmlns_prefix_cannot_be_declared, value}]
            )

            :ok
        end

        fatal({:xmlns_prefix_cannot_be_declared, value}, s)

      _ ->
        case value do
          :"http://www.w3.org/XML/1998/namespace" ->
            cond do
              r_xmerl_scanner(s, :quiet) ->
                :ok

              true ->
                :error_logger.error_msg(
                  '~p- fatal: ~p~n',
                  [2321, {:cannot_bind_prefix_to_xml_namespace, local}]
                )

                :ok
            end

            fatal({:cannot_bind_prefix_to_xml_namespace, local}, s)

          :"http://www.w3.org/2000/xmlns/" ->
            cond do
              r_xmerl_scanner(s, :quiet) ->
                :ok

              true ->
                :error_logger.error_msg(
                  '~p- fatal: ~p~n',
                  [2323, {:cannot_bind_prefix_to_xmlns_namespace, local}]
                )

                :ok
            end

            fatal(
              {:cannot_bind_prefix_to_xmlns_namespace, local},
              s
            )

          _ ->
            n
        end
    end
  end

  defp expanded_name(_Name, {'xml', local}, _NS, _S) do
    {:"http://www.w3.org/XML/1998/namespace", :erlang.list_to_atom(local)}
  end

  defp expanded_name(_Name, {prefix, local}, r_xmlNamespace(nodes: ns), s) do
    case :lists.keysearch(prefix, 1, ns) do
      {:value, {_, uRI}} ->
        {uRI, :erlang.list_to_atom(local)}

      false ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [2337, {:namespace_prefix_not_declared, prefix}]
            )

            :ok
        end

        fatal({:namespace_prefix_not_declared, prefix}, s)
    end
  end

  defp keyreplaceadd(k, pos, [h | t], obj)
       when k == :erlang.element(pos, h) do
    [obj | t]
  end

  defp keyreplaceadd(k, pos, [h | t], obj) do
    [h | keyreplaceadd(k, pos, t, obj)]
  end

  defp keyreplaceadd(_K, _Pos, [], obj) do
    [obj]
  end

  defp scan_att_value([], s = r_xmerl_scanner(continuation_fun: f), aT) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_att_value(moreBytes, s1, aT)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_att_value('%' ++ _T, s = r_xmerl_scanner(environment: :prolog), _AttType) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [2356, {:error, {:wfc_PEs_In_Internal_Subset}}]
        )

        :ok
    end

    fatal({:error, {:wfc_PEs_In_Internal_Subset}}, s)
  end

  defp scan_att_value(
         '%' ++ t,
         s0 =
           r_xmerl_scanner(rules_read_fun: read, rules_write_fun: write, rules_delete_fun: delete),
         attType
       ) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {name, t1, s1} = scan_pe_reference(t, s)

    {expandedRef, s2} =
      case expand_pe_reference(name, s1, :in_literal) do
        tuple when is_tuple(tuple) ->
          {expRef, sx} = fetch_not_parse(tuple, s1)
          {entV, _, _S2} = scan_entity_value(expRef, sx, :no_delim, name, :parameter)
          delete.(:parameter_entity, name, _S2)
          _S3 = write.(:parameter_entity, name, entV, _S2)
          entV2 = read.(:parameter_entity, name, _S3)
          {entV2, _S3}

        expRef ->
          {expRef, s1}
      end

    {_, t2, s3} = strip(expandedRef ++ t1, s2)
    scan_att_value(t2, s3, attType)
  end

  defp scan_att_value([h | t], s0, :CDATA = aT)
       when h == ?" or
              h == ?' do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_att_chars(t, s, h, [], [], aT, false)
  end

  defp scan_att_value([h | t], s0, attType) when h == ?" or h == ?' do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {t1, s1, isNorm} = normalize(t, s, false)
    scan_att_chars(t1, s1, h, [], [], attType, isNorm)
  end

  defp scan_att_chars([], s = r_xmerl_scanner(continuation_fun: f), h, acc, tmpAcc, aT, isNorm) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_att_chars(moreBytes, s1, h, acc, tmpAcc, aT, isNorm)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_att_chars([h | t], s0, h, acc, tmpAcc, attType, isNorm) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    check_att_default_val(r_xmerl_scanner(s, :validation), tmpAcc, attType, s)

    {acc2, s2, isNorm2} =
      cond do
        attType == :CDATA ->
          {acc, s, isNorm}

        true ->
          normalize(acc, s, isNorm)
      end

    {:lists.flatten(:lists.reverse(acc2)), t, s2, isNorm2}
  end

  defp scan_att_chars('&' ++ t, s0, delim, acc, tmpAcc, aT, isNorm) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {expRef, t1, s1} = scan_reference(t, s)

    case markup_delimeter(expRef) do
      true ->
        scan_att_chars(t1, s1, delim, [expRef | acc], [expRef | tmpAcc], aT, isNorm)

      _ ->
        case t do
          '#' ++ _ ->
            {t2, s2, isNorm2} =
              cond do
                hd(expRef) == 32 or hd(expRef) == 13 or
                  hd(expRef) == 10 or hd(expRef) == 9 ->
                  normalize(t1, s1, isNorm)

                true ->
                  {t1, s1, isNorm}
              end

            scan_att_chars(t2, s2, delim, expRef ++ acc, tmpAcc, aT, isNorm2)

          _ ->
            ch = string_to_char_set(r_xmerl_scanner(s, :encoding), expRef)
            scan_att_chars(ch ++ t1, s1, delim, acc, tmpAcc, aT, isNorm)
        end
    end
  end

  defp scan_att_chars('<' ++ _T, s0, _Delim, _Acc, _, _, _) do
    cond do
      r_xmerl_scanner(s0, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg('~p- fatal: ~p~n', [2432, :unexpected_char])
        :ok
    end

    fatal(:unexpected_char, s0)
  end

  defp scan_att_chars([h | t], s0, delim, acc, _TmpAcc, :CDATA, isNorm)
       when h == 32 or h == 13 or h == 10 or h == 9 do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_att_chars(t, s, delim, [?\s | acc], [], :CDATA, isNorm)
  end

  defp scan_att_chars([h | t], s0, delim, acc, tmpAcc, aT, isNorm)
       when h == 32 or h == 13 or h == 10 or h == 9 do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {t1, s1, isNorm2} = normalize(t, s, isNorm)
    check_att_default_val(r_xmerl_scanner(s, :validation), tmpAcc, aT, s1)
    scan_att_chars(t1, s1, delim, [?\s | acc], [], aT, isNorm2)
  end

  defp scan_att_chars(str, s0, delim, acc, tmpAcc, aT, isNorm) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {ch, t} = to_ucs(r_xmerl_scanner(s, :encoding), str)
    valid_Char(r_xmerl_scanner(s, :validation), aT, ch, s)
    scan_att_chars(t, s, delim, [ch | acc], [ch | tmpAcc], aT, isNorm)
  end

  defp markup_delimeter('&') do
    true
  end

  defp markup_delimeter('"') do
    true
  end

  defp markup_delimeter('\'') do
    true
  end

  defp markup_delimeter('<') do
    true
  end

  defp markup_delimeter('>') do
    true
  end

  defp markup_delimeter('%') do
    true
  end

  defp markup_delimeter(_) do
    false
  end

  defp check_att_default_val(:dtd, [], _Ent, _S) do
    :ok
  end

  defp check_att_default_val(:dtd, revName, ent, s) do
    check_att_default_val(:lists.reverse(revName), ent, s)
  end

  defp check_att_default_val(_, _, _, _) do
    :ok
  end

  defp check_att_default_val(name, ent, s = r_xmerl_scanner(rules_write_fun: write))
       when ent == :ENTITY or ent == :ENTITIES do
    case :xmerl_lib.is_letter(hd(name)) do
      true ->
        :ok

      _ ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [2468, {:illegal_first_character, ent, name}]
            )

            :ok
        end

        fatal({:illegal_first_character, ent, name}, s)
    end

    sName = :erlang.list_to_atom(name)
    write.(:entity, sName, :undeclared, s)
  end

  defp check_att_default_val(name, iDR, s = r_xmerl_scanner(rules_write_fun: write))
       when iDR == :IDREF or iDR == :IDREFS do
    case :xmerl_lib.is_letter(hd(name)) do
      true ->
        :ok

      _ ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [2476, {:illegal_first_character, iDR, name}]
            )

            :ok
        end

        fatal({:illegal_first_character, iDR, name}, s)
    end

    sName = :erlang.list_to_atom(name)
    write.(:id, sName, :undeclared, s)
  end

  defp check_att_default_val(
         name,
         :ID,
         s =
           r_xmerl_scanner(rules_write_fun: write, rules_read_fun: read, rules_delete_fun: delete)
       ) do
    case :xmerl_lib.is_name(name) do
      false ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [2485, {:ID_names_must_be_Name_production, name}]
            )

            :ok
        end

        fatal({:ID_names_must_be_Name_production, name}, s)

      _ ->
        :ok
    end

    sName =
      cond do
        is_list(name) ->
          :erlang.list_to_atom(name)

        true ->
          name
      end

    case read.(:id, sName, s) do
      :undeclared ->
        delete.(:id, sName, s)

      ^sName ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [2496, {:values_must_be_unique, :ID, sName}]
            )

            :ok
        end

        fatal({:values_must_be_unique, :ID, sName}, s)

      :undefined ->
        :ok
    end

    write.(:id, sName, sName, s)
  end

  defp check_att_default_val(_, _, _) do
    :ok
  end

  defp valid_Char(:dtd, aT, c, s)
       when aT == :NMTOKEN or
              aT == :NMTOKENS do
    vc_Valid_Char(aT, c, s)
  end

  defp valid_Char(_, _, [c], s) do
    case :xmerl_lib.is_char(c) do
      true ->
        :ok

      false ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [2510, {:unexpected_char, c}]
            )

            :ok
        end

        fatal({:unexpected_char, c}, s)
    end
  end

  defp valid_Char(_, _, c, s) do
    case :xmerl_lib.is_char(c) do
      true ->
        :ok

      false ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [2517, {:unexpected_char, c}]
            )

            :ok
        end

        fatal({:unexpected_char, c}, s)
    end
  end

  defp scan_content(t, s, name, attrs, space, lang, parents, nS) do
    scan_content(
      t,
      s,
      _Pos = 1,
      name,
      attrs,
      space,
      lang,
      parents,
      nS,
      _Acc = [],
      _MarkupDel = []
    )
  end

  defp scan_content(
         '<',
         s = r_xmerl_scanner(continuation_fun: f),
         pos,
         name,
         attrs,
         space,
         lang,
         parents,
         nS,
         acc,
         _
       ) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_content('<' ++ moreBytes, s1, pos, name, attrs, space, lang, parents, nS, acc, [])
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_content(
         [],
         s = r_xmerl_scanner(environment: {:external, {:entity, _}}),
         _Pos,
         _Name,
         _Attrs,
         _Space,
         _Lang,
         _Parents,
         _NS,
         acc,
         _
       ) do
    {:lists.reverse(acc), [], s}
  end

  defp scan_content(
         [],
         s = r_xmerl_scanner(environment: :internal_parsed_entity),
         _Pos,
         _Name,
         _Attrs,
         _Space,
         _Lang,
         _Parents,
         _NS,
         acc,
         _
       ) do
    {:lists.reverse(acc), [], s}
  end

  defp scan_content(
         [],
         s = r_xmerl_scanner(continuation_fun: f),
         pos,
         name,
         attrs,
         space,
         lang,
         parents,
         nS,
         acc,
         _
       ) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_content(moreBytes, s1, pos, name, attrs, space, lang, parents, nS, acc, [])
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_content('</' ++ t, s0, _Pos, name, _Attrs, _Space, _Lang, _Parents, _NS, acc, []) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 2)
    {eTagName, _NamespaceInfo, t1, s1} = scan_name(t, s)

    cond do
      eTagName == name ->
        :ok

      true ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [2557, {:endtag_does_not_match, {:was, eTagName, :should_have_been, name}}]
            )

            :ok
        end

        fatal(
          {:endtag_does_not_match, {:was, eTagName, :should_have_been, name}},
          s
        )
    end

    {_, t2, s2} = strip(t1, s1)

    case t2 do
      '>' ++ t3 ->
        {:lists.reverse(acc), t3, s2}

      _ ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [2564, {:error, {:unexpected_end_of_STag}}]
            )

            :ok
        end

        fatal({:error, {:unexpected_end_of_STag}}, s)
    end
  end

  defp scan_content(
         [?& | _T] = str,
         r_xmerl_scanner(environment: {:external, {:entity, eName}}) = s0,
         pos,
         name,
         attrs,
         space,
         lang,
         parents,
         nS,
         acc,
         _
       ) do
    {_EntV, t1, s1} = scan_entity_value(str, s0, [], eName, :general)
    scan_content(t1, s1, pos, name, attrs, space, lang, parents, nS, acc, [])
  end

  defp scan_content(
         '&' ++ t,
         r_xmerl_scanner(environment: :internal_parsed_entity) = s,
         pos,
         name,
         attrs,
         space,
         lang,
         parents,
         nS,
         acc,
         _
       ) do
    {_, t1, s1} = scan_reference(t, s)
    scan_content(t1, s1, pos, name, attrs, space, lang, parents, nS, acc, [])
  end

  defp scan_content('&' ++ t, s0, pos, name, attrs, space, lang, parents, nS, acc, []) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {expRef, t1, s1} = scan_reference(t, s)

    case markup_delimeter(expRef) do
      true ->
        scan_content(expRef ++ t1, s1, pos, name, attrs, space, lang, parents, nS, acc, expRef)

      _ ->
        scan_content(
          string_to_char_set(
            r_xmerl_scanner(s1, :encoding),
            expRef
          ) ++ t1,
          s1,
          pos,
          name,
          attrs,
          space,
          lang,
          parents,
          nS,
          acc,
          []
        )
    end
  end

  defp scan_content(
         '<!--' ++ t,
         s0 = r_xmerl_scanner(acc_fun: f, comments: cF),
         pos,
         name,
         attrs,
         space,
         lang,
         parents,
         nS,
         acc,
         []
       ) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 4)
    {c, t1, s1} = scan_comment(t, s, pos, parents, lang)

    case cF do
      true ->
        {acc2, pos2, s3} =
          case f.(c, acc, s1) do
            {acc1, s2} ->
              {acc1, pos + 1, s2}

            {acc1, pos1, s2} ->
              {acc1, pos1, s2}
          end

        scan_content(t1, s3, pos2, name, attrs, space, lang, parents, nS, acc2, [])

      false ->
        scan_content(t1, s1, pos, name, attrs, space, lang, parents, nS, acc, [])
    end
  end

  defp scan_content('<' ++ t, s0, pos, name, attrs, space, lang, parents, nS, acc, []) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {markup, t1, s1} = scan_content_markup(t, s, pos, name, attrs, space, lang, parents, nS)
    accF = r_xmerl_scanner(s1, :acc_fun)

    {newAcc, newPos, newS} =
      case accF.(markup, acc, s1) do
        {acc2, s2} ->
          {acc2, pos + 1, s2}

        {acc2, pos2, s2} ->
          {acc2, pos2, s2}
      end

    scan_content(t1, newS, newPos, name, attrs, space, lang, parents, nS, newAcc, [])
  end

  defp scan_content(
         [_H | t],
         s = r_xmerl_scanner(environment: {:external, {:entity, _}}),
         pos,
         name,
         attrs,
         space,
         lang,
         parents,
         nS,
         acc,
         _
       ) do
    scan_content(t, s, pos, name, attrs, space, lang, parents, nS, acc, [])
  end

  defp scan_content(
         t,
         s = r_xmerl_scanner(acc_fun: f, event_fun: event, hook_fun: hook, line: _L),
         pos,
         name,
         attrs,
         space,
         lang,
         parents,
         nS,
         acc,
         markupDel
       ) do
    text0 = r_xmlText(pos: pos, parents: parents)

    s1 =
      r_xmerl_scanner() =
      event.(
        r_xmerl_event(event: :started, line: r_xmerl_scanner(s, :line), data: text0),
        s
      )

    {data, t1, s2} = scan_char_data(t, s1, space, markupDel)
    text = r_xmlText(text0, value: data)
    {ret, s2b} = hook.(text, s2)

    s3 =
      r_xmerl_scanner() =
      event.(
        r_xmerl_event(event: :ended, line: r_xmerl_scanner(s2b, :line), data: ret),
        s2b
      )

    {newAcc, newPos, newS} =
      case f.(ret, acc, s3) do
        {acc4, s4} ->
          {acc4, pos + 1, s4}

        {acc4, pos4, s4} ->
          {acc4, pos4, s4}
      end

    scan_content(t1, newS, newPos, name, attrs, space, lang, parents, nS, newAcc, [])
  end

  defp scan_content_markup(
         [],
         s = r_xmerl_scanner(continuation_fun: f),
         pos,
         name,
         attrs,
         space,
         lang,
         parents,
         nS
       ) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_content_markup(moreBytes, s1, pos, name, attrs, space, lang, parents, nS)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_content_markup('![CDATA[' ++ t, s0, pos, _Name, _Attrs, _Space, _Lang, parents, _NS) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 8)
    scan_cdata(t, s, pos, parents)
  end

  defp scan_content_markup('?' ++ t, s0, pos, _Name, _Attrs, _Space, _Lang, parents, _NS) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_pi(t, s, pos, parents)
  end

  defp scan_content_markup(t, s, pos, _Name, _Attrs, space, lang, parents, nS) do
    scan_element(t, s, pos, space, lang, parents, nS)
  end

  defp scan_char_data(t, s, space, mUD) do
    scan_char_data(t, s, space, mUD, _Acc = [])
  end

  defp scan_char_data(
         [],
         s = r_xmerl_scanner(environment: {:external, {:entity, _}}),
         _Space,
         _MUD,
         acc
       ) do
    {:lists.reverse(acc), [], s}
  end

  defp scan_char_data(
         [],
         s = r_xmerl_scanner(environment: :internal_parsed_entity),
         _Space,
         _MUD,
         acc
       ) do
    {:lists.reverse(acc), [], s}
  end

  defp scan_char_data([], s = r_xmerl_scanner(continuation_fun: f), space, _MUD, acc) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_char_data(moreBytes, s1, space, _MUD, acc)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_char_data([?& | t], s, space, '&', acc) do
    scan_char_data(t, s, space, [], [?& | acc])
  end

  defp scan_char_data(t = [?& | _], s, _Space, _MUD, acc) do
    {:lists.reverse(acc), t, s}
  end

  defp scan_char_data(']]>' ++ _T, s, _Space, _MUD, _Acc) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [2693, :unexpected_cdata_end]
        )

        :ok
    end

    fatal(:unexpected_cdata_end, s)
  end

  defp scan_char_data([?< | t], s, space, '<', acc) do
    scan_char_data(t, s, space, [], [?< | acc])
  end

  defp scan_char_data(t = [?< | _], s, _Space, _MUD, acc) do
    {:lists.reverse(acc), t, s}
  end

  defp scan_char_data(t = [h | r], s, space, mUD, acc)
       when h == 32 or
              h == 13 or h == 10 or
              h == 9 do
    cond do
      mUD === [] and acc === [] and h === ?\n and
          space === :preserve ->
        case fast_accumulate_whitespace(r, s, t) do
          {:done, reply} ->
            reply

          {newAcc, t1, s1} ->
            scan_char_data(t1, s1, space, mUD, newAcc)
        end

      true ->
        {newAcc, t1, s1} = accumulate_whitespace(t, s, space, acc)
        scan_char_data(t1, s1, space, mUD, newAcc)
    end
  end

  defp scan_char_data([[h1, h2] | _T], s, _Space, _MUD, _Acc)
       when (h1 == 255 and h2 == 254) or
              (h1 == 255 and h2 == 255) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [2713, {:error, {:not_allowed_to_use_Unicode_noncharacters}}]
        )

        :ok
    end

    fatal(
      {:error, {:not_allowed_to_use_Unicode_noncharacters}},
      s
    )
  end

  defp scan_char_data(']]>' ++ _T, s, _Space, _MUD, _Acc) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [2715, {:error, {:illegal_character_in_content, ']]>'}}]
        )

        :ok
    end

    fatal({:error, {:illegal_character_in_content, ']]>'}}, s)
  end

  defp scan_char_data(str, s0, space, mUD, acc) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {ch, t} = wfc_legal_char(str, s)
    scan_char_data(t, s, space, mUD, [ch | acc])
  end

  defp scan_cdata(str, s, pos, parents) do
    scan_cdata(str, s, pos, parents, _Acc = [])
  end

  defp scan_cdata([], s = r_xmerl_scanner(continuation_fun: f), pos, parents, acc) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_cdata(moreBytes, s1, pos, parents, acc)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_cdata(']]>' ++ t, s0, pos, parents, acc) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 3)
    {r_xmlText(pos: pos, parents: parents, value: :lists.reverse(acc), type: :cdata), t, s}
  end

  defp scan_cdata(str, s0, pos, parents, acc) do
    {ch, t} = to_ucs(r_xmerl_scanner(s0, :encoding), str)

    case :xmerl_lib.is_char(ch) do
      true ->
        :no_debug
        s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
        scan_cdata(t, s, pos, parents, [ch | acc])

      false ->
        cond do
          r_xmerl_scanner(s0, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [2747, {:unexpected_char, ch}]
            )

            :ok
        end

        fatal({:unexpected_char, ch}, s0)
    end
  end

  defp scan_reference([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_reference(moreBytes, s1)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_reference('#x' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)

    cond do
      hd(t) != ?; ->
        scan_char_ref_hex(t, s, 0)

      true ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg('~p- fatal: ~p~n', [2765, :invalid_char_ref])
            :ok
        end

        fatal(:invalid_char_ref, s)
    end
  end

  defp scan_reference('#' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)

    cond do
      hd(t) != ?; ->
        scan_char_ref_dec(t, s, [])

      true ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg('~p- fatal: ~p~n', [2773, :invalid_char_ref])
            :ok
        end

        fatal(:invalid_char_ref, s)
    end
  end

  defp scan_reference(t, s) do
    case (try do
            scan_entity_ref(t, s)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [2778, :error_scanning_entity_ref]
            )

            :ok
        end

        fatal(:error_scanning_entity_ref, s)

      other ->
        other
    end
  end

  defp scan_entity_ref([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_entity_ref(moreBytes, s1)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_entity_ref('amp;' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 4)
    {'&', t, s}
  end

  defp scan_entity_ref('lt;' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 3)
    {'<', t, s}
  end

  defp scan_entity_ref('gt;' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 3)
    {'>', t, s}
  end

  defp scan_entity_ref('apos;' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 5)
    {'\'', t, s}
  end

  defp scan_entity_ref('quot;' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 5)
    {'"', t, s}
  end

  defp scan_entity_ref(t, s) do
    {name, _NamespaceInfo, t1, s1} = scan_name(t, s)
    t2 = scan_mandatory(';', t1, 1, s1, :expected_entity_reference_semicolon)
    s2 = s1
    entity = expand_reference(name, s2)
    {entity, t2, s2}
  end

  defp scan_pe_reference(t, s) do
    {name, _NamespaceInfo, t1, s1} = scan_name(t, s)
    t2 = scan_mandatory(';', t1, 1, s1, :expected_parsed_entity_reference_semicolon)
    {name, t2, r_xmerl_scanner(s1, col: r_xmerl_scanner(s1, :col) + 1)}
  end

  defp expand_pe_reference(name, r_xmerl_scanner(rules_read_fun: read) = s, wS) do
    case read.(:parameter_entity, name, s) do
      :undefined ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [2836, {:unknown_parameter_entity, name}]
            )

            :ok
        end

        fatal({:unknown_parameter_entity, name}, s)

      err = {:error, _Reason} ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg('~p- fatal: ~p~n', [2838, err])
            :ok
        end

        fatal(err, s)

      tuple when is_tuple(tuple) ->
        tuple

      result ->
        cond do
          wS == :in_literal ->
            result

          true ->
            ' ' ++ result ++ ' '
        end
    end
  end

  defp expand_reference(
         name,
         r_xmerl_scanner(environment: {:external, {:entity, _}})
       ) do
    :erlang.atom_to_list(name)
  end

  defp expand_reference(
         name,
         r_xmerl_scanner(environment: :internal_parsed_entity)
       ) do
    :erlang.atom_to_list(name)
  end

  defp expand_reference(name, r_xmerl_scanner(rules_read_fun: read) = s) do
    case read.(:entity, name, s) do
      :undefined ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [2868, {:unknown_entity_ref, name}]
            )

            :ok
        end

        fatal({:unknown_entity_ref, name}, s)

      {_, :external, {:error, :enoent}} ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [2870, {:error, {:entity_target_not_found, {:error, :enoent}, name}}]
            )

            :ok
        end

        fatal(
          {:error, {:entity_target_not_found, {:error, :enoent}, name}},
          s
        )

      {defEnv, entType, value} ->
        wfc_Entity_Declared(defEnv, s, name)
        value2 = string_to_char_set(r_xmerl_scanner(s, :encoding), value)
        wfc_Internal_parsed_entity(entType, value2, s)
        value
    end
  end

  defp scan_char_ref_dec([], s = r_xmerl_scanner(continuation_fun: f), acc) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_char_ref_dec(moreBytes, s1, acc)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_char_ref_dec([h | t], s0, acc) when h >= ?0 and h <= ?9 do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_char_ref_dec(t, s, [h | acc])
  end

  defp scan_char_ref_dec(';' ++ t, s0, acc) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    ref = :erlang.list_to_integer(:lists.reverse(acc))
    {ch, _} = wfc_legal_char(ref, s)
    {[ch], t, s}
  end

  defp scan_char_ref_hex([], s = r_xmerl_scanner(continuation_fun: f), acc) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_char_ref_hex(moreBytes, s1, acc)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_char_ref_hex([h | t], s0, acc) when h >= ?0 and h <= ?9 do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    dec = h - ?0
    scan_char_ref_hex(t, s, dec ||| acc <<< 4)
  end

  defp scan_char_ref_hex([h | t], s0, acc) when h >= ?a and h <= ?f do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    dec = h - ?a + 10
    scan_char_ref_hex(t, s, dec ||| acc <<< 4)
  end

  defp scan_char_ref_hex([h | t], s0, acc) when h >= ?A and h <= ?F do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    dec = h - ?A + 10
    scan_char_ref_hex(t, s, dec ||| acc <<< 4)
  end

  defp scan_char_ref_hex(';' ++ t, s0, acc) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {ch, _} = wfc_legal_char(acc, s)
    {[ch], t, s}
  end

  defp scan_eq(t, s) do
    {_, t1, s1} = strip(t, s)

    case t1 do
      [?= | t2] ->
        s2 = r_xmerl_scanner(s1, col: r_xmerl_scanner(s1, :col) + 1)
        {_, t3, s3} = strip(t2, s2)
        {t3, s3}

      _ ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg('~p- fatal: ~p~n', [2930, :assignment_expected])
            :ok
        end

        fatal(:assignment_expected, s)
    end
  end

  defp scan_name_no_colons(str, s) do
    nSC = r_xmerl_scanner(s, :namespace_conformant)

    case nSC do
      true ->
        {target, nSI, t1, s1} =
          scan_name(
            str,
            r_xmerl_scanner(s, namespace_conformant: :no_colons)
          )

        {target, nSI, t1, r_xmerl_scanner(s1, namespace_conformant: nSC)}

      false ->
        scan_name(str, s)
    end
  end

  defp scan_name([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_name(moreBytes, s1)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_name(
         str = [?: | t],
         s0 = r_xmerl_scanner(namespace_conformant: nSC)
       ) do
    cond do
      nSC == false ->
        :no_debug
        s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
        scan_nmtoken(t, s, [?:], nSC)

      nSC == :no_colons ->
        cond do
          r_xmerl_scanner(s0, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [2980, {:invalid_NCName, :lists.sublist(str, 1, 6)}]
            )

            :ok
        end

        fatal({:invalid_NCName, :lists.sublist(str, 1, 6)}, s0)

      true ->
        cond do
          r_xmerl_scanner(s0, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [2984, {:invalid_NCName, :lists.sublist(str, 1, 6)}]
            )

            :ok
        end

        fatal({:invalid_NCName, :lists.sublist(str, 1, 6)}, s0)
    end
  end

  defp scan_name([?_ | t], s0 = r_xmerl_scanner(namespace_conformant: nSC)) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_nmtoken(t, s, [?_], nSC)
  end

  defp scan_name('%' ++ _T, s = r_xmerl_scanner(environment: :prolog)) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [2990, {:error, {:wfc_PEs_In_Internal_Subset}}]
        )

        :ok
    end

    fatal({:error, {:wfc_PEs_In_Internal_Subset}}, s)
  end

  defp scan_name('%' ++ t, s0 = r_xmerl_scanner(environment: {:external, _})) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {pERefName, t1, s1} = scan_pe_reference(t, s)
    expRef = expand_pe_reference(pERefName, s1, :as_PE)
    {_, t2, s2} = strip(expRef ++ t1, s1)
    scan_name(t2, s2)
  end

  defp scan_name(str, s0 = r_xmerl_scanner(namespace_conformant: nSC)) do
    {ch, t} = to_ucs(r_xmerl_scanner(s0, :encoding), str)

    case :xmerl_lib.is_letter(ch) do
      true ->
        :no_debug
        s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
        scan_nmtoken(t, s, [ch], nSC)

      false ->
        cond do
          r_xmerl_scanner(s0, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [3005, {:invalid_name, :lists.sublist(str, 1, 6)}]
            )

            :ok
        end

        fatal({:invalid_name, :lists.sublist(str, 1, 6)}, s0)
    end
  end

  defp scan_name(str, s) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg('~p- fatal: ~p~n', [3008, {:invalid_name, str}])
        :ok
    end

    fatal({:invalid_name, str}, s)
  end

  defp scan_nmtoken(str, s, acc, nSC) do
    scan_nmtoken(str, s, acc, _Prefix = [], _Local = acc, nSC, isLatin1(hd(acc), true))
  end

  defp scan_nmtoken([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_nmtoken(moreBytes, s1)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_nmtoken('%' ++ t, s0 = r_xmerl_scanner(environment: {:external, _})) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {pERefName, t1, s1} = scan_pe_reference(t, s)
    expRef = expand_pe_reference(pERefName, s1, :as_PE)
    {_, t2, s2} = strip(expRef ++ t1, s1)
    scan_nmtoken(t2, s2)
  end

  defp scan_nmtoken(str, s) do
    {ch, t} = to_ucs(r_xmerl_scanner(s, :encoding), str)

    case :xmerl_lib.is_namechar(ch) do
      true ->
        scan_nmtoken(
          t,
          r_xmerl_scanner(s, col: r_xmerl_scanner(s, :col) + 1),
          _Acc = [ch],
          _Prefix = [],
          _Local = [ch],
          _NamespaceConformant = false,
          isLatin1(ch, true)
        )

      false ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [3039, {:invalid_nmtoken, :lists.sublist(str, 1, 6)}]
            )

            :ok
        end

        fatal({:invalid_nmtoken, :lists.sublist(str, 1, 6)}, s)
    end
  end

  defp scan_nmtoken(
         [],
         s = r_xmerl_scanner(continuation_fun: f),
         acc,
         prefix,
         local,
         nSC,
         isLatin1
       ) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_nmtoken(moreBytes, s1, acc, prefix, local, nSC, isLatin1)
      end,
      fn s1 ->
        {:erlang.list_to_atom(:lists.reverse(acc)), namespace_info(prefix, local), [], s1}
      end,
      s
    )
  end

  defp scan_nmtoken(str = [h | _], s, acc, prefix, local, _NSC, true)
       when h == 32 or h == 13 or h == 10 or h == 9 do
    nmString = :lists.reverse(acc)
    {:erlang.list_to_atom(nmString), namespace_info(prefix, local), str, s}
  end

  defp scan_nmtoken(str = [?: | _], s, acc, [], _Local, :no_colons, _IsLatin1) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3058, {:invalid_NCName, :lists.sublist(:lists.reverse(acc) ++ str, 1, 6)}]
        )

        :ok
    end

    fatal(
      {:invalid_NCName, :lists.sublist(:lists.reverse(acc) ++ str, 1, 6)},
      s
    )
  end

  defp scan_nmtoken([?: | t], s0, acc, [], local, nSC, isLatin1) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_nmtoken(t, s, [?: | acc], :lists.reverse(local), [], nSC, isLatin1)
  end

  defp scan_nmtoken(str = [?: | _T], s, acc, _Prefix, _Local, _NSC = true, _IsLatin1) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3067, {:invalid_NCName, :lists.sublist(:lists.reverse(acc) ++ str, 1, 6)}]
        )

        :ok
    end

    fatal(
      {:invalid_NCName, :lists.sublist(:lists.reverse(acc) ++ str, 1, 6)},
      s
    )
  end

  defp scan_nmtoken(str, s0, acc, prefix, local, nSC, isLatin1) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {ch, t} = to_ucs(r_xmerl_scanner(s, :encoding), str)

    case {:xmerl_lib.is_namechar(ch), isLatin1} do
      {true, _} ->
        scan_nmtoken(t, s, [ch | acc], prefix, [ch | local], nSC, isLatin1(ch, isLatin1))

      {_, true} ->
        nmStr = :lists.reverse(acc)
        {:erlang.list_to_atom(nmStr), namespace_info(prefix, local), str, s}

      _ ->
        {:lists.reverse(acc), namespace_info(prefix, local), str, s}
    end
  end

  defp namespace_info([], _) do
    []
  end

  defp namespace_info(prefix, local) do
    {prefix, :lists.reverse(local)}
  end

  defp isLatin1(_Ch, false) do
    false
  end

  defp isLatin1(ch, _) when ch > 255 do
    false
  end

  defp isLatin1(_, _) do
    true
  end

  defp scan_system_literal([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_system_literal(moreBytes, s1)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_system_literal('"' ++ t, s) do
    scan_system_literal(t, s, ?", [])
  end

  defp scan_system_literal('\'' ++ t, s) do
    scan_system_literal(t, s, ?', [])
  end

  defp scan_system_literal([], s = r_xmerl_scanner(continuation_fun: f), delimiter, acc) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_system_literal(moreBytes, s1, delimiter, acc)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_system_literal([h | t], s, h, acc) do
    {:lists.reverse(acc), t, r_xmerl_scanner(s, col: r_xmerl_scanner(s, :col) + 1)}
  end

  defp scan_system_literal('#' ++ _R, s, _H, _Acc) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3118, :fragment_identifier_in_system_literal]
        )

        :ok
    end

    fatal(:fragment_identifier_in_system_literal, s)
  end

  defp scan_system_literal(str, s, delimiter, acc) do
    {ch, t} = to_ucs(r_xmerl_scanner(s, :encoding), str)

    scan_system_literal(t, r_xmerl_scanner(s, col: r_xmerl_scanner(s, :col) + 1), delimiter, [
      ch | acc
    ])
  end

  defp scan_pubid_literal([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_pubid_literal(moreBytes, s1)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_pubid_literal([h | t], s) when h == ?" or h == ?' do
    scan_pubid_literal(t, r_xmerl_scanner(s, col: r_xmerl_scanner(s, :col) + 1), h, [])
  end

  defp scan_pubid_literal([h | _T], s) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3135, {:invalid_pubid_char, h}]
        )

        :ok
    end

    fatal({:invalid_pubid_char, h}, s)
  end

  defp scan_pubid_literal([], s = r_xmerl_scanner(continuation_fun: f), delimiter, acc) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_pubid_literal(moreBytes, s1, delimiter, acc)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_pubid_literal([h | t], s, h, acc) do
    {:lists.reverse(acc), t, r_xmerl_scanner(s, col: r_xmerl_scanner(s, :col) + 1)}
  end

  defp scan_pubid_literal(str = [h | _], s, delimiter, acc)
       when h == 32 or h == 13 or h == 10 or h == 9 do
    {_, t, s1} = pub_id_strip(str, s)
    scan_pubid_literal(t, s1, delimiter, [32 | acc])
  end

  defp scan_pubid_literal([h | t], s, delimiter, acc) do
    case is_pubid_char(h) do
      true ->
        scan_pubid_literal(t, r_xmerl_scanner(s, col: r_xmerl_scanner(s, :col) + 1), delimiter, [
          h | acc
        ])

      false ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [3158, {:invalid_pubid_char, h}]
            )

            :ok
        end

        fatal({:invalid_pubid_char, h}, s)
    end
  end

  defp is_pubid_char(x) when x >= ?a and x <= ?z do
    true
  end

  defp is_pubid_char(x) when x >= ?A and x <= ?Z do
    true
  end

  defp is_pubid_char(x) when x >= ?0 and x <= ?9 do
    true
  end

  defp is_pubid_char(x) do
    :lists.member(x, '-\'()+,./:=?;!*#@$_%')
  end

  defp scan_contentspec([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_contentspec(moreBytes, s1)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_contentspec('EMPTY' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 5)
    {:empty, t, s}
  end

  defp scan_contentspec('ANY' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 3)
    {:any, t, s}
  end

  defp scan_contentspec('%' ++ _T, s = r_xmerl_scanner(environment: :prolog)) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3186, {:error, {:wfc_PEs_In_Internal_Subset}}]
        )

        :ok
    end

    fatal({:error, {:wfc_PEs_In_Internal_Subset}}, s)
  end

  defp scan_contentspec('%' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {pERefName, t1, s1} = scan_pe_reference(t, s)
    expRef = expand_pe_reference(pERefName, s1, :as_PE)
    {_, t2, s2} = strip(expRef ++ t1, s1)
    scan_contentspec(t2, s2)
  end

  defp scan_contentspec('(' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {_, t1, s1} = strip(t, s)
    scan_elem_content(t1, s1)
  end

  defp scan_contentspec(_Str, s) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3198, :unexpected_character]
        )

        :ok
    end

    fatal(:unexpected_character, s)
  end

  defp scan_elem_content(t, s) do
    scan_elem_content(t, s, _Context = :children, _Mode = :unknown, _Acc = [])
  end

  defp scan_elem_content([], s = r_xmerl_scanner(continuation_fun: f), context, mode, acc) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_elem_content(moreBytes, s1, context, mode, acc)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_elem_content(')' ++ t, s0, context, mode0, acc0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)

    {mode, acc} =
      case {mode0, acc0} do
        {:unknown, [_X]} ->
          {:seq, acc0}

        {m, _L} when m == :seq or m == :choice ->
          {mode0, :lists.reverse(acc0)}
      end

    {occurrence, t1, s1} = scan_occurrence(t, s)
    vc_No_Duplicate_Types(s, context, acc)

    case {occurrence, context, acc} do
      {:once, :mixed, [:"#PCDATA"]} ->
        :ok

      {:*, :mixed, _} ->
        :ok

      {other, :mixed, _} ->
        cond do
          r_xmerl_scanner(s1, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [3229, {:illegal_for_mixed_content, other}]
            )

            :ok
        end

        fatal({:illegal_for_mixed_content, other}, s1)

      _ ->
        :ok
    end

    {_, t2, s2} = strip(t1, s1)
    {format_elem_content({occurrence, {mode, acc}}), t2, s2}
  end

  defp scan_elem_content('#PCDATA' ++ _T, s, :not_mixed, _Mode, _Acc) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3236, {:error, {:extra_set_of_parenthesis}}]
        )

        :ok
    end

    fatal({:error, {:extra_set_of_parenthesis}}, s)
  end

  defp scan_elem_content('#PCDATA' ++ _T, s, _Cont, mode, acc)
       when mode == :choice or mode == :seq or acc != [] do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3239, {:error, {:invalid_format_of_mixed_content}}]
        )

        :ok
    end

    fatal({:error, {:invalid_format_of_mixed_content}}, s)
  end

  defp scan_elem_content('#PCDATA' ++ t, s0, _Context, mode, acc) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 7)
    {_, t1, s1} = strip(t, s)
    scan_elem_content(t1, s1, :mixed, mode, [:"#PCDATA" | acc])
  end

  defp scan_elem_content(',' ++ _T, s, _Context, :choice, _Acc) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3245, {:mixing_comma_and_vertical_bar_in_content_model}]
        )

        :ok
    end

    fatal(
      {:mixing_comma_and_vertical_bar_in_content_model},
      s
    )
  end

  defp scan_elem_content(',' ++ t, s0, context, _Mode, acc) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {_, t1, s1} = strip(t, s)
    scan_elem_content2(t1, s1, context, :seq, acc)
  end

  defp scan_elem_content('|' ++ _T, s, _Context, :seq, _Acc) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3251, {:mixing_comma_and_vertical_bar_in_content_model}]
        )

        :ok
    end

    fatal(
      {:mixing_comma_and_vertical_bar_in_content_model},
      s
    )
  end

  defp scan_elem_content('|' ++ t, s0, context, _Mode, acc) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {_, t1, s1} = strip(t, s)
    scan_elem_content2(t1, s1, context, :choice, acc)
  end

  defp scan_elem_content(t, s, context, mode, acc) do
    scan_elem_content2(t, s, context, mode, acc)
  end

  defp scan_elem_content2('(' ++ _T, s, :mixed, _Mode, _Acc) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3261, {:error, {:element_names_must_not_be_parenthesized_in_mixed_content}}]
        )

        :ok
    end

    fatal(
      {:error, {:element_names_must_not_be_parenthesized_in_mixed_content}},
      s
    )
  end

  defp scan_elem_content2('(' ++ t, s0, context, mode, acc) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {_, t1, s1} = strip(t, s)
    {inner, t2, s2} = scan_elem_content(t1, s1, :not_mixed, :unknown, [])
    scan_elem_content(t2, s2, context, mode, [inner | acc])
  end

  defp scan_elem_content2(
         '%' ++ _T,
         s = r_xmerl_scanner(environment: :prolog),
         _Context,
         _Mode,
         _Acc
       ) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3268, {:error, {:wfc_PEs_In_Internal_Subset}}]
        )

        :ok
    end

    fatal({:error, {:wfc_PEs_In_Internal_Subset}}, s)
  end

  defp scan_elem_content2('%' ++ t, s0, context, mode, acc) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {pERefName, t1, s1} = scan_pe_reference(t, s)
    expRef = expand_pe_reference(pERefName, s1, :as_PE)
    {_, t2, s2} = strip(expRef ++ t1, s1)
    scan_elem_content(t2, s2, context, mode, acc)
  end

  defp scan_elem_content2(t, s, context, mode, acc) do
    {name, _NameStr, t1, s1} = scan_name(t, s)
    {occurrence, t2, s2} = scan_occurrence(t1, s1)

    case {occurrence, context} do
      {:once, :mixed} ->
        :ok

      {other, :mixed} ->
        cond do
          r_xmerl_scanner(s1, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [3281, {:illegal_for_mixed_content, other}]
            )

            :ok
        end

        fatal({:illegal_for_mixed_content, other}, s1)

      _ ->
        :ok
    end

    {_, t3, s3} = strip(t2, s2)
    mandatory_delimeter_wfc(t3, s3)
    newAcc = [format_elem_content({occurrence, name}) | acc]
    scan_elem_content(t3, s3, context, mode, newAcc)
  end

  defp format_elem_content({:once, what}) do
    what
  end

  defp format_elem_content(other) do
    other
  end

  defp scan_occurrence([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_occurrence(moreBytes, s1)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_occurrence([?? | t], s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {:"?", t, s}
  end

  defp scan_occurrence([?+ | t], s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {:+, t, s}
  end

  defp scan_occurrence([?* | t], s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {:*, t, s}
  end

  defp scan_occurrence(t, s) do
    {:once, t, s}
  end

  defp vc_Valid_Char(_AT, c, s) do
    case :xmerl_lib.is_namechar(c) do
      true ->
        :ok

      _ ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [3321, {:error, {:validity_constraint_Name_Token, c}}]
            )

            :ok
        end

        fatal({:error, {:validity_constraint_Name_Token, c}}, s)
    end
  end

  defp vc_ID_Attribute_Default(_, r_xmerl_scanner(validation: valid)) when valid != :dtd do
    :ok
  end

  defp vc_ID_Attribute_Default({_, :ID, _, def__, _}, _S)
       when def__ == :"#IMPLIED" or
              def__ == :"#REQUIRED" do
    :ok
  end

  defp vc_ID_Attribute_Default({_, :ID, _, def__, _}, s) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3333, {:error, {:validity_constraint_error_ID_Attribute_Default, def__}}]
        )

        :ok
    end

    fatal(
      {:error, {:validity_constraint_error_ID_Attribute_Default, def__}},
      s
    )
  end

  defp vc_Enumeration({_Name, {_, nameList}, defaultVal, _, _}, s)
       when is_list(defaultVal) do
    case :lists.member(
           :erlang.list_to_atom(defaultVal),
           nameList
         ) do
      true ->
        :ok

      _ ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [3341, {:error, {:vc_enumeration, :erlang.list_to_atom(defaultVal), nameList}}]
            )

            :ok
        end

        fatal(
          {:error, {:vc_enumeration, :erlang.list_to_atom(defaultVal), nameList}},
          s
        )
    end
  end

  defp vc_Enumeration(
         {_Name, {_, _NameList}, _DefaultVal, _, _},
         _S
       ) do
    :ok
  end

  defp vc_Entity_Name({_Name, :ENTITY, defaultVal, _, _}, s)
       when is_list(defaultVal) do
    read = r_xmerl_scanner(s, :rules_read_fun)

    case read.(:entity, :erlang.list_to_atom(defaultVal), s) do
      {_, :external, {_, {:ndata, _}}} ->
        :ok

      _ ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [3351, {:error, {:vc_Entity_Name, :erlang.list_to_atom(defaultVal)}}]
            )

            :ok
        end

        fatal(
          {:error, {:vc_Entity_Name, :erlang.list_to_atom(defaultVal)}},
          s
        )
    end
  end

  defp vc_Entity_Name({_Name, :ENTITY, _, _, _}, _S) do
    :ok
  end

  defp vc_Entity_Name({_, :ENTITIES, defaultVal, _, _}, s)
       when is_list(defaultVal) do
    read = r_xmerl_scanner(s, :rules_read_fun)

    nameListFun = fn
      [], acc, _St, _Fun ->
        :lists.reverse(acc)

      str, acc, st, fun ->
        {n, _, st2, str2} = scan_name(str, st)
        fun.(str2, [n | acc], st2, fun)
    end

    nameList = nameListFun.(defaultVal, [], s, nameListFun)

    vcFun = fn x ->
      case read.(:entity, x, s) do
        {_, :external, {_, {:ndata, _}}} ->
          :ok

        _ ->
          cond do
            r_xmerl_scanner(s, :quiet) ->
              :ok

            true ->
              :error_logger.error_msg(
                '~p- fatal: ~p~n',
                [3369, {:error, {:vc_Entity_Name, x}}]
              )

              :ok
          end

          fatal({:error, {:vc_Entity_Name, x}}, s)
      end
    end

    :lists.foreach(vcFun, nameList)
  end

  defp vc_Entity_Name({_, :ENTITIES, _, _, _}, _S) do
    :ok
  end

  defp vc_No_Duplicate_Types(r_xmerl_scanner(validation: :dtd) = s, :mixed, acc) do
    checkDupl = fn
      [h | t], f ->
        case :lists.member(h, t) do
          true ->
            cond do
              r_xmerl_scanner(s, :quiet) ->
                :ok

              true ->
                :error_logger.error_msg(
                  '~p- fatal: ~p~n',
                  [3381, {:no_duplicate_types_allowed, h}]
                )

                :ok
            end

            fatal({:no_duplicate_types_allowed, h}, s)

          _ ->
            f.(t, f)
        end

      [], _ ->
        :ok
    end

    checkDupl.(acc, checkDupl)
  end

  defp vc_No_Duplicate_Types(_, _, _) do
    :ok
  end

  defp mandatory_delimeter_wfc(',' ++ _T, _S) do
    :ok
  end

  defp mandatory_delimeter_wfc('|' ++ _T, _S) do
    :ok
  end

  defp mandatory_delimeter_wfc(')' ++ _T, _S) do
    :ok
  end

  defp mandatory_delimeter_wfc('%' ++ _T, _S) do
    :ok
  end

  defp mandatory_delimeter_wfc(t, s) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3404, {:comma_or_vertical_bar_mandatory_between_names_in_content_model, t}]
        )

        :ok
    end

    fatal(
      {:comma_or_vertical_bar_mandatory_between_names_in_content_model, t},
      s
    )
  end

  defp wfc_unique_att_spec([], _S) do
    :ok
  end

  defp wfc_unique_att_spec([r_xmlAttribute(name: n, expanded_name: eN) | atts], s) do
    case :lists.keymember(n, r_xmlAttribute(:name), atts) do
      true ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [3412, {:error, {:unique_att_spec_required, n}}]
            )

            :ok
        end

        fatal({:error, {:unique_att_spec_required, n}}, s)

      _ ->
        case r_xmerl_scanner(s, :namespace_conformant) and
               :lists.keymember(
                 eN,
                 r_xmlAttribute(:expanded_name),
                 atts
               ) do
          true ->
            cond do
              r_xmerl_scanner(s, :quiet) ->
                :ok

              true ->
                :error_logger.error_msg(
                  '~p- fatal: ~p~n',
                  [3417, {:error, {:unique_att_spec_required, eN}}]
                )

                :ok
            end

            fatal({:error, {:unique_att_spec_required, eN}}, s)

          _ ->
            wfc_unique_att_spec(atts, s)
        end
    end
  end

  defp wfc_legal_char(chars, s) when is_list(chars) do
    {ch, rest} = to_ucs(r_xmerl_scanner(s, :encoding), chars)

    case :xmerl_lib.is_char(ch) do
      true ->
        {ch, rest}

      _ ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [3429, {:error, {:wfc_Legal_Character, ch}}]
            )

            :ok
        end

        fatal({:error, {:wfc_Legal_Character, ch}}, s)
    end
  end

  defp wfc_legal_char(ch, s) do
    case :xmerl_lib.is_char(ch) do
      true ->
        {ch, []}

      _ ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [3436, {:error, {:wfc_Legal_Character, ch}}]
            )

            :ok
        end

        fatal({:error, {:wfc_Legal_Character, ch}}, s)
    end
  end

  defp wfc_whitespace_betw_attrs([wS | _] = l, s)
       when wS == 32 or wS == 13 or
              wS == 10 or wS == 9 do
    {l, s}
  end

  defp wfc_whitespace_betw_attrs([?/ | _] = l, s) do
    {l, s}
  end

  defp wfc_whitespace_betw_attrs([?> | _] = l, s) do
    {l, s}
  end

  defp wfc_whitespace_betw_attrs([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        wfc_whitespace_betw_attrs(moreBytes, s1)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp wfc_whitespace_betw_attrs(_, s) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3452, {:whitespace_required_between_attributes}]
        )

        :ok
    end

    fatal({:whitespace_required_between_attributes}, s)
  end

  defp wfc_Entity_Declared({:external, _}, s = r_xmerl_scanner(standalone: :yes), name) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3455, {:reference_to_externally_defed_entity_standalone_doc, name}]
        )

        :ok
    end

    fatal(
      {:reference_to_externally_defed_entity_standalone_doc, name},
      s
    )
  end

  defp wfc_Entity_Declared({:external, _}, _S, _) do
    :ok
  end

  defp wfc_Entity_Declared(_Env, _S, _) do
    :ok
  end

  defp wfc_Internal_parsed_entity(:internal, value, s) do
    scan_content(
      value,
      r_xmerl_scanner(s, environment: :internal_parsed_entity),
      _Name = [],
      [],
      r_xmerl_scanner(s, :space),
      _Lang = [],
      _Prnt = [],
      r_xmlNamespace()
    )
  end

  defp wfc_Internal_parsed_entity(_, _, _) do
    :ok
  end

  defp vc_Element_valid(_Name, {'xmlns', _}, s = r_xmerl_scanner(namespace_conformant: true)) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3471, {:error, {:illegal_element_prefix, :xmlns}}]
        )

        :ok
    end

    fatal({:error, {:illegal_element_prefix, :xmlns}}, s)
  end

  defp vc_Element_valid(name, _, s) do
    vc_Element_valid(name, s)
  end

  defp vc_Element_valid(
         _Name,
         r_xmerl_scanner(environment: :internal_parsed_entity)
       ) do
    :ok
  end

  defp vc_Element_valid(
         name,
         s = r_xmerl_scanner(rules_read_fun: read, validation: :dtd)
       ) do
    case read.(:elem_def, name, s) do
      r_xmlElement(elementdef: :undeclared) ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [3481, {:error, {:error_missing_element_declaration_in_DTD, name}}]
            )

            :ok
        end

        fatal(
          {:error, {:error_missing_element_declaration_in_DTD, name}},
          s
        )

      :undefined ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [3482, {:error, {:error_missing_element_declaration_in_DTD, name}}]
            )

            :ok
        end

        fatal(
          {:error, {:error_missing_element_declaration_in_DTD, name}},
          s
        )

      _ ->
        :ok
    end
  end

  defp vc_Element_valid(_, _) do
    :ok
  end

  defp scan_pe_def([], s = r_xmerl_scanner(continuation_fun: f), pEName) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_pe_def(moreBytes, s1, pEName)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_pe_def('\'' ++ t, s0, pEName) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_entity_value(t, s, ?', pEName, :parameter)
  end

  defp scan_pe_def('"' ++ t, s0, pEName) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_entity_value(t, s, ?", pEName, :parameter)
  end

  defp scan_pe_def(str, s, _PEName) do
    scan_external_id(str, s)
  end

  defp scan_notation_decl(
         t,
         r_xmerl_scanner(rules_write_fun: write, rules_read_fun: read, rules_delete_fun: delete) =
           s
       ) do
    {name, _NameStr, t1, s1} = scan_name_no_colons(t, s)
    {_, t2, s2} = mandatory_strip(t1, s1)
    {def__, t3, s3} = scan_notation_decl1(t2, s2)
    {_, t4, s4} = strip(t3, s3)
    t5 = scan_mandatory('>', t4, 1, s4, :expected_end_tag_notation_declaration)

    case read.(:notation, name, s) do
      :undeclared ->
        delete.(:notation, name, s4)

      _ ->
        :ok
    end

    s5 = write.(:notation, name, def__, s4)
    {t5, s5}
  end

  defp scan_notation_decl1([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_notation_decl1(moreBytes, s1)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_notation_decl1('SYSTEM' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 6)
    {_, t1, s1} = mandatory_strip(t, s)
    {sL, t2, s2} = scan_system_literal(t1, s1)
    {{:system, sL}, t2, s2}
  end

  defp scan_notation_decl1('PUBLIC' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 6)
    {_, t1, s1} = mandatory_strip(t, s)
    {pIDL, t2, s2} = scan_pubid_literal(t1, s1)
    {_, t3, s3} = strip(t2, s2)

    case t3 do
      '>' ++ _ ->
        {{:public, pIDL}, t3, r_xmerl_scanner(s3, col: r_xmerl_scanner(s3, :col) + 1)}

      _ ->
        {sL, t4, s4} = scan_system_literal(t3, s3)
        {{:public, pIDL, sL}, t4, s4}
    end
  end

  defp scan_external_id([], s = r_xmerl_scanner(continuation_fun: f)) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_external_id(moreBytes, s1)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_external_id('SYSTEM' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 6)
    {_, t1, s1} = mandatory_strip(t, s)
    {sL, t2, s2} = scan_system_literal(t1, s1)
    {{:system, sL}, t2, s2}
  end

  defp scan_external_id('PUBLIC' ++ t, s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 6)
    {_, t1, s1} = mandatory_strip(t, s)
    {pIDL, t2, s2} = scan_pubid_literal(t1, s1)
    {_, t3, s3} = mandatory_strip(t2, s2)
    {sL, t4, s4} = scan_system_literal(t3, s3)
    {{:public, pIDL, sL}, t4, s4}
  end

  defp scan_entity_value(str, s, delim, name, namespace) do
    scan_entity_value(str, s, delim, _Acc = [], name, namespace, [])
  end

  defp scan_entity_value(
         [],
         s = r_xmerl_scanner(environment: {:external, {:entity, _}}),
         _Delim,
         acc,
         _,
         _,
         []
       ) do
    {:lists.flatten(:lists.reverse(acc)), [], s}
  end

  defp scan_entity_value(
         [],
         s =
           r_xmerl_scanner(
             environment: {:external, {:entity, _}},
             validation: :dtd
           ),
         _Delim,
         _Acc,
         pEName,
         _,
         _
       ) do
    {{:error, {:failed_VC_Proper_Declaration_PE_Nesting, 1, pEName}}, [], s}
  end

  defp scan_entity_value([], s, :no_delim, acc, _, _, []) do
    {:lists.flatten(:lists.reverse(acc)), [], s}
  end

  defp scan_entity_value(
         [],
         s = r_xmerl_scanner(validation: :dtd),
         :no_delim,
         _Acc,
         pEName,
         _,
         _PENesting
       ) do
    {{:error, {:failed_VC_Proper_Declaration_PE_Nesting, 2, pEName}}, [], s}
  end

  defp scan_entity_value(
         [],
         s = r_xmerl_scanner(continuation_fun: f),
         delim,
         acc,
         pEName,
         namespace,
         pENesting
       ) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_entity_value(moreBytes, s1, delim, acc, pEName, namespace, pENesting)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_entity_value(
         [delim | t],
         s = r_xmerl_scanner(validation: :dtd),
         delim,
         _Acc,
         pEName,
         _NS,
         pENesting
       )
       when length(pENesting) != 0 do
    {{:error, {:failed_VC_Proper_Declaration_PE_Nesting, 3, pEName}}, t, s}
  end

  defp scan_entity_value([delim | t], s0, delim, acc, _PEName, _NS, _PENesting) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {:lists.flatten(:lists.reverse(acc)), t, s}
  end

  defp scan_entity_value('%' ++ _T, s = r_xmerl_scanner(environment: :prolog), _, _, _, _, _) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3608, {:error, {:wfc_PEs_In_Internal_Subset}}]
        )

        :ok
    end

    fatal({:error, {:wfc_PEs_In_Internal_Subset}}, s)
  end

  defp scan_entity_value('%' ++ t, s0, delim, acc, pEName, namespace, pENesting) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {pERefName, t1, s1} = scan_pe_reference(t, s)

    cond do
      pERefName == pEName and namespace == :parameter ->
        cond do
          r_xmerl_scanner(s1, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [3618, {:illegal_recursion_in_PE, pEName}]
            )

            :ok
        end

        fatal({:illegal_recursion_in_PE, pEName}, s1)

      true ->
        {expandedRef, s2} =
          case expand_pe_reference(pERefName, s1, :in_literal) do
            tuple when is_tuple(tuple) ->
              {expRef, sx} = fetch_not_parse(tuple, s1)

              {entV, _, s5} =
                scan_entity_value(expRef, sx, :no_delim, [], pERefName, :parameter, [])

              {string_to_char_set(r_xmerl_scanner(s5, :encoding), entV), s5}

            expRef ->
              {string_to_char_set(r_xmerl_scanner(s1, :encoding), expRef), s1}
          end

        s3 = r_xmerl_scanner(s2, col: r_xmerl_scanner(s2, :col) + 1)
        {acc2, _, s4} = scan_entity_value(expandedRef, s3, :no_delim, acc, pEName, namespace, [])

        scan_entity_value(
          t1,
          r_xmerl_scanner(s4, line: r_xmerl_scanner(s3, :line), col: r_xmerl_scanner(s3, :col)),
          delim,
          :lists.reverse(acc2),
          pEName,
          namespace,
          pENesting
        )
    end
  end

  defp scan_entity_value('&' ++ t, s0, delim, acc, pEName, namespace, pENesting) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)

    case t do
      '#' ++ _T ->
        {expRef, t1, s1} = scan_reference(t, s)
        tok = pe_nesting_token(expRef ++ t1, namespace, r_xmerl_scanner(s1, :validation))

        case markup_delimeter(expRef) do
          true ->
            scan_entity_value(
              t1,
              s1,
              delim,
              [expRef | acc],
              pEName,
              namespace,
              pe_push(tok, pENesting, s1)
            )

          _ ->
            expRef2 = string_to_char_set(r_xmerl_scanner(s, :encoding), expRef)

            scan_entity_value(
              expRef2 ++ t1,
              s1,
              delim,
              acc,
              pEName,
              namespace,
              pe_push(tok, pENesting, s1)
            )
        end

      _ ->
        {name, _NamespaceInfo, t1, s1} = scan_name(t, s)
        t2 = scan_mandatory(';', t1, 1, s1, :expected_entity_reference_semicolon)
        s2 = save_refed_entity_name(name, pEName, s1)

        scan_entity_value(
          t2,
          s2,
          delim,
          [[';', :erlang.atom_to_list(name), '&'] | acc],
          pEName,
          namespace,
          pENesting
        )
    end
  end

  defp scan_entity_value('<![' ++ t, s0, delim, acc, pEName, :parameter = nS, pENesting) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 3)
    scan_entity_value(t, s, delim, ['<![' | acc], pEName, nS, pe_push('<![', pENesting, s))
  end

  defp scan_entity_value('[' ++ t, s0, delim, acc, pEName, :parameter = nS, pENesting) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_entity_value(t, s, delim, ['[' | acc], pEName, nS, pe_push('[', pENesting, s))
  end

  defp scan_entity_value('<!--' ++ t, s0, delim, acc, pEName, :parameter = nS, pENesting) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 4)
    scan_entity_value(t, s, delim, ['<!--' | acc], pEName, nS, pe_push('<!--', pENesting, s))
  end

  defp scan_entity_value('<!' ++ t, s0, delim, acc, pEName, :parameter = nS, pENesting) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 2)
    scan_entity_value(t, s, delim, ['<!' | acc], pEName, nS, pe_push('<!', pENesting, s))
  end

  defp scan_entity_value('<?' ++ t, s0, delim, acc, pEName, :parameter = nS, pENesting) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 2)
    scan_entity_value(t, s, delim, ['<?' | acc], pEName, nS, pe_push('<?', pENesting, s))
  end

  defp scan_entity_value('</' ++ t, s0, delim, acc, pEName, :parameter = nS, pENesting) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 2)
    scan_entity_value(t, s, delim, ['</' | acc], pEName, nS, pe_push('</', pENesting, s))
  end

  defp scan_entity_value('<' ++ t, s0, delim, acc, pEName, :parameter = nS, pENesting) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_entity_value(t, s, delim, ['<' | acc], pEName, nS, pe_push('<', pENesting, s))
  end

  defp scan_entity_value('(' ++ t, s0, delim, acc, pEName, :parameter = nS, pENesting) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_entity_value(t, s, delim, ['(' | acc], pEName, nS, pe_push('(', pENesting, s))
  end

  defp scan_entity_value('>' ++ t, s0, delim, acc, pEName, :parameter = nS, pENesting) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_entity_value(t, s, delim, ['>' | acc], pEName, nS, pe_pop('>', pENesting, s))
  end

  defp scan_entity_value('?>' ++ t, s0, delim, acc, pEName, :parameter = nS, pENesting) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 2)
    scan_entity_value(t, s, delim, ['?>' | acc], pEName, nS, pe_pop('?>', pENesting, s))
  end

  defp scan_entity_value('-->' ++ t, s0, delim, acc, pEName, :parameter = nS, pENesting) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 3)
    scan_entity_value(t, s, delim, ['-->' | acc], pEName, nS, pe_pop('-->', pENesting, s))
  end

  defp scan_entity_value(']]>' ++ t, s0, delim, acc, pEName, :parameter = nS, pENesting) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 3)
    scan_entity_value(t, s, delim, [']]>' | acc], pEName, nS, pe_pop(']]>', pENesting, s))
  end

  defp scan_entity_value('/>' ++ t, s0, delim, acc, pEName, :parameter = nS, pENesting) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 2)
    scan_entity_value(t, s, delim, ['/>' | acc], pEName, nS, pe_pop('/>', pENesting, s))
  end

  defp scan_entity_value(')' ++ t, s0, delim, acc, pEName, :parameter = nS, pENesting) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    scan_entity_value(t, s, delim, [')' | acc], pEName, nS, pe_pop(')', pENesting, s))
  end

  defp scan_entity_value('\n' ++ t, s, delim, acc, pEName, namespace, pENesting) do
    scan_entity_value(
      t,
      r_xmerl_scanner(s, line: r_xmerl_scanner(s, :line) + 1),
      delim,
      ['\n' | acc],
      pEName,
      namespace,
      pENesting
    )
  end

  defp scan_entity_value(str, s0, delim, acc, pEName, namespace, pENesting) do
    {ch, t} = to_ucs(r_xmerl_scanner(s0, :encoding), str)

    case :xmerl_lib.is_char(ch) do
      true ->
        :no_debug
        s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
        scan_entity_value(t, s, delim, [ch | acc], pEName, namespace, pENesting)

      false ->
        cond do
          r_xmerl_scanner(s0, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [3757, {:unexpected_char, ch}]
            )

            :ok
        end

        fatal({:unexpected_char, ch}, s0)
    end
  end

  defp save_refed_entity_name(name, pEName, s) do
    case predefined_entity(name) do
      true ->
        s

      _ ->
        save_refed_entity_name1(name, pEName, s)
    end
  end

  defp save_refed_entity_name1(name, pEName, s = r_xmerl_scanner(entity_references: eRefs)) do
    case :lists.keysearch(pEName, 1, eRefs) do
      {:value, {_, refs}} ->
        newRefs =
          case :lists.member(name, refs) do
            true ->
              refs

            _ ->
              [name | refs]
          end

        r_xmerl_scanner(s,
          entity_references: :lists.keyreplace(pEName, 1, eRefs, {pEName, newRefs})
        )

      _ ->
        r_xmerl_scanner(s, entity_references: [{pEName, [name]} | eRefs])
    end
  end

  defp pe_push(tok, stack, _S)
       when tok == '<!' or tok == '<?' or
              tok == '<!--' or tok == '<![' or tok == '[' or tok == '<' or
              tok == '</' or tok == '(' do
    [tok | stack]
  end

  defp pe_push(tok, stack, r_xmerl_scanner(validation: :dtd))
       when tok == ')' or tok == '>' or tok == '?>' or tok == ']]>' or
              tok == '-->' or tok == '/>' do
    [tok | stack]
  end

  defp pe_push(_, stack, _S) do
    stack
  end

  defp pe_pop('>', ['<!' | rest], _S) do
    rest
  end

  defp pe_pop('?>', ['<?' | rest], _S) do
    rest
  end

  defp pe_pop('-->', ['<!--' | rest], _S) do
    rest
  end

  defp pe_pop(']]>', [['[', '<!['] | rest], _S) do
    rest
  end

  defp pe_pop('/>', ['<' | rest], _S) do
    rest
  end

  defp pe_pop('>', ['<' | rest], _S) do
    rest
  end

  defp pe_pop('>', ['</' | rest], _S) do
    rest
  end

  defp pe_pop(')', ['(' | rest], _S) do
    rest
  end

  defp pe_pop(token, _Stack, s = r_xmerl_scanner(validation: :dtd)) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3806, {:error, {:failed_VC_Proper_Declaration_PE_Nesting, 5, token}}]
        )

        :ok
    end

    fatal(
      {:error, {:failed_VC_Proper_Declaration_PE_Nesting, 5, token}},
      s
    )
  end

  defp pe_pop(_, rest, _) do
    rest
  end

  defp pe_nesting_token('<!' ++ _T, :parameter, :dtd) do
    '<!'
  end

  defp pe_nesting_token('<?' ++ _T, :parameter, :dtd) do
    '<?'
  end

  defp pe_nesting_token('<!--' ++ _T, :parameter, :dtd) do
    '<!--'
  end

  defp pe_nesting_token('<![' ++ _T, :parameter, :dtd) do
    '<!['
  end

  defp pe_nesting_token('[' ++ _T, :parameter, :dtd) do
    '['
  end

  defp pe_nesting_token('(' ++ _T, :parameter, :dtd) do
    '('
  end

  defp pe_nesting_token('>' ++ _T, :parameter, :dtd) do
    '>'
  end

  defp pe_nesting_token('?>' ++ _T, :parameter, :dtd) do
    '?>'
  end

  defp pe_nesting_token('-->' ++ _T, :parameter, :dtd) do
    '-->'
  end

  defp pe_nesting_token(']]>' ++ _T, :parameter, :dtd) do
    ']]>'
  end

  defp pe_nesting_token(')' ++ _T, :parameter, :dtd) do
    ')'
  end

  defp pe_nesting_token('/>' ++ _T, :parameter, :dtd) do
    '/>'
  end

  defp pe_nesting_token(_, _, _) do
    false
  end

  defp predefined_entity(:amp) do
    true
  end

  defp predefined_entity(:lt) do
    true
  end

  defp predefined_entity(:gt) do
    true
  end

  defp predefined_entity(:apos) do
    true
  end

  defp predefined_entity(:quot) do
    true
  end

  defp predefined_entity(_) do
    false
  end

  defp check_entity_recursion(
         eName,
         s = r_xmerl_scanner(entity_references: entityRefList)
       ) do
    set = :sofs.family(entityRefList)

    case (try do
            :sofs.family_to_digraph(set, [:acyclic])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, {:cyclic, _}} ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg(
              '~p- fatal: ~p~n',
              [3836, {:illegal_recursion_in_Entity, eName}]
            )

            :ok
        end

        fatal({:illegal_recursion_in_Entity, eName}, s)

      dG ->
        :digraph.delete(dG)
        :ok
    end
  end

  defp scan_comment(str, s) do
    scan_comment(str, s, _Pos = :undefined, _Parents = [], _Lang = [])
  end

  defp scan_comment(str, s = r_xmerl_scanner(col: c, event_fun: event), pos, parents, lang) do
    comment = r_xmlComment(pos: pos, parents: parents, language: lang, value: :undefined)

    s1 =
      r_xmerl_scanner() =
      event.(
        r_xmerl_event(
          event: :started,
          line: r_xmerl_scanner(s, :line),
          col: c,
          pos: pos,
          data: comment
        ),
        s
      )

    scan_comment1(str, s1, pos, comment, _Acc = [])
  end

  defp scan_comment1([], s = r_xmerl_scanner(continuation_fun: f), pos, comment, acc) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        scan_comment1(moreBytes, s1, pos, comment, acc)
      end,
      fatal_fun(:unexpected_end),
      s
    )
  end

  defp scan_comment1(
         '-->' ++ t,
         s0 = r_xmerl_scanner(col: c, event_fun: event, hook_fun: hook),
         _Pos,
         comment,
         acc
       ) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 3)
    comment1 = r_xmlComment(comment, value: :lists.reverse(acc))

    s1 =
      r_xmerl_scanner() =
      event.(
        r_xmerl_event(event: :ended, line: r_xmerl_scanner(s, :line), col: c, data: comment1),
        s
      )

    {ret, s2} = hook.(comment1, s1)
    {_, t3, s3} = strip(t, s2)
    {ret, t3, s3}
  end

  defp scan_comment1('--' ++ t, s, _Pos, _Comment, _Acc) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3882, {:invalid_comment, '--' ++ [hd(t)]}]
        )

        :ok
    end

    fatal({:invalid_comment, '--' ++ [hd(t)]}, s)
  end

  defp scan_comment1('\n' ++ t, s = r_xmerl_scanner(line: l), pos, cmt, acc) do
    scan_comment1(t, r_xmerl_scanner(s, line: l + 1, col: 1), pos, cmt, '\n' ++ acc)
  end

  defp scan_comment1('\r\n' ++ t, s = r_xmerl_scanner(line: l), pos, cmt, acc) do
    scan_comment1(t, r_xmerl_scanner(s, line: l + 1, col: 1), pos, cmt, '\n' ++ acc)
  end

  defp scan_comment1('\r' ++ t, s = r_xmerl_scanner(line: l), pos, cmt, acc) do
    scan_comment1(t, r_xmerl_scanner(s, line: l + 1, col: 1), pos, cmt, '\n' ++ acc)
  end

  defp scan_comment1(str, s = r_xmerl_scanner(col: c), pos, cmt, acc) do
    {ch, t} = wfc_legal_char(str, s)
    scan_comment1(t, r_xmerl_scanner(s, col: c + 1), pos, cmt, [ch | acc])
  end

  defp scan_markup_completion_gt([?> | _R] = t, s) do
    {t, s}
  end

  defp scan_markup_completion_gt([?% | t], s0) do
    :no_debug
    s = r_xmerl_scanner(s0, col: r_xmerl_scanner(s0, :col) + 1)
    {name, t1, s1} = scan_pe_reference(t, s)
    expandedRef = expand_pe_reference(name, s1, :as_PE)
    {_, t2, s2} = strip(expandedRef ++ t1, s1)
    scan_markup_completion_gt(t2, s2)
  end

  defp scan_markup_completion_gt(t, s) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3906, {:error, {:malformed_syntax_entity_completion, t}}]
        )

        :ok
    end

    fatal(
      {:error, {:malformed_syntax_entity_completion, t}},
      s
    )
  end

  defp scan_mandatory(pattern, t, n, s, errorMsg) do
    case :lists.prefix(pattern, t) do
      true ->
        :lists.nthtail(n, t)

      _ ->
        cond do
          r_xmerl_scanner(s, :quiet) ->
            :ok

          true ->
            :error_logger.error_msg('~p- fatal: ~p~n', [3914, errorMsg])
            :ok
        end

        fatal(errorMsg, s)
    end
  end

  defp strip(str, s) do
    strip(str, s, :all)
  end

  defp strip([], s = r_xmerl_scanner(continuation_fun: f), _) do
    :no_debug

    f.(
      fn moreBytes, s1 ->
        strip(moreBytes, s1)
      end,
      fn s1 ->
        {[], [], s1}
      end,
      s
    )
  end

  defp strip(' ' ++ t, s = r_xmerl_scanner(col: c), lim) do
    strip(t, r_xmerl_scanner(s, col: c + 1), lim)
  end

  defp strip('\t' ++ _T, s, :no_tab) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3929, {:error, {:no_tab_allowed}}]
        )

        :ok
    end

    fatal({:error, {:no_tab_allowed}}, s)
  end

  defp strip('\t' ++ t, s = r_xmerl_scanner(col: c), lim) do
    strip(t, r_xmerl_scanner(s, col: expand_tab(c)), lim)
  end

  defp strip('\n' ++ t, s = r_xmerl_scanner(line: l), lim) do
    strip(t, r_xmerl_scanner(s, line: l + 1, col: 1), lim)
  end

  defp strip('\r\n' ++ t, s = r_xmerl_scanner(line: l), lim) do
    strip(t, r_xmerl_scanner(s, line: l + 1, col: 1), lim)
  end

  defp strip('\r' ++ t, s = r_xmerl_scanner(line: l), lim) do
    strip(t, r_xmerl_scanner(s, line: l + 1, col: 1), lim)
  end

  defp strip(str, s, _Lim) do
    {[], str, s}
  end

  defp mandatory_strip([], s) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3946, {:error, {:whitespace_was_expected}}]
        )

        :ok
    end

    fatal({:error, {:whitespace_was_expected}}, s)
  end

  defp mandatory_strip(t, s)
       when hd(t) == 32 or hd(t) == 13 or
              hd(t) == 10 or hd(t) == 9 do
    strip(t, s, :all)
  end

  defp mandatory_strip([?% | t], s)
       when hd(t) == 32 or hd(t) == 13 or
              hd(t) == 10 or hd(t) == 9 do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3950, {:error, {:whitespace_was_expected}}]
        )

        :ok
    end

    fatal({:error, {:whitespace_was_expected}}, s)
  end

  defp mandatory_strip([?% | _T] = t, s) do
    {[], t, s}
  end

  defp mandatory_strip(_T, s) do
    cond do
      r_xmerl_scanner(s, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [3954, {:error, {:whitespace_was_expected}}]
        )

        :ok
    end

    fatal({:error, {:whitespace_was_expected}}, s)
  end

  defp pub_id_strip(str, s) do
    strip(str, s, :no_tab)
  end

  defp normalize('&' ++ t, s, isNorm) do
    case scan_reference(t, s) do
      {expRef, t1, s1}
      when hd(expRef) == 32 or
             hd(expRef) == 13 or hd(expRef) == 10 or
             hd(expRef) == 9 ->
        expRef2 = string_to_char_set(r_xmerl_scanner(s, :encoding), expRef)
        normalize(expRef2 ++ t1, s1, isNorm)

      _ ->
        {'&' ++ t, s, isNorm}
    end
  end

  defp normalize(t, s, isNorm) do
    case strip(t, s) do
      {_, ^t, ^s} ->
        {t, s, isNorm}

      {_, t1, s1} ->
        normalize(t1, s1, true)
    end
  end

  defp fast_accumulate_whitespace(' ' ++ t, s, _) do
    fast_acc_spaces(t, s, 1)
  end

  defp fast_accumulate_whitespace('\t' ++ t, s, _) do
    fast_acc_tabs(t, s, 1)
  end

  defp fast_accumulate_whitespace('<' ++ _ = r, s, _T) do
    r_xmerl_scanner(common_data: cD, line: line) = s
    {:done, {:erlang.element(3, cD), r, r_xmerl_scanner(s, col: 1, line: line + 1)}}
  end

  defp fast_accumulate_whitespace(_, s, t) do
    accumulate_whitespace(t, s, [])
  end

  defp fast_acc_spaces(' ' ++ t, s, n) do
    fast_acc_spaces(t, s, n + 1)
  end

  defp fast_acc_spaces(t, s, n) do
    fast_acc_end(t, s, n, n, ?\s, 1)
  end

  defp fast_acc_tabs('\t' ++ t, s, n) do
    fast_acc_tabs(t, s, n + 1)
  end

  defp fast_acc_tabs(t, s, n) do
    fast_acc_end(t, s, n, n * 8 + 1, ?\t, 2)
  end

  defp fast_acc_end(t, s, n, col, c, cD_I) do
    r_xmerl_scanner(common_data: cD, line: line0) = s
    line = line0 + 1

    try do
      ?< = hd(t)

      {:done,
       {:erlang.element(n, :erlang.element(cD_I, cD)), t,
        r_xmerl_scanner(s, col: col, line: line)}}
    catch
      _, _ ->
        accumulate_whitespace(t, s, line, col, :lists.duplicate(n, c) ++ '\n')
    end
  end

  def accumulate_whitespace(t, s, :preserve, acc) do
    accumulate_whitespace(t, s, acc)
  end

  def accumulate_whitespace(t, s, :normalize, acc) do
    {_WsAcc, t1, s1} = accumulate_whitespace(t, s, [])
    {[?\s | acc], t1, s1}
  end

  defp accumulate_whitespace(t, s, acc) do
    r_xmerl_scanner(line: line, col: col) = s
    accumulate_whitespace(t, s, line, col, acc)
  end

  defp accumulate_whitespace([], s0, line, col, acc) do
    r_xmerl_scanner(continuation_fun: f) = s0
    s = r_xmerl_scanner(s0, line: line, col: col)
    :no_debug

    f.(
      fn moreBytes, s1 ->
        accumulate_whitespace(moreBytes, s1, acc)
      end,
      fn s1 ->
        {acc, [], s1}
      end,
      s
    )
  end

  defp accumulate_whitespace(' ' ++ t, s, line, col, acc) do
    accumulate_whitespace(t, s, line, col + 1, [?\s | acc])
  end

  defp accumulate_whitespace('\t' ++ t, s, line, col, acc) do
    accumulate_whitespace(t, s, line, expand_tab(col), [?\t | acc])
  end

  defp accumulate_whitespace('\n' ++ t, s, line, _Col, acc) do
    accumulate_whitespace(t, s, line + 1, 1, [?\n | acc])
  end

  defp accumulate_whitespace('\r\n' ++ t, s, line, _Col, acc) do
    accumulate_whitespace(t, s, line + 1, 1, [?\n | acc])
  end

  defp accumulate_whitespace('\r' ++ t, s, line, _Col, acc) do
    accumulate_whitespace(t, s, line + 1, 1, [?\n | acc])
  end

  defp accumulate_whitespace(str, s, line, col, acc) do
    {acc, str, r_xmerl_scanner(s, line: line, col: col)}
  end

  defp expand_tab(col) do
    rem = rem(col - 1, 8)
    _NewCol = col + 8 - rem
  end

  defp validation_mode(false) do
    :off
  end

  defp validation_mode(true) do
    :dtd
  end

  defp validation_mode(other) do
    other
  end

  defp schemaLocations(el, r_xmerl_scanner(schemaLocation: [])) do
    schemaLocations(el)
  end

  defp schemaLocations(el, r_xmerl_scanner(schemaLocation: sL)) do
    case sL do
      [{_, _} | _] ->
        {:ok, sL}

      _ ->
        schemaLocations(el)
    end
  end

  defp schemaLocations(r_xmlElement(attributes: atts, xmlbase: _Base)) do
    pred = fn
      r_xmlAttribute(name: :schemaLocation) ->
        false

      r_xmlAttribute(nsinfo: {_, 'schemaLocation'}) ->
        false

      _ ->
        true
    end

    case :lists.dropwhile(pred, atts) do
      [r_xmlAttribute(value: paths) | _] ->
        case :string.tokens(paths, ' \n\t\r') do
          l when length(l) > 0 ->
            case rem(length(l), 2) do
              0 ->
                pairList = fn
                  [], _Fun ->
                    []

                  [[sLNS, sLLoc] | rest], fun ->
                    [{sLNS, sLLoc} | fun.(rest, fun)]
                end

                {:ok, pairList.(l, pairList)}

              _ ->
                {:error, {:schemaLocation_attribute, :namespace_location_not_in_pair}}
            end

          _ ->
            {:error, {:missing_schemaLocation}}
        end

      [] ->
        {:error, {:missing_schemaLocation}}
    end
  end

  defp inherit_options(s) do
    [{:xsdbase, r_xmerl_scanner(s, :xmlbase)}]
  end

  defp handle_schema_result({xSDRes = r_xmlElement(), _}, s5) do
    {xSDRes, s5}
  end

  defp handle_schema_result({:error, reason}, s5) do
    cond do
      r_xmerl_scanner(s5, :quiet) ->
        :ok

      true ->
        :error_logger.error_msg(
          '~p- fatal: ~p~n',
          [4112, {:failed_schema_validation, reason}]
        )

        :ok
    end

    fatal({:failed_schema_validation, reason}, s5)
  end

  defp fatal_fun(reason) do
    fn s ->
      cond do
        r_xmerl_scanner(s, :quiet) ->
          :ok

        true ->
          :error_logger.error_msg('~p- fatal: ~p~n', [4121, reason])
          :ok
      end

      fatal(reason, s)
    end
  end

  defp fatal(reason, s) do
    exit(
      {:fatal,
       {reason, {:file, r_xmerl_scanner(s, :filename)}, {:line, r_xmerl_scanner(s, :line)},
        {:col, r_xmerl_scanner(s, :col)}}}
    )
  end

  defp preformat(l1, l2, sep) do
    format1 =
      :lists.flatten(
        :lists.duplicate(
          length(l1) - 1,
          '~s '
        ) ++ '~s'
      )

    format2 =
      :lists.flatten(
        :lists.duplicate(
          length(l2) - 1,
          ' ~s' ++ sep
        ) ++ ' ~s'
      )

    :lists.flatten(
      :io_lib.format(
        format1 ++ format2,
        l1 ++ l2
      )
    )
  end

  defp rules_write(context, name, value, r_xmerl_scanner(rules: t) = s) do
    case :ets.lookup(t, {context, name}) do
      [] ->
        :ets.insert(t, {{context, name}, value})

      _ ->
        :ok
    end

    s
  end

  defp rules_read(context, name, r_xmerl_scanner(rules: t)) do
    case :ets.lookup(t, {context, name}) do
      [] ->
        :undefined

      [{_, v}] ->
        v
    end
  end

  defp rules_delete(context, name, r_xmerl_scanner(rules: t)) do
    :ets.delete(t, {context, name})
  end

  defp to_ucs(encoding, chars)
       when encoding == 'utf-8' or
              encoding == :undefined do
    utf8_2_ucs(chars)
  end

  defp to_ucs(_, [c | rest]) do
    {c, rest}
  end

  defp utf8_2_ucs([[a, b, c, d] | rest])
       when a &&& 248 === 240 and b &&& 192 === 128 and
              c &&& 192 === 128 and d &&& 192 === 128 do
    case (d &&& 63) ||| (c &&& 63 <<< 6) ||| (b &&& 63 <<< 12) ||| (a &&& 7 <<< 18) do
      ch when ch >= 65536 ->
        {ch, rest}

      ch ->
        {{:error, {:bad_character, ch}}, rest}
    end
  end

  defp utf8_2_ucs([[a, b, c] | rest])
       when a &&& 240 === 224 and
              b &&& 192 === 128 and
              c &&& 192 === 128 do
    case (c &&& 63) ||| (b &&& 63 <<< 6) ||| (a &&& 15 <<< 12) do
      ch when ch >= 2048 ->
        {ch, rest}

      ch ->
        {{:error, {:bad_character, ch}}, rest}
    end
  end

  defp utf8_2_ucs([[a, b] | rest])
       when a &&& 224 === 192 and
              b &&& 192 === 128 do
    case (b &&& 63) ||| (a &&& 31 <<< 6) do
      ch when ch >= 128 ->
        {ch, rest}

      ch ->
        {{:error, {:bad_character, ch}}, rest}
    end
  end

  defp utf8_2_ucs([a | rest]) when a < 128 do
    {a, rest}
  end

  defp utf8_2_ucs([a | rest]) do
    {{:error, {:bad_character, a}}, rest}
  end

  defp ucs_2_utf8(ch) when ch < 128 do
    [ch]
  end

  defp ucs_2_utf8(ch) when ch < 2048 do
    [(ch &&& 1984 >>> 6) ||| 192, (ch &&& 63) ||| 128]
  end

  defp ucs_2_utf8(ch) when ch < 65536 do
    [(ch &&& 61440 >>> 12) ||| 224, (ch &&& 4032 >>> 6) ||| 128, (ch &&& 63) ||| 128]
  end

  defp ucs_2_utf8(ch) when ch < 2_097_152 do
    [
      (ch &&& 1_835_008 >>> 18) ||| 240,
      (ch &&& 258_048 >>> 12) ||| 128,
      (ch &&& 4032 >>> 6) ||| 128,
      (ch &&& 63) ||| 128
    ]
  end

  defp string_to_char_set(enc, str)
       when enc === 'utf-8' or
              enc === :undefined do
    :lists.flatten(
      for x <- str do
        ucs_2_utf8(x)
      end
    )
  end

  defp string_to_char_set(_, str) do
    str
  end
end
