defmodule :m_docgen_edoc_xml_cb do
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

  def module(element, opts) do
    sortP = :proplists.get_value(:sort_functions, opts, true)
    xML = layout_module(element, sortP)
    rootAttributes = root_attributes(element, opts)
    :xmerl.export_simple([xML], :docgen_xmerl_xml_cb, rootAttributes)
  end

  def overview(element, opts) do
    xML = layout_chapter(element)
    rootAttributes = root_attributes(element, opts)
    :xmerl.export_simple([xML], :docgen_xmerl_xml_cb, rootAttributes)
  end

  defp layout_module(r_xmlElement(name: :module, content: es) = e, sortP) do
    :erlang.put(:type, :module)
    name = get_attrval(:name, e)
    desc = get_content(:description, es)

    shortDesc =
      text_only(
        get_content(
          :briefDescription,
          desc
        )
      )

    fullDesc =
      otp_xmlify(
        get_content(
          :fullDescription,
          desc
        )
      )

    types0 = get_content(:typedecls, es)

    types1 =
      :lists.sort(
        for et <- types0 do
          {type_name(et), et}
        end
      )

    functions =
      case sortP do
        true ->
          :lists.sort(
            for ef <- get_content(:functions, es) do
              {function_name(ef), ef}
            end
          )

        false ->
          for ef <- get_content(:functions, es) do
            {function_name(ef), ef}
          end
      end

    header =
      {:header,
       [
         '\n',
         {:title, [name]},
         '\n',
         {:prepared, ['']},
         '\n',
         {:responsible, ['']},
         '\n',
         {:docno, ['1']},
         '\n',
         {:approved, ['']},
         '\n',
         {:checked, ['']},
         '\n',
         {:date, ['']},
         '\n',
         {:rev, ['A']},
         '\n',
         {:file, [name ++ '.xml']}
       ]}

    module = {:module, [name]}
    moduleSummary = {:modulesummary, shortDesc}
    description = {:description, ['\n' | fullDesc]}

    types =
      case types1 do
        [] ->
          []

        _ ->
          [
            '\n',
            {:section,
             [
               {:title, ['DATA TYPES']},
               {:marker, [{:id, 'types'}], []},
               '\n'
               | types(types1)
             ]}
          ]
      end

    funcs = functions(functions)
    see = seealso_module(es)
    authors = {:authors, authors(es)}

    {:erlref,
     ['\n', header, '\n', module, '\n', moduleSummary, '\n', description] ++
       types ++ ['\n', funcs, '\n', see, '\n', authors]}
  end

  defp root_attributes(element, opts) do
    encoding =
      case get_attrval(:encoding, element) do
        '' ->
          defaultEncoding = :epp.default_encoding()
          :proplists.get_value(:encoding, opts, defaultEncoding)

        enc ->
          enc
      end

    [r_xmlAttribute(name: :encoding, value: reformat_encoding(encoding))]
  end

  defp reformat_encoding(:utf8) do
    'UTF-8'
  end

  defp reformat_encoding(list) when is_list(list) do
    case :string.lowercase(list) do
      'utf8' ->
        'UTF-8'

      _ ->
        list
    end
  end

  defp reformat_encoding(other) do
    other
  end

  defp layout_chapter(r_xmlElement(name: :overview, content: es)) do
    :erlang.put(:type, :chapter)
    title = get_text(:title, es)

    header =
      {:header,
       [
         '\n',
         {:title, [title]},
         '\n',
         {:prepared, ['']},
         '\n',
         {:docno, ['']},
         '\n',
         {:date, ['']},
         '\n',
         {:rev, ['']},
         '\n',
         {:file, ['chapter.xml']}
       ]}

    descEs = get_content(:description, es)
    fullDescEs = get_content(:fullDescription, descEs)
    sections = chapter_ify(fullDescEs, :first)
    {:chapter, ['\n', header, '\n' | sections]}
  end

  defp chapter_ify([], _) do
    []
  end

  defp chapter_ify(es, :first) do
    case find_next(:h3, es) do
      {^es, []} ->
        subSections = subchapter_ify(es, :first)
        [{:section, ['\n', {:title, ['Overview']}, '\n' | subSections]}]

      {firstEs, restEs} ->
        otp_xmlify(firstEs) ++ chapter_ify(restEs, :next)
    end
  end

  defp chapter_ify([r_xmlElement(name: :h3) = e | es], :next) do
    {sectionEs, restEs} = find_next(:h3, es)
    subSections = subchapter_ify(sectionEs, :first)
    {marker, title} = chapter_title(e)

    [
      {:section,
       ['\n', {:marker, [{:id, marker}], []}, '\n', {:title, [title]}, '\n' | subSections]}
      | chapter_ify(restEs, :next)
    ]
  end

  defp subchapter_ify([], _) do
    []
  end

  defp subchapter_ify(es, :first) do
    {firstEs, restEs} = find_next(:h4, es)
    otp_xmlify(firstEs) ++ subchapter_ify(restEs, :next)
  end

  defp subchapter_ify([r_xmlElement(name: :h4) = e | es], :next) do
    {sectionEs, restEs} = find_next(:h4, es)
    elements = otp_xmlify(sectionEs)
    {marker, title} = chapter_title(e)

    [
      {:section, ['\n', {:marker, [{:id, marker}], []}, '\n', {:title, [title]}, '\n' | elements]}
      | subchapter_ify(restEs, :next)
    ]
  end

  defp chapter_title(r_xmlElement(content: es)) do
    case es do
      [r_xmlElement(name: :a) = e] ->
        {get_attrval(:name, e), get_text(e)}
    end
  end

  defp otp_xmlify([]) do
    []
  end

  defp otp_xmlify(es0) do
    es =
      case is_paragraph(hd(es0)) do
        true ->
          es0

        false ->
          case find_next(:p, es0, []) do
            {[r_xmlText(value: str)] = first, rest} ->
              case is_empty(str) do
                true ->
                  rest

                false ->
                  [{:p, first} | rest]
              end

            {first, rest} ->
              [{:p, first} | rest]
          end
      end

    esFixed = otp_xmlify_fix(es)
    otp_xmlify_es(esFixed)
  end

  defp otp_xmlify_fix(es) do
    otp_xmlify_fix(es, [])
  end

  defp otp_xmlify_fix([r_xmlText(value: '\n \n' ++ _) = e1, e2 | es], res) do
    case is_paragraph(e2) do
      false ->
        {p, after__} = find_p_ending(es, [])
        otp_xmlify_fix(after__, [{:p, [e2 | p]}, e1 | res])

      true ->
        otp_xmlify_fix([e2 | es], [e1 | res])
    end
  end

  defp otp_xmlify_fix([r_xmlText(value: '\n\n') = e1, e2 | es], res) do
    case is_paragraph(e2) do
      false ->
        {p, after__} = find_p_ending(es, [])
        otp_xmlify_fix(after__, [{:p, [e2 | p]}, e1 | res])

      true ->
        otp_xmlify_fix([e2 | es], [e1 | res])
    end
  end

  defp otp_xmlify_fix([e | es], res) do
    otp_xmlify_fix(es, [e | res])
  end

  defp otp_xmlify_fix([], res) do
    :lists.reverse(res)
  end

  defp otp_xmlify_es([e | es]) do
    case is_paragraph(e) do
      true ->
        case otp_xmlify_psplit(e) do
          :nosplit ->
            otp_xmlify_e(e) ++ otp_xmlify_es(es)

          subEs ->
            :lists.flatmap(
              &otp_xmlify_e/1,
              subEs
            ) ++ otp_xmlify_es(es)
        end

      false ->
        otp_xmlify_e(e) ++ otp_xmlify_es(es)
    end
  end

  defp otp_xmlify_es([]) do
    []
  end

  defp otp_xmlify_psplit(p) do
    otp_xmlify_psplit(p_content(p), [], [])
  end

  defp otp_xmlify_psplit([r_xmlElement(name: name) = e | es], content, res) do
    cond do
      name == :blockquote or name == :ul or name == :ol or
        name == :dl or name == :pre or name == :table ->
        case content do
          [] ->
            otp_xmlify_psplit(es, [], [e | res])

          [r_xmlText(value: str)] ->
            case is_empty(str) do
              true ->
                otp_xmlify_psplit(es, [], [e | res])

              false ->
                pnew = {:p, :lists.reverse(content)}
                otp_xmlify_psplit(es, [], [e, pnew | res])
            end

          _ ->
            pnew = {:p, :lists.reverse(content)}
            otp_xmlify_psplit(es, [], [e, pnew | res])
        end

      true ->
        otp_xmlify_psplit(es, [e | content], res)
    end
  end

  defp otp_xmlify_psplit([e | es], content, res) do
    otp_xmlify_psplit(es, [e | content], res)
  end

  defp otp_xmlify_psplit([], _Content, []) do
    :nosplit
  end

  defp otp_xmlify_psplit([], [], res) do
    :lists.reverse(res)
  end

  defp otp_xmlify_psplit([], [r_xmlText(value: '\n\n')], res) do
    :lists.reverse(res)
  end

  defp otp_xmlify_psplit([], content, res) do
    pnew = {:p, :lists.reverse(content)}
    :lists.reverse([pnew | res])
  end

  defp otp_xmlify_e(r_xmlElement(name: :a) = e) do
    otp_xmlify_a(e)
  end

  defp otp_xmlify_e(r_xmlElement(name: tag) = e)
       when tag == :b or
              tag == :em or tag == :pre do
    content = text_only(r_xmlElement(e, :content))
    [r_xmlElement(e, content: content)]
  end

  defp otp_xmlify_e(r_xmlElement(name: :code) = e) do
    case (try do
            text_only(r_xmlElement(e, :content))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _Error} ->
        otp_xmlify_code(e)

      content ->
        [r_xmlElement(e, content: content)]
    end
  end

  defp otp_xmlify_e(r_xmlElement(name: tag) = e)
       when tag == :h1 or
              tag == :h2 or tag == :h3 or tag == :h4 or
              tag == :h5 do
    {name, text} = text_and_a_name_only(r_xmlElement(e, :content))
    [name, r_xmlElement(e, name: :b, content: text)]
  end

  defp otp_xmlify_e(r_xmlElement(name: tag) = e)
       when tag == :center or
              tag == :font do
    otp_xmlify_e(r_xmlElement(e, :content))
  end

  defp otp_xmlify_e(r_xmlElement(name: :table) = e) do
    case parent(e) do
      :module ->
        otp_xmlify_table(r_xmlElement(e, :content))

      :overview ->
        content0 = otp_xmlify_e(r_xmlElement(e, :content))
        summary = r_xmlText(value: get_attrval(:summary, e))
        tCaption = r_xmlElement(e, name: :tcaption, attributes: [], content: [summary])
        content = content0 ++ [tCaption]
        [r_xmlElement(e, attributes: [], content: content)]
    end
  end

  defp otp_xmlify_e(r_xmlElement(name: :tbody) = e) do
    otp_xmlify_e(r_xmlElement(e, :content))
  end

  defp otp_xmlify_e(r_xmlElement(name: :sup) = e) do
    text = get_text(e)

    [
      r_xmlText(
        parents: r_xmlElement(e, :parents),
        pos: r_xmlElement(e, :pos),
        language: r_xmlElement(e, :language),
        value: '(' ++ text ++ ')'
      )
    ]
  end

  defp otp_xmlify_e(r_xmlElement(name: :blockquote) = e) do
    content = otp_xmlify_blockquote(r_xmlElement(e, :content))
    [r_xmlElement(e, content: content)]
  end

  defp otp_xmlify_e(r_xmlElement(name: :th) = e) do
    content = otp_xmlify_e(r_xmlElement(e, :content))
    emE = r_xmlElement(e, name: :em, content: content)
    [r_xmlElement(e, name: :td, content: [emE])]
  end

  defp otp_xmlify_e(r_xmlElement(name: :p) = e) do
    content = otp_xmlify_e(r_xmlElement(e, :content))
    [r_xmlElement(e, content: content)]
  end

  defp otp_xmlify_e({:p, content1}) do
    content2 = otp_xmlify_e(content1)
    [{:p, content2}]
  end

  defp otp_xmlify_e(r_xmlElement(name: :ul) = e) do
    content = otp_xmlify_e(r_xmlElement(e, :content))
    [r_xmlElement(e, content: content)]
  end

  defp otp_xmlify_e(r_xmlElement(name: :li) = e) do
    content = otp_xmlify(r_xmlElement(e, :content))
    [r_xmlElement(e, content: content)]
  end

  defp otp_xmlify_e(r_xmlElement(name: :dl) = e) do
    content0 = otp_xmlify_e(r_xmlElement(e, :content))
    content = otp_xmlify_dl(content0)
    [r_xmlElement(e, content: content)]
  end

  defp otp_xmlify_e(r_xmlElement(name: :dt) = e) do
    content =
      case r_xmlElement(e, :content) do
        [r_xmlElement(name: :a) = a] ->
          :erlang.put(:dt_marker, otp_xmlify_e(a))
          otp_xmlify_e(r_xmlElement(a, :content))

        _ ->
          otp_xmlify_e(r_xmlElement(e, :content))
      end

    [r_xmlElement(e, content: content)]
  end

  defp otp_xmlify_e(r_xmlElement(name: :dd) = e) do
    content0 = otp_xmlify(r_xmlElement(e, :content))

    content =
      case :erlang.get(:dt_marker) do
        :undefined ->
          content0

        [marker] ->
          :erlang.put(:dt_marker, :undefined)
          [r_xmlElement(marker, content: []) | content0]
      end

    [r_xmlElement(e, content: content)]
  end

  defp otp_xmlify_e(r_xmlElement(name: :tr) = e) do
    content = otp_xmlify_e(r_xmlElement(e, :content))
    [r_xmlElement(e, content: content)]
  end

  defp otp_xmlify_e(r_xmlElement(name: :td) = e) do
    content = otp_xmlify_e(r_xmlElement(e, :content))
    [r_xmlElement(e, content: content)]
  end

  defp otp_xmlify_e(r_xmlElement(name: :img) = e) do
    content = otp_xmlify_e(r_xmlElement(e, :content))
    [otp_xmlify_img(r_xmlElement(e, content: content))]
  end

  defp otp_xmlify_e([e | es]) do
    otp_xmlify_e(e) ++ otp_xmlify_e(es)
  end

  defp otp_xmlify_e([]) do
    []
  end

  defp otp_xmlify_e(e) do
    [e]
  end

  defp otp_xmlify_a(a) do
    [attr0] = filter_a_attrs(r_xmlElement(a, :attributes))

    case attr0 do
      r_xmlAttribute(name: :href, value: href0) ->
        content0 = text_only(r_xmlElement(a, :content))
        {href, content} = otp_xmlify_a_href(href0, content0)

        [
          r_xmlElement(a,
            attributes: [r_xmlAttribute(attr0, value: href)],
            content: content
          )
        ]

      r_xmlAttribute(name: :name) ->
        content = otp_xmlify_e(r_xmlElement(a, :content))
        [r_xmlElement(a, attributes: [attr0], content: content)]
    end
  end

  defp filter_a_attrs([r_xmlAttribute(name: :href) = attr | _Attrs]) do
    [attr]
  end

  defp filter_a_attrs([r_xmlAttribute(name: :name) = attr | _Attrs]) do
    [attr]
  end

  defp filter_a_attrs([_Attr | attrs]) do
    filter_a_attrs(attrs)
  end

  defp filter_a_attrs([]) do
    []
  end

  defp otp_xmlify_a_href('#' ++ _ = marker, es0) do
    {make_mfa_anchor(marker), es0}
  end

  defp otp_xmlify_a_href('http:' ++ _ = uRL, es0) do
    {uRL, es0}
  end

  defp otp_xmlify_a_href('https:' ++ _ = uRL, es0) do
    {uRL, es0}
  end

  defp otp_xmlify_a_href('OTPROOT' ++ appRef, es0) do
    [appS, 'doc', fileRef1] = split(appRef, '/')

    fileRef =
      appS ++
        ':' ++
        otp_xmlify_a_fileref(
          fileRef1,
          appS
        )

    [r_xmlText(value: str0) = t] = es0

    str =
      case split(str0, '/') do
        [appS2] ->
          appS2

        [_AppS, modRef] ->
          case split(modRef, ':') do
            [module] ->
              module ++ '(3)'

            [_Module, _Type] ->
              modRef
          end

        [_AppS, modFunc, arity] ->
          modFunc ++ '/' ++ arity
      end

    {fileRef, [r_xmlText(t, value: str)]}
  end

  defp otp_xmlify_a_href('../' ++ file, es0) do
    {'../../' ++ file, es0}
  end

  defp otp_xmlify_a_href(fileRef1, es0) do
    fileRef2 = otp_xmlify_a_fileref(fileRef1, :this)
    {fileRef2, es0}
  end

  defp otp_xmlify_a_fileref(fileRef1, appS) do
    case split(fileRef1, '.#') do
      ['overview-summary', _Ext] ->
        'chapter'

      ['overview-summary', _Ext, marker] ->
        'chapter#' ++ marker

      [file, ext]
      when ext == 'xml' or
             (ext == 'html' and appS != :this) ->
        file

      [file, ext, marker0] ->
        marker = make_mfa_anchor(marker0)

        cond do
          ext == 'xml' or (ext == 'html' and appS != :this) ->
            file ++ '#' ++ marker

          true ->
            file ++ '.' ++ ext ++ '#' ++ marker
        end

      _ ->
        fileRef1
    end
  end

  defp otp_xmlify_blockquote([r_xmlElement(name: :p) = e | es]) do
    [e | otp_xmlify_blockquote(es)]
  end

  defp otp_xmlify_blockquote([r_xmlText() = e | es]) do
    {p, after__} = find_p_ending(es, [])
    [{:p, [e | p]} | otp_xmlify_blockquote(after__)]
  end

  defp otp_xmlify_blockquote([]) do
    []
  end

  defp otp_xmlify_code(e) do
    otp_xmlify_code(e, r_xmlElement(e, :content), [])
  end

  defp otp_xmlify_code(code, [r_xmlText() = e | es], acc) do
    otp_xmlify_code(code, es, [r_xmlElement(code, content: [e]) | acc])
  end

  defp otp_xmlify_code(code, [r_xmlElement() = e | es], acc) do
    otp_xmlify_code(code, es, [e | acc])
  end

  defp otp_xmlify_code(_Code, [], acc) do
    :lists.reverse(acc)
  end

  defp otp_xmlify_dl([r_xmlElement(name: :dt) = e | es]) do
    [e | otp_xmlify_dl(es, e)]
  end

  defp otp_xmlify_dl([e | es]) do
    [e | otp_xmlify_dl(es)]
  end

  defp otp_xmlify_dl([]) do
    []
  end

  defp otp_xmlify_dl([r_xmlElement(name: :dd) = e | es], _DT) do
    [e | otp_xmlify_dl(es)]
  end

  defp otp_xmlify_dl([r_xmlElement(name: :dt) = e | es], dT) do
    dD = r_xmlElement(dT, name: :dd, attributes: [], content: [])
    [dD, e | otp_xmlify_dl(es, e)]
  end

  defp otp_xmlify_dl([e | es], dT) do
    [e | otp_xmlify_dl(es, dT)]
  end

  defp otp_xmlify_dl([], dT) do
    dD = r_xmlElement(dT, name: :dd, attributes: [], content: [])
    [dD]
  end

  defp otp_xmlify_table([r_xmlText() = e | es]) do
    [e | otp_xmlify_table(es)]
  end

  defp otp_xmlify_table([r_xmlElement(name: :tbody) = e | es]) do
    otp_xmlify_table(r_xmlElement(e, :content)) ++ otp_xmlify_table(es)
  end

  defp otp_xmlify_table([r_xmlElement(name: :tr, content: content) | es]) do
    otp_xmlify_table(content) ++ [{:br, []}] ++ otp_xmlify_table(es)
  end

  defp otp_xmlify_table([r_xmlElement(name: :th, content: content) | es]) do
    [{:em, content} | otp_xmlify_table(es)]
  end

  defp otp_xmlify_table([r_xmlElement(name: :td, content: content) | es]) do
    otp_xmlify_e(content) ++ otp_xmlify_table(es)
  end

  defp otp_xmlify_table([]) do
    []
  end

  defp otp_xmlify_img(e0) do
    attrs =
      :lists.map(
        fn
          r_xmlAttribute(name: :src, value: path) = a ->
            v = otp_xmlify_a_fileref(path, :this)
            r_xmlAttribute(a, name: :file, value: v)

          a ->
            a
        end,
        r_xmlElement(e0, :attributes)
      )

    r_xmlElement(e0, name: :image, expanded_name: :image, attributes: attrs)
  end

  defp make_mfa_anchor(marker) do
    case split(marker, '-') do
      [func, arity] ->
        try do
          :erlang.list_to_integer(arity)
        catch
          _, _ ->
            marker
        else
          _ ->
            func ++ '/' ++ arity
        end

      _ ->
        marker
    end
  end

  defp find_next(tag, es) do
    find_next(tag, es, [])
  end

  defp find_next(tag, [r_xmlElement(name: tag) = e | es], accEs) do
    {:lists.reverse(accEs), [e | es]}
  end

  defp find_next(tag, [e | es], accEs) do
    find_next(tag, es, [e | accEs])
  end

  defp find_next(_Tag, [], accEs) do
    {:lists.reverse(accEs), []}
  end

  defp find_p_ending([r_xmlText(value: '\n \n' ++ _) = e | es], p) do
    {:lists.reverse(p), [e | es]}
  end

  defp find_p_ending([r_xmlElement(name: :p) = e | es], p) do
    {:lists.reverse(p), [e | es]}
  end

  defp find_p_ending([e | es], p) do
    find_p_ending(es, [e | p])
  end

  defp find_p_ending([], p) do
    {:lists.reverse(p), []}
  end

  defp is_paragraph(r_xmlElement(name: :p)) do
    true
  end

  defp is_paragraph({:p, _Es}) do
    true
  end

  defp is_paragraph(_E) do
    false
  end

  defp p_content(r_xmlElement(content: content)) do
    content
  end

  defp p_content({:p, content}) do
    content
  end

  defp is_empty('\n' ++ str) do
    is_empty(str)
  end

  defp is_empty(' ' ++ str) do
    is_empty(str)
  end

  defp is_empty('\t' ++ str) do
    is_empty(str)
  end

  defp is_empty('') do
    true
  end

  defp is_empty(_) do
    false
  end

  defp split(str, seps) do
    split(str, seps, [])
  end

  defp split([ch | str], seps, acc) do
    case :lists.member(ch, seps) do
      true ->
        split(str, seps, acc)

      false ->
        split(str, seps, acc, [ch])
    end
  end

  defp split([], _Seps, acc) do
    :lists.reverse(acc)
  end

  defp split([ch | str], seps, acc, chs) do
    case :lists.member(ch, seps) do
      true ->
        split(str, seps, [:lists.reverse(chs) | acc])

      false ->
        split(str, seps, acc, [ch | chs])
    end
  end

  defp split([], _Seps, acc, chs) do
    :lists.reverse([:lists.reverse(chs) | acc])
  end

  defp function_name(e) do
    get_attrval(:name, e) ++ '/' ++ get_attrval(:arity, e)
  end

  defp functions(fs) do
    es =
      :lists.flatmap(
        fn {name, e} ->
          function(name, e)
        end,
        fs
      )

    cond do
      es == [] ->
        []

      true ->
        {:funcs, es}
    end
  end

  defp function(_Name, e = r_xmlElement(content: es)) do
    typeSpec = get_content(:typespec, es)

    funcHeaders =
      case funcheader(typeSpec) do
        [] ->
          [
            signature(
              get_content(:args, es),
              get_attrval(:name, e)
            )
          ]

        specs ->
          specs
      end

    [
      '\n',
      {:func,
       ['\n'] ++
         for spec <- funcHeaders do
           {:name, [{:since, ''}], spec}
         end ++
         [
           '\n',
           {:fsummary, fsummary(es)},
           '\n',
           local_types(typeSpec),
           '\n',
           {:desc, label_anchor(e) ++ deprecated(es) ++ fulldesc(es) ++ seealso_function(es)}
         ]}
    ]
  end

  defp fsummary([]) do
    [' ']
  end

  defp fsummary(es) do
    desc = get_content(:description, es)

    case get_content(:briefDescription, desc) do
      [] ->
        fsummary_equiv(es)

      shortDesc ->
        text_only(shortDesc)
    end
  end

  defp fsummary_equiv(es) do
    case get_content(:equiv, es) do
      [] ->
        [' ']

      es1 ->
        case get_content(:expr, es1) do
          [] ->
            [' ']

          [expr] ->
            ['Equivalent to ', expr, '.', '\n']
        end
    end
  end

  defp label_anchor(e) do
    case get_attrval(:label, e) do
      '' ->
        []

      ref ->
        [{:marker, [{:id, ref}], []}, '\n']
    end
  end

  defp label_anchor(content, e) do
    case get_attrval(:label, e) do
      '' ->
        content

      ref ->
        {:p, [{:marker, [{:id, ref}], []}, {:em, content}]}
    end
  end

  defp signature(es, name) do
    [name, '('] ++ seq(&arg/1, es) ++ [') -> term()', '\n']
  end

  defp arg(r_xmlElement(content: es)) do
    [get_text(:argName, es)]
  end

  defp funcheader([]) do
    []
  end

  defp funcheader(es) do
    name = t_name(get_elem(:erlangName, es))

    for e <- get_elem(:type, es) do
      [name] ++ t_utype([e])
    end
  end

  defp local_types([]) do
    []
  end

  defp local_types(es) do
    local_defs2(get_elem(:localdef, es))
  end

  defp local_defs2([]) do
    []
  end

  defp local_defs2(es) do
    case collect_local_types(es) do
      [] ->
        local_defs3(es)

      localTypes ->
        :edoc_local_defs =
          :ets.new(
            :edoc_local_defs,
            [:named_table]
          )

        true = :ets.insert(:edoc_local_defs, localTypes)

        try do
          local_defs3(es)
        after
          :ets.delete(:edoc_local_defs)
        end
    end
  end

  defp local_defs3(es) do
    {:type,
     [
       '\n'
       | for e <- es do
           {:v, localdef2(e)}
         end
     ]}
  end

  defp collect_local_types(es) do
    :lists.append(
      for e <- es do
        collect_local_type(e)
      end
    )
  end

  defp collect_local_type(r_xmlElement(content: es)) do
    case get_elem(:typevar, es) do
      [] ->
        [{t_abstype(get_content(:abstype, es))}]

      [_] ->
        []
    end
  end

  defp localdef2(r_xmlElement(content: es)) do
    var =
      case get_elem(:typevar, es) do
        [] ->
          [t_abstype(get_content(:abstype, es))]

        [v] ->
          t_var(v)
      end

    var ++ [' = '] ++ t_utype(get_elem(:type, es))
  end

  defp type_name(r_xmlElement(content: es)) do
    t_name(get_elem(:erlangName, get_content(:typedef, es)))
  end

  defp types(ts) do
    es =
      :lists.flatmap(
        fn {name, e} ->
          typedecl(name, e)
        end,
        ts
      )

    ['\n', {:taglist, ['\n' | es]}]
  end

  defp typedecl(name, r_xmlElement(content: es)) do
    typedefEs = get_content(:typedef, es)
    id = 'type-' ++ name

    [
      {:tag, [{:marker, [{:id, id}], []}] ++ typedef(typedefEs)},
      '\n',
      {:item,
       local_defs(
         get_elem(
           :localdef,
           typedefEs
         )
       ) ++ fulldesc(es)},
      '\n'
    ]
  end

  defp typedef(es) do
    name =
      [t_name(get_elem(:erlangName, es)), '('] ++
        seq(&t_utype_elem/1, get_content(:argtypes, es), [')'])

    case get_elem(:type, es) do
      [] ->
        name

      type ->
        name ++ [' = '] ++ t_utype(type)
    end
  end

  defp local_defs([]) do
    [{:p, []}]
  end

  defp local_defs(es) do
    [
      '\n',
      {:ul,
       for e <- es do
         {:li, [{:p, localdef(e)}]}
       end}
    ]
  end

  defp localdef(e = r_xmlElement(content: es)) do
    var =
      case get_elem(:typevar, es) do
        [] ->
          [label_anchor(t_abstype(get_content(:abstype, es)), e)]

        [v] ->
          t_var(v)
      end

    var ++ [' = '] ++ t_utype(get_elem(:type, es))
  end

  defp deprecated(es) do
    case get_content(:deprecated, es) do
      [] ->
        []

      deprEs ->
        es2 =
          get_content(
            :fullDescription,
            get_content(:description, deprEs)
          )

        es3 = otp_xmlify_e(es2)
        [{:p, [{:em, ['This function is deprecated: ']} | es3]}, '\n']
    end
  end

  defp fulldesc(es) do
    case get_content(
           :fullDescription,
           get_content(:description, es)
         ) do
      [] ->
        index_desc(es)

      desc ->
        ['\n' | otp_xmlify(desc)] ++ ['\n']
    end
  end

  defp index_desc(es) do
    desc = get_content(:description, es)

    case get_content(:briefDescription, desc) do
      [] ->
        equiv(es)

      shortDesc ->
        shortDesc
    end
  end

  defp seealso_module(es) do
    case get_elem(:see, es) do
      [] ->
        []

      es1 ->
        {:section, [{:title, ['See also']}, {:p, seq(&see/1, es1, [])}]}
    end
  end

  defp seealso_function(es) do
    case get_elem(:see, es) do
      [] ->
        []

      es1 ->
        [{:p, [{:em, ['See also:']}, ' '] ++ seq(&see/1, es1, ['.'])}, '\n']
    end
  end

  defp see(r_xmlElement(content: es0) = e) do
    href0 = get_attrval(:href, e)
    {href, es} = otp_xmlify_a_href(href0, es0)
    [makesee(href, es)]
  end

  defp equiv(es) do
    case get_content(:equiv, es) do
      [] ->
        [' ']

      es1 ->
        case get_content(:expr, es1) do
          [] ->
            []

          [expr] ->
            expr1 = [expr]

            expr2 =
              case get_elem(:see, es1) do
                [] ->
                  {:c, expr1}

                [e = r_xmlElement()] ->
                  case get_attrval(:href, e) do
                    '' ->
                      {:c, expr1}

                    ref0 ->
                      {ref, _Es2} = otp_xmlify_a_href(ref0, [e])
                      makesee(ref, expr1)
                  end
              end

            [{:p, ['Equivalent to ', expr2, '.']}, '\n']
        end
    end
  end

  defp makesee(ref, es) do
    {tag, marker} = makesee(ref)
    {tag, [{:marker, marker}], es}
  end

  def makesee(ref) do
    case :string.split(ref, '#') do
      ['chapter'] ->
        {:seeguide, 'chapter'}

      ['chapter', anchor] ->
        {:seeguide, 'chapter#' ++ anchor}

      [mod, 'type-' ++ anchor] ->
        {:seeerl, mod ++ '#type-' ++ anchor}

      ['', _Anchor] ->
        case :erlang.get(:type) do
          :chapter ->
            {:seeguide, ref}

          :module ->
            case split(ref, '/') do
              [_, _] ->
                {:seemfa, ref}

              _ ->
                {:seeerl, ref}
            end
        end

      _Else ->
        case split(ref, ':') do
          [_, 'index'] ->
            {:seeapp, ref}

          _ ->
            case split(ref, '/') do
              [_, _] ->
                {:seemfa, ref}

              _ ->
                {:seeerl, ref}
            end
        end
    end
  end

  defp authors(es) do
    case get_elem(:author, es) do
      [] ->
        ['\n', {:aname, [' ']}, '\n', {:email, [' ']}]

      es1 ->
        ['\n' | seq(&author/1, es1, '', [])]
    end
  end

  defp author(e = r_xmlElement()) do
    name =
      case get_attrval(:name, e) do
        [] ->
          ' '

        n ->
          n
      end

    mail =
      case get_attrval(:email, e) do
        [] ->
          ' '

        m ->
          m
      end

    ['\n', {:aname, [name]}, '\n', {:email, [mail]}]
  end

  defp t_name([e | _]) do
    n = get_attrval(:name, e)

    case get_attrval(:module, e) do
      '' ->
        n

      m ->
        s = m ++ ':' ++ n

        case get_attrval(:app, e) do
          '' ->
            s

          a ->
            '//' ++ a ++ '/' ++ s
        end
    end
  end

  defp t_utype([e]) do
    flatten_type(t_utype_elem(e))
  end

  defp flatten_type(t) do
    for e <- :lists.flatten(t) do
      case is_integer(e) do
        true ->
          [e]

        false ->
          e
      end
    end
  end

  defp t_utype_elem(e = r_xmlElement(content: es)) do
    case get_attrval(:name, e) do
      '' ->
        t_type(es)

      name ->
        t = t_type(es)

        case t do
          [^name] ->
            t

          ^t ->
            [name] ++ ['::'] ++ t
        end
    end
  end

  defp t_type([e = r_xmlElement(name: :typevar)]) do
    t_var(e)
  end

  defp t_type([e = r_xmlElement(name: :atom)]) do
    t_atom(e)
  end

  defp t_type([e = r_xmlElement(name: :integer)]) do
    t_integer(e)
  end

  defp t_type([e = r_xmlElement(name: :range)]) do
    t_range(e)
  end

  defp t_type([e = r_xmlElement(name: :float)]) do
    t_float(e)
  end

  defp t_type([r_xmlElement(name: nil)]) do
    t_nil()
  end

  defp t_type([r_xmlElement(name: :list, content: es)]) do
    t_list(es)
  end

  defp t_type([r_xmlElement(name: :nonempty_list, content: es)]) do
    t_nonempty_list(es)
  end

  defp t_type([r_xmlElement(name: :tuple, content: es)]) do
    t_tuple(es)
  end

  defp t_type([r_xmlElement(name: :fun, content: es)]) do
    t_fun(es)
  end

  defp t_type([e = r_xmlElement(name: :abstype, content: es)]) do
    t_abstype(e, es)
  end

  defp t_type([r_xmlElement(name: :union, content: es)]) do
    t_union(es)
  end

  defp t_type([r_xmlElement(name: :record, content: es)]) do
    t_record(es)
  end

  defp t_type([r_xmlElement(name: :map, content: es)]) do
    t_map(es)
  end

  defp t_var(e) do
    [get_attrval(:name, e)]
  end

  defp t_atom(e) do
    [get_attrval(:value, e)]
  end

  defp t_integer(e) do
    [get_attrval(:value, e)]
  end

  defp t_range(e) do
    [get_attrval(:value, e)]
  end

  defp t_float(e) do
    [get_attrval(:value, e)]
  end

  defp t_nil() do
    ['[]']
  end

  defp t_list(es) do
    ['['] ++ t_utype(get_elem(:type, es)) ++ [']']
  end

  defp t_nonempty_list(es) do
    ['['] ++ t_utype(get_elem(:type, es)) ++ [', ...]']
  end

  defp t_tuple(es) do
    ['{'] ++ seq(&t_utype_elem/1, es, ['}'])
  end

  defp t_fun(es) do
    ['('] ++
      seq(&t_utype_elem/1, get_content(:argtypes, es), [') -> '] ++ t_utype(get_elem(:type, es)))
  end

  defp t_record([e | es]) do
    [
      '#',
      get_attrval(:value, e),
      '{' ++
        seq(
          &t_field/1,
          es
        ) ++ '}'
    ]
  end

  defp t_field(r_xmlElement(name: :field, content: [atom, type])) do
    [get_attrval(:value, atom), '='] ++ t_utype_elem(type)
  end

  defp t_map(es) do
    ['\#{'] ++ seq(&t_map_field/1, es, ['}'])
  end

  defp t_map_field(e = r_xmlElement(name: :map_field, content: [k, v])) do
    kElem = t_utype_elem(k)
    vElem = t_utype_elem(v)

    aS =
      case get_attrval(:assoc_type, e) do
        'assoc' ->
          ' => '

        'exact' ->
          ' := '
      end

    [kElem ++ aS ++ vElem]
  end

  defp t_abstype(e, es) do
    see_type(e, t_abstype(es))
  end

  defp t_abstype(es) do
    name = t_name(get_elem(:erlangName, es))
    [name, '('] ++ seq(&t_utype_elem/1, get_elem(:type, es), [')'])
  end

  defp see_type(e, es0) do
    case get_attrval(:href, e) do
      [] ->
        es0

      href0 ->
        try do
          false = is_local_type(es0)
          text = r_xmlText(value: :lists.append(es0))
          {href, es} = otp_xmlify_a_href(href0, [text])
          [makesee(href, es)]
        catch
          _, _ ->
            es0
        end
    end
  end

  defp is_local_type(es) do
    try do
      [_] = :ets.lookup(:edoc_local_defs, es)
      true
    catch
      _, _ ->
        false
    end
  end

  defp t_union(es) do
    seq(&t_utype_elem/1, es, ' | ', [])
  end

  defp seq(fun, es) do
    seq(fun, es, [])
  end

  defp seq(fun, es, tail) do
    seq(fun, es, ', ', tail)
  end

  defp seq(fun, [e], _Sep, tail) do
    fun.(e) ++ tail
  end

  defp seq(fun, [e | es], sep, tail) do
    fun.(e) ++ [sep] ++ seq(fun, es, sep, tail)
  end

  defp seq(_Fun, [], _Sep, tail) do
    tail
  end

  defp parent(e) do
    parents = r_xmlElement(e, :parents)
    {parent, _} = :lists.last(parents)
    parent
  end

  defp get_elem(name, [r_xmlElement(name: name) = e | es]) do
    [e | get_elem(name, es)]
  end

  defp get_elem(name, [_ | es]) do
    get_elem(name, es)
  end

  defp get_elem(_, []) do
    []
  end

  defp get_attr(name, [r_xmlAttribute(name: name) = a | as]) do
    [a | get_attr(name, as)]
  end

  defp get_attr(name, [_ | as]) do
    get_attr(name, as)
  end

  defp get_attr(_, []) do
    []
  end

  defp get_attrval(name, r_xmlElement(attributes: as)) do
    case get_attr(name, as) do
      [r_xmlAttribute(value: v)] ->
        v

      [] ->
        ''
    end
  end

  defp get_content(name, es) do
    case get_elem(name, es) do
      [r_xmlElement(content: es1)] ->
        es1

      [] ->
        []

      elems ->
        :lists.append(
          for r_xmlElement(content: es1) <- elems do
            es1
          end
        )
    end
  end

  defp get_text(name, es) do
    case get_content(name, es) do
      [r_xmlText(value: text)] ->
        text

      [] ->
        ''
    end
  end

  defp get_text(r_xmlElement(content: [r_xmlText(value: text)])) do
    text
  end

  defp get_text(r_xmlElement(content: [e])) do
    get_text(e)
  end

  defp text_and_a_name_only(es) do
    case (for r_xmlElement(
                name: :a,
                attributes: [r_xmlAttribute(name: :name)]
              ) = name <- es do
            name
          end) do
      [name | _] ->
        {r_xmlElement(name, content: []), text_only(es)}

      [] ->
        {'', text_only(es)}
    end
  end

  defp text_only([r_xmlElement(content: content) | es]) do
    text_only(content) ++ text_only(es)
  end

  defp text_only([r_xmlText() = e | es]) do
    [e | text_only(es)]
  end

  defp text_only([]) do
    []
  end
end
