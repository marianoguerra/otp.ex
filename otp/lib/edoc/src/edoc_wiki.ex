defmodule :m_edoc_wiki do
  use Bitwise
  require Record
  Record.defrecord(:r_context, :context, dir: '', env: :undefined, opts: [])
  Record.defrecord(:r_doclet_gen, :doclet_gen, sources: [], app: [], modules: [])

  Record.defrecord(:r_doclet_toc, :doclet_toc,
    paths: :undefined,
    indir: :undefined
  )

  Record.defrecord(:r_module, :module,
    name: [],
    parameters: :none,
    functions: [],
    exports: [],
    attributes: [],
    records: [],
    encoding: :latin1
  )

  Record.defrecord(:r_env, :env,
    module: [],
    root: '',
    file_suffix: :undefined,
    apps: :undefined,
    modules: :undefined,
    app_default: :undefined,
    macros: [],
    includes: []
  )

  Record.defrecord(:r_comment, :comment,
    line: 0,
    text: :undefined
  )

  Record.defrecord(:r_entry, :entry,
    name: :undefined,
    args: [],
    line: 0,
    export: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_tag, :tag, name: :undefined, line: 0, origin: :comment, data: :undefined)

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

  def parse_xml(data, line) do
    par(parse_xml_1(expand_text(data, line), line))
  end

  defp parse_xml_1(text, line) do
    text1 = '<doc>' ++ text ++ '</doc>'
    opts = [{:line, line}, {:encoding, :"iso-8859-1"}]

    case (try do
            {:ok, :xmerl_scan.string(text1, opts)}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, {e, _}} ->
        r_xmlElement(e, :content)

      {:EXIT, {:fatal, {reason, l, _C}}} ->
        throw_error(l, {'XML parse error: ~p.', [reason]})

      {:EXIT, reason} ->
        throw_error(line, {'error in XML parser: ~P.', [reason, 10]})

      other ->
        throw_error(line, {'nocatch in XML parser: ~P.', [other, 10]})
    end
  end

  def expand_text(cs, l) do
    :lists.reverse(expand_new_line(cs, l, []))
  end

  defp expand_new_line([?\s = c | cs], l, as) do
    expand_new_line(cs, l, [c | as])
  end

  defp expand_new_line([?\t = c | cs], l, as) do
    expand_new_line(cs, l, [c | as])
  end

  defp expand_new_line([?\n = c | cs], l, as) do
    expand_new_line(cs, l + 1, [c | as])
  end

  defp expand_new_line([[?=, ?=, ?=, ?=] | cs], l, as) do
    expand_heading(cs, 2, l, as)
  end

  defp expand_new_line([[?=, ?=, ?=] | cs], l, as) do
    expand_heading(cs, 1, l, as)
  end

  defp expand_new_line([[?=, ?=] | cs], l, as) do
    expand_heading(cs, 0, l, as)
  end

  defp expand_new_line(cs, l, as) do
    expand(cs, l, as)
  end

  defp expand([[?`, ?'] | cs], l, as) do
    expand(cs, l, [?` | as])
  end

  defp expand([[?`, ?`, ?`] | cs], l, as) do
    {cs1, skipped} = strip_empty_lines(cs)

    n =
      cond do
        skipped > 0 ->
          0

        true ->
          {as1, _} = :edoc_lib.split_at(as, ?\n)

          case :edoc_lib.is_space(as1) do
            true ->
              3 + length(as1)

            false ->
              2
          end
      end

    ss = :lists.duplicate(n, ?\s)
    expand_triple(cs1, l + skipped, ss ++ '[ATADC[!<>erp<' ++ as)
  end

  defp expand([[?`, ?`] | cs], l, as) do
    expand_double(:edoc_lib.strip_space(cs), l, '>edoc<' ++ as)
  end

  defp expand([?` | cs], l, as) do
    expand_single(:edoc_lib.strip_space(cs), l, '>edoc<' ++ as)
  end

  defp expand([?[ | cs], l, as) do
    expand_uri(cs, l, as)
  end

  defp expand([?\n = c | cs], l, as) do
    expand_new_line(cs, l + 1, [c | as])
  end

  defp expand([c | cs], l, as) do
    expand(cs, l, [c | as])
  end

  defp expand([], _, as) do
    as
  end

  defp expand_heading([?= | _] = cs, n, l, as) do
    expand_heading_1(cs, n, l, as)
  end

  defp expand_heading(cs, n, l, as) do
    {cs1, cs2} = :edoc_lib.split_at(cs, ?\n)

    case :edoc_lib.strip_space(:lists.reverse(cs1)) do
      [[?=, ?=] | cs3] ->
        {es, ts} =
          :lists.splitwith(
            fn x ->
              x === ?=
            end,
            cs3
          )

        cond do
          length(es) === n ->
            ts1 = :edoc_lib.strip_space(:lists.reverse(:edoc_lib.strip_space(ts)))
            expand_heading_2(ts1, cs2, n, l, as)

          true ->
            h1 = :lists.duplicate(n + 2, ?=)
            h2 = '==' ++ es
            throw_error(l, {'heading end marker mismatch: ~s...~s', [h1, h2]})
        end

      _ ->
        expand_heading_1(cs, n, l, as)
    end
  end

  defp expand_heading_1(cs, n, l, as) do
    expand(cs, l, :lists.duplicate(n + 2, ?=) ++ as)
  end

  defp expand_heading_2(ts, cs, n, l, as) do
    h = 3 + n
    ts1 = :io_lib.format('<h~w><a name="~ts">~ts</a></h~w>\n', [h, make_label(ts), ts, h])
    expand_new_line(cs, l + 1, :lists.reverse(:lists.flatten(ts1), as))
  end

  defp make_label([?\s | cs]) do
    [?_ | make_label(:edoc_lib.strip_space(cs))]
  end

  defp make_label([?\t | cs]) do
    [?_ | make_label(:edoc_lib.strip_space(cs))]
  end

  defp make_label([?\n | cs]) do
    [?_ | make_label(:edoc_lib.strip_space(cs))]
  end

  defp make_label([c | cs]) do
    [c | make_label(cs)]
  end

  defp make_label([]) do
    []
  end

  defp expand_single(cs, l, as) do
    expand_single(cs, l, as, l)
  end

  defp expand_single([?' | cs], l, as, _L0) do
    expand(cs, l, '>edoc/<' ++ :edoc_lib.strip_space(as))
  end

  defp expand_single([?< | cs], l, as, l0) do
    expand_single(cs, l, ';tl&' ++ as, l0)
  end

  defp expand_single([?> | cs], l, as, l0) do
    expand_single(cs, l, ';tg&' ++ as, l0)
  end

  defp expand_single([?& | cs], l, as, l0) do
    expand_single(cs, l, ';pma&' ++ as, l0)
  end

  defp expand_single([?\n = c | cs], l, as, l0) do
    expand_single(cs, l + 1, [c | as], l0)
  end

  defp expand_single([c | cs], l, as, l0) do
    expand_single(cs, l, [c | as], l0)
  end

  defp expand_single([], l, _, l0) do
    throw_error(l0, {'`-quote ended unexpectedly at line ~w', [l]})
  end

  defp expand_double(cs, l, as) do
    expand_double(cs, l, as, l)
  end

  defp expand_double([[?', ?'] | cs], l, as, _L0) do
    expand(cs, l, '>edoc/<' ++ :edoc_lib.strip_space(as))
  end

  defp expand_double([?< | cs], l, as, l0) do
    expand_double(cs, l, ';tl&' ++ as, l0)
  end

  defp expand_double([?> | cs], l, as, l0) do
    expand_double(cs, l, ';tg&' ++ as, l0)
  end

  defp expand_double([?& | cs], l, as, l0) do
    expand_double(cs, l, ';pma&' ++ as, l0)
  end

  defp expand_double([?\n = c | cs], l, as, l0) do
    expand_double(cs, l + 1, [c | as], l0)
  end

  defp expand_double([c | cs], l, as, l0) do
    expand_double(cs, l, [c | as], l0)
  end

  defp expand_double([], l, _, l0) do
    throw_error(l0, {'``-quote ended unexpectedly at line ~w', [l]})
  end

  defp expand_triple(cs, l, as) do
    expand_triple(cs, l, as, l)
  end

  defp expand_triple([[?', ?', ?'] | cs], l, as, _L0) do
    expand(cs, l, '>erp/<>]]' ++ :edoc_lib.strip_space(as))
  end

  defp expand_triple([[?], ?], ?>] | cs], l, as, l0) do
    expand_triple(cs, l, ';tg&]]' ++ as, l0)
  end

  defp expand_triple([?\n = c | cs], l, as, l0) do
    expand_triple(cs, l + 1, [c | as], l0)
  end

  defp expand_triple([c | cs], l, as, l0) do
    expand_triple(cs, l, [c | as], l0)
  end

  defp expand_triple([], l, _, l0) do
    throw_error(l0, {'```-quote ended unexpectedly at line ~w', [l]})
  end

  defp expand_uri('http:/' ++ cs, l, as) do
    expand_uri(cs, l, '/:ptth', as)
  end

  defp expand_uri('https:/' ++ cs, l, as) do
    expand_uri(cs, l, '/:sptth', as)
  end

  defp expand_uri('ftp:/' ++ cs, l, as) do
    expand_uri(cs, l, '/:ptf', as)
  end

  defp expand_uri('file:/' ++ cs, l, as) do
    expand_uri(cs, l, '/:elif', as)
  end

  defp expand_uri('mailto:/' ++ cs, l, as) do
    expand_uri(cs, l, '/:otliam', as)
  end

  defp expand_uri('nfs:/' ++ cs, l, as) do
    expand_uri(cs, l, '/:sfn', as)
  end

  defp expand_uri('shttp:/' ++ cs, l, as) do
    expand_uri(cs, l, '/:ptths', as)
  end

  defp expand_uri('xmpp:/' ++ cs, l, as) do
    expand_uri(cs, l, '/:ppmx', as)
  end

  defp expand_uri(cs, l, as) do
    expand(cs, l, [?[ | as])
  end

  defp expand_uri([?] | cs], l, us, as) do
    expand(cs, l, push_uri(us, '>tt/<' ++ us ++ '>tt<', as))
  end

  defp expand_uri([?\s = c | cs], l, us, as) do
    expand_uri(cs, 0, l, [c], us, as)
  end

  defp expand_uri([?\t = c | cs], l, us, as) do
    expand_uri(cs, 0, l, [c], us, as)
  end

  defp expand_uri([?\n = c | cs], l, us, as) do
    expand_uri(cs, 1, l, [c], us, as)
  end

  defp expand_uri([c | cs], l, us, as) do
    expand_uri(cs, l, [c | us], as)
  end

  defp expand_uri([], l, us, _As) do
    expand_uri_error(us, l)
  end

  defp expand_uri([?] | cs], n, l, ss, us, as) do
    ss1 = :lists.reverse(:edoc_lib.strip_space(:lists.reverse(:edoc_lib.strip_space(ss))))
    expand(cs, l + n, push_uri(us, ss1, as))
  end

  defp expand_uri([?\n = c | cs], n, l, ss, us, as) do
    expand_uri(cs, n + 1, l, [c | ss], us, as)
  end

  defp expand_uri([c | cs], n, l, ss, us, as) do
    expand_uri(cs, n, l, [c | ss], us, as)
  end

  defp expand_uri([], _, l, _Ss, us, _As) do
    expand_uri_error(us, l)
  end

  defp expand_uri_error(us, l) do
    {ps, _} = :edoc_lib.split_at(:lists.reverse(us), ?:)
    throw_error(l, {'reference \'[~ts:...\' ended unexpectedly', [ps]})
  end

  defp push_uri(us, ss, as) do
    '>a/<' ++ ss ++ '>"pot_"=tegrat "' ++ us ++ '"=ferh a<' ++ as
  end

  defp strip_empty_lines(cs) do
    strip_empty_lines(cs, 0)
  end

  defp strip_empty_lines([], n) do
    {[], n}
  end

  defp strip_empty_lines(cs, n) do
    {cs1, cs2} = :edoc_lib.split_at(cs, ?\n)

    case :edoc_lib.is_space(cs1) do
      true ->
        strip_empty_lines(cs2, n + 1)

      false ->
        {cs, n}
    end
  end

  defp par(es) do
    par(es, [], [])
  end

  defp par([e = r_xmlText(value: value) | es], as, bs) do
    par_text(value, as, bs, e, es)
  end

  defp par([e = r_xmlElement(name: name) | es], as, bs) do
    case name do
      :p ->
        par_flush(es, [e | as], bs)

      :hr ->
        par_flush(es, [e | as], bs)

      :h1 ->
        par_flush(es, [e | as], bs)

      :h2 ->
        par_flush(es, [e | as], bs)

      :h3 ->
        par_flush(es, [e | as], bs)

      :h4 ->
        par_flush(es, [e | as], bs)

      :h5 ->
        par_flush(es, [e | as], bs)

      :h6 ->
        par_flush(es, [e | as], bs)

      :pre ->
        par_flush(es, [e | as], bs)

      :address ->
        par_flush(es, [e | as], bs)

      :div ->
        par_flush(es, [par_elem(e) | as], bs)

      :blockquote ->
        par_flush(es, [par_elem(e) | as], bs)

      :form ->
        par_flush(es, [par_elem(e) | as], bs)

      :fieldset ->
        par_flush(es, [par_elem(e) | as], bs)

      :noscript ->
        par_flush(es, [par_elem(e) | as], bs)

      :ul ->
        par_flush(es, [par_subelem(e) | as], bs)

      :ol ->
        par_flush(es, [par_subelem(e) | as], bs)

      :dl ->
        par_flush(es, [par_subelem(e) | as], bs)

      :table ->
        par_flush(es, [par_subelem(e) | as], bs)

      _ ->
        par(es, [e | as], bs)
    end
  end

  defp par([e | es], as, bs) do
    par(es, [e | as], bs)
  end

  defp par([], as, bs) do
    :lists.reverse(as ++ bs)
  end

  defp par_text(cs, as, bs, e, es) do
    case ptxt(cs) do
      :none ->
        par(es, [e | as], bs)

      {cs1, ss, cs2} ->
        es1 =
          case cs1 do
            [] ->
              :lists.reverse(as)

            _ ->
              :lists.reverse(as, [r_xmlText(e, value: cs1)])
          end

        bs0 =
          case es1 do
            [] ->
              bs

            _ ->
              [r_xmlElement(name: :p, content: es1) | bs]
          end

        bs1 = [r_xmlText(value: ss) | bs0]

        case cs2 do
          [] ->
            par(es, [], bs1)

          _ ->
            par_text(cs2, [], bs1, r_xmlText(value: cs2), es)
        end
    end
  end

  defp par_flush(es, as, bs) do
    par(es, [], as ++ bs)
  end

  defp par_elem(e) do
    r_xmlElement(e, content: par(r_xmlElement(e, :content)))
  end

  defp par_subelem(e) do
    r_xmlElement(e, content: par_subelem_1(r_xmlElement(e, :content)))
  end

  defp par_subelem_1([e = r_xmlElement(name: name) | es]) do
    e1 =
      case par_skip(name) do
        true ->
          e

        false ->
          case par_sub(name) do
            true ->
              par_subelem(e)

            false ->
              par_elem(e)
          end
      end

    [e1 | par_subelem_1(es)]
  end

  defp par_subelem_1([e | es]) do
    [e | par_subelem_1(es)]
  end

  defp par_subelem_1([]) do
    []
  end

  defp par_skip(:caption) do
    true
  end

  defp par_skip(:col) do
    true
  end

  defp par_skip(:colgroup) do
    true
  end

  defp par_skip(_) do
    false
  end

  defp par_sub(:tr) do
    true
  end

  defp par_sub(:thead) do
    true
  end

  defp par_sub(:tfoot) do
    true
  end

  defp par_sub(:tbody) do
    true
  end

  defp par_sub(_) do
    false
  end

  defp ptxt(cs) do
    ptxt(cs, [])
  end

  defp ptxt([?\n | cs], as) do
    ptxt_1(cs, as, [?\n])
  end

  defp ptxt([c | cs], as) do
    ptxt(cs, [c | as])
  end

  defp ptxt([], _As) do
    :none
  end

  defp ptxt_1([c = ?\s | cs], as, ss) do
    ptxt_1(cs, as, [c | ss])
  end

  defp ptxt_1([c = ?\t | cs], as, ss) do
    ptxt_1(cs, as, [c | ss])
  end

  defp ptxt_1([c = ?\n | cs], as, ss) do
    ptxt_2(cs, as, [c | ss])
  end

  defp ptxt_1(cs, as, ss) do
    ptxt(cs, :lists.reverse(ss, as))
  end

  defp ptxt_2([c = ?\s | cs], as, ss) do
    ptxt_2(cs, as, [c | ss])
  end

  defp ptxt_2([c = ?\t | cs], as, ss) do
    ptxt_2(cs, as, [c | ss])
  end

  defp ptxt_2([c = ?\n | cs], as, ss) do
    ptxt_2(cs, as, [c | ss])
  end

  defp ptxt_2(cs, as, ss) do
    case :edoc_lib.is_space(as) do
      true ->
        {[], :lists.reverse(ss ++ as), cs}

      false ->
        {:lists.reverse(as), :lists.reverse(ss), cs}
    end
  end

  defp throw_error(l, d) do
    throw({:error, l, d})
  end
end
