defmodule :m_edoc_lib do
  use Bitwise
  import :edoc_report, only: [report: 2, warning: 2]
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

  def timestr({h, m, sec}) do
    :lists.flatten(:io_lib.fwrite('~2.2.0w:~2.2.0w:~2.2.0w', [h, m, sec]))
  end

  def datestr({y, m, d}) do
    ms = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']

    :lists.flatten(
      :io_lib.fwrite(
        '~s ~w ~w',
        [:lists.nth(m, ms), d, y]
      )
    )
  end

  def read_encoding(file, options) do
    case :epp.read_encoding(file, options) do
      :none ->
        :epp.default_encoding()

      encoding ->
        encoding
    end
  end

  def count(x, xs) do
    count(x, xs, 0)
  end

  defp count(x, [x | xs], n) do
    count(x, xs, n + 1)
  end

  defp count(x, [_ | xs], n) do
    count(x, xs, n)
  end

  defp count(_X, [], n) do
    n
  end

  def lines(cs) do
    lines(cs, [], [])
  end

  defp lines([?\n | cs], as, ls) do
    lines(cs, [], [:lists.reverse(as) | ls])
  end

  defp lines([c | cs], as, ls) do
    lines(cs, [c | as], ls)
  end

  defp lines([], as, ls) do
    :lists.reverse([:lists.reverse(as) | ls])
  end

  def split_at(cs, k) do
    split_at(cs, k, [])
  end

  defp split_at([k | cs], k, as) do
    {:lists.reverse(as), cs}
  end

  defp split_at([c | cs], k, as) do
    split_at(cs, k, [c | as])
  end

  defp split_at([], _K, as) do
    {:lists.reverse(as), []}
  end

  def split_at_stop(cs) do
    split_at_stop(cs, [])
  end

  defp split_at_stop([?., ?\s | cs], as) do
    {:lists.reverse(as), cs}
  end

  defp split_at_stop([?., ?\t | cs], as) do
    {:lists.reverse(as), cs}
  end

  defp split_at_stop([?., ?\n | cs], as) do
    {:lists.reverse(as), cs}
  end

  defp split_at_stop([?.], as) do
    {:lists.reverse(as), []}
  end

  defp split_at_stop([c | cs], as) do
    split_at_stop(cs, [c | as])
  end

  defp split_at_stop([], as) do
    {:lists.reverse(as), []}
  end

  def split_at_space(cs) do
    split_at_space(cs, [])
  end

  defp split_at_space([?\s | cs], as) do
    {:lists.reverse(as), cs}
  end

  defp split_at_space([?\t | cs], as) do
    {:lists.reverse(as), cs}
  end

  defp split_at_space([?\n | cs], as) do
    {:lists.reverse(as), cs}
  end

  defp split_at_space([c | cs], as) do
    split_at_space(cs, [c | as])
  end

  defp split_at_space([], as) do
    {:lists.reverse(as), []}
  end

  def is_space([?\s | cs]) do
    is_space(cs)
  end

  def is_space([?\t | cs]) do
    is_space(cs)
  end

  def is_space([?\n | cs]) do
    is_space(cs)
  end

  def is_space([_C | _Cs]) do
    false
  end

  def is_space([]) do
    true
  end

  def strip_space([?\s | cs]) do
    strip_space(cs)
  end

  def strip_space([?\t | cs]) do
    strip_space(cs)
  end

  def strip_space([?\n | cs]) do
    strip_space(cs)
  end

  def strip_space(cs) do
    cs
  end

  def segment(es, n) do
    segment(es, [], [], 0, n)
  end

  defp segment([e | es], as, cs, n, m) when n < m do
    segment(es, [e | as], cs, n + 1, m)
  end

  defp segment([_ | _] = es, as, cs, _N, m) do
    segment(es, [], [:lists.reverse(as) | cs], 0, m)
  end

  defp segment([], [], cs, _N, _M) do
    :lists.reverse(cs)
  end

  defp segment([], as, cs, _N, _M) do
    :lists.reverse([:lists.reverse(as) | cs])
  end

  def transpose([]) do
    []
  end

  def transpose([[] | xss]) do
    transpose(xss)
  end

  def transpose([[x | xs] | xss]) do
    [
      [
        x
        | for [h | _T] <- xss do
            h
          end
      ]
      | transpose([
          xs
          | for [_H | t] <- xss do
              t
            end
        ])
    ]
  end

  def get_first_sentence([r_xmlElement(name: :p, content: es) | _]) do
    get_first_sentence_1(es)
  end

  def get_first_sentence(es) do
    get_first_sentence_1(es)
  end

  defp get_first_sentence_1([e = r_xmlText(value: txt) | es]) do
    last =
      case es do
        [r_xmlElement(name: :p) | _] ->
          true

        [r_xmlElement(name: :br) | _] ->
          true

        [] ->
          true

        _ ->
          false
      end

    case end_of_sentence(txt, last) do
      {:value, txt1} ->
        [r_xmlText(e, value: txt1)]

      :none ->
        [e | get_first_sentence_1(es)]
    end
  end

  defp get_first_sentence_1([e | es]) do
    [e | get_first_sentence_1(es)]
  end

  defp get_first_sentence_1([]) do
    []
  end

  defp end_of_sentence(cs, last) do
    end_of_sentence(cs, last, [])
  end

  defp end_of_sentence([c = ?., ?\s | _], _, as) do
    end_of_sentence_1(c, true, as)
  end

  defp end_of_sentence([c = ?., ?\t | _], _, as) do
    end_of_sentence_1(c, true, as)
  end

  defp end_of_sentence([c = ?., ?\n | _], _, as) do
    end_of_sentence_1(c, true, as)
  end

  defp end_of_sentence([c = ?.], last, as) do
    end_of_sentence_1(c, last, as)
  end

  defp end_of_sentence([c = ?!, ?\s | _], _, as) do
    end_of_sentence_1(c, true, as)
  end

  defp end_of_sentence([c = ?!, ?\t | _], _, as) do
    end_of_sentence_1(c, true, as)
  end

  defp end_of_sentence([c = ?!, ?\n | _], _, as) do
    end_of_sentence_1(c, true, as)
  end

  defp end_of_sentence([c = ?!], last, as) do
    end_of_sentence_1(c, last, as)
  end

  defp end_of_sentence([c | cs], last, as) do
    end_of_sentence(cs, last, [c | as])
  end

  defp end_of_sentence([], last, as) do
    end_of_sentence_1(?., last, strip_space(as))
  end

  defp end_of_sentence_1(c, true, as) do
    {:value, :lists.reverse([c | as])}
  end

  defp end_of_sentence_1(_, false, _) do
    :none
  end

  def is_name([c | cs]) when c >= ?a and c <= ?z do
    is_name_1(cs)
  end

  def is_name([c | cs])
      when c >= 223 and c <= 255 and
             c !== 247 do
    is_name_1(cs)
  end

  def is_name(_) do
    false
  end

  defp is_name_1([c | cs]) when c >= ?a and c <= ?z do
    is_name_1(cs)
  end

  defp is_name_1([c | cs]) when c >= ?A and c <= ?Z do
    is_name_1(cs)
  end

  defp is_name_1([c | cs]) when c >= ?0 and c <= ?9 do
    is_name_1(cs)
  end

  defp is_name_1([c | cs])
       when c >= 192 and c <= 255 and
              c !== 215 and c !== 247 do
    is_name_1(cs)
  end

  defp is_name_1([?_ | cs]) do
    is_name_1(cs)
  end

  defp is_name_1([]) do
    true
  end

  defp is_name_1(_) do
    false
  end

  def unique([x | xs]) do
    [x | unique(xs, x)]
  end

  def unique([]) do
    []
  end

  defp unique([x | xs], x) do
    unique(xs, x)
  end

  defp unique([x | xs], _) do
    [x | unique(xs, x)]
  end

  defp unique([], _) do
    []
  end

  def parse_expr(s, l) do
    case :erl_scan.string(s ++ '.', l) do
      {:ok, ts, _} ->
        case :erl_parse.parse_exprs(ts) do
          {:ok, [expr]} ->
            expr

          {:error, {999_999, :erl_parse, _}} ->
            throw_error(:eof, l)

          {:error, e} ->
            throw_error(e, l)
        end

      {:error, e, _} ->
        throw_error(e, l)
    end
  end

  Record.defrecord(:r_info, :info, name: '', email: '', uri: '')

  def parse_contact(s, l) do
    i = scan_name(s, l, r_info(), [])
    {r_info(i, :name), r_info(i, :email), r_info(i, :uri)}
  end

  defp scan_name([?< | cs], l, i, as) do
    case r_info(i, :email) do
      '' ->
        {cs1, i1} = scan_email(cs, l, set_name(i, as), [])
        scan_name(cs1, l, i1, [])

      _ ->
        throw_error('multiple \'<...>\' sections.', l)
    end
  end

  defp scan_name([?[ | cs], l, i, as) do
    case r_info(i, :uri) do
      '' ->
        {cs1, i1} = scan_uri(cs, l, set_name(i, as), [])
        scan_name(cs1, l, i1, [])

      _ ->
        throw_error('multiple \'[...]\' sections.', l)
    end
  end

  defp scan_name([?\n | cs], l, i, as) do
    scan_name(cs, l + 1, i, [?\n | as])
  end

  defp scan_name([c | cs], l, i, as) do
    scan_name(cs, l, i, [c | as])
  end

  defp scan_name([], _L, i, as) do
    set_name(i, as)
  end

  defp scan_uri([?] | cs], _L, i, as) do
    {cs, r_info(i, uri: strip_and_reverse(as))}
  end

  defp scan_uri([?\n | cs], l, i, as) do
    scan_uri(cs, l + 1, i, [?\n | as])
  end

  defp scan_uri([c | cs], l, i, as) do
    scan_uri(cs, l, i, [c | as])
  end

  defp scan_uri([], l, _I, _As) do
    throw_error({:missing, ?]}, l)
  end

  defp scan_email([?> | cs], _L, i, as) do
    {cs, r_info(i, email: strip_and_reverse(as))}
  end

  defp scan_email([?\n | cs], l, i, as) do
    scan_email(cs, l + 1, i, [?\n | as])
  end

  defp scan_email([c | cs], l, i, as) do
    scan_email(cs, l, i, [c | as])
  end

  defp scan_email([], l, _I, _As) do
    throw_error({:missing, ?>}, l)
  end

  defp set_name(i, as) do
    case r_info(i, :name) do
      '' ->
        r_info(i, name: strip_and_reverse(as))

      _ ->
        i
    end
  end

  defp strip_and_reverse(as) do
    :edoc_lib.strip_space(:lists.reverse(:edoc_lib.strip_space(as)))
  end

  def escape_uri([c | cs]) when c >= ?a and c <= ?z do
    [c | escape_uri(cs)]
  end

  def escape_uri([c | cs]) when c >= ?A and c <= ?Z do
    [c | escape_uri(cs)]
  end

  def escape_uri([c | cs]) when c >= ?0 and c <= ?9 do
    [c | escape_uri(cs)]
  end

  def escape_uri([c = ?. | cs]) do
    [c | escape_uri(cs)]
  end

  def escape_uri([c = ?- | cs]) do
    [c | escape_uri(cs)]
  end

  def escape_uri([c = ?_ | cs]) do
    [c | escape_uri(cs)]
  end

  def escape_uri([c | cs]) when c > 127 do
    escape_byte(c &&& 192 >>> (6 + 192)) ++ escape_byte(c &&& 63 + 128) ++ escape_uri(cs)
  end

  def escape_uri([c | cs]) do
    escape_byte(c) ++ escape_uri(cs)
  end

  def escape_uri([]) do
    []
  end

  defp escape_byte(c) when c >= 0 and c <= 255 do
    [?%, hex_digit(c >>> 4), hex_digit(c &&& 15)]
  end

  defp hex_digit(n) when n >= 0 and n <= 9 do
    n + ?0
  end

  defp hex_digit(n) when n > 9 and n <= 15 do
    n + ?a - 10
  end

  def join_uri(base, '') do
    base
  end

  def join_uri('', path) do
    path
  end

  def join_uri(base, path) do
    base ++ '/' ++ path
  end

  def to_label([?\s | cs]) do
    to_label(cs)
  end

  def to_label([?\t | cs]) do
    to_label(cs)
  end

  def to_label([?\n | cs]) do
    to_label(cs)
  end

  def to_label([]) do
    []
  end

  def to_label(cs) do
    to_label_1(cs)
  end

  defp to_label_1([?\s | cs]) do
    to_label_2([?\s | cs])
  end

  defp to_label_1([?\t | cs]) do
    to_label_2([?\s | cs])
  end

  defp to_label_1([?\n | cs]) do
    to_label_2([?\s | cs])
  end

  defp to_label_1([c | cs]) do
    [c | to_label_1(cs)]
  end

  defp to_label_1([]) do
    []
  end

  defp to_label_2(cs) do
    case to_label(cs) do
      [] ->
        []

      cs1 ->
        [?_ | cs1]
    end
  end

  def filename([c | t]) when is_integer(c) and c > 0 do
    [c | filename(t)]
  end

  def filename([h | t]) do
    filename(h) ++ filename(t)
  end

  def filename([]) do
    []
  end

  def filename(n) when is_atom(n) do
    :erlang.atom_to_list(n)
  end

  def filename(n) do
    report('bad filename: `~tP\'.', [n, 25])
    exit(:error)
  end

  def copy_file(from, to) do
    case :file.copy(from, to) do
      {:ok, _} ->
        :ok

      {:error, r} ->
        r1 = :file.format_error(r)
        report('error copying \'~ts\' to \'~ts\': ~ts.', [from, to, r1])
        exit(:error)
    end
  end

  defp list_dir(dir, error) do
    case :file.list_dir(dir) do
      {:ok, fs} ->
        fs

      {:error, r} ->
        f =
          case error do
            false ->
              fn s, as ->
                warning(s, as)
                []
              end
          end

        r1 = :file.format_error(r)
        f.('could not read directory \'~ts\': ~ts.', [filename(dir), r1])
    end
  end

  def simplify_path(p) do
    case :filename.basename(p) do
      '.' ->
        simplify_path(:filename.dirname(p))

      '..' ->
        simplify_path(:filename.dirname(:filename.dirname(p)))

      _ ->
        p
    end
  end

  def try_subdir(dir, subdir) do
    d = :filename.join(dir, subdir)

    case :filelib.is_dir(d) do
      true ->
        d

      false ->
        dir
    end
  end

  def write_file(text, dir, name) do
    write_file(text, dir, name, [{:encoding, :latin1}])
  end

  def write_file(text, dir, name, options) do
    file = :filename.join([dir, name])
    :ok = :filelib.ensure_dir(file)

    case :file.open(file, [:write] ++ options) do
      {:ok, fD} ->
        :io.put_chars(fD, text)
        :ok = :file.close(fD)

      {:error, r} ->
        r1 = :file.format_error(r)
        report('could not write file \'~ts\': ~ts.', [file, r1])
        exit(:error)
    end
  end

  def write_info_file(app, modules, dir) do
    ts = [{:modules, modules}]

    ts1 =
      cond do
        app === [] ->
          ts

        true ->
          [{:application, app} | ts]
      end

    s0 =
      for t <- ts1 do
        :io_lib.fwrite('~p.\n', [t])
      end

    s = ['%% encoding: UTF-8\n' | s0]
    write_file(s, dir, 'edoc-info', [{:encoding, :unicode}])
  end

  defp read_file(file) do
    case :file.read_file(file) do
      {:ok, bin} ->
        enc = :edoc_lib.read_encoding(file, [])

        case (try do
                :unicode.characters_to_list(bin, enc)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          string when is_list(string) ->
            {:ok, string}

          _ ->
            {:error, :invalid_unicode}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp info_file_data(ts) do
    app = :proplists.get_value(:application, ts, [])
    ms = :proplists.append_values(:modules, ts)
    {app, ms}
  end

  def read_info_file(dir) do
    file = :filename.join(dir, 'edoc-info')

    case :filelib.is_file(file) do
      true ->
        case read_file(file) do
          {:ok, text} ->
            parse_info_file(text, file)

          {:error, r} ->
            r1 = :file.format_error(r)
            warning('could not read \'~ts\': ~ts.', [file, r1])
            {[], []}
        end

      false ->
        {[], []}
    end
  end

  defp parse_info_file(text, name) do
    case parse_terms(text) do
      {:ok, vs} ->
        info_file_data(vs)

      {:error, :eof} ->
        warning('unexpected end of file in \'~ts\'.', [name])
        {[], []}

      {:error, {_Line, module, r}} ->
        warning('~ts: ~ts.', [module.format_error(r), name])
        {[], []}
    end
  end

  defp parse_terms(text) do
    case :erl_scan.string(text) do
      {:ok, ts, _Line} ->
        parse_terms_1(ts, [], [])

      {:error, r, _Line} ->
        {:error, r}
    end
  end

  defp parse_terms_1([t = {:dot, _L} | ts], as, vs) do
    case :erl_parse.parse_term(
           :lists.reverse([
             t
             | as
           ])
         ) do
      {:ok, v} ->
        parse_terms_1(ts, [], [v | vs])

      {:error, r} ->
        {:error, r}
    end
  end

  defp parse_terms_1([t | ts], as, vs) do
    parse_terms_1(ts, [t | as], vs)
  end

  defp parse_terms_1([], [], vs) do
    {:ok, :lists.reverse(vs)}
  end

  defp parse_terms_1([], _As, _Vs) do
    {:error, :eof}
  end

  def find_sources(path, opts) do
    rec = :proplists.get_bool(:subpackages, opts)
    ext = :proplists.get_value(:source_suffix, opts, '.erl')
    find_sources(path, rec, ext, opts)
  end

  defp find_sources(path, rec, ext, _Opts) do
    :lists.flatten(find_sources_1(path, rec, ext))
  end

  defp find_sources_1([p | ps], rec, ext) do
    dir = p
    fs1 = find_sources_1(ps, rec, ext)

    case :filelib.is_dir(dir) do
      true ->
        [find_sources_2(dir, rec, ext) | fs1]

      false ->
        fs1
    end
  end

  defp find_sources_1([], _Rec, _Ext) do
    []
  end

  defp find_sources_2(dir, rec, ext) do
    es = list_dir(dir, false)

    es1 =
      for e <- es, is_source_file(e, ext) do
        {e, dir}
      end

    case rec do
      true ->
        [find_sources_3(es, dir, rec, ext) | es1]

      false ->
        es1
    end
  end

  defp find_sources_3(es, dir, rec, ext) do
    for e <- es, is_source_dir(e, dir) do
      find_sources_2(:filename.join(dir, e), rec, ext)
    end
  end

  defp is_source_file(name, ext) do
    :filename.extension(name) == ext and
      is_name(
        :filename.rootname(
          name,
          ext
        )
      )
  end

  defp is_source_dir(name, dir) do
    :filelib.is_dir(:filename.join(dir, name))
  end

  def find_file([p | ps], name) do
    file = :filename.join(p, name)

    case :filelib.is_file(file) do
      true ->
        file

      false ->
        find_file(ps, name)
    end
  end

  def find_file([], _Name) do
    ''
  end

  def find_doc_dirs() do
    find_doc_dirs(:code.get_path())
  end

  defp find_doc_dirs([p0 | ps]) do
    p = :filename.absname(p0)

    p1 =
      case :filename.basename(p) do
        'ebin' ->
          :filename.dirname(p)

        _ ->
          p
      end

    dir = try_subdir(p1, 'doc')
    file = :filename.join(dir, 'edoc-info')

    case :filelib.is_file(file) do
      true ->
        [dir | find_doc_dirs(ps)]

      false ->
        find_doc_dirs(ps)
    end
  end

  defp find_doc_dirs([]) do
    []
  end

  defp get_doc_links(app, modules, opts) do
    path =
      :proplists.append_values(
        :doc_path,
        opts
      ) ++ find_doc_dirs()

    ds =
      for p <- path do
        {p, read_info_file(p)}
      end

    ds1 = [{'', {app, modules}} | ds]
    d = :dict.new()
    make_links(ds1, d, d)
  end

  defp make_links([{dir, {app, ms}} | ds], a, m) do
    a1 =
      cond do
        app == [] ->
          a

        true ->
          add_new(app, dir, a)
      end

    f = fn k, d ->
      add_new(k, dir, d)
    end

    m1 = :lists.foldl(f, m, ms)
    make_links(ds, a1, m1)
  end

  defp make_links([], a, m) do
    f = fn d ->
      fn k ->
        case :dict.find(k, d) do
          {:ok, v} ->
            v

          :error ->
            ''
        end
      end
    end

    {f.(a), f.(m)}
  end

  defp add_new(k, v, d) do
    case :dict.is_key(k, d) do
      true ->
        d

      false ->
        :dict.store(k, v, d)
    end
  end

  def get_doc_env(opts) do
    get_doc_env([], [], opts)
  end

  def get_doc_env(app, modules, opts) do
    suffix = :proplists.get_value(:file_suffix, opts, '.html')
    appDefault = :proplists.get_value(:app_default, opts, 'http://www.erlang.org/edoc/doc')
    includes = :proplists.append_values(:includes, opts)
    {a, m} = get_doc_links(app, modules, opts)
    r_env(file_suffix: suffix, apps: a, modules: m, app_default: appDefault, includes: includes)
  end

  def run_doclet(fun, opts) do
    run_plugin(:doclet, :edoc_doclet, fun, opts)
  end

  def run_layout(fun, opts) do
    run_plugin(:layout, :edoc_layout, fun, opts)
  end

  defp run_plugin(name, default, fun, opts) do
    run_plugin(name, name, default, fun, opts)
  end

  defp run_plugin(name, key, default, fun, opts)
       when is_atom(name) do
    module = get_plugin(key, default, opts)

    case (try do
            {:ok, fun.(module)}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, value} ->
        value

      r ->
        report('error in ~ts \'~w\': ~tP.', [name, module, r, 20])
        exit(:error)
    end
  end

  defp get_plugin(key, default, opts) do
    case :proplists.get_value(key, opts, default) do
      m when is_atom(m) ->
        m

      other ->
        report('bad value for option \'~w\': ~tP.', [key, other, 10])
        exit(:error)
    end
  end

  defp throw_error({:missing, c}, l) do
    throw_error({'missing \'~c\'.', [c]}, l)
  end

  defp throw_error(:eof, l) do
    throw({:error, l, 'unexpected end of expression.'})
  end

  defp throw_error({l, m, d}, _L) do
    throw({:error, l, {:format_error, m, d}})
  end

  defp throw_error(d, l) do
    throw({:error, l, d})
  end
end
