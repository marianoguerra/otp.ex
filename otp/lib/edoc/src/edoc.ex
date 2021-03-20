defmodule :m_edoc do
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

  def file(name) do
    file(name, [])
  end

  def file(name, options) do
    text = read(name, options)
    srcSuffix = :proplists.get_value(:source_suffix, options, '.erl')
    baseName = :filename.basename(name, srcSuffix)
    suffix = :proplists.get_value(:file_suffix, options, '.html')
    dir = :proplists.get_value(:dir, options, :filename.dirname(name))
    encoding = [{:encoding, :edoc_lib.read_encoding(name, [])}]
    :edoc_lib.write_file(text, dir, baseName ++ suffix, encoding)
  end

  def files(files) do
    files(files, [])
  end

  def files(files, options) do
    run(files, options)
  end

  def application(app) do
    application(app, [])
  end

  def application(app, options) when is_atom(app) do
    case :code.lib_dir(app) do
      dir when is_list(dir) ->
        application(app, dir, options)

      _ ->
        :edoc_report.report('cannot find application directory for \'~s\'.', [app])
        exit(:error)
    end
  end

  def application(app, dir, options) when is_atom(app) do
    src = :edoc_lib.try_subdir(dir, 'src')

    overview =
      :filename.join(
        :edoc_lib.try_subdir(dir, 'doc'),
        'overview.edoc'
      )

    opts =
      options ++
        [
          {:source_path, [src]},
          :subpackages,
          {:title, :io_lib.fwrite('The ~ts application', [app])},
          {:overview, overview},
          {:dir,
           :filename.join(
             dir,
             'doc'
           )},
          {:includes,
           [
             :filename.join(
               dir,
               'include'
             )
           ]}
        ]

    opts1 = set_app_default(app, dir, opts)
    run([], [{:application, app} | opts1])
  end

  defp set_app_default(app, dir0, opts) do
    case :proplists.get_value(:app_default, opts) do
      :undefined ->
        appName = :erlang.atom_to_list(app)
        dir = :edoc_lib.simplify_path(:filename.absname(dir0))

        appDir =
          case :filename.basename(dir) do
            ^appName ->
              :filename.dirname(dir)

            _ ->
              'http://www.erlang.org/edoc/doc'
          end

        [{:app_default, appDir} | opts]

      _ ->
        opts
    end
  end

  defp opt_defaults() do
    []
  end

  defp opt_negations() do
    [
      {:no_preprocess, :preprocess},
      {:no_subpackages, :subpackages},
      {:no_report_missing_types, :report_missing_types}
    ]
  end

  def run(files, opts0) do
    opts = expand_opts(opts0)
    ctxt = init_context(opts)
    dir = r_context(ctxt, :dir)
    path = :proplists.append_values(:source_path, opts)
    ss = sources(path, opts)

    {ss1, ms} =
      expand_sources(
        expand_files(files) ++ ss,
        opts
      )

    app = :proplists.get_value(:application, opts, [])
    {app1, ms1} = target_dir_info(dir, app, ms, opts)
    ms2 = :edoc_lib.unique(:lists.sort(ms1))
    env = :edoc_lib.get_doc_env(app1, ms2, opts)
    ctxt1 = r_context(ctxt, env: env)
    cmd = r_doclet_gen(sources: ss1, app: app1, modules: ms2)

    f = fn m ->
      m.run(cmd, ctxt1)
    end

    :edoc_lib.run_doclet(f, opts)
  end

  defp expand_opts(opts0) do
    :proplists.substitute_negations(
      opt_negations(),
      opts0 ++ opt_defaults()
    )
  end

  defp init_context(opts) do
    r_context(dir: :proplists.get_value(:dir, opts, '.'), opts: opts)
  end

  defp sources(path, opts) do
    :edoc_lib.find_sources(path, opts)
  end

  defp expand_files([f | fs]) do
    [
      {:filename.basename(f), :filename.dirname(f)}
      | expand_files(fs)
    ]
  end

  defp expand_files([]) do
    []
  end

  defp expand_sources(ss, opts) do
    suffix = :proplists.get_value(:source_suffix, opts, '.erl')

    ss1 =
      for {f, d} <- ss do
        {f, d}
      end

    expand_sources(ss1, suffix, :sets.new(), [], [])
  end

  defp expand_sources([{f, d} | fs], suffix, s, as, ms) do
    m = :erlang.list_to_atom(:filename.rootname(f, suffix))

    case :sets.is_element(m, s) do
      true ->
        expand_sources(fs, suffix, s, as, ms)

      false ->
        s1 = :sets.add_element(m, s)
        expand_sources(fs, suffix, s1, [{m, f, d} | as], [m | ms])
    end
  end

  defp expand_sources([], _Suffix, _S, as, ms) do
    {:lists.reverse(as), :lists.reverse(ms)}
  end

  defp target_dir_info(dir, app, ms, opts) do
    case :proplists.get_bool(:new, opts) do
      true ->
        {app, ms}

      false ->
        {app1, ms1} = :edoc_lib.read_info_file(dir)

        {cond do
           app == [] ->
             app1

           true ->
             app
         end, ms ++ ms1}
    end
  end

  def toc(dir) do
    toc(dir, [])
  end

  def toc(dir, opts) do
    paths =
      :proplists.append_values(
        :doc_path,
        opts
      ) ++ :edoc_lib.find_doc_dirs()

    toc(dir, paths, opts)
  end

  def toc(dir, paths, opts0) do
    opts = expand_opts(opts0 ++ [{:dir, dir}])
    ctxt = init_context(opts)
    env = :edoc_lib.get_doc_env(:"", [], opts)
    ctxt1 = r_context(ctxt, env: env)

    f = fn m ->
      m.run(r_doclet_toc(paths: paths), ctxt1)
    end

    :edoc_lib.run_doclet(f, opts)
  end

  def read(file) do
    read(file, [])
  end

  def read(file, opts) do
    {_ModuleName, doc} = get_doc(file, opts)
    layout(doc, opts)
  end

  def layout(doc) do
    layout(doc, [])
  end

  def layout(doc, opts) do
    f = fn m ->
      m.module(doc, opts)
    end

    :edoc_lib.run_layout(f, opts)
  end

  def read_comments(file) do
    read_comments(file, [])
  end

  def read_comments(file, _Opts) do
    :erl_comment_scan.file(file)
  end

  def read_source(name) do
    read_source(name, [])
  end

  def read_source(name, opts0) do
    opts = expand_opts(opts0)

    case read_source_1(name, opts) do
      {:ok, forms} ->
        check_forms(forms, name, opts)
        forms

      {:error, r} ->
        :edoc_report.error({'error reading file \'~ts\'.', [:edoc_lib.filename(name)]})
        exit({:error, r})
    end
  end

  defp read_source_1(name, opts) do
    case :proplists.get_bool(:preprocess, opts) do
      true ->
        read_source_2(name, opts)

      false ->
        :epp_dodger.quick_parse_file(
          name,
          opts ++ [{:no_fail, false}]
        )
    end
  end

  defp read_source_2(name, opts) do
    includes =
      :proplists.append_values(
        :includes,
        opts
      ) ++ [:filename.dirname(name)]

    macros = :proplists.append_values(:macros, opts)
    parse_file(name, includes, macros)
  end

  defp parse_file(name, includes, macros) do
    case parse_file(:utf8, name, includes, macros) do
      :invalid_unicode ->
        parse_file(:latin1, name, includes, macros)

      ret ->
        ret
    end
  end

  defp parse_file(defEncoding, name, includes, macros) do
    options = [
      {:name, name},
      {:includes, includes},
      {:macros, macros},
      {:default_encoding, defEncoding}
    ]

    case :epp.open([:extra | options]) do
      {:ok, epp, extra} ->
        try do
          parse_file(epp)
        else
          forms ->
            encoding = :proplists.get_value(:encoding, extra)

            case find_invalid_unicode(forms) do
              :invalid_unicode when encoding !== :utf8 ->
                :invalid_unicode

              _ ->
                {:ok, forms}
            end
        after
          _ = :epp.close(epp)
        end

      error ->
        error
    end
  end

  defp find_invalid_unicode([h | t]) do
    case h do
      {:error, {_Line, :file_io_server, :invalid_unicode}} ->
        :invalid_unicode

      _Other ->
        find_invalid_unicode(t)
    end
  end

  defp find_invalid_unicode([]) do
    :none
  end

  defp parse_file(epp) do
    case scan_and_parse(epp) do
      {:ok, form} ->
        [form | parse_file(epp)]

      {:error, e} ->
        [{:error, e} | parse_file(epp)]

      {:eof, location} ->
        [{:eof, location}]
    end
  end

  defp scan_and_parse(epp) do
    case :epp.scan_erl_form(epp) do
      {:ok, toks0} ->
        toks = fix_last_line(toks0)

        case :erl_parse.parse_form(toks) do
          {:ok, form} ->
            {:ok, form}

          else__ ->
            else__
        end

      else__ ->
        else__
    end
  end

  defp fix_last_line(toks0) do
    toks1 = :lists.reverse(toks0)
    lastLine = :erl_scan.line(hd(toks1))
    fll(toks1, lastLine, [])
  end

  defp fll([{category, anno0, symbol} | l], lastLine, ts) do
    anno = :erl_anno.set_line(lastLine, anno0)
    :lists.reverse(l, [{category, anno, symbol} | ts])
  end

  defp fll([t | l], lastLine, ts) do
    fll(l, lastLine, [t | ts])
  end

  defp fll(l, _LastLine, ts) do
    :lists.reverse(l, ts)
  end

  defp check_forms(fs, name, opts) do
    fun = fn f ->
      case :erl_syntax.type(f) do
        :error_marker ->
          case :erl_syntax.error_marker_info(f) do
            {l, m, d} ->
              :edoc_report.error(l, name, {:format_error, m, d})

              case :proplists.get_bool(:preprocess, opts) do
                true ->
                  :ok

                false ->
                  helpful_message(name)
              end

            other ->
              :edoc_report.report(name, 'unknown error in source code: ~w.', [other])
          end

          exit(:error)

        _ ->
          :ok
      end
    end

    :lists.foreach(fun, fs)
  end

  defp helpful_message(name) do
    ms = [
      'If the error is caused by too exotic macro',
      'definitions or uses of macros, adding option',
      '{preprocess, true} can help. See also edoc(3).'
    ]

    :lists.foreach(
      fn m ->
        :edoc_report.report(name, m, [])
      end,
      ms
    )
  end

  def get_doc(file) do
    get_doc(file, [])
  end

  def get_doc(file, opts) do
    env = :edoc_lib.get_doc_env(opts)
    get_doc(file, env, opts)
  end

  def get_doc(file, env, opts) do
    :edoc_extract.source(file, env, opts)
  end
end
