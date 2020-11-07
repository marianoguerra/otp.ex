defmodule :m_leex do
  use Bitwise

  import :lists,
    only: [
      delete: 2,
      flatmap: 2,
      foldl: 3,
      foreach: 2,
      keydelete: 3,
      keysort: 2,
      map: 2,
      member: 2,
      reverse: 1,
      sort: 1
    ]

  import :orddict, only: [store: 3]
  import :ordsets, only: [add_element: 2, is_element: 2, union: 2]
  require Record

  Record.defrecord(:r_options, :options,
    includes: [],
    outdir: '.',
    output_type: :undefined,
    defines: [],
    warning: 1,
    verbose: false,
    optimize: 999,
    specific: [],
    outfile: '',
    cwd: :undefined
  )

  Record.defrecord(:r_leex, :leex,
    xfile: [],
    efile: [],
    ifile: [],
    gfile: [],
    module: :undefined,
    opts: [],
    encoding: :none,
    errors: [],
    warnings: []
  )

  Record.defrecord(:r_nfa_state, :nfa_state, no: :undefined, edges: [], accept: :noaccept)

  Record.defrecord(:r_dfa_state, :dfa_state, no: :undefined, nfa: [], trans: [], accept: :noaccept)

  def compile(
        input0,
        output0,
        r_options(warning: warnLevel, verbose: verbose, includes: includes, specific: specific)
      ) do
    input = assure_extension(shorten_filename(input0), '.xrl')
    output = assure_extension(shorten_filename(output0), '.erl')
    includefile = :lists.sublist(includes, 1)

    werror =
      :proplists.get_bool(
        :warnings_as_errors,
        specific
      )

    opts = [
      {:scannerfile, output},
      {:includefile, includefile},
      {:verbose, verbose},
      {:report_errors, true},
      {:report_warnings, warnLevel > 0},
      {:warnings_as_errors, werror}
    ]

    case file(input, opts) do
      {:ok, _} ->
        :ok

      :error ->
        :error
    end
  end

  def file(file) do
    file(file, [])
  end

  def file(file, opts0) do
    case is_filename(file) do
      :no ->
        :erlang.error(:badarg, [file, opts0])

      _ ->
        :ok
    end

    opts =
      case options(opts0) do
        :badarg ->
          :erlang.error(:badarg, [file, opts0])

        options ->
          options
      end

    st0 = r_leex()
    st1 = filenames(file, opts, st0)

    st =
      try do
        {:ok, rEAs, actions, code, st2} = parse_file(st1)
        {dFA, dF} = make_dfa(rEAs, st2)

        case werror(st2) do
          false ->
            st3 = out_file(st2, dFA, dF, actions, code)

            case :lists.member(:dfa_graph, r_leex(st3, :opts)) do
              true ->
                out_dfa_graph(st3, dFA, dF)

              false ->
                st3
            end

          true ->
            st2
        end
      catch
        r_leex() = st4 ->
          st4
      end

    leex_ret(st)
  end

  def format_error({:file_error, reason}) do
    :io_lib.fwrite('~ts', [:file.format_error(reason)])
  end

  def format_error(:missing_defs) do
    'missing Definitions'
  end

  def format_error(:missing_rules) do
    'missing Rules'
  end

  def format_error(:missing_code) do
    'missing Erlang code'
  end

  def format_error(:empty_rules) do
    'no rules'
  end

  def format_error(:bad_rule) do
    'bad rule'
  end

  def format_error({:regexp, e}) do
    es =
      case e do
        {:interval_range, _} ->
          'interval range'

        {:unterminated, cs} ->
          'unterminated ' ++ cs

        {:illegal_char, cs} ->
          'illegal character ' ++ cs

        {:char_class, what} ->
          ['illegal character class ', :io_lib.write_string(what)]
      end

    ['bad regexp `', es, '\'']
  end

  def format_error(:ignored_characters) do
    'ignored characters'
  end

  def format_error(:cannot_parse) do
    :io_lib.fwrite('cannot parse; probably encoding mismatch', [])
  end

  defp assure_extension(file, ext) do
    :lists.concat([strip_extension(file, ext), ext])
  end

  defp strip_extension(file, ext) do
    case :filename.extension(file) do
      ^ext ->
        :filename.rootname(file)

      _Other ->
        file
    end
  end

  defp options(options0) when is_list(options0) do
    try do
      options =
        flatmap(
          fn
            :return ->
              short_option(:return, true)

            :report ->
              short_option(:report, true)

            {:return, t} ->
              short_option(:return, t)

            {:report, t} ->
              short_option(:report, t)

            t ->
              [t]
          end,
          options0
        )

      options(
        options,
        [
          :scannerfile,
          :includefile,
          :report_errors,
          :report_warnings,
          :warnings_as_errors,
          :return_errors,
          :return_warnings,
          :verbose,
          :dfa_graph
        ],
        []
      )
    catch
      :error, _ ->
        :badarg
    end
  end

  defp options(option) do
    options([option])
  end

  defp short_option(:return, t) do
    [{:return_errors, t}, {:return_warnings, t}]
  end

  defp short_option(:report, t) do
    [{:report_errors, t}, {:report_warnings, t}]
  end

  defp options(options0, [key | keys], l)
       when is_list(options0) do
    options =
      case member(key, options0) do
        true ->
          [atom_option(key) | delete(key, options0)]

        false ->
          options0
      end

    v =
      case :lists.keyfind(key, 1, options) do
        {^key, filename0}
        when key === :includefile or
               key === :scannerfile ->
          case is_filename(filename0) do
            :no ->
              :badarg

            filename ->
              {:ok, [{key, filename}]}
          end

        {^key, bool} = kB when is_boolean(bool) ->
          {:ok, [kB]}

        {^key, _} ->
          :badarg

        false ->
          {:ok, [{key, default_option(key)}]}
      end

    case v do
      :badarg ->
        :badarg

      {:ok, keyValueL} ->
        newOptions = keydelete(key, 1, options)
        options(newOptions, keys, keyValueL ++ l)
    end
  end

  defp options([], [], l) do
    foldl(
      fn
        {_, false}, a ->
          a

        {tag, true}, a ->
          [tag | a]

        f, a ->
          [f | a]
      end,
      [],
      l
    )
  end

  defp options(_Options, _, _L) do
    :badarg
  end

  defp default_option(:dfa_graph) do
    false
  end

  defp default_option(:includefile) do
    []
  end

  defp default_option(:report_errors) do
    true
  end

  defp default_option(:report_warnings) do
    true
  end

  defp default_option(:warnings_as_errors) do
    false
  end

  defp default_option(:return_errors) do
    false
  end

  defp default_option(:return_warnings) do
    false
  end

  defp default_option(:scannerfile) do
    []
  end

  defp default_option(:verbose) do
    false
  end

  defp atom_option(:dfa_graph) do
    {:dfa_graph, true}
  end

  defp atom_option(:report_errors) do
    {:report_errors, true}
  end

  defp atom_option(:report_warnings) do
    {:report_warnings, true}
  end

  defp atom_option(:warnings_as_errors) do
    {:warnings_as_errors, true}
  end

  defp atom_option(:return_errors) do
    {:return_errors, true}
  end

  defp atom_option(:return_warnings) do
    {:return_warnings, true}
  end

  defp atom_option(:verbose) do
    {:verbose, true}
  end

  defp atom_option(key) do
    key
  end

  defp is_filename(t) do
    try do
      :filename.flatten(t)
    catch
      :error, _ ->
        :no
    end
  end

  defp shorten_filename(name0) do
    {:ok, cwd} = :file.get_cwd()

    case :string.prefix(name0, cwd) do
      :nomatch ->
        name0

      rest ->
        case :unicode.characters_to_list(rest) do
          '/' ++ n ->
            n

          n ->
            n
        end
    end
  end

  defp leex_ret(st) do
    report_errors(st)
    report_warnings(st)
    es = pack_errors(r_leex(st, :errors))
    ws = pack_warnings(r_leex(st, :warnings))
    werror = werror(st)

    cond do
      werror ->
        do_error_return(st, es, ws)

      es === [] ->
        case member(:return_warnings, r_leex(st, :opts)) do
          true ->
            {:ok, r_leex(st, :efile), ws}

          false ->
            {:ok, r_leex(st, :efile)}
        end

      true ->
        do_error_return(st, es, ws)
    end
  end

  defp do_error_return(st, es, ws) do
    case member(:return_errors, r_leex(st, :opts)) do
      true ->
        {:error, es, ws}

      false ->
        :error
    end
  end

  defp werror(st) do
    r_leex(st, :warnings) !== [] and
      member(
        :warnings_as_errors,
        r_leex(st, :opts)
      )
  end

  defp pack_errors([{file, _} | _] = es) do
    [
      {file,
       flatmap(
         fn {_, e} ->
           [e]
         end,
         sort(es)
       )}
    ]
  end

  defp pack_errors([]) do
    []
  end

  defp pack_warnings([{file, _} | _] = ws) do
    [
      {file,
       flatmap(
         fn {_, w} ->
           [w]
         end,
         sort(ws)
       )}
    ]
  end

  defp pack_warnings([]) do
    []
  end

  defp report_errors(st) do
    when_opt(
      fn ->
        foreach(
          fn
            {file, {:none, mod, e}} ->
              :io.fwrite('~ts: ~ts\n', [file, mod.format_error(e)])

            {file, {line, mod, e}} ->
              :io.fwrite('~ts:~w: ~ts\n', [file, line, mod.format_error(e)])
          end,
          sort(r_leex(st, :errors))
        )
      end,
      :report_errors,
      r_leex(st, :opts)
    )
  end

  defp report_warnings(st) do
    werror = member(:warnings_as_errors, r_leex(st, :opts))

    prefix =
      case werror do
        true ->
          ''

        false ->
          'Warning: '
      end

    reportWerror =
      werror and
        member(
          :report_errors,
          r_leex(st, :opts)
        )

    shouldReport =
      member(
        :report_warnings,
        r_leex(st, :opts)
      ) or reportWerror

    when_bool(
      fn ->
        foreach(
          fn
            {file, {:none, mod, w}} ->
              :io.fwrite(
                '~ts: ~s~ts\n',
                [file, prefix, mod.format_error(w)]
              )

            {file, {line, mod, w}} ->
              :io.fwrite(
                '~ts:~w: ~s~ts\n',
                [file, line, prefix, mod.format_error(w)]
              )
          end,
          sort(r_leex(st, :warnings))
        )
      end,
      shouldReport
    )
  end

  defp add_error(e, st) do
    add_error(r_leex(st, :xfile), e, st)
  end

  defp add_error(file, error, st) do
    throw(r_leex(st, errors: [{file, error} | r_leex(st, :errors)]))
  end

  defp add_warning(line, w, st) do
    r_leex(st,
      warnings: [
        {r_leex(st, :xfile), {line, :leex, w}}
        | r_leex(st, :warnings)
      ]
    )
  end

  defp filenames(file, opts, st0) do
    dir = :filename.dirname(file)
    base = :filename.basename(file, '.xrl')
    xfile = :filename.join(dir, base ++ '.xrl')
    efile = base ++ '.erl'
    gfile = base ++ '.dot'
    module = :erlang.list_to_atom(base)
    st1 = r_leex(st0, xfile: xfile, opts: opts, module: module)
    {:includefile, ifile0} = :lists.keyfind(:includefile, 1, opts)
    ifile = inc_file_name(ifile0)
    {:scannerfile, ofile} = :lists.keyfind(:scannerfile, 1, opts)

    cond do
      ofile === [] ->
        r_leex(st1,
          efile: :filename.join(dir, efile),
          ifile: ifile,
          gfile: :filename.join(dir, gfile)
        )

      true ->
        d = :filename.dirname(ofile)
        r_leex(st1, efile: ofile, ifile: ifile, gfile: :filename.join(d, gfile))
    end
  end

  defp when_opt(do__, opt, opts) do
    case member(opt, opts) do
      true ->
        do__.()

      false ->
        :ok
    end
  end

  defp when_bool(do__, bool) do
    case bool do
      true ->
        do__.()

      false ->
        :ok
    end
  end

  defp verbose_print(st, format, args) do
    when_opt(
      fn ->
        :io.fwrite(format, args)
      end,
      :verbose,
      r_leex(st, :opts)
    )
  end

  defp parse_file(st0) do
    case :file.open(r_leex(st0, :xfile), [:read]) do
      {:ok, xfile} ->
        st1 = r_leex(st0, encoding: :epp.set_encoding(xfile))

        try do
          verbose_print(st1, 'Parsing file ~ts, ', [r_leex(st1, :xfile)])
          {:ok, line1, st2} = parse_head(xfile, st1)
          {:ok, line2, macs, st3} = parse_defs(xfile, line1, st2)
          {:ok, line3, rEAs, actions, st4} = parse_rules(xfile, line2, macs, st3)
          {:ok, code, st5} = parse_code(xfile, line3, st4)
          verbose_print(st5, 'contained ~w rules.~n', [length(rEAs)])
          {:ok, rEAs, actions, code, st5}
        after
          :ok = :file.close(xfile)
        end

      {:error, error} ->
        add_error({:none, :leex, {:file_error, error}}, st0)
    end
  end

  defp parse_head(ifile, st) do
    {:ok, nextline(ifile, 0, st), st}
  end

  defp parse_defs(ifile, {:ok, 'Definitions.' ++ rest, l}, st) do
    st1 = warn_ignored_chars(l, rest, st)
    parse_defs(ifile, nextline(ifile, l, st), [], st1)
  end

  defp parse_defs(_, {:ok, _, l}, st) do
    add_error({l, :leex, :missing_defs}, st)
  end

  defp parse_defs(_, {:eof, l}, st) do
    add_error({l, :leex, :missing_defs}, st)
  end

  defp parse_defs(ifile, {:ok, chars, l} = line, ms, st) do
    mS = '^[ \t]*([A-Z_][A-Za-z0-9_]*)[ \t]*=[ \t]*([^ \t\r\n]*)[ \t\r\n]*$'

    case :re.run(chars, mS, [{:capture, :all_but_first, :list}, :unicode]) do
      {:match, [name, def__]} ->
        parse_defs(ifile, nextline(ifile, l, st), [{name, def__} | ms], st)

      _ ->
        {:ok, line, ms, st}
    end
  end

  defp parse_defs(_, line, ms, st) do
    {:ok, line, ms, st}
  end

  defp parse_rules(ifile, {:ok, 'Rules.' ++ rest, l}, ms, st) do
    st1 = warn_ignored_chars(l, rest, st)
    parse_rules(ifile, nextline(ifile, l, st), ms, [], [], 0, st1)
  end

  defp parse_rules(_, {:ok, _, l}, _, st) do
    add_error({l, :leex, :missing_rules}, st)
  end

  defp parse_rules(_, {:eof, l}, _, st) do
    add_error({l, :leex, :missing_rules}, st)
  end

  defp parse_rules(ifile, nextLine, ms, rEAs, as, n, st) do
    case nextLine do
      {:ok, 'Erlang code.' ++ _Rest, _} ->
        parse_rules_end(ifile, nextLine, rEAs, as, st)

      {:ok, chars, l0} ->
        case collect_rule(ifile, chars, l0) do
          {:ok, re, atoks, l1} ->
            {:ok, rEA, a, st1} = parse_rule(re, l0, atoks, ms, n, st)
            parse_rules(ifile, nextline(ifile, l1, st), ms, [rEA | rEAs], [a | as], n + 1, st1)

          {:error, e} ->
            add_error(e, st)
        end

      {:eof, _} ->
        parse_rules_end(ifile, nextLine, rEAs, as, st)
    end
  end

  defp parse_rules_end(_, {:ok, _, l}, [], [], st) do
    add_error({l, :leex, :empty_rules}, st)
  end

  defp parse_rules_end(_, {:eof, l}, [], [], st) do
    add_error({l, :leex, :empty_rules}, st)
  end

  defp parse_rules_end(_, nextLine, rEAs, as, st) do
    {:ok, nextLine, reverse(rEAs), reverse(as), st}
  end

  defp collect_rule(ifile, chars, l0) do
    {regExp, rest} = :string.take(chars, ' \t\r\n', true)

    case collect_action(ifile, rest, l0, []) do
      {:ok, [{:":", _} | toks], l1} ->
        {:ok, regExp, toks, l1}

      {:ok, _, _} ->
        {:error, {l0, :leex, :bad_rule}}

      {:eof, l1} ->
        {:error, {l1, :leex, :bad_rule}}

      {:error, e, _} ->
        {:error, e}
    end
  end

  defp collect_action(_Ifile, {:error, _}, l, _Cont0) do
    {:error, {l, :leex, :cannot_parse}, :ignored_end_line}
  end

  defp collect_action(ifile, chars, l0, cont0) do
    case :erl_scan.tokens(cont0, chars, l0) do
      {:done, {:ok, toks, _}, _} ->
        {:ok, toks, l0}

      {:done, {:eof, _}, _} ->
        {:eof, l0}

      {:done, {:error, e, _}, _} ->
        {:error, e, l0}

      {:more, cont1} ->
        collect_action(ifile, :io.get_line(ifile, :leex), l0 + 1, cont1)
    end
  end

  defp parse_rule(s, line, [{:dot, _}], ms, n, st) do
    case parse_rule_regexp(s, ms, st) do
      {:ok, r} ->
        {:ok, {r, n}, {n, :empty_action}, st}

      {:error, e} ->
        add_error({line, :leex, e}, st)
    end
  end

  defp parse_rule(s, line, atoks, ms, n, st) do
    case parse_rule_regexp(s, ms, st) do
      {:ok, r} ->
        tokenChars = var_used(:TokenChars, atoks)
        tokenLen = var_used(:TokenLen, atoks)
        tokenLine = var_used(:TokenLine, atoks)
        {:ok, {r, n}, {n, atoks, tokenChars, tokenLen, tokenLine}, st}

      {:error, e} ->
        add_error({line, :leex, e}, st)
    end
  end

  defp var_used(name, toks) do
    case :lists.keyfind(name, 3, toks) do
      {:var, _, ^name} ->
        true

      _ ->
        false
    end
  end

  defp parse_rule_regexp(rE0, [{m, exp} | ms], st) do
    split = :re.split(rE0, '\\{' ++ m ++ '\\}', [{:return, :list}, :unicode])
    rE1 = :lists.append(:lists.join(exp, split))
    parse_rule_regexp(rE1, ms, st)
  end

  defp parse_rule_regexp(rE, [], st) do
    case re_parse(rE, st) do
      {:ok, r} ->
        {:ok, r}

      {:error, e} ->
        {:error, {:regexp, e}}
    end
  end

  defp parse_code(ifile, {:ok, 'Erlang code.' ++ rest, codeL}, st) do
    st1 = warn_ignored_chars(codeL, rest, st)
    {:ok, codePos} = :file.position(ifile, :cur)
    endCodeLine = count_lines(ifile, codeL, st)
    nCodeLines = endCodeLine - codeL
    {:ok, {codeL, codePos, nCodeLines}, st1}
  end

  defp parse_code(_, {:ok, _, l}, st) do
    add_error({l, :leex, :missing_code}, st)
  end

  defp parse_code(_, {:eof, l}, st) do
    add_error({l, :leex, :missing_code}, st)
  end

  defp count_lines(file, n, st) do
    case :io.get_line(file, :leex) do
      :eof ->
        n

      {:error, _} ->
        add_error({n + 1, :leex, :cannot_parse}, st)

      _Line ->
        count_lines(file, n + 1, st)
    end
  end

  defp nextline(ifile, l, st) do
    case :io.get_line(ifile, :leex) do
      :eof ->
        {:eof, l}

      {:error, _} ->
        add_error({l + 1, :leex, :cannot_parse}, st)

      chars ->
        case :string.take(chars, ' \t\n') do
          {_, [?% | _Rest]} ->
            nextline(ifile, l + 1, st)

          {_, []} ->
            nextline(ifile, l + 1, st)

          _Other ->
            {:ok, chars, l + 1}
        end
    end
  end

  defp warn_ignored_chars(line, s, st) do
    case non_white(s) do
      [] ->
        st

      _ ->
        add_warning(line, :ignored_characters, st)
    end
  end

  defp non_white(s) do
    for c <- s, c > ?\s, c < 128 or c > 160 do
      c
    end
  end

  defp re_parse(cs0, st) do
    case (try do
            re_reg(cs0, 0, st)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {rE, _, []} ->
        {:ok, rE}

      {_, _, [c | _]} ->
        {:error, {:illegal_char, [c]}}

      {:parse_error, e} ->
        {:error, e}
    end
  end

  defp parse_error(e) do
    throw({:parse_error, e})
  end

  defp re_reg(cs, sn, st) do
    re_alt(cs, sn, st)
  end

  defp re_alt(cs0, sn0, st) do
    {l, sn1, cs1} = re_seq(cs0, sn0, st)

    case re_alt1(cs1, sn1, st) do
      {[], sn2, cs2} ->
        {l, sn2, cs2}

      {rs, sn2, cs2} ->
        {{:alt, [l | rs]}, sn2, cs2}
    end
  end

  defp re_alt1([?| | cs0], sn0, st) do
    {l, sn1, cs1} = re_seq(cs0, sn0, st)
    {rs, sn2, cs2} = re_alt1(cs1, sn1, st)
    {[l | rs], sn2, cs2}
  end

  defp re_alt1(cs, sn, _) do
    {[], sn, cs}
  end

  defp re_seq(cs0, sn0, st) do
    case re_seq1(cs0, sn0, st) do
      {[], sn1, cs1} ->
        {:epsilon, sn1, cs1}

      {[r], sn1, cs1} ->
        {r, sn1, cs1}

      {rs, sn1, cs1} ->
        {{:seq, rs}, sn1, cs1}
    end
  end

  defp re_seq1([c | _] = cs0, sn0, st)
       when c !== ?| and
              c !== ?) do
    {l, sn1, cs1} = re_repeat(cs0, sn0, st)
    {rs, sn2, cs2} = re_seq1(cs1, sn1, st)
    {[l | rs], sn2, cs2}
  end

  defp re_seq1(cs, sn, _) do
    {[], sn, cs}
  end

  defp re_repeat(cs0, sn0, st) do
    {s, sn1, cs1} = re_single(cs0, sn0, st)
    re_repeat1(cs1, sn1, s, st)
  end

  defp re_repeat1([?* | cs], sn, s, st) do
    re_repeat1(cs, sn, {:kclosure, s}, st)
  end

  defp re_repeat1([?+ | cs], sn, s, st) do
    re_repeat1(cs, sn, {:pclosure, s}, st)
  end

  defp re_repeat1([?? | cs], sn, s, st) do
    re_repeat1(cs, sn, {:optional, s}, st)
  end

  defp re_repeat1(cs, sn, s, _) do
    {s, sn, cs}
  end

  defp re_single([?( | cs0], sn0, st) do
    sn1 = sn0 + 1

    case re_reg(cs0, sn1, st) do
      {s, sn2, [?) | cs1]} ->
        {s, sn2, cs1}

      _ ->
        parse_error({:unterminated, '('})
    end
  end

  defp re_single([?. | cs], sn, _) do
    {{:comp_class, '\n'}, sn, cs}
  end

  defp re_single('[^' ++ cs0, sn, st) do
    case re_char_class(cs0, st) do
      {cc, [?] | cs1]} ->
        {{:comp_class, cc}, sn, cs1}

      _ ->
        parse_error({:unterminated, '['})
    end
  end

  defp re_single([?[ | cs0], sn, st) do
    case re_char_class(cs0, st) do
      {cc, [?] | cs1]} ->
        {{:char_class, cc}, sn, cs1}

      _ ->
        parse_error({:unterminated, '['})
    end
  end

  defp re_single([?\\ | cs0], sn, _) do
    {c, cs1} = re_char(?\\, cs0)
    {{:lit, [c]}, sn, cs1}
  end

  defp re_single([c | cs0], sn, st) do
    case special_char(c, st) do
      true ->
        parse_error({:illegal_char, [c]})

      false ->
        {^c, cs1} = re_char(c, cs0)
        {{:lit, [c]}, sn, cs1}
    end
  end

  defp re_char(?\\, [[o1, o2, o3] | s])
       when o1 >= ?0 and
              o1 <= ?7 and o2 >= ?0 and
              o2 <= ?7 and o3 >= ?0 and
              o3 <= ?7 do
    {(o1 * 8 + o2) * 8 + o3 - 73 * ?0, s}
  end

  defp re_char(?\\, [[?x, h1, h2] | s])
       when (h1 >= ?0 and h1 <= ?9) or (h1 >= ?A and h1 <= ?F) or
              (h1 >= ?a and h1 <= ?f and
                 h2 >= ?0 and h2 <= ?9) or (h2 >= ?A and h2 <= ?F) or (h2 >= ?a and h2 <= ?f) do
    {:erlang.list_to_integer([h1, h2], 16), s}
  end

  defp re_char(?\\, [[?x, ?{] | s0]) do
    re_hex(s0, [])
  end

  defp re_char(?\\, [?x | _]) do
    parse_error({:illegal_char, '\\x'})
  end

  defp re_char(?\\, [c | s]) do
    {escape_char(c), s}
  end

  defp re_char(?\\, []) do
    parse_error({:unterminated, '\\'})
  end

  defp re_char(c, s) do
    {c, s}
  end

  defp re_hex([c | cs], l)
       when (c >= ?0 and c <= ?9) or (c >= ?A and c <= ?F) or (c >= ?a and c <= ?f) do
    re_hex(cs, [c | l])
  end

  defp re_hex([?} | s], l0) do
    l = :lists.reverse(l0)

    case :erlang.list_to_integer(l, 16) do
      c when c <= 1_114_111 ->
        {c, s}

      _ ->
        parse_error({:illegal_char, [[?\\, ?x, ?{] | l] ++ '}'})
    end
  end

  defp re_hex(_, _) do
    parse_error({:unterminated, '\\x{'})
  end

  defp special_char(?^, _) do
    true
  end

  defp special_char(?., _) do
    true
  end

  defp special_char(?[, _) do
    true
  end

  defp special_char(?$, _) do
    true
  end

  defp special_char(?(, _) do
    true
  end

  defp special_char(?), _) do
    true
  end

  defp special_char(?|, _) do
    true
  end

  defp special_char(?*, _) do
    true
  end

  defp special_char(?+, _) do
    true
  end

  defp special_char(??, _) do
    true
  end

  defp special_char(?\\, _) do
    true
  end

  defp special_char(_, _) do
    false
  end

  defp re_char_class([?] | cs], st) do
    re_char_class(cs, [?]], st)
  end

  defp re_char_class(cs, st) do
    re_char_class(cs, [], st)
  end

  defp re_char_class([c1 | cs0], cc, st) when c1 !== ?] do
    case re_char(c1, cs0) do
      {cf, [[?-, c2] | cs1]} when c2 !== ?] ->
        case re_char(c2, cs1) do
          {cl, cs2} when cf < cl ->
            re_char_class(cs2, [{:range, cf, cl} | cc], st)

          {_, cs2} ->
            parse_error({:char_class, string_between([c1 | cs0], cs2)})
        end

      {c, cs1} ->
        re_char_class(cs1, [c | cc], st)
    end
  end

  defp re_char_class(cs, cc, _) do
    {reverse(cc), cs}
  end

  defp escape_char(?n) do
    ?\n
  end

  defp escape_char(?r) do
    ?\r
  end

  defp escape_char(?t) do
    ?\t
  end

  defp escape_char(?v) do
    ?\v
  end

  defp escape_char(?b) do
    ?\b
  end

  defp escape_char(?f) do
    ?\f
  end

  defp escape_char(?e) do
    ?\e
  end

  defp escape_char(?s) do
    ?\s
  end

  defp escape_char(?d) do
    ?\d
  end

  defp escape_char(c) do
    c
  end

  defp string_between(cs1, cs2) do
    :string.slice(cs1, 0, :string.length(cs1) - :string.length(cs2))
  end

  defp make_dfa(rEAs, st) do
    {nFA, nF} = build_combined_nfa(rEAs)
    verbose_print(st, 'NFA contains ~w states, ', [tuple_size(nFA)])
    {dFA0, dF0} = build_dfa(nFA, nF)
    verbose_print(st, 'DFA contains ~w states, ', [length(dFA0)])
    {dFA, dF} = minimise_dfa(dFA0, dF0)
    verbose_print(st, 'minimised to ~w states.~n', [length(dFA)])
    {dFA, dF}
  end

  defp build_combined_nfa(rEAs) do
    {nFA0, firsts, free} = build_nfa_list(rEAs, [], [], 1)
    f = r_nfa_state(no: free, edges: epsilon_trans(firsts))
    {:erlang.list_to_tuple(keysort(r_nfa_state(:no), [f | nFA0])), free}
  end

  defp build_nfa_list([{rE, action} | rEAs], nFA0, firsts, free0) do
    {nFA1, free1, first} = build_nfa(rE, free0, action)
    build_nfa_list(rEAs, nFA1 ++ nFA0, [first | firsts], free1)
  end

  defp build_nfa_list([], nFA, firsts, free) do
    {nFA, reverse(firsts), free}
  end

  defp epsilon_trans(firsts) do
    for f <- firsts do
      {:epsilon, f}
    end
  end

  defp build_nfa(rE, n0, action) do
    {nFA, n1, e} = build_nfa(rE, n0 + 1, n0, [])
    {[r_nfa_state(no: e, accept: {:accept, action}) | nFA], n1, n0}
  end

  defp build_nfa({:alt, rEs}, n, f, nFA) do
    build_nfa_alt(rEs, n, f, nFA)
  end

  defp build_nfa({:seq, rEs}, n, f, nFA) do
    build_nfa_seq(rEs, n, f, nFA)
  end

  defp build_nfa({:kclosure, rE}, n0, f, nFA0) do
    {nFA1, n1, e1} = build_nfa(rE, n0 + 1, n0, nFA0)
    e = n1

    {[
       [
         r_nfa_state(no: f, edges: [{:epsilon, n0}, {:epsilon, e}]),
         r_nfa_state(no: e1, edges: [{:epsilon, n0}, {:epsilon, e}])
       ]
       | nFA1
     ], n1 + 1, e}
  end

  defp build_nfa({:pclosure, rE}, n0, f, nFA0) do
    {nFA1, n1, e1} = build_nfa(rE, n0 + 1, n0, nFA0)
    e = n1

    {[
       [
         r_nfa_state(no: f, edges: [{:epsilon, n0}]),
         r_nfa_state(no: e1, edges: [{:epsilon, n0}, {:epsilon, e}])
       ]
       | nFA1
     ], n1 + 1, e}
  end

  defp build_nfa({:optional, rE}, n0, f, nFA0) do
    {nFA1, n1, e1} = build_nfa(rE, n0 + 1, n0, nFA0)
    e = n1

    {[
       [
         r_nfa_state(no: f, edges: [{:epsilon, n0}, {:epsilon, e}]),
         r_nfa_state(no: e1, edges: [{:epsilon, e}])
       ]
       | nFA1
     ], n1 + 1, e}
  end

  defp build_nfa({:char_class, cc}, n, f, nFA) do
    {[r_nfa_state(no: f, edges: [{pack_cc(cc), n}]) | nFA], n + 1, n}
  end

  defp build_nfa({:comp_class, cc}, n, f, nFA) do
    {[r_nfa_state(no: f, edges: [{comp_class(cc), n}]) | nFA], n + 1, n}
  end

  defp build_nfa({:lit, cs}, n, f, nFA) do
    build_nfa_lit(cs, n, f, nFA)
  end

  defp build_nfa(:epsilon, n, f, nFA) do
    {[r_nfa_state(no: f, edges: [{:epsilon, n}]) | nFA], n + 1, n}
  end

  defp build_nfa_lit(cs, n0, f0, nFA0) do
    foldl(
      fn c, {nFA, n, f} ->
        {[r_nfa_state(no: f, edges: [{[{c, c}], n}]) | nFA], n + 1, n}
      end,
      {nFA0, n0, f0},
      cs
    )
  end

  defp build_nfa_seq(rEs, n0, f0, nFA0) do
    foldl(
      fn rE, {nFA, n, f} ->
        build_nfa(rE, n, f, nFA)
      end,
      {nFA0, n0, f0},
      rEs
    )
  end

  defp build_nfa_alt([rE], n, f, nFA) do
    build_nfa(rE, n, f, nFA)
  end

  defp build_nfa_alt([rE | rEs], n0, f, nFA0) do
    {nFA1, n1, e1} = build_nfa(rE, n0 + 1, n0, nFA0)
    {nFA2, n2, e2} = build_nfa_alt(rEs, n1 + 1, n1, nFA1)
    e = n2

    {[
       [
         r_nfa_state(no: f, edges: [{:epsilon, n0}, {:epsilon, n1}]),
         r_nfa_state(no: e1, edges: [{:epsilon, e}]),
         r_nfa_state(no: e2, edges: [{:epsilon, e}])
       ]
       | nFA2
     ], n2 + 1, e}
  end

  defp pack_cc(cc) do
    crs =
      foldl(
        fn
          {:range, cf, cl}, set ->
            add_element({cf, cl}, set)

          c, set ->
            add_element({c, c}, set)
        end,
        :ordsets.new(),
        cc
      )

    pack_crs(crs)
  end

  defp pack_crs([[{c1, c2} = cr, {c3, c4}] | crs])
       when c1 <= c3 and c2 >= c4 do
    pack_crs([cr | crs])
  end

  defp pack_crs([[{c1, c2}, {c3, c4}] | crs])
       when c2 >= c3 and
              c2 < c4 do
    pack_crs([{c1, c4} | crs])
  end

  defp pack_crs([[{c1, c2}, {c3, c4}] | crs])
       when c2 + 1 === c3 do
    pack_crs([{c1, c4} | crs])
  end

  defp pack_crs([cr | crs]) do
    [cr | pack_crs(crs)]
  end

  defp pack_crs([]) do
    []
  end

  defp comp_class(cc) do
    crs = pack_cc(cc)
    comp = comp_crs(crs, 0)
    comp
  end

  defp comp_crs([{0, c2} | crs], 0) do
    comp_crs(crs, c2 + 1)
  end

  defp comp_crs([{c1, c2} | crs], last) do
    [{last, c1 - 1} | comp_crs(crs, c2 + 1)]
  end

  defp comp_crs([], last) do
    [{last, :maxchar}]
  end

  defp build_dfa(nFA, nf) do
    d = r_dfa_state(no: 0, nfa: eclosure([nf], nFA))
    {build_dfa([d], 1, [], nFA), 0}
  end

  defp build_dfa([u | us0], n0, ms, nFA) do
    {ts, us1, n1} = build_dfa(r_dfa_state(u, :nfa), us0, n0, [], [u | ms], nFA)
    m = r_dfa_state(u, trans: ts, accept: accept(r_dfa_state(u, :nfa), nFA))
    build_dfa(us1, n1, [m | ms], nFA)
  end

  defp build_dfa([], _, ms, _) do
    ms
  end

  defp build_dfa(set, us, n, ts, ms, nFA) do
    crs0 =
      for s <- set,
          {crs, _St} <- r_nfa_state(:erlang.element(s, nFA), :edges),
          crs !== :epsilon,
          cr <- crs do
        cr
      end

    crs1 = :lists.usort(crs0)
    test = disjoint_crs(crs1)
    build_dfa(test, set, us, n, ts, ms, nFA)
  end

  defp disjoint_crs([[{_C1, c2} = cr1, {c3, _C4} = cr2] | crs])
       when c2 < c3 do
    [cr1 | disjoint_crs([cr2 | crs])]
  end

  defp disjoint_crs([[{c1, c2}, {c3, c4}] | crs]) when c1 === c3 do
    [
      {c1, c2}
      | disjoint_crs(
          add_element(
            {c2 + 1, c4},
            crs
          )
        )
    ]
  end

  defp disjoint_crs([[{c1, c2}, {c3, c4}] | crs])
       when c1 < c3 and
              c2 >= c3 and c2 < c4 do
    [
      {c1, c3 - 1}
      | disjoint_crs(
          union(
            [{c3, c2}, {c2 + 1, c4}],
            crs
          )
        )
    ]
  end

  defp disjoint_crs([[{c1, c2}, {c3, c4}] | crs])
       when c1 < c3 and
              c2 === c4 do
    [
      {c1, c3 - 1}
      | disjoint_crs(
          add_element(
            {c3, c4},
            crs
          )
        )
    ]
  end

  defp disjoint_crs([[{c1, c2}, {c3, c4}] | crs])
       when c1 < c3 and
              c2 > c4 do
    [
      {c1, c3 - 1}
      | disjoint_crs(
          union(
            [{c3, c4}, {c4 + 1, c2}],
            crs
          )
        )
    ]
  end

  defp disjoint_crs([cr | crs]) do
    [cr | disjoint_crs(crs)]
  end

  defp disjoint_crs([]) do
    []
  end

  defp build_dfa([cr | crs], set, us, n, ts, ms, nFA) do
    case eclosure(move(set, cr, nFA), nFA) do
      s when s !== [] ->
        case dfa_state_exist(s, us, ms) do
          {:yes, t} ->
            build_dfa(crs, set, us, n, store(cr, t, ts), ms, nFA)

          :no ->
            u = r_dfa_state(no: n, nfa: s)
            build_dfa(crs, set, [u | us], n + 1, store(cr, n, ts), ms, nFA)
        end

      [] ->
        build_dfa(crs, set, us, n, ts, ms, nFA)
    end
  end

  defp build_dfa([], _, us, n, ts, _, _) do
    {ts, us, n}
  end

  defp dfa_state_exist(s, us, ms) do
    case :lists.keyfind(s, r_dfa_state(:nfa), us) do
      r_dfa_state(no: t) ->
        {:yes, t}

      false ->
        case :lists.keyfind(s, r_dfa_state(:nfa), ms) do
          r_dfa_state(no: t) ->
            {:yes, t}

          false ->
            :no
        end
    end
  end

  defp eclosure(sts, nFA) do
    eclosure(sts, nFA, [])
  end

  defp eclosure([st | sts], nFA, ec) do
    r_nfa_state(edges: es) = :erlang.element(st, nFA)

    eclosure(
      for {:epsilon, n} <- es,
          not is_element(n, ec) do
        n
      end ++ sts,
      nFA,
      add_element(st, ec)
    )
  end

  defp eclosure([], _, ec) do
    ec
  end

  defp move(sts, cr, nFA) do
    for n <- sts,
        {crs, st} <- r_nfa_state(:erlang.element(n, nFA), :edges),
        crs !== :epsilon,
        in_crs(cr, crs) do
      st
    end
  end

  defp in_crs({c1, c2}, [{c3, c4} | _Crs])
       when c1 >= c3 and
              c2 <= c4 do
    true
  end

  defp in_crs(cr, [cr | _Crs]) do
    true
  end

  defp in_crs(cr, [_ | crs]) do
    in_crs(cr, crs)
  end

  defp in_crs(_Cr, []) do
    false
  end

  defp accept([st | sts], nFA) do
    case :erlang.element(st, nFA) do
      r_nfa_state(accept: {:accept, a}) ->
        {:accept, a}

      r_nfa_state(accept: :noaccept) ->
        accept(sts, nFA)
    end
  end

  defp accept([], _) do
    :noaccept
  end

  defp minimise_dfa(dFA0, df0) do
    case min_dfa(dFA0) do
      {dFA1, []} ->
        {dFA2, rs} = pack_dfa(dFA1)
        {min_update(dFA2, rs), min_use(df0, rs)}

      {dFA1, rs} ->
        minimise_dfa(min_update(dFA1, rs), min_use(df0, rs))
    end
  end

  defp min_dfa(dFA) do
    min_dfa(dFA, [], [])
  end

  defp min_dfa([d | dFA0], rs0, mDFA) do
    {dFA1, rs1} =
      min_delete(
        dFA0,
        r_dfa_state(d, :trans),
        r_dfa_state(d, :accept),
        r_dfa_state(d, :no),
        rs0,
        []
      )

    min_dfa(dFA1, rs1, [d | mDFA])
  end

  defp min_dfa([], rs, mDFA) do
    {mDFA, rs}
  end

  defp min_delete([r_dfa_state(no: n, trans: t, accept: a) | dFA], t, a, newN, rs, mDFA) do
    min_delete(dFA, t, a, newN, [{n, newN} | rs], mDFA)
  end

  defp min_delete([d | dFA], t, a, newN, rs, mDFA) do
    min_delete(dFA, t, a, newN, rs, [d | mDFA])
  end

  defp min_delete([], _, _, _, rs, mDFA) do
    {mDFA, rs}
  end

  defp min_update(dFA, rs) do
    for d <- dFA do
      r_dfa_state(d, trans: min_update_trans(r_dfa_state(d, :trans), rs))
    end
  end

  defp min_update_trans(tr, rs) do
    for {c, s} <- tr do
      {c, min_use(s, rs)}
    end
  end

  defp min_use(old, [{old, new} | _]) do
    new
  end

  defp min_use(old, [_ | reds]) do
    min_use(old, reds)
  end

  defp min_use(old, []) do
    old
  end

  defp pack_dfa(dFA) do
    pack_dfa(dFA, 0, [], [])
  end

  defp pack_dfa([d | dFA], newN, rs, pDFA) do
    pack_dfa(dFA, newN + 1, [{r_dfa_state(d, :no), newN} | rs], [r_dfa_state(d, no: newN) | pDFA])
  end

  defp pack_dfa([], _, rs, pDFA) do
    {pDFA, rs}
  end

  defp out_file(st0, dFA, dF, actions, code) do
    verbose_print(st0, 'Writing file ~ts, ', [r_leex(st0, :efile)])

    case open_inc_file(st0) do
      {:ok, ifile} ->
        try do
          case :file.open(r_leex(st0, :efile), [:write]) do
            {:ok, ofile} ->
              set_encoding(st0, ofile)

              try do
                output_encoding_comment(ofile, st0)
                output_file_directive(ofile, r_leex(st0, :ifile), 0)
                out_file(ifile, ofile, st0, dFA, dF, actions, code, 1)
                verbose_print(st0, 'ok~n', [])
                st0
              after
                :ok = :file.close(ofile)
              end

            {:error, error} ->
              verbose_print(st0, 'error~n', [])
              add_error({:none, :leex, {:file_error, error}}, st0)
          end
        after
          :ok = :file.close(ifile)
        end

      {{:error, error}, ifile} ->
        add_error(ifile, {:none, :leex, {:file_error, error}}, st0)
    end
  end

  defp open_inc_file(state) do
    ifile = r_leex(state, :ifile)

    case :file.open(ifile, [:read]) do
      {:ok, f} ->
        _ = :epp.set_encoding(f)
        {:ok, f}

      error ->
        {error, ifile}
    end
  end

  defp inc_file_name([]) do
    incdir = :filename.join(:code.lib_dir(:parsetools), 'include')
    :filename.join(incdir, 'leexinc.hrl')
  end

  defp inc_file_name(filename) do
    filename
  end

  defp out_file(ifile, ofile, st, dFA, dF, actions, code, l) do
    case :io.get_line(ifile, :leex) do
      :eof ->
        output_file_directive(ofile, r_leex(st, :ifile), l)

      {:error, _} ->
        add_error(r_leex(st, :ifile), {l, :leex, :cannot_parse}, st)

      line ->
        case :string.slice(line, 0, 5) do
          '##mod' ->
            out_module(ofile, st)

          '##cod' ->
            out_erlang_code(ofile, st, code, l)

          '##dfa' ->
            out_dfa(ofile, st, dFA, code, dF, l)

          '##act' ->
            out_actions(ofile, r_leex(st, :xfile), actions)

          _ ->
            :io.put_chars(ofile, line)
        end

        out_file(ifile, ofile, st, dFA, dF, actions, code, l + 1)
    end
  end

  defp out_module(file, st) do
    :io.fwrite(file, '-module(~w).\n', [r_leex(st, :module)])
  end

  defp out_erlang_code(file, st, code, l) do
    {codeL, codePos, _NCodeLines} = code
    output_file_directive(file, r_leex(st, :xfile), codeL)
    {:ok, xfile} = :file.open(r_leex(st, :xfile), [:read])

    try do
      set_encoding(st, xfile)
      {:ok, _} = :file.position(xfile, codePos)
      :ok = file_copy(xfile, file)
    after
      :ok = :file.close(xfile)
    end

    :io.nl(file)
    output_file_directive(file, r_leex(st, :ifile), l)
  end

  defp file_copy(from, to) do
    case :io.get_line(from, :leex) do
      :eof ->
        :ok

      line when is_list(line) ->
        :io.fwrite(to, '~ts', [line])
        file_copy(from, to)
    end
  end

  defp out_dfa(file, st, dFA, code, dF, l) do
    {_CodeL, _CodePos, nCodeLines} = code
    output_file_directive(file, r_leex(st, :efile), l + (nCodeLines - 1) + 3)
    :io.fwrite(file, 'yystate() -> ~w.~n~n', [dF])

    foreach(
      fn s ->
        out_trans(file, s)
      end,
      dFA
    )

    :io.fwrite(file, 'yystate(S, Ics, Line, Tlen, Action, Alen) ->~n', [])
    :io.fwrite(file, '    {Action,Alen,Tlen,Ics,Line,S}.~n', [])
  end

  defp out_trans(
         file,
         r_dfa_state(no: n, trans: [], accept: {:accept, a})
       ) do
    :io.fwrite(file, 'yystate(~w, Ics, Line, Tlen, _, _) ->~n', [n])
    :io.fwrite(file, '    {~w,Tlen,Ics,Line};~n', [a])
  end

  defp out_trans(
         file,
         r_dfa_state(no: n, trans: tr, accept: {:accept, a})
       ) do
    foreach(
      fn t ->
        out_accept_tran(file, n, a, t)
      end,
      pack_trans(tr)
    )

    :io.fwrite(file, 'yystate(~w, Ics, Line, Tlen, _, _) ->~n', [n])
    :io.fwrite(file, '    {~w,Tlen,Ics,Line,~w};~n', [a, n])
  end

  defp out_trans(file, r_dfa_state(no: n, trans: tr, accept: :noaccept)) do
    foreach(
      fn t ->
        out_noaccept_tran(file, n, t)
      end,
      pack_trans(tr)
    )

    :io.fwrite(file, 'yystate(~w, Ics, Line, Tlen, Action, Alen) ->~n', [n])
    :io.fwrite(file, '    {Action,Alen,Tlen,Ics,Line,~w};~n', [n])
  end

  defp out_accept_tran(file, n, a, {{cf, :maxchar}, s}) do
    out_accept_head_max(file, n, cf)
    out_accept_body(file, s, 'Line', a)
  end

  defp out_accept_tran(file, n, a, {{cf, cl}, s}) do
    out_accept_head_range(file, n, cf, cl)
    out_accept_body(file, s, 'Line', a)
  end

  defp out_accept_tran(file, n, a, {?\n, s}) do
    out_accept_head_1(file, n, ?\n)
    out_accept_body(file, s, 'Line+1', a)
  end

  defp out_accept_tran(file, n, a, {c, s}) do
    out_accept_head_1(file, n, c)
    out_accept_body(file, s, 'Line', a)
  end

  defp out_accept_head_1(file, state, char) do
    out_head_1(file, state, char, '_', '_')
  end

  defp out_accept_head_max(file, state, min) do
    out_head_max(file, state, min, '_', '_')
  end

  defp out_accept_head_range(file, state, min, max) do
    out_head_range(file, state, min, max, '_', '_')
  end

  defp out_accept_body(file, next, line, action) do
    out_body(file, next, line, :io_lib.write(action), 'Tlen')
  end

  defp out_noaccept_tran(file, n, {{cf, :maxchar}, s}) do
    out_noaccept_head_max(file, n, cf)
    out_noaccept_body(file, s, 'Line')
  end

  defp out_noaccept_tran(file, n, {{cf, cl}, s}) do
    out_noaccept_head_range(file, n, cf, cl)
    out_noaccept_body(file, s, 'Line')
  end

  defp out_noaccept_tran(file, n, {?\n, s}) do
    out_noaccept_head_1(file, n, ?\n)
    out_noaccept_body(file, s, 'Line+1')
  end

  defp out_noaccept_tran(file, n, {c, s}) do
    out_noaccept_head_1(file, n, c)
    out_noaccept_body(file, s, 'Line')
  end

  defp out_noaccept_head_1(file, state, char) do
    out_head_1(file, state, char, 'Action', 'Alen')
  end

  defp out_noaccept_head_max(file, state, min) do
    out_head_max(file, state, min, 'Action', 'Alen')
  end

  defp out_noaccept_head_range(file, state, min, max) do
    out_head_range(file, state, min, max, 'Action', 'Alen')
  end

  defp out_noaccept_body(file, next, line) do
    out_body(file, next, line, 'Action', 'Alen')
  end

  defp out_head_1(file, state, char, action, alen) do
    :io.fwrite(file, 'yystate(~w, [~w|Ics], Line, Tlen, ~s, ~s) ->\n', [state, char, action, alen])
  end

  defp out_head_max(file, state, min, action, alen) do
    :io.fwrite(file, 'yystate(~w, [C|Ics], Line, Tlen, ~s, ~s) when C >= ~w ->\n', [
      state,
      action,
      alen,
      min
    ])
  end

  defp out_head_range(file, state, min, max, action, alen) do
    :io.fwrite(file, 'yystate(~w, [C|Ics], Line, Tlen, ~s, ~s) when C >= ~w, C =< ~w ->\n', [
      state,
      action,
      alen,
      min,
      max
    ])
  end

  defp out_body(file, next, line, action, alen) do
    :io.fwrite(file, '    yystate(~w, Ics, ~s, Tlen+1, ~s, ~s);\n', [next, line, action, alen])
  end

  defp pack_trans(trs) do
    pack_trans(trs, [])
  end

  defp pack_trans([{{c, c}, s} | trs], pt) do
    pack_trans(trs, [{c, s} | pt])
  end

  defp pack_trans([{{cf, ?\n}, s} | trs], pt) do
    pack_trans([{{cf, ?\n - 1}, s} | trs], [{?\n, s} | pt])
  end

  defp pack_trans([{{?\n, cl}, s} | trs], pt) do
    pack_trans([{{?\n + 1, cl}, s} | trs], [{?\n, s} | pt])
  end

  defp pack_trans([{{cf, cl}, s} | trs], pt)
       when cf < ?\n and
              cl > ?\n do
    pack_trans(
      [
        [{{cf, ?\n - 1}, s}, {{?\n + 1, cl}, s}]
        | trs
      ],
      [{?\n, s} | pt]
    )
  end

  defp pack_trans([{{cf, cl}, s} | trs], pt) when cl === cf + 1 do
    pack_trans(trs, [[{cf, s}, {cl, s}] | pt])
  end

  defp pack_trans([tr | trs], pt) do
    pack_trans(trs, pt ++ [tr])
  end

  defp pack_trans([], pt) do
    pt
  end

  defp out_actions(file, xrlFile, as) do
    as1 = prep_out_actions(as)

    foreach(
      fn a ->
        out_action(file, a)
      end,
      as1
    )

    :io.fwrite(file, 'yyaction(_, _, _, _) -> error.~n', [])

    foreach(
      fn a ->
        out_action_code(file, xrlFile, a)
      end,
      as1
    )
  end

  defp prep_out_actions(as) do
    map(
      fn
        {a, :empty_action} ->
          {a, :empty_action}

        {a, code, tokenChars, tokenLen, tokenLine} ->
          vs = [
            {tokenChars, 'TokenChars'},
            {tokenLen, 'TokenLen'},
            {tokenLine, 'TokenLine'},
            {tokenChars, 'YYtcs'},
            {:erlang.or(tokenLen, tokenChars), 'TokenLen'}
          ]

          vars =
            for {f, s} <- vs do
              cond do
                f ->
                  s

                true ->
                  '_'
              end
            end

          name = :erlang.list_to_atom(:lists.concat([:yyaction_, a]))
          [chars, len, line, _, _] = vars

          args =
            for v <- [chars, len, line], v !== '_' do
              v
            end

          argsChars = :lists.join(', ', args)
          {a, code, vars, name, args, argsChars}
      end,
      as
    )
  end

  defp out_action(file, {a, :empty_action}) do
    :io.fwrite(file, 'yyaction(~w, _, _, _) -> skip_token;~n', [a])
  end

  defp out_action(
         file,
         {a, _Code, vars, name, _Args, argsChars}
       ) do
    [_, _, line, tcs, len] = vars
    :io.fwrite(file, 'yyaction(~w, ~s, ~s, ~s) ->~n', [a, len, tcs, line])

    cond do
      tcs !== '_' ->
        :io.fwrite(file, '    TokenChars = yypre(YYtcs, TokenLen),~n', [])

      true ->
        :ok
    end

    :io.fwrite(file, '    ~s(~s);~n', [name, argsChars])
  end

  defp out_action_code(_File, _XrlFile, {_A, :empty_action}) do
    :ok
  end

  defp out_action_code(file, xrlFile, {_A, code, _Vars, name, args, argsChars}) do
    :io.fwrite(file, '\n-compile({inline,~w/~w}).\n', [name, length(args)])
    l = :erl_scan.line(hd(code))
    output_file_directive(file, xrlFile, l - 2)
    :io.fwrite(file, '~s(~s) ->~n', [name, argsChars])
    :io.fwrite(file, '    ~ts\n', [pp_tokens(code, l, file)])
  end

  defp pp_tokens(tokens, line0, file) do
    pp_tokens(tokens, line0, file, :none)
  end

  defp pp_tokens([], _Line0, _, _) do
    []
  end

  defp pp_tokens([t | ts], line0, file, prev) do
    line = :erl_scan.line(t)

    [
      [pp_sep(line, line0, prev, t), pp_symbol(t, file)]
      | pp_tokens(ts, line, file, t)
    ]
  end

  defp pp_symbol({:var, _, var}, _) do
    :erlang.atom_to_list(var)
  end

  defp pp_symbol({_, _, symbol}, file) do
    format_symbol(symbol, file)
  end

  defp pp_symbol({:dot, _}, _) do
    '.'
  end

  defp pp_symbol({symbol, _}, _) do
    :erlang.atom_to_list(symbol)
  end

  defp pp_sep(line, line0, prev, t) when line > line0 do
    ['\n    ' | pp_sep(line - 1, line0, prev, t)]
  end

  defp pp_sep(_, _, {:., _}, _) do
    ''
  end

  defp pp_sep(_, _, {:"#", _}, _) do
    ''
  end

  defp pp_sep(_, _, {:"(", _}, _) do
    ''
  end

  defp pp_sep(_, _, {:"[", _}, _) do
    ''
  end

  defp pp_sep(_, _, _, {:., _}) do
    ''
  end

  defp pp_sep(_, _, _, {:"#", _}) do
    ''
  end

  defp pp_sep(_, _, _, {:",", _}) do
    ''
  end

  defp pp_sep(_, _, _, {:")", _}) do
    ''
  end

  defp pp_sep(_, _, _, _) do
    ' '
  end

  defp out_dfa_graph(st, dFA, dF) do
    verbose_print(st, 'Writing DFA to file ~ts, ', [r_leex(st, :gfile)])

    case :file.open(r_leex(st, :gfile), [:write]) do
      {:ok, gfile} ->
        try do
          set_encoding(st, gfile)
          :io.fwrite(gfile, 'digraph DFA {~n', [])
          out_dfa_states(gfile, dFA, dF)
          out_dfa_edges(gfile, dFA)
          :io.fwrite(gfile, '}~n', [])
          verbose_print(st, 'ok~n', [])
          st
        after
          :ok = :file.close(gfile)
        end

      {:error, error} ->
        verbose_print(st, 'error~n', [])
        add_error({:none, :leex, {:file_error, error}}, st)
    end
  end

  defp out_dfa_states(file, dFA, dF) do
    foreach(
      fn s ->
        out_dfa_state(file, dF, s)
      end,
      dFA
    )

    :io.fwrite(file, '~n', [])
  end

  defp out_dfa_state(file, dF, r_dfa_state(no: dF, accept: {:accept, _})) do
    :io.fwrite(file, '  ~b [shape=doublecircle color=green];~n', [dF])
  end

  defp out_dfa_state(file, dF, r_dfa_state(no: dF, accept: :noaccept)) do
    :io.fwrite(file, '  ~b [shape=circle color=green];~n', [dF])
  end

  defp out_dfa_state(file, _, r_dfa_state(no: s, accept: {:accept, _})) do
    :io.fwrite(file, '  ~b [shape=doublecircle];~n', [s])
  end

  defp out_dfa_state(file, _, r_dfa_state(no: s, accept: :noaccept)) do
    :io.fwrite(file, '  ~b [shape=circle];~n', [s])
  end

  defp out_dfa_edges(file, dFA) do
    foreach(
      fn r_dfa_state(no: s, trans: trans) ->
        pt = pack_trans(trans)

        tdict =
          foldl(
            fn {cr, t}, d ->
              :orddict.append(t, cr, d)
            end,
            :orddict.new(),
            pt
          )

        foreach(
          fn t ->
            crs = :orddict.fetch(t, tdict)
            edgelab = dfa_edgelabel(crs, file)
            :io.fwrite(file, '  ~b -> ~b [label="~ts"];~n', [s, t, edgelab])
          end,
          sort(:orddict.fetch_keys(tdict))
        )
      end,
      dFA
    )
  end

  defp dfa_edgelabel([c], file) when is_integer(c) do
    quote(c, file)
  end

  defp dfa_edgelabel(cranges, file) do
    '[' ++
      map(
        fn
          {a, b} ->
            [quote(a, file), '-', quote(b, file)]

          c ->
            [quote(c, file)]
        end,
        cranges
      ) ++ ']'
  end

  defp set_encoding(r_leex(encoding: :none), file) do
    :ok =
      :io.setopts(
        file,
        [{:encoding, :epp.default_encoding()}]
      )
  end

  defp set_encoding(r_leex(encoding: e), file) do
    :ok = :io.setopts(file, [{:encoding, e}])
  end

  defp output_encoding_comment(_File, r_leex(encoding: :none)) do
    :ok
  end

  defp output_encoding_comment(file, r_leex(encoding: encoding)) do
    :io.fwrite(file, "%% ~s\n", [:epp.encoding_to_string(encoding)])
  end

  defp output_file_directive(file, filename, line) do
    :io.fwrite(file, "-file(~ts, ~w).\n", [format_filename(filename, file), line])
  end

  defp format_filename(filename0, file) do
    filename = :filename.flatten(filename0)

    case enc(file) do
      :unicode ->
        :io_lib.write_string(filename)

      :latin1 ->
        :io_lib.write_string_as_latin1(filename)
    end
  end

  defp format_symbol(symbol, file) do
    format =
      case enc(file) do
        :latin1 ->
          '~p'

        :unicode ->
          '~tp'
      end

    :io_lib.fwrite(format, [symbol])
  end

  defp enc(file) do
    case :lists.keyfind(:encoding, 1, :io.getopts(file)) do
      false ->
        :latin1

      {:encoding, enc} ->
        enc
    end
  end

  defp quote(?^, _File) do
    '\\^'
  end

  defp quote(?., _File) do
    '\\.'
  end

  defp quote(?$, _File) do
    '\\$'
  end

  defp quote(?-, _File) do
    '\\-'
  end

  defp quote(?[, _File) do
    '\\['
  end

  defp quote(?], _File) do
    '\\]'
  end

  defp quote(?\s, _File) do
    '\\\\s'
  end

  defp quote(?", _File) do
    '\\"'
  end

  defp quote(?\b, _File) do
    '\\\\b'
  end

  defp quote(?\f, _File) do
    '\\\\f'
  end

  defp quote(?\n, _File) do
    '\\\\n'
  end

  defp quote(?\r, _File) do
    '\\\\r'
  end

  defp quote(?\t, _File) do
    '\\\\t'
  end

  defp quote(?\e, _File) do
    '\\\\e'
  end

  defp quote(?\v, _File) do
    '\\\\v'
  end

  defp quote(?\d, _File) do
    '\\\\d'
  end

  defp quote(?\\, _File) do
    '\\\\'
  end

  defp quote(c, file) when is_integer(c) do
    s =
      case enc(file) do
        :unicode ->
          :io_lib.write_char(c)

        :latin1 ->
          :io_lib.write_char_as_latin1(c)
      end

    case s do
      [[?$, ?\\] | cs] ->
        '\\\\' ++ cs

      [?$ | cs] ->
        cs
    end
  end

  defp quote(:maxchar, _File) do
    'MAXCHAR'
  end
end
