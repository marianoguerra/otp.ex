defmodule :m_yecc do
  use Bitwise

  import :lists,
    only: [
      append: 1,
      append: 2,
      concat: 1,
      delete: 2,
      filter: 2,
      flatmap: 2,
      foldl: 3,
      foldr: 3,
      foreach: 2,
      keydelete: 3,
      keysort: 2,
      last: 1,
      map: 2,
      member: 2,
      reverse: 1,
      sort: 1,
      usort: 1
    ]

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

  Record.defrecord(:r_yecc, :yecc,
    infile: :undefined,
    outfile: :undefined,
    includefile: :undefined,
    includefile_version: :undefined,
    module: :undefined,
    encoding: :none,
    options: [],
    verbose: false,
    file_attrs: true,
    errors: [],
    warnings: [],
    conflicts_done: false,
    shift_reduce: [],
    reduce_reduce: [],
    n_states: 0,
    inport: :undefined,
    outport: :undefined,
    line: :undefined,
    parse_actions: :undefined,
    symbol_tab: :undefined,
    inv_symbol_tab: :undefined,
    state_tab: :undefined,
    prec_tab: :undefined,
    goto_tab: :undefined,
    terminals: [],
    nonterminals: [],
    all_symbols: [],
    prec: [],
    rules_list: [],
    rules: :undefined,
    rule_pointer2rule: :undefined,
    rootsymbol: [],
    endsymbol: [],
    expect_shift_reduce: [],
    expect_n_states: [],
    header: [],
    erlang_code: :none
  )

  Record.defrecord(:r_rule, :rule,
    n: :undefined,
    anno: :undefined,
    symbols: :undefined,
    tokens: :undefined,
    is_guard: :undefined,
    is_well_formed: :undefined
  )

  Record.defrecord(:r_reduce, :reduce,
    rule_nmbr: :undefined,
    head: :undefined,
    nmbr_of_daughters: :undefined,
    prec: :undefined,
    unused: :undefined
  )

  Record.defrecord(:r_shift, :shift,
    state: :undefined,
    pos: :undefined,
    prec: :undefined,
    rule_nmbr: :undefined
  )

  Record.defrecord(:r_user_code, :user_code,
    state: :undefined,
    terminal: :undefined,
    funname: :undefined,
    action: :undefined
  )

  Record.defrecord(:r_symbol, :symbol,
    anno: :none,
    name: :undefined
  )

  def compile(
        input0,
        output0,
        r_options(warning: warnLevel, verbose: verbose, includes: includes, specific: specific)
      ) do
    input = shorten_filename(input0)
    output = shorten_filename(output0)
    includefile = :lists.sublist(includes, 1)

    werror =
      :proplists.get_bool(
        :warnings_as_errors,
        specific
      )

    opts = [
      {:parserfile, output},
      {:includefile, includefile},
      {:verbose, verbose},
      {:report_errors, true},
      {:report_warnings, warnLevel > 0},
      {:warnings_as_errors, werror}
    ]

    case file(input, opts) do
      {:ok, _OutFile} ->
        :ok

      :error ->
        :error
    end
  end

  def format_error(:bad_declaration) do
    :io_lib.fwrite('unknown or bad declaration, ignored', [])
  end

  def format_error({:bad_expect, symName}) do
    :io_lib.fwrite('argument ~ts of Expect is not an integer', [format_symbol(symName)])
  end

  def format_error({:bad_rootsymbol, symName}) do
    :io_lib.fwrite('rootsymbol ~ts is not a nonterminal', [format_symbol(symName)])
  end

  def format_error({:bad_states, symName}) do
    :io_lib.fwrite('argument ~ts of States is not an integer', [format_symbol(symName)])
  end

  def format_error({:conflict, conflict}) do
    format_conflict(conflict)
  end

  def format_error({:conflicts, sR, rR}) do
    :io_lib.fwrite('conflicts: ~w shift/reduce, ~w reduce/reduce', [sR, rR])
  end

  def format_error({:duplicate_declaration, tag}) do
    :io_lib.fwrite('duplicate declaration of ~s', [:erlang.atom_to_list(tag)])
  end

  def format_error({:duplicate_nonterminal, nonterminal}) do
    :io_lib.fwrite('duplicate non-terminals ~ts', [format_symbol(nonterminal)])
  end

  def format_error({:duplicate_precedence, op}) do
    :io_lib.fwrite('duplicate precedence operator ~ts', [format_symbol(op)])
  end

  def format_error({:duplicate_terminal, terminal}) do
    :io_lib.fwrite('duplicate terminal ~ts', [format_symbol(terminal)])
  end

  def format_error({:endsymbol_is_nonterminal, symbol}) do
    :io_lib.fwrite('endsymbol ~ts is a nonterminal', [format_symbol(symbol)])
  end

  def format_error({:endsymbol_is_terminal, symbol}) do
    :io_lib.fwrite('endsymbol ~ts is a terminal', [format_symbol(symbol)])
  end

  def format_error({:error, module, error}) do
    module.format_error(error)
  end

  def format_error({:file_error, reason}) do
    :io_lib.fwrite('~ts', [:file.format_error(reason)])
  end

  def format_error(:illegal_empty) do
    :io_lib.fwrite('illegal use of empty symbol', [])
  end

  def format_error({:internal_error, error}) do
    :io_lib.fwrite('internal yecc error: ~w', [error])
  end

  def format_error({:missing_syntax_rule, nonterminal}) do
    :io_lib.fwrite('no syntax rule for non-terminal symbol ~ts', [format_symbol(nonterminal)])
  end

  def format_error({:n_states, exp, n}) do
    :io_lib.fwrite('expected ~w states, but got ~p states', [exp, n])
  end

  def format_error(:no_grammar_rules) do
    :io_lib.fwrite('grammar rules are missing', [])
  end

  def format_error(:nonterminals_missing) do
    :io_lib.fwrite('Nonterminals is missing', [])
  end

  def format_error({:precedence_op_is_endsymbol, symName}) do
    :io_lib.fwrite('precedence operator ~ts is endsymbol', [format_symbol(symName)])
  end

  def format_error({:precedence_op_is_unknown, symName}) do
    :io_lib.fwrite('unknown precedence operator ~ts', [format_symbol(symName)])
  end

  def format_error({:reserved, n}) do
    :io_lib.fwrite('the use of ~w should be avoided', [n])
  end

  def format_error({:symbol_terminal_and_nonterminal, symName}) do
    :io_lib.fwrite('symbol ~ts is both a terminal and nonterminal', [format_symbol(symName)])
  end

  def format_error(:rootsymbol_missing) do
    :io_lib.fwrite('Rootsymbol is missing', [])
  end

  def format_error(:terminals_missing) do
    :io_lib.fwrite('Terminals is missing', [])
  end

  def format_error({:undefined_nonterminal, symbol}) do
    :io_lib.fwrite('undefined nonterminal: ~ts', [format_symbol(symbol)])
  end

  def format_error({:undefined_pseudo_variable, atom}) do
    :io_lib.fwrite('undefined pseudo variable ~w', [atom])
  end

  def format_error({:undefined_symbol, symName}) do
    :io_lib.fwrite('undefined rhs symbol ~ts', [format_symbol(symName)])
  end

  def format_error({:unused_nonterminal, nonterminal}) do
    :io_lib.fwrite('non-terminal symbol ~ts not used', [format_symbol(nonterminal)])
  end

  def format_error({:unused_terminal, terminal}) do
    :io_lib.fwrite('terminal symbol ~ts not used', [format_symbol(terminal)])
  end

  def format_error({:bad_symbol, string}) do
    :io_lib.fwrite('bad symbol ~ts', [string])
  end

  def format_error(:cannot_parse) do
    :io_lib.fwrite('cannot parse; possibly encoding mismatch', [])
  end

  def file(file) do
    file(file, [:report_errors, :report_warnings])
  end

  def file(file, options) do
    case is_filename(file) do
      :no ->
        :erlang.error(:badarg, [file, options])

      _ ->
        :ok
    end

    case options(options) do
      :badarg ->
        :erlang.error(:badarg, [file, options])

      optionValues ->
        self = self()
        flag = :erlang.process_flag(:trap_exit, false)

        pid =
          spawn_link(fn ->
            infile(self, file, optionValues)
          end)

        receive do
          {^pid, rep} ->
            receive do
            after
              1 ->
                :ok
            end

            :erlang.process_flag(:trap_exit, flag)
            rep
        end
    end
  end

  def yecc(infile, outfile) do
    yecc(infile, outfile, false, [])
  end

  def yecc(infile, outfile, verbose) do
    yecc(infile, outfile, verbose, [])
  end

  def yecc(infilex, outfilex, verbose, includefilex) do
    _ = :erlang.statistics(:runtime)

    case file(
           infilex,
           [
             {:parserfile, outfilex},
             {:verbose, verbose},
             {:report, true},
             {:includefile, includefilex}
           ]
         ) do
      {:ok, _File} ->
        :erlang.statistics(:runtime)

      :error ->
        exit(:error)
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
          :file_attributes,
          :includefile,
          :parserfile,
          :report_errors,
          :report_warnings,
          :warnings_as_errors,
          :return_errors,
          :return_warnings,
          :time,
          :verbose
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
               key === :parserfile ->
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

  defp default_option(:file_attributes) do
    true
  end

  defp default_option(:includefile) do
    []
  end

  defp default_option(:parserfile) do
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

  defp default_option(:time) do
    false
  end

  defp default_option(:verbose) do
    false
  end

  defp atom_option(:file_attributes) do
    {:file_attributes, true}
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

  defp atom_option(:time) do
    {:time, true}
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

  defp start(infilex, options) do
    infile = assure_extension(infilex, '.yrl')
    {_, outfilex0} = :lists.keyfind(:parserfile, 1, options)
    {_, includefilex} = :lists.keyfind(:includefile, 1, options)

    outfilex =
      case outfilex0 do
        [] ->
          :filename.rootname(infilex, '.yrl')

        _ ->
          outfilex0
      end

    includefile =
      case includefilex do
        [] ->
          []

        _ ->
          assure_extension(includefilex, '.hrl')
      end

    includefileVersion = includefile_version(includefile)
    outfile = assure_extension(outfilex, '.erl')

    module =
      :erlang.list_to_atom(
        :filename.basename(
          outfile,
          '.erl'
        )
      )

    r_yecc(
      infile: infile,
      outfile: outfile,
      includefile: includefile,
      includefile_version: includefileVersion,
      module: module,
      options: options,
      verbose: member(:verbose, options),
      file_attrs: member(:file_attributes, options)
    )
  end

  defp assure_extension(file, ext) do
    concat([strip_extension(file, ext), ext])
  end

  defp strip_extension(file, ext) do
    case :filename.extension(file) do
      ^ext ->
        :filename.rootname(file)

      _Other ->
        file
    end
  end

  defp infile(parent, infilex, options) do
    st0 = start(infilex, options)

    st =
      case :file.open(
             r_yecc(st0, :infile),
             [:read, :read_ahead]
           ) do
        {:ok, inport} ->
          try do
            encoding = :epp.set_encoding(inport)
            st1 = r_yecc(st0, inport: inport, encoding: encoding)
            outfile(st1)
          after
            :ok = :file.close(inport)
          end

        {:error, reason} ->
          add_error(r_yecc(st0, :infile), :none, {:file_error, reason}, st0)
      end

    case {r_yecc(st, :errors), werror(st)} do
      {[], false} ->
        :ok

      _ ->
        _ = :file.delete(r_yecc(st, :outfile))
        :ok
    end

    send(parent, {self(), yecc_ret(st)})
  end

  defp werror(st) do
    r_yecc(st, :warnings) !== [] and
      member(
        :warnings_as_errors,
        r_yecc(st, :options)
      )
  end

  defp outfile(st0) do
    case :file.open(
           r_yecc(st0, :outfile),
           [:write, :delayed_write]
         ) do
      {:ok, outport} ->
        try do
          set_encoding(st0, outport)
          generate(r_yecc(st0, outport: outport, line: 1))
        catch
          st1 ->
            st1

          :exit, reason ->
            add_error({:internal_error, reason}, st0)
        after
          :ok = :file.close(outport)
        end

      {:error, reason} ->
        add_error(r_yecc(st0, :outfile), :none, {:file_error, reason}, st0)
    end
  end

  defp os_process_size() do
    case :os.type() do
      {:unix, :sunos} ->
        size = :os.cmd('ps -o vsz -p ' ++ :os.getpid() ++ ' | tail -1')
        :erlang.list_to_integer(nonl(size))

      _ ->
        0
    end
  end

  defp nonl([?\n]) do
    []
  end

  defp nonl([]) do
    []
  end

  defp nonl([h | t]) do
    [h | nonl(t)]
  end

  defp timeit(name, fun, st0) do
    time = :runtime
    {before, _} = :erlang.statistics(time)
    st = fun.(st0)
    {after__, _} = :erlang.statistics(time)
    mem0 = :erts_debug.flat_size(st) * :erlang.system_info(:wordsize)
    mem = :lists.flatten(:io_lib.format('~.1f kB', [mem0 / 1024]))

    sz =
      :lists.flatten(
        :io_lib.format(
          '~.1f MB',
          [os_process_size() / 1024]
        )
      )

    :io.fwrite(
      ' ~-30w: ~10.2f s ~12s ~10s\n',
      [name, (after__ - before) / 1000, mem, sz]
    )

    st
  end

  defp generate(st0) do
    st1 = output_encoding_comment(st0)

    passes = [
      {:parse_grammar, &parse_grammar/1},
      {:check_grammar, &check_grammar/1},
      {:states_and_goto_table, &states_and_goto_table/1},
      {:parse_actions, &parse_actions/1},
      {:action_conflicts, &action_conflicts/1},
      {:write_file, &write_file/1}
    ]

    f =
      case member(:time, r_yecc(st1, :options)) do
        true ->
          :io.fwrite("Generating parser from grammar in ~ts\n", [
            format_filename(r_yecc(st1, :infile), st1)
          ])

          &timeit/3

        false ->
          fn _Name, fn__, st ->
            fn__.(st)
          end
      end

    fun = fn {name, fun}, st ->
      st2 = f.(name, fun, st)

      cond do
        r_yecc(st2, :errors) === [] ->
          st2

        true ->
          throw(st2)
      end
    end

    foldl(fun, st1, passes)
  end

  defp parse_grammar(st) do
    parse_grammar(r_yecc(st, :inport), 1, st)
  end

  defp parse_grammar(inport, line, st) do
    {nextLine, grammar} = read_grammar(inport, st, line)
    parse_grammar(grammar, inport, nextLine, st)
  end

  defp parse_grammar(:eof, _Inport, _NextLine, st) do
    st
  end

  defp parse_grammar({r_symbol(name: :Header), ss}, inport, nextLine, st0) do
    st1 =
      r_yecc(st0,
        header:
          for {:string, _, s} <- ss do
            s
          end
      )

    parse_grammar(inport, nextLine, st1)
  end

  defp parse_grammar({r_symbol(name: :Erlang), [r_symbol(name: :code)]}, _Inport, nextLine, st) do
    r_yecc(st, erlang_code: nextLine)
  end

  defp parse_grammar(grammar, inport, nextLine, st0) do
    st = parse_grammar(grammar, st0)
    parse_grammar(inport, nextLine, st)
  end

  defp parse_grammar({:error, errorLine, error}, st) do
    add_error(:erl_anno.new(errorLine), error, st)
  end

  defp parse_grammar({:rule, rule, tokens}, st0) do
    nmbrOfDaughters =
      case rule do
        [_, r_symbol(name: :"$empty")] ->
          0

        _ ->
          length(rule) - 1
      end

    {isGuard, isWellFormed} = check_action(tokens)
    {tokens1, st} = subst_pseudo_vars(tokens, nmbrOfDaughters, st0)

    ruleDef =
      r_rule(symbols: rule, tokens: tokens1, is_guard: isGuard, is_well_formed: isWellFormed)

    r_yecc(st, rules_list: [ruleDef | r_yecc(st, :rules_list)])
  end

  defp parse_grammar({:prec, prec}, st) do
    r_yecc(st, prec: prec ++ r_yecc(st, :prec))
  end

  defp parse_grammar({r_symbol(), [{:string, anno, string}]}, st) do
    add_error(anno, {:bad_symbol, string}, st)
  end

  defp parse_grammar({r_symbol(anno: anno, name: name), symbols}, st) do
    cF = fn i ->
      case :erlang.element(i, st) do
        [] ->
          :erlang.setelement(i, st, symbols)

        _ ->
          add_error(anno, {:duplicate_declaration, name}, st)
      end
    end

    oneSymbol = length(symbols) === 1

    case name do
      :Nonterminals ->
        cF.(r_yecc(:nonterminals))

      :Terminals ->
        cF.(r_yecc(:terminals))

      :Rootsymbol when oneSymbol ->
        cF.(r_yecc(:rootsymbol))

      :Endsymbol when oneSymbol ->
        cF.(r_yecc(:endsymbol))

      :Expect when oneSymbol ->
        cF.(r_yecc(:expect_shift_reduce))

      :States when oneSymbol ->
        cF.(r_yecc(:expect_n_states))

      _ ->
        add_warning(anno, :bad_declaration, st)
    end
  end

  defp read_grammar(inport, st, line) do
    case :yeccscan.scan(inport, :"", line) do
      {:eof, nextLine} ->
        {nextLine, :eof}

      {:error, {errorLine, mod, what}, nextLine} ->
        {nextLine, {:error, errorLine, {:error, mod, what}}}

      {:error, :terminated} ->
        throw(st)

      {:error, _} ->
        file = r_yecc(st, :infile)
        throw(add_error(file, :none, :cannot_parse, st))

      {:ok, input, nextLine} ->
        {nextLine,
         case :yeccparser.parse(input) do
           {:error, {errorLine, mod, message}} ->
             {:error, errorLine, {:error, mod, message}}

           {:ok, {:rule, rule, {:erlang_code, tokens}}} ->
             {:rule, rule, tokens}

           {:ok, {r_symbol(name: p), [r_symbol(name: i) | opL]} = ss} ->
             a = precedence(p)

             cond do
               a !== :unknown and is_integer(i) and opL !== [] ->
                 ps =
                   for op <- opL do
                     {op, i, a}
                   end

                 {:prec, ps}

               true ->
                 ss
             end

           {:ok, ss} ->
             ss
         end}
    end
  end

  defp precedence(:Left) do
    :left
  end

  defp precedence(:Right) do
    :right
  end

  defp precedence(:Unary) do
    :unary
  end

  defp precedence(:Nonassoc) do
    :nonassoc
  end

  defp precedence(_) do
    :unknown
  end

  defp check_grammar(st0) do
    empty = r_symbol(anno: :none, name: :"$empty")
    allSymbols = r_yecc(st0, :nonterminals) ++ r_yecc(st0, :terminals) ++ [empty]
    st1 = r_yecc(st0, all_symbols: allSymbols)

    cs = [
      &check_nonterminals/1,
      &check_terminals/1,
      &check_rootsymbol/1,
      &check_endsymbol/1,
      &check_expect/1,
      &check_states/1,
      &check_precedences/1,
      &check_rules/1
    ]

    foldl(
      fn f, st ->
        f.(st)
      end,
      st1,
      cs
    )
  end

  defp check_nonterminals(st) do
    case r_yecc(st, :nonterminals) do
      [] ->
        add_error(:nonterminals_missing, st)

      nonterminals ->
        {unique, dups} = duplicates(names(nonterminals))
        st1 = add_warnings(dups, :duplicate_nonterminal, st)
        st2 = check_reserved(unique, st1)
        r_yecc(st2, nonterminals: [{} | unique])
    end
  end

  defp check_terminals(st0) do
    case r_yecc(st0, :terminals) do
      [] ->
        add_error(:terminals_missing, st0)

      terminals ->
        {unique, dups} = duplicates(names(terminals))
        st1 = add_warnings(dups, :duplicate_terminal, st0)
        common = intersect(r_yecc(st1, :nonterminals), unique)
        st2 = add_errors(common, :symbol_terminal_and_nonterminal, st1)
        st3 = check_reserved(unique, st2)
        r_yecc(st3, terminals: [:"$empty" | unique])
    end
  end

  defp check_reserved(names, st) do
    add_errors(intersect(names, [:"$empty", :"$end", :"$undefined"]), :reserved, st)
  end

  defp check_rootsymbol(st) do
    case r_yecc(st, :rootsymbol) do
      [] ->
        add_error(:rootsymbol_missing, st)

      [r_symbol(anno: anno, name: symName)] ->
        case kind_of_symbol(st, symName) do
          :nonterminal ->
            r_yecc(st, rootsymbol: symName)

          _ ->
            add_error(anno, {:bad_rootsymbol, symName}, st)
        end
    end
  end

  defp check_endsymbol(st) do
    case r_yecc(st, :endsymbol) do
      [] ->
        r_yecc(st, endsymbol: :"$end")

      [r_symbol(anno: anno, name: symName)] ->
        case kind_of_symbol(st, symName) do
          :nonterminal ->
            add_error(anno, {:endsymbol_is_nonterminal, symName}, st)

          :terminal ->
            add_error(anno, {:endsymbol_is_terminal, symName}, st)

          _ ->
            r_yecc(st, endsymbol: symName)
        end
    end
  end

  defp check_expect(st0) do
    case r_yecc(st0, :expect_shift_reduce) do
      [] ->
        r_yecc(st0, expect_shift_reduce: 0)

      [r_symbol(name: expect)] when is_integer(expect) ->
        r_yecc(st0, expect_shift_reduce: expect)

      [r_symbol(anno: anno, name: name)] ->
        st1 = add_error(anno, {:bad_expect, name}, st0)
        r_yecc(st1, expect_shift_reduce: 0)
    end
  end

  defp check_states(st) do
    case r_yecc(st, :expect_n_states) do
      [] ->
        st

      [r_symbol(name: nStates)] when is_integer(nStates) ->
        r_yecc(st, expect_n_states: nStates)

      [r_symbol(anno: anno, name: name)] ->
        add_error(anno, {:bad_states, name}, st)
    end
  end

  defp check_precedences(st0) do
    {st1, _} =
      foldr(
        fn {r_symbol(anno: anno, name: op), _I, _A}, {st, ps} ->
          case member(op, ps) do
            true ->
              {add_error(anno, {:duplicate_precedence, op}, st), ps}

            false ->
              {st, [op | ps]}
          end
        end,
        {st0, []},
        r_yecc(st0, :prec)
      )

    foldl(
      fn {r_symbol(anno: anno, name: op), i, a}, st ->
        case kind_of_symbol(st, op) do
          :endsymbol ->
            add_error(anno, {:precedence_op_is_endsymbol, op}, st)

          :unknown ->
            add_error(anno, {:precedence_op_is_unknown, op}, st)

          _ ->
            r_yecc(st, prec: [{op, i, a} | r_yecc(st, :prec)])
        end
      end,
      r_yecc(st1, prec: []),
      r_yecc(st1, :prec)
    )
  end

  defp check_rule(rule0, {st0, rules}) do
    symbols = r_rule(rule0, :symbols)
    r_symbol(anno: headAnno, name: head) = hd(symbols)

    case member(head, r_yecc(st0, :nonterminals)) do
      false ->
        {add_error(headAnno, {:undefined_nonterminal, head}, st0), rules}

      true ->
        st = check_rhs(tl(symbols), st0)

        rule =
          r_rule(rule0,
            anno: headAnno,
            symbols: names(symbols)
          )

        {st, [rule | rules]}
    end
  end

  defp check_rules(st0) do
    {st, rules0} = foldl(&check_rule/2, {st0, []}, r_yecc(st0, :rules_list))

    case r_yecc(st, :rules_list) do
      [] ->
        add_error(:no_grammar_rules, st)

      _ ->
        rule = r_rule(anno: :none, symbols: [{}, r_yecc(st, :rootsymbol)], tokens: [])
        rules1 = [rule | rules0]

        rules =
          map(
            fn {r, i} ->
              r_rule(r, n: i)
            end,
            count(0, rules1)
          )

        r_yecc(st,
          rules_list: rules,
          rules: :erlang.list_to_tuple(rules)
        )
    end
  end

  defp duplicates(list) do
    unique = usort(list)
    {unique, list -- unique}
  end

  defp names(symbols) do
    map(
      fn symbol ->
        r_symbol(symbol, :name)
      end,
      symbols
    )
  end

  defp symbol_anno(name, st) do
    r_symbol(anno: anno) = symbol_find(name, r_yecc(st, :all_symbols))
    anno
  end

  defp symbol_member(symbol, symbols) do
    symbol_find(r_symbol(symbol, :name), symbols) !== false
  end

  defp symbol_find(name, symbols) do
    :lists.keyfind(name, r_symbol(:name), symbols)
  end

  defp states_and_goto_table(st0) do
    st1 = create_symbol_table(st0)
    st = compute_states(st1)
    create_precedence_table(st)
  end

  defp parse_actions(st) do
    _ = :erlang.erase()
    parseActions = compute_parse_actions(r_yecc(st, :n_states), st, [])
    _ = :erlang.erase()
    r_yecc(st, parse_actions: parseActions, state_tab: [])
  end

  defp action_conflicts(st0) do
    st = find_action_conflicts(st0)
    r_yecc(st, conflicts_done: true)
  end

  Record.defrecord(:r_state_info, :state_info,
    reduce_only: :undefined,
    state_repr: :undefined,
    comment: :undefined
  )

  defp write_file(st0) do
    r_yecc(parse_actions: parseActions, goto_tab: gotoTab) = st0
    sorted = sort_parse_actions(parseActions)
    stateReprs = find_identical_shift_states(sorted)
    stateInfo = collect_some_state_info(sorted, stateReprs)

    stateJumps =
      find_partial_shift_states(
        sorted,
        stateReprs
      )

    userCodeActions = find_user_code(sorted, st0)

    r_yecc(
      infile: infile,
      outfile: outfile,
      inport: inport,
      outport: outport,
      nonterminals: nonterminals
    ) = st0

    {st10, n_lines, lastErlangCodeLine} = output_prelude(outport, inport, st0)
    st20 = r_yecc(st10, line: r_yecc(st10, :line) + n_lines)
    st25 = nl(st20)
    st30 = output_file_directive(st25, outfile, r_yecc(st25, :line))
    st40 = nl(st30)
    st50 = output_actions(st40, stateJumps, stateInfo)

    go0 =
      for {{from, symbol}, to} <- :ets.tab2list(gotoTab) do
        {symbol, {from, to}}
      end

    go = family_with_domain(go0, nonterminals)
    st60 = output_goto(st50, go, stateInfo)
    st70 = output_inlined(st60, userCodeActions, infile)
    st = nl(st70)

    case lastErlangCodeLine do
      {:last_erlang_code_line, last_line} ->
        output_file_directive(st, infile, last_line)

      :no_erlang_code ->
        st
    end
  end

  defp yecc_ret(st0) do
    st = check_expected(st0)
    report_errors(st)
    report_warnings(st)
    es = pack_errors(r_yecc(st, :errors))
    ws = pack_warnings(r_yecc(st, :warnings))
    werror = werror(st)

    cond do
      werror ->
        do_error_return(st, es, ws)

      es === [] ->
        case member(:return_warnings, r_yecc(st, :options)) do
          true ->
            {:ok, r_yecc(st, :outfile), ws}

          false ->
            {:ok, r_yecc(st, :outfile)}
        end

      true ->
        do_error_return(st, es, ws)
    end
  end

  defp do_error_return(st, es, ws) do
    case member(:return_errors, r_yecc(st, :options)) do
      true ->
        {:error, es, ws}

      false ->
        :error
    end
  end

  defp check_expected(st0) do
    r_yecc(
      shift_reduce: sR,
      reduce_reduce: rR,
      expect_shift_reduce: expSR,
      n_states: nStates0,
      expect_n_states: expStates,
      conflicts_done: done
    ) = st0

    n_RR = length(usort(rR))
    n_SR = length(usort(sR))

    st1 =
      cond do
        not done ->
          st0

        n_SR === expSR and n_RR === 0 ->
          st0

        true ->
          add_warning(:none, {:conflicts, n_SR, n_RR}, st0)
      end

    nStates = nStates0 + 1

    cond do
      not done or expStates === [] or nStates === expStates ->
        st1

      true ->
        add_warning(:none, {:n_states, expStates, nStates}, st1)
    end
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
    case member(:report_errors, r_yecc(st, :options)) do
      true ->
        foreach(
          fn
            {file, {:none, mod, e}} ->
              :io.fwrite("~ts: ~ts\n", [file, mod.format_error(e)])

            {file, {line, mod, e}} ->
              :io.fwrite("~ts:~w: ~ts\n", [file, line, mod.format_error(e)])
          end,
          sort(r_yecc(st, :errors))
        )

      false ->
        :ok
    end
  end

  defp report_warnings(st) do
    werror = member(:warnings_as_errors, r_yecc(st, :options))

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
          r_yecc(st, :options)
        )

    case member(
           :report_warnings,
           r_yecc(st, :options)
         ) or reportWerror do
      true ->
        foreach(
          fn
            {file, {:none, mod, w}} ->
              :io.fwrite("~ts: ~s~ts\n", [file, prefix, mod.format_error(w)])

            {file, {line, mod, w}} ->
              :io.fwrite("~ts:~w: ~s~ts\n", [file, line, prefix, mod.format_error(w)])
          end,
          sort(r_yecc(st, :warnings))
        )

      false ->
        :ok
    end
  end

  defp add_error(e, st) do
    add_error(:none, e, st)
  end

  defp add_error(anno, e, st) do
    add_error(r_yecc(st, :infile), anno, e, st)
  end

  defp add_error(file, anno, e, st) do
    loc = location(anno)

    r_yecc(st,
      errors: [
        {file, {loc, :yecc, e}}
        | r_yecc(st, :errors)
      ]
    )
  end

  defp add_errors(symNames, e0, st0) do
    foldl(
      fn symName, st ->
        add_error(symbol_anno(symName, st), {e0, symName}, st)
      end,
      st0,
      symNames
    )
  end

  defp add_warning(anno, w, st) do
    loc = location(anno)

    r_yecc(st,
      warnings: [
        {r_yecc(st, :infile), {loc, :yecc, w}}
        | r_yecc(st, :warnings)
      ]
    )
  end

  defp add_warnings(symNames, w0, st0) do
    foldl(
      fn symName, st ->
        add_warning(symbol_anno(symName, st), {w0, symName}, st)
      end,
      st0,
      symNames
    )
  end

  defp check_rhs([r_symbol(name: :"$empty")], st) do
    st
  end

  defp check_rhs(rhs, st0) do
    case symbol_find(:"$empty", rhs) do
      r_symbol(anno: anno) ->
        add_error(anno, :illegal_empty, st0)

      false ->
        foldl(
          fn sym, st ->
            case symbol_member(sym, r_yecc(st, :all_symbols)) do
              true ->
                st

              false ->
                e = {:undefined_symbol, r_symbol(sym, :name)}
                add_error(r_symbol(sym, :anno), e, st)
            end
          end,
          st0,
          rhs
        )
    end
  end

  defp check_action(tokens) do
    case :erl_parse.parse_exprs(
           add_roberts_dot(
             tokens,
             :erl_anno.new(0)
           )
         ) do
      {:error, _Error} ->
        {false, false}

      {:ok, [expr | exprs]} ->
        isGuard = exprs === [] and :erl_lint.is_guard_test(expr)
        {isGuard, true}
    end
  end

  defp add_roberts_dot([], anno) do
    [{:dot, anno}]
  end

  defp add_roberts_dot([{:dot, anno} | _], _) do
    [{:dot, anno}]
  end

  defp add_roberts_dot([token | tokens], _) do
    [
      token
      | add_roberts_dot(
          tokens,
          :erlang.element(2, token)
        )
    ]
  end

  defp subst_pseudo_vars([], _, st) do
    {[], st}
  end

  defp subst_pseudo_vars([h0 | t0], nmbrOfDaughters, st0) do
    {h, st1} = subst_pseudo_vars(h0, nmbrOfDaughters, st0)
    {t, st} = subst_pseudo_vars(t0, nmbrOfDaughters, st1)
    {[h | t], st}
  end

  defp subst_pseudo_vars({:atom, anno, atom}, nmbrOfDaughters, st0) do
    case :erlang.atom_to_list(atom) do
      [?$ | rest] ->
        try do
          :erlang.list_to_integer(rest)
        catch
          :error, _ ->
            {{:atom, anno, atom}, st0}
        else
          n when n > 0 and n <= nmbrOfDaughters ->
            {{:var, anno, :erlang.list_to_atom(append('__', rest))}, st0}

          _ ->
            st = add_error(anno, {:undefined_pseudo_variable, atom}, st0)
            {{:atom, anno, :"$undefined"}, st}
        end

      _ ->
        {{:atom, anno, atom}, st0}
    end
  end

  defp subst_pseudo_vars(tuple, nmbrOfDaughters, st0)
       when is_tuple(tuple) do
    {l, st} = subst_pseudo_vars(:erlang.tuple_to_list(tuple), nmbrOfDaughters, st0)
    {:erlang.list_to_tuple(l), st}
  end

  defp subst_pseudo_vars(something_else, _, st) do
    {something_else, st}
  end

  defp kind_of_symbol(st, symName) do
    case member(symName, r_yecc(st, :nonterminals)) do
      false ->
        case member(symName, r_yecc(st, :terminals)) do
          false ->
            case r_yecc(st, :endsymbol) do
              ^symName ->
                :endsymbol

              _ ->
                :unknown
            end

          true ->
            :terminal
        end

      true ->
        :nonterminal
    end
  end

  Record.defrecord(:r_tabs, :tabs,
    symbols: :undefined,
    inv_symbols: :undefined,
    state_id: :undefined,
    rp_rhs: :undefined,
    rp_info: :undefined,
    goto: :undefined
  )

  Record.defrecord(:r_item, :item,
    rule_pointer: :undefined,
    look_ahead: :undefined,
    rhs: :undefined
  )

  defp compute_states(st0) do
    symbolTab = r_yecc(st0, :symbol_tab)

    codedRules =
      map(
        fn r_rule(symbols: syms) = r ->
          r_rule(r, symbols: code_symbols(syms, symbolTab))
        end,
        r_yecc(st0, :rules_list)
      )

    codedNonterminals =
      code_symbols(
        r_yecc(st0, :nonterminals),
        symbolTab
      )

    stC =
      r_yecc(st0,
        rules_list: codedRules,
        rules: :erlang.list_to_tuple(codedRules),
        nonterminals: codedNonterminals
      )

    {ruleIndex, rulePointer2Rule} =
      make_rule_index(
        stC,
        r_yecc(st0, :rules_list)
      )

    stateTab0 = {}
    stateIdTab = :ets.new(:yecc_state_id, [:set])
    gotoTab = :ets.new(:yecc_goto, [:bag])
    rulePointerRhs = make_rhs_index(r_yecc(stC, :rules_list))
    rulePointerInfo = make_rule_pointer_info(stC, rulePointerRhs, ruleIndex)

    tables =
      r_tabs(
        symbols: symbolTab,
        state_id: stateIdTab,
        rp_rhs: rulePointerRhs,
        rp_info: rulePointerInfo,
        goto: gotoTab
      )

    _ = :erlang.erase()

    endsymCode =
      code_terminal(
        r_yecc(stC, :endsymbol),
        r_yecc(stC, :symbol_tab)
      )

    {stateId, state0} =
      compute_state(
        [{endsymCode, 1}],
        tables
      )

    stateNum0 = first_state()
    firstState = {stateNum0, state0}
    stateTab1 = insert_state(tables, stateTab0, firstState, stateId)

    {stateTab, n} =
      compute_states1([{stateNum0, get_current_symbols(state0)}], firstState, stateTab1, tables)

    true = :ets.delete(stateIdTab)

    st =
      r_yecc(st0,
        state_tab: stateTab,
        goto_tab: gotoTab,
        n_states: n,
        rule_pointer2rule: rulePointer2Rule
      )

    decode_goto(gotoTab, r_yecc(st, :inv_symbol_tab))
    check_usage(st)
  end

  defp first_state() do
    0
  end

  defp decode_goto(gotoTab, invSymTab) do
    g = :ets.tab2list(gotoTab)
    :ets.delete_all_objects(gotoTab)

    :ets.insert(
      gotoTab,
      map(
        fn {{from, sym, next}} ->
          {{from, decode_symbol(sym, invSymTab)}, next}
        end,
        g
      )
    )
  end

  defp check_usage(st0) do
    selSyms =
      :ets.fun2ms(fn {{_, sym}, _} ->
        sym
      end)

    usedSymbols = :ets.select(r_yecc(st0, :goto_tab), selSyms)
    syms = :ordsets.from_list([{}, :"$empty" | usedSymbols])
    nonTerms = :ordsets.from_list(r_yecc(st0, :nonterminals))

    unusedNonTerms =
      :ordsets.to_list(
        :ordsets.subtract(
          nonTerms,
          syms
        )
      )

    st1 = add_warnings(unusedNonTerms, :unused_nonterminal, st0)
    terms = :ordsets.from_list(r_yecc(st0, :terminals))

    st2 =
      add_warnings(
        :ordsets.to_list(
          :ordsets.subtract(
            terms,
            syms
          )
        ),
        :unused_terminal,
        st1
      )

    definedNonTerminals =
      map(
        fn r_rule(symbols: [name | _]) ->
          name
        end,
        r_yecc(st2, :rules_list)
      )

    defNonTerms = :ordsets.from_list(definedNonTerminals)
    undefNonTerms = :ordsets.subtract(nonTerms, defNonTerms)

    add_errors(
      :ordsets.to_list(
        :ordsets.subtract(
          undefNonTerms,
          unusedNonTerms
        )
      ),
      :missing_syntax_rule,
      st2
    )
  end

  defp lookup_state(stateTab, n) do
    :erlang.element(n + 1, stateTab)
  end

  defp insert_state(r_tabs(state_id: stateIdTab), stateTab0, state, stateId) do
    {n, _Items} = state
    insert_state_id(stateIdTab, n, stateId)

    stateTab =
      cond do
        tuple_size(stateTab0) > n ->
          stateTab0

        true ->
          :erlang.list_to_tuple(
            :erlang.tuple_to_list(stateTab0) ++
              :lists.duplicate(
                round(1 + n * 1.5),
                []
              )
          )
      end

    :erlang.setelement(n + 1, stateTab, state)
  end

  defp insert_state_id(stateIdTab, n, stateId) do
    true = :ets.insert(stateIdTab, {stateId, n})
  end

  defp compute_states1([], {n, _} = _CurrState, stateTab0, _Tables) do
    {stateTab0, n}
  end

  defp compute_states1([{n, symbols} | try], currState, stateTab, tables) do
    {_N, s} = lookup_state(stateTab, n)
    seeds = state_seeds(s, symbols)
    compute_states2(seeds, n, try, currState, stateTab, tables)
  end

  defp compute_states2([], _N, try, currState, stateTab, tables) do
    compute_states1(try, currState, stateTab, tables)
  end

  defp compute_states2([{sym, seed} | seeds], n, try, currState, stateTab, tables) do
    {stateId, newState} = compute_state(seed, tables)

    case check_states(newState, stateId, stateTab, tables) do
      :add ->
        {m, _} = currState
        currentSymbols = get_current_symbols(newState)
        next = m + 1
        nextState = {next, newState}
        newStateTab = insert_state(tables, stateTab, nextState, stateId)
        insert_goto(tables, n, sym, next)
        compute_states2(seeds, n, [{next, currentSymbols} | try], nextState, newStateTab, tables)

      {:old, m} ->
        insert_goto(tables, n, sym, m)
        compute_states2(seeds, n, try, currState, stateTab, tables)

      {:merge, m, newCurrent} ->
        try1 =
          case :lists.keyfind(m, 1, try) do
            false ->
              [{m, newCurrent} | try]

            {_, oldCurrent} ->
              case :ordsets.is_subset(newCurrent, oldCurrent) do
                true ->
                  try

                false ->
                  [
                    {m, :ordsets.union(newCurrent, oldCurrent)}
                    | keydelete(m, 1, try)
                  ]
              end
          end

        newStateTab = merge_states(newState, stateTab, tables, m, stateId)
        insert_goto(tables, n, sym, m)
        compute_states2(seeds, n, try1, currState, newStateTab, tables)
    end
  end

  defp insert_goto(tables, from, sym, to) do
    true = :ets.insert(r_tabs(tables, :goto), {{from, sym, to}})
  end

  defp create_symbol_table(st) do
    r_yecc(terminals: terminals, endsymbol: endsymbol) = st
    symbolTab = :ets.new(:yecc_symbols, [{:keypos, 1}])
    ts = [:"$empty", endsymbol | delete(:"$empty", terminals)]
    tsC = count(0, ts)

    nTsC =
      map(
        fn {nT, i} ->
          {nT, -i}
        end,
        count(1, r_yecc(st, :nonterminals))
      )

    cs = tsC ++ nTsC
    true = :ets.insert(symbolTab, cs)

    invSymTable =
      :ets.new(
        :yecc_inverted_terminals,
        [{:keypos, 2}]
      )

    true = :ets.insert(invSymTable, cs)

    r_yecc(st,
      symbol_tab: symbolTab,
      inv_symbol_tab: invSymTable
    )
  end

  defp get_current_symbols(state) do
    usort(get_current_symbols1(state, []))
  end

  defp get_current_symbols1([], syms) do
    syms
  end

  defp get_current_symbols1([r_item(rhs: rhs) | items], syms) do
    case rhs do
      [] ->
        get_current_symbols1(items, syms)

      [symbol | _] ->
        get_current_symbols1(items, [symbol | syms])
    end
  end

  defp state_seeds(items, symbols) do
    l =
      for r_item(rule_pointer: rP, look_ahead: lA, rhs: [s | _]) <- items do
        {s, {lA, rP + 1}}
      end

    state_seeds1(keysort(1, l), symbols)
  end

  defp state_seeds1(_L, []) do
    []
  end

  defp state_seeds1(l, [symbol | symbols]) do
    state_seeds(l, symbol, symbols, [])
  end

  defp state_seeds([{symbol, item} | l], symbol, symbols, is) do
    state_seeds(l, symbol, symbols, [item | is])
  end

  defp state_seeds([{s, _Item} | l], symbol, symbols, is)
       when s < symbol do
    state_seeds(l, symbol, symbols, is)
  end

  defp state_seeds(l, symbol, symbols, is) do
    [{symbol, is} | state_seeds1(l, symbols)]
  end

  defp compute_state(seed, tables) do
    rpInfo = r_tabs(tables, :rp_info)

    foreach(
      fn {lA, rulePointer} ->
        :erlang.put(rulePointer, lA)
      end,
      seed
    )

    foreach(
      fn {lA, rP} ->
        compute_closure(lA, rP, rpInfo)
      end,
      seed
    )

    closure = keysort(1, :erlang.erase())
    state_items(closure, [], [], r_tabs(tables, :rp_rhs))
  end

  defp state_items([{rP, lA} | l], is, id, rpRhs) do
    i = r_item(rule_pointer: rP, look_ahead: lA, rhs: :erlang.element(rP, rpRhs))
    state_items(l, [i | is], [rP | id], rpRhs)
  end

  defp state_items(_, is, id, _RpRhs) do
    {id, is}
  end

  defp compute_closure(lookahead, rulePointer, rpInfo) do
    case :erlang.element(rulePointer, rpInfo) do
      [] = void ->
        void

      {:no_union, expandingRules, newLookahead} ->
        compute_closure1(expandingRules, newLookahead, rpInfo)

      {:union, expandingRules, lookahead0} ->
        newLookahead = set_union(lookahead0, lookahead)
        compute_closure1(expandingRules, newLookahead, rpInfo)

      expandingRules ->
        compute_closure1(expandingRules, lookahead, rpInfo)
    end
  end

  defp compute_closure1([rulePointer | tail], newLookahead, rpInfo) do
    compute_closure1(tail, newLookahead, rpInfo)

    case :erlang.get(rulePointer) do
      :undefined ->
        :erlang.put(rulePointer, newLookahead)
        compute_closure(newLookahead, rulePointer, rpInfo)

      lookahead2 ->
        lookahead = set_union(lookahead2, newLookahead)

        cond do
          lookahead === lookahead2 ->
            lookahead2

          true ->
            :erlang.put(rulePointer, lookahead)
            compute_closure(newLookahead, rulePointer, rpInfo)
        end
    end
  end

  defp compute_closure1(nil__, _, _RpInfo) do
    nil__
  end

  defp check_states(newState, stateId, stateTab, r_tabs(state_id: stateIdTab)) do
    try do
      :ets.lookup_element(stateIdTab, stateId, 2)
    catch
      :error, _ ->
        :add
    else
      n ->
        {_N, oldState} = lookup_state(stateTab, n)
        check_state1(newState, oldState, [], n)
    end
  end

  defp check_state1(
         [r_item(look_ahead: lookahead1, rhs: rhs) | items1],
         [r_item(look_ahead: lookahead2) | items2],
         symbols,
         n
       ) do
    case set_is_subset(lookahead1, lookahead2) do
      true ->
        check_state1(items1, items2, symbols, n)

      false ->
        case rhs do
          [] ->
            check_state2(items1, items2, symbols, n)

          [symbol | _] ->
            check_state2(items1, items2, [symbol | symbols], n)
        end
    end
  end

  defp check_state1([], [], _Symbols, n) do
    {:old, n}
  end

  defp check_state2(
         [r_item(look_ahead: lookahead1, rhs: rhs) | items1],
         [r_item(look_ahead: lookahead2) | items2],
         symbols,
         n
       ) do
    case set_is_subset(lookahead1, lookahead2) do
      true ->
        check_state2(items1, items2, symbols, n)

      false ->
        case rhs do
          [] ->
            check_state2(items1, items2, symbols, n)

          [symbol | _] ->
            check_state2(items1, items2, [symbol | symbols], n)
        end
    end
  end

  defp check_state2([], [], symbols, n) do
    {:merge, n, usort(symbols)}
  end

  defp merge_states(newState, stateTab, tables, m, stateId) do
    {_M, old_state} = lookup_state(stateTab, m)
    mergedState = merge_states1(newState, old_state)
    insert_state(tables, stateTab, {m, mergedState}, stateId)
  end

  defp merge_states1([item1 | items1], [item2 | items2]) do
    lA1 = r_item(item1, :look_ahead)
    lA2 = r_item(item2, :look_ahead)

    cond do
      lA1 === lA2 ->
        [item1 | merge_states1(items1, items2)]

      true ->
        [
          r_item(item1, look_ahead: set_union(lA1, lA2))
          | merge_states1(items1, items2)
        ]
    end
  end

  defp merge_states1(_, _) do
    []
  end

  defp make_rhs_index(rulesList) do
    index =
      flatmap(
        fn r_rule(symbols: [_Non | daughters]) ->
          suffixes0(daughters)
        end,
        rulesList
      )

    :erlang.list_to_tuple(index)
  end

  defp suffixes0([0]) do
    [[], []]
  end

  defp suffixes0(l) do
    suffixes(l)
  end

  defp suffixes([] = l) do
    [l]
  end

  defp suffixes([_ | t] = l) do
    [l | suffixes(t)]
  end

  defp make_rule_pointer_info(stC, rpRhs, ruleIndex) do
    symbolTab = r_yecc(stC, :symbol_tab)
    lcTab = make_left_corner_table(stC)

    lA_index =
      map(
        fn syms ->
          rp_info(syms, symbolTab, lcTab, ruleIndex)
        end,
        :erlang.tuple_to_list(rpRhs)
      )

    :erlang.list_to_tuple(lA_index)
  end

  defp rp_info([], _SymbolTab, _LcTab, _RuleIndex) do
    []
  end

  defp rp_info([category | followers], symbolTab, lcTab, ruleIndex) do
    case :maps.find(category, ruleIndex) do
      :error ->
        []

      {:ok, expandingRules} when followers === [] ->
        expandingRules

      {:ok, expandingRules} ->
        case make_lookahead(followers, symbolTab, lcTab, set_empty()) do
          {:empty, lA} ->
            {:union, expandingRules, lA}

          lA ->
            {:no_union, expandingRules, lA}
        end
    end
  end

  defp make_lookahead([], _, _, lA) do
    {:empty, lA}
  end

  defp make_lookahead([symbol | symbols], symbolTab, lcTab, lA) do
    case :maps.find(symbol, lcTab) do
      {:ok, leftCorner} ->
        case empty_member(leftCorner) do
          true ->
            make_lookahead(symbols, symbolTab, lcTab, set_union(empty_delete(leftCorner), lA))

          false ->
            set_union(leftCorner, lA)
        end

      :error ->
        set_add(symbol, lA)
    end
  end

  defp make_left_corner_table(r_yecc(rules_list: rulesList) = st) do
    symbolTab = left_corner_symbol_table(st)

    rules =
      map(
        fn r_rule(symbols: [lhs | rhs]) ->
          {lhs, {lhs, rhs}}
        end,
        rulesList
      )

    leftHandTab = :maps.from_list(family(rules))

    x0 =
      for {h, {h, rhs}} <- rules, s <- rhs, not is_terminal(symbolTab, s) do
        {s, h}
      end

    xL = family_with_domain(x0, r_yecc(st, :nonterminals))
    x = :maps.from_list(xL)

    xref = fn nT ->
      :maps.get(nT, x)
    end

    e = set_empty()

    lC0 =
      :maps.from_list(
        for {h, _} <- xL do
          {h, e}
        end
      )

    {q, lC1} =
      foldl(
        fn {h, {h, [s | _]}}, {q0, lC} ->
          case :ets.lookup(symbolTab, s) do
            [{_, num} = symbolAndNum] when num >= 0 ->
              f = set_add_terminal(symbolAndNum, e)
              {[xref.(h) | q0], upd_first(h, f, lC)}

            _ ->
              {q0, lC}
          end
        end,
        {[], lC0},
        rules
      )

    left_corners(q, lC1, leftHandTab, symbolTab, xref)
  end

  defp left_corners(q0, lC0, leftHandTab, symbolTab, xref) do
    case usort(append(q0)) do
      [] ->
        lC0

      q1 ->
        rs =
          flatmap(
            fn nT ->
              :maps.get(nT, leftHandTab)
            end,
            q1
          )

        {lC, q} = left_corners2(rs, lC0, [], symbolTab, xref)
        left_corners(q, lC, leftHandTab, symbolTab, xref)
    end
  end

  defp left_corners2([], lC, q, _SymbolTab, _Xref) do
    {lC, q}
  end

  defp left_corners2([{head, rhs} | rs], lC, q0, symbolTab, xref) do
    ts = left_corner_rhs(rhs, head, lC, set_empty(), symbolTab)
    first0 = :maps.get(head, lC)

    case set_is_subset(ts, first0) do
      true ->
        left_corners2(rs, lC, q0, symbolTab, xref)

      false ->
        lC1 = upd_first(head, ts, lC)
        left_corners2(rs, lC1, [xref.(head) | q0], symbolTab, xref)
    end
  end

  defp upd_first(nT, ts, lC) do
    :maps.update_with(
      nT,
      fn first ->
        set_union(first, ts)
      end,
      lC
    )
  end

  defp left_corner_rhs([s | ss], head, lC, ts, symbolTab) do
    case :ets.lookup(symbolTab, s) do
      [{_, num} = symbolAndNum] when num >= 0 ->
        set_add_terminal(symbolAndNum, ts)

      [_NonTerminalSymbol] ->
        first = :maps.get(s, lC)

        case empty_member(first) do
          true ->
            nTs = set_union(empty_delete(first), ts)
            left_corner_rhs(ss, head, lC, nTs, symbolTab)

          false ->
            set_union(first, ts)
        end
    end
  end

  defp left_corner_rhs([], _Head, _LC, ts, _SymbolTab) do
    set_add(0, ts)
  end

  defp make_rule_index(
         r_yecc(
           nonterminals: nonterminals,
           rules_list: rulesList
         ),
         rulesListNoCodes
       ) do
    {rulesL, _N} =
      :lists.mapfoldl(
        fn r_rule(
             symbols: [
               nonterminal
               | daughters
             ]
           ),
           i ->
          i1 = i + length(daughters) + 1
          {{nonterminal, i}, i1}
        end,
        1,
        rulesList
      )

    indexedTab = family_with_domain(rulesL, nonterminals)

    symbol2Rule =
      for r_rule(symbols: symbols) = r <- rulesListNoCodes,
          foo <- symbols do
        {foo, r}
      end

    pointer2Rule =
      for {{_Foo, r}, i} <-
            count(
              1,
              symbol2Rule
            ) do
        {i, r}
      end

    {:maps.from_list(indexedTab), :maps.from_list(pointer2Rule)}
  end

  defp compute_parse_actions(n, st, stateActions) do
    case n < first_state() do
      true ->
        stateActions

      false ->
        {^n, stateN} = lookup_state(r_yecc(st, :state_tab), n)
        actions = compute_parse_actions1(stateN, n, st)
        compute_parse_actions(n - 1, st, [{n, actions} | stateActions])
    end
  end

  defp compute_parse_actions1([], _, _) do
    []
  end

  defp compute_parse_actions1(
         [
           r_item(rule_pointer: rulePointer, look_ahead: lookahead0, rhs: rhs)
           | items
         ],
         n,
         st
       ) do
    case rhs do
      [] ->
        lookahead =
          decode_terminals(
            lookahead0,
            r_yecc(st, :inv_symbol_tab)
          )

        case rule(rulePointer, st) do
          {[{} | _], _RuleLine, _} ->
            [{lookahead, :accept} | compute_parse_actions1(items, n, st)]

          {[head | daughters0], _RuleLine, _} ->
            daughters = delete(:"$empty", daughters0)

            [
              {lookahead,
               r_reduce(
                 rule_nmbr: rulePointer,
                 head: head,
                 nmbr_of_daughters: length(daughters),
                 prec: get_prec(daughters ++ [head], st)
               )}
              | compute_parse_actions1(items, n, st)
            ]
        end

      [symbol | daughters] ->
        case is_terminal(r_yecc(st, :symbol_tab), symbol) do
          true ->
            decSymbol =
              decode_symbol(
                symbol,
                r_yecc(st, :inv_symbol_tab)
              )

            {[head | _], _RuleLine, _} = rule(rulePointer, st)

            prec1 =
              case daughters do
                [] ->
                  get_prec([decSymbol, head], st)

                _ ->
                  get_prec([decSymbol], st)
              end

            pos =
              case daughters do
                [] ->
                  :z

                _ ->
                  :a
              end

            [
              {[decSymbol],
               r_shift(
                 state: goto(n, decSymbol, st),
                 pos: pos,
                 prec: prec1,
                 rule_nmbr: rulePointer
               )}
              | compute_parse_actions1(items, n, st)
            ]

          false ->
            compute_parse_actions1(items, n, st)
        end
    end
  end

  defp get_prec(symbols, st) do
    get_prec1(symbols, r_yecc(st, :prec_tab), {0, :none})
  end

  defp get_prec1([], _, p) do
    p
  end

  defp get_prec1([symbol | t], precTab, p) do
    case :ets.lookup(precTab, symbol) do
      [] ->
        get_prec1(t, precTab, p)

      [{_, n, ass}] ->
        get_prec1(t, precTab, {n, ass})
    end
  end

  defp create_precedence_table(st) do
    precTab = :ets.new(:yecc_precedences, [])
    true = :ets.insert(precTab, r_yecc(st, :prec))
    r_yecc(st, prec_tab: precTab)
  end

  Record.defrecord(:r_cxt, :cxt,
    terminal: :undefined,
    state_n: :undefined,
    yecc: :undefined,
    res: :undefined
  )

  defp find_action_conflicts(st0) do
    cxt0 = r_cxt(yecc: st0, res: [])

    {r_cxt(yecc: st, res: res), newParseActions0} =
      foldl(
        fn {n, actions0}, {cxt1, stateActions} ->
          l =
            for {lookahead, act} <- actions0,
                terminal <- lookahead do
              {terminal, act}
            end

          {cxt, actions} =
            foldl(
              fn {terminal, as}, {cxt2, acts0} ->
                cxt3 =
                  r_cxt(cxt2,
                    terminal: terminal,
                    state_n: n
                  )

                {action, cxt} =
                  find_action_conflicts2(
                    as,
                    cxt3
                  )

                {cxt,
                 [
                   {action, terminal}
                   | acts0
                 ]}
              end,
              {cxt1, []},
              family(l)
            )

          {cxt,
           [
             {n, inverse(family(actions))}
             | stateActions
           ]}
        end,
        {cxt0, []},
        r_yecc(st0, :parse_actions)
      )

    cond do
      length(res) > 0 and r_yecc(st, :verbose) ->
        :io.fwrite("\n*** Conflicts resolved by operator precedences:\n\n")

        foreach(
          fn {confl, name} ->
            report_conflict(confl, st, name, :prec)
          end,
          reverse(res)
        )

        :io.fwrite("*** End of resolved conflicts\n\n")

      true ->
        :ok
    end

    newParseActions = reverse(newParseActions0)
    r_yecc(st, parse_actions: newParseActions)
  end

  defp find_action_conflicts2([action], cxt) do
    {action, cxt}
  end

  defp find_action_conflicts2(
         [r_shift(state: st, pos: pos, prec: prec), r_shift(state: st) = s | as],
         cxt
       )
       when pos === :a or prec === {0, :none} do
    find_action_conflicts2([s | as], cxt)
  end

  defp find_action_conflicts2(
         [r_shift(state: newState, pos: :z) = s1, r_shift(state: newState) = s2 | _],
         cxt
       ) do
    confl = conflict(s1, s2, cxt)
    r_cxt(yecc: st0) = cxt
    st = conflict_error(confl, st0)
    {s1, r_cxt(cxt, yecc: st)}
  end

  defp find_action_conflicts2([r_shift(prec: {p1, ass1}) = s | rs], cxt0) do
    {r, cxt1} = find_reduce_reduce(rs, cxt0)
    r_cxt(res: res0, yecc: st0) = cxt1
    r_reduce(prec: {p2, ass2}) = r
    confl = conflict(r, s, cxt1)

    cond do
      p1 > p2 ->
        {s, r_cxt(cxt1, res: [{confl, :shift} | res0])}

      p2 > p1 ->
        {r, r_cxt(cxt1, res: [{confl, :reduce} | res0])}

      ass1 === :left and ass2 === :left ->
        {r, r_cxt(cxt1, res: [{confl, :reduce} | res0])}

      ass1 === :right and ass2 === :right ->
        {s, r_cxt(cxt1, res: [{confl, :shift} | res0])}

      ass1 === :nonassoc and ass2 === :nonassoc ->
        {:nonassoc, cxt1}

      p1 === 0 and p2 === 0 ->
        report_conflict(confl, st0, :shift, :default)
        st = add_conflict(confl, st0)
        {s, r_cxt(cxt1, yecc: st)}

      true ->
        st = conflict_error(confl, st0)
        {s, r_cxt(cxt1, yecc: st)}
    end
  end

  defp find_action_conflicts2(rs, cxt0) do
    find_reduce_reduce(rs, cxt0)
  end

  defp find_reduce_reduce([r], cxt) do
    {r, cxt}
  end

  defp find_reduce_reduce([:accept = a, r_reduce() = r | rs], cxt0) do
    confl = conflict(r, a, cxt0)
    st = conflict_error(confl, r_cxt(cxt0, :yecc))
    cxt = r_cxt(cxt0, yecc: st)
    find_reduce_reduce([r | rs], cxt)
  end

  defp find_reduce_reduce(
         [
           r_reduce(head: categ1, prec: {p1, _}) = r1,
           r_reduce(head: categ2, prec: {p2, _}) = r2 | rs
         ],
         cxt0
       ) do
    r_cxt(res: res0, yecc: st0) = cxt0
    confl = conflict(r1, r2, cxt0)

    {r, res, st} =
      cond do
        p1 > p2 ->
          {r1, [{confl, categ1} | res0], st0}

        p2 > p1 ->
          {r2, [{confl, categ2} | res0], st0}

        true ->
          st1 = conflict_error(confl, st0)
          {r1, res0, st1}
      end

    cxt = r_cxt(cxt0, res: res, yecc: st)
    find_reduce_reduce([r | rs], cxt)
  end

  defp sort_parse_actions([]) do
    []
  end

  defp sort_parse_actions([{n, la_actions} | tail]) do
    [
      {n, sort_parse_actions1(la_actions)}
      | sort_parse_actions(tail)
    ]
  end

  defp sort_parse_actions1(laActions) do
    as =
      filter(
        fn {_LA, a} ->
          a === :accept
        end,
        laActions
      )

    ss =
      filter(
        fn {_LA, a} ->
          elem(a, 0) === :shift
        end,
        laActions
      )

    rs =
      filter(
        fn {_LA, a} ->
          elem(a, 0) === :reduce
        end,
        laActions
      )

    ns =
      filter(
        fn {_LA, a} ->
          a === :nonassoc
        end,
        laActions
      )

    as ++ ss ++ rs ++ ns
  end

  defp find_identical_shift_states(stateActions) do
    l1 =
      for {state, actions} <- stateActions do
        {actions, state}
      end

    {sO, notSO} =
      :lists.partition(
        fn {actions, _States} ->
          shift_actions_only(actions)
        end,
        family(l1)
      )

    r =
      for {_Actions, states} <- sO, state <- states do
        {state, hd(states)}
      end ++
        for {_Actions, states} <- notSO,
            state <- states do
          {state, state}
        end

    :lists.keysort(1, r)
  end

  Record.defrecord(:r_part_data, :part_data,
    name: :undefined,
    eq_state: :undefined,
    actions: :undefined,
    n_actions: :undefined,
    states: :undefined
  )

  defp find_partial_shift_states(stateActionsL, stateReprs) do
    l =
      for {{state, actions}, {state, state}} <-
            :lists.zip(
              stateActionsL,
              stateReprs
            ),
          shift_actions_only(actions) do
        {state, actions}
      end

    stateActions = :sofs.family(l, [{:state, [:action]}])
    stateAction = :sofs.family_to_relation(stateActions)
    parts = :sofs.partition(:sofs.range(stateActions))
    partsL = :sofs.to_external(parts)
    partNameL = :lists.zip(seq1(length(partsL)), partsL)

    actPartL =
      for {partName, actions} <- partNameL,
          action <- actions do
        {action, partName}
      end

    actionPartName =
      :sofs.relation(
        actPartL,
        [{:action, :partname}]
      )

    statePartName =
      :sofs.relative_product(
        stateAction,
        actionPartName
      )

    partInStates = :sofs.relation_to_family(:sofs.converse(statePartName))

    partActions =
      :sofs.family(
        partNameL,
        [{:partname, [:action]}]
      )

    partState =
      :sofs.relative_product(
        partActions,
        :sofs.converse(stateActions)
      )

    partStates =
      sofs_family_with_domain(
        partState,
        :sofs.domain(partActions)
      )

    partDataL =
      for {{nm, p}, {nm, s}, {nm, eqS}} <-
            :lists.zip3(
              partNameL,
              :sofs.to_external(partInStates),
              :sofs.to_external(partStates)
            ) do
        r_part_data(
          name: nm,
          eq_state: eqS,
          actions: p,
          n_actions: length(p),
          states: :ordsets.from_list(s)
        )
      end

    true = length(partDataL) === length(partNameL)
    ps = select_parts(partDataL)

    j1 =
      for {_W, r_part_data(actions: actions, eq_state: [], states: states)} <- ps,
          state <- states do
        {state, actions, {:jump_some, hd(states)}}
      end

    j2 =
      for {_W, r_part_data(actions: actions, eq_state: eqS, states: states)} <- ps,
          to <- eqS,
          state <- states,
          state !== to do
        {state, actions, {:jump_all, to}}
      end

    j = :lists.keysort(1, j1 ++ j2)

    jumpStates =
      :ordsets.from_list(
        for {s, _, _} <- j do
          s
        end
      )

    {jS, nJS} =
      :sofs.partition(
        1,
        :sofs.relation(
          stateActionsL,
          [{:state, :actions}]
        ),
        :sofs.set(jumpStates, [:state])
      )

    r =
      for {s, actions} <- :sofs.to_external(nJS) do
        {s, {actions, :jump_none}}
      end ++
        for {{s, actions}, {s, part, {tag, toS}}} <-
              :lists.zip(
                :sofs.to_external(jS),
                j
              ) do
          {s, {actions -- part, {tag, toS, part}}}
        end

    true = length(stateActionsL) === length(r)
    :lists.keysort(1, r)
  end

  defp select_parts([]) do
    []
  end

  defp select_parts(partDataL) do
    t1 =
      for pD <- partDataL do
        {score(pD), pD}
      end

    [{w, pD} | ws] = :lists.reverse(:lists.keysort(1, t1))
    r_part_data(n_actions: nActions, states: s) = pD

    cond do
      w < 8 ->
        []

      true ->
        nL =
          for {w1, r_part_data(states: s0) = d} <- ws,
              w1 > 0,
              (newS = :ordsets.subtract(s0, s)) !== [] do
            r_part_data(d, states: newS)
          end

        cond do
          length(s) === 1 or nActions === 1 ->
            select_parts(nL)

          true ->
            [{w, pD} | select_parts(nL)]
        end
    end
  end

  defp score(r_part_data(n_actions: nActions, eq_state: [], states: s)) do
    length(s) * nActions * 28 - (36 + nActions * 28 + length(s) * 8)
  end

  defp score(r_part_data(n_actions: nActions, states: s)) do
    (length(s) - 1) * nActions * 28 - 8 * (length(s) - 1)
  end

  defp shift_actions_only(actions) do
    length(
      for {_Ts, {:shift, _, _, _, _}} <- actions do
        :foo
      end
    ) === length(actions)
  end

  defp collect_some_state_info(stateActions, stateReprs) do
    rF = fn {_LA, a} ->
      elem(a, 0) === :reduce
    end

    l =
      for {{state, laActions}, {state, repr}} <-
            :lists.zip(
              stateActions,
              stateReprs
            ) do
        {state,
         (
           rO = :lists.all(rF, laActions)

           c =
             for true <- [rO], repr !== state do
               :io_lib.fwrite(" %% ~w\n", [state])
             end

           r_state_info(reduce_only: rO, state_repr: repr, comment: c)
         )}
      end

    :erlang.list_to_tuple(l)
  end

  defp conflict_error(conflict, st0) do
    st1 = add_conflict(conflict, st0)
    add_error({:conflict, conflict}, st1)
  end

  defp report_conflict(conflict, st, actionName, how) do
    cond do
      r_yecc(st, :verbose) ->
        :io.fwrite("~s\n", [format_conflict(conflict)])
        formated = format_symbol(actionName)

        case how do
          :prec ->
            :io.fwrite("Resolved in favor of ~ts.\n\n", [formated])

          :default ->
            :io.fwrite("Conflict resolved in favor of ~ts.\n\n", [formated])
        end

      true ->
        :ok
    end
  end

  defp add_conflict(conflict, st) do
    case conflict do
      {symbol, stateN, _, {:reduce, _, _, _}} ->
        r_yecc(st,
          reduce_reduce: [
            {stateN, symbol}
            | r_yecc(st, :reduce_reduce)
          ]
        )

      {symbol, stateN, _, {:accept, _}} ->
        r_yecc(st,
          reduce_reduce: [
            {stateN, symbol}
            | r_yecc(st, :reduce_reduce)
          ]
        )

      {symbol, stateN, _, {:shift, _, _}} ->
        r_yecc(st,
          shift_reduce: [
            {stateN, symbol}
            | r_yecc(st, :shift_reduce)
          ]
        )

      {_Symbol, _StateN, {:one_level_up, _, _}, _Confl} ->
        st
    end
  end

  defp conflict(
         r_shift(prec: prec1, rule_nmbr: ruleNmbr1),
         r_shift(prec: prec2, rule_nmbr: ruleNmbr2),
         cxt
       ) do
    r_cxt(terminal: symbol, state_n: n, yecc: st) = cxt
    {_, l1, ruleN1} = rule(ruleNmbr1, st)
    {_, l2, ruleN2} = rule(ruleNmbr2, st)
    confl = {:one_level_up, {l1, ruleN1, prec1}, {l2, ruleN2, prec2}}
    {symbol, n, confl, confl}
  end

  defp conflict(r_reduce(rule_nmbr: ruleNmbr1), newAction, cxt) do
    r_cxt(terminal: symbol, state_n: n, yecc: st) = cxt
    {r1, ruleLine1, ruleN1} = rule(ruleNmbr1, st)

    confl =
      case newAction do
        :accept ->
          {:accept, r_yecc(st, :rootsymbol)}

        r_reduce(rule_nmbr: ruleNmbr2) ->
          {r2, ruleLine2, ruleN2} = rule(ruleNmbr2, st)
          {:reduce, r2, ruleN2, ruleLine2}

        r_shift(state: newState) ->
          {:shift, newState, last(r1)}
      end

    {symbol, n, {r1, ruleN1, ruleLine1}, confl}
  end

  defp format_conflict(
         {symbol, n, _, {:one_level_up, {l1, ruleN1, {p1, ass1}}, {l2, ruleN2, {p2, ass2}}}}
       ) do
    s1 =
      :io_lib.fwrite("Conflicting precedences of symbols when scanning ~ts in state ~w:\n", [
        format_symbol(symbol),
        n
      ])

    s2 =
      :io_lib.fwrite(
        "   ~s ~w (rule ~w at line ~w)\n      vs.\n",
        [format_assoc(ass1), p1, ruleN1, l1]
      )

    s3 =
      :io_lib.fwrite(
        "   ~s ~w (rule ~w at line ~w)\n",
        [format_assoc(ass2), p2, ruleN2, l2]
      )

    [s1, s2, s3]
  end

  defp format_conflict({symbol, n, reduce, confl}) do
    s1 =
      :io_lib.fwrite("Parse action conflict scanning symbol ~ts in state ~w:\n", [
        format_symbol(symbol),
        n
      ])

    s2 =
      case reduce do
        {[hR | tR], ruleNmbr, ruleLine} ->
          :io_lib.fwrite(
            "   Reduce to ~ts from ~ts (rule ~w at line ~w)\n      vs.\n",
            [format_symbol(hR), format_symbols(tR), ruleNmbr, ruleLine]
          )
      end

    s3 =
      case confl do
        {:reduce, [hR2 | tR2], ruleNmbr2, ruleLine2} ->
          :io_lib.fwrite(
            "   reduce to ~ts from ~ts (rule ~w at line ~w).",
            [format_symbol(hR2), format_symbols(tR2), ruleNmbr2, ruleLine2]
          )

        {:shift, newState, sym} ->
          :io_lib.fwrite("   shift to state ~w, adding right sisters to ~ts.", [
            newState,
            format_symbol(sym)
          ])

        {:accept, rootsymbol} ->
          :io_lib.fwrite("   reduce to rootsymbol ~ts.", [format_symbol(rootsymbol)])
      end

    [s1, s2, s3]
  end

  defp output_prelude(outport, inport, st0)
       when r_yecc(st0, :includefile) === [] do
    st5 = output_header(st0)
    r_yecc(infile: infile, module: module) = st5
    st10 = fwrite(st5, "-module(~w).\n", [module])
    st20 = fwrite(st10, "-export([parse/1, parse_and_scan/1, format_error/1]).\n", [])

    {st25, n_lines_1, lastErlangCodeLine} =
      case r_yecc(st20, :erlang_code) do
        :none ->
          {st20, 0, :no_erlang_code}

        next_line ->
          st_10 = output_file_directive(st20, infile, next_line - 1)
          last_line = include1([], inport, outport, infile, next_line, st_10)
          nmbr_of_lines = last_line - next_line
          {st_10, nmbr_of_lines, {:last_erlang_code_line, last_line}}
      end

    st30 = nl(st25)
    includeFile = :filename.join([:code.lib_dir(:parsetools), 'include', 'yeccpre.hrl'])
    st = output_file_directive(st30, includeFile, 0)
    n_lines_2 = include(st, includeFile, outport)
    {st, n_lines_1 + n_lines_2, lastErlangCodeLine}
  end

  defp output_prelude(outport, inport, st0) do
    st5 = output_header(st0)
    r_yecc(infile: infile, module: module, includefile: includefile) = st5
    st10 = fwrite(st5, "-module(~w).\n", [module])
    st20 = output_file_directive(st10, includefile, 0)
    n_lines_1 = include(st20, includefile, outport)
    st30 = nl(st20)

    case r_yecc(st30, :erlang_code) do
      :none ->
        {st30, n_lines_1, :no_erlang_code}

      next_line ->
        st = output_file_directive(st30, infile, next_line - 1)
        last_line = include1([], inport, outport, infile, next_line, st)
        nmbr_of_lines = last_line - next_line
        {st, nmbr_of_lines + n_lines_1, {:last_erlang_code_line, last_line}}
    end
  end

  defp output_header(st0) do
    :lists.foldl(
      fn str, st ->
        fwrite(st, "~ts\n", [str])
      end,
      st0,
      r_yecc(st0, :header)
    )
  end

  defp output_goto(st, [{_Nonterminal, []} | go], stateInfo) do
    output_goto(st, go, stateInfo)
  end

  defp output_goto(st0, [{nonterminal, list} | go], stateInfo) do
    f = function_name(st0, :yeccgoto, nonterminal)
    st05 = fwrite(st0, "-dialyzer({nowarn_function, ~w/7}).\n", [f])
    st10 = output_goto1(st05, list, f, stateInfo, true)
    st = output_goto_fini(f, nonterminal, st10)
    output_goto(st, go, stateInfo)
  end

  defp output_goto(st, [], _StateInfo) do
    st
  end

  defp output_goto1(st0, [{from, to} | tail], f, stateInfo, isFirst) do
    st10 = delim(st0, isFirst)
    {^to, toInfo} = lookup_state(stateInfo, to)
    r_state_info(reduce_only: rO, state_repr: repr, comment: c) = toInfo

    cond do
      rO ->
        fromS = :io_lib.fwrite('~w=_S', [from])
        toS = '_S'

      true ->
        fromS = :io_lib.fwrite('~w', [from])
        toS = :io_lib.fwrite('~w', [to])
    end

    st20 = fwrite(st10, "~w(~s, Cat, Ss, Stack, T, Ts, Tzr) ->\n", [f, fromS])
    st30 = fwrite(st20, "~s", [c])
    st = fwrite(st30, " yeccpars2_~w(~s, Cat, Ss, Stack, T, Ts, Tzr)", [repr, toS])
    output_goto1(st, tail, f, stateInfo, false)
  end

  defp output_goto1(st, [], _F, _StateInfo, _IsFirst) do
    st
  end

  defp output_goto_fini(f, nT, r_yecc(includefile_version: {1, 1}) = st0) do
    st10 = delim(st0, false)
    st = fwrite(st10, "~w(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->\n", [f])

    fwrite(
      st,
      :unicode.characters_to_binary([
        ' erlang:error({yecc_bug,"',
        '1.4',
        '",',
        :io_lib.fwrite(
          "{~ts, State, missing_in_goto_table}",
          [
            quoted_atom(
              st0,
              nT
            )
          ]
        ),
        '}).\n\n'
      ]),
      []
    )
  end

  defp output_goto_fini(_F, _NT, st) do
    fwrite(st, ".\n\n", [])
  end

  defp find_user_code(parseActions, st) do
    for {state, la_actions} <- parseActions,
        {action, terminals, ruleNmbr, nmbrOfDaughters} <- find_user_code2(la_actions),
        (case tokens(ruleNmbr, st) do
           [{:var, _, :__1}] ->
             nmbrOfDaughters !== 1

           _ ->
             true
         end),
        terminal <- terminals do
      r_user_code(
        state: state,
        terminal: terminal,
        funname: inlined_function_name(st, state, terminal),
        action: action
      )
    end
  end

  defp find_user_code2([]) do
    []
  end

  defp find_user_code2([
         {_,
          r_reduce(
            rule_nmbr: ruleNmbr,
            nmbr_of_daughters: nmbrOfDaughters
          ) = action}
       ]) do
    [{action, ['Cat'], ruleNmbr, nmbrOfDaughters}]
  end

  defp find_user_code2([
         {la,
          r_reduce(
            rule_nmbr: ruleNmbr,
            nmbr_of_daughters: nmbrOfDaughters
          ) = action}
         | t
       ]) do
    [
      {action, la, ruleNmbr, nmbrOfDaughters}
      | find_user_code2(t)
    ]
  end

  defp find_user_code2([_ | t]) do
    find_user_code2(t)
  end

  defp output_actions(st0, stateJumps, stateInfo) do
    y2CL =
      for {_State, {actions, j}} <- stateJumps,
          {_LA, r_shift(state: newState)} <-
            actions ++
              (for {_Tag, _To, part} <- [j],
                   a <- part do
                 a
               end) do
        newState
      end

    y2CS = :ordsets.from_list([0 | y2CL])

    y2S =
      :ordsets.from_list(
        for {s, _} <- stateJumps do
          s
        end
      )

    nY2CS = :ordsets.subtract(y2S, y2CS)

    sel =
      for s <- :ordsets.to_list(y2CS) do
        {s, true}
      end ++
        for s <- :ordsets.to_list(nY2CS) do
          {s, false}
        end

    selS =
      for {{state, _JActions}, {state, called}} <-
            :lists.zip(
              stateJumps,
              :lists.keysort(1, sel)
            ) do
        {state, called}
      end

    st05 = fwrite(st0, "-dialyzer({nowarn_function, yeccpars2/7}).\n", [])

    st10 =
      foldl(
        fn {state, called}, st_0 ->
          {^state, r_state_info(state_repr: iState)} = lookup_state(stateInfo, state)
          output_state_selection(st_0, state, iState, called)
        end,
        st05,
        selS
      )

    st20 = fwrite(st10, "yeccpars2(Other, _, _, _, _, _, _) ->\n", [])

    st =
      fwrite(
        st20,
        :unicode.characters_to_binary([
          ' erlang:error({yecc_bug,"',
          '1.4',
          '",',
          :io_lib.fwrite(
            "{missing_state_in_action_table, Other}",
            []
          ),
          '}).\n\n'
        ]),
        []
      )

    foldl(
      fn {state, jActions}, st_0 ->
        {^state, r_state_info(state_repr: iState)} = lookup_state(stateInfo, state)
        output_state_actions(st_0, state, iState, jActions, stateInfo)
      end,
      st,
      stateJumps
    )
  end

  defp output_state_selection(st0, state, iState, called) do
    comment =
      for false <- [called] do
        "%% "
      end

    st = fwrite(st0, "~syeccpars2(~w=S, Cat, Ss, Stack, T, Ts, Tzr) ->\n", [comment, state])
    fwrite(st, "~s yeccpars2_~w(S, Cat, Ss, Stack, T, Ts, Tzr);\n", [comment, iState])
  end

  defp output_state_actions(st, state, state, {actions, :jump_none}, sI) do
    st1 = output_state_actions_begin(st, state, actions)
    output_state_actions1(st1, state, actions, true, :normal, sI)
  end

  defp output_state_actions(st0, state, state, {actions, jump}, sI) do
    {tag, to, common} = jump

    cS =
      case tag do
        :jump_some ->
          :erlang.list_to_atom(:lists.concat([:cont_, to]))

        :jump_all ->
          to
      end

    st = output_state_actions1(st0, state, actions, true, {:to, cS}, sI)

    cond do
      to === state ->
        st1 = output_state_actions_begin(st, state, actions)
        output_state_actions1(st1, cS, common, true, :normal, sI)

      true ->
        st
    end
  end

  defp output_state_actions(st, state, jState, _XActions, _SI) do
    fwrite(st, "%% yeccpars2_~w: see yeccpars2_~w\n\n", [state, jState])
  end

  defp output_state_actions_begin(st, state, actions) do
    case (for {_, r_reduce()} <- actions do
            :yes
          end) do
      [] ->
        fwrite(st, "-dialyzer({nowarn_function, yeccpars2_~w/7}).\n", [state])

      _ ->
        st
    end
  end

  defp output_state_actions1(st, state, [], isFirst, :normal, _SI) do
    output_state_actions_fini(state, isFirst, st)
  end

  defp output_state_actions1(st0, state, [], isFirst, {:to, toS}, _SI) do
    st = delim(st0, isFirst)

    fwrite(
      st,
      "yeccpars2_~w(S, Cat, Ss, Stack, T, Ts, Tzr) ->\n yeccpars2_~w(S, Cat, Ss, Stack, T, Ts, Tzr).\n\n",
      [state, toS]
    )
  end

  defp output_state_actions1(st0, state, [{_, r_reduce() = action}], isFirst, _End, sI) do
    st = output_reduce(st0, state, 'Cat', action, isFirst, sI)
    fwrite(st, ".\n\n", [])
  end

  defp output_state_actions1(st0, state, [{lookahead, action} | tail], isFirst, end__, sI) do
    {_, st} =
      foldl(
        fn terminal, {isFst, st_0} ->
          {false, output_action(st_0, state, terminal, action, isFst, sI)}
        end,
        {isFirst, st0},
        lookahead
      )

    output_state_actions1(st, state, tail, false, end__, sI)
  end

  defp output_action(st, state, terminal, r_reduce() = action, isFirst, sI) do
    output_reduce(st, state, terminal, action, isFirst, sI)
  end

  defp output_action(st0, state, terminal, r_shift(state: newState), isFirst, _SI) do
    st10 = delim(st0, isFirst)

    st =
      fwrite(st10, "yeccpars2_~w(S, ~ts, Ss, Stack, T, Ts, Tzr) ->\n", [
        state,
        quoted_atom(st10, terminal)
      ])

    output_call_to_includefile(newState, st)
  end

  defp output_action(st0, state, terminal, :accept, isFirst, _SI) do
    st10 = delim(st0, isFirst)

    st =
      fwrite(st10, "yeccpars2_~w(_S, ~ts, _Ss, Stack, _T, _Ts, _Tzr) ->\n", [
        state,
        quoted_atom(st10, terminal)
      ])

    fwrite(st, " {ok, hd(Stack)}", [])
  end

  defp output_action(st, _State, _Terminal, :nonassoc, _IsFirst, _SI) do
    st
  end

  defp output_call_to_includefile(
         newState,
         r_yecc(includefile_version: {1, 1}) = st
       ) do
    fwrite(st, " yeccpars1(Ts, Tzr, ~w, [S | Ss], [T | Stack])", [newState])
  end

  defp output_call_to_includefile(newState, st) do
    fwrite(st, " yeccpars1(S, ~w, Ss, Stack, T, Ts, Tzr)", [newState])
  end

  defp output_state_actions_fini(state, isFirst, st0) do
    st10 = delim(st0, isFirst)
    st = fwrite(st10, "yeccpars2_~w(_, _, _, _, T, _, _) ->\n", [state])
    fwrite(st, " yeccerror(T).\n\n", [])
  end

  defp output_reduce(
         st0,
         state,
         terminal,
         r_reduce(rule_nmbr: ruleNmbr, head: head, nmbr_of_daughters: nmbrOfDaughters),
         isFirst,
         stateInfo
       ) do
    st10 = delim(st0, isFirst)

    quotedTerminal =
      cond do
        is_atom(terminal) ->
          quoted_atom(st10, terminal)

        true ->
          terminal
      end

    st20 =
      fwrite(st10, "yeccpars2_~w(_S, ~ts, Ss, Stack, T, Ts, Tzr) ->\n", [state, quotedTerminal])

    st30 =
      cond do
        nmbrOfDaughters < 2 ->
          ns = 'Ss'
          st20

        true ->
          ns = 'Nss'

          tmp =
            :lists.join(
              ',',
              :lists.duplicate(nmbrOfDaughters - 1, '_')
            )

          fwrite(st20, " [~s|Nss] = Ss,\n", [tmp])
      end

    st40 =
      case tokens(ruleNmbr, st30) do
        [{:var, _, :__1}] when nmbrOfDaughters === 1 ->
          newStack = 'Stack'
          st30

        _ ->
          newStack = 'NewStack'
          fwrite(st30, " NewStack = ~w(Stack),\n", [inlined_function_name(st30, state, terminal)])
      end

    cond do
      nmbrOfDaughters === 0 ->
        nextState = goto(state, head, st40)
        {^nextState, i} = lookup_state(stateInfo, nextState)
        r_state_info(reduce_only: rO, state_repr: repr, comment: c) = i

        cond do
          rO ->
            nextS = '_S'

          true ->
            nextS = :io_lib.fwrite('~w', [nextState])
        end

        st = fwrite(st40, "~s", [c])

        fwrite(st, " yeccpars2_~w(~s, ~ts, [~w | Ss], ~s, T, Ts, Tzr)", [
          repr,
          nextS,
          quotedTerminal,
          state,
          newStack
        ])

      true ->
        fwrite(st40, " ~w(hd(~s), ~ts, ~s, ~s, T, Ts, Tzr)", [
          function_name(st40, :yeccgoto, head),
          ns,
          quotedTerminal,
          ns,
          newStack
        ])
    end
  end

  defp delim(st, true) do
    st
  end

  defp delim(st, false) do
    fwrite(st, ";\n", [])
  end

  defp quoted_atom(r_yecc(encoding: :latin1), atom)
       when is_atom(atom) do
    :io_lib.write_atom_as_latin1(atom)
  end

  defp quoted_atom(_St, atomic) do
    :io_lib.write(atomic)
  end

  defp output_inlined(st, userCodeActions, infile) do
    foldl(
      fn r_user_code(
           funname: inlinedFunctionName,
           action: action
         ),
         st_0 ->
        output_inlined(st_0, inlinedFunctionName, action, infile)
      end,
      st,
      userCodeActions
    )
  end

  defp output_inlined(st0, functionName, reduce, infile) do
    r_reduce(
      rule_nmbr: ruleNmbr,
      nmbr_of_daughters: n_daughters
    ) = reduce

    r_rule(
      tokens: tokens,
      is_well_formed: wF
    ) = get_rule(ruleNmbr, st0)

    line0 = first_line(tokens)
    nLines = last_line(tokens) - line0

    st5 =
      cond do
        wF ->
          st0

        not wF ->
          r_yecc(outfile: outfile, line: curLine) = st0
          output_file_directive(st0, outfile, curLine)
      end

    codeStartLine = :lists.max([0, line0 - 4])
    st10 = fwrite(st5, "-compile({inline,~w/1}).\n", [functionName])
    st20 = output_file_directive(st10, infile, codeStartLine)
    st30 = fwrite(st20, "~w(__Stack0) ->\n", [functionName])

    st40 =
      case n_daughters do
        0 ->
          stack = '__Stack0'
          st30

        _ ->
          stack = '__Stack'

          a =
            concat(
              flatmap(
                fn i ->
                  [',__', i]
                end,
                :lists.seq(n_daughters, 1, -1)
              )
            )

          fwrite(st30, " ~s = __Stack0,\n", [append(['[', tl(a), ' | __Stack]'])])
      end

    st = r_yecc(st40, line: r_yecc(st40, :line) + nLines)

    fwrite(st, " [begin\n  ~ts\n  end | ~s].\n\n", [
      pp_tokens(tokens, line0, r_yecc(st, :encoding)),
      stack
    ])
  end

  defp inlined_function_name(st, state, terminal) do
    end__ =
      case terminal do
        'Cat' ->
          []

        _ ->
          [quoted_atom(st, terminal)]
      end

    :erlang.list_to_atom(concat([:yeccpars2_, state, :_] ++ end__))
  end

  defp function_name(st, name, suf) do
    :erlang.list_to_atom(concat([name, :_] ++ [quoted_atom(st, suf)]))
  end

  defp rule(rulePointer, st) do
    r_rule(n: n, anno: anno, symbols: symbols) =
      :maps.get(
        rulePointer,
        r_yecc(st, :rule_pointer2rule)
      )

    {symbols, anno, n}
  end

  defp get_rule(ruleNmbr, st) do
    :maps.get(ruleNmbr, r_yecc(st, :rule_pointer2rule))
  end

  defp tokens(ruleNmbr, st) do
    rule = :maps.get(ruleNmbr, r_yecc(st, :rule_pointer2rule))
    r_rule(rule, :tokens)
  end

  defp goto(from, symbol, st) do
    case :ets.lookup(r_yecc(st, :goto_tab), {from, symbol}) do
      [{_, to}] ->
        to

      [] ->
        :erlang.error({:error_in_goto_table, from, symbol})
    end
  end

  defp set_empty() do
    0
  end

  defp set_add(i, bM) do
    1 <<< i ||| bM
  end

  defp set_member(i, bM) do
    1 <<< i &&& bM !== 0
  end

  defp set_delete(i, bM) do
    1 <<< (i ^^^ bM)
  end

  defp set_union(bM1, bM2) do
    bM1 ||| bM2
  end

  defp set_is_subset(bM1, bM2) do
    bM1 &&& bM2 === bM1
  end

  defp empty_member(bM) do
    set_member(0, bM)
  end

  defp empty_delete(bM) do
    set_delete(0, bM)
  end

  defp code_symbols(ss, symbolTable) do
    map(
      fn s ->
        :ets.lookup_element(symbolTable, s, 2)
      end,
      ss
    )
  end

  defp decode_symbol(c, invSymbolTable) do
    :ets.lookup_element(invSymbolTable, c, 1)
  end

  defp code_terminal(t, symbolTab) do
    set_add(:ets.lookup_element(symbolTab, t, 2), 0)
  end

  defp decode_terminals(bM, invSymbolTab) do
    case :erlang.get(bM) do
      :undefined ->
        symbols = decode_terminals(bM, 0, invSymbolTab)
        :erlang.put(bM, symbols)
        symbols

      symbols ->
        symbols
    end
  end

  defp decode_terminals(0, _I, _InvSymbolTab) do
    []
  end

  defp decode_terminals(bM, i, invSymbolTab) do
    case set_member(i, bM) do
      true ->
        [
          :ets.lookup_element(invSymbolTab, i, 1)
          | decode_terminals(set_delete(i, bM), i + 1, invSymbolTab)
        ]

      false ->
        decode_terminals(bM, i + 1, invSymbolTab)
    end
  end

  defp set_add_terminal({_Symbol, terminalNum}, bM) do
    set_add(terminalNum, bM)
  end

  defp is_terminal(_Tab, symbolCode) do
    symbolCode >= 0
  end

  defp left_corner_symbol_table(st) do
    r_yecc(st, :inv_symbol_tab)
  end

  defp intersect(l1, l2) do
    :ordsets.to_list(
      :ordsets.intersection(
        :ordsets.from_list(l1),
        :ordsets.from_list(l2)
      )
    )
  end

  defp format_symbols([sym | syms]) do
    concat([format_symbol(sym) | format_symbols1(syms)])
  end

  defp format_symbols1([]) do
    []
  end

  defp format_symbols1([h | t]) do
    [' ', format_symbol(h) | format_symbols1(t)]
  end

  defp include(st, file, outport) do
    case :file.open(file, [:read]) do
      {:error, reason} ->
        throw(add_error(file, :none, {:file_error, reason}, st))

      {:ok, inport} ->
        _ = :epp.set_encoding(inport)
        line = :io.get_line(inport, :"")

        try do
          include1(line, inport, outport, file, 1, st) - 1
        after
          :ok = :file.close(inport)
        end
    end
  end

  defp include1(:eof, _, _, _File, l, _St) do
    l
  end

  defp include1({:error, _} = _Error, _Inport, _Outport, file, l, st) do
    throw(add_error(file, :erl_anno.new(l), :cannot_parse, st))
  end

  defp include1(line, inport, outport, file, l, st) do
    incr =
      case member(?\n, line) do
        true ->
          1

        false ->
          0
      end

    :io.put_chars(outport, line)
    include1(:io.get_line(inport, :""), inport, outport, file, l + incr, st)
  end

  defp includefile_version([]) do
    {1, 4}
  end

  defp includefile_version(includefile) do
    case :epp.open(includefile, []) do
      {:ok, epp} ->
        try do
          parse_file(epp)
        after
          :epp.close(epp)
        end

      {:error, _Error} ->
        {1, 1}
    end
  end

  defp parse_file(epp) do
    case :epp.parse_erl_form(epp) do
      {:ok, {:function, _Anno, :yeccpars1, 7, _Clauses}} ->
        {1, 4}

      {:eof, _Line} ->
        {1, 1}

      _Form ->
        parse_file(epp)
    end
  end

  defp pp_tokens(tokens, line0, enc) do
    concat(pp_tokens1(tokens, line0, enc, []))
  end

  defp pp_tokens1([], _Line0, _Enc, _T0) do
    []
  end

  defp pp_tokens1([t | ts], line0, enc, t0) do
    line = location(anno(t))

    [
      pp_sep(line, line0, t0),
      pp_symbol(t, enc)
      | pp_tokens1(ts, line, enc, t)
    ]
  end

  defp pp_symbol({:var, _, var}, _Enc) do
    var
  end

  defp pp_symbol({:string, _, string}, :latin1) do
    :io_lib.write_string_as_latin1(string)
  end

  defp pp_symbol({:string, _, string}, _Enc) do
    :io_lib.write_string(string)
  end

  defp pp_symbol({_, _, symbol}, :latin1) do
    :io_lib.fwrite("~p", [symbol])
  end

  defp pp_symbol({_, _, symbol}, _Enc) do
    :io_lib.fwrite("~tp", [symbol])
  end

  defp pp_symbol({symbol, _}, _Enc) do
    symbol
  end

  defp pp_sep(line, line0, t0) when line > line0 do
    ['\n   ' | pp_sep(line - 1, line0, t0)]
  end

  defp pp_sep(_Line, _Line0, {:., _}) do
    ''
  end

  defp pp_sep(_Line, _Line0, _T0) do
    ' '
  end

  defp set_encoding(r_yecc(encoding: :none), port) do
    :ok =
      :io.setopts(
        port,
        [{:encoding, :epp.default_encoding()}]
      )
  end

  defp set_encoding(r_yecc(encoding: e), port) do
    :ok = :io.setopts(port, [{:encoding, e}])
  end

  defp output_encoding_comment(r_yecc(encoding: :none) = st) do
    st
  end

  defp output_encoding_comment(r_yecc(encoding: encoding) = st) do
    fwrite(st, "%% ~s\n", [:epp.encoding_to_string(encoding)])
  end

  defp output_file_directive(st, filename, line) when r_yecc(st, :file_attrs) do
    fwrite(st, "-file(~ts, ~w).\n", [format_filename(filename, st), line])
  end

  defp output_file_directive(st, _Filename, _Line) do
    st
  end

  defp first_line(tokens) do
    location(anno(hd(tokens)))
  end

  defp last_line(tokens) do
    location(anno(:lists.last(tokens)))
  end

  defp location(:none) do
    :none
  end

  defp location(anno) do
    :erl_anno.line(anno)
  end

  defp anno(token) do
    :erlang.element(2, token)
  end

  defp fwrite(r_yecc(outport: outport, line: line) = st, format, args) do
    nLines = count_nl(format)
    :io.fwrite(outport, format, args)
    r_yecc(st, line: line + nLines)
  end

  defp count_nl(<<?\n, rest::binary>>) do
    1 + count_nl(rest)
  end

  defp count_nl(<<_, rest::binary>>) do
    count_nl(rest)
  end

  defp count_nl(<<>>) do
    0
  end

  defp nl(r_yecc(outport: outport, line: line) = st) do
    :io.nl(outport)
    r_yecc(st, line: line + 1)
  end

  defp format_filename(filename0, st) do
    filename = :filename.flatten(filename0)

    case :lists.keyfind(:encoding, 1, :io.getopts(r_yecc(st, :outport))) do
      {:encoding, :unicode} ->
        :io_lib.write_string(filename)

      _ ->
        :io_lib.write_string_as_latin1(filename)
    end
  end

  defp format_assoc(:left) do
    'Left'
  end

  defp format_assoc(:right) do
    'Right'
  end

  defp format_assoc(:unary) do
    'Unary'
  end

  defp format_assoc(:nonassoc) do
    'Nonassoc'
  end

  defp format_symbol(symbol) do
    string = concat([symbol])

    case :erl_scan.string(string) do
      {:ok, [{:atom, _, _}], _} ->
        :io_lib.fwrite("~tw", [symbol])

      {:ok, [{word, _}], _} when word !== :":" and word !== :-> ->
        case :erl_scan.reserved_word(word) do
          true ->
            string

          false ->
            :io_lib.fwrite("~tw", [symbol])
        end

      {:ok, [{:var, _, _}], _} ->
        string

      _ ->
        :io_lib.fwrite("~tw", [symbol])
    end
  end

  defp inverse(l) do
    sort(
      for {b, a} <- l do
        {a, b}
      end
    )
  end

  defp family(l) do
    :sofs.to_external(:sofs.relation_to_family(:sofs.relation(l)))
  end

  defp seq1(to) when to < 1 do
    []
  end

  defp seq1(to) do
    :lists.seq(1, to)
  end

  defp count(from, l) do
    :lists.zip(l, :lists.seq(from, length(l) - 1 + from))
  end

  defp family_with_domain(l, dL) do
    :sofs.to_external(
      sofs_family_with_domain(
        :sofs.relation(l),
        :sofs.set(dL)
      )
    )
  end

  defp sofs_family_with_domain(r0, d) do
    r = :sofs.restriction(r0, d)
    f = :sofs.relation_to_family(r)
    fD = :sofs.constant_function(d, :sofs.from_term([]))
    :sofs.family_union(f, fD)
  end
end
