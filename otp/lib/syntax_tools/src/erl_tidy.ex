defmodule :m_erl_tidy do
  use Bitwise
  require Record

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

  defp dir__defaults() do
    [{:follow_links, false}, :recursive, {:regexp, '.*\\.erl$'}, :verbose]
  end

  def dir() do
    dir('')
  end

  def dir(dir) do
    dir(dir, [])
  end

  Record.defrecord(:r_dir, :dir, follow_links: false, recursive: true, options: :undefined)

  def dir(dir, opts) do
    opts1 = opts ++ dir__defaults()

    env =
      r_dir(
        follow_links:
          :proplists.get_bool(
            :follow_links,
            opts1
          ),
        recursive: :proplists.get_bool(:recursive, opts1),
        options: opts1
      )

    regexp = :proplists.get_value(:regexp, opts1)

    case filename(dir) do
      '' ->
        dir1 = '.'

      dir1 ->
        :ok
    end

    dir_1(dir1, regexp, env)
  end

  defp dir_1(dir, regexp, env) do
    case :file.list_dir(dir) do
      {:ok, files} ->
        :lists.foreach(
          fn x ->
            dir_2(x, regexp, dir, env)
          end,
          files
        )

      {:error, _} ->
        report_error('error reading directory `~ts\'', [filename(dir)])
        exit(:error)
    end
  end

  defp dir_2(name, regexp, dir, env) do
    file =
      cond do
        dir === '' ->
          name

        true ->
          :filename.join(dir, name)
      end

    case file_type(file) do
      {:value, :regular} ->
        dir_4(file, regexp, env)

      {:value, :directory} when r_dir(env, :recursive) === true ->
        case is_symlink(name) do
          false ->
            dir_3(name, dir, regexp, env)

          true when r_dir(env, :follow_links) === true ->
            dir_3(name, dir, regexp, env)

          _ ->
            :ok
        end

      _ ->
        :ok
    end
  end

  defp dir_3(name, dir, regexp, env) do
    dir1 = :filename.join(dir, name)
    verbose('tidying directory `~ts\'.', [dir1], r_dir(env, :options))
    dir_1(dir1, regexp, env)
  end

  defp dir_4(file, regexp, env) do
    case :re.run(file, regexp, [:unicode]) do
      {:match, _} ->
        opts = [
          [{:outfile, file}, {:dir, ''}]
          | r_dir(env, :options)
        ]

        case (try do
                file(file, opts)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, value} ->
            warn('error tidying `~ts\'.~n~p', [file, value], opts)

          _ ->
            :ok
        end

      :nomatch ->
        :ok
    end
  end

  defp file__defaults() do
    [
      {:backup_suffix, '.bak'},
      :backups,
      {:dir, ''},
      {:printer, default_printer()},
      {:quiet, false},
      {:verbose, false}
    ]
  end

  defp default_printer() do
    &:erl_prettypr.format/2
  end

  def file(name) do
    file(name, [])
  end

  def file(name, opts) do
    parent = self()

    child =
      spawn_link(fn ->
        file_1(parent, name, opts)
      end)

    receive do
      {^child, :ok} ->
        :ok

      {^child, {:error, reason}} ->
        exit(reason)

      {:EXIT, ^child, reason} ->
        exit(reason)
    end
  end

  defp file_1(parent, name, opts) do
    try do
      file_2(name, opts)
    catch
      :syntax_error ->
        send(parent, {self(), :ok})

      :error, reason ->
        send(parent, {self(), {:error, reason}})
    else
      _ ->
        send(parent, {self(), :ok})
    end
  end

  defp file_2(name, opts) do
    opts1 = opts ++ file__defaults()
    {forms, emptyLines} = read_module(name, opts1)
    opts2 = [{:empty_lines, emptyLines} | opts1]
    comments = :erl_comment_scan.file(name)
    forms1 = :erl_recomment.recomment_forms(forms, comments)
    tree = module(forms1, [{:file, name} | opts1])

    case :proplists.get_bool(:test, opts1) do
      true ->
        :ok

      false ->
        case :proplists.get_bool(:stdout, opts1) do
          true ->
            print_module(tree, opts2)
            :ok

          false ->
            write_module(tree, name, opts2)
            :ok
        end
    end
  end

  defp read_module(name, opts) do
    verbose('reading module `~ts\'.', [filename(name)], opts)

    case :epp_dodger.parse_file(name, [:no_fail]) do
      {:ok, forms} ->
        {forms, empty_lines(name)}

      {:error, r} ->
        error_read_file(name)
        exit({:error, r})
    end
  end

  defp empty_lines(name) do
    {:ok, data} = :file.read_file(name)
    list = :binary.split(data, ["\n"], [:global])
    {:ok, nonEmptyLineRe} = :re.compile('\\S')

    {res, _} =
      :lists.foldl(
        fn line, {set, n} ->
          case :re.run(line, nonEmptyLineRe) do
            {:match, _} ->
              {set, n + 1}

            :nomatch ->
              {:sets.add_element(n, set), n + 1}
          end
        end,
        {:sets.new(), 1},
        list
      )

    res
  end

  defp write_module(tree, name, opts) do
    name1 = :proplists.get_value(:outfile, opts, filename(name))
    dir = filename(:proplists.get_value(:dir, opts, ''))

    file =
      cond do
        dir === '' ->
          name1

        true ->
          case file_type(dir) do
            {:value, :directory} ->
              :ok

            {:value, _} ->
              report_error('`~ts\' is not a directory.', [filename(dir)])
              exit(:error)

            :none ->
              case :file.make_dir(dir) do
                :ok ->
                  verbose('created directory `~ts\'.', [filename(dir)], opts)
                  :ok

                e ->
                  report_error('failed to create directory `~ts\'.', [filename(dir)])
                  exit({:make_dir, e})
              end
          end

          filename(:filename.join(dir, name1))
      end

    encoding =
      for enc <- [:epp.read_encoding(name)],
          enc !== :none do
        {:encoding, enc}
      end

    case :proplists.get_bool(:backups, opts) do
      true ->
        backup_file(file, opts)

      false ->
        :ok
    end

    printer = :proplists.get_value(:printer, opts)
    fD = open_output_file(file, encoding)
    verbose('writing to file `~ts\'.', [file], opts)

    v =
      try do
        {:ok, output(fD, printer, tree, opts ++ encoding)}
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    :ok = :file.close(fD)

    case v do
      {:ok, _} ->
        file

      {:EXIT, r} ->
        error_write_file(file)
        exit(r)

      r ->
        error_write_file(file)
        throw(r)
    end
  end

  defp print_module(tree, opts) do
    printer = :proplists.get_value(:printer, opts)
    :io.put_chars(printer.(tree, opts))
  end

  defp output(fD, printer, tree, opts) do
    :io.put_chars(fD, printer.(tree, opts))
    :io.nl(fD)
  end

  defp file_type(name) do
    file_type(name, false)
  end

  defp is_symlink(name) do
    file_type(name, true) === {:value, :symlink}
  end

  defp file_type(name, links) do
    v =
      case links do
        true ->
          try do
            :file.read_link_info(name)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        false ->
          try do
            :file.read_file_info(name)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
      end

    case v do
      {:ok, env} ->
        {:value, r_file_info(env, :type)}

      {:error, :enoent} ->
        :none

      {:error, r} ->
        error_read_file(name)
        exit({:error, r})

      {:EXIT, r} ->
        error_read_file(name)
        exit(r)

      r ->
        error_read_file(name)
        throw(r)
    end
  end

  defp open_output_file(fName, options) do
    case (try do
            :file.open(fName, [:write] ++ options)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, fD} ->
        fD

      {:error, r} ->
        error_open_output(fName)
        exit({:error, r})

      {:EXIT, r} ->
        error_open_output(fName)
        exit(r)

      r ->
        error_open_output(fName)
        exit(r)
    end
  end

  defp backup_file(name, opts) do
    case file_type(name) do
      {:value, :regular} ->
        backup_file_1(name, opts)

      {:value, _} ->
        error_backup_file(name)
        exit(:error)

      :none ->
        :ok
    end
  end

  defp backup_file_1(name, opts) do
    suffix = :proplists.get_value(:backup_suffix, opts, '')

    dest =
      :filename.join(
        :filename.dirname(name),
        :filename.basename(name) ++ suffix
      )

    case (try do
            :file.rename(name, dest)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :ok ->
        verbose('made backup of file `~ts\'.', [name], opts)

      {:error, r} ->
        error_backup_file(name)
        exit({:error, r})

      {:EXIT, r} ->
        error_backup_file(name)
        exit(r)

      r ->
        error_backup_file(name)
        throw(r)
    end
  end

  def module(forms) do
    module(forms, [])
  end

  def module(forms, opts) when is_list(forms) do
    module(:erl_syntax.form_list(forms), opts)
  end

  def module(forms, opts) do
    opts1 =
      :proplists.expand(
        module__expansions(),
        opts
      ) ++ module__defaults()

    file = :proplists.get_value(:file, opts1, '')
    forms1 = :erl_syntax.flatten_form_list(forms)
    module_1(forms1, file, opts1)
  end

  defp module__defaults() do
    [
      {:auto_export_vars, false},
      {:auto_list_comp, true},
      {:keep_unused, false},
      {:new_guard_tests, true},
      {:no_imports, false},
      {:old_guard_tests, false},
      {:quiet, false},
      {:verbose, false}
    ]
  end

  defp module__expansions() do
    [
      {:idem,
       [
         {:auto_export_vars, false},
         {:auto_list_comp, false},
         {:keep_unused, true},
         {:new_guard_tests, false},
         {:no_imports, false},
         {:old_guard_tests, false}
       ]}
    ]
  end

  defp module_1(forms, file, opts) do
    info = analyze_forms(forms, file)
    module = get_module_name(info, file)
    attrs = get_module_attributes(info)
    exports = get_module_exports(info)
    imports = get_module_imports(info)
    opts1 = check_imports(imports, opts, file)
    fs = :erl_syntax.form_list_elements(forms)
    {names, defs} = collect_functions(fs)
    exports1 = check_export_all(attrs, names, exports)

    roots =
      :ordsets.union(
        :ordsets.from_list(exports1),
        hidden_uses(fs, imports)
      )

    {names1, used, imported, defs1} = visit_used(names, defs, roots, imports, module, opts1)
    fs1 = update_forms(fs, defs1, imported, opts1)
    fs2 = filter_forms(fs1, names1, used, opts1)
    rewrite(forms, :erl_syntax.form_list(fs2))
  end

  defp analyze_forms(forms, file) do
    case (try do
            {:ok, :erl_syntax_lib.analyze_forms(forms)}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, l1} ->
        l1

      :syntax_error ->
        report_error({file, 0, 'syntax error.'})
        throw(:syntax_error)

      {:EXIT, r} ->
        exit(r)

      r ->
        throw(r)
    end
  end

  defp get_module_name(list, file) do
    case :lists.keyfind(:module, 1, list) do
      {:module, m} ->
        m

      _ ->
        report_error({file, 0, 'cannot determine module name.'})
        exit(:error)
    end
  end

  defp get_module_attributes(list) do
    case :lists.keyfind(:attributes, 1, list) do
      {:attributes, as} ->
        as

      _ ->
        []
    end
  end

  defp get_module_exports(list) do
    case :lists.keyfind(:exports, 1, list) do
      {:exports, es} ->
        es

      _ ->
        []
    end
  end

  defp get_module_imports(list) do
    case :lists.keyfind(:imports, 1, list) do
      {:imports, is} ->
        flatten_imports(is)

      _ ->
        []
    end
  end

  defp compile_attrs(as) do
    :lists.append(
      for {:compile, t} <- as do
        cond do
          is_list(t) ->
            t

          true ->
            [t]
        end
      end
    )
  end

  defp flatten_imports(is) do
    for {m, fs} <- is, f <- fs do
      {f, m}
    end
  end

  defp check_imports(is, opts, file) do
    case check_imports_1(:lists.sort(is)) do
      true ->
        opts

      false ->
        case :proplists.get_bool(:no_imports, opts) do
          true ->
            warn(
              {file, 0, 'conflicting import declarations - will not expand imports.'},
              [],
              opts
            )

            [{:no_imports, false} | opts]

          false ->
            opts
        end
    end
  end

  defp check_imports_1([[{f, m1}, {f, m2}] | _Is]) when m1 !== m2 do
    false
  end

  defp check_imports_1([_ | is]) do
    check_imports_1(is)
  end

  defp check_imports_1([]) do
    true
  end

  defp check_export_all(attrs, names, exports) do
    case :lists.member(
           :export_all,
           compile_attrs(attrs)
         ) do
      true ->
        exports ++ :sets.to_list(names)

      false ->
        exports
    end
  end

  defp filter_forms(fs, names, used, opts) do
    keep =
      case :proplists.get_bool(
             :keep_unused,
             opts
           ) do
        true ->
          names

        false ->
          used
      end

    for f <- fs, keep_form(f, keep, opts) do
      f
    end
  end

  defp keep_form(form, used, opts) do
    case :erl_syntax.type(form) do
      :function ->
        n = :erl_syntax_lib.analyze_function(form)

        case :sets.is_element(n, used) do
          false ->
            {f, a} = n
            file = :proplists.get_value(:file, opts, '')

            report(
              {file, :erl_syntax.get_pos(form), 'removing unused function `~tw/~w\'.'},
              [f, a],
              opts
            )

            false

          true ->
            true
        end

      :attribute ->
        case :erl_syntax_lib.analyze_attribute(form) do
          {:file, _} ->
            false

          _ ->
            true
        end

      :error_marker ->
        false

      :warning_marker ->
        false

      :eof_marker ->
        false

      _ ->
        true
    end
  end

  defp collect_functions(forms) do
    :lists.foldl(
      fn f, {names, defs} ->
        case :erl_syntax.type(f) do
          :function ->
            n = :erl_syntax_lib.analyze_function(f)
            {:sets.add_element(n, names), :dict.store(n, {f, []}, defs)}

          _ ->
            {names, defs}
        end
      end,
      {:sets.new(), :dict.new()},
      forms
    )
  end

  defp update_forms([f | fs], defs, imports, opts) do
    case :erl_syntax.type(f) do
      :function ->
        n = :erl_syntax_lib.analyze_function(f)
        {f1, fs1} = :dict.fetch(n, defs)
        [f1 | :lists.reverse(fs1)] ++ update_forms(fs, defs, imports, opts)

      :attribute ->
        [update_attribute(f, imports, opts) | update_forms(fs, defs, imports, opts)]

      _ ->
        [f | update_forms(fs, defs, imports, opts)]
    end
  end

  defp update_forms([], _, _, _) do
    []
  end

  defp update_attribute(f, imports, opts) do
    case :erl_syntax_lib.analyze_attribute(f) do
      {:import, {m, ns}} ->
        ns1 =
          :ordsets.from_list(
            for n <- ns,
                :sets.is_element(n, imports) do
              n
            end
          )

        case :ordsets.subtract(:ordsets.from_list(ns), ns1) do
          [] ->
            :ok

          names ->
            file = :proplists.get_value(:file, opts, '')

            report(
              {file, :erl_syntax.get_pos(f), 'removing unused imports:~ts'},
              [
                for {n, a} <- names do
                  :io_lib.fwrite('\n\t`~w:~tw/~w\'', [m, n, a])
                end
              ],
              opts
            )
        end

        is =
          for n <- ns1 do
            make_fname(n)
          end

        cond do
          is === [] ->
            :erl_syntax.warning_marker(:deleted)

          true ->
            f1 =
              :erl_syntax.attribute(
                :erl_syntax.atom(:import),
                [:erl_syntax.atom(m), :erl_syntax.list(is)]
              )

            rewrite(f, f1)
        end

      {:export, ns} ->
        es =
          for n <- :ordsets.from_list(ns) do
            make_fname(n)
          end

        f1 =
          :erl_syntax.attribute(
            :erl_syntax.atom(:export),
            [:erl_syntax.list(es)]
          )

        rewrite(f, f1)

      _ ->
        f
    end
  end

  defp make_fname({f, a}) do
    :erl_syntax.arity_qualifier(
      :erl_syntax.atom(f),
      :erl_syntax.integer(a)
    )
  end

  defp hidden_uses(fs, imports) do
    used =
      :lists.foldl(
        fn f, s ->
          case :erl_syntax.type(f) do
            :attribute ->
              hidden_uses_1(f, s)

            _ ->
              s
          end
        end,
        [],
        fs
      )

    :ordsets.subtract(
      used,
      :ordsets.from_list(
        for {f, _M} <- imports do
          f
        end
      )
    )
  end

  defp hidden_uses_1(tree, used) do
    :erl_syntax_lib.fold(&hidden_uses_2/2, used, tree)
  end

  defp hidden_uses_2(tree, used) do
    case :erl_syntax.type(tree) do
      :application ->
        f = :erl_syntax.application_operator(tree)

        case :erl_syntax.type(f) do
          :atom ->
            as = :erl_syntax.application_arguments(tree)
            n = {:erl_syntax.atom_value(f), length(as)}

            case is_auto_imported(n) do
              true ->
                used

              false ->
                :ordsets.add_element(n, used)
            end

          _ ->
            used
        end

      :implicit_fun ->
        f = :erl_syntax.implicit_fun_name(tree)

        case (try do
                {:ok, :erl_syntax_lib.analyze_function_name(f)}
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:ok, {name, arity} = n}
          when is_atom(name) and
                 is_integer(arity) ->
            :ordsets.add_element(n, used)

          _ ->
            used
        end

      _ ->
        used
    end
  end

  Record.defrecord(:r_env, :env,
    file: :undefined,
    module: :undefined,
    current: :undefined,
    imports: :dict.new(),
    context: :normal,
    verbosity: 1,
    quiet: false,
    no_imports: false,
    spawn_funs: false,
    auto_list_comp: true,
    auto_export_vars: false,
    new_guard_tests: true,
    old_guard_tests: false
  )

  Record.defrecord(:r_st, :st,
    varc: :undefined,
    used: :sets.new(),
    imported: :undefined,
    vars: :undefined,
    functions: :undefined,
    new_forms: [],
    rename: :undefined
  )

  defp visit_used(names, defs, roots, imports, module, opts) do
    file = :proplists.get_value(:file, opts, '')
    noImports = :proplists.get_bool(:no_imports, opts)
    rename = :proplists.append_values(:rename, opts)

    loop(
      roots,
      :sets.new(),
      defs,
      r_env(
        file: file,
        module: module,
        imports: :dict.from_list(imports),
        verbosity: verbosity(opts),
        no_imports: noImports,
        spawn_funs: :proplists.get_bool(:spawn_funs, opts),
        auto_list_comp:
          :proplists.get_bool(
            :auto_list_comp,
            opts
          ),
        auto_export_vars:
          :proplists.get_bool(
            :auto_export_vars,
            opts
          ),
        new_guard_tests:
          :proplists.get_bool(
            :new_guard_tests,
            opts
          ),
        old_guard_tests:
          :proplists.get_bool(
            :old_guard_tests,
            opts
          )
      ),
      r_st(
        used: :sets.from_list(roots),
        imported: :sets.new(),
        functions: names,
        rename:
          :dict.from_list(
            for {f1, f2} = x <- rename,
                is_remote_name(f1),
                is_atom_pair(f2) do
              x
            end
          )
      )
    )
  end

  defp loop([f | work], seen0, defs0, env, st0) do
    case :sets.is_element(f, seen0) do
      true ->
        loop(work, seen0, defs0, env, st0)

      false ->
        seen1 = :sets.add_element(f, seen0)

        case :dict.find(f, defs0) do
          {:ok, {form, fs}} ->
            vars = :erl_syntax_lib.variables(form)
            form1 = :erl_syntax_lib.annotate_bindings(form, [])

            {form2, st1} =
              visit(
                form1,
                r_env(env, current: f),
                r_st(st0, varc: 1, used: :sets.new(), vars: vars, new_forms: [])
              )

            fs1 = r_st(st1, :new_forms) ++ fs
            defs1 = :dict.store(f, {form2, fs1}, defs0)
            used = r_st(st1, :used)
            work1 = :sets.to_list(used) ++ work
            st2 = r_st(st1, used: :sets.union(used, r_st(st0, :used)))
            loop(work1, seen1, defs1, env, st2)

          :error ->
            loop(work, seen1, defs0, env, st0)
        end
    end
  end

  defp loop([], _, defs, _, st) do
    {r_st(st, :functions), r_st(st, :used), r_st(st, :imported), defs}
  end

  defp visit(tree, env, st0) do
    case :erl_syntax.type(tree) do
      :application ->
        visit_application(tree, env, st0)

      :infix_expr ->
        visit_infix_expr(tree, env, st0)

      :prefix_expr ->
        visit_prefix_expr(tree, env, st0)

      :implicit_fun ->
        visit_implicit_fun(tree, env, st0)

      :clause ->
        visit_clause(tree, env, st0)

      :list_comp ->
        visit_list_comp(tree, env, st0)

      :match_expr ->
        visit_match_expr(tree, env, st0)

      _ ->
        visit_other(tree, env, st0)
    end
  end

  defp visit_other(tree, env, st) do
    f = fn t, s ->
      visit(t, env, s)
    end

    :erl_syntax_lib.mapfold_subtrees(f, st, tree)
  end

  defp visit_list(ts, env, st0) do
    :lists.mapfoldl(
      fn t, s ->
        visit(t, env, s)
      end,
      st0,
      ts
    )
  end

  defp visit_implicit_fun(tree, _Env, st0) do
    f = :erl_syntax.implicit_fun_name(tree)

    case (try do
            {:ok, :erl_syntax_lib.analyze_function_name(f)}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, {name, arity} = n}
      when is_atom(name) and
             is_integer(arity) ->
        used = :sets.add_element(n, r_st(st0, :used))
        {tree, r_st(st0, used: used)}

      _ ->
        {tree, st0}
    end
  end

  defp visit_clause(tree, env, st0) do
    ps = :erl_syntax.clause_patterns(tree)

    {g, st1} =
      case :erl_syntax.clause_guard(tree) do
        :none ->
          {:none, st0}

        g0 ->
          visit(g0, r_env(env, context: :guard_test), st0)
      end

    {b, st2} = visit_list(:erl_syntax.clause_body(tree), env, st1)
    {rewrite(tree, :erl_syntax.clause(ps, g, b)), st2}
  end

  defp visit_infix_expr(tree, r_env(context: :guard_test), st0) do
    visit_other(tree, r_env(context: :guard_expr, file: ''), st0)
  end

  defp visit_infix_expr(tree, env, st0) do
    visit_other(tree, env, st0)
  end

  defp visit_prefix_expr(tree, r_env(context: :guard_test), st0) do
    visit_other(tree, r_env(context: :guard_expr, file: ''), st0)
  end

  defp visit_prefix_expr(tree, env, st0) do
    visit_other(tree, env, st0)
  end

  defp visit_application(tree, env, st0) do
    env1 =
      case env do
        r_env(context: :guard_test) ->
          r_env(env, context: :guard_expr)

        _ ->
          env
      end

    {f, st1} = visit(:erl_syntax.application_operator(tree), env1, st0)
    {as, st2} = visit_list(:erl_syntax.application_arguments(tree), env1, st1)

    case :erl_syntax.type(f) do
      :atom ->
        visit_atom_application(f, as, tree, env, st2)

      :implicit_fun ->
        visit_named_fun_application(f, as, tree, env, st2)

      :fun_expr ->
        visit_lambda_application(f, as, tree, env, st2)

      _ ->
        visit_nonlocal_application(f, as, tree, env, st2)
    end
  end

  defp visit_application_final(f, as, tree, st0) do
    {rewrite(tree, :erl_syntax.application(f, as)), st0}
  end

  defp revisit_application(f, as, tree, env, st0) do
    visit(rewrite(tree, :erl_syntax.application(f, as)), env, st0)
  end

  defp visit_atom_application(f, as, tree, r_env(context: :guard_test) = env, st0) do
    n = :erl_syntax.atom_value(f)
    a = length(as)

    n1 =
      case r_env(env, :old_guard_tests) do
        true ->
          reverse_guard_test(n, a)

        false ->
          case r_env(env, :new_guard_tests) do
            true ->
              rewrite_guard_test(n, a)

            false ->
              n
          end
      end

    cond do
      n1 !== n ->
        report(
          {r_env(env, :file), :erl_syntax.get_pos(f), 'changing guard test `~w\' to `~w\'.'},
          [n, n1],
          r_env(env, :verbosity)
        )

      true ->
        :ok
    end

    f1 = rewrite(f, :erl_syntax.atom(n1))
    visit_application_final(f1, as, tree, st0)
  end

  defp visit_atom_application(f, as, tree, r_env(context: :guard_expr), st0) do
    visit_application_final(f, as, tree, st0)
  end

  defp visit_atom_application(f, as, tree, env, st0) do
    n = {:erl_syntax.atom_value(f), length(as)}

    case is_auto_imported(n) do
      true ->
        visit_bif_call(n, f, as, tree, env, st0)

      false ->
        case is_imported(n, env) do
          true ->
            visit_import_application(n, f, as, tree, env, st0)

          false ->
            used = :sets.add_element(n, r_st(st0, :used))
            visit_application_final(f, as, tree, r_st(st0, used: used))
        end
    end
  end

  defp visit_import_application({n, a} = name, f, as, tree, env, st0) do
    m = :dict.fetch(name, r_env(env, :imports))

    expand =
      case r_env(env, :no_imports) do
        true ->
          true

        false ->
          auto_expand_import({m, n, a}, st0)
      end

    case expand do
      true ->
        report(
          {r_env(env, :file), :erl_syntax.get_pos(f),
           'expanding call to imported function `~w:~tw/~w\'.'},
          [m, n, a],
          r_env(env, :verbosity)
        )

        f1 =
          :erl_syntax.module_qualifier(
            :erl_syntax.atom(m),
            :erl_syntax.atom(n)
          )

        revisit_application(rewrite(f, f1), as, tree, env, st0)

      false ->
        is = :sets.add_element(name, r_st(st0, :imported))
        visit_application_final(f, as, tree, r_st(st0, imported: is))
    end
  end

  defp visit_bif_call({:apply, 2}, f, [e, args] = as, tree, env, st0) do
    case :erl_syntax.is_proper_list(args) do
      true ->
        report(
          {r_env(env, :file), :erl_syntax.get_pos(f),
           'changing use of `apply/2\' to direct function call.'},
          [],
          r_env(env, :verbosity)
        )

        as1 = :erl_syntax.list_elements(args)
        revisit_application(e, as1, tree, env, st0)

      false ->
        visit_application_final(f, as, tree, st0)
    end
  end

  defp visit_bif_call({:apply, 3}, f, [m, n, args] = as, tree, env, st0) do
    case :erl_syntax.is_proper_list(args) do
      true ->
        report(
          {r_env(env, :file), :erl_syntax.get_pos(f),
           'changing use of `apply/3\' to direct remote call.'},
          [],
          r_env(env, :verbosity)
        )

        f1 = rewrite(f, :erl_syntax.module_qualifier(m, n))
        as1 = :erl_syntax.list_elements(args)
        visit_nonlocal_application(f1, as1, tree, env, st0)

      false ->
        visit_application_final(f, as, tree, st0)
    end
  end

  defp visit_bif_call({:spawn, 3} = n, f, [_, _, _] = as, tree, env, st0) do
    visit_spawn_call(n, f, [], as, tree, env, st0)
  end

  defp visit_bif_call({:spawn_link, 3} = n, f, [_, _, _] = as, tree, env, st0) do
    visit_spawn_call(n, f, [], as, tree, env, st0)
  end

  defp visit_bif_call({:spawn, 4} = n, f, [a | [_, _, _] = as], tree, env, st0) do
    visit_spawn_call(n, f, [a], as, tree, env, st0)
  end

  defp visit_bif_call({:spawn_link, 4} = n, f, [a | [_, _, _] = as], tree, env, st0) do
    visit_spawn_call(n, f, [a], as, tree, env, st0)
  end

  defp visit_bif_call(_, f, as, tree, _Env, st0) do
    visit_application_final(f, as, tree, st0)
  end

  defp visit_spawn_call(
         {n, a},
         f,
         ps,
         [a1, a2, a3] = as,
         tree,
         r_env(spawn_funs: true) = env,
         st0
       ) do
    case :erl_syntax.is_proper_list(a3) do
      true ->
        report(
          {r_env(env, :file), :erl_syntax.get_pos(f),
           'changing use of `~tw/~w\' to `~tw/~w\' with a fun.'},
          [n, a, n, 1 + length(ps)],
          r_env(env, :verbosity)
        )

        f1 =
          case :erl_syntax.is_atom(a1, r_env(env, :module)) do
            true ->
              a2

            false ->
              clone(a1, :erl_syntax.module_qualifier(a1, a2))
          end

        as1 = :erl_syntax.list_elements(a3)
        {vs, st1} = new_variables(length(as1), st0)
        e1 = clone(f1, :erl_syntax.application(f1, vs))
        c1 = clone(e1, :erl_syntax.clause([], [e1]))
        e2 = clone(c1, :erl_syntax.fun_expr([c1]))
        c2 = clone(e2, :erl_syntax.clause(vs, [], [e2]))
        e3 = clone(c2, :erl_syntax.fun_expr([c2]))
        e4 = clone(e3, :erl_syntax.application(e3, as1))
        e5 = :erl_syntax_lib.annotate_bindings(e4, get_env(a1))
        {e6, st2} = visit(e5, env, st1)
        f2 = rewrite(f, :erl_syntax.atom(n))
        visit_nonlocal_application(f2, ps ++ [e6], tree, env, st2)

      false ->
        visit_application_final(f, ps ++ as, tree, st0)
    end
  end

  defp visit_spawn_call(_, f, ps, as, tree, _Env, st0) do
    visit_application_final(f, ps ++ as, tree, st0)
  end

  defp visit_named_fun_application(f, as, tree, env, st0) do
    name = :erl_syntax.implicit_fun_name(f)

    case (try do
            {:ok, :erl_syntax_lib.analyze_function_name(name)}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, {a, n}}
      when is_atom(a) and is_integer(n) and
             n === length(as) ->
        case is_nonlocal({a, n}, env) do
          true ->
            visit_application_final(f, as, tree, st0)

          false ->
            report(
              {r_env(env, :file), :erl_syntax.get_pos(f),
               'changing application of implicit fun to direct local call.'},
              [],
              r_env(env, :verbosity)
            )

            used = :sets.add_element({a, n}, r_st(st0, :used))
            f1 = rewrite(f, :erl_syntax.atom(a))
            revisit_application(f1, as, tree, env, r_st(st0, used: used))
        end

      _ ->
        visit_application_final(f, as, tree, st0)
    end
  end

  defp visit_lambda_application(f, as, tree, env, st0) do
    a = :erl_syntax.fun_expr_arity(f)

    case a === length(as) do
      true ->
        report(
          {r_env(env, :file), :erl_syntax.get_pos(f),
           'changing application of fun-expression to local function call.'},
          [],
          r_env(env, :verbosity)
        )

        {base, _} = r_env(env, :current)

        free =
          for v <- get_free_vars(f) do
            :erl_syntax.variable(v)
          end

        n = length(free)
        a1 = a + n
        {name, st1} = new_fname({base, a1}, st0)

        cs =
          augment_clauses(
            :erl_syntax.fun_expr_clauses(f),
            free
          )

        f1 = :erl_syntax.atom(name)
        new = rewrite(f, :erl_syntax.function(f1, cs))
        used = :sets.add_element({name, a1}, r_st(st1, :used))
        forms = [new | r_st(st1, :new_forms)]
        st2 = r_st(st1, new_forms: forms, used: used)
        visit_application_final(f1, as ++ free, tree, st2)

      false ->
        warn(
          {r_env(env, :file), :erl_syntax.get_pos(f),
           'arity mismatch in fun-expression application.'},
          [],
          r_env(env, :verbosity)
        )

        visit_application_final(f, as, tree, st0)
    end
  end

  defp augment_clauses(cs, vs) do
    for c <- cs do
      ps = :erl_syntax.clause_patterns(c)
      g = :erl_syntax.clause_guard(c)
      es = :erl_syntax.clause_body(c)
      rewrite(c, :erl_syntax.clause(ps ++ vs, g, es))
    end
  end

  defp visit_nonlocal_application(f, as, tree, env, st0) do
    case :erl_syntax.type(f) do
      :tuple ->
        case :erl_syntax.tuple_elements(f) do
          [x1, x2] ->
            report(
              {r_env(env, :file), :erl_syntax.get_pos(f),
               'changing application of 2-tuple to direct remote call.'},
              [],
              r_env(env, :verbosity)
            )

            f1 = :erl_syntax.module_qualifier(x1, x2)
            revisit_application(rewrite(f, f1), as, tree, env, st0)

          _ ->
            visit_application_final(f, as, tree, st0)
        end

      :module_qualifier ->
        case (try do
                {:ok, :erl_syntax_lib.analyze_function_name(f)}
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:ok, {m, n}} when is_atom(m) and is_atom(n) ->
            visit_remote_application({m, n, length(as)}, f, as, tree, env, st0)

          _ ->
            visit_application_final(f, as, tree, st0)
        end

      _ ->
        visit_application_final(f, as, tree, st0)
    end
  end

  defp visit_remote_application({:lists, :append, 2}, f, [a1, a2], tree, env, st0) do
    report(
      {r_env(env, :file), :erl_syntax.get_pos(f),
       'replacing call to `lists:append/2\' with the `++\' operator.'},
      [],
      r_env(env, :verbosity)
    )

    tree1 = :erl_syntax.infix_expr(a1, :erl_syntax.operator(:++), a2)
    visit(rewrite(tree, tree1), env, st0)
  end

  defp visit_remote_application({:lists, :subtract, 2}, f, [a1, a2], tree, env, st0) do
    report(
      {r_env(env, :file), :erl_syntax.get_pos(f),
       'replacing call to `lists:subtract/2\' with the `--\' operator.'},
      [],
      r_env(env, :verbosity)
    )

    tree1 = :erl_syntax.infix_expr(a1, :erl_syntax.operator(:--), a2)
    visit(rewrite(tree, tree1), env, st0)
  end

  defp visit_remote_application({:lists, :filter, 2}, f, [a1, a2] = as, tree, env, st0) do
    case :erlang.and(
           :erlang.and(
             :erlang.and(
               r_env(env, :auto_list_comp),
               :erl_syntax.type(a1) !== :variable
             ),
             get_var_exports(a1) === []
           ),
           get_var_exports(a2) === []
         ) do
      true ->
        report(
          {r_env(env, :file), :erl_syntax.get_pos(f),
           'replacing call to `lists:filter/2\' with a list comprehension.'},
          [],
          r_env(env, :verbosity)
        )

        {v, st1} = new_variable(st0)
        g = clone(a2, :erl_syntax.generator(v, a2))
        t = clone(a1, :erl_syntax.application(a1, [v]))
        l = :erl_syntax.list_comp(v, [g, t])
        l1 = :erl_syntax_lib.annotate_bindings(l, get_env(tree))
        visit(rewrite(tree, l1), env, st1)

      false ->
        visit_application_final(f, as, tree, st0)
    end
  end

  defp visit_remote_application({:lists, :map, 2}, f, [a1, a2] = as, tree, env, st0) do
    case :erlang.and(
           :erlang.and(
             :erlang.and(
               r_env(env, :auto_list_comp),
               :erl_syntax.type(a1) !== :variable
             ),
             get_var_exports(a1) === []
           ),
           get_var_exports(a2) === []
         ) do
      true ->
        report(
          {r_env(env, :file), :erl_syntax.get_pos(f),
           'replacing call to `lists:map/2\' with a list comprehension.'},
          [],
          r_env(env, :verbosity)
        )

        {v, st1} = new_variable(st0)
        t = clone(a1, :erl_syntax.application(a1, [v]))
        g = clone(a2, :erl_syntax.generator(v, a2))
        l = :erl_syntax.list_comp(t, [g])
        l1 = :erl_syntax_lib.annotate_bindings(l, get_env(tree))
        visit(rewrite(tree, l1), env, st1)

      false ->
        visit_application_final(f, as, tree, st0)
    end
  end

  defp visit_remote_application({m, n, a} = name, f, as, tree, env, st) do
    case is_auto_imported(name) do
      true ->
        visit_bif_call({n, a}, f, as, tree, env, st)

      false ->
        case rename_remote_call(name, st) do
          {m1, n1} ->
            report(
              {r_env(env, :file), :erl_syntax.get_pos(f),
               'updating obsolete call to `~w:~tw/~w\' to use `~w:~tw/~w\' instead.'},
              [m, n, a, m1, n1, a],
              r_env(env, :verbosity)
            )

            m2 = :erl_syntax.atom(m1)
            n2 = :erl_syntax.atom(n1)
            f1 = :erl_syntax.module_qualifier(m2, n2)
            revisit_application(rewrite(f, f1), as, tree, env, st)

          false ->
            visit_application_final(f, as, tree, st)
        end
    end
  end

  defp auto_expand_import({:lists, :append, 2}, _St) do
    true
  end

  defp auto_expand_import({:lists, :subtract, 2}, _St) do
    true
  end

  defp auto_expand_import({:lists, :filter, 2}, _St) do
    true
  end

  defp auto_expand_import({:lists, :map, 2}, _St) do
    true
  end

  defp auto_expand_import(name, st) do
    case is_auto_imported(name) do
      true ->
        true

      false ->
        rename_remote_call(name, st) !== false
    end
  end

  defp visit_list_comp(tree, env, st0) do
    es = :erl_syntax.list_comp_body(tree)
    {es1, st1} = visit_list_comp_body(es, env, st0)
    {t, st2} = visit(:erl_syntax.list_comp_template(tree), env, st1)
    {rewrite(tree, :erl_syntax.list_comp(t, es1)), st2}
  end

  defp visit_list_comp_body_join(env) do
    fn e, st0 ->
      case is_generator(e) do
        true ->
          visit_generator(e, env, st0)

        false ->
          visit_filter(e, env, st0)
      end
    end
  end

  defp visit_list_comp_body(es, env, st0) do
    :lists.mapfoldl(visit_list_comp_body_join(env), st0, es)
  end

  defp visit_filter(e, env, st0) do
    visit(e, env, st0)
  end

  defp visit_generator(g, env, st0) do
    p = :erl_syntax.generator_pattern(g)

    case :erl_syntax.type(p) do
      :variable ->
        b = :erl_syntax.generator_body(g)

        case :erl_syntax.type(b) do
          :list_comp ->
            t = :erl_syntax.list_comp_template(b)

            case :erl_syntax.type(t) do
              :variable ->
                visit_generator_1(g, env, st0)

              _ ->
                visit_filter(g, env, st0)
            end

          _ ->
            visit_filter(g, env, st0)
        end

      _ ->
        visit_filter(g, env, st0)
    end
  end

  defp visit_generator_1(g, env, st0) do
    recommend(
      {r_env(env, :file), :erl_syntax.get_pos(g),
       'unfold that this nested list comprehension can be unfolded by hand to get better efficiency.'},
      [],
      r_env(env, :verbosity)
    )

    visit_filter(g, env, st0)
  end

  defp visit_match_expr(tree, env, st0) do
    p = :erl_syntax.match_expr_pattern(tree)
    {b, st1} = visit(:erl_syntax.match_expr_body(tree), env, st0)

    case :erl_syntax.type(p) do
      :tuple ->
        ps = :erl_syntax.tuple_elements(p)

        case :lists.all(&is_variable/1, ps) do
          true ->
            vs =
              :lists.sort(
                for x <- ps do
                  :erl_syntax.variable_name(x)
                end
              )

            case :ordsets.is_set(vs) do
              true ->
                xs = get_var_exports(b)

                case :ordsets.intersection(vs, xs) do
                  [] ->
                    visit_match_body(ps, p, b, tree, env, st1)

                  _ ->
                    visit_match_expr_final(p, b, tree, env, st1)
                end

              false ->
                visit_match_expr_final(p, b, tree, env, st1)
            end

          false ->
            visit_match_expr_final(p, b, tree, env, st1)
        end

      _ ->
        visit_match_expr_final(p, b, tree, env, st1)
    end
  end

  defp visit_match_expr_final(p, b, tree, _Env, st0) do
    {rewrite(tree, :erl_syntax.match_expr(p, b)), st0}
  end

  defp visit_match_body(_Ps, p, b, tree, r_env(auto_export_vars: false) = env, st0) do
    visit_match_expr_final(p, b, tree, env, st0)
  end

  defp visit_match_body(ps, p, b, tree, env, st0) do
    case :erl_syntax.type(b) do
      :case_expr ->
        cs = :erl_syntax.case_expr_clauses(b)

        case multival_clauses(cs, length(ps), ps) do
          {true, cs1} ->
            report_export_vars(
              r_env(env, :file),
              :erl_syntax.get_pos(b),
              'case',
              r_env(env, :verbosity)
            )

            a = :erl_syntax.case_expr_argument(b)
            tree1 = :erl_syntax.case_expr(a, cs1)
            {rewrite(tree, tree1), st0}

          false ->
            visit_match_expr_final(p, b, tree, env, st0)
        end

      :if_expr ->
        cs = :erl_syntax.if_expr_clauses(b)

        case multival_clauses(cs, length(ps), ps) do
          {true, cs1} ->
            report_export_vars(
              r_env(env, :file),
              :erl_syntax.get_pos(b),
              'if',
              r_env(env, :verbosity)
            )

            tree1 = :erl_syntax.if_expr(cs1)
            {rewrite(tree, tree1), st0}

          false ->
            visit_match_expr_final(p, b, tree, env, st0)
        end

      :receive_expr ->
        as = :erl_syntax.receive_expr_action(b)
        c = :erl_syntax.clause([], as)
        cs = :erl_syntax.receive_expr_clauses(b)

        case multival_clauses([c | cs], length(ps), ps) do
          {true, [c1 | cs1]} ->
            report_export_vars(
              r_env(env, :file),
              :erl_syntax.get_pos(b),
              'receive',
              r_env(env, :verbosity)
            )

            t = :erl_syntax.receive_expr_timeout(b)
            as1 = :erl_syntax.clause_body(c1)
            tree1 = :erl_syntax.receive_expr(cs1, t, as1)
            {rewrite(tree, tree1), st0}

          false ->
            visit_match_expr_final(p, b, tree, env, st0)
        end

      _ ->
        visit_match_expr_final(p, b, tree, env, st0)
    end
  end

  defp multival_clauses(cs, n, vs) do
    multival_clauses(cs, n, vs, [])
  end

  defp multival_clauses([c | cs], n, vs, cs1) do
    case :erl_syntax.clause_body(c) do
      [] ->
        false

      es ->
        e = :lists.last(es)

        case :erl_syntax.type(e) do
          :tuple ->
            ts = :erl_syntax.tuple_elements(e)

            cond do
              length(ts) === n ->
                bs = make_matches(e, vs, ts)
                es1 = replace_last(es, bs)
                ps = :erl_syntax.clause_patterns(c)
                g = :erl_syntax.clause_guard(c)
                c1 = :erl_syntax.clause(ps, g, es1)
                multival_clauses(cs, n, vs, [rewrite(c, c1) | cs1])

              true ->
                false
            end

          _ ->
            case :erl_syntax_lib.is_fail_expr(e) do
              true ->
                bs = make_matches(vs, :erl_syntax.atom(false))
                es1 = replace_last(es, bs ++ [e])
                ps = :erl_syntax.clause_patterns(c)
                g = :erl_syntax.clause_guard(c)
                c1 = :erl_syntax.clause(ps, g, es1)
                multival_clauses(cs, n, vs, [rewrite(c, c1) | cs1])

              false ->
                false
            end
        end
    end
  end

  defp multival_clauses([], _N, _Vs, cs) do
    {true, :lists.reverse(cs)}
  end

  defp make_matches(e, vs, ts) do
    case make_matches(vs, ts) do
      [] ->
        []

      [b | bs] ->
        [rewrite(e, b) | bs]
    end
  end

  defp make_matches([v | vs], [t | ts]) do
    [:erl_syntax.match_expr(v, t) | make_matches(vs, ts)]
  end

  defp make_matches([v | vs], t) when t !== [] do
    [:erl_syntax.match_expr(v, t) | make_matches(vs, t)]
  end

  defp make_matches([], _) do
    []
  end

  defp rename_remote_call(f, st) do
    case :dict.find(f, r_st(st, :rename)) do
      :error ->
        rename_remote_call_1(f)

      {:ok, f1} ->
        f1
    end
  end

  defp rename_remote_call_1({:dict, :dict_to_list, 1}) do
    {:dict, :to_list}
  end

  defp rename_remote_call_1({:dict, :list_to_dict, 1}) do
    {:dict, :from_list}
  end

  defp rename_remote_call_1({:erl_eval, :arg_list, 2}) do
    {:erl_eval, :expr_list}
  end

  defp rename_remote_call_1({:erl_eval, :arg_list, 3}) do
    {:erl_eval, :expr_list}
  end

  defp rename_remote_call_1({:erl_eval, :seq, 2}) do
    {:erl_eval, :exprs}
  end

  defp rename_remote_call_1({:erl_eval, :seq, 3}) do
    {:erl_eval, :exprs}
  end

  defp rename_remote_call_1({:erl_pp, :seq, 1}) do
    {:erl_eval, :seq}
  end

  defp rename_remote_call_1({:erl_pp, :seq, 2}) do
    {:erl_eval, :seq}
  end

  defp rename_remote_call_1({:erlang, :info, 1}) do
    {:erlang, :system_info}
  end

  defp rename_remote_call_1({:io, :parse_erl_seq, 1}) do
    {:io, :parse_erl_exprs}
  end

  defp rename_remote_call_1({:io, :parse_erl_seq, 2}) do
    {:io, :parse_erl_exprs}
  end

  defp rename_remote_call_1({:io, :parse_erl_seq, 3}) do
    {:io, :parse_erl_exprs}
  end

  defp rename_remote_call_1({:io, :scan_erl_seq, 1}) do
    {:io, :scan_erl_exprs}
  end

  defp rename_remote_call_1({:io, :scan_erl_seq, 2}) do
    {:io, :scan_erl_exprs}
  end

  defp rename_remote_call_1({:io, :scan_erl_seq, 3}) do
    {:io, :scan_erl_exprs}
  end

  defp rename_remote_call_1({:io_lib, :reserved_word, 1}) do
    {:erl_scan, :reserved_word}
  end

  defp rename_remote_call_1({:io_lib, :scan, 1}) do
    {:erl_scan, :string}
  end

  defp rename_remote_call_1({:io_lib, :scan, 2}) do
    {:erl_scan, :string}
  end

  defp rename_remote_call_1({:io_lib, :scan, 3}) do
    {:erl_scan, :tokens}
  end

  defp rename_remote_call_1({:orddict, :dict_to_list, 1}) do
    {:orddict, :to_list}
  end

  defp rename_remote_call_1({:orddict, :list_to_dict, 1}) do
    {:orddict, :from_list}
  end

  defp rename_remote_call_1({:ordsets, :list_to_set, 1}) do
    {:ordsets, :from_list}
  end

  defp rename_remote_call_1({:ordsets, :new_set, 0}) do
    {:ordsets, :new}
  end

  defp rename_remote_call_1({:ordsets, :set_to_list, 1}) do
    {:ordsets, :to_list}
  end

  defp rename_remote_call_1({:ordsets, :subset, 2}) do
    {:ordsets, :is_subset}
  end

  defp rename_remote_call_1({:sets, :list_to_set, 1}) do
    {:sets, :from_list}
  end

  defp rename_remote_call_1({:sets, :new_set, 0}) do
    {:sets, :new}
  end

  defp rename_remote_call_1({:sets, :set_to_list, 1}) do
    {:sets, :to_list}
  end

  defp rename_remote_call_1({:sets, :subset, 2}) do
    {:sets, :is_subset}
  end

  defp rename_remote_call_1({:string, :index, 2}) do
    {:string, :str}
  end

  defp rename_remote_call_1({:unix, :cmd, 1}) do
    {:os, :cmd}
  end

  defp rename_remote_call_1(_) do
    false
  end

  defp rewrite_guard_test(:atom, 1) do
    :is_atom
  end

  defp rewrite_guard_test(:binary, 1) do
    :is_binary
  end

  defp rewrite_guard_test(:constant, 1) do
    :is_constant
  end

  defp rewrite_guard_test(:float, 1) do
    :is_float
  end

  defp rewrite_guard_test(:function, 1) do
    :is_function
  end

  defp rewrite_guard_test(:function, 2) do
    :is_function
  end

  defp rewrite_guard_test(:integer, 1) do
    :is_integer
  end

  defp rewrite_guard_test(:list, 1) do
    :is_list
  end

  defp rewrite_guard_test(:number, 1) do
    :is_number
  end

  defp rewrite_guard_test(:pid, 1) do
    :is_pid
  end

  defp rewrite_guard_test(:port, 1) do
    :is_port
  end

  defp rewrite_guard_test(:reference, 1) do
    :is_reference
  end

  defp rewrite_guard_test(:tuple, 1) do
    :is_tuple
  end

  defp rewrite_guard_test(:record, 2) do
    :is_record
  end

  defp rewrite_guard_test(:record, 3) do
    :is_record
  end

  defp rewrite_guard_test(n, _A) do
    n
  end

  defp reverse_guard_test(:is_atom, 1) do
    :atom
  end

  defp reverse_guard_test(:is_binary, 1) do
    :binary
  end

  defp reverse_guard_test(:is_constant, 1) do
    :constant
  end

  defp reverse_guard_test(:is_float, 1) do
    :float
  end

  defp reverse_guard_test(:is_function, 1) do
    :function
  end

  defp reverse_guard_test(:is_function, 2) do
    :function
  end

  defp reverse_guard_test(:is_integer, 1) do
    :integer
  end

  defp reverse_guard_test(:is_list, 1) do
    :list
  end

  defp reverse_guard_test(:is_number, 1) do
    :number
  end

  defp reverse_guard_test(:is_pid, 1) do
    :pid
  end

  defp reverse_guard_test(:is_port, 1) do
    :port
  end

  defp reverse_guard_test(:is_reference, 1) do
    :reference
  end

  defp reverse_guard_test(:is_tuple, 1) do
    :tuple
  end

  defp reverse_guard_test(:is_record, 2) do
    :record
  end

  defp reverse_guard_test(:is_record, 3) do
    :record
  end

  defp reverse_guard_test(n, _A) do
    n
  end

  defp is_remote_name({m, f, a})
       when is_atom(m) and is_atom(f) and
              is_integer(a) do
    true
  end

  defp is_remote_name(_) do
    false
  end

  defp is_atom_pair({m, f}) when is_atom(m) and is_atom(f) do
    true
  end

  defp is_atom_pair(_) do
    false
  end

  defp replace_last([_E], xs) do
    xs
  end

  defp replace_last([e | es], xs) do
    [e | replace_last(es, xs)]
  end

  defp is_generator(e) do
    :erl_syntax.type(e) === :generator
  end

  defp is_variable(e) do
    :erl_syntax.type(e) === :variable
  end

  defp new_variables(n, st0) when n > 0 do
    {v, st1} = new_variable(st0)
    {vs, st2} = new_variables(n - 1, st1)
    {[v | vs], st2}
  end

  defp new_variables(0, st) do
    {[], st}
  end

  defp new_variable(st0) do
    fun = fn n ->
      :erlang.list_to_atom('V' ++ :erlang.integer_to_list(n))
    end

    vs = r_st(st0, :vars)
    {name, n} = new_name(r_st(st0, :varc), fun, vs)

    st1 =
      r_st(st0,
        varc: n + 1,
        vars: :sets.add_element(name, vs)
      )

    {:erl_syntax.variable(name), st1}
  end

  defp new_fname({f, a}, st0) do
    base = :erlang.atom_to_list(f)

    fun = fn n ->
      {:erlang.list_to_atom(base ++ '_' ++ :erlang.integer_to_list(n)), a}
    end

    fs = r_st(st0, :functions)
    {{f1, _A} = name, _N} = new_name(1, fun, fs)
    {f1, r_st(st0, functions: :sets.add_element(name, fs))}
  end

  defp new_name(n, f, set) do
    name = f.(n)

    case :sets.is_element(name, set) do
      true ->
        new_name(n + 1, f, set)

      false ->
        {name, n}
    end
  end

  defp is_imported(f, env) do
    :dict.is_key(f, r_env(env, :imports))
  end

  defp is_auto_imported({:erlang, n, a}) do
    is_auto_imported({n, a})
  end

  defp is_auto_imported({_, _N, _A}) do
    false
  end

  defp is_auto_imported({n, a}) do
    :erl_internal.bif(n, a)
  end

  defp is_nonlocal(n, env) do
    case is_imported(n, env) do
      true ->
        true

      false ->
        is_auto_imported(n)
    end
  end

  defp get_var_exports(node) do
    get_var_exports_1(:erl_syntax.get_ann(node))
  end

  defp get_var_exports_1([{:bound, b} | _Bs]) do
    b
  end

  defp get_var_exports_1([_ | bs]) do
    get_var_exports_1(bs)
  end

  defp get_var_exports_1([]) do
    []
  end

  defp get_free_vars(node) do
    get_free_vars_1(:erl_syntax.get_ann(node))
  end

  defp get_free_vars_1([{:free, b} | _Bs]) do
    b
  end

  defp get_free_vars_1([_ | bs]) do
    get_free_vars_1(bs)
  end

  defp get_free_vars_1([]) do
    []
  end

  defp filename([c | t]) when is_integer(c) and c > 0 do
    [c | filename(t)]
  end

  defp filename([h | t]) do
    filename(h) ++ filename(t)
  end

  defp filename([]) do
    []
  end

  defp filename(n) when is_atom(n) do
    :erlang.atom_to_list(n)
  end

  defp filename(n) do
    report_error('bad filename: `~tP\'.', [n, 25])
    exit(:error)
  end

  defp get_env(tree) do
    case :lists.keyfind(:env, 1, :erl_syntax.get_ann(tree)) do
      {:env, env} ->
        env

      _ ->
        []
    end
  end

  defp rewrite(source, target) do
    :erl_syntax.copy_attrs(source, target)
  end

  defp clone(source, target) do
    :erl_syntax.copy_pos(source, target)
  end

  defp report_export_vars(f, l, type, opts) do
    report({f, l, 'rewrote ~s-expression to export variables.'}, [type], opts)
  end

  defp error_read_file(name) do
    report_error('error reading file `~ts\'.', [filename(name)])
  end

  defp error_write_file(name) do
    report_error('error writing to file `~ts\'.', [filename(name)])
  end

  defp error_backup_file(name) do
    report_error('could not create backup of file `~ts\'.', [filename(name)])
  end

  defp error_open_output(name) do
    report_error('cannot open file `~ts\' for output.', [filename(name)])
  end

  defp verbosity(opts) do
    case :proplists.get_bool(:quiet, opts) do
      true ->
        0

      false ->
        case :proplists.get_value(:verbose, opts) do
          true ->
            2

          n when is_integer(n) ->
            n

          _ ->
            1
        end
    end
  end

  defp report_error(d) do
    report_error(d, [])
  end

  defp report_error({f, l, d}, vs) do
    report({f, l, {:error, d}}, vs)
  end

  defp report_error(d, vs) do
    report({:error, d}, vs)
  end

  defp warn({f, l, d}, vs, n) do
    report({f, l, {:warning, d}}, vs, n)
  end

  defp warn(d, vs, n) do
    report({:warning, d}, vs, n)
  end

  defp recommend(d, vs, n) do
    report({:recommend, d}, vs, n)
  end

  defp verbose(d, vs, n) do
    report(2, d, vs, n)
  end

  defp report(d, vs) do
    report(d, vs, 1)
  end

  defp report(d, vs, n) do
    report(1, d, vs, n)
  end

  defp report(level, _D, _Vs, n)
       when is_integer(n) and
              n < level do
    :ok
  end

  defp report(_Level, d, vs, n) when is_integer(n) do
    :io.put_chars(format(d, vs))
  end

  defp report(level, d, vs, options) when is_list(options) do
    report(level, d, vs, verbosity(options))
  end

  defp format({:error, d}, vs) do
    ['error: ', format(d, vs)]
  end

  defp format({:warning, d}, vs) do
    ['warning: ', format(d, vs)]
  end

  defp format({:recommend, d}, vs) do
    ['recommendation: ', format(d, vs)]
  end

  defp format({'', l, d}, vs) when is_integer(l) and l > 0 do
    [:io_lib.fwrite('~tw: ', [l]), format(d, vs)]
  end

  defp format({'', _L, d}, vs) do
    format(d, vs)
  end

  defp format({f, l, d}, vs) when is_integer(l) and l > 0 do
    [:io_lib.fwrite('~ts:~tw: ', [filename(f), l]), format(d, vs)]
  end

  defp format({f, _L, d}, vs) do
    [:io_lib.fwrite('~ts: ', [filename(f)]), format(d, vs)]
  end

  defp format(s, vs) when is_list(s) do
    [:io_lib.fwrite(s, vs), ?\n]
  end
end
