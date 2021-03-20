defmodule :m_filelib do
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

  def wildcard(pattern) when is_list(pattern) do
    try do
      do_wildcard(pattern, '.', :file)
    catch
      :error, {:badpattern, _} = unUsUalVaRiAbLeNaMe ->
        :erlang.error(unUsUalVaRiAbLeNaMe)
    end
  end

  def wildcard(pattern, cwd)
      when is_list(pattern) and
             is_list(cwd) do
    try do
      do_wildcard(pattern, cwd, :file)
    catch
      :error, {:badpattern, _} = unUsUalVaRiAbLeNaMe ->
        :erlang.error(unUsUalVaRiAbLeNaMe)
    end
  end

  def wildcard(pattern, mod)
      when is_list(pattern) and
             is_atom(mod) do
    try do
      do_wildcard(pattern, '.', mod)
    catch
      :error, {:badpattern, _} = unUsUalVaRiAbLeNaMe ->
        :erlang.error(unUsUalVaRiAbLeNaMe)
    end
  end

  def wildcard(pattern, cwd, mod)
      when is_list(pattern) and
             is_list(cwd) and is_atom(mod) do
    try do
      do_wildcard(pattern, cwd, mod)
    catch
      :error, {:badpattern, _} = unUsUalVaRiAbLeNaMe ->
        :erlang.error(unUsUalVaRiAbLeNaMe)
    end
  end

  def is_dir(dir) do
    do_is_dir(dir, :file)
  end

  def is_dir(dir, mod) when is_atom(mod) do
    do_is_dir(dir, mod)
  end

  def is_file(file) do
    do_is_file(file, :file)
  end

  def is_file(file, mod) when is_atom(mod) do
    do_is_file(file, mod)
  end

  def is_regular(file) do
    do_is_regular(file, :file)
  end

  def is_regular(file, mod) when is_atom(mod) do
    do_is_regular(file, mod)
  end

  def fold_files(dir, regExp, recursive, fun, acc) do
    do_fold_files(dir, regExp, recursive, fun, acc, :file)
  end

  def fold_files(dir, regExp, recursive, fun, acc, mod)
      when is_atom(mod) do
    do_fold_files(dir, regExp, recursive, fun, acc, mod)
  end

  def last_modified(file) do
    do_last_modified(file, :file)
  end

  def last_modified(file, mod) when is_atom(mod) do
    do_last_modified(file, mod)
  end

  def file_size(file) do
    do_file_size(file, :file)
  end

  def file_size(file, mod) when is_atom(mod) do
    do_file_size(file, mod)
  end

  defp do_is_dir(dir, mod) do
    case eval_read_file_info(dir, mod) do
      {:ok, r_file_info(type: :directory)} ->
        true

      _ ->
        false
    end
  end

  defp do_is_file(file, mod) do
    case eval_read_file_info(file, mod) do
      {:ok, r_file_info(type: :regular)} ->
        true

      {:ok, r_file_info(type: :directory)} ->
        true

      _ ->
        false
    end
  end

  defp do_is_regular(file, mod) do
    case eval_read_file_info(file, mod) do
      {:ok, r_file_info(type: :regular)} ->
        true

      _ ->
        false
    end
  end

  defp do_fold_files(dir, regExp, recursive, fun, acc, mod) do
    {:ok, re1} = :re.compile(regExp, [:unicode])
    do_fold_files1(dir, re1, regExp, recursive, fun, acc, mod)
  end

  defp do_fold_files1(dir, regExp, origRE, recursive, fun, acc, mod) do
    case eval_list_dir(dir, mod) do
      {:ok, files} ->
        do_fold_files2(files, dir, regExp, origRE, recursive, fun, acc, mod)

      {:error, _} ->
        acc
    end
  end

  defp do_fold_files2([], _Dir, _RegExp, _OrigRE, _Recursive, _Fun, acc, _Mod) do
    acc
  end

  defp do_fold_files2([file | t], dir, regExp, origRE, recursive, fun, acc0, mod) do
    fullName = :filename.join(dir, file)

    case do_is_regular(fullName, mod) do
      true ->
        case (try do
                :re.run(
                  file,
                  cond do
                    is_binary(file) ->
                      origRE

                    true ->
                      regExp
                  end,
                  [{:capture, :none}]
                )
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          :match ->
            acc = fun.(fullName, acc0)
            do_fold_files2(t, dir, regExp, origRE, recursive, fun, acc, mod)

          {:EXIT, _} ->
            do_fold_files2(t, dir, regExp, origRE, recursive, fun, acc0, mod)

          :nomatch ->
            do_fold_files2(t, dir, regExp, origRE, recursive, fun, acc0, mod)
        end

      false ->
        case recursive and do_is_dir(fullName, mod) do
          true ->
            acc1 = do_fold_files1(fullName, regExp, origRE, recursive, fun, acc0, mod)
            do_fold_files2(t, dir, regExp, origRE, recursive, fun, acc1, mod)

          false ->
            do_fold_files2(t, dir, regExp, origRE, recursive, fun, acc0, mod)
        end
    end
  end

  defp do_last_modified(file, mod) do
    case eval_read_file_info(file, mod) do
      {:ok, info} ->
        r_file_info(info, :mtime)

      _ ->
        0
    end
  end

  defp do_file_size(file, mod) do
    case eval_read_file_info(file, mod) do
      {:ok, info} ->
        r_file_info(info, :size)

      _ ->
        0
    end
  end

  def ensure_dir('/') do
    :ok
  end

  def ensure_dir(f) do
    dir = :filename.dirname(f)

    case do_is_dir(dir, :file) do
      true ->
        :ok

      false when dir === f ->
        {:error, :einval}

      false ->
        _ = ensure_dir(dir)

        case :file.make_dir(dir) do
          {:error, :eexist} = eExist ->
            case do_is_dir(dir, :file) do
              true ->
                :ok

              false ->
                eExist
            end

          err ->
            err
        end
    end
  end

  defp do_wildcard(pattern, cwd, mod) do
    {compiled, prefixLen} = compile_wildcard(pattern, cwd)
    files0 = do_wildcard_1(compiled, mod)

    files =
      cond do
        prefixLen === 0 ->
          files0

        true ->
          for file <- files0 do
            :lists.nthtail(prefixLen, file)
          end
      end

    :lists.sort(files)
  end

  defp do_wildcard_1({:exists, file}, mod) do
    case exists(file, mod) do
      true ->
        [file]

      false ->
        []
    end
  end

  defp do_wildcard_1([base | rest], mod) do
    do_wildcard_2([base], rest, [], mod)
  end

  defp do_wildcard_2([file | rest], pattern, result, mod) do
    do_wildcard_2(rest, pattern, do_wildcard_3(file, pattern, result, mod), mod)
  end

  defp do_wildcard_2([], _, result, _Mod) do
    result
  end

  defp do_wildcard_3(base, [[:double_star] | rest], result, mod) do
    do_double_star('.', [base], rest, result, mod, true)
  end

  defp do_wildcard_3(base, ['..' | rest], result, mod) do
    case do_is_dir(base, mod) do
      true ->
        matches = [:filename.join(base, '..')]
        do_wildcard_2(matches, rest, result, mod)

      false ->
        result
    end
  end

  defp do_wildcard_3(base0, [pattern | rest], result, mod) do
    case do_list_dir(base0, mod) do
      {:ok, files} ->
        base = prepare_base(base0)
        matches = do_wildcard_4(pattern, base, files)
        do_wildcard_2(matches, rest, result, mod)

      _ ->
        result
    end
  end

  defp do_wildcard_3(base, [], result, _Mod) do
    [base | result]
  end

  defp do_wildcard_4(pattern, base, files) do
    case will_always_match(pattern) do
      false ->
        for f <- files, match_part(pattern, f) do
          base ++ f
        end

      true ->
        for f <- files do
          base ++ f
        end
    end
  end

  defp match_part([:question | rest1], [_ | rest2]) do
    match_part(rest1, rest2)
  end

  defp match_part([:accept], _) do
    true
  end

  defp match_part([:double_star], _) do
    true
  end

  defp match_part([:star | rest], file) do
    do_star(rest, file)
  end

  defp match_part([{:one_of, ordset} | rest], [c | file]) do
    :gb_sets.is_element(c, ordset) and
      match_part(
        rest,
        file
      )
  end

  defp match_part([{:alt, alts}], file) do
    do_alt(alts, file)
  end

  defp match_part([c | rest1], [c | rest2]) when is_integer(c) do
    match_part(rest1, rest2)
  end

  defp match_part([x | _], [y | _])
       when is_integer(x) and
              is_integer(y) do
    false
  end

  defp match_part([], []) do
    true
  end

  defp match_part([], [_ | _]) do
    false
  end

  defp match_part([_ | _], []) do
    false
  end

  defp will_always_match([:accept]) do
    true
  end

  defp will_always_match([:double_star]) do
    true
  end

  defp will_always_match(_) do
    false
  end

  defp prepare_base(base0) do
    base1 = :filename.join(base0, 'x')
    'x' ++ base2 = :lists.reverse(base1)
    :lists.reverse(base2)
  end

  defp do_double_star(base, [h | t], patterns, result0, mod, root) do
    full =
      case root do
        false ->
          :filename.join(base, h)

        true ->
          h
      end

    result1 =
      case do_list_dir(full, mod) do
        {:ok, files} ->
          do_double_star(full, files, patterns, result0, mod, false)

        _ ->
          result0
      end

    result2 =
      case patterns do
        _ when root ->
          result1

        [] ->
          [full | result1]

        [pattern | rest] ->
          case match_part(pattern, h) do
            true ->
              do_wildcard_2([full], rest, result1, mod)

            false ->
              result1
          end
      end

    do_double_star(base, t, patterns, result2, mod, root)
  end

  defp do_double_star(_Base, [], _Patterns, result, _Mod, _Root) do
    result
  end

  defp do_star(pattern, [_ | rest] = file) do
    match_part(pattern, file) or do_star(pattern, rest)
  end

  defp do_star(pattern, []) do
    match_part(pattern, [])
  end

  defp do_alt([alt | rest], file) do
    match_part(alt, file) or do_alt(rest, file)
  end

  defp do_alt([], _File) do
    false
  end

  defp do_list_dir(dir, mod) do
    eval_list_dir(dir, mod)
  end

  def compile_wildcard(pattern) when is_list(pattern) do
    {:compiled_wildcard,
     try do
       compile_wildcard(pattern, '.')
     catch
       :error, {:badpattern, _} = unUsUalVaRiAbLeNaMe ->
         :erlang.error(unUsUalVaRiAbLeNaMe)
     end}
  end

  defp compile_wildcard(pattern0, cwd0) do
    pattern = convert_escapes(pattern0)
    [root | rest] = :filename.split(pattern)

    case :filename.pathtype(root) do
      :relative ->
        cwd = prepare_base(cwd0)
        compile_wildcard_2([root | rest], {:cwd, cwd})

      _ ->
        compile_wildcard_2(rest, {:root, 0, root})
    end
  end

  defp compile_wildcard_2([part | rest], root) do
    pattern = compile_part(part)

    case is_literal_pattern(pattern) do
      true ->
        compile_wildcard_2(rest, compile_join(root, pattern))

      false ->
        compile_wildcard_3(rest, [pattern, root])
    end
  end

  defp compile_wildcard_2([], {:root, prefixLen, root}) do
    {{:exists, root}, prefixLen}
  end

  defp is_literal_pattern([h | t]) do
    is_integer(h) and is_literal_pattern(t)
  end

  defp is_literal_pattern([]) do
    true
  end

  defp compile_wildcard_3([part | rest], result) do
    compile_wildcard_3(rest, [compile_part(part) | result])
  end

  defp compile_wildcard_3([], result) do
    case :lists.reverse(result) do
      [{:root, prefixLen, root} | compiled] ->
        {[root | compiled], prefixLen}

      [{:cwd, root} | compiled] ->
        {[root | compiled], length(:filename.join(root, 'x')) - 1}
    end
  end

  defp compile_join({:cwd, '.'}, file) do
    {:root, 0, file}
  end

  defp compile_join({:cwd, cwd}, file0) do
    file = :filename.join([file0])
    root = :filename.join(cwd, file)
    prefixLen = length(root) - length(file)
    {:root, prefixLen, root}
  end

  defp compile_join({:root, prefixLen, root}, file) do
    {:root, prefixLen, :filename.join(root, file)}
  end

  defp compile_part(part0) do
    part = wrap_escapes(part0)
    compile_part(part, false, [])
  end

  defp compile_part_to_sep(part) do
    compile_part(part, true, [])
  end

  defp compile_part([], true, _) do
    badpattern(:missing_delimiter)
  end

  defp compile_part([?, | rest], true, result) do
    {:ok, ?,, :lists.reverse(result), rest}
  end

  defp compile_part([?} | rest], true, result) do
    {:ok, ?}, :lists.reverse(result), rest}
  end

  defp compile_part([?? | rest], upto, result) do
    compile_part(rest, upto, [:question | result])
  end

  defp compile_part([?*, ?*], upto, result) do
    compile_part([], upto, [:double_star | result])
  end

  defp compile_part([?*, ?* | rest], upto, result) do
    compile_part(rest, upto, [:star | result])
  end

  defp compile_part([?*], upto, result) do
    compile_part([], upto, [:accept | result])
  end

  defp compile_part([?* | rest], upto, result) do
    compile_part(rest, upto, [:star | result])
  end

  defp compile_part([?[ | rest], upto, result) do
    case compile_charset(rest, :ordsets.new()) do
      {:ok, charset, rest1} ->
        compile_part(rest1, upto, [charset | result])

      :error ->
        compile_part(rest, upto, [?[ | result])
    end
  end

  defp compile_part([?{ | rest], upto, result) do
    case compile_alt(rest) do
      {:ok, alt} ->
        :lists.reverse(result, [alt])

      :error ->
        compile_part(rest, upto, [?{ | result])
    end
  end

  defp compile_part([{:escaped, x} | rest], upto, result) do
    compile_part(rest, upto, [x | result])
  end

  defp compile_part([x | rest], upto, result) do
    compile_part(rest, upto, [x | result])
  end

  defp compile_part([], _Upto, result) do
    :lists.reverse(result)
  end

  defp compile_charset([?] | rest], ordset) do
    compile_charset1(rest, :ordsets.add_element(?], ordset))
  end

  defp compile_charset([], _Ordset) do
    :error
  end

  defp compile_charset(list, ordset) do
    compile_charset1(list, ordset)
  end

  defp compile_charset1([lower, ?-, upper | rest], ordset)
       when lower <= upper do
    compile_charset1(
      rest,
      compile_range(lower, upper, ordset)
    )
  end

  defp compile_charset1([?] | rest], ordset) do
    {:ok, {:one_of, :gb_sets.from_ordset(ordset)}, rest}
  end

  defp compile_charset1([{:escaped, x} | rest], ordset) do
    compile_charset1(rest, :ordsets.add_element(x, ordset))
  end

  defp compile_charset1([x | rest], ordset) do
    compile_charset1(rest, :ordsets.add_element(x, ordset))
  end

  defp compile_charset1([], _Ordset) do
    :error
  end

  defp compile_range(lower, current, ordset) when lower <= current do
    compile_range(lower, current - 1, :ordsets.add_element(current, ordset))
  end

  defp compile_range(_, _, ordset) do
    ordset
  end

  defp compile_alt(pattern) do
    compile_alt(pattern, [])
  end

  defp compile_alt(pattern, result) do
    case compile_part_to_sep(pattern) do
      {:ok, ?,, altPattern, rest} ->
        compile_alt(rest, [altPattern | result])

      {:ok, ?}, altPattern, rest} ->
        newResult = [altPattern | result]
        restPattern = compile_part(rest)

        {:ok,
         {:alt,
          for alt <- newResult do
            alt ++ restPattern
          end}}

      ^pattern ->
        :error
    end
  end

  defp convert_escapes([?@ | t]) do
    [?@, ?@] ++ convert_escapes(t)
  end

  defp convert_escapes([?\\ | t]) do
    [?@, ?e] ++ convert_escapes(t)
  end

  defp convert_escapes([h | t]) do
    [h | convert_escapes(t)]
  end

  defp convert_escapes([]) do
    []
  end

  defp wrap_escapes([?@, ?@] ++ t) do
    [?@ | wrap_escapes(t)]
  end

  defp wrap_escapes([?@, ?e] ++ [c | t]) do
    [{:escaped, c} | wrap_escapes(t)]
  end

  defp wrap_escapes([?@, ?e]) do
    []
  end

  defp wrap_escapes([h | t]) do
    [h | wrap_escapes(t)]
  end

  defp wrap_escapes([]) do
    []
  end

  defp badpattern(reason) do
    :erlang.error({:badpattern, reason})
  end

  defp exists(file, mod) do
    case eval_read_link_info(file, mod) do
      {:error, _} ->
        false

      {:ok, _Info} ->
        case :os.type() do
          {:win32, _} ->
            do_exists(:filename.split(file), mod, [])

          _ ->
            true
        end
    end
  end

  defp do_exists([p, '..' | ps], mod, acc) do
    path =
      case acc do
        [] ->
          p

        _ ->
          :filename.join(:lists.reverse(acc, [p]))
      end

    case eval_read_link_info(path, mod) do
      {:ok, r_file_info(type: :directory)} ->
        do_exists(ps, mod, acc)

      _ ->
        false
    end
  end

  defp do_exists([p | ps], mod, acc) do
    do_exists(ps, mod, [p | acc])
  end

  defp do_exists([], _, _) do
    true
  end

  defp eval_read_file_info(file, :file) do
    :file.read_file_info(file)
  end

  defp eval_read_file_info(file, :erl_prim_loader) do
    case :erl_prim_loader.read_file_info(file) do
      :error ->
        {:error, :erl_prim_loader}

      res ->
        res
    end
  end

  defp eval_read_file_info(file, mod) do
    mod.read_file_info(file)
  end

  defp eval_read_link_info(file, :file) do
    :file.read_link_info(file)
  end

  defp eval_read_link_info(file, :erl_prim_loader) do
    case :erl_prim_loader.read_link_info(file) do
      :error ->
        {:error, :erl_prim_loader}

      res ->
        res
    end
  end

  defp eval_read_link_info(file, mod) do
    mod.read_link_info(file)
  end

  defp eval_list_dir(dir, :file) do
    :file.list_dir(dir)
  end

  defp eval_list_dir(dir, :erl_prim_loader) do
    case :erl_prim_loader.list_dir(dir) do
      :error ->
        {:error, :erl_prim_loader}

      res ->
        res
    end
  end

  defp eval_list_dir(dir, mod) do
    mod.list_dir(dir)
  end

  defp keep_dir_search_rules(rules) do
    for {_, _} = t <- rules do
      t
    end
  end

  defp keep_suffix_search_rules(rules) do
    for {_, _, _} = t <- rules do
      t
    end
  end

  defp get_search_rules() do
    case :application.get_env(
           :kernel,
           :source_search_rules
         ) do
      :undefined ->
        default_search_rules()

      {:ok, []} ->
        default_search_rules()

      {:ok, r} when is_list(r) ->
        r
    end
  end

  defp default_search_rules() do
    [
      {'.beam', '.erl', erl_source_search_rules()},
      {'.erl', '.yrl', []},
      {'', '.src', erl_source_search_rules()},
      {'.so', '.c', c_source_search_rules()},
      {'.o', '.c', c_source_search_rules()},
      {'', '.c', c_source_search_rules()},
      {'', '.in', basic_source_search_rules()},
      {'', ''}
    ] ++ erl_source_search_rules()
  end

  defp basic_source_search_rules() do
    erl_source_search_rules() ++ c_source_search_rules()
  end

  defp erl_source_search_rules() do
    [
      {'ebin', 'src'},
      {'ebin', 'esrc'},
      {'ebin', :filename.join('src', '*')},
      {'ebin', :filename.join('esrc', '*')}
    ]
  end

  defp c_source_search_rules() do
    [{'priv', 'c_src'}, {'priv', 'src'}, {'bin', 'c_src'}, {'bin', 'src'}, {'', 'src'}]
  end

  def find_file(filename, dir) do
    find_file(filename, dir, [])
  end

  def find_file(filename, dir, []) do
    find_file(filename, dir, get_search_rules())
  end

  def find_file(filename, dir, rules) do
    try_dir_rules(keep_dir_search_rules(rules), filename, dir)
  end

  def find_source(filePath) do
    find_source(
      :filename.basename(filePath),
      :filename.dirname(filePath)
    )
  end

  def find_source(filename, dir) do
    find_source(filename, dir, [])
  end

  def find_source(filename, dir, []) do
    find_source(filename, dir, get_search_rules())
  end

  def find_source(filename, dir, rules) do
    try_suffix_rules(keep_suffix_search_rules(rules), filename, dir)
  end

  defp try_suffix_rules(rules, filename, dir) do
    ext = :filename.extension(filename)
    try_suffix_rules(rules, :filename.rootname(filename, ext), dir, ext)
  end

  defp try_suffix_rules([{ext, src, rules} | rest], root, dir, ext)
       when is_list(src) and is_list(rules) do
    case try_dir_rules(add_local_search(rules), root ++ src, dir) do
      {:ok, file} ->
        {:ok, file}

      _Other ->
        try_suffix_rules(rest, root, dir, ext)
    end
  end

  defp try_suffix_rules([_ | rest], root, dir, ext) do
    try_suffix_rules(rest, root, dir, ext)
  end

  defp try_suffix_rules([], _Root, _Dir, _Ext) do
    {:error, :not_found}
  end

  defp add_local_search(rules) do
    local = {'', ''}

    [local] ++
      :lists.filter(
        fn x ->
          x !== local
        end,
        rules
      )
  end

  defp try_dir_rules([{from, to} | rest], filename, dir)
       when is_list(from) and is_list(to) do
    case try_dir_rule(dir, filename, from, to) do
      {:ok, file} ->
        {:ok, file}

      :error ->
        try_dir_rules(rest, filename, dir)
    end
  end

  defp try_dir_rules([], _Filename, _Dir) do
    {:error, :not_found}
  end

  defp try_dir_rule(dir, filename, from, to) do
    case :lists.suffix(from, dir) do
      true ->
        newDir = :lists.sublist(dir, 1, length(dir) - length(from)) ++ to
        src = :filename.join(newDir, filename)

        case is_regular(src) do
          true ->
            {:ok, src}

          false ->
            find_regular_file(wildcard(src))
        end

      false ->
        :error
    end
  end

  defp find_regular_file([]) do
    :error
  end

  defp find_regular_file([file | files]) do
    case is_regular(file) do
      true ->
        {:ok, file}

      false ->
        find_regular_file(files)
    end
  end

  def safe_relative_path(path, cwd) do
    case :filename.pathtype(path) do
      :relative ->
        safe_relative_path(:filename.split(path), cwd, [], '')

      _ ->
        :unsafe
    end
  end

  defp safe_relative_path([], _Cwd, _PrevLinks, acc) do
    acc
  end

  defp safe_relative_path([segment | segments], cwd, prevLinks, acc) do
    accSegment = join(acc, segment)

    case safe_relative_path(accSegment) do
      :unsafe ->
        :unsafe

      safeAccSegment ->
        case :file.read_link(join(cwd, safeAccSegment)) do
          {:ok, linkPath} ->
            case :lists.member(linkPath, prevLinks) do
              true ->
                :unsafe

              false ->
                case safe_relative_path(
                       :filename.split(linkPath),
                       cwd,
                       [linkPath | prevLinks],
                       acc
                     ) do
                  :unsafe ->
                    :unsafe

                  newAcc ->
                    safe_relative_path(segments, cwd, [], newAcc)
                end
            end

          {:error, _} ->
            safe_relative_path(segments, cwd, prevLinks, safeAccSegment)
        end
    end
  end

  defp join([], path) do
    path
  end

  defp join(left, right) do
    :filename.join(left, right)
  end

  defp safe_relative_path(path) do
    case :filename.pathtype(path) do
      :relative ->
        cs0 = :filename.split(path)
        safe_relative_path_1(cs0, [])

      _ ->
        :unsafe
    end
  end

  defp safe_relative_path_1(['.' | t], acc) do
    safe_relative_path_1(t, acc)
  end

  defp safe_relative_path_1(["." | t], acc) do
    safe_relative_path_1(t, acc)
  end

  defp safe_relative_path_1(['..' | t], acc) do
    climb(t, acc)
  end

  defp safe_relative_path_1([".." | t], acc) do
    climb(t, acc)
  end

  defp safe_relative_path_1([h | t], acc) do
    safe_relative_path_1(t, [h | acc])
  end

  defp safe_relative_path_1([], []) do
    []
  end

  defp safe_relative_path_1([], acc) do
    :filename.join(:lists.reverse(acc))
  end

  defp climb(_, []) do
    :unsafe
  end

  defp climb(t, [_ | acc]) do
    safe_relative_path_1(t, acc)
  end
end
