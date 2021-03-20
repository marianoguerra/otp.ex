defmodule :m_xref_utils do
  use Bitwise
  import Kernel, except: [use: 2]

  import :lists,
    only: [
      append: 1,
      delete: 2,
      filter: 2,
      foldl: 3,
      foreach: 2,
      keydelete: 3,
      keysearch: 3,
      keysort: 2,
      last: 1,
      map: 2,
      member: 2,
      reverse: 1,
      sort: 1
    ]

  import :sofs,
    only: [
      difference: 2,
      domain: 1,
      family: 1,
      family_to_relation: 1,
      from_external: 2,
      from_term: 2,
      intersection: 2,
      partition: 2,
      relation: 1,
      relation_to_family: 1,
      restriction: 2,
      set: 1,
      to_external: 1,
      type: 1
    ]

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

  def xset(l, t) when is_list(l) do
    from_external(:lists.usort(l), t)
  end

  def xset(s, t) do
    from_external(s, t)
  end

  def is_directory(f) do
    case :file.read_file_info(f) do
      {:ok, info} ->
        r_file_info(info, :type) === :directory

      {:error, error} ->
        file_error(f, error)
    end
  end

  def file_info(f) do
    case :file.read_file_info(f) do
      {:ok, info} ->
        readable =
          case r_file_info(info, :access) do
            access when access === :read or access === :read_write ->
              :readable

            _ ->
              :unreadable
          end

        type =
          case r_file_info(info, :type) do
            :directory ->
              :directory

            :regular ->
              :file

            _ ->
              :error
          end

        case type do
          :error ->
            :erlang.error({:unrecognized_file, f})

          _ ->
            {:ok, {f, type, readable, r_file_info(info, :mtime)}}
        end

      {:error, error} ->
        file_error(f, error)
    end
  end

  def fa_to_mfa(fAs, mod) do
    fa_to_mfa(fAs, mod, [])
  end

  defp fa_to_mfa([{f, a} | mFs], mod, l) do
    fa_to_mfa(mFs, mod, [{mod, f, a} | l])
  end

  defp fa_to_mfa([], _Mod, l) do
    reverse(l)
  end

  def module_filename(dir, module) do
    :filename.join(
      dir,
      to_list(module) ++ :code.objfile_extension()
    )
  end

  def application_filename(appName) do
    to_list(appName) ++ '.app'
  end

  def application_filename(dir, appName) do
    :filename.join(
      to_list(dir),
      application_filename(appName)
    )
  end

  def is_string([], _) do
    false
  end

  def is_string(term, c) do
    is_string1(term, c)
  end

  defp is_string1([h | t], c) when h > c do
    is_string1(t, c)
  end

  defp is_string1([], _) do
    true
  end

  defp is_string1(_, _) do
    false
  end

  def is_path([s | ss]) do
    case is_string(s, 31) do
      true ->
        is_path(ss)

      false ->
        false
    end
  end

  def is_path([]) do
    true
  end

  def is_path(_) do
    false
  end

  def release_directory(dir, useLib, subDir) do
    sDir = subdir(dir, 'lib', useLib)

    case :file.list_dir(sDir) do
      {:ok, fileNames} ->
        files =
          for file <- fileNames do
            :filename.join(sDir, file)
          end

        case select_application_directories(files, subDir) do
          {:ok, applDirs} ->
            {:ok, :erlang.list_to_atom(:filename.basename(dir)), sDir, applDirs}

          error ->
            error
        end

      {:error, error} ->
        file_error(sDir, error)
    end
  end

  def select_application_directories(fileNames, dir) do
    select_application_directories(fileNames, dir, dir !== [], [])
  end

  def filename_to_application(fileName) do
    basename = :filename.basename(fileName)

    case (try do
            filename2appl(basename)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:erlang.list_to_atom(basename), []}

      split ->
        split
    end
  end

  def select_last_application_version(appVs) do
    tL = to_external(partition(1, relation(appVs)))

    for l <- tL do
      last(keysort(2, l))
    end
  end

  Record.defrecord(:r_scan, :scan, collected: [], errors: [], seen: [], unreadable: [])

  def scan_directory(file, recurse, collect, watch) do
    init = r_scan()
    s = find_files_dir(file, recurse, collect, watch, init)
    r_scan(collected: l, errors: e, seen: j, unreadable: u) = s
    {reverse(l), reverse(e), reverse(j), reverse(u)}
  end

  def split_filename(file, extension) do
    case (try do
            dir = :filename.dirname(file)
            basename = :filename.basename(file, extension)
            {dir, basename ++ extension}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        false

      r ->
        r
    end
  end

  def list_path(p, extensions) do
    list_dirs(p, 1, extensions, [], [])
  end

  defp list_dirs([d | ds], i, exts, cL, e) do
    fun = fn x, a ->
      file = :filename.join(d, x)

      case is_directory(file) do
        false ->
          ext = :filename.extension(x)

          case member(ext, exts) do
            true ->
              m = :erlang.list_to_atom(:filename.basename(x, ext))
              [{m, {i, d, x}} | a]

            false ->
              a
          end

        true ->
          a

        _Else ->
          a
      end
    end

    {nCL, nE} =
      case :file.list_dir(d) do
        {:ok, c0} ->
          {foldl(fun, cL, c0), e}

        {:error, error} ->
          {cL, [file_error(d, error) | e]}
      end

    list_dirs(ds, i + 1, exts, nCL, nE)
  end

  defp list_dirs([], _I, _Exts, c, e) do
    {c, e}
  end

  def predefined_functions() do
    [{:module_info, 0}, {:module_info, 1}]
  end

  def is_funfun(:erlang, :apply, 2) do
    true
  end

  def is_funfun(:erlang, :apply, 3) do
    true
  end

  def is_funfun(:erlang, :spawn, 1) do
    true
  end

  def is_funfun(:erlang, :spawn, 2) do
    true
  end

  def is_funfun(:erlang, :spawn, 3) do
    true
  end

  def is_funfun(:erlang, :spawn, 4) do
    true
  end

  def is_funfun(:erlang, :spawn_link, 1) do
    true
  end

  def is_funfun(:erlang, :spawn_link, 2) do
    true
  end

  def is_funfun(:erlang, :spawn_link, 3) do
    true
  end

  def is_funfun(:erlang, :spawn_link, 4) do
    true
  end

  def is_funfun(:erlang, :spawn_opt, 2) do
    true
  end

  def is_funfun(:erlang, :spawn_opt, 3) do
    true
  end

  def is_funfun(:erlang, :spawn_opt, 4) do
    true
  end

  def is_funfun(:erlang, :spawn_opt, 5) do
    true
  end

  def is_funfun(:erts_debug, :apply, 4) do
    true
  end

  def is_funfun(_, _, _) do
    false
  end

  def is_builtin(:erts_debug, :apply, 4) do
    true
  end

  def is_builtin(m, f, a) do
    :erlang.is_builtin(m, f, a)
  end

  def is_abstract_module(attributes) do
    case keysearch(:abstract, 1, attributes) do
      {:value, {:abstract, true}} ->
        true

      {:value, {:abstract, vals}} when is_list(vals) ->
        member(true, vals)

      _ ->
        false
    end
  end

  def is_static_function(:module_info, 0) do
    true
  end

  def is_static_function(:module_info, 1) do
    true
  end

  def is_static_function(:new, _) do
    true
  end

  def is_static_function(:instance, _) do
    true
  end

  def is_static_function(_F, _A) do
    false
  end

  def closure(s) do
    relation_to_graph(s)
  end

  def components(g) do
    from_term(
      :digraph_utils.cyclic_strong_components(g),
      [[:atom]]
    )
  end

  def condensation(g) do
    g2 = :digraph_utils.condensation(g)
    r = graph_to_relation(g2)
    true = :digraph.delete(g2)
    r
  end

  def path(g, [e]) do
    path(g, [e, e])
  end

  def path(g, p = [e1 | _]) do
    path(p, g, [[e1]])
  end

  def use(g, v) do
    neighbours(to_external(v), g, :reaching_neighbours, type(v))
  end

  def call(g, v) do
    neighbours(to_external(v), g, :reachable_neighbours, type(v))
  end

  def regexpr({:regexpr, rExpr}, var) do
    xs = match_list(to_external(var), rExpr)
    xset(xs, type(var))
  end

  def regexpr({modExpr, funExpr, arityExpr}, var) do
    type = type(var)

    v1 =
      case {modExpr, type} do
        {{:atom, mod}, [{modType, _}]} ->
          restriction(var, xset([mod], [modType]))

        {{:regexpr, mExpr}, [{modType, _}]} ->
          mods = match_list(to_external(domain(var)), mExpr)
          restriction(var, xset(mods, [modType]))

        {:variable, _} ->
          var

        {_, _} ->
          var
      end

    v2 =
      case funExpr do
        {:atom, funName} ->
          v1L = to_external(v1)
          xset(match_one(v1L, funName, 2), type)

        {:regexpr, fExpr} ->
          v1L = to_external(v1)
          xset(match_many(v1L, fExpr, 2), type)

        :variable ->
          v1
      end

    case arityExpr do
      {:integer, arity} ->
        v2L = to_external(v2)
        xset(match_one(v2L, arity, 3), type)

      {:regexpr, expr} ->
        v2L = to_external(v2)
        xset(match_many(v2L, expr, 3), type)

      :variable ->
        v2
    end
  end

  def relation_to_graph(s) do
    g = :digraph.new()

    fun = fn {from, to} ->
      :digraph.add_vertex(g, from)
      :digraph.add_vertex(g, to)
      :digraph.add_edge(g, from, to)
    end

    foreach(fun, to_external(s))
    g
  end

  def find_beam(module) when is_atom(module) do
    case :code.which(module) do
      :non_existing ->
        :erlang.error({:no_such_module, module})

      :preloaded ->
        {^module, {_M, _Bin, file}} = {module, :code.get_object_code(module)}
        {:ok, file}

      :cover_compiled ->
        :erlang.error({:cover_compiled, module})

      file ->
        {:ok, file}
    end
  end

  def find_beam(culprit) do
    :erlang.error(:badarg, [culprit])
  end

  def options(options, valid) do
    split_options(options, [], [], [], valid)
  end

  def format_error({:error, module, error}) do
    module.format_error(error)
  end

  def format_error({:file_error, fileName, reason}) do
    :io_lib.format(
      '~ts: ~tp~n',
      [fileName, :file.format_error(reason)]
    )
  end

  def format_error({:unrecognized_file, fileName}) do
    :io_lib.format('~tp is neither a regular file nor a directory~n', [fileName])
  end

  def format_error({:no_such_module, module}) do
    :io_lib.format('Cannot find module ~tp using the code path~n', [module])
  end

  def format_error({:interpreted, module}) do
    :io_lib.format('Cannot use BEAM code of interpreted module ~tp~n', [module])
  end

  def format_error(e) do
    :io_lib.format('~tp~n', [e])
  end

  defp to_list(x) when is_atom(x) do
    :erlang.atom_to_list(x)
  end

  defp to_list(x) when is_list(x) do
    x
  end

  defp select_application_directories([fileName | fileNames], dir, flag, l) do
    case is_directory(fileName) do
      true ->
        file = :filename.basename(fileName)
        {name, vsn} = filename_to_application(file)
        applDir = {name, vsn, subdir(fileName, dir, flag)}
        select_application_directories(fileNames, dir, flag, [applDir | l])

      false ->
        select_application_directories(fileNames, dir, flag, l)

      error ->
        error
    end
  end

  defp select_application_directories([], _Dir, _Flag, l) do
    {:ok, reverse(l)}
  end

  defp subdir(dir, _, false) do
    dir
  end

  defp subdir(dir, subDir, true) do
    eDir = :filename.join(dir, subDir)

    case is_directory(eDir) do
      true ->
        eDir

      _FalseOrError ->
        dir
    end
  end

  defp filename2appl(file) do
    [applName, v] = :string.split(file, '-', :trailing)
    true = :string.length(v) > 0
    vsnT = :string.lexemes(v, '.')

    vsn =
      for vsn <- vsnT do
        :erlang.list_to_integer(vsn)
      end

    {:erlang.list_to_atom(applName), vsn}
  end

  defp find_files_dir(dir, recurse, collect, watch, l) do
    case :file.list_dir(dir) do
      {:ok, files} ->
        find_files(sort(files), dir, recurse, collect, watch, l)

      {:error, error} ->
        r_scan(l, errors: [file_error(dir, error) | r_scan(l, :errors)])
    end
  end

  defp find_files([f | fs], dir, recurse, collect, watch, l) do
    file = :filename.join(dir, f)

    l1 =
      case file_info(file) do
        {:ok, {_, :directory, :readable, _}} when recurse ->
          find_files_dir(file, recurse, collect, watch, l)

        {:ok, {_, :directory, _, _}} ->
          l

        info ->
          r_scan(collected: b, errors: e, seen: j, unreadable: u) = l
          ext = :filename.extension(file)
          c = member(ext, collect)

          case c do
            true ->
              case info do
                {:ok, {_, :file, :readable, _}} ->
                  r_scan(l, collected: [{dir, f} | b])

                {:ok, {_, :file, :unreadable, _}} ->
                  r_scan(l, unreadable: [file | u])

                error ->
                  r_scan(l, errors: [error | e])
              end

            false ->
              case member(ext, watch) do
                true ->
                  r_scan(l, seen: [file | j])

                false ->
                  l
              end
          end
      end

    find_files(fs, dir, recurse, collect, watch, l1)
  end

  defp find_files([], _Dir, _Recurse, _Collect, _Watch, l) do
    l
  end

  defp graph_to_relation(g) do
    fun = fn e ->
      {_E, v1, v2, _Label} = :digraph.edge(g, e)
      {v1, v2}
    end

    from_term(
      map(fun, :digraph.edges(g)),
      [{[:atom], [:atom]}]
    )
  end

  defp path([e1, e2 | p], g, l) do
    case :digraph.get_short_path(g, e1, e2) do
      false ->
        false

      [_V | vs] ->
        path([e2 | p], g, [vs | l])
    end
  end

  defp path([_], _G, l) do
    append(reverse(l))
  end

  defp neighbours(vs, g, fun, vT) do
    neighbours(vs, g, fun, vT, [])
  end

  defp neighbours([v | vs], g, fun, vT, l) do
    ns = apply(:digraph_utils, fun, [[v], g])
    neighbours(ns, g, fun, vT, l, v, vs)
  end

  defp neighbours([], _G, _Fun, [vT], l) do
    xset(l, [{vT, vT}])
  end

  defp neighbours([n | ns], g, fun, vT, l, v, vs)
       when fun === :reachable_neighbours do
    neighbours(ns, g, fun, vT, [{v, n} | l], v, vs)
  end

  defp neighbours([n | ns], g, fun, vT, l, v, vs) do
    neighbours(ns, g, fun, vT, [{n, v} | l], v, vs)
  end

  defp neighbours([], g, fun, vT, l, _V, vs) do
    neighbours(vs, g, fun, vT, l)
  end

  defp match_list(l, rExpr) do
    {:ok, expr} = :re.compile(rExpr, [:unicode])

    filter(
      fn e ->
        match(e, expr)
      end,
      l
    )
  end

  defp match_one(varL, con, col) do
    select_each(
      varL,
      fn e ->
        con === :erlang.element(col, e)
      end
    )
  end

  defp match_many(varL, rExpr, col) do
    {:ok, expr} = :re.compile(rExpr, [:unicode])

    select_each(
      varL,
      fn e ->
        match(:erlang.element(col, e), expr)
      end
    )
  end

  defp match(i, expr) when is_integer(i) do
    s = :erlang.integer_to_list(i)
    {:match, [{0, length(s)}]} === :re.run(s, expr, [{:capture, :first}])
  end

  defp match(a, expr) when is_atom(a) do
    s = :erlang.atom_to_list(a)

    case :re.run(s, expr, [{:capture, :first}]) do
      {:match, [{0, size}]} ->
        size === byte_size(:unicode.characters_to_binary(s))

      _ ->
        false
    end
  end

  defp select_each([{mod, funs} | l], pred) do
    case filter(pred, funs) do
      [] ->
        select_each(l, pred)

      nFuns ->
        [{mod, nFuns} | select_each(l, pred)]
    end
  end

  defp select_each([], _Pred) do
    []
  end

  defp split_options([o | os], a, p, i, v) when is_atom(o) do
    split_options(os, [o | a], p, i, v)
  end

  defp split_options([o = {name, _} | os], a, p, i, v)
       when is_atom(name) do
    split_options(os, a, [o | p], i, v)
  end

  defp split_options([o | os], a, p, i, v) do
    split_options(os, a, p, [o | i], v)
  end

  defp split_options([], a, p, i, v) do
    atoms = to_external(set(a))
    pairs = to_external(relation_to_family(relation(p)))
    option_values(v, atoms, pairs, i, [])
  end

  defp split_options(o, a, p, i, v) do
    split_options([o], a, p, i, v)
  end

  defp option_values([o | os], a, p, i, vs) when is_atom(o) do
    option_values(os, delete(o, a), p, i, [member(o, a) | vs])
  end

  defp option_values([{name, allowedValues} | os], a, p, i, vs) do
    case keysearch(name, 1, p) do
      {:value, {_, values}} ->
        option_value(name, allowedValues, values, a, p, i, vs, os)

      false when allowedValues === [] ->
        option_values(os, a, p, i, [[] | vs])

      false ->
        [default | _] = allowedValues
        option_values(os, a, p, i, [[default] | vs])
    end
  end

  defp option_values([], a, p, invalid, values) do
    i2 = to_external(family_to_relation(family(p)))
    {reverse(values), invalid ++ a ++ i2}
  end

  defp option_value(name, [_Deflt, fun], vals, a, p, i, vs, os)
       when is_function(fun) do
    p1 = keydelete(name, 1, p)

    case fun.(vals) do
      true ->
        option_values(os, a, p1, i, [vals | vs])

      false ->
        option_values(os, a, [{name, vals} | p1], i, [[] | vs])
    end
  end

  defp option_value(name, allowedValues, values, a, p, i, vs, os) do
    p1 = keydelete(name, 1, p)
    vS = set(values)
    aVS = set(allowedValues)
    v1 = to_external(intersection(vS, aVS))

    {v, nP} =
      case to_external(difference(vS, aVS)) do
        _ when allowedValues === [] ->
          {values, p1}

        [] ->
          {v1, p1}

        _ when length(allowedValues) === 1 ->
          {values, p1}

        i1 ->
          {v1, [{name, i1} | p1]}
      end

    option_values(os, a, nP, i, [v | vs])
  end

  defp file_error(file, error) do
    :erlang.error({:file_error, file, error})
  end

  defp error(error) do
    {:error, :xref_utils, error}
  end
end
