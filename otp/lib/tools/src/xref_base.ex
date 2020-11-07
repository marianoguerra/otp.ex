defmodule :m_xref_base do
  use Bitwise

  import :lists,
    only: [
      filter: 2,
      flatten: 1,
      foldl: 3,
      foreach: 2,
      keysearch: 3,
      map: 2,
      mapfoldl: 3,
      member: 2,
      reverse: 1,
      sort: 1,
      usort: 1
    ]

  import :sofs,
    only: [
      a_function: 1,
      constant_function: 2,
      converse: 1,
      difference: 2,
      domain: 1,
      empty_set: 0,
      family: 1,
      family_difference: 2,
      family_intersection: 2,
      family_projection: 2,
      family_to_relation: 1,
      family_union: 1,
      family_union: 2,
      from_sets: 1,
      from_term: 1,
      image: 2,
      intersection: 2,
      inverse: 1,
      is_empty_set: 1,
      multiple_relative_product: 2,
      no_elements: 1,
      partition_family: 2,
      projection: 2,
      range: 1,
      relation: 1,
      relation_to_family: 1,
      relative_product1: 2,
      restriction: 2,
      restriction: 3,
      set: 1,
      specification: 2,
      substitution: 2,
      to_external: 1,
      union: 1,
      union: 2,
      union_of_family: 1
    ]

  require Record

  Record.defrecord(:r_xref, :xref,
    version: 1,
    mode: :functions,
    variables: :not_set_up,
    modules: :dict.new(),
    applications: :dict.new(),
    releases: :dict.new(),
    library_path: [],
    libraries: :dict.new(),
    builtins_default: false,
    recurse_default: false,
    verbose_default: false,
    warnings_default: true
  )

  Record.defrecord(:r_xref_mod, :xref_mod,
    name: :"",
    app_name: [],
    dir: '',
    mtime: :undefined,
    builtins: :undefined,
    info: :undefined,
    no_unresolved: 0,
    data: :undefined
  )

  Record.defrecord(:r_xref_app, :xref_app, name: :"", rel_name: [], vsn: [], dir: '')
  Record.defrecord(:r_xref_rel, :xref_rel, name: :"", dir: '')
  Record.defrecord(:r_xref_lib, :xref_lib, name: :"", dir: '')

  Record.defrecord(:r_xref_var, :xref_var,
    name: :"",
    value: :undefined,
    vtype: :undefined,
    otype: :undefined,
    type: :undefined
  )

  def new() do
    new([])
  end

  def new(options) do
    modes = [:functions, :modules, :function, :module]

    case :xref_utils.options(
           options,
           [{:xref_mode, modes}]
         ) do
      {[[:function]], []} ->
        {:ok, r_xref(mode: :functions)}

      {[[:module]], []} ->
        {:ok, r_xref(mode: :modules)}

      {[[oM]], []} ->
        {:ok, r_xref(mode: oM)}

      _ ->
        :erlang.error({:invalid_options, options})
    end
  end

  def delete(state)
      when r_xref(state, :variables) === :not_set_up do
    :ok
  end

  def delete(state) do
    fun = fn {x, _} ->
      case (try do
              :digraph.info(x)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        info when is_list(info) ->
          true = :digraph.delete(x)

        _Else ->
          :ok
      end
    end

    foreach(fun, :dict.to_list(r_xref(state, :variables)))
    :ok
  end

  def add_directory(state, dir) do
    add_directory(state, dir, [])
  end

  def add_directory(state, dir, options) do
    valOptions =
      option_values(
        [:builtins, :recurse, :verbose, :warnings],
        state
      )

    case :xref_utils.options(options, valOptions) do
      {[[oB], [oR], [oV], [oW]], []} ->
        try do
          do_add_directory(dir, [], oB, oR, oV, oW, state)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

      _ ->
        :erlang.error({:invalid_options, options})
    end
  end

  def add_module(state, file) do
    add_module(state, file, [])
  end

  def add_module(state, file, options) do
    valOptions =
      option_values(
        [:builtins, :verbose, :warnings],
        state
      )

    case :xref_utils.options(options, valOptions) do
      {[[oB], [oV], [oW]], []} ->
        case (try do
                do_add_a_module(file, [], oB, oV, oW, state)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:ok, [module], newState} ->
            {:ok, module, newState}

          {:ok, [], _NewState} ->
            :erlang.error({:no_debug_info, file})

          error ->
            error
        end

      _ ->
        :erlang.error({:invalid_options, options})
    end
  end

  def add_application(state, appDir) do
    add_application(state, appDir, [])
  end

  def add_application(state, appDir, options) do
    optVals =
      option_values(
        [:builtins, :verbose, :warnings],
        state
      )

    validOptions = [{:name, ['', &check_name/1]} | optVals]

    case :xref_utils.options(options, validOptions) do
      {[applName, [oB], [oV], [oW]], []} ->
        try do
          do_add_application(appDir, [], applName, oB, oV, oW, state)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

      _ ->
        :erlang.error({:invalid_options, options})
    end
  end

  def replace_module(state, module, file) do
    replace_module(state, module, file, [])
  end

  def replace_module(state, module, file, options) do
    validOptions =
      option_values(
        [:verbose, :warnings],
        state
      )

    case :xref_utils.options(options, validOptions) do
      {[[oV], [oW]], []} ->
        try do
          do_replace_module(module, file, oV, oW, state)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

      _ ->
        :erlang.error({:invalid_options, options})
    end
  end

  def replace_application(state, appl, dir) do
    replace_application(state, appl, dir, [])
  end

  def replace_application(state, appl, dir, options) do
    validOptions =
      option_values(
        [:builtins, :verbose, :warnings],
        state
      )

    case :xref_utils.options(options, validOptions) do
      {[[oB], [oV], [oW]], []} ->
        try do
          do_replace_application(appl, dir, oB, oV, oW, state)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

      _ ->
        :erlang.error({:invalid_options, options})
    end
  end

  def remove_module(state, mod) when is_atom(mod) do
    remove_module(state, [mod])
  end

  def remove_module(state, [mod | mods]) do
    case (try do
            do_remove_module(state, mod)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, _OldXMod, newState} ->
        remove_module(newState, mods)

      error ->
        error
    end
  end

  def remove_module(state, []) do
    {:ok, state}
  end

  def remove_application(state, appl) when is_atom(appl) do
    remove_application(state, [appl])
  end

  def remove_application(state, [appl | appls]) do
    case (try do
            do_remove_application(state, appl)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, _OldXApp, newState} ->
        remove_application(newState, appls)

      error ->
        error
    end
  end

  def remove_application(state, []) do
    {:ok, state}
  end

  def remove_release(state, rel) when is_atom(rel) do
    remove_release(state, [rel])
  end

  def remove_release(state, [rel | rels]) do
    case (try do
            do_remove_release(state, rel)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, _OldXRel, newState} ->
        remove_release(newState, rels)

      error ->
        error
    end
  end

  def remove_release(state, []) do
    {:ok, state}
  end

  def add_release(state, relDir) do
    add_release(state, relDir, [])
  end

  def add_release(state, relDir, options) do
    validOptions0 =
      option_values(
        [:builtins, :verbose, :warnings],
        state
      )

    validOptions = [
      {:name, ['', &check_name/1]}
      | validOptions0
    ]

    case :xref_utils.options(options, validOptions) do
      {[relName, [oB], [oV], [oW]], []} ->
        try do
          do_add_release(relDir, relName, oB, oV, oW, state)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

      _ ->
        :erlang.error({:invalid_options, options})
    end
  end

  def get_library_path(state) do
    {:ok, r_xref(state, :library_path)}
  end

  def set_library_path(state, path) do
    set_library_path(state, path, [])
  end

  def set_library_path(state, :code_path, _Options) do
    s1 =
      r_xref(state,
        library_path: :code_path,
        libraries: :dict.new()
      )

    {:ok, take_down(s1)}
  end

  def set_library_path(state, path, options) do
    case :xref_utils.is_path(path) do
      true ->
        validOptions = option_values([:verbose], state)

        case :xref_utils.options(options, validOptions) do
          {[[oV]], []} ->
            do_add_libraries(path, oV, state)

          _ ->
            :erlang.error({:invalid_options, options})
        end

      false ->
        :erlang.error({:invalid_path, path})
    end
  end

  def set_up(state) do
    set_up(state, [])
  end

  def set_up(state, options) do
    validOptions = option_values([:verbose], state)

    case :xref_utils.options(options, validOptions) do
      {[[verbose]], []} ->
        do_set_up(state, verbose)

      _ ->
        :erlang.error({:invalid_options, options})
    end
  end

  def q(s, q) do
    q(s, q, [])
  end

  def q(s, q, options) when is_atom(q) do
    q(s, :erlang.atom_to_list(q), options)
  end

  def q(s, q, options) do
    case :xref_utils.is_string(q, 1) do
      true ->
        case set_up(s, options) do
          {:ok, s1} ->
            case :xref_compiler.compile(q, r_xref(s1, :variables)) do
              {newT, ans} ->
                {{:ok, ans}, r_xref(s1, variables: newT)}

              error ->
                {error, s1}
            end

          error ->
            {error, s}
        end

      false ->
        {:erlang.error({:invalid_query, q}), s}
    end
  end

  def info(state) do
    d0 = sort(:dict.to_list(r_xref(state, :modules)))

    d =
      map(
        fn {_M, xMod} ->
          xMod
        end,
        d0
      )

    noApps = length(:dict.to_list(r_xref(state, :applications)))
    noRels = length(:dict.to_list(r_xref(state, :releases)))
    no = no_sum(state, d)

    [
      {:library_path, r_xref(state, :library_path)},
      {:mode, r_xref(state, :mode)},
      {:no_releases, noRels},
      {:no_applications, noApps}
    ] ++ no
  end

  def info(state, what) do
    do_info(state, what)
  end

  def info(state, what, qual) do
    try do
      do_info(state, what, qual)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  def update(state) do
    update(state, [])
  end

  def update(state, options) do
    validOptions =
      option_values(
        [:verbose, :warnings],
        state
      )

    case :xref_utils.options(options, validOptions) do
      {[[oV], [oW]], []} ->
        try do
          do_update(oV, oW, state)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

      _ ->
        :erlang.error({:invalid_options, options})
    end
  end

  def forget(state) do
    {u, _P} = do_variables(state)

    {:ok,
     foldl(
       fn v, s ->
         {:ok, nS} = forget(s, v)
         nS
       end,
       state,
       u
     )}
  end

  def forget(state, variable)
      when r_xref(state, :variables) === :not_set_up do
    :erlang.error({:not_user_variable, variable})
  end

  def forget(state, variable) when is_atom(variable) do
    forget(state, [variable])
  end

  def forget(state, variables) do
    vars = r_xref(state, :variables)
    do_forget(variables, vars, variables, state)
  end

  def variables(state) do
    variables(state, [:user])
  end

  def variables(state, options) do
    validOptions = option_values([:verbose], state)

    case :xref_utils.options(
           options,
           [[:user, :predefined] | validOptions]
         ) do
      {[user, predef, [oV]], []} ->
        case do_set_up(state, oV) do
          {:ok, newState} ->
            {u, p} = do_variables(newState)

            r1 =
              cond do
                user ->
                  [{:user, u}]

                true ->
                  []
              end

            r =
              cond do
                predef ->
                  [{:predefined, p} | r1]

                true ->
                  r1
              end

            {{:ok, r}, newState}

          error ->
            {error, state}
        end

      _ ->
        {:erlang.error({:invalid_options, options}), state}
    end
  end

  def analyze(state, analysis) do
    analyze(state, analysis, [])
  end

  def analyze(state, analysis, options) do
    case analysis(analysis, r_xref(state, :mode)) do
      p when is_list(p) ->
        q(state, p, options)

      :error ->
        r =
          case analysis(analysis, :functions) do
            :error ->
              :unknown_analysis

            p when is_list(p) ->
              :unavailable_analysis
          end

        error = :erlang.error({r, analysis})
        {error, state}
    end
  end

  def analysis(analysis) do
    analysis(analysis, :functions)
  end

  defp analysis(:undefined_function_calls, :functions) do
    '(XC - UC) || (XU - X - B)'
  end

  defp analysis(:undefined_functions, :modules) do
    'XU - X - B'
  end

  defp analysis(:undefined_functions, :functions) do
    'XU - range UC - X - B'
  end

  defp analysis(:locals_not_used, :functions) do
    '(L - (OL + range (closure LC | OL))) * ((UU + XU - LU) + domain EE + range EE)'
  end

  defp analysis(:exports_not_used, _) do
    'X - XU'
  end

  defp analysis({:call, f}, :functions) do
    make_query('range (E | ~tw : Fun)', [f])
  end

  defp analysis({:use, f}, :functions) do
    make_query('domain (E || ~tw : Fun)', [f])
  end

  defp analysis({:module_call, m}, _) do
    make_query('range (ME | ~tw : Mod)', [m])
  end

  defp analysis({:module_use, m}, _) do
    make_query('domain (ME || ~tw : Mod)', [m])
  end

  defp analysis({:application_call, a}, _) do
    make_query('range (AE | ~tw : App)', [a])
  end

  defp analysis({:application_use, a}, _) do
    make_query('domain (AE || ~tw : App)', [a])
  end

  defp analysis({:release_call, r}, _) do
    make_query('range (RE | ~tw : Rel)', [r])
  end

  defp analysis({:release_use, r}, _) do
    make_query('domain (RE || ~tw : Rel)', [r])
  end

  defp analysis(:deprecated_function_calls, :functions) do
    'XC || DF'
  end

  defp analysis(
         {:deprecated_function_calls, flag},
         :functions
       ) do
    case deprecated_flag(flag) do
      :undefined ->
        :error

      i ->
        make_query('XC || DF_~w', [i])
    end
  end

  defp analysis(:deprecated_functions, _) do
    'XU * DF'
  end

  defp analysis({:deprecated_functions, flag}, _) do
    case deprecated_flag(flag) do
      :undefined ->
        :error

      i ->
        make_query('XU * DF_~w', [i])
    end
  end

  defp analysis(_, _) do
    :error
  end

  def set_default(state, option, value) do
    case get_default(state, option) do
      {:ok, oldValue} ->
        values = option_values([option], state)

        case :xref_utils.options([{option, value}], values) do
          {_, []} ->
            newState = set_def(option, value, state)
            {:ok, oldValue, newState}

          {_, unknown} ->
            :erlang.error({:invalid_options, unknown})
        end

      error ->
        error
    end
  end

  def get_default(state, option) do
    case (try do
            current_default(state, option)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        :erlang.error({:invalid_options, [option]})

      value ->
        {:ok, value}
    end
  end

  def get_default(state) do
    fun = fn o ->
      v = current_default(state, o)
      {o, v}
    end

    map(fun, [:builtins, :recurse, :verbose, :warnings])
  end

  def set_default(state, options) do
    opts = [:builtins, :recurse, :verbose, :warnings]
    validOptions = option_values(opts, state)

    case :xref_utils.options(options, validOptions) do
      {values = [[_], [_], [_], [_]], []} ->
        {:ok, set_defaults(opts, values, state)}

      _ ->
        :erlang.error({:invalid_options, options})
    end
  end

  def format_error({:error, module, error}) do
    module.format_error(error)
  end

  def format_error({:invalid_options, options}) do
    :io_lib.format('Unknown option(s) or invalid option value(s): ~tp~n', [options])
  end

  def format_error({:invalid_filename, term}) do
    :io_lib.format('A file name (a string) was expected: ~tp~n', [term])
  end

  def format_error({:no_debug_info, fileName}) do
    :io_lib.format('The BEAM file ~tp has no debug info~n', [fileName])
  end

  def format_error({:invalid_path, term}) do
    :io_lib.format('A path (a list of strings) was expected: ~tp~n', [term])
  end

  def format_error({:invalid_query, term}) do
    :io_lib.format('A query (a string or an atom) was expected: ~tp~n', [term])
  end

  def format_error({:not_user_variable, variable}) do
    :io_lib.format('~tp is not a user variable~n', [variable])
  end

  def format_error({:unknown_analysis, term}) do
    :io_lib.format('~tp is not a predefined analysis~n', [term])
  end

  def format_error({:module_mismatch, module, readModule}) do
    :io_lib.format('Name of read module ~tp does not match analyzed module ~tp~n', [
      readModule,
      module
    ])
  end

  def format_error({:release_clash, {release, dir, oldDir}}) do
    :io_lib.format('The release ~tp read from ~tp clashes with release already read from ~tp~n', [
      release,
      dir,
      oldDir
    ])
  end

  def format_error({:application_clash, {application, dir, oldDir}}) do
    :io_lib.format(
      'The application ~tp read from ~tp clashes with application already read from ~tp~n',
      [application, dir, oldDir]
    )
  end

  def format_error({:module_clash, {module, dir, oldDir}}) do
    :io_lib.format('The module ~tp read from ~tp clashes with module already read from ~tp~n', [
      module,
      dir,
      oldDir
    ])
  end

  def format_error({:no_such_release, name}) do
    :io_lib.format('There is no analyzed release ~tp~n', [name])
  end

  def format_error({:no_such_application, name}) do
    :io_lib.format('There is no analyzed application ~tp~n', [name])
  end

  def format_error({:no_such_module, name}) do
    :io_lib.format('There is no analyzed module ~tp~n', [name])
  end

  def format_error({:no_such_info, term}) do
    :io_lib.format(
      '~tp is not one of \'modules\', \'applications\', \'releases\' and \'libraries\'~n',
      [term]
    )
  end

  def format_error(e) do
    :io_lib.format('~tp~n', [e])
  end

  defp check_name([n]) when is_atom(n) do
    true
  end

  defp check_name(_) do
    false
  end

  defp do_update(oV, oW, state) do
    changed = updated_modules(state)

    fun = fn {mod, file}, s ->
      {:ok, _M, nS} = do_replace_module(mod, file, oV, oW, s)
      nS
    end

    newState = foldl(fun, state, changed)
    {:ok, newState, to_external(domain(a_function(changed)))}
  end

  defp updated_modules(state) do
    fun = fn {m, xMod}, l ->
      rTime = r_xref_mod(xMod, :mtime)
      file = module_file(xMod)

      case :xref_utils.file_info(file) do
        {:ok, {_, :file, :readable, mTime}} when mTime !== rTime ->
          [{m, file} | l]

        _Else ->
          l
      end
    end

    foldl(fun, [], :dict.to_list(r_xref(state, :modules)))
  end

  defp do_forget([variable | variables], vars, vs, state) do
    case :dict.find(variable, vars) do
      {:ok, r_xref_var(vtype: :user)} ->
        do_forget(variables, vars, vs, state)

      _ ->
        :erlang.error({:not_user_variable, variable})
    end
  end

  defp do_forget([], vars, vs, state) do
    fun = fn v, vT ->
      {:ok, r_xref_var(value: value)} = :dict.find(v, vT)
      vT1 = :xref_compiler.update_graph_counter(value, -1, vT)
      :dict.erase(v, vT1)
    end

    newVars = foldl(fun, vars, vs)
    newState = r_xref(state, variables: newVars)
    {:ok, newState}
  end

  defp do_replace_module(module, file, oV, oW, state) do
    {:ok, oldXMod, state1} = do_remove_module(state, module)
    oldApp = r_xref_mod(oldXMod, :app_name)
    oB = r_xref_mod(oldXMod, :builtins)

    case do_add_a_module(file, oldApp, oB, oV, oW, state1) do
      {:ok, [^module], newState} ->
        {:ok, module, newState}

      {:ok, [readModule], _State} ->
        throw_error({:module_mismatch, module, readModule})

      {:ok, [], _NewState} ->
        throw_error({:no_debug_info, file})
    end
  end

  defp do_replace_application(appl, dir, oB, oV, oW, state) do
    {:ok, oldXApp, state1} =
      do_remove_application(
        state,
        appl
      )

    rel = r_xref_app(oldXApp, :rel_name)
    n = r_xref_app(oldXApp, :name)
    do_add_application(dir, rel, [n], oB, oV, oW, state1)
  end

  defp do_add_release(dir, relName, oB, oV, oW, state) do
    :ok = is_filename(dir)

    case :xref_utils.release_directory(dir, true, 'ebin') do
      {:ok, releaseDirName, applDir, dirs} ->
        applDirs = :xref_utils.select_last_application_version(dirs)

        release =
          case relName do
            [[]] ->
              releaseDirName

            [name] ->
              name
          end

        xRel = r_xref_rel(name: release, dir: applDir)
        newState = do_add_release(state, xRel)
        add_rel_appls(applDirs, [release], oB, oV, oW, newState)

      error ->
        throw(error)
    end
  end

  def do_add_release(s, xRel) do
    release = r_xref_rel(xRel, :name)

    case :dict.find(release, r_xref(s, :releases)) do
      {:ok, oldXRel} ->
        dir = r_xref_rel(xRel, :dir)
        oldDir = r_xref_rel(oldXRel, :dir)
        throw_error({:release_clash, {release, dir, oldDir}})

      :error ->
        d1 = :dict.store(release, xRel, r_xref(s, :releases))
        r_xref(s, releases: d1)
    end
  end

  defp add_rel_appls([applDir | applDirs], release, oB, oV, oW, state) do
    {:ok, _AppName, newState} = add_appldir(applDir, release, [[]], oB, oV, oW, state)
    add_rel_appls(applDirs, release, oB, oV, oW, newState)
  end

  defp add_rel_appls([], [release], _OB, _OV, _OW, newState) do
    {:ok, release, newState}
  end

  defp do_add_application(dir0, release, name, oB, oV, oW, state) do
    :ok = is_filename(dir0)

    case :xref_utils.select_application_directories(
           [dir0],
           'ebin'
         ) do
      {:ok, [applD]} ->
        add_appldir(applD, release, name, oB, oV, oW, state)

      error ->
        throw(error)
    end
  end

  defp add_appldir(applDir, release, name, oB, oV, oW, oldState) do
    {appName0, vsn, dir} = applDir

    appName =
      case name do
        [[]] ->
          appName0

        [n] ->
          n
      end

    appInfo = r_xref_app(name: appName, rel_name: release, vsn: vsn, dir: dir)
    state1 = do_add_application(oldState, appInfo)
    {:ok, _Modules, newState} = do_add_directory(dir, [appName], oB, false, oV, oW, state1)
    {:ok, appName, newState}
  end

  def do_add_application(s, xApp) do
    application = r_xref_app(xApp, :name)

    case :dict.find(application, r_xref(s, :applications)) do
      {:ok, oldXApp} ->
        dir = r_xref_app(xApp, :dir)
        oldDir = r_xref_app(oldXApp, :dir)
        throw_error({:application_clash, {application, dir, oldDir}})

      :error ->
        d1 = :dict.store(application, xApp, r_xref(s, :applications))
        r_xref(s, applications: d1)
    end
  end

  defp do_add_directory(dir, appName, bui, rec, ver, war, state) do
    :ok = is_filename(dir)

    {fileNames, errors, jams, unreadable} =
      :xref_utils.scan_directory(dir, rec, ['.beam'], ['.jam'])

    warnings(war, :jam, jams)
    warnings(war, :unreadable, unreadable)

    case errors do
      [] ->
        do_add_modules(fileNames, appName, bui, ver, war, state)

      [error | _] ->
        throw(error)
    end
  end

  defp do_add_modules(files, appName, oB, oV, oW, state0) do
    nFiles = length(files)

    reader = fn splitName, state ->
      _Pid = read_module(splitName, appName, oB, oV, oW, state)
    end

    n = parallelism()
    files1 = start_readers(files, reader, state0, n)
    nx = n
    add_mods(files1, reader, state0, [], nFiles, nx)
  end

  defp add_mods(_, _ReaderFun, state, modules, 0, _Nx) do
    {:ok, sort(modules), state}
  end

  defp add_mods(files, readerFun, state, modules, n, nx) do
    {i, nx1} =
      case nx > 0 do
        false ->
          {1, nx}

        true ->
          {2, nx - 1}
      end

    files1 = start_readers(files, readerFun, state, i)
    {:ok, m, newState} = process_module(state)
    add_mods(files1, readerFun, newState, m ++ modules, n - 1, nx1)
  end

  defp start_readers([splitName | files], readerFun, state, n)
       when n > 0 do
    _Pid = readerFun.(splitName, state)
    start_readers(files, readerFun, state, n - 1)
  end

  defp start_readers(files, _ReaderFun, _State, _) do
    files
  end

  defp parallelism() do
    case :erlang.system_info(:multi_scheduling) do
      :enabled ->
        :erlang.system_info(:schedulers_online)

      _ ->
        1
    end
  end

  defp do_add_a_module(file, appName, builtins, verbose, warnings, state) do
    case :xref_utils.split_filename(file, '.beam') do
      false ->
        throw_error({:invalid_filename, file})

      splitname ->
        do_add_module(splitname, appName, builtins, verbose, warnings, state)
    end
  end

  defp do_add_module(splitName, appName, builtins, verbose, warnings, state) do
    _Pid = read_module(splitName, appName, builtins, verbose, warnings, state)
    process_module(state)
  end

  defp read_module(splitName, appName, builtins, verbose, warnings, state) do
    me = self()
    r_xref(mode: mode) = state

    fun = fn ->
      send(me, {:xref_base, read_a_module(splitName, appName, builtins, verbose, warnings, mode)})
    end

    :erlang.spawn_opt(
      fun,
      [:link, {:min_heap_size, 1_000_000}, {:priority, :high}]
    )
  end

  defp read_a_module({dir, baseName}, appName, builtins, verbose, warnings, mode) do
    file = :filename.join(dir, baseName)

    case abst(file, builtins, mode) do
      {:ok, _M, :no_abstract_code} when verbose ->
        message(verbose, :no_debug_info, [file])
        :no

      {:ok, _M, :no_abstract_code} when not verbose ->
        message(warnings, :no_debug_info, [file])
        :no

      {:ok, m, data, unresCalls0} ->
        message(verbose, :done_file, [file])
        unresCalls = usort(unresCalls0)
        noUnresCalls = length(unresCalls)

        case noUnresCalls do
          0 ->
            :ok

          1 ->
            warnings(warnings, :unresolved_summary1, [[m]])

          n ->
            warnings(warnings, :unresolved_summary, [[m, n]])
        end

        case :xref_utils.file_info(file) do
          {:ok, {_, _, _, time}} ->
            xMod =
              r_xref_mod(
                name: m,
                app_name: appName,
                dir: dir,
                mtime: time,
                builtins: builtins,
                no_unresolved: noUnresCalls
              )

            {:ok, prepMod, bad} = prepare_module(mode, xMod, unresCalls, data)

            foreach(
              fn {tag, b} ->
                warnings(warnings, tag, [[file, b]])
              end,
              bad
            )

            {:ok, prepMod}

          error ->
            error
        end

      error ->
        message(verbose, :error, [])
        error
    end
  end

  defp process_module(state) do
    receive do
      {:xref_base, reply} ->
        case reply do
          :no ->
            {:ok, [], state}

          {:ok, prepMod} ->
            finish_module(prepMod, state)

          error ->
            throw(error)
        end
    end
  end

  defp abst(file, builtins, _Mode = :functions) do
    case :beam_lib.chunks(
           file,
           [:abstract_code, :exports, :attributes]
         ) do
      {:ok, {m, [{:abstract_code, noA}, _X, _A]}}
      when noA === :no_abstract_code ->
        {:ok, m, noA}

      {:ok, {m, [{:abstract_code, {:abstract_v1, forms}}, {:exports, x0}, {:attributes, a}]}} ->
        x = :xref_utils.fa_to_mfa(x0, m)
        d = deprecated(a, x, m)
        :xref_reader.module(m, forms, builtins, x, d)

      {:ok, {m, [{:abstract_code, {:abstract_v2, forms}}, {:exports, x0}, {:attributes, a}]}} ->
        x = :xref_utils.fa_to_mfa(x0, m)
        d = deprecated(a, x, m)
        :xref_reader.module(m, forms, builtins, x, d)

      {:ok, {m, [{:abstract_code, {:raw_abstract_v1, code}}, {:exports, x0}, {:attributes, a}]}} ->
        forms0 = :epp.interpret_file_attribute(code)
        forms1 = :erl_expand_records.module(forms0, [])
        forms = :erl_internal.add_predefined_functions(forms1)
        x = mfa_exports(x0, a, m)
        d = deprecated(a, x, m)
        :xref_reader.module(m, forms, builtins, x, d)

      error when :erlang.element(1, error) === :error ->
        error
    end
  end

  defp abst(file, builtins, _Mode = :modules) do
    case :beam_lib.chunks(
           file,
           [:exports, :imports, :attributes]
         ) do
      {:ok, {mod, [{:exports, x0}, {:imports, i0}, {:attributes, at}]}} ->
        x1 = mfa_exports(x0, at, mod)

        x =
          filter(
            fn mFA ->
              not predef_fun().(mFA)
            end,
            x1
          )

        d = deprecated(at, x, mod)

        i =
          case builtins do
            true ->
              i0

            false ->
              fun = fn {m, f, a} ->
                not :xref_utils.is_builtin(m, f, a)
              end

              filter(fun, i0)
          end

        {:ok, mod, {x, i, d}, []}

      error when :erlang.element(1, error) === :error ->
        error
    end
  end

  defp mfa_exports(x0, attributes, m) do
    x1 =
      case :xref_utils.is_abstract_module(attributes) do
        true ->
          for {f, a} <- x0 do
            {f, adjust_arity(f, a)}
          end

        false ->
          x0
      end

    :xref_utils.fa_to_mfa(x1, m)
  end

  defp adjust_arity(f, a) do
    case :xref_utils.is_static_function(f, a) do
      true ->
        a

      false ->
        a - 1
    end
  end

  defp deprecated(a, x, m) do
    dF = {[], [], [], []}

    case keysearch(:deprecated, 1, a) do
      {:value, {:deprecated, d0}} ->
        depr(d0, m, dF, x, [])

      false ->
        {dF, []}
    end
  end

  defp depr([d | depr], m, dF, x, bad) do
    case depr_cat(d, m, x) do
      {i, dt} ->
        nDF = :erlang.setelement(i, dF, dt ++ :erlang.element(i, dF))
        depr(depr, m, nDF, x, bad)

      :undefined ->
        depr(depr, m, dF, x, [d | bad])
    end
  end

  defp depr([], _M, dF, _X, bad) do
    {dF, reverse(bad)}
  end

  defp depr_cat({f, a, flg}, m, x) do
    case deprecated_flag(flg) do
      :undefined ->
        :undefined

      i ->
        depr_fa(f, a, x, m, i)
    end
  end

  defp depr_cat({f, a}, m, x) do
    depr_fa(f, a, x, m, 4)
  end

  defp depr_cat(:module, m, x) do
    depr_fa(:_, :_, x, m, 4)
  end

  defp depr_cat(_D, _M, _X) do
    :undefined
  end

  defp depr_fa(:_, :_, x, _M, i) do
    {i, x}
  end

  defp depr_fa(f, :_, x, _M, i) when is_atom(f) do
    {i,
     filter(
       fn {_, f1, _} ->
         f1 === f
       end,
       x
     )}
  end

  defp depr_fa(f, a, _X, m, i)
       when is_atom(f) and
              is_integer(a) and a >= 0 do
    {i, [{m, f, a}]}
  end

  defp depr_fa(_F, _A, _X, _M, _I) do
    :undefined
  end

  defp deprecated_flag(:next_version) do
    1
  end

  defp deprecated_flag(:next_major_release) do
    2
  end

  defp deprecated_flag(:eventually) do
    3
  end

  defp deprecated_flag(string) do
    depr_desc(string)
  end

  defp depr_desc([char | str]) when is_integer(char) do
    depr_desc(str)
  end

  defp depr_desc([]) do
    4
  end

  defp depr_desc(_) do
    :undefined
  end

  def do_add_module(s, xMod, unres, data) do
    r_xref(mode: mode) = s
    ^mode = r_xref(s, :mode)
    {:ok, prepMod, bad} = prepare_module(mode, xMod, unres, data)
    {:ok, ms, nS} = finish_module(prepMod, s)
    {:ok, ms, bad, nS}
  end

  defp prepare_module(_Mode = :functions, xMod, unres0, data) do
    {defAt0, lPreCAt0, xPreCAt0, lC0, xC0, x0, attrs, depr, oL0} = data
    {aLC0, aXC0, bad0} = attrs
    fT = [tspec(:func)]
    fET = [tspec(:fun_edge)]
    pCA = [tspec(:pre_call_at)]
    xPreCAt1 = :xref_utils.xset(xPreCAt0, pCA)
    lPreCAt1 = :xref_utils.xset(lPreCAt0, pCA)
    defAt = :xref_utils.xset(defAt0, [tspec(:def_at)])
    x1 = :xref_utils.xset(x0, fT)
    xC1 = :xref_utils.xset(xC0, fET)
    lC1 = :xref_utils.xset(lC0, fET)
    aXC1 = :xref_utils.xset(aXC0, pCA)
    aLC1 = :xref_utils.xset(aLC0, pCA)
    unresCalls = :xref_utils.xset(unres0, pCA)
    unres = domain(unresCalls)
    oL1 = :xref_utils.xset(oL0, fT)
    definedFuns = domain(defAt)
    {aXC, aLC, bad1, lPreCAt2, xPreCAt2} = extra_edges(aXC1, aLC1, bad0, definedFuns)

    bad =
      map(
        fn b ->
          {:xref_attr, b}
        end,
        bad1
      )

    lPreCAt = union(lPreCAt1, lPreCAt2)
    xPreCAt = union(xPreCAt1, xPreCAt2)
    noCalls = no_elements(lPreCAt) + no_elements(xPreCAt)
    lCallAt = relation_to_family(lPreCAt)
    xCallAt = relation_to_family(xPreCAt)
    callAt = family_union(lCallAt, xCallAt)
    l = difference(definedFuns, x1)
    x = difference(definedFuns, l)
    xC = union(xC1, aXC)
    lC = union(lC1, aLC)
    {dF1, dF_11, dF_21, dF_31, dBad} = depr_mod(depr, x)
    {eE, eCallAt} = inter_graph(x, l, lC, xC, callAt)

    {:ok,
     {:functions, xMod,
      [defAt, l, x, lCallAt, xCallAt, callAt, lC, xC, eE, eCallAt, oL1, dF1, dF_11, dF_21, dF_31],
      noCalls, unres}, dBad ++ bad}
  end

  defp prepare_module(_Mode = :modules, xMod, _Unres, data) do
    {x0, i0, depr} = data
    x1 = :xref_utils.xset(x0, [tspec(:func)])
    i1 = :xref_utils.xset(i0, [tspec(:func)])
    {dF1, dF_11, dF_21, dF_31, dBad} = depr_mod(depr, x1)
    {:ok, {:modules, xMod, [x1, i1, dF1, dF_11, dF_21, dF_31]}, dBad}
  end

  defp finish_module({:functions, xMod, list, noCalls, unres}, s) do
    :ok = check_module(xMod, s)

    [
      defAt2,
      l2,
      x2,
      lCallAt2,
      xCallAt2,
      callAt2,
      lC2,
      xC2,
      eE2,
      eCallAt2,
      oL2,
      dF2,
      dF_12,
      dF_22,
      dF_32
    ] = pack(list)

    lU = range(lC2)
    lPredefined = predefined_funs(lU)
    m = r_xref_mod(xMod, :name)
    mS = :xref_utils.xset(m, :atom)

    t =
      from_sets(
        {mS, defAt2, l2, x2, lCallAt2, xCallAt2, callAt2, lC2, xC2, lU, eE2, eCallAt2, unres,
         lPredefined, oL2, dF2, dF_12, dF_22, dF_32}
      )

    noUnres = r_xref_mod(xMod, :no_unresolved)
    info = no_info(x2, l2, lC2, xC2, eE2, unres, noCalls, noUnres)
    xMod1 = r_xref_mod(xMod, data: t, info: info)
    s1 = r_xref(s, modules: :dict.store(m, xMod1, r_xref(s, :modules)))
    {:ok, [m], take_down(s1)}
  end

  defp finish_module({:modules, xMod, list}, s) do
    :ok = check_module(xMod, s)
    [x2, i2, dF2, dF_12, dF_22, dF_32] = pack(list)
    m = r_xref_mod(xMod, :name)
    mS = :xref_utils.xset(m, :atom)
    t = from_sets({mS, x2, i2, dF2, dF_12, dF_22, dF_32})
    info = []
    xMod1 = r_xref_mod(xMod, data: t, info: info)
    s1 = r_xref(s, modules: :dict.store(m, xMod1, r_xref(s, :modules)))
    {:ok, [m], take_down(s1)}
  end

  defp check_module(xMod, state) do
    m = r_xref_mod(xMod, :name)

    case :dict.find(m, r_xref(state, :modules)) do
      {:ok, oldXMod} ->
        bF2 = module_file(xMod)
        bF1 = module_file(oldXMod)
        throw_error({:module_clash, {m, bF1, bF2}})

      :error ->
        :ok
    end
  end

  defp depr_mod({depr, bad0}, x) do
    {dF_10, dF_20, dF_30, dF0} = depr
    fT = [tspec(:func)]
    dF1 = :xref_utils.xset(dF0, fT)
    dF_11 = :xref_utils.xset(dF_10, fT)
    dF_21 = :xref_utils.xset(dF_20, fT)
    dF_31 = :xref_utils.xset(dF_30, fT)
    all = union(from_sets([dF1, dF_11, dF_21, dF_31]))

    fun =
      {:external,
       fn {m, f, a} ->
         :xref_utils.is_builtin(m, f, a)
       end}

    xB = union(x, specification(fun, all))
    dF_1 = intersection(dF_11, xB)
    dF_2 = union(intersection(dF_21, xB), dF_1)
    dF_3 = union(intersection(dF_31, xB), dF_2)
    dF = union(intersection(dF1, xB), dF_3)
    bad1 = difference(all, xB)

    bad2 =
      to_external(
        difference(
          bad1,
          predefined_funs(bad1)
        )
      )

    bad =
      map(
        fn b ->
          {:depr_attr, b}
        end,
        usort(bad2 ++ bad0)
      )

    {dF, dF_1, dF_2, dF_3, bad}
  end

  defp extra_edges(cAX, cAL, bad0, f) do
    aXC0 = domain(cAX)
    aLC0 = domain(cAL)
    aXC = restriction(aXC0, f)
    aLC = restriction(2, restriction(aLC0, f), f)
    lPreCAt2 = restriction(cAL, aLC)
    xPreCAt2 = restriction(cAX, aXC)

    bad =
      bad0 ++
        to_external(
          difference(
            aXC0,
            aXC
          )
        ) ++
        to_external(
          difference(
            aLC0,
            aLC
          )
        )

    {aXC, aLC, bad, lPreCAt2, xPreCAt2}
  end

  defp no_info(x, l, lC, xC, eE, unres, noCalls, noUnresCalls) do
    noUnres = no_elements(unres)

    [
      {:no_calls, {noCalls - noUnresCalls, noUnresCalls}},
      {:no_function_calls, {no_elements(lC), no_elements(xC) - noUnres, noUnres}},
      {:no_functions, {no_elements(l), no_elements(x)}},
      {:no_inter_function_calls, no_elements(eE)}
    ]
  end

  defp inter_graph(x, l, lC, xC, callAt) do
    g = :xref_utils.relation_to_graph(lC)

    reachable0 =
      :digraph_utils.reachable_neighbours(
        to_external(x),
        g
      )

    reachable = :xref_utils.xset(reachable0, [tspec(:func)])
    xL = union(difference(l, reachable), x)
    lEs = restriction(restriction(2, lC, xL), xL)
    xEs = restriction(xC, xL)
    es = union(lEs, xEs)
    e1 = to_external(restriction(difference(lC, lEs), xL))

    r0 =
      :xref_utils.xset(
        reachable(e1, g, []),
        [{tspec(:func), tspec(:fun_edge)}]
      )

    true = :digraph.delete(g)
    rL = restriction(r0, xL)
    rX = relative_product1(r0, xC)
    r = union(rL, converse(rX))

    eE0 =
      projection(
        {:external,
         fn {ee2, {ee1, _L}} ->
           {ee1, ee2}
         end},
        r
      )

    eE = union(es, eE0)

    sFun =
      {:external,
       fn {ee2, {ee1, ls}} ->
         {{ee1, ls}, {ee1, ee2}}
       end}

    eCallAt1 =
      relative_product1(
        projection(sFun, r),
        callAt
      )

    eCallAt2 = union(eCallAt1, restriction(callAt, es))
    eCallAt = family_union(relation_to_family(eCallAt2))
    :ok
    {eE, eCallAt}
  end

  defp reachable([e = {_X, l} | xs], g, r) do
    ns = :digraph_utils.reachable([l], g)
    reachable(xs, g, reach(ns, e, r))
  end

  defp reachable([], _G, r) do
    r
  end

  defp reach([n | ns], e, l) do
    reach(ns, e, [{n, e} | l])
  end

  defp reach([], _E, l) do
    l
  end

  defp tspec(:func) do
    {:atom, :atom, :atom}
  end

  defp tspec(:fun_edge) do
    {tspec(:func), tspec(:func)}
  end

  defp tspec(:def_at) do
    {tspec(:func), :atom}
  end

  defp tspec(:pre_call_at) do
    {tspec(:fun_edge), :atom}
  end

  defp do_remove_release(s, relName) do
    case :dict.find(relName, r_xref(s, :releases)) do
      :error ->
        throw_error({:no_such_release, relName})

      {:ok, xRel} ->
        s1 = take_down(s)
        s2 = remove_rel(s1, relName)
        {:ok, xRel, s2}
    end
  end

  defp do_remove_application(s, appName) do
    case :dict.find(appName, r_xref(s, :applications)) do
      :error ->
        throw_error({:no_such_application, appName})

      {:ok, xApp} ->
        s1 = take_down(s)
        s2 = remove_apps(s1, [appName])
        {:ok, xApp, s2}
    end
  end

  def do_remove_module(s, module) do
    case :dict.find(module, r_xref(s, :modules)) do
      :error ->
        throw_error({:no_such_module, module})

      {:ok, xMod} ->
        s1 = take_down(s)
        {:ok, xMod, remove_modules(s1, [module])}
    end
  end

  defp remove_rel(s, relName) do
    rels = [relName]

    fun = fn
      {a, xApp}, l when r_xref_app(xApp, :rel_name) === rels ->
        [a | l]

      _, l ->
        l
    end

    apps = foldl(fun, [], :dict.to_list(r_xref(s, :applications)))
    s1 = remove_apps(s, apps)
    newReleases = remove_erase(rels, r_xref(s1, :releases))
    r_xref(s1, releases: newReleases)
  end

  defp remove_apps(s, apps) do
    fun = fn {m, xMod}, l ->
      case r_xref_mod(xMod, :app_name) do
        [] ->
          l

        [appName] ->
          [{appName, m} | l]
      end
    end

    ms = foldl(fun, [], :dict.to_list(r_xref(s, :modules)))
    modules = to_external(image(relation(ms), set(apps)))
    s1 = remove_modules(s, modules)

    newApplications =
      remove_erase(
        apps,
        r_xref(s1, :applications)
      )

    r_xref(s1, applications: newApplications)
  end

  defp remove_modules(s, modules) do
    newModules = remove_erase(modules, r_xref(s, :modules))
    r_xref(s, modules: newModules)
  end

  defp remove_erase([k | ks], d) do
    remove_erase(ks, :dict.erase(k, d))
  end

  defp remove_erase([], d) do
    d
  end

  defp do_add_libraries(path, verbose, state) do
    message(verbose, :lib_search, [])
    {c, e} = :xref_utils.list_path(path, ['.beam'])
    message(verbose, :done, [])
    mDs = to_external(relation_to_family(relation(c)))
    reply = check_file(mDs, [], e, path, state)
    reply
  end

  defp check_file([{module, [{_N, dir, _File} | _]} | mDs], l, e, path, state) do
    xLib = r_xref_lib(name: module, dir: dir)
    check_file(mDs, [{module, xLib} | l], e, path, state)
  end

  defp check_file([], l, [], path, state) do
    d = :dict.from_list(l)
    state1 = r_xref(state, library_path: path, libraries: d)
    newState = take_down(state1)
    {:ok, newState}
  end

  defp check_file([], _L, [e | _], _Path, _State) do
    e
  end

  defp do_set_up(s, _VerboseOpt)
       when r_xref(s, :variables) !== :not_set_up do
    {:ok, s}
  end

  defp do_set_up(s, verboseOpt) do
    message(verboseOpt, :set_up, [])

    reply =
      try do
        do_set_up(s)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    message(verboseOpt, :done, [])
    reply
  end

  defp do_set_up(s) when r_xref(s, :mode) === :functions do
    modDictList = :dict.to_list(r_xref(s, :modules))

    [
      defAt0,
      l,
      x0,
      lCallAt,
      xCallAt,
      callAt,
      lC,
      xC,
      lU,
      eE0,
      eCallAt,
      uC,
      lPredefined,
      oL,
      mod_DF,
      mod_DF_1,
      mod_DF_2,
      mod_DF_3
    ] = make_families(modDictList, 19)

    {xC_1, xU, xPredefined} = do_set_up_1(xC)
    lC_1 = user_family(union_of_family(lC))
    e_1 = family_union(xC_1, lC_1)
    predefined = family_union(xPredefined, lPredefined)
    x1 = family_union(x0, predefined)
    f1 = family_union(l, x1)
    v = family_union(f1, xU)
    e = family_union(lC, xC)
    m = domain(v)
    m2A = make_M2A(modDictList)
    {a2R, a} = make_A2R(r_xref(s, :applications))
    r = set(:dict.fetch_keys(r_xref(s, :releases)))
    vEs = union_of_family(e)

    fun =
      {:external,
       fn {{m1, _F1, _A1}, {m2, _F2, _A2}} ->
         {m1, m2}
       end}

    mE = projection(fun, vEs)
    mE2AE = multiple_relative_product({m2A, m2A}, mE)
    aE = range(mE2AE)
    aE2RE = multiple_relative_product({a2R, a2R}, aE)
    rE = range(aE2RE)
    aM = domain(f1)

    {undef, u0, lib, lib_DF, lib_DF_1, lib_DF_2, lib_DF_3} =
      make_libs(xU, f1, aM, r_xref(s, :library_path), r_xref(s, :libraries))

    {b, u} = make_builtins(u0)
    x1_B = family_union(x1, b)
    f = family_union(f1, lib)

    dF =
      family_union(
        family_intersection(mod_DF, x1_B),
        lib_DF
      )

    dF_1 =
      family_union(
        family_intersection(mod_DF_1, x1_B),
        lib_DF_1
      )

    dF_2 =
      family_union(
        family_intersection(mod_DF_2, x1_B),
        lib_DF_2
      )

    dF_3 =
      family_union(
        family_intersection(mod_DF_3, x1_B),
        lib_DF_3
      )

    uU = family_difference(family_difference(f1, lU), xU)
    defAt = make_defat(undef, defAt0)
    lM = domain(lib)
    uM = difference(difference(domain(u), aM), lM)
    x = family_union(x1, lib)
    eE_conv = converse(union_of_family(eE0))
    eE_exported = restriction(eE_conv, union_of_family(x))

    eE_local =
      specification(
        {:external,
         fn {{m1, _, _}, {m2, _, _}} ->
           m1 === m2
         end},
        eE_conv
      )

    eE_0 = converse(union(eE_local, eE_exported))
    eE_1 = user_family(eE_0)

    eE1 =
      partition_family(
        {:external,
         fn {{m1, _, _}, _MFA2} ->
           m1
         end},
        eE_0
      )

    eE = family_union(family_difference(eE0, eE0), eE1)

    iFun = fn {mod, eE_M}, xMods ->
      iMFun = fn xrefMod ->
        [noCalls, noFunctionCalls, noFunctions, _NoInter] = r_xref_mod(xrefMod, :info)

        newInfo = [
          noCalls,
          noFunctionCalls,
          noFunctions,
          {:no_inter_function_calls, length(eE_M)}
        ]

        r_xref_mod(xrefMod, info: newInfo)
      end

      :dict.update(mod, iMFun, xMods)
    end

    xrefMods1 = foldl(iFun, r_xref(s, :modules), to_external(eE))
    s1 = r_xref(s, modules: xrefMods1)
    uC_1 = user_family(union_of_family(uC))
    :ok
    :ok
    :ok
    :ok
    :ok

    vs = [
      {:L, l},
      {:X, x},
      {:F, f},
      {:U, u},
      {:B, b},
      {:UU, uU},
      {:XU, xU},
      {:LU, lU},
      {:V, v},
      {:v, v},
      {:OL, oL},
      {:LC, {lC, lC_1}},
      {:XC, {xC, xC_1}},
      {:E, {e, e_1}},
      {:e, {e, e_1}},
      {:EE, {eE, eE_1}},
      {:UC, {uC, uC_1}},
      {:M, m},
      {:A, a},
      {:R, r},
      {:AM, aM},
      {:UM, uM},
      {:LM, lM},
      {:ME, mE},
      {:AE, aE},
      {:RE, rE},
      {:DF, dF},
      {:DF_1, dF_1},
      {:DF_2, dF_2},
      {:DF_3, dF_3},
      {:me2ae, mE2AE},
      {:ae, aE2RE},
      {:m2a, m2A},
      {:a2r, a2R},
      {:def_at, defAt},
      {:call_at, callAt},
      {:e_call_at, eCallAt},
      {:l_call_at, lCallAt},
      {:x_call_at, xCallAt}
    ]

    finish_set_up(s1, vs)
  end

  defp do_set_up(s) when r_xref(s, :mode) === :modules do
    modDictList = :dict.to_list(r_xref(s, :modules))
    [x0, i0, mod_DF, mod_DF_1, mod_DF_2, mod_DF_3] = make_families(modDictList, 7)
    i = union_of_family(i0)
    aM = domain(x0)
    {xU, predefined} = make_predefined(i, aM)
    x1 = family_union(x0, predefined)
    v = family_union(x1, xU)
    m = union(aM, domain(xU))
    m2A = make_M2A(modDictList)
    {a2R, a} = make_A2R(r_xref(s, :applications))
    r = set(:dict.fetch_keys(r_xref(s, :releases)))

    mE =
      projection(
        {:external,
         fn {m1, {m2, _F2, _A2}} ->
           {m1, m2}
         end},
        family_to_relation(i0)
      )

    mE2AE = multiple_relative_product({m2A, m2A}, mE)
    aE = range(mE2AE)
    aE2RE = multiple_relative_product({a2R, a2R}, aE)
    rE = range(aE2RE)

    {_Undef, u0, lib, lib_DF, lib_DF_1, lib_DF_2, lib_DF_3} =
      make_libs(xU, x1, aM, r_xref(s, :library_path), r_xref(s, :libraries))

    {b, u} = make_builtins(u0)
    x1_B = family_union(x1, b)

    dF =
      family_union(
        family_intersection(mod_DF, x1_B),
        lib_DF
      )

    dF_1 =
      family_union(
        family_intersection(mod_DF_1, x1_B),
        lib_DF_1
      )

    dF_2 =
      family_union(
        family_intersection(mod_DF_2, x1_B),
        lib_DF_2
      )

    dF_3 =
      family_union(
        family_intersection(mod_DF_3, x1_B),
        lib_DF_3
      )

    lM = domain(lib)
    uM = difference(difference(domain(u), aM), lM)
    x = family_union(x1, lib)
    empty = empty_set()

    vs = [
      {:X, x},
      {:U, u},
      {:B, b},
      {:XU, xU},
      {:v, v},
      {:e, {empty, empty}},
      {:M, m},
      {:A, a},
      {:R, r},
      {:AM, aM},
      {:UM, uM},
      {:LM, lM},
      {:ME, mE},
      {:AE, aE},
      {:RE, rE},
      {:DF, dF},
      {:DF_1, dF_1},
      {:DF_2, dF_2},
      {:DF_3, dF_3},
      {:me2ae, mE2AE},
      {:ae, aE2RE},
      {:m2a, m2A},
      {:a2r, a2R},
      {:def_at, empty},
      {:call_at, empty},
      {:e_call_at, empty},
      {:l_call_at, empty},
      {:x_call_at, empty}
    ]

    finish_set_up(s, vs)
  end

  defp finish_set_up(s, vs) do
    t = do_finish_set_up(vs, :dict.new())
    s1 = r_xref(s, variables: t)
    {:ok, s1}
  end

  defp do_finish_set_up([{key, value} | vs], t) do
    {type, oType} = var_type(key)
    val = r_xref_var(name: key, value: value, vtype: :predef, otype: oType, type: type)
    t1 = :dict.store(key, val, t)
    do_finish_set_up(vs, t1)
  end

  defp do_finish_set_up([], t) do
    t
  end

  defp var_type(:B) do
    {:function, :vertex}
  end

  defp var_type(:F) do
    {:function, :vertex}
  end

  defp var_type(:L) do
    {:function, :vertex}
  end

  defp var_type(:LU) do
    {:function, :vertex}
  end

  defp var_type(:U) do
    {:function, :vertex}
  end

  defp var_type(:UU) do
    {:function, :vertex}
  end

  defp var_type(:V) do
    {:function, :vertex}
  end

  defp var_type(:X) do
    {:function, :vertex}
  end

  defp var_type(:OL) do
    {:function, :vertex}
  end

  defp var_type(:XU) do
    {:function, :vertex}
  end

  defp var_type(:DF) do
    {:function, :vertex}
  end

  defp var_type(:DF_1) do
    {:function, :vertex}
  end

  defp var_type(:DF_2) do
    {:function, :vertex}
  end

  defp var_type(:DF_3) do
    {:function, :vertex}
  end

  defp var_type(:A) do
    {:application, :vertex}
  end

  defp var_type(:AM) do
    {:module, :vertex}
  end

  defp var_type(:LM) do
    {:module, :vertex}
  end

  defp var_type(:M) do
    {:module, :vertex}
  end

  defp var_type(:UM) do
    {:module, :vertex}
  end

  defp var_type(:R) do
    {:release, :vertex}
  end

  defp var_type(:E) do
    {:function, :edge}
  end

  defp var_type(:EE) do
    {:function, :edge}
  end

  defp var_type(:LC) do
    {:function, :edge}
  end

  defp var_type(:UC) do
    {:function, :edge}
  end

  defp var_type(:XC) do
    {:function, :edge}
  end

  defp var_type(:AE) do
    {:application, :edge}
  end

  defp var_type(:ME) do
    {:module, :edge}
  end

  defp var_type(:RE) do
    {:release, :edge}
  end

  defp var_type(_) do
    {:foo, :bar}
  end

  defp make_families(modDictList, n) do
    fun1 = fn {_, xMod} ->
      r_xref_mod(xMod, :data)
    end

    ss = from_sets(map(fun1, modDictList))
    make_fams(n, ss, [])
  end

  defp make_fams(1, _Ss, l) do
    l
  end

  defp make_fams(i, ss, l) do
    fun =
      {:external,
       fn r ->
         {:erlang.element(1, r), :erlang.element(i, r)}
       end}

    make_fams(i - 1, ss, [projection(fun, ss) | l])
  end

  defp make_M2A(modDictList) do
    fun = fn {m, xMod} ->
      {m, r_xref_mod(xMod, :app_name)}
    end

    mod0 = family(map(fun, modDictList))
    mod = family_to_relation(mod0)
    mod
  end

  defp make_A2R(applDict) do
    appDict = :dict.to_list(applDict)

    fun = fn {a, xApp} ->
      {a, r_xref_app(xApp, :rel_name)}
    end

    appl0 = family(map(fun, appDict))
    allApps = domain(appl0)
    appl = family_to_relation(appl0)
    {appl, allApps}
  end

  defp do_set_up_1(xC) do
    xCp = union_of_family(xC)
    xC_1 = user_family(xCp)
    i = range(xCp)
    {xU, xPredefined} = make_predefined(i, domain(xC))
    {xC_1, xU, xPredefined}
  end

  defp make_predefined(i, callingModules) do
    xPredefined0 = predefined_funs(i)
    xPredefined1 = converse(substitution(1, xPredefined0))
    xPredefined2 = restriction(xPredefined1, callingModules)
    xPredefined = relation_to_family(xPredefined2)
    xU = partition_family(1, i)
    {xU, xPredefined}
  end

  defp predefined_funs(functions) do
    specification({:external, predef_fun()}, functions)
  end

  defp predef_fun() do
    predefinedFuns = :xref_utils.predefined_functions()

    fn {_M, f, a} ->
      member({f, a}, predefinedFuns)
    end
  end

  defp make_defat(undef, defAt0) do
    zero = from_term(0)

    dAL =
      family_projection(
        fn s ->
          constant_function(s, zero)
        end,
        undef
      )

    family_union(defAt0, dAL)
  end

  defp make_libs(xU, f, aM, libPath, libDict) do
    undef = family_difference(xU, f)
    uM = difference(domain(family_to_relation(undef)), aM)

    fs =
      case is_empty_set(uM) do
        true ->
          []

        false when libPath === :code_path ->
          bFun = fn m, a ->
            case :xref_utils.find_beam(m) do
              {:ok, file} ->
                [file | a]

              _ ->
                a
            end
          end

          foldl(bFun, [], to_external(uM))

        false ->
          libraries = :dict.to_list(libDict)
          lb = restriction(a_function(libraries), uM)

          mFun = fn {m, xLib} ->
            r_xref_lib(dir: dir) = xLib
            :xref_utils.module_filename(dir, m)
          end

          map(mFun, to_external(lb))
      end

    fun = fn fileName, deprs ->
      case :beam_lib.chunks(
             fileName,
             [:exports, :attributes]
           ) do
        {:ok, {m, [{:exports, x}, {:attributes, a}]}} ->
          exports = mfa_exports(x, a, m)
          {deprecated, _Bad} = deprecated(a, exports, m)
          {{m, exports}, [{m, deprecated} | deprs]}

        error ->
          throw(error)
      end
    end

    {xL, dL} = mapfoldl(fun, [], fs)
    lF = from_term(xL)
    lib = family_intersection(undef, lF)
    {b, _} = make_builtins(undef)
    dLib = family_union(lib, b)
    [dF_1, dF_21, dF_31, dF1] = depr_lib(4, dL, dL, [], [], dLib)
    dF_2 = family_union(dF_21, dF_1)
    dF_3 = family_union(dF_31, dF_2)
    dF = family_union(dF1, dF_3)
    u = family_difference(undef, lib)
    {undef, u, lib, dF, dF_1, dF_2, dF_3}
  end

  defp depr_lib(0, _, _, lL, [], _Lib) do
    lL
  end

  defp depr_lib(i, [], dL, lL, l, lib) do
    dT = family_intersection(lib, from_term(l))
    depr_lib(i - 1, dL, dL, [dT | lL], [], lib)
  end

  defp depr_lib(i, [{m, d} | ds], dL, lL, l, lib) do
    depr_lib(i, ds, dL, lL, [{m, :erlang.element(i, d)} | l], lib)
  end

  defp make_builtins(u0) do
    tmp = family_to_relation(u0)

    fun2 =
      {:external,
       fn {_M, {m, f, a}} ->
         :xref_utils.is_builtin(m, f, a)
       end}

    b = relation_to_family(specification(fun2, tmp))
    u = family_difference(u0, b)
    {b, u}
  end

  defp user_family(r) do
    partition_family(
      {:external,
       fn {_MFA1, {m2, _, _}} ->
         m2
       end},
      r
    )
  end

  defp do_variables(state) do
    fun = fn
      {name, r_xref_var(vtype: :user)}, {p, u} ->
        {p, [name | u]}

      {name, r_xref_var(vtype: :predef)}, a = {p, u} ->
        case :erlang.atom_to_list(name) do
          [h | _] when h >= ?a and h <= ?z ->
            a

          _Else ->
            {[name | p], u}
        end

      {{:tmp, v}, _}, a ->
        :io.format('Bug in ~tp: temporary ~tp~n', [:xref_base, v])
        a

      _V, a ->
        a
    end

    {u, p} = foldl(fun, {[], []}, :dict.to_list(r_xref(state, :variables)))
    {sort(p), sort(u)}
  end

  defp take_down(s) when r_xref(s, :variables) === :not_set_up do
    s
  end

  defp take_down(s) do
    r_xref(s, variables: :not_set_up)
  end

  defp make_query(format, args) do
    flatten(:io_lib.format(format, args))
  end

  defp set_defaults([o | os], [[v] | vs], state) do
    newState = set_def(o, v, state)
    set_defaults(os, vs, newState)
  end

  defp set_defaults([], [], state) do
    state
  end

  defp set_def(:builtins, value, state) do
    r_xref(state, builtins_default: value)
  end

  defp set_def(:recurse, value, state) do
    r_xref(state, recurse_default: value)
  end

  defp set_def(:verbose, value, state) do
    r_xref(state, verbose_default: value)
  end

  defp set_def(:warnings, value, state) do
    r_xref(state, warnings_default: value)
  end

  defp option_values([option | options], state) do
    default = current_default(state, option)

    [
      {option, [default, true, false]}
      | option_values(options, state)
    ]
  end

  defp option_values([], _State) do
    []
  end

  defp current_default(state, :builtins) do
    r_xref(state, :builtins_default)
  end

  defp current_default(state, :recurse) do
    r_xref(state, :recurse_default)
  end

  defp current_default(state, :verbose) do
    r_xref(state, :verbose_default)
  end

  defp current_default(state, :warnings) do
    r_xref(state, :warnings_default)
  end

  defp do_info(s, :modules) do
    d = sort(:dict.to_list(r_xref(s, :modules)))

    map(
      fn {_M, xMod} ->
        mod_info(xMod)
      end,
      d
    )
  end

  defp do_info(s, :applications) do
    appMods = to_external(relation_to_family(relation(app_mods(s))))
    sum = sum_mods(s, appMods)

    map(
      fn appSum ->
        app_info(appSum, s)
      end,
      sum
    )
  end

  defp do_info(s, :releases) do
    {rA, rRA} = rel_apps(s)
    rel_apps_sums(rA, rRA, s)
  end

  defp do_info(s, :libraries) do
    d = sort(:dict.to_list(r_xref(s, :libraries)))

    map(
      fn {_L, xLib} ->
        lib_info(xLib)
      end,
      d
    )
  end

  defp do_info(_S, i) do
    :erlang.error({:no_such_info, i})
  end

  defp do_info(s, type, e) when is_atom(e) do
    do_info(s, type, [e])
  end

  defp do_info(s, :modules, modules0) when is_list(modules0) do
    modules = to_external(set(modules0))
    xMods = find_info(modules, r_xref(s, :modules), :no_such_module)

    map(
      fn xMod ->
        mod_info(xMod)
      end,
      xMods
    )
  end

  defp do_info(s, :applications, applications)
       when is_list(applications) do
    _XA = find_info(applications, r_xref(s, :applications), :no_such_application)
    aM = relation(app_mods(s))
    app = set(applications)
    appMods_S = relation_to_family(restriction(aM, app))
    appSums = sum_mods(s, to_external(appMods_S))

    map(
      fn appSum ->
        app_info(appSum, s)
      end,
      appSums
    )
  end

  defp do_info(s, :releases, releases)
       when is_list(releases) do
    _XR = find_info(releases, r_xref(s, :releases), :no_such_release)
    {aR, rRA} = rel_apps(s)
    aR_S = restriction(2, relation(aR), set(releases))
    rel_apps_sums(to_external(aR_S), rRA, s)
  end

  defp do_info(s, :libraries, libraries0)
       when is_list(libraries0) do
    libraries = to_external(set(libraries0))
    xLibs = find_info(libraries, r_xref(s, :libraries), :no_such_library)

    map(
      fn xLib ->
        lib_info(xLib)
      end,
      xLibs
    )
  end

  defp do_info(_S, i, j) when is_list(j) do
    throw_error({:no_such_info, i})
  end

  defp find_info([e | es], dict, error) do
    case :dict.find(e, dict) do
      :error ->
        throw_error({error, e})

      {:ok, x} ->
        [x | find_info(es, dict, error)]
    end
  end

  defp find_info([], _Dict, _Error) do
    []
  end

  defp rel_apps(s) do
    d = sort(:dict.to_list(r_xref(s, :applications)))

    fun = fn {_A, xApp}, acc = {aR, rRA} ->
      case r_xref_app(xApp, :rel_name) do
        [] ->
          acc

        [r] ->
          appName = r_xref_app(xApp, :name)
          {[{appName, r} | aR], [{r, xApp} | rRA]}
      end
    end

    foldl(fun, {[], []}, d)
  end

  defp rel_apps_sums(aR, rRA0, s) do
    appMods = app_mods(s)
    rRA1 = relation_to_family(relation(rRA0))
    rRA = inverse(substitution(1, rRA1))

    relMods =
      relative_product1(
        relation(aR),
        relation(appMods)
      )

    relAppsMods = relative_product1(rRA, relMods)
    relsAppsMods = to_external(relation_to_family(relAppsMods))
    sum = sum_mods(s, relsAppsMods)

    map(
      fn relAppsSums ->
        rel_info(relAppsSums, s)
      end,
      sum
    )
  end

  defp app_mods(s) do
    d = sort(:dict.to_list(r_xref(s, :modules)))

    fun = fn {_M, xMod}, acc ->
      case r_xref_mod(xMod, :app_name) do
        [] ->
          acc

        [appName] ->
          [{appName, xMod} | acc]
      end
    end

    foldl(fun, [], d)
  end

  defp mod_info(xMod) do
    r_xref_mod(name: m, app_name: appName, builtins: builtIns, dir: dir, info: info) = xMod
    app = sup_info(appName)

    {m,
     [
       [{:application, app}, {:builtins, builtIns}, {:directory, dir}]
       | info
     ]}
  end

  defp app_info({appName, modSums}, s) do
    xApp = :dict.fetch(appName, r_xref(s, :applications))
    r_xref_app(rel_name: relName, vsn: vsn, dir: dir) = xApp
    release = sup_info(relName)

    {appName,
     [
       [{:directory, dir}, {:release, release}, {:version, vsn}]
       | modSums
     ]}
  end

  defp rel_info({{relName, xApps}, modSums}, s) do
    noApps = length(xApps)
    xRel = :dict.fetch(relName, r_xref(s, :releases))
    dir = r_xref_rel(xRel, :dir)

    {relName,
     [
       [{:directory, dir}, {:no_applications, noApps}]
       | modSums
     ]}
  end

  defp lib_info(xLib) do
    r_xref_lib(name: libName, dir: dir) = xLib
    {libName, [{:directory, dir}]}
  end

  defp sup_info([]) do
    []
  end

  defp sup_info([name]) do
    [name]
  end

  defp sum_mods(s, appsMods) do
    sum_mods(s, appsMods, [])
  end

  defp sum_mods(s, [{n, xMods} | nX], l) do
    sum_mods(s, nX, [{n, no_sum(s, xMods)} | l])
  end

  defp sum_mods(_S, [], l) do
    reverse(l)
  end

  defp no_sum(s, l) when r_xref(s, :mode) === :functions do
    no_sum(l, 0, 0, 0, 0, 0, 0, 0, 0, length(l))
  end

  defp no_sum(s, l) when r_xref(s, :mode) === :modules do
    [{:no_analyzed_modules, length(l)}]
  end

  defp no_sum([xMod | d], c0, uC0, lC0, xC0, uFC0, l0, x0, eV0, noM) do
    [
      {:no_calls, {c, uC}},
      {:no_function_calls, {lC, xC, uFC}},
      {:no_functions, {l, x}},
      {:no_inter_function_calls, eV}
    ] = r_xref_mod(xMod, :info)

    no_sum(d, c0 + c, uC0 + uC, lC0 + lC, xC0 + xC, uFC0 + uFC, l0 + l, x0 + x, eV0 + eV, noM)
  end

  defp no_sum([], c, uC, lC, xC, uFC, l, x, eV, noM) do
    [
      {:no_analyzed_modules, noM},
      {:no_calls, {c, uC}},
      {:no_function_calls, {lC, xC, uFC}},
      {:no_functions, {l, x}},
      {:no_inter_function_calls, eV}
    ]
  end

  defp is_filename(f) when is_atom(f) do
    :ok
  end

  defp is_filename(f) do
    case :xref_utils.is_string(f, 31) do
      true ->
        :ok

      false ->
        throw_error({:invalid_filename, f})
    end
  end

  defp module_file(xMod) do
    :xref_utils.module_filename(
      r_xref_mod(xMod, :dir),
      r_xref_mod(xMod, :name)
    )
  end

  defp warnings(_Flag, _Message, []) do
    true
  end

  defp warnings(flag, message, [f | fs]) do
    message(flag, message, f)
    warnings(flag, message, fs)
  end

  defp pack(t) do
    pD = :erlang.erase()
    nT = pack1(t)
    _ = :erlang.erase()

    foreach(
      fn {k, v} ->
        :erlang.put(k, v)
      end,
      pD
    )

    nT
  end

  defp pack1(c) when not is_tuple(c) and not is_list(c) do
    c
  end

  defp pack1([t | ts]) do
    [pack1(t) | pack1(ts)]
  end

  defp pack1(t = {mod, fun, _})
       when is_atom(mod) and
              is_atom(fun) do
    case :erlang.get(t) do
      :undefined ->
        :erlang.put(t, t)
        t

      nT ->
        nT
    end
  end

  defp pack1({c, l}) when is_list(l) do
    {pack1(c), l}
  end

  defp pack1({mFA, l}) when is_integer(l) do
    {pack1(mFA), l}
  end

  defp pack1([]) do
    []
  end

  defp pack1(t) do
    case :erlang.get(t) do
      :undefined ->
        nT = tpack(t, tuple_size(t), [])
        :erlang.put(nT, nT)
        nT

      nT ->
        nT
    end
  end

  defp tpack(_T, 0, l) do
    :erlang.list_to_tuple(l)
  end

  defp tpack(t, i, l) do
    tpack(t, i - 1, [pack1(:erlang.element(i, t)) | l])
  end

  defp message(true, what, arg) do
    case what do
      :no_debug_info ->
        :io.format('Skipping ~ts (no debug information)~n', arg)

      :unresolved_summary1 ->
        :io.format('~tp: 1 unresolved call~n', arg)

      :unresolved_summary ->
        :io.format('~tp: ~tp unresolved calls~n', arg)

      :jam ->
        :io.format('Skipping ~ts (probably JAM file)~n', [arg])

      :unreadable ->
        :io.format('Skipping ~ts (unreadable)~n', [arg])

      :xref_attr ->
        :io.format('~ts: Skipping \'xref\' attribute ~tw~n', arg)

      :depr_attr ->
        :io.format('~ts: Skipping \'deprecated\' attribute ~tw~n', arg)

      :lib_search ->
        :io.format('Scanning library path for BEAM files... ', [])

      :lib_check ->
        :io.format('Checking library files... ', [])

      :set_up ->
        :io.format('Setting up...', arg)

      :done ->
        :io.format('done~n', arg)

      :done_file ->
        :io.format('done reading ~ts~n', arg)

      :error ->
        :io.format('error~n', arg)

      else__ ->
        :io.format('~tp~n', [{else__, arg}])
    end
  end

  defp message(_, _, _) do
    true
  end

  defp throw_error(reason) do
    throw(:erlang.error(reason))
  end

  defp error(reason) do
    {:error, :xref_base, reason}
  end
end
