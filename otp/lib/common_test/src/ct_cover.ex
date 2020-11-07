defmodule :m_ct_cover do
  use Bitwise
  require Record

  Record.defrecord(:r_conn, :conn,
    handle: :undefined,
    targetref: :undefined,
    address: :undefined,
    callback: :undefined
  )

  Record.defrecord(:r_testspec, :testspec,
    spec_dir: :undefined,
    nodes: [],
    init: [],
    label: [],
    profile: [],
    logdir: ['.'],
    logopts: [],
    basic_html: [],
    esc_chars: [],
    verbosity: [],
    silent_connections: [],
    cover: [],
    cover_stop: [],
    config: [],
    userconfig: [],
    event_handler: [],
    ct_hooks: [],
    enable_builtin_hooks: true,
    release_shell: false,
    include: [],
    auto_compile: [],
    abort_if_missing_suites: [],
    stylesheet: [],
    multiply_timetraps: [],
    scale_timetraps: [],
    create_priv_dir: [],
    alias: [],
    tests: [],
    unknown: [],
    merge_tests: true
  )

  Record.defrecord(:r_cover, :cover,
    app: :none,
    local_only: false,
    level: :details,
    excl_mods: [],
    incl_mods: [],
    cross: [],
    src: []
  )

  Record.defrecord(:r_conn_log, :conn_log,
    header: true,
    client: :undefined,
    name: :undefined,
    address: :undefined,
    conn_pid: :undefined,
    action: :undefined,
    module: :undefined
  )

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

  def add_nodes([]) do
    {:ok, []}
  end

  def add_nodes(nodes) do
    case :erlang.whereis(:cover_server) do
      :undefined ->
        {:error, :cover_not_running}

      _ ->
        nodes0 = :cover.which_nodes()

        nodes1 =
          for node <- nodes,
              :lists.member(node, nodes0) == false do
            node
          end

        :ct_logs.log('COVER INFO', 'Adding nodes to cover test: ~w', [nodes1])

        case :cover.start(nodes1) do
          result = {:ok, startedNodes} ->
            :ct_logs.log('COVER INFO', 'Successfully added nodes to cover test: ~w', [
              startedNodes
            ])

            result

          error ->
            :ct_logs.log('COVER INFO', 'Failed to add nodes to cover test: ~tp', [error])
            error
        end
    end
  end

  def remove_nodes([]) do
    :ok
  end

  def remove_nodes(nodes) do
    case :erlang.whereis(:cover_server) do
      :undefined ->
        {:error, :cover_not_running}

      _ ->
        nodes0 = :cover.which_nodes()

        toRemove =
          for node <- nodes,
              :lists.member(node, nodes0) do
            node
          end

        :ct_logs.log('COVER INFO', 'Removing nodes from cover test: ~w', [toRemove])

        case :cover.stop(toRemove) do
          :ok ->
            :ct_logs.log('COVER INFO', 'Successfully removed nodes from cover test.', [])
            :ok

          error ->
            :ct_logs.log('COVER INFO', 'Failed to remove nodes from cover test: ~tp', [error])
            error
        end
    end
  end

  def cross_cover_analyse(level, tests) do
    :test_server_ctrl.cross_cover_analyse(level, tests)
  end

  def get_spec(file) do
    try do
      get_spec_test(file)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp get_spec_test(file) do
    dir = :filename.dirname(file)

    case :filelib.is_file(file) do
      true ->
        case :file.consult(file) do
          {:ok, terms} ->
            import =
              case :lists.keysearch(:import, 1, terms) do
                {:value, {_, imps = [s | _]}} when is_list(s) ->
                  impsFN =
                    :lists.map(
                      fn f ->
                        :filename.absname(f, dir)
                      end,
                      imps
                    )

                  test_files(impsFN, impsFN)

                {:value, {_, imp = [iC | _]}} when is_integer(iC) ->
                  impFN = :filename.absname(imp, dir)
                  test_files([impFN], [impFN])

                _ ->
                  []
              end

            export =
              case :lists.keysearch(:export, 1, terms) do
                {:value, {_, exp = [eC | _]}} when is_integer(eC) ->
                  :filename.absname(exp, dir)

                {:value, {_, [exp]}} ->
                  :filename.absname(exp, dir)

                _ ->
                  :undefined
              end

            nodes =
              case :lists.keysearch(:nodes, 1, terms) do
                {:value, {_, ns}} ->
                  ns

                _ ->
                  []
              end

            case collect_apps(terms, []) do
              res when res == [] or length(res) == 1 ->
                apps =
                  case res do
                    [] ->
                      [r_cover(app: :none, level: :details)]

                    _ ->
                      res
                  end

                case get_cover_opts(apps, terms, dir, []) do
                  e = {:error, _} ->
                    e

                  [coverSpec] ->
                    coverSpec1 = remove_excludes_and_dups(coverSpec)
                    {file, nodes, import, export, coverSpec1}

                  _ ->
                    {:error, :multiple_apps_in_cover_spec}
                end

              apps when is_list(apps) ->
                {:error, :multiple_apps_in_cover_spec}
            end

          error ->
            {:error, {:invalid_cover_spec, error}}
        end

      false ->
        {:error, {:cant_read_cover_spec_file, file}}
    end
  end

  defp collect_apps([{:level, level} | ts], apps) do
    collect_apps(ts, [r_cover(app: :none, level: level) | apps])
  end

  defp collect_apps([{:incl_app, app, level} | ts], apps) do
    collect_apps(ts, [r_cover(app: app, level: level) | apps])
  end

  defp collect_apps([_ | ts], apps) do
    collect_apps(ts, apps)
  end

  defp collect_apps([], apps) do
    apps
  end

  defp get_cover_opts([app | apps], terms, dir, coverInfo) do
    case get_app_info(app, terms, dir) do
      e = {:error, _} ->
        e

      appInfo ->
        appInfo1 = files2mods(appInfo)
        get_cover_opts(apps, terms, dir, [appInfo1 | coverInfo])
    end
  end

  defp get_cover_opts([], _, _, coverInfo) do
    :lists.reverse(coverInfo)
  end

  defp get_app_info(app = r_cover(app: :none), [{:incl_dirs, dirs} | terms], dir) do
    get_app_info(app, [{:incl_dirs, :none, dirs} | terms], dir)
  end

  defp get_app_info(app = r_cover(app: name), [{:incl_dirs, name, dirs} | terms], dir) do
    case get_files(dirs, dir, '.beam', false, []) do
      e = {:error, _} ->
        e

      mods1 ->
        mods = r_cover(app, :incl_mods)
        get_app_info(r_cover(app, incl_mods: mods ++ mods1), terms, dir)
    end
  end

  defp get_app_info(app = r_cover(app: :none), [{:incl_dirs_r, dirs} | terms], dir) do
    get_app_info(app, [{:incl_dirs_r, :none, dirs} | terms], dir)
  end

  defp get_app_info(app = r_cover(app: name), [{:incl_dirs_r, name, dirs} | terms], dir) do
    case get_files(dirs, dir, '.beam', true, []) do
      e = {:error, _} ->
        e

      mods1 ->
        mods = r_cover(app, :incl_mods)
        get_app_info(r_cover(app, incl_mods: mods ++ mods1), terms, dir)
    end
  end

  defp get_app_info(app = r_cover(app: :none), [{:incl_mods, mods1} | terms], dir) do
    get_app_info(app, [{:incl_mods, :none, mods1} | terms], dir)
  end

  defp get_app_info(app = r_cover(app: name), [{:incl_mods, name, mods1} | terms], dir) do
    mods = r_cover(app, :incl_mods)
    get_app_info(r_cover(app, incl_mods: mods ++ mods1), terms, dir)
  end

  defp get_app_info(app = r_cover(app: :none), [{:excl_dirs, dirs} | terms], dir) do
    get_app_info(app, [{:excl_dirs, :none, dirs} | terms], dir)
  end

  defp get_app_info(app = r_cover(app: name), [{:excl_dirs, name, dirs} | terms], dir) do
    case get_files(dirs, dir, '.beam', false, []) do
      e = {:error, _} ->
        e

      mods1 ->
        mods = r_cover(app, :excl_mods)
        get_app_info(r_cover(app, excl_mods: mods ++ mods1), terms, dir)
    end
  end

  defp get_app_info(app = r_cover(app: :none), [{:excl_dirs_r, dirs} | terms], dir) do
    get_app_info(app, [{:excl_dirs_r, :none, dirs} | terms], dir)
  end

  defp get_app_info(app = r_cover(app: name), [{:excl_dirs_r, name, dirs} | terms], dir) do
    case get_files(dirs, dir, '.beam', true, []) do
      e = {:error, _} ->
        e

      mods1 ->
        mods = r_cover(app, :excl_mods)
        get_app_info(r_cover(app, excl_mods: mods ++ mods1), terms, dir)
    end
  end

  defp get_app_info(app = r_cover(app: :none), [{:excl_mods, mods1} | terms], dir) do
    get_app_info(app, [{:excl_mods, :none, mods1} | terms], dir)
  end

  defp get_app_info(app = r_cover(app: name), [{:excl_mods, name, mods1} | terms], dir) do
    mods = r_cover(app, :excl_mods)
    get_app_info(r_cover(app, excl_mods: mods ++ mods1), terms, dir)
  end

  defp get_app_info(app = r_cover(app: :none), [{:cross, cross} | terms], dir) do
    get_app_info(app, [{:cross, :none, cross} | terms], dir)
  end

  defp get_app_info(app = r_cover(app: name), [{:cross, name, cross1} | terms], dir) do
    cross = r_cover(app, :cross)
    get_app_info(r_cover(app, cross: cross ++ cross1), terms, dir)
  end

  defp get_app_info(app = r_cover(app: :none), [{:src_dirs, dirs} | terms], dir) do
    get_app_info(app, [{:src_dirs, :none, dirs} | terms], dir)
  end

  defp get_app_info(app = r_cover(app: name), [{:src_dirs, name, dirs} | terms], dir) do
    case get_files(dirs, dir, '.erl', false, []) do
      e = {:error, _} ->
        e

      src1 ->
        src = r_cover(app, :src)
        get_app_info(r_cover(app, src: src ++ src1), terms, dir)
    end
  end

  defp get_app_info(app = r_cover(app: :none), [{:src_dirs_r, dirs} | terms], dir) do
    get_app_info(app, [{:src_dirs_r, :none, dirs} | terms], dir)
  end

  defp get_app_info(app = r_cover(app: name), [{:src_dirs_r, name, dirs} | terms], dir) do
    case get_files(dirs, dir, '.erl', true, []) do
      e = {:error, _} ->
        e

      src1 ->
        src = r_cover(app, :src)
        get_app_info(r_cover(app, src: src ++ src1), terms, dir)
    end
  end

  defp get_app_info(app = r_cover(app: :none), [{:src_files, src1} | terms], dir) do
    get_app_info(app, [{:src_files, :none, src1} | terms], dir)
  end

  defp get_app_info(app = r_cover(app: name), [{:src_files, name, src1} | terms], dir) do
    src = r_cover(app, :src)
    get_app_info(r_cover(app, src: src ++ src1), terms, dir)
  end

  defp get_app_info(app = r_cover(app: :none), [{:local_only, bool} | terms], dir) do
    get_app_info(app, [{:local_only, :none, bool} | terms], dir)
  end

  defp get_app_info(app = r_cover(app: name), [{:local_only, name, bool} | terms], dir) do
    get_app_info(r_cover(app, local_only: bool), terms, dir)
  end

  defp get_app_info(app, [_ | terms], dir) do
    get_app_info(app, terms, dir)
  end

  defp get_app_info(app, [], _) do
    app
  end

  defp get_files([dir | dirs], rootDir, ext, recurse, files) do
    dirAbs = :filename.absname(dir, rootDir)

    case :file.list_dir(dirAbs) do
      {:ok, entries} ->
        {subDirs, matches} = analyse_files(entries, dirAbs, ext, [], [])

        cond do
          recurse == false ->
            get_files(dirs, rootDir, ext, recurse, files ++ matches)

          true ->
            files1 = get_files(subDirs, rootDir, ext, recurse, files ++ matches)
            get_files(dirs, rootDir, ext, recurse, files1)
        end

      {:error, reason} ->
        {:error, {reason, dirAbs}}
    end
  end

  defp get_files([], _RootDir, _Ext, _R, files) do
    files
  end

  defp analyse_files([f | fs], dir, ext, dirs, matches) do
    fullname = :filename.absname(f, dir)
    {:ok, info} = :file.read_file_info(fullname)

    case r_file_info(info, :type) do
      :directory ->
        analyse_files(fs, dir, ext, [fullname | dirs], matches)

      _ ->
        case :filename.extension(fullname) do
          '.beam' when ext == '.beam' ->
            mod = :erlang.list_to_atom(:filename.rootname(f))
            analyse_files(fs, dir, ext, dirs, [mod | matches])

          '.erl' when ext == '.erl' ->
            analyse_files(fs, dir, ext, dirs, [fullname | matches])

          _ ->
            analyse_files(fs, dir, ext, dirs, matches)
        end
    end
  end

  defp analyse_files([], _Dir, _Ext, dirs, matches) do
    {dirs, matches}
  end

  defp test_files([f | fs], ret) do
    case :filelib.is_file(f) do
      true ->
        test_files(fs, ret)

      false ->
        throw({:error, {:invalid_cover_file, f}})
    end
  end

  defp test_files([], ret) do
    ret
  end

  defp remove_excludes_and_dups(
         coverData =
           r_cover(
             excl_mods: excl,
             incl_mods: incl
           )
       ) do
    incl1 =
      for mod <- incl,
          :lists.member(mod, excl) == false do
        mod
      end

    incl2 =
      :lists.sort(
        :lists.foldl(
          fn m, l ->
            case :lists.member(m, l) do
              true ->
                l

              false ->
                [m | l]
            end
          end,
          [],
          incl1
        )
      )

    r_cover(coverData, incl_mods: incl2)
  end

  defp files2mods(info = r_cover(excl_mods: exclFs, incl_mods: inclFs, cross: cross)) do
    r_cover(info,
      excl_mods: files2mods1(exclFs),
      incl_mods: files2mods1(inclFs),
      cross:
        for {tag, fs} <- cross do
          {tag, files2mods1(fs)}
        end
    )
  end

  defp files2mods1([m | fs]) when is_atom(m) do
    [m | files2mods1(fs)]
  end

  defp files2mods1([f | fs]) when is_list(f) do
    m = :filename.rootname(:filename.basename(f))
    [:erlang.list_to_atom(m) | files2mods1(fs)]
  end

  defp files2mods1([]) do
    []
  end
end
