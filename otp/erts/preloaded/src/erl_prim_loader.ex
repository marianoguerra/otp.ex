defmodule :m_erl_prim_loader do
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

  Record.defrecord(:r_prim_state, :prim_state,
    debug: :undefined,
    primary_archive: :undefined
  )

  Record.defrecord(:r_state, :state,
    loader: :undefined,
    hosts: [],
    data: :undefined,
    timeout: :undefined,
    prim_state: :undefined
  )

  defp debug(r_prim_state(debug: deb), term) do
    case deb do
      false ->
        :ok

      true ->
        :erlang.display(term)
    end
  end

  def start() do
    self = self()

    pid =
      spawn_link(fn ->
        start_it(self)
      end)

    receive do
      {^pid, :ok} ->
        {:ok, pid}

      {:EXIT, ^pid, reason} ->
        {:error, reason}
    end
  end

  defp start_it(parent) do
    :erlang.process_flag(:trap_exit, true)
    :erlang.register(:erl_prim_loader, self())

    loader =
      case :init.get_argument(:loader) do
        {:ok, [[loader0]]} ->
          loader0

        :error ->
          'efile'
      end

    case loader do
      'efile' ->
        start_efile(parent)

      'inet' ->
        start_inet(parent)
    end
  end

  defp start_inet(parent) do
    hosts =
      case :init.get_argument(:hosts) do
        {:ok, [hosts0]} ->
          hosts0

        _ ->
          []
      end

    aL = ipv4_list(hosts)
    true
    {:ok, tcp} = find_master(aL)
    init_ack(parent)
    pS = prim_init()
    state = r_state(loader: :inet, hosts: aL, data: tcp, timeout: 60 * 1000, prim_state: pS)
    loop(state, parent, [])
  end

  defp start_efile(parent) do
    case :prim_file.get_cwd() do
      {:error, _} ->
        report =
          'Invalid current directory or invalid filename mode: loader cannot read current directory\n'

        :erlang.display(report)
        exit({:error, :invalid_current_directory})

      _ ->
        init_ack(parent)
    end

    pS = prim_init()
    state = r_state(loader: :efile, data: :noport, timeout: 6 * 60 * 1000, prim_state: pS)
    loop(state, parent, [])
  end

  defp init_ack(pid) do
    send(pid, {self(), :ok})
    :ok
  end

  def set_path(paths) when is_list(paths) do
    request({:set_path, paths})
  end

  def get_path() do
    request({:get_path, []})
  end

  def get_file(file) when is_atom(file) do
    get_file(:erlang.atom_to_list(file))
  end

  def get_file(file) do
    check_file_result(:get_file, file, request({:get_file, file}))
  end

  def list_dir(dir) do
    check_file_result(:list_dir, dir, request({:list_dir, dir}))
  end

  def read_file_info(file) do
    check_file_result(:read_file_info, file, request({:read_file_info, file}))
  end

  def read_link_info(file) do
    check_file_result(:read_link_info, file, request({:read_link_info, file}))
  end

  def get_cwd() do
    check_file_result(:get_cwd, [], request({:get_cwd, []}))
  end

  def get_cwd(drive) do
    check_file_result(:get_cwd, drive, request({:get_cwd, [drive]}))
  end

  def set_primary_archive(:undefined, :undefined, :undefined, parserFun) do
    request({:set_primary_archive, :undefined, :undefined, :undefined, parserFun})
  end

  def set_primary_archive(file, archiveBin, fileInfo, parserFun)
      when is_list(file) and is_binary(archiveBin) and
             elem(fileInfo, 0) === :file_info do
    request({:set_primary_archive, file, archiveBin, fileInfo, parserFun})
  end

  def purge_archive_cache() do
    request(:purge_archive_cache)
  end

  def get_modules(modules, fun) do
    request({:get_modules, {modules, fun}})
  end

  def get_modules(modules, fun, path) do
    request({:get_modules, {modules, fun, path}})
  end

  defp request(req) do
    loader = :erlang.whereis(:erl_prim_loader)
    send(loader, {self(), req})

    receive do
      {^loader, res} ->
        res

      {:EXIT, ^loader, _What} ->
        :error
    end
  end

  defp check_file_result(_, _, {:error, :enoent}) do
    :error
  end

  defp check_file_result(_, _, {:error, :enotdir}) do
    :error
  end

  defp check_file_result(_, _, {:error, :einval}) do
    :error
  end

  defp check_file_result(func, target, {:error, reason}) do
    case (try do
            :erlang.atom_to_list(reason)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        :error

      errno ->
        process =
          case :erlang.process_info(
                 self(),
                 :registered_name
               ) do
            {:registered_name, r} ->
              'Process: ' ++ :erlang.atom_to_list(r) ++ '.'

            _ ->
              ''
          end

        targetStr =
          cond do
            is_atom(target) ->
              :erlang.atom_to_list(target)

            is_list(target) ->
              target

            true ->
              []
          end

        report =
          case targetStr do
            [] ->
              'File operation error: ' ++
                errno ++ '. ' ++ 'Function: ' ++ :erlang.atom_to_list(func) ++ '. ' ++ process

            _ ->
              'File operation error: ' ++
                errno ++
                '. ' ++
                'Target: ' ++
                targetStr ++ '. ' ++ 'Function: ' ++ :erlang.atom_to_list(func) ++ '. ' ++ process
          end

        _ =
          try do
            send(
              :logger,
              {:log, :error, %{label: {:erl_prim_loader, :file_error}, report: report},
               %{
                 pid: self(),
                 gl: :erlang.group_leader(),
                 time: :os.system_time(:microsecond),
                 error_logger: %{tag: :error_report, type: :std_error}
               }}
            )
          catch
            _, _ ->
              :erlang.display({:erl_prim_loader, :file_error})
              :erlang.display(report)
          end

        :error
    end
  end

  defp check_file_result(_, _, other) do
    other
  end

  defp loop(st0, parent, paths) do
    receive do
      {pid, {:set_path, newPaths}} when is_pid(pid) ->
        send(pid, {self(), :ok})
        loop(st0, parent, to_strs(newPaths))

      {pid, req} when is_pid(pid) ->
        case handle_request(req, paths, st0) do
          :ignore ->
            :ok

          {resp, r_state() = st1} ->
            send(pid, {self(), resp})
            loop(st1, parent, paths)

          {_, state2, _} ->
            exit({:bad_state, req, state2})
        end

      {:EXIT, ^parent, w} ->
        _ = handle_stop(st0)
        exit(w)

      {:EXIT, p, w} ->
        st1 = handle_exit(st0, p, w)
        loop(st1, parent, paths)

      _Message ->
        loop(st0, parent, paths)
    after
      r_state(st0, :timeout) ->
        st1 = handle_timeout(st0, parent)
        loop(st1, parent, paths)
    end
  end

  defp handle_request(req, paths, st0) do
    case req do
      {:get_path, _} ->
        {{:ok, paths}, st0}

      {:get_file, file} ->
        handle_get_file(st0, paths, file)

      {:get_modules, {modules, fun}} ->
        handle_get_modules(st0, modules, fun, paths)

      {:get_modules, {modules, fun, modPaths}} ->
        handle_get_modules(st0, modules, fun, modPaths)

      {:list_dir, dir} ->
        handle_list_dir(st0, dir)

      {:read_file_info, file} ->
        handle_read_file_info(st0, file)

      {:read_link_info, file} ->
        handle_read_link_info(st0, file)

      {:get_cwd, []} ->
        handle_get_cwd(st0, [])

      {:get_cwd, [_] = args} ->
        handle_get_cwd(st0, args)

      {:set_primary_archive, file, archiveBin, fileInfo, parserFun} ->
        handle_set_primary_archive(st0, file, archiveBin, fileInfo, parserFun)

      :purge_archive_cache ->
        handle_purge_archive_cache(st0)

      _ ->
        :ignore
    end
  end

  defp handle_get_file(state = r_state(loader: :efile), paths, file) do
    (fn ->
       case (try do
               efile_get_file_from_port(state, file, paths)
             catch
               :error, e -> {:EXIT, {e, __STACKTRACE__}}
               :exit, e -> {:EXIT, e}
               e -> e
             end) do
         {:EXIT, xXXReason} ->
           {{:error, xXXReason}, state}

         xXXRes ->
           xXXRes
       end
     end).()
  end

  defp handle_get_file(state = r_state(loader: :inet), paths, file) do
    (fn ->
       case (try do
               inet_get_file_from_port(state, file, paths)
             catch
               :error, e -> {:EXIT, {e, __STACKTRACE__}}
               :exit, e -> {:EXIT, e}
               e -> e
             end) do
         {:EXIT, xXXReason} ->
           {{:error, xXXReason}, state}

         xXXRes ->
           xXXRes
       end
     end).()
  end

  defp handle_set_primary_archive(
         state = r_state(loader: :efile),
         file,
         archiveBin,
         fileInfo,
         parserFun
       ) do
    (fn ->
       case (try do
               efile_set_primary_archive(state, file, archiveBin, fileInfo, parserFun)
             catch
               :error, e -> {:EXIT, {e, __STACKTRACE__}}
               :exit, e -> {:EXIT, e}
               e -> e
             end) do
         {:EXIT, xXXReason} ->
           {{:error, xXXReason}, state}

         xXXRes ->
           xXXRes
       end
     end).()
  end

  defp handle_purge_archive_cache(r_state(loader: :efile) = state) do
    prim_purge_cache()
    {:ok, state}
  end

  defp handle_list_dir(state = r_state(loader: :efile), dir) do
    (fn ->
       case (try do
               efile_list_dir(state, dir)
             catch
               :error, e -> {:EXIT, {e, __STACKTRACE__}}
               :exit, e -> {:EXIT, e}
               e -> e
             end) do
         {:EXIT, xXXReason} ->
           {{:error, xXXReason}, state}

         xXXRes ->
           xXXRes
       end
     end).()
  end

  defp handle_list_dir(state = r_state(loader: :inet), dir) do
    (fn ->
       case (try do
               inet_list_dir(state, dir)
             catch
               :error, e -> {:EXIT, {e, __STACKTRACE__}}
               :exit, e -> {:EXIT, e}
               e -> e
             end) do
         {:EXIT, xXXReason} ->
           {{:error, xXXReason}, state}

         xXXRes ->
           xXXRes
       end
     end).()
  end

  defp handle_read_file_info(state = r_state(loader: :efile), file) do
    (fn ->
       case (try do
               efile_read_file_info(state, file, true)
             catch
               :error, e -> {:EXIT, {e, __STACKTRACE__}}
               :exit, e -> {:EXIT, e}
               e -> e
             end) do
         {:EXIT, xXXReason} ->
           {{:error, xXXReason}, state}

         xXXRes ->
           xXXRes
       end
     end).()
  end

  defp handle_read_file_info(state = r_state(loader: :inet), file) do
    (fn ->
       case (try do
               inet_read_file_info(state, file)
             catch
               :error, e -> {:EXIT, {e, __STACKTRACE__}}
               :exit, e -> {:EXIT, e}
               e -> e
             end) do
         {:EXIT, xXXReason} ->
           {{:error, xXXReason}, state}

         xXXRes ->
           xXXRes
       end
     end).()
  end

  defp handle_read_link_info(state = r_state(loader: :efile), file) do
    (fn ->
       case (try do
               efile_read_file_info(state, file, false)
             catch
               :error, e -> {:EXIT, {e, __STACKTRACE__}}
               :exit, e -> {:EXIT, e}
               e -> e
             end) do
         {:EXIT, xXXReason} ->
           {{:error, xXXReason}, state}

         xXXRes ->
           xXXRes
       end
     end).()
  end

  defp handle_read_link_info(state = r_state(loader: :inet), file) do
    (fn ->
       case (try do
               inet_read_link_info(state, file)
             catch
               :error, e -> {:EXIT, {e, __STACKTRACE__}}
               :exit, e -> {:EXIT, e}
               e -> e
             end) do
         {:EXIT, xXXReason} ->
           {{:error, xXXReason}, state}

         xXXRes ->
           xXXRes
       end
     end).()
  end

  defp handle_get_cwd(state = r_state(loader: :efile), drive) do
    (fn ->
       case (try do
               efile_get_cwd(state, drive)
             catch
               :error, e -> {:EXIT, {e, __STACKTRACE__}}
               :exit, e -> {:EXIT, e}
               e -> e
             end) do
         {:EXIT, xXXReason} ->
           {{:error, xXXReason}, state}

         xXXRes ->
           xXXRes
       end
     end).()
  end

  defp handle_get_cwd(state = r_state(loader: :inet), drive) do
    (fn ->
       case (try do
               inet_get_cwd(state, drive)
             catch
               :error, e -> {:EXIT, {e, __STACKTRACE__}}
               :exit, e -> {:EXIT, e}
               e -> e
             end) do
         {:EXIT, xXXReason} ->
           {{:error, xXXReason}, state}

         xXXRes ->
           xXXRes
       end
     end).()
  end

  defp handle_stop(state = r_state(loader: :efile)) do
    state
  end

  defp handle_stop(state = r_state(loader: :inet)) do
    inet_stop_port(state)
  end

  defp handle_exit(state = r_state(loader: :efile), _Who, _Reason) do
    state
  end

  defp handle_exit(state = r_state(loader: :inet), who, reason) do
    inet_exit_port(state, who, reason)
  end

  defp handle_timeout(state = r_state(loader: :efile), parent) do
    efile_timeout_handler(state, parent)
  end

  defp handle_timeout(state = r_state(loader: :inet), parent) do
    inet_timeout_handler(state, parent)
  end

  defp efile_get_file_from_port(state, file, paths) do
    case is_basename(file) do
      false ->
        efile_get_file_from_port2(state, file)

      true when paths === [] ->
        efile_get_file_from_port2(state, file)

      true ->
        efile_get_file_from_port3(state, file, paths)
    end
  end

  defp efile_get_file_from_port2(r_state(prim_state: pS) = state, file) do
    {res, pS2} = prim_get_file(pS, file)

    case res do
      {:error, :port_died} ->
        exit(:"prim_load port died")

      {:error, reason} ->
        {{:error, reason}, r_state(state, prim_state: pS2)}

      {:ok, binFile} ->
        {{:ok, binFile, file}, r_state(state, prim_state: pS2)}
    end
  end

  defp efile_get_file_from_port3(state, file, [p | paths]) do
    case efile_get_file_from_port2(
           state,
           join(p, file)
         ) do
      {{:error, reason}, state1} when reason !== :emfile ->
        case paths do
          [] ->
            {{:error, reason}, state1}

          _ ->
            efile_get_file_from_port3(state1, file, paths)
        end

      result ->
        result
    end
  end

  defp efile_get_file_from_port3(state, _File, []) do
    {{:error, :enoent}, state}
  end

  defp efile_set_primary_archive(
         r_state(prim_state: pS) = state,
         file,
         archiveBin,
         fileInfo,
         parserFun
       ) do
    {res, pS2} = prim_set_primary_archive(pS, file, archiveBin, fileInfo, parserFun)
    {res, r_state(state, prim_state: pS2)}
  end

  defp efile_list_dir(r_state(prim_state: pS) = state, dir) do
    {res, pS2} = prim_list_dir(pS, dir)
    {res, r_state(state, prim_state: pS2)}
  end

  defp efile_read_file_info(r_state(prim_state: pS) = state, file, followLinks) do
    {res, pS2} = prim_read_file_info(pS, file, followLinks)
    {res, r_state(state, prim_state: pS2)}
  end

  defp efile_get_cwd(r_state(prim_state: pS) = state, drive) do
    {res, pS2} = prim_get_cwd(pS, drive)
    {res, r_state(state, prim_state: pS2)}
  end

  defp efile_timeout_handler(state, _Parent) do
    prim_purge_cache()
    state
  end

  defp handle_get_modules(r_state(loader: :efile) = st, ms, process, paths) do
    primary = r_prim_state(r_state(st, :prim_state), :primary_archive)

    res =
      case efile_any_archives(paths, primary) do
        false ->
          efile_get_mods_par(ms, process, paths)

        true ->
          get = &efile_get_file_from_port/3
          gm_get_mods(st, get, ms, process, paths)
      end

    {res, st}
  end

  defp handle_get_modules(r_state(loader: :inet) = st, ms, process, paths) do
    get = &inet_get_file_from_port/3
    {gm_get_mods(st, get, ms, process, paths), st}
  end

  defp efile_get_mods_par(ms, process, paths) do
    self = self()
    ref = make_ref()

    gmSpawn = fn ->
      efile_gm_spawn({self, ref}, ms, process, paths)
    end

    _ = spawn_link(gmSpawn)
    n = length(ms)
    efile_gm_recv(n, ref, [], [])
  end

  defp efile_any_archives([h | t], primary) do
    case name_split(primary, h) do
      {:file, _} ->
        efile_any_archives(t, primary)

      {:archive, _, _} ->
        true
    end
  end

  defp efile_any_archives([], _) do
    false
  end

  defp efile_gm_recv(0, _Ref, succ, fail) do
    {:ok, {succ, fail}}
  end

  defp efile_gm_recv(n, ref, succ, fail) do
    receive do
      {^ref, mod, {:ok, res}} ->
        efile_gm_recv(n - 1, ref, [{mod, res} | succ], fail)

      {^ref, mod, {:error, res}} ->
        efile_gm_recv(n - 1, ref, succ, [{mod, res} | fail])
    end
  end

  defp efile_gm_spawn(parentRef, ms, process, paths) do
    efile_gm_spawn_1(0, ms, parentRef, process, paths)
  end

  defp efile_gm_spawn_1(n, ms, parentRef, process, paths)
       when n >= 32 do
    receive do
      {:DOWN, _, :process, _, _} ->
        efile_gm_spawn_1(n - 1, ms, parentRef, process, paths)
    end
  end

  defp efile_gm_spawn_1(n, [m | ms], parentRef, process, paths) do
    get = fn ->
      efile_gm_get(paths, m, parentRef, process)
    end

    _ = spawn_monitor(get)
    efile_gm_spawn_1(n + 1, ms, parentRef, process, paths)
  end

  defp efile_gm_spawn_1(_, [], _, _, _) do
    :ok
  end

  defp efile_gm_get(paths, mod, parentRef, process) do
    file = :erlang.atom_to_list(mod) ++ :init.objfile_extension()
    efile_gm_get_1(paths, file, mod, parentRef, process)
  end

  defp efile_gm_get_1([p | ps], file0, mod, {parent, ref} = pR, process) do
    file = join(p, file0)

    try do
      :prim_file.read_file(file)
    catch
      _, reason ->
        res = {:error, {:crash, reason}}
        send(parent, {ref, mod, res})
    else
      {:ok, bin} ->
        res = gm_process(mod, file, bin, process)
        send(parent, {ref, mod, res})

      error ->
        _ = check_file_result(:get_modules, file, error)
        efile_gm_get_1(ps, file0, mod, pR, process)
    end
  end

  defp efile_gm_get_1([], _, mod, {parent, ref}, _Process) do
    send(parent, {ref, mod, {:error, :enoent}})
  end

  defp gm_get_mods(st, get, ms, process, paths) do
    gm_get_mods(st, get, ms, process, paths, [], [])
  end

  defp gm_get_mods(st, get, [m | ms], process, paths, succ, fail) do
    file = :erlang.atom_to_list(m) ++ :init.objfile_extension()

    case gm_arch_get(st, get, m, file, paths, process) do
      {:ok, res} ->
        gm_get_mods(st, get, ms, process, paths, [{m, res} | succ], fail)

      {:error, res} ->
        gm_get_mods(st, get, ms, process, paths, succ, [{m, res} | fail])
    end
  end

  defp gm_get_mods(_St, _Get, [], _, _, succ, fail) do
    {:ok, {succ, fail}}
  end

  defp gm_arch_get(st, get, mod, file, paths, process) do
    case get.(st, file, paths) do
      {{:error, _} = e, _} ->
        e

      {{:ok, bin, full}, _} ->
        gm_process(mod, full, bin, process)
    end
  end

  defp gm_process(mod, file, bin, process) do
    try do
      process.(mod, file, bin)
    catch
      _, error ->
        {:error, {:crash, error}}
    else
      {:ok, _} = res ->
        res

      {:error, _} = res ->
        res

      other ->
        {:error, {:bad_return, other}}
    end
  end

  defp find_master(aL, retry, reqDelay, sReSleep, tries, lReSleep) do
    {:ok, u} = ll_udp_open(0)
    find_master(u, retry, aL, reqDelay, sReSleep, [], tries, lReSleep)
  end

  defp find_master(u, retry, addrL, reqDelay, sReSleep, ignore, tries, lReSleep) do
    case find_loop(u, retry, addrL, reqDelay, sReSleep, ignore, tries, lReSleep) do
      [] ->
        find_master(u, retry, addrL, reqDelay, sReSleep, ignore, tries, lReSleep)

      servers ->
        true

        case connect_master(servers) do
          {:ok, socket} ->
            ll_close(u)
            {:ok, socket}

          _Error ->
            find_master(u, retry, addrL, reqDelay, sReSleep, servers ++ ignore, tries, lReSleep)
        end
    end
  end

  defp connect_master([{_Prio, iP, port} | servers]) do
    case ll_tcp_connect(0, iP, port) do
      {:ok, s} ->
        {:ok, s}

      _Error ->
        connect_master(servers)
    end
  end

  defp connect_master([]) do
    {:error, :ebusy}
  end

  defp find_loop(u, retry, aL, reqDelay, sReSleep, ignore, tries, lReSleep) do
    case find_loop(u, retry, aL, reqDelay, []) do
      [] ->
        :erlang.display({:erl_prim_loader, :"no server found"})

        tries1 =
          cond do
            tries > 0 ->
              sleep(sReSleep)
              tries - 1

            true ->
              sleep(lReSleep)
              0
          end

        find_loop(u, retry, aL, reqDelay, sReSleep, ignore, tries1, lReSleep)

      servers ->
        keysort(1, servers -- ignore)
    end
  end

  defp find_collect(u, retry, aL, delay, acc) do
    receive do
      {:udp, ^u, iP, _Port,
       [
         ?E,
         ?B,
         ?O,
         ?O,
         ?T,
         ?R,
         priority,
         t1,
         t0
         | _Version
       ]} ->
        elem = {priority, iP, t1 * 256 + t0}
        true

        case member(elem, acc) do
          false ->
            find_collect(u, retry, aL, delay, [elem | acc])

          true ->
            find_collect(u, retry, aL, delay, acc)
        end

      _Garbage ->
        true
        find_collect(u, retry, aL, delay, acc)
    after
      delay ->
        true

        case keymember(0, 1, acc) do
          true ->
            acc

          false ->
            find_loop(u, retry, aL, delay, acc)
        end
    end
  end

  defp sleep(time) do
    receive do
    after
      time ->
        :ok
    end
  end

  defp inet_exit_port(state, port, _Reason)
       when r_state(state, :data) === port do
    r_state(state, data: :noport, timeout: :infinity)
  end

  defp inet_exit_port(state, _, _) do
    state
  end

  defp inet_timeout_handler(state, _Parent) do
    tcp = r_state(state, :data)

    cond do
      is_port(tcp) ->
        ll_close(tcp)

      true ->
        :ok
    end

    r_state(state, timeout: :infinity, data: :noport)
  end

  defp inet_get_file_from_port(state, file, paths) do
    case is_basename(file) do
      false ->
        inet_send_and_rcv({:get, file}, file, state)

      true when paths === [] ->
        inet_send_and_rcv({:get, file}, file, state)

      true ->
        inet_get_file_from_port1(file, paths, state)
    end
  end

  defp inet_get_file_from_port1(file, [p | paths], state) do
    file1 = join(p, file)

    case inet_send_and_rcv({:get, file1}, file1, state) do
      {{:error, reason}, state1} ->
        case paths do
          [] ->
            {{:error, reason}, state1}

          _ ->
            inet_get_file_from_port1(file, paths, state1)
        end

      result ->
        result
    end
  end

  defp inet_get_file_from_port1(_File, [], state) do
    {{:error, :file_not_found}, state}
  end

  defp inet_send_and_rcv(msg, tag, state)
       when r_state(state, :data) === :noport do
    {:ok, tcp} = find_master(r_state(state, :hosts))
    inet_send_and_rcv(msg, tag, r_state(state, data: tcp, timeout: 60 * 1000))
  end

  defp inet_send_and_rcv(msg, tag, r_state(data: tcp, timeout: timeout) = state) do
    :prim_inet.send(tcp, :erlang.term_to_binary(msg))

    receive do
      {:tcp, ^tcp, binMsg} ->
        case (try do
                :erlang.binary_to_term(binMsg)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:get, {:ok, binFile}} ->
            {{:ok, binFile, tag}, state}

          {_Cmd, res = {:ok, _}} ->
            {res, state}

          {_Cmd, {:error, error}} ->
            {{:error, error}, state}

          {:error, error} ->
            {{:error, error}, state}

          {:EXIT, error} ->
            {{:error, error}, state}
        end

      {:tcp_closed, ^tcp} ->
        inet_send_and_rcv(msg, tag, r_state(state, data: :noport))

      {:tcp_error, ^tcp, _Reason} ->
        inet_send_and_rcv(msg, tag, inet_stop_port(state))

      {:EXIT, ^tcp, _} ->
        inet_send_and_rcv(msg, tag, r_state(state, data: :noport))
    after
      timeout ->
        inet_send_and_rcv(msg, tag, inet_stop_port(state))
    end
  end

  defp inet_list_dir(state, dir) do
    inet_send_and_rcv({:list_dir, dir}, :list_dir, state)
  end

  defp inet_read_file_info(state, file) do
    inet_send_and_rcv({:read_file_info, file}, :read_file_info, state)
  end

  defp inet_read_link_info(state, file) do
    inet_send_and_rcv({:read_link_info, file}, :read_link_info, state)
  end

  defp inet_get_cwd(state, []) do
    inet_send_and_rcv(:get_cwd, :get_cwd, state)
  end

  defp inet_get_cwd(state, [drive]) do
    inet_send_and_rcv({:get_cwd, drive}, :get_cwd, state)
  end

  defp inet_stop_port(r_state(data: tcp) = state) do
    :prim_inet.close(tcp)
    r_state(state, data: :noport)
  end

  defp tcp_options() do
    [{:mode, :binary}, {:packet, 4}, {:active, true}, {:deliver, :term}]
  end

  defp tcp_timeout() do
    15000
  end

  defp udp_options() do
    [{:mode, :list}, {:active, true}, {:deliver, :term}, {:broadcast, true}]
  end

  defp ll_tcp_connect(localPort, iP, remotePort) do
    case ll_open_set_bind(:tcp, :inet, :stream, tcp_options(), {0, 0, 0, 0}, localPort) do
      {:ok, s} ->
        case :prim_inet.connect(s, iP, remotePort, tcp_timeout()) do
          :ok ->
            {:ok, s}

          error ->
            port_error(s, error)
        end

      error ->
        error
    end
  end

  defp ll_udp_open(p) do
    ll_open_set_bind(:udp, :inet, :dgram, udp_options(), {0, 0, 0, 0}, p)
  end

  defp ll_open_set_bind(protocol, family, type, sOpts, iP, port) do
    case :prim_inet.open(protocol, family, type) do
      {:ok, s} ->
        case :prim_inet.setopts(s, sOpts) do
          :ok ->
            case :prim_inet.bind(s, iP, port) do
              {:ok, _} ->
                {:ok, s}

              error ->
                port_error(s, error)
            end

          error ->
            port_error(s, error)
        end

      error ->
        error
    end
  end

  defp ll_close(s) do
    :erlang.unlink(s)
    :erlang.exit(s, :kill)
  end

  defp port_error(s, error) do
    :erlang.unlink(s)
    :prim_inet.close(s)
    error
  end

  def prim_init() do
    deb =
      case :init.get_argument(:loader_debug) do
        {:ok, _} ->
          true

        :error ->
          false
      end

    cache_new(r_prim_state(debug: deb))
  end

  defp prim_purge_cache() do
    do_prim_purge_cache(:erlang.get())
  end

  defp do_prim_purge_cache([{key, val} | t]) do
    case val do
      {cache, _FI} ->
        try do
          clear_cache(key, cache)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

      _ ->
        :ok
    end

    do_prim_purge_cache(t)
  end

  defp do_prim_purge_cache([]) do
    :ok
  end

  defp prim_set_primary_archive(pS, :undefined, :undefined, :undefined, _ParserFun) do
    debug(pS, {:set_primary_archive, :clean})

    case r_prim_state(pS, :primary_archive) do
      :undefined ->
        res = {:error, :enoent}
        debug(pS, {:return, res})
        {res, pS}

      archiveFile ->
        {:primary, primZip, _FI, _ParserFun2} = :erlang.erase(archiveFile)
        :ok = :prim_zip.close(primZip)
        pS2 = r_prim_state(pS, primary_archive: :undefined)
        res = {:ok, []}
        debug(pS2, {:return, res})
        {res, pS2}
    end
  end

  defp prim_set_primary_archive(pS, archiveFile0, archiveBin, r_file_info() = fileInfo, parserFun)
       when is_list(archiveFile0) and
              is_binary(archiveBin) do
    debug(
      pS,
      {:set_primary_archive, archiveFile0, byte_size(archiveBin)}
    )

    archiveFile = real_path(absname(archiveFile0))

    {res3, pS3} =
      case r_prim_state(pS, :primary_archive) do
        :undefined ->
          case load_prim_archive(archiveFile, archiveBin, fileInfo) do
            {:ok, primZip, fI, ebins} ->
              debug(pS, {:set_primary_archive, ebins})

              :erlang.put(
                archiveFile,
                {:primary, primZip, fI, parserFun}
              )

              {{:ok, ebins}, r_prim_state(pS, primary_archive: archiveFile)}

            error ->
              debug(pS, {:set_primary_archive, error})
              {error, pS}
          end

        oldArchiveFile ->
          debug(pS, {:set_primary_archive, :clean})
          {:primary, primZip, _FI, _ParserFun} = :erlang.erase(oldArchiveFile)
          :ok = :prim_zip.close(primZip)
          pS2 = r_prim_state(pS, primary_archive: :undefined)
          prim_set_primary_archive(pS2, archiveFile, archiveBin, fileInfo, parserFun)
      end

    debug(pS3, {:return, res3})
    {res3, pS3}
  end

  def prim_get_file(pS, file) do
    debug(pS, {:get_file, file})

    {res2, pS2} =
      case name_split(
             r_prim_state(pS, :primary_archive),
             file
           ) do
        {:file, primFile} ->
          res = :prim_file.read_file(primFile)
          {res, pS}

        {:archive, archiveFile, fileInArchive} ->
          debug(
            pS,
            {:archive_get_file, archiveFile, fileInArchive}
          )

          fileComponents = path_split(fileInArchive)

          fun = fn {components, _GetInfo, getBin}, acc ->
            cond do
              components === fileComponents ->
                {false, {:ok, getBin.()}}

              true ->
                {true, acc}
            end
          end

          apply_archive(pS, fun, {:error, :enoent}, archiveFile)
      end

    debug(pS, {:return, res2})
    {res2, pS2}
  end

  def prim_list_dir(pS, dir) do
    debug(pS, {:list_dir, dir})

    {res2, pS3} =
      case name_split(
             r_prim_state(pS, :primary_archive),
             dir
           ) do
        {:file, primDir} ->
          res = :prim_file.list_dir(primDir)
          {res, pS}

        {:archive, archiveFile, fileInArchive} ->
          debug(
            pS,
            {:archive_list_dir, archiveFile, fileInArchive}
          )

          dirComponents = path_split(fileInArchive)

          fun = fn {components, _GetInfo, _GetBin}, {status, names} = acc ->
            case components do
              [revName | dC] when dC === dirComponents ->
                case revName do
                  '' ->
                    {true, {:ok, names}}

                  _ ->
                    name = reverse(revName)
                    {true, {status, [name | names]}}
                end

              ['', revName | dC] when dC === dirComponents ->
                name = reverse(revName)
                {true, {status, [name | names]}}

              [revName] when dirComponents === [''] ->
                name = reverse(revName)
                {true, {:ok, [name | names]}}

              ['', revName] when dirComponents === [''] ->
                name = reverse(revName)
                {true, {:ok, [name | names]}}

              _ ->
                {true, acc}
            end
          end

          {{status, names}, pS2} = apply_archive(pS, fun, {:error, []}, archiveFile)

          case status do
            :ok ->
              {{:ok, names}, pS2}

            :error ->
              {{:error, :enotdir}, pS2}
          end
      end

    debug(pS, {:return, res2})
    {res2, pS3}
  end

  def prim_read_file_info(pS, file, followLinks) do
    debug(pS, {:read_file_info, file})

    {res2, pS2} =
      case name_split(
             r_prim_state(pS, :primary_archive),
             file
           ) do
        {:file, primFile} ->
          case followLinks do
            true ->
              {:prim_file.read_file_info(primFile), pS}

            false ->
              {:prim_file.read_link_info(primFile), pS}
          end

        {:archive, archiveFile, []} ->
          debug(pS, {:archive_read_file_info, archiveFile})

          case :prim_file.read_file_info(archiveFile) do
            {:ok, fI} ->
              {{:ok, r_file_info(fI, type: :directory)}, pS}

            other ->
              {other, pS}
          end

        {:archive, archiveFile, fileInArchive} ->
          debug(pS, {:archive_read_file_info, file})
          fileComponents = path_split(fileInArchive)

          fun = fn {components, getInfo, _GetBin}, acc ->
            case components do
              ['' | f] when f === fileComponents ->
                {false, {:ok, getInfo.()}}

              f when f === fileComponents ->
                {false, {:ok, getInfo.()}}

              _ ->
                {true, acc}
            end
          end

          apply_archive(pS, fun, {:error, :enoent}, archiveFile)
      end

    debug(pS2, {:return, res2})
    {res2, pS2}
  end

  def prim_get_cwd(pS, []) do
    debug(pS, {:get_cwd, []})
    res = :prim_file.get_cwd()
    debug(pS, {:return, res})
    {res, pS}
  end

  def prim_get_cwd(pS, [drive]) do
    debug(pS, {:get_cwd, drive})
    res = :prim_file.get_cwd(drive)
    debug(pS, {:return, res})
    {res, pS}
  end

  defp apply_archive(pS, fun, acc, archive) do
    case :erlang.get(archive) do
      :undefined ->
        case open_archive(archive, acc, fun) do
          {:ok, primZip, {acc2, fI, _}} ->
            debug(pS, {:cache, :ok})
            :erlang.put(archive, {{:ok, primZip}, fI})
            {acc2, pS}

          error ->
            debug(pS, {:cache, error})
            {error, pS}
        end

      {:primary, primZip, fI, parserFun} ->
        case :prim_file.read_file_info(archive) do
          {:ok, fI2} when r_file_info(fI, :mtime) === r_file_info(fI2, :mtime) ->
            case foldl_archive(primZip, acc, fun) do
              {:ok, _PrimZip2, acc2} ->
                {acc2, pS}

              error ->
                debug(pS, {:primary, error})
                {error, pS}
            end

          {:ok, fI2} ->
            :ok = clear_cache(archive, {:ok, primZip})

            case load_prim_archive(archive, fI2, parserFun) do
              {:ok, primZip2, fI3, _Ebins} ->
                debug(pS, {:cache, {:update, archive}})

                :erlang.put(
                  archive,
                  {:primary, primZip2, fI3, parserFun}
                )

              error2 ->
                debug(pS, {:cache, {:clear, error2}})
            end

            apply_archive(pS, fun, acc, archive)

          error ->
            debug(pS, {:cache, {:clear, error}})
            :ok = clear_cache(archive, {:ok, primZip})
            apply_archive(pS, fun, acc, archive)
        end

      {cache, fI} ->
        case :prim_file.read_file_info(archive) do
          {:ok, fI2} when r_file_info(fI, :mtime) === r_file_info(fI2, :mtime) ->
            case cache do
              {:ok, primZip} ->
                case foldl_archive(primZip, acc, fun) do
                  {:ok, _PrimZip2, acc2} ->
                    {acc2, pS}

                  error ->
                    debug(pS, {:cache, {:clear, error}})
                    :ok = clear_cache(archive, cache)
                    debug(pS, {:cache, error})
                    :erlang.erase(archive)
                    {error, pS}
                end

              error ->
                debug(pS, {:cache, error})
                {error, pS}
            end

          error ->
            debug(pS, {:cache, {:clear, error}})
            :ok = clear_cache(archive, cache)
            apply_archive(pS, fun, acc, archive)
        end
    end
  end

  defp open_archive(archive, acc, fun) do
    case :prim_file.read_file_info(archive) do
      {:ok, fileInfo} ->
        open_archive(archive, fileInfo, acc, fun)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp open_archive(archive, fileInfo, acc, fun) do
    fakeFI = r_file_info(fileInfo, type: :directory)

    wrapper = fn {n, gI, gB}, {a, i, dirs} ->
      components = path_split(n)

      dirs2 =
        case components do
          ['' | dir] ->
            [dir | dirs]

          _ ->
            dirs
        end

      {includes, dirs3, a2} =
        ensure_virtual_dirs(components, fun, fakeFI, [{true, components}], dirs2, a)

      {_Continue, a3} = fun.({components, gI, gB}, a2)
      {true, includes, {a3, i, dirs3}}
    end

    :prim_zip.open(wrapper, {acc, fakeFI, []}, archive)
  end

  defp ensure_virtual_dirs(components, fun, fakeFI, includes, dirs, acc) do
    case components do
      [_] ->
        {includes, dirs, acc}

      [_ | dir] ->
        case :lists.member(dir, dirs) do
          false ->
            getInfo = fn ->
              fakeFI
            end

            getBin = fn ->
              <<>>
            end

            virtualDir = ['' | dir]

            includes2 = [
              {true, virtualDir, getInfo, getBin}
              | includes
            ]

            dirs2 = [dir | dirs]
            {i, f, acc2} = ensure_virtual_dirs(dir, fun, fakeFI, includes2, dirs2, acc)

            {_Continue, acc3} =
              fun.(
                {virtualDir, getInfo, getBin},
                acc2
              )

            {i, f, acc3}

          true ->
            ensure_virtual_dirs(dir, fun, fakeFI, includes, dirs, acc)
        end
    end
  end

  defp foldl_archive(primZip, acc, fun) do
    wrapper = fn {components, gI, gB}, a ->
      {continue, a2} = fun.({components, gI, gB}, a)
      {continue, true, a2}
    end

    :prim_zip.foldl(wrapper, acc, primZip)
  end

  defp cache_new(pS) do
    pS
  end

  defp clear_cache(archive, cache) do
    :erlang.erase(archive)

    case cache do
      {:ok, primZip} ->
        :prim_zip.close(primZip)

      {:error, _} ->
        :ok
    end
  end

  def is_basename(file) do
    case deep_member(?/, file) do
      true ->
        false

      false ->
        case :erlang.system_info(:os_type) do
          {:win32, _} ->
            case file do
              [_, ?: | _] ->
                false

              _ ->
                not deep_member(?\\, file)
            end

          _ ->
            true
        end
    end
  end

  defp join(p, f) do
    p ++ '/' ++ f
  end

  defp member(x, [x | _]) do
    true
  end

  defp member(x, [_ | y]) do
    member(x, y)
  end

  defp member(_X, []) do
    false
  end

  defp deep_member(x, [x | _]) do
    true
  end

  defp deep_member(x, [list | y]) when is_list(list) do
    deep_member(x, list) or deep_member(x, y)
  end

  defp deep_member(x, [atom | y]) when is_atom(atom) do
    deep_member(
      x,
      :erlang.atom_to_list(atom)
    ) or deep_member(x, y)
  end

  defp deep_member(x, [_ | y]) do
    deep_member(x, y)
  end

  defp deep_member(_X, []) do
    false
  end

  defp keymember(x, i, [y | _])
       when :erlang.element(
              i,
              y
            ) === x do
    true
  end

  defp keymember(x, i, [_ | t]) do
    keymember(x, i, t)
  end

  defp keymember(_X, _I, []) do
    false
  end

  defp keysort(i, l) do
    keysort(i, l, [])
  end

  defp keysort(i, [x | l], ls) do
    keysort(i, l, keyins(x, i, ls))
  end

  defp keysort(_I, [], ls) do
    ls
  end

  defp keyins(x, i, [y | t]) when x < :erlang.element(i, y) do
    [x, y | t]
  end

  defp keyins(x, i, [y | t]) do
    [y | keyins(x, i, t)]
  end

  defp keyins(x, _I, []) do
    [x]
  end

  defp to_strs([p | paths]) when is_atom(p) do
    [:erlang.atom_to_list(p) | to_strs(paths)]
  end

  defp to_strs([p | paths]) when is_list(p) do
    [p | to_strs(paths)]
  end

  defp to_strs([_ | paths]) do
    to_strs(paths)
  end

  defp to_strs([]) do
    []
  end

  defp reverse([] = l) do
    l
  end

  defp reverse([_] = l) do
    l
  end

  defp reverse([a, b]) do
    [b, a]
  end

  defp reverse([a, b | l]) do
    :lists.reverse(l, [b, a])
  end

  defp path_split(list) do
    path_split(list, [], [])
  end

  defp path_split([?/ | tail], path, paths) do
    path_split(tail, [], [path | paths])
  end

  defp path_split([head | tail], path, paths) do
    path_split(tail, [head | path], paths)
  end

  defp path_split([], path, paths) do
    [path | paths]
  end

  defp path_join(paths) do
    path_join(paths, [])
  end

  defp path_join([''], acc) do
    acc
  end

  defp path_join([path], acc) do
    reverse(path) ++ acc
  end

  defp path_join([path | paths], acc) do
    path_join(paths, '/' ++ reverse(path) ++ acc)
  end

  defp name_split(:undefined, file) do
    revExt = reverse(:init.archive_extension())

    case archive_split(file, revExt, []) do
      :no_split ->
        {:file, file}

      archive ->
        archive
    end
  end

  defp name_split(archiveFile, file0) do
    file = absname(file0)

    case string_match(real_path(file), archiveFile) do
      :no_match ->
        name_split(:undefined, file)

      {:match, fileInArchive} ->
        {:archive, archiveFile, fileInArchive}
    end
  end

  defp string_match([char | file], [char | archive]) do
    string_match(file, archive)
  end

  defp string_match([] = file, []) do
    {:match, file}
  end

  defp string_match([?/ | file], []) do
    {:match, file}
  end

  defp string_match(_File, _Archive) do
    :no_match
  end

  defp archive_split('/' ++ file, revExt, acc) do
    case is_prefix(revExt, acc) do
      false ->
        archive_split(file, revExt, [?/ | acc])

      true ->
        archiveFile = absname(reverse(acc))
        {:archive, archiveFile, file}
    end
  end

  defp archive_split([h | t], revExt, acc) do
    archive_split(t, revExt, [h | acc])
  end

  defp archive_split([], revExt, acc) do
    case is_prefix(revExt, acc) do
      false ->
        :no_split

      true ->
        archiveFile = absname(reverse(acc))
        {:archive, archiveFile, []}
    end
  end

  defp is_prefix([h | t1], [h | t2]) do
    is_prefix(t1, t2)
  end

  defp is_prefix([_ | _], _) do
    false
  end

  defp is_prefix([], _) do
    true
  end

  defp ipv4_list([h | t]) do
    case ipv4_address(h) do
      {:ok, iP} ->
        [iP | ipv4_list(t)]

      _ ->
        ipv4_list(t)
    end
  end

  defp ipv4_list([]) do
    []
  end

  defp ipv4_address(cs) do
    case (try do
            ipv4_addr(cs, [])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, :einval}

      addr ->
        {:ok, addr}
    end
  end

  defp ipv4_addr([c | cs], iP) when c >= ?0 and c <= ?9 do
    ipv4_addr(cs, c - ?0, iP)
  end

  defp ipv4_addr([?. | cs], n, iP) when n < 256 do
    ipv4_addr(cs, [n | iP])
  end

  defp ipv4_addr([c | cs], n, iP) when c >= ?0 and c <= ?9 do
    ipv4_addr(cs, n * 10 + (c - ?0), iP)
  end

  defp ipv4_addr([], d, [c, b, a]) when d < 256 do
    {a, b, c, d}
  end

  defp absname(name) do
    name2 = normalize(name, [])

    case pathtype(name2) do
      :absolute ->
        name2

      :relative ->
        case :prim_file.get_cwd() do
          {:ok, cwd} ->
            cwd ++ '/' ++ name2

          {:error, _} ->
            name2
        end

      :volumerelative ->
        case :prim_file.get_cwd() do
          {:ok, cwd} ->
            absname_vr(name2, cwd)

          {:error, _} ->
            name2
        end
    end
  end

  defp absname_vr([?/ | nameRest], [drive, ?: | _]) do
    [drive, ?: | nameRest]
  end

  defp absname_vr(
         [drive, ?: | nameRest],
         [drive, ?: | _] = cwd
       ) do
    cwd ++ '/' ++ nameRest
  end

  defp absname_vr([drive, ?: | nameRest], _) do
    case :prim_file.get_cwd([drive, ?:]) do
      {:ok, driveCwd} ->
        driveCwd ++ '/' ++ nameRest

      {:error, _} ->
        [drive, ?:, ?/] ++ nameRest
    end
  end

  defp pathtype(name) when is_list(name) do
    case :erlang.system_info(:os_type) do
      {:unix, _} ->
        unix_pathtype(name)

      {:win32, _} ->
        win32_pathtype(name)
    end
  end

  defp unix_pathtype(name) do
    case name do
      [?/ | _] ->
        :absolute

      [list | rest] when is_list(list) ->
        unix_pathtype(list ++ rest)

      [atom | rest] when is_atom(atom) ->
        :erlang.atom_to_list(atom) ++ rest

      _ ->
        :relative
    end
  end

  defp win32_pathtype(name) do
    case name do
      [list | rest] when is_list(list) ->
        win32_pathtype(list ++ rest)

      [atom | rest] when is_atom(atom) ->
        win32_pathtype(:erlang.atom_to_list(atom) ++ rest)

      [char, list | rest] when is_list(list) ->
        win32_pathtype([char | list ++ rest])

      [?/, ?/ | _] ->
        :absolute

      [?/ | _] ->
        :volumerelative

      [c1, c2, list | rest] when is_list(list) ->
        win32_pathtype([c1, c2 | list ++ rest])

      [_Letter, ?:, ?/ | _] ->
        :absolute

      [_Letter, ?: | _] ->
        :volumerelative

      _ ->
        :relative
    end
  end

  defp normalize(name, acc) do
    case name do
      [list | rest] when is_list(list) ->
        normalize(list ++ rest, acc)

      [atom | rest] when is_atom(atom) ->
        normalize(:erlang.atom_to_list(atom) ++ rest, acc)

      [?\\ | chars] ->
        case :erlang.system_info(:os_type) do
          {:win32, _} ->
            normalize(chars, [?/ | acc])

          _ ->
            normalize(chars, [?\\ | acc])
        end

      [char | chars] ->
        normalize(chars, [char | acc])

      [] ->
        reverse(acc)
    end
  end

  defp real_path(name) do
    real_path(name, reverse(path_split(name)), [], [])
  end

  defp real_path(_Name, [], acc, _Links) do
    path_join(acc)
  end

  defp real_path(name, ['.' | paths], acc, links) do
    real_path(name, paths, acc, links)
  end

  defp real_path(name, ['..' | paths], [''] = acc, links) do
    real_path(name, paths, acc, links)
  end

  defp real_path(name, ['..' | paths], [prev | acc], links)
       when prev !== '..' do
    real_path(name, paths, acc, links)
  end

  defp real_path(name, [path | paths], acc, links) do
    this = [path | acc]
    thisFile = path_join(this)

    case :lists.member(thisFile, links) do
      true ->
        name

      false ->
        case :prim_file.read_link(thisFile) do
          {:ok, link} ->
            case reverse(path_split(link)) do
              ['' | _] = linkPaths ->
                real_path(name, linkPaths ++ paths, [], [thisFile | links])

              linkPaths ->
                case :erlang.system_info(:os_type) do
                  {:win32, _} ->
                    real_path(name, linkPaths ++ paths, [], [thisFile | links])

                  _ ->
                    real_path(name, linkPaths ++ paths, acc, [thisFile | links])
                end
            end

          _ ->
            real_path(name, paths, this, links)
        end
    end
  end

  defp load_prim_archive(archiveFile, archiveBin, r_file_info() = fileInfo) do
    fun = fn {components, _GI, _GB}, a ->
      case components do
        ['', 'nibe', revApp] ->
          ebin = :lists.reverse(revApp, '/ebin')
          {true, [ebin | a]}

        _ ->
          {true, a}
      end
    end

    ebins0 = [archiveFile]

    case open_archive({archiveFile, archiveBin}, fileInfo, ebins0, fun) do
      {:ok, primZip, {revEbins, fI, _}} ->
        ebins = reverse(revEbins)
        {:ok, primZip, fI, ebins}

      error ->
        error
    end
  end

  defp load_prim_archive(archiveFile, fileInfo, parserFun) do
    case parserFun.(archiveFile) do
      {:ok, archiveBin} ->
        load_prim_archive(archiveFile, archiveBin, fileInfo)

      error ->
        error
    end
  end
end
