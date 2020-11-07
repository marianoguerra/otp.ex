defmodule :m_init do
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

  Record.defrecord(:r_state, :state,
    flags: [],
    args: [],
    start: [],
    kernel: [],
    bootpid: :undefined,
    status: {:starting, :starting},
    script_id: [],
    loaded: [],
    subscribed: []
  )

  Record.defrecord(:r_es, :es,
    init: :undefined,
    debug: :undefined,
    path: :undefined,
    pa: :undefined,
    pz: :undefined,
    path_choice: :undefined,
    prim_load: :undefined,
    load_mode: :undefined,
    vars: :undefined
  )

  defp debug(false, _) do
    :ok
  end

  defp debug(_, t) do
    :erlang.display(t)
  end

  def get_arguments() do
    request(:get_arguments)
  end

  def get_plain_arguments() do
    bs2ss(request(:get_plain_arguments))
  end

  def get_argument(arg) do
    request({:get_argument, arg})
  end

  def script_id() do
    request(:script_id)
  end

  defp bs2as(l0) when is_list(l0) do
    map(&b2a/1, l0)
  end

  defp bs2as(l) do
    l
  end

  defp bs2ss(l0) when is_list(l0) do
    map(&b2s/1, l0)
  end

  defp bs2ss(l) do
    l
  end

  def get_status() do
    request(:get_status)
  end

  def fetch_loaded() do
    request(:fetch_loaded)
  end

  def ensure_loaded(module) do
    request({:ensure_loaded, module})
  end

  def make_permanent(boot, config) do
    request({:make_permanent, boot, config})
  end

  def notify_when_started(pid) do
    request({:notify_when_started, pid})
  end

  def wait_until_started() do
    receive do
      {:init, :started} ->
        :ok
    end
  end

  defp request(req) do
    send(:init, {self(), req})

    receive do
      {:init, rep} ->
        rep
    end
  end

  def restart() do
    restart([])
  end

  def restart([]) do
    send(:init, {:stop, :restart})
    :ok
  end

  def restart([{:mode, mode}])
      when mode === :embedded or
             mode === :interactive do
    send(:init, {:stop, {:restart, mode}})
    :ok
  end

  def restart(opts) when is_list(opts) do
    :erlang.error(:badarg, [opts])
  end

  def reboot() do
    send(:init, {:stop, :reboot})
    :ok
  end

  def stop() do
    send(:init, {:stop, :stop})
    :ok
  end

  def stop(status)
      when is_integer(status) and
             status >= 0 do
    stop_1(status)
  end

  def stop(status) when is_list(status) do
    case is_bytelist(status) do
      true ->
        stop_1(status)

      false ->
        :erlang.error(:badarg)
    end
  end

  def stop(_) do
    :erlang.error(:badarg)
  end

  defp is_bytelist([b | bs])
       when is_integer(b) and b >= 0 and
              b < 256 do
    is_bytelist(bs)
  end

  defp is_bytelist([]) do
    true
  end

  defp is_bytelist(_) do
    false
  end

  defp stop_1(status) do
    send(:init, {:stop, {:stop, status}})
    :ok
  end

  def boot(bootArgs) do
    :erlang.register(:init, self())
    :erlang.process_flag(:trap_exit, true)
    {start0, flags, args} = parse_boot_args(bootArgs)

    case b2a(get_flag(:profile_boot, flags, false)) do
      false ->
        :ok

      true ->
        debug_profile_start()
    end

    start = map(&prepare_run_args/1, start0)
    boot(start, flags, args)
  end

  defp prepare_run_args({:eval, [expr]}) do
    {:eval, expr}
  end

  defp prepare_run_args({_, l = []}) do
    bs2as(l)
  end

  defp prepare_run_args({_, l = [_]}) do
    bs2as(l)
  end

  defp prepare_run_args({:s, [[m, f] | args]}) do
    [[b2a(m), b2a(f)] | bs2as(args)]
  end

  defp prepare_run_args({:run, [[m, f] | args]}) do
    [[b2a(m), b2a(f)] | bs2ss(args)]
  end

  defp b2a(bin) when is_binary(bin) do
    :erlang.list_to_atom(b2s(bin))
  end

  defp b2a(a) when is_atom(a) do
    a
  end

  defp b2s(bin) when is_binary(bin) do
    try do
      :unicode.characters_to_list(
        bin,
        :file.native_name_encoding()
      )
    catch
      _, _ ->
        :erlang.binary_to_list(bin)
    end
  end

  defp b2s(l) when is_list(l) do
    l
  end

  defp map(_F, []) do
    []
  end

  defp map(f, [x | rest]) do
    [f.(x) | map(f, rest)]
  end

  def code_path_choice() do
    case get_argument(:code_path_choice) do
      {:ok, [['strict']]} ->
        :strict

      {:ok, [['relaxed']]} ->
        :relaxed

      _Else ->
        :relaxed
    end
  end

  defp boot(start, flags, args) do
    start_on_load_handler_process()
    bootPid = do_boot(flags, start)
    state = r_state(flags: flags, args: args, start: start, bootpid: bootPid)
    boot_loop(bootPid, state)
  end

  defp to_string(x, d) when is_list(x) and d < 4 do
    f = flatten(x, [])

    case printable_list(f) do
      true when length(f) > 0 ->
        f

      _false ->
        list =
          for e <- x do
            to_string(e, d + 1)
          end

        flatten(['[', join(list), ']'], [])
    end
  end

  defp to_string(x, _D) when is_list(x) do
    '[_]'
  end

  defp to_string(x, _D) when is_atom(x) do
    :erlang.atom_to_list(x)
  end

  defp to_string(x, _D) when is_pid(x) do
    :erlang.pid_to_list(x)
  end

  defp to_string(x, _D) when is_float(x) do
    :erlang.float_to_list(x)
  end

  defp to_string(x, _D) when is_integer(x) do
    :erlang.integer_to_list(x)
  end

  defp to_string(x, d) when is_tuple(x) and d < 4 do
    list =
      for e <- :erlang.tuple_to_list(x) do
        to_string(e, d + 1)
      end

    flatten(['{', join(list), '}'], [])
  end

  defp to_string(x, _D) when is_tuple(x) do
    '{_}'
  end

  defp to_string(_X, _D) do
    ''
  end

  defp printable_list([h | t])
       when is_integer(h) and h >= 32 and
              h <= 126 do
    printable_list(t)
  end

  defp printable_list([?\n | t]) do
    printable_list(t)
  end

  defp printable_list([?\r | t]) do
    printable_list(t)
  end

  defp printable_list([?\t | t]) do
    printable_list(t)
  end

  defp printable_list([]) do
    true
  end

  defp printable_list(_) do
    false
  end

  defp join([] = t) do
    t
  end

  defp join([_Elem] = t) do
    t
  end

  defp join([elem | t]) do
    [[elem, ','] | join(t)]
  end

  defp flatten([h | t], tail) when is_list(h) do
    flatten(h, flatten(t, tail))
  end

  defp flatten([h | t], tail) do
    [h | flatten(t, tail)]
  end

  defp flatten([], tail) do
    tail
  end

  defp things_to_string([x | rest]) do
    ' (' ++ to_string(x, 0) ++ ')' ++ things_to_string(rest)
  end

  defp things_to_string([]) do
    ''
  end

  defp halt_string(string, list) do
    string ++ things_to_string(list)
  end

  defp crash(string, list) do
    :erlang.halt(halt_string(string, list))
  end

  defp boot_loop(bootPid, state) do
    receive do
      {^bootPid, :loaded, newlyLoaded} ->
        loaded = newlyLoaded ++ r_state(state, :loaded)
        boot_loop(bootPid, r_state(state, loaded: loaded))

      {^bootPid, :started, kernelPid} ->
        boot_loop(
          bootPid,
          new_kernelpid(kernelPid, bootPid, state)
        )

      {^bootPid, :progress, :started} ->
        {inS, _} = r_state(state, :status)
        notify(r_state(state, :subscribed))

        boot_loop(
          bootPid,
          r_state(state, status: {inS, :started}, subscribed: [])
        )

      {^bootPid, :progress, newStatus} ->
        {inS, _} = r_state(state, :status)
        boot_loop(bootPid, r_state(state, status: {inS, newStatus}))

      {^bootPid, {:script_id, id}} ->
        boot_loop(bootPid, r_state(state, script_id: id))

      {:EXIT, ^bootPid, :normal} ->
        {_, pS} = r_state(state, :status)
        notify(r_state(state, :subscribed))
        loop(r_state(state, status: {:started, pS}, subscribed: []))

      {:EXIT, ^bootPid, reason} ->
        :erlang.display({'init terminating in do_boot', reason})
        crash('init terminating in do_boot', [reason])

      {:EXIT, pid, reason} ->
        kernel = r_state(state, :kernel)
        terminate(pid, kernel, reason)
        boot_loop(bootPid, state)

      {:stop, reason} ->
        stop(reason, state)

      {from, :fetch_loaded} ->
        send(from, {:init, r_state(state, :loaded)})
        garb_boot_loop(bootPid, r_state(state, loaded: []))

      {from, {:ensure_loaded, module}} ->
        {res, loaded} = ensure_loaded(module, r_state(state, :loaded))
        send(from, {:init, res})
        boot_loop(bootPid, r_state(state, loaded: loaded))

      msg ->
        boot_loop(bootPid, handle_msg(msg, state))
    end
  end

  defp ensure_loaded(module, loaded) do
    case :erlang.module_loaded(module) do
      true ->
        {{:module, module}, loaded}

      false ->
        do_ensure_loaded(module, loaded)
    end
  end

  defp do_ensure_loaded(module, loaded) do
    file = :erlang.atom_to_list(module) ++ objfile_extension()

    case :erl_prim_loader.get_file(file) do
      {:ok, binCode, fullName} ->
        case do_load_module(module, binCode) do
          :ok ->
            {{:module, module}, [{module, fullName} | loaded]}

          :error ->
            {:error, [{module, fullName} | loaded]}
        end

      error ->
        {error, loaded}
    end
  end

  defp notify(pids) do
    :lists.foreach(
      fn pid ->
        send(pid, {:init, :started})
      end,
      pids
    )
  end

  defp garb_boot_loop(bootPid, state) do
    :erlang.garbage_collect()
    boot_loop(bootPid, state)
  end

  defp new_kernelpid({name, {:ok, pid}}, bootPid, state)
       when is_pid(pid) do
    :erlang.link(pid)
    send(bootPid, {self(), :ok, pid})
    kernel = r_state(state, :kernel)
    r_state(state, kernel: [{name, pid} | kernel])
  end

  defp new_kernelpid({_Name, :ignore}, bootPid, state) do
    send(bootPid, {self(), :ignore})
    state
  end

  defp new_kernelpid({name, what}, bootPid, state) do
    :erlang.display({'could not start kernel pid', name, what})
    clear_system(bootPid, state)
    crash('could not start kernel pid', [name, what])
  end

  defp loop(state) do
    receive do
      {:EXIT, pid, reason} ->
        kernel = r_state(state, :kernel)
        terminate(pid, kernel, reason)
        loop(state)

      {:stop, reason} ->
        stop(reason, state)

      {from, :fetch_loaded} ->
        loaded = r_state(state, :loaded)
        send(from, {:init, loaded})
        loop(state)

      msg ->
        loop(handle_msg(msg, state))
    end
  end

  defp handle_msg(msg, state0) do
    case (try do
            do_handle_msg(msg, state0)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:new_state, state} ->
        state

      _ ->
        state0
    end
  end

  defp do_handle_msg(msg, state) do
    r_state(flags: flags, status: status, script_id: sid, args: args, subscribed: subscribed) =
      state

    case msg do
      {from, :get_plain_arguments} ->
        send(from, {:init, args})

      {from, :get_arguments} ->
        send(from, {:init, get_arguments(flags)})

      {from, {:get_argument, arg}} ->
        send(from, {:init, get_argument(arg, flags)})

      {from, :get_status} ->
        send(from, {:init, status})

      {from, :script_id} ->
        send(from, {:init, sid})

      {from, {:make_permanent, boot, config}} ->
        {res, state1} = make_permanent(boot, config, flags, state)
        send(from, {:init, res})
        {:new_state, state1}

      {from, {:notify_when_started, pid}} ->
        case status do
          {inS, pS} when inS === :started or pS === :started ->
            send(from, {:init, :started})

          _ ->
            send(from, {:init, :ok})
            {:new_state, r_state(state, subscribed: [pid | subscribed])}
        end

      {from, {:ensure_loaded, _}} ->
        send(from, {:init, :not_allowed})

      x ->
        case :erlang.whereis(:user) do
          :undefined ->
            try do
              send(
                :logger,
                {:log, :info, 'init got unexpected: ~p', [x],
                 %{
                   :pid => self(),
                   :gl => self(),
                   :time => :os.system_time(:microsecond),
                   :error_logger => %{:tag => :info_msg}
                 }}
              )
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end

          user ->
            send(user, x)
            :ok
        end
    end
  end

  defp make_permanent(boot, config, flags0, state) do
    case set_flag(:boot, boot, flags0) do
      {:ok, flags1} ->
        case set_flag(:config, config, flags1) do
          {:ok, flags} ->
            {:ok, r_state(state, flags: flags)}

          error ->
            {error, state}
        end

      error ->
        {error, state}
    end
  end

  defp set_flag(_Flag, false, flags) do
    {:ok, flags}
  end

  defp set_flag(flag, value, flags) when is_list(value) do
    encoding = :file.native_name_encoding()

    case (try do
            :unicode.characters_to_binary(value, encoding, encoding)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, :badarg}

      aValue ->
        {:ok, set_argument(flags, flag, aValue)}
    end
  end

  defp set_flag(_, _, _) do
    {:error, :badarg}
  end

  defp stop(reason, state) do
    bootPid = r_state(state, :bootpid)
    {_, progress} = r_state(state, :status)
    state1 = r_state(state, status: {:stopping, progress})
    clear_system(bootPid, state1)
    do_stop(reason, state1)
  end

  defp do_stop(
         {:restart, mode},
         r_state(start: start, flags: flags0, args: args)
       ) do
    flags = update_flag(:mode, flags0, :erlang.atom_to_binary(mode))
    do_restart(start, flags, args)
  end

  defp do_stop(
         :restart,
         r_state(start: start, flags: flags, args: args)
       ) do
    do_restart(start, flags, args)
  end

  defp do_stop(:reboot, _) do
    :erlang.halt()
  end

  defp do_stop(:stop, state) do
    stop_heart(state)
    :erlang.halt()
  end

  defp do_stop({:stop, status}, state) do
    stop_heart(state)
    :erlang.halt(status)
  end

  defp do_restart(start, flags, args) do
    flush()
    :erts_internal.erase_persistent_terms()
    boot(start, flags, args)
  end

  defp clear_system(bootPid, state) do
    heart = get_heart(r_state(state, :kernel))
    logger = get_logger(r_state(state, :kernel))
    shutdown_pids(heart, logger, bootPid, state)
    unload(heart)
    kill_em([logger])
    do_unload([:logger_server])
  end

  defp flush() do
    receive do
      _M ->
        flush()
    after
      0 ->
        :ok
    end
  end

  defp stop_heart(state) do
    case get_heart(r_state(state, :kernel)) do
      false ->
        :ok

      pid ->
        bootPid = self()
        shutdown_kernel_pid(pid, bootPid, self(), state)
    end
  end

  defp shutdown_pids(heart, logger, bootPid, state) do
    timer = shutdown_timer(r_state(state, :flags))

    try do
      shutdown(r_state(state, :kernel), bootPid, timer, state)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    kill_all_pids(heart, logger)
    kill_all_ports(heart)
    flush_timout(timer)
  end

  defp get_heart(kernel) do
    get_kernelpid(:heart, kernel)
  end

  defp get_logger(kernel) do
    get_kernelpid(:logger, kernel)
  end

  defp get_kernelpid(name, [{name, pid} | _Kernel]) do
    pid
  end

  defp get_kernelpid(name, [_ | kernel]) do
    get_kernelpid(name, kernel)
  end

  defp get_kernelpid(_, _) do
    false
  end

  defp shutdown([{except, _Pid} | kernel], bootPid, timer, state)
       when except == :heart or except == :logger do
    shutdown(kernel, bootPid, timer, state)
  end

  defp shutdown([{_Name, pid} | kernel], bootPid, timer, state) do
    shutdown_kernel_pid(pid, bootPid, timer, state)
    shutdown(kernel, bootPid, timer, state)
  end

  defp shutdown(_, _, _, _) do
    true
  end

  defp shutdown_kernel_pid(pid, bootPid, timer, state) do
    send(pid, {:EXIT, bootPid, :shutdown})
    shutdown_loop(pid, timer, state, [])
  end

  defp shutdown_loop(pid, timer, state, exits) do
    receive do
      {:EXIT, ^pid, _} ->
        resend(reverse(exits))
        :ok

      {^timer, :timeout} ->
        :erlang.display({:init, :shutdown_timeout})
        throw(:timeout)

      {:stop, _} ->
        shutdown_loop(pid, timer, state, exits)

      {from, :fetch_loaded} ->
        send(from, {:init, r_state(state, :loaded)})
        shutdown_loop(pid, timer, state, exits)

      {:EXIT, otherP, reason} ->
        shutdown_loop(pid, timer, state, [{:EXIT, otherP, reason} | exits])

      msg ->
        state1 = handle_msg(msg, state)
        shutdown_loop(pid, timer, state1, exits)
    end
  end

  defp resend([exitMsg | exits]) do
    send(self(), exitMsg)
    resend(exits)
  end

  defp resend(_) do
    :ok
  end

  defp kill_all_pids(heart, logger) do
    case get_pids(heart, logger) do
      [] ->
        :ok

      pids ->
        kill_em(pids)
        kill_all_pids(heart, logger)
    end
  end

  defp get_pids(heart, logger) do
    pids =
      for p <- :erlang.processes(),
          not :erts_internal.is_system_process(p) do
        p
      end

    delete(heart, logger, self(), pids)
  end

  defp delete(heart, logger, init, [heart | pids]) do
    delete(heart, logger, init, pids)
  end

  defp delete(heart, logger, init, [logger | pids]) do
    delete(heart, logger, init, pids)
  end

  defp delete(heart, logger, init, [init | pids]) do
    delete(heart, logger, init, pids)
  end

  defp delete(heart, logger, init, [pid | pids]) do
    [pid | delete(heart, logger, init, pids)]
  end

  defp delete(_, _, _, []) do
    []
  end

  defp kill_em([pid | pids]) do
    :erlang.exit(pid, :kill)
    kill_em(pids)
  end

  defp kill_em([]) do
    :ok
  end

  defp kill_all_ports(heart) do
    kill_all_ports(heart, :erlang.ports())
  end

  defp kill_all_ports(heart, [p | ps]) do
    case :erlang.port_info(p, :connected) do
      {:connected, ^heart} ->
        kill_all_ports(heart, ps)

      _ ->
        :erlang.exit(p, :kill)
        kill_all_ports(heart, ps)
    end
  end

  defp kill_all_ports(_, _) do
    :ok
  end

  defp unload(false) do
    do_unload(
      sub(
        [:logger_server | :erlang.pre_loaded()],
        :erlang.loaded()
      )
    )
  end

  defp unload(_) do
    do_unload(
      sub(
        [
          [:heart, :logger_server]
          | :erlang.pre_loaded()
        ],
        :erlang.loaded()
      )
    )
  end

  defp do_unload([m | mods]) do
    try do
      :erlang.purge_module(m)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    try do
      :erlang.delete_module(m)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    try do
      :erlang.purge_module(m)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    do_unload(mods)
  end

  defp do_unload([]) do
    :ok
  end

  defp sub([h | t], l) do
    sub(t, del(h, l))
  end

  defp sub([], l) do
    l
  end

  defp del(item, [item | t]) do
    t
  end

  defp del(item, [h | t]) do
    [h | del(item, t)]
  end

  defp del(_Item, []) do
    []
  end

  defp terminate(pid, kernel, reason) do
    case kernel_pid(pid, kernel) do
      {:ok, name} ->
        sleep(500)
        :erlang.display({'Kernel pid terminated', name, reason})
        crash('Kernel pid terminated', [name, reason])

      _ ->
        false
    end
  end

  defp kernel_pid(pid, [{name, pid} | _]) do
    {:ok, name}
  end

  defp kernel_pid(pid, [_ | t]) do
    kernel_pid(pid, t)
  end

  defp kernel_pid(_, _) do
    false
  end

  defp sleep(t) do
    receive do
    after
      t ->
        :ok
    end
  end

  defp start_prim_loader(init, path0, {pa, pz}) do
    path =
      case path0 do
        false ->
          pa ++ ['.' | pz]

        _ ->
          path0
      end

    case :erl_prim_loader.start() do
      {:ok, pid} ->
        :erl_prim_loader.set_path(path)
        add_to_kernel(init, pid)

      {:error, reason} ->
        :erlang.display({'cannot start loader', reason})
        exit(reason)
    end
  end

  defp add_to_kernel(init, pid) do
    send(init, {self(), :started, {:erl_prim_loader, {:ok, pid}}})

    receive do
      {^init, :ok, ^pid} ->
        :erlang.unlink(pid)
        :ok
    end
  end

  defp do_boot(flags, start) do
    self = self()

    spawn_link(fn ->
      do_boot(self, flags, start)
    end)
  end

  defp do_boot(init, flags, start) do
    :erlang.process_flag(:trap_exit, true)
    root = get_root(flags)
    path = get_flag_list(:path, flags, false)
    {pa, pz} = pathFls = path_flags(flags)
    start_prim_loader(init, bs2ss(path), pathFls)
    bootFile = bootfile(flags, root)
    bootList = get_boot(bootFile, root)
    loadMode = b2a(get_flag(:mode, flags, :interactive))
    deb = b2a(get_flag(:init_debug, flags, false))

    try do
      send(:init__boot__on_load_handler, {:init_debug_flag, deb})
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    bootVars = get_boot_vars(root, flags)
    pathChoice = code_path_choice()

    es =
      r_es(
        init: init,
        debug: deb,
        path: path,
        pa: pa,
        pz: pz,
        path_choice: pathChoice,
        prim_load: true,
        load_mode: loadMode,
        vars: bootVars
      )

    eval_script(bootList, es)

    try do
      :erlang.system_info({:purify, 'Node: ' ++ :erlang.atom_to_list(node())})
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    start_em(start)

    case b2a(get_flag(:profile_boot, flags, false)) do
      false ->
        :ok

      true ->
        debug_profile_format_mfas(debug_profile_mfas())
        debug_profile_stop()
    end

    :ok
  end

  defp get_root(flags) do
    case get_argument(:root, flags) do
      {:ok, [[root]]} ->
        root

      _ ->
        exit(:no_or_multiple_root_variables)
    end
  end

  defp get_boot_vars(root, flags) do
    bootVars = get_boot_vars_1(%{}, flags)
    rootKey = "ROOT"
    %{bootVars | rootKey => root}
  end

  defp get_boot_vars_1(vars, [{:boot_var, [key, value]} | t]) do
    get_boot_vars_1(%{vars | key => value}, t)
  end

  defp get_boot_vars_1(_, [{:boot_var, _} | _]) do
    exit(:invalid_boot_var_argument)
  end

  defp get_boot_vars_1(vars, [_ | t]) do
    get_boot_vars_1(vars, t)
  end

  defp get_boot_vars_1(vars, []) do
    vars
  end

  defp bootfile(flags, root) do
    b2s(get_flag(:boot, flags, root ++ '/bin/start'))
  end

  defp path_flags(flags) do
    pa = append(reverse(get_flag_args(:pa, flags)))
    pz = append(get_flag_args(:pz, flags))
    {bs2ss(pa), bs2ss(pz)}
  end

  defp get_boot(bootFile0, root) do
    bootFile = bootFile0 ++ '.boot'

    case get_boot(bootFile) do
      {:ok, cmdList} ->
        cmdList

      :not_found ->
        bootF = root ++ '/bin/' ++ bootFile

        case get_boot(bootF) do
          {:ok, cmdList} ->
            cmdList

          :not_found ->
            exit({:"cannot get bootfile", :erlang.list_to_atom(bootFile)})

          _ ->
            exit({:"bootfile format error", :erlang.list_to_atom(bootF)})
        end

      _ ->
        exit({:"bootfile format error", :erlang.list_to_atom(bootFile)})
    end
  end

  defp get_boot(bootFile) do
    case :erl_prim_loader.get_file(bootFile) do
      {:ok, bin, _} ->
        case :erlang.binary_to_term(bin) do
          {:script, id, cmdList} when is_list(cmdList) ->
            send(:init, {self(), {:script_id, id}})
            {:ok, cmdList}

          _ ->
            :error
        end

      _ ->
        :not_found
    end
  end

  defp eval_script(
         [{:progress, info} = progress | t],
         r_es(debug: deb) = es
       ) do
    debug(deb, progress)
    send(:init, {self(), :progress, info})
    eval_script(t, es)
  end

  defp eval_script([{:preLoaded, _} | t], r_es() = es) do
    eval_script(t, es)
  end

  defp eval_script(
         [{:path, path} | t],
         r_es(path: false, pa: pa, pz: pz, path_choice: pathChoice, vars: vars) = es
       ) do
    realPath0 = make_path(pa, pz, path, vars)
    realPath = patch_path(realPath0, pathChoice)
    :erl_prim_loader.set_path(realPath)
    eval_script(t, es)
  end

  defp eval_script([{:path, _} | t], r_es() = es) do
    eval_script(t, es)
  end

  defp eval_script(
         [{:kernel_load_completed} | t],
         r_es(load_mode: mode) = es0
       ) do
    es =
      case mode do
        :embedded ->
          es0

        _ ->
          r_es(es0, prim_load: false)
      end

    eval_script(t, es)
  end

  defp eval_script(
         [{:primLoad, mods} | t],
         r_es(init: init, prim_load: primLoad) = es
       )
       when is_list(mods) do
    case primLoad do
      true ->
        load_modules(mods, init)

      false ->
        :ok
    end

    eval_script(t, es)
  end

  defp eval_script(
         [
           {:kernelProcess, server, {mod, fun, args}}
           | t
         ],
         r_es(init: init, debug: deb) = es
       ) do
    debug(deb, {:start, server})
    start_in_kernel(server, mod, fun, args, init)
    eval_script(t, es)
  end

  defp eval_script(
         [{:apply, {mod, fun, args}} = apply | t],
         r_es(debug: deb) = es
       ) do
    debug(deb, apply)
    apply(mod, fun, args)
    eval_script(t, es)
  end

  defp eval_script([], r_es()) do
    :ok
  end

  defp eval_script(what, r_es()) do
    exit({:"unexpected command in bootfile", what})
  end

  defp load_modules(mods0, init) do
    mods =
      for m <- mods0, not :erlang.module_loaded(m) do
        m
      end

    f = prepare_loading_fun()

    case :erl_prim_loader.get_modules(mods, f) do
      {:ok, {prep0, []}} ->
        prep =
          for {_, {:prepared, code, _}} <- prep0 do
            code
          end

        :ok = :erlang.finish_loading(prep)

        loaded =
          for {mod, {_, _, full}} <- prep0 do
            {mod, full}
          end

        send(init, {self(), :loaded, loaded})

        beams =
          for {m, {:on_load, beam, full}} <- prep0 do
            {m, beam, full}
          end

        load_rest(beams, init)

      {:ok, {_, [_ | _] = errors}} ->
        ms =
          for {m, _} <- errors do
            m
          end

        exit({:load_failed, ms})
    end
  end

  defp load_rest([{mod, beam, full} | t], init) do
    do_load_module(mod, beam)
    send(init, {self(), :loaded, [{mod, full}]})
    load_rest(t, init)
  end

  defp load_rest([], _) do
    :ok
  end

  defp prepare_loading_fun() do
    fn mod, fullName, beam ->
      case :erlang.prepare_loading(mod, beam) do
        {:error, _} = error ->
          error

        prepared ->
          case :erlang.has_prepared_code_on_load(prepared) do
            true ->
              {:ok, {:on_load, beam, fullName}}

            false ->
              {:ok, {:prepared, prepared, fullName}}
          end
      end
    end
  end

  defp make_path(pa, pz, path, vars) do
    append([pa, append([fix_path(path, vars), pz])])
  end

  defp fix_path([path | ps], vars) when is_atom(path) do
    [
      add_var(:erlang.atom_to_list(path), vars)
      | fix_path(ps, vars)
    ]
  end

  defp fix_path([path | ps], vars) do
    [add_var(path, vars) | fix_path(ps, vars)]
  end

  defp fix_path(_, _) do
    []
  end

  defp add_var('$' ++ path0, vars) do
    {var, path} = extract_var(path0, [])
    key = :erlang.list_to_binary(var)

    case vars do
      %{^key => value0} ->
        value = b2s(value0)
        value ++ '/' ++ path

      _ ->
        error0 = 'cannot expand $' ++ var ++ ' in bootfile'
        error = :erlang.list_to_atom(error0)
        exit(error)
    end
  end

  defp add_var(path, _) do
    path
  end

  defp extract_var([?/ | path], var) do
    {reverse(var), path}
  end

  defp extract_var([h | t], var) do
    extract_var(t, [h | var])
  end

  defp extract_var([], var) do
    {reverse(var), []}
  end

  defp patch_path(dirs, :strict) do
    dirs
  end

  defp patch_path(dirs, :relaxed) do
    archiveExt = archive_extension()

    for dir <- dirs do
      patch_dir(dir, archiveExt)
    end
  end

  defp patch_dir(orig, archiveExt) do
    case funny_split(orig, ?/) do
      [['nibe', revApp, revArchive] | revTop] ->
        app = reverse(revApp)

        case funny_splitwith(revArchive, ?.) do
          {ext, base} when ext === archiveExt and base === app ->
            top =
              reverse(
                for c <- revTop do
                  reverse(c)
                end
              )

            dir = join(top ++ [app, 'ebin'], '/')
            archive = orig

          _ ->
            top =
              reverse(
                for c <- [revArchive | revTop] do
                  reverse(c)
                end
              )

            archive = join(top ++ [app ++ archiveExt, app, 'ebin'], '/')
            dir = orig
        end

        case :erl_prim_loader.read_file_info(dir) do
          {:ok, r_file_info(type: :directory)} ->
            dir

          _ ->
            case :erl_prim_loader.read_file_info(archive) do
              {:ok, r_file_info(type: :directory)} ->
                archive

              _ ->
                orig
            end
        end

      _ ->
        orig
    end
  end

  defp funny_split(list, sep) do
    funny_split(list, sep, [], [])
  end

  defp funny_split([sep | tail], sep, path, paths) do
    funny_split(tail, sep, [], [path | paths])
  end

  defp funny_split([head | tail], sep, path, paths) do
    funny_split(tail, sep, [head | path], paths)
  end

  defp funny_split([], _Sep, path, paths) do
    [path | paths]
  end

  defp funny_splitwith(list, sep) do
    funny_splitwith(list, sep, [], list)
  end

  defp funny_splitwith([sep | tail], sep, acc, _Orig) do
    {acc, tail}
  end

  defp funny_splitwith([head | tail], sep, acc, orig) do
    funny_splitwith(tail, sep, [head | acc], orig)
  end

  defp funny_splitwith([], _Sep, _Acc, orig) do
    {[], orig}
  end

  defp join([[h1, h2] | t], s) do
    h1 ++ s ++ join([h2 | t], s)
  end

  defp join([h], _) do
    h
  end

  defp start_in_kernel(server, mod, fun, args, init) do
    res = apply(mod, fun, args)
    send(init, {self(), :started, {server, res}})

    receive do
      {^init, :ok, pid} ->
        :erlang.unlink(pid)
        :ok

      {^init, :ignore} ->
        :ignore
    end
  end

  defp start_em([s | tail]) do
    case :erlang.whereis(:user) do
      :undefined ->
        :ok

      p when is_pid(p) ->
        :erlang.group_leader(p, self())
    end

    start_it(s)
    start_em(tail)
  end

  defp start_em([]) do
    :ok
  end

  defp start_it([]) do
    :ok
  end

  defp start_it({:eval, bin}) do
    str = b2s(bin)
    {:ok, ts, _} = :erl_scan.string(str)

    ts1 =
      case reverse(ts) do
        [{:dot, _} | _] ->
          ts

        tsR ->
          reverse([{:dot, :erl_anno.new(1)} | tsR])
      end

    {:ok, expr} = :erl_parse.parse_exprs(ts1)

    {:value, _Value, _Bs} =
      :erl_eval.exprs(
        expr,
        :erl_eval.new_bindings()
      )

    :ok
  end

  defp start_it([_ | _] = mFA) do
    case mFA do
      [m] ->
        m.start()

      [m, f] ->
        apply(m, f, [])

      [[m, f] | args] ->
        apply(m, f, [args])
    end
  end

  defp do_load_module(mod, binCode) do
    case :erlang.load_module(mod, binCode) do
      {:module, ^mod} ->
        :ok

      {:error, :on_load} ->
        send(:init__boot__on_load_handler, {:loaded, mod})
        :ok

      _ ->
        :error
    end
  end

  defp shutdown_timer(flags) do
    case get_flag(:shutdown_time, flags, :infinity) do
      :infinity ->
        self()

      time ->
        case (try do
                :erlang.list_to_integer(:erlang.binary_to_list(time))
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          t when is_integer(t) ->
            pid =
              spawn(fn ->
                timer(t)
              end)

            receive do
              {^pid, :started} ->
                pid
            end

          _ ->
            self()
        end
    end
  end

  defp flush_timout(pid) do
    receive do
      {^pid, :timeout} ->
        true
    after
      0 ->
        true
    end
  end

  defp timer(t) do
    send(:init, {self(), :started})

    receive do
    after
      t ->
        send(:init, {self(), :timeout})
    end
  end

  defp parse_boot_args(args) do
    parse_boot_args(args, [], [], [])
  end

  defp parse_boot_args([b | bs], ss, fs, as) do
    case check(b) do
      :start_extra_arg ->
        {reverse(ss), reverse(fs), :lists.reverse(as, bs)}

      :start_arg ->
        {s, rest} = get_args(bs, [])
        parse_boot_args(rest, [{:s, s} | ss], fs, as)

      :start_arg2 ->
        {s, rest} = get_args(bs, [])
        parse_boot_args(rest, [{:run, s} | ss], fs, as)

      :eval_arg ->
        {expr, rest} = get_args(bs, [])
        parse_boot_args(rest, [{:eval, expr} | ss], fs, as)

      {:flag, a} ->
        {f, rest} = get_args(bs, [])
        fl = {a, f}
        parse_boot_args(rest, ss, [fl | fs], as)

      :arg ->
        parse_boot_args(bs, ss, fs, [b | as])

      :end_args ->
        parse_boot_args(bs, ss, fs, as)
    end
  end

  defp parse_boot_args([], start, flags, args) do
    {reverse(start), reverse(flags), reverse(args)}
  end

  defp check("-extra") do
    :start_extra_arg
  end

  defp check("-s") do
    :start_arg
  end

  defp check("-run") do
    :start_arg2
  end

  defp check("-eval") do
    :eval_arg
  end

  defp check("--") do
    :end_args
  end

  defp check(<<"-", flag::binary>>) do
    {:flag, b2a(flag)}
  end

  defp check(_) do
    :arg
  end

  defp get_args([b | bs], as) do
    case check(b) do
      :start_extra_arg ->
        {reverse(as), [b | bs]}

      :start_arg ->
        {reverse(as), [b | bs]}

      :start_arg2 ->
        {reverse(as), [b | bs]}

      :eval_arg ->
        {reverse(as), [b | bs]}

      :end_args ->
        {reverse(as), bs}

      {:flag, _} ->
        {reverse(as), [b | bs]}

      :arg ->
        get_args(bs, [b | as])
    end
  end

  defp get_args([], as) do
    {reverse(as), []}
  end

  defp update_flag(flag, [{flag, _} | flags], value) do
    [{flag, [value]} | flags]
  end

  defp update_flag(flag, [head | flags], value) do
    [head | update_flag(flag, flags, value)]
  end

  defp update_flag(flag, [], value) do
    [{flag, [value]}]
  end

  defp get_flag(f, flags, default) do
    case :lists.keyfind(f, 1, flags) do
      {^f, []} ->
        true

      {^f, [v]} ->
        v

      {^f, v} ->
        v

      _ ->
        default
    end
  end

  defp get_flag_list(f, flags, default) do
    case :lists.keyfind(f, 1, flags) do
      {^f, [_ | _] = v} ->
        v

      _ ->
        default
    end
  end

  defp get_flag_args(f, flags) do
    get_flag_args(f, flags, [])
  end

  defp get_flag_args(f, [{f, v} | flags], acc) do
    get_flag_args(f, flags, [v | acc])
  end

  defp get_flag_args(f, [_ | flags], acc) do
    get_flag_args(f, flags, acc)
  end

  defp get_flag_args(_, [], acc) do
    reverse(acc)
  end

  defp get_arguments([{f, v} | flags]) do
    [{f, to_strings(v)} | get_arguments(flags)]
  end

  defp get_arguments([]) do
    []
  end

  defp to_strings([h | t]) when is_atom(h) do
    [:erlang.atom_to_list(h) | to_strings(t)]
  end

  defp to_strings([h | t]) when is_binary(h) do
    [b2s(h) | to_strings(t)]
  end

  defp to_strings([]) do
    []
  end

  defp get_argument(arg, flags) do
    case get_argument1(arg, flags) do
      [] ->
        :error

      value ->
        {:ok, value}
    end
  end

  defp get_argument1(arg, [{arg, v} | args]) do
    [to_strings(v) | get_argument1(arg, args)]
  end

  defp get_argument1(arg, [_ | args]) do
    get_argument1(arg, args)
  end

  defp get_argument1(_, []) do
    []
  end

  defp set_argument([{flag, _} | flags], flag, value) do
    [{flag, [value]} | flags]
  end

  defp set_argument([item | flags], flag, value) do
    [item | set_argument(flags, flag, value)]
  end

  defp set_argument([], flag, value) do
    [{flag, [value]}]
  end

  defp append([e]) do
    e
  end

  defp append([h | t]) do
    h ++ append(t)
  end

  defp append([]) do
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

  defp reverse([[a, b] | l]) do
    :lists.reverse(l, [b, a])
  end

  def objfile_extension() do
    '.beam'
  end

  def archive_extension() do
    '.ez'
  end

  def run_on_load_handlers() do
    ref =
      :erlang.monitor(
        :process,
        :init__boot__on_load_handler
      )

    try do
      send(:init__boot__on_load_handler, :run_on_load)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    receive do
      {:DOWN, ^ref, :process, _, :noproc} ->
        :ok

      {:DOWN, ^ref, :process, _, :on_load_done} ->
        :ok

      {:DOWN, ^ref, :process, _, res} ->
        exit(res)
    end
  end

  defp start_on_load_handler_process() do
    :erlang.register(
      :init__boot__on_load_handler,
      spawn(&on_load_handler_init/0)
    )
  end

  defp on_load_handler_init() do
    on_load_loop([], false)
  end

  defp on_load_loop(mods, debug0) do
    receive do
      {:init_debug_flag, debug} ->
        on_load_loop(mods, debug)

      {:loaded, mod} ->
        on_load_loop([mod | mods], debug0)

      :run_on_load ->
        run_on_load_handlers(mods, debug0)
        exit(:on_load_done)
    end
  end

  defp run_on_load_handlers([m | ms], debug) do
    debug(debug, {:running_on_load_handler, m})

    fun = fn ->
      res = :erlang.call_on_load_function(m)
      exit(res)
    end

    {pid, ref} = spawn_monitor(fun)

    receive do
      {:DOWN, ^ref, :process, ^pid, onLoadRes} ->
        keep = onLoadRes === :ok
        :erlang.finish_after_on_load(m, keep)

        case keep do
          false ->
            error = {:on_load_function_failed, m}
            debug(debug, error)
            exit(error)

          true ->
            debug(debug, {:on_load_handler_returned_ok, m})
            run_on_load_handlers(ms, debug)
        end
    end
  end

  defp run_on_load_handlers([], _) do
    :ok
  end

  defp debug_profile_start() do
    _ = :erlang.trace_pattern({:_, :_, :_}, true, [:call_time])
    _ = :erlang.trace_pattern(:on_load, true, [:call_time])
    _ = :erlang.trace(:all, true, [:call])
    :ok
  end

  defp debug_profile_stop() do
    _ = :erlang.trace_pattern({:_, :_, :_}, false, [:call_time])
    _ = :erlang.trace_pattern(:on_load, false, [:call_time])
    _ = :erlang.trace(:all, false, [:call])
    :ok
  end

  defp debug_profile_mfas() do
    _ = :erlang.trace_pattern({:_, :_, :_}, :pause, [:call_time])
    _ = :erlang.trace_pattern(:on_load, :pause, [:call_time])
    mFAs = collect_loaded_mfas() ++ :erlang.system_info(:snifs)
    collect_mfas(mFAs, [])
  end

  defp debug_profile_format_mfas(mFAs0) do
    mFAs = :lists.sort(mFAs0)

    :lists.foreach(
      fn {{us, c}, {m, f, a}} ->
        str = :io_lib.format('~w:~w/~w', [m, f, a])
        :io.format(:standard_error, '~55s - ~6w : ~w us~n', [str, c, us])
      end,
      mFAs
    )

    :ok
  end

  defp collect_loaded_mfas() do
    ms =
      for m <-
            (for mi <- :code.all_loaded() do
               :erlang.element(1, mi)
             end) do
        m
      end

    collect_loaded_mfas(ms, [])
  end

  defp collect_loaded_mfas([], mFAs) do
    mFAs
  end

  defp collect_loaded_mfas([m | ms], mFAs0) do
    mFAs =
      for {f, a} <- m.module_info(:functions) do
        {m, f, a}
      end

    collect_loaded_mfas(ms, mFAs ++ mFAs0)
  end

  defp collect_mfas([], info) do
    info
  end

  defp collect_mfas([mFA | mFAs], info) do
    case :erlang.trace_info(mFA, :call_time) do
      {:call_time, []} ->
        collect_mfas(mFAs, info)

      {:call_time, false} ->
        collect_mfas(mFAs, info)

      {:call_time, data} ->
        case collect_mfa(mFA, data, 0, 0) do
          {{0, _}, _} ->
            collect_mfas(mFAs, info)

          mfaData ->
            collect_mfas(mFAs, [mfaData | info])
        end
    end
  end

  defp collect_mfa(mfa, [], count, time) do
    {{time, count}, mfa}
  end

  defp collect_mfa(mfa, [{_Pid, c, s, us} | data], count, time) do
    collect_mfa(mfa, data, count + c, time + s * 1_000_000 + us)
  end
end
