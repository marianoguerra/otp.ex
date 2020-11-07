defmodule :m_reltool do
  use Bitwise
  require Record

  Record.defrecord(:r_common, :common,
    sys_debug: :undefined,
    wx_debug: :undefined,
    trap_exit: :undefined
  )

  Record.defrecord(:r_mod, :mod,
    name: :undefined,
    app_name: :undefined,
    incl_cond: :undefined,
    debug_info: :undefined,
    is_app_mod: :undefined,
    is_ebin_mod: :undefined,
    uses_mods: :undefined,
    exists: :undefined,
    status: :ok,
    used_by_mods: [],
    is_pre_included: :undefined,
    is_included: :undefined
  )

  Record.defrecord(:r_app_info, :app_info,
    description: '',
    id: '',
    vsn: '',
    modules: [],
    maxP: :infinity,
    maxT: :infinity,
    registered: [],
    incl_apps: [],
    applications: [],
    env: [],
    mod: :undefined,
    start_phases: :undefined,
    runtime_dependencies: []
  )

  Record.defrecord(:r_regexp, :regexp,
    source: :undefined,
    compiled: :undefined
  )

  Record.defrecord(:r_app, :app,
    name: :undefined,
    is_escript: :undefined,
    use_selected_vsn: :undefined,
    active_dir: :undefined,
    sorted_dirs: :undefined,
    vsn: :undefined,
    label: :undefined,
    info: :undefined,
    mods: :undefined,
    mod_cond: :undefined,
    incl_cond: :undefined,
    debug_info: :undefined,
    app_file: :undefined,
    app_type: :undefined,
    incl_app_filters: :undefined,
    excl_app_filters: :undefined,
    incl_archive_filters: :undefined,
    excl_archive_filters: :undefined,
    archive_opts: :undefined,
    status: :undefined,
    uses_mods: :undefined,
    used_by_mods: :undefined,
    uses_apps: :undefined,
    used_by_apps: :undefined,
    is_pre_included: :undefined,
    is_included: :undefined,
    rels: :undefined
  )

  Record.defrecord(:r_rel_app, :rel_app,
    name: :undefined,
    app_type: :undefined,
    incl_apps: :undefined
  )

  Record.defrecord(:r_rel, :rel,
    name: :undefined,
    vsn: :undefined,
    rel_apps: :undefined,
    load_dot_erlang: true
  )

  Record.defrecord(:r_sys, :sys,
    root_dir: :undefined,
    lib_dirs: :undefined,
    escripts: :undefined,
    mod_cond: :undefined,
    incl_cond: :undefined,
    apps: :undefined,
    boot_rel: :undefined,
    rels: :undefined,
    emu_name: :undefined,
    profile: :undefined,
    excl_lib: :undefined,
    incl_sys_filters: :undefined,
    excl_sys_filters: :undefined,
    incl_app_filters: :undefined,
    excl_app_filters: :undefined,
    incl_archive_filters: :undefined,
    excl_archive_filters: :undefined,
    archive_opts: :undefined,
    relocatable: :undefined,
    rel_app_type: :undefined,
    embedded_app_type: :undefined,
    app_file: :undefined,
    debug_info: :undefined
  )

  def start() do
    start([])
  end

  def start(options) when is_list(options) do
    case start_link(options) do
      {:ok, winPid} = oK ->
        :erlang.unlink(winPid)
        oK

      {:error, _Reason} = error ->
        error
    end
  end

  def debug() do
    start([{:wx_debug, 2}])
  end

  def start_link(options) when is_list(options) do
    case :reltool_sys_win.start_link(options) do
      {:ok, _WinPid} = oK ->
        oK

      {:error, reason} ->
        {:error, reason}
    end
  end

  def start_server(options) do
    case :reltool_server.start_link(options) do
      {:ok, serverPid, _Common, _Sys} ->
        {:ok, serverPid}

      {:error, reason} ->
        {:error, reason}
    end
  end

  def get_server(winPid) do
    case :reltool_sys_win.get_server(winPid) do
      {:ok, _ServerPid} = oK ->
        oK

      {:error, reason} ->
        {:error, :lists.flatten(:io_lib.format('~tp', [reason]))}
    end
  end

  def stop(pid) when is_pid(pid) do
    ref = :erlang.monitor(:process, pid)
    :erlang.unlink(pid)
    :erlang.exit(pid, :shutdown)

    receive do
      {:DOWN, ^ref, _, _, :shutdown} ->
        :ok

      {:DOWN, ^ref, _, _, reason} ->
        {:error, :lists.flatten(:io_lib.format('~tp', [reason]))}
    end
  end

  defp eval_server(pid, _DisplayWarnings, fun) when is_pid(pid) do
    fun.(pid)
  end

  defp eval_server(options, displayWarnings, fun)
       when is_list(options) do
    trapExit = :erlang.process_flag(:trap_exit, true)

    res =
      case start_server(options) do
        {:ok, pid} ->
          apply_fun(pid, displayWarnings, fun)

        {:error, _Reason} = error ->
          error
      end

    :erlang.process_flag(:trap_exit, trapExit)
    res
  end

  defp apply_fun(pid, false, fun) do
    res = fun.(pid)
    stop(pid)
    res
  end

  defp apply_fun(pid, true, fun) do
    case get_status(pid) do
      {:ok, warnings} ->
        for w <- warnings do
          :io.format('~w: ~ts\n', [:reltool, w])
        end

        apply_fun(pid, false, fun)

      {:error, _Reason} = error ->
        stop(pid)
        error
    end
  end

  def get_status(pidOrOptions)
      when is_pid(pidOrOptions) or
             is_list(pidOrOptions) do
    eval_server(pidOrOptions, false, fn pid ->
      :reltool_server.get_status(pid)
    end)
  end

  def get_config(pidOrOption) do
    get_config(pidOrOption, false, false)
  end

  def get_config(pidOrOptions, inclDef, inclDeriv)
      when is_pid(pidOrOptions) or is_list(pidOrOptions) do
    eval_server(pidOrOptions, true, fn pid ->
      :reltool_server.get_config(pid, inclDef, inclDeriv)
    end)
  end

  def get_rel(pidOrOptions, relName)
      when is_pid(pidOrOptions) or is_list(pidOrOptions) do
    eval_server(pidOrOptions, true, fn pid ->
      :reltool_server.get_rel(pid, relName)
    end)
  end

  def get_script(pidOrOptions, relName)
      when is_pid(pidOrOptions) or is_list(pidOrOptions) do
    eval_server(pidOrOptions, true, fn pid ->
      :reltool_server.get_script(pid, relName)
    end)
  end

  def create_target(pidOrOptions, targetDir)
      when is_pid(pidOrOptions) or is_list(pidOrOptions) do
    eval_server(pidOrOptions, true, fn pid ->
      :reltool_server.gen_target(pid, targetDir)
    end)
  end

  def get_target_spec(pidOrOptions)
      when is_pid(pidOrOptions) or
             is_list(pidOrOptions) do
    eval_server(pidOrOptions, true, fn pid ->
      :reltool_server.gen_spec(pid)
    end)
  end

  def eval_target_spec(spec, sourceDir, targetDir)
      when is_list(sourceDir) and is_list(targetDir) do
    :reltool_target.eval_spec(spec, sourceDir, targetDir)
  end

  def install(relName, targetDir) do
    :reltool_target.install(relName, targetDir)
  end
end
