defmodule :m_gen do
  use Bitwise

  def start(genMod, linkP, name, mod, args, options) do
    case where(name) do
      :undefined ->
        do_spawn(genMod, linkP, name, mod, args, options)

      pid ->
        {:error, {:already_started, pid}}
    end
  end

  def start(genMod, linkP, mod, args, options) do
    do_spawn(genMod, linkP, mod, args, options)
  end

  defp do_spawn(genMod, :link, mod, args, options) do
    time = timeout(options)

    :proc_lib.start_link(
      :gen,
      :init_it,
      [genMod, self(), self(), mod, args, options],
      time,
      spawn_opts(options)
    )
  end

  defp do_spawn(genMod, :monitor, mod, args, options) do
    time = timeout(options)

    ret =
      :proc_lib.start_monitor(
        :gen,
        :init_it,
        [genMod, self(), self(), mod, args, options],
        time,
        spawn_opts(options)
      )

    monitor_return(ret)
  end

  defp do_spawn(genMod, _, mod, args, options) do
    time = timeout(options)

    :proc_lib.start(
      :gen,
      :init_it,
      [genMod, self(), :self, mod, args, options],
      time,
      spawn_opts(options)
    )
  end

  defp do_spawn(genMod, :link, name, mod, args, options) do
    time = timeout(options)

    :proc_lib.start_link(
      :gen,
      :init_it,
      [genMod, self(), self(), name, mod, args, options],
      time,
      spawn_opts(options)
    )
  end

  defp do_spawn(genMod, :monitor, name, mod, args, options) do
    time = timeout(options)

    ret =
      :proc_lib.start_monitor(
        :gen,
        :init_it,
        [genMod, self(), self(), name, mod, args, options],
        time,
        spawn_opts(options)
      )

    monitor_return(ret)
  end

  defp do_spawn(genMod, _, name, mod, args, options) do
    time = timeout(options)

    :proc_lib.start(
      :gen,
      :init_it,
      [genMod, self(), :self, name, mod, args, options],
      time,
      spawn_opts(options)
    )
  end

  defp monitor_return({{:ok, pid}, mon})
       when is_pid(pid) and
              is_reference(mon) do
    {:ok, {pid, mon}}
  end

  defp monitor_return({error, mon}) when is_reference(mon) do
    receive do
      {:DOWN, ^mon, :process, _Pid, _Reason} ->
        :ok
    end

    error
  end

  def init_it(genMod, starter, parent, mod, args, options) do
    init_it2(genMod, starter, parent, self(), mod, args, options)
  end

  def init_it(genMod, starter, parent, name, mod, args, options) do
    case register_name(name) do
      true ->
        init_it2(genMod, starter, parent, name, mod, args, options)

      {false, pid} ->
        :proc_lib.init_ack(
          starter,
          {:error, {:already_started, pid}}
        )
    end
  end

  defp init_it2(genMod, starter, parent, name, mod, args, options) do
    genMod.init_it(starter, parent, name, mod, args, options)
  end

  def call(process, label, request) do
    call(process, label, request, 5000)
  end

  def call(process, label, request, timeout)
      when (is_pid(process) and
              timeout === :infinity) or (is_integer(timeout) and timeout >= 0) do
    do_call(process, label, request, timeout)
  end

  def call(process, label, request, timeout)
      when timeout === :infinity or
             (is_integer(timeout) and timeout >= 0) do
    fun = fn pid ->
      do_call(pid, label, request, timeout)
    end

    do_for_proc(process, fun)
  end

  defp do_call(process, label, request, timeout)
       when is_atom(process) === false do
    mref = :erlang.monitor(:process, process)
    :erlang.send(process, {label, {self(), mref}, request}, [:noconnect])

    receive do
      {^mref, reply} ->
        :erlang.demonitor(mref, [:flush])
        {:ok, reply}

      {:DOWN, ^mref, _, _, :noconnection} ->
        node = get_node(process)
        exit({:nodedown, node})

      {:DOWN, ^mref, _, _, reason} ->
        exit(reason)
    after
      timeout ->
        :erlang.demonitor(mref, [:flush])
        exit(:timeout)
    end
  end

  defp get_node(process) do
    case process do
      {_S, n} when is_atom(n) ->
        n

      _ when is_pid(process) ->
        node(process)
    end
  end

  def send_request(process, label, request) when is_pid(process) do
    do_send_request(process, label, request)
  end

  def send_request(process, label, request) do
    fun = fn pid ->
      do_send_request(pid, label, request)
    end

    try do
      do_for_proc(process, fun)
    catch
      :exit, reason ->
        mref = :erlang.make_ref()
        send(self(), {:DOWN, mref, :process, process, reason})
        mref
    end
  end

  defp do_send_request(process, label, request) do
    mref = :erlang.monitor(:process, process)
    :erlang.send(process, {label, {self(), {:"$gen_request_id", mref}}, request}, [:noconnect])
    mref
  end

  def wait_response(mref, timeout) when is_reference(mref) do
    receive do
      {{:"$gen_request_id", ^mref}, reply} ->
        :erlang.demonitor(mref, [:flush])
        {:reply, reply}

      {:DOWN, ^mref, _, object, reason} ->
        {:error, {reason, object}}
    after
      timeout ->
        :timeout
    end
  end

  def check_response(msg, mref) when is_reference(mref) do
    case msg do
      {{:"$gen_request_id", ^mref}, reply} ->
        :erlang.demonitor(mref, [:flush])
        {:reply, reply}

      {:DOWN, ^mref, _, object, reason} ->
        {:error, {reason, object}}

      _ ->
        :no_reply
    end
  end

  def reply({to, tag}, reply) do
    msg = {tag, reply}

    try do
      send(to, msg)
    catch
      _, _ ->
        msg
    end
  end

  def stop(process) do
    stop(process, :normal, :infinity)
  end

  def stop(process, reason, timeout)
      when timeout === :infinity or
             (is_integer(timeout) and timeout >= 0) do
    fun = fn pid ->
      :proc_lib.stop(pid, reason, timeout)
    end

    do_for_proc(process, fun)
  end

  defp do_for_proc(pid, fun) when is_pid(pid) do
    fun.(pid)
  end

  defp do_for_proc(name, fun) when is_atom(name) do
    case :erlang.whereis(name) do
      pid when is_pid(pid) ->
        fun.(pid)

      :undefined ->
        exit(:noproc)
    end
  end

  defp do_for_proc(process, fun)
       when (tuple_size(process) == 2 and
               :erlang.element(
                 1,
                 process
               ) == :global) or
              (tuple_size(process) == 3 and
                 :erlang.element(
                   1,
                   process
                 ) == :via) do
    case where(process) do
      pid when is_pid(pid) ->
        node = node(pid)

        try do
          fun.(pid)
        catch
          :exit, {:nodedown, ^node} ->
            exit(:noproc)
        end

      :undefined ->
        exit(:noproc)
    end
  end

  defp do_for_proc({name, node}, fun) when node === node() do
    do_for_proc(name, fun)
  end

  defp do_for_proc({_Name, node} = process, fun)
       when is_atom(node) do
    cond do
      node() === :nonode@nohost ->
        exit({:nodedown, node})

      true ->
        fun.(process)
    end
  end

  defp where({:global, name}) do
    :global.whereis_name(name)
  end

  defp where({:via, module, name}) do
    module.whereis_name(name)
  end

  defp where({:local, name}) do
    :erlang.whereis(name)
  end

  defp register_name({:local, name} = lN) do
    try do
      :erlang.register(name, self())
    catch
      :error, _ ->
        {false, where(lN)}
    else
      true ->
        true
    end
  end

  defp register_name({:global, name} = gN) do
    case :global.register_name(name, self()) do
      :yes ->
        true

      :no ->
        {false, where(gN)}
    end
  end

  defp register_name({:via, module, name} = gN) do
    case module.register_name(name, self()) do
      :yes ->
        true

      :no ->
        {false, where(gN)}
    end
  end

  def name({:local, name}) do
    name
  end

  def name({:global, name}) do
    name
  end

  def name({:via, _, name}) do
    name
  end

  def name(pid) when is_pid(pid) do
    pid
  end

  def unregister_name({:local, name}) do
    try do
      :erlang.unregister(name)
    catch
      _, _ ->
        :ok
    else
      _ ->
        :ok
    end
  end

  def unregister_name({:global, name}) do
    _ = :global.unregister_name(name)
    :ok
  end

  def unregister_name({:via, mod, name}) do
    _ = mod.unregister_name(name)
    :ok
  end

  def unregister_name(pid) when is_pid(pid) do
    :ok
  end

  def get_proc_name(pid) when is_pid(pid) do
    pid
  end

  def get_proc_name({:local, name}) do
    case :erlang.process_info(self(), :registered_name) do
      {:registered_name, ^name} ->
        name

      {:registered_name, _Name} ->
        exit(:process_not_registered)

      [] ->
        exit(:process_not_registered)
    end
  end

  def get_proc_name({:global, name}) do
    case :global.whereis_name(name) do
      :undefined ->
        exit(:process_not_registered_globally)

      pid when pid === self() ->
        name

      _Pid ->
        exit(:process_not_registered_globally)
    end
  end

  def get_proc_name({:via, mod, name}) do
    case mod.whereis_name(name) do
      :undefined ->
        exit({:process_not_registered_via, mod})

      pid when pid === self() ->
        name

      _Pid ->
        exit({:process_not_registered_via, mod})
    end
  end

  def get_parent() do
    case :erlang.get(:"$ancestors") do
      [parent | _] when is_pid(parent) ->
        parent

      [parent | _] when is_atom(parent) ->
        name_to_pid(parent)

      _ ->
        exit(:process_was_not_started_by_proc_lib)
    end
  end

  defp name_to_pid(name) do
    case :erlang.whereis(name) do
      :undefined ->
        case :global.whereis_name(name) do
          :undefined ->
            exit(:could_not_find_registered_name)

          pid ->
            pid
        end

      pid ->
        pid
    end
  end

  defp timeout(options) do
    case :lists.keyfind(:timeout, 1, options) do
      {_, time} ->
        time

      false ->
        :infinity
    end
  end

  defp spawn_opts(options) do
    case :lists.keyfind(:spawn_opt, 1, options) do
      {_, opts} ->
        opts

      false ->
        []
    end
  end

  def hibernate_after(options) do
    case :lists.keyfind(:hibernate_after, 1, options) do
      {_, hibernateAfterTimeout} ->
        hibernateAfterTimeout

      false ->
        :infinity
    end
  end

  def debug_options(name, opts) do
    case :lists.keyfind(:debug, 1, opts) do
      {_, options} ->
        try do
          :sys.debug_options(options)
        catch
          _, _ ->
            :error_logger.format('~tp: ignoring erroneous debug options - ~tp~n', [name, options])
            []
        end

      false ->
        []
    end
  end

  def format_status_header(tagLine, pid) when is_pid(pid) do
    :lists.concat([tagLine, ' ', :erlang.pid_to_list(pid)])
  end

  def format_status_header(tagLine, regName) when is_atom(regName) do
    :lists.concat([tagLine, ' ', regName])
  end

  def format_status_header(tagLine, name) do
    {tagLine, name}
  end
end
