defmodule :m_eunit_server do
  use Bitwise
  require Record

  Record.defrecord(:r_test, :test,
    f: :undefined,
    desc: :undefined,
    timeout: :undefined,
    location: :undefined,
    line: 0
  )

  Record.defrecord(:r_group, :group,
    desc: :undefined,
    order: :undefined,
    timeout: :undefined,
    context: :undefined,
    spawn: :undefined,
    tests: :undefined
  )

  Record.defrecord(:r_context, :context, setup: :undefined, cleanup: :undefined, process: :local)

  def start(server) when is_atom(server) do
    ensure_started(server)
  end

  def stop(server) do
    command(server, :stop)
  end

  Record.defrecord(:r_job, :job, super: :undefined, test: :undefined, options: :undefined)

  def start_test(server, super, t, options) do
    command(
      server,
      {:start, r_job(super: super, test: t, options: options)}
    )
  end

  def watch(server, module, opts) when is_atom(module) do
    command(server, {:watch, {:module, module}, opts})
  end

  def watch_path(server, path, opts) do
    command(
      server,
      {:watch, {:path, :filename.flatten(path)}, opts}
    )
  end

  def watch_regexp(server, regex, opts) do
    case :re.compile(regex, [:anchored]) do
      {:ok, r} ->
        command(server, {:watch, {:regexp, r}, opts})

      {:error, _} = error ->
        error
    end
  end

  defp command(server, cmd) do
    cond do
      is_atom(server) and cmd != :stop ->
        ensure_started(server)

      true ->
        :ok
    end

    cond do
      is_pid(server) ->
        command_1(server, cmd)

      true ->
        case :erlang.whereis(server) do
          :undefined ->
            {:error, :server_down}

          pid ->
            command_1(pid, cmd)
        end
    end
  end

  defp command_1(pid, cmd) when is_pid(pid) do
    send(pid, {:command, self(), cmd})
    command_wait(pid, 1000, :undefined)
  end

  defp command_wait(pid, timeout, monitor) do
    receive do
      {^pid, result} ->
        result

      {:DOWN, ^monitor, :process, ^pid, _R} ->
        {:error, :server_down}
    after
      timeout ->
        command_wait(pid, :infinity, :erlang.monitor(:process, pid))
    end
  end

  defp ensure_started(name) do
    ensure_started(name, 5)
  end

  defp ensure_started(name, n) when n > 0 do
    case :erlang.whereis(name) do
      :undefined ->
        parent = self()

        pid =
          spawn(fn ->
            server_start(name, parent)
          end)

        receive do
          {^pid, :ok} ->
            pid

          {^pid, :error} ->
            receive do
            after
              200 ->
                ensure_started(name, n - 1)
            end
        end

      pid ->
        pid
    end
  end

  defp ensure_started(_, _) do
    throw(:no_server)
  end

  defp server_start(:undefined = name, parent) do
    server_start_1(name, parent)
  end

  defp server_start(name, parent) do
    try do
      :erlang.register(name, self())
    catch
      _, _ ->
        send(parent, {self(), :error})
        exit(:error)
    else
      true ->
        server_start_1(name, parent)
    end
  end

  defp server_start_1(name, parent) do
    send(parent, {self(), :ok})
    server_init(name)
  end

  Record.defrecord(:r_state, :state,
    name: :undefined,
    stopped: :undefined,
    jobs: :undefined,
    queue: :undefined,
    auto_test: :undefined,
    modules: :undefined,
    paths: :undefined,
    regexps: :undefined
  )

  defp server_init(name) do
    server(
      r_state(
        name: name,
        stopped: false,
        jobs: :dict.new(),
        queue: :queue.new(),
        auto_test: :queue.new(),
        modules: :sets.new(),
        paths: :sets.new(),
        regexps: :sets.new()
      )
    )
  end

  defp server(st) do
    server_check_exit(st)
    :eunit_server.main(st)
  end

  def main(st) do
    receive do
      {:done, :auto_test, _Pid} ->
        server(auto_test_done(st))

      {:done, reference, _Pid} ->
        server(handle_done(reference, st))

      {:command, from, _Cmd} when r_state(st, :stopped) ->
        send(from, {self(), :stopped})

      {:command, from, cmd} ->
        server_command(from, cmd, st)

      {:code_monitor, {:loaded, m, _Time}} ->
        case is_watched(m, st) do
          true ->
            server(new_auto_test(self(), m, st))

          false ->
            server(st)
        end
    end
  end

  defp server_check_exit(st) do
    case :dict.size(r_state(st, :jobs)) do
      0 when r_state(st, :stopped) ->
        exit(:normal)

      _ ->
        :ok
    end
  end

  defp server_command(from, {:start, job}, st) do
    reference = make_ref()

    st1 =
      case :proplists.get_bool(
             :enqueue,
             r_job(job, :options)
           ) do
        true ->
          enqueue(job, from, reference, st)

        false ->
          start_job(job, from, reference, st)
      end

    server_command_reply(from, {:ok, reference})
    server(st1)
  end

  defp server_command(from, :stop, st) do
    server_command_reply(from, {:error, :stopped})

    try do
      :erlang.unregister(r_state(st, :name))
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    server(r_state(st, stopped: true))
  end

  defp server_command(from, {:watch, target, _Opts}, st) do
    st1 = add_watch(target, st)
    server_command_reply(from, :ok)
    server(st1)
  end

  defp server_command(from, {:forget, target}, st) do
    st1 = delete_watch(target, st)
    server_command_reply(from, :ok)
    server(st1)
  end

  defp server_command(from, cmd, st) do
    server_command_reply(
      from,
      {:error, {:unknown_command, cmd}}
    )

    server(st)
  end

  defp server_command_reply(from, result) do
    send(from, {self(), result})
  end

  defp enqueue(job, from, reference, st) do
    case :dict.size(r_state(st, :jobs)) do
      0 ->
        start_job(job, from, reference, st)

      _ ->
        r_state(st,
          queue:
            :queue.in(
              {job, from, reference},
              r_state(st, :queue)
            )
        )
    end
  end

  defp dequeue(st) do
    case :queue.out(r_state(st, :queue)) do
      {:empty, _} ->
        st

      {{:value, {job, from, reference}}, queue} ->
        start_job(job, from, reference, r_state(st, queue: queue))
    end
  end

  defp start_job(job, from, reference, st) do
    send(from, {:start, reference})
    order = :proplists.get_value(:order, r_job(job, :options), :inorder)
    :eunit_proc.start(r_job(job, :test), order, r_job(job, :super), reference)
    r_state(st, jobs: :dict.store(reference, from, r_state(st, :jobs)))
  end

  defp handle_done(reference, st) do
    case :dict.find(reference, r_state(st, :jobs)) do
      {:ok, from} ->
        send(from, {:done, reference})

        dequeue(
          r_state(st,
            jobs:
              :dict.erase(
                reference,
                r_state(st, :jobs)
              )
          )
        )

      :error ->
        st
    end
  end

  defp add_watch({:module, m}, st) do
    r_state(st, modules: :sets.add_element(m, r_state(st, :modules)))
  end

  defp add_watch({:path, p}, st) do
    r_state(st, paths: :sets.add_element(p, r_state(st, :paths)))
  end

  defp add_watch({:regexp, r}, st) do
    r_state(st, regexps: :sets.add_element(r, r_state(st, :regexps)))
  end

  defp delete_watch({:module, m}, st) do
    r_state(st, modules: :sets.del_element(m, r_state(st, :modules)))
  end

  defp delete_watch({:path, p}, st) do
    r_state(st, paths: :sets.del_element(p, r_state(st, :paths)))
  end

  defp delete_watch({:regexp, r}, st) do
    r_state(st, regexps: :sets.del_element(r, r_state(st, :regexps)))
  end

  defp is_watched(m, st) when is_atom(m) do
    :sets.is_element(
      m,
      r_state(st, :modules)
    ) or is_watched(:code.which(m), st)
  end

  defp is_watched(path, st) do
    :sets.is_element(
      :filename.dirname(path),
      r_state(st, :paths)
    ) or
      match_any(
        :sets.to_list(r_state(st, :regexps)),
        path
      )
  end

  defp match_any([r | rs], str) do
    case :re.run(str, r, [{:capture, :none}]) do
      :match ->
        true

      _ ->
        match_any(rs, str)
    end
  end

  defp match_any([], _Str) do
    false
  end

  defp new_auto_test(server, m, st) do
    case :queue.is_empty(r_state(st, :auto_test)) do
      true ->
        start_auto_test(server, m)

      false ->
        :ok
    end

    r_state(st,
      auto_test:
        :queue.in(
          {server, m},
          r_state(st, :auto_test)
        )
    )
  end

  defp auto_test_done(st) do
    {_, queue} = :queue.out(r_state(st, :auto_test))

    case :queue.out(queue) do
      {{:value, {server, m}}, _} ->
        start_auto_test(server, m)

      {:empty, _} ->
        :ok
    end

    r_state(st, auto_test: queue)
  end

  defp start_auto_test(server, m) do
    spawn(fn ->
      auto_super(server, m)
    end)
  end

  defp auto_super(server, m) do
    :erlang.process_flag(:trap_exit, true)

    receive do
    after
      333 ->
        :ok
    end

    :erlang.group_leader(:erlang.whereis(:user), self())

    pid =
      spawn_link(fn ->
        auto_proc(server, m)
      end)

    receive do
      {:EXIT, ^pid, _} ->
        :ok
    after
      60000 ->
        :erlang.exit(pid, :kill)
        :io.put_chars('\n== EUnit: automatic test was aborted ==\n')
        :io.put_chars('\n> ')
    end

    send(server, {:done, :auto_test, self()})
  end

  defp auto_proc(server, m) do
    :io.fwrite('\n== EUnit: testing module ~w ==\n', [m])
    :eunit.test(server, m, [:enqueue])
    :io.put_chars('\n-> ')
  end
end
