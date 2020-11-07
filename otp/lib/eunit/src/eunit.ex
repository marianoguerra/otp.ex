defmodule :m_eunit do
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

  def start() do
    start(:eunit_server)
  end

  def start(server) do
    :eunit_server.start(server)
  end

  def stop() do
    stop(:eunit_server)
  end

  def stop(server) do
    :eunit_server.stop(server)
  end

  def watch(target) do
    watch(target, [])
  end

  def watch(target, options) do
    watch(:eunit_server, target, options)
  end

  def watch(server, target, options) do
    :eunit_server.watch(server, target, options)
  end

  def watch_path(target) do
    watch_path(target, [])
  end

  def watch_path(target, options) do
    watch_path(:eunit_server, target, options)
  end

  def watch_path(server, target, options) do
    :eunit_server.watch_path(server, target, options)
  end

  def watch_regexp(target) do
    watch_regexp(target, [])
  end

  def watch_regexp(target, options) do
    watch_regexp(:eunit_server, target, options)
  end

  def watch_regexp(server, target, options) do
    :eunit_server.watch_regexp(server, target, options)
  end

  def watch_app(name) do
    watch_app(name, [])
  end

  def watch_app(name, options) do
    watch_app(:eunit_server, name, options)
  end

  def watch_app(server, name, options) do
    case :code.lib_dir(name) do
      path when is_list(path) ->
        watch_path(server, :filename.join(path, 'ebin'), options)

      _ ->
        :error
    end
  end

  def test(tests) do
    test(tests, [])
  end

  def test(tests, options) do
    test(:eunit_server, tests, all_options(options))
  end

  def test(server, tests, options) do
    listeners = listeners(options)
    serial = :eunit_serial.start(listeners)

    case :eunit_server.start_test(server, serial, tests, options) do
      {:ok, reference} ->
        test_run(reference, listeners)

      {:error, r} ->
        {:error, r}
    end
  end

  defp test_run(reference, listeners) do
    receive do
      {:start, ^reference} ->
        cast(listeners, {:start, reference})
    end

    receive do
      {:done, ^reference} ->
        cast(listeners, {:stop, reference, self()})
        wait_until_listeners_have_terminated(listeners)

        receive do
          {:result, ^reference, result} ->
            result
        end
    end
  end

  defp cast([p | ps], msg) do
    send(p, msg)
    cast(ps, msg)
  end

  defp cast([], _Msg) do
    :ok
  end

  defp wait_until_listeners_have_terminated([p | ps]) do
    mRef = :erlang.monitor(:process, p)

    receive do
      {:DOWN, ^mRef, :process, ^p, _} ->
        wait_until_listeners_have_terminated(ps)
    end
  end

  defp wait_until_listeners_have_terminated([]) do
    :ok
  end

  def submit(t) do
    submit(t, [])
  end

  def submit(t, options) do
    submit(:eunit_server, t, options)
  end

  def submit(server, t, options) do
    dummy = spawn(&devnull/0)
    :eunit_server.start_test(server, dummy, t, options)
  end

  defp listeners(options) do
    ls = [
      {:eunit_tty, options}
      | :proplists.get_all_values(:report, options)
    ]

    ps = start_listeners(ls)

    case :proplists.get_value(:event_log, options) do
      :undefined ->
        ps

      x ->
        logFile =
          cond do
            is_list(x) ->
              x

            true ->
              'eunit-events.log'
          end

        [
          spawn_link(fn ->
            event_logger(logFile)
          end)
          | ps
        ]
    end
  end

  defp start_listeners([p | ps]) when is_pid(p) or is_atom(p) do
    [p | start_listeners(ps)]
  end

  defp start_listeners([{mod, opts} | ps]) when is_atom(mod) do
    [mod.start(opts) | start_listeners(ps)]
  end

  defp start_listeners([]) do
    []
  end

  defp event_logger(logFile) do
    case :file.open(logFile, [:write]) do
      {:ok, fD} ->
        receive do
          {:start, reference} ->
            event_logger_loop(reference, fD)
        end

      error ->
        exit(error)
    end
  end

  defp event_logger_loop(reference, fD) do
    receive do
      {:status, _Id, _Info} = msg ->
        :io.fwrite(fD, '~tp.\n', [msg])
        event_logger_loop(reference, fD)

      {:stop, ^reference, _ReplyTo} ->
        :file.close(fD)
        exit(:normal)
    end
  end

  defp devnull() do
    receive do
      _ ->
        devnull()
    end
  end

  defp all_options(opts) do
    try do
      :os.getenv('EUNIT')
    catch
      _, _ ->
        opts
    else
      false ->
        opts

      s ->
        {:ok, ts, _} = :erl_scan.string(s)
        {:ok, v} = :erl_parse.parse_term(ts ++ [{:dot, :erl_anno.new(1)}])

        cond do
          is_list(v) ->
            opts ++ v

          true ->
            opts ++ [v]
        end
    end
  end
end
