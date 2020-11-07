defmodule :m_eunit_proc do
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

  Record.defrecord(:r_procstate, :procstate,
    ref: :undefined,
    id: :undefined,
    super: :undefined,
    insulator: :undefined,
    parent: :undefined,
    order: :undefined
  )

  def start(tests, order, super, reference)
      when is_pid(super) and is_reference(reference) do
    st = r_procstate(ref: reference, id: [], super: super, order: order)
    spawn_group(:local, r_group(tests: tests), st)
  end

  def get_output() do
    send(:erlang.group_leader(), {:get_output, self()})

    receive do
      {:output, output} ->
        output
    after
      100 ->
        abort_task(:get_output)
    end
  end

  defp message_super(id, info, st) do
    send(r_procstate(st, :super), {:status, id, info})
  end

  defp start_task(type, fun, st0) do
    st = r_procstate(st0, parent: self())

    f = fn ->
      insulator_process(type, fun, st)
    end

    case type do
      :local ->
        spawn_link(f)

      {:remote, node} ->
        pid = :erlang.spawn_link(node, f)
        reference = r_procstate(st, :ref)
        monitor = :erlang.monitor(:process, pid)

        receive do
          {:ok, ^reference, ^pid} ->
            pid

          {:DOWN, ^monitor, :process, ^pid, reason} ->
            msg = {:startup, reason}
            message_super(r_procstate(st, :id), {:cancel, msg}, st)
            send(self(), {:done, reference, pid})
        end

        :erlang.demonitor(monitor, [:flush])
        pid
    end
  end

  defp insulator_process(type, fun, st0) do
    :erlang.process_flag(:trap_exit, true)
    parent = r_procstate(st0, :parent)

    cond do
      type === :local ->
        :ok

      true ->
        send(parent, {:ok, r_procstate(st0, :ref), self()})
    end

    st = r_procstate(st0, insulator: self())

    child =
      spawn_link(fn ->
        child_process(fun.(st), st)
      end)

    insulator_wait(child, parent, [], st)
  end

  defp insulator_wait(child, parent, buf, st) do
    receive do
      {:child, ^child, id, {:begin, type, data}} ->
        message_super(id, {:progress, :begin, {type, data}}, st)
        insulator_wait(child, parent, [[] | buf], st)

      {:child, ^child, id, {:end, status, time}} ->
        data = [{:time, time}, {:output, :lists.reverse(hd(buf))}]
        message_super(id, {:progress, :end, {status, data}}, st)
        insulator_wait(child, parent, tl(buf), st)

      {:child, ^child, id, {:skipped, reason}} ->
        message_super(id, {:cancel, {:abort, reason}}, st)
        insulator_wait(child, parent, buf, st)

      {:child, ^child, id, {:abort, cause}} ->
        exit_messages(id, {:abort, cause}, st)
        terminate_insulator(st)

      {:io_request, ^child, replyAs, req} ->
        buf1 = io_request(child, replyAs, req, hd(buf))
        insulator_wait(child, parent, [buf1 | tl(buf)], st)

      {:io_request, from, replyAs, req} when is_pid(from) ->
        io_request(from, replyAs, req, [])
        insulator_wait(child, parent, buf, st)

      {:timeout, ^child, id} ->
        exit_messages(id, :timeout, st)
        kill_task(child, st)

      {:EXIT, ^child, :normal} ->
        terminate_insulator(st)

      {:EXIT, ^child, reason} ->
        exit_messages(r_procstate(st, :id), {:exit, reason}, st)
        terminate_insulator(st)

      {:EXIT, ^parent, _} ->
        kill_task(child, st)
    end
  end

  defp kill_task(child, st) do
    :erlang.exit(child, :kill)
    terminate_insulator(st)
  end

  defp terminate_insulator(st) do
    parent = r_procstate(st, :parent)
    send(parent, {:done, r_procstate(st, :ref), self()})
    :erlang.unlink(parent)
    exit(:normal)
  end

  defp exit_messages(id, cause, st) do
    message_super(id, {:cancel, cause}, st)

    case r_procstate(st, :id) do
      ^id ->
        :ok

      id1 ->
        message_super(id1, {:cancel, {:blame, id}}, st)
    end
  end

  defp message_insulator(data, st) do
    send(r_procstate(st, :insulator), {:child, self(), r_procstate(st, :id), data})
  end

  defp set_timeout(time, st) do
    :erlang.send_after(
      time,
      r_procstate(st, :insulator),
      {:timeout, self(), r_procstate(st, :id)}
    )
  end

  defp clear_timeout(ref) do
    :erlang.cancel_timer(ref)
  end

  defp with_timeout(:undefined, default, f, st) do
    with_timeout(default, f, st)
  end

  defp with_timeout(time, _Default, f, st) do
    with_timeout(time, f, st)
  end

  defp with_timeout(:infinity, f, _St) do
    {t0, _} = :erlang.statistics(:wall_clock)
    value = f.()
    {t1, _} = :erlang.statistics(:wall_clock)
    {value, t1 - t0}
  end

  defp with_timeout(time, f, st)
       when is_integer(time) and
              time > 4_294_967_295 do
    with_timeout(4_294_967_295, f, st)
  end

  defp with_timeout(time, f, st)
       when is_integer(time) and
              time < 0 do
    with_timeout(0, f, st)
  end

  defp with_timeout(time, f, st) when is_integer(time) do
    ref = set_timeout(time, st)
    {t0, _} = :erlang.statistics(:wall_clock)

    try do
      f.()
    else
      value ->
        {t1, _} = :erlang.statistics(:wall_clock)
        {value, t1 - t0}
    after
      clear_timeout(ref)
    end
  end

  defp child_process(fun, st) do
    :erlang.group_leader(r_procstate(st, :insulator), self())

    try do
      fun.()
    catch
      {:eunit_abort, cause} ->
        message_insulator({:abort, cause}, st)
        exit(:aborted)
    else
      _ ->
        :ok
    end
  end

  defp child_test_() do
    [
      {'test processes do not trap exit signals',
       {380,
        fn ->
          (fn ->
             case :erlang.process_flag(:trap_exit, false) do
               false ->
                 :ok

               __V ->
                 :erlang.error(
                   {:assertMatch,
                    [
                      {:module, :eunit_proc},
                      {:line, 380},
                      {:expression, 'process_flag ( trap_exit , false )'},
                      {:pattern, 'false'},
                      {:value, __V}
                    ]}
                 )
             end
           end).()
        end}}
    ]
  end

  defp abort_task(cause) do
    throw({:eunit_abort, cause})
  end

  defp wait_for_task(pid, st) do
    wait_for_tasks(:sets.from_list([pid]), st)
  end

  defp wait_for_tasks(pidSet, st) do
    case :sets.size(pidSet) do
      0 ->
        :ok

      _ ->
        reference = r_procstate(st, :ref)

        receive do
          {:done, ^reference, pid} ->
            rest = :sets.del_element(pid, pidSet)
            wait_for_tasks(rest, st)
        end
    end
  end

  defp tests(t, st) do
    i = :eunit_data.iter_init(t, r_procstate(st, :id))

    case r_procstate(st, :order) do
      :inorder ->
        tests_inorder(i, st)

      :inparallel ->
        tests_inparallel(i, 0, st)

      {:inparallel, n} when is_integer(n) and n >= 0 ->
        tests_inparallel(i, n, st)
    end
  end

  defp set_id(i, st) do
    r_procstate(st, id: :eunit_data.iter_id(i))
  end

  defp tests_inorder(i, st) do
    tests_inorder(i, 0, st)
  end

  defp tests_inorder(i, n, st) do
    case get_next_item(i) do
      {t, i1} ->
        handle_item(t, set_id(i1, st))
        tests_inorder(i1, n + 1, st)

      :none ->
        n
    end
  end

  defp tests_inparallel(i, k0, st) do
    tests_inparallel(i, 0, st, k0, k0, :sets.new())
  end

  defp tests_inparallel(i, n, st, k, k0, children)
       when k <= 0 and
              k0 > 0 do
    wait_for_tasks(children, st)
    tests_inparallel(i, n, st, k0, k0, :sets.new())
  end

  defp tests_inparallel(i, n, st, k, k0, children) do
    case get_next_item(i) do
      {t, i1} ->
        child = spawn_item(t, set_id(i1, st))
        tests_inparallel(i1, n + 1, st, k - 1, k0, :sets.add_element(child, children))

      :none ->
        wait_for_tasks(children, st)
        n
    end
  end

  defp spawn_item(t, st0) do
    fun = fn st ->
      fn ->
        handle_item(t, st)
      end
    end

    start_task(:local, fun, st0)
  end

  defp get_next_item(i) do
    try do
      :eunit_data.iter_next(i)
    catch
      term ->
        abort_task(term)
    end
  end

  defp handle_item(t, st) do
    case t do
      r_test() ->
        handle_test(t, st)

      r_group() ->
        handle_group(t, st)
    end
  end

  defp handle_test(t, st) do
    data = [{:desc, r_test(t, :desc)}, {:source, r_test(t, :location)}, {:line, r_test(t, :line)}]
    message_insulator({:begin, :test, data}, st)
    g0 = :erlang.group_leader()
    runner = self()
    g1 = new_group_leader(runner)
    :erlang.group_leader(g1, self())

    {status, time} =
      with_timeout(
        r_test(t, :timeout),
        5000,
        fn ->
          run_test(t)
        end,
        st
      )

    :erlang.group_leader(g0, self())
    output = group_leader_sync(g1)
    :io.put_chars(output)
    message_insulator({:end, status, time}, st)
    :ok
  end

  defp run_test(r_test(f: f)) do
    try do
      :eunit_test.run_testfun(f)
    catch
      wrapperError ->
        {:skipped, wrapperError}
    else
      {:ok, _Value} ->
        :ok

      {:error, exception} ->
        {:error, exception}
    end
  end

  defp set_group_order(r_group(order: :undefined), st) do
    st
  end

  defp set_group_order(r_group(order: order), st) do
    r_procstate(st, order: order)
  end

  defp handle_group(t, st0) do
    st = set_group_order(t, st0)

    case r_group(t, :spawn) do
      :undefined ->
        run_group(t, st)

      type ->
        child = spawn_group(type, t, st)
        wait_for_task(child, st)
    end
  end

  defp spawn_group(type, t, st0) do
    fun = fn st ->
      fn ->
        run_group(t, st)
      end
    end

    start_task(type, fun, st0)
  end

  defp run_group(t, st) do
    timeout = r_group(t, :timeout)

    data = [
      {:desc, r_group(t, :desc)},
      {:spawn, r_group(t, :spawn)},
      {:order, r_group(t, :order)}
    ]

    message_insulator({:begin, :group, data}, st)

    f = fn g ->
      enter_group(g, timeout, st)
    end

    try do
      with_context(t, f)
    catch
      {:context_error, why, trace} ->
        message_insulator({:skipped, {why, trace}}, st)
    else
      {status, time} ->
        message_insulator({:end, status, time}, st)
    end

    :ok
  end

  defp enter_group(t, timeout, st) do
    with_timeout(
      timeout,
      :infinity,
      fn ->
        tests(t, st)
      end,
      st
    )
  end

  defp with_context(r_group(context: :undefined, tests: t), f) do
    f.(t)
  end

  defp with_context(r_group(context: r_context() = c, tests: i), f) do
    :eunit_data.enter_context(c, i, f)
  end

  defp new_group_leader(runner) do
    spawn_link(:eunit_proc, :group_leader_process, [runner])
  end

  def group_leader_process(runner) do
    group_leader_loop(runner, :infinity, [])
  end

  defp group_leader_loop(runner, wait, buf) do
    receive do
      {:io_request, from, replyAs, req} ->
        p = :erlang.process_flag(:priority, :normal)
        buf1 = io_request(from, replyAs, req, buf)
        :erlang.process_flag(:priority, p)
        group_leader_loop(runner, wait, buf1)

      :stop ->
        receive do
        after
          2 ->
            :ok
        end

        :erlang.process_flag(:priority, :low)
        group_leader_loop(runner, 0, buf)

      {:get_output, from} ->
        send(from, {:output, :lists.flatten(:lists.reverse(buf))})
        group_leader_loop(runner, wait, buf)

      _ ->
        group_leader_loop(runner, wait, buf)
    after
      wait ->
        :erlang.process_flag(:priority, :normal)
        send(runner, {self(), :lists.reverse(buf)})
    end
  end

  defp group_leader_sync(g) do
    send(g, :stop)

    receive do
      {^g, buf} ->
        buf
    end
  end

  defp io_request(from, replyAs, req, buf) do
    {reply, buf1} = io_request(req, buf)
    io_reply(from, replyAs, reply)
    buf1
  end

  defp io_reply(from, replyAs, reply) do
    send(from, {:io_reply, replyAs, reply})
  end

  defp io_request({:put_chars, chars}, buf) do
    {:ok, [chars | buf]}
  end

  defp io_request({:put_chars, m, f, as}, buf) do
    try do
      apply(m, f, as)
    catch
      c, t ->
        {{:error, {c, t, __STACKTRACE__}}, buf}
    else
      chars ->
        {:ok, [chars | buf]}
    end
  end

  defp io_request({:put_chars, _Enc, chars}, buf) do
    io_request({:put_chars, chars}, buf)
  end

  defp io_request({:put_chars, _Enc, mod, func, args}, buf) do
    io_request({:put_chars, mod, func, args}, buf)
  end

  defp io_request({:get_chars, _Enc, _Prompt, _N}, buf) do
    {:eof, buf}
  end

  defp io_request({:get_chars, _Prompt, _N}, buf) do
    {:eof, buf}
  end

  defp io_request({:get_line, _Prompt}, buf) do
    {:eof, buf}
  end

  defp io_request({:get_line, _Enc, _Prompt}, buf) do
    {:eof, buf}
  end

  defp io_request({:get_until, _Prompt, _M, _F, _As}, buf) do
    {:eof, buf}
  end

  defp io_request(
         {:get_until, _Enc, _Prompt, _M, _F, _As},
         buf
       ) do
    {:eof, buf}
  end

  defp io_request({:setopts, _Opts}, buf) do
    {:ok, buf}
  end

  defp io_request(:getopts, buf) do
    {{:error, :enotsup}, buf}
  end

  defp io_request({:get_geometry, :columns}, buf) do
    {{:error, :enotsup}, buf}
  end

  defp io_request({:get_geometry, :rows}, buf) do
    {{:error, :enotsup}, buf}
  end

  defp io_request({:requests, reqs}, buf) do
    io_requests(reqs, {:ok, buf})
  end

  defp io_request(_, buf) do
    {{:error, :request}, buf}
  end

  defp io_requests([r | rs], {:ok, buf}) do
    io_requests(rs, io_request(r, buf))
  end

  defp io_requests(_, result) do
    result
  end

  defp io_error_test_() do
    [
      {684,
       fn ->
         (fn ->
            case :io.getopts() do
              {:error, :enotsup} ->
                :ok

              __V ->
                :erlang.error(
                  {:assertMatch,
                   [
                     {:module, :eunit_proc},
                     {:line, 684},
                     {:expression, 'io : getopts ( )'},
                     {:pattern, '{ error , enotsup }'},
                     {:value, __V}
                   ]}
                )
            end
          end).()
       end},
      {685,
       fn ->
         (fn ->
            case :io.columns() do
              {:error, :enotsup} ->
                :ok

              __V ->
                :erlang.error(
                  {:assertMatch,
                   [
                     {:module, :eunit_proc},
                     {:line, 685},
                     {:expression, 'io : columns ( )'},
                     {:pattern, '{ error , enotsup }'},
                     {:value, __V}
                   ]}
                )
            end
          end).()
       end},
      {686,
       fn ->
         (fn ->
            case :io.rows() do
              {:error, :enotsup} ->
                :ok

              __V ->
                :erlang.error(
                  {:assertMatch,
                   [
                     {:module, :eunit_proc},
                     {:line, 686},
                     {:expression, 'io : rows ( )'},
                     {:pattern, '{ error , enotsup }'},
                     {:value, __V}
                   ]}
                )
            end
          end).()
       end}
    ]
  end

  defp get_output_test() do
    :io.format('Hello')
    output = get_output()

    (fn ->
       __X = 'Hello'

       case output do
         ^__X ->
           :ok

         __V ->
           :erlang.error(
             {:assertEqual,
              [
                {:module, :eunit_proc},
                {:line, 691},
                {:expression, 'Output'},
                {:expected, __X},
                {:value, __V}
              ]}
           )
       end
     end).()
  end
end
