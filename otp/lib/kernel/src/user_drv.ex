defmodule :m_user_drv do
  use Bitwise

  def start() do
    spawn(:user_drv, :server, [:"tty_sl -c -e", {:shell, :start, [:init]}])
  end

  def start([pname]) do
    spawn(:user_drv, :server, [pname, {:shell, :start, [:init]}])
  end

  def start([pname | args]) do
    spawn(:user_drv, :server, [pname | args])
  end

  def start(pname) do
    spawn(:user_drv, :server, [pname, {:shell, :start, [:init]}])
  end

  def start(pname, shell) do
    spawn(:user_drv, :server, [pname, shell])
  end

  def start(iname, oname, shell) do
    spawn(:user_drv, :server, [iname, oname, shell])
  end

  def interfaces(userDrv) do
    case :erlang.process_info(userDrv, :dictionary) do
      {:dictionary, dict} ->
        case :lists.keysearch(:current_group, 1, dict) do
          {:value, gr = {_, group}} when is_pid(group) ->
            [gr]

          _ ->
            []
        end

      _ ->
        []
    end
  end

  def server(pid, shell) when is_pid(pid) do
    server1(pid, pid, shell)
  end

  def server(pname, shell) do
    :erlang.process_flag(:trap_exit, true)

    case (try do
            :erlang.open_port({:spawn, pname}, [:eof])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        :user.start()

      port ->
        server1(port, port, shell)
    end
  end

  def server(iname, oname, shell) do
    :erlang.process_flag(:trap_exit, true)

    case (try do
            :erlang.open_port({:spawn, iname}, [:eof])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        :user.start()

      iport ->
        oport = :erlang.open_port({:spawn, oname}, [:eof])
        server1(iport, oport, shell)
    end
  end

  defp server1(iport, oport, shell) do
    :erlang.put(:eof, false)
    user = start_user()
    gr1 = gr_add_cur(gr_new(), user, {})

    {curr, shell1} =
      case :init.get_argument(:remsh) do
        {:ok, [[node]]} ->
          aNode =
            cond do
              node() === :nonode@nohost ->
                _ = :net_kernel.start([:undefined, :shortnames])

                nodeName =
                  append_hostname(
                    node,
                    :net_kernel.nodename()
                  )

                true = :net_kernel.connect_node(nodeName)
                nodeName

              true ->
                append_hostname(node, node())
            end

          rShell = {aNode, :shell, :start, []}
          rGr = :group.start(self(), rShell, rem_sh_opts(aNode))
          {rGr, rShell}

        e when e === :error or e === {:ok, [[]]} ->
          {:group.start(self(), shell), shell}
      end

    :erlang.put(:current_group, curr)
    gr = gr_add_cur(gr1, curr, shell1)

    io_request(
      {:put_chars, :unicode,
       flatten(
         :io_lib.format(
           '~ts\n',
           [:erlang.system_info(:system_version)]
         )
       )},
      iport,
      oport
    )

    server_loop(iport, oport, curr, user, gr, {false, :queue.new()})
  end

  defp append_hostname(node, localNode) do
    case :string.find(node, '@') do
      :nomatch ->
        :erlang.list_to_atom(
          node ++
            :string.find(
              :erlang.atom_to_list(localNode),
              '@'
            )
        )

      _ ->
        :erlang.list_to_atom(node)
    end
  end

  defp rem_sh_opts(node) do
    [
      {:expand_fun,
       fn b ->
         :rpc.call(node, :edlin_expand, :expand, [b])
       end}
    ]
  end

  defp start_user() do
    case :erlang.whereis(:user_drv) do
      :undefined ->
        :erlang.register(:user_drv, self())

      _ ->
        :ok
    end

    case :erlang.whereis(:user) do
      :undefined ->
        user = :group.start(self(), {})
        :erlang.register(:user, user)
        user

      user ->
        user
    end
  end

  defp server_loop(iport, oport, user, gr, iOQueue) do
    curr = gr_cur_pid(gr)
    :erlang.put(:current_group, curr)
    server_loop(iport, oport, curr, user, gr, iOQueue)
  end

  defp server_loop(iport, oport, curr, user, gr, {resp, iOQ} = iOQueue) do
    receive do
      {^iport, {:data, bs}} ->
        bsBin = :erlang.list_to_binary(bs)
        unicode = :unicode.characters_to_list(bsBin, :utf8)
        port_bytes(unicode, iport, oport, curr, user, gr, iOQueue)

      {^iport, :eof} ->
        send(curr, {self(), :eof})
        server_loop(iport, oport, curr, user, gr, iOQueue)

      {requester, :tty_geometry} ->
        send(requester, {self(), :tty_geometry, get_tty_geometry(iport)})
        server_loop(iport, oport, curr, user, gr, iOQueue)

      {requester, :get_unicode_state} ->
        send(requester, {self(), :get_unicode_state, get_unicode_state(iport)})
        server_loop(iport, oport, curr, user, gr, iOQueue)

      {requester, :set_unicode_state, bool} ->
        send(requester, {self(), :set_unicode_state, set_unicode_state(iport, bool)})
        server_loop(iport, oport, curr, user, gr, iOQueue)

      req
      when :erlang.element(
             1,
             req
           ) === user or
             (:erlang.element(
                1,
                req
              ) === curr and
                tuple_size(req) === 2) or tuple_size(req) === 3 ->
        newQ = handle_req(req, iport, oport, iOQueue)
        server_loop(iport, oport, curr, user, gr, newQ)

      {^oport, :ok} ->
        {origin, reply} = resp
        send(origin, {:reply, reply})
        newQ = handle_req(:next, iport, oport, {false, iOQ})
        server_loop(iport, oport, curr, user, gr, newQ)

      {:EXIT, ^iport, _R} ->
        server_loop(iport, oport, curr, user, gr, iOQueue)

      {:EXIT, ^oport, _R} ->
        server_loop(iport, oport, curr, user, gr, iOQueue)

      {:EXIT, ^user, :shutdown} ->
        server_loop(iport, oport, curr, user, gr, iOQueue)

      {:EXIT, ^user, _R} ->
        newU = start_user()
        server_loop(iport, oport, curr, newU, gr_set_num(gr, 1, newU, {}), iOQueue)

      {:EXIT, pid, r} ->
        case gr_cur_pid(gr) do
          ^pid when r !== :die and r !== :terminated ->
            cond do
              r !== :normal ->
                io_requests([{:put_chars, :unicode, '*** ERROR: '}], iport, oport)

              true ->
                io_requests([{:put_chars, :unicode, '*** '}], iport, oport)
            end

            io_requests([{:put_chars, :unicode, 'Shell process terminated! '}], iport, oport)
            gr1 = gr_del_pid(gr, pid)

            case gr_get_info(gr, pid) do
              {ix, {:shell, :start, params}} ->
                io_requests([{:put_chars, :unicode, '***\n'}], iport, oport)
                pid1 = :group.start(self(), {:shell, :start, params})

                {:ok, gr2} =
                  gr_set_cur(
                    gr_set_num(gr1, ix, pid1, {:shell, :start, params}),
                    ix
                  )

                :erlang.put(:current_group, pid1)
                server_loop(iport, oport, pid1, user, gr2, iOQueue)

              _ ->
                io_requests([{:put_chars, :unicode, '(^G to start new job) ***\n'}], iport, oport)
                server_loop(iport, oport, curr, user, gr1, iOQueue)
            end

          _ ->
            server_loop(iport, oport, curr, user, gr_del_pid(gr, pid), iOQueue)
        end

      {requester, {:put_chars_sync, _, _, reply}} ->
        send(requester, {:reply, reply})
        server_loop(iport, oport, curr, user, gr, iOQueue)

      _X ->
        server_loop(iport, oport, curr, user, gr, iOQueue)
    end
  end

  defp handle_req(:next, iport, oport, {false, iOQ} = iOQueue) do
    case :queue.out(iOQ) do
      {:empty, _} ->
        iOQueue

      {{:value, {origin, req}}, execQ} ->
        case io_request(req, iport, oport) do
          :ok ->
            handle_req(:next, iport, oport, {false, execQ})

          reply ->
            {{origin, reply}, execQ}
        end
    end
  end

  defp handle_req(msg, iport, oport, {false, iOQ} = iOQueue) do
    :empty = :queue.peek(iOQ)
    {origin, req} = msg

    case io_request(req, iport, oport) do
      :ok ->
        iOQueue

      reply ->
        {{origin, reply}, iOQ}
    end
  end

  defp handle_req(msg, _Iport, _Oport, {resp, iOQ}) do
    {resp, :queue.in(msg, iOQ)}
  end

  defp port_bytes([?\a | _Bs], iport, oport, _Curr, user, gr, iOQueue) do
    handle_escape(iport, oport, user, gr, iOQueue)
  end

  defp port_bytes([3 | _Bs], iport, oport, curr, user, gr, iOQueue) do
    interrupt_shell(iport, oport, curr, user, gr, iOQueue)
  end

  defp port_bytes([b], iport, oport, curr, user, gr, iOQueue) do
    send(curr, {self(), {:data, [b]}})
    server_loop(iport, oport, curr, user, gr, iOQueue)
  end

  defp port_bytes(bs, iport, oport, curr, user, gr, iOQueue) do
    case member(?\a, bs) do
      true ->
        handle_escape(iport, oport, user, gr, iOQueue)

      false ->
        send(curr, {self(), {:data, bs}})
        server_loop(iport, oport, curr, user, gr, iOQueue)
    end
  end

  defp interrupt_shell(iport, oport, curr, user, gr, iOQueue) do
    case gr_get_info(gr, curr) do
      :undefined ->
        :ok

      _ ->
        :erlang.exit(curr, :interrupt)
    end

    server_loop(iport, oport, curr, user, gr, iOQueue)
  end

  defp handle_escape(iport, oport, user, gr, iOQueue) do
    case :application.get_env(:stdlib, :shell_esc) do
      {:ok, :abort} ->
        pid = gr_cur_pid(gr)
        :erlang.exit(pid, :die)

        gr1 =
          case gr_get_info(gr, pid) do
            {_Ix, {}} ->
              gr

            _ ->
              receive do
                {:EXIT, ^pid, _} ->
                  gr_del_pid(gr, pid)
              after
                1000 ->
                  gr
              end
          end

        pid1 = :group.start(self(), {:shell, :start, []})
        io_request({:put_chars, :unicode, '\n'}, iport, oport)
        server_loop(iport, oport, user, gr_add_cur(gr1, pid1, {:shell, :start, []}), iOQueue)

      _ ->
        io_request({:put_chars, :unicode, '\nUser switch command\n'}, iport, oport)
        :edlin.init(gr_cur_pid(gr))
        server_loop(iport, oport, user, switch_loop(iport, oport, gr), iOQueue)
    end
  end

  defp switch_loop(iport, oport, gr) do
    line = get_line(:edlin.start(' --> '), iport, oport)
    switch_cmd(:erl_scan.string(line), iport, oport, gr)
  end

  defp switch_cmd({:ok, [{:atom, _, :c}, {:integer, _, i}], _}, iport, oport, gr0) do
    case gr_set_cur(gr0, i) do
      {:ok, gr} ->
        gr

      :undefined ->
        unknown_group(iport, oport, gr0)
    end
  end

  defp switch_cmd({:ok, [{:atom, _, :c}], _}, iport, oport, gr) do
    case gr_get_info(gr, gr_cur_pid(gr)) do
      :undefined ->
        unknown_group(iport, oport, gr)

      _ ->
        gr
    end
  end

  defp switch_cmd({:ok, [{:atom, _, :i}, {:integer, _, i}], _}, iport, oport, gr) do
    case gr_get_num(gr, i) do
      {:pid, pid} ->
        :erlang.exit(pid, :interrupt)
        switch_loop(iport, oport, gr)

      :undefined ->
        unknown_group(iport, oport, gr)
    end
  end

  defp switch_cmd({:ok, [{:atom, _, :i}], _}, iport, oport, gr) do
    pid = gr_cur_pid(gr)

    case gr_get_info(gr, pid) do
      :undefined ->
        unknown_group(iport, oport, gr)

      _ ->
        :erlang.exit(pid, :interrupt)
        switch_loop(iport, oport, gr)
    end
  end

  defp switch_cmd({:ok, [{:atom, _, :k}, {:integer, _, i}], _}, iport, oport, gr) do
    case gr_get_num(gr, i) do
      {:pid, pid} ->
        :erlang.exit(pid, :die)

        case gr_get_info(gr, pid) do
          {_Ix, {}} ->
            switch_loop(iport, oport, gr)

          _ ->
            gr1 =
              receive do
                {:EXIT, ^pid, _} ->
                  gr_del_pid(gr, pid)
              after
                1000 ->
                  gr
              end

            switch_loop(iport, oport, gr1)
        end

      :undefined ->
        unknown_group(iport, oport, gr)
    end
  end

  defp switch_cmd({:ok, [{:atom, _, :k}], _}, iport, oport, gr) do
    pid = gr_cur_pid(gr)
    info = gr_get_info(gr, pid)

    case info do
      :undefined ->
        unknown_group(iport, oport, gr)

      {_Ix, {}} ->
        switch_loop(iport, oport, gr)

      _ ->
        :erlang.exit(pid, :die)

        gr1 =
          receive do
            {:EXIT, ^pid, _} ->
              gr_del_pid(gr, pid)
          after
            1000 ->
              gr
          end

        switch_loop(iport, oport, gr1)
    end
  end

  defp switch_cmd({:ok, [{:atom, _, :j}], _}, iport, oport, gr) do
    io_requests(gr_list(gr), iport, oport)
    switch_loop(iport, oport, gr)
  end

  defp switch_cmd({:ok, [{:atom, _, :s}, {:atom, _, shell}], _}, iport, oport, gr0) do
    pid = :group.start(self(), {shell, :start, []})
    gr = gr_add_cur(gr0, pid, {shell, :start, []})
    switch_loop(iport, oport, gr)
  end

  defp switch_cmd({:ok, [{:atom, _, :s}], _}, iport, oport, gr0) do
    pid = :group.start(self(), {:shell, :start, []})
    gr = gr_add_cur(gr0, pid, {:shell, :start, []})
    switch_loop(iport, oport, gr)
  end

  defp switch_cmd({:ok, [{:atom, _, :r}], _}, iport, oport, gr0) do
    case :erlang.is_alive() do
      true ->
        node = :pool.get_node()
        pid = :group.start(self(), {node, :shell, :start, []})
        gr = gr_add_cur(gr0, pid, {node, :shell, :start, []})
        switch_loop(iport, oport, gr)

      false ->
        io_request({:put_chars, :unicode, 'Not alive\n'}, iport, oport)
        switch_loop(iport, oport, gr0)
    end
  end

  defp switch_cmd({:ok, [{:atom, _, :r}, {:atom, _, node}], _}, iport, oport, gr0) do
    pid = :group.start(self(), {node, :shell, :start, []})
    gr = gr_add_cur(gr0, pid, {node, :shell, :start, []})
    switch_loop(iport, oport, gr)
  end

  defp switch_cmd(
         {:ok, [{:atom, _, :r}, {:atom, _, node}, {:atom, _, shell}], _},
         iport,
         oport,
         gr0
       ) do
    pid = :group.start(self(), {node, shell, :start, []})
    gr = gr_add_cur(gr0, pid, {node, shell, :start, []})
    switch_loop(iport, oport, gr)
  end

  defp switch_cmd({:ok, [{:atom, _, :q}], _}, iport, oport, gr) do
    case :erlang.system_info(:break_ignored) do
      true ->
        io_request({:put_chars, :unicode, 'Unknown command\n'}, iport, oport)
        switch_loop(iport, oport, gr)

      false ->
        :erlang.halt()
    end
  end

  defp switch_cmd({:ok, [{:atom, _, :h}], _}, iport, oport, gr) do
    list_commands(iport, oport)
    switch_loop(iport, oport, gr)
  end

  defp switch_cmd({:ok, [{:"?", _}], _}, iport, oport, gr) do
    list_commands(iport, oport)
    switch_loop(iport, oport, gr)
  end

  defp switch_cmd({:ok, [], _}, iport, oport, gr) do
    switch_loop(iport, oport, gr)
  end

  defp switch_cmd({:ok, _Ts, _}, iport, oport, gr) do
    io_request({:put_chars, :unicode, 'Unknown command\n'}, iport, oport)
    switch_loop(iport, oport, gr)
  end

  defp switch_cmd(_Ts, iport, oport, gr) do
    io_request({:put_chars, :unicode, 'Illegal input\n'}, iport, oport)
    switch_loop(iport, oport, gr)
  end

  defp unknown_group(iport, oport, gr) do
    io_request({:put_chars, :unicode, 'Unknown job\n'}, iport, oport)
    switch_loop(iport, oport, gr)
  end

  defp list_commands(iport, oport) do
    quitReq =
      case :erlang.system_info(:break_ignored) do
        true ->
          []

        false ->
          [{:put_chars, :unicode, '  q                 - quit erlang\n'}]
      end

    io_requests(
      [
        {:put_chars, :unicode, '  c [nn]            - connect to job\n'},
        {:put_chars, :unicode, '  i [nn]            - interrupt job\n'},
        {:put_chars, :unicode, '  k [nn]            - kill job\n'},
        {:put_chars, :unicode, '  j                 - list all jobs\n'},
        {:put_chars, :unicode, '  s [shell]         - start local shell\n'},
        {:put_chars, :unicode, '  r [node [shell]]  - start remote shell\n'}
      ] ++ quitReq ++ [{:put_chars, :unicode, '  ? | h             - this message\n'}],
      iport,
      oport
    )
  end

  defp get_line({:done, line, _Rest, rs}, iport, oport) do
    io_requests(rs, iport, oport)
    line
  end

  defp get_line({:undefined, _Char, cs, cont, rs}, iport, oport) do
    io_requests(rs, iport, oport)
    io_request(:beep, iport, oport)
    get_line(:edlin.edit_line(cs, cont), iport, oport)
  end

  defp get_line({what, cont0, rs}, iport, oport) do
    io_requests(rs, iport, oport)

    receive do
      {^iport, {:data, cs}} ->
        get_line(:edlin.edit_line(cs, cont0), iport, oport)

      {^iport, :eof} ->
        get_line(:edlin.edit_line(:eof, cont0), iport, oport)
    after
      get_line_timeout(what) ->
        get_line(:edlin.edit_line([], cont0), iport, oport)
    end
  end

  defp get_line_timeout(:blink) do
    1000
  end

  defp get_line_timeout(:more_chars) do
    :infinity
  end

  defp get_tty_geometry(iport) do
    case (try do
            :erlang.port_control(iport, 100 + 25_889_024, [])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      list when length(list) === 8 ->
        <<w::size(32)-native, h::size(32)-native>> = :erlang.list_to_binary(list)
        {w, h}

      _ ->
        :error
    end
  end

  defp get_unicode_state(iport) do
    case (try do
            :erlang.port_control(iport, 101 + 25_889_024, [])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      [int] when int > 0 ->
        true

      [int] when int === 0 ->
        false

      _ ->
        :error
    end
  end

  defp set_unicode_state(iport, bool) do
    data =
      case bool do
        true ->
          [1]

        false ->
          [0]
      end

    case (try do
            :erlang.port_control(iport, 102 + 25_889_024, data)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      [int] when int > 0 ->
        {:unicode, :utf8}

      [int] when int === 0 ->
        {:unicode, false}

      _ ->
        :error
    end
  end

  defp io_request({:requests, rs}, iport, oport) do
    io_requests(rs, iport, oport)
  end

  defp io_request(request, _Iport, oport) do
    case io_command(request) do
      {data, reply} ->
        true = :erlang.port_command(oport, data)
        reply

      :unhandled ->
        :ok
    end
  end

  defp io_requests([r | rs], iport, oport) do
    io_request(r, iport, oport)
    io_requests(rs, iport, oport)
  end

  defp io_requests([], _Iport, _Oport) do
    :ok
  end

  defp put_int16(n, tail) do
    [[n >>> 8 &&& 255, n &&& 255] | tail]
  end

  defp io_command({:put_chars_sync, :unicode, cs, reply}) do
    {[5 | :unicode.characters_to_binary(cs, :utf8)], reply}
  end

  defp io_command({:put_chars, :unicode, cs}) do
    {[0 | :unicode.characters_to_binary(cs, :utf8)], :ok}
  end

  defp io_command({:move_rel, n}) do
    {[1 | put_int16(n, [])], :ok}
  end

  defp io_command({:insert_chars, :unicode, cs}) do
    {[2 | :unicode.characters_to_binary(cs, :utf8)], :ok}
  end

  defp io_command({:delete_chars, n}) do
    {[3 | put_int16(n, [])], :ok}
  end

  defp io_command(:beep) do
    {[4], :ok}
  end

  defp io_command(_) do
    :unhandled
  end

  defp gr_new() do
    {0, 0, :none, []}
  end

  defp gr_get_num({_Next, _CurI, _CurP, gs}, i) do
    gr_get_num1(gs, i)
  end

  defp gr_get_num1([{i, _Pid, {}} | _Gs], i) do
    :undefined
  end

  defp gr_get_num1([{i, pid, _S} | _Gs], i) do
    {:pid, pid}
  end

  defp gr_get_num1([_G | gs], i) do
    gr_get_num1(gs, i)
  end

  defp gr_get_num1([], _I) do
    :undefined
  end

  defp gr_get_info({_Next, _CurI, _CurP, gs}, pid) do
    gr_get_info1(gs, pid)
  end

  defp gr_get_info1([{i, pid, s} | _Gs], pid) do
    {i, s}
  end

  defp gr_get_info1([_G | gs], i) do
    gr_get_info1(gs, i)
  end

  defp gr_get_info1([], _I) do
    :undefined
  end

  defp gr_add_cur({next, _CurI, _CurP, gs}, pid, shell) do
    {next + 1, next, pid, append(gs, [{next, pid, shell}])}
  end

  defp gr_set_cur({next, _CurI, _CurP, gs}, i) do
    case gr_get_num1(gs, i) do
      {:pid, pid} ->
        {:ok, {next, i, pid, gs}}

      :undefined ->
        :undefined
    end
  end

  defp gr_set_num({next, curI, curP, gs}, i, pid, shell) do
    {next, curI, curP, gr_set_num1(gs, i, pid, shell)}
  end

  defp gr_set_num1([{i, _Pid, _Shell} | gs], i, newPid, newShell) do
    [{i, newPid, newShell} | gs]
  end

  defp gr_set_num1([{i, pid, shell} | gs], newI, newPid, newShell)
       when newI > i do
    [{i, pid, shell} | gr_set_num1(gs, newI, newPid, newShell)]
  end

  defp gr_set_num1(gs, newI, newPid, newShell) do
    [{newI, newPid, newShell} | gs]
  end

  defp gr_del_pid({next, curI, curP, gs}, pid) do
    {next, curI, curP, gr_del_pid1(gs, pid)}
  end

  defp gr_del_pid1([{_I, pid, _S} | gs], pid) do
    gs
  end

  defp gr_del_pid1([g | gs], pid) do
    [g | gr_del_pid1(gs, pid)]
  end

  defp gr_del_pid1([], _Pid) do
    []
  end

  defp gr_cur_pid({_Next, _CurI, curP, _Gs}) do
    curP
  end

  defp gr_list({_Next, curI, _CurP, gs}) do
    gr_list(gs, curI, [])
  end

  defp gr_list([{_I, _Pid, {}} | gs], cur, jobs) do
    gr_list(gs, cur, jobs)
  end

  defp gr_list([{cur, _Pid, shell} | gs], cur, jobs) do
    gr_list(gs, cur, [
      {:put_chars, :unicode, flatten(:io_lib.format('~4w* ~w\n', [cur, shell]))}
      | jobs
    ])
  end

  defp gr_list([{i, _Pid, shell} | gs], cur, jobs) do
    gr_list(gs, cur, [
      {:put_chars, :unicode, flatten(:io_lib.format('~4w  ~w\n', [i, shell]))}
      | jobs
    ])
  end

  defp gr_list([], _Cur, jobs) do
    :lists.reverse(jobs)
  end

  defp append([h | t], x) do
    [h | append(t, x)]
  end

  defp append([], x) do
    x
  end

  defp member(x, [x | _Rest]) do
    true
  end

  defp member(x, [_H | rest]) do
    member(x, rest)
  end

  defp member(_X, []) do
    false
  end

  defp flatten(list) do
    flatten(list, [], [])
  end

  defp flatten([h | t], cont, tail) when is_list(h) do
    flatten(h, [t | cont], tail)
  end

  defp flatten([h | t], cont, tail) do
    [h | flatten(t, cont, tail)]
  end

  defp flatten([], [h | cont], tail) do
    flatten(h, cont, tail)
  end

  defp flatten([], [], tail) do
    tail
  end
end
