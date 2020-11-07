defmodule :m_test_server_io do
  use Bitwise
  require Record

  Record.defrecord(:r_st, :st,
    fds: :undefined,
    tags: [],
    shared_gl: :undefined,
    gls: :undefined,
    io_buffering: false,
    buffered: :undefined,
    html_footer: :undefined,
    job_name: :undefined,
    gl_props: :undefined,
    phase: :undefined,
    offline_buffer: :undefined,
    stopping: :undefined,
    pending_ops: :undefined
  )

  def start_link() do
    case :erlang.whereis(:test_server_io) do
      :undefined ->
        case :gen_server.start_link({:local, :test_server_io}, :test_server_io, [], []) do
          {:ok, pid} ->
            {:ok, pid}

          other ->
            other
        end

      pid ->
        reset_state()
        {:ok, pid}
    end
  end

  def stop(filesToClose) do
    oldGL = :erlang.group_leader()
    :erlang.group_leader(self(), self())
    req({:stop, filesToClose})
    :erlang.group_leader(oldGL, self())
    :ok
  end

  def finish() do
    req(:finish)
  end

  def get_gl(shared) when is_boolean(shared) do
    req({:get_gl, shared})
  end

  def set_fd(tag, fd) do
    req({:set_fd, tag, fd})
  end

  def start_transaction() do
    req({:start_transaction, self()})
  end

  def end_transaction() do
    req({:end_transaction, self()})
  end

  def print(from, tag, msg) do
    req({:print, from, tag, msg})
  end

  def print_buffered(pid) do
    req({:print_buffered, pid})
  end

  def print_unexpected(msg) do
    print(:xxxFrom, :unexpected_io, msg)
  end

  def set_footer(footer) do
    req({:set_footer, footer})
  end

  def set_job_name(name) do
    req({:set_job_name, name})
  end

  def set_gl_props(propList) do
    req({:set_gl_props, propList})
  end

  def reset_state() do
    req(:reset_state)
  end

  def init([]) do
    :erlang.process_flag(:trap_exit, true)
    :ct_util.mark_process()
    empty = :gb_trees.empty()
    {:ok, shared} = :test_server_gl.start_link(self())

    {:ok,
     r_st(
       fds: empty,
       shared_gl: shared,
       gls: :gb_sets.empty(),
       io_buffering: :gb_sets.empty(),
       buffered: empty,
       html_footer: '</body>\n</html>\n',
       job_name: '<name not set>',
       gl_props: [],
       phase: :starting,
       offline_buffer: [],
       pending_ops: []
     )}
  end

  defp req(req) do
    :gen_server.call(:test_server_io, req, :infinity)
  end

  def handle_call({:get_gl, false}, _From, r_st(gls: gls, gl_props: props) = st) do
    {:ok, pid} = :test_server_gl.start_link(self())
    :test_server_gl.set_props(pid, props)
    {:reply, pid, r_st(st, gls: :gb_sets.insert(pid, gls))}
  end

  def handle_call({:get_gl, true}, _From, r_st(shared_gl: shared) = st) do
    {:reply, shared, st}
  end

  def handle_call(
        {:set_fd, tag, fd},
        _From,
        r_st(fds: fds0, tags: tags0, offline_buffer: offlineBuff) = st
      ) do
    fds = :gb_trees.enter(tag, fd, fds0)

    st1 =
      r_st(st,
        fds: fds,
        tags: [tag | :lists.delete(tag, tags0)]
      )

    offlineBuff1 =
      cond do
        offlineBuff == [] ->
          []

        true ->
          :lists.filtermap(
            fn
              {t, from, str} when t == tag ->
                _ = output(from, tag, str, st1)
                false

              _ ->
                true
            end,
            :lists.reverse(offlineBuff)
          )
      end

    {:reply, :ok,
     r_st(st1,
       phase: :started,
       offline_buffer: :lists.reverse(offlineBuff1)
     )}
  end

  def handle_call(
        {:start_transaction, pid},
        _From,
        r_st(io_buffering: buffer0, buffered: buf0) = st
      ) do
    buf =
      case :gb_trees.is_defined(pid, buf0) do
        false ->
          :gb_trees.insert(pid, :queue.new(), buf0)

        true ->
          buf0
      end

    buffer = :gb_sets.add(pid, buffer0)
    {:reply, :ok, r_st(st, io_buffering: buffer, buffered: buf)}
  end

  def handle_call({:print, from, tag, str}, _From, st0) do
    st = output(from, tag, str, st0)
    {:reply, :ok, st}
  end

  def handle_call(
        {:end_transaction, pid},
        _From,
        r_st(io_buffering: buffer0, buffered: buffered0) = st0
      ) do
    q0 = :gb_trees.get(pid, buffered0)
    q = :queue.in(:eot, q0)
    buffered = :gb_trees.update(pid, q, buffered0)
    buffer = :gb_sets.delete_any(pid, buffer0)
    st = r_st(st0, io_buffering: buffer, buffered: buffered)
    {:reply, :ok, st}
  end

  def handle_call({:print_buffered, pid}, _From, r_st(buffered: buffered0) = st0) do
    q0 = :gb_trees.get(pid, buffered0)
    q = do_print_buffered(q0, st0)
    buffered = :gb_trees.update(pid, q, buffered0)
    st = r_st(st0, buffered: buffered)
    {:reply, :ok, st}
  end

  def handle_call({:set_footer, footer}, _From, st) do
    {:reply, :ok, r_st(st, html_footer: footer)}
  end

  def handle_call({:set_job_name, name}, _From, st) do
    {:reply, :ok, r_st(st, job_name: name)}
  end

  def handle_call({:set_gl_props, props}, _From, r_st(shared_gl: shared) = st) do
    :test_server_gl.set_props(shared, props)
    {:reply, :ok, r_st(st, gl_props: props)}
  end

  def handle_call(:reset_state, from, r_st(phase: :stopping, pending_ops: ops) = st) do
    op = fn newSt ->
      {_, result, newSt1} = handle_call(:reset_state, from, newSt)
      {result, newSt1}
    end

    {:noreply, r_st(st, pending_ops: [{from, op} | ops])}
  end

  def handle_call(
        :reset_state,
        _From,
        r_st(fds: fds, tags: tags, shared_gl: shared0, gls: gls, offline_buffer: offlineBuff)
      ) do
    :lists.foreach(
      fn tag ->
        case :gb_trees.lookup(tag, fds) do
          :none ->
            :ok

          {:value, fd} ->
            :file.close(fd)
        end
      end,
      tags
    )

    :test_server_gl.stop(shared0)
    glList = :gb_sets.to_list(gls)

    _ =
      for gL <- glList do
        :test_server_gl.stop(gL)
      end

    :timer.sleep(100)

    case :lists.filter(
           fn glPid ->
             :erlang.is_process_alive(glPid)
           end,
           glList
         ) do
      [] ->
        :ok

      _ ->
        :timer.sleep(2000)

        for gL <- glList do
          :erlang.exit(gL, :kill)
        end

        :ok
    end

    empty = :gb_trees.empty()
    {:ok, shared} = :test_server_gl.start_link(self())

    {:reply, :ok,
     r_st(
       fds: empty,
       shared_gl: shared,
       gls: :gb_sets.empty(),
       io_buffering: :gb_sets.empty(),
       buffered: empty,
       html_footer: '</body>\n</html>\n',
       job_name: '<name not set>',
       gl_props: [],
       phase: :starting,
       offline_buffer: offlineBuff,
       pending_ops: []
     )}
  end

  def handle_call(
        {:stop, fdTags},
        from,
        r_st(fds: fds0, tags: tags0, shared_gl: sGL, gls: gls0) = st0
      ) do
    st = r_st(st0, gls: :gb_sets.insert(sGL, gls0), phase: :stopping, stopping: from)
    gc(st)

    {fds1, tags1} =
      :lists.foldl(
        fn tag, {fds, tags} ->
          case :gb_trees.lookup(tag, fds) do
            :none ->
              {fds, tags}

            {:value, fd} ->
              _ = :file.close(fd)
              {:gb_trees.delete(tag, fds), :lists.delete(tag, tags)}
          end
        end,
        {fds0, tags0},
        fdTags
      )

    :erlang.send_after(1000, self(), :stop_group_leaders)
    {:noreply, r_st(st, fds: fds1, tags: tags1)}
  end

  def handle_call(:finish, from, st) do
    :gen_server.reply(from, :ok)
    {:stop, :normal, st}
  end

  def handle_info(
        {:EXIT, pid, :normal},
        r_st(gls: gls0, stopping: from) = st
      ) do
    gls = :gb_sets.delete_any(pid, gls0)

    case :gb_sets.is_empty(gls) and from !== :undefined do
      true ->
        :gen_server.reply(from, :ok)
        {:noreply, r_st(st, gls: gls, phase: :stopping, stopping: :undefined)}

      false ->
        {:noreply, r_st(st, gls: gls, phase: :stopping)}
    end
  end

  def handle_info({:EXIT, pid, :killed}, r_st(gls: gls0) = st) do
    {:noreply, r_st(st, gls: :gb_sets.delete_any(pid, gls0))}
  end

  def handle_info({:EXIT, _Pid, reason}, _St) do
    exit(reason)
  end

  def handle_info(:stop_group_leaders, r_st(gls: gls) = st) do
    glPids = :gb_sets.to_list(gls)

    _ =
      for gL <- glPids do
        :test_server_gl.stop(gL)
      end

    :timer.sleep(100)

    wait =
      case :lists.filter(
             fn glPid ->
               :erlang.is_process_alive(glPid)
             end,
             glPids
           ) do
        [] ->
          0

        _ ->
          2000
      end

    :erlang.send_after(wait, self(), :kill_group_leaders)
    {:noreply, st}
  end

  def handle_info(
        :kill_group_leaders,
        r_st(gls: gls, stopping: from, pending_ops: ops) = st
      ) do
    _ =
      for gL <- :gb_sets.to_list(gls) do
        :erlang.exit(gL, :kill)
      end

    cond do
      from != :undefined ->
        :gen_server.reply(from, :ok)

      true ->
        :ok
    end

    st1 =
      :lists.foldr(
        fn {replyTo, op}, newSt ->
          {result, newSt1} = op.(newSt)
          :gen_server.reply(replyTo, result)
          newSt1
        end,
        r_st(st, phase: :idle, pending_ops: []),
        ops
      )

    {:noreply, st1}
  end

  def handle_info(other, st) do
    :io.format('Ignoring: ~tp\n', [other])
    {:noreply, st}
  end

  def terminate(_, _) do
    :ok
  end

  defp output(
         from,
         tag,
         str,
         r_st(io_buffering: buffered, buffered: buf0, phase: phase, offline_buffer: offlineBuff) =
           st
       ) do
    case :gb_sets.is_member(from, buffered) do
      false ->
        case do_output(tag, str, phase, st) do
          :buffer when length(offlineBuff) > 500 ->
            r_st(st, offline_buffer: [])

          :buffer ->
            r_st(st, offline_buffer: [{tag, from, str} | offlineBuff])

          _ ->
            st
        end

      true ->
        q0 = :gb_trees.get(from, buf0)
        q = :queue.in({tag, str}, q0)
        buf = :gb_trees.update(from, q, buf0)
        r_st(st, buffered: buf)
    end
  end

  defp do_output(:stdout, str, _, r_st(job_name: :undefined)) do
    :io.put_chars(str)
  end

  defp do_output(:stdout, str0, _, r_st(job_name: name)) do
    str = :io_lib.format('Testing ~ts: ~ts\n', [name, str0])
    :io.put_chars(str)
  end

  defp do_output(tag, str, phase, r_st(fds: fds) = st) do
    case :gb_trees.lookup(tag, fds) do
      :none when phase != :started ->
        :buffer

      :none ->
        s =
          :io_lib.format('\n*** ERROR: ~w, line ~w: No known \'~tp\' log file\n', [
            :test_server_io,
            404,
            tag
          ])

        do_output(:stdout, [s, str], phase, st)

      {:value, fd} ->
        try do
          :io.put_chars(fd, str)

          case tag do
            :html ->
              finalise_table(fd, st)

            _ ->
              :ok
          end
        catch
          _, error ->
            s =
              :io_lib.format(
                '\n*** ERROR: ~w, line ~w: Error writing to log file \'~tp\': ~tp\n',
                [:test_server_io, 416, tag, error]
              )

            do_output(:stdout, [s, str], phase, st)
        end
    end
  end

  defp finalise_table(fd, r_st(html_footer: footer)) do
    case :file.position(fd, {:cur, 0}) do
      {:ok, pos} ->
        :io.put_chars(fd, ['\n</table>\n', footer])
        :file.position(fd, pos)

      {:error, :epipe} ->
        :ok
    end
  end

  defp do_print_buffered(q0, st) do
    item = :queue.get(q0)
    q = :queue.drop(q0)

    case item do
      :eot ->
        q

      {tag, str} ->
        _ = do_output(tag, str, :undefined, st)
        do_print_buffered(q, st)
    end
  end

  defp gc(r_st(gls: gls0)) do
    inUse0 =
      for p <- :erlang.processes() do
        case :erlang.process_info(p, :group_leader) do
          {:group_leader, gL} ->
            gL

          :undefined ->
            :undefined
        end
      end

    inUse = :ordsets.from_list(inUse0)
    gls = :gb_sets.to_list(gls0)
    notUsed = :ordsets.subtract(gls, inUse)

    _ =
      for pid <- notUsed do
        :test_server_gl.stop(pid)
      end

    :ok
  end
end
