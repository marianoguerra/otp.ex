defmodule :m_disk_log do
  use Bitwise
  require Record

  Record.defrecord(:r_state, :state,
    queue: [],
    messages: [],
    parent: :undefined,
    server: :undefined,
    cnt: 0,
    args: :undefined,
    error_status: :ok,
    cache_error: :ok
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

  Record.defrecord(:r_arg, :arg,
    name: 0,
    version: :undefined,
    file: :none,
    repair: true,
    size: :infinity,
    type: :halt,
    format: :internal,
    linkto: self(),
    head: :none,
    mode: :read_write,
    notify: false,
    quiet: false,
    options: []
  )

  Record.defrecord(:r_cache, :cache, fd: :undefined, sz: 0, c: [])
  Record.defrecord(:r_halt, :halt, fdc: :undefined, curB: :undefined, size: :undefined)

  Record.defrecord(:r_handle, :handle,
    filename: :undefined,
    maxB: :undefined,
    maxF: :undefined,
    curB: :undefined,
    curF: :undefined,
    cur_fdc: :undefined,
    cur_name: :undefined,
    cur_cnt: :undefined,
    acc_cnt: :undefined,
    firstPos: :undefined,
    noFull: :undefined,
    accFull: :undefined
  )

  Record.defrecord(:r_log, :log,
    status: :ok,
    name: :undefined,
    blocked_by: :none,
    users: 0,
    filename: :undefined,
    owners: [],
    type: :undefined,
    format: :undefined,
    format_type: :undefined,
    head: :none,
    mode: :undefined,
    size: :undefined,
    extra: :undefined,
    version: :undefined
  )

  Record.defrecord(:r_continuation, :continuation, pid: self(), pos: :undefined, b: :undefined)

  def open(a) do
    :disk_log_server.open(check_arg(a, r_arg(options: a)))
  end

  def log(log, term) do
    req(
      log,
      {:log, :internal, [:erlang.term_to_binary(term)]}
    )
  end

  def blog(log, bytes) do
    req(log, {:log, :external, [ensure_binary(bytes)]})
  end

  def log_terms(log, terms) do
    bs = terms2bins(terms)
    req(log, {:log, :internal, bs})
  end

  def blog_terms(log, bytess) do
    bs = ensure_binary_list(bytess)
    req(log, {:log, :external, bs})
  end

  def alog(log, term) do
    notify(
      log,
      {:alog, :internal, [:erlang.term_to_binary(term)]}
    )
  end

  def alog_terms(log, terms) do
    bs = terms2bins(terms)
    notify(log, {:alog, :internal, bs})
  end

  def balog(log, bytes) do
    notify(log, {:alog, :external, [ensure_binary(bytes)]})
  end

  def balog_terms(log, bytess) do
    bs = ensure_binary_list(bytess)
    notify(log, {:alog, :external, bs})
  end

  def close(log) do
    req(log, :close)
  end

  def lclose(log) do
    lclose(log, node())
  end

  def lclose(log, node) when node() === node do
    req(log, :close)
  end

  def lclose(_Log, _Node) do
    {:error, :no_such_log}
  end

  def truncate(log) do
    req(log, {:truncate, :none, :truncate, 1})
  end

  def truncate(log, head) do
    req(
      log,
      {:truncate, {:ok, :erlang.term_to_binary(head)}, :truncate, 2}
    )
  end

  def btruncate(log, head) do
    req(
      log,
      {:truncate, {:ok, ensure_binary(head)}, :btruncate, 2}
    )
  end

  def reopen(log, newFile) do
    req(log, {:reopen, newFile, :none, :reopen, 2})
  end

  def reopen(log, newFile, newHead) do
    req(
      log,
      {:reopen, newFile, {:ok, :erlang.term_to_binary(newHead)}, :reopen, 3}
    )
  end

  def breopen(log, newFile, newHead) do
    req(
      log,
      {:reopen, newFile, {:ok, ensure_binary(newHead)}, :breopen, 3}
    )
  end

  def inc_wrap_file(log) do
    req(log, :inc_wrap_file)
  end

  def change_size(log, newSize) do
    req(log, {:change_size, newSize})
  end

  def change_notify(log, pid, newNotify) do
    req(log, {:change_notify, pid, newNotify})
  end

  def change_header(log, newHead) do
    req(log, {:change_header, newHead})
  end

  def sync(log) do
    req(log, :sync)
  end

  def block(log) do
    block(log, true)
  end

  def block(log, queueLogRecords) do
    req(log, {:block, queueLogRecords})
  end

  def unblock(log) do
    req(log, :unblock)
  end

  def format_error(error) do
    do_format_error(error)
  end

  def info(log) do
    req(log, :info)
  end

  def pid2name(pid) do
    :disk_log_server.start()

    case :ets.lookup(:disk_log_pids, pid) do
      [] ->
        :undefined

      [{_Pid, log}] ->
        {:ok, log}
    end
  end

  def chunk(log, cont) do
    chunk(log, cont, :infinity)
  end

  def chunk(log, cont, :infinity) do
    ichunk(log, cont, 65536)
  end

  def chunk(log, cont, n) when is_integer(n) and n > 0 do
    ichunk(log, cont, n)
  end

  defp ichunk(log, :start, n) do
    r = req(log, {:chunk, 0, [], n})
    ichunk_end(r, log)
  end

  defp ichunk(log, more, n)
       when elem(more, 0) === :continuation do
    r =
      req2(
        r_continuation(more, :pid),
        {:chunk, r_continuation(more, :pos), r_continuation(more, :b), n}
      )

    ichunk_end(r, log)
  end

  defp ichunk(_Log, _, _) do
    {:error, {:badarg, :continuation}}
  end

  def ichunk_end({c, r}, log) when elem(c, 0) === :continuation do
    ichunk_end(r, :read_write, log, c, 0)
  end

  def ichunk_end({c, r, bad}, log)
      when elem(c, 0) === :continuation do
    ichunk_end(r, :read_only, log, c, bad)
  end

  def ichunk_end(r, _Log) do
    r
  end

  defp ichunk_end(r, mode, log, c, bad) do
    case (try do
            bins2terms(r, [])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        rR = :lists.reverse(r)
        ichunk_bad_end(rR, mode, log, c, bad, [])

      ts when bad > 0 ->
        {c, ts, bad}

      ts when bad === 0 ->
        {c, ts}
    end
  end

  defp bins2terms([], l) do
    l
  end

  defp bins2terms([b | bs], l) do
    bins2terms(bs, [:erlang.binary_to_term(b) | l])
  end

  defp ichunk_bad_end([b | bs], mode, log, c, bad, a) do
    case (try do
            :erlang.binary_to_term(b)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} when :read_write === mode ->
        infoList = info(log)
        {:value, {:file, fileName}} = :lists.keysearch(:file, 1, infoList)

        file =
          case r_continuation(c, :pos) do
            pos when is_integer(pos) ->
              fileName

            {fileNo, _} ->
              add_ext(fileName, fileNo)
          end

        {:error, {:corrupt_log_file, file}}

      {:EXIT, _} when :read_only === mode ->
        reread =
          :lists.foldl(
            fn bin, sz ->
              sz + byte_size(bin) + 8
            end,
            0,
            bs
          )

        newPos =
          case r_continuation(c, :pos) do
            pos when is_integer(pos) ->
              pos - reread

            {fileNo, pos} ->
              {fileNo, pos - reread}
          end

        newBad = bad + byte_size(b)
        {r_continuation(c, pos: newPos, b: []), :lists.reverse(a), newBad}

      t ->
        ichunk_bad_end(bs, mode, log, c, bad, [t | a])
    end
  end

  def bchunk(log, cont) do
    bchunk(log, cont, :infinity)
  end

  def bchunk(log, cont, :infinity) do
    bichunk(log, cont, 65536)
  end

  def bchunk(log, cont, n) when is_integer(n) and n > 0 do
    bichunk(log, cont, n)
  end

  defp bichunk(log, :start, n) do
    r = req(log, {:chunk, 0, [], n})
    bichunk_end(r)
  end

  defp bichunk(_Log, r_continuation(pid: pid, pos: pos, b: b), n) do
    r = req2(pid, {:chunk, pos, b, n})
    bichunk_end(r)
  end

  defp bichunk(_Log, _, _) do
    {:error, {:badarg, :continuation}}
  end

  defp bichunk_end({c = r_continuation(), r}) do
    {c, :lists.reverse(r)}
  end

  defp bichunk_end({c = r_continuation(), r, bad}) do
    {c, :lists.reverse(r), bad}
  end

  defp bichunk_end(r) do
    r
  end

  def chunk_step(log, cont, n) when is_integer(n) do
    ichunk_step(log, cont, n)
  end

  defp ichunk_step(log, :start, n) do
    req(log, {:chunk_step, 0, n})
  end

  defp ichunk_step(_Log, more, n)
       when elem(more, 0) === :continuation do
    req2(r_continuation(more, :pid), {:chunk_step, r_continuation(more, :pos), n})
  end

  defp ichunk_step(_Log, _, _) do
    {:error, {:badarg, :continuation}}
  end

  def chunk_info(more = r_continuation()) do
    [{:node, node(r_continuation(more, :pid))}]
  end

  def chunk_info(badCont) do
    {:error, {:no_continuation, badCont}}
  end

  def accessible_logs() do
    {:disk_log_server.all(), []}
  end

  def all() do
    :disk_log_server.all()
  end

  def istart_link(server) do
    {:ok, :proc_lib.spawn_link(:disk_log, :init, [self(), server])}
  end

  def start() do
    :disk_log_server.start()
  end

  def internal_open(pid, a) do
    req2(pid, {:internal_open, a})
  end

  def ll_open(a) do
    case check_arg(a, r_arg(options: a)) do
      {:ok, l} ->
        do_open(l)

      error ->
        error
    end
  end

  def ll_close(log) do
    close_disk_log2(log)
  end

  defp check_arg([], res) do
    ret =
      case r_arg(res, :head) do
        :none ->
          {:ok, res}

        _ ->
          case check_head(r_arg(res, :head), r_arg(res, :format)) do
            {:ok, head} ->
              {:ok, r_arg(res, head: head)}

            error ->
              error
          end
      end

    cond do
      r_arg(res, :name) === 0 ->
        {:error, {:badarg, :name}}

      r_arg(res, :file) === :none ->
        case (try do
                :lists.concat([r_arg(res, :name), '.LOG'])
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            {:error, {:badarg, :file}}

          fName ->
            check_arg([], r_arg(res, file: fName))
        end

      r_arg(res, :repair) === :truncate and
          r_arg(res, :mode) === :read_only ->
        {:error, {:badarg, :repair_read_only}}

      r_arg(res, :type) === :halt and is_tuple(r_arg(res, :size)) ->
        {:error, {:badarg, :size}}

      r_arg(res, :type) === :wrap ->
        {oldSize, version} = :disk_log_1.read_size_file_version(r_arg(res, :file))
        check_wrap_arg(ret, oldSize, version)

      true ->
        ret
    end
  end

  defp check_arg([{:file, f} | tail], res) when is_list(f) do
    check_arg(tail, r_arg(res, file: f))
  end

  defp check_arg([{:file, f} | tail], res) when is_atom(f) do
    check_arg(tail, r_arg(res, file: f))
  end

  defp check_arg([{:linkto, pid} | tail], res)
       when is_pid(pid) do
    check_arg(tail, r_arg(res, linkto: pid))
  end

  defp check_arg([{:linkto, :none} | tail], res) do
    check_arg(tail, r_arg(res, linkto: :none))
  end

  defp check_arg([{:name, name} | tail], res) do
    check_arg(tail, r_arg(res, name: name))
  end

  defp check_arg([{:repair, true} | tail], res) do
    check_arg(tail, r_arg(res, repair: true))
  end

  defp check_arg([{:repair, false} | tail], res) do
    check_arg(tail, r_arg(res, repair: false))
  end

  defp check_arg([{:repair, :truncate} | tail], res) do
    check_arg(tail, r_arg(res, repair: :truncate))
  end

  defp check_arg([{:size, int} | tail], res)
       when is_integer(int) and int > 0 do
    check_arg(tail, r_arg(res, size: int))
  end

  defp check_arg([{:size, :infinity} | tail], res) do
    check_arg(tail, r_arg(res, size: :infinity))
  end

  defp check_arg([{:size, {maxB, maxF}} | tail], res)
       when is_integer(maxB) and is_integer(maxF) and
              maxB > 0 and maxB <= 1 <<< (64 - 1) and maxF > 0 and
              maxF < 65000 do
    check_arg(tail, r_arg(res, size: {maxB, maxF}))
  end

  defp check_arg([{:type, :wrap} | tail], res) do
    check_arg(tail, r_arg(res, type: :wrap))
  end

  defp check_arg([{:type, :halt} | tail], res) do
    check_arg(tail, r_arg(res, type: :halt))
  end

  defp check_arg([{:format, :internal} | tail], res) do
    check_arg(tail, r_arg(res, format: :internal))
  end

  defp check_arg([{:format, :external} | tail], res) do
    check_arg(tail, r_arg(res, format: :external))
  end

  defp check_arg([{:notify, true} | tail], res) do
    check_arg(tail, r_arg(res, notify: true))
  end

  defp check_arg([{:notify, false} | tail], res) do
    check_arg(tail, r_arg(res, notify: false))
  end

  defp check_arg([{:head_func, headFunc} | tail], res) do
    check_arg(tail, r_arg(res, head: {:head_func, headFunc}))
  end

  defp check_arg([{:head, term} | tail], res) do
    check_arg(tail, r_arg(res, head: {:head, term}))
  end

  defp check_arg([{:mode, :read_only} | tail], res) do
    check_arg(tail, r_arg(res, mode: :read_only))
  end

  defp check_arg([{:mode, :read_write} | tail], res) do
    check_arg(tail, r_arg(res, mode: :read_write))
  end

  defp check_arg([{:quiet, boolean} | tail], res)
       when is_boolean(boolean) do
    check_arg(tail, r_arg(res, quiet: boolean))
  end

  defp check_arg(arg, _) do
    {:error, {:badarg, arg}}
  end

  defp check_wrap_arg({:ok, res}, {0, 0}, _Version)
       when r_arg(res, :size) === :infinity do
    {:error, {:badarg, :size}}
  end

  defp check_wrap_arg({:ok, res}, oldSize, version)
       when r_arg(res, :size) === :infinity do
    newRes = r_arg(res, size: oldSize)
    check_wrap_arg({:ok, newRes}, oldSize, version)
  end

  defp check_wrap_arg({:ok, res}, {0, 0}, version) do
    {:ok, r_arg(res, version: version)}
  end

  defp check_wrap_arg({:ok, res}, oldSize, version)
       when oldSize === r_arg(res, :size) do
    {:ok, r_arg(res, version: version)}
  end

  defp check_wrap_arg({:ok, res}, _OldSize, version)
       when r_arg(res, :repair) === :truncate and
              is_tuple(r_arg(res, :size)) do
    {:ok, r_arg(res, version: version)}
  end

  defp check_wrap_arg({:ok, res}, oldSize, _Version)
       when is_tuple(r_arg(res, :size)) do
    {:error, {:size_mismatch, oldSize, r_arg(res, :size)}}
  end

  defp check_wrap_arg({:ok, _Res}, _OldSize, _Version) do
    {:error, {:badarg, :size}}
  end

  defp check_wrap_arg(ret, _OldSize, _Version) do
    ret
  end

  def init(parent, server) do
    :void
    :erlang.process_flag(:trap_exit, true)
    loop(r_state(parent: parent, server: server))
  end

  defp loop(r_state(messages: []) = state) do
    receive do
      message ->
        handle(message, state)
    end
  end

  defp loop(r_state(messages: [m | ms]) = state) do
    handle(m, r_state(state, messages: ms))
  end

  defp handle({from, :write_cache}, s) when from === self() do
    case (try do
            do_write_cache(:erlang.get(:log))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :ok ->
        loop(s)

      error ->
        loop(r_state(s, cache_error: error))
    end
  end

  defp handle({from, {:log, format, b}} = message, s) do
    case :erlang.get(:log) do
      r_log(mode: :read_only) = l ->
        reply(from, {:error, {:read_only_mode, r_log(l, :name)}}, s)

      r_log(status: :ok, format: :external) = l
      when format === :internal ->
        reply(from, {:error, {:format_external, r_log(l, :name)}}, s)

      r_log(status: :ok, format: logFormat) ->
        log_loop(s, from, [b], [], :erlang.iolist_size(b), logFormat)

      r_log(status: {:blocked, false}) = l ->
        reply(from, {:error, {:blocked_log, r_log(l, :name)}}, s)

      r_log(blocked_by: ^from) = l ->
        reply(from, {:error, {:blocked_log, r_log(l, :name)}}, s)

      _ ->
        enqueue(message, s)
    end
  end

  defp handle({:alog, format, b} = message, s) do
    case :erlang.get(:log) do
      r_log(mode: :read_only) ->
        notify_owners({:read_only, b})
        loop(s)

      r_log(status: :ok, format: :external)
      when format === :internal ->
        notify_owners({:format_external, b})
        loop(s)

      r_log(status: :ok, format: logFormat) ->
        log_loop(s, [], [b], [], :erlang.iolist_size(b), logFormat)

      r_log(status: {:blocked, false}) ->
        notify_owners({:blocked_log, b})
        loop(s)

      _ ->
        enqueue(message, s)
    end
  end

  defp handle({from, {:block, queueLogRecs}} = message, s) do
    case :erlang.get(:log) do
      r_log(status: :ok) = l ->
        do_block(from, queueLogRecs, l)
        reply(from, :ok, s)

      r_log(status: {:blocked, false}) = l ->
        reply(from, {:error, {:blocked_log, r_log(l, :name)}}, s)

      r_log(blocked_by: ^from) = l ->
        reply(from, {:error, {:blocked_log, r_log(l, :name)}}, s)

      _ ->
        enqueue(message, s)
    end
  end

  defp handle({from, :unblock}, s) do
    case :erlang.get(:log) do
      r_log(status: :ok) = l ->
        reply(from, {:error, {:not_blocked, r_log(l, :name)}}, s)

      r_log(blocked_by: ^from) = l ->
        s2 = do_unblock(l, s)
        reply(from, :ok, s2)

      l ->
        reply(from, {:error, {:not_blocked_by_pid, r_log(l, :name)}}, s)
    end
  end

  defp handle({from, :sync} = message, s) do
    case :erlang.get(:log) do
      r_log(mode: :read_only) = l ->
        reply(from, {:error, {:read_only_mode, r_log(l, :name)}}, s)

      r_log(status: :ok, format: logFormat) ->
        log_loop(s, [], [], [from], 0, logFormat)

      r_log(status: {:blocked, false}) = l ->
        reply(from, {:error, {:blocked_log, r_log(l, :name)}}, s)

      r_log(blocked_by: ^from) = l ->
        reply(from, {:error, {:blocked_log, r_log(l, :name)}}, s)

      _ ->
        enqueue(message, s)
    end
  end

  defp handle({from, {:truncate, head, f, a}} = message, s) do
    case :erlang.get(:log) do
      r_log(mode: :read_only) = l ->
        reply(from, {:error, {:read_only_mode, r_log(l, :name)}}, s)

      r_log(status: :ok) when r_state(s, :cache_error) !== :ok ->
        loop(cache_error(s, [from]))

      r_log(status: :ok) = l ->
        h = merge_head(head, r_log(l, :head))

        case (try do
                do_trunc(l, h)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          :ok ->
            :erlang.erase(:is_full)
            notify_owners({:truncated, r_state(s, :cnt)})

            n =
              cond do
                h === :none ->
                  0

                true ->
                  1
              end

            reply(from, :ok, r_state(state_ok(s), cnt: n))

          error ->
            do_exit(s, from, error, {{:failed, error}, [{:disk_log, f, a}]})
        end

      r_log(status: {:blocked, false}) = l ->
        reply(from, {:error, {:blocked_log, r_log(l, :name)}}, s)

      r_log(blocked_by: ^from) = l ->
        reply(from, {:error, {:blocked_log, r_log(l, :name)}}, s)

      _ ->
        enqueue(message, s)
    end
  end

  defp handle({from, {:chunk, pos, b, n}} = message, s) do
    case :erlang.get(:log) do
      r_log(status: :ok) when r_state(s, :cache_error) !== :ok ->
        loop(cache_error(s, [from]))

      r_log(status: :ok) = l ->
        r = do_chunk(l, pos, b, n)
        reply(from, r, s)

      r_log(blocked_by: ^from) = l ->
        r = do_chunk(l, pos, b, n)
        reply(from, r, s)

      r_log(status: {:blocked, false}) = l ->
        reply(from, {:error, {:blocked_log, r_log(l, :name)}}, s)

      _L ->
        enqueue(message, s)
    end
  end

  defp handle({from, {:chunk_step, pos, n}} = message, s) do
    case :erlang.get(:log) do
      r_log(status: :ok) when r_state(s, :cache_error) !== :ok ->
        loop(cache_error(s, [from]))

      r_log(status: :ok) = l ->
        r = do_chunk_step(l, pos, n)
        reply(from, r, s)

      r_log(blocked_by: ^from) = l ->
        r = do_chunk_step(l, pos, n)
        reply(from, r, s)

      r_log(status: {:blocked, false}) = l ->
        reply(from, {:error, {:blocked_log, r_log(l, :name)}}, s)

      _ ->
        enqueue(message, s)
    end
  end

  defp handle(
         {from, {:change_notify, pid, newNotify}} = message,
         s
       ) do
    case :erlang.get(:log) do
      r_log(status: :ok) = l ->
        case do_change_notify(l, pid, newNotify) do
          {:ok, l1} ->
            :erlang.put(:log, l1)
            reply(from, :ok, s)

          error ->
            reply(from, error, s)
        end

      r_log(status: {:blocked, false}) = l ->
        reply(from, {:error, {:blocked_log, r_log(l, :name)}}, s)

      r_log(blocked_by: ^from) = l ->
        reply(from, {:error, {:blocked_log, r_log(l, :name)}}, s)

      _ ->
        enqueue(message, s)
    end
  end

  defp handle(
         {from, {:change_header, newHead}} = message,
         s
       ) do
    case :erlang.get(:log) do
      r_log(mode: :read_only) = l ->
        reply(from, {:error, {:read_only_mode, r_log(l, :name)}}, s)

      r_log(status: :ok, format: format) = l ->
        case check_head(newHead, format) do
          {:ok, head} ->
            :erlang.put(:log, r_log(l, head: mk_head(head, format)))
            reply(from, :ok, s)

          error ->
            reply(from, error, s)
        end

      r_log(status: {:blocked, false}) = l ->
        reply(from, {:error, {:blocked_log, r_log(l, :name)}}, s)

      r_log(blocked_by: ^from) = l ->
        reply(from, {:error, {:blocked_log, r_log(l, :name)}}, s)

      _ ->
        enqueue(message, s)
    end
  end

  defp handle({from, {:change_size, newSize}} = message, s) do
    case :erlang.get(:log) do
      r_log(mode: :read_only) = l ->
        reply(from, {:error, {:read_only_mode, r_log(l, :name)}}, s)

      r_log(status: :ok) = l ->
        case check_size(r_log(l, :type), newSize) do
          :ok ->
            case (try do
                    do_change_size(l, newSize)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              :ok ->
                reply(from, :ok, s)

              {:big, curSize} ->
                reply(
                  from,
                  {:error, {:new_size_too_small, r_log(l, :name), curSize}},
                  s
                )

              else__ ->
                reply(from, else__, state_err(s, else__))
            end

          :not_ok ->
            reply(from, {:error, {:badarg, :size}}, s)
        end

      r_log(status: {:blocked, false}) = l ->
        reply(from, {:error, {:blocked_log, r_log(l, :name)}}, s)

      r_log(blocked_by: ^from) = l ->
        reply(from, {:error, {:blocked_log, r_log(l, :name)}}, s)

      _ ->
        enqueue(message, s)
    end
  end

  defp handle({from, :inc_wrap_file} = message, s) do
    case :erlang.get(:log) do
      r_log(mode: :read_only) = l ->
        reply(from, {:error, {:read_only_mode, r_log(l, :name)}}, s)

      r_log(type: :halt) = l ->
        reply(from, {:error, {:halt_log, r_log(l, :name)}}, s)

      r_log(status: :ok) when r_state(s, :cache_error) !== :ok ->
        loop(cache_error(s, [from]))

      r_log(status: :ok) = l ->
        case (try do
                do_inc_wrap_file(l)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:ok, l2, lost} ->
            :erlang.put(:log, l2)
            notify_owners({:wrap, lost})
            reply(from, :ok, r_state(s, cnt: r_state(s, :cnt) - lost))

          {:error, error, l2} ->
            :erlang.put(:log, l2)
            reply(from, error, state_err(s, error))
        end

      r_log(status: {:blocked, false}) = l ->
        reply(from, {:error, {:blocked_log, r_log(l, :name)}}, s)

      r_log(blocked_by: ^from) = l ->
        reply(from, {:error, {:blocked_log, r_log(l, :name)}}, s)

      _ ->
        enqueue(message, s)
    end
  end

  defp handle({from, {:reopen, newFile, head, f, a}}, s) do
    case :erlang.get(:log) do
      r_log(mode: :read_only) = l ->
        reply(from, {:error, {:read_only_mode, r_log(l, :name)}}, s)

      r_log(status: :ok) when r_state(s, :cache_error) !== :ok ->
        loop(cache_error(s, [from]))

      r_log(status: :ok, filename: ^newFile) = l ->
        reply(from, {:error, {:same_file_name, r_log(l, :name)}}, s)

      r_log(status: :ok) = l ->
        case (try do
                close_disk_log2(l)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          :closed ->
            file = r_log(l, :filename)

            case (try do
                    rename_file(file, newFile, r_log(l, :type))
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              :ok ->
                h = merge_head(head, r_log(l, :head))

                case do_open(
                       r_arg(r_state(s, :args),
                         name: r_log(l, :name),
                         repair: :truncate,
                         head: h,
                         file: file
                       )
                     ) do
                  {:ok, res, l2, cnt} ->
                    :erlang.put(
                      :log,
                      r_log(l2,
                        owners: r_log(l, :owners),
                        head: r_log(l, :head),
                        users: r_log(l, :users)
                      )
                    )

                    notify_owners({:truncated, r_state(s, :cnt)})
                    :erlang.erase(:is_full)

                    case res do
                      {:error, _} ->
                        do_exit(s, from, res, {{:failed, res}, [{:disk_log, f, a}]})

                      _ ->
                        reply(from, :ok, r_state(s, cnt: cnt))
                    end

                  res ->
                    do_exit(s, from, res, {{:failed, res}, [{:disk_log, f, a}]})
                end

              error ->
                do_exit(s, from, error, {{:failed, error}, [{:disk_log, :reopen, 2}]})
            end

          error ->
            do_exit(s, from, error, {{:failed, error}, [{:disk_log, f, a}]})
        end

      l ->
        reply(from, {:error, {:blocked_log, r_log(l, :name)}}, s)
    end
  end

  defp handle({server, {:internal_open, a}}, s) do
    case :erlang.get(:log) do
      :undefined ->
        case do_open(a) do
          {:ok, res, l, cnt} ->
            :erlang.put(
              :log,
              opening_pid(r_arg(a, :linkto), r_arg(a, :notify), l)
            )

            reply(server, res, r_state(s, args: a, cnt: cnt))

          res ->
            do_fast_exit(s, server, res)
        end

      l ->
        testH = mk_head(r_arg(a, :head), r_arg(a, :format))

        case compare_arg(r_arg(a, :options), r_state(s, :args), testH, r_log(l, :head)) do
          :ok ->
            case add_pid(r_arg(a, :linkto), r_arg(a, :notify), l) do
              {:ok, l1} ->
                :erlang.put(:log, l1)
                reply(server, {:ok, r_log(l, :name)}, s)

              error ->
                reply(server, error, s)
            end

          error ->
            reply(server, error, s)
        end
    end
  end

  defp handle({from, :close}, s) do
    case do_close(from, s) do
      {:stop, s1} ->
        do_exit(s1, from, :ok, :normal)

      {:continue, s1} ->
        reply(from, :ok, s1)
    end
  end

  defp handle({from, :info}, s) do
    reply(from, do_info(:erlang.get(:log), r_state(s, :cnt)), s)
  end

  defp handle({:EXIT, from, reason}, r_state(parent: from) = s) do
    _ = do_stop(s)
    exit(reason)
  end

  defp handle({:EXIT, from, reason}, r_state(server: from) = s) do
    _ = do_stop(s)
    exit(reason)
  end

  defp handle({:EXIT, from, _Reason}, s) do
    l = :erlang.get(:log)

    case is_owner(from, l) do
      {true, _Notify} ->
        case close_owner(from, l, s) do
          {:stop, s1} ->
            _ = do_stop(s1)
            exit(:normal)

          {:continue, s1} ->
            loop(s1)
        end

      false ->
        s1 = do_unblock(from, :erlang.get(:log), s)
        loop(s1)
    end
  end

  defp handle({:system, from, req}, s) do
    :sys.handle_system_msg(req, from, r_state(s, :parent), :disk_log, [], s)
  end

  defp handle(_, s) do
    loop(s)
  end

  defp enqueue(message, r_state(queue: queue) = s) do
    loop(r_state(s, queue: [message | queue]))
  end

  defp log_loop(r_state(cache_error: cE) = s, pids, _Bins, _Sync, _Sz, _F)
       when cE !== :ok do
    loop(cache_error(s, pids))
  end

  defp log_loop(r_state() = s, pids, bins, sync, sz, _F)
       when sz > 64 * 1024 do
    loop(log_end(s, pids, bins, sync, sz))
  end

  defp log_loop(r_state(messages: []) = s, pids, bins, sync, sz, f) do
    receive do
      message ->
        log_loop(message, pids, bins, sync, sz, f, s)
    after
      0 ->
        loop(log_end(s, pids, bins, sync, sz))
    end
  end

  defp log_loop(r_state(messages: [m | ms]) = s, pids, bins, sync, sz, f) do
    s1 = r_state(s, messages: ms)
    log_loop(m, pids, bins, sync, sz, f, s1)
  end

  defp log_loop({:alog, :internal, b}, pids, bins, sync, sz, :internal = f, s) do
    log_loop(s, pids, [b | bins], sync, sz + :erlang.iolist_size(b), f)
  end

  defp log_loop({:alog, :binary, b}, pids, bins, sync, sz, f, s) do
    log_loop(s, pids, [b | bins], sync, sz + :erlang.iolist_size(b), f)
  end

  defp log_loop({from, {:log, :internal, b}}, pids, bins, sync, sz, :internal = f, s) do
    log_loop(s, [from | pids], [b | bins], sync, sz + :erlang.iolist_size(b), f)
  end

  defp log_loop({from, {:log, :binary, b}}, pids, bins, sync, sz, f, s) do
    log_loop(s, [from | pids], [b | bins], sync, sz + :erlang.iolist_size(b), f)
  end

  defp log_loop({from, :sync}, pids, bins, sync, sz, f, s) do
    log_loop(s, pids, bins, [from | sync], sz, f)
  end

  defp log_loop(message, pids, bins, sync, sz, _F, s) do
    nS = log_end(s, pids, bins, sync, sz)
    handle(message, nS)
  end

  defp log_end(s, [], [], sync, _Sz) do
    log_end_sync(s, sync)
  end

  defp log_end(r_state(cnt: cnt) = s, pids, bins, sync, sz) do
    case do_log(:erlang.get(:log), rflat(bins), sz) do
      n when is_integer(n) ->
        :ok = replies(pids, :ok)
        s1 = r_state(state_ok(s), cnt: cnt + n)
        log_end_sync(s1, sync)

      {:error, {:error, {:full, _Name}}, n} when pids === [] ->
        log_end_sync(state_ok(r_state(s, cnt: cnt + n)), sync)

      {:error, error, n} ->
        :ok = replies(pids, error)
        state_err(r_state(s, cnt: cnt + n), error)
    end
  end

  defp log_end_sync(s, []) do
    s
  end

  defp log_end_sync(s, sync) do
    res = do_sync(:erlang.get(:log))
    :ok = replies(sync, res)
    state_err(s, res)
  end

  defp rflat([b]) do
    b
  end

  defp rflat(b) do
    rflat(b, [])
  end

  defp rflat([b | bs], l) do
    rflat(bs, b ++ l)
  end

  defp rflat([], l) do
    l
  end

  defp do_change_notify(l, pid, notify) do
    case is_owner(pid, l) do
      {true, ^notify} ->
        {:ok, l}

      {true, _OldNotify}
      when notify !== true and
             notify !== false ->
        {:error, {:badarg, :notify}}

      {true, _OldNotify} ->
        owners = :lists.keydelete(pid, 1, r_log(l, :owners))
        l1 = r_log(l, owners: [{pid, notify} | owners])
        {:ok, l1}

      false ->
        {:error, {:not_owner, pid}}
    end
  end

  defp do_close(pid, s) do
    l = :erlang.get(:log)

    case is_owner(pid, l) do
      {true, _Notify} ->
        close_owner(pid, l, s)

      false ->
        close_user(pid, l, s)
    end
  end

  defp close_owner(pid, l, s) do
    l1 = r_log(l, owners: :lists.keydelete(pid, 1, r_log(l, :owners)))
    :erlang.put(:log, l1)
    s2 = do_unblock(pid, :erlang.get(:log), s)
    :erlang.unlink(pid)
    do_close2(l1, s2)
  end

  defp close_user(pid, r_log(users: users) = l, s) when users > 0 do
    l1 = r_log(l, users: users - 1)
    :erlang.put(:log, l1)
    s2 = do_unblock(pid, :erlang.get(:log), s)
    do_close2(l1, s2)
  end

  defp close_user(_Pid, _L, s) do
    {:continue, s}
  end

  defp do_close2(r_log(users: 0, owners: []), s) do
    {:stop, s}
  end

  defp do_close2(_L, s) do
    {:continue, s}
  end

  def system_continue(_Parent, _, state) do
    loop(state)
  end

  def system_terminate(reason, _Parent, _, state) do
    _ = do_stop(state)
    exit(reason)
  end

  def system_code_change(state, _Module, _OldVsn, _Extra) do
    {:ok, state}
  end

  defp do_exit(s, from, message0, reason) do
    r = do_stop(s)

    message =
      case r_state(s, :cache_error) do
        err when err !== :ok ->
          err

        _ when r === :closed ->
          message0

        _ when message0 === :ok ->
          r

        _ ->
          message0
      end

    _ = :disk_log_server.close(self())
    :ok = replies(from, message)
    :void
    exit(reason)
  end

  defp do_fast_exit(s, server, message) do
    _ = do_stop(s)
    send(server, {:disk_log, self(), message})
    exit(:normal)
  end

  defp do_stop(s) do
    proc_q(r_state(s, :queue) ++ r_state(s, :messages))
    close_disk_log(:erlang.get(:log))
  end

  defp proc_q([{from, _R} | tail]) when is_pid(from) do
    send(from, {:disk_log, self(), {:error, :disk_log_stopped}})
    proc_q(tail)
  end

  defp proc_q([_ | t]) do
    proc_q(t)
  end

  defp proc_q([]) do
    :ok
  end

  defp opening_pid(pid, notify, l) do
    {:ok, l1} = add_pid(pid, notify, l)
    l1
  end

  defp add_pid(pid, notify, l) when is_pid(pid) do
    case is_owner(pid, l) do
      false ->
        :erlang.link(pid)
        {:ok, r_log(l, owners: [{pid, notify} | r_log(l, :owners)])}

      {true, ^notify} ->
        {:ok, l}

      {true, curNotify} when notify !== curNotify ->
        {:error, {:arg_mismatch, :notify, curNotify, notify}}
    end
  end

  defp add_pid(_NotAPid, _Notify, l) do
    {:ok, r_log(l, users: r_log(l, :users) + 1)}
  end

  defp unblock_pid(r_log(blocked_by: :none)) do
    :ok
  end

  defp unblock_pid(r_log(blocked_by: pid) = l) do
    case is_owner(pid, l) do
      {true, _Notify} ->
        :ok

      false ->
        :erlang.unlink(pid)
    end
  end

  defp is_owner(pid, l) do
    case :lists.keysearch(pid, 1, r_log(l, :owners)) do
      {:value, {_Pid, notify}} ->
        {true, notify}

      false ->
        false
    end
  end

  defp rename_file(file, newFile, :halt) do
    case :file.rename(file, newFile) do
      :ok ->
        :ok

      else__ ->
        file_error(newFile, else__)
    end
  end

  defp rename_file(file, newFile, :wrap) do
    rename_file(wrap_file_extensions(file), file, newFile, :ok)
  end

  defp rename_file([ext | exts], file, newFile0, res) do
    newFile = add_ext(newFile0, ext)

    nRes =
      case :file.rename(
             add_ext(file, ext),
             newFile
           ) do
        :ok ->
          res

        else__ ->
          file_error(newFile, else__)
      end

    rename_file(exts, file, newFile0, nRes)
  end

  defp rename_file([], _File, _NewFiles, res) do
    res
  end

  defp file_error(fileName, {:error, error}) do
    {:error, {:file_error, fileName, error}}
  end

  defp compare_arg([], _A, :none, _OrigHead) do
    :ok
  end

  defp compare_arg([], _A, head, origHead)
       when head !== origHead do
    {:error, {:arg_mismatch, :head, origHead, head}}
  end

  defp compare_arg([], _A, _Head, _OrigHead) do
    :ok
  end

  defp compare_arg([{attr, val} | tail], a, head, origHead) do
    case compare_arg(attr, val, a) do
      {:not_ok, origVal} ->
        {:error, {:arg_mismatch, attr, origVal, val}}

      :ok ->
        compare_arg(tail, a, head, origHead)

      error ->
        error
    end
  end

  defp compare_arg(:file, f, a) when f !== r_arg(a, :file) do
    {:error, {:name_already_open, r_arg(a, :name)}}
  end

  defp compare_arg(:mode, :read_only, a)
       when r_arg(a, :mode) === :read_write do
    {:error, {:open_read_write, r_arg(a, :name)}}
  end

  defp compare_arg(:mode, :read_write, a)
       when r_arg(a, :mode) === :read_only do
    {:error, {:open_read_only, r_arg(a, :name)}}
  end

  defp compare_arg(:type, t, a) when t !== r_arg(a, :type) do
    {:not_ok, r_arg(a, :type)}
  end

  defp compare_arg(:format, f, a) when f !== r_arg(a, :format) do
    {:not_ok, r_arg(a, :format)}
  end

  defp compare_arg(:repair, r, a) when r !== r_arg(a, :repair) do
    {:not_ok, r_arg(a, :repair)}
  end

  defp compare_arg(_Attr, _Val, _A) do
    :ok
  end

  defp do_open(a) do
    r_arg(
      type: type,
      format: format,
      name: name,
      head: head0,
      file: fName,
      repair: repair,
      size: size,
      mode: mode,
      quiet: quiet,
      version: v
    ) = a

    :disk_log_1.set_quiet(quiet)
    head = mk_head(head0, format)

    case do_open2(type, format, name, fName, repair, size, mode, head, v) do
      {:ok, ret, extra, formatType, noItems} ->
        l =
          r_log(
            name: name,
            type: type,
            format: format,
            filename: fName,
            size: size,
            format_type: formatType,
            head: head,
            mode: mode,
            version: v,
            extra: extra
          )

        {:ok, ret, l, noItems}

      error ->
        error
    end
  end

  defp mk_head({:head, term}, :internal) do
    {:ok, :erlang.term_to_binary(term)}
  end

  defp mk_head({:head, bytes}, :external) do
    {:ok, ensure_binary(bytes)}
  end

  defp mk_head(h, _) do
    h
  end

  defp terms2bins([t | ts]) do
    [:erlang.term_to_binary(t) | terms2bins(ts)]
  end

  defp terms2bins([]) do
    []
  end

  defp ensure_binary_list(bs) do
    ensure_binary_list(bs, bs)
  end

  defp ensure_binary_list([b | bs], bs0) when is_binary(b) do
    ensure_binary_list(bs, bs0)
  end

  defp ensure_binary_list([], bs0) do
    bs0
  end

  defp ensure_binary_list(_, bs0) do
    make_binary_list(bs0)
  end

  defp make_binary_list([b | bs]) do
    [ensure_binary(b) | make_binary_list(bs)]
  end

  defp make_binary_list([]) do
    []
  end

  defp ensure_binary(bytes) do
    :erlang.iolist_to_binary(bytes)
  end

  defp do_change_size(r_log(type: :halt) = l, newSize) do
    halt = r_log(l, :extra)
    curB = r_halt(halt, :curB)
    newLog = r_log(l, extra: r_halt(halt, size: newSize))

    cond do
      newSize === :infinity ->
        :erlang.erase(:is_full)
        :erlang.put(:log, newLog)
        :ok

      curB <= newSize ->
        :erlang.erase(:is_full)
        :erlang.put(:log, newLog)
        :ok

      true ->
        {:big, curB}
    end
  end

  defp do_change_size(r_log(type: :wrap) = l, newSize) do
    r_log(extra: extra, version: version) = l
    {:ok, handle} = :disk_log_1.change_size_wrap(extra, newSize, version)
    :erlang.erase(:is_full)
    :erlang.put(:log, r_log(l, extra: handle))
    :ok
  end

  defp check_head({:head, :none}, _Format) do
    {:ok, :none}
  end

  defp check_head({:head_func, {m, f, a}}, _Format)
       when is_atom(m) and is_atom(f) and is_list(a) do
    {:ok, {m, f, a}}
  end

  defp check_head({:head, head}, :external) do
    case (try do
            ensure_binary(head)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:error, {:badarg, :head}}

      _ ->
        {:ok, {:head, head}}
    end
  end

  defp check_head({:head, term}, :internal) do
    {:ok, {:head, term}}
  end

  defp check_head(_Head, _Format) do
    {:error, {:badarg, :head}}
  end

  defp check_size(:wrap, {newMaxB, newMaxF})
       when is_integer(newMaxB) and is_integer(newMaxF) and
              newMaxB > 0 and newMaxB <= 1 <<< (64 - 1) and
              newMaxF > 0 and newMaxF < 65000 do
    :ok
  end

  defp check_size(:halt, newSize)
       when is_integer(newSize) and
              newSize > 0 do
    :ok
  end

  defp check_size(:halt, :infinity) do
    :ok
  end

  defp check_size(_, _) do
    :not_ok
  end

  defp do_inc_wrap_file(l) do
    r_log(format: format, extra: handle) = l

    case format do
      :internal ->
        case :disk_log_1.mf_int_inc(handle, r_log(l, :head)) do
          {:ok, handle2, lost} ->
            {:ok, r_log(l, extra: handle2), lost}

          {:error, error, handle2} ->
            {:error, error, r_log(l, extra: handle2)}
        end

      :external ->
        case :disk_log_1.mf_ext_inc(handle, r_log(l, :head)) do
          {:ok, handle2, lost} ->
            {:ok, r_log(l, extra: handle2), lost}

          {:error, error, handle2} ->
            {:error, error, r_log(l, extra: handle2)}
        end
    end
  end

  defp do_open2(:halt, :internal, name, fName, repair, size, mode, head, _V) do
    case (try do
            :disk_log_1.int_open(fName, repair, mode, head)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, {_Alloc, fdC, {noItems, _NoBytes}, fileSize}} ->
        halt = r_halt(fdc: fdC, curB: fileSize, size: size)
        {:ok, {:ok, name}, halt, :halt_int, noItems}

      {:repaired, fdC, rec, bad, fileSize} ->
        halt = r_halt(fdc: fdC, curB: fileSize, size: size)
        {:ok, {:repaired, name, {:recovered, rec}, {:badbytes, bad}}, halt, :halt_int, rec}

      error ->
        error
    end
  end

  defp do_open2(:wrap, :internal, name, fName, repair, size, mode, head, v) do
    {maxB, maxF} = size

    case (try do
            :disk_log_1.mf_int_open(fName, maxB, maxF, repair, mode, head, v)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, handle, cnt} ->
        {:ok, {:ok, name}, handle, :wrap_int, cnt}

      {:repaired, handle, rec, bad, cnt} ->
        {:ok, {:repaired, name, {:recovered, rec}, {:badbytes, bad}}, handle, :wrap_int, cnt}

      error ->
        error
    end
  end

  defp do_open2(:halt, :external, name, fName, repair, size, mode, head, _V) do
    case (try do
            :disk_log_1.ext_open(fName, repair, mode, head)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, {_Alloc, fdC, {noItems, _NoBytes}, fileSize}} ->
        halt = r_halt(fdc: fdC, curB: fileSize, size: size)
        {:ok, {:ok, name}, halt, :halt_ext, noItems}

      error ->
        error
    end
  end

  defp do_open2(:wrap, :external, name, fName, repair, size, mode, head, v) do
    {maxB, maxF} = size

    case (try do
            :disk_log_1.mf_ext_open(fName, maxB, maxF, repair, mode, head, v)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, handle, cnt} ->
        {:ok, {:ok, name}, handle, :wrap_ext, cnt}

      error ->
        error
    end
  end

  defp close_disk_log(:undefined) do
    :closed
  end

  defp close_disk_log(l) do
    unblock_pid(l)

    f = fn {pid, _} ->
      :erlang.unlink(pid)
    end

    :lists.foreach(f, r_log(l, :owners))

    r =
      try do
        close_disk_log2(l)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    :erlang.erase(:log)
    r
  end

  defp close_disk_log2(l) do
    case l do
      r_log(format_type: :halt_int, mode: mode, extra: halt) ->
        :disk_log_1.close(r_halt(halt, :fdc), r_log(l, :filename), mode)

      r_log(format_type: :wrap_int, mode: mode, extra: handle) ->
        :disk_log_1.mf_int_close(handle, mode)

      r_log(format_type: :halt_ext, extra: halt) ->
        :disk_log_1.fclose(r_halt(halt, :fdc), r_log(l, :filename))

      r_log(format_type: :wrap_ext, mode: mode, extra: handle) ->
        :disk_log_1.mf_ext_close(handle, mode)
    end

    :closed
  end

  defp do_format_error({:error, module, error}) do
    module.format_error(error)
  end

  defp do_format_error({:error, reason}) do
    do_format_error(reason)
  end

  defp do_format_error({node, error = {:error, _Reason}}) do
    :lists.append(
      :io_lib.format('~p: ', [node]),
      do_format_error(error)
    )
  end

  defp do_format_error({:badarg, arg}) do
    :io_lib.format('The argument ~p is missing, not recognized or not wellformed~n', [arg])
  end

  defp do_format_error({:size_mismatch, oldSize, argSize}) do
    :io_lib.format(
      'The given size ~p does not match the size ~p found on the disk log size file~n',
      [argSize, oldSize]
    )
  end

  defp do_format_error({:read_only_mode, log}) do
    :io_lib.format(
      'The disk log ~tp has been opened read-only, but the requested operation needs read-write access~n',
      [log]
    )
  end

  defp do_format_error({:format_external, log}) do
    :io_lib.format(
      'The requested operation can only be applied on internally formatted disk logs, but ~tp is externally formatted~n',
      [log]
    )
  end

  defp do_format_error({:blocked_log, log}) do
    :io_lib.format(
      'The blocked disk log ~tp does not queue requests, or the log has been blocked by the calling process~n',
      [log]
    )
  end

  defp do_format_error({:full, log}) do
    :io_lib.format('The halt log ~tp is full~n', [log])
  end

  defp do_format_error({:not_blocked, log}) do
    :io_lib.format('The disk log ~tp is not blocked~n', [log])
  end

  defp do_format_error({:not_owner, pid}) do
    :io_lib.format('The pid ~tp is not an owner of the disk log~n', [pid])
  end

  defp do_format_error({:not_blocked_by_pid, log}) do
    :io_lib.format(
      'The disk log ~tp is blocked, but only the blocking pid can unblock a disk log~n',
      [log]
    )
  end

  defp do_format_error({:new_size_too_small, log, currentSize}) do
    :io_lib.format(
      'The current size ~p of the halt log ~tp is greater than the requested new size~n',
      [currentSize, log]
    )
  end

  defp do_format_error({:halt_log, log}) do
    :io_lib.format('The halt log ~tp cannot be wrapped~n', [log])
  end

  defp do_format_error({:same_file_name, log}) do
    :io_lib.format('Current and new file name of the disk log ~tp are the same~n', [log])
  end

  defp do_format_error({:arg_mismatch, option, firstValue, argValue}) do
    :io_lib.format(
      'The value ~tp of the disk log option ~p does not match the current value ~tp~n',
      [argValue, option, firstValue]
    )
  end

  defp do_format_error({:name_already_open, log}) do
    :io_lib.format('The disk log ~tp has already opened another file~n', [log])
  end

  defp do_format_error({:open_read_write, log}) do
    :io_lib.format('The disk log ~tp has already been opened read-write~n', [log])
  end

  defp do_format_error({:open_read_only, log}) do
    :io_lib.format('The disk log ~tp has already been opened read-only~n', [log])
  end

  defp do_format_error({:not_internal_wrap, log}) do
    :io_lib.format(
      'The requested operation cannot be applied since ~tp is not an internally formatted disk log~n',
      [log]
    )
  end

  defp do_format_error(:no_such_log) do
    :io_lib.format('There is no disk log with the given name~n', [])
  end

  defp do_format_error(:nonode) do
    :io_lib.format('There seems to be no node up that can handle the request~n', [])
  end

  defp do_format_error(:nodedown) do
    :io_lib.format('There seems to be no node up that can handle the request~n', [])
  end

  defp do_format_error({:corrupt_log_file, fileName}) do
    :io_lib.format('The disk log file "~ts" contains corrupt data~n', [fileName])
  end

  defp do_format_error({:need_repair, fileName}) do
    :io_lib.format('The disk log file "~ts" has not been closed properly and needs repair~n', [
      fileName
    ])
  end

  defp do_format_error({:not_a_log_file, fileName}) do
    :io_lib.format('The file "~ts" is not a wrap log file~n', [fileName])
  end

  defp do_format_error({:invalid_header, invalidHeader}) do
    :io_lib.format('The disk log header is not wellformed: ~p~n', [invalidHeader])
  end

  defp do_format_error(:end_of_log) do
    :io_lib.format('An attempt was made to step outside a not yet full wrap log~n', [])
  end

  defp do_format_error({:invalid_index_file, fileName}) do
    :io_lib.format('The wrap log index file "~ts" cannot be used~n', [fileName])
  end

  defp do_format_error({:no_continuation, badCont}) do
    :io_lib.format('The term ~p is not a chunk continuation~n', [badCont])
  end

  defp do_format_error({:file_error, fileName, reason}) do
    :io_lib.format(
      '"~ts": ~tp~n',
      [fileName, :file.format_error(reason)]
    )
  end

  defp do_format_error(e) do
    :io_lib.format('~tp~n', [e])
  end

  def do_info(l, cnt) do
    r_log(
      name: name,
      type: type,
      mode: mode,
      filename: file,
      extra: extra,
      status: status,
      owners: owners,
      users: users,
      format: format,
      head: head
    ) = l

    size =
      case type do
        :wrap ->
          :disk_log_1.get_wrap_size(extra)

        :halt ->
          r_halt(extra, :size)
      end

    rW =
      case type do
        :wrap when mode === :read_write ->
          r_handle(
            curB: curB,
            curF: curF,
            cur_cnt: curCnt,
            acc_cnt: accCnt,
            noFull: noFull,
            accFull: accFull
          ) = extra

          newAccFull = accFull + noFull
          newExtra = r_handle(extra, noFull: 0, accFull: newAccFull)
          :erlang.put(:log, r_log(l, extra: newExtra))

          [
            {:no_current_bytes, curB},
            {:no_current_items, curCnt},
            {:no_items, cnt},
            {:no_written_items, curCnt + accCnt},
            {:current_file, curF},
            {:no_overflows, {newAccFull, noFull}}
          ]

        :halt when mode === :read_write ->
          isFull =
            case :erlang.get(:is_full) do
              :undefined ->
                false

              _ ->
                true
            end

          [{:full, isFull}, {:no_written_items, cnt}]

        _ when mode === :read_only ->
          []
      end

    headL =
      case mode do
        :read_write ->
          [
            {:head,
             case head do
               {:ok, h} ->
                 h

               :none ->
                 head

               {_M, _F, _A} ->
                 head
             end}
          ]

        :read_only ->
          []
      end

    common =
      [
        {:name, name},
        {:file, file},
        {:type, type},
        {:format, format},
        {:size, size},
        {:items, cnt},
        {:owners, owners},
        {:users, users}
      ] ++ headL ++ [{:mode, mode}, {:status, status}, {:node, node()}]

    common ++ rW
  end

  defp do_block(pid, queueLogRecs, l) do
    l2 =
      r_log(l,
        status: {:blocked, queueLogRecs},
        blocked_by: pid
      )

    :erlang.put(:log, l2)

    case is_owner(pid, l2) do
      {true, _Notify} ->
        :ok

      false ->
        :erlang.link(pid)
    end
  end

  defp do_unblock(pid, r_log(blocked_by: pid) = l, s) do
    do_unblock(l, s)
  end

  defp do_unblock(_Pid, _L, s) do
    s
  end

  defp do_unblock(l, s) do
    unblock_pid(l)
    l2 = r_log(l, blocked_by: :none, status: :ok)
    :erlang.put(:log, l2)
    [] = r_state(s, :messages)
    r_state(s, queue: [], messages: :lists.reverse(r_state(s, :queue)))
  end

  def do_log(l, b) do
    do_log(l, b, :erlang.iolist_size(b))
  end

  defp do_log(r_log(type: :halt) = l, b, bSz) do
    r_log(format: format, extra: halt) = l
    r_halt(curB: curSize, size: sz) = halt
    {bs, bSize} = logl(b, format, bSz)

    case :erlang.get(:is_full) do
      true ->
        {:error, {:error, {:full, r_log(l, :name)}}, 0}

      :undefined
      when sz === :infinity or
             curSize + bSize <= sz ->
        halt_write(halt, l, b, bs, bSize)

      :undefined ->
        halt_write_full(l, b, format, 0)
    end
  end

  defp do_log(r_log(format_type: :wrap_int) = l, b, _BSz) do
    case :disk_log_1.mf_int_log(r_log(l, :extra), b, r_log(l, :head)) do
      {:ok, handle, logged, lost, wraps} ->
        notify_owners_wrap(wraps)
        :erlang.put(:log, r_log(l, extra: handle))
        logged - lost

      {:ok, handle, logged} ->
        :erlang.put(:log, r_log(l, extra: handle))
        logged

      {:error, error, handle, logged, lost} ->
        :erlang.put(:log, r_log(l, extra: handle))
        {:error, error, logged - lost}
    end
  end

  defp do_log(r_log(format_type: :wrap_ext) = l, b, _BSz) do
    case :disk_log_1.mf_ext_log(r_log(l, :extra), b, r_log(l, :head)) do
      {:ok, handle, logged, lost, wraps} ->
        notify_owners_wrap(wraps)
        :erlang.put(:log, r_log(l, extra: handle))
        logged - lost

      {:ok, handle, logged} ->
        :erlang.put(:log, r_log(l, extra: handle))
        logged

      {:error, error, handle, logged, lost} ->
        :erlang.put(:log, r_log(l, extra: handle))
        {:error, error, logged - lost}
    end
  end

  defp logl(b, :external, :undefined) do
    {b, :erlang.iolist_size(b)}
  end

  defp logl(b, :external, sz) do
    {b, sz}
  end

  defp logl(b, :internal, _Sz) do
    :disk_log_1.logl(b)
  end

  defp halt_write_full(l, [bin | bins], format, n) do
    b = [bin]
    {bs, bSize} = logl(b, format, :undefined)
    halt = r_log(l, :extra)
    r_halt(curB: curSize, size: sz) = halt

    cond do
      curSize + bSize <= sz ->
        case halt_write(halt, l, b, bs, bSize) do
          n1 when is_integer(n1) ->
            halt_write_full(:erlang.get(:log), bins, format, n + n1)

          error ->
            error
        end

      true ->
        halt_write_full(l, [], format, n)
    end
  end

  defp halt_write_full(l, _Bs, _Format, n) do
    :erlang.put(:is_full, true)
    notify_owners(:full)
    {:error, {:error, {:full, r_log(l, :name)}}, n}
  end

  defp halt_write(halt, l, b, bs, bSize) do
    case :disk_log_1.fwrite(r_halt(halt, :fdc), r_log(l, :filename), bs, bSize) do
      {:ok, newFdC} ->
        nCurB = r_halt(halt, :curB) + bSize
        newHalt = r_halt(halt, fdc: newFdC, curB: nCurB)
        :erlang.put(:log, r_log(l, extra: newHalt))
        length(b)

      {error, newFdC} ->
        :erlang.put(:log, r_log(l, extra: r_halt(halt, fdc: newFdC)))
        {:error, error, 0}
    end
  end

  defp do_write_cache(r_log(filename: fName, type: :halt, extra: halt) = log) do
    {reply, newFdC} =
      :disk_log_1.write_cache(
        r_halt(halt, :fdc),
        fName
      )

    :erlang.put(:log, r_log(log, extra: r_halt(halt, fdc: newFdC)))
    reply
  end

  defp do_write_cache(r_log(type: :wrap, extra: handle) = log) do
    {reply, newHandle} = :disk_log_1.mf_write_cache(handle)
    :erlang.put(:log, r_log(log, extra: newHandle))
    reply
  end

  def do_sync(r_log(filename: fName, type: :halt, extra: halt) = log) do
    {reply, newFdC} = :disk_log_1.sync(r_halt(halt, :fdc), fName)
    :erlang.put(:log, r_log(log, extra: r_halt(halt, fdc: newFdC)))
    reply
  end

  def do_sync(r_log(type: :wrap, extra: handle) = log) do
    {reply, newHandle} = :disk_log_1.mf_sync(handle)
    :erlang.put(:log, r_log(log, extra: newHandle))
    reply
  end

  defp do_trunc(r_log(type: :halt) = l, head) do
    r_log(filename: fName, extra: halt) = l
    fdC = r_halt(halt, :fdc)

    {reply1, fdC2} =
      case r_log(l, :format) do
        :internal ->
          :disk_log_1.truncate(fdC, fName, head)

        :external ->
          case :disk_log_1.truncate_at(fdC, fName, :bof) do
            {:ok, nFdC} when head === :none ->
              {:ok, nFdC}

            {:ok, nFdC} ->
              {:ok, h} = head
              :disk_log_1.fwrite(nFdC, fName, h, byte_size(h))

            r ->
              r
          end
      end

    {reply, newHalt} =
      case :disk_log_1.position(fdC2, fName, :cur) do
        {:ok, newFdC, fileSize} when reply1 === :ok ->
          {:ok, r_halt(halt, fdc: newFdC, curB: fileSize)}

        {reply2, newFdC} ->
          {reply2, r_halt(halt, fdc: newFdC)}

        {:ok, newFdC, _} ->
          {reply1, r_halt(halt, fdc: newFdC)}
      end

    :erlang.put(:log, r_log(l, extra: newHalt))
    reply
  end

  defp do_trunc(r_log(type: :wrap) = l, head) do
    handle = r_log(l, :extra)
    oldHead = r_log(l, :head)
    {maxB, maxF} = :disk_log_1.get_wrap_size(handle)
    :ok = do_change_size(l, {maxB, 1})
    newLog = trunc_wrap(r_log(:erlang.get(:log), head: head))
    newLog2 = trunc_wrap(newLog)

    newHandle =
      r_handle(r_log(newLog2, :extra),
        noFull: 0,
        accFull: 0
      )

    do_change_size(
      r_log(newLog2,
        extra: newHandle,
        head: oldHead
      ),
      {maxB, maxF}
    )
  end

  defp trunc_wrap(l) do
    case do_inc_wrap_file(l) do
      {:ok, l2, _Lost} ->
        l2

      {:error, error, _L2} ->
        throw(error)
    end
  end

  defp do_chunk(r_log(format_type: :halt_int, extra: halt) = l, pos, b, n) do
    fdC = r_halt(halt, :fdc)

    {newFdC, reply} =
      case r_log(l, :mode) do
        :read_only ->
          :disk_log_1.chunk_read_only(fdC, r_log(l, :filename), pos, b, n)

        :read_write ->
          :disk_log_1.chunk(fdC, r_log(l, :filename), pos, b, n)
      end

    :erlang.put(:log, r_log(l, extra: r_halt(halt, fdc: newFdC)))
    reply
  end

  defp do_chunk(r_log(format_type: :wrap_int, mode: :read_only, extra: handle) = log, pos, b, n) do
    {newHandle, reply} = :disk_log_1.mf_int_chunk_read_only(handle, pos, b, n)
    :erlang.put(:log, r_log(log, extra: newHandle))
    reply
  end

  defp do_chunk(r_log(format_type: :wrap_int, extra: handle) = log, pos, b, n) do
    {newHandle, reply} = :disk_log_1.mf_int_chunk(handle, pos, b, n)
    :erlang.put(:log, r_log(log, extra: newHandle))
    reply
  end

  defp do_chunk(log, _Pos, _B, _) do
    {:error, {:format_external, r_log(log, :name)}}
  end

  defp do_chunk_step(r_log(format_type: :wrap_int, extra: handle), pos, n) do
    :disk_log_1.mf_int_chunk_step(handle, pos, n)
  end

  defp do_chunk_step(log, _Pos, _N) do
    {:error, {:not_internal_wrap, r_log(log, :name)}}
  end

  defp replies(pids, reply) do
    m = {:disk_log, self(), reply}
    send_reply(pids, m)
  end

  defp send_reply(pid, m) when is_pid(pid) do
    send(pid, m)
    :ok
  end

  defp send_reply([pid | pids], m) do
    send(pid, m)
    send_reply(pids, m)
  end

  defp send_reply([], _M) do
    :ok
  end

  defp reply(to, reply, s) do
    send(to, {:disk_log, self(), reply})
    loop(s)
  end

  defp req(log, r) do
    case :disk_log_server.get_log_pid(log) do
      :undefined ->
        {:error, :no_such_log}

      pid ->
        monitor_request(pid, r)
    end
  end

  defp monitor_request(pid, req) do
    ref = :erlang.monitor(:process, pid)
    send(pid, {self(), req})

    receive do
      {:DOWN, ^ref, :process, ^pid, _Info} ->
        {:error, :no_such_log}

      {:disk_log, ^pid, reply}
      when not is_tuple(reply) or
             :erlang.element(
               2,
               reply
             ) !== :disk_log_stopped ->
        :erlang.demonitor(ref, [:flush])
        reply
    end
  end

  defp req2(pid, r) do
    monitor_request(pid, r)
  end

  defp merge_head(:none, head) do
    head
  end

  defp merge_head(head, _) do
    head
  end

  defp wrap_file_extensions(file) do
    {_CurF, _CurFSz, _TotSz, noOfFiles} = :disk_log_1.read_index_file(file)

    fs =
      cond do
        noOfFiles >= 1 ->
          :lists.seq(1, noOfFiles)

        noOfFiles === 0 ->
          []
      end

    fun = fn ext ->
      case :file.read_file_info(add_ext(file, ext)) do
        {:ok, _} ->
          true

        _Else ->
          false
      end
    end

    :lists.filter(fun, ['idx', 'siz' | fs])
  end

  defp add_ext(file, ext) do
    :lists.concat([file, '.', ext])
  end

  defp notify(log, r) do
    case :disk_log_server.get_log_pid(log) do
      :undefined ->
        {:error, :no_such_log}

      pid ->
        send(pid, r)
        :ok
    end
  end

  defp notify_owners_wrap([]) do
    :ok
  end

  defp notify_owners_wrap([n | wraps]) do
    notify_owners({:wrap, n})
    notify_owners_wrap(wraps)
  end

  defp notify_owners(note) do
    l = :erlang.get(:log)
    msg = {:disk_log, node(), r_log(l, :name), note}

    :lists.foreach(
      fn
        {pid, true} ->
          send(pid, msg)

        _ ->
          :ok
      end,
      r_log(l, :owners)
    )
  end

  defp cache_error(r_state(cache_error: error) = s, pids) do
    :ok = replies(pids, error)
    state_err(r_state(s, cache_error: :ok), error)
  end

  defp state_ok(s) do
    state_err(s, :ok)
  end

  defp state_err(s, err) when r_state(s, :error_status) === err do
    s
  end

  defp state_err(s, err) do
    notify_owners({:error_status, err})
    r_state(s, error_status: err)
  end
end
