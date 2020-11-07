defmodule :m_crashdump_viewer do
  use Bitwise
  require Record

  Record.defrecord(:r_menu_item, :menu_item,
    index: :undefined,
    picture: :undefined,
    text: :undefined,
    depth: :undefined,
    children: :undefined,
    state: :undefined,
    target: :undefined
  )

  Record.defrecord(:r_general_info, :general_info,
    created: :undefined,
    slogan: :undefined,
    system_vsn: :undefined,
    compile_time: :undefined,
    taints: :undefined,
    node_name: :undefined,
    num_atoms: :undefined,
    num_procs: :undefined,
    num_ets: :undefined,
    num_timers: :undefined,
    num_fun: :undefined,
    mem_tot: :undefined,
    mem_max: :undefined,
    instr_info: :undefined,
    thread: :undefined
  )

  Record.defrecord(:r_proc, :proc,
    pid: :undefined,
    name: :undefined,
    init_func: :undefined,
    parent: 'unknown',
    start_time: 'unknown',
    state: :undefined,
    current_func: :undefined,
    msg_q_len: 0,
    msg_q: :undefined,
    last_calls: :undefined,
    links: :undefined,
    monitors: :undefined,
    mon_by: :undefined,
    prog_count: :undefined,
    cp: :undefined,
    arity: :undefined,
    dict: :undefined,
    reds: 0,
    num_heap_frag: 'unknown',
    heap_frag_data: :undefined,
    stack_heap: 0,
    old_heap: :undefined,
    heap_unused: :undefined,
    old_heap_unused: :undefined,
    bin_vheap: :undefined,
    old_bin_vheap: :undefined,
    bin_vheap_unused: :undefined,
    old_bin_vheap_unused: :undefined,
    new_heap_start: :undefined,
    new_heap_top: :undefined,
    stack_top: :undefined,
    stack_end: :undefined,
    old_heap_start: :undefined,
    old_heap_top: :undefined,
    old_heap_end: :undefined,
    memory: :undefined,
    stack_dump: :undefined,
    run_queue: 'unknown',
    int_state: :undefined
  )

  Record.defrecord(:r_port, :port,
    id: :undefined,
    state: :undefined,
    task_flags: 0,
    slot: :undefined,
    connected: :undefined,
    links: :undefined,
    name: :undefined,
    monitors: :undefined,
    suspended: :undefined,
    controls: :undefined,
    input: :undefined,
    output: :undefined,
    queue: :undefined,
    port_data: :undefined
  )

  Record.defrecord(:r_sched, :sched,
    name: :undefined,
    type: :undefined,
    process: :undefined,
    port: :undefined,
    run_q: 0,
    port_q: :undefined,
    details: %{}
  )

  Record.defrecord(:r_ets_table, :ets_table,
    pid: :undefined,
    slot: :undefined,
    id: :undefined,
    name: :undefined,
    is_named: :undefined,
    data_type: 'hash',
    buckets: '-',
    size: :undefined,
    memory: :undefined,
    details: %{}
  )

  Record.defrecord(:r_timer, :timer,
    pid: :undefined,
    name: :undefined,
    msg: :undefined,
    time: :undefined
  )

  Record.defrecord(:r_fu, :fu,
    module: :undefined,
    uniq: :undefined,
    index: :undefined,
    address: :undefined,
    native_address: :undefined,
    refc: :undefined
  )

  Record.defrecord(:r_nod, :nod,
    name: :undefined,
    channel: :undefined,
    conn_type: :undefined,
    controller: :undefined,
    creation: :undefined,
    remote_links: [],
    remote_mon: [],
    remote_mon_by: [],
    error: :undefined
  )

  Record.defrecord(:r_loaded_mod, :loaded_mod,
    mod: :undefined,
    current_size: :undefined,
    current_attrib: :undefined,
    current_comp_info: :undefined,
    old_size: :undefined,
    old_attrib: :undefined,
    old_comp_info: :undefined
  )

  Record.defrecord(:r_hash_table, :hash_table,
    name: :undefined,
    size: :undefined,
    used: :undefined,
    objs: :undefined,
    depth: :undefined
  )

  Record.defrecord(:r_index_table, :index_table,
    name: :undefined,
    size: :undefined,
    limit: :undefined,
    used: :undefined,
    rate: :undefined,
    entries: :undefined
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

  Record.defrecord(:r_state, :state,
    file: :undefined,
    dump_vsn: :undefined,
    wordsize: 4,
    num_atoms: 'unknown'
  )

  Record.defrecord(:r_dec_opts, :dec_opts,
    bin_addr_adj: 0,
    base64: true
  )

  def debug(f) do
    :ttb.tracer(:all, [{:file, 'cdv'}])
    :ttb.p(:all, [:call, :timestamp])
    mS = [{:_, [], [{:return_trace}, {:message, {:caller}}]}]
    tp(f, mS)
    :ttb.ctp(:crashdump_viewer, :stop_debug)
    :ok
  end

  defp tp([{m, f, a} | t], mS) do
    :ttb.tpl(m, f, a, mS)
    tp(t, mS)
  end

  defp tp([{m, f} | t], mS) do
    :ttb.tpl(m, f, mS)
    tp(t, mS)
  end

  defp tp([m | t], mS) do
    :ttb.tp(m, mS)
    tp(t, mS)
  end

  defp tp([], _MS) do
    :ok
  end

  def stop_debug() do
    :ttb.stop([:format])
  end

  def start() do
    start(:undefined)
  end

  def start(file) do
    :cdv_wx.start(file)
  end

  def stop() do
    case :erlang.whereis(:crashdump_viewer_server) do
      :undefined ->
        :ok

      pid ->
        ref = :erlang.monitor(:process, pid)
        cast(:stop)

        receive do
          {:DOWN, ^ref, :process, ^pid, _} ->
            :ok
        end
    end
  end

  def script_start() do
    do_script_start(fn ->
      start()
    end)

    :erlang.halt()
  end

  def script_start([fileAtom]) do
    file = :erlang.atom_to_list(fileAtom)

    case :filelib.is_regular(file) do
      true ->
        do_script_start(fn ->
          start(file)
        end)

      false ->
        :io.format('cdv error: the given file does not exist\n')
        usage()
    end

    :erlang.halt()
  end

  def script_start(_) do
    usage()
    :erlang.halt()
  end

  defp do_script_start(startFun) do
    :erlang.process_flag(:trap_exit, true)

    case startFun.() do
      :ok ->
        case :erlang.whereis(:cdv_wx) do
          pid when is_pid(pid) ->
            :erlang.link(pid)

            receive do
              {:EXIT, ^pid, :normal} ->
                :ok

              {:EXIT, ^pid, reason} ->
                :io.format('\ncdv crash: ~tp\n', [reason])
            end

          _ ->
            :ok
        end

      error ->
        :io.format('\ncdv start failed: ~tp\n', [error])
    end
  end

  defp usage() do
    :io.format(
      'usage: cdv [file]\n\tThe \'file\' must be an existing erlang crash dump.\n\tIf omitted a file dialog will be opened.\n',
      []
    )
  end

  def start_link() do
    case :erlang.whereis(:crashdump_viewer_server) do
      :undefined ->
        :gen_server.start_link({:local, :crashdump_viewer_server}, :crashdump_viewer, [], [])

      pid ->
        {:ok, pid}
    end
  end

  def read_file(file) do
    cast({:read_file, file})
  end

  def general_info() do
    call(:general_info)
  end

  def processes() do
    call(:procs_summary)
  end

  def ports() do
    call(:ports)
  end

  def ets_tables(owner) do
    call({:ets_tables, owner})
  end

  def internal_ets_tables() do
    call(:internal_ets_tables)
  end

  def timers(owner) do
    call({:timers, owner})
  end

  def funs() do
    call(:funs)
  end

  def atoms() do
    call(:atoms)
  end

  def dist_info() do
    call(:dist_info)
  end

  def node_info(channel) do
    call({:node_info, channel})
  end

  def loaded_modules() do
    call(:loaded_mods)
  end

  def loaded_mod_details(mod) do
    call({:loaded_mod_details, mod})
  end

  def memory() do
    call(:memory)
  end

  def persistent_terms() do
    call(:persistent_terms)
  end

  def allocated_areas() do
    call(:allocated_areas)
  end

  def allocator_info() do
    call(:allocator_info)
  end

  def hash_tables() do
    call(:hash_tables)
  end

  def index_tables() do
    call(:index_tables)
  end

  def schedulers() do
    call(:schedulers)
  end

  def proc_details(pid) do
    call({:proc_details, pid})
  end

  def port(id) do
    call({:port, id})
  end

  def expand_binary(pos) do
    call({:expand_binary, pos})
  end

  def get_dump_versions() do
    call(:get_dump_versions)
  end

  def init([]) do
    :ets.new(
      :cdv_dump_index_table,
      [:ordered_set, :named_table, :public]
    )

    :ets.new(
      :cdv_reg_proc_table,
      [:ordered_set, :named_table, :public]
    )

    :ets.new(
      :cdv_binary_index_table,
      [:ordered_set, :named_table, :public]
    )

    :ets.new(
      :cdv_heap_file_chars,
      [:ordered_set, :named_table, :public]
    )

    {:ok, r_state()}
  end

  def handle_call(:general_info, _From, state = r_state(file: file)) do
    genInfo = general_info(file)
    numAtoms = r_general_info(genInfo, :num_atoms)
    wS = parse_vsn_str(r_general_info(genInfo, :system_vsn), 4)

    tW =
      case :erlang.get(:truncated) do
        true ->
          case :erlang.get(:truncated_reason) do
            :undefined ->
              ['WARNING: The crash dump is truncated. Some information might be missing.']

            reason ->
              [
                'WARNING: The crash dump is truncated (' ++
                  reason ++ '). Some information might be missing.'
              ]
          end

        false ->
          []
      end

    :ets.insert(
      :cdv_reg_proc_table,
      {:cdv_dump_node_name, r_general_info(genInfo, :node_name)}
    )

    {:reply, {:ok, genInfo, tW}, r_state(state, wordsize: wS, num_atoms: numAtoms)}
  end

  def handle_call(
        {:expand_binary, {offset, size, pos}},
        _From,
        r_state(file: file, dump_vsn: dumpVsn) = state
      ) do
    fd = open(file)
    pos_bof(fd, pos)
    decodeOpts = get_decode_opts(dumpVsn)
    {bin, _Line} = get_binary(offset, size, bytes(fd), decodeOpts)
    close(fd)
    {:reply, {:ok, bin}, state}
  end

  def handle_call(:procs_summary, _From, state = r_state(file: file, wordsize: wS)) do
    tW = truncated_warning([:proc])
    procs = procs_summary(file, wS)
    {:reply, {:ok, procs, tW}, state}
  end

  def handle_call(
        {:proc_details, pid},
        _From,
        state = r_state(file: file, wordsize: wS, dump_vsn: dumpVsn)
      ) do
    reply =
      case get_proc_details(file, pid, wS, dumpVsn) do
        {:ok, proc, tW} ->
          {:ok, proc, tW}

        other ->
          {:error, other}
      end

    {:reply, reply, state}
  end

  def handle_call({:port, id}, _From, state = r_state(file: file)) do
    reply =
      case get_port(file, id) do
        {:ok, portInfo} ->
          tW = truncated_warning([{:port, id}])
          {:ok, portInfo, tW}

        other ->
          {:error, other}
      end

    {:reply, reply, state}
  end

  def handle_call(:ports, _From, state = r_state(file: file)) do
    tW = truncated_warning([:port])
    ports = get_ports(file)
    {:reply, {:ok, ports, tW}, state}
  end

  def handle_call({:ets_tables, pid0}, _From, state = r_state(file: file, wordsize: wS)) do
    pid =
      case pid0 do
        :all ->
          :"$2"

        _ ->
          pid0
      end

    tW = truncated_warning([:ets])
    ets = get_ets_tables(file, pid, wS)
    {:reply, {:ok, ets, tW}, state}
  end

  def handle_call(:internal_ets_tables, _From, state = r_state(file: file, wordsize: wS)) do
    internalEts = get_internal_ets_tables(file, wS)
    tW = truncated_warning([:internal_ets])
    {:reply, {:ok, internalEts, tW}, state}
  end

  def handle_call({:timers, pid0}, _From, state = r_state(file: file)) do
    pid =
      case pid0 do
        :all ->
          :"$2"

        _ ->
          pid0
      end

    tW = truncated_warning([:timer])
    timers = get_timers(file, pid)
    {:reply, {:ok, timers, tW}, state}
  end

  def handle_call(:dist_info, _From, state = r_state(file: file)) do
    tW = truncated_warning([:visible_node, :hidden_node, :not_connected])
    nods = nods(file)
    {:reply, {:ok, nods, tW}, state}
  end

  def handle_call({:node_info, channel}, _From, state = r_state(file: file)) do
    reply =
      case get_node(file, channel) do
        {:ok, nod} ->
          tW = truncated_warning([:visible_node, :hidden_node, :not_connected])
          {:ok, nod, tW}

        {:error, other} ->
          {:error, other}
      end

    {:reply, reply, state}
  end

  def handle_call(:loaded_mods, _From, state = r_state(file: file)) do
    tW = truncated_warning([:mod])
    {_CC, _OC, mods} = loaded_mods(file)
    {:reply, {:ok, mods, tW}, state}
  end

  def handle_call(
        {:loaded_mod_details, mod},
        _From,
        r_state(dump_vsn: dumpVsn, file: file) = state
      ) do
    tW = truncated_warning([{:mod, mod}])
    decodeOpts = get_decode_opts(dumpVsn)
    modInfo = get_loaded_mod_details(file, mod, decodeOpts)
    {:reply, {:ok, modInfo, tW}, state}
  end

  def handle_call(:funs, _From, state = r_state(file: file)) do
    tW = truncated_warning([:fu])
    funs = funs(file)
    {:reply, {:ok, funs, tW}, state}
  end

  def handle_call(:atoms, _From, state = r_state(file: file, num_atoms: numAtoms0)) do
    tW = truncated_warning([:atoms])

    numAtoms =
      try do
        :erlang.list_to_integer(numAtoms0)
      catch
        :error, :badarg ->
          -1
      end

    atoms = atoms(file, numAtoms)
    {:reply, {:ok, atoms, tW}, state}
  end

  def handle_call(:memory, _From, state = r_state(file: file)) do
    memory = memory(file)
    tW = truncated_warning([:memory])
    {:reply, {:ok, memory, tW}, state}
  end

  def handle_call(:persistent_terms, _From, state = r_state(file: file, dump_vsn: dumpVsn)) do
    tW = truncated_warning([:persistent_terms, :literals])
    decodeOpts = get_decode_opts(dumpVsn)
    terms = persistent_terms(file, decodeOpts)
    {:reply, {:ok, terms, tW}, state}
  end

  def handle_call(:allocated_areas, _From, state = r_state(file: file)) do
    allocatedAreas = allocated_areas(file)
    tW = truncated_warning([:allocated_areas])
    {:reply, {:ok, allocatedAreas, tW}, state}
  end

  def handle_call(:allocator_info, _From, state = r_state(file: file)) do
    slAlloc = allocator_info(file)
    tW = truncated_warning([:allocator])
    {:reply, {:ok, slAlloc, tW}, state}
  end

  def handle_call(:hash_tables, _From, state = r_state(file: file)) do
    hashTables = hash_tables(file)
    tW = truncated_warning([:hash_table, :index_table])
    {:reply, {:ok, hashTables, tW}, state}
  end

  def handle_call(:index_tables, _From, state = r_state(file: file)) do
    indexTables = index_tables(file)
    tW = truncated_warning([:hash_table, :index_table])
    {:reply, {:ok, indexTables, tW}, state}
  end

  def handle_call(:schedulers, _From, state = r_state(file: file)) do
    schedulers = schedulers(file)
    tW = truncated_warning([:scheduler])
    {:reply, {:ok, schedulers, tW}, state}
  end

  def handle_call(:get_dump_versions, _From, state = r_state(dump_vsn: dumpVsn)) do
    {:reply, {:ok, {[0, 5], dumpVsn}}, state}
  end

  def handle_cast({:read_file, file}, _State) do
    case do_read_file(file) do
      {:ok, dumpVsn} ->
        :observer_lib.report_progress({:ok, :done})
        {:noreply, r_state(file: file, dump_vsn: dumpVsn)}

      error ->
        end_progress(error)
        {:noreply, r_state()}
    end
  end

  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  def handle_info(_Info, state) do
    {:noreply, state}
  end

  def terminate(_Reason, _State) do
    :ok
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp call(request) do
    :gen_server.call(:crashdump_viewer_server, request, 3_600_000)
  end

  defp cast(msg) do
    :gen_server.cast(:crashdump_viewer_server, msg)
  end

  defp unexpected(_Fd, {:eof, _LastLine}, _Where) do
    :ok
  end

  defp unexpected(fd, {:part, what}, where) do
    skip_rest_of_line(fd)
    :io.format('WARNING: Found unexpected line in ~ts:~n~ts ...~n', [where, what])
  end

  defp unexpected(_Fd, what, where) do
    :io.format('WARNING: Found unexpected line in ~ts:~n~ts~n', [where, what])
  end

  defp truncated_warning([]) do
    []
  end

  defp truncated_warning([tag | tags]) do
    case truncated_here(tag) do
      true ->
        truncated_warning()

      false ->
        truncated_warning(tags)
    end
  end

  defp truncated_warning() do
    case :erlang.get(:truncated_reason) do
      :undefined ->
        ['WARNING: The crash dump is truncated here. Some information might be missing.']

      reason ->
        [
          'WARNING: The crash dump is truncated here (' ++
            reason ++ '). Some information might be missing.'
        ]
    end
  end

  defp truncated_here(tag) do
    case :erlang.get(:truncated) do
      true ->
        case :erlang.get(:last_tag) do
          {^tag, _Pos} ->
            true

          {{^tag, _Id}, _Pos} ->
            true

          _LastTag ->
            truncated_earlier(tag)
        end

      false ->
        false
    end
  end

  defp truncated_earlier({:proc, pid}) do
    compare_pid(pid, :erlang.get(:truncated_proc))
  end

  defp truncated_earlier(_Tag) do
    false
  end

  defp compare_pid('<' ++ id, '<' ++ otherId) do
    id >= otherId
  end

  defp compare_pid(_, _) do
    false
  end

  defp open(file) do
    {:ok, fd} =
      :file.open(
        file,
        [:read, :read_ahead, :raw, :binary]
      )

    fd
  end

  defp close(fd) do
    :erlang.erase(:chunk)
    :file.close(fd)
  end

  defp pos_bof(fd, pos) do
    case :erlang.get(:pos) do
      :undefined ->
        hard_pos_bof(fd, pos)

      oldPos when pos >= oldPos ->
        case :erlang.get(:chunk) do
          :undefined ->
            hard_pos_bof(fd, pos)

          chunk ->
            chunkSize = byte_size(chunk)
            chunkEnd = oldPos + chunkSize

            cond do
              pos <= chunkEnd ->
                diff = pos - oldPos
                :erlang.put(:pos, pos)

                :erlang.put(
                  :chunk,
                  :binary.part(chunk, diff, chunkEnd - pos)
                )

              true ->
                hard_pos_bof(fd, pos)
            end
        end

      _ ->
        hard_pos_bof(fd, pos)
    end
  end

  defp hard_pos_bof(fd, pos) do
    reset_chunk()
    :file.position(fd, {:bof, pos})
  end

  defp get_chunk(fd) do
    case :erlang.erase(:chunk) do
      :undefined ->
        case read(fd) do
          :eof ->
            put_pos(fd)
            :eof

          other ->
            other
        end

      bin ->
        {:ok, bin}
    end
  end

  defp progress_read(fd) do
    {r, bytes} =
      case read(fd) do
        {:ok, bin} ->
          {{:ok, bin}, byte_size(bin)}

        other ->
          {other, 0}
      end

    update_progress(bytes)
    r
  end

  defp read(fd) do
    :file.read(fd, 1000)
  end

  defp put_chunk(fd, bin) do
    {:ok, pos0} = :file.position(fd, :cur)
    pos = pos0 - byte_size(bin)
    :erlang.put(:chunk, bin)
    :erlang.put(:pos, pos)
  end

  defp put_pos(fd) do
    {:ok, pos} = :file.position(fd, :cur)
    :erlang.put(:pos, pos)
  end

  defp reset_chunk() do
    :erlang.erase(:chunk)
    :erlang.erase(:pos)
  end

  defp line_head(fd) do
    case get_chunk(fd) do
      {:ok, bin} ->
        line_head(fd, bin, [], 0)

      :eof ->
        {:eof, []}
    end
  end

  defp line_head(fd, bin, acc, 100) do
    put_chunk(fd, bin)
    {:part, :lists.reverse(acc)}
  end

  defp line_head(fd, <<?\n::size(8), bin::binary>>, acc, _N) do
    put_chunk(fd, bin)
    :lists.reverse(acc)
  end

  defp line_head(fd, <<?:::size(8), ?\r::size(8), ?\n::size(8), bin::binary>>, acc, _N) do
    put_chunk(fd, bin)
    :lists.reverse(acc)
  end

  defp line_head(fd, <<?:::size(8), ?\r::size(8)>>, acc, n) do
    case get_chunk(fd) do
      {:ok, bin} ->
        line_head(fd, <<?:, bin::binary>>, acc, n)

      :eof ->
        {:eof, :lists.reverse(acc)}
    end
  end

  defp line_head(fd, <<?:::size(8)>>, acc, n) do
    case get_chunk(fd) do
      {:ok, bin} ->
        line_head(fd, <<?:, bin::binary>>, acc, n)

      :eof ->
        {:eof, :lists.reverse(acc)}
    end
  end

  defp line_head(fd, <<?:::size(8), space::size(8), bin::binary>>, acc, _N)
       when space === ?\s or space === ?\n do
    put_chunk(fd, bin)
    :lists.reverse(acc)
  end

  defp line_head(fd, <<?:::size(8), bin::binary>>, acc, _N) do
    put_chunk(fd, bin)
    :lists.reverse(acc)
  end

  defp line_head(fd, <<?\r::size(8), bin::binary>>, acc, n) do
    line_head(fd, bin, acc, n + 1)
  end

  defp line_head(fd, <<char::size(8), bin::binary>>, acc, n) do
    line_head(fd, bin, [char | acc], n + 1)
  end

  defp line_head(fd, <<>>, acc, n) do
    case get_chunk(fd) do
      {:ok, bin} ->
        line_head(fd, bin, acc, n)

      :eof ->
        {:eof, :lists.reverse(acc)}
    end
  end

  defp skip_rest_of_line(fd) do
    case get_chunk(fd) do
      {:ok, bin} ->
        skip(fd, bin)

      :eof ->
        :ok
    end
  end

  defp skip(fd, <<?\n::size(8), bin::binary>>) do
    put_chunk(fd, bin)
    :ok
  end

  defp skip(fd, <<_Char::size(8), bin::binary>>) do
    skip(fd, bin)
  end

  defp skip(fd, <<>>) do
    case get_chunk(fd) do
      {:ok, bin} ->
        skip(fd, bin)

      :eof ->
        :ok
    end
  end

  defp string(fd) do
    string(fd, '-1')
  end

  defp string(fd, noExist) do
    case bytes(fd, :noexist) do
      :noexist ->
        noExist

      val ->
        byte_list_to_string(val)
    end
  end

  defp byte_list_to_string(byteList) do
    bin = :erlang.list_to_binary(byteList)

    case :unicode.characters_to_list(bin) do
      str when is_list(str) ->
        str

      _ ->
        byteList
    end
  end

  defp bytes(fd) do
    bytes(fd, '-1')
  end

  defp bytes(fd, noExist) do
    case get_rest_of_line(fd) do
      {:eof, []} ->
        noExist

      [] ->
        noExist

      {:eof, val} ->
        val

      '=abort:' ++ _ ->
        noExist

      val ->
        val
    end
  end

  defp get_rest_of_line(fd) do
    case get_chunk(fd) do
      {:ok, bin} ->
        get_rest_of_line_1(fd, bin, [])

      :eof ->
        {:eof, []}
    end
  end

  defp get_rest_of_line_1(fd, <<?\n::size(8), bin::binary>>, acc) do
    put_chunk(fd, bin)
    :lists.reverse(acc)
  end

  defp get_rest_of_line_1(fd, <<?\r::size(8), rest::binary>>, acc) do
    get_rest_of_line_1(fd, rest, acc)
  end

  defp get_rest_of_line_1(fd, <<char::size(8), rest::binary>>, acc) do
    get_rest_of_line_1(fd, rest, [char | acc])
  end

  defp get_rest_of_line_1(fd, <<>>, acc) do
    case get_chunk(fd) do
      {:ok, bin} ->
        get_rest_of_line_1(fd, bin, acc)

      :eof ->
        {:eof, :lists.reverse(acc)}
    end
  end

  defp split(str) do
    split(?\s, str, [])
  end

  defp split(char, str) do
    split(char, str, [])
  end

  defp split(char, [char | str], acc) do
    {:lists.reverse(acc), str}
  end

  defp split(_Char, [[?\r, ?\n] | str], acc) do
    {:lists.reverse(acc), str}
  end

  defp split(_Char, [?\n | str], acc) do
    {:lists.reverse(acc), str}
  end

  defp split(char, [h | t], acc) do
    split(char, t, [h | acc])
  end

  defp split(_Char, [], acc) do
    {:lists.reverse(acc), []}
  end

  defp parse_vsn_str([], wS) do
    wS
  end

  defp parse_vsn_str(str, wS) do
    case str do
      '[64-bit]' ++ _Rest ->
        8

      [_Char | rest] ->
        parse_vsn_str(rest, wS)
    end
  end

  defp do_read_file(file) do
    :erlang.erase(:literals)
    :erlang.put(:truncated, false)
    :erlang.erase(:truncated_reason)

    case :file.read_file_info(file) do
      {:ok, r_file_info(type: :regular, access: fileA, size: size)}
      when fileA === :read or fileA === :read_write ->
        fd = open(file)
        init_progress('Reading file', size)

        case progress_read(fd) do
          {:ok, <<?=::size(8), tagAndRest::binary>>} ->
            {tag, id, rest, n1} = tag(fd, tagAndRest, 1)

            case tag do
              :erl_crash_dump ->
                case check_dump_version(id) do
                  {:ok, dumpVsn} ->
                    reset_tables()
                    insert_index(tag, id, pos = n1 + 1)
                    put_last_tag(tag, '', pos)
                    decodeOpts = get_decode_opts(dumpVsn)
                    indexify(fd, decodeOpts, rest, n1)
                    end_progress()
                    check_if_truncated()
                    close(fd)
                    {:ok, dumpVsn}

                  error ->
                    close(fd)
                    error
                end

              _Other ->
                r = :io_lib.format('~ts is not an Erlang crash dump~n', [file])
                close(fd)
                {:error, r}
            end

          {:ok, <<"<Erlang crash dump>", _Rest::binary>>} ->
            r =
              :io_lib.format(
                'The crashdump ~ts is in the pre-R10B format, which is no longer supported.~n',
                [file]
              )

            close(fd)
            {:error, r}

          _Other ->
            r = :io_lib.format('~ts is not an Erlang crash dump~n', [file])
            close(fd)
            {:error, r}
        end

      _other ->
        r = :io_lib.format('~ts is not an Erlang crash dump~n', [file])
        {:error, r}
    end
  end

  defp check_dump_version(vsn) do
    dumpVsn =
      for l <- :string.lexemes(vsn, '.') do
        :erlang.list_to_integer(l)
      end

    cond do
      dumpVsn > [0, 5] ->
        info =
          'This Crashdump Viewer is too old for the given Erlang crash dump. Please use a newer version of Crashdump Viewer.'

        {:error, info}

      true ->
        {:ok, dumpVsn}
    end
  end

  defp indexify(fd, decodeOpts, bin, n) do
    case :binary.match(bin, "\n=") do
      {start, len} ->
        pos = start + len
        <<_::size(pos)-binary, tagAndRest::binary>> = bin
        {tag, id, rest, n1} = tag(fd, tagAndRest, n + pos)
        newPos = n1 + 1

        case tag do
          :binary ->
            {hexAddr, _} = get_hex(id)
            addr = hexAddr ||| r_dec_opts(decodeOpts, :bin_addr_adj)
            insert_binary_index(addr, newPos)

          _ ->
            insert_index(tag, id, newPos)
        end

        case put_last_tag(tag, id, newPos) do
          {{:proc_heap, lastId}, lastPos} ->
            :ets.insert(
              :cdv_heap_file_chars,
              {lastId, n + start + 1 - lastPos}
            )

          {{:literals, []}, lastPos} ->
            :ets.insert(
              :cdv_heap_file_chars,
              {:literals, n + start + 1 - lastPos}
            )

          _ ->
            :ok
        end

        indexify(fd, decodeOpts, rest, n1)

      :nomatch ->
        case progress_read(fd) do
          {:ok, chunk0} when is_binary(chunk0) ->
            {chunk, n1} =
              case :binary.last(bin) do
                ?\n ->
                  {<<?\n, chunk0::binary>>, n + byte_size(bin) - 1}

                _ ->
                  {chunk0, n + byte_size(bin)}
              end

            indexify(fd, decodeOpts, chunk, n1)

          :eof ->
            :eof
        end
    end
  end

  defp tag(fd, bin, n) do
    tag(fd, bin, n, [], [], :tag)
  end

  defp tag(_Fd, <<?\n::size(8), _::binary>> = rest, n, gat, di, _Now) do
    {tag_to_atom(:lists.reverse(gat)), :lists.reverse(di), rest, n}
  end

  defp tag(fd, <<?\r::size(8), rest::binary>>, n, gat, di, now) do
    tag(fd, rest, n + 1, gat, di, now)
  end

  defp tag(fd, <<?:::size(8), idAndRest::binary>>, n, gat, di, :tag) do
    tag(fd, idAndRest, n + 1, gat, di, :id)
  end

  defp tag(fd, <<char::size(8), rest::binary>>, n, gat, di, :tag) do
    tag(fd, rest, n + 1, [char | gat], di, :tag)
  end

  defp tag(fd, <<char::size(8), rest::binary>>, n, gat, di, :id) do
    tag(fd, rest, n + 1, gat, [char | di], :id)
  end

  defp tag(fd, <<>>, n, gat, di, now) do
    case progress_read(fd) do
      {:ok, chunk} when is_binary(chunk) ->
        tag(fd, chunk, n, gat, di, now)

      :eof ->
        {tag_to_atom(:lists.reverse(gat)), :lists.reverse(di), <<>>, n}
    end
  end

  defp check_if_truncated() do
    case :erlang.get(:last_tag) do
      {{:ende, _}, _} ->
        :erlang.put(:truncated, false)
        :erlang.put(:truncated_proc, false)

      {{:literals, []}, _} ->
        :erlang.put(:truncated, true)
        :erlang.put(:truncated_proc, false)
        delete_index(:literals, [])

      {truncatedTag, _} ->
        :erlang.put(:truncated, true)
        find_truncated_proc(truncatedTag)
    end
  end

  defp find_truncated_proc({tag, _Id})
       when tag == :atoms or
              tag == :binary or tag == :instr_data or
              tag == :memory_status or tag == :memory_map do
    :erlang.put(:truncated_proc, false)
  end

  defp find_truncated_proc({tag, pid}) do
    case is_proc_tag(tag) do
      true ->
        :erlang.put(:truncated_proc, pid)

      false ->
        :erlang.put(:truncated_proc, '<0.0.0>')
    end
  end

  defp is_proc_tag(tag)
       when tag == :proc or
              tag == :proc_dictionary or tag == :proc_messages or
              tag == :proc_stack or tag == :proc_heap do
    true
  end

  defp is_proc_tag(_) do
    false
  end

  defp general_info(file) do
    [{_Id, start}] = lookup_index(:erl_crash_dump)
    fd = open(file)
    pos_bof(fd, start)

    created =
      case get_rest_of_line(fd) do
        {:eof, someOfLine} ->
          someOfLine

        wholeLine ->
          wholeLine
      end

    {slogan, sysVsn} = get_slogan_and_sysvsn(fd, [])

    gI =
      get_general_info(
        fd,
        r_general_info(created: created, slogan: slogan, system_vsn: sysVsn)
      )

    {memTot, memMax} =
      case lookup_index(:memory) do
        [{_, memStart}] ->
          pos_bof(fd, memStart)
          memory = get_meminfo(fd, [])

          tot =
            case :lists.keysearch(:total, 1, memory) do
              {:value, {_, t}} ->
                t

              false ->
                ''
            end

          max =
            case :lists.keysearch(:maximum, 1, memory) do
              {:value, {_, m}} ->
                m

              false ->
                ''
            end

          {tot, max}

        _ ->
          {'', ''}
      end

    close(fd)
    {numProcs, numEts, numFuns, numTimers} = count()

    nodeName =
      case lookup_index(:node) do
        [{n, _Start}] ->
          n

        [] ->
          case lookup_index(:no_distribution) do
            [_] ->
              '\'nonode@nohost\''

            [] ->
              'unknown'
          end
      end

    instrInfo =
      case lookup_index(:old_instr_data) do
        [] ->
          case lookup_index(:instr_data) do
            [] ->
              false

            _ ->
              :instr_data
          end

        _ ->
          :old_instr_data
      end

    r_general_info(gI,
      node_name: nodeName,
      num_procs: :erlang.integer_to_list(numProcs),
      num_ets: :erlang.integer_to_list(numEts),
      num_timers: :erlang.integer_to_list(numTimers),
      num_fun: :erlang.integer_to_list(numFuns),
      mem_tot: memTot,
      mem_max: memMax,
      instr_info: instrInfo
    )
  end

  defp get_slogan_and_sysvsn(fd, acc) do
    case string(fd, :eof) do
      'Slogan: ' ++ sloganPart when acc == [] ->
        get_slogan_and_sysvsn(fd, [sloganPart])

      'System version: ' ++ systemVsn ->
        {:lists.append(:lists.reverse(acc)), systemVsn}

      :eof ->
        {:lists.append(:lists.reverse(acc)), '-1'}

      sloganPart ->
        get_slogan_and_sysvsn(fd, [[?\n | sloganPart] | acc])
    end
  end

  defp get_general_info(fd, genInfo) do
    case line_head(fd) do
      'Compiled' ->
        get_general_info(
          fd,
          r_general_info(genInfo, compile_time: bytes(fd))
        )

      'Taints' ->
        val =
          case string(fd) do
            '-1' ->
              '(none)'

            line ->
              line
          end

        get_general_info(fd, r_general_info(genInfo, taints: val))

      'Atoms' ->
        get_general_info(fd, r_general_info(genInfo, num_atoms: bytes(fd)))

      'Calling Thread' ->
        get_general_info(fd, r_general_info(genInfo, thread: bytes(fd)))

      '=' ++ _next_tag ->
        genInfo

      other ->
        unexpected(fd, other, 'general information')
        genInfo
    end
  end

  defp count() do
    {count_index(:proc), count_index(:ets), count_index(:fu), count_index(:timer)}
  end

  defp procs_summary(file, wS) do
    parseFun = fn fd, pid0 ->
      pid = :erlang.list_to_pid(pid0)
      proc = get_procinfo(fd, &main_procinfo/5, r_proc(pid: pid), wS)

      case r_proc(proc, :name) do
        :undefined ->
          true

        name ->
          :ets.insert(:cdv_reg_proc_table, {name, pid})
          :ets.insert(:cdv_reg_proc_table, {pid0, name})
      end

      case r_proc(proc, :memory) do
        :undefined ->
          r_proc(proc, memory: r_proc(proc, :stack_heap))

        _ ->
          proc
      end
    end

    lookup_and_parse_index(file, :proc, parseFun, 'processes')
  end

  defp get_proc_details(file, pid, wS, dumpVsn) do
    case lookup_index(:proc, pid) do
      [{_, start}] ->
        fd = open(file)

        {{stack, msgQ, dict}, tW} =
          case truncated_warning([{:proc, pid}]) do
            [] ->
              expand_memory(fd, pid, dumpVsn)

            tW0 ->
              {{[], [], []}, tW0}
          end

        pos_bof(fd, start)
        proc0 = r_proc(pid: pid, stack_dump: stack, msg_q: msgQ, dict: dict)
        proc = get_procinfo(fd, &all_procinfo/5, proc0, wS)
        close(fd)
        {:ok, proc, tW}

      _ ->
        maybe_other_node(pid)
    end
  end

  defp get_procinfo(fd, fun, proc, wS) do
    case line_head(fd) do
      'State' ->
        state =
          case bytes(fd) do
            'Garbing' ->
              'Garbing\n(limited info)'

            state0 ->
              state0
          end

        get_procinfo(fd, fun, r_proc(proc, state: state), wS)

      'Name' ->
        get_procinfo(fd, fun, r_proc(proc, name: string(fd)), wS)

      'Spawned as' ->
        iF = string(fd)

        case r_proc(proc, :name) do
          :undefined ->
            get_procinfo(fd, fun, r_proc(proc, name: iF, init_func: iF), wS)

          _ ->
            get_procinfo(fd, fun, r_proc(proc, init_func: iF), wS)
        end

      'Message queue length' ->
        get_procinfo(fd, fun, r_proc(proc, msg_q_len: :erlang.list_to_integer(bytes(fd))), wS)

      'Reductions' ->
        get_procinfo(fd, fun, r_proc(proc, reds: :erlang.list_to_integer(bytes(fd))), wS)

      'Stack+heap' ->
        get_procinfo(
          fd,
          fun,
          r_proc(proc, stack_heap: :erlang.list_to_integer(bytes(fd)) * wS),
          wS
        )

      'Memory' ->
        get_procinfo(fd, fun, r_proc(proc, memory: :erlang.list_to_integer(bytes(fd))), wS)

      {:eof, _} ->
        proc

      other ->
        fun.(fd, fun, proc, wS, other)
    end
  end

  defp main_procinfo(fd, fun, proc, wS, lineHead) do
    case lineHead do
      '=' ++ _next_tag ->
        proc

      'arity = ' ++ _ ->
        get_procinfo(fd, fun, proc, wS)

      _Other ->
        skip_rest_of_line(fd)
        get_procinfo(fd, fun, proc, wS)
    end
  end

  defp all_procinfo(fd, fun, proc, wS, lineHead) do
    case lineHead do
      'Spawned by' ->
        case bytes(fd) do
          '[]' ->
            get_procinfo(fd, fun, proc, wS)

          parent ->
            get_procinfo(fd, fun, r_proc(proc, parent: parent), wS)
        end

      'Started' ->
        get_procinfo(fd, fun, r_proc(proc, start_time: bytes(fd)), wS)

      'Last scheduled in for' ->
        get_procinfo(
          fd,
          fun,
          r_proc(proc, current_func: {'Last scheduled in for', string(fd)}),
          wS
        )

      'Current call' ->
        get_procinfo(fd, fun, r_proc(proc, current_func: {'Current call', string(fd)}), wS)

      'Number of heap fragments' ->
        get_procinfo(fd, fun, r_proc(proc, num_heap_frag: bytes(fd)), wS)

      'Heap fragment data' ->
        get_procinfo(fd, fun, r_proc(proc, heap_frag_data: bytes(fd)), wS)

      'OldHeap' ->
        bytes = :erlang.list_to_integer(bytes(fd)) * wS
        get_procinfo(fd, fun, r_proc(proc, old_heap: bytes), wS)

      'Heap unused' ->
        bytes = :erlang.list_to_integer(bytes(fd)) * wS
        get_procinfo(fd, fun, r_proc(proc, heap_unused: bytes), wS)

      'OldHeap unused' ->
        bytes = :erlang.list_to_integer(bytes(fd)) * wS
        get_procinfo(fd, fun, r_proc(proc, old_heap_unused: bytes), wS)

      'BinVHeap' ->
        bytes = :erlang.list_to_integer(bytes(fd)) * wS
        get_procinfo(fd, fun, r_proc(proc, bin_vheap: bytes), wS)

      'OldBinVHeap' ->
        bytes = :erlang.list_to_integer(bytes(fd)) * wS
        get_procinfo(fd, fun, r_proc(proc, old_bin_vheap: bytes), wS)

      'BinVHeap unused' ->
        bytes = :erlang.list_to_integer(bytes(fd)) * wS
        get_procinfo(fd, fun, r_proc(proc, bin_vheap_unused: bytes), wS)

      'OldBinVHeap unused' ->
        bytes =
          case bytes(fd) do
            'overflow' ->
              -1

            int ->
              :erlang.list_to_integer(int) * wS
          end

        get_procinfo(fd, fun, r_proc(proc, old_bin_vheap_unused: bytes), wS)

      'New heap start' ->
        get_procinfo(fd, fun, r_proc(proc, new_heap_start: bytes(fd)), wS)

      'New heap top' ->
        get_procinfo(fd, fun, r_proc(proc, new_heap_top: bytes(fd)), wS)

      'Stack top' ->
        get_procinfo(fd, fun, r_proc(proc, stack_top: bytes(fd)), wS)

      'Stack end' ->
        get_procinfo(fd, fun, r_proc(proc, stack_end: bytes(fd)), wS)

      'Old heap start' ->
        get_procinfo(fd, fun, r_proc(proc, old_heap_start: bytes(fd)), wS)

      'Old heap top' ->
        get_procinfo(fd, fun, r_proc(proc, old_heap_top: bytes(fd)), wS)

      'Old heap end' ->
        get_procinfo(fd, fun, r_proc(proc, old_heap_end: bytes(fd)), wS)

      'Last calls' ->
        get_procinfo(fd, fun, r_proc(proc, last_calls: get_last_calls(fd)), wS)

      'Link list' ->
        {links, monitors, monitoredBy} = get_link_list(fd)

        get_procinfo(
          fd,
          fun,
          r_proc(proc, links: links, monitors: monitors, mon_by: monitoredBy),
          wS
        )

      'Program counter' ->
        get_procinfo(fd, fun, r_proc(proc, prog_count: string(fd)), wS)

      'CP' ->
        get_procinfo(fd, fun, r_proc(proc, cp: string(fd)), wS)

      'arity = ' ++ arity ->
        get_procinfo(fd, fun, r_proc(proc, arity: arity -- '\r\n'), wS)

      'Run queue' ->
        get_procinfo(fd, fun, r_proc(proc, run_queue: string(fd)), wS)

      'Internal State' ->
        get_procinfo(fd, fun, r_proc(proc, int_state: string(fd)), wS)

      '=' ++ _next_tag ->
        proc

      other ->
        unexpected(fd, other, 'process info')
        get_procinfo(fd, fun, proc, wS)
    end
  end

  defp get_last_calls(fd) do
    case get_chunk(fd) do
      {:ok, bin} ->
        get_last_calls(fd, bin, [], [])

      :eof ->
        []
    end
  end

  defp get_last_calls(fd, <<?\n::size(8), bin::binary>>, [], lines) do
    put_chunk(fd, bin)
    :lists.reverse(lines)
  end

  defp get_last_calls(fd, <<?:::size(8)>>, acc, lines) do
    case get_chunk(fd) do
      {:ok, bin} ->
        get_last_calls(fd, <<?:::size(8), bin::binary>>, acc, lines)

      :eof ->
        case byte_list_to_string(:lists.reverse(acc)) do
          nextHeading
          when nextHeading == 'Link list' or nextHeading == 'Dictionary' or
                 nextHeading == 'Reductions' ->
            put_chunk(fd, :erlang.list_to_binary(nextHeading ++ ':'))
            :lists.reverse(lines)

          lastCallFunction ->
            :lists.reverse(lines, [lastCallFunction ++ ':...(truncated)'])
        end
    end
  end

  defp get_last_calls(fd, <<?:::size(8), ?\s::size(8), bin::binary>>, acc, lines) do
    headingBin =
      :erlang.list_to_binary(
        :lists.reverse(
          acc,
          [?:]
        )
      )

    put_chunk(fd, <<headingBin::binary, bin::binary>>)
    :lists.reverse(lines)
  end

  defp get_last_calls(fd, <<?\n::size(8), bin::binary>>, acc, lines) do
    get_last_calls(fd, bin, [], [byte_list_to_string(:lists.reverse(acc)) | lines])
  end

  defp get_last_calls(fd, <<?\r::size(8), bin::binary>>, acc, lines) do
    get_last_calls(fd, bin, acc, lines)
  end

  defp get_last_calls(fd, <<?\s::size(8), bin::binary>>, [], lines) do
    get_last_calls(fd, bin, [], lines)
  end

  defp get_last_calls(fd, <<char::size(8), bin::binary>>, acc, lines) do
    get_last_calls(fd, bin, [char | acc], lines)
  end

  defp get_last_calls(fd, <<>>, acc, lines) do
    case get_chunk(fd) do
      {:ok, bin} ->
        get_last_calls(fd, bin, acc, lines)

      :eof ->
        :lists.reverse(
          lines,
          [byte_list_to_string(:lists.reverse(acc))]
        )
    end
  end

  defp get_link_list(fd) do
    case get_chunk(fd) do
      {:ok, <<"[", bin::binary>>} ->
        %{:links => links, :mons => monitors, :mon_by => monitoredBy} =
          get_link_list(fd, bin, %{:links => [], :mons => [], :mon_by => []})

        {:lists.reverse(links), :lists.reverse(monitors), :lists.reverse(monitoredBy)}

      :eof ->
        {[], [], []}
    end
  end

  defp get_link_list(fd, <<nL::size(8), _::binary>> = bin, acc)
       when nL === ?\r or nL === ?\n do
    skip(fd, bin)
    acc
  end

  defp get_link_list(fd, bin, acc) do
    case :binary.split(bin, [", ", "]"]) do
      [link, rest] ->
        get_link_list(fd, rest, get_link(link, acc))

      [incomplete] ->
        case get_chunk(fd) do
          {:ok, more} ->
            get_link_list(fd, <<incomplete::binary, more::binary>>, acc)

          :eof ->
            acc
        end
    end
  end

  defp get_link(
         <<"#Port", _::binary>> = portBin,
         %{:links => links} = acc
       ) do
    portStr = :erlang.binary_to_list(portBin)
    %{acc | :links => [{portStr, portStr} | links]}
  end

  defp get_link(
         <<"<", _::binary>> = pidBin,
         %{:links => links} = acc
       ) do
    pidStr = :erlang.binary_to_list(pidBin)
    %{acc | :links => [{pidStr, pidStr} | links]}
  end

  defp get_link(
         <<"{to,", bin::binary>>,
         %{:mons => monitors} = acc
       ) do
    %{acc | :mons => [parse_monitor(bin) | monitors]}
  end

  defp get_link(
         <<"{from,", bin::binary>>,
         %{:mon_by => monitoredBy} = acc
       ) do
    %{acc | :mon_by => [parse_monitor(bin) | monitoredBy]}
  end

  defp get_link(unexpected, acc) do
    :io.format('WARNING: found unexpected data in link list:~n~ts~n', [unexpected])
    acc
  end

  defp parse_monitor(monBin) do
    case :binary.split(monBin, [",", "{", "}"], [:global]) do
      [pidBin, refBin, <<>>] ->
        pidStr = :erlang.binary_to_list(pidBin)
        refStr = :erlang.binary_to_list(refBin)
        {pidStr, pidStr ++ ' (' ++ refStr ++ ')'}

      [<<>>, nameBin, nodeBin, <<>>, refBin, <<>>] ->
        nameStr = :erlang.binary_to_list(nameBin)
        nodeStr = :erlang.binary_to_list(nodeBin)
        pidStr = get_pid_from_name(nameStr, nodeStr)
        refStr = :erlang.binary_to_list(refBin)
        {pidStr, '{' ++ nameStr ++ ',' ++ nodeStr ++ '} (' ++ refStr ++ ')'}
    end
  end

  defp get_pid_from_name(name, node) do
    case :ets.lookup(
           :cdv_reg_proc_table,
           :cdv_dump_node_name
         ) do
      [{_, ^node}] ->
        case :ets.lookup(:cdv_reg_proc_table, name) do
          [{_, pid}] when is_pid(pid) ->
            :erlang.pid_to_list(pid)

          _ ->
            '<unkonwn_pid>'
        end

      _ ->
        '<unknown_pid_other_node>'
    end
  end

  defp maybe_other_node(id) do
    channel =
      case split(?., id) do
        {'<' ++ n, _Rest} ->
          n

        {'#Port<' ++ n, _Rest} ->
          n

        {_, []} ->
          :not_found
      end

    maybe_other_node2(channel)
  end

  defp maybe_other_node2(:not_found) do
    :not_found
  end

  defp maybe_other_node2(channel) do
    ms =
      :ets.fun2ms(fn
        {{tag, start}, ch}
        when tag === :visible_node and ch === channel ->
          {'Visible Node', start}

        {{tag, start}, ch}
        when tag === :hidden_node and
               ch === channel ->
          {'Hidden Node', start}

        {{tag, start}, ch}
        when tag === :not_connected and
               ch === channel ->
          {'Not Connected Node', start}
      end)

    case :ets.select(:cdv_dump_index_table, ms) do
      [] ->
        :not_found

      [_] ->
        {:other_node, channel}
    end
  end

  defp expand_memory(fd, pid, dumpVsn) do
    decodeOpts = get_decode_opts(dumpVsn)
    :erlang.put(:fd, fd)
    dict0 = get_literals(fd, decodeOpts)
    dict = read_heap(fd, pid, decodeOpts, dict0)

    expanded =
      {read_stack_dump(fd, pid, decodeOpts, dict), read_messages(fd, pid, decodeOpts, dict),
       read_dictionary(fd, pid, decodeOpts, dict)}

    :erlang.erase(:fd)

    incompleteWarning =
      case :erlang.erase(:incomplete_heap) do
        :undefined ->
          []

        true ->
          ['WARNING: This process has an incomplete heap. Some information might be missing.']
      end

    {expanded, incompleteWarning}
  end

  defp get_literals(fd, decodeOpts) do
    case :erlang.get(:literals) do
      :undefined ->
        oldFd = :erlang.put(:fd, fd)
        literals = read_literals(fd, decodeOpts)
        :erlang.put(:fd, oldFd)
        :erlang.put(:literals, literals)
        literals

      literals ->
        literals
    end
  end

  defp read_literals(fd, decodeOpts) do
    case lookup_index(:literals, []) do
      [{_, start}] ->
        [{_, chars}] =
          :ets.lookup(
            :cdv_heap_file_chars,
            :literals
          )

        init_progress('Reading literals', chars)
        pos_bof(fd, start)
        read_heap(decodeOpts, :gb_trees.empty())

      [] ->
        :gb_trees.empty()
    end
  end

  defp get_decode_opts(dumpVsn) do
    binAddrAdj =
      cond do
        dumpVsn < [0, 3] ->
          15 <<< 64

        true ->
          0
      end

    base64 = dumpVsn >= [0, 5]
    r_dec_opts(bin_addr_adj: binAddrAdj, base64: base64)
  end

  defp read_stack_dump(fd, pid, decodeOpts, dict) do
    case lookup_index(:proc_stack, pid) do
      [{_, start}] ->
        pos_bof(fd, start)
        read_stack_dump1(fd, decodeOpts, dict, [])

      [] ->
        []
    end
  end

  defp read_stack_dump1(fd, decodeOpts, dict, acc) do
    case bytes(fd) do
      '=' ++ _next_tag ->
        :lists.reverse(acc)

      line ->
        stack = parse_top(line, decodeOpts, dict)
        read_stack_dump1(fd, decodeOpts, dict, [stack | acc])
    end
  end

  defp parse_top(line0, decodeOpts, d) do
    {label, line1} = get_label(line0)
    {term, line, ^d} = parse_term(line1, decodeOpts, d)
    [] = skip_blanks(line)
    {label, term}
  end

  defp read_messages(fd, pid, decodeOpts, dict) do
    case lookup_index(:proc_messages, pid) do
      [{_, start}] ->
        pos_bof(fd, start)
        read_messages1(fd, decodeOpts, dict, [])

      [] ->
        []
    end
  end

  defp read_messages1(fd, decodeOpts, dict, acc) do
    case bytes(fd) do
      '=' ++ _next_tag ->
        :lists.reverse(acc)

      line ->
        msg = parse_message(line, decodeOpts, dict)
        read_messages1(fd, decodeOpts, dict, [msg | acc])
    end
  end

  defp parse_message(line0, decodeOpts, d) do
    {msg, ':' ++ line1, _} = parse_term(line0, decodeOpts, d)
    {token, line, _} = parse_term(line1, decodeOpts, d)
    [] = skip_blanks(line)
    {msg, token}
  end

  defp read_dictionary(fd, pid, decodeOpts, dict) do
    case lookup_index(:proc_dictionary, pid) do
      [{_, start}] ->
        pos_bof(fd, start)
        read_dictionary1(fd, decodeOpts, dict, [])

      [] ->
        []
    end
  end

  defp read_dictionary1(fd, decodeOpts, dict, acc) do
    case bytes(fd) do
      '=' ++ _next_tag ->
        :lists.reverse(acc)

      line ->
        msg = parse_dictionary(line, decodeOpts, dict)
        read_dictionary1(fd, decodeOpts, dict, [msg | acc])
    end
  end

  defp parse_dictionary(line0, decodeOpts, d) do
    {entry, line, _} = parse_term(line0, decodeOpts, d)
    [] = skip_blanks(line)
    entry
  end

  defp read_heap(fd, pid, decodeOpts, dict0) do
    case lookup_index(:proc_heap, pid) do
      [{_, pos}] ->
        [{_, chars}] = :ets.lookup(:cdv_heap_file_chars, pid)
        init_progress('Reading process heap', chars)
        pos_bof(fd, pos)
        read_heap(decodeOpts, dict0)

      [] ->
        dict0
    end
  end

  defp read_heap(decodeOpts, dict0) do
    lines0 = read_heap_lines()
    lineMap = :maps.from_list(lines0)
    :erlang.put(:line_map, lineMap)

    refcBins =
      for {_, <<"Yc", _::binary>>} = refc <- lines0 do
        refc
      end

    lines = refcBins ++ lines0
    init_progress('Processing terms', map_size(lineMap))
    dict = parse_heap_terms(lines, decodeOpts, dict0)
    :erlang.erase(:line_map)
    end_progress()
    dict
  end

  defp read_heap_lines() do
    read_heap_lines_1(:erlang.get(:fd), [])
  end

  defp read_heap_lines_1(fd, acc) do
    case bytes(fd) do
      '=' ++ _next_tag ->
        end_progress()
        :erlang.put(:fd, :end_of_heap)
        acc

      line0 ->
        update_progress(length(line0) + 1)
        {addr, ':' ++ line1} = get_hex(line0)
        line = :erlang.list_to_binary(line1)
        read_heap_lines_1(fd, [{addr, line} | acc])
    end
  end

  defp parse_heap_terms([{addr, line0} | t], decodeOpts, dict0) do
    case :gb_trees.is_defined(addr, dict0) do
      true ->
        parse_heap_terms(t, decodeOpts, dict0)

      false ->
        dict = parse_line(addr, line0, decodeOpts, dict0)
        parse_heap_terms(t, decodeOpts, dict)
    end
  end

  defp parse_heap_terms([], _DecodeOpts, dict) do
    dict
  end

  defp parse_line(addr, line0, decodeOpts, dict0) do
    update_progress(1)
    line1 = :erlang.binary_to_list(line0)
    {_Term, line, dict} = parse_heap_term(line1, addr, decodeOpts, dict0)
    [] = skip_blanks(line)
    dict
  end

  defp get_port(file, port) do
    case lookup_index(:port, port) do
      [{_, start}] ->
        fd = open(file)
        pos_bof(fd, start)
        r = get_portinfo(fd, r_port(id: port))
        close(fd)
        {:ok, r}

      [] ->
        maybe_other_node(port)
    end
  end

  defp get_ports(file) do
    parseFun = fn fd, id ->
      get_portinfo(fd, r_port(id: port_to_tuple(id)))
    end

    lookup_and_parse_index(file, :port, parseFun, 'ports')
  end

  defp port_to_tuple('#Port<' ++ port) do
    [i1, i2] = :string.lexemes(port, '.>')
    {:erlang.list_to_integer(i1), :erlang.list_to_integer(i2)}
  end

  defp get_portinfo(fd, port) do
    case line_head(fd) do
      'State' ->
        get_portinfo(fd, r_port(port, state: bytes(fd)))

      'Task Flags' ->
        get_portinfo(fd, r_port(port, task_flags: bytes(fd)))

      'Slot' ->
        get_portinfo(
          fd,
          r_port(port, slot: :erlang.list_to_integer(bytes(fd)))
        )

      'Connected' ->
        connected0 = bytes(fd)

        connected =
          try do
            :erlang.list_to_pid(connected0)
          catch
            :error, :badarg ->
              connected0
          end

        get_portinfo(fd, r_port(port, connected: connected))

      'Links' ->
        pids = split_pid_list_no_space(bytes(fd))

        links =
          for pid <- pids do
            {pid, pid}
          end

        get_portinfo(fd, r_port(port, links: links))

      'Registered as' ->
        get_portinfo(fd, r_port(port, name: string(fd)))

      'Monitors' ->
        monitors0 = :string.lexemes(bytes(fd), '()')

        monitors =
          for mon <- monitors0 do
            [pid, ref] = :string.lexemes(mon, ',')
            {pid, pid ++ ' (' ++ ref ++ ')'}
          end

        get_portinfo(fd, r_port(port, monitors: monitors))

      'Suspended' ->
        pids = split_pid_list_no_space(bytes(fd))

        suspended =
          for pid <- pids do
            {pid, pid}
          end

        get_portinfo(fd, r_port(port, suspended: suspended))

      'Port controls linked-in driver' ->
        str = :lists.flatten(['Linked in driver: ' | string(fd)])
        get_portinfo(fd, r_port(port, controls: str))

      'Port controls forker process' ->
        str = :lists.flatten(['Forker process: ' | string(fd)])
        get_portinfo(fd, r_port(port, controls: str))

      'Port controls external process' ->
        str = :lists.flatten(['External proc: ' | string(fd)])
        get_portinfo(fd, r_port(port, controls: str))

      'Port is a file' ->
        str = :lists.flatten(['File: ' | string(fd)])
        get_portinfo(fd, r_port(port, controls: str))

      'Port is UNIX fd not opened by emulator' ->
        str = :lists.flatten(['UNIX fd not opened by emulator: ' | string(fd)])
        get_portinfo(fd, r_port(port, controls: str))

      'Input' ->
        get_portinfo(
          fd,
          r_port(port, input: :erlang.list_to_integer(bytes(fd)))
        )

      'Output' ->
        get_portinfo(
          fd,
          r_port(port, output: :erlang.list_to_integer(bytes(fd)))
        )

      'Queue' ->
        get_portinfo(
          fd,
          r_port(port, queue: :erlang.list_to_integer(bytes(fd)))
        )

      'Port Data' ->
        get_portinfo(fd, r_port(port, port_data: string(fd)))

      '=' ++ _next_tag ->
        port

      other ->
        unexpected(fd, other, 'port info')
        port
    end
  end

  defp split_pid_list_no_space(string) do
    split_pid_list_no_space(string, [], [])
  end

  defp split_pid_list_no_space([?> | rest], acc, pids) do
    split_pid_list_no_space(rest, [], [:lists.reverse(acc, [?>]) | pids])
  end

  defp split_pid_list_no_space([h | t], acc, pids) do
    split_pid_list_no_space(t, [h | acc], pids)
  end

  defp split_pid_list_no_space([], [], pids) do
    :lists.reverse(pids)
  end

  defp get_ets_tables(file, pid, wS) do
    parseFun = fn fd, id ->
      eT = get_etsinfo(fd, r_ets_table(pid: :erlang.list_to_pid(id)), wS)
      r_ets_table(eT, is_named: tab_is_named(eT))
    end

    lookup_and_parse_index(file, {:ets, pid}, parseFun, 'ets')
  end

  defp tab_is_named(r_ets_table(id: name, name: name)) do
    'yes'
  end

  defp tab_is_named(r_ets_table()) do
    'no'
  end

  defp get_etsinfo(fd, etsTable = r_ets_table(details: ds), wS) do
    case line_head(fd) do
      'Slot' ->
        get_etsinfo(
          fd,
          r_ets_table(etsTable, slot: :erlang.list_to_integer(bytes(fd))),
          wS
        )

      'Table' ->
        get_etsinfo(fd, r_ets_table(etsTable, id: string(fd)), wS)

      'Name' ->
        get_etsinfo(fd, r_ets_table(etsTable, name: string(fd)), wS)

      'Ordered set (AVL tree), Elements' ->
        skip_rest_of_line(fd)
        get_etsinfo(fd, r_ets_table(etsTable, data_type: 'tree'), wS)

      'Buckets' ->
        buckets = :erlang.list_to_integer(:string.trim(bytes(fd), :both, ' '))
        get_etsinfo(fd, r_ets_table(etsTable, buckets: buckets), wS)

      'Objects' ->
        get_etsinfo(
          fd,
          r_ets_table(etsTable, size: :erlang.list_to_integer(bytes(fd))),
          wS
        )

      'Words' ->
        words = :erlang.list_to_integer(bytes(fd))

        bytes =
          case words do
            -1 ->
              -1

            _ ->
              words * wS
          end

        get_etsinfo(fd, r_ets_table(etsTable, memory: {:bytes, bytes}), wS)

      '=' ++ _next_tag ->
        etsTable

      'Chain Length Min' ->
        val = bytes(fd)
        get_etsinfo(fd, r_ets_table(etsTable, details: %{ds | :chain_min => val}), wS)

      'Chain Length Avg' ->
        val =
          try do
            :erlang.list_to_float(:string.trim(bytes(fd), :both, ' '))
          catch
            _, _ ->
              '-'
          end

        get_etsinfo(fd, r_ets_table(etsTable, details: %{ds | :chain_avg => val}), wS)

      'Chain Length Max' ->
        val = bytes(fd)
        get_etsinfo(fd, r_ets_table(etsTable, details: %{ds | :chain_max => val}), wS)

      'Chain Length Std Dev' ->
        val = bytes(fd)
        get_etsinfo(fd, r_ets_table(etsTable, details: %{ds | :chain_stddev => val}), wS)

      'Chain Length Expected Std Dev' ->
        val = bytes(fd)

        get_etsinfo(
          fd,
          r_ets_table(etsTable, details: %{ds | :chain_exp_stddev => val}),
          wS
        )

      'Fixed' ->
        val = bytes(fd)
        get_etsinfo(fd, r_ets_table(etsTable, details: %{ds | :fixed => val}), wS)

      'Type' ->
        val = bytes(fd)
        get_etsinfo(fd, r_ets_table(etsTable, data_type: val), wS)

      'Protection' ->
        val = bytes(fd)
        get_etsinfo(fd, r_ets_table(etsTable, details: %{ds | :protection => val}), wS)

      'Compressed' ->
        val = bytes(fd)
        get_etsinfo(fd, r_ets_table(etsTable, details: %{ds | :compressed => val}), wS)

      'Write Concurrency' ->
        val = bytes(fd)
        get_etsinfo(fd, r_ets_table(etsTable, details: %{ds | :write_c => val}), wS)

      'Read Concurrency' ->
        val = bytes(fd)
        get_etsinfo(fd, r_ets_table(etsTable, details: %{ds | :read_c => val}), wS)

      other ->
        unexpected(fd, other, 'ETS info')
        etsTable
    end
  end

  defp get_internal_ets_tables(file, wS) do
    internalEts = lookup_index(:internal_ets)
    fd = open(file)

    r =
      :lists.map(
        fn {descr, start} ->
          pos_bof(fd, start)
          {descr, get_etsinfo(fd, r_ets_table(), wS)}
        end,
        internalEts
      )

    close(fd)
    r
  end

  defp get_timers(file, pid) do
    parseFun = fn fd, id ->
      get_timerinfo(fd, id)
    end

    t1 = lookup_and_parse_index(file, {:timer, pid}, parseFun, 'timers')

    t2 =
      case :ets.lookup(:cdv_reg_proc_table, pid) do
        [{_, name}] ->
          lookup_and_parse_index(file, {:timer, name}, parseFun, 'timers')

        _ ->
          []
      end

    t1 ++ t2
  end

  defp get_timerinfo(fd, id) do
    case (try do
            :erlang.list_to_pid(id)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      pid when is_pid(pid) ->
        get_timerinfo_1(fd, r_timer(pid: pid))

      _ ->
        case :ets.lookup(:cdv_reg_proc_table, id) do
          [{_, pid}] when is_pid(pid) ->
            get_timerinfo_1(fd, r_timer(pid: pid, name: id))

          [] ->
            get_timerinfo_1(fd, r_timer(name: id))
        end
    end
  end

  defp get_timerinfo_1(fd, timer) do
    case line_head(fd) do
      'Message' ->
        get_timerinfo_1(fd, r_timer(timer, msg: string(fd)))

      'Time left' ->
        timeLeft = :erlang.list_to_integer(bytes(fd) -- ' ms')
        get_timerinfo_1(fd, r_timer(timer, time: timeLeft))

      '=' ++ _next_tag ->
        timer

      other ->
        unexpected(fd, other, 'timer info')
        timer
    end
  end

  defp get_node(file, channel) do
    ms =
      :ets.fun2ms(fn
        {{tag, start}, ch}
        when tag === :visible_node and ch === channel ->
          {:visible, start}

        {{tag, start}, ch}
        when tag === :hidden_node and
               ch === channel ->
          {:hidden, start}

        {{tag, start}, ch}
        when tag === :not_connected and
               ch === channel ->
          {:not_connected, start}
      end)

    case :ets.select(:cdv_dump_index_table, ms) do
      [] ->
        {:error, :not_found}

      [{type, pos}] ->
        fd = open(file)
        nodeInfo = get_nodeinfo(fd, channel, type, pos)
        close(fd)
        {:ok, nodeInfo}
    end
  end

  defp nods(file) do
    case lookup_index(:no_distribution) do
      [] ->
        v = lookup_index(:visible_node)
        h = lookup_index(:hidden_node)
        n = lookup_index(:not_connected)
        fd = open(file)

        visible =
          :lists.map(
            fn {channel, start} ->
              get_nodeinfo(fd, channel, :visible, start)
            end,
            v
          )

        hidden =
          :lists.map(
            fn {channel, start} ->
              get_nodeinfo(fd, channel, :hidden, start)
            end,
            h
          )

        notConnected =
          :lists.map(
            fn {channel, start} ->
              get_nodeinfo(fd, channel, :not_connected, start)
            end,
            n
          )

        close(fd)
        visible ++ hidden ++ notConnected

      [_] ->
        []
    end
  end

  defp get_nodeinfo(fd, channel, type, start) do
    pos_bof(fd, start)

    get_nodeinfo(
      fd,
      r_nod(
        channel: :erlang.list_to_integer(channel),
        conn_type: type
      )
    )
  end

  defp get_nodeinfo(fd, nod) do
    case line_head(fd) do
      'Name' ->
        get_nodeinfo(fd, r_nod(nod, name: bytes(fd)))

      'Controller' ->
        get_nodeinfo(fd, r_nod(nod, controller: bytes(fd)))

      'Creation' ->
        creations =
          :lists.flatmap(
            fn c ->
              try do
                [:erlang.list_to_integer(c)]
              catch
                :error, :badarg ->
                  []
              end
            end,
            :string.lexemes(bytes(fd), ' ')
          )

        get_nodeinfo(
          fd,
          r_nod(nod, creation: {:creations, creations})
        )

      'Remote link' ->
        procs = bytes(fd)
        {local, remote} = split(procs)
        str = local ++ ' <-> ' ++ remote
        newRemLinks = [{local, str} | r_nod(nod, :remote_links)]
        get_nodeinfo(fd, r_nod(nod, remote_links: newRemLinks))

      'Remote monitoring' ->
        procs = bytes(fd)
        {local, remote} = split(procs)
        str = local ++ ' -> ' ++ remote
        newRemMon = [{local, str} | r_nod(nod, :remote_mon)]
        get_nodeinfo(fd, r_nod(nod, remote_mon: newRemMon))

      'Remotely monitored by' ->
        procs = bytes(fd)
        {local, remote} = split(procs)
        str = local ++ ' <- ' ++ remote
        newRemMonBy = [{local, str} | r_nod(nod, :remote_mon_by)]
        get_nodeinfo(fd, r_nod(nod, remote_mon_by: newRemMonBy))

      'Error' ->
        get_nodeinfo(fd, r_nod(nod, error: 'ERROR: ' ++ string(fd)))

      '=' ++ _next_tag ->
        nod

      other ->
        unexpected(fd, other, 'node info')
        nod
    end
  end

  defp get_loaded_mod_details(file, mod, decodeOpts) do
    [{_, start}] = lookup_index(:mod, mod)
    fd = open(file)
    pos_bof(fd, start)
    initLM = r_loaded_mod(mod: mod, old_size: 'No old code exists')

    fun = fn f, lM, lineHead ->
      all_modinfo(f, lM, lineHead, decodeOpts)
    end

    modInfo = get_loaded_mod_info(fd, initLM, fun)
    close(fd)
    modInfo
  end

  defp loaded_mods(file) do
    parseFun = fn fd, id ->
      get_loaded_mod_info(
        fd,
        r_loaded_mod(mod: get_atom(:erlang.list_to_binary(id))),
        &main_modinfo/3
      )
    end

    {cC, oC} =
      case lookup_index(:loaded_modules) do
        [{_, startTotal}] ->
          fd = open(file)
          pos_bof(fd, startTotal)
          r = get_loaded_mod_totals(fd, {'unknown', 'unknown'})
          close(fd)
          r

        [] ->
          {'unknown', 'unknown'}
      end

    {cC, oC, lookup_and_parse_index(file, :mod, parseFun, 'modules')}
  end

  defp get_loaded_mod_totals(fd, {cC, oC}) do
    case line_head(fd) do
      'Current code' ->
        get_loaded_mod_totals(fd, {bytes(fd), oC})

      'Old code' ->
        get_loaded_mod_totals(fd, {cC, bytes(fd)})

      '=' ++ _next_tag ->
        {cC, oC}

      other ->
        unexpected(fd, other, 'loaded modules info')
        {cC, oC}
    end
  end

  defp get_loaded_mod_info(fd, lM, fun) do
    case line_head(fd) do
      'Current size' ->
        cS = :erlang.list_to_integer(bytes(fd))
        get_loaded_mod_info(fd, r_loaded_mod(lM, current_size: cS), fun)

      'Old size' ->
        oS = :erlang.list_to_integer(bytes(fd))
        get_loaded_mod_info(fd, r_loaded_mod(lM, old_size: oS), fun)

      '=' ++ _next_tag ->
        lM

      {:eof, _} ->
        lM

      other ->
        lM1 = fun.(fd, lM, other)
        get_loaded_mod_info(fd, lM1, fun)
    end
  end

  defp main_modinfo(_Fd, lM, _LineHead) do
    lM
  end

  defp all_modinfo(fd, lM, lineHead, decodeOpts) do
    case lineHead do
      'Current attributes' ->
        str = get_attribute(fd, decodeOpts)
        r_loaded_mod(lM, current_attrib: str)

      'Current compilation info' ->
        str = get_attribute(fd, decodeOpts)
        r_loaded_mod(lM, current_comp_info: str)

      'Old attributes' ->
        str = get_attribute(fd, decodeOpts)
        r_loaded_mod(lM, old_attrib: str)

      'Old compilation info' ->
        str = get_attribute(fd, decodeOpts)
        r_loaded_mod(lM, old_comp_info: str)

      other ->
        unexpected(fd, other, 'loaded modules info')
        lM
    end
  end

  defp get_attribute(fd, decodeOpts) do
    term = do_get_attribute(fd, decodeOpts)
    :io_lib.format('~tp~n', [term])
  end

  defp do_get_attribute(fd, decodeOpts) do
    bytes = bytes(fd, '')

    try do
      get_binary(bytes, decodeOpts)
    catch
      _, _ ->
        {'WARNING: The term is probably truncated!', 'I cannot convert to binary.', bytes}
    else
      {bin, _} ->
        try do
          :erlang.binary_to_term(bin)
        catch
          _, _ ->
            {'WARNING: The term is probably truncated!', 'I cannot do binary_to_term/1.', bin}
        else
          term ->
            term
        end
    end
  end

  defp funs(file) do
    parseFun = fn fd, _Id ->
      get_funinfo(fd, r_fu())
    end

    lookup_and_parse_index(file, :fu, parseFun, 'funs')
  end

  defp get_funinfo(fd, fu) do
    case line_head(fd) do
      'Module' ->
        get_funinfo(fd, r_fu(fu, module: bytes(fd)))

      'Uniq' ->
        get_funinfo(
          fd,
          r_fu(fu, uniq: :erlang.list_to_integer(bytes(fd)))
        )

      'Index' ->
        get_funinfo(
          fd,
          r_fu(fu, index: :erlang.list_to_integer(bytes(fd)))
        )

      'Address' ->
        get_funinfo(fd, r_fu(fu, address: bytes(fd)))

      'Native_address' ->
        get_funinfo(fd, r_fu(fu, native_address: bytes(fd)))

      'Refc' ->
        get_funinfo(
          fd,
          r_fu(fu, refc: :erlang.list_to_integer(bytes(fd)))
        )

      '=' ++ _next_tag ->
        fu

      other ->
        unexpected(fd, other, 'fun info')
        fu
    end
  end

  defp atoms(file, numAtoms) do
    case lookup_index(:atoms) do
      [{_Id, start}] ->
        fd = open(file)
        pos_bof(fd, start)
        get_atoms(fd, numAtoms)

      _ ->
        []
    end
  end

  defp get_atoms(fd, numAtoms) do
    case get_chunk(fd) do
      {:ok, bin} ->
        init_progress('Processing atoms', numAtoms)
        get_atoms(fd, bin, numAtoms, [])

      :eof ->
        []
    end
  end

  defp get_atoms(fd, bin, numAtoms, atoms) do
    bins = :binary.split(bin, "\n", [:global])
    get_atoms1(fd, bins, numAtoms, atoms)
  end

  defp get_atoms1(_Fd, [<<"=", _::binary>> | _], _N, atoms) do
    end_progress()
    atoms
  end

  defp get_atoms1(fd, [lastBin], n, atoms) do
    case get_chunk(fd) do
      {:ok, bin0} ->
        get_atoms(fd, <<lastBin::binary, bin0::binary>>, n, atoms)

      :eof ->
        end_progress()
        [{n, get_atom(lastBin)} | atoms]
    end
  end

  defp get_atoms1(fd, [bin | bins], n, atoms) do
    update_progress()
    get_atoms1(fd, bins, n - 1, [{n, get_atom(bin)} | atoms])
  end

  defp get_atom(<<"'", atom::binary>>) do
    {atom, :q}
  end

  defp get_atom(atom) when is_binary(atom) do
    {atom, :nq}
  end

  defp persistent_terms(file, decodeOpts) do
    case lookup_index(:persistent_terms) do
      [{_Id, start}] ->
        fd = open(file)
        pos_bof(fd, start)
        terms = get_persistent_terms(fd)
        dict = get_literals(fd, decodeOpts)
        parse_persistent_terms(terms, decodeOpts, dict)

      _ ->
        []
    end
  end

  defp parse_persistent_terms([[name0, val0] | terms], decodeOpts, dict) do
    {name, _, _} = parse_term(:erlang.binary_to_list(name0), decodeOpts, dict)
    {val, _, _} = parse_term(:erlang.binary_to_list(val0), decodeOpts, dict)
    [{name, val} | parse_persistent_terms(terms, decodeOpts, dict)]
  end

  defp parse_persistent_terms([], _, _) do
    []
  end

  defp get_persistent_terms(fd) do
    case get_chunk(fd) do
      {:ok, bin} ->
        get_persistent_terms(fd, bin, [])

      :eof ->
        []
    end
  end

  defp get_persistent_terms(fd, bin, persistentTerms) do
    bins = :binary.split(bin, "\n", [:global])
    get_persistent_terms1(fd, bins, persistentTerms)
  end

  defp get_persistent_terms1(_Fd, [<<"=", _::binary>> | _], persistentTerms) do
    persistentTerms
  end

  defp get_persistent_terms1(fd, [lastBin], persistentTerms) do
    case get_chunk(fd) do
      {:ok, bin0} ->
        get_persistent_terms(
          fd,
          <<lastBin::binary, bin0::binary>>,
          persistentTerms
        )

      :eof ->
        [get_persistent_term(lastBin) | persistentTerms]
    end
  end

  defp get_persistent_terms1(fd, [bin | bins], persistent_Terms) do
    get_persistent_terms1(fd, bins, [get_persistent_term(bin) | persistent_Terms])
  end

  defp get_persistent_term(bin) do
    :binary.split(bin, "|")
  end

  defp memory(file) do
    case lookup_index(:memory) do
      [{_, start}] ->
        fd = open(file)
        pos_bof(fd, start)
        r = get_meminfo(fd, [])
        close(fd)
        r

      _ ->
        []
    end
  end

  defp get_meminfo(fd, acc) do
    case line_head(fd) do
      '=' ++ _next_tag ->
        :lists.reverse(acc)

      {:eof, _last_line} ->
        :lists.reverse(acc)

      key ->
        get_meminfo(
          fd,
          [{:erlang.list_to_atom(key), bytes(fd)} | acc]
        )
    end
  end

  defp allocated_areas(file) do
    case lookup_index(:allocated_areas) do
      [{_, start}] ->
        fd = open(file)
        pos_bof(fd, start)
        r = get_allocareainfo(fd, [])
        close(fd)
        r

      _ ->
        []
    end
  end

  defp get_allocareainfo(fd, acc) do
    case line_head(fd) do
      '=' ++ _next_tag ->
        :lists.reverse(acc)

      {:eof, _last_line} ->
        :lists.reverse(acc)

      key ->
        val = bytes(fd)

        allocInfo =
          case split(val) do
            {alloc, []} ->
              {key, alloc, ''}

            {alloc, used} ->
              {key, alloc, used}
          end

        get_allocareainfo(fd, [allocInfo | acc])
    end
  end

  defp allocator_info(file) do
    case lookup_index(:allocator) do
      [] ->
        []

      allAllocators ->
        fd = open(file)

        r =
          :lists.map(
            fn {heading, start} ->
              {heading, get_allocatorinfo(fd, start)}
            end,
            allAllocators
          )

        close(fd)
        [allocator_summary(r) | r]
    end
  end

  defp get_allocatorinfo(fd, start) do
    pos_bof(fd, start)
    get_allocatorinfo1(fd, [], 0)
  end

  defp get_allocatorinfo1(fd, acc, max) do
    case line_head(fd) do
      '=' ++ _next_tag ->
        pad_and_reverse(acc, max, [])

      {:eof, _last_line} ->
        pad_and_reverse(acc, max, [])

      key ->
        values = get_all_vals(bytes(fd), [])
        l = length(values)

        max1 =
          cond do
            l > max ->
              l

            true ->
              max
          end

        get_allocatorinfo1(fd, [{key, values} | acc], max1)
    end
  end

  defp get_all_vals([?\s | rest], acc) do
    [:lists.reverse(acc) | get_all_vals(rest, [])]
  end

  defp get_all_vals([], acc) do
    [:lists.reverse(acc)]
  end

  defp get_all_vals([char | rest], acc) do
    get_all_vals(rest, [char | acc])
  end

  defp pad_and_reverse([{k, v} | t], len, rev) do
    vLen = length(v)

    v1 =
      cond do
        vLen == len ->
          v

        true ->
          v ++ :lists.duplicate(len - vLen, '')
      end

    pad_and_reverse(t, len, [{k, v1} | rev])
  end

  defp pad_and_reverse([], _, rev) do
    rev
  end

  defp allocator_summary(allocators) do
    {sorted, doTotal} = sort_allocator_types(allocators, [], true)

    {typeTotals0, totals} =
      sum_allocator_data(
        sorted,
        doTotal
      )

    {totalMCS, typeTotals} =
      case :lists.keytake('mseg_alloc', 1, typeTotals0) do
        {:value, {_, [{'segments_size', segSize}]}, rest} ->
          {:erlang.integer_to_list(segSize), rest}

        false ->
          {'N/A', typeTotals0}
      end

    {totalBS, totalCS} =
      case totals do
        false ->
          {'N/A', 'N/A'}

        {tBS, tCS} ->
          {:erlang.integer_to_list(tBS), :erlang.integer_to_list(tCS)}
      end

    {'Allocator Summary', ['blocks size', 'carriers size', 'mseg carriers size'],
     [
       {'total', [totalBS, totalCS, totalMCS]}
       | format_allocator_summary(:lists.reverse(typeTotals))
     ]}
  end

  defp format_allocator_summary([{type, data} | rest]) do
    [
      format_allocator_summary(type, data)
      | format_allocator_summary(rest)
    ]
  end

  defp format_allocator_summary([]) do
    []
  end

  defp format_allocator_summary(type, data) do
    bS = get_size_value(:blocks_size, data)
    cS = get_size_value(:carriers_size, data)
    mCS = get_size_value(:mseg_carriers_size, data)
    {type, [bS, cS, mCS]}
  end

  defp get_size_value(key, data) do
    case :proplists.get_value(key, data) do
      :undefined ->
        'N/A'

      int ->
        :erlang.integer_to_list(int)
    end
  end

  defp sort_allocator_types([{name, data} | allocators], acc, doTotal) do
    type =
      case :string.lexemes(name, '[]') do
        [t, _Id] ->
          t

        [^name] ->
          name

        other ->
          other
      end

    typeData = :proplists.get_value(type, acc, [])
    {newTypeData, newDoTotal} = sort_type_data(type, data, typeData, doTotal)
    newAcc = :lists.keystore(type, 1, acc, {type, newTypeData})
    sort_allocator_types(allocators, newAcc, newDoTotal)
  end

  defp sort_allocator_types([], acc, doTotal) do
    {acc, doTotal}
  end

  defp sort_type_data(type, [{'option e', 'false'} | data], acc, _)
       when type !== 'sbmbc_alloc' do
    sort_type_data(type, data, acc, false)
  end

  defp sort_type_data(type, [{key, val0} | data], acc, doTotal) do
    case :lists.member(
           key,
           [
             'sbmbcs blocks size',
             'mbcs blocks size',
             'mbcs_pool blocks size',
             'sbcs blocks size',
             'sbmbcs carriers size',
             'mbcs carriers size',
             'sbcs carriers size',
             'mbcs mseg carriers size',
             'mbcs_pool carriers size',
             'sbcs mseg carriers size',
             'segments_size'
           ]
         ) do
      true ->
        val = :erlang.list_to_integer(hd(val0))
        sort_type_data(type, data, update_value(key, val, acc), doTotal)

      false ->
        sort_type_data(type, data, acc, doTotal)
    end
  end

  defp sort_type_data(_Type, [], acc, doTotal) do
    {acc, doTotal}
  end

  defp sum_allocator_data(allocData, false) do
    sum_allocator_data(allocData, [], false)
  end

  defp sum_allocator_data(allocData, true) do
    sum_allocator_data(allocData, [], {0, 0})
  end

  defp sum_allocator_data([{_Type, []} | allocData], typeAcc, total) do
    sum_allocator_data(allocData, typeAcc, total)
  end

  defp sum_allocator_data([{type, data} | allocData], typeAcc, total) do
    {typeSum, newTotal} = sum_type_data(data, [], total)
    sum_allocator_data(allocData, [{type, typeSum} | typeAcc], newTotal)
  end

  defp sum_allocator_data([], typeAcc, total) do
    {typeAcc, total}
  end

  defp sum_type_data([{key, value} | data], typeAcc, total) do
    newTotal =
      case total do
        false ->
          false

        {totalBS, totalCS} ->
          case :lists.member(key, [
                 'mbcs blocks size',
                 'mbcs_pool blocks size',
                 'sbcs blocks size'
               ]) do
            true ->
              {totalBS + value, totalCS}

            false ->
              case :lists.member(key, [
                     'mbcs carriers size',
                     'mbcs_pool carriers size',
                     'sbcs carriers size'
                   ]) do
                true ->
                  {totalBS, totalCS + value}

                false ->
                  {totalBS, totalCS}
              end
          end
      end

    newTypeAcc =
      case :lists.member(key, [
             'sbmbcs blocks size',
             'mbcs blocks size',
             'mbcs_pool blocks size',
             'sbcs blocks size'
           ]) do
        true ->
          update_value(:blocks_size, value, typeAcc)

        false ->
          case :lists.member(key, [
                 'sbmbcs carriers size',
                 'mbcs carriers size',
                 'mbcs_pool carriers size',
                 'sbcs carriers size'
               ]) do
            true ->
              update_value(:carriers_size, value, typeAcc)

            false ->
              case :lists.member(key, ['mbcs mseg carriers size', 'sbcs mseg carriers size']) do
                true ->
                  update_value(:mseg_carriers_size, value, typeAcc)

                false ->
                  update_value(key, value, typeAcc)
              end
          end
      end

    sum_type_data(data, newTypeAcc, newTotal)
  end

  defp sum_type_data([], typeAcc, total) do
    {typeAcc, total}
  end

  defp update_value(key, value, acc) do
    case :lists.keytake(key, 1, acc) do
      false ->
        [{key, value} | acc]

      {:value, {^key, old}, acc1} ->
        [{key, old + value} | acc1]
    end
  end

  defp hash_tables(file) do
    case lookup_index(:hash_table) do
      [] ->
        []

      allHashTables ->
        fd = open(file)

        r =
          :lists.map(
            fn {name, start} ->
              get_hashtableinfo(fd, name, start)
            end,
            allHashTables
          )

        close(fd)
        r
    end
  end

  defp get_hashtableinfo(fd, name, start) do
    pos_bof(fd, start)
    get_hashtableinfo1(fd, r_hash_table(name: name))
  end

  defp get_hashtableinfo1(fd, hashTable) do
    case line_head(fd) do
      'size' ->
        get_hashtableinfo1(fd, r_hash_table(hashTable, size: bytes(fd)))

      'used' ->
        get_hashtableinfo1(fd, r_hash_table(hashTable, used: bytes(fd)))

      'objs' ->
        get_hashtableinfo1(fd, r_hash_table(hashTable, objs: bytes(fd)))

      'depth' ->
        get_hashtableinfo1(fd, r_hash_table(hashTable, depth: bytes(fd)))

      '=' ++ _next_tag ->
        hashTable

      other ->
        unexpected(fd, other, 'hash table information')
        hashTable
    end
  end

  defp index_tables(file) do
    case lookup_index(:index_table) do
      [] ->
        []

      allIndexTables ->
        fd = open(file)

        r =
          :lists.map(
            fn {name, start} ->
              get_indextableinfo(fd, name, start)
            end,
            allIndexTables
          )

        close(fd)
        r
    end
  end

  defp get_indextableinfo(fd, name, start) do
    pos_bof(fd, start)
    get_indextableinfo1(fd, r_index_table(name: name))
  end

  defp get_indextableinfo1(fd, indexTable) do
    case line_head(fd) do
      'size' ->
        get_indextableinfo1(fd, r_index_table(indexTable, size: bytes(fd)))

      'used' ->
        get_indextableinfo1(fd, r_index_table(indexTable, used: bytes(fd)))

      'limit' ->
        get_indextableinfo1(fd, r_index_table(indexTable, limit: bytes(fd)))

      'rate' ->
        get_indextableinfo1(fd, r_index_table(indexTable, rate: bytes(fd)))

      'entries' ->
        get_indextableinfo1(
          fd,
          r_index_table(indexTable, entries: bytes(fd))
        )

      '=' ++ _next_tag ->
        indexTable

      other ->
        unexpected(fd, other, 'index table information')
        indexTable
    end
  end

  defp schedulers(file) do
    fd = open(file)

    schds0 =
      case lookup_index(:scheduler) do
        [] ->
          []

        normals ->
          [{normals, r_sched(type: :normal)}]
      end

    schds1 =
      case lookup_index(:dirty_cpu_scheduler) do
        [] ->
          schds0

        dirtyCpus ->
          [
            {dirtyCpus, get_dirty_runqueue(fd, :dirty_cpu_run_queue)}
            | schds0
          ]
      end

    schds2 =
      case lookup_index(:dirty_io_scheduler) do
        [] ->
          schds1

        dirtyIos ->
          [
            {dirtyIos, get_dirty_runqueue(fd, :dirty_io_run_queue)}
            | schds1
          ]
      end

    r = schedulers1(fd, schds2, [])
    close(fd)
    r
  end

  defp schedulers1(_Fd, [], acc) do
    acc
  end

  defp schedulers1(fd, [{scheds, sched0} | tail], acc0) do
    acc1 =
      :lists.foldl(
        fn {name, start}, accIn ->
          [
            get_schedulerinfo(fd, name, start, sched0)
            | accIn
          ]
        end,
        acc0,
        scheds
      )

    schedulers1(fd, tail, acc1)
  end

  defp get_schedulerinfo(fd, name, start, sched0) do
    pos_bof(fd, start)

    get_schedulerinfo1(
      fd,
      r_sched(sched0, name: :erlang.list_to_integer(name))
    )
  end

  defp sched_type(:dirty_cpu_run_queue) do
    :dirty_cpu
  end

  defp sched_type(:dirty_io_run_queue) do
    :dirty_io
  end

  defp get_schedulerinfo1(fd, sched) do
    case get_schedulerinfo2(fd, sched) do
      {:more, sched2} ->
        get_schedulerinfo1(fd, sched2)

      {:done, sched2} ->
        sched2
    end
  end

  defp get_schedulerinfo2(fd, sched = r_sched(details: ds)) do
    case line_head(fd) do
      'Current Process' ->
        {:more, r_sched(sched, process: bytes(fd, 'None'))}

      'Current Port' ->
        {:more, r_sched(sched, port: bytes(fd, 'None'))}

      'Scheduler Sleep Info Flags' ->
        {:more, r_sched(sched, details: %{ds | :sleep_info => bytes(fd, 'None')})}

      'Scheduler Sleep Info Aux Work' ->
        {:more, r_sched(sched, details: %{ds | :sleep_aux => bytes(fd, 'None')})}

      'Current Process State' ->
        {:more, r_sched(sched, details: %{ds | :currp_state => bytes(fd)})}

      'Current Process Internal State' ->
        {:more, r_sched(sched, details: %{ds | :currp_int_state => bytes(fd)})}

      'Current Process Program counter' ->
        {:more, r_sched(sched, details: %{ds | :currp_prg_cnt => string(fd)})}

      'Current Process CP' ->
        {:more, r_sched(sched, details: %{ds | :currp_cp => string(fd)})}

      'Current Process Limited Stack Trace' ->
        {:done, r_sched(sched, details: get_limited_stack(fd, 0, ds))}

      '=' ++ _next_tag ->
        {:done, sched}

      other ->
        case r_sched(sched, :type) do
          :normal ->
            get_runqueue_info2(fd, other, sched)

          _ ->
            unexpected(fd, other, 'dirty scheduler information')
            {:done, sched}
        end
    end
  end

  defp get_dirty_runqueue(fd, tag) do
    case lookup_index(tag) do
      [{_, start}] ->
        pos_bof(fd, start)
        get_runqueue_info1(fd, r_sched(type: sched_type(tag)))

      [] ->
        r_sched()
    end
  end

  defp get_runqueue_info1(fd, sched) do
    case get_runqueue_info2(fd, line_head(fd), sched) do
      {:more, sched2} ->
        get_runqueue_info1(fd, sched2)

      {:done, sched2} ->
        sched2
    end
  end

  defp get_runqueue_info2(fd, lineHead, sched = r_sched(details: ds)) do
    case lineHead do
      'Run Queue Max Length' ->
        rQMax = :erlang.list_to_integer(bytes(fd))
        rQ = rQMax + r_sched(sched, :run_q)

        {:more,
         r_sched(sched,
           run_q: rQ,
           details: %{ds | :runq_max => rQMax}
         )}

      'Run Queue High Length' ->
        rQHigh = :erlang.list_to_integer(bytes(fd))
        rQ = rQHigh + r_sched(sched, :run_q)

        {:more,
         r_sched(sched,
           run_q: rQ,
           details: %{ds | :runq_high => rQHigh}
         )}

      'Run Queue Normal Length' ->
        rQNorm = :erlang.list_to_integer(bytes(fd))
        rQ = rQNorm + r_sched(sched, :run_q)

        {:more,
         r_sched(sched,
           run_q: rQ,
           details: %{ds | :runq_norm => rQNorm}
         )}

      'Run Queue Low Length' ->
        rQLow = :erlang.list_to_integer(bytes(fd))
        rQ = rQLow + r_sched(sched, :run_q)

        {:more,
         r_sched(sched,
           run_q: rQ,
           details: %{ds | :runq_low => rQLow}
         )}

      'Run Queue Port Length' ->
        rQ = :erlang.list_to_integer(bytes(fd))
        {:more, r_sched(sched, port_q: rQ)}

      'Run Queue Flags' ->
        {:more, r_sched(sched, details: %{ds | :runq_flags => bytes(fd, 'None')})}

      '=' ++ _next_tag ->
        {:done, sched}

      other ->
        unexpected(fd, other, 'scheduler information')
        {:done, sched}
    end
  end

  defp get_limited_stack(fd, n, ds) do
    case string(fd) do
      addr = '0x' ++ _ ->
        get_limited_stack(fd, n + 1, %{ds | {:currp_stack, n} => addr})

      '=' ++ _next_tag ->
        ds

      line ->
        get_limited_stack(fd, n + 1, %{ds | {:currp_stack, n} => line})
    end
  end

  defp parse_heap_term([?l | line0], addr, decodeOpts, d0) do
    {h, '|' ++ line1, d1} = parse_term(line0, decodeOpts, d0)
    {t, line, d2} = parse_term(line1, decodeOpts, d1)
    term = [h | t]
    d = :gb_trees.insert(addr, term, d2)
    {term, line, d}
  end

  defp parse_heap_term([?t | line0], addr, decodeOpts, d) do
    {n, ':' ++ line} = get_hex(line0)
    parse_tuple(n, line, addr, decodeOpts, d, [])
  end

  defp parse_heap_term([?F | line0], addr, _DecodeOpts, d0) do
    {n, ':' ++ line1} = get_hex(line0)
    {chars, line} = get_chars(n, line1)
    term = :erlang.list_to_float(chars)
    d = :gb_trees.insert(addr, term, d0)
    {term, line, d}
  end

  defp parse_heap_term('B16#' ++ line0, addr, _DecodeOpts, d0) do
    {term, line} = get_hex(line0)
    d = :gb_trees.insert(addr, term, d0)
    {term, line, d}
  end

  defp parse_heap_term('B-16#' ++ line0, addr, _DecodeOpts, d0) do
    {term0, line} = get_hex(line0)
    term = -term0
    d = :gb_trees.insert(addr, term, d0)
    {term, line, d}
  end

  defp parse_heap_term('B' ++ line0, addr, _DecodeOpts, d0) do
    case :string.to_integer(line0) do
      {int, line} when is_integer(int) ->
        d = :gb_trees.insert(addr, int, d0)
        {int, line, d}
    end
  end

  defp parse_heap_term([?P | line0], addr, _DecodeOpts, d0) do
    {pid0, line} = get_id(line0)
    pid = [:"#CDVPid" | pid0]
    d = :gb_trees.insert(addr, pid, d0)
    {pid, line, d}
  end

  defp parse_heap_term([?p | line0], addr, _DecodeOpts, d0) do
    {port0, line} = get_id(line0)
    port = [:"#CDVPort" | port0]
    d = :gb_trees.insert(addr, port, d0)
    {port, line, d}
  end

  defp parse_heap_term('E' ++ line0, addr, decodeOpts, d0) do
    {bin, line} = get_binary(line0, decodeOpts)
    term = :erlang.binary_to_term(bin)
    d = :gb_trees.insert(addr, term, d0)
    {term, line, d}
  end

  defp parse_heap_term('Yh' ++ line0, addr, decodeOpts, d0) do
    {term, line} = get_binary(line0, decodeOpts)
    d = :gb_trees.insert(addr, term, d0)
    {term, line, d}
  end

  defp parse_heap_term('Yc' ++ line0, addr, decodeOpts, d0) do
    {binp0, ':' ++ line1} = get_hex(line0)
    {offset, ':' ++ line2} = get_hex(line1)
    {sz, line} = get_hex(line2)
    binp = binp0 ||| r_dec_opts(decodeOpts, :bin_addr_adj)

    case lookup_binary_index(binp) do
      [{_, start}] ->
        symbolicBin = {:"#CDVBin", start}
        term = cdvbin(offset, sz, symbolicBin)
        d1 = :gb_trees.insert(addr, term, d0)
        d = :gb_trees.enter(binp, symbolicBin, d1)
        {term, line, d}

      [] ->
        term = :"#CDVNonexistingBinary"
        d1 = :gb_trees.insert(addr, term, d0)
        d = :gb_trees.enter(binp, term, d1)
        {term, line, d}
    end
  end

  defp parse_heap_term('Ys' ++ line0, addr, decodeOpts, d0) do
    {binp0, ':' ++ line1} = get_hex(line0)
    {offset, ':' ++ line2} = get_hex(line1)
    {sz, line3} = get_hex(line2)
    {term, line, d1} = deref_bin(binp0, offset, sz, line3, decodeOpts, d0)
    d = :gb_trees.insert(addr, term, d1)
    {term, line, d}
  end

  defp parse_heap_term('Mf' ++ line0, addr, decodeOpts, d0) do
    {size, ':' ++ line1} = get_hex(line0)

    case parse_term(line1, decodeOpts, d0) do
      {keys, ':' ++ line2, d1} when is_tuple(keys) ->
        {values, line, d2} = parse_tuple(size, line2, addr, decodeOpts, d1, [])
        pairs = zip_tuples(tuple_size(keys), keys, values, [])
        map = :maps.from_list(pairs)
        d = :gb_trees.update(addr, map, d2)
        {map, line, d}

      {incomplete, _Line, d1} ->
        d = :gb_trees.insert(addr, incomplete, d1)
        {incomplete, '', d}
    end
  end

  defp parse_heap_term('Mh' ++ line0, addr, decodeOpts, d0) do
    {mapSize, ':' ++ line1} = get_hex(line0)
    {n, ':' ++ line2} = get_hex(line1)
    {nodes, line, d1} = parse_tuple(n, line2, addr, decodeOpts, d0, [])
    map = :maps.from_list(flatten_hashmap_nodes(nodes))
    ^mapSize = :maps.size(map)
    d = :gb_trees.update(addr, map, d1)
    {map, line, d}
  end

  defp parse_heap_term('Mn' ++ line0, addr, decodeOpts, d) do
    {n, ':' ++ line} = get_hex(line0)
    parse_tuple(n, line, addr, decodeOpts, d, [])
  end

  defp parse_tuple(0, line, addr, _, d0, acc) do
    tuple = :erlang.list_to_tuple(:lists.reverse(acc))
    d = :gb_trees.insert(addr, tuple, d0)
    {tuple, line, d}
  end

  defp parse_tuple(n, line0, addr, decodeOpts, d0, acc) do
    case parse_term(line0, decodeOpts, d0) do
      {term, [?, | line], d} when n > 1 ->
        parse_tuple(n - 1, line, addr, decodeOpts, d, [term | acc])

      {term, line, d} ->
        parse_tuple(n - 1, line, addr, decodeOpts, d, [term | acc])
    end
  end

  defp zip_tuples(0, _T1, _T2, acc) do
    acc
  end

  defp zip_tuples(n, t1, t2, acc) when n <= tuple_size(t1) do
    zip_tuples(n - 1, t1, t2, [
      {:erlang.element(n, t1), :erlang.element(n, t2)}
      | acc
    ])
  end

  defp flatten_hashmap_nodes(tuple) do
    flatten_hashmap_nodes_1(tuple_size(tuple), tuple, [])
  end

  defp flatten_hashmap_nodes_1(0, _Tuple, acc) do
    acc
  end

  defp flatten_hashmap_nodes_1(n, tuple0, acc0) do
    case :erlang.element(n, tuple0) do
      [k | v] ->
        flatten_hashmap_nodes_1(n - 1, tuple0, [{k, v} | acc0])

      tuple when is_tuple(tuple) ->
        acc = flatten_hashmap_nodes_1(n - 1, tuple0, acc0)
        flatten_hashmap_nodes_1(tuple_size(tuple), tuple, acc)
    end
  end

  defp parse_term([?H | line0], decodeOpts, d) do
    {ptr, line} = get_hex(line0)
    deref_ptr(ptr, line, decodeOpts, d)
  end

  defp parse_term([?N | line], _, d) do
    {[], line, d}
  end

  defp parse_term([?I | line0], _, d) do
    {int, line} = :string.to_integer(line0)
    {int, line, d}
  end

  defp parse_term([?A | _] = line, _, d) do
    parse_atom(line, d)
  end

  defp parse_term([?P | line0], _, d) do
    {pid, line} = get_id(line0)
    {[:"#CDVPid" | pid], line, d}
  end

  defp parse_term([?p | line0], _, d) do
    {port, line} = get_id(line0)
    {[:"#CDVPort" | port], line, d}
  end

  defp parse_term([?S | str0], _, d) do
    str = :lists.reverse(skip_blanks(:lists.reverse(str0)))
    {str, [], d}
  end

  defp parse_term([?D | line0], decodeOpts, d) do
    try do
      {attabSize, ':' ++ line1} = get_hex(line0)
      {attab, 'E' ++ line2} = parse_atom_translation_table(attabSize, line1, [])
      {bin, line3} = get_binary(line2, decodeOpts)

      {try do
         :erts_debug.dist_ext_to_term(attab, bin)
       catch
         :error, _ ->
           :"<invalid-distribution-message>"
       end, line3, d}
    catch
      :error, _ ->
        {:"#CDVBadDistExt", skip_dist_ext(line0), d}
    end
  end

  defp skip_dist_ext(line) do
    skip_dist_ext(:lists.reverse(line), [])
  end

  defp skip_dist_ext([], seqTraceToken) do
    seqTraceToken
  end

  defp skip_dist_ext([?: | _], seqTraceToken) do
    [?: | seqTraceToken]
  end

  defp skip_dist_ext([c | cs], keptCs) do
    skip_dist_ext(cs, [c | keptCs])
  end

  defp parse_atom([?A | line0], d) do
    {n, ':' ++ line1} = get_hex(line0)
    {chars, line} = get_chars(n, line1)

    {:erlang.binary_to_atom(
       :erlang.list_to_binary(chars),
       :utf8
     ), line, d}
  end

  defp parse_atom_translation_table(0, line0, as) do
    {:erlang.list_to_tuple(:lists.reverse(as)), line0}
  end

  defp parse_atom_translation_table(n, line0, as) do
    {a, line1, _} = parse_atom(line0, [])
    parse_atom_translation_table(n - 1, line1, [a | as])
  end

  defp deref_ptr(ptr, line, decodeOpts, d) do
    lookup0 = fn d0 ->
      :gb_trees.lookup(ptr, d0)
    end

    lookup = wrap_line_map(ptr, lookup0)
    do_deref_ptr(lookup, line, decodeOpts, d)
  end

  defp deref_bin(binp0, offset, sz, line, decodeOpts, d) do
    binp = binp0 ||| r_dec_opts(decodeOpts, :bin_addr_adj)

    lookup0 = fn d0 ->
      lookup_binary(binp, offset, sz, d0)
    end

    lookup = wrap_line_map(binp, lookup0)
    do_deref_ptr(lookup, line, decodeOpts, d)
  end

  defp lookup_binary(binp, offset, sz, d) do
    case lookup_binary_index(binp) do
      [{_, start}] ->
        term = cdvbin(offset, sz, {:"#CDVBin", start})
        {:value, term}

      [] ->
        case :gb_trees.lookup(binp, d) do
          {:value, <<_::size(offset)-bytes, sub::size(sz)-bytes, _::bytes>>} ->
            {:value, sub}

          {:value, symbolicBin} ->
            {:value, cdvbin(offset, sz, symbolicBin)}

          :none ->
            :none
        end
    end
  end

  defp wrap_line_map(ptr, lookup) do
    wrap_line_map_1(:erlang.get(:line_map), ptr, lookup)
  end

  defp wrap_line_map_1(%{} = lineMap, ptr, lookup) do
    fn d ->
      case lookup.(d) do
        {:value, _} = res ->
          res

        :none ->
          case lineMap do
            %{^ptr => line} ->
              {:line, ptr, line}

            %{} ->
              :none
          end
      end
    end
  end

  defp wrap_line_map_1(:undefined, _Ptr, lookup) do
    lookup
  end

  defp do_deref_ptr(lookup, line, decodeOpts, d0) do
    case lookup.(d0) do
      {:value, term} ->
        {term, line, d0}

      :none ->
        :erlang.put(:incomplete_heap, true)
        {:"#CDVIncompleteHeap", line, d0}

      {:line, addr, newLine} ->
        d = parse_line(addr, newLine, decodeOpts, d0)
        do_deref_ptr(lookup, line, decodeOpts, d)
    end
  end

  defp get_hex(l) do
    get_hex_1(l, 0)
  end

  defp get_hex_1([h | t] = l, acc) do
    case get_hex_digit(h) do
      :none ->
        {acc, l}

      digit ->
        get_hex_1(t, acc <<< 4 ||| digit)
    end
  end

  defp get_hex_1([], acc) do
    {acc, []}
  end

  defp get_hex_digit(c) when ?0 <= c and c <= ?9 do
    c - ?0
  end

  defp get_hex_digit(c) when ?a <= c and c <= ?f do
    c - ?a + 10
  end

  defp get_hex_digit(c) when ?A <= c and c <= ?F do
    c - ?A + 10
  end

  defp get_hex_digit(_) do
    :none
  end

  defp skip_blanks([?\s | t]) do
    skip_blanks(t)
  end

  defp skip_blanks([?\r | t]) do
    skip_blanks(t)
  end

  defp skip_blanks([?\n | t]) do
    skip_blanks(t)
  end

  defp skip_blanks([?\t | t]) do
    skip_blanks(t)
  end

  defp skip_blanks(t) do
    t
  end

  defp get_chars(n, line) do
    get_chars(n, line, [])
  end

  defp get_chars(0, line, acc) do
    {:lists.reverse(acc), line}
  end

  defp get_chars(n, [h | t], acc) do
    get_chars(n - 1, t, [h | acc])
  end

  defp get_id(line0) do
    [?< | line] =
      :lists.dropwhile(
        fn
          ?< ->
            false

          _ ->
            true
        end,
        line0
      )

    get_id(line, [], [])
  end

  defp get_id([?> | line], acc, id) do
    {:lists.reverse(
       id,
       [:erlang.list_to_integer(:lists.reverse(acc))]
     ), line}
  end

  defp get_id([?. | line], acc, id) do
    get_id(line, [], [:erlang.list_to_integer(:lists.reverse(acc)) | id])
  end

  defp get_id([h | t], acc, id) do
    get_id(t, [h | acc], id)
  end

  defp get_label(l) do
    get_label(l, [])
  end

  defp get_label([?: | line], acc) do
    label = :lists.reverse(acc)

    case get_hex(label) do
      {int, []} ->
        {int, line}

      _ ->
        {:erlang.list_to_atom(label), line}
    end
  end

  defp get_label([h | t], acc) do
    get_label(t, [h | acc])
  end

  defp get_binary(line0, decodeOpts) do
    case get_hex(line0) do
      {n, ':' ++ line} ->
        get_binary_1(n, line, decodeOpts)

      _ ->
        {:"#CDVTruncatedBinary", []}
    end
  end

  defp get_binary_1(n, line, r_dec_opts(base64: false)) do
    get_binary_hex(n, line, [], false)
  end

  defp get_binary_1(n, line0, r_dec_opts(base64: true)) do
    numBytes = div(n + 2, 3) * 4
    {base64, line} = :lists.split(numBytes, line0)
    bin = get_binary_base64(:erlang.list_to_binary(base64), <<>>, false)
    {bin, line}
  end

  defp get_binary(offset, size, line0, decodeOpts) do
    case get_hex(line0) do
      {_N, ':' ++ line} ->
        get_binary_1(offset, size, line, decodeOpts)

      _ ->
        {:"#CDVTruncatedBinary", []}
    end
  end

  defp get_binary_1(offset, size, line, r_dec_opts(base64: false)) do
    progress = size > 10000
    progress and init_progress('Reading binary', size)
    get_binary_hex(size, :lists.sublist(line, offset * 2 + 1, size * 2), [], progress)
  end

  defp get_binary_1(startOffset, size, line, r_dec_opts(base64: true)) do
    progress = size > 10000
    progress and init_progress('Reading binary', size)
    endOffset = startOffset + size
    startByte = div(startOffset, 3) * 4
    endByte = div(endOffset + 2, 3) * 4
    numBytes = endByte - startByte

    case :erlang.list_to_binary(line) do
      <<_::size(startByte)-bytes, base64::size(numBytes)-bytes, _::bytes>> ->
        bin0 = get_binary_base64(base64, <<>>, progress)
        skip = startOffset - div(startOffset, 3) * 3
        <<_::size(skip)-bytes, bin::size(size)-bytes, _::bytes>> = bin0
        {bin, []}

      _ ->
        {:"#CDVTruncatedBinary", []}
    end
  end

  defp get_binary_hex(0, line, acc, progress) do
    progress and end_progress()
    {:erlang.list_to_binary(:lists.reverse(acc)), line}
  end

  defp get_binary_hex(n, [[a, b] | line], acc, progress) do
    byte = get_hex_digit(a) <<< 4 ||| get_hex_digit(b)
    progress and update_progress()
    get_binary_hex(n - 1, line, [byte | acc], progress)
  end

  defp get_binary_hex(_N, [], _Acc, progress) do
    progress and end_progress()
    {:"#CDVTruncatedBinary", []}
  end

  defp get_binary_base64(<<chunk0::size(4 * 256)-bytes, t::bytes>>, acc0, progress) do
    chunk = :base64.decode(chunk0)
    acc = <<acc0::binary, chunk::binary>>
    progress and update_progress(div(4 * 256 * 3, 4))
    get_binary_base64(t, acc, progress)
  end

  defp get_binary_base64(chunk0, acc, progress) do
    case progress do
      true ->
        update_progress(div(4 * 256 * 3, 4))
        end_progress()

      false ->
        :ok
    end

    chunk = :base64.decode(chunk0)
    <<acc::binary, chunk::binary>>
  end

  defp cdvbin(offset, size, {:"#CDVBin", pos}) do
    [:"#CDVBin", offset, size, pos]
  end

  defp cdvbin(offset, size, [:"#CDVBin", _, _, pos]) do
    [:"#CDVBin", offset, size, pos]
  end

  defp cdvbin(_, _, :"#CDVTruncatedBinary") do
    :"#CDVTruncatedBinary"
  end

  defp cdvbin(_, _, :"#CDVNonexistingBinary") do
    :"#CDVNonexistingBinary"
  end

  defp reset_tables() do
    :ets.delete_all_objects(:cdv_dump_index_table)
    :ets.delete_all_objects(:cdv_reg_proc_table)
    :ets.delete_all_objects(:cdv_binary_index_table)
    :ets.delete_all_objects(:cdv_heap_file_chars)
  end

  defp insert_index(tag, id, pos) do
    :ets.insert(:cdv_dump_index_table, {{tag, pos}, id})
  end

  defp delete_index(tag, id) do
    ms = [{{{tag, :"$1"}, id}, [], [true]}]
    :ets.select_delete(:cdv_dump_index_table, ms)
  end

  defp lookup_index({tag, id}) do
    lookup_index(tag, id)
  end

  defp lookup_index(tag) do
    lookup_index(tag, :"$2")
  end

  defp lookup_index(tag, id) do
    :ets.select(
      :cdv_dump_index_table,
      [{{{tag, :"$1"}, id}, [], [{{id, :"$1"}}]}]
    )
  end

  defp count_index(tag) do
    :ets.select_count(
      :cdv_dump_index_table,
      [{{{tag, :_}, :_}, [], [true]}]
    )
  end

  defp insert_binary_index(addr, pos) do
    :ets.insert(:cdv_binary_index_table, {addr, pos})
  end

  defp lookup_binary_index(addr) do
    :ets.lookup(:cdv_binary_index_table, addr)
  end

  defp tag_to_atom('abort') do
    :abort
  end

  defp tag_to_atom('allocated_areas') do
    :allocated_areas
  end

  defp tag_to_atom('allocator') do
    :allocator
  end

  defp tag_to_atom('atoms') do
    :atoms
  end

  defp tag_to_atom('binary') do
    :binary
  end

  defp tag_to_atom('dirty_cpu_scheduler') do
    :dirty_cpu_scheduler
  end

  defp tag_to_atom('dirty_cpu_run_queue') do
    :dirty_cpu_run_queue
  end

  defp tag_to_atom('dirty_io_scheduler') do
    :dirty_io_scheduler
  end

  defp tag_to_atom('dirty_io_run_queue') do
    :dirty_io_run_queue
  end

  defp tag_to_atom('end') do
    :ende
  end

  defp tag_to_atom('erl_crash_dump') do
    :erl_crash_dump
  end

  defp tag_to_atom('ets') do
    :ets
  end

  defp tag_to_atom('fun') do
    :fu
  end

  defp tag_to_atom('hash_table') do
    :hash_table
  end

  defp tag_to_atom('hidden_node') do
    :hidden_node
  end

  defp tag_to_atom('index_table') do
    :index_table
  end

  defp tag_to_atom('instr_data') do
    :instr_data
  end

  defp tag_to_atom('internal_ets') do
    :internal_ets
  end

  defp tag_to_atom('literals') do
    :literals
  end

  defp tag_to_atom('loaded_modules') do
    :loaded_modules
  end

  defp tag_to_atom('memory') do
    :memory
  end

  defp tag_to_atom('mod') do
    :mod
  end

  defp tag_to_atom('persistent_terms') do
    :persistent_terms
  end

  defp tag_to_atom('no_distribution') do
    :no_distribution
  end

  defp tag_to_atom('node') do
    :node
  end

  defp tag_to_atom('not_connected') do
    :not_connected
  end

  defp tag_to_atom('old_instr_data') do
    :old_instr_data
  end

  defp tag_to_atom('port') do
    :port
  end

  defp tag_to_atom('proc') do
    :proc
  end

  defp tag_to_atom('proc_dictionary') do
    :proc_dictionary
  end

  defp tag_to_atom('proc_heap') do
    :proc_heap
  end

  defp tag_to_atom('proc_messages') do
    :proc_messages
  end

  defp tag_to_atom('proc_stack') do
    :proc_stack
  end

  defp tag_to_atom('scheduler') do
    :scheduler
  end

  defp tag_to_atom('timer') do
    :timer
  end

  defp tag_to_atom('visible_node') do
    :visible_node
  end

  defp tag_to_atom(unknownTag) do
    :io.format('WARNING: Found unexpected tag:~ts~n', [unknownTag])
    :erlang.list_to_atom(unknownTag)
  end

  defp put_last_tag(:abort, reason, _Pos) do
    :erlang.put(:truncated_reason, reason)
  end

  defp put_last_tag(tag, id, pos) do
    :erlang.put(:last_tag, {{tag, id}, pos})
  end

  defp lookup_and_parse_index(file, what, parseFun, str) when is_list(file) do
    indices = lookup_index(what)

    fun = fn fd, {id, start} ->
      pos_bof(fd, start)
      parseFun.(fd, id)
    end

    report = 'Processing ' ++ str
    progress_pmap(report, file, fun, indices)
  end

  def to_proplist(fields, record) do
    values = to_value_list(record)
    :lists.zip(fields, values)
  end

  def to_value_list(record) do
    [_RecordName | values] = :erlang.tuple_to_list(record)
    values
  end

  defp progress_pmap(report, file, fun, list) do
    nTot = length(list)
    nProcs = :erlang.system_info(:schedulers) * 2
    nPerProc = div(nTot, nProcs) + 1
    reportInterval = div(nTot, 100) + 1
    init_progress(report, 99)
    collector = self()

    {[], pids} =
      :lists.foldl(
        fn _, {l, ps} ->
          {l1, l2} =
            cond do
              length(l) >= nPerProc ->
                :lists.split(nPerProc, l)

              true ->
                {l, []}
            end

          {p, _Ref} =
            spawn_monitor(fn ->
              progress_map(
                collector,
                reportInterval,
                file,
                fun,
                l1
              )
            end)

          {l2, [p | ps]}
        end,
        {list, []},
        :lists.seq(1, nProcs)
      )

    collect(pids, [])
  end

  defp progress_map(collector, reportInterval, file, fun, list) do
    fd = open(file)

    init_progress(
      reportInterval,
      fn _ ->
        send(collector, :progress)
      end,
      :ok
    )

    progress_map(fd, fun, list, [])
  end

  defp progress_map(fd, fun, [h | t], acc) do
    update_progress()
    progress_map(fd, fun, t, [fun.(fd, h) | acc])
  end

  defp progress_map(fd, _Fun, [], acc) do
    close(fd)
    exit({:pmap_done, acc})
  end

  defp collect([], acc) do
    end_progress()
    :lists.append(acc)
  end

  defp collect(pids, acc) do
    receive do
      :progress ->
        update_progress()
        collect(pids, acc)

      {:DOWN, _Ref, :process, pid, {:pmap_done, result}} ->
        collect(:lists.delete(pid, pids), [result | acc])

      {:DOWN, _Ref, :process, pid, _Error} ->
        warning =
          'WARNING: an error occured while parsing data.\n' ++
            case :erlang.get(:truncated) do
              true ->
                'This might be because the dump is truncated.\n'

              false ->
                ''
            end

        :io.format(warning)
        collect(:lists.delete(pid, pids), acc)
    end
  end

  defp init_progress(report, n) do
    :observer_lib.report_progress({:ok, report})
    interval = div(n, 100) + 1

    fun = fn p0 ->
      p = p0 + 1
      :observer_lib.report_progress({:ok, p})
      p
    end

    init_progress(interval, fun, 0)
  end

  defp init_progress(interval, fun, acc) do
    :erlang.put(:progress, {interval, interval, fun, acc})
    :ok
  end

  defp update_progress() do
    update_progress(1)
  end

  defp update_progress(processed) do
    do_update_progress(:erlang.get(:progress), processed)
  end

  defp do_update_progress({count, interval, fun, acc}, processed)
       when processed > count do
    do_update_progress(
      {interval, interval, fun, fun.(acc)},
      processed - count
    )
  end

  defp do_update_progress({count, interval, fun, acc}, processed) do
    :erlang.put(
      :progress,
      {count - processed, interval, fun, acc}
    )

    :ok
  end

  defp end_progress() do
    end_progress({:ok, 100})
  end

  defp end_progress(report) do
    :observer_lib.report_progress(report)
    :erlang.erase(:progress)
    :ok
  end
end
