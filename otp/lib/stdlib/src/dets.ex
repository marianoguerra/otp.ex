defmodule :m_dets do
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

  Record.defrecord(:r_head, :head,
    m: :undefined,
    m2: :undefined,
    next: :undefined,
    fptr: :undefined,
    no_objects: :undefined,
    no_keys: :undefined,
    maxobjsize: :undefined,
    n: :undefined,
    type: :undefined,
    keypos: :undefined,
    freelists: :undefined,
    freelists_p: :undefined,
    no_collections: :undefined,
    auto_save: :undefined,
    update_mode: :undefined,
    fixed: false,
    hash_bif: :undefined,
    has_md5: :undefined,
    min_no_slots: :undefined,
    max_no_slots: :undefined,
    cache: :undefined,
    filename: :undefined,
    access: :read_write,
    ram_file: false,
    name: :undefined,
    parent: :undefined,
    server: :undefined,
    bump: :undefined,
    base: :undefined
  )

  Record.defrecord(:r_fileheader, :fileheader,
    freelist: :undefined,
    fl_base: :undefined,
    cookie: :undefined,
    closed_properly: :undefined,
    type: :undefined,
    version: :undefined,
    m: :undefined,
    next: :undefined,
    keypos: :undefined,
    no_objects: :undefined,
    no_keys: :undefined,
    min_no_slots: :undefined,
    max_no_slots: :undefined,
    no_colls: :undefined,
    hash_method: :undefined,
    read_md5: :undefined,
    has_md5: :undefined,
    md5: :undefined,
    trailer: :undefined,
    eof: :undefined,
    n: :undefined
  )

  Record.defrecord(:r_cache, :cache,
    cache: :undefined,
    csize: :undefined,
    inserts: :undefined,
    wrtime: :undefined,
    tsize: :undefined,
    delay: :undefined
  )

  Record.defrecord(:r_dets_cont, :dets_cont,
    what: :undefined,
    no_objs: :undefined,
    bin: :undefined,
    alloc: :undefined,
    tab: :undefined,
    proc: :undefined,
    match_program: :undefined
  )

  Record.defrecord(:r_open_args, :open_args,
    file: :undefined,
    type: :undefined,
    keypos: :undefined,
    repair: :undefined,
    min_no_slots: :undefined,
    max_no_slots: :undefined,
    ram_file: :undefined,
    delayed_write: :undefined,
    auto_save: :undefined,
    access: :undefined,
    debug: :undefined
  )

  def add_user(pid, tab, args) do
    req(pid, {:add_user, tab, args})
  end

  def all() do
    :dets_server.all()
  end

  def bchunk(tab, :start) do
    badarg(treq(tab, {:bchunk_init, tab}), [tab, :start])
  end

  def bchunk(tab, r_dets_cont(what: :bchunk, tab: tab) = state) do
    badarg(treq(tab, {:bchunk, state}), [tab, state])
  end

  def bchunk(tab, term) do
    :erlang.error(:badarg, [tab, term])
  end

  def close(tab) do
    case :dets_server.close(tab) do
      :badarg ->
        {:error, :not_owner}

      reply ->
        reply
    end
  end

  def delete(tab, key) do
    badarg(treq(tab, {:delete_key, [key]}), [tab, key])
  end

  def delete_all_objects(tab) do
    case treq(tab, :delete_all_objects) do
      :badarg ->
        :erlang.error(:badarg, [tab])

      :fixed ->
        match_delete(tab, :_)

      reply ->
        reply
    end
  end

  def delete_object(tab, o) do
    badarg(treq(tab, {:delete_object, [o]}), [tab, o])
  end

  def fsck(fname, _Version) do
    fsck(fname)
  end

  def fsck(fname) do
    try do
      {:ok, fd, fH} = read_file_header(fname, :read, false)
      :void

      case :dets_v9.check_file_header(fH, fd) do
        {:error, :not_closed} ->
          fsck(fd, make_ref(), fname, fH, :default, :default)

        {:ok, _Head} ->
          fsck(fd, make_ref(), fname, fH, :default, :default)

        error ->
          error
      end
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  def first(tab) do
    badarg_exit(treq(tab, :first), [tab])
  end

  def foldr(fun, acc, tab) do
    foldl(fun, acc, tab)
  end

  def foldl(fun, acc, tab) do
    ref = make_ref()
    badarg(do_traverse(fun, acc, tab, ref), [fun, acc, tab])
  end

  def from_ets(dTab, eTab) do
    :ets.safe_fixtable(eTab, true)
    spec = [{:_, [], [:"$_"]}]
    lC = :ets.select(eTab, spec, 100)
    initFun = from_ets_fun(lC, eTab)

    reply =
      treq(
        dTab,
        {:initialize, initFun, :term, :default}
      )

    :ets.safe_fixtable(eTab, false)

    case reply do
      {:thrown, thrown} ->
        throw(thrown)

      else__ ->
        badarg(else__, [dTab, eTab])
    end
  end

  defp from_ets_fun(lC, eTab) do
    fn
      :close ->
        :ok

      :read when lC === :"$end_of_table" ->
        :end_of_input

      :read ->
        {l, c} = lC
        {l, from_ets_fun(:ets.select(c), eTab)}
    end
  end

  def info(tab) do
    case (try do
            :dets_server.get_pid(tab)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _Reason} ->
        :undefined

      pid ->
        undefined(req(pid, :info))
    end
  end

  def info(tab, :owner) do
    case (try do
            :dets_server.get_pid(tab)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      pid when is_pid(pid) ->
        pid

      _ ->
        :undefined
    end
  end

  def info(tab, :users) do
    case :dets_server.users(tab) do
      [] ->
        :undefined

      users ->
        users
    end
  end

  def info(tab, tag) do
    case (try do
            :dets_server.get_pid(tab)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _Reason} ->
        :undefined

      pid ->
        undefined(req(pid, {:info, tag}))
    end
  end

  def init_table(tab, initFun) do
    init_table(tab, initFun, [])
  end

  def init_table(tab, initFun, options)
      when is_function(initFun) do
    case options(options, [:format, :min_no_slots]) do
      {:badarg, _} ->
        :erlang.error(:badarg, [tab, initFun, options])

      [format, minNoSlots] ->
        case treq(
               tab,
               {:initialize, initFun, format, minNoSlots}
             ) do
          {:thrown, thrown} ->
            throw(thrown)

          else__ ->
            badarg(else__, [tab, initFun, options])
        end
    end
  end

  def init_table(tab, initFun, options) do
    :erlang.error(:badarg, [tab, initFun, options])
  end

  def insert(tab, objs) when is_list(objs) do
    badarg(treq(tab, {:insert, objs}), [tab, objs])
  end

  def insert(tab, obj) do
    badarg(treq(tab, {:insert, [obj]}), [tab, obj])
  end

  def insert_new(tab, objs) when is_list(objs) do
    badarg(treq(tab, {:insert_new, objs}), [tab, objs])
  end

  def insert_new(tab, obj) do
    badarg(treq(tab, {:insert_new, [obj]}), [tab, obj])
  end

  def internal_close(pid) do
    req(pid, :close)
  end

  def internal_open(pid, ref, args) do
    req(pid, {:internal_open, ref, args})
  end

  def is_compatible_bchunk_format(tab, term) do
    badarg(
      treq(tab, {:is_compatible_bchunk_format, term}),
      [tab, term]
    )
  end

  def is_dets_file(fileName) do
    case (try do
            read_file_header(fileName, :read, false)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, fd, fH} ->
        _ = :file.close(fd)
        r_fileheader(fH, :cookie) === 11_259_375

      {:error, {:tooshort, _}} ->
        false

      {:error, {:not_a_dets_file, _}} ->
        false

      other ->
        other
    end
  end

  def lookup(tab, key) do
    badarg(treq(tab, {:lookup_keys, [key]}), [tab, key])
  end

  def lookup_keys(tab, keys) do
    case (try do
            :lists.usort(keys)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      uKeys when is_list(uKeys) and uKeys !== [] ->
        badarg(treq(tab, {:lookup_keys, uKeys}), [tab, keys])

      _Else ->
        :erlang.error(:badarg, [tab, keys])
    end
  end

  def match(tab, pat) do
    badarg(safe_match(tab, pat, :bindings), [tab, pat])
  end

  def match(tab, pat, n) do
    badarg(
      init_chunk_match(tab, pat, :bindings, n, :no_safe),
      [tab, pat, n]
    )
  end

  def match(state) when r_dets_cont(state, :what) === :bindings do
    badarg(chunk_match(state, :no_safe), [state])
  end

  def match(term) do
    :erlang.error(:badarg, [term])
  end

  def match_delete(tab, pat) do
    badarg(match_delete(tab, pat, :delete), [tab, pat])
  end

  defp match_delete(tab, pat, what) do
    case compile_match_spec(what, pat) do
      {spec, mP} ->
        case (try do
                :dets_server.get_pid(tab)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _Reason} ->
            :badarg

          proc ->
            r = req(proc, {:match_delete_init, mP, spec})
            do_match_delete(proc, r, what, 0)
        end

      :badarg ->
        :badarg
    end
  end

  defp do_match_delete(_Proc, {:done, n1}, :select, n) do
    n + n1
  end

  defp do_match_delete(_Proc, {:done, _N1}, _What, _N) do
    :ok
  end

  defp do_match_delete(proc, {:cont, state, n1}, what, n) do
    do_match_delete(proc, req(proc, {:match_delete, state}), what, n + n1)
  end

  defp do_match_delete(_Proc, error, _What, _N) do
    error
  end

  def match_object(tab, pat) do
    badarg(safe_match(tab, pat, :object), [tab, pat])
  end

  def match_object(tab, pat, n) do
    badarg(
      init_chunk_match(tab, pat, :object, n, :no_safe),
      [tab, pat, n]
    )
  end

  def match_object(state) when r_dets_cont(state, :what) === :object do
    badarg(chunk_match(state, :no_safe), [state])
  end

  def match_object(term) do
    :erlang.error(:badarg, [term])
  end

  def member(tab, key) do
    badarg(treq(tab, {:member, key}), [tab, key])
  end

  def next(tab, key) do
    badarg_exit(treq(tab, {:next, key}), [tab, key])
  end

  def open_file(file0) do
    file = to_list(file0)

    case is_list(file) do
      true ->
        case :dets_server.open_file(file) do
          :badarg ->
            :erlang.error(:dets_process_died, [file])

          reply ->
            einval(reply, [file])
        end

      false ->
        :erlang.error(:badarg, [file0])
    end
  end

  def open_file(tab, args) when is_list(args) do
    case (try do
            defaults(tab, args)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      openArgs when elem(openArgs, 0) === :open_args ->
        case :dets_server.open_file(tab, openArgs) do
          :badarg ->
            :erlang.error(:dets_process_died, [tab, args])

          reply ->
            einval(reply, [tab, args])
        end

      _ ->
        :erlang.error(:badarg, [tab, args])
    end
  end

  def open_file(tab, arg) do
    open_file(tab, [arg])
  end

  def pid2name(pid) do
    :dets_server.pid2name(pid)
  end

  def remove_user(pid, from) do
    req(pid, {:close, from})
  end

  def repair_continuation(r_dets_cont(match_program: {:match_spec, b}) = cont, mS) do
    case :ets.is_compiled_ms(b) do
      true ->
        cont

      false ->
        r_dets_cont(cont, match_program: {:match_spec, :ets.match_spec_compile(mS)})
    end
  end

  def repair_continuation(r_dets_cont() = cont, _MS) do
    cont
  end

  def repair_continuation(t, mS) do
    :erlang.error(:badarg, [t, mS])
  end

  def safe_fixtable(tab, bool) when bool or not bool do
    badarg(treq(tab, {:safe_fixtable, bool}), [tab, bool])
  end

  def safe_fixtable(tab, term) do
    :erlang.error(:badarg, [tab, term])
  end

  def select(tab, pat) do
    badarg(safe_match(tab, pat, :select), [tab, pat])
  end

  def select(tab, pat, n) do
    badarg(
      init_chunk_match(tab, pat, :select, n, :no_safe),
      [tab, pat, n]
    )
  end

  def select(state) when r_dets_cont(state, :what) === :select do
    badarg(chunk_match(state, :no_safe), [state])
  end

  def select(term) do
    :erlang.error(:badarg, [term])
  end

  def select_delete(tab, pat) do
    badarg(match_delete(tab, pat, :select), [tab, pat])
  end

  def slot(tab, slot)
      when is_integer(slot) and
             slot >= 0 do
    badarg(treq(tab, {:slot, slot}), [tab, slot])
  end

  def slot(tab, term) do
    :erlang.error(:badarg, [tab, term])
  end

  def start() do
    :dets_server.start()
  end

  def stop() do
    :dets_server.stop()
  end

  def istart_link(server) do
    {:ok, :proc_lib.spawn_link(:dets, :init, [self(), server])}
  end

  def sync(tab) do
    badarg(treq(tab, :sync), [tab])
  end

  def table(tab) do
    table(tab, [])
  end

  def table(tab, opts) do
    case options(opts, [:traverse, :n_objects]) do
      {:badarg, _} ->
        :erlang.error(:badarg, [tab, opts])

      [traverse, nObjs] ->
        tF =
          case traverse do
            :first_next ->
              fn ->
                qlc_next(tab, first(tab))
              end

            :select ->
              fn mS ->
                qlc_select(select(tab, mS, nObjs))
              end

            {:select, mS} ->
              fn ->
                qlc_select(select(tab, mS, nObjs))
              end
          end

        preFun = fn _ ->
          safe_fixtable(tab, true)
        end

        postFun = fn ->
          safe_fixtable(tab, false)
        end

        infoFun = fn tag ->
          table_info(tab, tag)
        end

        lookupFun =
          case traverse do
            {:select, _MS} ->
              :undefined

            _ ->
              fn
                _KeyPos, [k] ->
                  lookup(tab, k)

                _KeyPos, ks ->
                  lookup_keys(tab, ks)
              end
          end

        formatFun = fn
          {:all, _NElements, _ElementFun} ->
            as = [
              tab
              | for _ <- [[]], opts !== [] do
                  opts
                end
            ]

            {:dets, :table, as}

          {:match_spec, mS} ->
            {:dets, :table, [tab, [{:traverse, {:select, mS}} | listify(opts)]]}

          {:lookup, _KeyPos, [value], _NElements, elementFun} ->
            :io_lib.format('~w:lookup(~w, ~w)', [:dets, tab, elementFun.(value)])

          {:lookup, _KeyPos, values, _NElements, elementFun} ->
            vals =
              for v <- values do
                elementFun.(v)
              end

            :io_lib.format('lists:flatmap(fun(V) -> ~w:lookup(~w, V) end, ~w)', [:dets, tab, vals])
        end

        :qlc.table(
          tF,
          [
            {:pre_fun, preFun},
            {:post_fun, postFun},
            {:info_fun, infoFun},
            {:format_fun, formatFun},
            {:key_equality, :"=:="},
            {:lookup_fun, lookupFun}
          ]
        )
    end
  end

  defp qlc_next(_Tab, :"$end_of_table") do
    []
  end

  defp qlc_next(tab, key) do
    case lookup(tab, key) do
      objects when is_list(objects) ->
        objects ++
          fn ->
            qlc_next(tab, next(tab, key))
          end

      error ->
        exit(error)
    end
  end

  defp qlc_select(:"$end_of_table") do
    []
  end

  defp qlc_select({objects, cont}) when is_list(objects) do
    objects ++
      fn ->
        qlc_select(select(cont))
      end
  end

  defp qlc_select(error) do
    error
  end

  defp table_info(tab, :num_of_objects) do
    info(tab, :size)
  end

  defp table_info(tab, :keypos) do
    info(tab, :keypos)
  end

  defp table_info(tab, :is_unique_objects) do
    info(tab, :type) !== :duplicate_bag
  end

  defp table_info(_Tab, _) do
    :undefined
  end

  def to_ets(dTab, eTab) do
    case :ets.info(eTab, :protection) do
      :undefined ->
        :erlang.error(:badarg, [dTab, eTab])

      _ ->
        fun = fn x, t ->
          true = :ets.insert(t, x)
          t
        end

        foldl(fun, eTab, dTab)
    end
  end

  def traverse(tab, fun) do
    ref = make_ref()

    tFun = fn o, acc ->
      case fun.(o) do
        :continue ->
          acc

        {:continue, val} ->
          [val | acc]

        {:done, value} ->
          throw({ref, [value | acc]})

        other ->
          throw({ref, other})
      end
    end

    badarg(do_traverse(tFun, [], tab, ref), [tab, fun])
  end

  def update_counter(tab, key, c) do
    badarg(
      treq(tab, {:update_counter, key, c}),
      [tab, key, c]
    )
  end

  def verbose() do
    verbose(true)
  end

  def verbose(what) do
    :ok = :dets_server.verbose(what)
    all = :dets_server.all()

    fun = fn tab ->
      treq(tab, {:set_verbose, what})
    end

    :lists.foreach(fun, all)
    all
  end

  def where(tab, object) do
    badarg(treq(tab, {:where, object}), [tab, object])
  end

  defp do_traverse(fun, acc, tab, ref) do
    case (try do
            :dets_server.get_pid(tab)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _Reason} ->
        :badarg

      proc ->
        try do
          do_trav(proc, acc, fun)
        catch
          {^ref, result} ->
            result
        end
    end
  end

  defp do_trav(proc, acc, fun) do
    {spec, mP} = compile_match_spec(:object, :_)

    case req(proc, {:match, mP, spec, :default, :safe}) do
      {:cont, state} ->
        do_trav(state, proc, acc, fun)

      error ->
        error
    end
  end

  defp do_trav(state, proc, acc, fun) do
    case req(proc, {:match_init, state, :safe}) do
      :"$end_of_table" ->
        acc

      {:cont, {bins, newState}} ->
        do_trav_bins(newState, proc, acc, fun, :lists.reverse(bins))

      error ->
        error
    end
  end

  defp do_trav_bins(state, proc, acc, fun, []) do
    do_trav(state, proc, acc, fun)
  end

  defp do_trav_bins(state, proc, acc, fun, [bin | bins]) do
    case (try do
            :erlang.binary_to_term(bin)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        req(
          proc,
          {:corrupt, :dets_utils.bad_object(:do_trav_bins, bin)}
        )

      term ->
        newAcc = fun.(term, acc)
        do_trav_bins(state, proc, newAcc, fun, bins)
    end
  end

  defp safe_match(tab, pat, what) do
    do_safe_match(
      init_chunk_match(tab, pat, what, :default, :safe),
      []
    )
  end

  defp do_safe_match({:error, error}, _L) do
    {:error, error}
  end

  defp do_safe_match({l, c}, lL) do
    do_safe_match(chunk_match(c, :safe), l ++ lL)
  end

  defp do_safe_match(:"$end_of_table", l) do
    l
  end

  defp do_safe_match(:badarg, _L) do
    :badarg
  end

  defp init_chunk_match(tab, pat, what, n, safe)
       when (is_integer(n) and
               n >= 0) or
              n === :default do
    case compile_match_spec(what, pat) do
      {spec, mP} ->
        case (try do
                :dets_server.get_pid(tab)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _Reason} ->
            :badarg

          proc ->
            case req(proc, {:match, mP, spec, n, safe}) do
              {:done, l} ->
                {l,
                 r_dets_cont(
                   tab: tab,
                   proc: proc,
                   what: what,
                   bin: :eof,
                   no_objs: :default,
                   alloc: <<>>
                 )}

              {:cont, state} ->
                chunk_match(
                  r_dets_cont(state, what: what, tab: tab, proc: proc),
                  safe
                )

              error ->
                error
            end
        end

      :badarg ->
        :badarg
    end
  end

  defp init_chunk_match(_Tab, _Pat, _What, _N, _Safe) do
    :badarg
  end

  defp chunk_match(r_dets_cont(proc: proc) = state, safe) do
    case req(proc, {:match_init, state, safe}) do
      :"$end_of_table" = reply ->
        reply

      {:cont, {bins, newState}} ->
        mP = r_dets_cont(newState, :match_program)

        case (try do
                do_foldl_bins(bins, mP)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            case :ets.is_compiled_ms(mP) do
              true ->
                bad = :dets_utils.bad_object(:chunk_match, bins)
                req(proc, {:corrupt, bad})

              false ->
                :badarg
            end

          [] ->
            chunk_match(newState, safe)

          terms ->
            {terms, newState}
        end

      error ->
        error
    end
  end

  defp do_foldl_bins(bins, true) do
    foldl_bins(bins, [])
  end

  defp do_foldl_bins(bins, {:match_spec, mP}) do
    foldl_bins(bins, mP, [])
  end

  defp foldl_bins([], terms) do
    terms
  end

  defp foldl_bins([bin | bins], terms) do
    foldl_bins(bins, [:erlang.binary_to_term(bin) | terms])
  end

  defp foldl_bins([], _MP, terms) do
    terms
  end

  defp foldl_bins([bin | bins], mP, terms) do
    term = :erlang.binary_to_term(bin)

    case :ets.match_spec_run([term], mP) do
      [] ->
        foldl_bins(bins, mP, terms)

      [result] ->
        foldl_bins(bins, mP, [result | terms])
    end
  end

  defp compile_match_spec(:select, [{:_, [], [:"$_"]}] = spec) do
    {spec, true}
  end

  defp compile_match_spec(:select, spec) do
    try do
      {spec, {:match_spec, :ets.match_spec_compile(spec)}}
    catch
      :error, _ ->
        :badarg
    end
  end

  defp compile_match_spec(:object, pat) do
    compile_match_spec(:select, [{pat, [], [:"$_"]}])
  end

  defp compile_match_spec(:bindings, pat) do
    compile_match_spec(:select, [{pat, [], [:"$$"]}])
  end

  defp compile_match_spec(:delete, pat) do
    compile_match_spec(:select, [{pat, [], [true]}])
  end

  defp defaults(tab, args) do
    defaults0 =
      r_open_args(
        file: to_list(tab),
        type: :set,
        keypos: 1,
        repair: true,
        min_no_slots: :default,
        max_no_slots: :default,
        ram_file: false,
        delayed_write: {3000, 14000},
        auto_save: :timer.minutes(3),
        access: :read_write,
        debug: false
      )

    fun = &repl/2
    defaults = :lists.foldl(fun, defaults0, args)
    true = is_list(r_open_args(defaults, :file))
    is_comp_min_max(defaults)
  end

  defp to_list(t) when is_atom(t) do
    :erlang.atom_to_list(t)
  end

  defp to_list(t) do
    t
  end

  defp repl({:access, a}, defs) do
    mem(a, [:read, :read_write])
    r_open_args(defs, access: a)
  end

  defp repl({:auto_save, int}, defs)
       when is_integer(int) and int >= 0 do
    r_open_args(defs, auto_save: int)
  end

  defp repl({:auto_save, :infinity}, defs) do
    r_open_args(defs, auto_save: :infinity)
  end

  defp repl({:cache_size, int}, defs)
       when is_integer(int) and int >= 0 do
    defs
  end

  defp repl({:cache_size, :infinity}, defs) do
    defs
  end

  defp repl({:delayed_write, :default}, defs) do
    r_open_args(defs, delayed_write: {3000, 14000})
  end

  defp repl({:delayed_write, {delay, size} = c}, defs)
       when is_integer(delay) and delay >= 0 and
              is_integer(size) and size >= 0 do
    r_open_args(defs, delayed_write: c)
  end

  defp repl({:estimated_no_objects, i}, defs) do
    repl({:min_no_slots, i}, defs)
  end

  defp repl({:file, file}, defs) do
    r_open_args(defs, file: to_list(file))
  end

  defp repl({:keypos, p}, defs)
       when is_integer(p) and
              p > 0 do
    r_open_args(defs, keypos: p)
  end

  defp repl({:max_no_slots, i}, defs) do
    maxSlots = is_max_no_slots(i)
    r_open_args(defs, max_no_slots: maxSlots)
  end

  defp repl({:min_no_slots, i}, defs) do
    minSlots = is_min_no_slots(i)
    r_open_args(defs, min_no_slots: minSlots)
  end

  defp repl({:ram_file, bool}, defs) do
    mem(bool, [true, false])
    r_open_args(defs, ram_file: bool)
  end

  defp repl({:repair, t}, defs) do
    mem(t, [true, false, :force])
    r_open_args(defs, repair: t)
  end

  defp repl({:type, t}, defs) do
    mem(t, [:set, :bag, :duplicate_bag])
    r_open_args(defs, type: t)
  end

  defp repl({:version, version}, defs) do
    is_version(version)
    defs
  end

  defp repl({:debug, bool}, defs) do
    mem(bool, [true, false])
    r_open_args(defs, debug: bool)
  end

  defp repl({_, _}, _) do
    exit(:badarg)
  end

  defp is_min_no_slots(:default) do
    :default
  end

  defp is_min_no_slots(i) when is_integer(i) and i >= 256 do
    i
  end

  defp is_min_no_slots(i) when is_integer(i) and i >= 0 do
    256
  end

  defp is_max_no_slots(:default) do
    :default
  end

  defp is_max_no_slots(i)
       when is_integer(i) and i > 0 and
              i < 1 <<< 31 do
    i
  end

  defp is_comp_min_max(defs) do
    r_open_args(max_no_slots: max, min_no_slots: min) = defs

    cond do
      min === :default ->
        defs

      max === :default ->
        defs

      true ->
        true = min <= max
        defs
    end
  end

  defp is_version(:default) do
    true
  end

  defp is_version(9) do
    true
  end

  defp mem(x, l) do
    case :lists.member(x, l) do
      true ->
        true

      false ->
        exit(:badarg)
    end
  end

  defp options(options, keys) when is_list(options) do
    options(options, keys, [])
  end

  defp options(option, keys) do
    options([option], keys, [])
  end

  defp options(options, [key | keys], l)
       when is_list(options) do
    v =
      case :lists.keysearch(key, 1, options) do
        {:value, {:format, format}}
        when format === :term or
               format === :bchunk ->
          {:ok, format}

        {:value, {:min_no_slots, i}} ->
          case (try do
                  is_min_no_slots(i)
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end) do
            {:EXIT, _} ->
              :badarg

            minNoSlots ->
              {:ok, minNoSlots}
          end

        {:value, {:n_objects, :default}} ->
          {:ok, default_option(key)}

        {:value, {:n_objects, nObjs}}
        when is_integer(nObjs) and nObjs >= 1 ->
          {:ok, nObjs}

        {:value, {:traverse, :select}} ->
          {:ok, :select}

        {:value, {:traverse, {:select, mS}}} ->
          {:ok, {:select, mS}}

        {:value, {:traverse, :first_next}} ->
          {:ok, :first_next}

        {:value, {^key, _}} ->
          :badarg

        false ->
          default = default_option(key)
          {:ok, default}
      end

    case v do
      :badarg ->
        {:badarg, key}

      {:ok, value} ->
        newOptions = :lists.keydelete(key, 1, options)
        options(newOptions, keys, [value | l])
    end
  end

  defp options([], [], l) do
    :lists.reverse(l)
  end

  defp options(options, _, _L) do
    {:badarg, options}
  end

  defp default_option(:format) do
    :term
  end

  defp default_option(:min_no_slots) do
    :default
  end

  defp default_option(:traverse) do
    :select
  end

  defp default_option(:n_objects) do
    :default
  end

  defp listify(l) when is_list(l) do
    l
  end

  defp listify(t) do
    [t]
  end

  defp treq(tab, r) do
    case (try do
            :dets_server.get_pid(tab)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      pid when is_pid(pid) ->
        req(pid, r)

      _ ->
        :badarg
    end
  end

  defp req(proc, r) do
    ref = :erlang.monitor(:process, proc)
    send(proc, {:"$dets_call", self(), r})

    receive do
      {:DOWN, ^ref, :process, ^proc, _Info} ->
        :badarg

      {^proc, reply} ->
        :erlang.demonitor(ref, [:flush])
        reply
    end
  end

  defp einval({:error, {:file_error, _, :einval}}, a) do
    :erlang.error(:badarg, a)
  end

  defp einval({:error, {:file_error, _, :badarg}}, a) do
    :erlang.error(:badarg, a)
  end

  defp einval(reply, _A) do
    reply
  end

  defp badarg(:badarg, a) do
    :erlang.error(:badarg, a)
  end

  defp badarg(reply, _A) do
    reply
  end

  defp undefined(:badarg) do
    :undefined
  end

  defp undefined(reply) do
    reply
  end

  defp badarg_exit(:badarg, a) do
    :erlang.error(:badarg, a)
  end

  defp badarg_exit({:ok, reply}, _A) do
    reply
  end

  defp badarg_exit(reply, _A) do
    exit(reply)
  end

  def init(parent, server) do
    :erlang.process_flag(:trap_exit, true)

    receive do
      {:"$dets_call", from, {:internal_open, ref, args} = op} ->
        try do
          do_internal_open(parent, server, from, ref, args)
        catch
          :exit, :normal ->
            exit(:normal)

          _, bad ->
            bug_found(:no_name, op, bad, __STACKTRACE__, from)
            exit(bad)
        else
          head ->
            open_file_loop(head, 0)
        end
    end
  end

  defp open_file_loop(head, n)
       when :erlang.element(
              1,
              r_head(head, :update_mode)
            ) === :error do
    open_file_loop2(head, n)
  end

  defp open_file_loop(head, n) do
    receive do
      {:"$dets_call", from, {:match_init, _State, _Safe} = op} ->
        do_apply_op(op, from, head, n)

      {:"$dets_call", from, {:bchunk, _State} = op} ->
        do_apply_op(op, from, head, n)

      {:"$dets_call", from, {:next, _Key} = op} ->
        do_apply_op(op, from, head, n)

      {:"$dets_call", from, {:match_delete_init, _MP, _Spec} = op} ->
        do_apply_op(op, from, head, n)

      {:EXIT, pid, reason} when pid === r_head(head, :parent) ->
        _NewHead = do_stop(head)
        exit(reason)

      {:EXIT, pid, reason} when pid === r_head(head, :server) ->
        _NewHead = do_stop(head)
        exit(reason)

      {:EXIT, pid, _Reason} ->
        h2 = remove_fix(head, pid, :close)
        open_file_loop(h2, n)

      {:system, from, req} ->
        :sys.handle_system_msg(req, from, r_head(head, :parent), :dets, [], head)
    after
      0 ->
        open_file_loop2(head, n)
    end
  end

  defp open_file_loop2(head, n) do
    receive do
      {:"$dets_call", from, op} ->
        do_apply_op(op, from, head, n)

      {:EXIT, pid, reason} when pid === r_head(head, :parent) ->
        _NewHead = do_stop(head)
        exit(reason)

      {:EXIT, pid, reason} when pid === r_head(head, :server) ->
        _NewHead = do_stop(head)
        exit(reason)

      {:EXIT, pid, _Reason} ->
        h2 = remove_fix(head, pid, :close)
        open_file_loop(h2, n)

      {:system, from, req} ->
        :sys.handle_system_msg(req, from, r_head(head, :parent), :dets, [], head)

      message ->
        :error_logger.format('** dets: unexpected message(ignored): ~tw~n', [message])
        open_file_loop(head, n)
    end
  end

  defp do_apply_op(op, from, head, n) do
    try do
      apply_op(op, from, head, n)
    catch
      :exit, :normal ->
        exit(:normal)

      _, bad ->
        bug_found(r_head(head, :name), op, bad, __STACKTRACE__, from)
        open_file_loop(head, n)
    else
      :ok ->
        open_file_loop(head, n)

      {n2, h2} when elem(h2, 0) === :head and is_integer(n2) ->
        open_file_loop(h2, n2)

      h2 when elem(h2, 0) === :head ->
        open_file_loop(h2, n)

      {{:more, from1, op1, n1}, newHead} ->
        do_apply_op(op1, from1, newHead, n1)
    end
  end

  defp apply_op(op, from, head, n) do
    case op do
      {:add_user, tab, openArgs} ->
        r_open_args(file: fname, type: type, keypos: keypos, ram_file: ram, access: access) =
          openArgs

        res =
          cond do
            tab === r_head(head, :name) and
              r_head(head, :keypos) === keypos and
              r_head(head, :type) === type and
              r_head(head, :ram_file) === ram and
              r_head(head, :access) === access and
                fname === r_head(head, :filename) ->
              :ok

            true ->
              err({:error, :incompatible_arguments})
          end

        send(from, {self(), res})
        :ok

      :auto_save ->
        case r_head(head, :update_mode) do
          :saved ->
            head

          {:error, _Reason} ->
            head

          _Dirty when n === 0 ->
            :dets_utils.vformat('** dets: Auto save of ~tp\n', [r_head(head, :name)])
            {newHead, _Res} = perform_save(head, true)
            :erlang.garbage_collect()
            {0, newHead}

          :dirty ->
            start_auto_save_timer(head)
            {0, head}
        end

      :close ->
        send(from, {self(), fclose(head)})
        _NewHead = unlink_fixing_procs(head)
        :void
        exit(:normal)

      {:close, pid} ->
        newHead = remove_fix(head, pid, :close)
        send(from, {self(), status(newHead)})
        newHead

      {:corrupt, reason} ->
        {h2, error} = :dets_utils.corrupt_reason(head, reason)
        send(from, {self(), error})
        h2

      {:delayed_write, wrTime} ->
        delayed_write(head, wrTime)

      :info ->
        {h2, res} = finfo(head)
        send(from, {self(), res})
        h2

      {:info, tag} ->
        {h2, res} = finfo(head, tag)
        send(from, {self(), res})
        h2

      {:is_compatible_bchunk_format, term} ->
        res = test_bchunk_format(head, term)
        send(from, {self(), res})
        :ok

      {:internal_open, ref, args} ->
        do_internal_open(r_head(head, :parent), r_head(head, :server), from, ref, args)

      :may_grow when r_head(head, :update_mode) !== :saved ->
        cond do
          r_head(head, :update_mode) === :dirty ->
            {h2, _Res} = :dets_v9.may_grow(head, 0, :many_times)
            {n + 1, h2}

          true ->
            :ok
        end

      {:set_verbose, what} ->
        set_verbose(what)
        send(from, {self(), :ok})
        :ok

      {:where, object} ->
        {h2, res} = where_is_object(head, object)
        send(from, {self(), res})
        h2

      _Message
      when :erlang.element(
             1,
             r_head(head, :update_mode)
           ) === :error ->
        send(from, {self(), status(head)})
        :ok

      {:bchunk_init, tab} ->
        {h2, res} = do_bchunk_init(head, tab)
        send(from, {self(), res})
        h2

      {:bchunk, state} ->
        {h2, res} = do_bchunk(head, state)
        send(from, {self(), res})
        h2

      :delete_all_objects ->
        {h2, res} = fdelete_all_objects(head)
        send(from, {self(), res})
        :erlang.garbage_collect()
        {0, h2}

      {:delete_key, _Keys}
      when r_head(head, :update_mode) === :dirty ->
        stream_op(op, from, [], head, n)

      {:delete_object, objs}
      when r_head(head, :update_mode) === :dirty ->
        case check_objects(objs, r_head(head, :keypos)) do
          true ->
            stream_op(op, from, [], head, n)

          false ->
            send(from, {self(), :badarg})
            :ok
        end

      :first ->
        {h2, res} = ffirst(head)
        send(from, {self(), res})
        h2

      {:initialize, initFun, format, minNoSlots} ->
        {h2, res} = finit(head, initFun, format, minNoSlots)
        send(from, {self(), res})
        :erlang.garbage_collect()
        h2

      {:insert, objs} when r_head(head, :update_mode) === :dirty ->
        case check_objects(objs, r_head(head, :keypos)) do
          true ->
            stream_op(op, from, [], head, n)

          false ->
            send(from, {self(), :badarg})
            :ok
        end

      {:insert_new, objs}
      when r_head(head, :update_mode) === :dirty ->
        {h2, res} = finsert_new(head, objs)
        send(from, {self(), res})
        {n + 1, h2}

      {:lookup_keys, _Keys} ->
        stream_op(op, from, [], head, n)

      {:match_init, state, safe} ->
        {h1, res} = fmatch_init(head, state)

        h2 =
          case res do
            {:cont, _} ->
              h1

            _ when safe === :no_safe ->
              h1

            _ when safe === :safe ->
              do_safe_fixtable(h1, from, false)
          end

        send(from, {self(), res})
        h2

      {:match, mP, spec, nObjs, safe} ->
        {h2, res} = fmatch(head, mP, spec, nObjs, safe, from)
        send(from, {self(), res})
        h2

      {:member, _Key} = ^op ->
        stream_op(op, from, [], head, n)

      {:next, key} ->
        {h2, res} = fnext(head, key)
        send(from, {self(), res})
        h2

      {:match_delete, state}
      when r_head(head, :update_mode) === :dirty ->
        {h1, res} = fmatch_delete(head, state)

        h2 =
          case res do
            {:cont, _S, _N} ->
              h1

            _ ->
              do_safe_fixtable(h1, from, false)
          end

        send(from, {self(), res})
        {n + 1, h2}

      {:match_delete_init, mP, spec}
      when r_head(head, :update_mode) === :dirty ->
        {h2, res} = fmatch_delete_init(head, mP, spec, from)
        send(from, {self(), res})
        {n + 1, h2}

      {:safe_fixtable, bool} ->
        newHead = do_safe_fixtable(head, from, bool)
        send(from, {self(), :ok})
        newHead

      {:slot, slot} ->
        {h2, res} = fslot(head, slot)
        send(from, {self(), res})
        h2

      :sync ->
        {newHead, res} = perform_save(head, true)
        send(from, {self(), res})
        :erlang.garbage_collect()
        {0, newHead}

      {:update_counter, key, incr}
      when r_head(head, :update_mode) === :dirty ->
        {newHead, res} = do_update_counter(head, key, incr)
        send(from, {self(), res})
        {n + 1, newHead}

      writeOp when r_head(head, :update_mode) === :new_dirty ->
        h2 = r_head(head, update_mode: :dirty)
        apply_op(writeOp, from, h2, 0)

      writeOp
      when r_head(head, :access) === :read_write and
             r_head(head, :update_mode) === :saved ->
        case (try do
                :dets_v9.mark_dirty(head)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          :ok ->
            start_auto_save_timer(head)
            h2 = r_head(head, update_mode: :dirty)
            apply_op(writeOp, from, h2, 0)

          {newHead, error} when elem(newHead, 0) === :head ->
            send(from, {self(), error})
            newHead
        end

      writeOp
      when is_tuple(writeOp) and
             r_head(head, :access) === :read ->
        reason = {:access_mode, r_head(head, :filename)}
        send(from, {self(), err({:error, reason})})
        :ok
    end
  end

  defp bug_found(name, op, bad, stacktrace, from) do
    case :dets_utils.debug_mode() do
      true ->
        :error_logger.format(
          '** dets: Bug was found when accessing table ~tw,~n** dets: operation was ~tp and reply was ~tw.~n** dets: Stacktrace: ~tw~n',
          [name, op, bad, stacktrace]
        )

      false ->
        :error_logger.format('** dets: Bug was found when accessing table ~tw~n', [name])
    end

    cond do
      from !== self() ->
        send(from, {self(), {:error, {:dets_bug, name, op, bad}}})
        :ok

      true ->
        :ok
    end
  end

  defp do_internal_open(parent, server, from, ref, args) do
    :void

    case do_open_file(args, parent, server, ref) do
      {:ok, head} ->
        send(from, {self(), :ok})
        head

      error ->
        send(from, {self(), error})
        exit(:normal)
    end
  end

  defp start_auto_save_timer(head) when r_head(head, :auto_save) === :infinity do
    :ok
  end

  defp start_auto_save_timer(head) do
    millis = r_head(head, :auto_save)
    _Ref = :erlang.send_after(millis, self(), {:"$dets_call", self(), :auto_save})
    :ok
  end

  defp stream_op(op, pid, pids, head, n) do
    r_head(fixed: fxd, update_mode: m) = head
    stream_op(head, pids, [], n, pid, op, fxd, m)
  end

  defp stream_loop(head, pids, c, n, false = fxd, m) do
    receive do
      {:"$dets_call", from, message} ->
        stream_op(head, pids, c, n, from, message, fxd, m)
    after
      0 ->
        stream_end(head, pids, c, n, :no_more)
    end
  end

  defp stream_loop(head, pids, c, n, _Fxd, _M) do
    stream_end(head, pids, c, n, :no_more)
  end

  defp stream_op(head, pids, c, n, pid, {:lookup_keys, keys}, fxd, m) do
    nC = [{{:lookup, pid}, keys} | c]
    stream_loop(head, pids, nC, n, fxd, m)
  end

  defp stream_op(head, pids, c, n, pid, {:insert, _Objects} = op, fxd, :dirty = m) do
    nC = [op | c]
    stream_loop(head, [pid | pids], nC, n, fxd, m)
  end

  defp stream_op(head, pids, c, n, pid, {:delete_key, _Keys} = op, fxd, :dirty = m) do
    nC = [op | c]
    stream_loop(head, [pid | pids], nC, n, fxd, m)
  end

  defp stream_op(head, pids, c, n, pid, {:delete_object, _Os} = op, fxd, :dirty = m) do
    nC = [op | c]
    stream_loop(head, [pid | pids], nC, n, fxd, m)
  end

  defp stream_op(head, pids, c, n, pid, {:member, key}, fxd, m) do
    nC = [{{:lookup, [pid]}, [key]} | c]
    stream_loop(head, pids, nC, n, fxd, m)
  end

  defp stream_op(head, pids, c, n, pid, op, _Fxd, _M) do
    stream_end(head, pids, c, n, {pid, op})
  end

  defp stream_end(head, pids0, c, n, next) do
    case (try do
            update_cache(head, :lists.reverse(c))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {head1, [], pwriteList} ->
        stream_end1(pids0, next, n, c, head1, pwriteList)

      {head1, found, pwriteList} ->
        _ = lookup_replies(found)
        stream_end1(pids0, next, n, c, head1, pwriteList)

      head1 when elem(head1, 0) === :head ->
        stream_end2(pids0, pids0, next, n, c, head1, :ok)

      {head1, error} when elem(head1, 0) === :head ->
        fun = fn
          {{:lookup, [pid]}, _Keys}, l ->
            [pid | l]

          {{:lookup, pid}, _Keys}, l ->
            [pid | l]

          _, l ->
            l
        end

        lPs0 = :lists.foldl(fun, [], c)
        lPs = :lists.usort(:lists.flatten(lPs0))
        stream_end2(pids0 ++ lPs, pids0, next, n, c, head1, error)

      detsError ->
        throw(detsError)
    end
  end

  defp stream_end1(pids, next, n, c, head, []) do
    stream_end2(pids, pids, next, n, c, head, :ok)
  end

  defp stream_end1(pids, next, n, c, head, pwriteList) do
    {head1, pR} =
      try do
        :dets_utils.pwrite(head, pwriteList)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    stream_end2(pids, pids, next, n, c, head1, pR)
  end

  defp stream_end2([pid | pids], ps, next, n, c, head, reply) do
    send(pid, {self(), reply})
    stream_end2(pids, ps, next, n + 1, c, head, reply)
  end

  defp stream_end2([], ps, :no_more, n, c, head, _Reply) do
    penalty(head, ps, c)
    {n, head}
  end

  defp stream_end2([], _Ps, {from, op}, n, _C, head, _Reply) do
    {{:more, from, op, n}, head}
  end

  defp penalty(h, _Ps, _C) when r_head(h, :fixed) === false do
    :ok
  end

  defp penalty(_H, _Ps, [{{:lookup, _Pids}, _Keys}]) do
    :ok
  end

  defp penalty(r_head(fixed: {_, [{pid, _}]}), [pid], _C) do
    :ok
  end

  defp penalty(_H, _Ps, _C) do
    :timer.sleep(1)
  end

  defp lookup_replies([{p, o}]) do
    lookup_reply(p, o)
  end

  defp lookup_replies(q) do
    [{p, o} | l] = :dets_utils.family(q)
    lookup_replies(p, :lists.append(o), l)
  end

  defp lookup_replies(p, o, []) do
    lookup_reply(p, o)
  end

  defp lookup_replies(p, o, [{p2, o2} | l]) do
    _ = lookup_reply(p, o)
    lookup_replies(p2, :lists.append(o2), l)
  end

  defp lookup_reply([p], o) do
    send(p, {self(), o !== []})
  end

  defp lookup_reply(p, o) do
    send(p, {self(), o})
  end

  def system_continue(_Parent, _, head) do
    open_file_loop(head, 0)
  end

  def system_terminate(reason, _Parent, _, head) do
    _NewHead = do_stop(head)
    exit(reason)
  end

  def system_code_change(state, _Module, _OldVsn, _Extra) do
    {:ok, state}
  end

  defp read_file_header(fileName, access, ramFile) do
    bF =
      cond do
        ramFile ->
          case :file.read_file(fileName) do
            {:ok, b} ->
              b

            err ->
              :dets_utils.file_error(fileName, err)
          end

        true ->
          fileName
      end

    {:ok, fd} =
      :dets_utils.open(
        bF,
        open_args(access, ramFile)
      )

    {:ok, <<version::size(32)>>} = :dets_utils.pread_close(fd, fileName, 16, 4)

    cond do
      version <= 8 ->
        _ = :file.close(fd)
        throw({:error, {:format_8_no_longer_supported, fileName}})

      version === 9 ->
        :dets_v9.read_file_header(fd, fileName)

      true ->
        _ = :file.close(fd)
        throw({:error, {:not_a_dets_file, fileName}})
    end
  end

  defp fclose(head) do
    {head1, res} = perform_save(head, false)

    case r_head(head1, :ram_file) do
      true ->
        res

      false ->
        :dets_utils.stop_disk_map()
        res2 = :file.close(r_head(head1, :fptr))

        cond do
          res2 === :ok ->
            res

          true ->
            res2
        end
    end
  end

  defp perform_save(head, doSync)
       when r_head(head, :update_mode) === :dirty or
              r_head(head, :update_mode) === :new_dirty do
    case (try do
            {head1, []} = write_cache(head)
            {head2, :ok} = :dets_v9.do_perform_save(head1)
            :ok = ensure_written(head2, doSync)
            {r_head(head2, update_mode: :saved), :ok}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {newHead, _} = reply when elem(newHead, 0) === :head ->
        reply
    end
  end

  defp perform_save(head, _DoSync) do
    {head, status(head)}
  end

  defp ensure_written(head, doSync) when r_head(head, :ram_file) do
    {:ok, eOF} = :dets_utils.position(head, :eof)
    {:ok, bin} = :dets_utils.pread(head, 0, eOF, 0)

    cond do
      doSync ->
        :dets_utils.write_file(head, bin)

      not doSync ->
        case :file.write_file(r_head(head, :filename), bin) do
          :ok ->
            :ok

          error ->
            :dets_utils.corrupt_file(head, error)
        end
    end
  end

  defp ensure_written(head, true) when not r_head(head, :ram_file) do
    :dets_utils.sync(head)
  end

  defp ensure_written(head, false) when not r_head(head, :ram_file) do
    :ok
  end

  defp do_bchunk_init(head, tab) do
    case (try do
            write_cache(head)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {h2, []} ->
        case :dets_v9.table_parameters(h2) do
          :undefined ->
            {h2, {:error, :old_version}}

          parms ->
            l = :dets_utils.all_allocated(h2)

            bin =
              cond do
                l === <<>> ->
                  :eof

                true ->
                  <<>>
              end

            binParms = :erlang.term_to_binary(parms)

            {h2,
             {r_dets_cont(
                no_objs: :default,
                bin: bin,
                alloc: l,
                tab: tab,
                proc: self(),
                what: :bchunk
              ), [binParms]}}
        end

      {newHead, _} = headError when elem(newHead, 0) === :head ->
        headError
    end
  end

  defp do_bchunk(head, r_dets_cont(proc: proc)) when proc !== self() do
    {head, :badarg}
  end

  defp do_bchunk(head, r_dets_cont(bin: :eof)) do
    {head, :"$end_of_table"}
  end

  defp do_bchunk(head, state) do
    case :dets_v9.read_bchunks(head, r_dets_cont(state, :alloc)) do
      {:error, reason} ->
        :dets_utils.corrupt_reason(head, reason)

      {:finished, bins} ->
        {head, {r_dets_cont(state, bin: :eof), bins}}

      {bins, newL} ->
        {head, {r_dets_cont(state, alloc: newL), bins}}
    end
  end

  defp fdelete_all_objects(head) when r_head(head, :fixed) === false do
    case (try do
            do_delete_all_objects(head)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, newHead} ->
        start_auto_save_timer(newHead)
        {newHead, :ok}

      {:error, reason} ->
        :dets_utils.corrupt_reason(head, reason)
    end
  end

  defp fdelete_all_objects(head) do
    {head, :fixed}
  end

  defp do_delete_all_objects(head) do
    r_head(
      fptr: fd,
      name: tab,
      filename: fname,
      type: type,
      keypos: kp,
      ram_file: ram,
      auto_save: auto,
      min_no_slots: minSlots,
      max_no_slots: maxSlots,
      cache: cache
    ) = head

    cacheSz = :dets_utils.cache_size(cache)
    :ok = :dets_utils.truncate(fd, fname, :bof)
    :dets_v9.initiate_file(fd, tab, fname, type, kp, minSlots, maxSlots, ram, cacheSz, auto, true)
  end

  defp ffirst(h) do
    ref = make_ref()

    case (try do
            {ref, ffirst1(h)}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {^ref, {nH, r}} ->
        {nH, {:ok, r}}

      {nH, r} when elem(nH, 0) === :head ->
        {nH, {:error, r}}
    end
  end

  defp ffirst1(h) do
    check_safe_fixtable(h)
    {nH, []} = write_cache(h)
    ffirst(nH, 0)
  end

  defp ffirst(h, slot) do
    case :dets_v9.slot_objs(h, slot) do
      :"$end_of_table" ->
        {h, :"$end_of_table"}

      [] ->
        ffirst(h, slot + 1)

      [x | _] ->
        {h, :erlang.element(r_head(h, :keypos), x)}
    end
  end

  defp finsert(head, objects) do
    case (try do
            update_cache(head, objects, :insert)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {newHead, []} ->
        {newHead, :ok}

      {newHead, _} = headError when elem(newHead, 0) === :head ->
        headError
    end
  end

  defp finsert_new(head, objects) do
    keyPos = r_head(head, :keypos)

    case (try do
            :lists.map(
              fn obj ->
                :erlang.element(keyPos, obj)
              end,
              objects
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      keys when is_list(keys) ->
        case (try do
                update_cache(head, keys, {:lookup, :nopid})
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {head1, pidObjs} when is_list(pidObjs) ->
            case :lists.all(
                   fn {_P, oL} ->
                     oL === []
                   end,
                   pidObjs
                 ) do
              true ->
                case (try do
                        update_cache(head1, objects, :insert)
                      catch
                        :error, e -> {:EXIT, {e, __STACKTRACE__}}
                        :exit, e -> {:EXIT, e}
                        e -> e
                      end) do
                  {newHead, []} ->
                    {newHead, true}

                  {newHead, error} when elem(newHead, 0) === :head ->
                    {newHead, error}
                end

              false = reply ->
                {head1, reply}
            end

          {newHead, _} = headError when elem(newHead, 0) === :head ->
            headError
        end

      _ ->
        {head, :badarg}
    end
  end

  defp do_safe_fixtable(head, pid, true) do
    case r_head(head, :fixed) do
      false ->
        :erlang.link(pid)
        monTime = :erlang.monotonic_time()
        timeOffset = :erlang.time_offset()
        fixed = {{monTime, timeOffset}, [{pid, 1}]}
        ftab = :dets_utils.get_freelists(head)
        r_head(head, fixed: fixed, freelists: {ftab, ftab})

      {timeStamp, counters} ->
        case :lists.keysearch(pid, 1, counters) do
          {:value, {^pid, counter}} ->
            newCounters = :lists.keyreplace(pid, 1, counters, {pid, counter + 1})
            r_head(head, fixed: {timeStamp, newCounters})

          false ->
            :erlang.link(pid)
            fixed = {timeStamp, [{pid, 1} | counters]}
            r_head(head, fixed: fixed)
        end
    end
  end

  defp do_safe_fixtable(head, pid, false) do
    remove_fix(head, pid, false)
  end

  defp remove_fix(head, pid, how) do
    case r_head(head, :fixed) do
      false ->
        head

      {timeStamp, counters} ->
        case :lists.keysearch(pid, 1, counters) do
          {:value, {^pid, counter}}
          when counter === 1 or
                 how === :close ->
            :erlang.unlink(pid)

            case :lists.keydelete(pid, 1, counters) do
              [] ->
                check_growth(head)
                :erlang.garbage_collect()

                r_head(head,
                  fixed: false,
                  freelists: :dets_utils.get_freelists(head)
                )

              newCounters ->
                r_head(head, fixed: {timeStamp, newCounters})
            end

          {:value, {^pid, counter}} ->
            newCounters = :lists.keyreplace(pid, 1, counters, {pid, counter - 1})
            r_head(head, fixed: {timeStamp, newCounters})

          false ->
            head
        end
    end
  end

  defp do_stop(head) do
    _NewHead = unlink_fixing_procs(head)
    fclose(head)
  end

  defp unlink_fixing_procs(head) do
    case r_head(head, :fixed) do
      false ->
        head

      {_, counters} ->
        :lists.foreach(
          fn {pid, _Counter} ->
            :erlang.unlink(pid)
          end,
          counters
        )

        r_head(head,
          fixed: false,
          freelists: :dets_utils.get_freelists(head)
        )
    end
  end

  defp check_growth(r_head(access: :read)) do
    :ok
  end

  defp check_growth(head) do
    noThings = no_things(head)

    cond do
      noThings > r_head(head, :next) ->
        _Ref = :erlang.send_after(200, self(), {:"$dets_call", self(), :may_grow})
        :ok

      true ->
        :ok
    end
  end

  defp finfo(h) do
    case (try do
            write_cache(h)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {h2, []} ->
        info =
          try do
            [
              {:type, r_head(h2, :type)},
              {:keypos, r_head(h2, :keypos)},
              {:size, r_head(h2, :no_objects)},
              {:file_size, file_size(r_head(h2, :fptr), r_head(h2, :filename))},
              {:filename, r_head(h2, :filename)}
            ]
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        {h2, info}

      {h2, _} = headError when elem(h2, 0) === :head ->
        headError
    end
  end

  defp finfo(h, :access) do
    {h, r_head(h, :access)}
  end

  defp finfo(h, :auto_save) do
    {h, r_head(h, :auto_save)}
  end

  defp finfo(h, :bchunk_format) do
    case (try do
            write_cache(h)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {h2, []} ->
        case :dets_v9.table_parameters(h2) do
          :undefined = undef ->
            {h2, undef}

          parms ->
            {h2, :erlang.term_to_binary(parms)}
        end

      {h2, _} = headError when elem(h2, 0) === :head ->
        headError
    end
  end

  defp finfo(h, :delayed_write) do
    {h, :dets_utils.cache_size(r_head(h, :cache))}
  end

  defp finfo(h, :filename) do
    {h, r_head(h, :filename)}
  end

  defp finfo(h, :file_size) do
    case (try do
            write_cache(h)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {h2, []} ->
        {h2,
         try do
           file_size(r_head(h, :fptr), r_head(h, :filename))
         catch
           :error, e -> {:EXIT, {e, __STACKTRACE__}}
           :exit, e -> {:EXIT, e}
           e -> e
         end}

      {h2, _} = headError when elem(h2, 0) === :head ->
        headError
    end
  end

  defp finfo(h, :fixed) do
    {h, not (r_head(h, :fixed) === false)}
  end

  defp finfo(h, :hash) do
    {h, r_head(h, :hash_bif)}
  end

  defp finfo(h, :keypos) do
    {h, r_head(h, :keypos)}
  end

  defp finfo(h, :memory) do
    finfo(h, :file_size)
  end

  defp finfo(h, :no_objects) do
    finfo(h, :size)
  end

  defp finfo(h, :no_keys) do
    case (try do
            write_cache(h)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {h2, []} ->
        {h2, r_head(h2, :no_keys)}

      {h2, _} = headError when elem(h2, 0) === :head ->
        headError
    end
  end

  defp finfo(h, :no_slots) do
    {h, :dets_v9.no_slots(h)}
  end

  defp finfo(h, :pid) do
    {h, self()}
  end

  defp finfo(h, :ram_file) do
    {h, r_head(h, :ram_file)}
  end

  defp finfo(h, :safe_fixed) do
    {h,
     case r_head(h, :fixed) do
       false ->
         false

       {{fixMonTime, timeOffset}, refList} ->
         {make_timestamp(fixMonTime, timeOffset), refList}
     end}
  end

  defp finfo(h, :safe_fixed_monotonic_time) do
    {h,
     case r_head(h, :fixed) do
       false ->
         false

       {{fixMonTime, _TimeOffset}, refList} ->
         {fixMonTime, refList}
     end}
  end

  defp finfo(h, :size) do
    case (try do
            write_cache(h)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {h2, []} ->
        {h2, r_head(h2, :no_objects)}

      {h2, _} = headError when elem(h2, 0) === :head ->
        headError
    end
  end

  defp finfo(h, :type) do
    {h, r_head(h, :type)}
  end

  defp finfo(h, :version) do
    {h, 9}
  end

  defp finfo(h, _) do
    {h, :undefined}
  end

  defp file_size(fd, fileName) do
    {:ok, pos} = :dets_utils.position(fd, fileName, :eof)
    pos
  end

  defp test_bchunk_format(_Head, :undefined) do
    false
  end

  defp test_bchunk_format(head, term) do
    :dets_v9.try_bchunk_header(term, head) !== :not_ok
  end

  defp do_open_file([fname, verbose], parent, server, ref) do
    case (try do
            fopen2(fname, ref)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, {:tooshort, _}} ->
        err({:error, {:not_a_dets_file, fname}})

      {:error, _Reason} = error ->
        err(error)

      {:ok, head} ->
        maybe_put(:verbose, verbose)
        {:ok, r_head(head, parent: parent, server: server)}

      {:EXIT, _Reason} = error ->
        error

      bad ->
        :error_logger.format('** dets: Bug was found in open_file/1, reply was ~tw.~n', [bad])
        {:error, {:dets_bug, fname, bad}}
    end
  end

  defp do_open_file([tab, openArgs, verb], parent, server, _Ref) do
    case (try do
            fopen3(tab, openArgs)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, {:tooshort, _}} ->
        err({:error, {:not_a_dets_file, r_open_args(openArgs, :file)}})

      {:error, _Reason} = error ->
        err(error)

      {:ok, head} ->
        maybe_put(:verbose, verb)
        {:ok, r_head(head, parent: parent, server: server)}

      {:EXIT, _Reason} = error ->
        error

      bad ->
        :error_logger.format(
          '** dets: Bug was found in open_file/2, arguments were~n** dets: ~tw and reply was ~tw.~n',
          [openArgs, bad]
        )

        {:error, {:dets_bug, tab, {:open_file, openArgs}, bad}}
    end
  end

  defp maybe_put(_, :undefined) do
    :ignore
  end

  defp maybe_put(k, v) do
    :erlang.put(k, v)
  end

  defp finit(head, initFun, _Format, _NoSlots)
       when r_head(head, :access) === :read do
    _ =
      try do
        initFun.(:close)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    {head, {:error, {:access_mode, r_head(head, :filename)}}}
  end

  defp finit(head, initFun, _Format, _NoSlots)
       when r_head(head, :fixed) !== false do
    _ =
      try do
        initFun.(:close)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    {head, {:error, {:fixed_table, r_head(head, :name)}}}
  end

  defp finit(head, initFun, format, noSlots) do
    case (try do
            do_finit(head, initFun, format, noSlots)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, newHead} ->
        check_growth(newHead)
        start_auto_save_timer(newHead)
        {newHead, :ok}

      :badarg ->
        {head, :badarg}

      error ->
        :dets_utils.corrupt(head, error)
    end
  end

  defp do_finit(head, init, format, noSlots) do
    r_head(
      fptr: fd,
      type: type,
      keypos: kp,
      auto_save: auto,
      cache: cache,
      filename: fname,
      ram_file: ram,
      min_no_slots: minSlots0,
      max_no_slots: maxSlots,
      name: tab,
      update_mode: updateMode
    ) = head

    cacheSz = :dets_utils.cache_size(cache)

    {how, head1} =
      case format do
        :term when is_integer(noSlots) and noSlots > maxSlots ->
          throw(:badarg)

        :term ->
          minSlots = choose_no_slots(noSlots, minSlots0)

          cond do
            updateMode === :new_dirty and
                minSlots === minSlots0 ->
              {:general_init, head}

            true ->
              :ok = :dets_utils.truncate(fd, fname, :bof)

              {:ok, h} =
                :dets_v9.initiate_file(
                  fd,
                  tab,
                  fname,
                  type,
                  kp,
                  minSlots,
                  maxSlots,
                  ram,
                  cacheSz,
                  auto,
                  false
                )

              {:general_init, h}
          end

        :bchunk ->
          :ok = :dets_utils.truncate(fd, fname, :bof)
          {:bchunk_init, head}
      end

    case how do
      :bchunk_init ->
        case :dets_v9.bchunk_init(head1, init) do
          {:ok, newHead} ->
            {:ok, r_head(newHead, update_mode: :dirty)}

          error ->
            error
        end

      :general_init ->
        cntrs = :ets.new(:dets_init, [])
        input = :dets_v9.bulk_input(head1, init, cntrs)
        slotNumbers = {r_head(head1, :min_no_slots), :bulk_init, maxSlots}
        {reply, sizeData} = do_sort(head1, slotNumbers, input, cntrs, fname)
        bulk = true

        case reply do
          {:ok, noDups, h1} ->
            fsck_copy(sizeData, h1, bulk, noDups)

          else__ ->
            close_files(bulk, sizeData, head1)
            else__
        end
    end
  end

  defp flookup_keys(head, keys) do
    case (try do
            update_cache(head, keys, {:lookup, :nopid})
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {newHead, [{_NoPid, objs}]} ->
        {newHead, objs}

      {newHead, l} when is_list(l) ->
        {newHead,
         :lists.flatmap(
           fn {_Pid, oL} ->
             oL
           end,
           l
         )}

      {newHead, _} = headError when elem(newHead, 0) === :head ->
        headError
    end
  end

  defp fmatch_init(head, r_dets_cont(bin: :eof)) do
    {head, :"$end_of_table"}
  end

  defp fmatch_init(head, c) do
    case scan(head, c) do
      {:scan_error, reason} ->
        :dets_utils.corrupt_reason(head, reason)

      {ts, nC} ->
        {head, {:cont, {ts, nC}}}
    end
  end

  defp fmatch(head, mP, spec, n, safe, from) do
    keyPos = r_head(head, :keypos)

    case find_all_keys(spec, keyPos, []) do
      [] ->
        case (try do
                write_cache(head)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {head1, []} ->
            newHead =
              case safe do
                :safe ->
                  do_safe_fixtable(head1, from, true)

                :no_safe ->
                  head1
              end

            c0 = init_scan(newHead, n)
            {newHead, {:cont, r_dets_cont(c0, match_program: mP)}}

          {newHead, _} = headError when elem(newHead, 0) === :head ->
            headError
        end

      list ->
        keys = :lists.usort(list)
        {newHead, reply} = flookup_keys(head, keys)

        case reply do
          objs when is_list(objs) ->
            {:match_spec, mS} = mP
            matchingObjs = :ets.match_spec_run(objs, mS)
            {newHead, {:done, matchingObjs}}

          error ->
            {newHead, error}
        end
    end
  end

  defp find_all_keys([], _, ks) do
    ks
  end

  defp find_all_keys([{h, _, _} | t], keyPos, ks) when is_tuple(h) do
    case tuple_size(h) do
      enough when enough >= keyPos ->
        key = :erlang.element(keyPos, h)

        case contains_variable(key) do
          true ->
            []

          false ->
            find_all_keys(t, keyPos, [key | ks])
        end

      _ ->
        find_all_keys(t, keyPos, ks)
    end
  end

  defp find_all_keys(_, _, _) do
    []
  end

  defp contains_variable(:_) do
    true
  end

  defp contains_variable(a) when is_atom(a) do
    case :erlang.atom_to_list(a) do
      [?$ | t] ->
        case (try do
                :erlang.list_to_integer(t)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            false

          _ ->
            true
        end

      _ ->
        false
    end
  end

  defp contains_variable(t) when is_tuple(t) do
    contains_variable(:erlang.tuple_to_list(t))
  end

  defp contains_variable([]) do
    false
  end

  defp contains_variable([h | t]) do
    case contains_variable(h) do
      true ->
        true

      false ->
        contains_variable(t)
    end
  end

  defp contains_variable(_) do
    false
  end

  defp fmatch_delete_init(head, mP, spec, from) do
    keyPos = r_head(head, :keypos)

    case (try do
            case find_all_keys(spec, keyPos, []) do
              [] ->
                do_fmatch_delete_var_keys(head, mP, spec, from)

              list ->
                keys = :lists.usort(list)
                do_fmatch_constant_keys(head, keys, mP)
            end
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {newHead, _} = reply when elem(newHead, 0) === :head ->
        reply
    end
  end

  defp fmatch_delete(head, c) do
    case scan(head, c) do
      {:scan_error, reason} ->
        :dets_utils.corrupt_reason(head, reason)

      {[], _} ->
        {head, {:done, 0}}

      {rTs, nC} ->
        {:match_spec, mP} = r_dets_cont(c, :match_program)

        case (try do
                filter_binary_terms(rTs, mP, [])
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            bad = :dets_utils.bad_object(:fmatch_delete, rTs)
            :dets_utils.corrupt_reason(head, bad)

          terms ->
            do_fmatch_delete(head, terms, nC)
        end
    end
  end

  defp do_fmatch_delete_var_keys(head, _MP, [{:_, [], [true]}], _From)
       when r_head(head, :fixed) === false do
    {head1, []} = write_cache(head)
    n = r_head(head1, :no_objects)

    case fdelete_all_objects(head1) do
      {newHead, :ok} ->
        {newHead, {:done, n}}

      reply ->
        reply
    end
  end

  defp do_fmatch_delete_var_keys(head, mP, _Spec, from) do
    head1 = do_safe_fixtable(head, from, true)
    {newHead, []} = write_cache(head1)
    c0 = init_scan(newHead, :default)
    {newHead, {:cont, r_dets_cont(c0, match_program: mP), 0}}
  end

  defp do_fmatch_constant_keys(head, keys, {:match_spec, mP}) do
    case flookup_keys(head, keys) do
      {newHead, readTerms} when is_list(readTerms) ->
        terms = filter_terms(readTerms, mP, [])
        do_fmatch_delete(newHead, terms, :fixed)

      reply ->
        reply
    end
  end

  defp filter_binary_terms([bin | bins], mP, l) do
    term = :erlang.binary_to_term(bin)

    case :ets.match_spec_run([term], mP) do
      [true] ->
        filter_binary_terms(bins, mP, [term | l])

      _ ->
        filter_binary_terms(bins, mP, l)
    end
  end

  defp filter_binary_terms([], _MP, l) do
    l
  end

  defp filter_terms([term | terms], mP, l) do
    case :ets.match_spec_run([term], mP) do
      [true] ->
        filter_terms(terms, mP, [term | l])

      _ ->
        filter_terms(terms, mP, l)
    end
  end

  defp filter_terms([], _MP, l) do
    l
  end

  defp do_fmatch_delete(head, terms, what) do
    n = length(terms)

    case do_delete(head, terms, :delete_object) do
      {newHead, :ok} when what === :fixed ->
        {newHead, {:done, n}}

      {newHead, :ok} ->
        {newHead, {:cont, what, n}}

      reply ->
        reply
    end
  end

  defp do_delete(head, things, what) do
    case (try do
            update_cache(head, things, what)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {newHead, []} ->
        {newHead, :ok}

      {newHead, _} = headError when elem(newHead, 0) === :head ->
        headError
    end
  end

  defp fnext(head, key) do
    slot = :dets_v9.db_hash(key, head)
    ref = make_ref()

    case (try do
            {ref, fnext(head, key, slot)}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {^ref, {h, r}} ->
        {h, {:ok, r}}

      {newHead, _} = headError when elem(newHead, 0) === :head ->
        headError
    end
  end

  defp fnext(h, key, slot) do
    {nH, []} = write_cache(h)

    case :dets_v9.slot_objs(nH, slot) do
      :"$end_of_table" ->
        {nH, :"$end_of_table"}

      l ->
        fnext_search(nH, key, slot, l)
    end
  end

  defp fnext_search(h, k, slot, l) do
    kp = r_head(h, :keypos)

    case beyond_key(k, kp, l) do
      [] ->
        fnext_slot(h, k, slot + 1)

      l2 ->
        {h, :erlang.element(r_head(h, :keypos), hd(l2))}
    end
  end

  defp fnext_slot(h, k, slot) do
    case :dets_v9.slot_objs(h, slot) do
      :"$end_of_table" ->
        {h, :"$end_of_table"}

      [] ->
        fnext_slot(h, k, slot + 1)

      l ->
        {h, :erlang.element(r_head(h, :keypos), hd(l))}
    end
  end

  defp beyond_key(_K, _Kp, []) do
    []
  end

  defp beyond_key(k, kp, [h | t]) do
    case :dets_utils.cmp(:erlang.element(kp, h), k) do
      0 ->
        beyond_key2(k, kp, t)

      _ ->
        beyond_key(k, kp, t)
    end
  end

  defp beyond_key2(_K, _Kp, []) do
    []
  end

  defp beyond_key2(k, kp, [h | t] = l) do
    case :dets_utils.cmp(:erlang.element(kp, h), k) do
      0 ->
        beyond_key2(k, kp, t)

      _ ->
        l
    end
  end

  defp fopen2(fname, tab) do
    case :file.read_file_info(fname) do
      {:ok, _} ->
        acc = :read_write
        ram = false
        {:ok, fd, fH} = read_file_header(fname, acc, ram)

        do__ =
          case :dets_v9.check_file_header(fH, fd) do
            {:ok, head1} ->
              head2 = r_head(head1, filename: fname)

              try do
                {:ok, :dets_v9.init_freelist(head2)}
              catch
                _ ->
                  {:repair, ' has bad free lists, repairing ...'}
              end

            {:error, :not_closed} ->
              m = ' not properly closed, repairing ...'
              {:repair, m}

            else__ ->
              else__
          end

        case do__ do
          {:repair, mess} ->
            :io.format(:user, 'dets: file ~tp~s~n', [fname, mess])

            case fsck(fd, tab, fname, fH, :default, :default) do
              :ok ->
                fopen2(fname, tab)

              error ->
                throw(error)
            end

          {:ok, head} ->
            open_final(head, fname, acc, ram, {3000, 14000}, tab, false)

          {:error, reason} ->
            throw({:error, {reason, fname}})
        end

      error ->
        :dets_utils.file_error(fname, error)
    end
  end

  defp fopen3(tab, openArgs) do
    fileName = r_open_args(openArgs, :file)

    case :file.read_file_info(fileName) do
      {:ok, _} ->
        fopen_existing_file(tab, openArgs)

      error when r_open_args(openArgs, :access) === :read ->
        :dets_utils.file_error(fileName, error)

      _Error ->
        fopen_init_file(tab, openArgs)
    end
  end

  defp fopen_existing_file(tab, openArgs) do
    r_open_args(
      file: fname,
      type: type,
      keypos: kp,
      repair: rep,
      min_no_slots: minSlots,
      max_no_slots: maxSlots,
      ram_file: ram,
      delayed_write: cacheSz,
      auto_save: auto,
      access: acc,
      debug: debug
    ) = openArgs

    {:ok, fd, fH} = read_file_header(fname, acc, ram)

    minF =
      :erlang.or(
        minSlots === :default,
        minSlots === r_fileheader(fH, :min_no_slots)
      )

    maxF =
      :erlang.or(
        maxSlots === :default,
        maxSlots === r_fileheader(fH, :max_no_slots)
      )

    wh =
      case :dets_v9.check_file_header(fH, fd) do
        {:ok, head}
        when rep === :force and
               acc === :read_write and
               r_fileheader(fH, :no_colls) !== :undefined and minF and
               maxF ->
          {:compact, head}

        {:ok, _Head} when rep === :force and acc === :read ->
          throw({:error, {:access_mode, fname}})

        {:ok, _Head} when rep === :force ->
          m = ', repair forced.'
          {:repair, m}

        {:ok, head} ->
          {:final, head}

        {:error, :not_closed}
        when rep === :force and
               acc === :read_write ->
          m = ', repair forced.'
          {:repair, m}

        {:error, :not_closed}
        when rep === true and
               acc === :read_write ->
          m = ' not properly closed, repairing ...'
          {:repair, m}

        {:error, :not_closed} when rep === false ->
          throw({:error, {:needs_repair, fname}})

        {:error, reason} ->
          throw({:error, {reason, fname}})
      end

    do__ =
      case wh do
        {tag, hd} when tag === :final or tag === :compact ->
          hd1 = r_head(hd, filename: fname)

          try do
            {tag, :dets_v9.init_freelist(hd1)}
          catch
            _ ->
              {:repair, ' has bad free lists, repairing ...'}
          end

        else__ ->
          else__
      end

    case do__ do
      _ when r_fileheader(fH, :type) !== type ->
        throw({:error, {:type_mismatch, fname}})

      _ when r_fileheader(fH, :keypos) !== kp ->
        throw({:error, {:keypos_mismatch, fname}})

      {:compact, sourceHead} ->
        :io.format(:user, 'dets: file ~tp is now compacted ...~n', [fname])

        {:ok, newSourceHead} =
          open_final(sourceHead, fname, :read, false, {3000, 14000}, tab, debug)

        case (try do
                compact(newSourceHead)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          :ok ->
            :erlang.garbage_collect()
            fopen3(tab, r_open_args(openArgs, repair: false))

          _Err ->
            _ = :file.close(fd)
            :dets_utils.stop_disk_map()
            :io.format(:user, 'dets: compaction of file ~tp failed, now repairing ...~n', [fname])
            {:ok, fd2, _FH} = read_file_header(fname, acc, ram)
            do_repair(fd2, tab, fname, fH, minSlots, maxSlots, openArgs)
        end

      {:repair, mess} ->
        :io.format(:user, 'dets: file ~tp~s~n', [fname, mess])
        do_repair(fd, tab, fname, fH, minSlots, maxSlots, openArgs)

      {:final, h} ->
        h1 = r_head(h, auto_save: auto)
        open_final(h1, fname, acc, ram, cacheSz, tab, debug)
    end
  end

  defp do_repair(fd, tab, fname, fH, minSlots, maxSlots, openArgs) do
    case fsck(fd, tab, fname, fH, minSlots, maxSlots) do
      :ok ->
        :erlang.garbage_collect()
        fopen3(tab, r_open_args(openArgs, repair: false))

      error ->
        throw(error)
    end
  end

  defp open_final(head, fname, acc, ram, cacheSz, tab, debug) do
    head1 =
      r_head(head,
        access: acc,
        ram_file: ram,
        filename: fname,
        name: tab,
        cache: :dets_utils.new_cache(cacheSz)
      )

    init_disk_map(tab, debug)
    :dets_v9.cache_segps(r_head(head1, :fptr), fname, r_head(head1, :next))
    check_growth(head1)
    {:ok, head1}
  end

  defp fopen_init_file(tab, openArgs) do
    r_open_args(
      file: fname,
      type: type,
      keypos: kp,
      min_no_slots: minSlotsArg,
      max_no_slots: maxSlotsArg,
      ram_file: ram,
      delayed_write: cacheSz,
      auto_save: auto,
      debug: debug
    ) = openArgs

    minSlots = choose_no_slots(minSlotsArg, 256)

    maxSlots =
      choose_no_slots(
        maxSlotsArg,
        32 * 1024 * 1024
      )

    fileSpec =
      cond do
        ram ->
          []

        true ->
          fname
      end

    {:ok, fd} =
      :dets_utils.open(
        fileSpec,
        open_args(:read_write, ram)
      )

    init_disk_map(tab, debug)

    case (try do
            :dets_v9.initiate_file(
              fd,
              tab,
              fname,
              type,
              kp,
              minSlots,
              maxSlots,
              ram,
              cacheSz,
              auto,
              true
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, reason} when ram ->
        _ = :file.close(fd)
        throw({:error, reason})

      {:error, reason} ->
        _ = :file.close(fd)
        _ = :file.delete(fname)
        throw({:error, reason})

      {:ok, head} ->
        start_auto_save_timer(head)
        {:ok, r_head(head, update_mode: :new_dirty)}
    end
  end

  defp init_disk_map(name, debug) do
    case debug or :dets_utils.debug_mode() do
      true ->
        :dets_utils.init_disk_map(name)

      false ->
        :ok
    end
  end

  defp open_args(access, ramFile) do
    a1 =
      case access do
        :read ->
          []

        :read_write ->
          [:write]
      end

    a2 =
      case ramFile do
        true ->
          [:ram]

        false ->
          [:raw]
      end

    a1 ++ a2 ++ [:binary, :read]
  end

  defp compact(sourceHead) do
    r_head(
      name: tab,
      filename: fname,
      fptr: sFd,
      type: type,
      keypos: kp,
      ram_file: ram,
      auto_save: auto
    ) = sourceHead

    tmp = tempfile(fname)
    tblParms = :dets_v9.table_parameters(sourceHead)

    {:ok, fd} =
      :dets_utils.open(
        tmp,
        open_args(:read_write, false)
      )

    cacheSz = {3000, 14000}

    head =
      case (try do
              :dets_v9.prep_table_copy(fd, tab, tmp, type, kp, ram, cacheSz, auto, tblParms)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:ok, h} ->
          h

        error ->
          _ = :file.close(fd)
          _ = :file.delete(tmp)
          throw(error)
      end

    case :dets_v9.compact_init(sourceHead, head, tblParms) do
      {:ok, newHead} ->
        r =
          case fclose(newHead) do
            :ok ->
              :ok = :file.close(sFd)
              :dets_utils.rename(tmp, fname)

            e ->
              e
          end

        cond do
          r === :ok ->
            :ok

          true ->
            _ = :file.delete(tmp)
            throw(r)
        end

      err ->
        _ = :file.close(fd)
        _ = :file.delete(tmp)
        throw(err)
    end
  end

  defp fsck(fd, tab, fname, fH, minSlotsArg, maxSlotsArg) do
    r_fileheader(
      min_no_slots: minSlotsFile,
      max_no_slots: maxSlotsFile
    ) = fH

    estNoSlots0 = file_no_things(fH)
    minSlots = choose_no_slots(minSlotsArg, minSlotsFile)
    maxSlots = choose_no_slots(maxSlotsArg, maxSlotsFile)

    estNoSlots =
      :erlang.min(
        maxSlots,
        :erlang.max(minSlots, estNoSlots0)
      )

    slotNumbers = {minSlots, estNoSlots, maxSlots}

    case fsck_try(fd, tab, fH, fname, slotNumbers) do
      {:try_again, betterNoSlots} ->
        betterSlotNumbers = {minSlots, betterNoSlots, maxSlots}

        case fsck_try(fd, tab, fH, fname, betterSlotNumbers) do
          {:try_again, _} ->
            _ = :file.close(fd)
            {:error, {:cannot_repair, fname}}

          else__ ->
            else__
        end

      else__ ->
        else__
    end
  end

  defp choose_no_slots(:default, noSlots) do
    noSlots
  end

  defp choose_no_slots(noSlots, _) do
    noSlots
  end

  defp fsck_try(fd, tab, fH, fname, slotNumbers) do
    tmp = tempfile(fname)
    r_fileheader(type: type, keypos: keyPos) = fH
    {_MinSlots, estNoSlots, maxSlots} = slotNumbers

    openArgs =
      r_open_args(
        file: tmp,
        type: type,
        keypos: keyPos,
        repair: false,
        min_no_slots: estNoSlots,
        max_no_slots: maxSlots,
        ram_file: false,
        delayed_write: {3000, 14000},
        auto_save: :infinity,
        access: :read_write,
        debug: false
      )

    case (try do
            fopen3(tab, openArgs)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, head} ->
        case fsck_try_est(head, fd, fname, slotNumbers, fH) do
          {:ok, newHead} ->
            r =
              case fclose(newHead) do
                :ok ->
                  :dets_utils.rename(tmp, fname)

                error ->
                  error
              end

            cond do
              r === :ok ->
                :ok

              true ->
                _ = :file.delete(tmp)
                r
            end

          tryAgainOrError ->
            _ = :file.delete(tmp)
            tryAgainOrError
        end

      error ->
        _ = :file.close(fd)
        error
    end
  end

  defp tempfile(fname) do
    tmp = :lists.concat([fname, '.TMP'])

    case :file.delete(tmp) do
      {:error, _Reason} ->
        :ok

      :ok ->
        assure_no_file(tmp)
    end

    tmp
  end

  defp assure_no_file(file) do
    case :file.read_file_info(file) do
      {:ok, _FileInfo} ->
        :timer.sleep(100)
        assure_no_file(file)

      {:error, _} ->
        :ok
    end
  end

  defp fsck_try_est(head, fd, fname, slotNumbers, fH) do
    cntrs = :ets.new(:dets_repair, [])
    input = :dets_v9.fsck_input(head, fd, cntrs, fH)
    {reply, sizeData} = do_sort(head, slotNumbers, input, cntrs, fname)
    bulk = false

    case reply do
      {:ok, noDups, h1} ->
        _ = :file.close(fd)
        fsck_copy(sizeData, h1, bulk, noDups)

      {:try_again, _} = return ->
        close_files(bulk, sizeData, head)
        return

      else__ ->
        _ = :file.close(fd)
        close_files(bulk, sizeData, head)
        else__
    end
  end

  defp do_sort(head, slotNumbers, input, cntrs, fname) do
    output = :dets_v9.output_objs(head, slotNumbers, cntrs)
    tmpDir = :filename.dirname(fname)

    reply =
      try do
        :file_sorter.sort(input, output, [{:format, :binary}, {:tmpdir, tmpDir}])
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    l = :ets.tab2list(cntrs)
    :ets.delete(cntrs)
    {reply, :lists.reverse(:lists.keysort(1, l))}
  end

  defp fsck_copy([{_LogSz, pos, bins, _NoObjects} | sizeData], head, _Bulk, noDups)
       when is_list(bins) do
    true = noDups === 0

    pWs = [
      {pos, bins}
      | :lists.map(
          fn {_, p, b, _} ->
            {p, b}
          end,
          sizeData
        )
    ]

    r_head(fptr: fd, filename: fileName) = head
    :dets_utils.pwrite(fd, fileName, pWs)
    {:ok, r_head(head, update_mode: :dirty)}
  end

  defp fsck_copy(sizeData, head, bulk, noDups) do
    try do
      fsck_copy1(sizeData, head, bulk, noDups)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp fsck_copy1([szData | l], head, bulk, noDups) do
    out = r_head(head, :fptr)
    {logSz, pos, {fileName, fd}, noObjects} = szData

    size =
      cond do
        noObjects === 0 ->
          0

        true ->
          1 <<< (logSz - 1)
      end

    expectedSize = size * noObjects

    case close_tmp(fd) do
      :ok ->
        :ok

      err ->
        close_files(bulk, l, head)
        :dets_utils.file_error(fileName, err)
    end

    case :file.position(out, pos) do
      {:ok, ^pos} ->
        :ok

      err2 ->
        close_files(bulk, l, head)
        :dets_utils.file_error(r_head(head, :filename), err2)
    end

    cR = :file.copy({fileName, [:raw, :binary]}, out)
    _ = :file.delete(fileName)

    case cR do
      {:ok, copied}
      when copied === expectedSize or
             noObjects === 0 ->
        fsck_copy1(l, head, bulk, noDups)

      {:ok, _Copied} ->
        close_files(bulk, l, head)

        reason =
          cond do
            bulk ->
              :initialization_failed

            true ->
              :repair_failed
          end

        {:error, {reason, r_head(head, :filename)}}

      fError ->
        close_files(bulk, l, head)
        :dets_utils.file_error(fileName, fError)
    end
  end

  defp fsck_copy1([], head, _Bulk, noDups) when noDups !== 0 do
    {:error, {:initialization_failed, r_head(head, :filename)}}
  end

  defp fsck_copy1([], head, _Bulk, _NoDups) do
    {:ok, r_head(head, update_mode: :dirty)}
  end

  defp close_files(false, sizeData, head) do
    _ = :file.close(r_head(head, :fptr))
    close_files(true, sizeData, head)
  end

  defp close_files(true, sizeData, _Head) do
    fun = fn
      {_Size, _Pos, {fileName, fd}, _No} ->
        _ = close_tmp(fd)
        :file.delete(fileName)

      _ ->
        :ok
    end

    :lists.foreach(fun, sizeData)
  end

  defp close_tmp(fd) do
    :file.close(fd)
  end

  defp fslot(h, slot) do
    case (try do
            {nH, []} = write_cache(h)
            objs = :dets_v9.slot_objs(nH, slot)
            {nH, objs}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {newHead, _Objects} = reply
      when elem(newHead, 0) === :head ->
        reply
    end
  end

  defp do_update_counter(head, _Key, _Incr)
       when r_head(head, :type) !== :set do
    {head, :badarg}
  end

  defp do_update_counter(head, key, incr) do
    case flookup_keys(head, [key]) do
      {h1, [o]} ->
        kp = r_head(h1, :keypos)

        case (try do
                try_update_tuple(o, kp, incr)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            {h1, :badarg}

          {new, term} ->
            case finsert(h1, [term]) do
              {h2, :ok} ->
                {h2, new}

              reply ->
                reply
            end
        end

      {h1, []} ->
        {h1, :badarg}

      headError ->
        headError
    end
  end

  defp try_update_tuple(o, _Kp, {pos, incr}) do
    try_update_tuple2(o, pos, incr)
  end

  defp try_update_tuple(o, kp, incr) do
    try_update_tuple2(o, kp + 1, incr)
  end

  defp try_update_tuple2(o, pos, incr) do
    new = :erlang.element(pos, o) + incr
    {new, :erlang.setelement(pos, o, new)}
  end

  defp set_verbose(true) do
    :erlang.put(:verbose, :yes)
  end

  defp set_verbose(_) do
    :erlang.erase(:verbose)
  end

  defp where_is_object(head, object) do
    keypos = r_head(head, :keypos)

    case check_objects([object], keypos) do
      true ->
        case (try do
                write_cache(head)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {newHead, []} ->
            {newHead, :dets_v9.find_object(newHead, object)}

          {newHead, _} = headError when elem(newHead, 0) === :head ->
            headError
        end

      false ->
        {head, :badarg}
    end
  end

  defp check_objects([t | ts], kp) when tuple_size(t) >= kp do
    check_objects(ts, kp)
  end

  defp check_objects(l, _Kp) do
    l === []
  end

  defp no_things(head) do
    r_head(head, :no_keys)
  end

  defp file_no_things(fH) do
    r_fileheader(fH, :no_keys)
  end

  defp update_cache(head, keysOrObjects, what) do
    {head1, lU, pwriteList} =
      update_cache(
        head,
        [{what, keysOrObjects}]
      )

    {newHead, :ok} = :dets_utils.pwrite(head1, pwriteList)
    {newHead, lU}
  end

  defp update_cache(head, toAdd) do
    cache = r_head(head, :cache)
    r_cache(cache: c, csize: size0, inserts: ins) = cache
    newSize = size0 + :erlang.external_size(toAdd)
    {newC, newIns, lookup, found} = cache_binary(head, toAdd, c, size0, ins, false, [])
    newCache = r_cache(cache, cache: newC, csize: newSize, inserts: newIns)
    head1 = r_head(head, cache: newCache)

    cond do
      lookup or newSize >= r_cache(cache, :tsize) ->
        {newHead, lU, pwriteList} = :dets_v9.write_cache(head1)
        {newHead, found ++ lU, pwriteList}

      newC === [] ->
        {head1, found, []}

      r_cache(cache, :wrtime) === :undefined ->
        now = time_now()
        me = self()
        call = {:"$dets_call", me, {:delayed_write, now}}
        :erlang.send_after(r_cache(cache, :delay), me, call)
        {r_head(head1, cache: r_cache(newCache, wrtime: now)), found, []}

      size0 === 0 ->
        {r_head(head1, cache: r_cache(newCache, wrtime: time_now())), found, []}

      true ->
        {head1, found, []}
    end
  end

  defp cache_binary(head, [{q, os} | l], c, seq, ins, lu, f)
       when q === :delete_object do
    cache_obj_op(head, l, c, seq, ins, lu, f, os, r_head(head, :keypos), q)
  end

  defp cache_binary(head, [{q, os} | l], c, seq, ins, lu, f)
       when q === :insert do
    newIns = ins + length(os)
    cache_obj_op(head, l, c, seq, newIns, lu, f, os, r_head(head, :keypos), q)
  end

  defp cache_binary(head, [{q, ks} | l], c, seq, ins, lu, f)
       when q === :delete_key do
    cache_key_op(head, l, c, seq, ins, lu, f, ks, q)
  end

  defp cache_binary(head, [{q, ks} | l], c, seq, ins, _Lu, f)
       when c === [] do
    cache_key_op(head, l, c, seq, ins, true, f, ks, q)
  end

  defp cache_binary(head, [{q, ks} | l], c, seq, ins, lu, f) do
    case :dets_utils.cache_lookup(r_head(head, :type), ks, c, []) do
      false ->
        cache_key_op(head, l, c, seq, ins, true, f, ks, q)

      found ->
        {:lookup, pid} = q
        cache_binary(head, l, c, seq, ins, lu, [{pid, found} | f])
    end
  end

  defp cache_binary(_Head, [], c, _Seq, ins, lu, f) do
    {c, ins, lu, f}
  end

  defp cache_key_op(head, l, c, seq, ins, lu, f, [k | ks], q) do
    e = {k, {seq, q}}
    cache_key_op(head, l, [e | c], seq + 1, ins, lu, f, ks, q)
  end

  defp cache_key_op(head, l, c, seq, ins, lu, f, [], _Q) do
    cache_binary(head, l, c, seq, ins, lu, f)
  end

  defp cache_obj_op(head, l, c, seq, ins, lu, f, [o | os], kp, q) do
    e = {:erlang.element(kp, o), {seq, {q, o}}}
    cache_obj_op(head, l, [e | c], seq + 1, ins, lu, f, os, kp, q)
  end

  defp cache_obj_op(head, l, c, seq, ins, lu, f, [], _Kp, _Q) do
    cache_binary(head, l, c, seq, ins, lu, f)
  end

  defp delayed_write(head, wrTime) do
    cache = r_head(head, :cache)
    lastWrTime = r_cache(cache, :wrtime)

    cond do
      lastWrTime === wrTime ->
        case (try do
                write_cache(head)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {head2, []} ->
            newCache = r_cache(r_head(head2, :cache), wrtime: :undefined)
            r_head(head2, cache: newCache)

          {newHead, _Error} ->
            newHead
        end

      true ->
        cond do
          r_cache(cache, :csize) === 0 ->
            newCache = r_cache(cache, wrtime: :undefined)
            r_head(head, cache: newCache)

          true ->
            when__ = round((lastWrTime - wrTime) / 1000)
            me = self()
            call = {:"$dets_call", me, {:delayed_write, lastWrTime}}
            :erlang.send_after(when__, me, call)
            head
        end
    end
  end

  defp write_cache(head) do
    {head1, lU, pwriteList} = :dets_v9.write_cache(head)
    {newHead, :ok} = :dets_utils.pwrite(head1, pwriteList)
    {newHead, lU}
  end

  defp status(head) do
    case r_head(head, :update_mode) do
      :saved ->
        :ok

      :dirty ->
        :ok

      :new_dirty ->
        :ok

      error ->
        error
    end
  end

  defp init_scan(head, noObjs) do
    check_safe_fixtable(head)
    freeLists = :dets_utils.get_freelists(head)
    base = r_head(head, :base)

    case :dets_utils.find_next_allocated(freeLists, base, base) do
      {from, to} ->
        r_dets_cont(no_objs: noObjs, bin: <<>>, alloc: {from, to, <<>>})

      :none ->
        r_dets_cont(no_objs: noObjs, bin: :eof, alloc: <<>>)
    end
  end

  defp check_safe_fixtable(head) do
    case r_head(head, :fixed) === false and
           (:erlang.get(:verbose) === :yes or :dets_utils.debug_mode()) do
      true ->
        :error_logger.format('** dets: traversal of ~tp needs safe_fixtable~n', [
          r_head(head, :name)
        ])

      false ->
        :ok
    end
  end

  defp scan(_Head, r_dets_cont(alloc: <<>>) = c) do
    {[], c}
  end

  defp scan(head, c) do
    r_dets_cont(no_objs: no, alloc: l0, bin: bin) = c
    {from, to, l} = l0

    r =
      case no do
        :default ->
          0

        _ when is_integer(no) ->
          -no - 1
      end

    scan(bin, head, from, to, l, [], r, {c, r_head(head, :type)})
  end

  defp scan(bin, h, from, to, l, ts, r, {c0, type} = c) do
    case :dets_v9.scan_objs(h, bin, from, to, l, ts, r, type) do
      {:more, nFrom, nTo, nL, nTs, nR, sz} ->
        scan_read(h, nFrom, nTo, sz, nL, nTs, nR, c)

      {:stop, <<>> = b, nFrom, nTo, <<>> = nL, nTs} ->
        ftab = :dets_utils.get_freelists(h)

        case :dets_utils.find_next_allocated(ftab, nFrom, r_head(h, :base)) do
          :none ->
            {nTs, r_dets_cont(c0, bin: :eof, alloc: b)}

          _ ->
            {nTs, r_dets_cont(c0, bin: b, alloc: {nFrom, nTo, nL})}
        end

      {:stop, b, nFrom, nTo, nL, nTs} ->
        {nTs, r_dets_cont(c0, bin: b, alloc: {nFrom, nTo, nL})}

      :bad_object ->
        {:scan_error, :dets_utils.bad_object(:scan, {from, to, bin})}
    end
  end

  defp scan_read(_H, from, to, _Min, l0, ts, r, {c, _Type})
       when r >= 8192 do
    l = {from, to, l0}
    {ts, r_dets_cont(c, bin: <<>>, alloc: l)}
  end

  defp scan_read(h, from, _To, min, _L, ts, r, c) do
    max =
      cond do
        min < 8192 ->
          8192

        true ->
          min
      end

    freeLists = :dets_utils.get_freelists(h)

    case :dets_utils.find_allocated(freeLists, from, max, r_head(h, :base)) do
      <<>> = bin0 ->
        {cont, _} = c
        {ts, r_dets_cont(cont, bin: :eof, alloc: bin0)}

      <<from1::size(32), to1::size(32), l1::binary>> ->
        case :dets_utils.pread_n(r_head(h, :fptr), from1, max) do
          :eof ->
            {:scan_error, :premature_eof}

          newBin ->
            scan(newBin, h, from1, to1, l1, ts, r, c)
        end
    end
  end

  defp err(error) do
    case :erlang.get(:verbose) do
      :yes ->
        :error_logger.format('** dets: failed with ~tw~n', [error])
        error

      :undefined ->
        error
    end
  end

  defp time_now() do
    :erlang.monotonic_time(1_000_000)
  end

  defp make_timestamp(monTime, timeOffset) do
    erlangSystemTime = :erlang.convert_time_unit(monTime + timeOffset, :native, :microsecond)
    megaSecs = div(erlangSystemTime, 1_000_000_000_000)
    secs = div(erlangSystemTime, 1_000_000) - megaSecs * 1_000_000
    microSecs = rem(erlangSystemTime, 1_000_000)
    {megaSecs, secs, microSecs}
  end

  def file_info(fileName) do
    case (try do
            read_file_header(fileName, :read, false)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, fd, fH} ->
        _ = :file.close(fd)
        :dets_v9.file_info(fH)

      other ->
        other
    end
  end

  def get_head_field(fd, field) do
    :dets_utils.read_4(fd, field)
  end

  def view(fileName) do
    case (try do
            read_file_header(fileName, :read, false)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, fd, fH} ->
        try do
          :dets_v9.check_file_header(fH, fd)
        else
          {:ok, h0} ->
            case :dets_v9.check_file_header(fH, fd) do
              {:ok, ^h0} ->
                h = :dets_v9.init_freelist(h0)
                v_free_list(h)
                :dets_v9.v_segments(h)
                :ok

              x ->
                x
            end
        after
          _ = :file.close(fd)
        end

      x ->
        x
    end
  end

  defp v_free_list(head) do
    :io.format('FREE LIST ...... \n', [])
    :io.format('~p~n', [:dets_utils.all_free(head)])
    :io.format('END OF FREE LIST \n', [])
  end
end
