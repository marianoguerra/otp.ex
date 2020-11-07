defmodule :m_dialyzer_plt do
  use Bitwise
  require Record

  Record.defrecord(:r_plt, :plt,
    info: :undefined,
    types: :undefined,
    contracts: :undefined,
    callbacks: :undefined,
    exported_types: :undefined
  )

  Record.defrecord(:r_analysis, :analysis,
    analysis_pid: :undefined,
    type: :succ_typings,
    defines: [],
    doc_plt: :undefined,
    files: [],
    include_dirs: [],
    start_from: :byte_code,
    plt: :undefined,
    use_contracts: true,
    race_detection: false,
    behaviours_chk: false,
    timing: false,
    timing_server: :none,
    callgraph_file: '',
    solvers: :undefined
  )

  Record.defrecord(:r_options, :options,
    files: [],
    files_rec: [],
    analysis_type: :succ_typings,
    timing: false,
    defines: [],
    from: :byte_code,
    get_warnings: :maybe,
    init_plts: [],
    include_dirs: [],
    output_plt: :none,
    legal_warnings: :ordsets.new(),
    report_mode: :normal,
    erlang_mode: false,
    use_contracts: true,
    output_file: :none,
    output_format: :formatted,
    filename_opt: :basename,
    indent_opt: true,
    callgraph_file: '',
    check_plt: true,
    solvers: [],
    native: :maybe,
    native_cache: true
  )

  Record.defrecord(:r_contract, :contract, contracts: [], args: [], forms: [])

  Record.defrecord(:r_file_plt, :file_plt,
    version: '',
    file_md5_list: [],
    info: :dict.new(),
    contracts: :dict.new(),
    callbacks: :dict.new(),
    types: :dict.new(),
    exported_types: :sets.new(),
    mod_deps: :undefined,
    implementation_md5: []
  )

  def new() do
    [eTSInfo, eTSContracts] =
      for name <- [:plt_info, :plt_contracts] do
        :ets.new(name, [:public])
      end

    [eTSTypes, eTSCallbacks, eTSExpTypes] =
      for name <- [:plt_types, :plt_callbacks, :plt_exported_types] do
        :ets.new(name, [:compressed, :public])
      end

    r_plt(
      info: eTSInfo,
      types: eTSTypes,
      contracts: eTSContracts,
      callbacks: eTSCallbacks,
      exported_types: eTSExpTypes
    )
  end

  def delete_module(
        r_plt(
          info: info,
          types: types,
          contracts: contracts,
          callbacks: callbacks,
          exported_types: expTypes
        ),
        mod
      ) do
    r_plt(
      info: table_delete_module(info, mod),
      types: table_delete_module2(types, mod),
      contracts: table_delete_module(contracts, mod),
      callbacks: table_delete_module2(callbacks, mod),
      exported_types: table_delete_module1(expTypes, mod)
    )
  end

  def delete_list(
        r_plt(info: info, contracts: contracts) = plt,
        list
      ) do
    r_plt(plt,
      info: ets_table_delete_list(info, list),
      contracts: ets_table_delete_list(contracts, list)
    )
  end

  def insert_contract_list(r_plt(contracts: contracts) = pLT, list) do
    true = :ets.insert(contracts, list)
    pLT
  end

  def insert_callbacks(r_plt(callbacks: callbacks) = plt, codeserver) do
    callbacksList = :dialyzer_codeserver.get_callbacks(codeserver)

    callbacksByModule =
      for m <-
            :lists.usort(
              for {{m, _, _}, _} <- callbacksList do
                m
              end
            ) do
        {m,
         for {{m1, _, _}, _} = cb <- callbacksList,
             m1 === m do
           cb
         end}
      end

    true = :ets.insert(callbacks, callbacksByModule)
    plt
  end

  def is_contract(r_plt(contracts: eTSContracts), {m, f, _} = mFA)
      when is_atom(m) and is_atom(f) do
    :ets.member(eTSContracts, mFA)
  end

  def lookup_contract(r_plt(contracts: eTSContracts), {m, f, _} = mFA)
      when is_atom(m) and is_atom(f) do
    ets_table_lookup(eTSContracts, mFA)
  end

  def lookup_callbacks(r_plt(callbacks: eTSCallbacks), mod)
      when is_atom(mod) do
    ets_table_lookup(eTSCallbacks, mod)
  end

  def insert_list(r_plt(info: info) = pLT, list) do
    true = :ets.insert(info, list)
    pLT
  end

  def lookup(plt, {m, f, _} = mFA)
      when is_atom(m) and
             is_atom(f) do
    lookup_1(plt, mFA)
  end

  def lookup(plt, label) when is_integer(label) do
    lookup_1(plt, label)
  end

  defp lookup_1(r_plt(info: info), mFAorLabel) do
    ets_table_lookup(info, mFAorLabel)
  end

  def insert_types(pLT, records) do
    :ok = :dialyzer_utils.ets_move(records, r_plt(pLT, :types))
    pLT
  end

  def insert_exported_types(pLT, expTypes) do
    :ok =
      :dialyzer_utils.ets_move(
        expTypes,
        r_plt(pLT, :exported_types)
      )

    pLT
  end

  def get_module_types(r_plt(types: types), m) when is_atom(m) do
    ets_table_lookup(types, m)
  end

  def get_exported_types(r_plt(exported_types: eTSExpTypes)) do
    :sets.from_list(
      for {e} <- table_to_list(eTSExpTypes) do
        e
      end
    )
  end

  def lookup_module(r_plt(info: info), m) when is_atom(m) do
    table_lookup_module(info, m)
  end

  def all_modules(r_plt(info: info, contracts: cs)) do
    :sets.union(
      table_all_modules(info),
      table_all_modules(cs)
    )
  end

  def contains_mfa(r_plt(info: info, contracts: contracts), mFA) do
    :ets.member(info, mFA) or :ets.member(contracts, mFA)
  end

  def get_default_plt() do
    case :os.getenv('DIALYZER_PLT') do
      false ->
        {:ok, [[homeDir]]} = :init.get_argument(:home)
        :filename.join(homeDir, '.dialyzer_plt')

      userSpecPlt ->
        userSpecPlt
    end
  end

  def plt_and_info_from_file(fileName) do
    from_file(fileName, true)
  end

  def from_file(fileName) do
    from_file(fileName, false)
  end

  defp from_file(fileName, returnInfo) do
    plt = new()

    fun = fn ->
      from_file1(plt, fileName, returnInfo)
    end

    case subproc(fun) do
      {:ok, return} ->
        return

      {:error, msg} ->
        delete(plt)
        plt_error(msg)
    end
  end

  defp from_file1(plt, fileName, returnInfo) do
    case get_record_from_file(fileName) do
      {:ok, rec} ->
        case check_version(rec) do
          :error ->
            msg = :io_lib.format('Old PLT file ~ts\n', [fileName])
            {:error, msg}

          :ok ->
            r_file_plt(
              info: fileInfo,
              contracts: fileContracts,
              callbacks: fileCallbacks,
              types: fileTypes,
              exported_types: fileExpTypes
            ) = rec

            types =
              for {mod, types} <- :dict.to_list(fileTypes) do
                {mod, :maps.from_list(:dict.to_list(types))}
              end

            callbacksList = :dict.to_list(fileCallbacks)

            callbacksByModule =
              for m <-
                    :lists.usort(
                      for {{m, _, _}, _} <- callbacksList do
                        m
                      end
                    ) do
                {m,
                 for {{m1, _, _}, _} = cb <- callbacksList,
                     m1 === m do
                   cb
                 end}
              end

            r_plt(
              info: eTSInfo,
              types: eTSTypes,
              contracts: eTSContracts,
              callbacks: eTSCallbacks,
              exported_types: eTSExpTypes
            ) = plt

            [true, true, true] =
              for {eTS, data} <- [
                    {eTSInfo, :dict.to_list(fileInfo)},
                    {eTSTypes, types},
                    {eTSContracts, :dict.to_list(fileContracts)}
                  ] do
                :ets.insert(eTS, data)
              end

            true = :ets.insert(eTSCallbacks, callbacksByModule)

            true =
              :ets.insert(
                eTSExpTypes,
                for eT <- :sets.to_list(fileExpTypes) do
                  {eT}
                end
              )

            case returnInfo do
              false ->
                {:ok, plt}

              true ->
                pltInfo = {r_file_plt(rec, :file_md5_list), r_file_plt(rec, :mod_deps)}
                {:ok, {plt, pltInfo}}
            end
        end

      {:error, reason} ->
        msg = :io_lib.format('Could not read PLT file ~ts: ~p\n', [fileName, reason])
        {:error, msg}
    end
  end

  def included_files(fileName) do
    fun = fn ->
      included_files1(fileName)
    end

    subproc(fun)
  end

  defp included_files1(fileName) do
    case get_record_from_file(fileName) do
      {:ok, r_file_plt(file_md5_list: md5)} ->
        {:ok,
         for {file, _} <- md5 do
           file
         end}

      {:error, _What} = error ->
        error
    end
  end

  defp check_version(
         r_file_plt(
           version: :EFE_TODO_VSN_MACRO,
           implementation_md5: implMd5
         )
       ) do
    case compute_new_md5(implMd5, [], []) do
      :ok ->
        :ok

      {:differ, _, _} ->
        :error

      {:error, _} ->
        :error
    end
  end

  defp check_version(r_file_plt()) do
    :error
  end

  defp get_record_from_file(fileName) do
    case :file.read_file(fileName) do
      {:ok, bin} ->
        try do
          :erlang.binary_to_term(bin)
        catch
          _, _ ->
            {:error, :not_valid}
        else
          r_file_plt() = filePLT ->
            {:ok, filePLT}

          _ ->
            {:error, :not_valid}
        end

      {:error, :enoent} ->
        {:error, :no_such_file}

      {:error, _} ->
        {:error, :read_error}
    end
  end

  def merge_plts(list) do
    {infoList, typesList, expTypesList, contractsList, callbacksList} = group_fields(list)

    r_plt(
      info: table_merge(infoList),
      types: table_merge(typesList),
      exported_types: sets_merge(expTypesList),
      contracts: table_merge(contractsList),
      callbacks: table_merge(callbacksList)
    )
  end

  defp merge_disj_plts(list) do
    {infoList, typesList, expTypesList, contractsList, callbacksList} = group_fields(list)

    r_plt(
      info: table_disj_merge(infoList),
      types: table_disj_merge(typesList),
      exported_types: sets_disj_merge(expTypesList),
      contracts: table_disj_merge(contractsList),
      callbacks: table_disj_merge(callbacksList)
    )
  end

  defp group_fields(list) do
    infoList =
      for r_plt(info: info) <- list do
        info
      end

    typesList =
      for r_plt(types: types) <- list do
        types
      end

    expTypesList =
      for r_plt(exported_types: expTypes) <- list do
        expTypes
      end

    contractsList =
      for r_plt(contracts: contracts) <- list do
        contracts
      end

    callbacksList =
      for r_plt(callbacks: callbacks) <- list do
        callbacks
      end

    {infoList, typesList, expTypesList, contractsList, callbacksList}
  end

  def merge_plts_or_report_conflicts(pltFiles, plts) do
    try do
      merge_disj_plts(plts)
    catch
      {:dialyzer_error, :not_disjoint_plts} ->
        incFiles =
          :lists.append(
            for f <- pltFiles do
              {:ok, fs} = included_files(f)
              fs
            end
          )

        confFiles = find_duplicates(incFiles)

        msg =
          :io_lib.format(
            'Could not merge PLTs since they are not disjoint\nThe following files are included in more than one PLTs:\n~tp\n',
            [confFiles]
          )

        plt_error(msg)
    end
  end

  defp find_duplicates(list) do
    modList =
      for e <- list do
        :filename.basename(e)
      end

    sortedList = :lists.usort(modList)
    :lists.usort(modList -- sortedList)
  end

  def to_file(fileName, plt, modDeps, mD5_OldModDeps) do
    fun = fn ->
      to_file1(fileName, plt, modDeps, mD5_OldModDeps)
    end

    return = subproc(fun)
    delete(plt)

    case return do
      :ok ->
        :ok

      thrown ->
        throw(thrown)
    end
  end

  defp to_file1(
         fileName,
         r_plt(
           info: eTSInfo,
           types: eTSTypes,
           contracts: eTSContracts,
           callbacks: eTSCallbacks,
           exported_types: eTSExpTypes
         ),
         modDeps,
         {mD5, oldModDeps}
       ) do
    newModDeps =
      :dict.merge(
        fn _Key, oldVal, newVal ->
          :ordsets.union(oldVal, newVal)
        end,
        oldModDeps,
        modDeps
      )

    implMd5 = compute_implementation_md5()

    callbacksList =
      for {_M, cbs} <- tab2list(eTSCallbacks),
          cb <- cbs do
        cb
      end

    callbacks = :dict.from_list(callbacksList)
    info = :dict.from_list(tab2list(eTSInfo))
    types = tab2list(eTSTypes)
    contracts = :dict.from_list(tab2list(eTSContracts))

    expTypes =
      :sets.from_list(
        for {e} <- tab2list(eTSExpTypes) do
          e
        end
      )

    fileTypes =
      :dict.from_list(
        for {mod, mTypes} <- types do
          {mod, :dict.from_list(:maps.to_list(mTypes))}
        end
      )

    record =
      r_file_plt(
        version: :EFE_TODO_VSN_MACRO,
        file_md5_list: mD5,
        info: info,
        contracts: contracts,
        callbacks: callbacks,
        types: fileTypes,
        exported_types: expTypes,
        mod_deps: newModDeps,
        implementation_md5: implMd5
      )

    bin = :erlang.term_to_binary(record, [:compressed])

    case :file.write_file(fileName, bin) do
      :ok ->
        :ok

      {:error, reason} ->
        msg = :io_lib.format('Could not write PLT file ~ts: ~w\n', [fileName, reason])
        {:dialyzer_error, msg}
    end
  end

  def check_plt(fileName, removeFiles, addFiles) do
    fun = fn ->
      check_plt1(fileName, removeFiles, addFiles)
    end

    subproc(fun)
  end

  defp check_plt1(fileName, removeFiles, addFiles) do
    case get_record_from_file(fileName) do
      {:ok, r_file_plt(file_md5_list: md5, mod_deps: modDeps) = rec} ->
        case check_version(rec) do
          :ok ->
            case compute_new_md5(md5, removeFiles, addFiles) do
              :ok ->
                :ok

              {:differ, newMd5, diffMd5} ->
                {:differ, newMd5, diffMd5, modDeps}

              {:error, _What} = err ->
                err
            end

          :error ->
            case compute_new_md5(md5, removeFiles, addFiles) do
              :ok ->
                {:old_version, md5}

              {:differ, newMd5, _DiffMd5} ->
                {:old_version, newMd5}

              {:error, _What} = err ->
                err
            end
        end

      error ->
        error
    end
  end

  defp compute_new_md5(md5, [], []) do
    compute_new_md5_1(md5, [], [])
  end

  defp compute_new_md5(md5, removeFiles0, addFiles0) do
    removeFiles = removeFiles0 -- addFiles0
    addFiles = addFiles0 -- removeFiles0
    initDiffList = init_diff_list(removeFiles, addFiles)

    case init_md5_list(md5, removeFiles, addFiles) do
      {:ok, newMd5} ->
        compute_new_md5_1(newMd5, [], initDiffList)

      {:error, _What} = error ->
        error
    end
  end

  defp compute_new_md5_1([{file, md5} = entry | entries], newList, diff) do
    case compute_md5_from_file(file) do
      ^md5 ->
        compute_new_md5_1(entries, [entry | newList], diff)

      newMd5 ->
        modName = beam_file_to_module(file)
        compute_new_md5_1(entries, [{file, newMd5} | newList], [{:differ, modName} | diff])
    end
  end

  defp compute_new_md5_1([], _NewList, []) do
    :ok
  end

  defp compute_new_md5_1([], newList, diff) do
    {:differ, :lists.keysort(1, newList), diff}
  end

  defp compute_implementation_md5() do
    dir = :code.lib_dir(:dialyzer)
    files1 = ['erl_bif_types.beam', 'erl_types.beam']

    files2 =
      for f <- files1 do
        :filename.join([dir, 'ebin', f])
      end

    compute_md5_from_files(files2)
  end

  def compute_md5_from_files(files) do
    :lists.keysort(
      1,
      for f <- files do
        {f, compute_md5_from_file(f)}
      end
    )
  end

  defp compute_md5_from_file(file) do
    case :beam_lib.all_chunks(file) do
      {:ok, _, chunks} ->
        filtered =
          for {iD, chunk} <- chunks, iD !== 'CInf', iD !== 'Docs' do
            [iD, chunk]
          end

        :erlang.md5(:lists.sort(filtered))

      {:error, :beam_lib, {:file_error, _, :enoent}} ->
        msg = :io_lib.format('File not found: ~ts\n', [file])
        throw({:dialyzer_error, msg})

      {:error, :beam_lib, _} ->
        msg = :io_lib.format('Could not compute MD5 for .beam: ~ts\n', [file])
        throw({:dialyzer_error, msg})
    end
  end

  defp init_diff_list(removeFiles, addFiles) do
    removeSet0 =
      :sets.from_list(
        for f <- removeFiles do
          beam_file_to_module(f)
        end
      )

    addSet0 =
      :sets.from_list(
        for f <- addFiles do
          beam_file_to_module(f)
        end
      )

    diffSet = :sets.intersection(addSet0, removeSet0)
    removeSet = :sets.subtract(removeSet0, diffSet)

    for f <- :sets.to_list(removeSet) do
      {:removed, f}
    end
  end

  defp init_md5_list(md5, removeFiles, addFiles) do
    files =
      for f <- removeFiles do
        {:remove, f}
      end ++
        for f <- addFiles do
          {:add, f}
        end

    diffFiles = :lists.keysort(2, files)
    md5Sorted = :lists.keysort(1, md5)
    init_md5_list_1(md5Sorted, diffFiles, [])
  end

  defp init_md5_list_1([{file, _Md5} | md5Left], [{:remove, file} | diffLeft], acc) do
    init_md5_list_1(md5Left, diffLeft, acc)
  end

  defp init_md5_list_1([{file, _Md5} = entry | md5Left], [{:add, file} | diffLeft], acc) do
    init_md5_list_1(md5Left, diffLeft, [entry | acc])
  end

  defp init_md5_list_1(
         [{file1, _Md5} = entry | md5Left] = md5List,
         [{tag, file2} | diffLeft] = diffList,
         acc
       ) do
    case file1 < file2 do
      true ->
        init_md5_list_1(md5Left, diffList, [entry | acc])

      false ->
        true = file1 > file2

        case tag do
          :add ->
            init_md5_list_1(md5List, diffLeft, [{file2, <<>>} | acc])

          :remove ->
            {:error, {:no_file_to_remove, file2}}
        end
    end
  end

  defp init_md5_list_1([], diffList, acc) do
    addFiles =
      for {:add, f} <- diffList do
        {f, <<>>}
      end

    {:ok, :lists.reverse(acc, addFiles)}
  end

  defp init_md5_list_1(md5List, [], acc) do
    {:ok, :lists.reverse(acc, md5List)}
  end

  def delete(
        r_plt(
          info: eTSInfo,
          types: eTSTypes,
          contracts: eTSContracts,
          callbacks: eTSCallbacks,
          exported_types: eTSExpTypes
        )
      ) do
    true = :ets.delete(eTSContracts)
    true = :ets.delete(eTSTypes)
    true = :ets.delete(eTSInfo)
    true = :ets.delete(eTSCallbacks)
    true = :ets.delete(eTSExpTypes)
    :ok
  end

  defp tab2list(tab) do
    :dialyzer_utils.ets_tab2list(tab)
  end

  defp subproc(fun) do
    f = fn ->
      exit(fun.())
    end

    {pid, ref} = :erlang.spawn_monitor(f)

    receive do
      {:DOWN, ^ref, :process, ^pid, return} ->
        return
    end
  end

  def get_specs(r_plt(info: info)) do
    l =
      :lists.sort(
        for {{_, _, _} = mFA, val} <- table_to_list(info) do
          {mFA, val}
        end
      )

    :lists.flatten(create_specs(l, []))
  end

  defp beam_file_to_module(filename) do
    :erlang.list_to_atom(:filename.basename(filename, '.beam'))
  end

  def get_specs(r_plt(info: info), m, f, a)
      when is_atom(m) and
             is_atom(f) do
    mFA = {m, f, a}

    case ets_table_lookup(info, mFA) do
      :none ->
        :none

      {:value, val} ->
        :lists.flatten(create_specs([{mFA, val}], []))
    end
  end

  defp create_specs([{{m, f, _A}, {ret, args}} | left], m) do
    [
      :io_lib.format(
        '-spec ~tw(~ts) -> ~ts\n',
        [f, expand_args(args), :erl_types.t_to_string(ret)]
      )
      | create_specs(left, m)
    ]
  end

  defp create_specs(
         list = [{{m, _F, _A}, {_Ret, _Args}} | _],
         _M
       ) do
    [:io_lib.format('\n\n%% ------- Module: ~w -------\n\n', [m]) | create_specs(list, m)]
  end

  defp create_specs([], _) do
    []
  end

  defp expand_args([]) do
    []
  end

  defp expand_args([argType]) do
    case :erl_types.t_is_any(argType) do
      true ->
        ['_']

      false ->
        [:erl_types.t_to_string(argType)]
    end
  end

  defp expand_args([argType | left]) do
    [
      case :erl_types.t_is_any(argType) do
        true ->
          '_'

        false ->
          :erl_types.t_to_string(argType)
      end ++ ','
      | expand_args(left)
    ]
  end

  defp plt_error(msg) do
    throw({:dialyzer_error, :lists.flatten(msg)})
  end

  defp table_to_list(plt) do
    :ets.tab2list(plt)
  end

  defp table_delete_module(tab, mod) do
    mS =
      :ets.fun2ms(fn
        {{m, _F, _A}, _Val} ->
          m === mod

        {_, _} ->
          false
      end)

    _NumDeleted = :ets.select_delete(tab, mS)
    tab
  end

  defp table_delete_module1(tab, mod) do
    mS =
      :ets.fun2ms(fn {{m, _F, _A}} ->
        m === mod
      end)

    _NumDeleted = :ets.select_delete(tab, mS)
    tab
  end

  defp table_delete_module2(tab, mod) do
    true = :ets.delete(tab, mod)
    tab
  end

  defp ets_table_delete_list(tab, [h | t]) do
    :ets.delete(tab, h)
    ets_table_delete_list(tab, t)
  end

  defp ets_table_delete_list(tab, []) do
    tab
  end

  defp ets_table_lookup(plt, obj) do
    try do
      :ets.lookup_element(plt, obj, 2)
    catch
      _, _ ->
        :none
    else
      val ->
        {:value, val}
    end
  end

  defp table_lookup_module(tab, mod) do
    mS =
      :ets.fun2ms(fn {{m, f, a}, v} when m === mod ->
        {{m, f, a}, v}
      end)

    list =
      for {mFA, v} <- :ets.select(tab, mS) do
        {v1, v2} = v
        {mFA, v1, v2}
      end

    case list === [] do
      true ->
        :none

      false ->
        {:value, list}
    end
  end

  defp table_all_modules(tab) do
    ks = :ets.match(tab, {:"$1", :_}, 100)
    all_mods(ks, :sets.new())
  end

  defp all_mods(:"$end_of_table", s) do
    s
  end

  defp all_mods({listsOfKeys, cont}, s) do
    s1 =
      :lists.foldl(
        fn [{m, _F, _A}], s0 ->
          :sets.add_element(m, s0)
        end,
        s,
        listsOfKeys
      )

    all_mods(:ets.match(cont), s1)
  end

  defp table_merge([h | t]) do
    table_merge(t, h)
  end

  defp table_merge([], acc) do
    acc
  end

  defp table_merge([plt | plts], acc) do
    newAcc = merge_tables(plt, acc)
    table_merge(plts, newAcc)
  end

  defp table_disj_merge([h | t]) do
    table_disj_merge(t, h)
  end

  defp table_disj_merge([], acc) do
    acc
  end

  defp table_disj_merge([plt | plts], acc) do
    case table_is_disjoint(plt, acc) do
      true ->
        newAcc = merge_tables(plt, acc)
        table_disj_merge(plts, newAcc)

      false ->
        throw({:dialyzer_error, :not_disjoint_plts})
    end
  end

  defp sets_merge([h | t]) do
    sets_merge(t, h)
  end

  defp sets_merge([], acc) do
    acc
  end

  defp sets_merge([plt | plts], acc) do
    newAcc = merge_tables(plt, acc)
    sets_merge(plts, newAcc)
  end

  defp sets_disj_merge([h | t]) do
    sets_disj_merge(t, h)
  end

  defp sets_disj_merge([], acc) do
    acc
  end

  defp sets_disj_merge([plt | plts], acc) do
    case table_is_disjoint(plt, acc) do
      true ->
        newAcc = merge_tables(plt, acc)
        sets_disj_merge(plts, newAcc)

      false ->
        throw({:dialyzer_error, :not_disjoint_plts})
    end
  end

  defp table_is_disjoint(t1, t2) do
    tab_is_disj(:ets.first(t1), t1, t2)
  end

  defp tab_is_disj(:"$end_of_table", _T1, _T2) do
    true
  end

  defp tab_is_disj(k1, t1, t2) do
    case :ets.member(t2, k1) do
      false ->
        tab_is_disj(:ets.next(t1, k1), t1, t2)

      true ->
        false
    end
  end

  defp merge_tables(t1, t2) do
    tab_merge(:ets.first(t1), t1, t2)
  end

  defp tab_merge(:"$end_of_table", t1, t2) do
    case :ets.first(t1) do
      :"$end_of_table" ->
        true = :ets.delete(t1)
        t2

      key ->
        tab_merge(key, t1, t2)
    end
  end

  defp tab_merge(k1, t1, t2) do
    vs = :ets.lookup(t1, k1)
    nextK1 = :ets.next(t1, k1)
    true = :ets.delete(t1, k1)
    true = :ets.insert(t2, vs)
    tab_merge(nextK1, t1, t2)
  end

  def pp_non_returning() do
    pltFile = get_default_plt()
    plt = from_file(pltFile)
    list = table_to_list(r_plt(plt, :info))

    unit =
      for {mFA, {ret, args}} <- list,
          :erl_types.t_is_unit(ret) do
        {mFA, :erl_types.t_fun(args, ret)}
      end

    none =
      for {mFA, {ret, args}} <- list,
          :erl_types.t_is_none(ret) do
        {mFA, :erl_types.t_fun(args, ret)}
      end

    :io.format('=========================================\n')
    :io.format('=                Loops                  =\n')
    :io.format('=========================================\n\n')

    :lists.foreach(
      fn {{m, f, _}, type} ->
        :io.format('~w:~tw~ts.\n', [m, f, :dialyzer_utils.format_sig(type)])
      end,
      :lists.sort(unit)
    )

    :io.format('\n')
    :io.format('=========================================\n')
    :io.format('=                Errors                 =\n')
    :io.format('=========================================\n\n')

    :lists.foreach(
      fn {{m, f, _}, type} ->
        :io.format('~w:~w~s.\n', [m, f, :dialyzer_utils.format_sig(type)])
      end,
      :lists.sort(none)
    )

    delete(plt)
  end

  def pp_mod(mod) when is_atom(mod) do
    pltFile = get_default_plt()
    plt = from_file(pltFile)

    case lookup_module(plt, mod) do
      {:value, list} ->
        :lists.foreach(
          fn {{_, f, _}, ret, args} ->
            t = :erl_types.t_fun(args, ret)
            s = :dialyzer_utils.format_sig(t)
            :io.format('-spec ~tw~ts.\n', [f, s])
          end,
          :lists.sort(list)
        )

      :none ->
        :io.format('dialyzer: Found no module named \'~s\' in the PLT\n', [mod])
    end

    delete(plt)
  end
end
