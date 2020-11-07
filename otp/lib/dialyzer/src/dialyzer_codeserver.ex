defmodule :m_dialyzer_codeserver do
  use Bitwise
  require Record

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

  Record.defrecord(:r_codeserver, :codeserver,
    next_core_label: 0,
    code: :undefined,
    exported_types: :undefined,
    records: :undefined,
    contracts: :undefined,
    callbacks: :undefined,
    fun_meta_info: :undefined,
    exports: :undefined,
    temp_exported_types: :undefined,
    temp_records: :undefined,
    temp_contracts: :undefined,
    temp_callbacks: :undefined
  )

  defp ets_dict_find(key, table) do
    try do
      :ets.lookup_element(table, key, 2)
    catch
      _, _ ->
        :error
    else
      val ->
        {:ok, val}
    end
  end

  defp ets_map_store(key, element, table) do
    true = :ets.insert(table, {key, element})
    table
  end

  defp ets_dict_to_dict(table) do
    fold = fn {key, value}, dict ->
      :dict.store(key, value, dict)
    end

    :ets.foldl(fold, :dict.new(), table)
  end

  defp ets_set_is_element(key, table) do
    :ets.lookup(table, key) !== []
  end

  defp ets_set_insert_set(set, table) do
    ets_set_insert_list(:sets.to_list(set), table)
  end

  defp ets_set_insert_list(list, table) do
    true =
      :ets.insert(
        table,
        for e <- list do
          {e}
        end
      )
  end

  defp ets_set_to_set(table) do
    fold = fn {e}, set ->
      :sets.add_element(e, set)
    end

    :ets.foldl(fold, :sets.new(), table)
  end

  def new() do
    codeOptions = [:compressed, :public, {:read_concurrency, true}]
    code = :ets.new(:dialyzer_codeserver_code, codeOptions)
    readOptions = [:compressed, {:read_concurrency, true}]

    [contracts, callbacks, records, exportedTypes] =
      for name <- [
            :dialyzer_codeserver_contracts,
            :dialyzer_codeserver_callbacks,
            :dialyzer_codeserver_records,
            :dialyzer_codeserver_exported_types
          ] do
        :ets.new(name, readOptions)
      end

    tempOptions = [:public, {:write_concurrency, true}]

    [exports, funMetaInfo, tempExportedTypes, tempRecords, tempContracts, tempCallbacks] =
      for name <- [
            :dialyzer_codeserver_exports,
            :dialyzer_codeserver_fun_meta_info,
            :dialyzer_codeserver_temp_exported_types,
            :dialyzer_codeserver_temp_records,
            :dialyzer_codeserver_temp_contracts,
            :dialyzer_codeserver_temp_callbacks
          ] do
        :ets.new(name, tempOptions)
      end

    r_codeserver(
      code: code,
      exports: exports,
      fun_meta_info: funMetaInfo,
      exported_types: exportedTypes,
      records: records,
      contracts: contracts,
      callbacks: callbacks,
      temp_exported_types: tempExportedTypes,
      temp_records: tempRecords,
      temp_contracts: tempContracts,
      temp_callbacks: tempCallbacks
    )
  end

  def delete(cServer) do
    :lists.foreach(
      fn table ->
        true = :ets.delete(table)
      end,
      tables(cServer)
    )
  end

  def insert(mod, modCode, cS) do
    name = :cerl.module_name(modCode)
    exports = :cerl.module_exports(modCode)
    attrs = :cerl.module_attrs(modCode)
    defs = :cerl.module_defs(modCode)
    {files, smallDefs} = compress_file_anno(defs)
    as = :cerl.get_ann(modCode)

    funs =
      for val = {var, fun} <- smallDefs do
        {{mod, :cerl.fname_id(var), :cerl.fname_arity(var)}, val,
         {var, :cerl_trees.get_label(fun)}}
      end

    keys =
      for {key, _Value, _Label} <- funs do
        key
      end

    modEntry = {mod, {name, exports, attrs, keys, as}}
    modFileEntry = {{:mod, mod}, files}

    true =
      :ets.insert(
        r_codeserver(cS, :code),
        [[modEntry, modFileEntry] | funs]
      )

    cS
  end

  def get_temp_exported_types(r_codeserver(temp_exported_types: tempExpTypes)) do
    ets_set_to_set(tempExpTypes)
  end

  def insert_temp_exported_types(set, cS) do
    tempExportedTypes = r_codeserver(cS, :temp_exported_types)
    true = ets_set_insert_set(set, tempExportedTypes)
    cS
  end

  def insert_exports(list, r_codeserver(exports: exports) = cS) do
    true = ets_set_insert_list(list, exports)
    cS
  end

  def insert_fun_meta_info(list, r_codeserver(fun_meta_info: funMetaInfo) = cS) do
    true = :ets.insert(funMetaInfo, list)
    cS
  end

  def is_exported(mFA, r_codeserver(exports: exports)) do
    ets_set_is_element(mFA, exports)
  end

  def get_exported_types(r_codeserver(exported_types: expTypes)) do
    ets_set_to_set(expTypes)
  end

  def extract_exported_types(r_codeserver(exported_types: expTypes) = cS) do
    {r_codeserver(cS, exported_types: :clean), expTypes}
  end

  def get_exports(r_codeserver(exports: exports)) do
    ets_set_to_set(exports)
  end

  def finalize_exported_types(
        set,
        r_codeserver(
          exported_types: exportedTypes,
          temp_exported_types: tempETypes
        ) = cS
      ) do
    true = ets_set_insert_set(set, exportedTypes)
    true = :ets.delete(tempETypes)
    r_codeserver(cS, temp_exported_types: :clean)
  end

  def lookup_mod_code(mod, cS) when is_atom(mod) do
    table__lookup(r_codeserver(cS, :code), mod)
  end

  def lookup_mfa_code({_M, _F, _A} = mFA, cS) do
    table__lookup(r_codeserver(cS, :code), mFA)
  end

  def lookup_mfa_var_label({_M, _F, _A} = mFA, cS) do
    :ets.lookup_element(r_codeserver(cS, :code), mFA, 3)
  end

  def get_next_core_label(r_codeserver(next_core_label: nCL)) do
    nCL
  end

  def set_next_core_label(nCL, cS) do
    r_codeserver(cS, next_core_label: nCL)
  end

  def lookup_mod_records(mod, r_codeserver(records: recDict)) when is_atom(mod) do
    case ets_dict_find(mod, recDict) do
      :error ->
        :maps.new()

      {:ok, map} ->
        map
    end
  end

  def get_records_table(r_codeserver(records: recDict)) do
    recDict
  end

  def extract_records(r_codeserver(records: recDict) = cS) do
    {r_codeserver(cS, records: :clean), recDict}
  end

  def store_temp_records(mod, map, r_codeserver(temp_records: tempRecDict) = cS)
      when is_atom(mod) do
    case :maps.size(map) === 0 do
      true ->
        cS

      false ->
        r_codeserver(cS, temp_records: ets_map_store(mod, map, tempRecDict))
    end
  end

  def get_temp_records_table(r_codeserver(temp_records: tempRecDict)) do
    tempRecDict
  end

  def lookup_temp_mod_records(mod, r_codeserver(temp_records: tempRecDict)) do
    case ets_dict_find(mod, tempRecDict) do
      :error ->
        :maps.new()

      {:ok, map} ->
        map
    end
  end

  def finalize_records(
        r_codeserver(
          temp_records: tmpRecords,
          records: records
        ) = cS
      ) do
    a0 = :erl_anno.new(0)

    aFun = fn _ ->
      a0
    end

    fFun = fn {f, abs, type} ->
      newAbs = :erl_parse.map_anno(aFun, abs)
      {f, newAbs, type}
    end

    arFun = fn {arity, fields} ->
      {arity, :lists.map(fFun, fields)}
    end

    list = :dialyzer_utils.ets_tab2list(tmpRecords)
    true = :ets.delete(tmpRecords)

    fun = fn {mod, map} ->
      mFun = fn
        {:record, _}, {fileLine, arityFields} ->
          {fileLine, :lists.map(arFun, arityFields)}

        _, {{m, fileLine, abs, args}, type} ->
          {{m, fileLine, :erl_parse.map_anno(aFun, abs), args}, type}
      end

      {mod, :maps.map(mFun, map)}
    end

    newList = :lists.map(fun, list)
    true = :ets.insert(records, newList)
    r_codeserver(cS, temp_records: :clean)
  end

  def lookup_mod_contracts(mod, r_codeserver(contracts: contDict)) when is_atom(mod) do
    case ets_dict_find(mod, contDict) do
      :error ->
        :maps.new()

      {:ok, keys} ->
        :maps.from_list(
          for key <- keys do
            get_file_contract(key, contDict)
          end
        )
    end
  end

  defp get_file_contract(key, contDict) do
    {key, :ets.lookup_element(contDict, key, 2)}
  end

  def lookup_mfa_contract(mFA, r_codeserver(contracts: contDict)) do
    ets_dict_find(mFA, contDict)
  end

  def lookup_meta_info(morMFA, r_codeserver(fun_meta_info: funMetaInfo)) do
    ets_dict_find(morMFA, funMetaInfo)
  end

  def get_contracts(r_codeserver(contracts: contDict)) do
    :dict.filter(
      fn
        {_M, _F, _A}, _ ->
          true

        _, _ ->
          false
      end,
      ets_dict_to_dict(contDict)
    )
  end

  def get_callbacks(r_codeserver(callbacks: callbDict)) do
    :ets.tab2list(callbDict)
  end

  def store_temp_contracts(
        mod,
        specMap,
        callbackMap,
        r_codeserver(temp_contracts: cn, temp_callbacks: cb) = cS
      )
      when is_atom(mod) do
    cS1 = r_codeserver(cS, temp_contracts: ets_map_store(mod, specMap, cn))
    r_codeserver(cS1, temp_callbacks: ets_map_store(mod, callbackMap, cb))
  end

  def all_temp_modules(r_codeserver(temp_contracts: tempContTable)) do
    :ets.select(tempContTable, [{{:"$1", :"$2"}, [], [:"$1"]}])
  end

  def store_contracts(mod, specMap, callbackMap, cS) do
    r_codeserver(contracts: specDict, callbacks: callbackDict) = cS
    keys = :maps.keys(specMap)
    true = :ets.insert(specDict, :maps.to_list(specMap))
    true = :ets.insert(specDict, {mod, keys})

    true =
      :ets.insert(
        callbackDict,
        :maps.to_list(callbackMap)
      )

    cS
  end

  def get_temp_contracts(
        mod,
        r_codeserver(
          temp_contracts: tempContDict,
          temp_callbacks: tempCallDict
        )
      ) do
    [{^mod, contracts}] = :ets.lookup(tempContDict, mod)
    true = :ets.delete(tempContDict, mod)
    [{^mod, callbacks}] = :ets.lookup(tempCallDict, mod)
    true = :ets.delete(tempCallDict, mod)
    {contracts, callbacks}
  end

  def give_away(cServer, pid) do
    :lists.foreach(
      fn table ->
        true = :ets.give_away(table, pid, :any)
      end,
      tables(cServer)
    )
  end

  defp tables(
         r_codeserver(
           code: code,
           fun_meta_info: funMetaInfo,
           exports: exports,
           temp_exported_types: tempExpTypes,
           temp_records: tempRecords,
           temp_contracts: tempContracts,
           temp_callbacks: tempCallbacks,
           exported_types: exportedTypes,
           records: records,
           contracts: contracts,
           callbacks: callbacks
         )
       ) do
    for table <- [
          code,
          funMetaInfo,
          exports,
          tempExpTypes,
          tempRecords,
          tempContracts,
          tempCallbacks,
          exportedTypes,
          records,
          contracts,
          callbacks
        ],
        table !== :clean do
      table
    end
  end

  def finalize_contracts(
        r_codeserver(
          temp_contracts: tempContDict,
          temp_callbacks: tempCallDict
        ) = cS
      ) do
    true = :ets.delete(tempContDict)
    true = :ets.delete(tempCallDict)
    r_codeserver(cS, temp_contracts: :clean, temp_callbacks: :clean)
  end

  def translate_fake_file(r_codeserver(code: code), module, fakeFile) do
    files = :ets.lookup_element(code, {:mod, module}, 2)
    {^fakeFile, file} = :lists.keyfind(fakeFile, 1, files)
    file
  end

  defp table__lookup(tablePid, m) when is_atom(m) do
    {name, exports, attrs, keys, as} = :ets.lookup_element(tablePid, m, 2)

    defs =
      for key <- keys do
        table__lookup(tablePid, key)
      end

    :cerl.ann_c_module(as, name, exports, attrs, defs)
  end

  defp table__lookup(tablePid, mFA) do
    :ets.lookup_element(tablePid, mFA, 2)
  end

  defp compress_file_anno(term) do
    {files, smallTerm} = compress_file_anno(term, [])

    {for {file, {:file, fakeFile}} <- files do
       {fakeFile, file}
     end, smallTerm}
  end

  defp compress_file_anno({:file, f}, fs) when is_list(f) do
    case :lists.keyfind(f, 1, fs) do
      false ->
        i = :erlang.integer_to_list(length(fs))
        fileI = {:file, i}
        nFs = [{f, fileI} | fs]
        {nFs, fileI}

      {^f, fileI} ->
        {fs, fileI}
    end
  end

  defp compress_file_anno(t, fs) when is_tuple(t) do
    {nFs, nL} =
      compress_file_anno(
        :erlang.tuple_to_list(t),
        fs
      )

    {nFs, :erlang.list_to_tuple(nL)}
  end

  defp compress_file_anno([e | l], fs) do
    {fs1, nE} = compress_file_anno(e, fs)
    {nFs, nL} = compress_file_anno(l, fs1)
    {nFs, [nE | nL]}
  end

  defp compress_file_anno(t, fs) do
    {fs, t}
  end
end
