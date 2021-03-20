defmodule :m_asn1ct do
  use Bitwise
  import :asn1ct_gen_ber_bin_v2, only: [decode_class: 1, encode_tag_val: 3]
  require Record

  Record.defrecord(:r_module, :module,
    pos: :undefined,
    name: :undefined,
    defid: :undefined,
    tagdefault: :EXPLICIT,
    exports: {:exports, []},
    imports: {:imports, []},
    extensiondefault: :empty,
    typeorval: :undefined
  )

  Record.defrecord(:r_ExtensionAdditionGroup, :ExtensionAdditionGroup, number: :undefined)

  Record.defrecord(:r_SEQUENCE, :SEQUENCE,
    pname: false,
    tablecinf: false,
    extaddgroup: :undefined,
    components: []
  )

  Record.defrecord(:r_SET, :SET, pname: false, sorted: false, tablecinf: false, components: [])

  Record.defrecord(:r_ComponentType, :ComponentType,
    pos: :undefined,
    name: :undefined,
    typespec: :undefined,
    prop: :undefined,
    tags: :undefined,
    textual_order: :undefined
  )

  Record.defrecord(:r_ObjectClassFieldType, :ObjectClassFieldType,
    classname: :undefined,
    class: :undefined,
    fieldname: :undefined,
    type: :undefined
  )

  Record.defrecord(:r_typedef, :typedef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    typespec: :undefined
  )

  Record.defrecord(:r_classdef, :classdef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    module: :undefined,
    typespec: :undefined
  )

  Record.defrecord(:r_valuedef, :valuedef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    type: :undefined,
    value: :undefined,
    module: :undefined
  )

  Record.defrecord(:r_ptypedef, :ptypedef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    args: :undefined,
    typespec: :undefined
  )

  Record.defrecord(:r_pvaluedef, :pvaluedef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    args: :undefined,
    type: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_pvaluesetdef, :pvaluesetdef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    args: :undefined,
    type: :undefined,
    valueset: :undefined
  )

  Record.defrecord(:r_pobjectdef, :pobjectdef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    args: :undefined,
    class: :undefined,
    def: :undefined
  )

  Record.defrecord(:r_pobjectsetdef, :pobjectsetdef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    args: :undefined,
    class: :undefined,
    def: :undefined
  )

  Record.defrecord(:r_Constraint, :Constraint,
    SingleValue: :no,
    SizeConstraint: :no,
    ValueRange: :no,
    PermittedAlphabet: :no,
    ContainedSubtype: :no,
    TypeConstraint: :no,
    InnerSubtyping: :no,
    e: :no,
    Other: :no
  )

  Record.defrecord(:r_simpletableattributes, :simpletableattributes,
    objectsetname: :undefined,
    c_name: :undefined,
    c_index: :undefined,
    usedclassfield: :undefined,
    uniqueclassfield: :undefined,
    valueindex: :undefined
  )

  Record.defrecord(:r_type, :type,
    tag: [],
    def: :undefined,
    constraint: [],
    tablecinf: [],
    inlined: :no
  )

  Record.defrecord(:r_objectclass, :objectclass,
    fields: [],
    syntax: :undefined
  )

  Record.defrecord(:r_Object, :Object, classname: :undefined, gen: true, def: :undefined)

  Record.defrecord(:r_ObjectSet, :ObjectSet,
    class: :undefined,
    gen: true,
    uniquefname: :undefined,
    set: :undefined
  )

  Record.defrecord(:r_tag, :tag, class: :undefined, number: :undefined, type: :undefined, form: 32)

  Record.defrecord(:r_cmap, :cmap,
    single_value: :no,
    contained_subtype: :no,
    value_range: :no,
    size: :no,
    permitted_alphabet: :no,
    type_constraint: :no,
    inner_subtyping: :no
  )

  Record.defrecord(:r_EXTENSIONMARK, :EXTENSIONMARK,
    pos: :undefined,
    val: :undefined
  )

  Record.defrecord(:r_SymbolsFromModule, :SymbolsFromModule,
    symbols: :undefined,
    module: :undefined,
    objid: :undefined
  )

  Record.defrecord(:r_Externaltypereference, :Externaltypereference,
    pos: :undefined,
    module: :undefined,
    type: :undefined
  )

  Record.defrecord(:r_Externalvaluereference, :Externalvaluereference,
    pos: :undefined,
    module: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_seqtag, :seqtag, pos: :undefined, module: :undefined, val: :undefined)

  Record.defrecord(:r_state, :state,
    module: :undefined,
    mname: :undefined,
    tname: :undefined,
    erule: :undefined,
    parameters: [],
    inputmodules: [],
    abscomppath: [],
    recordtopname: [],
    options: :undefined,
    sourcedir: :undefined,
    error_context: :undefined
  )

  Record.defrecord(:r_gen, :gen,
    erule: :ber,
    der: false,
    jer: false,
    aligned: false,
    rec_prefix: '',
    macro_prefix: '',
    pack: :record,
    options: []
  )

  Record.defrecord(:r_abst, :abst,
    name: :undefined,
    types: :undefined,
    values: :undefined,
    ptypes: :undefined,
    classes: :undefined,
    objects: :undefined,
    objsets: :undefined
  )

  Record.defrecord(:r_gen_state, :gen_state,
    active: false,
    prefix: :undefined,
    inc_tag_pattern: :undefined,
    tag_pattern: :undefined,
    inc_type_pattern: :undefined,
    type_pattern: :undefined,
    func_name: :undefined,
    namelist: :undefined,
    tobe_refed_funcs: [],
    gen_refed_funcs: [],
    generated_functions: [],
    suffix_index: 1,
    current_suffix_index: :undefined
  )

  Record.defrecord(:r_options, :options,
    includes: [],
    outdir: '.',
    output_type: :undefined,
    defines: [],
    warning: 1,
    verbose: false,
    optimize: 999,
    specific: [],
    outfile: '',
    cwd: :undefined
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

  def compile(file) do
    compile(file, [])
  end

  def compile(file, options0) when is_list(options0) do
    try do
      translate_options(options0)
    catch
      error ->
        error
    else
      options1 ->
        options2 = includes(file, options1)
        includes = strip_includes(options2)

        in_process(fn ->
          compile_proc(file, includes, options2)
        end)
    end
  end

  Record.defrecord(:r_st, :st,
    file: [],
    files: [],
    inputmodules: [],
    code: :undefined,
    opts: [],
    outfile: :undefined,
    dbfile: :undefined,
    includes: [],
    erule: :undefined,
    error: :none,
    run: :undefined
  )

  defp compile_proc(file, includes, options) do
    erule = get_rule(options)
    st = r_st(opts: options, includes: includes, erule: erule)

    case input_file_type(file, includes) do
      {:single_file, suffixedFile} ->
        compile1(suffixedFile, st)

      {:multiple_files_file, setBase, fileName} ->
        case get_file_list(fileName, includes) do
          fileList when is_list(fileList) ->
            compile_set(setBase, fileList, st)

          err ->
            err
        end

      err = {:input_file_error, _Reason} ->
        {:error, err}
    end
  end

  defp set_passes() do
    [
      {:pass, :scan_parse, &set_scan_parse_pass/1},
      {:pass, :merge, &merge_pass/1}
      | common_passes()
    ]
  end

  defp single_passes() do
    [
      {:pass, :scan, &scan_pass/1},
      {:pass, :parse, &parse_pass/1}
      | common_passes()
    ]
  end

  defp parse_and_save_passes() do
    [{:pass, :scan, &scan_pass/1}, {:pass, :parse, &parse_pass/1}, {:pass, :save, &save_pass/1}]
  end

  defp common_passes() do
    [
      {:iff, :parse, {:pass, :parse_listing, &parse_listing/1}},
      {:pass, :check, &check_pass/1},
      {:iff, :abs, {:pass, :abs_listing, &abs_listing/1}},
      {:pass, :generate, &generate_pass/1},
      {:unless, :noobj, {:pass, :compile, &compile_pass/1}}
    ]
  end

  defp scan_pass(r_st(file: file) = st) do
    case :asn1ct_tok.file(file) do
      {:error, reason} ->
        {:error, r_st(st, error: reason)}

      tokens when is_list(tokens) ->
        {:ok, r_st(st, code: tokens)}
    end
  end

  defp set_scan_parse_pass(r_st(files: files) = st) do
    try do
      l = set_scan_parse_pass_1(files, st)
      {:ok, r_st(st, code: l)}
    catch
      error ->
        {:error, r_st(st, error: error)}
    end
  end

  defp set_scan_parse_pass_1([f | fs], r_st(file: file) = st) do
    case :asn1ct_tok.file(f) do
      {:error, error} ->
        throw(error)

      tokens when is_list(tokens) ->
        case :asn1ct_parser2.parse(file, tokens) do
          {:ok, m} ->
            [m | set_scan_parse_pass_1(fs, st)]

          {:error, errors} ->
            throw(errors)
        end
    end
  end

  defp set_scan_parse_pass_1([], _) do
    []
  end

  defp parse_pass(r_st(file: file, code: tokens) = st) do
    case :asn1ct_parser2.parse(file, tokens) do
      {:ok, m} ->
        {:ok, r_st(st, code: m)}

      {:error, errors} ->
        {:error, r_st(st, error: errors)}
    end
  end

  defp merge_pass(r_st(file: base, code: code) = st) do
    m = merge_modules(code, base)
    {:ok, r_st(st, code: m)}
  end

  defp check_pass(
         r_st(
           code: m,
           file: file,
           includes: includes,
           erule: erule,
           dbfile: dbFile,
           opts: opts,
           inputmodules: inputModules
         ) = st
       ) do
    start(includes)

    case :asn1ct_check.storeindb(
           r_state(
             erule: erule,
             options: opts
           ),
           m
         ) do
      :ok ->
        module = :asn1_db.dbget(r_module(m, :name), :MODULE)

        state =
          r_state(
            mname: r_module(module, :name),
            module: r_module(module, typeorval: []),
            erule: erule,
            inputmodules: inputModules,
            options: opts,
            sourcedir: :filename.dirname(file)
          )

        case :asn1ct_check.check(
               state,
               r_module(module, :typeorval)
             ) do
          {:error, reason} ->
            {:error, r_st(st, error: reason)}

          {:ok, newTypeOrVal, genTypeOrVal} ->
            newM = r_module(module, typeorval: newTypeOrVal)
            :asn1_db.dbput(r_module(newM, :name), :MODULE, newM)
            :asn1_db.dbsave(dbFile, r_module(m, :name))
            verbose('--~p--~n', [{:generated, dbFile}], opts)
            {:ok, r_st(st, code: {m, genTypeOrVal})}
        end

      {:error, reason} ->
        {:error, r_st(st, error: reason)}
    end
  end

  defp save_pass(r_st(code: m, erule: erule, opts: opts) = st) do
    :ok =
      :asn1ct_check.storeindb(
        r_state(
          erule: erule,
          options: opts
        ),
        m
      )

    {:ok, st}
  end

  defp parse_listing(r_st(code: code, outfile: outFile0) = st) do
    outFile = outFile0 ++ '.parse'

    case :file.write_file(
           outFile,
           :io_lib.format('~p\n', [code])
         ) do
      :ok ->
        :done

      {:error, reason} ->
        error = {:write_error, outFile, reason}
        {:error, r_st(st, error: [{:structured_error, {outFile0, :none}, :asn1ct, error}])}
    end
  end

  defp abs_listing(r_st(code: {m, _}, outfile: outFile)) do
    pretty2(r_module(m, :name), outFile ++ '.abs')
    :done
  end

  defp generate_pass(r_st(code: code, outfile: outFile, erule: erule, opts: opts) = st0) do
    st = r_st(st0, code: :undefined)
    generate(code, outFile, erule, opts)
    {:ok, st}
  end

  defp compile_pass(r_st(outfile: outFile, opts: opts0) = st) do
    :asn1_db.dbstop()
    :asn1ct_table.delete([:renamed_defs, :original_imports, :automatic_tags])
    opts = remove_asn_flags(opts0)

    case :c.c(outFile, opts) do
      {:ok, _Module} ->
        {:ok, st}

      _ ->
        {:error, st}
    end
  end

  defp run_passes(passes, r_st(opts: opts) = st) do
    run =
      case :lists.member(:time, opts) do
        false ->
          fn _, pass, s ->
            pass.(s)
          end

        true ->
          &run_tc/3
      end

    run_passes_1(passes, r_st(st, run: run))
  end

  defp run_tc(name, fun, st) do
    before0 = :erlang.statistics(:runtime)

    val =
      try do
        fun.(st)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    after0 = :erlang.statistics(:runtime)
    {before_c, _} = before0
    {after_c, _} = after0
    :io.format('~-31s: ~10.2f s\n', [name, (after_c - before_c) / 1000])
    val
  end

  defp run_passes_1(
         [{:unless, opt, pass} | passes],
         r_st(opts: opts) = st
       ) do
    case :proplists.get_bool(opt, opts) do
      false ->
        run_passes_1([pass | passes], st)

      true ->
        run_passes_1(passes, st)
    end
  end

  defp run_passes_1(
         [{:iff, opt, pass} | passes],
         r_st(opts: opts) = st
       ) do
    case :proplists.get_bool(opt, opts) do
      true ->
        run_passes_1([pass | passes], st)

      false ->
        run_passes_1(passes, st)
    end
  end

  defp run_passes_1(
         [{:pass, name, pass} | passes],
         r_st(run: run) = st0
       )
       when is_function(pass, 1) do
    try do
      run.(name, pass, st0)
    catch
      class, error ->
        :io.format('Internal error: ~p:~p\n~p\n', [class, error, __STACKTRACE__])
        {:error, {:internal_error, {class, error}}}
    else
      {:ok, st} ->
        run_passes_1(passes, st)

      {:error, r_st(error: errors)} ->
        {structured, allErrors} = clean_errors(errors)
        print_structured_errors(structured)
        {:error, allErrors}

      :done ->
        :ok
    end
  end

  defp run_passes_1([], _St) do
    :ok
  end

  defp clean_errors(errors) when is_list(errors) do
    f = fn
      {:structured_error, _, _, _} ->
        true

      _ ->
        false
    end

    {structured0, adHoc} = :lists.partition(f, errors)
    structured = :lists.sort(structured0)
    {structured, structured ++ adHoc}
  end

  defp clean_errors(adHoc) do
    {[], adHoc}
  end

  defp print_structured_errors([_ | _] = errors) do
    _ =
      for {:structured_error, {f, l}, m, e} <- errors do
        :io.format('~ts:~w: ~ts\n', [f, l, m.format_error(e)])
      end

    :ok
  end

  defp print_structured_errors(_) do
    :ok
  end

  defp compile1(file, r_st(opts: opts) = st0) do
    compiler_verbose(file, opts)
    passes = single_passes()
    base = :filename.rootname(:filename.basename(file))
    outFile = outfile(base, '', opts)
    dbFile = outfile(base, 'asn1db', opts)
    st1 = r_st(st0, file: file, outfile: outFile, dbfile: dbFile)
    run_passes(passes, st1)
  end

  defp compile_set(setBase, files, r_st(opts: opts) = st0) do
    compiler_verbose(files, opts)
    outFile = outfile(setBase, '', opts)
    dbFile = outfile(setBase, 'asn1db', opts)

    inputModules =
      for f0 <- files do
        f1 = :filename.basename(f0)
        f = :filename.rootname(f1)
        :erlang.list_to_atom(f)
      end

    st =
      r_st(st0,
        file: setBase,
        files: files,
        outfile: outFile,
        dbfile: dbFile,
        inputmodules: inputModules
      )

    passes = set_passes()
    run_passes(passes, st)
  end

  defp compiler_verbose(what, opts) do
    verbose('Erlang ASN.1 compiler ~s\n', ['0.0.1'], opts)
    verbose('Compiling: ~p\n', [what], opts)
    verbose('Options: ~p\n', [opts], opts)
  end

  defp merge_modules(moduleList, commonName) do
    newModuleList = remove_name_collisions(moduleList)

    case :asn1ct_table.size(:renamed_defs) do
      0 ->
        :asn1ct_table.delete(:renamed_defs)

      _ ->
        :ok
    end

    save_imports(newModuleList)

    typeOrVal =
      :lists.append(
        :lists.map(
          fn x ->
            r_module(x, :typeorval)
          end,
          newModuleList
        )
      )

    inputMNameList =
      :lists.map(
        fn x ->
          r_module(x, :name)
        end,
        newModuleList
      )

    cExports = common_exports(newModuleList)

    importsModuleNameList =
      :lists.map(
        fn x ->
          {r_module(x, :imports), r_module(x, :name)}
        end,
        newModuleList
      )

    cImports =
      common_imports(
        importsModuleNameList,
        inputMNameList
      )

    tagDefault = check_tagdefault(newModuleList)

    r_module(
      name: commonName,
      tagdefault: tagDefault,
      exports: cExports,
      imports: cImports,
      typeorval: typeOrVal
    )
  end

  defp remove_name_collisions(modules) do
    :asn1ct_table.new(:renamed_defs)
    :lists.foreach(&exit_if_nameduplicate/1, modules)
    remove_name_collisions2(modules, [])
  end

  defp remove_name_collisions2([m | ms], acc) do
    typeOrVal = r_module(m, :typeorval)
    mName = r_module(m, :name)
    {newM, newMs} = remove_name_collisions2(mName, typeOrVal, ms, [])

    remove_name_collisions2(
      newMs,
      [r_module(m, typeorval: newM) | acc]
    )
  end

  defp remove_name_collisions2([], acc) do
    finished_warn_prints()
    acc
  end

  defp remove_name_collisions2(modName, [t | ts], ms, acc) do
    name = get_name_of_def(t)

    case discover_dupl_in_mods(name, t, ms, [], 0) do
      {_, 0} ->
        remove_name_collisions2(modName, ts, ms, [t | acc])

      {newMs, 1} ->
        newT = set_name_of_def(modName, name, t)
        warn_renamed_def(modName, get_name_of_def(newT), name)

        :asn1ct_table.insert(
          :renamed_defs,
          {get_name_of_def(newT), name, modName}
        )

        remove_name_collisions2(modName, ts, newMs, [newT | acc])

      {newMs, 2} ->
        warn_kept_def(modName, name)
        remove_name_collisions2(modName, ts, newMs, [t | acc])

      {newMs, 3} ->
        warn_kept_def(modName, name)
        remove_name_collisions2(modName, ts, newMs, [t | acc])
    end
  end

  defp remove_name_collisions2(_, [], ms, acc) do
    {acc, ms}
  end

  defp discover_dupl_in_mods(
         name,
         def__,
         [m = r_module(name: n, typeorval: torV) | ms],
         acc,
         anyRenamed
       ) do
    fun = fn t, renamedOrDupl ->
      case {get_name_of_def(t), compare_defs(def__, t)} do
        {^name, :not_equal} ->
          newT = set_name_of_def(n, name, t)
          warn_renamed_def(n, get_name_of_def(newT), name)

          :asn1ct_table.insert(
            :renamed_defs,
            {get_name_of_def(newT), name, n}
          )

          {newT, 1 ||| renamedOrDupl}

        {^name, :equal} ->
          warn_deleted_def(n, name)
          {[], 2 ||| renamedOrDupl}

        _ ->
          {t, renamedOrDupl}
      end
    end

    {newTorV, newAnyRenamed} = :lists.mapfoldl(fun, anyRenamed, torV)

    discover_dupl_in_mods(
      name,
      def__,
      ms,
      [r_module(m, typeorval: :lists.flatten(newTorV)) | acc],
      newAnyRenamed
    )
  end

  defp discover_dupl_in_mods(_, _, [], acc, anyRenamed) do
    {acc, anyRenamed}
  end

  defp warn_renamed_def(modName, newName, oldName) do
    maybe_first_warn_print()

    :io.format(
      'NOTICE: The ASN.1 definition in module ~p with name ~p has been renamed in generated module. New name is ~p.~n',
      [modName, oldName, newName]
    )
  end

  defp warn_deleted_def(modName, defName) do
    maybe_first_warn_print()

    :io.format(
      'NOTICE: The ASN.1 definition in module ~p with name ~p has been deleted in generated module.~n',
      [modName, defName]
    )
  end

  defp warn_kept_def(modName, defName) do
    maybe_first_warn_print()

    :io.format(
      'NOTICE: The ASN.1 definition in module ~p with name ~p has kept its name due to equal definition as duplicate.~n',
      [modName, defName]
    )
  end

  defp maybe_first_warn_print() do
    case :erlang.get(:warn_duplicate_defs) do
      :undefined ->
        :erlang.put(:warn_duplicate_defs, true)

        :io.format(
          '~nDue to multiple occurrences of a definition name in multi-file compiled files:~n'
        )

      _ ->
        :ok
    end
  end

  defp finished_warn_prints() do
    :erlang.put(:warn_duplicate_defs, :undefined)
  end

  defp exit_if_nameduplicate(r_module(typeorval: torV)) do
    exit_if_nameduplicate(torV)
  end

  defp exit_if_nameduplicate([]) do
    :ok
  end

  defp exit_if_nameduplicate([def__ | rest]) do
    name = get_name_of_def(def__)
    exit_if_nameduplicate2(name, rest)
    exit_if_nameduplicate(rest)
  end

  defp exit_if_nameduplicate2(name, rest) do
    pred = fn def__ ->
      case get_name_of_def(def__) do
        ^name ->
          true

        _ ->
          false
      end
    end

    case :lists.any(pred, rest) do
      true ->
        throw({:error, {'more than one definition with same name', name}})

      _ ->
        :ok
    end
  end

  defp compare_defs(d1, d2) do
    compare_defs2(unset_pos_mod(d1), unset_pos_mod(d2))
  end

  defp compare_defs2(d, d) do
    :equal
  end

  defp compare_defs2(_, _) do
    :not_equal
  end

  def unset_pos_mod(def__) when elem(def__, 0) === :typedef do
    r_typedef(def__, pos: :undefined)
  end

  def unset_pos_mod(def__) when elem(def__, 0) === :classdef do
    r_classdef(def__, pos: :undefined)
  end

  def unset_pos_mod(def__) when elem(def__, 0) === :valuedef do
    r_valuedef(def__, pos: :undefined, module: :undefined)
  end

  def unset_pos_mod(def__) when elem(def__, 0) === :ptypedef do
    r_ptypedef(def__, pos: :undefined)
  end

  def unset_pos_mod(def__) when elem(def__, 0) === :pvaluedef do
    r_pvaluedef(def__, pos: :undefined)
  end

  def unset_pos_mod(def__) when elem(def__, 0) === :pvaluesetdef do
    r_pvaluesetdef(def__, pos: :undefined)
  end

  def unset_pos_mod(def__) when elem(def__, 0) === :pobjectdef do
    r_pobjectdef(def__, pos: :undefined)
  end

  def unset_pos_mod(def__) when elem(def__, 0) === :pobjectsetdef do
    r_pobjectsetdef(def__, pos: :undefined)
  end

  def unset_pos_mod(r_ComponentType() = def__) do
    r_ComponentType(def__, pos: :undefined)
  end

  def unset_pos_mod(def__) do
    def__
  end

  def get_pos_of_def(r_typedef(pos: pos)) do
    pos
  end

  def get_pos_of_def(r_classdef(pos: pos)) do
    pos
  end

  def get_pos_of_def(r_valuedef(pos: pos)) do
    pos
  end

  def get_pos_of_def(r_ptypedef(pos: pos)) do
    pos
  end

  def get_pos_of_def(r_pvaluedef(pos: pos)) do
    pos
  end

  def get_pos_of_def(r_pvaluesetdef(pos: pos)) do
    pos
  end

  def get_pos_of_def(r_pobjectdef(pos: pos)) do
    pos
  end

  def get_pos_of_def(r_pobjectsetdef(pos: pos)) do
    pos
  end

  def get_pos_of_def(r_Externaltypereference(pos: pos)) do
    pos
  end

  def get_pos_of_def(r_Externalvaluereference(pos: pos)) do
    pos
  end

  def get_pos_of_def(_) do
    :undefined
  end

  def get_name_of_def(r_typedef(name: name)) do
    name
  end

  def get_name_of_def(r_classdef(name: name)) do
    name
  end

  def get_name_of_def(r_valuedef(name: name)) do
    name
  end

  def get_name_of_def(r_ptypedef(name: name)) do
    name
  end

  def get_name_of_def(r_pvaluedef(name: name)) do
    name
  end

  def get_name_of_def(r_pvaluesetdef(name: name)) do
    name
  end

  def get_name_of_def(r_pobjectdef(name: name)) do
    name
  end

  def get_name_of_def(r_pobjectsetdef(name: name)) do
    name
  end

  def get_name_of_def(_) do
    :undefined
  end

  defp set_name_of_def(modName, name, oldDef) do
    newName = :erlang.list_to_atom(:lists.concat([name, modName]))

    case oldDef do
      r_typedef() ->
        r_typedef(oldDef, name: newName)

      r_classdef() ->
        r_classdef(oldDef, name: newName)

      r_valuedef() ->
        r_valuedef(oldDef, name: newName)

      r_ptypedef() ->
        r_ptypedef(oldDef, name: newName)

      r_pvaluedef() ->
        r_pvaluedef(oldDef, name: newName)

      r_pvaluesetdef() ->
        r_pvaluesetdef(oldDef, name: newName)

      r_pobjectdef() ->
        r_pobjectdef(oldDef, name: newName)

      r_pobjectsetdef() ->
        r_pobjectsetdef(oldDef, name: newName)
    end
  end

  defp save_imports(moduleList) do
    fun = fn m ->
      case r_module(m, :imports) do
        {_, []} ->
          []

        {_, i} ->
          {r_module(m, :name), i}
      end
    end

    importsList = :lists.map(fun, moduleList)

    case :lists.flatten(importsList) do
      [] ->
        :ok

      importsList2 ->
        :asn1ct_table.new(:original_imports)

        :lists.foreach(
          fn x ->
            :asn1ct_table.insert(:original_imports, x)
          end,
          importsList2
        )
    end
  end

  defp common_exports(moduleList) do
    case :lists.filter(
           fn x ->
             :erlang.element(2, r_module(x, :exports)) != :all
           end,
           moduleList
         ) do
      [] ->
        {:exports, :all}

      modsWithExpList ->
        cExports1 =
          :lists.append(
            :lists.map(
              fn x ->
                :erlang.element(
                  2,
                  r_module(x, :exports)
                )
              end,
              modsWithExpList
            )
          )

        cExports2 =
          export_all(
            :lists.subtract(
              moduleList,
              modsWithExpList
            )
          )

        {:exports, cExports1 ++ cExports2}
    end
  end

  defp export_all([]) do
    []
  end

  defp export_all(moduleList) do
    expList =
      :lists.map(
        fn m ->
          torVL = r_module(m, :typeorval)
          mName = r_module(m, :name)

          :lists.map(
            fn def__ ->
              case def__ do
                t when elem(t, 0) === :typedef ->
                  r_Externaltypereference(pos: 0, module: mName, type: r_typedef(t, :name))

                v when elem(v, 0) === :valuedef ->
                  r_Externalvaluereference(pos: 0, module: mName, value: r_valuedef(v, :name))

                c when elem(c, 0) === :classdef ->
                  r_Externaltypereference(pos: 0, module: mName, type: r_classdef(c, :name))

                p when elem(p, 0) === :ptypedef ->
                  r_Externaltypereference(pos: 0, module: mName, type: r_ptypedef(p, :name))

                pV
                when elem(pV, 0) === :pvaluesetdef ->
                  r_Externaltypereference(pos: 0, module: mName, type: r_pvaluesetdef(pV, :name))

                pO
                when elem(pO, 0) === :pobjectdef ->
                  r_Externalvaluereference(pos: 0, module: mName, value: r_pobjectdef(pO, :name))
              end
            end,
            torVL
          )
        end,
        moduleList
      )

    :lists.append(expList)
  end

  defp common_imports(iList, inputMNameL) do
    setExternalImportsList = remove_in_set_imports(iList, inputMNameL, [])
    {:imports, remove_import_doubles(setExternalImportsList)}
  end

  defp check_tagdefault(modList) do
    case have_same_tagdefault(modList) do
      {true, tagDefault} ->
        tagDefault

      {false, tagDefault} ->
        :asn1ct_table.new(:automatic_tags)
        save_automatic_tagged_types(modList)
        tagDefault
    end
  end

  defp have_same_tagdefault([r_module(tagdefault: t) | ms]) do
    have_same_tagdefault(ms, {true, t})
  end

  defp have_same_tagdefault([], tagDefault) do
    tagDefault
  end

  defp have_same_tagdefault([r_module(tagdefault: t) | ms], tDefault = {_, t}) do
    have_same_tagdefault(ms, tDefault)
  end

  defp have_same_tagdefault([r_module(tagdefault: t1) | ms], {_, t2}) do
    have_same_tagdefault(ms, {false, rank_tagdef([t1, t2])})
  end

  defp rank_tagdef(l) do
    case :lists.member(:EXPLICIT, l) do
      true ->
        :EXPLICIT

      _ ->
        :IMPLICIT
    end
  end

  defp save_automatic_tagged_types([]) do
    :done
  end

  defp save_automatic_tagged_types([
         r_module(tagdefault: :AUTOMATIC, typeorval: torV)
         | ms
       ]) do
    fun = fn t ->
      :asn1ct_table.insert(
        :automatic_tags,
        {get_name_of_def(t)}
      )
    end

    :lists.foreach(fun, torV)
    save_automatic_tagged_types(ms)
  end

  defp save_automatic_tagged_types([_M | ms]) do
    save_automatic_tagged_types(ms)
  end

  defp remove_in_set_imports([{{:imports, impL}, _ModName} | rest], inputMNameL, acc) do
    newImpL = remove_in_set_imports1(impL, inputMNameL, [])
    remove_in_set_imports(rest, inputMNameL, newImpL ++ acc)
  end

  defp remove_in_set_imports([], _, acc) do
    :lists.reverse(acc)
  end

  defp remove_in_set_imports1([i | is], inputMNameL, acc) do
    case r_SymbolsFromModule(i, :module) do
      r_Externaltypereference(type: mName) ->
        case :lists.member(mName, inputMNameL) do
          true ->
            remove_in_set_imports1(is, inputMNameL, acc)

          false ->
            remove_in_set_imports1(is, inputMNameL, [i | acc])
        end

      _ ->
        remove_in_set_imports1(is, inputMNameL, [i | acc])
    end
  end

  defp remove_in_set_imports1([], _, acc) do
    :lists.reverse(acc)
  end

  defp remove_import_doubles([]) do
    []
  end

  defp remove_import_doubles(importList) do
    mergedImportList =
      merge_symbols_from_module(
        importList,
        []
      )

    delete_double_of_symbol(mergedImportList, [])
  end

  defp merge_symbols_from_module([imp | imps], acc) do
    r_Externaltypereference(type: modName) = r_SymbolsFromModule(imp, :module)

    ifromModName =
      :lists.filter(
        fn i ->
          case r_SymbolsFromModule(i, :module) do
            r_Externaltypereference(type: ^modName) ->
              true

            r_Externalvaluereference(value: ^modName) ->
              true

            _ ->
              false
          end
        end,
        imps
      )

    newImps = :lists.subtract(imps, ifromModName)

    newImp =
      r_SymbolsFromModule(imp,
        symbols:
          :lists.append(
            :lists.map(
              fn sL ->
                r_SymbolsFromModule(sL, :symbols)
              end,
              [imp | ifromModName]
            )
          )
      )

    merge_symbols_from_module(newImps, [newImp | acc])
  end

  defp merge_symbols_from_module([], acc) do
    :lists.reverse(acc)
  end

  defp delete_double_of_symbol([i | is], acc) do
    symL = r_SymbolsFromModule(i, :symbols)
    newSymL = delete_double_of_symbol1(symL, [])

    delete_double_of_symbol(
      is,
      [r_SymbolsFromModule(i, symbols: newSymL) | acc]
    )
  end

  defp delete_double_of_symbol([], acc) do
    acc
  end

  defp delete_double_of_symbol1([tRef = r_Externaltypereference(type: trefName) | rest], acc) do
    newRest =
      :lists.filter(
        fn s ->
          case s do
            r_Externaltypereference(type: ^trefName) ->
              false

            _ ->
              true
          end
        end,
        rest
      )

    delete_double_of_symbol1(newRest, [tRef | acc])
  end

  defp delete_double_of_symbol1([vRef = r_Externalvaluereference(value: vName) | rest], acc) do
    newRest =
      :lists.filter(
        fn s ->
          case s do
            r_Externalvaluereference(value: ^vName) ->
              false

            _ ->
              true
          end
        end,
        rest
      )

    delete_double_of_symbol1(newRest, [vRef | acc])
  end

  defp delete_double_of_symbol1(
         [
           tRef = {r_Externaltypereference(type: mRef), r_Externaltypereference(type: tRef)}
           | rest
         ],
         acc
       ) do
    newRest =
      :lists.filter(
        fn s ->
          case s do
            {r_Externaltypereference(type: ^mRef), r_Externaltypereference(type: ^tRef)} ->
              false

            _ ->
              true
          end
        end,
        rest
      )

    delete_double_of_symbol1(newRest, [tRef | acc])
  end

  defp delete_double_of_symbol1([], acc) do
    acc
  end

  defp generate({m, codeTuple}, outFile, encodingRule, options) do
    {types, values, ptypes, classes, objects, objectSets} = codeTuple

    code =
      r_abst(
        name: r_module(m, :name),
        types: types,
        values: values,
        ptypes: ptypes,
        classes: classes,
        objects: objects,
        objsets: objectSets
      )

    setup_bit_string_format(options)
    setup_legacy_erlang_types(options)
    :asn1ct_table.new(:check_functions)
    gen = init_gen_record(encodingRule, options)
    check_maps_option(gen)

    try do
      specialized_decode_prepare(gen, m)
    catch
      {:error, reason} ->
        warning(
          'Error in configuration file: ~n~p~n',
          [reason],
          options,
          'Error in configuration file'
        )
    end

    :asn1ct_gen.pgen(outFile, gen, code)
    cleanup_bit_string_format()
    :erlang.erase(:tlv_format)
    :erlang.erase(:class_default_type)
    :asn1ct_table.delete(:check_functions)
    :ok
  end

  defp init_gen_record(encodingRule, options) do
    erule =
      case encodingRule do
        :uper ->
          :per

        _ ->
          encodingRule
      end

    der = :proplists.get_bool(:der, options)

    jer =
      :proplists.get_bool(
        :jer,
        options
      ) and encodingRule !== :jer

    aligned = encodingRule === :per
    recPrefix = :proplists.get_value(:record_name_prefix, options, '')
    macroPrefix = :proplists.get_value(:macro_name_prefix, options, '')

    pack =
      case :proplists.get_value(:maps, options, false) do
        true ->
          :map

        false ->
          :record
      end

    r_gen(
      erule: erule,
      der: der,
      jer: jer,
      aligned: aligned,
      rec_prefix: recPrefix,
      macro_prefix: macroPrefix,
      pack: pack,
      options: options
    )
  end

  defp setup_legacy_erlang_types(opts) do
    f =
      case :lists.member(:legacy_erlang_types, opts) do
        false ->
          case get_bit_string_format() do
            :bitstring ->
              false

            :compact ->
              legacy_forced_info(:compact_bit_string)
              true

            :legacy ->
              legacy_forced_info(:legacy_bit_string)
              true
          end

        true ->
          true
      end

    :erlang.put(:use_legacy_erlang_types, f)
  end

  defp legacy_forced_info(opt) do
    :io.format('Info: The option \'legacy_erlang_types\' is implied by the \'~s\' option.\n', [
      opt
    ])
  end

  def use_legacy_types() do
    :erlang.get(:use_legacy_erlang_types)
  end

  defp setup_bit_string_format(opts) do
    format =
      case {:lists.member(
              :compact_bit_string,
              opts
            ), :lists.member(:legacy_bit_string, opts)} do
        {false, false} ->
          :bitstring

        {true, false} ->
          :compact

        {false, true} ->
          :legacy

        {true, true} ->
          message = 'Contradicting options given: compact_bit_string and legacy_bit_string'
          exit({:error, {:asn1, message}})
      end

    :erlang.put(:bit_string_format, format)
  end

  defp cleanup_bit_string_format() do
    :erlang.erase(:bit_string_format)
  end

  def get_bit_string_format() do
    :erlang.get(:bit_string_format)
  end

  defp check_maps_option(r_gen(pack: :map)) do
    case get_bit_string_format() do
      :bitstring ->
        :ok

      _ ->
        message1 =
          'The \'maps\' option must not be combined with \'compact_bit_string\' or \'legacy_bit_string\''

        exit({:error, {:asn1, message1}})
    end

    case use_legacy_types() do
      false ->
        :ok

      true ->
        message2 = 'The \'maps\' option must not be combined with \'legacy_erlang_types\''
        exit({:error, {:asn1, message2}})
    end
  end

  defp check_maps_option(r_gen()) do
    :ok
  end

  def parse_and_save(module, s) do
    options = r_state(s, :options)
    sourceDir = r_state(s, :sourcedir)

    includes =
      for {:i, i} <- options do
        i
      end

    erule = r_state(s, :erule)
    maps = :lists.member(:maps, options)

    case get_input_file(module, [sourceDir | includes]) do
      {:file, suffixedASN1source} ->
        mtime = :filelib.last_modified(suffixedASN1source)

        case :asn1_db.dbload(module, erule, maps, mtime) do
          :ok ->
            :ok

          :error ->
            parse_and_save1(s, suffixedASN1source, options)
        end

      err when not maps ->
        case :asn1_db.dbload(module) do
          :ok ->
            warning(
              'could not do a consistency check of the ~p file: no asn1 source file was found.~n',
              [:lists.concat([module, '.asn1db'])],
              options
            )

          :error ->
            :ok
        end

        {:error, {:asn1, :input_file_error, err}}

      err ->
        {:error, {:asn1, :input_file_error, err}}
    end
  end

  defp parse_and_save1(r_state(erule: erule), file, options) do
    ext = :filename.extension(file)
    base = :filename.basename(file, ext)
    dbFile = outfile(base, 'asn1db', options)
    st = r_st(file: file, dbfile: dbFile, erule: erule)
    passes = parse_and_save_passes()
    run_passes(passes, st)
  end

  defp get_input_file(module, []) do
    module
  end

  defp get_input_file(module, [i | includes]) do
    case (try do
            input_file_type(:filename.join([i, module]))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:single_file, fileName} ->
        {:file, fileName}

      _ ->
        get_input_file(module, includes)
    end
  end

  defp input_file_type(name, i) do
    case input_file_type(name) do
      {:error, _} ->
        input_file_type2(:filename.basename(name), i)

      err = {:input_file_error, _} ->
        err

      res ->
        res
    end
  end

  defp input_file_type2(name, [i | is]) do
    case input_file_type(:filename.join([i, name])) do
      {:error, _} ->
        input_file_type2(name, is)

      err = {:input_file_error, _} ->
        err

      res ->
        res
    end
  end

  defp input_file_type2(name, []) do
    input_file_type(name)
  end

  defp input_file_type([]) do
    {:empty_name, []}
  end

  defp input_file_type(file) do
    case :filename.extension(file) do
      [] ->
        case :file.read_file_info(:lists.concat([file, '.asn1'])) do
          {:ok, _FileInfo} ->
            {:single_file, :lists.concat([file, '.asn1'])}

          _Error ->
            case :file.read_file_info(:lists.concat([file, '.asn'])) do
              {:ok, _FileInfo} ->
                {:single_file, :lists.concat([file, '.asn'])}

              ^_Error ->
                case :file.read_file_info(:lists.concat([file, '.py'])) do
                  {:ok, _FileInfo} ->
                    {:single_file, :lists.concat([file, '.py'])}

                  error ->
                    error
                end
            end
        end

      '.asn1config' ->
        case read_config_file_info(file, :asn1_module) do
          {:ok, asn1Module} ->
            input_file_type(asn1Module)

          error ->
            error
        end

      asn1SFix ->
        base = :filename.basename(file, asn1SFix)

        ret =
          case :filename.extension(base) do
            [] ->
              {:single_file, file}

            setSFix when setSFix == '.set' ->
              {:multiple_files_file, :erlang.list_to_atom(:filename.basename(base, setSFix)),
               file}

            _Error ->
              throw({:input_file_error, {:"Bad input file", file}})
          end

        case :file.read_file_info(file) do
          {:ok, _} ->
            ret

          err ->
            err
        end
    end
  end

  defp get_file_list(file, includes) do
    case :file.open(file, [:read]) do
      {:error, reason} ->
        {:error, {file, :file.format_error(reason)}}

      {:ok, stream} ->
        get_file_list1(stream, :filename.dirname(file), includes, [])
    end
  end

  defp get_file_list1(stream, dir, includes, acc) do
    ret = :io.get_line(stream, :"")

    case ret do
      :eof ->
        :ok = :file.close(stream)
        :lists.reverse(acc)

      fileName ->
        suffixedNameList =
          case (try do
                  input_file_type(
                    :filename.join([
                      dir,
                      :lists.delete(
                        ?\n,
                        fileName
                      )
                    ]),
                    includes
                  )
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end) do
            {:empty_name, []} ->
              []

            {:single_file, name} ->
              [name]

            {:multiple_files_file, _, name} ->
              get_file_list(name, includes)

            _Err ->
              []
          end

        get_file_list1(stream, dir, includes, suffixedNameList ++ acc)
    end
  end

  defp get_rule(options) do
    case (for rule <- [:ber, :per, :uper, :jer], opt <- options, rule === opt do
            rule
          end) do
      [rule] ->
        rule

      [rule | _] ->
        rule

      [] ->
        :ber
    end
  end

  defp translate_options([:ber_bin | t]) do
    :io.format('Warning: The option \'ber_bin\' is now called \'ber\'.\n')
    [:ber | translate_options(t)]
  end

  defp translate_options([:per_bin | t]) do
    :io.format('Warning: The option \'per_bin\' is now called \'per\'.\n')
    [:per | translate_options(t)]
  end

  defp translate_options([:uper_bin | t]) do
    :io.format('Warning: The option \'uper_bin\' is now called \'uper\'.\n')
    translate_options([:uper | t])
  end

  defp translate_options([:nif | t]) do
    :io.format('Warning: The option \'nif\' is no longer needed.\n')
    translate_options(t)
  end

  defp translate_options([:optimize | t]) do
    :io.format('Warning: The option \'optimize\' is no longer needed.\n')
    translate_options(t)
  end

  defp translate_options([:inline | t]) do
    :io.format('Warning: The option \'inline\' is no longer needed.\n')
    translate_options(t)
  end

  defp translate_options([{:inline, _} | _]) do
    :io.format('ERROR: The option {inline,OutputFilename} is no longer supported.\n')
    throw({:error, {:unsupported_option, :inline}})
  end

  defp translate_options([h | t]) do
    [h | translate_options(t)]
  end

  defp translate_options([]) do
    []
  end

  defp remove_asn_flags(options) do
    for x <- options, not is_asn1_flag(x) do
      x
    end
  end

  defp is_asn1_flag(:asn1config) do
    true
  end

  defp is_asn1_flag(:ber) do
    true
  end

  defp is_asn1_flag(:compact_bit_string) do
    true
  end

  defp is_asn1_flag(:debug) do
    true
  end

  defp is_asn1_flag(:der) do
    true
  end

  defp is_asn1_flag(:legacy_bit_string) do
    true
  end

  defp is_asn1_flag({:macro_name_prefix, _}) do
    true
  end

  defp is_asn1_flag({:n2n, _}) do
    true
  end

  defp is_asn1_flag(:noobj) do
    true
  end

  defp is_asn1_flag(:no_ok_wrapper) do
    true
  end

  defp is_asn1_flag(:optimize) do
    true
  end

  defp is_asn1_flag(:per) do
    true
  end

  defp is_asn1_flag({:record_name_prefix, _}) do
    true
  end

  defp is_asn1_flag(:undec_rec) do
    true
  end

  defp is_asn1_flag(:uper) do
    true
  end

  defp is_asn1_flag(:verbose) do
    true
  end

  defp is_asn1_flag(_) do
    false
  end

  defp outfile(base, ext, opts) do
    obase =
      case :lists.keysearch(:outdir, 1, opts) do
        {:value, {:outdir, odir}} ->
          :filename.join(odir, base)

        _NotFound ->
          base
      end

    case ext do
      [] ->
        obase

      _ ->
        :lists.concat([obase, '.', ext])
    end
  end

  defp includes(file, options) do
    options2 = include_append('.', options)

    options3 =
      include_append(
        :filename.dirname(file),
        options2
      )

    case :proplists.get_value(:outdir, options) do
      :undefined ->
        options3

      outDir ->
        include_prepend(outDir, options3)
    end
  end

  defp include_append(dir, options) do
    option_add({:i, dir}, options, fn opts ->
      opts ++ [{:i, dir}]
    end)
  end

  defp include_prepend(dir, options) do
    option_add({:i, dir}, options, fn opts ->
      [{:i, dir} | opts]
    end)
  end

  defp option_add(option, options, fun) do
    case :lists.member(option, options) do
      true ->
        options

      false ->
        fun.(options)
    end
  end

  defp strip_includes(includes) do
    for {:i, i} <- includes do
      i
    end
  end

  def compile_asn(file, outFile, options) do
    compile(:lists.concat([file, '.asn']), outFile, options)
  end

  def compile_asn1(file, outFile, options) do
    compile(:lists.concat([file, '.asn1']), outFile, options)
  end

  def compile_py(file, outFile, options) do
    compile(:lists.concat([file, '.py']), outFile, options)
  end

  def compile(file, _OutFile, options) do
    case compile(file, make_erl_options(options)) do
      {:error, _Reason} ->
        :error

      :ok ->
        :ok

      parseRes when is_tuple(parseRes) ->
        :io.format('~p~n', [parseRes])
        :ok

      scanRes when is_list(scanRes) ->
        :io.format('~p~n', [scanRes])
        :ok
    end
  end

  defp make_erl_options(opts) do
    includes = r_options(opts, :includes)
    defines = r_options(opts, :defines)
    outdir = r_options(opts, :outdir)
    warning = r_options(opts, :warning)
    verbose = r_options(opts, :verbose)
    specific = r_options(opts, :specific)
    optimize = r_options(opts, :optimize)
    outputType = r_options(opts, :output_type)
    cwd = r_options(opts, :cwd)

    options =
      case verbose do
        true ->
          [:verbose]

        false ->
          []
      end ++
        case warning do
          0 ->
            []

          _ ->
            [:warnings]
        end ++
        [] ++
        case optimize do
          1 ->
            [:optimize]

          999 ->
            []

          _ ->
            [{:optimize, optimize}]
        end ++
        :lists.map(
          fn
            {name, value} ->
              {:d, name, value}

            name ->
              {:d, name}
          end,
          defines
        ) ++
        case outputType do
          :undefined ->
            [:ber]

          _ ->
            [outputType]
        end

    options ++
      [
        :errors,
        {:cwd, cwd},
        {:outdir, outdir}
        | :lists.map(
            fn dir ->
              {:i, dir}
            end,
            includes
          )
      ] ++ specific
  end

  defp pretty2(module, absFile) do
    {:ok, f} = :file.open(absFile, [:write])
    m = :asn1_db.dbget(module, :MODULE)
    :io.format(f, '%%%%%%%%%%%%%%%%%%%   ~p  %%%%%%%%%%%%%%%%%%%~n', [module])
    :io.format(f, '~s.\n', [:asn1ct_pretty_format.term(r_module(m, :defid))])
    :io.format(f, '~s.\n', [:asn1ct_pretty_format.term(r_module(m, :tagdefault))])
    :io.format(f, '~s.\n', [:asn1ct_pretty_format.term(r_module(m, :exports))])
    :io.format(f, '~s.\n', [:asn1ct_pretty_format.term(r_module(m, :imports))])
    :io.format(f, '~s.\n\n', [:asn1ct_pretty_format.term(r_module(m, :extensiondefault))])
    {types, values, parameterizedTypes, classes, objects, objectSets} = r_module(m, :typeorval)
    :io.format(f, '%%%%%%%%%%%%%%%%%%% TYPES in ~p  %%%%%%%%%%%%%%%%%%%~n', [module])

    :lists.foreach(
      fn t ->
        :io.format(f, '~s.\n', [
          :asn1ct_pretty_format.term(
            :asn1_db.dbget(
              module,
              t
            )
          )
        ])
      end,
      types
    )

    :io.format(f, '%%%%%%%%%%%%%%%%%%% VALUES in ~p  %%%%%%%%%%%%%%%%%%%~n', [module])

    :lists.foreach(
      fn t ->
        :io.format(f, '~s.\n', [
          :asn1ct_pretty_format.term(
            :asn1_db.dbget(
              module,
              t
            )
          )
        ])
      end,
      values
    )

    :io.format(f, '%%%%%%%%%%%%%%%%%%% Parameterized Types in ~p  %%%%%%%%%%%%%%%%%%%~n', [module])

    :lists.foreach(
      fn t ->
        :io.format(f, '~s.\n', [
          :asn1ct_pretty_format.term(
            :asn1_db.dbget(
              module,
              t
            )
          )
        ])
      end,
      parameterizedTypes
    )

    :io.format(f, '%%%%%%%%%%%%%%%%%%% Classes in ~p  %%%%%%%%%%%%%%%%%%%~n', [module])

    :lists.foreach(
      fn t ->
        :io.format(f, '~s.\n', [
          :asn1ct_pretty_format.term(
            :asn1_db.dbget(
              module,
              t
            )
          )
        ])
      end,
      classes
    )

    :io.format(f, '%%%%%%%%%%%%%%%%%%% Objects in ~p  %%%%%%%%%%%%%%%%%%%~n', [module])

    :lists.foreach(
      fn t ->
        :io.format(f, '~s.\n', [
          :asn1ct_pretty_format.term(
            :asn1_db.dbget(
              module,
              t
            )
          )
        ])
      end,
      objects
    )

    :io.format(f, '%%%%%%%%%%%%%%%%%%% Object Sets in ~p  %%%%%%%%%%%%%%%%%%%~n', [module])

    :lists.foreach(
      fn t ->
        :io.format(f, '~s.\n', [
          :asn1ct_pretty_format.term(
            :asn1_db.dbget(
              module,
              t
            )
          )
        ])
      end,
      objectSets
    )
  end

  defp start(includes) when is_list(includes) do
    :asn1_db.dbstart(includes)
  end

  def test(module) do
    test_module(module, [])
  end

  def test(module, [] = options) do
    test_module(module, options)
  end

  def test(module, [{:i, _} | _] = options) do
    test_module(module, options)
  end

  def test(module, type) do
    test_type(module, type, [])
  end

  def test(module, type, [] = options) do
    test_type(module, type, options)
  end

  def test(module, type, [{:i, _} | _] = options) do
    test_type(module, type, options)
  end

  def test(module, type, value) do
    test_value(module, type, value)
  end

  defp test_module(module, includes) do
    in_process(fn ->
      start(strip_includes(includes))

      case check(module, includes) do
        {:ok, newTypes} ->
          test_each(module, newTypes)

        error ->
          error
      end
    end)
  end

  defp test_each(module, [type | rest]) do
    case test_type(module, type) do
      {:ok, _Result} ->
        test_each(module, rest)

      error ->
        error
    end
  end

  defp test_each(_, []) do
    :ok
  end

  defp test_type(module, type, includes) do
    in_process(fn ->
      start(strip_includes(includes))

      case check(module, includes) do
        {:ok, _NewTypes} ->
          test_type(module, type)

        error ->
          error
      end
    end)
  end

  defp test_type(module, type) do
    case get_value(module, type) do
      {:ok, val} ->
        test_value(module, type, val)

      {:error, reason} ->
        {:error, {:asn1, {:value, reason}}}
    end
  end

  defp test_value(module, type, value) do
    in_process(fn ->
      case (try do
              module.encode(type, value)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:ok, bytes} ->
          test_value_decode(module, type, value, bytes)

        bytes when is_binary(bytes) ->
          test_value_decode(module, type, value, bytes)

        error ->
          {:error, {:asn1, {:encode, {{module, type, value}, error}}}}
      end
    end)
  end

  defp test_value_decode(module, type, value, bytes) do
    newBytes = prepare_bytes(bytes)

    case module.decode(type, newBytes) do
      {:ok, ^value} ->
        {:ok, {module, type, value}}

      {:ok, ^value, <<>>} ->
        {:ok, {module, type, value}}

      ^value ->
        {:ok, {module, type, value}}

      {^value, <<>>} ->
        {:ok, {module, type, value}}

      {:ok, res} ->
        {:error, {:asn1, {:encode_decode_mismatch, {{module, type, value}, res}}}}

      {:ok, res, rest} ->
        {:error, {:asn1, {:encode_decode_mismatch, {{module, type, value}, {res, rest}}}}}

      error ->
        {:error, {:asn1, {{:decode, {module, type, value}, error}}}}
    end
  end

  def value(module, type) do
    value(module, type, [])
  end

  def value(module, type, includes) do
    in_process(fn ->
      start(strip_includes(includes))

      case check(module, includes) do
        {:ok, _NewTypes} ->
          get_value(module, type)

        error ->
          error
      end
    end)
  end

  defp get_value(module, type) do
    case :asn1ct_value.from_type(module, type) do
      {:error, reason} ->
        {:error, reason}

      result ->
        {:ok, result}
    end
  end

  defp check(module, includes) do
    case :asn1_db.dbload(module) do
      :error ->
        {:error, :asn1db_missing_or_out_of_date}

      :ok ->
        m = :asn1_db.dbget(module, :MODULE)
        typeOrVal = r_module(m, :typeorval)

        state =
          r_state(
            mname: r_module(m, :name),
            module: r_module(m, typeorval: []),
            options: includes
          )

        case :asn1ct_check.check(state, typeOrVal) do
          {:ok, {newTypes, _, _, _, _, _}, _} ->
            {:ok, newTypes}

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp prepare_bytes(bytes) when is_binary(bytes) do
    bytes
  end

  defp prepare_bytes(bytes) do
    :erlang.list_to_binary(bytes)
  end

  def vsn() do
    '0.0.1'
  end

  defp specialized_decode_prepare(r_gen(erule: :ber, options: options) = gen, m) do
    case :lists.member(:asn1config, options) do
      true ->
        special_decode_prepare_1(gen, m)

      false ->
        :ok
    end
  end

  defp specialized_decode_prepare(_, _) do
    :ok
  end

  defp special_decode_prepare_1(r_gen(options: options) = gen, m) do
    modName =
      case :lists.keyfind(:asn1config, 1, options) do
        {_, mName} ->
          mName

        false ->
          r_module(m, :name)
      end

    case read_config_file(gen, modName) do
      :no_config_file ->
        :ok

      cfgList ->
        selectedDecode =
          get_config_info(
            cfgList,
            :selective_decode
          )

        exclusiveDecode =
          get_config_info(
            cfgList,
            :exclusive_decode
          )

        commandList =
          create_partial_decode_gen_info(
            r_module(m, :name),
            selectedDecode
          )

        save_config(:partial_decode, commandList)
        save_gen_state(:selective_decode, selectedDecode)

        commandList2 =
          create_partial_inc_decode_gen_info(
            r_module(m, :name),
            exclusiveDecode
          )

        part_inc_tlv_tags = tlv_tags(commandList2)

        save_config(
          :partial_incomplete_decode,
          part_inc_tlv_tags
        )

        save_gen_state(:exclusive_decode, exclusiveDecode, part_inc_tlv_tags)
    end
  end

  defp create_partial_inc_decode_gen_info(modName, {mod, [{name, l} | ls]})
       when is_list(l) do
    topTypeName = partial_inc_dec_toptype(l)

    [
      {name, topTypeName, create_partial_inc_decode_gen_info1(modName, topTypeName, {mod, l})}
      | create_partial_inc_decode_gen_info(modName, {mod, ls})
    ]
  end

  defp create_partial_inc_decode_gen_info(_, {_, []}) do
    []
  end

  defp create_partial_inc_decode_gen_info(_, []) do
    []
  end

  defp create_partial_inc_decode_gen_info1(modName, topTypeName, {modName, [_TopType | rest]}) do
    case :asn1_db.dbget(modName, topTypeName) do
      r_typedef(typespec: tS) ->
        tagCommand = get_tag_command(tS, :mandatory, :mandatory)
        create_pdec_inc_command(modName, get_components(r_type(tS, :def)), rest, [tagCommand])

      _ ->
        throw({:error, {'wrong type list in asn1 config file', topTypeName}})
    end
  end

  defp create_partial_inc_decode_gen_info1(m1, _, {m2, _}) when m1 != m2 do
    throw({:error, {'wrong module name in asn1 config file', m2}})
  end

  defp create_partial_inc_decode_gen_info1(_, _, tNL) do
    throw({:error, {'wrong type list in asn1 config file', tNL}})
  end

  defp create_pdec_inc_command(_ModName, _, [], acc) do
    :lists.reverse(acc)
  end

  defp create_pdec_inc_command(modName, {comps1, comps2}, tNL, acc)
       when is_list(comps1) and is_list(comps2) do
    create_pdec_inc_command(modName, comps1 ++ comps2, tNL, acc)
  end

  defp create_pdec_inc_command(modN, clist, [cL | _Rest], [[]])
       when is_list(cL) do
    create_pdec_inc_command(modN, clist, cL, [])
  end

  defp create_pdec_inc_command(modN, clist, [cL | _Rest], acc)
       when is_list(cL) do
    innerDirectives = create_pdec_inc_command(modN, clist, cL, [])
    :lists.reverse([innerDirectives | acc])
  end

  defp create_pdec_inc_command(
         modName,
         cList = [
           r_ComponentType(name: name, typespec: tS, prop: prop)
           | comps
         ],
         tNL = [c1 | cs],
         acc
       ) do
    case c1 do
      {^name, :undecoded} ->
        tagCommand = get_tag_command(tS, :undec, prop)
        create_pdec_inc_command(modName, comps, cs, concat_sequential(tagCommand, acc))

      {^name, :parts} ->
        tagCommand = get_tag_command(tS, :parts, prop)
        create_pdec_inc_command(modName, comps, cs, concat_sequential(tagCommand, acc))

      l when is_list(l) ->
        create_pdec_inc_command(modName, cList, l, acc)

      {^name, restPartsList} when is_list(restPartsList) ->
        case get_tag_command(tS, :mandatory, prop) do
          :mandatory ->
            innerDirectives =
              create_pdec_inc_command(modName, r_type(tS, :def), restPartsList, [])

            create_pdec_inc_command(modName, comps, cs, [[:mandatory, innerDirectives] | acc])

          [opt, encTag] ->
            innerDirectives =
              create_pdec_inc_command(modName, r_type(tS, :def), restPartsList, [])

            create_pdec_inc_command(modName, comps, cs, [[opt, encTag, innerDirectives] | acc])
        end

      _ ->
        tagCommand = get_tag_command(tS, :mandatory, prop)
        create_pdec_inc_command(modName, comps, tNL, concat_sequential(tagCommand, acc))
    end
  end

  defp create_pdec_inc_command(
         modName,
         {:CHOICE, [r_ComponentType(name: c1, typespec: tS, prop: prop) | comps]},
         [{c1, directive} | rest],
         acc
       ) do
    case directive do
      list when is_list(list) ->
        tagCommand = get_tag_command(tS, :alt, prop)
        compAcc = create_pdec_inc_command(modName, get_components(r_type(tS, :def)), list, [])

        newAcc =
          case tagCommand do
            [command, tag] when is_atom(command) ->
              [[command, tag, compAcc] | acc]

            [l1, _L2 | ^rest] when is_list(l1) ->
              case :lists.reverse(tagCommand) do
                [atom | comms] when is_atom(atom) ->
                  [
                    concat_sequential(
                      :lists.reverse(comms),
                      [atom, compAcc]
                    )
                    | acc
                  ]

                [[command2, tag2] | comms] ->
                  [
                    concat_sequential(
                      :lists.reverse(comms),
                      [[command2, tag2, compAcc]]
                    )
                    | acc
                  ]
              end
          end

        create_pdec_inc_command(modName, {:CHOICE, comps}, rest, newAcc)

      :undecoded ->
        tagCommand = get_tag_command(tS, :alt_undec, prop)

        create_pdec_inc_command(
          modName,
          {:CHOICE, comps},
          rest,
          concat_sequential(tagCommand, acc)
        )

      :parts ->
        tagCommand = get_tag_command(tS, :alt_parts, prop)

        create_pdec_inc_command(
          modName,
          {:CHOICE, comps},
          rest,
          concat_sequential(tagCommand, acc)
        )
    end
  end

  defp create_pdec_inc_command(
         modName,
         {:CHOICE, [r_ComponentType(typespec: tS, prop: prop) | comps]},
         tNL,
         acc
       ) do
    tagCommand = get_tag_command(tS, :alt, prop)
    create_pdec_inc_command(modName, {:CHOICE, comps}, tNL, concat_sequential(tagCommand, acc))
  end

  defp create_pdec_inc_command(m, {:CHOICE, {cs1, cs2}}, tNL, acc)
       when is_list(cs1) and is_list(cs2) do
    create_pdec_inc_command(m, {:CHOICE, cs1 ++ cs2}, tNL, acc)
  end

  defp create_pdec_inc_command(modName, r_Externaltypereference(module: m, type: name), tNL, acc) do
    r_type(def: def__) = get_referenced_type(m, name)
    create_pdec_inc_command(modName, get_components(def__), tNL, acc)
  end

  defp create_pdec_inc_command(_, _, tNL, _) do
    throw({:error, {'unexpected error when creating partial decode command', tNL}})
  end

  def partial_inc_dec_toptype([t | _]) when is_atom(t) do
    t
  end

  def partial_inc_dec_toptype([{t, _} | _]) when is_atom(t) do
    t
  end

  def partial_inc_dec_toptype([l | _]) when is_list(l) do
    partial_inc_dec_toptype(l)
  end

  def partial_inc_dec_toptype(_) do
    throw({:error, {'no top type found for partial incomplete decode'}})
  end

  defp create_partial_decode_gen_info(modName, {modName, typeLists}) do
    for tL <- typeLists do
      create_partial_decode_gen_info1(modName, tL)
    end
  end

  defp create_partial_decode_gen_info(_, []) do
    []
  end

  defp create_partial_decode_gen_info(_M1, {m2, _}) do
    throw({:error, {'wrong module name in asn1 config file', m2}})
  end

  defp create_partial_decode_gen_info1(modName, {funcName, typeList}) do
    case typeList do
      [topType | rest] ->
        case :asn1_db.dbget(modName, topType) do
          r_typedef(typespec: tS) ->
            tagCommand = get_tag_command(tS, :choosen)

            ret =
              create_pdec_command(
                modName,
                get_components(r_type(tS, :def)),
                rest,
                concat_tags(tagCommand, [])
              )

            {funcName, ret}

          _ ->
            throw({:error, {'wrong type list in asn1 config file', typeList}})
        end

      _ ->
        []
    end
  end

  defp create_partial_decode_gen_info1(_, _) do
    :ok
  end

  defp create_pdec_command(_ModName, _, [], acc) do
    remove_empty_lists = fn
      [[] | l], res, fun ->
        fun.(l, res, fun)

      [], res, _ ->
        res

      [h | l], res, fun ->
        fun.(l, [h | res], fun)
    end

    remove_empty_lists.(acc, [], remove_empty_lists)
  end

  defp create_pdec_command(
         modName,
         [r_ComponentType(name: c1, typespec: tS) | _Comps],
         [c1 | cs],
         acc
       ) do
    tagCommand = get_tag_command(tS, :choosen)

    create_pdec_command(
      modName,
      get_components(r_type(tS, :def)),
      cs,
      concat_tags(tagCommand, acc)
    )
  end

  defp create_pdec_command(
         modName,
         [r_ComponentType(typespec: tS, prop: prop) | comps],
         [c2 | cs],
         acc
       ) do
    tagCommand =
      case prop do
        :mandatory ->
          get_tag_command(tS, :skip)

        _ ->
          get_tag_command(tS, :skip_optional)
      end

    create_pdec_command(modName, comps, [c2 | cs], concat_tags(tagCommand, acc))
  end

  defp create_pdec_command(
         modName,
         {:CHOICE, [comp = r_ComponentType(name: c1) | _]},
         tNL = [c1 | _Cs],
         acc
       ) do
    create_pdec_command(modName, [comp], tNL, acc)
  end

  defp create_pdec_command(modName, {:CHOICE, [r_ComponentType() | comps]}, tNL, acc) do
    create_pdec_command(modName, {:CHOICE, comps}, tNL, acc)
  end

  defp create_pdec_command(modName, {:CHOICE, {cs1, cs2}}, tNL, acc)
       when is_list(cs1) and is_list(cs2) do
    create_pdec_command(modName, {:CHOICE, cs1 ++ cs2}, tNL, acc)
  end

  defp create_pdec_command(
         modName,
         r_Externaltypereference(module: m, type: c1),
         typeNameList,
         acc
       ) do
    r_type(def: def__) = get_referenced_type(m, c1)
    create_pdec_command(modName, get_components(def__), typeNameList, acc)
  end

  defp create_pdec_command(modName, tS = r_type(def: def__), [c1 | cs], acc) do
    case c1 do
      [1] ->
        tagCommand = get_tag_command(tS, :choosen)
        create_pdec_command(modName, def__, cs, concat_tags(tagCommand, acc))

      [n] when is_integer(n) ->
        tagCommand = get_tag_command(tS, :skip)
        create_pdec_command(modName, def__, [[n - 1] | cs], concat_tags(tagCommand, acc))

      err ->
        throw({:error, {'unexpected error when creating partial decode command', err}})
    end
  end

  defp create_pdec_command(_, _, tNL, _) do
    throw({:error, {'unexpected error when creating partial decode command', tNL}})
  end

  defp get_components(r_SEQUENCE(components: {c1, c2}))
       when is_list(c1) and
              is_list(c2) do
    c1 ++ c2
  end

  defp get_components(r_SEQUENCE(components: components)) do
    components
  end

  defp get_components(r_SET(components: {c1, c2}))
       when is_list(c1) and
              is_list(c2) do
    c1 ++ c2
  end

  defp get_components(r_SET(components: components)) do
    components
  end

  defp get_components({:"SEQUENCE OF", components}) do
    components
  end

  defp get_components({:"SET OF", components}) do
    components
  end

  defp get_components(def__) do
    def__
  end

  defp concat_sequential(l = [a, b], acc)
       when is_atom(a) and
              is_binary(b) do
    [l | acc]
  end

  defp concat_sequential(l, acc) when is_list(l) do
    concat_sequential1(:lists.reverse(l), acc)
  end

  defp concat_sequential(a, acc) do
    [a | acc]
  end

  defp concat_sequential1([], acc) do
    acc
  end

  defp concat_sequential1([[]], acc) do
    acc
  end

  defp concat_sequential1([el | restEl], acc) when is_list(el) do
    concat_sequential1(restEl, [el | acc])
  end

  defp concat_sequential1([:mandatory | restEl], acc) do
    concat_sequential1(restEl, [:mandatory | acc])
  end

  defp concat_sequential1(l, acc) do
    [l | acc]
  end

  defp many_tags([:skip]) do
    false
  end

  defp many_tags([:skip_optional, _]) do
    false
  end

  defp many_tags([:choosen, _]) do
    false
  end

  defp many_tags(_) do
    true
  end

  defp concat_tags(ts, acc) do
    case many_tags(ts) do
      true when is_list(ts) ->
        :lists.reverse(ts) ++ acc

      true ->
        [ts | acc]

      false ->
        [ts | acc]
    end
  end

  defp get_tag_command(r_type(tag: []), _) do
    []
  end

  defp get_tag_command(r_type(), :skip) do
    :skip
  end

  defp get_tag_command(r_type(tag: tags), :skip_optional) do
    tag = hd(tags)

    [
      :skip_optional,
      encode_tag_val(decode_class(r_tag(tag, :class)), r_tag(tag, :form), r_tag(tag, :number))
    ]
  end

  defp get_tag_command(r_type(tag: [tag]), command) do
    [
      command,
      encode_tag_val(decode_class(r_tag(tag, :class)), r_tag(tag, :form), r_tag(tag, :number))
    ]
  end

  defp get_tag_command(t = r_type(tag: [tag | tags]), command) do
    tC = get_tag_command(r_type(t, tag: [tag]), command)
    tCs = get_tag_command(r_type(t, tag: tags), command)

    case many_tags(tCs) do
      true when is_list(tCs) ->
        [tC | tCs]

      _ ->
        [tC, tCs]
    end
  end

  defp get_tag_command(r_type(tag: []), _, _) do
    []
  end

  defp get_tag_command(r_type(tag: [tag]), :mandatory, prop) do
    case prop do
      :mandatory ->
        :mandatory

      {:DEFAULT, _} ->
        [
          :default,
          encode_tag_val(decode_class(r_tag(tag, :class)), r_tag(tag, :form), r_tag(tag, :number))
        ]

      _ ->
        [
          :opt,
          encode_tag_val(decode_class(r_tag(tag, :class)), r_tag(tag, :form), r_tag(tag, :number))
        ]
    end
  end

  defp get_tag_command(r_type(tag: [tag]), command, prop) do
    [
      anonymous_dec_command(command, prop),
      encode_tag_val(decode_class(r_tag(tag, :class)), r_tag(tag, :form), r_tag(tag, :number))
    ]
  end

  defp get_tag_command(r_type(tag: tag), command, prop)
       when elem(tag, 0) === :tag do
    get_tag_command(r_type(tag: [tag]), command, prop)
  end

  defp get_tag_command(t = r_type(tag: [tag | tags]), command, prop) do
    [
      get_tag_command(r_type(t, tag: [tag]), command, prop),
      get_tag_command(r_type(t, tag: tags), command, prop)
    ]
  end

  defp anonymous_dec_command(:undec, :OPTIONAL) do
    :opt_undec
  end

  defp anonymous_dec_command(command, _) do
    command
  end

  defp get_referenced_type(m, name) do
    case :asn1_db.dbget(m, name) do
      r_typedef(typespec: tS) ->
        case tS do
          r_type(def: r_Externaltypereference(module: m2, type: name2)) ->
            get_referenced_type(m2, name2)

          r_type() ->
            tS

          _ ->
            throw({:error, {'unexpected element when fetching referenced type', tS}})
        end

      t ->
        throw({:error, {'unexpected element when fetching referenced type', t}})
    end
  end

  defp tlv_tags([]) do
    []
  end

  defp tlv_tags([:mandatory | rest]) do
    [:mandatory | tlv_tags(rest)]
  end

  defp tlv_tags([[command, tag] | rest])
       when is_atom(command) and is_binary(tag) do
    [[command, tlv_tag(tag)] | tlv_tags(rest)]
  end

  defp tlv_tags([[command, directives] | rest])
       when is_atom(command) and is_list(directives) do
    [[command, tlv_tags(directives)] | tlv_tags(rest)]
  end

  defp tlv_tags([[] | rest]) do
    tlv_tags(rest)
  end

  defp tlv_tags([{name, topType, l1} | rest])
       when is_list(l1) and is_atom(topType) do
    [{name, topType, tlv_tags(l1)} | tlv_tags(rest)]
  end

  defp tlv_tags([[command, tag, l1] | rest])
       when is_list(l1) and is_binary(tag) do
    [[command, tlv_tag(tag), tlv_tags(l1)] | tlv_tags(rest)]
  end

  defp tlv_tags([[:mandatory | rest]]) do
    [[:mandatory | tlv_tags(rest)]]
  end

  defp tlv_tags([l = [l1 | _] | rest]) when is_list(l1) do
    [tlv_tags(l) | tlv_tags(rest)]
  end

  defp tlv_tag(<<cl::size(2), _::size(1), tagNo::size(5)>>)
       when tagNo < 31 do
    cl <<< (16 + tagNo)
  end

  defp tlv_tag(<<cl::size(2), _::size(1), 31::size(5), 0::size(1), tagNo::size(7)>>) do
    cl <<< (16 + tagNo)
  end

  defp tlv_tag(<<cl::size(2), _::size(1), 31::size(5), buffer::binary>>) do
    tagNo = tlv_tag1(buffer, 0)
    cl <<< (16 + tagNo)
  end

  defp tlv_tag1(<<0::size(1), partialTag::size(7)>>, acc) do
    acc <<< 7 ||| partialTag
  end

  defp tlv_tag1(
         <<1::size(1), partialTag::size(7), buffer::binary>>,
         acc
       ) do
    tlv_tag1(buffer, acc <<< 7 ||| partialTag)
  end

  defp read_config_file_info(moduleName, infoType) when is_atom(infoType) do
    name = ensure_ext(moduleName, '.asn1config')
    cfgList = read_config_file0(name, [])
    get_config_info(cfgList, infoType)
  end

  defp read_config_file(r_gen(options: options), moduleName) do
    name = ensure_ext(moduleName, '.asn1config')

    includes =
      for {:i, i} <- options do
        i
      end

    read_config_file0(name, ['.' | includes])
  end

  defp read_config_file0(name, [d | dirs]) do
    case :file.consult(:filename.join(d, name)) do
      {:ok, cfgList} ->
        cfgList

      {:error, :enoent} ->
        read_config_file0(name, dirs)

      {:error, reason} ->
        error = 'error reading asn1 config file: ' ++ :file.format_error(reason)
        throw({:error, error})
    end
  end

  defp read_config_file0(_, []) do
    :no_config_file
  end

  defp ensure_ext(moduleName, ext) do
    name = :filename.join([moduleName])

    case :filename.extension(name) do
      ^ext ->
        name

      _ ->
        name ++ ext
    end
  end

  defp get_config_info(cfgList, infoType) do
    case :lists.keysearch(infoType, 1, cfgList) do
      {:value, {^infoType, value}} ->
        value

      false ->
        []
    end
  end

  defp save_config(key, info) do
    :asn1ct_table.new_reuse(:asn1_general)

    :asn1ct_table.insert(
      :asn1_general,
      {{:asn1_config, key}, info}
    )
  end

  def read_config_data(key) do
    case :asn1ct_table.exists(:asn1_general) do
      false ->
        :undefined

      true ->
        case :asn1ct_table.lookup(
               :asn1_general,
               {:asn1_config, key}
             ) do
          [{_, data}] ->
            data

          err ->
            err
        end
    end
  end

  defp save_gen_state(:exclusive_decode, {_, confList}, partIncTlvTagList) do
    state =
      case get_gen_state() do
        s when elem(s, 0) === :gen_state ->
          s

        _ ->
          r_gen_state()
      end

    stateRec =
      r_gen_state(state,
        inc_tag_pattern: partIncTlvTagList,
        inc_type_pattern: confList
      )

    save_config(:gen_state, stateRec)
  end

  defp save_gen_state(_, _, _) do
    case get_gen_state() do
      s when elem(s, 0) === :gen_state ->
        :ok

      _ ->
        save_config(:gen_state, r_gen_state())
    end
  end

  defp save_gen_state(
         :selective_decode,
         {_, type_component_name_list}
       ) do
    state =
      case get_gen_state() do
        s when elem(s, 0) === :gen_state ->
          s

        _ ->
          r_gen_state()
      end

    stateRec = r_gen_state(state, type_pattern: type_component_name_list)
    save_config(:gen_state, stateRec)
  end

  defp save_gen_state(:selective_decode, _) do
    :ok
  end

  defp save_gen_state(genState)
       when elem(genState, 0) === :gen_state do
    save_config(:gen_state, genState)
  end

  def get_gen_state_field(field) do
    case read_config_data(:gen_state) do
      :undefined ->
        :undefined

      genState when elem(genState, 0) === :gen_state ->
        get_gen_state_field(genState, field)

      err ->
        exit({:error, {:asn1, {'false configuration file info', err}}})
    end
  end

  defp get_gen_state_field(r_gen_state(active: active), :active) do
    active
  end

  defp get_gen_state_field(_, :active) do
    false
  end

  defp get_gen_state_field(gS, :prefix) do
    r_gen_state(gS, :prefix)
  end

  defp get_gen_state_field(gS, :inc_tag_pattern) do
    r_gen_state(gS, :inc_tag_pattern)
  end

  defp get_gen_state_field(gS, :tag_pattern) do
    r_gen_state(gS, :tag_pattern)
  end

  defp get_gen_state_field(gS, :inc_type_pattern) do
    r_gen_state(gS, :inc_type_pattern)
  end

  defp get_gen_state_field(gS, :type_pattern) do
    r_gen_state(gS, :type_pattern)
  end

  defp get_gen_state_field(gS, :func_name) do
    r_gen_state(gS, :func_name)
  end

  defp get_gen_state_field(gS, :namelist) do
    r_gen_state(gS, :namelist)
  end

  defp get_gen_state_field(gS, :tobe_refed_funcs) do
    r_gen_state(gS, :tobe_refed_funcs)
  end

  defp get_gen_state_field(gS, :gen_refed_funcs) do
    r_gen_state(gS, :gen_refed_funcs)
  end

  defp get_gen_state_field(gS, :generated_functions) do
    r_gen_state(gS, :generated_functions)
  end

  defp get_gen_state_field(gS, :suffix_index) do
    r_gen_state(gS, :suffix_index)
  end

  defp get_gen_state_field(gS, :current_suffix_index) do
    r_gen_state(gS, :current_suffix_index)
  end

  defp get_gen_state() do
    read_config_data(:gen_state)
  end

  def update_gen_state(field, data) do
    case get_gen_state() do
      state when elem(state, 0) === :gen_state ->
        update_gen_state(field, state, data)

      _ ->
        exit({:error, {:asn1, {:internal, 'tried to update nonexistent gen_state', field, data}}})
    end
  end

  defp update_gen_state(:active, state, data) do
    save_gen_state(r_gen_state(state, active: data))
  end

  defp update_gen_state(:prefix, state, data) do
    save_gen_state(r_gen_state(state, prefix: data))
  end

  defp update_gen_state(:inc_tag_pattern, state, data) do
    save_gen_state(r_gen_state(state, inc_tag_pattern: data))
  end

  defp update_gen_state(:tag_pattern, state, data) do
    save_gen_state(r_gen_state(state, tag_pattern: data))
  end

  defp update_gen_state(:inc_type_pattern, state, data) do
    save_gen_state(r_gen_state(state, inc_type_pattern: data))
  end

  defp update_gen_state(:type_pattern, state, data) do
    save_gen_state(r_gen_state(state, type_pattern: data))
  end

  defp update_gen_state(:func_name, state, data) do
    save_gen_state(r_gen_state(state, func_name: data))
  end

  defp update_gen_state(:namelist, state, data) do
    save_gen_state(r_gen_state(state, namelist: data))
  end

  defp update_gen_state(:tobe_refed_funcs, state, data) do
    save_gen_state(r_gen_state(state, tobe_refed_funcs: data))
  end

  defp update_gen_state(:gen_refed_funcs, state, data) do
    save_gen_state(r_gen_state(state, gen_refed_funcs: data))
  end

  defp update_gen_state(:generated_functions, state, data) do
    save_gen_state(r_gen_state(state, generated_functions: data))
  end

  defp update_gen_state(:suffix_index, state, data) do
    save_gen_state(r_gen_state(state, suffix_index: data))
  end

  defp update_gen_state(:current_suffix_index, state, data) do
    save_gen_state(r_gen_state(state, current_suffix_index: data))
  end

  def update_namelist(name) do
    case get_gen_state_field(:namelist) do
      [^name, rest] ->
        update_gen_state(:namelist, rest)

      [^name | rest] ->
        update_gen_state(:namelist, rest)

      [{^name, list}] when is_list(list) ->
        update_gen_state(:namelist, list)

      [{^name, atom} | rest] when is_atom(atom) ->
        update_gen_state(:namelist, rest)

      other ->
        other
    end
  end

  def step_in_constructed() do
    case get_gen_state_field(:namelist) do
      [l] when is_list(l) ->
        update_gen_state(:namelist, l)

      _ ->
        :ok
    end
  end

  def is_function_generated(name) do
    case get_gen_state_field(:gen_refed_funcs) do
      l when is_list(l) ->
        :lists.member(name, l)

      _ ->
        false
    end
  end

  def get_tobe_refed_func(name) do
    case get_gen_state_field(:tobe_refed_funcs) do
      l when is_list(l) ->
        case :lists.keysearch(name, 1, l) do
          {_, element} ->
            element

          _ ->
            :undefined
        end

      _ ->
        :undefined
    end
  end

  def add_tobe_refed_func(data) do
    {name, sI, pattern} =
      (fn
         {n, si, p, _} ->
           {n, si, p}

         d ->
           d
       end).(data)

    newData =
      case sI do
        i when is_integer(i) ->
          (fn d ->
             d
           end).(data)

        _ ->
          (fn
             {n, _, p} ->
               {n, 0, p}

             {n, _, p, t} ->
               {n, 0, p, t}
           end).(data)
      end

    l = get_gen_state_field(:generated_functions)

    case generated_functions_member(:erlang.get(:currmod), name, l, pattern) do
      true ->
        :ok

      _ ->
        add_once_tobe_refed_func(newData)
        maybe_rename_function(:tobe_refed, name, pattern)
    end
  end

  defp add_once_tobe_refed_func(data) do
    tRFL = get_gen_state_field(:tobe_refed_funcs)
    {name, index} = {:erlang.element(1, data), :erlang.element(2, data)}

    case :lists.filter(
           fn
             {n, i, _}
             when n == name and
                    i == index ->
               true

             {n, i, _, _} when n == name and i == index ->
               true

             _ ->
               false
           end,
           tRFL
         ) do
      [] ->
        update_gen_state(:tobe_refed_funcs, [data | tRFL])

      _ ->
        :ok
    end
  end

  def generated_refed_func(name) do
    l = get_gen_state_field(:tobe_refed_funcs)
    newL = :lists.keydelete(name, 1, l)
    update_gen_state(:tobe_refed_funcs, newL)
    l2 = get_gen_state_field(:gen_refed_funcs)
    update_gen_state(:gen_refed_funcs, [name | l2])
  end

  def add_generated_refed_func(data) do
    case is_function_generated(data) do
      true ->
        :ok

      _ ->
        l = get_gen_state_field(:gen_refed_funcs)
        update_gen_state(:gen_refed_funcs, [data | l])
    end
  end

  def next_refed_func() do
    case get_gen_state_field(:tobe_refed_funcs) do
      [] ->
        []

      [h | t] ->
        update_gen_state(:tobe_refed_funcs, t)
        h
    end
  end

  def reset_gen_state() do
    save_gen_state(r_gen_state())
  end

  defp add_generated_function(data) do
    l = get_gen_state_field(:generated_functions)
    update_gen_state(:generated_functions, [data | l])
  end

  def maybe_rename_function(mode, name, pattern) do
    case get_gen_state_field(:generated_functions) do
      [] when mode == :inc_disp ->
        add_generated_function({name, 0, pattern})
        name

      [] ->
        exit({:error, {:asn1, :internal_error_exclusive_decode}})

      l ->
        case {mode, generated_functions_member(:erlang.get(:currmod), name, l)} do
          {_, true} ->
            l2 = generated_functions_filter(:erlang.get(:currmod), name, l)

            case :lists.keysearch(pattern, 3, l2) do
              false ->
                nextIndex = length(l2)
                suffix = :lists.concat(['_', nextIndex])
                newName = maybe_rename_function2(type_check(name), name, suffix)
                add_generated_function({name, nextIndex, pattern})
                newName

              value ->
                suffix = make_suffix(value)

                name2 =
                  case name do
                    r_Externaltypereference(type: t) ->
                      t

                    _ ->
                      name
                  end

                :lists.concat([name2, suffix])
            end

          {:inc_disp, _} ->
            add_generated_function({name, 0, pattern})
            name

          _ ->
            add_generated_function({name, 0, pattern})
            name
        end
    end
  end

  defp maybe_rename_function2(:record, r_Externaltypereference(type: name), suffix) do
    :lists.concat([name, suffix])
  end

  defp maybe_rename_function2(:list, list, suffix) do
    :lists.concat([:asn1ct_gen.list2name(list), suffix])
  end

  defp maybe_rename_function2(thing, name, suffix)
       when thing == :atom or
              thing == :integer or thing == :string do
    :lists.concat([name, suffix])
  end

  defp generated_functions_member(m, name, l, pattern) do
    case generated_functions_member(m, name, l) do
      true ->
        l2 = generated_functions_filter(m, name, l)

        case :lists.keysearch(pattern, 3, l2) do
          {:value, _} ->
            true

          _ ->
            false
        end

      _ ->
        false
    end
  end

  defp generated_functions_member(_M, name, [{name, _, _} | _]) do
    true
  end

  defp generated_functions_member(m, r_Externaltypereference(module: m, type: t), [
         {r_Externaltypereference(module: m, type: t), _, _} | _
       ]) do
    true
  end

  defp generated_functions_member(m, r_Externaltypereference(module: m, type: name), [
         {name, _, _} | _
       ]) do
    true
  end

  defp generated_functions_member(m, name, [_ | t]) do
    generated_functions_member(m, name, t)
  end

  defp generated_functions_member(_, _, []) do
    false
  end

  defp generated_functions_filter(_, name, l)
       when is_atom(name) or
              is_list(name) do
    :lists.filter(
      fn
        {n, _, _} when n == name ->
          true

        _ ->
          false
      end,
      l
    )
  end

  defp generated_functions_filter(m, r_Externaltypereference(module: m, type: name), l) do
    removeTType = fn
      {n, i, [n, p]} when n == name ->
        {n, i, p}

      {r_Externaltypereference(module: m1, type: n), i, p} when m1 == m ->
        {n, i, p}

      p ->
        p
    end

    l2 = :lists.map(removeTType, l)
    generated_functions_filter(m, name, l2)
  end

  def maybe_saved_sindex(name, pattern) do
    case get_gen_state_field(:generated_functions) do
      [] ->
        false

      l ->
        case generated_functions_member(:erlang.get(:currmod), name, l) do
          true ->
            l2 = generated_functions_filter(:erlang.get(:currmod), name, l)

            case :lists.keysearch(pattern, 3, l2) do
              {:value, {_, i, _}} ->
                i

              _ ->
                length(l2)
            end

          _ ->
            false
        end
    end
  end

  def current_sindex() do
    get_gen_state_field(:current_suffix_index)
  end

  def set_current_sindex(index) do
    update_gen_state(:current_suffix_index, index)
  end

  defp type_check(a) when is_atom(a) do
    :atom
  end

  defp type_check(l) when is_list(l) do
    pred = fn
      x when x <= 255 ->
        false

      _ ->
        true
    end

    case :lists.filter(pred, l) do
      [] ->
        :string

      _ ->
        :list
    end
  end

  defp type_check(r_Externaltypereference()) do
    :record
  end

  defp make_suffix({_, {_, 0, _}}) do
    ''
  end

  defp make_suffix({_, {_, i, _}}) do
    :lists.concat(['_', i])
  end

  defp make_suffix(_) do
    ''
  end

  def error(format, args, s) do
    case is_error(s) do
      true ->
        :io.format(format, args)

      false ->
        :ok
    end
  end

  def warning(format, args, s) do
    case is_warning(s) do
      true ->
        :io.format('Warning: ' ++ format, args)

      false ->
        :ok
    end
  end

  def warning(format, args, s, reason) do
    case {is_werr(s), is_error(s), is_warning(s)} do
      {true, true, _} ->
        :io.format(format, args)
        throw({:error, reason})

      {false, _, true} ->
        :io.format(format, args)

      _ ->
        :ok
    end
  end

  def verbose(format, args, s) do
    case is_verbose(s) do
      true ->
        :io.format(format, args)

      false ->
        :ok
    end
  end

  def format_error({:write_error, file, reason}) do
    :io_lib.format('writing output file ~s failed: ~s', [file, :file.format_error(reason)])
  end

  defp is_error(r_state(options: opts)) do
    is_error(opts)
  end

  defp is_error(r_gen(options: opts)) do
    is_error(opts)
  end

  defp is_error(o) do
    :lists.member(:errors, o) or is_verbose(o)
  end

  defp is_warning(s) when elem(s, 0) === :state do
    is_warning(r_state(s, :options))
  end

  defp is_warning(o) do
    :lists.member(:warnings, o) or is_verbose(o)
  end

  defp is_verbose(r_state(options: opts)) do
    is_verbose(opts)
  end

  defp is_verbose(r_gen(options: opts)) do
    is_verbose(opts)
  end

  defp is_verbose(o) do
    :lists.member(:verbose, o)
  end

  defp is_werr(s) when elem(s, 0) === :state do
    is_werr(r_state(s, :options))
  end

  defp is_werr(o) do
    :lists.member(:warnings_as_errors, o)
  end

  defp in_process(fun) do
    parent = self()

    pid =
      spawn_link(fn ->
        process(parent, fun)
      end)

    receive do
      {^pid, result} ->
        result

      {^pid, class, reason, stack} ->
        sT =
          try do
            throw(:x)
          catch
            :x ->
              __STACKTRACE__
          end

        :erlang.raise(class, reason, stack ++ sT)
    end
  end

  defp process(parent, fun) do
    try do
      send(parent, {self(), fun.()})
    catch
      class, reason ->
        send(parent, {self(), class, reason, __STACKTRACE__})
    end
  end
end
