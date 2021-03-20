defmodule :m_compile do
  use Bitwise

  import :lists,
    only: [
      any: 2,
      flatmap: 2,
      flatten: 1,
      foldr: 3,
      foreach: 2,
      keyfind: 3,
      last: 1,
      map: 2,
      member: 2,
      reverse: 1,
      reverse: 2
    ]

  require Record

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

  Record.defrecord(:r_c_alias, :c_alias, anno: [], var: :undefined, pat: :undefined)
  Record.defrecord(:r_c_apply, :c_apply, anno: [], op: :undefined, args: :undefined)

  Record.defrecord(:r_c_binary, :c_binary,
    anno: [],
    segments: :undefined
  )

  Record.defrecord(:r_c_bitstr, :c_bitstr,
    anno: [],
    val: :undefined,
    size: :undefined,
    unit: :undefined,
    type: :undefined,
    flags: :undefined
  )

  Record.defrecord(:r_c_call, :c_call,
    anno: [],
    module: :undefined,
    name: :undefined,
    args: :undefined
  )

  Record.defrecord(:r_c_case, :c_case, anno: [], arg: :undefined, clauses: :undefined)

  Record.defrecord(:r_c_catch, :c_catch,
    anno: [],
    body: :undefined
  )

  Record.defrecord(:r_c_clause, :c_clause,
    anno: [],
    pats: :undefined,
    guard: :undefined,
    body: :undefined
  )

  Record.defrecord(:r_c_cons, :c_cons, anno: [], hd: :undefined, tl: :undefined)
  Record.defrecord(:r_c_fun, :c_fun, anno: [], vars: :undefined, body: :undefined)

  Record.defrecord(:r_c_let, :c_let, anno: [], vars: :undefined, arg: :undefined, body: :undefined)

  Record.defrecord(:r_c_letrec, :c_letrec, anno: [], defs: :undefined, body: :undefined)

  Record.defrecord(:r_c_literal, :c_literal,
    anno: [],
    val: :undefined
  )

  Record.defrecord(:r_c_map, :c_map,
    anno: [],
    arg: :EFE_TODO_NESTED_RECORD,
    es: :undefined,
    is_pat: false
  )

  Record.defrecord(:r_c_map_pair, :c_map_pair,
    anno: [],
    op: :undefined,
    key: :undefined,
    val: :undefined
  )

  Record.defrecord(:r_c_module, :c_module,
    anno: [],
    name: :undefined,
    exports: :undefined,
    attrs: :undefined,
    defs: :undefined
  )

  Record.defrecord(:r_c_primop, :c_primop, anno: [], name: :undefined, args: :undefined)

  Record.defrecord(:r_c_receive, :c_receive,
    anno: [],
    clauses: :undefined,
    timeout: :undefined,
    action: :undefined
  )

  Record.defrecord(:r_c_seq, :c_seq, anno: [], arg: :undefined, body: :undefined)

  Record.defrecord(:r_c_try, :c_try,
    anno: [],
    arg: :undefined,
    vars: :undefined,
    body: :undefined,
    evars: :undefined,
    handler: :undefined
  )

  Record.defrecord(:r_c_tuple, :c_tuple, anno: [], es: :undefined)

  Record.defrecord(:r_c_values, :c_values,
    anno: [],
    es: :undefined
  )

  Record.defrecord(:r_c_var, :c_var, anno: [], name: :undefined)

  def file(file) do
    file(file, [:verbose, :report_errors, :report_warnings])
  end

  def file(file, opts) when is_list(opts) do
    do_compile({:file, file}, opts ++ env_default_opts())
  end

  def file(file, opt) do
    file(
      file,
      [opt, :verbose, :report_errors, :report_warnings]
    )
  end

  def forms(forms) do
    forms(
      forms,
      [:verbose, :report_errors, :report_warnings]
    )
  end

  def forms(forms, opts) when is_list(opts) do
    do_compile(
      {:forms, forms},
      [:binary | opts ++ env_default_opts()]
    )
  end

  def forms(forms, opt) when is_atom(opt) do
    forms(
      forms,
      [opt, :verbose, :report_errors, :report_warnings]
    )
  end

  def output_generated(opts) do
    noenv_output_generated(opts ++ env_default_opts())
  end

  def noenv_file(file, opts) when is_list(opts) do
    do_compile({:file, file}, opts)
  end

  def noenv_file(file, opt) do
    noenv_file(
      file,
      [opt, :verbose, :report_errors, :report_warnings]
    )
  end

  def noenv_forms(forms, opts) when is_list(opts) do
    do_compile({:forms, forms}, [:binary | opts])
  end

  def noenv_forms(forms, opt) when is_atom(opt) do
    noenv_forms(
      forms,
      [opt, :verbose, :report_errors, :report_warnings]
    )
  end

  def noenv_output_generated(opts) do
    {_, passes} = passes(:file, expand_opts(opts))

    any(
      fn
        {:save_binary, _T, _F} ->
          true

        _Other ->
          false
      end,
      passes
    )
  end

  def env_compiler_options() do
    env_default_opts()
  end

  def run_sub_passes(ps, st) do
    case :erlang.get(:compile__sub_pass_times) do
      :undefined ->
        runner = fn _Name, run, s ->
          run.(s)
        end

        run_sub_passes_1(ps, runner, st)

      times when is_list(times) ->
        runner = fn name, run, s0 ->
          t1 = :erlang.monotonic_time()
          s = run.(s0)
          t2 = :erlang.monotonic_time()

          :erlang.put(
            :compile__sub_pass_times,
            [
              {name, t2 - t1}
              | :erlang.get(:compile__sub_pass_times)
            ]
          )

          s
        end

        run_sub_passes_1(ps, runner, st)
    end
  end

  defp env_default_opts() do
    key = 'ERL_COMPILER_OPTIONS'

    case :os.getenv(key) do
      false ->
        []

      str when is_list(str) ->
        case :erl_scan.string(str) do
          {:ok, tokens, _} ->
            dot = {:dot, :erl_anno.new(1)}

            case :erl_parse.parse_term(tokens ++ [dot]) do
              {:ok, list} when is_list(list) ->
                list

              {:ok, term} ->
                [term]

              {:error, _Reason} ->
                :io.format('Ignoring bad term in ~s\n', [key])
                []
            end

          {:error, {_, _, _Reason}, _} ->
            :io.format('Ignoring bad term in ~s\n', [key])
            []
        end
    end
  end

  defp do_compile(input, opts0) do
    opts = expand_opts(opts0)

    intFun = fn ->
      try do
        internal(input, opts)
      catch
        :error, reason ->
          {:error, reason}
      end
    end

    case :lists.member(
           :no_spawn_compiler_process,
           opts
         ) do
      true ->
        intFun.()

      false ->
        {pid, ref} =
          spawn_monitor(fn ->
            exit(intFun.())
          end)

        receive do
          {:DOWN, ^ref, :process, ^pid, rep} ->
            rep
        end
    end
  end

  defp expand_opts(opts0) do
    opts =
      case {:proplists.get_value(
              :debug_info_key,
              opts0
            ), :proplists.get_value(:encrypt_debug_info, opts0),
            :proplists.get_value(:debug_info, opts0)} do
        {:undefined, :undefined, _} ->
          opts0

        {_, _, :undefined} ->
          [:debug_info | opts0]

        {_, _, _} ->
          opts0
      end

    opts1 =
      case :proplists.is_defined(
             :makedep_side_effect,
             opts
           ) do
        true ->
          :proplists.delete(:makedep, opts)

        false ->
          opts
      end

    foldr(&expand_opt/2, [], opts1)
  end

  defp expand_opt(:basic_validation, os) do
    [:no_code_generation, :to_pp, :binary | os]
  end

  defp expand_opt(:strong_validation, os) do
    [:no_code_generation, :to_kernel, :binary | os]
  end

  defp expand_opt(:report, os) do
    [:report_errors, :report_warnings | os]
  end

  defp expand_opt(:return, os) do
    [:return_errors, :return_warnings | os]
  end

  defp expand_opt(:no_bsm3, os) do
    [:no_bsm3, :no_bsm_opt | expand_opt(:no_bsm4, os)]
  end

  defp expand_opt(:no_bsm4, os) do
    expand_opt(:no_type_opt, os)
  end

  defp expand_opt(:r18, os) do
    expand_opt_before_21(os)
  end

  defp expand_opt(:r19, os) do
    expand_opt_before_21(os)
  end

  defp expand_opt(:r20, os) do
    expand_opt_before_21(os)
  end

  defp expand_opt(:r21, os) do
    expand_opt(
      :r22,
      [:no_put_tuple2 | expand_opt(:no_bsm3, os)]
    )
  end

  defp expand_opt(:r22, os) do
    expand_opt(
      :r23,
      [
        :no_shared_fun_wrappers,
        :no_swap
        | expand_opt(:no_bsm4, os)
      ]
    )
  end

  defp expand_opt(:r23, os) do
    expand_opt(:no_make_fun3, [:no_init_yregs | os])
  end

  defp expand_opt(:no_make_fun3, os) do
    [:no_make_fun3, :no_fun_opt | os]
  end

  defp expand_opt({:debug_info_key, _} = o, os) do
    [:encrypt_debug_info, o | os]
  end

  defp expand_opt(:no_type_opt = o, os) do
    [o, :no_ssa_opt_type_start, :no_ssa_opt_type_continue, :no_ssa_opt_type_finish | os]
  end

  defp expand_opt(o, os) do
    [o | os]
  end

  defp expand_opt_before_21(os) do
    [
      :no_init_yregs,
      :no_make_fun3,
      :no_fun_opt,
      :no_shared_fun_wrappers,
      :no_swap,
      :no_put_tuple2,
      :no_get_hd_tl,
      :no_ssa_opt_record,
      :no_utf8_atoms
      | expand_opt(
          :no_bsm3,
          os
        )
    ]
  end

  def format_error(:no_native_support) do
    'this system is not configured for native-code compilation.'
  end

  def format_error(:no_crypto) do
    'this system is not configured with crypto support.'
  end

  def format_error(:bad_crypto_key) do
    'invalid crypto key.'
  end

  def format_error(:no_crypto_key) do
    'no crypto key supplied.'
  end

  def format_error({:unimplemented_instruction, instruction}) do
    :io_lib.fwrite(
      'native-code compilation failed because of an unimplemented instruction (~s).',
      [instruction]
    )
  end

  def format_error({:native, e}) do
    :io_lib.fwrite('native-code compilation failed with reason: ~tP.', [e, 25])
  end

  def format_error({:native_crash, e, stk}) do
    :io_lib.fwrite('native-code compilation crashed with reason: ~tP.\n~tP\n', [e, 25, stk, 25])
  end

  def format_error({:open, e}) do
    :io_lib.format('open error \'~ts\'', [:file.format_error(e)])
  end

  def format_error({:epp, e}) do
    :epp.format_error(e)
  end

  def format_error(:write_error) do
    'error writing file'
  end

  def format_error({:write_error, error}) do
    :io_lib.format('error writing file: ~ts', [:file.format_error(error)])
  end

  def format_error({:rename, from, to, error}) do
    :io_lib.format('failed to rename ~ts to ~ts: ~ts', [from, to, :file.format_error(error)])
  end

  def format_error({:delete, file, error}) do
    :io_lib.format('failed to delete file ~ts: ~ts', [file, :file.format_error(error)])
  end

  def format_error({:delete_temp, file, error}) do
    :io_lib.format('failed to delete temporary file ~ts: ~ts', [file, :file.format_error(error)])
  end

  def format_error({:parse_transform, m, r}) do
    :io_lib.format('error in parse transform \'~ts\': ~tp', [m, r])
  end

  def format_error({:undef_parse_transform, m}) do
    :io_lib.format('undefined parse transform \'~ts\'', [m])
  end

  def format_error({:core_transform, m, r}) do
    :io_lib.format('error in core transform \'~s\': ~tp', [m, r])
  end

  def format_error({:crash, pass, reason}) do
    :io_lib.format('internal error in ~p;\ncrash reason: ~ts', [pass, format_error_reason(reason)])
  end

  def format_error({:bad_return, pass, reason}) do
    :io_lib.format('internal error in ~p;\nbad return value: ~ts', [
      pass,
      format_error_reason(reason)
    ])
  end

  def format_error({:module_name, mod, filename}) do
    :io_lib.format('Module name \'~s\' does not match file name \'~ts\'', [mod, filename])
  end

  def format_error(:reparsing_invalid_unicode) do
    'Non-UTF-8 character(s) detected, but no encoding declared. Encode the file in UTF-8 or add "%% coding: latin-1" at the beginning of the file. Note: The compiler will remove support for latin-1 encoded source files without the "%% coding: latin-1" string at the beginning of the file in Erlang/OTP 24! Retrying with latin-1 encoding.'
  end

  defp format_error_reason({reason, stack}) when is_list(stack) do
    stackFun = fn
      :escript, :run, 2 ->
        true

      :escript, :start, 1 ->
        true

      :init, :start_it, 1 ->
        true

      :init, :start_em, 1 ->
        true

      _Mod, _Fun, _Arity ->
        false
    end

    formatFun = fn term, _ ->
      :io_lib.format('~tp', [term])
    end

    [
      :io_lib.format('~tp', [reason]),
      '\n\n',
      :erl_error.format_stacktrace(1, stack, stackFun, formatFun)
    ]
  end

  defp format_error_reason(reason) do
    :io_lib.format('~tp', [reason])
  end

  Record.defrecord(:r_compile, :compile,
    filename: '',
    dir: '',
    base: '',
    ifile: '',
    ofile: '',
    module: [],
    core_code: [],
    abstract_code: [],
    options: [],
    mod_options: [],
    encoding: :none,
    errors: [],
    warnings: [],
    extra_chunks: []
  )

  defp internal({:forms, forms}, opts0) do
    {_, ps} = passes(:forms, opts0)
    source = :proplists.get_value(:source, opts0, '')
    opts1 = :proplists.delete(:source, opts0)
    compile = build_compile(opts1)
    internal_comp(ps, forms, source, '', compile)
  end

  defp internal({:file, file}, opts) do
    {ext, ps} = passes(:file, opts)
    compile = build_compile(opts)
    internal_comp(ps, :none, file, ext, compile)
  end

  defp build_compile(opts0) do
    extraChunks = :proplists.get_value(:extra_chunks, opts0, [])
    opts1 = :proplists.delete(:extra_chunks, opts0)
    r_compile(options: opts1, mod_options: opts1, extra_chunks: extraChunks)
  end

  defp internal_comp(passes, code0, file, suffix, st0) do
    dir = :filename.dirname(file)
    base = :filename.basename(file, suffix)

    st1 =
      r_compile(st0,
        filename: file,
        dir: dir,
        base: base,
        ifile: erlfile(dir, base, suffix),
        ofile: objfile(base, st0)
      )

    opts = r_compile(st1, :options)

    run0 =
      case member(:time, opts) do
        true ->
          :io.format('Compiling ~tp\n', [file])
          &run_tc/3

        false ->
          fn {_Name, fun}, code, st ->
            try do
              fun.(code, st)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end
          end
      end

    run =
      case keyfind(:eprof, 1, opts) do
        {:eprof, eprofPass} ->
          fn p, code, st ->
            run_eprof(p, code, eprofPass, st)
          end

        false ->
          run0
      end

    case fold_comp(passes, run, code0, st1) do
      {:ok, code, st2} ->
        comp_ret_ok(code, st2)

      {:error, st2} ->
        comp_ret_err(st2)
    end
  end

  defp fold_comp([{:delay, ps0} | passes], run, code, r_compile(options: opts) = st) do
    ps = select_passes(ps0, opts) ++ passes
    fold_comp(ps, run, code, st)
  end

  defp fold_comp([{name, test, pass} | ps], run, code, st) do
    case test.(st) do
      false ->
        fold_comp(ps, run, code, st)

      true ->
        fold_comp([{name, pass} | ps], run, code, st)
    end
  end

  defp fold_comp([{name, pass} | ps], run, code0, st0) do
    case run.({name, pass}, code0, st0) do
      {:ok, code, st1} ->
        fold_comp(ps, run, code, st1)

      {:error, _St1} = error ->
        error

      {:EXIT, reason} ->
        es = [{r_compile(st0, :ifile), [{:none, :compile, {:crash, name, reason}}]}]
        {:error, r_compile(st0, errors: r_compile(st0, :errors) ++ es)}

      other ->
        es = [{r_compile(st0, :ifile), [{:none, :compile, {:bad_return, name, other}}]}]
        {:error, r_compile(st0, errors: r_compile(st0, :errors) ++ es)}
    end
  end

  defp fold_comp([], _Run, code, st) do
    {:ok, code, st}
  end

  defp run_sub_passes_1([{name, run} | ps], runner, st0)
       when is_atom(name) and is_function(run, 1) do
    try do
      runner.(name, run, st0)
    catch
      c, e ->
        :io.format('Sub pass ~s\n', [name])
        :erlang.raise(c, e, __STACKTRACE__)
    else
      st ->
        run_sub_passes_1(ps, runner, st)
    end
  end

  defp run_sub_passes_1([], _, st) do
    st
  end

  defp run_tc({name, fun}, code, st) do
    oldTimes = :erlang.put(:compile__sub_pass_times, [])
    t1 = :erlang.monotonic_time()

    val =
      try do
        fun.(code, st)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    t2 = :erlang.monotonic_time()
    times = :erlang.get(:compile__sub_pass_times)

    case oldTimes do
      :undefined ->
        :erlang.erase(:compile__sub_pass_times)

      _ ->
        :erlang.put(:compile__sub_pass_times, oldTimes)
    end

    elapsed = :erlang.convert_time_unit(t2 - t1, :native, :microsecond)
    mem0 = :erts_debug.flat_size(val) * :erlang.system_info(:wordsize)
    mem = :lists.flatten(:io_lib.format('~.1f kB', [mem0 / 1024]))
    :io.format(' ~-30s: ~10.3f s ~12s\n', [name, elapsed / 1_000_000, mem])
    print_times(times, name)
    val
  end

  defp print_times(times0, name) do
    fam0 = :sofs.relation(times0)
    fam1 = :sofs.rel2fam(fam0)
    fam2 = :sofs.to_external(fam1)

    fam3 =
      for {w, times} <- fam2 do
        {w, :lists.sum(times)}
      end

    fam = reverse(:lists.keysort(2, fam3))

    total =
      case :lists.sum(
             for {_, t} <- fam do
               t
             end
           ) do
        0 ->
          1

        total0 ->
          total0
      end

    case fam do
      [] ->
        :ok

      [_ | _] ->
        :io.format('    %% Sub passes of ~s from slowest to fastest:\n', [name])
        print_times_1(fam, total)
    end
  end

  defp print_times_1([{name, t} | ts], total) do
    elapsed = :erlang.convert_time_unit(t, :native, :microsecond)

    :io.format(
      '    ~-27s: ~10.3f s ~3w %\n',
      [name, elapsed / 1_000_000, round(100 * t / total)]
    )

    print_times_1(ts, total)
  end

  defp print_times_1([], _Total) do
    :ok
  end

  defp run_eprof({name, fun}, code, name, st) do
    :io.format('~p: Running eprof\n', [name])
    :c.appcall(:tools, :eprof, :start_profiling, [[self()]])

    val =
      try do
        fun.(code, st)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    :c.appcall(:tools, :eprof, :stop_profiling, [])
    :c.appcall(:tools, :eprof, :analyze, [])
    val
  end

  defp run_eprof({_, fun}, code, _, st) do
    try do
      fun.(code, st)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp comp_ret_ok(
         code,
         r_compile(warnings: warn0, module: mod, options: opts) = st
       ) do
    case werror(st) do
      true ->
        case member(:report_warnings, opts) do
          true ->
            :io.format('~p: warnings being treated as errors\n', [:compile])

          false ->
            :ok
        end

        comp_ret_err(st)

      false ->
        warn = messages_per_file(warn0)
        report_warnings(r_compile(st, warnings: warn))

        ret1 =
          case member(:binary, opts) and
                 not member(
                   :no_code_generation,
                   opts
                 ) do
            true ->
              [code]

            false ->
              []
          end

        ret2 =
          case member(:return_warnings, opts) do
            true ->
              ret1 ++ [warn]

            false ->
              ret1
          end

        :erlang.list_to_tuple([:ok, mod | ret2])
    end
  end

  defp comp_ret_err(r_compile(warnings: warn0, errors: err0, options: opts) = st) do
    warn = messages_per_file(warn0)
    err = messages_per_file(err0)
    report_errors(r_compile(st, errors: err))
    report_warnings(r_compile(st, warnings: warn))

    case member(:return_errors, opts) do
      true ->
        {:error, err, warn}

      false ->
        :error
    end
  end

  defp not_werror(st) do
    not werror(st)
  end

  defp werror(r_compile(options: opts, warnings: ws)) do
    ws !== [] and member(:warnings_as_errors, opts)
  end

  defp messages_per_file(ms) do
    t =
      :lists.sort(
        for {file, messages} <- ms,
            m <- messages do
          {file, m}
        end
      )

    prioMs = [:erl_scan, :epp, :erl_parse]

    {prio0, rest} =
      :lists.mapfoldl(
        fn m, a ->
          :lists.partition(
            fn
              {_, {_, mod, _}} ->
                mod === m

              _ ->
                false
            end,
            a
          )
        end,
        t,
        prioMs
      )

    prio =
      :lists.sort(
        fn {_, {l1, _, _}}, {_, {l2, _, _}} ->
          l1 <= l2
        end,
        :lists.append(prio0)
      )

    flatmap(&mpf/1, [prio, rest])
  end

  defp mpf(ms) do
    for file <-
          :lists.usort(
            for {f, _} <- ms do
              f
            end
          ) do
      {file,
       for {f, m} <- ms, f === file do
         m
       end}
    end
  end

  defp passes(type, opts) do
    {ext, passes0} = passes_1(opts)

    passes1 =
      case type do
        :file ->
          passes0

        :forms ->
          fix_first_pass(passes0)
      end

    passes = select_passes(passes1, opts)

    {ext,
     case last(passes) do
       {:save_binary, _TestFun, _Fun} ->
         case passes do
           [{:read_beam_file, _} | _] ->
             passes

           _ ->
             [{:remove_file, &remove_file/2} | passes]
         end

       _ ->
         passes
     end}
  end

  defp passes_1([opt | opts]) do
    case pass(opt) do
      {_, _} = res ->
        res

      :none ->
        passes_1(opts)
    end
  end

  defp passes_1([]) do
    {'.erl', [{:parse_module, &parse_module/2} | standard_passes()]}
  end

  defp pass(:from_core) do
    {'.core',
     [
       {:parse_core, &parse_core/2}
       | core_passes(:non_verified_core)
     ]}
  end

  defp pass(:from_asm) do
    {'.S',
     [
       {:beam_consult_asm, &beam_consult_asm/2}
       | asm_passes()
     ]}
  end

  defp pass(:from_beam) do
    {'.beam',
     [
       {:read_beam_file, &read_beam_file/2}
       | binary_passes()
     ]}
  end

  defp pass(_) do
    :none
  end

  defp fix_first_pass([{:parse_core, _} | passes]) do
    [
      {:get_module_name_from_core, &get_module_name_from_core/2}
      | passes
    ]
  end

  defp fix_first_pass([{:beam_consult_asm, _} | passes]) do
    [
      {:get_module_name_from_asm, &get_module_name_from_asm/2}
      | passes
    ]
  end

  defp fix_first_pass([{:read_beam_file, _} | passes]) do
    [
      {:get_module_name_from_beam, &get_module_name_from_beam/2}
      | passes
    ]
  end

  defp fix_first_pass([_ | passes]) do
    passes
  end

  defp select_passes([{:pass, mod} | ps], opts) do
    f = fn code0, st ->
      case (try do
              mod.module(code0, r_compile(st, :options))
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:ok, code} ->
          {:ok, code, st}

        {:ok, code, ws} ->
          {:ok, code, r_compile(st, warnings: r_compile(st, :warnings) ++ ws)}

        {:error, es} ->
          {:error, r_compile(st, errors: r_compile(st, :errors) ++ es)}
      end
    end

    [{mod, f} | select_passes(ps, opts)]
  end

  defp select_passes([{:src_listing, ext} | _], _Opts) do
    [
      {:listing,
       fn code, st ->
         src_listing(ext, code, st)
       end}
    ]
  end

  defp select_passes([{:listing, ext} | _], _Opts) do
    [
      {:listing,
       fn code, st ->
         listing(ext, code, st)
       end}
    ]
  end

  defp select_passes([:done | _], _Opts) do
    []
  end

  defp select_passes([{:done, ext} | _], opts) do
    select_passes(
      [{:unless, :binary, {:listing, ext}}],
      opts
    )
  end

  defp select_passes([{:iff, flag, pass} | ps], opts) do
    select_cond(flag, true, pass, ps, opts)
  end

  defp select_passes([{:unless, flag, pass} | ps], opts) do
    select_cond(flag, false, pass, ps, opts)
  end

  defp select_passes([{_, fun} = p | ps], opts)
       when is_function(fun) do
    [p | select_passes(ps, opts)]
  end

  defp select_passes([{:delay, passes0} | ps], opts)
       when is_list(passes0) do
    case select_list_passes(passes0, opts) do
      {:done, passes} ->
        [{:delay, passes}]

      {:not_done, passes} ->
        [{:delay, passes} | select_passes(ps, opts)]
    end
  end

  defp select_passes([{_, test, fun} = p | ps], opts)
       when is_function(test) and is_function(fun) do
    [p | select_passes(ps, opts)]
  end

  defp select_passes([], _Opts) do
    []
  end

  defp select_passes([list | ps], opts) when is_list(list) do
    case select_passes(list, opts) do
      [] ->
        select_passes(ps, opts)

      nested ->
        case last(nested) do
          {:listing, _Fun} ->
            nested

          _Other ->
            nested ++ select_passes(ps, opts)
        end
    end
  end

  defp select_cond(flag, shouldBe, pass, ps, opts) do
    shouldNotBe = not shouldBe

    case member(flag, opts) do
      ^shouldBe ->
        select_passes([pass | ps], opts)

      ^shouldNotBe ->
        select_passes(ps, opts)
    end
  end

  defp select_list_passes(ps, opts) do
    select_list_passes_1(ps, opts, [])
  end

  defp select_list_passes_1([{:iff, flag, {:listing, _} = listing} | ps], opts, acc) do
    case member(flag, opts) do
      true ->
        {:done, reverse(acc, [listing])}

      false ->
        select_list_passes_1(ps, opts, acc)
    end
  end

  defp select_list_passes_1([{:iff, flag, {:done, ext}} | ps], opts, acc) do
    case member(flag, opts) do
      false ->
        select_list_passes_1(ps, opts, acc)

      true ->
        {:done,
         case member(:binary, opts) do
           false ->
             reverse(acc, [{:listing, ext}])

           true ->
             reverse(acc)
         end}
    end
  end

  defp select_list_passes_1([{:iff = op, flag, list0} | ps], opts, acc)
       when is_list(list0) do
    case select_list_passes(list0, opts) do
      {:done, list} ->
        {:done, reverse(acc) ++ list}

      {:not_done, list} ->
        select_list_passes_1(ps, opts, [{op, flag, list} | acc])
    end
  end

  defp select_list_passes_1([{:unless = op, flag, list0} | ps], opts, acc)
       when is_list(list0) do
    case select_list_passes(list0, opts) do
      {:done, list} ->
        {:done, reverse(acc) ++ list}

      {:not_done, list} ->
        select_list_passes_1(ps, opts, [{op, flag, list} | acc])
    end
  end

  defp select_list_passes_1([p | ps], opts, acc) do
    select_list_passes_1(ps, opts, [p | acc])
  end

  defp select_list_passes_1([], _, acc) do
    {:not_done, reverse(acc)}
  end

  defp standard_passes() do
    [
      {:transform_module, &transform_module/2},
      {:iff, :makedep_side_effect, {:makedep_and_output, &makedep_and_output/2}},
      {:iff, :makedep,
       [{:makedep, &makedep/2}, {:unless, :binary, {:makedep_output, &makedep_output/2}}]},
      {:iff, :makedep, :done},
      {:iff, :dpp, {:listing, 'pp'}},
      {:lint_module, &lint_module/2},
      {:compile_directives, &compile_directives/2},
      {:iff, :P, {:src_listing, 'P'}},
      {:iff, :to_pp, {:done, 'P'}},
      {:iff, :dabstr, {:listing, 'abstr'}},
      {:delay, [{:iff, :debug_info, {:save_abstract_code, &save_abstract_code/2}}]},
      {:expand_records, &expand_records/2},
      {:iff, :dexp, {:listing, 'expand'}},
      {:iff, :E, {:src_listing, 'E'}},
      {:iff, :to_exp, {:done, 'E'}},
      {:core, &core/2},
      {:iff, :dcore, {:listing, 'core'}},
      {:iff, :to_core0, {:done, 'core'}}
      | core_passes(:verified_core)
    ]
  end

  defp core_passes(coreStatus) do
    case coreStatus do
      :non_verified_core ->
        [
          {:core_lint_module, &core_lint_module/2},
          {:unless, :no_core_prepare, {:pass, :sys_core_prepare}},
          {:iff, :dprep, {:listing, 'prepare'}}
        ]

      :verified_core ->
        [{:iff, :clint0, {:core_lint_module, &core_lint_module/2}}]
    end ++
      [
        {:delay,
         [
           {:unless, :no_copt,
            [
              {:core_old_inliner, &test_old_inliner/1, &core_old_inliner/2},
              {:iff, :doldinline, {:listing, 'oldinline'}},
              {:unless, :no_fold, {:pass, :sys_core_fold}},
              {:iff, :dcorefold, {:listing, 'corefold'}},
              {:core_inline_module, &test_core_inliner/1, &core_inline_module/2},
              {:iff, :dinline, {:listing, 'inline'}},
              {:core_fold_after_inlining, &test_any_inliner/1,
               &core_fold_module_after_inlining/2},
              {:iff, :dcopt, {:listing, 'copt'}},
              {:unless, :no_alias, {:pass, :sys_core_alias}},
              {:iff, :dalias, {:listing, 'core_alias'}},
              {:core_transforms, &core_transforms/2}
            ]},
           {:iff, :to_core, {:done, 'core'}}
         ]}
        | kernel_passes()
      ]
  end

  defp kernel_passes() do
    [
      {:pass, :sys_core_bsm},
      {:iff, :dcbsm, {:listing, 'core_bsm'}},
      {:iff, :clint, {:core_lint_module, &core_lint_module/2}},
      {:iff, :core, {:save_core_code, &save_core_code/2}},
      {:v3_kernel, &v3_kernel/2},
      {:iff, :dkern, {:listing, 'kernel'}},
      {:iff, :to_kernel, {:done, 'kernel'}},
      {:pass, :beam_kernel_to_ssa},
      {:iff, :dssa, {:listing, 'ssa'}},
      {:iff, :ssalint, {:pass, :beam_ssa_lint}},
      {:delay,
       [
         {:unless, :no_bool_opt, {:pass, :beam_ssa_bool}},
         {:iff, :dbool, {:listing, 'bool'}},
         {:unless, :no_bool_opt, {:iff, :ssalint, {:pass, :beam_ssa_lint}}},
         {:unless, :no_share_opt, {:pass, :beam_ssa_share}},
         {:iff, :dssashare, {:listing, 'ssashare'}},
         {:unless, :no_share_opt, {:iff, :ssalint, {:pass, :beam_ssa_lint}}},
         {:unless, :no_bsm_opt, {:pass, :beam_ssa_bsm}},
         {:iff, :dssabsm, {:listing, 'ssabsm'}},
         {:unless, :no_bsm_opt, {:iff, :ssalint, {:pass, :beam_ssa_lint}}},
         {:unless, :no_fun_opt, {:pass, :beam_ssa_funs}},
         {:iff, :dssafuns, {:listing, 'ssafuns'}},
         {:unless, :no_fun_opt, {:iff, :ssalint, {:pass, :beam_ssa_lint}}},
         {:unless, :no_ssa_opt, {:pass, :beam_ssa_opt}},
         {:iff, :dssaopt, {:listing, 'ssaopt'}},
         {:unless, :no_ssa_opt, {:iff, :ssalint, {:pass, :beam_ssa_lint}}},
         {:unless, :no_throw_opt, {:pass, :beam_ssa_throw}},
         {:iff, :dthrow, {:listing, 'throw'}},
         {:unless, :no_throw_opt, {:iff, :ssalint, {:pass, :beam_ssa_lint}}},
         {:unless, :no_recv_opt, {:pass, :beam_ssa_recv}},
         {:iff, :drecv, {:listing, 'recv'}},
         {:unless, :no_recv_opt, {:iff, :ssalint, {:pass, :beam_ssa_lint}}}
       ]},
      {:pass, :beam_ssa_pre_codegen},
      {:iff, :dprecg, {:listing, 'precodegen'}},
      {:iff, :ssalint, {:pass, :beam_ssa_lint}},
      {:pass, :beam_ssa_codegen},
      {:iff, :dcg, {:listing, 'codegen'}},
      {:iff, :doldcg, {:listing, 'codegen'}},
      {:beam_validator_strong, &beam_validator_strong/2}
      | asm_passes()
    ]
  end

  defp asm_passes() do
    [
      {:delay,
       [
         {:pass, :beam_a},
         {:iff, :da, {:listing, 'a'}},
         {:unless, :no_postopt,
          [
            {:pass, :beam_block},
            {:iff, :dblk, {:listing, 'block'}},
            {:unless, :no_jopt, {:pass, :beam_jump}},
            {:iff, :djmp, {:listing, 'jump'}},
            {:unless, :no_peep_opt, {:pass, :beam_peep}},
            {:iff, :dpeep, {:listing, 'peep'}},
            {:pass, :beam_clean},
            {:iff, :dclean, {:listing, 'clean'}},
            {:unless, :no_stack_trimming, {:pass, :beam_trim}},
            {:iff, :dtrim, {:listing, 'trim'}},
            {:pass, :beam_flatten}
          ]},
         {:iff, :no_postopt, [{:pass, :beam_clean}]},
         {:iff, :diffable, {:diffable, &diffable/2}},
         {:pass, :beam_z},
         {:iff, :diffable, {:listing, 'S'}},
         {:iff, :dz, {:listing, 'z'}},
         {:iff, :dopt, {:listing, 'optimize'}},
         {:iff, :S, {:listing, 'S'}},
         {:iff, :to_asm, {:done, 'S'}}
       ]},
      {:beam_validator_weak, &beam_validator_weak/2},
      {:beam_asm, &beam_asm/2} | binary_passes()
    ]
  end

  defp binary_passes() do
    [
      {:iff, :to_dis, {:to_dis, &to_dis/2}},
      {:native_compile, &test_native/1, &native_compile/2},
      {:unless, :binary, {:save_binary, &not_werror/1, &save_binary/2}}
    ]
  end

  defp remove_file(code, st) do
    _ = :file.delete(r_compile(st, :ofile))
    {:ok, code, st}
  end

  Record.defrecord(:r_asm_module, :asm_module,
    module: :undefined,
    exports: :undefined,
    labels: :undefined,
    functions: [],
    attributes: []
  )

  defp preprocess_asm_forms(forms) do
    r = r_asm_module()
    r1 = collect_asm(forms, r)

    {r_asm_module(r1, :module),
     {r_asm_module(r1, :module), r_asm_module(r1, :exports), r_asm_module(r1, :attributes),
      reverse(r_asm_module(r1, :functions)), r_asm_module(r1, :labels)}}
  end

  defp collect_asm([{:module, m} | rest], r) do
    collect_asm(rest, r_asm_module(r, module: m))
  end

  defp collect_asm([{:exports, m} | rest], r) do
    collect_asm(rest, r_asm_module(r, exports: m))
  end

  defp collect_asm([{:labels, m} | rest], r) do
    collect_asm(rest, r_asm_module(r, labels: m))
  end

  defp collect_asm([{:function, a, b, c} | rest0], r0) do
    {code, rest} = collect_asm_function(rest0, [])
    func = {:function, a, b, c, code}
    r = r_asm_module(r0, functions: [func | r_asm_module(r0, :functions)])
    collect_asm(rest, r)
  end

  defp collect_asm([{:attributes, attr} | rest], r) do
    collect_asm(rest, r_asm_module(r, attributes: attr))
  end

  defp collect_asm([], r) do
    r
  end

  defp collect_asm_function([{:function, _, _, _} | _] = is, acc) do
    {reverse(acc), is}
  end

  defp collect_asm_function([i | is], acc) do
    collect_asm_function(is, [i | acc])
  end

  defp collect_asm_function([], acc) do
    {reverse(acc), []}
  end

  defp beam_consult_asm(_Code, st) do
    case :file.consult(r_compile(st, :ifile)) do
      {:ok, forms0} ->
        encoding = :epp.read_encoding(r_compile(st, :ifile))
        {module, forms} = preprocess_asm_forms(forms0)
        {:ok, forms, r_compile(st, module: module, encoding: encoding)}

      {:error, e} ->
        es = [{r_compile(st, :ifile), [{:none, :compile, {:open, e}}]}]
        {:error, r_compile(st, errors: r_compile(st, :errors) ++ es)}
    end
  end

  defp get_module_name_from_asm({mod, _, _, _, _} = asm, st) do
    {:ok, asm, r_compile(st, module: mod)}
  end

  defp get_module_name_from_asm(asm, st) do
    {:ok, asm, st}
  end

  defp read_beam_file(_Code, st) do
    case :file.read_file(r_compile(st, :ifile)) do
      {:ok, beam} ->
        infile = r_compile(st, :ifile)

        case no_native_compilation(infile, st) do
          true ->
            {:ok, :none, r_compile(st, module: :none)}

          false ->
            mod0 = :filename.rootname(:filename.basename(infile))
            mod = :erlang.list_to_atom(mod0)
            {:ok, beam, r_compile(st, module: mod, ofile: infile)}
        end

      {:error, e} ->
        es = [{r_compile(st, :ifile), [{:none, :compile, {:open, e}}]}]
        {:error, r_compile(st, errors: r_compile(st, :errors) ++ es)}
    end
  end

  defp get_module_name_from_beam(beam, st) do
    case :beam_lib.info(beam) do
      {:error, :beam_lib, error} ->
        es = [{'((forms))', [{:none, :beam_lib, error}]}]
        {:error, r_compile(st, errors: r_compile(st, :errors) ++ es)}

      info ->
        {:module, mod} = keyfind(:module, 1, info)
        {:ok, beam, r_compile(st, module: mod)}
    end
  end

  defp no_native_compilation(beamFile, r_compile(options: opts0)) do
    case :beam_lib.chunks(beamFile, ['CInf']) do
      {:ok, {_, [{'CInf', term0}]}} ->
        term = :erlang.binary_to_term(term0)
        opts = :proplists.get_value(:options, term, []) ++ opts0
        member(:no_new_funs, opts) or not is_native_enabled(opts)

      _ ->
        false
    end
  end

  defp parse_module(_Code, st0) do
    case do_parse_module(:utf8, st0) do
      {:ok, _, _} = ret ->
        ret

      {:error, _} = ret ->
        ret

      {:invalid_unicode, file, line} ->
        case do_parse_module(:latin1, st0) do
          {:ok, code, st} ->
            es = [{file, [{line, :compile, :reparsing_invalid_unicode}]}]
            {:ok, code, r_compile(st, warnings: es ++ r_compile(st, :warnings))}

          {:error, st} ->
            es = [{file, [{line, :compile, :reparsing_invalid_unicode}]}]
            {:error, r_compile(st, errors: es ++ r_compile(st, :errors))}
        end
    end
  end

  defp do_parse_module(
         defEncoding,
         r_compile(ifile: file, options: opts, dir: dir) = st
       ) do
    sourceName0 = :proplists.get_value(:source, opts, file)

    sourceName =
      case member(:deterministic, opts) do
        true ->
          :filename.basename(sourceName0)

        false ->
          sourceName0
      end

    r =
      :epp.parse_file(
        file,
        [
          {:includes, ['.', dir | inc_paths(opts)]},
          {:source_name, sourceName},
          {:macros, pre_defs(opts)},
          {:default_encoding, defEncoding},
          :extra
        ]
      )

    case r do
      {:ok, forms, extra} ->
        encoding = :proplists.get_value(:encoding, extra)

        case find_invalid_unicode(forms, file) do
          :none ->
            {:ok, forms, r_compile(st, encoding: encoding)}

          {:invalid_unicode, _, _} = ret ->
            case encoding do
              :none ->
                ret

              _ ->
                {:ok, forms, r_compile(st, encoding: encoding)}
            end
        end

      {:error, e} ->
        es = [{r_compile(st, :ifile), [{:none, :compile, {:epp, e}}]}]
        {:error, r_compile(st, errors: r_compile(st, :errors) ++ es)}
    end
  end

  defp find_invalid_unicode([h | t], file0) do
    case h do
      {:attribute, _, :file, {file, _}} ->
        find_invalid_unicode(t, file)

      {:error, {line, :file_io_server, :invalid_unicode}} ->
        {:invalid_unicode, file0, line}

      _Other ->
        find_invalid_unicode(t, file0)
    end
  end

  defp find_invalid_unicode([], _) do
    :none
  end

  defp parse_core(_Code, st) do
    case :file.read_file(r_compile(st, :ifile)) do
      {:ok, bin} ->
        case :core_scan.string(:erlang.binary_to_list(bin)) do
          {:ok, toks, _} ->
            case :core_parse.parse(toks) do
              {:ok, mod} ->
                name = r_c_literal(r_c_module(mod, :name), :val)
                {:ok, mod, r_compile(st, module: name)}

              {:error, e} ->
                es = [{r_compile(st, :ifile), [e]}]
                {:error, r_compile(st, errors: r_compile(st, :errors) ++ es)}
            end

          {:error, e, _} ->
            es = [{r_compile(st, :ifile), [e]}]
            {:error, r_compile(st, errors: r_compile(st, :errors) ++ es)}
        end

      {:error, e} ->
        es = [{r_compile(st, :ifile), [{:none, :compile, {:open, e}}]}]
        {:error, r_compile(st, errors: r_compile(st, :errors) ++ es)}
    end
  end

  defp get_module_name_from_core(core, st) do
    try do
      mod = :cerl.concrete(:cerl.module_name(core))
      {:ok, core, r_compile(st, module: mod)}
    catch
      _, _ ->
        {:ok, core, st}
    end
  end

  defp compile_options([{:attribute, _L, :compile, c} | fs])
       when is_list(c) do
    c ++ compile_options(fs)
  end

  defp compile_options([{:attribute, _L, :compile, c} | fs]) do
    [c | compile_options(fs)]
  end

  defp compile_options([_F | fs]) do
    compile_options(fs)
  end

  defp compile_options([]) do
    []
  end

  defp clean_parse_transforms(fs) do
    clean_parse_transforms_1(fs, [])
  end

  defp clean_parse_transforms_1([{:attribute, l, :compile, c0} | fs], acc)
       when is_list(c0) do
    c =
      :lists.filter(
        fn
          {:parse_transform, _} ->
            false

          _ ->
            true
        end,
        c0
      )

    clean_parse_transforms_1(
      fs,
      [{:attribute, l, :compile, c} | acc]
    )
  end

  defp clean_parse_transforms_1(
         [
           {:attribute, _, :compile, {:parse_transform, _}}
           | fs
         ],
         acc
       ) do
    clean_parse_transforms_1(fs, acc)
  end

  defp clean_parse_transforms_1([f | fs], acc) do
    clean_parse_transforms_1(fs, [f | acc])
  end

  defp clean_parse_transforms_1([], acc) do
    reverse(acc)
  end

  defp transforms(os) do
    for {:parse_transform, m} <- os do
      m
    end
  end

  defp transform_module(code0, r_compile(options: opt) = st) do
    case transforms(opt ++ compile_options(code0)) do
      [] ->
        {:ok, code0, st}

      ts ->
        code = clean_parse_transforms(code0)
        foldl_transform(ts, code, st)
    end
  end

  defp foldl_transform([t | ts], code0, st) do
    name = 'transform ' ++ :erlang.atom_to_list(t)

    case :code.ensure_loaded(t) === {:module, t} and
           :erlang.function_exported(
             t,
             :parse_transform,
             2
           ) do
      true ->
        fun = fn code, s ->
          t.parse_transform(code, r_compile(s, :options))
        end

        run =
          case member(:time, r_compile(st, :options)) do
            true ->
              &run_tc/3

            false ->
              fn {_Name, f}, code, s ->
                try do
                  f.(code, s)
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end
              end
          end

        case run.({name, fun}, code0, st) do
          {:error, es, ws} ->
            {:error,
             r_compile(st,
               warnings: r_compile(st, :warnings) ++ ws,
               errors: r_compile(st, :errors) ++ es
             )}

          {:EXIT, r} ->
            es = [{r_compile(st, :ifile), [{:none, :compile, {:parse_transform, t, r}}]}]
            {:error, r_compile(st, errors: r_compile(st, :errors) ++ es)}

          {:warning, forms, ws} ->
            foldl_transform(ts, forms, r_compile(st, warnings: r_compile(st, :warnings) ++ ws))

          forms ->
            foldl_transform(ts, forms, st)
        end

      false ->
        es = [{r_compile(st, :ifile), [{:none, :compile, {:undef_parse_transform, t}}]}]
        {:error, r_compile(st, errors: r_compile(st, :errors) ++ es)}
    end
  end

  defp foldl_transform([], code, st) do
    {:ok, code, st}
  end

  defp get_core_transforms(opts) do
    for {:core_transform, m} <- opts do
      m
    end
  end

  defp core_transforms(code, st) do
    ts = get_core_transforms(r_compile(st, :options))
    foldl_core_transforms(ts, code, st)
  end

  defp foldl_core_transforms([t | ts], code0, st) do
    name = 'core transform ' ++ :erlang.atom_to_list(t)

    fun = fn code, s ->
      t.core_transform(code, r_compile(s, :options))
    end

    run =
      case member(:time, r_compile(st, :options)) do
        true ->
          &run_tc/3

        false ->
          fn {_Name, f}, code, s ->
            try do
              f.(code, s)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end
          end
      end

    case run.({name, fun}, code0, st) do
      {:EXIT, r} ->
        es = [{r_compile(st, :ifile), [{:none, :compile, {:core_transform, t, r}}]}]
        {:error, r_compile(st, errors: r_compile(st, :errors) ++ es)}

      forms ->
        foldl_core_transforms(ts, forms, st)
    end
  end

  defp foldl_core_transforms([], code, st) do
    {:ok, code, st}
  end

  defp get_module([{:attribute, _, :module, m} | _]) do
    m
  end

  defp get_module([_ | rest]) do
    get_module(rest)
  end

  defp add_default_base(st, forms) do
    f = r_compile(st, :filename)

    case f do
      '' ->
        m = get_module(forms)
        r_compile(st, base: :erlang.atom_to_list(m))

      _ ->
        st
    end
  end

  defp lint_module(code, st) do
    case :erl_lint.module(code, r_compile(st, :ifile), r_compile(st, :options)) do
      {:ok, ws} ->
        st1 = add_default_base(st, code)
        {:ok, code, r_compile(st1, warnings: r_compile(st1, :warnings) ++ ws)}

      {:error, es, ws} ->
        {:error,
         r_compile(st,
           warnings: r_compile(st, :warnings) ++ ws,
           errors: r_compile(st, :errors) ++ es
         )}
    end
  end

  defp core_lint_module(code, st) do
    case :core_lint.module(code, r_compile(st, :options)) do
      {:ok, ws} ->
        {:ok, code, r_compile(st, warnings: r_compile(st, :warnings) ++ ws)}

      {:error, es, ws} ->
        {:error,
         r_compile(st,
           warnings: r_compile(st, :warnings) ++ ws,
           errors: r_compile(st, :errors) ++ es
         )}
    end
  end

  defp makedep_and_output(code0, st) do
    {:ok, depCode, st1} = makedep(code0, st)

    case makedep_output(depCode, st1) do
      {:ok, _IgnoreCode, st2} ->
        {:ok, code0, st2}

      {:error, st2} ->
        {:error, st2}
    end
  end

  defp makedep(
         code0,
         r_compile(ifile: ifile, ofile: ofile, options: opts) = st
       ) do
    target0 =
      case :proplists.get_value(
             :makedep_target,
             opts
           ) do
        :undefined ->
          shorten_filename(ofile)

        t ->
          t
      end

    target1 =
      case :proplists.get_value(
             :makedep_quote_target,
             opts
           ) do
        true ->
          fun = fn
            ?$ ->
              '$$'

            c ->
              c
          end

          map(fun, target0)

        _ ->
          target0
      end

    target = target1 ++ ':'

    {mainRule, phonyRules} =
      makedep_add_headers(ifile, code0, [], length(target), target, '', opts)

    makefile =
      case :proplists.get_value(
             :makedep_phony,
             opts
           ) do
        true ->
          mainRule ++ phonyRules

        _ ->
          mainRule
      end

    code = :erlang.iolist_to_binary([makefile, '\n'])
    {:ok, code, st}
  end

  defp makedep_add_headers(
         ifile,
         [{:attribute, _, :file, {file, _}} | rest],
         included,
         lineLen,
         mainTarget,
         phony,
         opts
       ) do
    {included1, lineLen1, mainTarget1, phony1} =
      makedep_add_header(ifile, included, lineLen, mainTarget, phony, file)

    makedep_add_headers(ifile, rest, included1, lineLen1, mainTarget1, phony1, opts)
  end

  defp makedep_add_headers(
         ifile,
         [{:error, {_, :epp, {:include, :file, file}}} | rest],
         included,
         lineLen,
         mainTarget,
         phony,
         opts
       ) do
    case :proplists.get_value(
           :makedep_add_missing,
           opts
         ) do
      true ->
        {included1, lineLen1, mainTarget1, phony1} =
          makedep_add_header(ifile, included, lineLen, mainTarget, phony, file)

        makedep_add_headers(ifile, rest, included1, lineLen1, mainTarget1, phony1, opts)

      _ ->
        makedep_add_headers(ifile, rest, included, lineLen, mainTarget, phony, opts)
    end
  end

  defp makedep_add_headers(ifile, [_ | rest], included, lineLen, mainTarget, phony, opts) do
    makedep_add_headers(ifile, rest, included, lineLen, mainTarget, phony, opts)
  end

  defp makedep_add_headers(_Ifile, [], _Included, _LineLen, mainTarget, phony, _Opts) do
    {mainTarget, phony}
  end

  defp makedep_add_header(ifile, included, lineLen, mainTarget, phony, file) do
    case member(file, included) do
      true ->
        {included, lineLen, mainTarget, phony}

      false ->
        included1 = [file | included]

        file1 =
          case file do
            './' ++ file0 ->
              file0

            _ ->
              file
          end

        phony1 =
          case file do
            ^ifile ->
              phony

            _ ->
              phony ++ '\n\n' ++ file1 ++ ':'
          end

        cond do
          lineLen + 1 + length(file1) > 76 ->
            lineLen1 = 2 + length(file1)
            mainTarget1 = mainTarget ++ ' \\\n  ' ++ file1
            {included1, lineLen1, mainTarget1, phony1}

          true ->
            lineLen1 = lineLen + 1 + length(file1)
            mainTarget1 = mainTarget ++ ' ' ++ file1
            {included1, lineLen1, mainTarget1, phony1}
        end
    end
  end

  defp makedep_output(code, r_compile(options: opts, ofile: ofile) = st) do
    output0 =
      case :proplists.get_value(
             :makedep_output,
             opts
           ) do
        :undefined ->
          outfile(:filename.basename(ofile, '.beam'), 'Pbeam', opts)

        o ->
          o
      end

    ret =
      case output0 do
        _ when is_list(output0) ->
          case :file.delete(output0) do
            ret2 when ret2 === :ok or ret2 === {:error, :enoent} ->
              case :file.open(output0, [:write]) do
                {:ok, iODev} ->
                  {:ok, iODev, true}

                {:error, reason2} ->
                  {:error, :open, reason2}
              end

            {:error, reason1} ->
              {:error, :delete, reason1}
          end

        _ ->
          {:ok, output0, false}
      end

    case ret do
      {:ok, output1, closeOutput} ->
        try do
          :io.fwrite(output1, '~ts', [code])

          cond do
            closeOutput ->
              :ok = :file.close(output1)

            true ->
              :ok
          end

          {:ok, code, st}
        catch
          :error, _ ->
            err = {r_compile(st, :ifile), [{:none, :compile, :write_error}]}
            {:error, r_compile(st, errors: r_compile(st, :errors) ++ [err])}
        end

      {:error, :open, reason} ->
        err = {r_compile(st, :ifile), [{:none, :compile, {:open, reason}}]}
        {:error, r_compile(st, errors: r_compile(st, :errors) ++ [err])}

      {:error, :delete, reason} ->
        err = {r_compile(st, :ifile), [{:none, :compile, {:delete, output0, reason}}]}
        {:error, r_compile(st, errors: r_compile(st, :errors) ++ [err])}
    end
  end

  defp expand_records(code0, r_compile(options: opts) = st) do
    code = :erl_expand_records.module(code0, opts)
    {:ok, code, st}
  end

  defp compile_directives(forms, r_compile(options: opts0) = st) do
    opts =
      expand_opts(
        flatten(
          for {:attribute, _, :compile, c} <- forms do
            c
          end
        )
      )

    {:ok, forms, r_compile(st, options: opts ++ opts0)}
  end

  defp core(forms, r_compile(options: opts) = st) do
    {:ok, core, ws} = :v3_core.module(forms, opts)
    mod = :cerl.concrete(:cerl.module_name(core))

    {:ok, core,
     r_compile(st, module: mod, options: opts, warnings: r_compile(st, :warnings) ++ ws)}
  end

  defp core_fold_module_after_inlining(code0, r_compile(options: opts) = st) do
    {:ok, code, _Ws} = :sys_core_fold.module(code0, opts)
    {:ok, code, st}
  end

  defp v3_kernel(code0, r_compile(options: opts, warnings: ws0) = st) do
    {:ok, code, ws} = :v3_kernel.module(code0, opts)

    case ws === [] or test_core_inliner(st) do
      false ->
        {:ok, code, r_compile(st, warnings: ws0 ++ ws)}

      true ->
        {:ok, code, st}
    end
  end

  defp test_old_inliner(r_compile(options: opts)) do
    any(
      fn
        {:inline, _} ->
          true

        _ ->
          false
      end,
      opts
    )
  end

  defp test_core_inliner(r_compile(options: opts)) do
    case any(
           fn
             :no_inline ->
               true

             _ ->
               false
           end,
           opts
         ) do
      true ->
        false

      false ->
        any(
          fn
            :inline ->
              true

            _ ->
              false
          end,
          opts
        )
    end
  end

  defp test_any_inliner(st) do
    test_old_inliner(st) or test_core_inliner(st)
  end

  defp core_old_inliner(code0, r_compile(options: opts) = st) do
    {:ok, code} = :sys_core_inline.module(code0, opts)
    {:ok, code, st}
  end

  defp core_inline_module(code0, r_compile(options: opts) = st) do
    code = :cerl_inline.core_transform(code0, opts)
    {:ok, code, st}
  end

  defp save_abstract_code(code, st) do
    {:ok, code, r_compile(st, abstract_code: :erl_parse.anno_to_term(code))}
  end

  defp debug_info(r_compile(module: module, ofile: oFile) = st) do
    {debugInfo, opts2} = debug_info_chunk(st)

    case member(:encrypt_debug_info, opts2) do
      true ->
        case :lists.keytake(:debug_info_key, 1, opts2) do
          {:value, {_, key}, opts3} ->
            encrypt_debug_info(debugInfo, key, [{:debug_info_key, :"********"} | opts3])

          false ->
            mode = :proplists.get_value(:crypto_mode, opts2, :des3_cbc)

            case :beam_lib.get_crypto_key({:debug_info, mode, module, oFile}) do
              :error ->
                {:error, [{:none, :compile, :no_crypto_key}]}

              key ->
                encrypt_debug_info(debugInfo, {mode, key}, opts2)
            end
        end

      false ->
        {:ok, debugInfo, opts2}
    end
  end

  defp debug_info_chunk(r_compile(mod_options: modOpts0, options: compOpts, abstract_code: abst)) do
    abstOpts = cleanup_compile_options(modOpts0)

    {backend, metadata, modOpts} =
      case :proplists.get_value(:debug_info, compOpts, false) do
        {optBackend, optMetadata} when is_atom(optBackend) ->
          modOpts1 = :proplists.delete(:debug_info, modOpts0)
          {optBackend, optMetadata, modOpts1}

        true ->
          modOpts1 = :proplists.delete(:debug_info, modOpts0)
          {:erl_abstract_code, {abst, abstOpts}, [:debug_info | modOpts1]}

        false ->
          {:erl_abstract_code, {:none, abstOpts}, modOpts0}
      end

    debugInfo =
      :erlang.term_to_binary(
        {:debug_info_v1, backend, metadata},
        [:compressed]
      )

    {debugInfo, modOpts}
  end

  defp encrypt_debug_info(debugInfo, key, opts) do
    try do
      realKey = generate_key(key)

      case start_crypto() do
        :ok ->
          {:ok, encrypt(realKey, debugInfo), opts}

        {:error, _} = e ->
          e
      end
    catch
      :error, _ ->
        {:error, [{:none, :compile, :bad_crypto_key}]}
    end
  end

  defp cleanup_compile_options(opts) do
    isDeterministic = :lists.member(:deterministic, opts)

    :lists.filter(
      fn opt ->
        keep_compile_option(opt, isDeterministic)
      end,
      opts
    )
  end

  defp keep_compile_option({:i, _}, deterministic) do
    not deterministic
  end

  defp keep_compile_option({:cwd, _}, deterministic) do
    not deterministic
  end

  defp keep_compile_option(:from_asm, _Deterministic) do
    false
  end

  defp keep_compile_option(:from_core, _Deterministic) do
    false
  end

  defp keep_compile_option({:parse_transform, _}, _Deterministic) do
    false
  end

  defp keep_compile_option({:d, _, _}, _Deterministic) do
    false
  end

  defp keep_compile_option(option, _Deterministic) do
    effects_code_generation(option)
  end

  defp start_crypto() do
    try do
      :crypto.start()
    catch
      :error, _ ->
        {:error, [{:none, :compile, :no_crypto}]}
    else
      {:error, {:already_started, :crypto}} ->
        :ok

      :ok ->
        :ok
    end
  end

  defp generate_key({type, string})
       when is_atom(type) and
              is_list(string) do
    :beam_lib.make_crypto_key(type, string)
  end

  defp generate_key(string) when is_list(string) do
    generate_key({:des3_cbc, string})
  end

  defp encrypt(
         {:des3_cbc = type, key, iVec, blockSize},
         bin0
       ) do
    bin1 =
      case rem(byte_size(bin0), blockSize) do
        0 ->
          bin0

        n ->
          :erlang.list_to_binary([bin0, :crypto.strong_rand_bytes(blockSize - n)])
      end

    bin = :crypto.crypto_one_time(:des_ede3_cbc, key, iVec, bin1, true)
    typeString = :erlang.atom_to_list(type)
    :erlang.list_to_binary([0, length(typeString), typeString, bin])
  end

  defp save_core_code(code, st) do
    {:ok, code, r_compile(st, core_code: :cerl.from_records(code))}
  end

  defp beam_validator_strong(code, st) do
    beam_validator_1(code, st, :strong)
  end

  defp beam_validator_weak(code, st) do
    beam_validator_1(code, st, :weak)
  end

  defp beam_validator_1(code, r_compile(errors: errors0) = st, level) do
    case :beam_validator.validate(code, level) do
      :ok ->
        {:ok, code, st}

      {:error, es} ->
        {:error, r_compile(st, errors: errors0 ++ es)}
    end
  end

  defp beam_asm(
         code0,
         r_compile(ifile: file, extra_chunks: extraChunks, options: compilerOpts) = st
       ) do
    case debug_info(st) do
      {:ok, debugInfo, opts0} ->
        opts1 =
          for o <- opts0, effects_code_generation(o) do
            o
          end

        chunks = [{"Dbgi", debugInfo} | extraChunks]
        compileInfo = compile_info(file, compilerOpts, opts1)
        {:ok, code} = :beam_asm.module(code0, chunks, compileInfo, compilerOpts)
        {:ok, code, r_compile(st, abstract_code: [])}

      {:error, es} ->
        {:error, r_compile(st, errors: r_compile(st, :errors) ++ [{file, es}])}
    end
  end

  defp compile_info(file, compilerOpts, opts) do
    isSlim = member(:slim, compilerOpts)
    isDeterministic = member(:deterministic, compilerOpts)
    info0 = :proplists.get_value(:compile_info, opts, [])

    info1 =
      case paranoid_absname(file) do
        [_ | _] = source
        when not isSlim and
               not isDeterministic ->
          [{:source, source} | info0]

        _ ->
          info0
      end

    info2 =
      case isDeterministic do
        false ->
          [
            {:options, :proplists.delete(:compile_info, opts)}
            | info1
          ]

        true ->
          info1
      end

    info2
  end

  defp paranoid_absname('' = file) do
    file
  end

  defp paranoid_absname(file) do
    case :file.get_cwd() do
      {:ok, cwd} ->
        :filename.absname(file, cwd)

      _ ->
        file
    end
  end

  defp test_native(r_compile(options: opts)) do
    is_native_enabled(opts)
  end

  defp is_native_enabled([:native | _]) do
    true
  end

  defp is_native_enabled([:no_native | _]) do
    false
  end

  defp is_native_enabled([_ | opts]) do
    is_native_enabled(opts)
  end

  defp is_native_enabled([]) do
    false
  end

  defp native_compile(:none, st) do
    {:ok, :none, st}
  end

  defp native_compile(code, st) do
    case :erlang.system_info(:hipe_architecture) do
      :undefined ->
        ws = [{r_compile(st, :ifile), [{:none, :compile, :no_native_support}]}]
        {:ok, code, r_compile(st, warnings: r_compile(st, :warnings) ++ ws)}

      _ ->
        native_compile_1(code, st)
    end
  end

  defp native_compile_1(code, st) do
    opts0 = r_compile(st, :options)
    ignoreErrors = member(:ignore_native_errors, opts0)

    opts =
      case keyfind(:hipe, 1, opts0) do
        {:hipe, l} when is_list(l) ->
          l

        {:hipe, x} ->
          [x]

        _ ->
          []
      end

    try do
      dialyzer_whining_inhibitor(:hipe, :compile, [
        r_compile(st, :module),
        r_compile(st, :core_code),
        code,
        opts
      ])
    catch
      :exit, {:unimplemented_instruction, _} = unimplemented ->
        ws = [{r_compile(st, :ifile), [{:none, :compile, unimplemented}]}]
        {:ok, code, r_compile(st, warnings: r_compile(st, :warnings) ++ ws)}

      class, r ->
        case ignoreErrors do
          true ->
            ws = [
              {r_compile(st, :ifile), [{:none, :compile, {:native_crash, r, __STACKTRACE__}}]}
            ]

            {:ok, code, r_compile(st, warnings: r_compile(st, :warnings) ++ ws)}

          false ->
            :erlang.raise(class, r, __STACKTRACE__)
        end
    else
      {:ok, {_Type, bin} = t} when is_binary(bin) ->
        {:ok, embed_native_code(code, t), st}

      {:error, r} ->
        case ignoreErrors do
          true ->
            ws = [{r_compile(st, :ifile), [{:none, :compile, {:native, r}}]}]
            {:ok, code, r_compile(st, warnings: r_compile(st, :warnings) ++ ws)}

          false ->
            es = [{r_compile(st, :ifile), [{:none, :compile, {:native, r}}]}]
            {:error, r_compile(st, errors: r_compile(st, :errors) ++ es)}
        end
    end
  end

  defp dialyzer_whining_inhibitor(m, f, a) do
    apply(id(m), id(f), id(a))
  end

  defp id(i) do
    i
  end

  defp embed_native_code(code, {architecture, nativeCode}) do
    {:ok, _, chunks0} = :beam_lib.all_chunks(code)
    chunkName = :hipe_unified_loader.chunk_name(architecture)
    chunks1 = :lists.keydelete(chunkName, 1, chunks0)
    chunks = chunks1 ++ [{chunkName, nativeCode}]
    {:ok, beamPlusNative} = :beam_lib.build_module(chunks)
    beamPlusNative
  end

  defp effects_code_generation(option) do
    case option do
      :beam ->
        false

      :report_warnings ->
        false

      :report_errors ->
        false

      :return_errors ->
        false

      :return_warnings ->
        false

      :warnings_as_errors ->
        false

      :binary ->
        false

      :verbose ->
        false

      {:cwd, _} ->
        false

      {:outdir, _} ->
        false

      _ ->
        true
    end
  end

  defp save_binary(:none, st) do
    {:ok, :none, st}
  end

  defp save_binary(
         code,
         r_compile(module: mod, ofile: outfile, options: opts) = st
       ) do
    case member(:no_error_module_mismatch, opts) do
      true ->
        save_binary_1(code, st)

      false ->
        base = :filename.rootname(:filename.basename(outfile))

        case :erlang.atom_to_list(mod) do
          ^base ->
            save_binary_1(code, st)

          _ ->
            es = [{r_compile(st, :ofile), [{:none, :compile, {:module_name, mod, base}}]}]
            {:error, r_compile(st, errors: r_compile(st, :errors) ++ es)}
        end
    end
  end

  defp save_binary_1(code, st) do
    ofile = r_compile(st, :ofile)
    tfile = tmpfile(ofile)

    case write_binary(tfile, code, st) do
      :ok ->
        case :file.rename(tfile, ofile) do
          :ok ->
            {:ok, :none, st}

          {:error, renameError} ->
            es0 = [{ofile, [{:none, :compile, {:rename, tfile, ofile, renameError}}]}]

            es =
              case :file.delete(tfile) do
                :ok ->
                  es0

                {:error, deleteError} ->
                  es0 ++ [{ofile, [{:none, :compile, {:delete_temp, tfile, deleteError}}]}]
              end

            {:error, r_compile(st, errors: r_compile(st, :errors) ++ es)}
        end

      {:error, error} ->
        es = [{tfile, [{:none, :compile, {:write_error, error}}]}]
        {:error, r_compile(st, errors: r_compile(st, :errors) ++ es)}
    end
  end

  defp write_binary(name, bin, st) do
    opts =
      case member(:compressed, r_compile(st, :options)) do
        true ->
          [:compressed]

        false ->
          []
      end

    case :file.write_file(name, bin, opts) do
      :ok ->
        :ok

      {:error, _} = error ->
        error
    end
  end

  defp report_errors(r_compile(options: opts, errors: errors)) do
    case member(:report_errors, opts) do
      true ->
        foreach(
          fn
            {{f, _L}, eds} ->
              list_errors(f, eds)

            {f, eds} ->
              list_errors(f, eds)
          end,
          errors
        )

      false ->
        :ok
    end
  end

  defp report_warnings(r_compile(options: opts, warnings: ws0)) do
    werror = member(:warnings_as_errors, opts)

    p =
      case werror do
        true ->
          ''

        false ->
          'Warning: '
      end

    reportWerror = werror and member(:report_errors, opts)

    case member(:report_warnings, opts) or reportWerror do
      true ->
        ws1 =
          flatmap(
            fn
              {{f, _L}, eds} ->
                format_message(f, p, eds)

              {f, eds} ->
                format_message(f, p, eds)
            end,
            ws0
          )

        ws = :lists.sort(ws1)

        foreach(
          fn {_, str} ->
            :io.put_chars(str)
          end,
          ws
        )

      false ->
        :ok
    end
  end

  defp format_message(f, p, [{:none, mod, e} | es]) do
    m = {:none, :io_lib.format('~ts: ~s~ts\n', [f, p, mod.format_error(e)])}
    [m | format_message(f, p, es)]
  end

  defp format_message(f, p, [{{line, column} = loc, mod, e} | es]) do
    m =
      {{f, loc},
       :io_lib.format(
         '~ts:~w:~w ~s~ts\n',
         [f, line, column, p, mod.format_error(e)]
       )}

    [m | format_message(f, p, es)]
  end

  defp format_message(f, p, [{line, mod, e} | es]) do
    m = {{f, {line, 0}}, :io_lib.format('~ts:~w: ~s~ts\n', [f, line, p, mod.format_error(e)])}
    [m | format_message(f, p, es)]
  end

  defp format_message(f, p, [{mod, e} | es]) do
    m = {:none, :io_lib.format('~ts: ~s~ts\n', [f, p, mod.format_error(e)])}
    [m | format_message(f, p, es)]
  end

  defp format_message(_, _, []) do
    []
  end

  defp list_errors(f, [{:none, mod, e} | es]) do
    :io.fwrite('~ts: ~ts\n', [f, mod.format_error(e)])
    list_errors(f, es)
  end

  defp list_errors(f, [{{line, column}, mod, e} | es]) do
    :io.fwrite('~ts:~w:~w: ~ts\n', [f, line, column, mod.format_error(e)])
    list_errors(f, es)
  end

  defp list_errors(f, [{line, mod, e} | es]) do
    :io.fwrite('~ts:~w: ~ts\n', [f, line, mod.format_error(e)])
    list_errors(f, es)
  end

  defp list_errors(f, [{mod, e} | es]) do
    :io.fwrite('~ts: ~ts\n', [f, mod.format_error(e)])
    list_errors(f, es)
  end

  defp list_errors(_F, []) do
    :ok
  end

  def iofile(file) when is_atom(file) do
    iofile(:erlang.atom_to_list(file))
  end

  def iofile(file) do
    {:filename.dirname(file), :filename.basename(file, '.erl')}
  end

  defp erlfile('.', base, suffix) do
    base ++ suffix
  end

  defp erlfile(dir, base, suffix) do
    :filename.join(dir, base ++ suffix)
  end

  defp outfile(base, ext, opts) when is_atom(ext) do
    outfile(base, :erlang.atom_to_list(ext), opts)
  end

  defp outfile(base, ext, opts) do
    obase =
      case keyfind(:outdir, 1, opts) do
        {:outdir, odir} ->
          :filename.join(odir, base)

        _Other ->
          base
      end

    obase ++ '.' ++ ext
  end

  defp objfile(base, st) do
    outfile(base, 'beam', r_compile(st, :options))
  end

  defp tmpfile(ofile) do
    reverse([?# | tl(reverse(ofile))])
  end

  defp pre_defs([{:d, m, v} | opts]) do
    [{m, v} | pre_defs(opts)]
  end

  defp pre_defs([{:d, m} | opts]) do
    [m | pre_defs(opts)]
  end

  defp pre_defs([_ | opts]) do
    pre_defs(opts)
  end

  defp pre_defs([]) do
    []
  end

  defp inc_paths(opts) do
    for {:i, p} <- opts, is_list(p) do
      p
    end
  end

  defp src_listing(ext, code, st) do
    listing(
      fn
        lf, {_Mod, _Exp, fs} ->
          do_src_listing(lf, fs)

        lf, fs ->
          do_src_listing(lf, fs)
      end,
      ext,
      code,
      st
    )
  end

  defp do_src_listing(lf, fs) do
    opts = [:lists.keyfind(:encoding, 1, :io.getopts(lf))]

    foreach(
      fn f ->
        :io.put_chars(lf, [:erl_pp.form(f, opts), '\n'])
      end,
      fs
    )
  end

  defp listing(ext, code, st0) do
    st = r_compile(st0, encoding: :none)

    listing(
      fn lf, fs ->
        :beam_listing.module(lf, fs)
      end,
      ext,
      code,
      st
    )
  end

  defp listing(lFun, ext, code, st) do
    lfile = outfile(r_compile(st, :base), ext, r_compile(st, :options))

    case :file.open(lfile, [:write, :delayed_write]) do
      {:ok, lf} ->
        ^code = restore_expanded_types(ext, code)
        output_encoding(lf, st)
        lFun.(lf, code)
        :ok = :file.close(lf)
        {:ok, code, st}

      {:error, error} ->
        es = [{lfile, [{:none, :compile, {:write_error, error}}]}]
        {:error, r_compile(st, errors: r_compile(st, :errors) ++ es)}
    end
  end

  defp to_dis(code, r_compile(module: module, ofile: outfile) = st) do
    loaded = :code.is_loaded(module)
    sticky = :code.is_sticky(module)

    _ =
      for _ <- [:EFE_DUMMY_GEN], sticky do
        :code.unstick_mod(module)
      end

    {:module, ^module} = :code.load_binary(module, '', code)
    destDir = :filename.dirname(outfile)

    disFile =
      :filename.join(
        destDir,
        :erlang.atom_to_list(module) ++ '.dis'
      )

    :ok = :erts_debug.dis_to_file(module, disFile)

    _ =
      for _ <- [:EFE_DUMMY_GEN], loaded !== false do
        {:module, ^module} = :code.load_file(module)
      end

    for _ <- [:EFE_DUMMY_GEN], sticky do
      :code.stick_mod(module)
    end

    {:ok, code, st}
  end

  defp output_encoding(f, r_compile(encoding: :none)) do
    :ok =
      :io.setopts(
        f,
        [{:encoding, :epp.default_encoding()}]
      )
  end

  defp output_encoding(f, r_compile(encoding: encoding)) do
    :ok = :io.setopts(f, [{:encoding, encoding}])
    :ok = :io.fwrite(f, "%% ~s\n", [:epp.encoding_to_string(encoding)])
  end

  defp restore_expanded_types('E', {m, i, fs0}) do
    fs = restore_expand_module(fs0)
    {m, i, fs}
  end

  defp restore_expanded_types(_Ext, code) do
    code
  end

  defp restore_expand_module([{:attribute, line, :type, [type]} | fs]) do
    [
      {:attribute, line, :type, type}
      | restore_expand_module(fs)
    ]
  end

  defp restore_expand_module([{:attribute, line, :opaque, [type]} | fs]) do
    [
      {:attribute, line, :opaque, type}
      | restore_expand_module(fs)
    ]
  end

  defp restore_expand_module([{:attribute, line, :spec, [arg]} | fs]) do
    [
      {:attribute, line, :spec, arg}
      | restore_expand_module(fs)
    ]
  end

  defp restore_expand_module([{:attribute, line, :callback, [arg]} | fs]) do
    [
      {:attribute, line, :callback, arg}
      | restore_expand_module(fs)
    ]
  end

  defp restore_expand_module([{:attribute, line, :record, [r]} | fs]) do
    [
      {:attribute, line, :record, r}
      | restore_expand_module(fs)
    ]
  end

  defp restore_expand_module([f | fs]) do
    [f | restore_expand_module(fs)]
  end

  defp restore_expand_module([]) do
    []
  end

  defp diffable(code0, st) do
    {mod, exp, attr, fs0, numLabels} = code0

    entryLabels0 =
      for {:function, name, arity, entry, _} <- fs0 do
        {entry, {name, arity}}
      end

    entryLabels = :maps.from_list(entryLabels0)

    fs =
      for f <- fs0 do
        diffable_fix_function(f, entryLabels)
      end

    code = {mod, exp, attr, fs, numLabels}
    {:ok, code, st}
  end

  defp diffable_fix_function(
         {:function, name, arity, entry0, is0},
         labelMap0
       ) do
    entry = :maps.get(entry0, labelMap0)
    {is1, labelMap} = diffable_label_map(is0, 1, labelMap0, [])

    fb = fn old ->
      :erlang.error({:no_fb, old})
    end

    is = :beam_utils.replace_labels(is1, [], labelMap, fb)
    {:function, name, arity, entry, is}
  end

  defp diffable_label_map([{:label, old} | is], new, map, acc) do
    case map do
      %{^old => newLabel} ->
        diffable_label_map(is, new, map, [{:label, newLabel} | acc])

      %{} ->
        diffable_label_map(is, new + 1, Map.put(map, old, new), [{:label, new} | acc])
    end
  end

  defp diffable_label_map([i | is], new, map, acc) do
    diffable_label_map(is, new, map, [i | acc])
  end

  defp diffable_label_map([], _New, map, acc) do
    {acc, map}
  end

  def options() do
    help(standard_passes())
  end

  defp help([{:delay, ps} | t]) do
    help(ps)
    help(t)
  end

  defp help([{:iff, flag, {:src_listing, ext}} | t]) do
    :io.fwrite('~p - Generate .~s source listing file\n', [flag, ext])
    help(t)
  end

  defp help([{:iff, flag, {:listing, ext}} | t]) do
    :io.fwrite('~p - Generate .~s file\n', [flag, ext])
    help(t)
  end

  defp help([{:iff, flag, {name, fun}} | t])
       when is_function(fun) do
    :io.fwrite('~p - Run ~s\n', [flag, name])
    help(t)
  end

  defp help([{:iff, _Flag, action} | t]) do
    help(action)
    help(t)
  end

  defp help([{:unless, flag, {:pass, pass}} | t]) do
    :io.fwrite('~p - Skip the ~s pass\n', [flag, pass])
    help(t)
  end

  defp help([{:unless, :no_postopt = flag, list} | t])
       when is_list(list) do
    :io.fwrite('~p - Skip all post optimisation\n', [flag])
    help(list)
    help(t)
  end

  defp help([{:unless, _Flag, action} | t]) do
    help(action)
    help(t)
  end

  defp help([_ | t]) do
    help(t)
  end

  defp help(_) do
    :ok
  end

  def compile(file0, _OutFile, options) do
    pre_load()
    file = shorten_filename(file0)

    case file(file, make_erl_options(options)) do
      {:ok, _Mod} ->
        :ok

      other ->
        other
    end
  end

  def compile_beam(file0, _OutFile, opts) do
    file = shorten_filename(file0)

    case file(
           file,
           [:from_beam | make_erl_options(opts)]
         ) do
      {:ok, _Mod} ->
        :ok

      other ->
        other
    end
  end

  def compile_asm(file0, _OutFile, opts) do
    file = shorten_filename(file0)

    case file(
           file,
           [:from_asm | make_erl_options(opts)]
         ) do
      {:ok, _Mod} ->
        :ok

      other ->
        other
    end
  end

  def compile_core(file0, _OutFile, opts) do
    file = shorten_filename(file0)

    case file(
           file,
           [:from_core | make_erl_options(opts)]
         ) do
      {:ok, _Mod} ->
        :ok

      other ->
        other
    end
  end

  defp shorten_filename(name0) do
    {:ok, cwd} = :file.get_cwd()

    case :lists.prefix(cwd, name0) do
      false ->
        name0

      true ->
        case :lists.nthtail(length(cwd), name0) do
          '/' ++ n ->
            n

          n ->
            n
        end
    end
  end

  defp make_erl_options(opts) do
    r_options(
      includes: includes,
      defines: defines,
      outdir: outdir,
      warning: warning,
      verbose: verbose,
      specific: specific,
      output_type: outputType,
      cwd: cwd
    ) = opts

    options =
      for _ <- [:EFE_DUMMY_GEN], verbose do
        :verbose
      end ++
        for _ <- [:EFE_DUMMY_GEN], warning !== 0 do
          :report_warnings
        end ++
        map(
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
            []

          :jam ->
            [:jam]

          :beam ->
            [:beam]

          :native ->
            [:native]
        end

    options ++
      [
        :report_errors,
        {:cwd, cwd},
        {:outdir, outdir}
        | for dir <- includes do
            {:i, dir}
          end
      ] ++ specific
  end

  defp pre_load() do
    l = [
      :beam_a,
      :beam_asm,
      :beam_block,
      :beam_call_types,
      :beam_clean,
      :beam_dict,
      :beam_digraph,
      :beam_flatten,
      :beam_jump,
      :beam_kernel_to_ssa,
      :beam_opcodes,
      :beam_peep,
      :beam_ssa,
      :beam_ssa_bool,
      :beam_ssa_bsm,
      :beam_ssa_codegen,
      :beam_ssa_dead,
      :beam_ssa_funs,
      :beam_ssa_opt,
      :beam_ssa_pre_codegen,
      :beam_ssa_recv,
      :beam_ssa_share,
      :beam_ssa_throw,
      :beam_ssa_type,
      :beam_trim,
      :beam_types,
      :beam_utils,
      :beam_validator,
      :beam_z,
      :cerl,
      :cerl_clauses,
      :cerl_sets,
      :cerl_trees,
      :core_lib,
      :epp,
      :erl_bifs,
      :erl_expand_records,
      :erl_lint,
      :erl_parse,
      :erl_scan,
      :sys_core_alias,
      :sys_core_bsm,
      :sys_core_fold,
      :v3_core,
      :v3_kernel
    ]

    _ = :code.ensure_modules_loaded(l)
    :ok
  end
end
