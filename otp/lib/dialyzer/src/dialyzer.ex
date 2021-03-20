defmodule :m_dialyzer do
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

  def plain_cl() do
    case :dialyzer_cl_parse.start() do
      {:check_init, opts} ->
        cl_halt(cl_check_init(opts), opts)

      {:plt_info, opts} ->
        cl_halt(cl_print_plt_info(opts), opts)

      {:gui, opts} ->
        try do
          check_gui_options(opts)
        catch
          {:dialyzer_error, msg} ->
            cl_error(msg)
        end

        case r_options(opts, :check_plt) do
          true ->
            case cl_check_init(r_options(opts, get_warnings: false)) do
              {:ok, _} ->
                gui_halt(internal_gui(opts), opts)

              {:error, _} = error ->
                cl_halt(error, opts)
            end

          false ->
            gui_halt(internal_gui(opts), opts)
        end

      {:cl, opts} ->
        case r_options(opts, :check_plt) do
          true ->
            case cl_check_init(r_options(opts, get_warnings: false)) do
              {:error, _} = error ->
                cl_halt(error, opts)

              {:ok, _} ->
                cl_halt(cl(opts), opts)
            end

          false ->
            cl_halt(cl(opts), opts)
        end

      {:error, msg} ->
        cl_error(msg)
    end
  end

  defp cl_check_init(r_options(analysis_type: analType) = opts) do
    case analType do
      :plt_build ->
        {:ok, 0}

      :plt_add ->
        {:ok, 0}

      :plt_remove ->
        {:ok, 0}

      other
      when other === :succ_typings or
             other === :plt_check ->
        f = fn ->
          newOpts = r_options(opts, analysis_type: :plt_check)
          {ret, _Warnings} = :dialyzer_cl.start(newOpts)
          ret
        end

        doit(f)
    end
  end

  defp cl_print_plt_info(opts) do
    f = fn ->
      print_plt_info(opts)
    end

    doit(f)
  end

  defp print_plt_info(r_options(init_plts: pLTs, output_file: outputFile)) do
    pLTInfo = get_plt_info(pLTs)
    do_print_plt_info(pLTInfo, outputFile)
  end

  defp get_plt_info([pLT | pLTs]) do
    string =
      case :dialyzer_plt.included_files(pLT) do
        {:ok, files} ->
          :io_lib.format('The PLT ~ts includes the following files:\n~tp\n\n', [pLT, files])

        {:error, :read_error} ->
          msg = :io_lib.format('Could not read the PLT file ~tp\n\n', [pLT])
          throw({:dialyzer_error, msg})

        {:error, :no_such_file} ->
          msg = :io_lib.format('The PLT file ~tp does not exist\n\n', [pLT])
          throw({:dialyzer_error, msg})
      end

    string ++ get_plt_info(pLTs)
  end

  defp get_plt_info([]) do
    ''
  end

  defp do_print_plt_info(pLTInfo, outputFile) do
    case outputFile === :none do
      true ->
        :io.format('~ts', [pLTInfo])
        0

      false ->
        case :file.open(outputFile, [:write]) do
          {:ok, fileDesc} ->
            :io.format(fileDesc, '~ts', [pLTInfo])
            :ok = :file.close(fileDesc)
            0

          {:error, reason} ->
            msg1 =
              :io_lib.format('Could not open output file ~tp, Reason: ~p\n', [outputFile, reason])

            throw({:dialyzer_error, msg1})
        end
    end
  end

  defp cl(opts) do
    f = fn ->
      {ret, _Warnings} = :dialyzer_cl.start(opts)
      ret
    end

    doit(f)
  end

  def run(opts) do
    try do
      :dialyzer_options.build([{:report_mode, :quiet}, {:erlang_mode, true} | opts])
    catch
      {:dialyzer_error, errorMsg} ->
        :erlang.error({:dialyzer_error, :lists.flatten(errorMsg)})
    else
      {:error, msg} ->
        throw({:dialyzer_error, msg})

      optsRecord ->
        :ok = check_init(optsRecord)

        case :dialyzer_cl.start(optsRecord) do
          {2, warnings} ->
            warnings

          {0, _} ->
            []
        end
    end
  end

  defp check_init(r_options(analysis_type: :plt_check)) do
    :ok
  end

  defp check_init(r_options(check_plt: true) = optsRecord) do
    case cl_check_init(optsRecord) do
      {:ok, _} ->
        :ok

      {:error, msg} ->
        throw({:dialyzer_error, msg})
    end
  end

  defp check_init(r_options(check_plt: false)) do
    :ok
  end

  defp internal_gui(optsRecord) do
    f = fn ->
      :dialyzer_gui_wx.start(optsRecord)
      0
    end

    doit(f)
  end

  def gui() do
    gui([])
  end

  def gui(opts) do
    try do
      :dialyzer_options.build([{:report_mode, :quiet} | opts])
    catch
      {:dialyzer_error, errorMsg} ->
        :erlang.error({:dialyzer_error, :lists.flatten(errorMsg)})
    else
      {:error, msg} ->
        throw({:dialyzer_error, msg})

      optsRecord ->
        :ok = check_gui_options(optsRecord)
        :ok = check_init(optsRecord)

        f = fn ->
          :dialyzer_gui_wx.start(optsRecord)
        end

        case doit(f) do
          {:ok, _} ->
            :ok

          {:error, msg} ->
            throw({:dialyzer_error, msg})
        end
    end
  end

  defp check_gui_options(r_options(analysis_type: :succ_typings)) do
    :ok
  end

  defp check_gui_options(r_options(analysis_type: mode)) do
    msg = :io_lib.format('Analysis mode ~w is illegal in GUI mode', [mode])
    throw({:dialyzer_error, msg})
  end

  def plt_info(plt) do
    case :dialyzer_plt.included_files(plt) do
      {:ok, files} ->
        {:ok, [{:files, files}]}

      error ->
        error
    end
  end

  defp doit(f) do
    try do
      {:ok, f.()}
    catch
      {:dialyzer_error, msg} ->
        {:error, :lists.flatten(msg)}
    end
  end

  defp cl_error(msg) do
    cl_halt({:error, msg}, r_options())
  end

  defp gui_halt(r, opts) do
    cl_halt(r, r_options(opts, report_mode: :quiet))
  end

  defp cl_halt({:ok, r = 0}, r_options(report_mode: :quiet)) do
    :erlang.halt(r)
  end

  defp cl_halt({:ok, r = 2}, r_options(report_mode: :quiet)) do
    :erlang.halt(r)
  end

  defp cl_halt({:ok, r = 0}, r_options()) do
    :io.put_chars('done (passed successfully)\n')
    :erlang.halt(r)
  end

  defp cl_halt({:ok, r = 2}, r_options(output_file: output)) do
    :io.put_chars('done (warnings were emitted)\n')
    cl_check_log(output)
    :erlang.halt(r)
  end

  defp cl_halt({:error, msg1}, r_options(output_file: output)) do
    :io.format('\ndialyzer: ~ts\n', [msg1])
    cl_check_log(output)
    :erlang.halt(1)
  end

  defp cl_check_log(:none) do
    :ok
  end

  defp cl_check_log(output) do
    :io.format('  Check output file `~ts\' for details\n', [output])
  end

  def format_warning(w) do
    format_warning(w, :basename)
  end

  def format_warning(rawWarning, fOpt) when is_atom(fOpt) do
    format_warning(rawWarning, [{:filename_opt, fOpt}])
  end

  def format_warning({tag, {file, line, _MFA}, msg}, opts) do
    format_warning({tag, {file, line}, msg}, opts)
  end

  def format_warning({_Tag, {file, line}, msg}, opts)
      when is_list(file) and is_integer(line) do
    f =
      case :proplists.get_value(:filename_opt, opts, :basename) do
        :fullpath ->
          file

        :basename ->
          :filename.basename(file)
      end

    indent = :proplists.get_value(:indent_opt, opts, true)
    string = message_to_string(msg, indent)
    :lists.flatten(:io_lib.format('~ts:~w: ~ts', [f, line, string]))
  end

  defp message_to_string(
         {:apply, [args, argNs, failReason, sigArgs, sigRet, contract]},
         i
       ) do
    :io_lib.format(
      'Fun application with arguments ~ts ',
      [a(args, i)]
    ) ++ call_or_apply_to_string(argNs, failReason, sigArgs, sigRet, contract, i)
  end

  defp message_to_string(
         {:app_call, [m, f, args, culprit, expectedType, foundType]},
         i
       ) do
    :io_lib.format(
      'The call ~s:~ts~ts requires that ~ts is of type ~ts not ~ts\n',
      [m, f, a(args, i), c(culprit, i), t(expectedType, i), t(foundType, i)]
    )
  end

  defp message_to_string(
         {:bin_construction, [culprit, size, seg, type]},
         i
       ) do
    :io_lib.format(
      'Binary construction will fail since the ~s field ~s in segment ~s has type ~s\n',
      [culprit, c(size, i), c(seg, i), t(type, i)]
    )
  end

  defp message_to_string(
         {:call, [m, f, args, argNs, failReason, sigArgs, sigRet, contract]},
         i
       ) do
    :io_lib.format(
      'The call ~w:~tw~ts ',
      [m, f, a(args, i)]
    ) ++
      call_or_apply_to_string(
        argNs,
        failReason,
        sigArgs,
        sigRet,
        contract,
        i
      )
  end

  defp message_to_string({:call_to_missing, [m, f, a]}, _I) do
    :io_lib.format('Call to missing or unexported function ~w:~tw/~w\n', [m, f, a])
  end

  defp message_to_string({:exact_eq, [type1, op, type2]}, i) do
    :io_lib.format('The test ~ts ~s ~ts can never evaluate to \'true\'\n', [
      t(type1, i),
      op,
      t(type2, i)
    ])
  end

  defp message_to_string({:fun_app_args, [argNs, args, type]}, i) do
    positionString = form_position_string(argNs)

    :io_lib.format(
      'Fun application with arguments ~ts will fail since the function has type ~ts, which differs in the ~s argument\n',
      [a(args, i), t(type, i), positionString]
    )
  end

  defp message_to_string({:fun_app_no_fun, [op, type, arity]}, i) do
    :io_lib.format('Fun application will fail since ~ts :: ~ts is not a function of arity ~w\n', [
      op,
      t(type, i),
      arity
    ])
  end

  defp message_to_string({:guard_fail, []}, _I) do
    'Clause guard cannot succeed.\n'
  end

  defp message_to_string({:guard_fail, [arg1, infix, arg2]}, i) do
    :io_lib.format('Guard test ~ts ~s ~ts can never succeed\n', [a(arg1, i), infix, a(arg2, i)])
  end

  defp message_to_string({:map_update, [type, key]}, i) do
    :io_lib.format('A key of type ~ts cannot exist in a map of type ~ts\n', [
      t(key, i),
      t(type, i)
    ])
  end

  defp message_to_string({:neg_guard_fail, [arg1, infix, arg2]}, i) do
    :io_lib.format('Guard test not(~ts ~s ~ts) can never succeed\n', [
      a(arg1, i),
      infix,
      a(arg2, i)
    ])
  end

  defp message_to_string({:guard_fail, [guard, args]}, i) do
    :io_lib.format('Guard test ~s~ts can never succeed\n', [guard, a(args, i)])
  end

  defp message_to_string({:neg_guard_fail, [guard, args]}, i) do
    :io_lib.format('Guard test not(~s~ts) can never succeed\n', [guard, a(args, i)])
  end

  defp message_to_string({:guard_fail_pat, [pat, type]}, i) do
    :io_lib.format('Clause guard cannot succeed. The ~ts was matched against the type ~ts\n', [
      ps(pat, i),
      t(type, i)
    ])
  end

  defp message_to_string({:improper_list_constr, [tlType]}, i) do
    :io_lib.format('Cons will produce an improper list since its 2nd argument is ~ts\n', [
      t(tlType, i)
    ])
  end

  defp message_to_string({:no_return, [type | name]}, _I) do
    nameString =
      case name do
        [] ->
          'The created fun '

        [f, a] ->
          :io_lib.format('Function ~tw/~w ', [f, a])
      end

    case type do
      :no_match ->
        nameString ++ 'has no clauses that will ever match\n'

      :only_explicit ->
        nameString ++ 'only terminates with explicit exception\n'

      :only_normal ->
        nameString ++ 'has no local return\n'

      :both ->
        nameString ++ 'has no local return\n'
    end
  end

  defp message_to_string({:record_constr, [recConstr, fieldDiffs]}, i) do
    :io_lib.format(
      'Record construction ~ts violates the declared type of field ~ts\n',
      [t(recConstr, i), field_diffs(fieldDiffs, i)]
    )
  end

  defp message_to_string({:record_constr, [name, field, type]}, i) do
    :io_lib.format(
      'Record construction violates the declared type for #~tw{} since ~ts cannot be of type ~ts\n',
      [name, ps(field, i), t(type, i)]
    )
  end

  defp message_to_string({:record_matching, [string, name]}, i) do
    :io_lib.format('The ~ts violates the declared type for #~tw{}\n', [rec_type(string, i), name])
  end

  defp message_to_string({:record_match, [pat, type]}, i) do
    :io_lib.format(
      'Matching of ~ts tagged with a record name violates the declared type of ~ts\n',
      [ps(pat, i), t(type, i)]
    )
  end

  defp message_to_string({:pattern_match, [pat, type]}, i) do
    :io_lib.format('The ~ts can never match the type ~ts\n', [ps(pat, i), t(type, i)])
  end

  defp message_to_string({:pattern_match_cov, [pat, type]}, i) do
    :io_lib.format(
      'The ~ts can never match since previous clauses completely covered the type ~ts\n',
      [ps(pat, i), t(type, i)]
    )
  end

  defp message_to_string({:unmatched_return, [type]}, i) do
    :io_lib.format('Expression produces a value of type ~ts, but this value is unmatched\n', [
      t(type, i)
    ])
  end

  defp message_to_string({:unused_fun, [f, a]}, _I) do
    :io_lib.format('Function ~tw/~w will never be called\n', [f, a])
  end

  defp message_to_string(
         {:contract_diff, [m, f, _A, contract, sig]},
         i
       ) do
    :io_lib.format(
      'Type specification ~ts is not equal to the success typing: ~ts\n',
      [con(m, f, contract, i), con(m, f, sig, i)]
    )
  end

  defp message_to_string(
         {:contract_subtype, [m, f, _A, contract, sig]},
         i
       ) do
    :io_lib.format(
      'Type specification ~ts is a subtype of the success typing: ~ts\n',
      [con(m, f, contract, i), con(m, f, sig, i)]
    )
  end

  defp message_to_string(
         {:contract_supertype, [m, f, _A, contract, sig]},
         i
       ) do
    :io_lib.format(
      'Type specification ~ts is a supertype of the success typing: ~ts\n',
      [con(m, f, contract, i), con(m, f, sig, i)]
    )
  end

  defp message_to_string(
         {:contract_range, [contract, m, f, argStrings, line, cRet]},
         i
       ) do
    :io_lib.format(
      'The contract ~ts cannot be right because the inferred return for ~tw~ts on line ~w is ~ts\n',
      [
        con(m, f, contract, i),
        f,
        a(argStrings, i),
        line,
        t(
          cRet,
          i
        )
      ]
    )
  end

  defp message_to_string({:invalid_contract, [m, f, a, sig]}, i) do
    :io_lib.format(
      'Invalid type specification for function ~w:~tw/~w. The success typing is ~ts\n',
      [m, f, a, sig(sig, i)]
    )
  end

  defp message_to_string(
         {:contract_with_opaque, [m, f, a, opaqueType, sigType]},
         i
       ) do
    :io_lib.format(
      'The specification for ~w:~tw/~w has an opaque subtype ~ts which is violated by the success typing ~ts\n',
      [m, f, a, t(opaqueType, i), sig(sigType, i)]
    )
  end

  defp message_to_string(
         {:extra_range, [m, f, a, extraRanges, sigRange]},
         i
       ) do
    :io_lib.format(
      'The specification for ~w:~tw/~w states that the function might also return ~ts but the inferred return is ~ts\n',
      [m, f, a, t(extraRanges, i), t(sigRange, i)]
    )
  end

  defp message_to_string(
         {:missing_range, [m, f, a, extraRanges, contrRange]},
         i
       ) do
    :io_lib.format(
      'The success typing for ~w:~tw/~w implies that the function might also return ~ts but the specification return is ~ts\n',
      [m, f, a, t(extraRanges, i), t(contrRange, i)]
    )
  end

  defp message_to_string({:overlapping_contract, [m, f, a]}, _I) do
    :io_lib.format(
      'Overloaded contract for ~w:~tw/~w has overlapping domains; such contracts are currently unsupported and are simply ignored\n',
      [m, f, a]
    )
  end

  defp message_to_string({:spec_missing_fun, [m, f, a]}, _I) do
    :io_lib.format('Contract for function that does not exist: ~w:~tw/~w\n', [m, f, a])
  end

  defp message_to_string(
         {:call_with_opaque, [m, f, args, argNs, expArgs]},
         i
       ) do
    :io_lib.format(
      'The call ~w:~tw~ts contains ~ts when ~ts\n',
      [m, f, a(args, i), form_positions(argNs), form_expected(expArgs, i)]
    )
  end

  defp message_to_string(
         {:call_without_opaque, [m, f, args, expectedTriples]},
         i
       ) do
    :io_lib.format(
      'The call ~w:~tw~ts does not have ~ts\n',
      [
        m,
        f,
        a(args, i),
        form_expected_without_opaque(
          expectedTriples,
          i
        )
      ]
    )
  end

  defp message_to_string({:opaque_eq, [type, _Op, opaqueType]}, i) do
    :io_lib.format(
      'Attempt to test for equality between a term of type ~ts and a term of opaque type ~ts\n',
      [t(type, i), t(opaqueType, i)]
    )
  end

  defp message_to_string(
         {:opaque_guard, [arg1, infix, arg2, argNs]},
         i
       ) do
    :io_lib.format(
      'Guard test ~ts ~s ~ts contains ~s\n',
      [a(arg1, i), infix, a(arg2, i), form_positions(argNs)]
    )
  end

  defp message_to_string({:opaque_guard, [guard, args]}, i) do
    :io_lib.format('Guard test ~w~ts breaks the opacity of its argument\n', [guard, a(args, i)])
  end

  defp message_to_string(
         {:opaque_match, [pat, opaqueType, opaqueTerm]},
         i
       ) do
    term =
      cond do
        opaqueType === opaqueTerm ->
          'the term'

        true ->
          t(opaqueTerm, i)
      end

    :io_lib.format(
      'The attempt to match a term of type ~ts against the ~ts breaks the opacity of ~ts\n',
      [t(opaqueType, i), ps(pat, i), term]
    )
  end

  defp message_to_string({:opaque_neq, [type, _Op, opaqueType]}, i) do
    :io_lib.format(
      'Attempt to test for inequality between a term of type ~ts and a term of opaque type ~ts\n',
      [t(type, i), t(opaqueType, i)]
    )
  end

  defp message_to_string(
         {:opaque_type_test, [fun, args, arg, argType]},
         i
       ) do
    :io_lib.format('The type test ~ts~ts breaks the opacity of the term ~ts~ts\n', [
      fun,
      a(args, i),
      arg,
      t(argType, i)
    ])
  end

  defp message_to_string({:opaque_size, [sizeType, size]}, i) do
    :io_lib.format('The size ~ts breaks the opacity of ~ts\n', [t(sizeType, i), c(size, i)])
  end

  defp message_to_string(
         {:opaque_call, [m, f, args, culprit, opaqueType]},
         i
       ) do
    :io_lib.format(
      'The call ~s:~ts~ts breaks the opacity of the term ~ts :: ~ts\n',
      [m, f, a(args, i), c(culprit, i), t(opaqueType, i)]
    )
  end

  defp message_to_string({:race_condition, [m, f, args, reason]}, i) do
    :io_lib.format('The call ~w:~tw~ts ~ts\n', [m, f, a(args, i), reason])
  end

  defp message_to_string(
         {:callback_type_mismatch, [b, f, a, sT, cT]},
         i
       ) do
    :io_lib.format(
      'The inferred return type of ~tw/~w ~ts has nothing in common with ~ts, which is the expected return type for the callback of the ~w behaviour\n',
      [f, a, t('(' ++ sT ++ ')', i), t(cT, i), b]
    )
  end

  defp message_to_string(
         {:callback_arg_type_mismatch, [b, f, a, n, sT, cT]},
         i
       ) do
    :io_lib.format(
      'The inferred type for the ~s argument of ~tw/~w (~ts) is not a supertype of ~ts, which is expected type for this argument in the callback of the ~w behaviour\n',
      [ordinal(n), f, a, t(sT, i), t(cT, i), b]
    )
  end

  defp message_to_string(
         {:callback_spec_type_mismatch, [b, f, a, sT, cT]},
         i
       ) do
    :io_lib.format(
      'The return type ~ts in the specification of ~tw/~w is not a subtype of ~ts, which is the expected return type for the callback of the ~w behaviour\n',
      [t(sT, i), f, a, t(cT, i), b]
    )
  end

  defp message_to_string(
         {:callback_spec_arg_type_mismatch, [b, f, a, n, sT, cT]},
         i
       ) do
    :io_lib.format(
      'The specified type for the ~ts argument of ~tw/~w (~ts) is not a supertype of ~ts, which is expected type for this argument in the callback of the ~w behaviour\n',
      [ordinal(n), f, a, t(sT, i), t(cT, i), b]
    )
  end

  defp message_to_string({:callback_missing, [b, f, a]}, _I) do
    :io_lib.format('Undefined callback function ~tw/~w (behaviour ~w)\n', [f, a, b])
  end

  defp message_to_string({:callback_info_missing, [b]}, _I) do
    :io_lib.format('Callback info about the ~w behaviour is not available\n', [b])
  end

  defp message_to_string({:unknown_type, {m, f, a}}, _I) do
    :io_lib.format('Unknown type ~w:~tw/~w', [m, f, a])
  end

  defp message_to_string({:unknown_function, {m, f, a}}, _I) do
    :io_lib.format('Unknown function ~w:~tw/~w', [m, f, a])
  end

  defp message_to_string({:unknown_behaviour, b}, _I) do
    :io_lib.format('Unknown behaviour ~w', [b])
  end

  defp call_or_apply_to_string(argNs, failReason, sigArgs, sigRet, {isOverloaded, contract}, i) do
    positionString = form_position_string(argNs)

    case failReason do
      :only_sig ->
        case argNs === [] do
          true ->
            :io_lib.format('will never return since the success typing arguments are ~ts\n', [
              t(sigArgs, i)
            ])

          false ->
            :io_lib.format(
              'will never return since it differs in the ~s argument from the success typing arguments: ~ts\n',
              [positionString, t(sigArgs, i)]
            )
        end

      :only_contract ->
        case argNs === [] or isOverloaded do
          true ->
            :io_lib.format('breaks the contract ~ts\n', [sig(contract, i)])

          false ->
            :io_lib.format('breaks the contract ~ts in the ~s argument\n', [
              sig(contract, i),
              positionString
            ])
        end

      :both ->
        :io_lib.format(
          'will never return since the success typing is ~ts -> ~ts and the contract is ~ts\n',
          [t(sigArgs, i), t(sigRet, i), sig(contract, i)]
        )
    end
  end

  defp form_positions(argNs) do
    case argNs do
      [_] ->
        'an opaque term as '

      [_, _ | _] ->
        'opaque terms as '
    end ++
      form_position_string(argNs) ++
      case argNs do
        [_] ->
          ' argument'

        [_, _ | _] ->
          ' arguments'
      end
  end

  defp form_expected_without_opaque([{n, t, tStr}], i) do
    case :erl_types.t_is_opaque(t) do
      true ->
        :io_lib.format('an opaque term of type ~ts as ', [t(tStr, i)])

      false ->
        :io_lib.format('a term of type ~ts (with opaque subterms) as ', [t(tStr, i)])
    end ++ form_position_string([n]) ++ ' argument'
  end

  defp form_expected_without_opaque(expectedTriples, _I) do
    {argNs, _Ts, _TStrs} = :lists.unzip3(expectedTriples)
    'opaque terms as ' ++ form_position_string(argNs) ++ ' arguments'
  end

  defp form_expected(expectedArgs, i) do
    case expectedArgs do
      [t] ->
        tS = :erl_types.t_to_string(t)

        case :erl_types.t_is_opaque(t) do
          true ->
            :io_lib.format('an opaque term of type ~ts is expected', [t(tS, i)])

          false ->
            :io_lib.format('a structured term of type ~ts is expected', [t(tS, i)])
        end

      [_, _ | _] ->
        'terms of different types are expected in these positions'
    end
  end

  defp form_position_string(argNs) do
    case argNs do
      [] ->
        ''

      [n1] ->
        ordinal(n1)

      [_, _ | _] ->
        [last | prevs] = :lists.reverse(argNs)

        ', ' ++ head =
          :lists.flatten(
            for n <- :lists.reverse(prevs) do
              :io_lib.format(', ~s', [ordinal(n)])
            end
          )

        head ++ ' and ' ++ ordinal(last)
    end
  end

  defp ordinal(1) do
    '1st'
  end

  defp ordinal(2) do
    '2nd'
  end

  defp ordinal(3) do
    '3rd'
  end

  defp ordinal(n) when is_integer(n) do
    :io_lib.format('~wth', [n])
  end

  defp con(m, f, src, i) do
    s = sig(src, i)
    :io_lib.format('~w:~tw~ts', [m, f, s])
  end

  defp sig(src, false) do
    src
  end

  defp sig(src, true) do
    try do
      str = :lists.flatten(:io_lib.format('-spec ~w:~tw~ts.', [:a, :b, src]))
      {:ok, tokens, _EndLocation} = :erl_scan.string(str)
      {:ok, {:attribute, _, :spec, {_MFA, types}}} = :erl_parse.parse_form(tokens)
      indentation(10) ++ pp_spec(types)
    catch
      _, _ ->
        src
    end
  end

  defp a('' = args, _I) do
    args
  end

  defp a(args, i) do
    t(args, i)
  end

  defp c(cerl, _I) do
    cerl
  end

  defp field_diffs(src, false) do
    src
  end

  defp field_diffs(src, true) do
    fields = :string.split(src, ' and ', :all)

    :lists.join(
      ' and ',
      for field <- fields do
        field_diff(field)
      end
    )
  end

  defp field_diff(field) do
    [f | ts] = :string.split(field, '::', :all)
    f ++ ' ::' ++ t(:lists.flatten(:lists.join('::', ts)), true)
  end

  defp rec_type('record ' ++ src, i) do
    'record ' ++ t(src, i)
  end

  defp ps('pattern ' ++ src, i) do
    'pattern ' ++ t(src, i)
  end

  defp ps('variable ' ++ _ = src, _I) do
    src
  end

  defp ps('record field' ++ rest, i) do
    [s, typeStr] = :string.split(rest, 'of type ', :all)
    'record field' ++ s ++ 'of type ' ++ t(typeStr, i)
  end

  defp t(src, false) do
    src
  end

  defp t('(' ++ _ = src, true) do
    ts(src)
  end

  defp t(src, true) do
    try do
      parse_type_or_literal(src)
    catch
      _, _ ->
        ts(src)
    else
      typeOrLiteral ->
        indentation(10) ++ pp_type(typeOrLiteral)
    end
  end

  defp ts(src) do
    ind = indentation(10)
    [c1 | src1] = src
    [c2 | revSrc2] = :lists.reverse(src1)
    src2 = :lists.reverse(revSrc2)

    try do
      types = parse_types_and_literals(src2)
      commaInd = [?, | ind]

      indentation(10 - 1) ++
        [
          c1
          | :lists.join(
              commaInd,
              for type <- types do
                pp_type(type)
              end
            )
        ] ++ [c2]
    catch
      _, _ ->
        src
    end
  end

  defp indentation(i) do
    [?\n | :lists.duplicate(i, ?\s)]
  end

  defp pp_type(type) do
    form = {:attribute, :erl_anno.new(0), :type, {:t, type, []}}

    typeDef =
      :erl_pp.form(
        form,
        [{:quote_singleton_atom_types, true}]
      )

    {:match, [s]} =
      :re.run(typeDef, "::\\s*(.*)\\.\\n*", [{:capture, :all_but_first, :list}, :dotall])

    s
  end

  defp pp_spec(spec) do
    form = {:attribute, :erl_anno.new(0), :spec, {{:a, :b, 0}, spec}}

    sig =
      :erl_pp.form(
        form,
        [{:quote_singleton_atom_types, true}]
      )

    {:match, [s]} =
      :re.run(sig, "-spec a:b\\s*(.*)\\.\\n*", [{:capture, :all_but_first, :list}, :dotall])

    s
  end

  defp parse_types_and_literals(src) do
    {:ok, tokens, _EndLocation} = :erl_scan.string(src)

    for ts <- types(tokens) do
      parse_a_type_or_literal(ts)
    end
  end

  defp parse_type_or_literal(src) do
    {:ok, tokens, _EndLocation} = :erl_scan.string(src)
    parse_a_type_or_literal(tokens)
  end

  defp parse_a_type_or_literal(ts0) do
    l = :erl_anno.new(1)
    ts = ts0 ++ [{:dot, l}]
    tokens = [{:-, l}, {:atom, l, :type}, {:atom, l, :t}, {:"(", l}, {:")", l}, {:"::", l}] ++ ts

    case :erl_parse.parse_form(tokens) do
      {:ok, {:attribute, _, :type, {:t, type, []}}} ->
        type

      {:error, _} ->
        {:ok, [t]} = :erl_parse.parse_exprs(ts)
        t
    end
  end

  defp types([]) do
    []
  end

  defp types(ts) do
    {ts0, ts1} = one_type(ts, [], [])
    [ts0 | types(ts1)]
  end

  defp one_type([], [], ts) do
    {:lists.reverse(ts), []}
  end

  defp one_type([{:",", _Lc} | toks], [], ts0) do
    {:lists.reverse(ts0), toks}
  end

  defp one_type([{:")", lrp} | toks], [], ts0) do
    {:lists.reverse(ts0), [{:")", lrp} | toks]}
  end

  defp one_type([{:"(", llp} | toks], e, ts0) do
    one_type(toks, [:")" | e], [{:"(", llp} | ts0])
  end

  defp one_type([{:"<<", lls} | toks], e, ts0) do
    one_type(toks, [:">>" | e], [{:"<<", lls} | ts0])
  end

  defp one_type([{:"[", lls} | toks], e, ts0) do
    one_type(toks, [:"]" | e], [{:"[", lls} | ts0])
  end

  defp one_type([{:"{", llc} | toks], e, ts0) do
    one_type(toks, [:"}" | e], [{:"{", llc} | ts0])
  end

  defp one_type([{rb, lrb} | toks], [rb | e], ts0) do
    one_type(toks, e, [{rb, lrb} | ts0])
  end

  defp one_type([t | toks], e, ts0) do
    one_type(toks, e, [t | ts0])
  end
end
