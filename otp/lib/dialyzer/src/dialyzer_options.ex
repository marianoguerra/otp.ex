defmodule :m_dialyzer_options do
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

  def build(opts) do
    defaultWarns = [
      :warn_return_no_exit,
      :warn_not_called,
      :warn_non_proper_list,
      :warn_fun_app,
      :warn_matching,
      :warn_opaque,
      :warn_callgraph,
      :warn_failing_call,
      :warn_bin_construction,
      :warn_map_construction,
      :warn_contract_range,
      :warn_contract_types,
      :warn_contract_syntax,
      :warn_behaviour,
      :warn_undefined_callbacks
    ]

    defaultWarns1 = :ordsets.from_list(defaultWarns)
    initPlt = :dialyzer_plt.get_default_plt()
    defaultOpts = r_options()

    defaultOpts1 =
      r_options(defaultOpts,
        legal_warnings: defaultWarns1,
        init_plts: [initPlt]
      )

    try do
      opts1 = preprocess_opts(opts)
      newOpts = build_options(opts1, defaultOpts1)
      postprocess_opts(newOpts)
    catch
      {:dialyzer_options_error, msg} ->
        {:error, msg}
    end
  end

  defp preprocess_opts([]) do
    []
  end

  defp preprocess_opts([{:init_plt, file} | opts]) do
    [{:plts, [file]} | preprocess_opts(opts)]
  end

  defp preprocess_opts([opt | opts]) do
    [opt | preprocess_opts(opts)]
  end

  defp postprocess_opts(opts = r_options()) do
    check_file_existence(opts)
    opts1 = check_output_plt(opts)
    adapt_get_warnings(opts1)
  end

  defp check_file_existence(r_options(analysis_type: :plt_remove)) do
    :ok
  end

  defp check_file_existence(r_options(files: files, files_rec: filesRec)) do
    assert_filenames_exist(files)
    assert_filenames_exist(filesRec)
  end

  defp check_output_plt(opts = r_options(analysis_type: mode, from: from, output_plt: outPLT)) do
    case is_plt_mode(mode) do
      true ->
        case from === :byte_code do
          true ->
            opts

          false ->
            msg = 'Byte code compiled with debug_info is needed to build the PLT'
            throw({:dialyzer_error, msg})
        end

      false ->
        case outPLT === :none do
          true ->
            opts

          false ->
            msg = :io_lib.format('Output PLT cannot be specified in analysis mode ~w', [mode])
            throw({:dialyzer_error, :lists.flatten(msg)})
        end
    end
  end

  defp adapt_get_warnings(
         opts =
           r_options(
             analysis_type: mode,
             get_warnings: warns
           )
       ) do
    case is_plt_mode(mode) do
      true ->
        case warns === :maybe do
          true ->
            r_options(opts, get_warnings: false)

          false ->
            opts
        end

      false ->
        case warns === :maybe do
          true ->
            r_options(opts, get_warnings: true)

          false ->
            opts
        end
    end
  end

  defp bad_option(string, term) do
    msg = :io_lib.format('~ts: ~tP', [string, term, 25])
    throw({:dialyzer_options_error, :lists.flatten(msg)})
  end

  defp build_options([{optName, :undefined} | rest], options)
       when is_atom(optName) do
    build_options(rest, options)
  end

  defp build_options([{optionName, value} = term | rest], options) do
    case optionName do
      :apps ->
        oldValues = r_options(options, :files_rec)
        appDirs = get_app_dirs(value)
        assert_filenames_form(term, appDirs)

        build_options(
          rest,
          r_options(options, files_rec: appDirs ++ oldValues)
        )

      :files ->
        assert_filenames_form(term, value)
        build_options(rest, r_options(options, files: value))

      :files_rec ->
        oldValues = r_options(options, :files_rec)
        assert_filenames_form(term, value)

        build_options(
          rest,
          r_options(options, files_rec: value ++ oldValues)
        )

      :analysis_type ->
        newOptions =
          case value do
            :succ_typings ->
              r_options(options, analysis_type: value)

            :plt_add ->
              r_options(options, analysis_type: value)

            :plt_build ->
              r_options(options, analysis_type: value)

            :plt_check ->
              r_options(options, analysis_type: value)

            :plt_remove ->
              r_options(options, analysis_type: value)

            :dataflow ->
              bad_option('Analysis type is no longer supported', term)

            :old_style ->
              bad_option('Analysis type is no longer supported', term)

            other ->
              bad_option('Unknown analysis type', other)
          end

        assert_plt_op(options, newOptions)
        build_options(rest, newOptions)

      :check_plt when is_boolean(value) ->
        build_options(rest, r_options(options, check_plt: value))

      :defines ->
        assert_defines(term, value)
        oldVal = r_options(options, :defines)

        newVal =
          :ordsets.union(
            :ordsets.from_list(value),
            oldVal
          )

        build_options(rest, r_options(options, defines: newVal))

      :from when value === :byte_code or value === :src_code ->
        build_options(rest, r_options(options, from: value))

      :get_warnings ->
        build_options(rest, r_options(options, get_warnings: value))

      :plts ->
        assert_filenames(term, value)
        build_options(rest, r_options(options, init_plts: value))

      :include_dirs ->
        assert_filenames(term, value)
        oldVal = r_options(options, :include_dirs)

        newVal =
          :ordsets.union(
            :ordsets.from_list(value),
            oldVal
          )

        build_options(rest, r_options(options, include_dirs: newVal))

      :use_spec ->
        build_options(rest, r_options(options, use_contracts: value))

      :old_style ->
        bad_option('Analysis type is no longer supported', :old_style)

      :output_file ->
        assert_filename(value)
        build_options(rest, r_options(options, output_file: value))

      :output_format ->
        assert_output_format(value)
        build_options(rest, r_options(options, output_format: value))

      :filename_opt ->
        assert_filename_opt(value)
        build_options(rest, r_options(options, filename_opt: value))

      :indent_opt ->
        build_options(rest, r_options(options, indent_opt: value))

      :output_plt ->
        assert_filename(value)
        build_options(rest, r_options(options, output_plt: value))

      :report_mode ->
        build_options(rest, r_options(options, report_mode: value))

      :erlang_mode ->
        build_options(rest, r_options(options, erlang_mode: true))

      :warnings ->
        newWarnings =
          build_warnings(
            value,
            r_options(options, :legal_warnings)
          )

        build_options(
          rest,
          r_options(options, legal_warnings: newWarnings)
        )

      :callgraph_file ->
        assert_filename(value)
        build_options(rest, r_options(options, callgraph_file: value))

      :timing ->
        build_options(rest, r_options(options, timing: value))

      :solvers ->
        assert_solvers(value)
        build_options(rest, r_options(options, solvers: value))

      :native ->
        build_options(rest, r_options(options, native: value))

      :native_cache ->
        build_options(rest, r_options(options, native_cache: value))

      _ ->
        bad_option('Unknown dialyzer command line option', term)
    end
  end

  defp build_options([], options) do
    options
  end

  defp get_app_dirs(apps) when is_list(apps) do
    :dialyzer_cl_parse.get_lib_dir(
      for a <- apps do
        :erlang.atom_to_list(a)
      end
    )
  end

  defp get_app_dirs(apps) do
    bad_option('Use a list of otp applications', apps)
  end

  defp assert_filenames(term, files) do
    assert_filenames_form(term, files)
    assert_filenames_exist(files)
  end

  defp assert_filenames_form(term, [fileName | left])
       when length(fileName) >= 0 do
    assert_filenames_form(term, left)
  end

  defp assert_filenames_form(_Term, []) do
    :ok
  end

  defp assert_filenames_form(term, [_ | _]) do
    bad_option('Malformed or non-existing filename', term)
  end

  defp assert_filenames_exist([fileName | left]) do
    case :filelib.is_file(fileName) or :filelib.is_dir(fileName) do
      true ->
        :ok

      false ->
        bad_option('No such file, directory or application', fileName)
    end

    assert_filenames_exist(left)
  end

  defp assert_filenames_exist([]) do
    :ok
  end

  defp assert_filename(fileName) when length(fileName) >= 0 do
    :ok
  end

  defp assert_filename(fileName) do
    bad_option('Malformed or non-existing filename', fileName)
  end

  defp assert_defines(term, [{macro, _Value} | defs])
       when is_atom(macro) do
    assert_defines(term, defs)
  end

  defp assert_defines(_Term, []) do
    :ok
  end

  defp assert_defines(term, [_ | _]) do
    bad_option('Malformed define', term)
  end

  defp assert_output_format(:raw) do
    :ok
  end

  defp assert_output_format(:formatted) do
    :ok
  end

  defp assert_output_format(term) do
    bad_option('Illegal value for output_format', term)
  end

  defp assert_filename_opt(:basename) do
    :ok
  end

  defp assert_filename_opt(:fullpath) do
    :ok
  end

  defp assert_filename_opt(term) do
    bad_option('Illegal value for filename_opt', term)
  end

  defp assert_plt_op(
         r_options(analysis_type: oldVal),
         r_options(analysis_type: newVal)
       ) do
    case is_plt_mode(oldVal) and is_plt_mode(newVal) do
      true ->
        bad_option('Options cannot be combined', [oldVal, newVal])

      false ->
        :ok
    end
  end

  defp is_plt_mode(:plt_add) do
    true
  end

  defp is_plt_mode(:plt_build) do
    true
  end

  defp is_plt_mode(:plt_remove) do
    true
  end

  defp is_plt_mode(:plt_check) do
    true
  end

  defp is_plt_mode(:succ_typings) do
    false
  end

  defp assert_solvers([]) do
    :ok
  end

  defp assert_solvers([:v1 | terms]) do
    assert_solvers(terms)
  end

  defp assert_solvers([:v2 | terms]) do
    assert_solvers(terms)
  end

  defp assert_solvers([term | _]) do
    bad_option('Illegal value for solver', term)
  end

  def build_warnings([opt | opts], warnings) do
    newWarnings =
      case opt do
        :no_return ->
          :ordsets.del_element(:warn_return_no_exit, warnings)

        :no_unused ->
          :ordsets.del_element(:warn_not_called, warnings)

        :no_improper_lists ->
          :ordsets.del_element(:warn_non_proper_list, warnings)

        :no_fun_app ->
          :ordsets.del_element(:warn_fun_app, warnings)

        :no_match ->
          :ordsets.del_element(:warn_matching, warnings)

        :no_opaque ->
          :ordsets.del_element(:warn_opaque, warnings)

        :no_fail_call ->
          :ordsets.del_element(:warn_failing_call, warnings)

        :no_contracts ->
          warnings1 =
            :ordsets.del_element(
              :warn_contract_syntax,
              warnings
            )

          :ordsets.del_element(:warn_contract_types, warnings1)

        :no_behaviours ->
          :ordsets.del_element(:warn_behaviour, warnings)

        :no_undefined_callbacks ->
          :ordsets.del_element(
            :warn_undefined_callbacks,
            warnings
          )

        :unmatched_returns ->
          :ordsets.add_element(:warn_umatched_return, warnings)

        :error_handling ->
          :ordsets.add_element(:warn_return_only_exit, warnings)

        :race_conditions ->
          :ordsets.add_element(:warn_race_condition, warnings)

        :no_missing_calls ->
          :ordsets.del_element(:warn_callgraph, warnings)

        :specdiffs ->
          s =
            :ordsets.from_list([
              :warn_contract_subtype,
              :warn_contract_supertype,
              :warn_contract_not_equal
            ])

          :ordsets.union(s, warnings)

        :overspecs ->
          :ordsets.add_element(:warn_contract_subtype, warnings)

        :underspecs ->
          :ordsets.add_element(:warn_contract_supertype, warnings)

        :unknown ->
          :ordsets.add_element(:warn_unknown, warnings)

        otherAtom ->
          bad_option('Unknown dialyzer warning option', otherAtom)
      end

    build_warnings(opts, newWarnings)
  end

  def build_warnings([], warnings) do
    warnings
  end
end
