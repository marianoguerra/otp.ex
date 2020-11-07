defmodule :m_dialyzer_cl_parse do
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

  def start() do
    init()
    args = :init.get_plain_arguments()

    try do
      ret = cl(args)
      ret
    catch
      {:dialyzer_cl_parse_error, msg} ->
        {:error, msg}

      _, r ->
        msg = :io_lib.format('~tp\n~tp\n', [r, __STACKTRACE__])
        {:error, :lists.flatten(msg)}
    end
  end

  defp cl(['--add_to_plt' | t]) do
    :erlang.put(:dialyzer_options_analysis_type, :plt_add)
    cl(t)
  end

  defp cl(['--apps' | t]) do
    t1 = get_lib_dir(t)
    {args, t2} = collect_args(t1)
    append_var(:dialyzer_options_files_rec, args)
    cl(t2)
  end

  defp cl(['--build_plt' | t]) do
    :erlang.put(:dialyzer_options_analysis_type, :plt_build)
    cl(t)
  end

  defp cl(['--check_plt' | t]) do
    :erlang.put(:dialyzer_options_analysis_type, :plt_check)
    cl(t)
  end

  defp cl(['-n' | t]) do
    cl(['--no_check_plt' | t])
  end

  defp cl(['--no_check_plt' | t]) do
    :erlang.put(:dialyzer_options_check_plt, false)
    cl(t)
  end

  defp cl(['-nn' | t]) do
    cl(['--no_native' | t])
  end

  defp cl(['--no_native' | t]) do
    :erlang.put(:dialyzer_options_native, false)
    cl(t)
  end

  defp cl(['--no_native_cache' | t]) do
    :erlang.put(:dialyzer_options_native_cache, false)
    cl(t)
  end

  defp cl(['--plt_info' | t]) do
    :erlang.put(:dialyzer_options_analysis_type, :plt_info)
    cl(t)
  end

  defp cl(['--get_warnings' | t]) do
    :erlang.put(:dialyzer_options_get_warnings, true)
    cl(t)
  end

  defp cl(['-D' | _]) do
    cl_error('No defines specified after -D')
  end

  defp cl(['-D' ++ define | t]) do
    def__ = :re.split(define, '=', [{:return, :list}, :unicode])
    append_defines(def__)
    cl(t)
  end

  defp cl(['-h' | _]) do
    help_message()
  end

  defp cl(['--help' | _]) do
    help_message()
  end

  defp cl(['-I']) do
    cl_error('no include directory specified after -I')
  end

  defp cl([['-I', dir] | t]) do
    append_include(dir)
    cl(t)
  end

  defp cl(['-I' ++ dir | t]) do
    append_include(dir)
    cl(t)
  end

  defp cl(['-c' ++ _ | t]) do
    newTail = command_line(t)
    cl(newTail)
  end

  defp cl(['-r' ++ _ | t0]) do
    {args, t} = collect_args(t0)
    append_var(:dialyzer_options_files_rec, args)
    cl(t)
  end

  defp cl(['--remove_from_plt' | t]) do
    :erlang.put(
      :dialyzer_options_analysis_type,
      :plt_remove
    )

    cl(t)
  end

  defp cl(['--com' ++ _ | t]) do
    newTail = command_line(t)
    cl(newTail)
  end

  defp cl(['--output']) do
    cl_error('No outfile specified')
  end

  defp cl(['-o']) do
    cl_error('No outfile specified')
  end

  defp cl([['--output', output] | t]) do
    :erlang.put(:dialyzer_output, output)
    cl(t)
  end

  defp cl(['--output_plt']) do
    cl_error('No outfile specified for --output_plt')
  end

  defp cl([['--output_plt', output] | t]) do
    :erlang.put(:dialyzer_output_plt, output)
    cl(t)
  end

  defp cl([['-o', output] | t]) do
    :erlang.put(:dialyzer_output, output)
    cl(t)
  end

  defp cl(['-o' ++ output | t]) do
    :erlang.put(:dialyzer_output, output)
    cl(t)
  end

  defp cl(['--raw' | t]) do
    :erlang.put(:dialyzer_output_format, :raw)
    cl(t)
  end

  defp cl(['--fullpath' | t]) do
    :erlang.put(:dialyzer_filename_opt, :fullpath)
    cl(t)
  end

  defp cl(['--no_indentation' | t]) do
    :erlang.put(:dialyzer_indent_opt, false)
    cl(t)
  end

  defp cl([['-pa', path] | t]) do
    case :code.add_patha(path) do
      true ->
        cl(t)

      {:error, _} ->
        cl_error('Bad directory for -pa: ' ++ path)
    end
  end

  defp cl(['--plt']) do
    :erlang.error('No plt specified for --plt')
  end

  defp cl([['--plt', pLT] | t]) do
    :erlang.put(:dialyzer_init_plts, [pLT])
    cl(t)
  end

  defp cl(['--plts']) do
    :erlang.error('No plts specified for --plts')
  end

  defp cl(['--plts' | t]) do
    {pLTs, newT} = get_plts(t, [])
    :erlang.put(:dialyzer_init_plts, pLTs)
    cl(newT)
  end

  defp cl(['-q' | t]) do
    :erlang.put(:dialyzer_options_report_mode, :quiet)
    cl(t)
  end

  defp cl(['--quiet' | t]) do
    :erlang.put(:dialyzer_options_report_mode, :quiet)
    cl(t)
  end

  defp cl(['--src' | t]) do
    :erlang.put(:dialyzer_options_from, :src_code)
    cl(t)
  end

  defp cl(['--no_spec' | t]) do
    :erlang.put(:dialyzer_options_use_contracts, false)
    cl(t)
  end

  defp cl(['--statistics' | t]) do
    :erlang.put(:dialyzer_timing, true)
    cl(t)
  end

  defp cl(['--resources' | t]) do
    :erlang.put(:dialyzer_options_report_mode, :quiet)
    :erlang.put(:dialyzer_timing, :debug)
    cl(t)
  end

  defp cl(['-v' | _]) do
    :io.format('Dialyzer version ' ++ :EFE_TODO_VSN_MACRO ++ '\n')
    :erlang.halt(0)
  end

  defp cl(['--version' | _]) do
    :io.format('Dialyzer version ' ++ :EFE_TODO_VSN_MACRO ++ '\n')
    :erlang.halt(0)
  end

  defp cl(['--verbose' | t]) do
    :erlang.put(:dialyzer_options_report_mode, :verbose)
    cl(t)
  end

  defp cl(['-W' | _]) do
    cl_error('-W given without warning')
  end

  defp cl(['-Whelp' | _]) do
    help_warnings()
  end

  defp cl(['-W' ++ warn | t]) do
    append_var(
      :dialyzer_warnings,
      [:erlang.list_to_atom(warn)]
    )

    cl(t)
  end

  defp cl(['--dump_callgraph']) do
    cl_error('No outfile specified for --dump_callgraph')
  end

  defp cl([['--dump_callgraph', file] | t]) do
    :erlang.put(:dialyzer_callgraph_file, file)
    cl(t)
  end

  defp cl(['--gui' | t]) do
    :erlang.put(:dialyzer_options_mode, :gui)
    cl(t)
  end

  defp cl([['--solver', solver] | t]) do
    append_var(
      :dialyzer_solvers,
      [:erlang.list_to_atom(solver)]
    )

    cl(t)
  end

  defp cl([h | _] = l) do
    case :filelib.is_file(h) or :filelib.is_dir(h) do
      true ->
        newTail = command_line(l)
        cl(newTail)

      false ->
        cl_error('Unknown option: ' ++ h)
    end
  end

  defp cl([]) do
    {retTag, opts} =
      case :erlang.get(:dialyzer_options_analysis_type) === :plt_info do
        true ->
          :erlang.put(:dialyzer_options_analysis_type, :plt_check)
          {:plt_info, cl_options()}

        false ->
          case :erlang.get(:dialyzer_options_mode) do
            :gui ->
              {:gui, common_options()}

            :cl ->
              case :erlang.get(:dialyzer_options_analysis_type) === :plt_check do
                true ->
                  {:check_init, cl_options()}

                false ->
                  {:cl, cl_options()}
              end
          end
      end

    case :dialyzer_options.build(opts) do
      {:error, msg} ->
        cl_error(msg)

      optsRecord ->
        {retTag, optsRecord}
    end
  end

  defp command_line(t0) do
    {args, t} = collect_args(t0)
    append_var(:dialyzer_options_files, args)

    case :lists.all(
           fn f ->
             :filename.extension(f) === '.erl'
           end,
           args
         ) do
      true ->
        :erlang.put(:dialyzer_options_from, :src_code)

      false ->
        :ok
    end

    t
  end

  defp cl_error(str) do
    msg = :lists.flatten(str)
    throw({:dialyzer_cl_parse_error, msg})
  end

  defp init() do
    :erlang.put(:dialyzer_options_mode, :cl)
    :erlang.put(:dialyzer_options_files_rec, [])
    :erlang.put(:dialyzer_options_report_mode, :normal)
    :erlang.put(:dialyzer_warnings, [])
    defaultOpts = r_options()

    :erlang.put(
      :dialyzer_include,
      r_options(defaultOpts, :include_dirs)
    )

    :erlang.put(
      :dialyzer_options_defines,
      r_options(defaultOpts, :defines)
    )

    :erlang.put(
      :dialyzer_options_files,
      r_options(defaultOpts, :files)
    )

    :erlang.put(:dialyzer_output_format, :formatted)
    :erlang.put(:dialyzer_filename_opt, :basename)
    :erlang.put(:dialyzer_indent_opt, true)

    :erlang.put(
      :dialyzer_options_check_plt,
      r_options(defaultOpts, :check_plt)
    )

    :erlang.put(:dialyzer_timing, r_options(defaultOpts, :timing))
    :erlang.put(:dialyzer_solvers, r_options(defaultOpts, :solvers))
    :ok
  end

  defp append_defines([def__, val]) do
    {:ok, tokens, _} = :erl_scan.string(val ++ '.')
    {:ok, erlVal} = :erl_parse.parse_term(tokens)

    append_var(
      :dialyzer_options_defines,
      [{:erlang.list_to_atom(def__), erlVal}]
    )
  end

  defp append_defines([def__]) do
    append_var(
      :dialyzer_options_defines,
      [{:erlang.list_to_atom(def__), true}]
    )
  end

  defp append_include(dir) do
    append_var(:dialyzer_include, [dir])
  end

  defp append_var(var, list) when is_list(list) do
    :erlang.put(var, :erlang.get(var) ++ list)
    :ok
  end

  def collect_args(list) do
    collect_args_1(list, [])
  end

  defp collect_args_1(['-' ++ _ | _] = l, acc) do
    {:lists.reverse(acc), l}
  end

  defp collect_args_1([arg | t], acc) do
    collect_args_1(t, [arg | acc])
  end

  defp collect_args_1([], acc) do
    {:lists.reverse(acc), []}
  end

  defp cl_options() do
    [
      [
        {:files, :erlang.get(:dialyzer_options_files)},
        {:files_rec, :erlang.get(:dialyzer_options_files_rec)},
        {:output_file, :erlang.get(:dialyzer_output)},
        {:output_format, :erlang.get(:dialyzer_output_format)},
        {:filename_opt, :erlang.get(:dialyzer_filename_opt)},
        {:indent_opt, :erlang.get(:dialyzer_indent_opt)},
        {:analysis_type, :erlang.get(:dialyzer_options_analysis_type)},
        {:get_warnings, :erlang.get(:dialyzer_options_get_warnings)},
        {:timing, :erlang.get(:dialyzer_timing)},
        {:callgraph_file, :erlang.get(:dialyzer_callgraph_file)}
      ]
      | common_options()
    ]
  end

  defp common_options() do
    [
      {:defines, :erlang.get(:dialyzer_options_defines)},
      {:from, :erlang.get(:dialyzer_options_from)},
      {:include_dirs, :erlang.get(:dialyzer_include)},
      {:plts, :erlang.get(:dialyzer_init_plts)},
      {:output_plt, :erlang.get(:dialyzer_output_plt)},
      {:report_mode, :erlang.get(:dialyzer_options_report_mode)},
      {:use_spec, :erlang.get(:dialyzer_options_use_contracts)},
      {:warnings, :erlang.get(:dialyzer_warnings)},
      {:check_plt, :erlang.get(:dialyzer_options_check_plt)},
      {:solvers, :erlang.get(:dialyzer_solvers)},
      {:native, :erlang.get(:dialyzer_options_native)},
      {:native_cache, :erlang.get(:dialyzer_options_native_cache)}
    ]
  end

  def get_lib_dir(apps) do
    get_lib_dir(apps, [])
  end

  defp get_lib_dir([h | t], acc) do
    newElem =
      case :code.lib_dir(:erlang.list_to_atom(h)) do
        {:error, :bad_name} ->
          h

        libDir when h === 'erts' ->
          ebinDir = :filename.join([libDir, 'ebin'])

          case :file.read_file_info(ebinDir) do
            {:error, :enoent} ->
              :filename.join([libDir, 'preloaded', 'ebin'])

            _ ->
              ebinDir
          end

        libDir ->
          :filename.join(libDir, 'ebin')
      end

    get_lib_dir(t, [newElem | acc])
  end

  defp get_lib_dir([], acc) do
    :lists.reverse(acc)
  end

  defp get_plts(['--' | t], acc) do
    {:lists.reverse(acc), t}
  end

  defp get_plts(['-' ++ _Opt = h | t], acc) do
    {:lists.reverse(acc), [h | t]}
  end

  defp get_plts([h | t], acc) do
    get_plts(t, [h | acc])
  end

  defp get_plts([], acc) do
    {:lists.reverse(acc), []}
  end

  defp help_warnings() do
    s = warning_options_msg()
    :io.put_chars(s)
    :erlang.halt(0)
  end

  defp help_message() do
    s =
      'Usage: dialyzer [--help] [--version] [--shell] [--quiet] [--verbose]\n\t\t[-pa dir]* [--plt plt] [--plts plt*] [-Ddefine]*\n                [-I include_dir]* [--output_plt file] [-Wwarn]* [--raw]\n                [--src] [--gui] [files_or_dirs] [-r dirs]\n                [--apps applications] [-o outfile]\n\t\t[--build_plt] [--add_to_plt] [--remove_from_plt]\n\t\t[--check_plt] [--no_check_plt] [--plt_info] [--get_warnings]\n                [--dump_callgraph file] [--no_native] [--fullpath]\n                [--no_indentation] [--statistics] [--no_native_cache]\nOptions:\n  files_or_dirs (for backwards compatibility also as: -c files_or_dirs)\n      Use Dialyzer from the command line to detect defects in the\n      specified files or directories containing .erl or .beam files,\n      depending on the type of the analysis.\n  -r dirs\n      Same as the previous but the specified directories are searched\n      recursively for subdirectories containing .erl or .beam files in\n      them, depending on the type of analysis.\n  --apps applications\n      Option typically used when building or modifying a plt as in:\n        dialyzer --build_plt --apps erts kernel stdlib mnesia ...\n      to conveniently refer to library applications corresponding to the\n      Erlang/OTP installation. However, the option is general and can also\n      be used during analysis in order to refer to Erlang/OTP applications.\n      In addition, file or directory names can also be included, as in:\n        dialyzer --apps inets ssl ./ebin ../other_lib/ebin/my_module.beam\n  -o outfile (or --output outfile)\n      When using Dialyzer from the command line, send the analysis\n      results to the specified outfile rather than to stdout.\n  --raw\n      When using Dialyzer from the command line, output the raw analysis\n      results (Erlang terms) instead of the formatted result.\n      The raw format is easier to post-process (for instance, to filter\n      warnings or to output HTML pages).\n  --src\n      Override the default, which is to analyze BEAM files, and\n      analyze starting from Erlang source code instead.\n  -Dname (or -Dname=value)\n      When analyzing from source, pass the define to Dialyzer. (**)\n  -I include_dir\n      When analyzing from source, pass the include_dir to Dialyzer. (**)\n  -pa dir\n      Include dir in the path for Erlang (useful when analyzing files\n      that have \'-include_lib()\' directives).\n  --output_plt file\n      Store the plt at the specified file after building it.\n  --plt plt\n      Use the specified plt as the initial plt (if the plt was built \n      during setup the files will be checked for consistency).\n  --plts plt*\n      Merge the specified plts to create the initial plt -- requires\n      that the plts are disjoint (i.e., do not have any module\n      appearing in more than one plt).\n      The plts are created in the usual way:\n        dialyzer --build_plt --output_plt plt_1 files_to_include\n        ...\n        dialyzer --build_plt --output_plt plt_n files_to_include\n      and then can be used in either of the following ways:\n        dialyzer files_to_analyze --plts plt_1 ... plt_n\n      or:\n        dialyzer --plts plt_1 ... plt_n -- files_to_analyze\n      (Note the -- delimiter in the second case)\n  -Wwarn\n      A family of options which selectively turn on/off warnings\n      (for help on the names of warnings use dialyzer -Whelp).\n  --shell\n      Do not disable the Erlang shell while running the GUI.\n  --version (or -v)\n      Print the Dialyzer version and some more information and exit.\n  --help (or -h)\n      Print this message and exit.\n  --quiet (or -q)\n      Make Dialyzer a bit more quiet.\n  --verbose\n      Make Dialyzer a bit more verbose.\n  --statistics\n      Prints information about the progress of execution (analysis phases,\n      time spent in each and size of the relative input).\n  --build_plt\n      The analysis starts from an empty plt and creates a new one from the\n      files specified with -c and -r. Only works for beam files.\n      Use --plt(s) or --output_plt to override the default plt location.\n  --add_to_plt\n      The plt is extended to also include the files specified with -c and -r.\n      Use --plt(s) to specify which plt to start from, and --output_plt to\n      specify where to put the plt. Note that the analysis might include\n      files from the plt if they depend on the new files.\n      This option only works with beam files.\n  --remove_from_plt\n      The information from the files specified with -c and -r is removed\n      from the plt. Note that this may cause a re-analysis of the remaining\n      dependent files.\n  --check_plt\n      Check the plt for consistency and rebuild it if it is not up-to-date.\n      Actually, this option is of rare use as it is on by default.\n  --no_check_plt (or -n)\n      Skip the plt check when running Dialyzer. Useful when working with\n      installed plts that never change.\n  --plt_info\n      Make Dialyzer print information about the plt and then quit. The plt\n      can be specified with --plt(s).\n  --get_warnings\n      Make Dialyzer emit warnings even when manipulating the plt. Warnings\n      are only emitted for files that are actually analyzed.\n  --dump_callgraph file\n      Dump the call graph into the specified file whose format is determined\n      by the file name extension. Supported extensions are: raw, dot, and ps.\n      If something else is used as file name extension, default format \'.raw\'\n      will be used.\n  --no_native (or -nn)\n      Bypass the native code compilation of some key files that Dialyzer\n      heuristically performs when dialyzing many files; this avoids the\n      compilation time but it may result in (much) longer analysis time.\n  --no_native_cache\n      By default, Dialyzer caches the results of native compilation in the\n      $XDG_CACHE_HOME/erlang/dialyzer_hipe_cache directory.\n      XDG_CACHE_HOME defaults to $HOME/.cache.  Use this option to disable\n      caching.\n  --fullpath\n      Display the full path names of files for which warnings are emitted.\n  --no_indentation\n      Do not indent contracts and success typings. Note that this option has\n      no effect when combined with the --raw option.\n  --gui\n      Use the GUI.\n\nNote:\n  * denotes that multiple occurrences of these options are possible.\n ** options -D and -I work both from command-line and in the Dialyzer GUI;\n    the syntax of defines and includes is the same as that used by "erlc".\n\n' ++
        warning_options_msg() ++
        '\nThe exit status of the command line version is:\n  0 - No problems were encountered during the analysis and no\n      warnings were emitted.\n  1 - Problems were encountered during the analysis.\n  2 - No problems were encountered, but warnings were emitted.\n'

    :io.put_chars(s)
    :erlang.halt(0)
  end

  defp warning_options_msg() do
    'Warning options:\n  -Wno_return\n     Suppress warnings for functions that will never return a value.\n  -Wno_unused\n     Suppress warnings for unused functions.\n  -Wno_improper_lists\n     Suppress warnings for construction of improper lists.\n  -Wno_fun_app\n     Suppress warnings for fun applications that will fail.\n  -Wno_match\n     Suppress warnings for patterns that are unused or cannot match.\n  -Wno_opaque\n     Suppress warnings for violations of opacity of data types.\n  -Wno_fail_call\n     Suppress warnings for failing calls.\n  -Wno_contracts\n     Suppress warnings about invalid contracts.\n  -Wno_behaviours\n     Suppress warnings about behaviour callbacks which drift from the published\n     recommended interfaces.\n  -Wno_missing_calls\n     Suppress warnings about calls to missing functions.\n  -Wno_undefined_callbacks\n     Suppress warnings about behaviours that have no -callback attributes for\n     their callbacks.\n  -Wunmatched_returns ***\n     Include warnings for function calls which ignore a structured return\n     value or do not match against one of many possible return value(s).\n  -Werror_handling ***\n     Include warnings for functions that only return by means of an exception.\n  -Wrace_conditions ***\n     Include warnings for possible race conditions.\n  -Wunderspecs ***\n     Warn about underspecified functions\n     (those whose -spec is strictly more allowing than the success typing).\n  -Wunknown ***\n     Let warnings about unknown functions and types affect the\n     exit status of the command line version. The default is to ignore\n     warnings about unknown functions and types when setting the exit\n     status. When using the Dialyzer from Erlang, warnings about unknown\n     functions and types are returned; the default is not to return\n     such warnings.\n\nThe following options are also available but their use is not recommended:\n(they are mostly for Dialyzer developers and internal debugging)\n  -Woverspecs ***\n     Warn about overspecified functions\n     (those whose -spec is strictly less allowing than the success typing).\n  -Wspecdiffs ***\n     Warn when the -spec is different than the success typing.\n\n*** Identifies options that turn on warnings rather than turning them off.\n'
  end
end
