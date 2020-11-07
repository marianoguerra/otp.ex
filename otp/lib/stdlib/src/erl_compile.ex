defmodule :m_erl_compile do
  use Bitwise
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

  defp compiler('.erl') do
    {:compile, :compile}
  end

  defp compiler('.S') do
    {:compile, :compile_asm}
  end

  defp compiler('.beam') do
    {:compile, :compile_beam}
  end

  defp compiler('.core') do
    {:compile, :compile_core}
  end

  defp compiler('.mib') do
    {:snmpc, :compile}
  end

  defp compiler('.bin') do
    {:snmpc, :mib_to_hrl}
  end

  defp compiler('.xrl') do
    {:leex, :compile}
  end

  defp compiler('.yrl') do
    {:yecc, :compile}
  end

  defp compiler('.script') do
    {:systools, :script2boot}
  end

  defp compiler('.rel') do
    {:systools, :compile_rel}
  end

  defp compiler('.idl') do
    {:ic, :compile}
  end

  defp compiler('.asn1') do
    {:asn1ct, :compile_asn1}
  end

  defp compiler('.asn') do
    {:asn1ct, :compile_asn}
  end

  defp compiler('.py') do
    {:asn1ct, :compile_py}
  end

  defp compiler(_) do
    :no
  end

  def compile_cmdline() do
    cmdline_init()
    list = :init.get_plain_arguments()
    compile_cmdline1(list)
  end

  def compile(args, cwd) do
    try do
      compile1(args, r_options(outdir: cwd, cwd: cwd))
    catch
      {:error, output} ->
        {:error, :unicode.characters_to_binary(output)}

      c, e ->
        {:crash, {c, e, __STACKTRACE__}}
    else
      :ok ->
        :ok
    end
  end

  defp compile_cmdline1(args) do
    {:ok, cwd} = :file.get_cwd()

    {pid, ref} =
      spawn_monitor(fn ->
        exit(compile(args, cwd))
      end)

    receive do
      {:DOWN, ^ref, :process, ^pid, result} ->
        case result do
          :ok ->
            :erlang.halt(0)

          {:error, output} ->
            :io.put_chars(:standard_error, output)
            :erlang.halt(1)

          {:crash, {c, e, stk}} ->
            :io.format(:standard_error, 'Crash: ~p:~tp\n~tp\n', [c, e, stk])
            :erlang.halt(2)
        end
    end
  end

  defp cmdline_init() do
    path =
      for d <- :code.get_path(), d !== '.' do
        d
      end

    true = :code.set_path(path)
    :ok
  end

  defp compile1(['--' | files], opts) do
    compile2(files, opts)
  end

  defp compile1(['-' ++ option | t], opts) do
    parse_generic_option(option, t, opts)
  end

  defp compile1(['+' ++ option | rest], opts) do
    term = make_term(option)
    specific = r_options(opts, :specific)
    compile1(rest, r_options(opts, specific: [term | specific]))
  end

  defp compile1(files, opts) do
    compile2(files, opts)
  end

  defp parse_generic_option('b' ++ opt, t0, opts) do
    {outputType, t} = get_option('b', opt, t0)

    compile1(
      t,
      r_options(opts, output_type: :erlang.list_to_atom(outputType))
    )
  end

  defp parse_generic_option('D' ++ opt, t0, r_options(defines: defs) = opts) do
    {val0, t} = get_option('D', opt, t0)
    {key0, val1} = split_at_equals(val0, [])
    key = :erlang.list_to_atom(key0)

    case val1 do
      [] ->
        compile1(t, r_options(opts, defines: [key | defs]))

      val2 ->
        val = make_term(val2)
        compile1(t, r_options(opts, defines: [{key, val} | defs]))
    end
  end

  defp parse_generic_option('help', _, _Opts) do
    usage()
  end

  defp parse_generic_option('I' ++ opt, t0, r_options(cwd: cwd) = opts) do
    {dir, t} = get_option('I', opt, t0)
    absDir = :filename.absname(dir, cwd)

    compile1(
      t,
      r_options(opts, includes: [absDir | r_options(opts, :includes)])
    )
  end

  defp parse_generic_option('M' ++ opt, t0, r_options(specific: spec) = opts) do
    {specOpts, t} = parse_dep_option(opt, t0)
    compile1(t, r_options(opts, specific: specOpts ++ spec))
  end

  defp parse_generic_option('o' ++ opt, t0, r_options(cwd: cwd) = opts) do
    {dir, t} = get_option('o', opt, t0)
    absName = :filename.absname(dir, cwd)

    case file_or_directory(absName) do
      :file ->
        compile1(t, r_options(opts, outfile: absName))

      :directory ->
        compile1(t, r_options(opts, outdir: absName))
    end
  end

  defp parse_generic_option('O' ++ opt, t, opts) do
    case opt do
      '' ->
        compile1(t, r_options(opts, optimize: 1))

      _ ->
        term = make_term(opt)
        compile1(t, r_options(opts, optimize: term))
    end
  end

  defp parse_generic_option('v', t, opts) do
    compile1(t, r_options(opts, verbose: true))
  end

  defp parse_generic_option('W' ++ warn, t, r_options(specific: spec) = opts) do
    case warn do
      'all' ->
        compile1(t, r_options(opts, warning: 999))

      'error' ->
        compile1(
          t,
          r_options(opts, specific: [:warnings_as_errors | spec])
        )

      '' ->
        compile1(t, r_options(opts, warning: 1))

      _ ->
        try do
          :erlang.list_to_integer(warn)
        catch
          :error, :badarg ->
            usage()
        else
          level ->
            compile1(t, r_options(opts, warning: level))
        end
    end
  end

  defp parse_generic_option('E', t, r_options(specific: spec) = opts) do
    compile1(t, r_options(opts, specific: [:E | spec]))
  end

  defp parse_generic_option('P', t, r_options(specific: spec) = opts) do
    compile1(t, r_options(opts, specific: [:P | spec]))
  end

  defp parse_generic_option('S', t, r_options(specific: spec) = opts) do
    compile1(t, r_options(opts, specific: [:S | spec]))
  end

  defp parse_generic_option(option, _T, _Opts) do
    usage(:io_lib.format('Unknown option: -~ts\n', [option]))
  end

  defp parse_dep_option('', t) do
    {[:makedep, {:makedep_output, :standard_io}], t}
  end

  defp parse_dep_option('D', t) do
    {[:makedep], t}
  end

  defp parse_dep_option('MD', t) do
    {[:makedep_side_effect], t}
  end

  defp parse_dep_option('F' ++ opt, t0) do
    {file, t} = get_option('MF', opt, t0)
    {[:makedep, {:makedep_output, file}], t}
  end

  defp parse_dep_option('G', t) do
    {[:makedep_add_missing], t}
  end

  defp parse_dep_option('P', t) do
    {[:makedep_phony], t}
  end

  defp parse_dep_option('Q' ++ opt, t0) do
    {target, t} = get_option('MT', opt, t0)
    {[:makedep_quote_target, {:makedep_target, target}], t}
  end

  defp parse_dep_option('T' ++ opt, t0) do
    {target, t} = get_option('MT', opt, t0)
    {[{:makedep_target, target}], t}
  end

  defp parse_dep_option(opt, _T) do
    usage(:io_lib.format('Unknown option: -M~ts\n', [opt]))
  end

  defp usage() do
    usage('')
  end

  defp usage(error) do
    h = [
      {'-b type', 'type of output file (e.g. beam)'},
      {'-d', 'turn on debugging of erlc itself'},
      {'-Dname', 'define name'},
      {'-Dname=value', 'define name to have value'},
      {'-help', 'shows this help text'},
      {'-I path', 'where to search for include files'},
      {'-M', 'generate a rule for make(1) describing the dependencies'},
      {'-MF file', 'write the dependencies to \'file\''},
      {'-MT target', 'change the target of the rule emitted by dependency generation'},
      {'-MQ target', 'same as -MT but quote characters special to make(1)'},
      {'-MG', 'consider missing headers as generated files and add them to the dependencies'},
      {'-MP', 'add a phony target for each dependency'},
      {'-MD', 'same as -M -MT file (with default \'file\')'},
      {'-MMD', 'generate dependencies as a side-effect'},
      {'-o name', 'name output directory or file'},
      {'-pa path', 'add path to the front of Erlang\'s code path'},
      {'-pz path', 'add path to the end of Erlang\'s code path'},
      {'-smp', 'compile using SMP emulator'},
      {'-v', 'verbose compiler output'},
      {'-Werror', 'make all warnings into errors'},
      {'-W0', 'disable warnings'},
      {'-Wnumber', 'set warning level to number'},
      {'-Wall', 'enable all warnings'},
      {'-W', 'enable warnings (default; same as -W1)'},
      {'-E', 'generate listing of expanded code (Erlang compiler)'},
      {'-S', 'generate assembly listing (Erlang compiler)'},
      {'-P', 'generate listing of preprocessed code (Erlang compiler)'},
      {'+term', 'pass the Erlang term unchanged to the compiler'}
    ]

    msg = [
      error,
      'Usage: erlc [Options] file.ext ...\n',
      'Options:\n',
      for {k, d} <- h do
        :io_lib.format('~-14s ~s\n', [k, d])
      end
    ]

    throw({:error, msg})
  end

  defp get_option(_Name, [], [[c | _] = option | t])
       when c !== ?- do
    {option, t}
  end

  defp get_option(_Name, [_ | _] = option, t) do
    {option, t}
  end

  defp get_option(name, _, _) do
    throw({:error, 'No value given to -' ++ name ++ ' option\n'})
  end

  defp split_at_equals([?= | t], acc) do
    {:lists.reverse(acc), t}
  end

  defp split_at_equals([h | t], acc) do
    split_at_equals(t, [h | acc])
  end

  defp split_at_equals([], acc) do
    {:lists.reverse(acc), []}
  end

  defp compile2(
         files,
         r_options(cwd: cwd, includes: incl, outfile: outfile) = opts0
       ) do
    opts = r_options(opts0, includes: :lists.reverse(incl))

    case {outfile, length(files)} do
      {'', _} ->
        compile3(files, cwd, opts)

      {[_ | _], 1} ->
        compile3(files, cwd, opts)

      {[_ | _], _N} ->
        throw({:error, 'Output file name given, but more than one input file.\n'})
    end
  end

  defp compile3([file | rest], cwd, options) do
    ext = :filename.extension(file)
    root = :filename.rootname(file)
    inFile = :filename.absname(root, cwd)

    outFile =
      case r_options(options, :outfile) do
        '' ->
          :filename.join(
            r_options(options, :outdir),
            :filename.basename(root)
          )

        outfile ->
          :filename.rootname(outfile)
      end

    compile_file(ext, inFile, outFile, options)
    compile3(rest, cwd, options)
  end

  defp compile3([], _Cwd, _Options) do
    :ok
  end

  defp compile_file('', input, _Output, _Options) do
    throw({:error, :io_lib.format('File has no extension: ~ts~n', [input])})
  end

  defp compile_file(ext, input, output, options) do
    case compiler(ext) do
      :no ->
        error = :io_lib.format('Unknown extension: \'~ts\'\n', [ext])
        throw({:error, error})

      {m, f} ->
        try do
          apply(m, f, [input, output, options])
        catch
          reason ->
            error =
              :io_lib.format(
                'Compiler function ~w:~w/3 failed:\n~tp\n~tp\n',
                [m, f, reason, __STACKTRACE__]
              )

            throw({:error, error})
        else
          :ok ->
            :ok

          :error ->
            throw({:error, ''})

          other ->
            error = :io_lib.format('Compiler function ~w:~w/3 returned:\n~tp~n', [m, f, other])
            throw({:error, error})
        end
    end
  end

  defp file_or_directory(name) do
    case :file.read_file_info(name) do
      {:ok, r_file_info(type: :regular)} ->
        :file

      {:ok, _} ->
        :directory

      {:error, _} ->
        case :filename.extension(name) do
          [] ->
            :directory

          _Other ->
            :file
        end
    end
  end

  defp make_term(str) do
    case :erl_scan.string(str) do
      {:ok, tokens, _} ->
        case :erl_parse.parse_term(tokens ++ [{:dot, :erl_anno.new(1)}]) do
          {:ok, term} ->
            term

          {:error, {_, _, reason}} ->
            throw({:error, :io_lib.format('~ts: ~ts~n', [reason, str])})
        end

      {:error, {_, _, reason}, _} ->
        throw({:error, :io_lib.format('~ts: ~ts~n', [reason, str])})
    end
  end
end
