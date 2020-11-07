defmodule :m_c do
  use Bitwise
  import :io, only: [format: 1, format: 2]

  import :lists,
    only: [
      flatmap: 2,
      flatten: 1,
      foldl: 3,
      foreach: 2,
      keysort: 2,
      max: 1,
      min: 1,
      reverse: 1,
      sort: 1,
      sublist: 3
    ]

  require Record

  Record.defrecord(:r_docs_v1, :docs_v1,
    anno: :undefined,
    beam_language: :erlang,
    format: "application/erlang+html",
    module_doc: :undefined,
    metadata: %{:otp_doc_vsn => {1, 0, 0}},
    docs: :undefined
  )

  Record.defrecord(:r_docs_v1_entry, :docs_v1_entry,
    kind_name_arity: :undefined,
    anno: :undefined,
    signature: :undefined,
    doc: :undefined,
    metadata: :undefined
  )

  def help() do
    :io.put_chars(
      "bt(Pid)    -- stack backtrace for a process\nc(Mod)     -- compile and load module or file <Mod>\ncd(Dir)    -- change working directory\nflush()    -- flush any messages sent to the shell\nhelp()     -- help info\nh(M)       -- module documentation\nh(M,F)     -- module function documentation\nh(M,F,A)   -- module function arity documentation\ni()        -- information about the system\nni()       -- information about the networked system\ni(X,Y,Z)   -- information about pid <X,Y,Z>\nl(Module)  -- load or reload module\nlm()       -- load all modified modules\nlc([File]) -- compile a list of Erlang modules\nls()       -- list files in the current directory\nls(Dir)    -- list files in directory <Dir>\nm()        -- which modules are loaded\nm(Mod)     -- information about module <Mod>\nmm()       -- list all modified modules\nmemory()   -- memory allocation information\nmemory(T)  -- memory allocation information of type <T>\nnc(File)   -- compile and load code in <File> on all nodes\nnl(Module) -- load module on all nodes\npid(X,Y,Z) -- convert X,Y,Z to a Pid\npwd()      -- print working directory\nq()        -- quit - shorthand for init:stop()\nregs()     -- information about registered processes\nnregs()    -- information about all registered processes\nuptime()   -- print node uptime\nxm(M)      -- cross reference check a module\ny(File)    -- generate a Yecc parser\n"
    )
  end

  def c(module) do
    c(module, [])
  end

  def c(module, singleOption) when not is_list(singleOption) do
    c(module, [singleOption])
  end

  def c(module, opts) when is_atom(module) do
    suffix =
      case :filename.extension(module) do
        '' ->
          src_suffix(opts)

        s ->
          s
      end

    srcFile = :filename.rootname(module, suffix) ++ suffix

    case :filelib.is_file(srcFile) do
      true ->
        compile_and_load(srcFile, opts)

      false ->
        c(module, opts, fn _ ->
          true
        end)
    end
  end

  def c(module, opts) do
    compile_and_load(module, opts)
  end

  def c(module, options, filter) when is_atom(module) do
    case find_beam(module) do
      beamFile when is_list(beamFile) ->
        c(module, options, filter, beamFile)

      error ->
        {:error, error}
    end
  end

  defp c(module, options, filter, beamFile) do
    case compile_info(module, beamFile) do
      info when is_list(info) ->
        case find_source(beamFile, info) do
          srcFile when is_list(srcFile) ->
            c(srcFile, options, filter, beamFile, info)

          error ->
            error
        end

      error ->
        error
    end
  end

  defp c(srcFile, newOpts, filter, beamFile, info) do
    f = fn opt ->
      not is_outdir_opt(opt) and filter.(opt)
    end

    options =
      newOpts ++
        [{:outdir, :filename.dirname(beamFile)}] ++
        :lists.filter(
          f,
          old_options(info)
        )

    format('Recompiling ~ts\n', [srcFile])
    safe_recompile(srcFile, options, beamFile)
  end

  def h(module) do
    case :code.get_doc(module) do
      {:ok, r_docs_v1(format: "application/erlang+html") = docs} ->
        format_docs(:shell_docs.render(module, docs))

      {:ok, r_docs_v1(format: enc)} ->
        {:error, {:unknown_format, enc}}

      error ->
        error
    end
  end

  def h(module, function) do
    case :code.get_doc(module) do
      {:ok, r_docs_v1(format: "application/erlang+html") = docs} ->
        format_docs(:shell_docs.render(module, function, docs))

      {:ok, r_docs_v1(format: enc)} ->
        {:error, {:unknown_format, enc}}

      error ->
        error
    end
  end

  def h(module, function, arity) do
    case :code.get_doc(module) do
      {:ok, r_docs_v1(format: "application/erlang+html") = docs} ->
        format_docs(:shell_docs.render(module, function, arity, docs))

      {:ok, r_docs_v1(format: enc)} ->
        {:error, {:unknown_format, enc}}

      error ->
        error
    end
  end

  def ht(module) do
    case :code.get_doc(module) do
      {:ok, r_docs_v1(format: "application/erlang+html") = docs} ->
        format_docs(:shell_docs.render_type(module, docs))

      {:ok, r_docs_v1(format: enc)} ->
        {:error, {:unknown_format, enc}}

      error ->
        error
    end
  end

  def ht(module, type) do
    case :code.get_doc(module) do
      {:ok, r_docs_v1(format: "application/erlang+html") = docs} ->
        format_docs(:shell_docs.render_type(module, type, docs))

      {:ok, r_docs_v1(format: enc)} ->
        {:error, {:unknown_format, enc}}

      error ->
        error
    end
  end

  def ht(module, type, arity) do
    case :code.get_doc(module) do
      {:ok, r_docs_v1(format: "application/erlang+html") = docs} ->
        format_docs(:shell_docs.render_type(module, type, arity, docs))

      {:ok, r_docs_v1(format: enc)} ->
        {:error, {:unknown_format, enc}}

      error ->
        error
    end
  end

  def hcb(module) do
    case :code.get_doc(module) do
      {:ok, r_docs_v1(format: "application/erlang+html") = docs} ->
        format_docs(:shell_docs.render_callback(module, docs))

      {:ok, r_docs_v1(format: enc)} ->
        {:error, {:unknown_format, enc}}

      error ->
        error
    end
  end

  def hcb(module, callback) do
    case :code.get_doc(module) do
      {:ok, r_docs_v1(format: "application/erlang+html") = docs} ->
        format_docs(:shell_docs.render_callback(module, callback, docs))

      {:ok, r_docs_v1(format: enc)} ->
        {:error, {:unknown_format, enc}}

      error ->
        error
    end
  end

  def hcb(module, callback, arity) do
    case :code.get_doc(module) do
      {:ok, r_docs_v1(format: "application/erlang+html") = docs} ->
        format_docs(:shell_docs.render_callback(module, callback, arity, docs))

      {:ok, r_docs_v1(format: enc)} ->
        {:error, {:unknown_format, enc}}

      error ->
        error
    end
  end

  defp format_docs({:error, _} = e) do
    e
  end

  defp format_docs(docs) do
    {:match, lines} =
      :re.run(docs, '(.+\n|\n)', [:unicode, :global, {:capture, :all_but_first, :binary}])

    _ =
      paged_output(
        fn line, _ ->
          format('~ts', line)
          {1, :undefined}
        end,
        :undefined,
        lines
      )

    :ok
  end

  defp old_options(info) do
    case :lists.keyfind(:options, 1, info) do
      {:options, opts} ->
        opts

      false ->
        []
    end
  end

  defp find_source(beamFile, info) do
    case :lists.keyfind(:source, 1, info) do
      {:source, srcFile} ->
        case :filelib.is_file(srcFile) do
          true ->
            srcFile

          false ->
            find_source(beamFile)
        end

      _ ->
        find_source(beamFile)
    end
  end

  defp find_source(beamFile) do
    case :filelib.find_source(beamFile) do
      {:ok, srcFile} ->
        srcFile

      _ ->
        {:error, :no_source}
    end
  end

  defp find_beam(module) when is_atom(module) do
    case :code.which(module) do
      beam when is_list(beam) and beam !== '' ->
        case :erlang.module_loaded(module) do
          false ->
            beam

          true ->
            case :filelib.is_file(beam) do
              true ->
                beam

              false ->
                find_beam_1(module)
            end
        end

      other when other === '' or other === :cover_compiled ->
        find_beam_1(module)

      error ->
        error
    end
  end

  defp find_beam_1(module) do
    file = :erlang.atom_to_list(module) ++ :code.objfile_extension()

    case :code.where_is_file(file) do
      beam when is_list(beam) ->
        beam

      error ->
        error
    end
  end

  defp compile_info(module, beam) when is_atom(module) do
    case :erlang.module_loaded(module) do
      true ->
        try do
          :erlang.get_module_info(module, :compile)
        catch
          _, _ ->
            []
        end

      false ->
        case :beam_lib.chunks(beam, [:compile_info]) do
          {:ok, {_Module, [{:compile_info, info}]}} ->
            info

          error ->
            error
        end
    end
  end

  defp safe_recompile(file, options, beamFile) do
    backup = beamFile ++ '.bak'

    case :file.rename(beamFile, backup) do
      status
      when status === :ok or
             status === {:error, :enoent} ->
        case compile_and_load(file, options) do
          {:ok, _} = result ->
            _ =
              cond do
                status === :ok ->
                  :file.delete(backup)

                true ->
                  :ok
              end

            result

          error ->
            _ =
              cond do
                status === :ok ->
                  :file.rename(backup, beamFile)

                true ->
                  :ok
              end

            error
        end

      error ->
        error
    end
  end

  defp compile_and_load(file, opts0) when is_list(opts0) do
    opts = [
      [:report_errors, :report_warnings]
      | ensure_from(
          :filename.extension(file),
          ensure_outdir('.', opts0)
        )
    ]

    case :compile.file(file, opts) do
      {:ok, mod} ->
        purge_and_load(mod, file, opts)

      {:ok, mod, _Ws} ->
        purge_and_load(mod, file, opts)

      other ->
        other
    end
  end

  defp compile_and_load(file, opt) do
    compile_and_load(file, [opt])
  end

  defp ensure_from(suffix, opts0) do
    case :lists.partition(
           &is_from_opt/1,
           opts0 ++ from_opt(suffix)
         ) do
      {[opt | _], opts} ->
        [opt | opts]

      {[], opts} ->
        opts
    end
  end

  defp ensure_outdir(dir, opts0) do
    {[opt | _], opts} =
      :lists.partition(
        &is_outdir_opt/1,
        opts0 ++ [{:outdir, dir}]
      )

    [opt | opts]
  end

  defp is_outdir_opt({:outdir, _}) do
    true
  end

  defp is_outdir_opt(_) do
    false
  end

  defp is_from_opt(:from_core) do
    true
  end

  defp is_from_opt(:from_asm) do
    true
  end

  defp is_from_opt(:from_beam) do
    true
  end

  defp is_from_opt(_) do
    false
  end

  defp from_opt('.core') do
    [:from_core]
  end

  defp from_opt('.S') do
    [:from_asm]
  end

  defp from_opt('.beam') do
    [:from_beam]
  end

  defp from_opt(_) do
    []
  end

  defp outdir([]) do
    '.'
  end

  defp outdir([opt | rest]) do
    case opt do
      {:outdir, d} ->
        d

      _ ->
        outdir(rest)
    end
  end

  defp src_suffix([:from_core | _]) do
    '.core'
  end

  defp src_suffix([:from_asm | _]) do
    '.S'
  end

  defp src_suffix([:from_beam | _]) do
    '.beam'
  end

  defp src_suffix([_ | opts]) do
    src_suffix(opts)
  end

  defp src_suffix([]) do
    '.erl'
  end

  defp purge_and_load(mod, file, opts) do
    dir = outdir(opts)
    base = :filename.basename(file, src_suffix(opts))
    outFile = :filename.join(dir, base)

    case :compile.output_generated(opts) do
      true ->
        case :erlang.atom_to_list(mod) do
          ^base ->
            :code.purge(mod)

            case :code.load_abs(outFile, mod) do
              {:error, _R} = error ->
                error

              _ ->
                {:ok, mod}
            end

          _OtherMod ->
            format('** Module name \'~p\' does not match file name \'~tp\' **~n', [mod, file])
            {:error, :badfile}
        end

      false ->
        format('** Warning: No object file created - nothing loaded **~n', [])
        :ok
    end
  end

  def lc(args) do
    case (try do
            split(args, [], [])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :error ->
        :error

      {opts, files} ->
        cOpts = [
          [:report_errors, :report_warnings]
          | reverse(opts)
        ]

        foreach(
          fn file ->
            :compile.file(file, cOpts)
          end,
          reverse(files)
        )
    end
  end

  def lc_batch() do
    :io.format('Error: no files to compile~n')
    :erlang.halt(1)
  end

  def lc_batch(args) do
    try do
      split(args, [], [])
    catch
      :error ->
        :erlang.halt(1)
    else
      {opts, files} ->
        cOpts = [
          [:report_errors, :report_warnings]
          | reverse(opts)
        ]

        res =
          for file <- reverse(files) do
            :compile.file(file, cOpts)
          end

        case :lists.member(:error, res) do
          true ->
            :erlang.halt(1)

          false ->
            :erlang.halt(0)
        end
    end
  end

  defp split([[:"@i", dir] | t], opts, files) do
    split(t, [{:i, :erlang.atom_to_list(dir)} | opts], files)
  end

  defp split([[:"@o", dir] | t], opts, files) do
    split(t, [{:outdir, :erlang.atom_to_list(dir)} | opts], files)
  end

  defp split([[:"@d", def__] | t], opts, files) do
    split(
      t,
      [split_def(:erlang.atom_to_list(def__), []) | opts],
      files
    )
  end

  defp split([file | t], opts, files) do
    split(t, opts, [file | files])
  end

  defp split([], opts, files) do
    {opts, files}
  end

  defp split_def([?= | t], res) do
    {:d, :erlang.list_to_atom(reverse(res)), make_term(t)}
  end

  defp split_def([h | t], res) do
    split_def(t, [h | res])
  end

  defp split_def([], res) do
    {:d, :erlang.list_to_atom(reverse(res))}
  end

  defp make_term(str) do
    case :erl_scan.string(str) do
      {:ok, tokens, _} ->
        case :erl_parse.parse_term(tokens ++ [{:dot, :erl_anno.new(1)}]) do
          {:ok, term} ->
            term

          {:error, {_, _, reason}} ->
            :io.format('~ts: ~ts~n', [reason, str])
            throw(:error)
        end

      {:error, {_, _, reason}, _} ->
        :io.format('~ts: ~ts~n', [reason, str])
        throw(:error)
    end
  end

  def nc(file) do
    nc(file, [])
  end

  def nc(file, opts0) when is_list(opts0) do
    opts = opts0 ++ [:report_errors, :report_warnings]

    case :compile.file(file, opts) do
      {:ok, mod} ->
        dir = outdir(opts)

        obj =
          :filename.basename(
            file,
            '.erl'
          ) ++ :code.objfile_extension()

        fname = :filename.join(dir, obj)

        case :file.read_file(fname) do
          {:ok, bin} ->
            :rpc.eval_everywhere(:code, :load_binary, [mod, fname, bin])
            {:ok, mod}

          other ->
            other
        end

      other ->
        other
    end
  end

  def nc(file, opt) when is_atom(opt) do
    nc(file, [opt])
  end

  def l(mod) do
    :code.purge(mod)
    :code.load_file(mod)
  end

  def nl(mod) do
    case :code.get_object_code(mod) do
      {_Module, bin, fname} ->
        :rpc.eval_everywhere(:code, :load_binary, [mod, fname, bin])

      other ->
        other
    end
  end

  def i() do
    i(:erlang.processes())
  end

  def ni() do
    i(all_procs())
  end

  def i(ps) do
    iformat('Pid', 'Initial Call', 'Heap', 'Reds', 'Msgs')
    iformat('Registered', 'Current Function', 'Stack', '', '')

    case paged_output(
           fn pid, {r, m, h, s} ->
             {a, b, c, d} = display_info(pid)
             {2, {r + a, m + b, h + c, s + d}}
           end,
           2,
           {0, 0, 0, 0},
           ps
         ) do
      {r, m, h, s} ->
        iformat('Total', '', w(h), w(r), w(m))
        iformat('', '', w(s), '', '')

      :less ->
        :ok
    end
  end

  defp paged_output(fun, acc, items) do
    paged_output(fun, 0, acc, items)
  end

  defp paged_output(fun, currLine, acc, items) do
    limit =
      case :io.rows() do
        {:ok, rows} ->
          rows - 2

        _ ->
          100
      end

    paged_output(fun, currLine, limit, acc, items)
  end

  defp paged_output(printFun, currLine, limit, acc, items)
       when currLine >= limit do
    case more() do
      :more ->
        paged_output(printFun, 0, limit, acc, items)

      :less ->
        :less
    end
  end

  defp paged_output(printFun, currLine, limit, acc, [h | t]) do
    {lines, newAcc} = printFun.(h, acc)
    paged_output(printFun, currLine + lines, limit, newAcc, t)
  end

  defp paged_output(_, _, _, acc, []) do
    acc
  end

  defp more() do
    case get_line(:"more (y/n)? (y) ", 'y\n') do
      'c\n' ->
        :more

      'y\n' ->
        :more

      'q\n' ->
        :less

      'n\n' ->
        :less

      _ ->
        more()
    end
  end

  defp get_line(p, default) do
    case line_string(:io.get_line(p)) do
      '\n' ->
        default

      l ->
        l
    end
  end

  defp line_string(binary) when is_binary(binary) do
    :unicode.characters_to_list(binary)
  end

  defp line_string(other) do
    other
  end

  defp mfa_string(fun) when is_function(fun) do
    {:module, m} = :erlang.fun_info(fun, :module)
    {:name, f} = :erlang.fun_info(fun, :name)
    {:arity, a} = :erlang.fun_info(fun, :arity)
    mfa_string({m, f, a})
  end

  defp mfa_string({m, f, a}) do
    :io_lib.format('~w:~tw/~w', [m, f, a])
  end

  defp mfa_string(x) do
    w(x)
  end

  def display_info(pid) do
    case pinfo(pid) do
      :undefined ->
        {0, 0, 0, 0}

      info ->
        call = initial_call(info)

        curr =
          case fetch(:current_function, info) do
            {mod, f, args} when is_list(args) ->
              {mod, f, length(args)}

            other ->
              other
          end

        reds = fetch(:reductions, info)
        lM = fetch(:message_queue_len, info)
        hS = fetch(:heap_size, info)
        sS = fetch(:stack_size, info)
        iformat(w(pid), mfa_string(call), w(hS), w(reds), w(lM))

        iformat(
          case fetch(:registered_name, info) do
            0 ->
              ''

            x ->
              :io_lib.format('~tw', [x])
          end,
          mfa_string(curr),
          w(sS),
          '',
          ''
        )

        {reds, lM, hS, sS}
    end
  end

  defp initial_call(info) do
    case fetch(:initial_call, info) do
      {:proc_lib, :init_p, _} ->
        :proc_lib.translate_initial_call(info)

      iCall ->
        iCall
    end
  end

  defp iformat(a1, a2, a3, a4, a5) do
    format('~-21ts ~-33ts ~8s ~8s ~4s~n', [a1, a2, a3, a4, a5])
  end

  defp all_procs() do
    case :erlang.is_alive() do
      true ->
        flatmap(
          fn n ->
            :rpc.call(n, :erlang, :processes, [])
          end,
          [node() | :erlang.nodes()]
        )

      false ->
        :erlang.processes()
    end
  end

  defp pinfo(pid) do
    case :erlang.is_alive() do
      true ->
        :rpc.call(node(pid), :erlang, :process_info, [pid])

      false ->
        :erlang.process_info(pid)
    end
  end

  defp fetch(key, info) do
    case :lists.keyfind(key, 1, info) do
      {_, val} ->
        val

      false ->
        0
    end
  end

  def pid(x, y, z) do
    :erlang.list_to_pid(
      '<' ++
        :erlang.integer_to_list(x) ++
        '.' ++ :erlang.integer_to_list(y) ++ '.' ++ :erlang.integer_to_list(z) ++ '>'
    )
  end

  def i(x, y, z) do
    pinfo(pid(x, y, z))
  end

  def q() do
    :init.stop()
  end

  def bt(pid) do
    case (try do
            :erlang.process_display(pid, :backtrace)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        :undefined

      _ ->
        :ok
    end
  end

  def m() do
    mformat('Module', 'File')

    foreach(
      fn {mod, file} ->
        mformat(mod, file)
      end,
      sort(:code.all_loaded())
    )
  end

  defp mformat(a1, a2) do
    format('~-20s  ~ts\n', [a1, a2])
  end

  def mm() do
    :code.modified_modules()
  end

  def lm() do
    for m <- mm() do
      l(m)
    end
  end

  def erlangrc() do
    case :init.get_argument(:home) do
      {:ok, [[home]]} ->
        erlangrc([home])

      _ ->
        {:error, :enoent}
    end
  end

  def erlangrc([home | _] = paths) when is_list(home) do
    f_p_e(paths, '.erlang')
  end

  defp error(fmt, args) do
    :error_logger.error_msg(fmt, args)
  end

  defp f_p_e(p, f) do
    case :file.path_eval(p, f) do
      {:error, :enoent} = enoent ->
        enoent

      {:error, e = {line, _Mod, _Term}} ->
        :erlang.error('file:path_eval(~tp,~tp): error on line ~p: ~ts~n', [
          p,
          f,
          line,
          :file.format_error(e)
        ])

        {:error, e}

      {:error, e} ->
        :erlang.error('file:path_eval(~tp,~tp): ~ts~n', [p, f, :file.format_error(e)])
        {:error, e}

      other ->
        other
    end
  end

  def bi(i) do
    case :erlang.system_info(i) do
      x when is_binary(x) ->
        :io.put_chars(:erlang.binary_to_list(x))

      x when is_list(x) ->
        :io.put_chars(x)

      x ->
        format('~w', [x])
    end
  end

  def m(m) do
    l = m.module_info()
    {:exports, e} = :lists.keyfind(:exports, 1, l)
    time = get_compile_time(l)
    cOpts = get_compile_options(l)
    format('Module: ~w~n', [m])
    print_md5(l)
    format('Compiled: ')
    print_time(time)
    print_object_file(m)
    format('Compiler options:  ~p~n', [cOpts])
    format('Exports: ~n', [])
    print_exports(keysort(1, e))
  end

  defp print_object_file(mod) do
    case :code.is_loaded(mod) do
      {:file, file} ->
        format('Object file: ~ts\n', [file])

      _ ->
        :ignore
    end
  end

  defp print_md5(l) do
    case :lists.keyfind(:md5, 1, l) do
      {:md5, <<mD5::size(128)>>} ->
        :io.format('MD5: ~.16b~n', [mD5])

      _ ->
        :ok
    end
  end

  defp get_compile_time(l) do
    case get_compile_info(l, :time) do
      {:ok, val} ->
        val

      :error ->
        :notime
    end
  end

  defp get_compile_options(l) do
    case get_compile_info(l, :options) do
      {:ok, val} ->
        val

      :error ->
        []
    end
  end

  defp get_compile_info(l, tag) do
    case :lists.keyfind(:compile, 1, l) do
      {:compile, i} ->
        case :lists.keyfind(tag, 1, i) do
          {^tag, val} ->
            {:ok, val}

          false ->
            :error
        end

      false ->
        :error
    end
  end

  defp print_exports(x) when length(x) > 16 do
    split_print_exports(x)
  end

  defp print_exports([]) do
    :ok
  end

  defp print_exports([{f, a} | tail]) do
    format('         ~tw/~w~n', [f, a])
    print_exports(tail)
  end

  defp split_print_exports(l) do
    len = length(l)
    mid = div(len, 2)
    l1 = sublist(l, 1, mid)
    l2 = sublist(l, mid + 1, len - mid + 1)
    split_print_exports(l1, l2)
  end

  defp split_print_exports([], [{f, a} | t]) do
    str = ' '
    format('~-30ts~tw/~w~n', [str, f, a])
    split_print_exports([], t)
  end

  defp split_print_exports([{f1, a1} | t1], [{f2, a2} | t2]) do
    str = flatten(:io_lib.format('~tw/~w', [f1, a1]))
    format('~-30ts~tw/~w~n', [str, f2, a2])
    split_print_exports(t1, t2)
  end

  defp split_print_exports([], []) do
    :ok
  end

  defp print_time({year, month, day, hour, min, _Secs}) do
    format('~s ~w ~w, ', [month(month), day, year])
    format('~.2.0w:~.2.0w~n', [hour, min])
  end

  defp print_time(:notime) do
    format('No compile time info available~n', [])
  end

  defp month(1) do
    'January'
  end

  defp month(2) do
    'February'
  end

  defp month(3) do
    'March'
  end

  defp month(4) do
    'April'
  end

  defp month(5) do
    'May'
  end

  defp month(6) do
    'June'
  end

  defp month(7) do
    'July'
  end

  defp month(8) do
    'August'
  end

  defp month(9) do
    'September'
  end

  defp month(10) do
    'October'
  end

  defp month(11) do
    'November'
  end

  defp month(12) do
    'December'
  end

  def flush() do
    receive do
      x ->
        case :lists.keyfind(:encoding, 1, :io.getopts()) do
          {:encoding, :unicode} ->
            format('Shell got ~tp~n', [x])

          _ ->
            format('Shell got ~p~n', [x])
        end

        flush()
    after
      0 ->
        :ok
    end
  end

  def nregs() do
    foreach(
      fn n ->
        print_node_regs(n)
      end,
      all_regs()
    )
  end

  def regs() do
    print_node_regs({node(), :erlang.registered()})
  end

  defp all_regs() do
    case :erlang.is_alive() do
      true ->
        for n <- [node() | :erlang.nodes()] do
          {n, :rpc.call(n, :erlang, :registered, [])}
        end

      false ->
        [{node(), :erlang.registered()}]
    end
  end

  defp print_node_regs({n, list}) when is_list(list) do
    {pids, ports, _Dead} = pids_and_ports(n, sort(list), [], [], [])
    format('~n** Registered procs on node ~w **~n', [n])
    procformat('Name', 'Pid', 'Initial Call', 'Reds', 'Msgs')

    foreach(
      fn {name, pI, pid} ->
        procline(name, pI, pid)
      end,
      pids
    )

    format('~n** Registered ports on node ~w **~n', [n])
    portformat('Name', 'Id', 'Command')

    foreach(
      fn {name, pI, id} ->
        portline(name, pI, id)
      end,
      ports
    )
  end

  defp pids_and_ports(_, [], pids, ports, dead) do
    {reverse(pids), reverse(ports), reverse(dead)}
  end

  defp pids_and_ports(node, [name | names], pids, ports, dead) do
    case pwhereis(node, name) do
      pid when is_pid(pid) ->
        pids_and_ports(node, names, [{name, pinfo(pid), pid} | pids], ports, dead)

      id when is_port(id) ->
        pids_and_ports(node, names, pids, [{name, portinfo(id), id} | ports], dead)

      :undefined ->
        pids_and_ports(node, names, pids, ports, [name | dead])
    end
  end

  defp pwhereis(node, name) do
    case :erlang.is_alive() do
      true ->
        :rpc.call(node, :erlang, :whereis, [name])

      false ->
        :erlang.whereis(name)
    end
  end

  defp portinfo(id) do
    case :erlang.is_alive() do
      true ->
        [:rpc.call(node(id), :erlang, :port_info, [id, :name])]

      false ->
        [:erlang.port_info(id, :name)]
    end
  end

  defp procline(name, info, pid) do
    call = initial_call(info)
    reds = fetch(:reductions, info)
    lM = fetch(:message_queue_len, info)

    procformat(
      :io_lib.format('~tw', [name]),
      :io_lib.format('~w', [pid]),
      :io_lib.format('~ts', [mfa_string(call)]),
      :erlang.integer_to_list(reds),
      :erlang.integer_to_list(lM)
    )
  end

  defp procformat(name, pid, call, reds, lM) do
    format('~-21ts ~-12s ~-25ts ~12s ~4s~n', [name, pid, call, reds, lM])
  end

  defp portline(name, info, id) do
    cmd = fetch(:name, info)
    portformat(:io_lib.format('~tw', [name]), :erlang.port_to_list(id), cmd)
  end

  defp portformat(name, id, cmd) do
    format('~-21ts ~-15s ~-40ts~n', [name, id, cmd])
  end

  def pwd() do
    case :file.get_cwd() do
      {:ok, str} ->
        :ok = :io.format('~ts\n', [str])

      {:error, _} ->
        :ok = :io.format('Cannot determine current directory\n')
    end
  end

  def cd(dir) do
    _ = :file.set_cwd(dir)
    pwd()
  end

  def ls() do
    ls('.')
  end

  def ls(dir) do
    case :file.list_dir(dir) do
      {:ok, entries} ->
        ls_print(sort(entries))

      {:error, :enotdir} ->
        ls_print([dir])

      {:error, error} ->
        format('~ts\n', [:file.format_error(error)])
    end
  end

  defp ls_print([]) do
    :ok
  end

  defp ls_print(l) do
    width = min([max(lengths(l, [])), 40]) + 5
    ls_print(l, width, 0)
  end

  defp ls_print(x, width, len) when width + len >= 80 do
    :io.nl()
    ls_print(x, width, 0)
  end

  defp ls_print([h | t], width, len) do
    :io.format('~-*ts', [width, h])
    ls_print(t, width, len + width)
  end

  defp ls_print([], _, _) do
    :io.nl()
  end

  defp lengths([h | t], l) do
    lengths(t, [length(h) | l])
  end

  defp lengths([], l) do
    l
  end

  defp w(x) do
    :io_lib.write(x)
  end

  def memory() do
    :erlang.memory()
  end

  def memory(typeSpec) do
    :erlang.memory(typeSpec)
  end

  def uptime() do
    :io.format('~s~n', [uptime(get_uptime())])
  end

  defp uptime({d, {h, m, s}}) do
    :lists.flatten([
      for _ <- [:EFE_DUMMY_GEN], d > 0 do
        :io_lib.format('~p days, ', [d])
      end,
      for _ <- [:EFE_DUMMY_GEN], d + h > 0 do
        :io_lib.format('~p hours, ', [h])
      end,
      for _ <- [:EFE_DUMMY_GEN], d + h + m > 0 do
        :io_lib.format('~p minutes and ', [m])
      end,
      :io_lib.format('~p seconds', [s])
    ])
  end

  defp get_uptime() do
    {upTime, _} = :erlang.statistics(:wall_clock)
    :calendar.seconds_to_daystime(div(upTime, 1000))
  end

  def xm(m) do
    appcall(:tools, :xref, :m, [m])
  end

  def y(file) do
    y(file, [])
  end

  def y(file, opts) do
    appcall(:parsetools, :yecc, :file, [file, opts])
  end

  def appcall(app, m, f, args) do
    try do
      apply(m, f, args)
    catch
      :error, :undef ->
        case __STACKTRACE__ do
          [{^m, ^f, ^args, _} | _] ->
            arity = length(args)
            :io.format('Call to ~w:~w/~w in application ~w failed.\n', [m, f, arity, app])

          stk ->
            :erlang.raise(:error, :undef, stk)
        end
    end
  end
end
