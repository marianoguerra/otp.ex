defmodule :m_escript do
  use Bitwise
  require Record

  Record.defrecord(:r_state, :state,
    file: :undefined,
    module: :undefined,
    forms_or_bin: :undefined,
    source: :undefined,
    n_errors: :undefined,
    mode: :undefined,
    exports_main: :undefined,
    has_records: :undefined
  )

  Record.defrecord(:r_sections, :sections,
    type: :undefined,
    shebang: :undefined,
    comment: :undefined,
    emu_args: :undefined,
    body: :undefined
  )

  Record.defrecord(:r_extract_options, :extract_options, compile_source: :undefined)

  def create(file, options) when is_list(options) do
    try do
      s = prepare(options, r_sections())

      binList =
        for section <- [
              r_sections(s, :shebang),
              r_sections(s, :comment),
              r_sections(s, :emu_args),
              r_sections(s, :body)
            ],
            section !== :undefined do
          section
        end

      case file do
        :binary ->
          {:ok, :erlang.list_to_binary(binList)}

        _ ->
          case :file.write_file(file, binList) do
            :ok ->
              :ok

            {:error, reason} ->
              {:error, {reason, file}}
          end
      end
    catch
      prepareReason ->
        {:error, prepareReason}
    end
  end

  defp prepare([h | t], s) do
    case h do
      {:shebang, :undefined} ->
        prepare(t, s)

      :shebang ->
        prepare(t, r_sections(s, shebang: '#!' ++ '/usr/bin/env escript' ++ '\n'))

      {:shebang, :default} ->
        prepare(t, r_sections(s, shebang: '#!' ++ '/usr/bin/env escript' ++ '\n'))

      {:shebang, shebang} when is_list(shebang) ->
        prepare(t, r_sections(s, shebang: '#!' ++ shebang ++ '\n'))

      {:comment, :undefined} ->
        prepare(t, s)

      :comment ->
        prepare(t, r_sections(s, comment: '%% ' ++ 'This is an -*- erlang -*- file' ++ '\n'))

      {:comment, :default} ->
        prepare(t, r_sections(s, comment: '%% ' ++ 'This is an -*- erlang -*- file' ++ '\n'))

      {:comment, comment} when is_list(comment) ->
        prepare(t, r_sections(s, comment: '%% ' ++ comment ++ '\n'))

      {:emu_args, :undefined} ->
        prepare(t, s)

      {:emu_args, args} when is_list(args) ->
        prepare(t, r_sections(s, emu_args: '%%!' ++ args ++ '\n'))

      {type, file} when is_list(file) ->
        case :file.read_file(file) do
          {:ok, bin} ->
            prepare(t, r_sections(s, type: type, body: bin))

          {:error, reason} ->
            throw({reason, h})
        end

      {type, bin} when is_binary(bin) ->
        prepare(t, r_sections(s, type: type, body: bin))

      {:archive = type, zipFiles, zipOptions}
      when is_list(zipFiles) and is_list(zipOptions) ->
        file = 'dummy.zip'

        case :zip.create(file, zipFiles, zipOptions ++ [:memory]) do
          {:ok, {^file, zipBin}} ->
            prepare(t, r_sections(s, type: type, body: zipBin))

          {:error, reason} ->
            throw({reason, h})
        end

      _ ->
        throw({:badarg, h})
    end
  end

  defp prepare([], r_sections(body: :undefined)) do
    throw(:missing_body)
  end

  defp prepare([], r_sections(type: type) = s)
       when type === :source or
              type === :beam or type === :archive do
    s
  end

  defp prepare([], r_sections(type: type)) do
    throw({:illegal_type, type})
  end

  defp prepare(badOptions, _) do
    throw({:badarg, badOptions})
  end

  def extract(file, options)
      when is_list(file) and
             is_list(options) do
    try do
      eO =
        parse_extract_options(
          options,
          r_extract_options(compile_source: false)
        )

      {headerSz, nextLineNo, fd, sections} =
        parse_header(
          file,
          not r_extract_options(eO, :compile_source)
        )

      type = r_sections(sections, :type)

      case {type, r_extract_options(eO, :compile_source)} do
        {:source, true} ->
          bin = compile_source(type, file, fd, nextLineNo, headerSz)

        {_, _} ->
          :ok = :file.close(fd)

          case :file.read_file(file) do
            {:ok, <<_Header::size(headerSz)-binary, bin::binary>>} ->
              :ok

            {:error, readReason} ->
              bin = :get_rid_of_compiler_warning
              throw(readReason)
          end
      end

      return_sections(sections, bin)
    catch
      reason ->
        {:error, reason}
    end
  end

  defp parse_extract_options([h | t], eO) do
    case h do
      :compile_source ->
        eO2 = r_extract_options(eO, compile_source: true)
        parse_extract_options(t, eO2)

      _ ->
        throw({:badarg, h})
    end
  end

  defp parse_extract_options([], eO) do
    eO
  end

  defp compile_source(type, file, fd, nextLineNo, headerSz) do
    {:text, _Module, forms, _HasRecs, _Mode} =
      do_parse_file(type, file, fd, nextLineNo, headerSz, false)

    :ok = :file.close(fd)

    case :compile.forms(
           forms,
           [:return_errors, :debug_info]
         ) do
      {:ok, _, beamBin} ->
        beamBin

      {:error, errors, warnings} ->
        throw(
          {:compile, [{:errors, format_errors(errors)}, {:warnings, format_errors(warnings)}]}
        )
    end
  end

  defp format_errors(compileErrors) do
    for {file, fileErrors} <- compileErrors,
        {lineNo, mod, error} <- fileErrors do
      :lists.flatten([file, ':', :erlang.integer_to_list(lineNo), ': ', mod.format_error(error)])
    end
  end

  defp return_sections(s, bin) do
    {:ok,
     [
       normalize_section(:shebang, r_sections(s, :shebang)),
       normalize_section(:comment, r_sections(s, :comment)),
       normalize_section(:emu_args, r_sections(s, :emu_args)),
       normalize_section(r_sections(s, :type), bin)
     ]}
  end

  defp normalize_section(name, :undefined) do
    {name, :undefined}
  end

  defp normalize_section(:shebang, '#!' ++ chars) do
    chopped = :string.trim(chars, :trailing, '$\n')
    stripped = :string.trim(chopped, :both)

    cond do
      stripped === '/usr/bin/env escript' ->
        {:shebang, :default}

      true ->
        {:shebang, stripped}
    end
  end

  defp normalize_section(:comment, chars) do
    chopped = :string.trim(chars, :trailing, '$\n')

    stripped =
      :string.trim(
        :string.trim(chopped, :leading, '$%'),
        :both
      )

    cond do
      stripped === 'This is an -*- erlang -*- file' ->
        {:comment, :default}

      true ->
        {:comment, stripped}
    end
  end

  defp normalize_section(:emu_args, '%%!' ++ chars) do
    chopped = :string.trim(chars, :trailing, '$\n')
    stripped = :string.trim(chopped, :both)
    {:emu_args, stripped}
  end

  defp normalize_section(name, chars) do
    {name, chars}
  end

  def script_name() do
    [scriptName | _] = :init.get_plain_arguments()
    scriptName
  end

  def start() do
    start([])
  end

  def start(escriptOptions) do
    try do
      :erlang.process_flag(:trap_exit, false)

      case :init.get_plain_arguments() do
        [file | args] ->
          parse_and_run(file, args, escriptOptions)

        [] ->
          :io.format(:standard_error, 'escript: Missing filename\n', [])
          my_halt(127)
      end
    catch
      str ->
        put_chars(:io_lib.format('escript: ~ts\n', [str]))
        my_halt(127)

      _, reason ->
        put_chars(:io_lib.format('escript: Internal error: ~tp\n', [reason]))
        put_chars(:io_lib.format('~tp\n', [__STACKTRACE__]))
        my_halt(127)
    end
  end

  defp parse_and_run(file, args, options) do
    checkOnly = :lists.member('s', options)
    {source, module, formsOrBin, hasRecs, mode} = parse_file(file, checkOnly)

    mode2 =
      case :lists.member('d', options) do
        true ->
          :debug

        false ->
          case :lists.member('c', options) do
            true ->
              :compile

            false ->
              case :lists.member('i', options) do
                true ->
                  :interpret

                false ->
                  case :lists.member('n', options) do
                    true ->
                      :native

                    false ->
                      mode
                  end
              end
          end
      end

    cond do
      is_list(formsOrBin) ->
        case mode2 do
          :interpret ->
            interpret(formsOrBin, hasRecs, file, args)

          :compile ->
            case :compile.forms(formsOrBin, [:report]) do
              {:ok, ^module, beamBin} ->
                {:module, ^module} = :code.load_binary(module, file, beamBin)
                run(module, args)

              _Other ->
                fatal('There were compilation errors.')
            end

          :native ->
            case :compile.forms(formsOrBin, [:report, :native]) do
              {:ok, ^module, beamBin} ->
                {:module, ^module} = :code.load_binary(module, file, beamBin)
                run(module, args)

              _Other ->
                fatal('There were compilation errors.')
            end

          :debug ->
            case :compile.forms(
                   formsOrBin,
                   [:report, :debug_info]
                 ) do
              {:ok, ^module, beamBin} ->
                {:module, ^module} = :code.load_binary(module, file, beamBin)
                debug(module, {module, file, file, beamBin}, args)

              _Other ->
                fatal('There were compilation errors.')
            end
        end

      is_binary(formsOrBin) ->
        case source do
          :archive ->
            {:ok, fileInfo} = :file.read_file_info(file)

            case :code.set_primary_archive(file, formsOrBin, fileInfo, &:escript.parse_file/1) do
              :ok when checkOnly ->
                case :code.load_file(module) do
                  {:module, _} ->
                    case :erlang.function_exported(module, :main, 1) do
                      true ->
                        my_halt(0)

                      false ->
                        text = :lists.concat(['Function ', module, ':main/1 is not exported'])
                        fatal(text)
                    end

                  _ ->
                    text = :lists.concat(['Cannot load module ', module, ' from archive'])
                    fatal(text)
                end

              :ok ->
                case mode2 do
                  :run ->
                    run(module, args)

                  :debug ->
                    debug(module, module, args)
                end

              {:error, :bad_eocd} ->
                fatal('Not an archive file')

              {:error, reason} ->
                fatal(reason)
            end

          :beam ->
            case mode2 do
              :run ->
                {:module, ^module} = :code.load_binary(module, file, formsOrBin)
                run(module, args)

              :debug ->
                [base | rest] = :lists.reverse(:filename.split(file))

                base2 =
                  :filename.basename(
                    base,
                    :code.objfile_extension()
                  )

                rest2 =
                  case rest do
                    ['ebin' | top] ->
                      ['src' | top]

                    _ ->
                      rest
                  end

                srcFile =
                  :filename.join(
                    :lists.reverse([
                      base2 ++ '.erl'
                      | rest2
                    ])
                  )

                debug(module, {module, srcFile, file, formsOrBin}, args)
            end
        end
    end
  end

  def parse_file(file) do
    try do
      parse_file(file, false)
    catch
      reason ->
        {:error, reason}
    else
      {_Source, _Module, formsOrBin, _HasRecs, _Mode}
      when is_binary(formsOrBin) ->
        {:ok, formsOrBin}

      _ ->
        {:error, :no_archive_bin}
    end
  end

  defp parse_file(file, checkOnly) do
    {headerSz, nextLineNo, fd, sections} = parse_header(file, false)
    do_parse_file(r_sections(sections, :type), file, fd, nextLineNo, headerSz, checkOnly)
  end

  defp do_parse_file(type, file, fd, nextLineNo, headerSz, checkOnly) do
    s = initial_state(file)

    r_state(
      mode: mode,
      source: source,
      module: module,
      forms_or_bin: formsOrBin,
      has_records: hasRecs
    ) =
      case type do
        :archive ->
          :ok = :file.close(fd)
          parse_archive(s, file, headerSz)

        :beam ->
          :ok = :file.close(fd)
          parse_beam(s, file, headerSz, checkOnly)

        :source ->
          parse_source(s, file, fd, nextLineNo, headerSz, checkOnly)
      end

    {source, module, formsOrBin, hasRecs, mode}
  end

  defp initial_state(file) do
    r_state(file: file, n_errors: 0, mode: :interpret, exports_main: false, has_records: false)
  end

  defp parse_header(file, keepFirst) do
    lineNo = 1

    {:ok, fd} =
      case :file.open(file, [:read]) do
        {:ok, fd0} ->
          {:ok, fd0}

        {:error, r} ->
          fatal(:lists.concat([:file.format_error(r), ': \'', file, '\'']))
      end

    {:ok, headerSz0} = :file.position(fd, :cur)
    line1 = get_line(fd)

    case classify_line(line1) do
      :shebang ->
        find_first_body_line(fd, headerSz0, lineNo, keepFirst, r_sections(shebang: line1))

      :archive ->
        {headerSz0, lineNo, fd, r_sections(type: :archive)}

      :beam ->
        {headerSz0, lineNo, fd, r_sections(type: :beam)}

      _ ->
        find_first_body_line(fd, headerSz0, lineNo, keepFirst, r_sections())
    end
  end

  defp find_first_body_line(fd, headerSz0, lineNo, keepFirst, sections) do
    {:ok, headerSz1} = :file.position(fd, :cur)
    line2 = get_line(fd)
    {:ok, headerSz2} = :file.position(fd, :cur)

    cond do
      r_sections(sections, :shebang) === :undefined and
          keepFirst === true ->
        {headerSz0, lineNo, fd, r_sections(sections, type: guess_type(line2))}

      r_sections(sections, :shebang) === :undefined ->
        {headerSz1, lineNo, fd, r_sections(sections, type: guess_type(line2))}

      true ->
        case classify_line(line2) do
          :emu_args ->
            line3 = get_line(fd)

            {headerSz2, lineNo + 2, fd,
             r_sections(sections, type: guess_type(line3), comment: :undefined, emu_args: line2)}

          :comment ->
            line3 = get_line(fd)
            {:ok, headerSz3} = :file.position(fd, :cur)
            line3Type = classify_line(line3)

            cond do
              line3Type === :emu_args ->
                line4 = get_line(fd)

                {headerSz3, lineNo + 3, fd,
                 r_sections(sections, type: guess_type(line4), comment: line2, emu_args: line3)}

              true ->
                {headerSz2, lineNo + 2, fd,
                 r_sections(sections, type: guess_type(line3), comment: line2)}
            end

          _ ->
            {headerSz1, lineNo + 1, fd, r_sections(sections, type: guess_type(line2))}
        end
    end
  end

  defp classify_line(line) do
    case line do
      '#!' ++ _ ->
        :shebang

      'PK' ++ _ ->
        :archive

      'FOR1' ++ _ ->
        :beam

      '%%!' ++ _ ->
        :emu_args

      '%' ++ _ ->
        :comment

      _ ->
        :undefined
    end
  end

  defp guess_type(line) do
    case classify_line(line) do
      :archive ->
        :archive

      :beam ->
        :beam

      _ ->
        :source
    end
  end

  defp get_line(p) do
    case :io.get_line(p, :"") do
      :eof ->
        fatal('Premature end of file reached')

      line ->
        line
    end
  end

  defp parse_archive(s, file, headerSz) do
    case :file.read_file(file) do
      {:ok, <<_Header::size(headerSz)-binary, bin::binary>>} ->
        mod =
          case :init.get_argument(:escript) do
            {:ok, [['main', m]]} ->
              :erlang.list_to_atom(m)

            _ ->
              revBase = :lists.reverse(:filename.basename(file))

              revBase2 =
                case :lists.dropwhile(
                       fn x ->
                         x !== ?.
                       end,
                       revBase
                     ) do
                  [?. | rest] ->
                    rest

                  [] ->
                    revBase
                end

              :erlang.list_to_atom(:lists.reverse(revBase2))
          end

        r_state(s, source: :archive, mode: :run, module: mod, forms_or_bin: bin)

      {:ok, _} ->
        fatal('Illegal archive format')

      {:error, reason} ->
        fatal(:file.format_error(reason))
    end
  end

  defp parse_beam(s, file, headerSz, checkOnly) do
    {:ok, <<_Header::size(headerSz)-binary, bin::binary>>} = :file.read_file(file)

    case :beam_lib.chunks(bin, [:exports]) do
      {:ok, {module, [{:exports, exports}]}} ->
        case checkOnly do
          true ->
            case :lists.member({:main, 1}, exports) do
              true ->
                my_halt(0)

              false ->
                text = :lists.concat(['Function ', module, ':main/1 is not exported'])
                fatal(text)
            end

          false ->
            r_state(s, source: :beam, mode: :run, module: module, forms_or_bin: bin)
        end

      {:error, :beam_lib, reason} when is_tuple(reason) ->
        fatal(:erlang.element(1, reason))
    end
  end

  defp parse_source(s, file, fd, startLine, headerSz, checkOnly) do
    {preDefMacros, module} = pre_def_macros(file)
    includePath = []
    {:ok, _} = :file.position(fd, 0)
    _ = :io.get_line(fd, :"")
    encoding = :epp.set_encoding(fd)
    {:ok, _} = :file.position(fd, headerSz)

    case :epp.open(file, fd, startLine, includePath, preDefMacros) do
      {:ok, epp} ->
        _ =
          for _ <- [:EFE_DUMMY_GEN], encoding !== :none do
            :io.setopts(fd, [{:encoding, encoding}])
          end

        {:ok, fileForm} = :epp.parse_erl_form(epp)
        optModRes = :epp.parse_erl_form(epp)
        s2 = r_state(s, source: :text, module: module)

        s3 =
          case optModRes do
            {:ok, {:attribute, _, :module, m} = form} ->
              epp_parse_file(epp, r_state(s2, module: m), [form, fileForm])

            {:ok, _} ->
              modForm = {:attribute, a1(), :module, module}
              epp_parse_file2(epp, s2, [modForm, fileForm], optModRes)

            {:error, _} ->
              epp_parse_file2(epp, s2, [fileForm], optModRes)

            {:eof, lastLine} ->
              r_state(s, forms_or_bin: [fileForm, {:eof, lastLine}])
          end

        :ok = :epp.close(epp)
        :ok = :file.close(fd)
        check_source(s3, checkOnly)

      {:error, reason} ->
        :io.format(:standard_error, 'escript: ~tp\n', [reason])
        fatal('Preprocessor error')
    end
  end

  defp check_source(s, checkOnly) do
    case s do
      r_state(n_errors: nerrs) when nerrs !== 0 ->
        fatal('There were compilation errors.')

      r_state(
        exports_main: expMain,
        forms_or_bin: [[fileForm2, modForm2] | forms]
      ) ->
        forms2 =
          case expMain do
            false ->
              [{:attribute, a0(), :export, [{:main, 1}]} | forms]

            true ->
              forms
          end

        forms3 = [[fileForm2, modForm2] | forms2]

        case checkOnly do
          true ->
            case :compile.forms(
                   forms3,
                   [:report, :strong_validation]
                 ) do
              {:ok, _} ->
                my_halt(0)

              _Other ->
                fatal('There were compilation errors.')
            end

          false ->
            r_state(s, forms_or_bin: forms3)
        end
    end
  end

  defp pre_def_macros(file) do
    {megaSecs, secs, microSecs} = :erlang.timestamp()
    unique = :erlang.unique_integer([:positive])

    replace = fn char ->
      case char do
        ?. ->
          ?_

        _ ->
          char
      end
    end

    cleanBase =
      :lists.map(
        replace,
        :filename.basename(file)
      )

    moduleStr =
      cleanBase ++
        '__' ++
        'escript__' ++
        :erlang.integer_to_list(megaSecs) ++
        '__' ++
        :erlang.integer_to_list(secs) ++
        '__' ++ :erlang.integer_to_list(microSecs) ++ '__' ++ :erlang.integer_to_list(unique)

    module = :erlang.list_to_atom(moduleStr)
    preDefMacros = [{:MODULE, module, :redefine}, {:MODULE_STRING, moduleStr, :redefine}]
    {preDefMacros, module}
  end

  defp epp_parse_file(epp, s, forms) do
    parsed = :epp.parse_erl_form(epp)
    epp_parse_file2(epp, s, forms, parsed)
  end

  defp epp_parse_file2(epp, s, forms, parsed) do
    case parsed do
      {:ok, form} ->
        case form do
          {:attribute, _, :record, _} ->
            s2 = r_state(s, has_records: true)
            epp_parse_file(epp, s2, [form | forms])

          {:attribute, ln, :mode, newMode} ->
            s2 = r_state(s, mode: newMode)

            cond do
              newMode === :compile or newMode === :interpret or
                newMode === :debug or newMode === :native ->
                epp_parse_file(epp, s2, [form | forms])

              true ->
                args = :lists.flatten(:io_lib.format('illegal mode attribute: ~p', [newMode]))
                :io.format(:standard_error, '~ts:~w ~s\n', [r_state(s, :file), ln, args])
                error = {:error, {ln, :erl_parse, args}}
                nerrs = r_state(s, :n_errors) + 1
                epp_parse_file(epp, r_state(s2, n_errors: nerrs), [error | forms])
            end

          {:attribute, _, :export, fs} ->
            case :lists.member({:main, 1}, fs) do
              false ->
                epp_parse_file(epp, s, [form | forms])

              true ->
                epp_parse_file(epp, r_state(s, exports_main: true), [form | forms])
            end

          _ ->
            epp_parse_file(epp, s, [form | forms])
        end

      {:error, {ln, mod, args}} = form ->
        :io.format(:standard_error, '~ts:~w: ~ts\n', [
          r_state(s, :file),
          ln,
          mod.format_error(args)
        ])

        epp_parse_file(epp, r_state(s, n_errors: r_state(s, :n_errors) + 1), [form | forms])

      {:eof, lastLine} ->
        r_state(s,
          forms_or_bin:
            :lists.reverse([
              {:eof, lastLine}
              | forms
            ])
        )
    end
  end

  defp debug(module, absMod, args) do
    case hidden_apply(:debugger, :debugger, :start, []) do
      {:ok, _} ->
        case hidden_apply(:debugger, :int, :i, [absMod]) do
          {:module, _} ->
            hidden_apply(:debugger, :debugger, :auto_attach, [[:init]])
            run(module, args)

          :error ->
            text = :lists.concat(['Cannot load the code for ', module, ' into the debugger'])
            fatal(text)
        end

      _ ->
        fatal('Cannot start the debugger')
    end
  end

  defp run(module, args) do
    try do
      module.main(args)
      my_halt(0)
    catch
      class, reason ->
        fatal(format_exception(class, reason, __STACKTRACE__))
    end
  end

  defp interpret(forms, hasRecs, file, args) do
    case :erl_lint.module(forms) do
      {:ok, ws} ->
        report_warnings(ws)

      {:error, es, ws} ->
        report_errors(es)
        report_warnings(ws)
        fatal('There were compilation errors.')
    end

    forms2 =
      case hasRecs do
        false ->
          forms

        true ->
          :erl_expand_records.module(forms, [])
      end

    dict = parse_to_map(forms2)
    argsA = :erl_parse.abstract(args, 0)
    anno = a0()
    call = {:call, anno, {:atom, anno, :main}, [argsA]}

    try do
      _ =
        :erl_eval.expr(
          call,
          :erl_eval.new_bindings(),
          {:value,
           fn i, j ->
             code_handler(i, j, dict, file)
           end}
        )

      my_halt(0)
    catch
      class, reason ->
        fatal(format_exception(class, reason, __STACKTRACE__))
    end
  end

  defp report_errors(errors) do
    :lists.foreach(
      fn
        {{f, _L}, eds} ->
          list_errors(f, eds)

        {f, eds} ->
          list_errors(f, eds)
      end,
      errors
    )
  end

  defp list_errors(f, [{line, mod, e} | es]) do
    :io.format(:standard_error, '~ts:~w: ~ts\n', [f, line, mod.format_error(e)])
    list_errors(f, es)
  end

  defp list_errors(f, [{mod, e} | es]) do
    :io.format(:standard_error, '~ts: ~ts\n', [f, mod.format_error(e)])
    list_errors(f, es)
  end

  defp list_errors(_F, []) do
    :ok
  end

  defp report_warnings(ws0) do
    ws1 =
      :lists.flatmap(
        fn
          {{f, _L}, eds} ->
            format_message(f, eds)

          {f, eds} ->
            format_message(f, eds)
        end,
        ws0
      )

    ws = :ordsets.from_list(ws1)

    :lists.foreach(
      fn {_, str} ->
        :io.put_chars(:standard_error, str)
      end,
      ws
    )
  end

  defp format_message(f, [{line, mod, e} | es]) do
    m = {{f, line}, :io_lib.format('~ts:~w: Warning: ~ts\n', [f, line, mod.format_error(e)])}
    [m | format_message(f, es)]
  end

  defp format_message(f, [{mod, e} | es]) do
    m = {:none, :io_lib.format('~ts: Warning: ~ts\n', [f, mod.format_error(e)])}
    [m | format_message(f, es)]
  end

  defp format_message(_, []) do
    []
  end

  defp parse_to_map(l) do
    parse_to_map(l, :maps.new())
  end

  defp parse_to_map(
         [{:function, _, name, arity, clauses} | t],
         map0
       ) do
    map = :maps.put({:local, name, arity}, clauses, map0)
    parse_to_map(t, map)
  end

  defp parse_to_map(
         [{:attribute, _, :import, {mod, funcs}} | t],
         map0
       ) do
    map =
      :lists.foldl(
        fn i, d ->
          :maps.put({:remote, i}, mod, d)
        end,
        map0,
        funcs
      )

    parse_to_map(t, map)
  end

  defp parse_to_map([_ | t], map) do
    parse_to_map(t, map)
  end

  defp parse_to_map([], map) do
    map
  end

  defp code_handler(:local, [:file], _, file) do
    file
  end

  defp code_handler(name, args, map, file) do
    arity = length(args)

    case :maps.find({:local, name, arity}, map) do
      {:ok, cs} ->
        lF =
          {:value,
           fn i, j ->
             code_handler(i, j, map, file)
           end}

        case :erl_eval.match_clause(cs, args, :erl_eval.new_bindings(), lF) do
          {body, bs} ->
            eval_exprs(body, bs, lF, :none, :none)

          :nomatch ->
            :erlang.error({:function_clause, [{:local, name, args}]})
        end

      :error ->
        case :maps.find({:remote, {name, arity}}, map) do
          {:ok, mod} ->
            apply(mod, name, args)

          :error ->
            :io.format(:standard_error, 'Script does not export ~tw/~w\n', [name, arity])
            my_halt(127)
        end
    end
  end

  defp eval_exprs([e], bs0, lf, ef, _RBs) do
    rBs1 = :value
    :erl_eval.expr(e, bs0, lf, ef, rBs1)
  end

  defp eval_exprs([e | es], bs0, lf, ef, rBs) do
    rBs1 = :none
    {:value, _V, bs} = :erl_eval.expr(e, bs0, lf, ef, rBs1)
    eval_exprs(es, bs, lf, ef, rBs)
  end

  defp format_exception(class, reason, stackTrace) do
    enc = encoding()

    p =
      case enc do
        :latin1 ->
          'P'

        _ ->
          'tP'
      end

    pF = fn term, i ->
      :io_lib.format(
        '~.' ++ :erlang.integer_to_list(i) ++ p,
        [term, 50]
      )
    end

    stackFun = fn m, _F, _A ->
      :erlang.or(m === :erl_eval, m === :escript)
    end

    :erl_error.format_exception(1, class, reason, stackTrace, stackFun, pF, enc)
  end

  defp encoding() do
    case :io.getopts() do
      {:error, _} = _Err ->
        :latin1

      opts ->
        case :lists.keyfind(:encoding, 1, opts) do
          false ->
            :latin1

          {:encoding, encoding} ->
            encoding
        end
    end
  end

  defp put_chars(string) do
    try do
      :io.put_chars(:standard_error, string)
    catch
      _, _ ->
        display_err(:lists.flatten(string))
    end
  end

  defp display_err(string) do
    port = :erlang.open_port({:fd, 2, 2}, [:out, :binary])
    send(port, {self(), {:command, :erlang.list_to_binary(string)}})
    :erlang.port_close(port)
  end

  defp a0() do
    anno(0)
  end

  defp a1() do
    anno(1)
  end

  defp anno(l) do
    :erl_anno.new(l)
  end

  defp fatal(str) do
    throw(str)
  end

  defp my_halt(reason) do
    :erlang.halt(reason)
  end

  defp hidden_apply(app, m, f, args) do
    try do
      apply(
        (fn ->
           m
         end).(),
        f,
        args
      )
    catch
      :error, :undef ->
        case __STACKTRACE__ do
          [{^m, ^f, ^args, _} | _] ->
            arity = length(args)

            text =
              :io_lib.format('Call to ~w:~w/~w in application ~w failed.\n', [m, f, arity, app])

            fatal(text)

          stk ->
            :erlang.raise(:error, :undef, stk)
        end
    end
  end
end
