defmodule :m_ct_make do
  use Bitwise
  require Record

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

  def all() do
    all([])
  end

  def all(options) do
    {makeOpts, compileOpts} = sort_options(options, [], [])

    case read_emakefile(:Emakefile, compileOpts) do
      files when is_list(files) ->
        do_make_files(files, makeOpts)

      :error ->
        {:error, []}
    end
  end

  def files(fs) do
    files(fs, [])
  end

  def files(fs0, options) do
    fs =
      for f <- fs0 do
        :filename.rootname(f, '.erl')
      end

    {makeOpts, compileOpts} = sort_options(options, [], [])

    case get_opts_from_emakefile(fs, :Emakefile, compileOpts) do
      files when is_list(files) ->
        do_make_files(files, makeOpts)

      :error ->
        {:error, []}
    end
  end

  defp do_make_files(fs, opts) do
    process(fs, :lists.member(:noexec, opts), load_opt(opts), [])
  end

  defp sort_options([h | t], make, comp) do
    case :lists.member(
           h,
           [:noexec, :load, :netload, :noload]
         ) do
      true ->
        sort_options(t, [h | make], comp)

      false ->
        sort_options(t, make, [h | comp])
    end
  end

  defp sort_options([], make, comp) do
    {make, :lists.reverse(comp)}
  end

  defp read_emakefile(emakefile, opts) do
    case :file.consult(emakefile) do
      {:ok, emake} ->
        transform(emake, opts, [], [])

      {:error, :enoent} ->
        mods =
          for f <- :filelib.wildcard('*.erl') do
            :filename.rootname(f)
          end

        [{mods, opts}]

      {:error, other} ->
        :io.format('make: Trouble reading \'Emakefile\':~n~tp~n', [other])
        :error
    end
  end

  defp transform([{mod, modOpts} | emake], opts, files, already) do
    case expand(mod, already) do
      [] ->
        transform(emake, opts, files, already)

      mods ->
        transform(emake, opts, [{mods, modOpts ++ opts} | files], mods ++ already)
    end
  end

  defp transform([mod | emake], opts, files, already) do
    case expand(mod, already) do
      [] ->
        transform(emake, opts, files, already)

      mods ->
        transform(emake, opts, [{mods, opts} | files], mods ++ already)
    end
  end

  defp transform([], _Opts, files, _Already) do
    :lists.reverse(files)
  end

  defp expand(mod, already) when is_atom(mod) do
    expand(:erlang.atom_to_list(mod), already)
  end

  defp expand(mods, already)
       when is_list(mods) and
              not is_integer(hd(mods)) do
    :lists.concat(
      for mod <- mods do
        expand(mod, already)
      end
    )
  end

  defp expand(mod, already) do
    case :lists.member(?*, mod) do
      true ->
        fun = fn f, acc ->
          m = :filename.rootname(f)

          case :lists.member(m, already) do
            true ->
              acc

            false ->
              [m | acc]
          end
        end

        :lists.foldl(fun, [], :filelib.wildcard(mod ++ '.erl'))

      false ->
        mod2 = :filename.rootname(mod, '.erl')

        case :lists.member(mod2, already) do
          true ->
            []

          false ->
            [mod2]
        end
    end
  end

  defp get_opts_from_emakefile(mods, emakefile, opts) do
    case :file.consult(emakefile) do
      {:ok, emake} ->
        modsandopts = transform(emake, opts, [], [])

        modStrings =
          for m <- mods do
            coerce_2_list(m)
          end

        get_opts_from_emakefile2(modsandopts, modStrings, opts, [])

      {:error, :enoent} ->
        [{mods, opts}]

      {:error, other} ->
        :io.format('make: Trouble reading \'Emakefile\':~n~tp~n', [other])
        :error
    end
  end

  defp get_opts_from_emakefile2([{makefileMods, o} | rest], mods, opts, result) do
    case members(mods, makefileMods, [], mods) do
      {[], _} ->
        get_opts_from_emakefile2(rest, mods, opts, result)

      {i, restOfMods} ->
        get_opts_from_emakefile2(rest, restOfMods, opts, [{i, o} | result])
    end
  end

  defp get_opts_from_emakefile2([], [], _Opts, result) do
    result
  end

  defp get_opts_from_emakefile2([], restOfMods, opts, result) do
    [{restOfMods, opts} | result]
  end

  defp members([h | t], makefileMods, i, rest) do
    case :lists.member(h, makefileMods) do
      true ->
        members(t, makefileMods, [h | i], :lists.delete(h, rest))

      false ->
        members(t, makefileMods, i, rest)
    end
  end

  defp members([], _MakefileMods, i, rest) do
    {i, rest}
  end

  defp load_opt(opts) do
    case :lists.member(:netload, opts) do
      true ->
        :netload

      false ->
        case :lists.member(:load, opts) do
          true ->
            :load

          _ ->
            :noload
        end
    end
  end

  defp process([{[], _Opts} | rest], noExec, load, result) do
    process(rest, noExec, load, result)
  end

  defp process([{[h | t], opts} | rest], noExec, load, result) do
    case recompilep(coerce_2_list(h), noExec, load, opts) do
      :error ->
        process([{t, opts} | rest], noExec, load, [{h, :error} | result])

      info ->
        process([{t, opts} | rest], noExec, load, [{h, info} | result])
    end
  end

  defp process([], noExec, _Load, result) do
    cond do
      not noExec ->
        case :lists.keysearch(:error, 2, result) do
          {:value, _} ->
            {:error, result}

          false ->
            {:up_to_date, result}
        end

      true ->
        result
    end
  end

  defp recompilep(file, noExec, load, opts) do
    objName =
      :lists.append(
        :filename.basename(file),
        :code.objfile_extension()
      )

    objFile =
      case :lists.keysearch(:outdir, 1, opts) do
        {:value, {:outdir, outDir}} ->
          :filename.join(coerce_2_list(outDir), objName)

        false ->
          objName
      end

    case exists(objFile) do
      true ->
        recompilep1(file, noExec, load, opts, objFile)

      false ->
        recompile(file, noExec, load, opts)
    end
  end

  defp recompilep1(file, noExec, load, opts, objFile) do
    {:ok, erl} =
      :file.read_file_info(
        :lists.append(
          file,
          '.erl'
        )
      )

    {:ok, obj} = :file.read_file_info(objFile)

    case {readable(erl), writable(obj)} do
      {true, true} ->
        recompilep1(erl, obj, file, noExec, load, opts)

      _ ->
        :error
    end
  end

  defp recompilep1(r_file_info(mtime: te), r_file_info(mtime: to), file, noExec, load, opts)
       when te > to do
    recompile(file, noExec, load, opts)
  end

  defp recompilep1(_Erl, r_file_info(mtime: to), file, noExec, load, opts) do
    recompile2(to, file, noExec, load, opts)
  end

  defp recompile2(objMTime, file, noExec, load, opts) do
    includePath = include_opt(opts)

    case check_includes(:lists.append(file, '.erl'), includePath, objMTime) do
      true ->
        recompile(file, noExec, load, opts)

      false ->
        :up_to_date
    end
  end

  defp include_opt([{:i, path} | rest]) do
    [path | include_opt(rest)]
  end

  defp include_opt([_First | rest]) do
    include_opt(rest)
  end

  defp include_opt([]) do
    []
  end

  defp recompile(file, noExec, load, opts) do
    case do_recompile(file, noExec, load, opts) do
      {:ok, _} ->
        :ok

      other ->
        other
    end
  end

  defp do_recompile(_File, true, _Load, _Opts) do
    :out_of_date
  end

  defp do_recompile(file, false, load, opts) do
    :io.format('Recompile: ~ts\n', [file])

    case :compile.file(
           file,
           [[:report_errors, :report_warnings] | opts]
         ) do
      ok when is_tuple(ok) and :erlang.element(1, ok) == :ok ->
        maybe_load(:erlang.element(2, ok), load, opts)

      _Error ->
        :error
    end
  end

  defp maybe_load(_Mod, :noload, _Opts) do
    :ok
  end

  defp maybe_load(mod, load, opts) do
    case :compile.output_generated(opts) do
      true ->
        dir = :proplists.get_value(:outdir, opts, '.')
        do_load(dir, mod, load)

      false ->
        :io.format('** Warning: No object file created - nothing loaded **~n')
        :ok
    end
  end

  defp do_load(dir, mod, :load) do
    :code.purge(mod)

    case :code.load_abs(:filename.join(dir, mod), mod) do
      {:module, ^mod} ->
        {:ok, mod}

      other ->
        other
    end
  end

  defp do_load(dir, mod, :netload) do
    obj = :erlang.atom_to_list(mod) ++ :code.objfile_extension()
    fname = :filename.join(dir, obj)

    case :file.read_file(fname) do
      {:ok, bin} ->
        :rpc.eval_everywhere(:code, :load_binary, [mod, fname, bin])
        {:ok, mod}

      other ->
        other
    end
  end

  defp exists(file) do
    case :file.read_file_info(file) do
      {:ok, _} ->
        true

      _ ->
        false
    end
  end

  defp readable(r_file_info(access: :read_write)) do
    true
  end

  defp readable(r_file_info(access: :read)) do
    true
  end

  defp readable(_) do
    false
  end

  defp writable(r_file_info(access: :read_write)) do
    true
  end

  defp writable(r_file_info(access: :write)) do
    true
  end

  defp writable(_) do
    false
  end

  defp coerce_2_list(x) when is_atom(x) do
    :erlang.atom_to_list(x)
  end

  defp coerce_2_list(x) do
    x
  end

  defp check_includes(file, includePath, objMTime) do
    path = [:filename.dirname(file) | includePath]

    case :epp.open(file, path, []) do
      {:ok, epp} ->
        check_includes2(epp, file, objMTime)

      _Error ->
        false
    end
  end

  defp check_includes2(epp, file, objMTime) do
    a1 = :erl_anno.new(1)

    case :epp.parse_erl_form(epp) do
      {:ok, {:attribute, ^a1, :file, {^file, ^a1}}} ->
        check_includes2(epp, file, objMTime)

      {:ok, {:attribute, ^a1, :file, {incFile, ^a1}}} ->
        case :file.read_file_info(incFile) do
          {:ok, r_file_info(mtime: mTime)} when mTime > objMTime ->
            :epp.close(epp)
            true

          _ ->
            check_includes2(epp, file, objMTime)
        end

      {:ok, _} ->
        check_includes2(epp, file, objMTime)

      {:eof, _} ->
        :epp.close(epp)
        false

      {:error, _Error} ->
        check_includes2(epp, file, objMTime)

      {:warning, _Warning} ->
        check_includes2(epp, file, objMTime)
    end
  end
end
