defmodule :m_file_sorter do
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

  Record.defrecord(:r_w, :w,
    keypos: :undefined,
    runs: [[]],
    seq: 1,
    in: :undefined,
    out: :undefined,
    fun_out: :undefined,
    prefix: :undefined,
    temp: [],
    format: :undefined,
    runsize: :undefined,
    no_files: :undefined,
    order: :undefined,
    chunksize: :undefined,
    wfd: :undefined,
    ref: :undefined,
    z: :undefined,
    unique: :undefined,
    hdlen: :undefined,
    inout_value: :undefined
  )

  Record.defrecord(:r_opts, :opts,
    format: binary_term_fun(),
    size: 524_288,
    no_files: 16,
    tmpdir: :default,
    order: :ascending,
    compressed: false,
    unique: false,
    header: 4
  )

  def sort(fileName) do
    sort([fileName], fileName)
  end

  def sort(input, output) do
    sort(input, output, [])
  end

  def sort(input0, output0, options) do
    case {is_input(input0), maybe_output(output0), options(options)} do
      {{true, input}, {true, output}, r_opts() = opts} ->
        do_sort(0, input, output, opts, :sort)

      t ->
        badarg(
          culprit(:erlang.tuple_to_list(t)),
          [input0, output0, options]
        )
    end
  end

  def keysort(keyPos, fileName) do
    keysort(keyPos, [fileName], fileName)
  end

  def keysort(keyPos, input, output) do
    keysort(keyPos, input, output, [])
  end

  def keysort(keyPos, input0, output0, options) do
    r =
      case {is_keypos(keyPos), is_input(input0), maybe_output(output0), options(options)} do
        {_, _, _, r_opts(format: :binary)} ->
          {input0, output0, [{:badarg, :format}]}

        {_, _, _, r_opts(order: order)} when is_function(order) ->
          {input0, output0, [{:badarg, :order}]}

        {true, {true, in__}, {true, out}, r_opts() = opts} ->
          {in__, out, opts}

        t ->
          {input0, output0, :erlang.tuple_to_list(t)}
      end

    case r do
      {input, output, r_opts() = o} ->
        do_sort(keyPos, input, output, o, :sort)

      {_, _, o} ->
        badarg(culprit(o), [keyPos, input0, output0, options])
    end
  end

  def merge(files, output) do
    merge(files, output, [])
  end

  def merge(files0, output0, options) do
    case {is_files(files0), maybe_output(output0), options(options)} do
      {{true, files}, {true, output}, r_opts() = opts} ->
        do_sort(0, files, output, opts, :merge)

      t ->
        badarg(
          culprit(:erlang.tuple_to_list(t)),
          [files0, output0, options]
        )
    end
  end

  def keymerge(keyPos, files, output) do
    keymerge(keyPos, files, output, [])
  end

  def keymerge(keyPos, files0, output0, options) do
    r =
      case {is_keypos(keyPos), is_files(files0), maybe_output(output0), options(options)} do
        {_, _, _, r_opts(format: :binary)} ->
          {files0, output0, [{:badarg, :format}]}

        {_, _, _, r_opts(order: order)} when is_function(order) ->
          {files0, output0, [{:badarg, :order}]}

        {true, {true, fs}, {true, out}, r_opts() = opts} ->
          {fs, out, opts}

        t ->
          {files0, output0, :erlang.tuple_to_list(t)}
      end

    case r do
      {files, output, r_opts() = o} ->
        do_sort(keyPos, files, output, o, :merge)

      {_, _, o} ->
        badarg(culprit(o), [keyPos, files0, output0, options])
    end
  end

  def check(fileName) do
    check([fileName], [])
  end

  def check(files0, options) do
    case {is_files(files0), options(options)} do
      {{true, files}, r_opts() = opts} ->
        do_sort(0, files, :undefined, opts, :check)

      t ->
        badarg(
          culprit(:erlang.tuple_to_list(t)),
          [files0, options]
        )
    end
  end

  def keycheck(keyPos, fileName) do
    keycheck(keyPos, [fileName], [])
  end

  def keycheck(keyPos, files0, options) do
    r =
      case {is_keypos(keyPos), is_files(files0), options(options)} do
        {_, _, r_opts(format: :binary)} ->
          {files0, [{:badarg, :format}]}

        {_, _, r_opts(order: order)} when is_function(order) ->
          {files0, [{:badarg, :order}]}

        {true, {true, fs}, r_opts() = opts} ->
          {fs, opts}

        t ->
          {files0, :erlang.tuple_to_list(t)}
      end

    case r do
      {files, r_opts() = o} ->
        do_sort(keyPos, files, :undefined, o, :check)

      {_, o} ->
        badarg(culprit(o), [keyPos, files0, options])
    end
  end

  defp culprit([{:error, _} = e | _]) do
    e
  end

  defp culprit([{:badarg, _} = b | _]) do
    b
  end

  defp culprit([_ | b]) do
    culprit(b)
  end

  defp badarg({:error, _} = e, _Args) do
    e
  end

  defp badarg({:badarg, _} = b, args) do
    :erlang.error(b, args)
  end

  defp options(options) when is_list(options) do
    options(options, r_opts())
  end

  defp options(option) do
    options([option])
  end

  defp options([{:format, format} | l], opts)
       when format === :binary or format === :term or
              is_function(format, 1) do
    options(l, r_opts(opts, format: format))
  end

  defp options([{:format, :binary_term} | l], opts) do
    options(l, r_opts(opts, format: binary_term_fun()))
  end

  defp options([{:size, size} | l], opts)
       when is_integer(size) and size >= 0 do
    options(l, r_opts(opts, size: :erlang.max(size, 1)))
  end

  defp options([{:no_files, noFiles} | l], opts)
       when is_integer(noFiles) and noFiles > 1 do
    options(l, r_opts(opts, no_files: noFiles))
  end

  defp options([{:tmpdir, ''} | l], opts) do
    options(l, r_opts(opts, tmpdir: :default))
  end

  defp options([{:tmpdir, dir} | l], opts) do
    case (try do
            :filename.absname(dir)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:badarg, dir}

      fileName ->
        options(l, r_opts(opts, tmpdir: {:dir, fileName}))
    end
  end

  defp options([{:order, fun} | l], opts)
       when is_function(
              fun,
              2
            ) do
    options(l, r_opts(opts, order: fun))
  end

  defp options([{:order, order} | l], opts)
       when order === :ascending or order === :descending do
    options(l, r_opts(opts, order: order))
  end

  defp options([{:compressed, bool} | l], opts)
       when is_boolean(bool) do
    options(l, r_opts(opts, compressed: bool))
  end

  defp options([{:unique, bool} | l], opts)
       when is_boolean(bool) do
    options(l, r_opts(opts, unique: bool))
  end

  defp options([{:header, len} | l], opts)
       when is_integer(len) and len > 0 and len < 1 <<< 31 do
    options(l, r_opts(opts, header: len))
  end

  defp options([], opts) do
    cond do
      r_opts(opts, :format) === :term and
          r_opts(opts, :header) !== 4 ->
        {:badarg, :header}

      true ->
        opts
    end
  end

  defp options([bad | _], _Opts) do
    {:badarg, bad}
  end

  defp options(bad, _Opts) do
    {:badarg, bad}
  end

  defp do_sort(keyPos0, input0, output0, opts, do__) do
    r_opts(
      format: format0,
      size: size,
      no_files: noFiles,
      tmpdir: tmpDir,
      order: order,
      compressed: compressed,
      unique: unique,
      header: hdLen
    ) = opts

    prefix = tmp_prefix(output0, tmpDir)
    chunkSize = 16384
    ref = make_ref()

    keyPos =
      case keyPos0 do
        [kp] ->
          kp

        _ ->
          keyPos0
      end

    {format, input} = wrap_input(format0, do__, input0)

    z =
      cond do
        compressed ->
          [:compressed]

        true ->
          []
      end

    {output, funOut} = wrap_output_terms(format0, output0, z)

    w =
      r_w(
        keypos: keyPos,
        out: output,
        fun_out: funOut,
        prefix: prefix,
        format: format,
        runsize: size,
        no_files: noFiles,
        order: order,
        chunksize: chunkSize,
        ref: ref,
        z: z,
        unique: unique,
        hdlen: hdLen,
        inout_value: :no_value
      )

    try do
      doit(do__, input, w)
    catch
      {^ref, error} ->
        error
    end
  end

  defp doit(:sort, input, w) do
    files(1, [], 0, w, input)
  end

  defp doit(:merge, input, w) do
    last_merge(input, w)
  end

  defp doit(:check, input, w) do
    check_files(input, w, [])
  end

  defp wrap_input(:term, :check, files) do
    fun = fn file ->
      fn__ = merge_terms_fun(file_rterms(:no_file, [file]))
      {:fn, fn__, file}
    end

    {binary_term_fun(),
     for f <- files do
       fun.(f)
     end}
  end

  defp wrap_input(format, :check, files) do
    {format, files}
  end

  defp wrap_input(:term, :merge, files) do
    fun = fn file ->
      merge_terms_fun(file_rterms(:no_file, [file]))
    end

    input =
      :lists.reverse(
        for f <- files do
          fun.(f)
        end
      )

    {binary_term_fun(), input}
  end

  defp wrap_input(format, :merge, files) do
    input =
      :lists.reverse(
        for f <- files do
          merge_bins_fun(f)
        end
      )

    {format, input}
  end

  defp wrap_input(:term, :sort, inFun)
       when is_function(
              inFun,
              1
            ) do
    {binary_term_fun(), fun_rterms(inFun)}
  end

  defp wrap_input(:term, :sort, files) do
    {binary_term_fun(), file_rterms(:no_file, files)}
  end

  defp wrap_input(format, :sort, input) do
    {format, input}
  end

  defp merge_terms_fun(rFun) do
    fn
      :close ->
        rFun.(:close)

      {i, [], _LSz, w} ->
        case rFun.(:read) do
          :end_of_input ->
            :eof

          {objs, nRFun} when is_function(nRFun, 1) ->
            {_, [], ts, _} = fun_objs(objs, [], 0, 1 <<< 31, i, w)
            {{i, ts, 16384}, merge_terms_fun(nRFun)}

          error ->
            :erlang.error(error, w)
        end
    end
  end

  defp merge_bins_fun(fileName) do
    fn
      :close ->
        :ok

      {_I, _L, _LSz, w} = a ->
        fun = read_fun(fileName, :user, w)
        fun.(a)
    end
  end

  defp wrap_output_terms(:term, outFun, _Z)
       when is_function(
              outFun,
              1
            ) do
    {fun_wterms(outFun), true}
  end

  defp wrap_output_terms(:term, file, z) when file !== :undefined do
    {file_wterms(:name, file, z ++ [:write]), false}
  end

  defp wrap_output_terms(_Format, output, _Z) do
    {output, is_function(output, 1)}
  end

  defp binary_term_fun() do
    &binary_to_term/1
  end

  defp check_files([], _W, l) do
    {:ok, :lists.reverse(l)}
  end

  defp check_files([fN | fNs], w, l) do
    {iFun, fileName} =
      case fN do
        {:fn, fun, file} ->
          {fun, file}

        file ->
          {read_fun(file, :user, w), file}
      end

    nW = r_w(w, in: iFun)
    check_run(iFun, fileName, fNs, nW, l, 2, :nolast)
  end

  defp check_run(iFun, f, fNs, w, l, i, last) do
    case iFun.({{:merge, i}, [], 0, w}) do
      {{_I, objs, _LSz}, iFun1} ->
        nW = r_w(w, in: iFun1)
        check_objs0(iFun1, f, fNs, nW, l, i, last, :lists.reverse(objs))

      :eof ->
        nW = r_w(w, in: :undefined)
        check_files(fNs, nW, l)
    end
  end

  defp check_objs0(iFun, f, fNs, w, l, i, :nolast, [{t, _BT} | os]) do
    check_objs1(iFun, f, fNs, w, l, i, t, os)
  end

  defp check_objs0(iFun, f, fNs, w, l, i, last, []) do
    check_run(iFun, f, fNs, w, l, i, last)
  end

  defp check_objs0(iFun, f, fNs, w, l, i, {:last, last}, os) do
    check_objs1(iFun, f, fNs, w, l, i, last, os)
  end

  defp check_objs1(iFun, f, fNs, w, l, i, lastT, os) do
    case w do
      r_w(order: :ascending, unique: true) ->
        ucheck_objs(iFun, f, fNs, w, l, i, lastT, os)

      r_w(order: :ascending, unique: false) ->
        check_objs(iFun, f, fNs, w, l, i, lastT, os)

      r_w(order: :descending, unique: true) ->
        rucheck_objs(iFun, f, fNs, w, l, i, lastT, os)

      r_w(order: :descending, unique: false) ->
        rcheck_objs(iFun, f, fNs, w, l, i, lastT, os)

      r_w(order: cF, unique: true) ->
        uccheck_objs(iFun, f, fNs, w, l, i, lastT, os, cF)

      r_w(order: cF, unique: false) ->
        ccheck_objs(iFun, f, fNs, w, l, i, lastT, os, cF)
    end
  end

  defp check_objs(iFun, f, fNs, w, l, i, last, [{t, _BT} | os])
       when t >= last do
    check_objs(iFun, f, fNs, w, l, i + 1, t, os)
  end

  defp check_objs(iFun, f, fNs, w, l, i, _Last, [{_T, bT} | _]) do
    culprit_found(iFun, f, fNs, w, l, i, bT)
  end

  defp check_objs(iFun, f, fNs, w, l, i, last, []) do
    check_run(iFun, f, fNs, w, l, i, {:last, last})
  end

  defp rcheck_objs(iFun, f, fNs, w, l, i, last, [{t, _BT} | os])
       when t <= last do
    rcheck_objs(iFun, f, fNs, w, l, i + 1, t, os)
  end

  defp rcheck_objs(iFun, f, fNs, w, l, i, _Last, [{_T, bT} | _]) do
    culprit_found(iFun, f, fNs, w, l, i, bT)
  end

  defp rcheck_objs(iFun, f, fNs, w, l, i, last, []) do
    check_run(iFun, f, fNs, w, l, i, {:last, last})
  end

  defp ucheck_objs(iFun, f, fNs, w, l, i, lT, [{t, _BT} | os])
       when t > lT do
    ucheck_objs(iFun, f, fNs, w, l, i + 1, t, os)
  end

  defp ucheck_objs(iFun, f, fNs, w, l, i, _LT, [{_T, bT} | _]) do
    culprit_found(iFun, f, fNs, w, l, i, bT)
  end

  defp ucheck_objs(iFun, f, fNs, w, l, i, lT, []) do
    check_run(iFun, f, fNs, w, l, i, {:last, lT})
  end

  defp rucheck_objs(iFun, f, fNs, w, l, i, lT, [{t, _BT} | os])
       when t < lT do
    rucheck_objs(iFun, f, fNs, w, l, i + 1, t, os)
  end

  defp rucheck_objs(iFun, f, fNs, w, l, i, _LT, [{_T, bT} | _]) do
    culprit_found(iFun, f, fNs, w, l, i, bT)
  end

  defp rucheck_objs(iFun, f, fNs, w, l, i, lT, []) do
    check_run(iFun, f, fNs, w, l, i, {:last, lT})
  end

  defp ccheck_objs(iFun, f, fNs, w, l, i, lT, [{t, bT} | os], cF) do
    case cF.(lT, t) do
      true ->
        ccheck_objs(iFun, f, fNs, w, l, i + 1, t, os, cF)

      false ->
        culprit_found(iFun, f, fNs, w, l, i, bT)
    end
  end

  defp ccheck_objs(iFun, f, fNs, w, l, i, lT, [], _CF) do
    check_run(iFun, f, fNs, w, l, i, {:last, lT})
  end

  defp uccheck_objs(iFun, f, fNs, w, l, i, lT, [{t, bT} | os], cF) do
    case cF.(lT, t) do
      true ->
        case cF.(t, lT) do
          true ->
            culprit_found(iFun, f, fNs, w, l, i, bT)

          false ->
            uccheck_objs(iFun, f, fNs, w, l, i + 1, t, os, cF)
        end

      false ->
        culprit_found(iFun, f, fNs, w, l, i, bT)
    end
  end

  defp uccheck_objs(iFun, f, fNs, w, l, i, lT, [], _CF) do
    check_run(iFun, f, fNs, w, l, i, {:last, lT})
  end

  defp culprit_found(iFun, f, fNs, w, l, i, [_Size | bT]) do
    iFun.(:close)
    check_files(fNs, w, [{f, i, :erlang.binary_to_term(bT)} | l])
  end

  defp files(_I, l, _LSz, r_w(seq: 1, out: out) = w, []) do
    case out do
      fun when is_function(fun) ->
        sL = internal_sort(l, w)
        w1 = outfun(binterm_objects(sL, []), w)
        nW = close_input(w1)
        outfun(:close, nW)

      ^out ->
        _ = write_run(l, w, out)
        :ok
    end
  end

  defp files(_I, l, _LSz, w, []) do
    w1 = write_run(l, w)
    last_merge(:lists.append(r_w(w1, :runs)), w1)
  end

  defp files(i, l, lSz, w, fun) when is_function(fun) do
    nW = r_w(w, in: fun)
    fun_run(i, l, lSz, nW, [])
  end

  defp files(i, l, lSz, w, [fileName | fileNames]) do
    inFun = read_fun(fileName, :user, w)
    nW = r_w(w, in: inFun)
    file_run(inFun, fileNames, i, l, lSz, nW)
  end

  defp file_run(inFun, fileNames, i, l, lSz, w)
       when lSz < r_w(w, :runsize) do
    case inFun.({i, l, lSz, w}) do
      {{i1, l1, lSz1}, inFun1} ->
        nW = r_w(w, in: inFun1)
        file_run(inFun1, fileNames, i1, l1, lSz1, nW)

      :eof ->
        nW = r_w(w, in: :undefined)
        files(i, l, lSz, nW, fileNames)
    end
  end

  defp file_run(inFun, fileNames, i, l, _LSz, w) do
    nW = write_run(l, w)
    file_run(inFun, fileNames, i, [], 0, nW)
  end

  defp fun_run(i, l, lSz, w, []) do
    case infun(w) do
      {:end_of_input, nW} ->
        files(i, l, lSz, nW, [])

      {:cont, nW, objs} ->
        fun_run(i, l, lSz, nW, objs)
    end
  end

  defp fun_run(i, l, lSz, r_w(runsize: runsize) = w, objs)
       when lSz < runsize do
    {nI, nObjs, nL, nLSz} = fun_objs(objs, l, lSz, runsize, i, w)
    fun_run(nI, nL, nLSz, w, nObjs)
  end

  defp fun_run(i, l, _LSz, w, objs) do
    nW = write_run(l, w)
    fun_run(i, [], 0, nW, objs)
  end

  defp write_run([], w) do
    w
  end

  defp write_run(l, w) do
    {w1, temp} = next_temp(w)
    nW = write_run(l, w1, temp)
    [r | rs] = r_w(nW, :runs)
    merge_runs([[temp | r] | rs], [], nW)
  end

  defp write_run(l, w, fileName) do
    sL = internal_sort(l, w)
    bTs = binterms(sL, [])
    {fd, w1} = open_file(fileName, w)
    write(fd, fileName, bTs, w1)
    close_file(fd, w1)
  end

  defp internal_sort([] = l, _W) do
    l
  end

  defp internal_sort(l, r_w(order: cFun, unique: unique))
       when is_function(cFun) do
    fun = fn {t1, _}, {t2, _} ->
      cFun.(t1, t2)
    end

    rL = :lists.reverse(l)

    :lists.reverse(
      cond do
        unique ->
          :lists.usort(fun, rL)

        true ->
          :lists.sort(fun, rL)
      end
    )
  end

  defp internal_sort(l, r_w(unique: true, keypos: 0) = w) do
    rev(:lists.usort(l), w)
  end

  defp internal_sort(l, r_w(unique: false, keypos: 0) = w) do
    rev(:lists.sort(l), w)
  end

  defp internal_sort(l, r_w(unique: true) = w) do
    rev(:lists.ukeysort(1, :lists.reverse(l)), w)
  end

  defp internal_sort(l, r_w(unique: false) = w) do
    rev(:lists.keysort(1, :lists.reverse(l)), w)
  end

  defp rev(l, r_w(order: :ascending)) do
    :lists.reverse(l)
  end

  defp rev(l, r_w(order: :descending)) do
    l
  end

  defp last_merge(r, w) when length(r) <= r_w(w, :no_files) do
    case r_w(w, :out) do
      fun when is_function(fun) ->
        {fs, w1} = init_merge(:lists.reverse(r), 1, [], w)
        :ok
        w2 = merge_files(fs, [], 0, :nolast, w1)
        nW = close_input(w2)
        outfun(:close, nW)

      out ->
        _ = merge_files(r, w, out)
        :ok
    end
  end

  defp last_merge(r, w) do
    l = :lists.sublist(r, r_w(w, :no_files))
    {m, nW} = merge_files(l, w)
    last_merge([m | :lists.nthtail(r_w(w, :no_files), r)], nW)
  end

  defp merge_runs([r | rs], nRs0, w)
       when length(r) < r_w(w, :no_files) do
    nRs = :lists.reverse(nRs0) ++ [r | rs]
    r_w(w, runs: nRs)
  end

  defp merge_runs([r], nRs0, w) do
    {m, nW} = merge_files(r, w)
    nRs = [[] | :lists.reverse([[m] | nRs0])]
    r_w(nW, runs: nRs)
  end

  defp merge_runs([[r, r1] | rs], nRs0, w) do
    {m, nW} = merge_files(r, w)
    merge_runs([[m | r1] | rs], [[] | nRs0], nW)
  end

  defp merge_files(r, w) do
    {w1, temp} = next_temp(w)
    :ok
    {temp, merge_files(r, w1, temp)}
  end

  defp merge_files(r, w, fileName) do
    {fs, w1} = init_merge(:lists.reverse(r), 1, [], w)
    {fd, w2} = open_file(fileName, w1)
    w3 = r_w(w2, wfd: {fd, fileName})
    w4 = merge_files(fs, [], 0, :nolast, w3)
    nW = r_w(w4, wfd: :undefined)
    close_file(fd, nW)
  end

  defp init_merge([fN | fNs], i, fs, w) do
    iFun =
      case fN do
        _ when is_function(fN) ->
          fN

        _ ->
          read_fun(fN, :fsort, w)
      end

    w1 = r_w(w, temp: [iFun | :lists.delete(fN, r_w(w, :temp))])

    case read_more(iFun, i, 0, w1) do
      {ts, _LSz, nIFun, nW} ->
        inEtc = {i, nIFun}
        init_merge(fNs, i + 1, [[ts | inEtc] | fs], nW)

      {:eof, nW} ->
        init_merge(fNs, i + 1, fs, nW)
    end
  end

  defp init_merge([], _I, fs0, r_w(order: :ascending) = w) do
    {:lists.sort(fs0), w}
  end

  defp init_merge([], _I, fs0, r_w(order: :descending) = w) do
    {:lists.reverse(:lists.sort(fs0)), w}
  end

  defp init_merge([], _I, fs0, r_w(order: order) = w)
       when is_function(order) do
    {:lists.sort(
       cfun_files(r_w(w, :order)),
       :lists.reverse(fs0)
     ), w}
  end

  defp cfun_files(cFun) do
    fn f1, f2 ->
      [[{t1, _} | _] | _] = f1
      [[{t2, _} | _] | _] = f2
      cFun.(t1, t2)
    end
  end

  defp merge_files([[f1, f2] | fs], l0, lSz, last0, w)
       when lSz < 16384 do
    [ts0 | inEtc] = f1
    kind = merge_kind(w)

    {last, l, ts} =
      case {last0, kind} do
        {{:last, lst}, ^kind} ->
          {lst, l0, ts0}

        {:nolast, {:ukmerge, _Kp}} ->
          [{[t | _I], bT} | ts1] = ts0
          {t, [bT], ts1}

        {:nolast, {:rukmerge, _Kp}} ->
          [{[t | _I], bT} | ts1] = ts0
          {{t, bT}, [], ts1}

        {:nolast, _} ->
          [{t, bT} | ts1] = ts0
          {t, [bT], ts1}
      end

    [[{t2, bT2} | ts2T] = ts2 | inEtc2] = f2

    {nInEtc, nFs, nL, nLast} =
      case kind do
        :umerge ->
          umerge_files(l, f2, fs, inEtc2, ts2, ts, inEtc, t2, last)

        {:ukmerge, kp} ->
          ukmerge_files(l, f2, fs, inEtc2, ts2, ts, inEtc, t2, kp, last)

        :merge ->
          merge_files(l, f2, fs, inEtc2, bT2, ts2T, ts, inEtc, t2)

        :rumerge ->
          rumerge_files(l, f2, fs, inEtc2, ts2, ts, inEtc, t2, last)

        {:rukmerge, kp} ->
          {lt, ltBT} = last
          rukmerge_files(l, f2, fs, inEtc2, ts2, ts, inEtc, t2, kp, lt, ltBT)

        :rmerge ->
          rmerge_files(l, f2, fs, inEtc2, bT2, ts2T, ts, inEtc, t2)

        {:ucmerge, cF} ->
          {i2, _} = inEtc2
          {i, _} = inEtc
          ucmerge_files(l, f2, fs, inEtc2, ts2, i2, ts, i, inEtc, t2, cF, last)

        {:cmerge, cF} ->
          {i2, _} = inEtc2
          {i, _} = inEtc
          cmerge_files(l, f2, fs, inEtc2, bT2, ts2T, i2, ts, i, inEtc, t2, cF)
      end

    read_chunk(nInEtc, nFs, nL, lSz, nLast, w)
  end

  defp merge_files([f1], l, lSz, last, w) when lSz < 16384 do
    [ts | inEtc] = f1
    nL = last_file(ts, l, last, merge_kind(w), w)
    read_chunk(inEtc, [], nL, lSz, :nolast, w)
  end

  defp merge_files([], [], 0, :nolast, w) do
    merge_write(w, [])
  end

  defp merge_files([], l, _LSz, last, w) do
    ^last = :nolast
    merge_write(w, l)
  end

  defp merge_files(fs, l, _LSz, last, w) do
    nW = merge_write(w, l)
    merge_files(fs, [], 0, last, nW)
  end

  defp merge_kind(r_w(order: :ascending, unique: true, keypos: 0)) do
    :umerge
  end

  defp merge_kind(r_w(order: :ascending, unique: true, keypos: kp)) do
    {:ukmerge, kp}
  end

  defp merge_kind(r_w(order: :ascending, unique: false)) do
    :merge
  end

  defp merge_kind(r_w(order: :descending, unique: true, keypos: 0)) do
    :rumerge
  end

  defp merge_kind(r_w(order: :descending, unique: true, keypos: kp)) do
    {:rukmerge, kp}
  end

  defp merge_kind(r_w(order: :descending, unique: false)) do
    :rmerge
  end

  defp merge_kind(r_w(order: cF, unique: true)) do
    {:ucmerge, cF}
  end

  defp merge_kind(r_w(order: cF, unique: false)) do
    {:cmerge, cF}
  end

  defp merge_write(w, l) do
    case {r_w(w, :wfd), r_w(w, :out)} do
      {:undefined, fun} when is_function(fun) ->
        outfun(objects(l, []), w)

      {{fd, fileName}, _} ->
        write(fd, fileName, :lists.reverse(l), w)
        w
    end
  end

  defp umerge_files(l, f2, fs, inEtc2, ts2, [{t, _BT} | ts], inEtc, t2, last)
       when t == last do
    umerge_files(l, f2, fs, inEtc2, ts2, ts, inEtc, t2, last)
  end

  defp umerge_files(l, f2, fs, inEtc2, ts2, [{t, bT} | ts], inEtc, t2, _Last)
       when t <= t2 do
    umerge_files([bT | l], f2, fs, inEtc2, ts2, ts, inEtc, t2, t)
  end

  defp umerge_files(l, f2, fs, _InEtc2, _Ts2, [], inEtc, _T2, last) do
    {inEtc, [f2 | fs], l, {:last, last}}
  end

  defp umerge_files(l, _F2, fs, inEtc2, ts2, ts, inEtc, _T2, last) do
    [f3 | nFs] = insert([ts | inEtc], fs)
    [[{t3, _BT3} | _] = ts3 | inEtc3] = f3
    umerge_files(l, f3, nFs, inEtc3, ts3, ts2, inEtc2, t3, last)
  end

  defp rumerge_files(l, f2, fs, inEtc2, ts2, [{t, _BT} | ts], inEtc, t2, last)
       when t == last do
    rumerge_files(l, f2, fs, inEtc2, ts2, ts, inEtc, t2, last)
  end

  defp rumerge_files(l, f2, fs, inEtc2, ts2, [{t, bT} | ts], inEtc, t2, _Last)
       when t >= t2 do
    rumerge_files([bT | l], f2, fs, inEtc2, ts2, ts, inEtc, t2, t)
  end

  defp rumerge_files(l, f2, fs, _InEtc2, _Ts2, [], inEtc, _T2, last) do
    {inEtc, [f2 | fs], l, {:last, last}}
  end

  defp rumerge_files(l, _F2, fs, inEtc2, ts2, ts, inEtc, _T2, last) do
    [f3 | nFs] = rinsert([ts | inEtc], fs)
    [[{t3, _BT3} | _] = ts3 | inEtc3] = f3
    rumerge_files(l, f3, nFs, inEtc3, ts3, ts2, inEtc2, t3, last)
  end

  defp merge_files(l, f2, fs, inEtc2, bT2, ts2, [{t, bT} | ts], inEtc, t2)
       when t <= t2 do
    merge_files([bT | l], f2, fs, inEtc2, bT2, ts2, ts, inEtc, t2)
  end

  defp merge_files(l, f2, fs, _InEtc2, _BT2, _Ts2, [], inEtc, _T2) do
    {inEtc, [f2 | fs], l, {:last, :foo}}
  end

  defp merge_files(l, _F2, fs, inEtc2, bT2, ts2, ts, inEtc, _T2) do
    l1 = [bT2 | l]
    [f3 | nFs] = insert([ts | inEtc], fs)
    [[{t3, bT3} | ts3] | inEtc3] = f3
    merge_files(l1, f3, nFs, inEtc3, bT3, ts3, ts2, inEtc2, t3)
  end

  defp rmerge_files(l, f2, fs, inEtc2, bT2, ts2, [{t, bT} | ts], inEtc, t2)
       when t >= t2 do
    rmerge_files([bT | l], f2, fs, inEtc2, bT2, ts2, ts, inEtc, t2)
  end

  defp rmerge_files(l, f2, fs, _InEtc2, _BT2, _Ts2, [], inEtc, _T2) do
    {inEtc, [f2 | fs], l, {:last, :foo}}
  end

  defp rmerge_files(l, _F2, fs, inEtc2, bT2, ts2, ts, inEtc, _T2) do
    l1 = [bT2 | l]
    [f3 | nFs] = rinsert([ts | inEtc], fs)
    [[{t3, bT3} | ts3] | inEtc3] = f3
    rmerge_files(l1, f3, nFs, inEtc3, bT3, ts3, ts2, inEtc2, t3)
  end

  defp ukmerge_files(l, f2, fs, inEtc2, ts2, [{[t | _I], _BT} | ts], inEtc, t2, kp, last)
       when t == last do
    ukmerge_files(l, f2, fs, inEtc2, ts2, ts, inEtc, t2, kp, last)
  end

  defp ukmerge_files(l, f2, fs, inEtc2, ts2, [{[t0 | _I] = t, bT} | ts], inEtc, t2, kp, _Last)
       when t <= t2 do
    ukmerge_files([bT | l], f2, fs, inEtc2, ts2, ts, inEtc, t2, kp, t0)
  end

  defp ukmerge_files(l, f2, fs, _InEtc2, _Ts2, [], inEtc, _T2, _Kp, last) do
    {inEtc, [f2 | fs], l, {:last, last}}
  end

  defp ukmerge_files(l, _F2, fs, inEtc2, ts2, ts, inEtc, _T2, kp, last) do
    [f3 | nFs] = insert([ts | inEtc], fs)
    [[{t3, _BT3} | _] = ts3 | inEtc3] = f3
    ukmerge_files(l, f3, nFs, inEtc3, ts3, ts2, inEtc2, t3, kp, last)
  end

  defp rukmerge_files(l, f2, fs, inEtc2, ts2, [{[t | _I], bT} | ts], inEtc, t2, kp, last, _LastBT)
       when t == last do
    rukmerge_files(l, f2, fs, inEtc2, ts2, ts, inEtc, t2, kp, t, bT)
  end

  defp rukmerge_files(
         l,
         f2,
         fs,
         inEtc2,
         ts2,
         [{[t0 | _I] = t, bT} | ts],
         inEtc,
         t2,
         kp,
         _Last,
         lastBT
       )
       when t >= t2 do
    rukmerge_files([lastBT | l], f2, fs, inEtc2, ts2, ts, inEtc, t2, kp, t0, bT)
  end

  defp rukmerge_files(l, f2, fs, _InEtc2, _Ts2, [], inEtc, _T2, _Kp, last, lastBT) do
    {inEtc, [f2 | fs], l, {:last, {last, lastBT}}}
  end

  defp rukmerge_files(l, _F2, fs, inEtc2, ts2, ts, inEtc, _T2, kp, last, lastBT) do
    [f3 | nFs] = rinsert([ts | inEtc], fs)
    [[{t3, _BT3} | _] = ts3 | inEtc3] = f3
    rukmerge_files(l, f3, nFs, inEtc3, ts3, ts2, inEtc2, t3, kp, last, lastBT)
  end

  defp ucmerge_files(l, f2, fs, inEtc2, ts2, i2, [{t, bT} | ts] = ts0, i, inEtc, t2, cF, last)
       when i < i2 do
    case cF.(t, t2) do
      true ->
        case cF.(t, last) do
          true ->
            ucmerge_files(l, f2, fs, inEtc2, ts2, i2, ts, i, inEtc, t2, cF, last)

          false ->
            ucmerge_files([bT | l], f2, fs, inEtc2, ts2, i2, ts, i, inEtc, t2, cF, t)
        end

      false ->
        [f3 | nFs] = cinsert([ts0 | inEtc], fs, cF)
        [[{t3, _BT3} | _] = ts3 | {i3, _} = inEtc3] = f3
        ucmerge_files(l, f3, nFs, inEtc3, ts3, i3, ts2, i2, inEtc2, t3, cF, last)
    end
  end

  defp ucmerge_files(l, f2, fs, inEtc2, ts2, i2, [{t, bT} | ts] = ts0, i, inEtc, t2, cF, last) do
    case cF.(t2, t) do
      true ->
        [f3 | nFs] = cinsert([ts0 | inEtc], fs, cF)
        [[{t3, _BT3} | _] = ts3 | {i3, _} = inEtc3] = f3
        ucmerge_files(l, f3, nFs, inEtc3, ts3, i3, ts2, i2, inEtc2, t3, cF, last)

      false ->
        case cF.(t, last) do
          true ->
            ucmerge_files(l, f2, fs, inEtc2, ts2, i2, ts, i, inEtc, t2, cF, last)

          false ->
            ucmerge_files([bT | l], f2, fs, inEtc2, ts2, i2, ts, i, inEtc, t2, cF, t)
        end
    end
  end

  defp ucmerge_files(l, f2, fs, _InEtc2, _Ts2, _I2, [], _I, inEtc, _T2, _CF, last) do
    {inEtc, [f2 | fs], l, {:last, last}}
  end

  defp cmerge_files(l, f2, fs, inEtc2, bT2, ts2, i2, [{t, bT} | ts] = ts0, i, inEtc, t2, cF)
       when i < i2 do
    case cF.(t, t2) do
      true ->
        cmerge_files([bT | l], f2, fs, inEtc2, bT2, ts2, i2, ts, i, inEtc, t2, cF)

      false ->
        l1 = [bT2 | l]
        [f3 | nFs] = cinsert([ts0 | inEtc], fs, cF)
        [[{t3, bT3} | ts3] | {i3, _} = inEtc3] = f3
        cmerge_files(l1, f3, nFs, inEtc3, bT3, ts3, i3, ts2, i2, inEtc2, t3, cF)
    end
  end

  defp cmerge_files(l, f2, fs, inEtc2, bT2, ts2, i2, [{t, bT} | ts] = ts0, i, inEtc, t2, cF) do
    case cF.(t2, t) do
      true ->
        l1 = [bT2 | l]
        [f3 | nFs] = cinsert([ts0 | inEtc], fs, cF)
        [[{t3, bT3} | ts3] | {i3, _} = inEtc3] = f3
        cmerge_files(l1, f3, nFs, inEtc3, bT3, ts3, i3, ts2, i2, inEtc2, t3, cF)

      false ->
        cmerge_files([bT | l], f2, fs, inEtc2, bT2, ts2, i2, ts, i, inEtc, t2, cF)
    end
  end

  defp cmerge_files(l, f2, fs, _InEtc2, _BT2, _Ts2, _I2, [], _I, inEtc, _T2, _CF) do
    {inEtc, [f2 | fs], l, {:last, :foo}}
  end

  defp last_file(ts, l, {:last, t}, {:ukmerge, _}, _W) do
    kulast_file(ts, t, l)
  end

  defp last_file(ts, l, {:last, {t, bT}}, {:rukmerge, _}, _W) do
    ruklast_file(ts, t, bT, l)
  end

  defp last_file(ts, l, {:last, t}, {:ucmerge, cF}, _W) do
    uclast_file(ts, t, cF, l)
  end

  defp last_file(ts, l, {:last, t}, _Kind, r_w(unique: true)) do
    ulast_file(ts, t, l)
  end

  defp last_file(ts, l, _Last, _Kind, _W) do
    last_file(ts, l)
  end

  defp ulast_file([{t, _BT} | ts], last, l) when last == t do
    last_file(ts, l)
  end

  defp ulast_file(ts, _Last, l) do
    last_file(ts, l)
  end

  defp kulast_file([{[t | _I], _BT} | ts], last, l)
       when last == t do
    last_file(ts, l)
  end

  defp kulast_file(ts, _Last, l) do
    last_file(ts, l)
  end

  defp ruklast_file([{[t | _I], bT} | ts], last, _LastBT, l)
       when last == t do
    last_file(ts, [bT | l])
  end

  defp ruklast_file(ts, _Last, lastBT, l) do
    last_file(ts, [lastBT | l])
  end

  defp uclast_file([{t, bT} | ts], last, cF, l) do
    case cF.(t, last) do
      true ->
        last_file(ts, l)

      false ->
        last_file(ts, [bT | l])
    end
  end

  defp last_file([[{_Ta, bTa}, {_Tb, bTb}] | ts], l) do
    last_file(ts, [[bTb, bTa] | l])
  end

  defp last_file([{_T, bT} | ts], l) do
    last_file(ts, [bT | l])
  end

  defp last_file([], l) do
    l
  end

  defp insert(a, [[x1, x2, x3, x4] | xs]) when a > x4 do
    [[x1, x2, x3, x4] | insert(a, xs)]
  end

  defp insert(a, [[x1, x2, x3] | t]) when a > x3 do
    [[x1, x2, x3, a] | t]
  end

  defp insert(a, [[x1, x2] | xs]) when a > x2 do
    [[x1, x2, a] | xs]
  end

  defp insert(a, [x1 | t]) when a > x1 do
    [[x1, a] | t]
  end

  defp insert(a, xs) do
    [a | xs]
  end

  defp rinsert(a, [[x1, x2, x3, x4] | xs]) when a < x4 do
    [[x1, x2, x3, x4] | rinsert(a, xs)]
  end

  defp rinsert(a, [[x1, x2, x3] | t]) when a < x3 do
    [[x1, x2, x3, a] | t]
  end

  defp rinsert(a, [[x1, x2] | xs]) when a < x2 do
    [[x1, x2, a] | xs]
  end

  defp rinsert(a, [x1 | t]) when a < x1 do
    [[x1, a] | t]
  end

  defp rinsert(a, xs) do
    [a | xs]
  end

  defp cinsert(
         a,
         [f1 | [f2 | [f3 | [f4 | fs] = t4] = t3] = t2] = t1,
         cF
       ) do
    case cfun(cF, f4, a) do
      true ->
        [[f1, f2, f3, f4] | cinsert(a, fs, cF)]

      false ->
        case cfun(cF, f2, a) do
          true ->
            [
              [f1, f2]
              | case cfun(cF, f3, a) do
                  true ->
                    [[f3, a] | t4]

                  false ->
                    [a | t3]
                end
            ]

          false ->
            case cfun(cF, f1, a) do
              true ->
                [[f1, a] | t2]

              false ->
                [a | t1]
            end
        end
    end
  end

  defp cinsert(a, [f1 | [f2 | fs] = t2] = t1, cF) do
    case cfun(cF, f2, a) do
      true ->
        [[f1, f2] | cinsert(a, fs, cF)]

      false ->
        case cfun(cF, f1, a) do
          true ->
            [[f1, a] | t2]

          false ->
            [a | t1]
        end
    end
  end

  defp cinsert(a, [f | fs] = t, cF) do
    case cfun(cF, f, a) do
      true ->
        [[f, a] | fs]

      false ->
        [a | t]
    end
  end

  defp cinsert(a, _, _CF) do
    [a]
  end

  defp cfun(cF, f1, f2) do
    [[{t1, _} | _] | {i1, _}] = f1
    [[{t2, _} | _] | {i2, _}] = f2

    cond do
      i1 < i2 ->
        cF.(t1, t2)

      true ->
        not cF.(t2, t1)
    end
  end

  defp binterm_objects([{_T, [_Sz | bT]} | ts], l) do
    binterm_objects(ts, [bT | l])
  end

  defp binterm_objects([], l) do
    l
  end

  defp objects([[_Sz | bT] | ts], l) do
    objects(ts, [bT | l])
  end

  defp objects([], l) do
    l
  end

  defp binterms([[{_T1, bT1}, {_T2, bT2}] | ts], l) do
    binterms(ts, [[bT2, bT1] | l])
  end

  defp binterms([{_T, bT} | ts], l) do
    binterms(ts, [bT | l])
  end

  defp binterms([], l) do
    l
  end

  defp read_chunk(inEtc, fs, l, lSz, last, w) do
    {i, iFun} = inEtc

    case read_more(iFun, i, lSz, w) do
      {ts, nLSz, nIFun, r_w(order: :ascending) = nW} ->
        nInEtc = {i, nIFun}
        nFs = insert([ts | nInEtc], fs)
        merge_files(nFs, l, nLSz, last, nW)

      {ts, nLSz, nIFun, r_w(order: :descending) = nW} ->
        nInEtc = {i, nIFun}
        nFs = rinsert([ts | nInEtc], fs)
        merge_files(nFs, l, nLSz, last, nW)

      {ts, nLSz, nIFun, nW} ->
        nInEtc = {i, nIFun}
        nFs = cinsert([ts | nInEtc], fs, r_w(nW, :order))
        merge_files(nFs, l, nLSz, last, nW)

      {:eof, nW} ->
        merge_files(fs, l, lSz, last, nW)
    end
  end

  defp read_more(iFun, i, lSz, w) do
    case iFun.({{:merge, i}, [], lSz, w}) do
      {{_, [], nLSz}, nIFun} ->
        read_more(nIFun, i, nLSz, w)

      {{_, l, nLSz}, nInFun} ->
        nW =
          case :lists.member(iFun, r_w(w, :temp)) do
            true ->
              r_w(w, temp: [nInFun | :lists.delete(iFun, r_w(w, :temp))])

            false ->
              w
          end

        {:lists.reverse(l), nLSz, nInFun, nW}

      :eof ->
        nW = r_w(w, temp: :lists.delete(iFun, r_w(w, :temp)))
        {:eof, nW}
    end
  end

  defp read_fun(fileName, owner, w) do
    case :file.open(
           fileName,
           [:raw, :binary, :read, :compressed]
         ) do
      {:ok, fd} ->
        read_fun2(fd, <<>>, 0, fileName, owner)

      error ->
        file_error(fileName, error, w)
    end
  end

  defp read_fun2(fd, bin, size, fileName, owner) do
    fn
      :close ->
        close_read_fun(fd, fileName, owner)

      {i, l, lSz, w} ->
        case read_objs(fd, fileName, i, l, bin, size, lSz, w) do
          {{i1, l1, bin1, size1}, lSz1} ->
            nIFun = read_fun2(fd, bin1, size1, fileName, owner)
            {{i1, l1, lSz1}, nIFun}

          :eof ->
            close_read_fun(fd, fileName, owner)
            :eof
        end
    end
  end

  defp close_read_fun(fd, _FileName, :user) do
    _ = :file.close(fd)
    :ok
  end

  defp close_read_fun(fd, fileName, :fsort) do
    _ = :file.close(fd)
    _ = :file.delete(fileName)
    :ok
  end

  defp read_objs(fd, fileName, i, l, bin0, size0, lSz, w) do
    max = :erlang.max(size0, 16384)
    bSz0 = byte_size(bin0)
    min = size0 - bSz0 + r_w(w, :hdlen)
    noBytes = :erlang.max(min, max)

    case read(fd, fileName, noBytes, w) do
      {:ok, bin} ->
        bSz = byte_size(bin)
        nLSz = lSz + bSz

        case (try do
                file_loop(l, i, bin0, bin, size0, bSz0, bSz, min, w)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _R} ->
            :erlang.error({:error, {:bad_object, fileName}}, w)

          reply ->
            {reply, nLSz}
        end

      :eof when byte_size(bin0) === 0 ->
        :eof

      :eof ->
        :erlang.error({:error, {:premature_eof, fileName}}, w)
    end
  end

  defp file_loop(l, i, _B1, b2, sz, 0, _B2Sz, _Min, w) do
    file_loop(l, i, b2, sz, w)
  end

  defp file_loop(l, i, b1, b2, sz, _B1Sz, b2Sz, min, w)
       when b2Sz > min do
    {b3, b4} = :erlang.split_binary(b2, min)
    {i1, l1, <<>>, sz1} = file_loop(l, i, :erlang.list_to_binary([b1, b3]), sz, w)
    file_loop(l1, i1, b4, sz1, w)
  end

  defp file_loop(l, i, b1, b2, sz, _B1Sz, _B2Sz, _Min, w) do
    file_loop(l, i, :erlang.list_to_binary([b1, b2]), sz, w)
  end

  defp file_loop(l, i, b, sz, w) do
    r_w(keypos: kp, format: format, hdlen: hdLen) = w
    file_loop1(l, i, b, sz, kp, format, hdLen)
  end

  defp file_loop1(l, i, hB, 0, kp, f, hdLen) do
    <<size::size(hdLen)-unit(8), b::binary>> = hB
    file_loop2(l, i, b, size, <<size::size(hdLen)-unit(8)>>, kp, f, hdLen)
  end

  defp file_loop1(l, i, b, sz, kp, f, hdLen) do
    file_loop2(l, i, b, sz, <<sz::size(hdLen)-unit(8)>>, kp, f, hdLen)
  end

  defp file_loop2(l, _I, b, sz, szB, 0, :binary, hdLen) do
    {nL, nB, nSz, nSzB} = file_binloop(l, sz, szB, b, hdLen)

    cond do
      byte_size(nB) === nSz ->
        <<bin::size(nSz)-binary>> = nB
        {0, [{bin, [nSzB | bin]} | nL], <<>>, 0}

      true ->
        {0, nL, nB, nSz}
    end
  end

  defp file_loop2(l, _I, b, sz, szB, 0, fun, hdLen) do
    file_binterm_loop(l, sz, szB, b, fun, hdLen)
  end

  defp file_loop2(l, {:merge, i}, b, sz, szB, kp, fun, hdLen) do
    merge_loop(kp, i, l, sz, szB, b, fun, hdLen)
  end

  defp file_loop2(l, i, b, sz, szB, kp, fun, hdLen)
       when is_integer(i) do
    key_loop(kp, i, l, sz, szB, b, fun, hdLen)
  end

  defp file_binloop(l, size, sizeB, b, hL) do
    case b do
      <<bin::size(size)-binary, nSizeB::size(hL)-binary, r::binary>> ->
        <<nSize::size(hL)-unit(8)>> = nSizeB
        file_binloop([{bin, [sizeB | bin]} | l], nSize, nSizeB, r, hL)

      _ ->
        {l, b, size, sizeB}
    end
  end

  defp file_binterm_loop(l, size, sizeB, b, fun, hL) do
    case b do
      <<binTerm::size(size)-binary, nSizeB::size(hL)-binary, r::binary>> ->
        <<nSize::size(hL)-unit(8)>> = nSizeB
        bT = [sizeB | binTerm]
        term = fun.(binTerm)
        file_binterm_loop([{term, bT} | l], nSize, nSizeB, r, fun, hL)

      <<binTerm::size(size)-binary>> ->
        term = fun.(binTerm)
        nL = [{term, [sizeB | binTerm]} | l]
        {0, nL, <<>>, 0}

      _ ->
        {0, l, b, size}
    end
  end

  defp key_loop(keyPos, i, l, size, sizeB, b, fun, hL) do
    case b do
      <<binTerm::size(size)-binary, nSizeB::size(hL)-binary, r::binary>> ->
        <<nSize::size(hL)-unit(8)>> = nSizeB
        bT = [sizeB | binTerm]
        uniqueKey = make_key(keyPos, fun.(binTerm))
        e = {uniqueKey, bT}
        key_loop(keyPos, i + 1, [e | l], nSize, nSizeB, r, fun, hL)

      <<binTerm::size(size)-binary>> ->
        uniqueKey = make_key(keyPos, fun.(binTerm))
        nL = [{uniqueKey, [sizeB | binTerm]} | l]
        {i + 1, nL, <<>>, 0}

      _ ->
        {i, l, b, size}
    end
  end

  defp merge_loop(keyPos, i, l, size, sizeB, b, fun, hL) do
    case b do
      <<binTerm::size(size)-binary, nSizeB::size(hL)-binary, r::binary>> ->
        <<nSize::size(hL)-unit(8)>> = nSizeB
        bT = [sizeB | binTerm]
        uniqueKey = make_stable_key(keyPos, i, fun.(binTerm))
        e = {uniqueKey, bT}
        merge_loop(keyPos, i, [e | l], nSize, nSizeB, r, fun, hL)

      <<binTerm::size(size)-binary>> ->
        uniqueKey = make_stable_key(keyPos, i, fun.(binTerm))
        nL = [{uniqueKey, [sizeB | binTerm]} | l]
        {{:merge, i}, nL, <<>>, 0}

      _ ->
        {{:merge, i}, l, b, size}
    end
  end

  defp fun_objs(objs, l, lSz, noBytes, i, w) do
    r_w(keypos: keypos, format: format, hdlen: hL) = w

    case (try do
            fun_loop(objs, l, lSz, noBytes, i, keypos, format, hL)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _R} ->
        :erlang.error({:error, :bad_object}, w)

      reply ->
        reply
    end
  end

  defp fun_loop(objs, l, lSz, runSize, _I, 0, :binary, hdLen) do
    fun_binloop(objs, l, lSz, runSize, hdLen)
  end

  defp fun_loop(objs, l, lSz, runSize, _I, 0, fun, hdLen) do
    fun_loop(objs, l, lSz, runSize, fun, hdLen)
  end

  defp fun_loop(objs, l, lSz, runSize, {:merge, i}, keypos, fun, hdLen) do
    fun_mergeloop(objs, l, lSz, runSize, i, keypos, fun, hdLen)
  end

  defp fun_loop(objs, l, lSz, runSize, i, keypos, fun, hdLen)
       when is_integer(i) do
    fun_keyloop(objs, l, lSz, runSize, i, keypos, fun, hdLen)
  end

  defp fun_binloop([b | bs], l, lSz, runSize, hL)
       when lSz < runSize do
    size = byte_size(b)
    obj = {b, [<<size::size(hL)-unit(8)>> | b]}
    fun_binloop(bs, [obj | l], lSz + size, runSize, hL)
  end

  defp fun_binloop(bs, l, lSz, _RunSize, _HL) do
    {0, bs, l, lSz}
  end

  defp fun_loop([b | bs], l, lSz, runSize, fun, hL)
       when lSz < runSize do
    size = byte_size(b)
    obj = {fun.(b), [<<size::size(hL)-unit(8)>> | b]}
    fun_loop(bs, [obj | l], lSz + size, runSize, fun, hL)
  end

  defp fun_loop(bs, l, lSz, _RunSize, _Fun, _HL) do
    {0, bs, l, lSz}
  end

  defp fun_keyloop([b | bs], l, lSz, runSize, i, kp, fun, hL)
       when lSz < runSize do
    size = byte_size(b)
    uniqueKey = make_key(kp, fun.(b))
    e = {uniqueKey, [<<size::size(hL)-unit(8)>> | b]}
    fun_keyloop(bs, [e | l], lSz + size, runSize, i + 1, kp, fun, hL)
  end

  defp fun_keyloop(bs, l, lSz, _RunSize, i, _Kp, _Fun, _HL) do
    {i, bs, l, lSz}
  end

  defp fun_mergeloop([b | bs], l, lSz, runSize, i, kp, fun, hL)
       when lSz < runSize do
    size = byte_size(b)
    uniqueKey = make_stable_key(kp, i, fun.(b))
    e = {uniqueKey, [<<size::size(hL)-unit(8)>> | b]}
    fun_mergeloop(bs, [e | l], lSz + size, runSize, i, kp, fun, hL)
  end

  defp fun_mergeloop(bs, l, lSz, _RunSize, i, _Kp, _Fun, _HL) do
    {{:merge, i}, bs, l, lSz}
  end

  defp make_key(kp, t) when is_integer(kp) do
    :erlang.element(kp, t)
  end

  defp make_key([kp1, kp2], t) do
    [:erlang.element(kp1, t), :erlang.element(kp2, t)]
  end

  defp make_key([[kp1, kp2] | kps], t) do
    [
      [:erlang.element(kp1, t), :erlang.element(kp2, t)]
      | make_key2(kps, t)
    ]
  end

  defp make_stable_key(kp, i, t) when is_integer(kp) do
    [:erlang.element(kp, t) | i]
  end

  defp make_stable_key([kp1, kp2], i, t) do
    [
      [:erlang.element(kp1, t) | :erlang.element(kp2, t)]
      | i
    ]
  end

  defp make_stable_key([[kp1, kp2] | kps], i, t) do
    [
      [
        [:erlang.element(kp1, t), :erlang.element(kp2, t)]
        | make_key2(kps, t)
      ]
      | i
    ]
  end

  defp make_key2([kp], t) do
    [:erlang.element(kp, t)]
  end

  defp make_key2([kp | kps], t) do
    [:erlang.element(kp, t) | make_key2(kps, t)]
  end

  defp infun(w) do
    w1 = r_w(w, in: :undefined)

    try do
      r_w(w, :in).(:read)
    catch
      class, reason ->
        cleanup(w1)
        :erlang.raise(class, reason, __STACKTRACE__)
    else
      :end_of_input ->
        {:end_of_input, w1}

      {:end_of_input, value} ->
        {:end_of_input, r_w(w1, inout_value: {:value, value})}

      {objs, nFun}
      when is_function(nFun, 1) and
             is_list(objs) ->
        {:cont, r_w(w, in: nFun), objs}

      error ->
        :erlang.error(error, w1)
    end
  end

  defp outfun(a, r_w(inout_value: val) = w)
       when val !== :no_value do
    w1 = r_w(w, inout_value: :no_value)

    w2 =
      cond do
        r_w(w1, :fun_out) ->
          outfun(val, w1)

        true ->
          w1
      end

    outfun(a, w2)
  end

  defp outfun(a, w) do
    w1 = r_w(w, out: :undefined)

    try do
      r_w(w, :out).(a)
    catch
      class, reason ->
        cleanup(w1)
        :erlang.raise(class, reason, __STACKTRACE__)
    else
      reply when a === :close ->
        reply

      nF when is_function(nF, 1) ->
        r_w(w, out: nF)

      error ->
        :erlang.error(error, w1)
    end
  end

  defp is_keypos(keypos)
       when is_integer(keypos) and
              keypos > 0 do
    true
  end

  defp is_keypos([]) do
    {:badarg, []}
  end

  defp is_keypos(l) do
    is_keyposs(l)
  end

  defp is_keyposs([kp | kps]) when is_integer(kp) and kp > 0 do
    is_keyposs(kps)
  end

  defp is_keyposs([]) do
    true
  end

  defp is_keyposs([bad | _]) do
    {:badarg, bad}
  end

  defp is_keyposs(bad) do
    {:badarg, bad}
  end

  defp is_input(fun) when is_function(fun, 1) do
    {true, fun}
  end

  defp is_input(files) do
    is_files(files)
  end

  defp is_files(fs) do
    is_files(fs, [])
  end

  defp is_files([f | fs], l) do
    case read_file_info(f) do
      {:ok, file, _FI} ->
        is_files(fs, [file | l])

      error ->
        error
    end
  end

  defp is_files([], l) do
    {true, :lists.reverse(l)}
  end

  defp is_files(bad, _L) do
    {:badarg, bad}
  end

  defp maybe_output(fun) when is_function(fun, 1) do
    {true, fun}
  end

  defp maybe_output(file) do
    case read_file_info(file) do
      {:badarg, _File} = badarg ->
        badarg

      {:ok, fileName, _FileInfo} ->
        {true, fileName}

      {:error, {:file_error, fileName, _Reason}} ->
        {true, fileName}
    end
  end

  defp read_file_info(file) do
    case (try do
            :filename.absname(file)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        {:badarg, file}

      fileName ->
        case :file.read_file_info(fileName) do
          {:ok, fileInfo} ->
            {:ok, fileName, fileInfo}

          {:error, :einval} ->
            {:badarg, file}

          {:error, reason} ->
            {:error, {:file_error, fileName, reason}}
        end
    end
  end

  defp next_temp(w) do
    seq = r_w(w, :seq)
    nW = r_w(w, seq: seq + 1)
    temp = :lists.concat([r_w(w, :prefix), seq])
    {nW, temp}
  end

  defp tmp_prefix(f, tmpDirOpt)
       when is_function(f) or
              f === :undefined do
    {:ok, curDir} = :file.get_cwd()
    tmp_prefix1(curDir, tmpDirOpt)
  end

  defp tmp_prefix(outFile, tmpDirOpt) do
    dir = :filename.dirname(outFile)
    tmp_prefix1(dir, tmpDirOpt)
  end

  defp tmp_prefix1(dir, tmpDirOpt) do
    u = '_'
    node = node()
    pid = :os.getpid()
    unique = :erlang.unique_integer([:positive])
    f = :lists.concat(['fs_', node, u, pid, u, unique, '.'])

    tmpDir =
      case tmpDirOpt do
        :default ->
          dir

        {:dir, tDir} ->
          tDir
      end

    :filename.join(:filename.absname(tmpDir), f)
  end

  defp open_file(fileName, w) do
    case :file.open(
           fileName,
           r_w(w, :z) ++ [:raw, :binary, :write]
         ) do
      {:ok, fd} ->
        {fd, r_w(w, temp: [{fd, fileName} | r_w(w, :temp)])}

      error ->
        file_error(fileName, error, w)
    end
  end

  defp read(fd, fileName, n, w) do
    case :file.read(fd, n) do
      {:ok, bin} ->
        {:ok, bin}

      :eof ->
        :eof

      {:error, :enomem} ->
        :erlang.error({:error, {:bad_object, fileName}}, w)

      {:error, :einval} ->
        :erlang.error({:error, {:bad_object, fileName}}, w)

      error ->
        file_error(fileName, error, w)
    end
  end

  defp write(fd, fileName, b, w) do
    case :file.write(fd, b) do
      :ok ->
        :ok

      error ->
        file_error(fileName, error, w)
    end
  end

  defp file_error(file, {:error, reason}, w) do
    :erlang.error({:error, {:file_error, file, reason}}, w)
  end

  defp error(error, w) do
    cleanup(w)
    throw({r_w(w, :ref), error})
  end

  defp cleanup(w) do
    close_out(w)
    w1 = close_input(w)

    f = fn
      iFun when is_function(iFun) ->
        iFun.(:close)

      {fd, fileName} ->
        _ = :file.close(fd)
        _ = :file.delete(fileName)

      fileName ->
        _ = :file.delete(fileName)
    end

    :lists.foreach(f, r_w(w1, :temp))
  end

  defp close_input(r_w(in: in__) = w) when is_function(in__) do
    try do
      in__.(:close)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    r_w(w, in: :undefined)
  end

  defp close_input(r_w(in: :undefined) = w) do
    w
  end

  defp close_out(r_w(out: out)) when is_function(out) do
    try do
      out.(:close)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp close_out(_) do
    :ok
  end

  defp close_file(fd, w) do
    {^fd, fileName} = :lists.keyfind(fd, 1, r_w(w, :temp))
    :ok

    case :file.close(fd) do
      :ok ->
        r_w(w, temp: [fileName | :lists.keydelete(fd, 1, r_w(w, :temp))])

      error ->
        file_error(fileName, error, w)
    end
  end

  defp file_rterms(:no_file, files) do
    fn
      :close ->
        :ok

      :read when files === [] ->
        :end_of_input

      :read ->
        [f | fs] = files

        case :file.open(f, [:read, :compressed]) do
          {:ok, fd} ->
            file_rterms2(fd, [], 0, f, fs)

          {:error, reason} ->
            {:error, {:file_error, f, reason}}
        end
    end
  end

  defp file_rterms({fd, fileName}, files) do
    fn
      :close ->
        :file.close(fd)

      :read ->
        file_rterms2(fd, [], 0, fileName, files)
    end
  end

  defp file_rterms2(fd, l, lSz, fileName, files) when lSz < 16384 do
    case :io.read(fd, :"") do
      {:ok, term} ->
        b = :erlang.term_to_binary(term)
        file_rterms2(fd, [b | l], lSz + byte_size(b), fileName, files)

      :eof ->
        _ = :file.close(fd)
        {:lists.reverse(l), file_rterms(:no_file, files)}

      _Error ->
        _ = :file.close(fd)
        {:error, {:bad_term, fileName}}
    end
  end

  defp file_rterms2(fd, l, _LSz, fileName, files) do
    {:lists.reverse(l), file_rterms({fd, fileName}, files)}
  end

  defp file_wterms(w, f, args) do
    fn
      :close when w === :name ->
        :ok

      :close ->
        {:fd, fd} = w
        :file.close(fd)

      l when w === :name ->
        case :file.open(f, args) do
          {:ok, fd} ->
            write_terms(fd, f, l, args)

          {:error, reason} ->
            {:error, {:file_error, f, reason}}
        end

      l ->
        {:fd, fd} = w
        write_terms(fd, f, l, args)
    end
  end

  defp write_terms(fd, f, [b | bs], args) do
    case :io.request(
           fd,
           {:format, '~p.~n', [:erlang.binary_to_term(b)]}
         ) do
      :ok ->
        write_terms(fd, f, bs, args)

      {:error, reason} ->
        _ = :file.close(fd)
        {:error, {:file_error, f, reason}}
    end
  end

  defp write_terms(fd, f, [], args) do
    file_wterms({:fd, fd}, f, args)
  end

  defp fun_rterms(inFun) do
    fn
      :close ->
        inFun.(:close)

      :read ->
        case inFun.(:read) do
          {ts, nInFun}
          when is_list(ts) and
                 is_function(nInFun, 1) ->
            {to_bin(ts, []), fun_rterms(nInFun)}

          else__ ->
            else__
        end
    end
  end

  defp fun_wterms(outFun) do
    fn
      :close ->
        outFun.(:close)

      l ->
        case outFun.(wterms_arg(l)) do
          nOutFun when is_function(nOutFun, 1) ->
            fun_wterms(nOutFun)

          else__ ->
            else__
        end
    end
  end

  defp to_bin([e | es], l) do
    to_bin(es, [:erlang.term_to_binary(e) | l])
  end

  defp to_bin([], l) do
    :lists.reverse(l)
  end

  defp wterms_arg(l) when is_list(l) do
    to_term(l, [])
  end

  defp wterms_arg(value) do
    value
  end

  defp to_term([b | bs], l) do
    to_term(bs, [:erlang.binary_to_term(b) | l])
  end

  defp to_term([], l) do
    :lists.reverse(l)
  end
end
