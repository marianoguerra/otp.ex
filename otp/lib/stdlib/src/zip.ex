defmodule :m_zip do
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

  Record.defrecord(:r_zip_file, :zip_file,
    name: :undefined,
    info: :undefined,
    comment: :undefined,
    offset: :undefined,
    comp_size: :undefined
  )

  Record.defrecord(:r_zip_comment, :zip_comment, comment: :undefined)

  Record.defrecord(:r_unzip_opts, :unzip_opts,
    output: :undefined,
    input: :undefined,
    file_filter: :undefined,
    open_opts: :undefined,
    feedback: :undefined,
    cwd: :undefined
  )

  Record.defrecord(:r_zip_opts, :zip_opts,
    output: :undefined,
    input: :undefined,
    comment: :undefined,
    open_opts: :undefined,
    feedback: :undefined,
    cwd: :undefined,
    compress: :undefined,
    uncompress: :undefined
  )

  Record.defrecord(:r_list_dir_opts, :list_dir_opts,
    input: :undefined,
    raw_iterator: :undefined,
    open_opts: :undefined
  )

  Record.defrecord(:r_openzip_opts, :openzip_opts,
    output: :undefined,
    open_opts: :undefined,
    cwd: :undefined
  )

  Record.defrecord(:r_openzip, :openzip,
    zip_comment: :undefined,
    files: :undefined,
    in: :undefined,
    input: :undefined,
    output: :undefined,
    zlib: :undefined,
    cwd: :undefined
  )

  Record.defrecord(:r_zip_file_extra, :zip_file_extra, crc32: :undefined)

  Record.defrecord(:r_local_file_header, :local_file_header,
    version_needed: :undefined,
    gp_flag: :undefined,
    comp_method: :undefined,
    last_mod_time: :undefined,
    last_mod_date: :undefined,
    crc32: :undefined,
    comp_size: :undefined,
    uncomp_size: :undefined,
    file_name_length: :undefined,
    extra_field_length: :undefined
  )

  Record.defrecord(:r_cd_file_header, :cd_file_header,
    version_made_by: :undefined,
    version_needed: :undefined,
    gp_flag: :undefined,
    comp_method: :undefined,
    last_mod_time: :undefined,
    last_mod_date: :undefined,
    crc32: :undefined,
    comp_size: :undefined,
    uncomp_size: :undefined,
    file_name_length: :undefined,
    extra_field_length: :undefined,
    file_comment_length: :undefined,
    disk_num_start: :undefined,
    internal_attr: :undefined,
    external_attr: :undefined,
    local_header_offset: :undefined
  )

  Record.defrecord(:r_eocd, :eocd,
    disk_num: :undefined,
    start_disk_num: :undefined,
    entries_on_disk: :undefined,
    entries: :undefined,
    size: :undefined,
    offset: :undefined,
    zip_comment_length: :undefined
  )

  def openzip_open(f) do
    openzip_open(f, [])
  end

  def openzip_open(f, options) do
    case (try do
            do_openzip_open(f, options)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, openZip} ->
        {:ok, openZip}

      error ->
        {:error, error}
    end
  end

  defp do_openzip_open(f, options) do
    opts = get_openzip_options(options)
    r_openzip_opts(output: output, open_opts: opO, cwd: cWD) = opts
    input = get_input(f)
    in0 = input.({:open, f, opO -- [:write]}, [])

    {[r_zip_comment(comment: c) | files], in1} =
      get_central_dir(
        in0,
        &raw_file_info_etc/5,
        input
      )

    z = :zlib.open()

    {:ok,
     r_openzip(
       zip_comment: c,
       files: files,
       in: in1,
       input: input,
       output: output,
       zlib: z,
       cwd: cWD
     )}
  end

  def openzip_get(openZip) do
    case (try do
            do_openzip_get(openZip)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, result} ->
        {:ok, result}

      error ->
        {:error, error}
    end
  end

  defp do_openzip_get(
         r_openzip(files: files, in: in0, input: input, output: output, zlib: z, cwd: cWD)
       ) do
    zipOpts =
      r_unzip_opts(
        output: output,
        input: input,
        file_filter: &all/1,
        open_opts: [],
        feedback: &silent/1,
        cwd: cWD
      )

    r = get_z_files(files, z, in0, zipOpts, [])
    {:ok, r}
  end

  defp do_openzip_get(_) do
    throw(:einval)
  end

  def openzip_get(fileName, openZip) do
    case (try do
            do_openzip_get(fileName, openZip)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, result} ->
        {:ok, result}

      error ->
        {:error, error}
    end
  end

  defp do_openzip_get(
         f,
         r_openzip(files: files, in: in0, input: input, output: output, zlib: z, cwd: cWD)
       ) do
    case file_name_search(f, files) do
      {r_zip_file(offset: offset), _} = zFile ->
        in1 = input.({:seek, :bof, offset}, in0)

        case get_z_file(in1, z, input, output, [], &silent/1, cWD, zFile, &all/1) do
          {:file, r, _In2} ->
            {:ok, r}

          _ ->
            throw(:file_not_found)
        end

      _ ->
        throw(:file_not_found)
    end
  end

  defp do_openzip_get(_, _) do
    throw(:einval)
  end

  defp file_name_search(name, files) do
    fun = fn {zipFile, _} ->
      not :string.equal(r_zip_file(zipFile, :name), name, _IgnoreCase = false, _Norm = :nfc)
    end

    case :lists.dropwhile(fun, files) do
      [zFile | _] ->
        zFile

      [] ->
        false
    end
  end

  def openzip_list_dir(r_openzip(zip_comment: comment, files: files)) do
    {zipFiles, _Extras} = :lists.unzip(files)
    {:ok, [r_zip_comment(comment: comment) | zipFiles]}
  end

  def openzip_list_dir(_) do
    {:error, :einval}
  end

  def openzip_list_dir(r_openzip(files: files), [:names_only]) do
    {zipFiles, _Extras} = :lists.unzip(files)

    names =
      for {r_zip_file(name: name), _} <- zipFiles do
        name
      end

    {:ok, names}
  end

  def openzip_list_dir(_, _) do
    {:error, :einval}
  end

  def openzip_close(r_openzip(in: in0, input: input, zlib: z)) do
    input.(:close, in0)
    :zlib.close(z)
  end

  def openzip_close(_) do
    {:error, :einval}
  end

  def unzip(f) do
    unzip(f, [])
  end

  def unzip(f, options) do
    case (try do
            do_unzip(f, options)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, r} ->
        {:ok, r}

      error ->
        {:error, error}
    end
  end

  defp do_unzip(f, options) do
    opts = get_unzip_options(f, options)
    r_unzip_opts(input: input, open_opts: opO) = opts
    in0 = input.({:open, f, opO -- [:write]}, [])
    rawIterator = &raw_file_info_etc/5
    {info, in1} = get_central_dir(in0, rawIterator, input)
    z = :zlib.open()

    files =
      try do
        get_z_files(info, z, in1, opts, [])
      after
        :zlib.close(z)
        input.(:close, in1)
      end

    {:ok, files}
  end

  def foldl(fun, acc0, archive) when is_function(fun, 4) do
    zipFun = fn {name, getInfo, getBin}, a ->
      a2 = fun.(name, getInfo, getBin, a)
      {true, false, a2}
    end

    case :prim_zip.open(zipFun, acc0, archive) do
      {:ok, primZip, acc1} ->
        :ok = :prim_zip.close(primZip)
        {:ok, acc1}

      {:error, :bad_eocd} ->
        {:error, 'Not an archive file'}

      {:error, reason} ->
        {:error, reason}
    end
  end

  def foldl(_, _, _) do
    {:error, :einval}
  end

  def zip(f, files) do
    zip(f, files, [])
  end

  def zip(f, files, options) do
    case (try do
            do_zip(f, files, options)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, r} ->
        {:ok, r}

      error ->
        {:error, error}
    end
  end

  defp do_zip(f, files, options) do
    opts = get_zip_options(files, options)
    r_zip_opts(output: output, open_opts: opO) = opts
    out0 = output.({:open, f, opO}, [])
    z = :zlib.open()

    try do
      {out1, lHS, pos} = put_z_files(files, z, out0, 0, opts, [])
      :zlib.close(z)
      out2 = put_central_dir(lHS, pos, out1, opts)
      out3 = output.({:close, f}, out2)
      {:ok, out3}
    catch
      c, r ->
        :zlib.close(z)
        output.({:close, f}, out0)
        :erlang.raise(c, r, __STACKTRACE__)
    end
  end

  def list_dir(f) do
    list_dir(f, [])
  end

  def list_dir(f, options) do
    case (try do
            do_list_dir(f, options)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, r} ->
        {:ok, r}

      error ->
        {:error, error}
    end
  end

  defp do_list_dir(f, options) do
    opts = get_list_dir_options(f, options)
    r_list_dir_opts(input: input, open_opts: opO, raw_iterator: rawIterator) = opts
    in0 = input.({:open, f, opO}, [])
    {info, in1} = get_central_dir(in0, rawIterator, input)
    input.(:close, in1)
    {:ok, info}
  end

  def t(f) when is_pid(f) do
    zip_t(f)
  end

  def t(f) when elem(f, 0) === :openzip do
    openzip_t(f)
  end

  def t(f) do
    t(f, &raw_short_print_info_etc/5)
  end

  defp t(f, rawPrint) do
    case (try do
            do_t(f, rawPrint)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :ok ->
        :ok

      error ->
        {:error, error}
    end
  end

  defp do_t(f, rawPrint) do
    input = get_input(f)
    opO = [:raw]
    in0 = input.({:open, f, opO}, [])
    {_Info, in1} = get_central_dir(in0, rawPrint, input)
    input.(:close, in1)
    :ok
  end

  def tt(f) when is_pid(f) do
    zip_tt(f)
  end

  def tt(f) when elem(f, 0) === :openzip do
    openzip_tt(f)
  end

  def tt(f) do
    t(f, &raw_long_print_info_etc/5)
  end

  defp get_unzip_opt([], opts) do
    opts
  end

  defp get_unzip_opt([:verbose | rest], opts) do
    get_unzip_opt(rest, r_unzip_opts(opts, feedback: &verbose_unzip/1))
  end

  defp get_unzip_opt([:cooked | rest], r_unzip_opts(open_opts: opO) = opts) do
    get_unzip_opt(rest, r_unzip_opts(opts, open_opts: opO -- [:raw]))
  end

  defp get_unzip_opt([:memory | rest], opts) do
    get_unzip_opt(rest, r_unzip_opts(opts, output: &binary_io/2))
  end

  defp get_unzip_opt([{:cwd, cWD} | rest], opts) do
    get_unzip_opt(rest, r_unzip_opts(opts, cwd: cWD))
  end

  defp get_unzip_opt([{:file_filter, f} | rest], opts) do
    filter1 = fn {zipFile, _Extra} ->
      f.(zipFile)
    end

    filter2 = fun_and_1(filter1, r_unzip_opts(opts, :file_filter))
    get_unzip_opt(rest, r_unzip_opts(opts, file_filter: filter2))
  end

  defp get_unzip_opt([{:file_list, l} | rest], opts) do
    fileInList = fn f ->
      file_in_list(f, l)
    end

    filter = fun_and_1(fileInList, r_unzip_opts(opts, :file_filter))
    get_unzip_opt(rest, r_unzip_opts(opts, file_filter: filter))
  end

  defp get_unzip_opt([:keep_old_files | rest], opts) do
    keep = &keep_old_file/1
    filter = fun_and_1(keep, r_unzip_opts(opts, :file_filter))
    get_unzip_opt(rest, r_unzip_opts(opts, file_filter: filter))
  end

  defp get_unzip_opt([unknown | _Rest], _Opts) do
    throw({:bad_option, unknown})
  end

  defp get_list_dir_opt([], opts) do
    opts
  end

  defp get_list_dir_opt([:cooked | rest], r_list_dir_opts(open_opts: opO) = opts) do
    get_list_dir_opt(
      rest,
      r_list_dir_opts(opts, open_opts: opO -- [:raw])
    )
  end

  defp get_list_dir_opt([:names_only | rest], opts) do
    get_list_dir_opt(
      rest,
      r_list_dir_opts(opts,
        raw_iterator: fn a, b, c, d, e ->
          raw_name_only(a, b, c, d, e)
        end
      )
    )
  end

  defp get_list_dir_opt([unknown | _Rest], _Opts) do
    throw({:bad_option, unknown})
  end

  defp get_zip_opt([], opts) do
    opts
  end

  defp get_zip_opt([:verbose | rest], opts) do
    get_zip_opt(rest, r_zip_opts(opts, feedback: &verbose_zip/1))
  end

  defp get_zip_opt([:cooked | rest], r_zip_opts(open_opts: opO) = opts) do
    get_zip_opt(rest, r_zip_opts(opts, open_opts: opO -- [:raw]))
  end

  defp get_zip_opt([:memory | rest], opts) do
    get_zip_opt(rest, r_zip_opts(opts, output: &binary_io/2))
  end

  defp get_zip_opt([{:cwd, cWD} | rest], opts) do
    get_zip_opt(rest, r_zip_opts(opts, cwd: cWD))
  end

  defp get_zip_opt([{:comment, c} | rest], opts) do
    get_zip_opt(rest, r_zip_opts(opts, comment: c))
  end

  defp get_zip_opt([{:compress, which} = o | rest], opts) do
    which2 =
      case which do
        :all ->
          :all

        suffixes when is_list(suffixes) ->
          :lists.usort(suffixes)

        {:add, suffixes} when is_list(suffixes) ->
          :lists.usort(r_zip_opts(opts, :compress) ++ suffixes)

        {:del, suffixes} when is_list(suffixes) ->
          :lists.usort(r_zip_opts(opts, :compress) -- suffixes)

        _ ->
          throw({:bad_option, o})
      end

    get_zip_opt(rest, r_zip_opts(opts, compress: which2))
  end

  defp get_zip_opt([{:uncompress, which} = o | rest], opts) do
    which2 =
      case which do
        :all ->
          :all

        suffixes when is_list(suffixes) ->
          :lists.usort(suffixes)

        {:add, suffixes} when is_list(suffixes) ->
          :lists.usort(r_zip_opts(opts, :uncompress) ++ suffixes)

        {:del, suffixes} when is_list(suffixes) ->
          :lists.usort(r_zip_opts(opts, :uncompress) -- suffixes)

        _ ->
          throw({:bad_option, o})
      end

    get_zip_opt(rest, r_zip_opts(opts, uncompress: which2))
  end

  defp get_zip_opt([unknown | _Rest], _Opts) do
    throw({:bad_option, unknown})
  end

  defp silent(_) do
    :ok
  end

  defp verbose_unzip(fN) do
    :io.format('extracting: ~ts\n', [:io_lib.write_string(fN)])
  end

  defp verbose_zip(fN) do
    :io.format('adding: ~ts\n', [:io_lib.write_string(fN)])
  end

  defp all(_) do
    true
  end

  defp file_in_list({r_zip_file(name: fileName), _}, list) do
    :lists.member(fileName, list)
  end

  defp file_in_list(_, _) do
    false
  end

  defp keep_old_file({r_zip_file(name: fileName), _}) do
    not (:filelib.is_file(fileName) or :filelib.is_dir(fileName))
  end

  defp keep_old_file(_) do
    false
  end

  defp fun_and_1(fun1, fun2) do
    fn a ->
      fun1.(a) and fun2.(a)
    end
  end

  defp get_zip_options(files, options) do
    suffixes = ['.Z', '.zip', '.zoo', '.arc', '.lzh', '.arj']

    opts =
      r_zip_opts(
        output: &file_io/2,
        input: get_zip_input({:files, files}),
        open_opts: [:raw, :write],
        comment: '',
        feedback: &silent/1,
        cwd: '',
        compress: :all,
        uncompress: suffixes
      )

    opts1 =
      r_zip_opts(comment: comment) =
      get_zip_opt(
        options,
        opts
      )

    {comment1, _} = encode_string(comment)
    r_zip_opts(opts1, comment: comment1)
  end

  defp get_unzip_options(f, options) do
    opts =
      r_unzip_opts(
        file_filter: &all/1,
        output: &file_io/2,
        input: get_input(f),
        open_opts: [:raw],
        feedback: &silent/1,
        cwd: ''
      )

    get_unzip_opt(options, opts)
  end

  defp get_openzip_options(options) do
    opts = r_openzip_opts(open_opts: [:raw, :read], output: &file_io/2, cwd: '')
    get_openzip_opt(options, opts)
  end

  defp get_input(f) when is_binary(f) do
    &binary_io/2
  end

  defp get_input(f) when is_list(f) do
    &file_io/2
  end

  defp get_input(_) do
    throw(:einval)
  end

  defp get_zip_input({f, b}) when is_binary(b) and is_list(f) do
    &binary_io/2
  end

  defp get_zip_input({f, b, r_file_info()})
       when is_binary(b) and
              is_list(f) do
    &binary_io/2
  end

  defp get_zip_input({f, r_file_info(), b})
       when is_binary(b) and
              is_list(f) do
    &binary_io/2
  end

  defp get_zip_input(f) when is_list(f) do
    &file_io/2
  end

  defp get_zip_input({:files, []}) do
    &binary_io/2
  end

  defp get_zip_input({:files, [file | _]}) do
    get_zip_input(file)
  end

  defp get_zip_input(_) do
    throw(:einval)
  end

  defp get_list_dir_options(f, options) do
    opts =
      r_list_dir_opts(
        raw_iterator: &raw_file_info_public/5,
        input: get_input(f),
        open_opts: [:raw]
      )

    get_list_dir_opt(options, opts)
  end

  def table(f) do
    list_dir(f)
  end

  def table(f, o) do
    list_dir(f, o)
  end

  def create(f, fs) do
    zip(f, fs)
  end

  def create(f, fs, o) do
    zip(f, fs, o)
  end

  def extract(f) do
    unzip(f)
  end

  def extract(f, o) do
    unzip(f, o)
  end

  defp put_central_dir(lHS, pos, out0, r_zip_opts(output: output, comment: comment)) do
    {out1, sz} = put_cd_files_loop(lHS, output, out0, 0)
    put_eocd(length(lHS), pos, sz, comment, output, out1)
  end

  defp put_cd_files_loop([], _Output, out, sz) do
    {out, sz}
  end

  defp put_cd_files_loop([{lH, name, pos} | lHRest], output, out0, sz0) do
    cDFH = cd_file_header_from_lh_and_pos(lH, pos)
    bCDFH = cd_file_header_to_bin(cDFH)
    b = [<<33_639_248::size(32)-little>>, bCDFH, name]
    out1 = output.({:write, b}, out0)

    sz1 =
      sz0 + (4 + 2 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4) +
        r_local_file_header(lH, :file_name_length)

    put_cd_files_loop(lHRest, output, out1, sz1)
  end

  defp put_eocd(n, pos, sz, comment, output, out0) do
    commentSz = length(comment)

    eOCD =
      r_eocd(
        disk_num: 0,
        start_disk_num: 0,
        entries_on_disk: n,
        entries: n,
        size: sz,
        offset: pos,
        zip_comment_length: commentSz
      )

    bEOCD = eocd_to_bin(eOCD)
    b = [<<101_010_256::size(32)-little>>, bEOCD, comment]
    output.({:write, b}, out0)
  end

  defp get_filename({name, _}, type) do
    get_filename(name, type)
  end

  defp get_filename({name, _, _}, type) do
    get_filename(name, type)
  end

  defp get_filename(name, :regular) do
    name
  end

  defp get_filename(name, :directory) do
    case :lists.reverse(name) do
      [?/ | _Rev] ->
        name

      rev ->
        :lists.reverse([?/ | rev])
    end
  end

  defp add_cwd(_CWD, {_Name, _} = f) do
    f
  end

  defp add_cwd('', f) do
    f
  end

  defp add_cwd(cWD, f) do
    :filename.join(cWD, f)
  end

  defp get_comp_method(_, n, _, _) when is_integer(n) and n < 10 do
    0
  end

  defp get_comp_method(_, _, _, :directory) do
    0
  end

  defp get_comp_method(f, _, r_zip_opts(compress: compress, uncompress: uncompress), _) do
    ext = :filename.extension(f)

    test = fn which ->
      which === :all or :lists.member(ext, which)
    end

    case test.(compress) and not test.(uncompress) do
      true ->
        8

      false ->
        0
    end
  end

  defp put_z_files([], _Z, out, pos, _Opts, acc) do
    {out, :lists.reverse(acc, []), pos}
  end

  defp put_z_files(
         [f | rest],
         z,
         out0,
         pos0,
         r_zip_opts(input: input, output: output, open_opts: opO, feedback: fB, cwd: cWD) = opts,
         acc
       ) do
    in0 = []
    f1 = add_cwd(cWD, f)
    fileInfo = input.({:file_info, f1}, in0)
    type = r_file_info(fileInfo, :type)

    uncompSize =
      case type do
        :regular ->
          r_file_info(fileInfo, :size)

        :directory ->
          0
      end

    fileName0 = get_filename(f, type)
    {fileName, gPFlag} = encode_string(fileName0)
    compMethod = get_comp_method(fileName, uncompSize, opts, type)

    lH =
      local_file_header_from_info_method_name(fileInfo, uncompSize, compMethod, fileName, gPFlag)

    bLH = local_file_header_to_bin(lH)
    b = [<<67_324_752::size(32)-little>>, bLH]
    out1 = output.({:write, b}, out0)
    out2 = output.({:write, fileName}, out1)

    {out3, compSize, cRC} =
      put_z_file(compMethod, uncompSize, out2, f1, 0, input, output, opO, z, type)

    fB.(fileName0)
    patch = <<cRC::size(32)-little, compSize::size(32)-little>>

    out4 =
      output.(
        {:pwrite, pos0 + 4 + 2 + 2 + 2 + 2 + 2, patch},
        out3
      )

    out5 = output.({:seek, :eof, 0}, out4)

    pos1 =
      pos0 + (4 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2) +
        r_local_file_header(lH, :file_name_length)

    pos2 = pos1 + compSize
    lH2 = r_local_file_header(lH, comp_size: compSize, crc32: cRC)
    thisAcc = [{lH2, fileName, pos0}]

    {out6, subAcc, pos3} =
      case type do
        :regular ->
          {out5, thisAcc, pos2}

        :directory ->
          files = input.({:list_dir, f1}, [])
          revFiles = reverse_join_files(f, files, [])
          put_z_files(revFiles, z, out5, pos2, opts, thisAcc)
      end

    acc2 = :lists.reverse(subAcc) ++ acc
    put_z_files(rest, z, out6, pos3, opts, acc2)
  end

  defp reverse_join_files(dir, [file | files], acc) do
    reverse_join_files(dir, files, [:filename.join([dir, file]) | acc])
  end

  defp reverse_join_files(_Dir, [], acc) do
    acc
  end

  defp put_z_file(_Method, sz, out, _F, pos, _Input, _Output, _OpO, _Z, :directory) do
    {out, pos + sz, 0}
  end

  defp put_z_file(_Method, 0, out, _F, pos, _Input, _Output, _OpO, _Z, :regular) do
    {out, pos, 0}
  end

  defp put_z_file(0, uncompSize, out0, f, pos0, input, output, opO, z, :regular) do
    in0 = []
    in1 = input.({:open, f, opO -- [:write]}, in0)
    cRC0 = :zlib.crc32(z, <<>>)
    {data, in2} = input.({:read, uncompSize}, in1)
    out1 = output.({:write, data}, out0)
    cRC = :zlib.crc32(z, cRC0, data)
    input.(:close, in2)
    {out1, pos0 + :erlang.iolist_size(data), cRC}
  end

  defp put_z_file(8, uncompSize, out0, f, pos0, input, output, opO, z, :regular) do
    in0 = []
    in1 = input.({:open, f, opO -- [:write]}, in0)
    :ok = :zlib.deflateInit(z, :default, :deflated, -15, 8, :default)
    {out1, pos1} = put_z_data_loop(uncompSize, in1, out0, pos0, input, output, z)
    cRC = :zlib.crc32(z)
    :ok = :zlib.deflateEnd(z)
    input.(:close, in1)
    {out1, pos1, cRC}
  end

  defp get_sync(n, n) do
    :finish
  end

  defp get_sync(_, _) do
    :full
  end

  defp put_z_data_loop(0, _In, out, pos, _Input, _Output, _Z) do
    {out, pos}
  end

  defp put_z_data_loop(uncompSize, in0, out0, pos0, input, output, z) do
    n = :erlang.min(8 * 1024, uncompSize)

    case input.({:read, n}, in0) do
      {:eof, _In1} ->
        {out0, pos0}

      {uncompressed, in1} ->
        compressed = :zlib.deflate(z, uncompressed, get_sync(n, uncompSize))
        sz = :erlang.iolist_size(compressed)
        out1 = output.({:write, compressed}, out0)
        put_z_data_loop(uncompSize - n, in1, out1, pos0 + sz, input, output, z)
    end
  end

  defp raw_name_only(cD, fileName, _FileComment, _BExtraField, acc)
       when elem(cD, 0) === :cd_file_header do
    [fileName | acc]
  end

  defp raw_name_only(eOCD, _, _Comment, _, acc)
       when elem(eOCD, 0) === :eocd do
    acc
  end

  defp raw_short_print_info_etc(cD, fileName, _FileComment, _BExtraField, acc)
       when elem(cD, 0) === :cd_file_header do
    print_file_name(fileName)
    acc
  end

  defp raw_short_print_info_etc(eOCD, x, comment, y, acc)
       when elem(eOCD, 0) === :eocd do
    raw_long_print_info_etc(eOCD, x, comment, y, acc)
  end

  defp print_file_name(fileName) do
    :io.format('~ts\n', [fileName])
  end

  defp raw_long_print_info_etc(
         r_cd_file_header(
           comp_size: compSize,
           uncomp_size: uncompSize,
           last_mod_date: lMDate,
           last_mod_time: lMTime
         ),
         fileName,
         fileComment,
         _BExtraField,
         acc
       ) do
    mTime = dos_date_time_to_datetime(lMDate, lMTime)
    print_header(compSize, mTime, uncompSize, fileName, fileComment)
    acc
  end

  defp raw_long_print_info_etc(eOCD, _, comment, _, acc)
       when elem(eOCD, 0) === :eocd do
    print_comment(comment)
    acc
  end

  defp print_header(compSize, mTime, uncompSize, fileName, fileComment) do
    :io.format(
      '~8w ~s ~8w ~2w% ~ts ~ts\n',
      [
        compSize,
        time_to_string(mTime),
        uncompSize,
        get_percent(
          compSize,
          uncompSize
        ),
        fileName,
        fileComment
      ]
    )
  end

  defp print_comment('') do
    :ok
  end

  defp print_comment(comment) do
    :io.format('Archive comment: ~ts\n', [comment])
  end

  defp get_percent(_, 0) do
    100
  end

  defp get_percent(compSize, size) do
    round(compSize * 100 / size)
  end

  defp time_to_string({{y, mon, day}, {h, min, _}}) do
    :io_lib.format(
      '~s ~2w ~s:~s ~w',
      [month(mon), day, two_d(h), two_d(min), y]
    )
  end

  defp two_d(n) do
    tl(:erlang.integer_to_list(n + 100))
  end

  defp month(1) do
    'Jan'
  end

  defp month(2) do
    'Feb'
  end

  defp month(3) do
    'Mar'
  end

  defp month(4) do
    'Apr'
  end

  defp month(5) do
    'May'
  end

  defp month(6) do
    'Jun'
  end

  defp month(7) do
    'Jul'
  end

  defp month(8) do
    'Aug'
  end

  defp month(9) do
    'Sep'
  end

  defp month(10) do
    'Oct'
  end

  defp month(11) do
    'Nov'
  end

  defp month(12) do
    'Dec'
  end

  defp cd_file_header_from_lh_and_pos(lH, pos) do
    r_local_file_header(
      version_needed: versionNeeded,
      gp_flag: gPFlag,
      comp_method: compMethod,
      last_mod_time: lastModTime,
      last_mod_date: lastModDate,
      crc32: cRC32,
      comp_size: compSize,
      uncomp_size: uncompSize,
      file_name_length: fileNameLength,
      extra_field_length: extraFieldLength
    ) = lH

    r_cd_file_header(
      version_made_by: 20 ||| 3 <<< 8,
      version_needed: versionNeeded,
      gp_flag: gPFlag,
      comp_method: compMethod,
      last_mod_time: lastModTime,
      last_mod_date: lastModDate,
      crc32: cRC32,
      comp_size: compSize,
      uncomp_size: uncompSize,
      file_name_length: fileNameLength,
      extra_field_length: extraFieldLength,
      file_comment_length: 0,
      disk_num_start: 0,
      internal_attr: 0,
      external_attr: 420 <<< 16,
      local_header_offset: pos
    )
  end

  defp cd_file_header_to_bin(
         r_cd_file_header(
           version_made_by: versionMadeBy,
           version_needed: versionNeeded,
           gp_flag: gPFlag,
           comp_method: compMethod,
           last_mod_time: lastModTime,
           last_mod_date: lastModDate,
           crc32: cRC32,
           comp_size: compSize,
           uncomp_size: uncompSize,
           file_name_length: fileNameLength,
           extra_field_length: extraFieldLength,
           file_comment_length: fileCommentLength,
           disk_num_start: diskNumStart,
           internal_attr: internalAttr,
           external_attr: externalAttr,
           local_header_offset: localHeaderOffset
         )
       ) do
    <<versionMadeBy::size(16)-little, versionNeeded::size(16)-little, gPFlag::size(16)-little,
      compMethod::size(16)-little, lastModTime::size(16)-little, lastModDate::size(16)-little,
      cRC32::size(32)-little, compSize::size(32)-little, uncompSize::size(32)-little,
      fileNameLength::size(16)-little, extraFieldLength::size(16)-little,
      fileCommentLength::size(16)-little, diskNumStart::size(16)-little,
      internalAttr::size(16)-little, externalAttr::size(32)-little,
      localHeaderOffset::size(32)-little>>
  end

  defp local_file_header_to_bin(
         r_local_file_header(
           version_needed: versionNeeded,
           gp_flag: gPFlag,
           comp_method: compMethod,
           last_mod_time: lastModTime,
           last_mod_date: lastModDate,
           crc32: cRC32,
           comp_size: compSize,
           uncomp_size: uncompSize,
           file_name_length: fileNameLength,
           extra_field_length: extraFieldLength
         )
       ) do
    <<versionNeeded::size(16)-little, gPFlag::size(16)-little, compMethod::size(16)-little,
      lastModTime::size(16)-little, lastModDate::size(16)-little, cRC32::size(32)-little,
      compSize::size(32)-little, uncompSize::size(32)-little, fileNameLength::size(16)-little,
      extraFieldLength::size(16)-little>>
  end

  defp eocd_to_bin(
         r_eocd(
           disk_num: diskNum,
           start_disk_num: startDiskNum,
           entries_on_disk: entriesOnDisk,
           entries: entries,
           size: size,
           offset: offset,
           zip_comment_length: zipCommentLength
         )
       ) do
    <<diskNum::size(16)-little, startDiskNum::size(16)-little, entriesOnDisk::size(16)-little,
      entries::size(16)-little, size::size(32)-little, offset::size(32)-little,
      zipCommentLength::size(16)-little>>
  end

  defp local_file_header_from_info_method_name(
         r_file_info(mtime: mTime),
         uncompSize,
         compMethod,
         name,
         gPFlag
       ) do
    {modDate, modTime} = dos_date_time_from_datetime(mTime)

    r_local_file_header(
      version_needed: 20,
      gp_flag: gPFlag,
      comp_method: compMethod,
      last_mod_time: modTime,
      last_mod_date: modDate,
      crc32: -1,
      comp_size: -1,
      uncomp_size: uncompSize,
      file_name_length: length(name),
      extra_field_length: 0
    )
  end

  defp server_init(parent) do
    :erlang.process_flag(:trap_exit, true)
    server_loop(parent, :not_open)
  end

  defp server_loop(parent, openZip) do
    receive do
      {from, {:open, archive, options}} ->
        case openzip_open(archive, options) do
          {:ok, newOpenZip} ->
            send(from, {self(), {:ok, self()}})
            server_loop(parent, newOpenZip)

          error ->
            send(from, {self(), error})
        end

      {from, :close} ->
        send(from, {self(), openzip_close(openZip)})

      {from, :get} ->
        send(from, {self(), openzip_get(openZip)})
        server_loop(parent, openZip)

      {from, {:get, fileName}} ->
        send(from, {self(), openzip_get(fileName, openZip)})
        server_loop(parent, openZip)

      {from, :list_dir} ->
        send(from, {self(), openzip_list_dir(openZip)})
        server_loop(parent, openZip)

      {from, {:list_dir, opts}} ->
        send(from, {self(), openzip_list_dir(openZip, opts)})
        server_loop(parent, openZip)

      {from, :get_state} ->
        send(from, {self(), openZip})
        server_loop(parent, openZip)

      {:EXIT, ^parent, reason} ->
        _ = openzip_close(openZip)
        exit({:parent_died, reason})

      _ ->
        {:error, :bad_msg}
    end
  end

  def zip_open(archive) do
    zip_open(archive, [])
  end

  def zip_open(archive, options) do
    self = self()

    pid =
      spawn_link(fn ->
        server_init(self)
      end)

    request(self, pid, {:open, archive, options})
  end

  def zip_get(pid) when is_pid(pid) do
    request(self(), pid, :get)
  end

  def zip_close(pid) when is_pid(pid) do
    request(self(), pid, :close)
  end

  def zip_get(fileName, pid) when is_pid(pid) do
    request(self(), pid, {:get, fileName})
  end

  def zip_list_dir(pid) when is_pid(pid) do
    request(self(), pid, :list_dir)
  end

  def zip_list_dir(pid, opts) when is_pid(pid) do
    request(self(), pid, {:list_dir, opts})
  end

  def zip_get_state(pid) when is_pid(pid) do
    request(self(), pid, :get_state)
  end

  defp request(self, pid, req) do
    send(pid, {self, req})

    receive do
      {^pid, r} ->
        r
    end
  end

  def zip_t(pid) when is_pid(pid) do
    openzip = request(self(), pid, :get_state)
    openzip_t(openzip)
  end

  def zip_tt(pid) when is_pid(pid) do
    openzip = request(self(), pid, :get_state)
    openzip_tt(openzip)
  end

  def openzip_tt(r_openzip(zip_comment: zipComment, files: files)) do
    print_comment(zipComment)

    lists_foreach(
      fn {r_zip_file(comp_size: compSize, name: fileName, comment: fileComment, info: fI), _} ->
        r_file_info(size: uncompSize, mtime: mTime) = fI
        print_header(compSize, mTime, uncompSize, fileName, fileComment)
      end,
      files
    )

    :ok
  end

  def openzip_t(r_openzip(zip_comment: zipComment, files: files)) do
    print_comment(zipComment)

    lists_foreach(
      fn {r_zip_file(name: fileName), _} ->
        print_file_name(fileName)
      end,
      files
    )

    :ok
  end

  defp lists_foreach(_, []) do
    :ok
  end

  defp lists_foreach(f, [hd | tl]) do
    f.(hd)
    lists_foreach(f, tl)
  end

  defp get_openzip_opt([], opts) do
    opts
  end

  defp get_openzip_opt([:cooked | rest], r_openzip_opts(open_opts: oO) = opts) do
    get_openzip_opt(rest, r_openzip_opts(opts, open_opts: oO -- [:raw]))
  end

  defp get_openzip_opt([:memory | rest], opts) do
    get_openzip_opt(rest, r_openzip_opts(opts, output: &binary_io/2))
  end

  defp get_openzip_opt([{:cwd, cWD} | rest], opts) do
    get_openzip_opt(rest, r_openzip_opts(opts, cwd: cWD))
  end

  defp get_openzip_opt([unknown | _Rest], _Opts) do
    throw({:bad_option, unknown})
  end

  defp get_central_dir(in0, rawIterator, input) do
    {b, in1} = get_end_of_central_dir(in0, 4 + 2 + 2 + 2 + 2 + 4 + 4 + 2, input)
    {eOCD, bComment} = eocd_and_comment_from_bin(b)
    in2 = input.({:seek, :bof, r_eocd(eOCD, :offset)}, in1)
    n = r_eocd(eOCD, :entries)
    acc0 = []
    comment = heuristic_to_string(bComment)
    out0 = rawIterator.(eOCD, '', comment, <<>>, acc0)
    get_cd_loop(n, in2, rawIterator, input, out0)
  end

  defp get_cd_loop(0, in__, _RawIterator, _Input, acc) do
    {:lists.reverse(acc), in__}
  end

  defp get_cd_loop(n, in0, rawIterator, input, acc0) do
    {b, in1} =
      input.(
        {:read, 4 + 2 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4},
        in0
      )

    bCD =
      case b do
        <<33_639_248::size(32)-little, xBCD::binary>> ->
          xBCD

        _ ->
          throw(:bad_central_directory)
      end

    cD = cd_file_header_from_bin(bCD)
    fileNameLen = r_cd_file_header(cD, :file_name_length)
    extraLen = r_cd_file_header(cD, :extra_field_length)
    commentLen = r_cd_file_header(cD, :file_comment_length)
    toRead = fileNameLen + extraLen + commentLen
    gPFlag = r_cd_file_header(cD, :gp_flag)
    {b2, in2} = input.({:read, toRead}, in1)

    {fileName, comment, bExtra} =
      get_name_extra_comment(b2, fileNameLen, extraLen, commentLen, gPFlag)

    acc1 = rawIterator.(cD, fileName, comment, bExtra, acc0)
    get_cd_loop(n - 1, in2, rawIterator, input, acc1)
  end

  defp get_name_extra_comment(b, fileNameLen, extraLen, commentLen, gPFlag) do
    try do
      <<bFileName::size(fileNameLen)-binary, bExtra::size(extraLen)-binary,
        bComment::size(commentLen)-binary>> = b

      {binary_to_chars(bFileName, gPFlag), heuristic_to_string(bComment), bExtra}
    catch
      _, _ ->
        throw(:bad_central_directory)
    end
  end

  defp get_end_of_central_dir(_In, sz, _Input) when sz > 65535 do
    throw(:bad_eocd)
  end

  defp get_end_of_central_dir(in0, sz, input) do
    in1 = input.({:seek, :eof, -sz}, in0)
    {b, in2} = input.({:read, sz}, in1)

    case find_eocd_header(b) do
      :none ->
        get_end_of_central_dir(in2, sz + sz, input)

      header ->
        {header, in2}
    end
  end

  defp find_eocd_header(<<101_010_256::size(32)-little, rest::binary>>) do
    rest
  end

  defp find_eocd_header(<<_::size(8), rest::binary>>)
       when byte_size(rest) > 4 + 2 + 2 + 2 + 2 + 4 + 4 + 2 - 4 do
    find_eocd_header(rest)
  end

  defp find_eocd_header(_) do
    :none
  end

  defp raw_file_info_etc(cD, fileName, fileComment, bExtraField, acc)
       when elem(cD, 0) === :cd_file_header do
    r_cd_file_header(comp_size: compSize, local_header_offset: offset, crc32: cRC) = cD
    fileInfo = cd_file_header_to_file_info(fileName, cD, bExtraField)

    [
      {r_zip_file(
         name: fileName,
         info: fileInfo,
         comment: fileComment,
         offset: offset,
         comp_size: compSize
       ), r_zip_file_extra(crc32: cRC)}
      | acc
    ]
  end

  defp raw_file_info_etc(eOCD, _, comment, _, acc)
       when elem(eOCD, 0) === :eocd do
    [r_zip_comment(comment: comment) | acc]
  end

  defp raw_file_info_public(cD, fileName, fileComment, bExtraField, acc0) do
    [h1 | t] = raw_file_info_etc(cD, fileName, fileComment, bExtraField, acc0)

    h2 =
      case h1 do
        {zF, extra} when elem(extra, 0) === :zip_file_extra ->
          zF

        other ->
          other
      end

    [h2 | t]
  end

  defp cd_file_header_to_file_info(
         fileName,
         r_cd_file_header(
           uncomp_size: uncompSize,
           last_mod_time: modTime,
           last_mod_date: modDate
         ),
         extraField
       ) do
    t = dos_date_time_to_datetime(modDate, modTime)

    type =
      case :lists.last(fileName) do
        ?/ ->
          :directory

        _ ->
          :regular
      end

    fI =
      r_file_info(
        size: uncompSize,
        type: type,
        access: :read_write,
        atime: t,
        mtime: t,
        ctime: t,
        mode: 54,
        links: 1,
        major_device: 0,
        minor_device: 0,
        inode: 0,
        uid: 0,
        gid: 0
      )

    add_extra_info(fI, extraField)
  end

  defp add_extra_info(fI, _) do
    fI
  end

  defp get_z_files([], _Z, _In, _Opts, acc) do
    :lists.reverse(acc)
  end

  defp get_z_files([r_zip_comment(comment: _) | rest], z, in__, opts, acc) do
    get_z_files(rest, z, in__, opts, acc)
  end

  defp get_z_files(
         [{r_zip_file(offset: offset), _} = zFile | rest],
         z,
         in0,
         r_unzip_opts(
           input: input,
           output: output,
           open_opts: opO,
           file_filter: filter,
           feedback: fB,
           cwd: cWD
         ) = opts,
         acc0
       ) do
    case filter.(zFile) do
      true ->
        in1 = input.({:seek, :bof, offset}, in0)

        {in2, acc1} =
          case get_z_file(in1, z, input, output, opO, fB, cWD, zFile, filter) do
            {:file, gZD, inx} ->
              {inx, [gZD | acc0]}

            {_, inx} ->
              {inx, acc0}
          end

        get_z_files(rest, z, in2, opts, acc1)

      _ ->
        get_z_files(rest, z, in0, opts, acc0)
    end
  end

  defp get_z_file(in0, z, input, output, opO, fB, cWD, {zipFile, extra}, filter) do
    case input.(
           {:read, 4 + 2 + 2 + 2 + 2 + 2 + 4 + 4 + 4 + 2 + 2},
           in0
         ) do
      {:eof, in1} ->
        {:eof, in1}

      {<<67_324_752::size(32)-little, b::binary>>, in1} ->
        lH = local_file_header_from_bin(b)

        r_local_file_header(
          gp_flag: gPFlag,
          comp_method: compMethod,
          file_name_length: fileNameLen,
          extra_field_length: extraLen
        ) = lH

        {compSize, cRC32} =
          case gPFlag &&& 8 === 8 do
            true ->
              {r_zip_file(zipFile, :comp_size), r_zip_file_extra(extra, :crc32)}

            false ->
              {r_local_file_header(lH, :comp_size), r_local_file_header(lH, :crc32)}
          end

        {bFileN, in3} =
          input.(
            {:read, fileNameLen + extraLen},
            in1
          )

        {fileName, _} = get_file_name_extra(fileNameLen, extraLen, bFileN, gPFlag)

        readAndWrite =
          case check_valid_location(
                 cWD,
                 fileName
               ) do
            {true, fileName1} ->
              true

            {false, fileName1} ->
              filter.({r_zip_file(zipFile, name: fileName1), extra})
          end

        case readAndWrite do
          true ->
            case :lists.last(fileName) do
              ?/ ->
                output.({:ensure_dir, fileName1}, [])
                {:dir, in3}

              _ ->
                {out, in4, cRC, _UncompSize} =
                  get_z_data(compMethod, in3, fileName1, compSize, input, output, opO, z)

                in5 = skip_z_data_descriptor(gPFlag, input, in4)
                fB.(fileName)
                cRC === cRC32 or throw({:bad_crc, fileName})
                {:file, out, in5}
            end

          false ->
            {:ignore, in3}
        end

      _ ->
        throw(:bad_local_file_header)
    end
  end

  defp check_valid_location(cWD, fileName) do
    case check_dir_level(:filename.split(fileName), 0) do
      {fileOrDir, level} when level < 0 ->
        cWD1 =
          cond do
            cWD == '' ->
              './'

            true ->
              cWD
          end

        :error_logger.format('Illegal path: ~ts, extracting in ~ts~n', [
          add_cwd(cWD, fileName),
          cWD1
        ])

        {false, add_cwd(cWD, fileOrDir)}

      _ ->
        {true, add_cwd(cWD, fileName)}
    end
  end

  defp check_dir_level([fileOrDir], level) do
    {fileOrDir, level}
  end

  defp check_dir_level(['.' | parts], level) do
    check_dir_level(parts, level)
  end

  defp check_dir_level(['..' | parts], level) do
    check_dir_level(parts, level - 1)
  end

  defp check_dir_level([_Dir | parts], level) do
    check_dir_level(parts, level + 1)
  end

  defp get_file_name_extra(fileNameLen, extraLen, b, gPFlag) do
    try do
      <<bFileName::size(fileNameLen)-binary, bExtra::size(extraLen)-binary>> = b
      {binary_to_chars(bFileName, gPFlag), bExtra}
    catch
      _, _ ->
        throw(:bad_file_header)
    end
  end

  defp get_z_data(8, in0, fileName, compSize, input, output, opO, z) do
    :ok = :zlib.inflateInit(z, -15)
    out0 = output.({:open, fileName, [:write | opO]}, [])
    {in1, out1, uncompSize} = get_z_data_loop(compSize, 0, in0, out0, input, output, z)
    cRC = :zlib.crc32(z)

    try do
      :zlib.inflateEnd(z)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    out2 = output.({:close, fileName}, out1)
    {out2, in1, cRC, uncompSize}
  end

  defp get_z_data(0, in0, fileName, compSize, input, output, opO, z) do
    out0 = output.({:open, fileName, [:write | opO]}, [])
    cRC0 = :zlib.crc32(z, <<>>)
    {in1, out1, cRC} = copy_data_loop(compSize, in0, out0, input, output, cRC0, z)
    out2 = output.({:close, fileName}, out1)
    {out2, in1, cRC, compSize}
  end

  defp get_z_data(_, _, _, _, _, _, _, _) do
    throw(:bad_file_header)
  end

  defp copy_data_loop(0, in__, out, _Input, _Output, cRC, _Z) do
    {in__, out, cRC}
  end

  defp copy_data_loop(compSize, in0, out0, input, output, cRC0, z) do
    n = :erlang.min(16 * 1024, compSize)

    case input.({:read, n}, in0) do
      {:eof, in1} ->
        {out0, in1}

      {uncompressed, in1} ->
        cRC1 = :zlib.crc32(z, cRC0, uncompressed)
        out1 = output.({:write, uncompressed}, out0)
        copy_data_loop(compSize - n, in1, out1, input, output, cRC1, z)
    end
  end

  defp get_z_data_loop(0, uncompSize, in__, out, _Input, _Output, _Z) do
    {in__, out, uncompSize}
  end

  defp get_z_data_loop(compSize, uncompSize, in0, out0, input, output, z) do
    n = :erlang.min(16 * 1024, compSize)

    case input.({:read, n}, in0) do
      {:eof, in1} ->
        {out0, in1}

      {compressed, in1} ->
        uncompressed = :zlib.inflate(z, compressed)
        out1 = output.({:write, uncompressed}, out0)

        get_z_data_loop(
          compSize - n,
          uncompSize + :erlang.iolist_size(uncompressed),
          in1,
          out1,
          input,
          output,
          z
        )
    end
  end

  defp skip_z_data_descriptor(gPFlag, input, in0) when gPFlag &&& 8 === 8 do
    input.({:seek, :cur, 12}, in0)
  end

  defp skip_z_data_descriptor(_GPFlag, _Input, in0) do
    in0
  end

  defp dos_date_time_to_datetime(dosDate, dosTime) do
    <<hour::size(5), min::size(6), sec::size(5)>> = <<dosTime::size(16)>>
    <<yearFrom1980::size(7), month::size(4), day::size(5)>> = <<dosDate::size(16)>>
    {{yearFrom1980 + 1980, month, day}, {hour, min, sec}}
  end

  defp dos_date_time_from_datetime({{year, month, day}, {hour, min, sec}}) do
    yearFrom1980 = year - 1980
    <<dosTime::size(16)>> = <<hour::size(5), min::size(6), sec::size(5)>>
    <<dosDate::size(16)>> = <<yearFrom1980::size(7), month::size(4), day::size(5)>>
    {dosDate, dosTime}
  end

  defp pwrite_binary(b, pos, bin) when byte_size(b) === pos do
    append_bins(bin, b)
  end

  defp pwrite_binary(b, pos, bin) do
    :erlang.iolist_to_binary(pwrite_iolist(b, pos, bin))
  end

  defp append_bins([bin | bins], b) when is_binary(bin) do
    append_bins(bins, <<b::binary, bin::binary>>)
  end

  defp append_bins([list | bins], b) when is_list(list) do
    append_bins(bins, append_bins(list, b))
  end

  defp append_bins(bin, b) when is_binary(bin) do
    <<b::binary, bin::binary>>
  end

  defp append_bins([_ | _] = list, b) do
    <<b::binary, :erlang.iolist_to_binary(list)::binary>>
  end

  defp append_bins([], b) do
    b
  end

  defp pwrite_iolist(b, pos, bin) do
    {left, right} = :erlang.split_binary(b, pos)
    sz = :erlang.iolist_size(bin)
    r = skip_bin(right, sz)
    [left, bin | r]
  end

  defp skip_bin(b, pos) when is_binary(b) do
    case b do
      <<_::size(pos)-binary, bin::binary>> ->
        bin

      _ ->
        <<>>
    end
  end

  defp binary_to_chars(b, gPFlag) do
    :ok

    case gPFlag &&& 2048 do
      0 ->
        :erlang.binary_to_list(b)

      2048 ->
        case :unicode.characters_to_list(b) do
          list when is_list(list) ->
            list
        end
    end
  end

  defp heuristic_to_string(b) when is_binary(b) do
    case :unicode.characters_to_binary(b) do
      ^b ->
        :unicode.characters_to_list(b)

      _ ->
        :erlang.binary_to_list(b)
    end
  end

  defp encode_string(string) do
    case :lists.any(
           fn c ->
             c > 127
           end,
           string
         ) do
      true ->
        case :unicode.characters_to_binary(string) do
          b when is_binary(b) ->
            {:erlang.binary_to_list(b), 2048}

          _ ->
            throw({:bad_unicode, string})
        end

      false ->
        {string, 0}
    end
  end

  defp eocd_and_comment_from_bin(
         <<diskNum::size(16)-little, startDiskNum::size(16)-little,
           entriesOnDisk::size(16)-little, entries::size(16)-little, size::size(32)-little,
           offset::size(32)-little, zipCommentLength::size(16)-little,
           comment::size(zipCommentLength)-binary>>
       ) do
    {r_eocd(
       disk_num: diskNum,
       start_disk_num: startDiskNum,
       entries_on_disk: entriesOnDisk,
       entries: entries,
       size: size,
       offset: offset,
       zip_comment_length: zipCommentLength
     ), comment}
  end

  defp eocd_and_comment_from_bin(_) do
    throw(:bad_eocd)
  end

  defp cd_file_header_from_bin(
         <<versionMadeBy::size(16)-little, versionNeeded::size(16)-little,
           gPFlag::size(16)-little, compMethod::size(16)-little, lastModTime::size(16)-little,
           lastModDate::size(16)-little, cRC32::size(32)-little, compSize::size(32)-little,
           uncompSize::size(32)-little, fileNameLength::size(16)-little,
           extraFieldLength::size(16)-little, fileCommentLength::size(16)-little,
           diskNumStart::size(16)-little, internalAttr::size(16)-little,
           externalAttr::size(32)-little, localHeaderOffset::size(32)-little>>
       ) do
    r_cd_file_header(
      version_made_by: versionMadeBy,
      version_needed: versionNeeded,
      gp_flag: gPFlag,
      comp_method: compMethod,
      last_mod_time: lastModTime,
      last_mod_date: lastModDate,
      crc32: cRC32,
      comp_size: compSize,
      uncomp_size: uncompSize,
      file_name_length: fileNameLength,
      extra_field_length: extraFieldLength,
      file_comment_length: fileCommentLength,
      disk_num_start: diskNumStart,
      internal_attr: internalAttr,
      external_attr: externalAttr,
      local_header_offset: localHeaderOffset
    )
  end

  defp cd_file_header_from_bin(_) do
    throw(:bad_cd_file_header)
  end

  defp local_file_header_from_bin(
         <<versionNeeded::size(16)-little, gPFlag::size(16)-little, compMethod::size(16)-little,
           lastModTime::size(16)-little, lastModDate::size(16)-little, cRC32::size(32)-little,
           compSize::size(32)-little, uncompSize::size(32)-little,
           fileNameLength::size(16)-little, extraFieldLength::size(16)-little>>
       ) do
    r_local_file_header(
      version_needed: versionNeeded,
      gp_flag: gPFlag,
      comp_method: compMethod,
      last_mod_time: lastModTime,
      last_mod_date: lastModDate,
      crc32: cRC32,
      comp_size: compSize,
      uncomp_size: uncompSize,
      file_name_length: fileNameLength,
      extra_field_length: extraFieldLength
    )
  end

  defp local_file_header_from_bin(_) do
    throw(:bad_local_file_header)
  end

  defp binary_io({:file_info, {_Filename, _B, r_file_info() = fI}}, _A) do
    fI
  end

  defp binary_io({:file_info, {_Filename, r_file_info() = fI, _B}}, _A) do
    fI
  end

  defp binary_io({:file_info, {_Filename, b}}, a) do
    binary_io({:file_info, b}, a)
  end

  defp binary_io({:file_info, b}, _) do
    {type, size} =
      cond do
        is_binary(b) ->
          {:regular, byte_size(b)}

        b === :directory ->
          {:directory, 0}
      end

    now = :calendar.local_time()

    r_file_info(
      size: size,
      type: type,
      access: :read_write,
      atime: now,
      mtime: now,
      ctime: now,
      mode: 0,
      links: 1,
      major_device: 0,
      minor_device: 0,
      inode: 0,
      uid: 0,
      gid: 0
    )
  end

  defp binary_io({:open, {_Filename, b, _FI}, _Opts}, _)
       when is_binary(b) do
    {0, b}
  end

  defp binary_io({:open, {_Filename, _FI, b}, _Opts}, _)
       when is_binary(b) do
    {0, b}
  end

  defp binary_io({:open, {_Filename, b}, _Opts}, _)
       when is_binary(b) do
    {0, b}
  end

  defp binary_io({:open, b, _Opts}, _) when is_binary(b) do
    {0, b}
  end

  defp binary_io({:open, filename, _Opts}, _)
       when is_list(filename) do
    {0, <<>>}
  end

  defp binary_io({:read, n}, {pos, b})
       when pos >= byte_size(b) do
    {:eof, {pos + n, b}}
  end

  defp binary_io({:read, n}, {pos, b})
       when pos + n > byte_size(b) do
    <<_::size(pos)-binary, read::binary>> = b
    {read, {byte_size(b), b}}
  end

  defp binary_io({:pread, pos, n}, {oldPos, b}) do
    case b do
      <<_::size(pos)-binary, read::size(n)-binary, _Rest::binary>> ->
        {read, {pos + n, b}}

      _ ->
        {:eof, {oldPos, b}}
    end
  end

  defp binary_io({:read, n}, {pos, b}) do
    <<_::size(pos)-binary, read::size(n)-binary, _::binary>> = b
    {read, {pos + n, b}}
  end

  defp binary_io({:seek, :bof, pos}, {_OldPos, b}) do
    {pos, b}
  end

  defp binary_io({:seek, :cur, pos}, {oldPos, b}) do
    {oldPos + pos, b}
  end

  defp binary_io({:seek, :eof, pos}, {_OldPos, b}) do
    {byte_size(b) + pos, b}
  end

  defp binary_io({:pwrite, pos, data}, {oldPos, b}) do
    {oldPos, pwrite_binary(b, pos, data)}
  end

  defp binary_io({:write, data}, {pos, b}) do
    {pos + :erlang.iolist_size(data), pwrite_binary(b, pos, data)}
  end

  defp binary_io(:close, {_Pos, b}) do
    b
  end

  defp binary_io({:close, fN}, {_Pos, b}) do
    {fN, b}
  end

  defp binary_io({:list_dir, _F}, _B) do
    []
  end

  defp binary_io({:set_file_info, _F, _FI}, b) do
    b
  end

  defp binary_io({:ensure_dir, _Dir}, b) do
    b
  end

  defp file_io({:file_info, f}, _) do
    case :file.read_file_info(f) do
      {:ok, info} ->
        info

      {:error, e} ->
        throw(e)
    end
  end

  defp file_io({:open, fN, opts}, _) do
    case :lists.member(:write, opts) do
      true ->
        :ok = :filelib.ensure_dir(fN)

      _ ->
        :ok
    end

    case :file.open(fN, opts ++ [:binary]) do
      {:ok, h} ->
        h

      {:error, e} ->
        throw(e)
    end
  end

  defp file_io({:read, n}, h) do
    case :file.read(h, n) do
      {:ok, b} ->
        {b, h}

      :eof ->
        {:eof, h}

      {:error, e} ->
        throw(e)
    end
  end

  defp file_io({:pread, pos, n}, h) do
    case :file.pread(h, pos, n) do
      {:ok, b} ->
        {b, h}

      :eof ->
        {:eof, h}

      {:error, e} ->
        throw(e)
    end
  end

  defp file_io({:seek, s, pos}, h) do
    case :file.position(h, {s, pos}) do
      {:ok, _NewPos} ->
        h

      {:error, error} ->
        throw(error)
    end
  end

  defp file_io({:write, data}, h) do
    case :file.write(h, data) do
      :ok ->
        h

      {:error, error} ->
        throw(error)
    end
  end

  defp file_io({:pwrite, pos, data}, h) do
    case :file.pwrite(h, pos, data) do
      :ok ->
        h

      {:error, error} ->
        throw(error)
    end
  end

  defp file_io({:close, fN}, h) do
    case :file.close(h) do
      :ok ->
        fN

      {:error, error} ->
        throw(error)
    end
  end

  defp file_io(:close, h) do
    file_io({:close, :ok}, h)
  end

  defp file_io({:list_dir, f}, _H) do
    case :file.list_dir(f) do
      {:ok, files} ->
        files

      {:error, error} ->
        throw(error)
    end
  end

  defp file_io({:set_file_info, f, fI}, h) do
    case :file.write_file_info(f, fI) do
      :ok ->
        h

      {:error, error} ->
        throw(error)
    end
  end

  defp file_io({:ensure_dir, dir}, h) do
    :ok = :filelib.ensure_dir(dir)
    h
  end
end
