defmodule :m_erl_tar do
  use Bitwise
  import Kernel, except: [to_string: 1]
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

  Record.defrecord(:r_add_opts, :add_opts,
    read_info: :undefined,
    chunk_size: 0,
    verbose: false,
    atime: :undefined,
    mtime: :undefined,
    ctime: :undefined,
    uid: 0,
    gid: 0
  )

  Record.defrecord(:r_read_opts, :read_opts,
    cwd: :undefined,
    keep_old_files: false,
    files: :all,
    output: :file,
    open_mode: [],
    verbose: false
  )

  Record.defrecord(:r_tar_header, :tar_header,
    name: '',
    mode: 33188,
    uid: 0,
    gid: 0,
    size: 0,
    mtime: :undefined,
    typeflag: :undefined,
    linkname: '',
    uname: '',
    gname: '',
    devmajor: 0,
    devminor: 0,
    atime: :undefined,
    ctime: :undefined
  )

  Record.defrecord(:r_sparse_entry, :sparse_entry,
    offset: 0,
    num_bytes: 0
  )

  Record.defrecord(:r_sparse_array, :sparse_array, entries: [], is_extended: false, max_entries: 0)

  Record.defrecord(:r_header_v7, :header_v7,
    name: :undefined,
    mode: :undefined,
    uid: :undefined,
    gid: :undefined,
    size: :undefined,
    mtime: :undefined,
    checksum: :undefined,
    typeflag: :undefined,
    linkname: :undefined
  )

  Record.defrecord(:r_header_gnu, :header_gnu,
    header_v7: :undefined,
    magic: :undefined,
    version: :undefined,
    uname: :undefined,
    gname: :undefined,
    devmajor: :undefined,
    devminor: :undefined,
    atime: :undefined,
    ctime: :undefined,
    sparse: :undefined,
    real_size: :undefined
  )

  Record.defrecord(:r_header_star, :header_star,
    header_v7: :undefined,
    magic: :undefined,
    version: :undefined,
    uname: :undefined,
    gname: :undefined,
    devmajor: :undefined,
    devminor: :undefined,
    prefix: :undefined,
    atime: :undefined,
    ctime: :undefined,
    trailer: :undefined
  )

  Record.defrecord(:r_header_ustar, :header_ustar,
    header_v7: :undefined,
    magic: :undefined,
    version: :undefined,
    uname: :undefined,
    gname: :undefined,
    devmajor: :undefined,
    devminor: :undefined,
    prefix: :undefined
  )

  Record.defrecord(:r_reader, :reader,
    handle: :undefined,
    access: :undefined,
    pos: 0,
    func: :undefined
  )

  Record.defrecord(:r_reg_file_reader, :reg_file_reader,
    handle: :undefined,
    num_bytes: 0,
    pos: 0,
    size: 0
  )

  Record.defrecord(:r_sparse_file_reader, :sparse_file_reader,
    handle: :undefined,
    num_bytes: 0,
    pos: 0,
    size: 0,
    sparse_map: :EFE_TODO_NESTED_RECORD
  )

  def format_error(:invalid_tar_checksum) do
    'Checksum failed'
  end

  def format_error(:bad_header) do
    'Unrecognized tar header format'
  end

  def format_error({:bad_header, reason}) do
    :lists.flatten(:io_lib.format('Unrecognized tar header format: ~p', [reason]))
  end

  def format_error({:invalid_header, :negative_size}) do
    'Invalid header: negative size'
  end

  def format_error(:invalid_sparse_header_size) do
    'Invalid sparse header: negative size'
  end

  def format_error(:invalid_sparse_map_entry) do
    'Invalid sparse map entry'
  end

  def format_error({:invalid_sparse_map_entry, reason}) do
    :lists.flatten(:io_lib.format('Invalid sparse map entry: ~p', [reason]))
  end

  def format_error(:invalid_end_of_archive) do
    'Invalid end of archive'
  end

  def format_error(:eof) do
    'Unexpected end of file'
  end

  def format_error(:integer_overflow) do
    'Failed to parse numeric: integer overflow'
  end

  def format_error({:misaligned_read, pos}) do
    :lists.flatten(
      :io_lib.format('Read a block which was misaligned: block_size=~p pos=~p', [512, pos])
    )
  end

  def format_error(:invalid_gnu_1_0_sparsemap) do
    'Invalid GNU sparse map (version 1.0)'
  end

  def format_error({:invalid_gnu_0_1_sparsemap, format}) do
    :lists.flatten(:io_lib.format('Invalid GNU sparse map (version ~s)', [format]))
  end

  def format_error(:unsafe_path) do
    'The path points above the current working directory'
  end

  def format_error({name, reason}) do
    :lists.flatten(
      :io_lib.format(
        '~ts: ~ts',
        [name, format_error(reason)]
      )
    )
  end

  def format_error(atom) when is_atom(atom) do
    :file.format_error(atom)
  end

  def format_error(term) do
    :lists.flatten(:io_lib.format('~tp', [term]))
  end

  def init(userData, accessMode, fun)
      when is_function(
             fun,
             2
           ) do
    reader = r_reader(handle: userData, access: accessMode, func: fun)
    {:ok, pos, reader2} = do_position(reader, {:cur, 0})
    {:ok, r_reader(reader2, pos: pos)}
  end

  def init(_UserData, _AccessMode, _Fun) do
    {:error, :badarg}
  end

  def extract(name) do
    extract(name, [])
  end

  def extract({:binary, bin}, opts) when is_list(opts) do
    do_extract({:binary, bin}, opts)
  end

  def extract({:file, fd}, opts) when is_list(opts) do
    do_extract({:file, fd}, opts)
  end

  def extract(r_reader() = reader, opts) when is_list(opts) do
    do_extract(reader, opts)
  end

  def extract(name, opts)
      when is_list(name) or
             (is_binary(name) and is_list(opts)) do
    do_extract(name, opts)
  end

  defp do_extract(handle, opts) when is_list(opts) do
    opts2 = extract_opts(opts)

    acc =
      cond do
        r_read_opts(opts2, :output) === :memory ->
          []

        true ->
          :ok
      end

    foldl_read(handle, &extract1/4, acc, opts2)
  end

  defp extract1(:eof, reader, _, acc) when is_list(acc) do
    {:ok, {:ok, :lists.reverse(acc)}, reader}
  end

  defp extract1(:eof, reader, _, :leading_slash) do
    :error_logger.info_msg('erl_tar: removed leading \'/\' from member names\n')
    {:ok, :ok, reader}
  end

  defp extract1(:eof, reader, _, acc) do
    {:ok, acc, reader}
  end

  defp extract1(r_tar_header(name: name, size: size) = header, reader0, opts, acc0) do
    case check_extract(name, opts) do
      true ->
        case do_read(reader0, size) do
          {:ok, bin, reader1} ->
            acc = extract2(header, bin, opts, acc0)
            {:ok, acc, reader1}

          {:error, _} = err ->
            throw(err)
        end

      false ->
        {:ok, acc0, skip_file(reader0)}
    end
  end

  defp extract2(header, bin, opts, acc) do
    case write_extracted_element(header, bin, opts) do
      :ok ->
        case header do
          r_tar_header(name: '/' ++ _) ->
            :leading_slash

          r_tar_header() ->
            acc
        end

      {:ok, nameBin} when is_list(acc) ->
        [nameBin | acc]

      {:error, _} = err ->
        throw(err)
    end
  end

  defp check_extract(_, r_read_opts(files: :all)) do
    true
  end

  defp check_extract(name, r_read_opts(files: files)) do
    :ordsets.is_element(name, files)
  end

  def table(name) do
    table(name, [])
  end

  def table(name, opts) when is_list(opts) do
    foldl_read(name, &table1/4, [], table_opts(opts))
  end

  defp table1(:eof, reader, _, result) do
    {:ok, {:ok, :lists.reverse(result)}, reader}
  end

  defp table1(r_tar_header() = header, reader, r_read_opts(verbose: verbose), result) do
    attrs = table1_attrs(header, verbose)
    reader2 = skip_file(reader)
    {:ok, [attrs | result], reader2}
  end

  defp table1_attrs(
         r_tar_header(typeflag: typeflag, mode: mode) = header,
         true
       ) do
    type = typeflag(typeflag)
    name = r_tar_header(header, :name)
    mtime = r_tar_header(header, :mtime)
    uid = r_tar_header(header, :uid)
    gid = r_tar_header(header, :gid)
    size = r_tar_header(header, :size)
    {name, type, size, mtime, mode, uid, gid}
  end

  defp table1_attrs(r_tar_header(name: name), _Verbose) do
    name
  end

  defp typeflag(?0) do
    :regular
  end

  defp typeflag(0) do
    :regular
  end

  defp typeflag(?S) do
    :regular
  end

  defp typeflag(?7) do
    :regular
  end

  defp typeflag(?1) do
    :link
  end

  defp typeflag(?2) do
    :symlink
  end

  defp typeflag(?3) do
    :char
  end

  defp typeflag(?4) do
    :block
  end

  defp typeflag(?5) do
    :directory
  end

  defp typeflag(?6) do
    :fifo
  end

  defp typeflag(_) do
    :unknown
  end

  def t(name) when is_list(name) or is_binary(name) do
    case table(name) do
      {:ok, list} ->
        :lists.foreach(
          fn n ->
            :ok = :io.format('~ts\n', [n])
          end,
          list
        )

      error ->
        error
    end
  end

  def tt(name) do
    case table(name, [:verbose]) do
      {:ok, list} ->
        :lists.foreach(&print_header/1, list)

      error ->
        error
    end
  end

  defp print_header({name, type, size, mtime, mode, uid, gid}) do
    :io.format(
      '~s~s ~4w/~-4w ~7w ~s ~s\n',
      [type_to_string(type), mode_to_string(mode), uid, gid, size, time_to_string(mtime), name]
    )
  end

  defp type_to_string(:regular) do
    '-'
  end

  defp type_to_string(:directory) do
    'd'
  end

  defp type_to_string(:link) do
    'l'
  end

  defp type_to_string(:symlink) do
    's'
  end

  defp type_to_string(:char) do
    'c'
  end

  defp type_to_string(:block) do
    'b'
  end

  defp type_to_string(:fifo) do
    'f'
  end

  defp type_to_string(:unknown) do
    '?'
  end

  defp mode_to_string(mode) do
    mode_to_string(mode, 'xwrxwrxwr', [])
  end

  defp mode_to_string(mode, [c | t], acc) when mode &&& 1 === 1 do
    mode_to_string(mode >>> 1, t, [c | acc])
  end

  defp mode_to_string(mode, [_ | t], acc) do
    mode_to_string(mode >>> 1, t, [?- | acc])
  end

  defp mode_to_string(_, [], acc) do
    acc
  end

  defp time_to_string(secs0) do
    epoch = :calendar.datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
    secs = epoch + secs0
    dateTime0 = :calendar.gregorian_seconds_to_datetime(secs)
    dateTime = :calendar.universal_time_to_local_time(dateTime0)
    {{y, mon, day}, {h, min, _}} = dateTime

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

  def open({:binary, bin}, mode) when is_binary(bin) do
    do_open({:binary, bin}, mode)
  end

  def open({:file, fd}, mode) do
    do_open({:file, fd}, mode)
  end

  def open(name, mode)
      when is_list(name) or
             is_binary(name) do
    do_open(name, mode)
  end

  defp do_open(name, mode) when is_list(mode) do
    case open_mode(mode) do
      {:ok, access, raw, opts} ->
        open1(name, access, raw, opts)

      {:error, reason} ->
        {:error, {name, reason}}
    end
  end

  defp open1({:binary, bin0} = handle, :read, _Raw, opts)
       when is_binary(bin0) do
    bin =
      case :lists.member(:compressed, opts) do
        true ->
          try do
            :zlib.gunzip(bin0)
          catch
            _, _ ->
              bin0
          end

        false ->
          bin0
      end

    case :file.open(bin, [:ram, :binary, :read]) do
      {:ok, file} ->
        {:ok, r_reader(handle: file, access: :read, func: &file_op/2)}

      {:error, reason} ->
        {:error, {handle, reason}}
    end
  end

  defp open1({:file, fd} = handle, :read, [:raw], opts) do
    case not :lists.member(:compressed, opts) do
      true ->
        reader = r_reader(handle: fd, access: :read, func: &file_op/2)

        case do_position(reader, {:cur, 0}) do
          {:ok, pos, reader2} ->
            {:ok, r_reader(reader2, pos: pos)}

          {:error, reason} ->
            {:error, {handle, reason}}
        end

      false ->
        {:error, {handle, {:incompatible_option, :compressed}}}
    end
  end

  defp open1({:file, _Fd} = handle, :read, [], _Opts) do
    {:error, {handle, {:incompatible_option, :cooked}}}
  end

  defp open1(name, access, raw, opts)
       when is_list(name) or is_binary(name) do
    case :file.open(
           name,
           raw ++ [:binary, access | opts]
         ) do
      {:ok, file} ->
        {:ok, r_reader(handle: file, access: access, func: &file_op/2)}

      {:error, reason} ->
        {:error, {name, reason}}
    end
  end

  defp open_mode(mode) do
    open_mode(mode, false, [:raw], [])
  end

  defp open_mode(:read, _, raw, _) do
    {:ok, :read, raw, []}
  end

  defp open_mode(:write, _, raw, _) do
    {:ok, :write, raw, []}
  end

  defp open_mode([:read | rest], false, raw, opts) do
    open_mode(rest, :read, raw, opts)
  end

  defp open_mode([:write | rest], false, raw, opts) do
    open_mode(rest, :write, raw, opts)
  end

  defp open_mode([:compressed | rest], access, raw, opts) do
    open_mode(rest, access, raw, [:compressed, :read_ahead | opts])
  end

  defp open_mode([:cooked | rest], access, _Raw, opts) do
    open_mode(rest, access, [], opts)
  end

  defp open_mode([], access, raw, opts) do
    {:ok, access, raw, opts}
  end

  defp open_mode(_, _, _, _) do
    {:error, :einval}
  end

  defp file_op(:write, {fd, data}) do
    :file.write(fd, data)
  end

  defp file_op(:position, {fd, pos}) do
    :file.position(fd, pos)
  end

  defp file_op(:read2, {fd, size}) do
    :file.read(fd, size)
  end

  defp file_op(:close, fd) do
    :file.close(fd)
  end

  def close(r_reader(access: :read) = reader) do
    :ok = do_close(reader)
  end

  def close(r_reader(access: :write) = reader) do
    {:ok, reader2} = pad_file(reader)
    :ok = do_close(reader2)
    :ok
  end

  def close(_) do
    {:error, :einval}
  end

  defp pad_file(r_reader(pos: pos) = reader) do
    padCurrent = skip_padding(pos + 512)
    padding = <<0::size(padCurrent)-unit(8)>>

    do_write(
      reader,
      [
        padding,
        <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>,
        <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>
      ]
    )
  end

  def create(name, fileList)
      when is_list(name) or
             is_binary(name) do
    create(name, fileList, [])
  end

  def create(name, fileList, options)
      when is_list(name) or
             is_binary(name) do
    mode =
      :lists.filter(
        fn x ->
          :erlang.or(x === :compressed, x === :cooked)
        end,
        options
      )

    case open(name, [:write | mode]) do
      {:ok, tarFile} ->
        do_create(tarFile, fileList, options)

      {:error, _} = err ->
        err
    end
  end

  defp do_create(tarFile, [], _Opts) do
    close(tarFile)
  end

  defp do_create(tarFile, [{nameInArchive, nameOrBin} | rest], opts) do
    case add(tarFile, nameOrBin, nameInArchive, opts) do
      :ok ->
        do_create(tarFile, rest, opts)

      {:error, _} = err ->
        _ = close(tarFile)
        err
    end
  end

  defp do_create(tarFile, [name | rest], opts) do
    case add(tarFile, name, name, opts) do
      :ok ->
        do_create(tarFile, rest, opts)

      {:error, _} = err ->
        _ = close(tarFile)
        err
    end
  end

  def add(reader, {nameInArchive, name}, opts)
      when is_list(nameInArchive) and is_list(name) do
    do_add(reader, name, nameInArchive, opts)
  end

  def add(reader, {nameInArchive, bin}, opts)
      when is_list(nameInArchive) and is_binary(bin) do
    do_add(reader, bin, nameInArchive, opts)
  end

  def add(reader, name, opts) when is_list(name) do
    do_add(reader, name, name, opts)
  end

  def add(reader, nameOrBin, nameInArchive, options)
      when is_list(nameOrBin) or
             (is_binary(nameOrBin) and is_list(nameInArchive) and
                is_list(options)) do
    do_add(reader, nameOrBin, nameInArchive, options)
  end

  defp do_add(r_reader(access: :write) = reader, name, nameInArchive, options)
       when is_list(nameInArchive) and is_list(options) do
    rF = apply_file_info_opts_fun(options, :read_link_info)
    opts = r_add_opts(read_info: rF)
    add1(reader, name, nameInArchive, add_opts(options, options, opts))
  end

  defp do_add(r_reader(access: :read), _, _, _) do
    {:error, :eacces}
  end

  defp do_add(reader, _, _, _) do
    {:error, {:badarg, reader}}
  end

  defp add_opts([:dereference | t], allOptions, opts) do
    rF =
      apply_file_info_opts_fun(
        allOptions,
        :read_file_info
      )

    add_opts(t, allOptions, r_add_opts(opts, read_info: rF))
  end

  defp add_opts([:verbose | t], allOptions, opts) do
    add_opts(t, allOptions, r_add_opts(opts, verbose: true))
  end

  defp add_opts([{:chunks, n} | t], allOptions, opts) do
    add_opts(t, allOptions, r_add_opts(opts, chunk_size: n))
  end

  defp add_opts([{:atime, value} | t], allOptions, opts) do
    add_opts(t, allOptions, r_add_opts(opts, atime: value))
  end

  defp add_opts([{:mtime, value} | t], allOptions, opts) do
    add_opts(t, allOptions, r_add_opts(opts, mtime: value))
  end

  defp add_opts([{:ctime, value} | t], allOptions, opts) do
    add_opts(t, allOptions, r_add_opts(opts, ctime: value))
  end

  defp add_opts([{:uid, value} | t], allOptions, opts) do
    add_opts(t, allOptions, r_add_opts(opts, uid: value))
  end

  defp add_opts([{:gid, value} | t], allOptions, opts) do
    add_opts(t, allOptions, r_add_opts(opts, gid: value))
  end

  defp add_opts([_ | t], allOptions, opts) do
    add_opts(t, allOptions, opts)
  end

  defp add_opts([], _AllOptions, opts) do
    opts
  end

  defp apply_file_info_opts(opts, {:ok, fileInfo}) do
    {:ok, do_apply_file_info_opts(opts, fileInfo)}
  end

  defp apply_file_info_opts(_Opts, other) do
    other
  end

  defp do_apply_file_info_opts([{:atime, value} | t], fileInfo) do
    do_apply_file_info_opts(t, r_file_info(fileInfo, atime: value))
  end

  defp do_apply_file_info_opts([{:mtime, value} | t], fileInfo) do
    do_apply_file_info_opts(t, r_file_info(fileInfo, mtime: value))
  end

  defp do_apply_file_info_opts([{:ctime, value} | t], fileInfo) do
    do_apply_file_info_opts(t, r_file_info(fileInfo, ctime: value))
  end

  defp do_apply_file_info_opts([{:uid, value} | t], fileInfo) do
    do_apply_file_info_opts(t, r_file_info(fileInfo, uid: value))
  end

  defp do_apply_file_info_opts([{:gid, value} | t], fileInfo) do
    do_apply_file_info_opts(t, r_file_info(fileInfo, gid: value))
  end

  defp do_apply_file_info_opts([_ | t], fileInfo) do
    do_apply_file_info_opts(t, fileInfo)
  end

  defp do_apply_file_info_opts([], fileInfo) do
    fileInfo
  end

  defp apply_file_info_opts_fun(options, infoFunction) do
    fn f ->
      apply_file_info_opts(
        options,
        apply(:file, infoFunction, [f, [{:time, :posix}]])
      )
    end
  end

  defp add1(r_reader() = reader, name, nameInArchive, r_add_opts(read_info: readInfo) = opts)
       when is_list(name) do
    res =
      case readInfo.(name) do
        {:error, reason0} ->
          {:error, {name, reason0}}

        {:ok, r_file_info(type: :symlink) = fi} ->
          add_verbose(opts, 'a ~ts~n', [nameInArchive])
          {:ok, linkname} = :file.read_link(name)
          header = fileinfo_to_header(nameInArchive, fi, linkname)
          add_header(reader, header, opts)

        {:ok, r_file_info(type: :regular) = fi} ->
          add_verbose(opts, 'a ~ts~n', [nameInArchive])
          header = fileinfo_to_header(nameInArchive, fi, false)
          {:ok, reader2} = add_header(reader, header, opts)
          fileSize = r_tar_header(header, :size)
          {:ok, ^fileSize, reader3} = do_copy(reader2, name, opts)
          padding = skip_padding(fileSize)
          pad = <<0::size(padding)-unit(8)>>
          do_write(reader3, pad)

        {:ok, r_file_info(type: :directory) = fi} ->
          add_directory(reader, name, nameInArchive, fi, opts)

        {:ok, r_file_info() = fi} ->
          add_verbose(opts, 'a ~ts~n', [nameInArchive])
          header = fileinfo_to_header(nameInArchive, fi, false)
          add_header(reader, header, opts)
      end

    case res do
      :ok ->
        :ok

      {:ok, _Reader} ->
        :ok

      {:error, _Reason} = err ->
        err
    end
  end

  defp add1(reader, bin, nameInArchive, opts)
       when is_binary(bin) do
    add_verbose(opts, 'a ~ts~n', [nameInArchive])
    now = :os.system_time(:seconds)

    header =
      r_tar_header(
        name: nameInArchive,
        size: byte_size(bin),
        typeflag: ?0,
        atime: add_opts_time(r_add_opts(opts, :atime), now),
        mtime: add_opts_time(r_add_opts(opts, :mtime), now),
        ctime: add_opts_time(r_add_opts(opts, :ctime), now),
        uid: r_add_opts(opts, :uid),
        gid: r_add_opts(opts, :gid),
        mode: 33188
      )

    {:ok, reader2} = add_header(reader, header, opts)
    padding = skip_padding(byte_size(bin))
    data = [bin, <<0::size(padding)-unit(8)>>]

    case do_write(reader2, data) do
      {:ok, _Reader3} ->
        :ok

      {:error, reason} ->
        {:error, {nameInArchive, reason}}
    end
  end

  defp add_opts_time(:undefined, now) do
    now
  end

  defp add_opts_time(time, _Now) do
    time
  end

  defp add_directory(reader, dirName, nameInArchive, info, opts) do
    case :file.list_dir(dirName) do
      {:ok, []} ->
        add_verbose(opts, 'a ~ts~n', [nameInArchive])
        header = fileinfo_to_header(nameInArchive, info, false)
        add_header(reader, header, opts)

      {:ok, files} ->
        add_verbose(opts, 'a ~ts~n', [nameInArchive])

        try do
          add_files(reader, files, dirName, nameInArchive, opts)
        catch
          {:error, {_Name, _Reason}} = err ->
            err

          {:error, reason} ->
            {:error, {dirName, reason}}
        else
          :ok ->
            :ok

          {:error, _} = err ->
            err
        end

      {:error, reason} ->
        {:error, {dirName, reason}}
    end
  end

  defp add_files(_Reader, [], _Dir, _DirInArchive, _Opts) do
    :ok
  end

  defp add_files(reader, [name | rest], dir, dirInArchive, r_add_opts(read_info: info) = opts) do
    fullName = :filename.join(dir, name)
    nameInArchive = :filename.join(dirInArchive, name)

    res =
      case info.(fullName) do
        {:error, reason} ->
          {:error, {fullName, reason}}

        {:ok, r_file_info(type: :directory) = fi} ->
          add_directory(reader, fullName, nameInArchive, fi, opts)

        {:ok, r_file_info(type: :symlink) = fi} ->
          add_verbose(opts, 'a ~ts~n', [nameInArchive])
          {:ok, linkname} = :file.read_link(fullName)
          header = fileinfo_to_header(nameInArchive, fi, linkname)
          add_header(reader, header, opts)

        {:ok, r_file_info(type: :regular) = fi} ->
          add_verbose(opts, 'a ~ts~n', [nameInArchive])
          header = fileinfo_to_header(nameInArchive, fi, false)
          {:ok, reader2} = add_header(reader, header, opts)
          fileSize = r_tar_header(header, :size)
          {:ok, ^fileSize, reader3} = do_copy(reader2, fullName, opts)
          padding = skip_padding(fileSize)
          pad = <<0::size(padding)-unit(8)>>
          do_write(reader3, pad)

        {:ok, r_file_info() = fi} ->
          add_verbose(opts, 'a ~ts~n', [nameInArchive])
          header = fileinfo_to_header(nameInArchive, fi, false)
          add_header(reader, header, opts)
      end

    case res do
      :ok ->
        add_files(reader, rest, dir, dirInArchive, opts)

      {:ok, readerNext} ->
        add_files(readerNext, rest, dir, dirInArchive, opts)

      {:error, _} = err ->
        err
    end
  end

  defp format_string(string, size) when length(string) > size do
    throw({:error, {:write_string, :field_too_long}})
  end

  defp format_string(string, size) do
    ascii = to_ascii(string)

    cond do
      byte_size(ascii) < size ->
        [ascii, 0]

      true ->
        ascii
    end
  end

  defp format_octal(octal) do
    :erlang.iolist_to_binary(:io_lib.fwrite('~.8B', [octal]))
  end

  defp add_header(r_reader() = reader, r_tar_header() = header, opts) do
    {:ok, iodata} = build_header(header, opts)
    do_write(reader, iodata)
  end

  defp write_to_block(block, ioData, start) when is_list(ioData) do
    write_to_block(block, :erlang.iolist_to_binary(ioData), start)
  end

  defp write_to_block(block, bin, start) when is_binary(bin) do
    size = byte_size(bin)
    <<head::size(start)-unit(8), _::size(size)-unit(8), rest::binary>> = block
    <<head::size(start)-unit(8), bin::binary, rest::binary>>
  end

  defp build_header(r_tar_header() = header, opts) do
    r_tar_header(
      name: name,
      mode: mode,
      uid: uid,
      gid: gid,
      size: size,
      typeflag: type,
      linkname: linkname,
      uname: uname,
      gname: gname,
      devmajor: devmaj,
      devminor: devmin
    ) = header

    mtime = r_tar_header(header, :mtime)

    block0 =
      <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0>>

    {block1, pax0} = write_string(block0, 0, 100, name, "path", %{})
    block2 = write_octal(block1, 100, 8, mode)
    {block3, pax1} = write_numeric(block2, 108, 8, uid, "uid", pax0)
    {block4, pax2} = write_numeric(block3, 116, 8, gid, "gid", pax1)
    {block5, pax3} = write_numeric(block4, 124, 12, size, "size", pax2)
    {block6, pax4} = write_numeric(block5, 136, 12, mtime, "", pax3)
    {block7, pax5} = write_string(block6, 156, 1, <<type>>, "", pax4)
    {block8, pax6} = write_string(block7, 157, 100, linkname, "linkpath", pax5)
    {block9, pax7} = write_string(block8, 265, 32, uname, "uname", pax6)
    {block10, pax8} = write_string(block9, 297, 32, gname, "gname", pax7)
    {block11, pax9} = write_numeric(block10, 329, 8, devmaj, "", pax8)
    {block12, pax10} = write_numeric(block11, 337, 8, devmin, "", pax9)
    {block13, pax11} = set_path(block12, pax10)

    paxEntry =
      case :maps.size(pax11) do
        0 ->
          []

        _ ->
          build_pax_entry(header, pax11, opts)
      end

    block14 = set_format(block13, 4)
    block15 = set_checksum(block14)
    {:ok, [paxEntry, block15]}
  end

  defp set_path(block0, pax) do
    case :maps.get("path", pax, nil) do
      nil ->
        {block0, pax}

      paxPath ->
        case split_ustar_path(paxPath) do
          {:ok, ustarName, ustarPrefix} ->
            {block1, _} = write_string(block0, 0, 100, ustarName, "", %{})
            {block2, _} = write_string(block1, 345, 155, ustarPrefix, "", %{})
            {block2, :maps.remove("path", pax)}

          false ->
            {block0, pax}
        end
    end
  end

  defp set_format(block0, format)
       when format === 4 or
              format === 5 do
    block1 = write_to_block(block0, "ustar\000", 257)
    write_to_block(block1, "00", 263)
  end

  defp set_format(_Block, format) do
    throw({:error, {:invalid_format, format}})
  end

  defp set_checksum(block) do
    checksum = compute_checksum(block)
    write_octal(block, 148, 8, checksum)
  end

  defp build_pax_entry(header, paxAttrs, opts) do
    path = r_tar_header(header, :name)
    filename = :filename.basename(path)
    dir = :filename.dirname(path)
    path2 = :filename.join([dir, 'PaxHeaders.0', filename])
    asciiPath = to_ascii(path2)

    path3 =
      cond do
        byte_size(asciiPath) > 100 ->
          binary_part(asciiPath, 0, 100 - 1)

        true ->
          asciiPath
      end

    keys = :maps.keys(paxAttrs)
    sortedKeys = :lists.sort(keys)
    paxFile = build_pax_file(sortedKeys, paxAttrs)
    size = byte_size(paxFile)
    padding = rem(512 - rem(byte_size(paxFile), 512), 512)
    pad = <<0::size(padding)-unit(8)>>

    paxHeader =
      r_tar_header(
        name: :unicode.characters_to_list(path3),
        size: size,
        mtime: r_tar_header(header, :mtime),
        atime: r_tar_header(header, :atime),
        ctime: r_tar_header(header, :ctime),
        typeflag: ?x
      )

    {:ok, paxHeaderData} = build_header(paxHeader, opts)
    [paxHeaderData, paxFile, pad]
  end

  defp build_pax_file(keys, paxAttrs) do
    build_pax_file(keys, paxAttrs, [])
  end

  defp build_pax_file([], _, acc) do
    :unicode.characters_to_binary(acc)
  end

  defp build_pax_file([k | rest], attrs, acc) do
    v = :maps.get(k, attrs)
    size = sizeof(k) + sizeof(v) + 3
    size2 = sizeof(size) + size
    key = to_string(k)
    value = to_string(v)

    record =
      :unicode.characters_to_binary(
        :io_lib.format(
          '~B ~ts=~ts\n',
          [size2, key, value]
        )
      )

    cond do
      byte_size(record) !== size2 ->
        size3 = byte_size(record)
        record2 = :io_lib.format('~B ~ts=~ts\n', [size3, key, value])
        build_pax_file(rest, attrs, [acc, record2])

      true ->
        build_pax_file(rest, attrs, [acc, record])
    end
  end

  defp sizeof(bin) when is_binary(bin) do
    byte_size(bin)
  end

  defp sizeof(list) when is_list(list) do
    length(list)
  end

  defp sizeof(n) when is_integer(n) do
    byte_size(:erlang.integer_to_binary(n))
  end

  defp sizeof(n) when is_float(n) do
    byte_size(:erlang.float_to_binary(n))
  end

  defp to_string(bin) when is_binary(bin) do
    :unicode.characters_to_list(bin)
  end

  defp to_string(list) when is_list(list) do
    list
  end

  defp to_string(n) when is_integer(n) do
    :erlang.integer_to_list(n)
  end

  defp to_string(n) when is_float(n) do
    :erlang.float_to_list(n)
  end

  defp split_ustar_path(path) do
    len = length(path)
    notAscii = not is_ascii(path)

    cond do
      len <= 100 or notAscii ->
        false

      true ->
        pathBin = :binary.list_to_bin(path)

        case :binary.split(pathBin, [<<?/>>], [:global, :trim_all]) do
          [part] when byte_size(part) >= 100 ->
            false

          parts ->
            case :lists.last(parts) do
              name when byte_size(name) >= 100 ->
                false

              name ->
                parts2 = :lists.sublist(parts, length(parts) - 1)
                join_split_ustar_path(parts2, {:ok, name, nil})
            end
        end
    end
  end

  defp join_split_ustar_path([], acc) do
    acc
  end

  defp join_split_ustar_path([part | _], {:ok, _, nil})
       when byte_size(part) > 155 do
    false
  end

  defp join_split_ustar_path([part | _], {:ok, _Name, acc})
       when byte_size(part) + byte_size(acc) > 155 do
    false
  end

  defp join_split_ustar_path([part | rest], {:ok, name, nil}) do
    join_split_ustar_path(rest, {:ok, name, part})
  end

  defp join_split_ustar_path([part | rest], {:ok, name, acc}) do
    join_split_ustar_path(
      rest,
      {:ok, name, <<acc::binary, ?/, part::binary>>}
    )
  end

  defp write_octal(block, pos, size, x) do
    octal = zero_pad(format_octal(x), size - 1)

    cond do
      byte_size(octal) < size ->
        write_to_block(block, octal, pos)

      true ->
        throw({:error, {:write_failed, :octal_field_too_long}})
    end
  end

  defp write_string(block, pos, size, str, paxAttr, pax0) do
    notAscii = not is_ascii(str)

    cond do
      paxAttr !== "" and (length(str) > size or notAscii) ->
        pax1 = :maps.put(paxAttr, str, pax0)
        {block, pax1}

      true ->
        formatted = format_string(str, size)
        {write_to_block(block, formatted, pos), pax0}
    end
  end

  defp write_numeric(block, pos, size, x, paxAttr, pax0) do
    octal = zero_pad(format_octal(x), size - 1)

    cond do
      byte_size(octal) < size ->
        {write_to_block(block, [octal, 0], pos), pax0}

      paxAttr !== "" ->
        pax1 = :maps.put(paxAttr, x, pax0)
        {block, pax1}

      true ->
        throw({:error, {:write_failed, :numeric_field_too_long}})
    end
  end

  defp zero_pad(str, size) when byte_size(str) >= size do
    str
  end

  defp zero_pad(str, size) do
    padding = size - byte_size(str)
    pad = :binary.copy(<<?0>>, padding)
    <<pad::binary, str::binary>>
  end

  defp read_block(reader) do
    case do_read(reader, 512) do
      :eof ->
        throw({:error, :eof})

      {:ok,
       <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0>>, reader1} ->
        case do_read(reader1, 512) do
          :eof ->
            :eof

          {:ok,
           <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
             0, 0, 0, 0, 0, 0, 0, 0>>, _Reader2} ->
            :eof

          {:ok, _Block, _Reader2} ->
            throw({:error, :invalid_end_of_archive})

          {:error, _} = err ->
            throw(err)
        end

      {:ok, block, reader1} when is_binary(block) ->
        {:ok, block, reader1}

      {:error, _} = err ->
        throw(err)
    end
  end

  defp get_header(r_reader() = reader) do
    case read_block(reader) do
      :eof ->
        :eof

      {:ok, block, reader1} ->
        convert_header(block, reader1)
    end
  end

  defp to_v7(bin)
       when is_binary(bin) and
              byte_size(bin) === 512 do
    r_header_v7(
      name: binary_part(bin, 0, 100),
      mode: binary_part(bin, 100, 8),
      uid: binary_part(bin, 108, 8),
      gid: binary_part(bin, 116, 8),
      size: binary_part(bin, 124, 12),
      mtime: binary_part(bin, 136, 12),
      checksum: binary_part(bin, 148, 8),
      typeflag: :binary.at(bin, 156),
      linkname: binary_part(bin, 157, 100)
    )
  end

  defp to_v7(_) do
    {:error, :header_block_too_small}
  end

  defp to_gnu(r_header_v7() = v7, bin)
       when is_binary(bin) and
              byte_size(bin) === 512 do
    r_header_gnu(
      header_v7: v7,
      magic: binary_part(bin, 257, 6),
      version: binary_part(bin, 263, 2),
      uname: binary_part(bin, 265, 32),
      gname: binary_part(bin, 297, 32),
      devmajor: binary_part(bin, 329, 8),
      devminor: binary_part(bin, 337, 8),
      atime: binary_part(bin, 345, 12),
      ctime: binary_part(bin, 357, 12),
      sparse: to_sparse_array(binary_part(bin, 386, 24 * 4 + 1)),
      real_size: binary_part(bin, 483, 12)
    )
  end

  defp to_star(r_header_v7() = v7, bin)
       when is_binary(bin) and
              byte_size(bin) === 512 do
    r_header_star(
      header_v7: v7,
      magic: binary_part(bin, 257, 6),
      version: binary_part(bin, 263, 2),
      uname: binary_part(bin, 265, 32),
      gname: binary_part(bin, 297, 32),
      devmajor: binary_part(bin, 329, 8),
      devminor: binary_part(bin, 337, 8),
      prefix: binary_part(bin, 345, 131),
      atime: binary_part(bin, 476, 12),
      ctime: binary_part(bin, 488, 12),
      trailer: binary_part(bin, 508, 4)
    )
  end

  defp to_ustar(r_header_v7() = v7, bin)
       when is_binary(bin) and
              byte_size(bin) === 512 do
    r_header_ustar(
      header_v7: v7,
      magic: binary_part(bin, 257, 6),
      version: binary_part(bin, 263, 2),
      uname: binary_part(bin, 265, 32),
      gname: binary_part(bin, 297, 32),
      devmajor: binary_part(bin, 329, 8),
      devminor: binary_part(bin, 337, 8),
      prefix: binary_part(bin, 345, 155)
    )
  end

  defp to_sparse_array(bin) when is_binary(bin) do
    maxEntries = div(byte_size(bin), 24)
    isExtended = 1 === :binary.at(bin, 24 * maxEntries)
    entries = parse_sparse_entries(bin, maxEntries - 1, [])
    r_sparse_array(entries: entries, max_entries: maxEntries, is_extended: isExtended)
  end

  defp parse_sparse_entries(<<>>, _, acc) do
    acc
  end

  defp parse_sparse_entries(_, -1, acc) do
    acc
  end

  defp parse_sparse_entries(bin, n, acc) do
    case to_sparse_entry(binary_part(bin, n * 24, 24)) do
      nil ->
        parse_sparse_entries(bin, n - 1, acc)

      entry = r_sparse_entry() ->
        parse_sparse_entries(bin, n - 1, [entry | acc])
    end
  end

  defp to_sparse_entry(bin)
       when is_binary(bin) and
              byte_size(bin) === 24 do
    offsetBin = binary_part(bin, 0, 12)
    numBytesBin = binary_part(bin, 12, 12)

    case {offsetBin, numBytesBin} do
      {<<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>} ->
        nil

      _ ->
        r_sparse_entry(
          offset: parse_numeric(offsetBin),
          num_bytes: parse_numeric(numBytesBin)
        )
    end
  end

  defp get_format(bin)
       when is_binary(bin) and
              byte_size(bin) === 512 do
    do_get_format(to_v7(bin), bin)
  end

  defp do_get_format({:error, _} = err, _Bin) do
    err
  end

  defp do_get_format(r_header_v7() = v7, bin)
       when is_binary(bin) and
              byte_size(bin) === 512 do
    checksum = parse_octal(r_header_v7(v7, :checksum))
    chk1 = compute_checksum(bin)
    chk2 = compute_signed_checksum(bin)

    cond do
      checksum !== chk1 and checksum !== chk2 ->
        0

      true ->
        ustar = to_ustar(v7, bin)
        star = to_star(v7, bin)
        magic = r_header_ustar(ustar, :magic)
        version = r_header_ustar(ustar, :version)
        trailer = r_header_star(star, :trailer)

        format =
          cond do
            magic === "ustar\000" and trailer === "tar\000" ->
              3

            magic === "ustar\000" ->
              4

            magic === "ustar " and version === " \000" ->
              2

            true ->
              1
          end

        {:ok, format, v7}
    end
  end

  defp unpack_format(format, r_header_v7() = v7, bin, reader)
       when is_binary(bin) and byte_size(bin) === 512 do
    mtime = parse_numeric(r_header_v7(v7, :mtime))

    header0 =
      r_tar_header(
        name: parse_string(r_header_v7(v7, :name)),
        mode: parse_numeric(r_header_v7(v7, :mode)),
        uid: parse_numeric(r_header_v7(v7, :uid)),
        gid: parse_numeric(r_header_v7(v7, :gid)),
        size: parse_numeric(r_header_v7(v7, :size)),
        mtime: mtime,
        atime: mtime,
        ctime: mtime,
        typeflag: r_header_v7(v7, :typeflag),
        linkname: parse_string(r_header_v7(v7, :linkname))
      )

    typeflag = r_tar_header(header0, :typeflag)

    header1 =
      cond do
        format > 1 ->
          unpack_modern(format, v7, bin, header0)

        true ->
          name = r_tar_header(header0, :name)
          r_tar_header(header0, name: safe_join_path('', name))
      end

    headerOnly = is_header_only_type(typeflag)

    header2 =
      cond do
        headerOnly ->
          r_tar_header(header1, size: 0)

        true ->
          header1
      end

    cond do
      typeflag === ?S ->
        gnu = to_gnu(v7, bin)
        realSize = parse_numeric(r_header_gnu(gnu, :real_size))
        {sparsemap, reader2} = parse_sparse_map(gnu, reader)
        header3 = r_tar_header(header2, size: realSize)
        {header3, new_sparse_file_reader(reader2, sparsemap, realSize)}

      true ->
        fileReader =
          r_reg_file_reader(
            handle: reader,
            num_bytes: r_tar_header(header2, :size),
            size: r_tar_header(header2, :size),
            pos: 0
          )

        {header2, fileReader}
    end
  end

  defp unpack_modern(format, r_header_v7() = v7, bin, r_tar_header() = header0)
       when is_binary(bin) do
    typeflag = r_tar_header(header0, :typeflag)
    ustar = to_ustar(v7, bin)

    h0 =
      r_tar_header(header0,
        uname: parse_string(r_header_ustar(ustar, :uname)),
        gname: parse_string(r_header_ustar(ustar, :gname))
      )

    h1 =
      cond do
        typeflag === ?3 or typeflag === ?4 ->
          ma = parse_numeric(r_header_ustar(ustar, :devmajor))
          mi = parse_numeric(r_header_ustar(ustar, :devminor))
          r_tar_header(h0, devmajor: ma, devminor: mi)

        true ->
          h0
      end

    {prefix, h2} =
      case format do
        4 ->
          {parse_string(r_header_ustar(ustar, :prefix)), h1}

        3 ->
          star = to_star(v7, bin)
          prefix0 = parse_string(r_header_star(star, :prefix))
          atime0 = r_header_star(star, :atime)
          atime = parse_numeric(atime0)
          ctime0 = r_header_star(star, :ctime)
          ctime = parse_numeric(ctime0)
          {prefix0, r_tar_header(h1, atime: atime, ctime: ctime)}

        _ ->
          {'', h1}
      end

    name = r_tar_header(h2, :name)
    r_tar_header(h2, name: safe_join_path(prefix, name))
  end

  defp safe_join_path([], name) do
    :filename.join([name])
  end

  defp safe_join_path(prefix, []) do
    :filename.join([prefix])
  end

  defp safe_join_path(prefix, name) do
    :filename.join(prefix, name)
  end

  defp new_sparse_file_reader(reader, sparsemap, realSize) do
    true = validate_sparse_entries(sparsemap, realSize)

    r_sparse_file_reader(
      handle: reader,
      num_bytes: realSize,
      pos: 0,
      size: realSize,
      sparse_map: sparsemap
    )
  end

  defp validate_sparse_entries(entries, realSize) do
    validate_sparse_entries(entries, realSize, 0, 0)
  end

  defp validate_sparse_entries([], _RealSize, _I, _LastOffset) do
    true
  end

  defp validate_sparse_entries([r_sparse_entry() = entry | rest], realSize, i, lastOffset) do
    offset = r_sparse_entry(entry, :offset)
    numBytes = r_sparse_entry(entry, :num_bytes)

    cond do
      offset > 1 <<< (63 - 1 - numBytes) ->
        throw({:error, {:invalid_sparse_map_entry, :offset_too_large}})

      offset + numBytes > realSize ->
        throw({:error, {:invalid_sparse_map_entry, :offset_too_large}})

      i > 0 and lastOffset > offset ->
        throw({:error, {:invalid_sparse_map_entry, :overlapping_offsets}})

      true ->
        :ok
    end

    validate_sparse_entries(rest, realSize, i + 1, offset + numBytes)
  end

  defp parse_sparse_map(r_header_gnu(sparse: sparse), reader)
       when r_sparse_array(sparse, :is_extended) do
    parse_sparse_map(sparse, reader, [])
  end

  defp parse_sparse_map(r_header_gnu(sparse: sparse), reader) do
    {r_sparse_array(sparse, :entries), reader}
  end

  defp parse_sparse_map(r_sparse_array(is_extended: true, entries: entries), reader, acc) do
    case read_block(reader) do
      :eof ->
        throw({:error, :eof})

      {:ok, block, reader2} ->
        sparse2 = to_sparse_array(block)
        parse_sparse_map(sparse2, reader2, entries ++ acc)
    end
  end

  defp parse_sparse_map(r_sparse_array(entries: entries), reader, acc) do
    sorted =
      :lists.sort(
        fn r_sparse_entry(offset: a), r_sparse_entry(offset: b) ->
          a <= b
        end,
        entries ++ acc
      )

    {sorted, reader}
  end

  defp compute_checksum(
         <<h1::size(148)-binary, h2::size(8)-binary, rest::size(512 - 148 - 8)-binary, _::binary>>
       ) do
    c0 = checksum(h1) + byte_size(h2) * ?\s
    c1 = checksum(rest)
    c0 + c1
  end

  defp compute_signed_checksum(
         <<h1::size(148)-binary, h2::size(8)-binary, rest::size(512 - 148 - 8)-binary, _::binary>>
       ) do
    c0 = signed_checksum(h1) + byte_size(h2) * ?\s
    c1 = signed_checksum(rest)
    c0 + c1
  end

  defp checksum(bin) do
    checksum(bin, 0)
  end

  defp checksum(<<a::unsigned, rest::binary>>, sum) do
    checksum(rest, sum + a)
  end

  defp checksum(<<>>, sum) do
    sum
  end

  defp signed_checksum(bin) do
    signed_checksum(bin, 0)
  end

  defp signed_checksum(<<a::signed, rest::binary>>, sum) do
    signed_checksum(rest, sum + a)
  end

  defp signed_checksum(<<>>, sum) do
    sum
  end

  defp parse_numeric(<<>>) do
    0
  end

  defp parse_numeric(<<first, _::binary>> = bin) do
    cond do
      first &&& 128 !== 0 ->
        inv =
          cond do
            first &&& 64 !== 0 ->
              0

            true ->
              255
          end

        bytes = :binary.bin_to_list(bin)

        reducer = fn c, {i, x} ->
          c1 = c ^^^ inv

          c2 =
            cond do
              i === 0 ->
                c1 &&& 127

              true ->
                c1
            end

          cond do
            x >>> 56 > 0 ->
              throw({:error, :integer_overflow})

            true ->
              {i + 1, x <<< 8 ||| c2}
          end
        end

        {_, n} = :lists.foldl(reducer, {0, 0}, bytes)

        cond do
          n >>> 63 > 0 ->
            throw({:error, :integer_overflow})

          true ->
            cond do
              inv === 255 ->
                -1 ^^^ n

              true ->
                n
            end
        end

      true ->
        parse_octal(bin)
    end
  end

  defp parse_octal(bin) when is_binary(bin) do
    do_parse_octal(bin, <<>>)
  end

  defp do_parse_octal(<<>>, <<>>) do
    0
  end

  defp do_parse_octal(<<>>, acc) do
    case :io_lib.fread('~8u', :binary.bin_to_list(acc)) do
      {:error, _} ->
        throw({:error, :invalid_tar_checksum})

      {:ok, [octal], []} ->
        octal

      {:ok, _, _} ->
        throw({:error, :invalid_tar_checksum})
    end
  end

  defp do_parse_octal(<<?\s, rest::binary>>, acc) do
    do_parse_octal(rest, acc)
  end

  defp do_parse_octal(<<0, rest::binary>>, acc) do
    do_parse_octal(rest, acc)
  end

  defp do_parse_octal(<<c, rest::binary>>, acc) do
    do_parse_octal(rest, <<acc::binary, c>>)
  end

  defp parse_string(bin) when is_binary(bin) do
    do_parse_string(bin, <<>>)
  end

  defp do_parse_string(<<>>, acc) do
    case :unicode.characters_to_list(acc) do
      str when is_list(str) ->
        str

      {:incomplete, _Str, _Rest} ->
        :binary.bin_to_list(acc)

      {:error, _Str, _Rest} ->
        throw({:error, {:bad_header, :invalid_string}})
    end
  end

  defp do_parse_string(<<0, _::binary>>, acc) do
    do_parse_string(<<>>, acc)
  end

  defp do_parse_string(<<c, rest::binary>>, acc) do
    do_parse_string(rest, <<acc::binary, c>>)
  end

  defp convert_header(bin, r_reader(pos: pos) = reader)
       when byte_size(bin) === 512 and rem(pos, 512) === 0 do
    case get_format(bin) do
      0 ->
        throw({:error, :bad_header})

      {:ok, format, v7} ->
        unpack_format(format, v7, bin, reader)

      {:error, reason} ->
        throw({:error, {:bad_header, reason}})
    end
  end

  defp convert_header(bin, r_reader(pos: pos)) when byte_size(bin) === 512 do
    throw({:error, :misaligned_read, pos})
  end

  defp convert_header(bin, _Reader) when byte_size(bin) === 0 do
    :eof
  end

  defp convert_header(_Bin, _Reader) do
    throw({:error, :eof})
  end

  defp fileinfo_to_header(name, r_file_info() = fi, link) when is_list(name) do
    baseHeader =
      r_tar_header(
        name: name,
        mtime: r_file_info(fi, :mtime),
        atime: r_file_info(fi, :atime),
        ctime: r_file_info(fi, :ctime),
        mode: r_file_info(fi, :mode),
        uid: r_file_info(fi, :uid),
        gid: r_file_info(fi, :gid),
        typeflag: ?0
      )

    do_fileinfo_to_header(baseHeader, fi, link)
  end

  defp do_fileinfo_to_header(header, r_file_info(size: size, type: :regular), _Link) do
    r_tar_header(header, size: size, typeflag: ?0)
  end

  defp do_fileinfo_to_header(
         r_tar_header(name: name) = header,
         r_file_info(type: :directory),
         _Link
       ) do
    r_tar_header(header, name: name ++ '/', typeflag: ?5)
  end

  defp do_fileinfo_to_header(header, r_file_info(type: :symlink), link) do
    r_tar_header(header, typeflag: ?2, linkname: link)
  end

  defp do_fileinfo_to_header(header, r_file_info(type: :device, mode: mode) = fi, _Link)
       when mode &&& 61440 === 8192 do
    r_tar_header(header,
      typeflag: ?3,
      devmajor: r_file_info(fi, :major_device),
      devminor: r_file_info(fi, :minor_device)
    )
  end

  defp do_fileinfo_to_header(header, r_file_info(type: :device, mode: mode) = fi, _Link)
       when mode &&& 61440 === 24576 do
    r_tar_header(header,
      typeflag: ?4,
      devmajor: r_file_info(fi, :major_device),
      devminor: r_file_info(fi, :minor_device)
    )
  end

  defp do_fileinfo_to_header(header, r_file_info(type: :other, mode: mode), _Link)
       when mode &&& 61440 === 4096 do
    r_tar_header(header, typeflag: ?6)
  end

  defp do_fileinfo_to_header(header, fi, _Link) do
    {:error, {:invalid_file_type, r_tar_header(header, :name), fi}}
  end

  defp is_ascii(str) when is_list(str) do
    not :lists.any(
      fn char ->
        char >= 128
      end,
      str
    )
  end

  defp is_ascii(bin) when is_binary(bin) do
    is_ascii1(bin)
  end

  defp is_ascii1(<<>>) do
    true
  end

  defp is_ascii1(<<c, _Rest::binary>>) when c >= 128 do
    false
  end

  defp is_ascii1(<<_, rest::binary>>) do
    is_ascii1(rest)
  end

  defp to_ascii(str) when is_list(str) do
    case is_ascii(str) do
      true ->
        :unicode.characters_to_binary(str)

      false ->
        chars =
          :lists.filter(
            fn char ->
              char < 128
            end,
            str
          )

        :unicode.characters_to_binary(chars)
    end
  end

  defp to_ascii(bin) when is_binary(bin) do
    to_ascii(bin, <<>>)
  end

  defp to_ascii(<<>>, acc) do
    acc
  end

  defp to_ascii(<<c, rest::binary>>, acc) when c < 128 do
    to_ascii(rest, <<acc::binary, c>>)
  end

  defp to_ascii(<<_, rest::binary>>, acc) do
    to_ascii(rest, acc)
  end

  defp is_header_only_type(?2) do
    true
  end

  defp is_header_only_type(?1) do
    true
  end

  defp is_header_only_type(?5) do
    true
  end

  defp is_header_only_type(_) do
    false
  end

  defp foldl_read(r_reader(access: :read) = reader, fun, accu, r_read_opts() = opts)
       when is_function(fun, 4) do
    case foldl_read0(reader, fun, accu, opts) do
      {:ok, result, _Reader2} ->
        result

      {:error, _} = err ->
        err
    end
  end

  defp foldl_read(r_reader(access: access), _Fun, _Accu, _Opts) do
    {:error, {:read_mode_expected, access}}
  end

  defp foldl_read(tarName, fun, accu, r_read_opts() = opts)
       when is_function(fun, 4) do
    try do
      open(tarName, [:read | r_read_opts(opts, :open_mode)])
    catch
      err ->
        err
    else
      {:ok, r_reader(access: :read) = reader} ->
        try do
          foldl_read(reader, fun, accu, opts)
        after
          _ = close(reader)
        end

      {:error, _} = err ->
        err
    end
  end

  defp foldl_read0(reader, fun, accu, opts) do
    try do
      foldl_read1(fun, accu, reader, opts, %{})
    catch
      {:error, {reason, format, args}} ->
        read_verbose(opts, format, args)
        {:error, reason}

      err ->
        err
    else
      {:ok, _, _} = ok ->
        ok
    end
  end

  defp foldl_read1(fun, accu0, reader0, opts, extraHeaders) do
    {:ok, reader1} = skip_unread(reader0)

    case get_header(reader1) do
      :eof ->
        fun.(:eof, reader1, opts, accu0)

      {header, reader2} ->
        case r_tar_header(header, :typeflag) do
          ?x ->
            {extraHeaders2, reader3} = parse_pax(reader2)
            extraHeaders3 = :maps.merge(extraHeaders, extraHeaders2)
            foldl_read1(fun, accu0, reader3, opts, extraHeaders3)

          ?L ->
            {realName, reader3} = get_real_name(reader2)
            extraHeaders2 = :maps.put("path", parse_string(realName), extraHeaders)
            foldl_read1(fun, accu0, reader3, opts, extraHeaders2)

          ?K ->
            {realName, reader3} = get_real_name(reader2)
            extraHeaders2 = :maps.put("linkpath", parse_string(realName), extraHeaders)
            foldl_read1(fun, accu0, reader3, opts, extraHeaders2)

          _ ->
            header1 = merge_pax(header, extraHeaders)
            {:ok, newAccu, reader3} = fun.(header1, reader2, opts, accu0)
            foldl_read1(fun, newAccu, reader3, opts, %{})
        end
    end
  end

  defp merge_pax(header, extraHeaders)
       when is_map(extraHeaders) do
    do_merge_pax(header, :maps.to_list(extraHeaders))
  end

  defp do_merge_pax(header, []) do
    header
  end

  defp do_merge_pax(header, [{"path", path} | rest]) do
    do_merge_pax(
      r_tar_header(header, name: :unicode.characters_to_list(path)),
      rest
    )
  end

  defp do_merge_pax(header, [{"linkpath", linkPath} | rest]) do
    do_merge_pax(
      r_tar_header(header, linkname: :unicode.characters_to_list(linkPath)),
      rest
    )
  end

  defp do_merge_pax(header, [{"gname", gname} | rest]) do
    do_merge_pax(
      r_tar_header(header, gname: :unicode.characters_to_list(gname)),
      rest
    )
  end

  defp do_merge_pax(header, [{"uname", uname} | rest]) do
    do_merge_pax(
      r_tar_header(header, uname: :unicode.characters_to_list(uname)),
      rest
    )
  end

  defp do_merge_pax(header, [{"uid", uid} | rest]) do
    uid2 = :erlang.binary_to_integer(uid)
    do_merge_pax(r_tar_header(header, uid: uid2), rest)
  end

  defp do_merge_pax(header, [{"gid", gid} | rest]) do
    gid2 = :erlang.binary_to_integer(gid)
    do_merge_pax(r_tar_header(header, gid: gid2), rest)
  end

  defp do_merge_pax(header, [{"atime", atime} | rest]) do
    atime2 = parse_pax_time(atime)
    do_merge_pax(r_tar_header(header, atime: atime2), rest)
  end

  defp do_merge_pax(header, [{"mtime", mtime} | rest]) do
    mtime2 = parse_pax_time(mtime)
    do_merge_pax(r_tar_header(header, mtime: mtime2), rest)
  end

  defp do_merge_pax(header, [{"ctime", ctime} | rest]) do
    ctime2 = parse_pax_time(ctime)
    do_merge_pax(r_tar_header(header, ctime: ctime2), rest)
  end

  defp do_merge_pax(header, [{"size", size} | rest]) do
    size2 = :erlang.binary_to_integer(size)
    do_merge_pax(r_tar_header(header, size: size2), rest)
  end

  defp do_merge_pax(
         header,
         [{<<"SCHILY.xattr.", _Key::binary>>, _Value} | rest]
       ) do
    do_merge_pax(header, rest)
  end

  defp do_merge_pax(header, [_Ignore | rest]) do
    do_merge_pax(header, rest)
  end

  defp parse_pax_time(bin) when is_binary(bin) do
    totalNano =
      case :binary.split(bin, [<<?.>>]) do
        [secondsStr, nanoStr0] ->
          seconds = :erlang.binary_to_integer(secondsStr)

          cond do
            byte_size(nanoStr0) < 9 ->
              paddingN = 9 - byte_size(nanoStr0)
              padding = :binary.copy(<<?0>>, paddingN)
              nanoStr1 = <<nanoStr0::binary, padding::binary>>
              nano = :erlang.binary_to_integer(nanoStr1)
              seconds * 1_000_000_000 + nano

            byte_size(nanoStr0) > 9 ->
              nanoStr1 = binary_part(nanoStr0, 0, 9)
              nano = :erlang.binary_to_integer(nanoStr1)
              seconds * 1_000_000_000 + nano

            true ->
              seconds * 1_000_000_000 + :erlang.binary_to_integer(nanoStr0)
          end

        [secondsStr] ->
          :erlang.binary_to_integer(secondsStr) * 1_000_000_000
      end

    micro = div(totalNano, 1000)
    mega = div(micro, 1_000_000_000_000)
    secs = div(micro, 1_000_000) - mega * 1_000_000
    secs
  end

  defp parse_pax(r_reg_file_reader(handle: handle, num_bytes: 0)) do
    {%{}, handle}
  end

  defp parse_pax(r_reg_file_reader(handle: handle0, num_bytes: numBytes)) do
    case do_read(handle0, numBytes) do
      {:ok, bytes, handle1} ->
        do_parse_pax(handle1, bytes, %{})

      {:error, _} = err ->
        throw(err)
    end
  end

  defp do_parse_pax(reader, <<>>, headers) do
    {headers, reader}
  end

  defp do_parse_pax(reader, bin, headers) do
    {key, value, residual} = parse_pax_record(bin)
    newHeaders = :maps.put(key, value, headers)
    do_parse_pax(reader, residual, newHeaders)
  end

  defp parse_pax_record(bin) when is_binary(bin) do
    case :binary.split(bin, [<<?\n>>]) do
      [record, residual] ->
        case :binary.split(record, [<<?\s>>], [:trim_all]) do
          [_Len, record1] ->
            case :binary.split(record1, [<<?=>>], [:trim_all]) do
              [attrName, attrValue] ->
                {attrName, attrValue, residual}

              _Other ->
                throw({:error, :malformed_pax_record})
            end

          _Other ->
            throw({:error, :malformed_pax_record})
        end

      _Other ->
        throw({:error, :malformed_pax_record})
    end
  end

  defp get_real_name(r_reg_file_reader(handle: handle, num_bytes: 0)) do
    {'', handle}
  end

  defp get_real_name(r_reg_file_reader(handle: handle0, num_bytes: numBytes)) do
    case do_read(handle0, numBytes) do
      {:ok, realName, handle1} ->
        {realName, handle1}

      {:error, _} = err ->
        throw(err)
    end
  end

  defp get_real_name(r_sparse_file_reader(num_bytes: numBytes) = reader0) do
    case do_read(reader0, numBytes) do
      {:ok, realName, reader1} ->
        {realName, reader1}

      {:error, _} = err ->
        throw(err)
    end
  end

  defp skip_file(r_reg_file_reader(handle: handle0, pos: pos, size: size) = reader) do
    padding = skip_padding(size)
    absPos = r_reader(handle0, :pos) + (size - pos) + padding

    case do_position(handle0, absPos) do
      {:ok, _, handle1} ->
        r_reg_file_reader(reader, handle: handle1, num_bytes: 0, pos: size)

      err ->
        throw(err)
    end
  end

  defp skip_file(r_sparse_file_reader(pos: pos, size: size) = reader) do
    case do_read(reader, size - pos) do
      {:ok, _, reader2} ->
        reader2

      err ->
        throw(err)
    end
  end

  defp skip_padding(0) do
    0
  end

  defp skip_padding(size) when rem(size, 512) === 0 do
    0
  end

  defp skip_padding(size) when size <= 512 do
    512 - size
  end

  defp skip_padding(size) do
    512 - rem(size, 512)
  end

  defp skip_unread(r_reader(pos: pos) = reader0) when rem(pos, 512) > 0 do
    padding = skip_padding(pos + 512)
    absPos = pos + padding

    case do_position(reader0, absPos) do
      {:ok, _, reader1} ->
        {:ok, reader1}

      err ->
        throw(err)
    end
  end

  defp skip_unread(r_reader() = reader) do
    {:ok, reader}
  end

  defp skip_unread(r_reg_file_reader(handle: handle, num_bytes: 0)) do
    skip_unread(handle)
  end

  defp skip_unread(r_reg_file_reader() = reader) do
    r_reg_file_reader(handle: handle) = skip_file(reader)
    {:ok, handle}
  end

  defp skip_unread(r_sparse_file_reader(handle: handle, num_bytes: 0)) do
    skip_unread(handle)
  end

  defp skip_unread(r_sparse_file_reader() = reader) do
    r_sparse_file_reader(handle: handle) = skip_file(reader)
    {:ok, handle}
  end

  defp write_extracted_element(
         r_tar_header(name: name, typeflag: type),
         bin,
         r_read_opts(output: :memory) = opts
       ) do
    case typeflag(type) do
      :regular ->
        read_verbose(opts, 'x ~ts~n', [name])
        {:ok, {name, bin}}

      _ ->
        :ok
    end
  end

  defp write_extracted_element(r_tar_header(name: name0) = header, bin, opts) do
    name1 = make_safe_path(name0, opts)

    created =
      case typeflag(r_tar_header(header, :typeflag)) do
        :regular ->
          create_regular(name1, name0, bin, opts)

        :directory ->
          read_verbose(opts, 'x ~ts~n', [name0])
          create_extracted_dir(name1, opts)

        :symlink ->
          read_verbose(opts, 'x ~ts~n', [name0])
          linkName = safe_link_name(header, opts)
          create_symlink(name1, linkName, opts)

        device when device === :char or device === :block ->
          create_regular(name1, name0, <<>>, opts)

        :fifo ->
          create_regular(name1, name0, <<>>, opts)

        other ->
          read_verbose(opts, 'x ~ts - unsupported type ~p~n', [name0, other])
          :not_written
      end

    case created do
      :ok ->
        set_extracted_file_info(name1, header)

      :not_written ->
        :ok
    end
  end

  defp make_safe_path([?/ | path], opts) do
    make_safe_path(path, opts)
  end

  defp make_safe_path(path0, r_read_opts(cwd: cwd)) do
    case :filelib.safe_relative_path(path0, cwd) do
      :unsafe ->
        throw({:error, {path0, :unsafe_path}})

      path ->
        :filename.absname(path, cwd)
    end
  end

  defp safe_link_name(r_tar_header(linkname: path0), r_read_opts(cwd: cwd)) do
    case :filelib.safe_relative_path(path0, cwd) do
      :unsafe ->
        throw({:error, {path0, :unsafe_symlink}})

      path ->
        path
    end
  end

  defp create_regular(name, nameInArchive, bin, opts) do
    case write_extracted_file(name, bin, opts) do
      :not_written ->
        read_verbose(opts, 'x ~ts - exists, not created~n', [nameInArchive])
        :not_written

      ok ->
        read_verbose(opts, 'x ~ts~n', [nameInArchive])
        ok
    end
  end

  defp create_extracted_dir(name, _Opts) do
    case :file.make_dir(name) do
      :ok ->
        :ok

      {:error, :enotsup} ->
        :not_written

      {:error, :eexist} ->
        :not_written

      {:error, :enoent} ->
        make_dirs(name, :dir)

      {:error, reason} ->
        throw({:error, reason})
    end
  end

  defp create_symlink(name, linkname, opts) do
    case :file.make_symlink(linkname, name) do
      :ok ->
        :ok

      {:error, :enoent} ->
        :ok = make_dirs(name, :file)
        create_symlink(name, linkname, opts)

      {:error, :eexist} ->
        :not_written

      {:error, :enotsup} ->
        read_verbose(opts, 'x ~ts - symbolic links not supported~n', [name])
        :not_written

      {:error, reason} ->
        throw({:error, reason})
    end
  end

  defp write_extracted_file(name, bin, opts) do
    write =
      case r_read_opts(opts, :keep_old_files) do
        true ->
          case :file.read_file_info(name) do
            {:ok, _} ->
              false

            _ ->
              true
          end

        false ->
          true
      end

    case write do
      true ->
        write_file(name, bin)

      false ->
        :not_written
    end
  end

  defp write_file(name, bin) do
    case :file.write_file(name, bin) do
      :ok ->
        :ok

      {:error, :enoent} ->
        case make_dirs(name, :file) do
          :ok ->
            write_file(name, bin)

          {:error, reason} ->
            throw({:error, reason})
        end

      {:error, reason} ->
        throw({:error, reason})
    end
  end

  defp set_extracted_file_info(_, r_tar_header(typeflag: ?2)) do
    :ok
  end

  defp set_extracted_file_info(_, r_tar_header(typeflag: ?1)) do
    :ok
  end

  defp set_extracted_file_info(name, r_tar_header(typeflag: ?3) = header) do
    set_device_info(name, header)
  end

  defp set_extracted_file_info(name, r_tar_header(typeflag: ?4) = header) do
    set_device_info(name, header)
  end

  defp set_extracted_file_info(name, r_tar_header(mtime: mtime, mode: mode)) do
    info = r_file_info(mode: mode, mtime: mtime)
    :file.write_file_info(name, info, [{:time, :posix}])
  end

  defp set_device_info(name, r_tar_header() = header) do
    mtime = r_tar_header(header, :mtime)
    mode = r_tar_header(header, :mode)
    devmajor = r_tar_header(header, :devmajor)
    devminor = r_tar_header(header, :devminor)
    info = r_file_info(mode: mode, mtime: mtime, major_device: devmajor, minor_device: devminor)
    :file.write_file_info(name, info)
  end

  defp make_dirs(name, :file) do
    :filelib.ensure_dir(name)
  end

  defp make_dirs(name, :dir) do
    :filelib.ensure_dir(:filename.join(name, '*'))
  end

  defp read_verbose(r_read_opts(verbose: true), format, args) do
    :io.format(format, args)
  end

  defp read_verbose(_, _, _) do
    :ok
  end

  defp add_verbose(r_add_opts(verbose: true), format, args) do
    :io.format(format, args)
  end

  defp add_verbose(_, _, _) do
    :ok
  end

  defp do_write(r_reader(handle: handle, func: fun) = reader0, data)
       when is_function(fun, 2) do
    case fun.(:write, {handle, data}) do
      :ok ->
        {:ok, pos, reader1} = do_position(reader0, {:cur, 0})
        {:ok, r_reader(reader1, pos: pos)}

      {:error, _} = err ->
        err
    end
  end

  defp do_copy(r_reader(func: fun) = reader, source, r_add_opts(chunk_size: 0) = opts)
       when is_function(fun, 2) do
    do_copy(reader, source, r_add_opts(opts, chunk_size: 65536))
  end

  defp do_copy(r_reader(func: fun) = reader, source, r_add_opts(chunk_size: chunkSize))
       when is_function(fun, 2) do
    case :file.open(source, [:read, :binary]) do
      {:ok, sourceFd} ->
        case copy_chunked(reader, sourceFd, chunkSize, 0) do
          {:ok, _Copied, _Reader2} = ok ->
            _ = :file.close(sourceFd)
            ok

          err ->
            _ = :file.close(sourceFd)
            throw(err)
        end

      err ->
        throw(err)
    end
  end

  defp copy_chunked(r_reader() = reader, source, chunkSize, copied) do
    case :file.read(source, chunkSize) do
      {:ok, bin} ->
        {:ok, reader2} = do_write(reader, bin)
        copy_chunked(reader2, source, chunkSize, copied + byte_size(bin))

      :eof ->
        {:ok, copied, reader}

      other ->
        other
    end
  end

  defp do_position(r_reader(handle: handle, func: fun) = reader, pos)
       when is_function(fun, 2) do
    case fun.(:position, {handle, pos}) do
      {:ok, newPos} ->
        {:ok, absPos} = fun.(:position, {handle, {:cur, 0}})
        {:ok, newPos, r_reader(reader, pos: absPos)}

      other ->
        other
    end
  end

  defp do_read(
         r_reg_file_reader(handle: handle, pos: pos, size: size) = reader,
         len
       ) do
    numBytes = size - pos

    actualLen =
      cond do
        numBytes - len < 0 ->
          numBytes

        true ->
          len
      end

    case do_read(handle, actualLen) do
      {:ok, bin, handle2} ->
        newPos = pos + actualLen
        numBytes2 = size - newPos
        reader1 = r_reg_file_reader(reader, handle: handle2, pos: newPos, num_bytes: numBytes2)
        {:ok, bin, reader1}

      other ->
        other
    end
  end

  defp do_read(r_sparse_file_reader() = reader, len) do
    do_sparse_read(reader, len)
  end

  defp do_read(
         r_reader(pos: pos, handle: handle, func: fun) = reader,
         len
       )
       when is_function(fun, 2) do
    case fun.(:read2, {handle, len}) do
      {:ok, list} when is_list(list) ->
        bin = :erlang.list_to_binary(list)
        newPos = pos + byte_size(bin)
        {:ok, bin, r_reader(reader, pos: newPos)}

      {:ok, bin} when is_binary(bin) ->
        newPos = pos + byte_size(bin)
        {:ok, bin, r_reader(reader, pos: newPos)}

      other ->
        other
    end
  end

  defp do_sparse_read(reader, len) do
    do_sparse_read(reader, len, <<>>)
  end

  defp do_sparse_read(
         r_sparse_file_reader(
           sparse_map: [
             r_sparse_entry(num_bytes: 0)
             | entries
           ]
         ) = reader0,
         len,
         acc
       ) do
    reader1 = r_sparse_file_reader(reader0, sparse_map: entries)
    do_sparse_read(reader1, len, acc)
  end

  defp do_sparse_read(
         r_sparse_file_reader(sparse_map: [], pos: pos, size: size) = reader0,
         len,
         acc
       )
       when pos < size do
    {:ok, bin, reader1} = read_sparse_hole(reader0, size, len)
    do_sparse_read(reader1, len - byte_size(bin), <<acc::binary, bin::binary>>)
  end

  defp do_sparse_read(r_sparse_file_reader(sparse_map: []) = reader, _Len, acc) do
    {:ok, acc, reader}
  end

  defp do_sparse_read(r_sparse_file_reader() = reader, 0, acc) do
    {:ok, acc, reader}
  end

  defp do_sparse_read(
         r_sparse_file_reader(
           sparse_map: [r_sparse_entry(offset: offset) | _],
           pos: pos
         ) = reader0,
         len,
         acc
       )
       when pos < offset do
    {:ok, bin, reader1} = read_sparse_hole(reader0, offset, offset - pos)
    do_sparse_read(reader1, len - byte_size(bin), <<acc::binary, bin::binary>>)
  end

  defp do_sparse_read(
         r_sparse_file_reader(
           sparse_map: [entry | entries],
           pos: pos
         ) = reader0,
         len,
         acc
       ) do
    endPos = r_sparse_entry(entry, :offset) + r_sparse_entry(entry, :num_bytes)
    numBytes = endPos - pos

    actualLen =
      cond do
        len > numBytes ->
          numBytes

        true ->
          len
      end

    case do_read(r_sparse_file_reader(reader0, :handle), actualLen) do
      {:ok, bin, handle} ->
        bytesRead = byte_size(bin)
        actualEndPos = pos + bytesRead

        reader1 =
          cond do
            actualEndPos === endPos ->
              r_sparse_file_reader(reader0, sparse_map: entries)

            true ->
              reader0
          end

        size = r_sparse_file_reader(reader1, :size)
        numBytes2 = size - actualEndPos

        reader2 =
          r_sparse_file_reader(reader1, handle: handle, pos: actualEndPos, num_bytes: numBytes2)

        do_sparse_read(reader2, len - byte_size(bin), <<acc::binary, bin::binary>>)

      other ->
        other
    end
  end

  defp read_sparse_hole(r_sparse_file_reader(pos: pos) = reader, offset, len) do
    n = offset - pos

    n2 =
      cond do
        n > len ->
          len

        true ->
          n
      end

    bin = <<0::size(n2)-unit(8)>>
    numBytes = r_sparse_file_reader(reader, :size) - (pos + n2)
    {:ok, bin, r_sparse_file_reader(reader, num_bytes: numBytes, pos: pos + n2)}
  end

  defp do_close(r_reader(handle: handle, func: fun))
       when is_function(fun, 2) do
    fun.(:close, handle)
  end

  defp extract_opts(list) do
    extract_opts(list, default_options())
  end

  defp table_opts(list) do
    read_opts(list, default_options())
  end

  defp default_options() do
    {:ok, cwd} = :file.get_cwd()
    r_read_opts(cwd: cwd)
  end

  defp extract_opts([:keep_old_files | rest], opts) do
    extract_opts(rest, r_read_opts(opts, keep_old_files: true))
  end

  defp extract_opts([{:cwd, cwd} | rest], opts) do
    extract_opts(rest, r_read_opts(opts, cwd: cwd))
  end

  defp extract_opts([{:files, files} | rest], opts) do
    set = :ordsets.from_list(files)
    extract_opts(rest, r_read_opts(opts, files: set))
  end

  defp extract_opts([:memory | rest], opts) do
    extract_opts(rest, r_read_opts(opts, output: :memory))
  end

  defp extract_opts(
         [:compressed | rest],
         opts = r_read_opts(open_mode: openMode)
       ) do
    extract_opts(
      rest,
      r_read_opts(opts, open_mode: [:compressed | openMode])
    )
  end

  defp extract_opts(
         [:cooked | rest],
         opts = r_read_opts(open_mode: openMode)
       ) do
    extract_opts(
      rest,
      r_read_opts(opts, open_mode: [:cooked | openMode])
    )
  end

  defp extract_opts([:verbose | rest], opts) do
    extract_opts(rest, r_read_opts(opts, verbose: true))
  end

  defp extract_opts([other | rest], opts) do
    extract_opts(rest, read_opts([other], opts))
  end

  defp extract_opts([], opts) do
    opts
  end

  defp read_opts(
         [:compressed | rest],
         opts = r_read_opts(open_mode: openMode)
       ) do
    read_opts(
      rest,
      r_read_opts(opts, open_mode: [:compressed | openMode])
    )
  end

  defp read_opts(
         [:cooked | rest],
         opts = r_read_opts(open_mode: openMode)
       ) do
    read_opts(
      rest,
      r_read_opts(opts, open_mode: [:cooked | openMode])
    )
  end

  defp read_opts([:verbose | rest], opts) do
    read_opts(rest, r_read_opts(opts, verbose: true))
  end

  defp read_opts([_ | rest], opts) do
    read_opts(rest, opts)
  end

  defp read_opts([], opts) do
    opts
  end
end
