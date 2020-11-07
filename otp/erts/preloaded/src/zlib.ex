defmodule :m_zlib do
  use Bitwise
  require Record

  Record.defrecord(:r_zlib_opts, :zlib_opts,
    stream: :undefined,
    method: :undefined,
    input_chunk_size: :undefined,
    output_chunk_size: :undefined,
    flush: :undefined
  )

  def on_load() do
    case :erlang.load_nif(
           :erlang.atom_to_list(:zlib),
           0
         ) do
      :ok ->
        :ok
    end
  end

  def open() do
    open_nif()
  end

  defp open_nif() do
    :erlang.nif_error(:undef)
  end

  def close(z) do
    close_nif(z)
  end

  defp close_nif(_Z) do
    :erlang.nif_error(:undef)
  end

  def set_controlling_process(z, pid) do
    set_controller_nif(z, pid)
  end

  defp set_controller_nif(_Z, _Pid) do
    :erlang.nif_error(:undef)
  end

  def deflateInit(z) do
    deflateInit(z, :default)
  end

  def deflateInit(z, level) do
    deflateInit(z, level, :deflated, 15, 8, :default)
  end

  def deflateInit(z, level, method, windowBits, memLevel, strategy) do
    deflateInit_nif(
      z,
      arg_level(level),
      arg_method(method),
      arg_bitsz(windowBits),
      arg_mem(memLevel),
      arg_strategy(strategy)
    )
  end

  defp deflateInit_nif(_Z, _Level, _Method, _WindowBits, _MemLevel, _Strategy) do
    :erlang.nif_error(:undef)
  end

  def deflateSetDictionary(z, dictionary) do
    deflateSetDictionary_nif(z, dictionary)
  end

  defp deflateSetDictionary_nif(_Z, _Dictionary) do
    :erlang.nif_error(:undef)
  end

  def deflateReset(z) do
    deflateReset_nif(z)
  end

  defp deflateReset_nif(_Z) do
    :erlang.nif_error(:undef)
  end

  def deflateParams(z, level0, strategy0) do
    level = arg_level(level0)
    strategy = arg_strategy(strategy0)
    progress = deflate(z, <<>>, :sync)

    case deflateParams_nif(z, level, strategy) do
      :ok ->
        save_progress(z, :deflate, progress)
        :ok

      other ->
        other
    end
  end

  defp deflateParams_nif(_Z, _Level, _Strategy) do
    :erlang.nif_error(:undef)
  end

  def deflate(z, data) do
    deflate(z, data, :none)
  end

  def deflate(z, data, flush) do
    progress = restore_progress(z, :deflate)
    enqueue_input(z, data)

    append_iolist(
      progress,
      dequeue_all_chunks(z, deflate_opts(flush))
    )
  end

  defp deflate_opts(flush) do
    r_zlib_opts(
      method: &deflate_nif/4,
      input_chunk_size: 8 <<< 10,
      output_chunk_size: 8 <<< 10,
      flush: arg_flush(flush)
    )
  end

  defp deflate_nif(_Z, _InputChSize, _OutputChSize, _Flush) do
    :erlang.nif_error(:undef)
  end

  def deflateEnd(z) do
    deflateEnd_nif(z)
  end

  defp deflateEnd_nif(_Z) do
    :erlang.nif_error(:undef)
  end

  def inflateInit(z) do
    inflateInit(z, 15)
  end

  def inflateInit(z, windowBits) do
    inflateInit(z, windowBits, :cut)
  end

  def inflateInit(z, windowBits, eoSBehavior) do
    inflateInit_nif(z, arg_bitsz(windowBits), arg_eos_behavior(eoSBehavior))
  end

  defp inflateInit_nif(_Z, _WindowBits, _EoSBehavior) do
    :erlang.nif_error(:undef)
  end

  def inflateSetDictionary(z, dictionary) do
    inflateSetDictionary_nif(z, dictionary)
  end

  defp inflateSetDictionary_nif(_Z, _Dictionary) do
    :erlang.nif_error(:undef)
  end

  def inflateGetDictionary(z) do
    case inflateGetDictionary_nif(z) do
      dictionary when is_binary(dictionary) ->
        dictionary

      :not_supported ->
        :erlang.error(:enotsup)
    end
  end

  defp inflateGetDictionary_nif(_Z) do
    :erlang.nif_error(:undef)
  end

  def inflateReset(z) do
    inflateReset_nif(z)
  end

  defp inflateReset_nif(_Z) do
    :erlang.nif_error(:undef)
  end

  def inflate(z, data) do
    inflate(z, data, [])
  end

  def inflate(z, data, options) do
    enqueue_input(z, data)
    result = dequeue_all_chunks(z, inflate_opts())

    case proplist_get_value(options, :exception_on_need_dict, true) do
      true ->
        exception_on_need_dict(z, result)

      false ->
        result
    end
  end

  defp inflate_nif(_Z, _InputChSize, _OutputChSize, _Flush) do
    :erlang.nif_error(:undef)
  end

  defp inflate_opts() do
    r_zlib_opts(
      method: &inflate_nif/4,
      input_chunk_size: 8 <<< 10,
      output_chunk_size: 16 <<< 10,
      flush: arg_flush(:none)
    )
  end

  def inflateChunk(z, data) do
    enqueue_input(z, data)
    inflateChunk(z)
  end

  def inflateChunk(z) do
    opts0 = inflate_opts()
    opts = r_zlib_opts(opts0, output_chunk_size: getBufSize(z))
    result0 = dequeue_next_chunk(z, opts)
    result1 = exception_on_need_dict(z, result0)
    yield_inflateChunk(z, result1)
  end

  defp yield_inflateChunk(_Z, {:continue, output}) do
    {:more, :lists.flatten(output)}
  end

  defp yield_inflateChunk(_Z, {:finished, output}) do
    :lists.flatten(output)
  end

  defp exception_on_need_dict(z, {:need_dictionary, adler, output}) do
    progress = restore_progress(z, :inflate)
    save_progress(z, :inflate, append_iolist(progress, output))
    :erlang.error({:need_dictionary, adler})
  end

  defp exception_on_need_dict(z, {mark, output}) do
    progress = restore_progress(z, :inflate)
    {mark, append_iolist(progress, output)}
  end

  defp exception_on_need_dict(z, output)
       when is_list(output) or
              is_binary(output) do
    progress = restore_progress(z, :inflate)
    append_iolist(progress, output)
  end

  def safeInflate(z, data) do
    enqueue_input(z, data)
    dequeue_next_chunk(z, inflate_opts())
  end

  def inflateEnd(z) do
    inflateEnd_nif(z)
  end

  defp inflateEnd_nif(_Z) do
    :erlang.nif_error(:undef)
  end

  def setBufSize(z, size)
      when is_integer(size) and size > 16 and
             size < 1 <<< 24 do
    setBufSize_nif(z, size)
  end

  def setBufSize(_Z, _Size) do
    :erlang.error(:badarg)
  end

  defp setBufSize_nif(_Z, _Size) do
    :erlang.nif_error(:undef)
  end

  def getBufSize(z) do
    getBufSize_nif(z)
  end

  defp getBufSize_nif(_Z) do
    :erlang.nif_error(:undef)
  end

  def crc32(z) do
    crc32_nif(z)
  end

  defp crc32_nif(_Z) do
    :erlang.nif_error(:undef)
  end

  def crc32(z, data) when is_reference(z) do
    :erlang.crc32(data)
  end

  def crc32(_Z, _Data) do
    :erlang.error(:badarg)
  end

  def crc32(z, cRC, data) when is_reference(z) do
    :erlang.crc32(cRC, data)
  end

  def crc32(_Z, _CRC, _Data) do
    :erlang.error(:badarg)
  end

  def crc32_combine(z, cRC1, cRC2, size2) when is_reference(z) do
    :erlang.crc32_combine(cRC1, cRC2, size2)
  end

  def crc32_combine(_Z, _CRC1, _CRC2, _Size2) do
    :erlang.error(:badarg)
  end

  def adler32(z, data) when is_reference(z) do
    :erlang.adler32(data)
  end

  def adler32(_Z, _Data) do
    :erlang.error(:badarg)
  end

  def adler32(z, adler, data) when is_reference(z) do
    :erlang.adler32(adler, data)
  end

  def adler32(_Z, _Adler, _Data) do
    :erlang.error(:badarg)
  end

  def adler32_combine(z, adler1, adler2, size2) when is_reference(z) do
    :erlang.adler32_combine(adler1, adler2, size2)
  end

  def adler32_combine(_Z, _Adler1, _Adler2, _Size2) do
    :erlang.error(:badarg)
  end

  def compress(data) do
    z = open()

    bs =
      try do
        deflateInit(z, :default)
        b = deflate(z, data, :finish)
        deflateEnd(z)
        b
      after
        close(z)
      end

    :erlang.iolist_to_binary(bs)
  end

  def uncompress(data) do
    try do
      :erlang.iolist_size(data)
    catch
      _, _ ->
        :erlang.error(:badarg)
    else
      size ->
        cond do
          size >= 8 ->
            z = open()

            bs =
              try do
                inflateInit(z)
                b = inflate(z, data)
                inflateEnd(z)
                b
              after
                close(z)
              end

            :erlang.iolist_to_binary(bs)

          true ->
            :erlang.error(:data_error)
        end
    end
  end

  def zip(data) do
    z = open()

    bs =
      try do
        deflateInit(z, :default, :deflated, -15, 8, :default)
        b = deflate(z, data, :finish)
        deflateEnd(z)
        b
      after
        close(z)
      end

    :erlang.iolist_to_binary(bs)
  end

  def unzip(data) do
    z = open()

    bs =
      try do
        inflateInit(z, -15)
        b = inflate(z, data)
        inflateEnd(z)
        b
      after
        close(z)
      end

    :erlang.iolist_to_binary(bs)
  end

  def gzip(data) do
    z = open()

    bs =
      try do
        deflateInit(z, :default, :deflated, 16 + 15, 8, :default)
        b = deflate(z, data, :finish)
        deflateEnd(z)
        b
      after
        close(z)
      end

    :erlang.iolist_to_binary(bs)
  end

  def gunzip(data) do
    z = open()

    bs =
      try do
        inflateInit(z, 16 + 15, :reset)
        b = inflate(z, data)
        inflateEnd(z)
        b
      after
        close(z)
      end

    :erlang.iolist_to_binary(bs)
  end

  defp dequeue_all_chunks(z, opts) do
    dequeue_all_chunks_1(z, opts, [])
  end

  defp dequeue_all_chunks_1(z, opts, output) do
    case dequeue_next_chunk(z, opts) do
      {:need_dictionary, _, _} = needDict ->
        needDict

      {:continue, chunk} ->
        dequeue_all_chunks_1(z, opts, append_iolist(output, chunk))

      {:finished, chunk} ->
        append_iolist(output, chunk)
    end
  end

  defp dequeue_next_chunk(z, opts) do
    method = r_zlib_opts(opts, :method)
    iChSz = r_zlib_opts(opts, :input_chunk_size)
    oChSz = r_zlib_opts(opts, :output_chunk_size)
    flush = r_zlib_opts(opts, :flush)
    method.(z, iChSz, oChSz, flush)
  end

  defp append_iolist([], d) when is_list(d) do
    d
  end

  defp append_iolist([], d) do
    [d]
  end

  defp append_iolist(iO, []) do
    iO
  end

  defp append_iolist(iO, [d]) do
    [iO, d]
  end

  defp append_iolist(iO, d) do
    [iO, d]
  end

  defp save_progress(z, kind, output) do
    :ok = setStash_nif(z, {kind, output})
  end

  defp restore_progress(z, kind) do
    case getStash_nif(z) do
      {:ok, {^kind, output}} ->
        :ok = clearStash_nif(z)
        output

      :empty ->
        []
    end
  end

  defp clearStash_nif(_Z) do
    :erlang.nif_error(:undef)
  end

  defp setStash_nif(_Z, _Term) do
    :erlang.nif_error(:undef)
  end

  defp getStash_nif(_Z) do
    :erlang.nif_error(:undef)
  end

  defp proplist_get_value([], _Name, defVal) do
    defVal
  end

  defp proplist_get_value([{name, value} | _Opts], name, _DefVal) do
    value
  end

  defp proplist_get_value([_Head | opts], name, defVal) do
    proplist_get_value(opts, name, defVal)
  end

  defp arg_flush(:none) do
    0
  end

  defp arg_flush(:sync) do
    2
  end

  defp arg_flush(:full) do
    3
  end

  defp arg_flush(:finish) do
    4
  end

  defp arg_flush(_) do
    :erlang.error(:bad_flush_mode)
  end

  defp arg_level(:none) do
    0
  end

  defp arg_level(:best_speed) do
    1
  end

  defp arg_level(:best_compression) do
    9
  end

  defp arg_level(:default) do
    -1
  end

  defp arg_level(level)
       when is_integer(level) and
              level >= 0 and level <= 9 do
    level
  end

  defp arg_level(_) do
    :erlang.error(:bad_compression_level)
  end

  defp arg_strategy(:filtered) do
    1
  end

  defp arg_strategy(:huffman_only) do
    2
  end

  defp arg_strategy(:rle) do
    3
  end

  defp arg_strategy(:default) do
    0
  end

  defp arg_strategy(_) do
    :erlang.error(:bad_compression_strategy)
  end

  defp arg_method(:deflated) do
    8
  end

  defp arg_method(_) do
    :erlang.error(:bad_compression_method)
  end

  defp arg_eos_behavior(:error) do
    0
  end

  defp arg_eos_behavior(:reset) do
    1
  end

  defp arg_eos_behavior(:cut) do
    2
  end

  defp arg_eos_behavior(_) do
    :erlang.error(:bad_eos_behavior)
  end

  defp arg_bitsz(bits)
       when is_integer(bits) and ((8 <= bits and bits < 48) or (-15 <= bits and bits <= -8)) do
    bits
  end

  defp arg_bitsz(_) do
    :erlang.error(:bad_windowbits)
  end

  defp arg_mem(level)
       when is_integer(level) and
              1 <= level and level <= 9 do
    level
  end

  defp arg_mem(_) do
    :erlang.error(:bad_memlevel)
  end

  defp enqueue_input(z, iOData) do
    enqueue_input_1(z, :erlang.iolist_to_iovec(iOData))
  end

  defp enqueue_input_1(_Z, []) do
    :ok
  end

  defp enqueue_input_1(z, iOVec) do
    case enqueue_nif(z, iOVec) do
      {:continue, remainder} ->
        enqueue_input_1(z, remainder)

      :ok ->
        :ok
    end
  end

  defp enqueue_nif(_Z, _IOVec) do
    :erlang.nif_error(:undef)
  end
end
