defmodule :m_file_io_server do
  use Bitwise
  require Record

  Record.defrecord(:r_state, :state,
    handle: :undefined,
    owner: :undefined,
    mref: :undefined,
    buf: :undefined,
    read_mode: :undefined,
    unic: :undefined
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

  def format_error({_Line, :file_io_server, reason}) do
    :io_lib.format('~w', [reason])
  end

  def format_error({_Line, mod, reason}) do
    mod.format_error(reason)
  end

  def format_error(:invalid_unicode) do
    :io_lib.format('cannot translate from UTF-8', [])
  end

  def format_error(errorId) do
    :erl_posix_msg.message(errorId)
  end

  def start(owner, fileName, modeList)
      when (is_pid(owner) and
              is_list(fileName)) or
             (is_binary(fileName) and
                is_list(modeList)) do
    do_start(:spawn, owner, fileName, modeList)
  end

  def start_link(owner, fileName, modeList)
      when (is_pid(owner) and
              is_list(fileName)) or
             (is_binary(fileName) and
                is_list(modeList)) do
    do_start(:spawn_link, owner, fileName, modeList)
  end

  defp do_start(spawn, owner, fileName, modeList) do
    self = self()
    ref = make_ref()
    utag = :erlang.dt_spread_tag(true)

    pid =
      apply(:erlang, spawn, [
        fn ->
          :erlang.dt_restore_tag(utag)

          case parse_options(modeList) do
            {readMode, unicodeMode, opts0} ->
              opts = maybe_add_read_ahead(readMode, opts0)

              case :raw_file_io.open(fileName, [:raw | opts]) do
                {:error, reason} = error ->
                  send(self, {ref, error})
                  exit(reason)

                {:ok, handle} ->
                  m = :erlang.monitor(:process, owner)
                  send(self, {ref, :ok})

                  server_loop(
                    r_state(
                      handle: handle,
                      owner: owner,
                      mref: m,
                      buf: <<>>,
                      read_mode: readMode,
                      unic: unicodeMode
                    )
                  )
              end

            {:error, reason1} = error1 ->
              send(self, {ref, error1})
              exit(reason1)
          end
        end
      ])

    :erlang.dt_restore_tag(utag)
    mref = :erlang.monitor(:process, pid)

    receive do
      {^ref, {:error, _Reason} = error} ->
        :erlang.demonitor(mref, [:flush])
        error

      {^ref, :ok} ->
        :erlang.demonitor(mref)

        receive do
          {:DOWN, ^mref, _, _, reason} ->
            {:error, reason}
        after
          0 ->
            {:ok, pid}
        end

      {:DOWN, ^mref, _, _, reason} ->
        {:error, reason}
    end
  end

  defp parse_options(list) do
    parse_options(expand_encoding(list), :list, :latin1, [])
  end

  defp parse_options([], :list, uni, acc) do
    {:list, uni, [:binary | :lists.reverse(acc)]}
  end

  defp parse_options([], :binary, uni, acc) do
    {:binary, uni, :lists.reverse(acc)}
  end

  defp parse_options([{:encoding, encoding} | t], rMode, _, acc) do
    case valid_enc(encoding) do
      {:ok, expandedEnc} ->
        parse_options(t, rMode, expandedEnc, acc)

      {:error, _Reason} = error ->
        error
    end
  end

  defp parse_options([:binary | t], _, uni, acc) do
    parse_options(t, :binary, uni, [:binary | acc])
  end

  defp parse_options([h | t], r, u, acc) do
    parse_options(t, r, u, [h | acc])
  end

  defp expand_encoding([]) do
    []
  end

  defp expand_encoding([:latin1 | t]) do
    [{:encoding, :latin1} | expand_encoding(t)]
  end

  defp expand_encoding([:unicode | t]) do
    [{:encoding, :unicode} | expand_encoding(t)]
  end

  defp expand_encoding([h | t]) do
    [h | expand_encoding(t)]
  end

  defp valid_enc(:latin1) do
    {:ok, :latin1}
  end

  defp valid_enc(:utf8) do
    {:ok, :unicode}
  end

  defp valid_enc(:unicode) do
    {:ok, :unicode}
  end

  defp valid_enc(:utf16) do
    {:ok, {:utf16, :big}}
  end

  defp valid_enc({:utf16, :big}) do
    {:ok, {:utf16, :big}}
  end

  defp valid_enc({:utf16, :little}) do
    {:ok, {:utf16, :little}}
  end

  defp valid_enc(:utf32) do
    {:ok, {:utf32, :big}}
  end

  defp valid_enc({:utf32, :big}) do
    {:ok, {:utf32, :big}}
  end

  defp valid_enc({:utf32, :little}) do
    {:ok, {:utf32, :little}}
  end

  defp valid_enc(_Other) do
    {:error, :badarg}
  end

  defp maybe_add_read_ahead(:binary, opts) do
    opts
  end

  defp maybe_add_read_ahead(:list, opts) do
    p = fn
      :read_ahead ->
        true

      {:read_ahead, _} ->
        true

      :append ->
        true

      :exclusive ->
        true

      :write ->
        true

      _ ->
        false
    end

    case :lists.any(p, opts) do
      false ->
        [{:read_ahead, 4096} | opts]

      true ->
        opts
    end
  end

  defp server_loop(r_state(mref: mref) = state) do
    receive do
      {:file_request, from, replyAs, request}
      when is_pid(from) ->
        case file_request(request, state) do
          {:reply, reply, newState} ->
            _ = file_reply(from, replyAs, reply)
            server_loop(newState)

          {:error, reply, newState} ->
            _ = file_reply(from, replyAs, reply)
            server_loop(newState)

          {:stop, reason, reply, _NewState} ->
            _ = file_reply(from, replyAs, reply)
            exit(reason)
        end

      {:io_request, from, replyAs, request} when is_pid(from) ->
        case io_request(request, state) do
          {:reply, reply, newState} ->
            _ = io_reply(from, replyAs, reply)
            server_loop(newState)

          {:error, reply, newState} ->
            _ = io_reply(from, replyAs, reply)
            server_loop(newState)

          {:stop, reason, reply, _NewState} ->
            _ = io_reply(from, replyAs, reply)
            exit(reason)
        end

      {:DOWN, ^mref, _, _, reason} ->
        exit(reason)

      _ ->
        server_loop(state)
    end
  end

  defp file_reply(from, replyAs, reply) do
    send(from, {:file_reply, replyAs, reply})
  end

  defp io_reply(from, replyAs, reply) do
    send(from, {:io_reply, replyAs, reply})
  end

  defp file_request(
         {:advise, offset, length, advise},
         r_state(handle: handle) = state
       ) do
    case apply(r_file_descriptor(handle, :module), :advise, [handle, offset, length, advise]) do
      {:error, reason} = reply ->
        {:stop, reason, reply, state}

      reply ->
        {:reply, reply, state}
    end
  end

  defp file_request(
         {:allocate, offset, length},
         r_state(handle: handle) = state
       ) do
    reply = apply(r_file_descriptor(handle, :module), :allocate, [handle, offset, length])
    {:reply, reply, state}
  end

  defp file_request({:pread, at, sz}, state)
       when at === :cur or
              at === {:cur, 0} do
    case get_chars(sz, :latin1, state) do
      {:reply, reply, newState}
      when is_list(reply) or
             is_binary(reply) ->
        {:reply, {:ok, reply}, newState}

      other ->
        other
    end
  end

  defp file_request(
         {:pread, at, sz},
         r_state(handle: handle, buf: buf) = state
       ) do
    case position(handle, at, buf) do
      {:error, _} = reply ->
        {:error, reply, state}

      _ ->
        case get_chars(sz, :latin1, r_state(state, buf: <<>>)) do
          {:reply, reply, newState}
          when is_list(reply) or
                 is_binary(reply) ->
            {:reply, {:ok, reply}, newState}

          other ->
            other
        end
    end
  end

  defp file_request({:pwrite, at, data}, r_state(buf: <<>>) = state)
       when at === :cur or at === {:cur, 0} do
    put_chars(data, :latin1, state)
  end

  defp file_request(
         {:pwrite, at, data},
         r_state(handle: handle, buf: buf) = state
       ) do
    case position(handle, at, buf) do
      {:error, _} = reply ->
        {:error, reply, state}

      _ ->
        put_chars(data, :latin1, state)
    end
  end

  defp file_request(:datasync, r_state(handle: handle) = state) do
    case apply(r_file_descriptor(handle, :module), :datasync, [handle]) do
      {:error, reason} = reply ->
        {:stop, reason, reply, state}

      reply ->
        {:reply, reply, state}
    end
  end

  defp file_request(:sync, r_state(handle: handle) = state) do
    case apply(r_file_descriptor(handle, :module), :sync, [handle]) do
      {:error, reason} = reply ->
        {:stop, reason, reply, state}

      reply ->
        {:reply, reply, state}
    end
  end

  defp file_request(:close, r_state(handle: handle) = state) do
    case apply(r_file_descriptor(handle, :module), :close, [handle]) do
      {:error, reason} = reply ->
        {:stop, reason, reply, r_state(state, buf: <<>>)}

      reply ->
        {:stop, :normal, reply, r_state(state, buf: <<>>)}
    end
  end

  defp file_request(
         {:position, at},
         r_state(handle: handle, buf: buf) = state
       ) do
    case position(handle, at, buf) do
      {:error, _} = reply ->
        {:error, reply, state}

      reply ->
        std_reply(reply, state)
    end
  end

  defp file_request(:truncate, r_state(handle: handle) = state) do
    case apply(r_file_descriptor(handle, :module), :truncate, [handle]) do
      {:error, reason} = reply ->
        {:stop, reason, reply, r_state(state, buf: <<>>)}

      reply ->
        std_reply(reply, state)
    end
  end

  defp file_request(
         {:read_handle_info, opts},
         r_state(handle: handle) = state
       ) do
    case apply(r_file_descriptor(handle, :module), :read_handle_info, [handle, opts]) do
      {:error, reason} = reply ->
        {:stop, reason, reply, state}

      reply ->
        {:reply, reply, state}
    end
  end

  defp file_request(unknown, r_state() = state) do
    reason = {:request, unknown}
    {:error, {:error, reason}, state}
  end

  defp std_reply({:error, _} = reply, state) do
    {:error, reply, r_state(state, buf: <<>>)}
  end

  defp std_reply(reply, state) do
    {:reply, reply, r_state(state, buf: <<>>)}
  end

  defp io_request(
         {:put_chars, enc, chars},
         r_state(buf: <<>>) = state
       ) do
    put_chars(chars, enc, state)
  end

  defp io_request(
         {:put_chars, enc, chars},
         r_state(handle: handle, buf: buf) = state
       ) do
    case position(handle, :cur, buf) do
      {:error, reason} = reply ->
        {:stop, reason, reply, state}

      _ ->
        put_chars(chars, enc, r_state(state, buf: <<>>))
    end
  end

  defp io_request(
         {:put_chars, enc, mod, func, args},
         r_state() = state
       ) do
    case (try do
            apply(mod, func, args)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      chars when is_list(chars) or is_binary(chars) ->
        io_request({:put_chars, enc, chars}, state)

      _ ->
        {:error, {:error, func}, state}
    end
  end

  defp io_request(
         {:get_until, enc, _Prompt, mod, func, xtraArgs},
         r_state() = state
       ) do
    get_chars(:io_lib, :get_until, {mod, func, xtraArgs}, enc, state)
  end

  defp io_request({:get_chars, enc, _Prompt, n}, r_state() = state) do
    get_chars(n, enc, state)
  end

  defp io_request(
         {:get_line, outEnc, _Prompt},
         r_state(buf: buf, read_mode: mode, unic: inEnc) = state0
       ) do
    try do
      workEnc =
        case inEnc do
          {_, _} ->
            outEnc

          _ ->
            inEnc
        end

      {res, state} = get_line(:start, convert_enc(buf, inEnc, workEnc), workEnc, state0)
      {:reply, cast(res, mode, workEnc, outEnc), state}
    catch
      :exit, exError ->
        {:stop, exError, {:error, exError}, r_state(state0, buf: <<>>)}
    end
  end

  defp io_request({:setopts, opts}, r_state() = state)
       when is_list(opts) do
    setopts(opts, state)
  end

  defp io_request(:getopts, r_state() = state) do
    getopts(state)
  end

  defp io_request({:put_chars, chars}, r_state() = state) do
    io_request({:put_chars, :latin1, chars}, state)
  end

  defp io_request({:put_chars, mod, func, args}, r_state() = state) do
    io_request(
      {:put_chars, :latin1, mod, func, args},
      state
    )
  end

  defp io_request(
         {:get_until, _Prompt, mod, func, xtraArgs},
         r_state() = state
       ) do
    io_request(
      {:get_until, :latin1, _Prompt, mod, func, xtraArgs},
      state
    )
  end

  defp io_request({:get_chars, _Prompt, n}, r_state() = state) do
    io_request({:get_chars, :latin1, _Prompt, n}, state)
  end

  defp io_request({:get_line, _Prompt}, r_state() = state) do
    io_request({:get_line, :latin1, _Prompt}, state)
  end

  defp io_request({:requests, requests}, r_state() = state)
       when is_list(requests) do
    io_request_loop(requests, {:reply, :ok, state})
  end

  defp io_request(unknown, r_state() = state) do
    reason = {:request, unknown}
    {:error, {:error, reason}, state}
  end

  defp io_request_loop([], result) do
    result
  end

  defp io_request_loop(
         [_Request | _Tail],
         {:stop, _Reason, _Reply, _State} = result
       ) do
    result
  end

  defp io_request_loop(
         [_Request | _Tail],
         {:error, _Reply, _State} = result
       ) do
    result
  end

  defp io_request_loop([request | tail], {:reply, _Reply, state}) do
    io_request_loop(tail, io_request(request, state))
  end

  defp put_chars(chars, :latin1, r_state(handle: handle, unic: :latin1) = state) do
    newState = r_state(state, buf: <<>>)

    case apply(r_file_descriptor(handle, :module), :write, [handle, chars]) do
      {:error, reason} = reply ->
        {:stop, reason, reply, newState}

      reply ->
        {:reply, reply, newState}
    end
  end

  defp put_chars(chars, inEncoding, r_state(handle: handle, unic: outEncoding) = state) do
    newState = r_state(state, buf: <<>>)

    case :unicode.characters_to_binary(chars, inEncoding, outEncoding) do
      bin when is_binary(bin) ->
        case apply(r_file_descriptor(handle, :module), :write, [handle, bin]) do
          {:error, reason} = reply ->
            {:stop, reason, reply, newState}

          reply ->
            {:reply, reply, newState}
        end

      {:error, _, _} ->
        {:stop, :no_translation, {:error, {:no_translation, inEncoding, outEncoding}}, newState}
    end
  end

  defp get_line(
         s,
         {<<>>, cont},
         outEnc,
         r_state(handle: handle, read_mode: mode, unic: inEnc) = state
       ) do
    case apply(r_file_descriptor(handle, :module), :read, [handle, read_size(mode)]) do
      {:ok, bin} ->
        get_line(s, convert_enc([cont, bin], inEnc, outEnc), outEnc, state)

      :eof ->
        get_line(s, {:eof, cont}, outEnc, state)

      {:error, reason} = error ->
        {:stop, reason, error, state}
    end
  end

  defp get_line(s0, {buf, bCont}, outEnc, r_state(unic: inEnc) = state) do
    case :io_lib.collect_line(s0, buf, outEnc, []) do
      {:stop, result, cont0} ->
        {cont, <<>>} = convert_enc(cont0, outEnc, inEnc)
        {result, r_state(state, buf: cast_binary([cont, bCont]))}

      s ->
        get_line(s, {<<>>, bCont}, outEnc, state)
    end
  end

  defp convert_enc(bins, enc, enc) do
    {cast_binary(bins), <<>>}
  end

  defp convert_enc(:eof, _, _) do
    {<<>>, <<>>}
  end

  defp convert_enc(bin, inEnc, outEnc) do
    case :unicode.characters_to_binary(bin, inEnc, outEnc) do
      res when is_binary(res) ->
        {res, <<>>}

      {:incomplete, res, cont} ->
        {res, cont}

      {:error, _, _} ->
        exit({:no_translation, inEnc, outEnc})
    end
  end

  defp get_chars(0, enc, r_state(read_mode: readMode, unic: inEncoding) = state) do
    {:reply, cast(<<>>, readMode, inEncoding, enc), state}
  end

  defp get_chars(n, enc, r_state(buf: buf, read_mode: readMode, unic: :latin1) = state)
       when is_integer(n) and n > 0 and
              n <= byte_size(buf) do
    {b1, b2} = :erlang.split_binary(buf, n)
    {:reply, cast(b1, readMode, :latin1, enc), r_state(state, buf: b2)}
  end

  defp get_chars(n, enc, r_state(buf: buf, read_mode: readMode, unic: :latin1) = state)
       when is_integer(n) and n > 0 and
              n <= byte_size(buf) do
    {b1, b2} = :erlang.split_binary(buf, n)
    {:reply, cast(b1, readMode, :latin1, enc), r_state(state, buf: b2)}
  end

  defp get_chars(
         n,
         outEnc,
         r_state(handle: handle, buf: buf, read_mode: readMode, unic: :latin1) = state
       )
       when is_integer(n) and n > 0 do
    bufSize = byte_size(buf)
    needSize = n - bufSize
    size = :erlang.max(needSize, 8 * 1024)

    case apply(r_file_descriptor(handle, :module), :read, [handle, size]) do
      {:ok, b} ->
        cond do
          bufSize + byte_size(b) < n ->
            std_reply(cat(buf, b, readMode, :latin1, outEnc), state)

          true ->
            {b1, b2} = :erlang.split_binary(b, needSize)
            {:reply, cat(buf, b1, readMode, :latin1, outEnc), r_state(state, buf: b2)}
        end

      :eof when bufSize === 0 ->
        {:reply, :eof, state}

      :eof ->
        std_reply(cast(buf, readMode, :latin1, outEnc), state)

      {:error, reason} = error ->
        {:stop, reason, error, r_state(state, buf: <<>>)}
    end
  end

  defp get_chars(
         n,
         outEnc,
         r_state(handle: handle, buf: buf, read_mode: readMode, unic: inEncoding) = state
       )
       when is_integer(n) and n > 0 do
    try do
      {bufCount, splitPos} = count_and_find(buf, n, inEncoding)

      case bufCount >= n do
        true ->
          {b1, b2} =
            case splitPos do
              :none ->
                {buf, <<>>}

              _ ->
                :erlang.split_binary(buf, splitPos)
            end

          {:reply, cast(b1, readMode, inEncoding, outEnc), r_state(state, buf: b2)}

        false ->
          needSize = (n - bufCount) * 4
          size = :erlang.max(needSize, 8 * 1024)

          case apply(r_file_descriptor(handle, :module), :read, [handle, size]) do
            {:ok, b} ->
              newBuf = :erlang.list_to_binary([buf, b])
              {newCount, newSplit} = count_and_find(newBuf, n, inEncoding)

              case newCount >= n do
                true ->
                  {b01, b02} =
                    case newSplit do
                      :none ->
                        {newBuf, <<>>}

                      _ ->
                        :erlang.split_binary(newBuf, newSplit)
                    end

                  {:reply, cast(b01, readMode, inEncoding, outEnc), r_state(state, buf: b02)}

                false ->
                  std_reply(
                    cast(newBuf, readMode, inEncoding, outEnc),
                    r_state(state, buf: <<>>)
                  )
              end

            :eof when bufCount === 0 ->
              {:reply, :eof, state}

            :eof ->
              std_reply(
                cast(buf, readMode, inEncoding, outEnc),
                r_state(state, buf: <<>>)
              )

            {:error, reason} = error ->
              {:stop, reason, error, r_state(state, buf: <<>>)}
          end
      end
    catch
      :exit, exError ->
        {:stop, exError, {:error, exError}, r_state(state, buf: <<>>)}
    end
  end

  defp get_chars(_N, _, r_state() = state) do
    {:error, {:error, :get_chars}, state}
  end

  defp get_chars(mod, func, xtraArg, outEnc, r_state(buf: <<>>) = state) do
    get_chars_empty(mod, func, xtraArg, :start, outEnc, state)
  end

  defp get_chars(mod, func, xtraArg, outEnc, r_state(buf: buf) = state) do
    get_chars_apply(mod, func, xtraArg, :start, outEnc, r_state(state, buf: <<>>), buf)
  end

  defp get_chars_empty(
         mod,
         func,
         xtraArg,
         s,
         :latin1,
         r_state(handle: handle, read_mode: readMode, unic: :latin1) = state
       ) do
    case apply(r_file_descriptor(handle, :module), :read, [handle, read_size(readMode)]) do
      {:ok, bin} ->
        get_chars_apply(mod, func, xtraArg, s, :latin1, state, bin)

      :eof ->
        get_chars_apply(mod, func, xtraArg, s, :latin1, state, :eof)

      {:error, reason} = error ->
        {:stop, reason, error, state}
    end
  end

  defp get_chars_empty(
         mod,
         func,
         xtraArg,
         s,
         outEnc,
         r_state(handle: handle, read_mode: readMode) = state
       ) do
    case apply(r_file_descriptor(handle, :module), :read, [handle, read_size(readMode)]) do
      {:ok, bin} ->
        get_chars_apply(mod, func, xtraArg, s, outEnc, state, bin)

      :eof ->
        get_chars_apply(mod, func, xtraArg, s, outEnc, state, :eof)

      {:error, reason} = error ->
        {:stop, reason, error, state}
    end
  end

  defp get_chars_notempty(
         mod,
         func,
         xtraArg,
         s,
         outEnc,
         r_state(handle: handle, read_mode: readMode, buf: b) = state
       ) do
    case apply(r_file_descriptor(handle, :module), :read, [handle, read_size(readMode)]) do
      {:ok, bin} ->
        get_chars_apply(mod, func, xtraArg, s, outEnc, state, :erlang.list_to_binary([b, bin]))

      :eof ->
        case b do
          <<>> ->
            get_chars_apply(mod, func, xtraArg, s, outEnc, state, :eof)

          _ ->
            {:stop, :invalid_unicode, invalid_unicode_error(mod, func, xtraArg, s), state}
        end

      {:error, reason} = error ->
        {:stop, reason, error, state}
    end
  end

  defp get_chars_apply(
         mod,
         func,
         xtraArg,
         s0,
         :latin1,
         r_state(read_mode: readMode, unic: :latin1) = state,
         data0
       ) do
    data1 =
      case readMode do
        :list when is_binary(data0) ->
          :erlang.binary_to_list(data0)

        _ ->
          data0
      end

    case (try do
            apply(mod, func, [s0, data1, :latin1, xtraArg])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:stop, result, buf} ->
        {:reply, result, r_state(state, buf: cast_binary(buf))}

      {:EXIT, reason} ->
        {:stop, reason, {:error, err_func(mod, func, xtraArg)}, state}

      s1 ->
        get_chars_empty(mod, func, xtraArg, s1, :latin1, state)
    end
  end

  defp get_chars_apply(
         mod,
         func,
         xtraArg,
         s0,
         outEnc,
         r_state(read_mode: readMode, unic: inEnc) = state,
         data0
       ) do
    try do
      {data1, newBuff} =
        case readMode do
          :list when is_binary(data0) ->
            case :unicode.characters_to_list(
                   data0,
                   inEnc
                 ) do
              {tag, decoded, rest}
              when (decoded !== [] and
                      tag === :error) or
                     (decoded !== [] and
                        tag === :incomplete) ->
                {decoded, :erlang.iolist_to_binary(rest)}

              {:error, [], _} ->
                exit(:invalid_unicode)

              {:incomplete, [], r} ->
                {[], r}

              list when is_list(list) ->
                {list, <<>>}
            end

          :binary when is_binary(data0) ->
            case :unicode.characters_to_binary(data0, inEnc, outEnc) do
              {tag2, decoded2, rest2}
              when (decoded2 !== <<>> and
                      tag2 === :error) or
                     (decoded2 !== <<>> and
                        tag2 === :incomplete) ->
                {decoded2, :erlang.iolist_to_binary(rest2)}

              {:error, <<>>, _} ->
                exit(:invalid_unicode)

              {:incomplete, <<>>, r} ->
                {<<>>, r}

              binary when is_binary(binary) ->
                {binary, <<>>}
            end

          _ ->
            {data0, <<>>}
        end

      case (try do
              apply(mod, func, [s0, data1, outEnc, xtraArg])
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:stop, result, buf} ->
          {:reply, result,
           r_state(state,
             buf:
               cond do
                 is_binary(buf) ->
                   :erlang.list_to_binary([
                     :unicode.characters_to_binary(
                       buf,
                       outEnc,
                       inEnc
                     ),
                     newBuff
                   ])

                 is_list(buf) ->
                   :erlang.list_to_binary([
                     :unicode.characters_to_binary(
                       buf,
                       :unicode,
                       inEnc
                     ),
                     newBuff
                   ])

                 true ->
                   newBuff
               end
           )}

        {:EXIT, reason} ->
          {:stop, reason, {:error, err_func(mod, func, xtraArg)}, state}

        s1 ->
          get_chars_notempty(mod, func, xtraArg, s1, outEnc, r_state(state, buf: newBuff))
      end
    catch
      :exit, exReason ->
        {:stop, exReason, invalid_unicode_error(mod, func, xtraArg, s0), state}

      :error, errReason ->
        {:stop, errReason, {:error, err_func(mod, func, xtraArg)}, state}
    end
  end

  defp invalid_unicode_error(mod, func, xtraArg, s) do
    try do
      {:erl_scan, :tokens, _Args} = xtraArg
      location = :erl_scan.continuation_location(s)
      {:error, {location, :file_io_server, :invalid_unicode}, location}
    catch
      _, _ ->
        {:error, err_func(mod, func, xtraArg)}
    end
  end

  defp err_func(:io_lib, :get_until, {_, f, _}) do
    f
  end

  defp setopts(opts0, state) do
    opts =
      :proplists.unfold(
        :proplists.substitute_negations(
          [{:list, :binary}],
          expand_encoding(opts0)
        )
      )

    case check_valid_opts(opts) do
      true ->
        do_setopts(opts, state)

      false ->
        {:error, {:error, :enotsup}, state}
    end
  end

  defp check_valid_opts([]) do
    true
  end

  defp check_valid_opts([{:binary, _} | t]) do
    check_valid_opts(t)
  end

  defp check_valid_opts([{:encoding, _Enc} | t]) do
    check_valid_opts(t)
  end

  defp check_valid_opts(_) do
    false
  end

  defp do_setopts(opts, state) do
    case valid_enc(:proplists.get_value(:encoding, opts, r_state(state, :unic))) do
      {:ok, newUnic} ->
        case :proplists.get_value(:binary, opts) do
          true ->
            {:reply, :ok, r_state(state, read_mode: :binary, unic: newUnic)}

          false ->
            {:reply, :ok, r_state(state, read_mode: :list, unic: newUnic)}

          :undefined ->
            {:reply, :ok, r_state(state, unic: newUnic)}
        end

      _ ->
        {:error, {:error, :badarg}, state}
    end
  end

  defp getopts(r_state(read_mode: rM, unic: unic) = state) do
    bin = {:binary, rM === :binary}
    uni = {:encoding, unic}
    {:reply, [bin, uni], state}
  end

  defp cat(b1, b2, :binary, :latin1, :latin1) do
    :erlang.list_to_binary([b1, b2])
  end

  defp cat(b1, b2, :binary, inEncoding, outEncoding) do
    case :unicode.characters_to_binary([b1, b2], inEncoding, outEncoding) do
      good when is_binary(good) ->
        good

      _ ->
        exit({:no_translation, inEncoding, outEncoding})
    end
  end

  defp cat(b1, b2, :list, :latin1, _) do
    :erlang.binary_to_list(b1) ++ :erlang.binary_to_list(b2)
  end

  defp cast(:eof, _, _, _) do
    :eof
  end

  defp cast(b, :binary, :latin1, :latin1) do
    b
  end

  defp cast(b, :binary, inEncoding, outEncoding) do
    case :unicode.characters_to_binary(b, inEncoding, outEncoding) do
      good when is_binary(good) ->
        good

      _ ->
        exit({:no_translation, inEncoding, outEncoding})
    end
  end

  defp cast(b, :list, :latin1, _) do
    :erlang.binary_to_list(b)
  end

  defp cast(b, :list, inEncoding, outEncoding) do
    try do
      :unicode.characters_to_list(
        :unicode.characters_to_binary(
          b,
          inEncoding,
          outEncoding
        ),
        outEncoding
      )
    catch
      :error, _ ->
        exit({:no_translation, inEncoding, outEncoding})
    end
  end

  defp cast_binary(binary) when is_binary(binary) do
    binary
  end

  defp cast_binary([<<>> | list]) do
    cast_binary(list)
  end

  defp cast_binary(list) when is_list(list) do
    :erlang.list_to_binary(list)
  end

  defp cast_binary(_EOF) do
    <<>>
  end

  defp read_size(:binary) do
    8 * 1024
  end

  defp read_size(:list) do
    128
  end

  def count_and_find(bin, n, encoding) do
    cafu(
      bin,
      n,
      0,
      0,
      :none,
      case encoding do
        :unicode ->
          :utf8

        oth ->
          oth
      end
    )
  end

  defp cafu(<<>>, 0, count, byteCount, _SavePos, _) do
    {count, byteCount}
  end

  defp cafu(<<>>, _N, count, _ByteCount, savePos, _) do
    {count, savePos}
  end

  defp cafu(<<_::utf8, rest::binary>>, 0, count, byteCount, _SavePos, :utf8) do
    cafu(rest, -1, count + 1, 0, byteCount, :utf8)
  end

  defp cafu(<<_::utf8, rest::binary>>, n, count, _ByteCount, savePos, :utf8)
       when n < 0 do
    cafu(rest, -1, count + 1, 0, savePos, :utf8)
  end

  defp cafu(<<_::utf8, rest::binary>> = whole, n, count, byteCount, savePos, :utf8) do
    delta = byte_size(whole) - byte_size(rest)
    cafu(rest, n - 1, count + 1, byteCount + delta, savePos, :utf8)
  end

  defp cafu(<<_::utf16-big, rest::binary>>, 0, count, byteCount, _SavePos, {:utf16, :big}) do
    cafu(rest, -1, count + 1, 0, byteCount, {:utf16, :big})
  end

  defp cafu(<<_::utf16-big, rest::binary>>, n, count, _ByteCount, savePos, {:utf16, :big})
       when n < 0 do
    cafu(rest, -1, count + 1, 0, savePos, {:utf16, :big})
  end

  defp cafu(<<_::utf16-big, rest::binary>> = whole, n, count, byteCount, savePos, {:utf16, :big}) do
    delta = byte_size(whole) - byte_size(rest)
    cafu(rest, n - 1, count + 1, byteCount + delta, savePos, {:utf16, :big})
  end

  defp cafu(<<_::utf16-little, rest::binary>>, 0, count, byteCount, _SavePos, {:utf16, :little}) do
    cafu(rest, -1, count + 1, 0, byteCount, {:utf16, :little})
  end

  defp cafu(<<_::utf16-little, rest::binary>>, n, count, _ByteCount, savePos, {:utf16, :little})
       when n < 0 do
    cafu(rest, -1, count + 1, 0, savePos, {:utf16, :little})
  end

  defp cafu(
         <<_::utf16-little, rest::binary>> = whole,
         n,
         count,
         byteCount,
         savePos,
         {:utf16, :little}
       ) do
    delta = byte_size(whole) - byte_size(rest)
    cafu(rest, n - 1, count + 1, byteCount + delta, savePos, {:utf16, :little})
  end

  defp cafu(<<_::utf32-big, rest::binary>>, 0, count, byteCount, _SavePos, {:utf32, :big}) do
    cafu(rest, -1, count + 1, 0, byteCount, {:utf32, :big})
  end

  defp cafu(<<_::utf32-big, rest::binary>>, n, count, _ByteCount, savePos, {:utf32, :big})
       when n < 0 do
    cafu(rest, -1, count + 1, 0, savePos, {:utf32, :big})
  end

  defp cafu(<<_::utf32-big, rest::binary>> = whole, n, count, byteCount, savePos, {:utf32, :big}) do
    delta = byte_size(whole) - byte_size(rest)
    cafu(rest, n - 1, count + 1, byteCount + delta, savePos, {:utf32, :big})
  end

  defp cafu(<<_::utf32-little, rest::binary>>, 0, count, byteCount, _SavePos, {:utf32, :little}) do
    cafu(rest, -1, count + 1, 0, byteCount, {:utf32, :little})
  end

  defp cafu(<<_::utf32-little, rest::binary>>, n, count, _ByteCount, savePos, {:utf32, :little})
       when n < 0 do
    cafu(rest, -1, count + 1, 0, savePos, {:utf32, :little})
  end

  defp cafu(
         <<_::utf32-little, rest::binary>> = whole,
         n,
         count,
         byteCount,
         savePos,
         {:utf32, :little}
       ) do
    delta = byte_size(whole) - byte_size(rest)
    cafu(rest, n - 1, count + 1, byteCount + delta, savePos, {:utf32, :little})
  end

  defp cafu(_Other, 0, count, byteCount, _, _) do
    {count, byteCount}
  end

  defp cafu(other, _N, count, 0, savePos, enc) do
    case cbv(enc, other) do
      false ->
        exit(:invalid_unicode)

      _ ->
        {count, savePos}
    end
  end

  defp cafu(other, _N, count, byteCount, :none, enc) do
    case cbv(enc, other) do
      false ->
        exit(:invalid_unicode)

      _ ->
        {count, byteCount}
    end
  end

  defp cafu(other, _N, count, _ByteCount, savePos, enc) do
    case cbv(enc, other) do
      false ->
        exit(:invalid_unicode)

      _ ->
        {count, savePos}
    end
  end

  defp cbv(
         :utf8,
         <<1::size(1), 1::size(1), 0::size(1), _::size(5)>>
       ) do
    1
  end

  defp cbv(
         :utf8,
         <<1::size(1), 1::size(1), 1::size(1), 0::size(1), _::size(4), r::binary>>
       ) do
    case r do
      <<>> ->
        2

      <<1::size(1), 0::size(1), _::size(6)>> ->
        1

      _ ->
        false
    end
  end

  defp cbv(
         :utf8,
         <<1::size(1), 1::size(1), 1::size(1), 1::size(1), 0::size(1), _::size(3), r::binary>>
       ) do
    case r do
      <<>> ->
        3

      <<1::size(1), 0::size(1), _::size(6)>> ->
        2

      <<1::size(1), 0::size(1), _::size(6), 1::size(1), 0::size(1), _::size(6)>> ->
        1

      _ ->
        false
    end
  end

  defp cbv(:utf8, _) do
    false
  end

  defp cbv({:utf16, :big}, <<a::size(8)>>)
       when a <= 215 or a >= 224 do
    1
  end

  defp cbv(
         {:utf16, :big},
         <<54::size(6), _::size(2)>>
       ) do
    3
  end

  defp cbv(
         {:utf16, :big},
         <<54::size(6), _::size(10)>>
       ) do
    2
  end

  defp cbv(
         {:utf16, :big},
         <<54::size(6), _::size(10), 55::size(6), _::size(2)>>
       ) do
    1
  end

  defp cbv({:utf16, :big}, _) do
    false
  end

  defp cbv({:utf16, :little}, <<_::size(8)>>) do
    1
  end

  defp cbv(
         {:utf16, :little},
         <<_::size(8), 54::size(6), _::size(2)>>
       ) do
    2
  end

  defp cbv(
         {:utf16, :little},
         <<_::size(8), 54::size(6), _::size(2), _::size(8)>>
       ) do
    1
  end

  defp cbv({:utf16, :little}, _) do
    false
  end

  defp cbv({:utf32, :big}, <<0::size(8)>>) do
    3
  end

  defp cbv({:utf32, :big}, <<0::size(8), x::size(8)>>)
       when x <= 16 do
    2
  end

  defp cbv(
         {:utf32, :big},
         <<0::size(8), x::size(8), y::size(8)>>
       )
       when (x <= 16 and x > 0) or y <= 215 or y >= 224 do
    1
  end

  defp cbv({:utf32, :big}, _) do
    false
  end

  defp cbv({:utf32, :little}, <<_::size(8)>>) do
    3
  end

  defp cbv(
         {:utf32, :little},
         <<_::size(8), _::size(8)>>
       ) do
    2
  end

  defp cbv(
         {:utf32, :little},
         <<x::size(8), 255::size(8), 0::size(8)>>
       )
       when x === 254 or x === 255 do
    false
  end

  defp cbv(
         {:utf32, :little},
         <<_::size(8), y::size(8), x::size(8)>>
       )
       when (x <= 16 and x > 0) or y <= 215 or y >= 224 do
    1
  end

  defp cbv({:utf32, :little}, _) do
    false
  end

  defp position(handle, at, buf) do
    seekTo =
      case at do
        {:cur, offs} ->
          {:cur, offs - byte_size(buf)}

        :cur ->
          {:cur, -byte_size(buf)}

        _ ->
          at
      end

    apply(r_file_descriptor(handle, :module), :position, [handle, seekTo])
  end
end
