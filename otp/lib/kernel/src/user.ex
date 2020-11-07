defmodule :m_user do
  use Bitwise

  def start() do
    start_port([:eof, :binary])
  end

  def start([[mod, fun] | args]) do
    pid = apply(mod, fun, args)

    id =
      spawn(fn ->
        server(pid)
      end)

    :erlang.register(:user, id)
    id
  end

  def start_out() do
    start_port([:out, :binary])
  end

  defp start_port(portSettings) do
    id =
      spawn(fn ->
        server({:fd, 0, 1}, portSettings)
      end)

    :erlang.register(:user, id)
    id
  end

  def interfaces(user) do
    case :erlang.process_info(user, :dictionary) do
      {:dictionary, dict} ->
        case :lists.keysearch(:shell, 1, dict) do
          {:value, sh = {:shell, shell}} when is_pid(shell) ->
            [sh]

          _ ->
            []
        end

      _ ->
        []
    end
  end

  defp server(pid) when is_pid(pid) do
    :erlang.process_flag(:trap_exit, true)
    :erlang.link(pid)
    run(pid)
  end

  defp server(portName, portSettings) do
    :erlang.process_flag(:trap_exit, true)
    port = :erlang.open_port(portName, portSettings)
    run(port)
  end

  defp run(p) do
    :erlang.put(:read_mode, :list)
    :erlang.put(:encoding, :latin1)

    case :init.get_argument(:noshell) do
      {:ok, [_ | _]} ->
        :erlang.put(:shell, :noshell)
        server_loop(p, :queue.new())

      _ ->
        :erlang.group_leader(self(), self())
        catch_loop(p, start_init_shell())
    end
  end

  defp catch_loop(port, shell) do
    catch_loop(port, shell, :queue.new())
  end

  defp catch_loop(port, shell, q) do
    case (try do
            server_loop(port, q)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      :new_shell ->
        :erlang.exit(shell, :kill)
        catch_loop(port, start_new_shell())

      {:unknown_exit, {^shell, reason}, _} ->
        case reason do
          :normal ->
            put_port("*** ", port)

          _ ->
            put_port("*** ERROR: ", port)
        end

        put_port("Shell process terminated! ***\n", port)
        catch_loop(port, start_new_shell())

      {:unknown_exit, _, q1} ->
        catch_loop(port, shell, q1)

      {:EXIT, r} ->
        exit(r)
    end
  end

  defp link_and_save_shell(shell) do
    :erlang.link(shell)
    :erlang.put(:shell, shell)
    shell
  end

  defp start_init_shell() do
    link_and_save_shell(:shell.start(:init))
  end

  defp start_new_shell() do
    link_and_save_shell(:shell.start())
  end

  defp server_loop(port, q) do
    receive do
      {:io_request, from, replyAs, request} when is_pid(from) ->
        server_loop(
          port,
          do_io_request(request, from, replyAs, port, q)
        )

      {^port, {:data, bytes}} ->
        case :erlang.get(:shell) do
          :noshell ->
            server_loop(port, :queue.snoc(q, bytes))

          _ ->
            case contains_ctrl_g_or_ctrl_c(bytes) do
              false ->
                server_loop(port, :queue.snoc(q, bytes))

              _ ->
                throw(:new_shell)
            end
        end

      {^port, :eof} ->
        :erlang.put(:eof, true)
        server_loop(port, q)

      {:EXIT, ^port, :badsig} ->
        server_loop(port, q)

      {:EXIT, ^port, what} ->
        exit(what)

      {:EXIT, somePid, what} ->
        case :erlang.get(:shell) do
          :noshell ->
            server_loop(port, q)

          _ ->
            throw({:unknown_exit, {somePid, what}, q})
        end

      _Other ->
        server_loop(port, q)
    end
  end

  defp get_fd_geometry(port) do
    case (try do
            :erlang.port_control(port, 100 + 25_889_024, [])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      list when length(list) === 8 ->
        <<w::size(32)-native, h::size(32)-native>> = :erlang.list_to_binary(list)
        {w, h}

      _ ->
        :error
    end
  end

  defp do_io_request(req, from, replyAs, port, q0) do
    case io_request(req, port, q0) do
      {_Status, reply, q1} ->
        _ = io_reply(from, replyAs, reply)
        q1

      {:exit, what} ->
        :ok = send_port(port, :close)
        exit(what)
    end
  end

  defp io_request({:put_chars, :unicode, chars}, port, q) do
    case wrap_characters_to_binary(chars, :unicode, :erlang.get(:encoding)) do
      :error ->
        {:error, {:error, :put_chars}, q}

      bin ->
        put_chars(bin, port, q)
    end
  end

  defp io_request({:put_chars, :unicode, mod, func, args}, port, q) do
    case (try do
            apply(mod, func, args)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      data when is_list(data) or is_binary(data) ->
        case wrap_characters_to_binary(data, :unicode, :erlang.get(:encoding)) do
          bin when is_binary(bin) ->
            put_chars(bin, port, q)

          :error ->
            {:error, {:error, :put_chars}, q}
        end

      undef ->
        put_chars(undef, port, q)
    end
  end

  defp io_request({:put_chars, :latin1, chars}, port, q) do
    case (try do
            :unicode.characters_to_binary(chars, :latin1, :erlang.get(:encoding))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      data when is_binary(data) ->
        put_chars(data, port, q)

      _ ->
        {:error, {:error, :put_chars}, q}
    end
  end

  defp io_request({:put_chars, :latin1, mod, func, args}, port, q) do
    case (try do
            apply(mod, func, args)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      data when is_list(data) or is_binary(data) ->
        case (try do
                :unicode.characters_to_binary(data, :latin1, :erlang.get(:encoding))
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          bin when is_binary(bin) ->
            put_chars(bin, port, q)

          _ ->
            {:error, {:error, :put_chars}, q}
        end

      undef ->
        put_chars(undef, port, q)
    end
  end

  defp io_request({:get_chars, enc, prompt, n}, port, q) do
    get_chars(prompt, :io_lib, :collect_chars, n, port, q, enc)
  end

  defp io_request({:get_line, enc, prompt}, port, q) do
    case :erlang.get(:read_mode) do
      :binary ->
        get_line_bin(prompt, port, q, enc)

      _ ->
        get_chars(prompt, :io_lib, :collect_line, [], port, q, enc)
    end
  end

  defp io_request({:get_until, enc, prompt, m, f, as}, port, q) do
    get_chars(prompt, :io_lib, :get_until, {m, f, as}, port, q, enc)
  end

  defp io_request(:getopts, port, q) do
    getopts(port, q)
  end

  defp io_request({:setopts, opts}, port, q) when is_list(opts) do
    setopts(opts, port, q)
  end

  defp io_request({:requests, reqs}, port, q) do
    io_requests(reqs, {:ok, :ok, q}, port)
  end

  defp io_request({:get_geometry, :columns}, port, q) do
    case get_fd_geometry(port) do
      {w, _H} ->
        {:ok, w, q}

      _ ->
        {:error, {:error, :enotsup}, q}
    end
  end

  defp io_request({:get_geometry, :rows}, port, q) do
    case get_fd_geometry(port) do
      {_W, h} ->
        {:ok, h, q}

      _ ->
        {:error, {:error, :enotsup}, q}
    end
  end

  defp io_request({:put_chars, chars}, port, q) do
    io_request({:put_chars, :latin1, chars}, port, q)
  end

  defp io_request({:put_chars, mod, func, args}, port, q) do
    io_request({:put_chars, :latin1, mod, func, args}, port, q)
  end

  defp io_request({:get_chars, prompt, n}, port, q) do
    io_request({:get_chars, :latin1, prompt, n}, port, q)
  end

  defp io_request({:get_line, prompt}, port, q) do
    io_request({:get_line, :latin1, prompt}, port, q)
  end

  defp io_request({:get_until, prompt, m, f, as}, port, q) do
    io_request({:get_until, :latin1, prompt, m, f, as}, port, q)
  end

  defp io_request(r, _Port, q) do
    {:error, {:error, {:request, r}}, q}
  end

  defp io_requests([r | rs], {:ok, _Res, q}, port) do
    io_requests(rs, io_request(r, port, q), port)
  end

  defp io_requests([_ | _], error, _) do
    error
  end

  defp io_requests([], stat, _) do
    stat
  end

  defp put_port(list, port) do
    true = :erlang.port_command(port, list)
    :ok
  end

  defp send_port(port, command) do
    send(port, {self(), command})
    :ok
  end

  defp io_reply(from, replyAs, reply) do
    send(from, {:io_reply, replyAs, reply})
  end

  defp put_chars(chars, port, q) when is_binary(chars) do
    :ok = put_port(chars, port)
    {:ok, :ok, q}
  end

  defp put_chars(chars, port, q) do
    case (try do
            :erlang.list_to_binary(chars)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      binary when is_binary(binary) ->
        put_chars(binary, port, q)

      _ ->
        {:error, {:error, :put_chars}, q}
    end
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

  defp setopts(opts0, port, q) do
    opts =
      :proplists.unfold(
        :proplists.substitute_negations(
          [{:list, :binary}],
          expand_encoding(opts0)
        )
      )

    case check_valid_opts(opts) do
      true ->
        do_setopts(opts, port, q)

      false ->
        {:error, {:error, :enotsup}, q}
    end
  end

  defp check_valid_opts([]) do
    true
  end

  defp check_valid_opts([{:binary, _} | t]) do
    check_valid_opts(t)
  end

  defp check_valid_opts([{:encoding, valid} | t])
       when valid === :latin1 or valid === :utf8 or
              valid === :unicode do
    check_valid_opts(t)
  end

  defp check_valid_opts(_) do
    false
  end

  defp do_setopts(opts, _Port, q) do
    case :proplists.get_value(:encoding, opts) do
      valid when valid === :unicode or valid === :utf8 ->
        :erlang.put(:encoding, :unicode)

      :latin1 ->
        :erlang.put(:encoding, :latin1)

      :undefined ->
        :ok
    end

    case :proplists.get_value(:binary, opts) do
      true ->
        :erlang.put(:read_mode, :binary)
        {:ok, :ok, q}

      false ->
        :erlang.put(:read_mode, :list)
        {:ok, :ok, q}

      _ ->
        {:ok, :ok, q}
    end
  end

  defp getopts(_Port, q) do
    bin = {:binary, :erlang.get(:read_mode) === :binary}
    uni = {:encoding, :erlang.get(:encoding)}
    {:ok, [bin, uni], q}
  end

  defp get_line_bin(prompt, port, q, enc) do
    case prompt(port, prompt) do
      :error ->
        {:error, {:error, :get_line}, q}

      :ok ->
        case {:erlang.get(:eof), :queue.is_empty(q)} do
          {true, true} ->
            {:ok, :eof, q}

          _ ->
            get_line(prompt, port, q, [], enc)
        end
    end
  end

  defp get_line(prompt, port, q, acc, enc) do
    case :queue.is_empty(q) do
      true ->
        receive do
          {^port, {:data, bytes}} ->
            get_line_bytes(prompt, port, q, acc, bytes, enc)

          {^port, :eof} ->
            :erlang.put(:eof, true)
            {:ok, :eof, :queue.new()}

          {:io_request, from, replyAs, {:get_geometry, _} = req}
          when is_pid(from) ->
            do_io_request(req, from, replyAs, port, :queue.new())
            get_line(prompt, port, q, acc, enc)

          {:io_request, from, replyAs, request} when is_pid(from) ->
            do_io_request(request, from, replyAs, port, :queue.new())

            case prompt(port, prompt) do
              :error ->
                {:error, {:error, :get_line}, q}

              :ok ->
                get_line(prompt, port, q, acc, enc)
            end

          {:EXIT, from, what} when node(from) === node() ->
            {:exit, what}
        end

      false ->
        get_line_doit(prompt, port, q, acc, enc)
    end
  end

  defp get_line_bytes(prompt, port, q, acc, bytes, enc) do
    case :erlang.get(:shell) do
      :noshell ->
        get_line_doit(prompt, port, :queue.snoc(q, bytes), acc, enc)

      _ ->
        case contains_ctrl_g_or_ctrl_c(bytes) do
          false ->
            get_line_doit(prompt, port, :queue.snoc(q, bytes), acc, enc)

          _ ->
            throw(:new_shell)
        end
    end
  end

  defp is_cr_at(pos, bin) do
    case bin do
      <<_::size(pos)-binary, ?\r, _::binary>> ->
        true

      _ ->
        false
    end
  end

  defp srch(<<>>, _, _) do
    :nomatch
  end

  defp srch(<<x::size(8), _::binary>>, x, n) do
    {:match, [{n, 1}]}
  end

  defp srch(<<_::size(8), t::binary>>, x, n) do
    srch(t, x, n + 1)
  end

  defp get_line_doit(prompt, port, q, accu, enc) do
    case :queue.is_empty(q) do
      true ->
        case :erlang.get(:eof) do
          true ->
            case accu do
              [] ->
                {:ok, :eof, q}

              _ ->
                {:ok, binrev(accu, []), q}
            end

          _ ->
            get_line(prompt, port, q, accu, enc)
        end

      false ->
        bin = :queue.head(q)

        case srch(bin, ?\n, 0) do
          :nomatch ->
            x = byte_size(bin) - 1

            case is_cr_at(x, bin) do
              true ->
                <<d::size(x)-binary, _::binary>> = bin
                get_line_doit(prompt, port, :queue.tail(q), [[<<?\r>>, d] | accu], enc)

              false ->
                get_line_doit(prompt, port, :queue.tail(q), [bin | accu], enc)
            end

          {:match, [{pos, 1}]} ->
            posPlus = pos + 1

            case accu do
              [] ->
                {head, tail} =
                  case is_cr_at(pos - 1, bin) do
                    false ->
                      <<h::size(posPlus)-binary, t::binary>> = bin
                      {h, t}

                    true ->
                      posMinus = pos - 1
                      <<h::size(posMinus)-binary, _, _, t::binary>> = bin
                      {binrev([], [h, ?\n]), t}
                  end

                case tail do
                  <<>> ->
                    {:ok, cast(head, enc), :queue.tail(q)}

                  _ ->
                    {:ok, cast(head, enc), :queue.cons(tail, :queue.tail(q))}
                end

              [<<?\r>> | stack1] when pos === 0 ->
                <<_::size(posPlus)-binary, tail::binary>> = bin

                case tail do
                  <<>> ->
                    {:ok, cast(binrev(stack1, [?\n]), enc), :queue.tail(q)}

                  _ ->
                    {:ok, cast(binrev(stack1, [?\n]), enc), :queue.cons(tail, :queue.tail(q))}
                end

              _ ->
                {head, tail} =
                  case is_cr_at(pos - 1, bin) do
                    false ->
                      <<h::size(posPlus)-binary, t::binary>> = bin
                      {h, t}

                    true ->
                      posMinus = pos - 1
                      <<h::size(posMinus)-binary, _, _, t::binary>> = bin
                      {[h, ?\n], t}
                  end

                case tail do
                  <<>> ->
                    {:ok, cast(binrev(accu, [head]), enc), :queue.tail(q)}

                  _ ->
                    {:ok, cast(binrev(accu, [head]), enc), :queue.cons(tail, :queue.tail(q))}
                end
            end
        end
    end
  end

  defp binrev(l, t) do
    :erlang.list_to_binary(:lists.reverse(l, t))
  end

  defp get_chars(prompt, m, f, xa, port, q, enc) do
    case prompt(port, prompt) do
      :error ->
        {:error, {:error, :get_chars}, q}

      :ok ->
        case {:erlang.get(:eof), :queue.is_empty(q)} do
          {true, true} ->
            {:ok, :eof, q}

          _ ->
            get_chars(prompt, m, f, xa, port, q, :start, enc)
        end
    end
  end

  defp get_chars(prompt, m, f, xa, port, q, state, enc) do
    case :queue.is_empty(q) do
      true ->
        receive do
          {^port, {:data, bytes}} ->
            get_chars_bytes(state, m, f, xa, port, q, bytes, enc)

          {^port, :eof} ->
            :erlang.put(:eof, true)
            {:ok, :eof, :queue.new()}

          {:io_request, from, replyAs, {:get_geometry, _} = req}
          when is_pid(from) ->
            do_io_request(req, from, replyAs, port, :queue.new())
            get_chars(prompt, m, f, xa, port, q, state, enc)

          {:io_request, from, replyAs, request} when is_pid(from) ->
            get_chars_req(prompt, m, f, xa, port, q, state, request, from, replyAs, enc)

          {:EXIT, from, what} when node(from) === node() ->
            {:exit, what}
        end

      false ->
        get_chars_apply(state, m, f, xa, port, q, enc)
    end
  end

  defp get_chars_req(prompt, m, f, xtraArg, port, q, state, req, from, replyAs, enc) do
    do_io_request(req, from, replyAs, port, :queue.new())

    case prompt(port, prompt) do
      :error ->
        {:error, {:error, :get_chars}, q}

      :ok ->
        get_chars(prompt, m, f, xtraArg, port, q, state, enc)
    end
  end

  defp get_chars_bytes(state, m, f, xa, port, q, bytes, enc) do
    case :erlang.get(:shell) do
      :noshell ->
        get_chars_apply(state, m, f, xa, port, :queue.snoc(q, bytes), enc)

      _ ->
        case contains_ctrl_g_or_ctrl_c(bytes) do
          false ->
            get_chars_apply(state, m, f, xa, port, :queue.snoc(q, bytes), enc)

          _ ->
            throw(:new_shell)
        end
    end
  end

  defp get_chars_apply(state0, m, f, xa, port, q, enc) do
    case (try do
            apply(m, f, [state0, cast(:queue.head(q), enc), enc, xa])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:stop, result, <<>>} ->
        {:ok, result, :queue.tail(q)}

      {:stop, result, []} ->
        {:ok, result, :queue.tail(q)}

      {:stop, result, :eof} ->
        {:ok, result, :queue.tail(q)}

      {:stop, result, buf} ->
        {:ok, result, :queue.cons(buf, :queue.tail(q))}

      {:EXIT, _Why} ->
        {:error, {:error, err_func(m, f, xa)}, :queue.new()}

      state1 ->
        get_chars_more(state1, m, f, xa, port, :queue.tail(q), enc)
    end
  end

  defp get_chars_more(state, m, f, xa, port, q, enc) do
    case :queue.is_empty(q) do
      true ->
        case :erlang.get(:eof) do
          :undefined ->
            receive do
              {^port, {:data, bytes}} ->
                get_chars_bytes(state, m, f, xa, port, q, bytes, enc)

              {^port, :eof} ->
                :erlang.put(:eof, true)
                get_chars_apply(state, m, f, xa, port, :queue.snoc(q, :eof), enc)

              {:EXIT, from, what} when node(from) === node() ->
                {:exit, what}
            end

          _ ->
            get_chars_apply(state, m, f, xa, port, :queue.snoc(q, :eof), enc)
        end

      false ->
        get_chars_apply(state, m, f, xa, port, q, enc)
    end
  end

  defp prompt(_Port, :"") do
    :ok
  end

  defp prompt(port, prompt) do
    encoding = :erlang.get(:encoding)
    promptString = :io_lib.format_prompt(prompt, encoding)

    case wrap_characters_to_binary(promptString, :unicode, encoding) do
      bin when is_binary(bin) ->
        put_port(bin, port)

      :error ->
        :error
    end
  end

  defp err_func(:io_lib, :get_until, {_, f, _}) do
    f
  end

  defp err_func(_, f, _) do
    f
  end

  defp contains_ctrl_g_or_ctrl_c(binOrList) do
    case {:re.run(binOrList, <<3>>), :re.run(binOrList, <<7>>)} do
      {:nomatch, :nomatch} ->
        false

      _ ->
        true
    end
  end

  defp cast(data, _Encoding) when is_atom(data) do
    data
  end

  defp cast(data, encoding) do
    ioEncoding = :erlang.get(:encoding)
    cast(data, :erlang.get(:read_mode), ioEncoding, encoding)
  end

  defp cast(b, :binary, :latin1, :latin1)
       when is_binary(b) do
    b
  end

  defp cast(l, :binary, :latin1, :latin1) do
    case (try do
            :erlang.iolist_to_binary(l)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      bin when is_binary(bin) ->
        bin

      _ ->
        exit({:no_translation, :latin1, :latin1})
    end
  end

  defp cast(data, :binary, :unicode, :latin1)
       when is_binary(data) or is_list(data) do
    case (try do
            :unicode.characters_to_binary(data, :unicode, :latin1)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      bin when is_binary(bin) ->
        bin

      _ ->
        exit({:no_translation, :unicode, :latin1})
    end
  end

  defp cast(data, :binary, :latin1, :unicode)
       when is_binary(data) or is_list(data) do
    case (try do
            :unicode.characters_to_binary(data, :latin1, :unicode)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      bin when is_binary(bin) ->
        bin

      _ ->
        exit({:no_translation, :latin1, :unicode})
    end
  end

  defp cast(b, :binary, :unicode, :unicode)
       when is_binary(b) do
    b
  end

  defp cast(l, :binary, :unicode, :unicode) do
    case (try do
            :unicode.characters_to_binary(l, :unicode)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      bin when is_binary(bin) ->
        bin

      _ ->
        exit({:no_translation, :unicode, :unicode})
    end
  end

  defp cast(b, :list, :latin1, :latin1) when is_binary(b) do
    :erlang.binary_to_list(b)
  end

  defp cast(l, :list, :latin1, :latin1) do
    case (try do
            :erlang.iolist_to_binary(l)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      bin when is_binary(bin) ->
        :erlang.binary_to_list(bin)

      _ ->
        exit({:no_translation, :latin1, :latin1})
    end
  end

  defp cast(data, :list, :unicode, :latin1)
       when is_binary(data) or is_list(data) do
    case (try do
            :unicode.characters_to_list(data, :unicode)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      chars when is_list(chars) ->
        for x <- chars do
          case x do
            high when high > 255 ->
              exit({:no_translation, :unicode, :latin1})

            low ->
              low
          end
        end

      _ ->
        exit({:no_translation, :unicode, :latin1})
    end
  end

  defp cast(data, :list, :latin1, :unicode)
       when is_binary(data) or is_list(data) do
    case (try do
            :unicode.characters_to_list(data, :latin1)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      chars when is_list(chars) ->
        chars

      _ ->
        exit({:no_translation, :latin1, :unicode})
    end
  end

  defp cast(data, :list, :unicode, :unicode)
       when is_binary(data) or is_list(data) do
    case (try do
            :unicode.characters_to_list(data, :unicode)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      chars when is_list(chars) ->
        chars

      _ ->
        exit({:no_translation, :unicode, :unicode})
    end
  end

  defp wrap_characters_to_binary(chars, :unicode, :latin1) do
    case (try do
            :unicode.characters_to_binary(chars, :unicode, :latin1)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      bin when is_binary(bin) ->
        bin

      _ ->
        case (try do
                :unicode.characters_to_list(chars, :unicode)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          l when is_list(l) ->
            :erlang.list_to_binary(
              for x <- l do
                case x do
                  high when high > 255 ->
                    ['\\x{', :erlang.integer_to_list(x, 16), ?}]

                  low ->
                    low
                end
              end
            )

          _ ->
            :error
        end
    end
  end

  defp wrap_characters_to_binary(bin, from, from) when is_binary(bin) do
    bin
  end

  defp wrap_characters_to_binary(chars, from, to) do
    case (try do
            :unicode.characters_to_binary(chars, from, to)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      bin when is_binary(bin) ->
        bin

      _ ->
        :error
    end
  end
end
