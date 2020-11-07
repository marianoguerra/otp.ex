defmodule :m_standard_error do
  use Bitwise
  @behaviour :supervisor_bridge
  def start_link() do
    :supervisor_bridge.start_link({:local, :standard_error_sup}, :standard_error, [])
  end

  def terminate(_Reason, pid) do
    try do
      :erlang.exit(pid, :kill)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  def init([]) do
    case (try do
            start_port([:out, :binary])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      pid when is_pid(pid) ->
        {:ok, pid, pid}

      _ ->
        {:error, :no_stderror}
    end
  end

  defp start_port(portSettings) do
    id =
      spawn(fn ->
        server({:fd, 2, 2}, portSettings)
      end)

    :erlang.register(:standard_error, id)
    id
  end

  defp server(portName, portSettings) do
    :erlang.process_flag(:trap_exit, true)
    port = :erlang.open_port(portName, portSettings)
    run(port)
  end

  defp run(p) do
    :erlang.put(:encoding, :latin1)
    server_loop(p)
  end

  defp server_loop(port) do
    receive do
      {:io_request, from, replyAs, request} when is_pid(from) ->
        _ = do_io_request(request, from, replyAs, port)
        server_loop(port)

      {:EXIT, ^port, :badsig} ->
        server_loop(port)

      {:EXIT, ^port, what} ->
        exit(what)

      _Other ->
        server_loop(port)
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

  defp do_io_request(req, from, replyAs, port) do
    {_Status, reply} = io_request(req, port)
    io_reply(from, replyAs, reply)
  end

  defp io_request({:put_chars, :unicode, chars}, port) do
    case wrap_characters_to_binary(chars, :unicode, :erlang.get(:encoding)) do
      :error ->
        {:error, {:error, :put_chars}}

      bin ->
        put_chars(bin, port)
    end
  end

  defp io_request(
         {:put_chars, :unicode, mod, func, args},
         port
       ) do
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
            put_chars(bin, port)

          :error ->
            {:error, {:error, :put_chars}}
        end

      _ ->
        {:error, {:error, :put_chars}}
    end
  end

  defp io_request({:put_chars, :latin1, chars}, port) do
    case (try do
            :unicode.characters_to_binary(chars, :latin1, :erlang.get(:encoding))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      data when is_binary(data) ->
        put_chars(data, port)

      _ ->
        {:error, {:error, :put_chars}}
    end
  end

  defp io_request({:put_chars, :latin1, mod, func, args}, port) do
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
            put_chars(bin, port)

          _ ->
            {:error, {:error, :put_chars}}
        end

      _ ->
        {:error, {:error, :put_chars}}
    end
  end

  defp io_request({:put_chars, chars}, port) do
    io_request({:put_chars, :latin1, chars}, port)
  end

  defp io_request({:put_chars, mod, func, args}, port) do
    io_request({:put_chars, :latin1, mod, func, args}, port)
  end

  defp io_request({:get_geometry, :columns}, port) do
    case get_fd_geometry(port) do
      {w, _H} ->
        {:ok, w}

      _ ->
        {:error, {:error, :enotsup}}
    end
  end

  defp io_request({:get_geometry, :rows}, port) do
    case get_fd_geometry(port) do
      {_W, h} ->
        {:ok, h}

      _ ->
        {:error, {:error, :enotsup}}
    end
  end

  defp io_request(:getopts, _Port) do
    getopts()
  end

  defp io_request({:setopts, opts}, _Port) when is_list(opts) do
    setopts(opts)
  end

  defp io_request({:requests, reqs}, port) do
    io_requests(reqs, {:ok, :ok}, port)
  end

  defp io_request(r, _Port) do
    {:error, {:error, {:request, r}}}
  end

  defp io_requests([r | rs], {:ok, _Res}, port) do
    io_requests(rs, io_request(r, port), port)
  end

  defp io_requests([_ | _], error, _) do
    error
  end

  defp io_requests([], stat, _) do
    stat
  end

  defp put_port(list, port) do
    send_port(port, {:command, list})
  end

  defp send_port(port, command) do
    send(port, {self(), command})
  end

  defp io_reply(from, replyAs, reply) do
    send(from, {:io_reply, replyAs, reply})
  end

  defp put_chars(chars, port) when is_binary(chars) do
    _ = put_port(chars, port)
    {:ok, :ok}
  end

  defp setopts(opts0) do
    opts = expand_encoding(opts0)

    case check_valid_opts(opts) do
      true ->
        do_setopts(opts)

      false ->
        {:error, {:error, :enotsup}}
    end
  end

  defp check_valid_opts([]) do
    true
  end

  defp check_valid_opts([{:encoding, valid} | t])
       when valid === :unicode or valid === :utf8 or
              valid === :latin1 do
    check_valid_opts(t)
  end

  defp check_valid_opts(_) do
    false
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

  defp do_setopts(opts) do
    case :proplists.get_value(:encoding, opts) do
      valid when valid === :unicode or valid === :utf8 ->
        :erlang.put(:encoding, :unicode)

      :latin1 ->
        :erlang.put(:encoding, :latin1)

      :undefined ->
        :ok
    end

    {:ok, :ok}
  end

  defp getopts() do
    uni = {:encoding, :erlang.get(:encoding)}
    {:ok, [uni]}
  end

  defp wrap_characters_to_binary(chars, from, to) do
    trNl = :erlang.whereis(:user_drv) !== :undefined

    limit =
      case to do
        :latin1 ->
          255

        _Else ->
          1_114_111
      end

    case (try do
            :unicode.characters_to_list(chars, from)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      l when is_list(l) ->
        :unicode.characters_to_binary(
          for x <- l do
            case x do
              ?\n when trNl ->
                '\r\n'

              high when high > limit ->
                ['\\x{', :erlang.integer_to_list(x, 16), ?}]

              low ->
                low
            end
          end,
          :unicode,
          to
        )

      _ ->
        :error
    end
  end
end
