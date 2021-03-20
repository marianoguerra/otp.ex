defmodule :m_group do
  use Bitwise

  def start(drv, shell) do
    start(drv, shell, [])
  end

  def start(drv, shell, options) do
    spawn_link(:group, :server, [drv, shell, options])
  end

  def server(drv, shell, options) do
    :erlang.process_flag(:trap_exit, true)
    :edlin.init()

    :erlang.put(
      :line_buffer,
      :proplists.get_value(:line_buffer, options, :group_history.load())
    )

    :erlang.put(:read_mode, :list)
    :erlang.put(:user_drv, drv)

    :erlang.put(
      :expand_fun,
      :proplists.get_value(:expand_fun, options, fn b ->
        :edlin_expand.expand(b)
      end)
    )

    :erlang.put(
      :echo,
      :proplists.get_value(:echo, options, true)
    )

    start_shell(shell)
    server_loop(drv, :erlang.get(:shell), [])
  end

  def interfaces(group) do
    case :erlang.process_info(group, :dictionary) do
      {:dictionary, dict} ->
        get_pids(dict, [], false)

      _ ->
        []
    end
  end

  defp get_pids([drv = {:user_drv, _} | rest], found, _) do
    get_pids(rest, [drv | found], true)
  end

  defp get_pids([sh = {:shell, _} | rest], found, active) do
    get_pids(rest, [sh | found], active)
  end

  defp get_pids([_ | rest], found, active) do
    get_pids(rest, found, active)
  end

  defp get_pids([], found, true) do
    found
  end

  defp get_pids([], _Found, false) do
    []
  end

  defp start_shell({mod, func, args}) do
    start_shell1(mod, func, args)
  end

  defp start_shell({node, mod, func, args}) do
    start_shell1(:rpc, :call, [node, mod, func, args])
  end

  defp start_shell(shell) when is_atom(shell) do
    start_shell1(shell, :start, [])
  end

  defp start_shell(shell) when is_function(shell) do
    start_shell1(shell)
  end

  defp start_shell(shell) when is_pid(shell) do
    :erlang.group_leader(self(), shell)
    :erlang.link(shell)
    :erlang.put(:shell, shell)
  end

  defp start_shell(_Shell) do
    :ok
  end

  defp start_shell1(m, f, args) do
    g = :erlang.group_leader()
    :erlang.group_leader(self(), self())

    case (try do
            apply(m, f, args)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      shell when is_pid(shell) ->
        :erlang.group_leader(g, self())
        :erlang.link(shell)
        :erlang.put(:shell, shell)

      error ->
        exit(error)
    end
  end

  defp start_shell1(fun) do
    g = :erlang.group_leader()
    :erlang.group_leader(self(), self())

    case (try do
            fun.()
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      shell when is_pid(shell) ->
        :erlang.group_leader(g, self())
        :erlang.link(shell)
        :erlang.put(:shell, shell)

      error ->
        exit(error)
    end
  end

  defp server_loop(drv, shell, buf0) do
    receive do
      {:io_request, from, replyAs, req} when is_pid(from) ->
        buf = io_request(req, from, replyAs, drv, shell, buf0)
        server_loop(drv, shell, buf)

      {:reply, {{from, replyAs}, reply}} ->
        io_reply(from, replyAs, reply)
        server_loop(drv, shell, buf0)

      {:driver_id, replyTo} ->
        send(replyTo, {self(), :driver_id, drv})
        server_loop(drv, shell, buf0)

      {^drv, :echo, bool} ->
        :erlang.put(:echo, bool)
        server_loop(drv, shell, buf0)

      {:EXIT, ^drv, :interrupt} ->
        exit_shell(:interrupt)
        server_loop(drv, shell, buf0)

      {:EXIT, ^drv, r} ->
        exit(r)

      {:EXIT, ^shell, r} ->
        exit(r)

      notDrvTuple
      when not is_tuple(notDrvTuple) or tuple_size(notDrvTuple) !== 2 or
             :erlang.element(
               1,
               notDrvTuple
             ) !== drv ->
        server_loop(drv, shell, buf0)
    end
  end

  defp exit_shell(reason) do
    case :erlang.get(:shell) do
      :undefined ->
        true

      pid ->
        :erlang.exit(pid, reason)
    end
  end

  defp get_tty_geometry(drv) do
    send(drv, {self(), :tty_geometry})

    receive do
      {^drv, :tty_geometry, geometry} ->
        geometry
    after
      2000 ->
        :timeout
    end
  end

  defp get_unicode_state(drv) do
    send(drv, {self(), :get_unicode_state})

    receive do
      {^drv, :get_unicode_state, uniState} ->
        uniState

      {^drv, :get_unicode_state, :error} ->
        {:error, :internal}
    after
      2000 ->
        {:error, :timeout}
    end
  end

  defp set_unicode_state(drv, bool) do
    send(drv, {self(), :set_unicode_state, bool})

    receive do
      {^drv, :set_unicode_state, _OldUniState} ->
        :ok
    after
      2000 ->
        :timeout
    end
  end

  defp io_request(req, from, replyAs, drv, shell, buf0) do
    case io_request(req, drv, shell, {from, replyAs}, buf0) do
      {:ok, reply, buf} ->
        io_reply(from, replyAs, reply)
        buf

      {:noreply, buf} ->
        buf

      {:error, reply, buf} ->
        io_reply(from, replyAs, reply)
        buf

      {:exit, r} ->
        exit_shell(:kill)
        exit(r)
    end
  end

  defp io_request({:put_chars, :unicode, chars}, drv, _Shell, from, buf) do
    case (try do
            :unicode.characters_to_binary(chars, :utf8)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      binary when is_binary(binary) ->
        send_drv(
          drv,
          {:put_chars_sync, :unicode, binary, {from, :ok}}
        )

        {:noreply, buf}

      _ ->
        {:error, {:error, {:put_chars, :unicode, chars}}, buf}
    end
  end

  defp io_request({:put_chars, :unicode, m, f, as}, drv, _Shell, from, buf) do
    case (try do
            apply(m, f, as)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      binary when is_binary(binary) ->
        send_drv(
          drv,
          {:put_chars_sync, :unicode, binary, {from, :ok}}
        )

        {:noreply, buf}

      chars ->
        case (try do
                :unicode.characters_to_binary(chars, :utf8)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          b when is_binary(b) ->
            send_drv(
              drv,
              {:put_chars_sync, :unicode, b, {from, :ok}}
            )

            {:noreply, buf}

          _ ->
            {:error, {:error, f}, buf}
        end
    end
  end

  defp io_request({:put_chars, :latin1, binary}, drv, _Shell, from, buf)
       when is_binary(binary) do
    send_drv(
      drv,
      {:put_chars_sync, :unicode, :unicode.characters_to_binary(binary, :latin1), {from, :ok}}
    )

    {:noreply, buf}
  end

  defp io_request({:put_chars, :latin1, chars}, drv, _Shell, from, buf) do
    case (try do
            :unicode.characters_to_binary(chars, :latin1)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      binary when is_binary(binary) ->
        send_drv(
          drv,
          {:put_chars_sync, :unicode, binary, {from, :ok}}
        )

        {:noreply, buf}

      _ ->
        {:error, {:error, {:put_chars, :latin1, chars}}, buf}
    end
  end

  defp io_request({:put_chars, :latin1, m, f, as}, drv, _Shell, from, buf) do
    case (try do
            apply(m, f, as)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      binary when is_binary(binary) ->
        send_drv(
          drv,
          {:put_chars_sync, :unicode, :unicode.characters_to_binary(binary, :latin1), {from, :ok}}
        )

        {:noreply, buf}

      chars ->
        case (try do
                :unicode.characters_to_binary(chars, :latin1)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          b when is_binary(b) ->
            send_drv(
              drv,
              {:put_chars_sync, :unicode, b, {from, :ok}}
            )

            {:noreply, buf}

          _ ->
            {:error, {:error, f}, buf}
        end
    end
  end

  defp io_request({:get_chars, encoding, prompt, n}, drv, shell, _From, buf) do
    get_chars_n(prompt, :io_lib, :collect_chars, n, drv, shell, buf, encoding)
  end

  defp io_request({:get_line, encoding, prompt}, drv, shell, _From, buf) do
    get_chars_line(prompt, :io_lib, :collect_line, [], drv, shell, buf, encoding)
  end

  defp io_request({:get_until, encoding, prompt, m, f, as}, drv, shell, _From, buf) do
    get_chars_line(prompt, :io_lib, :get_until, {m, f, as}, drv, shell, buf, encoding)
  end

  defp io_request({:get_password, _Encoding}, drv, shell, _From, buf) do
    get_password_chars(drv, shell, buf)
  end

  defp io_request({:setopts, opts}, drv, _Shell, _From, buf)
       when is_list(opts) do
    setopts(opts, drv, buf)
  end

  defp io_request(:getopts, drv, _Shell, _From, buf) do
    getopts(drv, buf)
  end

  defp io_request({:requests, reqs}, drv, shell, from, buf) do
    io_requests(reqs, {:ok, :ok, buf}, from, drv, shell)
  end

  defp io_request({:get_geometry, :columns}, drv, _Shell, _From, buf) do
    case get_tty_geometry(drv) do
      {w, _H} ->
        {:ok, w, buf}

      _ ->
        {:error, {:error, :enotsup}, buf}
    end
  end

  defp io_request({:get_geometry, :rows}, drv, _Shell, _From, buf) do
    case get_tty_geometry(drv) do
      {_W, h} ->
        {:ok, h, buf}

      _ ->
        {:error, {:error, :enotsup}, buf}
    end
  end

  defp io_request({:put_chars, chars}, drv, shell, from, buf) do
    io_request({:put_chars, :latin1, chars}, drv, shell, from, buf)
  end

  defp io_request({:put_chars, m, f, as}, drv, shell, from, buf) do
    io_request({:put_chars, :latin1, m, f, as}, drv, shell, from, buf)
  end

  defp io_request({:get_chars, prompt, n}, drv, shell, from, buf) do
    io_request({:get_chars, :latin1, prompt, n}, drv, shell, from, buf)
  end

  defp io_request({:get_line, prompt}, drv, shell, from, buf) do
    io_request({:get_line, :latin1, prompt}, drv, shell, from, buf)
  end

  defp io_request({:get_until, prompt, m, f, as}, drv, shell, from, buf) do
    io_request({:get_until, :latin1, prompt, m, f, as}, drv, shell, from, buf)
  end

  defp io_request(:get_password, drv, shell, from, buf) do
    io_request({:get_password, :latin1}, drv, shell, from, buf)
  end

  defp io_request(_, _Drv, _Shell, _From, buf) do
    {:error, {:error, :request}, buf}
  end

  defp io_requests([r | rs], {:noreply, buf}, from, drv, shell) do
    reqFrom =
      cond do
        rs === [] ->
          from

        true ->
          :undefined
      end

    io_requests(rs, io_request(r, drv, shell, reqFrom, buf), from, drv, shell)
  end

  defp io_requests([r | rs], {:ok, :ok, buf}, from, drv, shell) do
    reqFrom =
      cond do
        rs === [] ->
          from

        true ->
          :undefined
      end

    io_requests(rs, io_request(r, drv, shell, reqFrom, buf), from, drv, shell)
  end

  defp io_requests([_ | _], error, _From, _Drv, _Shell) do
    error
  end

  defp io_requests([], stat, _From, _, _Shell) do
    stat
  end

  defp io_reply(:undefined, _ReplyAs, _Reply) do
    :ok
  end

  defp io_reply(from, replyAs, reply) do
    send(from, {:io_reply, replyAs, reply})
    :ok
  end

  defp send_drv(drv, msg) do
    send(drv, {self(), msg})
    :ok
  end

  defp send_drv_reqs(_Drv, []) do
    :ok
  end

  defp send_drv_reqs(drv, rs) do
    send_drv(drv, {:requests, rs})
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

  defp setopts(opts0, drv, buf) do
    opts =
      :proplists.unfold(
        :proplists.substitute_negations(
          [{:list, :binary}],
          expand_encoding(opts0)
        )
      )

    case check_valid_opts(opts) do
      true ->
        do_setopts(opts, drv, buf)

      false ->
        {:error, {:error, :enotsup}, buf}
    end
  end

  defp check_valid_opts([]) do
    true
  end

  defp check_valid_opts([{:binary, _} | t]) do
    check_valid_opts(t)
  end

  defp check_valid_opts([{:encoding, valid} | t])
       when valid === :unicode or valid === :utf8 or
              valid === :latin1 do
    check_valid_opts(t)
  end

  defp check_valid_opts([{:echo, _} | t]) do
    check_valid_opts(t)
  end

  defp check_valid_opts([{:expand_fun, _} | t]) do
    check_valid_opts(t)
  end

  defp check_valid_opts(_) do
    false
  end

  defp do_setopts(opts, drv, buf) do
    :erlang.put(
      :expand_fun,
      :proplists.get_value(:expand_fun, opts, :erlang.get(:expand_fun))
    )

    :erlang.put(
      :echo,
      :proplists.get_value(:echo, opts, :erlang.get(:echo))
    )

    case :proplists.get_value(:encoding, opts) do
      valid when valid === :unicode or valid === :utf8 ->
        set_unicode_state(drv, true)

      :latin1 ->
        set_unicode_state(drv, false)

      _ ->
        :ok
    end

    case :proplists.get_value(
           :binary,
           opts,
           case :erlang.get(:read_mode) do
             :binary ->
               true

             _ ->
               false
           end
         ) do
      true ->
        :erlang.put(:read_mode, :binary)
        {:ok, :ok, buf}

      false ->
        :erlang.put(:read_mode, :list)
        {:ok, :ok, buf}

      _ ->
        {:ok, :ok, buf}
    end
  end

  defp getopts(drv, buf) do
    exp =
      {:expand_fun,
       case :erlang.get(:expand_fun) do
         func when is_function(func) ->
           func

         _ ->
           false
       end}

    echo =
      {:echo,
       case :erlang.get(:echo) do
         bool when bool === true or bool === false ->
           bool

         _ ->
           false
       end}

    bin =
      {:binary,
       case :erlang.get(:read_mode) do
         :binary ->
           true

         _ ->
           false
       end}

    uni =
      {:encoding,
       case get_unicode_state(drv) do
         true ->
           :unicode

         _ ->
           :latin1
       end}

    {:ok, [exp, echo, bin, uni], buf}
  end

  defp get_password_chars(drv, shell, buf) do
    case get_password_line(buf, drv, shell) do
      {:done, line, buf1} ->
        {:ok, line, buf1}

      :interrupted ->
        {:error, {:error, :interrupted}, []}

      :terminated ->
        {:exit, :terminated}
    end
  end

  defp get_chars_n(prompt, m, f, xa, drv, shell, buf, encoding) do
    pbs = prompt_bytes(prompt, encoding)

    case :erlang.get(:echo) do
      true ->
        get_chars_loop(pbs, m, f, xa, drv, shell, buf, :start, encoding)

      false ->
        get_chars_n_loop(pbs, m, f, xa, drv, shell, buf, :start, encoding)
    end
  end

  defp get_chars_line(prompt, m, f, xa, drv, shell, buf, encoding) do
    pbs = prompt_bytes(prompt, encoding)
    get_chars_loop(pbs, m, f, xa, drv, shell, buf, :start, encoding)
  end

  defp get_chars_loop(pbs, m, f, xa, drv, shell, buf0, state, encoding) do
    result =
      case :erlang.get(:echo) do
        true ->
          get_line(buf0, pbs, drv, shell, encoding)

        false ->
          get_line_echo_off(buf0, pbs, drv, shell)
      end

    case result do
      {:done, line, buf} ->
        get_chars_apply(pbs, m, f, xa, drv, shell, buf, state, line, encoding)

      :interrupted ->
        {:error, {:error, :interrupted}, []}

      :terminated ->
        {:exit, :terminated}
    end
  end

  defp get_chars_apply(pbs, m, f, xa, drv, shell, buf, state0, line, encoding) do
    case (try do
            apply(m, f, [state0, cast(line, :erlang.get(:read_mode), encoding), encoding, xa])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:stop, result, rest} ->
        {:ok, result, append(rest, buf, encoding)}

      {:EXIT, _} ->
        {:error, {:error, err_func(m, f, xa)}, []}

      state1 ->
        get_chars_loop(pbs, m, f, xa, drv, shell, buf, state1, encoding)
    end
  end

  defp get_chars_n_loop(pbs, m, f, xa, drv, shell, buf0, state, encoding) do
    try do
      apply(m, f, [state, cast(buf0, :erlang.get(:read_mode), encoding), encoding, xa])
    catch
      _, _ ->
        {:error, {:error, err_func(m, f, xa)}, []}
    else
      {:stop, result, rest} ->
        {:ok, result, rest}

      state1 ->
        case get_chars_echo_off(pbs, drv, shell) do
          :interrupted ->
            {:error, {:error, :interrupted}, []}

          :terminated ->
            {:exit, :terminated}

          buf ->
            get_chars_n_loop(pbs, m, f, xa, drv, shell, buf, state1, encoding)
        end
    end
  end

  defp err_func(:io_lib, :get_until, {_, f, _}) do
    f
  end

  defp err_func(_, f, _) do
    f
  end

  defp get_line(chars, pbs, drv, shell, encoding) do
    {:more_chars, cont, rs} = :edlin.start(pbs)
    send_drv_reqs(drv, rs)

    get_line1(
      :edlin.edit_line(chars, cont),
      drv,
      shell,
      new_stack(:erlang.get(:line_buffer)),
      encoding
    )
  end

  defp get_line1({:done, line, rest, rs}, drv, _Shell, ls, _Encoding) do
    send_drv_reqs(drv, rs)
    save_line_buffer(line, get_lines(ls))
    {:done, line, rest}
  end

  defp get_line1({:undefined, {_A, mode, char}, cs, cont, rs}, drv, shell, ls0, encoding)
       when (mode === :none and char === 16) or (mode === :meta_left_sq_bracket and char === ?A) do
    send_drv_reqs(drv, rs)

    case up_stack(
           save_line(
             ls0,
             :edlin.current_line(cont)
           )
         ) do
      {:none, _Ls} ->
        send_drv(drv, :beep)
        get_line1(:edlin.edit_line(cs, cont), drv, shell, ls0, encoding)

      {lcs, ls} ->
        send_drv_reqs(drv, :edlin.erase_line(cont))
        {:more_chars, ncont, nrs} = :edlin.start(:edlin.prompt(cont))
        send_drv_reqs(drv, nrs)

        get_line1(
          :edlin.edit_line1(
            :lists.sublist(lcs, 1, length(lcs) - 1),
            ncont
          ),
          drv,
          shell,
          ls,
          encoding
        )
    end
  end

  defp get_line1({:undefined, {_A, mode, char}, cs, cont, rs}, drv, shell, ls0, encoding)
       when (mode === :none and char === 14) or (mode === :meta_left_sq_bracket and char === ?B) do
    send_drv_reqs(drv, rs)

    case down_stack(
           save_line(
             ls0,
             :edlin.current_line(cont)
           )
         ) do
      {:none, _Ls} ->
        send_drv(drv, :beep)
        get_line1(:edlin.edit_line(cs, cont), drv, shell, ls0, encoding)

      {lcs, ls} ->
        send_drv_reqs(drv, :edlin.erase_line(cont))
        {:more_chars, ncont, nrs} = :edlin.start(:edlin.prompt(cont))
        send_drv_reqs(drv, nrs)

        get_line1(
          :edlin.edit_line1(
            :lists.sublist(lcs, 1, length(lcs) - 1),
            ncont
          ),
          drv,
          shell,
          ls,
          encoding
        )
    end
  end

  defp get_line1({:undefined, {_A, mode, char}, cs, cont, rs}, drv, shell, ls, encoding)
       when mode === :none and char === 18 do
    send_drv_reqs(drv, rs)
    send_drv_reqs(drv, :edlin.erase_line(cont))
    :erlang.put(:search_quit_prompt, :edlin.prompt(cont))
    pbs = prompt_bytes('(search)`\': ', encoding)
    {:more_chars, ncont, nrs} = :edlin.start(pbs, :search)
    send_drv_reqs(drv, nrs)
    get_line1(:edlin.edit_line1(cs, ncont), drv, shell, ls, encoding)
  end

  defp get_line1({:expand, before, cs0, cont, rs}, drv, shell, ls0, encoding) do
    send_drv_reqs(drv, rs)
    expandFun = :erlang.get(:expand_fun)
    {found, add, matches} = expandFun.(before)

    case found do
      :no ->
        send_drv(drv, :beep)

      :yes ->
        :ok
    end

    cs1 = append(add, cs0, encoding)

    cs =
      case matches do
        [] ->
          cs1

        _ ->
          matchStr = :edlin_expand.format_matches(matches)

          send_drv(
            drv,
            {:put_chars, :unicode, :unicode.characters_to_binary(matchStr, :unicode)}
          )

          [?\f | cs1]
      end

    get_line1(:edlin.edit_line(cs, cont), drv, shell, ls0, encoding)
  end

  defp get_line1({:undefined, _Char, cs, cont, rs}, drv, shell, ls, encoding) do
    send_drv_reqs(drv, rs)
    send_drv(drv, :beep)
    get_line1(:edlin.edit_line(cs, cont), drv, shell, ls, encoding)
  end

  defp get_line1(
         {_What, cont = {:line, _Prompt, _Chars, :search_found}, rs},
         drv,
         shell,
         ls0,
         encoding
       ) do
    line = :edlin.current_line(cont)
    ls = save_line(new_stack(get_lines(ls0)), line)
    get_line1({:done, line, '', rs}, drv, shell, ls, encoding)
  end

  defp get_line1(
         {what, cont = {:line, _Prompt, _Chars, :search_quit}, rs},
         drv,
         shell,
         ls,
         encoding
       ) do
    line = :edlin.current_chars(cont)

    case :erlang.get(:search_quit_prompt) do
      :undefined ->
        lsFallback = save_line(new_stack(get_lines(ls)), line)
        get_line1({:done, '\n', line, rs}, drv, shell, lsFallback, encoding)

      prompt ->
        nCont = {:line, prompt, {:lists.reverse(line), []}, :none}
        send_drv_reqs(drv, rs)
        send_drv_reqs(drv, :edlin.erase_line(cont))
        send_drv_reqs(drv, :edlin.redraw_line(nCont))
        get_line1({what, nCont, []}, drv, shell, pad_stack(ls), encoding)
    end
  end

  defp get_line1({what, {:line, prompt, {revCmd0, _Aft}, :search}, rs}, drv, shell, ls0, encoding) do
    send_drv_reqs(drv, rs)

    {search, ls1, revCmd} =
      case revCmd0 do
        [19 | revCmd1] ->
          {&search_down_stack/2, ls0, revCmd1}

        [18 | revCmd1] ->
          {&search_up_stack/2, ls0, revCmd1}

        _ ->
          {&search_up_stack/2, new_stack(get_lines(ls0)), revCmd0}
      end

    cmd = :lists.reverse(revCmd)

    {ls, newStack} =
      case search.(ls1, cmd) do
        {:none, ls2} ->
          send_drv(drv, :beep)
          {ls2, {revCmd, '\': '}}

        {line, ls2} ->
          send_drv_reqs(drv, [{:put_chars, encoding, line}])
          {ls2, {revCmd, '\': ' ++ line}}
      end

    cont = {:line, prompt, newStack, :search}
    more_data(what, cont, drv, shell, ls, encoding)
  end

  defp get_line1({what, cont0, rs}, drv, shell, ls, encoding) do
    send_drv_reqs(drv, rs)
    more_data(what, cont0, drv, shell, ls, encoding)
  end

  defp more_data(what, cont0, drv, shell, ls, encoding) do
    receive do
      {^drv, {:data, cs}} ->
        get_line1(:edlin.edit_line(cs, cont0), drv, shell, ls, encoding)

      {^drv, :eof} ->
        get_line1(:edlin.edit_line(:eof, cont0), drv, shell, ls, encoding)

      {:io_request, from, replyAs, req} when is_pid(from) ->
        {:more_chars, cont, _More} = :edlin.edit_line([], cont0)
        send_drv_reqs(drv, :edlin.erase_line(cont))
        io_request(req, from, replyAs, drv, shell, [])
        send_drv_reqs(drv, :edlin.redraw_line(cont))
        get_line1({:more_chars, cont, []}, drv, shell, ls, encoding)

      {:reply, {{from, replyAs}, reply}} ->
        io_reply(from, replyAs, reply)
        more_data(what, cont0, drv, shell, ls, encoding)

      {:EXIT, ^drv, :interrupt} ->
        :interrupted

      {:EXIT, ^drv, _} ->
        :terminated

      {:EXIT, ^shell, r} ->
        exit(r)
    after
      get_line_timeout(what) ->
        get_line1(:edlin.edit_line([], cont0), drv, shell, ls, encoding)
    end
  end

  defp get_line_echo_off(chars, pbs, drv, shell) do
    send_drv_reqs(drv, [{:put_chars, :unicode, pbs}])
    get_line_echo_off1(edit_line(chars, []), drv, shell)
  end

  defp get_line_echo_off1({chars, []}, drv, shell) do
    receive do
      {^drv, {:data, cs}} ->
        get_line_echo_off1(edit_line(cs, chars), drv, shell)

      {^drv, :eof} ->
        get_line_echo_off1(edit_line(:eof, chars), drv, shell)

      {:io_request, from, replyAs, req} when is_pid(from) ->
        io_request(req, from, replyAs, drv, shell, [])
        get_line_echo_off1({chars, []}, drv, shell)

      {:reply, {{from, replyAs}, reply}}
      when from !== :undefined ->
        io_reply(from, replyAs, reply)
        get_line_echo_off1({chars, []}, drv, shell)

      {:EXIT, ^drv, :interrupt} ->
        :interrupted

      {:EXIT, ^drv, _} ->
        :terminated

      {:EXIT, ^shell, r} ->
        exit(r)
    end
  end

  defp get_line_echo_off1({chars, rest}, _Drv, _Shell) do
    {:done, :lists.reverse(chars),
     case rest do
       :done ->
         []

       _ ->
         rest
     end}
  end

  defp get_chars_echo_off(pbs, drv, shell) do
    send_drv_reqs(drv, [{:put_chars, :unicode, pbs}])
    get_chars_echo_off1(drv, shell)
  end

  defp get_chars_echo_off1(drv, shell) do
    receive do
      {^drv, {:data, cs}} ->
        cs

      {^drv, :eof} ->
        :eof

      {:io_request, from, replyAs, req} when is_pid(from) ->
        io_request(req, from, replyAs, drv, shell, [])
        get_chars_echo_off1(drv, shell)

      {:reply, {{from, replyAs}, reply}}
      when from !== :undefined ->
        io_reply(from, replyAs, reply)
        get_chars_echo_off1(drv, shell)

      {:EXIT, ^drv, :interrupt} ->
        :interrupted

      {:EXIT, ^drv, _} ->
        :terminated

      {:EXIT, ^shell, r} ->
        exit(r)
    end
  end

  defp edit_line(:eof, chars) do
    {chars, :done}
  end

  defp edit_line([], chars) do
    {chars, []}
  end

  defp edit_line([?\r, ?\n | cs], chars) do
    {[?\n | chars], remainder_after_nl(cs)}
  end

  defp edit_line([nL | cs], chars)
       when nL === ?\r or
              nL === ?\n do
    {[?\n | chars], remainder_after_nl(cs)}
  end

  defp edit_line([erase | cs], [])
       when erase === ?\d or
              erase === ?\b do
    edit_line(cs, [])
  end

  defp edit_line([erase | cs], [_ | chars])
       when erase === ?\d or
              erase === ?\b do
    edit_line(cs, chars)
  end

  defp edit_line([char | cs], chars) do
    edit_line(cs, [char | chars])
  end

  defp remainder_after_nl('') do
    :done
  end

  defp remainder_after_nl(cs) do
    cs
  end

  defp get_line_timeout(:blink) do
    1000
  end

  defp get_line_timeout(:more_chars) do
    :infinity
  end

  defp new_stack(ls) do
    {:stack, ls, {}, []}
  end

  defp up_stack({:stack, [l | u], {}, d}) do
    {l, {:stack, u, l, d}}
  end

  defp up_stack({:stack, [], {}, d}) do
    {:none, {:stack, [], {}, d}}
  end

  defp up_stack({:stack, u, c, d}) do
    up_stack({:stack, u, {}, [c | d]})
  end

  defp down_stack({:stack, u, {}, [l | d]}) do
    {l, {:stack, u, l, d}}
  end

  defp down_stack({:stack, u, {}, []}) do
    {:none, {:stack, u, {}, []}}
  end

  defp down_stack({:stack, u, c, d}) do
    down_stack({:stack, [c | u], {}, d})
  end

  defp save_line({:stack, u, {}, []}, line) do
    {:stack, u, {}, [line]}
  end

  defp save_line({:stack, u, _L, d}, line) do
    {:stack, u, line, d}
  end

  defp get_lines(ls) do
    get_all_lines(ls)
  end

  defp get_all_lines({:stack, u, {}, []}) do
    u
  end

  defp get_all_lines({:stack, u, {}, d}) do
    case :lists.reverse(d, u) do
      ['\n' | lines] ->
        lines

      lines ->
        lines
    end
  end

  defp get_all_lines({:stack, u, l, d}) do
    get_all_lines({:stack, u, {}, [l | d]})
  end

  defp pad_stack({:stack, u, l, d}) do
    {:stack, u, l, d ++ ['\n']}
  end

  defp save_line_buffer('\n', lines) do
    save_line_buffer(lines)
  end

  defp save_line_buffer(line, [line | _Lines] = lines) do
    save_line_buffer(lines)
  end

  defp save_line_buffer(line, lines) do
    :group_history.add(line)
    save_line_buffer([line | lines])
  end

  defp save_line_buffer(lines) do
    :erlang.put(:line_buffer, lines)
  end

  defp search_up_stack(stack, substr) do
    case up_stack(stack) do
      {:none, newStack} ->
        {:none, newStack}

      {l, newStack} ->
        case :string.find(l, substr) do
          :nomatch ->
            search_up_stack(newStack, substr)

          _ ->
            {:string.trim(l, :trailing, '$\n'), newStack}
        end
    end
  end

  defp search_down_stack(stack, substr) do
    case down_stack(stack) do
      {:none, newStack} ->
        {:none, newStack}

      {l, newStack} ->
        case :string.find(l, substr) do
          :nomatch ->
            search_down_stack(newStack, substr)

          _ ->
            {:string.trim(l, :trailing, '$\n'), newStack}
        end
    end
  end

  defp get_password_line(chars, drv, shell) do
    get_password1(edit_password(chars, []), drv, shell)
  end

  defp get_password1({chars, []}, drv, shell) do
    receive do
      {^drv, {:data, cs}} ->
        get_password1(edit_password(cs, chars), drv, shell)

      {:io_request, from, replyAs, req} when is_pid(from) ->
        io_request(req, from, replyAs, drv, shell, [])
        get_password1({chars, []}, drv, shell)

      {:reply, {{from, replyAs}, reply}} ->
        io_reply(from, replyAs, reply)
        get_password1({chars, []}, drv, shell)

      {:EXIT, ^drv, :interrupt} ->
        :interrupted

      {:EXIT, ^drv, _} ->
        :terminated

      {:EXIT, ^shell, r} ->
        exit(r)
    end
  end

  defp get_password1({chars, rest}, drv, _Shell) do
    send_drv_reqs(drv, [{:put_chars, :unicode, '\n'}])

    {:done, :lists.reverse(chars),
     case rest do
       :done ->
         []

       _ ->
         rest
     end}
  end

  defp edit_password([], chars) do
    {chars, []}
  end

  defp edit_password([?\r], chars) do
    {chars, :done}
  end

  defp edit_password([?\r | cs], chars) do
    {chars, cs}
  end

  defp edit_password([?\d | cs], []) do
    edit_password(cs, [])
  end

  defp edit_password([?\d | cs], [_ | chars]) do
    edit_password(cs, chars)
  end

  defp edit_password([char | cs], chars) do
    edit_password(cs, [char | chars])
  end

  defp prompt_bytes(prompt, encoding) do
    :lists.flatten(:io_lib.format_prompt(prompt, encoding))
  end

  defp cast(l, :binary, :latin1) when is_list(l) do
    :erlang.list_to_binary(l)
  end

  defp cast(l, :list, :latin1) when is_list(l) do
    :erlang.binary_to_list(:erlang.list_to_binary(l))
  end

  defp cast(l, :binary, :unicode) when is_list(l) do
    :unicode.characters_to_binary(l, :utf8)
  end

  defp cast(other, _, _) do
    other
  end

  defp append(b, l, :latin1) when is_binary(b) do
    :erlang.binary_to_list(b) ++ l
  end

  defp append(b, l, :unicode) when is_binary(b) do
    :unicode.characters_to_list(b, :utf8) ++ l
  end

  defp append(l1, l2, _) when is_list(l1) do
    l1 ++ l2
  end

  defp append(_Eof, l, _) do
    l
  end
end
