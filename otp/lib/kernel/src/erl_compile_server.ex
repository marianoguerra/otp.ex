defmodule :m_erl_compile_server do
  use Bitwise
  @behaviour :gen_server
  require Record
  Record.defrecord(:r_st, :st, cwd: [], config: :undefined, timeout: 10 * 1000, jobs: %{})

  def start_link() do
    :gen_server.start_link({:local, :erl_compile_server}, :erl_compile_server, [], [])
  end

  def init([]) do
    path =
      for d <- :code.get_path(), d !== '.' do
        d
      end

    true = :code.set_path(path)
    config = init_config()
    {:ok, r_st(config: config), 10 * 1000}
  end

  def compile(parameters) do
    :gen_server.call(:erl_compile_server, {:compile, parameters}, :infinity)
  end

  def handle_call({:compile, parameters}, from, r_st(jobs: jobs) = st0) do
    {erlcArgs, pathArgs} = parse_command_line(parameters)

    case verify_context(pathArgs, parameters, st0) do
      {:ok, st1} ->
        %{:cwd => cwd, :encoding => enc} = parameters

        pidRef =
          spawn_monitor(fn ->
            exit(do_compile(erlcArgs, cwd, enc))
          end)

        st = r_st(st1, jobs: %{jobs | pidRef => from})
        {:noreply, r_st(st, timeout: 10 * 1000)}

      :wrong_config ->
        case map_size(jobs) do
          0 ->
            :erlang.halt()

          _ ->
            {:reply, :wrong_config, r_st(st0, timeout: 1), 1}
        end
    end
  end

  def handle_cast(_, st) do
    {:noreply, st}
  end

  def handle_info(
        {:DOWN, ref, :process, pid, reason},
        r_st(jobs: jobs0) = st0
      ) do
    key = {pid, ref}
    client = :erlang.map_get(key, jobs0)
    jobs = :maps.remove(key, jobs0)
    st = r_st(st0, jobs: jobs)
    :gen_server.reply(client, reason)

    case map_size(jobs) === 0 do
      true ->
        {:noreply, st, r_st(st, :timeout)}

      false ->
        {:noreply, st}
    end
  end

  def handle_info(:timeout, r_st(jobs: jobs))
      when map_size(jobs) === 0 do
    :erlang.halt()
  end

  def handle_info(_, r_st(timeout: timeout) = st) do
    {:noreply, st, timeout}
  end

  defp verify_context(pathArgs, %{:env => env} = parameters, st0) do
    case ensure_cwd(parameters, st0) do
      {:ok, r_st(config: config) = st} ->
        case make_config(pathArgs, env) do
          ^config ->
            {:ok, st}

          _ ->
            :wrong_config
        end

      :wrong_config ->
        :wrong_config
    end
  end

  defp ensure_cwd(%{:cwd => cwd}, r_st(cwd: cwd) = st) do
    {:ok, st}
  end

  defp ensure_cwd(%{:cwd => newCwd}, r_st(jobs: jobs) = st)
       when map_size(jobs) === 0 do
    :ok = :file.set_cwd(newCwd)
    {:ok, r_st(st, cwd: newCwd)}
  end

  defp ensure_cwd(%{}, r_st()) do
    :wrong_config
  end

  defp do_compile(erlcArgs, cwd, enc) do
    gL = create_gl()
    :erlang.group_leader(gL, self())
    result = :erl_compile.compile(erlcArgs, cwd)
    stdOutput = ensure_enc(gl_get_output(gL), enc)

    case result do
      :ok ->
        {:ok, stdOutput}

      {:error, stdErrorOutput0} ->
        stdErrorOutput = ensure_enc(stdErrorOutput0, enc)
        {:error, stdOutput, stdErrorOutput}
    end
  end

  defp parse_command_line(%{:command_line => cmdLine, :cwd => cwd}) do
    parse_command_line_1(cmdLine, cwd, [], [])
  end

  defp parse_command_line_1([['-pa', pa] | t], cwd, paAcc, pzAcc) do
    parse_command_line_1(t, cwd, [pa | paAcc], pzAcc)
  end

  defp parse_command_line_1([['-pz', pz] | t], cwd, paAcc, pzAcc) do
    parse_command_line_1(t, cwd, paAcc, [pz | pzAcc])
  end

  defp parse_command_line_1(['-extra' | erlcArgs], cwd, paAcc, pzAcc) do
    paArgs = clean_path_args(:lists.reverse(paAcc), cwd)
    pzArgs = clean_path_args(:lists.reverse(pzAcc), cwd)
    {erlcArgs, [{:pa, paArgs}, {:pz, pzArgs}]}
  end

  defp parse_command_line_1([_ | t], cwd, paAcc, pzAcc) do
    parse_command_line_1(t, cwd, paAcc, pzAcc)
  end

  defp ensure_enc(chars, :latin1) do
    l = :unicode.characters_to_list(chars, :unicode)

    :unicode.characters_to_binary(
      for x <- l do
        case x do
          high when high > 255 ->
            ['\\x{', :erlang.integer_to_list(x, 16), ?}]

          low ->
            low
        end
      end,
      :unicode,
      :latin1
    )
  end

  defp ensure_enc(chars, _Enc) do
    chars
  end

  defp init_config() do
    envVars = [
      'ERL_AFLAGS',
      'ERL_FLAGS',
      'ERL_ZFLAGS',
      'ERL_COMPILER_OPTIONS',
      'ERL_LIBS',
      'ERLC_CONFIGURATION'
    ]

    env0 =
      for name <- envVars do
        {name, :os.getenv(name)}
      end

    env =
      for {_, val} = p <- env0, val !== false do
        p
      end

    {:ok, cwd} = :file.get_cwd()

    make_config(
      [get_path_arg(:pa, cwd), get_path_arg(:pz, cwd)],
      env
    )
  end

  defp get_path_arg(pathArg, cwd) do
    case :init.get_argument(pathArg) do
      :error ->
        {pathArg, []}

      {:ok, paths0} ->
        paths1 = :lists.append(paths0)
        paths = clean_path_args(paths1, cwd)
        {pathArg, paths}
    end
  end

  defp clean_path_args(pathArgs, cwd) do
    for p <- pathArgs do
      :filename.absname(p, cwd)
    end
  end

  defp make_config(pathArgs, env0) do
    env = :lists.sort(env0)

    pathArgs ++
      [
        :erlang.iolist_to_binary(
          for {name, val} <- env do
            [name, ?=, val, ?\n]
          end
        )
      ]
  end

  defp create_gl() do
    spawn_link(fn ->
      gl_loop([])
    end)
  end

  defp gl_get_output(gL) do
    send(gL, {self(), :get_output})

    receive do
      {^gL, output} ->
        output
    end
  end

  defp gl_loop(state0) do
    receive do
      {:io_request, from, replyAs, request} ->
        {_Tag, reply, state} = gl_request(request, state0)
        gl_reply(from, replyAs, reply)
        gl_loop(state)

      {from, :get_output} ->
        output = :erlang.iolist_to_binary(state0)
        send(from, {self(), output})
        gl_loop(state0)

      _Unknown ->
        gl_loop(state0)
    end
  end

  defp gl_reply(from, replyAs, reply) do
    send(from, {:io_reply, replyAs, reply})
    :ok
  end

  defp gl_request({:put_chars, encoding, chars}, state) do
    gl_put_chars(
      :unicode.characters_to_binary(
        chars,
        encoding
      ),
      state
    )
  end

  defp gl_request(
         {:put_chars, encoding, module, function, args},
         state
       ) do
    try do
      gl_request(
        {:put_chars, encoding, apply(module, function, args)},
        state
      )
    catch
      _, _ ->
        {{:error, function}, state}
    end
  end

  defp gl_request({:requests, reqs}, state) do
    gl_multi_request(reqs, {:ok, state})
  end

  defp gl_request(_Other, state) do
    {:error, {:error, :request}, state}
  end

  defp gl_multi_request([r | rs], {:ok, state}) do
    gl_multi_request(rs, gl_request(r, state))
  end

  defp gl_multi_request([_ | _], error) do
    error
  end

  defp gl_multi_request([], result) do
    result
  end

  defp gl_put_chars(chars, output) do
    {:ok, :ok, [output, chars]}
  end
end
