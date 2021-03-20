defmodule :cowboy_router do
  use Bitwise
  @behaviour :cowboy_middleware
  def compile(routes) do
    compile(routes, [])
  end

  defp compile([], acc) do
    :lists.reverse(acc)
  end

  defp compile([{host, paths} | tail], acc) do
    compile([{host, [], paths} | tail], acc)
  end

  defp compile([{hostMatch, fields, paths} | tail], acc) do
    hostRules = (case (hostMatch) do
                   :_ ->
                     :_
                   _ ->
                     compile_host(hostMatch)
                 end)
    pathRules = compile_paths(paths, [])
    hosts = (case (hostRules) do
               :_ ->
                 [{:_, fields, pathRules}]
               _ ->
                 for r <- hostRules do
                   {r, fields, pathRules}
                 end
             end)
    compile(tail, hosts ++ acc)
  end

  defp compile_host(hostMatch) when is_list(hostMatch) do
    compile_host(:erlang.list_to_binary(hostMatch))
  end

  defp compile_host(hostMatch) when is_binary(hostMatch) do
    compile_rules(hostMatch, ?., [], [], <<>>)
  end

  defp compile_paths([], acc) do
    :lists.reverse(acc)
  end

  defp compile_paths([{pathMatch, handler, opts} | tail], acc) do
    compile_paths([{pathMatch, [], handler, opts} | tail],
                    acc)
  end

  defp compile_paths([{pathMatch, fields, handler, opts} | tail],
            acc)
      when is_list(pathMatch) do
    compile_paths([{:erlang.iolist_to_binary(pathMatch),
                      fields, handler, opts} |
                       tail],
                    acc)
  end

  defp compile_paths([{:_, fields, handler, opts} | tail], acc) do
    compile_paths(tail,
                    [{:_, fields, handler, opts}] ++ acc)
  end

  defp compile_paths([{"*", fields, handler, opts} | tail], acc) do
    compile_paths(tail, [{"*", fields, handler, opts} | acc])
  end

  defp compile_paths([{<<?/, pathMatch :: bits>>, fields, handler,
              opts} |
               tail],
            acc) do
    pathRules = compile_rules(pathMatch, ?/, [], [], <<>>)
    paths = (for r <- pathRules do
               {:lists.reverse(r), fields, handler, opts}
             end)
    compile_paths(tail, paths ++ acc)
  end

  defp compile_paths([{pathMatch, _, _, _} | _], _) do
    :erlang.error({:badarg,
                     'The following route MUST begin with a slash: ' ++ :erlang.binary_to_list(pathMatch)})
  end

  defp compile_rules(<<>>, _, segments, rules, <<>>) do
    [segments | rules]
  end

  defp compile_rules(<<>>, _, segments, rules, acc) do
    [[acc | segments] | rules]
  end

  defp compile_rules(<<s, rest :: bits>>, s, segments, rules,
            <<>>) do
    compile_rules(rest, s, segments, rules, <<>>)
  end

  defp compile_rules(<<s, rest :: bits>>, s, segments, rules, acc) do
    compile_rules(rest, s, [acc | segments], rules, <<>>)
  end

  defp compile_rules(<<?:, rest :: bits>>, s, segments, rules,
            <<>>) do
    {nameBin, rest2} = compile_binding(rest, s, <<>>)
    name = :erlang.binary_to_atom(nameBin, :utf8)
    compile_rules(rest2, s, segments, rules, name)
  end

  defp compile_rules(<<?[, ?., ?., ?., ?], rest :: bits>>, s,
            segments, rules, acc)
      when acc === <<>> do
    compile_rules(rest, s, [:"..." | segments], rules, acc)
  end

  defp compile_rules(<<?[, ?., ?., ?., ?], rest :: bits>>, s,
            segments, rules, acc) do
    compile_rules(rest, s, [:"...", acc | segments], rules, acc)
  end

  defp compile_rules(<<?[, s, rest :: bits>>, s, segments, rules,
            acc) do
    compile_brackets(rest, s, [acc | segments], rules)
  end

  defp compile_rules(<<?[, rest :: bits>>, s, segments, rules,
            <<>>) do
    compile_brackets(rest, s, segments, rules)
  end

  defp compile_rules(<<?[, _ :: bits>>, _, _, _, _) do
    :erlang.error(:badarg)
  end

  defp compile_rules(<<?], _ :: bits>>, _, _, _, _) do
    :erlang.error(:badarg)
  end

  defp compile_rules(<<c, rest :: bits>>, s, segments, rules, acc) do
    compile_rules(rest, s, segments, rules,
                    <<acc :: binary, c>>)
  end

  defp compile_binding(<<>>, _, <<>>) do
    :erlang.error(:badarg)
  end

  defp compile_binding(rest = <<>>, _, acc) do
    {acc, rest}
  end

  defp compile_binding(rest = <<c, _ :: bits>>, s, acc) when c === s or
                                                  c === ?[ or c === ?] do
    {acc, rest}
  end

  defp compile_binding(<<c, rest :: bits>>, s, acc) do
    compile_binding(rest, s, <<acc :: binary, c>>)
  end

  defp compile_brackets(rest, s, segments, rules) do
    {bracket, rest2} = compile_brackets_split(rest, <<>>, 0)
    rules1 = compile_rules(rest2, s, segments, [], <<>>)
    rules2 = compile_rules(<<bracket :: binary,
                               rest2 :: binary>>,
                             s, segments, [], <<>>)
    rules ++ rules2 ++ rules1
  end

  defp compile_brackets_split(<<>>, _, _) do
    :erlang.error(:badarg)
  end

  defp compile_brackets_split(<<c, rest :: bits>>, acc, n) when c === ?[ do
    compile_brackets_split(rest, <<acc :: binary, c>>,
                             n + 1)
  end

  defp compile_brackets_split(<<c, rest :: bits>>, acc, n) when (c === ?] and
                                               n > 0) do
    compile_brackets_split(rest, <<acc :: binary, c>>,
                             n - 1)
  end

  defp compile_brackets_split(<<?], rest :: bits>>, acc, 0) do
    {acc, rest}
  end

  defp compile_brackets_split(<<c, rest :: bits>>, acc, n) do
    compile_brackets_split(rest, <<acc :: binary, c>>, n)
  end

  def execute(req = %{host: host, path: path},
           env = %{dispatch: dispatch0}) do
    dispatch = (case (dispatch0) do
                  {:persistent_term, key} ->
                    :persistent_term.get(key)
                  _ ->
                    dispatch0
                end)
    case (match(dispatch, host, path)) do
      {:ok, handler, handlerOpts, bindings, hostInfo,
         pathInfo} ->
        {:ok,
           Map.merge(req, %{host_info: hostInfo,
                              path_info: pathInfo, bindings: bindings}),
           Map.merge(env, %{handler: handler,
                              handler_opts: handlerOpts})}
      {:error, :notfound, :host} ->
        {:stop, :cowboy_req.reply(400, req)}
      {:error, :badrequest, :path} ->
        {:stop, :cowboy_req.reply(400, req)}
      {:error, :notfound, :path} ->
        {:stop, :cowboy_req.reply(404, req)}
    end
  end

  defp match([], _, _) do
    {:error, :notfound, :host}
  end

  defp match([{:_, [], pathMatchs} | _Tail], _, path) do
    match_path(pathMatchs, :undefined, path, %{})
  end

  defp match([{hostMatch, fields, pathMatchs} | tail],
            tokens, path)
      when is_list(tokens) do
    case (list_match(tokens, hostMatch, %{})) do
      false ->
        match(tail, tokens, path)
      {true, bindings, hostInfo} ->
        hostInfo2 = (case (hostInfo) do
                       :undefined ->
                         :undefined
                       _ ->
                         :lists.reverse(hostInfo)
                     end)
        case (check_constraints(fields, bindings)) do
          {:ok, bindings2} ->
            match_path(pathMatchs, hostInfo2, path, bindings2)
          :nomatch ->
            match(tail, tokens, path)
        end
    end
  end

  defp match(dispatch, host, path) do
    match(dispatch, split_host(host), path)
  end

  defp match_path([], _, _, _) do
    {:error, :notfound, :path}
  end

  defp match_path([{:_, [], handler, opts} | _Tail], hostInfo, _,
            bindings) do
    {:ok, handler, opts, bindings, hostInfo, :undefined}
  end

  defp match_path([{"*", _, handler, opts} | _Tail], hostInfo, "*",
            bindings) do
    {:ok, handler, opts, bindings, hostInfo, :undefined}
  end

  defp match_path([_ | tail], hostInfo, "*", bindings) do
    match_path(tail, hostInfo, "*", bindings)
  end

  defp match_path([{pathMatch, fields, handler, opts} | tail],
            hostInfo, tokens, bindings)
      when is_list(tokens) do
    case (list_match(tokens, pathMatch, bindings)) do
      false ->
        match_path(tail, hostInfo, tokens, bindings)
      {true, pathBinds, pathInfo} ->
        case (check_constraints(fields, pathBinds)) do
          {:ok, pathBinds2} ->
            {:ok, handler, opts, pathBinds2, hostInfo, pathInfo}
          :nomatch ->
            match_path(tail, hostInfo, tokens, bindings)
        end
    end
  end

  defp match_path(_Dispatch, _HostInfo, :badrequest, _Bindings) do
    {:error, :badrequest, :path}
  end

  defp match_path(dispatch, hostInfo, path, bindings) do
    match_path(dispatch, hostInfo, split_path(path),
                 bindings)
  end

  defp check_constraints([], bindings) do
    {:ok, bindings}
  end

  defp check_constraints([field | tail], bindings) when is_atom(field) do
    check_constraints(tail, bindings)
  end

  defp check_constraints([field | tail], bindings) do
    name = :erlang.element(1, field)
    case (bindings) do
      %{^name => value0} ->
        constraints = :erlang.element(2, field)
        case (:cowboy_constraints.validate(value0,
                                             constraints)) do
          {:ok, value} ->
            check_constraints(tail, Map.put(bindings, name, value))
          {:error, _} ->
            :nomatch
        end
      _ ->
        check_constraints(tail, bindings)
    end
  end

  defp split_host(host) do
    split_host(host, [])
  end

  defp split_host(host, acc) do
    case (:binary.match(host, ".")) do
      :nomatch when host === <<>> ->
        acc
      :nomatch ->
        [host | acc]
      {pos, _} ->
        <<segment :: size(pos) - binary, _ :: size(8),
            rest :: bits>> = host
        false = byte_size(segment) == 0
        split_host(rest, [segment | acc])
    end
  end

  defp split_path(<<?/, path :: bits>>) do
    split_path(path, [])
  end

  defp split_path(_) do
    :badrequest
  end

  defp split_path(path, acc) do
    try do
      case (:binary.match(path, "/")) do
        :nomatch when path === <<>> ->
          remove_dot_segments(:lists.reverse(for s <- acc do
                                               :cow_uri.urldecode(s)
                                             end),
                                [])
        :nomatch ->
          remove_dot_segments(:lists.reverse(for s <- [path |
                                                           acc] do
                                               :cow_uri.urldecode(s)
                                             end),
                                [])
        {pos, _} ->
          <<segment :: size(pos) - binary, _ :: size(8),
              rest :: bits>> = path
          split_path(rest, [segment | acc])
      end
    catch
      :error, _ ->
        :badrequest
    end
  end

  defp remove_dot_segments([], acc) do
    :lists.reverse(acc)
  end

  defp remove_dot_segments(["." | segments], acc) do
    remove_dot_segments(segments, acc)
  end

  defp remove_dot_segments([".." | segments], acc = []) do
    remove_dot_segments(segments, acc)
  end

  defp remove_dot_segments([".." | segments], [_ | acc]) do
    remove_dot_segments(segments, acc)
  end

  defp remove_dot_segments([s | segments], acc) do
    remove_dot_segments(segments, [s | acc])
  end

  defp list_match(list, [:"..."], binds) do
    {true, binds, list}
  end

  defp list_match([_E | tail], [:_ | tailMatch], binds) do
    list_match(tail, tailMatch, binds)
  end

  defp list_match([e | tail], [e | tailMatch], binds) do
    list_match(tail, tailMatch, binds)
  end

  defp list_match([e | tail], [v | tailMatch], binds)
      when is_atom(v) do
    case (binds) do
      %{^v => ^e} ->
        list_match(tail, tailMatch, binds)
      %{^v => _} ->
        false
      _ ->
        list_match(tail, tailMatch, Map.put(binds, v, e))
    end
  end

  defp list_match([], [], binds) do
    {true, binds, :undefined}
  end

  defp list_match(_List, _Match, _Binds) do
    false
  end

end