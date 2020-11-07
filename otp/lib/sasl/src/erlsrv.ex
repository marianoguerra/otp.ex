defmodule :m_erlsrv do
  use Bitwise

  def erlsrv(eVer) do
    root = :code.root_dir()
    :filename.join([root, 'erts-' ++ eVer, 'bin', 'erlsrv.exe'])
  end

  defp current_version() do
    hd(:string.lexemes(:erlang.system_info(:version), '_ '))
  end

  defp run_erlsrv(command) do
    run_erlsrv(current_version(), command)
  end

  defp run_erlsrv(eVer, command) do
    case (try do
            :erlang.open_port(
              {:spawn, '"' ++ erlsrv(eVer) ++ '" ' ++ command},
              [{:line, 1000}, :in, :eof]
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, {reason, _}} ->
        {:port_error, reason}

      port ->
        case read_all_data(port) do
          [] ->
            :failed

          x ->
            {:ok, x}
        end
    end
  end

  defp run_erlsrv_interactive(eVer, commands) do
    case (try do
            :erlang.open_port(
              {:spawn, '"' ++ erlsrv(eVer) ++ '" readargs'},
              [{:line, 1000}, :eof]
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, {reason, _}} ->
        {:port_error, reason}

      port ->
        write_all_data(port, commands)

        case read_all_data(port) do
          [] ->
            :failed

          x ->
            {:ok, x}
        end
    end
  end

  defp write_all_data(port, []) do
    send(port, {self(), {:command, :io_lib.nl()}})
    :ok
  end

  defp write_all_data(port, [h | t]) do
    send(port, {self(), {:command, :unicode.characters_to_binary([h, :io_lib.nl()])}})
    write_all_data(port, t)
  end

  defp read_all_data(port) do
    data0 = :lists.reverse(read_all_data(port, [], []))

    for data <- data0 do
      :unicode.characters_to_list(:erlang.list_to_binary(data))
    end
  end

  defp read_all_data(port, line, lines) do
    receive do
      {^port, {:data, {:noeol, data}}} ->
        read_all_data(port, line ++ data, lines)

      {^port, {:data, {:eol, data}}} ->
        read_all_data(port, [], [line ++ data | lines])

      {^port, _Other} ->
        send(port, {self(), :close})

        receive do
          {^port, :closed} ->
            case line do
              [] ->
                lines

              _ ->
                [line | lines]
            end
        end
    end
  end

  def get_all_services() do
    case run_erlsrv('list') do
      :failed ->
        []

      {:ok, [_]} ->
        []

      {:ok, [_H | t]} ->
        f = fn x ->
          hd(:string.lexemes(x, '\t '))
        end

        :lists.map(f, t)

      _ ->
        {:error, :external_program_failed}
    end
  end

  def disable_service(serviceName) do
    disable_service(current_version(), serviceName)
  end

  def disable_service(eVer, serviceName) do
    run_erlsrv(eVer, 'disable ' ++ serviceName)
  end

  def enable_service(serviceName) do
    enable_service(current_version(), serviceName)
  end

  def enable_service(eVer, serviceName) do
    run_erlsrv(eVer, 'enable ' ++ serviceName)
  end

  def remove_service(serviceName) do
    run_erlsrv('remove ' ++ serviceName)
  end

  def rename_service(fromName, toName) do
    rename_service(current_version(), fromName, toName)
  end

  def rename_service(eVer, fromName, toName) do
    run_erlsrv(eVer, 'rename ' ++ fromName ++ ' ' ++ toName)
  end

  def get_service(serviceName) do
    get_service(current_version(), serviceName)
  end

  def get_service(eVer, serviceName) do
    case run_erlsrv(eVer, 'list ' ++ serviceName) do
      :failed ->
        {:error, :no_such_service}

      {:port_error, reason} ->
        {:error, {:port_error, reason}}

      {:ok, data} ->
        table = [
          {'Service name', :servicename, []},
          {'StopAction', :stopaction, []},
          {'OnFail', :onfail, 'ignore'},
          {'Machine', :machine, []},
          {'WorkDir', :workdir, []},
          {'SName', :sname, []},
          {'Name', :name, []},
          {'Priority', :priority, 'default'},
          {'DebugType', :debugtype, 'none'},
          {'Args', :args, []},
          {'InternalServiceName', :internalservicename, []},
          {'Comment', :comment, []}
        ]

        f = fn x ->
          {name, value} = splitline(x)

          case :lists.keysearch(name, 1, table) do
            {:value, {^name, _Atom, ^value}} ->
              []

            {:value, {^name, atom, _}} ->
              {atom, value}

            _ ->
              []
          end
        end

        {before, after__} = split_by_env(data)
        firstPass = :lists.flatten(:lists.map(f, before))
        secondPass = split_arglist(firstPass)

        envParts =
          :lists.map(
            fn s ->
              x = :string.trim(s, :leading, '$\t')

              case hd(:string.lexemes(x, '=')) do
                ^x ->
                  {x, ''}

                y ->
                  {y, :lists.sublist(x, length(y) + 2, length(x))}
              end
            end,
            after__
          )

        case envParts do
          [] ->
            secondPass

          _ ->
            :lists.append(secondPass, [{:env, envParts}])
        end
    end
  end

  def store_service(service) do
    store_service(current_version(), service)
  end

  def store_service(emulatorVersion, service) do
    case :lists.keysearch(:servicename, 1, service) do
      false ->
        {:error, :no_servicename}

      {:value, {_, name}} ->
        {action, service1} =
          case get_service(
                 emulatorVersion,
                 name
               ) do
            {:error, :no_such_service} ->
              {'add', service}

            _ ->
              {'set', :lists.keydelete(:internalservicename, 1, service)}
          end

        commands = [action | build_commands(name, service1)]

        case run_erlsrv_interactive(
               emulatorVersion,
               commands
             ) do
          {:ok, _} ->
            :ok

          x ->
            {:error, x}
        end

      _ ->
        {:error, :malformed_description}
    end
  end

  defp build_commands(action, service) do
    [action | :lists.reverse(build_commands2(service, []))]
  end

  defp build_commands2([], a) do
    a
  end

  defp build_commands2([{:env, []} | t], a) do
    build_commands2(t, a)
  end

  defp build_commands2([{:env, [{var, val} | et]} | t], a) do
    build_commands2(
      [{:env, et} | t],
      [[var ++ '=' ++ val, '-env'] | a]
    )
  end

  defp build_commands2([{:servicename, _} | t], a) do
    build_commands2(t, a)
  end

  defp build_commands2([{atom, []} | t], a) do
    build_commands2(
      t,
      ['-' ++ :erlang.atom_to_list(atom) | a]
    )
  end

  defp build_commands2([{:args, l} | t], a) do
    build_commands2(t, [[concat_args(l), '-args'] | a])
  end

  defp build_commands2([{atom, value} | t], a) do
    build_commands2(
      t,
      [[value, '-' ++ :erlang.atom_to_list(atom)] | a]
    )
  end

  defp concat_args([h | t]) do
    h ++ concat_args2(t)
  end

  defp concat_args2([]) do
    ''
  end

  defp concat_args2([h | t]) do
    ' ' ++ h ++ concat_args2(t)
  end

  def new_service(newServiceName, oldService, data) do
    new_service(newServiceName, oldService, data, [])
  end

  def new_service(newServiceName, oldService, data, restartName) do
    tmp0 = :lists.keydelete(:internalservicename, 1, oldService)
    tmp1 = :lists.keyreplace(:servicename, 1, tmp0, {:servicename, newServiceName})

    tmp =
      case :lists.keysearch(:env, 1, tmp1) do
        {:value, {:env, env0}} ->
          env1 = :lists.keydelete('ERLSRV_SERVICE_NAME', 1, env0)
          :lists.keyreplace(:env, 1, tmp1, {:env, [{'ERLSRV_SERVICE_NAME', restartName} | env1]})

        _ ->
          tmp1
      end

    argsTmp =
      case :lists.keysearch(:args, 1, tmp) do
        false ->
          []

        {:value, {:args, oldArgs}} ->
          oldArgs
      end

    args = backstrip(argsTmp, '++')

    {found, tail} =
      :lists.foldr(
        fn a, {flag, accIn} ->
          case {flag, a} do
            {true, _} ->
              {flag, accIn}

            {false, '++'} ->
              {true, accIn}

            _ ->
              {false, [a | accIn]}
          end
        end,
        {false, []},
        args
      )

    {otherFlags, _DataDir} =
      case found do
        true ->
          check_tail(tail)

        false ->
          {[], false}
      end

    newArgs1 =
      case data do
        [] ->
          otherFlags

        _ ->
          [['-data', data] | otherFlags]
      end

    case found do
      false ->
        a =
          case newArgs1 do
            [] ->
              []

            _ ->
              ['++' | newArgs1]
          end

        case {args, a} do
          {[], []} ->
            tmp

          {[], _} ->
            tmp ++ [{:args, a}]

          {_, _} ->
            :lists.keyreplace(:args, 1, tmp, {:args, args ++ a})
        end

      true ->
        stripArgs = backstrip(args, ['++' | tail])

        newArgs2 =
          case newArgs1 do
            [] ->
              []

            _ ->
              ['++' | newArgs1]
          end

        newArgs = stripArgs ++ newArgs2
        :lists.keyreplace(:args, 1, tmp, {:args, newArgs})
    end
  end

  defp backstrip(list, tail) do
    :lists.reverse(
      backstrip2(
        :lists.reverse(list),
        :lists.reverse(tail)
      )
    )
  end

  defp backstrip2([a | t1], [a | t2]) do
    backstrip2(t1, t2)
  end

  defp backstrip2(l, _) do
    l
  end

  defp check_tail(tail) do
    {a, b} = check_tail(tail, [], false)
    {:lists.reverse(a), b}
  end

  defp check_tail([], otherFlags, dataDir) do
    {otherFlags, dataDir}
  end

  defp check_tail([['-data', theDataDir] | t], otherFlags, _DataDir) do
    check_tail(t, otherFlags, theDataDir)
  end

  defp check_tail([h | t], otherFlags, dataDir) do
    check_tail(t, [h | otherFlags], dataDir)
  end

  defp split_arglist([]) do
    []
  end

  defp split_arglist([{:args, str} | t]) do
    [{:args, parse_arglist(str)} | t]
  end

  defp split_arglist([h | t]) do
    [h | split_arglist(t)]
  end

  defp parse_arglist(str) do
    :lists.reverse(parse_arglist(str, []))
  end

  defp parse_arglist(str, accum) do
    stripped = :string.trim(str, :leading)

    case length(stripped) do
      0 ->
        accum

      _ ->
        {next, rest} = pick_argument(str)
        parse_arglist(rest, [next | accum])
    end
  end

  defp pick_argument(str) do
    {rev, rest} = pick_argument(:normal, str, [])
    {:lists.reverse(rev), rest}
  end

  defp pick_argument(_, [], acc) do
    {acc, ''}
  end

  defp pick_argument(:normal, [?\s | t], acc) do
    {acc, t}
  end

  defp pick_argument(:normal, [?\\ | t], acc) do
    pick_argument(:normal_escaped, t, [?\\ | acc])
  end

  defp pick_argument(:normal, [?" | t], acc) do
    pick_argument(:quoted, t, [?" | acc])
  end

  defp pick_argument(:normal_escaped, [?" | t], acc) do
    pick_argument(:bquoted, t, [?" | acc])
  end

  defp pick_argument(:normal_escaped, [a | t], acc) do
    pick_argument(:normal, t, [a | acc])
  end

  defp pick_argument(:quoted_escaped, [h | t], acc) do
    pick_argument(:quoted, t, [h | acc])
  end

  defp pick_argument(:quoted, [?" | t], acc) do
    pick_argument(:normal, t, [?" | acc])
  end

  defp pick_argument(:quoted, [?\\ | t], acc) do
    pick_argument(:quoted_escaped, t, [?\\ | acc])
  end

  defp pick_argument(:quoted, [h | t], acc) do
    pick_argument(:quoted, t, [h | acc])
  end

  defp pick_argument(:bquoted_escaped, [?" | t], acc) do
    pick_argument(:normal, t, [?" | acc])
  end

  defp pick_argument(:bquoted_escaped, [h | t], acc) do
    pick_argument(:bquoted, t, [h | acc])
  end

  defp pick_argument(:bquoted, [?\\ | t], acc) do
    pick_argument(:bquoted_escaped, t, [?\\ | acc])
  end

  defp pick_argument(:bquoted, [h | t], acc) do
    pick_argument(:bquoted, t, [h | acc])
  end

  defp pick_argument(:normal, [h | t], acc) do
    pick_argument(:normal, t, [h | acc])
  end

  defp split_helper('Env:', {where, 0}) do
    {where + 1, where}
  end

  defp split_helper(_, {where, pos}) do
    {where + 1, pos}
  end

  defp split_by_env(data) do
    case :lists.foldl(&split_helper/2, {0, 0}, data) do
      {_, 0} ->
        {data, []}

      {len, pos} ->
        {:lists.sublist(data, pos), :lists.sublist(data, pos + 2, len)}
    end
  end

  defp splitline(line) do
    case :string.split(line, ':') do
      [_] ->
        {line, ''}

      [n, v] ->
        {n, :string.slice(v, 1)}
    end
  end
end
