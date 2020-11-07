defmodule :m_proc_lib do
  use Bitwise
  import Kernel, except: [spawn: 3, spawn_link: 3, spawn: 1, spawn_link: 1]

  def spawn(f) when is_function(f) do
    parent = get_my_name()
    ancestors = get_ancestors()
    :erlang.spawn(:proc_lib, :init_p, [parent, ancestors, f])
  end

  def spawn(m, f, a)
      when is_atom(m) and is_atom(f) and
             is_list(a) do
    parent = get_my_name()
    ancestors = get_ancestors()
    :erlang.spawn(:proc_lib, :init_p, [parent, ancestors, m, f, a])
  end

  def spawn_link(f) when is_function(f) do
    parent = get_my_name()
    ancestors = get_ancestors()
    :erlang.spawn_link(:proc_lib, :init_p, [parent, ancestors, f])
  end

  def spawn_link(m, f, a)
      when is_atom(m) and is_atom(f) and
             is_list(a) do
    parent = get_my_name()
    ancestors = get_ancestors()
    :erlang.spawn_link(:proc_lib, :init_p, [parent, ancestors, m, f, a])
  end

  def spawn(node, f) when is_function(f) do
    parent = get_my_name()
    ancestors = get_ancestors()
    :erlang.spawn(node, :proc_lib, :init_p, [parent, ancestors, f])
  end

  def spawn(node, m, f, a)
      when is_atom(m) and
             is_atom(f) and is_list(a) do
    parent = get_my_name()
    ancestors = get_ancestors()
    :erlang.spawn(node, :proc_lib, :init_p, [parent, ancestors, m, f, a])
  end

  def spawn_link(node, f) when is_function(f) do
    parent = get_my_name()
    ancestors = get_ancestors()
    :erlang.spawn_link(node, :proc_lib, :init_p, [parent, ancestors, f])
  end

  def spawn_link(node, m, f, a)
      when is_atom(m) and
             is_atom(f) and is_list(a) do
    parent = get_my_name()
    ancestors = get_ancestors()
    :erlang.spawn_link(node, :proc_lib, :init_p, [parent, ancestors, m, f, a])
  end

  def spawn_opt(f, opts) when is_function(f) do
    parent = get_my_name()
    ancestors = get_ancestors()
    :erlang.spawn_opt(:proc_lib, :init_p, [parent, ancestors, f], opts)
  end

  def spawn_opt(node, f, opts) when is_function(f) do
    parent = get_my_name()
    ancestors = get_ancestors()
    :erlang.spawn_opt(node, :proc_lib, :init_p, [parent, ancestors, f], opts)
  end

  def spawn_opt(m, f, a, opts)
      when is_atom(m) and
             is_atom(f) and is_list(a) do
    parent = get_my_name()
    ancestors = get_ancestors()
    :erlang.spawn_opt(:proc_lib, :init_p, [parent, ancestors, m, f, a], opts)
  end

  def spawn_opt(node, m, f, a, opts)
      when is_atom(m) and
             is_atom(f) and is_list(a) do
    parent = get_my_name()
    ancestors = get_ancestors()
    :erlang.spawn_opt(node, :proc_lib, :init_p, [parent, ancestors, m, f, a], opts)
  end

  defp spawn_mon(m, f, a) do
    parent = get_my_name()
    ancestors = get_ancestors()
    :erlang.spawn_monitor(:proc_lib, :init_p, [parent, ancestors, m, f, a])
  end

  def hibernate(m, f, a)
      when is_atom(m) and is_atom(f) and
             is_list(a) do
    :erlang.hibernate(:proc_lib, :wake_up, [m, f, a])
  end

  def init_p(parent, ancestors, fun) when is_function(fun) do
    :erlang.put(:"$ancestors", [parent | ancestors])
    mfa = :erlang.fun_info_mfa(fun)
    :erlang.put(:"$initial_call", mfa)

    try do
      fun.()
    catch
      class, reason ->
        exit_p(class, reason, __STACKTRACE__)
    end
  end

  def init_p(parent, ancestors, m, f, a)
      when is_atom(m) and
             is_atom(f) and is_list(a) do
    :erlang.put(:"$ancestors", [parent | ancestors])
    :erlang.put(:"$initial_call", trans_init(m, f, a))
    init_p_do_apply(m, f, a)
  end

  defp init_p_do_apply(m, f, a) do
    try do
      apply(m, f, a)
    catch
      class, reason ->
        exit_p(class, reason, __STACKTRACE__)
    end
  end

  def wake_up(m, f, a)
      when is_atom(m) and is_atom(f) and
             is_list(a) do
    try do
      apply(m, f, a)
    catch
      class, reason ->
        exit_p(class, reason, __STACKTRACE__)
    end
  end

  defp exit_p(class, reason, stacktrace) do
    case :erlang.get(:"$initial_call") do
      {m, f, a}
      when is_atom(m) and is_atom(f) and
             is_integer(a) ->
        mFA = {m, f, make_dummy_args(a, [])}
        crash_report(class, reason, mFA, stacktrace)
        :erlang.raise(:exit, exit_reason(class, reason, stacktrace), stacktrace)

      _ ->
        crash_report(class, reason, [], stacktrace)
        :erlang.raise(:exit, exit_reason(class, reason, stacktrace), stacktrace)
    end
  end

  defp exit_reason(:error, reason, stacktrace) do
    {reason, stacktrace}
  end

  defp exit_reason(:exit, reason, _Stacktrace) do
    reason
  end

  defp exit_reason(:throw, reason, stacktrace) do
    {{:nocatch, reason}, stacktrace}
  end

  def start(m, f, a)
      when is_atom(m) and is_atom(f) and
             is_list(a) do
    start(m, f, a, :infinity)
  end

  def start(m, f, a, timeout)
      when is_atom(m) and
             is_atom(f) and is_list(a) do
    sync_start(spawn_mon(m, f, a), timeout)
  end

  def start(m, f, a, timeout, spawnOpts)
      when is_atom(m) and
             is_atom(f) and is_list(a) do
    case :lists.member(:monitor, spawnOpts) do
      true ->
        :erlang.error(:badarg, [m, f, a, timeout, spawnOpts])

      false ->
        :ok
    end

    sync_start(
      :proc_lib.spawn_opt(m, f, a, [:monitor | spawnOpts]),
      timeout
    )
  end

  defp sync_start({pid, ref}, timeout) do
    receive do
      {:ack, ^pid, return} ->
        :erlang.demonitor(ref, [:flush])
        return

      {:DOWN, ^ref, :process, ^pid, reason} ->
        {:error, reason}
    after
      timeout ->
        :erlang.demonitor(ref, [:flush])
        kill_flush(pid)
        {:error, :timeout}
    end
  end

  def start_link(m, f, a)
      when is_atom(m) and is_atom(f) and
             is_list(a) do
    start_link(m, f, a, :infinity)
  end

  def start_link(m, f, a, timeout)
      when is_atom(m) and
             is_atom(f) and is_list(a) do
    sync_start_link(:proc_lib.spawn_link(m, f, a), timeout)
  end

  def start_link(m, f, a, timeout, spawnOpts)
      when is_atom(m) and
             is_atom(f) and is_list(a) do
    case :lists.member(:monitor, spawnOpts) do
      true ->
        :erlang.error(:badarg, [m, f, a, timeout, spawnOpts])

      false ->
        :ok
    end

    sync_start_link(
      :proc_lib.spawn_opt(m, f, a, [:link | spawnOpts]),
      timeout
    )
  end

  defp sync_start_link(pid, timeout) do
    receive do
      {:ack, ^pid, return} ->
        return

      {:EXIT, ^pid, reason} ->
        {:error, reason}
    after
      timeout ->
        kill_flush(pid)
        {:error, :timeout}
    end
  end

  def start_monitor(m, f, a)
      when is_atom(m) and is_atom(f) and
             is_list(a) do
    start_monitor(m, f, a, :infinity)
  end

  def start_monitor(m, f, a, timeout)
      when is_atom(m) and
             is_atom(f) and is_list(a) do
    sync_start_monitor(spawn_mon(m, f, a), timeout)
  end

  def start_monitor(m, f, a, timeout, spawnOpts)
      when is_atom(m) and
             is_atom(f) and is_list(a) do
    case :lists.member(:monitor, spawnOpts) do
      true ->
        :erlang.error(:badarg, [m, f, a, timeout, spawnOpts])

      false ->
        :ok
    end

    sync_start_monitor(
      :proc_lib.spawn_opt(m, f, a, [:monitor | spawnOpts]),
      timeout
    )
  end

  defp sync_start_monitor({pid, ref}, timeout) do
    receive do
      {:ack, ^pid, return} ->
        {return, ref}

      {:DOWN, ^ref, :process, ^pid, reason} = down ->
        send(self(), down)
        {{:error, reason}, ref}
    after
      timeout ->
        kill_flush(pid)
        {{:error, :timeout}, ref}
    end
  end

  defp kill_flush(pid) do
    :erlang.unlink(pid)
    :erlang.exit(pid, :kill)

    receive do
      {:EXIT, ^pid, _} ->
        :ok
    after
      0 ->
        :ok
    end

    :ok
  end

  def init_ack(parent, return) do
    send(parent, {:ack, self(), return})
    :ok
  end

  def init_ack(return) do
    [parent | _] = :erlang.get(:"$ancestors")
    init_ack(parent, return)
  end

  def initial_call(dictOrPid) do
    case raw_initial_call(dictOrPid) do
      {m, f, a} ->
        {m, f, make_dummy_args(a, [])}

      false ->
        false
    end
  end

  defp make_dummy_args(0, acc) do
    acc
  end

  defp make_dummy_args(n, acc) do
    arg = :erlang.list_to_atom('Argument__' ++ :erlang.integer_to_list(n))
    make_dummy_args(n - 1, [arg | acc])
  end

  def translate_initial_call(dictOrPid) do
    case raw_initial_call(dictOrPid) do
      {_, _, _} = mFA ->
        mFA

      false ->
        {:proc_lib, :init_p, 5}
    end
  end

  defp raw_initial_call({x, y, z})
       when is_integer(x) and
              is_integer(y) and is_integer(z) do
    raw_initial_call(:c.pid(x, y, z))
  end

  defp raw_initial_call(pid) when is_pid(pid) do
    case get_process_info(pid, :dictionary) do
      {:dictionary, dict} ->
        raw_init_call(dict)

      _ ->
        false
    end
  end

  defp raw_initial_call(procInfo) when is_list(procInfo) do
    case :lists.keyfind(:dictionary, 1, procInfo) do
      {:dictionary, dict} ->
        raw_init_call(dict)

      _ ->
        false
    end
  end

  defp raw_init_call(dict) do
    case :lists.keyfind(:"$initial_call", 1, dict) do
      {_, {_, _, _} = mFA} ->
        mFA

      _ ->
        false
    end
  end

  defp trans_init(:gen, :init_it, [:gen_server, _, _, :supervisor, {_, module, _}, _]) do
    {:supervisor, module, 1}
  end

  defp trans_init(:gen, :init_it, [:gen_server, _, _, _, :supervisor, {_, module, _}, _]) do
    {:supervisor, module, 1}
  end

  defp trans_init(:gen, :init_it, [:gen_server, _, _, :supervisor_bridge, [module | _], _]) do
    {:supervisor_bridge, module, 1}
  end

  defp trans_init(:gen, :init_it, [:gen_server, _, _, _, :supervisor_bridge, [module | _], _]) do
    {:supervisor_bridge, module, 1}
  end

  defp trans_init(:gen, :init_it, [:gen_event | _]) do
    {:gen_event, :init_it, 6}
  end

  defp trans_init(:gen, :init_it, [_GenMod, _, _, module, _, _])
       when is_atom(module) do
    {module, :init, 1}
  end

  defp trans_init(:gen, :init_it, [[_GenMod, _, _, _, module] | _])
       when is_atom(module) do
    {module, :init, 1}
  end

  defp trans_init(m, f, a) when is_atom(m) and is_atom(f) do
    {m, f, length(a)}
  end

  defp crash_report(:exit, :normal, _, _) do
    :ok
  end

  defp crash_report(:exit, :shutdown, _, _) do
    :ok
  end

  defp crash_report(:exit, {:shutdown, _}, _, _) do
    :ok
  end

  defp crash_report(class, reason, startF, stacktrace) do
    case :logger.allow(:error, :proc_lib) do
      true ->
        :erlang.apply(:logger, :macro_log, [
          %{
            :mfa => {:proc_lib, :crash_report, 4},
            :line => 525,
            :file => 'otp/lib/stdlib/src/proc_lib.erl'
          },
          :error,
          %{
            :label => {:proc_lib, :crash},
            :report => [my_info(class, reason, startF, stacktrace), linked_info(self())]
          },
          %{
            :domain => [:otp, :sasl],
            :report_cb => &:proc_lib.report_cb/2,
            :logger_formatter => %{:title => 'CRASH REPORT'},
            :error_logger => %{:tag => :error_report, :type => :crash_report}
          }
        ])

      false ->
        :ok
    end
  end

  defp my_info(class, reason, [], stacktrace) do
    my_info_1(class, reason, stacktrace)
  end

  defp my_info(class, reason, startF, stacktrace) do
    [{:initial_call, startF} | my_info_1(class, reason, stacktrace)]
  end

  defp my_info_1(class, reason, stacktrace) do
    [
      {:pid, self()},
      get_process_info(self(), :registered_name),
      {:error_info, {class, reason, stacktrace}},
      get_ancestors(self()),
      get_process_info(self(), :message_queue_len),
      get_messages(self()),
      get_process_info(self(), :links),
      get_cleaned_dictionary(self()),
      get_process_info(self(), :trap_exit),
      get_process_info(self(), :status),
      get_process_info(self(), :heap_size),
      get_process_info(self(), :stack_size),
      get_process_info(self(), :reductions)
    ]
  end

  defp get_ancestors(pid) do
    case get_dictionary(pid, :"$ancestors") do
      {:"$ancestors", ancestors} ->
        {:ancestors, ancestors}

      _ ->
        {:ancestors, []}
    end
  end

  defp get_messages(pid) do
    messages = get_process_messages(pid)
    {:messages, :error_logger.limit_term(messages)}
  end

  defp get_process_messages(pid) do
    depth = :error_logger.get_format_depth()

    case pid !== self() or depth === :unlimited do
      true ->
        {:messages, messages} = get_process_info(pid, :messages)
        messages

      false ->
        receive_messages(depth)
    end
  end

  defp receive_messages(0) do
    []
  end

  defp receive_messages(n) do
    receive do
      m ->
        [m | receive_messages(n - 1)]
    after
      0 ->
        []
    end
  end

  defp get_cleaned_dictionary(pid) do
    case get_process_info(pid, :dictionary) do
      {:dictionary, dict} ->
        {:dictionary, cleaned_dict(dict)}

      _ ->
        {:dictionary, []}
    end
  end

  defp cleaned_dict(dict) do
    cleanDict = clean_dict(dict)
    :error_logger.limit_term(cleanDict)
  end

  defp clean_dict([{:"$ancestors", _} | dict]) do
    clean_dict(dict)
  end

  defp clean_dict([{:"$initial_call", _} | dict]) do
    clean_dict(dict)
  end

  defp clean_dict([e | dict]) do
    [e | clean_dict(dict)]
  end

  defp clean_dict([]) do
    []
  end

  defp get_dictionary(pid, tag) do
    case get_process_info(pid, :dictionary) do
      {:dictionary, dict} ->
        case :lists.keysearch(tag, 1, dict) do
          {:value, value} ->
            value

          _ ->
            :undefined
        end

      _ ->
        :undefined
    end
  end

  defp linked_info(pid) do
    make_neighbour_reports1(neighbours(pid))
  end

  defp make_neighbour_reports1([p | ps]) do
    reportBody = make_neighbour_report(p)

    case :lists.member(:undefined, reportBody) do
      true ->
        make_neighbour_reports1(ps)

      false ->
        [{:neighbour, reportBody} | make_neighbour_reports1(ps)]
    end
  end

  defp make_neighbour_reports1([]) do
    []
  end

  defp make_neighbour_report(pid) do
    [
      {:pid, pid},
      get_process_info(pid, :registered_name),
      get_initial_call(pid),
      get_process_info(pid, :current_function),
      get_ancestors(pid),
      get_process_info(pid, :message_queue_len),
      get_process_info(pid, :links),
      get_process_info(pid, :trap_exit),
      get_process_info(pid, :status),
      get_process_info(pid, :heap_size),
      get_process_info(pid, :stack_size),
      get_process_info(pid, :reductions),
      get_process_info(pid, :current_stacktrace)
    ]
  end

  defp get_initial_call(pid) do
    case get_dictionary(pid, :"$initial_call") do
      {:"$initial_call", {m, f, a}} ->
        {:initial_call, {m, f, make_dummy_args(a, [])}}

      _ ->
        get_process_info(pid, :initial_call)
    end
  end

  defp neighbours(pid) do
    {_, visited} =
      visit(
        adjacents(pid),
        {max_neighbours(), [pid]}
      )

    :lists.delete(pid, visited)
  end

  defp max_neighbours() do
    15
  end

  defp visit([p | ps], {n, vs} = nVs) when n > 0 do
    case :lists.member(p, vs) do
      false ->
        visit(adjacents(p), visit(ps, {n - 1, [p | vs]}))

      true ->
        visit(ps, nVs)
    end
  end

  defp visit(_, {_N, _Vs} = nVs) do
    nVs
  end

  defp adjacents(pid) do
    case (try do
            proc_info(pid, :links)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:links, links} ->
        no_trap(links)

      _ ->
        []
    end
  end

  defp no_trap([p | ps]) do
    case (try do
            proc_info(p, :trap_exit)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:trap_exit, false} ->
        [p | no_trap(ps)]

      _ ->
        no_trap(ps)
    end
  end

  defp no_trap([]) do
    []
  end

  defp get_process_info(pid, tag) do
    translate_process_info(
      tag,
      try do
        proc_info(pid, tag)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end
    )
  end

  defp translate_process_info(:registered_name, []) do
    {:registered_name, []}
  end

  defp translate_process_info(_, {:EXIT, _}) do
    :undefined
  end

  defp translate_process_info(_, result) do
    result
  end

  defp get_my_name() do
    case proc_info(self(), :registered_name) do
      {:registered_name, name} ->
        name

      _ ->
        self()
    end
  end

  defp get_ancestors() do
    case :erlang.get(:"$ancestors") do
      a when is_list(a) ->
        a

      _ ->
        []
    end
  end

  defp proc_info(pid, item) when node(pid) === node() do
    :erlang.process_info(pid, item)
  end

  defp proc_info(pid, item) do
    case :lists.member(node(pid), :erlang.nodes()) do
      true ->
        check(:rpc.call(node(pid), :erlang, :process_info, [pid, item]))

      _ ->
        :hidden
    end
  end

  defp check({:badrpc, :nodedown}) do
    :undefined
  end

  defp check({:badrpc, error}) do
    error
  end

  defp check(res) do
    res
  end

  def report_cb(
        %{:label => {:proc_lib, :crash}, :report => crashReport},
        extra
      ) do
    default = %{
      :chars_limit => :unlimited,
      :depth => :unlimited,
      :single_line => false,
      :encoding => :utf8
    }

    do_format(crashReport, :maps.merge(default, extra))
  end

  def format(crashReport) do
    format(crashReport, :latin1)
  end

  def format(crashReport, encoding) do
    format(crashReport, encoding, :unlimited)
  end

  def format(crashReport, encoding, depth) do
    do_format(
      crashReport,
      %{:chars_limit => :unlimited, :depth => depth, :encoding => encoding, :single_line => false}
    )
  end

  defp do_format([ownReport, linkReport], extra) do
    %{:encoding => enc, :single_line => single, :chars_limit => limit0} = extra

    indent =
      cond do
        single ->
          ''

        true ->
          '  '
      end

    nl = nl(single, ' ')
    sep = nl(single, report_separator())

    {partLimit, limit} =
      case limit0 do
        :unlimited ->
          {limit0, limit0}

        _ when is_integer(limit0) ->
          num = length(ownReport)

          hardcodedSize =
            length(indent) + length('crasher') + length(nl) + length(sep) + length(sep) * num

          limit1 = max(limit0 - hardcodedSize, 1)
          eL = div(limit1, 3)
          pL = div(limit1 - eL, num)
          {pL, limit1}
      end

    linkFormat = format_link_reports(linkReport, indent, extra, partLimit)
    linkFormatSize = size(enc, linkFormat)
    ownFormat = format_own_report(ownReport, indent, extra, linkFormatSize, partLimit, limit)

    :io_lib.format(
      '~scrasher:' ++ nl ++ '~ts' ++ sep ++ '~ts',
      [indent, ownFormat, linkFormat]
    )
  end

  defp format_own_report(ownReport, indent, extra, linkFormatSize, partLimit, limit0) do
    myIndent = indent ++ indent

    case separate_error_info(ownReport) do
      {first, {class, reason, stackTrace}, rest} ->
        f = format_report(first, myIndent, extra, partLimit)
        r = format_report(rest, myIndent, extra, partLimit)
        %{:encoding => enc, :single_line => single} = extra
        sep = nl(single, part_separator())

        limit =
          case limit0 do
            :unlimited ->
              limit0

            _ when is_integer(limit0) ->
              sizeOfOther =
                size(enc, f) +
                  size(
                    enc,
                    r
                  ) - length(sep) * (length(f) + length(r)) + linkFormatSize

              max(limit0 - sizeOfOther, 1)
          end

        eI = format_exception(class, reason, stackTrace, extra, limit)
        :lists.join(sep, [f, eI, r])

      :no ->
        limit =
          case limit0 do
            :unlimited ->
              limit0

            _ when is_integer(limit0) ->
              max(limit0 - linkFormatSize, 1)
          end

        format_report(ownReport, myIndent, extra, limit)
    end
  end

  defp separate_error_info(report) do
    try do
      :lists.splitwith(
        fn a ->
          :erlang.element(1, a) !== :error_info
        end,
        report
      )
    catch
      _, _ ->
        :no
    else
      {first, [{:error_info, errorInfo} | rest]} ->
        {first, errorInfo, rest}

      _ ->
        :no
    end
  end

  defp format_link_reports(linkReports, indent, extra, partLimit)
       when is_integer(partLimit) do
    %{:encoding => enc, :depth => depth, :single_line => single} = extra

    pids =
      for {:neighbour, [{:pid, p} | _]} <- linkReports do
        p
      end

    {p, tl} = p(enc, depth)

    width =
      cond do
        single ->
          '0'

        true ->
          ''
      end

    :io_lib.format(indent ++ 'neighbours: ~' ++ width ++ p, [pids | tl], [
      {:chars_limit, partLimit}
    ])
  end

  defp format_link_reports(linkReports, indent, extra, partLimit) do
    %{:single_line => single} = extra
    myIndent = indent ++ indent

    linkFormat =
      :lists.join(
        nl(single, report_separator()),
        format_link_report(linkReports, myIndent, extra, partLimit)
      )

    [indent, 'neighbours:', nl(single, ' '), linkFormat]
  end

  defp format_link_report([link | reps], indent0, extra, partLimit) do
    %{:single_line => single} = extra

    rep =
      case link do
        {:neighbour, rep0} ->
          rep0

        _ ->
          link
      end

    indent =
      cond do
        single ->
          ''

        true ->
          indent0
      end

    linkIndent = ['  ', indent]

    [
      [indent, 'neighbour:', nl(single, ' '), format_report(rep, linkIndent, extra, partLimit)]
      | format_link_report(reps, indent, extra, partLimit)
    ]
  end

  defp format_link_report(rep, indent, extra, partLimit) do
    format_report(rep, indent, extra, partLimit)
  end

  defp format_report(rep, indent, extra, limit) when is_list(rep) do
    %{:single_line => single} = extra

    :lists.join(
      nl(single, part_separator()),
      format_rep(rep, indent, extra, limit)
    )
  end

  defp format_report(rep, indent0, extra, limit) do
    %{:encoding => enc, :depth => depth, :single_line => single} = extra
    {p, tl} = p(enc, depth)

    {indent, width} =
      cond do
        single ->
          {'', '0'}

        true ->
          {indent0, ''}
      end

    opts = chars_limit_opt(limit)
    :io_lib.format('~s~' ++ width ++ p, [[indent, rep] | tl], opts)
  end

  defp format_rep([{:initial_call, initialCall} | rep], indent, extra, limit) do
    [
      format_mfa(indent, initialCall, extra, limit)
      | format_rep(rep, indent, extra, limit)
    ]
  end

  defp format_rep([{tag, data} | rep], indent, extra, limit) do
    [
      format_tag(indent, tag, data, extra, limit)
      | format_rep(rep, indent, extra, limit)
    ]
  end

  defp format_rep(_, _, _Extra, _Limit) do
    []
  end

  defp format_exception(class, reason, stackTrace, extra, limit) do
    %{:encoding => enc, :depth => depth, :single_line => single} = extra

    stackFun = fn m, _F, _A ->
      :erlang.or(m === :erl_eval, m === :proc_lib)
    end

    cond do
      single ->
        {p, tl} = p(enc, depth)
        opts = chars_limit_opt(limit)

        [
          :erlang.atom_to_list(class),
          ': ',
          :io_lib.format('~0' ++ p, [{reason, stackTrace} | tl], opts)
        ]

      true ->
        pF = pp_fun(extra, enc)
        eI = '    '

        lim =
          case limit do
            :unlimited ->
              -1

            _ ->
              limit
          end

        fE =
          :erl_error.format_exception(
            1 + length(eI),
            class,
            reason,
            stackTrace,
            stackFun,
            pF,
            enc,
            lim
          )

        [eI, fE]
    end
  end

  defp format_mfa(indent0, {m, f, args} = startF, extra, limit) do
    %{:encoding => enc, :single_line => single} = extra

    indent =
      cond do
        single ->
          ''

        true ->
          indent0
      end

    try do
      a = length(args)

      [
        indent,
        'initial call: ',
        to_string(m, enc),
        ?:,
        to_string(f, enc),
        ?/,
        :erlang.integer_to_list(a)
      ]
    catch
      :error, _ ->
        format_tag(indent, :initial_call, startF, extra, limit)
    end
  end

  defp to_string(a, :latin1) do
    :io_lib.write_atom_as_latin1(a)
  end

  defp to_string(a, _) do
    :io_lib.write_atom(a)
  end

  defp pp_fun(extra, enc) do
    %{:encoding => ^enc, :depth => depth, :single_line => single} = extra
    {p, tl} = p(enc, depth)

    width =
      cond do
        single ->
          '0'

        true ->
          ''
      end

    fn term, i, limit ->
      s =
        :io_lib.format('~' ++ width ++ '.' ++ :erlang.integer_to_list(i) ++ p, [term | tl], [
          {:chars_limit, limit}
        ])

      {s, sub(limit, s, enc)}
    end
  end

  defp format_tag(indent0, tag, data, extra, limit) do
    %{:encoding => enc, :depth => depth, :single_line => single} = extra
    {p, tl} = p(enc, depth)

    {indent, width} =
      cond do
        single ->
          {'', '0'}

        true ->
          {indent0, ''}
      end

    opts = chars_limit_opt(limit)

    :io_lib.format(
      '~s~' ++ width ++ 'p: ~' ++ width ++ '.18' ++ p,
      [[indent, tag, data] | tl],
      opts
    )
  end

  defp p(encoding, depth) do
    {letter, tl} =
      case depth do
        :unlimited ->
          {'p', []}

        _ ->
          {'P', [depth]}
      end

    p = modifier(encoding) ++ letter
    {p, tl}
  end

  defp report_separator() do
    '; '
  end

  defp part_separator() do
    ', '
  end

  defp chars_limit_opt(charsLimit) do
    for _ <- [:EFE_DUMMY_GEN], is_integer(charsLimit) do
      {:chars_limit, charsLimit}
    end
  end

  defp modifier(:latin1) do
    ''
  end

  defp modifier(_) do
    't'
  end

  defp nl(true, else__) do
    else__
  end

  defp nl(false, _) do
    '\n'
  end

  defp sub(t, _, _Enc) when t < 0 do
    t
  end

  defp sub(t, e, enc) do
    sz = size(enc, e)

    cond do
      t >= sz ->
        t - sz

      true ->
        0
    end
  end

  defp size(:latin1, s) do
    :erlang.iolist_size(s)
  end

  defp size(_, s) do
    :string.length(s)
  end

  def stop(process) do
    stop(process, :normal, :infinity)
  end

  def stop(process, reason, timeout) do
    {pid, mref} =
      :erlang.spawn_monitor(
        do_stop(
          process,
          reason
        )
      )

    receive do
      {:DOWN, ^mref, _, _, ^reason} ->
        :ok

      {:DOWN, ^mref, _, _, {:noproc, {:sys, :terminate, _}}} ->
        exit(:noproc)

      {:DOWN, ^mref, _, _, crashReason} ->
        exit(crashReason)
    after
      timeout ->
        :erlang.exit(pid, :kill)

        receive do
          {:DOWN, ^mref, _, _, _} ->
            exit(:timeout)
        end
    end
  end

  defp do_stop(process, reason) do
    fn ->
      mref = :erlang.monitor(:process, process)
      :ok = :sys.terminate(process, reason, :infinity)

      receive do
        {:DOWN, ^mref, _, _, exitReason} ->
          exit(exitReason)
      end
    end
  end
end
