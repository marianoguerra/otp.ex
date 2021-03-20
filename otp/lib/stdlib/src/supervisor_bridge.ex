defmodule :m_supervisor_bridge do
  use Bitwise
  @behaviour :gen_server
  require Record

  Record.defrecord(:r_state, :state,
    mod: :undefined,
    pid: :undefined,
    child_state: :undefined,
    name: :undefined
  )

  def start_link(mod, startArgs) do
    :gen_server.start_link(:supervisor_bridge, [mod, startArgs, :self], [])
  end

  def start_link(name, mod, startArgs) do
    :gen_server.start_link(name, :supervisor_bridge, [mod, startArgs, name], [])
  end

  def init([mod, startArgs, name0]) do
    :erlang.process_flag(:trap_exit, true)
    name = supname(name0, mod)

    case mod.init(startArgs) do
      {:ok, pid, childState} when is_pid(pid) ->
        :erlang.link(pid)
        report_progress(pid, mod, startArgs, name)
        {:ok, r_state(mod: mod, pid: pid, child_state: childState, name: name)}

      :ignore ->
        :ignore

      {:error, reason} ->
        {:stop, reason}
    end
  end

  defp supname(:self, mod) do
    {self(), mod}
  end

  defp supname(n, _) do
    n
  end

  def handle_call(:which_children, _From, state) do
    {:reply, [], state}
  end

  def handle_call(_Req, _From, state) do
    {:reply, {:error, :badcall}, state}
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  def handle_info({:EXIT, pid, reason}, state)
      when r_state(state, :pid) === pid do
    case reason do
      :normal ->
        :ok

      :shutdown ->
        :ok

      {:shutdown, _Term} ->
        :ok

      _ ->
        report_error(:child_terminated, reason, state)
    end

    {:stop, reason, r_state(state, pid: :undefined)}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  def terminate(_Reason, r_state(pid: :undefined)) do
    :ok
  end

  def terminate(reason, state) do
    terminate_pid(reason, state)
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp terminate_pid(reason, r_state(mod: mod, child_state: childState)) do
    mod.terminate(reason, childState)
  end

  defp report_progress(pid, mod, startArgs, supName) do
    case :logger.allow(:info, :supervisor_bridge) do
      true ->
        :erlang.apply(:logger, :macro_log, [
          %{
            mfa: {:supervisor_bridge, :report_progress, 4},
            line: 136,
            file: 'otp/lib/stdlib/src/supervisor_bridge.erl'
          },
          :info,
          %{
            label: {:supervisor, :progress},
            report: [
              {:supervisor, supName},
              {:started, [{:pid, pid}, {:mfa, {mod, :init, [startArgs]}}]}
            ]
          },
          %{
            domain: [:otp, :sasl],
            report_cb: &:supervisor_bridge.format_log/2,
            logger_formatter: %{title: 'PROGRESS REPORT'},
            error_logger: %{
              tag: :info_report,
              type: :progress,
              report_cb: &:supervisor_bridge.format_log/1
            }
          }
        ])

      false ->
        :ok
    end
  end

  defp report_error(error, reason, r_state(name: name, pid: pid, mod: mod)) do
    case :logger.allow(:error, :supervisor_bridge) do
      true ->
        :erlang.apply(:logger, :macro_log, [
          %{
            mfa: {:supervisor_bridge, :report_error, 3},
            line: 149,
            file: 'otp/lib/stdlib/src/supervisor_bridge.erl'
          },
          :error,
          %{
            label: {:supervisor, :error},
            report: [
              {:supervisor, name},
              {:errorContext, error},
              {:reason, reason},
              {:offender, [{:pid, pid}, {:mod, mod}]}
            ]
          },
          %{
            domain: [:otp, :sasl],
            report_cb: &:supervisor_bridge.format_log/2,
            logger_formatter: %{title: 'SUPERVISOR REPORT'},
            error_logger: %{
              tag: :error_report,
              type: :supervisor_report,
              report_cb: &:supervisor_bridge.format_log/1
            }
          }
        ])

      false ->
        :ok
    end
  end

  def format_log(logReport) do
    depth = :error_logger.get_format_depth()
    formatOpts = %{chars_limit: :unlimited, depth: depth, single_line: false, encoding: :utf8}

    format_log_multi(
      limit_report(logReport, depth),
      formatOpts
    )
  end

  defp limit_report(logReport, :unlimited) do
    logReport
  end

  defp limit_report(
         %{
           label: {:supervisor, :progress},
           report: [{:supervisor, _} = supervisor, {:started, child}]
         } = logReport,
         depth
       ) do
    Map.put(logReport, :report, [supervisor, {:started, limit_child_report(child, depth)}])
  end

  defp limit_report(
         %{
           label: {:supervisor, :error},
           report: [
             {:supervisor, _} = supervisor,
             {:errorContext, ctxt},
             {:reason, reason},
             {:offender, child}
           ]
         } = logReport,
         depth
       ) do
    Map.put(logReport, :report, [
      supervisor,
      {:errorContext, :io_lib.limit_term(ctxt, depth)},
      {:reason, :io_lib.limit_term(reason, depth)},
      {:offender,
       :io_lib.limit_term(
         child,
         depth
       )}
    ])
  end

  defp limit_child_report(childReport, depth) do
    {:mfa, {m, f, [as]}} = :lists.keyfind(:mfa, 1, childReport)
    newMFAs = {m, f, [:io_lib.limit_term(as, depth)]}
    :lists.keyreplace(:mfa, 1, childReport, {:mfa, newMFAs})
  end

  def format_log(report, formatOpts0) do
    default = %{chars_limit: :unlimited, depth: :unlimited, single_line: false, encoding: :utf8}
    formatOpts = :maps.merge(default, formatOpts0)

    ioOpts =
      case formatOpts do
        %{chars_limit: :unlimited} ->
          []

        %{chars_limit: limit} ->
          [{:chars_limit, limit}]
      end

    {format, args} = format_log_single(report, formatOpts)
    :io_lib.format(format, args, ioOpts)
  end

  defp format_log_single(
         %{label: {:supervisor, :progress}, report: [{:supervisor, supName}, {:started, child}]},
         %{single_line: true, depth: depth} = formatOpts
       ) do
    p = p(formatOpts)
    {childFormat, childArgs} = format_child_log_progress_single(child, 'Started:', formatOpts)
    format = 'Supervisor: ' ++ p ++ '.'

    args =
      case depth do
        :unlimited ->
          [supName]

        _ ->
          [supName, depth]
      end

    {format ++ childFormat, args ++ childArgs}
  end

  defp format_log_single(
         %{
           label: {:supervisor, _Error},
           report: [
             {:supervisor, supName},
             {:errorContext, ctxt},
             {:reason, reason},
             {:offender, child}
           ]
         },
         %{single_line: true, depth: depth} = formatOpts
       ) do
    p = p(formatOpts)
    format = :lists.append(['Supervisor: ', p, '. Context: ', p, '. Reason: ', p, '.'])
    {childFormat, childArgs} = format_child_log_error_single(child, 'Offender:')

    args =
      case depth do
        :unlimited ->
          [supName, ctxt, reason]

        _ ->
          [supName, depth, ctxt, depth, reason, depth]
      end

    {format ++ childFormat, args ++ childArgs}
  end

  defp format_log_single(report, formatOpts) do
    format_log_multi(report, formatOpts)
  end

  defp format_log_multi(
         %{label: {:supervisor, :progress}, report: [{:supervisor, supName}, {:started, child}]},
         %{depth: depth} = formatOpts
       ) do
    p = p(formatOpts)
    format = :lists.append(['    supervisor: ', p, '~n', '    started: ', p, '~n'])

    args =
      case depth do
        :unlimited ->
          [supName, child]

        _ ->
          [supName, depth, child, depth]
      end

    {format, args}
  end

  defp format_log_multi(
         %{
           label: {:supervisor, _Error},
           report: [
             {:supervisor, supName},
             {:errorContext, ctxt},
             {:reason, reason},
             {:offender, child}
           ]
         },
         %{depth: depth} = formatOpts
       ) do
    p = p(formatOpts)

    format =
      :lists.append([
        '    supervisor: ',
        p,
        '~n',
        '    errorContext: ',
        p,
        '~n',
        '    reason: ',
        p,
        '~n',
        '    offender: ',
        p,
        '~n'
      ])

    args =
      case depth do
        :unlimited ->
          [supName, ctxt, reason, child]

        _ ->
          [supName, depth, ctxt, depth, reason, depth, child, depth]
      end

    {format, args}
  end

  defp format_child_log_progress_single(child, tag, formatOpts) do
    {:pid, pid} = :lists.keyfind(:pid, 1, child)
    {:mfa, mFAs} = :lists.keyfind(:mfa, 1, child)

    args =
      case :maps.get(:depth, formatOpts) do
        :unlimited ->
          [mFAs]

        depth ->
          [mFAs, depth]
      end

    {' ~s pid=~w,mfa=' ++ p(formatOpts) ++ '.', [tag, pid] ++ args}
  end

  defp format_child_log_error_single(child, tag) do
    {:pid, pid} = :lists.keyfind(:pid, 1, child)
    {:mod, mod} = :lists.keyfind(:mod, 1, child)
    {' ~s pid=~w,mod=~w.', [tag, pid, mod]}
  end

  defp p(%{single_line: single, depth: depth, encoding: enc}) do
    '~' ++ single(single) ++ mod(enc) ++ p(depth)
  end

  defp p(:unlimited) do
    'p'
  end

  defp p(_Depth) do
    'P'
  end

  defp single(true) do
    '0'
  end

  defp single(false) do
    ''
  end

  defp mod(:latin1) do
    ''
  end

  defp mod(_) do
    't'
  end
end
