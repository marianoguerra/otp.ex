defmodule :m_cth_log_redirect do
  use Bitwise
  @behaviour :gen_server
  require Record

  Record.defrecord(:r_eh_state, :eh_state,
    log_func: :undefined,
    curr_suite: :undefined,
    curr_group: :undefined,
    curr_func: :undefined,
    parallel_tcs: false,
    handle_remote_events: false
  )

  def id(_Opts) do
    :cth_log_redirect
  end

  def init(:cth_log_redirect, _Opts) do
    :ct_util.mark_process()
    :ok = start_log_handler()
    :tc_log_async
  end

  def pre_init_per_suite(suite, config, state) do
    set_curr_func({suite, :init_per_suite}, config)
    {config, state}
  end

  def pre_end_per_suite(suite, config, state) do
    set_curr_func({suite, :end_per_suite}, config)
    {config, state}
  end

  def post_end_per_suite(_Suite, config, return, state) do
    set_curr_func(:undefined, config)
    {return, state}
  end

  def pre_init_per_group(_Suite, group, config, state) do
    set_curr_func({:group, group, :init_per_group}, config)
    {config, state}
  end

  def post_init_per_group(_Suite, group, config, result, :tc_log_async)
      when is_list(config) do
    case :lists.member(
           :parallel,
           :proplists.get_value(:tc_group_properties, config, [])
         ) do
      true ->
        {result, {set_log_func(:tc_log), group}}

      false ->
        {result, :tc_log_async}
    end
  end

  def post_init_per_group(_Suite, _Group, _Config, result, state) do
    {result, state}
  end

  def pre_init_per_testcase(_Suite, tC, config, state) do
    set_curr_func(tC, config)
    {config, state}
  end

  def post_init_per_testcase(_Suite, _TC, _Config, return, state) do
    {return, state}
  end

  def pre_end_per_testcase(_Suite, _TC, config, state) do
    {config, state}
  end

  def post_end_per_testcase(_Suite, _TC, _Config, result, state) do
    :gen_server.call(:cth_log_redirect, :flush, 300_000)
    {result, state}
  end

  def pre_end_per_group(_Suite, group, config, {:tc_log, group}) do
    set_curr_func({:group, group, :end_per_group}, config)
    {config, set_log_func(:tc_log_async)}
  end

  def pre_end_per_group(_Suite, group, config, state) do
    set_curr_func({:group, group, :end_per_group}, config)
    {config, state}
  end

  def post_end_per_group(_Suite, _Group, config, return, state) do
    set_curr_func({:group, :undefined}, config)
    {return, state}
  end

  defp start_log_handler() do
    case :erlang.whereis(:cth_log_redirect) do
      :undefined ->
        childSpec = %{
          :id => :cth_log_redirect,
          :start =>
            {:gen_server, :start_link, [{:local, :cth_log_redirect}, :cth_log_redirect, [], []]},
          :restart => :transient,
          :shutdown => 2000,
          :type => :worker,
          :modules => [:cth_log_redirect]
        }

        {:ok, _} =
          :supervisor.start_child(
            :logger_sup,
            childSpec
          )

        :ok

      _Pid ->
        :ok
    end

    {defaultFormatter, defaultLevel} =
      case :logger.get_handler_config(:default) do
        {:ok, default} ->
          {:maps.get(:formatter, default), :maps.get(:level, default)}

        _Else ->
          {{:logger_formatter, %{:legacy_header => true, :single_line => false}}, :info}
      end

    :ok =
      :logger.add_handler(
        :cth_log_redirect,
        :cth_log_redirect,
        %{:level => defaultLevel, :formatter => defaultFormatter}
      )
  end

  def init([]) do
    {:ok, r_eh_state(log_func: :tc_log_async)}
  end

  def log(
        %{:msg => {:report, msg}, :meta => %{:domain => [:otp, :sasl]}} = log,
        config
      ) do
    case :erlang.whereis(:sasl_sup) do
      :undefined ->
        :ok

      _Else ->
        level =
          case :application.get_env(
                 :sasl,
                 :errlog_type
               ) do
            {:ok, :error} ->
              :error

            {:ok, _} ->
              :info

            :undefined ->
              :info
          end

        case level do
          :error ->
            case msg do
              %{:label => {_, :progress}} ->
                :ok

              _ ->
                do_log(add_log_category(log, :sasl), config)
            end

          _ ->
            do_log(add_log_category(log, :sasl), config)
        end
    end
  end

  def log(
        %{:meta => %{:domain => [:otp]}} = log,
        config
      ) do
    do_log(add_log_category(log, :error_logger), config)
  end

  def log(%{:meta => %{:domain => _}}, _) do
    :ok
  end

  def log(log, config) do
    do_log(add_log_category(log, :error_logger), config)
  end

  defp add_log_category(%{:meta => meta} = log, category) do
    %{log | :meta => %{meta | :cth_log_redirect => %{:category => category}}}
  end

  defp do_log(log, config) do
    :gen_server.call(:cth_log_redirect, {:log, log, config})
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  def handle_call(
        {:log, %{:meta => %{:gl => gL}}, _},
        _From,
        r_eh_state(handle_remote_events: false) = state
      )
      when node(gL) != node() do
    {:reply, :ok, state}
  end

  def handle_call(
        {:log, %{:meta => %{:cth_log_redirect => %{:category => category}}} = log,
         %{:formatter => {formatter, fConfig}}},
        _From,
        r_eh_state(log_func: logFunc) = state
      ) do
    header = format_header(state)
    string = formatter.format(log, fConfig)

    case logFunc do
      :tc_log ->
        :ct_logs.tc_log(category, 50, header, '~ts', [string], [])

      :tc_log_async ->
        :ct_logs.tc_log_async(:sasl, 50, header, '~ts', [string])
    end

    {:reply, :ok, state}
  end

  def handle_call(:flush, _From, state) do
    {:reply, :ok, state}
  end

  def handle_call({:set_curr_func, {:group, group, conf}, config}, _From, state)
      when is_list(config) do
    parallel =
      case :proplists.get_value(
             :tc_group_properties,
             config
           ) do
        :undefined ->
          false

        props ->
          :lists.member(:parallel, props)
      end

    {:reply, :ok, r_eh_state(state, curr_group: group, curr_func: conf, parallel_tcs: parallel)}
  end

  def handle_call({:set_curr_func, {:group, group, conf}, _SkipOrFail}, _From, state) do
    {:reply, :ok, r_eh_state(state, curr_group: group, curr_func: conf, parallel_tcs: false)}
  end

  def handle_call({:set_curr_func, {:group, :undefined}, _Config}, _From, state) do
    {:reply, :ok,
     r_eh_state(state, curr_group: :undefined, curr_func: :undefined, parallel_tcs: false)}
  end

  def handle_call({:set_curr_func, {suite, conf}, _Config}, _From, state) do
    {:reply, :ok, r_eh_state(state, curr_suite: suite, curr_func: conf, parallel_tcs: false)}
  end

  def handle_call({:set_curr_func, :undefined, _Config}, _From, state) do
    {:reply, :ok,
     r_eh_state(state, curr_suite: :undefined, curr_func: :undefined, parallel_tcs: false)}
  end

  def handle_call({:set_curr_func, tC, _Config}, _From, state) do
    {:reply, :ok, r_eh_state(state, curr_func: tC)}
  end

  def handle_call({:set_logfunc, newLogFunc}, _From, state) do
    {:reply, newLogFunc, r_eh_state(state, log_func: newLogFunc)}
  end

  def handle_call({:handle_remote_events, bool}, _From, state) do
    {:reply, :ok, r_eh_state(state, handle_remote_events: bool)}
  end

  def terminate(_) do
    _ = :logger.remove_handler(:cth_log_redirect)

    _ =
      :supervisor.terminate_child(
        :logger_sup,
        :cth_log_redirect
      )

    _ =
      :supervisor.delete_child(
        :logger_sup,
        :cth_log_redirect
      )

    :ok
  end

  def terminate(_Arg, _State) do
    :ok
  end

  defp set_curr_func(currFunc, config) do
    :gen_server.call(
      :cth_log_redirect,
      {:set_curr_func, currFunc, config}
    )
  end

  defp set_log_func(func) do
    :gen_server.call(
      :cth_log_redirect,
      {:set_logfunc, func}
    )
  end

  def handle_remote_events(bool) do
    :gen_server.call(
      :cth_log_redirect,
      {:handle_remote_events, bool}
    )
  end

  defp format_header(
         r_eh_state(curr_suite: :undefined, curr_group: :undefined, curr_func: :undefined)
       ) do
    :lists.flatten(:io_lib.format('System report', []))
  end

  defp format_header(r_eh_state(curr_suite: suite, curr_group: :undefined, curr_func: :undefined)) do
    :lists.flatten(:io_lib.format('System report during ~w', [suite]))
  end

  defp format_header(r_eh_state(curr_suite: suite, curr_group: :undefined, curr_func: tcOrConf)) do
    :lists.flatten(:io_lib.format('System report during ~w:~tw/1', [suite, tcOrConf]))
  end

  defp format_header(r_eh_state(curr_suite: suite, curr_group: group, curr_func: conf))
       when conf == :init_per_group or
              conf == :end_per_group do
    :lists.flatten(:io_lib.format('System report during ~w:~w/2 for ~tw', [suite, conf, group]))
  end

  defp format_header(r_eh_state(curr_suite: suite, curr_group: group, parallel_tcs: true)) do
    :lists.flatten(:io_lib.format('System report during ~tw in ~w', [group, suite]))
  end

  defp format_header(r_eh_state(curr_suite: suite, curr_group: group, curr_func: tC)) do
    :lists.flatten(:io_lib.format('System report during ~w:~tw/1 in ~tw', [suite, tC, group]))
  end
end
