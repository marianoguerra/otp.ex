defmodule :m_cth_conn_log do
  use Bitwise

  def init(_Id, hookOpts) do
    confOpts = :ct.get_config(:ct_conn_log, [])
    {:ok, merge_log_info(confOpts, hookOpts)}
  end

  defp merge_log_info([{mod, confOpts} | confList], hookList) do
    {opts, hookList1} =
      case :lists.keytake(mod, 1, hookList) do
        false ->
          {confOpts, hookList}

        {:value, {_, hookOpts}, hL1} ->
          {confOpts ++ hookOpts, hL1}
      end

    [
      {mod, get_log_opts(mod, opts)}
      | merge_log_info(confList, hookList1)
    ]
  end

  defp merge_log_info([], hookList) do
    for {mod, opts} <- hookList do
      {mod, get_log_opts(mod, opts)}
    end
  end

  defp get_log_opts(mod, opts) do
    defaultLogType =
      cond do
        mod == :ct_telnet ->
          :raw

        true ->
          :html
      end

    logType = :proplists.get_value(:log_type, opts, defaultLogType)
    hosts = :proplists.get_value(:hosts, opts, [])
    {logType, hosts}
  end

  def pre_init_per_testcase(_Suite, testCase, config, cthState) do
    logs =
      :lists.map(
        fn {connMod, {logType, hosts}} ->
          :ct_util.set_testdata({{:cth_conn_log, connMod}, logType})

          case logType do
            ^logType when logType == :raw or logType == :pretty ->
              dir =
                :test_server.lookup_config(
                  :priv_dir,
                  config
                )

              tCStr = :erlang.atom_to_list(testCase)
              connModStr = :erlang.atom_to_list(connMod)
              defLogName = tCStr ++ '-' ++ connModStr ++ '.txt'
              defLog = :filename.join(dir, defLogName)

              ls =
                for host <- hosts do
                  {host,
                   :filename.join(
                     dir,
                     tCStr ++ '-' ++ :erlang.atom_to_list(host) ++ '-' ++ connModStr ++ '.txt'
                   )}
                end ++ [{:default, defLog}]

              str =
                '<table borders=1><b>' ++
                  connModStr ++
                  ' logs:</b>\n' ++
                  for {s, l} <- ls do
                    :io_lib.format(
                      '<tr><td>~tp</td><td><a href="~ts">~ts</a></td></tr>',
                      [s, :ct_logs.uri(l), :filename.basename(l)]
                    )
                  end ++ '</table>'

              :ct.log(str, [], [:no_css])
              {connMod, {logType, ls}}

            _ ->
              {connMod, {logType, []}}
          end
        end,
        cthState
      )

    gL = :erlang.group_leader()

    update = fn
      init when init == :undefined or init == [] ->
        :error_logger.add_report_handler(
          :ct_conn_log_h,
          {gL, logs}
        )

        [testCase]

      prevUsers ->
        :error_logger.info_report(:update, {gL, logs})

        receive do
          {:updated, ^gL} ->
            [testCase | prevUsers]
        after
          5000 ->
            {:error, :no_response}
        end
    end

    :ct_util.update_testdata(:cth_conn_log, update, [:create])
    {config, cthState}
  end

  def post_end_per_testcase(_Suite, testCase, _Config, return, cthState) do
    update = fn prevUsers ->
      case :lists.delete(testCase, prevUsers) do
        [] ->
          :"$delete"

        prevUsers1 ->
          prevUsers1
      end
    end

    case :ct_util.update_testdata(
           :cth_conn_log,
           update
         ) do
      :deleted ->
        _ =
          for {connMod, _} <- cthState do
            :ct_util.delete_testdata({:cth_conn_log, connMod})
          end

        :error_logger.delete_report_handler(:ct_conn_log_h)

      {:error, :no_response} ->
        exit({:cth_conn_log, :no_response_from_logger})

      _PrevUsers ->
        :ok
    end

    {return, cthState}
  end
end
