defmodule :m_kernel do
  use Bitwise
  @behaviour :supervisor
  def start(_, []) do
    :ok = :logger.internal_init_logger()

    case :supervisor.start_link({:local, :kernel_sup}, :kernel, []) do
      {:ok, pid} ->
        :ok = :erl_signal_handler.start()
        :ok = :logger.add_handlers(:kernel)
        {:ok, pid, []}

      error ->
        error
    end
  end

  def stop(_State) do
    :ok
  end

  def config_change(changed, new, removed) do
    do_distribution_change(changed, new, removed)
    do_global_groups_change(changed, new, removed)
    :ok
  end

  def init([]) do
    supFlags = %{strategy: :one_for_all, intensity: 0, period: 1}

    config = %{
      id: :kernel_config,
      start: {:kernel_config, :start_link, []},
      restart: :permanent,
      shutdown: 2000,
      type: :worker,
      modules: [:kernel_config]
    }

    refC = %{
      id: :kernel_refc,
      start: {:kernel_refc, :start_link, []},
      restart: :permanent,
      shutdown: 2000,
      type: :worker,
      modules: [:kernel_refc]
    }

    code = %{
      id: :code_server,
      start: {:code, :start_link, []},
      restart: :permanent,
      shutdown: 2000,
      type: :worker,
      modules: [:code]
    }

    file = %{
      id: :file_server_2,
      start: {:file_server, :start_link, []},
      restart: :permanent,
      shutdown: 2000,
      type: :worker,
      modules: [:file, :file_server, :file_io_server, :prim_file]
    }

    stdError = %{
      id: :standard_error,
      start: {:standard_error, :start_link, []},
      restart: :temporary,
      shutdown: 2000,
      type: :supervisor,
      modules: [:standard_error]
    }

    user = %{
      id: :user,
      start: {:user_sup, :start, []},
      restart: :temporary,
      shutdown: 2000,
      type: :supervisor,
      modules: [:user_sup]
    }

    safeSup = %{
      id: :kernel_safe_sup,
      start: {:supervisor, :start_link, [{:local, :kernel_safe_sup}, :kernel, :safe]},
      restart: :permanent,
      shutdown: :infinity,
      type: :supervisor,
      modules: [:kernel]
    }

    loggerSup = %{
      id: :logger_sup,
      start: {:logger_sup, :start_link, []},
      restart: :permanent,
      shutdown: :infinity,
      type: :supervisor,
      modules: [:logger_sup]
    }

    case :init.get_argument(:mode) do
      {:ok, [['minimal'] | _]} ->
        {:ok, {supFlags, [code, file, stdError, user, loggerSup, config, refC, safeSup]}}

      _ ->
        distChildren =
          case :application.get_env(
                 :kernel,
                 :start_distribution
               ) do
            {:ok, false} ->
              []

            _ ->
              start_distribution()
          end

        inetDb = %{
          id: :inet_db,
          start: {:inet_db, :start_link, []},
          restart: :permanent,
          shutdown: 2000,
          type: :worker,
          modules: [:inet_db]
        }

        sigSrv = %{
          id: :erl_signal_server,
          start: {:gen_event, :start_link, [{:local, :erl_signal_server}]},
          restart: :permanent,
          shutdown: 2000,
          type: :worker,
          modules: :dynamic
        }

        timer = start_timer()
        compileServer = start_compile_server()

        {:ok,
         {supFlags,
          [code, inetDb | distChildren] ++
            [file, sigSrv, stdError, user, config, refC, safeSup, loggerSup] ++
            timer ++ compileServer}}
    end
  end

  def init(:safe) do
    supFlags = %{strategy: :one_for_one, intensity: 4, period: 3600}
    boot = start_boot_server()
    diskLog = start_disk_log()
    pg = start_pg2() ++ start_pg()
    :init.run_on_load_handlers()
    {:ok, {supFlags, boot ++ diskLog ++ pg}}
  end

  defp start_distribution() do
    rpc = %{
      id: :rex,
      start: {:rpc, :start_link, []},
      restart: :permanent,
      shutdown: 2000,
      type: :worker,
      modules: [:rpc]
    }

    global = %{
      id: :global_name_server,
      start: {:global, :start_link, []},
      restart: :permanent,
      shutdown: 2000,
      type: :worker,
      modules: [:global]
    }

    distAC = start_dist_ac()

    netSup = %{
      id: :net_sup,
      start: {:erl_distribution, :start_link, []},
      restart: :permanent,
      shutdown: :infinity,
      type: :supervisor,
      modules: [:erl_distribution]
    }

    glGroup = %{
      id: :global_group,
      start: {:global_group, :start_link, []},
      restart: :permanent,
      shutdown: 2000,
      type: :worker,
      modules: [:global_group]
    }

    [rpc, global | distAC] ++ [netSup, glGroup]
  end

  defp start_dist_ac() do
    spec = [
      %{
        id: :dist_ac,
        start: {:dist_ac, :start_link, []},
        restart: :permanent,
        shutdown: 2000,
        type: :worker,
        modules: [:dist_ac]
      }
    ]

    case :application.get_env(:kernel, :start_dist_ac) do
      {:ok, true} ->
        spec

      {:ok, false} ->
        []

      :undefined ->
        case :application.get_env(:kernel, :distributed) do
          {:ok, _} ->
            spec

          _ ->
            []
        end
    end
  end

  defp start_boot_server() do
    case :application.get_env(
           :kernel,
           :start_boot_server
         ) do
      {:ok, true} ->
        args = get_boot_args()

        [
          %{
            id: :boot_server,
            start: {:erl_boot_server, :start_link, [args]},
            restart: :permanent,
            shutdown: 1000,
            type: :worker,
            modules: [:erl_boot_server]
          }
        ]

      _ ->
        []
    end
  end

  defp get_boot_args() do
    case :application.get_env(
           :kernel,
           :boot_server_slaves
         ) do
      {:ok, slaves} ->
        slaves

      _ ->
        []
    end
  end

  defp start_disk_log() do
    case :application.get_env(:kernel, :start_disk_log) do
      {:ok, true} ->
        [
          %{
            id: :disk_log_server,
            start: {:disk_log_server, :start_link, []},
            restart: :permanent,
            shutdown: 2000,
            type: :worker,
            modules: [:disk_log_server]
          },
          %{
            id: :disk_log_sup,
            start: {:disk_log_sup, :start_link, []},
            restart: :permanent,
            shutdown: 1000,
            type: :supervisor,
            modules: [:disk_log_sup]
          }
        ]

      _ ->
        []
    end
  end

  defp start_pg() do
    case :application.get_env(:kernel, :start_pg) do
      {:ok, true} ->
        [
          %{
            id: :pg,
            start: {:pg, :start_link, []},
            restart: :permanent,
            shutdown: 1000,
            type: :worker,
            modules: [:pg]
          }
        ]

      _ ->
        []
    end
  end

  defp start_pg2() do
    case :application.get_env(:kernel, :start_pg2) do
      {:ok, true} ->
        [
          %{
            id: :pg2,
            start: {:pg2, :start_link, []},
            restart: :permanent,
            shutdown: 1000,
            type: :worker,
            modules: [:pg2]
          }
        ]

      _ ->
        []
    end
  end

  defp start_timer() do
    case :application.get_env(:kernel, :start_timer) do
      {:ok, true} ->
        [
          %{
            id: :timer_server,
            start: {:timer, :start_link, []},
            restart: :permanent,
            shutdown: 1000,
            type: :worker,
            modules: [:timer]
          }
        ]

      _ ->
        []
    end
  end

  defp start_compile_server() do
    case :application.get_env(
           :kernel,
           :start_compile_server
         ) do
      {:ok, true} ->
        [
          %{
            id: :erl_compile_server,
            start: {:erl_compile_server, :start_link, []},
            restart: :permanent,
            shutdown: 2000,
            type: :worker,
            modules: [:erl_compile_server]
          }
        ]

      _ ->
        []
    end
  end

  defp do_distribution_change(changed, new, removed) do
    case is_dist_changed(changed, new, removed) do
      {false, false, false} ->
        :ok

      {c, false, false} ->
        :gen_server.call(:dist_ac, {:distribution_changed, c}, :infinity)

      {false, _, false} ->
        :error_logger.error_report(
          'Distribution not changed: Not allowed to add the \'distributed\' parameter.'
        )

        {:error, {:distribution_not_changed, 'Not allowed to add the \'distributed\' parameter'}}

      {false, false, _} ->
        :error_logger.error_report(
          'Distribution not changed: Not allowed to remove the distribution parameter.'
        )

        {:error,
         {:distribution_not_changed, 'Not allowed to remove the \'distributed\' parameter'}}
    end
  end

  defp is_dist_changed(changed, new, removed) do
    c =
      case :lists.keyfind(:distributed, 1, changed) do
        false ->
          false

        {:distributed, newDistC} ->
          newDistC
      end

    n =
      case :lists.keyfind(:distributed, 1, new) do
        false ->
          false

        {:distributed, newDistN} ->
          newDistN
      end

    r = :lists.member(:distributed, removed)
    {c, n, r}
  end

  defp do_global_groups_change(changed, new, removed) do
    case is_gg_changed(changed, new, removed) do
      {false, false, false} ->
        :ok

      {c, false, false} ->
        :global_group.global_groups_changed(c)

      {false, n, false} ->
        :global_group.global_groups_added(n)

      {false, false, r} ->
        :global_group.global_groups_removed(r)
    end
  end

  defp is_gg_changed(changed, new, removed) do
    c =
      case :lists.keyfind(:global_groups, 1, changed) do
        false ->
          false

        {:global_groups, newDistC} ->
          newDistC
      end

    n =
      case :lists.keyfind(:global_groups, 1, new) do
        false ->
          false

        {:global_groups, newDistN} ->
          newDistN
      end

    r = :lists.member(:global_groups, removed)
    {c, n, r}
  end
end
