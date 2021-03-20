defmodule :m_erl_distribution do
  use Bitwise
  @behaviour :supervisor
  def start_link() do
    do_start_link([{:sname, :shortnames}, {:name, :longnames}])
  end

  def start(args) do
    c = %{
      id: :net_sup_dynamic,
      start: {:erl_distribution, :start_link, [args, false, :net_sup_dynamic]},
      restart: :permanent,
      shutdown: 1000,
      type: :supervisor,
      modules: [:erl_distribution]
    }

    :supervisor.start_child(:kernel_sup, c)
  end

  def stop() do
    case :supervisor.terminate_child(
           :kernel_sup,
           :net_sup_dynamic
         ) do
      :ok ->
        :supervisor.delete_child(:kernel_sup, :net_sup_dynamic)

      error ->
        case :erlang.whereis(:net_sup) do
          pid when is_pid(pid) ->
            {:error, :not_allowed}

          _ ->
            error
        end
    end
  end

  def start_link(args, cleanHalt, netSup) do
    :supervisor.start_link({:local, :net_sup}, :erl_distribution, [args, cleanHalt, netSup])
  end

  def init(netArgs) do
    epmd =
      case :init.get_argument(:no_epmd) do
        {:ok, [[]]} ->
          []

        _ ->
          epmdMod = :net_kernel.epmd_module()

          [
            %{
              id: epmdMod,
              start: {epmdMod, :start_link, []},
              restart: :permanent,
              shutdown: 2000,
              type: :worker,
              modules: [epmdMod]
            }
          ]
      end

    auth = %{
      id: :auth,
      start: {:auth, :start_link, []},
      restart: :permanent,
      shutdown: 2000,
      type: :worker,
      modules: [:auth]
    }

    kernel = %{
      id: :net_kernel,
      start: {:net_kernel, :start_link, netArgs},
      restart: :permanent,
      shutdown: 2000,
      type: :worker,
      modules: [:net_kernel]
    }

    earlySpecs = :net_kernel.protocol_childspecs()
    supFlags = %{strategy: :one_for_all, intensity: 0, period: 1}
    {:ok, {supFlags, earlySpecs ++ epmd ++ [auth, kernel]}}
  end

  defp do_start_link([{arg, flag} | t]) do
    case :init.get_argument(arg) do
      {:ok, [[name]]} ->
        start_link(
          [
            :erlang.list_to_atom(name),
            flag
            | ticktime()
          ],
          true,
          :net_sup
        )

      {:ok, [[name] | _Rest]} ->
        case :logger.allow(:warning, :erl_distribution) do
          true ->
            apply(:logger, :macro_log, [
              %{
                mfa: {:erl_distribution, :do_start_link, 1},
                line: 109,
                file: 'otp/lib/kernel/src/erl_distribution.erl'
              },
              :warning,
              'Multiple -~p given to erl, using the first, ~p',
              [arg, name]
            ])

          false ->
            :ok
        end

        start_link(
          [
            :erlang.list_to_atom(name),
            flag
            | ticktime()
          ],
          true,
          :net_sup
        )

      _ ->
        do_start_link(t)
    end
  end

  defp do_start_link([]) do
    :ignore
  end

  defp ticktime() do
    case (try do
            :application.get_env(:net_ticktime)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, value} when is_integer(value) and value > 0 ->
        [value * 250]

      _ ->
        []
    end
  end
end
