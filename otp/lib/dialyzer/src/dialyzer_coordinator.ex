defmodule :m_dialyzer_coordinator do
  use Bitwise
  require Record

  Record.defrecord(:r_state, :state,
    mode: :undefined,
    active: 0,
    result: :undefined,
    next_label: 0,
    jobs: :undefined,
    job_fun: :undefined,
    init_data: :undefined,
    regulator: :undefined,
    scc_to_pid: :undefined
  )

  Record.defrecord(:r_analysis, :analysis,
    analysis_pid: :undefined,
    type: :succ_typings,
    defines: [],
    doc_plt: :undefined,
    files: [],
    include_dirs: [],
    start_from: :byte_code,
    plt: :undefined,
    use_contracts: true,
    race_detection: false,
    behaviours_chk: false,
    timing: false,
    timing_server: :none,
    callgraph_file: '',
    solvers: :undefined
  )

  Record.defrecord(:r_options, :options,
    files: [],
    files_rec: [],
    analysis_type: :succ_typings,
    timing: false,
    defines: [],
    from: :byte_code,
    get_warnings: :maybe,
    init_plts: [],
    include_dirs: [],
    output_plt: :none,
    legal_warnings: :ordsets.new(),
    report_mode: :normal,
    erlang_mode: false,
    use_contracts: true,
    output_file: :none,
    output_format: :formatted,
    filename_opt: :basename,
    indent_opt: true,
    callgraph_file: '',
    check_plt: true,
    solvers: [],
    native: :maybe,
    native_cache: true
  )

  Record.defrecord(:r_contract, :contract, contracts: [], args: [], forms: [])

  def parallel_job(mode, jobs, initData, timing) do
    state = spawn_jobs(mode, jobs, initData, timing)
    collect_result(state)
  end

  defp spawn_jobs(mode, jobs, initData, timing) do
    collector = self()
    regulator = spawn_regulator()
    typesigOrDataflow = mode === :typesig or mode === :dataflow

    sCCtoPID =
      case typesigOrDataflow do
        true ->
          :ets.new(:scc_to_pid, [{:read_concurrency, true}])

        false ->
          :unused
      end

    coordinator = {collector, regulator, sCCtoPID}

    jobFun = fn job ->
      pid = :dialyzer_worker.launch(mode, job, initData, coordinator)

      case typesigOrDataflow do
        true ->
          true = :ets.insert(sCCtoPID, {job, pid})

        false ->
          true
      end
    end

    jobCount = length(jobs)

    numberOfInitJobs =
      min(
        jobCount,
        20 * :dialyzer_utils.parallelism()
      )

    {initJobs, restJobs} =
      :lists.split(
        numberOfInitJobs,
        jobs
      )

    :lists.foreach(jobFun, initJobs)

    unit =
      case mode do
        :typesig ->
          'SCCs'

        _ ->
          'modules'
      end

    :dialyzer_timing.send_size_info(timing, jobCount, unit)

    initResult =
      case mode do
        :compile ->
          :dialyzer_analysis_callgraph.compile_init_result()

        _ ->
          []
      end

    r_state(
      mode: mode,
      active: jobCount,
      result: initResult,
      next_label: 0,
      job_fun: jobFun,
      jobs: restJobs,
      init_data: initData,
      regulator: regulator,
      scc_to_pid: sCCtoPID
    )
  end

  defp collect_result(
         r_state(
           mode: mode,
           active: active,
           result: result,
           next_label: nextLabel,
           init_data: initData,
           jobs: jobsLeft,
           job_fun: jobFun,
           regulator: regulator,
           scc_to_pid: sCCtoPID
         ) = state
       ) do
    receive do
      {:next_label_request, estimation, pid} ->
        send(pid, {:next_label_reply, nextLabel})
        collect_result(r_state(state, next_label: nextLabel + estimation))

      {:done, job, data} ->
        newResult = update_result(mode, initData, job, data, result)
        typesigOrDataflow = mode === :typesig or mode === :dataflow

        case active do
          1 ->
            kill_regulator(regulator)

            case mode do
              :compile ->
                {newResult, nextLabel}

              _ when typesigOrDataflow ->
                :ets.delete(sCCtoPID)
                newResult

              :warnings ->
                newResult
            end

          n ->
            case typesigOrDataflow do
              true ->
                true = :ets.delete(sCCtoPID, job)

              false ->
                true
            end

            newJobsLeft =
              case jobsLeft do
                [] ->
                  []

                [newJob | jobsLeft1] ->
                  jobFun.(newJob)
                  jobsLeft1
              end

            newState = r_state(state, result: newResult, jobs: newJobsLeft, active: n - 1)
            collect_result(newState)
        end
    end
  end

  defp update_result(mode, initData, job, data, result) do
    case mode do
      :compile ->
        :dialyzer_analysis_callgraph.add_to_result(job, data, result, initData)

      x when x === :typesig or x === :dataflow ->
        :dialyzer_succ_typings.lookup_names(
          data,
          initData
        ) ++ result

      :warnings ->
        data ++ result
    end
  end

  def sccs_to_pids(sCCs, {_Collector, _Regulator, sCCtoPID}) do
    fold = fn sCC, pids ->
      try do
        :ets.lookup_element(sCCtoPID, sCC, 2)
      catch
        _, _ ->
          pids
      else
        pid when is_pid(pid) ->
          [pid | pids]
      end
    end

    :lists.foldl(fold, [], sCCs)
  end

  def job_done(job, result, {collector, regulator, _SCCtoPID}) do
    send(regulator, :done)
    send(collector, {:done, job, result})
    :ok
  end

  def get_next_label(
        estimatedSize,
        {collector, _Regulator, _SCCtoPID}
      ) do
    send(collector, {:next_label_request, estimatedSize, self()})

    receive do
      {:next_label_reply, nextLabel} ->
        nextLabel
    end
  end

  defp wait_activation() do
    receive do
      :activate ->
        :ok
    end
  end

  defp activate_pid(pid) do
    send(pid, :activate)
  end

  def request_activation({_Collector, regulator, _SCCtoPID}) do
    send(regulator, {:req, self()})
    wait_activation()
  end

  defp spawn_regulator() do
    initTickets = :dialyzer_utils.parallelism()

    spawn_link(fn ->
      regulator_loop(initTickets, :queue.new())
    end)
  end

  defp regulator_loop(tickets, queue) do
    receive do
      {:req, pid} ->
        case tickets do
          0 ->
            regulator_loop(0, :queue.in(pid, queue))

          n ->
            activate_pid(pid)
            regulator_loop(n - 1, queue)
        end

      :done ->
        {waiting, newQueue} = :queue.out(queue)

        newTickets =
          case waiting do
            :empty ->
              tickets + 1

            {:value, pid} ->
              activate_pid(pid)
              tickets
          end

        regulator_loop(newTickets, newQueue)

      :stop ->
        :ok
    end
  end

  defp kill_regulator(regulator) do
    send(regulator, :stop)
  end
end
