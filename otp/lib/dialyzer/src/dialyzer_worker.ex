defmodule :m_dialyzer_worker do
  use Bitwise
  require Record

  Record.defrecord(:r_state, :state,
    mode: :undefined,
    job: :undefined,
    coordinator: :undefined,
    init_data: :undefined,
    depends_on: []
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

  def launch(mode, job, initData, coordinator) do
    state = r_state(mode: mode, job: job, init_data: initData, coordinator: coordinator)

    spawn_link(fn ->
      init(state)
    end)
  end

  defp init(r_state(job: sCC, mode: mode, init_data: initData, coordinator: coordinator) = state)
       when mode === :typesig or mode === :dataflow do
    dependsOnSCCs =
      :dialyzer_succ_typings.find_depends_on(
        sCC,
        initData
      )

    :ok

    pids =
      :dialyzer_coordinator.sccs_to_pids(
        dependsOnSCCs,
        coordinator
      )

    :ok

    dependsOn =
      for pid <- pids do
        {pid, :erlang.monitor(:process, pid)}
      end

    loop(:updating, r_state(state, depends_on: dependsOn))
  end

  defp init(r_state(mode: mode) = state)
       when mode === :compile or
              mode === :warnings do
    loop(:running, state)
  end

  defp loop(:updating, r_state(mode: mode) = state)
       when mode === :typesig or mode === :dataflow do
    :ok

    nextStatus =
      case waits_more_success_typings(state) do
        true ->
          :waiting

        false ->
          :running
      end

    loop(nextStatus, state)
  end

  defp loop(:waiting, r_state(mode: mode) = state)
       when mode === :typesig or mode === :dataflow do
    :ok
    newState = wait_for_success_typings(state)
    loop(:updating, newState)
  end

  defp loop(:running, r_state(mode: :compile) = state) do
    request_activation(state)
    :ok

    result =
      case start_compilation(state) do
        {:ok, estimatedSize, data} ->
          label = ask_coordinator_for_label(estimatedSize, state)
          continue_compilation(label, data)

        {:error, _Reason} = error ->
          error
      end

    report_to_coordinator(result, state)
  end

  defp loop(:running, r_state(mode: :warnings) = state) do
    request_activation(state)
    :ok
    result = collect_warnings(state)
    report_to_coordinator(result, state)
  end

  defp loop(:running, r_state(mode: mode) = state)
       when mode === :typesig or mode === :dataflow do
    request_activation(state)
    :ok
    notFixpoint = do_work(state)
    report_to_coordinator(notFixpoint, state)
  end

  defp waits_more_success_typings(r_state(depends_on: depends)) do
    depends !== []
  end

  defp wait_for_success_typings(r_state(depends_on: dependsOn) = state) do
    receive do
      {:DOWN, ref, :process, pid, _Info} ->
        :ok
        r_state(state, depends_on: dependsOn -- [{pid, ref}])
    after
      5000 ->
        :ok
        state
    end
  end

  defp request_activation(r_state(coordinator: coordinator)) do
    :dialyzer_coordinator.request_activation(coordinator)
  end

  defp do_work(r_state(mode: mode, job: job, init_data: initData)) do
    case mode do
      :typesig ->
        :dialyzer_succ_typings.find_succ_types_for_scc(
          job,
          initData
        )

      :dataflow ->
        :dialyzer_succ_typings.refine_one_module(job, initData)
    end
  end

  defp report_to_coordinator(
         result,
         r_state(job: job, coordinator: coordinator)
       ) do
    :ok
    :dialyzer_coordinator.job_done(job, result, coordinator)
  end

  defp start_compilation(r_state(job: job, init_data: initData)) do
    :dialyzer_analysis_callgraph.start_compilation(
      job,
      initData
    )
  end

  defp ask_coordinator_for_label(estimatedSize, r_state(coordinator: coordinator)) do
    :dialyzer_coordinator.get_next_label(
      estimatedSize,
      coordinator
    )
  end

  defp continue_compilation(label, data) do
    :dialyzer_analysis_callgraph.continue_compilation(
      label,
      data
    )
  end

  defp collect_warnings(r_state(job: job, init_data: initData)) do
    :dialyzer_succ_typings.collect_warnings(job, initData)
  end
end
