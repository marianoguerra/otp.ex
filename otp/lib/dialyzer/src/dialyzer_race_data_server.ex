defmodule :m_dialyzer_race_data_server do
  use Bitwise
  require Record

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

  Record.defrecord(:r_state, :state,
    race_code: :dict.new(),
    public_tables: [],
    named_tables: [],
    beh_api_calls: []
  )

  def new() do
    spawn_link(fn ->
      loop(r_state())
    end)
  end

  def duplicate(server) do
    call(:dup, server)
  end

  def stop(server) do
    cast(:stop, server)
  end

  def call(query, server) do
    ref = make_ref()
    send(server, {:call, self(), ref, query})

    receive do
      {^ref, reply} ->
        reply
    end
  end

  def cast(message, server) do
    send(server, {:cast, message})
    :ok
  end

  defp loop(state) do
    receive do
      {:call, from, ref, query} ->
        reply = handle_call(query, state)
        send(from, {ref, reply})
        loop(state)

      {:cast, :stop} ->
        :ok

      {:cast, message} ->
        newState = handle_cast(message, state)
        loop(newState)
    end
  end

  defp handle_cast(:race_code_new, state) do
    r_state(state, race_code: :dict.new())
  end

  defp handle_cast({tag, data}, state) do
    case tag do
      :renew_race_info ->
        renew_race_info_handler(data, state)

      :renew_race_code ->
        renew_race_code_handler(data, state)

      :renew_race_public_tables ->
        renew_race_public_tables_handler(data, state)

      :put_race_code ->
        r_state(state, race_code: data)

      :put_public_tables ->
        r_state(state, public_tables: data)

      :put_named_tables ->
        r_state(state, named_tables: data)

      :put_behaviour_api_calls ->
        r_state(state, beh_api_calls: data)
    end
  end

  defp handle_call(
         query,
         r_state(
           race_code: raceCode,
           public_tables: publicTables,
           named_tables: namedTables,
           beh_api_calls: behApiCalls
         ) = state
       ) do
    case query do
      :dup ->
        spawn_link(fn ->
          loop(state)
        end)

      :get_race_code ->
        raceCode

      :get_public_tables ->
        publicTables

      :get_named_tables ->
        namedTables

      :get_behaviour_api_calls ->
        behApiCalls
    end
  end

  defp renew_race_info_handler(
         {raceCode, publicTables, namedTables},
         r_state() = state
       ) do
    r_state(state, race_code: raceCode, public_tables: publicTables, named_tables: namedTables)
  end

  defp renew_race_code_handler(
         {fun, funArgs, code},
         r_state(race_code: raceCode) = state
       ) do
    r_state(state, race_code: :dict.store(fun, [funArgs, code], raceCode))
  end

  defp renew_race_public_tables_handler(varLabel, r_state(public_tables: pT) = state) do
    r_state(state,
      public_tables:
        :ordsets.add_element(
          varLabel,
          pT
        )
    )
  end
end
