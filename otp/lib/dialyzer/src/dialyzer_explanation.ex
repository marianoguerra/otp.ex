defmodule :m_dialyzer_explanation do
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

  def expl_loop(parent, cServer, plt) do
    receive do
      {^parent, :warning, _Warning} ->
        send_explanation(parent, :none)
        expl_loop(parent, cServer, plt)

      {^parent, :further, _Explanation} ->
        send(parent, {self(), :further, :none})
        expl_loop(parent, cServer, plt)

      other ->
        :io.format('Unknown message: ~p\n', [other])
        expl_loop(parent, cServer, plt)
    end
  end

  defp send_explanation(parent, expl) do
    send(parent, {self(), :explanation, expl})
    :ok
  end
end
