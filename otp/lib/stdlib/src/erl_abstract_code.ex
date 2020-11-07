defmodule :m_erl_abstract_code do
  use Bitwise

  def debug_info(_Format, _Module, {:none, _CompilerOpts}, _Opts) do
    {:error, :missing}
  end

  def debug_info(:erlang_v1, _Module, {abstrCode, _CompilerOpts}, _Opts) do
    {:ok, abstrCode}
  end

  def debug_info(:core_v1, _Module, {abstrCode, compilerOpts}, opts) do
    coreOpts = add_core_returns(delete_reports(compilerOpts ++ opts))

    try do
      :compile.noenv_forms(abstrCode, coreOpts)
    catch
      :error, _ ->
        {:error, :failed_conversion}
    else
      {:ok, _, core, _} ->
        {:ok, core}

      _What ->
        {:error, :failed_conversion}
    end
  end

  def debug_info(_, _, _, _) do
    {:error, :unknown_format}
  end

  defp delete_reports(opts) do
    for opt <- opts, not is_report_option(opt) do
      opt
    end
  end

  defp is_report_option(:report) do
    true
  end

  defp is_report_option(:report_errors) do
    true
  end

  defp is_report_option(:report_warnings) do
    true
  end

  defp is_report_option(_) do
    false
  end

  defp add_core_returns(opts) do
    [:to_core, :return_errors, :return_warnings] ++ opts
  end
end
