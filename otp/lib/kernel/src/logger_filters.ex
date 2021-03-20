defmodule :m_logger_filters do
  use Bitwise

  def domain(
        %{meta: meta} = logEvent,
        {action, compare, matchDomain}
      )
      when (action == :log or action == :stop) and
             (compare == :super or compare == :sub or compare == :equal or compare == :not_equal or
                compare == :undefined) and is_list(matchDomain) do
    filter_domain(compare, meta, matchDomain, on_match(action, logEvent))
  end

  def domain(logEvent, extra) do
    :erlang.error(:badarg, [logEvent, extra])
  end

  def level(%{level: l1} = logEvent, {action, op, l2})
      when (action == :log or action == :stop) and
             (op == :neq or op == :eq or op == :lt or op == :gt or op == :lteq or op == :gteq) and
             (l2 === :emergency or l2 === :alert or l2 === :critical or l2 === :error or
                l2 === :warning or l2 === :notice or l2 === :info or l2 === :debug) do
    filter_level(op, l1, l2, on_match(action, logEvent))
  end

  def level(logEvent, extra) do
    :erlang.error(:badarg, [logEvent, extra])
  end

  def progress(logEvent, action)
      when action == :log or action == :stop do
    filter_progress(logEvent, on_match(action, logEvent))
  end

  def progress(logEvent, action) do
    :erlang.error(:badarg, [logEvent, action])
  end

  def remote_gl(logEvent, action)
      when action == :log or action == :stop do
    filter_remote_gl(logEvent, on_match(action, logEvent))
  end

  def remote_gl(logEvent, action) do
    :erlang.error(:badarg, [logEvent, action])
  end

  defp filter_domain(:super, %{domain: domain}, matchDomain, onMatch) do
    is_prefix(domain, matchDomain, onMatch)
  end

  defp filter_domain(:sub, %{domain: domain}, matchDomain, onMatch) do
    is_prefix(matchDomain, domain, onMatch)
  end

  defp filter_domain(:equal, %{domain: domain}, domain, onMatch) do
    onMatch
  end

  defp filter_domain(:not_equal, %{domain: domain}, matchDomain, onMatch)
       when domain !== matchDomain do
    onMatch
  end

  defp filter_domain(compare, meta, _, onMatch) do
    case :maps.is_key(:domain, meta) do
      false
      when compare == :undefined or
             compare == :not_equal ->
        onMatch

      _ ->
        :ignore
    end
  end

  defp is_prefix(d1, d2, onMatch)
       when is_list(d1) and
              is_list(d2) do
    case :lists.prefix(d1, d2) do
      true ->
        onMatch

      false ->
        :ignore
    end
  end

  defp is_prefix(_, _, _) do
    :ignore
  end

  defp filter_level(op, l1, l2, onMatch) do
    case :logger.compare_levels(l1, l2) do
      :eq when op == :eq or op == :lteq or op == :gteq ->
        onMatch

      :lt when op == :lt or op == :lteq or op == :neq ->
        onMatch

      :gt when op == :gt or op == :gteq or op == :neq ->
        onMatch

      _ ->
        :ignore
    end
  end

  defp filter_progress(
         %{msg: {:report, %{label: {_, :progress}}}},
         onMatch
       ) do
    onMatch
  end

  defp filter_progress(_, _) do
    :ignore
  end

  defp filter_remote_gl(%{meta: %{gl: gL}}, onMatch)
       when node(gL) !== node() do
    onMatch
  end

  defp filter_remote_gl(_, _) do
    :ignore
  end

  defp on_match(:log, logEvent) do
    logEvent
  end

  defp on_match(:stop, _) do
    :stop
  end
end
