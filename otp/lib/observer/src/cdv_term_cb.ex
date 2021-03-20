defmodule :m_cdv_term_cb do
  use Bitwise

  def get_details({type, {t, key}}, _) do
    [{^key, term}] = :ets.lookup(t, key)
    {:ok, {'Expanded Term', {type, [term, t]}, []}}
  end

  def detail_pages() do
    [{'Term', &init_term_page/2}]
  end

  defp init_term_page(parentWin, {type, [term, tab]}) do
    :observer_lib.report_progress({:ok, 'Expanding term'})
    :observer_lib.report_progress({:ok, :start_pulse})
    expanded = expand(term, true)
    binSaved = expand(term, tab)
    :observer_lib.report_progress({:ok, :stop_pulse})
    cs = :observer_lib.colors(parentWin)

    :cdv_multi_wx.start_link(
      parentWin,
      [
        {'Format ~p', :cdv_html_wx, {type, format_term_fun('~p', binSaved, tab, cs)}},
        {'Format ~tp', :cdv_html_wx, {type, format_term_fun('~tp', binSaved, tab, cs)}},
        {'Format ~w', :cdv_html_wx, {type, format_term_fun('~w', binSaved, tab, cs)}},
        {'Format ~tw', :cdv_html_wx, {type, format_term_fun('~tw', binSaved, tab, cs)}},
        {'Format ~s', :cdv_html_wx,
         {type,
          format_term_fun(
            '~s',
            expanded,
            tab,
            cs
          )}},
        {'Format ~ts', :cdv_html_wx,
         {type,
          format_term_fun(
            '~ts',
            expanded,
            tab,
            cs
          )}}
      ]
    )
  end

  defp format_term_fun(format, term, tab, cs) do
    fn ->
      :observer_lib.report_progress({:ok, 'Formatting term'})
      :observer_lib.report_progress({:ok, :start_pulse})

      try do
        :io_lib.format(format, [term])
      catch
        :error, :badarg ->
          warning = 'This term cannot be formatted with ' ++ format
          :observer_html_lib.warning(warning, cs)
      else
        str ->
          {:expand, plain_html(str, cs), tab}
      after
        :observer_lib.report_progress({:ok, :stop_pulse})
      end
    end
  end

  defp plain_html(text, cs) do
    :observer_html_lib.plain_page(text, cs)
  end

  defp expand([:"#CDVBin", offset, size, pos], true) do
    {:ok, bin} = :crashdump_viewer.expand_binary({offset, size, pos})
    bin
  end

  defp expand(bin, tab)
       when is_binary(bin) and
              not is_boolean(tab) do
    :observer_lib.make_obsbin(bin, tab)
  end

  defp expand([h | t], expand) do
    case expand(t, expand) do
      eT when is_list(eT) ->
        [expand(h, expand) | eT]

      eT ->
        [expand(h, expand), eT]
    end
  end

  defp expand(tuple, expand) when is_tuple(tuple) do
    :erlang.list_to_tuple(
      expand(
        :erlang.tuple_to_list(tuple),
        expand
      )
    )
  end

  defp expand(term, _) do
    term
  end
end
