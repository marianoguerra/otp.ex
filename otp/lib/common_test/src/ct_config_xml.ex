defmodule :m_ct_config_xml do
  use Bitwise

  def read_config(configFile) do
    case (try do
            do_read_xml_config(configFile)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, config} ->
        {:ok, config}

      error = {:error, _} ->
        error
    end
  end

  def check_parameter(file) do
    case :filelib.is_file(file) do
      true ->
        {:ok, {:file, file}}

      false ->
        {:error, {:nofile, file}}
    end
  end

  defp do_read_xml_config(configFile) do
    case (try do
            :xmerl_sax_parser.file(
              configFile,
              [{:event_fun, &event/3}, {:event_state, []}]
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, entityList, _} ->
        {:ok, :lists.reverse(transform_entity_list(entityList))}

      oops ->
        {:error, {:parsing_failed, oops}}
    end
  end

  defp event(event, _LineNo, state) do
    tag(event, state)
  end

  defp tag(:startDocument, state) do
    state
  end

  defp tag(
         {:startElement, _Uri, 'config', _QName, _Attributes},
         []
       ) do
    [{'config', []}]
  end

  defp tag(
         {:startElement, _Uri, name, _QName, _Attributes},
         tags
       ) do
    [{name, []} | tags]
  end

  defp tag(
         {:characters, string},
         [{tag, _Value} | tags]
       ) do
    [{tag, string} | tags]
  end

  defp tag(
         {:endElement, _Uri, _Name, _QName},
         [entity, {prevEntityTag, prevEntityValue} | tags]
       ) do
    newHead = {prevEntityTag, [entity | prevEntityValue]}
    [newHead | tags]
  end

  defp tag(
         {:endElement, _Uri, 'config', _QName},
         [{'config', config}]
       ) do
    config
  end

  defp tag(:endDocument, {_Tags, result}) do
    result
  end

  defp tag(_El, state) do
    state
  end

  defp transform_entity_list(entityList) do
    :lists.map(&transform_entity/1, entityList)
  end

  defp transform_entity({tag, [value | rest]}) when is_tuple(value) do
    {:erlang.list_to_atom(tag), transform_entity_list(:lists.reverse([value | rest]))}
  end

  defp transform_entity({tag, string}) do
    case list_to_term(string) do
      {:ok, value} ->
        {:erlang.list_to_atom(tag), value}

      error ->
        throw(error)
    end
  end

  defp list_to_term(string) do
    {:ok, t, _} = :erl_scan.string(string ++ '.')

    case (try do
            :erl_parse.parse_term(t)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, term} ->
        {:ok, term}

      error ->
        {:error, {error, string}}
    end
  end
end
