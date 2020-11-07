defmodule :m_mnesia_text do
  use Bitwise

  def load_textfile(file) do
    ensure_started()

    case parse(file) do
      {:ok, {tabs, data}} ->
        badtabs = make_tabs(:lists.map(&validate_tab/1, tabs))
        load_data(del_data(badtabs, data, []))

      other ->
        other
    end
  end

  def dump_to_textfile(file) do
    dump_to_textfile(
      :mnesia_lib.is_running(),
      :file.open(file, [:write])
    )
  end

  defp dump_to_textfile(:yes, {:ok, f}) do
    tabs =
      :lists.delete(
        :schema,
        :mnesia_lib.local_active_tables()
      )

    defs =
      :lists.map(
        fn t ->
          {t,
           [
             {:record_name, :mnesia_lib.val({t, :record_name})},
             {:attributes, :mnesia_lib.val({t, :attributes})}
           ]}
        end,
        tabs
      )

    :io.format(f, '~p.~n', [{:tables, defs}])

    :lists.foreach(
      fn t ->
        dump_tab(f, t)
      end,
      tabs
    )

    :file.close(f)
  end

  defp dump_to_textfile(_, _) do
    :error
  end

  defp dump_tab(f, t) do
    w = :mnesia_lib.val({t, :wild_pattern})

    {:atomic, all} =
      :mnesia.transaction(fn ->
        :mnesia.match_object(t, w, :read)
      end)

    :lists.foreach(
      fn term ->
        :io.format(f, '~p.~n', [:erlang.setelement(1, term, t)])
      end,
      all
    )
  end

  defp ensure_started() do
    case :mnesia_lib.is_running() do
      :yes ->
        :yes

      :no ->
        case :mnesia_lib.exists(:mnesia_lib.dir('schema.DAT')) do
          true ->
            :mnesia.start()

          false ->
            :mnesia.create_schema([node()])
            :mnesia.start()
        end
    end
  end

  defp del_data(bad, [h | t], ack) do
    case :lists.member(:erlang.element(1, h), bad) do
      true ->
        del_data(bad, t, ack)

      false ->
        del_data(bad, t, [h | ack])
    end
  end

  defp del_data(_Bad, [], ack) do
    :lists.reverse(ack)
  end

  defp validate_tab({tabname, list}) do
    {tabname, list}
  end

  defp validate_tab({tabname, recName, list}) do
    {tabname, recName, list}
  end

  defp validate_tab(_) do
    :erlang.error(:badtab)
  end

  defp make_tabs([{tab, def__} | tail]) do
    try do
      :mnesia.table_info(tab, :where_to_read)
    catch
      :exit, _ ->
        case :mnesia.create_table(tab, def__) do
          {:aborted, reason} ->
            :io.format('** Failed to create table ~tw ~n** Reason = ~tw, Args = ~tp~n', [
              tab,
              reason,
              def__
            ])

            [tab | make_tabs(tail)]

          _ ->
            :io.format('New table ~tw~n', [tab])
            make_tabs(tail)
        end
    else
      node ->
        :io.format('** Table ~tw already exists on ~p, just entering data~n', [tab, node])
        make_tabs(tail)
    end
  end

  defp make_tabs([]) do
    []
  end

  defp load_data(l) do
    :mnesia.transaction(fn ->
      f = fn x ->
        tab = :erlang.element(1, x)
        rN = :mnesia.table_info(tab, :record_name)
        rec = :erlang.setelement(1, x, rN)
        :mnesia.write(tab, rec, :write)
      end

      :lists.foreach(f, l)
    end)
  end

  def parse(file) do
    case file(file) do
      {:ok, terms} ->
        try do
          collect(terms)
        catch
          error ->
            error
        else
          other ->
            {:ok, other}
        end

      other ->
        other
    end
  end

  defp collect([{_, {:tables, tabs}} | l]) do
    {tabs, collect_data(tabs, l)}
  end

  defp collect(_) do
    :io.format('No tables found\n', [])
    :erlang.error(:bad_header)
  end

  defp collect_data(tabs, [{line, term} | tail])
       when is_tuple(term) do
    case :lists.keysearch(:erlang.element(1, term), 1, tabs) do
      {:value, _} ->
        [term | collect_data(tabs, tail)]

      _Other ->
        :io.format('Object:~tp at line ~w unknown\n', [term, line])
        :erlang.error(:undefined_object)
    end
  end

  defp collect_data(_Tabs, []) do
    []
  end

  defp collect_data(_Tabs, [h | _T]) do
    :io.format('Object:~tp unknown\n', [h])
    :erlang.error(:undefined_object)
  end

  defp error(what) do
    throw({:error, what})
  end

  def file(file) do
    case :file.open(file, [:read]) do
      {:ok, stream} ->
        res = read_terms(stream, file, 1, [])
        :file.close(stream)
        res

      _Other ->
        {:error, :open}
    end
  end

  defp read_terms(stream, file, line, l) do
    case read_term_from_stream(stream, file, line) do
      {:ok, term, nextLine} ->
        read_terms(stream, file, nextLine, [term | l])

      :error ->
        {:error, :read}

      :eof ->
        {:ok, :lists.reverse(l)}
    end
  end

  defp read_term_from_stream(stream, file, line) do
    r =
      :io.request(
        stream,
        {:get_until, :latin1, :"", :erl_scan, :tokens, [line]}
      )

    case r do
      {:ok, toks, endLine} ->
        case :erl_parse.parse_term(toks) do
          {:ok, term} ->
            {:ok, {line, term}, endLine}

          {:error, {newLine, mod, what}} ->
            str = mod.format_error(what)
            :io.format('Error in line:~p of:~tp ~ts\n', [newLine, file, str])
            :error
        end

      {:eof, _EndLine} ->
        :eof

      other ->
        :io.format('Error1 **~p~n', [other])
        :error
    end
  end
end
