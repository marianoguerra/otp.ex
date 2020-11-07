defmodule :m_asn1_db do
  use Bitwise
  require Record

  Record.defrecord(:r_state, :state,
    parent: :undefined,
    monitor: :undefined,
    includes: :undefined,
    table: :undefined
  )

  def dbstart(includes0) do
    includes =
      case includes0 do
        [] ->
          ['.']

        [_ | _] ->
          includes0
      end

    parent = self()
    :undefined = :erlang.get(:asn1_db)

    :erlang.put(
      :asn1_db,
      spawn_link(fn ->
        init(parent, includes)
      end)
    )

    :ok
  end

  def dbload(module, erule, maps, mtime) do
    req({:load, module, {erule, maps}, mtime})
  end

  def dbload(module) do
    req({:load, module, :any, {{0, 0, 0}, {0, 0, 0}}})
  end

  def dbnew(module, erule, maps) do
    req({:new, module, {erule, maps}})
  end

  def dbsave(outFile, module) do
    cast({:save, outFile, module})
  end

  def dbput(module, k, v) do
    cast({:set, module, k, v})
  end

  def dbput(module, kvs) do
    cast({:set, module, kvs})
  end

  def dbget(module, k) do
    req({:get, module, k})
  end

  def dbstop() do
    resp = req(:stop)
    :erlang.erase(:asn1_db)
    resp
  end

  defp req(request) do
    dbPid = :erlang.get(:asn1_db)
    ref = :erlang.monitor(:process, dbPid)
    send(:erlang.get(:asn1_db), {{ref, self()}, request})

    receive do
      {{^ref, :asn1_db}, reply} ->
        :erlang.demonitor(ref, [:flush])
        reply

      {:DOWN, ^ref, _, _, info} ->
        exit({:db_error, info})
    end
  end

  defp cast(request) do
    send(:erlang.get(:asn1_db), request)
    :ok
  end

  defp reply({ref, from}, response) do
    send(from, {{ref, :asn1_db}, response})
    :ok
  end

  defp init(parent, includes) do
    mRef = :erlang.monitor(:process, parent)

    loop(
      r_state(parent: parent, monitor: mRef, includes: includes, table: :ets.new(:asn1_db, []))
    )
  end

  defp loop(r_state(parent: parent, monitor: mRef, table: table, includes: includes) = state) do
    receive do
      {:set, mod, k2, v} ->
        [{_, modtab}] = :ets.lookup(table, mod)
        :ets.insert(modtab, {k2, v})
        loop(state)

      {:set, mod, kvs} ->
        [{_, modtab}] = :ets.lookup(table, mod)
        :ets.insert(modtab, kvs)
        loop(state)

      {from, {:get, mod, k2}} ->
        case get_table(table, mod, includes) do
          {:ok, tab} ->
            reply(from, lookup(tab, k2))

          :error ->
            reply(from, :undefined)
        end

        loop(state)

      {:save, outFile, mod} ->
        [{_, mtab}] = :ets.lookup(table, mod)
        tempFile = outFile ++ '.#temp'
        :ok = :ets.tab2file(mtab, tempFile)
        :ok = :file.rename(tempFile, outFile)
        loop(state)

      {from, {:new, mod, eruleMaps}} ->
        [] = :ets.lookup(table, mod)

        modTableId =
          :ets.new(
            :erlang.list_to_atom(:lists.concat(['asn1_', mod])),
            []
          )

        :ets.insert(table, {mod, modTableId})

        :ets.insert(
          modTableId,
          {:__version_and_erule__, info(eruleMaps)}
        )

        reply(from, :ok)
        loop(state)

      {from, {:load, mod, eruleMaps, mtime}} ->
        case :ets.member(table, mod) do
          true ->
            reply(from, :ok)

          false ->
            case load_table(mod, eruleMaps, mtime, includes) do
              {:ok, modTableId} ->
                :ets.insert(table, {mod, modTableId})
                reply(from, :ok)

              :error ->
                reply(from, :error)
            end
        end

        loop(state)

      {from, :stop} ->
        reply(from, :stopped)

      {:DOWN, ^mRef, :process, ^parent, reason} ->
        exit(reason)
    end
  end

  defp get_table(table, mod, includes) do
    case :ets.lookup(table, mod) do
      [{^mod, tab}] ->
        {:ok, tab}

      [] ->
        load_table(mod, :any, {{0, 0, 0}, {0, 0, 0}}, includes)
    end
  end

  defp lookup(tab, k) do
    case :ets.lookup(tab, k) do
      [] ->
        :undefined

      [{^k, v}] ->
        v
    end
  end

  defp info(eruleMaps) do
    {:asn1ct.vsn(), eruleMaps}
  end

  defp load_table(mod, eruleMaps, mtime, includes) do
    base = :lists.concat([mod, '.asn1db'])

    case path_find(includes, mtime, base) do
      :error ->
        :error

      {:ok, modTab} when eruleMaps === :any ->
        {:ok, modTab}

      {:ok, modTab} ->
        vsn = :asn1ct.vsn()

        case :ets.lookup(modTab, :__version_and_erule__) do
          [{_, {^vsn, ^eruleMaps}}] ->
            {:ok, modTab}

          _ ->
            :ets.delete(modTab)
            :error
        end
    end
  end

  defp path_find([h | t], mtime, base) do
    file = :filename.join(h, base)

    case :filelib.last_modified(file) do
      0 ->
        path_find(t, mtime, base)

      dbMtime when dbMtime >= mtime ->
        case :ets.file2tab(file) do
          {:ok, _} = ret ->
            ret

          _ ->
            path_find(t, mtime, base)
        end

      _ ->
        path_find(t, mtime, base)
    end
  end

  defp path_find([], _, _) do
    :error
  end
end
