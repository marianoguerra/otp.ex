defmodule :m_digraph do
  use Bitwise
  require Record

  Record.defrecord(:r_digraph, :digraph,
    vtab: :notable,
    etab: :notable,
    ntab: :notable,
    cyclic: true
  )

  def new() do
    new([])
  end

  def new(type) do
    case check_type(type, :protected, []) do
      {access, ts} ->
        v = :ets.new(:vertices, [:set, access])
        e = :ets.new(:edges, [:set, access])
        n = :ets.new(:neighbours, [:bag, access])
        :ets.insert(n, [{:"$vid", 0}, {:"$eid", 0}])
        set_type(ts, r_digraph(vtab: v, etab: e, ntab: n))

      :error ->
        :erlang.error(:badarg)
    end
  end

  defp check_type([:acyclic | ts], a, l) do
    check_type(ts, a, [{:cyclic, false} | l])
  end

  defp check_type([:cyclic | ts], a, l) do
    check_type(ts, a, [{:cyclic, true} | l])
  end

  defp check_type([:protected | ts], _, l) do
    check_type(ts, :protected, l)
  end

  defp check_type([:private | ts], _, l) do
    check_type(ts, :private, l)
  end

  defp check_type([], a, l) do
    {a, l}
  end

  defp check_type(_, _, _) do
    :error
  end

  defp set_type([{:cyclic, v} | ks], g) do
    set_type(ks, r_digraph(g, cyclic: v))
  end

  defp set_type([], g) do
    g
  end

  def delete(g) do
    :ets.delete(r_digraph(g, :vtab))
    :ets.delete(r_digraph(g, :etab))
    :ets.delete(r_digraph(g, :ntab))
  end

  def info(g) do
    vT = r_digraph(g, :vtab)
    eT = r_digraph(g, :etab)
    nT = r_digraph(g, :ntab)

    cyclicity =
      case r_digraph(g, :cyclic) do
        true ->
          :cyclic

        false ->
          :acyclic
      end

    protection = :ets.info(vT, :protection)

    memory =
      :ets.info(vT, :memory) +
        :ets.info(
          eT,
          :memory
        ) +
        :ets.info(
          nT,
          :memory
        )

    [{:cyclicity, cyclicity}, {:memory, memory}, {:protection, protection}]
  end

  def add_vertex(g) do
    do_add_vertex({new_vertex_id(g), []}, g)
  end

  def add_vertex(g, v) do
    do_add_vertex({v, []}, g)
  end

  def add_vertex(g, v, d) do
    do_add_vertex({v, d}, g)
  end

  def del_vertex(g, v) do
    do_del_vertex(v, g)
  end

  def del_vertices(g, vs) do
    do_del_vertices(vs, g)
  end

  def vertex(g, v) do
    case :ets.lookup(r_digraph(g, :vtab), v) do
      [] ->
        false

      [vertex] ->
        vertex
    end
  end

  def no_vertices(g) do
    :ets.info(r_digraph(g, :vtab), :size)
  end

  def vertices(g) do
    :ets.select(r_digraph(g, :vtab), [{{:"$1", :_}, [], [:"$1"]}])
  end

  def source_vertices(g) do
    collect_vertices(g, :in)
  end

  def sink_vertices(g) do
    collect_vertices(g, :out)
  end

  def in_degree(g, v) do
    length(:ets.lookup(r_digraph(g, :ntab), {:in, v}))
  end

  def in_neighbours(g, v) do
    eT = r_digraph(g, :etab)
    nT = r_digraph(g, :ntab)
    collect_elems(:ets.lookup(nT, {:in, v}), eT, 2)
  end

  def in_edges(g, v) do
    for {{:in, _}, e} <-
          :ets.lookup(
            r_digraph(g, :ntab),
            {:in, v}
          ) do
      e
    end
  end

  def out_degree(g, v) do
    length(:ets.lookup(r_digraph(g, :ntab), {:out, v}))
  end

  def out_neighbours(g, v) do
    eT = r_digraph(g, :etab)
    nT = r_digraph(g, :ntab)
    collect_elems(:ets.lookup(nT, {:out, v}), eT, 3)
  end

  def out_edges(g, v) do
    for {{:out, _}, e} <-
          :ets.lookup(
            r_digraph(g, :ntab),
            {:out, v}
          ) do
      e
    end
  end

  def add_edge(g, v1, v2) do
    do_add_edge({new_edge_id(g), v1, v2, []}, g)
  end

  def add_edge(g, v1, v2, d) do
    do_add_edge({new_edge_id(g), v1, v2, d}, g)
  end

  def add_edge(g, e, v1, v2, d) do
    do_add_edge({e, v1, v2, d}, g)
  end

  def del_edge(g, e) do
    do_del_edges([e], g)
  end

  def del_edges(g, es) do
    do_del_edges(es, g)
  end

  def no_edges(g) do
    :ets.info(r_digraph(g, :etab), :size)
  end

  def edges(g) do
    :ets.select(r_digraph(g, :etab), [{{:"$1", :_, :_, :_}, [], [:"$1"]}])
  end

  def edges(g, v) do
    :ets.select(
      r_digraph(g, :ntab),
      [{{{:out, v}, :"$1"}, [], [:"$1"]}, {{{:in, v}, :"$1"}, [], [:"$1"]}]
    )
  end

  def edge(g, e) do
    case :ets.lookup(r_digraph(g, :etab), e) do
      [] ->
        false

      [edge] ->
        edge
    end
  end

  defp new_edge_id(g) do
    nT = r_digraph(g, :ntab)
    [{:"$eid", k}] = :ets.lookup(nT, :"$eid")
    true = :ets.delete(nT, :"$eid")
    true = :ets.insert(nT, {:"$eid", k + 1})
    [:"$e" | k]
  end

  defp new_vertex_id(g) do
    nT = r_digraph(g, :ntab)
    [{:"$vid", k}] = :ets.lookup(nT, :"$vid")
    true = :ets.delete(nT, :"$vid")
    true = :ets.insert(nT, {:"$vid", k + 1})
    [:"$v" | k]
  end

  defp collect_elems(keys, table, index) do
    collect_elems(keys, table, index, [])
  end

  defp collect_elems([{_, key} | keys], table, index, acc) do
    collect_elems(keys, table, index, [:ets.lookup_element(table, key, index) | acc])
  end

  defp collect_elems([], _, _, acc) do
    acc
  end

  defp do_add_vertex({v, _Label} = vL, g) do
    :ets.insert(r_digraph(g, :vtab), vL)
    v
  end

  defp collect_vertices(g, type) do
    vs = vertices(g)

    :lists.foldl(
      fn v, a ->
        case :ets.member(r_digraph(g, :ntab), {type, v}) do
          true ->
            a

          false ->
            [v | a]
        end
      end,
      [],
      vs
    )
  end

  defp do_del_vertices([v | vs], g) do
    do_del_vertex(v, g)
    do_del_vertices(vs, g)
  end

  defp do_del_vertices([], r_digraph()) do
    true
  end

  defp do_del_vertex(v, g) do
    do_del_nedges(:ets.lookup(r_digraph(g, :ntab), {:in, v}), g)
    do_del_nedges(:ets.lookup(r_digraph(g, :ntab), {:out, v}), g)
    :ets.delete(r_digraph(g, :vtab), v)
  end

  defp do_del_nedges([{_, e} | ns], g) do
    case :ets.lookup(r_digraph(g, :etab), e) do
      [{^e, v1, v2, _}] ->
        do_del_edge(e, v1, v2, g)
        do_del_nedges(ns, g)

      [] ->
        do_del_nedges(ns, g)
    end
  end

  defp do_del_nedges([], r_digraph()) do
    true
  end

  defp do_del_edges([e | es], g) do
    case :ets.lookup(r_digraph(g, :etab), e) do
      [{^e, v1, v2, _}] ->
        do_del_edge(e, v1, v2, g)
        do_del_edges(es, g)

      [] ->
        do_del_edges(es, g)
    end
  end

  defp do_del_edges([], r_digraph()) do
    true
  end

  defp do_del_edge(e, v1, v2, g) do
    :ets.select_delete(
      r_digraph(g, :ntab),
      [{{{:in, v2}, e}, [], [true]}, {{{:out, v1}, e}, [], [true]}]
    )

    :ets.delete(r_digraph(g, :etab), e)
  end

  defp rm_edges([v1, v2 | vs], g) do
    rm_edge(v1, v2, g)
    rm_edges([v2 | vs], g)
  end

  defp rm_edges(_, _) do
    true
  end

  defp rm_edge(v1, v2, g) do
    es = out_edges(g, v1)
    rm_edge_0(es, v1, v2, g)
  end

  defp rm_edge_0([e | es], v1, v2, g) do
    case :ets.lookup(r_digraph(g, :etab), e) do
      [{^e, ^v1, ^v2, _}] ->
        do_del_edge(e, v1, v2, g)
        rm_edge_0(es, v1, v2, g)

      _ ->
        rm_edge_0(es, v1, v2, g)
    end
  end

  defp rm_edge_0([], _, _, r_digraph()) do
    :ok
  end

  defp do_add_edge({e, v1, v2, label}, g) do
    case :ets.member(r_digraph(g, :vtab), v1) do
      false ->
        {:error, {:bad_vertex, v1}}

      true ->
        case :ets.member(r_digraph(g, :vtab), v2) do
          false ->
            {:error, {:bad_vertex, v2}}

          true ->
            case other_edge_exists(g, e, v1, v2) do
              true ->
                {:error, {:bad_edge, [v1, v2]}}

              false when r_digraph(g, :cyclic) === false ->
                acyclic_add_edge(e, v1, v2, label, g)

              false ->
                do_insert_edge(e, v1, v2, label, g)
            end
        end
    end
  end

  defp other_edge_exists(r_digraph(etab: eT), e, v1, v2) do
    case :ets.lookup(eT, e) do
      [{^e, vert1, vert2, _}]
      when vert1 !== v1 or
             vert2 !== v2 ->
        true

      _ ->
        false
    end
  end

  defp do_insert_edge(e, v1, v2, label, r_digraph(ntab: nT, etab: eT)) do
    :ets.insert(nT, [{{:out, v1}, e}, {{:in, v2}, e}])
    :ets.insert(eT, {e, v1, v2, label})
    e
  end

  defp acyclic_add_edge(_E, v1, v2, _L, _G) when v1 === v2 do
    {:error, {:bad_edge, [v1, v2]}}
  end

  defp acyclic_add_edge(e, v1, v2, label, g) do
    case get_path(g, v2, v1) do
      false ->
        do_insert_edge(e, v1, v2, label, g)

      path ->
        {:error, {:bad_edge, path}}
    end
  end

  def del_path(g, v1, v2) do
    case get_path(g, v1, v2) do
      false ->
        true

      path ->
        rm_edges(path, g)
        del_path(g, v1, v2)
    end
  end

  def get_cycle(g, v) do
    case one_path(out_neighbours(g, v), v, [], [v], [v], 2, g, 1) do
      false ->
        case :lists.member(v, out_neighbours(g, v)) do
          true ->
            [v]

          false ->
            false
        end

      vs ->
        vs
    end
  end

  def get_path(g, v1, v2) do
    one_path(out_neighbours(g, v1), v2, [], [v1], [v1], 1, g, 1)
  end

  defp prune_short_path(counter, min) when counter < min do
    :short
  end

  defp prune_short_path(_Counter, _Min) do
    :ok
  end

  defp one_path([w | ws], w, cont, xs, ps, prune, g, counter) do
    case prune_short_path(counter, prune) do
      :short ->
        one_path(ws, w, cont, xs, ps, prune, g, counter)

      :ok ->
        :lists.reverse([w | ps])
    end
  end

  defp one_path([v | vs], w, cont, xs, ps, prune, g, counter) do
    case :lists.member(v, xs) do
      true ->
        one_path(vs, w, cont, xs, ps, prune, g, counter)

      false ->
        one_path(
          out_neighbours(g, v),
          w,
          [{vs, ps} | cont],
          [v | xs],
          [v | ps],
          prune,
          g,
          counter + 1
        )
    end
  end

  defp one_path([], w, [{vs, ps} | cont], xs, _, prune, g, counter) do
    one_path(vs, w, cont, xs, ps, prune, g, counter - 1)
  end

  defp one_path([], _, [], _, _, _, _, _Counter) do
    false
  end

  def get_short_cycle(g, v) do
    get_short_path(g, v, v)
  end

  def get_short_path(g, v1, v2) do
    t = new()
    add_vertex(t, v1)
    q = :queue.new()
    q1 = queue_out_neighbours(v1, g, q)
    l = spath(q1, g, v2, t)
    delete(t)
    l
  end

  defp spath(q, g, sink, t) do
    case :queue.out(q) do
      {{:value, e}, q1} ->
        {_E, v1, v2, _Label} = edge(g, e)

        cond do
          sink === v2 ->
            follow_path(v1, t, [v2])

          true ->
            case vertex(t, v2) do
              false ->
                add_vertex(t, v2)
                add_edge(t, v2, v1)
                nQ = queue_out_neighbours(v2, g, q1)
                spath(nQ, g, sink, t)

              _V ->
                spath(q1, g, sink, t)
            end
        end

      {:empty, _Q1} ->
        false
    end
  end

  defp follow_path(v, t, p) do
    p1 = [v | p]

    case out_neighbours(t, v) do
      [n] ->
        follow_path(n, t, p1)

      [] ->
        p1
    end
  end

  defp queue_out_neighbours(v, g, q0) do
    :lists.foldl(
      fn e, q ->
        :queue.in(e, q)
      end,
      q0,
      out_edges(g, v)
    )
  end
end
