defmodule :m_beam_digraph do
  use Bitwise
  import :lists, only: [foldl: 3, reverse: 1]
  require Record
  Record.defrecord(:r_dg, :dg, vs: %{}, in_es: %{}, out_es: %{})

  def new() do
    r_dg()
  end

  def add_vertex(dg, v) do
    add_vertex(dg, v, :vertex)
  end

  def add_vertex(dg, v, label) do
    r_dg(in_es: inEsMap0, out_es: outEsMap0, vs: vs0) = dg
    inEsMap = init_edge_map(v, inEsMap0)
    outEsMap = init_edge_map(v, outEsMap0)
    vs = %{vs0 | v => label}
    r_dg(dg, vs: vs, in_es: inEsMap, out_es: outEsMap)
  end

  defp init_edge_map(v, esMap) do
    case :erlang.is_map_key(v, esMap) do
      true ->
        esMap

      false ->
        %{esMap | v => :ordsets.new()}
    end
  end

  def add_edge(dg, from, to) do
    add_edge(dg, from, to, :edge)
  end

  def add_edge(dg, from, to, label) do
    r_dg(in_es: inEsMap0, out_es: outEsMap0) = dg
    name = {from, to, label}
    inEsMap = edge_map_add(to, name, inEsMap0)
    outEsMap = edge_map_add(from, name, outEsMap0)
    r_dg(dg, in_es: inEsMap, out_es: outEsMap)
  end

  defp edge_map_add(v, e, esMap) do
    es0 = :erlang.map_get(v, esMap)
    es = :ordsets.add_element(e, es0)
    %{esMap | v => es}
  end

  def del_edge(dg, {from, to, _} = e) do
    r_dg(in_es: inEsMap0, out_es: outEsMap0) = dg
    inEsMap = edge_map_del(to, e, inEsMap0)
    outEsMap = edge_map_del(from, e, outEsMap0)
    r_dg(dg, in_es: inEsMap, out_es: outEsMap)
  end

  defp edge_map_del(v, e, esMap) do
    es0 = :erlang.map_get(v, esMap)
    es = es0 -- [e]
    %{esMap | v => es}
  end

  def del_edges(g, es) when is_list(es) do
    foldl(
      fn e, a ->
        del_edge(a, e)
      end,
      g,
      es
    )
  end

  def has_vertex(r_dg(vs: vs), v) do
    :erlang.is_map_key(v, vs)
  end

  def in_degree(r_dg(in_es: inEsMap), v) do
    length(:erlang.map_get(v, inEsMap))
  end

  def in_edges(r_dg(in_es: inEsMap), v) do
    :erlang.map_get(v, inEsMap)
  end

  def in_neighbours(r_dg(in_es: inEsMap), v) do
    for {from, _, _} <- :erlang.map_get(v, inEsMap) do
      from
    end
  end

  def is_path(g, from, to) do
    seen = :cerl_sets.new()

    try do
      _ = is_path_1([from], to, g, seen)
      false
    catch
      true ->
        true
    end
  end

  defp is_path_1([to | _], to, _G, _Seen) do
    throw(true)
  end

  defp is_path_1([v | vs], to, g, seen0) do
    case :cerl_sets.is_element(v, seen0) do
      true ->
        is_path_1(vs, to, g, seen0)

      false ->
        seen1 = :cerl_sets.add_element(v, seen0)
        successors = out_neighbours(g, v)
        seen = is_path_1(successors, to, g, seen1)
        is_path_1(vs, to, g, seen)
    end
  end

  defp is_path_1([], _To, _G, seen) do
    seen
  end

  def out_degree(r_dg(out_es: outEsMap), v) do
    length(:erlang.map_get(v, outEsMap))
  end

  def out_edges(r_dg(out_es: outEsMap), v) do
    :erlang.map_get(v, outEsMap)
  end

  def out_neighbours(r_dg(out_es: outEsMap), v) do
    for {_, to, _} <- :erlang.map_get(v, outEsMap) do
      to
    end
  end

  def vertex(r_dg(vs: vs), v) do
    :erlang.map_get(v, vs)
  end

  def vertices(r_dg(vs: vs)) do
    :maps.to_list(vs)
  end

  def reverse_postorder(g, vs) do
    seen = :cerl_sets.new()
    {rPO, _} = reverse_postorder_1(vs, g, seen, [])
    rPO
  end

  defp reverse_postorder_1([v | vs], g, seen0, acc0) do
    case :cerl_sets.is_element(v, seen0) do
      true ->
        reverse_postorder_1(vs, g, seen0, acc0)

      false ->
        seen1 = :cerl_sets.add_element(v, seen0)
        successors = out_neighbours(g, v)
        {acc, seen} = reverse_postorder_1(successors, g, seen1, acc0)
        reverse_postorder_1(vs, g, seen, [v | acc])
    end
  end

  defp reverse_postorder_1([], _, seen, acc) do
    {acc, seen}
  end

  def roots(g) do
    roots_1(vertices(g), g)
  end

  defp roots_1([{v, _} | vs], g) do
    case in_degree(g, v) do
      0 ->
        [v | roots_1(vs, g)]

      _ ->
        roots_1(vs, g)
    end
  end

  defp roots_1([], _G) do
    []
  end

  def topsort(g) do
    seen = roots(g)
    reverse_postorder(g, seen)
  end

  def strong_components(g, vs) do
    sc_1(vs, g, %{}, %{})
  end

  defp sc_1([v | vs], g, roots0, components) when not :erlang.is_map_key(v, roots0) do
    {roots, component} = sc_2([v], g, v, roots0, [])
    sc_1(vs, g, roots, %{components | v => component})
  end

  defp sc_1([v | vs], g, roots, components0) do
    root = :erlang.map_get(v, roots)
    components = %{components0 | v => :erlang.map_get(root, components0)}
    sc_1(vs, g, roots, components)
  end

  defp sc_1([], _G, _Roots, components) do
    components
  end

  defp sc_2([v | vs], g, root, roots, acc) when not :erlang.is_map_key(v, roots) do
    sc_2(in_neighbours(g, v) ++ vs, g, root, %{roots | v => root}, [v | acc])
  end

  defp sc_2([_V | vs], g, root, roots, acc) do
    sc_2(vs, g, root, roots, acc)
  end

  defp sc_2([], _G, _Root, roots, acc) do
    {roots, reverse(acc)}
  end
end
