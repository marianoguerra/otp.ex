defmodule :m_digraph_utils do
  use Bitwise

  def components(g) do
    forest(g, &inout/3)
  end

  def strong_components(g) do
    forest(g, &in/3, revpostorder(g))
  end

  def cyclic_strong_components(g) do
    remove_singletons(strong_components(g), g, [])
  end

  def reachable(vs, g) when is_list(vs) do
    :lists.append(forest(g, &out/3, vs, :first))
  end

  def reachable_neighbours(vs, g) when is_list(vs) do
    :lists.append(forest(g, &out/3, vs, :not_first))
  end

  def reaching(vs, g) when is_list(vs) do
    :lists.append(forest(g, &in/3, vs, :first))
  end

  def reaching_neighbours(vs, g) when is_list(vs) do
    :lists.append(forest(g, &in/3, vs, :not_first))
  end

  def topsort(g) do
    l = revpostorder(g)

    case length(forest(g, &in/3, l)) === length(:digraph.vertices(g)) do
      true ->
        l

      false ->
        false
    end
  end

  def is_acyclic(g) do
    loop_vertices(g) === [] and topsort(g) !== false
  end

  def arborescence_root(g) do
    case :digraph.no_edges(g) === :digraph.no_vertices(g) - 1 do
      true ->
        try do
          f = fn v, z ->
            case :digraph.in_degree(g, v) do
              1 ->
                z

              0 when z === [] ->
                [v]
            end
          end

          [root] = :lists.foldl(f, [], :digraph.vertices(g))
          {:yes, root}
        catch
          _, _ ->
            :no
        end

      false ->
        :no
    end
  end

  def is_arborescence(g) do
    arborescence_root(g) !== :no
  end

  def is_tree(g) do
    :digraph.no_edges(g) === :digraph.no_vertices(g) - 1 and length(components(g)) === 1
  end

  def loop_vertices(g) do
    for v <- :digraph.vertices(g),
        is_reflexive_vertex(v, g) do
      v
    end
  end

  def subgraph(g, vs) do
    try do
      subgraph_opts(g, vs, [])
    catch
      :badarg ->
        :erlang.error(:badarg)
    end
  end

  def subgraph(g, vs, opts) do
    try do
      subgraph_opts(g, vs, opts)
    catch
      :badarg ->
        :erlang.error(:badarg)
    end
  end

  def condensation(g) do
    sCs = strong_components(g)
    v2I = :ets.new(:condensation, [])
    i2C = :ets.new(:condensation, [])

    cFun = fn sC, n ->
      :lists.foreach(
        fn v ->
          true = :ets.insert(v2I, {v, n})
        end,
        sC
      )

      true = :ets.insert(i2C, {n, sC})
      n + 1
    end

    :lists.foldl(cFun, 1, sCs)
    sCG = subgraph_opts(g, [], [])

    :lists.foreach(
      fn sC ->
        condense(sC, g, sCG, v2I, i2C)
      end,
      sCs
    )

    :ets.delete(v2I)
    :ets.delete(i2C)
    sCG
  end

  def preorder(g) do
    :lists.reverse(revpreorder(g))
  end

  def postorder(g) do
    :lists.reverse(revpostorder(g))
  end

  defp forest(g, sF) do
    forest(g, sF, :digraph.vertices(g))
  end

  defp forest(g, sF, vs) do
    forest(g, sF, vs, :first)
  end

  defp forest(g, sF, vs, handleFirst) do
    t = :ets.new(:forest, [:set])

    f = fn v, lL ->
      pretraverse(handleFirst, v, sF, g, t, lL)
    end

    lL = :lists.foldl(f, [], vs)
    :ets.delete(t)
    lL
  end

  defp pretraverse(:first, v, sF, g, t, lL) do
    ptraverse([v], sF, g, t, [], lL)
  end

  defp pretraverse(:not_first, v, sF, g, t, lL) do
    case :ets.member(t, v) do
      false ->
        ptraverse(sF.(g, v, []), sF, g, t, [], lL)

      true ->
        lL
    end
  end

  defp ptraverse([v | vs], sF, g, t, rs, lL) do
    case :ets.member(t, v) do
      false ->
        :ets.insert(t, {v})
        ptraverse(sF.(g, v, vs), sF, g, t, [v | rs], lL)

      true ->
        ptraverse(vs, sF, g, t, rs, lL)
    end
  end

  defp ptraverse([], _SF, _G, _T, [], lL) do
    lL
  end

  defp ptraverse([], _SF, _G, _T, rs, lL) do
    [rs | lL]
  end

  defp revpreorder(g) do
    :lists.append(forest(g, &out/3))
  end

  defp revpostorder(g) do
    t = :ets.new(:forest, [:set])
    l = posttraverse(:digraph.vertices(g), g, t, [])
    :ets.delete(t)
    l
  end

  defp posttraverse([v | vs], g, t, l) do
    l1 =
      case :ets.member(t, v) do
        false ->
          :ets.insert(t, {v})
          [v | posttraverse(out(g, v, []), g, t, l)]

        true ->
          l
      end

    posttraverse(vs, g, t, l1)
  end

  defp posttraverse([], _G, _T, l) do
    l
  end

  defp unquote(:in)(g, v, vs) do
    :digraph.in_neighbours(g, v) ++ vs
  end

  defp out(g, v, vs) do
    :digraph.out_neighbours(g, v) ++ vs
  end

  defp inout(g, v, vs) do
    __MODULE__.in(g, v, out(g, v, vs))
  end

  defp remove_singletons([c = [v] | cs], g, l) do
    case is_reflexive_vertex(v, g) do
      true ->
        remove_singletons(cs, g, [c | l])

      false ->
        remove_singletons(cs, g, l)
    end
  end

  defp remove_singletons([c | cs], g, l) do
    remove_singletons(cs, g, [c | l])
  end

  defp remove_singletons([], _G, l) do
    l
  end

  defp is_reflexive_vertex(v, g) do
    :lists.member(v, :digraph.out_neighbours(g, v))
  end

  defp subgraph_opts(g, vs, opts) do
    subgraph_opts(opts, :inherit, true, g, vs)
  end

  defp subgraph_opts([{:type, type} | opts], _Type0, keep, g, vs)
       when type === :inherit or is_list(type) do
    subgraph_opts(opts, type, keep, g, vs)
  end

  defp subgraph_opts([{:keep_labels, keep} | opts], type, _Keep0, g, vs)
       when is_boolean(keep) do
    subgraph_opts(opts, type, keep, g, vs)
  end

  defp subgraph_opts([], :inherit, keep, g, vs) do
    info = :digraph.info(g)
    {_, {_, cyclicity}} = :lists.keysearch(:cyclicity, 1, info)
    {_, {_, protection}} = :lists.keysearch(:protection, 1, info)
    subgraph(g, vs, [cyclicity, protection], keep)
  end

  defp subgraph_opts([], type, keep, g, vs) do
    subgraph(g, vs, type, keep)
  end

  defp subgraph_opts(_, _Type, _Keep, _G, _Vs) do
    throw(:badarg)
  end

  defp subgraph(g, vs, type, keep) do
    try do
      :digraph.new(type)
    catch
      :error, :badarg ->
        throw(:badarg)
    else
      sG ->
        :lists.foreach(
          fn v ->
            subgraph_vertex(v, g, sG, keep)
          end,
          vs
        )

        eFun = fn v ->
          :lists.foreach(
            fn e ->
              subgraph_edge(e, g, sG, keep)
            end,
            :digraph.out_edges(g, v)
          )
        end

        :lists.foreach(eFun, :digraph.vertices(sG))
        sG
    end
  end

  defp subgraph_vertex(v, g, sG, keep) do
    case :digraph.vertex(g, v) do
      false ->
        :ok

      _ when not keep ->
        :digraph.add_vertex(sG, v)

      {_V, label} when keep ->
        :digraph.add_vertex(sG, v, label)
    end
  end

  defp subgraph_edge(e, g, sG, keep) do
    {_E, v1, v2, label} = :digraph.edge(g, e)

    case :digraph.vertex(sG, v2) do
      false ->
        :ok

      _ when not keep ->
        :digraph.add_edge(sG, e, v1, v2, [])

      _ when keep ->
        :digraph.add_edge(sG, e, v1, v2, label)
    end
  end

  defp condense(sC, g, sCG, v2I, i2C) do
    t = :ets.new(:condense, [])

    nFun = fn neighbour ->
      [{_V, i}] = :ets.lookup(v2I, neighbour)
      :ets.insert(t, {i})
    end

    vFun = fn v ->
      :lists.foreach(nFun, :digraph.out_neighbours(g, v))
    end

    :lists.foreach(vFun, sC)
    :digraph.add_vertex(sCG, sC)
    condense(:ets.first(t), t, sC, g, sCG, i2C)
    :ets.delete(t)
  end

  defp condense(:"$end_of_table", _T, _SC, _G, _SCG, _I2C) do
    :ok
  end

  defp condense(i, t, sC, g, sCG, i2C) do
    [{_, c}] = :ets.lookup(i2C, i)
    :digraph.add_vertex(sCG, c)

    _ =
      for _ <- [:EFE_DUMMY_GEN], c !== sC do
        :digraph.add_edge(sCG, sC, c)
      end

    condense(:ets.next(t, i), t, sC, g, sCG, i2C)
  end
end
