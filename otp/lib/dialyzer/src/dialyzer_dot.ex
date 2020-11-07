defmodule :m_dialyzer_dot do
  use Bitwise

  def translate_digraph(g, fileName, gName) do
    translate_digraph(
      g,
      fileName,
      gName,
      fn x ->
        :io_lib.format('~p', [x])
      end,
      []
    )
  end

  def translate_digraph(g, fileName, gName, fun, opts) do
    edges =
      for x <- :digraph.edges(g) do
        :digraph.edge(g, x)
      end

    edgeList =
      for {_, x, y, _} <- edges do
        {x, y}
      end

    translate_list(edgeList, fileName, gName, fun, opts)
  end

  def translate_list(list, fileName, gName) do
    translate_list(
      list,
      fileName,
      gName,
      fn x ->
        :lists.flatten(:io_lib.format('~p', [x]))
      end,
      []
    )
  end

  def translate_list(list, fileName, gName, opts) do
    translate_list(
      list,
      fileName,
      gName,
      fn x ->
        :lists.flatten(:io_lib.format('~p', [x]))
      end,
      opts
    )
  end

  def translate_list(list, fileName, gName, fun, opts) do
    {nodeList1, nodeList2} = :lists.unzip(list)
    nodeList = nodeList1 ++ nodeList2
    nodeSet = :ordsets.from_list(nodeList)
    start = ['digraph ', gName, ' {']

    vertexList =
      for v <- nodeSet do
        node_format(opts, fun, v)
      end

    end__ = ['graph [', gName, '=', gName, ']}']

    edgeList =
      for {x, y} <- list do
        edge_format(opts, fun, x, y)
      end

    string = [start, vertexList, edgeList, end__]

    :ok =
      :file.write_file(
        fileName,
        :erlang.list_to_binary(string)
      )
  end

  defp node_format(opt, fun, v) do
    optText = nodeoptions(opt, fun, v)
    tmp = :io_lib.format('~p', [fun.(v)])
    string = :lists.flatten(tmp)
    {width, heigth} = calc_dim(string)
    w = (div(width, 7) + 1) * 0.55
    h = heigth * 0.4
    sL = :io_lib.format('~f', [w])
    sH = :io_lib.format('~f', [h])
    [string, ' [width=', sL, ' heigth=', sH, ' ', optText, '];\n']
  end

  defp edge_format(opt, fun, v1, v2) do
    optText =
      case :lists.flatten(edgeoptions(opt, fun, v1, v2)) do
        [] ->
          []

        [_ | x] ->
          x
      end

    string = [:io_lib.format('~p', [fun.(v1)]), ' -> ', :io_lib.format('~p', [fun.(v2)])]
    [string, ' [', optText, '];\n']
  end

  defp calc_dim(string) do
    calc_dim(string, 1, 0, 0)
  end

  defp calc_dim('\\n' ++ t, h, tmpW, maxW) do
    calc_dim(t, h + 1, 0, :erlang.max(tmpW, maxW))
  end

  defp calc_dim([_ | t], h, tmpW, maxW) do
    calc_dim(t, h, tmpW + 1, maxW)
  end

  defp calc_dim([], h, tmpW, maxW) do
    {:erlang.max(tmpW, maxW), h}
  end

  defp edgeoptions([{:all_edges, {optName, optVal}} | t], fun, v1, v2) do
    case legal_edgeoption(optName) do
      true ->
        [:io_lib.format(',~p=~p ', [optName, optVal]) | edgeoptions(t, fun, v1, v2)]
    end
  end

  defp edgeoptions([{n1, n2, {optName, optVal}} | t], fun, v1, v2) do
    case fun.(n1) === fun.(v1) and fun.(n2) === fun.(v2) do
      true ->
        [:io_lib.format(',~p=~p ', [optName, optVal]) | edgeoptions(t, fun, v1, v2)]

      false ->
        edgeoptions(t, fun, v1, v2)
    end
  end

  defp edgeoptions([_ | t], fun, v1, v2) do
    edgeoptions(t, fun, v1, v2)
  end

  defp edgeoptions([], _, _, _) do
    []
  end

  defp nodeoptions([{:all_nodes, {optName, optVal}} | t], fun, v) do
    case legal_nodeoption(optName) do
      true ->
        [:io_lib.format(',~p=~p ', [optName, optVal]) | nodeoptions(t, fun, v)]

      false ->
        nodeoptions(t, fun, v)
    end
  end

  defp nodeoptions([{node, {optName, optVal}} | t], fun, v) do
    case fun.(node) === fun.(v) and legal_nodeoption(optName) do
      true ->
        [:io_lib.format('~p=~p ', [optName, optVal]) | nodeoptions(t, fun, v)]

      false ->
        nodeoptions(t, fun, v)
    end
  end

  defp nodeoptions([_ | t], fun, v) do
    nodeoptions(t, fun, v)
  end

  defp nodeoptions([], _Fun, _V) do
    []
  end

  defp legal_nodeoption(:bottomlabel) do
    true
  end

  defp legal_nodeoption(:color) do
    true
  end

  defp legal_nodeoption(:comment) do
    true
  end

  defp legal_nodeoption(:distortion) do
    true
  end

  defp legal_nodeoption(:fillcolor) do
    true
  end

  defp legal_nodeoption(:fixedsize) do
    true
  end

  defp legal_nodeoption(:fontcolor) do
    true
  end

  defp legal_nodeoption(:fontname) do
    true
  end

  defp legal_nodeoption(:fontsize) do
    true
  end

  defp legal_nodeoption(:group) do
    true
  end

  defp legal_nodeoption(:height) do
    true
  end

  defp legal_nodeoption(:label) do
    true
  end

  defp legal_nodeoption(:layer) do
    true
  end

  defp legal_nodeoption(:orientation) do
    true
  end

  defp legal_nodeoption(:peripheries) do
    true
  end

  defp legal_nodeoption(:regular) do
    true
  end

  defp legal_nodeoption(:shape) do
    true
  end

  defp legal_nodeoption(:shapefile) do
    true
  end

  defp legal_nodeoption(:sides) do
    true
  end

  defp legal_nodeoption(:skew) do
    true
  end

  defp legal_nodeoption(:style) do
    true
  end

  defp legal_nodeoption(:toplabel) do
    true
  end

  defp legal_nodeoption(:URL) do
    true
  end

  defp legal_nodeoption(:z) do
    true
  end

  defp legal_nodeoption(option) when is_atom(option) do
    false
  end

  defp legal_edgeoption(option) when is_atom(option) do
    true
  end
end
