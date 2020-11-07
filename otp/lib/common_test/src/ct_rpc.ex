defmodule :m_ct_rpc do
  use Bitwise

  def app_node(app, candidates) do
    app_node(app, candidates, true, [])
  end

  def app_node(app, candidates, failOnBadRPC) do
    app_node(app, candidates, failOnBadRPC, [])
  end

  def app_node(app, [], _, _) do
    :ct.fail({:application_not_running, app})
  end

  def app_node(app, _Candidates = [candidateNode | nodes], failOnBadRPC, cookie) do
    cookie0 = set_the_cookie(cookie)
    result = :rpc.call(candidateNode, :application, :which_applications, [])
    _ = set_the_cookie(cookie0)

    case result do
      {:badrpc, reason} when failOnBadRPC == true ->
        :ct.fail({reason, candidateNode})

      {:badrpc, _} when failOnBadRPC == false ->
        app_node(app, nodes, failOnBadRPC)

      apps ->
        case :lists.keysearch(app, 1, apps) do
          {:value, _} ->
            candidateNode

          _ ->
            app_node(app, nodes, failOnBadRPC)
        end
    end
  end

  def call(node, module, function, args) do
    call(node, module, function, args, :infinity, [])
  end

  def call(node, module, function, args, timeOut) do
    call(node, module, function, args, timeOut, [])
  end

  def call({fun, funArgs}, module, function, args, timeOut, cookie) do
    node = fun.(funArgs)
    call(node, module, function, args, timeOut, cookie)
  end

  def call(node, module, function, args, timeOut, cookie)
      when is_atom(node) do
    cookie0 = set_the_cookie(cookie)
    result = :rpc.call(node, module, function, args, timeOut)
    _ = set_the_cookie(cookie0)
    result
  end

  def cast(node, module, function, args) do
    cast(node, module, function, args, [])
  end

  def cast({fun, funArgs}, module, function, args, cookie) do
    node = fun.(funArgs)
    cast(node, module, function, args, cookie)
  end

  def cast(node, module, function, args, cookie)
      when is_atom(node) do
    cookie0 = set_the_cookie(cookie)
    true = :rpc.cast(node, module, function, args)
    _ = set_the_cookie(cookie0)
    :ok
  end

  defp set_the_cookie([]) do
    []
  end

  defp set_the_cookie(cookie) do
    cookie0 = :erlang.get_cookie()
    :erlang.set_cookie(node(), cookie)
    cookie0
  end
end
