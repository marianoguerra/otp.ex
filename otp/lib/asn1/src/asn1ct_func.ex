defmodule :m_asn1ct_func do
  use Bitwise

  def start_link() do
    {:ok, pid} = :gen_server.start_link(:asn1ct_func, [], [])
    :erlang.put(:asn1ct_func, pid)
    :ok
  end

  def call(m, f, args) do
    a = length(args)
    mFA = {m, f, a}
    need(mFA)

    case m do
      :binary ->
        :asn1ct_gen.emit(['binary:', f, '(', call_args(args, ''), ')'])

      _ ->
        :asn1ct_gen.emit([f, '(', call_args(args, ''), ')'])
    end
  end

  def need({:binary, _, _}) do
    :ok
  end

  def need({:erlang, _, _}) do
    :ok
  end

  def need(mFA) do
    :asn1ct_rtt.assert_defined(mFA)
    cast({:need, mFA})
  end

  def call_gen(prefix, key, gen, args)
      when is_function(
             gen,
             2
           ) do
    f = req({:gen_func, prefix, key, gen})
    :asn1ct_gen.emit([{:asis, f}, '(', call_args(args, ''), ')'])
  end

  def call_gen(prefix, key, gen) when is_function(gen, 2) do
    req({:gen_func, prefix, key, gen})
  end

  def generate(fd) do
    do_generate(fd)
    used0 = req(:get_used)
    :erlang.erase(:asn1ct_func)
    used = :sofs.set(used0, [:mfa])

    code =
      :sofs.relation(
        :asn1ct_rtt.code(),
        [{:mfa, :code}]
      )

    funcs0 = :sofs.image(code, used)
    funcs = :sofs.to_external(funcs0)
    :ok = :file.write(fd, funcs)
  end

  def is_used({m, f, a} = mFA)
      when is_atom(m) and
             is_atom(f) and is_integer(a) do
    req({:is_used, mFA})
  end

  defp req(req) do
    :gen_server.call(:erlang.get(:asn1ct_func), req, :infinity)
  end

  defp cast(req) do
    :gen_server.cast(:erlang.get(:asn1ct_func), req)
  end

  require Record
  Record.defrecord(:r_st, :st, used: :undefined, gen: :undefined, gc: 1)

  def init([]) do
    st = r_st(used: :gb_sets.empty(), gen: :gb_trees.empty())
    {:ok, st}
  end

  def handle_cast({:need, mFA}, r_st(used: used0) = st) do
    case :gb_sets.is_member(mFA, used0) do
      false ->
        used = pull_in_deps(:gb_sets.singleton(mFA), used0)
        {:noreply, r_st(st, used: used)}

      true ->
        {:noreply, st}
    end
  end

  def handle_call(:get_used, _From, r_st(used: used) = st) do
    {:stop, :normal, :gb_sets.to_list(used), st}
  end

  def handle_call(:get_gen, _From, r_st(gen: g0) = st) do
    {l, g} = do_get_gen(:gb_trees.to_list(g0), [], [])
    {:reply, l, r_st(st, gen: :gb_trees.from_orddict(g))}
  end

  def handle_call({:gen_func, prefix, key, genFun}, _From, r_st(gen: g0, gc: gc0) = st) do
    case :gb_trees.lookup(key, g0) do
      :none ->
        name = :erlang.list_to_atom(prefix ++ :erlang.integer_to_list(gc0))
        gc = gc0 + 1
        g = :gb_trees.insert(key, {name, genFun}, g0)
        {:reply, name, r_st(st, gen: g, gc: gc)}

      {:value, {name, _}} ->
        {:reply, name, st}
    end
  end

  def handle_call({:is_used, mFA}, _From, r_st(used: used) = st) do
    {:reply, :gb_sets.is_member(mFA, used), st}
  end

  def terminate(_, _) do
    :ok
  end

  defp call_args([a | as], sep) do
    [sep, a | call_args(as, ', ')]
  end

  defp call_args([], _) do
    []
  end

  defp pull_in_deps(ws0, used0) do
    case :gb_sets.is_empty(ws0) do
      true ->
        used0

      false ->
        {mFA, ws1} = :gb_sets.take_smallest(ws0)
        used = :gb_sets.add(mFA, used0)
        needs = :asn1ct_rtt.dependencies(mFA)
        ws = update_worklist(needs, used, ws1)
        pull_in_deps(ws, used)
    end
  end

  defp update_worklist([h | t], used, ws) do
    case :gb_sets.is_member(h, used) do
      false ->
        update_worklist(t, used, :gb_sets.add(h, ws))

      true ->
        update_worklist(t, used, ws)
    end
  end

  defp update_worklist([], _, ws) do
    ws
  end

  defp do_get_gen([{_, {_, :done}} = keep | t], gacc, kacc) do
    do_get_gen(t, gacc, [keep | kacc])
  end

  defp do_get_gen([{k, {name, _} = v} | t], gacc, kacc) do
    do_get_gen(t, [v | gacc], [{k, {name, :done}} | kacc])
  end

  defp do_get_gen([], gacc, kacc) do
    {:lists.sort(gacc), :lists.reverse(kacc)}
  end

  defp do_generate(fd) do
    case req(:get_gen) do
      [] ->
        :ok

      [_ | _] = gen ->
        _ =
          for {name, genFun} <- gen do
            :ok = :file.write(fd, '\n')
            genFun.(fd, name)
          end

        do_generate(fd)
    end
  end
end
