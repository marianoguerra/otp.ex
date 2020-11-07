defmodule :m_asn1ct_name do
  use Bitwise

  def start() do
    parent = self()

    case :erlang.get(:asn1ct_name) do
      :undefined ->
        :erlang.put(
          :asn1ct_name,
          spawn_link(fn ->
            ref = :erlang.monitor(:process, parent)
            name_server_loop({ref, parent}, [])
          end)
        )

        :ok

      _Pid ->
        clear()
    end
  end

  defp name_server_loop({ref, parent} = monitor, vars) do
    receive do
      {_From, :clear} ->
        name_server_loop(monitor, [])

      {from, {:current, variable}} ->
        send(from, {:asn1ct_name, get_curr(vars, variable)})
        name_server_loop(monitor, vars)

      {_From, {:new, variable}} ->
        name_server_loop(monitor, new_var(vars, variable))

      {from, {:prev, variable}} ->
        send(from, {:asn1ct_name, get_prev(vars, variable)})
        name_server_loop(monitor, vars)

      {from, {:next, variable}} ->
        send(from, {:asn1ct_name, get_next(vars, variable)})
        name_server_loop(monitor, vars)

      {:DOWN, ^ref, :process, ^parent, reason} ->
        exit(reason)
    end
  end

  defp req(req) do
    pid = :erlang.get(:asn1ct_name)
    ref = :erlang.monitor(:process, pid)
    send(pid, {self(), req})

    receive do
      {:asn1ct_name, reply} ->
        reply

      {:DOWN, ^ref, :process, ^pid, reason} ->
        :erlang.error({:name_server_died, reason})
    end
  end

  defp cast(req) do
    send(:erlang.get(:asn1ct_name), {self(), req})
    :ok
  end

  def clear() do
    cast(:clear)
  end

  def curr(v) do
    req({:current, v})
  end

  def new(v) do
    cast({:new, v})
  end

  def prev(v) do
    case req({:prev, v}) do
      :none ->
        exit(:"cant get prev of none")

      rep ->
        rep
    end
  end

  def next(v) do
    req({:next, v})
  end

  def all(v) do
    curr = curr(v)

    cond do
      curr == v ->
        []

      true ->
        :lists.reverse(generate(v, last(curr), [], 0))
    end
  end

  defp generate(v, number, res, pos) do
    ell = pos + 1

    cond do
      ell > number ->
        res

      true ->
        generate(v, number, [:erlang.list_to_atom(:lists.concat([v, ell])) | res], ell)
    end
  end

  defp last(v) do
    last2(:lists.reverse(:erlang.atom_to_list(v)))
  end

  defp last2(revL) do
    :erlang.list_to_integer(:lists.reverse(get_digs(revL)))
  end

  defp get_digs([h | t]) do
    cond do
      h < ?9 + 1 and h > ?0 - 1 ->
        [h | get_digs(t)]

      true ->
        []
    end
  end

  defp get_curr([], variable) do
    variable
  end

  defp get_curr([{variable, digit} | _Tail], variable) do
    :erlang.list_to_atom(:lists.concat([variable, digit]))
  end

  defp get_curr([_ | tail], variable) do
    get_curr(tail, variable)
  end

  defp new_var(vars, variable) do
    case :lists.keyfind(variable, 1, vars) do
      false ->
        [{variable, 1} | vars]

      {^variable, digit} ->
        newVars = :lists.keydelete(variable, 1, vars)
        [{variable, digit + 1} | newVars]
    end
  end

  defp get_prev(vars, variable) do
    case :lists.keyfind(variable, 1, vars) do
      false ->
        :none

      {^variable, digit} when digit <= 1 ->
        variable

      {^variable, digit} when digit > 1 ->
        :erlang.list_to_atom(:lists.concat([variable, digit - 1]))
    end
  end

  defp get_next(vars, variable) do
    case :lists.keyfind(variable, 1, vars) do
      false ->
        :erlang.list_to_atom(:lists.concat([variable, '1']))

      {^variable, digit} when digit >= 0 ->
        :erlang.list_to_atom(:lists.concat([variable, digit + 1]))
    end
  end
end
