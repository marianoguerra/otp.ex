defmodule :m_eunit_listener do
  use Bitwise
  require Record

  Record.defrecord(:r_test, :test,
    f: :undefined,
    desc: :undefined,
    timeout: :undefined,
    location: :undefined,
    line: 0
  )

  Record.defrecord(:r_group, :group,
    desc: :undefined,
    order: :undefined,
    timeout: :undefined,
    context: :undefined,
    spawn: :undefined,
    tests: :undefined
  )

  Record.defrecord(:r_context, :context, setup: :undefined, cleanup: :undefined, process: :local)

  Record.defrecord(:r_state, :state,
    callback: :undefined,
    pass: 0,
    fail: 0,
    skip: 0,
    cancel: 0,
    state: :undefined
  )

  def start(callback) do
    start(callback, [])
  end

  def start(callback, options) do
    st = r_state(callback: callback)

    :erlang.spawn_opt(
      init_fun(st, options),
      :proplists.get_all_values(:spawn, options)
    )
  end

  defp init_fun(st0, options) do
    fn ->
      st1 = call(:init, [options], st0)
      st2 = expect([], :undefined, st1)

      data = [
        {:pass, r_state(st2, :pass)},
        {:fail, r_state(st2, :fail)},
        {:skip, r_state(st2, :skip)},
        {:cancel, r_state(st2, :cancel)}
      ]

      call(:terminate, [{:ok, data}, r_state(st2, :state)], st2)
      exit(:normal)
    end
  end

  defp expect(id, parentId, st) do
    case wait_for(id, :begin, parentId) do
      {:done, data} ->
        {:done, data, st}

      {:ok, msg} ->
        case msg do
          {:group, data} ->
            group(id, data, st)

          {:test, data} ->
            st1 = handle_begin(:test, id, data, st)

            case wait_for(id, :end, parentId) do
              {:cancel, reason} ->
                handle_cancel(:test, id, data, reason, st1)

              {:ok, result} ->
                handle_end(:test, id, data, result, st1)
            end
        end
    end
  end

  defp group(id, data, st) do
    st1 = handle_begin(:group, id, data, st)
    group_loop(0, id, data, st1)
  end

  defp group_loop(n, id, data, st) do
    n1 = n + 1

    case expect(id ++ [n1], id, st) do
      {:done, {:cancel, reason}, st1} ->
        handle_cancel(:group, id, data, reason, st1)

      {:done, result, st1} ->
        handle_end(:group, id, data, result, st1)

      st1 ->
        group_loop(n1, id, data, st1)
    end
  end

  defp wait_for(id, type, parentId) do
    :ok

    receive do
      {:status, ^id, {:progress, ^type, data}} ->
        :ok
        {:ok, data}

      {:status, ^parentId, {:progress, :end, data}}
      when type === :begin ->
        :ok
        {:done, data}

      {:status, ^id, {:cancel, reason}} when type === :end ->
        :ok
        {:cancel, reason}

      {:status, ^parentId, {:cancel, _Reason}} ->
        :ok
        {:done, {:cancel, _Reason}}
    end
  end

  defp call(f, as, st) when is_atom(f) do
    try do
      apply(r_state(st, :callback), f, as)
    catch
      class, term ->
        cond do
          f !== :terminate ->
            call(
              :terminate,
              [{:error, {class, term, __STACKTRACE__}}, r_state(st, :state)],
              st
            )

          true ->
            :ok
        end

        :erlang.raise(class, term, __STACKTRACE__)
    else
      substate ->
        r_state(st, state: substate)
    end
  end

  defp handle_begin(:group, id, data0, st) do
    data = [{:id, id} | data0]
    :ok
    call(:handle_begin, [:group, data, r_state(st, :state)], st)
  end

  defp handle_begin(:test, id, data0, st) do
    data = [{:id, id} | data0]
    :ok
    call(:handle_begin, [:test, data, r_state(st, :state)], st)
  end

  defp handle_end(:group, id, data0, {count, data1}, st) do
    data = [[{:id, id}, {:size, count}] | data0 ++ data1]
    :ok
    call(:handle_end, [:group, data, r_state(st, :state)], st)
  end

  defp handle_end(:test, id, data0, {status, data1}, st) do
    data = [[{:id, id}, {:status, status}] | data0 ++ data1]
    :ok

    st1 =
      case status do
        :ok ->
          r_state(st, pass: r_state(st, :pass) + 1)

        {:skipped, _} ->
          r_state(st, skip: r_state(st, :skip) + 1)

        {:error, _} ->
          r_state(st, fail: r_state(st, :fail) + 1)
      end

    call(:handle_end, [:test, data, r_state(st, :state)], st1)
  end

  defp handle_cancel(:group, id, data0, reason, st) do
    data = [[{:id, id}, {:reason, reason}] | data0]
    :ok

    call(
      :handle_cancel,
      [:group, data, r_state(st, :state)],
      r_state(st, cancel: r_state(st, :cancel) + 1)
    )
  end

  defp handle_cancel(:test, id, data0, reason, st) do
    data = [[{:id, id}, {:reason, reason}] | data0]
    :ok

    call(
      :handle_cancel,
      [:test, data, r_state(st, :state)],
      r_state(st, cancel: r_state(st, :cancel) + 1)
    )
  end
end
