defmodule :m_eunit_serial do
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
    listeners: :undefined,
    cancelled: :eunit_lib.trie_new(),
    messages: :dict.new()
  )

  def start(pids) do
    spawn(serializer_fun(pids))
  end

  defp serializer_fun(pids) do
    fn ->
      st =
        r_state(
          listeners: :sets.from_list(pids),
          cancelled: :eunit_lib.trie_new(),
          messages: :dict.new()
        )

      expect([], :undefined, 0, st)
      exit(:normal)
    end
  end

  defp expect(id, parentId, groupMinSize, st0) do
    case wait(id, :begin, parentId, groupMinSize, st0) do
      {:done, st1} ->
        {true, st1}

      {:cancel, :prefix, _Msg, st1} ->
        {true, st1}

      {:cancel, :exact, msg, st1} ->
        cast_cancel(id, msg, st1)
        {false, st1}

      {:ok, msg, st1} ->
        cast(msg, st1)

        st2 =
          case msg do
            {:status, _, {:progress, :begin, {:group, _Info}}} ->
              group(id, 0, st1)

            _ ->
              st1
          end

        case wait(id, :end, parentId, groupMinSize, st2) do
          {:cancel, why, msg1, st3} ->
            cast_cancel(id, msg1, st3)
            {why === :prefix, st3}

          {:ok, msg1, st3} ->
            cast(msg1, st3)
            {false, st3}
        end
    end
  end

  defp group(parentId, groupMinSize, st) do
    n = groupMinSize + 1

    case expect(parentId ++ [n], parentId, groupMinSize, st) do
      {false, st1} ->
        group(parentId, n, st1)

      {true, st1} ->
        st1
    end
  end

  defp cast_cancel(id, :undefined, st) do
    cast({:status, id, {:cancel, :undefined}}, st)
  end

  defp cast_cancel(_Id, msg, st) do
    cast(msg, st)
  end

  defp cast(msg, st) do
    :sets.fold(
      fn l, m ->
        send(l, m)
      end,
      msg,
      r_state(st, :listeners)
    )

    :ok
  end

  defp wait(id, type, parentId, groupMinSize, st) do
    case check_cancelled(id, st) do
      :no ->
        case recall(id, st) do
          :undefined ->
            wait_1(id, type, parentId, groupMinSize, st)

          msg ->
            {:ok, msg, forget(id, st)}
        end

      why ->
        {:cancel, why, recall(id, st), forget(id, st)}
    end
  end

  defp wait_1(id, type, parentId, groupMinSize, st) do
    receive do
      {:status, ^id, {:progress, ^type, _}} = msg ->
        {:ok, msg, st}

      {:status, ^parentId, {:progress, :end, {^groupMinSize, _}}} = msg ->
        {:done, remember(parentId, msg, st)}

      {:status, someId, {:cancel, _Cause}} = msg ->
        st1 = set_cancelled(someId, msg, st)
        wait(id, type, parentId, groupMinSize, st1)
    end
  end

  defp set_cancelled(id, msg, st0) do
    st = remember(id, msg, st0)

    r_state(st,
      cancelled:
        :eunit_lib.trie_store(
          id,
          r_state(st0, :cancelled)
        )
    )
  end

  defp check_cancelled(id, st) do
    :eunit_lib.trie_match(id, r_state(st, :cancelled))
  end

  defp remember(id, msg, st) do
    r_state(st, messages: :dict.store(id, msg, r_state(st, :messages)))
  end

  defp forget(id, st) do
    r_state(st, messages: :dict.store(id, :undefined, r_state(st, :messages)))
  end

  defp recall(id, st) do
    case :dict.find(id, r_state(st, :messages)) do
      {:ok, msg} ->
        msg

      :error ->
        :undefined
    end
  end
end
