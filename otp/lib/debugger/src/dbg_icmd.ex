defmodule :m_dbg_icmd do
  use Bitwise
  require Record

  Record.defrecord(:r_ieval, :ieval,
    level: 1,
    line: -1,
    module: :undefined,
    function: :undefined,
    arguments: :undefined,
    top: false
  )

  def cmd(expr, bs, ieval) do
    cmd(expr, bs, :erlang.get(:next_break), ieval)
  end

  defp cmd(expr, bs, :break, ieval) do
    break(expr, bs, ieval)
  end

  defp cmd(expr, bs, :running, r_ieval(level: le, module: m) = ieval) do
    line = :erlang.element(2, expr)

    case break_p(m, line, le, bs) do
      true ->
        :erlang.put(:next_break, :break)
        break(expr, bs, ieval)

      false ->
        handle_cmd(bs, :running, ieval)
    end
  end

  defp cmd(expr, bs, next, r_ieval(level: le) = ieval)
       when is_integer(next) and next < le do
    line = :erlang.element(2, expr)
    handle_cmd(bs, next, r_ieval(ieval, line: line))
  end

  defp cmd(expr, bs, next, r_ieval(level: le) = ieval)
       when is_integer(next) and next >= le do
    :erlang.put(:next_break, :break)
    break(expr, bs, ieval)
  end

  defp break_p(mod, line, le, bs) do
    case :lists.keysearch({mod, line}, 1, :erlang.get(:breakpoints)) do
      {:value, {_Point, [:active, action, _, cond__]}} ->
        case :erlang.get(:user_eval) do
          [{^line, ^le} | _] ->
            false

          _ ->
            bool =
              case cond__ do
                :null ->
                  true

                {cM, cN} ->
                  try do
                    apply(cM, cN, [bs])
                  catch
                    _C, _R ->
                      false
                  else
                    true ->
                      true

                    false ->
                      false

                    _Term ->
                      false
                  end
              end

            cond do
              bool ->
                case action do
                  :enable ->
                    :ignore

                  :disable ->
                    :dbg_iserver.cast(
                      :erlang.get(:int),
                      {:break_option, {mod, line}, :status, :inactive}
                    )

                  :delete ->
                    :dbg_iserver.cast(
                      :erlang.get(:int),
                      {:delete_break, {mod, line}}
                    )
                end

              true ->
                :ignore
            end

            bool
        end

      _Other ->
        false
    end
  end

  defp break(expr, bs, r_ieval(level: le, module: m) = ieval) do
    line = :erlang.element(2, expr)

    :dbg_iserver.cast(
      :erlang.get(:int),
      {:set_status, self(), :break, {m, line}}
    )

    tell_attached({:break_at, m, line, le})
    handle_cmd(bs, :break, r_ieval(ieval, line: line))
  end

  defp handle_cmd(bs, :break, r_ieval(level: le) = ieval) do
    receive do
      {:user, {:cmd, cmd}} ->
        :dbg_iserver.cast(
          :erlang.get(:int),
          {:set_status, self(), :running, {}}
        )

        tell_attached(:running)

        case cmd do
          :step ->
            bs

          :next ->
            :erlang.put(:next_break, le)
            bs

          :continue ->
            :erlang.put(:next_break, :running)
            bs

          :finish ->
            :erlang.put(:next_break, le - 1)
            bs

          :skip ->
            {:skip, bs}
        end

      {:user, {:eval, cmd}} ->
        bs1 = eval_nonrestricted(cmd, bs, ieval)
        handle_cmd(bs1, :break, ieval)

      msg ->
        :dbg_ieval.check_exit_msg(msg, bs, ieval)
        handle_msg(msg, :break, bs, ieval)
        handle_cmd(bs, :break, ieval)
    end
  end

  defp handle_cmd(bs, status, ieval) do
    receive do
      msg ->
        :dbg_ieval.check_exit_msg(msg, bs, ieval)
        handle_msg(msg, status, bs, ieval)
        handle_cmd(bs, status, ieval)
    after
      0 ->
        bs
    end
  end

  def step(meta) do
    send(meta, {:user, {:cmd, :step}})
    :ok
  end

  def next(meta) do
    send(meta, {:user, {:cmd, :next}})
    :ok
  end

  def continue(meta) do
    send(meta, {:user, {:cmd, :continue}})
    :ok
  end

  def finish(meta) do
    send(meta, {:user, {:cmd, :finish}})
    :ok
  end

  def skip(meta) do
    send(meta, {:user, {:cmd, :skip}})
  end

  def timeout(meta) do
    send(meta, {:user, :timeout})
  end

  def stop(meta) do
    send(meta, {:user, {:cmd, :stop}})
  end

  def eval(meta, {mod, cmd}) do
    eval(meta, {mod, cmd, :nostack})
  end

  def eval(meta, {mod, cmd, sP}) do
    cmd2 =
      case :lists.reverse(cmd) do
        [10, ?. | _] ->
          cmd

        [10 | t] ->
          :lists.reverse([10, ?. | t])

        [?. | t] ->
          :lists.reverse([10, ?. | t])

        t ->
          :lists.reverse([10, ?. | t])
      end

    send(meta, {:user, {:eval, {self(), mod, cmd2, sP}}})
  end

  def set(meta, tag, args) do
    send(meta, {:user, {:set, tag, args}})
  end

  def get(meta, tag, args) do
    send(meta, {:user, {:get, tag, self(), args}})

    receive do
      {^meta, ^tag, reply} ->
        reply
    end
  end

  def handle_msg({:int, msg}, status, bs, ieval) do
    handle_int_msg(msg, status, bs, ieval)
  end

  def handle_msg({:user, msg}, status, bs, ieval) do
    handle_user_msg(msg, status, bs, ieval)
  end

  def handle_msg(msg, status, bs, ieval) do
    :io.format('***WARNING*** Unexp msg ~p, info ~p~n', [msg, {status, bs, ieval}])
  end

  defp handle_int_msg({:attached, attPid}, status, _Bs, r_ieval(level: le, module: m, line: line)) do
    :erlang.put(:attached, attPid)
    :erlang.put(:next_break, :break)

    cond do
      le === 1 ->
        tell_attached({:attached, :undefined, -1, :erlang.get(:trace)})

      true ->
        tell_attached({:attached, m, line, :erlang.get(:trace)})

        msg =
          case status do
            :idle ->
              {:func_at, m, line, le}

            :break ->
              {:break_at, m, line, le}

            :wait_at ->
              {:wait_at, m, line, le}

            :wait_after_at ->
              {:wait_after_at, m, line, le}

            _ ->
              :running
          end

        tell_attached(msg)
    end
  end

  defp handle_int_msg(:detached, _Status, _Bs, _Ieval) do
    :erlang.put(:attached, :undefined)
    :erlang.put(:next_break, :running)
    :erlang.put(:trace, false)
  end

  defp handle_int_msg({:old_code, mod}, status, bs, r_ieval(level: le, module: m) = ieval) do
    cond do
      status === :idle and le === 1 ->
        :erlang.erase({mod, :db})
        :erlang.put(:cache, [])

      true ->
        case :dbg_istk.in_use_p(mod, m) do
          true ->
            :erlang.exit(:erlang.get(:self), :kill)
            :dbg_ieval.exception(:exit, :old_code, bs, ieval)

          false ->
            :erlang.erase({mod, :db})
            :erlang.put(:cache, [])
        end
    end
  end

  defp handle_int_msg({:new_break, break}, _Status, _Bs, _Ieval) do
    :erlang.put(
      :breakpoints,
      [break | :erlang.get(:breakpoints)]
    )
  end

  defp handle_int_msg({:delete_break, point}, _Status, _Bs, _Ieval) do
    :erlang.put(
      :breakpoints,
      :lists.keydelete(point, 1, :erlang.get(:breakpoints))
    )
  end

  defp handle_int_msg({:break_options, break}, _Status, _Bs, _Ieval) do
    {point, _Options} = break

    :erlang.put(
      :breakpoints,
      :lists.keyreplace(point, 1, :erlang.get(:breakpoints), break)
    )
  end

  defp handle_int_msg(:no_break, _Status, _Bs, _Ieval) do
    :erlang.put(:breakpoints, [])
  end

  defp handle_int_msg({:no_break, m}, _Status, _Bs, _Ieval) do
    :erlang.put(
      :breakpoints,
      for {mod, _L} = mL <- :erlang.get(:breakpoints),
          mod !== m do
        mL
      end
    )
  end

  defp handle_int_msg(:stop, :exit_at, _Bs, _Ieval) do
    :erlang.exit(:normal)
  end

  defp handle_user_msg({:cmd, :stop}, status, _Bs, _Ieval) do
    case :lists.member(
           status,
           [:running, :wait_at, :wait_after_at]
         ) do
      true ->
        :erlang.put(:next_break, :break)

      false when is_integer(status) or is_tuple(status) ->
        :erlang.put(:next_break, :break)

      false ->
        :ignore
    end
  end

  defp handle_user_msg({:cmd, :continue}, status, _Bs, _Ieval) do
    case :lists.member(
           status,
           [:wait_at, :wait_after_at]
         ) do
      true ->
        :erlang.put(:next_break, :running)

      false ->
        :ignore
    end
  end

  defp handle_user_msg({:cmd, _Cmd}, _Status, _Bs, _Ieval) do
    :ignore
  end

  defp handle_user_msg(:timeout, _Status, _Bs, _Ieval) do
    :ignore
  end

  defp handle_user_msg({:eval, cmd}, :wait_at, bs, _Ieval) do
    eval_restricted(cmd, bs)
  end

  defp handle_user_msg({:eval, cmd}, :wait_after_at, bs, _Ieval) do
    eval_restricted(cmd, bs)
  end

  defp handle_user_msg({:set, :trace, bool}, _Status, _Bs, _Ieval) do
    :erlang.put(:trace, bool)
    tell_attached({:trace, bool})
  end

  defp handle_user_msg({:set, :stack_trace, flag}, _Status, _Bs, _Ieval) do
    set_stack_trace(flag)
  end

  defp handle_user_msg({:get, :bindings, from, sP}, _Status, bs, _Ieval) do
    reply(from, :bindings, bindings(bs, sP))
  end

  defp handle_user_msg({:get, :stack_frame, from, {dir, sP}}, _Status, _Bs, _Ieval) do
    reply(from, :stack_frame, :dbg_istk.stack_frame(dir, sP))
  end

  defp handle_user_msg({:get, :messages, from, _}, _Status, _Bs, _Ieval) do
    reply(from, :messages, messages())
  end

  defp handle_user_msg({:get, :backtrace, from, n}, _Status, _Bs, ieval) do
    reply(from, :backtrace, :dbg_istk.backtrace(n, ieval))
  end

  defp set_stack_trace(true) do
    set_stack_trace(:all)
  end

  defp set_stack_trace(flag) do
    cond do
      flag === false ->
        :erlang.put(:stack, [])

      flag === :no_tail or flag === :all ->
        :ignore
    end

    :erlang.put(:trace_stack, flag)
    tell_attached({:stack_trace, flag})
  end

  defp reply(from, tag, reply) do
    send(from, {self(), tag, reply})
  end

  defp bindings(bs, :nostack) do
    bs
  end

  defp bindings(bs, sP) do
    case :dbg_istk.stack_level() do
      le when sP > le ->
        bs

      _ ->
        :dbg_istk.bindings(sP)
    end
  end

  defp messages() do
    {:messages, msgs} =
      :erlang.process_info(
        :erlang.get(:self),
        :messages
      )

    msgs
  end

  defp eval_restricted({from, _Mod, cmd, sP}, bs) do
    case (try do
            parse_cmd(cmd, 1)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _Reason} ->
        send(from, {self(), {:eval_rsp, :"Parse error"}})

      {[{:var, _, var}], xBs} ->
        bs2 = bindings(bs, sP)

        res =
          case get_binding(var, bs2) do
            {:value, value} ->
              value

            :unbound ->
              case get_binding(var, xBs) do
                {:value, _} ->
                  :"Only possible to inspect variables"

                :unbound ->
                  :unbound
              end
          end

        send(from, {self(), {:eval_rsp, res}})

      {_Forms, _XBs} ->
        rsp = :"Only possible to inspect variables"
        send(from, {self(), {:eval_rsp, rsp}})
    end
  end

  defp eval_nonrestricted({from, mod, cmd, sP}, bs, r_ieval(level: le))
       when sP < le do
    _ = eval_restricted({from, mod, cmd, sP}, bs)
    bs
  end

  defp eval_nonrestricted(
         {from, _Mod, cmd, _SP},
         bs,
         r_ieval(level: le, module: m, line: line) = ieval
       ) do
    case (try do
            parse_cmd(cmd, line)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _Reason} ->
        send(from, {self(), {:eval_rsp, :"Parse error"}})
        bs

      {forms, xBs} ->
        mark_running(line, le)
        bs1 = merge_bindings(bs, xBs)

        {res, bs2} =
          :lists.foldl(
            fn expr, {_Res, bs0} ->
              eval_nonrestricted_1(expr, bs0, ieval)
            end,
            {:null, bs1},
            forms
          )

        mark_break(m, line, le)
        send(from, {self(), {:eval_rsp, res}})
        remove_binding_structs(bs2, xBs)
    end
  end

  defp eval_nonrestricted_1({:match, _, {:var, _, var}, expr}, bs, ieval) do
    {res, bs2} = eval_expr(expr, bs, ieval)

    bs3 =
      case :lists.keyfind(var, 1, bs) do
        {^var, _Value} ->
          :lists.keyreplace(var, 1, bs2, {var, res})

        false ->
          [{var, res} | bs2]
      end

    {res, bs3}
  end

  defp eval_nonrestricted_1({:var, _, var}, bs, _Ieval) do
    res =
      case :lists.keyfind(var, 1, bs) do
        {^var, value} ->
          value

        false ->
          :unbound
      end

    {res, bs}
  end

  defp eval_nonrestricted_1(expr, bs, ieval) do
    eval_expr(expr, bs, ieval)
  end

  defp eval_expr(expr, bs, ieval) do
    {:value, res, bs2} = :dbg_ieval.eval_expr(expr, bs, r_ieval(ieval, top: false))
    {res, bs2}
  end

  defp merge_bindings(bs1, xBs) do
    bs1 ++ :erl_eval.bindings(xBs)
  end

  defp remove_binding_structs(bs1, xBs) do
    :lists.foldl(
      fn {n, _V}, bs ->
        :lists.keydelete(n, 1, bs)
      end,
      bs1,
      :erl_eval.bindings(xBs)
    )
  end

  defp mark_running(lineNo, le) do
    :erlang.put(:next_break, :running)

    :erlang.put(
      :user_eval,
      [{lineNo, le} | :erlang.get(:user_eval)]
    )

    :dbg_iserver.cast(
      :erlang.get(:int),
      {:set_status, self(), :running, {}}
    )

    tell_attached(:running)
  end

  defp mark_break(cm, lineNo, le) do
    :erlang.put(:next_break, :break)
    :erlang.put(:user_eval, tl(:erlang.get(:user_eval)))
    tell_attached({:break_at, cm, lineNo, le})

    :dbg_iserver.cast(
      :erlang.get(:int),
      {:set_status, self(), :break, {cm, lineNo}}
    )
  end

  defp parse_cmd(cmd, lineNo) do
    {:ok, tokens, _} = :erl_scan.string(cmd, lineNo, [:text])
    {:ok, forms, bs} = :erl_eval.extended_parse_exprs(tokens)
    {forms, bs}
  end

  def tell_attached(msg) do
    case :erlang.get(:attached) do
      :undefined ->
        :ignore

      attPid ->
        send(attPid, {self(), msg})
        :ignore
    end
  end

  def get_binding(var, bs) do
    case :lists.keyfind(var, 1, bs) do
      {^var, value} ->
        {:value, value}

      false ->
        :unbound
    end
  end
end
