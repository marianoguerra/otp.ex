defmodule :m_supervisor do
  use Bitwise
  @behaviour :gen_server
  require Record

  Record.defrecord(:r_child, :child,
    pid: :undefined,
    id: :undefined,
    mfargs: :undefined,
    restart_type: :undefined,
    shutdown: :undefined,
    child_type: :undefined,
    modules: []
  )

  Record.defrecord(:r_state, :state,
    name: :undefined,
    strategy: :undefined,
    children: {[], %{}},
    dynamics: :undefined,
    intensity: :undefined,
    period: :undefined,
    restarts: [],
    dynamic_restarts: 0,
    module: :undefined,
    args: :undefined
  )

  def start_link(mod, args) do
    :gen_server.start_link(:supervisor, {:self, mod, args}, [])
  end

  def start_link(supName, mod, args) do
    :gen_server.start_link(supName, :supervisor, {supName, mod, args}, [])
  end

  def start_child(supervisor, childSpec) do
    call(supervisor, {:start_child, childSpec})
  end

  def restart_child(supervisor, id) do
    call(supervisor, {:restart_child, id})
  end

  def delete_child(supervisor, id) do
    call(supervisor, {:delete_child, id})
  end

  def terminate_child(supervisor, id) do
    call(supervisor, {:terminate_child, id})
  end

  def get_childspec(supervisor, id) do
    call(supervisor, {:get_childspec, id})
  end

  def which_children(supervisor) do
    call(supervisor, :which_children)
  end

  def count_children(supervisor) do
    call(supervisor, :count_children)
  end

  defp call(supervisor, req) do
    :gen_server.call(supervisor, req, :infinity)
  end

  def check_childspecs(childSpecs) when is_list(childSpecs) do
    case check_startspec(childSpecs) do
      {:ok, _} ->
        :ok

      error ->
        {:error, error}
    end
  end

  def check_childspecs(x) do
    {:error, {:badarg, x}}
  end

  def get_callback_module(pid) do
    {:status, _Pid, {:module, _Mod}, [_PDict, _SysState, _Parent, _Dbg, misc]} =
      :sys.get_status(pid)

    case :lists.keyfind(:supervisor, 1, misc) do
      {:supervisor, [{'Callback', mod}]} ->
        mod

      _ ->
        [[_Header, _Data, {:data, [{'State', state}]}] | _] = misc
        r_state(state, :module)
    end
  end

  def init({supName, mod, args}) do
    :erlang.process_flag(:trap_exit, true)

    case mod.init(args) do
      {:ok, {supFlags, startSpec}} ->
        case init_state(supName, supFlags, mod, args) do
          {:ok, state}
          when r_state(state, :strategy) === :simple_one_for_one ->
            init_dynamic(state, startSpec)

          {:ok, state} ->
            init_children(state, startSpec)

          error ->
            {:stop, {:supervisor_data, error}}
        end

      :ignore ->
        :ignore

      error ->
        {:stop, {:bad_return, {mod, :init, error}}}
    end
  end

  defp init_children(state, startSpec) do
    supName = r_state(state, :name)

    case check_startspec(startSpec) do
      {:ok, children} ->
        case start_children(children, supName) do
          {:ok, nChildren} ->
            {:ok, r_state(state, children: nChildren)}

          {:error, nChildren, reason} ->
            _ = terminate_children(nChildren, supName)
            {:stop, {:shutdown, reason}}
        end

      error ->
        {:stop, {:start_spec, error}}
    end
  end

  defp init_dynamic(state, [startSpec]) do
    case check_startspec([startSpec]) do
      {:ok, children} ->
        {:ok, dyn_init(r_state(state, children: children))}

      error ->
        {:stop, {:start_spec, error}}
    end
  end

  defp init_dynamic(_State, startSpec) do
    {:stop, {:bad_start_spec, startSpec}}
  end

  defp start_children(children, supName) do
    start = fn id, child ->
      case do_start_child(supName, child) do
        {:ok, :undefined}
        when r_child(child, :restart_type) === :temporary ->
          :remove

        {:ok, pid} ->
          {:update, r_child(child, pid: pid)}

        {:ok, pid, _Extra} ->
          {:update, r_child(child, pid: pid)}

        {:error, reason} ->
          case :logger.allow(:error, :supervisor) do
            true ->
              :erlang.apply(:logger, :macro_log, [
                %{
                  :mfa => {:supervisor, :start_children, 2},
                  :line => 363,
                  :file => 'otp/lib/stdlib/src/supervisor.erl'
                },
                :error,
                %{
                  :label => {:supervisor, :start_error},
                  :report => [
                    {:supervisor, supName},
                    {:errorContext, :start_error},
                    {:reason, reason},
                    {:offender, extract_child(child)}
                  ]
                },
                %{
                  :domain => [:otp, :sasl],
                  :report_cb => &:supervisor.format_log/2,
                  :logger_formatter => %{:title => 'SUPERVISOR REPORT'},
                  :error_logger => %{
                    :tag => :error_report,
                    :type => :supervisor_report,
                    :report_cb => &:supervisor.format_log/1
                  }
                }
              ])

            false ->
              :ok
          end

          {:abort, {:failed_to_start_child, id, reason}}
      end
    end

    children_map(start, children)
  end

  defp do_start_child(supName, child) do
    r_child(mfargs: {m, f, args}) = child

    case do_start_child_i(m, f, args) do
      {:ok, pid} when is_pid(pid) ->
        nChild = r_child(child, pid: pid)
        report_progress(nChild, supName)
        {:ok, pid}

      {:ok, pid, extra} when is_pid(pid) ->
        nChild = r_child(child, pid: pid)
        report_progress(nChild, supName)
        {:ok, pid, extra}

      other ->
        other
    end
  end

  defp do_start_child_i(m, f, a) do
    case (try do
            apply(m, f, a)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, pid} when is_pid(pid) ->
        {:ok, pid}

      {:ok, pid, extra} when is_pid(pid) ->
        {:ok, pid, extra}

      :ignore ->
        {:ok, :undefined}

      {:error, error} ->
        {:error, error}

      what ->
        {:error, what}
    end
  end

  def handle_call({:start_child, eArgs}, _From, state)
      when r_state(state, :strategy) === :simple_one_for_one do
    child = get_dynamic_child(state)
    r_child(mfargs: {m, f, a}) = child
    args = a ++ eArgs

    case do_start_child_i(m, f, args) do
      {:ok, :undefined} ->
        {:reply, {:ok, :undefined}, state}

      {:ok, pid} ->
        nState = dyn_store(pid, args, state)
        {:reply, {:ok, pid}, nState}

      {:ok, pid, extra} ->
        nState = dyn_store(pid, args, state)
        {:reply, {:ok, pid, extra}, nState}

      what ->
        {:reply, what, state}
    end
  end

  def handle_call({:start_child, childSpec}, _From, state) do
    case check_childspec(childSpec) do
      {:ok, child} ->
        {resp, nState} = handle_start_child(child, state)
        {:reply, resp, nState}

      what ->
        {:reply, {:error, what}, state}
    end
  end

  def handle_call({:terminate_child, id}, _From, state)
      when not is_pid(id) and
             r_state(state, :strategy) === :simple_one_for_one do
    {:reply, {:error, :simple_one_for_one}, state}
  end

  def handle_call({:terminate_child, id}, _From, state) do
    case find_child(id, state) do
      {:ok, child} ->
        do_terminate(child, r_state(state, :name))
        {:reply, :ok, del_child(child, state)}

      :error ->
        {:reply, {:error, :not_found}, state}
    end
  end

  def handle_call({:restart_child, _Id}, _From, state)
      when r_state(state, :strategy) === :simple_one_for_one do
    {:reply, {:error, :simple_one_for_one}, state}
  end

  def handle_call({:restart_child, id}, _From, state) do
    case find_child(id, state) do
      {:ok, child} when r_child(child, :pid) === :undefined ->
        case do_start_child(r_state(state, :name), child) do
          {:ok, pid} ->
            nState = set_pid(pid, id, state)
            {:reply, {:ok, pid}, nState}

          {:ok, pid, extra} ->
            nState = set_pid(pid, id, state)
            {:reply, {:ok, pid, extra}, nState}

          error ->
            {:reply, error, state}
        end

      {:ok, r_child(pid: {:restarting, _})} ->
        {:reply, {:error, :restarting}, state}

      {:ok, _} ->
        {:reply, {:error, :running}, state}

      _ ->
        {:reply, {:error, :not_found}, state}
    end
  end

  def handle_call({:delete_child, _Id}, _From, state)
      when r_state(state, :strategy) === :simple_one_for_one do
    {:reply, {:error, :simple_one_for_one}, state}
  end

  def handle_call({:delete_child, id}, _From, state) do
    case find_child(id, state) do
      {:ok, child} when r_child(child, :pid) === :undefined ->
        nState = remove_child(id, state)
        {:reply, :ok, nState}

      {:ok, r_child(pid: {:restarting, _})} ->
        {:reply, {:error, :restarting}, state}

      {:ok, _} ->
        {:reply, {:error, :running}, state}

      _ ->
        {:reply, {:error, :not_found}, state}
    end
  end

  def handle_call({:get_childspec, id}, _From, state) do
    case find_child(id, state) do
      {:ok, child} ->
        {:reply, {:ok, child_to_spec(child)}, state}

      :error ->
        {:reply, {:error, :not_found}, state}
    end
  end

  def handle_call(:which_children, _From, state)
      when r_state(state, :strategy) === :simple_one_for_one do
    r_child(
      child_type: cT,
      modules: mods
    ) = get_dynamic_child(state)

    reply =
      dyn_map(
        fn
          {:restarting, _} ->
            {:undefined, :restarting, cT, mods}

          pid ->
            {:undefined, pid, cT, mods}
        end,
        state
      )

    {:reply, reply, state}
  end

  def handle_call(:which_children, _From, state) do
    resp =
      children_to_list(
        fn
          id, r_child(pid: {:restarting, _}, child_type: childType, modules: mods) ->
            {id, :restarting, childType, mods}

          id, r_child(pid: pid, child_type: childType, modules: mods) ->
            {id, pid, childType, mods}
        end,
        r_state(state, :children)
      )

    {:reply, resp, state}
  end

  def handle_call(:count_children, _From, r_state(dynamic_restarts: restarts) = state)
      when r_state(state, :strategy) === :simple_one_for_one do
    r_child(child_type: cT) = get_dynamic_child(state)
    sz = dyn_size(state)
    active = sz - restarts

    reply =
      case cT do
        :supervisor ->
          [{:specs, 1}, {:active, active}, {:supervisors, sz}, {:workers, 0}]

        :worker ->
          [{:specs, 1}, {:active, active}, {:supervisors, 0}, {:workers, sz}]
      end

    {:reply, reply, state}
  end

  def handle_call(:count_children, _From, state) do
    {specs, active, supers, workers} =
      children_fold(
        fn _Id, child, counts ->
          count_child(
            child,
            counts
          )
        end,
        {0, 0, 0, 0},
        r_state(state, :children)
      )

    reply = [{:specs, specs}, {:active, active}, {:supervisors, supers}, {:workers, workers}]
    {:reply, reply, state}
  end

  defp count_child(
         r_child(pid: pid, child_type: :worker),
         {specs, active, supers, workers}
       ) do
    case is_pid(pid) and :erlang.is_process_alive(pid) do
      true ->
        {specs + 1, active + 1, supers, workers + 1}

      false ->
        {specs + 1, active, supers, workers + 1}
    end
  end

  defp count_child(
         r_child(pid: pid, child_type: :supervisor),
         {specs, active, supers, workers}
       ) do
    case is_pid(pid) and :erlang.is_process_alive(pid) do
      true ->
        {specs + 1, active + 1, supers + 1, workers}

      false ->
        {specs + 1, active, supers + 1, workers}
    end
  end

  def handle_cast({:try_again_restart, tryAgainId}, state) do
    case find_child_and_args(tryAgainId, state) do
      {:ok, child = r_child(pid: {:restarting, _})} ->
        case restart(child, state) do
          {:ok, state1} ->
            {:noreply, state1}

          {:shutdown, state1} ->
            {:stop, :shutdown, state1}
        end

      _ ->
        {:noreply, state}
    end
  end

  def handle_info({:EXIT, pid, reason}, state) do
    case restart_child(pid, reason, state) do
      {:ok, state1} ->
        {:noreply, state1}

      {:shutdown, state1} ->
        {:stop, :shutdown, state1}
    end
  end

  def handle_info(msg, state) do
    case :logger.allow(:error, :supervisor) do
      true ->
        :erlang.apply(:logger, :macro_log, [
          %{
            :mfa => {:supervisor, :handle_info, 2},
            :line => 588,
            :file => 'otp/lib/stdlib/src/supervisor.erl'
          },
          :error,
          'Supervisor received unexpected message: ~tp~n',
          [msg],
          %{:domain => [:otp], :error_logger => %{:tag => :error}}
        ])

      false ->
        :ok
    end

    {:noreply, state}
  end

  def terminate(_Reason, state)
      when r_state(state, :strategy) === :simple_one_for_one do
    terminate_dynamic_children(state)
  end

  def terminate(_Reason, state) do
    terminate_children(r_state(state, :children), r_state(state, :name))
  end

  def code_change(_, state, _) do
    case r_state(state, :module).init(r_state(state, :args)) do
      {:ok, {supFlags, startSpec}} ->
        case set_flags(supFlags, state) do
          {:ok, state1} ->
            update_childspec(state1, startSpec)

          {:invalid_type, ^supFlags} ->
            {:error, {:bad_flags, supFlags}}

          error ->
            {:error, error}
        end

      :ignore ->
        {:ok, state}

      error ->
        error
    end
  end

  defp update_childspec(state, startSpec)
       when r_state(state, :strategy) === :simple_one_for_one do
    case check_startspec(startSpec) do
      {:ok, {[_], _} = children} ->
        {:ok, r_state(state, children: children)}

      error ->
        {:error, error}
    end
  end

  defp update_childspec(state, startSpec) do
    case check_startspec(startSpec) do
      {:ok, children} ->
        oldC = r_state(state, :children)
        newC = update_childspec1(oldC, children, [])
        {:ok, r_state(state, children: newC)}

      error ->
        {:error, error}
    end
  end

  defp update_childspec1({[id | oldIds], oldDb}, {ids, db}, keepOld) do
    case update_chsp(:maps.get(id, oldDb), db) do
      {:ok, newDb} ->
        update_childspec1({oldIds, oldDb}, {ids, newDb}, keepOld)

      false ->
        update_childspec1({oldIds, oldDb}, {ids, db}, [id | keepOld])
    end
  end

  defp update_childspec1({[], oldDb}, {ids, db}, keepOld) do
    keepOldDb = :maps.with(keepOld, oldDb)
    {:lists.reverse(ids ++ keepOld), :maps.merge(keepOldDb, db)}
  end

  defp update_chsp(r_child(id: id) = oldChild, newDb) do
    case :maps.find(id, newDb) do
      {:ok, child} ->
        {:ok, %{newDb | id => r_child(child, pid: r_child(oldChild, :pid))}}

      :error ->
        false
    end
  end

  defp handle_start_child(child, state) do
    case find_child(r_child(child, :id), state) do
      :error ->
        case do_start_child(r_state(state, :name), child) do
          {:ok, :undefined}
          when r_child(child, :restart_type) === :temporary ->
            {{:ok, :undefined}, state}

          {:ok, pid} ->
            {{:ok, pid}, save_child(r_child(child, pid: pid), state)}

          {:ok, pid, extra} ->
            {{:ok, pid, extra}, save_child(r_child(child, pid: pid), state)}

          {:error, what} ->
            {{:error, {what, child}}, state}
        end

      {:ok, oldChild} when is_pid(r_child(oldChild, :pid)) ->
        {{:error, {:already_started, r_child(oldChild, :pid)}}, state}

      {:ok, _OldChild} ->
        {{:error, :already_present}, state}
    end
  end

  defp restart_child(pid, reason, state) do
    case find_child_and_args(pid, state) do
      {:ok, child} ->
        do_restart(reason, child, state)

      :error ->
        {:ok, state}
    end
  end

  defp do_restart(reason, child, state)
       when r_child(child, :restart_type) === :permanent do
    case :logger.allow(:error, :supervisor) do
      true ->
        :erlang.apply(:logger, :macro_log, [
          %{
            :mfa => {:supervisor, :do_restart, 3},
            :line => 707,
            :file => 'otp/lib/stdlib/src/supervisor.erl'
          },
          :error,
          %{
            :label => {:supervisor, :child_terminated},
            :report => [
              {:supervisor, r_state(state, :name)},
              {:errorContext, :child_terminated},
              {:reason, reason},
              {:offender, extract_child(child)}
            ]
          },
          %{
            :domain => [:otp, :sasl],
            :report_cb => &:supervisor.format_log/2,
            :logger_formatter => %{:title => 'SUPERVISOR REPORT'},
            :error_logger => %{
              :tag => :error_report,
              :type => :supervisor_report,
              :report_cb => &:supervisor.format_log/1
            }
          }
        ])

      false ->
        :ok
    end

    restart(child, state)
  end

  defp do_restart(:normal, child, state) do
    nState = del_child(child, state)
    {:ok, nState}
  end

  defp do_restart(:shutdown, child, state) do
    nState = del_child(child, state)
    {:ok, nState}
  end

  defp do_restart({:shutdown, _Term}, child, state) do
    nState = del_child(child, state)
    {:ok, nState}
  end

  defp do_restart(reason, child, state)
       when r_child(child, :restart_type) === :transient do
    case :logger.allow(:error, :supervisor) do
      true ->
        :erlang.apply(:logger, :macro_log, [
          %{
            :mfa => {:supervisor, :do_restart, 3},
            :line => 719,
            :file => 'otp/lib/stdlib/src/supervisor.erl'
          },
          :error,
          %{
            :label => {:supervisor, :child_terminated},
            :report => [
              {:supervisor, r_state(state, :name)},
              {:errorContext, :child_terminated},
              {:reason, reason},
              {:offender, extract_child(child)}
            ]
          },
          %{
            :domain => [:otp, :sasl],
            :report_cb => &:supervisor.format_log/2,
            :logger_formatter => %{:title => 'SUPERVISOR REPORT'},
            :error_logger => %{
              :tag => :error_report,
              :type => :supervisor_report,
              :report_cb => &:supervisor.format_log/1
            }
          }
        ])

      false ->
        :ok
    end

    restart(child, state)
  end

  defp do_restart(reason, child, state)
       when r_child(child, :restart_type) === :temporary do
    case :logger.allow(:error, :supervisor) do
      true ->
        :erlang.apply(:logger, :macro_log, [
          %{
            :mfa => {:supervisor, :do_restart, 3},
            :line => 722,
            :file => 'otp/lib/stdlib/src/supervisor.erl'
          },
          :error,
          %{
            :label => {:supervisor, :child_terminated},
            :report => [
              {:supervisor, r_state(state, :name)},
              {:errorContext, :child_terminated},
              {:reason, reason},
              {:offender, extract_child(child)}
            ]
          },
          %{
            :domain => [:otp, :sasl],
            :report_cb => &:supervisor.format_log/2,
            :logger_formatter => %{:title => 'SUPERVISOR REPORT'},
            :error_logger => %{
              :tag => :error_report,
              :type => :supervisor_report,
              :report_cb => &:supervisor.format_log/1
            }
          }
        ])

      false ->
        :ok
    end

    nState = del_child(child, state)
    {:ok, nState}
  end

  defp restart(child, state) do
    case add_restart(state) do
      {:ok, nState} ->
        case restart(r_state(nState, :strategy), child, nState) do
          {{:try_again, tryAgainId}, nState2} ->
            try_again_restart(tryAgainId)
            {:ok, nState2}

          other ->
            other
        end

      {:terminate, nState} ->
        case :logger.allow(:error, :supervisor) do
          true ->
            :erlang.apply(:logger, :macro_log, [
              %{
                :mfa => {:supervisor, :restart, 2},
                :line => 742,
                :file => 'otp/lib/stdlib/src/supervisor.erl'
              },
              :error,
              %{
                :label => {:supervisor, :shutdown},
                :report => [
                  {:supervisor, r_state(state, :name)},
                  {:errorContext, :shutdown},
                  {:reason, :reached_max_restart_intensity},
                  {:offender, extract_child(child)}
                ]
              },
              %{
                :domain => [:otp, :sasl],
                :report_cb => &:supervisor.format_log/2,
                :logger_formatter => %{:title => 'SUPERVISOR REPORT'},
                :error_logger => %{
                  :tag => :error_report,
                  :type => :supervisor_report,
                  :report_cb => &:supervisor.format_log/1
                }
              }
            ])

          false ->
            :ok
        end

        {:shutdown, del_child(child, nState)}
    end
  end

  defp restart(:simple_one_for_one, child, state0) do
    r_child(pid: oldPid, mfargs: {m, f, a}) = child

    state1 =
      case oldPid do
        {:restarting, _} ->
          nRes = r_state(state0, :dynamic_restarts) - 1
          r_state(state0, dynamic_restarts: nRes)

        _ ->
          state0
      end

    state2 = dyn_erase(oldPid, state1)

    case do_start_child_i(m, f, a) do
      {:ok, pid} ->
        nState = dyn_store(pid, a, state2)
        {:ok, nState}

      {:ok, pid, _Extra} ->
        nState = dyn_store(pid, a, state2)
        {:ok, nState}

      {:error, error} ->
        rOldPid = restarting(oldPid)
        nRestarts = r_state(state2, :dynamic_restarts) + 1
        state3 = r_state(state2, dynamic_restarts: nRestarts)
        nState = dyn_store(rOldPid, a, state3)

        case :logger.allow(:error, :supervisor) do
          true ->
            :erlang.apply(:logger, :macro_log, [
              %{
                :mfa => {:supervisor, :restart, 3},
                :line => 769,
                :file => 'otp/lib/stdlib/src/supervisor.erl'
              },
              :error,
              %{
                :label => {:supervisor, :start_error},
                :report => [
                  {:supervisor, r_state(nState, :name)},
                  {:errorContext, :start_error},
                  {:reason, error},
                  {:offender, extract_child(child)}
                ]
              },
              %{
                :domain => [:otp, :sasl],
                :report_cb => &:supervisor.format_log/2,
                :logger_formatter => %{:title => 'SUPERVISOR REPORT'},
                :error_logger => %{
                  :tag => :error_report,
                  :type => :supervisor_report,
                  :report_cb => &:supervisor.format_log/1
                }
              }
            ])

          false ->
            :ok
        end

        {{:try_again, rOldPid}, nState}
    end
  end

  defp restart(:one_for_one, r_child(id: id) = child, state) do
    oldPid = r_child(child, :pid)

    case do_start_child(r_state(state, :name), child) do
      {:ok, pid} ->
        nState = set_pid(pid, id, state)
        {:ok, nState}

      {:ok, pid, _Extra} ->
        nState = set_pid(pid, id, state)
        {:ok, nState}

      {:error, reason} ->
        nState = set_pid(restarting(oldPid), id, state)

        case :logger.allow(:error, :supervisor) do
          true ->
            :erlang.apply(:logger, :macro_log, [
              %{
                :mfa => {:supervisor, :restart, 3},
                :line => 783,
                :file => 'otp/lib/stdlib/src/supervisor.erl'
              },
              :error,
              %{
                :label => {:supervisor, :start_error},
                :report => [
                  {:supervisor, r_state(state, :name)},
                  {:errorContext, :start_error},
                  {:reason, reason},
                  {:offender, extract_child(child)}
                ]
              },
              %{
                :domain => [:otp, :sasl],
                :report_cb => &:supervisor.format_log/2,
                :logger_formatter => %{:title => 'SUPERVISOR REPORT'},
                :error_logger => %{
                  :tag => :error_report,
                  :type => :supervisor_report,
                  :report_cb => &:supervisor.format_log/1
                }
              }
            ])

          false ->
            :ok
        end

        {{:try_again, id}, nState}
    end
  end

  defp restart(:rest_for_one, r_child(id: id) = child, r_state(name: supName) = state) do
    {chAfter, chBefore} =
      split_child(
        id,
        r_state(state, :children)
      )

    {return, chAfter2} = restart_multiple_children(child, chAfter, supName)
    {return, r_state(state, children: append(chAfter2, chBefore))}
  end

  defp restart(:one_for_all, child, r_state(name: supName) = state) do
    children1 =
      del_child(
        r_child(child, :id),
        r_state(state, :children)
      )

    {return, nChildren} = restart_multiple_children(child, children1, supName)
    {return, r_state(state, children: nChildren)}
  end

  defp restart_multiple_children(child, children, supName) do
    children1 = terminate_children(children, supName)

    case start_children(children1, supName) do
      {:ok, nChildren} ->
        {:ok, nChildren}

      {:error, nChildren, {:failed_to_start_child, failedId, _Reason}} ->
        newPid =
          cond do
            failedId === r_child(child, :id) ->
              restarting(r_child(child, :pid))

            true ->
              {:restarting, :undefined}
          end

        {{:try_again, failedId}, set_pid(newPid, failedId, nChildren)}
    end
  end

  defp restarting(pid) when is_pid(pid) do
    {:restarting, pid}
  end

  defp restarting(rPid) do
    rPid
  end

  defp try_again_restart(tryAgainId) do
    :gen_server.cast(
      self(),
      {:try_again_restart, tryAgainId}
    )
  end

  defp terminate_children(children, supName) do
    terminate = fn
      _Id, child
      when r_child(child, :restart_type) === :temporary ->
        do_terminate(child, supName)
        :remove

      _Id, child ->
        do_terminate(child, supName)
        {:update, r_child(child, pid: :undefined)}
    end

    {:ok, nChildren} = children_map(terminate, children)
    nChildren
  end

  defp do_terminate(child, supName) when is_pid(r_child(child, :pid)) do
    case shutdown(r_child(child, :pid), r_child(child, :shutdown)) do
      :ok ->
        :ok

      {:error, :normal} when not (r_child(child, :restart_type) === :permanent) ->
        :ok

      {:error, otherReason} ->
        case :logger.allow(:error, :supervisor) do
          true ->
            :erlang.apply(:logger, :macro_log, [
              %{
                :mfa => {:supervisor, :do_terminate, 2},
                :line => 844,
                :file => 'otp/lib/stdlib/src/supervisor.erl'
              },
              :error,
              %{
                :label => {:supervisor, :shutdown_error},
                :report => [
                  {:supervisor, supName},
                  {:errorContext, :shutdown_error},
                  {:reason, otherReason},
                  {:offender, extract_child(child)}
                ]
              },
              %{
                :domain => [:otp, :sasl],
                :report_cb => &:supervisor.format_log/2,
                :logger_formatter => %{:title => 'SUPERVISOR REPORT'},
                :error_logger => %{
                  :tag => :error_report,
                  :type => :supervisor_report,
                  :report_cb => &:supervisor.format_log/1
                }
              }
            ])

          false ->
            :ok
        end
    end

    :ok
  end

  defp do_terminate(_Child, _SupName) do
    :ok
  end

  defp shutdown(pid, :brutal_kill) do
    case monitor_child(pid) do
      :ok ->
        :erlang.exit(pid, :kill)

        receive do
          {:DOWN, _MRef, :process, ^pid, :killed} ->
            :ok

          {:DOWN, _MRef, :process, ^pid, otherReason} ->
            {:error, otherReason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp shutdown(pid, time) do
    case monitor_child(pid) do
      :ok ->
        :erlang.exit(pid, :shutdown)

        receive do
          {:DOWN, _MRef, :process, ^pid, :shutdown} ->
            :ok

          {:DOWN, _MRef, :process, ^pid, otherReason} ->
            {:error, otherReason}
        after
          time ->
            :erlang.exit(pid, :kill)

            receive do
              {:DOWN, _MRef, :process, ^pid, otherReason} ->
                {:error, otherReason}
            end
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp monitor_child(pid) do
    :erlang.monitor(:process, pid)
    :erlang.unlink(pid)

    receive do
      {:EXIT, ^pid, reason} ->
        receive do
          {:DOWN, _, :process, ^pid, _} ->
            {:error, reason}
        end
    after
      0 ->
        :ok
    end
  end

  defp terminate_dynamic_children(state) do
    child = get_dynamic_child(state)
    {pids, eStack0} = monitor_dynamic_children(child, state)
    sz = :maps.size(pids)

    eStack =
      case r_child(child, :shutdown) do
        :brutal_kill ->
          :maps.fold(
            fn p, _, _ ->
              :erlang.exit(p, :kill)
            end,
            :ok,
            pids
          )

          wait_dynamic_children(child, pids, sz, :undefined, eStack0)

        :infinity ->
          :maps.fold(
            fn p, _, _ ->
              :erlang.exit(p, :shutdown)
            end,
            :ok,
            pids
          )

          wait_dynamic_children(child, pids, sz, :undefined, eStack0)

        time ->
          :maps.fold(
            fn p, _, _ ->
              :erlang.exit(p, :shutdown)
            end,
            :ok,
            pids
          )

          tRef = :erlang.start_timer(time, self(), :kill)
          wait_dynamic_children(child, pids, sz, tRef, eStack0)
      end

    :maps.fold(
      fn reason, ls, _ ->
        case :logger.allow(:error, :supervisor) do
          true ->
            :erlang.apply(:logger, :macro_log, [
              %{
                :mfa => {:supervisor, :terminate_dynamic_children, 1},
                :line => 948,
                :file => 'otp/lib/stdlib/src/supervisor.erl'
              },
              :error,
              %{
                :label => {:supervisor, :shutdown_error},
                :report => [
                  {:supervisor, r_state(state, :name)},
                  {:errorContext, :shutdown_error},
                  {:reason, reason},
                  {:offender, extract_child(r_child(child, pid: ls))}
                ]
              },
              %{
                :domain => [:otp, :sasl],
                :report_cb => &:supervisor.format_log/2,
                :logger_formatter => %{:title => 'SUPERVISOR REPORT'},
                :error_logger => %{
                  :tag => :error_report,
                  :type => :supervisor_report,
                  :report_cb => &:supervisor.format_log/1
                }
              }
            ])

          false ->
            :ok
        end
      end,
      :ok,
      eStack
    )
  end

  defp monitor_dynamic_children(child, state) do
    dyn_fold(
      fn
        p, {pids, eStack} when is_pid(p) ->
          case monitor_child(p) do
            :ok ->
              {:maps.put(p, p, pids), eStack}

            {:error, :normal} when not (r_child(child, :restart_type) === :permanent) ->
              {pids, eStack}

            {:error, reason} ->
              {pids, maps_prepend(reason, p, eStack)}
          end

        {:restarting, _}, {pids, eStack} ->
          {pids, eStack}
      end,
      {:maps.new(), :maps.new()},
      state
    )
  end

  defp wait_dynamic_children(_Child, _Pids, 0, :undefined, eStack) do
    eStack
  end

  defp wait_dynamic_children(_Child, _Pids, 0, tRef, eStack) do
    _ = :erlang.cancel_timer(tRef)

    receive do
      {:timeout, ^tRef, :kill} ->
        eStack
    after
      0 ->
        eStack
    end
  end

  defp wait_dynamic_children(r_child(shutdown: :brutal_kill) = child, pids, sz, tRef, eStack) do
    receive do
      {:DOWN, _MRef, :process, pid, :killed} ->
        wait_dynamic_children(child, :maps.remove(pid, pids), sz - 1, tRef, eStack)

      {:DOWN, _MRef, :process, pid, reason} ->
        wait_dynamic_children(
          child,
          :maps.remove(pid, pids),
          sz - 1,
          tRef,
          maps_prepend(reason, pid, eStack)
        )
    end
  end

  defp wait_dynamic_children(child, pids, sz, tRef, eStack) do
    receive do
      {:DOWN, _MRef, :process, pid, :shutdown} ->
        wait_dynamic_children(child, :maps.remove(pid, pids), sz - 1, tRef, eStack)

      {:DOWN, _MRef, :process, pid, {:shutdown, _}} ->
        wait_dynamic_children(child, :maps.remove(pid, pids), sz - 1, tRef, eStack)

      {:DOWN, _MRef, :process, pid, :normal}
      when not (r_child(child, :restart_type) === :permanent) ->
        wait_dynamic_children(child, :maps.remove(pid, pids), sz - 1, tRef, eStack)

      {:DOWN, _MRef, :process, pid, reason} ->
        wait_dynamic_children(
          child,
          :maps.remove(pid, pids),
          sz - 1,
          tRef,
          maps_prepend(reason, pid, eStack)
        )

      {:timeout, ^tRef, :kill} ->
        :maps.fold(
          fn p, _, _ ->
            :erlang.exit(p, :kill)
          end,
          :ok,
          pids
        )

        wait_dynamic_children(child, pids, sz, :undefined, eStack)
    end
  end

  defp maps_prepend(key, value, map) do
    case :maps.find(key, map) do
      {:ok, values} ->
        :maps.put(key, [value | values], map)

      :error ->
        :maps.put(key, [value], map)
    end
  end

  defp save_child(r_child(mfargs: {m, f, _}) = child, state)
       when r_child(child, :restart_type) === :temporary do
    do_save_child(
      r_child(child, mfargs: {m, f, :undefined}),
      state
    )
  end

  defp save_child(child, state) do
    do_save_child(child, state)
  end

  defp do_save_child(
         r_child(id: id) = child,
         r_state(children: {ids, db}) = state
       ) do
    r_state(state, children: {[id | ids], %{db | id => child}})
  end

  defp del_child(r_child(pid: pid), state)
       when r_state(state, :strategy) === :simple_one_for_one do
    dyn_erase(pid, state)
  end

  defp del_child(child, state)
       when elem(child, 0) === :child and
              elem(state, 0) === :state do
    nChildren =
      del_child(
        r_child(child, :id),
        r_state(state, :children)
      )

    r_state(state, children: nChildren)
  end

  defp del_child(id, {ids, db}) do
    case :maps.get(id, db) do
      child when r_child(child, :restart_type) === :temporary ->
        {:lists.delete(id, ids), :maps.remove(id, db)}

      child ->
        {ids, %{db | id => r_child(child, pid: :undefined)}}
    end
  end

  defp split_child(id, {ids, db}) do
    {idsAfter, idsBefore} = split_ids(id, ids, [])
    dbBefore = :maps.with(idsBefore, db)
    %{^id => ch} = dbAfter = :maps.with(idsAfter, db)
    {{idsAfter, %{dbAfter | id => r_child(ch, pid: :undefined)}}, {idsBefore, dbBefore}}
  end

  defp split_ids(id, [id | ids], after__) do
    {:lists.reverse([id | after__]), ids}
  end

  defp split_ids(id, [other | ids], after__) do
    split_ids(id, ids, [other | after__])
  end

  defp find_child(pid, state)
       when is_pid(pid) and
              r_state(state, :strategy) === :simple_one_for_one do
    case find_dynamic_child(pid, state) do
      :error ->
        case find_dynamic_child(restarting(pid), state) do
          :error ->
            case :erlang.is_process_alive(pid) do
              true ->
                :error

              false ->
                {:ok, get_dynamic_child(state)}
            end

          other ->
            other
        end

      other ->
        other
    end
  end

  defp find_child(id, r_state(children: {_Ids, db})) do
    :maps.find(id, db)
  end

  defp find_child_and_args(pid, state)
       when r_state(state, :strategy) === :simple_one_for_one do
    case find_dynamic_child(pid, state) do
      {:ok, r_child(mfargs: {m, f, _}) = child} ->
        {:ok, args} = dyn_args(pid, state)
        {:ok, r_child(child, mfargs: {m, f, args})}

      :error ->
        :error
    end
  end

  defp find_child_and_args(pid, state) when is_pid(pid) do
    find_child_by_pid(pid, state)
  end

  defp find_child_and_args(id, r_state(children: {_Ids, db})) do
    :maps.find(id, db)
  end

  defp find_dynamic_child(pid, state) do
    case dyn_exists(pid, state) do
      true ->
        child = get_dynamic_child(state)
        {:ok, r_child(child, pid: pid)}

      false ->
        :error
    end
  end

  defp find_child_by_pid(pid, r_state(children: {_Ids, db})) do
    fun = fn
      _Id, r_child(pid: p) = ch, _ when p === pid ->
        throw(ch)

      _, _, :error ->
        :error
    end

    try do
      :maps.fold(fun, :error, db)
    catch
      child ->
        {:ok, child}
    end
  end

  defp get_dynamic_child(r_state(children: {[id], db})) do
    %{^id => child} = db
    child
  end

  defp set_pid(pid, id, r_state(children: children) = state) do
    r_state(state, children: set_pid(pid, id, children))
  end

  defp set_pid(pid, id, {ids, db}) do
    newDb =
      :maps.update_with(
        id,
        fn child ->
          r_child(child, pid: pid)
        end,
        db
      )

    {ids, newDb}
  end

  defp remove_child(id, r_state(children: {ids, db}) = state) do
    newIds = :lists.delete(id, ids)
    newDb = :maps.remove(id, db)
    r_state(state, children: {newIds, newDb})
  end

  defp children_map(fun, {ids, db}) do
    children_map(fun, ids, db, [])
  end

  defp children_map(fun, [id | ids], db, acc) do
    case fun.(id, :maps.get(id, db)) do
      {:update, child} ->
        children_map(fun, ids, %{db | id => child}, [id | acc])

      :remove ->
        children_map(fun, ids, :maps.remove(id, db), acc)

      {:abort, reason} ->
        {:error, {:lists.reverse(ids) ++ [id | acc], db}, reason}
    end
  end

  defp children_map(_Fun, [], db, acc) do
    {:ok, {acc, db}}
  end

  defp children_to_list(fun, {ids, db}) do
    children_to_list(fun, ids, db, [])
  end

  defp children_to_list(fun, [id | ids], db, acc) do
    children_to_list(fun, ids, db, [fun.(id, :maps.get(id, db)) | acc])
  end

  defp children_to_list(_Fun, [], _Db, acc) do
    :lists.reverse(acc)
  end

  defp children_fold(fun, init, {_Ids, db}) do
    :maps.fold(fun, init, db)
  end

  defp append({ids1, db1}, {ids2, db2}) do
    {ids1 ++ ids2, :maps.merge(db1, db2)}
  end

  defp init_state(supName, type, mod, args) do
    set_flags(
      type,
      r_state(name: supname(supName, mod), module: mod, args: args)
    )
  end

  defp set_flags(flags, state) do
    try do
      check_flags(flags)
    catch
      thrown ->
        thrown
    else
      %{:strategy => strategy, :intensity => maxIntensity, :period => period} ->
        {:ok, r_state(state, strategy: strategy, intensity: maxIntensity, period: period)}
    end
  end

  defp check_flags(supFlags) when is_map(supFlags) do
    do_check_flags(
      :maps.merge(
        %{:strategy => :one_for_one, :intensity => 1, :period => 5},
        supFlags
      )
    )
  end

  defp check_flags({strategy, maxIntensity, period}) do
    check_flags(%{:strategy => strategy, :intensity => maxIntensity, :period => period})
  end

  defp check_flags(what) do
    throw({:invalid_type, what})
  end

  defp do_check_flags(
         %{:strategy => strategy, :intensity => maxIntensity, :period => period} = flags
       ) do
    validStrategy(strategy)
    validIntensity(maxIntensity)
    validPeriod(period)
    flags
  end

  defp validStrategy(:simple_one_for_one) do
    true
  end

  defp validStrategy(:one_for_one) do
    true
  end

  defp validStrategy(:one_for_all) do
    true
  end

  defp validStrategy(:rest_for_one) do
    true
  end

  defp validStrategy(what) do
    throw({:invalid_strategy, what})
  end

  defp validIntensity(max) when is_integer(max) and max >= 0 do
    true
  end

  defp validIntensity(what) do
    throw({:invalid_intensity, what})
  end

  defp validPeriod(period)
       when is_integer(period) and
              period > 0 do
    true
  end

  defp validPeriod(what) do
    throw({:invalid_period, what})
  end

  defp supname(:self, mod) do
    {self(), mod}
  end

  defp supname(n, _) do
    n
  end

  defp check_startspec(children) do
    check_startspec(children, [], %{})
  end

  defp check_startspec([childSpec | t], ids, db) do
    case check_childspec(childSpec) do
      {:ok, r_child(id: id) = child} ->
        case :maps.is_key(id, db) do
          true ->
            {:duplicate_child_name, id}

          false ->
            check_startspec(t, [id | ids], %{db | id => child})
        end

      error ->
        error
    end
  end

  defp check_startspec([], ids, db) do
    {:ok, {:lists.reverse(ids), db}}
  end

  defp check_childspec(childSpec) when is_map(childSpec) do
    try do
      do_check_childspec(
        :maps.merge(
          %{:restart => :permanent, :type => :worker},
          childSpec
        )
      )
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp check_childspec({id, func, restartType, shutdown, childType, mods}) do
    check_childspec(%{
      :id => id,
      :start => func,
      :restart => restartType,
      :shutdown => shutdown,
      :type => childType,
      :modules => mods
    })
  end

  defp check_childspec(x) do
    {:invalid_child_spec, x}
  end

  defp do_check_childspec(%{:restart => restartType, :type => childType} = childSpec) do
    id =
      case childSpec do
        %{:id => i} ->
          i

        _ ->
          throw(:missing_id)
      end

    func =
      case childSpec do
        %{:start => f} ->
          f

        _ ->
          throw(:missing_start)
      end

    validId(id)
    validFunc(func)
    validRestartType(restartType)
    validChildType(childType)

    shutdown =
      case childSpec do
        %{:shutdown => s} ->
          s

        %{:type => :worker} ->
          5000

        %{:type => :supervisor} ->
          :infinity
      end

    validShutdown(shutdown)

    mods =
      case childSpec do
        %{:modules => ms} ->
          ms

        _ ->
          {m, _, _} = func
          [m]
      end

    validMods(mods)

    {:ok,
     r_child(
       id: id,
       mfargs: func,
       restart_type: restartType,
       shutdown: shutdown,
       child_type: childType,
       modules: mods
     )}
  end

  defp validChildType(:supervisor) do
    true
  end

  defp validChildType(:worker) do
    true
  end

  defp validChildType(what) do
    throw({:invalid_child_type, what})
  end

  defp validId(_Id) do
    true
  end

  defp validFunc({m, f, a})
       when is_atom(m) and is_atom(f) and
              is_list(a) do
    true
  end

  defp validFunc(func) do
    throw({:invalid_mfa, func})
  end

  defp validRestartType(:permanent) do
    true
  end

  defp validRestartType(:temporary) do
    true
  end

  defp validRestartType(:transient) do
    true
  end

  defp validRestartType(restartType) do
    throw({:invalid_restart_type, restartType})
  end

  defp validShutdown(shutdown)
       when is_integer(shutdown) and
              shutdown > 0 do
    true
  end

  defp validShutdown(:infinity) do
    true
  end

  defp validShutdown(:brutal_kill) do
    true
  end

  defp validShutdown(shutdown) do
    throw({:invalid_shutdown, shutdown})
  end

  defp validMods(:dynamic) do
    true
  end

  defp validMods(mods) when is_list(mods) do
    :lists.foreach(
      fn mod ->
        cond do
          is_atom(mod) ->
            :ok

          true ->
            throw({:invalid_module, mod})
        end
      end,
      mods
    )
  end

  defp validMods(mods) do
    throw({:invalid_modules, mods})
  end

  defp child_to_spec(
         r_child(
           id: id,
           mfargs: func,
           restart_type: restartType,
           shutdown: shutdown,
           child_type: childType,
           modules: mods
         )
       ) do
    %{
      :id => id,
      :start => func,
      :restart => restartType,
      :shutdown => shutdown,
      :type => childType,
      :modules => mods
    }
  end

  defp add_restart(state) do
    i = r_state(state, :intensity)
    p = r_state(state, :period)
    r = r_state(state, :restarts)
    now = :erlang.monotonic_time(1)
    r1 = add_restart([now | r], now, p)
    state1 = r_state(state, restarts: r1)

    case length(r1) do
      curI when curI <= i ->
        {:ok, state1}

      _ ->
        {:terminate, state1}
    end
  end

  defp add_restart([r | restarts], now, period) do
    case inPeriod(r, now, period) do
      true ->
        [r | add_restart(restarts, now, period)]

      _ ->
        []
    end
  end

  defp add_restart([], _, _) do
    []
  end

  defp inPeriod(then, now, period) do
    now <= then + period
  end

  defp extract_child(child) when is_list(r_child(child, :pid)) do
    [
      {:nb_children, length(r_child(child, :pid))},
      {:id, r_child(child, :id)},
      {:mfargs, r_child(child, :mfargs)},
      {:restart_type, r_child(child, :restart_type)},
      {:shutdown, r_child(child, :shutdown)},
      {:child_type, r_child(child, :child_type)}
    ]
  end

  defp extract_child(child) do
    [
      {:pid, r_child(child, :pid)},
      {:id, r_child(child, :id)},
      {:mfargs, r_child(child, :mfargs)},
      {:restart_type, r_child(child, :restart_type)},
      {:shutdown, r_child(child, :shutdown)},
      {:child_type, r_child(child, :child_type)}
    ]
  end

  defp report_progress(child, supName) do
    case :logger.allow(:info, :supervisor) do
      true ->
        :erlang.apply(:logger, :macro_log, [
          %{
            :mfa => {:supervisor, :report_progress, 2},
            :line => 1433,
            :file => 'otp/lib/stdlib/src/supervisor.erl'
          },
          :info,
          %{
            :label => {:supervisor, :progress},
            :report => [{:supervisor, supName}, {:started, extract_child(child)}]
          },
          %{
            :domain => [:otp, :sasl],
            :report_cb => &:supervisor.format_log/2,
            :logger_formatter => %{:title => 'PROGRESS REPORT'},
            :error_logger => %{
              :tag => :info_report,
              :type => :progress,
              :report_cb => &:supervisor.format_log/1
            }
          }
        ])

      false ->
        :ok
    end
  end

  def format_log(logReport) do
    depth = :error_logger.get_format_depth()

    formatOpts = %{
      :chars_limit => :unlimited,
      :depth => depth,
      :single_line => false,
      :encoding => :utf8
    }

    format_log_multi(
      limit_report(logReport, depth),
      formatOpts
    )
  end

  defp limit_report(logReport, :unlimited) do
    logReport
  end

  defp limit_report(
         %{
           :label => {:supervisor, :progress},
           :report => [{:supervisor, _} = supervisor, {:started, child}]
         } = logReport,
         depth
       ) do
    %{logReport | :report => [supervisor, {:started, limit_child_report(child, depth)}]}
  end

  defp limit_report(
         %{
           :label => {:supervisor, _Error},
           :report => [
             {:supervisor, _} = supervisor,
             {:errorContext, ctxt},
             {:reason, reason},
             {:offender, child}
           ]
         } = logReport,
         depth
       ) do
    %{
      logReport
      | :report => [
          supervisor,
          {:errorContext, :io_lib.limit_term(ctxt, depth)},
          {:reason, :io_lib.limit_term(reason, depth)},
          {:offender, limit_child_report(child, depth)}
        ]
    }
  end

  defp limit_child_report(report, depth) do
    :io_lib.limit_term(report, depth)
  end

  def format_log(report, formatOpts0) do
    default = %{
      :chars_limit => :unlimited,
      :depth => :unlimited,
      :single_line => false,
      :encoding => :utf8
    }

    formatOpts = :maps.merge(default, formatOpts0)

    ioOpts =
      case formatOpts do
        %{:chars_limit => :unlimited} ->
          []

        %{:chars_limit => limit} ->
          [{:chars_limit, limit}]
      end

    {format, args} = format_log_single(report, formatOpts)
    :io_lib.format(format, args, ioOpts)
  end

  defp format_log_single(
         %{
           :label => {:supervisor, :progress},
           :report => [{:supervisor, supName}, {:started, child}]
         },
         %{:single_line => true, :depth => depth} = formatOpts
       ) do
    p = p(formatOpts)
    {childFormat, childArgs} = format_child_log_single(child, 'Started:')
    format = 'Supervisor: ' ++ p ++ '.'

    args =
      case depth do
        :unlimited ->
          [supName]

        _ ->
          [supName, depth]
      end

    {format ++ childFormat, args ++ childArgs}
  end

  defp format_log_single(
         %{
           :label => {:supervisor, _Error},
           :report => [
             {:supervisor, supName},
             {:errorContext, ctxt},
             {:reason, reason},
             {:offender, child}
           ]
         },
         %{:single_line => true, :depth => depth} = formatOpts
       ) do
    p = p(formatOpts)
    format = :lists.append(['Supervisor: ', p, '. Context: ', p, '. Reason: ', p, '.'])
    {childFormat, childArgs} = format_child_log_single(child, 'Offender:')

    args =
      case depth do
        :unlimited ->
          [supName, ctxt, reason]

        _ ->
          [supName, depth, ctxt, depth, reason, depth]
      end

    {format ++ childFormat, args ++ childArgs}
  end

  defp format_log_single(report, formatOpts) do
    format_log_multi(report, formatOpts)
  end

  defp format_log_multi(
         %{
           :label => {:supervisor, :progress},
           :report => [{:supervisor, supName}, {:started, child}]
         },
         %{:depth => depth} = formatOpts
       ) do
    p = p(formatOpts)
    format = :lists.append(['    supervisor: ', p, '~n', '    started: ', p, '~n'])

    args =
      case depth do
        :unlimited ->
          [supName, child]

        _ ->
          [supName, depth, child, depth]
      end

    {format, args}
  end

  defp format_log_multi(
         %{
           :label => {:supervisor, _Error},
           :report => [
             {:supervisor, supName},
             {:errorContext, ctxt},
             {:reason, reason},
             {:offender, child}
           ]
         },
         %{:depth => depth} = formatOpts
       ) do
    p = p(formatOpts)

    format =
      :lists.append([
        '    supervisor: ',
        p,
        '~n',
        '    errorContext: ',
        p,
        '~n',
        '    reason: ',
        p,
        '~n',
        '    offender: ',
        p,
        '~n'
      ])

    args =
      case depth do
        :unlimited ->
          [supName, ctxt, reason, child]

        _ ->
          [supName, depth, ctxt, depth, reason, depth, child, depth]
      end

    {format, args}
  end

  defp format_child_log_single(child, tag) do
    {:id, id} = :lists.keyfind(:id, 1, child)

    case :lists.keyfind(:pid, 1, child) do
      false ->
        {:nb_children, numCh} = :lists.keyfind(:nb_children, 1, child)
        {' ~s id=~w,nb_children=~w.', [tag, id, numCh]}

      t when is_tuple(t) ->
        {:pid, pid} = :lists.keyfind(:pid, 1, child)
        {' ~s id=~w,pid=~w.', [tag, id, pid]}
    end
  end

  defp p(%{:single_line => single, :depth => depth, :encoding => enc}) do
    '~' ++ single(single) ++ mod(enc) ++ p(depth)
  end

  defp p(:unlimited) do
    'p'
  end

  defp p(_Depth) do
    'P'
  end

  defp single(true) do
    '0'
  end

  defp single(false) do
    ''
  end

  defp mod(:latin1) do
    ''
  end

  defp mod(_) do
    't'
  end

  def format_status(:terminate, [_PDict, state]) do
    state
  end

  def format_status(_, [_PDict, state]) do
    [{:data, [{'State', state}]}, {:supervisor, [{'Callback', r_state(state, :module)}]}]
  end

  defp dyn_size(r_state(dynamics: {_Kind, db})) do
    map_size(db)
  end

  defp dyn_erase(pid, r_state(dynamics: {kind, db}) = state) do
    r_state(state, dynamics: {kind, :maps.remove(pid, db)})
  end

  defp dyn_store(pid, args, r_state(dynamics: {kind, db}) = state) do
    case kind do
      :mapsets ->
        r_state(state, dynamics: {:mapsets, %{db | pid => []}})

      :maps ->
        r_state(state, dynamics: {:maps, %{db | pid => args}})
    end
  end

  defp dyn_fold(fun, init, r_state(dynamics: {_Kind, db})) do
    :maps.fold(
      fn pid, _, acc ->
        fun.(pid, acc)
      end,
      init,
      db
    )
  end

  defp dyn_map(fun, r_state(dynamics: {_Kind, db})) do
    :lists.map(fun, :maps.keys(db))
  end

  defp dyn_exists(pid, r_state(dynamics: {_Kind, db})) do
    :erlang.is_map_key(pid, db)
  end

  defp dyn_args(_Pid, r_state(dynamics: {:mapsets, _Db})) do
    {:ok, :undefined}
  end

  defp dyn_args(pid, r_state(dynamics: {:maps, db})) do
    :maps.find(pid, db)
  end

  defp dyn_init(state) do
    dyn_init(get_dynamic_child(state), state)
  end

  defp dyn_init(child, state)
       when r_child(child, :restart_type) === :temporary do
    r_state(state, dynamics: {:mapsets, :maps.new()})
  end

  defp dyn_init(_Child, state) do
    r_state(state, dynamics: {:maps, :maps.new()})
  end
end
