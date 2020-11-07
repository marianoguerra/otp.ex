defmodule :m_ct_repeat do
  use Bitwise

  def loop_test(if__, args) when is_list(args) do
    {:ok, cwd} = :file.get_cwd()

    case get_loop_info(args) do
      :no_loop ->
        false

      e = {:error, _} ->
        :io.format('Common Test error: ~tp\n\n', [e])
        :ok = :file.set_cwd(cwd)
        e

      {:repeat, n} ->
        :io.format('\nCommon Test: Will repeat tests ~w times.\n\n', [n])
        args1 = [{:loop_info, [{:repeat, 1, n}]} | args]
        result = loop(if__, :repeat, 0, n, :undefined, args1, :undefined, [])
        :ok = :file.set_cwd(cwd)
        result

      {:stop_time, stopTime} ->
        result =
          case remaining_time(stopTime) do
            0 ->
              :io.format('\nCommon Test: No time left to run tests.\n\n', [])
              {:error, :not_enough_time}

            secs ->
              :io.format('\nCommon Test: Will repeat tests for ~s.\n\n', [ts(secs)])

              tPid =
                case :proplists.get_value(
                       :force_stop,
                       args
                     ) do
                  false__
                  when false__ == false or
                         false__ == :undefined ->
                    :undefined

                  forceStop ->
                    ctrlPid = self()

                    spawn(fn ->
                      :ct_util.mark_process()
                      stop_after(ctrlPid, secs, forceStop)
                    end)
                end

              args1 = [
                {:loop_info, [{:stop_time, secs, stopTime, 1}]}
                | args
              ]

              loop(if__, :stop_time, 0, secs, stopTime, args1, tPid, [])
          end

        :ok = :file.set_cwd(cwd)
        result
    end
  end

  defp loop(_, :repeat, n, n, _, _Args, _, accResult) do
    :lists.reverse(accResult)
  end

  defp loop(if__, type, n, data0, data1, args, tPid, accResult) do
    pid = spawn_tester(if__, self(), args)

    receive do
      {:EXIT, ^pid, reason} ->
        case reason do
          {:user_error, what} ->
            :io.format('\nTest run failed!\nReason: ~tp\n\n\n', [what])
            cancel(tPid)
            {:error, what}

          _ ->
            :io.format(
              'Test run crashed! This could be an internal error - please report!\n\n~tp\n\n\n',
              [reason]
            )

            cancel(tPid)
            {:error, reason}
        end

      {^pid, {:error, reason}} ->
        :io.format('\nTest run failed!\nReason: ~tp\n\n\n', [reason])
        cancel(tPid)
        {:error, reason}

      {^pid, result} ->
        cond do
          type == :repeat ->
            :io.format('\nTest run ~w(~w) complete.\n\n\n', [n + 1, data0])
            :lists.keydelete(:loop_info, 1, args)
            args1 = [{:loop_info, [{:repeat, n + 2, data0}]} | args]
            loop(if__, :repeat, n + 1, data0, data1, args1, tPid, [result | accResult])

          type == :stop_time ->
            case remaining_time(data1) do
              0 ->
                :io.format('\nTest time (~s) has run out.\n\n\n', [ts(data0)])
                cancel(tPid)
                :lists.reverse([result | accResult])

              secs ->
                :io.format('\n~s of test time remaining, starting run #~w...\n\n\n', [
                  ts(secs),
                  n + 2
                ])

                :lists.keydelete(:loop_info, 1, args)
                sT = {:stop_time, data0, data1, n + 2}
                args1 = [{:loop_info, [sT]} | args]
                loop(if__, :stop_time, n + 1, data0, data1, args1, tPid, [result | accResult])
            end
        end
    end
  end

  defp spawn_tester(:script, ctrl, args) do
    spawn_link(fn ->
      :ct_run.script_start1(ctrl, args)
    end)
  end

  defp spawn_tester(:func, ctrl, opts) do
    tester = fn ->
      :ct_util.mark_process()

      case (try do
              :ct_run.run_test2(opts)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:EXIT, reason} ->
          exit(reason)

        result ->
          send(ctrl, {self(), result})
      end
    end

    spawn_link(tester)
  end

  defp remaining_time(stopTime) do
    now = :calendar.datetime_to_gregorian_seconds(:calendar.local_time())
    diff = stopTime - now

    cond do
      diff > 0 ->
        diff

      true ->
        0
    end
  end

  defp get_loop_info(args) when is_list(args) do
    case :lists.keysearch(:until, 1, args) do
      {:value, {:until, time}} ->
        time1 = delistify(time)

        case (try do
                get_stop_time(:until, time1)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            {:error, {:bad_time_format, time1}}

          stop ->
            {:stop_time, stop}
        end

      false ->
        case :lists.keysearch(:duration, 1, args) do
          {:value, {:duration, time}} ->
            time1 = delistify(time)

            case (try do
                    get_stop_time(:duration, time1)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              {:EXIT, _} ->
                {:error, {:bad_time_format, time1}}

              stop ->
                {:stop_time, stop}
            end

          false ->
            case :lists.keysearch(:repeat, 1, args) do
              {:value, {:repeat, r}} ->
                case r do
                  n when is_integer(n) and n > 0 ->
                    {:repeat, n}

                  [str] ->
                    case (try do
                            :erlang.list_to_integer(str)
                          catch
                            :error, e -> {:EXIT, {e, __STACKTRACE__}}
                            :exit, e -> {:EXIT, e}
                            e -> e
                          end) do
                      n when is_integer(n) and n > 0 ->
                        {:repeat, n}

                      _ ->
                        {:error, {:invalid_repeat_value, str}}
                    end

                  _ ->
                    {:error, {:invalid_repeat_value, r}}
                end

              false ->
                :no_loop
            end
        end
    end
  end

  defp get_stop_time(
         :until,
         [y1, y2, mo1, mo2, d1, d2, h1, h2, mi1, mi2, s1, s2]
       ) do
    date =
      case [mo1, mo2] do
        '00' ->
          :erlang.date()

        _ ->
          y = :erlang.list_to_integer([y1, y2])
          mo = :erlang.list_to_integer([mo1, mo2])
          d = :erlang.list_to_integer([d1, d2])
          {yNow, _, _} = :erlang.date()
          dec = trunc(yNow / 100)

          year =
            cond do
              y < yNow - dec * 100 ->
                (dec + 1) * 100 + y

              true ->
                dec * 100 + y
            end

          {year, mo, d}
      end

    time =
      {:erlang.list_to_integer([h1, h2]), :erlang.list_to_integer([mi1, mi2]),
       :erlang.list_to_integer([s1, s2])}

    :calendar.datetime_to_gregorian_seconds({date, time})
  end

  defp get_stop_time(:until, time = [_, _, _, _, _, _]) do
    get_stop_time(:until, '000000' ++ time)
  end

  defp get_stop_time(:duration, [h1, h2, mi1, mi2, s1, s2]) do
    secs =
      :erlang.list_to_integer([h1, h2]) * 3600 + :erlang.list_to_integer([mi1, mi2]) * 60 +
        :erlang.list_to_integer([s1, s2])

    :calendar.datetime_to_gregorian_seconds(:calendar.local_time()) + secs
  end

  defp cancel(pid) do
    try do
      :erlang.exit(pid, :kill)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp stop_after(_CtrlPid, secs, forceStop) do
    :timer.sleep(secs * 1000)

    case forceStop do
      skipRest when skipRest == :skip_rest or skipRest == ['skip_rest'] ->
        :ct_util.set_testdata({:skip_rest, true})

      _ ->
        :ok
    end

    :test_server_ctrl.abort()
  end

  def log_loop_info(args) do
    case :lists.keysearch(:loop_info, 1, args) do
      false ->
        :ok

      {:value, {_, [{:repeat, c, n}]}} ->
        :ct_logs.log('Test loop info', 'Test run ~w of ~w', [c, n])

      {:value, {_, [{:stop_time, secs0, stopTime, n}]}} ->
        logStr1 =
          case :lists.keysearch(:duration, 1, args) do
            {:value, {_, dur}} ->
              :io_lib.format('Specified test duration: ~s (~w secs)\n', [delistify(dur), secs0])

            _ ->
              case :lists.keysearch(:until, 1, args) do
                {:value, {_, until}} ->
                  :io_lib.format('Specified end time: ~s (duration ~w secs)\n', [
                    delistify(until),
                    secs0
                  ])

                _ ->
                  :ok
              end
          end

        logStr2 = :io_lib.format('Test run #~w\n', [n])
        secs = remaining_time(stopTime)

        logStr3 =
          :io_lib.format(
            'Test time remaining: ~w secs (~w%)\n',
            [secs, trunc(secs / secs0 * 100)]
          )

        logStr4 =
          case :proplists.get_value(
                 :force_stop,
                 args
               ) do
            false__ when false__ == false or false__ == :undefined ->
              ''

            forceStop ->
              :io_lib.format('force_stop is set to: ~w', [forceStop])
          end

        :ct_logs.log('Test loop info', '~ts', [logStr1 ++ logStr2 ++ logStr3 ++ logStr4])
    end
  end

  defp ts(secs) do
    :erlang.integer_to_list(secs) ++ ' secs'
  end

  defp delistify([x]) do
    x
  end

  defp delistify(x) do
    x
  end
end
