defmodule :m_eunit_test do
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

  defp get_stacktrace(trace) do
    get_stacktrace(trace, [])
  end

  defp get_stacktrace(trace, ts) do
    :eunit_lib.uniq(prune_trace(trace, ts))
  end

  defp prune_trace([{:eunit_data, _, _} | rest], tail) do
    prune_trace(rest, tail)
  end

  defp prune_trace([{:eunit_data, _, _, _} | rest], tail) do
    prune_trace(rest, tail)
  end

  defp prune_trace([{:eunit_test, _, _} | _Rest], tail) do
    tail
  end

  defp prune_trace([{:eunit_test, _, _, _} | _Rest], tail) do
    tail
  end

  defp prune_trace([t | ts], tail) do
    [t | prune_trace(ts, tail)]
  end

  defp prune_trace([], tail) do
    tail
  end

  def run_testfun(f) do
    try do
      f.()
    catch
      {:eunit_internal, term} ->
        throw(term)

      class, reason ->
        {:error, {class, reason, __STACKTRACE__}}
    else
      value ->
        {:ok, value}
    end
  end

  defp macro_test_() do
    {'macro definitions',
     [
       {87,
        fn ->
          {88, f} =
            {88,
             fn ->
               :undefined
             end}

          {:ok, :undefined} = run_testfun(f)
        end},
       {91,
        fn ->
          {92, f} =
            {92,
             fn ->
               (fn ->
                  __T = :erlang.is_process_alive(self())

                  case true do
                    ^__T ->
                      :ok

                    __V ->
                      :erlang.error(
                        {:assert,
                         [
                           {:module, :eunit_test},
                           {:line, 92},
                           {:expression, 'true'},
                           {:expected, true},
                           case not __T do
                             ^__V ->
                               {:value, false}

                             _ ->
                               {:not_boolean, __V}
                           end
                         ]}
                      )
                  end
                end).()
             end}

          {:ok, :ok} = run_testfun(f)
        end},
       {95,
        fn ->
          {96, f} =
            {96,
             fn ->
               (fn ->
                  __T = :erlang.is_process_alive(self())

                  case false do
                    ^__T ->
                      :ok

                    __V ->
                      :erlang.error(
                        {:assert,
                         [
                           {:module, :eunit_test},
                           {:line, 96},
                           {:expression, 'false'},
                           {:expected, true},
                           case not __T do
                             ^__V ->
                               {:value, false}

                             _ ->
                               {:not_boolean, __V}
                           end
                         ]}
                      )
                  end
                end).()
             end}

          {:error,
           {:error,
            {:assertion_failed,
             [{:module, _}, {:line, _}, {:expression, _}, {:expected, true}, {:value, false}]},
            _}} = run_testfun(f)
        end},
       {106,
        fn ->
          {107, f} =
            {107,
             fn ->
               (fn ->
                  __T = :erlang.is_process_alive(self())

                  case [] do
                    ^__T ->
                      :ok

                    __V ->
                      :erlang.error(
                        {:assert,
                         [
                           {:module, :eunit_test},
                           {:line, 107},
                           {:expression, '[ ]'},
                           {:expected, true},
                           case not __T do
                             ^__V ->
                               {:value, false}

                             _ ->
                               {:not_boolean, __V}
                           end
                         ]}
                      )
                  end
                end).()
             end}

          {:error,
           {:error,
            {:assertion_failed,
             [
               {:module, _},
               {:line, _},
               {:expression, _},
               {:expected, true},
               {:value, {:not_a_boolean, []}}
             ]}, _}} = run_testfun(f)
        end},
       {117,
        fn ->
          {118, f} =
            {118,
             fn ->
               (fn ->
                  __T = :erlang.is_process_alive(self())

                  case not false do
                    ^__T ->
                      :ok

                    __V ->
                      :erlang.error(
                        {:assert,
                         [
                           {:module, :eunit_test},
                           {:line, 118},
                           {:expression, 'not ( false )'},
                           {:expected, true},
                           case not __T do
                             ^__V ->
                               {:value, false}

                             _ ->
                               {:not_boolean, __V}
                           end
                         ]}
                      )
                  end
                end).()
             end}

          {:ok, :ok} = run_testfun(f)
        end},
       {121,
        fn ->
          {122, f} =
            {122,
             fn ->
               (fn ->
                  __T = :erlang.is_process_alive(self())

                  case not true do
                    ^__T ->
                      :ok

                    __V ->
                      :erlang.error(
                        {:assert,
                         [
                           {:module, :eunit_test},
                           {:line, 122},
                           {:expression, 'not ( true )'},
                           {:expected, true},
                           case not __T do
                             ^__V ->
                               {:value, false}

                             _ ->
                               {:not_boolean, __V}
                           end
                         ]}
                      )
                  end
                end).()
             end}

          {:error,
           {:error,
            {:assertion_failed,
             [{:module, _}, {:line, _}, {:expression, _}, {:expected, true}, {:value, false}]},
            _}} = run_testfun(f)
        end},
       {132,
        fn ->
          {133, f} =
            {133,
             fn ->
               (fn ->
                  case :ok do
                    :ok ->
                      :ok

                    __V ->
                      :erlang.error(
                        {:assertMatch,
                         [
                           {:module, :eunit_test},
                           {:line, 133},
                           {:expression, 'ok'},
                           {:pattern, 'ok'},
                           {:value, __V}
                         ]}
                      )
                  end
                end).()
             end}

          {:ok, :ok} = run_testfun(f)
        end},
       {136,
        fn ->
          {137, f} =
            {137,
             fn ->
               (fn ->
                  case [] do
                    [_] ->
                      :ok

                    __V ->
                      :erlang.error(
                        {:assertMatch,
                         [
                           {:module, :eunit_test},
                           {:line, 137},
                           {:expression, '[ ]'},
                           {:pattern, '[ _ ]'},
                           {:value, __V}
                         ]}
                      )
                  end
                end).()
             end}

          {:error,
           {:error,
            {:assertMatch_failed,
             [{:module, _}, {:line, _}, {:expression, _}, {:pattern, '[ _ ]'}, {:value, []}]},
            _}} = run_testfun(f)
        end},
       {147,
        fn ->
          {148, f} =
            {148,
             fn ->
               (fn ->
                  __V = :error

                  case __V do
                    :ok ->
                      :erlang.error(
                        {:assertNotMatch,
                         [
                           {:module, :eunit_test},
                           {:line, 148},
                           {:expression, 'error'},
                           {:pattern, 'ok'},
                           {:value, __V}
                         ]}
                      )

                    _ ->
                      :ok
                  end
                end).()
             end}

          {:ok, :ok} = run_testfun(f)
        end},
       {151,
        fn ->
          {152, f} =
            {152,
             fn ->
               (fn ->
                  __V = [42]

                  case __V do
                    [_] ->
                      :erlang.error(
                        {:assertNotMatch,
                         [
                           {:module, :eunit_test},
                           {:line, 152},
                           {:expression, '[ 42 ]'},
                           {:pattern, '[ _ ]'},
                           {:value, __V}
                         ]}
                      )

                    _ ->
                      :ok
                  end
                end).()
             end}

          {:error,
           {:error,
            {:assertNotMatch_failed,
             [{:module, _}, {:line, _}, {:expression, _}, {:pattern, '[ _ ]'}, {:value, [42]}]},
            _}} = run_testfun(f)
        end},
       {162,
        fn ->
          {163, f} =
            {163,
             fn ->
               (fn ->
                  __X = :ok

                  case :ok do
                    ^__X ->
                      :ok

                    __V ->
                      :erlang.error(
                        {:assertEqual,
                         [
                           {:module, :eunit_test},
                           {:line, 163},
                           {:expression, 'ok'},
                           {:expected, __X},
                           {:value, __V}
                         ]}
                      )
                  end
                end).()
             end}

          {:ok, :ok} = run_testfun(f)
        end},
       {166,
        fn ->
          {167, f} =
            {167,
             fn ->
               (fn ->
                  __X = id(3)

                  case id(1 + 1) do
                    ^__X ->
                      :ok

                    __V ->
                      :erlang.error(
                        {:assertEqual,
                         [
                           {:module, :eunit_test},
                           {:line, 167},
                           {:expression, 'id ( 1 + 1 )'},
                           {:expected, __X},
                           {:value, __V}
                         ]}
                      )
                  end
                end).()
             end}

          {:error,
           {:error,
            {:assertEqual_failed,
             [{:module, _}, {:line, _}, {:expression, _}, {:expected, 3}, {:value, 2}]},
            _}} = run_testfun(f)
        end},
       {177,
        fn ->
          {178, f} =
            {178,
             fn ->
               (fn ->
                  __X = id(1)

                  case id(0) do
                    ^__X ->
                      :erlang.error(
                        {:assertNotEqual,
                         [
                           {:module, :eunit_test},
                           {:line, 178},
                           {:expression, 'id ( 0 )'},
                           {:value, __X}
                         ]}
                      )

                    _ ->
                      :ok
                  end
                end).()
             end}

          {:ok, :ok} = run_testfun(f)
        end},
       {181,
        fn ->
          {182, f} =
            {182,
             fn ->
               (fn ->
                  __X = 2

                  case 1 + 1 do
                    ^__X ->
                      :erlang.error(
                        {:assertNotEqual,
                         [
                           {:module, :eunit_test},
                           {:line, 182},
                           {:expression, '1 + 1'},
                           {:value, __X}
                         ]}
                      )

                    _ ->
                      :ok
                  end
                end).()
             end}

          {:error,
           {:error,
            {:assertNotEqual_failed, [{:module, _}, {:line, _}, {:expression, _}, {:value, 2}]},
            _}} = run_testfun(f)
        end},
       {191,
        fn ->
          {192, f} =
            {192,
             fn ->
               (fn ->
                  try do
                    :erlang.error(:badarith)
                  catch
                    :error, :badarith ->
                      :ok

                    __C, __T ->
                      :erlang.error(
                        {:assertException,
                         [
                           {:module, :eunit_test},
                           {:line, 192},
                           {:expression, 'erlang : error ( badarith )'},
                           {:pattern, '{ ' ++ 'error' ++ ' , ' ++ 'badarith' ++ ' , [...] }'},
                           {:unexpected_exception, {__C, __T, __STACKTRACE__}}
                         ]}
                      )
                  else
                    __V ->
                      :erlang.error(
                        {:assertException,
                         [
                           {:module, :eunit_test},
                           {:line, 193},
                           {:expression, 'erlang : error ( badarith )'},
                           {:pattern, '{ ' ++ 'error' ++ ' , ' ++ 'badarith' ++ ' , [...] }'},
                           {:unexpected_success, __V}
                         ]}
                      )
                  end
                end).()
             end}

          {:ok, :ok} = run_testfun(f)
        end},
       {196,
        fn ->
          {197, f} =
            {197,
             fn ->
               (fn ->
                  try do
                    :ok
                  catch
                    :error, :badarith ->
                      :ok

                    __C, __T ->
                      :erlang.error(
                        {:assertException,
                         [
                           {:module, :eunit_test},
                           {:line, 197},
                           {:expression, 'ok'},
                           {:pattern, '{ ' ++ 'error' ++ ' , ' ++ 'badarith' ++ ' , [...] }'},
                           {:unexpected_exception, {__C, __T, __STACKTRACE__}}
                         ]}
                      )
                  else
                    __V ->
                      :erlang.error(
                        {:assertException,
                         [
                           {:module, :eunit_test},
                           {:line, 197},
                           {:expression, 'ok'},
                           {:pattern, '{ ' ++ 'error' ++ ' , ' ++ 'badarith' ++ ' , [...] }'},
                           {:unexpected_success, __V}
                         ]}
                      )
                  end
                end).()
             end}

          {:error,
           {:error,
            {:assertException_failed,
             [
               {:module, _},
               {:line, _},
               {:expression, _},
               {:pattern, _},
               {:unexpected_success, :ok}
             ]}, _}} = run_testfun(f)
        end},
       {207,
        fn ->
          {208, f} =
            {208,
             fn ->
               (fn ->
                  try do
                    :erlang.error(:badarith)
                  catch
                    :error, :badarg ->
                      :ok

                    __C, __T ->
                      :erlang.error(
                        {:assertException,
                         [
                           {:module, :eunit_test},
                           {:line, 208},
                           {:expression, 'erlang : error ( badarith )'},
                           {:pattern, '{ ' ++ 'error' ++ ' , ' ++ 'badarg' ++ ' , [...] }'},
                           {:unexpected_exception, {__C, __T, __STACKTRACE__}}
                         ]}
                      )
                  else
                    __V ->
                      :erlang.error(
                        {:assertException,
                         [
                           {:module, :eunit_test},
                           {:line, 209},
                           {:expression, 'erlang : error ( badarith )'},
                           {:pattern, '{ ' ++ 'error' ++ ' , ' ++ 'badarg' ++ ' , [...] }'},
                           {:unexpected_success, __V}
                         ]}
                      )
                  end
                end).()
             end}

          {:error,
           {:error,
            {:assertException_failed,
             [
               {:module, _},
               {:line, _},
               {:expression, _},
               {:pattern, _},
               {:unexpected_exception, {:error, :badarith, _}}
             ]}, _}} = run_testfun(f)
        end},
       {220,
        fn ->
          {221, f} =
            {221,
             fn ->
               (fn ->
                  try do
                    :erlang.error(:badarith)
                  catch
                    :error, :badarith ->
                      :ok

                    __C, __T ->
                      :erlang.error(
                        {:assertException,
                         [
                           {:module, :eunit_test},
                           {:line, 221},
                           {:expression, 'erlang : error ( badarith )'},
                           {:pattern, '{ ' ++ 'error' ++ ' , ' ++ 'badarith' ++ ' , [...] }'},
                           {:unexpected_exception, {__C, __T, __STACKTRACE__}}
                         ]}
                      )
                  else
                    __V ->
                      :erlang.error(
                        {:assertException,
                         [
                           {:module, :eunit_test},
                           {:line, 222},
                           {:expression, 'erlang : error ( badarith )'},
                           {:pattern, '{ ' ++ 'error' ++ ' , ' ++ 'badarith' ++ ' , [...] }'},
                           {:unexpected_success, __V}
                         ]}
                      )
                  end
                end).()
             end}

          {:ok, :ok} = run_testfun(f)
        end},
       {225,
        fn ->
          {226, f} =
            {226,
             fn ->
               (fn ->
                  try do
                    exit(:normal)
                  catch
                    :exit, :normal ->
                      :ok

                    __C, __T ->
                      :erlang.error(
                        {:assertException,
                         [
                           {:module, :eunit_test},
                           {:line, 226},
                           {:expression, 'exit ( normal )'},
                           {:pattern, '{ ' ++ 'exit' ++ ' , ' ++ 'normal' ++ ' , [...] }'},
                           {:unexpected_exception, {__C, __T, __STACKTRACE__}}
                         ]}
                      )
                  else
                    __V ->
                      :erlang.error(
                        {:assertException,
                         [
                           {:module, :eunit_test},
                           {:line, 226},
                           {:expression, 'exit ( normal )'},
                           {:pattern, '{ ' ++ 'exit' ++ ' , ' ++ 'normal' ++ ' , [...] }'},
                           {:unexpected_success, __V}
                         ]}
                      )
                  end
                end).()
             end}

          {:ok, :ok} = run_testfun(f)
        end},
       {229,
        fn ->
          {230, f} =
            {230,
             fn ->
               (fn ->
                  try do
                    throw(:foo)
                  catch
                    :foo ->
                      :ok

                    __C, __T ->
                      :erlang.error(
                        {:assertException,
                         [
                           {:module, :eunit_test},
                           {:line, 230},
                           {:expression, 'throw ( foo )'},
                           {:pattern, '{ ' ++ 'throw' ++ ' , ' ++ 'foo' ++ ' , [...] }'},
                           {:unexpected_exception, {__C, __T, __STACKTRACE__}}
                         ]}
                      )
                  else
                    __V ->
                      :erlang.error(
                        {:assertException,
                         [
                           {:module, :eunit_test},
                           {:line, 230},
                           {:expression, 'throw ( foo )'},
                           {:pattern, '{ ' ++ 'throw' ++ ' , ' ++ 'foo' ++ ' , [...] }'},
                           {:unexpected_success, __V}
                         ]}
                      )
                  end
                end).()
             end}

          {:ok, :ok} = run_testfun(f)
        end},
       {233,
        fn ->
          {234, f} =
            {234,
             fn ->
               (fn ->
                  try do
                    42
                  catch
                    __C, __T ->
                      case __C do
                        :error ->
                          case __T do
                            :badarith ->
                              :erlang.error(
                                {:assertNotException,
                                 [
                                   {:module, :eunit_test},
                                   {:line, 234},
                                   {:expression, '42'},
                                   {:pattern,
                                    '{ ' ++ 'error' ++ ' , ' ++ 'badarith' ++ ' , [...] }'},
                                   {:unexpected_exception, {__C, __T, __STACKTRACE__}}
                                 ]}
                              )

                            _ ->
                              :ok
                          end

                        _ ->
                          :ok
                      end
                  else
                    _ ->
                      :ok
                  end
                end).()
             end}

          {:ok, :ok} = run_testfun(f)
        end},
       {237,
        fn ->
          {238, f} =
            {238,
             fn ->
               (fn ->
                  try do
                    :erlang.error(:badarg)
                  catch
                    __C, __T ->
                      case __C do
                        :error ->
                          case __T do
                            :badarith ->
                              :erlang.error(
                                {:assertNotException,
                                 [
                                   {:module, :eunit_test},
                                   {:line, 238},
                                   {:expression, 'erlang : error ( badarg )'},
                                   {:pattern,
                                    '{ ' ++ 'error' ++ ' , ' ++ 'badarith' ++ ' , [...] }'},
                                   {:unexpected_exception, {__C, __T, __STACKTRACE__}}
                                 ]}
                              )

                            _ ->
                              :ok
                          end

                        _ ->
                          :ok
                      end
                  else
                    _ ->
                      :ok
                  end
                end).()
             end}

          {:ok, :ok} = run_testfun(f)
        end},
       {242,
        fn ->
          {243, f} =
            {243,
             fn ->
               (fn ->
                  try do
                    :erlang.error(:badarith)
                  catch
                    __C, __T ->
                      case __C do
                        :error ->
                          case __T do
                            :badarith ->
                              :erlang.error(
                                {:assertNotException,
                                 [
                                   {:module, :eunit_test},
                                   {:line, 243},
                                   {:expression, 'erlang : error ( badarith )'},
                                   {:pattern,
                                    '{ ' ++ 'error' ++ ' , ' ++ 'badarith' ++ ' , [...] }'},
                                   {:unexpected_exception, {__C, __T, __STACKTRACE__}}
                                 ]}
                              )

                            _ ->
                              :ok
                          end

                        _ ->
                          :ok
                      end
                  else
                    _ ->
                      :ok
                  end
                end).()
             end}

          {:error,
           {:error,
            {:assertNotException_failed,
             [
               {:module, _},
               {:line, _},
               {:expression, _},
               {:pattern, _},
               {:unexpected_exception, {:error, :badarith, _}}
             ]}, _}} = run_testfun(f)
        end}
     ]}
  end

  def mf_wrapper(m, f) do
    fn ->
      try do
        apply(m, f, [])
      catch
        :error, :undef ->
          case :erlang.module_loaded(m) do
            false ->
              fail({:module_not_found, m})

            true ->
              case :erlang.function_exported(m, f, 0) do
                false ->
                  fail({:no_such_function, {m, f, 0}})

                true ->
                  rethrow(:error, :undef, __STACKTRACE__, [{m, f, 0}])
              end
          end
      end
    end
  end

  defp rethrow(class, reason, trace, ts) do
    :erlang.raise(class, reason, get_stacktrace(trace, ts))
  end

  defp fail(term) do
    throw({:eunit_internal, term})
  end

  defp wrapper_test_() do
    {'error handling in function wrapper',
     [
       {301,
        fn ->
          (fn ->
             try do
               run_testfun(mf_wrapper(:eunit_nonexisting, :test))
             catch
               {:module_not_found, :eunit_nonexisting} ->
                 :ok

               __C, __T ->
                 :erlang.error(
                   {:assertException,
                    [
                      {:module, :eunit_test},
                      {:line, 301},
                      {:expression, 'run_testfun ( mf_wrapper ( eunit_nonexisting , test ) )'},
                      {:pattern,
                       '{ ' ++
                         'throw' ++
                         ' , ' ++ '{ module_not_found , eunit_nonexisting }' ++ ' , [...] }'},
                      {:unexpected_exception, {__C, __T, __STACKTRACE__}}
                    ]}
                 )
             else
               __V ->
                 :erlang.error(
                   {:assertException,
                    [
                      {:module, :eunit_test},
                      {:line, 302},
                      {:expression, 'run_testfun ( mf_wrapper ( eunit_nonexisting , test ) )'},
                      {:pattern,
                       '{ ' ++
                         'throw' ++
                         ' , ' ++ '{ module_not_found , eunit_nonexisting }' ++ ' , [...] }'},
                      {:unexpected_success, __V}
                    ]}
                 )
             end
           end).()
        end},
       {303,
        fn ->
          (fn ->
             try do
               run_testfun(
                 mf_wrapper(
                   :eunit_test,
                   :nonexisting_test
                 )
               )
             catch
               {:no_such_function, {:eunit_test, :nonexisting_test, 0}} ->
                 :ok

               __C, __T ->
                 :erlang.error(
                   {:assertException,
                    [
                      {:module, :eunit_test},
                      {:line, 304},
                      {:expression, 'run_testfun ( mf_wrapper ( ? MODULE , nonexisting_test ) )'},
                      {:pattern,
                       '{ ' ++
                         'throw' ++
                         ' , ' ++
                         '{ no_such_function , { ? MODULE , nonexisting_test , 0 } }' ++
                         ' , [...] }'},
                      {:unexpected_exception, {__C, __T, __STACKTRACE__}}
                    ]}
                 )
             else
               __V ->
                 :erlang.error(
                   {:assertException,
                    [
                      {:module, :eunit_test},
                      {:line, 305},
                      {:expression, 'run_testfun ( mf_wrapper ( ? MODULE , nonexisting_test ) )'},
                      {:pattern,
                       '{ ' ++
                         'throw' ++
                         ' , ' ++
                         '{ no_such_function , { ? MODULE , nonexisting_test , 0 } }' ++
                         ' , [...] }'},
                      {:unexpected_success, __V}
                    ]}
                 )
             end
           end).()
        end},
       {306,
        fn ->
          {:error, {:error, :undef, _T}} =
            run_testfun(
              mf_wrapper(
                :eunit_test,
                :wrapper_test_exported_
              )
            )
        end}
     ]}
  end

  defp wrapper_test_exported_() do
    {:ok, :eunit_test.nonexisting_function()}
  end

  def enter_context(setup, cleanup, instantiate, callback) do
    try do
      setup.()
    catch
      class, term ->
        context_error(:setup_failed, class, __STACKTRACE__, term)
    else
      r ->
        try do
          instantiate.(r)
        catch
          class, term ->
            context_error(:instantiation_failed, class, __STACKTRACE__, term)
        else
          t ->
            case :eunit_lib.is_not_test(t) do
              true ->
                {_, stacktrace} =
                  :erlang.process_info(
                    self(),
                    :current_stacktrace
                  )

                {:module, m} = :erlang.fun_info(instantiate, :module)
                {:name, n} = :erlang.fun_info(instantiate, :name)
                {:arity, a} = :erlang.fun_info(instantiate, :arity)
                context_error({:bad_instantiator, {{m, n, a}, t}}, :error, stacktrace, :badarg)

              false ->
                :ok
            end

            try do
              callback.(t)
            after
              try do
                cleanup.(r)
              catch
                class, term ->
                  context_error(:cleanup_failed, class, __STACKTRACE__, term)
              end
            end
        end
    end
  end

  defp context_error(type, class, trace, term) do
    throw({:context_error, type, {class, term, get_stacktrace(trace)}})
  end

  def multi_setup(list) do
    {setupAll, cleanupAll} = multi_setup(list, &ok/1)

    {fn ->
       :lists.reverse(setupAll.([]))
     end,
     fn rs ->
       cleanupAll.(:lists.reverse(rs))
     end}
  end

  defp multi_setup([{tag, s, c} | es], cleanupPrev) do
    cleanup = fn [r | rs] ->
      try do
        c.(r)
      catch
        class, term ->
          throw({tag, {class, term, __STACKTRACE__}})
      else
        _ ->
          cleanupPrev.(rs)
      end
    end

    {setupRest, cleanupAll} = multi_setup(es, cleanup)

    {fn rs ->
       try do
         s.()
       catch
         class, term ->
           cleanupPrev.(rs)
           throw({tag, {class, term, __STACKTRACE__}})
       else
         r ->
           setupRest.([r | rs])
       end
     end, cleanupAll}
  end

  defp multi_setup([{tag, s} | es], cleanupPrev) do
    multi_setup([{tag, s, &ok/1} | es], cleanupPrev)
  end

  defp multi_setup([], cleanupAll) do
    {fn rs ->
       rs
     end, cleanupAll}
  end

  defp id(i) do
    i
  end

  defp ok(_) do
    :ok
  end
end
