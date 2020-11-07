defmodule :m_eunit_lib do
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

  def format_exception(exception) do
    format_exception(exception, 20)
  end

  def format_exception({class, term, trace}, depth)
      when is_atom(class) and is_list(trace) do
    case is_stacktrace(trace) do
      true ->
        :io_lib.format(
          '~ts**~w:~ts',
          [format_stacktrace(trace), class, format_term(term, depth)]
        )

      false ->
        format_term(term, depth)
    end
  end

  def format_exception(term, depth) do
    format_term(term, depth)
  end

  defp format_term(term, depth) do
    :io_lib.format('~tP\n', [term, depth])
  end

  def format_exit_term(term) do
    {reason, trace} = analyze_exit_term(term)
    :io_lib.format('~tP~ts', [reason, 15, trace])
  end

  defp analyze_exit_term({reason, [_ | _] = trace} = term) do
    case is_stacktrace(trace) do
      true ->
        {reason, format_stacktrace(trace)}

      false ->
        {term, ''}
    end
  end

  defp analyze_exit_term(term) do
    {term, ''}
  end

  defp is_stacktrace([]) do
    true
  end

  defp is_stacktrace([{m, f, a, l} | fs])
       when is_atom(m) and
              is_atom(f) and is_integer(a) and
              is_list(l) do
    is_stacktrace(fs)
  end

  defp is_stacktrace([{m, f, as, l} | fs])
       when is_atom(m) and
              is_atom(f) and is_list(as) and
              is_list(l) do
    is_stacktrace(fs)
  end

  defp is_stacktrace([{m, f, a} | fs])
       when is_atom(m) and
              is_atom(f) and is_integer(a) do
    is_stacktrace(fs)
  end

  defp is_stacktrace([{m, f, as} | fs])
       when is_atom(m) and
              is_atom(f) and is_list(as) do
    is_stacktrace(fs)
  end

  defp is_stacktrace(_) do
    false
  end

  defp format_stacktrace(trace) do
    format_stacktrace(trace, 'in function', 'in call from')
  end

  defp format_stacktrace([{m, f, a, l} | fs], pre, pre1)
       when is_integer(a) do
    [
      :io_lib.fwrite(
        '~ts ~w:~tw/~w~ts\n',
        [pre, m, f, a, format_stacktrace_location(l)]
      )
      | format_stacktrace(fs, pre1, pre1)
    ]
  end

  defp format_stacktrace([{m, f, as, l} | fs], pre, pre1)
       when is_list(as) do
    a = length(as)

    c =
      case is_op(m, f, a) do
        true when a === 1 ->
          [a1] = as
          :io_lib.fwrite('~ts ~ts', [f, format_arg(a1)])

        true when a === 2 ->
          [a1, a2] = as
          :io_lib.fwrite('~ts ~ts ~ts', [format_arg(a1), f, format_arg(a2)])

        false ->
          :io_lib.fwrite('~tw(~ts)', [f, format_arglist(as)])
      end

    [
      :io_lib.fwrite(
        '~ts ~w:~tw/~w~ts\n  called as ~ts\n',
        [pre, m, f, a, format_stacktrace_location(l), c]
      )
      | format_stacktrace(fs, pre1, pre1)
    ]
  end

  defp format_stacktrace([{m, f, as} | fs], pre, pre1) do
    format_stacktrace([{m, f, as, []} | fs], pre, pre1)
  end

  defp format_stacktrace([], _Pre, _Pre1) do
    ''
  end

  defp format_stacktrace_location(location) do
    file = :proplists.get_value(:file, location)
    line = :proplists.get_value(:line, location)

    cond do
      file !== :undefined and line !== :undefined ->
        :io_lib.format(' (~ts, line ~w)', [file, line])

      true ->
        ''
    end
  end

  defp format_arg(a) do
    :io_lib.format('~tP', [a, 15])
  end

  defp format_arglist([a]) do
    format_arg(a)
  end

  defp format_arglist([a | as]) do
    [:io_lib.format('~tP,', [a, 15]) | format_arglist(as)]
  end

  defp format_arglist([]) do
    ''
  end

  defp is_op(:erlang, f, a) do
    :erl_internal.arith_op(f, a) or
      :erl_internal.bool_op(
        f,
        a
      ) or
      :erl_internal.comp_op(
        f,
        a
      ) or
      :erl_internal.list_op(
        f,
        a
      ) or
      :erl_internal.send_op(
        f,
        a
      )
  end

  defp is_op(_M, _F, _A) do
    false
  end

  def format_error(error) do
    format_error(error, 20)
  end

  def format_error({:bad_test, term}, depth) do
    error_msg('bad test descriptor', '~tP', [term, depth])
  end

  def format_error({:bad_generator, {{m, f, a}, term}}, depth) do
    error_msg(:io_lib.format('result from generator ~w:~tw/~w is not a test', [m, f, a]), '~tP', [
      term,
      depth
    ])
  end

  def format_error(
        {:generator_failed, {{m, f, a}, exception}},
        depth
      ) do
    error_msg(:io_lib.format('test generator ~w:~tw/~w failed', [m, f, a]), '~ts', [
      format_exception(exception, depth)
    ])
  end

  def format_error({:no_such_function, {m, f, a}}, _)
      when is_atom(m) and is_atom(f) and is_integer(a) do
    error_msg(:io_lib.format('no such function: ~w:~tw/~w', [m, f, a]), '', [])
  end

  def format_error({:module_not_found, m}, _) do
    error_msg('test module not found', '~tp', [m])
  end

  def format_error({:application_not_found, a}, _)
      when is_atom(a) do
    error_msg('application not found', '~w', [a])
  end

  def format_error({:file_read_error, {_R, msg, f}}, _) do
    error_msg('error reading file', '~ts: ~ts', [msg, f])
  end

  def format_error({:setup_failed, exception}, depth) do
    error_msg('context setup failed', '~ts', [format_exception(exception, depth)])
  end

  def format_error({:cleanup_failed, exception}, depth) do
    error_msg('context cleanup failed', '~ts', [format_exception(exception, depth)])
  end

  def format_error(
        {{:bad_instantiator, {{m, f, a}, term}}, _DummyException},
        depth
      ) do
    error_msg(
      :io_lib.format('result from instantiator ~w:~tw/~w is not a test', [m, f, a]),
      '~tP',
      [term, depth]
    )
  end

  def format_error({:instantiation_failed, exception}, depth) do
    error_msg('instantiation of subtests failed', '~ts', [format_exception(exception, depth)])
  end

  defp error_msg(title, fmt, args) do
    msg = :io_lib.format('**' ++ fmt, args)
    :io_lib.fwrite('*** ~ts ***\n~ts\n\n', [title, msg])
  end

  defp format_exception_test_() do
    [
      {205,
       fn ->
         (fn ->
            case :lists.reverse(
                   :lists.flatten(
                     format_exception(
                       try do
                         :erlang.error(:dummy)
                       catch
                         c, r ->
                           {c, r, __STACKTRACE__}
                       end
                     )
                   )
                 ) do
              '\nymmud:rorre' ++ _ ->
                :ok

              __V ->
                :erlang.error(
                  {:assertMatch,
                   [
                     {:module, :eunit_lib},
                     {:line, 206},
                     {:expression,
                      'lists : reverse ( lists : flatten ( format_exception ( try erlang : error ( dummy ) catch C : R : S -> { C , R , S } end ) ) )'},
                     {:pattern, '"\\nymmud:rorre" ++ _'},
                     {:value, __V}
                   ]}
                )
            end
          end).()
       end},
      {211,
       fn ->
         (fn ->
            case :lists.reverse(
                   :lists.flatten(
                     format_exception(
                       try do
                         :erlang.error(
                           :dummy,
                           [:a]
                         )
                       catch
                         c, r ->
                           {c, r, __STACKTRACE__}
                       end
                     )
                   )
                 ) do
              '\nymmud:rorre' ++ _ ->
                :ok

              __V ->
                :erlang.error(
                  {:assertMatch,
                   [
                     {:module, :eunit_lib},
                     {:line, 212},
                     {:expression,
                      'lists : reverse ( lists : flatten ( format_exception ( try erlang : error ( dummy , [ a ] ) catch C : R : S -> { C , R , S } end ) ) )'},
                     {:pattern, '"\\nymmud:rorre" ++ _'},
                     {:value, __V}
                   ]}
                )
            end
          end).()
       end}
    ]
  end

  def is_not_test(t) do
    case t do
      :ok ->
        true

      :error ->
        true

      true ->
        true

      false ->
        true

      :undefined ->
        true

      {:ok, _} ->
        true

      {:error, _} ->
        true

      {:EXIT, _} ->
        true

      n when is_number(n) ->
        true

      [n | _] when is_number(n) ->
        true

      x when is_binary(x) ->
        true

      x when is_pid(x) ->
        true

      x when is_port(x) ->
        true

      x when is_reference(x) ->
        true

      _ ->
        false
    end
  end

  def dlist_next([x | xs] = xs0) when is_list(x) do
    case is_nonempty_string(x) do
      true ->
        xs0

      false ->
        dlist_next(x, xs)
    end
  end

  def dlist_next([_ | _] = xs) do
    case is_nonempty_string(xs) do
      true ->
        [xs]

      false ->
        xs
    end
  end

  def dlist_next([]) do
    []
  end

  def dlist_next(x) do
    [x]
  end

  defp dlist_next([x], ys) when is_list(x) do
    case is_nonempty_string(x) do
      true ->
        [x | ys]

      false ->
        dlist_next(x, ys)
    end
  end

  defp dlist_next([x], ys) do
    [x | ys]
  end

  defp dlist_next([x | xs], ys) when is_list(x) do
    case is_nonempty_string(x) do
      true ->
        [[x, xs] | ys]

      false ->
        dlist_next(x, [xs | ys])
    end
  end

  defp dlist_next([x | xs], ys) do
    [[x, xs] | ys]
  end

  defp dlist_next([], xs) do
    dlist_next(xs)
  end

  defp dlist_test_() do
    {'deep list traversal',
     [
       {'non-list term -> singleton list',
        {288,
         fn ->
           [:any] = dlist_next(:any)
         end}},
       {'empty list -> empty list',
        {290,
         fn ->
           [] = dlist_next([])
         end}},
       {'singleton list -> singleton list',
        {292,
         fn ->
           [:any] = dlist_next([:any])
         end}},
       {'taking the head of a flat list',
        {294,
         fn ->
           [:a, :b, :c] = dlist_next([:a, :b, :c])
         end}},
       {'skipping an initial empty list',
        {296,
         fn ->
           [:a, :b, :c] = dlist_next([[], :a, :b, :c])
         end}},
       {'skipping nested initial empty lists',
        {298,
         fn ->
           [:a, :b, :c] = dlist_next([[[[]]], :a, :b, :c])
         end}},
       {'skipping a final empty list',
        {300,
         fn ->
           [] = dlist_next([[]])
         end}},
       {'skipping nested final empty lists',
        {302,
         fn ->
           [] = dlist_next([[[[]]]])
         end}},
       {'the first element is in a sublist',
        {304,
         fn ->
           [:a, :b, :c] = dlist_next([[:a], :b, :c])
         end}},
       {'recognizing a naked string',
        {306,
         fn ->
           ['abc'] = dlist_next('abc')
         end}},
       {'recognizing a wrapped string',
        {308,
         fn ->
           ['abc'] = dlist_next(['abc'])
         end}},
       {'recognizing a leading string',
        {310,
         fn ->
           ['abc', :a, :b, :c] = dlist_next(['abc', :a, :b, :c])
         end}},
       {'recognizing a nested string',
        {312,
         fn ->
           ['abc'] = dlist_next([['abc']])
         end}},
       {'recognizing a leading string in a sublist',
        {314,
         fn ->
           ['abc', :a, :b, :c] = dlist_next([['abc'], :a, :b, :c])
         end}},
       {'traversing an empty list',
        {316,
         fn ->
           [] = dlist_flatten([])
         end}},
       {'traversing a flat list',
        {318,
         fn ->
           [:a, :b, :c] = dlist_flatten([:a, :b, :c])
         end}},
       {'traversing a deep list',
        {320,
         fn ->
           [:a, :b, :c] = dlist_flatten([[], [:a, [:b, []], :c], []])
         end}},
       {'traversing a deep but empty list',
        {322,
         fn ->
           [] = dlist_flatten([[], [[[]]], []])
         end}}
     ]}
  end

  defp dlist_flatten(xs) do
    case dlist_next(xs) do
      [x | xs1] ->
        [x | dlist_flatten(xs1)]

      [] ->
        []
    end
  end

  def is_string([c | cs])
      when is_integer(c) and c >= 0 and
             c <= 1_114_111 do
    is_string(cs)
  end

  def is_string([_ | _]) do
    false
  end

  def is_string([]) do
    true
  end

  def is_string(_) do
    false
  end

  defp is_nonempty_string([]) do
    false
  end

  defp is_nonempty_string(cs) do
    is_string(cs)
  end

  defp is_string_test_() do
    {'is_string',
     [
       {'no non-lists',
        {352,
         fn ->
           (fn ->
              __T = :erlang.is_process_alive(self())

              case not is_string(?A) do
                ^__T ->
                  :ok

                __V ->
                  :erlang.error(
                    {:assert,
                     [
                       {:module, :eunit_lib},
                       {:line, 352},
                       {:expression, 'not is_string ( $A )'},
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
         end}},
       {'no non-integer lists',
        {353,
         fn ->
           (fn ->
              __T = :erlang.is_process_alive(self())

              case not is_string([true]) do
                ^__T ->
                  :ok

                __V ->
                  :erlang.error(
                    {:assert,
                     [
                       {:module, :eunit_lib},
                       {:line, 353},
                       {:expression, 'not is_string ( [ true ] )'},
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
         end}},
       {'empty string',
        {354,
         fn ->
           (fn ->
              __T = :erlang.is_process_alive(self())

              case is_string('') do
                ^__T ->
                  :ok

                __V ->
                  :erlang.error(
                    {:assert,
                     [
                       {:module, :eunit_lib},
                       {:line, 354},
                       {:expression, 'is_string ( "" )'},
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
         end}},
       {'ascii string',
        {355,
         fn ->
           (fn ->
              __T = :erlang.is_process_alive(self())

              case is_string(:lists.seq(0, 127)) do
                ^__T ->
                  :ok

                __V ->
                  :erlang.error(
                    {:assert,
                     [
                       {:module, :eunit_lib},
                       {:line, 355},
                       {:expression, 'is_string ( lists : seq ( 0 , 127 ) )'},
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
         end}},
       {'latin-1 string',
        {356,
         fn ->
           (fn ->
              __T = :erlang.is_process_alive(self())

              case is_string(:lists.seq(0, 255)) do
                ^__T ->
                  :ok

                __V ->
                  :erlang.error(
                    {:assert,
                     [
                       {:module, :eunit_lib},
                       {:line, 356},
                       {:expression, 'is_string ( lists : seq ( 0 , 255 ) )'},
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
         end}},
       {'unicode string',
        {358,
         fn ->
           (fn ->
              __T = :erlang.is_process_alive(self())

              case is_string([0, ?A, 1_114_110, 1_114_111]) do
                ^__T ->
                  :ok

                __V ->
                  :erlang.error(
                    {:assert,
                     [
                       {:module, :eunit_lib},
                       {:line, 358},
                       {:expression, 'is_string ( [ 0 , $A , 1114110 , 1114111 ] )'},
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
         end}},
       {'not above unicode range',
        {360,
         fn ->
           (fn ->
              __T = :erlang.is_process_alive(self())

              case not is_string([0, ?A, 1_114_112]) do
                ^__T ->
                  :ok

                __V ->
                  :erlang.error(
                    {:assert,
                     [
                       {:module, :eunit_lib},
                       {:line, 360},
                       {:expression, 'not is_string ( [ 0 , $A , 1114112 ] )'},
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
         end}},
       {'no negative codepoints',
        {361,
         fn ->
           (fn ->
              __T = :erlang.is_process_alive(self())

              case not is_string([?A, -1, 0]) do
                ^__T ->
                  :ok

                __V ->
                  :erlang.error(
                    {:assert,
                     [
                       {:module, :eunit_lib},
                       {:line, 361},
                       {:expression, 'not is_string ( [ $A , - 1 , 0 ] )'},
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
         end}}
     ]}
  end

  def split_node(n) when is_atom(n) do
    split_node(:erlang.atom_to_list(n))
  end

  def split_node(cs) do
    split_node_1(cs, [])
  end

  defp split_node_1([?@ | cs], as) do
    split_node_2(as, cs)
  end

  defp split_node_1([c | cs], as) do
    split_node_1(cs, [c | as])
  end

  defp split_node_1([], as) do
    split_node_2(as, 'localhost')
  end

  defp split_node_2(as, cs) do
    {:erlang.list_to_atom(:lists.reverse(as)), :erlang.list_to_atom(cs)}
  end

  def fun_parent(f) do
    {:module, m} = :erlang.fun_info(f, :module)
    {:name, n} = :erlang.fun_info(f, :name)

    case :erlang.fun_info(f, :type) do
      {:type, :external} ->
        {:arity, a} = :erlang.fun_info(f, :arity)
        {m, n, a}

      {:type, :local} ->
        [?- | s] = :erlang.atom_to_list(n)
        [s2, t] = :string.split(s, '/', :trailing)
        {m, :erlang.list_to_atom(s2), :erlang.element(1, :string.to_integer(t))}
    end
  end

  defp fun_parent_test() do
    {:eunit_lib, :fun_parent_test, 0} =
      fun_parent(fn a ->
        {:ok, a}
      end)
  end

  def uniq([[x, x] | xs]) do
    uniq([x | xs])
  end

  def uniq([x | xs]) do
    [x | uniq(xs)]
  end

  def uniq([]) do
    []
  end

  defp uniq_test_() do
    {'uniq',
     [
       {412,
        fn ->
          (fn ->
             try do
               uniq(:ok)
             catch
               :error, :function_clause ->
                 :ok

               __C, __T ->
                 :erlang.error(
                   {:assertException,
                    [
                      {:module, :eunit_lib},
                      {:line, 412},
                      {:expression, 'uniq ( ok )'},
                      {:pattern, '{ ' ++ 'error' ++ ' , ' ++ 'function_clause' ++ ' , [...] }'},
                      {:unexpected_exception, {__C, __T, __STACKTRACE__}}
                    ]}
                 )
             else
               __V ->
                 :erlang.error(
                   {:assertException,
                    [
                      {:module, :eunit_lib},
                      {:line, 412},
                      {:expression, 'uniq ( ok )'},
                      {:pattern, '{ ' ++ 'error' ++ ' , ' ++ 'function_clause' ++ ' , [...] }'},
                      {:unexpected_success, __V}
                    ]}
                 )
             end
           end).()
        end},
       {413,
        fn ->
          (fn ->
             try do
               uniq([1 | 2])
             catch
               :error, :function_clause ->
                 :ok

               __C, __T ->
                 :erlang.error(
                   {:assertException,
                    [
                      {:module, :eunit_lib},
                      {:line, 413},
                      {:expression, 'uniq ( [ 1 | 2 ] )'},
                      {:pattern, '{ ' ++ 'error' ++ ' , ' ++ 'function_clause' ++ ' , [...] }'},
                      {:unexpected_exception, {__C, __T, __STACKTRACE__}}
                    ]}
                 )
             else
               __V ->
                 :erlang.error(
                   {:assertException,
                    [
                      {:module, :eunit_lib},
                      {:line, 413},
                      {:expression, 'uniq ( [ 1 | 2 ] )'},
                      {:pattern, '{ ' ++ 'error' ++ ' , ' ++ 'function_clause' ++ ' , [...] }'},
                      {:unexpected_success, __V}
                    ]}
                 )
             end
           end).()
        end},
       {414,
        fn ->
          [] = uniq([])
        end},
       {415,
        fn ->
          [1, 2, 3] = uniq([1, 2, 3])
        end},
       {416,
        fn ->
          [1, 2, 3] = uniq([1, 2, 2, 3])
        end},
       {417,
        fn ->
          [1, 2, 3, 2, 1] = uniq([1, 2, 2, 3, 2, 2, 1])
        end},
       {418,
        fn ->
          [1, 2, 3] = uniq([1, 1, 1, 2, 2, 2, 3, 3, 3])
        end},
       {419,
        fn ->
          ['1', '2', '3'] = uniq(['1', '1', '2', '2', '3', '3'])
        end}
     ]}
  end

  def command(cmd) do
    command(cmd, '')
  end

  def command(cmd, dir) do
    command(cmd, dir, [])
  end

  def command(cmd, dir, env) do
    cD =
      cond do
        dir === '' ->
          []

        true ->
          [{:cd, dir}]
      end

    setEnv =
      cond do
        env === [] ->
          []

        true ->
          [{:env, env}]
      end

    opt = cD ++ setEnv ++ [:stream, :exit_status, :use_stdio, :stderr_to_stdout, :in, :eof]
    p = :erlang.open_port({:spawn, cmd}, opt)
    get_data(p, [])
  end

  defp get_data(p, d) do
    receive do
      {^p, {:data, d1}} ->
        get_data(p, [d1 | d])

      {^p, :eof} ->
        :erlang.port_close(p)

        receive do
          {^p, {:exit_status, n}} ->
            {n, normalize(:lists.flatten(:lists.reverse(d)))}
        end
    end
  end

  defp normalize([[?\r, ?\n] | cs]) do
    [?\n | normalize(cs)]
  end

  defp normalize([?\r | cs]) do
    [?\n | normalize(cs)]
  end

  defp normalize([c | cs]) do
    [c | normalize(cs)]
  end

  defp normalize([]) do
    []
  end

  defp cmd_test_() do
    [
      {472,
       fn ->
         {0, 'hello\n'} = :eunit_lib.command('echo hello')
       end}
    ] ++
      case :os.type() do
        {:unix, _} ->
          unix_cmd_tests()

        {:win32, _} ->
          win32_cmd_tests()

        _ ->
          []
      end
  end

  defp unix_cmd_tests() do
    [
      {'command execution, status, and output',
       [
         {484,
          fn ->
            (fn ->
               case :eunit_lib.command('echo hello') do
                 {0, __Out} ->
                   __Out

                 {__N, _} ->
                   :erlang.error(
                     {:command_failed,
                      [
                        {:module, :eunit_lib},
                        {:line, 484},
                        {:command, 'echo hello'},
                        {:expected_status, 0},
                        {:status, __N}
                      ]}
                   )
               end
             end).()
          end},
         {485,
          fn ->
            (fn ->
               case :eunit_lib.command('true') do
                 {0, _} ->
                   :ok

                 {__N, _} ->
                   :erlang.error(
                     {:assertCmd_failed,
                      [
                        {:module, :eunit_lib},
                        {:line, 485},
                        {:command, 'true'},
                        {:expected_status, 0},
                        {:status, __N}
                      ]}
                   )
               end
             end).()
          end},
         {486,
          fn ->
            (fn ->
               case :eunit_lib.command('false') do
                 {1, _} ->
                   :ok

                 {__N, _} ->
                   :erlang.error(
                     {:assertCmd_failed,
                      [
                        {:module, :eunit_lib},
                        {:line, 486},
                        {:command, 'false'},
                        {:expected_status, 1},
                        {:status, __N}
                      ]}
                   )
               end
             end).()
          end},
         {487,
          fn ->
            (fn ->
               case :eunit_lib.command('true') do
                 {0, _} ->
                   :ok

                 {__N, _} ->
                   :erlang.error(
                     {:assertCmd_failed,
                      [
                        {:module, :eunit_lib},
                        {:line, 487},
                        {:command, 'true'},
                        {:expected_status, 0},
                        {:status, __N}
                      ]}
                   )
               end
             end).()
          end},
         {488,
          fn ->
            (fn ->
               case :eunit_lib.command('echo hello') do
                 {_, 'hello\n'} ->
                   :ok

                 {_, __T} ->
                   :erlang.error(
                     {:assertCmdOutput_failed,
                      [
                        {:module, :eunit_lib},
                        {:line, 488},
                        {:command, 'echo hello'},
                        {:expected_output, 'hello\n'},
                        {:output, __T}
                      ]}
                   )
               end
             end).()
          end},
         {489,
          fn ->
            (fn ->
               case :eunit_lib.command('echo -n hello') do
                 {_, 'hello'} ->
                   :ok

                 {_, __T} ->
                   :erlang.error(
                     {:assertCmdOutput_failed,
                      [
                        {:module, :eunit_lib},
                        {:line, 489},
                        {:command, 'echo -n hello'},
                        {:expected_output, 'hello'},
                        {:output, __T}
                      ]}
                   )
               end
             end).()
          end}
       ]},
      {'file setup and cleanup', :setup,
       fn ->
         (fn ->
            case :eunit_lib.command('mktemp tmp.XXXXXXXX') do
              {0, __Out} ->
                __Out

              {__N, _} ->
                :erlang.error(
                  {:command_failed,
                   [
                     {:module, :eunit_lib},
                     {:line, 493},
                     {:command, 'mktemp tmp.XXXXXXXX'},
                     {:expected_status, 0},
                     {:status, __N}
                   ]}
                )
            end
          end).()
       end,
       fn file ->
         (fn ->
            case :eunit_lib.command('rm ' ++ file) do
              {0, __Out} ->
                __Out

              {__N, _} ->
                :erlang.error(
                  {:command_failed,
                   [
                     {:module, :eunit_lib},
                     {:line, 494},
                     {:command, 'rm ' ++ file},
                     {:expected_status, 0},
                     {:status, __N}
                   ]}
                )
            end
          end).()
       end,
       fn file ->
         [
           {496,
            fn ->
              (fn ->
                 case :eunit_lib.command('echo xyzzy >' ++ file) do
                   {0, _} ->
                     :ok

                   {__N, _} ->
                     :erlang.error(
                       {:assertCmd_failed,
                        [
                          {:module, :eunit_lib},
                          {:line, 496},
                          {:command, 'echo xyzzy >' ++ file},
                          {:expected_status, 0},
                          {:status, __N}
                        ]}
                     )
                 end
               end).()
            end},
           {497,
            fn ->
              (fn ->
                 case :eunit_lib.command('cat ' ++ file) do
                   {_, 'xyzzy\n'} ->
                     :ok

                   {_, __T} ->
                     :erlang.error(
                       {:assertCmdOutput_failed,
                        [
                          {:module, :eunit_lib},
                          {:line, 497},
                          {:command, 'cat ' ++ file},
                          {:expected_output, 'xyzzy\n'},
                          {:output, __T}
                        ]}
                     )
                 end
               end).()
            end}
         ]
       end}
    ]
  end

  defp win32_cmd_tests() do
    [
      {'command execution, status, and output',
       [
         {503,
          fn ->
            (fn ->
               case :eunit_lib.command('echo hello') do
                 {0, __Out} ->
                   __Out

                 {__N, _} ->
                   :erlang.error(
                     {:command_failed,
                      [
                        {:module, :eunit_lib},
                        {:line, 503},
                        {:command, 'echo hello'},
                        {:expected_status, 0},
                        {:status, __N}
                      ]}
                   )
               end
             end).()
          end},
         {504,
          fn ->
            (fn ->
               case :eunit_lib.command('echo hello') do
                 {_, 'hello\n'} ->
                   :ok

                 {_, __T} ->
                   :erlang.error(
                     {:assertCmdOutput_failed,
                      [
                        {:module, :eunit_lib},
                        {:line, 504},
                        {:command, 'echo hello'},
                        {:expected_output, 'hello\n'},
                        {:output, __T}
                      ]}
                   )
               end
             end).()
          end}
       ]}
    ]
  end

  def consult_file(file) do
    case :file.path_consult(
           ['.'] ++ :code.get_path(),
           file
         ) do
      {:ok, data, _Path} ->
        data

      {:error, reason} ->
        msg = :file.format_error(reason)
        throw({:file_read_error, {reason, msg, file}})
    end
  end

  def list_dir(dir) do
    case :file.list_dir(dir) do
      {:ok, fs} ->
        fs

      {:error, reason} ->
        msg = :file.format_error(reason)
        throw({:file_read_error, {reason, msg, dir}})
    end
  end

  def trie_new() do
    :gb_trees.empty()
  end

  def trie_store([_ | _], []) do
    []
  end

  def trie_store([e | es], t) do
    case :gb_trees.lookup(e, t) do
      :none ->
        cond do
          es === [] ->
            :gb_trees.insert(e, [], t)

          true ->
            :gb_trees.insert(e, trie_store(es, :gb_trees.empty()), t)
        end

      {:value, []} ->
        t

      {:value, t1} ->
        :gb_trees.update(e, trie_store(es, t1), t)
    end
  end

  def trie_store([], _T) do
    []
  end

  def trie_match([_ | _], []) do
    :prefix
  end

  def trie_match([e | es], t) do
    case :gb_trees.lookup(e, t) do
      :none ->
        :no

      {:value, []} ->
        cond do
          es === [] ->
            :exact

          true ->
            :prefix
        end

      {:value, t1} ->
        trie_match(es, t1)
    end
  end

  def trie_match([], []) do
    :exact
  end

  def trie_match([], _T) do
    :no
  end

  defp trie_test_() do
    [
      {'basic representation',
       [
         {590,
          fn ->
            (fn ->
               __T = :erlang.is_process_alive(self())

               case trie_new() === :gb_trees.empty() do
                 ^__T ->
                   :ok

                 __V ->
                   :erlang.error(
                     {:assert,
                      [
                        {:module, :eunit_lib},
                        {:line, 590},
                        {:expression, 'trie_new ( ) =:= gb_trees : empty ( )'},
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
          end},
         {591,
          fn ->
            (fn ->
               __T = :erlang.is_process_alive(self())

               case trie_store(
                      [1],
                      trie_new()
                    ) ===
                      :gb_trees.insert(
                        1,
                        [],
                        :gb_trees.empty()
                      ) do
                 ^__T ->
                   :ok

                 __V ->
                   :erlang.error(
                     {:assert,
                      [
                        {:module, :eunit_lib},
                        {:line, 592},
                        {:expression,
                         'trie_store ( [ 1 ] , trie_new ( ) ) =:= gb_trees : insert ( 1 , [ ] , gb_trees : empty ( ) )'},
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
          end},
         {593,
          fn ->
            (fn ->
               __T = :erlang.is_process_alive(self())

               case trie_store(
                      [1, 2],
                      trie_new()
                    ) ===
                      :gb_trees.insert(
                        1,
                        :gb_trees.insert(
                          2,
                          [],
                          :gb_trees.empty()
                        ),
                        :gb_trees.empty()
                      ) do
                 ^__T ->
                   :ok

                 __V ->
                   :erlang.error(
                     {:assert,
                      [
                        {:module, :eunit_lib},
                        {:line, 597},
                        {:expression,
                         'trie_store ( [ 1 , 2 ] , trie_new ( ) ) =:= gb_trees : insert ( 1 , gb_trees : insert ( 2 , [ ] , gb_trees : empty ( ) ) , gb_trees : empty ( ) )'},
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
          end},
         {598,
          fn ->
            (fn ->
               __T = :erlang.is_process_alive(self())

               case [] === trie_store([1], []) do
                 ^__T ->
                   :ok

                 __V ->
                   :erlang.error(
                     {:assert,
                      [
                        {:module, :eunit_lib},
                        {:line, 598},
                        {:expression, '[ ] =:= trie_store ( [ 1 ] , [ ] )'},
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
          end},
         {599,
          fn ->
            (fn ->
               __T = :erlang.is_process_alive(self())

               case [] === trie_store([], :gb_trees.empty()) do
                 ^__T ->
                   :ok

                 __V ->
                   :erlang.error(
                     {:assert,
                      [
                        {:module, :eunit_lib},
                        {:line, 599},
                        {:expression, '[ ] =:= trie_store ( [ ] , gb_trees : empty ( ) )'},
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
       ]},
      {'basic storing and matching',
       [
         {602,
          fn ->
            :no = trie_match([], trie_new())
          end},
         {603,
          fn ->
            :exact = trie_match([], trie_store([], trie_new()))
          end},
         {604,
          fn ->
            :no = trie_match([], trie_store([1], trie_new()))
          end},
         {605,
          fn ->
            :exact = trie_match([1], trie_store([1], trie_new()))
          end},
         {606,
          fn ->
            :prefix =
              trie_match(
                [1, 2],
                trie_store([1], trie_new())
              )
          end},
         {607,
          fn ->
            :no = trie_match([1], trie_store([1, 2], trie_new()))
          end},
         {608,
          fn ->
            :no = trie_match([1, 3], trie_store([1, 2], trie_new()))
          end},
         {609,
          fn ->
            :exact =
              trie_match(
                [1, 2, 3, 4, 5],
                trie_store(
                  [1, 2, 3, 4, 5],
                  trie_new()
                )
              )
          end},
         {611,
          fn ->
            :prefix =
              trie_match(
                [1, 2, 3, 4, 5],
                trie_store([1, 2, 3], trie_new())
              )
          end},
         {613,
          fn ->
            :no =
              trie_match(
                [1, 2, 2, 4, 5],
                trie_store([1, 2, 3], trie_new())
              )
          end}
       ]},
      {'matching with partially overlapping patterns', :setup,
       fn ->
         trie_store([1, 3, 2], trie_store([1, 2, 3], trie_new()))
       end,
       fn t ->
         [
           {622,
            fn ->
              :no = trie_match([], t)
            end},
           {623,
            fn ->
              :no = trie_match([1], t)
            end},
           {624,
            fn ->
              :no = trie_match([1, 2], t)
            end},
           {625,
            fn ->
              :no = trie_match([1, 3], t)
            end},
           {626,
            fn ->
              :exact = trie_match([1, 2, 3], t)
            end},
           {627,
            fn ->
              :exact = trie_match([1, 3, 2], t)
            end},
           {628,
            fn ->
              :no = trie_match([1, 2, 2], t)
            end},
           {629,
            fn ->
              :no = trie_match([1, 3, 3], t)
            end},
           {630,
            fn ->
              :prefix = trie_match([1, 2, 3, 4], t)
            end},
           {631,
            fn ->
              :prefix = trie_match([1, 3, 2, 1], t)
            end}
         ]
       end},
      {'matching with more general pattern overriding less general', :setup,
       fn ->
         trie_store([1], trie_store([1, 2, 3], trie_new()))
       end,
       fn _ ->
         :ok
       end,
       fn t ->
         [
           {638,
            fn ->
              :no = trie_match([], t)
            end},
           {639,
            fn ->
              :exact = trie_match([1], t)
            end},
           {640,
            fn ->
              :prefix = trie_match([1, 2], t)
            end},
           {641,
            fn ->
              :prefix = trie_match([1, 2, 3], t)
            end},
           {642,
            fn ->
              :prefix = trie_match([1, 2, 3, 4], t)
            end}
         ]
       end}
    ]
  end
end
