defmodule :m_eunit_tty do
  use Bitwise
  @behaviour :eunit_listener
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
  Record.defrecord(:r_state, :state, verbose: false, indent: 0, print_depth: 20)

  def start() do
    start([])
  end

  def start(options) do
    :eunit_listener.start(:eunit_tty, options)
  end

  def init(options) do
    printDepth = :proplists.get_value(:print_depth, options, 20)

    st =
      r_state(
        verbose: :proplists.get_bool(:verbose, options),
        print_depth: printDepth
      )

    :erlang.put(
      :no_tty,
      :proplists.get_bool(:no_tty, options)
    )

    receive do
      {:start, _Reference} ->
        cond do
          r_state(st, :verbose) ->
            print_header()

          true ->
            :ok
        end

        st
    end
  end

  def terminate({:ok, data}, st) do
    pass = :proplists.get_value(:pass, data, 0)
    fail = :proplists.get_value(:fail, data, 0)
    skip = :proplists.get_value(:skip, data, 0)
    cancel = :proplists.get_value(:cancel, data, 0)

    cond do
      fail === 0 and skip === 0 and cancel === 0 ->
        cond do
          pass === 0 ->
            fwrite('  There were no tests to run.\n')

          true ->
            cond do
              r_state(st, :verbose) ->
                print_bar()

              true ->
                :ok
            end

            cond do
              pass === 1 ->
                fwrite('  Test passed.\n')

              pass === 2 ->
                fwrite('  2 tests passed.\n')

              true ->
                fwrite('  All ~w tests passed.\n', [pass])
            end
        end

        sync_end(:ok)

      true ->
        print_bar()
        fwrite('  Failed: ~w.  Skipped: ~w.  Passed: ~w.\n', [fail, skip, pass])

        cond do
          cancel !== 0 ->
            fwrite('One or more tests were cancelled.\n')

          true ->
            :ok
        end

        sync_end(:error)
    end
  end

  def terminate({:error, reason}, r_state(print_depth: depth)) do
    fwrite('Internal error: ~tP.\n', [reason, depth])
    sync_end(:error)
  end

  defp sync_end(result) do
    receive do
      {:stop, reference, replyTo} ->
        send(replyTo, {:result, reference, result})
        :ok
    end
  end

  defp print_header() do
    fwrite('======================== EUnit ========================\n')
  end

  defp print_bar() do
    fwrite('=======================================================\n')
  end

  def handle_begin(:group, data, st) do
    :ok
    desc = :proplists.get_value(:desc, data)

    cond do
      desc !== '' and desc !== :undefined and
          r_state(st, :verbose) ->
        i = r_state(st, :indent)
        print_group_start(i, desc)
        r_state(st, indent: i + 1)

      true ->
        st
    end
  end

  def handle_begin(:test, data, st) do
    :ok

    cond do
      r_state(st, :verbose) ->
        print_test_begin(r_state(st, :indent), data)

      true ->
        :ok
    end

    st
  end

  def handle_end(:group, data, st) do
    :ok
    desc = :proplists.get_value(:desc, data)

    cond do
      desc !== '' and desc !== :undefined and
          r_state(st, :verbose) ->
        time = :proplists.get_value(:time, data)
        i = r_state(st, :indent)
        print_group_end(i, time)
        r_state(st, indent: i - 1)

      true ->
        st
    end
  end

  def handle_end(:test, data, st) do
    :ok

    case :proplists.get_value(:status, data) do
      :ok ->
        cond do
          r_state(st, :verbose) ->
            print_test_end(data)

          true ->
            :ok
        end

        st

      status ->
        cond do
          r_state(st, :verbose) ->
            :ok

          true ->
            print_test_begin(r_state(st, :indent), data)
        end

        print_test_error(status, data, st)
        st
    end
  end

  def handle_cancel(:group, data, st) do
    :ok
    i = r_state(st, :indent)

    case :proplists.get_value(:reason, data) do
      :undefined ->
        r_state(st, indent: i - 1)

      reason ->
        desc = :proplists.get_value(:desc, data)

        cond do
          desc !== '' and desc !== :undefined and
              r_state(st, :verbose) ->
            print_group_cancel(i, reason, st)

          true ->
            print_group_start(i, desc)
            print_group_cancel(i, reason, st)
        end

        r_state(st, indent: i - 1)
    end
  end

  def handle_cancel(:test, data, st) do
    :ok

    cond do
      r_state(st, :verbose) ->
        :ok

      true ->
        print_test_begin(r_state(st, :indent), data)
    end

    print_test_cancel(
      :proplists.get_value(:reason, data),
      st
    )

    st
  end

  defp indent(n) when is_integer(n) and n >= 1 do
    fwrite(:lists.duplicate(n * 2, ?\s))
  end

  defp indent(_N) do
    :ok
  end

  defp print_group_start(i, desc) do
    indent(i)
    fwrite('~ts\n', [desc])
  end

  defp print_group_end(i, time) do
    cond do
      time > 0 ->
        indent(i)
        fwrite('[done in ~.3f s]\n', [time / 1000])

      true ->
        :ok
    end
  end

  defp print_test_begin(i, data) do
    desc = :proplists.get_value(:desc, data)
    line = :proplists.get_value(:line, data, 0)
    indent(i)

    l =
      cond do
        line === 0 ->
          ''

        true ->
          :io_lib.fwrite('~w:', [line])
      end

    d =
      cond do
        desc === '' or desc === :undefined ->
          ''

        true ->
          :io_lib.fwrite(' (~ts)', [desc])
      end

    case :proplists.get_value(:source, data) do
      {module, name, _Arity} ->
        fwrite('~ts:~ts ~ts~ts...', [module, l, name, d])

      _ ->
        fwrite('~ts~ts...', [l, d])
    end
  end

  defp print_test_end(data) do
    time = :proplists.get_value(:time, data, 0)

    t =
      cond do
        time > 0 ->
          :io_lib.fwrite('[~.3f s] ', [time / 1000])

        true ->
          ''
      end

    fwrite('~tsok\n', [t])
  end

  defp print_test_error({:error, exception}, data, r_state(print_depth: depth)) do
    output = :proplists.get_value(:output, data)

    fwrite(
      '*failed*\n~ts',
      [:eunit_lib.format_exception(exception, depth)]
    )

    case output do
      <<>> ->
        fwrite('\n\n')

      <<text::size(800)-binary, _::size(1)-binary, _::binary>> ->
        fwrite('  output:<<"~ts">>...\n\n', [text])

      _ ->
        fwrite('  output:<<"~ts">>\n\n', [output])
    end
  end

  defp print_test_error({:skipped, reason}, _, _St) do
    fwrite('*did not run*\n::~ts\n', [format_skipped(reason)])
  end

  defp format_skipped({:module_not_found, m}) do
    :io_lib.fwrite('missing module: ~w', [m])
  end

  defp format_skipped({:no_such_function, {m, f, a}}) do
    :io_lib.fwrite('no such function: ~w:~tw/~w', [m, f, a])
  end

  defp print_test_cancel(reason, r_state(print_depth: depth)) do
    fwrite(format_cancel(reason, depth))
  end

  defp print_group_cancel(_I, {:blame, _}, _) do
    :ok
  end

  defp print_group_cancel(i, reason, r_state(print_depth: depth)) do
    indent(i)
    fwrite(format_cancel(reason, depth))
  end

  defp format_cancel(:undefined, _) do
    '*skipped*\n'
  end

  defp format_cancel(:timeout, _) do
    '*timed out*\n'
  end

  defp format_cancel({:startup, reason}, depth) do
    :io_lib.fwrite('*could not start test process*\n::~tP\n\n', [reason, depth])
  end

  defp format_cancel({:blame, _SubId}, _) do
    '*cancelled because of subtask*\n'
  end

  defp format_cancel({:exit, reason}, depth) do
    :io_lib.fwrite('*unexpected termination of test process*\n::~tP\n\n', [reason, depth])
  end

  defp format_cancel({:abort, reason}, depth) do
    :eunit_lib.format_error(reason, depth)
  end

  defp fwrite(string) do
    fwrite(string, [])
  end

  defp fwrite(string, args) do
    case :erlang.get(:no_tty) do
      false ->
        :io.fwrite(string, args)

      true ->
        :ok
    end
  end
end
