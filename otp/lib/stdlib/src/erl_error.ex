defmodule :m_erl_error do
  use Bitwise

  def format_exception(i, class, reason, stackTrace, stackFun, formatFun) do
    format_exception(i, class, reason, stackTrace, stackFun, formatFun, :latin1)
  end

  def format_exception(i, class, reason, stackTrace, stackFun, formatFun, encoding) do
    fF = wrap_format_fun_2(formatFun)
    format_exception(i, class, reason, stackTrace, stackFun, fF, encoding, -1)
  end

  def format_exception(i, class, reason, stackTrace, stackFun, formatFun, encoding, charsLimit)
      when is_integer(i) and i >= 1 and
             is_function(stackFun, 3) and
             is_function(formatFun, 3) and is_integer(charsLimit) do
    s = n_spaces(i - 1)
    {term, trace1, trace} = analyze_exception(class, reason, stackTrace)

    stLimit =
      cond do
        charsLimit < 0 ->
          charsLimit

        true ->
          div(charsLimit, 3)
      end

    st = format_stacktrace1(s, trace, formatFun, stackFun, encoding, stLimit)
    lim = sub(sub(charsLimit, exited(class), :latin1), st, encoding)
    expl0 = explain_reason(term, class, trace1, formatFun, s, encoding, lim)

    formatString =
      case encoding do
        :latin1 ->
          '~s~s'

        _ ->
          '~s~ts'
      end

    expl =
      :io_lib.fwrite(
        formatString,
        [exited(class), expl0]
      )

    case st do
      [] ->
        expl

      _ ->
        [expl, ?\n, st]
    end
  end

  def format_stacktrace(i, stackTrace, stackFun, formatFun) do
    format_stacktrace(i, stackTrace, stackFun, formatFun, :latin1)
  end

  def format_stacktrace(i, stackTrace, stackFun, formatFun, encoding)
      when is_integer(i) and i >= 1 and
             is_function(stackFun, 3) and
             is_function(formatFun, 2) do
    s = n_spaces(i - 1)
    fF = wrap_format_fun_2(formatFun)
    format_stacktrace1(s, stackTrace, fF, stackFun, encoding, -1)
  end

  def format_call(i, forMForFun, as, formatFun) do
    format_call(i, forMForFun, as, formatFun, :latin1)
  end

  def format_call(i, forMForFun, as, formatFun, enc)
      when is_integer(i) and i >= 1 and is_list(as) and
             is_function(formatFun, 2) do
    fF = wrap_format_fun_2(formatFun)
    format_call('', n_spaces(i - 1), forMForFun, as, fF, enc)
  end

  def format_fun(fun) do
    format_fun(fun, :latin1)
  end

  def format_fun(fun, enc) when is_function(fun) do
    {:module, m} = :erlang.fun_info(fun, :module)
    {:name, f} = :erlang.fun_info(fun, :name)
    {:arity, a} = :erlang.fun_info(fun, :arity)

    case :erlang.fun_info(fun, :type) do
      {:type, :local} when f === '' ->
        :io_lib.fwrite("~w", [fun])

      {:type, :local} when m === :erl_eval ->
        :io_lib.fwrite("interpreted function with arity ~w", [a])

      {:type, :local} ->
        mfa_to_string(m, f, a, enc)

      {:type, :external} ->
        mfa_to_string(m, f, a, enc)
    end
  end

  defp wrap_format_fun_2(formatFun) do
    fn t, i1, cL ->
      {formatFun.(t, i1), cL}
    end
  end

  defp analyze_exception(:error, term, stack) do
    case {is_stacktrace(stack), stack, term} do
      {true, [{_, _, as, _} = mFAL | mFAs], :function_clause}
      when is_list(as) ->
        {term, [mFAL], mFAs}

      {true, [{:shell, f, a, _}], :function_clause}
      when is_integer(a) ->
        {term, [{f, a}], []}

      {true, [{_, _, _, _} = mFAL | mFAs], :undef} ->
        {term, [mFAL], mFAs}

      {true, _, _} ->
        {term, [], stack}

      {false, _, _} ->
        {{term, stack}, [], []}
    end
  end

  defp analyze_exception(_Class, term, stack) do
    case is_stacktrace(stack) do
      true ->
        {term, [], stack}

      false ->
        {{term, stack}, [], []}
    end
  end

  defp is_stacktrace([]) do
    true
  end

  defp is_stacktrace([{m, f, a, i} | fs])
       when is_atom(m) and
              is_atom(f) and is_integer(a) and
              is_list(i) do
    is_stacktrace(fs)
  end

  defp is_stacktrace([{m, f, as, i} | fs])
       when is_atom(m) and
              is_atom(f) and length(as) >= 0 and
              is_list(i) do
    is_stacktrace(fs)
  end

  defp is_stacktrace(_) do
    false
  end

  defp explain_reason(:badarg, :error, [], _PF, _S, _Enc, _CL) do
    "bad argument"
  end

  defp explain_reason({:badarg, v}, :error = cl, [], pF, s, _Enc, cL) do
    format_value(v, "bad argument: ", cl, pF, s, cL)
  end

  defp explain_reason(:badarith, :error, [], _PF, _S, _Enc, _CL) do
    "an error occurred when evaluating an arithmetic expression"
  end

  defp explain_reason({:badarity, {fun, as}}, :error, [], _PF, _S, enc, _CL)
       when is_function(fun) do
    :io_lib.fwrite(
      "~ts called with ~s",
      [format_fun(fun, enc), argss(length(as))]
    )
  end

  defp explain_reason({:badfun, term}, :error = cl, [], pF, s, _Enc, cL) do
    format_value(term, "bad function ", cl, pF, s, cL)
  end

  defp explain_reason({:badmatch, term}, :error = cl, [], pF, s, _Enc, cL) do
    str = "no match of right hand side value "
    format_value(term, str, cl, pF, s, cL)
  end

  defp explain_reason({:case_clause, v}, :error = cl, [], pF, s, _Enc, cL) do
    format_value(v, "no case clause matching ", cl, pF, s, cL)
  end

  defp explain_reason(:function_clause, :error, [{f, a}], _PF, _S, _Enc, _CL) do
    fAs = :io_lib.fwrite("~w/~w", [f, a])
    ["no function clause matching call to " | fAs]
  end

  defp explain_reason(:function_clause, :error = cl, [{m, f, as, loc}], pF, s, enc, cL) do
    str = "no function clause matching "

    [
      [format_errstr_call(str, cl, {m, f}, as, pF, s, enc, cL), ?\s]
      | location(loc)
    ]
  end

  defp explain_reason(:if_clause, :error, [], _PF, _S, _Enc, _CL) do
    "no true branch found when evaluating an if expression"
  end

  defp explain_reason(:noproc, :error, [], _PF, _S, _Enc, _CL) do
    "no such process or port"
  end

  defp explain_reason(:notalive, :error, [], _PF, _S, _Enc, _CL) do
    "the node cannot be part of a distributed system"
  end

  defp explain_reason(:system_limit, :error, [], _PF, _S, _Enc, _CL) do
    "a system limit has been reached"
  end

  defp explain_reason(:timeout_value, :error, [], _PF, _S, _Enc, _CL) do
    "bad receive timeout value"
  end

  defp explain_reason({:try_clause, v}, :error = cl, [], pF, s, _Enc, cL) do
    format_value(v, "no try clause matching ", cl, pF, s, cL)
  end

  defp explain_reason(:undef, :error, [{m, f, a, _}], _PF, _S, enc, _CL) do
    :io_lib.fwrite("undefined function ~ts", [mfa_to_string(m, f, n_args(a), enc)])
  end

  defp explain_reason({:shell_undef, f, a, _}, :error, [], _PF, _S, enc, _CL) do
    fS = to_string(f, enc)
    :io_lib.fwrite("undefined shell command ~ts/~w", [fS, n_args(a)])
  end

  defp explain_reason({:argument_limit, _Fun}, :error, [], _PF, _S, _Enc, _CL) do
    :io_lib.fwrite("limit of number of arguments to interpreted function exceeded", [])
  end

  defp explain_reason({:bad_filter, v}, :error = cl, [], pF, s, _Enc, cL) do
    format_value(v, "bad filter ", cl, pF, s, cL)
  end

  defp explain_reason({:bad_generator, v}, :error = cl, [], pF, s, _Enc, cL) do
    format_value(v, "bad generator ", cl, pF, s, cL)
  end

  defp explain_reason({:unbound, v}, :error, [], _PF, _S, _Enc, _CL) do
    :io_lib.fwrite("variable ~w is unbound", [v])
  end

  defp explain_reason({:restricted_shell_bad_return, v}, :exit = cl, [], pF, s, _Enc, cL) do
    str = "restricted shell module returned bad value "
    format_value(v, str, cl, pF, s, cL)
  end

  defp explain_reason({:restricted_shell_disallowed, {forMF, as}}, :exit = cl, [], pF, s, enc, cL) do
    str = "restricted shell does not allow "
    format_errstr_call(str, cl, forMF, as, pF, s, enc, cL)
  end

  defp explain_reason(:restricted_shell_started, :exit, [], _PF, _S, _Enc, _CL) do
    "restricted shell starts now"
  end

  defp explain_reason(:restricted_shell_stopped, :exit, [], _PF, _S, _Enc, _CL) do
    "restricted shell stopped"
  end

  defp explain_reason(reason, class, [], pF, s, _Enc, cL) do
    {l, _} = pF.(reason, :erlang.iolist_size(s) + 1 + exited_size(class), cL)
    l
  end

  defp n_args(a) when is_integer(a) do
    a
  end

  defp n_args(as) when is_list(as) do
    length(as)
  end

  defp argss(0) do
    "no arguments"
  end

  defp argss(1) do
    "one argument"
  end

  defp argss(2) do
    "two arguments"
  end

  defp argss(i) do
    :io_lib.fwrite("~w arguments", [i])
  end

  defp format_stacktrace1(s0, stack0, pF, sF, enc, cL) do
    stack1 =
      :lists.dropwhile(
        fn {m, f, a, _} ->
          sF.(m, f, a)
        end,
        :lists.reverse(stack0)
      )

    s = ['  ' | s0]
    stack = :lists.reverse(stack1)
    format_stacktrace2(s, stack, 1, pF, enc, cL)
  end

  defp format_stacktrace2(_S, _Stack, _N, _PF, _Enc, _CL = 0) do
    []
  end

  defp format_stacktrace2(s, [{m, f, a, l} | fs], n, pF, enc, cL)
       when is_integer(a) do
    cs =
      :io_lib.fwrite(
        "~s~s ~ts ~ts",
        [sep(n, s), origin(n, m, f, a), mfa_to_string(m, f, a, enc), location(l)]
      )

    cL1 = sub(cL, cs, enc)
    [cs | format_stacktrace2(s, fs, n + 1, pF, enc, cL1)]
  end

  defp format_stacktrace2(s, [{m, f, as, _} | fs], n, pF, enc, cL)
       when is_list(as) do
    a = length(as)
    calledAs = [s, "   called as "]
    c = format_call('', calledAs, {m, f}, as, pF, enc, cL)

    cs =
      :io_lib.fwrite(
        "~s~s ~ts\n~s~ts",
        [sep(n, s), origin(n, m, f, a), mfa_to_string(m, f, a, enc), calledAs, c]
      )

    cL1 = sub(cL, enc, cs)
    [cs | format_stacktrace2(s, fs, n + 1, pF, enc, cL1)]
  end

  defp format_stacktrace2(_S, [], _N, _PF, _Enc, _CL) do
    ''
  end

  defp location(l) do
    file = :proplists.get_value(:file, l)
    line = :proplists.get_value(:line, l)

    cond do
      file !== :undefined and line !== :undefined ->
        :io_lib.format('(~ts, line ~w)', [file, line])

      true ->
        ''
    end
  end

  defp sep(1, s) do
    s
  end

  defp sep(_, s) do
    [?\n | s]
  end

  defp origin(1, m, f, a) do
    case is_op({m, f}, n_args(a)) do
      {:yes, ^f} ->
        "in operator "

      :no ->
        "in function "
    end
  end

  defp origin(_N, _M, _F, _A) do
    "in call from"
  end

  defp format_errstr_call(errStr, class, forMForFun, as, pF, pre0, enc, cL) do
    pre1 = [pre0 | n_spaces(exited_size(class))]
    format_call(errStr, pre1, forMForFun, as, pF, enc, cL)
  end

  defp format_call(errStr, pre1, forMForFun, as, pF, enc) do
    format_call(errStr, pre1, forMForFun, as, pF, enc, -1)
  end

  defp format_call(errStr, pre1, forMForFun, as, pF, enc, cL) do
    arity = length(as)

    [
      errStr
      | case is_op(forMForFun, arity) do
          {:yes, op} ->
            format_op(errStr, pre1, op, as, pF, enc, cL)

          :no ->
            mFs = mf_to_string(forMForFun, arity, enc)
            i1 = :string.length([[pre1, errStr] | mFs])
            s1 = pp_arguments(pF, as, i1, enc, cL)
            s2 = pp_arguments(pF, as, :string.length([pre1 | mFs]), enc, cL)
            s3 = pp_arguments(pF, [:a2345, :b2345], i1, enc, cL)
            long = count_nl(s3) > 0

            case :erlang.or(long, count_nl(s2) < count_nl(s1)) do
              true ->
                [?\n, pre1, mFs, s2]

              false ->
                [mFs, s1]
            end
        end
    ]
  end

  defp format_op(errStr, pre, op, [a1], pF, _Enc, cL) do
    opS = :io_lib.fwrite("~s ", [op])
    i1 = :erlang.iolist_size([errStr, pre, opS])
    {s, _} = pF.(a1, i1 + 1, cL)
    [opS | s]
  end

  defp format_op(errStr, pre, op, [a1, a2], pF, enc, cL) do
    i1 = :erlang.iolist_size([errStr, pre])
    {s1, cL1} = pF.(a1, i1 + 1, cL)
    {s2, _} = pF.(a2, i1 + 1, cL1)
    opS = :erlang.atom_to_list(op)
    pre1 = [?\n | n_spaces(i1)]

    case count_nl(s1) > 0 do
      true ->
        [[s1, pre1, opS, pre1] | s2]

      false ->
        opS2 = :io_lib.fwrite(" ~s ", [op])
        size1 = :erlang.iolist_size([[errStr, pre] | opS2])
        size2 = size(enc, s1)
        {s2_2, _} = pF.(a2, size1 + size2 + 1, cL1)

        case count_nl(s2) < count_nl(s2_2) do
          true ->
            [[s1, pre1, opS, pre1] | s2]

          false ->
            [[s1, opS2] | s2_2]
        end
    end
  end

  defp pp_arguments(pF, as, i, enc, cL) do
    case {as, printable_list(enc, as)} do
      {[int | t], true} ->
        l = :erlang.integer_to_list(int)
        ll = length(l)
        a = :erlang.list_to_atom(:lists.duplicate(ll, ?a))
        {s0, _} = pF.([a | t], i + 1, cL)
        s = :unicode.characters_to_list(s0, enc)

        brackets_to_parens(
          [?[, l, :string.slice(s, 1 + ll)],
          enc
        )

      _ ->
        {s, _CL1} = pF.(as, i + 1, cL)
        brackets_to_parens(s, enc)
    end
  end

  defp brackets_to_parens(s, enc) do
    b = :unicode.characters_to_binary(s, enc)
    sz = byte_size(b) - 2
    <<?[, r::size(sz)-binary, ?]>> = b
    [?(, r, ?)]
  end

  defp printable_list(:latin1, as) do
    :io_lib.printable_latin1_list(as)
  end

  defp printable_list(_, as) do
    :io_lib.printable_list(as)
  end

  defp mfa_to_string(m, f, a, enc) do
    :io_lib.fwrite("~ts/~w", [mf_to_string({m, f}, a, enc), a])
  end

  defp mf_to_string({m, f}, a, enc) do
    case :erl_internal.bif(m, f, a) do
      true ->
        :io_lib.fwrite("~w", [f])

      false ->
        case is_op({m, f}, a) do
          {:yes, :/} ->
            :io_lib.fwrite("~w", [f])

          {:yes, ^f} ->
            :erlang.atom_to_list(f)

          :no ->
            fS = to_string(f, enc)
            :io_lib.fwrite("~w:~ts", [m, fS])
        end
    end
  end

  defp mf_to_string(fun, _A, enc) when is_function(fun) do
    format_fun(fun, enc)
  end

  defp mf_to_string(f, _A, enc) do
    fS = to_string(f, enc)
    :io_lib.fwrite("~ts", [fS])
  end

  defp format_value(v, errStr, class, pF, s, cL) do
    pre1Sz = exited_size(class)
    {s1, _} = pF.(v, pre1Sz + :erlang.iolist_size([s, errStr]) + 1, cL)

    [
      errStr
      | case count_nl(s1) do
          n1 when n1 > 1 ->
            {s2, _} = pF.(v, :erlang.iolist_size(s) + 1 + pre1Sz, cL)

            case count_nl(s2) < n1 do
              true ->
                [[?\n, s, n_spaces(pre1Sz)] | s2]

              false ->
                s1
            end

          _ ->
            s1
        end
    ]
  end

  defp count_nl([e | es]) do
    count_nl(e) + count_nl(es)
  end

  defp count_nl(?\n) do
    1
  end

  defp count_nl(bin) when is_binary(bin) do
    count_nl(:erlang.binary_to_list(bin))
  end

  defp count_nl(_) do
    0
  end

  defp n_spaces(n) do
    :lists.duplicate(n, ?\s)
  end

  defp is_op(forMForFun, a) do
    try do
      {:erlang, f} = forMForFun
      _ = :erl_internal.op_type(f, a)
      {:yes, f}
    catch
      :error, _ ->
        :no
    end
  end

  defp exited_size(class) do
    :erlang.iolist_size(exited(class))
  end

  defp exited(:error) do
    "exception error: "
  end

  defp exited(:exit) do
    "exception exit: "
  end

  defp exited(:throw) do
    "exception throw: "
  end

  defp to_string(a, :latin1) do
    :io_lib.write_atom_as_latin1(a)
  end

  defp to_string(a, _) do
    :io_lib.write_atom(a)
  end

  defp sub(t, _, _Enc) when t < 0 do
    t
  end

  defp sub(t, s, enc) do
    sub(t, size(enc, s))
  end

  defp sub(t, sz) when t >= sz do
    t - sz
  end

  defp sub(_T, _Sz) do
    0
  end

  defp size(:latin1, s) do
    :erlang.iolist_size(s)
  end

  defp size(_, s) do
    :string.length(s)
  end
end
