defmodule :m_yeccparser do
  use Bitwise
  require Record

  Record.defrecord(:r_symbol, :symbol,
    anno: :undefined,
    name: :undefined
  )

  defp symbol(symbol) do
    r_symbol(anno: anno_of(symbol), name: value_of(symbol))
  end

  defp value_of(token) do
    :erlang.element(3, token)
  end

  defp anno_of(token) do
    :erlang.element(2, token)
  end

  def parse(tokens) do
    yeccpars0(tokens, {:no_func, :no_line}, 0, [], [])
  end

  def parse_and_scan({f, a}) do
    yeccpars0([], {{f, a}, :no_line}, 0, [], [])
  end

  def parse_and_scan({m, f, a}) do
    arity = length(a)
    yeccpars0([], {{Function.capture(m, f, arity), a}, :no_line}, 0, [], [])
  end

  def format_error(message) do
    case :io_lib.deep_char_list(message) do
      true ->
        message

      _ ->
        :io_lib.write(message)
    end
  end

  defp return_error(line, message) do
    throw({:error, {line, :yeccparser, message}})
  end

  defp yeccpars0(tokens, tzr, state, states, vstack) do
    try do
      yeccpars1(tokens, tzr, state, states, vstack)
    catch
      :error, error ->
        try do
          yecc_error_type(error, __STACKTRACE__)
        catch
          _, _ ->
            :erlang.raise(:error, error, __STACKTRACE__)
        else
          desc ->
            :erlang.raise(:error, {:yecc_bug, '1.4', desc}, __STACKTRACE__)
        end

      {:error, {_Line, :yeccparser, _M}} = error ->
        error
    end
  end

  defp yecc_error_type(
         :function_clause,
         [{:yeccparser, f, arityOrArgs, _} | _]
       ) do
    case :erlang.atom_to_list(f) do
      'yeccgoto_' ++ symbolL ->
        {:ok, [{:atom, _, symbol}], _} = :erl_scan.string(symbolL)

        state =
          case arityOrArgs do
            [s, _, _, _, _, _, _] ->
              s

            _ ->
              :state_is_unknown
          end

        {symbol, state, :missing_in_goto_table}
    end
  end

  defp yeccpars1([token | tokens], tzr, state, states, vstack) do
    yeccpars2(state, :erlang.element(1, token), states, vstack, token, tokens, tzr)
  end

  defp yeccpars1([], {{f, a}, _Line}, state, states, vstack) do
    case apply(f, a) do
      {:ok, tokens, endline} ->
        yeccpars1(tokens, {{f, a}, endline}, state, states, vstack)

      {:eof, endline} ->
        yeccpars1([], {:no_func, endline}, state, states, vstack)

      {:error, descriptor, _Endline} ->
        {:error, descriptor}
    end
  end

  defp yeccpars1([], {:no_func, :no_line}, state, states, vstack) do
    line = 999_999
    yeccpars2(state, :"$end", states, vstack, yecc_end(line), [], {:no_func, line})
  end

  defp yeccpars1([], {:no_func, endline}, state, states, vstack) do
    yeccpars2(state, :"$end", states, vstack, yecc_end(endline), [], {:no_func, endline})
  end

  defp yeccpars1(state1, state, states, vstack, token0, [token | tokens], tzr) do
    yeccpars2(
      state,
      :erlang.element(1, token),
      [state1 | states],
      [token0 | vstack],
      token,
      tokens,
      tzr
    )
  end

  defp yeccpars1(state1, state, states, vstack, token0, [], {{_F, _A}, _Line} = tzr) do
    yeccpars1([], tzr, state, [state1 | states], [token0 | vstack])
  end

  defp yeccpars1(state1, state, states, vstack, token0, [], {:no_func, :no_line}) do
    line = yecctoken_end_location(token0)

    yeccpars2(
      state,
      :"$end",
      [state1 | states],
      [token0 | vstack],
      yecc_end(line),
      [],
      {:no_func, line}
    )
  end

  defp yeccpars1(state1, state, states, vstack, token0, [], {:no_func, line}) do
    yeccpars2(
      state,
      :"$end",
      [state1 | states],
      [token0 | vstack],
      yecc_end(line),
      [],
      {:no_func, line}
    )
  end

  defp yecc_end({line, _Column}) do
    {:"$end", line}
  end

  defp yecc_end(line) do
    {:"$end", line}
  end

  defp yecctoken_end_location(token) do
    try do
      :erl_anno.end_location(:erlang.element(2, token))
    catch
      _, _ ->
        yecctoken_location(token)
    else
      :undefined ->
        yecctoken_location(token)

      loc ->
        loc
    end
  end

  defp yeccerror(token) do
    text = yecctoken_to_string(token)
    location = yecctoken_location(token)
    {:error, {location, :yeccparser, ['syntax error before: ', text]}}
  end

  defp yecctoken_to_string(token) do
    try do
      :erl_scan.text(token)
    catch
      _, _ ->
        yecctoken2string(token)
    else
      :undefined ->
        yecctoken2string(token)

      txt ->
        txt
    end
  end

  defp yecctoken_location(token) do
    try do
      :erl_scan.location(token)
    catch
      _, _ ->
        :erlang.element(2, token)
    end
  end

  defp yecctoken2string({:atom, _, a}) do
    :io_lib.write_atom(a)
  end

  defp yecctoken2string({:integer, _, n}) do
    :io_lib.write(n)
  end

  defp yecctoken2string({:float, _, f}) do
    :io_lib.write(f)
  end

  defp yecctoken2string({:char, _, c}) do
    :io_lib.write_char(c)
  end

  defp yecctoken2string({:var, _, v}) do
    :io_lib.format('~s', [v])
  end

  defp yecctoken2string({:string, _, s}) do
    :io_lib.write_string(s)
  end

  defp yecctoken2string({:reserved_symbol, _, a}) do
    :io_lib.write(a)
  end

  defp yecctoken2string({_Cat, _, val}) do
    :io_lib.format('~tp', [val])
  end

  defp yecctoken2string({:dot, _}) do
    '\'.\''
  end

  defp yecctoken2string({:"$end", _}) do
    []
  end

  defp yecctoken2string({other, _}) when is_atom(other) do
    :io_lib.write_atom(other)
  end

  defp yecctoken2string(other) do
    :io_lib.format('~tp', [other])
  end

  defp yeccpars2(0 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_0(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(6 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_6(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(7 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_7(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(8 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_8(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(9 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_9(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(10 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_0(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(15 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_15(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(18 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_18(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(19 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_19(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(20 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_20(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(21 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_21(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(22 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_22(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(23 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_23(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(24 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_24(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(25 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_25(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(26 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_26(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(27 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_27(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(29 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_29(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(32 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_32(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(34 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_34(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(35 = s, cat, ss, stack, t, ts, tzr) do
    yeccpars2_35(s, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2(other, _, _, _, _, _, _) do
    :erlang.error({:yecc_bug, '1.4', {:missing_state_in_action_table, other}})
  end

  defp yeccpars2_0(s, :atom, ss, stack, t, ts, tzr) do
    yeccpars1(s, 6, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_0(s, :integer, ss, stack, t, ts, tzr) do
    yeccpars1(s, 7, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_0(s, :reserved_word, ss, stack, t, ts, tzr) do
    yeccpars1(s, 8, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_0(s, :var, ss, stack, t, ts, tzr) do
    yeccpars1(s, 9, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_0(_, _, _, _, t, _, _) do
    yeccerror(t)
  end

  defp yeccpars2_1(s, :atom, ss, stack, t, ts, tzr) do
    yeccpars1(s, 6, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_1(s, :integer, ss, stack, t, ts, tzr) do
    yeccpars1(s, 7, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_1(s, :reserved_word, ss, stack, t, ts, tzr) do
    yeccpars1(s, 8, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_1(s, :string, ss, stack, t, ts, tzr) do
    yeccpars1(s, 32, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_1(s, :var, ss, stack, t, ts, tzr) do
    yeccpars1(s, 9, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_1(_S, cat, ss, stack, t, ts, tzr) do
    yeccgoto_head(hd(ss), cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_2(_S, cat, ss, stack, t, ts, tzr) do
    yeccgoto_grammar(hd(ss), cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_3(s, :->, ss, stack, t, ts, tzr) do
    yeccpars1(s, 10, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_3(_, _, _, _, t, _, _) do
    yeccerror(t)
  end

  defp yeccpars2_4(_S, :"$end", _Ss, stack, _T, _Ts, _Tzr) do
    {:ok, hd(stack)}
  end

  defp yeccpars2_4(_, _, _, _, t, _, _) do
    yeccerror(t)
  end

  defp yeccpars2_5(_S, cat, ss, stack, t, ts, tzr) do
    yeccgoto_grammar(hd(ss), cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_6(_S, cat, ss, stack, t, ts, tzr) do
    newStack = yeccpars2_6_(stack)
    yeccgoto_symbol(hd(ss), cat, ss, newStack, t, ts, tzr)
  end

  defp yeccpars2_7(_S, cat, ss, stack, t, ts, tzr) do
    newStack = yeccpars2_7_(stack)
    yeccgoto_symbol(hd(ss), cat, ss, newStack, t, ts, tzr)
  end

  defp yeccpars2_8(_S, cat, ss, stack, t, ts, tzr) do
    newStack = yeccpars2_8_(stack)
    yeccgoto_symbol(hd(ss), cat, ss, newStack, t, ts, tzr)
  end

  defp yeccpars2_9(_S, cat, ss, stack, t, ts, tzr) do
    newStack = yeccpars2_9_(stack)
    yeccgoto_symbol(hd(ss), cat, ss, newStack, t, ts, tzr)
  end

  defp yeccpars2_11(s, :":", ss, stack, t, ts, tzr) do
    yeccpars1(s, 15, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_11(_S, cat, ss, stack, t, ts, tzr) do
    newStack = yeccpars2_11_(stack)
    yeccpars2_14(14, cat, [11 | ss], newStack, t, ts, tzr)
  end

  defp yeccpars2_12(s, :atom, ss, stack, t, ts, tzr) do
    yeccpars1(s, 6, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_12(s, :integer, ss, stack, t, ts, tzr) do
    yeccpars1(s, 7, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_12(s, :reserved_word, ss, stack, t, ts, tzr) do
    yeccpars1(s, 8, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_12(s, :var, ss, stack, t, ts, tzr) do
    yeccpars1(s, 9, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_12(_S, cat, ss, stack, t, ts, tzr) do
    newStack = yeccpars2_12_(stack)
    yeccgoto_symbols(hd(ss), cat, ss, newStack, t, ts, tzr)
  end

  defp yeccpars2_13(_S, cat, ss, stack, t, ts, tzr) do
    [_ | nss] = ss
    newStack = yeccpars2_13_(stack)
    yeccgoto_symbols(hd(nss), cat, nss, newStack, t, ts, tzr)
  end

  defp yeccpars2_14(s, :dot, ss, stack, t, ts, tzr) do
    yeccpars1(s, 29, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_14(_, _, _, _, t, _, _) do
    yeccerror(t)
  end

  defp yeccpars2_15(s, :->, ss, stack, t, ts, tzr) do
    yeccpars1(s, 18, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_15(s, :":", ss, stack, t, ts, tzr) do
    yeccpars1(s, 19, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_15(s, :atom, ss, stack, t, ts, tzr) do
    yeccpars1(s, 20, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_15(s, :char, ss, stack, t, ts, tzr) do
    yeccpars1(s, 21, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_15(s, :float, ss, stack, t, ts, tzr) do
    yeccpars1(s, 22, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_15(s, :integer, ss, stack, t, ts, tzr) do
    yeccpars1(s, 23, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_15(s, :reserved_symbol, ss, stack, t, ts, tzr) do
    yeccpars1(s, 24, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_15(s, :reserved_word, ss, stack, t, ts, tzr) do
    yeccpars1(s, 25, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_15(s, :string, ss, stack, t, ts, tzr) do
    yeccpars1(s, 26, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_15(s, :var, ss, stack, t, ts, tzr) do
    yeccpars1(s, 27, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_15(_, _, _, _, t, _, _) do
    yeccerror(t)
  end

  defp yeccpars2_16(_S, cat, ss, stack, t, ts, tzr) do
    [_ | nss] = ss
    newStack = yeccpars2_16_(stack)
    yeccgoto_attached_code(hd(nss), cat, nss, newStack, t, ts, tzr)
  end

  defp yeccpars2_17(s, :->, ss, stack, t, ts, tzr) do
    yeccpars1(s, 18, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_17(s, :":", ss, stack, t, ts, tzr) do
    yeccpars1(s, 19, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_17(s, :atom, ss, stack, t, ts, tzr) do
    yeccpars1(s, 20, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_17(s, :char, ss, stack, t, ts, tzr) do
    yeccpars1(s, 21, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_17(s, :float, ss, stack, t, ts, tzr) do
    yeccpars1(s, 22, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_17(s, :integer, ss, stack, t, ts, tzr) do
    yeccpars1(s, 23, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_17(s, :reserved_symbol, ss, stack, t, ts, tzr) do
    yeccpars1(s, 24, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_17(s, :reserved_word, ss, stack, t, ts, tzr) do
    yeccpars1(s, 25, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_17(s, :string, ss, stack, t, ts, tzr) do
    yeccpars1(s, 26, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_17(s, :var, ss, stack, t, ts, tzr) do
    yeccpars1(s, 27, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_17(_S, cat, ss, stack, t, ts, tzr) do
    newStack = yeccpars2_17_(stack)
    yeccgoto_tokens(hd(ss), cat, ss, newStack, t, ts, tzr)
  end

  defp yeccpars2_18(_S, cat, ss, stack, t, ts, tzr) do
    newStack = yeccpars2_18_(stack)
    yeccgoto_token(hd(ss), cat, ss, newStack, t, ts, tzr)
  end

  defp yeccpars2_19(_S, cat, ss, stack, t, ts, tzr) do
    newStack = yeccpars2_19_(stack)
    yeccgoto_token(hd(ss), cat, ss, newStack, t, ts, tzr)
  end

  defp yeccpars2_20(_S, cat, ss, stack, t, ts, tzr) do
    yeccgoto_token(hd(ss), cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_21(_S, cat, ss, stack, t, ts, tzr) do
    yeccgoto_token(hd(ss), cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_22(_S, cat, ss, stack, t, ts, tzr) do
    yeccgoto_token(hd(ss), cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_23(_S, cat, ss, stack, t, ts, tzr) do
    yeccgoto_token(hd(ss), cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_24(_S, cat, ss, stack, t, ts, tzr) do
    newStack = yeccpars2_24_(stack)
    yeccgoto_token(hd(ss), cat, ss, newStack, t, ts, tzr)
  end

  defp yeccpars2_25(_S, cat, ss, stack, t, ts, tzr) do
    newStack = yeccpars2_25_(stack)
    yeccgoto_token(hd(ss), cat, ss, newStack, t, ts, tzr)
  end

  defp yeccpars2_26(_S, cat, ss, stack, t, ts, tzr) do
    yeccgoto_token(hd(ss), cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_27(_S, cat, ss, stack, t, ts, tzr) do
    yeccgoto_token(hd(ss), cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_28(_S, cat, ss, stack, t, ts, tzr) do
    [_ | nss] = ss
    newStack = yeccpars2_28_(stack)
    yeccgoto_tokens(hd(nss), cat, nss, newStack, t, ts, tzr)
  end

  defp yeccpars2_29(_S, cat, ss, stack, t, ts, tzr) do
    [_, _, _, _ | nss] = ss
    newStack = yeccpars2_29_(stack)
    yeccgoto_rule(hd(nss), cat, nss, newStack, t, ts, tzr)
  end

  defp yeccpars2_30(s, :dot, ss, stack, t, ts, tzr) do
    yeccpars1(s, 35, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_30(_, _, _, _, t, _, _) do
    yeccerror(t)
  end

  defp yeccpars2_31(s, :dot, ss, stack, t, ts, tzr) do
    yeccpars1(s, 34, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_31(_, _, _, _, t, _, _) do
    yeccerror(t)
  end

  defp yeccpars2_32(s, :string, ss, stack, t, ts, tzr) do
    yeccpars1(s, 32, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_32(_S, cat, ss, stack, t, ts, tzr) do
    newStack = yeccpars2_32_(stack)
    yeccgoto_strings(hd(ss), cat, ss, newStack, t, ts, tzr)
  end

  defp yeccpars2_33(_S, cat, ss, stack, t, ts, tzr) do
    [_ | nss] = ss
    newStack = yeccpars2_33_(stack)
    yeccgoto_strings(hd(nss), cat, nss, newStack, t, ts, tzr)
  end

  defp yeccpars2_34(_S, cat, ss, stack, t, ts, tzr) do
    [_, _ | nss] = ss
    newStack = yeccpars2_34_(stack)
    yeccgoto_declaration(hd(nss), cat, nss, newStack, t, ts, tzr)
  end

  defp yeccpars2_35(_S, cat, ss, stack, t, ts, tzr) do
    [_, _ | nss] = ss
    newStack = yeccpars2_35_(stack)
    yeccgoto_declaration(hd(nss), cat, nss, newStack, t, ts, tzr)
  end

  defp yeccgoto_attached_code(11, cat, ss, stack, t, ts, tzr) do
    yeccpars2_14(14, cat, ss, stack, t, ts, tzr)
  end

  defp yeccgoto_declaration(0 = _S, cat, ss, stack, t, ts, tzr) do
    yeccpars2_5(_S, cat, ss, stack, t, ts, tzr)
  end

  defp yeccgoto_grammar(0, cat, ss, stack, t, ts, tzr) do
    yeccpars2_4(4, cat, ss, stack, t, ts, tzr)
  end

  defp yeccgoto_head(0, cat, ss, stack, t, ts, tzr) do
    yeccpars2_3(3, cat, ss, stack, t, ts, tzr)
  end

  defp yeccgoto_rule(0 = _S, cat, ss, stack, t, ts, tzr) do
    yeccpars2_2(_S, cat, ss, stack, t, ts, tzr)
  end

  defp yeccgoto_strings(1, cat, ss, stack, t, ts, tzr) do
    yeccpars2_31(31, cat, ss, stack, t, ts, tzr)
  end

  defp yeccgoto_strings(32 = _S, cat, ss, stack, t, ts, tzr) do
    yeccpars2_33(_S, cat, ss, stack, t, ts, tzr)
  end

  defp yeccgoto_symbol(0, cat, ss, stack, t, ts, tzr) do
    yeccpars2_1(1, cat, ss, stack, t, ts, tzr)
  end

  defp yeccgoto_symbol(1, cat, ss, stack, t, ts, tzr) do
    yeccpars2_12(12, cat, ss, stack, t, ts, tzr)
  end

  defp yeccgoto_symbol(10, cat, ss, stack, t, ts, tzr) do
    yeccpars2_12(12, cat, ss, stack, t, ts, tzr)
  end

  defp yeccgoto_symbol(12, cat, ss, stack, t, ts, tzr) do
    yeccpars2_12(12, cat, ss, stack, t, ts, tzr)
  end

  defp yeccgoto_symbols(1, cat, ss, stack, t, ts, tzr) do
    yeccpars2_30(30, cat, ss, stack, t, ts, tzr)
  end

  defp yeccgoto_symbols(10, cat, ss, stack, t, ts, tzr) do
    yeccpars2_11(11, cat, ss, stack, t, ts, tzr)
  end

  defp yeccgoto_symbols(12 = _S, cat, ss, stack, t, ts, tzr) do
    yeccpars2_13(_S, cat, ss, stack, t, ts, tzr)
  end

  defp yeccgoto_token(15, cat, ss, stack, t, ts, tzr) do
    yeccpars2_17(17, cat, ss, stack, t, ts, tzr)
  end

  defp yeccgoto_token(17, cat, ss, stack, t, ts, tzr) do
    yeccpars2_17(17, cat, ss, stack, t, ts, tzr)
  end

  defp yeccgoto_tokens(15 = _S, cat, ss, stack, t, ts, tzr) do
    yeccpars2_16(_S, cat, ss, stack, t, ts, tzr)
  end

  defp yeccgoto_tokens(17 = _S, cat, ss, stack, t, ts, tzr) do
    yeccpars2_28(_S, cat, ss, stack, t, ts, tzr)
  end

  defp yeccpars2_6_(__Stack0) do
    [__1 | __Stack] = __Stack0

    [
      symbol(__1)
      | __Stack
    ]
  end

  defp yeccpars2_7_(__Stack0) do
    [__1 | __Stack] = __Stack0

    [
      symbol(__1)
      | __Stack
    ]
  end

  defp yeccpars2_8_(__Stack0) do
    [__1 | __Stack] = __Stack0

    [
      symbol(__1)
      | __Stack
    ]
  end

  defp yeccpars2_9_(__Stack0) do
    [__1 | __Stack] = __Stack0

    [
      symbol(__1)
      | __Stack
    ]
  end

  defp yeccpars2_11_(__Stack0) do
    [
      {:erlang_code, [{:atom, :erl_anno.new(0), :"$undefined"}]}
      | __Stack0
    ]
  end

  defp yeccpars2_12_(__Stack0) do
    [__1 | __Stack] = __Stack0

    [
      [__1]
      | __Stack
    ]
  end

  defp yeccpars2_13_(__Stack0) do
    [__2, __1 | __Stack] = __Stack0

    [
      [__1 | __2]
      | __Stack
    ]
  end

  defp yeccpars2_16_(__Stack0) do
    [__2, __1 | __Stack] = __Stack0

    [
      {:erlang_code, __2}
      | __Stack
    ]
  end

  defp yeccpars2_17_(__Stack0) do
    [__1 | __Stack] = __Stack0

    [
      [__1]
      | __Stack
    ]
  end

  defp yeccpars2_18_(__Stack0) do
    [__1 | __Stack] = __Stack0

    [
      {:->, anno_of(__1)}
      | __Stack
    ]
  end

  defp yeccpars2_19_(__Stack0) do
    [__1 | __Stack] = __Stack0

    [
      {:":", anno_of(__1)}
      | __Stack
    ]
  end

  defp yeccpars2_24_(__Stack0) do
    [__1 | __Stack] = __Stack0

    [
      {value_of(__1), anno_of(__1)}
      | __Stack
    ]
  end

  defp yeccpars2_25_(__Stack0) do
    [__1 | __Stack] = __Stack0

    [
      {value_of(__1), anno_of(__1)}
      | __Stack
    ]
  end

  defp yeccpars2_28_(__Stack0) do
    [__2, __1 | __Stack] = __Stack0

    [
      [__1 | __2]
      | __Stack
    ]
  end

  defp yeccpars2_29_(__Stack0) do
    [__5, __4, __3, __2, __1 | __Stack] = __Stack0

    [
      {:rule, [__1 | __3], __4}
      | __Stack
    ]
  end

  defp yeccpars2_32_(__Stack0) do
    [__1 | __Stack] = __Stack0

    [
      [__1]
      | __Stack
    ]
  end

  defp yeccpars2_33_(__Stack0) do
    [__2, __1 | __Stack] = __Stack0

    [
      [__1 | __2]
      | __Stack
    ]
  end

  defp yeccpars2_34_(__Stack0) do
    [__3, __2, __1 | __Stack] = __Stack0

    [
      {__1, __2}
      | __Stack
    ]
  end

  defp yeccpars2_35_(__Stack0) do
    [__3, __2, __1 | __Stack] = __Stack0

    [
      {__1, __2}
      | __Stack
    ]
  end
end
