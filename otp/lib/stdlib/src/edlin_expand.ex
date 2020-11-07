defmodule :m_edlin_expand do
  use Bitwise
  import :lists, only: [prefix: 2, reverse: 1]

  def expand(bef0) do
    {bef1, word, _} = :edlin.over_word(bef0, [], 0)

    case over_white(bef1, [], 0) do
      {[?, | bef2], _White, _Nwh} ->
        {bef3, _White1, _Nwh1} = over_white(bef2, [], 0)
        {bef4, mod, _Nm} = :edlin.over_word(bef3, [], 0)

        case expand_function(bef4) do
          :help ->
            expand_function_name(mod, word, ',')

          _ ->
            expand_module_name(word, ',')
        end

      {[?: | bef2], _White, _Nwh} ->
        {bef3, _White1, _Nwh1} = over_white(bef2, [], 0)
        {_, mod, _Nm} = :edlin.over_word(bef3, [], 0)
        expand_function_name(mod, word, '(')

      {_, _, _} ->
        completeChar =
          case expand_function(bef1) do
            :help ->
              ','

            _ ->
              ':'
          end

        expand_module_name(word, completeChar)
    end
  end

  defp expand_function('(' ++ str) do
    case :edlin.over_word(str, [], 0) do
      {_, 'h', _} ->
        :help

      {_, 'ht', _} ->
        :help_type

      _ ->
        :module
    end
  end

  defp expand_function(_) do
    :module
  end

  defp expand_module_name('', _) do
    {:no, [], []}
  end

  defp expand_module_name(prefix, completeChar) do
    match(
      prefix,
      for {m, p, _} <- :code.all_available() do
        {:erlang.list_to_atom(m), p}
      end,
      completeChar
    )
  end

  defp expand_function_name(modStr, funcPrefix, completeChar) do
    case to_atom(modStr) do
      {:ok, mod} ->
        exports =
          case :erlang.module_loaded(mod) do
            true ->
              mod.module_info(:exports)

            false ->
              case :beam_lib.chunks(:code.which(mod), [:exports]) do
                {:ok, {^mod, [{:exports, e}]}} ->
                  e

                _ ->
                  {:no, [], []}
              end
          end

        case exports do
          {:no, [], []} ->
            {:no, [], []}

          ^exports ->
            match(funcPrefix, exports, completeChar)
        end

      :error ->
        {:no, [], []}
    end
  end

  defp to_atom(str) do
    case :erl_scan.string(str) do
      {:ok, [{:atom, _, a}], _} ->
        {:ok, a}

      _ ->
        :error
    end
  end

  defp match(prefix, alts, extra0) do
    len = :string.length(prefix)

    matches =
      :lists.sort(
        for {h, a} <- alts,
            prefix(prefix, s = flat_write(h)) do
          {s, a}
        end
      )

    case longest_common_head(
           for {n, _} <- matches do
             n
           end
         ) do
      {:partial, []} ->
        {:no, [], matches}

      {:partial, str} ->
        case :string.slice(str, len) do
          [] ->
            {:yes, [], matches}

          remain ->
            {:yes, remain, []}
        end

      {:complete, str} ->
        extra =
          case {extra0, matches} do
            {'(', [{^str, 0}]} ->
              '()'

            {_, _} ->
              extra0
          end

        {:yes, :string.slice(str, len) ++ extra, []}

      :no ->
        {:no, [], []}
    end
  end

  defp flat_write(t) when is_atom(t) do
    :lists.flatten(:io_lib.fwrite('~tw', [t]))
  end

  defp flat_write(s) do
    s
  end

  def format_matches(l) do
    {s1, dots} = format_col(:lists.sort(l), [])

    s =
      case dots do
        true ->
          {_, prefix} = longest_common_head(vals(l))
          prefixLen = :string.length(prefix)

          case prefixLen <= 3 do
            true ->
              s1

            false ->
              leadingDotsL = leading_dots(l, prefixLen)
              {s2, _} = format_col(:lists.sort(leadingDotsL), [])
              s2
          end

        false ->
          s1
      end

    ['\n' | s]
  end

  defp format_col([], _) do
    []
  end

  defp format_col(l, acc) do
    lL = 79
    format_col(l, field_width(l, lL), 0, acc, lL, false)
  end

  defp format_col(x, width, len, acc, lL, dots)
       when width + len > lL do
    format_col(x, width, 0, ['\n' | acc], lL, dots)
  end

  defp format_col([a | t], width, len, acc0, lL, dots) do
    {h0, r} = format_val(a)
    hmax = lL - length(r)

    {h, newDots} =
      case :string.length(h0) > hmax do
        true ->
          {:io_lib.format('~-*ts', [hmax - 3, h0]) ++ '...', true}

        false ->
          {h0, dots}
      end

    acc = [:io_lib.format('~-*ts', [width, h ++ r]) | acc0]
    format_col(t, width, len + width, acc, lL, newDots)
  end

  defp format_col([], _, _, acc, _LL, dots) do
    {:lists.reverse(acc, '\n'), dots}
  end

  defp format_val({h, i}) when is_integer(i) do
    {h, '/' ++ :erlang.integer_to_list(i)}
  end

  defp format_val({h, _}) do
    {h, ''}
  end

  defp format_val(h) do
    {h, ''}
  end

  defp field_width(l, lL) do
    field_width(l, 0, lL)
  end

  defp field_width([{h, _} | t], w, lL) do
    case :string.length(h) do
      l when l > w ->
        field_width(t, l, lL)

      _ ->
        field_width(t, w, lL)
    end
  end

  defp field_width([h | t], w, lL) do
    case :string.length(h) do
      l when l > w ->
        field_width(t, l, lL)

      _ ->
        field_width(t, w, lL)
    end
  end

  defp field_width([], w, lL) when w < lL - 3 do
    w + 4
  end

  defp field_width([], _, lL) do
    lL
  end

  defp vals([]) do
    []
  end

  defp vals([{s, _} | l]) do
    [s | vals(l)]
  end

  defp vals([s | l]) do
    [s | vals(l)]
  end

  defp leading_dots([], _Len) do
    []
  end

  defp leading_dots([{h, i} | l], len) do
    [{'...' ++ :string.slice(h, len), i} | leading_dots(l, len)]
  end

  defp leading_dots([h | l], len) do
    ['...' ++ :string.slice(h, len) | leading_dots(l, len)]
  end

  defp longest_common_head([]) do
    :no
  end

  defp longest_common_head(lL) do
    longest_common_head(lL, [])
  end

  defp longest_common_head([[] | _], l) do
    {:partial, reverse(l)}
  end

  defp longest_common_head(lL, l) do
    case same_head(lL) do
      true ->
        [[h | _] | _] = lL
        lL1 = all_tails(lL)

        case all_nil(lL1) do
          false ->
            longest_common_head(lL1, [h | l])

          true ->
            {:complete, reverse([h | l])}
        end

      false ->
        {:partial, reverse(l)}
    end
  end

  defp same_head([[h | _] | t1]) do
    same_head(h, t1)
  end

  defp same_head(h, [[h | _] | t]) do
    same_head(h, t)
  end

  defp same_head(_, []) do
    true
  end

  defp same_head(_, _) do
    false
  end

  defp all_tails(lL) do
    all_tails(lL, [])
  end

  defp all_tails([[_ | t] | t1], l) do
    all_tails(t1, [t | l])
  end

  defp all_tails([], l) do
    l
  end

  defp all_nil([]) do
    true
  end

  defp all_nil([[] | rest]) do
    all_nil(rest)
  end

  defp all_nil(_) do
    false
  end

  defp over_white([?\s | cs], stack, n) do
    over_white(cs, [?\s | stack], n + 1)
  end

  defp over_white([?\t | cs], stack, n) do
    over_white(cs, [?\t | stack], n + 1)
  end

  defp over_white(cs, stack, n) when is_list(cs) do
    {cs, stack, n}
  end
end
