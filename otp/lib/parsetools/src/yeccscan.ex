defmodule :m_yeccscan do
  use Bitwise

  def scan(inport) do
    scan(inport, :"", 1)
  end

  def scan(inport, prompt, line1) do
    case (try do
            :io.scan_erl_form(inport, prompt, line1)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:eof, line2} ->
        {:eof, line2}

      {:ok, tokens, line2} ->
        case tokens do
          [] ->
            scan(inport, prompt, line2)

          _ ->
            {:ok, lex(tokens), line2}
        end

      {:error, reason} ->
        {:error, reason}

      {:error, descriptor, line2} ->
        {:error, descriptor, line2}

      {:EXIT, why} ->
        :io.format(:"yeccscan: Error scanning input line ~w~n", [line1])
        exit(why)
    end
  end

  defp lex([]) do
    []
  end

  defp lex([token | tokens]) do
    case token do
      {:dot, line} ->
        [{:dot, line} | lex(tokens)]

      {:":", line} ->
        [{:":", line} | lex(tokens)]

      {:->, line} ->
        [{:->, line} | lex(tokens)]

      {category, line, symbol} ->
        [{category, line, symbol} | lex(tokens)]

      {other, line} ->
        cat =
          case :erl_scan.reserved_word(other) do
            true ->
              :reserved_word

            false ->
              :reserved_symbol
          end

        [{cat, line, other} | lex(tokens)]
    end
  end
end
