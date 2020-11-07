defmodule :m_erl_comment_scan do
  use Bitwise

  def file(name) do
    name1 = filename(name)

    case (try do
            {:ok, :file.read_file(name1)}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, v} ->
        case v do
          {:ok, b} ->
            encoding = :epp.read_encoding_from_binary(b)

            enc =
              case encoding do
                :none ->
                  :epp.default_encoding()

                enc0 ->
                  enc0
              end

            case (try do
                    :unicode.characters_to_list(b, enc)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              string when is_list(string) ->
                string(string)

              r when encoding === :none ->
                case (try do
                        :unicode.characters_to_list(b, :latin1)
                      catch
                        :error, e -> {:EXIT, {e, __STACKTRACE__}}
                        :exit, e -> {:EXIT, e}
                        e -> e
                      end) do
                  string when is_list(string) ->
                    string(string)

                  _ ->
                    error_read_file(name1)
                    exit(r)
                end

              r ->
                error_read_file(name1)
                exit(r)
            end

          {:error, e} ->
            error_read_file(name1)
            exit({:read, e})
        end

      {:EXIT, e} ->
        error_read_file(name1)
        exit(e)

      r ->
        error_read_file(name1)
        throw(r)
    end
  end

  def string(text) do
    :lists.reverse(join_lines(scan_lines(text)))
  end

  def scan_lines(text) do
    scan_lines(text, 1, 0, 0, [])
  end

  defp scan_lines([?\s | cs], l, col, m, ack) do
    scan_lines(cs, l, col + 1, m, ack)
  end

  defp scan_lines([?\t | cs], l, col, m, ack) do
    scan_lines(cs, l, tab(col), m, ack)
  end

  defp scan_lines([?\n | cs], l, _Col, _M, ack) do
    scan_lines(cs, l + 1, 0, 0, ack)
  end

  defp scan_lines([[?\r, ?\n] | cs], l, _Col, _M, ack) do
    scan_lines(cs, l + 1, 0, 0, ack)
  end

  defp scan_lines([?\r | cs], l, _Col, _M, ack) do
    scan_lines(cs, l + 1, 0, 0, ack)
  end

  defp scan_lines([?% | cs], l, col, m, ack) do
    scan_comment(cs, '', l, col, m, ack)
  end

  defp scan_lines([?$ | cs], l, col, _M, ack) do
    scan_char(cs, l, col + 1, ack)
  end

  defp scan_lines([?" | cs], l, col, _M, ack) do
    scan_string(cs, ?", l, col + 1, ack)
  end

  defp scan_lines([?' | cs], l, col, _M, ack) do
    scan_string(cs, ?', l, col + 1, ack)
  end

  defp scan_lines([_C | cs], l, col, _M, ack) do
    n = col + 1
    scan_lines(cs, l, n, n, ack)
  end

  defp scan_lines([], _L, _Col, _M, ack) do
    ack
  end

  defp tab(col) do
    col - rem(col, 8) + 8
  end

  defp scan_comment([?\n | cs], cs1, l, col, m, ack) do
    seen_comment(cs, cs1, l, col, m, ack)
  end

  defp scan_comment([[?\r, ?\n] | cs], cs1, l, col, m, ack) do
    seen_comment(cs, cs1, l, col, m, ack)
  end

  defp scan_comment([?\r | cs], cs1, l, col, m, ack) do
    seen_comment(cs, cs1, l, col, m, ack)
  end

  defp scan_comment([c | cs], cs1, l, col, m, ack) do
    scan_comment(cs, [c | cs1], l, col, m, ack)
  end

  defp scan_comment([], cs1, l, col, m, ack) do
    seen_comment([], cs1, l, col, m, ack)
  end

  defp seen_comment(cs, cs1, l, col, m, ack) do
    n = col - m
    text = :lists.reverse(:string.trim(cs1, :leading))
    ack1 = [{l, col + 1, n, text} | ack]
    scan_lines(cs, l + 1, 0, 0, ack1)
  end

  defp scan_string([quote | cs], quote, l, col, ack) do
    n = col + 1
    scan_lines(cs, l, n, n, ack)
  end

  defp scan_string([?\t | cs], quote, l, col, ack) do
    scan_string(cs, quote, l, tab(col), ack)
  end

  defp scan_string([?\n | cs], quote, l, _Col, ack) do
    scan_string(cs, quote, l + 1, 0, ack)
  end

  defp scan_string([[?\r, ?\n] | cs], quote, l, _Col, ack) do
    scan_string(cs, quote, l + 1, 0, ack)
  end

  defp scan_string([?\r | cs], quote, l, _Col, ack) do
    scan_string(cs, quote, l + 1, 0, ack)
  end

  defp scan_string([[?\\, _C] | cs], quote, l, col, ack) do
    scan_string(cs, quote, l, col + 2, ack)
  end

  defp scan_string([_C | cs], quote, l, col, ack) do
    scan_string(cs, quote, l, col + 1, ack)
  end

  defp scan_string([], _Quote, _L, _Col, ack) do
    ack
  end

  defp scan_char([?\t | cs], l, col, ack) do
    n = tab(col)
    scan_lines(cs, l, n, n, ack)
  end

  defp scan_char([?\n | cs], l, _Col, ack) do
    scan_lines(cs, l + 1, 0, 0, ack)
  end

  defp scan_char([[?\r, ?\n] | cs], l, _Col, ack) do
    scan_lines(cs, l + 1, 0, 0, ack)
  end

  defp scan_char([?\r | cs], l, _Col, ack) do
    scan_lines(cs, l + 1, 0, 0, ack)
  end

  defp scan_char([[?\\, _C] | cs], l, col, ack) do
    n = col + 2
    scan_lines(cs, l, n, n, ack)
  end

  defp scan_char([_C | cs], l, col, ack) do
    n = col + 1
    scan_lines(cs, l, n, n, ack)
  end

  defp scan_char([], _L, _Col, ack) do
    ack
  end

  def join_lines([{l, col, ind, txt} | lines]) do
    join_lines(lines, [txt], l, col, ind)
  end

  def join_lines([]) do
    []
  end

  defp join_lines([{l1, col1, ind1, txt1} | lines], txt, l, col, ind) do
    cond do
      l1 === l - 1 and col1 === col and ind + 1 === col ->
        join_lines(lines, [txt1 | txt], l1, col1, ind1)

      true ->
        [{l, col, ind, txt} | join_lines(lines, [txt1], l1, col1, ind1)]
    end
  end

  defp join_lines([], txt, l, col, ind) do
    [{l, col, ind, txt}]
  end

  defp filename([c | t]) when is_integer(c) and c > 0 do
    [c | filename(t)]
  end

  defp filename([]) do
    []
  end

  defp filename(n) do
    report_error('bad filename: `~tP\'.', [n, 25])
    exit(:error)
  end

  defp error_read_file(name) do
    report_error('error reading file `~ts\'.', [name])
  end

  defp report_error(s, vs) do
    :error_logger.error_msg(
      :lists.concat([:erl_comment_scan, ': ', s, '\n']),
      vs
    )
  end
end
