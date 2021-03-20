defmodule :m_erl_scan do
  use Bitwise
  require Record

  Record.defrecord(:r_erl_scan, :erl_scan,
    resword_fun: &reserved_word/1,
    ws: false,
    comment: false,
    text: false
  )

  def format_error({:string, quote, head}) do
    :lists.flatten([
      'unterminated ' ++
        string_thing(quote) ++
        ' starting with ' ++
        :io_lib.write_string(
          head,
          quote
        )
    ])
  end

  def format_error({:illegal, type}) do
    :lists.flatten(:io_lib.fwrite('illegal ~w', [type]))
  end

  def format_error(:char) do
    'unterminated character'
  end

  def format_error({:base, base}) do
    :lists.flatten(:io_lib.fwrite('illegal base \'~w\'', [base]))
  end

  def format_error(other) do
    :lists.flatten(:io_lib.write(other))
  end

  def string(string) do
    string(string, 1, [])
  end

  def string(string, startLocation) do
    string(string, startLocation, [])
  end

  def string(string, line, options)
      when is_list(string) and
             is_integer(line) do
    string1(string, options(options), line, :no_col, [])
  end

  def string(string, {line, column}, options)
      when is_list(string) and is_integer(line) and
             is_integer(column) and column >= 1 do
    string1(string, options(options), line, column, [])
  end

  def tokens(cont, charSpec, startLocation) do
    tokens(cont, charSpec, startLocation, [])
  end

  def tokens([], charSpec, line, options)
      when is_integer(line) do
    tokens1(charSpec, options(options), line, :no_col, [], &scan/6, [])
  end

  def tokens([], charSpec, {line, column}, options)
      when is_integer(line) and
             is_integer(column) and column >= 1 do
    tokens1(charSpec, options(options), line, column, [], &scan/6, [])
  end

  def tokens({:erl_scan_continuation, cs, col, toks, line, st, any, fun}, charSpec, _Loc, _Opts) do
    tokens1(cs ++ charSpec, st, line, col, toks, fun, any)
  end

  def continuation_location({:erl_scan_continuation, _, :no_col, _, line, _, _, _}) do
    line
  end

  def continuation_location({:erl_scan_continuation, _, col, _, line, _, _, _}) do
    {line, col}
  end

  def column(token) do
    :erl_anno.column(:erlang.element(2, token))
  end

  def end_location(token) do
    :erl_anno.end_location(:erlang.element(2, token))
  end

  def line(token) do
    :erl_anno.line(:erlang.element(2, token))
  end

  def location(token) do
    :erl_anno.location(:erlang.element(2, token))
  end

  def text(token) do
    :erl_anno.text(:erlang.element(2, token))
  end

  def category({category, _Anno}) do
    category
  end

  def category({category, _Anno, _Symbol}) do
    category
  end

  def category(t) do
    :erlang.error(:badarg, [t])
  end

  def symbol({category, _Anno}) do
    category
  end

  def symbol({_Category, _Anno, symbol}) do
    symbol
  end

  def symbol(t) do
    :erlang.error(:badarg, [t])
  end

  defp string_thing(?') do
    'atom'
  end

  defp string_thing(_) do
    'string'
  end

  defp options(opts0) when is_list(opts0) do
    opts = :lists.foldr(&expand_opt/2, [], opts0)

    [rW_fun] =
      case opts(opts, [:reserved_word_fun], []) do
        :badarg ->
          :erlang.error(:badarg, [opts0])

        r ->
          r
      end

    comment = :proplists.get_bool(:return_comments, opts)
    wS = :proplists.get_bool(:return_white_spaces, opts)
    txt = :proplists.get_bool(:text, opts)
    r_erl_scan(resword_fun: rW_fun, comment: comment, ws: wS, text: txt)
  end

  defp options(opt) do
    options([opt])
  end

  defp opts(options, [key | keys], l) do
    v =
      case :lists.keyfind(key, 1, options) do
        {:reserved_word_fun, f} when is_function(f, 1) ->
          {:ok, f}

        {^key, _} ->
          :badarg

        false ->
          {:ok, default_option(key)}
      end

    case v do
      :badarg ->
        :badarg

      {:ok, value} ->
        opts(options, keys, [value | l])
    end
  end

  defp opts(_Options, [], l) do
    :lists.reverse(l)
  end

  defp default_option(:reserved_word_fun) do
    &reserved_word/1
  end

  defp expand_opt(:return, os) do
    [:return_comments, :return_white_spaces | os]
  end

  defp expand_opt(o, os) do
    [o | os]
  end

  defp tokens1(cs, st, line, col, toks, fun, any)
       when is_list(cs) or cs === :eof do
    case fun.(cs, st, line, col, toks, any) do
      {:more, {cs0, ncol, ntoks, nline, nany, nfun}} ->
        {:more, {:erl_scan_continuation, cs0, ncol, ntoks, nline, st, nany, nfun}}

      {:ok, toks0, :eof, nline, ncol} ->
        res =
          case toks0 do
            [] ->
              {:eof, location(nline, ncol)}

            _ ->
              {:ok, :lists.reverse(toks0), location(nline, ncol)}
          end

        {:done, res, :eof}

      {:ok, toks0, rest, nline, ncol} ->
        {:done, {:ok, :lists.reverse(toks0), location(nline, ncol)}, rest}

      {{:error, _, _} = error, rest} ->
        {:done, error, rest}
    end
  end

  defp string1(cs, st, line, col, toks) do
    case scan1(cs, st, line, col, toks) do
      {:more, {cs0, ncol, ntoks, nline, any, fun}} ->
        case fun.(cs0 ++ :eof, st, nline, ncol, ntoks, any) do
          {:ok, toks1, _Rest, line2, col2} ->
            {:ok, :lists.reverse(toks1), location(line2, col2)}

          {{:error, _, _} = error, _Rest} ->
            error
        end

      {:ok, ntoks, [_ | _] = rest, nline, ncol} ->
        string1(rest, st, nline, ncol, ntoks)

      {:ok, ntoks, _, nline, ncol} ->
        {:ok, :lists.reverse(ntoks), location(nline, ncol)}

      {{:error, _, _} = error, _Rest} ->
        error
    end
  end

  defp scan(cs, st, line, col, toks, _) do
    scan1(cs, st, line, col, toks)
  end

  defp scan_atom(cs0, st, line, col, toks, ncs0) do
    case scan_name(cs0, ncs0) do
      {:more, ncs} ->
        {:more, {[], col, toks, line, ncs, &scan_atom/6}}

      {wcs, cs} ->
        case (try do
                :erlang.list_to_atom(wcs)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          name when is_atom(name) ->
            case r_erl_scan(st, :resword_fun).(name) do
              true ->
                tok2(cs, st, line, col, toks, wcs, name)

              false ->
                tok3(cs, st, line, col, toks, :atom, wcs, name)
            end

          _Error ->
            ncol = incr_column(col, length(wcs))
            scan_error({:illegal, :atom}, line, col, line, ncol, cs)
        end
    end
  end

  defp scan_variable(cs0, st, line, col, toks, ncs0) do
    case scan_name(cs0, ncs0) do
      {:more, ncs} ->
        {:more, {[], col, toks, line, ncs, &scan_variable/6}}

      {wcs, cs} ->
        case (try do
                :erlang.list_to_atom(wcs)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          name when is_atom(name) ->
            tok3(cs, st, line, col, toks, :var, wcs, name)

          _Error ->
            ncol = incr_column(col, length(wcs))
            scan_error({:illegal, :var}, line, col, line, ncol, cs)
        end
    end
  end

  defp scan_dot([?% | _] = cs, st, line, col, toks, ncs) do
    anno = anno(line, col, st, ncs)
    {:ok, [{:dot, anno} | toks], cs, line, incr_column(col, 1)}
  end

  defp scan_dot([?\n = c | cs], st, line, col, toks, ncs) do
    anno =
      anno(
        line,
        col,
        st,
        cond do
          r_erl_scan(st, :text) ->
            ncs ++ [c]

          true ->
            []
        end
      )

    {:ok, [{:dot, anno} | toks], cs, line + 1, new_column(col, 1)}
  end

  defp scan_dot([c | cs], st, line, col, toks, ncs)
       when is_integer(c) and ((c >= ?\0 and c <= ?\s) or (c >= 128 and c <= 160)) do
    anno =
      anno(
        line,
        col,
        st,
        cond do
          r_erl_scan(st, :text) ->
            ncs ++ [c]

          true ->
            []
        end
      )

    {:ok, [{:dot, anno} | toks], cs, line, incr_column(col, 2)}
  end

  defp scan_dot(:eof = cs, st, line, col, toks, ncs) do
    anno = anno(line, col, st, ncs)
    {:ok, [{:dot, anno} | toks], cs, line, incr_column(col, 1)}
  end

  defp scan_dot(cs, st, line, col, toks, ncs) do
    tok2(cs, st, line, col, toks, ncs, :., 1)
  end

  defp scan_newline([?\s | cs], st, line, col, toks) do
    scan_nl_spcs(cs, st, line, col, toks, 2)
  end

  defp scan_newline([?\t | cs], st, line, col, toks) do
    scan_nl_tabs(cs, st, line, col, toks, 2)
  end

  defp scan_newline([?\r | cs], st, line, col, toks) do
    newline_end(cs, st, line, col, toks, 2, '\n\r')
  end

  defp scan_newline([?\f | cs], st, line, col, toks) do
    newline_end(cs, st, line, col, toks, 2, '\n\f')
  end

  defp scan_newline([], _St, line, col, toks) do
    {:more, {[?\n], col, toks, line, [], &scan/6}}
  end

  defp scan_newline(cs, st, line, col, toks) do
    scan_nl_white_space(cs, st, line, col, toks, '\n')
  end

  defp scan_nl_spcs([?\s | cs], st, line, col, toks, n)
       when n < 17 do
    scan_nl_spcs(cs, st, line, col, toks, n + 1)
  end

  defp scan_nl_spcs([] = cs, _St, line, col, toks, n) do
    {:more, {cs, col, toks, line, n, &scan_nl_spcs/6}}
  end

  defp scan_nl_spcs(cs, st, line, col, toks, n) do
    newline_end(cs, st, line, col, toks, n, nl_spcs(n))
  end

  defp scan_nl_tabs([?\t | cs], st, line, col, toks, n)
       when n < 11 do
    scan_nl_tabs(cs, st, line, col, toks, n + 1)
  end

  defp scan_nl_tabs([] = cs, _St, line, col, toks, n) do
    {:more, {cs, col, toks, line, n, &scan_nl_tabs/6}}
  end

  defp scan_nl_tabs(cs, st, line, col, toks, n) do
    newline_end(cs, st, line, col, toks, n, nl_tabs(n))
  end

  defp scan_nl_white_space(
         [?\n | cs],
         r_erl_scan(text: false) = st,
         line,
         :no_col = col,
         toks0,
         ncs
       ) do
    toks = [
      {:white_space, anno(line), :lists.reverse(ncs)}
      | toks0
    ]

    scan_newline(cs, st, line + 1, col, toks)
  end

  defp scan_nl_white_space([?\n | cs], st, line, col, toks, ncs0) do
    ncs = :lists.reverse(ncs0)
    anno = anno(line, col, st, ncs)
    token = {:white_space, anno, ncs}
    scan_newline(cs, st, line + 1, new_column(col, length(ncs)), [token | toks])
  end

  defp scan_nl_white_space([c | cs], st, line, col, toks, ncs)
       when is_integer(c) and ((c >= ?\0 and c <= ?\s) or (c >= 128 and c <= 160)) do
    scan_nl_white_space(cs, st, line, col, toks, [c | ncs])
  end

  defp scan_nl_white_space([] = cs, _St, line, col, toks, ncs) do
    {:more, {cs, col, toks, line, ncs, &scan_nl_white_space/6}}
  end

  defp scan_nl_white_space(cs, r_erl_scan(text: false) = st, line, :no_col = col, toks, ncs) do
    anno = anno(line)
    scan1(cs, st, line + 1, col, [{:white_space, anno, :lists.reverse(ncs)} | toks])
  end

  defp scan_nl_white_space(cs, st, line, col, toks, ncs0) do
    ncs = :lists.reverse(ncs0)
    anno = anno(line, col, st, ncs)
    token = {:white_space, anno, ncs}
    scan1(cs, st, line + 1, new_column(col, length(ncs)), [token | toks])
  end

  defp newline_end(cs, r_erl_scan(text: false) = st, line, :no_col = col, toks, _N, ncs) do
    scan1(cs, st, line + 1, col, [{:white_space, anno(line), ncs} | toks])
  end

  defp newline_end(cs, st, line, col, toks, n, ncs) do
    anno = anno(line, col, st, ncs)
    scan1(cs, st, line + 1, new_column(col, n), [{:white_space, anno, ncs} | toks])
  end

  defp scan_spcs([?\s | cs], st, line, col, toks, n)
       when n < 16 do
    scan_spcs(cs, st, line, col, toks, n + 1)
  end

  defp scan_spcs([] = cs, _St, line, col, toks, n) do
    {:more, {cs, col, toks, line, n, &scan_spcs/6}}
  end

  defp scan_spcs(cs, st, line, col, toks, n) do
    white_space_end(cs, st, line, col, toks, n, spcs(n))
  end

  defp scan_tabs([?\t | cs], st, line, col, toks, n)
       when n < 10 do
    scan_tabs(cs, st, line, col, toks, n + 1)
  end

  defp scan_tabs([] = cs, _St, line, col, toks, n) do
    {:more, {cs, col, toks, line, n, &scan_tabs/6}}
  end

  defp scan_tabs(cs, st, line, col, toks, n) do
    white_space_end(cs, st, line, col, toks, n, tabs(n))
  end

  defp skip_white_space([?\n | cs], st, line, col, toks, _N) do
    skip_white_space(cs, st, line + 1, new_column(col, 1), toks, 0)
  end

  defp skip_white_space([c | cs], st, line, col, toks, n)
       when is_integer(c) and ((c >= ?\0 and c <= ?\s) or (c >= 128 and c <= 160)) do
    skip_white_space(cs, st, line, col, toks, n + 1)
  end

  defp skip_white_space([] = cs, _St, line, col, toks, n) do
    {:more, {cs, col, toks, line, n, &skip_white_space/6}}
  end

  defp skip_white_space(cs, st, line, col, toks, n) do
    scan1(cs, st, line, incr_column(col, n), toks)
  end

  defp scan_white_space([?\n | _] = cs, st, line, col, toks, ncs) do
    white_space_end(cs, st, line, col, toks, length(ncs), :lists.reverse(ncs))
  end

  defp scan_white_space([c | cs], st, line, col, toks, ncs)
       when is_integer(c) and ((c >= ?\0 and c <= ?\s) or (c >= 128 and c <= 160)) do
    scan_white_space(cs, st, line, col, toks, [c | ncs])
  end

  defp scan_white_space([] = cs, _St, line, col, toks, ncs) do
    {:more, {cs, col, toks, line, ncs, &scan_white_space/6}}
  end

  defp scan_white_space(cs, st, line, col, toks, ncs) do
    white_space_end(cs, st, line, col, toks, length(ncs), :lists.reverse(ncs))
  end

  defp white_space_end(cs, st, line, col, toks, n, ncs) do
    tok3(cs, st, line, col, toks, :white_space, ncs, ncs, n)
  end

  defp scan_char([?\\ | cs] = cs0, st, line, col, toks) do
    case scan_escape(cs, incr_column(col, 2)) do
      :more ->
        {:more, {[?$ | cs0], col, toks, line, [], &scan/6}}

      {:error, ncs, error, ncol} ->
        scan_error(error, line, col, line, ncol, ncs)

      {:eof, ncol} ->
        scan_error(:char, line, col, line, ncol, :eof)

      {:nl, val, str, ncs, ncol} ->
        anno =
          anno(
            line,
            col,
            st,
            cond do
              r_erl_scan(st, :text) ->
                '$\\' ++ str

              true ->
                []
            end
          )

        ntoks = [{:char, anno, val} | toks]
        scan1(ncs, st, line + 1, ncol, ntoks)

      {val, str, ncs, ncol} ->
        anno =
          anno(
            line,
            col,
            st,
            cond do
              r_erl_scan(st, :text) ->
                '$\\' ++ str

              true ->
                []
            end
          )

        ntoks = [{:char, anno, val} | toks]
        scan1(ncs, st, line, ncol, ntoks)
    end
  end

  defp scan_char([?\n = c | cs], st, line, col, toks) do
    anno =
      anno(
        line,
        col,
        st,
        cond do
          r_erl_scan(st, :text) ->
            [?$, c]

          true ->
            []
        end
      )

    scan1(cs, st, line + 1, new_column(col, 1), [{:char, anno, c} | toks])
  end

  defp scan_char([c | cs], st, line, col, toks)
       when is_integer(c) and
              ((c >= 0 and c < 55296) or (c > 57343 and c < 65534) or
                 (c > 65535 and c <= 1_114_111)) do
    anno =
      anno(
        line,
        col,
        st,
        cond do
          r_erl_scan(st, :text) ->
            [?$, c]

          true ->
            []
        end
      )

    scan1(cs, st, line, incr_column(col, 2), [{:char, anno, c} | toks])
  end

  defp scan_char([c | _Cs], _St, line, col, _Toks)
       when is_integer(c) and c >= 0 do
    scan_error({:illegal, :character}, line, col, line, incr_column(col, 1), :eof)
  end

  defp scan_char([], _St, line, col, toks) do
    {:more, {[?$], col, toks, line, [], &scan/6}}
  end

  defp scan_char(:eof, _St, line, col, _Toks) do
    scan_error(:char, line, col, line, incr_column(col, 1), :eof)
  end

  defp scan_string(cs, st, line, col, toks, {wcs, str, line0, col0}) do
    case scan_string0(cs, st, line, col, ?", str, wcs) do
      {:more, ncs, nline, ncol, nstr, nwcs} ->
        state = {nwcs, nstr, line0, col0}
        {:more, {ncs, ncol, toks, nline, state, &scan_string/6}}

      {:char_error, ncs, error, nline, ncol, endCol} ->
        scan_error(error, nline, ncol, nline, endCol, ncs)

      {:error, nline, ncol, nwcs, ncs} ->
        estr = :string.slice(nwcs, 0, 16)
        scan_error({:string, ?", estr}, line0, col0, nline, ncol, ncs)

      {ncs, nline, ncol, nstr, nwcs} ->
        anno = anno(line0, col0, st, nstr)
        scan1(ncs, st, nline, ncol, [{:string, anno, nwcs} | toks])
    end
  end

  defp scan_qatom(cs, st, line, col, toks, {wcs, str, line0, col0}) do
    case scan_string0(cs, st, line, col, ?', str, wcs) do
      {:more, ncs, nline, ncol, nstr, nwcs} ->
        state = {nwcs, nstr, line0, col0}
        {:more, {ncs, ncol, toks, nline, state, &scan_qatom/6}}

      {:char_error, ncs, error, nline, ncol, endCol} ->
        scan_error(error, nline, ncol, nline, endCol, ncs)

      {:error, nline, ncol, nwcs, ncs} ->
        estr = :string.slice(nwcs, 0, 16)
        scan_error({:string, ?', estr}, line0, col0, nline, ncol, ncs)

      {ncs, nline, ncol, nstr, nwcs} ->
        case (try do
                :erlang.list_to_atom(nwcs)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          a when is_atom(a) ->
            anno = anno(line0, col0, st, nstr)
            scan1(ncs, st, nline, ncol, [{:atom, anno, a} | toks])

          _ ->
            scan_error({:illegal, :atom}, line0, col0, nline, ncol, ncs)
        end
    end
  end

  defp scan_string0(cs, r_erl_scan(text: false), line, :no_col = col, q, [], wcs) do
    scan_string_no_col(cs, line, col, q, wcs)
  end

  defp scan_string0(cs, r_erl_scan(text: true), line, :no_col = col, q, str, wcs) do
    scan_string1(cs, line, col, q, str, wcs)
  end

  defp scan_string0(cs, st, line, col, q, [], wcs) do
    scan_string_col(cs, st, line, col, q, wcs)
  end

  defp scan_string0(cs, _St, line, col, q, str, wcs) do
    scan_string1(cs, line, col, q, str, wcs)
  end

  defp scan_string_no_col([q | cs], line, col, q, wcs) do
    {cs, line, col, _DontCare = [], :lists.reverse(wcs)}
  end

  defp scan_string_no_col([?\n = c | cs], line, col, q, wcs) do
    scan_string_no_col(cs, line + 1, col, q, [c | wcs])
  end

  defp scan_string_no_col([c | cs], line, col, q, wcs)
       when c !== ?\\ and
              is_integer(c) and
              ((c >= 0 and c < 55296) or (c > 57343 and c < 65534) or
                 (c > 65535 and c <= 1_114_111)) do
    scan_string_no_col(cs, line, col, q, [c | wcs])
  end

  defp scan_string_no_col(cs, line, col, q, wcs) do
    scan_string1(cs, line, col, q, wcs, wcs)
  end

  defp scan_string_col([q | cs], st, line, col, q, wcs0) do
    wcs = :lists.reverse(wcs0)

    str =
      cond do
        r_erl_scan(st, :text) ->
          [q | wcs ++ [q]]

        true ->
          []
      end

    {cs, line, col + 1, str, wcs}
  end

  defp scan_string_col([?\n = c | cs], st, line, _xCol, q, wcs) do
    scan_string_col(cs, st, line + 1, 1, q, [c | wcs])
  end

  defp scan_string_col([c | cs], st, line, col, q, wcs)
       when c !== ?\\ and
              is_integer(c) and
              ((c >= 0 and c < 55296) or (c > 57343 and c < 65534) or
                 (c > 65535 and c <= 1_114_111)) do
    scan_string_col(cs, st, line, col + 1, q, [c | wcs])
  end

  defp scan_string_col(cs, _St, line, col, q, wcs) do
    scan_string1(cs, line, col, q, wcs, wcs)
  end

  defp scan_string1([q | cs], line, col, q, str0, wcs0) do
    wcs = :lists.reverse(wcs0)
    str = [q | :lists.reverse(str0, [q])]
    {cs, line, incr_column(col, 1), str, wcs}
  end

  defp scan_string1([?\n = c | cs], line, col, q, str, wcs) do
    ncol = new_column(col, 1)
    scan_string1(cs, line + 1, ncol, q, [c | str], [c | wcs])
  end

  defp scan_string1([?\\ | cs] = cs0, line, col, q, str, wcs) do
    case scan_escape(cs, col) do
      :more ->
        {:more, cs0, line, col, str, wcs}

      {:error, ncs, error, ncol} ->
        {:char_error, ncs, error, line, col, incr_column(ncol, 1)}

      {:eof, ncol} ->
        {:error, line, incr_column(ncol, 1), :lists.reverse(wcs), :eof}

      {:nl, val, valStr, ncs, ncol} ->
        nstr = :lists.reverse(valStr, [?\\ | str])
        nwcs = [val | wcs]
        scan_string1(ncs, line + 1, ncol, q, nstr, nwcs)

      {val, valStr, ncs, ncol} ->
        nstr = :lists.reverse(valStr, [?\\ | str])
        nwcs = [val | wcs]
        scan_string1(ncs, line, incr_column(ncol, 1), q, nstr, nwcs)
    end
  end

  defp scan_string1([c | cs], line, :no_col = col, q, str, wcs)
       when is_integer(c) and
              ((c >= 0 and c < 55296) or (c > 57343 and c < 65534) or
                 (c > 65535 and c <= 1_114_111)) do
    scan_string1(cs, line, col, q, [c | str], [c | wcs])
  end

  defp scan_string1([c | cs], line, col, q, str, wcs)
       when is_integer(c) and
              ((c >= 0 and c < 55296) or (c > 57343 and c < 65534) or
                 (c > 65535 and c <= 1_114_111)) do
    scan_string1(cs, line, col + 1, q, [c | str], [c | wcs])
  end

  defp scan_string1([c | cs], line, col, _Q, _Str, _Wcs)
       when is_integer(c) and c >= 0 do
    {:char_error, cs, {:illegal, :character}, line, col, incr_column(col, 1)}
  end

  defp scan_string1([] = cs, line, col, _Q, str, wcs) do
    {:more, cs, line, col, str, wcs}
  end

  defp scan_string1(:eof, line, col, _Q, _Str, wcs) do
    {:error, line, col, :lists.reverse(wcs), :eof}
  end

  defp scan_escape([o1, o2, o3 | cs], col)
       when o1 >= ?0 and
              o1 <= ?7 and o2 >= ?0 and o2 <= ?7 and
              o3 >= ?0 and o3 <= ?7 do
    val = (o1 * 8 + o2) * 8 + o3 - 73 * ?0
    {val, [o1, o2, o3], cs, incr_column(col, 3)}
  end

  defp scan_escape([o1, o2], _Col)
       when o1 >= ?0 and o1 <= ?7 and
              o2 >= ?0 and o2 <= ?7 do
    :more
  end

  defp scan_escape([o1, o2 | cs], col)
       when o1 >= ?0 and
              o1 <= ?7 and o2 >= ?0 and o2 <= ?7 do
    val = o1 * 8 + o2 - 9 * ?0
    {val, [o1, o2], cs, incr_column(col, 2)}
  end

  defp scan_escape([o1], _Col) when o1 >= ?0 and o1 <= ?7 do
    :more
  end

  defp scan_escape([o1 | cs], col) when o1 >= ?0 and o1 <= ?7 do
    {o1 - ?0, [o1], cs, incr_column(col, 1)}
  end

  defp scan_escape([?x, ?{ | cs], col) do
    scan_hex(cs, incr_column(col, 2), [])
  end

  defp scan_escape([?x], _Col) do
    :more
  end

  defp scan_escape([?x | :eof], col) do
    {:eof, incr_column(col, 1)}
  end

  defp scan_escape([?x, h1, h2 | cs], col)
       when (h1 >= ?0 and h1 <= ?9) or (h1 >= ?A and h1 <= ?F) or
              (h1 >= ?a and h1 <= ?f and
                 h2 >= ?0 and h2 <= ?9) or (h2 >= ?A and h2 <= ?F) or (h2 >= ?a and h2 <= ?f) do
    val = :erlang.list_to_integer([h1, h2], 16)
    {val, [?x, h1, h2], cs, incr_column(col, 3)}
  end

  defp scan_escape([?x, h1], _Col)
       when (h1 >= ?0 and h1 <= ?9) or (h1 >= ?A and h1 <= ?F) or (h1 >= ?a and h1 <= ?f) do
    :more
  end

  defp scan_escape([?x | cs], col) do
    {:error, cs, {:illegal, :character}, incr_column(col, 1)}
  end

  defp scan_escape([?^ = c0, ?\n = c | cs], col) do
    {:nl, c, [c0, c], cs, new_column(col, 1)}
  end

  defp scan_escape([?^ = c0, c | cs], col)
       when is_integer(c) and
              c >= 0 do
    val = c &&& 31
    {val, [c0, c], cs, incr_column(col, 2)}
  end

  defp scan_escape([?^], _Col) do
    :more
  end

  defp scan_escape([?^ | :eof], col) do
    {:eof, incr_column(col, 1)}
  end

  defp scan_escape([?\n = c | cs], col) do
    {:nl, c, [c], cs, new_column(col, 1)}
  end

  defp scan_escape([c0 | cs], col)
       when is_integer(c0) and
              ((c0 >= 0 and c0 < 55296) or (c0 > 57343 and c0 < 65534) or
                 (c0 > 65535 and c0 <= 1_114_111)) do
    c = escape_char(c0)
    {c, [c0], cs, incr_column(col, 1)}
  end

  defp scan_escape([c | cs], col)
       when is_integer(c) and
              c >= 0 do
    {:error, cs, {:illegal, :character}, incr_column(col, 1)}
  end

  defp scan_escape([], _Col) do
    :more
  end

  defp scan_escape(:eof, col) do
    {:eof, col}
  end

  defp scan_hex([c | cs], :no_col = col, wcs)
       when (c >= ?0 and c <= ?9) or (c >= ?A and c <= ?F) or (c >= ?a and c <= ?f) do
    scan_hex(cs, col, [c | wcs])
  end

  defp scan_hex([c | cs], col, wcs)
       when (c >= ?0 and c <= ?9) or (c >= ?A and c <= ?F) or (c >= ?a and c <= ?f) do
    scan_hex(cs, col + 1, [c | wcs])
  end

  defp scan_hex(cs, col, wcs) do
    scan_esc_end(cs, col, wcs, 16, 'x{')
  end

  defp scan_esc_end([?} | cs], col, wcs0, b, str0) do
    wcs = :lists.reverse(wcs0)

    case (try do
            :erlang.list_to_integer(wcs, b)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      val
      when is_integer(val) and
             ((val >= 0 and val < 55296) or (val > 57343 and val < 65534) or
                (val > 65535 and val <= 1_114_111)) ->
        {val, str0 ++ wcs ++ [?}], cs, incr_column(col, 1)}

      _ ->
        {:error, cs, {:illegal, :character}, incr_column(col, 1)}
    end
  end

  defp scan_esc_end([], _Col, _Wcs, _B, _Str0) do
    :more
  end

  defp scan_esc_end(:eof, col, _Wcs, _B, _Str0) do
    {:eof, col}
  end

  defp scan_esc_end(cs, col, _Wcs, _B, _Str0) do
    {:error, cs, {:illegal, :character}, col}
  end

  defp escape_char(?n) do
    ?\n
  end

  defp escape_char(?r) do
    ?\r
  end

  defp escape_char(?t) do
    ?\t
  end

  defp escape_char(?v) do
    ?\v
  end

  defp escape_char(?b) do
    ?\b
  end

  defp escape_char(?f) do
    ?\f
  end

  defp escape_char(?e) do
    ?\e
  end

  defp escape_char(?s) do
    ?\s
  end

  defp escape_char(?d) do
    ?\d
  end

  defp escape_char(c) do
    c
  end

  defp scan_number(cs, st, line, col, toks, {ncs, us}) do
    scan_number(cs, st, line, col, toks, ncs, us)
  end

  defp scan_number([c | cs], st, line, col, toks, ncs, us)
       when c >= ?0 and c <= ?9 do
    scan_number(cs, st, line, col, toks, [c | ncs], us)
  end

  defp scan_number([?_, next | cs], st, line, col, toks, [prev | _] = ncs, _Us)
       when next >= ?0 and next <= ?9 and prev >= ?0 and prev <= ?9 do
    scan_number(cs, st, line, col, toks, [next, ?_ | ncs], :with_underscore)
  end

  defp scan_number([?_] = cs, _St, line, col, toks, ncs, us) do
    {:more, {cs, col, toks, line, {ncs, us}, &scan_number/6}}
  end

  defp scan_number([?., c | cs], st, line, col, toks, ncs, us)
       when c >= ?0 and c <= ?9 do
    scan_fraction(cs, st, line, col, toks, [c, ?. | ncs], us)
  end

  defp scan_number([?.] = cs, _St, line, col, toks, ncs, us) do
    {:more, {cs, col, toks, line, {ncs, us}, &scan_number/6}}
  end

  defp scan_number([?# | cs] = cs0, st, line, col, toks, ncs0, us) do
    ncs = :lists.reverse(ncs0)

    case (try do
            :erlang.list_to_integer(
              remove_digit_separators(
                ncs,
                us
              )
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      b when b >= 2 and b <= 1 + ?Z - ?A + 10 ->
        bcs = ncs ++ [?#]
        scan_based_int(cs, st, line, col, toks, b, [], bcs, :no_underscore)

      b ->
        len = length(ncs)
        scan_error({:base, b}, line, col, line, incr_column(col, len), cs0)
    end
  end

  defp scan_number([] = cs, _St, line, col, toks, ncs, us) do
    {:more, {cs, col, toks, line, {ncs, us}, &scan_number/6}}
  end

  defp scan_number(cs, st, line, col, toks, ncs0, us) do
    ncs = :lists.reverse(ncs0)

    case (try do
            :erlang.list_to_integer(
              remove_digit_separators(
                ncs,
                us
              )
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      n when is_integer(n) ->
        tok3(cs, st, line, col, toks, :integer, ncs, n)

      _ ->
        ncol = incr_column(col, length(ncs))
        scan_error({:illegal, :integer}, line, col, line, ncol, cs)
    end
  end

  defp remove_digit_separators(number, :no_underscore) do
    number
  end

  defp remove_digit_separators(number, :with_underscore) do
    for c <- number, c !== ?_ do
      c
    end
  end

  defp scan_based_int(cs, st, line, col, toks, {b, nCs, bCs, us}) do
    scan_based_int(cs, st, line, col, toks, b, nCs, bCs, us)
  end

  defp scan_based_int([c | cs], st, line, col, toks, b, ncs, bcs, us)
       when (c >= ?0 and c <= ?9 and c < ?0 + b) or (c >= ?A and b > 10 and c < ?A + b - 10) or
              (c >= ?a and b > 10 and c < ?a + b - 10) do
    scan_based_int(cs, st, line, col, toks, b, [c | ncs], bcs, us)
  end

  defp scan_based_int([?_, next | cs], st, line, col, toks, b, [prev | _] = ncs, bcs, _Us)
       when ((next >= ?0 and next <= ?9 and next < ?0 + b) or
               (next >= ?A and b > 10 and next < ?A + b - 10) or
               (next >= ?a and b > 10 and next < ?a + b - 10)) and
              ((prev >= ?0 and prev <= ?9 and prev < ?0 + b) or
                 (prev >= ?A and b > 10 and prev < ?A + b - 10) or
                 (prev >= ?a and b > 10 and prev < ?a + b - 10)) do
    scan_based_int(cs, st, line, col, toks, b, [next, ?_ | ncs], bcs, :with_underscore)
  end

  defp scan_based_int([?_] = cs, _St, line, col, toks, b, nCs, bCs, us) do
    {:more, {cs, col, toks, line, {b, nCs, bCs, us}, &scan_based_int/6}}
  end

  defp scan_based_int([] = cs, _St, line, col, toks, b, nCs, bCs, us) do
    {:more, {cs, col, toks, line, {b, nCs, bCs, us}, &scan_based_int/6}}
  end

  defp scan_based_int(cs, st, line, col, toks, b, ncs0, bcs, us) do
    ncs = :lists.reverse(ncs0)

    case (try do
            :erlang.list_to_integer(
              remove_digit_separators(
                ncs,
                us
              ),
              b
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      n when is_integer(n) ->
        tok3(cs, st, line, col, toks, :integer, bcs ++ ncs, n)

      _ ->
        len = length(bcs) + length(ncs)
        ncol = incr_column(col, len)
        scan_error({:illegal, :integer}, line, col, line, ncol, cs)
    end
  end

  defp scan_fraction(cs, st, line, col, toks, {ncs, us}) do
    scan_fraction(cs, st, line, col, toks, ncs, us)
  end

  defp scan_fraction([c | cs], st, line, col, toks, ncs, us)
       when c >= ?0 and c <= ?9 do
    scan_fraction(cs, st, line, col, toks, [c | ncs], us)
  end

  defp scan_fraction([?_, next | cs], st, line, col, toks, [prev | _] = ncs, _Us)
       when next >= ?0 and next <= ?9 and prev >= ?0 and prev <= ?9 do
    scan_fraction(cs, st, line, col, toks, [next, ?_ | ncs], :with_underscore)
  end

  defp scan_fraction([?_] = cs, _St, line, col, toks, ncs, us) do
    {:more, {cs, col, toks, line, {ncs, us}, &scan_fraction/6}}
  end

  defp scan_fraction([e | cs], st, line, col, toks, ncs, us)
       when e === ?e or e === ?E do
    scan_exponent_sign(cs, st, line, col, toks, [e | ncs], us)
  end

  defp scan_fraction([] = cs, _St, line, col, toks, ncs, us) do
    {:more, {cs, col, toks, line, {ncs, us}, &scan_fraction/6}}
  end

  defp scan_fraction(cs, st, line, col, toks, ncs, us) do
    float_end(cs, st, line, col, toks, ncs, us)
  end

  defp scan_exponent_sign(cs, st, line, col, toks, {ncs, us}) do
    scan_exponent_sign(cs, st, line, col, toks, ncs, us)
  end

  defp scan_exponent_sign([c | cs], st, line, col, toks, ncs, us)
       when c === ?+ or c === ?- do
    scan_exponent(cs, st, line, col, toks, [c | ncs], us)
  end

  defp scan_exponent_sign([] = cs, _St, line, col, toks, ncs, us) do
    {:more, {cs, col, toks, line, {ncs, us}, &scan_exponent_sign/6}}
  end

  defp scan_exponent_sign(cs, st, line, col, toks, ncs, us) do
    scan_exponent(cs, st, line, col, toks, ncs, us)
  end

  defp scan_exponent(cs, st, line, col, toks, {ncs, us}) do
    scan_exponent(cs, st, line, col, toks, ncs, us)
  end

  defp scan_exponent([c | cs], st, line, col, toks, ncs, us)
       when c >= ?0 and c <= ?9 do
    scan_exponent(cs, st, line, col, toks, [c | ncs], us)
  end

  defp scan_exponent([?_, next | cs], st, line, col, toks, [prev | _] = ncs, _)
       when next >= ?0 and next <= ?9 and prev >= ?0 and prev <= ?9 do
    scan_exponent(cs, st, line, col, toks, [next, ?_ | ncs], :with_underscore)
  end

  defp scan_exponent([?_] = cs, _St, line, col, toks, ncs, us) do
    {:more, {cs, col, toks, line, {ncs, us}, &scan_exponent/6}}
  end

  defp scan_exponent([] = cs, _St, line, col, toks, ncs, us) do
    {:more, {cs, col, toks, line, {ncs, us}, &scan_exponent/6}}
  end

  defp scan_exponent(cs, st, line, col, toks, ncs, us) do
    float_end(cs, st, line, col, toks, ncs, us)
  end

  defp float_end(cs, st, line, col, toks, ncs0, us) do
    ncs = :lists.reverse(ncs0)

    case (try do
            :erlang.list_to_float(remove_digit_separators(ncs, us))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      f when is_float(f) ->
        tok3(cs, st, line, col, toks, :float, ncs, f)

      _ ->
        ncol = incr_column(col, length(ncs))
        scan_error({:illegal, :float}, line, col, line, ncol, cs)
    end
  end

  defp skip_comment([c | cs], st, line, col, toks, n)
       when c !== ?\n and is_integer(c) and c >= 0 do
    case is_integer(c) and
           ((c >= 0 and c < 55296) or (c > 57343 and c < 65534) or (c > 65535 and c <= 1_114_111)) do
      true ->
        skip_comment(cs, st, line, col, toks, n + 1)

      false ->
        ncol = incr_column(col, n + 1)
        scan_error({:illegal, :character}, line, col, line, ncol, cs)
    end
  end

  defp skip_comment([] = cs, _St, line, col, toks, n) do
    {:more, {cs, col, toks, line, n, &skip_comment/6}}
  end

  defp skip_comment(cs, st, line, col, toks, n) do
    scan1(cs, st, line, incr_column(col, n), toks)
  end

  defp scan_comment([c | cs], st, line, col, toks, ncs)
       when c !== ?\n and is_integer(c) and c >= 0 do
    case is_integer(c) and
           ((c >= 0 and c < 55296) or (c > 57343 and c < 65534) or (c > 65535 and c <= 1_114_111)) do
      true ->
        scan_comment(cs, st, line, col, toks, [c | ncs])

      false ->
        ncol = incr_column(col, length(ncs) + 1)
        scan_error({:illegal, :character}, line, col, line, ncol, cs)
    end
  end

  defp scan_comment([] = cs, _St, line, col, toks, ncs) do
    {:more, {cs, col, toks, line, ncs, &scan_comment/6}}
  end

  defp scan_comment(cs, st, line, col, toks, ncs0) do
    ncs = :lists.reverse(ncs0)
    tok3(cs, st, line, col, toks, :comment, ncs, ncs)
  end

  defp tok2(cs, r_erl_scan(text: false) = st, line, :no_col = col, toks, _Wcs, p) do
    scan1(cs, st, line, col, [{p, anno(line)} | toks])
  end

  defp tok2(cs, st, line, col, toks, wcs, p) do
    anno = anno(line, col, st, wcs)
    scan1(cs, st, line, incr_column(col, length(wcs)), [{p, anno} | toks])
  end

  defp tok2(cs, r_erl_scan(text: false) = st, line, :no_col = col, toks, _Wcs, p, _N) do
    scan1(cs, st, line, col, [{p, anno(line)} | toks])
  end

  defp tok2(cs, st, line, col, toks, wcs, p, n) do
    anno = anno(line, col, st, wcs)
    scan1(cs, st, line, incr_column(col, n), [{p, anno} | toks])
  end

  defp tok3(cs, r_erl_scan(text: false) = st, line, :no_col = col, toks, item, _S, sym) do
    scan1(cs, st, line, col, [{item, anno(line), sym} | toks])
  end

  defp tok3(cs, st, line, col, toks, item, string, sym) do
    token = {item, anno(line, col, st, string), sym}
    scan1(cs, st, line, incr_column(col, length(string)), [token | toks])
  end

  defp tok3(
         cs,
         r_erl_scan(text: false) = st,
         line,
         :no_col = col,
         toks,
         item,
         _String,
         sym,
         _Length
       ) do
    scan1(cs, st, line, col, [{item, anno(line), sym} | toks])
  end

  defp tok3(cs, st, line, col, toks, item, string, sym, length) do
    token = {item, anno(line, col, st, string), sym}
    scan1(cs, st, line, incr_column(col, length), [token | toks])
  end

  defp scan_error(error, line, col, endLine, endCol, rest) do
    loc = location(line, col)
    endLoc = location(endLine, endCol)
    scan_error(error, loc, endLoc, rest)
  end

  defp scan_error(error, errorLoc, endLoc, rest) do
    {{:error, {errorLoc, :erl_scan, error}, endLoc}, rest}
  end

  defp anno(line, :no_col, r_erl_scan(text: false), _String) do
    anno(line)
  end

  defp anno(line, :no_col, r_erl_scan(text: true), string) do
    anno = anno(line)
    :erl_anno.set_text(string, anno)
  end

  defp anno(line, col, r_erl_scan(text: false), _String) do
    anno({line, col})
  end

  defp anno(line, col, r_erl_scan(text: true), string) do
    anno = anno({line, col})
    :erl_anno.set_text(string, anno)
  end

  defp location(line, :no_col) do
    line
  end

  defp location(line, col) when is_integer(col) do
    {line, col}
  end

  defp anno(location) do
    :erl_anno.new(location)
  end

  defp incr_column(:no_col = col, _N) do
    col
  end

  defp incr_column(col, n) when is_integer(col) do
    col + n
  end

  defp new_column(:no_col = col, _Ncol) do
    col
  end

  defp new_column(col, ncol) when is_integer(col) do
    ncol
  end

  defp nl_spcs(2) do
    '\n '
  end

  defp nl_spcs(3) do
    '\n  '
  end

  defp nl_spcs(4) do
    '\n   '
  end

  defp nl_spcs(5) do
    '\n    '
  end

  defp nl_spcs(6) do
    '\n     '
  end

  defp nl_spcs(7) do
    '\n      '
  end

  defp nl_spcs(8) do
    '\n       '
  end

  defp nl_spcs(9) do
    '\n        '
  end

  defp nl_spcs(10) do
    '\n         '
  end

  defp nl_spcs(11) do
    '\n          '
  end

  defp nl_spcs(12) do
    '\n           '
  end

  defp nl_spcs(13) do
    '\n            '
  end

  defp nl_spcs(14) do
    '\n             '
  end

  defp nl_spcs(15) do
    '\n              '
  end

  defp nl_spcs(16) do
    '\n               '
  end

  defp nl_spcs(17) do
    '\n                '
  end

  defp spcs(1) do
    ' '
  end

  defp spcs(2) do
    '  '
  end

  defp spcs(3) do
    '   '
  end

  defp spcs(4) do
    '    '
  end

  defp spcs(5) do
    '     '
  end

  defp spcs(6) do
    '      '
  end

  defp spcs(7) do
    '       '
  end

  defp spcs(8) do
    '        '
  end

  defp spcs(9) do
    '         '
  end

  defp spcs(10) do
    '          '
  end

  defp spcs(11) do
    '           '
  end

  defp spcs(12) do
    '            '
  end

  defp spcs(13) do
    '             '
  end

  defp spcs(14) do
    '              '
  end

  defp spcs(15) do
    '               '
  end

  defp spcs(16) do
    '                '
  end

  defp nl_tabs(2) do
    '\n\t'
  end

  defp nl_tabs(3) do
    '\n\t\t'
  end

  defp nl_tabs(4) do
    '\n\t\t\t'
  end

  defp nl_tabs(5) do
    '\n\t\t\t\t'
  end

  defp nl_tabs(6) do
    '\n\t\t\t\t\t'
  end

  defp nl_tabs(7) do
    '\n\t\t\t\t\t\t'
  end

  defp nl_tabs(8) do
    '\n\t\t\t\t\t\t\t'
  end

  defp nl_tabs(9) do
    '\n\t\t\t\t\t\t\t\t'
  end

  defp nl_tabs(10) do
    '\n\t\t\t\t\t\t\t\t\t'
  end

  defp nl_tabs(11) do
    '\n\t\t\t\t\t\t\t\t\t\t'
  end

  defp tabs(1) do
    '\t'
  end

  defp tabs(2) do
    '\t\t'
  end

  defp tabs(3) do
    '\t\t\t'
  end

  defp tabs(4) do
    '\t\t\t\t'
  end

  defp tabs(5) do
    '\t\t\t\t\t'
  end

  defp tabs(6) do
    '\t\t\t\t\t\t'
  end

  defp tabs(7) do
    '\t\t\t\t\t\t\t'
  end

  defp tabs(8) do
    '\t\t\t\t\t\t\t\t'
  end

  defp tabs(9) do
    '\t\t\t\t\t\t\t\t\t'
  end

  defp tabs(10) do
    '\t\t\t\t\t\t\t\t\t\t'
  end

  def reserved_word(:after) do
    true
  end

  def reserved_word(:begin) do
    true
  end

  def reserved_word(:case) do
    true
  end

  def reserved_word(:try) do
    true
  end

  def reserved_word(:cond) do
    true
  end

  def reserved_word(:catch) do
    true
  end

  def reserved_word(:andalso) do
    true
  end

  def reserved_word(:orelse) do
    true
  end

  def reserved_word(:end) do
    true
  end

  def reserved_word(:fun) do
    true
  end

  def reserved_word(:if) do
    true
  end

  def reserved_word(:let) do
    true
  end

  def reserved_word(:of) do
    true
  end

  def reserved_word(:receive) do
    true
  end

  def reserved_word(:when) do
    true
  end

  def reserved_word(:bnot) do
    true
  end

  def reserved_word(:not) do
    true
  end

  def reserved_word(:div) do
    true
  end

  def reserved_word(:rem) do
    true
  end

  def reserved_word(:band) do
    true
  end

  def reserved_word(:and) do
    true
  end

  def reserved_word(:bor) do
    true
  end

  def reserved_word(:bxor) do
    true
  end

  def reserved_word(:bsl) do
    true
  end

  def reserved_word(:bsr) do
    true
  end

  def reserved_word(:or) do
    true
  end

  def reserved_word(:xor) do
    true
  end

  def reserved_word(_) do
    false
  end
end
