defmodule :m_io do
  use Bitwise

  defp to_tuple(t) when is_tuple(t) do
    t
  end

  defp to_tuple(t) do
    {t}
  end

  defp o_request(io, request, func) do
    case request(io, request) do
      {:error, reason} ->
        [
          _Name
          | args
        ] = :erlang.tuple_to_list(to_tuple(request))

        {:EXIT, {:get_stacktrace, [_Current | mfas]}} =
          try do
            :erlang.error(:get_stacktrace)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        :erlang.raise(:error, conv_reason(func, reason), [{:io, func, [io | args]} | mfas])

      other ->
        other
    end
  end

  def printable_range() do
    :erlang.nif_error(:undefined)
  end

  def put_chars(chars) do
    put_chars(default_output(), chars)
  end

  def put_chars(io, chars) do
    put_chars(io, :unicode, chars)
  end

  defp put_chars(io, encoding, chars) do
    o_request(io, {:put_chars, encoding, chars}, :put_chars)
  end

  def nl() do
    nl(default_output())
  end

  def nl(io) do
    o_request(io, :nl, :nl)
  end

  def columns() do
    columns(default_output())
  end

  def columns(io) do
    case request(io, {:get_geometry, :columns}) do
      n when is_integer(n) and n > 0 ->
        {:ok, n}

      _ ->
        {:error, :enotsup}
    end
  end

  def rows() do
    rows(default_output())
  end

  def rows(io) do
    case request(io, {:get_geometry, :rows}) do
      n when is_integer(n) and n > 0 ->
        {:ok, n}

      _ ->
        {:error, :enotsup}
    end
  end

  def get_chars(prompt, n) do
    get_chars(default_input(), prompt, n)
  end

  def get_chars(io, prompt, n) when is_integer(n) and n >= 0 do
    request(io, {:get_chars, :unicode, prompt, n})
  end

  def get_line(prompt) do
    get_line(default_input(), prompt)
  end

  def get_line(io, prompt) do
    request(io, {:get_line, :unicode, prompt})
  end

  def get_password() do
    get_password(default_input())
  end

  def get_password(io) do
    request(io, {:get_password, :unicode})
  end

  def getopts() do
    getopts(default_input())
  end

  def getopts(io) do
    request(io, :getopts)
  end

  def setopts(opts) do
    setopts(default_input(), opts)
  end

  def setopts(io, opts) do
    request(io, {:setopts, opts})
  end

  def write(term) do
    write(default_output(), term)
  end

  def write(io, term) do
    o_request(io, {:write, term}, :write)
  end

  def read(prompt) do
    read(default_input(), prompt)
  end

  def read(io, prompt) do
    case request(
           io,
           {:get_until, :unicode, prompt, :erl_scan, :tokens, [1]}
         ) do
      {:ok, toks, _EndLine} ->
        :erl_parse.parse_term(toks)

      {:error, e, _EndLine} ->
        {:error, e}

      {:eof, _EndLine} ->
        :eof

      other ->
        other
    end
  end

  def read(io, prompt, pos0) do
    read(io, prompt, pos0, [])
  end

  def read(io, prompt, pos0, options) do
    args = [pos0, options]

    case request(
           io,
           {:get_until, :unicode, prompt, :erl_scan, :tokens, args}
         ) do
      {:ok, toks, endLocation} ->
        case :erl_parse.parse_term(toks) do
          {:ok, term} ->
            {:ok, term, endLocation}

          {:error, errorInfo} ->
            {:error, errorInfo, endLocation}
        end

      {:error, _E, _EndLocation} = error ->
        error

      {:eof, _EndLocation} = eof ->
        eof

      other ->
        other
    end
  end

  defp conv_reason(_, :arguments) do
    :badarg
  end

  defp conv_reason(_, :terminated) do
    :terminated
  end

  defp conv_reason(_, {:no_translation, _, _}) do
    :no_translation
  end

  defp conv_reason(_, _Reason) do
    :badarg
  end

  def fwrite(format) do
    format(format)
  end

  def fwrite(format, args) do
    format(format, args)
  end

  def fwrite(io, format, args) do
    format(io, format, args)
  end

  def fread(prompt, format) do
    fread(default_input(), prompt, format)
  end

  def fread(io, prompt, format) do
    request(io, {:fread, prompt, format})
  end

  def format(format) do
    format(format, [])
  end

  def format(format, args) do
    format(default_output(), format, args)
  end

  def format(io, format, args) do
    o_request(io, {:format, format, args}, :format)
  end

  def scan_erl_exprs(prompt) do
    scan_erl_exprs(default_input(), prompt, 1)
  end

  def scan_erl_exprs(io, prompt) do
    scan_erl_exprs(io, prompt, 1)
  end

  def scan_erl_exprs(io, prompt, pos0) do
    scan_erl_exprs(io, prompt, pos0, [])
  end

  def scan_erl_exprs(io, prompt, pos0, options) do
    request(
      io,
      {:get_until, :unicode, prompt, :erl_scan, :tokens, [pos0, options]}
    )
  end

  def scan_erl_form(prompt) do
    scan_erl_form(default_input(), prompt, 1)
  end

  def scan_erl_form(io, prompt) do
    scan_erl_form(io, prompt, 1)
  end

  def scan_erl_form(io, prompt, pos0) do
    scan_erl_form(io, prompt, pos0, [])
  end

  def scan_erl_form(io, prompt, pos0, options) do
    request(
      io,
      {:get_until, :unicode, prompt, :erl_scan, :tokens, [pos0, options]}
    )
  end

  def parse_erl_exprs(prompt) do
    parse_erl_exprs(default_input(), prompt, 1)
  end

  def parse_erl_exprs(io, prompt) do
    parse_erl_exprs(io, prompt, 1)
  end

  def parse_erl_exprs(io, prompt, pos0) do
    parse_erl_exprs(io, prompt, pos0, [])
  end

  def parse_erl_exprs(io, prompt, pos0, options) do
    case request(
           io,
           {:get_until, :unicode, prompt, :erl_scan, :tokens, [pos0, options]}
         ) do
      {:ok, toks, endPos} ->
        case :erl_parse.parse_exprs(toks) do
          {:ok, exprs} ->
            {:ok, exprs, endPos}

          {:error, e} ->
            {:error, e, endPos}
        end

      other ->
        other
    end
  end

  def parse_erl_form(prompt) do
    parse_erl_form(default_input(), prompt, 1)
  end

  def parse_erl_form(io, prompt) do
    parse_erl_form(io, prompt, 1)
  end

  def parse_erl_form(io, prompt, pos0) do
    parse_erl_form(io, prompt, pos0, [])
  end

  def parse_erl_form(io, prompt, pos0, options) do
    args = [pos0, options]

    case request(
           io,
           {:get_until, :unicode, prompt, :erl_scan, :tokens, args}
         ) do
      {:ok, toks, endPos} ->
        case :erl_parse.parse_form(toks) do
          {:ok, exprs} ->
            {:ok, exprs, endPos}

          {:error, e} ->
            {:error, e, endPos}
        end

      other ->
        other
    end
  end

  def request(request) do
    request(default_output(), request)
  end

  def request(:standard_io, request) do
    request(:erlang.group_leader(), request)
  end

  def request(pid, request) when is_pid(pid) do
    execute_request(pid, io_request(pid, request))
  end

  def request(name, request) when is_atom(name) do
    case :erlang.whereis(name) do
      :undefined ->
        {:error, :arguments}

      pid ->
        request(pid, request)
    end
  end

  defp execute_request(pid, {convert, converted}) do
    mref = :erlang.monitor(:process, pid)
    send(pid, {:io_request, self(), mref, converted})

    receive do
      {:io_reply, ^mref, reply} ->
        :erlang.demonitor(mref, [:flush])

        cond do
          convert ->
            convert_binaries(reply)

          true ->
            reply
        end

      {:DOWN, ^mref, _, _, _} ->
        receive do
          {:EXIT, ^pid, _What} ->
            true
        after
          0 ->
            true
        end

        {:error, :terminated}
    end
  end

  def requests(requests) do
    requests(default_output(), requests)
  end

  def requests(:standard_io, requests) do
    requests(:erlang.group_leader(), requests)
  end

  def requests(pid, requests) when is_pid(pid) do
    {convert, converted} = io_requests(pid, requests)
    execute_request(pid, {convert, {:requests, converted}})
  end

  def requests(name, requests) when is_atom(name) do
    case :erlang.whereis(name) do
      :undefined ->
        {:error, :arguments}

      pid ->
        requests(pid, requests)
    end
  end

  defp default_input() do
    :erlang.group_leader()
  end

  defp default_output() do
    :erlang.group_leader()
  end

  defp io_requests(pid, rs) do
    io_requests(pid, rs, [], [])
  end

  defp io_requests(pid, [{:requests, rs1} | rs], cont, tail) do
    io_requests(pid, rs1, [rs | cont], tail)
  end

  defp io_requests(pid, [r], [], _Tail) do
    {conv, request} = io_request(pid, r)
    {conv, [request]}
  end

  defp io_requests(pid, [r | rs], cont, tail) do
    {_, request} = io_request(pid, r)
    {conv, requests} = io_requests(pid, rs, cont, tail)
    {conv, [request | requests]}
  end

  defp io_requests(pid, [], [rs | cont], tail) do
    io_requests(pid, rs, cont, tail)
  end

  defp io_requests(_Pid, [], [], _Tail) do
    {false, []}
  end

  defp bc_req(pid, req0, maybeConvert) do
    case :net_kernel.dflag_unicode_io(pid) do
      true ->
        {false, req0}

      false ->
        case :erlang.tuple_to_list(req0) do
          [op, _Enc] ->
            {maybeConvert, op}

          [op, _Enc | t] ->
            req = :erlang.list_to_tuple([op | t])
            {maybeConvert, req}
        end
    end
  end

  defp io_request(pid, {:write, term}) do
    bc_req(pid, {:put_chars, :unicode, :io_lib, :write, [term]}, false)
  end

  defp io_request(pid, {:format, format, args}) do
    bc_req(
      pid,
      {:put_chars, :unicode, :io_lib, :format, [format, args]},
      false
    )
  end

  defp io_request(pid, {:fwrite, format, args}) do
    bc_req(
      pid,
      {:put_chars, :unicode, :io_lib, :fwrite, [format, args]},
      false
    )
  end

  defp io_request(pid, :nl) do
    bc_req(pid, {:put_chars, :unicode, :io_lib.nl()}, false)
  end

  defp io_request(pid, {:put_chars, enc, chars} = request0)
       when is_list(chars) and node(pid) === node() do
    request =
      case (try do
              :unicode.characters_to_binary(chars, enc)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        binary when is_binary(binary) ->
          {:put_chars, enc, binary}

        _ ->
          request0
      end

    {false, request}
  end

  defp io_request(pid, {:put_chars, enc, chars} = request0)
       when is_list(chars) do
    case :net_kernel.dflag_unicode_io(pid) do
      true ->
        case (try do
                :unicode.characters_to_binary(chars, enc, :unicode)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          binary when is_binary(binary) ->
            {false, {:put_chars, :unicode, binary}}

          _ ->
            {false, request0}
        end

      false ->
        case (try do
                :unicode.characters_to_binary(chars, enc, :latin1)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          binary when is_binary(binary) ->
            {false, {:put_chars, binary}}

          _ ->
            {false, {:put_chars, chars}}
        end
    end
  end

  defp io_request(pid, {:fread, prompt, format}) do
    bc_req(
      pid,
      {:get_until, :unicode, prompt, :io_lib, :fread, [format]},
      true
    )
  end

  defp io_request(pid, {:get_until, enc, prompt, m, f, a}) do
    bc_req(pid, {:get_until, enc, prompt, m, f, a}, true)
  end

  defp io_request(pid, {:get_chars, enc, prompt, n}) do
    bc_req(pid, {:get_chars, enc, prompt, n}, true)
  end

  defp io_request(pid, {:get_line, enc, prompt}) do
    bc_req(pid, {:get_line, enc, prompt}, true)
  end

  defp io_request(pid, {:get_password, enc}) do
    bc_req(pid, {:get_password, enc}, true)
  end

  defp io_request(_Pid, r) do
    {false, r}
  end

  defp convert_binaries(bin) when is_binary(bin) do
    :unicode.characters_to_binary(bin, :latin1, :unicode)
  end

  defp convert_binaries(else__) do
    else__
  end
end
