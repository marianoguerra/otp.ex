defmodule :m_epp do
  use Bitwise
  require Record

  Record.defrecord(:r_epp, :epp,
    file: :undefined,
    location: 1,
    delta: 0,
    name: '',
    name2: '',
    istk: [],
    sstk: [],
    path: [],
    macs: %{},
    uses: %{},
    default_encoding: :utf8,
    pre_opened: false,
    fname: []
  )

  def open(name, path) do
    open(name, path, [])
  end

  def open(name, path, pdm) do
    internal_open(
      [{:name, name}, {:includes, path}, {:macros, pdm}],
      r_epp()
    )
  end

  def open(name, file, startLocation, path, pdm) do
    internal_open(
      [{:name, name}, {:includes, path}, {:macros, pdm}],
      r_epp(file: file, pre_opened: true, location: startLocation)
    )
  end

  def open(options) do
    internal_open(options, r_epp())
  end

  defp internal_open(options, st) do
    case :proplists.get_value(:name, options) do
      :undefined ->
        :erlang.error(:badarg)

      name ->
        self = self()

        epp =
          spawn(fn ->
            server(self, name, options, st)
          end)

        case epp_request(epp) do
          {:ok, pid, encoding} ->
            case :proplists.get_bool(:extra, options) do
              true ->
                {:ok, pid, [{:encoding, encoding}]}

              false ->
                {:ok, pid}
            end

          other ->
            other
        end
    end
  end

  def close(epp) do
    ref = :erlang.monitor(:process, epp)
    r = epp_request(epp, :close)

    receive do
      {:DOWN, ^ref, _, _, _} ->
        :ok
    end

    r
  end

  def scan_erl_form(epp) do
    epp_request(epp, :scan_erl_form)
  end

  def parse_erl_form(epp) do
    case epp_request(epp, :scan_erl_form) do
      {:ok, toks} ->
        :erl_parse.parse_form(toks)

      other ->
        other
    end
  end

  def macro_defs(epp) do
    epp_request(epp, :macro_defs)
  end

  def format_error(:cannot_parse) do
    :io_lib.format('cannot parse file, giving up', [])
  end

  def format_error({:bad, w}) do
    :io_lib.format('badly formed \'~s\'', [w])
  end

  def format_error(:missing_parenthesis) do
    :io_lib.format('badly formed define: missing closing right parenthesis', [])
  end

  def format_error(:premature_end) do
    'premature end'
  end

  def format_error({:call, what}) do
    :io_lib.format('illegal macro call \'~ts\'', [what])
  end

  def format_error({:undefined, m, :none}) do
    :io_lib.format('undefined macro \'~ts\'', [m])
  end

  def format_error({:undefined, m, a}) do
    :io_lib.format('undefined macro \'~ts/~p\'', [m, a])
  end

  def format_error({:depth, what}) do
    :io_lib.format('~s too deep', [what])
  end

  def format_error({:mismatch, m}) do
    :io_lib.format('argument mismatch for macro \'~ts\'', [m])
  end

  def format_error({:arg_error, m}) do
    :io_lib.format('badly formed argument for macro \'~ts\'', [m])
  end

  def format_error({:redefine, m}) do
    :io_lib.format('redefining macro \'~ts\'', [m])
  end

  def format_error({:redefine_predef, m}) do
    :io_lib.format('redefining predefined macro \'~s\'', [m])
  end

  def format_error({:circular, m, :none}) do
    :io_lib.format('circular macro \'~ts\'', [m])
  end

  def format_error({:circular, m, a}) do
    :io_lib.format('circular macro \'~ts/~p\'', [m, a])
  end

  def format_error({:include, w, f}) do
    :io_lib.format('can\'t find include ~s "~ts"', [w, f])
  end

  def format_error({:illegal, how, what}) do
    :io_lib.format('~s \'-~s\'', [how, what])
  end

  def format_error({:illegal_function, macro}) do
    :io_lib.format('?~s can only be used within a function', [macro])
  end

  def format_error({:illegal_function_usage, macro}) do
    :io_lib.format('?~s must not begin a form', [macro])
  end

  def format_error(:elif_after_else) do
    '\'elif\' following \'else\''
  end

  def format_error({:NYI, what}) do
    :io_lib.format('not yet implemented \'~s\'', [what])
  end

  def format_error({:error, term}) do
    :io_lib.format('-error(~tp).', [term])
  end

  def format_error({:warning, term}) do
    :io_lib.format('-warning(~tp).', [term])
  end

  def format_error(e) do
    :file.format_error(e)
  end

  def parse_file(ifile, path, predefs) do
    parse_file(
      ifile,
      [{:includes, path}, {:macros, predefs}]
    )
  end

  def parse_file(ifile, options) do
    case internal_open([{:name, ifile} | options], r_epp()) do
      {:ok, epp} ->
        forms = parse_file(epp)
        close(epp)
        {:ok, forms}

      {:ok, epp, extra} ->
        forms = parse_file(epp)
        close(epp)
        {:ok, forms, extra}

      {:error, e} ->
        {:error, e}
    end
  end

  def parse_file(epp) do
    case parse_erl_form(epp) do
      {:ok, form} ->
        [form | parse_file(epp)]

      {:error, e} ->
        [{:error, e} | parse_file(epp)]

      {:warning, w} ->
        [{:warning, w} | parse_file(epp)]

      {:eof, location} ->
        [{:eof, location}]
    end
  end

  def default_encoding() do
    :utf8
  end

  def encoding_to_string(:latin1) do
    'coding: latin-1'
  end

  def encoding_to_string(:utf8) do
    'coding: utf-8'
  end

  def read_encoding(name) do
    read_encoding(name, [])
  end

  def read_encoding(name, options) do
    inComment = :proplists.get_value(:in_comment_only, options, true)

    case :file.open(name, [:read]) do
      {:ok, file} ->
        try do
          read_encoding_from_file(file, inComment)
        after
          :ok = :file.close(file)
        end

      _Error ->
        :none
    end
  end

  def set_encoding(file) do
    set_encoding(file, :utf8)
  end

  def set_encoding(file, default) do
    encoding = read_encoding_from_file(file, true)

    enc =
      case encoding do
        :none ->
          default

        ^encoding ->
          encoding
      end

    :ok = :io.setopts(file, [{:encoding, enc}])
    encoding
  end

  def read_encoding_from_binary(binary) do
    read_encoding_from_binary(binary, [])
  end

  def read_encoding_from_binary(binary, options) do
    inComment = :proplists.get_value(:in_comment_only, options, true)

    try do
      com_nl(binary, fake_reader(0), 0, inComment)
    catch
      :no ->
        :none
    end
  end

  defp fake_reader(n) do
    fn
      () when n === 16 ->
        throw(:no)

      () ->
        {<<>>, fake_reader(n + 1)}
    end
  end

  defp read_encoding_from_file(file, inComment) do
    {:ok, pos0} = :file.position(file, :cur)
    opts = :io.getopts(file)
    encoding0 = :lists.keyfind(:encoding, 1, opts)
    binary0 = :lists.keyfind(:binary, 1, opts)
    :ok = :io.setopts(file, [:binary, {:encoding, :latin1}])

    try do
      {b, fun} = reader(file, 0).()
      com_nl(b, fun, 0, inComment)
    catch
      :no ->
        :none
    after
      {:ok, ^pos0} = :file.position(file, pos0)
      :ok = :io.setopts(file, [binary0, encoding0])
    end
  end

  defp reader(fd, n) do
    fn
      () when n === 16 ->
        throw(:no)

      () ->
        case :file.read(fd, 32) do
          :eof ->
            {<<>>, reader(fd, n + 1)}

          {:ok, bin} ->
            {bin, reader(fd, n + 1)}

          {:error, _} ->
            throw(:no)
        end
    end
  end

  defp com_nl(_, _, 2, _) do
    throw(:no)
  end

  defp com_nl(b, fun, n, false = com) do
    com_c(b, fun, n, com)
  end

  defp com_nl(b, fun, n, true = com) do
    com(b, fun, n, com)
  end

  defp com(<<"\n", b::binary>>, fun, n, com) do
    com_nl(b, fun, n + 1, com)
  end

  defp com(<<"%", b::binary>>, fun, n, com) do
    com_c(b, fun, n, com)
  end

  defp com(<<_::size(1)-unit(8), b::binary>>, fun, n, com) do
    com(b, fun, n, com)
  end

  defp com(<<>>, fun, n, com) do
    {b, fun1} = fun.()
    com(b, fun1, n, com)
  end

  defp com_c(<<"c", b::binary>>, fun, n, com) do
    com_oding(b, fun, n, com)
  end

  defp com_c(<<"\n", b::binary>>, fun, n, com) do
    com_nl(b, fun, n + 1, com)
  end

  defp com_c(<<_::size(1)-unit(8), b::binary>>, fun, n, com) do
    com_c(b, fun, n, com)
  end

  defp com_c(<<>>, fun, n, com) do
    {b, fun1} = fun.()
    com_c(b, fun1, n, com)
  end

  defp com_oding(<<"oding", b::binary>>, fun, n, com) do
    com_sep(b, fun, n, com)
  end

  defp com_oding(b, fun, n, com)
       when byte_size(b) >= length('oding') do
    com_c(b, fun, n, com)
  end

  defp com_oding(b, fun, n, com) do
    {b1, fun1} = fun.()
    com_oding(:erlang.list_to_binary([b, b1]), fun1, n, com)
  end

  defp com_sep(<<":", b::binary>>, fun, n, com) do
    com_space(b, fun, n, com)
  end

  defp com_sep(<<"=", b::binary>>, fun, n, com) do
    com_space(b, fun, n, com)
  end

  defp com_sep(<<" ", b::binary>>, fun, n, com) do
    com_sep(b, fun, n, com)
  end

  defp com_sep(<<>>, fun, n, com) do
    {b, fun1} = fun.()
    com_sep(b, fun1, n, com)
  end

  defp com_sep(b, fun, n, com) do
    com_c(b, fun, n, com)
  end

  defp com_space(<<" ", b::binary>>, fun, n, com) do
    com_space(b, fun, n, com)
  end

  defp com_space(<<>>, fun, n, com) do
    {b, fun1} = fun.()
    com_space(b, fun1, n, com)
  end

  defp com_space(b, fun, n, _Com) do
    com_enc(b, fun, n, [], [])
  end

  defp com_enc(<<c::size(1)-unit(8), b::binary>>, fun, n, l, ps)
       when (c >= ?a and c <= ?z) or (c >= ?A and c <= ?Z) or
              (c >= ?0 and c <= ?9) do
    com_enc(b, fun, n, [c | l], ps)
  end

  defp com_enc(<<>>, fun, n, l, ps) do
    case fun.() do
      {<<>>, _} ->
        com_enc_end([l | ps])

      {b, fun1} ->
        com_enc(b, fun1, n, l, ps)
    end
  end

  defp com_enc(<<"-", b::binary>>, fun, n, l, ps) do
    com_enc(b, fun, n, [], [l | ps])
  end

  defp com_enc(_B, _Fun, _N, l, ps) do
    com_enc_end([l | ps])
  end

  defp com_enc_end(ps0) do
    ps =
      :lists.reverse(
        for p <- ps0 do
          :lists.reverse(lowercase(p))
        end
      )

    com_encoding(ps)
  end

  defp com_encoding([['latin', '1'] | _]) do
    :latin1
  end

  defp com_encoding([['utf', '8'] | _]) do
    :utf8
  end

  defp com_encoding(_) do
    throw(:no)
  end

  defp lowercase(s) do
    :unicode.characters_to_list(:string.lowercase(s))
  end

  def normalize_typed_record_fields([]) do
    {:typed, []}
  end

  def normalize_typed_record_fields(fields) do
    normalize_typed_record_fields(fields, [], false)
  end

  defp normalize_typed_record_fields([], newFields, typed) do
    case typed do
      true ->
        {:typed, :lists.reverse(newFields)}

      false ->
        :not_typed
    end
  end

  defp normalize_typed_record_fields([{:typed_record_field, field, _} | rest], newFields, _Typed) do
    normalize_typed_record_fields(rest, [field | newFields], true)
  end

  defp normalize_typed_record_fields([field | rest], newFields, typed) do
    normalize_typed_record_fields(rest, [field | newFields], typed)
  end

  def restore_typed_record_fields([]) do
    []
  end

  def restore_typed_record_fields([
        [
          {:attribute, la, :record, {record, _NewFields}},
          {:attribute, la, :type, {{:record, record}, fields, []}}
        ]
        | forms
      ]) do
    [
      {:attribute, la, :record, {record, fields}}
      | restore_typed_record_fields(forms)
    ]
  end

  def restore_typed_record_fields([
        {:attribute, la, :type, {{:record, record}, fields, []}}
        | forms
      ]) do
    [
      {:attribute, la, :record, {record, fields}}
      | restore_typed_record_fields(forms)
    ]
  end

  def restore_typed_record_fields([form | forms]) do
    [form | restore_typed_record_fields(forms)]
  end

  defp server(pid, name, options, r_epp(pre_opened: preOpened) = st) do
    :erlang.process_flag(:trap_exit, true)

    case preOpened do
      false ->
        case :file.open(name, [:read]) do
          {:ok, file} ->
            init_server(pid, name, options, r_epp(st, file: file))

          {:error, e} ->
            epp_reply(pid, {:error, e})
        end

      true ->
        init_server(pid, name, options, st)
    end
  end

  defp init_server(pid, fileName, options, st0) do
    sourceName = :proplists.get_value(:source_name, options, fileName)
    pdm = :proplists.get_value(:macros, options, [])
    ms0 = predef_macros(fileName)

    case user_predef(pdm, ms0) do
      {:ok, ms1} ->
        r_epp(file: file, location: atLocation) = st0
        defEncoding = :proplists.get_value(:default_encoding, options, :utf8)
        encoding = set_encoding(file, defEncoding)
        epp_reply(pid, {:ok, self(), encoding})

        path = [
          :filename.dirname(fileName)
          | :proplists.get_value(:includes, options, [])
        ]

        st =
          r_epp(st0,
            delta: 0,
            name: sourceName,
            name2: sourceName,
            path: path,
            macs: ms1,
            default_encoding: defEncoding
          )

        from = wait_request(st)
        anno = :erl_anno.new(atLocation)
        enter_file_reply(from, file_name(sourceName), anno, atLocation, :code)
        wait_req_scan(st)

      {:error, e} ->
        epp_reply(pid, {:error, e})
    end
  end

  defp predef_macros(file) do
    machine = :erlang.list_to_atom(:erlang.system_info(:machine))
    anno = line1()
    otpVersion = :erlang.list_to_integer(:erlang.system_info(:otp_release))

    defs = [
      {:FILE, {:none, [{:string, anno, file}]}},
      {:FUNCTION_NAME, :undefined},
      {:FUNCTION_ARITY, :undefined},
      {:LINE, {:none, [{:integer, anno, 1}]}},
      {:MODULE, :undefined},
      {:MODULE_STRING, :undefined},
      {:BASE_MODULE, :undefined},
      {:BASE_MODULE_STRING, :undefined},
      {:MACHINE, {:none, [{:atom, anno, machine}]}},
      {machine, {:none, [{:atom, anno, true}]}},
      {:OTP_RELEASE, {:none, [{:integer, anno, otpVersion}]}}
    ]

    :maps.from_list(defs)
  end

  defp user_predef([{m, val, :redefine} | pdm], ms)
       when is_atom(m) do
    exp = :erl_parse.tokens(:erl_parse.abstract(val))
    user_predef(pdm, %{ms | m => {:none, exp}})
  end

  defp user_predef([{m, val} | pdm], ms) when is_atom(m) do
    case ms do
      %{^m => defs} when is_list(defs) ->
        {:error, {:redefine, m}}

      %{^m => _Defs} ->
        {:error, {:redefine_predef, m}}

      _ ->
        exp = :erl_parse.tokens(:erl_parse.abstract(val))
        user_predef(pdm, %{ms | m => [{:none, {:none, exp}}]})
    end
  end

  defp user_predef([m | pdm], ms) when is_atom(m) do
    user_predef([{m, true} | pdm], ms)
  end

  defp user_predef([md | _Pdm], _Ms) do
    {:error, {:bad, md}}
  end

  defp user_predef([], ms) do
    {:ok, ms}
  end

  defp wait_request(st) do
    receive do
      {:epp_request, from, :scan_erl_form} ->
        from

      {:epp_request, from, :macro_defs} ->
        defs =
          for {k, v} <- :maps.to_list(r_epp(st, :macs)) do
            {{:atom, k}, v}
          end

        epp_reply(from, defs)
        wait_request(st)

      {:epp_request, from, :close} ->
        close_file(st)
        epp_reply(from, :ok)
        exit(:normal)

      {:EXIT, _, r} ->
        exit(r)

      other ->
        :io.fwrite('Epp: unknown \'~w\'\n', [other])
        wait_request(st)
    end
  end

  defp close_file(r_epp(pre_opened: true)) do
    :ok
  end

  defp close_file(r_epp(pre_opened: false, file: file)) do
    :ok = :file.close(file)
  end

  defp wait_req_scan(st) do
    from = wait_request(st)
    scan_toks(from, st)
  end

  defp wait_req_skip(st, sis) do
    from = wait_request(st)
    skip_toks(from, st, sis)
  end

  defp enter_file(_NewName, inc, from, st)
       when length(r_epp(st, :sstk)) >= 8 do
    epp_reply(from, {:error, {loc(inc), :epp, {:depth, 'include'}}})
    wait_req_scan(st)
  end

  defp enter_file(newName, inc, from, st) do
    case :file.path_open(r_epp(st, :path), newName, [:read]) do
      {:ok, newF, pname} ->
        loc = start_loc(r_epp(st, :location))
        wait_req_scan(enter_file2(newF, pname, from, st, loc))

      {:error, _E} ->
        epp_reply(
          from,
          {:error, {loc(inc), :epp, {:include, :file, newName}}}
        )

        wait_req_scan(st)
    end
  end

  defp enter_file2(newF, pname, from, st0, atLocation) do
    anno = :erl_anno.new(atLocation)
    enter_file_reply(from, pname, anno, atLocation, :code)
    ms0 = r_epp(st0, :macs)
    ms = %{ms0 | :FILE => {:none, [{:string, anno, pname}]}}
    path = [:filename.dirname(pname) | tl(r_epp(st0, :path))]
    defEncoding = r_epp(st0, :default_encoding)
    _ = set_encoding(newF, defEncoding)

    r_epp(
      file: newF,
      location: atLocation,
      name: pname,
      name2: pname,
      delta: 0,
      sstk: [st0 | r_epp(st0, :sstk)],
      path: path,
      macs: ms,
      default_encoding: defEncoding
    )
  end

  defp enter_file_reply(from, name, locationAnno, atLocation, where) do
    anno0 = loc_anno(atLocation)

    anno =
      case where do
        :code ->
          anno0

        :generated ->
          :erl_anno.set_generated(true, anno0)
      end

    rep =
      {:ok,
       [
         {:-, anno},
         {:atom, anno, :file},
         {:"(", anno},
         {:string, anno, name},
         {:",", anno},
         {:integer, anno, get_line(locationAnno)},
         {:")", locationAnno},
         {:dot, anno}
       ]}

    epp_reply(from, rep)
  end

  defp file_name([c | t]) when is_integer(c) and c > 0 do
    [c | file_name(t)]
  end

  defp file_name([h | t]) do
    file_name(h) ++ file_name(t)
  end

  defp file_name([]) do
    []
  end

  defp file_name(n) when is_atom(n) do
    :erlang.atom_to_list(n)
  end

  defp leave_file(from, st) do
    case r_epp(st, :istk) do
      [i | cis] ->
        epp_reply(
          from,
          {:error, {r_epp(st, :location), :epp, {:illegal, 'unterminated', i}}}
        )

        leave_file(wait_request(st), r_epp(st, istk: cis))

      [] ->
        case r_epp(st, :sstk) do
          [oldSt | sts] ->
            close_file(st)
            r_epp(location: oldLoc, delta: delta, name: oldName, name2: oldName2) = oldSt
            currLoc = add_line(oldLoc, delta)
            anno = :erl_anno.new(currLoc)
            ms0 = r_epp(st, :macs)
            ms = %{ms0 | :FILE => {:none, [{:string, anno, oldName2}]}}
            nextSt = r_epp(oldSt, sstk: sts, macs: ms, uses: r_epp(st, :uses))
            enter_file_reply(from, oldName, anno, currLoc, :code)

            case oldName2 === oldName do
              true ->
                :ok

              false ->
                nFrom = wait_request(nextSt)
                oldAnno = :erl_anno.new(oldLoc)
                enter_file_reply(nFrom, oldName2, oldAnno, currLoc, :generated)
            end

            wait_req_scan(nextSt)

          [] ->
            epp_reply(from, {:eof, r_epp(st, :location)})
            wait_req_scan(st)
        end
    end
  end

  defp scan_toks(from, st) do
    case :io.scan_erl_form(r_epp(st, :file), :"", r_epp(st, :location)) do
      {:ok, toks, cl} ->
        scan_toks(toks, from, r_epp(st, location: cl))

      {:error, e, cl} ->
        epp_reply(from, {:error, e})
        wait_req_scan(r_epp(st, location: cl))

      {:eof, cl} ->
        leave_file(from, r_epp(st, location: cl))

      {:error, _E} ->
        epp_reply(
          from,
          {:error, {r_epp(st, :location), :epp, :cannot_parse}}
        )

        leave_file(wait_request(st), st)
    end
  end

  defp scan_toks(
         [
           [{:-, _Lh}, {:atom, _Ld, :define} = define]
           | toks
         ],
         from,
         st
       ) do
    scan_define(toks, define, from, st)
  end

  defp scan_toks(
         [
           [{:-, _Lh}, {:atom, _Ld, :undef} = undef]
           | toks
         ],
         from,
         st
       ) do
    scan_undef(toks, undef, from, st)
  end

  defp scan_toks(
         [
           [{:-, _Lh}, {:atom, _Ld, :error} = error]
           | toks
         ],
         from,
         st
       ) do
    scan_err_warn(toks, error, from, st)
  end

  defp scan_toks(
         [
           [{:-, _Lh}, {:atom, _Ld, :warning} = warn]
           | toks
         ],
         from,
         st
       ) do
    scan_err_warn(toks, warn, from, st)
  end

  defp scan_toks(
         [
           [{:-, _Lh}, {:atom, _Li, :include} = inc]
           | toks
         ],
         from,
         st
       ) do
    scan_include(toks, inc, from, st)
  end

  defp scan_toks(
         [
           [{:-, _Lh}, {:atom, _Li, :include_lib} = incLib]
           | toks
         ],
         from,
         st
       ) do
    scan_include_lib(toks, incLib, from, st)
  end

  defp scan_toks(
         [
           [{:-, _Lh}, {:atom, _Li, :ifdef} = ifDef]
           | toks
         ],
         from,
         st
       ) do
    scan_ifdef(toks, ifDef, from, st)
  end

  defp scan_toks(
         [
           [{:-, _Lh}, {:atom, _Li, :ifndef} = ifnDef]
           | toks
         ],
         from,
         st
       ) do
    scan_ifndef(toks, ifnDef, from, st)
  end

  defp scan_toks(
         [
           [{:-, _Lh}, {:atom, _Le, :else} = else__]
           | toks
         ],
         from,
         st
       ) do
    scan_else(toks, else__, from, st)
  end

  defp scan_toks([[{:-, _Lh}, {:if, _Le} = if__] | toks], from, st) do
    scan_if(toks, if__, from, st)
  end

  defp scan_toks(
         [
           [{:-, _Lh}, {:atom, _Le, :elif} = elif]
           | toks
         ],
         from,
         st
       ) do
    scan_elif(toks, elif, from, st)
  end

  defp scan_toks(
         [
           [{:-, _Lh}, {:atom, _Le, :endif} = endif]
           | toks
         ],
         from,
         st
       ) do
    scan_endif(toks, endif, from, st)
  end

  defp scan_toks(
         [
           [{:-, _Lh}, {:atom, _Lf, :file} = fileToken]
           | toks0
         ],
         from,
         st
       ) do
    case (try do
            expand_macros(toks0, st)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      toks1 when is_list(toks1) ->
        scan_file(toks1, fileToken, from, st)

      {:error, errL, what} ->
        epp_reply(from, {:error, {errL, :epp, what}})
        wait_req_scan(st)
    end
  end

  defp scan_toks(toks0, from, st) do
    case (try do
            expand_macros(toks0, r_epp(st, fname: toks0))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      toks1 when is_list(toks1) ->
        epp_reply(from, {:ok, toks1})

        wait_req_scan(
          r_epp(st,
            macs:
              scan_module(
                toks1,
                r_epp(st, :macs)
              )
          )
        )

      {:error, errL, what} ->
        epp_reply(from, {:error, {errL, :epp, what}})
        wait_req_scan(st)
    end
  end

  defp scan_module(
         [
           [{:-, _Lh}, {:atom, _Lm, :module}, {:"(", _Ll}]
           | ts
         ],
         ms
       ) do
    scan_module_1(ts, ms)
  end

  defp scan_module(
         [
           [{:-, _Lh}, {:atom, _Lm, :extends}, {:"(", _Ll}]
           | ts
         ],
         ms
       ) do
    scan_extends(ts, ms)
  end

  defp scan_module(_Ts, ms) do
    ms
  end

  defp scan_module_1([[{:atom, _, _} = a, {:",", l}] | ts], ms) do
    scan_module_1([[a, {:")", l}] | ts], ms)
  end

  defp scan_module_1(
         [[{:atom, ln, a} = modAtom, {:")", _Lr}] | _Ts],
         ms0
       ) do
    modString = :erlang.atom_to_list(a)
    ms = %{ms0 | :MODULE => {:none, [modAtom]}}
    %{ms | :MODULE_STRING => {:none, [{:string, ln, modString}]}}
  end

  defp scan_module_1(_Ts, ms) do
    ms
  end

  defp scan_extends(
         [[{:atom, ln, a} = modAtom, {:")", _Lr}] | _Ts],
         ms0
       ) do
    modString = :erlang.atom_to_list(a)
    ms = %{ms0 | :BASE_MODULE => {:none, [modAtom]}}
    %{ms | :BASE_MODULE_STRING => {:none, [{:string, ln, modString}]}}
  end

  defp scan_extends(_Ts, ms) do
    ms
  end

  defp scan_err_warn([{:"(", _} | _] = toks0, {:atom, _, tag} = token, from, st) do
    try do
      expand_macros(toks0, st)
    catch
      _, _ ->
        epp_reply(
          from,
          {:error, {loc(token), :epp, {:bad, tag}}}
        )
    else
      toks when is_list(toks) ->
        case :erl_parse.parse_term(toks) do
          {:ok, term} ->
            epp_reply(from, {tag, {loc(token), :epp, {tag, term}}})

          {:error, _} ->
            epp_reply(
              from,
              {:error, {loc(token), :epp, {:bad, tag}}}
            )
        end
    end

    wait_req_scan(st)
  end

  defp scan_err_warn(_Toks, {:atom, _, tag} = token, from, st) do
    epp_reply(
      from,
      {:error, {loc(token), :epp, {:bad, tag}}}
    )

    wait_req_scan(st)
  end

  defp scan_define([[{:"(", _Lp}, {type, _Lm, _} = mac] | toks], def__, from, st)
       when type === :atom or type === :var do
    scan_define_1(toks, mac, def__, from, st)
  end

  defp scan_define(_Toks, def__, from, st) do
    epp_reply(
      from,
      {:error, {loc(def__), :epp, {:bad, :define}}}
    )

    wait_req_scan(st)
  end

  defp scan_define_1([{:",", _} = comma | toks], mac, _Def, from, st) do
    case (try do
            macro_expansion(toks, comma)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      expansion when is_list(expansion) ->
        scan_define_2(:none, {:none, expansion}, mac, from, st)

      {:error, errL, what} ->
        epp_reply(from, {:error, {errL, :epp, what}})
        wait_req_scan(st)
    end
  end

  defp scan_define_1([{:"(", _Lc} | toks], mac, def__, from, st) do
    case (try do
            macro_pars(toks, [])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, {as, _} = macroDef} ->
        len = length(as)
        scan_define_2(len, macroDef, mac, from, st)

      {:error, errL, what} ->
        epp_reply(from, {:error, {errL, :epp, what}})
        wait_req_scan(st)

      _ ->
        epp_reply(
          from,
          {:error, {loc(def__), :epp, {:bad, :define}}}
        )

        wait_req_scan(st)
    end
  end

  defp scan_define_1(_Toks, _Mac, def__, from, st) do
    epp_reply(
      from,
      {:error, {loc(def__), :epp, {:bad, :define}}}
    )

    wait_req_scan(st)
  end

  defp scan_define_2(arity, def__, {_, _, key} = mac, from, r_epp(macs: ms) = st) do
    case ms do
      %{^key => defs} when is_list(defs) ->
        case :proplists.is_defined(arity, defs) do
          true ->
            epp_reply(
              from,
              {:error, {loc(mac), :epp, {:redefine, key}}}
            )

            wait_req_scan(st)

          false ->
            scan_define_cont(from, st, key, defs, arity, def__)
        end

      %{^key => _} ->
        epp_reply(
          from,
          {:error, {loc(mac), :epp, {:redefine_predef, key}}}
        )

        wait_req_scan(st)

      _ ->
        scan_define_cont(from, st, key, [], arity, def__)
    end
  end

  defp scan_define_cont(f, r_epp(macs: ms0) = st, m, defs, arity, def__) do
    ms = %{ms0 | m => [{arity, def__} | defs]}

    try do
      macro_uses(def__)
    catch
      {:error, line, reason} ->
        epp_reply(f, {:error, {line, :epp, reason}})
        wait_req_scan(st)
    else
      u ->
        uses0 = r_epp(st, :uses)

        val = [
          {arity, u}
          | case uses0 do
              %{^m => useList} ->
                useList

              _ ->
                []
            end
        ]

        uses = %{uses0 | m => val}
        scan_toks(f, r_epp(st, uses: uses, macs: ms))
    end
  end

  defp macro_uses({_Args, tokens}) do
    uses0 = macro_ref(tokens)
    :lists.usort(uses0)
  end

  defp macro_ref([]) do
    []
  end

  defp macro_ref([[{:"?", _}, {:"?", _}] | rest]) do
    macro_ref(rest)
  end

  defp macro_ref([[{:"?", _}, {:atom, _, a} = atom] | rest]) do
    lm = loc(atom)
    arity = count_args(rest, lm, a)
    [{a, arity} | macro_ref(rest)]
  end

  defp macro_ref([[{:"?", _}, {:var, _, a} = var] | rest]) do
    lm = loc(var)
    arity = count_args(rest, lm, a)
    [{a, arity} | macro_ref(rest)]
  end

  defp macro_ref([_Token | rest]) do
    macro_ref(rest)
  end

  defp scan_undef([{:"(", _Llp}, {:atom, _Lm, m}, {:")", _Lrp}, {:dot, _Ld}], _Undef, from, st) do
    macs = :maps.remove(m, r_epp(st, :macs))
    uses = :maps.remove(m, r_epp(st, :uses))
    scan_toks(from, r_epp(st, macs: macs, uses: uses))
  end

  defp scan_undef([{:"(", _Llp}, {:var, _Lm, m}, {:")", _Lrp}, {:dot, _Ld}], _Undef, from, st) do
    macs = :maps.remove(m, r_epp(st, :macs))
    uses = :maps.remove(m, r_epp(st, :uses))
    scan_toks(from, r_epp(st, macs: macs, uses: uses))
  end

  defp scan_undef(_Toks, undef, from, st) do
    epp_reply(
      from,
      {:error, {loc(undef), :epp, {:bad, :undef}}}
    )

    wait_req_scan(st)
  end

  defp scan_include(tokens0, inc, from, st) do
    tokens = coalesce_strings(tokens0)
    scan_include1(tokens, inc, from, st)
  end

  defp scan_include1(
         [{:"(", _Llp}, {:string, _Lf, newName0}, {:")", _Lrp}, {:dot, _Ld}],
         inc,
         from,
         st
       ) do
    newName = expand_var(newName0)
    enter_file(newName, inc, from, st)
  end

  defp scan_include1(_Toks, inc, from, st) do
    epp_reply(
      from,
      {:error, {loc(inc), :epp, {:bad, :include}}}
    )

    wait_req_scan(st)
  end

  defp expand_lib_dir(name) do
    try do
      [app | path] = :filename.split(name)
      libDir = :code.lib_dir(:erlang.list_to_atom(app))
      {:ok, fname_join([libDir | path])}
    catch
      _, _ ->
        :error
    end
  end

  defp scan_include_lib(tokens0, inc, from, st) do
    tokens = coalesce_strings(tokens0)
    scan_include_lib1(tokens, inc, from, st)
  end

  defp scan_include_lib1(
         [{:"(", _Llp}, {:string, _Lf, _NewName0}, {:")", _Lrp}, {:dot, _Ld}],
         inc,
         from,
         st
       )
       when length(r_epp(st, :sstk)) >= 8 do
    epp_reply(from, {:error, {loc(inc), :epp, {:depth, 'include_lib'}}})
    wait_req_scan(st)
  end

  defp scan_include_lib1(
         [{:"(", _Llp}, {:string, _Lf, newName0}, {:")", _Lrp}, {:dot, _Ld}],
         inc,
         from,
         st
       ) do
    newName = expand_var(newName0)
    loc = start_loc(r_epp(st, :location))

    case :file.path_open(r_epp(st, :path), newName, [:read]) do
      {:ok, newF, pname} ->
        wait_req_scan(enter_file2(newF, pname, from, st, loc))

      {:error, _E1} ->
        case expand_lib_dir(newName) do
          {:ok, header} ->
            case :file.open(header, [:read]) do
              {:ok, newF} ->
                wait_req_scan(enter_file2(newF, header, from, st, loc))

              {:error, _E2} ->
                epp_reply(
                  from,
                  {:error, {loc(inc), :epp, {:include, :lib, newName}}}
                )

                wait_req_scan(st)
            end

          :error ->
            epp_reply(
              from,
              {:error, {loc(inc), :epp, {:include, :lib, newName}}}
            )

            wait_req_scan(st)
        end
    end
  end

  defp scan_include_lib1(_Toks, inc, from, st) do
    epp_reply(
      from,
      {:error, {loc(inc), :epp, {:bad, :include_lib}}}
    )

    wait_req_scan(st)
  end

  defp scan_ifdef([{:"(", _Llp}, {:atom, _Lm, m}, {:")", _Lrp}, {:dot, _Ld}], _IfD, from, st) do
    case r_epp(st, :macs) do
      %{^m => _Def} ->
        scan_toks(from, r_epp(st, istk: [:ifdef | r_epp(st, :istk)]))

      _ ->
        skip_toks(from, st, [:ifdef])
    end
  end

  defp scan_ifdef([{:"(", _Llp}, {:var, _Lm, m}, {:")", _Lrp}, {:dot, _Ld}], _IfD, from, st) do
    case r_epp(st, :macs) do
      %{^m => _Def} ->
        scan_toks(from, r_epp(st, istk: [:ifdef | r_epp(st, :istk)]))

      _ ->
        skip_toks(from, st, [:ifdef])
    end
  end

  defp scan_ifdef(_Toks, ifDef, from, st) do
    epp_reply(
      from,
      {:error, {loc(ifDef), :epp, {:bad, :ifdef}}}
    )

    wait_req_skip(st, [:ifdef])
  end

  defp scan_ifndef([{:"(", _Llp}, {:atom, _Lm, m}, {:")", _Lrp}, {:dot, _Ld}], _IfnD, from, st) do
    case r_epp(st, :macs) do
      %{^m => _Def} ->
        skip_toks(from, st, [:ifndef])

      _ ->
        scan_toks(from, r_epp(st, istk: [:ifndef | r_epp(st, :istk)]))
    end
  end

  defp scan_ifndef([{:"(", _Llp}, {:var, _Lm, m}, {:")", _Lrp}, {:dot, _Ld}], _IfnD, from, st) do
    case r_epp(st, :macs) do
      %{^m => _Def} ->
        skip_toks(from, st, [:ifndef])

      _ ->
        scan_toks(from, r_epp(st, istk: [:ifndef | r_epp(st, :istk)]))
    end
  end

  defp scan_ifndef(_Toks, ifnDef, from, st) do
    epp_reply(
      from,
      {:error, {loc(ifnDef), :epp, {:bad, :ifndef}}}
    )

    wait_req_skip(st, [:ifndef])
  end

  defp scan_else([{:dot, _Ld}], else__, from, st) do
    case r_epp(st, :istk) do
      [:else | cis] ->
        epp_reply(
          from,
          {:error, {loc(else__), :epp, {:illegal, 'repeated', :else}}}
        )

        wait_req_skip(r_epp(st, istk: cis), [:else])

      [_I | cis] ->
        skip_toks(from, r_epp(st, istk: cis), [:else])

      [] ->
        epp_reply(
          from,
          {:error, {loc(else__), :epp, {:illegal, 'unbalanced', :else}}}
        )

        wait_req_scan(st)
    end
  end

  defp scan_else(_Toks, else__, from, st) do
    epp_reply(
      from,
      {:error, {loc(else__), :epp, {:bad, :else}}}
    )

    wait_req_scan(st)
  end

  defp scan_if([{:"(", _} | _] = toks, if__, from, st) do
    try do
      eval_if(toks, st)
    catch
      error0 ->
        error =
          case error0 do
            {_, :erl_parse, _} ->
              {:error, error0}

            {:error, errL, what} ->
              {:error, {errL, :epp, what}}

            _ ->
              {:error, {loc(if__), :epp, error0}}
          end

        epp_reply(from, error)
        wait_req_skip(st, [:if])
    else
      true ->
        scan_toks(from, r_epp(st, istk: [:if | r_epp(st, :istk)]))

      _ ->
        skip_toks(from, st, [:if])
    end
  end

  defp scan_if(_Toks, if__, from, st) do
    epp_reply(
      from,
      {:error, {loc(if__), :epp, {:bad, :if}}}
    )

    wait_req_skip(st, [:if])
  end

  defp eval_if(toks0, st) do
    toks = expand_macros(toks0, st)

    es1 =
      case :erl_parse.parse_exprs(toks) do
        {:ok, es0} ->
          es0

        {:error, e} ->
          throw(e)
      end

    es = rewrite_expr(es1, st)
    assert_guard_expr(es)
    bs = :erl_eval.new_bindings()

    localFun = fn _Name, _Args ->
      :erlang.error(:badarg)
    end

    try do
      :erl_eval.exprs(es, bs, {:value, localFun})
    catch
      _, _ ->
        false
    else
      {:value, res, _} ->
        res
    end
  end

  defp assert_guard_expr([e0]) do
    e = rewrite_expr(e0, :none)

    case :erl_lint.is_guard_expr(e) do
      false ->
        throw({:bad, :if})

      true ->
        :ok
    end
  end

  defp assert_guard_expr(_) do
    throw({:bad, :if})
  end

  defp rewrite_expr(
         {:call, _, {:atom, _, :defined}, [n0]},
         r_epp(macs: macs)
       ) do
    n =
      case n0 do
        {:var, _, n1} ->
          n1

        {:atom, _, n1} ->
          n1

        _ ->
          throw({:bad, :if})
      end

    {:atom, 0, :maps.is_key(n, macs)}
  end

  defp rewrite_expr({:call, _, {:atom, _, name}, as0}, :none) do
    as = rewrite_expr(as0, :none)
    arity = length(as)

    case :erl_internal.bif(name, arity) and
           not :erl_internal.guard_bif(
             name,
             arity
           ) do
      false ->
        to_conses(as)

      true ->
        throw({:bad, :if})
    end
  end

  defp rewrite_expr([h | t], st) do
    [rewrite_expr(h, st) | rewrite_expr(t, st)]
  end

  defp rewrite_expr(tuple, st) when is_tuple(tuple) do
    :erlang.list_to_tuple(
      rewrite_expr(
        :erlang.tuple_to_list(tuple),
        st
      )
    )
  end

  defp rewrite_expr(other, _) do
    other
  end

  defp to_conses([h | t]) do
    {:cons, 0, h, to_conses(t)}
  end

  defp to_conses([]) do
    {nil, 0}
  end

  defp scan_elif(_Toks, elif, from, st) do
    case r_epp(st, :istk) do
      [:else | cis] ->
        epp_reply(
          from,
          {:error, {loc(elif), :epp, {:illegal, 'unbalanced', :elif}}}
        )

        wait_req_skip(r_epp(st, istk: cis), [:else])

      [_I | cis] ->
        skip_toks(from, r_epp(st, istk: cis), [:elif])

      [] ->
        epp_reply(
          from,
          {:error, {loc(elif), :epp, {:illegal, 'unbalanced', :elif}}}
        )

        wait_req_scan(st)
    end
  end

  defp scan_endif([{:dot, _Ld}], endif, from, st) do
    case r_epp(st, :istk) do
      [_I | cis] ->
        scan_toks(from, r_epp(st, istk: cis))

      [] ->
        epp_reply(
          from,
          {:error, {loc(endif), :epp, {:illegal, 'unbalanced', :endif}}}
        )

        wait_req_scan(st)
    end
  end

  defp scan_endif(_Toks, endif, from, st) do
    epp_reply(
      from,
      {:error, {loc(endif), :epp, {:bad, :endif}}}
    )

    wait_req_scan(st)
  end

  defp scan_file(tokens0, tf, from, st) do
    tokens = coalesce_strings(tokens0)
    scan_file1(tokens, tf, from, st)
  end

  defp scan_file1(
         [
           {:"(", _Llp},
           {:string, _Ls, name},
           {:",", _Lc},
           {:integer, _Li, ln},
           {:")", _Lrp},
           {:dot, _Ld}
         ],
         tf,
         from,
         st
       ) do
    anno = :erl_anno.new(ln)
    enter_file_reply(from, name, anno, loc(tf), :generated)
    ms0 = r_epp(st, :macs)
    ms = %{ms0 | :FILE => {:none, [{:string, line1(), name}]}}
    locf = loc(tf)
    newLoc = new_location(ln, r_epp(st, :location), locf)

    delta =
      get_line(
        :erlang.element(
          2,
          tf
        )
      ) - ln + r_epp(st, :delta)

    wait_req_scan(r_epp(st, name2: name, location: newLoc, delta: delta, macs: ms))
  end

  defp scan_file1(_Toks, tf, from, st) do
    epp_reply(
      from,
      {:error, {loc(tf), :epp, {:bad, :file}}}
    )

    wait_req_scan(st)
  end

  defp new_location(ln, le, lf) when is_integer(lf) do
    ln + (le - lf)
  end

  defp new_location(ln, {le, _}, {lf, _}) do
    {ln + (le - lf), 1}
  end

  defp skip_toks(from, st, [i | sis]) do
    case :io.scan_erl_form(r_epp(st, :file), :"", r_epp(st, :location)) do
      {:ok, [[{:-, _Lh}, {:atom, _Li, :ifdef}] | _Toks], cl} ->
        skip_toks(from, r_epp(st, location: cl), [[:ifdef, i] | sis])

      {:ok, [[{:-, _Lh}, {:atom, _Li, :ifndef}] | _Toks], cl} ->
        skip_toks(from, r_epp(st, location: cl), [[:ifndef, i] | sis])

      {:ok, [[{:-, _Lh}, {:if, _Li}] | _Toks], cl} ->
        skip_toks(from, r_epp(st, location: cl), [[:if, i] | sis])

      {:ok, [[{:-, _Lh}, {:atom, _Le, :else} = else__] | _Toks], cl} ->
        skip_else(else__, from, r_epp(st, location: cl), [i | sis])

      {:ok, [[{:-, _Lh}, {:atom, _Le, :elif} = elif] | toks], cl} ->
        skip_elif(toks, elif, from, r_epp(st, location: cl), [i | sis])

      {:ok, [[{:-, _Lh}, {:atom, _Le, :endif}] | _Toks], cl} ->
        skip_toks(from, r_epp(st, location: cl), sis)

      {:ok, _Toks, cl} ->
        skip_toks(from, r_epp(st, location: cl), [i | sis])

      {:error, e, cl} ->
        case e do
          {_, :file_io_server, :invalid_unicode} ->
            epp_reply(from, {:error, e})
            leave_file(wait_request(st), st)

          _ ->
            skip_toks(from, r_epp(st, location: cl), [i | sis])
        end

      {:eof, cl} ->
        leave_file(from, r_epp(st, location: cl, istk: [i | sis]))

      {:error, _E} ->
        epp_reply(
          from,
          {:error, {r_epp(st, :location), :epp, :cannot_parse}}
        )

        leave_file(wait_request(st), st)
    end
  end

  defp skip_toks(from, st, []) do
    scan_toks(from, st)
  end

  defp skip_else(else__, from, st, [:else | sis]) do
    epp_reply(
      from,
      {:error, {loc(else__), :epp, {:illegal, 'repeated', :else}}}
    )

    wait_req_skip(st, [:else | sis])
  end

  defp skip_else(_Else, from, st, [:elif | sis]) do
    skip_toks(from, st, [:else | sis])
  end

  defp skip_else(_Else, from, st, [_I]) do
    scan_toks(from, r_epp(st, istk: [:else | r_epp(st, :istk)]))
  end

  defp skip_else(_Else, from, st, sis) do
    skip_toks(from, st, sis)
  end

  defp skip_elif(_Toks, elif, from, st, [:else | _] = sis) do
    epp_reply(
      from,
      {:error, {loc(elif), :epp, :elif_after_else}}
    )

    wait_req_skip(st, sis)
  end

  defp skip_elif(toks, elif, from, st, [_I]) do
    scan_if(toks, elif, from, st)
  end

  defp skip_elif(_Toks, _Elif, from, st, sis) do
    skip_toks(from, st, sis)
  end

  defp macro_pars([[{:")", _Lp}, {:",", _Ld} = comma] | ex], args) do
    {:ok, {:lists.reverse(args), macro_expansion(ex, comma)}}
  end

  defp macro_pars(
         [
           [{:var, _, name}, {:")", _Lp}, {:",", _Ld} = comma]
           | ex
         ],
         args
       ) do
    false = :lists.member(name, args)
    {:ok, {:lists.reverse([name | args]), macro_expansion(ex, comma)}}
  end

  defp macro_pars([[{:var, _L, name}, {:",", _}] | ts], args) do
    false = :lists.member(name, args)
    macro_pars(ts, [name | args])
  end

  defp macro_expansion([{:")", _Lp}, {:dot, _Ld}], _T0) do
    []
  end

  defp macro_expansion([{:dot, _} = dot], _T0) do
    throw({:error, loc(dot), :missing_parenthesis})
  end

  defp macro_expansion([t | ts], _T0) do
    [t | macro_expansion(ts, t)]
  end

  defp macro_expansion([], t0) do
    throw({:error, loc(t0), :premature_end})
  end

  defp expand_macros(macT, m, toks, st) do
    r_epp(macs: ms, uses: u) = st
    lm = loc(macT)
    tinfo = :erlang.element(2, macT)

    case expand_macro1(lm, m, toks, ms) do
      {:ok, {:none, exp}} ->
        check_uses([{m, :none}], [], u, lm)

        toks1 =
          expand_macros(
            expand_macro(exp, tinfo, [], %{}),
            st
          )

        expand_macros(toks1 ++ toks, st)

      {:ok, {as, exp}} ->
        check_uses([{m, length(as)}], [], u, lm)
        {bs, toks1} = bind_args(toks, lm, m, as, %{})
        expand_macros(expand_macro(exp, tinfo, toks1, bs), st)
    end
  end

  defp expand_macro1(lm, m, toks, ms) do
    arity = count_args(toks, lm, m)

    case ms do
      %{^m => :undefined} ->
        throw({:error, lm, {:undefined, m, arity}})

      %{^m => [{:none, def__}]} ->
        {:ok, def__}

      %{^m => defs} when is_list(defs) ->
        case :proplists.get_value(arity, defs) do
          :undefined ->
            throw({:error, lm, {:mismatch, m}})

          def__ ->
            {:ok, def__}
        end

      %{^m => preDef} ->
        {:ok, preDef}

      _ ->
        throw({:error, lm, {:undefined, m, arity}})
    end
  end

  defp check_uses([], _Anc, _U, _Lm) do
    :ok
  end

  defp check_uses([m | rest], anc, u, lm) do
    case :lists.member(m, anc) do
      true ->
        {name, arity} = m
        throw({:error, lm, {:circular, name, arity}})

      false ->
        l = get_macro_uses(m, u)
        check_uses(l, [m | anc], u, lm)
        check_uses(rest, anc, u, lm)
    end
  end

  defp get_macro_uses({m, arity}, u) do
    case u do
      %{^m => l} ->
        :proplists.get_value(arity, l, :proplists.get_value(:none, l, []))

      _ ->
        []
    end
  end

  defp expand_macros(
         [[{:"?", _Lq}, {:atom, _Lm, m} = macT] | toks],
         st
       ) do
    expand_macros(macT, m, toks, st)
  end

  defp expand_macros(
         [
           [{:"?", _Lq}, {:var, lm, :FUNCTION_NAME} = token]
           | toks
         ],
         st0
       ) do
    st = update_fun_name(token, st0)

    case r_epp(st, :fname) do
      :undefined ->
        [{:"?", _Lq}, token]

      {name, _} ->
        [{:atom, lm, name}]
    end ++ expand_macros(toks, st)
  end

  defp expand_macros(
         [
           [{:"?", _Lq}, {:var, lm, :FUNCTION_ARITY} = token]
           | toks
         ],
         st0
       ) do
    st = update_fun_name(token, st0)

    case r_epp(st, :fname) do
      :undefined ->
        [{:"?", _Lq}, token]

      {_, arity} ->
        [{:integer, lm, arity}]
    end ++ expand_macros(toks, st)
  end

  defp expand_macros(
         [[{:"?", _Lq}, {:var, lm, :LINE} = tok] | toks],
         st
       ) do
    line = :erl_scan.line(tok)
    [{:integer, lm, line} | expand_macros(toks, st)]
  end

  defp expand_macros(
         [[{:"?", _Lq}, {:var, _Lm, m} = macT] | toks],
         st
       ) do
    expand_macros(macT, m, toks, st)
  end

  defp expand_macros([[{:"?", _Lq}, token] | _Toks], _St) do
    t =
      case :erl_scan.text(token) do
        text when is_list(text) ->
          text

        :undefined ->
          symbol = :erl_scan.symbol(token)
          :io_lib.fwrite("~tp", [symbol])
      end

    throw({:error, loc(token), {:call, [?? | t]}})
  end

  defp expand_macros([t | ts], st) do
    [t | expand_macros(ts, st)]
  end

  defp expand_macros([], _St) do
    []
  end

  defp bind_args([[{:"(", _Llp}, {:")", _Lrp}] | toks], _Lm, _M, [], bs) do
    {bs, toks}
  end

  defp bind_args([{:"(", _Llp} | toks0], lm, m, [a | as], bs) do
    {arg, toks1} = macro_arg(toks0, [], [])
    macro_args(toks1, lm, m, as, store_arg(lm, m, a, arg, bs))
  end

  defp bind_args(_Toks, lm, m, _As, _Bs) do
    throw({:error, lm, {:mismatch, m}})
  end

  defp macro_args([{:")", _Lrp} | toks], _Lm, _M, [], bs) do
    {bs, toks}
  end

  defp macro_args([{:",", _Lc} | toks0], lm, m, [a | as], bs) do
    {arg, toks1} = macro_arg(toks0, [], [])
    macro_args(toks1, lm, m, as, store_arg(lm, m, a, arg, bs))
  end

  defp macro_args([], lm, m, _As, _Bs) do
    throw({:error, lm, {:arg_error, m}})
  end

  defp macro_args(_Toks, lm, m, _As, _Bs) do
    throw({:error, lm, {:mismatch, m}})
  end

  defp store_arg(l, m, _A, [], _Bs) do
    throw({:error, l, {:mismatch, m}})
  end

  defp store_arg(_L, _M, a, arg, bs) do
    %{bs | a => arg}
  end

  defp count_args([[{:"(", _Llp}, {:")", _Lrp}] | _Toks], _Lm, _M) do
    0
  end

  defp count_args([[{:"(", _Llp}, {:",", _Lc}] | _Toks], lm, m) do
    throw({:error, lm, {:arg_error, m}})
  end

  defp count_args([{:"(", _Llp} | toks0], lm, m) do
    {_Arg, toks1} = macro_arg(toks0, [], [])
    count_args(toks1, lm, m, 1)
  end

  defp count_args(_Toks, _Lm, _M) do
    :none
  end

  defp count_args([{:")", _Lrp} | _Toks], _Lm, _M, nbArgs) do
    nbArgs
  end

  defp count_args([[{:",", _Lc}, {:")", _Lrp}] | _Toks], lm, m, _NbArgs) do
    throw({:error, lm, {:arg_error, m}})
  end

  defp count_args([{:",", _Lc} | toks0], lm, m, nbArgs) do
    {_Arg, toks1} = macro_arg(toks0, [], [])
    count_args(toks1, lm, m, nbArgs + 1)
  end

  defp count_args([], lm, m, _NbArgs) do
    throw({:error, lm, {:arg_error, m}})
  end

  defp count_args(_Toks, lm, m, _NbArgs) do
    throw({:error, lm, {:mismatch, m}})
  end

  defp macro_arg([{:",", lc} | toks], [], arg) do
    {:lists.reverse(arg), [{:",", lc} | toks]}
  end

  defp macro_arg([{:")", lrp} | toks], [], arg) do
    {:lists.reverse(arg), [{:")", lrp} | toks]}
  end

  defp macro_arg([{:"(", llp} | toks], e, arg) do
    macro_arg(toks, [:")" | e], [{:"(", llp} | arg])
  end

  defp macro_arg([{:"<<", lls} | toks], e, arg) do
    macro_arg(toks, [:">>" | e], [{:"<<", lls} | arg])
  end

  defp macro_arg([{:"[", lls} | toks], e, arg) do
    macro_arg(toks, [:"]" | e], [{:"[", lls} | arg])
  end

  defp macro_arg([{:"{", llc} | toks], e, arg) do
    macro_arg(toks, [:"}" | e], [{:"{", llc} | arg])
  end

  defp macro_arg([{:begin, lb} | toks], e, arg) do
    macro_arg(toks, [:end | e], [{:begin, lb} | arg])
  end

  defp macro_arg([{:if, li} | toks], e, arg) do
    macro_arg(toks, [:end | e], [{:if, li} | arg])
  end

  defp macro_arg([{:case, lc} | toks], e, arg) do
    macro_arg(toks, [:end | e], [{:case, lc} | arg])
  end

  defp macro_arg([{:fun, lc} | [{:"(", _} | _] = toks], e, arg) do
    macro_arg(toks, [:end | e], [{:fun, lc} | arg])
  end

  defp macro_arg(
         [
           [{:fun, _} = fun, {:var, _, _} = name]
           | [
               {:"(", _}
               | _
             ] = toks
         ],
         e,
         arg
       ) do
    macro_arg(toks, [:end | e], [[name, fun] | arg])
  end

  defp macro_arg([{:receive, lr} | toks], e, arg) do
    macro_arg(toks, [:end | e], [{:receive, lr} | arg])
  end

  defp macro_arg([{:try, lr} | toks], e, arg) do
    macro_arg(toks, [:end | e], [{:try, lr} | arg])
  end

  defp macro_arg([{:cond, lr} | toks], e, arg) do
    macro_arg(toks, [:end | e], [{:cond, lr} | arg])
  end

  defp macro_arg([{rb, lrb} | toks], [rb | e], arg) do
    macro_arg(toks, e, [{rb, lrb} | arg])
  end

  defp macro_arg([t | toks], e, arg) do
    macro_arg(toks, e, [t | arg])
  end

  defp macro_arg([], _E, arg) do
    {:lists.reverse(arg), []}
  end

  defp expand_macro([{:var, _Lv, v} | ts], l, rest, bs) do
    case bs do
      %{^v => val} ->
        expand_arg(val, ts, l, rest, bs)

      _ ->
        [{:var, l, v} | expand_macro(ts, l, rest, bs)]
    end
  end

  defp expand_macro([[{:"?", _}, {:"?", _}, {:var, _Lv, v}] | ts], l, rest, bs) do
    case bs do
      %{^v => val} ->
        expand_arg(stringify(val, l), ts, l, rest, bs)

      _ ->
        [{:var, l, v} | expand_macro(ts, l, rest, bs)]
    end
  end

  defp expand_macro([t | ts], l, rest, bs) do
    [:erlang.setelement(2, t, l) | expand_macro(ts, l, rest, bs)]
  end

  defp expand_macro([], _L, rest, _Bs) do
    rest
  end

  defp expand_arg([a | as], ts, _L, rest, bs) do
    nextL = :erlang.element(2, a)
    [a | expand_arg(as, ts, nextL, rest, bs)]
  end

  defp expand_arg([], ts, l, rest, bs) do
    expand_macro(ts, l, rest, bs)
  end

  defp update_fun_name(token, r_epp(fname: toks0) = st)
       when is_list(toks0) do
    toks1 =
      try do
        expand_macros(toks0, r_epp(st, fname: :undefined))
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    case toks1 do
      [[{:atom, _, name}, {:"(", _}] | toks] ->
        fA = update_fun_name_1(toks, 1, {name, 0}, st)
        r_epp(st, fname: fA)

      [{:"?", _} | _] ->
        {:var, _, macro} = token
        throw({:error, loc(token), {:illegal_function_usage, macro}})

      _ when is_list(toks1) ->
        {:var, _, macro} = token
        throw({:error, loc(token), {:illegal_function, macro}})

      _ ->
        r_epp(st, fname: {:_, 0})
    end
  end

  defp update_fun_name(_Token, st) do
    st
  end

  defp update_fun_name_1([tok | toks], l, fA, st) do
    case classify_token(tok) do
      :comma ->
        cond do
          l === 1 ->
            {name, arity} = fA
            update_fun_name_1(toks, l, {name, arity + 1}, st)

          true ->
            update_fun_name_1(toks, l, fA, st)
        end

      :left ->
        update_fun_name_1(toks, l + 1, fA, st)

      :right when l === 1 ->
        fA

      :right ->
        update_fun_name_1(toks, l - 1, fA, st)

      :other ->
        case fA do
          {name, 0} ->
            update_fun_name_1(toks, l, {name, 1}, st)

          {_, _} ->
            update_fun_name_1(toks, l, fA, st)
        end
    end
  end

  defp update_fun_name_1([], _, fA, _) do
    fA
  end

  defp classify_token({c, _}) do
    classify_token_1(c)
  end

  defp classify_token(_) do
    :other
  end

  defp classify_token_1(:",") do
    :comma
  end

  defp classify_token_1(:"(") do
    :left
  end

  defp classify_token_1(:"{") do
    :left
  end

  defp classify_token_1(:"[") do
    :left
  end

  defp classify_token_1(:"<<") do
    :left
  end

  defp classify_token_1(:")") do
    :right
  end

  defp classify_token_1(:"}") do
    :right
  end

  defp classify_token_1(:"]") do
    :right
  end

  defp classify_token_1(:">>") do
    :right
  end

  defp classify_token_1(_) do
    :other
  end

  defp token_src({:dot, _}) do
    '.'
  end

  defp token_src({x, _}) when is_atom(x) do
    :erlang.atom_to_list(x)
  end

  defp token_src({:var, _, x}) do
    :erlang.atom_to_list(x)
  end

  defp token_src({:char, _, c}) do
    :io_lib.write_char(c)
  end

  defp token_src({:string, _, x}) do
    :io_lib.write_string(x)
  end

  defp token_src({_, _, x}) do
    :io_lib.format('~w', [x])
  end

  defp stringify1([]) do
    []
  end

  defp stringify1([t | tokens]) do
    [:io_lib.format(' ~ts', [token_src(t)]) | stringify1(tokens)]
  end

  defp stringify(ts, l) do
    [?\s | s] = :lists.flatten(stringify1(ts))
    [{:string, l, s}]
  end

  defp coalesce_strings([{:string, a, s} | tokens]) do
    coalesce_strings(tokens, a, [s])
  end

  defp coalesce_strings([t | tokens]) do
    [t | coalesce_strings(tokens)]
  end

  defp coalesce_strings([]) do
    []
  end

  defp coalesce_strings([{:string, _, s} | tokens], a, s0) do
    coalesce_strings(tokens, a, [s | s0])
  end

  defp coalesce_strings(tokens, a, s) do
    [
      {:string, a, :lists.append(:lists.reverse(s))}
      | coalesce_strings(tokens)
    ]
  end

  defp epp_request(epp) do
    wait_epp_reply(epp, :erlang.monitor(:process, epp))
  end

  defp epp_request(epp, req) do
    send(epp, {:epp_request, self(), req})
    wait_epp_reply(epp, :erlang.monitor(:process, epp))
  end

  defp epp_reply(from, rep) do
    send(from, {:epp_reply, self(), rep})
    :ok
  end

  defp wait_epp_reply(epp, mref) do
    receive do
      {:epp_reply, ^epp, rep} ->
        :erlang.demonitor(mref, [:flush])
        rep

      {:DOWN, ^mref, _, _, e} ->
        receive do
          {:epp_reply, ^epp, rep} ->
            rep
        after
          0 ->
            exit(e)
        end
    end
  end

  defp expand_var([?$ | _] = newName) do
    case (try do
            expand_var1(newName)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, expName} ->
        expName

      _ ->
        newName
    end
  end

  defp expand_var(newName) do
    newName
  end

  defp expand_var1(newName) do
    [[?$ | var] | rest] = :filename.split(newName)
    value = :os.getenv(var)
    true = value !== false
    {:ok, fname_join([value | rest])}
  end

  defp fname_join(['.' | [_ | _] = rest]) do
    fname_join(rest)
  end

  defp fname_join(components) do
    :filename.join(components)
  end

  defp loc_anno(line) when is_integer(line) do
    :erl_anno.new(line)
  end

  defp loc_anno({line, _Column}) do
    :erl_anno.new(line)
  end

  defp loc(token) do
    :erl_scan.location(token)
  end

  defp add_line(line, offset) when is_integer(line) do
    line + offset
  end

  defp add_line({line, column}, offset) do
    {line + offset, column}
  end

  defp start_loc(line) when is_integer(line) do
    1
  end

  defp start_loc({_Line, _Column}) do
    {1, 1}
  end

  defp line1() do
    :erl_anno.new(1)
  end

  defp get_line(anno) do
    :erl_anno.line(anno)
  end

  def interpret_file_attribute(forms) do
    interpret_file_attr(forms, 0, [])
  end

  defp interpret_file_attr(
         [
           {:attribute, anno, :file, {file, line}} = form
           | forms
         ],
         delta,
         fs
       ) do
    l = get_line(anno)
    generated = :erl_anno.generated(anno)

    cond do
      generated ->
        interpret_file_attr(forms, l + delta - line, fs)

      not generated ->
        case fs do
          [[_, ^file] | fs1] ->
            [form | interpret_file_attr(forms, 0, [file | fs1])]

          _ ->
            [form | interpret_file_attr(forms, 0, [file | fs])]
        end
    end
  end

  defp interpret_file_attr([form0 | forms], delta, fs) do
    f = fn anno ->
      line = :erl_anno.line(anno)
      :erl_anno.set_line(line + delta, anno)
    end

    form = :erl_parse.map_anno(f, form0)
    [form | interpret_file_attr(forms, delta, fs)]
  end

  defp interpret_file_attr([], _Delta, _Fs) do
    []
  end
end
