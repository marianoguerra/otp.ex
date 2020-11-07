defmodule :m_epp_dodger do
  use Bitwise

  def parse_file(file) do
    parse_file(file, [])
  end

  def parse_file(file, options) do
    parse_file(file, &parse/3, options)
  end

  def quick_parse_file(file) do
    quick_parse_file(file, [])
  end

  def quick_parse_file(file, options) do
    parse_file(file, &quick_parse/3, options ++ [:no_fail])
  end

  defp parse_file(file, parser, options) do
    case do_parse_file(:utf8, file, parser, options) do
      {:ok, forms} = ret ->
        case find_invalid_unicode(forms) do
          :none ->
            ret

          :invalid_unicode ->
            case :epp.read_encoding(file) do
              :utf8 ->
                ret

              _ ->
                do_parse_file(:latin1, file, parser, options)
            end
        end

      else__ ->
        else__
    end
  end

  defp do_parse_file(defEncoding, file, parser, options) do
    case :file.open(file, [:read]) do
      {:ok, dev} ->
        _ = :epp.set_encoding(dev, defEncoding)

        try do
          parser.(dev, 1, options)
        after
          :ok = :file.close(dev)
        end

      {:error, error} ->
        {:error, {0, :file, error}}
    end
  end

  defp find_invalid_unicode([h | t]) do
    case h do
      {:error, {_Line, :file_io_server, :invalid_unicode}} ->
        :invalid_unicode

      _Other ->
        find_invalid_unicode(t)
    end
  end

  defp find_invalid_unicode([]) do
    :none
  end

  def parse(dev) do
    parse(dev, 1)
  end

  def parse(dev, l) do
    parse(dev, l, [])
  end

  def parse(dev, l0, options) do
    parse(dev, l0, &parse_form/3, options)
  end

  def quick_parse(dev) do
    quick_parse(dev, 1)
  end

  def quick_parse(dev, l) do
    quick_parse(dev, l, [])
  end

  def quick_parse(dev, l0, options) do
    parse(dev, l0, &quick_parse_form/3, options)
  end

  defp parse(dev, l0, parser, options) do
    parse(dev, l0, [], parser, options)
  end

  defp parse(dev, l0, fs, parser, options) do
    case parser.(dev, l0, options) do
      {:ok, :none, l1} ->
        parse(dev, l1, fs, parser, options)

      {:ok, f, l1} ->
        parse(dev, l1, [f | fs], parser, options)

      {:error, ioErr, l1} ->
        parse(dev, l1, [{:error, ioErr} | fs], parser, options)

      {:eof, _L1} ->
        {:ok, :lists.reverse(fs)}
    end
  end

  def parse_form(dev, l0) do
    parse_form(dev, l0, [])
  end

  def parse_form(dev, l0, options) do
    parse_form(dev, l0, &normal_parser/2, options)
  end

  def quick_parse_form(dev, l0) do
    quick_parse_form(dev, l0, [])
  end

  def quick_parse_form(dev, l0, options) do
    parse_form(dev, l0, &quick_parser/2, options)
  end

  require Record
  Record.defrecord(:r_opt, :opt, clever: false)

  defp parse_form(dev, l0, parser, options) do
    noFail = :proplists.get_bool(:no_fail, options)
    opt = r_opt(clever: :proplists.get_bool(:clever, options))

    case :io.scan_erl_form(dev, '', l0) do
      {:ok, ts, l1} ->
        case (try do
                {:ok, parser.(ts, opt)}
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, term} ->
            {:error, io_error(l1, {:unknown, term}), l1}

          {:error, term} ->
            ioErr = io_error(l1, term)
            {:error, ioErr, l1}

          {:parse_error, _IoErr} when noFail ->
            {:ok,
             :erl_syntax.set_pos(
               :erl_syntax.text(tokens_to_string(ts)),
               start_pos(ts, l1)
             ), l1}

          {:parse_error, ioErr} ->
            {:error, ioErr, l1}

          {:ok, f} ->
            {:ok, f, l1}
        end

      {:error, _IoErr, _L1} = err ->
        err

      {:error, _Reason} ->
        {:eof, l0}

      {:eof, _L1} = eof ->
        eof
    end
  end

  defp io_error(l, desc) do
    {l, :epp_dodger, desc}
  end

  defp start_pos([t | _Ts], _L) do
    :erl_anno.line(:erlang.element(2, t))
  end

  defp start_pos([], l) do
    l
  end

  defp parse_tokens(ts) do
    parse_tokens(ts, &fix_form/1)
  end

  defp parse_tokens(ts, fix) do
    case :erl_parse.parse_form(ts) do
      {:ok, form} ->
        form

      {:error, ioErr} ->
        case fix.(ts) do
          {:form, form} ->
            form

          {:retry, ts1, fix1} ->
            parse_tokens(ts1, fix1)

          :error ->
            throw({:parse_error, ioErr})
        end
    end
  end

  defp quick_parser(ts, _Opt) do
    filter_form(parse_tokens(quickscan_form(ts)))
  end

  defp quickscan_form([[{:-, _L}, {:atom, la, :define}] | _Ts]) do
    kill_form(la)
  end

  defp quickscan_form([[{:-, _L}, {:atom, la, :undef}] | _Ts]) do
    kill_form(la)
  end

  defp quickscan_form([[{:-, _L}, {:atom, la, :include}] | _Ts]) do
    kill_form(la)
  end

  defp quickscan_form([
         [{:-, _L}, {:atom, la, :include_lib}]
         | _Ts
       ]) do
    kill_form(la)
  end

  defp quickscan_form([[{:-, _L}, {:atom, la, :ifdef}] | _Ts]) do
    kill_form(la)
  end

  defp quickscan_form([[{:-, _L}, {:atom, la, :ifndef}] | _Ts]) do
    kill_form(la)
  end

  defp quickscan_form([[{:-, _L}, {:if, la}] | _Ts]) do
    kill_form(la)
  end

  defp quickscan_form([[{:-, _L}, {:atom, la, :elif}] | _Ts]) do
    kill_form(la)
  end

  defp quickscan_form([[{:-, _L}, {:atom, la, :else}] | _Ts]) do
    kill_form(la)
  end

  defp quickscan_form([[{:-, _L}, {:atom, la, :endif}] | _Ts]) do
    kill_form(la)
  end

  defp quickscan_form([
         [{:-, l}, {:"?", _}, {type, _, _} = n]
         | [
             {:"(", _}
             | _
           ] = ts
       ])
       when type === :atom or type === :var do
    quickscan_macros_1(n, ts, [{:-, l}])
  end

  defp quickscan_form([
         [{:"?", _L}, {type, _, _} = n]
         | [
             {:"(", _}
             | _
           ] = ts
       ])
       when type === :atom or type === :var do
    quickscan_macros_1(n, ts, [])
  end

  defp quickscan_form(ts) do
    quickscan_macros(ts)
  end

  defp kill_form(l) do
    [
      {:atom, l, :"?preprocessor declaration?"},
      {:"(", l},
      {:")", l},
      {:->, l},
      {:atom, l, :kill},
      {:dot, l}
    ]
  end

  defp quickscan_macros(ts) do
    quickscan_macros(ts, [])
  end

  defp quickscan_macros(
         [[{:"?", _}, {type, _, a}] | ts],
         [{:string, l, s} | as]
       )
       when type === :atom or type === :var do
    {_, ts1} = skip_macro_args(ts)
    s1 = s ++ quick_macro_string(a)
    quickscan_macros(ts1, [{:string, l, s1} | as])
  end

  defp quickscan_macros(
         [
           [{:"?", _}, {type, _, _} = n]
           | [
               {:"(", _}
               | _
             ] = ts
         ],
         [{:":", _} | _] = as
       )
       when type === :atom or type === :var do
    ts1 =
      case skip_macro_args(ts) do
        {_, [{:->, _} | _] = ts2} ->
          ts2

        {_, [{:when, _} | _] = ts2} ->
          ts2

        _ ->
          ts
      end

    quickscan_macros_1(n, ts1, as)
  end

  defp quickscan_macros([[{:"?", _}, {type, _, _} = n] | ts], as)
       when type === :atom or type === :var do
    {_, ts1} = skip_macro_args(ts)
    quickscan_macros_1(n, ts1, as)
  end

  defp quickscan_macros([t | ts], as) do
    quickscan_macros(ts, [t | as])
  end

  defp quickscan_macros([], as) do
    :lists.reverse(as)
  end

  defp quickscan_macros_1({_Type, _, a}, [{:string, l, s} | ts], as) do
    s1 = quick_macro_string(a) ++ s
    quickscan_macros(ts, [{:string, l, s1} | as])
  end

  defp quickscan_macros_1({_Type, l, a}, ts, as) do
    quickscan_macros(
      ts,
      [{:atom, l, quick_macro_atom(a)} | as]
    )
  end

  defp quick_macro_atom(a) do
    :erlang.list_to_atom('?' ++ :erlang.atom_to_list(a))
  end

  defp quick_macro_string(a) do
    '(?' ++ :erlang.atom_to_list(a) ++ ')'
  end

  defp skip_macro_args([{:"(", _} = t | ts]) do
    skip_macro_args(ts, [:")"], [t])
  end

  defp skip_macro_args(ts) do
    {[], ts}
  end

  defp skip_macro_args([{:"(", _} = t | ts], es, as) do
    skip_macro_args(ts, [:")" | es], [t | as])
  end

  defp skip_macro_args([{:"{", _} = t | ts], es, as) do
    skip_macro_args(ts, [:"}" | es], [t | as])
  end

  defp skip_macro_args([{:"[", _} = t | ts], es, as) do
    skip_macro_args(ts, [:"]" | es], [t | as])
  end

  defp skip_macro_args([{:"<<", _} = t | ts], es, as) do
    skip_macro_args(ts, [:">>" | es], [t | as])
  end

  defp skip_macro_args([{:begin, _} = t | ts], es, as) do
    skip_macro_args(ts, [:end | es], [t | as])
  end

  defp skip_macro_args([{:if, _} = t | ts], es, as) do
    skip_macro_args(ts, [:end | es], [t | as])
  end

  defp skip_macro_args([{:case, _} = t | ts], es, as) do
    skip_macro_args(ts, [:end | es], [t | as])
  end

  defp skip_macro_args([{:receive, _} = t | ts], es, as) do
    skip_macro_args(ts, [:end | es], [t | as])
  end

  defp skip_macro_args([{:try, _} = t | ts], es, as) do
    skip_macro_args(ts, [:end | es], [t | as])
  end

  defp skip_macro_args([{e, _} = t | ts], [e], as) do
    {:lists.reverse([t | as]), ts}
  end

  defp skip_macro_args([{e, _} = t | ts], [e | es], as) do
    skip_macro_args(ts, es, [t | as])
  end

  defp skip_macro_args([t | ts], es, as) do
    skip_macro_args(ts, es, [t | as])
  end

  defp skip_macro_args([], _Es, _As) do
    throw({:error, :macro_args})
  end

  defp filter_form(
         {:function, _, :"?preprocessor declaration?", _,
          [{:clause, _, [], [], [{:atom, _, :kill}]}]}
       ) do
    :none
  end

  defp filter_form(t) do
    t
  end

  defp normal_parser(ts0, opt) do
    case scan_form(ts0, opt) do
      ts when is_list(ts) ->
        rewrite_form(parse_tokens(ts))

      node ->
        node
    end
  end

  defp scan_form([[{:-, _L}, {:atom, la, :define}] | ts], opt) do
    [
      [
        {:atom, la, :"?preprocessor declaration?"},
        {:"(", la},
        {:")", la},
        {:->, la},
        {:atom, la, :define}
      ]
      | scan_macros(ts, opt)
    ]
  end

  defp scan_form([[{:-, _L}, {:atom, la, :undef}] | ts], opt) do
    [
      [
        {:atom, la, :"?preprocessor declaration?"},
        {:"(", la},
        {:")", la},
        {:->, la},
        {:atom, la, :undef}
      ]
      | scan_macros(ts, opt)
    ]
  end

  defp scan_form(
         [[{:-, _L}, {:atom, la, :include}] | ts],
         opt
       ) do
    [
      [
        {:atom, la, :"?preprocessor declaration?"},
        {:"(", la},
        {:")", la},
        {:->, la},
        {:atom, la, :include}
      ]
      | scan_macros(ts, opt)
    ]
  end

  defp scan_form(
         [[{:-, _L}, {:atom, la, :include_lib}] | ts],
         opt
       ) do
    [
      [
        {:atom, la, :"?preprocessor declaration?"},
        {:"(", la},
        {:")", la},
        {:->, la},
        {:atom, la, :include_lib}
      ]
      | scan_macros(ts, opt)
    ]
  end

  defp scan_form([[{:-, _L}, {:atom, la, :ifdef}] | ts], opt) do
    [
      [
        {:atom, la, :"?preprocessor declaration?"},
        {:"(", la},
        {:")", la},
        {:->, la},
        {:atom, la, :ifdef}
      ]
      | scan_macros(ts, opt)
    ]
  end

  defp scan_form([[{:-, _L}, {:atom, la, :ifndef}] | ts], opt) do
    [
      [
        {:atom, la, :"?preprocessor declaration?"},
        {:"(", la},
        {:")", la},
        {:->, la},
        {:atom, la, :ifndef}
      ]
      | scan_macros(ts, opt)
    ]
  end

  defp scan_form([[{:-, _L}, {:if, la}] | ts], opt) do
    [
      [
        {:atom, la, :"?preprocessor declaration?"},
        {:"(", la},
        {:")", la},
        {:->, la},
        {:atom, la, :if}
      ]
      | scan_macros(ts, opt)
    ]
  end

  defp scan_form([[{:-, _L}, {:atom, la, :elif}] | ts], opt) do
    [
      [
        {:atom, la, :"?preprocessor declaration?"},
        {:"(", la},
        {:")", la},
        {:->, la},
        {:atom, la, :elif}
      ]
      | scan_macros(ts, opt)
    ]
  end

  defp scan_form([[{:-, _L}, {:atom, la, :else}] | ts], opt) do
    [
      [
        {:atom, la, :"?preprocessor declaration?"},
        {:"(", la},
        {:")", la},
        {:->, la},
        {:atom, la, :else}
      ]
      | scan_macros(ts, opt)
    ]
  end

  defp scan_form([[{:-, _L}, {:atom, la, :endif}] | ts], opt) do
    [
      [
        {:atom, la, :"?preprocessor declaration?"},
        {:"(", la},
        {:")", la},
        {:->, la},
        {:atom, la, :endif}
      ]
      | scan_macros(ts, opt)
    ]
  end

  defp scan_form([[{:-, _L}, {:atom, la, :error}] | ts], _Opt) do
    desc = build_info_string('-error', ts)
    errorInfo = {la, :epp_dodger, {:error, desc}}
    :erl_syntax.error_marker(errorInfo)
  end

  defp scan_form(
         [[{:-, _L}, {:atom, la, :warning}] | ts],
         _Opt
       ) do
    desc = build_info_string('-warning', ts)
    errorInfo = {la, :epp_dodger, {:warning, desc}}
    :erl_syntax.error_marker(errorInfo)
  end

  defp scan_form(
         [
           [{:-, l}, {:"?", l1}, {type, _, _} = n]
           | [
               {:"(", _}
               | _
             ] = ts
         ],
         opt
       )
       when type === :atom or type === :var do
    macro(l1, n, ts, [{:-, l}], opt)
  end

  defp scan_form(
         [
           [{:"?", l}, {type, _, _} = n]
           | [
               {:"(", _}
               | _
             ] = ts
         ],
         opt
       )
       when type === :atom or type === :var do
    macro(l, n, ts, [], opt)
  end

  defp scan_form(ts, opt) do
    scan_macros(ts, opt)
  end

  defp build_info_string(prefix, ts0) do
    ts = :lists.droplast(ts0)
    string = :lists.droplast(tokens_to_string(ts))
    prefix ++ ' ' ++ string ++ '.'
  end

  defp scan_macros(ts, opt) do
    scan_macros(ts, [], opt)
  end

  defp scan_macros(
         [[{:"?", _} = m, {type, _, _} = n] | ts],
         [{:string, l, _} = s | as],
         r_opt(clever: true) = opt
       )
       when type === :atom or type === :var do
    scan_macros([[m, n] | ts], [[{:++, l}, s] | as], opt)
  end

  defp scan_macros(
         [
           [{:"?", l}, {type, _, _} = n]
           | [
               {:"(", _}
               | _
             ] = ts
         ],
         [{:":", _} | _] = as,
         opt
       )
       when type === :atom or type === :var do
    {args, rest} = skip_macro_args(ts)

    case rest do
      [{:->, _} | _] ->
        macro_call(args, l, n, rest, as, opt)

      [{:when, _} | _] ->
        macro_call(args, l, n, rest, as, opt)

      _ ->
        macro(l, n, ts, as, opt)
    end
  end

  defp scan_macros(
         [
           [{:"?", l}, {type, _, _} = n]
           | [
               {:"(", _}
               | _
             ] = ts
         ],
         as,
         opt
       )
       when type === :atom or type === :var do
    {args, rest} = skip_macro_args(ts)
    macro_call(args, l, n, rest, as, opt)
  end

  defp scan_macros([[{:"?", l}, {type, _, _} = n] | ts], as, opt)
       when type === :atom or type === :var do
    macro(l, n, ts, as, opt)
  end

  defp scan_macros([t | ts], as, opt) do
    scan_macros(ts, [t | as], opt)
  end

  defp scan_macros([], as, _Opt) do
    :lists.reverse(as)
  end

  defp macro(l, {type, _, a}, rest, as, opt) do
    scan_macros_1([], rest, [{:atom, l, macro_atom(type, a)} | as], opt)
  end

  defp macro_call([{:"(", _}, {:")", _}], l, {_, ln, _} = n, rest, as, opt) do
    {open, close} = parentheses(as)

    scan_macros_1(
      [],
      rest,
      :lists.reverse(
        open ++ [{:atom, l, :"? <macro> ("}, {:"(", l}, n, {:")", ln}] ++ close,
        as
      ),
      opt
    )
  end

  defp macro_call([{:"(", _} | args], l, {_, ln, _} = n, rest, as, opt) do
    {open, close} = parentheses(as)

    scan_macros_1(
      args ++ close,
      rest,
      :lists.reverse(
        open ++ [{:atom, l, :"? <macro> ("}, {:"(", l}, n, {:",", ln}],
        as
      ),
      opt
    )
  end

  defp macro_atom(:atom, a) do
    :erlang.list_to_atom('? ' ++ :erlang.atom_to_list(a))
  end

  defp macro_atom(:var, a) do
    :erlang.list_to_atom('?,' ++ :erlang.atom_to_list(a))
  end

  defp parentheses([{:string, _, _} | _]) do
    {[], []}
  end

  defp parentheses(_) do
    {[{:"(", 0}], [{:")", 0}]}
  end

  defp scan_macros_1(args, [{:string, l, _} | _] = rest, as, r_opt(clever: true) = opt) do
    scan_macros(args ++ [{:++, l} | rest], as, opt)
  end

  defp scan_macros_1(args, rest, as, opt) do
    scan_macros(args ++ rest, as, opt)
  end

  defp rewrite_form(
         {:function, l, :"?preprocessor declaration?", _,
          [{:clause, _, [], [], [{:call, _, a, as}]}]}
       ) do
    :erl_syntax.set_pos(
      :erl_syntax.attribute(
        a,
        rewrite_list(as)
      ),
      l
    )
  end

  defp rewrite_form({:function, l, :"?preprocessor declaration?", _, [{:clause, _, [], [], [a]}]}) do
    :erl_syntax.set_pos(:erl_syntax.attribute(a), l)
  end

  defp rewrite_form(t) do
    rewrite(t)
  end

  defp rewrite_list([t | ts]) do
    [rewrite(t) | rewrite_list(ts)]
  end

  defp rewrite_list([]) do
    []
  end

  defp rewrite(node) do
    case :erl_syntax.type(node) do
      :atom ->
        case :erlang.atom_to_list(:erl_syntax.atom_value(node)) do
          '? ' ++ as ->
            a1 = :erlang.list_to_atom(as)
            n = :erl_syntax.copy_pos(node, :erl_syntax.atom(a1))
            :erl_syntax.copy_pos(node, :erl_syntax.macro(n))

          '?,' ++ as ->
            a1 = :erlang.list_to_atom(as)
            n = :erl_syntax.copy_pos(node, :erl_syntax.variable(a1))
            :erl_syntax.copy_pos(node, :erl_syntax.macro(n))

          _ ->
            node
        end

      :application ->
        f = :erl_syntax.application_operator(node)

        case :erl_syntax.type(f) do
          :atom ->
            case :erl_syntax.atom_value(f) do
              :"? <macro> (" ->
                [a | as] = :erl_syntax.application_arguments(node)
                m = :erl_syntax.macro(a, rewrite_list(as))
                :erl_syntax.copy_pos(node, m)

              _ ->
                rewrite_1(node)
            end

          _ ->
            rewrite_1(node)
        end

      _ ->
        rewrite_1(node)
    end
  end

  defp rewrite_1(node) do
    case :erl_syntax.subtrees(node) do
      [] ->
        node

      gs ->
        node1 =
          :erl_syntax.make_tree(
            :erl_syntax.type(node),
            for ts <- gs do
              for t <- ts do
                rewrite(t)
              end
            end
          )

        :erl_syntax.copy_pos(node, node1)
    end
  end

  defp fix_form(
         [
           [
             {:atom, _, :"?preprocessor declaration?"},
             {:"(", _},
             {:")", _},
             {:->, _},
             {:atom, _, :define},
             {:"(", _}
           ]
           | _
         ] = ts
       ) do
    case :lists.reverse(ts) do
      [[{:dot, _}, {:")", _}] | _] ->
        {:retry, ts, &fix_define/1}

      [{:dot, l} | ts1] ->
        ts2 = :lists.reverse([[{:dot, l}, {:")", l}] | ts1])
        {:retry, ts2, &fix_define/1}

      _ ->
        :error
    end
  end

  defp fix_form(_Ts) do
    :error
  end

  defp fix_define([
         [
           {:atom, l, :"?preprocessor declaration?"},
           {:"(", _},
           {:")", _},
           {:->, _},
           {:atom, la, :define},
           {:"(", _},
           n,
           {:",", _}
         ]
         | ts
       ]) do
    [[{:dot, _}, {:")", _}] | ts1] = :lists.reverse(ts)
    s = tokens_to_string(:lists.reverse(ts1))
    a = :erl_syntax.set_pos(:erl_syntax.atom(:define), la)
    txt = :erl_syntax.set_pos(:erl_syntax.text(s), la)

    {:form,
     :erl_syntax.set_pos(
       :erl_syntax.attribute(a, [n, txt]),
       l
     )}
  end

  defp fix_define(_Ts) do
    :error
  end

  def tokens_to_string([{:atom, _, a} | ts]) do
    :io_lib.write_atom(a) ++ ' ' ++ tokens_to_string(ts)
  end

  def tokens_to_string([{:string, _, s} | ts]) do
    :io_lib.write_string(s) ++ ' ' ++ tokens_to_string(ts)
  end

  def tokens_to_string([{:char, _, c} | ts]) do
    :io_lib.write_char(c) ++ ' ' ++ tokens_to_string(ts)
  end

  def tokens_to_string([{:float, _, f} | ts]) do
    :erlang.float_to_list(f) ++ ' ' ++ tokens_to_string(ts)
  end

  def tokens_to_string([{:integer, _, n} | ts]) do
    :erlang.integer_to_list(n) ++ ' ' ++ tokens_to_string(ts)
  end

  def tokens_to_string([{:var, _, a} | ts]) do
    :erlang.atom_to_list(a) ++ ' ' ++ tokens_to_string(ts)
  end

  def tokens_to_string([{:dot, _} | ts]) do
    '.\n' ++ tokens_to_string(ts)
  end

  def tokens_to_string([{a, _} | ts]) do
    :erlang.atom_to_list(a) ++ ' ' ++ tokens_to_string(ts)
  end

  def tokens_to_string([]) do
    ''
  end

  def format_error(:macro_args) do
    errormsg('macro call missing end parenthesis')
  end

  def format_error({:error, error}) do
    error
  end

  def format_error({:warning, error}) do
    error
  end

  def format_error({:unknown, reason}) do
    errormsg(:io_lib.format('unknown error: ~tP', [reason, 15]))
  end

  defp errormsg(string) do
    :io_lib.format('~s: ~ts', [:epp_dodger, string])
  end
end
