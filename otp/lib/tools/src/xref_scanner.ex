defmodule :m_xref_scanner do
  use Bitwise
  require Record

  Record.defrecord(:r_xref, :xref,
    version: 1,
    mode: :functions,
    variables: :not_set_up,
    modules: :dict.new(),
    applications: :dict.new(),
    releases: :dict.new(),
    library_path: [],
    libraries: :dict.new(),
    builtins_default: false,
    recurse_default: false,
    verbose_default: false,
    warnings_default: true
  )

  Record.defrecord(:r_xref_mod, :xref_mod,
    name: :"",
    app_name: [],
    dir: '',
    mtime: :undefined,
    builtins: :undefined,
    info: :undefined,
    no_unresolved: 0,
    data: :undefined
  )

  Record.defrecord(:r_xref_app, :xref_app, name: :"", rel_name: [], vsn: [], dir: '')
  Record.defrecord(:r_xref_rel, :xref_rel, name: :"", dir: '')
  Record.defrecord(:r_xref_lib, :xref_lib, name: :"", dir: '')

  Record.defrecord(:r_xref_var, :xref_var,
    name: :"",
    value: :undefined,
    vtype: :undefined,
    otype: :undefined,
    type: :undefined
  )

  def scan(chars) do
    case :erl_scan.string(chars) do
      {:ok, tokens, _Line} ->
        {:ok, lex(a1(tokens))}

      {:error, {line, module, info}, _EndLine} ->
        {:error, module.format_error(info), line}
    end
  end

  defp a1([[{:-, n}, {:integer, n, 1}] | l]) do
    [{:integer, n, -1} | a1(l)]
  end

  defp a1([t | l]) do
    [t | a1(l)]
  end

  defp a1([]) do
    []
  end

  defp lex([
         [{:atom, n, v1}, {:->, _}, {:atom, _, v2}]
         | l
       ]) do
    constant = {:constant, :unknown, :edge, {v1, v2}}
    [{:edge, n, constant} | lex(l)]
  end

  defp lex([
         [{:"{", n}, {:atom, _, v1}, {:",", _}, {:atom, _, v2}, {:"}", _}]
         | l
       ]) do
    constant = {:constant, :unknown, :edge, {v1, v2}}
    [{:edge, n, constant} | lex(l)]
  end

  defp lex([
         [
           {:atom, n, m},
           {:":", _},
           {:atom, _, f},
           {:/, _},
           {:integer, _, a},
           {:->, _},
           {:atom, _, m2},
           {:":", _},
           {:atom, _, f2},
           {:/, _},
           {:integer, _, a2}
         ]
         | l
       ]) do
    constant = {:constant, :Fun, :edge, {{m, f, a}, {m2, f2, a2}}}
    [{:edge, n, constant} | lex(l)]
  end

  defp lex([
         [{:atom, n, m}, {:":", _}, {:atom, _, f}, {:/, _}, {:integer, _, a}]
         | l
       ]) do
    constant = {:constant, :Fun, :vertex, {m, f, a}}
    [{:vertex, n, constant} | lex(l)]
  end

  defp lex([
         [
           {:"{", n},
           {:"{", _},
           {:atom, _, m},
           {:",", _},
           {:atom, _, f},
           {:",", _},
           {:integer, _, a},
           {:"}", _},
           {:",", _},
           {:"{", _},
           {:atom, _, m2},
           {:",", _},
           {:atom, _, f2},
           {:",", _},
           {:integer, _, a2},
           {:"}", _},
           {:"}", _}
         ]
         | l
       ]) do
    constant = {:constant, :Fun, :edge, {{m, f, a}, {m2, f2, a2}}}
    [{:edge, n, constant} | lex(l)]
  end

  defp lex([
         [
           {:"{", n},
           {:atom, _, m},
           {:",", _},
           {:atom, _, f},
           {:",", _},
           {:integer, _, a},
           {:"}", _}
         ]
         | l
       ]) do
    constant = {:constant, :Fun, :vertex, {m, f, a}}
    [{:vertex, n, constant} | lex(l)]
  end

  defp lex([[{:":", n1}, {:var, n2, decl}] | l]) do
    case is_type(decl) do
      false ->
        [[{:":", n1}, {:var, n2, decl}] | lex(l)]

      true ->
        [{:decl, n1, decl} | lex(l)]
    end
  end

  defp lex([[{:":", n}, {:=, _}] | l]) do
    [{:":=", n} | lex(l)]
  end

  defp lex([[{:||, n}, {:|, _}] | l]) do
    [{:|||, n} | lex(l)]
  end

  defp lex([v = {:var, n, var} | l]) do
    t =
      case is_type(var) do
        false ->
          v

        true ->
          {:cast, n, var}
      end

    [t | lex(l)]
  end

  defp lex([t | ts]) do
    [t | lex(ts)]
  end

  defp lex([]) do
    [{:"$end", :erl_anno.new(1 <<< 23)}]
  end

  defp is_type(:Rel) do
    true
  end

  defp is_type(:App) do
    true
  end

  defp is_type(:Mod) do
    true
  end

  defp is_type(:Fun) do
    true
  end

  defp is_type(:Lin) do
    true
  end

  defp is_type(:LLin) do
    true
  end

  defp is_type(:XLin) do
    true
  end

  defp is_type(:ELin) do
    true
  end

  defp is_type(:XXL) do
    true
  end

  defp is_type(_) do
    false
  end
end
