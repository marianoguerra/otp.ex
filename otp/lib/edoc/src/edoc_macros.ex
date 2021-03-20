defmodule :m_edoc_macros do
  use Bitwise
  import :edoc_report, only: [error: 3, report: 2, warning: 4]
  require Record
  Record.defrecord(:r_context, :context, dir: '', env: :undefined, opts: [])
  Record.defrecord(:r_doclet_gen, :doclet_gen, sources: [], app: [], modules: [])

  Record.defrecord(:r_doclet_toc, :doclet_toc,
    paths: :undefined,
    indir: :undefined
  )

  Record.defrecord(:r_module, :module,
    name: [],
    parameters: :none,
    functions: [],
    exports: [],
    attributes: [],
    records: [],
    encoding: :latin1
  )

  Record.defrecord(:r_env, :env,
    module: [],
    root: '',
    file_suffix: :undefined,
    apps: :undefined,
    modules: :undefined,
    app_default: :undefined,
    macros: [],
    includes: []
  )

  Record.defrecord(:r_comment, :comment,
    line: 0,
    text: :undefined
  )

  Record.defrecord(:r_entry, :entry,
    name: :undefined,
    args: [],
    line: 0,
    export: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_tag, :tag, name: :undefined, line: 0, origin: :comment, data: :undefined)
  Record.defrecord(:r_t_spec, :t_spec, name: :undefined, type: :undefined, defs: [])

  Record.defrecord(:r_t_typedef, :t_typedef,
    name: :undefined,
    args: :undefined,
    type: :undefined,
    defs: []
  )

  Record.defrecord(:r_t_throws, :t_throws,
    type: :undefined,
    defs: []
  )

  Record.defrecord(:r_t_def, :t_def,
    name: :undefined,
    type: :undefined
  )

  Record.defrecord(:r_t_name, :t_name, app: [], module: [], name: [])
  Record.defrecord(:r_t_var, :t_var, a: [], name: [])
  Record.defrecord(:r_t_type, :t_type, a: [], name: :undefined, args: [])
  Record.defrecord(:r_t_union, :t_union, a: [], types: [])
  Record.defrecord(:r_t_fun, :t_fun, a: [], args: :undefined, range: :undefined)
  Record.defrecord(:r_t_tuple, :t_tuple, a: [], types: [])
  Record.defrecord(:r_t_list, :t_list, a: [], type: :undefined)
  Record.defrecord(:r_t_nil, :t_nil, a: [])

  Record.defrecord(:r_t_nonempty_list, :t_nonempty_list,
    a: [],
    type: :undefined
  )

  Record.defrecord(:r_t_atom, :t_atom, a: [], val: :undefined)
  Record.defrecord(:r_t_integer, :t_integer, a: [], val: :undefined)
  Record.defrecord(:r_t_integer_range, :t_integer_range, a: [], from: :undefined, to: :undefined)
  Record.defrecord(:r_t_binary, :t_binary, a: [], base_size: 0, unit_size: 0)
  Record.defrecord(:r_t_float, :t_float, a: [], val: :undefined)
  Record.defrecord(:r_t_record, :t_record, a: [], name: :undefined, fields: [])
  Record.defrecord(:r_t_field, :t_field, a: [], name: :undefined, type: :undefined)
  Record.defrecord(:r_t_paren, :t_paren, a: [], type: :undefined)
  Record.defrecord(:r_t_map, :t_map, a: [], types: [])

  Record.defrecord(:r_t_map_field, :t_map_field,
    a: [],
    assoc_type: :undefined,
    k_type: :undefined,
    v_type: :undefined
  )

  def std_macros(env) do
    cond do
      r_env(env, :module) === [] ->
        []

      true ->
        [{:module, :erlang.atom_to_list(r_env(env, :module))}]
    end ++
      [
        {:date, &date_macro/3},
        {:docRoot, r_env(env, :root)},
        {:link, &link_macro/3},
        {:section, &section_macro/3},
        {:time, &time_macro/3},
        {:type, &type_macro/3},
        {:version, &version_macro/3}
      ]
  end

  def check_defs([{k, d} | ds])
      when is_atom(k) and
             is_list(d) do
    check_defs(ds)
  end

  def check_defs([x | _Ds]) do
    report('bad macro definition: ~P.', [x, 10])
    exit(:error)
  end

  def check_defs([]) do
    :ok
  end

  defp date_macro(_S, _Line, _Env) do
    :edoc_lib.datestr(:erlang.date())
  end

  defp time_macro(_S, _Line, _Env) do
    :edoc_lib.timestr(:erlang.time())
  end

  defp version_macro(s, line, env) do
    date_macro(s, line, env) ++ ' ' ++ time_macro(s, line, env)
  end

  defp link_macro(s, line, env) do
    {s1, s2} = :edoc_lib.split_at_stop(s)
    ref = :edoc_parser.parse_ref(s1, line)
    uRI = :edoc_refs.get_uri(ref, env)

    txt =
      cond do
        s2 === [] ->
          '<code>' ++ s1 ++ '</code>'

        true ->
          s2
      end

    target =
      case :edoc_refs.is_top(ref, env) do
        true ->
          ' target="_top"'

        false ->
          ''
      end

    :lists.flatten(:io_lib.fwrite('<a href="~ts"~ts>~ts</a>', [uRI, target, txt]))
  end

  defp section_macro(s, _Line, _Env) do
    s1 = :lists.reverse(:edoc_lib.strip_space(:lists.reverse(:edoc_lib.strip_space(s))))

    :lists.flatten(
      :io_lib.format(
        '<a href="#~ts">~ts</a>',
        [:edoc_lib.to_label(s1), s1]
      )
    )
  end

  defp type_macro(s, line, env) do
    s1 = 't()=' ++ s
    def__ = :edoc_parser.parse_typedef(s1, line)
    {r_t_typedef(type: t), _} = def__
    txt = :edoc_layout.type(:edoc_data.type(t, env))
    :lists.flatten(:io_lib.fwrite('<code>~ts</code>', [txt]))
  end

  def expand_tags(ts, env, where) do
    defs = :dict.from_list(:lists.reverse(r_env(env, :macros)))
    expand_tags(ts, defs, env, where)
  end

  defp expand_tags([r_tag(data: cs, line: l) = t | ts], defs, env, where) do
    [
      r_tag(t, data: expand_tag(cs, l, defs, env, where))
      | expand_tags(ts, defs, env, where)
    ]
  end

  defp expand_tags([t | ts], defs, env, where) do
    [t | expand_tags(ts, defs, env, where)]
  end

  defp expand_tags([], _, _, _) do
    []
  end

  defp expand_tag(cs, l, defs, env, where) do
    case (try do
            {:ok, expand_text(cs, l, defs, env, where)}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, cs1} ->
        :lists.reverse(cs1)

      {:EXIT, r} ->
        exit(r)

      {:error, l1, error} ->
        error(l1, where, error)
        exit(:error)

      other ->
        throw(other)
    end
  end

  Record.defrecord(:r_state, :state, where: :undefined, env: :undefined, seen: :undefined)

  defp expand_text(cs, l, defs, env, where) do
    st = r_state(where: where, env: env, seen: :sets.new())
    expand(cs, l, defs, st, [])
  end

  defp expand([?@, ?@ | cs], l, defs, st, as) do
    expand(cs, l, defs, st, [?@ | as])
  end

  defp expand([?@, ?{ | cs], l, defs, st, as) do
    expand(cs, l, defs, st, [?{ | as])
  end

  defp expand([?@, ?} | cs], l, defs, st, as) do
    expand(cs, l, defs, st, [?} | as])
  end

  defp expand([?{, ?@ | cs], l, defs, st, as) do
    expand_macro(cs, l, defs, st, as)
  end

  defp expand([?\n = c | cs], l, defs, st, as) do
    expand(cs, l + 1, defs, st, [c | as])
  end

  defp expand([c | cs], l, defs, st, as) do
    expand(cs, l, defs, st, [c | as])
  end

  defp expand([], _, _, _, as) do
    as
  end

  defp expand_macro(cs, l, defs, st, as) do
    {m, cs1, l1} = macro_name(cs, l)
    {arg, cs2, l2} = macro_content(cs1, l1)
    as1 = expand_macro_def(m, arg, l, defs, st, as)
    expand(cs2, l2, defs, st, as1)
  end

  defp expand_macro_def(m, arg, l, defs, st, as) do
    seen = r_state(st, :seen)

    case :sets.is_element(m, seen) do
      true ->
        throw_error(l, {'recursive macro expansion of {@~s}.', [m]})

      false ->
        arg1 = :lists.reverse(expand(arg, l, defs, st, []))
        defs1 = :dict.store(:"?", arg1, defs)
        st1 = r_state(st, seen: :sets.add_element(m, seen))

        case :dict.find(m, defs) do
          {:ok, def__} ->
            txt =
              cond do
                is_function(def__) ->
                  def__.(arg1, l, r_state(st1, :env))

                is_list(def__) ->
                  def__
              end

            expand(txt, l, defs1, st1, as)

          :error ->
            warning(l, r_state(st1, :where), 'undefined macro {@~s}.', [m])
            '??'
        end
    end
  end

  defp macro_name(cs, l) do
    macro_name(cs, [], l)
  end

  defp macro_name([c | cs], as, l) when c >= ?a and c <= ?z do
    macro_name_1(cs, [c | as], l)
  end

  defp macro_name([c | cs], as, l) when c >= ?A and c <= ?Z do
    macro_name_1(cs, [c | as], l)
  end

  defp macro_name([c | cs], as, l)
       when c >= 192 and c <= 255 and
              c !== 215 and c !== 247 do
    macro_name_1(cs, [c | as], l)
  end

  defp macro_name([?_ | cs], as, l) do
    macro_name_1(cs, [?_ | as], l)
  end

  defp macro_name([?? | cs], as, l) do
    macro_name_1(cs, [?? | as], l)
  end

  defp macro_name([?\s | _Cs], _As, l) do
    throw_error(l, :macro_name)
  end

  defp macro_name([?\t | _Cs], _As, l) do
    throw_error(l, :macro_name)
  end

  defp macro_name([?\n | _Cs], _As, l) do
    throw_error(l, :macro_name)
  end

  defp macro_name([c | _Cs], as, l) do
    throw_error(l, {:macro_name, [c | as]})
  end

  defp macro_name([], _As, l) do
    throw_error(l, :macro_name)
  end

  defp macro_name_1([c | cs], as, l) when c >= ?a and c <= ?z do
    macro_name_1(cs, [c | as], l)
  end

  defp macro_name_1([c | cs], as, l) when c >= ?A and c <= ?Z do
    macro_name_1(cs, [c | as], l)
  end

  defp macro_name_1([c | cs], as, l) when c >= ?0 and c <= ?9 do
    macro_name_1(cs, [c | as], l)
  end

  defp macro_name_1([c | cs], as, l)
       when c >= 192 and c <= 255 and
              c !== 215 and c !== 247 do
    macro_name_1(cs, [c | as], l)
  end

  defp macro_name_1([?_ | cs], as, l) do
    macro_name_1(cs, [?_ | as], l)
  end

  defp macro_name_1([?\s | cs], as, l) do
    macro_name_2(cs, as, l)
  end

  defp macro_name_1([?\t | cs], as, l) do
    macro_name_2(cs, as, l)
  end

  defp macro_name_1([?\n | cs], as, l) do
    macro_name_2(cs, as, l + 1)
  end

  defp macro_name_1([?} | _] = cs, as, l) do
    macro_name_3(cs, as, l)
  end

  defp macro_name_1([c | _Cs], as, l) do
    throw_error(l, {:macro_name, [c | as]})
  end

  defp macro_name_1([], _As, l) do
    throw_error(l, :unterminated_macro)
  end

  defp macro_name_2([?\s | cs], as, l) do
    macro_name_2(cs, as, l)
  end

  defp macro_name_2([?\t | cs], as, l) do
    macro_name_2(cs, as, l)
  end

  defp macro_name_2([?\n | cs], as, l) do
    macro_name_2(cs, as, l + 1)
  end

  defp macro_name_2([_ | _] = cs, as, l) do
    macro_name_3(cs, as, l)
  end

  defp macro_name_2([], _As, l) do
    throw_error(l, :unterminated_macro)
  end

  defp macro_name_3(cs, as, l) do
    {:erlang.list_to_atom(:lists.reverse(as)), cs, l}
  end

  defp macro_content(cs, l) do
    case (try do
            {:ok, macro_content(cs, [], l, 0)}
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, x} ->
        x

      {:EXIT, r} ->
        exit(r)

      :end ->
        throw_error(l, :unterminated_macro)

      other ->
        throw(other)
    end
  end

  defp macro_content([?@, ?@ | cs], as, l, n) do
    macro_content(cs, [?@, ?@ | as], l, n)
  end

  defp macro_content([?@, ?} | cs], as, l, n) do
    macro_content(cs, [?}, ?@ | as], l, n)
  end

  defp macro_content([?@, ?{ | cs], as, l, n) do
    macro_content(cs, [?{, ?@ | as], l, n)
  end

  defp macro_content([?{, ?@ | cs], as, l, n) do
    macro_content(cs, [?@, ?{ | as], l, n + 1)
  end

  defp macro_content([?} | cs], as, l, 0) do
    {:lists.reverse(as), cs, l}
  end

  defp macro_content([?} | cs], as, l, n) do
    macro_content(cs, [?} | as], l, n - 1)
  end

  defp macro_content([?\n = c | cs], as, l, n) do
    macro_content(cs, [c | as], l + 1, n)
  end

  defp macro_content([c | cs], as, l, n) do
    macro_content(cs, [c | as], l, n)
  end

  defp macro_content([], _As, _L, _N) do
    throw(:end)
  end

  defp throw_error(l, :unterminated_macro) do
    throw_error(l, {'unexpected end of macro.', []})
  end

  defp throw_error(l, :macro_name) do
    throw_error(l, {'missing macro name.', []})
  end

  defp throw_error(l, {:macro_name, s}) do
    throw_error(l, {'bad macro name: \'@~s...\'.', [:lists.reverse(s)]})
  end

  defp throw_error(l, d) do
    throw({:error, l, d})
  end
end
