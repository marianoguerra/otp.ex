defmodule :m_edoc_types do
  use Bitwise
  require Record
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

  Record.defrecord(:r_xmlDecl, :xmlDecl,
    vsn: :undefined,
    encoding: :undefined,
    standalone: :undefined,
    attributes: :undefined
  )

  Record.defrecord(:r_xmlAttribute, :xmlAttribute,
    name: :undefined,
    expanded_name: [],
    nsinfo: [],
    namespace: [],
    parents: [],
    pos: :undefined,
    language: [],
    value: :undefined,
    normalized: :undefined
  )

  Record.defrecord(:r_xmlNamespace, :xmlNamespace,
    default: [],
    nodes: []
  )

  Record.defrecord(:r_xmlNsNode, :xmlNsNode,
    parents: [],
    pos: :undefined,
    prefix: :undefined,
    uri: []
  )

  Record.defrecord(:r_xmlElement, :xmlElement,
    name: :undefined,
    expanded_name: [],
    nsinfo: [],
    namespace: :EFE_TODO_NESTED_RECORD,
    parents: [],
    pos: :undefined,
    attributes: [],
    content: [],
    language: '',
    xmlbase: '',
    elementdef: :undeclared
  )

  Record.defrecord(:r_xmlText, :xmlText,
    parents: [],
    pos: :undefined,
    language: [],
    value: :undefined,
    type: :text
  )

  Record.defrecord(:r_xmlComment, :xmlComment,
    parents: [],
    pos: :undefined,
    language: [],
    value: :undefined
  )

  Record.defrecord(:r_xmlPI, :xmlPI,
    name: :undefined,
    parents: [],
    pos: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_xmlDocument, :xmlDocument, content: :undefined)

  Record.defrecord(:r_xmlContext, :xmlContext,
    axis_type: :forward,
    context_node: :undefined,
    context_position: 1,
    nodeset: [],
    bindings: [],
    functions: [],
    namespace: [],
    whole_document: :undefined
  )

  Record.defrecord(:r_xmlNode, :xmlNode, type: :element, node: :undefined, parents: [], pos: 1)

  Record.defrecord(:r_xmlObj, :xmlObj,
    type: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_xmerl_fun_states, :xmerl_fun_states,
    event: :undefined,
    hook: :undefined,
    rules: :undefined,
    fetch: :undefined,
    cont: :undefined
  )

  Record.defrecord(:r_xmerl_scanner, :xmerl_scanner,
    encoding: :undefined,
    standalone: :no,
    environment: :prolog,
    declarations: [],
    doctype_name: :undefined,
    doctype_DTD: :internal,
    comments: true,
    document: false,
    default_attrs: false,
    rules: :undefined,
    keep_rules: false,
    namespace_conformant: false,
    xmlbase: :undefined,
    xmlbase_cache: :undefined,
    fetch_path: [],
    filename: :file_name_unknown,
    validation: :off,
    schemaLocation: [],
    space: :preserve,
    event_fun: :undefined,
    hook_fun: :undefined,
    acc_fun: :undefined,
    fetch_fun: :undefined,
    close_fun: :undefined,
    continuation_fun: :undefined,
    rules_read_fun: :undefined,
    rules_write_fun: :undefined,
    rules_delete_fun: :undefined,
    user_state: :undefined,
    fun_states: :EFE_TODO_NESTED_RECORD,
    entity_references: [],
    text_decl: false,
    quiet: false,
    col: 1,
    line: 1,
    common_data: []
  )

  Record.defrecord(:r_xmerl_event, :xmerl_event,
    event: :undefined,
    line: :undefined,
    col: :undefined,
    pos: :undefined,
    data: :undefined
  )

  def is_predefined(:cons, 2) do
    true
  end

  def is_predefined(:deep_string, 0) do
    true
  end

  def is_predefined(f, a) do
    :erl_internal.is_type(f, a)
  end

  def is_new_predefined(:map, 0) do
    true
  end

  def is_new_predefined(_, _) do
    false
  end

  def to_ref(r_t_typedef(name: n)) do
    to_ref(n)
  end

  def to_ref(r_t_def(name: n)) do
    to_ref(n)
  end

  def to_ref(r_t_type(name: n)) do
    to_ref(n)
  end

  def to_ref(r_t_name(module: [], name: n)) do
    :edoc_refs.type(n)
  end

  def to_ref(r_t_name(app: [], module: m, name: n)) do
    :edoc_refs.type(m, n)
  end

  def to_ref(r_t_name(app: a, module: m, name: n)) do
    :edoc_refs.type(a, m, n)
  end

  def to_label(n) do
    :edoc_refs.to_label(to_ref(n))
  end

  defp get_uri(name, env) do
    :edoc_refs.get_uri(to_ref(name), env)
  end

  def to_xml(r_t_var(name: n), _Env) do
    {:typevar, [{:name, :erlang.atom_to_list(n)}], []}
  end

  def to_xml(r_t_name(module: [], name: n), _Env) do
    {:erlangName, [{:name, :erlang.atom_to_list(n)}], []}
  end

  def to_xml(r_t_name(app: [], module: m, name: n), _Env) do
    {:erlangName, [{:module, :erlang.atom_to_list(m)}, {:name, :erlang.atom_to_list(n)}], []}
  end

  def to_xml(r_t_name(app: a, module: m, name: n), _Env) do
    {:erlangName,
     [
       {:app, :erlang.atom_to_list(a)},
       {:module, :erlang.atom_to_list(m)},
       {:name, :erlang.atom_to_list(n)}
     ], []}
  end

  def to_xml(r_t_type(name: n, args: as), env) do
    predef =
      case n do
        r_t_name(module: [], name: t) ->
          nArgs = length(as)
          is_predefined(t, nArgs)

        _ ->
          false
      end

    hRef =
      case predef do
        true ->
          []

        false ->
          [{:href, get_uri(n, env)}]
      end

    {:abstype, hRef, [to_xml(n, env) | map(&wrap_utype/2, as, env)]}
  end

  def to_xml(r_t_fun(args: as, range: t), env) do
    {:fun,
     [
       {:argtypes, map(&wrap_utype/2, as, env)},
       wrap_utype(
         t,
         env
       )
     ]}
  end

  def to_xml(r_t_map(types: ts), env) do
    {:map, map(&to_xml/2, ts, env)}
  end

  def to_xml(r_t_map_field(assoc_type: aT, k_type: k, v_type: v), env) do
    {:map_field, [{:assoc_type, aT}], [wrap_utype(k, env), wrap_utype(v, env)]}
  end

  def to_xml(r_t_tuple(types: ts), env) do
    {:tuple, map(&wrap_utype/2, ts, env)}
  end

  def to_xml(r_t_list(type: t), env) do
    {:list, [wrap_utype(t, env)]}
  end

  def to_xml(r_t_nil(), _Env) do
    nil
  end

  def to_xml(r_t_paren(type: t), env) do
    {:paren, [wrap_utype(t, env)]}
  end

  def to_xml(r_t_nonempty_list(type: t), env) do
    {:nonempty_list, [wrap_utype(t, env)]}
  end

  def to_xml(r_t_atom(val: v), _Env) do
    {:atom, [{:value, :erlang.atom_to_list(v)}], []}
  end

  def to_xml(r_t_integer(val: v), _Env) do
    {:integer, [{:value, :erlang.integer_to_list(v)}], []}
  end

  def to_xml(r_t_integer_range(from: from, to: to), _Env) do
    {:range, [{:value, :erlang.integer_to_list(from) ++ '..' ++ :erlang.integer_to_list(to)}], []}
  end

  def to_xml(r_t_binary(base_size: 0, unit_size: 0), _Ens) do
    {:binary, [{:value, '<<>>'}], []}
  end

  def to_xml(r_t_binary(base_size: b, unit_size: 0), _Ens) do
    {:binary, [{:value, :io_lib.fwrite('<<_:~w>>', [b])}], []}
  end

  def to_xml(r_t_binary(base_size: 0, unit_size: u), _Ens) do
    {:binary, [{:value, :io_lib.fwrite('<<_:_*~w>>', [u])}], []}
  end

  def to_xml(r_t_binary(base_size: b, unit_size: u), _Ens) do
    {:binary, [{:value, :io_lib.fwrite('<<_:~w, _:_*~w>>', [b, u])}], []}
  end

  def to_xml(r_t_float(val: v), _Env) do
    {:float, [{:value, :io_lib.write(v)}], []}
  end

  def to_xml(r_t_union(types: ts), env) do
    {:union, map(&wrap_utype/2, ts, env)}
  end

  def to_xml(r_t_record(name: n = r_t_atom(), fields: fs), env) do
    {:record, [to_xml(n, env) | map(&to_xml/2, fs, env)]}
  end

  def to_xml(r_t_field(name: n = r_t_atom(), type: t), env) do
    {:field, [to_xml(n, env), wrap_type(t, env)]}
  end

  def to_xml(r_t_def(name: n = r_t_var(), type: t), env) do
    {:localdef, [to_xml(n, env), wrap_type(t, env)]}
  end

  def to_xml(r_t_def(name: n, type: t), env) do
    {:localdef, [{:label, to_label(n)}], [to_xml(n, env), wrap_type(t, env)]}
  end

  def to_xml(r_t_spec(name: n, type: t, defs: ds), env) do
    {:typespec, [to_xml(n, env), wrap_utype(t, env) | map(&to_xml/2, ds, env)]}
  end

  def to_xml(
        r_t_typedef(name: n, args: as, type: :undefined, defs: ds),
        env
      ) do
    {:typedef,
     [
       to_xml(n, env),
       {:argtypes, map(&wrap_utype/2, as, env)}
       | map(&to_xml/2, ds, env)
     ]}
  end

  def to_xml(r_t_typedef(name: n, args: as, type: t, defs: ds), env) do
    {:typedef,
     [
       to_xml(n, env),
       {:argtypes, map(&wrap_utype/2, as, env)},
       wrap_type(t, env) | map(&to_xml/2, ds, env)
     ]}
  end

  def to_xml(r_t_throws(type: t, defs: ds), env) do
    {:throws, [wrap_type(t, env) | map(&to_xml/2, ds, env)]}
  end

  defp wrap_type(t, env) do
    {:type, [to_xml(t, env)]}
  end

  defp wrap_utype(t, env) do
    e = to_xml(t, env)

    case arg_name(t) do
      :_ ->
        {:type, [e]}

      a ->
        {:type, [{:name, :erlang.atom_to_list(a)}], [e]}
    end
  end

  defp map(f, xs, env) do
    for x <- xs do
      f.(x, env)
    end
  end

  defp is_name(a) when is_atom(a) do
    true
  end

  defp is_name(_) do
    false
  end

  defp is_desc(a) when is_list(a) do
    true
  end

  defp is_desc(_) do
    false
  end

  defp arg_name(t) do
    find(:erlang.element(2, t), &is_name/1, :_)
  end

  def arg_names(s) do
    arg_anns(s, &is_name/1, :_)
  end

  def arg_descs(s) do
    arg_anns(s, &is_desc/1, '')
  end

  def range_desc(r_t_spec(type: r_t_fun(range: t))) do
    find(:erlang.element(2, t), &is_desc/1, '')
  end

  defp arg_anns(r_t_spec(type: r_t_fun(args: as)), f, def__) do
    for a <- as do
      find(:erlang.element(2, a), f, def__)
    end
  end

  defp find([a | as], f, def__) do
    case f.(a) do
      true ->
        a

      false ->
        find(as, f, def__)
    end
  end

  defp find([], _, def__) do
    def__
  end

  def set_arg_names(s, ns) do
    set_arg_anns(s, ns, &is_name/1)
  end

  defp set_arg_anns(r_t_spec(type: r_t_fun(args: as) = t) = s, ns, f) do
    zip = fn a, n ->
      :erlang.setelement(2, a, update(:erlang.element(2, a), n, f))
    end

    r_t_spec(s, type: r_t_fun(t, args: :lists.zipwith(zip, as, ns)))
  end

  defp update([a | as], n, f) do
    case f.(a) do
      true ->
        [n | as]

      false ->
        [a | update(as, n, f)]
    end
  end

  defp update([], n, _) do
    [n]
  end
end
