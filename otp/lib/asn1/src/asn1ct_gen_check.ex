defmodule :m_asn1ct_gen_check do
  use Bitwise
  import :asn1ct_gen, only: [emit: 1]
  require Record

  Record.defrecord(:r_module, :module,
    pos: :undefined,
    name: :undefined,
    defid: :undefined,
    tagdefault: :EXPLICIT,
    exports: {:exports, []},
    imports: {:imports, []},
    extensiondefault: :empty,
    typeorval: :undefined
  )

  Record.defrecord(:r_ExtensionAdditionGroup, :ExtensionAdditionGroup, number: :undefined)

  Record.defrecord(:r_SEQUENCE, :SEQUENCE,
    pname: false,
    tablecinf: false,
    extaddgroup: :undefined,
    components: []
  )

  Record.defrecord(:r_SET, :SET, pname: false, sorted: false, tablecinf: false, components: [])

  Record.defrecord(:r_ComponentType, :ComponentType,
    pos: :undefined,
    name: :undefined,
    typespec: :undefined,
    prop: :undefined,
    tags: :undefined,
    textual_order: :undefined
  )

  Record.defrecord(:r_ObjectClassFieldType, :ObjectClassFieldType,
    classname: :undefined,
    class: :undefined,
    fieldname: :undefined,
    type: :undefined
  )

  Record.defrecord(:r_typedef, :typedef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    typespec: :undefined
  )

  Record.defrecord(:r_classdef, :classdef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    module: :undefined,
    typespec: :undefined
  )

  Record.defrecord(:r_valuedef, :valuedef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    type: :undefined,
    value: :undefined,
    module: :undefined
  )

  Record.defrecord(:r_ptypedef, :ptypedef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    args: :undefined,
    typespec: :undefined
  )

  Record.defrecord(:r_pvaluedef, :pvaluedef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    args: :undefined,
    type: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_pvaluesetdef, :pvaluesetdef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    args: :undefined,
    type: :undefined,
    valueset: :undefined
  )

  Record.defrecord(:r_pobjectdef, :pobjectdef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    args: :undefined,
    class: :undefined,
    def: :undefined
  )

  Record.defrecord(:r_pobjectsetdef, :pobjectsetdef,
    checked: false,
    pos: :undefined,
    name: :undefined,
    args: :undefined,
    class: :undefined,
    def: :undefined
  )

  Record.defrecord(:r_Constraint, :Constraint,
    SingleValue: :no,
    SizeConstraint: :no,
    ValueRange: :no,
    PermittedAlphabet: :no,
    ContainedSubtype: :no,
    TypeConstraint: :no,
    InnerSubtyping: :no,
    e: :no,
    Other: :no
  )

  Record.defrecord(:r_simpletableattributes, :simpletableattributes,
    objectsetname: :undefined,
    c_name: :undefined,
    c_index: :undefined,
    usedclassfield: :undefined,
    uniqueclassfield: :undefined,
    valueindex: :undefined
  )

  Record.defrecord(:r_type, :type,
    tag: [],
    def: :undefined,
    constraint: [],
    tablecinf: [],
    inlined: :no
  )

  Record.defrecord(:r_objectclass, :objectclass,
    fields: [],
    syntax: :undefined
  )

  Record.defrecord(:r_Object, :Object, classname: :undefined, gen: true, def: :undefined)

  Record.defrecord(:r_ObjectSet, :ObjectSet,
    class: :undefined,
    gen: true,
    uniquefname: :undefined,
    set: :undefined
  )

  Record.defrecord(:r_tag, :tag, class: :undefined, number: :undefined, type: :undefined, form: 32)

  Record.defrecord(:r_cmap, :cmap,
    single_value: :no,
    contained_subtype: :no,
    value_range: :no,
    size: :no,
    permitted_alphabet: :no,
    type_constraint: :no,
    inner_subtyping: :no
  )

  Record.defrecord(:r_EXTENSIONMARK, :EXTENSIONMARK,
    pos: :undefined,
    val: :undefined
  )

  Record.defrecord(:r_SymbolsFromModule, :SymbolsFromModule,
    symbols: :undefined,
    module: :undefined,
    objid: :undefined
  )

  Record.defrecord(:r_Externaltypereference, :Externaltypereference,
    pos: :undefined,
    module: :undefined,
    type: :undefined
  )

  Record.defrecord(:r_Externalvaluereference, :Externalvaluereference,
    pos: :undefined,
    module: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_seqtag, :seqtag, pos: :undefined, module: :undefined, val: :undefined)

  Record.defrecord(:r_state, :state,
    module: :undefined,
    mname: :undefined,
    tname: :undefined,
    erule: :undefined,
    parameters: [],
    inputmodules: [],
    abscomppath: [],
    recordtopname: [],
    options: :undefined,
    sourcedir: :undefined,
    error_context: :undefined
  )

  Record.defrecord(:r_gen, :gen,
    erule: :ber,
    der: false,
    jer: false,
    aligned: false,
    rec_prefix: '',
    macro_prefix: '',
    pack: :record,
    options: []
  )

  Record.defrecord(:r_abst, :abst,
    name: :undefined,
    types: :undefined,
    values: :undefined,
    ptypes: :undefined,
    classes: :undefined,
    objects: :undefined,
    objsets: :undefined
  )

  Record.defrecord(:r_gen_state, :gen_state,
    active: false,
    prefix: :undefined,
    inc_tag_pattern: :undefined,
    tag_pattern: :undefined,
    inc_type_pattern: :undefined,
    type_pattern: :undefined,
    func_name: :undefined,
    namelist: :undefined,
    tobe_refed_funcs: [],
    gen_refed_funcs: [],
    generated_functions: [],
    suffix_index: 1,
    current_suffix_index: :undefined
  )

  def emit(gen, type, default, value) do
    key = {type, default}

    doGen = fn fd, name ->
      :file.write(fd, gen(gen, name, type, default))
    end

    emit(' case ')
    :asn1ct_func.call_gen('is_default_', key, doGen, [value])
    emit([' of', :nl, 'true -> {[],0};', :nl, 'false ->', :nl])
  end

  defp gen(r_gen(pack: pack) = gen, name, r_type(def: t), default) do
    defMarker =
      case pack do
        :record ->
          'asn1_DEFAULT'

        :map ->
          :erlang.atom_to_list(:asn1__MISSING_IN_MAP)
      end

    nameStr = :erlang.atom_to_list(name)

    [
      [nameStr, '(', defMarker, ') ->\n', 'true;\n']
      | case do_gen(gen, t, default) do
          {:literal, literal} ->
            [
              nameStr,
              '(Def) when Def =:= ',
              term2str(literal),
              ' ->\n',
              'true;\n',
              nameStr,
              '(_) ->\n',
              'false.\n\n'
            ]

          {:exception, func, args} ->
            [
              nameStr,
              '(Value) ->\n',
              'try ',
              func,
              '(Value',
              arg2str(args),
              ') of\n',
              '_ -> true\ncatch throw:false -> false\nend.\n\n'
            ]
        end
    ]
  end

  defp do_gen(_Gen, _, :asn1_NOVALUE) do
    {:literal, :asn1_NOVALUE}
  end

  defp do_gen(gen, r_Externaltypereference(module: m, type: t), default) do
    r_typedef(typespec: r_type(def: td)) = :asn1_db.dbget(m, t)
    do_gen(gen, td, default)
  end

  defp do_gen(_Gen, :BOOLEAN, default) do
    {:literal, default}
  end

  defp do_gen(_Gen, {:"BIT STRING", []}, default) do
    true = is_bitstring(default)

    case :asn1ct.use_legacy_types() do
      false ->
        {:literal, default}

      true ->
        {:exception, need(:check_legacy_bitstring, 2), [default]}
    end
  end

  defp do_gen(_Gen, {:"BIT STRING", [_ | _] = nBL}, default) do
    do_named_bitstring(nBL, default)
  end

  defp do_gen(_Gen, {:ENUMERATED, _}, default) do
    {:literal, default}
  end

  defp do_gen(_Gen, :INTEGER, default) do
    {:literal, default}
  end

  defp do_gen(_Gen, {:INTEGER, nNL}, default) do
    {:exception, need(:check_int, 3), [default, nNL]}
  end

  defp do_gen(_Gen, :NULL, default) do
    {:literal, default}
  end

  defp do_gen(_Gen, :"OCTET STRING", default) do
    true = is_binary(default)

    case :asn1ct.use_legacy_types() do
      false ->
        {:literal, default}

      true ->
        {:exception, need(:check_octetstring, 2), [default]}
    end
  end

  defp do_gen(_Gen, :"OBJECT IDENTIFIER", default0) do
    default = pre_process_oid(default0)
    {:exception, need(:check_objectidentifier, 2), [default]}
  end

  defp do_gen(gen, {:CHOICE, cs}, default) do
    {tag, value} = default

    [type] =
      for r_ComponentType(name: t, typespec: type) <- cs,
          t === tag do
        type
      end

    case do_gen(gen, r_type(type, :def), value) do
      {:literal, lit} ->
        {:literal, {tag, lit}}

      {:exception, func0, args} ->
        key = {tag, func0, args}

        doGen = fn fd, name ->
          s = gen_choice(name, tag, func0, args)
          :ok = :file.write(fd, s)
        end

        func = :asn1ct_func.call_gen('is_default_choice', key, doGen)
        {:exception, :erlang.atom_to_list(func), []}
    end
  end

  defp do_gen(gen, r_SEQUENCE(components: cs), default) do
    do_seq_set(gen, cs, default)
  end

  defp do_gen(gen, {:"SEQUENCE OF", type}, default) do
    do_sof(gen, type, default)
  end

  defp do_gen(gen, r_SET(components: cs), default) do
    do_seq_set(gen, cs, default)
  end

  defp do_gen(gen, {:"SET OF", type}, default) do
    do_sof(gen, type, default)
  end

  defp do_gen(_Gen, type, default) do
    case :asn1ct_gen.unify_if_string(type) do
      :restrictedstring ->
        {:exception, need(:check_restrictedstring, 2), [default]}

      _ ->
        {:literal, default}
    end
  end

  defp do_named_bitstring(nBL, default0) when is_list(default0) do
    default = :lists.sort(default0)
    bs = :asn1ct_gen.named_bitstring_value(default, nBL)

    func =
      case :asn1ct.use_legacy_types() do
        false ->
          :check_named_bitstring

        true ->
          :check_legacy_named_bitstring
      end

    {:exception, need(func, 4), [default, bs, bit_size(bs)]}
  end

  defp do_named_bitstring(_, default) when is_bitstring(default) do
    func =
      case :asn1ct.use_legacy_types() do
        false ->
          :check_named_bitstring

        true ->
          :check_legacy_named_bitstring
      end

    {:exception, need(func, 3), [default, bit_size(default)]}
  end

  defp do_seq_set(r_gen(pack: :record) = gen, cs0, default) do
    tag = :erlang.element(1, default)

    cs1 =
      for r_ComponentType(typespec: t) <- cs0 do
        t
      end

    cs = components(gen, cs1, tl(:erlang.tuple_to_list(default)))

    case are_all_literals(cs) do
      true ->
        literal =
          :erlang.list_to_tuple([
            tag
            | for {:literal, l} <- cs do
                l
              end
          ])

        {:literal, literal}

      false ->
        key = {cs, default}

        doGen = fn fd, name ->
          s = gen_components(name, tag, cs)
          :ok = :file.write(fd, s)
        end

        func = :asn1ct_func.call_gen('is_default_cs_', key, doGen)
        {:exception, :erlang.atom_to_list(func), []}
    end
  end

  defp do_seq_set(r_gen(pack: :map) = gen, cs0, default) do
    cs1 =
      for r_ComponentType(name: n, typespec: t) <- cs0 do
        {n, t}
      end

    cs = map_components(gen, cs1, default)

    allLiterals =
      :lists.all(
        fn
          {_, {:literal, _}} ->
            true

          {_, _} ->
            false
        end,
        cs
      )

    case allLiterals do
      true ->
        l =
          for {name, {:literal, lit}} <- cs do
            {name, lit}
          end

        {:literal, :maps.from_list(l)}

      false ->
        key = {cs, default}

        doGen = fn fd, name ->
          s = gen_map_components(name, cs)
          :ok = :file.write(fd, s)
        end

        func = :asn1ct_func.call_gen('is_default_cs_', key, doGen)
        {:exception, :erlang.atom_to_list(func), []}
    end
  end

  defp do_sof(gen, type, default0) do
    default = :lists.sort(default0)
    cs0 = :lists.duplicate(length(default), type)
    cs = components(gen, cs0, default)

    case are_all_literals(cs) do
      true ->
        literal =
          for {:literal, lit} <- cs do
            lit
          end

        {:exception, need(:check_literal_sof, 2), [literal]}

      false ->
        key = cs

        doGen = fn fd, name ->
          s = gen_sof(name, cs)
          :ok = :file.write(fd, s)
        end

        func = :asn1ct_func.call_gen('is_default_sof', key, doGen)
        {:exception, :erlang.atom_to_list(func), []}
    end
  end

  defp are_all_literals([{:literal, _} | t]) do
    are_all_literals(t)
  end

  defp are_all_literals([_ | _]) do
    false
  end

  defp are_all_literals([]) do
    true
  end

  defp gen_components(name, tag, cs) do
    [
      [:erlang.atom_to_list(name), '(Value) ->\n', 'case Value of\n', '{', term2str(tag)]
      | gen_cs_1(cs, 1, [])
    ]
  end

  defp gen_cs_1([{:literal, lit} | t], i, acc) do
    [[',\n', term2str(lit)] | gen_cs_1(t, i, acc)]
  end

  defp gen_cs_1([h | t], i, acc) do
    var = 'E' ++ :erlang.integer_to_list(i)
    [[',\n', var] | gen_cs_1(t, i + 1, [{var, h} | acc])]
  end

  defp gen_cs_1([], _, acc) do
    ['} ->\n' | gen_cs_2(acc, '')]
  end

  defp gen_cs_2([{var, {:exception, func, args}} | t], sep) do
    [[sep, func, '(', var, arg2str(args), ')'] | gen_cs_2(t, ',\n')]
  end

  defp gen_cs_2([], _) do
    [';\n', '_ ->\nthrow(false)\nend.\n']
  end

  defp gen_map_components(name, cs) do
    [
      [:erlang.atom_to_list(name), '(Value) ->\n', 'case Value of\n', '\#{']
      | gen_map_cs_1(cs, 1, '', [])
    ]
  end

  defp gen_map_cs_1([{name, {:literal, lit}} | t], i, sep, acc) do
    var = 'E' ++ :erlang.integer_to_list(i)
    g = var ++ ' =:= ' ++ term2str(lit)
    [[sep, term2str(name), ':=', var] | gen_map_cs_1(t, i + 1, ',\n', [{:guard, g} | acc])]
  end

  defp gen_map_cs_1([{name, exc} | t], i, sep, acc) do
    var = 'E' ++ :erlang.integer_to_list(i)
    [[sep, term2str(name), ':=', var] | gen_map_cs_1(t, i + 1, ',\n', [{:exc, {var, exc}} | acc])]
  end

  defp gen_map_cs_1([], _, _, acc) do
    g =
      :lists.join(
        ', ',
        for {:guard, s} <- acc do
          s
        end
      )

    exc =
      for {:exc, e} <- acc do
        e
      end

    body = gen_map_cs_2(exc, '')

    case g do
      [] ->
        ['} ->\n' | body]

      [_ | _] ->
        [['} when ', g, ' ->\n'] | body]
    end
  end

  defp gen_map_cs_2([{var, {:exception, func, args}} | t], sep) do
    [
      [sep, func, '(', var, arg2str(args), ')']
      | gen_map_cs_2(
          t,
          ',\n'
        )
    ]
  end

  defp gen_map_cs_2([], _) do
    [';\n', '_ ->\nthrow(false)\nend.\n']
  end

  defp gen_sof(name, cs) do
    [
      [
        :erlang.atom_to_list(name),
        '(Value) ->\n',
        'case length(Value) of\n',
        :erlang.integer_to_list(length(cs)),
        ' -> ok;\n_ -> throw(false)\nend,\nT0 = lists:sort(Value)'
      ]
      | gen_sof_1(cs, 1)
    ]
  end

  defp gen_sof_1([{:exception, func, args} | cs], i) do
    numStr = :erlang.integer_to_list(i)
    h = 'H' ++ numStr
    t = 'T' ++ numStr
    prev = 'T' ++ :erlang.integer_to_list(i - 1)

    [
      [
        ',\n',
        '[',
        h,
        case cs do
          [] ->
            []

          [_ | _] ->
            ['|', t]
        end,
        '] = ',
        prev,
        ',\n',
        func,
        '(',
        h,
        arg2str(args),
        ')'
      ]
      | gen_sof_1(cs, i + 1)
    ]
  end

  defp gen_sof_1([], _) do
    '.\n'
  end

  defp components(gen, [r_type(def: def__) | ts], [v | vs]) do
    [do_gen(gen, def__, v) | components(gen, ts, vs)]
  end

  defp components(_Gen, [], []) do
    []
  end

  defp map_components(gen, [{name, r_type(def: def__)} | ts], value) do
    case :maps.find(name, value) do
      {:ok, v} ->
        [{name, do_gen(gen, def__, v)} | map_components(gen, ts, value)]

      :error ->
        map_components(gen, ts, value)
    end
  end

  defp map_components(_Gen, [], _Value) do
    []
  end

  defp gen_choice(name, tag, func, args) do
    nameStr = :erlang.atom_to_list(name)

    [
      nameStr,
      '({',
      term2str(tag),
      ',Value}) ->\n ',
      func,
      '(Value',
      arg2str(args),
      ');\n',
      nameStr,
      '(_) ->\n throw(false).\n'
    ]
  end

  defp pre_process_oid(oid) do
    reserved = reserved_oid()
    pre_process_oid(:erlang.tuple_to_list(oid), reserved, [])
  end

  defp pre_process_oid([h | t] = tail, res0, acc) do
    case :lists.keyfind(h, 2, res0) do
      false ->
        {:lists.reverse(acc), tail}

      {names0, ^h, res} ->
        names =
          case is_list(names0) do
            false ->
              [names0]

            true ->
              names0
          end

        keys = [h | names]
        pre_process_oid(t, res, [keys | acc])
    end
  end

  defp reserved_oid() do
    [
      {[:"itu-t", :ccitt], 0,
       [
         {:recommendation, 0, []},
         {:question, 1, []},
         {:administration, 2, []},
         {:"network-operator", 3, []},
         {:"identified-organization", 4, []}
       ]},
      {:iso, 1,
       [{:standard, 0, []}, {:"member-body", 2, []}, {:"identified-organization", 3, []}]},
      {[:"joint-iso-itu-t", :"joint-iso-ccitt"], 2, []}
    ]
  end

  defp arg2str(args) do
    for arg <- args do
      ', ' ++ term2str(arg)
    end
  end

  defp term2str(t) do
    :io_lib.format('~w', [t])
  end

  defp need(f, a) do
    :asn1ct_func.need({:check, f, a})
    :erlang.atom_to_list(f)
  end
end
