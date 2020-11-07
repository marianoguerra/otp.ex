defmodule :m_asn1ct_value do
  use Bitwise
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

  def from_type(m, typename) do
    case :asn1_db.dbload(m) do
      :error ->
        {:error, {:not_found, {m, typename}}}

      :ok ->
        r_typedef(typespec: type) = :asn1_db.dbget(m, typename)
        from_type(m, [typename], type)

      vdef when elem(vdef, 0) === :valuedef ->
        from_value(vdef)

      err ->
        {:error, {:other, err}}
    end
  end

  defp from_type(m, typename, type)
       when elem(type, 0) === :type do
    innerType = get_inner(r_type(type, :def))

    case :asn1ct_gen.type(innerType) do
      r_Externaltypereference(module: emod, type: etype) ->
        from_type(emod, etype)

      {_, :user} ->
        from_type(m, innerType)

      {:primitive, :bif} ->
        from_type_prim(m, type)

      :ASN1_OPEN_TYPE ->
        case r_type(type, :constraint) do
          [r_Externaltypereference(type: trefConstraint)] ->
            from_type(m, trefConstraint)

          _ ->
            eRule = get_encoding_rule(m)
            open_type_value(eRule)
        end

      {:constructed, :bif} when typename == [:EXTERNAL] ->
        val = from_type_constructed(m, typename, innerType, type)

        t =
          case m.maps() do
            false ->
              :transform_to_EXTERNAL1994

            true ->
              :transform_to_EXTERNAL1994_maps
          end

        apply(:asn1ct_eval_ext, t, [val])

      {:constructed, :bif} ->
        from_type_constructed(m, typename, innerType, type)
    end
  end

  defp from_type(m, typename, r_ComponentType(name: name, typespec: type)) do
    from_type(m, [name | typename], type)
  end

  defp from_type(_, _, _) do
    :undefined
  end

  defp from_value(r_valuedef(type: r_type(def: :INTEGER), value: val)) do
    val
  end

  defp get_inner(a) when is_atom(a) do
    a
  end

  defp get_inner(ext)
       when elem(ext, 0) === :Externaltypereference do
    ext
  end

  defp get_inner({:typereference, _Pos, name}) do
    name
  end

  defp get_inner(t) when is_tuple(t) do
    case :asn1ct_gen.get_inner(t) do
      {:fixedtypevaluefield, _, type} ->
        r_type(type, :def)

      {:typefield, _FieldName} ->
        :ASN1_OPEN_TYPE

      other ->
        other
    end
  end

  defp from_type_constructed(m, typename, innerType, d)
       when elem(d, 0) === :type do
    case innerType do
      :SET ->
        get_sequence(m, typename, d)

      :SEQUENCE ->
        get_sequence(m, typename, d)

      :CHOICE ->
        get_choice(m, typename, d)

      :"SEQUENCE OF" ->
        {_, type} = r_type(d, :def)

        nameSuffix =
          :asn1ct_gen.constructed_suffix(
            innerType,
            r_type(type, :def)
          )

        get_sequence_of(m, typename, d, nameSuffix)

      :"SET OF" ->
        {_, type} = r_type(d, :def)

        nameSuffix =
          :asn1ct_gen.constructed_suffix(
            innerType,
            r_type(type, :def)
          )

        get_sequence_of(m, typename, d, nameSuffix)
    end
  end

  defp get_sequence(m, typename, type) do
    {_SEQorSET, compList} =
      case r_type(type, :def) do
        r_SEQUENCE(components: cl) ->
          {:SEQUENCE, cl}

        r_SET(components: cl) ->
          {:SET, to_textual_order(cl)}
      end

    cs = get_components(m, typename, compList)

    case m.maps() do
      false ->
        recordTag = :erlang.list_to_atom(:asn1ct_gen.list2rname(typename))

        :erlang.list_to_tuple([
          recordTag
          | for {_, val} <- cs do
              val
            end
        ])

      true ->
        :maps.from_list(cs)
    end
  end

  defp get_components(m, typename, {root, ext}) do
    get_components2(m, typename, filter_complist(root ++ ext))
  end

  defp get_components(m, typename, {rl1, el, rl2}) do
    get_components2(m, typename, filter_complist(rl1 ++ el ++ rl2))
  end

  defp get_components(m, typename, compList) do
    get_components2(m, typename, compList)
  end

  defp get_components2(m, typename, [h | t]) do
    r_ComponentType(name: name) = h
    [{name, from_type(m, typename, h)} | get_components(m, typename, t)]
  end

  defp get_components2(_, _, []) do
    []
  end

  defp filter_complist(compList) when is_list(compList) do
    :lists.filter(
      fn
        r_ExtensionAdditionGroup() ->
          false

        :ExtensionAdditionGroupEnd ->
          false

        _ ->
          true
      end,
      compList
    )
  end

  defp get_choice(m, typename, type) do
    {:CHOICE, tCompList} = r_type(type, :def)

    case tCompList do
      [] ->
        {:asn1_EMPTY, :asn1_EMPTY}

      {compList, extList} ->
        cList = compList ++ extList
        c = :lists.nth(random(length(cList)), cList)
        {r_ComponentType(c, :name), from_type(m, typename, c)}

      compList when is_list(compList) ->
        c = :lists.nth(random(length(compList)), compList)
        {r_ComponentType(c, :name), from_type(m, typename, c)}
    end
  end

  defp get_sequence_of(m, typename, type, typeSuffix) do
    {_, oftype} = r_type(type, :def)
    c = r_type(type, :constraint)
    s = size_random(c)
    newTypeName = [typeSuffix | typename]
    gen_list(m, newTypeName, oftype, s)
  end

  defp gen_list(_, _, _, 0) do
    []
  end

  defp gen_list(m, typename, oftype, n) do
    [from_type(m, typename, oftype) | gen_list(m, typename, oftype, n - 1)]
  end

  defp from_type_prim(m, d) do
    c = r_type(d, :constraint)

    case r_type(d, :def) do
      :INTEGER ->
        i_random(c)

      {:INTEGER, [_ | _] = nNL} ->
        case c do
          [] ->
            {n, _} = :lists.nth(random(length(nNL)), nNL)
            n

          _ ->
            v = i_random(c)

            case :lists.keyfind(v, 2, nNL) do
              false ->
                v

              {n, ^v} ->
                n
            end
        end

      enum
      when is_tuple(enum) and
             :erlang.element(1, enum) == :ENUMERATED ->
        namedNumberList =
          case enum do
            {_, _, nNL} ->
              nNL

            {_, nNL} ->
              nNL
          end

        nNew =
          case namedNumberList do
            {n1, n2} ->
              n1 ++ n2

            _ ->
              namedNumberList
          end

        nN =
          for {x, _} <- nNew do
            x
          end

        case nN do
          [] ->
            :io.format(:user, 'Enum = ~p~n', [enum])
            :asn1_EMPTY

          _ ->
            case c do
              [] ->
                :lists.nth(random(length(nN)), nN)

              _ ->
                :lists.nth(
                  (fn
                     0 ->
                       1

                     x ->
                       x
                   end).(i_random(c)),
                  nN
                )
            end
        end

      {:"BIT STRING", namedNumberList} ->
        nN =
          for {x, _} <- namedNumberList do
            x
          end

        case nN do
          [] ->
            random_unnamed_bit_string(m, c)

          _ ->
            [:lists.nth(random(length(nN)), nN)]
        end

      :NULL ->
        :NULL

      :"OBJECT IDENTIFIER" ->
        len = random(3)

        olist =
          for _X <- :lists.seq(1, len) do
            random(1000) - 1
          end

        :erlang.list_to_tuple([
          [random(3) - 1, random(40) - 1]
          | olist
        ])

      :"RELATIVE-OID" ->
        len = random(5)

        olist =
          for _X <- :lists.seq(1, len) do
            random(65535) - 1
          end

        :erlang.list_to_tuple(olist)

      :ObjectDescriptor ->
        'Dummy ObjectDescriptor'

      :REAL ->
        case random(3) do
          1 ->
            case random(3) do
              3 ->
                {129, 2, 10}

              2 ->
                {1, 2, 1}

              _ ->
                {255, 2, 2}
            end

          _ ->
            case random(2) do
              2 ->
                '123.E10'

              _ ->
                '-123.E-10'
            end
        end

      :BOOLEAN ->
        true

      :"OCTET STRING" ->
        s0 = adjust_list(size_random(c), c_string(c, 'OCTET STRING'))

        case m.legacy_erlang_types() do
          false ->
            :erlang.list_to_binary(s0)

          true ->
            s0
        end

      :NumericString ->
        adjust_list(size_random(c), c_string(c, '0123456789'))

      :TeletexString ->
        adjust_list(size_random(c), c_string(c, 'TeletexString'))

      :T61String ->
        adjust_list(size_random(c), c_string(c, 'T61String'))

      :VideotexString ->
        adjust_list(size_random(c), c_string(c, 'VideotexString'))

      :UTCTime ->
        '97100211-0500'

      :GeneralizedTime ->
        '19971002103130.5'

      :GraphicString ->
        adjust_list(size_random(c), c_string(c, 'GraphicString'))

      :VisibleString ->
        adjust_list(size_random(c), c_string(c, 'VisibleString'))

      :GeneralString ->
        adjust_list(size_random(c), c_string(c, 'GeneralString'))

      :PrintableString ->
        adjust_list(size_random(c), c_string(c, 'PrintableString'))

      :IA5String ->
        adjust_list(size_random(c), c_string(c, 'IA5String'))

      :BMPString ->
        adjust_list(size_random(c), c_string(c, 'BMPString'))

      :UTF8String ->
        l =
          adjust_list(
            random(50),
            [?U, ?T, ?F, ?8, ?S, ?t, ?r, ?i, ?n, ?g, 65535, 65518, 1_114_111, 65535, 4095]
          )

        :unicode.characters_to_binary(l)

      :UniversalString ->
        adjust_list(size_random(c), c_string(c, 'UniversalString'))
    end
  end

  defp c_string(c, default) do
    case get_constraint(c, :PermittedAlphabet) do
      {:SingleValue, sv} when is_list(sv) ->
        sv

      {:SingleValue, v} when is_integer(v) ->
        [v]

      :no ->
        default
    end
  end

  defp random_unnamed_bit_string(m, c) do
    bl1 =
      :lists.reverse(
        adjust_list(
          size_random(c),
          [1, 0, 1, 1]
        )
      )

    bl2 =
      :lists.reverse(
        :lists.dropwhile(
          fn
            0 ->
              true

            1 ->
              false
          end,
          bl1
        )
      )

    val =
      case {length(bl2), get_constraint(c, :SizeConstraint)} do
        {len, len} ->
          bl2

        {_Len, int} when is_integer(int) ->
          bl1

        {len, {min, _}} when min > len ->
          bl1

        _ ->
          bl2
      end

    case m.bit_string_format() do
      :legacy ->
        val

      :bitstring ->
        for b <- val, into: <<>> do
          <<b::size(1)>>
        end

      :compact ->
        bitString =
          for b <- val, into: <<>> do
            <<b::size(1)>>
          end

        padLen = 8 - bit_size(bitString) &&& 7 &&& 7
        {padLen, <<bitString::bitstring, 0::size(padLen)>>}
    end
  end

  defp random(upper) do
    :rand.uniform(upper)
  end

  defp size_random(c) do
    case get_constraint(c, :SizeConstraint) do
      :no ->
        c_random({0, 5}, :no)

      {{lb, ub}, _} when is_integer(lb) and is_integer(ub) ->
        cond do
          ub - lb <= 4 ->
            c_random({lb, ub}, :no)

          true ->
            c_random({lb, lb + 4}, :no)
        end

      {lb, ub} when ub - lb <= 4 ->
        c_random({lb, ub}, :no)

      {lb, _} ->
        c_random({lb, lb + 4}, :no)

      sv ->
        c_random(:no, sv)
    end
  end

  defp i_random(c) do
    c_random(
      get_constraint(c, :ValueRange),
      get_constraint(c, :SingleValue)
    )
  end

  defp c_random(vRange, single) do
    case {vRange, single} do
      {:no, :no} ->
        random(268_435_455) - (268_435_455 >>> 1)

      {r, :no} ->
        case r do
          {lb, ub} when is_integer(lb) and is_integer(ub) ->
            range = ub - lb + 1
            lb + (random(range) - 1)

          {lb, :MAX} ->
            lb + random(268_435_455) - 1

          {:MIN, ub} ->
            ub - random(268_435_455) - 1

          {a, {:ASN1_OK, b}} ->
            range = b - a + 1
            a + (random(range) - 1)
        end

      {_, s} when is_integer(s) ->
        s

      {_, s} when is_list(s) ->
        :lists.nth(random(length(s)), s)
    end
  end

  defp adjust_list(len, orig) do
    adjust_list1(len, orig, orig, [])
  end

  defp adjust_list1(0, _Orig, [_Oh | _Ot], acc) do
    :lists.reverse(acc)
  end

  defp adjust_list1(len, orig, [], acc) do
    adjust_list1(len, orig, orig, acc)
  end

  defp adjust_list1(len, orig, [oh | ot], acc) do
    adjust_list1(len - 1, orig, ot, [oh | acc])
  end

  defp get_constraint(c, key) do
    case :lists.keyfind(key, 1, c) do
      false ->
        :no

      {:ValueRange, {lb, ub}} ->
        {check_external(lb), check_external(ub)}

      {:SizeConstraint, n} ->
        n

      {^key, value} ->
        value
    end
  end

  defp check_external(extRef)
       when elem(extRef, 0) === :Externalvaluereference do
    r_Externalvaluereference(module: emod, value: evalue) = extRef
    from_type(emod, evalue)
  end

  defp check_external(value) do
    value
  end

  defp get_encoding_rule(m) do
    mod =
      cond do
        is_list(m) ->
          :erlang.list_to_atom(m)

        true ->
          m
      end

    case (try do
            mod.encoding_rule()
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      a when is_atom(a) ->
        a

      _ ->
        :unknown
    end
  end

  defp open_type_value(:ber) do
    <<4, 9, 111, 112, 101, 110, 95, 116, 121, 112, 101>>
  end

  defp open_type_value(_) do
    "\n\topen_type"
  end

  defp to_textual_order({root, ext}) do
    {to_textual_order(root), ext}
  end

  defp to_textual_order(cs) when is_list(cs) do
    case cs do
      [r_ComponentType(textual_order: :undefined) | _] ->
        cs

      _ ->
        :lists.keysort(r_ComponentType(:textual_order), cs)
    end
  end
end
