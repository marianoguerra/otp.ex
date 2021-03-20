defmodule :m_asn1ct_constructed_per do
  use Bitwise
  import :asn1ct_gen, only: [emit: 1, get_record_name_prefix: 1]
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

  def gen_encode_set(gen, typeName, d) do
    gen_encode_constructed(gen, typeName, d)
  end

  def gen_encode_sequence(gen, typeName, d) do
    gen_encode_constructed(gen, typeName, d)
  end

  defp gen_encode_constructed(erule, typename, r_type() = d) do
    :asn1ct_name.start()
    imm = gen_encode_constructed_imm(erule, typename, d)
    :asn1ct_imm.enc_cg(imm, is_aligned(erule))
    emit(['.', :nl])
  end

  defp gen_encode_constructed_imm(gen, typename, r_type() = d) do
    {compList, tableConsInfo} = enc_complist(d)
    externalImm = external_imm(gen, typename)
    optionals = optionals(to_textual_order(compList))
    immOptionals = enc_optionals(gen, optionals)
    ext = extensible_enc(compList)
    aligned = is_aligned(gen)

    extImm =
      case ext do
        {:ext, extPos, numExt} when numExt > 0 ->
          gen_encode_extaddgroup(gen, compList)
          value = make_var(:val)
          enc_extensions(gen, value, extPos, numExt, aligned)

        _ ->
          []
      end

    matchImm = enc_map_match(gen, compList)
    {encObj, objSetImm} = enc_table(gen, tableConsInfo, d)

    immSetExt =
      case ext do
        {:ext, _Pos, numExt2} when numExt2 > 0 ->
          :asn1ct_imm.per_enc_extension_bit({:var, 'Extensions'}, aligned)

        {:ext, _Pos, _} ->
          :asn1ct_imm.per_enc_extension_bit([], aligned)

        _ ->
          []
      end

    immBody = gen_enc_components_call(gen, typename, compList, encObj, ext)

    externalImm ++
      matchImm ++
      extImm ++ objSetImm ++ :asn1ct_imm.enc_append([immSetExt] ++ immOptionals ++ immBody)
  end

  defp external_imm(gen, [:EXTERNAL]) do
    next = :asn1ct_gen.mk_var(:asn1ct_name.next(:val))
    curr = :asn1ct_gen.mk_var(:asn1ct_name.curr(:val))
    :asn1ct_name.new(:val)

    f =
      case gen do
        r_gen(pack: :record) ->
          :transform_to_EXTERNAL1990

        r_gen(pack: :map) ->
          :transform_to_EXTERNAL1990_maps
      end

    [{:call, :ext, f, [{:var, curr}], {:var, next}}]
  end

  defp external_imm(_, _) do
    []
  end

  defp enc_extensions(r_gen(pack: :record), value, extPos, numExt, aligned) do
    :asn1ct_imm.per_enc_extensions(value, extPos, numExt, aligned)
  end

  defp enc_extensions(r_gen(pack: :map), value, extPos, numExt, aligned) do
    vars =
      for pos <-
            :lists.seq(
              extPos,
              extPos + numExt - 1
            ) do
        {:var, :lists.concat(['Input@', pos])}
      end

    undefined = :erlang.atom_to_list(:asn1__MISSING_IN_MAP)
    :asn1ct_imm.per_enc_extensions_map(value, vars, undefined, aligned)
  end

  defp enc_complist(r_type(def: def__)) do
    case def__ do
      r_SEQUENCE(tablecinf: tCI, components: cL0, extaddgroup: extAddGroup) ->
        case extAddGroup do
          :undefined ->
            {cL0, tCI}

          _ when is_integer(extAddGroup) ->
            cL = add_textual_order(cL0)
            {cL, tCI}
        end

      r_SET(tablecinf: tCI, components: cL) ->
        {cL, tCI}
    end
  end

  defp enc_table(
         gen,
         r_simpletableattributes(
           objectsetname: objectSet,
           c_name: attrN,
           c_index: n,
           usedclassfield: uniqueFieldName,
           uniqueclassfield: uniqueFieldName,
           valueindex: valueIndex0
         ),
         _
       ) do
    {module, objSetName} = objectSet

    r_typedef(typespec: r_ObjectSet(gen: mustGen)) =
      :asn1_db.dbget(
        module,
        objSetName
      )

    case mustGen do
      true ->
        valueIndex = valueIndex0 ++ [{n + 1, :ASN1_top}]
        val = make_var(:val)
        {objSetImm, dst} = enc_dig_out_value(gen, valueIndex, val)
        {{attrN, dst}, objSetImm}

      false ->
        {false, []}
    end
  end

  defp enc_table(_Gen, r_simpletableattributes(), _) do
    {false, []}
  end

  defp enc_table(_Gen, _, r_type(tablecinf: tCInf)) do
    case tCInf do
      [{:objfun, _} | _] ->
        {{'got objfun through args', {:var, 'ObjFun'}}, []}

      _ ->
        {false, []}
    end
  end

  defp enc_optionals(gen, optionals) do
    var = make_var(:val)
    enc_optionals_1(gen, optionals, var)
  end

  defp enc_optionals_1(r_gen(pack: :record) = gen, [{pos, defVals} | t], var) do
    {imm0, element} = :asn1ct_imm.enc_element(pos + 1, var)
    imm = :asn1ct_imm.per_enc_optional(element, defVals)
    [imm0 ++ imm | enc_optionals_1(gen, t, var)]
  end

  defp enc_optionals_1(r_gen(pack: :map) = gen, [{pos, defVals0} | t], v) do
    var = {:var, :lists.concat(['Input@', pos])}
    defVals = translate_missing_value(gen, defVals0)
    imm = :asn1ct_imm.per_enc_optional(var, defVals)
    [imm | enc_optionals_1(gen, t, v)]
  end

  defp enc_optionals_1(_, [], _) do
    []
  end

  defp enc_map_match(r_gen(pack: :record), _Cs) do
    []
  end

  defp enc_map_match(r_gen(pack: :map), cs0) do
    var0 = 'Input'
    cs = enc_flatten_components(cs0)

    m =
      for r_ComponentType(prop: :mandatory, name: name, textual_order: order) <- cs do
        [quote_atom(name), ':=', :lists.concat([var0, '@', order])]
      end

    mand =
      case m do
        [] ->
          []

        [_ | _] ->
          patt = {:expr, :lists.flatten(['\#{', :lists.join(',', m), '}'])}
          [{:assign, patt, {:var, :asn1ct_name.curr(:val)}}]
      end

    os0 =
      for r_ComponentType(prop: prop, name: name, textual_order: order) <- cs,
          prop !== :mandatory do
        {name, order}
      end

    {:var, val} = make_var(:val)

    f = fn {name, order} ->
      var = :lists.concat([var0, '@', order])

      p0 = [
        'case ',
        val,
        ' of\n  \#{',
        quote_atom(name),
        ':=',
        var,
        '_0} -> ',
        var,
        '_0;\n  _ -> ',
        :erlang.atom_to_list(:asn1__MISSING_IN_MAP),
        '\nend'
      ]

      p = :lists.flatten(p0)
      {:assign, {:var, var}, p}
    end

    os =
      for o <- os0 do
        f.(o)
      end

    mand ++ os
  end

  defp enc_flatten_components({root1, ext0, root2} = cL) do
    {_, gs} = extgroup_pos_and_length(cL)
    ext = wrap_extensionAdditionGroups(ext0, gs)

    root1 ++
      root2 ++
      for c <- ext do
        mark_optional(c)
      end
  end

  defp enc_flatten_components({root, ext}) do
    enc_flatten_components({root, ext, []})
  end

  defp enc_flatten_components(cs) do
    cs
  end

  defp gen_encode_extaddgroup(r_gen(pack: :record), compList) do
    case extgroup_pos_and_length(compList) do
      {:extgrouppos, []} ->
        :ok

      {:extgrouppos, extGroupPosLenList} ->
        _ =
          for g <- extGroupPosLenList do
            gen_encode_eag_record(g)
          end

        :ok
    end
  end

  defp gen_encode_extaddgroup(r_gen(pack: :map), cs0) do
    cs = enc_flatten_components(cs0)
    gen_encode_eag_map(cs)
  end

  defp gen_encode_eag_map([r_ComponentType(name: group, typespec: type) | cs]) do
    case type do
      r_type(def: r_SEQUENCE(extaddgroup: g, components: gCs0))
      when is_integer(g) ->
        ns =
          for r_ComponentType(name: n, prop: :mandatory) <- gCs0 do
            n
          end

        test_for_mandatory(ns, group)
        gen_encode_eag_map(cs)

      _ ->
        gen_encode_eag_map(cs)
    end
  end

  defp gen_encode_eag_map([]) do
    :ok
  end

  defp test_for_mandatory([mand | _], group) do
    emit([
      {:next, :val},
      ' = case ',
      {:curr, :val},
      ' of',
      :nl,
      '\#{',
      quote_atom(mand),
      ':=_} -> ',
      {:curr, :val},
      '\#{',
      {:asis, group},
      '=>',
      {:curr, :val},
      '};',
      :nl,
      '\#{} -> ',
      {:curr, :val},
      :nl,
      'end,',
      :nl
    ])

    :asn1ct_name.new(:val)
  end

  defp test_for_mandatory([], _) do
    :ok
  end

  defp gen_encode_eag_record({actualPos, virtualPos, len}) do
    val = :asn1ct_gen.mk_var(:asn1ct_name.curr(:val))
    elements = get_input_vars(val, virtualPos, len)
    expr = any_non_value(val, virtualPos, len)

    emit([
      {:next, :val},
      ' = case ',
      expr,
      ' of',
      :nl,
      'false -> setelement(',
      {:asis, actualPos + 1},
      ', ',
      {:curr, :val},
      ', asn1_NOVALUE);',
      :nl,
      'true -> setelement(',
      {:asis, actualPos + 1},
      ', ',
      {:curr, :val},
      ', {extaddgroup,',
      elements,
      '})',
      :nl,
      'end,',
      :nl
    ])

    :asn1ct_name.new(:val)
  end

  defp any_non_value(val, pos, n) do
    l = any_non_value_1(val, pos, n)
    :lists.join(' orelse ', l)
  end

  defp any_non_value_1(_, _, 0) do
    []
  end

  defp any_non_value_1(val, pos, n) do
    var = get_input_var(val, pos)
    [var ++ ' =/= asn1_NOVALUE' | any_non_value_1(val, pos + 1, n - 1)]
  end

  def gen_decode_set(erules, typename, d) do
    gen_decode_constructed(erules, typename, d)
  end

  def gen_decode_sequence(erules, typename, d) do
    gen_decode_constructed(erules, typename, d)
  end

  defp gen_decode_constructed(erule, typename, r_type() = d) do
    imm0 = gen_dec_constructed_imm(erule, typename, r_type() = d)
    imm = opt_imm(imm0)
    :asn1ct_name.start()
    emit_gen_dec_imm(imm)
    emit(['.', :nl, :nl])
  end

  defp opt_imm(imm0) do
    {imm, _} = opt_imm_1(imm0, :unknown, [])
    imm
  end

  defp opt_imm_1([{:imm, imm0, f} | t], al0, acc) do
    {imm, al} = :asn1ct_imm.optimize_alignment(imm0, al0)
    opt_imm_1(t, al, [{:imm, imm, f} | acc])
  end

  defp opt_imm_1([:ignore | t], al, acc) do
    opt_imm_1(t, al, acc)
  end

  defp opt_imm_1([{:ignore, _} = h | t], al, acc) do
    opt_imm_1(t, al, [h | acc])
  end

  defp opt_imm_1([{:safe, :ignore} | t], al, acc) do
    opt_imm_1(t, al, acc)
  end

  defp opt_imm_1([{:safe, _} = h | t], al, acc) do
    opt_imm_1(t, al, [h | acc])
  end

  defp opt_imm_1([{:group, g0} | t], al0, acc) do
    {g, al} = opt_imm_1(g0, al0, [])
    opt_imm_1(t, al, [{:group, g} | acc])
  end

  defp opt_imm_1([emit | t], _, acc) when is_function(emit, 1) do
    opt_imm_1(t, :unknown, [emit | acc])
  end

  defp opt_imm_1([], al, acc) do
    {:lists.reverse(acc), al}
  end

  defp emit_gen_dec_imm(l) do
    emit_gen_dec_imm(l, '', [])
  end

  defp emit_gen_dec_imm([{:ignore, fun} | t], sep, st0) do
    st = fun.(st0)
    emit_gen_dec_imm(t, sep, st)
  end

  defp emit_gen_dec_imm([{:group, l} | t], sep, st0) do
    emit(sep)
    st = emit_gen_dec_imm_group(l, st0)
    emit_gen_dec_imm(t, [:com, :nl], st)
  end

  defp emit_gen_dec_imm([{:imm, imm, emit} | t], sep, st0) do
    emit(sep)
    st = emit.(imm, st0)
    emit_gen_dec_imm(t, [:com, :nl], st)
  end

  defp emit_gen_dec_imm([{:safe, item} | t], sep, st) do
    emit_gen_dec_imm([item | t], sep, st)
  end

  defp emit_gen_dec_imm([emit | t], sep, st0) do
    emit(sep)
    st = emit.(st0)
    emit_gen_dec_imm(t, [:com, :nl], st)
  end

  defp emit_gen_dec_imm([], _, _) do
    :ok
  end

  defp emit_gen_dec_imm_group([h | t], st0) do
    st = emit_gen_dec_group_item(h, st0)
    emit_gen_dec_imm_group(t, st)
  end

  defp emit_gen_dec_imm_group([], st) do
    st
  end

  defp emit_gen_dec_group_item({:ignore, fun}, st) do
    fun.(st)
  end

  defp emit_gen_dec_group_item({:imm, imm, fun}, st) do
    fun.(imm, st)
  end

  defp emit_gen_dec_group_item({:safe, item}, st) do
    emit_gen_dec_group_item(item, st)
  end

  defp emit_gen_dec_group_item(emit, st) do
    emit.(st)
  end

  defp gen_dec_constructed_imm(erule, typename, r_type() = d) do
    {compList, tableConsInfo} =
      case r_type(d, :def) do
        r_SEQUENCE(tablecinf: tCI, components: cL) ->
          {add_textual_order(cL), tCI}

        r_SET(tablecinf: tCI, components: cL) ->
          {cL, tCI}
      end

    ext = extensible_dec(compList)

    emitExt =
      case ext do
        {:ext, _Pos, _NumExt} ->
          gen_dec_extension_value()

        _ ->
          :ignore
      end

    optionals = optionals(compList)

    emitOpt =
      case optionals do
        [] ->
          :ignore

        [_ | _] ->
          gen_dec_optionals(optionals)
      end

    objSetInfo =
      case tableConsInfo do
        r_simpletableattributes(
          objectsetname: objectSet,
          c_name: attrN,
          usedclassfield: uniqueFieldName,
          uniqueclassfield: uniqueFieldName,
          valueindex: valIndex
        ) ->
          f = fn r_ComponentType(typespec: cT) ->
            case {:asn1ct_gen.get_constraint(
                    r_type(cT, :constraint),
                    :componentrelation
                  ), r_type(cT, :tablecinf)} do
              {:no, [{:objfun, _} | _R]} ->
                true

              _ ->
                false
            end
          end

          case :lists.any(f, flat_complist(compList)) do
            true ->
              {{attrN, {:deep, objectSet, uniqueFieldName, valIndex}}, uniqueFieldName, valIndex}

            false ->
              {{attrN, objectSet}, uniqueFieldName, valIndex}
          end

        _ ->
          case r_type(d, :tablecinf) do
            [{:objfun, _} | _] ->
              {{'got objfun through args', 'ObjFun'}, false, false}

            _ ->
              {false, false, false}
          end
      end

    {decObjInf, _, _} = objSetInfo

    emitComp =
      gen_dec_components_call(erule, typename, compList, decObjInf, ext, length(optionals))

    emitObjSets = gen_dec_objsets_fun(erule, objSetInfo)

    emitPack = fn _ ->
      gen_dec_pack(erule, typename, compList)
    end

    restGroup = {:group, [{:safe, emitObjSets}, {:safe, emitPack}]}
    [emitExt, emitOpt | emitComp ++ [restGroup]]
  end

  defp gen_dec_objsets_fun(gen, objSetInfo) do
    fn {accTerm, accBytes} ->
      {_, _UniqueFName, valueIndex} = objSetInfo

      case {accTerm, accBytes} do
        {[], []} ->
          :ok

        {_, []} ->
          :ok

        {[{objSet, leadingAttr, term}], listOfOpenTypes} ->
          valueMatch = value_match(gen, valueIndex, term)

          _ =
            for t <- listOfOpenTypes do
              gen_dec_open_type(gen, valueMatch, objSet, leadingAttr, t)
              emit([:com, :nl])
            end

          :ok
      end
    end
  end

  defp gen_dec_pack(gen, typename, compList) do
    case typename do
      [:EXTERNAL] ->
        dec_external(gen, typename)

      _ ->
        :asn1ct_name.new(:res)
        gen_dec_do_pack(gen, typename, compList)
        emit([:com, :nl, '{', {:curr, :res}, ',', {:curr, :bytes}, '}'])
    end
  end

  defp dec_external(r_gen(pack: :record) = gen, typename) do
    recordName =
      :erlang.list_to_atom(
        record_name(
          gen,
          typename
        )
      )

    all =
      for term <- :asn1ct_name.all(:term) do
        {:var, term}
      end

    record = [{:asis, recordName} | all]

    emit([
      'OldFormat={',
      :lists.join(',', record),
      '},',
      :nl,
      'ASN11994Format =',
      :nl,
      {:call, :ext, :transform_to_EXTERNAL1994, ['OldFormat']},
      :com,
      :nl,
      '{ASN11994Format,',
      {:curr, :bytes},
      '}'
    ])
  end

  defp dec_external(r_gen(pack: :map), _Typename) do
    vars = :asn1ct_name.all(:term)
    names = [:"direct-reference", :"indirect-reference", :"data-value-descriptor", :encoding]
    zipped = :lists.zip(names, vars)

    mapInit =
      :lists.join(
        ',',
        for {n, v} <- zipped do
          ['\'', n, '\'=>', {:var, v}]
        end
      )

    emit([
      'OldFormat = \#{',
      mapInit,
      '}',
      :com,
      :nl,
      'ASN11994Format =',
      :nl,
      {:call, :ext, :transform_to_EXTERNAL1994_maps, ['OldFormat']},
      :com,
      :nl,
      '{ASN11994Format,',
      {:curr, :bytes},
      '}'
    ])
  end

  defp gen_dec_do_pack(r_gen(pack: :record) = gen, typeName, compList) do
    zipped0 =
      zip_components(
        compList,
        :asn1ct_name.all(:term)
      )

    zipped = textual_order(zipped0)
    recordName = ['\'', record_name(gen, typeName), '\'']

    l = [
      recordName
      | for {_, var} <- zipped do
          {:var, var}
        end
    ]

    emit([{:curr, :res}, ' = {', :lists.join(',', l), '}'])
  end

  defp gen_dec_do_pack(r_gen(pack: :map), _, compList0) do
    compList = enc_flatten_components(compList0)

    zipped0 =
      zip_components(
        compList,
        :asn1ct_name.all(:term)
      )

    zipped = textual_order(zipped0)

    pF = fn
      {r_ComponentType(prop: :OPTIONAL), _} ->
        false

      {_, _} ->
        true
    end

    {mandatory, optional} = :lists.partition(pF, zipped)

    l =
      for {r_ComponentType(name: name), var} <- mandatory do
        [{:asis, name}, '=>', {:var, var}]
      end

    emit([{:curr, :res}, ' = \#{', :lists.join(',', l), '}'])
    gen_dec_map_optional(optional)
    gen_dec_merge_maps(:asn1ct_name.all(:map))
  end

  defp gen_dec_map_optional([{r_ComponentType(name: name), var} | t]) do
    :asn1ct_name.new(:res)

    emit([
      :com,
      :nl,
      {:curr, :res},
      ' = case ',
      {:var, var},
      ' of',
      :nl,
      '  asn1_NOVALUE -> ',
      {:prev, :res},
      ';',
      :nl,
      '  _ -> ',
      {:prev, :res},
      '\#{',
      {:asis, name},
      '=>',
      {:var, var},
      '}',
      :nl,
      'end'
    ])

    gen_dec_map_optional(t)
  end

  defp gen_dec_map_optional([]) do
    :ok
  end

  defp gen_dec_merge_maps([m | ms]) do
    :asn1ct_name.new(:res)
    emit([:com, :nl, {:curr, :res}, ' = maps:merge(', {:prev, :res}, ', ', {:var, m}, ')'])
    gen_dec_merge_maps(ms)
  end

  defp gen_dec_merge_maps([]) do
    :ok
  end

  defp quote_atom(a) when is_atom(a) do
    :io_lib.format('~p', [a])
  end

  defp record_name(gen, typename0) do
    [topType | typename1] = :lists.reverse(typename0)
    typename = filter_ext_add_groups(typename1, [topType])
    :lists.concat([get_record_name_prefix(gen), :asn1ct_gen.list2rname(typename)])
  end

  defp filter_ext_add_groups([h | t], acc) when is_atom(h) do
    case :erlang.atom_to_list(h) do
      'ExtAddGroup' ++ _ ->
        filter_ext_add_groups(t, acc)

      _ ->
        filter_ext_add_groups(t, [h | acc])
    end
  end

  defp filter_ext_add_groups([h | t], acc) do
    filter_ext_add_groups(t, [h | acc])
  end

  defp filter_ext_add_groups([], acc) do
    acc
  end

  defp zip_components({root, ext}, vars) do
    zip_components({root, ext, []}, vars)
  end

  defp zip_components({r1, ext0, r2}, vars) do
    ext =
      for c <- ext0 do
        mark_optional(c)
      end

    zip_components(r1 ++ r2 ++ ext, vars)
  end

  defp zip_components(cs, vars) when is_list(cs) do
    zip_components_1(cs, vars)
  end

  defp zip_components_1([r_ComponentType() = c | cs], [v | vs]) do
    [{c, v} | zip_components_1(cs, vs)]
  end

  defp zip_components_1([_ | cs], vs) do
    zip_components_1(cs, vs)
  end

  defp zip_components_1([], []) do
    []
  end

  defp textual_order([{r_ComponentType(textual_order: :undefined), _} | _] = l) do
    l
  end

  defp textual_order(l0) do
    l =
      for {r_ComponentType(textual_order: ix), _} = p <- l0 do
        {ix, p}
      end

    for {_, c} <- :lists.sort(l) do
      c
    end
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

  defp to_textual_order(cs) do
    cs
  end

  defp gen_dec_open_type(
         erule,
         val,
         {xmod, xtype},
         leadingAttr,
         {_, {name, restFieldNames}, term, tmpTerm, prop}
       ) do
    r_typedef(typespec: objSet0) = :asn1_db.dbget(xmod, xtype)
    r_ObjectSet(class: class, set: objSet1) = objSet0
    r_Externaltypereference(module: clMod, type: clType) = class
    r_classdef(typespec: classDef) = :asn1_db.dbget(clMod, clType)
    r_objectclass(fields: classFields) = classDef
    extensible = :lists.member(:EXTENSIONMARK, objSet1)
    typename = [name, clType]
    objSet = index_object_set(erule, clType, name, objSet1, classFields)
    key = :erlang.md5(:erlang.term_to_binary({:decode, objSet, restFieldNames, prop, extensible}))

    gen = fn _Fd, n ->
      dec_objset_optional(n, prop)
      dec_objset(erule, n, objSet, restFieldNames, typename)
      dec_objset_default(n, name, leadingAttr, extensible)
    end

    prefix = :lists.concat(['dec_os_', name])
    f = :asn1ct_func.call_gen(prefix, key, gen)
    emit([term, ' = ', {:asis, f}, '(', tmpTerm, ', ', val, ')'])
  end

  defp dec_objset_optional(n, {:DEFAULT, val}) do
    dec_objset_optional_1(n, val)
  end

  defp dec_objset_optional(n, :OPTIONAL) do
    dec_objset_optional_1(n, :asn1_NOVALUE)
  end

  defp dec_objset_optional(_N, :mandatory) do
    :ok
  end

  defp dec_objset_optional_1(n, val) do
    emit([{:asis, n}, '(', {:asis, val}, ', _Id) ->', :nl, {:asis, val}, ';', :nl])
  end

  defp dec_objset(_Erule, _N, [], _, _) do
    :ok
  end

  defp dec_objset(erule, n, [obj | objs], restFields, cl) do
    dec_objset_1(erule, n, obj, restFields, cl)
    emit([';', :nl])
    dec_objset(erule, n, objs, restFields, cl)
  end

  defp dec_objset_default(n, c, leadingAttr, false) do
    emit([
      {:asis, n},
      '(Bytes, Id) ->',
      :nl,
      'exit({\'Type not compatible with table constraint\',{{component,',
      {:asis, c},
      '},{value,Bytes},{unique_name_and_value,',
      {:asis, leadingAttr},
      ',Id}}}).',
      :nl,
      :nl
    ])
  end

  defp dec_objset_default(n, _, _, true) do
    emit([
      {:asis, n},
      '(Bytes, Id) ->',
      :nl
      | case :asn1ct.use_legacy_types() do
          false ->
            ['{asn1_OPENTYPE,Bytes}.', :nl, :nl]

          true ->
            ['Bytes.', :nl, :nl]
        end
    ])
  end

  defp dec_objset_1(erule, n, {id, obj}, restFields, typename) do
    emit([{:asis, n}, '(Bytes, Id) when Id =:= ', {:asis, id}, ' ->', :nl])
    dec_objset_2(erule, obj, restFields, typename)
  end

  defp dec_objset_2(erule, obj, restFields0, typename) do
    case obj do
      r_typedef(name: {:primitive, :bif}, typespec: type) ->
        imm = :asn1ct_gen_per.gen_dec_imm(erule, type)
        {term, _} = :asn1ct_imm.dec_slim_cg(imm, :Bytes)
        emit([:com, :nl, term])

      r_typedef(name: {:constructed, :bif}, typespec: type) = def__ ->
        prefix = 'dec_outlined_'
        key = {:dec_outlined, def__}

        gen = fn _Fd, name ->
          gen_dec_obj(erule, name, typename, type)
        end

        func = :asn1ct_func.call_gen(prefix, key, gen)
        emit(['{Term,_} = ', {:asis, func}, '(Bytes)', :com, :nl, 'Term'])

      r_typedef(name: type) ->
        emit(['{Result,_} = ', {:asis, enc_func('dec_', type)}, '(Bytes),', :nl, 'Result'])

      r_Externaltypereference(module: mod, type: type) ->
        emit('{Term,_} = ')
        func = enc_func('dec_', type)

        case :erlang.get(:currmod) do
          ^mod ->
            emit([{:asis, func}, '(Bytes)'])

          _ ->
            emit([{:asis, mod}, ':', {:asis, func}, '(Bytes)'])
        end

        emit([:com, :nl, 'Term'])

      r_Externalvaluereference(module: mod, value: value) ->
        case :asn1_db.dbget(mod, value) do
          r_typedef(typespec: r_Object(def: def__)) ->
            {:object, _, fields} = def__
            [nextField | restFields] = restFields0
            {^nextField, typedef} = :lists.keyfind(nextField, 1, fields)
            dec_objset_2(erule, typedef, restFields, typename)
        end
    end
  end

  defp gen_dec_obj(erules, name, typename, type) do
    emit([{:asis, name}, '(Bytes) ->', :nl])
    innerType = :asn1ct_gen.get_inner(r_type(type, :def))
    :asn1ct_gen.gen_decode_constructed(erules, typename, innerType, type)
  end

  def gen_encode_choice(erule, topType, d) do
    :asn1ct_name.start()
    imm = gen_encode_choice_imm(erule, topType, d)
    :asn1ct_imm.enc_cg(imm, is_aligned(erule))
    emit(['.', :nl])
  end

  defp gen_encode_choice_imm(erule, topType, r_type(def: {:CHOICE, compList})) do
    ext = extensible_enc(compList)
    aligned = is_aligned(erule)
    cs = gen_enc_choice(erule, topType, compList, ext)

    [
      {:assign, {:expr, '{ChoiceTag,ChoiceVal}'}, 'Val'}
      | :asn1ct_imm.per_enc_choice({:var, 'ChoiceTag'}, cs, aligned)
    ]
  end

  def gen_decode_choice(erules, typename, d) when elem(d, 0) === :type do
    :asn1ct_name.start()
    :asn1ct_name.new(:bytes)
    {:CHOICE, compList} = r_type(d, :def)
    ext = extensible_enc(compList)
    gen_dec_choice(erules, typename, compList, ext)
    emit(['.', :nl])
  end

  def gen_encode_sof(erule, typename, seqOrSetOf, d) do
    :asn1ct_name.start()
    imm = gen_encode_sof_imm(erule, typename, seqOrSetOf, d)
    :asn1ct_imm.enc_cg(imm, is_aligned(erule))
    emit(['.', :nl, :nl])
  end

  defp gen_encode_sof_imm(erule, typename, seqOrSetOf, r_type() = d) do
    {_SeqOrSetOf, componentType} = r_type(d, :def)
    aligned = is_aligned(erule)
    compType = r_type(componentType, :def)

    constructed_Suffix =
      :asn1ct_gen.constructed_suffix(
        seqOrSetOf,
        compType
      )

    conttype = :asn1ct_gen.get_inner(compType)
    currmod = :erlang.get(:currmod)

    imm0 =
      case :asn1ct_gen.type(conttype) do
        {:primitive, :bif} ->
          :asn1ct_gen_per.gen_encode_prim_imm({:var, 'Comp'}, componentType, aligned)

        {:constructed, :bif} ->
          typeName = [constructed_Suffix | typename]
          enc = enc_func(:asn1ct_gen.list2name(typeName))

          objArg =
            case r_type(d, :tablecinf) do
              [{:objfun, _} | _] ->
                [{:var, 'ObjFun'}]

              _ ->
                []
            end

          [{:apply, {:local, enc, compType}, [{:var, 'Comp'} | objArg]}]

        r_Externaltypereference(module: ^currmod, type: ename) ->
          [{:apply, {:local, enc_func(ename), compType}, [{:var, 'Comp'}]}]

        r_Externaltypereference(module: eMod, type: ename) ->
          [{:apply, {eMod, enc_func(ename), compType}, [{:var, 'Comp'}]}]

        :ASN1_OPEN_TYPE ->
          :asn1ct_gen_per.gen_encode_prim_imm(
            {:var, 'Comp'},
            r_type(def: :ASN1_OPEN_TYPE),
            aligned
          )
      end

    :asn1ct_imm.per_enc_sof({:var, 'Val'}, r_type(d, :constraint), :Comp, imm0, aligned)
  end

  def gen_decode_sof(erules, typename, seqOrSetOf, r_type() = d) do
    :asn1ct_name.start()
    do_gen_decode_sof(erules, typename, seqOrSetOf, d)
    emit(['.', :nl, :nl])
  end

  defp do_gen_decode_sof(erules, typename, seqOrSetOf, d) do
    {_SeqOrSetOf, componentType} = r_type(d, :def)

    sizeConstraint =
      :asn1ct_imm.effective_constraint(
        :bitstring,
        r_type(d, :constraint)
      )

    objFun =
      case r_type(d, :tablecinf) do
        [{:objfun, _} | _R] ->
          ', ObjFun'

        _ ->
          ''
      end

    {num, buf} = gen_decode_length(sizeConstraint, erules)
    key = :erlang.md5(:erlang.term_to_binary({typename, seqOrSetOf, componentType}))

    gen = fn _Fd, name ->
      gen_decode_sof_components(erules, name, typename, seqOrSetOf, componentType)
    end

    f = :asn1ct_func.call_gen('dec_components', key, gen)
    emit([',', :nl, {:asis, f}, '(', num, ', ', buf, objFun, ', [])'])
  end

  defp is_aligned(r_gen(erule: :per, aligned: aligned)) do
    aligned
  end

  defp gen_decode_length(constraint, erule) do
    emit(['%% Length with constraint ', {:asis, constraint}, :nl])
    imm = :asn1ct_imm.per_dec_length(constraint, true, is_aligned(erule))
    :asn1ct_imm.dec_slim_cg(imm, 'Bytes')
  end

  defp gen_decode_sof_components(erule, name, typename, seqOrSetOf, cont) do
    {objFun, objFun_Var} =
      case r_type(cont, :tablecinf) do
        [{:objfun, _} | _R] ->
          {', ObjFun', ', _'}

        _ ->
          {'', ''}
      end

    emit([
      {:asis, name},
      '(0, Bytes',
      objFun_Var,
      ', Acc) ->',
      :nl,
      '{lists:reverse(Acc),Bytes};',
      :nl
    ])

    emit([{:asis, name}, '(Num, Bytes', objFun, ', Acc) ->', :nl, '{Term,Remain} = '])

    constructed_Suffix =
      :asn1ct_gen.constructed_suffix(
        seqOrSetOf,
        r_type(cont, :def)
      )

    conttype = :asn1ct_gen.get_inner(r_type(cont, :def))

    case :asn1ct_gen.type(conttype) do
      {:primitive, :bif} ->
        :asn1ct_gen_per.gen_dec_prim(erule, cont, 'Bytes')
        emit([:com, :nl])

      {:constructed, :bif} ->
        newTypename = [constructed_Suffix | typename]
        emit([{:asis, dec_func(:asn1ct_gen.list2name(newTypename))}, '(Bytes', objFun, '),', :nl])

      r_Externaltypereference() = etype ->
        :asn1ct_gen_per.gen_dec_external(etype, 'Bytes')
        emit([:com, :nl])

      :ASN1_OPEN_TYPE ->
        :asn1ct_gen_per.gen_dec_prim(erule, r_type(def: :ASN1_OPEN_TYPE), 'Bytes')
        emit([:com, :nl])

      _ ->
        emit([{:asis, dec_func(conttype)}, '(Bytes),', :nl])
    end

    emit([{:asis, name}, '(Num-1, Remain', objFun, ', [Term|Acc]).', :nl])
  end

  defp extensible_dec(compList) when is_list(compList) do
    :noext
  end

  defp extensible_dec({rootList, extList}) do
    {:ext, length(rootList) + 1, ext_length(extList)}
  end

  defp extensible_dec({rl1, ext, rl2}) do
    {:ext, length(rl1) + length(rl2) + 1, ext_length(ext)}
  end

  defp extensible_enc(compList) when is_list(compList) do
    :noext
  end

  defp extensible_enc({rootList, extList}) do
    {:ext, length(rootList) + 1, ext_length(extList)}
  end

  defp extensible_enc({rl1, ext, _Rl2}) do
    {:ext, length(rl1) + 1, ext_length(ext)}
  end

  defp ext_length(extList) do
    ext_length(extList, :normal, 0)
  end

  defp ext_length([{:ExtensionAdditionGroup, _Num} | t], _, acc) do
    ext_length(t, :group, acc)
  end

  defp ext_length([:ExtensionAdditionGroupEnd | t], :group, acc) do
    ext_length(t, :normal, acc + 1)
  end

  defp ext_length([r_ComponentType() | t], state = :group, acc) do
    ext_length(t, state, acc)
  end

  defp ext_length([r_ComponentType() | t], state = :normal, acc) do
    ext_length(t, state, acc + 1)
  end

  defp ext_length([], _, acc) do
    acc
  end

  defp extgroup_pos_and_length(compList) when is_list(compList) do
    {:extgrouppos, []}
  end

  defp extgroup_pos_and_length({rootList, extList}) do
    actualPos = length(rootList) + 1
    virtualPos = actualPos
    extgrouppos(extList, actualPos, virtualPos, [])
  end

  defp extgroup_pos_and_length({rootList, extList, _Rl2}) do
    extgroup_pos_and_length({rootList, extList})
  end

  defp extgrouppos([{:ExtensionAdditionGroup, _Num} | t], actualPos, virtualPos, acc) do
    extgrouppos(t, actualPos, virtualPos, 0, acc)
  end

  defp extgrouppos([_ | t], actualPos, virtualPos, acc) do
    extgrouppos(t, actualPos + 1, virtualPos + 1, acc)
  end

  defp extgrouppos([], _, _, acc) do
    {:extgrouppos, :lists.reverse(acc)}
  end

  defp extgrouppos([:ExtensionAdditionGroupEnd | t], actualPos, virtualPos, len, acc) do
    extgrouppos(t, actualPos + 1, virtualPos + len, [{actualPos, virtualPos, len} | acc])
  end

  defp extgrouppos([_ | t], actualPos, virtualPos, len, acc) do
    extgrouppos(t, actualPos, virtualPos, len + 1, acc)
  end

  defp gen_dec_extension_value() do
    imm0 = {:get_bits, 1, [1]}

    e = fn imm, _ ->
      emit(['{Ext,', {:next, :bytes}, '} = '])
      bytesVar = :asn1ct_gen.mk_var(:asn1ct_name.curr(:bytes))
      :asn1ct_imm.dec_code_gen(imm, bytesVar)
      :asn1ct_name.new(:bytes)
    end

    {:imm, imm0, e}
  end

  defp gen_dec_optionals(optionals) do
    imm0 = {:get_bits, length(optionals), [1]}

    e = fn imm, _ ->
      bytesVar = :asn1ct_gen.mk_var(:asn1ct_name.curr(:bytes))
      emit(['{Opt,', {:next, :bytes}, '} = '])
      :asn1ct_imm.dec_code_gen(imm, bytesVar)
      :asn1ct_name.new(:bytes)
    end

    {:imm, imm0, e}
  end

  defp optionals({root1, ext, root2}) do
    opt1 = optionals(root1, 1)

    extComps =
      length(
        for c = r_ComponentType() <- ext do
          c
        end
      )

    opt2 = optionals(root2, 1 + length(root1) + extComps)
    opt1 ++ opt2
  end

  defp optionals({l, _Ext}) do
    optionals(l, 1)
  end

  defp optionals(l) do
    optionals(l, 1)
  end

  defp optionals([r_ComponentType(prop: :OPTIONAL) | rest], pos) do
    [{pos, [:asn1_NOVALUE]} | optionals(rest, pos + 1)]
  end

  defp optionals(
         [r_ComponentType(typespec: t, prop: {:DEFAULT, val}) | cs],
         pos
       ) do
    vals = def_values(t, val)
    [{pos, vals} | optionals(cs, pos + 1)]
  end

  defp optionals([r_ComponentType() | rest], pos) do
    optionals(rest, pos + 1)
  end

  defp optionals([], _) do
    []
  end

  defp create_optionality_table(cs) do
    isOptional = fn
      :OPTIONAL ->
        true

      {:DEFAULT, _} ->
        true

      _ ->
        false
    end

    optionalsElNum =
      for r_ComponentType(
            prop: o,
            textual_order: tO
          ) <- cs,
          isOptional.(o) do
        tO
      end

    {table, _} =
      :lists.mapfoldl(
        fn x, num ->
          {{num, x}, num + 1}
        end,
        1,
        :lists.sort(optionalsElNum)
      )

    table
  end

  defp get_optionality_pos(textPos, optTable) do
    case :lists.keysearch(textPos, 2, optTable) do
      {:value, {optNum, _}} ->
        optNum

      _ ->
        :no_num
    end
  end

  defp add_textual_order(cs) when is_list(cs) do
    {newCs, _} = add_textual_order1(cs, 1)
    newCs
  end

  defp add_textual_order({root, ext}) do
    {newRoot, num} = add_textual_order1(root, 1)
    {newExt, _} = add_textual_order1(ext, num)
    {newRoot, newExt}
  end

  defp add_textual_order({r1, ext, r2}) do
    {newR1, num1} = add_textual_order1(r1, 1)
    {newExt, num2} = add_textual_order1(ext, num1)
    {newR2, _} = add_textual_order1(r2, num2)
    {newR1, newExt, newR2}
  end

  defp add_textual_order1(cs, numIn) do
    :lists.mapfoldl(
      fn
        c = r_ComponentType(), num ->
          {r_ComponentType(c, textual_order: num), num + 1}

        otherMarker, num ->
          {otherMarker, num}
      end,
      numIn,
      cs
    )
  end

  defp gen_enc_components_call(erule, topType, {root, extList}, dynamicEnc, ext) do
    gen_enc_components_call(erule, topType, {root, extList, []}, dynamicEnc, ext)
  end

  defp gen_enc_components_call(erule, topType, {r1, extList0, r2} = cL, dynamicEnc, ext) do
    root = r1 ++ r2
    imm0 = gen_enc_components_call1(erule, topType, root, dynamicEnc, :noext)

    extImm =
      case ext do
        {:ext, _, extNum} when extNum > 0 ->
          [{:var, 'Extensions'}]

        _ ->
          []
      end

    {:extgrouppos, extGroupPosLen} = extgroup_pos_and_length(cL)

    extList1 =
      wrap_extensionAdditionGroups(
        extList0,
        extGroupPosLen
      )

    extList =
      for c <- extList1 do
        mark_optional(c)
      end

    imm1 = gen_enc_components_call1(erule, topType, extList, dynamicEnc, ext)
    imm0 ++ [extImm | imm1]
  end

  defp gen_enc_components_call(erule, topType, compList, dynamicEnc, ext) do
    gen_enc_components_call1(erule, topType, compList, dynamicEnc, ext)
  end

  defp mark_optional(r_ComponentType(prop: prop0) = c) do
    prop =
      case prop0 do
        :mandatory ->
          :OPTIONAL

        :OPTIONAL = keep ->
          keep

        {:DEFAULT, _} = keep ->
          keep
      end

    r_ComponentType(c, prop: prop)
  end

  defp mark_optional(other) do
    other
  end

  defp gen_enc_components_call1(gen, topType, [c | rest], dynamicEnc, ext) do
    r_ComponentType(name: cname, typespec: type, prop: prop, textual_order: num) = c
    innerType = :asn1ct_gen.get_inner(r_type(type, :def))
    commentString = attribute_comment(innerType, num, cname)
    immComment = :asn1ct_imm.enc_comment(commentString)
    {imm0, element} = enc_fetch_field(gen, num, prop)
    imm1 = gen_enc_line_imm(gen, topType, cname, type, element, dynamicEnc, ext)

    imm2 =
      case prop do
        :mandatory ->
          imm1

        :OPTIONAL ->
          enc_absent(gen, element, [:asn1_NOVALUE], imm1)

        {:DEFAULT, def__} when ext === :noext ->
          defValues = def_values(type, def__)
          enc_absent(gen, element, defValues, imm1)

        {:DEFAULT, _} ->
          enc_absent(gen, element, [:asn1_DEFAULT], imm1)
      end

    imm =
      case imm2 do
        [] ->
          []

        _ ->
          [immComment | imm0 ++ imm2]
      end

    [imm | gen_enc_components_call1(gen, topType, rest, dynamicEnc, ext)]
  end

  defp gen_enc_components_call1(_Gen, _TopType, [], _, _) do
    []
  end

  defp enc_absent(gen, var, absent0, imm) do
    absent = translate_missing_value(gen, absent0)
    :asn1ct_imm.enc_absent(var, absent, imm)
  end

  defp translate_missing_value(r_gen(pack: :record), optionals) do
    optionals
  end

  defp translate_missing_value(r_gen(pack: :map), optionals) do
    case optionals do
      [:asn1_NOVALUE | t] ->
        [:asn1__MISSING_IN_MAP | t]

      [:asn1_DEFAULT | t] ->
        [:asn1__MISSING_IN_MAP | t]

      {:call, _, _, _} ->
        optionals
    end
  end

  defp enc_fetch_field(r_gen(pack: :record), num, _Prop) do
    val = make_var(:val)
    :asn1ct_imm.enc_element(num + 1, val)
  end

  defp enc_fetch_field(r_gen(pack: :map), num, _) do
    {[], {:var, :lists.concat(['Input@', num])}}
  end

  defp def_values(r_type(def: r_Externaltypereference(module: mod, type: type)), def__) do
    r_typedef(typespec: t) = :asn1_db.dbget(mod, type)
    def_values(t, def__)
  end

  defp def_values(r_type(def: {:"BIT STRING", []}), bs) when is_bitstring(bs) do
    case :asn1ct.use_legacy_types() do
      false ->
        [:asn1_DEFAULT, bs]

      true ->
        listBs =
          for <<(<<b::size(1)>> <- bs)>> do
            b
          end

        intBs =
          :lists.foldl(
            fn b, a ->
              a <<< 1 ||| b
            end,
            0,
            :lists.reverse(listBs)
          )

        sz = bit_size(bs)

        compact =
          case 8 - rem(sz, 8) do
            8 ->
              {0, bs}

            unused ->
              {unused, <<bs::size(sz)-bits, 0::size(unused)>>}
          end

        [:asn1_DEFAULT, bs, compact, listBs, intBs]
    end
  end

  defp def_values(r_type(def: {:"BIT STRING", [_ | _] = ns}), list)
       when is_list(list) do
    bs = :asn1ct_gen.named_bitstring_value(list, ns)

    as =
      case :asn1ct.use_legacy_types() do
        false ->
          [list, bs]

        true ->
          listBs =
            for <<(<<b::size(1)>> <- bs)>> do
              b
            end

          intBs =
            :lists.foldl(
              fn b, a ->
                a <<< 1 ||| b
              end,
              0,
              :lists.reverse(listBs)
            )

          [list, bs, listBs, intBs]
      end

    {:call, :per_common, :is_default_bitstring, as}
  end

  defp def_values(r_type(def: {:INTEGER, ns}), def__) do
    [
      :asn1_DEFAULT,
      def__
      | case :lists.keyfind(def__, 2, ns) do
          false ->
            []

          {val, ^def__} ->
            [val]
        end
    ]
  end

  defp def_values(_, def__) do
    [:asn1_DEFAULT, def__]
  end

  defp gen_enc_line_imm(erule, topType, cname, type, element, dynamicEnc, ext) do
    imm0 = gen_enc_line_imm_1(erule, topType, cname, type, element, dynamicEnc)
    aligned = is_aligned(erule)

    case ext do
      {:ext, _Ep2, _} ->
        :asn1ct_imm.per_enc_open_type(imm0, aligned)

      _ ->
        imm0
    end
  end

  defp gen_enc_line_imm_1(erule, topType, cname, type, element, dynamicEnc) do
    atype =
      case type do
        r_type(def: r_ObjectClassFieldType(type: innerType)) ->
          innerType

        _ ->
          :asn1ct_gen.get_inner(r_type(type, :def))
      end

    aligned = is_aligned(erule)

    case atype do
      {:typefield, _} ->
        {_LeadingAttrName, fun} = dynamicEnc

        case r_ObjectClassFieldType(r_type(type, :def), :fieldname) do
          {name, restFieldNames} when is_atom(name) ->
            imm = enc_var_type_call(erule, name, restFieldNames, type, fun, element)
            :asn1ct_imm.per_enc_open_type(imm, aligned)
        end

      _ ->
        currMod = :erlang.get(:currmod)

        case :asn1ct_gen.type(atype) do
          r_Externaltypereference(module: ^currMod, type: eType) ->
            [{:apply, {:local, enc_func(eType), atype}, [element]}]

          r_Externaltypereference(module: mod, type: eType) ->
            [{:apply, {mod, enc_func(eType), atype}, [element]}]

          {:primitive, :bif} ->
            :asn1ct_gen_per.gen_encode_prim_imm(element, type, aligned)

          :ASN1_OPEN_TYPE ->
            case r_type(type, :def) do
              r_ObjectClassFieldType(type: openType) ->
                :asn1ct_gen_per.gen_encode_prim_imm(element, r_type(def: openType), aligned)

              _ ->
                :asn1ct_gen_per.gen_encode_prim_imm(element, type, aligned)
            end

          {:constructed, :bif} ->
            newTypename = [cname | topType]
            enc = enc_func(:asn1ct_gen.list2name(newTypename))

            case {r_type(type, :tablecinf), dynamicEnc} do
              {[{:objfun, _} | _R], {_, encFun}} ->
                [{:apply, {:local, enc, type}, [element, encFun]}]

              _ ->
                [{:apply, {:local, enc, type}, [element]}]
            end
        end
    end
  end

  defp enc_func(type) do
    enc_func('enc_', type)
  end

  defp enc_func(prefix, name) do
    :erlang.list_to_atom(:lists.concat([prefix, name]))
  end

  defp enc_var_type_call(erule, name, restFieldNames, r_type(tablecinf: tCI), fun, val) do
    [{:objfun, r_Externaltypereference(module: xmod, type: xtype)}] = tCI
    r_typedef(typespec: objSet0) = :asn1_db.dbget(xmod, xtype)
    r_ObjectSet(class: class, set: objSet1) = objSet0
    r_Externaltypereference(module: clMod, type: clType) = class
    r_classdef(typespec: classDef) = :asn1_db.dbget(clMod, clType)
    r_objectclass(fields: classFields) = classDef
    extensible = :lists.member(:EXTENSIONMARK, objSet1)
    objSet = index_object_set(erule, clType, name, objSet1, classFields)
    key = :erlang.md5(:erlang.term_to_binary({:encode, objSet, restFieldNames, extensible}))
    typeName = [clType, name]
    imm = enc_objset_imm(erule, typeName, name, objSet, restFieldNames, extensible)
    lambda = {:lambda, [{:var, 'Val'}, {:var, 'Id'}], imm}

    gen = fn _Fd, n ->
      aligned = is_aligned(erule)
      emit([{:asis, n}, '(Val, Id) ->', :nl])
      :asn1ct_imm.enc_cg(imm, aligned)
      emit(['.', :nl])
    end

    prefix = :lists.concat(['enc_os_', name])
    [{:call_gen, prefix, key, gen, lambda, [val, fun]}]
  end

  defp index_object_set(_Erules, _ClType, name, set0, classFields) do
    set = index_object_set_1(name, set0, classFields)
    :lists.sort(set)
  end

  defp index_object_set_1(name, [{_, key, code} | t], classFields) do
    case index_object_set_2(name, code, classFields) do
      :none ->
        index_object_set_1(name, t, classFields)

      type ->
        [{key, type} | index_object_set_1(name, t, classFields)]
    end
  end

  defp index_object_set_1(name, [_ | t], classFields) do
    index_object_set_1(name, t, classFields)
  end

  defp index_object_set_1(_, [], _) do
    []
  end

  defp index_object_set_2(name, [{name, type} | _], _ClassFields) do
    type
  end

  defp index_object_set_2(name, [_ | t], classFields) do
    index_object_set_2(name, t, classFields)
  end

  defp index_object_set_2(name, [], classFields) do
    case :lists.keyfind(name, 2, classFields) do
      {:typefield, ^name, :OPTIONAL} ->
        :none

      {:objectfield, ^name, _, _, :OPTIONAL} ->
        :none

      {:typefield, ^name, {:DEFAULT, r_type() = type}} ->
        innerType = :asn1ct_gen.get_inner(r_type(type, :def))

        case :asn1ct_gen.type(innerType) do
          {:primitive, :bif} ->
            r_typedef(name: {:primitive, :bif}, typespec: type)

          {:constructed, :bif} ->
            r_typedef(name: {:constructed, :bif}, typespec: type)
        end
    end
  end

  defp enc_objset_imm(erule, typeName, component, objSet, restFieldNames, extensible) do
    aligned = is_aligned(erule)

    e =
      {:error,
       fn ->
         emit([
           'exit({\'Type not compatible with table constraint\',{component,',
           {:asis, component},
           '},{value,Val},{unique_name_and_value,\'_\'}})',
           :nl
         ])
       end}

    [
      {:cond,
       for {key, obj} <- objSet do
         [{:eq, {:var, 'Id'}, key} | enc_obj(erule, obj, typeName, restFieldNames, aligned)]
       end ++
         [
           [
             :_,
             case extensible do
               false ->
                 e

               true ->
                 case :asn1ct.use_legacy_types() do
                   false ->
                     {:call, :per_common, :open_type_to_binary, [{:var, 'Val'}]}

                   true ->
                     {:call, :per_common, :legacy_open_type_to_binary, [{:var, 'Val'}]}
                 end
             end
           ]
         ]}
    ]
  end

  defp enc_obj(erule, obj, typeName, restFieldNames0, aligned) do
    val = {:var, 'Val'}

    case obj do
      r_typedef(name: {:constructed, :bif}, typespec: type) = def__ ->
        prefix = 'enc_outlined_'
        key = {:enc_outlined, def__}

        gen = fn _Fd, name ->
          gen_enc_obj(erule, name, typeName, type)
        end

        [{:call_gen, prefix, key, gen, :undefined, [val]}]

      r_typedef(name: {:primitive, :bif}, typespec: def__) ->
        :asn1ct_gen_per.gen_encode_prim_imm({:var, 'Val'}, def__, aligned)

      r_typedef(name: type) ->
        [{:apply, {:local, enc_func(type), type}, [{:var, 'Val'}]}]

      r_Externalvaluereference(module: mod, value: value) ->
        case :asn1_db.dbget(mod, value) do
          r_typedef(typespec: r_Object(def: def__)) ->
            {:object, _, fields} = def__
            [nextField | restFieldNames] = restFieldNames0
            {^nextField, typedef} = :lists.keyfind(nextField, 1, fields)
            enc_obj(erule, typedef, typeName, restFieldNames, aligned)
        end

      r_Externaltypereference(module: mod, type: type) ->
        func = enc_func(type)

        case :erlang.get(:currmod) do
          ^mod ->
            [{:apply, {:local, func, obj}, [{:var, 'Val'}]}]

          _ ->
            [{:apply, {mod, func, obj}, [{:var, 'Val'}]}]
        end
    end
  end

  defp gen_enc_obj(erules, name, typename, type) do
    emit([{:asis, name}, '(Val) ->', :nl])
    innerType = :asn1ct_gen.get_inner(r_type(type, :def))
    :asn1ct_gen.gen_encode_constructed(erules, typename, innerType, type)
  end

  defp gen_dec_components_call(erule, topType, {root, extList}, decInfObj, ext, numberOfOptionals) do
    gen_dec_components_call(
      erule,
      topType,
      {root, extList, []},
      decInfObj,
      ext,
      numberOfOptionals
    )
  end

  defp gen_dec_components_call(
         gen,
         topType,
         {root1, extList, root2} = cL,
         decInfObj,
         ext,
         numberOfOptionals
       ) do
    optTable = create_optionality_table(root1 ++ root2)

    init =
      {:ignore,
       fn _ ->
         {[], []}
       end}

    {emitRoot, tpos} =
      gen_dec_comp_calls(
        root1 ++ root2,
        gen,
        topType,
        optTable,
        decInfObj,
        :noext,
        numberOfOptionals,
        1,
        []
      )

    emitGetExt = gen_dec_get_extension(gen)
    {:extgrouppos, extGroupPosLen} = extgroup_pos_and_length(cL)

    newExtList =
      wrap_extensionAdditionGroups(
        extList,
        extGroupPosLen
      )

    {emitExts, _} =
      gen_dec_comp_calls(
        newExtList,
        gen,
        topType,
        optTable,
        decInfObj,
        ext,
        numberOfOptionals,
        tpos,
        []
      )

    numExtsToSkip = ext_length(extList)

    finish = fn st ->
      emit([{:next, :bytes}, '= '])

      mod =
        case gen do
          r_gen(erule: :per, aligned: false) ->
            :uper

          r_gen(erule: :per, aligned: true) ->
            :per
        end

      :asn1ct_func.call(mod, :skipextensions, [{:curr, :bytes}, numExtsToSkip + 1, 'Extensions'])
      :asn1ct_name.new(:bytes)
      st
    end

    [init] ++
      emitRoot ++
      [
        emitGetExt
        | emitExts
      ] ++ [finish]
  end

  defp gen_dec_components_call(erule, topType, compList, decInfObj, ext, numberOfOptionals) do
    optTable = create_optionality_table(compList)

    init =
      {:ignore,
       fn _ ->
         {[], []}
       end}

    {cs, _} =
      gen_dec_comp_calls(
        compList,
        erule,
        topType,
        optTable,
        decInfObj,
        ext,
        numberOfOptionals,
        1,
        []
      )

    [init | cs]
  end

  defp gen_dec_get_extension(erule) do
    imm0 = :asn1ct_imm.per_dec_extension_map(is_aligned(erule))

    e = fn imm, st ->
      emit([
        :nl,
        '%% Extensions',
        :nl,
        '{Extensions,',
        {:next, :bytes},
        '} = ',
        'case Ext of',
        :nl,
        '0 -> {<<>>,',
        {:curr, :bytes},
        '};',
        :nl,
        '1 ->',
        :nl
      ])

      bytesVar = :asn1ct_gen.mk_var(:asn1ct_name.curr(:bytes))
      {dst, dstBuf} = :asn1ct_imm.dec_slim_cg(imm, bytesVar)
      emit([:com, :nl, '{', dst, ',', dstBuf, '}', :nl, 'end'])
      :asn1ct_name.new(:bytes)
      st
    end

    {:imm, imm0, e}
  end

  defp gen_dec_comp_calls(
         [c | cs],
         erule,
         topType,
         optTable,
         decInfObj,
         ext,
         numberOfOptionals,
         tpos,
         acc
       ) do
    l = gen_dec_comp_call(c, erule, topType, tpos, optTable, decInfObj, ext, numberOfOptionals)

    gen_dec_comp_calls(
      cs,
      erule,
      topType,
      optTable,
      decInfObj,
      ext,
      numberOfOptionals,
      tpos + 1,
      [l | acc]
    )
  end

  defp gen_dec_comp_calls([], _, _, _, _, _, _, tpos, acc) do
    {:lists.append(:lists.reverse(acc)), tpos}
  end

  defp gen_dec_comp_call(comp, gen, topType, tpos, optTable, decInfObj, ext, numberOfOptionals) do
    r_ComponentType(name: cname, typespec: type, prop: prop, textual_order: textPos) = comp

    pos =
      case ext do
        :noext ->
          tpos

        {:ext, epos, _Enum} ->
          tpos - epos + 1
      end

    innerType = :asn1ct_gen.get_inner(r_type(type, :def))
    commentString = attribute_comment(innerType, textPos, cname)

    comment = fn st ->
      emit([:nl, '%% ', commentString, :nl])
      st
    end

    preamble =
      case {innerType, is_mandatory_predef_tab_c(ext, prop, decInfObj)} do
        {{:typefield, _}, true} ->
          fn st ->
            :asn1ct_name.new(:term)
            :asn1ct_name.new(:tmpterm)
            emit(['{', {:curr, :tmpterm}, ', ', {:next, :bytes}, '} = '])
            st
          end

        _ ->
          case type do
            r_type(def: r_SEQUENCE(extaddgroup: groupNum, components: compList))
            when is_integer(groupNum) ->
              dec_match_extadd_fun(gen, compList)

            _ ->
              fn st ->
                :asn1ct_name.new(:term)
                emit(['{', {:curr, :term}])
                emit([',', {:next, :bytes}, '} = '])
                st
              end
          end
      end

    {pre, post} =
      comp_call_pre_post(gen, ext, prop, pos, type, textPos, optTable, numberOfOptionals, ext)

    lines = gen_dec_seq_line_imm(gen, topType, comp, tpos, decInfObj, ext)

    advBuffer =
      {:ignore,
       fn st ->
         :asn1ct_name.new(:bytes)
         st
       end}

    [
      {:group,
       [{:safe, comment}, {:safe, preamble}] ++ pre ++ lines ++ post ++ [{:safe, advBuffer}]}
    ]
  end

  defp dec_match_extadd_fun(r_gen(pack: :record), compList) do
    fn st ->
      emit(['{{_,'])
      emit_extaddgroupTerms(:term, compList)
      emit(['}'])
      emit([',', {:next, :bytes}, '} = '])
      st
    end
  end

  defp dec_match_extadd_fun(r_gen(pack: :map), _CompList) do
    fn st ->
      :asn1ct_name.new(:map)
      emit(['{', {:curr, :map}, ',', {:next, :bytes}, '} = '])
      st
    end
  end

  defp comp_call_pre_post(_Gen, :noext, :mandatory, _, _, _, _, _, _) do
    {[], []}
  end

  defp comp_call_pre_post(_Gen, :noext, prop, _, type, textPos, optTable, numOptionals, ext) do
    optPos = get_optionality_pos(textPos, optTable)

    element =
      case numOptionals - optPos do
        0 ->
          'Opt band 1'

        shift ->
          :lists.concat(['(Opt bsr ', shift, ') band 1'])
      end

    {[
       fn st ->
         emit(['case ', element, ' of', :nl, '1 ->', :nl])
         st
       end
     ],
     [
       fn st ->
         emit([';', :nl, '0 ->', :nl, '{'])
         gen_dec_component_no_val(ext, type, prop)
         emit([',', {:curr, :bytes}, '}', :nl, 'end'])
         st
       end
     ]}
  end

  defp comp_call_pre_post(gen, {:ext, _, _}, prop, pos, type, _, _, _, ext) do
    {[
       fn st ->
         emit(['case Extensions of', :nl, '  <<_:', pos - 1, ',1:1,_/bitstring>> ->', :nl])
         st
       end
     ], [extadd_group_fun(gen, prop, type, ext)]}
  end

  defp extadd_group_fun(r_gen(pack: :record), prop, type, ext) do
    fn st ->
      emit([';', :nl, '_  ->', :nl, '{'])

      case type do
        r_type(
          def:
            r_SEQUENCE(
              extaddgroup: number2,
              components: extGroupCompList2
            )
        )
        when is_integer(number2) ->
          emit('{extAddGroup,')
          gen_dec_extaddGroup_no_val(ext, type, extGroupCompList2)
          emit('}')

        _ ->
          gen_dec_component_no_val(ext, type, prop)
      end

      emit([',', {:curr, :bytes}, '}', :nl, 'end'])
      st
    end
  end

  defp extadd_group_fun(r_gen(pack: :map), prop, type, ext) do
    fn st ->
      emit([';', :nl, '_  ->', :nl, '{'])

      case type do
        r_type(def: r_SEQUENCE(extaddgroup: number2, components: comp))
        when is_integer(number2) ->
          dec_map_extaddgroup_no_val(ext, type, comp)

        _ ->
          gen_dec_component_no_val(ext, type, prop)
      end

      emit([',', {:curr, :bytes}, '}', :nl, 'end'])
      st
    end
  end

  defp is_mandatory_predef_tab_c(:noext, :mandatory, {'got objfun through args', 'ObjFun'}) do
    true
  end

  defp is_mandatory_predef_tab_c(_, _, {'got objfun through args', 'ObjFun'}) do
    false
  end

  defp is_mandatory_predef_tab_c(_, _, _) do
    true
  end

  defp gen_dec_extaddGroup_no_val(ext, type, [r_ComponentType(prop: prop)]) do
    gen_dec_component_no_val(ext, type, prop)
    :ok
  end

  defp gen_dec_extaddGroup_no_val(ext, type, [r_ComponentType(prop: prop) | rest]) do
    gen_dec_component_no_val(ext, type, prop)
    emit(',')
    gen_dec_extaddGroup_no_val(ext, type, rest)
  end

  defp gen_dec_extaddGroup_no_val(_, _, []) do
    :ok
  end

  defp gen_dec_component_no_val(_, type, {:DEFAULT, defVal0}) do
    defVal = :asn1ct_gen.conform_value(type, defVal0)
    emit([{:asis, defVal}])
  end

  defp gen_dec_component_no_val(_, _, :OPTIONAL) do
    emit(['asn1_NOVALUE'])
  end

  defp gen_dec_component_no_val({:ext, _, _}, _, :mandatory) do
    emit(['asn1_NOVALUE'])
  end

  defp dec_map_extaddgroup_no_val(ext, type, comp) do
    l0 =
      for r_ComponentType(name: n, prop: p) <- comp do
        dec_map_extaddgroup_no_val_1(n, p, ext, type)
      end

    l =
      for e <- l0, e !== [] do
        e
      end

    emit(['\#{', :lists.join(',', l), '}'])
  end

  defp dec_map_extaddgroup_no_val_1(name, {:DEFAULT, defVal0}, _Ext, type) do
    defVal = :asn1ct_gen.conform_value(type, defVal0)
    [name, '=>', {:asis, defVal}]
  end

  defp dec_map_extaddgroup_no_val_1(_Name, :OPTIONAL, _, _) do
    []
  end

  defp dec_map_extaddgroup_no_val_1(_Name, :mandatory, {:ext, _, _}, _) do
    []
  end

  defp gen_dec_choice_line(erule, topType, comp, pre) do
    imm0 = gen_dec_line_imm(erule, topType, comp, false, pre)

    init =
      {:ignore,
       fn _ ->
         {[], []}
       end}

    imm = [{:group, [init | imm0]}]
    emit_gen_dec_imm(imm)
  end

  defp gen_dec_seq_line_imm(erule, topType, comp, pos, decInfObj, ext) do
    pre = gen_dec_line_open_type(erule, ext, pos)
    gen_dec_line_imm(erule, topType, comp, decInfObj, pre)
  end

  defp gen_dec_line_imm(erule, topType, comp, decInfObj, pre) do
    r_ComponentType(name: cname, typespec: type) = comp

    atype =
      case type do
        r_type(def: r_ObjectClassFieldType(type: innerType)) ->
          innerType

        _ ->
          :asn1ct_gen.get_inner(r_type(type, :def))
      end

    decode = gen_dec_line_special(erule, atype, topType, comp, decInfObj)

    post = fn {saveBytes, finish} ->
      {accTerm, accBytes} = finish.()
      r_ComponentType(name: ^cname) = comp

      case decInfObj do
        {^cname, objSet} ->
          objSetRef =
            case objSet do
              {:deep, oSName, _, _} ->
                oSName

              _ ->
                objSet
            end

          {accTerm ++ [{objSetRef, cname, :asn1ct_gen.mk_var(:asn1ct_name.curr(:term))}],
           accBytes ++ saveBytes}

        _ ->
          {accTerm, accBytes ++ saveBytes}
      end
    end

    [pre, decode, {:safe, post}]
  end

  defp gen_dec_line_open_type(erule, {:ext, ep, _}, pos) when pos >= ep do
    imm = :asn1ct_imm.per_dec_open_type(is_aligned(erule))

    {:safe,
     fn st ->
       emit(['begin', :nl])
       bytesVar = :asn1ct_gen.mk_var(:asn1ct_name.curr(:bytes))
       {dst, dstBuf} = :asn1ct_imm.dec_slim_cg(imm, bytesVar)
       emit([',', :nl, '{TmpValx', pos, ',_} = '])

       {dst,
        fn ->
          emit([',', :nl, '{TmpValx', pos, ',', dstBuf, '}', :nl, 'end'])
          st
        end}
     end}
  end

  defp gen_dec_line_open_type(_, _, _) do
    {:safe,
     fn st ->
       {:asn1ct_gen.mk_var(:asn1ct_name.curr(:bytes)),
        fn ->
          st
        end}
     end}
  end

  defp gen_dec_line_special(erule, {:typefield, _}, _TopType, comp, decInfObj) do
    r_ComponentType(name: cname, typespec: type, prop: prop) = comp

    fn {_BytesVar, prevSt} ->
      case decInfObj do
        false ->
          {name, restFieldNames} = r_ObjectClassFieldType(r_type(type, :def), :fieldname)
          imm = :asn1ct_imm.per_dec_open_type(is_aligned(erule))
          bytesVar = :asn1ct_gen.mk_var(:asn1ct_name.curr(:bytes))

          {tmpTerm, tempBuf} =
            :asn1ct_imm.dec_slim_cg(
              imm,
              bytesVar
            )

          emit([:com, :nl])

          r_type(tablecinf: [{:objfun, r_Externaltypereference(module: xmod, type: xtype)}]) =
            type

          gen_dec_open_type(
            erule,
            'ObjFun',
            {xmod, xtype},
            :_,
            {:_, {name, restFieldNames}, :Result, tmpTerm, :mandatory}
          )

          emit([:com, :nl, '{', {:asis, cname}, ',{Result,', tempBuf, '}}'])
          {[], prevSt}

        {'got objfun through args', 'ObjFun'} ->
          cond do
            prop === :mandatory ->
              :ok

            true ->
              :asn1ct_name.new(:tmpterm)
              :asn1ct_name.new(:tmpbytes)
              emit([:nl, '    {', {:curr, :tmpterm}, ', ', {:curr, :tmpbytes}, '} ='])
          end

          {name, restFieldNames} = r_ObjectClassFieldType(r_type(type, :def), :fieldname)
          imm = :asn1ct_imm.per_dec_open_type(is_aligned(erule))
          bytesVar = :asn1ct_gen.mk_var(:asn1ct_name.curr(:bytes))
          :asn1ct_imm.dec_code_gen(imm, bytesVar)
          emit([:com, :nl])

          r_type(tablecinf: [{:objfun, r_Externaltypereference(module: xmod, type: xtype)}]) =
            type

          term = :asn1ct_gen.mk_var(:asn1ct_name.curr(:term))
          tmpTerm = :asn1ct_gen.mk_var(:asn1ct_name.curr(:tmpterm))

          cond do
            prop === :mandatory ->
              gen_dec_open_type(
                erule,
                'ObjFun',
                {xmod, xtype},
                :_,
                {:_, {name, restFieldNames}, term, tmpTerm, prop}
              )

            true ->
              emit(['     {'])

              gen_dec_open_type(
                erule,
                'ObjFun',
                {xmod, xtype},
                :_,
                {:_, {name, restFieldNames}, :_, tmpTerm, prop}
              )

              emit([',', :nl, {:curr, :tmpbytes}, '}'])
          end

          {[], prevSt}

        _ ->
          imm = :asn1ct_imm.per_dec_open_type(is_aligned(erule))
          bytesVar = :asn1ct_gen.mk_var(:asn1ct_name.curr(:bytes))
          :asn1ct_imm.dec_code_gen(imm, bytesVar)
          refedFieldName = r_ObjectClassFieldType(r_type(type, :def), :fieldname)

          {[
             {cname, refedFieldName, :asn1ct_gen.mk_var(:asn1ct_name.curr(:term)),
              :asn1ct_gen.mk_var(:asn1ct_name.curr(:tmpterm)), prop}
           ], prevSt}
      end
    end
  end

  defp gen_dec_line_special(gen, atype, topType, comp, decInfObj) do
    case gen_dec_line_other(gen, atype, topType, comp) do
      fun when is_function(fun, 1) ->
        fn {bytesVar, prevSt} ->
          fun.(bytesVar)
          gen_dec_line_dec_inf(gen, comp, decInfObj)
          {[], prevSt}
        end

      imm0 ->
        {:imm, imm0,
         fn imm, {bytesVar, prevSt} ->
           :asn1ct_imm.dec_code_gen(imm, bytesVar)
           gen_dec_line_dec_inf(gen, comp, decInfObj)
           {[], prevSt}
         end}
    end
  end

  defp gen_dec_line_dec_inf(gen, comp, decInfObj) do
    r_ComponentType(name: cname) = comp

    case decInfObj do
      {^cname, {_, _OSet, _UniqueFName, valIndex}} ->
        term = :asn1ct_gen.mk_var(:asn1ct_name.curr(:term))
        valueMatch = value_match(gen, valIndex, term)
        emit([',', :nl, 'ObjFun = ', valueMatch])

      _ ->
        :ok
    end
  end

  defp gen_dec_line_other(erule, atype, topType, comp) do
    r_ComponentType(name: cname, typespec: type) = comp

    case :asn1ct_gen.type(atype) do
      r_Externaltypereference() = etype ->
        fn bytesVar ->
          :asn1ct_gen_per.gen_dec_external(etype, bytesVar)
        end

      {:primitive, :bif} ->
        :asn1ct_gen_per.gen_dec_imm(erule, type)

      :ASN1_OPEN_TYPE ->
        case r_type(type, :def) do
          r_ObjectClassFieldType(type: openType) ->
            :asn1ct_gen_per.gen_dec_imm(erule, r_type(def: openType))

          _ ->
            :asn1ct_gen_per.gen_dec_imm(erule, type)
        end

      {:constructed, :bif} ->
        newTypename = [cname | topType]
        decFunc = dec_func(:asn1ct_gen.list2name(newTypename))

        case r_type(type, :tablecinf) do
          [{:objfun, _} | _R] ->
            fn bytesVar ->
              emit([{:asis, decFunc}, '(', bytesVar, ', ObjFun)'])
            end

          _ ->
            fn bytesVar ->
              emit([{:asis, decFunc}, '(', bytesVar, ')'])
            end
        end
    end
  end

  defp gen_enc_choice(erule, topType, {root, exts}, ext) do
    constr = choice_constraint(root)

    gen_enc_choices(root, erule, topType, 0, constr, ext) ++
      gen_enc_choices(exts, erule, topType, 0, :ext, ext)
  end

  defp gen_enc_choice(erule, topType, {root, exts, []}, ext) do
    gen_enc_choice(erule, topType, {root, exts}, ext)
  end

  defp gen_enc_choice(erule, topType, root, ext) when is_list(root) do
    constr = choice_constraint(root)
    gen_enc_choices(root, erule, topType, 0, constr, ext)
  end

  defp choice_constraint(l) do
    case length(l) do
      0 ->
        [{:SingleValue, 0}]

      len ->
        [{:ValueRange, {0, len - 1}}]
    end
  end

  defp gen_enc_choices([h | t], erule, topType, pos, constr, ext) do
    r_ComponentType(name: cname, typespec: type) = h
    aligned = is_aligned(erule)

    encObj =
      case :asn1ct_gen.get_constraint(
             r_type(type, :constraint),
             :componentrelation
           ) do
        :no ->
          case r_type(type, :tablecinf) do
            [{:objfun, _} | _] ->
              {'got objfun through args', {:var, 'ObjFun'}}

            _ ->
              false
          end

        _ ->
          {:no_attr, {:var, 'ObjFun'}}
      end

    doExt =
      case constr do
        :ext ->
          ext

        _ ->
          :noext
      end

    tag =
      case {ext, constr} do
        {:noext, _} ->
          :asn1ct_imm.per_enc_integer(pos, constr, aligned)

        {{:ext, _, _}, :ext} ->
          [
            {:put_bits, 1, 1, [1]}
            | :asn1ct_imm.per_enc_small_number(pos, aligned)
          ]

        {{:ext, _, _}, _} ->
          [
            {:put_bits, 0, 1, [1]}
            | :asn1ct_imm.per_enc_integer(pos, constr, aligned)
          ]
      end

    body = gen_enc_line_imm(erule, topType, cname, type, {:var, 'ChoiceVal'}, encObj, doExt)
    imm = tag ++ body
    [{cname, imm} | gen_enc_choices(t, erule, topType, pos + 1, constr, ext)]
  end

  defp gen_enc_choices([], _, _, _, _, _) do
    []
  end

  defp gen_dec_choice(erule, topType, compList, {:ext, _, _} = ext) do
    {rootList, extList} = split_complist(compList)
    emit(['case Bytes of', :nl])

    case rootList do
      [] ->
        :ok

      [_ | _] ->
        emit(['<<0:1,Bytes1/bitstring>> ->', :nl])
        :asn1ct_name.new(:bytes)
        gen_dec_choice1(erule, topType, rootList, :noext)
        emit([';', :nl, :nl])
    end

    emit(['<<1:1,Bytes1/bitstring>> ->', :nl])
    :asn1ct_name.clear()
    :asn1ct_name.new(:bytes)
    :asn1ct_name.new(:bytes)
    gen_dec_choice1(erule, topType, extList, ext)
    emit([:nl, 'end'])
  end

  defp gen_dec_choice(erule, topType, compList, :noext) do
    gen_dec_choice1(erule, topType, compList, :noext)
  end

  defp split_complist({root1, ext, root2}) do
    {root1 ++ root2, ext}
  end

  defp split_complist({_, _} = compList) do
    compList
  end

  defp gen_dec_choice1(erule, topType, compList, :noext = ext) do
    emit_getchoice(erule, compList, ext)
    emit(['case Choice of', :nl])

    pre =
      {:safe,
       fn st ->
         {:asn1ct_gen.mk_var(:asn1ct_name.curr(:bytes)),
          fn ->
            st
          end}
       end}

    gen_dec_choice2(erule, topType, compList, pre)
    emit([:nl, 'end'])
  end

  defp gen_dec_choice1(erule, topType, compList, {:ext, _, _} = ext) do
    emit_getchoice(erule, compList, ext)
    imm = :asn1ct_imm.per_dec_open_type(is_aligned(erule))
    emit(['begin', :nl])
    bytesVar = :asn1ct_gen.mk_var(:asn1ct_name.curr(:bytes))
    {dst, dstBuf} = :asn1ct_imm.dec_slim_cg(imm, bytesVar)
    emit([:nl, 'end,', :nl, 'case Choice of', :nl])

    pre =
      {:safe,
       fn st ->
         emit(['{TmpVal,_} = '])

         {dst,
          fn ->
            emit([',', :nl, '{TmpVal,', dstBuf, '}'])
            st
          end}
       end}

    gen_dec_choice2(erule, topType, compList, pre)

    case compList do
      [] ->
        :ok

      [_ | _] ->
        emit([';', :nl])
    end

    emit(['_ ->', :nl, '{{asn1_ExtAlt,', dst, '},', dstBuf, '}', :nl, 'end'])
  end

  defp emit_getchoice(erule, compList, ext) do
    al = is_aligned(erule)

    imm =
      case {ext, compList} do
        {:noext, [_]} ->
          {:value, 0}

        {:noext, _} ->
          :asn1ct_imm.per_dec_constrained(0, length(compList) - 1, al)

        {{:ext, _, _}, _} ->
          :asn1ct_imm.per_dec_normally_small_number(al)
      end

    emit(['{Choice,', {:curr, :bytes}, '} = ', :nl])
    bytesVar = :asn1ct_gen.mk_var(:asn1ct_name.prev(:bytes))
    :asn1ct_imm.dec_code_gen(imm, bytesVar)
    emit([:com, :nl])
  end

  defp gen_dec_choice2(erule, topType, l, ext) do
    gen_dec_choice2(erule, topType, l, 0, [], ext)
  end

  defp gen_dec_choice2(erule, topType, [h0 | t], pos, sep0, pre) do
    r_ComponentType(name: cname, typespec: type) = h0
    h = r_ComponentType(h0, prop: :mandatory)
    emit([sep0, pos, ' ->', :nl])

    case r_type(type, :def) do
      r_ObjectClassFieldType(type: {:typefield, _}) ->
        emit('{Cname,{Val,NewBytes}} = begin\n')
        gen_dec_choice_line(erule, topType, h, pre)
        emit([:nl, 'end,', :nl, '{{Cname,Val},NewBytes}'])

      _ ->
        emit('{Val,NewBytes} = begin\n')
        gen_dec_choice_line(erule, topType, h, pre)
        emit([:nl, 'end,', :nl, '{{', {:asis, cname}, ',Val},NewBytes}'])
    end

    sep = [';', :nl]
    gen_dec_choice2(erule, topType, t, pos + 1, sep, pre)
  end

  defp gen_dec_choice2(_, _, [], _, _, _) do
    :ok
  end

  defp get_input_vars(val, i, n) do
    l = get_input_vars_1(val, i, n)
    :lists.join(',', l)
  end

  defp get_input_vars_1(_Val, _I, 0) do
    []
  end

  defp get_input_vars_1(val, i, n) do
    [get_input_var(val, i) | get_input_vars_1(val, i + 1, n - 1)]
  end

  defp get_input_var(val, i) do
    :lists.flatten(:io_lib.format('element(~w, ~s)', [i + 1, val]))
  end

  defp emit_extaddgroupTerms(varSeries, [_]) do
    :asn1ct_name.new(varSeries)
    emit({:curr, varSeries})
    :ok
  end

  defp emit_extaddgroupTerms(varSeries, [_ | rest]) do
    :asn1ct_name.new(varSeries)
    emit([{:curr, varSeries}, ','])
    emit_extaddgroupTerms(varSeries, rest)
  end

  defp emit_extaddgroupTerms(_, []) do
    :ok
  end

  defp flat_complist({rl1, el, rl2}) do
    rl1 ++ el ++ rl2
  end

  defp flat_complist({rl, el}) do
    rl ++ el
  end

  defp flat_complist(compList) do
    compList
  end

  defp wrap_extensionAdditionGroups(extCompList, extGroupPosLen) do
    wrap_eags(extCompList, extGroupPosLen, 0, 0)
  end

  defp wrap_eags(
         [{:ExtensionAdditionGroup, _Number} | t0],
         [{actualPos, _, _} | gs],
         _ExtAddGroupDiff,
         extGroupNum
       ) do
    {extGroupCompList,
     [
       :ExtensionAdditionGroupEnd
       | t
     ]} =
      :lists.splitwith(
        fn
          r_ComponentType() ->
            true

          _ ->
            false
        end,
        t0
      )

    name = :erlang.list_to_atom(:lists.concat(['ExtAddGroup', extGroupNum + 1]))

    seq =
      r_type(
        def:
          r_SEQUENCE(
            extaddgroup: extGroupNum + 1,
            components: extGroupCompList
          )
      )

    comp = r_ComponentType(name: name, typespec: seq, textual_order: actualPos, prop: :OPTIONAL)
    [comp | wrap_eags(t, gs, length(extGroupCompList) - 1, extGroupNum + 1)]
  end

  defp wrap_eags(
         [r_ComponentType(textual_order: tord) = h | t],
         extAddGrpLenPos,
         extAddGroupDiff,
         extGroupNum
       )
       when is_integer(tord) do
    comp = r_ComponentType(h, textual_order: tord - extAddGroupDiff)
    [comp | wrap_eags(t, extAddGrpLenPos, extAddGroupDiff, extGroupNum)]
  end

  defp wrap_eags([h | t], extAddGrpLenPos, extAddGroupDiff, extGroupNum) do
    [h | wrap_eags(t, extAddGrpLenPos, extAddGroupDiff, extGroupNum)]
  end

  defp wrap_eags([], _, _, _) do
    []
  end

  defp value_match(r_gen(pack: :record), vIs, value) do
    value_match_rec(vIs, value)
  end

  defp value_match(r_gen(pack: :map), vIs, value) do
    value_match_map(vIs, value)
  end

  defp value_match_rec([], value) do
    value
  end

  defp value_match_rec([{vI, _} | vIs], value0) do
    value = value_match_rec(vIs, value0)
    :lists.concat(['element(', vI, ', ', value, ')'])
  end

  defp value_match_map([], value) do
    value
  end

  defp value_match_map([{_, name} | vIs], value0) do
    value = value_match_map(vIs, value0)
    :lists.concat(['maps:get(', name, ', ', value, ')'])
  end

  defp enc_dig_out_value(_Gen, [], value) do
    {[], value}
  end

  defp enc_dig_out_value(r_gen(pack: :record) = gen, [{n, _} | t], value) do
    {imm0, dst0} = enc_dig_out_value(gen, t, value)
    {imm, dst} = :asn1ct_imm.enc_element(n, dst0)
    {imm0 ++ imm, dst}
  end

  defp enc_dig_out_value(r_gen(pack: :map), [{n, :ASN1_top}], _Value) do
    {[], {:var, :lists.concat(['Input@', n - 1])}}
  end

  defp enc_dig_out_value(r_gen(pack: :map) = gen, [{_, name} | t], value) do
    {imm0, dst0} = enc_dig_out_value(gen, t, value)
    {imm, dst} = :asn1ct_imm.enc_maps_get(name, dst0)
    {imm0 ++ imm, dst}
  end

  defp make_var(base) do
    {:var, :erlang.atom_to_list(:asn1ct_gen.mk_var(:asn1ct_name.curr(base)))}
  end

  defp attribute_comment(innerType, textPos, cname) do
    dispType =
      case innerType do
        r_Externaltypereference(type: t) ->
          t

        iT when is_tuple(iT) ->
          :erlang.element(2, iT)

        _ ->
          innerType
      end

    comment = ['attribute ', cname, '(', textPos, ') with type ', dispType]
    :lists.concat(comment)
  end

  defp dec_func(tname) do
    :erlang.list_to_atom(:lists.concat(['dec_', tname]))
  end
end
