defmodule :m_asn1ct_gen_jer do
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

  def gen_encode_sequence(gen, typename, r_type() = d) do
    {_SeqOrSet, tableConsInfo, compList0} =
      case r_type(d, :def) do
        r_SEQUENCE(tablecinf: tCI, components: cL) ->
          {:SEQUENCE, tCI, cL}

        r_SET(tablecinf: tCI, components: cL) ->
          {:SET, tCI, cL}
      end

    compList = filter_complist(compList0)

    compList1 =
      case compList do
        {rl1, el, rl2} ->
          rl1 ++ el ++ rl2

        {rl, el} ->
          rl ++ el

        _ ->
          compList
      end

    encObj =
      case tableConsInfo do
        r_simpletableattributes(usedclassfield: used, uniqueclassfield: unique)
        when used != unique ->
          false

        r_simpletableattributes(
          objectsetname: objectSetRef,
          c_name: attrN,
          c_index: n,
          usedclassfield: uniqueFieldName,
          uniqueclassfield: uniqueFieldName,
          valueindex: _ValueIndex
        ) ->
          {objSetMod, objSetName} = objectSetRef
          oSDef = :asn1_db.dbget(objSetMod, objSetName)

          case r_ObjectSet(r_typedef(oSDef, :typespec), :gen) do
            true ->
              {attrN, n}

            _ ->
              false
          end

        _ ->
          case r_type(d, :tablecinf) do
            [{:objfun, _} | _] ->
              {'got objfun through args', 'ObjFun'}

            _ ->
              false
          end
      end

    compTypes = gen_enc_comptypes(gen, typename, compList1, 1, encObj, [])
    prefix = :asn1ct_gen.get_record_name_prefix(gen)

    {:sequence, :erlang.list_to_atom(:lists.concat([prefix, :asn1ct_gen.list2name(typename)])),
     length(compList1), compTypes}
  end

  def gen_decode_sequence(_, _, _) do
    :ok
  end

  def gen_encode_set(erules, typename, d) when elem(d, 0) === :type do
    gen_encode_sequence(erules, typename, d)
  end

  def gen_decode_set(_, _, _) do
    :ok
  end

  def gen_encode_sof(erules, typename, innerTypename, d)
      when elem(d, 0) === :type do
    :asn1ct_name.start()
    {_SeqOrSetOf, cont} = r_type(d, :def)

    nameSuffix =
      :asn1ct_gen.constructed_suffix(
        innerTypename,
        r_type(d, :def)
      )

    {:sof, gen_typeinfo(erules, [nameSuffix | typename], cont)}
  end

  def gen_decode_sof(_, _, _, _) do
    :ok
  end

  def gen_encode_choice(erules, typeName, d) when elem(d, 0) === :type do
    {:CHOICE, compList} = r_type(d, :def)

    compList1 =
      case compList do
        {rl1, el, rl2} ->
          rl1 ++ el ++ rl2

        {rl, el} ->
          rl ++ el

        _ ->
          compList
      end

    {:choice,
     :maps.from_list(
       for {altName, altType, _OptOrMand} <-
             gen_enc_comptypes(erules, typeName, compList1, 0, 0, []) do
         {altName, altType}
       end
     )}
  end

  def gen_decode_choice(_, _, _) do
    :ok
  end

  defp gen_enc_comptypes(
         erules,
         topType,
         [r_ComponentType(name: cname, typespec: type, prop: prop) | rest],
         pos,
         encObj,
         acc
       ) do
    typeInfo = gen_enc_line(erules, topType, cname, type, 'Dummy', 3, prop, encObj)

    gen_enc_comptypes(erules, topType, rest, pos, encObj, [
      {:erlang.atom_to_binary(cname, :utf8), typeInfo, prop}
      | acc
    ])
  end

  defp gen_enc_comptypes(_, _, [], _, _, acc) do
    :lists.reverse(acc)
  end

  defp gen_enc_classtypes(erules, topType, [{tName, r_typedef(typespec: tSpec)} | rest], acc) do
    typeInfo = gen_enc_line(erules, topType, tName, tSpec, 'Dummy', 3, :mandatory, false)
    gen_enc_classtypes(erules, topType, rest, [{tName, typeInfo} | acc])
  end

  defp gen_enc_classtypes(_, _, [], acc) do
    :lists.reverse(acc)
  end

  defp gen_enc_line(
         erules,
         topType,
         cname,
         type = r_type(constraint: c, def: r_ObjectClassFieldType(type: {:typefield, _})),
         element,
         indent,
         optOrMand = :mandatory,
         encObj
       )
       when is_list(element) do
    case :asn1ct_gen.get_constraint(
           c,
           :componentrelation
         ) do
      {:componentrelation, _, _} ->
        gen_enc_line(
          erules,
          topType,
          cname,
          type,
          element,
          indent,
          optOrMand,
          ['{', {:curr, :tmpBytes}, ',_} = '],
          encObj
        )

      _ ->
        gen_enc_line(
          erules,
          topType,
          cname,
          type,
          element,
          indent,
          optOrMand,
          ['{', {:curr, :encBytes}, ',', {:curr, :encLen}, '} = '],
          encObj
        )
    end
  end

  defp gen_enc_line(erules, topType, cname, type, element, indent, optOrMand, encObj)
       when is_list(element) do
    gen_enc_line(
      erules,
      topType,
      cname,
      type,
      element,
      indent,
      optOrMand,
      [{:curr, :encV}, ' = '],
      encObj
    )
  end

  defp gen_enc_line(erules, topType, cname, type, element, _Indent, _OptOrMand, _Assign, encObj)
       when is_list(element) do
    innerType =
      case type do
        r_type(def: def__) ->
          :asn1ct_gen.get_inner(def__)

        r_ObjectSet(class: extRef) ->
          :asn1ct_gen.get_inner(extRef)
      end

    whatKind = :asn1ct_gen.type(innerType)

    cR =
      case type do
        r_type(constraint: constraint) ->
          :asn1ct_gen.get_constraint(
            constraint,
            :componentrelation
          )

        _ ->
          []
      end

    typeInfo =
      case {type, cR} do
        {r_type(
           def:
             r_ObjectClassFieldType(
               type: {:typefield, _},
               fieldname: refedFieldName
             )
         ), {:componentrelation, _, _}} ->
          {name, _RestFieldNames} = refedFieldName
          true = is_atom(name)
          {:ObjClassFieldType, encObj, cR}

        _ ->
          case whatKind do
            {:primitive, :bif} ->
              gen_encode_prim(:jer, type, element)

            :ASN1_OPEN_TYPE ->
              case r_type(type, :def) do
                r_ObjectClassFieldType() ->
                  gen_encode_prim(:jer, r_type(def: :ASN1_OPEN_TYPE), element)

                _ ->
                  gen_encode_prim(:jer, type, element)
              end

            {:constructed, :bif} ->
              typename = [cname | topType]
              gen_encode_constructed(erules, typename, innerType, type)

            r_Externaltypereference(module: mod, type: eType) ->
              {:typeinfo, {mod, typeinfo_func(eType)}}
          end
      end

    typeInfo
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

  defp filter_complist({root, ext}) do
    {root, filter_complist(ext)}
  end

  defp filter_complist({root1, ext, root2}) do
    {root1, filter_complist(ext), root2}
  end

  def gen_encode_constructed(erules, typename, innerType, d)
      when elem(d, 0) === :type do
    case innerType do
      :SET ->
        gen_encode_set(erules, typename, d)

      :SEQUENCE ->
        gen_encode_sequence(erules, typename, d)

      :CHOICE ->
        gen_encode_choice(erules, typename, d)

      :"SEQUENCE OF" ->
        gen_encode_sof(erules, typename, innerType, d)

      :"SET OF" ->
        gen_encode_sof(erules, typename, innerType, d)
    end
  end

  def dialyzer_suppressions(_) do
    case :asn1ct.use_legacy_types() do
      false ->
        :ok

      true ->
        suppress({:ber, :encode_bit_string, 4})
    end

    suppress({:ber, :decode_selective, 2})
    emit(['    ok.', :nl])
  end

  defp suppress({m, f, a} = mFA) do
    case :asn1ct_func.is_used(mFA) do
      false ->
        :ok

      true ->
        args =
          for i <- :lists.seq(1, a) do
            :lists.concat(['element(', i, ', Arg)'])
          end

        emit(['    ', {:call, m, f, args}, :com, :nl])
    end
  end

  def gen_encode(erules, r_typedef() = d) do
    gen_encode_user(erules, d, true)
  end

  def gen_encode(erules, typename, type)
      when elem(type, 0) === :type do
    innerType = :asn1ct_gen.get_inner(r_type(type, :def))

    objFun =
      case :lists.keysearch(:objfun, 1, r_type(type, :tablecinf)) do
        {:value, {_, _Name}} ->
          ', ObjFun'

        false ->
          ''
      end

    case :asn1ct_gen.type(innerType) do
      {:constructed, :bif} ->
        func = {:asis, enc_func(:asn1ct_gen.list2name(typename))}

        emit([
          :nl,
          :nl,
          :nl,
          '%%================================',
          :nl,
          '%%  ',
          :asn1ct_gen.list2name(typename),
          :nl,
          '%%================================',
          :nl,
          func,
          '(Val',
          objFun,
          ') ->',
          :nl,
          '   '
        ])

        typeInfo = gen_encode_constructed(erules, typename, innerType, type)
        emit([{:asis, typeInfo}, '.', :nl])

      _ ->
        true
    end
  end

  def gen_encode(erules, tname, r_ComponentType(name: cname, typespec: type)) do
    newTname = [cname | tname]
    newType = r_type(type, tag: [])
    gen_encode(erules, newTname, newType)
  end

  defp gen_encode_user(erules, r_typedef() = d, _Wrapper) do
    typename = [r_typedef(d, :name)]
    type = r_typedef(d, :typespec)
    innerType = :asn1ct_gen.get_inner(r_type(type, :def))
    emit([:nl, :nl, '%%================================'])
    emit([:nl, '%%  ', typename])
    emit([:nl, '%%================================', :nl])
    funcName = {:asis, typeinfo_func(:asn1ct_gen.list2name(typename))}
    emit([funcName, '() ->', :nl])
    currentMod = :erlang.get(:currmod)

    typeInfo =
      case :asn1ct_gen.type(innerType) do
        {:constructed, :bif} ->
          gen_encode_constructed(erules, typename, innerType, type)

        {:primitive, :bif} ->
          gen_encode_prim(:jer, type, 'Val')

        r_Externaltypereference(module: ^currentMod, type: etype) ->
          {:typeinfo, {currentMod, typeinfo_func(etype)}}

        r_Externaltypereference(module: emod, type: etype) ->
          {:typeinfo, {emod, typeinfo_func(etype)}}

        :ASN1_OPEN_TYPE ->
          gen_encode_prim(:jer, r_type(type, def: :ASN1_OPEN_TYPE), 'Val')
      end

    emit([{:asis, typeInfo}, '.', :nl, :nl])
  end

  defp gen_typeinfo(erules, typename, type) do
    innerType = :asn1ct_gen.get_inner(r_type(type, :def))
    currentMod = :erlang.get(:currmod)

    case :asn1ct_gen.type(innerType) do
      {:constructed, :bif} ->
        gen_encode_constructed(erules, typename, innerType, type)

      {:primitive, :bif} ->
        gen_encode_prim(:jer, type, 'Val')

      r_Externaltypereference(module: ^currentMod, type: etype) ->
        {:typeinfo, {currentMod, typeinfo_func(etype)}}

      r_Externaltypereference(module: emod, type: etype) ->
        {:typeinfo, {emod, typeinfo_func(etype)}}

      :ASN1_OPEN_TYPE ->
        gen_encode_prim(:jer, r_type(type, def: :ASN1_OPEN_TYPE), 'Val')
    end
  end

  def gen_encode_prim(_Erules, r_type() = d, _Value) do
    bitStringConstraint = get_size_constraint(r_type(d, :constraint))
    intConstr = int_constr(r_type(d, :constraint))
    :asn1ct_name.new(:enumval)

    type =
      case r_type(d, :def) do
        :"OCTET STRING" ->
          maybe_legacy_octet_string()

        :UTF8String ->
          :string

        :ObjectDescriptor ->
          :string

        :NumericString ->
          :string

        :TeletexString ->
          :string

        :T61String ->
          :string

        :VideotexString ->
          :string

        :GraphicString ->
          :string

        :VisibleString ->
          :string

        :GeneralString ->
          :string

        :PrintableString ->
          :string

        :IA5String ->
          :string

        :UTCTime ->
          :string

        :GeneralizedTime ->
          :string

        b1 = :"BIT STRING" ->
          maybe_legacy_bit_string(b1, bitStringConstraint)

        b2 = {:"BIT STRING", _NNL} ->
          maybe_legacy_bit_string(b2, bitStringConstraint)

        {:INTEGER, nNL} ->
          {:INTEGER_NNL, nNL}

        {:ENUMERATED, {nNL, ext}} ->
          {:ENUMERATED_EXT, :maps.from_list(nNL ++ ext)}

        {:ENUMERATED, nNL} ->
          {:ENUMERATED, :maps.from_list(nNL)}

        other ->
          other
      end

    case intConstr do
      [] ->
        type

      _ ->
        {type, intConstr}
    end
  end

  defp maybe_legacy_octet_string() do
    case :asn1ct.use_legacy_types() do
      true ->
        :legacy_octet_string

      false ->
        :octet_string
    end
  end

  defp maybe_legacy_bit_string(bitStrType, sizeConstraint) do
    type =
      case :asn1ct.get_bit_string_format() do
        :bitstring ->
          :bit_string

        :compact ->
          :compact_bit_string

        :legacy ->
          :legacy_bit_string
      end

    type1 =
      case bitStrType do
        {:"BIT STRING", []} ->
          type

        :"BIT STRING" ->
          type

        {:"BIT STRING", nNL} ->
          {:erlang.list_to_atom(:lists.concat([type, '_nnl'])), nNL}
      end

    case sizeConstraint do
      s when is_integer(s) ->
        {type1, s}

      _ ->
        type1
    end
  end

  def gen_decode(_, _) do
    :ok
  end

  def gen_inc_decode(_Erules, _Type) do
    :ok
  end

  def gen_decode_selected(_Erules, _Type, _FuncName) do
    :ok
  end

  def gen_decode(_, _, _) do
    :ok
  end

  def gen_dec_prim(_Att, _BytesVar) do
    :ok
  end

  defp int_constr(c) do
    case :asn1ct_imm.effective_constraint(:integer, c) do
      [{_, []}] ->
        []

      [{:ValueRange, {:MIN, _}}] ->
        []

      [{:ValueRange, {_, _} = range}] ->
        range

      [{:SingleValue, sv}] ->
        sv

      [] ->
        []
    end
  end

  def gen_obj_code(_Erules, _Module, _Obj) do
    :ok
  end

  def gen_objectset_code(erules, objSet) do
    objSetName = r_typedef(objSet, :name)
    def__ = r_typedef(objSet, :typespec)
    set = r_ObjectSet(def__, :set)

    emit([
      :nl,
      :nl,
      :nl,
      '%%================================',
      :nl,
      '%%  ',
      objSetName,
      :nl,
      '%%================================',
      :nl
    ])

    funcName = {:asis, typeinfo_func(:asn1ct_gen.list2name([objSetName]))}

    selectValMap =
      :maps.from_list(
        for {_, selectVal, typeList} <- set do
          {selectVal,
           :maps.from_list(
             gen_enc_classtypes(
               erules,
               objSetName,
               for tNameType = {_TypeName, r_typedef()} <- typeList do
                 tNameType
               end,
               []
             )
           )}
        end
      )

    emit([funcName, '() ->', :nl])
    emit([{:asis, selectValMap}, '.', :nl])
  end

  defp get_size_constraint(c) do
    case :lists.keyfind(:SizeConstraint, 1, c) do
      false ->
        []

      {_, {_, []}} ->
        []

      {_, {sv, sv}} ->
        sv

      {_, {_, _} = tc} ->
        tc
    end
  end

  def extaddgroup2sequence(extList) when is_list(extList) do
    :lists.filter(
      fn
        r_ExtensionAdditionGroup() ->
          false

        :ExtensionAdditionGroupEnd ->
          false

        _ ->
          true
      end,
      extList
    )
  end

  defp typeinfo_func(tname) do
    :erlang.list_to_atom(:lists.concat(['typeinfo_', tname]))
  end

  defp enc_func(tname) do
    :erlang.list_to_atom(:lists.concat(['enc_', tname]))
  end
end
