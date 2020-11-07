defmodule :m_asn1ct_gen_ber_bin_v2 do
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
    gen_encode_user(erules, r_typedef() = d, true)
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
          '(Val, TagIn',
          objFun,
          ') ->',
          :nl,
          '   '
        ])

        :asn1ct_gen.gen_encode_constructed(erules, typename, innerType, type)

      _ ->
        true
    end
  end

  def gen_encode(erules, tname, r_ComponentType(name: cname, typespec: type)) do
    newTname = [cname | tname]
    newType = r_type(type, tag: [])
    gen_encode(erules, newTname, newType)
  end

  defp gen_encode_user(erules, r_typedef() = d, wrapper) do
    typename = [r_typedef(d, :name)]
    type = r_typedef(d, :typespec)
    innerType = :asn1ct_gen.get_inner(r_type(type, :def))
    emit([:nl, :nl, '%%================================'])
    emit([:nl, '%%  ', typename])
    emit([:nl, '%%================================', :nl])
    funcName = {:asis, enc_func(:asn1ct_gen.list2name(typename))}

    case wrapper do
      true ->
        oTag = r_type(type, :tag)

        tag0 =
          for r_tag(class: class, form: form, number: number) <- oTag do
            encode_tag_val(decode_class(class), form, number)
          end

        tag = :lists.reverse(tag0)

        emit([funcName, '(Val) ->', :nl, '    ', funcName, '(Val, ', {:asis, tag}, ').', :nl, :nl])

      false ->
        :ok
    end

    emit([funcName, '(Val, TagIn) ->', :nl])
    currentMod = :erlang.get(:currmod)

    case :asn1ct_gen.type(innerType) do
      {:constructed, :bif} ->
        :asn1ct_gen.gen_encode_constructed(erules, typename, innerType, d)

      {:primitive, :bif} ->
        gen_encode_prim(:ber, type, 'TagIn', 'Val')
        emit(['.', :nl])

      r_Externaltypereference(module: ^currentMod, type: etype) ->
        emit(['   ', {:asis, enc_func(etype)}, '(Val, TagIn).', :nl])

      r_Externaltypereference(module: emod, type: etype) ->
        emit(['   ', {:asis, emod}, ':', {:asis, enc_func(etype)}, '(Val, TagIn).', :nl])

      :ASN1_OPEN_TYPE ->
        emit(['%% OPEN TYPE', :nl])
        gen_encode_prim(:ber, r_type(type, def: :ASN1_OPEN_TYPE), 'TagIn', 'Val')
        emit(['.', :nl])
    end
  end

  def gen_encode_prim(_Erules, r_type() = d, doTag, value) do
    bitStringConstraint = get_size_constraint(r_type(d, :constraint))

    maxBitStrSize =
      case bitStringConstraint do
        [] ->
          :none

        {_, :MAX} ->
          :none

        {_, max} ->
          max

        max when is_integer(max) ->
          max
      end

    :asn1ct_name.new(:enumval)

    type =
      case r_type(d, :def) do
        :"OCTET STRING" ->
          :restricted_string

        :ObjectDescriptor ->
          :restricted_string

        :NumericString ->
          :restricted_string

        :TeletexString ->
          :restricted_string

        :T61String ->
          :restricted_string

        :VideotexString ->
          :restricted_string

        :GraphicString ->
          :restricted_string

        :VisibleString ->
          :restricted_string

        :GeneralString ->
          :restricted_string

        :PrintableString ->
          :restricted_string

        :IA5String ->
          :restricted_string

        :UTCTime ->
          :restricted_string

        :GeneralizedTime ->
          :restricted_string

        other ->
          other
      end

    case type do
      :restricted_string ->
        call(:encode_restricted_string, [value, doTag])

      :BOOLEAN ->
        call(:encode_boolean, [value, doTag])

      :INTEGER ->
        call(:encode_integer, [value, doTag])

      {:INTEGER, namedNumberList} ->
        call(
          :encode_integer,
          [value, {:asis, namedNumberList}, doTag]
        )

      {:ENUMERATED, namedNumberList = {_, _}} ->
        emit(['case ', value, ' of', :nl])
        emit_enc_enumerated_cases(namedNumberList, doTag)

      {:ENUMERATED, namedNumberList} ->
        emit(['case ', value, ' of', :nl])
        emit_enc_enumerated_cases(namedNumberList, doTag)

      :REAL ->
        :asn1ct_name.new(:realval)
        :asn1ct_name.new(:realsize)

        emit([
          'begin',
          :nl,
          {:curr, :realval},
          ' = ',
          {:call, :real_common, :ber_encode_real, [value]},
          :com,
          :nl,
          {:curr, :realsize},
          ' = ',
          {:call, :erlang, :byte_size, [{:curr, :realval}]},
          :com,
          :nl,
          {:call, :ber, :encode_tags, [doTag, {:curr, :realval}, {:curr, :realsize}]},
          :nl,
          'end'
        ])

      {:"BIT STRING", []} ->
        case :asn1ct.use_legacy_types() do
          false when maxBitStrSize === :none ->
            call(:encode_unnamed_bit_string, [value, doTag])

          false ->
            call(
              :encode_unnamed_bit_string,
              [{:asis, maxBitStrSize}, value, doTag]
            )

          true ->
            call(
              :encode_bit_string,
              [{:asis, bitStringConstraint}, value, {:asis, []}, doTag]
            )
        end

      {:"BIT STRING", namedNumberList} ->
        case :asn1ct.use_legacy_types() do
          false when maxBitStrSize === :none ->
            call(
              :encode_named_bit_string,
              [value, {:asis, namedNumberList}, doTag]
            )

          false ->
            call(
              :encode_named_bit_string,
              [{:asis, maxBitStrSize}, value, {:asis, namedNumberList}, doTag]
            )

          true ->
            call(
              :encode_bit_string,
              [{:asis, bitStringConstraint}, value, {:asis, namedNumberList}, doTag]
            )
        end

      :NULL ->
        call(:encode_null, [value, doTag])

      :"OBJECT IDENTIFIER" ->
        call(:encode_object_identifier, [value, doTag])

      :"RELATIVE-OID" ->
        call(:encode_relative_oid, [value, doTag])

      :UniversalString ->
        call(:encode_universal_string, [value, doTag])

      :UTF8String ->
        call(:encode_UTF8_string, [value, doTag])

      :BMPString ->
        call(:encode_BMP_string, [value, doTag])

      :ASN1_OPEN_TYPE ->
        call(:encode_open_type, [value, doTag])
    end
  end

  defp emit_enc_enumerated_cases({l1, l2}, tags) do
    emit_enc_enumerated_cases(l1 ++ l2, tags, :ext)
  end

  defp emit_enc_enumerated_cases(l, tags) do
    emit_enc_enumerated_cases(l, tags, :noext)
  end

  defp emit_enc_enumerated_cases([{enumName, enumVal} | t], tags, ext) do
    {bytes, len} = encode_integer(enumVal)

    emit([
      {:asis, enumName},
      ' -> ',
      {:call, :ber, :encode_tags, [tags, {:asis, bytes}, len]},
      ';',
      :nl
    ])

    emit_enc_enumerated_cases(t, tags, ext)
  end

  defp emit_enc_enumerated_cases([], _Tags, _Ext) do
    emit([
      {:curr, :enumval},
      ' -> exit({error,{asn1, {enumerated_not_in_range,',
      {:curr, :enumval},
      '}}})'
    ])

    emit([:nl, 'end'])
  end

  defp encode_integer(val) do
    bytes =
      cond do
        val >= 0 ->
          encode_integer_pos(val, [])

        true ->
          encode_integer_neg(val, [])
      end

    {bytes, length(bytes)}
  end

  defp encode_integer_pos(0, [b | _Acc] = l) when b < 128 do
    l
  end

  defp encode_integer_pos(n, acc) do
    encode_integer_pos(n >>> 8, [n &&& 255 | acc])
  end

  defp encode_integer_neg(-1, [b1 | _T] = l) when b1 > 127 do
    l
  end

  defp encode_integer_neg(n, acc) do
    encode_integer_neg(n >>> 8, [n &&& 255 | acc])
  end

  def gen_decode(erules, type) when elem(type, 0) === :typedef do
    def__ = r_typedef(type, :typespec)
    innerTag = r_type(def__, :tag)

    tag =
      for x <- innerTag do
        decode_class(r_tag(x, :class)) <<< (10 + r_tag(x, :number))
      end

    funcName0 =
      case {:asn1ct.get_gen_state_field(:active), :asn1ct.get_gen_state_field(:prefix)} do
        {true, pref} ->
          case :asn1ct.current_sindex() do
            i when is_integer(i) and i > 0 ->
              [pref, r_typedef(type, :name), '_', i]

            _ ->
              [pref, r_typedef(type, :name)]
          end

        {_, _} ->
          ['dec_', r_typedef(type, :name)]
      end

    funcName = {:asis, :erlang.list_to_atom(:lists.concat(funcName0))}

    emit([
      :nl,
      :nl,
      funcName,
      '(Tlv) ->',
      :nl,
      '   ',
      funcName,
      '(Tlv, ',
      {:asis, tag},
      ').',
      :nl,
      :nl,
      funcName,
      '(Tlv, TagIn) ->',
      :nl
    ])

    gen_decode_user(erules, type)
  end

  def gen_inc_decode(erules, type) when elem(type, 0) === :typedef do
    prefix = :asn1ct.get_gen_state_field(:prefix)
    suffix = :asn1ct_gen.index2suffix(:asn1ct.current_sindex())
    funcName0 = [prefix, r_typedef(type, :name), suffix]
    funcName = {:asis, :erlang.list_to_atom(:lists.concat(funcName0))}
    emit([:nl, :nl, funcName, '(Tlv, TagIn) ->', :nl])
    gen_decode_user(erules, type)
  end

  def gen_decode_selected(erules, type, funcName) do
    emit([funcName, '(Bin) ->', :nl])
    patterns = :asn1ct.read_config_data(:partial_decode)

    pattern =
      case :lists.keysearch(funcName, 1, patterns) do
        {:value, {_, p}} ->
          p

        false ->
          exit({:error, {:internal, :no_pattern_saved}})
      end

    emit([
      '  case ',
      {:call, :ber, :decode_selective, [{:asis, pattern}, 'Bin']},
      ' of',
      :nl,
      '    {ok,Bin2} when is_binary(Bin2) ->',
      :nl,
      '      {Tlv,_} = ',
      {:call, :ber, :ber_decode_nif, ['Bin2']},
      :com,
      :nl
    ])

    emit('{ok,')
    gen_decode_selected_type(erules, type)
    emit(['};', :nl, '    Err -> exit({error,{selective_decode,Err}})', :nl, '  end.', :nl])
  end

  defp gen_decode_selected_type(_Erules, typeDef) do
    def__ = r_typedef(typeDef, :typespec)
    innerType = :asn1ct_gen.get_inner(r_type(def__, :def))
    bytesVar = 'Tlv'

    tag =
      for x <- r_type(def__, :tag) do
        decode_class(r_tag(x, :class)) <<< (10 + r_tag(x, :number))
      end

    case :asn1ct_gen.type(innerType) do
      :ASN1_OPEN_TYPE ->
        :asn1ct_name.new(:len)
        gen_dec_prim(r_type(def__, def: :ASN1_OPEN_TYPE), bytesVar, tag)

      {:primitive, :bif} ->
        :asn1ct_name.new(:len)
        gen_dec_prim(def__, bytesVar, tag)

      {:constructed, :bif} ->
        topType =
          case r_typedef(typeDef, :name) do
            a when is_atom(a) ->
              [a]

            n ->
              n
          end

        decFunName = :lists.concat(['\'', :dec, '_', :asn1ct_gen.list2name(topType), '\''])
        emit([decFunName, '(', bytesVar, ', ', {:asis, tag}, ')'])

      theType ->
        decFunName = mkfuncname(theType, :dec)
        emit([decFunName, '(', bytesVar, ', ', {:asis, tag}, ')'])
    end
  end

  def gen_decode(erules, typename, type)
      when elem(type, 0) === :type do
    innerType = :asn1ct_gen.get_inner(r_type(type, :def))

    functionName =
      case :asn1ct.get_gen_state_field(:active) do
        true ->
          pattern = :asn1ct.get_gen_state_field(:namelist)

          suffix =
            case :asn1ct.maybe_saved_sindex(
                   typename,
                   pattern
                 ) do
              i when is_integer(i) and i > 0 ->
                :lists.concat(['_', i])

              _ ->
                ''
            end

          :lists.concat(['\'dec-inc-', :asn1ct_gen.list2name(typename), suffix])

        _ ->
          :lists.concat(['\'dec_', :asn1ct_gen.list2name(typename)])
      end

    case :asn1ct_gen.type(innerType) do
      {:constructed, :bif} ->
        objFun =
          case r_type(type, :tablecinf) do
            [{:objfun, _} | _R] ->
              ', ObjFun'

            _ ->
              ''
          end

        emit([functionName, '\'(Tlv, TagIn', objFun, ') ->', :nl])
        :asn1ct_gen.gen_decode_constructed(erules, typename, innerType, type)

      rec when elem(rec, 0) === :Externaltypereference ->
        case {typename, :asn1ct.get_gen_state_field(:namelist)} do
          {[cname | _], [{cname, _} | _]} ->
            case :asn1ct.is_function_generated(typename) do
              true ->
                :ok

              _ ->
                :asn1ct.generated_refed_func(typename)
                r_Externaltypereference(module: m, type: name) = rec
                typeDef = :asn1_db.dbget(m, name)
                gen_decode(erules, typeDef)
            end

          _ ->
            true
        end

      _ ->
        true
    end
  end

  def gen_decode(erules, tname, r_ComponentType(name: cname, typespec: type)) do
    newTname = [cname | tname]
    newType = r_type(type, tag: [])

    case {:asn1ct.get_gen_state_field(:active), :asn1ct.get_tobe_refed_func(newTname)} do
      {true, {_, nameList}} ->
        :asn1ct.update_gen_state(:namelist, nameList)
        gen_decode(erules, newTname, newType)

      {no, _} when no == false or no == :undefined ->
        gen_decode(erules, newTname, newType)

      _ ->
        :ok
    end
  end

  defp gen_decode_user(erules, d) when elem(d, 0) === :typedef do
    typename = [r_typedef(d, :name)]
    def__ = r_typedef(d, :typespec)
    innerType = :asn1ct_gen.get_inner(r_type(def__, :def))
    bytesVar = 'Tlv'

    case :asn1ct_gen.type(innerType) do
      :ASN1_OPEN_TYPE ->
        :asn1ct_name.new(:len)
        gen_dec_prim(r_type(def__, def: :ASN1_OPEN_TYPE), bytesVar, {:string, 'TagIn'})
        emit(['.', :nl, :nl])

      {:primitive, :bif} ->
        :asn1ct_name.new(:len)
        gen_dec_prim(def__, bytesVar, {:string, 'TagIn'})
        emit(['.', :nl, :nl])

      {:constructed, :bif} ->
        :asn1ct.update_namelist(r_typedef(d, :name))
        :asn1ct_gen.gen_decode_constructed(erules, typename, innerType, d)

      theType ->
        decFunName = mkfuncname(theType, :dec)
        emit([decFunName, '(', bytesVar, ', TagIn).', :nl, :nl])
    end
  end

  def gen_dec_prim(att, bytesVar, doTag) do
    typename = r_type(att, :def)
    constraint = get_size_constraint(r_type(att, :constraint))
    intConstr = int_constr(r_type(att, :constraint))

    newTypeName =
      case typename do
        :NumericString ->
          :restricted_string

        :TeletexString ->
          :restricted_string

        :T61String ->
          :restricted_string

        :VideotexString ->
          :restricted_string

        :GraphicString ->
          :restricted_string

        :VisibleString ->
          :restricted_string

        :GeneralString ->
          :restricted_string

        :PrintableString ->
          :restricted_string

        :IA5String ->
          :restricted_string

        :ObjectDescriptor ->
          :restricted_string

        :UTCTime ->
          :restricted_string

        :GeneralizedTime ->
          :restricted_string

        :"OCTET STRING" ->
          case :asn1ct.use_legacy_types() do
            true ->
              :restricted_string

            false ->
              typename
          end

        _ ->
          typename
      end

    tagStr =
      case doTag do
        {:string, tag1} ->
          tag1

        _ when is_list(doTag) ->
          {:asis, doTag}
      end

    case newTypeName do
      :BOOLEAN ->
        call(:decode_boolean, [bytesVar, tagStr])

      :INTEGER ->
        check_constraint(:decode_integer, [bytesVar, tagStr], intConstr, :identity, :identity)

      {:INTEGER, nNL} ->
        check_constraint(:decode_integer, [bytesVar, tagStr], intConstr, :identity, fn val ->
          :asn1ct_name.new(:val)
          emit([{:curr, :val}, ' = '])
          val.()
          emit([:com, :nl, {:call, :ber, :number2name, [{:curr, :val}, {:asis, nNL}]}])
        end)

      {:ENUMERATED, nNL} ->
        gen_dec_enumerated(bytesVar, nNL, tagStr)

      :REAL ->
        :asn1ct_name.new(:tmpbuf)

        emit([
          'begin',
          :nl,
          {:curr, :tmpbuf},
          ' = ',
          {:call, :ber, :match_tags, [bytesVar, tagStr]},
          :com,
          :nl,
          {:call, :real_common, :decode_real, [{:curr, :tmpbuf}]},
          :nl,
          'end',
          :nl
        ])

      {:"BIT STRING", nNL} ->
        gen_dec_bit_string(bytesVar, constraint, nNL, tagStr)

      :NULL ->
        call(:decode_null, [bytesVar, tagStr])

      :"OBJECT IDENTIFIER" ->
        call(:decode_object_identifier, [bytesVar, tagStr])

      :"RELATIVE-OID" ->
        call(:decode_relative_oid, [bytesVar, tagStr])

      :"OCTET STRING" ->
        check_constraint(
          :decode_octet_string,
          [bytesVar, tagStr],
          constraint,
          {:erlang, :byte_size},
          :identity
        )

      :restricted_string ->
        check_constraint(
          :decode_restricted_string,
          [bytesVar, tagStr],
          constraint,
          {:erlang, :byte_size},
          fn val ->
            emit('binary_to_list(')
            val.()
            emit(')')
          end
        )

      :UniversalString ->
        check_constraint(
          :decode_universal_string,
          [bytesVar, tagStr],
          constraint,
          {:erlang, :length},
          :identity
        )

      :UTF8String ->
        call(:decode_UTF8_string, [bytesVar, tagStr])

      :BMPString ->
        check_constraint(
          :decode_BMP_string,
          [bytesVar, tagStr],
          constraint,
          {:erlang, :length},
          :identity
        )

      :ASN1_OPEN_TYPE ->
        call(:decode_open_type_as_binary, [bytesVar, tagStr])
    end
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

  defp gen_dec_bit_string(bytesVar, _Constraint, [_ | _] = nNL, tagStr) do
    call(
      :decode_named_bit_string,
      [bytesVar, {:asis, nNL}, tagStr]
    )
  end

  defp gen_dec_bit_string(bytesVar, constraint, [], tagStr) do
    case :asn1ct.get_bit_string_format() do
      :compact ->
        check_constraint(
          :decode_compact_bit_string,
          [bytesVar, tagStr],
          constraint,
          {:ber, :compact_bit_string_size},
          :identity
        )

      :legacy ->
        check_constraint(
          :decode_native_bit_string,
          [bytesVar, tagStr],
          constraint,
          {:erlang, :bit_size},
          fn val ->
            :asn1ct_name.new(:val)
            emit([{:curr, :val}, ' = '])
            val.()
            emit([:com, :nl, {:call, :ber, :native_to_legacy_bit_string, [{:curr, :val}]}])
          end
        )

      :bitstring ->
        check_constraint(
          :decode_native_bit_string,
          [bytesVar, tagStr],
          constraint,
          {:erlang, :bit_size},
          :identity
        )
    end
  end

  defp check_constraint(f, args, constr, preConstr0, returnVal0) do
    preConstr =
      case preConstr0 do
        :identity ->
          fn v ->
            v
          end

        {mod, name} ->
          fn v ->
            :asn1ct_name.new(:c)
            emit([{:curr, :c}, ' = ', {:call, mod, name, [v]}, :com, :nl])
            {:curr, :c}
          end
      end

    returnVal =
      case returnVal0 do
        :identity ->
          fn val ->
            val.()
          end

        _ ->
          returnVal0
      end

    case constr do
      [] when returnVal0 === :identity ->
        call(f, args)

      [] ->
        emit(['begin', :nl])

        returnVal.(fn ->
          call(f, args)
        end)

        emit([:nl, 'end', :nl])

      _ ->
        :asn1ct_name.new(:val)
        emit(['begin', :nl, {:curr, :val}, ' = ', {:call, :ber, f, args}, :com, :nl])
        preVal0 = :asn1ct_gen.mk_var(:asn1ct_name.curr(:val))
        preVal = preConstr.(preVal0)
        emit('if ')

        case constr do
          {min, max} ->
            emit([{:asis, min}, ' =< ', preVal, ', ', preVal, ' =< ', {:asis, max}])

          sv when is_integer(sv) ->
            emit([preVal, ' =:= ', {:asis, sv}])
        end

        emit([' ->', :nl])

        returnVal.(fn ->
          emit(preVal0)
        end)

        emit([';', :nl, 'true ->', :nl, 'exit({error,{asn1,bad_range}})', :nl, 'end', :nl, 'end'])
    end
  end

  defp gen_dec_enumerated(bytesVar, nNL0, tagStr) do
    :asn1ct_name.new(:enum)
    emit(['case ', {:call, :ber, :decode_integer, [bytesVar, tagStr]}, ' of', :nl])

    nNL =
      case nNL0 do
        {l1, l2} ->
          l1 ++ l2 ++ [:accept]

        [_ | _] ->
          nNL0 ++ [:error]
      end

    gen_dec_enumerated_1(nNL)
    emit('end')
  end

  defp gen_dec_enumerated_1([:accept]) do
    :asn1ct_name.new(:default)
    emit([{:curr, :default}, ' -> {asn1_enum,', {:curr, :default}, '}', :nl])
  end

  defp gen_dec_enumerated_1([:error]) do
    :asn1ct_name.new(:default)

    emit([
      {:curr, :default},
      ' -> exit({error,{asn1,{illegal_enumerated,',
      {:curr, :default},
      '}}})',
      :nl
    ])
  end

  defp gen_dec_enumerated_1([{v, k} | t]) do
    emit([{:asis, k}, ' -> ', {:asis, v}, ';', :nl])
    gen_dec_enumerated_1(t)
  end

  def gen_obj_code(erules, _Module, obj)
      when elem(obj, 0) === :typedef do
    objName = r_typedef(obj, :name)
    def__ = r_typedef(obj, :typespec)
    r_Externaltypereference(module: m, type: clName) = r_Object(def__, :classname)
    class = :asn1_db.dbget(m, clName)
    {:object, _, fields} = r_Object(def__, :def)

    emit([
      :nl,
      :nl,
      :nl,
      '%%================================',
      :nl,
      '%%  ',
      objName,
      :nl,
      '%%================================',
      :nl
    ])

    encConstructed = gen_encode_objectfields(clName, get_class_fields(class), objName, fields, [])
    emit(:nl)
    gen_encode_constr_type(erules, encConstructed)
    emit(:nl)
    decConstructed = gen_decode_objectfields(clName, get_class_fields(class), objName, fields, [])
    emit(:nl)
    gen_decode_constr_type(erules, decConstructed)
    emit_tlv_format_function()
  end

  defp gen_encode_objectfields(
         className,
         [{:typefield, name, optOrMand} | rest],
         objName,
         objectFields,
         constrAcc
       ) do
    emitFuncClause = fn arg ->
      emit([
        {:asis, enc_func(objName)},
        '(',
        {:asis, name},
        ', ',
        arg,
        ', _RestPrimFieldName) ->',
        :nl
      ])
    end

    maybeConstr =
      case {get_object_field(
              name,
              objectFields
            ), optOrMand} do
        {false, :OPTIONAL} ->
          emitFuncClause.('Val')
          emit(['   {Val,0}'])
          []

        {false, {:DEFAULT, defaultType}} ->
          emitFuncClause.('Val')
          gen_encode_default_call(className, name, defaultType)

        {{^name, typeSpec}, _} ->
          emitFuncClause.('Val')
          gen_encode_field_call(objName, name, typeSpec)
      end

    case more_genfields(rest) do
      true ->
        emit([';', :nl])

      false ->
        emit(['.', :nl])
    end

    gen_encode_objectfields(className, rest, objName, objectFields, maybeConstr ++ constrAcc)
  end

  defp gen_encode_objectfields(
         className,
         [{:objectfield, name, _, _, optOrMand} | rest],
         objName,
         objectFields,
         constrAcc
       ) do
    currentMod = :erlang.get(:currmod)

    emitFuncClause = fn args ->
      emit([{:asis, enc_func(objName)}, '(', {:asis, name}, ', ', args, ') ->', :nl])
    end

    case {get_object_field(name, objectFields), optOrMand} do
      {false, :OPTIONAL} ->
        emitFuncClause.('_,_')
        emit(['  exit({error,{\'use of missing field in object\', ', {:asis, name}, '}})'])

      {false, {:DEFAULT, _DefaultObject}} ->
        exit({:error, {:asn1, {'not implemented yet', name}}})

      {{^name, r_Externalvaluereference(module: ^currentMod, value: typeName)}, _} ->
        emitFuncClause.(' Val, [H|T]')
        emit([indent(3), {:asis, enc_func(typeName)}, '(H, Val, T)'])

      {{^name, r_Externalvaluereference(module: m, value: typeName)}, _} ->
        emitFuncClause.(' Val, [H|T]')
        emit([indent(3), {:asis, m}, ':', {:asis, enc_func(typeName)}, '(H, Val, T)'])

      {{^name, r_typedef(name: typeName)}, _} when is_atom(typeName) ->
        emitFuncClause.(' Val, [H|T]')
        emit([indent(3), {:asis, enc_func(typeName)}, '(H, Val, T)'])
    end

    case more_genfields(rest) do
      true ->
        emit([';', :nl])

      false ->
        emit(['.', :nl])
    end

    gen_encode_objectfields(className, rest, objName, objectFields, constrAcc)
  end

  defp gen_encode_objectfields(className, [_C | cs], o, oF, acc) do
    gen_encode_objectfields(className, cs, o, oF, acc)
  end

  defp gen_encode_objectfields(_, [], _, _, acc) do
    acc
  end

  defp gen_encode_constr_type(erules, [typeDef | rest])
       when elem(typeDef, 0) === :typedef do
    case is_already_generated(:enc, r_typedef(typeDef, :name)) do
      true ->
        :ok

      false ->
        gen_encode_user(erules, typeDef, false)
    end

    gen_encode_constr_type(erules, rest)
  end

  defp gen_encode_constr_type(_, []) do
    :ok
  end

  defp gen_encode_field_call(_ObjName, _FieldName, r_Externaltypereference(module: m, type: t)) do
    currentMod = :erlang.get(:currmod)
    tDef = :asn1_db.dbget(m, t)
    def__ = r_typedef(tDef, :typespec)
    oTag = r_type(def__, :tag)

    tag =
      for x <- oTag do
        encode_tag_val(decode_class(r_tag(x, :class)), r_tag(x, :form), r_tag(x, :number))
      end

    cond do
      m == currentMod ->
        emit(['   ', {:asis, enc_func(t)}, '(Val, ', {:asis, tag}, ')'])
        []

      true ->
        emit(['   ', {:asis, m}, ':', {:asis, enc_func(t)}, '(Val, ', {:asis, tag}, ')'])
        []
    end
  end

  defp gen_encode_field_call(objName, fieldName, type) do
    def__ = r_typedef(type, :typespec)
    oTag = r_type(def__, :tag)

    tag =
      for x <- oTag do
        encode_tag_val(decode_class(r_tag(x, :class)), r_tag(x, :form), r_tag(x, :number))
      end

    case r_typedef(type, :name) do
      {:primitive, :bif} ->
        gen_encode_prim(:ber, def__, {:asis, :lists.reverse(tag)}, 'Val')
        []

      {:constructed, :bif} ->
        name = :lists.concat([objName, :_, fieldName])
        emit(['   ', {:asis, enc_func(name)}, '(Val,', {:asis, tag}, ')'])
        [r_typedef(type, name: :erlang.list_to_atom(name))]

      {extMod, typeName} ->
        emit([
          '   ',
          {:asis, extMod},
          ':',
          {:asis, enc_func(typeName)},
          '(Val,',
          {:asis, tag},
          ')'
        ])

        []

      typeName ->
        emit(['   ', {:asis, enc_func(typeName)}, '(Val,', {:asis, tag}, ')'])
        []
    end
  end

  defp gen_encode_default_call(className, fieldName, type) do
    currentMod = :erlang.get(:currmod)
    innerType = :asn1ct_gen.get_inner(r_type(type, :def))
    oTag = r_type(type, :tag)

    tag =
      for x <- oTag do
        encode_tag_val(decode_class(r_tag(x, :class)), r_tag(x, :form), r_tag(x, :number))
      end

    case :asn1ct_gen.type(innerType) do
      {:constructed, :bif} ->
        name = :lists.concat([className, :_, fieldName])
        emit(['   ', {:asis, enc_func(name)}, '(Val, ', {:asis, tag}, ')'])
        [r_typedef(name: :erlang.list_to_atom(name), typespec: type)]

      {:primitive, :bif} ->
        gen_encode_prim(:ber, type, {:asis, :lists.reverse(tag)}, 'Val')
        []

      r_Externaltypereference(module: ^currentMod, type: etype) ->
        emit(['   \'enc_', etype, '\'(Val, ', {:asis, tag}, ')', :nl])
        []

      r_Externaltypereference(module: emod, type: etype) ->
        emit(['   \'', emod, '\':\'enc_', etype, '\'(Val, ', {:asis, tag}, ')', :nl])
        []
    end
  end

  defp gen_decode_objectfields(
         className,
         [{:typefield, name, optOrMand} | rest],
         objName,
         objectFields,
         constrAcc
       ) do
    emitFuncClause = fn arg ->
      emit([{:asis, dec_func(objName)}, '(', {:asis, name}, ', ', arg, ',_) ->', :nl])
    end

    maybeConstr =
      case {get_object_field(
              name,
              objectFields
            ), optOrMand} do
        {false, :OPTIONAL} ->
          emitFuncClause.(' Bytes')
          emit(['   Bytes'])
          []

        {false, {:DEFAULT, defaultType}} ->
          emitFuncClause.('Bytes')
          emit_tlv_format('Bytes')
          gen_decode_default_call(className, name, 'Tlv', defaultType)

        {{^name, typeSpec}, _} ->
          emitFuncClause.('Bytes')
          emit_tlv_format('Bytes')
          gen_decode_field_call(objName, name, 'Tlv', typeSpec)
      end

    case more_genfields(rest) do
      true ->
        emit([';', :nl])

      false ->
        emit(['.', :nl])
    end

    gen_decode_objectfields(className, rest, objName, objectFields, maybeConstr ++ constrAcc)
  end

  defp gen_decode_objectfields(
         className,
         [{:objectfield, name, _, _, optOrMand} | rest],
         objName,
         objectFields,
         constrAcc
       ) do
    currentMod = :erlang.get(:currmod)

    emitFuncClause = fn args ->
      emit([{:asis, dec_func(objName)}, '(', {:asis, name}, ', ', args, ') ->', :nl])
    end

    case {get_object_field(name, objectFields), optOrMand} do
      {false, :OPTIONAL} ->
        emitFuncClause.('_,_')

        emit(['  exit({error,{\'illegal use of missing field in object\', ', {:asis, name}, '}})'])

      {false, {:DEFAULT, _DefaultObject}} ->
        exit({:error, {:asn1, {'not implemented yet', name}}})

      {{^name, r_Externalvaluereference(module: ^currentMod, value: typeName)}, _} ->
        emitFuncClause.('Bytes,[H|T]')
        emit([indent(3), {:asis, dec_func(typeName)}, '(H, Bytes, T)'])

      {{^name, r_Externalvaluereference(module: m, value: typeName)}, _} ->
        emitFuncClause.('Bytes,[H|T]')
        emit([indent(3), {:asis, m}, ':', {:asis, dec_func(typeName)}, '(H, Bytes, T)'])

      {{^name, r_typedef(name: typeName)}, _} when is_atom(typeName) ->
        emitFuncClause.('Bytes,[H|T]')
        emit([indent(3), {:asis, dec_func(typeName)}, '(H, Bytes, T)'])
    end

    case more_genfields(rest) do
      true ->
        emit([';', :nl])

      false ->
        emit(['.', :nl])
    end

    gen_decode_objectfields(className, rest, objName, objectFields, constrAcc)
  end

  defp gen_decode_objectfields(cN, [_C | cs], o, oF, cAcc) do
    gen_decode_objectfields(cN, cs, o, oF, cAcc)
  end

  defp gen_decode_objectfields(_, [], _, _, cAcc) do
    cAcc
  end

  defp emit_tlv_format(bytes) do
    notice_tlv_format_gen()
    emit(['  Tlv = tlv_format(', bytes, '),', :nl])
  end

  defp notice_tlv_format_gen() do
    module = :erlang.get(:currmod)

    case :erlang.get(:tlv_format) do
      {:done, ^module} ->
        :ok

      _ ->
        :erlang.put(:tlv_format, true)
    end
  end

  defp emit_tlv_format_function() do
    module = :erlang.get(:currmod)

    case :erlang.get(:tlv_format) do
      true ->
        emit_tlv_format_function1()
        :erlang.put(:tlv_format, {:done, module})

      _ ->
        :ok
    end
  end

  defp emit_tlv_format_function1() do
    emit([
      'tlv_format(Bytes) when is_binary(Bytes) ->',
      :nl,
      '  {Tlv,_} = ',
      {:call, :ber, :ber_decode_nif, ['Bytes']},
      :com,
      :nl,
      '  Tlv;',
      :nl,
      'tlv_format(Bytes) ->',
      :nl,
      '  Bytes.',
      :nl
    ])
  end

  defp gen_decode_constr_type(erules, [typeDef | rest])
       when elem(typeDef, 0) === :typedef do
    case is_already_generated(:dec, r_typedef(typeDef, :name)) do
      true ->
        :ok

      _ ->
        emit([:nl, :nl, '\'dec_', r_typedef(typeDef, :name), '\'(Tlv, TagIn) ->', :nl])
        gen_decode_user(erules, typeDef)
    end

    gen_decode_constr_type(erules, rest)
  end

  defp gen_decode_constr_type(_, []) do
    :ok
  end

  defp gen_decode_field_call(
         _ObjName,
         _FieldName,
         bytes,
         r_Externaltypereference(module: m, type: t)
       ) do
    currentMod = :erlang.get(:currmod)
    tDef = :asn1_db.dbget(m, t)
    def__ = r_typedef(tDef, :typespec)
    oTag = r_type(def__, :tag)

    tag =
      for x <- oTag do
        decode_class(r_tag(x, :class)) <<< (10 + r_tag(x, :number))
      end

    cond do
      m == currentMod ->
        emit(['   ', {:asis, dec_func(t)}, '(', bytes, ', ', {:asis, tag}, ')'])
        []

      true ->
        emit(['   ', {:asis, m}, ':', {:asis, dec_func(t)}, '(', bytes, ', ', {:asis, tag}, ')'])
        []
    end
  end

  defp gen_decode_field_call(objName, fieldName, bytes, type) do
    def__ = r_typedef(type, :typespec)
    oTag = r_type(def__, :tag)

    tag =
      for x <- oTag do
        decode_class(r_tag(x, :class)) <<< (10 + r_tag(x, :number))
      end

    case r_typedef(type, :name) do
      {:primitive, :bif} ->
        gen_dec_prim(def__, bytes, tag)
        []

      {:constructed, :bif} ->
        name = :lists.concat([objName, '_', fieldName])
        emit(['   ', {:asis, dec_func(name)}, '(', bytes, ',', {:asis, tag}, ')'])
        [r_typedef(type, name: :erlang.list_to_atom(name))]

      {extMod, typeName} ->
        emit([
          '   ',
          {:asis, extMod},
          ':',
          {:asis, dec_func(typeName)},
          '(',
          bytes,
          ',',
          {:asis, tag},
          ')'
        ])

        []

      typeName ->
        emit(['   ', {:asis, dec_func(typeName)}, '(', bytes, ',', {:asis, tag}, ')'])
        []
    end
  end

  defp gen_decode_default_call(className, fieldName, bytes, type) do
    currentMod = :erlang.get(:currmod)
    innerType = :asn1ct_gen.get_inner(r_type(type, :def))
    oTag = r_type(type, :tag)

    tag =
      for x <- oTag do
        decode_class(r_tag(x, :class)) <<< (10 + r_tag(x, :number))
      end

    case :asn1ct_gen.type(innerType) do
      {:constructed, :bif} ->
        emit(['   \'dec_', className, :_, fieldName, '\'(', bytes, ',', {:asis, tag}, ')'])

        [
          r_typedef(
            name: :erlang.list_to_atom(:lists.concat([className, :_, fieldName])),
            typespec: type
          )
        ]

      {:primitive, :bif} ->
        gen_dec_prim(type, bytes, tag)
        []

      r_Externaltypereference(module: ^currentMod, type: etype) ->
        emit(['   \'dec_', etype, '\'(', bytes, ' ,', {:asis, tag}, ')', :nl])
        []

      r_Externaltypereference(module: emod, type: etype) ->
        emit(['   \'', emod, '\':\'dec_', etype, '\'(', bytes, ', ', {:asis, tag}, ')', :nl])
        []
    end
  end

  defp is_already_generated(operation, name) do
    case :erlang.get(:class_default_type) do
      :undefined ->
        :erlang.put(:class_default_type, [{operation, name}])
        false

      generatedList ->
        case :lists.member(
               {operation, name},
               generatedList
             ) do
          true ->
            true

          false ->
            :erlang.put(
              :class_default_type,
              [{operation, name} | generatedList]
            )

            false
        end
    end
  end

  defp more_genfields([]) do
    false
  end

  defp more_genfields([field | fields]) do
    case :erlang.element(1, field) do
      :typefield ->
        true

      :objectfield ->
        true

      _ ->
        more_genfields(fields)
    end
  end

  def gen_objectset_code(erules, objSet) do
    objSetName = r_typedef(objSet, :name)
    def__ = r_typedef(objSet, :typespec)

    r_Externaltypereference(
      module: classModule,
      type: className
    ) = r_ObjectSet(def__, :class)

    classDef = :asn1_db.dbget(classModule, className)
    uniqueFName = r_ObjectSet(def__, :uniquefname)
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

    case className do
      {_Module, extClassName} ->
        gen_objset_code(erules, objSetName, uniqueFName, set, extClassName, classDef)

      _ ->
        gen_objset_code(erules, objSetName, uniqueFName, set, className, classDef)
    end

    emit(:nl)
  end

  defp gen_objset_code(erules, objSetName, uniqueFName, set, className, classDef) do
    classFields = get_class_fields(classDef)

    internalFuncs =
      gen_objset_enc(erules, objSetName, uniqueFName, set, className, classFields, 1, [])

    gen_objset_dec(erules, objSetName, uniqueFName, set, className, classFields, 1)
    gen_internal_funcs(erules, internalFuncs)
  end

  defp gen_objset_enc(_, _, {:unique, :undefined}, _, _, _, _, _) do
    []
  end

  defp gen_objset_enc(
         erules,
         objSetName,
         uniqueName,
         [{objName, val, fields} | t],
         clName,
         clFields,
         nthObj,
         acc
       ) do
    currMod = :erlang.get(:currmod)

    {internalFunc, newNthObj} =
      case objName do
        {:no_mod, :no_name} ->
          gen_inlined_enc_funs(fields, clFields, objSetName, val, nthObj)

        {^currMod, name} ->
          emit([
            asis_atom(['getenc_', objSetName]),
            '(Id) when Id =:= ',
            {:asis, val},
            ' ->',
            :nl,
            '    fun ',
            asis_atom(['enc_', name]),
            '/3;',
            :nl
          ])

          {[], nthObj}

        {moduleName, name} ->
          emit([asis_atom(['getenc_', objSetName]), '(Id) when Id =:= ', {:asis, val}, ' ->', :nl])

          emit_ext_fun(:enc, moduleName, name)
          emit([';', :nl])
          {[], nthObj}

        _ ->
          emit([
            asis_atom(['getenc_', objSetName]),
            '(',
            {:asis, val},
            ') ->',
            :nl,
            '  fun ',
            asis_atom(['enc_', objName]),
            '/3;',
            :nl
          ])

          {[], nthObj}
      end

    gen_objset_enc(
      erules,
      objSetName,
      uniqueName,
      t,
      clName,
      clFields,
      newNthObj,
      internalFunc ++ acc
    )
  end

  defp gen_objset_enc(
         _,
         objSetName,
         _UniqueName,
         [:EXTENSIONMARK],
         _ClName,
         _ClFields,
         _NthObj,
         acc
       ) do
    emit([
      asis_atom(['getenc_', objSetName]),
      '(_) ->',
      :nl,
      indent(2),
      'fun(_, Val, _RestPrimFieldName) ->',
      :nl
    ])

    emit_enc_open_type(4)
    emit([:nl, indent(2), 'end.', :nl, :nl])
    acc
  end

  defp gen_objset_enc(_, objSetName, uniqueName, [], _, _, _, acc) do
    emit_default_getenc(objSetName, uniqueName)
    emit(['.', :nl, :nl])
    acc
  end

  defp emit_ext_fun(encDec, moduleName, name) do
    emit([indent(3), 'fun(T,V,O) -> \'', moduleName, '\':\'', encDec, '_', name, '\'(T,V,O) end'])
  end

  defp emit_default_getenc(objSetName, uniqueName) do
    emit([
      asis_atom(['getenc_', objSetName]),
      '(ErrV) ->',
      :nl,
      indent(3),
      'fun(C,V,_) ->',
      :nl,
      'exit({\'Type not compatible with table constraint\',{component,C},{value,V}, {unique_name_and_value,',
      {:asis, uniqueName},
      ', ErrV}}) end'
    ])
  end

  defp gen_inlined_enc_funs(fields, [{:typefield, _, _} | _] = t, objSetName, val, nthObj) do
    emit([
      asis_atom(['getenc_', objSetName]),
      '(',
      {:asis, val},
      ') ->',
      :nl,
      indent(3),
      'fun(Type, Val, _RestPrimFieldName) ->',
      :nl,
      indent(6),
      'case Type of',
      :nl
    ])

    gen_inlined_enc_funs1(fields, t, objSetName, [], nthObj, [])
  end

  defp gen_inlined_enc_funs(fields, [_ | rest], objSetName, val, nthObj) do
    gen_inlined_enc_funs(fields, rest, objSetName, val, nthObj)
  end

  defp gen_inlined_enc_funs(_, [], _, _, nthObj) do
    {[], nthObj}
  end

  defp gen_inlined_enc_funs1(
         fields,
         [{:typefield, name, _} | rest],
         objSetName,
         sep0,
         nthObj,
         acc0
       ) do
    emit(sep0)
    sep = [';', :nl]
    currMod = :erlang.get(:currmod)
    internalDefFunName = :asn1ct_gen.list2name([nthObj, name, objSetName])

    {acc, nAdd} =
      case :lists.keyfind(name, 1, fields) do
        {_, r_type() = type} ->
          {ret, n} = emit_inner_of_fun(type, internalDefFunName)
          {ret ++ acc0, n}

        {_, r_typedef() = type} ->
          emit([indent(9), {:asis, name}, ' ->', :nl])
          {ret, n} = emit_inner_of_fun(type, internalDefFunName)
          {ret ++ acc0, n}

        {_, r_Externaltypereference(module: m, type: t)} ->
          emit([indent(9), {:asis, name}, ' ->', :nl])

          cond do
            m === currMod ->
              emit([indent(12), '\'enc_', t, '\'(Val)'])

            true ->
              r_typedef(typespec: type) = :asn1_db.dbget(m, t)
              oTag = r_type(type, :tag)

              tag =
                for x <- oTag do
                  encode_tag_val(
                    decode_class(r_tag(x, :class)),
                    r_tag(x, :form),
                    r_tag(x, :number)
                  )
                end

              emit([indent(12), '\'', m, '\':\'enc_', t, '\'(Val, ', {:asis, tag}, ')'])
          end

          {acc0, 0}

        false ->
          emit([indent(9), {:asis, name}, ' ->', :nl])
          emit_enc_open_type(11)
          {acc0, 0}
      end

    gen_inlined_enc_funs1(fields, rest, objSetName, sep, nthObj + nAdd, acc)
  end

  defp gen_inlined_enc_funs1(fields, [_ | rest], objSetName, sep, nthObj, acc) do
    gen_inlined_enc_funs1(fields, rest, objSetName, sep, nthObj, acc)
  end

  defp gen_inlined_enc_funs1(_, [], _, _, nthObj, acc) do
    emit([:nl, indent(6), 'end', :nl, indent(3), 'end;', :nl])
    {acc, nthObj}
  end

  defp emit_enc_open_type(i) do
    indent = indent(i)

    s = [
      [
        indent,
        'case Val of',
        :nl,
        indent,
        indent(2),
        '{asn1_OPENTYPE,Bin} when is_binary(Bin) ->',
        :nl,
        indent,
        indent(4),
        '{Bin,byte_size(Bin)}'
      ]
      | case :asn1ct.use_legacy_types() do
          false ->
            [:nl, indent, 'end']

          true ->
            [
              ';',
              :nl,
              indent,
              indent(2),
              'Bin when is_binary(Bin) ->',
              :nl,
              indent,
              indent(4),
              '{Bin,byte_size(Bin)};',
              :nl,
              indent,
              indent(2),
              '_ ->',
              :nl,
              indent,
              indent(4),
              '{Val,length(Val)}',
              :nl,
              indent,
              'end'
            ]
        end
    ]

    emit(s)
  end

  defp emit_inner_of_fun(
         tDef = r_typedef(name: {extMod, name}, typespec: type),
         internalDefFunName
       ) do
    oTag = r_type(type, :tag)

    tag =
      for x <- oTag do
        encode_tag_val(decode_class(r_tag(x, :class)), r_tag(x, :form), r_tag(x, :number))
      end

    case {extMod, name} do
      {:primitive, :bif} ->
        emit(indent(12))
        gen_encode_prim(:ber, type, [{:asis, :lists.reverse(tag)}], 'Val')
        {[], 0}

      {:constructed, :bif} ->
        emit([indent(12), '\'enc_', internalDefFunName, '\'(Val, ', {:asis, tag}, ')'])
        {[r_typedef(tDef, name: internalDefFunName)], 1}

      _ ->
        emit([indent(12), '\'', extMod, '\':\'enc_', name, '\'(Val', {:asis, tag}, ')'])
        {[], 0}
    end
  end

  defp emit_inner_of_fun(r_typedef(name: name), _) do
    emit([indent(12), '\'enc_', name, '\'(Val)'])
    {[], 0}
  end

  defp emit_inner_of_fun(type, _) when elem(type, 0) === :type do
    currMod = :erlang.get(:currmod)

    case r_type(type, :def) do
      def__ when is_atom(def__) ->
        oTag = r_type(type, :tag)

        tag =
          for x <- oTag do
            encode_tag_val(decode_class(r_tag(x, :class)), r_tag(x, :form), r_tag(x, :number))
          end

        emit([indent(9), def__, ' ->', :nl, indent(12)])
        gen_encode_prim(:ber, type, {:asis, :lists.reverse(tag)}, 'Val')

      r_Externaltypereference(module: ^currMod, type: t) ->
        emit([indent(9), t, ' ->', :nl, indent(12), '\'enc_', t, '\'(Val)'])

      r_Externaltypereference(module: extMod, type: t) ->
        r_typedef(typespec: extType) = :asn1_db.dbget(extMod, t)
        oTag = r_type(extType, :tag)

        tag =
          for x <- oTag do
            encode_tag_val(decode_class(r_tag(x, :class)), r_tag(x, :form), r_tag(x, :number))
          end

        emit([
          indent(9),
          t,
          ' ->',
          :nl,
          indent(12),
          extMod,
          ':\'enc_',
          t,
          '\'(Val, ',
          {:asis, tag},
          ')'
        ])
    end

    {[], 0}
  end

  defp indent(n) do
    :lists.duplicate(n, 32)
  end

  defp gen_objset_dec(_, _, {:unique, :undefined}, _, _, _, _) do
    :ok
  end

  defp gen_objset_dec(
         erules,
         objSName,
         uniqueName,
         [{objName, val, fields} | t],
         clName,
         clFields,
         nthObj
       ) do
    currMod = :erlang.get(:currmod)

    newNthObj =
      case objName do
        {:no_mod, :no_name} ->
          gen_inlined_dec_funs(fields, clFields, objSName, val, nthObj)

        {^currMod, name} ->
          emit([
            asis_atom(['getdec_', objSName]),
            '(Id) when Id =:= ',
            {:asis, val},
            ' ->',
            :nl,
            '    fun \'dec_',
            name,
            '\'/3;',
            :nl
          ])

          nthObj

        {moduleName, name} ->
          emit([asis_atom(['getdec_', objSName]), '(Id) when Id =:= ', {:asis, val}, ' ->', :nl])
          emit_ext_fun(:dec, moduleName, name)
          emit([';', :nl])
          nthObj

        _ ->
          emit([
            asis_atom(['getdec_', objSName]),
            '(',
            {:asis, val},
            ') ->',
            :nl,
            '    fun \'dec_',
            objName,
            '\'/3;',
            :nl
          ])

          nthObj
      end

    gen_objset_dec(erules, objSName, uniqueName, t, clName, clFields, newNthObj)
  end

  defp gen_objset_dec(_, objSetName, _UniqueName, [:EXTENSIONMARK], _ClName, _ClFields, _NthObj) do
    emit([
      asis_atom(['getdec_', objSetName]),
      '(_) ->',
      :nl,
      indent(2),
      'fun(_,Bytes, _RestPrimFieldName) ->',
      :nl
    ])

    emit_dec_open_type(4)
    emit([:nl, indent(2), 'end.', :nl, :nl])
    :ok
  end

  defp gen_objset_dec(_, objSetName, uniqueName, [], _, _, _) do
    emit_default_getdec(objSetName, uniqueName)
    emit(['.', :nl, :nl])
    :ok
  end

  defp emit_default_getdec(objSetName, uniqueName) do
    emit(['\'getdec_', objSetName, '\'(ErrV) ->', :nl])

    emit([
      indent(2),
      'fun(C,V,_) -> exit({{component,C},{value,V},{unique_name_and_value,',
      {:asis, uniqueName},
      ', ErrV}}) end'
    ])
  end

  defp gen_inlined_dec_funs(fields, [{:typefield, _, _} | _] = clFields, objSetName, val, nthObj) do
    emit(['\'getdec_', objSetName, '\'(', {:asis, val}, ') ->', :nl])

    emit([
      indent(3),
      'fun(Type, Bytes, _RestPrimFieldName) ->',
      :nl,
      indent(6),
      'case Type of',
      :nl
    ])

    gen_inlined_dec_funs1(fields, clFields, objSetName, '', nthObj)
  end

  defp gen_inlined_dec_funs(fields, [_ | clFields], objSetName, val, nthObj) do
    gen_inlined_dec_funs(fields, clFields, objSetName, val, nthObj)
  end

  defp gen_inlined_dec_funs(_, _, _, _, nthObj) do
    nthObj
  end

  defp gen_inlined_dec_funs1(fields, [{:typefield, name, prop} | rest], objSetName, sep0, nthObj) do
    emit(sep0)
    sep = [';', :nl]

    decProp =
      case prop do
        :OPTIONAL ->
          :opt_or_default

        {:DEFAULT, _} ->
          :opt_or_default

        _ ->
          :mandatory
      end

    internalDefFunName = [nthObj, name, objSetName]

    n =
      case :lists.keyfind(name, 1, fields) do
        {_, r_type() = type} ->
          emit_inner_of_decfun(type, decProp, internalDefFunName)

        {_, r_typedef() = type} ->
          emit([indent(9), {:asis, name}, ' ->', :nl])
          emit_inner_of_decfun(type, decProp, internalDefFunName)

        {_, r_Externaltypereference(module: m, type: t)} ->
          emit([indent(9), {:asis, name}, ' ->', :nl])
          currMod = :erlang.get(:currmod)

          cond do
            m === currMod ->
              emit([indent(12), '\'dec_', t, '\'(Bytes)'])

            true ->
              r_typedef(typespec: type) = :asn1_db.dbget(m, t)
              oTag = r_type(type, :tag)

              tag =
                for x <- oTag do
                  decode_class(r_tag(x, :class)) <<< (10 + r_tag(x, :number))
                end

              emit([indent(12), '\'', m, '\':\'dec_', t, '\'(Bytes, ', {:asis, tag}, ')'])
          end

          0

        false ->
          emit([indent(9), {:asis, name}, ' ->', :nl])
          emit_dec_open_type(11)
          0
      end

    gen_inlined_dec_funs1(fields, rest, objSetName, sep, nthObj + n)
  end

  defp gen_inlined_dec_funs1(fields, [_ | rest], objSetName, sep, nthObj) do
    gen_inlined_dec_funs1(fields, rest, objSetName, sep, nthObj)
  end

  defp gen_inlined_dec_funs1(_, [], _, _, nthObj) do
    emit([:nl, indent(6), 'end', :nl, indent(3), 'end;', :nl])
    nthObj
  end

  defp emit_dec_open_type(i) do
    indent = indent(i)

    s =
      case :asn1ct.use_legacy_types() do
        false ->
          [
            indent,
            'case Bytes of',
            :nl,
            indent,
            indent(2),
            'Bin when is_binary(Bin) -> ',
            :nl,
            indent,
            indent(4),
            '{asn1_OPENTYPE,Bin};',
            :nl,
            indent,
            indent(2),
            '_ ->',
            :nl,
            indent,
            indent(4),
            '{asn1_OPENTYPE,',
            {:call, :ber, :ber_encode, ['Bytes']},
            '}',
            :nl,
            indent,
            'end'
          ]

        true ->
          [
            indent,
            'case Bytes of',
            :nl,
            indent,
            indent(2),
            'Bin when is_binary(Bin) -> ',
            :nl,
            indent,
            indent(4),
            'Bin;',
            :nl,
            indent,
            indent(2),
            '_ ->',
            :nl,
            indent,
            indent(4),
            {:call, :ber, :ber_encode, ['Bytes']},
            :nl,
            indent,
            'end'
          ]
      end

    emit(s)
  end

  defp emit_inner_of_decfun(
         r_typedef(name: {extName, name}, typespec: type),
         _Prop,
         internalDefFunName
       ) do
    oTag = r_type(type, :tag)

    tag =
      for x <- oTag do
        decode_class(r_tag(x, :class)) <<< (10 + r_tag(x, :number))
      end

    case {extName, name} do
      {:primitive, :bif} ->
        emit(indent(12))
        gen_dec_prim(type, 'Bytes', tag)
        0

      {:constructed, :bif} ->
        emit([
          indent(12),
          '\'dec_',
          :asn1ct_gen.list2name(internalDefFunName),
          '\'(Bytes, ',
          {:asis, tag},
          ')'
        ])

        1

      _ ->
        emit([indent(12), '\'', extName, '\':\'dec_', name, '\'(Bytes, ', {:asis, tag}, ')'])
        0
    end
  end

  defp emit_inner_of_decfun(r_typedef(name: name), _Prop, _) do
    emit([indent(12), '\'dec_', name, '\'(Bytes)'])
    0
  end

  defp emit_inner_of_decfun(r_type() = type, _Prop, _) do
    oTag = r_type(type, :tag)

    tag =
      for x <- oTag do
        decode_class(r_tag(x, :class)) <<< (10 + r_tag(x, :number))
      end

    currMod = :erlang.get(:currmod)
    def__ = r_type(type, :def)
    innerType = :asn1ct_gen.get_inner(def__)
    whatKind = :asn1ct_gen.type(innerType)

    case whatKind do
      {:primitive, :bif} ->
        emit([indent(9), def__, ' ->', :nl, indent(12)])
        gen_dec_prim(type, 'Bytes', tag)

      r_Externaltypereference(module: ^currMod, type: t) ->
        emit([indent(9), t, ' ->', :nl, indent(12), '\'dec_', t, '\'(Bytes)'])

      r_Externaltypereference(module: extMod, type: t) ->
        emit([
          indent(9),
          t,
          ' ->',
          :nl,
          indent(12),
          extMod,
          ':\'dec_',
          t,
          '\'(Bytes, ',
          {:asis, tag},
          ')'
        ])
    end

    0
  end

  defp gen_internal_funcs(_, []) do
    :ok
  end

  defp gen_internal_funcs(erules, [typeDef | rest]) do
    gen_encode_user(erules, typeDef, false)
    emit([:nl, :nl, '\'dec_', r_typedef(typeDef, :name), '\'(Tlv, TagIn) ->', :nl])
    gen_decode_user(erules, typeDef)
    gen_internal_funcs(erules, rest)
  end

  def decode_class(:UNIVERSAL) do
    0
  end

  def decode_class(:APPLICATION) do
    64
  end

  def decode_class(:CONTEXT) do
    128
  end

  def decode_class(:PRIVATE) do
    192
  end

  defp mkfuncname(r_Externaltypereference(module: mod, type: eType), decOrEnc) do
    currMod = :erlang.get(:currmod)

    case currMod do
      ^mod ->
        :lists.concat(['\'', decOrEnc, '_', eType, '\''])

      _ ->
        :lists.concat(['\'', mod, '\':\'', decOrEnc, '_', eType, '\''])
    end
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

  defp get_class_fields(r_classdef(typespec: objClass)) do
    r_objectclass(objClass, :fields)
  end

  defp get_class_fields(r_objectclass(fields: fields)) do
    fields
  end

  defp get_class_fields(_) do
    []
  end

  defp get_object_field(name, objectFields) do
    case :lists.keysearch(name, 1, objectFields) do
      {:value, field} ->
        field

      false ->
        false
    end
  end

  def encode_tag_val(class, form, tagNo) when tagNo <= 30 do
    <<class >>> 6::size(2), form >>> 5::size(1), tagNo::size(5)>>
  end

  def encode_tag_val(class, form, tagNo) do
    {octets, _Len} = mk_object_val(tagNo)
    binOct = :erlang.list_to_binary(octets)
    <<class >>> 6::size(2), form >>> 5::size(1), 31::size(5), binOct::binary>>
  end

  defp mk_object_val(val) when val <= 127 do
    {[255 &&& val], 1}
  end

  defp mk_object_val(val) do
    mk_object_val(val >>> 7, [val &&& 127], 1)
  end

  defp mk_object_val(0, ack, len) do
    {ack, len}
  end

  defp mk_object_val(val, ack, len) do
    mk_object_val(val >>> 7, [(val &&& 127) ||| 128 | ack], len + 1)
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

  defp call(f, args) do
    :asn1ct_func.call(:ber, f, args)
  end

  defp enc_func(tname) do
    :erlang.list_to_atom(:lists.concat(['enc_', tname]))
  end

  defp dec_func(tname) do
    :erlang.list_to_atom(:lists.concat(['dec_', tname]))
  end

  defp asis_atom(list) do
    {:asis, :erlang.list_to_atom(:lists.concat(list))}
  end
end
