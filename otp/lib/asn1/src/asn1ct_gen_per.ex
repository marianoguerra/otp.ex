defmodule :m_asn1ct_gen_per do
  use Bitwise
  import :asn1ct_func, only: [call: 3]
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

  def dialyzer_suppressions(r_gen(erule: :per, aligned: aligned)) do
    mod =
      case aligned do
        false ->
          :uper

        true ->
          :per
      end

    suppress({mod, :complete, 1})
    suppress({:per_common, :to_bitstring, 2})
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

  def gen_encode(erules, type) when elem(type, 0) === :typedef do
    gen_encode_user(erules, type)
  end

  def gen_encode(erules, typename, r_ComponentType(name: cname, typespec: type)) do
    newTypename = [cname | typename]
    gen_encode(erules, newTypename, type)
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
        func = enc_func(:asn1ct_gen.list2name(typename))
        emit([{:asis, func}, '(Val', objFun, ') ->', :nl])
        :asn1ct_gen.gen_encode_constructed(erules, typename, innerType, type)

      _ ->
        true
    end
  end

  defp gen_encode_user(erules, d) when elem(d, 0) === :typedef do
    currMod = :erlang.get(:currmod)
    typename = [r_typedef(d, :name)]
    def__ = r_typedef(d, :typespec)
    innerType = :asn1ct_gen.get_inner(r_type(def__, :def))
    func = enc_func(:asn1ct_gen.list2name(typename))
    emit([{:asis, func}, '(Val) ->', :nl])

    case :asn1ct_gen.type(innerType) do
      {:primitive, :bif} ->
        gen_encode_prim(erules, def__)
        emit(['.', :nl])

      :ASN1_OPEN_TYPE ->
        gen_encode_prim(erules, r_type(def__, def: :ASN1_OPEN_TYPE))
        emit(['.', :nl])

      {:constructed, :bif} ->
        :asn1ct_gen.gen_encode_constructed(erules, typename, innerType, d)

      r_Externaltypereference(module: ^currMod, type: etype) ->
        emit([{:asis, enc_func(etype)}, '(Val).', :nl])

      r_Externaltypereference(module: emod, type: etype) ->
        emit([{:asis, emod}, ':', {:asis, enc_func(etype)}, '(Val).', :nl])
    end
  end

  defp gen_encode_prim(erules, d) do
    value = {:var, :erlang.atom_to_list(:asn1ct_gen.mk_var(:asn1ct_name.curr(:val)))}
    gen_encode_prim(erules, d, value)
  end

  defp gen_encode_prim(r_gen(erule: :per, aligned: aligned), r_type() = d, value) do
    imm = gen_encode_prim_imm(value, d, aligned)
    :asn1ct_imm.enc_cg(imm, aligned)
  end

  def gen_encode_prim_imm(val, r_type(def: type0, constraint: constraint), aligned) do
    case simplify_type(type0) do
      :k_m_string ->
        type =
          case type0 do
            :GeneralizedTime ->
              :VisibleString

            :UTCTime ->
              :VisibleString

            _ ->
              type0
          end

        :asn1ct_imm.per_enc_k_m_string(val, type, constraint, aligned)

      :restricted_string ->
        toBinary = {:erlang, :iolist_to_binary}
        :asn1ct_imm.per_enc_restricted_string(val, toBinary, aligned)

      {:ENUMERATED, nNL} ->
        :asn1ct_imm.per_enc_enumerated(val, nNL, aligned)

      :INTEGER ->
        :asn1ct_imm.per_enc_integer(val, constraint, aligned)

      {:INTEGER, nNL} ->
        :asn1ct_imm.per_enc_integer(val, nNL, constraint, aligned)

      :REAL ->
        toBinary = {:real_common, :encode_real}
        :asn1ct_imm.per_enc_restricted_string(val, toBinary, aligned)

      {:"BIT STRING", nNL} ->
        case :asn1ct.use_legacy_types() do
          false ->
            :asn1ct_imm.per_enc_bit_string(val, nNL, constraint, aligned)

          true ->
            :asn1ct_imm.per_enc_legacy_bit_string(val, nNL, constraint, aligned)
        end

      :NULL ->
        :asn1ct_imm.per_enc_null(val, aligned)

      :"OBJECT IDENTIFIER" ->
        toBinary = {:per_common, :encode_oid}
        :asn1ct_imm.per_enc_restricted_string(val, toBinary, aligned)

      :"RELATIVE-OID" ->
        toBinary = {:per_common, :encode_relative_oid}
        :asn1ct_imm.per_enc_restricted_string(val, toBinary, aligned)

      :BOOLEAN ->
        :asn1ct_imm.per_enc_boolean(val, aligned)

      :"OCTET STRING" ->
        case :asn1ct.use_legacy_types() do
          false ->
            :asn1ct_imm.per_enc_octet_string(val, constraint, aligned)

          true ->
            :asn1ct_imm.per_enc_legacy_octet_string(val, constraint, aligned)
        end

      :ASN1_OPEN_TYPE ->
        case constraint do
          [r_Externaltypereference(type: tname)] ->
            encFunc = enc_func(tname)
            imm = [{:apply, {:local, encFunc, []}, [val]}]
            :asn1ct_imm.per_enc_open_type(imm, aligned)

          [] ->
            imm = [{:call, :erlang, :iolist_to_binary, [val]}]
            :asn1ct_imm.per_enc_open_type(imm, aligned)
        end
    end
  end

  defp dec_func(tname) do
    :erlang.list_to_atom(:lists.concat(['dec_', tname]))
  end

  defp enc_func(tname) do
    :erlang.list_to_atom(:lists.concat(['enc_', tname]))
  end

  defp simplify_type(type) do
    case type do
      :BMPString ->
        :k_m_string

      :IA5String ->
        :k_m_string

      :NumericString ->
        :k_m_string

      :PrintableString ->
        :k_m_string

      :VisibleString ->
        :k_m_string

      :UniversalString ->
        :k_m_string

      :GeneralizedTime ->
        :k_m_string

      :UTCTime ->
        :k_m_string

      :TeletexString ->
        :restricted_string

      :T61String ->
        :restricted_string

      :VideotexString ->
        :restricted_string

      :GraphicString ->
        :restricted_string

      :GeneralString ->
        :restricted_string

      :UTF8String ->
        :restricted_string

      :ObjectDescriptor ->
        :restricted_string

      other ->
        other
    end
  end

  def gen_obj_code(_Erules, _Module, r_typedef()) do
    :ok
  end

  def gen_objectset_code(_Erules, _ObjSet) do
    :ok
  end

  def gen_decode(erules, r_typedef() = type) do
    decFunc = dec_func(r_typedef(type, :name))
    emit([:nl, :nl, {:asis, decFunc}, '(Bytes) ->', :nl])
    gen_decode_user(erules, type)
  end

  def gen_decode(erules, tname, r_ComponentType(name: cname, typespec: type)) do
    newTname = [cname | tname]
    gen_decode(erules, newTname, type)
  end

  def gen_decode(erules, typename, type)
      when elem(type, 0) === :type do
    innerType = :asn1ct_gen.get_inner(r_type(type, :def))

    case :asn1ct_gen.type(innerType) do
      {:constructed, :bif} ->
        objFun =
          case r_type(type, :tablecinf) do
            [{:objfun, _} | _R] ->
              ', ObjFun'

            _ ->
              ''
          end

        emit([
          :nl,
          {:asis, dec_func(:asn1ct_gen.list2name(typename))},
          '(Bytes',
          objFun,
          ') ->',
          :nl
        ])

        :asn1ct_gen.gen_decode_constructed(erules, typename, innerType, type)

      _ ->
        true
    end
  end

  defp gen_decode_user(erules, d) when elem(d, 0) === :typedef do
    typename = [r_typedef(d, :name)]
    def__ = r_typedef(d, :typespec)
    innerType = :asn1ct_gen.get_inner(r_type(def__, :def))

    case :asn1ct_gen.type(innerType) do
      {:primitive, :bif} ->
        gen_dec_prim(erules, def__, 'Bytes')
        emit(['.', :nl, :nl])

      :ASN1_OPEN_TYPE ->
        gen_dec_prim(erules, r_type(def__, def: :ASN1_OPEN_TYPE), 'Bytes')
        emit(['.', :nl, :nl])

      {:constructed, :bif} ->
        :asn1ct_gen.gen_decode_constructed(erules, typename, innerType, d)

      r_Externaltypereference() = etype ->
        gen_dec_external(etype, 'Bytes')
        emit(['.', :nl, :nl])
    end
  end

  def gen_dec_external(ext, bytesVar) do
    currMod = :erlang.get(:currmod)
    r_Externaltypereference(module: mod, type: type) = ext

    emit([
      case currMod do
        ^mod ->
          []

        _ ->
          [{:asis, mod}, ':']
      end,
      {:asis, dec_func(type)},
      '(',
      bytesVar,
      ')'
    ])
  end

  def gen_dec_imm(
        r_gen(erule: :per, aligned: aligned),
        r_type(def: name, constraint: c)
      ) do
    gen_dec_imm_1(name, c, aligned)
  end

  defp gen_dec_imm_1(:ASN1_OPEN_TYPE, constraint, aligned) do
    imm_decode_open_type(constraint, aligned)
  end

  defp gen_dec_imm_1({:"BIT STRING", nNL}, constr0, aligned) do
    constr =
      :asn1ct_imm.effective_constraint(
        :bitstring,
        constr0
      )

    imm = :asn1ct_imm.per_dec_raw_bitstring(constr, aligned)

    case nNL do
      [] ->
        case :asn1ct.get_bit_string_format() do
          :compact ->
            gen_dec_bit_string(:decode_compact_bit_string, imm)

          :legacy ->
            gen_dec_bit_string(:decode_legacy_bit_string, imm)

          :bitstring ->
            gen_dec_copy_bitstring(imm)
        end

      [_ | _] ->
        d = fn v, buf ->
          as = [v, {:asis, nNL}]
          call = {:call, :per_common, :decode_named_bit_string, as}
          emit(['{', call, :com, buf, '}'])
        end

        {:call, d, imm}
    end
  end

  defp gen_dec_imm_1(:NULL, _Constr, _Aligned) do
    {:value, :NULL}
  end

  defp gen_dec_imm_1(:BOOLEAN, _Constr, _Aligned) do
    :asn1ct_imm.per_dec_boolean()
  end

  defp gen_dec_imm_1({:ENUMERATED, {base, ext}}, _Constr, aligned) do
    :asn1ct_imm.per_dec_enumerated(base, ext, aligned)
  end

  defp gen_dec_imm_1({:ENUMERATED, namedNumberList}, _Constr, aligned) do
    :asn1ct_imm.per_dec_enumerated(namedNumberList, aligned)
  end

  defp gen_dec_imm_1(:INTEGER, constr, aligned) do
    :asn1ct_imm.per_dec_integer(constr, aligned)
  end

  defp gen_dec_imm_1({:INTEGER, namedNumberList}, constraint, aligned) do
    :asn1ct_imm.per_dec_named_integer(constraint, namedNumberList, aligned)
  end

  defp gen_dec_imm_1(:BMPString = type, constraint, aligned) do
    gen_dec_k_m_string(type, constraint, aligned)
  end

  defp gen_dec_imm_1(:NumericString = type, constraint, aligned) do
    gen_dec_k_m_string(type, constraint, aligned)
  end

  defp gen_dec_imm_1(:PrintableString = type, constraint, aligned) do
    gen_dec_k_m_string(type, constraint, aligned)
  end

  defp gen_dec_imm_1(:VisibleString = type, constraint, aligned) do
    gen_dec_k_m_string(type, constraint, aligned)
  end

  defp gen_dec_imm_1(:IA5String = type, constraint, aligned) do
    gen_dec_k_m_string(type, constraint, aligned)
  end

  defp gen_dec_imm_1(:UniversalString = type, constraint, aligned) do
    gen_dec_k_m_string(type, constraint, aligned)
  end

  defp gen_dec_imm_1(:UTCTime, constraint, aligned) do
    gen_dec_k_m_string(:VisibleString, constraint, aligned)
  end

  defp gen_dec_imm_1(:GeneralizedTime, constraint, aligned) do
    gen_dec_k_m_string(:VisibleString, constraint, aligned)
  end

  defp gen_dec_imm_1(:"OCTET STRING", constraint, aligned) do
    szConstr =
      :asn1ct_imm.effective_constraint(
        :bitstring,
        constraint
      )

    imm =
      :asn1ct_imm.per_dec_octet_string(
        szConstr,
        aligned
      )

    case :asn1ct.use_legacy_types() do
      false ->
        {:convert, {:binary, :copy}, imm}

      true ->
        {:convert, :binary_to_list, imm}
    end
  end

  defp gen_dec_imm_1(:TeletexString, _Constraint, aligned) do
    gen_dec_restricted_string(aligned)
  end

  defp gen_dec_imm_1(:T61String, _Constraint, aligned) do
    gen_dec_restricted_string(aligned)
  end

  defp gen_dec_imm_1(:VideotexString, _Constraint, aligned) do
    gen_dec_restricted_string(aligned)
  end

  defp gen_dec_imm_1(:GraphicString, _Constraint, aligned) do
    gen_dec_restricted_string(aligned)
  end

  defp gen_dec_imm_1(:GeneralString, _Constraint, aligned) do
    gen_dec_restricted_string(aligned)
  end

  defp gen_dec_imm_1(:ObjectDescriptor, _Constraint, aligned) do
    gen_dec_restricted_string(aligned)
  end

  defp gen_dec_imm_1(:"OBJECT IDENTIFIER", _Constraint, aligned) do
    dec = fn v, buf ->
      emit(['{', {:call, :per_common, :decode_oid, [v]}, :com, buf, '}'])
    end

    {:call, dec, gen_dec_restricted_string(aligned)}
  end

  defp gen_dec_imm_1(:"RELATIVE-OID", _Constraint, aligned) do
    dec = fn v, buf ->
      emit(['{', {:call, :per_common, :decode_relative_oid, [v]}, :com, buf, '}'])
    end

    {:call, dec, gen_dec_restricted_string(aligned)}
  end

  defp gen_dec_imm_1(:UTF8String, _Constraint, aligned) do
    :asn1ct_imm.per_dec_restricted_string(aligned)
  end

  defp gen_dec_imm_1(:REAL, _Constraint, aligned) do
    :asn1ct_imm.per_dec_real(aligned)
  end

  defp gen_dec_bit_string(f, imm) do
    d = fn v, buf ->
      emit(['{', {:call, :per_common, f, [v]}, :com, buf, '}'])
    end

    {:call, d, imm}
  end

  defp gen_dec_copy_bitstring(imm) do
    d = fn v, buf ->
      emit(['{list_to_bitstring([', v, ']),', buf, '}'])
    end

    {:call, d, imm}
  end

  defp gen_dec_k_m_string(type, constraint, aligned) do
    :asn1ct_imm.per_dec_k_m_string(type, constraint, aligned)
  end

  defp gen_dec_restricted_string(aligned) do
    imm = :asn1ct_imm.per_dec_restricted_string(aligned)
    {:convert, :binary_to_list, imm}
  end

  def gen_dec_prim(erule, type, bytesVar) do
    imm = gen_dec_imm(erule, type)
    :asn1ct_imm.dec_code_gen(imm, bytesVar)
  end

  def extaddgroup2sequence(extList) do
    extaddgroup2sequence(extList, 0, [])
  end

  defp extaddgroup2sequence([{:ExtensionAdditionGroup, number0} | t], extNum, acc) do
    number =
      case number0 do
        :undefined ->
          1

        _ ->
          number0
      end

    {extGroupComps,
     [
       :ExtensionAdditionGroupEnd
       | t2
     ]} =
      :lists.splitwith(
        fn elem ->
          elem(elem, 0) === :ComponentType
        end,
        t
      )

    extaddgroup2sequence(t2, extNum + 1, [
      r_ComponentType(
        name: :erlang.list_to_atom('ExtAddGroup' ++ :erlang.integer_to_list(extNum + 1)),
        typespec:
          r_type(
            def:
              r_SEQUENCE(
                extaddgroup: number,
                components: extGroupComps
              )
          ),
        prop: :OPTIONAL
      )
      | acc
    ])
  end

  defp extaddgroup2sequence([c | t], extNum, acc) do
    extaddgroup2sequence(t, extNum, [c | acc])
  end

  defp extaddgroup2sequence([], _, acc) do
    :lists.reverse(acc)
  end

  defp imm_decode_open_type([r_Externaltypereference(type: tname)], aligned) do
    imm_dec_open_type_1(tname, aligned)
  end

  defp imm_decode_open_type([r_type(def: r_Externaltypereference(type: tname))], aligned) do
    imm_dec_open_type_1(tname, aligned)
  end

  defp imm_decode_open_type(_, aligned) do
    :asn1ct_imm.per_dec_open_type(aligned)
  end

  defp imm_dec_open_type_1(type, aligned) do
    d = fn openType, buf ->
      :asn1ct_name.new(:tmpval)

      emit([
        'begin',
        :nl,
        '{',
        {:curr, :tmpval},
        ',_} = ',
        {:asis, dec_func(type)},
        '(',
        openType,
        '),',
        :nl,
        '{',
        {:curr, :tmpval},
        :com,
        buf,
        '}',
        :nl,
        'end'
      ])
    end

    {:call, d, :asn1ct_imm.per_dec_open_type(aligned)}
  end
end
