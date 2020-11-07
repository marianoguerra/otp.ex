defmodule :m_asn1ct_check do
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

  Record.defrecord(:r_newt, :newt,
    type: :unchanged,
    tag: :unchanged,
    constraint: :unchanged,
    inlined: :no
  )

  def check(
        s,
        {types, values, parameterizedTypes, classes, objects, objectSets}
      ) do
    tupleIs = fn
      {t, _}, t ->
        true

      _, _ ->
        false
    end

    isClass = fn x ->
      tupleIs.(x, :asn1_class)
    end

    isObjSet = fn x ->
      tupleIs.(x, :objectsetdef)
    end

    isPObjSet = fn x ->
      tupleIs.(x, :pobjectsetdef)
    end

    isObject = fn x ->
      tupleIs.(x, :objectdef)
    end

    isValueSet = fn x ->
      tupleIs.(x, :valueset)
    end

    element2 = fn x ->
      :erlang.element(2, x)
    end

    element1 = fn x ->
      :erlang.element(1, x)
    end

    save_asn1db_uptodate(s, r_state(s, :erule), r_state(s, :mname))
    :erlang.put(:top_module, r_state(s, :mname))
    paramError = checkp(s, parameterizedTypes)
    :asn1ct_table.new(:parameterized_objects)
    :asn1ct_table.new(:inlined_objects)
    terror = checkt(s, types)
    :no_debug

    {pObjSetNames1, terror2} =
      filter_errors(
        isPObjSet,
        terror
      )

    verror = checkv(s, values ++ objectSets)
    :no_debug
    {addClasses, terror3} = filter_errors(isClass, terror2)
    newClasses = classes ++ addClasses
    cerror = checkc(s, newClasses)
    :no_debug
    {objSetNames, verror2} = filter_errors(isObjSet, verror)

    {pObjSetNames, verror3} =
      filter_errors(
        isPObjSet,
        verror2
      )

    {objectNames, verror4} =
      filter_errors(
        isObject,
        verror3
      )

    newObjects = objects ++ objectNames
    newObjectSets = objSetNames ++ pObjSetNames ++ pObjSetNames1

    {valueSetNames, verror5} =
      filter_errors(
        isValueSet,
        verror4
      )

    {oerror, exclO, exclOS} = checko(s, newObjects ++ newObjectSets, [], [], [])
    :no_debug
    inlinedObjTuples = :asn1ct_table.to_list(:inlined_objects)
    inlinedObjects = :lists.map(element2, inlinedObjTuples)
    :asn1ct_table.delete(:inlined_objects)
    parameterizedElems = :asn1ct_table.to_list(:parameterized_objects)

    parObjectSets =
      :lists.filter(
        fn
          {_OSName, :objectset, _} ->
            true

          _ ->
            false
        end,
        parameterizedElems
      )

    parObjectSetNames = :lists.map(element1, parObjectSets)

    parTypes =
      :lists.filter(
        fn
          {_TypeName, :type, _} ->
            true

          _ ->
            false
        end,
        parameterizedElems
      )

    parTypesNames = :lists.map(element1, parTypes)
    :asn1ct_table.delete(:parameterized_objects)
    :erlang.put(:asn1_reference, :undefined)
    exporterror = check_exports(s, r_state(s, :module))
    importError = check_imports(s, r_state(s, :module))

    allErrors =
      :lists.flatten([paramError, terror3, verror5, cerror, oerror, exporterror, importError])

    case allErrors do
      [] ->
        contextSwitchTs = context_switch_in_spec()
        instanceOf = instance_of_in_spec(r_state(s, :mname))

        newTypes =
          :lists.subtract(
            types,
            addClasses
          ) ++ contextSwitchTs ++ instanceOf ++ parTypesNames

        newValues =
          :lists.subtract(
            values,
            pObjSetNames ++ objectNames ++ valueSetNames
          )

        {:ok, {newTypes, newValues, parameterizedTypes, newClasses, newObjects, newObjectSets},
         {newTypes, newValues, parameterizedTypes, newClasses,
          :lists.subtract(newObjects, exclO) ++ inlinedObjects,
          :lists.subtract(
            newObjectSets,
            exclOS
          ) ++ parObjectSetNames}}

      _ ->
        {:error, allErrors}
    end
  end

  defp context_switch_in_spec() do
    l = [
      {:external, :EXTERNAL},
      {:embedded_pdv, :"EMBEDDED PDV"},
      {:character_string, :"CHARACTER STRING"}
    ]

    f = fn {t, tName}, acc ->
      case :erlang.get(t) do
        :generate ->
          :erlang.erase(t)
          [tName | acc]

        _ ->
          acc
      end
    end

    :lists.foldl(f, [], l)
  end

  defp instance_of_in_spec(modName) do
    case :erlang.get(:instance_of) do
      l when is_list(l) ->
        case :lists.member(modName, l) do
          true ->
            :erlang.erase(:instance_of)
            [:"INSTANCE OF"]

          _ ->
            :erlang.erase(:instance_of)
            []
        end

      _ ->
        []
    end
  end

  defp instance_of_decl(modName) do
    mods = get_instance_of()

    case :lists.member(modName, mods) do
      true ->
        :ok

      _ ->
        :erlang.put(:instance_of, [modName | mods])
    end
  end

  defp get_instance_of() do
    case :erlang.get(:instance_of) do
      :undefined ->
        []

      l ->
        l
    end
  end

  defp put_once(t, state) do
    case :erlang.get(t) do
      prevS when prevS > state ->
        :erlang.put(t, state)

      _ ->
        :ok
    end
  end

  defp filter_errors(pred, errorList) do
    element2 = fn x ->
      :erlang.element(2, x)
    end

    removedTupleElements = :lists.filter(pred, errorList)

    removedNames =
      :lists.map(
        element2,
        removedTupleElements
      )

    restErrors =
      :lists.subtract(
        errorList,
        removedTupleElements
      )

    {removedNames, restErrors}
  end

  defp check_exports(s, module = r_module()) do
    case r_module(module, :exports) do
      {:exports, []} ->
        []

      {:exports, :all} ->
        []

      {:exports, exportList} when is_list(exportList) ->
        isNotDefined = fn x ->
          try do
            _ = get_referenced_type(s, x)
            false
          catch
            {:error, _} ->
              true
          end
        end

        for ext = r_Externaltypereference(type: undef) <- exportList,
            isNotDefined.(ext) do
          return_asn1_error(s, ext, {:undefined_export, undef})
        end
    end
  end

  defp check_imports(s, r_module(imports: {:imports, imports})) do
    check_imports_1(s, imports, [])
  end

  defp check_imports_1(_S, [], acc) do
    acc
  end

  defp check_imports_1(s, [r_SymbolsFromModule(symbols: imports, module: moduleRef) | sFMs], acc) do
    module = name_of_def(moduleRef)

    refs =
      for ref <- imports do
        {try do
           get_referenced_type(s, ref)
         catch
           error ->
             error
         end, ref}
      end

    createError = fn ref ->
      error = {:undefined_import, name_of_def(ref), module}
      return_asn1_error(s, ref, error)
    end

    errors =
      for {{:error, _}, ref} <- refs do
        createError.(ref)
      end

    check_imports_1(s, sFMs, errors ++ acc)
  end

  defp checkt(s0, names) do
    check = &do_checkt/3
    types = check_fold(s0, names, check)
    ctxtSwitch = check_contextswitchingtypes(s0, [])
    check_fold(s0, :lists.reverse(ctxtSwitch), check) ++ types
  end

  defp do_checkt(s, name, r_typedef(typespec: typeSpec) = type0) do
    newS = r_state(s, tname: name)

    try do
      check_type(newS, type0, typeSpec)
    catch
      {:error, reason} ->
        reason

      {:asn1_class, _ClassDef} ->
        {:asn1_class, name}

      :pobjectsetdef ->
        {:pobjectsetdef, name}

      :pvalueset ->
        {:pvalueset, name}
    else
      r_type() = ts ->
        case r_typedef(type0, :checked) do
          true ->
            :ok

          _ ->
            type = r_typedef(type0, checked: true, typespec: ts)
            :asn1_db.dbput(r_state(newS, :mname), name, type)
            :ok
        end
    end
  end

  defp check_contextswitchingtypes(s, acc) do
    cSTList = [
      {:external, :EXTERNAL},
      {:embedded_pdv, :"EMBEDDED PDV"},
      {:character_string, :"CHARACTER STRING"}
    ]

    check_contextswitchingtypes(s, cSTList, acc)
  end

  defp check_contextswitchingtypes(s, [{t, tName} | ts], acc) do
    case :erlang.get(t) do
      :unchecked ->
        :erlang.put(t, :generate)
        check_contextswitchingtypes(s, ts, [tName | acc])

      _ ->
        check_contextswitchingtypes(s, ts, acc)
    end
  end

  defp check_contextswitchingtypes(_, [], acc) do
    acc
  end

  defp checkv(s, names) do
    check_fold(s, names, &do_checkv/3)
  end

  defp do_checkv(s, name, value)
       when elem(value, 0) === :valuedef or
              elem(value, 0) === :typedef or
              elem(value, 0) === :pvaluedef or
              elem(value, 0) === :pvaluesetdef do
    try do
      check_value(s, value)
    catch
      {:error, reason} ->
        reason

      {:pobjectsetdef} ->
        {:pobjectsetdef, name}

      {:objectsetdef} ->
        {:objectsetdef, name}

      {:asn1_class, _} ->
        r_valuedef(checked: c, pos: pos, name: n, type: type, value: def__) = value
        className = r_type(type, :def)
        newSpec = r_Object(classname: className, def: def__)
        newDef = r_typedef(checked: c, pos: pos, name: n, typespec: newSpec)
        :asn1_db.dbput(r_state(s, :mname), name, newDef)
        {:objectdef, name}
    else
      {:valueset, vSet} ->
        pos = :asn1ct.get_pos_of_def(value)
        checkedVSDef = r_typedef(checked: true, pos: pos, name: name, typespec: vSet)
        :asn1_db.dbput(r_state(s, :mname), name, checkedVSDef)
        {:valueset, name}

      v ->
        :asn1_db.dbput(r_state(s, :mname), name, v)
        :ok
    end
  end

  defp checkp(s, names) do
    check_fold(s, names, &do_checkp/3)
  end

  defp do_checkp(s0, name, r_ptypedef(typespec: typeSpec) = type0) do
    s = r_state(s0, tname: name)

    try do
      check_ptype(s, type0, typeSpec)
    catch
      {:error, reason} ->
        reason

      {:asn1_class, _ClassDef} ->
        {:asn1_class, name}

      {:asn1_param_class, _} ->
        :ok
    else
      r_type() = ts ->
        type = r_ptypedef(type0, checked: true, typespec: ts)
        :asn1_db.dbput(r_state(s, :mname), name, type)
        :ok
    end
  end

  defp checkc(s, names) do
    check_fold(s, names, &do_checkc/3)
  end

  defp do_checkc(s, name, class) do
    try do
      case is_classname(name) do
        false ->
          asn1_error(s, {:illegal_class_name, name})

        true ->
          do_checkc_1(s, name, class)
      end
    catch
      {:error, reason} ->
        reason
    end
  end

  defp do_checkc_1(s, name, r_classdef() = class) do
    c = check_class(s, class)
    store_class(s, true, r_classdef(class, typespec: c), name)
    :ok
  end

  defp do_checkc_1(s, name, r_typedef(typespec: r_type(def: def__) = tS)) do
    c = check_class(s, tS)

    {mod, pos} =
      case def__ do
        r_Externaltypereference(module: m, pos: p) ->
          {m, p}

        {:pt, r_Externaltypereference(module: m, pos: p), _} ->
          {m, p}
      end

    class = r_classdef(name: name, typespec: c, pos: pos, module: mod)
    store_class(s, true, class, name)
    :ok
  end

  defp is_classname(name) when is_atom(name) do
    :lists.all(
      fn
        ?- ->
          true

        d when ?0 <= d and d <= ?9 ->
          true

        uC when ?A <= uC and uC <= ?Z ->
          true

        _ ->
          false
      end,
      :erlang.atom_to_list(name)
    )
  end

  defp checko(s0, [name | os], acc, exclO, exclOS) do
    item = :asn1_db.dbget(r_state(s0, :mname), name)
    s = r_state(s0, error_context: item)

    try do
      checko_1(s, item, name, exclO, exclOS)
    catch
      {:error, error} ->
        checko(s, os, [error | acc], exclO, exclOS)
    else
      {newExclO, newExclOS} ->
        checko(s, os, acc, newExclO, newExclOS)
    end
  end

  defp checko(_S, [], acc, exclO, exclOS) do
    {:lists.reverse(acc), :lists.reverse(exclO), :lists.reverse(exclOS)}
  end

  defp checko_1(s, r_typedef(typespec: tS) = object, name, exclO, exclOS) do
    newS = r_state(s, tname: name)
    o = check_object(newS, object, tS)
    newObj = r_typedef(object, checked: true, typespec: o)
    :asn1_db.dbput(r_state(newS, :mname), name, newObj)

    case o do
      r_Object(gen: true) ->
        {exclO, exclOS}

      r_Object(gen: false) ->
        {[name | exclO], exclOS}

      r_ObjectSet(gen: true) ->
        {exclO, exclOS}

      r_ObjectSet(gen: false) ->
        {exclO, [name | exclOS]}
    end
  end

  defp checko_1(s, r_pobjectdef() = pObject, name, exclO, exclOS) do
    newS = r_state(s, tname: name)
    pO = check_pobject(newS, pObject)
    newPObj = r_pobjectdef(pObject, def: pO)
    :asn1_db.dbput(r_state(newS, :mname), name, newPObj)
    {[name | exclO], exclOS}
  end

  defp checko_1(s, r_pvaluesetdef() = pObjSet, name, exclO, exclOS) do
    newS = r_state(s, tname: name)
    pOS = check_pobjectset(newS, pObjSet)
    :asn1_db.dbput(r_state(newS, :mname), name, pOS)
    {exclO, [name | exclOS]}
  end

  defp check_class(
         s,
         cDef = r_classdef(checked: ch, name: name, typespec: tS)
       ) do
    case ch do
      true ->
        tS

      :idle ->
        tS

      _ ->
        store_class(s, :idle, cDef, name)
        checkedTS = check_class(s, tS)
        store_class(s, true, r_classdef(cDef, typespec: checkedTS), name)
        checkedTS
    end
  end

  defp check_class(s = r_state(mname: m, tname: t), classSpec)
       when elem(classSpec, 0) === :type do
    def__ = r_type(classSpec, :def)

    case def__ do
      r_Externaltypereference(module: ^m, type: ^t) ->
        r_objectclass(fields: def__)

      tref = r_Externaltypereference(type: tName) ->
        {mName, refType} = get_referenced_type(s, tref)
        r_classdef() = cD = get_class_def(s, refType)
        newState = update_state(r_state(s, tname: tName), mName)
        check_class(newState, cD)

      {:pt, classRef, params} ->
        {_, pClassDef} = get_referenced_type(s, classRef)
        newParaList = match_parameters(s, params)
        instantiate_pclass(s, pClassDef, newParaList)
    end
  end

  defp check_class(s, r_objectclass() = c) do
    check_objectclass(s, c)
  end

  defp check_class(s, className) do
    {refMod, def__} = get_referenced_type(s, className)

    case def__ do
      classDef when elem(classDef, 0) === :classdef ->
        case r_classdef(classDef, :checked) do
          true ->
            r_classdef(classDef, :typespec)

          :idle ->
            r_classdef(classDef, :typespec)

          false ->
            name = r_Externaltypereference(className, :type)
            store_class(s, :idle, classDef, name)
            newS = update_state(r_state(s, tname: name), refMod)
            checkedTS = check_class(newS, r_classdef(classDef, :typespec))
            store_class(s, true, r_classdef(classDef, typespec: checkedTS), name)
            checkedTS
        end

      typeDef when elem(typeDef, 0) === :typedef ->
        case r_typedef(typeDef, :typespec) do
          r_type(def: ext) when elem(ext, 0) === :Externaltypereference ->
            check_class(s, ext)
        end
    end
  end

  defp check_objectclass(s, r_objectclass(fields: fs0, syntax: syntax0) = c) do
    fs = check_class_fields(s, fs0)

    case syntax0 do
      {:"WITH SYNTAX", syntax1} ->
        syntax = preprocess_syntax(s, syntax1, fs)

        r_objectclass(c,
          fields: fs,
          syntax: {:preprocessed_syntax, syntax}
        )

      _ ->
        r_objectclass(c, fields: fs)
    end
  end

  defp instantiate_pclass(s = r_state(parameters: _OldArgs), pClassDef, params) do
    r_ptypedef(args: args, typespec: type) = pClassDef
    matchedArgs = match_args(s, args, params, [])
    newS = r_state(s, parameters: matchedArgs, abscomppath: [])
    check_class(newS, r_classdef(name: r_state(s, :tname), typespec: type))
  end

  defp store_class(s, mode, classDef, className) do
    newCDef = r_classdef(classDef, checked: mode)
    :asn1_db.dbput(r_state(s, :mname), className, newCDef)
  end

  defp check_class_fields(s, fields) do
    check_class_fields(s, fields, [])
  end

  defp check_class_fields(s, [f | fields], acc) do
    newField =
      case :erlang.element(1, f) do
        :fixedtypevaluefield ->
          {_, name, type, unique, oSpec} = f

          case {unique, oSpec} do
            {:UNIQUE, {:DEFAULT, _}} ->
              asn1_error(s, {:unique_and_default, name})

            {_, _} ->
              :ok
          end

          refType = check_type(s, r_typedef(typespec: type), type)
          {:fixedtypevaluefield, name, refType, unique, oSpec}

        :object_or_fixedtypevalue_field ->
          {_, name, type, unique, oSpec} = f
          type2 = maybe_unchecked_OCFT(s, type)

          cat =
            case :asn1ct_gen.type(:asn1ct_gen.get_inner(r_type(type2, :def))) do
              def__
              when elem(def__, 0) === :Externaltypereference ->
                {_, d} = get_referenced_type(s, def__, true)
                d

              {:undefined, :user} ->
                {_, d} =
                  get_referenced_type(
                    s,
                    r_Externaltypereference(
                      module: r_state(s, :mname),
                      type: r_type(type, :def)
                    )
                  )

                d

              _ ->
                type
            end

          case cat do
            class when elem(class, 0) === :classdef ->
              toExt = fn
                r_type(def: cE = r_Externaltypereference()) ->
                  cE

                t ->
                  t
              end

              {:objectfield, name, toExt.(type), unique, oSpec}

            _ ->
              refType = check_type(s, r_typedef(typespec: type), type)
              {:fixedtypevaluefield, name, refType, unique, oSpec}
          end

        :objectset_or_fixedtypevalueset_field ->
          {_, name, type, oSpec} = f

          refType =
            try do
              check_type(s, r_typedef(typespec: type), type)
            catch
              {:asn1_class, _ClassDef} ->
                case if_current_checked_type(s, type) do
                  true ->
                    r_type(type, :def)

                  _ ->
                    check_class(s, type)
                end
            else
              r_type() = checkedType ->
                checkedType
            end

          cond do
            elem(refType, 0) === :Externaltypereference ->
              {:objectsetfield, name, type, oSpec}

            elem(refType, 0) === :classdef ->
              {:objectsetfield, name, type, oSpec}

            elem(refType, 0) === :objectclass ->
              {:objectsetfield, name, type, oSpec}

            true ->
              {:fixedtypevaluesetfield, name, refType, oSpec}
          end

        :typefield ->
          case f do
            {tF, name, {:DEFAULT, type}} ->
              {tF, name, {:DEFAULT, check_type(s, r_typedef(typespec: type), type)}}

            _ ->
              f
          end

        _ ->
          f
      end

    check_class_fields(s, fields, [newField | acc])
  end

  defp check_class_fields(_S, [], acc) do
    :lists.reverse(acc)
  end

  defp maybe_unchecked_OCFT(s, type) do
    case r_type(type, :def) do
      r_ObjectClassFieldType(type: :undefined) ->
        check_type(s, r_typedef(typespec: type), type)

      _ ->
        type
    end
  end

  defp if_current_checked_type(s, r_type(def: def__)) do
    currentModule = r_state(s, :mname)
    currentCheckedName = r_state(s, :tname)
    mergedModules = r_state(s, :inputmodules)

    case def__ do
      r_Externaltypereference(module: ^currentModule, type: ^currentCheckedName) ->
        true

      r_Externaltypereference(module: moduleName, type: ^currentCheckedName) ->
        case mergedModules do
          :undefined ->
            false

          _ ->
            :lists.member(moduleName, mergedModules)
        end

      _ ->
        false
    end
  end

  defp check_pobject(_S, pObject)
       when elem(pObject, 0) === :pobjectdef do
    def__ = r_pobjectdef(pObject, :def)
    def__
  end

  defp check_pobjectset(s, pObjSet) do
    r_pvaluesetdef(pos: pos, name: name, args: args, type: type, valueset: valueSet) = pObjSet
    {mod, def__} = get_referenced_type(s, r_type(type, :def))

    case def__ do
      r_classdef() ->
        className =
          r_Externaltypereference(
            module: mod,
            type: get_datastr_name(def__)
          )

        {:valueset, set} = valueSet
        objectSet = r_ObjectSet(class: className, set: set)

        r_pobjectsetdef(
          pos: pos,
          name: name,
          args: args,
          class: r_type(type, :def),
          def: objectSet
        )

      _ ->
        pObjSet
    end
  end

  Record.defrecord(:r_osi, :osi,
    st: :undefined,
    classref: :undefined,
    uniq: :undefined,
    ext: :undefined
  )

  defp check_object(_S, objDef, objSpec)
       when r_typedef(objDef, :checked) == true do
    objSpec
  end

  defp check_object(s, _ObjDef, r_Object(classname: classRef, def: objectDef)) do
    :no_debug
    _ = check_externaltypereference(s, classRef)

    {classDef, newClassRef} =
      case get_referenced_type(s, classRef, true) do
        {mName, r_classdef(checked: false, name: cLName) = clDef} ->
          type = r_Externaltypereference(classRef, :type)

          newState =
            update_state(
              r_state(s, tname: type),
              mName
            )

          objClass = check_class(newState, clDef)

          {r_classdef(clDef,
             checked: true,
             typespec: objClass
           ), r_Externaltypereference(module: mName, type: cLName)}

        {mName, r_classdef(name: cLName) = clDef} ->
          {clDef, r_Externaltypereference(module: mName, type: cLName)}

        _ ->
          asn1_error(s, :illegal_object)
      end

    newObj =
      case objectDef do
        {:object, _, _} = def__ ->
          newSettingList = check_objectdefn(s, def__, classDef)
          r_Object(def: newSettingList)

        {:po, {:object, defObj}, argsList} ->
          {_, object} = get_referenced_type(s, defObj)
          instantiate_po(s, classDef, object, argsList)

        {:pv, {:simpledefinedvalue, objRef}, argList} ->
          {_, object} = get_referenced_type(s, objRef)
          instantiate_po(s, classDef, object, argList)

        r_Externalvaluereference() ->
          {_, object} = get_referenced_type(s, objectDef)
          check_object(s, object, object_to_check(s, object))

        [] ->
          def__ = {:object, :defaultsyntax, []}
          newSettingList = check_objectdefn(s, def__, classDef)
          r_Object(def: newSettingList)

        _ ->
          asn1_error(s, :illegal_object)
      end

    fields = r_objectclass(r_classdef(classDef, :typespec), :fields)
    gen = gen_incl(s, r_Object(newObj, :def), fields)
    r_Object(newObj, classname: newClassRef, gen: gen)
  end

  defp check_object(s, _, r_ObjectSet(class: classRef0, set: set0) = objSet0) do
    {_, classDef} = get_referenced_type(s, classRef0)
    classRef = check_externaltypereference(s, classRef0)

    {uniqueFieldName, uniqueInfo} =
      case get_unique_fieldname(
             s,
             classDef
           ) do
        :no_unique ->
          {{:unique, :undefined}, {:unique, :undefined}}

        other ->
          {:erlang.element(1, other), other}
      end

    oSI0 = r_osi(st: s, classref: classRef, uniq: uniqueInfo, ext: false)

    {set1, oSI1} =
      cond do
        is_list(set0) ->
          check_object_set_list(set0, oSI0)

        true ->
          check_object_set(set0, oSI0)
      end

    ext =
      case set1 do
        [] ->
          true

        [_ | _] ->
          r_osi(oSI1, :ext)
      end

    set2 = remove_duplicate_objects(s, set1)

    set =
      case ext do
        false ->
          set2

        true ->
          set2 ++ [:EXTENSIONMARK]
      end

    objSet =
      r_ObjectSet(objSet0,
        uniquefname: uniqueFieldName,
        set: set
      )

    gen = gen_incl_set(s, set, classDef)
    r_ObjectSet(objSet, class: classRef, gen: gen)
  end

  defp check_object_set({:element_set, root0, ext0}, oSI0) do
    oSI =
      case ext0 do
        :none ->
          oSI0

        _ ->
          r_osi(oSI0, ext: true)
      end

    case {root0, ext0} do
      {:empty, :empty} ->
        {[], oSI}

      {:empty, ext} ->
        check_object_set(ext, oSI)

      {root, :none} ->
        check_object_set(root, oSI)

      {root, :empty} ->
        check_object_set(root, oSI)

      {root, ext} ->
        check_object_set_list([root, ext], oSI)
    end
  end

  defp check_object_set(r_Externaltypereference() = ref, r_osi(st: s) = oSI) do
    {_, r_typedef(typespec: oSdef) = oS} =
      get_referenced_type(
        s,
        ref
      )

    objectSet = check_object(s, oS, oSdef)
    check_object_set_objset(objectSet, oSI)
  end

  defp check_object_set(r_Externalvaluereference() = ref, r_osi(st: s) = oSI) do
    {refedMod, objName, r_Object(def: def__)} = check_referenced_object(s, ref)
    objList = check_object_set_mk(refedMod, objName, def__, oSI)
    {objList, oSI}
  end

  defp check_object_set({:EXCEPT, incl0, excl0}, oSI) do
    {incl1, _} = check_object_set(incl0, oSI)
    {excl1, _} = check_object_set(excl0, oSI)

    exclude =
      :sofs.set(
        for {n, _} <- excl1 do
          n
        end,
        [:name]
      )

    incl2 =
      for {name, _, _} = obj <- incl1 do
        {name, obj}
      end

    incl3 = :sofs.relation(incl2, [{:name, :object}])
    incl4 = :sofs.drestriction(incl3, exclude)
    incl5 = :sofs.to_external(incl4)

    incl =
      for {_, obj} <- incl5 do
        obj
      end

    {incl, oSI}
  end

  defp check_object_set({:object, _, _} = obj0, oSI) do
    r_osi(st: s, classref: classRef) = oSI

    r_Object(def: def__) =
      check_object(s, r_typedef(typespec: obj0), r_Object(classname: classRef, def: obj0))

    objList = check_object_set_mk(def__, oSI)
    {objList, oSI}
  end

  defp check_object_set(
         r_ObjectClassFieldType(classname: objName, fieldname: fieldNames),
         r_osi(st: s) = oSI
       ) do
    set = check_ObjectSetFromObjects(s, objName, fieldNames)
    check_object_set_objset_list(set, oSI)
  end

  defp check_object_set(
         {:ObjectSetFromObjects, obj, fieldNames},
         r_osi(st: s) = oSI
       ) do
    objName = :erlang.element(tuple_size(obj), obj)
    set = check_ObjectSetFromObjects(s, objName, fieldNames)
    check_object_set_objset_list(set, oSI)
  end

  defp check_object_set({:pt, definedObjSet, paramList0}, oSI) do
    r_osi(st: s, classref: classRef) = oSI
    {_, pObjSetDef} = get_referenced_type(s, definedObjSet)
    paramList = match_parameters(s, paramList0)
    objectSet = instantiate_pos(s, classRef, pObjSetDef, paramList)
    check_object_set_objset(objectSet, oSI)
  end

  defp check_object_set(
         {:pos, {:objectset, _, definedObjSet}, params0},
         oSI
       ) do
    r_osi(st: s, classref: classRef) = oSI
    {_, pObjSetDef} = get_referenced_type(s, definedObjSet)
    params = match_parameters(s, params0)
    objectSet = instantiate_pos(s, classRef, pObjSetDef, params)
    check_object_set_objset(objectSet, oSI)
  end

  defp check_object_set(
         {:pv, {:simpledefinedvalue, definedObject}, params} = pV,
         oSI
       ) do
    r_osi(st: s, classref: classRef) = oSI
    args = match_parameters(s, params)

    r_Object(def: def__) =
      check_object(
        s,
        pV,
        r_Object(
          classname: classRef,
          def: {:po, {:object, definedObject}, args}
        )
      )

    objList = check_object_set_mk(def__, oSI)
    {objList, oSI}
  end

  defp check_object_set({:SingleValue, val}, oSI) do
    check_object_set(val, oSI)
  end

  defp check_object_set(
         {:ValueFromObject, {:object, object}, fieldNames},
         oSI
       ) do
    r_osi(st: s) = oSI

    case extract_field(s, object, fieldNames) do
      r_Object(def: def__) ->
        objList = check_object_set_mk(def__, oSI)
        {objList, oSI}

      _ ->
        asn1_error(s, :illegal_object)
    end
  end

  defp check_object_set(r_type(def: def__), oSI) do
    check_object_set(def__, oSI)
  end

  defp check_object_set({:union, a0, b0}, oSI0) do
    {a, oSI1} = check_object_set(a0, oSI0)
    {b, oSI} = check_object_set(b0, oSI1)
    {a ++ b, oSI}
  end

  defp check_object_set_list([h | t], oSI0) do
    {set0, oSI1} = check_object_set(h, oSI0)
    {set1, oSI2} = check_object_set_list(t, oSI1)
    {set0 ++ set1, oSI2}
  end

  defp check_object_set_list([], oSI) do
    {[], oSI}
  end

  defp check_object_set_objset(r_ObjectSet(set: set), oSI) do
    check_object_set_objset_list(set, oSI)
  end

  defp check_object_set_objset_list(set, oSI) do
    check_object_set_objset_list_1(set, oSI, [])
  end

  defp check_object_set_objset_list_1([:EXTENSIONMARK | t], oSI, acc) do
    check_object_set_objset_list_1(t, r_osi(oSI, ext: true), acc)
  end

  defp check_object_set_objset_list_1([h | t], oSI, acc) do
    check_object_set_objset_list_1(t, oSI, [h | acc])
  end

  defp check_object_set_objset_list_1([], oSI, acc) do
    {acc, oSI}
  end

  defp check_object_set_mk(fields, oSI) do
    check_object_set_mk(:no_mod, :no_name, fields, oSI)
  end

  defp check_object_set_mk(m, n, def__, r_osi(uniq: {:unique, :undefined})) do
    {_, _, fields} = def__
    [{{m, n}, :no_unique_value, fields}]
  end

  defp check_object_set_mk(m, n, def__, r_osi(uniq: {uniqField, _})) do
    {_, _, fields} = def__

    case :lists.keyfind(uniqField, 1, fields) do
      {^uniqField, r_valuedef(value: val)} ->
        [{{m, n}, val, fields}]

      false ->
        case fields do
          [{_, r_typedef(typespec: r_ObjectSet(set: [:EXTENSIONMARK]))}] ->
            []

          _ ->
            [{{m, n}, :no_unique_value, fields}]
        end
    end
  end

  defp remove_duplicate_objects(s, set0) when is_list(set0) do
    set1 =
      for {_, id, _} = orig <- set0 do
        {id, orig}
      end

    set2 = :sofs.relation(set1)
    set3 = :sofs.relation_to_family(set2)
    set = :sofs.to_external(set3)
    remove_duplicate_objects_1(s, set)
  end

  defp remove_duplicate_objects_1(s, [{:no_unique_value, objs} | t]) do
    objs ++ remove_duplicate_objects_1(s, t)
  end

  defp remove_duplicate_objects_1(s, [{_, [_] = objs} | t]) do
    objs ++ remove_duplicate_objects_1(s, t)
  end

  defp remove_duplicate_objects_1(s, [{id, [_ | _] = objs} | t]) do
    makeSortable = fn what ->
      sortable_type(s, what)
    end

    tagged = order_tag_set(objs, makeSortable)

    case :lists.ukeysort(1, tagged) do
      [{_, obj}] ->
        [obj | remove_duplicate_objects_1(s, t)]

      [_ | _] ->
        asn1_error(s, {:non_unique_object, id})
    end
  end

  defp remove_duplicate_objects_1(_, []) do
    []
  end

  defp order_tag_set([{_, _, fields} = orig | fs], fun) do
    pair =
      {for {fId, f} <- fields do
         {fId, traverse(f, fun)}
       end, orig}

    [pair | order_tag_set(fs, fun)]
  end

  defp order_tag_set([], _) do
    []
  end

  defp sortable_type(s, r_Externaltypereference() = eRef) do
    try do
      get_referenced_type(s, eRef)
    catch
      _, _ ->
        eRef
    else
      {_, r_typedef() = oI} ->
        r_typedef(oI, pos: :undefined, name: :undefined)
    end
  end

  defp sortable_type(_, r_typedef() = tD) do
    :asn1ct.unset_pos_mod(r_typedef(tD, name: :undefined))
  end

  defp sortable_type(_, type) do
    :asn1ct.unset_pos_mod(type)
  end

  defp traverse(structure0, fun) do
    structure = fun.(structure0)
    traverse_1(structure, fun)
  end

  defp traverse_1(r_typedef(typespec: tS0) = tD, fun) do
    tS = traverse(tS0, fun)
    r_typedef(tD, typespec: tS)
  end

  defp traverse_1(r_valuedef(type: tS0) = vD, fun) do
    tS = traverse(tS0, fun)
    r_valuedef(vD, type: tS)
  end

  defp traverse_1(r_type(def: tS0) = tD, fun) do
    tS = traverse(tS0, fun)
    r_type(tD, def: tS)
  end

  defp traverse_1(r_SEQUENCE(components: cs0) = seq, fun) do
    cs = traverse_seq_set(cs0, fun)
    r_SEQUENCE(seq, components: cs)
  end

  defp traverse_1({:"SEQUENCE OF", type0}, fun) do
    type = traverse(type0, fun)
    {:"SEQUENCE OF", type}
  end

  defp traverse_1({:"SET OF", type0}, fun) do
    type = traverse(type0, fun)
    {:"SET OF", type}
  end

  defp traverse_1(r_SET(components: cs0) = set, fun) do
    cs = traverse_seq_set(cs0, fun)
    r_SET(set, components: cs)
  end

  defp traverse_1({:CHOICE, cs0}, fun) do
    cs = traverse_seq_set(cs0, fun)
    {:CHOICE, cs}
  end

  defp traverse_1(leaf, _) do
    leaf
  end

  defp traverse_seq_set(list, fun) when is_list(list) do
    traverse_seq_set_1(list, fun)
  end

  defp traverse_seq_set({set, ext}, fun) do
    {traverse_seq_set_1(set, fun), traverse_seq_set_1(ext, fun)}
  end

  defp traverse_seq_set({set1, set2, set3}, fun) do
    {traverse_seq_set_1(set1, fun), traverse_seq_set_1(set2, fun), traverse_seq_set_1(set3, fun)}
  end

  defp traverse_seq_set_1([r_ComponentType() = cT0 | cs], fun) do
    cT = r_ComponentType(typespec: tS0) = fun.(cT0)
    tS = traverse(tS0, fun)
    [r_ComponentType(cT, typespec: tS) | traverse_seq_set_1(cs, fun)]
  end

  defp traverse_seq_set_1([{:"COMPONENTS OF", _} = cO0 | cs], fun) do
    {:"COMPONENTS OF", tS0} = fun.(cO0)
    tS = traverse(tS0, fun)
    [{:"COMPONENTS OF", tS} | traverse_seq_set_1(cs, fun)]
  end

  defp traverse_seq_set_1([], _) do
    []
  end

  defp object_to_check(_, r_typedef(typespec: objDef)) do
    objDef
  end

  defp object_to_check(s, r_valuedef(type: class, value: objectRef)) do
    case class do
      r_type(def: r_Externaltypereference() = def__) ->
        r_Object(classname: def__, def: objectRef)

      _ ->
        asn1_error(s, :illegal_object)
    end
  end

  defp check_referenced_object(s, objRef)
       when elem(objRef, 0) === :Externalvaluereference do
    case get_referenced_type(s, objRef) do
      {refedMod, objectDef}
      when elem(objectDef, 0) === :valuedef ->
        :no_debug
        r_type(def: classRef) = r_valuedef(objectDef, :type)
        def__ = r_valuedef(objectDef, :value)

        {refedMod, get_datastr_name(objectDef),
         check_object(
           update_state(s, refedMod),
           objectDef,
           r_Object(classname: classRef, def: def__)
         )}

      {refedMod, objectDef}
      when elem(objectDef, 0) === :typedef ->
        {refedMod, get_datastr_name(objectDef),
         check_object(update_state(s, refedMod), objectDef, r_typedef(objectDef, :typespec))}
    end
  end

  defp check_ObjectSetFromObjects(s, objName, fields) do
    {_, obj0} = get_referenced_type(s, objName)

    case check_object(s, obj0, r_typedef(obj0, :typespec)) do
      r_ObjectSet() = obj1 ->
        get_fieldname_set(s, obj1, fields)

      r_Object(classname: class, def: {:object, _, objFs}) ->
        objSet = r_ObjectSet(class: class, set: [{:_, :_, objFs}])
        get_fieldname_set(s, objSet, fields)
    end
  end

  defp get_type_from_object(s, object, fieldNames)
       when elem(object, 0) === :Externaltypereference or
              elem(object, 0) === :Externalvaluereference do
    extract_field(s, object, fieldNames)
  end

  defp get_value_from_object(s, def__, fieldNames) do
    case extract_field(s, def__, fieldNames) do
      r_valuedef(value: val) ->
        val

      {:valueset, _} = val ->
        val

      _ ->
        asn1_error(s, :illegal_value)
    end
  end

  defp extract_field(s, def0, fieldNames) do
    {_, def1} = get_referenced_type(s, def0)
    def2 = check_object(s, def1, r_typedef(def1, :typespec))
    def__ = r_typedef(def1, typespec: def2)
    get_fieldname_element(s, def__, fieldNames)
  end

  defp get_fieldname_element(s, object0, [{_RefType, fieldName} | fields]) do
    object =
      case object0 do
        r_typedef(typespec: r_Object(def: obj)) ->
          obj

        {_, _, _} = obj ->
          obj
      end

    case check_fieldname_element(s, fieldName, object) do
      r_Object(def: d) when fields !== [] ->
        get_fieldname_element(s, d, fields)

      r_ObjectSet() = set ->
        get_fieldname_set(s, set, fields)

      result when fields === [] ->
        result
    end
  end

  defp get_fieldname_element(_S, def__, []) do
    def__
  end

  defp get_fieldname_set(s, r_ObjectSet(set: set0), t) do
    get_fieldname_set_1(s, set0, t, [])
  end

  defp get_fieldname_set_1(s, [:EXTENSIONMARK = ext | t], fields, acc) do
    get_fieldname_set_1(s, t, fields, [ext | acc])
  end

  defp get_fieldname_set_1(s, [h | t], fields, acc) do
    try do
      get_fieldname_element(s, h, fields)
    catch
      {:error, _} ->
        get_fieldname_set_1(s, t, fields, acc)
    else
      l when is_list(l) ->
        get_fieldname_set_1(s, t, fields, l ++ acc)

      {:valueset, l} ->
        get_fieldname_set_1(s, t, fields, l ++ acc)

      other ->
        get_fieldname_set_1(s, t, fields, [other | acc])
    end
  end

  defp get_fieldname_set_1(_, [], _Fields, acc) do
    case acc do
      [r_valuedef() | _] ->
        {:valueset, acc}

      _ ->
        acc
    end
  end

  defp check_fieldname_element(s, name, {_, _, fields}) do
    case :lists.keyfind(name, 1, fields) do
      {^name, def__} ->
        check_fieldname_element_1(s, def__)

      false ->
        asn1_error(s, {:undefined_field, name})
    end
  end

  defp check_fieldname_element_1(s, r_typedef(typespec: ts) = tDef) do
    case ts do
      r_Object() ->
        check_object(s, tDef, ts)

      _ ->
        check_type(s, tDef, ts)
    end
  end

  defp check_fieldname_element_1(s, r_valuedef() = vDef) do
    try do
      check_value(s, vDef)
    catch
      {:asn1_class, _} ->
        r_valuedef(checked: c, pos: pos, name: n, type: type, value: def__) = vDef
        className = r_type(type, :def)
        newSpec = r_Object(classname: className, def: def__)
        newDef = r_typedef(checked: c, pos: pos, name: n, typespec: newSpec)
        check_fieldname_element_1(s, newDef)
    end
  end

  defp check_fieldname_element_1(_S, {:value_tag, val}) do
    r_valuedef(value: val)
  end

  defp check_fieldname_element_1(s, eref)
       when elem(eref, 0) === :Externaltypereference or
              elem(eref, 0) === :Externalvaluereference do
    {_, tDef} = get_referenced_type(s, eref)
    check_fieldname_element_1(s, tDef)
  end

  defp instantiate_po(s = r_state(parameters: _OldArgs), _ClassDef, object, argsList)
       when elem(object, 0) === :pobjectdef do
    formalParams = get_pt_args(object)
    matchedArgs = match_args(s, formalParams, argsList, [])
    newS = r_state(s, parameters: matchedArgs)

    check_object(
      newS,
      object,
      r_Object(classname: r_pobjectdef(object, :class), def: r_pobjectdef(object, :def))
    )
  end

  defp instantiate_pos(s = r_state(parameters: _OldArgs), classRef, objectSetDef, argsList) do
    formalParams = get_pt_args(objectSetDef)

    oSet =
      case get_pt_spec(objectSetDef) do
        {:valueset, set} ->
          r_ObjectSet(class: classRef, set: set)

        set when elem(set, 0) === :ObjectSet ->
          set

        _ ->
          asn1_error(s, :invalid_objectset)
      end

    matchedArgs = match_args(s, formalParams, argsList, [])
    newS = r_state(s, parameters: matchedArgs)
    check_object(newS, objectSetDef, oSet)
  end

  defp gen_incl(s, {_, _, fields}, cFields) do
    gen_incl1(s, fields, cFields)
  end

  defp gen_incl1(_, _, []) do
    false
  end

  defp gen_incl1(s, fields, [c | cFields]) do
    case :erlang.element(1, c) do
      :typefield ->
        true

      :objectfield ->
        case :lists.keysearch(:erlang.element(2, c), 1, fields) do
          {:value, field} ->
            classRef =
              case :erlang.element(3, c) do
                r_type(def: ref) ->
                  ref

                eref when elem(eref, 0) === :Externaltypereference ->
                  eref
              end

            classFields = get_objclass_fields(s, classRef)

            objDef =
              case :erlang.element(2, field) do
                tDef when elem(tDef, 0) === :typedef ->
                  check_object(s, tDef, r_typedef(tDef, :typespec))

                eRef ->
                  {_, t} = get_referenced_type(s, eRef)
                  check_object(s, t, object_to_check(s, t))
              end

            case gen_incl(s, r_Object(objDef, :def), classFields) do
              true ->
                true

              _ ->
                gen_incl1(s, fields, cFields)
            end

          _ ->
            gen_incl1(s, fields, cFields)
        end

      _ ->
        gen_incl1(s, fields, cFields)
    end
  end

  defp get_objclass_fields(s, eref = r_Externaltypereference()) do
    {_, classDef} = get_referenced_type(s, eref, true)
    get_objclass_fields(s, classDef)
  end

  defp get_objclass_fields(s, cD = r_classdef(typespec: r_Externaltypereference())) do
    get_objclass_fields(s, r_classdef(cD, :typespec))
  end

  defp get_objclass_fields(_, r_classdef(typespec: cDef))
       when elem(cDef, 0) === :objectclass do
    r_objectclass(cDef, :fields)
  end

  defp gen_incl_set(s, fields, r_typedef(typespec: r_type(def: eref)))
       when elem(eref, 0) === :Externaltypereference do
    {_, cDef} = get_referenced_type(s, eref)
    gen_incl_set(s, fields, cDef)
  end

  defp gen_incl_set(s, fields, classDef) do
    case get_unique_fieldname(s, classDef) do
      :no_unique ->
        false

      {_, _} ->
        gen_incl_set1(s, fields, r_objectclass(r_classdef(classDef, :typespec), :fields))
    end
  end

  defp gen_incl_set1(_, [], _CFields) do
    false
  end

  defp gen_incl_set1(_, [:EXTENSIONMARK], _) do
    true
  end

  defp gen_incl_set1(_, [:EXTENSIONMARK | _], _) do
    true
  end

  defp gen_incl_set1(s, [object | rest], cFields) do
    fields = :erlang.element(tuple_size(object), object)

    case gen_incl1(s, fields, cFields) do
      true ->
        true

      false ->
        gen_incl_set1(s, rest, cFields)
    end
  end

  defp check_objectdefn(s, def__, r_classdef(typespec: objClass)) do
    r_objectclass(syntax: syntax0, fields: classFields) = objClass

    case def__ do
      {:object, :defaultsyntax, fields} ->
        check_defaultfields(s, fields, classFields)

      {:object, :definedsyntax, fields} ->
        syntax = get_syntax(s, syntax0, classFields)

        case match_syntax(s, syntax, fields, []) do
          {:match, newFields, []} ->
            {:object, :defaultsyntax, newFields}

          {:match, _, [what | _]} ->
            syntax_match_error(s, what)

          {:nomatch, [what | _]} ->
            syntax_match_error(s, what)

          {:nomatch, []} ->
            syntax_match_error(s)
        end
    end
  end

  defp get_syntax(_, {:preprocessed_syntax, syntax}, _) do
    syntax
  end

  defp get_syntax(s, {:"WITH SYNTAX", syntax}, classFields) do
    preprocess_syntax(s, syntax, classFields)
  end

  defp preprocess_syntax(s, syntax0, cs) do
    syntax = preprocess_syntax_1(s, syntax0, cs, true)
    present0 = preprocess_get_fields(syntax, [])
    present1 = :lists.sort(present0)
    present = :ordsets.from_list(present1)

    case present === present1 do
      false ->
        dupl = present1 -- present
        asn1_error(s, {:syntax_duplicated_fields, dupl})

      true ->
        :ok
    end

    mandatory0 = get_mandatory_class_fields(cs)
    mandatory = :ordsets.from_list(mandatory0)

    case :ordsets.subtract(mandatory, present) do
      [] ->
        syntax

      [_ | _] = missing ->
        asn1_error(
          s,
          {:syntax_missing_mandatory_fields, missing}
        )
    end
  end

  defp preprocess_syntax_1(s, [h | t], cs, mandatory) when is_list(h) do
    [
      {:optional, preprocess_syntax_1(s, h, cs, false)}
      | preprocess_syntax_1(s, t, cs, mandatory)
    ]
  end

  defp preprocess_syntax_1(s, [{:valuefieldreference, name} | t], cs, mandatory) do
    f = preprocess_check_field(s, name, cs, mandatory)
    [f | preprocess_syntax_1(s, t, cs, mandatory)]
  end

  defp preprocess_syntax_1(s, [{:typefieldreference, name} | t], cs, mandatory) do
    f = preprocess_check_field(s, name, cs, mandatory)
    [f | preprocess_syntax_1(s, t, cs, mandatory)]
  end

  defp preprocess_syntax_1(s, [{token, _} | t], cs, mandatory)
       when is_atom(token) do
    [{:token, token} | preprocess_syntax_1(s, t, cs, mandatory)]
  end

  defp preprocess_syntax_1(s, [token | t], cs, mandatory)
       when is_atom(token) do
    [{:token, token} | preprocess_syntax_1(s, t, cs, mandatory)]
  end

  defp preprocess_syntax_1(_, [], _, _) do
    []
  end

  defp preprocess_check_field(s, name, cs, mandatory) do
    case :lists.keyfind(name, 2, cs) do
      tuple when is_tuple(tuple) ->
        case not mandatory and is_mandatory_class_field(tuple) do
          true ->
            asn1_error(
              s,
              {:syntax_mandatory_in_optional_group, name}
            )

          false ->
            {:field, tuple}
        end

      false ->
        asn1_error(s, {:syntax_undefined_field, name})
    end
  end

  defp preprocess_get_fields([{:field, f} | t], acc) do
    name = :erlang.element(2, f)
    preprocess_get_fields(t, [name | acc])
  end

  defp preprocess_get_fields([{:optional, l} | t], acc) do
    preprocess_get_fields(t, preprocess_get_fields(l, acc))
  end

  defp preprocess_get_fields([_ | t], acc) do
    preprocess_get_fields(t, acc)
  end

  defp preprocess_get_fields([], acc) do
    acc
  end

  defp match_syntax(s, [{:token, token} | t], [a | as] = args, acc) do
    case a do
      {:word_or_setting, _, r_Externaltypereference(type: ^token)} ->
        match_syntax(s, t, as, acc)

      {^token, line} when is_integer(line) ->
        match_syntax(s, t, as, acc)

      _ ->
        {:nomatch, args}
    end
  end

  defp match_syntax(s, [{:field, field} | t] = fs, [a | as0] = args0, acc) do
    try do
      match_syntax_type(s, field, a)
    catch
      _, _ ->
        {:nomatch, args0}
    else
      {:match, match} ->
        match_syntax(s, t, as0, :lists.reverse(match) ++ acc)

      {:params, _Name, r_ptypedef(args: params) = p, ref} ->
        {args, as} = :lists.split(length(params), as0)
        val = match_syntax_params(s, p, ref, args)
        match_syntax(s, fs, [val | as], acc)
    end
  end

  defp match_syntax(s, [{:optional, l} | t], as0, acc) do
    case match_syntax(s, l, as0, []) do
      {:match, match, as} ->
        match_syntax(s, t, as, :lists.reverse(match) ++ acc)

      {:nomatch, ^as0} ->
        match_syntax(s, t, as0, acc)

      {:nomatch, _} = noMatch ->
        noMatch
    end
  end

  defp match_syntax(_, [_ | _], [], _Acc) do
    {:nomatch, []}
  end

  defp match_syntax(_, [], as, acc) do
    {:match, acc, as}
  end

  defp match_syntax_type(s, type, {:value_tag, val}) do
    match_syntax_type(s, type, val)
  end

  defp match_syntax_type(s, type, {:setting, _, val}) do
    match_syntax_type(s, type, val)
  end

  defp match_syntax_type(s, type, {:word_or_setting, _, val}) do
    match_syntax_type(s, type, val)
  end

  defp match_syntax_type(_S, _Type, {atom, line})
       when is_atom(atom) and
              is_integer(line) do
    throw(:nomatch)
  end

  defp match_syntax_type(
         s,
         {:fixedtypevaluefield, name, r_type() = t, _, _} = type,
         r_Externalvaluereference() = valRef0
       ) do
    try do
      get_referenced_type(s, valRef0)
    catch
      {:error, _} ->
        valRef = r_valuedef(name: name, type: t, value: valRef0, module: r_state(s, :mname))
        match_syntax_type(s, type, valRef)
    else
      {m, r_valuedef() = valDef} ->
        match_syntax_type(update_state(s, m), type, valDef)
    end
  end

  defp match_syntax_type(s, {:fixedtypevaluefield, name, r_type(), _, _}, r_valuedef() = val0) do
    val = check_value(s, val0)
    {:match, [{name, val}]}
  end

  defp match_syntax_type(
         s,
         {:fixedtypevaluefield, name, r_type(), _, _},
         {:ValueFromObject, {:object, object}, fieldNames}
       ) do
    val = extract_field(s, object, fieldNames)
    {:match, [{name, val}]}
  end

  defp match_syntax_type(
         s,
         {:fixedtypevaluefield, name, r_type() = t, _, _} = type,
         any
       ) do
    valDef = r_valuedef(name: name, type: t, value: any, module: r_state(s, :mname))
    match_syntax_type(s, type, valDef)
  end

  defp match_syntax_type(_S, {:fixedtypevaluesetfield, name, r_type(), _}, any) do
    {:match, [{name, any}]}
  end

  defp match_syntax_type(s, {:objectfield, name, _, _, _}, r_Externalvaluereference() = ref) do
    {m, obj} = get_referenced_type(s, ref)
    check_object(s, obj, object_to_check(s, obj))
    {:match, [{name, r_Externalvaluereference(ref, module: m)}]}
  end

  defp match_syntax_type(s, {:objectfield, name, class, _, _}, {:object, _, _} = objDef) do
    inlinedObjName = :erlang.list_to_atom(:lists.concat([r_state(s, :tname), :_, name]))
    objSpec = r_Object(classname: class, def: objDef)
    checkedObj = check_object(s, r_typedef(typespec: objSpec), objSpec)
    inlObj = r_typedef(checked: true, name: inlinedObjName, typespec: checkedObj)
    objKey = {inlinedObjName, inlinedObjName}
    insert_once(s, :inlined_objects, objKey)
    :asn1_db.dbput(:erlang.get(:top_module), inlinedObjName, inlObj)
    {:match, [{name, inlObj}]}
  end

  defp match_syntax_type(_S, {:objectfield, name, _, _, _}, any) do
    {:match, [{name, any}]}
  end

  defp match_syntax_type(s, {:objectsetfield, name, cDef0, _}, any) do
    cDef =
      case cDef0 do
        r_type(def: cDef1) ->
          cDef1

        cDef1 ->
          cDef1
      end

    case match_syntax_objset(s, any, cDef) do
      r_typedef(typespec: r_ObjectSet() = ts0) = def__ ->
        ts = check_object(s, def__, ts0)
        {:match, [{name, r_typedef(def__, checked: true, typespec: ts)}]}

      _ ->
        syntax_match_error(s, any)
    end
  end

  defp match_syntax_type(s, {:typefield, name0, _}, r_type(def: {:pt, _, _} = def__) = actual) do
    t = check_type(s, r_typedef(typespec: actual), actual)
    r_Externaltypereference(type: ptName) = :erlang.element(2, def__)
    nameList = [ptName, r_state(s, :tname)]
    name = :erlang.list_to_atom(:asn1ct_gen.list2name(nameList))
    newTDef = r_typedef(checked: true, name: name, typespec: t)
    :asn1_db.dbput(r_state(s, :mname), name, newTDef)
    insert_once(s, :parameterized_objects, {name, :type, newTDef})
    {:match, [{name0, newTDef}]}
  end

  defp match_syntax_type(s, {:typefield, name, _}, r_type(def: r_ObjectClassFieldType()) = actual) do
    t = check_type(s, r_typedef(typespec: actual), actual)
    {:match, [{name, ocft_def(t)}]}
  end

  defp match_syntax_type(s, {:typefield, name, _}, r_type(def: r_Externaltypereference() = ref)) do
    match_syntax_external(s, name, ref)
  end

  defp match_syntax_type(s, {:typefield, name, _}, r_type(def: def__) = actual) do
    t = check_type(s, r_typedef(typespec: actual), actual)
    typeName = :asn1ct_gen.type(:asn1ct_gen.get_inner(def__))
    {:match, [{name, r_typedef(checked: true, name: typeName, typespec: t)}]}
  end

  defp match_syntax_type(s, {:typefield, name, _}, r_Externaltypereference() = ref) do
    match_syntax_external(s, name, ref)
  end

  defp match_syntax_type(_S, {:variabletypevaluefield, name, _, _}, any) do
    {:match, [{name, any}]}
  end

  defp match_syntax_type(_S, {:variabletypevaluesetfield, name, _, _}, any) do
    {:match, [{name, any}]}
  end

  defp match_syntax_type(_S, _Type, _Actual) do
    throw(:nomatch)
  end

  defp match_syntax_params(
         s0,
         r_ptypedef(name: name) = ptDef,
         r_Externaltypereference(module: m, type: n) = eRef0,
         args
       ) do
    s = r_state(s0, mname: m, module: load_asn1_module(s0, m), tname: name)
    type = check_type(s, ptDef, r_type(def: {:pt, eRef0, args}))
    eRefName = new_reference_name(n)
    eRef = r_Externaltypereference(type: eRefName, module: r_state(s0, :mname))
    tDef = r_typedef(checked: true, name: eRefName, typespec: type)
    insert_once(s0, :parameterized_objects, {eRefName, :type, tDef})
    :asn1_db.dbput(r_state(s0, :mname), r_Externaltypereference(eRef, :type), tDef)
    eRef
  end

  defp match_syntax_external(r_state(mname: mname) = s0, name, ref0) do
    {m, t0} = get_referenced_type(s0, ref0)
    ref1 = r_Externaltypereference(ref0, module: m)

    case t0 do
      r_ptypedef() ->
        {:params, name, t0, ref1}

      r_typedef(checked: false) = tDef0 when mname !== m ->
        s = r_state(s0, mname: m, module: load_asn1_module(s0, m), tname: get_datastr_name(tDef0))
        type = check_type(s, tDef0, r_typedef(tDef0, :typespec))
        tDef = r_typedef(tDef0, checked: true, typespec: type)
        :asn1_db.dbput(m, get_datastr_name(tDef), tDef)
        {:match, [{name, merged_name(s, ref1)}]}

      tDef ->
        type = :asn1ct.get_name_of_def(tDef)
        ref = r_Externaltypereference(ref1, type: type)
        {:match, [{name, ref}]}
    end
  end

  defp match_syntax_objset(_S, {:element_set, _, _} = set, classDef) do
    make_objset(classDef, set)
  end

  defp match_syntax_objset(s, r_Externaltypereference() = ref, _) do
    {_, t} = get_referenced_type(s, ref)
    t
  end

  defp match_syntax_objset(s, r_Externalvaluereference() = ref, _) do
    {_, t} = get_referenced_type(s, ref)
    t
  end

  defp match_syntax_objset(_, [_ | _] = set, classDef) do
    make_objset(classDef, set)
  end

  defp match_syntax_objset(s, {:object, :definedsyntax, words}, classDef) do
    case words do
      [word] ->
        match_syntax_objset_1(s, word, classDef)

      [_ | _] ->
        :none
    end
  end

  defp match_syntax_objset(s, r_type(def: r_Externaltypereference() = set), classDef) do
    match_syntax_objset(s, set, classDef)
  end

  defp match_syntax_objset(_, r_type(), _) do
    :none
  end

  defp match_syntax_objset_1(s, {:setting, _, set}, classDef) do
    match_syntax_objset(s, set, classDef)
  end

  defp match_syntax_objset_1(s, {:word_or_setting, _, set}, classDef) do
    match_syntax_objset(s, set, classDef)
  end

  defp match_syntax_objset_1(
         s,
         r_type(def: {:TypeFromObject, {:object, object}, fNs}),
         classDef
       ) do
    set = extract_field(s, object, fNs)
    [_ | _] = set
    r_typedef(checked: true, typespec: r_ObjectSet(class: classDef, set: set))
  end

  defp match_syntax_objset_1(_, r_type(def: r_ObjectClassFieldType()) = set, classDef) do
    make_objset(classDef, set)
  end

  defp match_syntax_objset_1(_, {:object, _, _} = object, classDef) do
    make_objset(classDef, [object])
  end

  defp make_objset(classDef, set) do
    r_typedef(typespec: r_ObjectSet(class: classDef, set: set))
  end

  defp syntax_match_error(s) do
    asn1_error(s, :syntax_nomatch)
  end

  defp syntax_match_error(s, what0) do
    what = printable_string(what0)
    asn1_error(s, {:syntax_nomatch, what})
  end

  defp printable_string(def__) do
    printable_string_1(def__)
  end

  defp printable_string_1({:word_or_setting, _, def__}) do
    printable_string_1(def__)
  end

  defp printable_string_1({:value_tag, v}) do
    printable_string_1(v)
  end

  defp printable_string_1({r_seqtag(val: val1), val2}) do
    :erlang.atom_to_list(val1) ++ ' ' ++ printable_string_1(val2)
  end

  defp printable_string_1(r_type(def: def__)) do
    :erlang.atom_to_list(:asn1ct_gen.get_inner(def__))
  end

  defp printable_string_1(r_Externaltypereference(type: type)) do
    :erlang.atom_to_list(type)
  end

  defp printable_string_1(r_Externalvaluereference(value: type)) do
    :erlang.atom_to_list(type)
  end

  defp printable_string_1({atom, line})
       when is_atom(atom) and
              is_integer(line) do
    q(atom)
  end

  defp printable_string_1({:object, :definedsyntax, l}) do
    str =
      :lists.join(
        ?\s,
        for item <- l do
          printable_string_1(item)
        end
      )

    q(:lists.flatten(str))
  end

  defp printable_string_1([_ | _] = def__) do
    case :lists.all(&is_integer/1, def__) do
      true ->
        :lists.flatten(:io_lib.format('~p', [def__]))

      false ->
        str =
          :lists.join(
            ?\s,
            for item <- def__ do
              printable_string_1(item)
            end
          )

        q(:lists.flatten(str))
    end
  end

  defp printable_string_1(def__) do
    :lists.flatten(:io_lib.format('~p', [def__]))
  end

  defp q(s) do
    :lists.concat(['"', s, '"'])
  end

  defp check_defaultfields(s, fields, classFields) do
    present =
      :ordsets.from_list(
        for {f, _} <- fields do
          f
        end
      )

    mandatory0 = get_mandatory_class_fields(classFields)
    mandatory = :ordsets.from_list(mandatory0)

    all =
      :ordsets.from_list(
        for f <- classFields do
          :erlang.element(2, f)
        end
      )

    r_state(tname: obj) = s

    case :ordsets.subtract(present, all) do
      [] ->
        :ok

      [_ | _] = invalid ->
        asn1_error(s, {:invalid_fields, invalid, obj})
    end

    case :ordsets.subtract(mandatory, present) do
      [] ->
        check_defaultfields_1(s, fields, classFields, [])

      [_ | _] = missing ->
        asn1_error(s, {:missing_mandatory_fields, missing, obj})
    end
  end

  defp check_defaultfields_1(_S, [], _ClassFields, acc) do
    {:object, :defaultsyntax, :lists.reverse(acc)}
  end

  defp check_defaultfields_1(s, [{fName, spec} | fields], classFields, acc) do
    cField = :lists.keyfind(fName, 2, classFields)
    {:match, match} = match_syntax_type(s, cField, spec)
    check_defaultfields_1(s, fields, classFields, match ++ acc)
  end

  defp get_mandatory_class_fields(classFields) do
    for f <- classFields, is_mandatory_class_field(f) do
      :erlang.element(2, f)
    end
  end

  defp is_mandatory_class_field({:fixedtypevaluefield, _, _, _, :MANDATORY}) do
    true
  end

  defp is_mandatory_class_field({:objectfield, _, _, _, :MANDATORY}) do
    true
  end

  defp is_mandatory_class_field({:objectsetfield, _, _, :MANDATORY}) do
    true
  end

  defp is_mandatory_class_field({:typefield, _, :MANDATORY}) do
    true
  end

  defp is_mandatory_class_field({:variabletypevaluefield, _, _, :MANDATORY}) do
    true
  end

  defp is_mandatory_class_field({:variabletypevaluesetfield, _, _, :MANDATORY}) do
    true
  end

  defp is_mandatory_class_field(_) do
    false
  end

  defp merged_name(r_state(inputmodules: []), eRef) do
    eRef
  end

  defp merged_name(s, eRef = r_Externaltypereference(module: m)) do
    case {r_state(s, :mname), :lists.member(m, r_state(s, :inputmodules))} do
      {^m, _} ->
        eRef

      {mergeM, true} ->
        newName = renamed_reference(s, eRef)
        r_Externaltypereference(eRef, module: mergeM, type: newName)

      {_, _} ->
        eRef
    end
  end

  defp ocft_def(r_type(def: r_ObjectClassFieldType(type: oCFT)) = t) do
    case oCFT do
      {:fixedtypevaluefield, _, innerType} ->
        case :asn1ct_gen.type(:asn1ct_gen.get_inner(r_type(innerType, :def))) do
          bif
          when bif === {:primitive, :bif} or
                 bif === {:constructed, :bif} ->
            r_typedef(checked: true, name: bif, typespec: innerType)

          r_Externaltypereference() = ref ->
            ref
        end

      :ASN1_OPEN_TYPE ->
        r_typedef(checked: true, typespec: r_type(t, def: :ASN1_OPEN_TYPE))
    end
  end

  defp check_value(oldS, v) when elem(v, 0) === :pvaluesetdef do
    r_pvaluesetdef(checked: checked, type: type) = v

    case checked do
      true ->
        v

      {:error, _} ->
        v

      false ->
        case get_referenced_type(oldS, r_type(type, :def)) do
          {_, class} when elem(class, 0) === :classdef ->
            throw({:pobjectsetdef})

          _ ->
            :continue
        end
    end
  end

  defp check_value(_OldS, v) when elem(v, 0) === :pvaluedef do
    v
  end

  defp check_value(oldS, v) when elem(v, 0) === :typedef do
    :no_debug
    r_typedef(typespec: tS) = v

    case tS do
      r_ObjectSet(class: classRef) ->
        {_RefM, tSDef} = get_referenced_type(oldS, classRef)

        case tSDef do
          r_classdef() ->
            throw({:objectsetdef})

          r_typedef(typespec: r_type(def: eref))
          when elem(eref, 0) === :Externaltypereference ->
            check_value(oldS, r_typedef(v, typespec: r_ObjectSet(tS, class: eref)))

          r_typedef(typespec: hostType) ->
            valueSet0 = r_ObjectSet(tS, :set)
            constr = check_constraints(oldS, hostType, [valueSet0])
            type = check_type(oldS, tSDef, r_typedef(tSDef, :typespec))
            {:valueset, r_type(type, constraint: constr)}
        end

      _ ->
        throw({:objectsetdef})
    end
  end

  defp check_value(
         s,
         r_valuedef(pos: pos, name: name, type: type, value: {:valueset, constr})
       ) do
    newType = r_type(type, constraint: [constr])
    {:valueset, check_type(s, r_typedef(pos: pos, name: name, typespec: newType), newType)}
  end

  defp check_value(s, r_valuedef() = v) do
    :no_debug

    case v do
      r_valuedef(checked: true) ->
        v

      r_valuedef(checked: false) ->
        check_valuedef(s, v)
    end
  end

  defp check_valuedef(r_state(recordtopname: topName) = s0, v0) do
    r_valuedef(name: name, type: vtype0, value: value, module: modName) = v0
    v = r_valuedef(v0, checked: true)
    vtype1 = expand_valuedef_type(vtype0)
    vtype = check_type(s0, r_typedef(name: name, typespec: vtype1), vtype1)
    def__ = r_type(vtype, :def)
    s1 = r_state(s0, tname: def__)
    sVal = update_state(s1, modName)

    case def__ do
      r_Externaltypereference(type: recName) = ext ->
        {refM, type} = get_referenced_type(s1, ext)
        s2 = update_state(s1, refM)

        case type do
          r_typedef(typespec: typeSpec0) = typeDef ->
            typeSpec = check_type(s2, typeDef, typeSpec0)

            s3 =
              case is_contextswitchtype(type) do
                true ->
                  s2

                false ->
                  r_state(s2, recordtopname: [recName | topName])
              end

            r_valuedef(value: checkedVal) =
              check_value(
                s3,
                r_valuedef(v0, type: typeSpec)
              )

            r_valuedef(v, value: checkedVal)

          r_type() ->
            r_valuedef(value: checkedVal) =
              check_value(
                r_state(s2,
                  recordtopname: [
                    recName
                    | topName
                  ]
                ),
                r_valuedef(v, type: type)
              )

            r_valuedef(v, value: checkedVal)
        end

      :ASN1_OPEN_TYPE ->
        {:opentypefieldvalue, aNYType, aNYValue} = value

        checkedV =
          check_value(
            sVal,
            r_valuedef(name: name, type: aNYType, value: aNYValue, module: modName)
          )

        r_valuedef(v, value: r_valuedef(checkedV, :value))

      :INTEGER ->
        r_valuedef(v, value: normalize_value(sVal, vtype, value, []))

      {:INTEGER, _NamedNumberList} ->
        r_valuedef(v, value: normalize_value(sVal, vtype, value, []))

      r_SEQUENCE() ->
        {:ok, seqVal} = convert_external(sVal, vtype, value)
        r_valuedef(v, value: normalize_value(sVal, vtype, seqVal, topName))

      _ ->
        r_valuedef(v, value: normalize_value(sVal, vtype, value, topName))
    end
  end

  defp expand_valuedef_type(r_type(def: seq) = type)
       when elem(seq, 0) === :SEQUENCE do
    newComponents =
      case r_SEQUENCE(seq, :components) do
        {r1, _Ext, r2} ->
          r1 ++ r2

        {root, _Ext} ->
          root

        root ->
          take_only_rootset(root)
      end

    newSeq = r_SEQUENCE(seq, components: newComponents)
    r_type(type, def: newSeq)
  end

  defp expand_valuedef_type(r_type(def: set) = type)
       when elem(set, 0) === :SET do
    newComponents =
      case r_SET(set, :components) do
        {r1, _Ext, r2} ->
          r1 ++ r2

        {root, _Ext} ->
          root

        root ->
          take_only_rootset(root)
      end

    newSet = r_SET(set, components: newComponents)
    r_type(type, def: newSet)
  end

  defp expand_valuedef_type(type) do
    type
  end

  defp is_contextswitchtype(r_typedef(name: :EXTERNAL)) do
    true
  end

  defp is_contextswitchtype(r_typedef(name: :"EMBEDDED PDV")) do
    true
  end

  defp is_contextswitchtype(r_typedef(name: :"CHARACTER STRING")) do
    true
  end

  defp is_contextswitchtype(_) do
    false
  end

  defp validate_objectidentifier(s, oidType, r_Externalvaluereference() = id) do
    get_oid_value(s, oidType, false, id)
  end

  defp validate_objectidentifier(s, oidType, {:ValueFromObject, {:object, obj}, fields}) do
    case extract_field(s, obj, fields) do
      r_valuedef(checked: true, value: value, type: type)
      when is_tuple(value) ->
        _ = get_oid_type(s, oidType, type)
        value

      _ ->
        asn1_error(s, {:illegal_oid, oidType})
    end
  end

  defp validate_objectidentifier(s, oidType, [{r_seqtag(module: mod, pos: pos, val: atom), val}]) do
    rec = r_Externalvaluereference(pos: pos, module: mod, value: atom)
    validate_oid(s, oidType, [rec, val], [])
  end

  defp validate_objectidentifier(s, oidType, [_ | _] = l0) do
    validate_oid(s, oidType, l0, [])
  end

  defp validate_objectidentifier(s, oidType, _) do
    asn1_error(s, {:illegal_oid, oidType})
  end

  defp get_oid_value(s, oidType, allowInteger, r_Externalvaluereference() = id) do
    case get_referenced_type(s, id) do
      {_, r_valuedef(checked: checked, type: type, value: v)} ->
        case get_oid_type(s, oidType, type) do
          :INTEGER when not allowInteger ->
            asn1_error(s, {:illegal_oid, oidType})

          _ when checked ->
            v

          :INTEGER ->
            v

          _ ->
            validate_objectidentifier(s, oidType, v)
        end

      _ ->
        asn1_error(s, {:illegal_oid, oidType})
    end
  end

  defp validate_oid(s, oidType, [], acc) do
    oid = :lists.reverse(acc)
    validate_oid_path(s, oidType, oid)
    :erlang.list_to_tuple(oid)
  end

  defp validate_oid(s, oidType, [value | vrest], acc)
       when is_integer(value) do
    validate_oid(s, oidType, vrest, [value | acc])
  end

  defp validate_oid(s, oidType, [{:NamedNumber, _Name, value} | vrest], acc)
       when is_integer(value) do
    validate_oid(s, oidType, vrest, [value | acc])
  end

  defp validate_oid(s, oidType, [r_Externalvaluereference() = id | vrest], acc) do
    neededOidType =
      case acc do
        [] ->
          :o_id

        [_ | _] ->
          :rel_oid
      end

    try do
      get_oid_value(s, neededOidType, true, id)
    catch
      _, _ ->
        case reserved_objectid(r_Externalvaluereference(id, :value), acc) do
          value when is_integer(value) ->
            validate_oid(s, oidType, vrest, [value | acc])

          false ->
            asn1_error(s, {:illegal_oid, oidType})
        end
    else
      val when is_integer(val) ->
        validate_oid(s, oidType, vrest, [val | acc])

      val when is_tuple(val) ->
        l = :erlang.tuple_to_list(val)
        validate_oid(s, oidType, vrest, :lists.reverse(l, acc))
    end
  end

  defp validate_oid(s, oidType, _V, _Acc) do
    asn1_error(s, {:illegal_oid, oidType})
  end

  defp get_oid_type(s, oidType, r_type(def: def__)) do
    get_oid_type(s, oidType, def__)
  end

  defp get_oid_type(s, oidType, r_Externaltypereference() = id) do
    {_, oI} = get_referenced_type(s, id)
    get_oid_type(s, oidType, r_typedef(oI, :typespec))
  end

  defp get_oid_type(_S, :o_id, :"OBJECT IDENTIFIER" = t) do
    t
  end

  defp get_oid_type(_S, :rel_oid, :"RELATIVE-OID" = t) do
    t
  end

  defp get_oid_type(_S, _, :INTEGER = t) do
    t
  end

  defp get_oid_type(s, oidType, _) do
    asn1_error(s, {:illegal_oid, oidType})
  end

  defp reserved_objectid(:"itu-t", []) do
    0
  end

  defp reserved_objectid(:ccitt, []) do
    0
  end

  defp reserved_objectid(:recommendation, [0]) do
    0
  end

  defp reserved_objectid(:question, [0]) do
    1
  end

  defp reserved_objectid(:administration, [0]) do
    2
  end

  defp reserved_objectid(:"network-operator", [0]) do
    3
  end

  defp reserved_objectid(:"identified-organization", [0]) do
    4
  end

  defp reserved_objectid(:a, [0, 0]) do
    1
  end

  defp reserved_objectid(:b, [0, 0]) do
    2
  end

  defp reserved_objectid(:c, [0, 0]) do
    3
  end

  defp reserved_objectid(:d, [0, 0]) do
    4
  end

  defp reserved_objectid(:e, [0, 0]) do
    5
  end

  defp reserved_objectid(:f, [0, 0]) do
    6
  end

  defp reserved_objectid(:g, [0, 0]) do
    7
  end

  defp reserved_objectid(:h, [0, 0]) do
    8
  end

  defp reserved_objectid(:i, [0, 0]) do
    9
  end

  defp reserved_objectid(:j, [0, 0]) do
    10
  end

  defp reserved_objectid(:k, [0, 0]) do
    11
  end

  defp reserved_objectid(:l, [0, 0]) do
    12
  end

  defp reserved_objectid(:m, [0, 0]) do
    13
  end

  defp reserved_objectid(:n, [0, 0]) do
    14
  end

  defp reserved_objectid(:o, [0, 0]) do
    15
  end

  defp reserved_objectid(:p, [0, 0]) do
    16
  end

  defp reserved_objectid(:q, [0, 0]) do
    17
  end

  defp reserved_objectid(:r, [0, 0]) do
    18
  end

  defp reserved_objectid(:s, [0, 0]) do
    19
  end

  defp reserved_objectid(:t, [0, 0]) do
    20
  end

  defp reserved_objectid(:u, [0, 0]) do
    21
  end

  defp reserved_objectid(:v, [0, 0]) do
    22
  end

  defp reserved_objectid(:w, [0, 0]) do
    23
  end

  defp reserved_objectid(:x, [0, 0]) do
    24
  end

  defp reserved_objectid(:y, [0, 0]) do
    25
  end

  defp reserved_objectid(:z, [0, 0]) do
    26
  end

  defp reserved_objectid(:iso, []) do
    1
  end

  defp reserved_objectid(:standard, [1]) do
    0
  end

  defp reserved_objectid(:"member-body", [1]) do
    2
  end

  defp reserved_objectid(:"identified-organization", [1]) do
    3
  end

  defp reserved_objectid(:"joint-iso-itu-t", []) do
    2
  end

  defp reserved_objectid(:"joint-iso-ccitt", []) do
    2
  end

  defp reserved_objectid(_, _) do
    false
  end

  defp validate_oid_path(_, :rel_oid, _) do
    :ok
  end

  defp validate_oid_path(_, :o_id, [[0, i] | _])
       when 0 <= i and
              i <= 9 do
    :ok
  end

  defp validate_oid_path(_, :o_id, [[1, i] | _])
       when 0 <= i and
              i <= 3 do
    :ok
  end

  defp validate_oid_path(_, :o_id, [2 | _]) do
    :ok
  end

  defp validate_oid_path(s, :o_id = oidType, _) do
    asn1_error(s, {:illegal_oid, oidType})
  end

  defp convert_external(s, vtype, value) do
    case vtype do
      r_type(tag: [{:tag, :UNIVERSAL, 8, :IMPLICIT, 32}]) ->
        case value do
          [{r_seqtag(val: :identification), _} | _] ->
            {:ok, to_EXTERNAL1990(s, value)}

          _ ->
            {:ok, value}
        end

      _ ->
        {:ok, value}
    end
  end

  defp to_EXTERNAL1990(
         s,
         [
           {r_seqtag(val: :identification) = t, {:CHOICE, {:syntax, stx}}}
           | rest
         ]
       ) do
    to_EXTERNAL1990(s, rest, [{r_seqtag(t, val: :"direct-reference"), stx}])
  end

  defp to_EXTERNAL1990(
         s,
         [
           {r_seqtag(val: :identification) = t, {:CHOICE, {:"presentation-context-id", i}}}
           | rest
         ]
       ) do
    to_EXTERNAL1990(s, rest, [{r_seqtag(t, val: :"indirect-reference"), i}])
  end

  defp to_EXTERNAL1990(
         s,
         [
           {r_seqtag(val: :identification) = t,
            {:CHOICE, {:"context-negotiation", [{_, pCid}, {_, trStx}]}}}
           | rest
         ]
       ) do
    to_EXTERNAL1990(s, rest, [
      {r_seqtag(t, val: :"indirect-reference"), pCid},
      {r_seqtag(t, val: :"direct-reference"), trStx}
    ])
  end

  defp to_EXTERNAL1990(s, _) do
    asn1_error(s, :illegal_external_value)
  end

  defp to_EXTERNAL1990(s, [v = {r_seqtag(val: :"data-value-descriptor"), _} | rest], acc) do
    to_EXTERNAL1990(s, rest, [v | acc])
  end

  defp to_EXTERNAL1990(_S, [{r_seqtag(val: :"data-value") = t, val}], acc) do
    encoding = {r_seqtag(t, val: :encoding), {:CHOICE, {:"octet-aligned", val}}}
    :lists.reverse([encoding | acc])
  end

  defp to_EXTERNAL1990(s, _, _) do
    asn1_error(s, :illegal_external_value)
  end

  defp normalize_value(_, _, :mandatory, _) do
    :mandatory
  end

  defp normalize_value(_, _, :OPTIONAL, _) do
    :OPTIONAL
  end

  defp normalize_value(s, type, {:DEFAULT, value}, nameList) do
    case (try do
            get_canonic_type(s, type, nameList)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:BOOLEAN, cType, _} ->
        normalize_boolean(s, value, cType)

      {:INTEGER, cType, _} ->
        normalize_integer(s, value, cType)

      {:"BIT STRING", cType, _} ->
        normalize_bitstring(s, value, cType)

      {:"OCTET STRING", _, _} ->
        normalize_octetstring(s, value)

      {:NULL, _CType, _} ->
        :NULL

      {:"RELATIVE-OID", _, _} ->
        normalize_relative_oid(s, value)

      {:"OBJECT IDENTIFIER", _, _} ->
        normalize_objectidentifier(s, value)

      {:ObjectDescriptor, _, _} ->
        normalize_objectdescriptor(value)

      {:REAL, _, _} ->
        normalize_real(value)

      {:ENUMERATED, cType, _} ->
        normalize_enumerated(s, value, cType)

      {:CHOICE, cType, newNameList} ->
        choiceComponents =
          get_choice_components(
            s,
            {:CHOICE, cType}
          )

        normalize_choice(s, value, choiceComponents, newNameList)

      {:SEQUENCE, cType, newNameList} ->
        normalize_sequence(s, value, cType, newNameList)

      {:"SEQUENCE OF", cType, newNameList} ->
        normalize_seqof(s, value, cType, newNameList)

      {:SET, cType, newNameList} ->
        normalize_set(s, value, cType, newNameList)

      {:"SET OF", cType, newNameList} ->
        normalize_setof(s, value, cType, newNameList)

      {:restrictedstring, cType, _} ->
        normalize_restrictedstring(s, value, cType)

      {:ASN1_OPEN_TYPE, {:typefield, _TF}, nL} ->
        normalize_objectclassfieldvalue(s, value, nL)

      err ->
        :asn1ct.warning(
          'could not check default value ~p~nType:~n~p~nNameList:~n~p~n',
          [value, type, err],
          s,
          'could not check default value'
        )

        value
    end
  end

  defp normalize_value(s, type, val, nameList) do
    normalize_value(s, type, {:DEFAULT, val}, nameList)
  end

  defp normalize_boolean(_, true, _) do
    true
  end

  defp normalize_boolean(_, false, _) do
    false
  end

  defp normalize_boolean(s, bool = r_Externalvaluereference(), cType) do
    get_normalized_value(s, bool, cType, &normalize_boolean/3, [])
  end

  defp normalize_boolean(s, _, _) do
    asn1_error(s, {:illegal_value, 'BOOLEAN'})
  end

  defp normalize_integer(_S, int, _) when is_integer(int) do
    int
  end

  defp normalize_integer(s, r_Externalvaluereference(value: name) = ref, nNL) do
    case :lists.keyfind(name, 1, nNL) do
      {^name, val} ->
        val

      false ->
        try do
          get_referenced_value(s, ref)
        catch
          _ ->
            asn1_error(s, :illegal_integer_value)
        else
          val when is_integer(val) ->
            val

          _ ->
            asn1_error(s, :illegal_integer_value)
        end
    end
  end

  defp normalize_integer(s, {:ValueFromObject, {:object, obj}, fieldNames}, _) do
    case extract_field(s, obj, fieldNames) do
      r_valuedef(value: val) when is_integer(val) ->
        val

      _ ->
        asn1_error(s, :illegal_integer_value)
    end
  end

  defp normalize_integer(s, _, _) do
    asn1_error(s, :illegal_integer_value)
  end

  defp normalize_bitstring(s, value, type) do
    case value do
      {:hstring, string} when is_list(string) ->
        hstring_to_bitstring(string)

      {:bstring, string} when is_list(string) ->
        bstring_to_bitstring(string)

      r_Externalvaluereference() ->
        val = get_referenced_value(s, value)
        normalize_bitstring(s, val, type)

      {:ValueFromObject, {:object, obj}, fieldNames} ->
        case extract_field(s, obj, fieldNames) do
          r_valuedef(value: val) ->
            normalize_bitstring(s, val, type)

          _ ->
            asn1_error(s, {:illegal_value, 'BIT STRING'})
        end

      recList when is_list(recList) ->
        for item <- recList do
          normalize_bs_item(s, item, type)
        end

      bs when is_bitstring(bs) ->
        bs

      _ ->
        asn1_error(s, {:illegal_value, 'BIT STRING'})
    end
  end

  defp normalize_bs_item(s, r_Externalvaluereference(value: name), type) do
    case :lists.keymember(name, 1, type) do
      true ->
        name

      false ->
        asn1_error(s, {:illegal_value, 'BIT STRING'})
    end
  end

  defp normalize_bs_item(_, atom, _) when is_atom(atom) do
    atom
  end

  defp normalize_bs_item(s, _, _) do
    asn1_error(s, {:illegal_value, 'BIT STRING'})
  end

  defp hstring_to_binary(l) do
    byte_align(hstring_to_bitstring(l))
  end

  defp bstring_to_binary(l) do
    byte_align(bstring_to_bitstring(l))
  end

  defp byte_align(bs) do
    case rem(bit_size(bs), 8) do
      0 ->
        bs

      n ->
        <<bs::bitstring, 0::size(8 - n)>>
    end
  end

  defp hstring_to_bitstring(l) do
    for d <- l, into: <<>> do
      <<hex_to_int(d)::size(4)>>
    end
  end

  defp bstring_to_bitstring(l) do
    for d <- l, into: <<>> do
      <<d - ?0::size(1)>>
    end
  end

  defp hex_to_int(d) when ?0 <= d and d <= ?9 do
    d - ?0
  end

  defp hex_to_int(d) when ?A <= d and d <= ?F do
    d - (?A - 10)
  end

  defp normalize_octetstring(s, value) do
    case value do
      {:bstring, string} ->
        bstring_to_binary(string)

      {:hstring, string} ->
        hstring_to_binary(string)

      r_Externalvaluereference() ->
        case get_referenced_value(s, value) do
          string when is_binary(string) ->
            string

          other ->
            normalize_octetstring(s, other)
        end

      {:ValueFromObject, {:object, obj}, fieldNames} ->
        case extract_field(s, obj, fieldNames) do
          r_valuedef(value: val) when is_binary(val) ->
            val

          _ ->
            asn1_error(s, :illegal_octet_string_value)
        end

      val when is_binary(val) ->
        val

      _ ->
        asn1_error(s, :illegal_octet_string_value)
    end
  end

  defp normalize_objectidentifier(s, value) do
    validate_objectidentifier(s, :o_id, value)
  end

  defp normalize_relative_oid(s, value) do
    validate_objectidentifier(s, :rel_oid, value)
  end

  defp normalize_objectdescriptor(value) do
    value
  end

  defp normalize_real(value) do
    value
  end

  defp normalize_enumerated(s, id0, nNL) do
    {id, _} = lookup_enum_value(s, id0, nNL)
    id
  end

  defp lookup_enum_value(s, id, {base, ext}) do
    lookup_enum_value(s, id, base ++ ext)
  end

  defp lookup_enum_value(s, r_Externalvaluereference(value: id), nNL) do
    lookup_enum_value(s, id, nNL)
  end

  defp lookup_enum_value(s, id, nNL) when is_atom(id) do
    case :lists.keyfind(id, 1, nNL) do
      {_, _} = ret ->
        ret

      false ->
        asn1_error(s, {:undefined, id})
    end
  end

  defp normalize_choice(s, {:CHOICE, {c, v}}, cType, nameList)
       when is_atom(c) do
    case :lists.keyfind(c, r_ComponentType(:name), cType) do
      r_ComponentType(typespec: cT, name: name) ->
        {c, normalize_value(s, cT, {:DEFAULT, v}, [name | nameList])}

      false ->
        asn1_error(s, {:illegal_id, c})
    end
  end

  defp normalize_choice(s, cV = {name, _ChoiceVal}, cType, nameList)
       when is_atom(name) do
    normalize_choice(s, {:CHOICE, cV}, cType, nameList)
  end

  defp normalize_choice(s, v, _CType, _NameList) do
    asn1_error(s, {:illegal_id, error_value(v)})
  end

  defp normalize_sequence(s, value, components, nameList)
       when is_tuple(components) do
    normalize_sequence(s, value, :lists.flatten(:erlang.tuple_to_list(components)), nameList)
  end

  defp normalize_sequence(s, {name, value}, components, nameList)
       when is_atom(name) and is_list(value) do
    normalize_sequence(s, value, components, nameList)
  end

  defp normalize_sequence(s, value, components, nameList) do
    normalized_record(:SEQUENCE, s, value, components, nameList)
  end

  defp normalize_set(s, value, components, nameList)
       when is_tuple(components) do
    normalize_set(s, value, :lists.flatten(:erlang.tuple_to_list(components)), nameList)
  end

  defp normalize_set(s, {name, value}, components, nameList)
       when is_atom(name) and is_list(value) do
    normalized_record(:SET, s, value, components, nameList)
  end

  defp normalize_set(s, value, components, nameList) do
    newName = :erlang.list_to_atom(:asn1ct_gen.list2name(nameList))

    case is_record_normalized(s, newName, value, length(components)) do
      true ->
        value

      _ ->
        sortedVal = sort_value(components, value)
        normalized_record(:SET, s, sortedVal, components, nameList)
    end
  end

  defp sort_value(components, value0) when is_list(value0) do
    {keys0, _} =
      :lists.mapfoldl(
        fn r_ComponentType(name: n), i ->
          {{n, i}, i + 1}
        end,
        0,
        components
      )

    keys = :gb_trees.from_orddict(:orddict.from_list(keys0))

    value1 =
      for {r_seqtag(val: n), _} = pair <- value0 do
        {case :gb_trees.lookup(n, keys) do
           {:value, k} ->
             k

           :none ->
             :end
         end, pair}
      end

    value = :lists.sort(value1)

    for {_, pair} <- value do
      pair
    end
  end

  defp sort_value(_Components, r_Externalvaluereference() = value) do
    value
  end

  defp sort_val_if_set([:SET | _], val, type) do
    sort_value(type, val)
  end

  defp sort_val_if_set(_, val, _) do
    val
  end

  defp normalized_record(sorS, s, value, components, nameList) do
    newName =
      :erlang.list_to_atom(
        :lists.concat([get_record_prefix_name(s), :asn1ct_gen.list2name(nameList)])
      )

    case is_record_normalized(s, newName, value, length(components)) do
      true ->
        value

      false ->
        noComps = length(components)
        listOfVals = normalize_seq_or_set(sorS, s, value, components, nameList, [])
        ^noComps = length(listOfVals)

        case use_maps(s) do
          false ->
            :erlang.list_to_tuple([newName | listOfVals])

          true ->
            create_map_value(components, listOfVals)
        end
    end
  end

  defp is_record_normalized(s, name, v = r_Externalvaluereference(), numComps) do
    case get_referenced_type(s, v) do
      {_M, r_valuedef(type: _T1, value: v2)} ->
        is_record_normalized(s, name, v2, numComps)

      _ ->
        false
    end
  end

  defp is_record_normalized(_S, name, value, numComps)
       when is_tuple(value) do
    tuple_size(value) === numComps + 1 and
      :erlang.element(
        1,
        value
      ) === name
  end

  defp is_record_normalized(_S, _Name, value, _NumComps)
       when is_map(value) do
    true
  end

  defp is_record_normalized(_, _, _, _) do
    false
  end

  defp use_maps(r_state(options: opts)) do
    :lists.member(:maps, opts)
  end

  defp create_map_value(components, listOfVals) do
    zipped = :lists.zip(components, listOfVals)

    l =
      for {r_ComponentType(name: name), v} <- zipped,
          v !== :asn1_NOVALUE do
        {name, v}
      end

    :maps.from_list(l)
  end

  defp normalize_seq_or_set(
         sorS,
         s,
         [{r_seqtag(val: cname), v} | vs],
         [r_ComponentType(name: cname, typespec: tS) | cs],
         nameList,
         acc
       ) do
    newNameList =
      case r_type(tS, :def) do
        r_Externaltypereference(type: tName) ->
          [tName]

        _ ->
          [cname | nameList]
      end

    nVal = normalize_value(s, tS, {:DEFAULT, v}, newNameList)
    normalize_seq_or_set(sorS, s, vs, cs, nameList, [nVal | acc])
  end

  defp normalize_seq_or_set(
         sorS,
         s,
         values = [{r_seqtag(val: cname0), _V} | _Vs],
         [r_ComponentType(prop: :OPTIONAL) | cs],
         nameList,
         acc
       ) do
    verify_valid_component(s, cname0, cs)
    normalize_seq_or_set(sorS, s, values, cs, nameList, [:asn1_NOVALUE | acc])
  end

  defp normalize_seq_or_set(
         sorS,
         s,
         values = [{r_seqtag(val: cname0), _V} | _Vs],
         [
           r_ComponentType(name: cname, typespec: tS, prop: {:DEFAULT, value})
           | cs
         ],
         nameList,
         acc
       ) do
    verify_valid_component(s, cname0, cs)

    newNameList =
      case r_type(tS, :def) do
        r_Externaltypereference(type: tName) ->
          [tName]

        _ ->
          [cname | nameList]
      end

    nVal = normalize_value(s, tS, {:DEFAULT, value}, newNameList)
    normalize_seq_or_set(sorS, s, values, cs, nameList, [nVal | acc])
  end

  defp normalize_seq_or_set(
         sorS,
         s,
         [],
         [
           r_ComponentType(name: name, typespec: tS, prop: {:DEFAULT, value})
           | cs
         ],
         nameList,
         acc
       ) do
    newNameList =
      case r_type(tS, :def) do
        r_Externaltypereference(type: tName) ->
          [tName]

        _ ->
          [name | nameList]
      end

    nVal = normalize_value(s, tS, {:DEFAULT, value}, newNameList)
    normalize_seq_or_set(sorS, s, [], cs, nameList, [nVal | acc])
  end

  defp normalize_seq_or_set(sorS, s, [], [r_ComponentType(prop: :OPTIONAL) | cs], nameList, acc) do
    normalize_seq_or_set(sorS, s, [], cs, nameList, [:asn1_NOVALUE | acc])
  end

  defp normalize_seq_or_set(sorS, s, value = r_Externalvaluereference(), cs, nameList, acc) do
    get_normalized_value(s, value, cs, &normalize_seq_or_set/6, [sorS, nameList, acc])
  end

  defp normalize_seq_or_set(_SorS, _S, [], [], _, acc) do
    :lists.reverse(acc)
  end

  defp normalize_seq_or_set(_SorS, s, v, cs, _, _) do
    case v do
      [{r_seqtag(val: name), _} | _] ->
        asn1_error(s, {:illegal_id, error_value(name)})

      [] ->
        [r_ComponentType(name: name) | _] = cs
        asn1_error(s, {:missing_id, error_value(name)})
    end
  end

  defp verify_valid_component(s, name, cs) do
    case :lists.keyfind(name, r_ComponentType(:name), cs) do
      false ->
        asn1_error(s, {:illegal_id, error_value(name)})

      r_ComponentType() ->
        :ok
    end
  end

  defp normalize_seqof(s, value, type, nameList) do
    normalize_s_of(:"SEQUENCE OF", s, value, type, nameList)
  end

  defp normalize_setof(s, value, type, nameList) do
    normalize_s_of(:"SET OF", s, value, type, nameList)
  end

  defp normalize_s_of(sorS, s, value, type, nameList)
       when is_list(value) do
    defValueList =
      :lists.map(
        fn x ->
          {:DEFAULT, x}
        end,
        value
      )

    suffix = :asn1ct_gen.constructed_suffix(sorS, type)
    def__ = r_type(type, :def)
    innerType = :asn1ct_gen.get_inner(def__)
    whatKind = :asn1ct_gen.type(innerType)

    newNameList =
      case whatKind do
        {:constructed, :bif} ->
          [suffix | nameList]

        r_Externaltypereference(type: name) ->
          [name]

        _ ->
          []
      end

    normFun = fn x ->
      normalize_value(s, type, x, newNameList)
    end

    case (try do
            :lists.map(normFun, defValueList)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      list when is_list(list) ->
        list

      _ ->
        :asn1ct.warning(
          '~p could not handle value ~p~n',
          [sorS, value],
          s,
          'could not handle value'
        )

        value
    end
  end

  defp normalize_s_of(sorS, s, value, type, nameList)
       when elem(value, 0) === :Externalvaluereference do
    get_normalized_value(s, value, type, &normalize_s_of/5, [sorS, nameList])
  end

  defp normalize_restrictedstring(s, [h | t], cType)
       when is_list(h) or
              is_tuple(h) do
    [
      normalize_restrictedstring(s, h, cType)
      | normalize_restrictedstring(s, t, cType)
    ]
  end

  defp normalize_restrictedstring(_S, cString, _) when is_list(cString) do
    cString
  end

  defp normalize_restrictedstring(s, eRef, cType)
       when elem(eRef, 0) === :Externalvaluereference do
    get_normalized_value(s, eRef, cType, &normalize_restrictedstring/3, [])
  end

  defp normalize_objectclassfieldvalue(s, {:opentypefieldvalue, type, value}, nameList) do
    normalize_value(s, type, value, nameList)
  end

  defp normalize_objectclassfieldvalue(_S, other, _NameList) do
    other
  end

  defp get_normalized_value(s, val, type, func, addArg) do
    case (try do
            get_referenced_type(s, val)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {extM, _VDef = r_valuedef(type: _T1, value: v)} ->
        v2 = sort_val_if_set(addArg, v, type)
        call_Func(update_state(s, extM), v2, type, func, addArg)

      {:error, _} ->
        :asn1ct.warning('default value not comparable ~p~n', [val], s)
        val

      {extM, newVal} ->
        v2 = sort_val_if_set(addArg, newVal, type)
        call_Func(update_state(s, extM), v2, type, func, addArg)

      _ ->
        :asn1ct.warning(
          'default value not comparable ~p~n',
          [val],
          s,
          'default value not comparable'
        )

        val
    end
  end

  defp call_Func(s, val, type, func, argList) do
    case argList do
      [] ->
        func.(s, val, type)

      [lastArg] ->
        func.(s, val, type, lastArg)

      [arg1, lastArg1] ->
        func.(arg1, s, val, type, lastArg1)

      [arg1, lastArg1, lastArg2] ->
        func.(arg1, s, val, type, lastArg1, lastArg2)
    end
  end

  defp get_canonic_type(s, type, nameList) do
    {innerType, newType, newNameList} =
      case r_type(type, :def) do
        :INTEGER = name ->
          {name, [], nameList}

        name when is_atom(name) ->
          {name, type, nameList}

        ref when elem(ref, 0) === :Externaltypereference ->
          {_,
           r_typedef(
             name: name,
             typespec: refedType
           )} =
            get_referenced_type(
              s,
              ref
            )

          get_canonic_type(s, refedType, [name])

        {name, t} when is_atom(name) ->
          {name, t, nameList}

        seq when elem(seq, 0) === :SEQUENCE ->
          {:SEQUENCE, r_SEQUENCE(seq, :components), nameList}

        set when elem(set, 0) === :SET ->
          {:SET, r_SET(set, :components), nameList}

        r_ObjectClassFieldType(type: t) ->
          {:ASN1_OPEN_TYPE, t, nameList}
      end

    {:asn1ct_gen.unify_if_string(innerType), newType, newNameList}
  end

  defp check_ptype(s, type, ts) when elem(ts, 0) === :type do
    check_formal_parameters(s, r_ptypedef(type, :args))
    def__ = r_type(ts, :def)

    newDef =
      case def__ do
        seq when elem(seq, 0) === :SEQUENCE ->
          components = expand_components(s, r_SEQUENCE(seq, :components))

          r_newt(
            type:
              r_SEQUENCE(seq,
                pname: get_datastr_name(type),
                components: components
              )
          )

        set when elem(set, 0) === :SET ->
          components = expand_components(s, r_SET(set, :components))

          r_newt(
            type:
              r_SET(set,
                pname: get_datastr_name(type),
                components: components
              )
          )

        _Other ->
          r_newt()
      end

    ts2 =
      case newDef do
        r_newt(type: :unchanged) ->
          ts

        r_newt(type: tDef) ->
          r_type(ts, def: tDef)
      end

    ts2
  end

  defp check_ptype(_S, _PTDef, ts)
       when elem(ts, 0) === :objectclass do
    throw({:asn1_param_class, ts})
  end

  defp check_formal_parameters(s, args) do
    _ =
      for a <- args do
        check_formal_parameter(s, a)
      end

    :ok
  end

  defp check_formal_parameter(_, {_, _}) do
    :ok
  end

  defp check_formal_parameter(_, r_Externaltypereference()) do
    :ok
  end

  defp check_formal_parameter(s, r_Externalvaluereference(value: name)) do
    asn1_error(s, {:illegal_typereference, name})
  end

  defp check_type(_S, type, ts)
       when elem(type, 0) === :typedef and
              r_typedef(type, :checked) == true do
    ts
  end

  defp check_type(_S, type, ts)
       when elem(type, 0) === :typedef and
              r_typedef(type, :checked) == :idle do
    ts
  end

  defp check_type(s = r_state(recordtopname: topName), type, ts)
       when elem(ts, 0) === :type do
    {def__, tag, constr, isInlined} =
      case match_parameter(s, r_type(ts, :def)) do
        r_type(tag: pTag, constraint: _Ctmp, def: dtmp, inlined: inl) ->
          {dtmp, merge_tags(r_type(ts, :tag), pTag), r_type(ts, :constraint), inl}

        r_typedef(typespec: r_type(tag: pTag, def: dtmp, inlined: inl)) ->
          {dtmp, merge_tags(r_type(ts, :tag), pTag), r_type(ts, :constraint), inl}

        dtmp ->
          {dtmp, r_type(ts, :tag), r_type(ts, :constraint), r_type(ts, :inlined)}
      end

    tempNewDef = r_newt(type: def__, tag: tag, constraint: constr, inlined: isInlined)

    testFun = fn tref ->
      {_, maybeChoice} = get_referenced_type(s, tref, true)

      case (try do
              r_type(r_typedef(maybeChoice, :typespec), :def)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:CHOICE, _} ->
          maybe_illicit_implicit_tag(s, :choice, tag)

        :ANY ->
          maybe_illicit_implicit_tag(s, :open_type, tag)

        :"ANY DEFINED BY" ->
          maybe_illicit_implicit_tag(s, :open_type, tag)

        :ASN1_OPEN_TYPE ->
          maybe_illicit_implicit_tag(s, :open_type, tag)

        _ ->
          tag
      end
    end

    newDef =
      case def__ do
        ext when elem(ext, 0) === :Externaltypereference ->
          {refMod, refTypeDef, isParamDef} =
            case get_referenced_type(s, ext) do
              {:undefined, tmpTDef} ->
                {:erlang.get(:top_module), tmpTDef, true}

              {tmpRefMod, tmpRefDef} ->
                {tmpRefMod, tmpRefDef, false}
            end

          case get_class_def(s, refTypeDef) do
            :none ->
              :ok

            r_classdef() ->
              throw({:asn1_class, refTypeDef})
          end

          ct = testFun.(ext)

          {refType, extRef} =
            case r_typedef(refTypeDef, :checked) do
              true ->
                {r_typedef(refTypeDef, :typespec), ext}

              _ ->
                newRefTypeDef1 = r_typedef(refTypeDef, checked: :idle)

                :asn1_db.dbput(
                  refMod,
                  get_datastr_name(newRefTypeDef1),
                  newRefTypeDef1
                )

                newS =
                  r_state(s,
                    mname: refMod,
                    module:
                      load_asn1_module(
                        s,
                        refMod
                      ),
                    tname: get_datastr_name(newRefTypeDef1),
                    abscomppath: [],
                    recordtopname: []
                  )

                refType1 =
                  check_type(
                    newS,
                    refTypeDef,
                    r_typedef(refTypeDef, :typespec)
                  )

                newRefTypeDef2 =
                  r_typedef(refTypeDef,
                    checked: true,
                    typespec: refType1
                  )

                tmpName = get_datastr_name(newRefTypeDef2)
                :asn1_db.dbput(refMod, tmpName, newRefTypeDef2)

                case {refMod == :erlang.get(:top_module), isParamDef} do
                  {true, true} ->
                    key = {tmpName, :type, newRefTypeDef2}

                    :asn1ct_gen.insert_once(
                      :parameterized_objects,
                      key
                    )

                  _ ->
                    :ok
                end

                pos = r_Externaltypereference(ext, :pos)
                {refType1, r_Externaltypereference(module: refMod, pos: pos, type: tmpName)}
            end

          case :asn1ct_gen.prim_bif(:asn1ct_gen.get_inner(r_type(refType, :def))) do
            true ->
              newC = check_constraints(s, refType, constr ++ r_type(refType, :constraint))

              r_newt(tempNewDef,
                type: r_type(refType, :def),
                tag: merge_tags(ct, r_type(refType, :tag)),
                constraint: newC
              )

            _ ->
              newExt = r_Externaltypereference(extRef, module: merged_mod(s, refMod, ext))

              r_newt(tempNewDef,
                type:
                  check_externaltypereference(
                    s,
                    newExt
                  ),
                tag: merge_tags(ct, r_type(refType, :tag))
              )
          end

        :ANY ->
          ct = maybe_illicit_implicit_tag(s, :open_type, tag)
          r_newt(tempNewDef, type: :ASN1_OPEN_TYPE, tag: ct)

        {:ANY_DEFINED_BY, _} ->
          ct = maybe_illicit_implicit_tag(s, :open_type, tag)
          r_newt(tempNewDef, type: :ASN1_OPEN_TYPE, tag: ct)

        :INTEGER ->
          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 2,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        {:INTEGER, namedNumberList} ->
          r_newt(tempNewDef,
            type: {:INTEGER, check_integer(s, namedNumberList)},
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 2,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        :REAL ->
          check_real(s, constr)

          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 9,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        {:"BIT STRING", namedNumberList} ->
          newL = check_bitstring(s, namedNumberList)

          r_newt(tempNewDef,
            type: {:"BIT STRING", newL},
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 3,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        :NULL ->
          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 5,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        :"OBJECT IDENTIFIER" ->
          check_objectidentifier(s, constr)

          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 6,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        :ObjectDescriptor ->
          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 7,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        :EXTERNAL ->
          put_once(:external, :unchecked)

          r_newt(tempNewDef,
            type:
              r_Externaltypereference(
                module: r_state(s, :mname),
                type: :EXTERNAL
              ),
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 8,
                  type: :IMPLICIT,
                  form: 32
                )
              )
          )

        {:"INSTANCE OF", definedObjectClass, constraint} ->
          iOFDef = check_instance_of(s, definedObjectClass, constraint)

          r_newt(tempNewDef,
            type: iOFDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 8,
                  type: :IMPLICIT,
                  form: 32
                )
              )
          )

        {:ENUMERATED, namedNumberList} ->
          r_newt(tempNewDef,
            type: {:ENUMERATED, check_enumerated(s, namedNumberList)},
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 10,
                  type: :IMPLICIT,
                  form: 0
                )
              ),
            constraint: []
          )

        :"EMBEDDED PDV" ->
          put_once(:embedded_pdv, :unchecked)

          r_newt(tempNewDef,
            type: r_Externaltypereference(module: r_state(s, :mname), type: :"EMBEDDED PDV"),
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 11,
                  type: :IMPLICIT,
                  form: 32
                )
              )
          )

        :BOOLEAN ->
          check_boolean(s, constr)

          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 1,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        :"OCTET STRING" ->
          check_octetstring(s, constr)

          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 4,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        :NumericString ->
          check_restrictedstring(s, def__, constr)

          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 18,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        tString
        when tString === :TeletexString or
               tString === :T61String ->
          check_restrictedstring(s, def__, constr)

          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 20,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        :VideotexString ->
          check_restrictedstring(s, def__, constr)

          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 21,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        :UTCTime ->
          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 23,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        :GeneralizedTime ->
          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 24,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        :GraphicString ->
          check_restrictedstring(s, def__, constr)

          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 25,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        :VisibleString ->
          check_restrictedstring(s, def__, constr)

          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 26,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        :GeneralString ->
          check_restrictedstring(s, def__, constr)

          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 27,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        :PrintableString ->
          check_restrictedstring(s, def__, constr)

          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 19,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        :IA5String ->
          check_restrictedstring(s, def__, constr)

          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 22,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        :BMPString ->
          check_restrictedstring(s, def__, constr)

          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 30,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        :UniversalString ->
          check_restrictedstring(s, def__, constr)

          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 28,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        :UTF8String ->
          check_restrictedstring(s, def__, constr)

          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 12,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        :"RELATIVE-OID" ->
          check_relative_oid(s, constr)

          r_newt(tempNewDef,
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 13,
                  type: :IMPLICIT,
                  form: 0
                )
              )
          )

        :"CHARACTER STRING" ->
          put_once(:character_string, :unchecked)

          r_newt(tempNewDef,
            type: r_Externaltypereference(module: r_state(s, :mname), type: :"CHARACTER STRING"),
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 29,
                  type: :IMPLICIT,
                  form: 32
                )
              )
          )

        seq when elem(seq, 0) === :SEQUENCE ->
          recordName =
            case topName do
              [] ->
                [get_datastr_name(type)]

              _ ->
                topName
            end

          {tableCInf, components} =
            check_sequence(
              r_state(s, recordtopname: recordName),
              type,
              r_SEQUENCE(seq, :components)
            )

          r_newt(tempNewDef,
            type:
              r_SEQUENCE(seq,
                tablecinf:
                  tablecinf_choose(
                    seq,
                    tableCInf
                  ),
                components: components
              ),
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 16,
                  type: :IMPLICIT,
                  form: 32
                )
              )
          )

        {:"SEQUENCE OF", components} ->
          r_newt(tempNewDef,
            type: {:"SEQUENCE OF", check_sequenceof(s, type, components)},
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 16,
                  type: :IMPLICIT,
                  form: 32
                )
              )
          )

        {:CHOICE, _} = choice ->
          ct = maybe_illicit_implicit_tag(s, :choice, tag)
          components = get_choice_components(s, choice)

          r_newt(tempNewDef,
            type: {:CHOICE, check_choice(s, type, components)},
            tag: ct
          )

        set when elem(set, 0) === :SET ->
          recordName =
            case topName do
              [] ->
                [get_datastr_name(type)]

              _ ->
                topName
            end

          {sorted, tableCInf, components} =
            check_set(r_state(s, recordtopname: recordName), type, r_SET(set, :components))

          r_newt(tempNewDef,
            type:
              r_SET(set,
                sorted: sorted,
                tablecinf:
                  tablecinf_choose(
                    set,
                    tableCInf
                  ),
                components: components
              ),
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 17,
                  type: :IMPLICIT,
                  form: 32
                )
              )
          )

        {:"SET OF", components} ->
          r_newt(tempNewDef,
            type: {:"SET OF", check_setof(s, type, components)},
            tag:
              merge_tags(
                tag,
                r_tag(
                  class: :UNIVERSAL,
                  number: 17,
                  type: :IMPLICIT,
                  form: 32
                )
              )
          )

        {:pt, ptype, paraList} ->
          {_RefMod, ptypedef} = get_referenced_type(s, ptype)
          notify_if_not_ptype(s, ptypedef)
          newParaList = match_parameters(s, paraList)
          instance = instantiate_ptype(s, ptypedef, newParaList)

          r_newt(tempNewDef,
            type: r_type(instance, :def),
            tag: merge_tags(tag, r_type(instance, :tag)),
            constraint: r_type(instance, :constraint),
            inlined: :yes
          )

        r_ObjectClassFieldType(classname: clRef0) = oCFT0 ->
          clRef = match_parameter(s, clRef0)
          oCFT = r_ObjectClassFieldType(oCFT0, classname: clRef)
          classSpec = check_class(s, clRef)

          newTypeDef =
            maybe_open_type(s, classSpec, r_ObjectClassFieldType(oCFT, class: classSpec), constr)

          innerTag = get_innertag(s, newTypeDef)
          mergedTag = merge_tags(tag, innerTag)

          ct =
            case is_open_type(newTypeDef) do
              true ->
                maybe_illicit_implicit_tag(s, :open_type, mergedTag)

              _ ->
                mergedTag
            end

          case topName do
            [] when r_typedef(type, :name) !== :undefined ->
              r_type(
                constraint: c,
                def: simplified
              ) =
                simplify_type(
                  r_type(
                    def: newTypeDef,
                    constraint: constr
                  )
                )

              r_newt(tempNewDef, type: simplified, tag: ct, constraint: c)

            _ ->
              r_newt(tempNewDef, type: newTypeDef, tag: ct)
          end

        {:TypeFromObject, {:object, object}, typeField} ->
          checkedT = get_type_from_object(s, object, typeField)

          r_newt(tempNewDef,
            tag: merge_tags(tag, r_type(checkedT, :tag)),
            type: r_type(checkedT, :def)
          )

        {:SelectionType, name, t} ->
          checkedT = check_selectiontype(s, name, t)

          r_newt(tempNewDef,
            tag: merge_tags(tag, r_type(checkedT, :tag)),
            type: r_type(checkedT, :def)
          )

        :ASN1_OPEN_TYPE ->
          tempNewDef
      end

    r_newt(type: tDef, tag: newTags, constraint: newConstr, inlined: inlined) = newDef

    r_type(ts,
      def: tDef,
      inlined: inlined,
      constraint: check_constraints(s, r_type(def: tDef), newConstr),
      tag:
        :lists.map(
          fn
            r_tag(type: {:default, tTx}) = tempTag ->
              r_tag(tempTag, type: tTx)

            other ->
              other
          end,
          newTags
        )
    )
  end

  defp simplify_comps(comps) do
    for comp <- comps do
      simplify_comp(comp)
    end
  end

  defp simplify_comp(r_ComponentType(typespec: type0) = c) do
    type = simplify_type(type0)
    r_ComponentType(c, typespec: type)
  end

  defp simplify_comp(other) do
    other
  end

  defp simplify_type(r_type(tag: tag, def: inner, constraint: constr0) = t) do
    case inner do
      r_ObjectClassFieldType(type: {:fixedtypevaluefield, _, type}) = oCFT ->
        constr = [
          {:ocft, oCFT}
          | r_type(type, :constraint) ++ constr0
        ]

        r_type(type, tag: tag, constraint: constr)

      _ ->
        t
    end
  end

  defp tablecinf_choose(setOrSeq, false) do
    tablecinf_choose(setOrSeq)
  end

  defp tablecinf_choose(_, tableCInf) do
    tableCInf
  end

  defp tablecinf_choose(r_SET(tablecinf: tCI)) do
    tCI
  end

  defp tablecinf_choose(r_SEQUENCE(tablecinf: tCI)) do
    tCI
  end

  defp get_innertag(_S, r_ObjectClassFieldType(type: type)) do
    case type do
      {:fixedtypevaluefield, _, r_type(tag: tag)} ->
        tag

      {typeFieldName, _} when is_atom(typeFieldName) ->
        []

      _ ->
        []
    end
  end

  defp get_class_def(s, r_typedef(typespec: r_type(def: r_Externaltypereference() = eref))) do
    {_, nextDef} = get_referenced_type(s, eref, true)
    get_class_def(s, nextDef)
  end

  defp get_class_def(s, r_Externaltypereference() = eref) do
    {_, nextDef} = get_referenced_type(s, eref, true)
    get_class_def(s, nextDef)
  end

  defp get_class_def(_S, r_classdef() = cD) do
    cD
  end

  defp get_class_def(_S, _) do
    :none
  end

  defp maybe_illicit_implicit_tag(s, kind, tag) do
    case tag do
      [r_tag(type: :IMPLICIT) | _T] ->
        asn1_error(s, {:implicit_tag_before, kind})

      [chTag = r_tag(type: {:default, _}) | t] ->
        case kind do
          :open_type ->
            [r_tag(chTag, type: :EXPLICIT, form: 32) | t]

          :choice ->
            [r_tag(chTag, type: :EXPLICIT, form: 32) | t]
        end

      _ ->
        tag
    end
  end

  defp merged_mod(s, refMod, ext) do
    case s do
      r_state(inputmodules: []) ->
        refMod

      _ ->
        r_Externaltypereference(ext, :module)
    end
  end

  defp maybe_open_type(_, _, r_ObjectClassFieldType(fieldname: {_, _}) = oCFT, _) do
    oCFT
  end

  defp maybe_open_type(
         s,
         r_objectclass(fields: fs) = classSpec,
         r_ObjectClassFieldType(fieldname: fieldRefList) = oCFT,
         constr
       ) do
    type = get_OCFType(s, fs, fieldRefList)
    fieldNames = get_referenced_fieldname(fieldRefList)

    case :lists.last(fieldRefList) do
      {:valuefieldreference, _} ->
        r_ObjectClassFieldType(oCFT, fieldname: fieldNames, type: type)

      {:typefieldreference, _} ->
        case {get_unique_fieldname(s, r_classdef(typespec: classSpec)),
              get_componentrelation(constr)} do
          {:no_unique, _} ->
            r_ObjectClassFieldType(oCFT, fieldname: fieldNames, type: :ASN1_OPEN_TYPE)

          {_, :no} ->
            r_ObjectClassFieldType(oCFT, fieldname: fieldNames, type: :ASN1_OPEN_TYPE)

          _ ->
            r_ObjectClassFieldType(oCFT, fieldname: fieldNames, type: type)
        end
    end
  end

  defp get_componentrelation([
         {:element_set, {:componentrelation, _, _} = cr, :none}
         | _
       ]) do
    cr
  end

  defp get_componentrelation([_ | t]) do
    get_componentrelation(t)
  end

  defp get_componentrelation([]) do
    :no
  end

  defp is_open_type(r_ObjectClassFieldType(type: :ASN1_OPEN_TYPE)) do
    true
  end

  defp is_open_type(r_ObjectClassFieldType()) do
    false
  end

  defp notify_if_not_ptype(s, r_pvaluesetdef(type: type)) do
    case r_type(type, :def) do
      ref when elem(ref, 0) === :Externaltypereference ->
        case get_referenced_type(s, ref) do
          {_, r_classdef()} ->
            throw(:pobjectsetdef)

          {_, r_typedef()} ->
            throw(:pvalueset)
        end

      t when elem(t, 0) === :type ->
        throw(:pvalueset)
    end
  end

  defp notify_if_not_ptype(_S, pT = r_ptypedef()) do
    case r_ptypedef(pT, :typespec) do
      r_objectclass() ->
        throw({:asn1_class, pT})

      _ ->
        :ok
    end
  end

  defp notify_if_not_ptype(s, r_pobjectsetdef(class: cl)) do
    case cl do
      r_Externaltypereference() ->
        case get_referenced_type(s, cl) do
          {_, r_classdef()} ->
            throw(:pobjectsetdef)

          {_, r_typedef()} ->
            throw(:pvalueset)
        end

      _ ->
        throw(:pobjectsetdef)
    end
  end

  defp notify_if_not_ptype(s, pT) do
    asn1_error(s, {:param_bad_type, error_value(pT)})
  end

  defp instantiate_ptype(s, ptypedef, paraList) do
    r_ptypedef(args: args, typespec: type) = ptypedef
    newType = check_ptype(s, ptypedef, r_type(type, inlined: :yes))
    matchedArgs = match_args(s, args, paraList, [])
    oldArgs = r_state(s, :parameters)

    newS =
      r_state(s,
        parameters: matchedArgs ++ oldArgs,
        abscomppath: []
      )

    check_type(newS, r_ptypedef(ptypedef, typespec: newType), newType)
  end

  defp get_datastr_name(type) do
    :asn1ct.get_name_of_def(type)
  end

  defp get_pt_args(r_ptypedef(args: args)) do
    args
  end

  defp get_pt_args(r_pvaluesetdef(args: args)) do
    args
  end

  defp get_pt_args(r_pvaluedef(args: args)) do
    args
  end

  defp get_pt_args(r_pobjectdef(args: args)) do
    args
  end

  defp get_pt_args(r_pobjectsetdef(args: args)) do
    args
  end

  defp get_pt_spec(r_ptypedef(typespec: type)) do
    type
  end

  defp get_pt_spec(r_pvaluedef(value: value)) do
    value
  end

  defp get_pt_spec(r_pvaluesetdef(valueset: vS)) do
    vS
  end

  defp get_pt_spec(r_pobjectdef(def: def__)) do
    def__
  end

  defp get_pt_spec(r_pobjectsetdef(def: def__)) do
    def__
  end

  defp match_args(s, fA = [formArg | ft], aA = [actArg | at], acc) do
    oldParams = r_state(s, :parameters)

    case categorize_arg(s, formArg, actArg) do
      [categorizedArg] ->
        match_args(
          r_state(s,
            parameters: [
              {formArg, categorizedArg}
              | oldParams
            ]
          ),
          ft,
          at,
          [{formArg, categorizedArg} | acc]
        )

      categorizedArgs ->
        match_args(
          r_state(s, parameters: categorizedArgs ++ oldParams),
          fA,
          categorizedArgs ++ aA,
          acc
        )
    end
  end

  defp match_args(_S, [], [], acc) do
    :lists.reverse(acc)
  end

  defp match_args(s, _, _, _) do
    asn1_error(s, :param_wrong_number_of_arguments)
  end

  defp categorize_arg(s, {governor, param}, actArg) do
    case {governor_category(s, governor), parameter_name_style(param)} do
      {:type, :beginning_lowercase} ->
        categorize(s, :value, governor, actArg)

      {:type, :beginning_uppercase} ->
        categorize(actArg)

      {{:class, classRef}, :beginning_lowercase} ->
        categorize(s, :object, actArg, classRef)

      {{:class, classRef}, :beginning_uppercase} ->
        categorize(s, :object_set, actArg, classRef)
    end
  end

  defp categorize_arg(_S, _FormalArg, actualArg) do
    categorize(actualArg)
  end

  defp governor_category(s, r_type(def: r_Externaltypereference() = eref)) do
    governor_category(s, eref)
  end

  defp governor_category(_S, r_type()) do
    :type
  end

  defp governor_category(s, r_Externaltypereference() = ref) do
    case get_class_def(s, ref) do
      r_classdef(pos: pos, module: mod, name: name) ->
        {:class, r_Externaltypereference(pos: pos, module: mod, type: name)}

      :none ->
        :type
    end
  end

  defp parameter_name_style(r_Externaltypereference()) do
    :beginning_uppercase
  end

  defp parameter_name_style(r_Externalvaluereference()) do
    :beginning_lowercase
  end

  defp categorize({:object, _, type}) do
    def__ = fn
      d = r_type() ->
        r_typedef(
          name: new_reference_name('type_argument'),
          typespec: r_type(d, inlined: :yes)
        )

      {:setting, _, eref}
      when elem(eref, 0) === :Externaltypereference ->
        eref

      d ->
        d
    end

    for x <- type do
      def__.(x)
    end
  end

  defp categorize(r_type() = def__) do
    [
      r_typedef(
        name: new_reference_name('type_argument'),
        typespec: r_type(def__, inlined: :yes)
      )
    ]
  end

  defp categorize(def__) do
    [def__]
  end

  defp categorize(s, :object_set, def__, classRef) do
    newObjSetSpec =
      check_object(
        s,
        def__,
        r_ObjectSet(
          class: classRef,
          set: parse_objectset(def__)
        )
      )

    name = new_reference_name('object_set_argument')
    [save_object_set_instance(s, name, newObjSetSpec)]
  end

  defp categorize(_S, :object, def__, _ClassRef) do
    [def__]
  end

  defp categorize(_S, :value, _Type, value)
       when elem(value, 0) === :valuedef do
    [value]
  end

  defp categorize(s, :value, type, value) do
    [r_valuedef(type: type, value: value, module: r_state(s, :mname))]
  end

  defp parse_objectset({:valueset, r_type(def: r_Externaltypereference() = ref)}) do
    ref
  end

  defp parse_objectset({:valueset, set}) do
    set
  end

  defp parse_objectset(r_type(def: ref))
       when elem(ref, 0) === :Externaltypereference do
    ref
  end

  defp parse_objectset(set) do
    set
  end

  defp check_constraints(_S, _HostType, []) do
    []
  end

  defp check_constraints(s, hostType0, [_ | _] = cs0) do
    hostType = get_real_host_type(hostType0, cs0)
    cs1 = top_level_intersections(cs0)

    cs2 =
      for c <- cs1 do
        coalesce_constraints(c)
      end

    {_, cs3} = filter_extensions(cs2)
    cs = simplify_element_sets(s, hostType, cs3)
    finish_constraints(cs)
  end

  defp get_real_host_type(hostType, cs) do
    case :lists.keyfind(:ocft, 1, cs) do
      false ->
        hostType

      {_, oCFT} ->
        r_type(hostType, def: oCFT)
    end
  end

  defp top_level_intersections([{:element_set, {:intersection, _, _} = c, :none}]) do
    top_level_intersections_1(c)
  end

  defp top_level_intersections(cs) do
    cs
  end

  defp top_level_intersections_1({:intersection, a, b}) do
    [
      {:element_set, a, :none}
      | top_level_intersections_1(b)
    ]
  end

  defp top_level_intersections_1(other) do
    [{:element_set, other, :none}]
  end

  defp coalesce_constraints(
         {:element_set, {tag, {:element_set, a, _}}, {tag, {:element_set, b, _}}}
       ) do
    {:element_set, {tag, {:element_set, a, b}}, :none}
  end

  defp coalesce_constraints(other) do
    other
  end

  defp filter_extensions([h0 | t0]) do
    case filter_extensions(t0) do
      {true, t} ->
        h = remove_extension(h0)
        {true, [h | t]}

      {false, t} ->
        {any_extension(h0), [h0 | t]}
    end
  end

  defp filter_extensions([]) do
    {false, []}
  end

  defp remove_extension({:element_set, root, _}) do
    {:element_set, remove_extension(root), :none}
  end

  defp remove_extension(tuple) when is_tuple(tuple) do
    l =
      for el <- :erlang.tuple_to_list(tuple) do
        remove_extension(el)
      end

    :erlang.list_to_tuple(l)
  end

  defp remove_extension(other) do
    other
  end

  defp any_extension({:element_set, _, ext}) when ext !== :none do
    true
  end

  defp any_extension(tuple) when is_tuple(tuple) do
    any_extension_tuple(1, tuple)
  end

  defp any_extension(_) do
    false
  end

  defp any_extension_tuple(i, t) when i <= tuple_size(t) do
    any_extension(
      :erlang.element(
        i,
        t
      )
    ) or any_extension_tuple(i + 1, t)
  end

  defp any_extension_tuple(_, _) do
    false
  end

  defp simplify_element_sets(s, hostType, [{:element_set, r0, e0} | t0]) do
    r1 = simplify_element_set(s, hostType, r0)
    e1 = simplify_element_set(s, hostType, e0)

    case simplify_element_sets(s, hostType, t0) do
      [{:element_set, r2, e2}] ->
        [{:element_set, cs_intersection(s, r1, r2), cs_intersection(s, e1, e2)}]

      l when is_list(l) ->
        [{:element_set, r1, e1} | l]
    end
  end

  defp simplify_element_sets(s, hostType, [h | t]) do
    [h | simplify_element_sets(s, hostType, t)]
  end

  defp simplify_element_sets(_, _, []) do
    []
  end

  defp simplify_element_set(_S, _HostType, :empty) do
    {:set, []}
  end

  defp simplify_element_set(s, hostType, {:SingleValue, vs0})
       when is_list(vs0) do
    vs1 =
      for v <- vs0 do
        resolve_value(s, hostType, v)
      end

    vs = make_constr_set_vs(vs1)
    simplify_element_set(s, hostType, vs)
  end

  defp simplify_element_set(s, hostType, {:SingleValue, v0}) do
    v1 = resolve_value(s, hostType, v0)
    v = {:set, [{:range, v1, v1}]}
    simplify_element_set(s, hostType, v)
  end

  defp simplify_element_set(s, hostType, {:ValueRange, {lb0, ub0}}) do
    lb = resolve_value(s, hostType, lb0)
    ub = resolve_value(s, hostType, ub0)
    v = make_constr_set(s, lb, ub)
    simplify_element_set(s, hostType, v)
  end

  defp simplify_element_set(s, hostType, {:"ALL-EXCEPT", set0}) do
    set = simplify_element_set(s, hostType, set0)
    {:"ALL-EXCEPT", set}
  end

  defp simplify_element_set(s, hostType, {:intersection, a0, b0}) do
    a = simplify_element_set(s, hostType, a0)
    b = simplify_element_set(s, hostType, b0)
    cs_intersection(s, a, b)
  end

  defp simplify_element_set(s, hostType, {:union, a0, b0}) do
    a = simplify_element_set(s, hostType, a0)
    b = simplify_element_set(s, hostType, b0)
    cs_union(s, a, b)
  end

  defp simplify_element_set(s, hostType, {:simpletable, {:element_set, type, _}}) do
    check_simpletable(s, hostType, type)
  end

  defp simplify_element_set(s, _, {:componentrelation, r, id}) do
    check_componentrelation(s, r, id)
  end

  defp simplify_element_set(s, hostType, {tag, {:element_set, _, _} = el0}) do
    [el1] = simplify_element_sets(s, hostType, [el0])
    {tag, el1}
  end

  defp simplify_element_set(s, hostType, r_type() = type) do
    simplify_element_set_type(s, hostType, type)
  end

  defp simplify_element_set(_, _, c) do
    c
  end

  defp simplify_element_set_type(s, hostType, r_type(def: def0) = type0) do
    r_Externaltypereference() = def0

    case get_referenced_type(s, def0) do
      {_, r_valuedef(checked: false, value: {:valueset, vs0})} ->
        [vs1] = simplify_element_sets(s, hostType, [vs0])

        case vs1 do
          {:element_set, set, :none} ->
            set

          {:element_set, set, {:set, []}} ->
            set
        end

      {_, {:valueset, r_type(def: r_Externaltypereference()) = type}} ->
        simplify_element_set_type(s, hostType, type)

      _ ->
        case hostType do
          r_type(def: r_ObjectClassFieldType()) ->
            r_type(def: def__) = check_type(s, hostType, type0)
            def__

          _ ->
            r_type(constraint: cs) = check_type(s, hostType, type0)
            c = convert_back(cs)
            simplify_element_set(s, hostType, c)
        end
    end
  end

  defp convert_back([[h1, h2] | t]) do
    {:intersection, h1, convert_back([h2 | t])}
  end

  defp convert_back([h]) do
    h
  end

  defp convert_back([]) do
    :none
  end

  defp check_simpletable(s, hostType, type) do
    case hostType do
      r_type(def: r_ObjectClassFieldType()) ->
        :ok

      _ ->
        asn1_error(s, :illegal_table_constraint)
    end

    def__ =
      case type do
        r_type(def: d) ->
          d

        {:SingleValue, r_Externalvaluereference() = objRef} ->
          objRef

        _ ->
          asn1_error(s, :invalid_table_constraint)
      end

    c = match_parameter(s, def__)

    case c do
      r_Externaltypereference() ->
        eRef = check_externaltypereference(s, c)
        {:simpletable, r_Externaltypereference(eRef, :type)}

      r_Externalvaluereference() ->
        {_, torVDef} = get_referenced_type(s, c)

        set =
          case torVDef do
            r_typedef(typespec: r_Object(classname: className)) ->
              r_ObjectSet(class: className, set: {:SingleValue, c})

            r_valuedef(type: r_type(def: classDef), value: r_Externalvaluereference() = obj) ->
              r_ObjectSet(class: classDef, set: {:SingleValue, obj})
          end

        {:simpletable, check_object(s, type, set)}

      {:ValueFromObject, {_, object}, fieldNames} ->
        {:simpletable, extract_field(s, object, fieldNames)}
    end
  end

  defp check_componentrelation(s, {:objectset, opos, objset0}, id) do
    objSet = match_parameter(s, objset0)
    ext = check_externaltypereference(s, objSet)
    {:componentrelation, {:objectset, opos, ext}, id}
  end

  defp make_constr_set(_, :MIN, ub) do
    {:set, [{:a_range, make_constr_set_val(ub)}]}
  end

  defp make_constr_set(_, lb, ub) when lb <= ub do
    {:set, [{:range, make_constr_set_val(lb), make_constr_set_val(ub)}]}
  end

  defp make_constr_set(s, _, _) do
    asn1_error(s, :reversed_range)
  end

  defp make_constr_set_val([c]) when is_integer(c) do
    c
  end

  defp make_constr_set_val(val) do
    val
  end

  defp make_constr_set_vs(vs) do
    {:set, make_constr_set_vs_1(vs)}
  end

  defp make_constr_set_vs_1([]) do
    []
  end

  defp make_constr_set_vs_1([v]) do
    [{:range, v, v}]
  end

  defp make_constr_set_vs_1([v0 | vs]) do
    v1 = make_constr_set_vs_1(vs)
    range_union([{:range, v0, v0}], v1)
  end

  defp cs_intersection(_S, other, :none) do
    other
  end

  defp cs_intersection(_S, :none, other) do
    other
  end

  defp cs_intersection(_S, {:set, setA}, {:set, setB}) do
    {:set, range_intersection(setA, setB)}
  end

  defp cs_intersection(_S, a, b) do
    {:intersection, a, b}
  end

  defp range_intersection([], []) do
    []
  end

  defp range_intersection([_ | _], []) do
    []
  end

  defp range_intersection([], [_ | _]) do
    []
  end

  defp range_intersection([h1 | _] = a, [h2 | _] = b) when h1 > h2 do
    range_intersection(b, a)
  end

  defp range_intersection([h1 | t1], [h2 | t2] = b) do
    case {h1, h2} do
      {{:a_range, ub0}, {:a_range, ub1}} when ub0 < ub1 ->
        [
          h1
          | range_intersection(
              t1,
              [{:range, ub0 + 1, ub1} | t2]
            )
        ]

      {{:a_range, _}, {:a_range, _}} ->
        [h1 | range_intersection(t1, t2)]

      {{:a_range, ub0}, {:range, lb1, _Ub1}} when ub0 < lb1 ->
        range_intersection(t1, b)

      {{:a_range, ub0}, {:range, lb1, ub1}} when ub0 < ub1 ->
        [
          {:range, lb1, ub0}
          | range_intersection(
              t1,
              [
                {:range, ub0 + 1, ub1}
                | t2
              ]
            )
        ]

      {{:a_range, ub}, {:range, _Lb1, ub}} ->
        [h2 | range_intersection(t1, t2)]

      {{:a_range, ub0}, {:range, _Lb1, ub1}} ->
        [
          h2
          | range_intersection(
              [{:range, ub1 + 1, ub0} | t1],
              t2
            )
        ]

      {{:range, _Lb0, ub0}, {:range, lb1, _Ub1}}
      when ub0 < lb1 ->
        range_intersection(t1, b)

      {{:range, _Lb0, ub0}, {:range, lb1, ub1}} when ub0 < ub1 ->
        [
          {:range, lb1, ub0}
          | range_intersection(
              t1,
              [
                {:range, ub0 + 1, ub1}
                | t2
              ]
            )
        ]

      {{:range, _Lb0, ub}, {:range, _Lb1, ub}} ->
        [h2 | range_intersection(t1, t2)]

      {{:range, _Lb0, ub0}, {:range, _Lb1, ub1}} ->
        [
          h2
          | range_intersection(
              [{:range, ub1 + 1, ub0} | t1],
              t2
            )
        ]
    end
  end

  defp cs_union(_S, {:set, setA}, {:set, setB}) do
    {:set, range_union(setA, setB)}
  end

  defp cs_union(_S, a, b) do
    {:union, a, b}
  end

  defp range_union(a, b) do
    range_union_1(:lists.merge(a, b))
  end

  defp range_union_1([[{:a_range, ub0}, {:a_range, ub1}] | t]) do
    range_union_1([{:a_range, max(ub0, ub1)} | t])
  end

  defp range_union_1([[{:a_range, ub0}, {:range, lb1, ub1}] | t])
       when lb1 - 1 <= ub0 do
    range_union_1([{:a_range, max(ub0, ub1)} | t])
  end

  defp range_union_1([{:a_range, _} = h | t]) do
    [h | range_union_1(t)]
  end

  defp range_union_1([[{:range, lb0, ub0}, {:range, lb1, ub1}] | t])
       when lb1 - 1 <= ub0 do
    range_union_1([{:range, lb0, max(ub0, ub1)} | t])
  end

  defp range_union_1([{:range, _, _} = h | t]) do
    [h | range_union_1(t)]
  end

  defp range_union_1([]) do
    []
  end

  defp finish_constraints(cs) do
    finish_constraints_1(cs, &smart_collapse/1)
  end

  defp finish_constraints_1(
         [
           {:element_set, {tag, {:element_set, _, _} = set0}, :none}
           | t
         ],
         collapse0
       ) do
    collapse = collapse_fun(tag)

    case finish_constraints_1([set0], collapse) do
      [] ->
        finish_constraints_1(t, collapse0)

      [set] ->
        [{tag, set} | finish_constraints_1(t, collapse0)]
    end
  end

  defp finish_constraints_1(
         [
           {:element_set, {:set, [{:a_range, :MAX}]}, _}
           | t
         ],
         collapse
       ) do
    finish_constraints_1(t, collapse)
  end

  defp finish_constraints_1(
         [
           {:element_set, {:intersection, a0, b0}, :none}
           | t
         ],
         collapse
       ) do
    a = {:element_set, a0, :none}
    b = {:element_set, b0, :none}
    finish_constraints_1([[a, b] | t], collapse)
  end

  defp finish_constraints_1([{:element_set, root, ext} | t], collapse) do
    case finish_constraint(root, ext, collapse) do
      :none ->
        finish_constraints_1(t, collapse)

      constr ->
        [constr | finish_constraints_1(t, collapse)]
    end
  end

  defp finish_constraints_1([h | t], collapse) do
    [h | finish_constraints_1(t, collapse)]
  end

  defp finish_constraints_1([], _) do
    []
  end

  defp finish_constraint({:set, root0}, ext, collapse) do
    case collapse.(root0) do
      :none ->
        :none

      root ->
        finish_constraint(root, ext, collapse)
    end
  end

  defp finish_constraint(root, ext, _Collapse) do
    case ext do
      :none ->
        root

      _ ->
        {root, []}
    end
  end

  defp collapse_fun(:SizeConstraint) do
    &size_constraint_collapse/1
  end

  defp collapse_fun(:PermittedAlphabet) do
    &single_value_collapse/1
  end

  defp single_value_collapse(v) do
    {:SingleValue, :ordsets.from_list(single_value_collapse_1(v))}
  end

  defp single_value_collapse_1([{:range, lb, ub} | t])
       when is_integer(lb) and
              is_integer(ub) do
    :lists.seq(lb, ub) ++ single_value_collapse_1(t)
  end

  defp single_value_collapse_1([]) do
    []
  end

  defp smart_collapse([{:a_range, ub}]) do
    {:ValueRange, {:MIN, ub}}
  end

  defp smart_collapse([{:a_range, _} | t]) do
    {:range, _, ub} = :lists.last(t)
    {:ValueRange, {:MIN, ub}}
  end

  defp smart_collapse([{:range, lb, ub}]) do
    {:ValueRange, {lb, ub}}
  end

  defp smart_collapse([_ | _] = l) do
    v =
      :lists.foldr(
        fn {:range, lb, ub}, a ->
          seq(lb, ub) ++ a
        end,
        [],
        l
      )

    {:SingleValue, v}
  end

  defp size_constraint_collapse([{:range, 0, :MAX}]) do
    :none
  end

  defp size_constraint_collapse(root) do
    [{:range, lb, _} | _] = root
    {:range, _, ub} = :lists.last(root)
    {lb, ub}
  end

  defp seq(same, same) do
    [same]
  end

  defp seq(lb, ub)
       when is_integer(lb) and
              is_integer(ub) do
    :lists.seq(lb, ub)
  end

  defp resolve_value(s, hostType, val) do
    id = match_parameter(s, val)
    resolve_value1(s, hostType, id)
  end

  defp resolve_value1(s, hostType, r_Externalvaluereference(value: name) = eRef) do
    case resolve_namednumber(s, hostType, name) do
      v when is_integer(v) ->
        v

      :not_named ->
        resolve_value1(s, hostType, get_referenced_value(s, eRef))
    end
  end

  defp resolve_value1(s, hostType, {:gt, v}) do
    case resolve_value1(s, hostType, v) do
      int when is_integer(int) ->
        int + 1

      _Other ->
        asn1_error(s, :illegal_integer_value)
    end
  end

  defp resolve_value1(s, hostType, {:lt, v}) do
    case resolve_value1(s, hostType, v) do
      int when is_integer(int) ->
        int - 1

      _Other ->
        asn1_error(s, :illegal_integer_value)
    end
  end

  defp resolve_value1(s, _HostType, {:ValueFromObject, {:object, object}, fieldName}) do
    get_value_from_object(s, object, fieldName)
  end

  defp resolve_value1(_, _, r_valuedef(checked: true, value: v)) do
    v
  end

  defp resolve_value1(s, _, r_valuedef(value: {:ValueFromObject, {:object, object}, fieldName})) do
    get_value_from_object(s, object, fieldName)
  end

  defp resolve_value1(s, _HostType, r_valuedef() = vDef) do
    r_valuedef(value: val) = check_value(s, vDef)
    val
  end

  defp resolve_value1(_, _, v) do
    v
  end

  defp resolve_namednumber(s, r_type(def: def__), name) do
    case def__ do
      {:ENUMERATED, nameList} ->
        resolve_namednumber_1(s, name, nameList)

      {:INTEGER, nameList} ->
        resolve_namednumber_1(s, name, nameList)

      _ ->
        :not_named
    end
  end

  defp resolve_namednumber_1(s, name, nameList) do
    try do
      namedNumberList = check_enumerated(s, nameList)
      {_, n} = lookup_enum_value(s, name, namedNumberList)
      n
    catch
      _, _ ->
        :not_named
    end
  end

  defp check_imported(s, imodule, name) do
    check_imported(s, imodule, name, false)
  end

  defp check_imported(s, imodule, name, isParsed) do
    case :asn1_db.dbget(imodule, :MODULE) do
      :undefined when isParsed == true ->
        errStr = :io_lib.format('Type ~s imported from non existing module ~s~n', [name, imodule])
        :erlang.error({:imported, errStr, s})

      :undefined ->
        parse_and_save(s, imodule)
        check_imported(s, imodule, name, true)

      im when elem(im, 0) === :module ->
        case is_exported(im, name) do
          false ->
            errStr =
              :io_lib.format('Imported type ~s not exported from module ~s~n', [name, imodule])

            :erlang.error({:imported, errStr, s})

          _ ->
            :ok
        end
    end

    :ok
  end

  defp is_exported(module, name)
       when elem(module, 0) === :module do
    {:exports, exports} = r_module(module, :exports)

    case exports do
      :all ->
        true

      [] ->
        false

      l when is_list(l) ->
        case :lists.keysearch(name, r_Externaltypereference(:type), exports) do
          false ->
            false

          _ ->
            true
        end
    end
  end

  defp check_externaltypereference(s, etref = r_Externaltypereference(module: emod)) do
    currmod = r_state(s, :mname)
    mergedMods = r_state(s, :inputmodules)

    case emod do
      ^currmod ->
        check_reference(s, etref)

      _ ->
        case :lists.member(emod, mergedMods) do
          true ->
            check_reference(s, etref)

          false ->
            {newMod, _} = get_referenced_type(s, etref)
            r_Externaltypereference(etref, module: newMod)
        end
    end
  end

  defp check_reference(s, r_Externaltypereference(pos: pos, module: emod, type: name)) do
    modName = r_state(s, :mname)

    case :asn1_db.dbget(modName, name) do
      :undefined ->
        case imported(s, name) do
          {:ok, imodule} ->
            check_imported(s, imodule, name)
            r_Externaltypereference(module: imodule, type: name)

          _ ->
            {m, t} = get_renamed_reference(s, name, emod)
            newName = :asn1ct.get_name_of_def(t)
            newPos = :asn1ct.get_pos_of_def(t)
            r_Externaltypereference(pos: newPos, module: m, type: newName)
        end

      _ ->
        r_Externaltypereference(pos: pos, module: modName, type: name)
    end
  end

  defp get_referenced_value(s, t) do
    case get_referenced_type(s, t) do
      {extMod, r_valuedef(value: r_Externalvaluereference() = ref)} ->
        get_referenced_value(update_state(s, extMod), ref)

      {_, r_valuedef(value: val)} ->
        val
    end
  end

  defp get_referenced_type(s, t) do
    get_referenced_type(s, t, false)
  end

  defp get_referenced_type(s, t, recurse) do
    case do_get_referenced_type(s, t) do
      {_, r_typedef(typespec: r_type(def: r_Externaltypereference() = eRef))} when recurse ->
        get_referenced_type(s, eRef, recurse)

      {_, _} = res ->
        res
    end
  end

  defp do_get_referenced_type(s, t0) do
    case match_parameter(s, t0) do
      ^t0 ->
        do_get_ref_type_1(s, t0)

      t ->
        do_get_referenced_type(s, t)
    end
  end

  defp do_get_ref_type_1(s, r_Externaltypereference(pos: p, module: m, type: t)) do
    do_get_ref_type_2(s, p, m, t)
  end

  defp do_get_ref_type_1(s, r_Externalvaluereference(pos: p, module: m, value: v)) do
    do_get_ref_type_2(s, p, m, v)
  end

  defp do_get_ref_type_1(_, t) do
    {:undefined, t}
  end

  defp do_get_ref_type_2(r_state(mname: current, inputmodules: modules) = s, pos, m, t) do
    case m === current or :lists.member(m, modules) do
      true ->
        get_referenced1(s, m, t, pos)

      false ->
        get_referenced(s, m, t, pos)
    end
  end

  defp get_referenced(s, emod, ename, pos) do
    :no_debug
    parse_and_save(s, emod)
    :no_debug

    case :asn1_db.dbget(emod, ename) do
      :undefined ->
        case :asn1_db.dbget(emod, :MODULE) do
          :undefined ->
            asn1_error(s, {:undefined_import, ename, emod})

          _ ->
            newS = update_state(s, emod)
            get_imported(newS, ename, emod, pos)
        end

      t when elem(t, 0) === :typedef ->
        :no_debug
        {emod, t}

      v ->
        :no_debug
        {emod, v}
    end
  end

  defp get_referenced1(s, moduleName, name, pos) do
    case :asn1_db.dbget(r_state(s, :mname), name) do
      :undefined ->
        get_imported(s, name, moduleName, pos)

      t ->
        {r_state(s, :mname), t}
    end
  end

  defp get_imported(s, name, module, pos) do
    :no_debug

    case imported(s, name) do
      {:ok, imodule} ->
        parse_and_save(s, imodule)

        case :asn1_db.dbget(imodule, :MODULE) do
          :undefined ->
            asn1_error(s, {:undefined_import, name, module})

          im when elem(im, 0) === :module ->
            case is_exported(im, name) do
              false ->
                asn1_error(s, {:undefined_export, name})

              _ ->
                :no_debug

                get_referenced_type(
                  s,
                  r_Externaltypereference(module: imodule, type: name, pos: pos)
                )
            end
        end

      _ ->
        get_renamed_reference(s, name, module)
    end
  end

  defp save_object_set_instance(s, name, objSetSpec)
       when elem(objSetSpec, 0) === :ObjectSet do
    newObjSet = r_typedef(checked: true, name: name, typespec: objSetSpec)
    :asn1_db.dbput(r_state(s, :mname), name, newObjSet)

    case objSetSpec do
      r_ObjectSet(uniquefname: {:unique, :undefined}) ->
        :ok

      _ ->
        objSetKey = {name, :objectset, newObjSet}
        insert_once(s, :parameterized_objects, objSetKey)
    end

    r_Externaltypereference(module: r_state(s, :mname), type: name)
  end

  defp load_asn1_module(r_state(mname: m, module: mod), m) do
    mod
  end

  defp load_asn1_module(_, m) do
    :asn1_db.dbget(m, :MODULE)
  end

  defp parse_and_save(s, module) when elem(s, 0) === :state do
    erule = r_state(s, :erule)

    case asn1db_member(s, erule, module) do
      true ->
        :ok

      _ ->
        case :asn1ct.parse_and_save(module, s) do
          :ok ->
            save_asn1db_uptodate(s, erule, module)

          err ->
            err
        end
    end
  end

  defp asn1db_member(s, erule, module) do
    asn1dbUTL = get_asn1db_uptodate(s)
    :lists.member({erule, module}, asn1dbUTL)
  end

  defp save_asn1db_uptodate(s, erule, module) do
    asn1dbUTL = get_asn1db_uptodate(s)
    asn1dbUTL2 = :lists.keydelete(module, 2, asn1dbUTL)
    put_asn1db_uptodate([{erule, module} | asn1dbUTL2])
  end

  defp get_asn1db_uptodate(s) do
    case :erlang.get(:asn1db_uptodate) do
      :undefined ->
        [{r_state(s, :erule), r_state(s, :mname)}]

      l ->
        l
    end
  end

  defp put_asn1db_uptodate(l) do
    :erlang.put(:asn1db_uptodate, l)
  end

  defp update_state(s, :undefined) do
    s
  end

  defp update_state(s = r_state(mname: moduleName), moduleName) do
    s
  end

  defp update_state(s, moduleName) do
    case :lists.member(moduleName, r_state(s, :inputmodules)) do
      true ->
        s

      _ ->
        parse_and_save(s, moduleName)
        mod = r_module() = :asn1_db.dbget(moduleName, :MODULE)
        r_state(s, mname: moduleName, module: mod)
    end
  end

  defp get_renamed_reference(s, name, module) do
    case renamed_reference(s, name, module) do
      :undefined ->
        asn1_error(s, {:undefined, name})

      newTypeName when newTypeName !== name ->
        get_referenced1(s, module, newTypeName, :undefined)
    end
  end

  defp renamed_reference(s, r_Externaltypereference(type: name, module: module)) do
    case renamed_reference(s, name, module) do
      :undefined ->
        name

      other ->
        other
    end
  end

  defp renamed_reference(s, name, module) do
    case :asn1ct_table.exists(:renamed_defs) do
      false ->
        :undefined

      true ->
        case :asn1ct_table.match(
               :renamed_defs,
               {:"$1", name, module}
             ) do
          [] ->
            case :asn1ct_table.exists(:original_imports) do
              false ->
                :undefined

              true ->
                case :asn1ct_table.match(
                       :original_imports,
                       {module, :"$1"}
                     ) do
                  [] ->
                    :undefined

                  [[importsList]] ->
                    case get_importmoduleoftype(importsList, name) do
                      :undefined ->
                        :undefined

                      nextMod ->
                        renamed_reference(s, name, nextMod)
                    end
                end
            end

          [[newTypeName]] ->
            newTypeName
        end
    end
  end

  defp get_importmoduleoftype([i | is], name) do
    index = r_Externaltypereference(:type)

    case :lists.keysearch(name, index, r_SymbolsFromModule(i, :symbols)) do
      {:value, _Ref} ->
        r_Externaltypereference(r_SymbolsFromModule(i, :module), :type)

      _ ->
        get_importmoduleoftype(is, name)
    end
  end

  defp get_importmoduleoftype([], _) do
    :undefined
  end

  defp match_parameters(s, names) do
    for name <- names do
      match_parameter(s, name)
    end
  end

  defp match_parameter(r_state(parameters: ps) = s, name) do
    match_parameter(s, name, ps)
  end

  defp match_parameter(_S, name, []) do
    name
  end

  defp match_parameter(s, {:valueset, {:element_set, r_type() = ts, :none}}, ps) do
    match_parameter(s, {:valueset, ts}, ps)
  end

  defp match_parameter(_S, r_Externaltypereference(type: name), [
         {r_Externaltypereference(type: name), newName} | _T
       ]) do
    newName
  end

  defp match_parameter(_S, r_Externaltypereference(type: name), [
         {{_, r_Externaltypereference(type: name)}, newName} | _T
       ]) do
    newName
  end

  defp match_parameter(_S, r_Externalvaluereference(value: name), [
         {r_Externalvaluereference(value: name), newName} | _T
       ]) do
    newName
  end

  defp match_parameter(_S, r_Externalvaluereference(value: name), [
         {{_, r_Externalvaluereference(value: name)}, newName} | _T
       ]) do
    newName
  end

  defp match_parameter(_S, r_type(def: r_Externaltypereference(module: m, type: name)), [
         {r_Externaltypereference(module: m, type: name), type}
       ]) do
    type
  end

  defp match_parameter(_S, {:valueset, r_type(def: r_Externaltypereference(type: name))}, [
         {{_, r_Externaltypereference(type: name)}, {:valueset, r_type(def: newName)}}
         | _T
       ]) do
    newName
  end

  defp match_parameter(_S, {:valueset, r_type(def: r_Externaltypereference(type: name))}, [
         {{_, r_Externaltypereference(type: name)},
          newName = r_type(def: r_Externaltypereference())}
         | _T
       ]) do
    r_type(newName, :def)
  end

  defp match_parameter(_S, {:valueset, r_type(def: r_Externaltypereference(type: name))}, [
         {{_, r_Externaltypereference(type: name)}, newName} | _T
       ]) do
    newName
  end

  defp match_parameter(s, {:valueset, t = r_type(def: {:pt, _, _Args})}, _Ps) do
    try do
      check_type(s, r_typedef(name: r_state(s, :tname), typespec: t), t)
    catch
      :pobjectsetdef ->
        {_, objRef, _Params} = r_type(t, :def)
        {_, objDef} = get_referenced_type(s, objRef)

        classRef =
          case objDef do
            r_pvaluesetdef(type: tDef) ->
              r_type(tDef, :def)

            r_pobjectsetdef(class: clRef) ->
              clRef
          end

        {homeMod, _} = get_referenced_type(s, classRef)
        rightClassRef = r_Externaltypereference(classRef, module: homeMod)
        objectSet = r_ObjectSet(class: rightClassRef, set: t)
        objSpec = check_object(s, r_typedef(typespec: objectSet), objectSet)

        name =
          :erlang.list_to_atom(
            :asn1ct_gen.list2name([
              get_datastr_name(objDef)
              | r_state(s, :recordtopname)
            ])
          )

        save_object_set_instance(s, name, objSpec)
    else
      r_type(def: ts) ->
        ts
    end
  end

  defp match_parameter(
         s,
         {:valueset, {:pos, {:objectset, _, pOSref}, args}},
         ps
       ) do
    match_parameter(s, {:valueset, r_type(def: {:pt, pOSref, args})}, ps)
  end

  defp match_parameter(s, name, [_H | t]) do
    match_parameter(s, name, t)
  end

  defp imported(s, name) do
    {:imports, ilist} = r_module(r_state(s, :module), :imports)
    imported1(name, ilist)
  end

  defp imported1(
         name,
         [
           r_SymbolsFromModule(
             symbols: symlist,
             module: r_Externaltypereference(type: moduleName)
           )
           | t
         ]
       ) do
    case :lists.keysearch(name, r_Externaltypereference(:type), symlist) do
      {:value, _V} ->
        {:ok, moduleName}

      _ ->
        imported1(name, t)
    end
  end

  defp imported1(_Name, []) do
    false
  end

  defp check_named_number_list(_S, []) do
    []
  end

  defp check_named_number_list(_S, [{_, _} | _] = nNL) do
    nNL
  end

  defp check_named_number_list(s, nNL0) do
    case check_unique(nNL0, 2) do
      [] ->
        nNL1 =
          for {:NamedNumber, id, val} <- nNL0 do
            {id, resolve_valueref(s, val)}
          end

        nNL = :lists.keysort(2, nNL1)

        case check_unique(nNL, 2) do
          [] ->
            nNL

          [val | _] ->
            asn1_error(s, {:value_reused, val})
        end

      [h | _] ->
        asn1_error(s, {:namelist_redefinition, h})
    end
  end

  defp resolve_valueref(s, r_Externalvaluereference() = t) do
    get_referenced_value(s, t)
  end

  defp resolve_valueref(_, val) when is_integer(val) do
    val
  end

  defp check_integer(s, nNL) do
    check_named_number_list(s, nNL)
  end

  defp check_bitstring(s, nNL0) do
    nNL = check_named_number_list(s, nNL0)

    _ =
      for {_, bit} <- nNL, bit < 0 do
        asn1_error(s, {:invalid_bit_number, bit})
      end

    nNL
  end

  defp check_real(_S, _Constr) do
    :ok
  end

  defp check_instance_of(s, definedObjectClass, constraint) do
    check_type_identifier(s, definedObjectClass)
    iof_associated_type(s, constraint)
  end

  defp check_type_identifier(s, eref = r_Externaltypereference(type: class)) do
    case get_referenced_type(s, eref) do
      {_, r_classdef(name: :"TYPE-IDENTIFIER")} ->
        :ok

      {_, r_classdef(typespec: r_Externaltypereference() = nextEref)} ->
        check_type_identifier(s, nextEref)

      {_, tD = r_typedef(typespec: r_type(def: r_Externaltypereference()))} ->
        check_type_identifier(s, r_type(r_typedef(tD, :typespec), :def))

      _ ->
        asn1_error(s, {:illegal_instance_of, class})
    end
  end

  defp iof_associated_type(s, []) do
    case :erlang.get(:instance_of) do
      :undefined ->
        associateSeq = iof_associated_type1(s, [])
        tag = [r_tag(class: :UNIVERSAL, number: 8, type: :IMPLICIT, form: 32)]

        typeDef =
          r_typedef(
            checked: true,
            name: :"INSTANCE OF",
            typespec: r_type(tag: tag, def: associateSeq)
          )

        :asn1_db.dbput(r_state(s, :mname), :"INSTANCE OF", typeDef)
        instance_of_decl(r_state(s, :mname))

      _ ->
        instance_of_decl(r_state(s, :mname))
        :ok
    end

    r_Externaltypereference(module: r_state(s, :mname), type: :"INSTANCE OF")
  end

  defp iof_associated_type(s, c) do
    iof_associated_type1(s, c)
  end

  defp iof_associated_type1(s, c) do
    {tableCInf, comp1Cnstr, comp2Cnstr, comp2tablecinf} = instance_of_constraints(s, c)
    moduleName = r_state(s, :mname)

    typefield_type =
      case c do
        [] ->
          :ASN1_OPEN_TYPE

        _ ->
          {:typefield, :Type}
      end

    objIdTag = [{:UNIVERSAL, 8}]
    c1TypeTag = [r_tag(class: :UNIVERSAL, number: 6, type: :IMPLICIT, form: 0)]
    typeIdentifierRef = r_Externaltypereference(module: moduleName, type: :"TYPE-IDENTIFIER")

    objectIdentifier =
      r_ObjectClassFieldType(
        classname: typeIdentifierRef,
        class: [],
        fieldname: {:id, []},
        type: {:fixedtypevaluefield, :id, r_type(def: :"OBJECT IDENTIFIER")}
      )

    typefield =
      r_ObjectClassFieldType(
        classname: typeIdentifierRef,
        class: [],
        fieldname: {:Type, []},
        type: typefield_type
      )

    iOFComponents0 = [
      r_ComponentType(
        name: :"type-id",
        typespec: r_type(tag: c1TypeTag, def: objectIdentifier, constraint: comp1Cnstr),
        prop: :mandatory,
        tags: objIdTag
      ),
      r_ComponentType(
        name: :value,
        typespec:
          r_type(
            tag: [r_tag(class: :CONTEXT, number: 0, type: :EXPLICIT, form: 32)],
            def: typefield,
            constraint: comp2Cnstr,
            tablecinf: comp2tablecinf
          ),
        prop: :mandatory,
        tags: [{:CONTEXT, 0}]
      )
    ]

    iOFComponents = textual_order(iOFComponents0)

    r_SEQUENCE(
      tablecinf: tableCInf,
      components: simplify_comps(iOFComponents)
    )
  end

  defp instance_of_constraints(_, []) do
    {false, [], [], []}
  end

  defp instance_of_constraints(
         s,
         [{:element_set, {:simpletable, c}, :none}]
       ) do
    {:element_set, type, :none} = c
    instance_of_constraints_1(s, type)
  end

  defp instance_of_constraints_1(s, type) do
    r_type(def: r_Externaltypereference(type: name)) = type
    moduleName = r_state(s, :mname)
    objectSetRef = r_Externaltypereference(module: moduleName, type: name)

    cRel = [
      {:componentrelation, {:objectset, :undefined, objectSetRef},
       [{:innermost, [r_Externalvaluereference(module: moduleName, value: :type)]}]}
    ]

    mod = r_state(s, :mname)

    tableCInf =
      r_simpletableattributes(
        objectsetname: {mod, name},
        c_name: :"type-id",
        c_index: 1,
        usedclassfield: :id,
        uniqueclassfield: :id,
        valueindex: []
      )

    {tableCInf, [{:simpletable, name}], cRel, [{:objfun, objectSetRef}]}
  end

  defp check_enumerated(_S, [{name, number} | _] = nNL)
       when is_atom(name) and is_integer(number) do
    nNL
  end

  defp check_enumerated(_S, {[{name, number} | _], l} = nNL)
       when is_atom(name) and is_integer(number) and
              is_list(l) do
    nNL
  end

  defp check_enumerated(s, nNL) do
    check_enum_ids(s, nNL, :gb_sets.empty())
    check_enum(s, nNL, :gb_sets.empty(), [])
  end

  defp check_enum_ids(s, [{:NamedNumber, id, _} | t], ids0) do
    ids = check_enum_update_ids(s, id, ids0)
    check_enum_ids(s, t, ids)
  end

  defp check_enum_ids(s, [:EXTENSIONMARK | t], ids) do
    check_enum_ids(s, t, ids)
  end

  defp check_enum_ids(s, [id | t], ids0) when is_atom(id) do
    ids = check_enum_update_ids(s, id, ids0)
    check_enum_ids(s, t, ids)
  end

  defp check_enum_ids(_, [], _) do
    :ok
  end

  defp check_enum(s, [{:NamedNumber, id, n} | t], used0, acc) do
    used = check_enum_update_used(s, id, n, used0)
    check_enum(s, t, used, [{id, n} | acc])
  end

  defp check_enum(s, [:EXTENSIONMARK | ext0], used0, acc0) do
    acc = :lists.reverse(acc0)
    {root, used, cnt} = check_enum_number_root(acc, used0, 0, [])
    ext = check_enum_ext(s, ext0, used, cnt, [])
    {root, ext}
  end

  defp check_enum(s, [id | t], used, acc) when is_atom(id) do
    check_enum(s, t, used, [id | acc])
  end

  defp check_enum(_, [], used, acc0) do
    acc = :lists.reverse(acc0)
    {root, _, _} = check_enum_number_root(acc, used, 0, [])
    :lists.keysort(2, root)
  end

  defp check_enum_number_root([id | t] = t0, used0, cnt, acc)
       when is_atom(id) do
    case :gb_sets.is_element(cnt, used0) do
      false ->
        used = :gb_sets.insert(cnt, used0)
        check_enum_number_root(t, used, cnt + 1, [{id, cnt} | acc])

      true ->
        check_enum_number_root(t0, used0, cnt + 1, acc)
    end
  end

  defp check_enum_number_root([h | t], used, cnt, acc) do
    check_enum_number_root(t, used, cnt, [h | acc])
  end

  defp check_enum_number_root([], used, cnt, acc) do
    {:lists.keysort(2, acc), used, cnt}
  end

  defp check_enum_ext(s, [{:NamedNumber, id, n} | t], used0, c, acc) do
    used = check_enum_update_used(s, id, n, used0)

    cond do
      n < c ->
        asn1_error(s, {:enum_not_ascending, id, n, c - 1})

      true ->
        :ok
    end

    check_enum_ext(s, t, used, n + 1, [{id, n} | acc])
  end

  defp check_enum_ext(s, [id | t] = t0, used0, c, acc)
       when is_atom(id) do
    case :gb_sets.is_element(c, used0) do
      true ->
        check_enum_ext(s, t0, used0, c + 1, acc)

      false ->
        used = :gb_sets.insert(c, used0)
        check_enum_ext(s, t, used, c + 1, [{id, c} | acc])
    end
  end

  defp check_enum_ext(_, [], _, _, acc) do
    :lists.keysort(2, acc)
  end

  defp check_enum_update_ids(s, id, ids) do
    case :gb_sets.is_element(id, ids) do
      false ->
        :gb_sets.insert(id, ids)

      true ->
        asn1_error(s, {:enum_illegal_redefinition, id})
    end
  end

  defp check_enum_update_used(s, id, n, used) do
    case :gb_sets.is_element(n, used) do
      false ->
        :gb_sets.insert(n, used)

      true ->
        asn1_error(s, {:enum_reused_value, id, n})
    end
  end

  defp check_boolean(_S, _Constr) do
    :ok
  end

  defp check_octetstring(_S, _Constr) do
    :ok
  end

  defp check_sequence(s, type, comps) do
    components = expand_components(s, comps)

    case check_unique(
           for c <- components,
               elem(c, 0) === :ComponentType do
             c
           end,
           r_ComponentType(:name)
         ) do
      [] ->
        components2 = maybe_automatic_tags(s, components)
        newComps = check_each_component2(s, type, components2)
        check_unique_sequence_tags(s, newComps)

        {cRelInf, newComps2} =
          componentrelation_leadingattr(
            s,
            newComps
          )

        compListWithTblInf = get_tableconstraint_info(s, type, newComps2)
        newComps3 = textual_order(compListWithTblInf)
        newComps4 = simplify_comps(newComps3)
        compListTuple = complist_as_tuple(newComps4)
        {cRelInf, compListTuple}

      dupl ->
        asn1_error(
          s,
          {:duplicate_identifier, error_value(hd(dupl))}
        )
    end
  end

  defp complist_as_tuple(compList) do
    complist_as_tuple(compList, [], [], [], :root)
  end

  defp complist_as_tuple([r_EXTENSIONMARK() | t], acc, ext, acc2, :root) do
    complist_as_tuple(t, acc, ext, acc2, :ext)
  end

  defp complist_as_tuple([r_EXTENSIONMARK() | t], acc, ext, acc2, :ext) do
    complist_as_tuple(t, acc, ext, acc2, :root2)
  end

  defp complist_as_tuple([c | t], acc, ext, acc2, :root) do
    complist_as_tuple(t, [c | acc], ext, acc2, :root)
  end

  defp complist_as_tuple([c | t], acc, ext, acc2, :ext) do
    complist_as_tuple(t, acc, [c | ext], acc2, :ext)
  end

  defp complist_as_tuple([c | t], acc, ext, acc2, :root2) do
    complist_as_tuple(t, acc, ext, [c | acc2], :root2)
  end

  defp complist_as_tuple([], acc, _Ext, _Acc2, :root) do
    :lists.reverse(acc)
  end

  defp complist_as_tuple([], acc, ext, _Acc2, :ext) do
    {:lists.reverse(acc), :lists.reverse(ext)}
  end

  defp complist_as_tuple([], acc, ext, acc2, :root2) do
    {:lists.reverse(acc), :lists.reverse(ext), :lists.reverse(acc2)}
  end

  defp expand_components(s, [{:"COMPONENTS OF", type} | t]) do
    compList =
      expand_components2(
        s,
        get_referenced_type(s, r_type(type, :def))
      )

    expand_components(s, compList) ++
      expand_components(
        s,
        t
      )
  end

  defp expand_components(s, [h | t]) do
    [h | expand_components(s, t)]
  end

  defp expand_components(_, []) do
    []
  end

  defp expand_components2(_S, {_, r_typedef(typespec: r_type(def: seq))})
       when elem(seq, 0) === :SEQUENCE do
    case r_SEQUENCE(seq, :components) do
      {r1, _Ext, r2} ->
        r1 ++ r2

      {root, _Ext} ->
        root

      root ->
        take_only_rootset(root)
    end
  end

  defp expand_components2(_S, {_, r_typedef(typespec: r_type(def: set))})
       when elem(set, 0) === :SET do
    case r_SET(set, :components) do
      {r1, _Ext, r2} ->
        r1 ++ r2

      {root, _Ext} ->
        root

      root ->
        take_only_rootset(root)
    end
  end

  defp expand_components2(
         _S,
         {_, r_typedef(typespec: refType = r_type(def: r_Externaltypereference()))}
       ) do
    [{:"COMPONENTS OF", refType}]
  end

  defp expand_components2(s, {_, pT = {:pt, _, _}}) do
    pTType = check_type(s, pT, r_type(def: pT))
    expand_components2(s, {:dummy, r_typedef(typespec: pTType)})
  end

  defp expand_components2(s, {_, oCFT = r_ObjectClassFieldType()}) do
    uncheckedType = r_type(def: oCFT)
    type = check_type(s, r_typedef(typespec: uncheckedType), uncheckedType)
    expand_components2(s, {:undefined, ocft_def(type)})
  end

  defp expand_components2(s, {_, eRef})
       when elem(eRef, 0) === :Externaltypereference do
    expand_components2(s, get_referenced_type(s, eRef))
  end

  defp expand_components2(s, {_, what}) do
    asn1_error(
      s,
      {:illegal_COMPONENTS_OF, error_value(what)}
    )
  end

  defp take_only_rootset([]) do
    []
  end

  defp take_only_rootset([r_EXTENSIONMARK() | _T]) do
    []
  end

  defp take_only_rootset([h | t]) do
    [h | take_only_rootset(t)]
  end

  defp check_unique_sequence_tags(s, compList) do
    tagComps =
      case complist_as_tuple(compList) do
        {r1, ext, r2} ->
          r1 ++
            for c = r_ComponentType() <- ext do
              r_ComponentType(c, prop: :OPTIONAL)
            end ++ r2

        {r1, ext} ->
          r1 ++
            for c = r_ComponentType() <- ext do
              r_ComponentType(c, prop: :OPTIONAL)
            end

        _ ->
          compList
      end

    check_unique_sequence_tags0(s, tagComps)
  end

  defp check_unique_sequence_tags0(s, [r_ComponentType(prop: :mandatory) | rest]) do
    check_unique_sequence_tags0(s, rest)
  end

  defp check_unique_sequence_tags0(s, [c = r_ComponentType() | rest]) do
    check_unique_sequence_tags1(s, rest, [c])
  end

  defp check_unique_sequence_tags0(s, [_ExtensionMarker | rest]) do
    check_unique_sequence_tags0(s, rest)
  end

  defp check_unique_sequence_tags0(_S, []) do
    true
  end

  defp check_unique_sequence_tags1(s, [c | rest], acc)
       when elem(c, 0) === :ComponentType do
    case r_ComponentType(c, :prop) do
      :mandatory ->
        check_unique_tags(s, :lists.reverse([c | acc]))
        check_unique_sequence_tags(s, rest)

      _ ->
        check_unique_sequence_tags1(s, rest, [c | acc])
    end
  end

  defp check_unique_sequence_tags1(s, [h | rest], acc) do
    check_unique_sequence_tags1(s, rest, [h | acc])
  end

  defp check_unique_sequence_tags1(s, [], acc) do
    check_unique_tags(s, :lists.reverse(acc))
  end

  defp check_sequenceof(s, type, component)
       when elem(component, 0) === :type do
    simplify_type(check_type(s, type, component))
  end

  defp check_set(s, type, components) do
    {tableCInf, newComponents} = check_sequence(s, type, components)
    check_unique_tags(s, collect_components(newComponents), [])

    case {:lists.member(:der, r_state(s, :options)), r_state(s, :erule)} do
      {true, _} ->
        {sorted, sortedComponents} = sort_components(:der, s, newComponents)
        {sorted, tableCInf, sortedComponents}

      {_, pER} when pER === :per or pER === :uper ->
        {sorted, sortedComponents} = sort_components(:per, s, newComponents)
        {sorted, tableCInf, sortedComponents}

      _ ->
        {false, tableCInf, newComponents}
    end
  end

  defp collect_components({c1, c2, c3}) do
    collect_components(c1 ++ c2 ++ c3)
  end

  defp collect_components({c1, c2}) do
    collect_components(c1 ++ c2)
  end

  defp collect_components(cs) do
    [] =
      for emptyTag = r_ComponentType(tags: []) <- cs do
        emptyTag
      end

    cs
  end

  defp sort_components(:der, s, components) do
    {r1, ext, r2} = extension(textual_order(components))

    compsList =
      case ext do
        :noext ->
          r1

        _ ->
          r1 ++ ext ++ r2
      end

    case {untagged_choice(s, compsList), ext} do
      {false, :noext} ->
        {true, sort_components1(compsList)}

      {false, _} ->
        {true, {sort_components1(compsList), []}}

      {true, :noext} ->
        {:dynamic, r1}

      _ ->
        {:dynamic, {r1, ext, r2}}
    end
  end

  defp sort_components(:per, s, components) do
    {r1, ext, r2} = extension(textual_order(components))
    root = tag_untagged_choice(s, r1 ++ r2)

    case ext do
      :noext ->
        {true, sort_components1(root)}

      _ ->
        {true, {sort_components1(root), ext}}
    end
  end

  defp sort_components1(cs0) do
    cs1 =
      for r_ComponentType(tags: [tag | _]) = c <- cs0 do
        {tag_key(tag), c}
      end

    cs = :lists.sort(cs1)

    for {_, c} <- cs do
      c
    end
  end

  defp tag_key({:UNIVERSAL, tag}) do
    {0, tag}
  end

  defp tag_key({:APPLICATION, tag}) do
    {1, tag}
  end

  defp tag_key({:CONTEXT, tag}) do
    {2, tag}
  end

  defp tag_key({:PRIVATE, tag}) do
    {3, tag}
  end

  defp untagged_choice(
         _S,
         [r_ComponentType(typespec: r_type(tag: [], def: {:CHOICE, _})) | _Rest]
       ) do
    true
  end

  defp untagged_choice(s, [r_ComponentType(typespec: r_type(tag: [], def: exRef)) | rest])
       when elem(exRef, 0) === :Externaltypereference do
    case get_referenced_type(s, exRef) do
      {_, r_typedef(typespec: r_type(tag: [], def: {:CHOICE, _}))} ->
        true

      _ ->
        untagged_choice(s, rest)
    end
  end

  defp untagged_choice(s, [_ | rest]) do
    untagged_choice(s, rest)
  end

  defp untagged_choice(_, []) do
    false
  end

  defp tag_untagged_choice(s, cs) do
    tag_untagged_choice(s, cs, [])
  end

  defp tag_untagged_choice(
         s,
         [c = r_ComponentType(typespec: r_type(tag: [], def: {:CHOICE, _})) | rest],
         acc
       ) do
    tagList = r_ComponentType(c, :tags)
    taggedC = r_ComponentType(c, tags: get_least_tag(tagList))
    tag_untagged_choice(s, rest, [taggedC | acc])
  end

  defp tag_untagged_choice(
         s,
         [c = r_ComponentType(typespec: r_type(tag: [], def: exRef)) | rest],
         acc
       )
       when elem(exRef, 0) === :Externaltypereference do
    case get_referenced_type(s, exRef) do
      {_, r_typedef(typespec: r_type(tag: [], def: {:CHOICE, _}))} ->
        tagList = r_ComponentType(c, :tags)
        taggedC = r_ComponentType(c, tags: get_least_tag(tagList))
        tag_untagged_choice(s, rest, [taggedC | acc])

      _ ->
        tag_untagged_choice(s, rest, [c | acc])
    end
  end

  defp tag_untagged_choice(s, [c | rest], acc) do
    tag_untagged_choice(s, rest, [c | acc])
  end

  defp tag_untagged_choice(_S, [], acc) do
    acc
  end

  defp get_least_tag([]) do
    []
  end

  defp get_least_tag(tagList) do
    pred = fn
      {:PRIVATE, _}, {:CONTEXT, _} ->
        true

      {:CONTEXT, _}, {:APPLICATION, _} ->
        true

      {:APPLICATION, _}, {:UNIVERSAL, _} ->
        true

      {a, t1}, {a, t2} when t1 <= t2 ->
        true

      _, _ ->
        false
    end

    [t | _] = :lists.sort(pred, tagList)
    [t]
  end

  defp textual_order(cs) do
    fun = fn
      c = r_ComponentType(), index ->
        {r_ComponentType(c, textual_order: index), index + 1}

      other, index ->
        {other, index}
    end

    {newCs, _} = textual_order(cs, fun, 1)
    newCs
  end

  defp textual_order(cs, fun, ixIn) when is_list(cs) do
    :lists.mapfoldl(fun, ixIn, cs)
  end

  defp textual_order({root, ext}, fun, ixIn) do
    {newRoot, ixR} = textual_order(root, fun, ixIn)
    {newExt, _} = textual_order(ext, fun, ixR)
    {{newRoot, newExt}, :dummy}
  end

  defp textual_order({root1, ext, root2}, fun, ixIn) do
    {newRoot1, ixR} = textual_order(root1, fun, ixIn)
    {newExt, ixE} = textual_order(ext, fun, ixR)
    {newRoot2, _} = textual_order(root2, fun, ixE)
    {{newRoot1, newExt, newRoot2}, :dummy}
  end

  defp extension(components) when is_list(components) do
    {components, :noext, []}
  end

  defp extension({root, extList}) do
    toOpt = fn
      :mandatory ->
        :OPTIONAL

      x ->
        x
    end

    {root,
     for x = r_ComponentType(prop: y) <- extList do
       r_ComponentType(x, prop: toOpt.(y))
     end, []}
  end

  defp extension({root1, extList, root2}) do
    toOpt = fn
      :mandatory ->
        :OPTIONAL

      x ->
        x
    end

    {root1,
     for x = r_ComponentType(prop: y) <- extList do
       r_ComponentType(x, prop: toOpt.(y))
     end, root2}
  end

  defp check_setof(s, type, component)
       when elem(component, 0) === :type do
    simplify_type(check_type(s, type, component))
  end

  defp check_selectiontype(s, name, r_type(def: eref))
       when elem(eref, 0) === :Externaltypereference do
    {refMod, typeDef} = get_referenced_type(s, eref)

    newS =
      r_state(s,
        module: load_asn1_module(s, refMod),
        mname: refMod,
        tname: get_datastr_name(typeDef)
      )

    check_selectiontype2(newS, name, typeDef)
  end

  defp check_selectiontype(s, name, type = r_type(def: {:pt, _, _})) do
    tName =
      case r_state(s, :recordtopname) do
        [] ->
          r_state(s, :tname)

        n ->
          n
      end

    tDef = r_typedef(name: tName, typespec: type)
    check_selectiontype2(s, name, tDef)
  end

  defp check_selectiontype(s, _Name, type) do
    asn1_error(s, {:illegal_choice_type, error_value(type)})
  end

  defp check_selectiontype2(s, name, typeDef) do
    newS = r_state(s, recordtopname: get_datastr_name(typeDef))

    components =
      try do
        checkedType = check_type(newS, typeDef, r_typedef(typeDef, :typespec))
        get_choice_components(s, r_type(checkedType, :def))
      catch
        :error, _ ->
          asn1_error(
            s,
            {:illegal_choice_type, error_value(typeDef)}
          )
      end

    case :lists.keyfind(name, r_ComponentType(:name), components) do
      r_ComponentType(typespec: tS) ->
        tS

      false ->
        asn1_error(s, {:illegal_id, error_value(name)})
    end
  end

  defp get_choice_components(_S, {:CHOICE, components})
       when is_list(components) do
    components
  end

  defp get_choice_components(_S, {:CHOICE, {c1, c2}})
       when is_list(c1) and
              is_list(c2) do
    c1 ++ c2
  end

  defp get_choice_components(s, eRef = r_Externaltypereference()) do
    {_RefMod, typeDef} = get_referenced_type(s, eRef)
    r_typedef(typespec: tS) = typeDef
    get_choice_components(s, r_type(tS, :def))
  end

  defp check_restrictedstring(_S, _Def, _Constr) do
    :ok
  end

  defp check_objectidentifier(_S, _Constr) do
    :ok
  end

  defp check_relative_oid(_S, _Constr) do
    :ok
  end

  defp check_choice(s, type, components) when is_list(components) do
    components1 =
      for c = r_ComponentType() <- components do
        c
      end

    case check_unique(components1, r_ComponentType(:name)) do
      [] ->
        components2 = maybe_automatic_tags(s, components)
        newComps = check_each_alternative2(s, type, components2)

        newComps2 =
          :lists.filter(
            fn
              r_ExtensionAdditionGroup() ->
                false

              :ExtensionAdditionGroupEnd ->
                false

              _ ->
                true
            end,
            newComps
          )

        newComps3 = simplify_comps(newComps2)
        check_unique_tags(s, newComps3)
        complist_as_tuple(newComps3)

      dupl ->
        asn1_error(
          s,
          {:duplicate_identifier, error_value(hd(dupl))}
        )
    end
  end

  defp check_choice(_S, _, []) do
    []
  end

  defp maybe_automatic_tags(s, c) do
    tagNos = tag_nums(c)

    case r_module(r_state(s, :module), :tagdefault) do
      :AUTOMATIC ->
        generate_automatic_tags(s, c, tagNos)

      _ ->
        name = r_state(s, :tname)

        case is_automatic_tagged_in_multi_file(name) do
          true ->
            generate_automatic_tags(s, c, tagNos)

          false ->
            c
        end
    end
  end

  defp tag_nums(cl) do
    tag_nums(cl, 0, 0)
  end

  defp tag_nums([r_EXTENSIONMARK() | rest], ext, root2) do
    tag_nums_ext(rest, ext, root2)
  end

  defp tag_nums([_ | rest], ext, root2) do
    tag_nums(rest, ext + 1, root2 + 1)
  end

  defp tag_nums([], ext, root2) do
    [0, ext, root2]
  end

  defp tag_nums_ext([r_EXTENSIONMARK() | rest], ext, root2) do
    tag_nums_root2(rest, ext, root2)
  end

  defp tag_nums_ext([_ | rest], ext, root2) do
    tag_nums_ext(rest, ext, root2)
  end

  defp tag_nums_ext([], ext, _Root2) do
    [0, ext, 0]
  end

  defp tag_nums_root2([_ | rest], ext, root2) do
    tag_nums_root2(rest, ext + 1, root2)
  end

  defp tag_nums_root2([], ext, root2) do
    [0, ext, root2]
  end

  defp is_automatic_tagged_in_multi_file(name) do
    case :asn1ct_table.exists(:automatic_tags) do
      false ->
        false

      true ->
        case :asn1ct_table.lookup(:automatic_tags, name) do
          [] ->
            false

          _ ->
            true
        end
    end
  end

  defp generate_automatic_tags(_S, c, tagNo) do
    case any_manual_tag(c) do
      true ->
        c

      false ->
        generate_automatic_tags1(c, tagNo)
    end
  end

  defp generate_automatic_tags1([h | t], [tagNo | tagNos])
       when elem(h, 0) === :ComponentType do
    r_ComponentType(typespec: ts) = h

    newTs =
      r_type(ts,
        tag: [r_tag(class: :CONTEXT, number: tagNo, type: {:default, :IMPLICIT}, form: 0)]
      )

    [
      r_ComponentType(h, typespec: newTs)
      | generate_automatic_tags1(
          t,
          [tagNo + 1 | tagNos]
        )
    ]
  end

  defp generate_automatic_tags1([extMark = r_EXTENSIONMARK() | t], [_TagNo | tagNos]) do
    [extMark | generate_automatic_tags1(t, tagNos)]
  end

  defp generate_automatic_tags1([h | t], tagList) do
    [h | generate_automatic_tags1(t, tagList)]
  end

  defp generate_automatic_tags1([], _) do
    []
  end

  defp any_manual_tag([r_ComponentType(typespec: r_type(tag: tag)) | _Rest])
       when tag !== [] do
    true
  end

  defp any_manual_tag([_ | rest]) do
    any_manual_tag(rest)
  end

  defp any_manual_tag([]) do
    false
  end

  defp check_unique_tags(s, c) do
    case r_module(r_state(s, :module), :tagdefault) do
      :AUTOMATIC ->
        case any_manual_tag(c) do
          false ->
            true

          true ->
            check_unique_tags(s, c, [])
        end

      _ ->
        check_unique_tags(s, c, [])
    end
  end

  defp check_unique_tags(s, [r_ComponentType(name: name, tags: tags0) | t], acc) do
    tags =
      for tag <- tags0 do
        {tag, name}
      end

    check_unique_tags(s, t, tags ++ acc)
  end

  defp check_unique_tags(s, [_ | t], acc) do
    check_unique_tags(s, t, acc)
  end

  defp check_unique_tags(s, [], acc) do
    r0 = :sofs.relation(acc)
    r1 = :sofs.relation_to_family(r0)
    r2 = :sofs.to_external(r1)

    dup =
      for {_, [[_, _] | _] = els} <- r2 do
        els
      end

    case dup do
      [] ->
        :ok

      [firstDupl | _] ->
        asn1_error(s, {:duplicate_tags, firstDupl})
    end
  end

  defp check_unique(l, pos) do
    slist = :lists.keysort(pos, l)
    check_unique2(slist, pos, [])
  end

  defp check_unique2([[a, b] | t], pos, acc)
       when :erlang.element(pos, a) ==
              :erlang.element(
                pos,
                b
              ) do
    check_unique2([b | t], pos, [:erlang.element(pos, b) | acc])
  end

  defp check_unique2([_ | t], pos, acc) do
    check_unique2(t, pos, acc)
  end

  defp check_unique2([], _, acc) do
    :lists.reverse(acc)
  end

  defp check_each_component2(s, type, components) do
    check_each_component2(s, type, components, [])
  end

  defp check_each_component2(
         s =
           r_state(
             abscomppath: path,
             recordtopname: topName
           ),
         type,
         [c = r_ComponentType(name: cname, typespec: ts, prop: prop) | ct],
         acc
       ) do
    newAbsCPath =
      case r_type(ts, :def) do
        r_Externaltypereference() ->
          []

        _ ->
          [cname | path]
      end

    checkedTs =
      check_type(
        r_state(s,
          abscomppath: newAbsCPath,
          recordtopname: [cname | topName]
        ),
        type,
        ts
      )

    newTags = get_taglist(s, checkedTs)

    newProp =
      case normalize_value(s, checkedTs, prop, [cname | topName]) do
        :mandatory ->
          :mandatory

        :OPTIONAL ->
          :OPTIONAL

        defaultValue ->
          {:DEFAULT, defaultValue}
      end

    newC = r_ComponentType(c, typespec: checkedTs, prop: newProp, tags: newTags)
    check_each_component2(s, type, ct, [newC | acc])
  end

  defp check_each_component2(s, type, [otherMarker | ct], acc) do
    check_each_component2(s, type, ct, [otherMarker | acc])
  end

  defp check_each_component2(_S, _, [], acc) do
    :lists.reverse(acc)
  end

  defp check_each_alternative2(s, type, [c | ct]) do
    check_each_alternative2(s, type, [c | ct], [])
  end

  defp check_each_alternative2(
         s =
           r_state(
             abscomppath: path,
             recordtopname: topName
           ),
         type,
         [c = r_ComponentType(name: cname, typespec: ts) | ct],
         acc
       ) do
    newAbsCPath =
      case r_type(ts, :def) do
        r_Externaltypereference() ->
          []

        _ ->
          [cname | path]
      end

    checkedTs =
      check_type(
        r_state(s,
          abscomppath: newAbsCPath,
          recordtopname: [cname | topName]
        ),
        type,
        ts
      )

    newTags = get_taglist(s, checkedTs)
    newC = r_ComponentType(c, typespec: checkedTs, tags: newTags)
    check_each_alternative2(s, type, ct, [newC | acc])
  end

  defp check_each_alternative2(s, type, [otherMarker | ct], acc) do
    check_each_alternative2(s, type, ct, [otherMarker | acc])
  end

  defp check_each_alternative2(_S, _, [], acc) do
    :lists.reverse(acc)
  end

  defp componentrelation_leadingattr(s, compList) do
    case get_simple_table_if_used(s, compList) do
      [] ->
        {false, compList}

      _ ->
        componentrelation_leadingattr(s, compList, compList, [], [])
    end
  end

  defp componentrelation_leadingattr(_, [], _CompList, [], newCompList) do
    {false, :lists.reverse(newCompList)}
  end

  defp componentrelation_leadingattr(_, [], _CompList, leadingAttr, newCompList) do
    {:lists.last(leadingAttr), :lists.reverse(newCompList)}
  end

  defp componentrelation_leadingattr(s, [c = r_ComponentType() | cs], compList, acc, compAcc) do
    {lAAcc, newC} =
      case (try do
              componentrelation1(s, r_ComponentType(c, :typespec), [r_ComponentType(c, :name)])
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:EXIT, _} ->
          {[], c}

        {cRI = [{_A1, _B1, _C1, _D1} | _Rest], newTSpec} ->
          case leading_attr_index(s, compList, cRI, :lists.reverse(r_state(s, :abscomppath)), []) do
            [] ->
              {[], c}

            [
              {objSet, attr, n, classDef, _Path, valueIndex}
              | _NewRest
            ] ->
              oS = object_set_mod_name(s, objSet)

              uniqFN =
                get_unique_fieldname(
                  s,
                  r_classdef(typespec: classDef)
                )

              res =
                r_simpletableattributes(
                  objectsetname: oS,
                  c_name: attr,
                  c_index: n,
                  usedclassfield: uniqFN,
                  uniqueclassfield: uniqFN,
                  valueindex: valueIndex
                )

              {[res], r_ComponentType(c, typespec: newTSpec)}
          end

        _ ->
          {[], c}
      end

    componentrelation_leadingattr(s, cs, compList, lAAcc ++ acc, [newC | compAcc])
  end

  defp componentrelation_leadingattr(
         s,
         [notComponentType | cs],
         compList,
         leadingAttr,
         newCompList
       ) do
    componentrelation_leadingattr(s, cs, compList, leadingAttr, [notComponentType | newCompList])
  end

  defp object_set_mod_name(_S, objSet) when is_atom(objSet) do
    objSet
  end

  defp object_set_mod_name(r_state(mname: m), r_Externaltypereference(module: m, type: t)) do
    {m, t}
  end

  defp object_set_mod_name(s, r_Externaltypereference(module: m, type: t)) do
    case :lists.member(m, r_state(s, :inputmodules)) do
      true ->
        t

      false ->
        {m, t}
    end
  end

  defp get_simple_table_if_used(s, cs) do
    cNames =
      for r_ComponentType(name: name) <- cs do
        name
      end

    justComponents =
      for c = r_ComponentType() <- cs do
        c
      end

    refedSimpleTable = any_component_relation(s, justComponents, cNames, [], [])
    get_simple_table_info(s, cs, remove_doubles(refedSimpleTable))
  end

  defp remove_doubles(l) do
    remove_doubles(l, [])
  end

  defp remove_doubles([h | t], acc) do
    newT = remove_doubles1(h, t)
    remove_doubles(newT, [h | acc])
  end

  defp remove_doubles([], acc) do
    acc
  end

  defp remove_doubles1(el, l) do
    case :lists.delete(el, l) do
      ^l ->
        l

      newL ->
        remove_doubles1(el, newL)
    end
  end

  defp get_simple_table_info(s, cs, atLists) do
    for atList <- atLists do
      get_simple_table_info1(s, cs, atList, [])
    end
  end

  defp get_simple_table_info1(s, cs, [cname | cnames], path) do
    r_ComponentType() = c = :lists.keyfind(cname, r_ComponentType(:name), cs)
    get_simple_table_info2(s, c, cnames, [cname | path])
  end

  defp get_simple_table_info2(s, r_ComponentType(name: name, typespec: tS), [], path) do
    oCFT = simple_table_get_ocft(s, name, tS)

    case :lists.keymember(:simpletable, 1, r_type(tS, :constraint)) do
      true ->
        simple_table_info(s, oCFT, path)

      false ->
        asn1_error(s, {:missing_table_constraint, name})
    end
  end

  defp get_simple_table_info2(s, r_ComponentType(typespec: tS), cnames, path) do
    components = get_atlist_components(r_type(tS, :def))
    get_simple_table_info1(s, components, cnames, path)
  end

  defp simple_table_get_ocft(_, _, r_type(def: r_ObjectClassFieldType() = oCFT)) do
    oCFT
  end

  defp simple_table_get_ocft(s, component, r_type(constraint: constr)) do
    case :lists.keyfind(:ocft, 1, constr) do
      {:ocft, oCFT} ->
        oCFT

      false ->
        asn1_error(s, {:missing_ocft, component})
    end
  end

  defp simple_table_info(
         s,
         r_ObjectClassFieldType(classname: clRef, class: objectClass, fieldname: fieldName),
         path
       ) do
    objectClassFieldName =
      case fieldName do
        {lastFieldName, []} ->
          lastFieldName

        {_FirstFieldName, fieldNames} ->
          :lists.last(fieldNames)
      end

    classDef =
      case objectClass do
        [] ->
          {_, cDef} = get_referenced_type(s, clRef)
          cDef

        _ ->
          r_classdef(typespec: objectClass)
      end

    uniqueName = get_unique_fieldname(s, classDef)
    {:lists.reverse(path), objectClassFieldName, uniqueName}
  end

  defp any_component_relation(
         s,
         [r_ComponentType(name: cName, typespec: type) | cs],
         cNames,
         namePath,
         acc
       ) do
    cRelPath =
      case :lists.keyfind(:componentrelation, 1, r_type(type, :constraint)) do
        {_, _, atNotation} ->
          atNot = extract_at_notation(atNotation)
          evaluate_atpath(s, namePath, cNames, atNot)

        false ->
          []
      end

    innerAcc =
      case {r_type(type, :inlined), :asn1ct_gen.type(:asn1ct_gen.get_inner(r_type(type, :def)))} do
        {:no, {:constructed, :bif}} ->
          {innerCs, newNamePath} =
            case get_components(r_type(type, :def)) do
              t when elem(t, 0) === :type ->
                {t, namePath}

              iC ->
                {iC, [cName | namePath]}
            end

          any_component_relation(s, innerCs, cNames, newNamePath, [])

        _ ->
          []
      end

    any_component_relation(s, cs, cNames, namePath, innerAcc ++ cRelPath ++ acc)
  end

  defp any_component_relation(s, type, cNames, namePath, acc)
       when elem(type, 0) === :type do
    cRelPath =
      case :lists.keyfind(:componentrelation, 1, r_type(type, :constraint)) do
        {_, _, atNotation} ->
          atNot = extract_at_notation(atNotation)
          evaluate_atpath(s, namePath, cNames, atNot)

        false ->
          []
      end

    innerAcc =
      case {r_type(type, :inlined), :asn1ct_gen.type(:asn1ct_gen.get_inner(r_type(type, :def)))} do
        {:no, {:constructed, :bif}} ->
          innerCs = get_components(r_type(type, :def))
          any_component_relation(s, innerCs, cNames, namePath, [])

        _ ->
          []
      end

    innerAcc ++ cRelPath ++ acc
  end

  defp any_component_relation(s, [r_ExtensionAdditionGroup() | cs], cNames, namePath, acc) do
    any_component_relation(s, cs, cNames, namePath, acc)
  end

  defp any_component_relation(s, [:ExtensionAdditionGroupEnd | cs], cNames, namePath, acc) do
    any_component_relation(s, cs, cNames, namePath, acc)
  end

  defp any_component_relation(_, [], _, _, acc) do
    acc
  end

  defp evaluate_atpath(_, [], cnames, {:innermost, atPath = [ref | _Refs]}) do
    case :lists.member(ref, cnames) do
      true ->
        [atPath]

      false ->
        []
    end
  end

  defp evaluate_atpath(
         s = r_state(abscomppath: topPath),
         namePath,
         cnames,
         {:outermost, atPath = [_Ref | _Refs]}
       ) do
    atPathBelowTop =
      case topPath do
        [] ->
          atPath

        _ ->
          case :lists.prefix(topPath, atPath) do
            true ->
              :lists.subtract(atPath, topPath)

            _ ->
              []
          end
      end

    case {namePath, atPathBelowTop} do
      {[h | _T1], [h | _T2]} ->
        []

      {_, []} ->
        []

      {_, [h | _T]} ->
        case :lists.member(h, cnames) do
          true ->
            [atPathBelowTop]

          _ ->
            asn1_error(s, {:invalid_at_path, atPath})
        end
    end
  end

  defp evaluate_atpath(_, _, _, _) do
    []
  end

  defp get_atlist_components(def__) do
    get_components(:atlist, def__)
  end

  defp get_components(def__) do
    get_components(:any, def__)
  end

  defp get_components(_, r_SEQUENCE(components: cs)) do
    tuple2complist(cs)
  end

  defp get_components(_, r_SET(components: cs)) do
    tuple2complist(cs)
  end

  defp get_components(_, {:CHOICE, cs}) do
    tuple2complist(cs)
  end

  defp get_components(:any, {:"SEQUENCE OF", t = r_type(def: _Def, inlined: :no)}) do
    t
  end

  defp get_components(:any, {:"SET OF", t = r_type(def: _Def, inlined: :no)}) do
    t
  end

  defp get_components(_, _) do
    []
  end

  defp tuple2complist({r, e}) do
    r ++ e
  end

  defp tuple2complist({r1, e, r2}) do
    r1 ++ e ++ r2
  end

  defp tuple2complist(list) when is_list(list) do
    list
  end

  defp extract_at_notation([{level, valueRefs}]) do
    {level,
     for r_Externalvaluereference(value: name) <- valueRefs do
       name
     end}
  end

  defp componentrelation1(
         s,
         c = r_type(def: def__, constraint: constraint, tablecinf: tCI),
         path
       ) do
    ret =
      case :lists.keyfind(:componentrelation, 1, constraint) do
        {_, {_, _, objectSet}, atList} ->
          [{_, aL = [r_Externalvaluereference() | _R1]} | _R2] = atList
          classDef = get_ObjectClassFieldType_classdef(s, def__)

          atPath =
            :lists.map(
              fn r_Externalvaluereference(value: v) ->
                v
              end,
              aL
            )

          {[{objectSet, atPath, classDef, path}], def__}

        false ->
          innertype_comprel(s, def__, path)
      end

    case ret do
      :nofunobj ->
        :nofunobj

      {cRelI = [{objSet, _, _, _}], newDef} ->
        tCItmp = :lists.subtract(tCI, [{:objfun, objSet}])

        {cRelI,
         r_type(c,
           tablecinf: [{:objfun, objSet} | tCItmp],
           def: newDef
         )}

      {compRelInf, newDef} ->
        tCItmp = :lists.subtract(tCI, [{:objfun, :anyset}])

        {compRelInf,
         r_type(c,
           tablecinf: [{:objfun, :anyset} | tCItmp],
           def: newDef
         )}
    end
  end

  defp innertype_comprel(s, {:"SEQUENCE OF", type}, path) do
    case innertype_comprel1(s, type, path) do
      :nofunobj ->
        :nofunobj

      {compRelInf, newType} ->
        {compRelInf, {:"SEQUENCE OF", newType}}
    end
  end

  defp innertype_comprel(s, {:"SET OF", type}, path) do
    case innertype_comprel1(s, type, path) do
      :nofunobj ->
        :nofunobj

      {compRelInf, newType} ->
        {compRelInf, {:"SET OF", newType}}
    end
  end

  defp innertype_comprel(s, {:CHOICE, cTypeList}, path) do
    case componentlist_comprel(s, cTypeList, [], path, []) do
      :nofunobj ->
        :nofunobj

      {compRelInf, newCs} ->
        {compRelInf, {:CHOICE, newCs}}
    end
  end

  defp innertype_comprel(s, seq = r_SEQUENCE(components: cs), path) do
    case componentlist_comprel(s, cs, [], path, []) do
      :nofunobj ->
        :nofunobj

      {compRelInf, newCs} ->
        {compRelInf, r_SEQUENCE(seq, components: newCs)}
    end
  end

  defp innertype_comprel(s, set = r_SET(components: cs), path) do
    case componentlist_comprel(s, cs, [], path, []) do
      :nofunobj ->
        :nofunobj

      {compRelInf, newCs} ->
        {compRelInf, r_SET(set, components: newCs)}
    end
  end

  defp innertype_comprel(_, _, _) do
    :nofunobj
  end

  defp componentlist_comprel(
         s,
         [c = r_ComponentType(name: name, typespec: type) | cs],
         acc,
         path,
         newCL
       ) do
    case (try do
            componentrelation1(s, type, path ++ [name])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        componentlist_comprel(s, cs, acc, path, [c | newCL])

      :nofunobj ->
        componentlist_comprel(s, cs, acc, path, [c | newCL])

      {cRelInf, newType} ->
        componentlist_comprel(s, cs, cRelInf ++ acc, path, [
          r_ComponentType(c, typespec: newType) | newCL
        ])
    end
  end

  defp componentlist_comprel(_, [], acc, _, newCL) do
    case acc do
      [] ->
        :nofunobj

      _ ->
        {acc, :lists.reverse(newCL)}
    end
  end

  defp innertype_comprel1(
         s,
         t = r_type(def: def__, constraint: cons, tablecinf: tCI),
         path
       ) do
    ret =
      case :lists.keyfind(:componentrelation, 1, cons) do
        {_, {_, _, objectSet}, atList} ->
          [{_, aL = [r_Externalvaluereference(value: _Attr) | _R1]} | _R2] = atList
          classDef = get_ObjectClassFieldType_classdef(s, def__)

          atPath =
            :lists.map(
              fn r_Externalvaluereference(value: v) ->
                v
              end,
              aL
            )

          [{objectSet, atPath, classDef, path}]

        false ->
          innertype_comprel(s, def__, path)
      end

    case ret do
      :nofunobj ->
        :nofunobj

      l = [{objSet, _, _, _}] ->
        tCItmp = :lists.subtract(tCI, [{:objfun, objSet}])
        {l, r_type(t, tablecinf: [{:objfun, objSet} | tCItmp])}

      {cRelInf, newDef} ->
        tCItmp = :lists.subtract(tCI, [{:objfun, :anyset}])

        {cRelInf,
         r_type(t,
           def: newDef,
           tablecinf: [{:objfun, :anyset} | tCItmp]
         )}
    end
  end

  defp leading_attr_index(s, cs, [h = {_, attrP, _, _} | t], absP, acc) do
    attrInfo =
      case :lists.prefix(absP, attrP) do
        true ->
          relativAttrP = :lists.subtract(attrP, absP)
          {hd(relativAttrP), tl(relativAttrP)}

        false ->
          {hd(attrP), tl(attrP)}
      end

    case leading_attr_index1(s, cs, h, attrInfo, 1) do
      0 ->
        leading_attr_index(s, cs, t, absP, acc)

      res ->
        leading_attr_index(s, cs, t, absP, [res | acc])
    end
  end

  defp leading_attr_index(_, _Cs, [], _, acc) do
    :lists.reverse(acc)
  end

  defp leading_attr_index1(_, [], _, _, _) do
    0
  end

  defp leading_attr_index1(
         s,
         [c | cs],
         arg = {objectSet, _, cDef, p},
         attrInfo = {attr, subAttr},
         n
       ) do
    case r_ComponentType(c, :name) do
      ^attr ->
        valueMatch = value_match(s, c, attr, subAttr)
        {objectSet, attr, n, cDef, p, valueMatch}

      _ ->
        leading_attr_index1(s, cs, arg, attrInfo, n + 1)
    end
  end

  defp value_match(s, c, name, subAttr) do
    value_match(s, c, name, subAttr, [])
  end

  defp value_match(_S, r_ComponentType(), _Name, [], acc) do
    acc
  end

  defp value_match(s, r_ComponentType(typespec: type), name, [at | ats], acc) do
    innerType = :asn1ct_gen.get_inner(r_type(type, :def))

    components =
      case get_atlist_components(r_type(type, :def)) do
        [] ->
          asn1_error(s, {:invalid_element, name})

        comps ->
          comps
      end

    {index, valueIndex} = component_value_index(s, innerType, at, components)
    value_match(s, :lists.nth(index, components), at, ats, [valueIndex | acc])
  end

  defp component_value_index(s, :CHOICE, at, components) do
    {component_index(s, at, components), 2}
  end

  defp component_value_index(s, _, at, components) do
    index = component_index(s, at, components)
    {index, {index + 1, at}}
  end

  defp component_index(s, name, components) do
    component_index1(s, name, components, 1)
  end

  defp component_index1(_S, name, [r_ComponentType(name: name) | _Cs], n) do
    n
  end

  defp component_index1(s, name, [_C | cs], n) do
    component_index1(s, name, cs, n + 1)
  end

  defp component_index1(s, name, [], _) do
    asn1_error(s, {:invalid_at_list, name})
  end

  defp get_unique_fieldname(s, r_classdef(typespec: tS)) do
    fields = r_objectclass(tS, :fields)
    get_unique_fieldname1(s, fields, [])
  end

  defp get_unique_fieldname(s, r_typedef(typespec: r_type(def: classRef))) do
    {_M, classDef} = get_referenced_type(s, classRef)
    get_unique_fieldname(s, classDef)
  end

  defp get_unique_fieldname1(
         s,
         [{:fixedtypevaluefield, name, _, :UNIQUE, opt} | t],
         acc
       ) do
    get_unique_fieldname1(s, t, [{name, opt} | acc])
  end

  defp get_unique_fieldname1(s, [_ | t], acc) do
    get_unique_fieldname1(s, t, acc)
  end

  defp get_unique_fieldname1(s, [], acc) do
    case acc do
      [] ->
        :no_unique

      [name] ->
        name

      [_ | _] ->
        asn1_error(s, :multiple_uniqs)
    end
  end

  defp get_tableconstraint_info(s, type, {checkedTs, eComps, checkedTs2}) do
    {get_tableconstraint_info(s, type, checkedTs, []),
     get_tableconstraint_info(s, type, eComps, []),
     get_tableconstraint_info(s, type, checkedTs2, [])}
  end

  defp get_tableconstraint_info(s, type, {checkedTs, eComps}) do
    {get_tableconstraint_info(s, type, checkedTs, []),
     get_tableconstraint_info(s, type, eComps, [])}
  end

  defp get_tableconstraint_info(s, type, checkedTs) do
    get_tableconstraint_info(s, type, checkedTs, [])
  end

  defp get_tableconstraint_info(_S, _Type, [], acc) do
    :lists.reverse(acc)
  end

  defp get_tableconstraint_info(s, type, [c = r_ComponentType(typespec: checkedTs) | cs], acc) do
    accComp =
      case r_type(checkedTs, :def) do
        oCFT = r_ObjectClassFieldType() ->
          newOCFT = r_ObjectClassFieldType(oCFT, class: [])
          r_ComponentType(c, typespec: r_type(checkedTs, def: newOCFT))

        {:"SEQUENCE OF", sOType}
        when elem(sOType, 0) === :type and
               :erlang.element(
                 1,
                 r_type(sOType, :def)
               ) == :CHOICE ->
          cTypeList = :erlang.element(2, r_type(sOType, :def))
          newInnerCList = get_tableconstraint_info(s, type, cTypeList)

          r_ComponentType(c,
            typespec:
              r_type(checkedTs,
                def: {:"SEQUENCE OF", r_type(sOType, def: {:CHOICE, newInnerCList})}
              )
          )

        {:"SET OF", sOType}
        when elem(sOType, 0) === :type and
               :erlang.element(
                 1,
                 r_type(sOType, :def)
               ) == :CHOICE ->
          cTypeList = :erlang.element(2, r_type(sOType, :def))
          newInnerCList = get_tableconstraint_info(s, type, cTypeList)

          r_ComponentType(c,
            typespec:
              r_type(checkedTs, def: {:"SET OF", r_type(sOType, def: {:CHOICE, newInnerCList})})
          )

        _ ->
          c
      end

    get_tableconstraint_info(s, type, cs, [accComp | acc])
  end

  defp get_tableconstraint_info(s, type, [c | cs], acc) do
    get_tableconstraint_info(s, type, cs, [c | acc])
  end

  defp get_referenced_fieldname([{_, firstFieldname}]) do
    {firstFieldname, []}
  end

  defp get_referenced_fieldname([{_, firstFieldname} | t]) do
    {firstFieldname,
     for x <- t do
       :erlang.element(2, x)
     end}
  end

  defp get_ObjectClassFieldType_classdef(s, r_ObjectClassFieldType(classname: name, class: [])) do
    {_, r_classdef(typespec: tS)} = get_referenced_type(s, name)
    tS
  end

  defp get_ObjectClassFieldType_classdef(_, r_ObjectClassFieldType(class: cl)) do
    cl
  end

  defp get_OCFType(s, fields, fieldnameList = [{_FieldType, _PrimFieldName} | _]) do
    get_OCFType(
      s,
      fields,
      for {_, pFN} <- fieldnameList do
        pFN
      end
    )
  end

  defp get_OCFType(s, fields, [primFieldName | rest]) do
    case :lists.keysearch(primFieldName, 2, fields) do
      {:value, {:fixedtypevaluefield, _, type, _Unique, _OptSpec}} ->
        {:fixedtypevaluefield, primFieldName, type}

      {:value, {:objectfield, _, classRef, _Unique, _OptSpec}} ->
        {mName, classDef} = get_referenced_type(s, classRef)

        newS =
          update_state(
            r_state(s, tname: get_datastr_name(classDef)),
            mName
          )

        checkedCDef = check_class(newS, classDef)
        get_OCFType(s, r_objectclass(checkedCDef, :fields), rest)

      {:value, {:objectsetfield, _, type, _OptSpec}} ->
        {mName, classDef} =
          get_referenced_type(
            s,
            r_type(type, :def)
          )

        newS =
          update_state(
            r_state(s, tname: get_datastr_name(classDef)),
            mName
          )

        checkedCDef = check_class(newS, classDef)
        get_OCFType(s, r_objectclass(checkedCDef, :fields), rest)

      {:value, other} ->
        {:erlang.element(1, other), primFieldName}

      _ ->
        asn1_error(s, {:illegal_object_field, primFieldName})
    end
  end

  defp get_taglist(s, ext)
       when elem(ext, 0) === :Externaltypereference do
    {_, t} = get_referenced_type(s, ext)
    get_taglist(s, r_typedef(t, :typespec))
  end

  defp get_taglist(s, type) when elem(type, 0) === :type do
    case r_type(type, :tag) do
      [] ->
        get_taglist(s, r_type(type, :def))

      [tag | _] ->
        [:asn1ct_gen.def_to_tag(tag)]
    end
  end

  defp get_taglist(s, {:CHOICE, {rc, ec}}) do
    get_taglist1(s, rc ++ ec)
  end

  defp get_taglist(s, {:CHOICE, {r1, e, r2}}) do
    get_taglist1(s, r1 ++ e ++ r2)
  end

  defp get_taglist(s, {:CHOICE, components}) do
    get_taglist1(s, components)
  end

  defp get_taglist(_S, r_ObjectClassFieldType(type: {:typefield, _})) do
    []
  end

  defp get_taglist(s, r_ObjectClassFieldType(type: {:fixedtypevaluefield, _, type})) do
    get_taglist(s, type)
  end

  defp get_taglist(_, _) do
    []
  end

  defp get_taglist1(s, [r_ComponentType(name: _Cname, tags: tagL) | rest])
       when is_list(tagL) do
    tagL ++ get_taglist1(s, rest)
  end

  defp get_taglist1(
         s,
         [r_ComponentType(typespec: ts, tags: :undefined) | rest]
       ) do
    get_taglist(s, ts) ++ get_taglist1(s, rest)
  end

  defp get_taglist1(s, [_H | rest]) do
    get_taglist1(s, rest)
  end

  defp get_taglist1(_S, []) do
    []
  end

  defp merge_tags(t1, t2) when is_list(t2) do
    merge_tags2(t1 ++ t2, [])
  end

  defp merge_tags(t1, t2) do
    merge_tags2(t1 ++ [t2], [])
  end

  defp merge_tags2([[t1 = r_tag(type: :IMPLICIT), t2] | rest], acc) do
    merge_tags2(
      [
        r_tag(t1,
          type: r_tag(t2, :type),
          form: r_tag(t2, :form)
        )
        | rest
      ],
      acc
    )
  end

  defp merge_tags2(
         [
           [t1 = r_tag(type: {:default, :IMPLICIT}), t2]
           | rest
         ],
         acc
       ) do
    merge_tags2(
      [
        r_tag(t1,
          type: r_tag(t2, :type),
          form: r_tag(t2, :form)
        )
        | rest
      ],
      acc
    )
  end

  defp merge_tags2(
         [
           [t1 = r_tag(type: {:default, :AUTOMATIC}), t2]
           | rest
         ],
         acc
       ) do
    merge_tags2(
      [
        r_tag(t1,
          type: r_tag(t2, :type),
          form: r_tag(t2, :form)
        )
        | rest
      ],
      acc
    )
  end

  defp merge_tags2([h | t], acc) do
    merge_tags2(t, [h | acc])
  end

  defp merge_tags2([], acc) do
    :lists.reverse(acc)
  end

  def storeindb(s0, r_module(name: modName, typeorval: tVlist0) = m) do
    s = r_state(s0, mname: modName)

    tVlist1 =
      for def__ <- tVlist0 do
        {:asn1ct.get_name_of_def(def__), def__}
      end

    case check_duplicate_defs(s, tVlist1) do
      :ok ->
        storeindb_1(s, m, tVlist0, tVlist1)

      {:error, _} = error ->
        error
    end
  end

  defp storeindb_1(s, r_module(name: modName) = m, tVlist0, tVlist) do
    newM = r_module(m, typeorval: findtypes_and_values(tVlist0))
    maps = :lists.member(:maps, r_state(s, :options))
    :asn1_db.dbnew(modName, r_state(s, :erule), maps)
    :asn1_db.dbput(modName, :MODULE, newM)
    :asn1_db.dbput(modName, tVlist)
    include_default_class(s, r_module(newM, :name))
    include_default_type(r_module(newM, :name))
    :ok
  end

  defp check_duplicate_defs(s, defs) do
    set0 = :sofs.relation(defs)
    set1 = :sofs.relation_to_family(set0)
    set = :sofs.to_external(set1)

    case (for {n, [[_, _] | _] = dup} <- set do
            duplicate_def(s, n, dup)
          end) do
      [] ->
        :ok

      [_ | _] = e ->
        {:error, :lists.append(e)}
    end
  end

  defp duplicate_def(s, name, dups0) do
    dups1 =
      for def__ <- dups0 do
        {:asn1ct.get_pos_of_def(def__), def__}
      end

    [{prev, _} | dups] = :lists.sort(dups1)
    duplicate_def_1(s, dups, name, prev)
  end

  defp duplicate_def_1(s, [{_, def__} | t], name, prev) do
    e = return_asn1_error(s, def__, {:already_defined, name, prev})
    [e | duplicate_def_1(s, t, name, prev)]
  end

  defp duplicate_def_1(_, [], _, _) do
    []
  end

  defp findtypes_and_values(tVList) do
    findtypes_and_values(tVList, [], [], [], [], [], [])
  end

  defp findtypes_and_values([h | t], tacc, vacc, pacc, cacc, oacc, oSacc)
       when elem(h, 0) === :typedef and
              elem(r_typedef(h, :typespec), 0) === :Object do
    findtypes_and_values(t, tacc, vacc, pacc, cacc, [r_typedef(h, :name) | oacc], oSacc)
  end

  defp findtypes_and_values([h | t], tacc, vacc, pacc, cacc, oacc, oSacc)
       when elem(h, 0) === :typedef and
              elem(r_typedef(h, :typespec), 0) === :ObjectSet do
    findtypes_and_values(t, tacc, vacc, pacc, cacc, oacc, [r_typedef(h, :name) | oSacc])
  end

  defp findtypes_and_values([h | t], tacc, vacc, pacc, cacc, oacc, oSacc)
       when elem(h, 0) === :typedef do
    findtypes_and_values(t, [r_typedef(h, :name) | tacc], vacc, pacc, cacc, oacc, oSacc)
  end

  defp findtypes_and_values([h | t], tacc, vacc, pacc, cacc, oacc, oSacc)
       when elem(h, 0) === :valuedef do
    findtypes_and_values(t, tacc, [r_valuedef(h, :name) | vacc], pacc, cacc, oacc, oSacc)
  end

  defp findtypes_and_values([h | t], tacc, vacc, pacc, cacc, oacc, oSacc)
       when elem(h, 0) === :ptypedef do
    findtypes_and_values(t, tacc, vacc, [r_ptypedef(h, :name) | pacc], cacc, oacc, oSacc)
  end

  defp findtypes_and_values([h | t], tacc, vacc, pacc, cacc, oacc, oSacc)
       when elem(h, 0) === :classdef do
    findtypes_and_values(t, tacc, vacc, pacc, [r_classdef(h, :name) | cacc], oacc, oSacc)
  end

  defp findtypes_and_values([h | t], tacc, vacc, pacc, cacc, oacc, oSacc)
       when elem(h, 0) === :pvaluedef do
    findtypes_and_values(t, tacc, [r_pvaluedef(h, :name) | vacc], pacc, cacc, oacc, oSacc)
  end

  defp findtypes_and_values([h | t], tacc, vacc, pacc, cacc, oacc, oSacc)
       when elem(h, 0) === :pvaluesetdef do
    findtypes_and_values(t, tacc, [r_pvaluesetdef(h, :name) | vacc], pacc, cacc, oacc, oSacc)
  end

  defp findtypes_and_values([h | t], tacc, vacc, pacc, cacc, oacc, oSacc)
       when elem(h, 0) === :pobjectdef do
    findtypes_and_values(t, tacc, vacc, pacc, cacc, [r_pobjectdef(h, :name) | oacc], oSacc)
  end

  defp findtypes_and_values([h | t], tacc, vacc, pacc, cacc, oacc, oSacc)
       when elem(h, 0) === :pobjectsetdef do
    findtypes_and_values(t, tacc, vacc, pacc, cacc, oacc, [r_pobjectsetdef(h, :name) | oSacc])
  end

  defp findtypes_and_values([], tacc, vacc, pacc, cacc, oacc, oSacc) do
    {:lists.reverse(tacc), :lists.reverse(vacc), :lists.reverse(pacc), :lists.reverse(cacc),
     :lists.reverse(oacc), :lists.reverse(oSacc)}
  end

  defp return_asn1_error(r_state(error_context: context) = s, error) do
    return_asn1_error(s, context, error)
  end

  defp return_asn1_error(r_state(mname: where), item, error) do
    pos = :asn1ct.get_pos_of_def(item)
    {:structured_error, {where, pos}, :asn1ct_check, error}
  end

  defp asn1_error(s, error) do
    throw({:error, return_asn1_error(s, error)})
  end

  def format_error({:already_defined, name, prevLine}) do
    :io_lib.format('the name ~p has already been defined at line ~p', [name, prevLine])
  end

  def format_error({:duplicate_identifier, ids}) do
    :io_lib.format('the identifier \'~p\' has already been used', [ids])
  end

  def format_error({:duplicate_tags, elements}) do
    :io_lib.format('duplicate tags in the elements: ~s', [format_elements(elements)])
  end

  def format_error({:enum_illegal_redefinition, id}) do
    :io_lib.format('\'~s\' must not be redefined', [id])
  end

  def format_error({:enum_not_ascending, id, n, prev}) do
    :io_lib.format(
      'the values for enumerations which follow \'...\' must be in ascending order, but \'~p(~p)\' is less than the previous value \'~p\'',
      [id, n, prev]
    )
  end

  def format_error({:enum_reused_value, id, val}) do
    :io_lib.format('\'~s\' has the value \'~p\' which is used more than once', [id, val])
  end

  def format_error({:illegal_id, id}) do
    :io_lib.format('illegal identifier: ~p', [id])
  end

  def format_error({:illegal_choice_type, ref}) do
    :io_lib.format('expecting a CHOICE type: ~p', [ref])
  end

  def format_error({:illegal_class_name, class}) do
    :io_lib.format(
      'the class name \'~s\' is illegal (it must start with an uppercase letter and only contain uppercase letters, digits, or hyphens)',
      [class]
    )
  end

  def format_error({:illegal_COMPONENTS_OF, ref}) do
    :io_lib.format('expected a SEQUENCE or SET got: ~p', [ref])
  end

  def format_error(:illegal_external_value) do
    'illegal value in EXTERNAL type'
  end

  def format_error({:illegal_instance_of, class}) do
    :io_lib.format(
      'using INSTANCE OF on class \'~s\' is illegal, because INSTANCE OF may only be used on the class TYPE-IDENTIFIER',
      [class]
    )
  end

  def format_error(:illegal_integer_value) do
    'expecting an integer value'
  end

  def format_error(:illegal_object) do
    'expecting an object'
  end

  def format_error({:illegal_object_field, id}) do
    :io_lib.format('expecting a class field: ~p', [id])
  end

  def format_error({:illegal_oid, :o_id}) do
    'illegal OBJECT IDENTIFIER'
  end

  def format_error({:illegal_oid, :rel_oid}) do
    'illegal RELATIVE-OID'
  end

  def format_error(:illegal_octet_string_value) do
    'expecting a bstring or an hstring as value for an OCTET STRING'
  end

  def format_error({:illegal_typereference, name}) do
    :io_lib.format(
      '\'~p\' is used as a typereference, but does not start with an uppercase letter',
      [name]
    )
  end

  def format_error(:illegal_table_constraint) do
    'table constraints may only be applied to CLASS.&field constructs'
  end

  def format_error(:illegal_value) do
    'expecting a value'
  end

  def format_error({:illegal_value, tYPE}) do
    :io_lib.format('expecting a ~s value', [tYPE])
  end

  def format_error({:invalid_fields, fields, obj}) do
    :io_lib.format('invalid ~s in ~p', [format_fields(fields), obj])
  end

  def format_error({:invalid_bit_number, bit}) do
    :io_lib.format('the bit number \'~p\' is invalid', [bit])
  end

  def format_error(:invalid_table_constraint) do
    'the table constraint is not an object set'
  end

  def format_error(:invalid_objectset) do
    'expecting an object set'
  end

  def format_error({:implicit_tag_before, kind}) do
    'illegal implicit tag before ' ++
      case kind do
        :choice ->
          '\'CHOICE\''

        :open_type ->
          'open type'
      end
  end

  def format_error({:missing_mandatory_fields, fields, obj}) do
    :io_lib.format('missing mandatory ~s in ~p', [format_fields(fields), obj])
  end

  def format_error({:missing_table_constraint, component}) do
    :io_lib.format(
      'the component \'~s\' is referenced by a component relation constraint using the \'@field-name\' notation, but does not have a table constraint',
      [component]
    )
  end

  def format_error({:missing_id, id}) do
    :io_lib.format('expected the mandatory component \'~p\'', [id])
  end

  def format_error({:missing_ocft, component}) do
    :io_lib.format(
      'the component \'~s\' must be an ObjectClassFieldType (CLASSNAME.&field-name)',
      [component]
    )
  end

  def format_error(:multiple_uniqs) do
    'implementation limitation: only one UNIQUE field is allowed in CLASS'
  end

  def format_error({:namelist_redefinition, name}) do
    :io_lib.format('the name \'~s\' cannot be redefined', [name])
  end

  def format_error({:param_bad_type, ref}) do
    :io_lib.format('\'~p\' is not a parameterized type', [ref])
  end

  def format_error(:param_wrong_number_of_arguments) do
    'wrong number of arguments'
  end

  def format_error(:reversed_range) do
    'ranges must be given in increasing order'
  end

  def format_error({:syntax_duplicated_fields, fields}) do
    :io_lib.format('~s must only occur once in the syntax list', [format_fields(fields)])
  end

  def format_error(:syntax_nomatch) do
    'unexpected end of object definition'
  end

  def format_error({:syntax_mandatory_in_optional_group, name}) do
    :io_lib.format(
      'the field \'&~s\' must not be within an optional group since it is not optional',
      [name]
    )
  end

  def format_error({:syntax_missing_mandatory_fields, fields}) do
    :io_lib.format('missing mandatory ~s in the syntax list', [format_fields(fields)])
  end

  def format_error({:syntax_nomatch, actual}) do
    :io_lib.format('~s is not the next item allowed according to the defined syntax', [actual])
  end

  def format_error({:syntax_undefined_field, field}) do
    :io_lib.format('\'&~s\' is not a field of the class being defined', [field])
  end

  def format_error({:undefined, name}) do
    :io_lib.format('\'~s\' is referenced, but is not defined', [name])
  end

  def format_error({:undefined_export, ref}) do
    :io_lib.format('\'~s\' is exported but is not defined', [ref])
  end

  def format_error({:undefined_field, fieldName}) do
    :io_lib.format('the field \'&~s\' is undefined', [fieldName])
  end

  def format_error({:undefined_import, ref, module}) do
    :io_lib.format('\'~s\' is not exported from ~s', [ref, module])
  end

  def format_error({:unique_and_default, field}) do
    :io_lib.format('the field \'&~s\' must not have both \'UNIQUE\' and \'DEFAULT\'', [field])
  end

  def format_error({:value_reused, val}) do
    :io_lib.format('the value \'~p\' is used more than once', [val])
  end

  def format_error({:non_unique_object, id}) do
    :io_lib.format('object set with a UNIQUE field value of \'~p\' is used more than once', [id])
  end

  def format_error(other) do
    :io_lib.format('~p', [other])
  end

  defp format_fields([f]) do
    :io_lib.format('field \'&~s\'', [f])
  end

  defp format_fields([h | t]) do
    [
      :io_lib.format('fields \'&~s\'', [h])
      | for f <- t do
          :io_lib.format(', \'&~s\'', [f])
        end
    ]
  end

  defp format_elements([[h1, h2] | t]) do
    [:io_lib.format('~p, ', [h1]) | format_elements([h2 | t])]
  end

  defp format_elements([h]) do
    :io_lib.format('~p', [h])
  end

  defp include_default_type(module) do
    nameAbsList = default_type_list()
    include_default_type1(module, nameAbsList)
  end

  defp include_default_type1(_, []) do
    :ok
  end

  defp include_default_type1(module, [{name, tS} | rest]) do
    case :asn1_db.dbget(module, name) do
      :undefined ->
        t = r_typedef(name: name, typespec: tS)
        :asn1_db.dbput(module, name, t)

      _ ->
        :ok
    end

    include_default_type1(module, rest)
  end

  defp default_type_list() do
    syntax =
      r_ComponentType(
        name: :syntax,
        typespec: r_type(def: :"OBJECT IDENTIFIER"),
        prop: :mandatory
      )

    presentation_Cid =
      r_ComponentType(
        name: :"presentation-context-id",
        typespec: r_type(def: :INTEGER),
        prop: :mandatory
      )

    transfer_syntax =
      r_ComponentType(
        name: :"transfer-syntax",
        typespec: r_type(def: :"OBJECT IDENTIFIER"),
        prop: :mandatory
      )

    negotiation_items =
      r_type(
        def:
          r_SEQUENCE(
            components: [presentation_Cid, r_ComponentType(transfer_syntax, prop: :mandatory)]
          )
      )

    context_negot =
      r_ComponentType(name: :"context-negotiation", typespec: negotiation_items, prop: :mandatory)

    data_value_descriptor =
      r_ComponentType(
        name: :"data-value-descriptor",
        typespec: r_type(def: :ObjectDescriptor),
        prop: :OPTIONAL
      )

    data_value =
      r_ComponentType(
        name: :"data-value",
        typespec: r_type(def: :"OCTET STRING"),
        prop: :mandatory
      )

    direct_reference =
      r_ComponentType(
        name: :"direct-reference",
        typespec: r_type(def: :"OBJECT IDENTIFIER"),
        prop: :OPTIONAL,
        tags: [{:UNIVERSAL, 6}]
      )

    indirect_reference =
      r_ComponentType(
        name: :"indirect-reference",
        typespec: r_type(def: :INTEGER),
        prop: :OPTIONAL,
        tags: [{:UNIVERSAL, 2}]
      )

    single_ASN1_type =
      r_ComponentType(
        name: :"single-ASN1-type",
        typespec:
          r_type(
            tag: [{:tag, :CONTEXT, 0, :EXPLICIT, 32}],
            def: :ANY
          ),
        prop: :mandatory,
        tags: [{:CONTEXT, 0}]
      )

    octet_aligned =
      r_ComponentType(
        name: :"octet-aligned",
        typespec:
          r_type(
            tag: [{:tag, :CONTEXT, 1, :IMPLICIT, 0}],
            def: :"OCTET STRING"
          ),
        prop: :mandatory,
        tags: [{:CONTEXT, 1}]
      )

    arbitrary =
      r_ComponentType(
        name: :arbitrary,
        typespec:
          r_type(
            tag: [{:tag, :CONTEXT, 2, :IMPLICIT, 0}],
            def: {:"BIT STRING", []}
          ),
        prop: :mandatory,
        tags: [{:CONTEXT, 2}]
      )

    encoding =
      r_ComponentType(
        name: :encoding,
        typespec: r_type(def: {:CHOICE, [single_ASN1_type, octet_aligned, arbitrary]}),
        prop: :mandatory
      )

    eXTERNAL_components1990 = [
      direct_reference,
      indirect_reference,
      data_value_descriptor,
      encoding
    ]

    abstract =
      r_ComponentType(
        name: :abstract,
        typespec: r_type(def: :"OBJECT IDENTIFIER"),
        prop: :mandatory
      )

    transfer =
      r_ComponentType(
        name: :transfer,
        typespec: r_type(def: :"OBJECT IDENTIFIER"),
        prop: :mandatory
      )

    abstractTrSeq = r_SEQUENCE(components: [abstract, transfer])

    syntaxes =
      r_ComponentType(name: :syntaxes, typespec: r_type(def: abstractTrSeq), prop: :mandatory)

    fixed = r_ComponentType(name: :fixed, typespec: r_type(def: :NULL), prop: :mandatory)
    negotiations = [syntaxes, syntax, presentation_Cid, context_negot, transfer_syntax, fixed]

    identification2 =
      r_ComponentType(
        name: :identification,
        typespec: r_type(def: {:CHOICE, negotiations}),
        prop: :mandatory
      )

    embeddedPdv_components = [identification2, data_value]

    string_value =
      r_ComponentType(
        name: :"string-value",
        typespec: r_type(def: :"OCTET STRING"),
        prop: :mandatory
      )

    characterString_components = [identification2, string_value]

    [
      {:EXTERNAL,
       r_type(
         tag: [r_tag(class: :UNIVERSAL, number: 8, type: :IMPLICIT, form: 32)],
         def: r_SEQUENCE(components: eXTERNAL_components1990)
       )},
      {:"EMBEDDED PDV",
       r_type(
         tag: [r_tag(class: :UNIVERSAL, number: 11, type: :IMPLICIT, form: 32)],
         def: r_SEQUENCE(components: embeddedPdv_components)
       )},
      {:"CHARACTER STRING",
       r_type(
         tag: [r_tag(class: :UNIVERSAL, number: 29, type: :IMPLICIT, form: 32)],
         def: r_SEQUENCE(components: characterString_components)
       )}
    ]
  end

  defp include_default_class(s, module) do
    _ =
      for classDef <- default_class_list() do
        include_default_class1(s, module, classDef)
      end

    :ok
  end

  defp include_default_class1(s, module, {name, ts0}) do
    case :asn1_db.dbget(module, name) do
      :undefined ->
        r_objectclass(fields: fields, syntax: {:"WITH SYNTAX", syntax0}) = ts0
        syntax = preprocess_syntax(s, syntax0, fields)
        ts = r_objectclass(ts0, syntax: {:preprocessed_syntax, syntax})
        c = r_classdef(checked: true, module: module, name: name, typespec: ts)
        :asn1_db.dbput(module, name, c)

      _ ->
        :ok
    end
  end

  defp default_class_list() do
    [
      {:"TYPE-IDENTIFIER",
       r_objectclass(
         fields: [
           {:fixedtypevaluefield, :id,
            r_type(
              tag: [r_tag(class: :UNIVERSAL, number: 6, type: :IMPLICIT, form: 0)],
              def: :"OBJECT IDENTIFIER"
            ), :UNIQUE, :MANDATORY},
           {:typefield, :Type, :MANDATORY}
         ],
         syntax:
           {:"WITH SYNTAX",
            [{:typefieldreference, :Type}, :IDENTIFIED, :BY, {:valuefieldreference, :id}]}
       )},
      {:"ABSTRACT-SYNTAX",
       r_objectclass(
         fields: [
           {:fixedtypevaluefield, :id,
            r_type(
              tag: [r_tag(class: :UNIVERSAL, number: 6, type: :IMPLICIT, form: 0)],
              def: :"OBJECT IDENTIFIER"
            ), :UNIQUE, :MANDATORY},
           {:typefield, :Type, :MANDATORY},
           {:fixedtypevaluefield, :property,
            r_type(
              tag: [r_tag(class: :UNIVERSAL, number: 3, type: :IMPLICIT, form: 0)],
              def: {:"BIT STRING", []}
            ), :undefined, {:DEFAULT, [0, 1, 0]}}
         ],
         syntax:
           {:"WITH SYNTAX",
            [
              {:typefieldreference, :Type},
              :IDENTIFIED,
              :BY,
              {:valuefieldreference, :id},
              [:HAS, :PROPERTY, {:valuefieldreference, :property}]
            ]}
       )}
    ]
  end

  defp new_reference_name(name) do
    case :erlang.get(:asn1_reference) do
      :undefined ->
        :erlang.put(:asn1_reference, 1)
        :erlang.list_to_atom(:lists.concat([:internal_, name, '_', 1]))

      num when is_integer(num) ->
        :erlang.put(:asn1_reference, num + 1)
        :erlang.list_to_atom(:lists.concat([:internal_, name, '_', num + 1]))
    end
  end

  defp get_record_prefix_name(s) do
    case :lists.keysearch(:record_name_prefix, 1, r_state(s, :options)) do
      {:value, {_, prefix}} ->
        prefix

      _ ->
        ''
    end
  end

  defp insert_once(s, tab, key) do
    case :erlang.get(:top_module) do
      m when m == r_state(s, :mname) ->
        :asn1ct_gen.insert_once(tab, key)
        :ok

      _ ->
        :skipped
    end
  end

  defp check_fold(s0, [h | t], check) do
    type = :asn1_db.dbget(r_state(s0, :mname), h)
    s = r_state(s0, error_context: type)

    case check.(s, h, type) do
      :ok ->
        check_fold(s, t, check)

      error ->
        [error | check_fold(s, t, check)]
    end
  end

  defp check_fold(_, [], check) when is_function(check, 3) do
    []
  end

  defp error_value(value) when is_integer(value) do
    value
  end

  defp error_value(value) when is_atom(value) do
    value
  end

  defp error_value(r_type(def: value)) when is_atom(value) do
    value
  end

  defp error_value(r_type(def: value)) do
    error_value(value)
  end

  defp error_value(refOrType) do
    try do
      name_of_def(refOrType)
    catch
      _, _ ->
        case get_datastr_name(refOrType) do
          :undefined ->
            refOrType

          name ->
            name
        end
    else
      name ->
        name
    end
  end

  defp name_of_def(r_Externaltypereference(type: n)) do
    n
  end

  defp name_of_def(r_Externalvaluereference(value: n)) do
    n
  end
end
