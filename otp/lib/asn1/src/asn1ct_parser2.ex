defmodule :m_asn1ct_parser2 do
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

  Record.defrecord(:r_typereference, :typereference,
    pos: :undefined,
    val: :undefined
  )

  Record.defrecord(:r_constraint, :constraint,
    c: :undefined,
    e: :undefined
  )

  Record.defrecord(:r_identifier, :identifier,
    pos: :undefined,
    val: :undefined
  )

  def parse(file0, tokens0) do
    try do
      do_parse(tokens0)
    catch
      {:asn1_error, fun} when is_function(fun, 0) ->
        handle_parse_error(file0, fun.())

      {:asn1_error, {:parse_error, tokens}} ->
        handle_parse_error(file0, tokens)
    else
      {:ok, r_module()} = result ->
        result
    after
      clean_process_dictionary()
    end
  end

  defp handle_parse_error(file0, [token | _]) do
    file = :filename.basename(file0)
    line = get_line(token)
    error = {:structured_error, {file, line}, :asn1ct_parser2, {:syntax_error, get_token(token)}}
    {:error, [error]}
  end

  defp do_parse(tokens0) do
    {moduleDefinition, tokens1} = parse_ModuleDefinition(tokens0)
    {types, tokens2} = parse_AssignmentList(tokens1)

    case tokens2 do
      [{:END, _} | _Rest3] ->
        {:ok, r_module(moduleDefinition, typeorval: types)}

      _ ->
        parse_error(tokens2)
    end
  end

  defp clean_process_dictionary() do
    mod = :erlang.erase(:asn1_module)
    _ = :erlang.erase({mod, :imports})
    _ = :erlang.erase(:tagdefault)
    _ = :erlang.erase(:extensiondefault)
    :ok
  end

  def format_error({:syntax_error, token}) when is_atom(token) do
    :io_lib.format('syntax error before: \'~s\'', [token])
  end

  def format_error({:syntax_error, token}) do
    :io_lib.format('syntax error before: \'~p\'', [token])
  end

  defp parse_ModuleDefinition([
         {:typereference, l1, moduleIdentifier}
         | rest0
       ]) do
    :erlang.put(:asn1_module, moduleIdentifier)

    {_DefinitiveIdentifier, rest02} =
      case rest0 do
        [{:"{", _} | _Rest01] ->
          parse_ObjectIdentifierValue(rest0)

        _ ->
          {[], rest0}
      end

    rest =
      case rest02 do
        [{:DEFINITIONS, _} | rest03] ->
          rest03

        _ ->
          parse_error(rest02)
      end

    {tagDefault, rest2} =
      case rest do
        [[{:EXPLICIT, _L3}, {:TAGS, _L4}] | rest1] ->
          :erlang.put(:tagdefault, :EXPLICIT)
          {:EXPLICIT, rest1}

        [[{:IMPLICIT, _L3}, {:TAGS, _L4}] | rest1] ->
          :erlang.put(:tagdefault, :IMPLICIT)
          {:IMPLICIT, rest1}

        [[{:AUTOMATIC, _L3}, {:TAGS, _L4}] | rest1] ->
          :erlang.put(:tagdefault, :AUTOMATIC)
          {:AUTOMATIC, rest1}

        rest1 ->
          :erlang.put(:tagdefault, :EXPLICIT)
          {:EXPLICIT, rest1}
      end

    {extensionDefault, rest3} =
      case rest2 do
        [
          [{:EXTENSIBILITY, _L5}, {:IMPLIED, _L6}]
          | rest21
        ] ->
          :erlang.put(:extensiondefault, :IMPLIED)
          {:IMPLIED, rest21}

        _ ->
          :erlang.put(:extensiondefault, :undefined)
          {:undefined, rest2}
      end

    case rest3 do
      [[{:"::=", _L7}, {:BEGIN, _L8}] | rest4] ->
        {exports, rest5} = parse_Exports(rest4)
        {{:imports, imports}, rest6} = parse_Imports(rest5)

        :erlang.put(
          {:erlang.get(:asn1_module), :imports},
          imports
        )

        {r_module(
           pos: l1,
           name: moduleIdentifier,
           defid: [],
           tagdefault: tagDefault,
           extensiondefault: extensionDefault,
           exports: exports,
           imports: {:imports, imports}
         ), rest6}

      _ ->
        parse_error(rest3)
    end
  end

  defp parse_ModuleDefinition(tokens) do
    parse_error(tokens)
  end

  defp parse_Exports([[{:EXPORTS, _L1}, {:";", _L2}] | rest]) do
    {{:exports, []}, rest}
  end

  defp parse_Exports([[{:EXPORTS, _}, {:ALL, _}, {:";", _}] | rest]) do
    {{:exports, :all}, rest}
  end

  defp parse_Exports([{:EXPORTS, _L1} | rest]) do
    {symbolList, rest2} = parse_SymbolList(rest)

    case rest2 do
      [{:";", _} | rest3] ->
        {{:exports, symbolList}, rest3}

      _ ->
        parse_error(rest2)
    end
  end

  defp parse_Exports(rest) do
    {{:exports, :all}, rest}
  end

  defp parse_SymbolList(tokens) do
    parse_SymbolList(tokens, [])
  end

  defp parse_SymbolList(tokens, acc) do
    {symbol, rest} = parse_Symbol(tokens)

    case rest do
      [{:",", _L1} | rest2] ->
        parse_SymbolList(rest2, [symbol | acc])

      rest2 ->
        {:lists.reverse(acc, [symbol]), rest2}
    end
  end

  defp parse_Symbol(tokens) do
    parse_Reference(tokens)
  end

  defp parse_Reference([
         [{:typereference, l1, trefName}, {:"{", _L2}, {:"}", _L3}]
         | rest
       ]) do
    {tref2Exttref(l1, trefName), rest}
  end

  defp parse_Reference([
         [
           tref1 = {:typereference, _, _},
           {:., _},
           tref2 = {:typereference, _, _},
           {:"{", _L2},
           {:"}", _L3}
         ]
         | rest
       ]) do
    {{tref2Exttref(tref1), tref2Exttref(tref2)}, rest}
  end

  defp parse_Reference([
         tref = {:typereference, _L1, _TrefName}
         | rest
       ]) do
    {tref2Exttref(tref), rest}
  end

  defp parse_Reference([[r_identifier() = vref, {:"{", _L2}, {:"}", _L3}] | rest]) do
    {identifier2Extvalueref(vref), rest}
  end

  defp parse_Reference([r_identifier() = vref | rest]) do
    {identifier2Extvalueref(vref), rest}
  end

  defp parse_Reference(tokens) do
    parse_error(tokens)
  end

  defp parse_Imports([[{:IMPORTS, _L1}, {:";", _L2}] | rest]) do
    {{:imports, []}, rest}
  end

  defp parse_Imports([{:IMPORTS, _L1} | rest]) do
    {symbolsFromModuleList, rest2} = parse_SymbolsFromModuleList(rest)

    case rest2 do
      [{:";", _L2} | rest3] ->
        {{:imports, symbolsFromModuleList}, rest3}

      _ ->
        parse_error(rest2)
    end
  end

  defp parse_Imports(tokens) do
    {{:imports, []}, tokens}
  end

  defp parse_SymbolsFromModuleList(tokens) do
    parse_SymbolsFromModuleList(tokens, [])
  end

  defp parse_SymbolsFromModuleList(tokens, acc) do
    {symbolsFromModule, rest} = parse_SymbolsFromModule(tokens)

    try do
      parse_SymbolsFromModule(rest)
    catch
      {:asn1_error, _} ->
        {:lists.reverse(acc, [symbolsFromModule]), rest}
    else
      {sl, _Rest2} when elem(sl, 0) === :SymbolsFromModule ->
        parse_SymbolsFromModuleList(
          rest,
          [symbolsFromModule | acc]
        )
    end
  end

  defp parse_SymbolsFromModule(tokens) do
    setRefModuleName = fn n ->
      fn
        x when elem(x, 0) === :Externaltypereference ->
          r_Externaltypereference(x, module: n)

        x when elem(x, 0) === :Externalvaluereference ->
          r_Externalvaluereference(x, module: n)
      end
    end

    {symbolList, rest} = parse_SymbolList(tokens)

    case rest do
      [
        [{:FROM, _L1}, {:typereference, _, name} = tref]
        | [[r_identifier(), {:",", _}] | _] = rest2
      ] ->
        newSymbolList =
          :lists.map(
            setRefModuleName.(name),
            symbolList
          )

        {r_SymbolsFromModule(symbols: newSymbolList, module: tref2Exttref(tref)), rest2}

      [
        [{:FROM, _L1}, {:typereference, _, name} = tref]
        | [[r_identifier(), {:FROM, _}] | _] = rest2
      ] ->
        newSymbolList =
          :lists.map(
            setRefModuleName.(name),
            symbolList
          )

        {r_SymbolsFromModule(symbols: newSymbolList, module: tref2Exttref(tref)), rest2}

      [
        [{:FROM, _L1}, {:typereference, _, name} = tref, r_identifier()]
        | rest2
      ] ->
        newSymbolList =
          :lists.map(
            setRefModuleName.(name),
            symbolList
          )

        {r_SymbolsFromModule(symbols: newSymbolList, module: tref2Exttref(tref)), rest2}

      [
        [{:FROM, _L1}, {:typereference, _, name} = tref]
        | [{:"{", _} | _] = rest2
      ] ->
        {_ObjIdVal, rest3} = parse_ObjectIdentifierValue(rest2)

        newSymbolList =
          :lists.map(
            setRefModuleName.(name),
            symbolList
          )

        {r_SymbolsFromModule(symbols: newSymbolList, module: tref2Exttref(tref)), rest3}

      [
        [{:FROM, _L1}, {:typereference, _, name} = tref]
        | rest2
      ] ->
        newSymbolList =
          :lists.map(
            setRefModuleName.(name),
            symbolList
          )

        {r_SymbolsFromModule(symbols: newSymbolList, module: tref2Exttref(tref)), rest2}

      _ ->
        parse_error(rest)
    end
  end

  defp parse_ObjectIdentifierValue([{:"{", _} | rest]) do
    parse_ObjectIdentifierValue(rest, [])
  end

  defp parse_ObjectIdentifierValue([{:number, _, num} | rest], acc) do
    parse_ObjectIdentifierValue(rest, [num | acc])
  end

  defp parse_ObjectIdentifierValue(
         [
           [r_identifier(val: id), {:"(", _}, {:number, _, num}, {:")", _}]
           | rest
         ],
         acc
       ) do
    parse_ObjectIdentifierValue(
      rest,
      [{:NamedNumber, id, num} | acc]
    )
  end

  defp parse_ObjectIdentifierValue(
         [
           [r_identifier(val: id), {:"(", _}, r_identifier(val: id2), {:")", _}]
           | rest
         ],
         acc
       ) do
    parse_ObjectIdentifierValue(
      rest,
      [{:NamedNumber, id, id2} | acc]
    )
  end

  defp parse_ObjectIdentifierValue(
         [
           [
             r_identifier(val: id),
             {:"(", _},
             {:typereference, _, tref},
             {:., _},
             r_identifier(val: id2),
             {:")", _}
           ]
           | rest
         ],
         acc
       ) do
    parse_ObjectIdentifierValue(
      rest,
      [
        {:NamedNumber, id, {:ExternalValue, tref, id2}}
        | acc
      ]
    )
  end

  defp parse_ObjectIdentifierValue([r_identifier() = id | rest], acc) do
    parse_ObjectIdentifierValue(
      rest,
      [identifier2Extvalueref(id) | acc]
    )
  end

  defp parse_ObjectIdentifierValue([{:"}", _} | rest], acc) do
    {:lists.reverse(acc), rest}
  end

  defp parse_ObjectIdentifierValue(tokens, _Acc) do
    parse_error(tokens)
  end

  defp parse_AssignmentList(tokens) do
    parse_AssignmentList(tokens, [])
  end

  defp parse_AssignmentList([{:END, _} | _] = tokens, acc) do
    {:lists.reverse(acc), tokens}
  end

  defp parse_AssignmentList([{:"$end", _} | _] = tokens, acc) do
    {:lists.reverse(acc), tokens}
  end

  defp parse_AssignmentList(tokens0, acc) do
    {assignment, tokens} = parse_Assignment(tokens0)
    parse_AssignmentList(tokens, [assignment | acc])
  end

  defp parse_Assignment([
         [{:typereference, l1, name}, {:"::=", _}]
         | tokens0
       ]) do
    flist = [{:type, &parse_Type/1}, {:class, &parse_ObjectClass/1}]

    case parse_or_tag(tokens0, flist) do
      {{:type, type}, tokens} ->
        {r_typedef(pos: l1, name: name, typespec: type), tokens}

      {{:class, type}, tokens} ->
        {r_classdef(pos: l1, name: name, module: resolve_module(type), typespec: type), tokens}
    end
  end

  defp parse_Assignment(
         [
           [{:typereference, _, _}, {:"{", _}]
           | _
         ] = tokens
       ) do
    flist = [
      &parse_ParameterizedTypeAssignment/1,
      &parse_ParameterizedValueSetTypeAssignment/1,
      &parse_ParameterizedObjectClassAssignment/1
    ]

    parse_or(tokens, flist)
  end

  defp parse_Assignment([{:typereference, _, _} | _] = tokens) do
    flist = [&parse_ObjectSetAssignment/1, &parse_ValueSetTypeAssignment/1]
    parse_or(tokens, flist)
  end

  defp parse_Assignment([[r_identifier(), {:"{", _}] | _] = tokens) do
    flist = [&parse_ParameterizedValueAssignment/1, &parse_ParameterizedObjectAssignment/1]
    parse_or(tokens, flist)
  end

  defp parse_Assignment([r_identifier() | _] = tokens) do
    flist = [&parse_ValueAssignment/1, &parse_ObjectAssignment/1]
    parse_or(tokens, flist)
  end

  defp parse_Assignment(tokens) do
    parse_error(tokens)
  end

  defp parse_or(tokens, flist) do
    parse_or(tokens, flist, [])
  end

  defp parse_or(tokens, [fun | funs], errList)
       when is_function(fun, 1) do
    try do
      fun.(tokens)
    catch
      {:asn1_error, error} ->
        parse_or(tokens, funs, [error | errList])
    else
      {_, rest} = result when is_list(rest) ->
        result
    end
  end

  defp parse_or(_Tokens, [], errList) do
    throw(
      {:asn1_error,
       fn ->
         prioritize_error(errList)
       end}
    )
  end

  defp parse_or_tag(tokens, flist) do
    parse_or_tag(tokens, flist, [])
  end

  defp parse_or_tag(tokens, [{tag, fun} | funs], errList)
       when is_function(fun, 1) do
    try do
      fun.(tokens)
    catch
      {:asn1_error, error} ->
        parse_or_tag(tokens, funs, [error | errList])
    else
      {parsed, rest} when is_list(rest) ->
        {{tag, parsed}, rest}
    end
  end

  defp parse_or_tag(_Tokens, [], errList) do
    throw(
      {:asn1_error,
       fn ->
         prioritize_error(errList)
       end}
    )
  end

  defp prioritize_error(errors0) do
    errors1 = prioritize_error_1(errors0)

    errors2 =
      for l <- errors1 do
        {length(l), l}
      end

    errors = :lists.sort(errors2)

    [res | _] =
      for {_, l} <- errors do
        l
      end

    res
  end

  defp prioritize_error_1([f | t]) when is_function(f, 0) do
    [f.() | prioritize_error_1(t)]
  end

  defp prioritize_error_1([{:parse_error, tokens} | t]) do
    [tokens | prioritize_error_1(t)]
  end

  defp prioritize_error_1([]) do
    []
  end

  defp parse_Type(tokens) do
    {tag, rest3} =
      case tokens do
        [{:"[", _} | _] ->
          parse_Tag(tokens)

        _ ->
          {[], tokens}
      end

    {tag2, rest4} =
      case rest3 do
        [{:IMPLICIT, _} | rest31] when elem(tag, 0) === :tag ->
          {[r_tag(tag, type: :IMPLICIT)], rest31}

        [{:EXPLICIT, _} | rest31] when elem(tag, 0) === :tag ->
          {[r_tag(tag, type: :EXPLICIT)], rest31}

        rest31 when elem(tag, 0) === :tag ->
          {[r_tag(tag, type: {:default, :erlang.get(:tagdefault)})], rest31}

        rest31 ->
          {tag, rest31}
      end

    flist = [&parse_BuiltinType/1, &parse_ReferencedType/1, &parse_TypeWithConstraint/1]
    {type, rest5} = parse_or(rest4, flist)

    case rest5 do
      [{:"(", _} | _] ->
        {constraints, rest6} = parse_Constraints(rest5)

        {r_type(type,
           tag: tag2,
           constraint: merge_constraints(constraints)
         ), rest6}

      [_ | _] ->
        {r_type(type, tag: tag2), rest5}
    end
  end

  defp parse_BuiltinType([[{:BIT, _}, {:STRING, _}] | rest]) do
    case rest do
      [{:"{", _} | rest2] ->
        {namedNumberList, rest3} = parse_NamedNumberList(rest2)

        case rest3 do
          [{:"}", _} | rest4] ->
            {r_type(def: {:"BIT STRING", namedNumberList}), rest4}

          _ ->
            parse_error(rest3)
        end

      _ ->
        {r_type(def: {:"BIT STRING", []}), rest}
    end
  end

  defp parse_BuiltinType([{:BOOLEAN, _} | rest]) do
    {r_type(def: :BOOLEAN), rest}
  end

  defp parse_BuiltinType([
         {:restrictedcharacterstringtype, _, stringName}
         | rest
       ]) do
    {r_type(def: stringName), rest}
  end

  defp parse_BuiltinType([[{:CHARACTER, _}, {:STRING, _}] | rest]) do
    {r_type(def: :"CHARACTER STRING"), rest}
  end

  defp parse_BuiltinType([[{:CHOICE, _}, {:"{", _}] | rest]) do
    {l0, rest2} = parse_AlternativeTypeLists(rest)

    case rest2 do
      [{:"}", _} | rest3] ->
        needExt =
          not :lists.keymember(:EXTENSIONMARK, 1, l0) and
            :erlang.get(:extensiondefault) === :IMPLIED

        l =
          case needExt do
            true ->
              l0 ++ [r_EXTENSIONMARK()]

            false ->
              l0
          end

        {r_type(def: {:CHOICE, l}), rest3}

      _ ->
        parse_error(rest2)
    end
  end

  defp parse_BuiltinType([[{:EMBEDDED, _}, {:PDV, _}] | rest]) do
    {r_type(def: :"EMBEDDED PDV"), rest}
  end

  defp parse_BuiltinType([[{:ENUMERATED, _}, {:"{", _}] | rest]) do
    {enumerations, rest2} = parse_Enumerations(rest)

    case rest2 do
      [{:"}", _} | rest3] ->
        {r_type(def: {:ENUMERATED, enumerations}), rest3}

      _ ->
        parse_error(rest2)
    end
  end

  defp parse_BuiltinType([{:EXTERNAL, _} | rest]) do
    {r_type(def: :EXTERNAL), rest}
  end

  defp parse_BuiltinType([[{:INSTANCE, _}, {:OF, _}] | rest]) do
    {definedObjectClass, rest2} = parse_DefinedObjectClass(rest)

    case rest2 do
      [{:"(", _} | _] ->
        {constraint0, rest3} = parse_Constraint(rest2)
        constraint = merge_constraints([constraint0])
        {r_type(def: {:"INSTANCE OF", definedObjectClass, constraint}), rest3}

      _ ->
        {r_type(def: {:"INSTANCE OF", definedObjectClass, []}), rest2}
    end
  end

  defp parse_BuiltinType([{:INTEGER, _} | rest]) do
    case rest do
      [{:"{", _} | rest2] ->
        {namedNumberList, rest3} = parse_NamedNumberList(rest2)

        case rest3 do
          [{:"}", _} | rest4] ->
            {r_type(def: {:INTEGER, namedNumberList}), rest4}

          _ ->
            parse_error(rest3)
        end

      _ ->
        {r_type(def: :INTEGER), rest}
    end
  end

  defp parse_BuiltinType([{:NULL, _} | rest]) do
    {r_type(def: :NULL), rest}
  end

  defp parse_BuiltinType([[{:OBJECT, _}, {:IDENTIFIER, _}] | rest]) do
    {r_type(def: :"OBJECT IDENTIFIER"), rest}
  end

  defp parse_BuiltinType([[{:OCTET, _}, {:STRING, _}] | rest]) do
    {r_type(def: :"OCTET STRING"), rest}
  end

  defp parse_BuiltinType([{:REAL, _} | rest]) do
    {r_type(def: :REAL), rest}
  end

  defp parse_BuiltinType([{:"RELATIVE-OID", _} | rest]) do
    {r_type(def: :"RELATIVE-OID"), rest}
  end

  defp parse_BuiltinType([[{:SEQUENCE, _}, {:"{", _}, {:"}", _}] | rest]) do
    {r_type(def: r_SEQUENCE(components: [])), rest}
  end

  defp parse_BuiltinType([
         [{:SEQUENCE, _}, {:"{", _}, {:..., line}, {:"}", _}]
         | rest
       ]) do
    {r_type(def: r_SEQUENCE(components: [r_EXTENSIONMARK(pos: line)])), rest}
  end

  defp parse_BuiltinType([
         [{:SEQUENCE, _}, {:"{", _}, {:..., line}, {:!, _}]
         | rest
       ]) do
    {exceptionIdentification, rest2} = parse_ExceptionIdentification(rest)

    case rest2 do
      [{:"}", _} | rest3] ->
        {r_type(
           def:
             r_SEQUENCE(
               components: [
                 r_EXTENSIONMARK(
                   pos: line,
                   val: exceptionIdentification
                 )
               ]
             )
         ), rest3}

      _ ->
        {componentTypeLists, rest3} =
          parse_ComponentTypeLists2(
            rest2,
            [r_EXTENSIONMARK(pos: line)]
          )

        case rest3 do
          [{:"}", _} | rest4] ->
            {r_type(def: r_SEQUENCE(components: componentTypeLists)), rest4}

          _ ->
            parse_error(rest3)
        end
    end
  end

  defp parse_BuiltinType([[{:SEQUENCE, _}, {:"{", _}] | rest]) do
    {componentTypeLists, rest2} = parse_ComponentTypeLists(rest)

    case rest2 do
      [{:"}", _} | rest3] ->
        componentTypeLists2 =
          case {for ext = r_EXTENSIONMARK() <- componentTypeLists do
                  ext
                end, :erlang.get(:extensiondefault)} do
            {[], :IMPLIED} ->
              componentTypeLists ++ [r_EXTENSIONMARK()]

            _ ->
              componentTypeLists
          end

        {r_type(def: r_SEQUENCE(components: componentTypeLists2)), rest3}

      _ ->
        parse_error(rest2)
    end
  end

  defp parse_BuiltinType([
         [{:SEQUENCE, _}, {:OF, _}]
         | [
             [r_identifier(), {:<, _}]
             | _
           ] = tokens0
       ]) do
    {type, tokens} = parse_SelectionType(tokens0)
    {r_type(def: {:"SEQUENCE OF", type}), tokens}
  end

  defp parse_BuiltinType([[{:SEQUENCE, _}, {:OF, _}, r_identifier()] | rest]) do
    {type, rest2} = parse_Type(rest)
    {r_type(def: {:"SEQUENCE OF", type}), rest2}
  end

  defp parse_BuiltinType([[{:SEQUENCE, _}, {:OF, _}] | rest]) do
    {type, rest2} = parse_Type(rest)
    {r_type(def: {:"SEQUENCE OF", type}), rest2}
  end

  defp parse_BuiltinType([
         [{:SET, _}, {:"{", _}, {:..., line}, {:"}", _}]
         | rest
       ]) do
    {r_type(def: r_SET(components: [r_EXTENSIONMARK(pos: line)])), rest}
  end

  defp parse_BuiltinType([
         [{:SET, _}, {:"{", _}, {:..., line}, {:!, _}]
         | rest
       ]) do
    {exceptionIdentification, rest2} = parse_ExceptionIdentification(rest)

    case rest2 do
      [{:"}", _} | rest3] ->
        {r_type(
           def:
             r_SET(
               components: [
                 r_EXTENSIONMARK(
                   pos: line,
                   val: exceptionIdentification
                 )
               ]
             )
         ), rest3}

      _ ->
        {componentTypeLists, rest3} =
          parse_ComponentTypeLists2(
            rest2,
            [r_EXTENSIONMARK(pos: line)]
          )

        case rest3 do
          [{:"}", _} | rest4] ->
            {r_type(def: r_SET(components: componentTypeLists)), rest4}

          _ ->
            parse_error(rest3)
        end
    end
  end

  defp parse_BuiltinType([[{:SET, _}, {:"{", _}] | rest]) do
    {componentTypeLists, rest2} = parse_ComponentTypeLists(rest)

    case rest2 do
      [{:"}", _} | rest3] ->
        componentTypeLists2 =
          case {for ext = r_EXTENSIONMARK() <- componentTypeLists do
                  ext
                end, :erlang.get(:extensiondefault)} do
            {[], :IMPLIED} ->
              componentTypeLists ++ [r_EXTENSIONMARK()]

            _ ->
              componentTypeLists
          end

        {r_type(def: r_SET(components: componentTypeLists2)), rest3}

      _ ->
        parse_error(rest2)
    end
  end

  defp parse_BuiltinType([
         [{:SET, _}, {:OF, _}]
         | [
             [r_identifier(), {:<, _}]
             | _
           ] = tokens0
       ]) do
    {type, tokens} = parse_SelectionType(tokens0)
    {r_type(def: {:"SET OF", type}), tokens}
  end

  defp parse_BuiltinType([[{:SET, _}, {:OF, _}, r_identifier()] | rest]) do
    {type, rest2} = parse_Type(rest)
    {r_type(def: {:"SET OF", type}), rest2}
  end

  defp parse_BuiltinType([[{:SET, _}, {:OF, _}] | rest]) do
    {type, rest2} = parse_Type(rest)
    {r_type(def: {:"SET OF", type}), rest2}
  end

  defp parse_BuiltinType([{:GeneralizedTime, _} | rest]) do
    {r_type(def: :GeneralizedTime), rest}
  end

  defp parse_BuiltinType([{:UTCTime, _} | rest]) do
    {r_type(def: :UTCTime), rest}
  end

  defp parse_BuiltinType([{:ObjectDescriptor, _} | rest]) do
    {r_type(def: :ObjectDescriptor), rest}
  end

  defp parse_BuiltinType([
         [{:ANY, _}, {:DEFINED, _}, {:BY, _}, r_identifier(val: id)]
         | rest
       ]) do
    {r_type(def: {:ANY_DEFINED_BY, id}), rest}
  end

  defp parse_BuiltinType([{:ANY, _} | rest]) do
    {r_type(def: :ANY), rest}
  end

  defp parse_BuiltinType(tokens) do
    parse_ObjectClassFieldType(tokens)
  end

  defp parse_TypeWithConstraint([{:SEQUENCE, _} | [{:"(", _} | _] = rest0]) do
    {constraint, rest2} = parse_Constraint(rest0)

    rest4 =
      case rest2 do
        [[{:OF, _}, r_identifier()] | rest3] ->
          rest3

        [{:OF, _} | rest3] ->
          rest3

        _ ->
          parse_error(rest2)
      end

    {type, rest5} = parse_Type(rest4)

    {r_type(
       def: {:"SEQUENCE OF", type},
       constraint: merge_constraints([constraint])
     ), rest5}
  end

  defp parse_TypeWithConstraint([
         [{:SEQUENCE, _}, {:SIZE, _}]
         | [
             {:"(", _}
             | _
           ] = rest0
       ]) do
    {constraint, rest2} = parse_Constraint(rest0)
    r_constraint(c: c) = constraint
    constraint2 = r_constraint(constraint, c: {:element_set, {:SizeConstraint, c}, :none})

    rest4 =
      case rest2 do
        [[{:OF, _}, r_identifier()] | rest3] ->
          rest3

        [{:OF, _} | rest3] ->
          rest3

        _ ->
          parse_error(rest2)
      end

    {type, rest5} = parse_Type(rest4)

    {r_type(
       def: {:"SEQUENCE OF", type},
       constraint: merge_constraints([constraint2])
     ), rest5}
  end

  defp parse_TypeWithConstraint([{:SET, _} | [{:"(", _} | _] = rest0]) do
    {constraint, rest2} = parse_Constraint(rest0)

    rest4 =
      case rest2 do
        [[{:OF, _}, r_identifier()] | rest3] ->
          rest3

        [{:OF, _} | rest3] ->
          rest3

        _ ->
          parse_error(rest2)
      end

    {type, rest5} = parse_Type(rest4)

    {r_type(
       def: {:"SET OF", type},
       constraint: merge_constraints([constraint])
     ), rest5}
  end

  defp parse_TypeWithConstraint([
         [{:SET, _}, {:SIZE, _}]
         | [
             {:"(", _}
             | _
           ] = rest0
       ]) do
    {constraint, rest2} = parse_Constraint(rest0)
    r_constraint(c: c) = constraint
    constraint2 = r_constraint(constraint, c: {:element_set, {:SizeConstraint, c}, :none})

    rest4 =
      case rest2 do
        [[{:OF, _}, r_identifier()] | rest3] ->
          rest3

        [{:OF, _} | rest3] ->
          rest3

        _ ->
          parse_error(rest2)
      end

    {type, rest5} = parse_Type(rest4)

    {r_type(
       def: {:"SET OF", type},
       constraint: merge_constraints([constraint2])
     ), rest5}
  end

  defp parse_TypeWithConstraint(tokens) do
    parse_error(tokens)
  end

  defp parse_ReferencedType(tokens) do
    flist = [
      &parse_ParameterizedType/1,
      &parse_DefinedType/1,
      &parse_SelectionType/1,
      &parse_TypeFromObject/1
    ]

    parse_or(tokens, flist)
  end

  defp parse_DefinedType([
         [{:typereference, l1, module}, {:., _}, {:typereference, _, typeName}]
         | tokens
       ]) do
    {r_type(def: r_Externaltypereference(pos: l1, module: module, type: typeName)), tokens}
  end

  defp parse_DefinedType([{:typereference, _, _} = tr | tokens]) do
    {r_type(def: tref2Exttref(tr)), tokens}
  end

  defp parse_DefinedType(tokens) do
    parse_error(tokens)
  end

  defp parse_SelectionType([[r_identifier(val: name), {:<, _}] | rest]) do
    {type, rest2} = parse_Type(rest)
    {r_type(def: {:SelectionType, name, type}), rest2}
  end

  defp parse_SelectionType(tokens) do
    parse_error(tokens)
  end

  defp resolve_module(type) do
    current = :erlang.get(:asn1_module)
    imports = :erlang.get({current, :imports})
    resolve_module(type, current, imports)
  end

  defp resolve_module(_Type, current, :undefined) do
    current
  end

  defp resolve_module(type, current, imports) do
    case (for r_SymbolsFromModule(symbols: s, module: mod) <- imports,
              r_Externaltypereference(type: t) <- s,
              type === t do
            mod
          end) do
      [r_Externaltypereference(type: mod) | _] ->
        mod

      [] ->
        current
    end
  end

  defp parse_Constraints(tokens) do
    parse_Constraints(tokens, [])
  end

  defp parse_Constraints(tokens, acc) do
    {constraint, rest} = parse_Constraint(tokens)

    case rest do
      [{:"(", _} | _Rest2] ->
        parse_Constraints(rest, [constraint | acc])

      _ ->
        {:lists.reverse(acc, [constraint]), rest}
    end
  end

  defp parse_Constraint([{:"(", _} | rest]) do
    {constraint, rest2} = parse_ConstraintSpec(rest)
    {exception, rest3} = parse_ExceptionSpec(rest2)

    case rest3 do
      [{:")", _} | rest4] ->
        {r_constraint(c: constraint, e: exception), rest4}

      [_ | _] ->
        parse_error(rest3)
    end
  end

  defp parse_ConstraintSpec(tokens) do
    flist = [&parse_GeneralConstraint/1, &parse_SubtypeConstraint/1]
    parse_or(tokens, flist)
  end

  defp parse_ExceptionSpec([lPar = {:")", _} | rest]) do
    {:undefined, [lPar | rest]}
  end

  defp parse_ExceptionSpec([{:!, _} | rest]) do
    parse_ExceptionIdentification(rest)
  end

  defp parse_ExceptionSpec(tokens) do
    parse_error(tokens)
  end

  defp parse_ExceptionIdentification(tokens) do
    flist = [&parse_SignedNumber/1, &parse_DefinedValue/1, &parse_TypeColonValue/1]
    parse_or(tokens, flist)
  end

  defp parse_TypeColonValue(tokens) do
    {type, rest} = parse_Type(tokens)

    case rest do
      [{:":", _} | rest2] ->
        {value, rest3} = parse_Value(rest2)
        {{type, value}, rest3}

      [_ | _] ->
        parse_error(rest)
    end
  end

  defp parse_SubtypeConstraint(tokens) do
    parse_ElementSetSpecs(tokens)
  end

  defp parse_ElementSetSpecs(tokens) do
    {rootElems, rest} = parse_ElementSetSpec(tokens)

    case rest do
      [[{:",", _}, {:..., _}, {:",", _}] | rest2] ->
        {additionalElems, rest3} = parse_ElementSetSpec(rest2)
        {{:element_set, rootElems, additionalElems}, rest3}

      [[{:",", _}, {:..., _}] | rest2] ->
        {{:element_set, rootElems, :empty}, rest2}

      _ ->
        {{:element_set, rootElems, :none}, rest}
    end
  end

  defp parse_ElementSetSpec([[{:ALL, _}, {:EXCEPT, _}] | rest]) do
    {exclusions, rest2} = parse_Elements(rest)
    {{:"ALL-EXCEPT", exclusions}, rest2}
  end

  defp parse_ElementSetSpec(tokens) do
    parse_Unions(tokens)
  end

  defp parse_Unions(tokens) do
    {interSec, rest} = parse_Intersections(tokens)
    {unions, rest2} = parse_UnionsRec(rest)

    case {interSec, unions} do
      {^interSec, []} ->
        {interSec, rest2}

      {v1, v2} ->
        {{:union, v1, v2}, rest2}
    end
  end

  defp parse_UnionsRec([{:|, _} | rest]) do
    {interSec, rest2} = parse_Intersections(rest)
    {uRec, rest3} = parse_UnionsRec(rest2)

    case {interSec, uRec} do
      {v1, []} ->
        {v1, rest3}

      {v1, v2} ->
        {{:union, v1, v2}, rest3}
    end
  end

  defp parse_UnionsRec([{:UNION, info} | rest]) do
    parse_UnionsRec([{:|, info} | rest])
  end

  defp parse_UnionsRec(tokens) do
    {[], tokens}
  end

  defp parse_Intersections(tokens) do
    {interSec, rest} = parse_IntersectionElements(tokens)
    {iRec, rest2} = parse_IElemsRec(rest)

    case {interSec, iRec} do
      {v1, []} ->
        {v1, rest2}

      {v1, v2} ->
        {{:intersection, v1, v2}, rest2}
    end
  end

  defp parse_IElemsRec([{:^, _} | rest]) do
    {interSec, rest2} = parse_IntersectionElements(rest)
    {iRec, rest3} = parse_IElemsRec(rest2)

    case {interSec, iRec} do
      {v1, []} ->
        {v1, rest2}

      {v1, v2} ->
        {{:intersection, v1, v2}, rest3}
    end
  end

  defp parse_IElemsRec([{:INTERSECTION, info} | rest]) do
    parse_IElemsRec([{:^, info} | rest])
  end

  defp parse_IElemsRec(tokens) do
    {[], tokens}
  end

  defp parse_IntersectionElements(tokens) do
    {interSec, rest} = parse_Elements(tokens)

    case rest do
      [{:EXCEPT, _} | rest2] ->
        {exclusion, rest3} = parse_Elements(rest2)
        {{:EXCEPT, interSec, exclusion}, rest3}

      ^rest ->
        {interSec, rest}
    end
  end

  defp parse_Elements([{:"(", _} | rest]) do
    {elems, rest2} = parse_ElementSetSpec(rest)

    case rest2 do
      [{:")", _} | rest3] ->
        {elems, rest3}

      [_ | _] ->
        parse_error(rest2)
    end
  end

  defp parse_Elements(tokens) do
    flist = [
      &parse_ObjectSetElements/1,
      &parse_SubtypeElements/1,
      &parse_Object/1,
      &parse_DefinedObjectSet/1
    ]

    parse_or(tokens, flist)
  end

  defp parse_DefinedObjectClass([
         [{:typereference, _, modName}, {:., _}, {:typereference, pos, name}]
         | tokens
       ]) do
    ext = r_Externaltypereference(pos: pos, module: modName, type: name)
    {ext, tokens}
  end

  defp parse_DefinedObjectClass([
         tr = {:typereference, _, _ObjClName}
         | rest
       ]) do
    {tref2Exttref(tr), rest}
  end

  defp parse_DefinedObjectClass(tokens) do
    parse_error(tokens)
  end

  defp parse_ObjectClass(tokens) do
    flist = [&parse_ObjectClassDefn/1, &parse_DefinedObjectClass/1]
    parse_or(tokens, flist)
  end

  defp parse_ObjectClassDefn([[{:CLASS, _}, {:"{", _}] | rest]) do
    {type, rest2} = parse_FieldSpec(rest)
    {withSyntaxSpec, rest3} = parse_WithSyntaxSpec(rest2)
    {r_objectclass(fields: type, syntax: withSyntaxSpec), rest3}
  end

  defp parse_ObjectClassDefn(tokens) do
    parse_error(tokens)
  end

  defp parse_FieldSpec(tokens) do
    parse_FieldSpec(tokens, [])
  end

  defp parse_FieldSpec(tokens0, acc) do
    fl =
      case tokens0 do
        [{:valuefieldreference, _, _} | _] ->
          [&parse_FixedTypeValueFieldSpec/1, &parse_VariableTypeValueFieldSpec/1]

        [{:typefieldreference, _, _} | _] ->
          [
            &parse_FixedTypeValueSetFieldSpec/1,
            &parse_VariableTypeValueSetFieldSpec/1,
            &parse_TypeFieldSpec/1
          ]

        [_ | _] ->
          parse_error(tokens0)
      end

    case parse_or(tokens0, fl) do
      {type, [{:"}", _} | rest]} ->
        {:lists.reverse(acc, [type]), rest}

      {type, [{:",", _} | rest2]} ->
        parse_FieldSpec(rest2, [type | acc])
    end
  end

  defp parse_PrimitiveFieldName([{:typefieldreference, _, fieldName} | rest]) do
    {{:typefieldreference, fieldName}, rest}
  end

  defp parse_PrimitiveFieldName([
         {:valuefieldreference, _, fieldName}
         | rest
       ]) do
    {{:valuefieldreference, fieldName}, rest}
  end

  defp parse_PrimitiveFieldName(tokens) do
    parse_error(tokens)
  end

  defp parse_FieldName(tokens) do
    {field, rest} = parse_PrimitiveFieldName(tokens)
    parse_FieldName(rest, [field])
  end

  defp parse_FieldName([{:., _} | rest0], acc) do
    {fieldName, rest1} = parse_PrimitiveFieldName(rest0)
    parse_FieldName(rest1, [fieldName | acc])
  end

  defp parse_FieldName(tokens, acc) do
    {:lists.reverse(acc), tokens}
  end

  defp parse_FixedTypeValueFieldSpec([
         {:valuefieldreference, _, vFieldName}
         | rest
       ]) do
    {type, rest2} = parse_Type(rest)

    {unique, rest3} =
      case rest2 do
        [{:UNIQUE, _} | rest4] ->
          {:UNIQUE, rest4}

        _ ->
          {:undefined, rest2}
      end

    {optionalitySpec, rest5} = parse_ValueOptionalitySpec(rest3)

    case is_end_delimiter(rest5) do
      false ->
        parse_error(rest5)

      true ->
        :ok
    end

    tag =
      case unique do
        :UNIQUE ->
          :fixedtypevaluefield

        _ ->
          :object_or_fixedtypevalue_field
      end

    {{tag, vFieldName, type, unique, optionalitySpec}, rest5}
  end

  defp parse_VariableTypeValueFieldSpec([
         {:valuefieldreference, _, vFieldName}
         | rest0
       ]) do
    {fieldRef, rest1} = parse_FieldName(rest0)
    {optionalitySpec, rest} = parse_ValueOptionalitySpec(rest1)

    case is_end_delimiter(rest) do
      true ->
        {{:variabletypevaluefield, vFieldName, fieldRef, optionalitySpec}, rest}

      false ->
        parse_error(rest)
    end
  end

  defp parse_TypeFieldSpec([{:typefieldreference, _, name} | rest0]) do
    {optionalitySpec, rest} = parse_TypeOptionalitySpec(rest0)

    case is_end_delimiter(rest) do
      true ->
        {{:typefield, name, optionalitySpec}, rest}

      false ->
        parse_error(rest)
    end
  end

  defp parse_FixedTypeValueSetFieldSpec([{:typefieldreference, _, name} | rest0]) do
    {type, rest1} = parse_Type(rest0)
    {optionalitySpec, rest} = parse_ValueSetOptionalitySpec(rest1)

    case is_end_delimiter(rest) do
      true ->
        {{:objectset_or_fixedtypevalueset_field, name, type, optionalitySpec}, rest}

      false ->
        parse_error(rest)
    end
  end

  defp parse_VariableTypeValueSetFieldSpec([{:typefieldreference, _, name} | rest0]) do
    {fieldRef, rest1} = parse_FieldName(rest0)
    {optionalitySpec, rest} = parse_ValueSetOptionalitySpec(rest1)

    case is_end_delimiter(rest) do
      true ->
        {{:variabletypevaluesetfield, name, fieldRef, optionalitySpec}, rest}

      false ->
        parse_error(rest)
    end
  end

  defp is_end_delimiter([{:",", _} | _]) do
    true
  end

  defp is_end_delimiter([{:"}", _} | _]) do
    true
  end

  defp is_end_delimiter([_ | _]) do
    false
  end

  defp parse_ValueOptionalitySpec(tokens) do
    case tokens do
      [{:OPTIONAL, _} | rest] ->
        {:OPTIONAL, rest}

      [{:DEFAULT, _} | rest] ->
        {value, rest2} = parse_Value(rest)
        {{:DEFAULT, value}, rest2}

      _ ->
        {:MANDATORY, tokens}
    end
  end

  defp parse_TypeOptionalitySpec(tokens) do
    case tokens do
      [{:OPTIONAL, _} | rest] ->
        {:OPTIONAL, rest}

      [{:DEFAULT, _} | rest] ->
        {type, rest2} = parse_Type(rest)
        {{:DEFAULT, type}, rest2}

      _ ->
        {:MANDATORY, tokens}
    end
  end

  defp parse_ValueSetOptionalitySpec(tokens) do
    case tokens do
      [{:OPTIONAL, _} | rest] ->
        {:OPTIONAL, rest}

      [{:DEFAULT, _} | rest] ->
        {valueSet, rest2} = parse_ValueSet(rest)
        {{:DEFAULT, valueSet}, rest2}

      _ ->
        {:MANDATORY, tokens}
    end
  end

  defp parse_WithSyntaxSpec([[{:WITH, _}, {:SYNTAX, _}] | rest]) do
    {syntaxList, rest2} = parse_SyntaxList(rest)
    {{:"WITH SYNTAX", syntaxList}, rest2}
  end

  defp parse_WithSyntaxSpec(tokens) do
    {[], tokens}
  end

  defp parse_SyntaxList([{:"{", _} | rest]) do
    parse_SyntaxList(rest, [])
  end

  defp parse_SyntaxList(tokens) do
    parse_error(tokens)
  end

  defp parse_SyntaxList(tokens, acc) do
    {syntaxList, rest} = parse_TokenOrGroupSpec(tokens)

    case rest do
      [{:"}", _} | rest2] ->
        {:lists.reverse(acc, [syntaxList]), rest2}

      _ ->
        parse_SyntaxList(rest, [syntaxList | acc])
    end
  end

  defp parse_TokenOrGroupSpec(tokens) do
    flist = [&parse_RequiredToken/1, &parse_OptionalGroup/1]
    parse_or(tokens, flist)
  end

  defp parse_RequiredToken(
         [
           {:typereference, _, wordName}
           | rest
         ] = tokens
       ) do
    case is_word(wordName) do
      false ->
        parse_error(tokens)

      true ->
        {wordName, rest}
    end
  end

  defp parse_RequiredToken([{:",", l1} | rest]) do
    {{:",", l1}, rest}
  end

  defp parse_RequiredToken([{wordName, _} | rest] = tokens) do
    case is_word(wordName) do
      false ->
        parse_error(tokens)

      true ->
        {wordName, rest}
    end
  end

  defp parse_RequiredToken(tokens) do
    parse_PrimitiveFieldName(tokens)
  end

  defp parse_OptionalGroup([{:"[", _} | rest]) do
    {spec, rest2} = parse_TokenOrGroupSpec(rest)
    {specList, rest3} = parse_OptionalGroup(rest2, [spec])
    {specList, rest3}
  end

  defp parse_OptionalGroup(tokens) do
    parse_error(tokens)
  end

  defp parse_OptionalGroup([{:"]", _} | rest], acc) do
    {:lists.reverse(acc), rest}
  end

  defp parse_OptionalGroup(tokens, acc) do
    {spec, rest} = parse_TokenOrGroupSpec(tokens)
    parse_OptionalGroup(rest, [spec | acc])
  end

  defp parse_DefinedObject([r_identifier() = id | rest]) do
    {{:object, identifier2Extvalueref(id)}, rest}
  end

  defp parse_DefinedObject([
         [{:typereference, l1, modName}, {:., _}, r_identifier(val: objName)]
         | rest
       ]) do
    {{:object, r_Externaltypereference(pos: l1, module: modName, type: objName)}, rest}
  end

  defp parse_DefinedObject(tokens) do
    parse_error(tokens)
  end

  defp parse_ObjectAssignment([r_identifier(pos: l1, val: objName) | rest]) do
    {class, rest2} = parse_DefinedObjectClass(rest)

    case rest2 do
      [{:"::=", _} | rest3] ->
        {object, rest4} = parse_Object(rest3)

        {r_typedef(pos: l1, name: objName, typespec: r_Object(classname: class, def: object)),
         rest4}

      _ ->
        parse_error(rest2)
    end
  end

  defp parse_Object(tokens) do
    flist = [&parse_ObjectDefn/1, &parse_DefinedObject/1]
    parse_or(tokens, flist)
  end

  defp parse_ObjectDefn(tokens) do
    flist = [&parse_DefaultSyntax/1, &parse_DefinedSyntax/1]
    parse_or(tokens, flist)
  end

  defp parse_DefaultSyntax([{:"{", _} | rest]) do
    parse_DefaultSyntax(rest, [])
  end

  defp parse_DefaultSyntax(tokens) do
    parse_error(tokens)
  end

  defp parse_DefaultSyntax(tokens, acc) do
    {setting, rest} = parse_FieldSetting(tokens)

    case rest do
      [{:",", _} | rest2] ->
        parse_DefaultSyntax(rest2, [setting | acc])

      [{:"}", _} | rest3] ->
        {{:object, :defaultsyntax, :lists.reverse(acc, [setting])}, rest3}

      _ ->
        parse_error(rest)
    end
  end

  defp parse_FieldSetting(tokens) do
    {{_, primFieldName}, rest} = parse_PrimitiveFieldName(tokens)
    {setting, rest2} = parse_Setting(rest)
    {{primFieldName, setting}, rest2}
  end

  defp parse_DefinedSyntax([{:"{", _} | rest]) do
    parse_DefinedSyntax(rest, [])
  end

  defp parse_DefinedSyntax(tokens) do
    parse_error(tokens)
  end

  defp parse_DefinedSyntax(tokens, acc) do
    case tokens do
      [{:"}", _} | rest2] ->
        {{:object, :definedsyntax, :lists.reverse(acc)}, rest2}

      _ ->
        {defSynTok, rest3} = parse_DefinedSyntaxToken(tokens)
        parse_DefinedSyntax(rest3, [defSynTok | acc])
    end
  end

  defp parse_DefinedSyntaxToken([{:",", _} = comma | rest]) do
    {comma, rest}
  end

  defp parse_DefinedSyntaxToken(
         [
           [{:typereference, _, _Name}, {t, _}]
           | _
         ] = tokens
       )
       when t === :. or t === :"(" do
    parse_Setting(tokens)
  end

  defp parse_DefinedSyntaxToken(
         [
           {:typereference, l1, name} = tRef
           | rest
         ] = tokens
       ) do
    case is_word(name) do
      false ->
        case lookahead_definedsyntax(rest) do
          :word_or_setting ->
            {{:setting, l1, tref2Exttref(tRef)}, rest}

          :setting ->
            parse_Setting(tokens)
        end

      true ->
        {{:word_or_setting, l1, tref2Exttref(tRef)}, rest}
    end
  end

  defp parse_DefinedSyntaxToken(tokens) do
    try do
      parse_Setting(tokens)
    catch
      {:asn1_error, _} ->
        parse_Word(tokens)
    else
      {_, _} = result ->
        result
    end
  end

  defp lookahead_definedsyntax([{:typereference, _, name} | _Rest]) do
    case is_word(name) do
      true ->
        :word_or_setting

      false ->
        :setting
    end
  end

  defp lookahead_definedsyntax([{:"}", _} | _Rest]) do
    :word_or_setting
  end

  defp lookahead_definedsyntax(_) do
    :setting
  end

  defp parse_Word([{name, pos} | rest] = tokens) do
    case is_word(name) do
      false ->
        parse_error(tokens)

      true ->
        {{:word_or_setting, pos, tref2Exttref(pos, name)}, rest}
    end
  end

  defp parse_Word(tokens) do
    parse_error(tokens)
  end

  defp parse_Setting(tokens) do
    flist = [
      {:type_tag, &parse_Type/1},
      {:value_tag, &parse_Value/1},
      {:object_tag, &parse_Object/1},
      {:objectset_tag, &parse_ObjectSet/1}
    ]

    case parse_or_tag(tokens, flist) do
      {{:value_tag, _}, _} = result ->
        result

      {{tag, setting}, rest} when is_atom(tag) ->
        {setting, rest}
    end
  end

  defp parse_DefinedObjectSet([
         [{:typereference, l1, moduleName}, {:., _}, {:typereference, l2, objSetName}]
         | rest
       ]) do
    {{:objectset, l1, r_Externaltypereference(pos: l2, module: moduleName, type: objSetName)},
     rest}
  end

  defp parse_DefinedObjectSet([{:typereference, l1, objSetName} | rest]) do
    {{:objectset, l1,
      r_Externaltypereference(pos: l1, module: resolve_module(objSetName), type: objSetName)},
     rest}
  end

  defp parse_DefinedObjectSet(tokens) do
    parse_error(tokens)
  end

  defp parse_ObjectSetAssignment([{:typereference, l1, objSetName} | rest]) do
    {class, rest2} = parse_DefinedObjectClass(rest)

    case rest2 do
      [{:"::=", _} | rest3] ->
        {objectSet, rest4} = parse_ObjectSet(rest3)

        {r_typedef(
           pos: l1,
           name: objSetName,
           typespec: r_ObjectSet(class: class, set: objectSet)
         ), rest4}

      _ ->
        parse_error(rest2)
    end
  end

  defp parse_ObjectSet([{:"{", _} | rest]) do
    {objSetSpec, rest2} = parse_ObjectSetSpec(rest)

    case rest2 do
      [{:"}", _} | rest3] ->
        {objSetSpec, rest3}

      _ ->
        parse_error(rest2)
    end
  end

  defp parse_ObjectSet(tokens) do
    parse_error(tokens)
  end

  defp parse_ObjectSetSpec([[{:..., _}, {:",", _}] | tokens0]) do
    {elements, tokens} = parse_ElementSetSpec(tokens0)
    {{:element_set, :empty, elements}, tokens}
  end

  defp parse_ObjectSetSpec([{:..., _} | tokens]) do
    {{:element_set, :empty, :empty}, tokens}
  end

  defp parse_ObjectSetSpec(tokens) do
    parse_ElementSetSpecs(tokens)
  end

  defp parse_ObjectSetElements(tokens) do
    flist = [&parse_ObjectSetFromObjects/1, &parse_ParameterizedObjectSet/1]
    parse_or(tokens, flist)
  end

  defp parse_ObjectClassFieldType(tokens) do
    {class, rest} = parse_DefinedObjectClass(tokens)

    case rest do
      [{:., _} | rest2] ->
        {fieldName, rest3} = parse_FieldName(rest2)
        oCFT = r_ObjectClassFieldType(classname: class, class: class, fieldname: fieldName)
        {r_type(def: oCFT), rest3}

      _ ->
        parse_error(rest)
    end
  end

  defp parse_ObjectClassFieldValue(tokens) do
    parse_OpenTypeFieldVal(tokens)
  end

  defp parse_OpenTypeFieldVal(tokens) do
    {type, rest} = parse_Type(tokens)

    case rest do
      [{:":", _} | rest2] ->
        {value, rest3} = parse_Value(rest2)
        {{:opentypefieldvalue, type, value}, rest3}

      _ ->
        parse_error(rest)
    end
  end

  defp parse_ReferencedObjects(tokens) do
    flist = [&parse_DefinedObject/1, &parse_DefinedObjectSet/1, &parse_ParameterizedObjectSet/1]
    parse_or(tokens, flist)
  end

  defp parse_ValueFromObject(tokens) do
    {objects, rest} = parse_ReferencedObjects(tokens)

    case rest do
      [{:., _} | rest2] ->
        {name, rest3} = parse_FieldName(rest2)

        case :lists.last(name) do
          {:valuefieldreference, _} ->
            {{:ValueFromObject, objects, name}, rest3}

          _ ->
            parse_error(rest2)
        end

      _ ->
        parse_error(rest)
    end
  end

  defp parse_TypeFromObject(tokens) do
    {objects, rest} = parse_ReferencedObjects(tokens)

    case rest do
      [{:., _} | rest2] ->
        {name, rest3} = parse_FieldName(rest2)

        case :lists.last(name) do
          {:typefieldreference, _FieldName} ->
            {r_type(def: {:TypeFromObject, objects, name}), rest3}

          _ ->
            parse_error(rest2)
        end

      _ ->
        parse_error(rest)
    end
  end

  defp parse_ObjectSetFromObjects(tokens) do
    {objects, rest} = parse_ReferencedObjects(tokens)

    case rest do
      [{:., _} | rest2] ->
        {name, rest3} = parse_FieldName(rest2)

        case :lists.last(name) do
          {:typefieldreference, _FieldName} ->
            {{:ObjectSetFromObjects, objects, name}, rest3}

          _ ->
            parse_error(rest2)
        end

      _ ->
        parse_error(rest)
    end
  end

  defp parse_GeneralConstraint(tokens) do
    flist = [
      &parse_UserDefinedConstraint/1,
      &parse_TableConstraint/1,
      &parse_ContentsConstraint/1
    ]

    parse_or(tokens, flist)
  end

  defp parse_UserDefinedConstraint([
         [{:CONSTRAINED, _}, {:BY, _}, {:"{", _}, {:"}", _}]
         | rest
       ]) do
    {{:constrained_by, []}, rest}
  end

  defp parse_UserDefinedConstraint([
         [{:CONSTRAINED, _}, {:BY, _}, {:"{", _}]
         | rest
       ]) do
    {param, rest2} = parse_UserDefinedConstraintParameter(rest)

    case rest2 do
      [{:"}", _} | rest3] ->
        {{:constrained_by, param}, rest3}

      _ ->
        parse_error(rest2)
    end
  end

  defp parse_UserDefinedConstraint(tokens) do
    parse_error(tokens)
  end

  defp parse_UserDefinedConstraintParameter(tokens) do
    parse_UserDefinedConstraintParameter(tokens, [])
  end

  defp parse_UserDefinedConstraintParameter(tokens0, acc) do
    flist = [&parse_GovernorAndActualParameter/1, &parse_ActualParameter/1]

    case parse_or(tokens0, flist) do
      {result, [{:",", _} | tokens]} ->
        parse_UserDefinedConstraintParameter(
          tokens,
          [result | acc]
        )

      {result, tokens} ->
        {:lists.reverse(acc, [result]), tokens}
    end
  end

  defp parse_GovernorAndActualParameter(tokens) do
    {governor, rest} = parse_Governor(tokens)

    case rest do
      [{:":", _} | rest2] ->
        {params, rest3} = parse_ActualParameter(rest2)
        {{:Governor_Params, governor, params}, rest3}

      _ ->
        parse_error(rest)
    end
  end

  defp parse_TableConstraint(tokens) do
    flist = [&parse_ComponentRelationConstraint/1, &parse_SimpleTableConstraint/1]
    parse_or(tokens, flist)
  end

  defp parse_SimpleTableConstraint(tokens) do
    {objectSet, rest} = parse_ObjectSet(tokens)
    {{:element_set, {:simpletable, objectSet}, :none}, rest}
  end

  defp parse_ComponentRelationConstraint([{:"{", _} | rest]) do
    {objectSet, rest2} = parse_DefinedObjectSet(rest)

    case rest2 do
      [[{:"}", _}, {:"{", _}] | rest3] ->
        {atNot, rest4} = parse_AtNotationList(rest3, [])

        case rest4 do
          [{:"}", _} | rest5] ->
            ret = {:element_set, {:componentrelation, objectSet, atNot}, :none}
            {ret, rest5}

          _ ->
            parse_error(rest4)
        end

      _ ->
        parse_error(rest2)
    end
  end

  defp parse_ComponentRelationConstraint(tokens) do
    parse_error(tokens)
  end

  defp parse_AtNotationList(tokens, acc) do
    {atNot, rest} = parse_AtNotation(tokens)

    case rest do
      [{:",", _} | rest2] ->
        parse_AtNotationList(rest2, [atNot | acc])

      _ ->
        {:lists.reverse(acc, [atNot]), rest}
    end
  end

  defp parse_AtNotation([[{:@, _}, {:., _}] | rest]) do
    {cIdList, rest2} = parse_ComponentIdList(rest)
    {{:innermost, cIdList}, rest2}
  end

  defp parse_AtNotation([{:@, _} | rest]) do
    {cIdList, rest2} = parse_ComponentIdList(rest)
    {{:outermost, cIdList}, rest2}
  end

  defp parse_AtNotation(tokens) do
    parse_error(tokens)
  end

  defp parse_ComponentIdList(tokens) do
    parse_ComponentIdList(tokens, [])
  end

  defp parse_ComponentIdList([[r_identifier() = id, {:., _}] | rest], acc) do
    parse_ComponentIdList(
      rest,
      [identifier2Extvalueref(id) | acc]
    )
  end

  defp parse_ComponentIdList([r_identifier() = id | rest], acc) do
    {:lists.reverse(acc, [identifier2Extvalueref(id)]), rest}
  end

  defp parse_ComponentIdList(tokens, _) do
    parse_error(tokens)
  end

  defp parse_ContentsConstraint([{:CONTAINING, _} | rest]) do
    {type, rest2} = parse_Type(rest)

    case rest2 do
      [[{:ENCODED, _}, {:BY, _}] | rest3] ->
        {value, rest4} = parse_Value(rest3)
        {{:contentsconstraint, type, value}, rest4}

      _ ->
        {{:contentsconstraint, type, []}, rest2}
    end
  end

  defp parse_ContentsConstraint([[{:ENCODED, _}, {:BY, _}] | rest]) do
    {value, rest2} = parse_Value(rest)
    {{:contentsconstraint, [], value}, rest2}
  end

  defp parse_ContentsConstraint(tokens) do
    parse_error(tokens)
  end

  defp parse_Governor(tokens) do
    flist = [&parse_Type/1, &parse_DefinedObjectClass/1]
    parse_or(tokens, flist)
  end

  defp parse_ActualParameter(tokens) do
    flist = [
      &parse_Type/1,
      &parse_Value/1,
      &parse_ValueSet/1,
      &parse_DefinedObjectClass/1,
      &parse_Object/1,
      &parse_ObjectSet/1
    ]

    parse_or(tokens, flist)
  end

  defp parse_ParameterizedTypeAssignment([{:typereference, l1, name} | rest]) do
    {parameterList, rest2} = parse_ParameterList(rest)

    case rest2 do
      [{:"::=", _} | rest3] ->
        {type, rest4} = parse_Type(rest3)
        {r_ptypedef(pos: l1, name: name, args: parameterList, typespec: type), rest4}

      _ ->
        parse_error(rest2)
    end
  end

  defp parse_ParameterizedValueAssignment([r_identifier(pos: l1, val: name) | rest]) do
    {parameterList, rest2} = parse_ParameterList(rest)
    {type, rest3} = parse_Type(rest2)

    case rest3 do
      [{:"::=", _} | rest4] ->
        {value, rest5} = parse_Value(rest4)
        {r_pvaluedef(pos: l1, name: name, args: parameterList, type: type, value: value), rest5}

      _ ->
        parse_error(rest3)
    end
  end

  defp parse_ParameterizedValueSetTypeAssignment([{:typereference, l1, name} | rest]) do
    {parameterList, rest2} = parse_ParameterList(rest)
    {type, rest3} = parse_Type(rest2)

    case rest3 do
      [{:"::=", _} | rest4] ->
        {valueSet, rest5} = parse_ValueSet(rest4)

        {r_pvaluesetdef(pos: l1, name: name, args: parameterList, type: type, valueset: valueSet),
         rest5}

      _ ->
        parse_error(rest3)
    end
  end

  defp parse_ParameterizedObjectClassAssignment([{:typereference, l1, name} | rest]) do
    {parameterList, rest2} = parse_ParameterList(rest)

    case rest2 do
      [{:"::=", _} | rest3] ->
        {class, rest4} = parse_ObjectClass(rest3)
        {r_ptypedef(pos: l1, name: name, args: parameterList, typespec: class), rest4}

      _ ->
        parse_error(rest2)
    end
  end

  defp parse_ParameterizedObjectAssignment([r_identifier(pos: l1, val: name) | rest]) do
    {parameterList, rest2} = parse_ParameterList(rest)
    {class, rest3} = parse_DefinedObjectClass(rest2)

    case rest3 do
      [{:"::=", _} | rest4] ->
        {object, rest5} = parse_Object(rest4)
        {r_pobjectdef(pos: l1, name: name, args: parameterList, class: class, def: object), rest5}

      _ ->
        parse_error(rest3)
    end
  end

  defp parse_ParameterList([{:"{", _} | tokens]) do
    parse_ParameterList(tokens, [])
  end

  defp parse_ParameterList(tokens, acc) do
    {parameter, rest} = parse_Parameter(tokens)

    case rest do
      [{:",", _} | rest2] ->
        parse_ParameterList(rest2, [parameter | acc])

      [{:"}", _} | rest3] ->
        {:lists.reverse(acc, [parameter]), rest3}

      _ ->
        parse_error(rest)
    end
  end

  defp parse_Parameter(tokens) do
    flist = [&parse_ParamGovAndRef/1, &parse_Reference/1]
    parse_or(tokens, flist)
  end

  defp parse_ParamGovAndRef(tokens) do
    {paramGov, rest} = parse_ParamGovernor(tokens)

    case rest do
      [{:":", _} | rest2] ->
        {ref, rest3} = parse_Reference(rest2)
        {{paramGov, ref}, rest3}

      _ ->
        parse_error(rest)
    end
  end

  defp parse_ParamGovernor(tokens) do
    flist = [&parse_Governor/1, &parse_Reference/1]
    parse_or(tokens, flist)
  end

  defp parse_SimpleDefinedType([
         [{:typereference, l1, moduleName}, {:., _}, {:typereference, _, typeName}]
         | rest
       ]) do
    {r_Externaltypereference(pos: l1, module: moduleName, type: typeName), rest}
  end

  defp parse_SimpleDefinedType([tref = {:typereference, _, _} | rest]) do
    {tref2Exttref(tref), rest}
  end

  defp parse_SimpleDefinedType(tokens) do
    parse_error(tokens)
  end

  defp parse_SimpleDefinedValue([
         [{:typereference, l1, moduleName}, {:., _}, r_identifier(val: value)]
         | rest
       ]) do
    {{:simpledefinedvalue, r_Externalvaluereference(pos: l1, module: moduleName, value: value)},
     rest}
  end

  defp parse_SimpleDefinedValue([r_identifier() = id | rest]) do
    {{:simpledefinedvalue, identifier2Extvalueref(id)}, rest}
  end

  defp parse_SimpleDefinedValue(tokens) do
    parse_error(tokens)
  end

  defp parse_ParameterizedType(tokens) do
    {type, rest} = parse_SimpleDefinedType(tokens)
    {params, rest2} = parse_ActualParameterList(rest)
    {r_type(def: {:pt, type, params}), rest2}
  end

  defp parse_ParameterizedValue(tokens) do
    {value, rest} = parse_SimpleDefinedValue(tokens)
    {params, rest2} = parse_ActualParameterList(rest)
    {{:pv, value, params}, rest2}
  end

  defp parse_ParameterizedObjectSet(tokens) do
    {objectSet, rest} = parse_DefinedObjectSet(tokens)
    {params, rest2} = parse_ActualParameterList(rest)
    {{:pos, objectSet, params}, rest2}
  end

  defp parse_ActualParameterList([{:"{", _} | rest]) do
    parse_ActualParameterList(rest, [])
  end

  defp parse_ActualParameterList(tokens) do
    parse_error(tokens)
  end

  defp parse_ActualParameterList(tokens, acc) do
    {parameter, rest} = parse_ActualParameter(tokens)

    case rest do
      [{:",", _} | rest2] ->
        parse_ActualParameterList(rest2, [parameter | acc])

      [{:"}", _} | rest3] ->
        {:lists.reverse(acc, [parameter]), rest3}

      _ ->
        parse_error(rest)
    end
  end

  defp is_word(token) do
    list = :erlang.atom_to_list(token)

    case not_allowed_word(list) do
      true ->
        false

      false ->
        is_word_1(list)
    end
  end

  defp is_word_1([h | t]) do
    check_first(h) and check_rest(t)
  end

  defp not_allowed_word(name) do
    :lists.member(
      name,
      [
        'BIT',
        'BOOLEAN',
        'CHARACTER',
        'CHOICE',
        'EMBEDDED',
        'END',
        'ENUMERATED',
        'EXTERNAL',
        'FALSE',
        'INSTANCE',
        'INTEGER',
        'INTERSECTION',
        'MINUS-INFINITY',
        'NULL',
        'OBJECT',
        'OCTET',
        'PLUS-INFINITY',
        'REAL',
        'SEQUENCE',
        'SET',
        'TRUE',
        'UNION'
      ]
    )
  end

  defp check_first(c) do
    ?A <= c and c <= ?Z
  end

  defp check_rest([r | rs])
       when (?A <= r and r <= ?Z) or
              r === ?- do
    check_rest(rs)
  end

  defp check_rest([]) do
    true
  end

  defp check_rest(_) do
    false
  end

  defp parse_AlternativeTypeLists(tokens0) do
    {root, tokens1} = parse_AlternativeTypeList(tokens0)

    case tokens1 do
      [{:",", _} | tokens2] ->
        {extMarker, tokens3} = parse_ExtensionAndException(tokens2)
        {extAlts, tokens4} = parse_ExtensionAdditionAlternatives(tokens3)
        {_, tokens} = parse_OptionalExtensionMarker(tokens4, [])
        {root ++ extMarker ++ extAlts, tokens}

      tokens ->
        {root, tokens}
    end
  end

  defp parse_ExtensionAndException([{:..., l} | tokens0]) do
    {[r_EXTENSIONMARK(pos: l)],
     case tokens0 do
       [{:!, _} | tokens1] ->
         {_, tokens} = parse_ExceptionIdentification(tokens1)
         tokens

       _ ->
         tokens0
     end}
  end

  defp parse_AlternativeTypeList([r_identifier() | _] = tokens0) do
    {altType, tokens} = parse_NamedType(tokens0)
    parse_AlternativeTypeList_1(tokens, [altType])
  end

  defp parse_AlternativeTypeList(tokens) do
    parse_error(tokens)
  end

  defp parse_AlternativeTypeList_1([{:",", _} | [r_identifier() | _] = tokens0], acc) do
    {altType, tokens} = parse_NamedType(tokens0)
    parse_AlternativeTypeList_1(tokens, [altType | acc])
  end

  defp parse_AlternativeTypeList_1(tokens, acc) do
    {:lists.reverse(acc), tokens}
  end

  defp parse_ExtensionAdditionAlternatives([{:",", _} | _] = tokens0) do
    parse_ExtensionAdditionAlternativesList(tokens0, [])
  end

  defp parse_ExtensionAdditionAlternatives(tokens) do
    {[], tokens}
  end

  defp parse_ExtensionAdditionAlternativesList([{:",", _} | tokens1] = tokens0, acc) do
    try do
      parse_ExtensionAdditionAlternative(tokens1)
    catch
      {:asn1_error, _} ->
        {:lists.append(:lists.reverse(acc)), tokens0}
    else
      {extAddAlt, tokens2} ->
        parse_ExtensionAdditionAlternativesList(
          tokens2,
          [extAddAlt | acc]
        )
    end
  end

  defp parse_ExtensionAdditionAlternativesList(tokens, acc) do
    {:lists.append(:lists.reverse(acc)), tokens}
  end

  defp parse_ExtensionAdditionAlternative([r_identifier() | _] = tokens0) do
    {namedType, tokens} = parse_NamedType(tokens0)
    {[namedType], tokens}
  end

  defp parse_ExtensionAdditionAlternative([[{:"[", _}, {:"[", _}] | tokens0]) do
    tokens2 =
      case tokens0 do
        [[{:number, _, _}, {:":", _}] | tokens1] ->
          tokens1

        _ ->
          tokens0
      end

    {groupList, tokens3} = parse_AlternativeTypeList(tokens2)

    case tokens3 do
      [[{:"]", _}, {:"]", _}] | tokens] ->
        {groupList, tokens}

      _ ->
        parse_error(tokens3)
    end
  end

  defp parse_ExtensionAdditionAlternative(tokens) do
    parse_error(tokens)
  end

  defp parse_NamedType([r_identifier(pos: l1, val: idname) | rest]) do
    {type, rest2} = parse_Type(rest)
    {r_ComponentType(pos: l1, name: idname, typespec: type, prop: :mandatory), rest2}
  end

  defp parse_NamedType(tokens) do
    parse_error(tokens)
  end

  defp parse_ComponentTypeLists(tokens) do
    parse_ComponentTypeLists(tokens, [])
  end

  defp parse_ComponentTypeLists([r_identifier() | _Rest0] = tokens, clist) do
    {compList, rest1} = parse_ComponentTypeList(tokens, [])
    parse_ComponentTypeLists(rest1, clist ++ compList)
  end

  defp parse_ComponentTypeLists(
         [[{:COMPONENTS, _}, {:OF, _}] | _] = tokens,
         clist
       ) do
    {compList, rest1} = parse_ComponentTypeList(tokens, [])
    parse_ComponentTypeLists(rest1, clist ++ compList)
  end

  defp parse_ComponentTypeLists([[{:",", l1}, {:..., _}, {:!, _}] | rest02], clist0)
       when clist0 !== [] do
    {_, rest03} = parse_ExceptionIdentification(rest02)

    parse_ComponentTypeLists2(
      rest03,
      clist0 ++ [r_EXTENSIONMARK(pos: l1)]
    )
  end

  defp parse_ComponentTypeLists([[{:",", _}, {:..., l1}] | rest02], clist0)
       when clist0 !== [] do
    parse_ComponentTypeLists2(
      rest02,
      clist0 ++ [r_EXTENSIONMARK(pos: l1)]
    )
  end

  defp parse_ComponentTypeLists([{:..., l1} | rest02], clist0) do
    parse_ComponentTypeLists2(
      rest02,
      clist0 ++ [r_EXTENSIONMARK(pos: l1)]
    )
  end

  defp parse_ComponentTypeLists(tokens = [{:"}", _L1} | _Rest02], clist0) do
    {clist0, tokens}
  end

  defp parse_ComponentTypeLists(tokens, _) do
    parse_error(tokens)
  end

  defp parse_ComponentTypeLists2(tokens, clist) do
    {extAdd, rest} = parse_ExtensionAdditions(tokens, clist)

    {clist2, rest2} =
      parse_OptionalExtensionMarker(
        rest,
        :lists.flatten(extAdd)
      )

    case rest2 do
      [{:",", _} | rest3] ->
        {compList, rest4} = parse_ComponentTypeList(rest3, [])
        {clist2 ++ compList, rest4}

      _ ->
        {clist2, rest2}
    end
  end

  defp parse_OptionalExtensionMarker([[{:",", _}, {:..., l1}] | rest], clist) do
    {clist ++ [r_EXTENSIONMARK(pos: l1)], rest}
  end

  defp parse_OptionalExtensionMarker(tokens, clist) do
    {clist, tokens}
  end

  defp parse_ComponentTypeList([{:",", _} | [r_identifier() | _] = tokens0], acc)
       when acc !== [] do
    {componentType, tokens} = parse_ComponentType(tokens0)
    parse_ComponentTypeList(tokens, [componentType | acc])
  end

  defp parse_ComponentTypeList(
         [
           {:",", _}
           | [
               [{:COMPONENTS, _}, {:OF, _}]
               | _
             ] = tokens0
         ],
         acc
       )
       when acc !== [] do
    {componentType, tokens} = parse_ComponentType(tokens0)
    parse_ComponentTypeList(tokens, [componentType | acc])
  end

  defp parse_ComponentTypeList(tokens = [{:"}", _} | _], acc) do
    {:lists.reverse(acc), tokens}
  end

  defp parse_ComponentTypeList(tokens = [[{:"]", _}, {:"]", _}] | _], acc) do
    {:lists.reverse(acc), tokens}
  end

  defp parse_ComponentTypeList(tokens = [[{:",", _}, {:..., _}] | _], acc) do
    {:lists.reverse(acc), tokens}
  end

  defp parse_ComponentTypeList(tokens, []) do
    {componentType, rest} = parse_ComponentType(tokens)
    parse_ComponentTypeList(rest, [componentType])
  end

  defp parse_ComponentTypeList(tokens, _) do
    parse_error(tokens)
  end

  defp parse_ExtensionAdditions(tokens = [{:",", _} | _], clist) do
    {extAddList, rest2} = parse_ExtensionAdditionList(tokens, [])
    {clist ++ extAddList, rest2}
  end

  defp parse_ExtensionAdditions(tokens, clist) do
    {clist, tokens}
  end

  defp parse_ExtensionAdditionList([{:",", _} | [r_identifier() | _] = tokens0], acc) do
    {componentType, tokens} = parse_ComponentType(tokens0)

    parse_ExtensionAdditionList(
      tokens,
      [componentType | acc]
    )
  end

  defp parse_ExtensionAdditionList(
         [
           {:",", _}
           | [
               [{:COMPONENTS, _}, {:OF, _}]
               | _
             ] = tokens0
         ],
         acc
       ) do
    {componentType, tokens} = parse_ComponentType(tokens0)

    parse_ExtensionAdditionList(
      tokens,
      [componentType | acc]
    )
  end

  defp parse_ExtensionAdditionList([[{:",", _}, {:"[", _}, {:"[", _}] | tokens], acc) do
    {extAddGroup, rest2} = parse_ExtensionAdditionGroup(tokens)
    parse_ExtensionAdditionList(rest2, [extAddGroup | acc])
  end

  defp parse_ExtensionAdditionList([{:"}", _} | _] = tokens, acc) do
    {:lists.reverse(acc), tokens}
  end

  defp parse_ExtensionAdditionList([[{:",", _}, {:..., _}] | _] = tokens, acc) do
    {:lists.reverse(acc), tokens}
  end

  defp parse_ExtensionAdditionList(tokens, _) do
    parse_error(tokens)
  end

  defp parse_ExtensionAdditionGroup([[{:number, _, num}, {:":", _}] | tokens]) do
    parse_ExtensionAdditionGroup2(tokens, num)
  end

  defp parse_ExtensionAdditionGroup(tokens) do
    parse_ExtensionAdditionGroup2(tokens, :undefined)
  end

  defp parse_ExtensionAdditionGroup2(tokens, num) do
    {compTypeList, rest} =
      parse_ComponentTypeList(
        tokens,
        []
      )

    case rest do
      [[{:"]", _}, {:"]", _}] | rest2] ->
        {[
           {:ExtensionAdditionGroup, num}
           | compTypeList
         ] ++ [:ExtensionAdditionGroupEnd], rest2}

      _ ->
        parse_error(rest)
    end
  end

  defp parse_ComponentType([[{:COMPONENTS, _}, {:OF, _}] | rest]) do
    {type, rest2} = parse_Type(rest)
    {{:"COMPONENTS OF", type}, rest2}
  end

  defp parse_ComponentType(tokens) do
    result = {namedType, rest} = parse_NamedType(tokens)

    case rest do
      [{:OPTIONAL, _} | rest2] ->
        {r_ComponentType(namedType, prop: :OPTIONAL), rest2}

      [{:DEFAULT, _} | rest2] ->
        {value, rest21} = parse_Value(rest2)
        {r_ComponentType(namedType, prop: {:DEFAULT, value}), rest21}

      _ ->
        result
    end
  end

  defp parse_Enumerations(tokens0) do
    {root, tokens1} = parse_Enumeration(tokens0)

    case tokens1 do
      [[{:",", _}, {:..., _}, {:",", _}] | tokens2] ->
        {ext, tokens} = parse_Enumeration(tokens2)
        {root ++ [:EXTENSIONMARK | ext], tokens}

      [[{:",", _}, {:..., _}] | tokens] ->
        {root ++ [:EXTENSIONMARK], tokens}

      _ ->
        case :erlang.get(:extensiondefault) do
          :IMPLIED ->
            {root ++ [:EXTENSIONMARK], tokens1}

          _ ->
            {root, tokens1}
        end
    end
  end

  defp parse_Enumeration(tokens0) do
    {item, tokens} = parse_EnumerationItem(tokens0)
    parse_Enumeration_1(tokens, [item])
  end

  defp parse_Enumeration_1([{:",", _} | tokens1] = tokens0, acc) do
    try do
      parse_EnumerationItem(tokens1)
    catch
      {:asn1_error, _} ->
        {:lists.reverse(acc), tokens0}
    else
      {item, tokens} ->
        parse_Enumeration_1(tokens, [item | acc])
    end
  end

  defp parse_Enumeration_1(tokens, acc) do
    {:lists.reverse(acc), tokens}
  end

  defp parse_EnumerationItem([[r_identifier(), {:"(", _}] | _] = tokens) do
    parse_NamedNumber(tokens)
  end

  defp parse_EnumerationItem([r_identifier(val: id) | tokens]) do
    {id, tokens}
  end

  defp parse_EnumerationItem(tokens) do
    parse_error(tokens)
  end

  defp parse_NamedNumberList(tokens) do
    parse_NamedNumberList(tokens, [])
  end

  defp parse_NamedNumberList(tokens, acc) do
    {namedNum, rest} = parse_NamedNumber(tokens)

    case rest do
      [{:",", _} | rest2] ->
        parse_NamedNumberList(rest2, [namedNum | acc])

      _ ->
        {:lists.reverse(acc, [namedNum]), rest}
    end
  end

  defp parse_NamedNumber([[r_identifier(val: name), {:"(", _}] | rest]) do
    flist = [&parse_SignedNumber/1, &parse_DefinedValue/1]

    case parse_or(rest, flist) do
      {namedNum, [{:")", _} | rest2]} ->
        {{:NamedNumber, name, namedNum}, rest2}

      _ ->
        parse_error(rest)
    end
  end

  defp parse_NamedNumber(tokens) do
    parse_error(tokens)
  end

  defp parse_SignedNumber([{:number, _, value} | rest]) do
    {value, rest}
  end

  defp parse_SignedNumber(tokens) do
    parse_error(tokens)
  end

  defp parse_Tag([{:"[", _} | rest]) do
    {class, rest2} = parse_Class(rest)

    {classNumber, rest3} =
      case rest2 do
        [{:number, _, num} | rest21] ->
          {num, rest21}

        _ ->
          parse_DefinedValue(rest2)
      end

    case rest3 do
      [{:"]", _} | rest4] ->
        {r_tag(class: class, number: classNumber), rest4}

      _ ->
        parse_error(rest3)
    end
  end

  defp parse_Class([{:UNIVERSAL, _} | rest]) do
    {:UNIVERSAL, rest}
  end

  defp parse_Class([{:APPLICATION, _} | rest]) do
    {:APPLICATION, rest}
  end

  defp parse_Class([{:PRIVATE, _} | rest]) do
    {:PRIVATE, rest}
  end

  defp parse_Class(tokens) do
    {:CONTEXT, tokens}
  end

  defp parse_Value(tokens) do
    flist = [&parse_BuiltinValue/1, &parse_ValueFromObject/1, &parse_DefinedValue/1]
    parse_or(tokens, flist)
  end

  defp parse_BuiltinValue([{:bstring, _, bstr} | rest]) do
    {{:bstring, bstr}, rest}
  end

  defp parse_BuiltinValue([{:hstring, _, hstr} | rest]) do
    {{:hstring, hstr}, rest}
  end

  defp parse_BuiltinValue([[{:"{", _}, {:"}", _}] | rest]) do
    {[], rest}
  end

  defp parse_BuiltinValue(tokens = [{:"{", _} | _Rest]) do
    flist = [&parse_SequenceOfValue/1, &parse_SequenceValue/1, &parse_ObjectIdentifierValue/1]
    parse_or(tokens, flist)
  end

  defp parse_BuiltinValue([[r_identifier(val: idName), {:":", _}] | rest]) do
    {value, rest2} = parse_Value(rest)
    {{:CHOICE, {idName, value}}, rest2}
  end

  defp parse_BuiltinValue([[{:NULL, _}, {:":", _}] | _] = tokens) do
    parse_ObjectClassFieldValue(tokens)
  end

  defp parse_BuiltinValue([{:NULL, _} | rest]) do
    {:NULL, rest}
  end

  defp parse_BuiltinValue([{:TRUE, _} | rest]) do
    {true, rest}
  end

  defp parse_BuiltinValue([{:FALSE, _} | rest]) do
    {false, rest}
  end

  defp parse_BuiltinValue([{:"PLUS-INFINITY", _} | rest]) do
    {:"PLUS-INFINITY", rest}
  end

  defp parse_BuiltinValue([{:"MINUS-INFINITY", _} | rest]) do
    {:"MINUS-INFINITY", rest}
  end

  defp parse_BuiltinValue([{:cstring, _, cstr} | rest]) do
    {cstr, rest}
  end

  defp parse_BuiltinValue([{:number, _, num} | rest]) do
    {num, rest}
  end

  defp parse_BuiltinValue(tokens) do
    parse_ObjectClassFieldValue(tokens)
  end

  defp parse_DefinedValue(tokens) do
    flist = [&parse_ParameterizedValue/1, &parse_DefinedValue2/1]
    parse_or(tokens, flist)
  end

  defp parse_DefinedValue2([
         [{:typereference, l1, tname}, {:., _}, r_identifier(val: idname)]
         | rest
       ]) do
    {r_Externalvaluereference(pos: l1, module: tname, value: idname), rest}
  end

  defp parse_DefinedValue2([r_identifier() = id | rest]) do
    {identifier2Extvalueref(id), rest}
  end

  defp parse_DefinedValue2(tokens) do
    parse_error(tokens)
  end

  defp parse_SequenceValue([{:"{", _} | tokens]) do
    parse_SequenceValue(tokens, [])
  end

  defp parse_SequenceValue([r_identifier(pos: pos, val: idName) | rest], acc) do
    {value, rest2} = parse_Value(rest)
    seqTag = r_seqtag(pos: pos, module: :erlang.get(:asn1_module), val: idName)

    case rest2 do
      [{:",", _} | rest3] ->
        parse_SequenceValue(rest3, [{seqTag, value} | acc])

      [{:"}", _} | rest3] ->
        {:lists.reverse(acc, [{seqTag, value}]), rest3}

      _ ->
        parse_error(rest2)
    end
  end

  defp parse_SequenceValue(tokens, _Acc) do
    parse_error(tokens)
  end

  defp parse_SequenceOfValue([{:"{", _} | tokens]) do
    parse_SequenceOfValue(tokens, [])
  end

  defp parse_SequenceOfValue(tokens, acc) do
    {value, rest2} = parse_Value(tokens)

    case rest2 do
      [{:",", _} | rest3] ->
        parse_SequenceOfValue(rest3, [value | acc])

      [{:"}", _} | rest3] ->
        {:lists.reverse(acc, [value]), rest3}

      _ ->
        parse_error(rest2)
    end
  end

  defp parse_ValueSetTypeAssignment([{:typereference, l1, name} | rest]) do
    {type, rest2} = parse_Type(rest)

    case rest2 do
      [{:"::=", _} | rest3] ->
        {valueSet, rest4} = parse_ValueSet(rest3)

        {r_valuedef(
           pos: l1,
           name: name,
           type: type,
           value: valueSet,
           module: :erlang.get(:asn1_module)
         ), rest4}

      _ ->
        parse_error(rest2)
    end
  end

  defp parse_ValueSet([{:"{", _} | rest]) do
    {elems, rest2} = parse_ElementSetSpecs(rest)

    case rest2 do
      [{:"}", _} | rest3] ->
        {{:valueset, elems}, rest3}

      _ ->
        parse_error(rest2)
    end
  end

  defp parse_ValueSet(tokens) do
    parse_error(tokens)
  end

  defp parse_ValueAssignment([r_identifier(pos: l1, val: idName) | rest]) do
    {type, rest2} = parse_Type(rest)

    case rest2 do
      [{:"::=", _} | rest3] ->
        {value, rest4} = parse_Value(rest3)

        {r_valuedef(
           pos: l1,
           name: idName,
           type: type,
           value: value,
           module: :erlang.get(:asn1_module)
         ), rest4}

      _ ->
        parse_error(rest2)
    end
  end

  defp parse_SubtypeElements([{:SIZE, _} | tokens]) do
    {constraint, rest} = parse_Constraint(tokens)
    {{:SizeConstraint, r_constraint(constraint, :c)}, rest}
  end

  defp parse_SubtypeElements([{:FROM, _} | tokens]) do
    {constraint, rest} = parse_Constraint(tokens)
    {{:PermittedAlphabet, r_constraint(constraint, :c)}, rest}
  end

  defp parse_SubtypeElements([[{:WITH, _}, {:COMPONENT, _}] | tokens]) do
    {constraint, rest} = parse_Constraint(tokens)
    {{:"WITH COMPONENT", constraint}, rest}
  end

  defp parse_SubtypeElements([
         [{:WITH, _}, {:COMPONENTS, _}, {:"{", _}, {:..., _}, {:",", _}]
         | tokens
       ]) do
    {constraint, rest} = parse_TypeConstraints(tokens)

    case rest do
      [{:"}", _} | rest2] ->
        {{:"WITH COMPONENTS", {:PartialSpecification, constraint}}, rest2}

      _ ->
        parse_error(rest)
    end
  end

  defp parse_SubtypeElements([
         [{:WITH, _}, {:COMPONENTS, _}, {:"{", _}]
         | tokens
       ]) do
    {constraint, rest} = parse_TypeConstraints(tokens)

    case rest do
      [{:"}", _} | rest2] ->
        {{:"WITH COMPONENTS", {:FullSpecification, constraint}}, rest2}

      _ ->
        parse_error(rest)
    end
  end

  defp parse_SubtypeElements([{:PATTERN, _} | tokens]) do
    {value, rest} = parse_Value(tokens)
    {{:pattern, value}, rest}
  end

  defp parse_SubtypeElements(tokens) do
    flist = [&parse_ContainedSubtype/1, &parse_Value/1, &parse_MIN/1, &parse_Type/1]

    case parse_or(tokens, flist) do
      {r_type(), _} = result ->
        result

      {lower, [{:.., _} | rest]} ->
        {upper, rest2} = parse_UpperEndpoint(rest)
        {{:ValueRange, {lower, upper}}, rest2}

      {lower, [[{:<, _}, {:.., _}] | rest]} ->
        {upper, rest2} = parse_UpperEndpoint(rest)
        {{:ValueRange, {{:gt, lower}, upper}}, rest2}

      {res = {:ContainedSubtype, _Type}, rest} ->
        {res, rest}

      {value, rest} ->
        {{:SingleValue, value}, rest}
    end
  end

  defp parse_ContainedSubtype([{:INCLUDES, _} | rest]) do
    {type, rest2} = parse_Type(rest)
    {{:ContainedSubtype, type}, rest2}
  end

  defp parse_ContainedSubtype(tokens) do
    parse_error(tokens)
  end

  defp parse_UpperEndpoint([{:<, _} | rest]) do
    parse_UpperEndpoint(:lt, rest)
  end

  defp parse_UpperEndpoint(tokens) do
    parse_UpperEndpoint(false, tokens)
  end

  defp parse_UpperEndpoint(lt, tokens) do
    flist = [&parse_MAX/1, &parse_Value/1]

    case parse_or(tokens, flist) do
      {value, rest2} when lt === :lt ->
        {{:lt, value}, rest2}

      {value, rest2} ->
        {value, rest2}
    end
  end

  defp parse_MIN([{:MIN, _} | t]) do
    {:MIN, t}
  end

  defp parse_MIN(tokens) do
    parse_error(tokens)
  end

  defp parse_MAX([{:MAX, _} | t]) do
    {:MAX, t}
  end

  defp parse_MAX(tokens) do
    parse_error(tokens)
  end

  defp parse_TypeConstraints(tokens) do
    parse_TypeConstraints(tokens, [])
  end

  defp parse_TypeConstraints([r_identifier() | rest], acc) do
    {componentConstraint, rest2} = parse_ComponentConstraint(rest)

    case rest2 do
      [{:",", _} | rest3] ->
        parse_TypeConstraints(
          rest3,
          [componentConstraint | acc]
        )

      _ ->
        {:lists.reverse(acc, [componentConstraint]), rest2}
    end
  end

  defp parse_TypeConstraints(tokens, _) do
    parse_error(tokens)
  end

  defp parse_ComponentConstraint(tokens = [{:"(", _} | _Rest]) do
    {valueConstraint, rest2} = parse_Constraint(tokens)
    {presenceConstraint, rest3} = parse_PresenceConstraint(rest2)
    {{valueConstraint, presenceConstraint}, rest3}
  end

  defp parse_ComponentConstraint(tokens) do
    {presenceConstraint, rest} = parse_PresenceConstraint(tokens)
    {{:asn1_empty, presenceConstraint}, rest}
  end

  defp parse_PresenceConstraint([{:PRESENT, _} | rest]) do
    {:PRESENT, rest}
  end

  defp parse_PresenceConstraint([{:ABSENT, _} | rest]) do
    {:ABSENT, rest}
  end

  defp parse_PresenceConstraint([{:OPTIONAL, _} | rest]) do
    {:OPTIONAL, rest}
  end

  defp parse_PresenceConstraint(tokens) do
    {:asn1_empty, tokens}
  end

  defp merge_constraints(clist) do
    merge_constraints(clist, [], [])
  end

  defp merge_constraints([r_constraint(c: c, e: e) | t], cacc0, eacc0) do
    eacc =
      case e do
        :undefined ->
          eacc0

        ^e ->
          [e | eacc0]
      end

    cacc = [c | cacc0]
    merge_constraints(t, cacc, eacc)
  end

  defp merge_constraints([], cacc, []) do
    :lists.reverse(cacc)
  end

  defp merge_constraints([], cacc, eacc) do
    :lists.reverse(cacc) ++ [{:element_set, {:Errors, eacc}, :none}]
  end

  defp get_line({token, pos, _})
       when is_integer(pos) and
              is_atom(token) do
    pos
  end

  defp get_line({token, pos})
       when is_integer(pos) and
              is_atom(token) do
    pos
  end

  defp get_token({:valuefieldreference, _, fieldName}) do
    :erlang.list_to_atom([
      ?&
      | :erlang.atom_to_list(fieldName)
    ])
  end

  defp get_token({:typefieldreference, _, fieldName}) do
    :erlang.list_to_atom([
      ?&
      | :erlang.atom_to_list(fieldName)
    ])
  end

  defp get_token({token, pos, value})
       when is_integer(pos) and
              is_atom(token) do
    value
  end

  defp get_token({:"$end", pos}) when is_integer(pos) do
    :"END-OF-FILE"
  end

  defp get_token({token, pos})
       when is_integer(pos) and
              is_atom(token) do
    token
  end

  defp tref2Exttref(r_typereference(pos: pos, val: name)) do
    r_Externaltypereference(pos: pos, module: resolve_module(name), type: name)
  end

  defp tref2Exttref(pos, name) do
    r_Externaltypereference(pos: pos, module: resolve_module(name), type: name)
  end

  defp identifier2Extvalueref(r_identifier(pos: pos, val: name)) do
    r_Externalvaluereference(pos: pos, module: resolve_module(name), value: name)
  end

  defp parse_error(tokens) do
    throw({:asn1_error, {:parse_error, tokens}})
  end
end
