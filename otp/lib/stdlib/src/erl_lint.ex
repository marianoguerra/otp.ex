defmodule :m_erl_lint do
  use Bitwise

  import :lists,
    only: [all: 2, any: 2, foldl: 3, foldr: 3, map: 2, mapfoldl: 3, member: 2, reverse: 1]

  def bool_option(on, off, default, opts) do
    foldl(
      fn
        opt, _Def when opt === on ->
          true

        opt, _Def when opt === off ->
          false

        _Opt, def__ ->
          def__
      end,
      default,
      opts
    )
  end

  def value_option(flag, default, opts) do
    foldl(
      fn
        {opt, val}, _Def when opt === flag ->
          val

        _Opt, def__ ->
          def__
      end,
      default,
      opts
    )
  end

  def value_option(flag, default, on, onVal, off, offVal, opts) do
    foldl(
      fn
        {opt, val}, _Def when opt === flag ->
          val

        opt, _Def when opt === on ->
          onVal

        opt, _Def when opt === off ->
          offVal

        _Opt, def__ ->
          def__
      end,
      default,
      opts
    )
  end

  require Record

  Record.defrecord(:r_bittype, :bittype,
    type: :undefined,
    unit: :undefined,
    sign: :undefined,
    endian: :undefined
  )

  Record.defrecord(:r_typeinfo, :typeinfo,
    attr: :undefined,
    line: :undefined
  )

  Record.defrecord(:r_usage, :usage,
    calls: :maps.new(),
    imported: [],
    used_records: :gb_sets.new(),
    used_types: :maps.new()
  )

  Record.defrecord(:r_lint, :lint,
    state: :start,
    module: :"",
    behaviour: [],
    exports: :gb_sets.empty(),
    imports: [],
    compile: [],
    records: :maps.new(),
    locals: :gb_sets.empty(),
    no_auto: :gb_sets.empty(),
    defined: :gb_sets.empty(),
    on_load: [],
    on_load_line: :erl_anno.new(0),
    clashes: [],
    not_deprecated: [],
    not_removed: :gb_sets.empty(),
    func: [],
    warn_format: 0,
    enabled_warnings: [],
    nowarn_bif_clash: [],
    errors: [],
    warnings: [],
    file: '',
    recdef_top: false,
    xqlc: false,
    called: [],
    usage: :EFE_TODO_NESTED_RECORD,
    specs: :maps.new(),
    callbacks: :maps.new(),
    optional_callbacks: :maps.new(),
    types: :maps.new(),
    exp_types: :gb_sets.empty(),
    in_try_head: false,
    bvt: :none,
    gexpr_context: :guard
  )

  def format_error(:undefined_module) do
    'no module definition'
  end

  def format_error(:redefine_module) do
    'redefining module'
  end

  def format_error(:pmod_unsupported) do
    'parameterized modules are no longer supported'
  end

  def format_error(:non_latin1_module_unsupported) do
    'module names with non-latin1 characters are not supported'
  end

  def format_error(:invalid_call) do
    'invalid function call'
  end

  def format_error(:invalid_record) do
    'invalid record expression'
  end

  def format_error({:attribute, a}) do
    :io_lib.format('attribute ~tw after function definitions', [a])
  end

  def format_error({:missing_qlc_hrl, a}) do
    :io_lib.format('qlc:q/~w called, but "qlc.hrl" not included', [a])
  end

  def format_error({:redefine_import, {{f, a}, m}}) do
    :io_lib.format('function ~tw/~w already imported from ~w', [f, a, m])
  end

  def format_error({:bad_inline, {f, a}}) do
    :io_lib.format('inlined function ~tw/~w undefined', [f, a])
  end

  def format_error({:invalid_deprecated, d}) do
    :io_lib.format('badly formed deprecated attribute ~tw', [d])
  end

  def format_error({:bad_deprecated, {f, a}}) do
    :io_lib.format('deprecated function ~tw/~w undefined or not exported', [f, a])
  end

  def format_error({:invalid_removed, d}) do
    :io_lib.format('badly formed removed attribute ~tw', [d])
  end

  def format_error({:bad_removed, {f, a}})
      when f === :_ or
             a === :_ do
    :io_lib.format('at least one function matching ~tw/~w is still exported', [f, a])
  end

  def format_error({:bad_removed, {f, a}}) do
    :io_lib.format('removed function ~tw/~w is still exported', [f, a])
  end

  def format_error({:bad_nowarn_unused_function, {f, a}}) do
    :io_lib.format('function ~tw/~w undefined', [f, a])
  end

  def format_error({:bad_nowarn_bif_clash, {f, a}}) do
    :io_lib.format('function ~tw/~w undefined', [f, a])
  end

  def format_error(:disallowed_nowarn_bif_clash) do
    :io_lib.format(
      'compile directive nowarn_bif_clash is no longer allowed,~n - use explicit module names or -compile({no_auto_import, [F/A]})',
      []
    )
  end

  def format_error({:bad_on_load, term}) do
    :io_lib.format('badly formed on_load attribute: ~tw', [term])
  end

  def format_error(:multiple_on_loads) do
    'more than one on_load attribute'
  end

  def format_error({:bad_on_load_arity, {f, a}}) do
    :io_lib.format('function ~tw/~w has wrong arity (must be 0)', [f, a])
  end

  def format_error({:undefined_on_load, {f, a}}) do
    :io_lib.format('function ~tw/~w undefined', [f, a])
  end

  def format_error(:nif_inline) do
    'inlining is enabled - local calls to NIFs may call their Erlang implementation instead'
  end

  def format_error(:export_all) do
    'export_all flag enabled - all functions will be exported'
  end

  def format_error({:duplicated_export, {f, a}}) do
    :io_lib.format('function ~tw/~w already exported', [f, a])
  end

  def format_error({:unused_import, {{f, a}, m}}) do
    :io_lib.format('import ~w:~tw/~w is unused', [m, f, a])
  end

  def format_error({:undefined_function, {f, a}}) do
    :io_lib.format('function ~tw/~w undefined', [f, a])
  end

  def format_error({:redefine_function, {f, a}}) do
    :io_lib.format('function ~tw/~w already defined', [f, a])
  end

  def format_error({:define_import, {f, a}}) do
    :io_lib.format('defining imported function ~tw/~w', [f, a])
  end

  def format_error({:unused_function, {f, a}}) do
    :io_lib.format('function ~tw/~w is unused', [f, a])
  end

  def format_error({:call_to_redefined_bif, {f, a}}) do
    :io_lib.format(
      'ambiguous call of overridden auto-imported BIF ~w/~w~n - use erlang:~w/~w or "-compile({no_auto_import,[~w/~w]})." to resolve name clash',
      [f, a, f, a, f, a]
    )
  end

  def format_error({:call_to_redefined_old_bif, {f, a}}) do
    :io_lib.format(
      'ambiguous call of overridden pre R14 auto-imported BIF ~w/~w~n - use erlang:~w/~w or "-compile({no_auto_import,[~w/~w]})." to resolve name clash',
      [f, a, f, a, f, a]
    )
  end

  def format_error({:redefine_old_bif_import, {f, a}}) do
    :io_lib.format(
      'import directive overrides pre R14 auto-imported BIF ~w/~w~n - use "-compile({no_auto_import,[~w/~w]})." to resolve name clash',
      [f, a, f, a]
    )
  end

  def format_error({:redefine_bif_import, {f, a}}) do
    :io_lib.format(
      'import directive overrides auto-imported BIF ~w/~w~n - use "-compile({no_auto_import,[~w/~w]})." to resolve name clash',
      [f, a, f, a]
    )
  end

  def format_error({:deprecated, mFA, string, rel}) do
    :io_lib.format('~s is deprecated and will be removed in ~s; ~s', [
      format_mfa(mFA),
      rel,
      string
    ])
  end

  def format_error({:deprecated, mFA, string})
      when is_list(string) do
    :io_lib.format('~s is deprecated; ~s', [format_mfa(mFA), string])
  end

  def format_error({:deprecated_type, {m1, f1, a1}, string})
      when is_list(string) do
    :io_lib.format('the type ~p:~p~s is deprecated; ~s', [m1, f1, gen_type_paren(a1), string])
  end

  def format_error({:removed, mFA, replacementMFA, rel}) do
    :io_lib.format(
      'call to ~s will fail, since it was removed in ~s; use ~s',
      [format_mfa(mFA), rel, format_mfa(replacementMFA)]
    )
  end

  def format_error({:removed, mFA, string}) when is_list(string) do
    :io_lib.format('~s is removed; ~s', [format_mfa(mFA), string])
  end

  def format_error({:removed_type, mNA, string}) do
    :io_lib.format('the type ~s is removed; ~s', [format_mna(mNA), string])
  end

  def format_error({:obsolete_guard, {f, a}}) do
    :io_lib.format('~p/~p obsolete (use is_~p/~p)', [f, a, f, a])
  end

  def format_error({:obsolete_guard_overridden, test}) do
    :io_lib.format(
      'obsolete ~s/1 (meaning is_~s/1) is illegal when there is a local/imported function named is_~p/1 ',
      [test, test, test]
    )
  end

  def format_error({:too_many_arguments, arity}) do
    :io_lib.format('too many arguments (~w) - maximum allowed is ~w', [arity, 255])
  end

  def format_error(:illegal_pattern) do
    'illegal pattern'
  end

  def format_error(:illegal_map_key) do
    'illegal map key in pattern'
  end

  def format_error(:illegal_bin_pattern) do
    'binary patterns cannot be matched in parallel using \'=\''
  end

  def format_error(:illegal_expr) do
    'illegal expression'
  end

  def format_error({:illegal_guard_local_call, {f, a}}) do
    :io_lib.format('call to local/imported function ~tw/~w is illegal in guard', [f, a])
  end

  def format_error(:illegal_guard_expr) do
    'illegal guard expression'
  end

  def format_error(:illegal_map_construction) do
    'only association operators \'=>\' are allowed in map construction'
  end

  def format_error({:undefined_record, t}) do
    :io_lib.format('record ~tw undefined', [t])
  end

  def format_error({:redefine_record, t}) do
    :io_lib.format('record ~tw already defined', [t])
  end

  def format_error({:redefine_field, t, f}) do
    :io_lib.format('field ~tw already defined in record ~tw', [f, t])
  end

  def format_error(:bad_multi_field_init) do
    :io_lib.format('\'_\' initializes no omitted fields', [])
  end

  def format_error({:undefined_field, t, f}) do
    :io_lib.format('field ~tw undefined in record ~tw', [f, t])
  end

  def format_error(:illegal_record_info) do
    'illegal record info'
  end

  def format_error({:field_name_is_variable, t, f}) do
    :io_lib.format('field ~tw is not an atom or _ in record ~tw', [f, t])
  end

  def format_error({:wildcard_in_update, t}) do
    :io_lib.format('meaningless use of _ in update of record ~tw', [t])
  end

  def format_error({:unused_record, t}) do
    :io_lib.format('record ~tw is unused', [t])
  end

  def format_error({:untyped_record, t}) do
    :io_lib.format('record ~tw has field(s) without type information', [t])
  end

  def format_error({:unbound_var, v}) do
    :io_lib.format('variable ~w is unbound', [v])
  end

  def format_error({:unsafe_var, v, {what, where}}) do
    :io_lib.format('variable ~w unsafe in ~w ~s', [v, what, format_where(where)])
  end

  def format_error({:exported_var, v, {what, where}}) do
    :io_lib.format('variable ~w exported from ~w ~s', [v, what, format_where(where)])
  end

  def format_error({:shadowed_var, v, in__}) do
    :io_lib.format('variable ~w shadowed in ~w', [v, in__])
  end

  def format_error({:unused_var, v}) do
    :io_lib.format('variable ~w is unused', [v])
  end

  def format_error({:variable_in_record_def, v}) do
    :io_lib.format('variable ~w in record definition', [v])
  end

  def format_error({:stacktrace_guard, v}) do
    :io_lib.format('stacktrace variable ~w must not be used in a guard', [v])
  end

  def format_error({:stacktrace_bound, v}) do
    :io_lib.format('stacktrace variable ~w must not be previously bound', [v])
  end

  def format_error({:undefined_bittype, type}) do
    :io_lib.format('bit type ~tw undefined', [type])
  end

  def format_error({:bittype_mismatch, val1, val2, what}) do
    :io_lib.format('conflict in ~s specification for bit field: \'~p\' and \'~p\'', [
      what,
      val1,
      val2
    ])
  end

  def format_error(:bittype_unit) do
    'a bit unit size must not be specified unless a size is specified too'
  end

  def format_error(:illegal_bitsize) do
    'illegal bit size'
  end

  def format_error({:illegal_bitsize_local_call, {f, a}}) do
    :io_lib.format(
      'call to local/imported function ~tw/~w is illegal in a size expression for a binary segment',
      [f, a]
    )
  end

  def format_error(:non_integer_bitsize) do
    'a size expression in a pattern evaluates to a non-integer value; this pattern cannot possibly match'
  end

  def format_error(:unsized_binary_not_at_end) do
    'a binary field without size is only allowed at the end of a binary pattern'
  end

  def format_error(:typed_literal_string) do
    'a literal string in a binary pattern must not have a type or a size'
  end

  def format_error(:utf_bittype_size_or_unit) do
    'neither size nor unit must be given for segments of type utf8/utf16/utf32'
  end

  def format_error({:bad_bitsize, type}) do
    :io_lib.format('bad ~s bit size', [type])
  end

  def format_error(:unsized_binary_in_bin_gen_pattern) do
    'binary fields without size are not allowed in patterns of bit string generators'
  end

  def format_error({:conflicting_behaviours, {name, arity}, b, firstL, firstB}) do
    :io_lib.format(
      'conflicting behaviours - callback ~tw/~w required by both \'~p\' and \'~p\' ~s',
      [name, arity, b, firstB, format_where(firstL)]
    )
  end

  def format_error({:undefined_behaviour_func, {func, arity}, behaviour}) do
    :io_lib.format('undefined callback function ~tw/~w (behaviour \'~w\')', [
      func,
      arity,
      behaviour
    ])
  end

  def format_error({:undefined_behaviour, behaviour}) do
    :io_lib.format('behaviour ~tw undefined', [behaviour])
  end

  def format_error({:undefined_behaviour_callbacks, behaviour}) do
    :io_lib.format('behaviour ~w callback functions are undefined', [behaviour])
  end

  def format_error({:ill_defined_behaviour_callbacks, behaviour}) do
    :io_lib.format('behaviour ~w callback functions erroneously defined', [behaviour])
  end

  def format_error({:ill_defined_optional_callbacks, behaviour}) do
    :io_lib.format('behaviour ~w optional callback functions erroneously defined', [behaviour])
  end

  def format_error({:behaviour_info, {_M, f, a}}) do
    :io_lib.format('cannot define callback attibute for ~tw/~w when behaviour_info is defined', [
      f,
      a
    ])
  end

  def format_error({:redefine_optional_callback, {f, a}}) do
    :io_lib.format('optional callback ~tw/~w duplicated', [f, a])
  end

  def format_error({:undefined_callback, {_M, f, a}}) do
    :io_lib.format('callback ~tw/~w is undefined', [f, a])
  end

  def format_error({:singleton_typevar, name}) do
    :io_lib.format('type variable ~w is only used once (is unbound)', [name])
  end

  def format_error({:bad_export_type, _ETs}) do
    :io_lib.format('bad export_type declaration', [])
  end

  def format_error({:duplicated_export_type, {t, a}}) do
    :io_lib.format('type ~tw/~w already exported', [t, a])
  end

  def format_error({:undefined_type, {typeName, arity}}) do
    :io_lib.format('type ~tw~s undefined', [typeName, gen_type_paren(arity)])
  end

  def format_error({:unused_type, {typeName, arity}}) do
    :io_lib.format('type ~tw~s is unused', [typeName, gen_type_paren(arity)])
  end

  def format_error({:new_builtin_type, {typeName, arity}}) do
    :io_lib.format(
      'type ~w~s is a new builtin type; its (re)definition is allowed only until the next release',
      [typeName, gen_type_paren(arity)]
    )
  end

  def format_error({:builtin_type, {typeName, arity}}) do
    :io_lib.format('type ~w~s is a builtin type; it cannot be redefined', [
      typeName,
      gen_type_paren(arity)
    ])
  end

  def format_error({:renamed_type, oldName, newName}) do
    :io_lib.format('type ~w() is now called ~w(); please use the new name instead', [
      oldName,
      newName
    ])
  end

  def format_error({:redefine_type, {typeName, arity}}) do
    :io_lib.format('type ~tw~s already defined', [typeName, gen_type_paren(arity)])
  end

  def format_error({:type_syntax, constr}) do
    :io_lib.format('bad ~tw type', [constr])
  end

  def format_error(:old_abstract_code) do
    :io_lib.format(
      'abstract code generated before Erlang/OTP 19.0 and having typed record fields cannot be compiled',
      []
    )
  end

  def format_error({:redefine_spec, {m, f, a}}) do
    :io_lib.format('spec for ~tw:~tw/~w already defined', [m, f, a])
  end

  def format_error({:redefine_spec, {f, a}}) do
    :io_lib.format('spec for ~tw/~w already defined', [f, a])
  end

  def format_error({:redefine_callback, {f, a}}) do
    :io_lib.format('callback ~tw/~w already defined', [f, a])
  end

  def format_error({:bad_callback, {m, f, a}}) do
    :io_lib.format('explicit module not allowed for callback ~tw:~tw/~w', [m, f, a])
  end

  def format_error({:bad_module, {m, f, a}}) do
    :io_lib.format('spec for function ~w:~tw/~w from other module', [m, f, a])
  end

  def format_error({:spec_fun_undefined, {f, a}}) do
    :io_lib.format('spec for undefined function ~tw/~w', [f, a])
  end

  def format_error({:missing_spec, {f, a}}) do
    :io_lib.format('missing specification for function ~tw/~w', [f, a])
  end

  def format_error(:spec_wrong_arity) do
    'spec has wrong arity'
  end

  def format_error(:callback_wrong_arity) do
    'callback has wrong arity'
  end

  def format_error({:deprecated_builtin_type, {name, arity}, replacement, rel}) do
    useS =
      case replacement do
        {mod, newName} ->
          :io_lib.format('use ~w:~w/~w', [mod, newName, arity])

        {mod, newName, newArity} ->
          :io_lib.format(
            'use ~w:~w/~w or preferably ~w:~w/~w',
            [mod, newName, arity, mod, newName, newArity]
          )
      end

    :io_lib.format('type ~w/~w is deprecated and will be removed in ~s; use ~s', [
      name,
      arity,
      rel,
      useS
    ])
  end

  def format_error({:not_exported_opaque, {typeName, arity}}) do
    :io_lib.format('opaque type ~tw~s is not exported', [typeName, gen_type_paren(arity)])
  end

  def format_error({:underspecified_opaque, {typeName, arity}}) do
    :io_lib.format('opaque type ~tw~s is underspecified and therefore meaningless', [
      typeName,
      gen_type_paren(arity)
    ])
  end

  def format_error({:bad_dialyzer_attribute, term}) do
    :io_lib.format('badly formed dialyzer attribute: ~tw', [term])
  end

  def format_error({:bad_dialyzer_option, term}) do
    :io_lib.format('unknown dialyzer warning option: ~tw', [term])
  end

  def format_error({:format_error, {fmt, args}}) do
    :io_lib.format(fmt, args)
  end

  defp gen_type_paren(arity)
       when is_integer(arity) and
              arity >= 0 do
    gen_type_paren_1(arity, ')')
  end

  defp gen_type_paren_1(0, acc) do
    '(' ++ acc
  end

  defp gen_type_paren_1(1, acc) do
    '(_' ++ acc
  end

  defp gen_type_paren_1(n, acc) do
    gen_type_paren_1(n - 1, ',_' ++ acc)
  end

  defp format_mfa({m, f, [_ | _] = as}) do
    ',' ++ arityString =
      :lists.append(
        for a <- as do
          [?, | :erlang.integer_to_list(a)]
        end
      )

    format_mf(m, f, arityString)
  end

  defp format_mfa({m, f, a}) when is_integer(a) do
    format_mf(m, f, :erlang.integer_to_list(a))
  end

  defp format_mf(m, f, arityString)
       when is_atom(m) and
              is_atom(f) do
    :erlang.atom_to_list(m) ++ ':' ++ :erlang.atom_to_list(f) ++ '/' ++ arityString
  end

  defp format_mna({m, n, a}) when is_integer(a) do
    :erlang.atom_to_list(m) ++ ':' ++ :erlang.atom_to_list(n) ++ gen_type_paren(a)
  end

  defp format_where(l) when is_integer(l) do
    :io_lib.format('(line ~p)', [l])
  end

  defp format_where({l, c})
       when is_integer(l) and
              is_integer(c) do
    :io_lib.format('(line ~p, column ~p)', [l, c])
  end

  defp pseudolocals() do
    [{:module_info, 0}, {:module_info, 1}, {:record_info, 2}]
  end

  def exprs(exprs, bindingsList) do
    exprs_opt(exprs, bindingsList, [])
  end

  def exprs_opt(exprs, bindingsList, opts) do
    {st0, vs} =
      foldl(
        fn
          {{:record, _SequenceNumber, _Name}, attr0}, {st1, vs1} ->
            attr = set_file(attr0, 'none')
            {attribute_state(attr, st1), vs1}

          {v, _}, {st1, vs1} ->
            {st1, [{v, {:bound, :unused, []}} | vs1]}
        end,
        {start('nofile', opts), []},
        bindingsList
      )

    vt = :orddict.from_list(vs)
    {_Evt, st} = exprs(set_file(exprs, 'nofile'), vt, st0)
    return_status(st)
  end

  def used_vars(exprs, bindingsList) do
    vs =
      foldl(
        fn
          {{:record, _SequenceNumber, _Name}, _Attr}, vs0 ->
            vs0

          {v, _Val}, vs0 ->
            [{v, {:bound, :unused, []}} | vs0]
        end,
        [],
        bindingsList
      )

    vt = :orddict.from_list(vs)
    {evt, _St} = exprs(set_file(exprs, 'nofile'), vt, start())

    {:ok,
     foldl(
       fn
         {v, {_, :used, _}}, l ->
           [v | l]

         _, l ->
           l
       end,
       [],
       evt
     )}
  end

  def module(forms) do
    opts = compiler_options(forms)
    st = forms(forms, start('nofile', opts))
    return_status(st)
  end

  def module(forms, fileName) do
    opts = compiler_options(forms)
    st = forms(forms, start(fileName, opts))
    return_status(st)
  end

  def module(forms, fileName, opts0) do
    opts = compiler_options(forms) ++ opts0
    st = forms(forms, start(fileName, opts))
    return_status(st)
  end

  defp compiler_options(forms) do
    :lists.flatten(
      for {:attribute, _, :compile, c} <- forms do
        c
      end
    )
  end

  defp start() do
    start('nofile', [])
  end

  defp start(file, opts) do
    enabled0 = [
      {:unused_vars, bool_option(:warn_unused_vars, :nowarn_unused_vars, true, opts)},
      {:export_all, bool_option(:warn_export_all, :nowarn_export_all, true, opts)},
      {:export_vars, bool_option(:warn_export_vars, :nowarn_export_vars, false, opts)},
      {:shadow_vars, bool_option(:warn_shadow_vars, :nowarn_shadow_vars, true, opts)},
      {:unused_import, bool_option(:warn_unused_import, :nowarn_unused_import, false, opts)},
      {:unused_function, bool_option(:warn_unused_function, :nowarn_unused_function, true, opts)},
      {:unused_type, bool_option(:warn_unused_type, :nowarn_unused_type, true, opts)},
      {:bif_clash, bool_option(:warn_bif_clash, :nowarn_bif_clash, true, opts)},
      {:unused_record, bool_option(:warn_unused_record, :nowarn_unused_record, true, opts)},
      {:deprecated_function,
       bool_option(
         :warn_deprecated_function,
         :nowarn_deprecated_function,
         true,
         opts
       )},
      {:deprecated_type,
       bool_option(
         :warn_deprecated_type,
         :nowarn_deprecated_type,
         true,
         opts
       )},
      {:obsolete_guard,
       bool_option(
         :warn_obsolete_guard,
         :nowarn_obsolete_guard,
         true,
         opts
       )},
      {:untyped_record,
       bool_option(
         :warn_untyped_record,
         :nowarn_untyped_record,
         false,
         opts
       )},
      {:missing_spec,
       bool_option(
         :warn_missing_spec,
         :nowarn_missing_spec,
         false,
         opts
       )},
      {:missing_spec_all,
       bool_option(
         :warn_missing_spec_all,
         :nowarn_missing_spec_all,
         false,
         opts
       )},
      {:removed,
       bool_option(
         :warn_removed,
         :nowarn_removed,
         true,
         opts
       )},
      {:nif_inline,
       bool_option(
         :warn_nif_inline,
         :nowarn_nif_inline,
         true,
         opts
       )}
    ]

    enabled1 =
      for {category, true} <- enabled0 do
        category
      end

    enabled = :ordsets.from_list(enabled1)

    calls =
      case :ordsets.is_element(
             :unused_function,
             enabled
           ) do
        true ->
          :maps.from_list([{{:module_info, 1}, pseudolocals()}])

        false ->
          :undefined
      end

    r_lint(
      state: :start,
      exports: :gb_sets.from_list([{:module_info, 0}, {:module_info, 1}]),
      compile: opts,
      defined: :gb_sets.from_list(pseudolocals()),
      called:
        for f <- pseudolocals() do
          {f, 0}
        end,
      usage: r_usage(calls: calls),
      warn_format: value_option(:warn_format, 1, :warn_format, 1, :nowarn_format, 0, opts),
      enabled_warnings: enabled,
      nowarn_bif_clash:
        nowarn_function(
          :nowarn_bif_clash,
          opts
        ),
      file: file
    )
  end

  defp is_warn_enabled(type, r_lint(enabled_warnings: enabled)) do
    :ordsets.is_element(type, enabled)
  end

  defp return_status(st) do
    ws = pack_warnings(r_lint(st, :warnings))

    case pack_errors(r_lint(st, :errors)) do
      [] ->
        {:ok, ws}

      es ->
        {:error, es, ws}
    end
  end

  defp pack_errors(es) do
    {es1, _} =
      mapfoldl(
        fn {file, e}, i ->
          {{file, {i, e}}, i - 1}
        end,
        -1,
        es
      )

    map(
      fn {file, eIs} ->
        {file,
         map(
           fn {_I, e} ->
             e
           end,
           eIs
         )}
      end,
      pack_warnings(es1)
    )
  end

  defp pack_warnings(ws) do
    for file <-
          :lists.usort(
            for {f, _} <- ws do
              f
            end
          ) do
      {file,
       :lists.sort(
         for {f, w} <- ws, f === file do
           w
         end
       )}
    end
  end

  defp add_error(e, st) do
    add_lint_error(e, r_lint(st, :file), st)
  end

  defp add_error(anno, e0, r_lint(gexpr_context: context) = st) do
    e =
      case {e0, context} do
        {:illegal_guard_expr, :bin_seg_size} ->
          :illegal_bitsize

        {{:illegal_guard_local_call, fA}, :bin_seg_size} ->
          {:illegal_bitsize_local_call, fA}

        {_, _} ->
          e0
      end

    {file, location} = loc(anno, st)
    add_lint_error({location, :erl_lint, e}, file, st)
  end

  defp add_lint_error(e, file, st) do
    r_lint(st, errors: [{file, e} | r_lint(st, :errors)])
  end

  defp add_warning(w, st) do
    add_lint_warning(w, r_lint(st, :file), st)
  end

  defp add_warning(fileLine, w, st) do
    {file, location} = loc(fileLine, st)
    add_lint_warning({location, :erl_lint, w}, file, st)
  end

  defp add_lint_warning(w, file, st) do
    r_lint(st, warnings: [{file, w} | r_lint(st, :warnings)])
  end

  defp loc(anno, st) do
    location = :erl_anno.location(anno)

    case :erl_anno.file(anno) do
      :undefined ->
        {r_lint(st, :file), location}

      file ->
        {file, location}
    end
  end

  defp forms(forms0, st0) do
    forms = eval_file_attribute(forms0, st0)
    locals = local_functions(forms)
    autoImportSuppressed = auto_import_suppressed(r_lint(st0, :compile))
    stDeprecated = disallowed_compile_flags(forms, st0)

    st1 =
      includes_qlc_hrl(
        forms,
        r_lint(stDeprecated,
          locals: locals,
          no_auto: autoImportSuppressed
        )
      )

    st2 = bif_clashes(forms, st1)
    st3 = not_deprecated(forms, st2)
    st4 = not_removed(forms, st3)
    st5 = foldl(&form/2, pre_scan(forms, st4), forms)
    post_traversal_check(forms, st5)
  end

  defp pre_scan([{:attribute, l, :compile, c} | fs], st) do
    case is_warn_enabled(
           :export_all,
           st
         ) and member(:export_all, :lists.flatten([c])) do
      true ->
        pre_scan(fs, add_warning(l, :export_all, st))

      false ->
        pre_scan(fs, st)
    end
  end

  defp pre_scan([_ | fs], st) do
    pre_scan(fs, st)
  end

  defp pre_scan([], st) do
    st
  end

  defp includes_qlc_hrl(forms, st) do
    qH =
      for {:attribute, _, :file, {file, _line}} <- forms,
          :filename.basename(file) === 'qlc.hrl' do
        file
      end

    r_lint(st, xqlc: qH !== [])
  end

  defp eval_file_attribute(forms, st) do
    eval_file_attr(forms, r_lint(st, :file))
  end

  defp eval_file_attr(
         [
           {:attribute, _L, :file, {file, _Line}} = form
           | forms
         ],
         _File
       ) do
    [form | eval_file_attr(forms, file)]
  end

  defp eval_file_attr([form0 | forms], file) do
    form = set_form_file(form0, file)
    [form | eval_file_attr(forms, file)]
  end

  defp eval_file_attr([], _File) do
    []
  end

  defp set_form_file({:attribute, l, k, v}, file) do
    {:attribute, :erl_anno.set_file(file, l), k, v}
  end

  defp set_form_file({:function, l, n, a, c}, file) do
    {:function, :erl_anno.set_file(file, l), n, a, c}
  end

  defp set_form_file(form, _File) do
    form
  end

  defp set_file(ts, file) when is_list(ts) do
    for t <- ts do
      anno_set_file(t, file)
    end
  end

  defp set_file(t, file) do
    anno_set_file(t, file)
  end

  defp anno_set_file(t, file) do
    f = fn anno ->
      :erl_anno.set_file(file, anno)
    end

    :erl_parse.map_anno(f, t)
  end

  defp form({:error, e}, st) do
    add_error(e, st)
  end

  defp form({:warning, w}, st) do
    add_warning(w, st)
  end

  defp form({:attribute, _L, :file, {file, _Line}}, st) do
    r_lint(st, file: file)
  end

  defp form({:attribute, _L, :compile, _}, st) do
    st
  end

  defp form(form, r_lint(state: state) = st) do
    case state do
      :start ->
        start_state(form, st)

      :attribute ->
        attribute_state(form, st)

      :function ->
        function_state(form, st)
    end
  end

  defp start_state(
         {:attribute, line, :module, {_, _}} = form,
         st0
       ) do
    st1 = add_error(line, :pmod_unsupported, st0)
    attribute_state(form, r_lint(st1, state: :attribute))
  end

  defp start_state({:attribute, line, :module, m}, st0) do
    st1 = r_lint(st0, module: m)
    st2 = r_lint(st1, state: :attribute)
    check_module_name(m, line, st2)
  end

  defp start_state(form, st) do
    anno =
      case form do
        {:eof, l} ->
          :erl_anno.new(l)

        _ ->
          :erlang.element(2, form)
      end

    st1 = add_error(anno, :undefined_module, st)
    attribute_state(form, r_lint(st1, state: :attribute))
  end

  defp attribute_state(
         {:attribute, _L, :module, _M},
         r_lint(module: :"") = st
       ) do
    st
  end

  defp attribute_state({:attribute, l, :module, _M}, st) do
    add_error(l, :redefine_module, st)
  end

  defp attribute_state({:attribute, l, :export, es}, st) do
    export(l, es, st)
  end

  defp attribute_state({:attribute, l, :export_type, es}, st) do
    export_type(l, es, st)
  end

  defp attribute_state({:attribute, l, :import, is}, st) do
    import(l, is, st)
  end

  defp attribute_state({:attribute, l, :record, {name, fields}}, st) do
    record_def(l, name, fields, st)
  end

  defp attribute_state({:attribute, la, :behaviour, behaviour}, st) do
    r_lint(st, behaviour: r_lint(st, :behaviour) ++ [{la, behaviour}])
  end

  defp attribute_state({:attribute, la, :behavior, behaviour}, st) do
    r_lint(st, behaviour: r_lint(st, :behaviour) ++ [{la, behaviour}])
  end

  defp attribute_state(
         {:attribute, l, :type, {typeName, typeDef, args}},
         st
       ) do
    type_def(:type, l, typeName, typeDef, args, st)
  end

  defp attribute_state(
         {:attribute, l, :opaque, {typeName, typeDef, args}},
         st
       ) do
    type_def(:opaque, l, typeName, typeDef, args, st)
  end

  defp attribute_state({:attribute, l, :spec, {fun, types}}, st) do
    spec_decl(l, fun, types, st)
  end

  defp attribute_state({:attribute, l, :callback, {fun, types}}, st) do
    callback_decl(l, fun, types, st)
  end

  defp attribute_state({:attribute, l, :optional_callbacks, es}, st) do
    optional_callbacks(l, es, st)
  end

  defp attribute_state({:attribute, l, :on_load, val}, st) do
    on_load(l, val, st)
  end

  defp attribute_state({:attribute, _L, _Other, _Val}, st) do
    st
  end

  defp attribute_state(form, st) do
    function_state(form, r_lint(st, state: :function))
  end

  defp function_state({:attribute, l, :record, {name, fields}}, st) do
    record_def(l, name, fields, st)
  end

  defp function_state(
         {:attribute, l, :type, {typeName, typeDef, args}},
         st
       ) do
    type_def(:type, l, typeName, typeDef, args, st)
  end

  defp function_state(
         {:attribute, l, :opaque, {typeName, typeDef, args}},
         st
       ) do
    type_def(:opaque, l, typeName, typeDef, args, st)
  end

  defp function_state({:attribute, l, :spec, {fun, types}}, st) do
    spec_decl(l, fun, types, st)
  end

  defp function_state({:attribute, _L, :dialyzer, _Val}, st) do
    st
  end

  defp function_state({:attribute, la, attr, _Val}, st) do
    add_error(la, {:attribute, attr}, st)
  end

  defp function_state({:function, l, n, a, cs}, st) do
    function(l, n, a, cs, st)
  end

  defp function_state({:eof, l}, st) do
    eof(l, st)
  end

  defp eof(_Line, st0) do
    st0
  end

  defp bif_clashes(forms, r_lint(nowarn_bif_clash: nowarn) = st) do
    clashes0 =
      for {:function, _L, name, arity, _Cs} <- forms,
          :erl_internal.bif(name, arity) do
        {name, arity}
      end

    clashes =
      :ordsets.subtract(
        :ordsets.from_list(clashes0),
        nowarn
      )

    r_lint(st, clashes: clashes)
  end

  defp not_deprecated(forms, r_lint(compile: opts) = st0) do
    mFAsL =
      for {:attribute, l, :compile, args} <- forms,
          {:nowarn_deprecated_function, mFAs0} <- :lists.flatten([args]),
          mFA <- :lists.flatten([mFAs0]) do
        {mFA, l}
      end

    nowarn =
      for {:nowarn_deprecated_function, mFAs0} <- opts,
          mFA <- :lists.flatten([mFAs0]) do
        mFA
      end

    mL =
      for {{m, _F, _A}, l} <- mFAsL, is_atom(m) do
        {m, l}
      end

    st1 =
      foldl(
        fn {m, l}, st2 ->
          check_module_name(m, l, st2)
        end,
        st0,
        mL
      )

    r_lint(st1, not_deprecated: :ordsets.from_list(nowarn))
  end

  defp not_removed(forms, r_lint(compile: opts) = st0) do
    mFAsL =
      for {:attribute, l, :compile, args} <- forms,
          {:nowarn_removed, mFAs0} <- :lists.flatten([args]),
          mFA <- :lists.flatten([mFAs0]) do
        {mFA, l}
      end

    nowarn =
      for {:nowarn_removed, mFAs0} <- opts,
          mFA <- :lists.flatten([mFAs0]) do
        mFA
      end

    st1 =
      foldl(
        fn
          {{m, _F, _A}, l}, st2 ->
            check_module_name(m, l, st2)

          {m, l}, st2 ->
            check_module_name(m, l, st2)
        end,
        st0,
        mFAsL
      )

    r_lint(st1, not_removed: :gb_sets.from_list(nowarn))
  end

  defp disallowed_compile_flags(forms, st0) do
    errors0 =
      for {:attribute, a, :compile, :nowarn_bif_clash} <- forms,
          {_, l} <- [loc(a, st0)] do
        {r_lint(st0, :file), {l, :erl_lint, :disallowed_nowarn_bif_clash}}
      end

    errors1 =
      for {:attribute, a, :compile, {:nowarn_bif_clash, {_, _}}} <- forms,
          {_, l} <- [loc(a, st0)] do
        {r_lint(st0, :file), {l, :erl_lint, :disallowed_nowarn_bif_clash}}
      end

    disabled = not is_warn_enabled(:bif_clash, st0)

    errors =
      cond do
        disabled and errors0 === [] ->
          [
            {r_lint(st0, :file), {:erl_lint, :disallowed_nowarn_bif_clash}}
            | r_lint(st0, :errors)
          ]

        disabled ->
          errors0 ++ errors1 ++ r_lint(st0, :errors)

        true ->
          errors1 ++ r_lint(st0, :errors)
      end

    r_lint(st0, errors: errors)
  end

  defp post_traversal_check(forms, st0) do
    st1 = check_behaviour(st0)
    st2 = check_deprecated(forms, st1)
    st3 = check_imports(forms, st2)
    st4 = check_inlines(forms, st3)
    st5 = check_undefined_functions(st4)
    st6 = check_unused_functions(forms, st5)
    st7 = check_bif_clashes(forms, st6)
    st8 = check_specs_without_function(st7)
    st9 = check_functions_without_spec(forms, st8)
    stA = check_undefined_types(st9)
    stB = check_unused_types(forms, stA)
    stC = check_untyped_records(forms, stB)
    stD = check_on_load(stC)
    stE = check_unused_records(forms, stD)
    stF = check_local_opaque_types(stE)
    stG = check_dialyzer_attribute(forms, stF)
    stH = check_callback_information(stG)
    check_removed(forms, stH)
  end

  defp check_behaviour(st0) do
    behaviour_check(r_lint(st0, :behaviour), st0)
  end

  defp behaviour_check(bs, st0) do
    {allBfs0, st1} = all_behaviour_callbacks(bs, [], st0)
    st = behaviour_missing_callbacks(allBfs0, st1)
    exports = exports(st0)

    f = fn bfs, oBfs ->
      for b <- bfs,
          not :lists.member(b, oBfs) or
            :gb_sets.is_member(
              b,
              exports
            ) do
        b
      end
    end

    allBfs =
      for {item, bfs0, oBfs0} <- allBfs0 do
        {item, f.(bfs0, oBfs0)}
      end

    behaviour_conflicting(allBfs, st)
  end

  defp all_behaviour_callbacks([{line, b} | bs], acc, st0) do
    {bfs0, oBfs0, st} = behaviour_callbacks(line, b, st0)
    all_behaviour_callbacks(bs, [{{line, b}, bfs0, oBfs0} | acc], st)
  end

  defp all_behaviour_callbacks([], acc, st) do
    {reverse(acc), st}
  end

  defp behaviour_callbacks(line, b, st0) do
    try do
      b.behaviour_info(:callbacks)
    catch
      _, _ ->
        st1 = add_warning(line, {:undefined_behaviour, b}, st0)
        st2 = check_module_name(b, line, st1)
        {[], [], st2}
    else
      :undefined ->
        st1 = add_warning(line, {:undefined_behaviour_callbacks, b}, st0)
        {[], [], st1}

      funcs ->
        case is_fa_list(funcs) do
          true ->
            try do
              b.behaviour_info(:optional_callbacks)
            catch
              _, _ ->
                {funcs, [], st0}
            else
              :undefined ->
                {funcs, [], st0}

              optFuncs ->
                case is_fa_list(optFuncs) do
                  true ->
                    {funcs, optFuncs, st0}

                  false ->
                    w = {:ill_defined_optional_callbacks, b}
                    st1 = add_warning(line, w, st0)
                    {funcs, [], st1}
                end
            end

          false ->
            st1 = add_warning(line, {:ill_defined_behaviour_callbacks, b}, st0)
            {[], [], st1}
        end
    end
  end

  defp behaviour_missing_callbacks([{{line, b}, bfs0, oBfs} | t], st0) do
    bfs =
      :ordsets.subtract(
        :ordsets.from_list(bfs0),
        :ordsets.from_list(oBfs)
      )

    exports = :gb_sets.to_list(exports(st0))
    missing = :ordsets.subtract(bfs, exports)

    st =
      foldl(
        fn f, s0 ->
          case is_fa(f) do
            true ->
              m = {:undefined_behaviour_func, f, b}
              add_warning(line, m, s0)

            false ->
              s0
          end
        end,
        st0,
        missing
      )

    behaviour_missing_callbacks(t, st)
  end

  defp behaviour_missing_callbacks([], st) do
    st
  end

  defp behaviour_conflicting(allBfs, st) do
    r0 = :sofs.relation(allBfs, [{:item, [:callback]}])
    r1 = :sofs.family_to_relation(r0)
    r2 = :sofs.converse(r1)
    r3 = :sofs.relation_to_family(r2)

    r4 =
      :sofs.family_specification(
        fn s ->
          :sofs.no_elements(s) > 1
        end,
        r3
      )

    r = :sofs.to_external(r4)
    behaviour_add_conflicts(r, st)
  end

  defp behaviour_add_conflicts([{cb, [{firstLoc, firstB} | cs]} | t], st0) do
    firstL = :erlang.element(2, loc(firstLoc, st0))
    st = behaviour_add_conflict(cs, cb, firstL, firstB, st0)
    behaviour_add_conflicts(t, st)
  end

  defp behaviour_add_conflicts([], st) do
    st
  end

  defp behaviour_add_conflict([{line, b} | cs], cb, firstL, firstB, st0) do
    st = add_warning(line, {:conflicting_behaviours, cb, b, firstL, firstB}, st0)
    behaviour_add_conflict(cs, cb, firstL, firstB, st)
  end

  defp behaviour_add_conflict([], _, _, _, st) do
    st
  end

  defp check_deprecated(forms, st0) do
    exports =
      case member(
             :export_all,
             r_lint(st0, :compile)
           ) do
        true ->
          r_lint(st0, :defined)

        false ->
          r_lint(st0, :exports)
      end

    x = ignore_predefined_funcs(:gb_sets.to_list(exports))
    r_lint(module: mod) = st0

    bad =
      for {:attribute, l, :deprecated, depr} <- forms,
          d <- :lists.flatten([depr]),
          e <- depr_cat(d, x, mod) do
        {e, l}
      end

    foldl(
      fn {e, l}, st1 ->
        add_error(l, e, st1)
      end,
      st0,
      bad
    )
  end

  defp depr_cat({f, a, flg} = d, x, mod) do
    case deprecated_flag(flg) do
      false ->
        [{:invalid_deprecated, d}]

      true ->
        depr_fa(f, a, x, mod)
    end
  end

  defp depr_cat({f, a}, x, mod) do
    depr_fa(f, a, x, mod)
  end

  defp depr_cat(:module, _X, _Mod) do
    []
  end

  defp depr_cat(d, _X, _Mod) do
    [{:invalid_deprecated, d}]
  end

  defp depr_fa(:_, :_, _X, _Mod) do
    []
  end

  defp depr_fa(f, :_, x, _Mod) when is_atom(f) do
    case :lists.filter(
           fn {f1, _} ->
             f1 === f
           end,
           x
         ) do
      [] ->
        [{:bad_deprecated, {f, :_}}]

      _ ->
        []
    end
  end

  defp depr_fa(f, a, x, mod)
       when is_atom(f) and
              is_integer(a) and a >= 0 do
    case :lists.member({f, a}, x) do
      true ->
        []

      false ->
        case :erlang.is_builtin(mod, f, a) do
          true ->
            []

          false ->
            [{:bad_deprecated, {f, a}}]
        end
    end
  end

  defp depr_fa(f, a, _X, _Mod) do
    [{:invalid_deprecated, {f, a}}]
  end

  defp deprecated_flag(:next_version) do
    true
  end

  defp deprecated_flag(:next_major_release) do
    true
  end

  defp deprecated_flag(:eventually) do
    true
  end

  defp deprecated_flag(string) do
    deprecated_desc(string)
  end

  defp deprecated_desc([char | str]) when is_integer(char) do
    deprecated_desc(str)
  end

  defp deprecated_desc([]) do
    true
  end

  defp deprecated_desc(_) do
    false
  end

  defp check_removed(forms, st0) do
    exports =
      case member(
             :export_all,
             r_lint(st0, :compile)
           ) do
        true ->
          r_lint(st0, :defined)

        false ->
          r_lint(st0, :exports)
      end

    x = ignore_predefined_funcs(:gb_sets.to_list(exports))
    r_lint(module: mod) = st0

    bad =
      for {:attribute, l, :removed, removed} <- forms,
          r <- :lists.flatten([removed]),
          e <- removed_cat(r, x, mod) do
        {e, l}
      end

    foldl(
      fn {e, l}, st1 ->
        add_error(l, e, st1)
      end,
      st0,
      bad
    )
  end

  defp removed_cat({f, a, desc} = r, x, mod) do
    case removed_desc(desc) do
      false ->
        [{:invalid_removed, r}]

      true ->
        removed_fa(f, a, x, mod)
    end
  end

  defp removed_cat({f, a}, x, mod) do
    removed_fa(f, a, x, mod)
  end

  defp removed_cat(:module, x, mod) do
    removed_fa(:_, :_, x, mod)
  end

  defp removed_cat(r, _X, _Mod) do
    [{:invalid_removed, r}]
  end

  defp removed_fa(:_, :_, x, _Mod) do
    case x do
      [_ | _] ->
        [{:bad_removed, {:_, :_}}]

      [] ->
        []
    end
  end

  defp removed_fa(f, :_, x, _Mod) when is_atom(f) do
    case :lists.filter(
           fn {f1, _} ->
             f1 === f
           end,
           x
         ) do
      [_ | _] ->
        [{:bad_removed, {f, :_}}]

      _ ->
        []
    end
  end

  defp removed_fa(f, a, x, mod)
       when is_atom(f) and
              is_integer(a) and a >= 0 do
    case :lists.member({f, a}, x) do
      true ->
        [{:bad_removed, {f, a}}]

      false ->
        case :erlang.is_builtin(mod, f, a) do
          true ->
            [{:bad_removed, {f, a}}]

          false ->
            []
        end
    end
  end

  defp removed_fa(f, a, _X, _Mod) do
    [{:invalid_removed, {f, a}}]
  end

  defp removed_desc([char | str]) when is_integer(char) do
    removed_desc(str)
  end

  defp removed_desc([]) do
    true
  end

  defp removed_desc(_) do
    false
  end

  defp ignore_predefined_funcs([{:behaviour_info, 1} | fs]) do
    ignore_predefined_funcs(fs)
  end

  defp ignore_predefined_funcs([{:module_info, 0} | fs]) do
    ignore_predefined_funcs(fs)
  end

  defp ignore_predefined_funcs([{:module_info, 1} | fs]) do
    ignore_predefined_funcs(fs)
  end

  defp ignore_predefined_funcs([other | fs]) do
    [other | ignore_predefined_funcs(fs)]
  end

  defp ignore_predefined_funcs([]) do
    []
  end

  defp check_imports(forms, st0) do
    case is_warn_enabled(:unused_import, st0) do
      false ->
        st0

      true ->
        usage = r_lint(st0, :usage)

        unused =
          :ordsets.subtract(
            r_lint(st0, :imports),
            r_usage(usage, :imported)
          )

        imports =
          for {:attribute, l, :import, {mod, fs}} <- forms,
              fA <- :lists.usort(fs) do
            {{fA, mod}, l}
          end

        bad =
          for fM <- unused, {fM2, l} <- imports, fM === fM2 do
            {fM, l}
          end

        func_line_warning(:unused_import, bad, st0)
    end
  end

  defp check_inlines(forms, st0) do
    check_option_functions(forms, :inline, :bad_inline, st0)
  end

  defp check_unused_functions(forms, st0) do
    st1 = check_option_functions(forms, :nowarn_unused_function, :bad_nowarn_unused_function, st0)
    opts = r_lint(st1, :compile)

    case member(:export_all, opts) or
           not is_warn_enabled(
             :unused_function,
             st1
           ) do
      true ->
        st1

      false ->
        nowarn = nowarn_function(:nowarn_unused_function, opts)
        usage = r_lint(st1, :usage)

        used =
          reached_functions(
            initially_reached(st1),
            r_usage(usage, :calls)
          )

        usedOrNowarn = :ordsets.union(used, nowarn)

        unused =
          :ordsets.subtract(
            :gb_sets.to_list(r_lint(st1, :defined)),
            usedOrNowarn
          )

        functions =
          for {:function, l, n, a, _} <- forms do
            {{n, a}, l}
          end

        bad =
          for fA <- unused, {fA2, l} <- functions, fA === fA2 do
            {fA, l}
          end

        func_line_warning(:unused_function, bad, st1)
    end
  end

  defp initially_reached(r_lint(exports: exp, on_load: onLoad)) do
    onLoad ++ :gb_sets.to_list(exp)
  end

  defp reached_functions(root, ref) do
    reached_functions(root, [], ref, :gb_sets.empty())
  end

  defp reached_functions([r | rs], more0, ref, reached0) do
    case :gb_sets.is_element(r, reached0) do
      true ->
        reached_functions(rs, more0, ref, reached0)

      false ->
        reached = :gb_sets.add_element(r, reached0)

        case :maps.find(r, ref) do
          {:ok, more} ->
            reached_functions(rs, [more | more0], ref, reached)

          :error ->
            reached_functions(rs, more0, ref, reached)
        end
    end
  end

  defp reached_functions([], [_ | _] = more, ref, reached) do
    reached_functions(:lists.append(more), [], ref, reached)
  end

  defp reached_functions([], [], _Ref, reached) do
    :gb_sets.to_list(reached)
  end

  defp check_undefined_functions(r_lint(called: called0, defined: def0) = st0) do
    called = :sofs.relation(called0, [{:func, :location}])

    def__ =
      :sofs.from_external(
        :gb_sets.to_list(def0),
        [:func]
      )

    undef =
      :sofs.to_external(
        :sofs.drestriction(
          called,
          def__
        )
      )

    foldl(
      fn {nA, l}, st ->
        add_error(l, {:undefined_function, nA}, st)
      end,
      st0,
      undef
    )
  end

  defp check_undefined_types(r_lint(usage: usage, types: def__) = st0) do
    used = r_usage(usage, :used_types)
    uTAs = :maps.keys(used)

    undef =
      for tA <- uTAs,
          not :erlang.is_map_key(tA, def__),
          not is_default_type(tA) do
        {tA, :erlang.map_get(tA, used)}
      end

    foldl(
      fn {tA, l}, st ->
        add_error(l, {:undefined_type, tA}, st)
      end,
      st0,
      undef
    )
  end

  defp check_bif_clashes(forms, st0) do
    check_option_functions(forms, :nowarn_bif_clash, :bad_nowarn_bif_clash, st0)
  end

  defp check_option_functions(forms, tag0, type, st0) do
    fAsL =
      for {:attribute, l, :compile, args} <- forms,
          {tag, fAs0} <- :lists.flatten([args]),
          tag0 === tag,
          fA <- :lists.flatten([fAs0]) do
        {fA, l}
      end

    defFunctions =
      (:gb_sets.to_list(r_lint(st0, :defined)) -- pseudolocals()) ++
        for {{f, a}, _} <- :orddict.to_list(r_lint(st0, :imports)) do
          {f, a}
        end

    bad =
      for {fA, l} <- fAsL,
          not member(fA, defFunctions) do
        {fA, l}
      end

    func_line_error(type, bad, st0)
  end

  defp nowarn_function(tag, opts) do
    :ordsets.from_list(
      for {tag1, fAs} <- opts, tag1 === tag, fA <- :lists.flatten([fAs]) do
        fA
      end
    )
  end

  defp func_line_warning(type, fs, st) do
    foldl(
      fn {f, line}, st0 ->
        add_warning(line, {type, f}, st0)
      end,
      st,
      fs
    )
  end

  defp func_line_error(type, fs, st) do
    foldl(
      fn {f, line}, st0 ->
        add_error(line, {type, f}, st0)
      end,
      st,
      fs
    )
  end

  defp check_untyped_records(forms, st0) do
    case is_warn_enabled(:untyped_record, st0) do
      true ->
        recNames = :maps.keys(r_lint(st0, :records))

        tRecNames =
          for {:attribute, _, :record, {name, fields}} <- forms,
              :lists.all(
                fn
                  {:typed_record_field, _, _} ->
                    true

                  _ ->
                    false
                end,
                fields
              ) do
            name
          end

        foldl(
          fn n, st ->
            {l, fields} = :erlang.map_get(n, r_lint(st0, :records))

            case fields do
              [] ->
                st

              [_ | _] ->
                add_warning(l, {:untyped_record, n}, st)
            end
          end,
          st0,
          :ordsets.subtract(
            :ordsets.from_list(recNames),
            :ordsets.from_list(tRecNames)
          )
        )

      false ->
        st0
    end
  end

  defp check_unused_records(forms, st0) do
    attrFiles =
      for {:attribute, _L, :file, {file, _Line}} <- forms do
        file
      end

    case {is_warn_enabled(:unused_record, st0), attrFiles} do
      {true, [firstFile | _]} ->
        usage = r_lint(st0, :usage)
        usedRecords = r_usage(usage, :used_records)

        uRecs =
          :gb_sets.fold(
            fn used, recs ->
              :maps.remove(used, recs)
            end,
            r_lint(st0, :records),
            usedRecords
          )

        unused =
          for {name, {fileLine, _Fields}} <- :maps.to_list(uRecs),
              :erlang.element(1, loc(fileLine, st0)) === firstFile do
            {name, fileLine}
          end

        foldl(
          fn {n, l}, st ->
            add_warning(l, {:unused_record, n}, st)
          end,
          st0,
          unused
        )

      _ ->
        st0
    end
  end

  defp check_callback_information(
         r_lint(
           callbacks: callbacks,
           optional_callbacks: optionalCbs,
           defined: defined
         ) = st0
       ) do
    optFun = fn mFA, line, st ->
      case :erlang.is_map_key(mFA, callbacks) do
        true ->
          st

        false ->
          add_error(line, {:undefined_callback, mFA}, st)
      end
    end

    st1 = :maps.fold(optFun, st0, optionalCbs)

    case :gb_sets.is_member(
           {:behaviour_info, 1},
           defined
         ) do
      false ->
        st1

      true ->
        case map_size(callbacks) do
          0 ->
            st1

          _ ->
            foldFun = fn fa, line, st ->
              add_error(line, {:behaviour_info, fa}, st)
            end

            :maps.fold(foldFun, st1, callbacks)
        end
    end
  end

  defp export(line, es, r_lint(exports: es0, called: called) = st0) do
    {es1, c1, st1} =
      foldl(
        fn nA, {e, c, st2} ->
          st =
            case :gb_sets.is_element(nA, e) do
              true ->
                warn = {:duplicated_export, nA}
                add_warning(line, warn, st2)

              false ->
                st2
            end

          {:gb_sets.add_element(nA, e), [{nA, line} | c], st}
        end,
        {es0, called, st0},
        es
      )

    r_lint(st1, exports: es1, called: c1)
  end

  defp export_type(line, eTs, r_lint(usage: usage, exp_types: eTs0) = st0) do
    uTs0 = r_usage(usage, :used_types)

    try do
      foldl(
        fn {t, a} = tA, {e, u, st2}
           when is_atom(t) and
                  is_integer(a) ->
          st =
            case :gb_sets.is_element(tA, e) do
              true ->
                warn = {:duplicated_export_type, tA}
                add_warning(line, warn, st2)

              false ->
                st2
            end

          {:gb_sets.add_element(tA, e), :maps.put(tA, line, u), st}
        end,
        {eTs0, uTs0, st0},
        eTs
      )
    catch
      :error, _ ->
        add_error(line, {:bad_export_type, eTs}, st0)
    else
      {eTs1, uTs1, st1} ->
        r_lint(st1,
          usage: r_usage(usage, used_types: uTs1),
          exp_types: eTs1
        )
    end
  end

  defp exports(r_lint(compile: opts, defined: defs, exports: es)) do
    case :lists.member(:export_all, opts) do
      true ->
        defs

      false ->
        es
    end
  end

  defp import(line, {mod, fs}, st00) do
    st = check_module_name(mod, line, st00)
    mfs = :ordsets.from_list(fs)

    case check_imports(line, mfs, r_lint(st, :imports)) do
      [] ->
        r_lint(st, imports: add_imports(mod, mfs, r_lint(st, :imports)))

      efs ->
        {err, st1} =
          foldl(
            fn
              {:bif, {f, a}, _}, {err, st0} ->
                warn =
                  is_warn_enabled(:bif_clash, st0) and
                    not bif_clash_specifically_disabled(
                      st0,
                      {f, a}
                    )

                autoImpSup =
                  is_autoimport_suppressed(
                    r_lint(st0, :no_auto),
                    {f, a}
                  )

                oldBif = :erl_internal.old_bif(f, a)

                {err,
                 cond do
                   warn and not autoImpSup and oldBif ->
                     add_error(
                       line,
                       {:redefine_old_bif_import, {f, a}},
                       st0
                     )

                   warn and not autoImpSup ->
                     add_warning(
                       line,
                       {:redefine_bif_import, {f, a}},
                       st0
                     )

                   true ->
                     st0
                 end}

              ef, {_Err, st0} ->
                {true, add_error(line, {:redefine_import, ef}, st0)}
            end,
            {false, st},
            efs
          )

        cond do
          not err ->
            r_lint(st1, imports: add_imports(mod, mfs, r_lint(st, :imports)))

          true ->
            st1
        end
    end
  end

  defp check_imports(_Line, fs, is) do
    foldl(
      fn f, efs ->
        case :orddict.find(f, is) do
          {:ok, mod} ->
            [{f, mod} | efs]

          :error ->
            {n, a} = f

            case :erl_internal.bif(n, a) do
              true ->
                [{:bif, f, :erlang} | efs]

              false ->
                efs
            end
        end
      end,
      [],
      fs
    )
  end

  defp add_imports(mod, fs, is) do
    foldl(
      fn f, is0 ->
        :orddict.store(f, mod, is0)
      end,
      is,
      fs
    )
  end

  defp imported(f, a, st) do
    case :orddict.find({f, a}, r_lint(st, :imports)) do
      {:ok, mod} ->
        {:yes, mod}

      :error ->
        :no
    end
  end

  defp on_load(line, {name, arity} = fa, r_lint(on_load: onLoad0) = st0)
       when is_atom(name) and is_integer(arity) do
    st =
      r_lint(st0,
        on_load: [fa | onLoad0],
        on_load_line: line
      )

    case st do
      r_lint(on_load: [{_, 0}]) ->
        st

      r_lint(on_load: [{_, _}]) ->
        add_error(line, {:bad_on_load_arity, fa}, st)

      r_lint(on_load: [_, _ | _]) ->
        add_error(line, :multiple_on_loads, st)
    end
  end

  defp on_load(line, val, st) do
    add_error(line, {:bad_on_load, val}, st)
  end

  defp check_on_load(r_lint(defined: defined, on_load: [{_, 0} = fa], on_load_line: line) = st) do
    case :gb_sets.is_member(fa, defined) do
      true ->
        st

      false ->
        add_error(line, {:undefined_on_load, fa}, st)
    end
  end

  defp check_on_load(st) do
    st
  end

  defp call_function(line, f, a, r_lint(usage: usage0, called: cd, func: func, file: file) = st) do
    r_usage(calls: cs) = usage0
    nA = {f, a}

    usage =
      case cs do
        :undefined ->
          usage0

        _ ->
          r_usage(usage0, calls: maps_prepend(func, nA, cs))
      end

    anno = :erl_anno.set_file(file, line)
    r_lint(st, called: [{nA, anno} | cd], usage: usage)
  end

  defp function(line, name, arity, cs, st0) do
    st1 = r_lint(st0, func: {name, arity})
    st2 = define_function(line, name, arity, st1)
    clauses(cs, st2)
  end

  defp define_function(line, name, arity, st0) do
    st1 = keyword_warning(line, name, st0)
    nA = {name, arity}

    case :gb_sets.is_member(nA, r_lint(st1, :defined)) do
      true ->
        add_error(line, {:redefine_function, nA}, st1)

      false ->
        st2 = function_check_max_args(line, arity, st1)

        st3 =
          r_lint(st2,
            defined:
              :gb_sets.add_element(
                nA,
                r_lint(st2, :defined)
              )
          )

        case imported(name, arity, st3) do
          {:yes, _M} ->
            add_error(line, {:define_import, nA}, st3)

          :no ->
            st3
        end
    end
  end

  defp function_check_max_args(line, arity, st) when arity > 255 do
    add_error(line, {:too_many_arguments, arity}, st)
  end

  defp function_check_max_args(_, _, st) do
    st
  end

  defp clauses(cs, st) do
    foldl(
      fn c, st0 ->
        {_, st1} = clause(c, st0)
        st1
      end,
      st,
      cs
    )
  end

  defp clause({:clause, _Line, h, g, b}, st0) do
    vt0 = []
    {hvt, hnew, st1} = head(h, vt0, st0)
    vt1 = vtupdate(hvt, vtupdate(hnew, vt0))
    {gvt, st2} = guard(g, vt1, st1)
    vt2 = vtupdate(gvt, vt1)
    {bvt, st3} = exprs(b, vt2, st2)
    upd = vtupdate(bvt, vt2)
    check_unused_vars(upd, vt0, st3)
  end

  defp head(ps, vt, st0) do
    head(ps, vt, vt, st0)
  end

  defp head([p | ps], vt, old, st0) do
    {pvt, pnew, st1} = pattern(p, vt, old, st0)
    {psvt, psnew, st2} = head(ps, vt, old, st1)
    {vtmerge_pat(pvt, psvt), vtmerge_pat(pnew, psnew), st2}
  end

  defp head([], _Vt, _Env, st) do
    {[], [], st}
  end

  defp pattern(p, vt, st) do
    pattern(p, vt, vt, st)
  end

  defp pattern({:var, _Line, :_}, _Vt, _Old, st) do
    {[], [], st}
  end

  defp pattern({:var, line, v}, _Vt, old, st) do
    pat_var(v, line, old, [], st)
  end

  defp pattern({:char, _Line, _C}, _Vt, _Old, st) do
    {[], [], st}
  end

  defp pattern({:integer, _Line, _I}, _Vt, _Old, st) do
    {[], [], st}
  end

  defp pattern({:float, _Line, _F}, _Vt, _Old, st) do
    {[], [], st}
  end

  defp pattern({:atom, line, a}, _Vt, _Old, st) do
    {[], [], keyword_warning(line, a, st)}
  end

  defp pattern({:string, _Line, _S}, _Vt, _Old, st) do
    {[], [], st}
  end

  defp pattern({nil, _Line}, _Vt, _Old, st) do
    {[], [], st}
  end

  defp pattern({:cons, _Line, h, t}, vt, old, st0) do
    {hvt, hnew, st1} = pattern(h, vt, old, st0)
    {tvt, tnew, st2} = pattern(t, vt, old, st1)
    {vtmerge_pat(hvt, tvt), vtmerge_pat(hnew, tnew), st2}
  end

  defp pattern({:tuple, _Line, ps}, vt, old, st) do
    pattern_list(ps, vt, old, st)
  end

  defp pattern({:map, _Line, ps}, vt, old, st) do
    pattern_map(ps, vt, old, st)
  end

  defp pattern({:record_index, line, name, field}, _Vt, _Old, st) do
    {vt1, st1} =
      check_record(line, name, st, fn dfs, st1 ->
        pattern_field(field, name, dfs, st1)
      end)

    {vt1, [], st1}
  end

  defp pattern({:record, line, name, pfs}, vt, old, st) do
    case :maps.find(name, r_lint(st, :records)) do
      {:ok, {_Line, fields}} ->
        st1 = used_record(name, st)
        st2 = check_multi_field_init(pfs, line, fields, st1)
        pattern_fields(pfs, name, fields, vt, old, st2)

      :error ->
        {[], [], add_error(line, {:undefined_record, name}, st)}
    end
  end

  defp pattern({:bin, _, fs}, vt, old, st) do
    pattern_bin(fs, vt, old, st)
  end

  defp pattern({:op, _Line, :++, {nil, _}, r}, vt, old, st) do
    pattern(r, vt, old, st)
  end

  defp pattern({:op, _Line, :++, {:cons, li, {:char, _L2, _C}, t}, r}, vt, old, st) do
    pattern({:op, li, :++, t, r}, vt, old, st)
  end

  defp pattern({:op, _Line, :++, {:cons, li, {:integer, _L2, _I}, t}, r}, vt, old, st) do
    pattern({:op, li, :++, t, r}, vt, old, st)
  end

  defp pattern({:op, _Line, :++, {:string, _Li, _S}, r}, vt, old, st) do
    pattern(r, vt, old, st)
  end

  defp pattern({:match, _Line, pat1, pat2}, vt, old, st0) do
    {lvt, lnew, st1} = pattern(pat1, vt, old, st0)
    {rvt, rnew, st2} = pattern(pat2, vt, old, st1)
    st3 = reject_invalid_alias(pat1, pat2, vt, st2)
    {vtmerge_pat(lvt, rvt), vtmerge_pat(lnew, rnew), st3}
  end

  defp pattern(pat, _Vt, _Old, st) do
    case is_pattern_expr(pat) do
      true ->
        {[], [], st}

      false ->
        {[], [], add_error(:erlang.element(2, pat), :illegal_pattern, st)}
    end
  end

  defp pattern_list(ps, vt, old, st) do
    foldl(
      fn p, {psvt, psnew, st0} ->
        {pvt, pnew, st1} = pattern(p, vt, old, st0)
        {vtmerge_pat(pvt, psvt), vtmerge_pat(psnew, pnew), st1}
      end,
      {[], [], st},
      ps
    )
  end

  defp check_multi_field_init(fs, line, fields, st) do
    case has_wildcard_field(fs) and init_fields(fs, line, fields) === [] do
      true ->
        add_error(line, :bad_multi_field_init, st)

      false ->
        st
    end
  end

  defp reject_invalid_alias_expr({:bin, _, _} = p, {:match, _, p0, e}, vt, st0) do
    st = reject_invalid_alias(p, p0, vt, st0)
    reject_invalid_alias_expr(p, e, vt, st)
  end

  defp reject_invalid_alias_expr({:map, _, _} = p, {:match, _, p0, e}, vt, st0) do
    st = reject_invalid_alias(p, p0, vt, st0)
    reject_invalid_alias_expr(p, e, vt, st)
  end

  defp reject_invalid_alias_expr({:match, _, _, _} = p, {:match, _, p0, e}, vt, st0) do
    st = reject_invalid_alias(p, p0, vt, st0)
    reject_invalid_alias_expr(p, e, vt, st)
  end

  defp reject_invalid_alias_expr(_, _, _, st) do
    st
  end

  defp reject_invalid_alias({:bin, line, _}, {:bin, _, _}, _, st) do
    add_error(line, :illegal_bin_pattern, st)
  end

  defp reject_invalid_alias({:map, _Line, ps1}, {:map, _, ps2}, vt, st0) do
    fun = fn
      {:map_field_exact, l, {:var, _, k}, _V}, sti ->
        case is_var_bound(k, vt) do
          true ->
            sti

          false ->
            add_error(l, {:unbound_var, k}, sti)
        end

      {:map_field_exact, _L, _K, _V}, sti ->
        sti
    end

    foldl(fun, foldl(fun, st0, ps1), ps2)
  end

  defp reject_invalid_alias({:cons, _, h1, t1}, {:cons, _, h2, t2}, vt, st0) do
    st = reject_invalid_alias(h1, h2, vt, st0)
    reject_invalid_alias(t1, t2, vt, st)
  end

  defp reject_invalid_alias({:tuple, _, es1}, {:tuple, _, es2}, vt, st) do
    reject_invalid_alias_list(es1, es2, vt, st)
  end

  defp reject_invalid_alias(
         {:record, _, name1, pfs1},
         {:record, _, name2, pfs2},
         vt,
         r_lint(records: recs) = st
       ) do
    case recs do
      %{^name1 => {_Line1, fields1}, ^name2 => {_Line2, fields2}} ->
        reject_invalid_alias_rec(pfs1, pfs2, fields1, fields2, vt, st)

      %{} ->
        st
    end
  end

  defp reject_invalid_alias({:match, _, p1, p2}, p, vt, st0) do
    st = reject_invalid_alias(p1, p, vt, st0)
    reject_invalid_alias(p2, p, vt, st)
  end

  defp reject_invalid_alias(p, {:match, _, _, _} = m, vt, st) do
    reject_invalid_alias(m, p, vt, st)
  end

  defp reject_invalid_alias(_P1, _P2, _Vt, st) do
    st
  end

  defp reject_invalid_alias_list([e1 | es1], [e2 | es2], vt, st0) do
    st = reject_invalid_alias(e1, e2, vt, st0)
    reject_invalid_alias_list(es1, es2, vt, st)
  end

  defp reject_invalid_alias_list(_, _, _, st) do
    st
  end

  defp reject_invalid_alias_rec(pfsA0, pfsB0, fieldsA0, fieldsB0, vt, st) do
    pfsA1 = rbia_field_vars(pfsA0)
    pfsB1 = rbia_field_vars(pfsB0)
    fieldsA1 = rbia_fields(:lists.reverse(fieldsA0), 0, [])
    fieldsB1 = rbia_fields(:lists.reverse(fieldsB0), 0, [])
    fieldsA = :sofs.relation(fieldsA1)
    pfsA = :sofs.relation(pfsA1)
    a = :sofs.join(fieldsA, 1, pfsA, 1)
    fieldsB = :sofs.relation(fieldsB1)
    pfsB = :sofs.relation(pfsB1)
    b = :sofs.join(fieldsB, 1, pfsB, 1)
    c = :sofs.join(a, 2, b, 2)

    d =
      :sofs.projection(
        {:external,
         fn {_, _, p1, _, p2} ->
           {p1, p2}
         end},
        c
      )

    e = :sofs.to_external(d)
    {ps1, ps2} = :lists.unzip(e)
    reject_invalid_alias_list(ps1, ps2, vt, st)
  end

  defp rbia_field_vars(fs) do
    for {:record_field, _, {:atom, _, name}, pat} <- fs do
      {name, pat}
    end
  end

  defp rbia_fields([{:record_field, _, {:atom, _, name}, _} | fs], i, acc) do
    rbia_fields(fs, i + 1, [{name, i} | acc])
  end

  defp rbia_fields([_ | fs], i, acc) do
    rbia_fields(fs, i + 1, acc)
  end

  defp rbia_fields([], _, acc) do
    acc
  end

  def is_pattern_expr(expr) do
    case is_pattern_expr_1(expr) do
      false ->
        false

      true ->
        case :erl_eval.partial_eval(expr) do
          {:integer, _, _} ->
            true

          {:char, _, _} ->
            true

          {:float, _, _} ->
            true

          {:atom, _, _} ->
            true

          _ ->
            false
        end
    end
  end

  defp is_pattern_expr_1({:char, _Line, _C}) do
    true
  end

  defp is_pattern_expr_1({:integer, _Line, _I}) do
    true
  end

  defp is_pattern_expr_1({:float, _Line, _F}) do
    true
  end

  defp is_pattern_expr_1({:atom, _Line, _A}) do
    true
  end

  defp is_pattern_expr_1({:tuple, _Line, es}) do
    all(&is_pattern_expr/1, es)
  end

  defp is_pattern_expr_1({nil, _Line}) do
    true
  end

  defp is_pattern_expr_1({:cons, _Line, h, t}) do
    is_pattern_expr_1(h) and is_pattern_expr_1(t)
  end

  defp is_pattern_expr_1({:op, _Line, op, a}) do
    :erl_internal.arith_op(op, 1) and is_pattern_expr_1(a)
  end

  defp is_pattern_expr_1({:op, _Line, op, a1, a2}) do
    :erl_internal.arith_op(
      op,
      2
    ) and all(&is_pattern_expr/1, [a1, a2])
  end

  defp is_pattern_expr_1(_Other) do
    false
  end

  defp pattern_map(ps, vt, old, st) do
    foldl(
      fn
        {:map_field_assoc, l, _, _}, {psvt, psnew, st0} ->
          {psvt, psnew, add_error(l, :illegal_pattern, st0)}

        {:map_field_exact, _L, k, v}, {psvt, psnew, st0} ->
          st1 = r_lint(st0, gexpr_context: :map_key)
          {kvt, st2} = gexpr(k, vt, st1)
          {vvt, vnew, st3} = pattern(v, vt, old, st2)
          {vtmerge_pat(vtmerge_pat(kvt, vvt), psvt), vtmerge_pat(psnew, vnew), st3}
      end,
      {[], [], st},
      ps
    )
  end

  defp pattern_bin(es, vt, old, st0) do
    {_Sz, esvt, esnew, st1} =
      foldl(
        fn e, acc ->
          pattern_element(e, vt, old, acc)
        end,
        {0, [], [], st0},
        es
      )

    {esvt, esnew, st1}
  end

  defp pattern_element(
         {:bin_element, line, {:string, _, _}, size, ts} = be,
         vt,
         old,
         {sz, esvt, esnew, st0} = acc
       ) do
    case good_string_size_type(size, ts) do
      true ->
        pattern_element_1(be, vt, old, acc)

      false ->
        st = add_error(line, :typed_literal_string, st0)
        {sz, esvt, esnew, st}
    end
  end

  defp pattern_element(be, vt, old, acc) do
    pattern_element_1(be, vt, old, acc)
  end

  defp pattern_element_1({:bin_element, line, e, sz0, ts}, vt, old, {size0, esvt, esnew, st0}) do
    {pevt, penew, st1} = pat_bit_expr(e, old, esnew, st0)
    {sz1, szvt, sznew, st2} = pat_bit_size(sz0, vt, esnew, st1)
    {sz2, bt, st3} = bit_type(line, sz1, ts, st2)
    {sz3, st4} = bit_size_check(line, sz2, bt, st3)

    sz4 =
      case {e, sz3} do
        {{:string, _, s}, :all} ->
          8 * length(s)

        {_, _} ->
          sz3
      end

    {size1, st5} = add_bit_size(line, sz4, size0, false, st4)
    {size1, vtmerge(szvt, vtmerge(pevt, esvt)), vtmerge(sznew, vtmerge(esnew, penew)), st5}
  end

  defp good_string_size_type(:default, :default) do
    true
  end

  defp good_string_size_type(:default, ts) do
    :lists.any(
      fn
        :utf8 ->
          true

        :utf16 ->
          true

        :utf32 ->
          true

        _ ->
          false
      end,
      ts
    )
  end

  defp good_string_size_type(_, _) do
    false
  end

  defp pat_bit_expr({:var, _, :_}, _Old, _New, st) do
    {[], [], st}
  end

  defp pat_bit_expr({:var, ln, v}, old, new, st) do
    pat_var(v, ln, old, new, st)
  end

  defp pat_bit_expr({:string, _, _}, _Old, _new, st) do
    {[], [], st}
  end

  defp pat_bit_expr({:bin, l, _}, _Old, _New, st) do
    {[], [], add_error(l, :illegal_pattern, st)}
  end

  defp pat_bit_expr(p, _Old, _New, st) do
    case is_pattern_expr(p) do
      true ->
        {[], [], st}

      false ->
        {[], [], add_error(:erlang.element(2, p), :illegal_pattern, st)}
    end
  end

  defp pat_bit_size(:default, _Vt, _New, st) do
    {:default, [], [], st}
  end

  defp pat_bit_size({:var, lv, v}, vt0, new0, st0) do
    {vt, new, st1} = pat_binsize_var(v, lv, vt0, new0, st0)
    {:unknown, vt, new, st1}
  end

  defp pat_bit_size(size, vt0, new0, st0) do
    line = :erlang.element(2, size)

    case :erl_eval.partial_eval(size) do
      {:integer, ^line, i} ->
        {i, [], [], st0}

      expr ->
        st1 = r_lint(st0, bvt: new0, gexpr_context: :bin_seg_size)
        {vt, r_lint(bvt: new) = st2} = gexpr(size, vt0, st1)

        st3 =
          r_lint(st2,
            bvt: :none,
            gexpr_context: r_lint(st0, :gexpr_context)
          )

        st =
          case is_bit_size_illegal(expr) do
            true ->
              add_warning(line, :non_integer_bitsize, st3)

            false ->
              st3
          end

        {:unknown, vt, new, st}
    end
  end

  defp is_bit_size_illegal({:atom, _, _}) do
    true
  end

  defp is_bit_size_illegal({:bin, _, _}) do
    true
  end

  defp is_bit_size_illegal({:cons, _, _, _}) do
    true
  end

  defp is_bit_size_illegal({:float, _, _}) do
    true
  end

  defp is_bit_size_illegal({:map, _, _}) do
    true
  end

  defp is_bit_size_illegal({nil, _}) do
    true
  end

  defp is_bit_size_illegal({:tuple, _, _}) do
    true
  end

  defp is_bit_size_illegal(_) do
    false
  end

  defp expr_bin(es, vt, st0, check) do
    {_Sz, esvt, st1} =
      foldl(
        fn e, acc ->
          bin_element(e, vt, acc, check)
        end,
        {0, [], st0},
        es
      )

    {esvt, st1}
  end

  defp bin_element({:bin_element, line, e, sz0, ts}, vt, {size0, esvt, st0}, check) do
    {vt1, st1} = check.(e, vt, st0)
    {sz1, vt2, st2} = bit_size(sz0, vt, st1, check)
    {sz2, bt, st3} = bit_type(line, sz1, ts, st2)
    {sz3, st4} = bit_size_check(line, sz2, bt, st3)
    {size1, st5} = add_bit_size(line, sz3, size0, true, st4)
    {size1, vtmerge([vt2, vt1, esvt]), st5}
  end

  defp bit_size(:default, _Vt, st, _Check) do
    {:default, [], st}
  end

  defp bit_size({:atom, _Line, :all}, _Vt, st, _Check) do
    {:all, [], st}
  end

  defp bit_size(size, vt, st, check) do
    info = is_guard_test2_info(st)

    case is_gexpr(size, info) do
      true ->
        case :erl_eval.partial_eval(size) do
          {:integer, _ILn, i} ->
            {i, [], st}

          _Other ->
            {evt, st1} = check.(size, vt, st)
            {:unknown, evt, st1}
        end

      false ->
        {evt, st1} = check.(size, vt, st)
        {:unknown, evt, st1}
    end
  end

  defp bit_type(line, size0, type, st) do
    case :erl_bits.set_bit_type(size0, type) do
      {:ok, size1, bt} ->
        {size1, bt, st}

      {:error, what} ->
        {:ok, size1, bt} = :erl_bits.set_bit_type(:default, [])
        {size1, bt, add_error(line, what, st)}
    end
  end

  defp bit_size_check(_Line, :unknown, _, st) do
    {:unknown, st}
  end

  defp bit_size_check(_Line, :undefined, r_bittype(type: type), st) do
    true =
      :erlang.or(
        :erlang.or(
          type === :utf8,
          type === :utf16
        ),
        type === :utf32
      )

    {:undefined, st}
  end

  defp bit_size_check(line, :all, r_bittype(type: type), st) do
    case type do
      :binary ->
        {:all, st}

      _ ->
        {:unknown, add_error(line, :illegal_bitsize, st)}
    end
  end

  defp bit_size_check(line, size, r_bittype(type: type, unit: unit), st) do
    sz = unit * size
    st2 = elemtype_check(line, type, sz, st)
    {sz, st2}
  end

  defp elemtype_check(_Line, :float, 32, st) do
    st
  end

  defp elemtype_check(_Line, :float, 64, st) do
    st
  end

  defp elemtype_check(line, :float, _Size, st) do
    add_warning(line, {:bad_bitsize, 'float'}, st)
  end

  defp elemtype_check(_Line, _Type, _Size, st) do
    st
  end

  defp add_bit_size(line, _Sz1, :all, false, st) do
    {:all, add_error(line, :unsized_binary_not_at_end, st)}
  end

  defp add_bit_size(_Line, _Sz1, :all, true, st) do
    {:all, st}
  end

  defp add_bit_size(_Line, :all, _Sz2, _B, st) do
    {:all, st}
  end

  defp add_bit_size(_Line, :undefined, _Sz2, _B, st) do
    {:undefined, st}
  end

  defp add_bit_size(_Line, :unknown, _Sz2, _B, st) do
    {:unknown, st}
  end

  defp add_bit_size(_Line, _Sz1, :undefined, _B, st) do
    {:unknown, st}
  end

  defp add_bit_size(_Line, _Sz1, :unknown, _B, st) do
    {:unknown, st}
  end

  defp add_bit_size(_Line, sz1, sz2, _B, st) do
    {sz1 + sz2, st}
  end

  defp guard([l | r], vt, st0) when is_list(l) do
    {gvt, st1} = guard_tests(l, vt, st0)
    {gsvt, st2} = guard(r, vtupdate(gvt, vt), st1)
    {vtupdate(gvt, gsvt), st2}
  end

  defp guard(l, vt, st0) do
    guard_tests(l, vt, st0)
  end

  defp guard_tests([g | gs], vt, st0) do
    {gvt, st1} = guard_test(g, vt, st0)
    {gsvt, st2} = guard_tests(gs, vtupdate(gvt, vt), st1)
    {vtupdate(gvt, gsvt), st2}
  end

  defp guard_tests([], _Vt, st) do
    {[], st}
  end

  defp guard_test(g, vt, st0) do
    st1 = obsolete_guard(g, st0)
    guard_test2(g, vt, st1)
  end

  defp guard_test2({:call, line, {:atom, lr, :record}, [e, a]}, vt, st0) do
    gexpr({:call, line, {:atom, lr, :is_record}, [e, a]}, vt, st0)
  end

  defp guard_test2({:call, line, {:atom, _La, f}, as} = g, vt, st0) do
    {asvt, st1} = gexpr_list(as, vt, st0)
    a = length(as)

    case :erl_internal.type_test(f, a) do
      true when f !== :is_record and a !== 2 ->
        case no_guard_bif_clash(st1, {f, a}) do
          false ->
            {asvt, add_error(line, {:illegal_guard_local_call, {f, a}}, st1)}

          true ->
            {asvt, st1}
        end

      _ ->
        gexpr(g, vt, st0)
    end
  end

  defp guard_test2(g, vt, st) do
    gexpr(g, vt, st)
  end

  defp gexpr({:var, line, v}, vt, st) do
    expr_var(v, line, vt, st)
  end

  defp gexpr({:char, _Line, _C}, _Vt, st) do
    {[], st}
  end

  defp gexpr({:integer, _Line, _I}, _Vt, st) do
    {[], st}
  end

  defp gexpr({:float, _Line, _F}, _Vt, st) do
    {[], st}
  end

  defp gexpr({:atom, line, a}, _Vt, st) do
    {[], keyword_warning(line, a, st)}
  end

  defp gexpr({:string, _Line, _S}, _Vt, st) do
    {[], st}
  end

  defp gexpr({nil, _Line}, _Vt, st) do
    {[], st}
  end

  defp gexpr({:cons, _Line, h, t}, vt, st) do
    gexpr_list([h, t], vt, st)
  end

  defp gexpr({:tuple, _Line, es}, vt, st) do
    gexpr_list(es, vt, st)
  end

  defp gexpr({:map, _Line, es}, vt, st) do
    map_fields(es, vt, check_assoc_fields(es, st), &gexpr_list/3)
  end

  defp gexpr({:map, _Line, src, es}, vt, st) do
    {svt, st1} = gexpr(src, vt, st)
    {fvt, st2} = map_fields(es, vt, st1, &gexpr_list/3)
    {vtmerge(svt, fvt), st2}
  end

  defp gexpr({:record_index, line, name, field}, _Vt, st) do
    check_record(line, name, st, fn dfs, st1 ->
      record_field(field, name, dfs, st1)
    end)
  end

  defp gexpr({:record_field, line, rec, name, field}, vt, st0) do
    {rvt, st1} = gexpr(rec, vt, st0)

    {fvt, st2} =
      check_record(line, name, st1, fn dfs, st ->
        record_field(field, name, dfs, st)
      end)

    {vtmerge(rvt, fvt), st2}
  end

  defp gexpr({:record, line, name, inits}, vt, st) do
    check_record(line, name, st, fn dfs, st1 ->
      ginit_fields(inits, line, name, dfs, vt, st1)
    end)
  end

  defp gexpr({:bin, _Line, fs}, vt, st) do
    expr_bin(fs, vt, st, &gexpr/3)
  end

  defp gexpr({:call, _Line, {:atom, _Lr, :is_record}, [e, {:atom, ln, name}]}, vt, st0) do
    {rvt, st1} = gexpr(e, vt, st0)
    {rvt, exist_record(ln, name, st1)}
  end

  defp gexpr({:call, line, {:atom, _Lr, :is_record}, [e, r]}, vt, st0) do
    {asvt, st1} = gexpr_list([e, r], vt, st0)
    {asvt, add_error(line, :illegal_guard_expr, st1)}
  end

  defp gexpr(
         {:call, line, {:remote, _Lr, {:atom, _Lm, :erlang}, {:atom, lf, :is_record}}, [e, a]},
         vt,
         st0
       ) do
    gexpr({:call, line, {:atom, lf, :is_record}, [e, a]}, vt, st0)
  end

  defp gexpr(
         {:call, line, {:atom, _Lr, :is_record}, [e0, {:atom, _, _Name}, {:integer, _, _}]},
         vt,
         st0
       ) do
    {e, st1} = gexpr(e0, vt, st0)

    case no_guard_bif_clash(st0, {:is_record, 3}) do
      true ->
        {e, st1}

      false ->
        {e, add_error(line, {:illegal_guard_local_call, {:is_record, 3}}, st1)}
    end
  end

  defp gexpr({:call, line, {:atom, _Lr, :is_record}, [_, _, _] = asvt0}, vt, st0) do
    {asvt, st1} = gexpr_list(asvt0, vt, st0)
    {asvt, add_error(line, :illegal_guard_expr, st1)}
  end

  defp gexpr(
         {:call, line, {:remote, _, {:atom, _, :erlang}, {:atom, _, :is_record} = isr},
          [_, _, _] = args},
         vt,
         st0
       ) do
    gexpr({:call, line, isr, args}, vt, st0)
  end

  defp gexpr({:call, line, {:atom, _La, f}, as}, vt, st0) do
    {asvt, st1} = gexpr_list(as, vt, st0)
    a = length(as)

    case :erl_internal.guard_bif(
           f,
           a
         ) and no_guard_bif_clash(st1, {f, a}) do
      true ->
        true = :erl_internal.bif(f, a)
        {asvt, st1}

      false ->
        case is_local_function(
               r_lint(st1, :locals),
               {f, a}
             ) or
               is_imported_function(
                 r_lint(st1, :imports),
                 {f, a}
               ) do
          true ->
            {asvt, add_error(line, {:illegal_guard_local_call, {f, a}}, st1)}

          _ ->
            {asvt, add_error(line, :illegal_guard_expr, st1)}
        end
    end
  end

  defp gexpr({:call, line, {:remote, _Lr, {:atom, _Lm, :erlang}, {:atom, _Lf, f}}, as}, vt, st0) do
    {asvt, st1} = gexpr_list(as, vt, st0)
    a = length(as)

    case :erl_internal.guard_bif(f, a) or
           is_gexpr_op(
             f,
             a
           ) do
      true ->
        {asvt, st1}

      false ->
        {asvt, add_error(line, :illegal_guard_expr, st1)}
    end
  end

  defp gexpr({:op, line, op, a}, vt, st0) do
    {avt, st1} = gexpr(a, vt, st0)

    case is_gexpr_op(op, 1) do
      true ->
        {avt, st1}

      false ->
        {avt, add_error(line, :illegal_guard_expr, st1)}
    end
  end

  defp gexpr({:op, _, :andalso, l, r}, vt, st) do
    gexpr_list([l, r], vt, st)
  end

  defp gexpr({:op, _, :orelse, l, r}, vt, st) do
    gexpr_list([l, r], vt, st)
  end

  defp gexpr({:op, line, op, l, r}, vt, st0) do
    {avt, st1} = gexpr_list([l, r], vt, st0)

    case is_gexpr_op(op, 2) do
      true ->
        {avt, st1}

      false ->
        {avt, add_error(line, :illegal_guard_expr, st1)}
    end
  end

  defp gexpr(e, _Vt, st) do
    {[], add_error(:erlang.element(2, e), :illegal_guard_expr, st)}
  end

  defp gexpr_list(es, vt, st) do
    foldl(
      fn e, {esvt, st0} ->
        {evt, st1} = gexpr(e, vt, st0)
        {vtmerge(evt, esvt), st1}
      end,
      {[], st},
      es
    )
  end

  def is_guard_test(e) do
    is_guard_test2(
      e,
      {:maps.new(),
       fn _ ->
         false
       end}
    )
  end

  def is_guard_test(expression, forms) do
    is_guard_test(expression, forms, fn _ ->
      false
    end)
  end

  def is_guard_test(expression, forms, isOverridden) do
    recordAttributes =
      for a = {:attribute, _, :record, _D} <- forms do
        a
      end

    st0 =
      foldl(
        fn attr0, st1 ->
          attr = set_file(attr0, 'none')
          attribute_state(attr, st1)
        end,
        start(),
        recordAttributes
      )

    is_guard_test2(
      set_file(expression, 'nofile'),
      {r_lint(st0, :records), isOverridden}
    )
  end

  defp is_guard_test2(
         {:call, line, {:atom, lr, :record}, [e, a]},
         info
       ) do
    is_gexpr(
      {:call, line, {:atom, lr, :is_record}, [e, a]},
      info
    )
  end

  defp is_guard_test2(
         {:call, _Line, {:atom, _La, test}, as} = call,
         {_, isOverridden} = info
       ) do
    a = length(as)

    not isOverridden.({test, a}) and
      case :erl_internal.type_test(test, a) do
        true ->
          is_gexpr_list(as, info)

        false ->
          is_gexpr(call, info)
      end
  end

  defp is_guard_test2(g, info) do
    is_gexpr(g, info)
  end

  def is_guard_expr(e) do
    is_gexpr(
      e,
      {[],
       fn {_, _} ->
         false
       end}
    )
  end

  defp is_gexpr({:var, _L, _V}, _Info) do
    true
  end

  defp is_gexpr({:char, _L, _C}, _Info) do
    true
  end

  defp is_gexpr({:integer, _L, _I}, _Info) do
    true
  end

  defp is_gexpr({:float, _L, _F}, _Info) do
    true
  end

  defp is_gexpr({:atom, _L, _A}, _Info) do
    true
  end

  defp is_gexpr({:string, _L, _S}, _Info) do
    true
  end

  defp is_gexpr({nil, _L}, _Info) do
    true
  end

  defp is_gexpr({:cons, _L, h, t}, info) do
    is_gexpr_list([h, t], info)
  end

  defp is_gexpr({:tuple, _L, es}, info) do
    is_gexpr_list(es, info)
  end

  defp is_gexpr({:map, _L, es}, info) do
    is_map_fields(es, info)
  end

  defp is_gexpr({:map, _L, src, es}, info) do
    is_gexpr(src, info) and is_map_fields(es, info)
  end

  defp is_gexpr({:record_index, _L, _Name, field}, info) do
    is_gexpr(field, info)
  end

  defp is_gexpr({:record_field, _L, rec, _Name, field}, info) do
    is_gexpr_list([rec, field], info)
  end

  defp is_gexpr({:record, l, name, inits}, info) do
    is_gexpr_fields(inits, l, name, info)
  end

  defp is_gexpr({:bin, _L, fs}, info) do
    all(
      fn {:bin_element, _Line, e, sz, _Ts} ->
        :erlang.and(
          is_gexpr(e, info),
          sz === :default or is_gexpr(sz, info)
        )
      end,
      fs
    )
  end

  defp is_gexpr(
         {:call, _L, {:atom, _Lf, f}, as},
         {_, isOverridden} = info
       ) do
    a = length(as)

    not isOverridden.({f, a}) and
      :erl_internal.guard_bif(
        f,
        a
      ) and
      is_gexpr_list(
        as,
        info
      )
  end

  defp is_gexpr(
         {:call, _L, {:remote, _Lr, {:atom, _Lm, :erlang}, {:atom, _Lf, f}}, as},
         info
       ) do
    a = length(as)

    (:erl_internal.guard_bif(f, a) or
       is_gexpr_op(
         f,
         a
       )) and
      is_gexpr_list(
        as,
        info
      )
  end

  defp is_gexpr(
         {:call, l, {:tuple, lt, [{:atom, lm, :erlang}, {:atom, lf, f}]}, as},
         info
       ) do
    is_gexpr(
      {:call, l, {:remote, lt, {:atom, lm, :erlang}, {:atom, lf, f}}, as},
      info
    )
  end

  defp is_gexpr({:op, _L, op, a}, info) do
    is_gexpr_op(op, 1) and is_gexpr(a, info)
  end

  defp is_gexpr({:op, _L, :andalso, a1, a2}, info) do
    is_gexpr_list([a1, a2], info)
  end

  defp is_gexpr({:op, _L, :orelse, a1, a2}, info) do
    is_gexpr_list([a1, a2], info)
  end

  defp is_gexpr({:op, _L, op, a1, a2}, info) do
    is_gexpr_op(op, 2) and is_gexpr_list([a1, a2], info)
  end

  defp is_gexpr(_Other, _Info) do
    false
  end

  defp is_gexpr_op(op, a) do
    try do
      :erl_internal.op_type(op, a)
    catch
      _, _ ->
        false
    else
      :arith ->
        true

      :bool ->
        true

      :comp ->
        true

      :list ->
        false

      :send ->
        false
    end
  end

  defp is_gexpr_list(es, info) do
    all(
      fn e ->
        is_gexpr(e, info)
      end,
      es
    )
  end

  defp is_map_fields([{tag, _, k, v} | fs], info)
       when tag === :map_field_assoc or
              tag === :map_field_exact do
    is_gexpr(k, info) and
      is_gexpr(
        v,
        info
      ) and is_map_fields(fs, info)
  end

  defp is_map_fields([], _Info) do
    true
  end

  defp is_map_fields(_T, _Info) do
    false
  end

  defp is_gexpr_fields(fs, l, name, {rDs, _} = info) do
    iFs =
      case :maps.find(name, rDs) do
        {:ok, {_Line, fields}} ->
          fs ++ init_fields(fs, l, fields)

        :error ->
          fs
      end

    all(
      fn
        {:record_field, _Lf, _Name, v} ->
          is_gexpr(v, info)

        _Other ->
          false
      end,
      iFs
    )
  end

  defp exprs([e | es], vt, st0) do
    {evt, st1} = expr(e, vt, st0)
    {esvt, st2} = exprs(es, vtupdate(evt, vt), st1)
    {vtupdate(evt, esvt), st2}
  end

  defp exprs([], _Vt, st) do
    {[], st}
  end

  defp expr({:var, line, v}, vt, st) do
    expr_var(v, line, vt, st)
  end

  defp expr({:char, _Line, _C}, _Vt, st) do
    {[], st}
  end

  defp expr({:integer, _Line, _I}, _Vt, st) do
    {[], st}
  end

  defp expr({:float, _Line, _F}, _Vt, st) do
    {[], st}
  end

  defp expr({:atom, line, a}, _Vt, st) do
    {[], keyword_warning(line, a, st)}
  end

  defp expr({:string, _Line, _S}, _Vt, st) do
    {[], st}
  end

  defp expr({nil, _Line}, _Vt, st) do
    {[], st}
  end

  defp expr({:cons, _Line, h, t}, vt, st) do
    expr_list([h, t], vt, st)
  end

  defp expr({:lc, _Line, e, qs}, vt, st) do
    handle_comprehension(e, qs, vt, st)
  end

  defp expr({:bc, _Line, e, qs}, vt, st) do
    handle_comprehension(e, qs, vt, st)
  end

  defp expr({:tuple, _Line, es}, vt, st) do
    expr_list(es, vt, st)
  end

  defp expr({:map, _Line, es}, vt, st) do
    map_fields(es, vt, check_assoc_fields(es, st), &expr_list/3)
  end

  defp expr({:map, _Line, src, es}, vt, st) do
    {svt, st1} = expr(src, vt, st)
    {fvt, st2} = map_fields(es, vt, st1, &expr_list/3)
    {vtupdate(svt, fvt), st2}
  end

  defp expr({:record_index, line, name, field}, _Vt, st) do
    check_record(line, name, st, fn dfs, st1 ->
      record_field(field, name, dfs, st1)
    end)
  end

  defp expr({:record, line, name, inits}, vt, st) do
    check_record(line, name, st, fn dfs, st1 ->
      init_fields(inits, line, name, dfs, vt, st1)
    end)
  end

  defp expr({:record_field, line, rec, name, field}, vt, st0) do
    {rvt, st1} = record_expr(line, rec, vt, st0)

    {fvt, st2} =
      check_record(line, name, st1, fn dfs, st ->
        record_field(field, name, dfs, st)
      end)

    {vtmerge(rvt, fvt), st2}
  end

  defp expr({:record, line, rec, name, upds}, vt, st0) do
    {rvt, st1} = record_expr(line, rec, vt, st0)

    {usvt, st2} =
      check_record(line, name, st1, fn dfs, st ->
        update_fields(upds, name, dfs, vt, st)
      end)

    case has_wildcard_field(upds) do
      true ->
        {[], add_error(line, {:wildcard_in_update, name}, st2)}

      false ->
        {vtmerge(rvt, usvt), st2}
    end
  end

  defp expr({:bin, _Line, fs}, vt, st) do
    expr_bin(fs, vt, st, &expr/3)
  end

  defp expr({:block, _Line, es}, vt, st) do
    exprs(es, vt, st)
  end

  defp expr({:if, line, cs}, vt, st) do
    icrt_clauses(cs, {:if, line}, vt, st)
  end

  defp expr({:case, line, e, cs}, vt, st0) do
    {evt, st1} = expr(e, vt, st0)
    {cvt, st2} = icrt_clauses(cs, {:case, line}, vtupdate(evt, vt), st1)
    {vtmerge(evt, cvt), st2}
  end

  defp expr({:receive, line, cs}, vt, st) do
    icrt_clauses(cs, {:receive, line}, vt, st)
  end

  defp expr({:receive, line, cs, to, toEs}, vt, st0) do
    {tvt, st1} = expr(to, vt, st0)
    {tevt, st2} = exprs(toEs, vt, st1)
    {cvt, st3} = icrt_clauses(cs, vt, st2)
    csvts = [tevt | cvt]
    rvt = icrt_export(csvts, vt, {:receive, line}, st3)
    {vtmerge([tvt, tevt, rvt]), st3}
  end

  defp expr({:fun, line, body}, vt, st) do
    case body do
      {:clauses, cs} ->
        fun_clauses(cs, vt, st)

      {:function, :record_info, 2} ->
        {[], add_error(line, :illegal_record_info, st)}

      {:function, f, a} ->
        case not is_local_function(
               r_lint(st, :locals),
               {f, a}
             ) and :erl_internal.bif(f, a) and
               not is_autoimport_suppressed(
                 r_lint(st, :no_auto),
                 {f, a}
               ) do
          true ->
            {[], st}

          false ->
            {[], call_function(line, f, a, st)}
        end

      {:function, m, f, a} ->
        expr_list([m, f, a], vt, st)
    end
  end

  defp expr({:named_fun, _, :_, cs}, vt, st) do
    fun_clauses(cs, vt, st)
  end

  defp expr({:named_fun, line, name, cs}, vt, st0) do
    nvt0 = [{name, {:bound, :unused, [line]}}]
    st1 = shadow_vars(nvt0, vt, :"named fun", st0)
    nvt1 = vtupdate(vtsubtract(vt, nvt0), nvt0)
    {csvt, st2} = fun_clauses(cs, nvt1, st1)
    {_, st3} = check_unused_vars(vtupdate(csvt, nvt0), [], st2)
    {vtold(csvt, vt), st3}
  end

  defp expr({:call, _Line, {:atom, _Lr, :is_record}, [e, {:atom, ln, name}]}, vt, st0) do
    {rvt, st1} = expr(e, vt, st0)
    {rvt, exist_record(ln, name, st1)}
  end

  defp expr(
         {:call, line, {:remote, _Lr, {:atom, _Lm, :erlang}, {:atom, lf, :is_record}}, [e, a]},
         vt,
         st0
       ) do
    expr({:call, line, {:atom, lf, :is_record}, [e, a]}, vt, st0)
  end

  defp expr({:call, l, {:tuple, lt, [{:atom, lm, :erlang}, {:atom, lf, :is_record}]}, as}, vt, st) do
    expr({:call, l, {:remote, lt, {:atom, lm, :erlang}, {:atom, lf, :is_record}}, as}, vt, st)
  end

  defp expr({:call, line, {:remote, _Lr, {:atom, _Lm, m}, {:atom, lf, f}}, as}, vt, st0) do
    st1 = keyword_warning(lf, f, st0)
    st2 = check_remote_function(line, m, f, as, st1)
    st3 = check_module_name(m, line, st2)
    expr_list(as, vt, st3)
  end

  defp expr({:call, line, {:remote, _Lr, m, f}, as}, vt, st0) do
    st1 = keyword_warning(line, m, st0)
    st2 = keyword_warning(line, f, st1)

    st3 =
      case m do
        {:atom, lm, mod} ->
          check_module_name(mod, lm, st2)

        _ ->
          st2
      end

    expr_list([m, f | as], vt, st3)
  end

  defp expr({:call, line, {:atom, la, f}, as}, vt, st0) do
    st1 = keyword_warning(la, f, st0)
    {asvt, st2} = expr_list(as, vt, st1)
    a = length(as)
    isLocal = is_local_function(r_lint(st2, :locals), {f, a})
    isAutoBif = :erl_internal.bif(f, a)

    autoSuppressed =
      is_autoimport_suppressed(
        r_lint(st2, :no_auto),
        {f, a}
      )

    warn =
      :erlang.and(
        is_warn_enabled(:bif_clash, st2),
        not bif_clash_specifically_disabled(st2, {f, a})
      )

    imported = imported(f, a, st2)

    case not isLocal and imported === :no and isAutoBif and not autoSuppressed do
      true ->
        st3 = deprecated_function(line, :erlang, f, as, st2)
        {asvt, st3}

      false ->
        {asvt,
         case imported do
           {:yes, m} ->
             st3 = check_remote_function(line, m, f, as, st2)
             u0 = r_lint(st3, :usage)

             imp =
               :ordsets.add_element(
                 {{f, a}, m},
                 r_usage(u0, :imported)
               )

             r_lint(st3, usage: r_usage(u0, imported: imp))

           :no ->
             case {f, a} do
               {:record_info, 2} ->
                 check_record_info_call(line, la, as, st2)

               n ->
                 st3 =
                   cond do
                     not autoSuppressed and isAutoBif and warn ->
                       case :erl_internal.old_bif(f, a) do
                         true ->
                           add_error(
                             line,
                             {:call_to_redefined_old_bif, {f, a}},
                             st2
                           )

                         false ->
                           add_warning(
                             line,
                             {:call_to_redefined_bif, {f, a}},
                             st2
                           )
                       end

                     true ->
                       st2
                   end

                 cond do
                   n === r_lint(st3, :func) ->
                     st3

                   true ->
                     call_function(line, f, a, st3)
                 end
             end
         end}
    end
  end

  defp expr({:call, line, f, as}, vt, st0) do
    st = warn_invalid_call(line, f, st0)
    expr_list([f | as], vt, st)
  end

  defp expr({:try, line, es, scs, ccs, as}, vt, st0) do
    {evt0, st1} = exprs(es, vt, st0)
    tryLine = {:try, line}
    uvt = vtunsafe(tryLine, evt0, vt)
    {sccs, st2} = try_clauses(scs, ccs, tryLine, vtupdate(evt0, vt), uvt, st1)
    evt1 = vtupdate(uvt, evt0)
    rvt0 = sccs
    rvt1 = vtupdate(vtunsafe(tryLine, rvt0, vt), rvt0)
    evt2 = vtmerge(evt1, rvt1)
    {avt0, st} = exprs(as, vtupdate(evt2, vt), st2)
    avt1 = vtupdate(vtunsafe(tryLine, avt0, vt), avt0)
    avt = vtmerge(evt2, avt1)
    {avt, st}
  end

  defp expr({:catch, line, e}, vt, st0) do
    {evt, st} = expr(e, vt, st0)
    {vtupdate(vtunsafe({:catch, line}, evt, vt), evt), st}
  end

  defp expr({:match, _Line, p, e}, vt, st0) do
    {evt, st1} = expr(e, vt, st0)
    {pvt, pnew, st2} = pattern(p, vtupdate(evt, vt), st1)
    st = reject_invalid_alias_expr(p, e, vt, st2)
    {vtupdate(pnew, vtmerge(evt, pvt)), st}
  end

  defp expr({:op, _Line, _Op, a}, vt, st) do
    expr(a, vt, st)
  end

  defp expr({:op, line, op, l, r}, vt, st0)
       when op === :orelse or op === :andalso do
    {evt1, st1} = expr(l, vt, st0)
    vt1 = vtupdate(evt1, vt)
    {evt2, st2} = expr(r, vt1, st1)
    evt3 = vtupdate(vtunsafe({op, line}, evt2, vt1), evt2)
    {vtmerge(evt1, evt3), st2}
  end

  defp expr({:op, _Line, _Op, l, r}, vt, st) do
    expr_list([l, r], vt, st)
  end

  defp expr({:remote, line, _M, _F}, _Vt, st) do
    {[], add_error(line, :illegal_expr, st)}
  end

  defp expr_list(es, vt, st) do
    foldl(
      fn e, {esvt, st0} ->
        {evt, st1} = expr(e, vt, st0)
        {vtmerge_pat(evt, esvt), st1}
      end,
      {[], st},
      es
    )
  end

  defp record_expr(line, rec, vt, st0) do
    st1 = warn_invalid_record(line, rec, st0)
    expr(rec, vt, st1)
  end

  defp check_assoc_fields([{:map_field_exact, line, _, _} | fs], st) do
    check_assoc_fields(
      fs,
      add_error(line, :illegal_map_construction, st)
    )
  end

  defp check_assoc_fields([{:map_field_assoc, _, _, _} | fs], st) do
    check_assoc_fields(fs, st)
  end

  defp check_assoc_fields([], st) do
    st
  end

  defp map_fields([{tag, _, k, v} | fs], vt, st, f)
       when tag === :map_field_assoc or
              tag === :map_field_exact do
    {pvt, st2} = f.([k, v], vt, st)
    {vts, st3} = map_fields(fs, vt, st2, f)
    {vtupdate(pvt, vts), st3}
  end

  defp map_fields([], _, st, _) do
    {[], st}
  end

  defp warn_invalid_record(line, r, st) do
    case is_valid_record(r) do
      true ->
        st

      false ->
        add_warning(line, :invalid_record, st)
    end
  end

  defp is_valid_record(rec) do
    case rec do
      {:char, _, _} ->
        false

      {:integer, _, _} ->
        false

      {:float, _, _} ->
        false

      {:atom, _, _} ->
        false

      {:string, _, _} ->
        false

      {:cons, _, _, _} ->
        false

      {nil, _} ->
        false

      {:lc, _, _, _} ->
        false

      {:record_index, _, _, _} ->
        false

      {:fun, _, _} ->
        false

      {:named_fun, _, _, _} ->
        false

      _ ->
        true
    end
  end

  defp warn_invalid_call(line, f, st) do
    case is_valid_call(f) do
      true ->
        st

      false ->
        add_warning(line, :invalid_call, st)
    end
  end

  defp is_valid_call(call) do
    case call do
      {:char, _, _} ->
        false

      {:integer, _, _} ->
        false

      {:float, _, _} ->
        false

      {:string, _, _} ->
        false

      {:cons, _, _, _} ->
        false

      {nil, _} ->
        false

      {:lc, _, _, _} ->
        false

      {:record_index, _, _, _} ->
        false

      {:tuple, _, exprs} when length(exprs) !== 2 ->
        false

      _ ->
        true
    end
  end

  defp record_def(line, name, fs0, st0) do
    case :erlang.is_map_key(name, r_lint(st0, :records)) do
      true ->
        add_error(line, {:redefine_record, name}, st0)

      false ->
        {fs1, st1} = def_fields(normalise_fields(fs0), name, st0)
        st2 = r_lint(st1, records: :maps.put(name, {line, fs1}, r_lint(st1, :records)))

        types =
          for {:typed_record_field, _, t} <- fs0 do
            t
          end

        check_type({:type, nowarn(), :product, types}, st2)
    end
  end

  defp def_fields(fs0, name, st0) do
    foldl(
      fn {:record_field, lf, {:atom, la, f}, v}, {fs, st} ->
        case exist_field(f, fs) do
          true ->
            {fs, add_error(lf, {:redefine_field, name, f}, st)}

          false ->
            st1 = r_lint(st, recdef_top: true)
            {_, st2} = expr(v, [], st1)

            st3 =
              r_lint(st1,
                warnings: r_lint(st2, :warnings),
                errors: r_lint(st2, :errors),
                called: r_lint(st2, :called),
                recdef_top: false
              )

            nV =
              case r_lint(st2, :errors) === r_lint(st1, :errors) do
                true ->
                  v

                false ->
                  {:atom, la, :undefined}
              end

            {[{:record_field, lf, {:atom, la, f}, nV} | fs], st3}
        end
      end,
      {[], st0},
      fs0
    )
  end

  defp normalise_fields(fs) do
    map(
      fn
        {:record_field, lf, field} ->
          {:record_field, lf, field, {:atom, lf, :undefined}}

        {:typed_record_field, {:record_field, lf, field}, _Type} ->
          {:record_field, lf, field, {:atom, lf, :undefined}}

        {:typed_record_field, field, _Type} ->
          field

        f ->
          f
      end,
      fs
    )
  end

  defp exist_record(line, name, st) do
    case :erlang.is_map_key(name, r_lint(st, :records)) do
      true ->
        used_record(name, st)

      false ->
        add_error(line, {:undefined_record, name}, st)
    end
  end

  defp check_record(line, name, st, checkFun) do
    case :maps.find(name, r_lint(st, :records)) do
      {:ok, {_Line, fields}} ->
        checkFun.(fields, used_record(name, st))

      :error ->
        {[], add_error(line, {:undefined_record, name}, st)}
    end
  end

  defp used_record(name, r_lint(usage: usage) = st) do
    usedRecs =
      :gb_sets.add_element(
        name,
        r_usage(usage, :used_records)
      )

    r_lint(st, usage: r_usage(usage, used_records: usedRecs))
  end

  defp check_fields(fs, name, fields, vt, st0, checkFun) do
    {_SeenFields, uvt, st1} =
      foldl(
        fn field, {sfsa, vta, sta} ->
          {sfsb, {vtb, stb}} =
            check_field(
              field,
              name,
              fields,
              vt,
              sta,
              sfsa,
              checkFun
            )

          {sfsb, vtmerge_pat(vta, vtb), stb}
        end,
        {[], [], st0},
        fs
      )

    {uvt, st1}
  end

  defp check_field({:record_field, lf, {:atom, la, f}, val}, name, fields, vt, st, sfs, checkFun) do
    case member(f, sfs) do
      true ->
        {sfs, {[], add_error(lf, {:redefine_field, name, f}, st)}}

      false ->
        {[f | sfs],
         case find_field(f, fields) do
           {:ok, _I} ->
             checkFun.(val, vt, st)

           :error ->
             {[], add_error(la, {:undefined_field, name, f}, st)}
         end}
    end
  end

  defp check_field(
         {:record_field, _Lf, {:var, la, :_ = f}, val},
         _Name,
         _Fields,
         vt,
         st,
         sfs,
         checkFun
       ) do
    case member(f, sfs) do
      true ->
        {sfs, {[], add_error(la, :bad_multi_field_init, st)}}

      false ->
        {[f | sfs], checkFun.(val, vt, st)}
    end
  end

  defp check_field(
         {:record_field, _Lf, {:var, la, v}, _Val},
         name,
         _Fields,
         vt,
         st,
         sfs,
         _CheckFun
       ) do
    {sfs, {vt, add_error(la, {:field_name_is_variable, name, v}, st)}}
  end

  defp pattern_field({:atom, la, f}, name, fields, st) do
    case find_field(f, fields) do
      {:ok, _I} ->
        {[], st}

      :error ->
        {[], add_error(la, {:undefined_field, name, f}, st)}
    end
  end

  defp pattern_fields(fs, name, fields, vt0, old, st0) do
    checkFun = fn val, vt, st ->
      pattern(val, vt, old, st)
    end

    {_SeenFields, uvt, unew, st1} =
      foldl(
        fn field, {sfsa, vta, newa, sta} ->
          case check_field(field, name, fields, vt0, sta, sfsa, checkFun) do
            {sfsb, {vtb, stb}} ->
              {sfsb, vtmerge_pat(vta, vtb), [], stb}

            {sfsb, {vtb, newb, stb}} ->
              {sfsb, vtmerge_pat(vta, vtb), vtmerge_pat(newa, newb), stb}
          end
        end,
        {[], [], [], st0},
        fs
      )

    {uvt, unew, st1}
  end

  defp record_field({:atom, la, f}, name, fields, st) do
    case find_field(f, fields) do
      {:ok, _I} ->
        {[], st}

      :error ->
        {[], add_error(la, {:undefined_field, name, f}, st)}
    end
  end

  defp init_fields(ifs, line, name, dfs, vt0, st0) do
    {vt1, st1} = check_fields(ifs, name, dfs, vt0, st0, &expr/3)
    defs = init_fields(ifs, line, dfs)
    {_, st2} = check_fields(defs, name, dfs, vt1, st1, &expr/3)
    {vt1, r_lint(st1, usage: r_lint(st2, :usage))}
  end

  defp ginit_fields(ifs, line, name, dfs, vt0, st0) do
    {vt1, st1} = check_fields(ifs, name, dfs, vt0, st0, &gexpr/3)
    defs = init_fields(ifs, line, dfs)
    st2 = r_lint(st1, errors: [])
    {_, st3} = check_fields(defs, name, dfs, vt1, st2, &gexpr/3)
    r_lint(usage: usage, errors: errors) = st3

    illErrs =
      for {_File, {_Line, :erl_lint, :illegal_guard_expr}} = e <- errors do
        e
      end

    st4 =
      r_lint(st1,
        usage: usage,
        errors: illErrs ++ r_lint(st1, :errors)
      )

    {vt1, st4}
  end

  defp init_fields(ifs, line, dfs) do
    for {:record_field, lf, {:atom, la, f}, di} <- dfs,
        not exist_field(f, ifs) do
      {:record_field, lf, {:atom, la, f}, copy_expr(di, line)}
    end
  end

  defp update_fields(ufs, name, dfs, vt, st) do
    check_fields(ufs, name, dfs, vt, st, &expr/3)
  end

  defp exist_field(
         f,
         [{:record_field, _Lf, {:atom, _La, f}, _Val} | _Fs]
       ) do
    true
  end

  defp exist_field(f, [_ | fs]) do
    exist_field(f, fs)
  end

  defp exist_field(_F, []) do
    false
  end

  defp find_field(
         _F,
         [{:record_field, _Lf, {:atom, _La, _F}, val} | _Fs]
       ) do
    {:ok, val}
  end

  defp find_field(f, [_ | fs]) do
    find_field(f, fs)
  end

  defp find_field(_F, []) do
    :error
  end

  defp type_def(attr, line, typeName, protoType, args, st0) do
    typeDefs = r_lint(st0, :types)
    arity = length(args)
    typePair = {typeName, arity}
    info = r_typeinfo(attr: attr, line: line)

    storeType = fn st ->
      newDefs = :maps.put(typePair, info, typeDefs)
      checkType = {:type, nowarn(), :product, [protoType | args]}
      check_type(checkType, r_lint(st, types: newDefs))
    end

    case is_default_type(typePair) do
      true ->
        case is_obsolete_builtin_type(typePair) do
          true ->
            storeType.(st0)

          false ->
            case is_newly_introduced_builtin_type(typePair) do
              true ->
                warn = {:new_builtin_type, typePair}
                st1 = add_warning(line, warn, st0)
                storeType.(st1)

              false ->
                add_error(line, {:builtin_type, typePair}, st0)
            end
        end

      false ->
        case :erlang.is_map_key(typePair, typeDefs) do
          true ->
            add_error(line, {:redefine_type, typePair}, st0)

          false ->
            st1 =
              case attr === :opaque and
                     is_underspecified(
                       protoType,
                       arity
                     ) do
                true ->
                  warn = {:underspecified_opaque, typePair}
                  add_warning(line, warn, st0)

                false ->
                  st0
              end

            storeType.(st1)
        end
    end
  end

  defp is_underspecified({:type, _, :term, []}, 0) do
    true
  end

  defp is_underspecified({:type, _, :any, []}, 0) do
    true
  end

  defp is_underspecified(_ProtType, _Arity) do
    false
  end

  defp check_type(types, st) do
    {seenVars, st1} = check_type(types, :maps.new(), st)

    :maps.fold(
      fn
        var, {:seen_once, line}, accSt ->
          case :erlang.atom_to_list(var) do
            '_' ++ _ ->
              accSt

            _ ->
              add_error(line, {:singleton_typevar, var}, accSt)
          end

        _Var, :seen_multiple, accSt ->
          accSt
      end,
      st1,
      seenVars
    )
  end

  defp check_type({:ann_type, _L, [_Var, type]}, seenVars, st) do
    check_type(type, seenVars, st)
  end

  defp check_type({:remote_type, l, [{:atom, _, mod}, {:atom, _, name}, args]}, seenVars, st00) do
    st0 = check_module_name(mod, l, st00)
    st = deprecated_type(l, mod, name, args, st0)
    currentMod = r_lint(st, :module)

    case mod === currentMod do
      true ->
        check_type({:user_type, l, name, args}, seenVars, st)

      false ->
        :lists.foldl(
          fn t, {accSeenVars, accSt} ->
            check_type(t, accSeenVars, accSt)
          end,
          {seenVars, st},
          args
        )
    end
  end

  defp check_type({:integer, _L, _}, seenVars, st) do
    {seenVars, st}
  end

  defp check_type({:atom, _L, _}, seenVars, st) do
    {seenVars, st}
  end

  defp check_type({:var, _L, :_}, seenVars, st) do
    {seenVars, st}
  end

  defp check_type({:var, l, name}, seenVars, st) do
    newSeenVars =
      case :maps.find(name, seenVars) do
        {:ok, {:seen_once, _}} ->
          :maps.put(name, :seen_multiple, seenVars)

        {:ok, :seen_multiple} ->
          seenVars

        :error ->
          :maps.put(name, {:seen_once, l}, seenVars)
      end

    {newSeenVars, st}
  end

  defp check_type({:type, l, :bool, []}, seenVars, st) do
    {seenVars, add_warning(l, {:renamed_type, :bool, :boolean}, st)}
  end

  defp check_type({:type, l, :fun, [dom, range]}, seenVars, st) do
    st1 =
      case dom do
        {:type, _, :product, _} ->
          st

        {:type, _, :any} ->
          st

        _ ->
          add_error(l, {:type_syntax, :fun}, st)
      end

    check_type({:type, nowarn(), :product, [dom, range]}, seenVars, st1)
  end

  defp check_type({:type, l, :range, [from, to]}, seenVars, st) do
    st1 =
      case {:erl_eval.partial_eval(from), :erl_eval.partial_eval(to)} do
        {{:integer, _, x}, {:integer, _, y}} when x < y ->
          st

        _ ->
          add_error(l, {:type_syntax, :range}, st)
      end

    {seenVars, st1}
  end

  defp check_type({:type, _L, :map, :any}, seenVars, st) do
    {seenVars, st}
  end

  defp check_type({:type, _L, :map, pairs}, seenVars, st) do
    :lists.foldl(
      fn pair, {accSeenVars, accSt} ->
        check_type(pair, accSeenVars, accSt)
      end,
      {seenVars, st},
      pairs
    )
  end

  defp check_type({:type, _L, :map_field_assoc, [dom, range]}, seenVars, st) do
    check_type({:type, nowarn(), :product, [dom, range]}, seenVars, st)
  end

  defp check_type({:type, _L, :tuple, :any}, seenVars, st) do
    {seenVars, st}
  end

  defp check_type({:type, _L, :any}, seenVars, st) do
    {seenVars, st}
  end

  defp check_type({:type, l, :binary, [base, unit]}, seenVars, st) do
    st1 =
      case {:erl_eval.partial_eval(base), :erl_eval.partial_eval(unit)} do
        {{:integer, _, baseVal}, {:integer, _, unitVal}}
        when baseVal >= 0 and unitVal >= 0 ->
          st

        _ ->
          add_error(l, {:type_syntax, :binary}, st)
      end

    {seenVars, st1}
  end

  defp check_type({:type, l, :record, [name | fields]}, seenVars, st) do
    case name do
      {:atom, _, atom} ->
        st1 = used_record(atom, st)
        check_record_types(l, atom, fields, seenVars, st1)

      _ ->
        {seenVars, add_error(l, {:type_syntax, :record}, st)}
    end
  end

  defp check_type({:type, _L, tag, args}, seenVars, st)
       when tag === :product or tag === :union or
              tag === :tuple do
    :lists.foldl(
      fn t, {accSeenVars, accSt} ->
        check_type(t, accSeenVars, accSt)
      end,
      {seenVars, st},
      args
    )
  end

  defp check_type({:type, la, typeName, args}, seenVars, st) do
    r_lint(module: module, types: types) = st
    arity = length(args)
    typePair = {typeName, arity}

    obsolete =
      is_warn_enabled(
        :deprecated_type,
        st
      ) and obsolete_builtin_type(typePair)

    st1 =
      case obsolete do
        {:deprecated, repl, _}
        when :erlang.element(
               1,
               repl
             ) !== module ->
          case :maps.find(typePair, types) do
            {:ok, _} ->
              used_type(typePair, la, st)

            :error ->
              {:deprecated, replacement, rel} = obsolete
              tag = :deprecated_builtin_type
              w = {tag, typePair, replacement, rel}
              add_warning(la, w, st)
          end

        _ ->
          st
      end

    check_type({:type, nowarn(), :product, args}, seenVars, st1)
  end

  defp check_type({:user_type, l, typeName, args}, seenVars, st) do
    arity = length(args)
    typePair = {typeName, arity}
    st1 = used_type(typePair, l, st)

    :lists.foldl(
      fn t, {accSeenVars, accSt} ->
        check_type(t, accSeenVars, accSt)
      end,
      {seenVars, st1},
      args
    )
  end

  defp check_type([{:typed_record_field, field, _T} | _], seenVars, st) do
    {seenVars, add_error(:erlang.element(2, field), :old_abstract_code, st)}
  end

  defp check_type(i, seenVars, st) do
    case :erl_eval.partial_eval(i) do
      {:integer, _ILn, _Integer} ->
        {seenVars, st}

      _Other ->
        {seenVars, add_error(:erlang.element(2, i), {:type_syntax, :integer}, st)}
    end
  end

  defp check_record_types(line, name, fields, seenVars, st) do
    case :maps.find(name, r_lint(st, :records)) do
      {:ok, {_L, defFields}} ->
        case :lists.all(
               fn
                 {:type, _, :field_type, _} ->
                   true

                 _ ->
                   false
               end,
               fields
             ) do
          true ->
            check_record_types(fields, name, defFields, seenVars, st, [])

          false ->
            {seenVars, add_error(line, {:type_syntax, :record}, st)}
        end

      :error ->
        {seenVars, add_error(line, {:undefined_record, name}, st)}
    end
  end

  defp check_record_types(
         [
           {:type, _, :field_type, [{:atom, aL, fName}, type]}
           | left
         ],
         name,
         defFields,
         seenVars,
         st,
         seenFields
       ) do
    st1 =
      case exist_field(fName, defFields) do
        true ->
          st

        false ->
          add_error(aL, {:undefined_field, name, fName}, st)
      end

    st2 =
      case :ordsets.is_element(fName, seenFields) do
        true ->
          add_error(aL, {:redefine_field, name, fName}, st1)

        false ->
          st1
      end

    {newSeenVars, st3} = check_type(type, seenVars, st2)
    newSeenFields = :ordsets.add_element(fName, seenFields)
    check_record_types(left, name, defFields, newSeenVars, st3, newSeenFields)
  end

  defp check_record_types([], _Name, _DefFields, seenVars, st, _SeenFields) do
    {seenVars, st}
  end

  defp used_type(typePair, l, r_lint(usage: usage, file: file) = st) do
    oldUsed = r_usage(usage, :used_types)
    usedTypes = :maps.put(typePair, :erl_anno.set_file(file, l), oldUsed)
    r_lint(st, usage: r_usage(usage, used_types: usedTypes))
  end

  defp is_default_type({name, numberOfTypeVariables}) do
    :erl_internal.is_type(name, numberOfTypeVariables)
  end

  defp is_newly_introduced_builtin_type({name, _}) when is_atom(name) do
    false
  end

  defp is_obsolete_builtin_type(typePair) do
    obsolete_builtin_type(typePair) !== :no
  end

  defp obsolete_builtin_type({1, 255}) do
    {:deprecated, {2, 255}, ''}
  end

  defp obsolete_builtin_type({name, a})
       when is_atom(name) and
              is_integer(a) do
    :no
  end

  defp spec_decl(line, mFA0, typeSpecs, st00 = r_lint(specs: specs, module: mod)) do
    mFA =
      case mFA0 do
        {f, arity} ->
          {mod, f, arity}

        {_M, _F, arity} ->
          mFA0
      end

    st0 = check_module_name(:erlang.element(1, mFA), line, st00)
    st1 = r_lint(st0, specs: :maps.put(mFA, line, specs))

    case :erlang.is_map_key(mFA, specs) do
      true ->
        add_error(line, {:redefine_spec, mFA0}, st1)

      false ->
        case mFA do
          {^mod, _, _} ->
            check_specs(typeSpecs, :spec_wrong_arity, arity, st1)

          _ ->
            add_error(line, {:bad_module, mFA}, st1)
        end
    end
  end

  defp callback_decl(line, mFA0, typeSpecs, st0 = r_lint(callbacks: callbacks, module: mod)) do
    case mFA0 do
      {m, _F, _A} ->
        st1 = check_module_name(m, line, st0)
        add_error(line, {:bad_callback, mFA0}, st1)

      {f, arity} ->
        mFA = {mod, f, arity}
        st1 = r_lint(st0, callbacks: :maps.put(mFA, line, callbacks))

        case :erlang.is_map_key(mFA, callbacks) do
          true ->
            add_error(line, {:redefine_callback, mFA0}, st1)

          false ->
            check_specs(typeSpecs, :callback_wrong_arity, arity, st1)
        end
    end
  end

  defp optional_callbacks(line, term, st0) do
    try do
      true = is_fa_list(term)
      term
    catch
      _, _ ->
        st0
    else
      fAs ->
        optional_cbs(line, fAs, st0)
    end
  end

  defp optional_cbs(_Line, [], st) do
    st
  end

  defp optional_cbs(line, [{f, a} | fAs], st0) do
    r_lint(optional_callbacks: optionalCbs, module: mod) = st0
    mFA = {mod, f, a}
    st1 = r_lint(st0, optional_callbacks: :maps.put(mFA, line, optionalCbs))

    st2 =
      case :erlang.is_map_key(mFA, optionalCbs) do
        true ->
          add_error(line, {:redefine_optional_callback, {f, a}}, st1)

        false ->
          st1
      end

    optional_cbs(line, fAs, st2)
  end

  defp is_fa_list([e | l]) do
    is_fa(e) and is_fa_list(l)
  end

  defp is_fa_list([]) do
    true
  end

  defp is_fa_list(_) do
    false
  end

  defp is_fa({funcName, arity})
       when is_atom(funcName) and
              is_integer(arity) and arity >= 0 do
    true
  end

  defp is_fa(_) do
    false
  end

  defp check_module_name(m, line, st) do
    case is_latin1_name(m) do
      true ->
        st

      false ->
        add_error(line, :non_latin1_module_unsupported, st)
    end
  end

  defp is_latin1_name(name) do
    :io_lib.latin1_char_list(:erlang.atom_to_list(name))
  end

  defp check_specs([funType | left], eTag, arity, st0) do
    {funType1, cTypes} =
      case funType do
        {:type, _, :bounded_fun, [fT = {:type, _, :fun, _}, cs]} ->
          types0 =
            for {:type, _, :constraint, [_, t]} <- cs do
              t
            end

          {fT, :lists.append(types0)}

        {:type, _, :fun, _} = fT ->
          {fT, []}
      end

    {:type, l, :fun, [{:type, _, :product, d}, _]} = funType1
    specArity = length(d)

    st1 =
      case arity === specArity do
        true ->
          st0

        false ->
          add_error(l, eTag, st0)
      end

    st2 =
      check_type(
        {:type, nowarn(), :product, [funType1 | cTypes]},
        st1
      )

    check_specs(left, eTag, arity, st2)
  end

  defp check_specs([], _ETag, _Arity, st) do
    st
  end

  defp nowarn() do
    a0 = :erl_anno.new(0)
    a1 = :erl_anno.set_generated(true, a0)
    :erl_anno.set_file('', a1)
  end

  defp check_specs_without_function(r_lint(module: mod, defined: funcs, specs: specs) = st) do
    fun = fn
      {m, f, a}, line, accSt when m === mod ->
        fA = {f, a}

        case :gb_sets.is_element(fA, funcs) do
          true ->
            accSt

          false ->
            add_error(line, {:spec_fun_undefined, fA}, accSt)
        end

      {_M, _F, _A}, _Line, accSt ->
        accSt
    end

    :maps.fold(fun, st, specs)
  end

  defp check_functions_without_spec(forms, st0) do
    case is_warn_enabled(:missing_spec_all, st0) do
      true ->
        add_missing_spec_warnings(forms, st0, :all)

      false ->
        case is_warn_enabled(:missing_spec, st0) do
          true ->
            add_missing_spec_warnings(forms, st0, :exported)

          false ->
            st0
        end
    end
  end

  defp add_missing_spec_warnings(forms, st0, type) do
    specs =
      for {_M, f, a} <- :maps.keys(r_lint(st0, :specs)) do
        {f, a}
      end

    warns =
      case type do
        :all ->
          for {:function, l, f, a, _} <- forms,
              not :lists.member(fA = {f, a}, specs) do
            {fA, l}
          end

        :exported ->
          exps0 = :gb_sets.to_list(r_lint(st0, :exports)) -- pseudolocals()
          exps = exps0 -- specs

          for {:function, l, f, a, _} <- forms,
              member(fA = {f, a}, exps) do
            {fA, l}
          end
      end

    foldl(
      fn {fA, l}, st ->
        add_warning(l, {:missing_spec, fA}, st)
      end,
      st0,
      warns
    )
  end

  defp check_unused_types(forms, st) do
    case is_warn_enabled(:unused_type, st) do
      true ->
        check_unused_types_1(forms, st)

      false ->
        st
    end
  end

  defp check_unused_types_1(
         forms,
         r_lint(usage: usage, types: ts, exp_types: expTs) = st
       ) do
    case (for {:attribute, _L, :file, {file, _Line}} <- forms do
            file
          end) do
      [firstFile | _] ->
        d = r_usage(usage, :used_types)
        l = :gb_sets.to_list(expTs) ++ :maps.keys(d)
        usedTypes = :gb_sets.from_list(l)

        foldFun = fn
          {{:record, _} = _Type, 0}, _, accSt ->
            accSt

          type, r_typeinfo(line: fileLine), accSt ->
            case loc(fileLine, accSt) do
              {^firstFile, _} ->
                case :gb_sets.is_member(type, usedTypes) do
                  true ->
                    accSt

                  false ->
                    warn = {:unused_type, type}
                    add_warning(fileLine, warn, accSt)
                end

              _ ->
                accSt
            end
        end

        :maps.fold(foldFun, st, ts)

      [] ->
        st
    end
  end

  defp check_local_opaque_types(st) do
    r_lint(types: ts, exp_types: expTs) = st

    foldFun = fn
      _Type, r_typeinfo(attr: :type), accSt ->
        accSt

      type, r_typeinfo(attr: :opaque, line: fileLine), accSt ->
        case :gb_sets.is_element(type, expTs) do
          true ->
            accSt

          false ->
            warn = {:not_exported_opaque, type}
            add_warning(fileLine, warn, accSt)
        end
    end

    :maps.fold(foldFun, st, ts)
  end

  defp check_dialyzer_attribute(forms, st0) do
    vals =
      for {:attribute, l, :dialyzer, val} <- forms,
          v0 <- :lists.flatten([val]),
          v <-
            (case v0 do
               {o, f} ->
                 for a <- :lists.flatten([o]),
                     b <- :lists.flatten([f]) do
                   {a, b}
                 end

               t ->
                 [t]
             end) do
        {l, v}
      end

    {wellformed, bad} =
      :lists.partition(
        fn
          {_, {option, fA}}
          when is_atom(option) ->
            is_fa(fA)

          {_, option} when is_atom(option) ->
            true

          _ ->
            false
        end,
        vals
      )

    st1 =
      foldl(
        fn {l, term}, st ->
          add_error(l, {:bad_dialyzer_attribute, term}, st)
        end,
        st0,
        bad
      )

    defFunctions = :gb_sets.to_list(r_lint(st0, :defined)) -- pseudolocals()

    fun = fn
      {l, {option, fA}}, st ->
        case is_function_dialyzer_option(option) do
          true ->
            case :lists.member(fA, defFunctions) do
              true ->
                st

              false ->
                add_error(l, {:undefined_function, fA}, st)
            end

          false ->
            add_error(l, {:bad_dialyzer_option, option}, st)
        end

      {l, option}, st ->
        case is_module_dialyzer_option(option) do
          true ->
            st

          false ->
            add_error(l, {:bad_dialyzer_option, option}, st)
        end
    end

    foldl(fun, st1, wellformed)
  end

  defp is_function_dialyzer_option(:nowarn_function) do
    true
  end

  defp is_function_dialyzer_option(option) do
    is_module_dialyzer_option(option)
  end

  defp is_module_dialyzer_option(option) do
    :lists.member(
      option,
      [
        :no_return,
        :no_unused,
        :no_improper_lists,
        :no_fun_app,
        :no_match,
        :no_opaque,
        :no_fail_call,
        :no_contracts,
        :no_behaviours,
        :no_undefined_callbacks,
        :unmatched_returns,
        :error_handling,
        :race_conditions,
        :no_missing_calls,
        :specdiffs,
        :overspecs,
        :underspecs,
        :unknown
      ]
    )
  end

  defp try_clauses(scs, ccs, in__, vt, uvt, st0) do
    {csvt0, st1} = icrt_clauses(scs, vt, st0)
    st2 = r_lint(st1, in_try_head: true)
    {csvt1, st3} = icrt_clauses(ccs, vtupdate(uvt, vt), st2)
    csvt = csvt0 ++ csvt1
    updVt = icrt_export(csvt, vt, in__, st3)
    {updVt, r_lint(st3, in_try_head: false)}
  end

  defp icrt_clauses(cs, in__, vt, st0) do
    {csvt, st1} = icrt_clauses(cs, vt, st0)
    updVt = icrt_export(csvt, vt, in__, st1)
    {updVt, st1}
  end

  defp icrt_clauses(cs, vt, st) do
    mapfoldl(
      fn c, st0 ->
        icrt_clause(c, vt, st0)
      end,
      st,
      cs
    )
  end

  defp icrt_clause({:clause, _Line, h, g, b}, vt0, st0) do
    vt1 = taint_stack_var(vt0, h, st0)
    {hvt, hnew, st1} = head(h, vt1, st0)
    vt2 = vtupdate(hvt, hnew)
    vt3 = taint_stack_var(vt2, h, st0)
    {gvt, st2} = guard(g, vtupdate(vt3, vt0), r_lint(st1, in_try_head: false))
    vt4 = vtupdate(gvt, vt2)
    {bvt, st3} = exprs(b, vtupdate(vt4, vt0), st2)
    {vtupdate(bvt, vt4), st3}
  end

  defp taint_stack_var(vt, pat, r_lint(in_try_head: true)) do
    [{:tuple, _, [_, _, {:var, _, stk}]}] = pat

    case stk do
      :_ ->
        vt

      _ ->
        :lists.map(
          fn
            {v, {:bound, used, lines}} when v === stk ->
              {v, {:stacktrace, used, lines}}

            b ->
              b
          end,
          vt
        )
    end
  end

  defp taint_stack_var(vt, _Pat, r_lint(in_try_head: false)) do
    vt
  end

  defp icrt_export(vts, vt, {tag, attrs}, st) do
    {_File, loc} = loc(attrs, st)
    icrt_export(:lists.merge(vts), vt, {tag, loc}, length(vts), [])
  end

  defp icrt_export(
         [{v, {{:export, _}, _, _}} | vs0],
         [{v, {{:export, _} = s0, _, ls}} | vt],
         in__,
         i,
         acc
       ) do
    {vVs, vs} =
      :lists.partition(
        fn {k, _} ->
          k === v
        end,
        vs0
      )

    s =
      foldl(
        fn {_, {s1, _, _}}, accS ->
          merge_state(accS, s1)
        end,
        s0,
        vVs
      )

    icrt_export(vs, vt, in__, i, [{v, {s, :used, ls}} | acc])
  end

  defp icrt_export([{v, _} | vs0], [{v, {_, _, ls}} | vt], in__, i, acc) do
    vs =
      :lists.dropwhile(
        fn {k, _} ->
          k === v
        end,
        vs0
      )

    icrt_export(vs, vt, in__, i, [{v, {:bound, :used, ls}} | acc])
  end

  defp icrt_export([{v1, _} | _] = vs, [{v2, _} | vt], in__, i, acc)
       when v1 > v2 do
    icrt_export(vs, vt, in__, i, acc)
  end

  defp icrt_export([{v, _} | _] = vs0, vt, in__, i, acc) do
    {vVs, vs} =
      :lists.partition(
        fn {k, _} ->
          k === v
        end,
        vs0
      )

    f = fn {_, {s, u, ls}}, {accI, accS0, accLs0} ->
      accS =
        case {s, accS0} do
          {{:unsafe, _}, {:unsafe, _}} ->
            {:unsafe, in__}

          {{:unsafe, _}, _} ->
            s

          _ ->
            accS0
        end

      accLs =
        case u do
          :used ->
            accLs0

          :unused ->
            merge_lines(accLs0, ls)
        end

      {accI + 1, accS, accLs}
    end

    {count, s1, ls} = foldl(f, {0, {:export, in__}, []}, vVs)

    s =
      case count do
        ^i ->
          s1

        _ ->
          {:unsafe, in__}
      end

    u =
      case ls do
        [] ->
          :used

        _ ->
          :unused
      end

    icrt_export(vs, vt, in__, i, [{v, {s, u, ls}} | acc])
  end

  defp icrt_export([], _, _, _, acc) do
    reverse(acc)
  end

  defp handle_comprehension(e, qs, vt0, st0) do
    {vt1, uvt, st1} = lc_quals(qs, vt0, st0)
    {evt, st2} = expr(e, vt1, st1)
    vt2 = vtupdate(evt, vt1)
    {_, st3} = check_old_unused_vars(vt2, uvt, st2)
    {_, st4} = check_unused_vars(uvt, vt0, st3)
    {_, st} = check_unused_vars(vt2, vt0, st4)
    vt3 = vtmerge(vtsubtract(vt2, uvt), uvt)
    vt4 = vtold(vt3, vt0)
    vt = vt_no_unsafe(vt_no_unused(vt4))
    {vt, st}
  end

  defp lc_quals(qs, vt0, st0) do
    oldRecDef = r_lint(st0, :recdef_top)
    {vt, uvt, st} = lc_quals(qs, vt0, [], r_lint(st0, recdef_top: false))
    {vt, uvt, r_lint(st, recdef_top: oldRecDef)}
  end

  defp lc_quals([{:generate, _Line, p, e} | qs], vt0, uvt0, st0) do
    {vt, uvt, st} = handle_generator(p, e, vt0, uvt0, st0)
    lc_quals(qs, vt, uvt, st)
  end

  defp lc_quals([{:b_generate, _Line, p, e} | qs], vt0, uvt0, st0) do
    st1 = handle_bitstring_gen_pat(p, st0)
    {vt, uvt, st} = handle_generator(p, e, vt0, uvt0, st1)
    lc_quals(qs, vt, uvt, st)
  end

  defp lc_quals([f | qs], vt, uvt, st0) do
    info = is_guard_test2_info(st0)

    {fvt, st1} =
      case is_guard_test2(f, info) do
        true ->
          guard_test(f, vt, st0)

        false ->
          expr(f, vt, st0)
      end

    lc_quals(qs, vtupdate(fvt, vt), uvt, st1)
  end

  defp lc_quals([], vt, uvt, st) do
    {vt, uvt, st}
  end

  defp is_guard_test2_info(r_lint(records: rDs, locals: locals, imports: imports)) do
    {rDs,
     fn fA ->
       is_local_function(
         locals,
         fA
       ) or is_imported_function(imports, fA)
     end}
  end

  defp handle_generator(p, e, vt, uvt, st0) do
    {evt, st1} = expr(e, vt, st0)
    vt1 = vtupdate(vtold(evt, vt), vt)
    {_, st2} = check_unused_vars(evt, vt, st1)
    {pvt, pnew, st3} = pattern(p, vt1, [], st2)
    vt2 = vtupdate(pvt, vt1)
    st4 = shadow_vars(pnew, vt1, :generate, st3)
    svt = vtold(vt2, pnew)
    {_, st5} = check_old_unused_vars(svt, uvt, st4)
    nUvt = vtupdate(vtnew(svt, uvt), uvt)
    vt3 = vtupdate(vtsubtract(vt2, pnew), pnew)
    {vt3, nUvt, st5}
  end

  defp handle_bitstring_gen_pat({:bin, _, segments = [_ | _]}, st) do
    case :lists.last(segments) do
      {:bin_element, line, _, :default, flags}
      when is_list(flags) ->
        case member(:binary, flags) or
               member(
                 :bytes,
                 flags
               ) or
               member(
                 :bits,
                 flags
               ) or
               member(
                 :bitstring,
                 flags
               ) do
          true ->
            add_error(line, :unsized_binary_in_bin_gen_pattern, st)

          false ->
            st
        end

      _ ->
        st
    end
  end

  defp handle_bitstring_gen_pat(_, st) do
    st
  end

  defp fun_clauses(cs, vt, st) do
    oldRecDef = r_lint(st, :recdef_top)

    {bvt, st2} =
      foldl(
        fn c, {bvt0, st0} ->
          {cvt, st1} = fun_clause(c, vt, st0)
          {vtmerge(cvt, bvt0), st1}
        end,
        {[], r_lint(st, recdef_top: false)},
        cs
      )

    uvt = vt_no_unsafe(vt_no_unused(vtold(bvt, vt)))
    {uvt, r_lint(st2, recdef_top: oldRecDef)}
  end

  defp fun_clause({:clause, _Line, h, g, b}, vt0, st0) do
    {hvt, hnew, st1} = head(h, vt0, [], st0)
    vt1 = vtupdate(hvt, vt0)
    st2 = shadow_vars(hnew, vt0, :fun, st1)
    vt2 = vtupdate(vtsubtract(vt1, hnew), hnew)
    {gvt, st3} = guard(g, vt2, st2)
    vt3 = vtupdate(gvt, vt2)
    {bvt, st4} = exprs(b, vt3, st3)
    cvt = vtupdate(bvt, vt3)
    {_, st5} = check_unused_vars(cvt, vt0, st4)
    svt = vtold(vt1, hnew)
    {_, st6} = check_old_unused_vars(cvt, svt, st5)
    vt4 = vtmerge(svt, vtsubtract(cvt, svt))
    {vtold(vt4, vt0), st6}
  end

  defp pat_var(v, line, vt, new, st) do
    case :orddict.find(v, new) do
      {:ok, {:bound, _Usage, ls}} ->
        {[], [{v, {:bound, :used, ls}}], st}

      :error ->
        case :orddict.find(v, vt) do
          {:ok, {:bound, _Usage, ls}} ->
            {[{v, {:bound, :used, ls}}], [], st}

          {:ok, {{:unsafe, in__}, _Usage, ls}} ->
            {[{v, {:bound, :used, ls}}], [], add_error(line, {:unsafe_var, v, in__}, st)}

          {:ok, {{:export, from}, _Usage, ls}} ->
            {[{v, {:bound, :used, ls}}], [], add_warning(line, {:exported_var, v, from}, st)}

          {:ok, {:stacktrace, _Usage, ls}} ->
            {[{v, {:bound, :used, ls}}], [], add_error(line, {:stacktrace_bound, v}, st)}

          :error when r_lint(st, :recdef_top) ->
            {[], [{v, {:bound, :unused, [line]}}],
             add_error(line, {:variable_in_record_def, v}, st)}

          :error ->
            {[], [{v, {:bound, :unused, [line]}}], st}
        end
    end
  end

  defp pat_binsize_var(v, line, vt, new, st) do
    case :orddict.find(v, new) do
      {:ok, {:bound, _Used, ls}} ->
        {[], [{v, {:bound, :used, ls}}], st}

      :error ->
        case :orddict.find(v, vt) do
          {:ok, {:bound, _Used, ls}} ->
            {[{v, {:bound, :used, ls}}], [], st}

          {:ok, {{:unsafe, in__}, _Used, ls}} ->
            {[{v, {:bound, :used, ls}}], [], add_error(line, {:unsafe_var, v, in__}, st)}

          {:ok, {{:export, from}, _Used, ls}} ->
            {[{v, {:bound, :used, ls}}], [], exported_var(line, v, from, st)}

          :error ->
            {[{v, {:bound, :used, [line]}}], [], add_error(line, {:unbound_var, v}, st)}
        end
    end
  end

  defp expr_var(v, line, vt, r_lint(bvt: :none) = st) do
    do_expr_var(v, line, vt, st)
  end

  defp expr_var(v, line, vt0, r_lint(bvt: bvt0) = st0)
       when is_list(bvt0) do
    {vt, bvt, st} = pat_binsize_var(v, line, vt0, bvt0, st0)
    {vt, r_lint(st, bvt: vtmerge(bvt0, bvt))}
  end

  defp do_expr_var(v, line, vt, st) do
    case :orddict.find(v, vt) do
      {:ok, {:bound, _Usage, ls}} ->
        {[{v, {:bound, :used, ls}}], st}

      {:ok, {{:unsafe, in__}, _Usage, ls}} ->
        {[{v, {:bound, :used, ls}}], add_error(line, {:unsafe_var, v, in__}, st)}

      {:ok, {{:export, from}, _Usage, ls}} ->
        case is_warn_enabled(:export_vars, st) do
          true ->
            {[{v, {:bound, :used, ls}}], add_warning(line, {:exported_var, v, from}, st)}

          false ->
            {[{v, {{:export, from}, :used, ls}}], st}
        end

      {:ok, {:stacktrace, _Usage, ls}} ->
        {[{v, {:bound, :used, ls}}], add_error(line, {:stacktrace_guard, v}, st)}

      :error ->
        {[{v, {:bound, :used, [line]}}], add_error(line, {:unbound_var, v}, st)}
    end
  end

  defp exported_var(line, v, from, st) do
    case is_warn_enabled(:export_vars, st) do
      true ->
        add_warning(line, {:exported_var, v, from}, st)

      false ->
        st
    end
  end

  defp shadow_vars(vt, vt0, in__, st0) do
    case is_warn_enabled(:shadow_vars, st0) do
      true ->
        foldl(
          fn
            {v, {_, _, [l | _]}}, st ->
              add_warning(l, {:shadowed_var, v, in__}, st)

            _, st ->
              st
          end,
          st0,
          vtold(vt, vt_no_unsafe(vt0))
        )

      false ->
        st0
    end
  end

  defp check_unused_vars(vt, vt0, st0) do
    u = unused_vars(vt, vt0, st0)
    warn_unused_vars(u, vt, st0)
  end

  defp check_old_unused_vars(vt, vt0, st0) do
    u = unused_vars(vtold(vt, vt0), [], st0)
    warn_unused_vars(u, vt, st0)
  end

  defp unused_vars(vt, vt0, _St0) do
    u0 =
      :orddict.filter(
        fn
          v, {_State, :unused, _Ls} ->
            case :erlang.atom_to_list(v) do
              '_' ++ _ ->
                false

              _ ->
                true
            end

          _V, _How ->
            false
        end,
        vt
      )

    vtnew(u0, vt0)
  end

  defp warn_unused_vars([], vt, st0) do
    {vt, st0}
  end

  defp warn_unused_vars(u, vt, st0) do
    st1 =
      case is_warn_enabled(:unused_vars, st0) do
        false ->
          st0

        true ->
          foldl(
            fn {v, {_, :unused, ls}}, st ->
              foldl(
                fn l, st2 ->
                  add_warning(l, {:unused_var, v}, st2)
                end,
                st,
                ls
              )
            end,
            st0,
            u
          )
      end

    uVt =
      map(
        fn {v, {state, _, ls}} ->
          {v, {state, :used, ls}}
        end,
        u
      )

    {vtmerge(vt, uVt), st1}
  end

  defp is_var_bound(v, vt) do
    case :orddict.find(v, vt) do
      {:ok, {:bound, _Usage, _}} ->
        true

      _ ->
        false
    end
  end

  defp vtupdate(uvt, vt0) do
    :orddict.merge(
      fn _V, {s, u1, l1}, {_S, u2, l2} ->
        {s, merge_used(u1, u2), merge_lines(l1, l2)}
      end,
      uvt,
      vt0
    )
  end

  defp vtunsafe({tag, fileLine}, uvt, vt) do
    line = :erl_anno.location(fileLine)

    for {v, {_, u, ls}} <- vtnew(uvt, vt) do
      {v, {{:unsafe, {tag, line}}, u, ls}}
    end
  end

  defp vtmerge(vt1, vt2) do
    :orddict.merge(
      fn _V, {s1, u1, l1}, {s2, u2, l2} ->
        {merge_state(s1, s2), merge_used(u1, u2), merge_lines(l1, l2)}
      end,
      vt1,
      vt2
    )
  end

  defp vtmerge(vts) do
    foldl(
      fn vt, mvts ->
        vtmerge(vt, mvts)
      end,
      [],
      vts
    )
  end

  defp vtmerge_pat(vt1, vt2) do
    :orddict.merge(
      fn _V, {s1, _Usage1, l1}, {s2, _Usage2, l2} ->
        {merge_state(s1, s2), :used, merge_lines(l1, l2)}
      end,
      vt1,
      vt2
    )
  end

  defp merge_lines(ls1, ls2) do
    :ordsets.union(ls1, ls2)
  end

  defp merge_state({:unsafe, _F1} = s1, _S2) do
    s1
  end

  defp merge_state(_S1, {:unsafe, _F2} = s2) do
    s2
  end

  defp merge_state(:bound, s2) do
    s2
  end

  defp merge_state(s1, :bound) do
    s1
  end

  defp merge_state({:export, f1}, {:export, _F2}) do
    {:export, f1}
  end

  defp merge_used(:used, _Usage2) do
    :used
  end

  defp merge_used(_Usage1, :used) do
    :used
  end

  defp merge_used(:unused, :unused) do
    :unused
  end

  defp vtnew(new, old) do
    :orddict.filter(
      fn v, _How ->
        not :orddict.is_key(v, old)
      end,
      new
    )
  end

  defp vtsubtract(new, old) do
    vtnew(new, old)
  end

  defp vtold(new, old) do
    :orddict.filter(
      fn v, _How ->
        :orddict.is_key(v, old)
      end,
      new
    )
  end

  defp vt_no_unsafe(vt) do
    for {_, {s, _U, _L}} = v <- vt,
        (case s do
           {:unsafe, _} ->
             false

           _ ->
             true
         end) do
      v
    end
  end

  defp vt_no_unused(vt) do
    for {_, {_, u, _L}} = v <- vt, u !== :unused do
      v
    end
  end

  defp copy_expr(expr, anno) do
    :erl_parse.map_anno(
      fn _A ->
        anno
      end,
      expr
    )
  end

  defp check_record_info_call(_Line, la, [{:atom, li, info}, {:atom, _Ln, name}], st) do
    case member(info, [:fields, :size]) do
      true ->
        exist_record(la, name, st)

      false ->
        add_error(li, :illegal_record_info, st)
    end
  end

  defp check_record_info_call(line, _La, _As, st) do
    add_error(line, :illegal_record_info, st)
  end

  defp has_wildcard_field([
         {:record_field, _Lf, {:var, _La, :_}, _Val}
         | _Fs
       ]) do
    true
  end

  defp has_wildcard_field([_ | fs]) do
    has_wildcard_field(fs)
  end

  defp has_wildcard_field([]) do
    false
  end

  defp check_remote_function(line, m, f, as, st0) do
    st1 = deprecated_function(line, m, f, as, st0)
    st2 = check_qlc_hrl(line, m, f, as, st1)
    st3 = check_load_nif(line, m, f, as, st2)
    format_function(line, m, f, as, st3)
  end

  defp check_load_nif(line, :erlang, :load_nif, [_, _], st) do
    case is_warn_enabled(:nif_inline, st) do
      true ->
        check_nif_inline(line, st)

      false ->
        st
    end
  end

  defp check_load_nif(_Line, _ModName, _FuncName, _Args, st) do
    st
  end

  defp check_nif_inline(line, st) do
    case any(&is_inline_opt/1, r_lint(st, :compile)) do
      true ->
        add_warning(line, :nif_inline, st)

      false ->
        st
    end
  end

  defp is_inline_opt({:inline, [_ | _] = _FAs}) do
    true
  end

  defp is_inline_opt(:inline) do
    true
  end

  defp is_inline_opt(_) do
    false
  end

  defp check_qlc_hrl(line, m, f, as, st) do
    arity = length(as)

    case as do
      [{:lc, _L, _E, _Qs} | _]
      when m === :qlc and
             f === :q and arity < 3 and
             not r_lint(st, :xqlc) ->
        add_warning(line, {:missing_qlc_hrl, arity}, st)

      _ ->
        st
    end
  end

  defp deprecated_function(line, m, f, as, st) do
    arity = length(as)
    mFA = {m, f, arity}

    case :otp_internal.obsolete(m, f, arity) do
      {:deprecated, string} when is_list(string) ->
        case not is_warn_enabled(
               :deprecated_function,
               st
             ) or
               :ordsets.is_element(
                 mFA,
                 r_lint(st, :not_deprecated)
               ) do
          true ->
            st

          false ->
            add_warning(line, {:deprecated, mFA, string}, st)
        end

      {:deprecated, replacement, rel} ->
        case not is_warn_enabled(
               :deprecated_function,
               st
             ) or
               :ordsets.is_element(
                 mFA,
                 r_lint(st, :not_deprecated)
               ) do
          true ->
            st

          false ->
            add_warning(line, {:deprecated, mFA, replacement, rel}, st)
        end

      {:removed, string} when is_list(string) ->
        add_removed_warning(line, mFA, {:removed, mFA, string}, st)

      {:removed, replacement, rel} ->
        add_removed_warning(line, mFA, {:removed, mFA, replacement, rel}, st)

      :no ->
        st
    end
  end

  defp add_removed_warning(line, {m, _, _} = mFA, warning, r_lint(not_removed: notRemoved) = st) do
    case is_warn_enabled(:removed, st) and
           not :gb_sets.is_element(
             m,
             notRemoved
           ) and
           not :gb_sets.is_element(
             mFA,
             notRemoved
           ) do
      true ->
        add_warning(line, warning, st)

      false ->
        st
    end
  end

  defp deprecated_type(l, m, n, as, st) do
    nAs = length(as)

    case :otp_internal.obsolete_type(m, n, nAs) do
      {:deprecated, string} when is_list(string) ->
        case is_warn_enabled(:deprecated_type, st) do
          true ->
            add_warning(l, {:deprecated_type, {m, n, nAs}, string}, st)

          false ->
            st
        end

      {:removed, string} ->
        add_warning(l, {:removed_type, {m, n, nAs}, string}, st)

      :no ->
        st
    end
  end

  defp obsolete_guard({:call, line, {:atom, lr, f}, as}, st0) do
    arity = length(as)

    case :erl_internal.old_type_test(f, arity) do
      false ->
        deprecated_function(line, :erlang, f, as, st0)

      true ->
        st =
          case is_warn_enabled(:obsolete_guard, st0) do
            true ->
              add_warning(lr, {:obsolete_guard, {f, arity}}, st0)

            false ->
              st0
          end

        test_overriden_by_local(lr, f, arity, st)
    end
  end

  defp obsolete_guard(_G, st) do
    st
  end

  defp test_overriden_by_local(line, oldTest, arity, st) do
    modernTest = :erlang.list_to_atom('is_' ++ :erlang.atom_to_list(oldTest))

    case is_local_function(
           r_lint(st, :locals),
           {modernTest, arity}
         ) do
      true ->
        add_error(line, {:obsolete_guard_overridden, oldTest}, st)

      false ->
        st
    end
  end

  defp keyword_warning(_Line, _A, st) do
    st
  end

  defp format_function(line, m, f, as, st) do
    case is_format_function(m, f) do
      true ->
        case r_lint(st, :warn_format) do
          lev when lev > 0 ->
            case check_format_1(as) do
              {:warn, level, fmt, fas} when level <= lev ->
                add_warning(line, {:format_error, {fmt, fas}}, st)

              _ ->
                st
            end

          _Lev ->
            st
        end

      false ->
        st
    end
  end

  defp is_format_function(:io, :fwrite) do
    true
  end

  defp is_format_function(:io, :format) do
    true
  end

  defp is_format_function(:io_lib, :fwrite) do
    true
  end

  defp is_format_function(:io_lib, :format) do
    true
  end

  defp is_format_function(m, f) when is_atom(m) and is_atom(f) do
    false
  end

  defp check_format_1([fmt]) do
    check_format_1([fmt, {nil, 0}])
  end

  defp check_format_1([fmt, as]) do
    check_format_2(fmt, canonicalize_string(as))
  end

  defp check_format_1([_Dev, fmt, as]) do
    check_format_1([fmt, as])
  end

  defp check_format_1(_As) do
    {:warn, 1, 'format call with wrong number of arguments', []}
  end

  defp canonicalize_string({:string, line, cs}) do
    foldr(
      fn c, t ->
        {:cons, line, {:integer, line, c}, t}
      end,
      {nil, line},
      cs
    )
  end

  defp canonicalize_string(term) do
    term
  end

  defp check_format_2(fmt, as) do
    case fmt do
      {:string, _L, s} ->
        check_format_2a(s, as)

      {:atom, _L, a} ->
        check_format_2a(:erlang.atom_to_list(a), as)

      _ ->
        {:warn, 2, 'format string not a textual constant', []}
    end
  end

  defp check_format_2a(fmt, as) do
    case args_list(as) do
      true ->
        check_format_3(fmt, as)

      false ->
        {:warn, 1, 'format arguments not a list', []}

      :maybe ->
        {:warn, 2, 'format arguments perhaps not a list', []}
    end
  end

  defp check_format_3(fmt, as) do
    case check_format_string(fmt) do
      {:ok, need} ->
        case args_length(as) do
          len when length(need) === len ->
            :ok

          _Len ->
            {:warn, 1, 'wrong number of arguments in format call', []}
        end

      {:error, s} ->
        {:warn, 1, 'format string invalid (~ts)', [s]}
    end
  end

  defp args_list({:cons, _L, _H, t}) do
    args_list(t)
  end

  defp args_list({:string, _L, _Cs}) do
    :maybe
  end

  defp args_list({nil, _L}) do
    true
  end

  defp args_list({:atom, _, _}) do
    false
  end

  defp args_list({:integer, _, _}) do
    false
  end

  defp args_list({:float, _, _}) do
    false
  end

  defp args_list(_Other) do
    :maybe
  end

  defp args_length({:cons, _L, _H, t}) do
    1 + args_length(t)
  end

  defp args_length({nil, _L}) do
    0
  end

  defp check_format_string(fmt) do
    extract_sequences(fmt, [])
  end

  defp extract_sequences(fmt, need0) do
    case :string.find(fmt, [?~]) do
      :nomatch ->
        {:ok, :lists.reverse(need0)}

      [?~ | fmt1] ->
        case extract_sequence(1, fmt1, need0) do
          {:ok, need1, rest} ->
            extract_sequences(rest, need1)

          error ->
            error
        end
    end
  end

  defp extract_sequence(1, [?-, c | fmt], need)
       when c >= ?0 and
              c <= ?9 do
    extract_sequence_digits(1, fmt, need)
  end

  defp extract_sequence(1, [c | fmt], need)
       when c >= ?0 and
              c <= ?9 do
    extract_sequence_digits(1, fmt, need)
  end

  defp extract_sequence(1, [?-, ?* | fmt], need) do
    extract_sequence(2, fmt, [:int | need])
  end

  defp extract_sequence(1, [?* | fmt], need) do
    extract_sequence(2, fmt, [:int | need])
  end

  defp extract_sequence(1, fmt, need) do
    extract_sequence(2, fmt, need)
  end

  defp extract_sequence(2, [?., c | fmt], need)
       when c >= ?0 and
              c <= ?9 do
    extract_sequence_digits(2, fmt, need)
  end

  defp extract_sequence(2, [?., ?* | fmt], need) do
    extract_sequence(3, fmt, [:int | need])
  end

  defp extract_sequence(2, [?. | fmt], need) do
    extract_sequence(3, fmt, need)
  end

  defp extract_sequence(2, fmt, need) do
    extract_sequence(4, fmt, need)
  end

  defp extract_sequence(3, [?., ?* | fmt], need) do
    extract_sequence(4, fmt, [:int | need])
  end

  defp extract_sequence(3, [?., _ | fmt], need) do
    extract_sequence(4, fmt, need)
  end

  defp extract_sequence(3, fmt, need) do
    extract_sequence(4, fmt, need)
  end

  defp extract_sequence(4, [?t, ?l | fmt], need) do
    extract_sequence(4, [?l, ?t | fmt], need)
  end

  defp extract_sequence(4, [?t, ?c | fmt], need) do
    extract_sequence(5, [?c | fmt], need)
  end

  defp extract_sequence(4, [?t, ?s | fmt], need) do
    extract_sequence(5, [?s | fmt], need)
  end

  defp extract_sequence(4, [?t, ?p | fmt], need) do
    extract_sequence(5, [?p | fmt], need)
  end

  defp extract_sequence(4, [?t, ?P | fmt], need) do
    extract_sequence(5, [?P | fmt], need)
  end

  defp extract_sequence(4, [?t, ?w | fmt], need) do
    extract_sequence(5, [?w | fmt], need)
  end

  defp extract_sequence(4, [?t, ?W | fmt], need) do
    extract_sequence(5, [?W | fmt], need)
  end

  defp extract_sequence(4, [?t, c | _Fmt], _Need) do
    {:error, 'invalid control ~t' ++ [c]}
  end

  defp extract_sequence(4, [?l, ?p | fmt], need) do
    extract_sequence(5, [?p | fmt], need)
  end

  defp extract_sequence(4, [?l, ?t, ?p | fmt], need) do
    extract_sequence(5, [?p | fmt], need)
  end

  defp extract_sequence(4, [?l, ?P | fmt], need) do
    extract_sequence(5, [?P | fmt], need)
  end

  defp extract_sequence(4, [?l, ?t, ?P | fmt], need) do
    extract_sequence(5, [?P | fmt], need)
  end

  defp extract_sequence(4, [?l, ?t, c | _Fmt], _Need) do
    {:error, 'invalid control ~lt' ++ [c]}
  end

  defp extract_sequence(4, [?l, c | _Fmt], _Need) do
    {:error, 'invalid control ~l' ++ [c]}
  end

  defp extract_sequence(4, fmt, need) do
    extract_sequence(5, fmt, need)
  end

  defp extract_sequence(5, [c | fmt], need0) do
    case control_type(c, need0) do
      :error ->
        {:error, 'invalid control ~' ++ [c]}

      need1 ->
        {:ok, need1, fmt}
    end
  end

  defp extract_sequence(_, [], _Need) do
    {:error, 'truncated'}
  end

  defp extract_sequence_digits(fld, [c | fmt], need)
       when c >= ?0 and
              c <= ?9 do
    extract_sequence_digits(fld, fmt, need)
  end

  defp extract_sequence_digits(fld, fmt, need) do
    extract_sequence(fld + 1, fmt, need)
  end

  defp control_type(?~, need) do
    need
  end

  defp control_type(?c, need) do
    [:int | need]
  end

  defp control_type(?f, need) do
    [:float | need]
  end

  defp control_type(?e, need) do
    [:float | need]
  end

  defp control_type(?g, need) do
    [:float | need]
  end

  defp control_type(?s, need) do
    [:string | need]
  end

  defp control_type(?w, need) do
    [:term | need]
  end

  defp control_type(?p, need) do
    [:term | need]
  end

  defp control_type(?W, need) do
    [:int, :term | need]
  end

  defp control_type(?P, need) do
    [:int, :term | need]
  end

  defp control_type(?b, need) do
    [:term | need]
  end

  defp control_type(?B, need) do
    [:term | need]
  end

  defp control_type(?x, need) do
    [:string, :term | need]
  end

  defp control_type(?X, need) do
    [:string, :term | need]
  end

  defp control_type(?+, need) do
    [:term | need]
  end

  defp control_type(?#, need) do
    [:term | need]
  end

  defp control_type(?n, need) do
    need
  end

  defp control_type(?i, need) do
    [:term | need]
  end

  defp control_type(_C, _Need) do
    :error
  end

  defp local_functions(forms) do
    :gb_sets.from_list(
      for {:function, _, func, arity, _} <- forms do
        {func, arity}
      end
    )
  end

  defp is_local_function(localSet, {func, arity}) do
    :gb_sets.is_element({func, arity}, localSet)
  end

  defp is_imported_function(importSet, {func, arity}) do
    case :orddict.find({func, arity}, importSet) do
      {:ok, _Mod} ->
        true

      :error ->
        false
    end
  end

  defp is_imported_from_erlang(importSet, {func, arity}) do
    case :orddict.find({func, arity}, importSet) do
      {:ok, :erlang} ->
        true

      _ ->
        false
    end
  end

  defp auto_import_suppressed(compileFlags) do
    case :lists.member(:no_auto_import, compileFlags) do
      true ->
        :all

      false ->
        l0 =
          for {:no_auto_import, x} <- compileFlags do
            x
          end

        l1 =
          for {y, z} <- :lists.flatten(l0), is_atom(y), is_integer(z) do
            {y, z}
          end

        :gb_sets.from_list(l1)
    end
  end

  defp is_autoimport_suppressed(:all, {_Func, _Arity}) do
    true
  end

  defp is_autoimport_suppressed(noAutoSet, {func, arity}) do
    :gb_sets.is_element({func, arity}, noAutoSet)
  end

  defp bif_clash_specifically_disabled(st, {f, a}) do
    :lists.member({f, a}, r_lint(st, :nowarn_bif_clash))
  end

  defp no_guard_bif_clash(st, {f, a}) do
    not is_local_function(r_lint(st, :locals), {f, a}) and
      (not is_imported_function(
         r_lint(st, :imports),
         {f, a}
       ) or
         is_imported_from_erlang(
           r_lint(st, :imports),
           {f, a}
         )) and
      (not is_autoimport_suppressed(
         r_lint(st, :no_auto),
         {f, a}
       ) or
         is_imported_from_erlang(
           r_lint(st, :imports),
           {f, a}
         ))
  end

  defp maps_prepend(key, value, map) do
    case :maps.find(key, map) do
      {:ok, values} ->
        :maps.put(key, [value | values], map)

      :error ->
        :maps.put(key, [value], map)
    end
  end
end
