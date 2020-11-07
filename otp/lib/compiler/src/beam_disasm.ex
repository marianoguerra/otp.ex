defmodule :m_beam_disasm do
  use Bitwise
  @author "Kostis Sagonas"
  require Record

  Record.defrecord(:r_function, :function,
    name: :undefined,
    arity: :undefined,
    entry: :undefined,
    code: []
  )

  Record.defrecord(:r_beam_file, :beam_file,
    module: :undefined,
    labeled_exports: [],
    attributes: [],
    compile_info: [],
    code: []
  )

  def function__code(r_function(code: code)) do
    code
  end

  defp function__code_update(function, newCode) do
    r_function(function, code: newCode)
  end

  def format_error({:internal, error}) do
    :io_lib.format('~p: disassembly failed with reason ~P.', [:beam_disasm, error, 25])
  end

  def format_error({:error, module, error}) do
    :lists.flatten(module.format_error(error))
  end

  def file(file) do
    try do
      process_chunks(file)
    catch
      :error, reason ->
        {:error, :beam_disasm, {:internal, {reason, __STACKTRACE__}}}
    end
  end

  defp process_chunks(f) do
    case :beam_lib.chunks(
           f,
           [:atoms, 'Code', 'StrT', :indexed_imports, :labeled_exports]
         ) do
      {:ok,
       {module,
        [
          {:atoms, atomsList},
          {'Code', codeBin},
          {'StrT', strBin},
          {:indexed_imports, importsList},
          {:labeled_exports, exports}
        ]}} ->
        atoms = mk_atoms(atomsList)
        lambdaBin = optional_chunk(f, 'FunT')
        lambdas = beam_disasm_lambdas(lambdaBin, atoms)
        literalBin = optional_chunk(f, 'LitT')
        literals = beam_disasm_literals(literalBin)

        code =
          beam_disasm_code(
            codeBin,
            atoms,
            mk_imports(importsList),
            strBin,
            lambdas,
            literals,
            module
          )

        attributes =
          case optional_chunk(f, :attributes) do
            :none ->
              []

            atts when is_list(atts) ->
              atts
          end

        compInfo =
          case optional_chunk(f, 'CInf') do
            :none ->
              []

            compInfoBin when is_binary(compInfoBin) ->
              :erlang.binary_to_term(compInfoBin)
          end

        r_beam_file(
          module: module,
          labeled_exports: exports,
          attributes: attributes,
          compile_info: compInfo,
          code: code
        )

      error ->
        error
    end
  end

  defp optional_chunk(f, chunkTag) do
    case :beam_lib.chunks(f, [chunkTag]) do
      {:ok, {_Module, [{^chunkTag, chunk}]}} ->
        chunk

      {:error, :beam_lib, {:missing_chunk, _, _}} ->
        :none
    end
  end

  defp beam_disasm_lambdas(:none, _) do
    :none
  end

  defp beam_disasm_lambdas(<<_::size(32), tab::binary>>, atoms) do
    disasm_lambdas(tab, atoms, 0)
  end

  defp disasm_lambdas(
         <<f::size(32), a::size(32), lbl::size(32), index::size(32), numFree::size(32),
           oldUniq::size(32), more::binary>>,
         atoms,
         oldIndex
       ) do
    info = {lookup(f, atoms), a, lbl, index, numFree, oldUniq}
    [{oldIndex, info} | disasm_lambdas(more, atoms, oldIndex + 1)]
  end

  defp disasm_lambdas(<<>>, _, _) do
    []
  end

  defp beam_disasm_literals(:none) do
    :none
  end

  defp beam_disasm_literals(<<_::size(32), compressed::binary>>) do
    <<_::size(32), tab::binary>> = :zlib.uncompress(compressed)
    :gb_trees.from_orddict(disasm_literals(tab, 0))
  end

  defp disasm_literals(
         <<sz::size(32), ext::size(sz)-binary, t::binary>>,
         index
       ) do
    [
      {index, :erlang.binary_to_term(ext)}
      | disasm_literals(t, index + 1)
    ]
  end

  defp disasm_literals(<<>>, _) do
    []
  end

  defp beam_disasm_code(
         <<_SS::size(32), _IS::size(32), _OM::size(32), _L::size(32), _F::size(32),
           codeBin::binary>>,
         atoms,
         imports,
         str,
         lambdas,
         literals,
         m
       ) do
    code = :erlang.binary_to_list(codeBin)

    try do
      disasm_code(code, atoms, literals)
    catch
      :error, rsn ->
        :ok
        exit({:beam_disasm, 273, rsn})
    else
      disasmCode ->
        functions = get_function_chunks(disasmCode)
        labels = mk_labels(local_labels(functions))

        for function = r_function(code: is) <- functions do
          function__code_update(
            function,
            resolve_names(is, imports, str, labels, lambdas, literals, m)
          )
        end
    end
  end

  defp disasm_code([b | bs], atoms, literals) do
    {instr, restBs} = disasm_instr(b, bs, atoms, literals)
    [instr | disasm_code(restBs, atoms, literals)]
  end

  defp disasm_code([], _, _) do
    []
  end

  defp get_function_chunks([]) do
    exit({:beam_disasm, 299, :empty_code_segment})
  end

  defp get_function_chunks(code) do
    get_funs(labels_r(code, []))
  end

  defp labels_r([], r) do
    {r, []}
  end

  defp labels_r([{:label, _} = i | is], r) do
    labels_r(is, [i | r])
  end

  defp labels_r([{:line, _} = i | is], r) do
    labels_r(is, [i | r])
  end

  defp labels_r(is, r) do
    {r, is}
  end

  defp get_funs({[], []}) do
    []
  end

  defp get_funs({_, []}) do
    exit({:beam_disasm, 312, :no_func_info_in_code_segment})
  end

  defp get_funs(
         {lsR0,
          [
            {:func_info, [{:atom, m} = atomM, {:atom, f} = atomF, arityArg]}
            | code0
          ]}
       )
       when is_atom(m) and is_atom(f) do
    arity = resolve_arg_unsigned(arityArg)
    {lsR, code, restCode} = get_fun(code0, [])
    [{:label, [{:u, entry}]} | _] = code

    [
      r_function(
        name: f,
        arity: arity,
        entry: entry,
        code:
          :lists.reverse(
            lsR0,
            [{:func_info, atomM, atomF, arity} | code]
          )
      )
      | get_funs({lsR, restCode})
    ]
  end

  defp get_fun([{:func_info, _} | _] = is, r0) do
    {lsR, r} = labels_r(r0, [])
    {lsR, :lists.reverse(r), is}
  end

  defp get_fun([{:int_code_end, []}], r) do
    {[], :lists.reverse(r), []}
  end

  defp get_fun([i | is], r) do
    get_fun(is, [i | r])
  end

  defp get_fun([], r) do
    :io.format(:"warning: code segment did not end with int_code_end~n", [])
    {[], :lists.reverse(r), []}
  end

  defp local_labels(funs) do
    :lists.sort(
      :lists.foldl(
        fn f, r ->
          local_labels_1(function__code(f), r)
        end,
        [],
        funs
      )
    )
  end

  defp local_labels_1(code0, r) do
    code1 =
      :lists.dropwhile(
        fn
          {:label, _} ->
            true

          {:line, _} ->
            true

          {:func_info, _, _, _} ->
            false
        end,
        code0
      )

    [{:func_info, {:atom, m}, {:atom, f}, a} | code] = code1
    local_labels_2(code, r, {m, f, a})
  end

  defp local_labels_2([{:label, [{:u, l}]} | code], r, mFA) do
    local_labels_2(code, [{l, mFA} | r], mFA)
  end

  defp local_labels_2(_, r, _) do
    r
  end

  defp disasm_instr(b, bs, atoms, literals) do
    {symOp, arity} = :beam_opcodes.opname(b)

    case symOp do
      :select_val ->
        disasm_select_inst(:select_val, bs, atoms, literals)

      :select_tuple_arity ->
        disasm_select_inst(:select_tuple_arity, bs, atoms, literals)

      :put_map_assoc ->
        disasm_map_inst(:put_map_assoc, arity, bs, atoms, literals)

      :put_map_exact ->
        disasm_map_inst(:put_map_exact, arity, bs, atoms, literals)

      :get_map_elements ->
        disasm_map_inst(:get_map_elements, arity, bs, atoms, literals)

      :has_map_fields ->
        disasm_map_inst(:has_map_fields, arity, bs, atoms, literals)

      :put_tuple2 ->
        disasm_put_tuple2(bs, atoms, literals)

      :make_fun3 ->
        disasm_make_fun3(bs, atoms, literals)

      :init_yregs ->
        disasm_init_yregs(bs, atoms, literals)

      _ ->
        try do
          decode_n_args(arity, bs, atoms, literals)
        catch
          :error, rsn ->
            :ok
            exit({:beam_disasm, 390, {:cannot_disasm_instr, {symOp, arity, rsn}}})
        else
          {args, restBs} ->
            :ok
            {{symOp, args}, restBs}
        end
    end
  end

  defp disasm_select_inst(inst, bs, atoms, literals) do
    {x, bs1} = decode_arg(bs, atoms, literals)
    {f, bs2} = decode_arg(bs1, atoms, literals)
    {z, bs3} = decode_arg(bs2, atoms, literals)
    {u, bs4} = decode_arg(bs3, atoms, literals)
    {:u, len} = u
    {list, restBs} = decode_n_args(len, bs4, atoms, literals)
    {{inst, [x, f, {z, u, list}]}, restBs}
  end

  defp disasm_map_inst(inst, arity, bs0, atoms, literals) do
    {args0, bs1} = decode_n_args(arity, bs0, atoms, literals)
    [z | args1] = :lists.reverse(args0)
    args = :lists.reverse(args1)
    {u, bs2} = decode_arg(bs1, atoms, literals)
    {:u, len} = u
    {list, restBs} = decode_n_args(len, bs2, atoms, literals)
    {{inst, args ++ [{z, u, list}]}, restBs}
  end

  defp disasm_put_tuple2(bs, atoms, literals) do
    {x, bs1} = decode_arg(bs, atoms, literals)
    {z, bs2} = decode_arg(bs1, atoms, literals)
    {u, bs3} = decode_arg(bs2, atoms, literals)
    {:u, len} = u
    {list, restBs} = decode_n_args(len, bs3, atoms, literals)
    {{:put_tuple2, [x, {z, u, list}]}, restBs}
  end

  defp disasm_make_fun3(bs, atoms, literals) do
    {fun, bs1} = decode_arg(bs, atoms, literals)
    {dst, bs2} = decode_arg(bs1, atoms, literals)
    {z, bs3} = decode_arg(bs2, atoms, literals)
    {u, bs4} = decode_arg(bs3, atoms, literals)
    {:u, len} = u
    {list, restBs} = decode_n_args(len, bs4, atoms, literals)
    {{:make_fun3, [fun, dst, {z, u, list}]}, restBs}
  end

  defp disasm_init_yregs(bs1, atoms, literals) do
    {z, bs2} = decode_arg(bs1, atoms, literals)
    {u, bs3} = decode_arg(bs2, atoms, literals)
    {:u, len} = u
    {list, restBs} = decode_n_args(len, bs3, atoms, literals)
    {{:init_yregs, [{z, u, list}]}, restBs}
  end

  defp decode_arg([b | bs]) do
    tag = decode_tag(b &&& 7)
    :ok

    case tag do
      :z ->
        decode_z_tagged(tag, b, bs, :no_literals)

      _ ->
        decode_int(tag, b, bs)
    end
  end

  defp decode_arg([b | bs0], atoms, literals) do
    tag = decode_tag(b &&& 7)
    :ok

    case tag do
      :z ->
        decode_z_tagged(tag, b, bs0, literals)

      :a ->
        case decode_int(tag, b, bs0) do
          {{:a, 0}, bs} ->
            {nil, bs}

          {{:a, i}, bs} ->
            {{:atom, lookup(i, atoms)}, bs}
        end

      _ ->
        decode_int(tag, b, bs0)
    end
  end

  defp decode_int(tag, b, bs) when b &&& 8 === 0 do
    n = b >>> 4
    {{tag, n}, bs}
  end

  defp decode_int(tag, b, bs) when b &&& 16 === 0 do
    [b1 | bs1] = bs
    val0 = b &&& 224
    n = val0 <<< 3 ||| b1
    :ok
    {{tag, n}, bs1}
  end

  defp decode_int(tag, b, bs) do
    {len, bs1} = decode_int_length(b, bs)
    {intBs, remBs} = take_bytes(len, bs1)
    n = build_arg(intBs)
    [f | _] = intBs

    num =
      cond do
        f > 127 and tag === :i ->
          decode_negative(n, len)

        true ->
          n
      end

    :ok
    {{tag, num}, remBs}
  end

  defp decode_int_length(b, bs) do
    case b >>> 5 do
      7 ->
        {arg, argBs} = decode_arg(bs)

        case arg do
          {:u, l} ->
            {l + 9, argBs}

          _ ->
            exit({:beam_disasm, 536, {:decode_int, :weird_bignum_sublength, arg}})
        end

      l ->
        {l + 2, bs}
    end
  end

  defp decode_negative(n, len) do
    n - (1 <<< (len * 8))
  end

  defp decode_z_tagged(tag, b, bs, literals) when b &&& 8 === 0 do
    n = b >>> 4

    case n do
      0 ->
        decode_float(bs)

      1 ->
        {{tag, n}, bs}

      2 ->
        decode_fr(bs)

      3 ->
        decode_alloc_list(bs, literals)

      4 ->
        {{:u, litIndex}, restBs} = decode_arg(bs)

        case :gb_trees.get(litIndex, literals) do
          float when is_float(float) ->
            {{:float, float}, restBs}

          literal ->
            {{:literal, literal}, restBs}
        end

      _ ->
        exit({:beam_disasm, 571, {:decode_z_tagged, {:invalid_extended_tag, n}}})
    end
  end

  defp decode_z_tagged(_, b, _, _) do
    exit({:beam_disasm, 574, {:decode_z_tagged, {:weird_value, b}}})
  end

  defp decode_float(bs) do
    {fL, restBs} = take_bytes(8, bs)
    <<float::size(64)-float>> = :erlang.list_to_binary(fL)
    {{:float, float}, restBs}
  end

  defp decode_fr(bs) do
    {{:u, fr}, restBs} = decode_arg(bs)
    {{:fr, fr}, restBs}
  end

  defp decode_alloc_list(bs, literals) do
    {{:u, n}, restBs} = decode_arg(bs)
    decode_alloc_list_1(n, literals, restBs, [])
  end

  defp decode_alloc_list_1(0, _Literals, restBs, acc) do
    {{:u, {:alloc, :lists.reverse(acc)}}, restBs}
  end

  defp decode_alloc_list_1(n, literals, bs0, acc) do
    {{:u, type}, bs1} = decode_arg(bs0)
    {{:u, val}, bs} = decode_arg(bs1)

    res =
      case type do
        0 ->
          {:words, val}

        1 ->
          {:floats, val}

        2 ->
          {:funs, val}
      end

    decode_alloc_list_1(n - 1, literals, bs, [res | acc])
  end

  defp take_bytes(n, bs) do
    take_bytes(n, bs, [])
  end

  defp take_bytes(n, [b | bs], acc) when n > 0 do
    take_bytes(n - 1, bs, [b | acc])
  end

  defp take_bytes(0, bs, acc) do
    {:lists.reverse(acc), bs}
  end

  defp build_arg(bs) do
    build_arg(bs, 0)
  end

  defp build_arg([b | bs], n) do
    build_arg(bs, n <<< 8 ||| b)
  end

  defp build_arg([], n) do
    n
  end

  defp decode_n_args(n, bs, atoms, literals) when n >= 0 do
    decode_n_args(n, [], bs, atoms, literals)
  end

  defp decode_n_args(n, acc, bs0, atoms, literals) when n > 0 do
    {a1, bs} = decode_arg(bs0, atoms, literals)
    decode_n_args(n - 1, [a1 | acc], bs, atoms, literals)
  end

  defp decode_n_args(0, acc, bs, _, _) do
    {:lists.reverse(acc), bs}
  end

  defp resolve_names(fun, imports, str, lbls, lambdas, literals, m) do
    for instr <- fun do
      resolve_inst(instr, imports, str, lbls, lambdas, literals, m)
    end
  end

  defp resolve_inst({:make_fun2, args}, _, _, _, lambdas, _, m) do
    [oldIndex] = resolve_args(args)
    {^oldIndex, {f, a, _Lbl, _Index, numFree, oldUniq}} = :lists.keyfind(oldIndex, 1, lambdas)
    {:make_fun2, {m, f, a}, oldIndex, oldUniq, numFree}
  end

  defp resolve_inst({:make_fun3, [fun, dst, {{:z, 1}, {:u, _}, env0}]}, _, _, _, lambdas, _, m) do
    oldIndex = resolve_arg(fun)
    env1 = resolve_args(env0)
    {^oldIndex, {f, a, _Lbl, _Index, _NumFree, oldUniq}} = :lists.keyfind(oldIndex, 1, lambdas)
    {:make_fun3, {m, f, a}, oldIndex, oldUniq, dst, {:list, env1}}
  end

  defp resolve_inst(instr, imports, str, lbls, _Lambdas, _Literals, _M) do
    resolve_inst(instr, imports, str, lbls)
  end

  defp resolve_inst({:label, [{:u, l}]}, _, _, _) do
    {:label, l}
  end

  defp resolve_inst(funcInfo, _, _, _)
       when :erlang.element(
              1,
              funcInfo
            ) === :func_info do
    funcInfo
  end

  defp resolve_inst({:call, [{:u, n}, {:f, l}]}, _, _, lbls) do
    {:call, n, lookup(l, lbls)}
  end

  defp resolve_inst({:call_last, [{:u, n}, {:f, l}, {:u, u}]}, _, _, lbls) do
    {:call_last, n, lookup(l, lbls), u}
  end

  defp resolve_inst({:call_only, [{:u, n}, {:f, l}]}, _, _, lbls) do
    {:call_only, n, lookup(l, lbls)}
  end

  defp resolve_inst({:call_ext, [{:u, n}, {:u, mFAix}]}, imports, _, _) do
    {:call_ext, n, lookup(mFAix + 1, imports)}
  end

  defp resolve_inst({:call_ext_last, [{:u, n}, {:u, mFAix}, {:u, x}]}, imports, _, _) do
    {:call_ext_last, n, lookup(mFAix + 1, imports), x}
  end

  defp resolve_inst({:bif0, args}, imports, _, _) do
    [bif, reg] = resolve_args(args)

    {:extfunc, _Mod, bifName, _Arity} =
      lookup(
        bif + 1,
        imports
      )

    {:bif, bifName, :nofail, [], reg}
  end

  defp resolve_inst({:bif1, args}, imports, _, _) do
    [f, bif, a1, reg] = resolve_args(args)

    {:extfunc, _Mod, bifName, _Arity} =
      lookup(
        bif + 1,
        imports
      )

    {:bif, bifName, f, [a1], reg}
  end

  defp resolve_inst({:bif2, args}, imports, _, _) do
    [f, bif, a1, a2, reg] = resolve_args(args)

    {:extfunc, _Mod, bifName, _Arity} =
      lookup(
        bif + 1,
        imports
      )

    {:bif, bifName, f, [a1, a2], reg}
  end

  defp resolve_inst({:allocate, [{:u, x0}, {:u, x1}]}, _, _, _) do
    {:allocate, x0, x1}
  end

  defp resolve_inst({:allocate_heap, [{:u, x0}, {:u, x1}, {:u, x2}]}, _, _, _) do
    {:allocate_heap, x0, x1, x2}
  end

  defp resolve_inst({:allocate_zero, [{:u, x0}, {:u, x1}]}, _, _, _) do
    {:allocate_zero, x0, x1}
  end

  defp resolve_inst({:allocate_heap_zero, [{:u, x0}, {:u, x1}, {:u, x2}]}, _, _, _) do
    {:allocate_heap_zero, x0, x1, x2}
  end

  defp resolve_inst({:test_heap, [{:u, x0}, {:u, x1}]}, _, _, _) do
    {:test_heap, x0, x1}
  end

  defp resolve_inst({:init, [dst]}, _, _, _) do
    {:init, dst}
  end

  defp resolve_inst({:deallocate, [{:u, l}]}, _, _, _) do
    {:deallocate, l}
  end

  defp resolve_inst({:return, []}, _, _, _) do
    :return
  end

  defp resolve_inst({:send, []}, _, _, _) do
    :send
  end

  defp resolve_inst({:remove_message, []}, _, _, _) do
    :remove_message
  end

  defp resolve_inst({:timeout, []}, _, _, _) do
    :timeout
  end

  defp resolve_inst({:loop_rec, [lbl, dst]}, _, _, _) do
    {:loop_rec, lbl, dst}
  end

  defp resolve_inst({:loop_rec_end, [lbl]}, _, _, _) do
    {:loop_rec_end, lbl}
  end

  defp resolve_inst({:wait, [lbl]}, _, _, _) do
    {:wait, lbl}
  end

  defp resolve_inst({:wait_timeout, [lbl, int]}, _, _, _) do
    {:wait_timeout, lbl, resolve_arg(int)}
  end

  defp resolve_inst({:is_lt = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:is_ge = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:is_eq = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:is_ne = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:is_eq_exact = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:is_ne_exact = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:is_integer = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:is_float = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:is_number = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:is_atom = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:is_pid = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:is_reference = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:is_port = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:is_nil = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:is_binary = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:is_list = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:is_nonempty_list = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:is_tuple = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:test_arity = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:is_tagged_tuple = i, args0}, _, _, _) do
    [f | args] = resolve_args(args0)
    {:test, i, f, args}
  end

  defp resolve_inst({:select_val, args}, _, _, _) do
    [reg, fLbl, {{:z, 1}, {:u, _Len}, list0}] = args
    list = resolve_args(list0)
    {:select_val, reg, fLbl, {:list, list}}
  end

  defp resolve_inst({:select_tuple_arity, args}, _, _, _) do
    [reg, fLbl, {{:z, 1}, {:u, _Len}, list0}] = args
    list = resolve_args(list0)
    {:select_tuple_arity, reg, fLbl, {:list, list}}
  end

  defp resolve_inst({:jump, [lbl]}, _, _, _) do
    {:jump, lbl}
  end

  defp resolve_inst({:catch, [dst, lbl]}, _, _, _) do
    {:catch, dst, lbl}
  end

  defp resolve_inst({:catch_end, [dst]}, _, _, _) do
    {:catch_end, dst}
  end

  defp resolve_inst({:move, [src, dst]}, _, _, _) do
    {:move, resolve_arg(src), dst}
  end

  defp resolve_inst({:get_list, [src, dst1, dst2]}, _, _, _) do
    {:get_list, src, dst1, dst2}
  end

  defp resolve_inst({:get_tuple_element, [src, {:u, off}, dst]}, _, _, _) do
    {:get_tuple_element, resolve_arg(src), off, resolve_arg(dst)}
  end

  defp resolve_inst({:set_tuple_element, [src, dst, {:u, off}]}, _, _, _) do
    {:set_tuple_element, resolve_arg(src), resolve_arg(dst), off}
  end

  defp resolve_inst({:put_list, [src1, src2, dst]}, _, _, _) do
    {:put_list, resolve_arg(src1), resolve_arg(src2), dst}
  end

  defp resolve_inst({:put_tuple, [{:u, arity}, dst]}, _, _, _) do
    {:put_tuple, arity, dst}
  end

  defp resolve_inst({:put, [src]}, _, _, _) do
    {:put, resolve_arg(src)}
  end

  defp resolve_inst({:badmatch, [x]}, _, _, _) do
    {:badmatch, resolve_arg(x)}
  end

  defp resolve_inst({:if_end, []}, _, _, _) do
    :if_end
  end

  defp resolve_inst({:case_end, [x]}, _, _, _) do
    {:case_end, resolve_arg(x)}
  end

  defp resolve_inst({:call_fun, [{:u, n}]}, _, _, _) do
    {:call_fun, n}
  end

  defp resolve_inst({:is_function = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:call_ext_only, [{:u, n}, {:u, mFAix}]}, imports, _, _) do
    {:call_ext_only, n, lookup(mFAix + 1, imports)}
  end

  defp resolve_inst({:bs_put_integer, [lbl, arg2, {:u, n}, {:u, u}, arg5]}, _, _, _) do
    [a2, a5] = resolve_args([arg2, arg5])
    {:bs_put_integer, lbl, a2, n, decode_field_flags(u), a5}
  end

  defp resolve_inst({:bs_put_binary, [lbl, arg2, {:u, n}, {:u, u}, arg5]}, _, _, _) do
    [a2, a5] = resolve_args([arg2, arg5])
    {:bs_put_binary, lbl, a2, n, decode_field_flags(u), a5}
  end

  defp resolve_inst({:bs_put_float, [lbl, arg2, {:u, n}, {:u, u}, arg5]}, _, _, _) do
    [a2, a5] = resolve_args([arg2, arg5])
    {:bs_put_float, lbl, a2, n, decode_field_flags(u), a5}
  end

  defp resolve_inst({:bs_put_string, [{:u, len}, {:u, off}]}, _, strings, _) do
    string =
      cond do
        len > 0 ->
          :erlang.binary_to_list(strings, off + 1, off + len)

        true ->
          ''
      end

    {:bs_put_string, len, {:string, string}}
  end

  defp resolve_inst({:fclearerror, []}, _, _, _) do
    :fclearerror
  end

  defp resolve_inst({:fcheckerror, [arg]}, _, _, _) do
    {:fcheckerror, resolve_arg(arg)}
  end

  defp resolve_inst({:fmove, args}, _, _, _) do
    [fR, reg] = resolve_args(args)
    {:fmove, fR, reg}
  end

  defp resolve_inst({:fconv, args}, _, _, _) do
    [reg, fR] = resolve_args(args)
    {:fconv, reg, fR}
  end

  defp resolve_inst({:fadd = i, args}, _, _, _) do
    [f, a1, a2, reg] = resolve_args(args)
    {:arithfbif, i, f, [a1, a2], reg}
  end

  defp resolve_inst({:fsub = i, args}, _, _, _) do
    [f, a1, a2, reg] = resolve_args(args)
    {:arithfbif, i, f, [a1, a2], reg}
  end

  defp resolve_inst({:fmul = i, args}, _, _, _) do
    [f, a1, a2, reg] = resolve_args(args)
    {:arithfbif, i, f, [a1, a2], reg}
  end

  defp resolve_inst({:fdiv = i, args}, _, _, _) do
    [f, a1, a2, reg] = resolve_args(args)
    {:arithfbif, i, f, [a1, a2], reg}
  end

  defp resolve_inst({:fnegate, args}, _, _, _) do
    [f, arg, reg] = resolve_args(args)
    {:arithfbif, :fnegate, f, [arg], reg}
  end

  defp resolve_inst({:try, [reg, lbl]}, _, _, _) do
    {:try, reg, lbl}
  end

  defp resolve_inst({:try_end, [reg]}, _, _, _) do
    {:try_end, reg}
  end

  defp resolve_inst({:try_case, [reg]}, _, _, _) do
    {:try_case, reg}
  end

  defp resolve_inst({:try_case_end, [arg]}, _, _, _) do
    {:try_case_end, resolve_arg(arg)}
  end

  defp resolve_inst({:raise, [_Reg1, _Reg2] = regs}, _, _, _) do
    {:raise, {:f, 0}, regs, {:x, 0}}
  end

  defp resolve_inst({:bs_init2, [lbl, arg2, {:u, w}, {:u, r}, {:u, f}, arg6]}, _, _, _) do
    [a2, a6] = resolve_args([arg2, arg6])
    {:bs_init2, lbl, a2, w, r, decode_field_flags(f), a6}
  end

  defp resolve_inst({:bs_add = i, [lbl, arg2, arg3, arg4, arg5]}, _, _, _) do
    [a2, a3, a4, a5] = resolve_args([arg2, arg3, arg4, arg5])
    {i, lbl, [a2, a3, a4], a5}
  end

  defp resolve_inst({:apply, [{:u, arity}]}, _, _, _) do
    {:apply, arity}
  end

  defp resolve_inst({:apply_last, [{:u, arity}, {:u, d}]}, _, _, _) do
    {:apply_last, arity, d}
  end

  defp resolve_inst({:is_boolean = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:is_function2 = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:bs_start_match2 = i, [f, reg, {:u, live}, {:u, max}, ms]}, _, _, _) do
    {:test, i, f, [reg, live, max, ms]}
  end

  defp resolve_inst(
         {:bs_get_integer2 = i, [lbl, ms, {:u, live}, arg2, {:u, n}, {:u, u}, arg5]},
         _,
         _,
         _
       ) do
    [a2, a5] = resolve_args([arg2, arg5])
    {:test, i, lbl, [ms, live, a2, n, decode_field_flags(u), a5]}
  end

  defp resolve_inst(
         {:bs_get_binary2 = i, [lbl, ms, {:u, live}, arg2, {:u, n}, {:u, u}, arg5]},
         _,
         _,
         _
       ) do
    [a2, a5] = resolve_args([arg2, arg5])
    {:test, i, lbl, [ms, live, a2, n, decode_field_flags(u), a5]}
  end

  defp resolve_inst(
         {:bs_get_float2 = i, [lbl, ms, {:u, live}, arg2, {:u, n}, {:u, u}, arg5]},
         _,
         _,
         _
       ) do
    [a2, a5] = resolve_args([arg2, arg5])
    {:test, i, lbl, [ms, live, a2, n, decode_field_flags(u), a5]}
  end

  defp resolve_inst({:bs_skip_bits2 = i, [lbl, ms, arg2, {:u, n}, {:u, u}]}, _, _, _) do
    a2 = resolve_arg(arg2)
    {:test, i, lbl, [ms, a2, n, decode_field_flags(u)]}
  end

  defp resolve_inst({:bs_test_tail2 = i, [f, ms, {:u, n}]}, _, _, _) do
    {:test, i, f, [ms, n]}
  end

  defp resolve_inst({:bs_save2 = i, [ms, {:u, n}]}, _, _, _) do
    {i, ms, n}
  end

  defp resolve_inst({:bs_restore2 = i, [ms, {:u, n}]}, _, _, _) do
    {i, ms, n}
  end

  defp resolve_inst({:bs_save2 = i, [ms, {:atom, _} = atom]}, _, _, _) do
    {i, ms, atom}
  end

  defp resolve_inst({:bs_restore2 = i, [ms, {:atom, _} = atom]}, _, _, _) do
    {i, ms, atom}
  end

  defp resolve_inst({:gc_bif1, args}, imports, _, _) do
    [f, live, bif, a1, reg] = resolve_args(args)

    {:extfunc, _Mod, bifName, _Arity} =
      lookup(
        bif + 1,
        imports
      )

    {:gc_bif, bifName, f, live, [a1], reg}
  end

  defp resolve_inst({:gc_bif2, args}, imports, _, _) do
    [f, live, bif, a1, a2, reg] = resolve_args(args)

    {:extfunc, _Mod, bifName, _Arity} =
      lookup(
        bif + 1,
        imports
      )

    {:gc_bif, bifName, f, live, [a1, a2], reg}
  end

  defp resolve_inst({:gc_bif3, args}, imports, _, _) do
    [f, live, bif, a1, a2, a3, reg] = resolve_args(args)

    {:extfunc, _Mod, bifName, _Arity} =
      lookup(
        bif + 1,
        imports
      )

    {:gc_bif, bifName, f, live, [a1, a2, a3], reg}
  end

  defp resolve_inst({:is_bitstr = i, args0}, _, _, _) do
    [l | args] = resolve_args(args0)
    {:test, i, l, args}
  end

  defp resolve_inst({:bs_context_to_binary = i, [reg0]}, _, _, _) do
    reg = resolve_arg(reg0)
    {i, reg}
  end

  defp resolve_inst({:bs_test_unit = i, [f, ms, {:u, n}]}, _, _, _) do
    {:test, i, f, [ms, n]}
  end

  defp resolve_inst({:bs_match_string = i, [f, ms, {:u, bits}, {:u, off}]}, _, strings, _) do
    len = div(bits + 7, 8)

    string =
      cond do
        len > 0 ->
          <<_::size(off)-binary, bin::size(len)-binary, _::binary>> = strings
          bin

        true ->
          <<>>
      end

    {:test, i, f, [ms, bits, string]}
  end

  defp resolve_inst({:bs_init_writable = i, []}, _, _, _) do
    i
  end

  defp resolve_inst(
         {:bs_append = i, [lbl, arg2, {:u, w}, {:u, r}, {:u, u}, arg6, {:u, f}, arg8]},
         _,
         _,
         _
       ) do
    [a2, a6, a8] = resolve_args([arg2, arg6, arg8])
    {i, lbl, a2, w, r, u, a6, decode_field_flags(f), a8}
  end

  defp resolve_inst({:bs_private_append = i, [lbl, arg2, {:u, u}, arg4, {:u, f}, arg6]}, _, _, _) do
    [a2, a4, a6] = resolve_args([arg2, arg4, arg6])
    {i, lbl, a2, u, a4, decode_field_flags(f), a6}
  end

  defp resolve_inst({:trim = i, [{:u, n}, {:u, remaining}]}, _, _, _) do
    {i, n, remaining}
  end

  defp resolve_inst({:bs_init_bits, [lbl, arg2, {:u, w}, {:u, r}, {:u, f}, arg6]}, _, _, _) do
    [a2, a6] = resolve_args([arg2, arg6])
    {:bs_init_bits, lbl, a2, w, r, decode_field_flags(f), a6}
  end

  defp resolve_inst({:bs_get_utf8 = i, [lbl, arg2, arg3, {:u, u}, arg4]}, _, _, _) do
    [a2, a3, a4] = resolve_args([arg2, arg3, arg4])
    {:test, i, lbl, [a2, a3, decode_field_flags(u), a4]}
  end

  defp resolve_inst({:bs_skip_utf8 = i, [lbl, arg2, arg3, {:u, u}]}, _, _, _) do
    [a2, a3] = resolve_args([arg2, arg3])
    {:test, i, lbl, [a2, a3, decode_field_flags(u)]}
  end

  defp resolve_inst({:bs_get_utf16 = i, [lbl, arg2, arg3, {:u, u}, arg4]}, _, _, _) do
    [a2, a3, a4] = resolve_args([arg2, arg3, arg4])
    {:test, i, lbl, [a2, a3, decode_field_flags(u), a4]}
  end

  defp resolve_inst({:bs_skip_utf16 = i, [lbl, arg2, arg3, {:u, u}]}, _, _, _) do
    [a2, a3] = resolve_args([arg2, arg3])
    {:test, i, lbl, [a2, a3, decode_field_flags(u)]}
  end

  defp resolve_inst({:bs_get_utf32 = i, [lbl, arg2, arg3, {:u, u}, arg4]}, _, _, _) do
    [a2, a3, a4] = resolve_args([arg2, arg3, arg4])
    {:test, i, lbl, [a2, a3, decode_field_flags(u), a4]}
  end

  defp resolve_inst({:bs_skip_utf32 = i, [lbl, arg2, arg3, {:u, u}]}, _, _, _) do
    [a2, a3] = resolve_args([arg2, arg3])
    {:test, i, lbl, [a2, a3, decode_field_flags(u)]}
  end

  defp resolve_inst({:bs_utf8_size = i, [lbl, arg2, arg3]}, _, _, _) do
    [a2, a3] = resolve_args([arg2, arg3])
    {i, lbl, a2, a3}
  end

  defp resolve_inst({:bs_put_utf8 = i, [lbl, {:u, u}, arg3]}, _, _, _) do
    a3 = resolve_arg(arg3)
    {i, lbl, decode_field_flags(u), a3}
  end

  defp resolve_inst({:bs_utf16_size = i, [lbl, arg2, arg3]}, _, _, _) do
    [a2, a3] = resolve_args([arg2, arg3])
    {i, lbl, a2, a3}
  end

  defp resolve_inst({:bs_put_utf16 = i, [lbl, {:u, u}, arg3]}, _, _, _) do
    a3 = resolve_arg(arg3)
    {i, lbl, decode_field_flags(u), a3}
  end

  defp resolve_inst({:bs_put_utf32 = i, [lbl, {:u, u}, arg3]}, _, _, _) do
    a3 = resolve_arg(arg3)
    {i, lbl, decode_field_flags(u), a3}
  end

  defp resolve_inst({:on_load, []}, _, _, _) do
    :on_load
  end

  defp resolve_inst({:recv_mark, [lbl]}, _, _, _) do
    {:recv_mark, lbl}
  end

  defp resolve_inst({:recv_set, [lbl]}, _, _, _) do
    {:recv_set, lbl}
  end

  defp resolve_inst({:line, [index]}, _, _, _) do
    {:line, resolve_arg(index)}
  end

  defp resolve_inst({:put_map_assoc, args}, _, _, _) do
    [fLbl, src, dst, {:u, n}, {{:z, 1}, {:u, _Len}, list0}] = args
    list = resolve_args(list0)
    {:put_map_assoc, fLbl, src, dst, n, {:list, list}}
  end

  defp resolve_inst({:put_map_exact, args}, _, _, _) do
    [fLbl, src, dst, {:u, n}, {{:z, 1}, {:u, _Len}, list0}] = args
    list = resolve_args(list0)
    {:put_map_exact, fLbl, src, dst, n, {:list, list}}
  end

  defp resolve_inst({:is_map = i, args0}, _, _, _) do
    [fLbl | args] = resolve_args(args0)
    {:test, i, fLbl, args}
  end

  defp resolve_inst({:has_map_fields, args0}, _, _, _) do
    [fLbl, src, {{:z, 1}, {:u, _Len}, list0}] = args0
    list = resolve_args(list0)
    {:test, :has_map_fields, fLbl, src, {:list, list}}
  end

  defp resolve_inst({:get_map_elements, args0}, _, _, _) do
    [fLbl, src, {{:z, 1}, {:u, _Len}, list0}] = args0
    list = resolve_args(list0)
    {:get_map_elements, fLbl, src, {:list, list}}
  end

  defp resolve_inst({:build_stacktrace, []}, _, _, _) do
    :build_stacktrace
  end

  defp resolve_inst({:raw_raise, []}, _, _, _) do
    :raw_raise
  end

  defp resolve_inst({:get_hd, [src, dst]}, _, _, _) do
    {:get_hd, src, dst}
  end

  defp resolve_inst({:get_tl, [src, dst]}, _, _, _) do
    {:get_tl, src, dst}
  end

  defp resolve_inst({:put_tuple2, [dst, {{:z, 1}, {:u, _}, list0}]}, _, _, _) do
    list = resolve_args(list0)
    {:put_tuple2, dst, {:list, list}}
  end

  defp resolve_inst({:bs_start_match3, [fail, bin, live, dst]}, _, _, _) do
    {:bs_start_match3, fail, bin, live, dst}
  end

  defp resolve_inst({:bs_get_tail, [src, dst, live]}, _, _, _) do
    {:bs_get_tail, src, dst, live}
  end

  defp resolve_inst({:bs_get_position, [src, dst, live]}, _, _, _) do
    {:bs_get_position, src, dst, live}
  end

  defp resolve_inst({:bs_set_position, [src, dst]}, _, _, _) do
    {:bs_set_position, src, dst}
  end

  defp resolve_inst({:bs_start_match4, [fail, live, src, dst]}, _, _, _) do
    {:bs_start_match4, fail, live, src, dst}
  end

  defp resolve_inst({:swap, [_, _] = list}, _, _, _) do
    [r1, r2] = resolve_args(list)
    {:swap, r1, r2}
  end

  defp resolve_inst({:init_yregs, [{{:z, 1}, {:u, _}, list0}]}, _, _, _) do
    list = resolve_args(list0)
    {:init_yregs, {:list, list}}
  end

  defp resolve_inst(x, _, _, _) do
    exit({:beam_disasm, 1171, {:resolve_inst, x}})
  end

  defp resolve_args(args) do
    for a <- args do
      resolve_arg(a)
    end
  end

  defp resolve_arg({:x, n} = arg)
       when is_integer(n) and
              n >= 0 do
    arg
  end

  defp resolve_arg({:y, n} = arg)
       when is_integer(n) and
              n >= 0 do
    arg
  end

  defp resolve_arg({:fr, n} = arg)
       when is_integer(n) and
              n >= 0 do
    arg
  end

  defp resolve_arg({:f, n} = arg)
       when is_integer(n) and
              n >= 0 do
    arg
  end

  defp resolve_arg({:u, _} = arg) do
    resolve_arg_unsigned(arg)
  end

  defp resolve_arg({:i, _} = arg) do
    resolve_arg_integer(arg)
  end

  defp resolve_arg({:atom, atom} = arg) when is_atom(atom) do
    arg
  end

  defp resolve_arg({:float, f} = arg) when is_float(f) do
    arg
  end

  defp resolve_arg({:literal, _} = arg) do
    arg
  end

  defp resolve_arg(nil) do
    nil
  end

  defp resolve_arg_unsigned({:u, n}) when is_integer(n) and n >= 0 do
    n
  end

  defp resolve_arg_integer({:i, n}) when is_integer(n) do
    {:integer, n}
  end

  defp decode_field_flags(fF) do
    {:field_flags, fF}
  end

  defp mk_imports(importList) do
    :gb_trees.from_orddict(
      for {i, m, f, a} <- importList do
        {i, {:extfunc, m, f, a}}
      end
    )
  end

  defp mk_atoms(atomList) do
    :gb_trees.from_orddict(atomList)
  end

  defp mk_labels(labelList) do
    :gb_trees.from_orddict(labelList)
  end

  defp lookup(i, imports) do
    :gb_trees.get(i, imports)
  end
end
