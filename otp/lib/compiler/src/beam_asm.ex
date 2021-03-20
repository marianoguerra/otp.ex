defmodule :m_beam_asm do
  use Bitwise
  import :lists, only: [duplicate: 2, keymember: 3, map: 2, member: 2, splitwith: 2]

  def module(code, extraChunks, compileInfo, compilerOpts) do
    {:ok, assemble(code, extraChunks, compileInfo, compilerOpts)}
  end

  defp assemble({mod, exp0, attr0, asm0, numLabels}, extraChunks, compileInfo, compilerOpts) do
    {1, dict0} = :beam_dict.atom(mod, :beam_dict.new())

    {0, dict1} =
      :beam_dict.fname(
        :erlang.atom_to_list(mod) ++ '.erl',
        dict0
      )

    dict2 = shared_fun_wrappers(compilerOpts, dict1)
    numFuncs = length(asm0)
    {asm, attr} = on_load(asm0, attr0)
    exp = :cerl_sets.from_list(exp0)
    {code, dict} = assemble_1(asm, exp, dict2, [])
    build_file(code, attr, dict, numLabels, numFuncs, extraChunks, compileInfo, compilerOpts)
  end

  defp shared_fun_wrappers(opts, dict) do
    case :proplists.get_bool(
           :no_shared_fun_wrappers,
           opts
         ) do
      false ->
        swap = :beam_opcodes.opcode(:swap, 2)
        :beam_dict.opcode(swap, dict)

      true ->
        dict
    end
  end

  defp on_load(fs0, attr0) do
    case :proplists.get_value(:on_load, attr0) do
      :undefined ->
        {fs0, attr0}

      [{name, 0}] ->
        fs =
          map(
            fn
              {:function, n, 0, entry, is0}
              when n === name ->
                is = insert_on_load_instruction(is0, entry)
                {:function, n, 0, entry, is}

              f ->
                f
            end,
            fs0
          )

        attr = :proplists.delete(:on_load, attr0)
        {fs, attr}
    end
  end

  defp insert_on_load_instruction(is0, entry) do
    {bef, [{:label, ^entry} = el | is]} =
      splitwith(
        fn
          {:label, l}
          when l === entry ->
            false

          _ ->
            true
        end,
        is0
      )

    bef ++ [el, :on_load | is]
  end

  defp assemble_1([{:function, name, arity, entry, asm} | t], exp, dict0, acc) do
    dict1 =
      case :cerl_sets.is_element(
             {name, arity},
             exp
           ) do
        true ->
          :beam_dict.export(name, arity, entry, dict0)

        false ->
          :beam_dict.local(name, arity, entry, dict0)
      end

    {code, dict2} = assemble_function(asm, acc, dict1)
    assemble_1(t, exp, dict2, code)
  end

  defp assemble_1([], _Exp, dict0, acc) do
    {intCodeEnd, dict1} = make_op(:int_code_end, dict0)

    {:erlang.list_to_binary(
       :lists.reverse(
         acc,
         [intCodeEnd]
       )
     ), dict1}
  end

  defp assemble_function([h | t], acc, dict0) do
    {code, dict} = make_op(h, dict0)
    assemble_function(t, [code | acc], dict)
  end

  defp assemble_function([], code, dict) do
    {code, dict}
  end

  defp build_file(code, attr, dict, numLabels, numFuncs, extraChunks, compileInfo, compilerOpts) do
    codeChunk =
      chunk(
        "Code",
        <<16::size(32), :beam_opcodes.format_number()::size(32),
          :beam_dict.highest_opcode(dict)::size(32), numLabels::size(32), numFuncs::size(32)>>,
        code
      )

    atomEncoding = atom_encoding(compilerOpts)

    {numAtoms, atomTab} =
      :beam_dict.atom_table(
        dict,
        atomEncoding
      )

    atomChunk = chunk(atom_chunk_name(atomEncoding), <<numAtoms::size(32)>>, atomTab)
    {numImps, impTab0} = :beam_dict.import_table(dict)
    imp = flatten_imports(impTab0)
    importChunk = chunk("ImpT", <<numImps::size(32)>>, imp)
    {numExps, expTab0} = :beam_dict.export_table(dict)
    exp = flatten_exports(expTab0)
    expChunk = chunk("ExpT", <<numExps::size(32)>>, exp)
    {numLocals, locals} = :beam_dict.local_table(dict)
    loc = flatten_exports(locals)
    locChunk = chunk("LocT", <<numLocals::size(32)>>, loc)
    {_, stringTab} = :beam_dict.string_table(dict)
    stringChunk = chunk("StrT", stringTab)

    lambdaChunk =
      case :beam_dict.lambda_table(dict) do
        {0, []} ->
          []

        {numLambdas, lambdaTab} ->
          chunk("FunT", <<numLambdas::size(32)>>, lambdaTab)
      end

    literalChunk =
      case :beam_dict.literal_table(dict) do
        {0, []} ->
          []

        {numLiterals, litTab0} ->
          litTab1 = [<<numLiterals::size(32)>>, litTab0]
          litTab = :zlib.compress(litTab1)
          chunk("LitT", <<:erlang.iolist_size(litTab1)::size(32)>>, litTab)
      end

    lineChunk = chunk("Line", build_line_table(dict))

    essentials0 = [
      atomChunk,
      codeChunk,
      stringChunk,
      importChunk,
      expChunk,
      lambdaChunk,
      literalChunk
    ]

    essentials1 =
      for c <- essentials0 do
        :erlang.iolist_to_binary(c)
      end

    mD5 = module_md5(essentials1)
    essentials = finalize_fun_table(essentials1, mD5)
    {attributes, compile} = build_attributes(attr, compileInfo, mD5)
    attrChunk = chunk("Attr", attributes)
    compileChunk = chunk("CInf", compile)

    checkedChunks =
      for {key, value} <- extraChunks do
        chunk(key, value)
      end

    chunks =
      case member(:slim, compilerOpts) do
        true ->
          [essentials, attrChunk]

        false ->
          [essentials, locChunk, attrChunk, compileChunk, checkedChunks, lineChunk]
      end

    build_form("BEAM", chunks)
  end

  defp atom_encoding(opts) do
    case :proplists.get_bool(:no_utf8_atoms, opts) do
      false ->
        :utf8

      true ->
        :latin1
    end
  end

  defp atom_chunk_name(:utf8) do
    "AtU8"
  end

  defp atom_chunk_name(:latin1) do
    "Atom"
  end

  defp finalize_fun_table(essentials, mD5) do
    for e <- essentials do
      finalize_fun_table_1(e, mD5)
    end
  end

  defp finalize_fun_table_1(
         <<"FunT", keep::size(8)-binary, table0::binary>>,
         mD5
       ) do
    <<uniq::size(27), _::size(101)-bits>> = mD5
    table = finalize_fun_table_2(table0, uniq, <<>>)
    <<"FunT", keep::binary, table::binary>>
  end

  defp finalize_fun_table_1(chunk, _) do
    chunk
  end

  defp finalize_fun_table_2(<<keep::size(20)-binary, 0::size(32), t::binary>>, uniq, acc) do
    finalize_fun_table_2(t, uniq, <<acc::binary, keep::binary, uniq::size(32)>>)
  end

  defp finalize_fun_table_2(<<>>, _, acc) do
    acc
  end

  defp build_form(id, chunks0)
       when byte_size(id) === 4 and
              is_list(chunks0) do
    chunks = :erlang.list_to_binary(chunks0)
    size = byte_size(chunks)
    0 = rem(size, 4)
    <<"FOR1", size + 4::size(32), id::binary, chunks::binary>>
  end

  defp chunk(id, contents)
       when byte_size(id) === 4 and
              is_binary(contents) do
    size = byte_size(contents)

    [
      <<id::binary, size::size(32)>>,
      contents
      | pad(size)
    ]
  end

  defp chunk(id, head, contents)
       when byte_size(id) === 4 and is_binary(head) and
              is_binary(contents) do
    size = byte_size(head) + byte_size(contents)
    [<<id::binary, size::size(32), head::binary>>, contents | pad(size)]
  end

  defp chunk(id, head, contents) when is_list(contents) do
    chunk(id, head, :erlang.list_to_binary(contents))
  end

  defp pad(size) do
    case rem(size, 4) do
      0 ->
        []

      rem ->
        duplicate(4 - rem, 0)
    end
  end

  defp flatten_exports(exps) do
    :erlang.list_to_binary(
      map(
        fn {f, a, l} ->
          <<f::size(32), a::size(32), l::size(32)>>
        end,
        exps
      )
    )
  end

  defp flatten_imports(imps) do
    :erlang.list_to_binary(
      map(
        fn {m, f, a} ->
          <<m::size(32), f::size(32), a::size(32)>>
        end,
        imps
      )
    )
  end

  defp build_attributes(attr, compile, mD5) do
    attrBinary =
      :erlang.term_to_binary(
        set_vsn_attribute(
          attr,
          mD5
        )
      )

    compileBinary =
      :erlang.term_to_binary([
        {:version, :EFE_TODO_COMPILER_VSN_MACRO}
        | compile
      ])

    {attrBinary, compileBinary}
  end

  defp build_line_table(dict) do
    {numLineInstrs, numFnames0, fnames0, numLines, lines0} = :beam_dict.line_table(dict)
    numFnames = numFnames0 - 1
    [_ | fnames1] = fnames0

    fnames2 =
      for f <- fnames1 do
        :unicode.characters_to_binary(f)
      end

    fnames =
      for f <- fnames2, into: <<>> do
        <<byte_size(f)::size(16), f::binary>>
      end

    lines1 = encode_line_items(lines0, 0)
    lines = :erlang.iolist_to_binary(lines1)
    ver = 0
    bits = 0

    <<ver::size(32), bits::size(32), numLineInstrs::size(32), numLines::size(32),
      numFnames::size(32), lines::binary, fnames::binary>>
  end

  defp set_vsn_attribute(attr, mD5) do
    case keymember(:vsn, 1, attr) do
      true ->
        attr

      false ->
        <<number::size(128)>> = mD5
        [{:vsn, [number]} | attr]
    end
  end

  defp module_md5(essentials0) do
    essentials = filter_essentials(essentials0)
    :erlang.md5(essentials)
  end

  defp filter_essentials([
         <<_Tag::size(4)-binary, sz::size(32), data::size(sz)-binary, _Padding::binary>>
         | t
       ]) do
    [data | filter_essentials(t)]
  end

  defp filter_essentials([<<>> | t]) do
    filter_essentials(t)
  end

  defp filter_essentials([]) do
    []
  end

  defp bif_type(:fnegate, 1) do
    {:op, :fnegate}
  end

  defp bif_type(:fadd, 2) do
    {:op, :fadd}
  end

  defp bif_type(:fsub, 2) do
    {:op, :fsub}
  end

  defp bif_type(:fmul, 2) do
    {:op, :fmul}
  end

  defp bif_type(:fdiv, 2) do
    {:op, :fdiv}
  end

  defp bif_type(_, 1) do
    :bif1
  end

  defp bif_type(_, 2) do
    :bif2
  end

  defp make_op({:%, _}, dict) do
    {[], dict}
  end

  defp make_op({:line, location}, dict0) do
    {index, dict} = :beam_dict.line(location, dict0)
    encode_op(:line, [index], dict)
  end

  defp make_op({:bif, bif, {:f, _}, [], dest}, dict) do
    encode_op(:bif0, [{:extfunc, :erlang, bif, 0}, dest], dict)
  end

  defp make_op(
         {:bif, :raise, _Fail, [_A1, _A2] = args, _Dest},
         dict
       ) do
    encode_op(:raise, args, dict)
  end

  defp make_op({:bif, bif, fail, args, dest}, dict) do
    arity = length(args)

    case bif_type(bif, arity) do
      {:op, op} ->
        make_op(
          :erlang.list_to_tuple([
            op,
            fail
            | args ++ [dest]
          ]),
          dict
        )

      bifOp when is_atom(bifOp) ->
        encode_op(
          bifOp,
          [
            fail,
            {:extfunc, :erlang, bif, arity}
            | args ++ [dest]
          ],
          dict
        )
    end
  end

  defp make_op({:gc_bif, bif, fail, live, args, dest}, dict) do
    arity = length(args)

    bifOp =
      case arity do
        1 ->
          :gc_bif1

        2 ->
          :gc_bif2

        3 ->
          :gc_bif3
      end

    encode_op(
      bifOp,
      [
        fail,
        live,
        {:extfunc, :erlang, bif, arity}
        | args ++ [dest]
      ],
      dict
    )
  end

  defp make_op(
         {:bs_add = op, fail, [src1, src2, unit], dest},
         dict
       ) do
    encode_op(op, [fail, src1, src2, unit, dest], dict)
  end

  defp make_op(
         {:test, cond__, fail, src, {:list, _} = ops},
         dict
       ) do
    encode_op(cond__, [fail, src, ops], dict)
  end

  defp make_op({:test, cond__, fail, ops}, dict)
       when is_list(ops) do
    encode_op(cond__, [fail | ops], dict)
  end

  defp make_op(
         {:test, cond__, fail, live, [op | ops], dst},
         dict
       )
       when is_list(ops) do
    encode_op(cond__, [fail, op, live | ops ++ [dst]], dict)
  end

  defp make_op(
         {:make_fun2, {:f, lbl}, _Index, _OldUniq, numFree},
         dict0
       ) do
    {fun, dict} = :beam_dict.lambda(lbl, numFree, dict0)
    make_op({:make_fun2, fun}, dict)
  end

  defp make_op(
         {:make_fun3, {:f, lbl}, _Index, _OldUniq, dst, {:list, env}},
         dict0
       ) do
    numFree = length(env)
    {fun, dict} = :beam_dict.lambda(lbl, numFree, dict0)
    make_op({:make_fun3, fun, dst, {:list, env}}, dict)
  end

  defp make_op({:kill, y}, dict) do
    make_op({:init, y}, dict)
  end

  defp make_op({name, arg1}, dict) do
    encode_op(name, [arg1], dict)
  end

  defp make_op({name, arg1, arg2}, dict) do
    encode_op(name, [arg1, arg2], dict)
  end

  defp make_op({name, arg1, arg2, arg3}, dict) do
    encode_op(name, [arg1, arg2, arg3], dict)
  end

  defp make_op({name, arg1, arg2, arg3, arg4}, dict) do
    encode_op(name, [arg1, arg2, arg3, arg4], dict)
  end

  defp make_op({name, arg1, arg2, arg3, arg4, arg5}, dict) do
    encode_op(name, [arg1, arg2, arg3, arg4, arg5], dict)
  end

  defp make_op(
         {name, arg1, arg2, arg3, arg4, arg5, arg6},
         dict
       ) do
    encode_op(name, [arg1, arg2, arg3, arg4, arg5, arg6], dict)
  end

  defp make_op(
         {name, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8},
         dict
       ) do
    encode_op(name, [arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8], dict)
  end

  defp make_op(op, dict) when is_atom(op) do
    encode_op(op, [], dict)
  end

  defp encode_op(name, args, dict0) when is_atom(name) do
    op = :beam_opcodes.opcode(name, length(args))
    dict = :beam_dict.opcode(op, dict0)
    encode_op_1(args, dict, op)
  end

  defp encode_op_1([a0 | as], dict0, acc) do
    {a, dict} = encode_arg(a0, dict0)
    encode_op_1(as, dict, [acc, a])
  end

  defp encode_op_1([], dict, acc) do
    {acc, dict}
  end

  defp flag_to_bit(:little) do
    2
  end

  defp flag_to_bit(:big) do
    0
  end

  defp flag_to_bit(:signed) do
    4
  end

  defp flag_to_bit(:unsigned) do
    0
  end

  defp flag_to_bit(:native) do
    16
  end

  defp flag_to_bit({:anno, _}) do
    0
  end

  defp encode_list([h | t], dict0, acc) when not is_list(h) do
    {enc, dict} = encode_arg(h, dict0)
    encode_list(t, dict, [acc, enc])
  end

  defp encode_list([], dict, acc) do
    {acc, dict}
  end

  def encode(tag, n) when n < 0 do
    encode1(tag, negative_to_bytes(n))
  end

  def encode(tag, n) when n < 16 do
    n <<< 4 ||| tag
  end

  def encode(tag, n) when n < 2048 do
    [(n >>> 3 &&& 224) ||| tag ||| 8, n &&& 255]
  end

  def encode(tag, n) do
    encode1(tag, to_bytes(n))
  end

  defp to_bytes(n) do
    bin = :binary.encode_unsigned(n)

    case bin do
      <<0::size(1), _::bits>> ->
        bin

      <<1::size(1), _::bits>> ->
        [0, bin]
    end
  end

  defp negative_to_bytes(n) when n >= -32768 do
    <<n::size(16)>>
  end

  defp negative_to_bytes(n) do
    bytes = byte_size(:binary.encode_unsigned(-n))
    bin = <<n::size(bytes)-unit(8)>>

    case bin do
      <<0::size(1), _::bits>> ->
        [255, bin]

      <<1::size(1), _::bits>> ->
        bin
    end
  end
end
