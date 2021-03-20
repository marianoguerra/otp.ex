defmodule :m_beam_dict do
  use Bitwise
  require Record

  Record.defrecord(:r_asm, :asm,
    atoms: %{},
    exports: [],
    locals: [],
    imports: :gb_trees.empty(),
    strings: <<>>,
    lambdas: {0, []},
    wrappers: %{},
    literals: %{},
    fnames: %{},
    lines: %{},
    num_lines: 0,
    next_import: 0,
    string_offset: 0,
    next_literal: 0,
    highest_opcode: 0
  )

  def new() do
    r_asm()
  end

  def opcode(op, dict) when r_asm(dict, :highest_opcode) >= op do
    dict
  end

  def opcode(op, dict) do
    r_asm(dict, highest_opcode: op)
  end

  def highest_opcode(r_asm(highest_opcode: op)) do
    op
  end

  def atom(atom, r_asm(atoms: atoms) = dict)
      when is_atom(atom) do
    case atoms do
      %{^atom => index} ->
        {index, dict}

      _ ->
        nextIndex = :maps.size(atoms) + 1
        {nextIndex, r_asm(dict, atoms: Map.put(atoms, atom, nextIndex))}
    end
  end

  def export(func, arity, label, dict0)
      when is_atom(func) and is_integer(arity) and
             is_integer(label) do
    {index, dict1} = atom(func, dict0)

    r_asm(dict1,
      exports: [
        {index, arity, label}
        | r_asm(dict1, :exports)
      ]
    )
  end

  def local(func, arity, label, dict0)
      when is_atom(func) and is_integer(arity) and
             is_integer(label) do
    {index, dict1} = atom(func, dict0)

    r_asm(dict1,
      locals: [
        {index, arity, label}
        | r_asm(dict1, :locals)
      ]
    )
  end

  def import(mod0, name0, arity, r_asm(imports: imp0, next_import: nextIndex) = d0)
      when is_atom(mod0) and is_atom(name0) and
             is_integer(arity) do
    {mod, d1} = atom(mod0, d0)
    {name, d2} = atom(name0, d1)
    mFA = {mod, name, arity}

    case :gb_trees.lookup(mFA, imp0) do
      {:value, index} ->
        {index, d2}

      :none ->
        imp = :gb_trees.insert(mFA, nextIndex, imp0)
        {nextIndex, r_asm(d2, imports: imp, next_import: nextIndex + 1)}
    end
  end

  def string(binString, dict) when is_binary(binString) do
    r_asm(strings: strings, string_offset: nextOffset) = dict

    case old_string(binString, strings) do
      :none ->
        newDict =
          r_asm(dict,
            strings: <<strings::binary, binString::binary>>,
            string_offset: nextOffset + byte_size(binString)
          )

        {nextOffset, newDict}

      offset when is_integer(offset) ->
        {nextOffset - offset, dict}
    end
  end

  def lambda(
        lbl,
        numFree,
        r_asm(
          wrappers: wrappers0,
          lambdas: {oldIndex, lambdas0}
        ) = dict
      ) do
    case wrappers0 do
      %{^lbl => index} ->
        {index, dict}

      %{} ->
        index = oldIndex
        wrappers = Map.put(wrappers0, lbl, index)
        lambdas = [{lbl, {index, lbl, numFree}} | lambdas0]

        {oldIndex,
         r_asm(dict,
           wrappers: wrappers,
           lambdas: {oldIndex + 1, lambdas}
         )}
    end
  end

  def literal(
        lit,
        r_asm(literals: tab0, next_literal: nextIndex) = dict
      ) do
    case tab0 do
      %{^lit => index} ->
        {index, dict}

      %{} ->
        tab = Map.put(tab0, lit, nextIndex)
        {nextIndex, r_asm(dict, literals: tab, next_literal: nextIndex + 1)}
    end
  end

  def line([], r_asm(num_lines: n) = dict) do
    {0, r_asm(dict, num_lines: n + 1)}
  end

  def line(
        [{:location, name, line} | _],
        r_asm(lines: lines, num_lines: n) = dict0
      ) do
    {fnameIndex, dict1} = fname(name, dict0)
    key = {fnameIndex, line}

    case lines do
      %{^key => index} ->
        {index, r_asm(dict1, num_lines: n + 1)}

      _ ->
        index = :maps.size(lines) + 1

        {index,
         r_asm(dict1,
           lines: Map.put(lines, key, index),
           num_lines: n + 1
         )}
    end
  end

  def line([_ | t], r_asm() = dict) do
    line(t, dict)
  end

  def fname(name, r_asm(fnames: fnames) = dict) do
    case fnames do
      %{^name => index} ->
        {index, dict}

      _ ->
        index = :maps.size(fnames)
        {index, r_asm(dict, fnames: Map.put(fnames, name, index))}
    end
  end

  def atom_table(r_asm(atoms: atoms), encoding) do
    numAtoms = :maps.size(atoms)
    sorted = :lists.keysort(2, :maps.to_list(atoms))

    {numAtoms,
     for {a, _} <- sorted do
       l = :erlang.atom_to_binary(a, encoding)
       [byte_size(l), l]
     end}
  end

  def local_table(r_asm(locals: locals)) do
    {length(locals), locals}
  end

  def export_table(r_asm(exports: exports)) do
    {length(exports), exports}
  end

  def import_table(r_asm(imports: imp, next_import: numImports)) do
    sorted = :lists.keysort(2, :gb_trees.to_list(imp))

    impTab =
      for {mFA, _} <- sorted do
        mFA
      end

    {numImports, impTab}
  end

  def string_table(r_asm(strings: strings, string_offset: size)) do
    {size, strings}
  end

  def lambda_table(
        r_asm(
          locals: loc0,
          lambdas: {numLambdas, lambdas0}
        )
      ) do
    lambdas1 = :sofs.relation(lambdas0)

    loc =
      :sofs.relation(
        for {f, a, lbl} <- loc0 do
          {lbl, {f, a}}
        end
      )

    lambdas2 = :sofs.relative_product1(lambdas1, loc)
    oldUniq = 0

    lambdas =
      for {{index, lbl, numFree}, {f, a}} <- :sofs.to_external(lambdas2) do
        <<f::size(32), a::size(32), lbl::size(32), index::size(32), numFree::size(32),
          oldUniq::size(32)>>
      end

    {numLambdas, lambdas}
  end

  def literal_table(r_asm(literals: tab, next_literal: numLiterals)) do
    l0 =
      :maps.fold(
        fn lit, num, acc ->
          [{num, my_term_to_binary(lit)} | acc]
        end,
        [],
        tab
      )

    l1 = :lists.sort(l0)

    l =
      for {_, term} <- l1 do
        [<<byte_size(term)::size(32)>>, term]
      end

    {numLiterals, l}
  end

  defp my_term_to_binary(term) do
    :erlang.term_to_binary(term, [{:minor_version, 2}])
  end

  def line_table(r_asm(fnames: fnames0, lines: lines0, num_lines: numLineInstrs)) do
    numFnames = :maps.size(fnames0)
    fnames1 = :lists.keysort(2, :maps.to_list(fnames0))

    fnames =
      for {name, _} <- fnames1 do
        name
      end

    numLines = :maps.size(lines0)
    lines1 = :lists.keysort(2, :maps.to_list(lines0))

    lines =
      for {l, _} <- lines1 do
        l
      end

    {numLineInstrs, numFnames, fnames, numLines, lines}
  end

  defp old_string(str, pool) do
    case :binary.match(pool, str) do
      :nomatch ->
        :none

      {start, _Length} ->
        byte_size(pool) - start
    end
  end
end
