defmodule :m_eunit_autoexport do
  use Bitwise
  require Record

  Record.defrecord(:r_test, :test,
    f: :undefined,
    desc: :undefined,
    timeout: :undefined,
    location: :undefined,
    line: 0
  )

  Record.defrecord(:r_group, :group,
    desc: :undefined,
    order: :undefined,
    timeout: :undefined,
    context: :undefined,
    spawn: :undefined,
    tests: :undefined
  )

  Record.defrecord(:r_context, :context, setup: :undefined, cleanup: :undefined, process: :local)

  def parse_transform(forms, options) do
    testSuffix = :proplists.get_value(:eunit_test_suffix, options, '_test')
    generatorSuffix = :proplists.get_value(:eunit_generator_suffix, options, '_test_')
    exportSuffix = :proplists.get_value(:eunit_export_suffix, options, '_exported_')

    f = fn form, set ->
      form(form, set, testSuffix, generatorSuffix, exportSuffix)
    end

    exports = :sets.to_list(:lists.foldl(f, :sets.new(), forms))
    rewrite(forms, exports)
  end

  defp form({:function, _L, name, 0, _Cs}, s, testSuffix, generatorSuffix, exportSuffix) do
    n = :erlang.atom_to_list(name)

    case :lists.suffix(testSuffix, n) do
      true ->
        :sets.add_element({name, 0}, s)

      false ->
        case :lists.suffix(generatorSuffix, n) do
          true ->
            :sets.add_element({name, 0}, s)

          false ->
            case :lists.suffix(exportSuffix, n) do
              true ->
                :sets.add_element({name, 0}, s)

              false ->
                s
            end
        end
    end
  end

  defp form({:function, _L, :eunit_wrapper_, 1, _Cs}, s, _, _, _) do
    :sets.add_element({:eunit_wrapper_, 1}, s)
  end

  defp form(_, s, _, _, _) do
    s
  end

  defp rewrite(
         [
           {:attribute, _, :module, {name, _Ps}} = m
           | fs
         ],
         exports
       ) do
    module_decl(name, m, fs, exports)
  end

  defp rewrite(
         [{:attribute, _, :module, name} = m | fs],
         exports
       ) do
    module_decl(name, m, fs, exports)
  end

  defp rewrite([f | fs], exports) do
    [f | rewrite(fs, exports)]
  end

  defp rewrite([], _Exports) do
    []
  end

  defp rewrite([{:function, _, :test, 0, _} = f | fs], as, module, _Test) do
    rewrite(fs, [f | as], module, false)
  end

  defp rewrite([f | fs], as, module, test) do
    rewrite(fs, [f | as], module, test)
  end

  defp rewrite([], as, module, test) do
    l = :erl_anno.new(0)

    {cond do
       test ->
         [
           {:function, l, :test, 0,
            [
              {:clause, l, [], [],
               [
                 {:call, l, {:remote, l, {:atom, l, :eunit}, {:atom, l, :test}},
                  [{:atom, l, module}]}
               ]}
            ]}
           | as
         ]

       true ->
         as
     end, test}
  end

  defp module_decl(name, m, fs, exports) do
    module = name
    {fs1, test} = rewrite(fs, [], module, true)

    es =
      cond do
        test ->
          [{:test, 0} | exports]

        true ->
          exports
      end

    [
      [m, {:attribute, :erl_anno.new(0), :export, es}]
      | :lists.reverse(fs1)
    ]
  end
end
