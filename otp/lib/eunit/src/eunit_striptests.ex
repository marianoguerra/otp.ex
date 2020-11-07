defmodule :m_eunit_striptests do
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

    exports =
      :lists.foldl(
        fn
          {:attribute, _, :export, es}, s ->
            :sets.union(:sets.from_list(es), s)

          _F, s ->
            s
        end,
        :sets.new(),
        forms
      )

    f = fn form, acc ->
      form(form, acc, exports, testSuffix, generatorSuffix, exportSuffix)
    end

    :lists.reverse(:lists.foldl(f, [], forms))
  end

  defp form(
         {:function, _L, name, 0, _Cs} = form,
         acc,
         exports,
         testSuffix,
         generatorSuffix,
         exportSuffix
       ) do
    n = :erlang.atom_to_list(name)

    case not :sets.is_element(
           {name, 0},
           exports
         ) and
           (:lists.suffix(
              testSuffix,
              n
            ) or
              :lists.suffix(
                generatorSuffix,
                n
              ) or
              :lists.suffix(
                exportSuffix,
                n
              )) do
      true ->
        acc

      false ->
        [form | acc]
    end
  end

  defp form({:function, _L, :eunit_wrapper_, 1, _Cs}, acc, _, _, _, _) do
    acc
  end

  defp form(form, acc, _, _, _, _) do
    [form | acc]
  end
end
