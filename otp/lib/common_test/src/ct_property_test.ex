defmodule :m_ct_property_test do
  use Bitwise

  def init_per_suite(config) do
    case init_tool(config) do
      {:skip, _} = skip ->
        skip

      config1 ->
        path = property_tests_path('property_test', config1)

        case compile_tests(path, config1) do
          :error ->
            {:fail, 'Property test compilation failed in ' ++ path}

          {:skip, reason} ->
            {:skip, reason}

          :up_to_date ->
            add_code_pathz(path)
            [{:property_dir, path} | config1]
        end
    end
  end

  def init_tool(config) do
    toolsToCheck = :proplists.get_value(:prop_tools, config, [:eqc, :proper, :triq])

    case which_module_exists(toolsToCheck) do
      {:ok, toolModule} ->
        case :code.where_is_file(:lists.concat([toolModule, '.beam'])) do
          :non_existing ->
            :ct.log('Found ~p, but ~tp~n is not found', [
              toolModule,
              :lists.concat([toolModule, '.beam'])
            ])

            {:skip, 'Strange Property testing tool installation'}

          toolPath ->
            :ct.pal('Found property tester ~p~nat ~tp', [toolModule, toolPath])
            [{:property_test_tool, toolModule} | config]
        end

      :not_found ->
        :ct.pal('No property tester found', [])
        {:skip, 'No property testing tool found'}
    end
  end

  def quickcheck(property, config) do
    tool = :proplists.get_value(:property_test_tool, config)
    f = function_name(:quickcheck, tool)
    mk_ct_return(apply(tool, f, [property]), tool)
  end

  def present_result(module, cmds, triple, config) do
    present_result(module, cmds, triple, config, [])
  end

  def present_result(module, cmds, {h, sf, result}, config, options0) do
    defSpec =
      cond do
        is_tuple(cmds) ->
          [{'Distribution sequential/parallel', &sequential_parallel/1}]

        is_list(cmds) ->
          []
      end ++
        [
          {'Function calls', &cmnd_names/1},
          {'Length of command sequences', &print_frequency_ranges/0, &num_calls/1}
        ]

    options =
      add_default_options(
        options0,
        [{:print_fun, &:ct.log/2}, {:spec, defSpec}]
      )

    do_present_result(module, cmds, h, sf, result, config, options)
  end

  def title(str, fun) do
    title(str, fun, &:io.format/2)
  end

  def title(str, fun, printFun) do
    fn l ->
      printFun.('~n~s~n~n~s~n', [str, fun.(l)])
    end
  end

  def print_frequency() do
    fn l ->
      for {v, _Num, pcnt} <-
            with_percentage(
              get_frequencies_no_range(l),
              length(l)
            ) do
        :io_lib.format('~5.1f% ~p~n', [pcnt, v])
      end
    end
  end

  def print_frequency_ranges() do
    print_frequency_ranges([{:ngroups, 10}])
  end

  defp print_frequency_ranges(options0) do
    fn
      [] ->
        :io_lib.format(:"Empty list!~n", [])

      l ->
        try do
          options = set_default_print_freq_range_opts(options0, l)
          do_print_frequency_ranges(l, options)
        catch
          c, e ->
            :ct.pal(
              '~p:~p ~p:~p~n~p~n~p',
              [:ct_property_test, 150, c, e, __STACKTRACE__, l]
            )
        end
    end
  end

  defp mk_ct_return(true, _Tool) do
    true
  end

  defp mk_ct_return(other, tool) do
    try do
      :lists.last(hd(tool.counterexample()))
    catch
      _, _ ->
        {:fail, other}
    else
      {:set, {:var, _}, {:call, m, f, args}} ->
        {:fail, :io_lib.format('~p:~tp/~p returned bad result', [m, f, length(args)])}
    end
  end

  defp which_module_exists([module | modules]) do
    case module_exists(module) do
      true ->
        {:ok, module}

      false ->
        which_module_exists(modules)
    end
  end

  defp which_module_exists(_) do
    :not_found
  end

  defp module_exists(module) do
    is_list(
      try do
        module.module_info()
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end
    )
  end

  defp property_tests_path(dir, config) do
    dataDir = :proplists.get_value(:data_dir, config)
    :filename.join(:lists.droplast(:filename.split(dataDir)) ++ [dir])
  end

  defp add_code_pathz(dir) do
    case :lists.member(dir, :code.get_path()) do
      true ->
        :ok

      false ->
        true = :code.add_pathz(dir)
        :ok
    end
  end

  defp compile_tests(path, config) do
    toolModule =
      :proplists.get_value(
        :property_test_tool,
        config
      )

    macroDefs = macro_def(toolModule)
    {:ok, cwd} = :file.get_cwd()

    case :file.set_cwd(path) do
      :ok ->
        case :file.list_dir('.') do
          {:ok, []} ->
            :ct.pal('No files found in ~tp', [path])
            :ok = :file.set_cwd(cwd)
            {:skip, 'No files found'}

          {:ok, fileNames} ->
            beamFiles =
              for f <- fileNames,
                  :filename.extension(f) == '.beam' do
                f
              end

            erlFiles =
              for f <- fileNames,
                  :filename.extension(f) == '.erl' do
                f
              end

            _ =
              for f <- beamFiles do
                :file.delete(f)
              end

            :ct.pal('Compiling in ~tp~n  Deleted:   ~p~n  ErlFiles:  ~tp~n  MacroDefs: ~p', [
              path,
              beamFiles,
              erlFiles,
              macroDefs
            ])

            result = :make.all([:load | macroDefs])
            :ok = :file.set_cwd(cwd)
            result
        end

      {:error, error} ->
        :ct.pal('file:set_cwd(~tp) returned ~p.~nCwd = ~tp', [path, {:error, error}, cwd])
        :error
    end
  end

  defp macro_def(:eqc) do
    [{:d, :EQC}]
  end

  defp macro_def(:proper) do
    [{:d, :PROPER}]
  end

  defp macro_def(:triq) do
    [{:d, :TRIQ}]
  end

  defp function_name(:quickcheck, :triq) do
    :check
  end

  defp function_name(f, _) do
    f
  end

  defp do_present_result(_Module, cmds, _H, _Sf, :ok, config, options) do
    [printFun, spec] =
      for k <- [:print_fun, :spec] do
        :proplists.get_value(k, options)
      end

    tool = :proplists.get_value(:property_test_tool, config)
    aGGREGATE = function_name(:aggregate, tool)

    :lists.foldr(
      fn
        {title, freqFun, collecFun}, result ->
          apply(tool, aGGREGATE, [title(title, freqFun.(), printFun), collecFun.(cmds), result])

        {title, collecFun}, result ->
          apply(tool, aGGREGATE, [
            title(title, print_frequency(), printFun),
            collecFun.(cmds),
            result
          ])
      end,
      true,
      spec
    )
  end

  defp do_present_result(module, cmds, h, sf, result, _Config, options) do
    [printFun] =
      for k <- [:print_fun] do
        :proplists.get_value(k, options)
      end

    printFun.('Module = ~p,~nCommands = ~p,~nHistory = ~p,~nFinalDynState = ~p,~nResult = ~p', [
      module,
      cmds,
      h,
      sf,
      result
    ])

    result == :ok
  end

  def cmnd_names(cs) do
    traverse_commands(&cmnd_name/1, cs)
  end

  defp cmnd_name(l) do
    for {:set, _Var, {:call, _Mod, f, _As}} <- l do
      f
    end
  end

  def num_calls(cs) do
    traverse_commands(&num_call/1, cs)
  end

  defp num_call(l) do
    [length(l)]
  end

  def sequential_parallel(cs) do
    traverse_commands(
      fn l ->
        dup_module(l, :sequential)
      end,
      fn l ->
        for l1 <- l do
          dup_module(l1, mkmod('parallel', num(l1, l)))
        end
      end,
      cs
    )
  end

  defp dup_module(l, modName) do
    :lists.duplicate(length(l), modName)
  end

  defp mkmod(pfxStr, n) do
    :erlang.list_to_atom(pfxStr ++ '_' ++ :erlang.integer_to_list(n))
  end

  defp traverse_commands(fun, l) when is_list(l) do
    fun.(l)
  end

  defp traverse_commands(fun, {seq, parLs}) do
    fun.(:lists.append([seq | parLs]))
  end

  defp traverse_commands(fseq, _Fpar, l) when is_list(l) do
    fseq.(l)
  end

  defp traverse_commands(fseq, fpar, {seq, parLs}) do
    :lists.append([fseq.(seq) | fpar.(parLs)])
  end

  defp set_default_print_freq_range_opts(opts0, l) do
    add_default_options(
      opts0,
      [{:ngroups, 10}, {:min, 0}, {:max, max_in_list(l)}]
    )
  end

  defp add_default_options(opts0, defaultOpts) do
    for {key, defVal} <- defaultOpts do
      set_def_opt(key, defVal, opts0)
    end
  end

  defp set_def_opt(key, defaultValue, opts) do
    {key, :proplists.get_value(key, opts, defaultValue)}
  end

  defp max_in_list(l) do
    case :lists.last(l) do
      max when is_integer(max) ->
        max

      {max, _} ->
        max
    end
  end

  defp do_print_frequency_ranges(l0, options) do
    [n, min, max] =
      for k <- [:ngroups, :min, :max] do
        :proplists.get_value(k, options)
      end

    l =
      cond do
        n > max ->
          l0 ++ [{n, 0}]

        n <= max ->
          l0
      end

    try do
      interval = round((max - min) / n)
      intervalLowerLimits = :lists.seq(min, max, interval)

      ranges =
        for i <- intervalLowerLimits do
          {i, i + interval - 1}
        end

      acc0 =
        for rng <- ranges do
          {rng, 0}
        end

      fs0 = get_frequencies(l, acc0)

      sumVal =
        :lists.sum(
          for {_, v} <- fs0 do
            v
          end
        )

      fs = with_percentage(fs0, sumVal)

      distInfo = [
        {'min', :lists.min(l)},
        {'mean', mean(l)},
        {'median', median(l)},
        {'max', :lists.max(l)}
      ]

      npos_value = num_digits(sumVal)
      npos_range = num_digits(max)

      [
        :io_lib.format('Range~*s: ~s~n', [2 * npos_range - 2, '', 'Number in range']),
        :io_lib.format(
          '~*c:~*c~n',
          [2 * npos_range + 3, ?-, max(16, npos_value + 10), ?-]
        ),
        for {interv = {rlow, rhigh}, val, percent} <- fs do
          :io_lib.format(
            '~*w - ~*w:  ~*w  ~5.1f% ~s~n',
            [
              npos_range,
              rlow,
              npos_range,
              rhigh,
              npos_value,
              val,
              percent,
              cond_prt_vals(distInfo, interv)
            ]
          )
        end,
        :io_lib.format(
          :"~*c    ~*c~n",
          [2 * npos_range, 32, npos_value + 3, ?-]
        ),
        :io_lib.format(
          :"~*c      ~*w~n",
          [2 * npos_range, 32, npos_value, sumVal]
        )
      ]
    catch
      c, e ->
        :ct.pal(:"*** Failed printing (~p:~p) for~n~p~n", [c, e, l])
    end
  end

  defp cond_prt_vals(lVs, currentInterval) do
    for {label, value} <- lVs do
      prt_val(label, value, currentInterval)
    end
  end

  defp prt_val(label, value, currentInterval) do
    case in_interval(value, currentInterval) do
      true ->
        :io_lib.format(
          ' <-- ~s=' ++
            cond do
              is_float(value) ->
                '~.1f'

              true ->
                '~p'
            end,
          [label, value]
        )

      false ->
        ''
    end
  end

  defp get_frequencies([{i, num} | t], [{{lower, upper}, cnt} | acc])
       when lower <= i and i <= upper do
    get_frequencies(t, [{{lower, upper}, cnt + num} | acc])
  end

  defp get_frequencies(
         l = [{i, _Num} | _],
         [ah = {{_Lower, upper}, _Cnt} | acc]
       )
       when i > upper do
    [ah | get_frequencies(l, acc)]
  end

  defp get_frequencies([i | t], acc) when is_integer(i) do
    get_frequencies([{i, 1} | t], acc)
  end

  defp get_frequencies([], acc) do
    acc
  end

  defp get_frequencies_no_range([]) do
    :io_lib.format('No values~n', [])
  end

  defp get_frequencies_no_range(l) do
    [h | t] = :lists.sort(l)
    get_frequencies_no_range(t, h, 1, [])
  end

  defp get_frequencies_no_range([h | t], h, n, acc) do
    get_frequencies_no_range(t, h, n + 1, acc)
  end

  defp get_frequencies_no_range([h1 | t], h, n, acc) do
    get_frequencies_no_range(t, h1, 1, [{h, n} | acc])
  end

  defp get_frequencies_no_range([], h, n, acc) do
    :lists.reverse(:lists.keysort(2, [{h, n} | acc]))
  end

  defp with_percentage(fs, sum) do
    for {rng, val} <- fs do
      {rng, val, 100 * val / sum}
    end
  end

  defp num_digits(i) do
    1 + trunc(:math.log(i) / :math.log(10))
  end

  defp num(elem, list) do
    length(
      :lists.takewhile(
        fn e ->
          e != elem
        end,
        list
      )
    ) + 1
  end

  defp is_odd(i) do
    rem(i, 2) == 1
  end

  defp in_interval(value, {rlow, rhigh}) do
    try do
      rlow <= round(value) and round(value) <= rhigh
    catch
      _, _ ->
        false
    end
  end

  defp mean(l = [x | _]) when is_number(x) do
    :lists.sum(l) / length(l)
  end

  defp mean(l = [{_Value, _Weight} | _]) do
    sumOfWeights =
      :lists.sum(
        for {_, w} <- l do
          w
        end
      )

    weightedSum =
      :lists.sum(
        for {v, w} <- l do
          w * v
        end
      )

    weightedSum / sumOfWeights
  end

  defp mean(_) do
    :undefined
  end

  defp median(l = [x | _]) when is_number(x) do
    len = length(l)

    case is_odd(len) do
      true ->
        hd(:lists.nthtail(div(len, 2), l))

      false ->
        [[m1, m2] | _] = :lists.nthtail(div(len, 2) - 1, l)
        (m1 + m2) / 2
    end
  end

  defp median(l = [{_Value, _Weight} | _]) do
    median(
      :lists.append(
        for {v, w} <- l do
          :lists.duplicate(w, v)
        end
      )
    )
  end

  defp median(_) do
    :undefined
  end
end
