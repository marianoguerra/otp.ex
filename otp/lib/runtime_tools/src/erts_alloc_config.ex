defmodule :m_erts_alloc_config do
  use Bitwise
  require Record

  Record.defrecord(:r_state, :state,
    have_scenario: false,
    alloc: :undefined
  )

  Record.defrecord(:r_alloc, :alloc,
    name: :undefined,
    enabled: :undefined,
    need_config_change: :undefined,
    alloc_util: :undefined,
    instances: :undefined,
    strategy: :undefined,
    acul: :undefined,
    low_mbc_blocks_size: :undefined,
    high_mbc_blocks_size: :undefined,
    sbct: :undefined,
    segments: :undefined
  )

  Record.defrecord(:r_conf, :conf,
    segments: :undefined,
    format_to: :undefined
  )

  Record.defrecord(:r_segment, :segment,
    size: :undefined,
    number: :undefined
  )

  def save_scenario() do
    req(:save_scenario)
  end

  def make_config() do
    make_config(:erlang.group_leader())
  end

  def make_config(fileName) when is_list(fileName) do
    case :file.open(fileName, [:write]) do
      {:ok, iODev} ->
        res = req({:make_config, iODev})
        :ok = :file.close(iODev)
        res

      error ->
        error
    end
  end

  def make_config(iODev) do
    req({:make_config, iODev})
  end

  def stop() do
    req(:stop)
  end

  def state() do
    req(:state)
  end

  defp req(req) do
    ref = make_ref()
    reqMsg = {:request, self(), ref, req}
    req(reqMsg, ref, true)
  end

  defp req(reqMsg, ref, tryStart) do
    req(reqMsg, ref, tryStart, :erlang.monitor(:process, :__erts_alloc_config__))
  end

  defp req(reqMsg, ref, tryStart, mon) do
    try do
      send(:__erts_alloc_config__, reqMsg)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    receive do
      {:response, ^ref, res} ->
        :erlang.demonitor(mon, [:flush])
        res

      {:DOWN, ^mon, _, _, :noproc} ->
        case tryStart do
          true ->
            start_server(ref, reqMsg)

          false ->
            {:error, :server_died}
        end

      {:DOWN, ^mon, _, _, reason} ->
        {:error, reason}
    end
  end

  defp start_server(ref, reqMsg) do
    starter = self()

    pid =
      spawn(fn ->
        :erlang.register(:__erts_alloc_config__, self())
        send(starter, {ref, self(), :started})
        server_loop(make_state())
      end)

    mon = :erlang.monitor(:process, pid)

    receive do
      {^ref, ^pid, :started} ->
        req(reqMsg, ref, false, mon)

      {:DOWN, ^mon, _, _, _} ->
        req(reqMsg, ref, false)
    end
  end

  defp server_loop(state) do
    newState =
      receive do
        {:request, from, ref, :save_scenario} ->
          alloc = save_scenario(r_state(state, :alloc))
          send(from, {:response, ref, :ok})
          r_state(state, alloc: alloc, have_scenario: true)

        {:request, from, ref, {:make_config, iODev}} ->
          case r_state(state, :have_scenario) do
            true ->
              conf = r_conf(segments: 150, format_to: iODev)
              res = mk_config(conf, r_state(state, :alloc))
              send(from, {:response, ref, res})
              :ok

            _ ->
              send(from, {:response, ref, :no_scenario_saved})
              :ok
          end

          state

        {:request, from, ref, :stop} ->
          send(from, {:response, ref, :ok})
          exit(:normal)

        {:request, from, ref, :state} ->
          send(from, {:response, ref, state})
          state

        {:request, from, ref, req} ->
          send(from, {:response, ref, {:unknown_request, req}})
          state

        _ ->
          state
      end

    server_loop(newState)
  end

  defp carrier_migration_support(:aoff) do
    true
  end

  defp carrier_migration_support(:aoffcbf) do
    true
  end

  defp carrier_migration_support(:aoffcaobf) do
    true
  end

  defp carrier_migration_support(_) do
    false
  end

  defp allocator_instances(:ll_alloc, strategy) do
    case carrier_migration_support(strategy) do
      true ->
        :erlang.system_info(:schedulers)

      false ->
        1
    end
  end

  defp allocator_instances(_A, :undefined) do
    1
  end

  defp allocator_instances(_A, _Strategy) do
    :erlang.system_info(:schedulers)
  end

  defp strategy(:temp_alloc, _AI) do
    :af
  end

  defp strategy(a, aI) do
    try do
      {^a, optList} = :lists.keyfind(a, 1, aI)
      {:as, s} = :lists.keyfind(:as, 1, optList)
      s
    catch
      _, _ ->
        :undefined
    end
  end

  defp strategy_str(:af) do
    'A fit'
  end

  defp strategy_str(:gf) do
    'Good fit'
  end

  defp strategy_str(:bf) do
    'Best fit'
  end

  defp strategy_str(:aobf) do
    'Address order best fit'
  end

  defp strategy_str(:aoff) do
    'Address order first fit'
  end

  defp strategy_str(:aoffcbf) do
    'Address order first fit carrier best fit'
  end

  defp strategy_str(:aoffcaobf) do
    'Address order first fit carrier adress order best fit'
  end

  defp strategy_str(:ageffcaoff) do
    'Age order first fit carrier address order first fit'
  end

  defp strategy_str(:ageffcbf) do
    'Age order first fit carrier best fit'
  end

  defp strategy_str(:ageffcaobf) do
    'Age order first fit carrier adress order best fit'
  end

  defp default_acul(a, s) do
    case carrier_migration_support(s) do
      false ->
        0

      true ->
        case a do
          :ll_alloc ->
            85

          :eheap_alloc ->
            45

          _ ->
            60
        end
    end
  end

  defp make_state() do
    {_, _, _, aI} = :erlang.system_info(:allocator)

    r_state(
      alloc:
        :lists.map(
          fn a ->
            s = strategy(a, aI)

            r_alloc(
              name: a,
              strategy: s,
              acul: default_acul(a, s),
              instances: allocator_instances(a, s)
            )
          end,
          [
            :binary_alloc,
            :ets_alloc,
            :eheap_alloc,
            :fix_alloc,
            :ll_alloc,
            :mseg_alloc,
            :sl_alloc,
            :std_alloc,
            :sys_alloc,
            :temp_alloc,
            :driver_alloc
          ]
        )
    )
  end

  defp ai_value(key1, key2, aI) do
    case :lists.keysearch(key1, 1, aI) do
      {:value, {^key1, value1}} ->
        case :lists.keysearch(key2, 1, value1) do
          {:value, result} ->
            result

          _ ->
            :undefined
        end

      _ ->
        :undefined
    end
  end

  defp chk_mbcs_blocks_size(
         r_alloc(
           low_mbc_blocks_size: :undefined,
           high_mbc_blocks_size: :undefined
         ) = alc,
         min,
         max
       ) do
    r_alloc(alc, low_mbc_blocks_size: min, high_mbc_blocks_size: max, enabled: true)
  end

  defp chk_mbcs_blocks_size(
         r_alloc(
           low_mbc_blocks_size: lowBS,
           high_mbc_blocks_size: highBS
         ) = alc,
         min,
         max
       ) do
    true = is_integer(lowBS)
    true = is_integer(highBS)

    alc1 =
      case min < lowBS do
        true ->
          r_alloc(alc, low_mbc_blocks_size: min)

        false ->
          alc
      end

    case max > highBS do
      true ->
        r_alloc(alc1, high_mbc_blocks_size: max)

      false ->
        alc1
    end
  end

  defp set_alloc_util(r_alloc(alloc_util: aU) = alc, aU) do
    alc
  end

  defp set_alloc_util(alc, val) do
    r_alloc(alc, alloc_util: val)
  end

  defp chk_sbct(r_alloc(sbct: :undefined) = alc, aI) do
    case ai_value(:options, :sbct, aI) do
      {:sbct, bytes} when is_integer(bytes) ->
        r_alloc(alc, sbct: b2kb(bytes))

      _ ->
        alc
    end
  end

  defp chk_sbct(alc, _AI) do
    alc
  end

  defp save_scenario(alcList) do
    oP = :erlang.process_flag(:priority, :high)
    res = do_save_scenario(alcList)
    :erlang.process_flag(:priority, oP)
    res
  end

  defp save_ai2(r_alloc(name: name) = alc0, aI) do
    alc1 = chk_sbct(alc0, aI)

    {alc, isAUtil} =
      case ai_value(:mbcs, :blocks, aI) do
        {:blocks, bs} ->
          case ai_value(name, :size, bs) do
            {:size, minBS, _, maxBS} ->
              {chk_mbcs_blocks_size(alc1, minBS, maxBS), true}

            _ ->
              {alc1, false}
          end

        _ ->
          {alc1, false}
      end

    set_alloc_util(alc, isAUtil)
  end

  defp save_ai(alc, [{:instance, 0, aI}]) do
    save_ai2(alc, aI)
  end

  defp save_ai(
         alc,
         [[{:instance, _, _}, {:instance, _, _}] | _]
       ) do
    r_alloc(alc, enabled: true, need_config_change: true)
  end

  defp save_ai(alc, aI) do
    save_ai2(alc, aI)
  end

  defp do_save_scenario(alcList) do
    :lists.map(
      fn
        r_alloc(enabled: false) = alc ->
          alc

        r_alloc(name: name) = alc ->
          case :erlang.system_info({:allocator, name}) do
            :undefined ->
              exit({:bad_allocator_name, name})

            false ->
              r_alloc(alc, enabled: false)

            aI when is_list(aI) ->
              save_ai(alc, aI)
          end
      end,
      alcList
    )
  end

  defp conf_size(bytes) when is_integer(bytes) and bytes < 0 do
    exit({:bad_value, bytes})
  end

  defp conf_size(bytes)
       when is_integer(bytes) and
              bytes < 1 * 1_048_576 do
    (div(div(bytes - 1, 1024) + 1 - 1, 256) + 1) * 256
  end

  defp conf_size(bytes)
       when is_integer(bytes) and
              bytes < 10 * 1_048_576 do
    (div(div(bytes - 1, 1024) + 1 - 1, div(1 * 1_048_576 - 1, 1024) + 1) + 1) *
      (div(1 * 1_048_576 - 1, 1024) + 1)
  end

  defp conf_size(bytes)
       when is_integer(bytes) and
              bytes < 100 * 1_048_576 do
    (div(div(bytes - 1, 1024) + 1 - 1, div(2 * 1_048_576 - 1, 1024) + 1) + 1) *
      (div(2 * 1_048_576 - 1, 1024) + 1)
  end

  defp conf_size(bytes)
       when is_integer(bytes) and
              bytes < 256 * 1_048_576 do
    (div(div(bytes - 1, 1024) + 1 - 1, div(5 * 1_048_576 - 1, 1024) + 1) + 1) *
      (div(5 * 1_048_576 - 1, 1024) + 1)
  end

  defp conf_size(bytes) when is_integer(bytes) do
    (div(div(bytes - 1, 1024) + 1 - 1, div(10 * 1_048_576 - 1, 1024) + 1) + 1) *
      (div(10 * 1_048_576 - 1, 1024) + 1)
  end

  defp sbct(r_conf(format_to: fTO), r_alloc(name: a, sbct: sBCT)) do
    fc(fTO, 'Sbc threshold size of ~p kilobytes.', [sBCT])
    format(fTO, ' +M~csbct ~p~n', [alloc_char(a), sBCT])
  end

  defp default_mmbcs(:temp_alloc = a, _Insts) do
    {:value, {^a, mMBCS_Default}} =
      :lists.keysearch(a, 1, [
        {:binary_alloc, 131_072},
        {:std_alloc, 131_072},
        {:ets_alloc, 131_072},
        {:fix_alloc, 131_072},
        {:eheap_alloc, 524_288},
        {:ll_alloc, 131_072},
        {:sl_alloc, 131_072},
        {:temp_alloc, 131_072},
        {:driver_alloc, 131_072}
      ])

    mMBCS_Default
  end

  defp default_mmbcs(a, insts) do
    {:value, {^a, mMBCS_Default}} =
      :lists.keysearch(a, 1, [
        {:binary_alloc, 131_072},
        {:std_alloc, 131_072},
        {:ets_alloc, 131_072},
        {:fix_alloc, 131_072},
        {:eheap_alloc, 524_288},
        {:ll_alloc, 131_072},
        {:sl_alloc, 131_072},
        {:temp_alloc, 131_072},
        {:driver_alloc, 131_072}
      ])

    i =
      case insts > 4 do
        true ->
          4

        _ ->
          insts
      end

    (div(div(mMBCS_Default, i) - 1, div(1 * 1024 - 1, 1024) + 1) + 1) *
      (div(1 * 1024 - 1, 1024) + 1)
  end

  defp mmbcs(
         r_conf(format_to: fTO),
         r_alloc(name: a, instances: insts, low_mbc_blocks_size: blocksSize)
       ) do
    bS =
      case a do
        :temp_alloc ->
          blocksSize

        _ ->
          div(blocksSize, insts)
      end

    defMMBCS = default_mmbcs(a, insts)

    case {insts, bS > defMMBCS} do
      {1, true} ->
        mMBCS = conf_size(bS)
        fc(fTO, 'Main mbc size of ~p kilobytes.', [mMBCS])
        format(fTO, ' +M~cmmbcs ~p~n', [alloc_char(a), mMBCS])

      _ ->
        mMBCS = div(defMMBCS - 1, 1024) + 1
        fc(fTO, 'Main mbc size of ~p kilobytes.', [mMBCS])
        format(fTO, ' +M~cmmbcs ~p~n', [alloc_char(a), mMBCS])
        :ok
    end
  end

  defp smbcs_lmbcs(
         r_conf(format_to: fTO),
         r_alloc(name: a, segments: segments)
       ) do
    mBCS = r_segment(segments, :size)
    aC = alloc_char(a)
    fc(fTO, 'Mseg mbc size of ~p kilobytes.', [mBCS])
    format(fTO, ' +M~csmbcs ~p +M~clmbcs ~p~n', [aC, mBCS, aC, mBCS])
    :ok
  end

  defp alloc_char(:binary_alloc) do
    ?B
  end

  defp alloc_char(:std_alloc) do
    ?D
  end

  defp alloc_char(:ets_alloc) do
    ?E
  end

  defp alloc_char(:fix_alloc) do
    ?F
  end

  defp alloc_char(:eheap_alloc) do
    ?H
  end

  defp alloc_char(:ll_alloc) do
    ?L
  end

  defp alloc_char(:mseg_alloc) do
    ?M
  end

  defp alloc_char(:driver_alloc) do
    ?R
  end

  defp alloc_char(:sl_alloc) do
    ?S
  end

  defp alloc_char(:temp_alloc) do
    ?T
  end

  defp alloc_char(:sys_alloc) do
    ?Y
  end

  defp alloc_char(alloc) do
    exit({:bad_allocator, alloc})
  end

  defp conf_alloc(
         r_conf(format_to: fTO),
         r_alloc(name: a, enabled: false)
       ) do
    fcl(fTO, a)

    fcp(
      fTO,
      'WARNING: ~p has been disabled. Consider enabling ~p by passing the "+M~ce true" command line argument and rerun erts_alloc_config.',
      [a, a, alloc_char(a)]
    )
  end

  defp conf_alloc(
         r_conf(format_to: fTO),
         r_alloc(name: a, need_config_change: true)
       ) do
    fcl(fTO, a)

    fcp(
      fTO,
      'WARNING: ~p has been configured in a way that prevents erts_alloc_config from creating a configuration. The configuration will be automatically adjusted to fit erts_alloc_config if you use the "+Mea config" command line argument while running erts_alloc_config.',
      [a]
    )
  end

  defp conf_alloc(
         r_conf(format_to: fTO) = conf,
         r_alloc(name: a, alloc_util: true) = alc
       ) do
    fcl(fTO, a)
    chk_xnote(conf, alc)
    au_conf_alloc(conf, alc)
    format(fTO, '#~n', [])
  end

  defp conf_alloc(r_conf(format_to: fTO) = conf, r_alloc(name: a) = alc) do
    fcl(fTO, a)
    chk_xnote(conf, alc)
  end

  defp chk_xnote(r_conf(format_to: fTO), r_alloc(name: :sys_alloc)) do
    fcp(fTO, 'Cannot be configured. Default malloc implementation used.')
  end

  defp chk_xnote(r_conf(format_to: fTO), r_alloc(name: :mseg_alloc)) do
    fcp(fTO, 'Default configuration used.')
  end

  defp chk_xnote(r_conf(format_to: fTO), r_alloc(name: :ll_alloc)) do
    fcp(
      fTO,
      'Note, blocks allocated with ll_alloc are very seldom deallocated. Placing blocks in mseg carriers is therefore very likely only a waste of resources.'
    )
  end

  defp chk_xnote(r_conf(), r_alloc()) do
    :ok
  end

  defp au_conf_alloc(
         r_conf(format_to: fTO) = conf,
         r_alloc(
           name: a,
           alloc_util: true,
           instances: insts,
           acul: acul,
           strategy: strategy,
           low_mbc_blocks_size: low,
           high_mbc_blocks_size: high
         ) = alc
       ) do
    fcp(fTO, 'Usage of mbcs: ~p - ~p kilobytes', [div(low - 1, 1024) + 1, div(high - 1, 1024) + 1])

    case insts do
      1 ->
        fc(fTO, 'One instance used.')
        format(fTO, ' +M~ct false~n', [alloc_char(a)])

      _ ->
        fc(fTO, '~p + 1 instances used.', [insts])
        format(fTO, ' +M~ct true~n', [alloc_char(a)])

        case strategy do
          :undefined ->
            :ok

          _ ->
            fc(fTO, 'Allocation strategy: ~s.', [strategy_str(strategy)])
            format(fTO, ' +M~cas ~s~n', [alloc_char(a), :erlang.atom_to_list(strategy)])
        end

        case carrier_migration_support(strategy) do
          false ->
            :ok

          true ->
            fc(fTO, 'Abandon carrier utilization limit of ~p%.', [acul])
            format(fTO, ' +M~cacul ~p~n', [alloc_char(a), acul])
        end
    end

    mmbcs(conf, alc)
    smbcs_lmbcs(conf, alc)
    sbct(conf, alc)
  end

  defp calc_seg_size(growth, segs) do
    conf_size(div(round(growth * 1.25 * 2), segs))
  end

  defp calc_growth_segments(conf, alcList0) do
    calcSmall = fn
      r_alloc(name: :ll_alloc, instances: 1) = alc, acc ->
        {r_alloc(alc, segments: r_segment(size: conf_size(0), number: 0)), acc}

      r_alloc(
        alloc_util: true,
        instances: insts,
        low_mbc_blocks_size: lowMBC,
        high_mbc_blocks_size: high
      ) = alc,
      {sL, aL} ->
        low =
          case insts do
            1 ->
              lowMBC

            _ ->
              0
          end

        growth = high - low

        case growth >= 20 * 1_048_576 do
          true ->
            {alc, {sL, aL + 1}}

          false ->
            segs = 5
            segSize = calc_seg_size(growth, segs)
            {r_alloc(alc, segments: r_segment(size: segSize, number: segs)), {sL - segs, aL}}
        end

      alc, acc ->
        {alc, acc}
    end

    {alcList1, {segsLeft, allocsLeft}} =
      :lists.mapfoldl(
        calcSmall,
        {r_conf(conf, :segments), 0},
        alcList0
      )

    case allocsLeft do
      0 ->
        alcList1

      _ ->
        segsPerAlloc =
          case div(segsLeft, allocsLeft) + 1 do
            sPA when sPA < 5 ->
              5

            sPA ->
              sPA
          end

        calcLarge = fn
          r_alloc(
            alloc_util: true,
            segments: :undefined,
            instances: insts,
            low_mbc_blocks_size: lowMBC,
            high_mbc_blocks_size: high
          ) = alc ->
            low =
              case insts do
                1 ->
                  lowMBC

                _ ->
                  0
              end

            growth = high - low
            segSize = calc_seg_size(growth, segsPerAlloc)

            r_alloc(alc,
              segments:
                r_segment(
                  size: segSize,
                  number: segsPerAlloc
                )
            )

          alc ->
            alc
        end

        :lists.map(calcLarge, alcList1)
    end
  end

  defp mk_config(r_conf(format_to: fTO) = conf, alcList) do
    format_header(fTO)

    res =
      :lists.foreach(
        fn alc ->
          conf_alloc(conf, alc)
        end,
        calc_growth_segments(conf, alcList)
      )

    format_footer(fTO)
    res
  end

  defp format_header(fTO) do
    {y, mo, d} = :erlang.date()
    {h, mi, s} = :erlang.time()
    fcl(fTO)
    fcl(fTO, 'erts_alloc configuration')
    fcl(fTO)

    fcp(
      fTO,
      'This erts_alloc configuration was automatically generated at ~w-~2..0w-~2..0w ~2..0w:~2..0w.~2..0w by erts_alloc_config.',
      [y, mo, d, h, mi, s]
    )

    fcp(fTO, '~s was used when generating the configuration.', [
      :string.trim(:erlang.system_info(:system_version), :both, '$\n')
    ])

    case :erlang.system_info(:schedulers) do
      1 ->
        :ok

      schdlrs ->
        fcp(
          fTO,
          'NOTE: This configuration was made for ~p schedulers. It is very important that ~p schedulers are used.',
          [schdlrs, schdlrs]
        )
    end

    fcp(
      fTO,
      'This configuration is intended as a suggestion and may need to be adjusted manually. Instead of modifying this file, you are advised to write another configuration file and override values that you want to change. Doing it this way simplifies things when you want to rerun erts_alloc_config.'
    )

    fcp(
      fTO,
      'This configuration is based on the actual use of multi-block carriers (mbcs) for a set of different runtime scenarios. Note that this configuration may perform bad, ever horrible, for other runtime scenarios.'
    )

    fcp(
      fTO,
      'You are advised to rerun erts_alloc_config if the applications run when the configuration was made are changed, or if the load on the applications have changed since the configuration was made. You are also advised to rerun erts_alloc_config if the Erlang runtime system used is changed.'
    )

    fcp(
      fTO,
      'Note, that the singel-block carrier (sbc) parameters very much effects the use of mbcs. Therefore, if you change the sbc parameters, you are advised to rerun erts_alloc_config.'
    )

    fcp(fTO, 'For more information see the erts_alloc_config(3) documentation.')
    :ok
  end

  defp format_footer(fTO) do
    fcl(fTO)
  end

  defp b2kb(b) when is_integer(b) do
    maxKB = div(1 <<< (:erlang.system_info(:wordsize) * 8), 1024)

    case div(b - 1, 1024) + 1 do
      kB when kB > maxKB ->
        maxKB

      kB ->
        kB
    end
  end

  defp format(false, _Frmt) do
    :ok
  end

  defp format(iODev, frmt) do
    :io.format(iODev, frmt, [])
  end

  defp format(false, _Frmt, _Args) do
    :ok
  end

  defp format(iODev, frmt, args) do
    :io.format(iODev, frmt, args)
  end

  defp fcp(iODev, frmt, args) do
    fc(iODev, frmt, args)
    format(iODev, '#~n')
  end

  defp fcp(iODev, frmt) do
    fc(iODev, frmt)
    format(iODev, '#~n')
  end

  defp fc(iODev, frmt, args) do
    fc(iODev, :lists.flatten(:io_lib.format(frmt, args)))
  end

  defp fc(iODev, string) do
    fc_aux(iODev, :string.lexemes(string, ' '), 0)
  end

  defp fc_aux(_IODev, [], 0) do
    :ok
  end

  defp fc_aux(iODev, [], _Len) do
    format(iODev, '~n')
  end

  defp fc_aux(iODev, [t | ts], 0) do
    len = 2 + :string.length(t)
    format(iODev, '# ~s', [t])
    fc_aux(iODev, ts, len)
  end

  defp fc_aux(iODev, [t | ts] = aTs, len) do
    tLength = :string.length(t)

    case tLength + len >= 76 do
      true ->
        format(iODev, '~n')
        fc_aux(iODev, aTs, 0)

      false ->
        newLen = len + 1 + tLength
        format(iODev, ' ~s', [t])
        fc_aux(iODev, ts, newLen)
    end
  end

  defp fcl(fTO) do
    endStr = '# '
    precision = :string.length(endStr)
    fieldWidth = -1 * 76
    format(fTO, '~*.*.*s~n', [fieldWidth, precision, ?-, endStr])
  end

  defp fcl(fTO, a) when is_atom(a) do
    fcl(fTO, :erlang.atom_to_list(a))
  end

  defp fcl(fTO, str) when is_list(str) do
    str2 = '# --- ' ++ str ++ ' '
    precision = :string.length(str2)
    fieldWidth = -1 * 76
    format(fTO, '~*.*.*s~n', [fieldWidth, precision, ?-, str2])
  end
end
