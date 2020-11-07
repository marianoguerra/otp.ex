defmodule :m_systools_relup do
  use Bitwise
  require Record

  Record.defrecord(:r_release, :release,
    name: :undefined,
    vsn: :undefined,
    erts_vsn: :undefined,
    applications: :undefined,
    incl_apps: :undefined
  )

  Record.defrecord(:r_application, :application,
    name: :undefined,
    type: :permanent,
    vsn: '',
    id: '',
    description: '',
    modules: [],
    uses: [],
    includes: [],
    regs: [],
    env: [],
    maxT: :infinity,
    maxP: :infinity,
    mod: [],
    start_phases: :undefined,
    dir: ''
  )

  def mk_relup(topRelFile, baseUpRelDcs, baseDnRelDcs) do
    mk_relup(topRelFile, baseUpRelDcs, baseDnRelDcs, [])
  end

  def mk_relup(topRelFile, baseUpRelDcs, baseDnRelDcs, opts) do
    case check_opts(opts) do
      [] ->
        r =
          try do
            do_mk_relup(topRelFile, baseUpRelDcs, baseDnRelDcs, add_code_path(opts), opts)
          catch
            error ->
              error
          end

        done_mk_relup(opts, r)

      badArg ->
        :erlang.error({:badarg, badArg})
    end
  end

  defp check_opts([{:outdir, dir} | _Opts]) when is_list(dir) do
    []
  end

  defp check_opts([{:outdir, badArg} | _Opts]) do
    [{:outdir, badArg}]
  end

  defp check_opts([_Opt | opts]) do
    check_opts(opts)
  end

  defp check_opts([]) do
    []
  end

  defp do_mk_relup(topRelFile, baseUpRelDcs, baseDnRelDcs, path, opts) do
    case :systools_make.get_release(
           to_list(topRelFile),
           path
         ) do
      {:ok, topRel, nameVsnApps, ws0} ->
        case :lists.member({:warning, :missing_sasl}, ws0) do
          true ->
            throw({:error, :systools_relup, {:missing_sasl, topRel}})

          false ->
            :ok
        end

        topApps =
          :lists.map(
            fn {_, app} ->
              app
            end,
            nameVsnApps
          )

        {up, ws1} = foreach_baserel_up(topRel, topApps, baseUpRelDcs, path, opts, ws0)
        {dn, ws2} = foreach_baserel_dn(topRel, topApps, baseDnRelDcs, path, opts, ws1)
        relup = {r_release(topRel, :vsn), up, dn}
        {:ok, relup, ws2}

      other ->
        other
    end
  end

  defp done_mk_relup(opts, {:ok, relup, ws}) do
    wAE = get_opt(:warnings_as_errors, opts)
    silent = get_opt(:silent, opts)
    noexec = get_opt(:noexec, opts)

    cond do
      wAE and ws !== [] ->
        return_error(
          silent,
          {:error, :systools_relup, {:warnings_treated_as_errors, ws}}
        )

      not noexec ->
        case write_relup_file(relup, opts) do
          :ok ->
            return_ok(silent, relup, ws)

          error ->
            return_error(silent, error)
        end

      true ->
        return_ok(true, relup, ws)
    end
  end

  defp done_mk_relup(opts, error) do
    return_error(
      get_opt(:silent, opts) or
        get_opt(
          :noexec,
          opts
        ),
      error
    )
  end

  defp return_error(true, error) do
    error
  end

  defp return_error(false, error) do
    print_error(error)
    :error
  end

  defp return_ok(true, relup, ws) do
    {:ok, relup, :systools_relup, ws}
  end

  defp return_ok(false, _Relup, ws) do
    print_warnings(ws)
    :ok
  end

  defp foreach_baserel_up(topRel, topApps, baseRelDcs, path, opts, ws) do
    foreach_baserel_up(topRel, topApps, baseRelDcs, path, opts, ws, [])
  end

  defp foreach_baserel_up(topRel, topApps, [baseRelDc | baseRelDcs], path, opts, ws, acc) do
    baseRelFile = extract_filename(baseRelDc)

    {baseRel, {baseNameVsns, baseApps}, ws0} =
      case :systools_make.get_release(
             baseRelFile,
             path
           ) do
        {:ok, bR, nameVsnApps, warns} ->
          case :lists.member(
                 {:warning, :missing_sasl},
                 warns
               ) do
            true ->
              throw({:error, :systools_relup, {:missing_sasl, bR}})

            false ->
              {bR, :lists.unzip(nameVsnApps), warns}
          end

        other1 ->
          throw(other1)
      end

    {rUs1, ws1} = collect_appup_scripts(:up, topApps, baseRel, ws0 ++ ws, [])
    {rUs2, ws2} = create_add_app_scripts(baseRel, topRel, rUs1, ws1)
    {rUs3, ws3} = create_remove_app_scripts(baseRel, topRel, rUs2, ws2)
    {rUs4, ws4} = check_for_emulator_restart(topRel, baseRel, rUs3, ws3, opts)

    case :systools_rc.translate_scripts(:up, rUs4, topApps, baseApps) do
      {:ok, rUs5} ->
        {rUs, ws5} = fix_r15_sasl_upgrade(rUs5, ws4, baseNameVsns)
        vDR = {r_release(baseRel, :vsn), extract_description(baseRelDc), rUs}
        foreach_baserel_up(topRel, topApps, baseRelDcs, path, opts, ws5, [vDR | acc])

      xXX ->
        throw(xXX)
    end
  end

  defp foreach_baserel_up(_, _, [], _, _, ws, acc) do
    {acc, ws}
  end

  defp foreach_baserel_dn(topRel, topApps, baseRelDcs, path, opts, ws) do
    foreach_baserel_dn(topRel, topApps, baseRelDcs, path, opts, ws, [])
  end

  defp foreach_baserel_dn(topRel, topApps, [baseRelDc | baseRelDcs], path, opts, ws, acc) do
    baseRelFile = extract_filename(baseRelDc)

    {baseRel, baseApps, ws0} =
      case :systools_make.get_release(
             baseRelFile,
             path
           ) do
        {:ok, bR, nameVsnApps, warns} ->
          case :lists.member(
                 {:warning, :missing_sasl},
                 warns
               ) do
            true ->
              throw({:error, :systools_relup, {:missing_sasl, bR}})

            false ->
              nApps =
                :lists.map(
                  fn {_, app} ->
                    app
                  end,
                  nameVsnApps
                )

              {bR, nApps, warns}
          end

        other ->
          throw(other)
      end

    {rUs1, ws1} = collect_appup_scripts(:dn, topApps, baseRel, ws0 ++ ws, [])
    {rUs2, ws2} = create_add_app_scripts(topRel, baseRel, rUs1, ws1)
    {rUs3, ws3} = create_remove_app_scripts(topRel, baseRel, rUs2, ws2)
    {rUs4, ws4} = check_for_emulator_restart(topRel, baseRel, rUs3, ws3, opts)

    case :systools_rc.translate_scripts(:dn, rUs4, baseApps, topApps) do
      {:ok, rUs} ->
        vDR = {r_release(baseRel, :vsn), extract_description(baseRelDc), rUs}
        foreach_baserel_dn(topRel, topApps, baseRelDcs, path, opts, ws4, [vDR | acc])

      xXX ->
        throw(xXX)
    end
  end

  defp foreach_baserel_dn(_, _, [], _, _, ws, acc) do
    {acc, ws}
  end

  defp check_for_emulator_restart(
         r_release(erts_vsn: vsn1, name: n1),
         r_release(erts_vsn: vsn2, name: n2),
         rUs,
         ws,
         opts
       )
       when vsn1 != vsn2 do
    newRUs = [[:restart_new_emulator] | rUs]

    newWs = [
      {:erts_vsn_changed, {{n1, vsn1}, {n2, vsn2}}}
      | ws
    ]

    check_for_restart_emulator_opt(newRUs, newWs, opts)
  end

  defp check_for_emulator_restart(_, _, rUs, ws, opts) do
    check_for_restart_emulator_opt(rUs, ws, opts)
  end

  defp check_for_restart_emulator_opt(rUs, ws, opts) do
    case get_opt(:restart_emulator, opts) do
      true ->
        {rUs ++ [[:restart_emulator]], ws}

      _ ->
        {rUs, ws}
    end
  end

  defp fix_r15_sasl_upgrade([:restart_new_emulator | restRUs] = rUs, ws, baseApps) do
    case :lists.keyfind(:sasl, 1, baseApps) do
      {:sasl, vsn} when vsn < '2.2' ->
        {:lists.delete(
           :restart_emulator,
           restRUs
         ) ++ [:restart_new_emulator], [:pre_R15_emulator_upgrade | ws]}

      _ ->
        {rUs, ws}
    end
  end

  defp fix_r15_sasl_upgrade(rUs, ws, _BaseApps) do
    {rUs, ws}
  end

  defp collect_appup_scripts(mode, [topApp | topApps], baseRel, ws, rUs) do
    case :lists.keysearch(r_application(topApp, :name), 1, r_release(baseRel, :applications)) do
      {:value, {_Name, baseVsn, _Type}} ->
        cond do
          r_application(topApp, :vsn) == baseVsn ->
            collect_appup_scripts(mode, topApps, baseRel, ws, rUs)

          true ->
            {rU1s, ws1} = get_script_from_appup(mode, topApp, baseVsn, ws, rUs)
            collect_appup_scripts(mode, topApps, baseRel, ws1, rU1s)
        end

      false ->
        collect_appup_scripts(mode, topApps, baseRel, ws, rUs)
    end
  end

  defp collect_appup_scripts(_, [], _, ws, rUs) do
    {rUs, ws}
  end

  defp create_add_app_scripts(fromRel, toRel, rU0s, w0s) do
    addedNs =
      for {n, _V, t} <- r_release(toRel, :applications),
          not :lists.keymember(n, 1, r_release(fromRel, :applications)) do
        {n, t}
      end

    rUs =
      for {n, t} <- addedNs do
        [{:add_application, n, t}]
      end

    {rUs ++ rU0s, w0s}
  end

  defp create_remove_app_scripts(fromRel, toRel, rU0s, w0s) do
    removedNs =
      for {n, _V, _T} <- r_release(fromRel, :applications),
          not :lists.keymember(n, 1, r_release(toRel, :applications)) do
        n
      end

    rUs =
      for n <- removedNs do
        [{:remove_application, n}]
      end

    {rUs ++ rU0s, w0s}
  end

  defp get_script_from_appup(mode, topApp, baseVsn, ws, rUs) do
    fName =
      :filename.join([
        r_application(topApp, :dir),
        to_list(r_application(topApp, :name)) ++ '.appup'
      ])

    {vsnRUs, topVsn} =
      case :systools_lib.read_term(fName) do
        {:ok, {topVsn0, upVsnRUs, dnVsnRUs}} ->
          vsnRUs0 =
            case mode do
              :up ->
                upVsnRUs

              :dn ->
                dnVsnRUs
            end

          {vsnRUs0, topVsn0}

        x ->
          throw({:error, :systools_relup, {:file_problem, {fName, x}}})
      end

    ws1 =
      cond do
        r_application(topApp, :vsn) == topVsn ->
          ws

        true ->
          [{:bad_vsn, {topVsn, r_application(topApp, :vsn)}} | ws]
      end

    case appup_search_for_version(baseVsn, vsnRUs) do
      {:ok, rU} ->
        {rUs ++ [rU], ws1}

      :error ->
        throw({:error, :systools_relup, {:no_relup, fName, topApp, baseVsn}})
    end
  end

  def appup_search_for_version(baseVsn, [{baseVsn, rU} | _]) do
    {:ok, rU}
  end

  def appup_search_for_version(baseVsn, [{vsn, rU} | vsnRUs])
      when is_binary(vsn) do
    case :re.run(baseVsn, vsn, [:unicode, {:capture, :first, :list}]) do
      {:match, [^baseVsn]} ->
        {:ok, rU}

      _ ->
        appup_search_for_version(baseVsn, vsnRUs)
    end
  end

  def appup_search_for_version(baseVsn, [_ | vsnRUs]) do
    appup_search_for_version(baseVsn, vsnRUs)
  end

  def appup_search_for_version(_, []) do
    :error
  end

  defp extract_filename({n, _D}) do
    to_list(n)
  end

  defp extract_filename(n) do
    to_list(n)
  end

  defp extract_description({_N, d}) do
    d
  end

  defp extract_description(_) do
    []
  end

  defp to_list(x) when is_atom(x) do
    :erlang.atom_to_list(x)
  end

  defp to_list(x) when is_list(x) do
    x
  end

  defp write_relup_file(relup, opts) do
    filename =
      :filename.join(
        :filename.absname(
          get_opt(
            :outdir,
            opts
          )
        ),
        'relup'
      )

    case :file.open(
           filename,
           [:write, {:encoding, :utf8}]
         ) do
      {:ok, fd} ->
        :io.format(fd, '%% ~s~n~tp.~n', [:epp.encoding_to_string(:utf8), relup])

        case :file.close(fd) do
          :ok ->
            :ok

          {:error, reason} ->
            {:error, :systools_relup, {:file_problem, {'relup', {:close, reason}}}}
        end

      {:error, reason} ->
        {:error, :systools_relup, {:file_problem, {'relup', {:open, reason}}}}
    end
  end

  defp add_code_path(opts) do
    case get_opt(:path, opts) do
      false ->
        :code.get_path()

      paths0 ->
        paths1 =
          for p <- paths0 do
            to_list(p)
          end

        paths2 = :systools_lib.get_path(paths1)
        make_set(paths2 ++ :code.get_path())
    end
  end

  defp get_opt(opt, opts) do
    case :lists.keysearch(opt, 1, opts) do
      {:value, {_, val}} ->
        val

      _ ->
        case :lists.member(opt, opts) do
          true ->
            true

          _ ->
            default(opt)
        end
    end
  end

  defp make_set([]) do
    []
  end

  defp make_set([h | t]) do
    [
      h
      | for y <- make_set(t), y !== h do
          y
        end
    ]
  end

  defp default(:path) do
    false
  end

  defp default(:noexec) do
    false
  end

  defp default(:silent) do
    false
  end

  defp default(:restart_emulator) do
    false
  end

  defp default(:outdir) do
    '.'
  end

  defp default(:warnings_as_errors) do
    false
  end

  defp print_error({:error, mod, error}) do
    s = apply(mod, :format_error, [error])
    :io.format('~ts', [s])
  end

  defp print_error(other) do
    :io.format('Error: ~tp~n', [other])
  end

  def format_error({:file_problem, {file, what}}) do
    :io_lib.format('Could not ~w file ~ts~n', [get_reason(what), file])
  end

  def format_error({:no_relup, file, app, vsn}) do
    :io_lib.format(
      'No release upgrade script entry for ~w-~ts to ~w-~ts in file ~ts~n',
      [r_application(app, :name), r_application(app, :vsn), r_application(app, :name), vsn, file]
    )
  end

  def format_error({:missing_sasl, release}) do
    :io_lib.format('No sasl application in release ~ts, ~ts. Can not be upgraded.', [
      r_release(release, :name),
      r_release(release, :vsn)
    ])
  end

  def format_error({:warnings_treated_as_errors, warnings}) do
    :io_lib.format(
      'Warnings being treated as errors:~n~ts',
      [
        for w <- warnings do
          format_warning('', w)
        end
      ]
    )
  end

  def format_error(error) do
    :io_lib.format('~tp~n', [error])
  end

  defp print_warnings(ws) when is_list(ws) do
    :lists.foreach(
      fn w ->
        print_warning(w)
      end,
      ws
    )
  end

  defp print_warnings(w) do
    print_warning(w)
  end

  defp print_warning(w) do
    :io.format('~ts', [format_warning(w)])
  end

  def format_warning(w) do
    format_warning('*WARNING* ', w)
  end

  defp format_warning(prefix, {:erts_vsn_changed, {rel1, rel2}}) do
    :io_lib.format('~tsThe ERTS version changed between ~tp and ~tp~n', [prefix, rel1, rel2])
  end

  defp format_warning(prefix, :pre_R15_emulator_upgrade) do
    :io_lib.format(
      '~tsUpgrade from an OTP version earlier than R15. New code should be compiled with the old emulator.~n',
      [prefix]
    )
  end

  defp format_warning(prefix, what) do
    :io_lib.format('~ts~tp~n', [prefix, what])
  end

  defp get_reason({:error, {:open, _, _}}) do
    :open
  end

  defp get_reason({:error, {:read, _, _}}) do
    :read
  end

  defp get_reason({:error, {:parse, _, _}}) do
    :parse
  end

  defp get_reason({:error, {:close, _, _}}) do
    :close
  end

  defp get_reason({:error, {:open, _}}) do
    :open
  end

  defp get_reason({:error, {:read, _}}) do
    :read
  end

  defp get_reason({:error, {:parse, _}}) do
    :parse
  end

  defp get_reason({:open, _}) do
    :open
  end

  defp get_reason({:read, _}) do
    :read
  end

  defp get_reason({:parse, _}) do
    :parse
  end

  defp get_reason({:close, _}) do
    :close
  end

  defp get_reason(:open) do
    :open
  end

  defp get_reason(:read) do
    :read
  end

  defp get_reason(:parse) do
    :parse
  end
end
