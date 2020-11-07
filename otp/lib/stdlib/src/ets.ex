defmodule :m_ets do
  use Bitwise
  import Kernel, except: [to_string: 1]

  def all() do
    receive_all(:ets.internal_request_all(), :erlang.system_info(:schedulers), [])
  end

  defp receive_all(_Ref, 0, all) do
    all
  end

  defp receive_all(ref, n, all) do
    receive do
      {^ref, schedAll} ->
        receive_all(ref, n - 1, schedAll ++ all)
    end
  end

  def internal_request_all() do
    :erlang.nif_error(:undef)
  end

  def delete(_) do
    :erlang.nif_error(:undef)
  end

  def delete(_, _) do
    :erlang.nif_error(:undef)
  end

  def delete_all_objects(tab) do
    _ = :ets.internal_delete_all(tab, :undefined)
    true
  end

  def internal_delete_all(_, _) do
    :erlang.nif_error(:undef)
  end

  def delete_object(_, _) do
    :erlang.nif_error(:undef)
  end

  def first(_) do
    :erlang.nif_error(:undef)
  end

  def give_away(_, _, _) do
    :erlang.nif_error(:undef)
  end

  def info(_) do
    :erlang.nif_error(:undef)
  end

  def info(_, _) do
    :erlang.nif_error(:undef)
  end

  def insert(_, _) do
    :erlang.nif_error(:undef)
  end

  def insert_new(_, _) do
    :erlang.nif_error(:undef)
  end

  def is_compiled_ms(_) do
    :erlang.nif_error(:undef)
  end

  def last(_) do
    :erlang.nif_error(:undef)
  end

  def lookup(_, _) do
    :erlang.nif_error(:undef)
  end

  def lookup_element(_, _, _) do
    :erlang.nif_error(:undef)
  end

  def match(_, _) do
    :erlang.nif_error(:undef)
  end

  def match(_, _, _) do
    :erlang.nif_error(:undef)
  end

  def match(_) do
    :erlang.nif_error(:undef)
  end

  def match_object(_, _) do
    :erlang.nif_error(:undef)
  end

  def match_object(_, _, _) do
    :erlang.nif_error(:undef)
  end

  def match_object(_) do
    :erlang.nif_error(:undef)
  end

  def match_spec_compile(_) do
    :erlang.nif_error(:undef)
  end

  def match_spec_run_r(_, _, _) do
    :erlang.nif_error(:undef)
  end

  def member(_, _) do
    :erlang.nif_error(:undef)
  end

  def new(_, _) do
    :erlang.nif_error(:undef)
  end

  def next(_, _) do
    :erlang.nif_error(:undef)
  end

  def prev(_, _) do
    :erlang.nif_error(:undef)
  end

  def rename(_, _) do
    :erlang.nif_error(:undef)
  end

  def safe_fixtable(_, _) do
    :erlang.nif_error(:undef)
  end

  def select(_, _) do
    :erlang.nif_error(:undef)
  end

  def select(_, _, _) do
    :erlang.nif_error(:undef)
  end

  def select(_) do
    :erlang.nif_error(:undef)
  end

  def select_count(_, _) do
    :erlang.nif_error(:undef)
  end

  def select_delete(tab, [{:_, [], [true]}]) do
    :ets.internal_delete_all(tab, :undefined)
  end

  def select_delete(tab, matchSpec) do
    :ets.internal_select_delete(tab, matchSpec)
  end

  def internal_select_delete(_, _) do
    :erlang.nif_error(:undef)
  end

  def select_replace(_, _) do
    :erlang.nif_error(:undef)
  end

  def select_reverse(_, _) do
    :erlang.nif_error(:undef)
  end

  def select_reverse(_, _, _) do
    :erlang.nif_error(:undef)
  end

  def select_reverse(_) do
    :erlang.nif_error(:undef)
  end

  def setopts(_, _) do
    :erlang.nif_error(:undef)
  end

  def slot(_, _) do
    :erlang.nif_error(:undef)
  end

  def take(_, _) do
    :erlang.nif_error(:undef)
  end

  def update_counter(_, _, _) do
    :erlang.nif_error(:undef)
  end

  def update_counter(_, _, _, _) do
    :erlang.nif_error(:undef)
  end

  def update_element(_, _, _) do
    :erlang.nif_error(:undef)
  end

  def whereis(_) do
    :erlang.nif_error(:undef)
  end

  def match_spec_run(list, compiledMS) do
    :lists.reverse(:ets.match_spec_run_r(list, compiledMS, []))
  end

  def repair_continuation(:"$end_of_table", _) do
    :"$end_of_table"
  end

  def repair_continuation(
        untouched = {table, lastkey, endCondition, n2, mSRef, l2, n3, n4},
        mS
      )
      when is_integer(n2) and is_list(l2) and
             is_integer(n3) and is_integer(n4) do
    case :ets.is_compiled_ms(mSRef) do
      true ->
        untouched

      false ->
        {table, lastkey, endCondition, n2, :ets.match_spec_compile(mS), l2, n3, n4}
    end
  end

  def repair_continuation(untouched = {table, n1, n2, mSRef, l, n3}, mS)
      when is_integer(n1) and is_integer(n2) and
             is_list(l) and is_integer(n3) do
    case :ets.is_compiled_ms(mSRef) do
      true ->
        untouched

      false ->
        {table, n1, n2, :ets.match_spec_compile(mS), l, n3}
    end
  end

  def fun2ms(shellFun) when is_function(shellFun) do
    case :erl_eval.fun_data(shellFun) do
      {:fun_data, importList, clauses} ->
        case :ms_transform.transform_from_shell(:ets, clauses, importList) do
          {:error, [{_, [{_, _, code} | _]} | _], _} ->
            :io.format('Error: ~ts~n', [:ms_transform.format_error(code)])
            {:error, :transform_error}

          else__ ->
            else__
        end

      _ ->
        exit(
          {:badarg,
           {:ets, :fun2ms,
            [
              :function,
              :called,
              :with,
              :real,
              :fun,
              :should,
              :be,
              :transformed,
              :with,
              :parse_transform,
              :or,
              :called,
              :with,
              :a,
              :fun,
              :generated,
              :in,
              :the,
              :shell
            ]}}
        )
    end
  end

  def foldl(f, accu, t) do
    :ets.safe_fixtable(t, true)
    first = :ets.first(t)

    try do
      do_foldl(f, accu, first, t)
    after
      :ets.safe_fixtable(t, false)
    end
  end

  defp do_foldl(f, accu0, key, t) do
    case key do
      :"$end_of_table" ->
        accu0

      _ ->
        do_foldl(f, :lists.foldl(f, accu0, :ets.lookup(t, key)), :ets.next(t, key), t)
    end
  end

  def foldr(f, accu, t) do
    :ets.safe_fixtable(t, true)
    last = :ets.last(t)

    try do
      do_foldr(f, accu, last, t)
    after
      :ets.safe_fixtable(t, false)
    end
  end

  defp do_foldr(f, accu0, key, t) do
    case key do
      :"$end_of_table" ->
        accu0

      _ ->
        do_foldr(f, :lists.foldr(f, accu0, :ets.lookup(t, key)), :ets.prev(t, key), t)
    end
  end

  def from_dets(etsTable, detsTable) do
    case (try do
            :dets.to_ets(detsTable, etsTable)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, reason} ->
        :erlang.error(reason, [etsTable, detsTable])

      {:EXIT, {reason1, _Stack1}} ->
        :erlang.error(reason1, [etsTable, detsTable])

      {:EXIT, eReason} ->
        :erlang.error(eReason, [etsTable, detsTable])

      ^etsTable ->
        true

      unexpected ->
        :erlang.error(unexpected, [etsTable, detsTable])
    end
  end

  def to_dets(etsTable, detsTable) do
    case (try do
            :dets.from_ets(detsTable, etsTable)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, reason} ->
        :erlang.error(reason, [etsTable, detsTable])

      {:EXIT, {reason1, _Stack1}} ->
        :erlang.error(reason1, [etsTable, detsTable])

      {:EXIT, eReason} ->
        :erlang.error(eReason, [etsTable, detsTable])

      :ok ->
        detsTable

      unexpected ->
        :erlang.error(unexpected, [etsTable, detsTable])
    end
  end

  def test_ms(term, mS) do
    case :erlang.match_spec_test(term, mS, :table) do
      {:ok, result, _Flags, _Messages} ->
        {:ok, result}

      {:error, _Errors} = error ->
        error
    end
  end

  def init_table(table, fun) do
    :ets.delete_all_objects(table)
    init_table_continue(table, fun.(:read))
  end

  defp init_table_continue(_Table, :end_of_input) do
    true
  end

  defp init_table_continue(table, {list, fun})
       when is_list(list) and
              is_function(fun) do
    case (try do
            init_table_sub(table, list)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, reason} ->
        try do
          fun.(:close)
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

        exit(reason)

      true ->
        init_table_continue(table, fun.(:read))
    end
  end

  defp init_table_continue(_Table, error) do
    exit(error)
  end

  defp init_table_sub(_Table, []) do
    true
  end

  defp init_table_sub(table, [h | t]) do
    :ets.insert(table, h)
    init_table_sub(table, t)
  end

  def match_delete(table, pattern) do
    :ets.select_delete(table, [{pattern, [], [true]}])
    true
  end

  def tab2list(t) do
    :ets.match_object(t, :_)
  end

  def filter(tn, f, a) when is_atom(tn) or is_integer(tn) do
    do_filter(tn, :ets.first(tn), f, a, [])
  end

  defp do_filter(_Tab, :"$end_of_table", _, _, ack) do
    ack
  end

  defp do_filter(tab, key, f, a, ack) do
    case apply(f, [:ets.lookup(tab, key) | a]) do
      false ->
        do_filter(tab, :ets.next(tab, key), f, a, ack)

      true ->
        ack2 = :ets.lookup(tab, key) ++ ack
        do_filter(tab, :ets.next(tab, key), f, a, ack2)

      {true, value} ->
        do_filter(tab, :ets.next(tab, key), f, a, [value | ack])
    end
  end

  require Record

  Record.defrecord(:r_filetab_options, :filetab_options,
    object_count: false,
    md5sum: false,
    sync: false
  )

  def tab2file(tab, file) do
    tab2file(tab, file, [])
  end

  def tab2file(tab, file, options) do
    try do
      {:ok, ftOptions} = parse_ft_options(options)
      _ = :file.delete(file)

      case :file.read_file_info(file) do
        {:error, :enoent} ->
          :ok

        _ ->
          throw(:eaccess)
      end

      name = make_ref()

      case :disk_log.open([{:name, name}, {:file, file}]) do
        {:ok, ^name} ->
          :ok

        {:error, reason} ->
          throw(reason)
      end

      try do
        info0 =
          case :ets.info(tab) do
            :undefined ->
              throw(:badtab)

            i ->
              i
          end

        info = [
          :erlang.list_to_tuple(
            info0 ++
              [
                {:major_version, 1},
                {:minor_version, 0},
                {:extended_info, ft_options_to_list(ftOptions)}
              ]
          )
        ]

        {logFun, initState} =
          case r_filetab_options(ftOptions, :md5sum) do
            true ->
              {fn oldstate, termlist ->
                 {newState, binList} =
                   md5terms(
                     oldstate,
                     termlist
                   )

                 case :disk_log.blog_terms(
                        name,
                        binList
                      ) do
                   :ok ->
                     newState

                   {:error, reason2} ->
                     throw(reason2)
                 end
               end, :erlang.md5_init()}

            false ->
              {fn _, termlist ->
                 case :disk_log.log_terms(
                        name,
                        termlist
                      ) do
                   :ok ->
                     true

                   {:error, reason2} ->
                     throw(reason2)
                 end
               end, true}
          end

        :ets.safe_fixtable(tab, true)

        {newState1, num} =
          try do
            newState = logFun.(initState, info)
            dump_file(:ets.select(tab, [{:_, [], [:"$_"]}], 100), logFun, newState, 0)
          after
            try do
              :ets.safe_fixtable(tab, false)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end
          end

        endInfo =
          case r_filetab_options(ftOptions, :object_count) do
            true ->
              [{:count, num}]

            false ->
              []
          end ++
            case r_filetab_options(ftOptions, :md5sum) do
              true ->
                [{:md5, :erlang.md5_final(newState1)}]

              false ->
                []
            end

        case endInfo do
          [] ->
            :ok

          list ->
            logFun.(newState1, [[:"$end_of_table", list]])
        end

        case r_filetab_options(ftOptions, :sync) do
          true ->
            case :disk_log.sync(name) do
              :ok ->
                :ok

              {:error, reason2} ->
                throw(reason2)
            end

          false ->
            :ok
        end

        :disk_log.close(name)
      catch
        tReason ->
          _ = :disk_log.close(name)
          _ = :file.delete(file)
          throw(tReason)

        :exit, exReason ->
          _ = :disk_log.close(name)
          _ = :file.delete(file)
          exit(exReason)

        :error, erReason ->
          _ = :disk_log.close(name)
          _ = :file.delete(file)
          :erlang.raise(:error, erReason, __STACKTRACE__)
      end
    catch
      tReason2 ->
        {:error, tReason2}

      :exit, exReason2 ->
        {:error, exReason2}
    end
  end

  defp dump_file(:"$end_of_table", _LogFun, state, num) do
    {state, num}
  end

  defp dump_file({terms, context}, logFun, state, num) do
    count = length(terms)
    newState = logFun.(state, terms)
    dump_file(:ets.select(context), logFun, newState, num + count)
  end

  defp ft_options_to_list(r_filetab_options(md5sum: mD5, object_count: pS)) do
    case pS do
      true ->
        [:object_count]

      _ ->
        []
    end ++
      case mD5 do
        true ->
          [:md5sum]

        _ ->
          []
      end
  end

  defp md5terms(state, []) do
    {state, []}
  end

  defp md5terms(state, [h | t]) do
    b = :erlang.term_to_binary(h)
    newState = :erlang.md5_update(state, b)
    {finState, tL} = md5terms(newState, t)
    {finState, [b | tL]}
  end

  defp parse_ft_options(options) when is_list(options) do
    {:ok, parse_ft_options(options, r_filetab_options(), false)}
  end

  defp parse_ft_options([], ftOpt, _) do
    ftOpt
  end

  defp parse_ft_options([{:sync, true} | rest], ftOpt, eI) do
    parse_ft_options(rest, r_filetab_options(ftOpt, sync: true), eI)
  end

  defp parse_ft_options([{:sync, false} | rest], ftOpt, eI) do
    parse_ft_options(rest, ftOpt, eI)
  end

  defp parse_ft_options([{:extended_info, l} | rest], ftOpt0, false) do
    ftOpt1 = parse_ft_info_options(ftOpt0, l)
    parse_ft_options(rest, ftOpt1, true)
  end

  defp parse_ft_options([other | _], _, _) do
    throw({:unknown_option, other})
  end

  defp parse_ft_options(malformed, _, _) do
    throw({:malformed_option, malformed})
  end

  defp parse_ft_info_options(ftOpt, []) do
    ftOpt
  end

  defp parse_ft_info_options(ftOpt, [:object_count | t]) do
    parse_ft_info_options(r_filetab_options(ftOpt, object_count: true), t)
  end

  defp parse_ft_info_options(ftOpt, [:md5sum | t]) do
    parse_ft_info_options(r_filetab_options(ftOpt, md5sum: true), t)
  end

  defp parse_ft_info_options(_, [unexpected | _]) do
    throw({:unknown_option, [{:extended_info, [unexpected]}]})
  end

  defp parse_ft_info_options(_, malformed) do
    throw({:malformed_option, malformed})
  end

  def file2tab(file) do
    file2tab(file, [])
  end

  def file2tab(file, opts) do
    try do
      {:ok, verify, tabArg} = parse_f2t_opts(opts, false, [])
      name = make_ref()

      {:ok, ^name} =
        case :disk_log.open([{:name, name}, {:file, file}, {:mode, :read_only}]) do
          {:ok, ^name} ->
            {:ok, name}

          {:repaired, ^name, _, _} ->
            case verify do
              true ->
                _ = :disk_log.close(name)
                throw(:badfile)

              false ->
                {:ok, name}
            end

          {:error, other1} ->
            throw({:read_error, other1})

          other2 ->
            throw(other2)
        end

      {:ok, major, minor, ftOptions, mD5State, fullHeader, dLContext} =
        try do
          get_header_data(name, verify)
        catch
          :badfile ->
            _ = :disk_log.close(name)
            throw(:badfile)
        end

      try do
        cond do
          major > 1 ->
            throw({:unsupported_file_version, {major, minor}})

          true ->
            :ok
        end

        {:ok, tab, headCount} = create_tab(fullHeader, tabArg)

        strippedOptions =
          case verify do
            true ->
              ftOptions

            false ->
              r_filetab_options()
          end

        {readFun, initState} =
          case r_filetab_options(strippedOptions, :md5sum) do
            true ->
              {fn {oldMD5State, oldCount, _OL, oDLContext} = oS ->
                 case wrap_bchunk(name, oDLContext, 100, verify) do
                   :eof ->
                     {oS, []}

                   {nDLContext, blist} ->
                     {termlist, newMD5State, newCount, newLast} =
                       md5_and_convert(
                         blist,
                         oldMD5State,
                         oldCount
                       )

                     {{newMD5State, newCount, newLast, nDLContext}, termlist}
                 end
               end, {mD5State, 0, [], dLContext}}

            false ->
              {fn {_, oldCount, _OL, oDLContext} = oS ->
                 case wrap_chunk(name, oDLContext, 100, verify) do
                   :eof ->
                     {oS, []}

                   {nDLContext, list} ->
                     {newLast, newCount, newList} =
                       scan_for_endinfo(
                         list,
                         oldCount
                       )

                     {{false, newCount, newLast, nDLContext}, newList}
                 end
               end, {false, 0, [], dLContext}}
          end

        try do
          do_read_and_verify(readFun, initState, tab, strippedOptions, headCount, verify)
        catch
          tReason ->
            :ets.delete(tab)
            throw(tReason)

          :exit, exReason ->
            :ets.delete(tab)
            exit(exReason)

          :error, erReason ->
            :ets.delete(tab)
            :erlang.raise(:error, erReason, __STACKTRACE__)
        end
      after
        _ = :disk_log.close(name)
      end
    catch
      tReason2 ->
        {:error, tReason2}

      :exit, exReason2 ->
        {:error, exReason2}
    end
  end

  defp do_read_and_verify(readFun, initState, tab, ftOptions, headCount, verify) do
    case load_table(readFun, initState, tab) do
      {:ok, {_, finalCount, [], _}} ->
        case {r_filetab_options(ftOptions, :md5sum), r_filetab_options(ftOptions, :object_count)} do
          {false, false} ->
            case verify do
              false ->
                :ok

              true ->
                case finalCount do
                  ^headCount ->
                    :ok

                  _ ->
                    throw(:invalid_object_count)
                end
            end

          _ ->
            throw(:badfile)
        end

        {:ok, tab}

      {:ok, {finalMD5State, finalCount, [:"$end_of_table", lastInfo], _}} ->
        eCount =
          case :lists.keyfind(:count, 1, lastInfo) do
            {:count, n} ->
              n

            _ ->
              false
          end

        eMD5 =
          case :lists.keyfind(:md5, 1, lastInfo) do
            {:md5, m} ->
              m

            _ ->
              false
          end

        case r_filetab_options(ftOptions, :md5sum) do
          true ->
            case :erlang.md5_final(finalMD5State) do
              ^eMD5 ->
                :ok

              _MD5MisM ->
                throw(:checksum_error)
            end

          false ->
            :ok
        end

        case r_filetab_options(ftOptions, :object_count) do
          true ->
            case finalCount do
              ^eCount ->
                :ok

              _Other ->
                throw(:invalid_object_count)
            end

          false ->
            case {verify, r_filetab_options(ftOptions, :md5sum)} do
              {true, false} ->
                case finalCount do
                  ^headCount ->
                    :ok

                  _Other2 ->
                    throw(:invalid_object_count)
                end

              _ ->
                :ok
            end
        end

        {:ok, tab}
    end
  end

  defp parse_f2t_opts([], verify, tab) do
    {:ok, verify, tab}
  end

  defp parse_f2t_opts([{:verify, true} | t], _OV, tab) do
    parse_f2t_opts(t, true, tab)
  end

  defp parse_f2t_opts([{:verify, false} | t], oV, tab) do
    parse_f2t_opts(t, oV, tab)
  end

  defp parse_f2t_opts([{:table, tab} | t], oV, []) do
    parse_f2t_opts(t, oV, tab)
  end

  defp parse_f2t_opts([unexpected | _], _, _) do
    throw({:unknown_option, unexpected})
  end

  defp parse_f2t_opts(malformed, _, _) do
    throw({:malformed_option, malformed})
  end

  defp count_mandatory([]) do
    0
  end

  defp count_mandatory([{tag, _} | t])
       when tag === :name or
              tag === :type or tag === :protection or
              tag === :named_table or tag === :keypos or
              tag === :size do
    1 + count_mandatory(t)
  end

  defp count_mandatory([_ | t]) do
    count_mandatory(t)
  end

  defp verify_header_mandatory(l) do
    count_mandatory(l) === 6
  end

  defp wrap_bchunk(name, c, n, true) do
    case :disk_log.bchunk(name, c, n) do
      {_, _, x} when x > 0 ->
        throw(:badfile)

      {nC, bin, _} ->
        {nC, bin}

      y ->
        y
    end
  end

  defp wrap_bchunk(name, c, n, false) do
    case :disk_log.bchunk(name, c, n) do
      {nC, bin, _} ->
        {nC, bin}

      y ->
        y
    end
  end

  defp wrap_chunk(name, c, n, true) do
    case :disk_log.chunk(name, c, n) do
      {_, _, x} when x > 0 ->
        throw(:badfile)

      {nC, tL, _} ->
        {nC, tL}

      y ->
        y
    end
  end

  defp wrap_chunk(name, c, n, false) do
    case :disk_log.chunk(name, c, n) do
      {nC, tL, _} ->
        {nC, tL}

      y ->
        y
    end
  end

  defp get_header_data(name, true) do
    case wrap_bchunk(name, :start, 1, true) do
      {c, [bin]} when is_binary(bin) ->
        t = :erlang.binary_to_term(bin)

        case t do
          tup when is_tuple(tup) ->
            l = :erlang.tuple_to_list(tup)

            case verify_header_mandatory(l) do
              false ->
                throw(:badfile)

              true ->
                major =
                  case :lists.keyfind(:major, 1, l) do
                    {:major, maj} ->
                      maj

                    _ ->
                      0
                  end

                minor =
                  case :lists.keyfind(:minor, 1, l) do
                    {:minor, min} ->
                      min

                    _ ->
                      0
                  end

                ftOptions =
                  case :lists.keyfind(:extended_info, 1, l) do
                    {:extended_info, i} when is_list(i) ->
                      r_filetab_options(
                        object_count:
                          :lists.member(
                            :object_count,
                            i
                          ),
                        md5sum: :lists.member(:md5sum, i)
                      )

                    _ ->
                      r_filetab_options()
                  end

                mD5Initial =
                  case r_filetab_options(ftOptions, :md5sum) do
                    true ->
                      x = :erlang.md5_init()
                      :erlang.md5_update(x, bin)

                    false ->
                      false
                  end

                {:ok, major, minor, ftOptions, mD5Initial, l, c}
            end

          _X ->
            throw(:badfile)
        end

      _Y ->
        throw(:badfile)
    end
  end

  defp get_header_data(name, false) do
    case wrap_chunk(name, :start, 1, false) do
      {c, [tup]} when is_tuple(tup) ->
        l = :erlang.tuple_to_list(tup)

        case verify_header_mandatory(l) do
          false ->
            throw(:badfile)

          true ->
            major =
              case :lists.keyfind(:major_version, 1, l) do
                {:major_version, maj} ->
                  maj

                _ ->
                  0
              end

            minor =
              case :lists.keyfind(:minor_version, 1, l) do
                {:minor_version, min} ->
                  min

                _ ->
                  0
              end

            ftOptions =
              case :lists.keyfind(:extended_info, 1, l) do
                {:extended_info, i} when is_list(i) ->
                  r_filetab_options(
                    object_count: :lists.member(:object_count, i),
                    md5sum: :lists.member(:md5sum, i)
                  )

                _ ->
                  r_filetab_options()
              end

            {:ok, major, minor, ftOptions, false, l, c}
        end

      _ ->
        throw(:badfile)
    end
  end

  defp md5_and_convert([], mD5State, count) do
    {[], mD5State, count, []}
  end

  defp md5_and_convert([h | t], mD5State, count) when is_binary(h) do
    case (try do
            :erlang.binary_to_term(h)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        md5_and_convert(t, mD5State, count)

      [:"$end_of_table", _Dat] = l ->
        {[], mD5State, count, l}

      term ->
        x = :erlang.md5_update(mD5State, h)
        {rest, newMD5, newCount, newLast} = md5_and_convert(t, x, count + 1)
        {[term | rest], newMD5, newCount, newLast}
    end
  end

  defp scan_for_endinfo([], count) do
    {[], count, []}
  end

  defp scan_for_endinfo([[:"$end_of_table", dat]], count) do
    {[:"$end_of_table", dat], count, []}
  end

  defp scan_for_endinfo([term | t], count) do
    {newLast, nCount, rest} = scan_for_endinfo(t, count + 1)
    {newLast, nCount, [term | rest]}
  end

  defp load_table(readFun, state, tab) do
    {newState, newData} = readFun.(state)

    case newData do
      [] ->
        {:ok, newState}

      list ->
        :ets.insert(tab, list)
        load_table(readFun, newState, tab)
    end
  end

  defp create_tab(i, tabArg) do
    {:name, name} = :lists.keyfind(:name, 1, i)
    {:type, type} = :lists.keyfind(:type, 1, i)
    {:protection, p} = :lists.keyfind(:protection, 1, i)
    {:keypos, _Kp} = keypos = :lists.keyfind(:keypos, 1, i)
    {:size, sz} = :lists.keyfind(:size, 1, i)
    l1 = [type, p, keypos]

    l2 =
      case :lists.keyfind(:named_table, 1, i) do
        {:named_table, true} ->
          [:named_table | l1]

        {:named_table, false} ->
          l1
      end

    l3 =
      case :lists.keyfind(:compressed, 1, i) do
        {:compressed, true} ->
          [:compressed | l2]

        {:compressed, false} ->
          l2

        false ->
          l2
      end

    l4 =
      case :lists.keyfind(:write_concurrency, 1, i) do
        {:write_concurrency, _} = wcc ->
          [wcc | l3]

        _ ->
          l3
      end

    l5 =
      case :lists.keyfind(:read_concurrency, 1, i) do
        {:read_concurrency, _} = rcc ->
          [rcc | l4]

        false ->
          l4
      end

    case tabArg do
      [] ->
        try do
          tab = :ets.new(name, l5)
          {:ok, tab, sz}
        catch
          _, _ ->
            throw(:cannot_create_table)
        end

      _ ->
        {:ok, tabArg, sz}
    end
  end

  def tabfile_info(file) when is_list(file) or is_atom(file) do
    try do
      name = make_ref()

      {:ok, ^name} =
        case :disk_log.open([{:name, name}, {:file, file}, {:mode, :read_only}]) do
          {:ok, ^name} ->
            {:ok, name}

          {:repaired, ^name, _, _} ->
            {:ok, name}

          {:error, other1} ->
            throw({:read_error, other1})

          other2 ->
            throw(other2)
        end

      {:ok, major, minor, _FtOptions, _MD5State, fullHeader, _DLContext} =
        try do
          get_header_data(name, false)
        catch
          :badfile ->
            _ = :disk_log.close(name)
            throw(:badfile)
        end

      case :disk_log.close(name) do
        :ok ->
          :ok

        {:error, reason} ->
          throw(reason)
      end

      {:value, n} = :lists.keysearch(:name, 1, fullHeader)
      {:value, type} = :lists.keysearch(:type, 1, fullHeader)
      {:value, p} = :lists.keysearch(:protection, 1, fullHeader)
      {:value, val} = :lists.keysearch(:named_table, 1, fullHeader)
      {:value, kp} = :lists.keysearch(:keypos, 1, fullHeader)
      {:value, sz} = :lists.keysearch(:size, 1, fullHeader)

      ei =
        case :lists.keyfind(:extended_info, 1, fullHeader) do
          false ->
            {:extended_info, []}

          ei0 ->
            ei0
        end

      {:ok, [n, type, p, val, kp, sz, ei, {:version, {major, minor}}]}
    catch
      tReason ->
        {:error, tReason}

      :exit, exReason ->
        {:error, exReason}
    end
  end

  def table(tab) do
    table(tab, [])
  end

  def table(tab, opts) do
    case options(opts, [:traverse, :n_objects]) do
      {:badarg, _} ->
        :erlang.error(:badarg, [tab, opts])

      [[traverse, nObjs], qlcOptions] ->
        tF =
          case traverse do
            :first_next ->
              fn ->
                qlc_next(tab, :ets.first(tab))
              end

            :last_prev ->
              fn ->
                qlc_prev(tab, :ets.last(tab))
              end

            :select ->
              fn mS ->
                qlc_select(:ets.select(tab, mS, nObjs))
              end

            {:select, mS} ->
              fn ->
                qlc_select(:ets.select(tab, mS, nObjs))
              end
          end

        preFun = fn _ ->
          :ets.safe_fixtable(tab, true)
        end

        postFun = fn ->
          :ets.safe_fixtable(tab, false)
        end

        infoFun = fn tag ->
          table_info(tab, tag)
        end

        keyEquality =
          case :ets.info(tab, :type) do
            :ordered_set ->
              :==

            _ ->
              :"=:="
          end

        lookupFun =
          case traverse do
            {:select, _MS} ->
              :undefined

            _ ->
              fn
                _Pos, [k] ->
                  :ets.lookup(tab, k)

                _Pos, ks ->
                  :lists.flatmap(
                    fn k ->
                      :ets.lookup(tab, k)
                    end,
                    ks
                  )
              end
          end

        formatFun = fn
          {:all, _NElements, _ElementFun} ->
            as = [
              tab
              | for _ <- [[]], opts !== [] do
                  opts
                end
            ]

            {:ets, :table, as}

          {:match_spec, mS} ->
            {:ets, :table, [tab, [{:traverse, {:select, mS}} | listify(opts)]]}

          {:lookup, _KeyPos, [value], _NElements, elementFun} ->
            :io_lib.format('~w:lookup(~w, ~w)', [:ets, tab, elementFun.(value)])

          {:lookup, _KeyPos, values, _NElements, elementFun} ->
            vals =
              for v <- values do
                elementFun.(v)
              end

            :io_lib.format('lists:flatmap(fun(V) -> ~w:lookup(~w, V) end, ~w)', [:ets, tab, vals])
        end

        :qlc.table(
          tF,
          [
            {:pre_fun, preFun},
            {:post_fun, postFun},
            {:info_fun, infoFun},
            {:format_fun, formatFun},
            {:key_equality, keyEquality},
            {:lookup_fun, lookupFun}
          ] ++ qlcOptions
        )
    end
  end

  defp table_info(tab, :num_of_objects) do
    :ets.info(tab, :size)
  end

  defp table_info(tab, :keypos) do
    :ets.info(tab, :keypos)
  end

  defp table_info(tab, :is_unique_objects) do
    :ets.info(tab, :type) !== :duplicate_bag
  end

  defp table_info(tab, :is_sorted_key) do
    :ets.info(tab, :type) === :ordered_set
  end

  defp table_info(_Tab, _) do
    :undefined
  end

  defp qlc_next(_Tab, :"$end_of_table") do
    []
  end

  defp qlc_next(tab, key) do
    :ets.lookup(tab, key) ++
      fn ->
        qlc_next(tab, :ets.next(tab, key))
      end
  end

  defp qlc_prev(_Tab, :"$end_of_table") do
    []
  end

  defp qlc_prev(tab, key) do
    :ets.lookup(tab, key) ++
      fn ->
        qlc_prev(tab, :ets.prev(tab, key))
      end
  end

  defp qlc_select(:"$end_of_table") do
    []
  end

  defp qlc_select({objects, cont}) do
    objects ++
      fn ->
        qlc_select(:ets.select(cont))
      end
  end

  defp options(options, keys) when is_list(options) do
    options(options, keys, [])
  end

  defp options(option, keys) do
    options([option], keys, [])
  end

  defp options(options, [key | keys], l)
       when is_list(options) do
    v =
      case :lists.keyfind(key, 1, options) do
        {:n_objects, :default} ->
          {:ok, default_option(key)}

        {:n_objects, nObjs}
        when is_integer(nObjs) and
               nObjs >= 1 ->
          {:ok, nObjs}

        {:traverse, :select} ->
          {:ok, :select}

        {:traverse, {:select, _MS} = select} ->
          {:ok, select}

        {:traverse, :first_next} ->
          {:ok, :first_next}

        {:traverse, :last_prev} ->
          {:ok, :last_prev}

        {^key, _} ->
          :badarg

        false ->
          default = default_option(key)
          {:ok, default}
      end

    case v do
      :badarg ->
        {:badarg, key}

      {:ok, value} ->
        newOptions = :lists.keydelete(key, 1, options)
        options(newOptions, keys, [value | l])
    end
  end

  defp options(options, [], l) do
    [:lists.reverse(l), options]
  end

  defp default_option(:traverse) do
    :select
  end

  defp default_option(:n_objects) do
    100
  end

  defp listify(l) when is_list(l) do
    l
  end

  defp listify(t) do
    [t]
  end

  def i() do
    hform(:id, :name, :type, :size, :mem, :owner)
    :io.format(' ----------------------------------------------------------------------------\n')
    :lists.foreach(&prinfo/1, tabs())
    :ok
  end

  defp tabs() do
    :lists.sort(:ets.all())
  end

  defp prinfo(tab) do
    case (try do
            prinfo2(tab)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        :io.format('~-10s ... unreadable \n', [to_string(tab)])

      :ok ->
        :ok
    end
  end

  defp prinfo2(tab) do
    name = :ets.info(tab, :name)
    type = :ets.info(tab, :type)
    size = :ets.info(tab, :size)
    mem = :ets.info(tab, :memory)
    owner = :ets.info(tab, :owner)
    hform(tab, name, type, size, mem, is_reg(owner))
  end

  defp is_reg(owner) do
    case :erlang.process_info(owner, :registered_name) do
      {:registered_name, name} ->
        name

      _ ->
        owner
    end
  end

  defp hform(a0, b0, c0, d0, e0, f0) do
    [a, b, c, d, e, f] =
      for t <- [a0, b0, c0, d0, e0, f0] do
        to_string(t)
      end

    a1 = pad_right(a, 15)
    b1 = pad_right(b, 17)
    c1 = pad_right(c, 5)
    d1 = pad_right(d, 6)
    e1 = pad_right(e, 8)
    :io.format(' ~s ~s ~s ~s ~s ~s\n', [a1, b1, c1, d1, e1, f])
  end

  defp pad_right(string, len) do
    cond do
      length(string) >= len ->
        string

      true ->
        [space] = ' '
        string ++ :lists.duplicate(len - length(string), space)
    end
  end

  defp to_string(x) do
    :lists.flatten(:io_lib.format('~p', [x]))
  end

  def i(tab) do
    i(tab, 40)
  end

  def i(tab, height) do
    i(tab, height, 80)
  end

  def i(tab, height, width) do
    first = :ets.first(tab)
    display_items(height, width, tab, first, 1, 1)
  end

  defp display_items(height, width, tab, :"$end_of_table", turn, opos) do
    p = :"EOT  (q)uit (p)Digits (k)ill /Regexp -->"
    choice(height, width, p, :eot, tab, :"$end_of_table", turn, opos)
  end

  defp display_items(height, width, tab, key, turn, opos)
       when turn < height do
    do_display(height, width, tab, key, turn, opos)
  end

  defp display_items(height, width, tab, key, turn, opos)
       when turn >= height do
    p = :"(c)ontinue (q)uit (p)Digits (k)ill /Regexp -->"
    choice(height, width, p, :normal, tab, key, turn, opos)
  end

  defp choice(height, width, p, mode, tab, key, turn, opos) do
    case get_line(p, 'c\n') do
      'c\n' when mode === :normal ->
        do_display(height, width, tab, key, 1, opos)

      'c\n'
      when is_tuple(mode) and
             :erlang.element(1, mode) === :re ->
        {:re, re} = mode
        re_search(height, width, tab, key, re, 1, opos)

      'q\n' ->
        :ok

      'k\n' ->
        :ets.delete(tab)
        :ok

      [?p | digs] ->
        try do
          case (try do
                  :erlang.list_to_integer(nonl(digs))
                catch
                  :error, e -> {:EXIT, {e, __STACKTRACE__}}
                  :exit, e -> {:EXIT, e}
                  e -> e
                end) do
            {:EXIT, _} ->
              :io.put_chars('Bad digits\n')

            number when mode === :normal ->
              print_number(tab, :ets.first(tab), number)

            number when mode === :eot ->
              print_number(tab, :ets.first(tab), number)

            number ->
              {:re, re} = mode
              print_re_num(tab, :ets.first(tab), number, re)
          end
        catch
          :error, e -> {:EXIT, {e, __STACKTRACE__}}
          :exit, e -> {:EXIT, e}
          e -> e
        end

        choice(height, width, p, mode, tab, key, turn, opos)

      [?/ | regexp] ->
        case :re.compile(nonl(regexp), [:unicode]) do
          {:ok, re} ->
            re_search(height, width, tab, :ets.first(tab), re, 1, 1)

          {:error, {errorString, _Pos}} ->
            :io.format('~ts\n', [errorString])
            choice(height, width, p, mode, tab, key, turn, opos)
        end

      :eof ->
        :ok

      _ ->
        choice(height, width, p, mode, tab, key, turn, opos)
    end
  end

  defp get_line(p, default) do
    case line_string(:io.get_line(p)) do
      '\n' ->
        default

      l ->
        l
    end
  end

  defp line_string(binary) when is_binary(binary) do
    :unicode.characters_to_list(binary)
  end

  defp line_string(other) do
    other
  end

  defp nonl(s) do
    :string.trim(s, :trailing, '$\n')
  end

  defp print_number(tab, key, num) do
    os = :ets.lookup(tab, key)
    len = length(os)

    cond do
      num - len < 1 ->
        o = :lists.nth(num, os)
        :io.format('~p~n', [o])

      true ->
        print_number(tab, :ets.next(tab, key), num - len)
    end
  end

  defp do_display(height, width, tab, key, turn, opos) do
    objs = :ets.lookup(tab, key)
    do_display_items(height, width, objs, opos)
    len = length(objs)
    display_items(height, width, tab, :ets.next(tab, key), turn + len, opos + len)
  end

  defp do_display_items(height, width, [obj | tail], opos) do
    do_display_item(height, width, obj, opos)
    do_display_items(height, width, tail, opos + 1)
  end

  defp do_display_items(_Height, _Width, [], opos) do
    opos
  end

  defp do_display_item(_Height, width, i, opos) do
    l = to_string(i)

    l2 =
      cond do
        length(l) > width - 8 ->
          :string.slice(l, 0, width - 13) ++ '  ...'

        true ->
          l
      end

    :io.format('<~-4w> ~s~n', [opos, l2])
  end

  defp re_search(height, width, tab, :"$end_of_table", re, turn, opos) do
    p = :"EOT  (q)uit (p)Digits (k)ill /Regexp -->"
    choice(height, width, p, {:re, re}, tab, :"$end_of_table", turn, opos)
  end

  defp re_search(height, width, tab, key, re, turn, opos)
       when turn < height do
    re_display(height, width, tab, key, :ets.lookup(tab, key), re, turn, opos)
  end

  defp re_search(height, width, tab, key, re, turn, opos) do
    p = :"(c)ontinue (q)uit (p)Digits (k)ill /Regexp -->"
    choice(height, width, p, {:re, re}, tab, key, turn, opos)
  end

  defp re_display(height, width, tab, key, [], re, turn, opos) do
    re_search(height, width, tab, :ets.next(tab, key), re, turn, opos)
  end

  defp re_display(height, width, tab, key, [h | t], re, turn, opos) do
    str = to_string(h)

    case :re.run(str, re, [{:capture, :none}]) do
      :match ->
        do_display_item(height, width, h, opos)
        re_display(height, width, tab, key, t, re, turn + 1, opos + 1)

      :nomatch ->
        re_display(height, width, tab, key, t, re, turn, opos)
    end
  end

  defp print_re_num(_, :"$end_of_table", _, _) do
    :ok
  end

  defp print_re_num(tab, key, num, re) do
    os = re_match(:ets.lookup(tab, key), re)
    len = length(os)

    cond do
      num - len < 1 ->
        o = :lists.nth(num, os)
        :io.format('~p~n', [o])

      true ->
        print_re_num(tab, :ets.next(tab, key), num - len, re)
    end
  end

  defp re_match([], _) do
    []
  end

  defp re_match([h | t], re) do
    case :re.run(to_string(h), re, [{:capture, :none}]) do
      :match ->
        [h | re_match(t, re)]

      :nomatch ->
        re_match(t, re)
    end
  end
end
