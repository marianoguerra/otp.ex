defmodule :m_re do
  use Bitwise
  import Kernel, except: [inspect: 2]

  def version() do
    :erlang.nif_error(:undef)
  end

  def compile(_) do
    :erlang.nif_error(:undef)
  end

  def compile(_, _) do
    :erlang.nif_error(:undef)
  end

  def run(_, _) do
    :erlang.nif_error(:undef)
  end

  def run(_, _, _) do
    :erlang.nif_error(:undef)
  end

  def internal_run(_, _, _, _) do
    :erlang.nif_error(:undef)
  end

  def inspect(_, _) do
    :erlang.nif_error(:undef)
  end

  def split(subject, rE) do
    split(subject, rE, [])
  end

  def split(subject, rE, options) do
    try do
      {newOpt, convert, limit, strip, group} =
        process_split_params(options, :iodata, -1, false, false)

      unicode = check_for_unicode(rE, options)
      flatSubject = to_binary(subject, unicode)

      case compile_split(rE, newOpt) do
        {:error, _Err} ->
          throw(:badre)

        {preCompiled, numSub, runOpt} ->
          case :re.run(flatSubject, preCompiled, runOpt ++ [:global]) do
            :nomatch ->
              case group do
                true ->
                  convert_any_split_result([[flatSubject]], convert, unicode, true)

                false ->
                  convert_any_split_result([flatSubject], convert, unicode, false)
              end

            {:match, matches} ->
              res = do_split(flatSubject, 0, matches, numSub, limit, group)

              stripped =
                case strip do
                  true ->
                    backstrip_empty(res, group)

                  false ->
                    res
                end

              convert_any_split_result(stripped, convert, unicode, group)
          end
      end
    catch
      :badopt ->
        :erlang.error(:badarg, [subject, rE, options])

      :badre ->
        :erlang.error(:badarg, [subject, rE, options])

      :error, :badarg ->
        :erlang.error(:badarg, [subject, rE, options])
    end
  end

  defp backstrip_empty(list, false) do
    do_backstrip_empty(list)
  end

  defp backstrip_empty(list, true) do
    do_backstrip_empty_g(list)
  end

  defp do_backstrip_empty_g([]) do
    []
  end

  defp do_backstrip_empty_g([h]) do
    case do_backstrip_empty(h) do
      [] ->
        []

      _ ->
        [h]
    end
  end

  defp do_backstrip_empty_g([h | t]) do
    case do_backstrip_empty_g(t) do
      [] ->
        case do_backstrip_empty(h) do
          [] ->
            []

          _ ->
            [h]
        end

      other ->
        [h | other]
    end
  end

  defp do_backstrip_empty([]) do
    []
  end

  defp do_backstrip_empty([<<>>]) do
    []
  end

  defp do_backstrip_empty([<<>> | t]) do
    case do_backstrip_empty(t) do
      [] ->
        []

      other ->
        [<<>> | other]
    end
  end

  defp do_backstrip_empty([h | t]) do
    [h | do_backstrip_empty(t)]
  end

  defp convert_any_split_result(list, type, uni, true) do
    for part <- list do
      convert_split_result(part, type, uni)
    end
  end

  defp convert_any_split_result(list, type, uni, false) do
    convert_split_result(list, type, uni)
  end

  defp convert_split_result(list, :iodata, _Unicode) do
    list
  end

  defp convert_split_result(list, :binary, _Unicode) do
    list
  end

  defp convert_split_result(list, :list, true) do
    for element <- list do
      :unicode.characters_to_list(element, :unicode)
    end
  end

  defp convert_split_result(list, :list, false) do
    for element <- list do
      :erlang.binary_to_list(element)
    end
  end

  defp do_split(subj, off, _, _, 0, false) do
    <<_::size(off)-binary, rest::binary>> = subj
    [rest]
  end

  defp do_split(subj, off, [], _, _, false) do
    <<_::size(off)-binary, rest::binary>> = subj
    [rest]
  end

  defp do_split(subj, off, _, _, _, false)
       when byte_size(subj) <= off do
    [<<>>]
  end

  defp do_split(subj, off, _, _, 0, true) do
    <<_::size(off)-binary, rest::binary>> = subj
    [[rest]]
  end

  defp do_split(subj, off, [], _, _, true) do
    <<_::size(off)-binary, rest::binary>> = subj
    [[rest]]
  end

  defp do_split(subj, off, _, _, _, true)
       when byte_size(subj) <= off do
    [[<<>>]]
  end

  defp do_split(subj, offset, [[{mainI, mainL} | sub] | t], numSub, limit, group) do
    newOffset = mainI + mainL
    keptLen = mainI - offset

    case {keptLen, empty_sub(sub), mainL} do
      {0, true, 0} ->
        do_split(subj, newOffset, t, numSub, limit, group)

      _ ->
        <<_::size(offset)-binary, keep::size(keptLen)-binary, _::binary>> = subj
        eSub = extend_subpatterns(sub, numSub)
        tail = do_split(subj, newOffset, t, numSub, limit - 1, group)

        case group do
          false ->
            [keep | dig_subpatterns(subj, :lists.reverse(eSub), tail)]

          true ->
            [
              [keep | dig_subpatterns(subj, :lists.reverse(eSub), [])]
              | tail
            ]
        end
    end
  end

  defp empty_sub([]) do
    true
  end

  defp empty_sub([{_, 0} | t]) do
    empty_sub(t)
  end

  defp empty_sub(_) do
    false
  end

  defp dig_subpatterns(_, [], acc) do
    acc
  end

  defp dig_subpatterns(subj, [{-1, 0} | t], acc) do
    dig_subpatterns(subj, t, [<<>> | acc])
  end

  defp dig_subpatterns(subj, [{i, l} | t], acc) do
    <<_::size(i)-binary, part::size(l)-binary, _::binary>> = subj
    dig_subpatterns(subj, t, [part | acc])
  end

  defp extend_subpatterns(_, 0) do
    []
  end

  defp extend_subpatterns([], n) do
    [{0, 0} | extend_subpatterns([], n - 1)]
  end

  defp extend_subpatterns([h | t], n) do
    [h | extend_subpatterns(t, n - 1)]
  end

  defp compile_split({:re_pattern, n, _, _, _} = comp, options) do
    {comp, n, options}
  end

  defp compile_split(pat, options0) when not is_tuple(pat) do
    options =
      :lists.filter(
        fn o ->
          not runopt(o)
        end,
        options0
      )

    case :re.compile(pat, options) do
      {:error, err} ->
        {:error, err}

      {:ok, {:re_pattern, n, _, _, _} = comp} ->
        newOpt =
          :lists.filter(
            fn oO ->
              not copt(oO)
            end,
            options0
          )

        {comp, n, newOpt}
    end
  end

  defp compile_split(_, _) do
    throw(:badre)
  end

  def replace(subject, rE, replacement) do
    replace(subject, rE, replacement, [])
  end

  def replace(subject, rE, replacement, options) do
    try do
      {newOpt, convert} =
        process_repl_params(
          options,
          :iodata
        )

      unicode = check_for_unicode(rE, options)
      flatSubject = to_binary(subject, unicode)
      flatReplacement = to_binary(replacement, unicode)
      ioList = do_replace(flatSubject, subject, rE, flatReplacement, newOpt)

      case convert do
        :iodata ->
          ioList

        :binary ->
          case unicode do
            false ->
              :erlang.iolist_to_binary(ioList)

            true ->
              :unicode.characters_to_binary(ioList, :unicode)
          end

        :list ->
          case unicode do
            false ->
              :erlang.binary_to_list(:erlang.iolist_to_binary(ioList))

            true ->
              :unicode.characters_to_list(ioList, :unicode)
          end
      end
    catch
      :badopt ->
        :erlang.error(
          :badarg,
          [subject, rE, replacement, options]
        )

      :badre ->
        :erlang.error(
          :badarg,
          [subject, rE, replacement, options]
        )

      :error, :badarg ->
        :erlang.error(
          :badarg,
          [subject, rE, replacement, options]
        )
    end
  end

  defp do_replace(flatSubject, subject, rE, replacement, options) do
    case :re.run(flatSubject, rE, options) do
      :nomatch ->
        subject

      {:match, [mlist | t]} when is_list(mlist) ->
        apply_mlist(flatSubject, replacement, [mlist | t])

      {:match, slist} ->
        apply_mlist(flatSubject, replacement, [slist])
    end
  end

  defp process_repl_params([], convert) do
    {[], convert}
  end

  defp process_repl_params([:report_errors | _], _) do
    throw(:badopt)
  end

  defp process_repl_params([{:capture, _, _} | _], _) do
    throw(:badopt)
  end

  defp process_repl_params([{:capture, _} | _], _) do
    throw(:badopt)
  end

  defp process_repl_params([{:return, :iodata} | t], _C) do
    process_repl_params(t, :iodata)
  end

  defp process_repl_params([{:return, :list} | t], _C) do
    process_repl_params(t, :list)
  end

  defp process_repl_params([{:return, :binary} | t], _C) do
    process_repl_params(t, :binary)
  end

  defp process_repl_params([{:return, _} | _], _) do
    throw(:badopt)
  end

  defp process_repl_params([h | t], c) do
    {nT, nC} = process_repl_params(t, c)
    {[h | nT], nC}
  end

  defp process_split_params([], convert, limit, strip, group) do
    {[], convert, limit, strip, group}
  end

  defp process_split_params([:trim | t], c, _L, _S, g) do
    process_split_params(t, c, -1, true, g)
  end

  defp process_split_params([{:parts, 0} | t], c, _L, _S, g) do
    process_split_params(t, c, -1, true, g)
  end

  defp process_split_params([{:parts, n} | t], c, _L, _S, g)
       when is_integer(n) and n >= 1 do
    process_split_params(t, c, n - 1, false, g)
  end

  defp process_split_params([{:parts, :infinity} | t], c, _L, _S, g) do
    process_split_params(t, c, -1, false, g)
  end

  defp process_split_params([{:parts, _} | _], _, _, _, _) do
    throw(:badopt)
  end

  defp process_split_params([:group | t], c, l, s, _G) do
    process_split_params(t, c, l, s, true)
  end

  defp process_split_params([:global | _], _, _, _, _) do
    throw(:badopt)
  end

  defp process_split_params([:report_errors | _], _, _, _, _) do
    throw(:badopt)
  end

  defp process_split_params([{:capture, _, _} | _], _, _, _, _) do
    throw(:badopt)
  end

  defp process_split_params([{:capture, _} | _], _, _, _, _) do
    throw(:badopt)
  end

  defp process_split_params([{:return, :iodata} | t], _C, l, s, g) do
    process_split_params(t, :iodata, l, s, g)
  end

  defp process_split_params([{:return, :list} | t], _C, l, s, g) do
    process_split_params(t, :list, l, s, g)
  end

  defp process_split_params([{:return, :binary} | t], _C, l, s, g) do
    process_split_params(t, :binary, l, s, g)
  end

  defp process_split_params([{:return, _} | _], _, _, _, _) do
    throw(:badopt)
  end

  defp process_split_params([h | t], c, l, s, g) do
    {nT, nC, nL, nS, nG} = process_split_params(t, c, l, s, g)
    {[h | nT], nC, nL, nS, nG}
  end

  defp apply_mlist(subject, replacement, mlist) do
    do_mlist(subject, subject, 0, precomp_repl(replacement), mlist)
  end

  defp precomp_repl(<<>>) do
    []
  end

  defp precomp_repl(<<?\\, ?g, ?{, rest::binary>>)
       when byte_size(rest) > 0 do
    {nS, <<?}, nRest::binary>>} = pick_int(rest)
    [:erlang.list_to_integer(nS) | precomp_repl(nRest)]
  end

  defp precomp_repl(<<?\\, ?g, rest::binary>>)
       when byte_size(rest) > 0 do
    {nS, nRest} = pick_int(rest)
    [:erlang.list_to_integer(nS) | precomp_repl(nRest)]
  end

  defp precomp_repl(<<?\\, x, rest::binary>>)
       when x < ?1 or
              x > ?9 do
    case precomp_repl(rest) do
      [bHead | t0] when is_binary(bHead) ->
        [<<x, bHead::binary>> | t0]

      other ->
        [<<x>> | other]
    end
  end

  defp precomp_repl(<<?\\, rest::binary>>)
       when byte_size(rest) > 0 do
    {nS, nRest} = pick_int(rest)
    [:erlang.list_to_integer(nS) | precomp_repl(nRest)]
  end

  defp precomp_repl(<<?&, rest::binary>>) do
    [0 | precomp_repl(rest)]
  end

  defp precomp_repl(<<x, rest::binary>>) do
    case precomp_repl(rest) do
      [bHead | t0] when is_binary(bHead) ->
        [<<x, bHead::binary>> | t0]

      other ->
        [<<x>> | other]
    end
  end

  defp pick_int(<<x, r::binary>>)
       when x >= ?0 and
              x <= ?9 do
    {found, rest} = pick_int(r)
    {[x | found], rest}
  end

  defp pick_int(bin) do
    {[], bin}
  end

  defp do_mlist(_, <<>>, _, _, []) do
    []
  end

  defp do_mlist(_, subject, _, _, []) do
    subject
  end

  defp do_mlist(whole, subject, pos, repl, [[{mPos, count} | sub] | tail])
       when mPos > pos do
    eatLength = mPos - pos
    <<untouched::size(eatLength)-binary, rest::binary>> = subject
    [untouched | do_mlist(whole, rest, mPos, repl, [[{mPos, count} | sub] | tail])]
  end

  defp do_mlist(whole, subject, pos, repl, [[{mPos, count} | sub] | tail])
       when mPos === pos do
    eatLength = count
    <<_::size(eatLength)-binary, rest::binary>> = subject
    newData = do_replace(whole, repl, [{mPos, count} | sub])
    [newData | do_mlist(whole, rest, pos + eatLength, repl, tail)]
  end

  defp do_replace(_, [bin], _) when is_binary(bin) do
    bin
  end

  defp do_replace(subject, repl, subExprs0) do
    subExprs = :erlang.list_to_tuple(subExprs0)

    for part <- repl do
      case part do
        n when is_integer(n) ->
          cond do
            tuple_size(subExprs) <= n ->
              <<>>

            true ->
              {sPos, sLen} = :erlang.element(n + 1, subExprs)

              cond do
                sPos < 0 ->
                  <<>>

                true ->
                  <<_::size(sPos)-binary, res::size(sLen)-binary, _::binary>> = subject
                  res
              end
          end

        other ->
          other
      end
    end
  end

  defp check_for_unicode({:re_pattern, _, 1, _, _}, _) do
    true
  end

  defp check_for_unicode({:re_pattern, _, 0, _, _}, _) do
    false
  end

  defp check_for_unicode(_, l) do
    :lists.member(:unicode, l)
  end

  defp check_for_crlf({:re_pattern, _, _, 1, _}, _) do
    true
  end

  defp check_for_crlf({:re_pattern, _, _, 0, _}, _) do
    false
  end

  defp check_for_crlf(_, l) do
    case :lists.keysearch(:newline, 1, l) do
      {:value, {:newline, :any}} ->
        true

      {:value, {:newline, :crlf}} ->
        true

      {:value, {:newline, :anycrlf}} ->
        true

      _ ->
        false
    end
  end

  defp process_parameters([], initialOffset, selectReturn, convertReturn, _, _) do
    {[], initialOffset, selectReturn, convertReturn}
  end

  defp process_parameters([{:offset, n} | t], _Init0, select0, return0, cC, rE) do
    process_parameters(t, n, select0, return0, cC, rE)
  end

  defp process_parameters([:global | t], init0, select0, return0, cC, rE) do
    process_parameters(t, init0, select0, return0, cC, rE)
  end

  defp process_parameters([{:capture, values, type} | t], init0, select0, _Return0, cC, rE) do
    process_parameters([{:capture, values} | t], init0, select0, type, cC, rE)
  end

  defp process_parameters([{:capture, values} | t], init0, select0, return0, cC, rE) do
    {newTail, init1, select1, return1} = process_parameters(t, init0, select0, return0, cC, rE)

    case select1 do
      false ->
        case values do
          :all ->
            {[{:capture, :all} | newTail], init1, :all, return0}

          :all_names ->
            case :re.inspect(rE, :namelist) do
              {:namelist, []} ->
                {[{:capture, :first} | newTail], init1, :none, return0}

              {:namelist, list} ->
                {[{:capture, [0 | list]} | newTail], init1, :stripfirst, return0}
            end

          :first ->
            {[{:capture, :first} | newTail], init1, :all, return0}

          :all_but_first ->
            {[{:capture, :all} | newTail], init1, :stripfirst, return0}

          :none ->
            {[{:capture, :first} | newTail], init1, :none, return0}

          [] ->
            {[{:capture, :first} | newTail], init1, :none, return0}

          list when is_list(list) ->
            {[{:capture, [0 | list]} | newTail], init1, :stripfirst, return0}

          _ ->
            throw(:badlist)
        end

      _ ->
        {newTail, init1, select1, return1}
    end
  end

  defp process_parameters([h | t], init0, select0, return0, true, rE) do
    case copt(h) do
      true ->
        process_parameters(t, init0, select0, return0, true, rE)

      false ->
        {newT, init, select, return} = process_parameters(t, init0, select0, return0, true, rE)
        {[h | newT], init, select, return}
    end
  end

  defp process_parameters([h | t], init0, select0, return0, false, rE) do
    {newT, init, select, return} = process_parameters(t, init0, select0, return0, false, rE)
    {[h | newT], init, select, return}
  end

  defp process_parameters(_, _, _, _, _, _) do
    throw(:badlist)
  end

  defp postprocess({:match, []}, _, _, _, _) do
    :nomatch
  end

  defp postprocess({:match, _}, :none, _, _, _) do
    :match
  end

  defp postprocess({:match, m}, any, :binary, flat, uni) do
    binarify(
      postprocess({:match, m}, any, :index, flat, uni),
      flat
    )
  end

  defp postprocess({:match, m}, any, :list, flat, uni) do
    listify(postprocess({:match, m}, any, :index, flat, uni), flat, uni)
  end

  defp postprocess({:match, m}, :all, :index, _, _) do
    {:match, m}
  end

  defp postprocess({:match, m}, false, :index, _, _) do
    {:match, m}
  end

  defp postprocess({:match, m}, :stripfirst, :index, _, _) do
    {:match,
     for [_ | t] <- m do
       t
     end}
  end

  defp binarify({:match, m}, flat) do
    {:match,
     for one <- m do
       for {i, l} <- one do
         case {i, l} do
           {-1, 0} ->
             <<>>

           {sPos, sLen} ->
             <<_::size(sPos)-binary, res::size(sLen)-binary, _::binary>> = flat
             res
         end
       end
     end}
  end

  defp listify({:match, m}, flat, uni) do
    {:match,
     for one <- m do
       for {i, l} <- one do
         case {i, l} do
           {_, 0} ->
             []

           {sPos, sLen} ->
             case uni do
               true ->
                 <<_::size(sPos)-binary, res::size(sLen)-binary, _::binary>> = flat
                 :unicode.characters_to_list(res, :unicode)

               false ->
                 start = sPos + 1
                 end__ = sPos + sLen
                 :erlang.binary_to_list(flat, start, end__)
             end
         end
       end
     end}
  end

  defp ubinarify({:match, m}, flat) do
    {:match,
     for {i, l} <- m do
       case {i, l} do
         {-1, 0} ->
           <<>>

         {sPos, sLen} ->
           <<_::size(sPos)-binary, res::size(sLen)-binary, _::binary>> = flat
           res
       end
     end}
  end

  defp ubinarify(else__, _) do
    else__
  end

  defp ulistify({:match, m}, flat) do
    {:match,
     for {i, l} <- m do
       case {i, l} do
         {_, 0} ->
           []

         {sPos, sLen} ->
           <<_::size(sPos)-binary, res::size(sLen)-binary, _::binary>> = flat
           :unicode.characters_to_list(res, :unicode)
       end
     end}
  end

  defp ulistify(else__, _) do
    else__
  end

  defp process_uparams([:global | _T], _RetType) do
    throw(false)
  end

  defp process_uparams([{:capture, values, type} | t], _OldType) do
    process_uparams([{:capture, values} | t], type)
  end

  defp process_uparams([h | t], type) do
    {nL, nType} = process_uparams(t, type)
    {[h | nL], nType}
  end

  defp process_uparams([], type) do
    {[], type}
  end

  def ucompile(rE, options) do
    try do
      :re.compile(
        :unicode.characters_to_binary(rE, :unicode),
        options
      )
    catch
      :error, anyError ->
        {:EXIT, {:new_stacktrace, [{mod, _, l, loc} | rest]}} =
          try do
            :erlang.error(
              :new_stacktrace,
              [rE, options]
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        :erlang.raise(:error, anyError, [{mod, :compile, l, loc} | rest])
    end
  end

  def urun(subject, rE, options) do
    try do
      urun2(subject, rE, options)
    catch
      :error, anyError ->
        {:EXIT, {:new_stacktrace, [{mod, _, l, loc} | rest]}} =
          try do
            :erlang.error(
              :new_stacktrace,
              [subject, rE, options]
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        :erlang.raise(:error, anyError, [{mod, :run, l, loc} | rest])
    end
  end

  defp urun2(subject0, rE0, options0) do
    {options, retType} =
      case (try do
              process_uparams(options0, :index)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {a, b} ->
          {a, b}

        _ ->
          {options0, false}
      end

    subject =
      :unicode.characters_to_binary(
        subject0,
        :unicode
      )

    rE =
      case rE0 do
        binRE when is_binary(binRE) ->
          binRE

        {:re_pattern, _, _, _, _} = reCompiled ->
          reCompiled

        listRE ->
          :unicode.characters_to_binary(listRE, :unicode)
      end

    ret = :re.run(subject, rE, options)

    case retType do
      :binary ->
        ubinarify(ret, subject)

      :list ->
        ulistify(ret, subject)

      _ ->
        ret
    end
  end

  def grun(subject, rE, {options, needClean}) do
    try do
      grun2(subject, rE, {options, needClean})
    catch
      :error, anyError ->
        {:EXIT, {:new_stacktrace, [{mod, _, l, loc} | rest]}} =
          try do
            :erlang.error(
              :new_stacktrace,
              [subject, rE, options]
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        :erlang.raise(:error, anyError, [{mod, :run, l, loc} | rest])
    end
  end

  def grun(subject, rE, {options, needClean, origRE}) do
    try do
      grun2(subject, rE, {options, needClean})
    catch
      :error, anyError ->
        {:EXIT, {:new_stacktrace, [{mod, _, l, loc} | rest]}} =
          try do
            :erlang.error(
              :new_stacktrace,
              [subject, origRE, options]
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

        :erlang.raise(:error, anyError, [{mod, :run, l, loc} | rest])
    end
  end

  defp grun2(subject, rE, {options, needClean}) do
    unicode = check_for_unicode(rE, options)
    cRLF = check_for_crlf(rE, options)
    flatSubject = to_binary(subject, unicode)
    do_grun(flatSubject, subject, unicode, cRLF, rE, {options, needClean})
  end

  defp do_grun(flatSubject, subject, unicode, cRLF, rE, {options0, needClean}) do
    {strippedOptions, initialOffset, selectReturn, convertReturn} =
      case (try do
              process_parameters(options0, 0, false, :index, needClean, rE)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        :badlist ->
          :erlang.error(:badarg, [subject, rE, options0])

        correctReturn ->
          correctReturn
      end

    try do
      postprocess(
        loopexec(
          flatSubject,
          rE,
          initialOffset,
          byte_size(flatSubject),
          unicode,
          cRLF,
          strippedOptions,
          true
        ),
        selectReturn,
        convertReturn,
        flatSubject,
        unicode
      )
    catch
      errTuple ->
        errTuple
    end
  end

  defp loopexec(_, _, x, y, _, _, _, _) when x > y do
    {:match, []}
  end

  defp loopexec(subject, rE, x, y, unicode, cRLF, options, first) do
    case :re.internal_run(subject, rE, [{:offset, x}] ++ options, first) do
      {:error, err} ->
        throw({:error, err})

      :nomatch ->
        {:match, []}

      {:match, [{a, b} | more]} ->
        {:match, rest} =
          case b > 0 do
            true ->
              loopexec(subject, rE, a + b, y, unicode, cRLF, options, false)

            false ->
              {:match, m} =
                case :re.internal_run(
                       subject,
                       rE,
                       [{:offset, x}, :notempty_atstart, :anchored] ++ options,
                       false
                     ) do
                  :nomatch ->
                    {:match, []}

                  {:match, other} ->
                    {:match, other}
                end

              newA =
                case m do
                  [{_, nStep} | _] when nStep > 0 ->
                    a + nStep

                  _ ->
                    forward(subject, a, 1, unicode, cRLF)
                end

              {:match, mM} = loopexec(subject, rE, newA, y, unicode, cRLF, options, false)

              case m do
                [] ->
                  {:match, mM}

                _ ->
                  {:match, [m | mM]}
              end
          end

        {:match, [[{a, b} | more] | rest]}
    end
  end

  defp forward(_Chal, a, 0, _, _) do
    a
  end

  defp forward(chal, a, n, u, true) do
    <<_::size(a)-binary, tl::binary>> = chal

    case tl do
      <<?\r, ?\n, _::binary>> ->
        forward(chal, a + 2, n - 1, u, true)

      _ ->
        forward2(chal, a, n, u, true)
    end
  end

  defp forward(chal, a, n, u, false) do
    forward2(chal, a, n, u, false)
  end

  defp forward2(chal, a, n, false, cRLF) do
    forward(chal, a + 1, n - 1, false, cRLF)
  end

  defp forward2(chal, a, n, true, cRLF) do
    <<_::size(a)-binary, tl::binary>> = chal

    forw =
      case tl do
        <<1::size(1), 1::size(1), 0::size(1), _::size(5), _::binary>> ->
          2

        <<1::size(1), 1::size(1), 1::size(1), 0::size(1), _::size(4), _::binary>> ->
          3

        <<1::size(1), 1::size(1), 1::size(1), 1::size(1), 0::size(1), _::size(3), _::binary>> ->
          4

        _ ->
          1
      end

    forward(chal, a + forw, n - 1, true, cRLF)
  end

  defp copt(:caseless) do
    true
  end

  defp copt(:no_start_optimize) do
    true
  end

  defp copt(:never_utf) do
    true
  end

  defp copt(:ucp) do
    true
  end

  defp copt(:dollar_endonly) do
    true
  end

  defp copt(:dotall) do
    true
  end

  defp copt(:extended) do
    true
  end

  defp copt(:firstline) do
    true
  end

  defp copt(:multiline) do
    true
  end

  defp copt(:no_auto_capture) do
    true
  end

  defp copt(:dupnames) do
    true
  end

  defp copt(:ungreedy) do
    true
  end

  defp copt(:unicode) do
    true
  end

  defp copt(_) do
    false
  end

  defp runopt(:notempty) do
    true
  end

  defp runopt(:notempty_atstart) do
    true
  end

  defp runopt(:notbol) do
    true
  end

  defp runopt(:noteol) do
    true
  end

  defp runopt({:offset, _}) do
    true
  end

  defp runopt({:capture, _, _}) do
    true
  end

  defp runopt({:capture, _}) do
    true
  end

  defp runopt(:global) do
    true
  end

  defp runopt({:match_limit, _}) do
    true
  end

  defp runopt({:match_limit_recursion, _}) do
    true
  end

  defp runopt(_) do
    false
  end

  defp to_binary(bin, _IsUnicode) when is_binary(bin) do
    bin
  end

  defp to_binary(data, true) do
    :unicode.characters_to_binary(data, :unicode)
  end

  defp to_binary(data, false) do
    :erlang.iolist_to_binary(data)
  end
end
