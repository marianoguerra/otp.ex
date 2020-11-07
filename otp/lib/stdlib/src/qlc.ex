defmodule :m_qlc do
  use Bitwise
  require Record
  Record.defrecord(:r_qlc_append, :qlc_append, hl: :undefined)

  Record.defrecord(:r_qlc_table, :qlc_table,
    trav_fun: :undefined,
    trav_MS: :undefined,
    pre_fun: :undefined,
    post_fun: :undefined,
    info_fun: :undefined,
    format_fun: :undefined,
    lookup_fun: :undefined,
    parent_fun: :undefined,
    key_equality: :undefined,
    lu_vals: :undefined,
    ms: :no_match_spec
  )

  Record.defrecord(:r_qlc_sort, :qlc_sort,
    h: :undefined,
    keypos: :undefined,
    unique: :undefined,
    compressed: :undefined,
    order: :undefined,
    fs_opts: :undefined,
    tmpdir_usage: :allowed,
    tmpdir: :undefined
  )

  Record.defrecord(:r_qlc_lc, :qlc_lc,
    lc: :undefined,
    opt: :undefined
  )

  Record.defrecord(:r_qlc_list, :qlc_list,
    l: :undefined,
    ms: :no_match_spec
  )

  Record.defrecord(:r_qlc_join, :qlc_join,
    kind: :undefined,
    opt: :undefined,
    h1: :undefined,
    q1: :undefined,
    c1: :undefined,
    h2: :undefined,
    q2: :undefined,
    c2: :undefined
  )

  Record.defrecord(:r_qlc_cursor, :qlc_cursor, c: :undefined)

  Record.defrecord(:r_qlc_opt, :qlc_opt,
    unique: false,
    cache: false,
    max_lookup: -1,
    join: :any,
    tmpdir: '',
    lookup: :any,
    max_list: 512 * 1024,
    tmpdir_usage: :allowed
  )

  Record.defrecord(:r_setup, :setup, parent: :undefined)
  Record.defrecord(:r_qlc_handle, :qlc_handle, h: :undefined)

  defp get_handle(r_qlc_handle(h: r_qlc_lc(opt: {:qlc_opt, u, c, m}) = h)) do
    r_qlc_lc(h, opt: r_qlc_opt(unique: u, cache: c, max_lookup: m))
  end

  defp get_handle(r_qlc_handle(h: h)) do
    h
  end

  defp get_handle(l) when is_list(l) do
    l
  end

  defp get_handle(_) do
    :badarg
  end

  def append(qHs) do
    hs =
      for qH <- qHs do
        case get_handle(qH) do
          :badarg ->
            :erlang.error(:badarg, [qHs])

          h ->
            h
        end
      end

    r_qlc_handle(h: r_qlc_append(hl: hs))
  end

  def append(qH1, qH2) do
    hs =
      for qH <- [qH1, qH2] do
        case get_handle(qH) do
          :badarg ->
            :erlang.error(:badarg, [qH1, qH2])

          h ->
            h
        end
      end

    r_qlc_handle(h: r_qlc_append(hl: hs))
  end

  def cursor(qH) do
    cursor(qH, [])
  end

  def cursor(qH, options) do
    case {options(
            options,
            [:unique_all, :cache_all, :tmpdir, :spawn_options, :max_list_size, :tmpdir_usage]
          ), get_handle(qH)} do
      {b1, b2} when b1 === :badarg or b2 === :badarg ->
        :erlang.error(:badarg, [qH, options])

      {[gUnique, gCache, tmpDir, spawnOptions0, maxList, tmpUsage], h} ->
        spawnOptions = spawn_options(spawnOptions0)

        case cursor_process(h, gUnique, gCache, tmpDir, spawnOptions, maxList, tmpUsage) do
          pid when is_pid(pid) ->
            r_qlc_cursor(c: {pid, self()})

          error ->
            error
        end
    end
  end

  def delete_cursor(r_qlc_cursor(c: {_, owner}) = c) when owner !== self() do
    :erlang.error(:not_cursor_owner, [c])
  end

  def delete_cursor(r_qlc_cursor(c: {pid, _})) do
    stop_cursor(pid)
  end

  def delete_cursor(t) do
    :erlang.error(:badarg, [t])
  end

  def e(qH) do
    eval(qH, [])
  end

  def e(qH, options) do
    eval(qH, options)
  end

  def eval(qH) do
    eval(qH, [])
  end

  def eval(qH, options) do
    case {options(
            options,
            [:unique_all, :cache_all, :tmpdir, :max_list_size, :tmpdir_usage]
          ), get_handle(qH)} do
      {b1, b2} when b1 === :badarg or b2 === :badarg ->
        :erlang.error(:badarg, [qH, options])

      {[gUnique, gCache, tmpDir, maxList, tmpUsage], handle} ->
        try do
          prep = prepare_qlc(handle, [], gUnique, gCache, tmpDir, maxList, tmpUsage)

          case setup_qlc(prep, r_setup(parent: self())) do
            {l, post, _LocalPost} when is_list(l) ->
              post_funs(post)
              l

            {objs, post, _LocalPost} when is_function(objs) ->
              try do
                collect(objs)
              after
                post_funs(post)
              end
          end
        catch
          term ->
            case __STACKTRACE__ do
              [{:qlc, :throw_error, _, _} | _] ->
                term

              _ ->
                :erlang.raise(:throw, term, __STACKTRACE__)
            end
        end
    end
  end

  def fold(fun, acc0, qH) do
    fold(fun, acc0, qH, [])
  end

  def fold(fun, acc0, qH, options) do
    case {options(
            options,
            [:unique_all, :cache_all, :tmpdir, :max_list_size, :tmpdir_usage]
          ), get_handle(qH)} do
      {b1, b2} when b1 === :badarg or b2 === :badarg ->
        :erlang.error(:badarg, [fun, acc0, qH, options])

      {[gUnique, gCache, tmpDir, maxList, tmpUsage], handle} ->
        try do
          prep = prepare_qlc(handle, :not_a_list, gUnique, gCache, tmpDir, maxList, tmpUsage)

          case setup_qlc(prep, r_setup(parent: self())) do
            {objs, post, _LocalPost}
            when is_function(objs) or
                   is_list(objs) ->
              try do
                fold_loop(fun, objs, acc0)
              after
                post_funs(post)
              end
          end
        catch
          term ->
            case __STACKTRACE__ do
              [{:qlc, :throw_error, _, _} | _] ->
                term

              _ ->
                :erlang.raise(:throw, term, __STACKTRACE__)
            end
        end
    end
  end

  def format_error(:not_a_query_list_comprehension) do
    :io_lib.format('argument is not a query list comprehension', [])
  end

  def format_error({:used_generator_variable, v}) do
    :io_lib.format('generated variable ~w must not be used in list expression', [v])
  end

  def format_error(:binary_generator) do
    :io_lib.format('cannot handle binary generators', [])
  end

  def format_error(:too_complex_join) do
    :io_lib.format('cannot handle join of three or more generators efficiently', [])
  end

  def format_error(:too_many_joins) do
    :io_lib.format('cannot handle more than one join efficiently', [])
  end

  def format_error(:nomatch_pattern) do
    :io_lib.format('pattern cannot possibly match', [])
  end

  def format_error(:nomatch_filter) do
    :io_lib.format('filter evaluates to \'false\'', [])
  end

  def format_error({line, mod, reason}) when is_integer(line) do
    :io_lib.format(
      '~p: ~ts~n',
      [line, :lists.flatten(mod.format_error(reason))]
    )
  end

  def format_error({:bad_object, fileName}) do
    :io_lib.format('the temporary file "~ts" holding answers is corrupt', [fileName])
  end

  def format_error(:bad_object) do
    :io_lib.format('the keys could not be extracted from some term', [])
  end

  def format_error({:file_error, fileName, reason}) do
    :io_lib.format(
      '"~ts": ~tp~n',
      [fileName, :file.format_error(reason)]
    )
  end

  def format_error({:premature_eof, fileName}) do
    :io_lib.format('"~ts": end-of-file was encountered inside some binary term', [fileName])
  end

  def format_error({:tmpdir_usage, why}) do
    :io_lib.format('temporary file was needed for ~w~n', [why])
  end

  def format_error({:error, module, reason}) do
    module.format_error(reason)
  end

  def format_error(e) do
    :io_lib.format('~tp~n', [e])
  end

  def info(qH) do
    info(qH, [])
  end

  def info(qH, options) do
    case {options(
            options,
            [
              :unique_all,
              :cache_all,
              :flat,
              :format,
              :n_elements,
              :depth,
              :tmpdir,
              :max_list_size,
              :tmpdir_usage
            ]
          ), get_handle(qH)} do
      {b1, b2} when b1 === :badarg or b2 === :badarg ->
        :erlang.error(:badarg, [qH, options])

      {[gUnique, gCache, flat, format, nElements, depth, tmpDir, maxList, tmpUsage], h} ->
        try do
          prep = prepare_qlc(h, [], gUnique, gCache, tmpDir, maxList, tmpUsage)
          info = le_info(prep, {nElements, depth})
          abstractCode = abstract(info, flat, nElements, depth)

          case format do
            :abstract_code ->
              abstract_code(abstractCode)

            :string ->
              hook = fn {:special, _Line, string}, _I, _P, _F ->
                string
              end

              :lists.flatten(:erl_pp.expr(abstractCode, 0, hook))

            :debug ->
              info
          end
        catch
          term ->
            case __STACKTRACE__ do
              [{:qlc, :throw_error, _, _} | _] ->
                term

              _ ->
                :erlang.raise(:throw, term, __STACKTRACE__)
            end
        end
    end
  end

  def keysort(keyPos, qH) do
    keysort(keyPos, qH, [])
  end

  def keysort(keyPos, qH, options) do
    case {is_keypos(keyPos),
          options(
            options,
            [:tmpdir, :order, :unique, :compressed, :size, :no_files]
          ), get_handle(qH)} do
      {true, [[tmpDir, order, unique, compressed] | _], h}
      when h !== :badarg ->
        r_qlc_handle(
          h:
            r_qlc_sort(
              h: h,
              keypos: {:keysort, keyPos},
              unique: unique,
              compressed: compressed,
              order: order,
              fs_opts: listify(options),
              tmpdir: tmpDir
            )
        )

      _ ->
        :erlang.error(:badarg, [keyPos, qH, options])
    end
  end

  def next_answers(c) do
    next_answers(c, 10)
  end

  def next_answers(r_qlc_cursor(c: {_, owner}) = c, numOfAnswers)
      when owner !== self() do
    :erlang.error(:not_cursor_owner, [c, numOfAnswers])
  end

  def next_answers(r_qlc_cursor(c: {pid, _}) = c, numOfAnswers) do
    n =
      case numOfAnswers do
        :all_remaining ->
          -1

        _ when is_integer(numOfAnswers) and numOfAnswers > 0 ->
          numOfAnswers

        _ ->
          :erlang.error(:badarg, [c, numOfAnswers])
      end

    next_loop(pid, [], n)
  end

  def next_answers(t1, t2) do
    :erlang.error(:badarg, [t1, t2])
  end

  def parse_transform(forms, options) do
    :qlc_pt.parse_transform(forms, options)
  end

  def q(qLC_lc) do
    q(qLC_lc, [])
  end

  def q(r_qlc_lc() = qLC_lc, options) do
    case options(
           options,
           [:unique, :cache, :max_lookup, :join, :lookup]
         ) do
      [unique, cache, max, join, lookup] ->
        opt = r_qlc_opt(unique: unique, cache: cache, max_lookup: max, join: join, lookup: lookup)
        r_qlc_handle(h: r_qlc_lc(qLC_lc, opt: opt))

      _ ->
        :erlang.error(:badarg, [qLC_lc, options])
    end
  end

  def q(t1, t2) do
    :erlang.error(:badarg, [t1, t2])
  end

  def sort(qH) do
    sort(qH, [])
  end

  def sort(qH, options) do
    case {options(
            options,
            [:tmpdir, :order, :unique, :compressed, :size, :no_files]
          ), get_handle(qH)} do
      {b1, b2} when b1 === :badarg or b2 === :badarg ->
        :erlang.error(:badarg, [qH, options])

      {[[tD, order, unique, compressed] | _], h} ->
        r_qlc_handle(
          h:
            r_qlc_sort(
              h: h,
              keypos: :sort,
              unique: unique,
              compressed: compressed,
              order: order,
              fs_opts: listify(options),
              tmpdir: tD
            )
        )
    end
  end

  def string_to_handle(str) do
    string_to_handle(str, [])
  end

  def string_to_handle(str, options) do
    string_to_handle(str, options, :erl_eval.new_bindings())
  end

  def string_to_handle(str, options, bindings) when is_list(str) do
    case options(
           options,
           [:unique, :cache, :max_lookup, :join, :lookup]
         ) do
      :badarg ->
        :erlang.error(:badarg, [str, options, bindings])

      [unique, cache, maxLookup, join, lookup] ->
        case :erl_scan.string(str, 1, [:text]) do
          {:ok, tokens, _} ->
            scanRes =
              case :erl_eval.extended_parse_exprs(tokens) do
                {:ok, [expr0], sBs} ->
                  {:ok, expr0, sBs}

                {:ok, _ExprList, _SBs} ->
                  :erlang.error(:badarg, [str, options, bindings])

                e ->
                  e
              end

            case scanRes do
              {:ok, expr, xBs} ->
                bs1 = merge_binding_structs(bindings, xBs)

                case :qlc_pt.transform_expression(expr, bs1) do
                  {:ok, {:call, _, _QlcQ, handle}} ->
                    {:value, qLC_lc, _} = :erl_eval.exprs(handle, bs1)

                    o =
                      r_qlc_opt(
                        unique: unique,
                        cache: cache,
                        max_lookup: maxLookup,
                        join: join,
                        lookup: lookup
                      )

                    r_qlc_handle(h: r_qlc_lc(qLC_lc, opt: o))

                  {:not_ok, [{:error, error} | _]} ->
                    :erlang.error(error)
                end

              {:error, errorInfo} ->
                :erlang.error(errorInfo)
            end

          {:error, errorInfo, _EndLine} ->
            :erlang.error(errorInfo)
        end
    end
  end

  def string_to_handle(t1, t2, t3) do
    :erlang.error(:badarg, [t1, t2, t3])
  end

  def table(traverseFun, options)
      when is_function(traverseFun) do
    case {is_function(traverseFun, 0), isFun1 = is_function(traverseFun, 1)} do
      {false, false} ->
        :erlang.error(:badarg, [traverseFun, options])

      _ ->
        case options(
               options,
               [
                 :pre_fun,
                 :post_fun,
                 :info_fun,
                 :format_fun,
                 :lookup_fun,
                 :parent_fun,
                 :key_equality
               ]
             ) do
          [preFun, postFun, infoFun, formatFun, lookupFun, parentFun, keyEquality] ->
            t =
              r_qlc_table(
                trav_fun: traverseFun,
                pre_fun: preFun,
                post_fun: postFun,
                info_fun: infoFun,
                parent_fun: parentFun,
                trav_MS: isFun1,
                format_fun: formatFun,
                lookup_fun: lookupFun,
                key_equality: keyEquality
              )

            r_qlc_handle(h: t)

          :badarg ->
            :erlang.error(:badarg, [traverseFun, options])
        end
    end
  end

  def table(t1, t2) do
    :erlang.error(:badarg, [t1, t2])
  end

  def transform_from_evaluator(lC, bs0) do
    :qlc_pt.transform_from_evaluator(lC, bs0)
  end

  def template_state() do
    1
  end

  def aux_name(name, n, allNames) do
    {vN, _} = aux_name1(name, n, allNames)
    vN
  end

  def name_suffix(a, suff) do
    :erlang.list_to_atom(:lists.concat([a, suff]))
  end

  def vars(e) do
    var_ufold(
      fn {:var, _L, v} ->
        v
      end,
      e
    )
  end

  def var_ufold(f, e) do
    :ordsets.from_list(var_fold(f, [], e))
  end

  def all_selections([]) do
    [[]]
  end

  def all_selections([{i, cs} | iCs]) do
    for c <- cs, l <- all_selections(iCs) do
      [{i, c} | l]
    end
  end

  defp merge_binding_structs(bs1, bs2) do
    :lists.foldl(
      fn {n, v}, bs ->
        :erl_eval.add_binding(n, v, bs)
      end,
      bs1,
      :erl_eval.bindings(bs2)
    )
  end

  defp aux_name1(name, n, allNames) do
    sN = name_suffix(name, n)

    case :gb_sets.is_member(sN, allNames) do
      true ->
        aux_name1(name, n + 1, allNames)

      false ->
        {sN, n}
    end
  end

  def var_fold(f, a, {:var, _, v} = var) when v !== :_ do
    [f.(var) | a]
  end

  def var_fold(f, a, t) when is_tuple(t) do
    var_fold(f, a, :erlang.tuple_to_list(t))
  end

  def var_fold(f, a, [e | es]) do
    var_fold(f, var_fold(f, a, e), es)
  end

  def var_fold(_F, a, _T) do
    a
  end

  defp options(options, keys) when is_list(options) do
    options(options, keys, [])
  end

  defp options(option, keys) do
    options([option], keys, [])
  end

  defp options(options0, [key | keys], l)
       when is_list(options0) do
    options =
      case :lists.member(key, options0) do
        true ->
          [atom_option(key) | :lists.delete(key, options0)]

        false ->
          options0
      end

    v =
      case :lists.keyfind(key, 1, options) do
        {:format_fun, u = :undefined} ->
          {:ok, u}

        {:info_fun, u = :undefined} ->
          {:ok, u}

        {:lookup_fun, u = :undefined} ->
          {:ok, u}

        {:parent_fun, u = :undefined} ->
          {:ok, u}

        {:post_fun, u = :undefined} ->
          {:ok, u}

        {:pre_fun, u = :undefined} ->
          {:ok, u}

        {:info_fun, fun} when is_function(fun, 1) ->
          {:ok, fun}

        {:pre_fun, fun} when is_function(fun, 1) ->
          {:ok, fun}

        {:post_fun, fun} when is_function(fun, 0) ->
          {:ok, fun}

        {:lookup_fun, fun} when is_function(fun, 2) ->
          {:ok, fun}

        {:max_lookup, max} when is_integer(max) and max >= 0 ->
          {:ok, max}

        {:max_lookup, :infinity} ->
          {:ok, -1}

        {:format_fun, fun} when is_function(fun, 1) ->
          {:ok, fun}

        {:parent_fun, fun} when is_function(fun, 0) ->
          {:ok, fun}

        {:key_equality, kE = :==} ->
          {:ok, kE}

        {:key_equality, kE = :"=:="} ->
          {:ok, kE}

        {:join, j = :any} ->
          {:ok, j}

        {:join, j = :nested_loop} ->
          {:ok, j}

        {:join, j = :merge} ->
          {:ok, j}

        {:join, j = :lookup} ->
          {:ok, j}

        {:lookup, lookUp}
        when is_boolean(lookUp) or
               lookUp === :any ->
          {:ok, lookUp}

        {:max_list_size, max}
        when is_integer(max) and
               max >= 0 ->
          {:ok, max}

        {:tmpdir_usage, tmpUsage}
        when tmpUsage === :allowed or
               tmpUsage === :not_allowed or
               tmpUsage === :info_msg or
               tmpUsage === :warning_msg or
               tmpUsage === :error_msg ->
          {:ok, tmpUsage}

        {:unique, unique} when is_boolean(unique) ->
          {:ok, unique}

        {:cache, cache}
        when is_boolean(cache) or
               cache === :list ->
          {:ok, cache}

        {:cache, :ets} ->
          {:ok, true}

        {:cache, :no} ->
          {:ok, false}

        {:unique_all, uniqueAll} when is_boolean(uniqueAll) ->
          {:ok, uniqueAll}

        {:cache_all, cacheAll}
        when is_boolean(cacheAll) or
               cacheAll === :list ->
          {:ok, cacheAll}

        {:cache_all, :ets} ->
          {:ok, true}

        {:cache_all, :no} ->
          {:ok, false}

        {:spawn_options, :default} ->
          {:ok, :default}

        {:spawn_options, spawnOptions} ->
          case is_proper_list(spawnOptions) do
            true ->
              {:ok, spawnOptions}

            false ->
              :badarg
          end

        {:flat, flat} when is_boolean(flat) ->
          {:ok, flat}

        {:format, format}
        when format === :string or
               format === :abstract_code or
               format === :debug ->
          {:ok, format}

        {:n_elements, nElements}
        when nElements === :infinity or
               (is_integer(nElements) and
                  nElements > 0) ->
          {:ok, nElements}

        {:depth, depth}
        when depth === :infinity or
               (is_integer(depth) and depth >= 0) ->
          {:ok, depth}

        {:order, order}
        when is_function(order, 2) or
               order === :ascending or order === :descending ->
          {:ok, order}

        {:compressed, comp} when comp ->
          {:ok, [:compressed]}

        {:compressed, comp} when not comp ->
          {:ok, []}

        {:tmpdir, t} ->
          {:ok, t}

        {:size, size} when is_integer(size) and size > 0 ->
          {:ok, size}

        {:no_files, noFiles}
        when is_integer(noFiles) and
               noFiles > 1 ->
          {:ok, noFiles}

        {^key, _} ->
          :badarg

        false ->
          default = default_option(key)
          {:ok, default}
      end

    case v do
      :badarg ->
        :badarg

      {:ok, value} ->
        newOptions = :lists.keydelete(key, 1, options)
        options(newOptions, keys, [value | l])
    end
  end

  defp options([], [], l) do
    :lists.reverse(l)
  end

  defp options(_Options, _, _L) do
    :badarg
  end

  defp default_option(:pre_fun) do
    :undefined
  end

  defp default_option(:post_fun) do
    :undefined
  end

  defp default_option(:info_fun) do
    :undefined
  end

  defp default_option(:format_fun) do
    :undefined
  end

  defp default_option(:lookup_fun) do
    :undefined
  end

  defp default_option(:max_lookup) do
    -1
  end

  defp default_option(:join) do
    :any
  end

  defp default_option(:lookup) do
    :any
  end

  defp default_option(:parent_fun) do
    :undefined
  end

  defp default_option(:key_equality) do
    :"=:="
  end

  defp default_option(:spawn_options) do
    :default
  end

  defp default_option(:flat) do
    true
  end

  defp default_option(:format) do
    :string
  end

  defp default_option(:n_elements) do
    :infinity
  end

  defp default_option(:depth) do
    :infinity
  end

  defp default_option(:max_list_size) do
    512 * 1024
  end

  defp default_option(:tmpdir_usage) do
    :allowed
  end

  defp default_option(:cache) do
    false
  end

  defp default_option(:cache_all) do
    false
  end

  defp default_option(:unique) do
    false
  end

  defp default_option(:unique_all) do
    false
  end

  defp default_option(:order) do
    :ascending
  end

  defp default_option(:compressed) do
    []
  end

  defp default_option(:tmpdir) do
    ''
  end

  defp default_option(:size) do
    524_288
  end

  defp default_option(:no_files) do
    16
  end

  defp atom_option(:cache) do
    {:cache, true}
  end

  defp atom_option(:unique) do
    {:unique, true}
  end

  defp atom_option(:cache_all) do
    {:cache_all, true}
  end

  defp atom_option(:unique_all) do
    {:unique_all, true}
  end

  defp atom_option(:lookup) do
    {:lookup, true}
  end

  defp atom_option(:flat) do
    {:flat, true}
  end

  defp atom_option(key) do
    key
  end

  defp is_proper_list([_ | l]) do
    is_proper_list(l)
  end

  defp is_proper_list(l) do
    l === []
  end

  defp spawn_options(:default) do
    [:link]
  end

  defp spawn_options(spawnOptions) do
    :lists.delete(
      :monitor,
      case :lists.member(:link, spawnOptions) do
        true ->
          spawnOptions

        false ->
          [:link | spawnOptions]
      end
    )
  end

  defp is_keypos(keypos)
       when is_integer(keypos) and
              keypos > 0 do
    true
  end

  defp is_keypos([]) do
    false
  end

  defp is_keypos(l) do
    is_keyposs(l)
  end

  defp is_keyposs([kp | kps]) when is_integer(kp) and kp > 0 do
    is_keyposs(kps)
  end

  defp is_keyposs(kps) do
    kps === []
  end

  defp listify(l) when is_list(l) do
    l
  end

  defp listify(t) do
    [t]
  end

  Record.defrecord(:r_optz, :optz,
    unique: false,
    cache: false,
    join_option: :any,
    fast_join: :no,
    opt: :undefined
  )

  Record.defrecord(:r_qlc, :qlc,
    lcf: :undefined,
    codef: :undefined,
    qdata: :undefined,
    init_value: :undefined,
    optz: :undefined
  )

  Record.defrecord(:r_simple_qlc, :simple_qlc,
    p: :undefined,
    le: :undefined,
    line: :undefined,
    init_value: :undefined,
    optz: :undefined
  )

  Record.defrecord(:r_prepared, :prepared,
    qh: :undefined,
    sorted: :no,
    sort_info: [],
    sort_info2: [],
    lu_skip_quals: [],
    join: {[], []},
    n_objs: :undefined,
    is_unique_objects: false,
    is_cached: false
  )

  defp cursor_process(h, gUnique, gCache, tmpDir, spawnOptions, maxList, tmpUsage) do
    parent = self()
    setup = r_setup(parent: parent)

    cF = fn ->
      :erlang.process_flag(:trap_exit, true)
      monRef = :erlang.monitor(:process, parent)

      {objs, post, _LocalPost} =
        try do
          prep = prepare_qlc(h, :not_a_list, gUnique, gCache, tmpDir, maxList, tmpUsage)
          setup_qlc(prep, setup)
        catch
          class, reason ->
            send(parent, {self(), {:caught, class, reason, __STACKTRACE__}})
            exit(:normal)
        end

      send(parent, {self(), :ok})
      wait_for_request(parent, monRef, post)
      reply(parent, monRef, post, objs)
    end

    pid = :erlang.spawn_opt(cF, spawnOptions)
    parent_fun(pid, parent)
  end

  defp parent_fun(pid, parent) do
    receive do
      {^pid, :ok} ->
        pid

      {tPid, {:parent_fun, fun}} ->
        v =
          try do
            {:value, fun.()}
          catch
            class, reason ->
              {:parent_fun_caught, class, reason, __STACKTRACE__}
          end

        send(tPid, {parent, v})
        parent_fun(pid, parent)

      {^pid, {:caught, :throw, error, [{:qlc, :throw_error, _, _} | _]}} ->
        error

      {^pid, {:caught, class, reason, stacktrace}} ->
        :erlang.raise(class, reason, stacktrace)
    end
  end

  defp reply(parent, monRef, post, []) do
    no_more(parent, monRef, post)
  end

  defp reply(parent, monRef, post, [answer | cont]) do
    send(parent, {self(), {:answer, answer}})
    wait_for_request(parent, monRef, post)
    reply(parent, monRef, post, cont)
  end

  defp reply(parent, monRef, post, cont) do
    reply =
      try do
        cond do
          is_function(cont) ->
            cont.()

          true ->
            throw_error(cont)
        end
      catch
        class, reason ->
          post_funs(post)
          message = {:caught, class, reason, __STACKTRACE__}
          send(parent, {self(), message})
          exit(:normal)
      end

    reply(parent, monRef, post, reply)
  end

  defp no_more(parent, monRef, post) do
    send(parent, {self(), :no_more})
    wait_for_request(parent, monRef, post)
    no_more(parent, monRef, post)
  end

  defp wait_for_request(parent, monRef, post) do
    receive do
      {^parent, :stop} ->
        post_funs(post)
        exit(:normal)

      {^parent, :more} ->
        :ok

      {:EXIT, ^parent, _Reason} ->
        post_funs(post)
        exit(:normal)

      {:DOWN, ^monRef, :process, ^parent, _Info} ->
        post_funs(post)
        exit(:normal)

      {:EXIT, pid, _Reason} when pid === self() ->
        wait_for_request(parent, monRef, post)

      other ->
        :error_logger.error_msg('The qlc cursor ~w received an unexpected message:\n~tp\n', [
          self(),
          other
        ])

        wait_for_request(parent, monRef, post)
    end
  end

  defp abstract_code({:special, line, string}) do
    {:string, line, string}
  end

  defp abstract_code(tuple) when is_tuple(tuple) do
    :erlang.list_to_tuple(abstract_code(:erlang.tuple_to_list(tuple)))
  end

  defp abstract_code([h | t]) do
    [abstract_code(h) | abstract_code(t)]
  end

  defp abstract_code(term) do
    term
  end

  defp abstract(info, false = _Flat, nElements, depth) do
    abstract(info, nElements, depth)
  end

  defp abstract(info, true = _Flat, nElements, depth) do
    abstract = abstract(info, nElements, depth)
    vars = abstract_vars(abstract)
    {_, body0, expr} = flatten_abstr(abstract, 1, vars, [])

    case body0 do
      [] ->
        expr

      [{:match, _, ^expr, q}] ->
        q

      [{:match, _, ^expr, q} | body] ->
        {:block, anno0(), :lists.reverse(body, [q])}

      _ ->
        {:block, anno0(), :lists.reverse(body0, [expr])}
    end
  end

  defp abstract(info, nElements, depth) do
    abstract1(info, nElements, depth, anno1())
  end

  defp abstract1({:qlc, e0, qs0, opt}, nElements, depth, a) do
    qs =
      :lists.map(
        fn
          {:generate, p, lE} ->
            {:generate, a, :erlang.binary_to_term(p), abstract1(lE, nElements, depth, a)}

          f ->
            :erlang.binary_to_term(f)
        end,
        qs0
      )

    e = :erlang.binary_to_term(e0)

    os =
      case opt do
        [] ->
          []

        _ ->
          [abstract_term(opt, 1)]
      end

    {:call, a, {:remote, a, {:atom, a, :qlc}, {:atom, a, :q}}, [{:lc, a, e, qs} | os]}
  end

  defp abstract1({:table, {m, f, as0}}, _NElements, _Depth, anno)
       when is_atom(m) and is_atom(f) and is_list(as0) do
    as =
      for a <- as0 do
        abstract_term(a, 1)
      end

    {:call, anno, {:remote, anno, {:atom, anno, m}, {:atom, anno, f}}, as}
  end

  defp abstract1({:table, tableDesc}, _NElements, _Depth, _A) do
    case :io_lib.deep_char_list(tableDesc) do
      true ->
        {:ok, tokens, _} = :erl_scan.string(:lists.flatten(tableDesc ++ '.'), 1, [:text])
        {:ok, es, bs} = :erl_eval.extended_parse_exprs(tokens)
        [expr] = :erl_eval.subst_values_for_vars(es, bs)
        special(expr)

      false ->
        tableDesc
    end
  end

  defp abstract1({:append, infos}, nElements, depth, a) do
    as =
      :lists.foldr(
        fn info, as0 ->
          {:cons, a, abstract1(info, nElements, depth, a), as0}
        end,
        {nil, a},
        infos
      )

    {:call, a, {:remote, a, {:atom, a, :qlc}, {:atom, a, :append}}, [as]}
  end

  defp abstract1({:sort, info, sortOptions}, nElements, depth, a) do
    {:call, a, {:remote, a, {:atom, a, :qlc}, {:atom, a, :sort}},
     [abstract1(info, nElements, depth, a), abstract_term(sortOptions, 1)]}
  end

  defp abstract1({:keysort, info, kp, sortOptions}, nElements, depth, a) do
    {:call, a, {:remote, a, {:atom, a, :qlc}, {:atom, a, :keysort}},
     [abstract_term(kp, 1), abstract1(info, nElements, depth, a), abstract_term(sortOptions, 1)]}
  end

  defp abstract1({:list, l, mS}, nElements, depth, a) do
    {:call, a, {:remote, a, {:atom, a, :ets}, {:atom, a, :match_spec_run}},
     [
       abstract1(l, nElements, depth, a),
       {:call, a, {:remote, a, {:atom, a, :ets}, {:atom, a, :match_spec_compile}},
        [abstract_term(depth(mS, depth), 1)]}
     ]}
  end

  defp abstract1({:list, l}, nElements, depth, _A)
       when nElements === :infinity or
              nElements >= length(l) do
    abstract_term(depth(l, depth), 1)
  end

  defp abstract1({:list, l}, nElements, depth, _A) do
    abstract_term(
      depth(
        :lists.sublist(l, nElements),
        depth
      ) ++ :...,
      1
    )
  end

  defp special({:value, _, thing}) do
    abstract_term(thing)
  end

  defp special(tuple) when is_tuple(tuple) do
    :erlang.list_to_tuple(special(:erlang.tuple_to_list(tuple)))
  end

  defp special([e | es]) do
    [special(e) | special(es)]
  end

  defp special(expr) do
    expr
  end

  defp depth(list, :infinity) do
    list
  end

  defp depth(list, depth) do
    for e <- list do
      depth1(e, depth)
    end
  end

  defp depth_fun(:infinity = _Depth) do
    fn e ->
      e
    end
  end

  defp depth_fun(depth) do
    fn e ->
      depth1(e, depth)
    end
  end

  defp depth1([] = l, _D) do
    l
  end

  defp depth1(_Term, 0) do
    :...
  end

  defp depth1(tuple, d) when is_tuple(tuple) do
    depth_tuple(tuple, tuple_size(tuple), 1, d - 1, [])
  end

  defp depth1(list, d) when is_list(list) do
    cond do
      d === 1 ->
        [:...]

      true ->
        depth_list(list, d - 1)
    end
  end

  defp depth1(binary, d) when byte_size(binary) > d - 1 do
    d1 = d - 1
    <<bin::size(d1)-bytes, _::bytes>> = binary
    <<bin::bytes, "...">>
  end

  defp depth1(t, _Depth) do
    t
  end

  defp depth_list([] = l, _D) do
    l
  end

  defp depth_list(_L, 0) do
    :...
  end

  defp depth_list([e | es], d) do
    [depth1(e, d) | depth_list(es, d - 1)]
  end

  defp depth_tuple(_Tuple, sz, i, _D, l) when i > sz do
    :erlang.list_to_tuple(:lists.reverse(l))
  end

  defp depth_tuple(_L, _Sz, _I, 0, l) do
    :erlang.list_to_tuple(:lists.reverse(l, [:...]))
  end

  defp depth_tuple(tuple, sz, i, d, l) do
    e = depth1(:erlang.element(i, tuple), d)
    depth_tuple(tuple, sz, i + 1, d - 1, [e | l])
  end

  defp abstract_term(term) do
    abstract_term(term, 0)
  end

  defp abstract_term(term, line) do
    abstr_term(term, anno(line))
  end

  defp abstr_term(tuple, line) when is_tuple(tuple) do
    {:tuple, line,
     for e <- :erlang.tuple_to_list(tuple) do
       abstr_term(e, line)
     end}
  end

  defp abstr_term([_ | _] = l, line) do
    case :io_lib.char_list(l) do
      true ->
        :erl_parse.abstract(l, :erl_anno.line(line))

      false ->
        abstr_list(l, line)
    end
  end

  defp abstr_term(fun, line) when is_function(fun) do
    case :erl_eval.fun_data(fun) do
      {:fun_data, _Bs, cs} ->
        {:fun, line, {:clauses, cs}}

      {:named_fun_data, _Bs, name, cs} ->
        {:named_fun, line, name, cs}

      false ->
        {:name, name} = :erlang.fun_info(fun, :name)
        {:arity, arity} = :erlang.fun_info(fun, :arity)

        case :erlang.fun_info(fun, :type) do
          {:type, :external} ->
            {:module, module} = :erlang.fun_info(fun, :module)

            {:fun, line,
             {:function, {:atom, line, module}, {:atom, line, name}, {:integer, line, arity}}}

          {:type, :local} ->
            {:fun, line, {:function, name, arity}}
        end
    end
  end

  defp abstr_term(pPR, line)
       when is_pid(pPR) or is_port(pPR) or
              is_reference(pPR) do
    {:special, line, :lists.flatten(:io_lib.write(pPR))}
  end

  defp abstr_term(map, line) when is_map(map) do
    {:map, line,
     for {k, v} <- :maps.to_list(map) do
       {:map_field_assoc, line, abstr_term(k, line), abstr_term(v, line)}
     end}
  end

  defp abstr_term(simple, line) do
    :erl_parse.abstract(simple, :erl_anno.line(line))
  end

  defp abstr_list([h | t], line) do
    {:cons, line, abstr_term(h, line), abstr_list(t, line)}
  end

  defp abstr_list(t, line) do
    abstr_term(t, line)
  end

  defp flatten_abstr(
         {:call, l1, {:remote, l2, {:atom, l3, :qlc}, {:atom, l4, :q}}, [lC0 | os]},
         vN0,
         vars,
         body0
       ) do
    {:lc, l, e, qs0} = lC0

    f = fn
      {:generate, ln, p, lE0}, {vN1, body1} ->
        {vN2, body2, lE} = flatten_abstr(lE0, vN1, vars, body1)
        {{:generate, ln, p, lE}, {vN2, body2}}

      fil, vN_Body ->
        {fil, vN_Body}
    end

    {qs, {vN3, body}} = :lists.mapfoldl(f, {vN0, body0}, qs0)
    lC = {:lc, l, e, qs}
    {v, vN} = aux_name1(:V, vN3, vars)
    var = {:var, l1, v}
    qLC = {:call, l1, {:remote, l2, {:atom, l3, :qlc}, {:atom, l4, :q}}, [lC | os]}
    {vN + 1, [{:match, l1, var, qLC} | body], var}
  end

  defp flatten_abstr(t0, vN0, vars, body0) when is_tuple(t0) do
    {vN, body, l} = flatten_abstr(:erlang.tuple_to_list(t0), vN0, vars, body0)
    {vN, body, :erlang.list_to_tuple(l)}
  end

  defp flatten_abstr([e0 | es0], vN0, vars, body0) do
    {vN1, body1, e} = flatten_abstr(e0, vN0, vars, body0)
    {vN, body, es} = flatten_abstr(es0, vN1, vars, body1)
    {vN, body, [e | es]}
  end

  defp flatten_abstr(e, vN, _Vars, body) do
    {vN, body, e}
  end

  defp abstract_vars(abstract) do
    :gb_sets.from_list(:ordsets.to_list(vars(abstract)))
  end

  defp collect([] = l) do
    l
  end

  defp collect([answer | cont]) do
    [answer | collect(cont)]
  end

  defp collect(cont) do
    case cont.() do
      answers when is_list(answers) ->
        collect(answers)

      term ->
        throw_error(term)
    end
  end

  defp fold_loop(fun, [obj | cont], acc) do
    fold_loop(fun, cont, fun.(obj, acc))
  end

  defp fold_loop(_Fun, [], acc) do
    acc
  end

  defp fold_loop(fun, cont, acc) do
    case cont.() do
      objects when is_list(objects) ->
        fold_loop(fun, objects, acc)

      term ->
        term
    end
  end

  defp next_loop(pid, l, n) when n !== 0 do
    case monitor_request(pid, :more) do
      :no_more ->
        :lists.reverse(l)

      {:answer, answer} ->
        next_loop(pid, [answer | l], n - 1)

      {:caught, :throw, error, [{:qlc, :throw_error, _, _} | _]} ->
        error

      {:caught, class, reason, stacktrace} ->
        {:current_stacktrace, currentStacktrace} =
          :erlang.process_info(
            self(),
            :current_stacktrace
          )

        :erlang.raise(class, reason, stacktrace ++ currentStacktrace)

      :error ->
        :erlang.error({:qlc_cursor_pid_no_longer_exists, pid})
    end
  end

  defp next_loop(_Pid, l, _N) do
    :lists.reverse(l)
  end

  defp stop_cursor(pid) do
    :erlang.monitor(:process, pid)
    :erlang.unlink(pid)

    receive do
      {:EXIT, ^pid, _Reason} ->
        receive do
          {:DOWN, _, :process, ^pid, _} ->
            :ok
        end
    after
      0 ->
        send(pid, {self(), :stop})

        receive do
          {:DOWN, _, :process, ^pid, _} ->
            :ok
        end
    end
  end

  defp monitor_request(pid, req) do
    ref = :erlang.monitor(:process, pid)
    send(pid, {self(), req})

    receive do
      {:DOWN, ^ref, :process, ^pid, _Info} ->
        receive do
          {:EXIT, ^pid, _Reason} ->
            :ok
        after
          1 ->
            :ok
        end

        :error

      {:EXIT, ^pid, _Reason} ->
        receive do
          {:DOWN, _, :process, ^pid, _} ->
            :error
        end

      {^pid, reply} ->
        :erlang.demonitor(ref, [:flush])
        reply
    end
  end

  Record.defrecord(:r_join, :join,
    op: :undefined,
    q1: :undefined,
    q2: :undefined,
    wh1: :undefined,
    wh2: :undefined,
    cs_fun: :undefined
  )

  defp le_info(
         r_prepared(qh: r_simple_qlc(le: lE, p: p, line: l, optz: optz)),
         infOpt
       ) do
    qVar = :erlang.term_to_binary({:var, l, p})
    {:qlc, qVar, [{:generate, qVar, le_info(lE, infOpt)}], opt_info(optz)}
  end

  defp le_info(
         r_prepared(qh: r_qlc(codef: codeF, qdata: qdata, optz: optz)),
         infOpt
       ) do
    code = codeF.()
    templateState = template_state()
    e = :erlang.element(templateState, code)
    qualInfo0 = qual_info(qdata, code, infOpt)

    qualInfo1 =
      case r_optz(optz, :fast_join) do
        r_qlc_join() = join ->
          join_info(join, qualInfo0, qdata, code)

        :no ->
          qualInfo0
      end

    qualInfo =
      for i <- qualInfo1, i !== :skip do
        i
      end

    {:qlc, e, qualInfo, opt_info(optz)}
  end

  defp le_info(
         r_prepared(
           qh: r_qlc_table(format_fun: formatFun, trav_MS: travMS, ms: mS, lu_vals: luVals)
         ),
         infOpt
       ) do
    {nElements, depth} = infOpt
    depthFun = depth_fun(depth)

    case luVals do
      _ when formatFun === :undefined ->
        {:table, {:"$MOD", :"$FUN", []}}

      {pos, vals} ->
        formated =
          try do
            formatFun.({:lookup, pos, vals, nElements, depthFun})
          catch
            _, _ ->
              formatFun.({:lookup, pos, vals})
          end

        cond do
          mS === :no_match_spec ->
            {:table, formated}

          true ->
            {:list, {:table, formated}, depth(mS, depth)}
        end

      _ when travMS and is_list(mS) ->
        {:table, formatFun.({:match_spec, depth(mS, depth)})}

      _ when mS === :no_match_spec ->
        try do
          {:table, formatFun.({:all, nElements, depthFun})}
        catch
          _, _ ->
            {:table, formatFun.(:all)}
        end
    end
  end

  defp le_info(r_prepared(qh: r_qlc_append(hl: hL)), infOpt) do
    {:append,
     for h <- hL do
       le_info(h, infOpt)
     end}
  end

  defp le_info(
         r_prepared(qh: r_qlc_sort(h: h, keypos: :sort, fs_opts: sortOptions0, tmpdir: tmpDir)),
         infOpt
       ) do
    sortOptions =
      sort_options_global_tmp(
        sortOptions0,
        tmpDir
      )

    {:sort, le_info(h, infOpt), sortOptions}
  end

  defp le_info(
         r_prepared(
           qh: r_qlc_sort(h: h, keypos: {:keysort, kp}, fs_opts: sortOptions0, tmpdir: tmpDir)
         ),
         infOpt
       ) do
    sortOptions =
      sort_options_global_tmp(
        sortOptions0,
        tmpDir
      )

    {:keysort, le_info(h, infOpt), kp, sortOptions}
  end

  defp le_info(r_prepared(qh: r_qlc_list(l: l, ms: :no_match_spec)), _InfOpt) do
    {:list, l}
  end

  defp le_info(r_prepared(qh: r_qlc_list(l: l, ms: mS)), _InfOpt)
       when is_list(l) do
    {:list, {:list, l}, mS}
  end

  defp le_info(r_prepared(qh: r_qlc_list(l: l, ms: mS)), infOpt) do
    {:list, le_info(l, infOpt), mS}
  end

  defp qual_info([{_QNum, _GoI, -1, :fil} | qdata], code, infOpt) do
    [:skip | qual_info(qdata, code, infOpt)]
  end

  defp qual_info([{qNum, _GoI, _SI, :fil} | qdata], code, infOpt) do
    [:erlang.element(qNum + 1, code) | qual_info(qdata, code, infOpt)]
  end

  defp qual_info([{_QNum, _GoI, _SI, {:gen, r_join()}} | qdata], code, infOpt) do
    [:skip | qual_info(qdata, code, infOpt)]
  end

  defp qual_info([{qNum, _GoI, _SI, {:gen, lE}} | qdata], code, infOpt) do
    [
      {:generate, :erlang.element(qNum + 1, code), le_info(lE, infOpt)}
      | qual_info(qdata, code, infOpt)
    ]
  end

  defp qual_info([], _Code, _InfOpt) do
    []
  end

  defp join_info(join, qInfo, qdata, code) do
    r_qlc_join(kind: kind, q1: qNum1a, c1: c1, q2: qNum2a, c2: c2, opt: opt) = join

    {{jQNum, _, _, _}, rev, qNum1, qNum2, _WH1, _WH2, csFun} =
      find_join_data(qdata, qNum1a, qNum2a)

    {cs1_0, cs2_0, compat} = csFun.()

    [cs1, cs2] =
      case compat do
        [] ->
          for cVs <- [cs1_0, cs2_0] do
            for {c, vs} <- cVs do
              {c,
               for v <- vs do
                 {v, :"=:="}
               end}
            end
          end

        _ ->
          [cs1_0, cs2_0]
      end

    l = anno0()
    g1_0 = {:var, l, :G1}
    g2_0 = {:var, l, :G2}
    jP = :erlang.element(jQNum + 1, code)

    {{i1, g1}, {i2, g2}, qInfoL} =
      case kind do
        {:merge, _} ->
          {jG1, qInfo1} =
            join_merge_info(
              qNum1,
              qInfo,
              code,
              g1_0,
              cs1
            )

          {jG2, qInfo2} =
            join_merge_info(
              qNum2,
              qInfo,
              code,
              g2_0,
              cs2
            )

          {jG1, jG2, qInfo1 ++ qInfo2}

        _ when rev ->
          {jG2, qInfo2} =
            join_merge_info(
              qNum2,
              qInfo,
              code,
              g2_0,
              cs2
            )

          {j1, qInfo1} =
            join_lookup_info(
              qNum1,
              qInfo,
              g1_0
            )

          {{j1, g1_0}, jG2, qInfo2 ++ [qInfo1]}

        _ ->
          {jG1, qInfo1} =
            join_merge_info(
              qNum1,
              qInfo,
              code,
              g1_0,
              cs1
            )

          {j2, qInfo2} =
            join_lookup_info(
              qNum2,
              qInfo,
              g2_0
            )

          {jG1, {j2, g2_0}, qInfo1 ++ [qInfo2]}
      end

    {jOptVal, jOp} = kind2op(kind)
    jOpt = [{:join, jOptVal}] ++ opt_info(join_unique_cache(opt))

    jFil =
      :erlang.term_to_binary(
        {:op, l, jOp, {:call, l, {:atom, l, :element}, [{:integer, l, c1}, g1]},
         {:call, l, {:atom, l, :element}, [{:integer, l, c2}, g2]}}
      )

    p = :erlang.term_to_binary({:cons, l, g1, g2})
    jInfo = {:generate, jP, {:qlc, p, qInfoL ++ [jFil], jOpt}}

    {before, [^i1 | after__]} =
      :lists.split(
        qNum1 - 1,
        qInfo
      )

    before ++ [jInfo] ++ :lists.delete(i2, after__)
  end

  defp kind2op({:merge, _KE}) do
    {:merge, :==}
  end

  defp kind2op({:lookup, kE, _LU_fun}) do
    {:lookup, kE}
  end

  defp join_merge_info(qNum, qInfo, code, g, extraConstants) do
    {:generate, _, lEInfo} = i = :lists.nth(qNum, qInfo)

    p =
      :erlang.binary_to_term(
        :erlang.element(
          qNum + 1,
          code
        )
      )

    case {p, extraConstants} do
      {{:var, _, _}, []} ->
        tP = :erlang.term_to_binary(g)
        i2 = {:generate, tP, lEInfo}
        {{i, g}, [i2]}

      _ ->
        {ePV, m} =
          case p do
            {:var, _, _} ->
              {p, p}

            _ ->
              {pV, _} = aux_name1(:P, 0, abstract_vars(p))
              l = :erl_anno.new(0)
              v = {:var, l, pV}
              {v, {:match, l, v, p}}
          end

        dQP = :erlang.term_to_binary(ePV)
        lEI = {:generate, :erlang.term_to_binary(m), lEInfo}
        tP = :erlang.term_to_binary(g)

        cFs =
          for {col, constOps} <- extraConstants do
            a = anno0()
            call = {:call, a, {:atom, a, :element}, [{:integer, a, col}, ePV]}

            f =
              list2op(
                for {con, op} <- constOps do
                  {:op, a, op, abstract_term(con), call}
                end,
                :or,
                a
              )

            :erlang.term_to_binary(f)
          end

        {{i, g}, [{:generate, tP, {:qlc, dQP, [lEI | cFs], []}}]}
    end
  end

  defp list2op([e], _Op, _Anno) do
    e
  end

  defp list2op([e | es], op, anno) do
    {:op, anno, op, e, list2op(es, op, anno)}
  end

  defp join_lookup_info(qNum, qInfo, g) do
    {:generate, _, lEInfo} = i = :lists.nth(qNum, qInfo)
    tP = :erlang.term_to_binary(g)
    {i, {:generate, tP, lEInfo}}
  end

  defp opt_info(r_optz(unique: unique, cache: cache0, join_option: joinOption)) do
    cache =
      cond do
        cache0 ->
          :ets

        true ->
          cache0
      end

    for {t, v} <- [{:cache, cache}, {:unique, unique}],
        v !== default_option(t) do
      {t, v}
    end ++
      for {t, v} <- [{:join, joinOption}],
          v === :nested_loop do
        {t, v}
      end
  end

  defp prepare_qlc(h, initialValue, gUnique, gCache, tmpDir, maxList, tmpUsage) do
    gOpt =
      r_qlc_opt(
        unique: gUnique,
        cache: gCache,
        tmpdir: tmpDir,
        max_list: maxList,
        tmpdir_usage: tmpUsage
      )

    case opt_le(prep_le(h, gOpt), 1) do
      r_prepared(qh: r_qlc() = qLC) = prep ->
        r_prepared(prep, qh: r_qlc(qLC, init_value: initialValue))

      r_prepared(qh: r_simple_qlc() = simpleQLC) = prep ->
        r_prepared(prep, qh: r_simple_qlc(simpleQLC, init_value: initialValue))

      prep ->
        prep
    end
  end

  defp prep_le(r_qlc_lc(lc: lC_fun, opt: r_qlc_opt() = opt0) = h, gOpt) do
    r_qlc_opt(
      unique: gUnique,
      cache: gCache,
      tmpdir: tmpDir,
      max_list: maxList,
      tmpdir_usage: tmpUsage
    ) = gOpt

    unique = :erlang.or(r_qlc_opt(opt0, :unique), gUnique)

    cache =
      cond do
        not gCache ->
          r_qlc_opt(opt0, :cache)

        true ->
          gCache
      end

    opt =
      r_qlc_opt(opt0,
        unique: unique,
        cache: cache,
        tmpdir: tmpDir,
        max_list: maxList,
        tmpdir_usage: tmpUsage
      )

    prep_qlc_lc(lC_fun.(), opt, gOpt, h)
  end

  defp prep_le(r_qlc_table(info_fun: iF) = t, gOpt) do
    {sortInfo, sorted} = table_sort_info(t)
    isUnique = grd(iF, :is_unique_objects)
    prep = r_prepared(qh: t, sort_info: sortInfo, sorted: sorted, is_unique_objects: isUnique)

    opt =
      cond do
        isUnique or
            (not r_qlc_opt(gOpt, :unique) and
               r_qlc_table(t, :ms) === :no_match_spec) ->
          r_qlc_opt(gOpt, cache: false)

        true ->
          gOpt
      end

    may_create_simple(opt, prep)
  end

  defp prep_le(r_qlc_append(hl: hL), gOpt) do
    case :lists.flatmap(
           fn
             r_prepared(qh: r_qlc_list(l: [])) ->
               []

             r_prepared(qh: r_qlc_append(hl: hL1)) ->
               hL1

             h ->
               [h]
           end,
           for h <- hL do
             prep_le(h, gOpt)
           end
         ) do
      [] = nil__ ->
        short_list(nil__)

      [prep] ->
        prep

      prepL ->
        cache =
          :lists.all(
            fn r_prepared(is_cached: isC) ->
              isC !== false
            end,
            prepL
          )

        prep = r_prepared(qh: r_qlc_append(hl: prepL), is_cached: cache)
        may_create_simple(gOpt, prep)
    end
  end

  defp prep_le(r_qlc_sort(h: h0) = q0, gOpt) do
    q = r_qlc_sort(q0, h: prep_le(h0, gOpt))
    prep_sort(q, gOpt)
  end

  defp prep_le([[_, _] | _] = l, gOpt) do
    prep = r_prepared(qh: r_qlc_list(l: l), is_cached: true)

    opt =
      cond do
        not r_qlc_opt(gOpt, :unique) ->
          r_qlc_opt(gOpt, cache: false)

        true ->
          gOpt
      end

    may_create_simple(opt, prep)
  end

  defp prep_le(l, _GOpt) when is_list(l) do
    short_list(l)
  end

  defp prep_le(t, _GOpt) do
    :erlang.error({:unsupported_qlc_handle, r_qlc_handle(h: t)})
  end

  defp eval_le(lE_fun, gOpt) do
    case lE_fun.() do
      {:error, :qlc, _} = error ->
        throw_error(error)

      r ->
        case get_handle(r) do
          :badarg ->
            :erlang.error(:badarg, [r])

          h ->
            prep_le(h, gOpt)
        end
    end
  end

  defp prep_qlc_lc({:simple_v1, pVar, lE_fun, l}, opt, gOpt, _H) do
    check_lookup_option(opt, false)
    prep_simple_qlc(pVar, anno(l), eval_le(lE_fun, gOpt), opt)
  end

  defp prep_qlc_lc({:qlc_v1, qFun, codeF, qdata0, qOpt}, opt, gOpt, _H) do
    f = fn
      {_QNum, _GoI, _SI, :fil} = qualData, modGens ->
        {qualData, modGens}

      {_QNum, _GoI, _SI, {:gen, r_join()}} = qualData, modGens ->
        {qualData, modGens}

      {qNum, goI, sI, {:gen, lE_fun}}, modGens0 ->
        prep1 = eval_le(lE_fun, gOpt)
        {prep, modGens} = prep_generator(qNum, prep1, qOpt, opt, modGens0)
        {{qNum, goI, sI, {:gen, prep}}, modGens}
    end

    {qdata, modGens} = :lists.mapfoldl(f, [], qdata0)
    someLookUp = :lists.keymember(true, 2, modGens)
    check_lookup_option(opt, someLookUp)

    case modGens do
      [{_QNum, _LookUp, :all, onePrep}] ->
        check_join_option(opt)
        onePrep

      _ ->
        prep0 = prep_qlc(qFun, codeF, qdata, qOpt, opt)

        lU_SkipQuals =
          :lists.flatmap(
            fn {qNum, _LookUp, fs, _Prep} ->
              [{qNum, fs}]
            end,
            modGens
          )

        prep1 = r_prepared(prep0, lu_skip_quals: lU_SkipQuals)
        prep_join(prep1, qOpt, opt)
    end
  end

  defp prep_qlc_lc(_, _Opt, _GOpt, h) do
    :erlang.error({:unsupported_qlc_handle, r_qlc_handle(h: h)})
  end

  defp prep_generator(qNum, prep0, qOpt, opt, modGens) do
    posFun = fn keyEquality ->
      pos_fun(keyEquality, qOpt, qNum)
    end

    mSFs =
      case match_specs(qOpt, qNum) do
        :undefined ->
          {:no_match_spec, []}

        {_, _} = mSFs0 ->
          mSFs0
      end

    r_prepared(qh: lE) = prep0

    case prep_gen(lE, prep0, posFun, mSFs, opt) do
      {:replace, fs, lookUp, prep} ->
        {prep, [{qNum, lookUp, fs, prep} | modGens]}

      {:skip, skipFils, lookUp, prep} ->
        {prep, [{qNum, lookUp, skipFils, prep} | modGens]}

      {:no, _Fs, _LookUp, prep} ->
        {prep, modGens}
    end
  end

  defp pos_fun(:undefined, qOpt, qNum) do
    {:"=:=", constants(qOpt, qNum)}
  end

  defp pos_fun(:"=:=", qOpt, qNum) do
    {:"=:=", constants(qOpt, qNum)}
  end

  defp pos_fun(:==, qOpt, qNum) do
    try do
      {:==, equal_constants(qOpt, qNum)}
    catch
      _, _ ->
        {:"=:=", constants(qOpt, qNum)}
    end
  end

  defp prep_gen(
         r_qlc_table(
           lu_vals: luV0,
           ms: mS0,
           trav_MS: travMS,
           info_fun: iF,
           lookup_fun: lU_fun,
           key_equality: keyEquality
         ) = lE0,
         prep0,
         posFun0,
         {mS, fs},
         opt
       ) do
    posFun = posFun0.(keyEquality)
    {luV, {sTag, skipFils}} = find_const_positions(iF, lU_fun, posFun, opt)
    lU = luV !== false

    cond do
      luV0 !== :undefined or mS0 !== :no_match_spec ->
        {:no, [], false, prep0}

      mS !== :no_match_spec and lU ->
        mS1 =
          cond do
            fs === skipFils or sTag === fs ->
              case mS do
                [{:"$1", _Guard, [:"$1"]}] ->
                  :no_match_spec

                [{head, _Guard, body}] ->
                  [{head, [], body}]
              end

            true ->
              mS
          end

        prep = r_prepared(prep0, qh: r_qlc_table(lE0, lu_vals: luV, ms: mS1))
        {:replace, fs, lU, prep}

      lU ->
        prep = r_prepared(prep0, qh: r_qlc_table(lE0, lu_vals: luV))
        {:skip, skipFils, lU, prep}

      travMS and mS !== :no_match_spec ->
        prep =
          r_prepared(prep0,
            qh: r_qlc_table(lE0, ms: mS),
            is_unique_objects: false
          )

        {:replace, fs, false, may_create_simple(opt, prep)}

      true ->
        {:no, [], false, prep0}
    end
  end

  defp prep_gen(r_qlc_list(l: []), prep0, _PosFun, {_MS, fs}, _Opt) do
    {:replace, fs, false, prep0}
  end

  defp prep_gen(r_qlc_list(ms: :no_match_spec) = lE0, prep0, _PosFun, {mS, fs}, opt)
       when mS !== :no_match_spec do
    prep = r_prepared(prep0, qh: r_qlc_list(lE0, ms: mS), is_cached: false)
    {:replace, fs, false, may_create_simple(opt, prep)}
  end

  defp prep_gen(r_qlc_list(), prep0, _PosFun, {mS, fs}, opt)
       when mS !== :no_match_spec do
    listMS = r_qlc_list(l: prep0, ms: mS)
    lE = r_prepared(qh: listMS, is_cached: false)
    {:replace, fs, false, may_create_simple(opt, lE)}
  end

  defp prep_gen(_LE0, prep0, _PosFun, _MSFs, _Opt) do
    {:no, [], false, prep0}
  end

  defp may_create_simple(
         r_qlc_opt(unique: unique, cache: cache) = opt,
         r_prepared(
           is_cached: isCached,
           is_unique_objects: isUnique
         ) = prep
       ) do
    cond do
      (unique and not isUnique) or
          (cache !== false and not isCached) ->
        prep_simple_qlc(:SQV, anno(1), prep, opt)

      true ->
        prep
    end
  end

  defp prep_simple_qlc(pVar, line, lE, opt) do
    check_join_option(opt)

    r_prepared(
      is_cached: isCached,
      sort_info: sortInfo,
      sorted: sorted,
      is_unique_objects: isUnique
    ) = lE

    r_qlc_opt(unique: unique, cache: cache) = opt

    cachez =
      cond do
        unique ->
          cache

        not isCached ->
          cache

        true ->
          false
      end

    optz = r_optz(unique: :erlang.and(unique, not isUnique), cache: cachez, opt: opt)
    qLC = r_simple_qlc(p: pVar, le: lE, line: line, init_value: :not_a_list, optz: optz)

    r_prepared(
      qh: qLC,
      is_unique_objects: :erlang.or(isUnique, unique),
      sort_info: sortInfo,
      sorted: sorted,
      is_cached: :erlang.or(isCached, cachez !== false)
    )
  end

  defp prep_sort(r_qlc_sort(h: r_prepared(sorted: :yes) = prep), _GOpt) do
    prep
  end

  defp prep_sort(
         r_qlc_sort(h: r_prepared(is_unique_objects: isUniqueObjs)) = q,
         gOpt
       ) do
    s1 = sort_unique(isUniqueObjs, q)
    s2 = sort_tmpdir(s1, gOpt)
    s = r_qlc_sort(s2, tmpdir_usage: r_qlc_opt(gOpt, :tmpdir_usage))
    {sortInfo, sorted} = sort_sort_info(s)

    r_prepared(
      qh: s,
      is_cached: true,
      sort_info: sortInfo,
      sorted: sorted,
      is_unique_objects:
        :erlang.or(
          r_qlc_sort(s, :unique),
          isUniqueObjs
        )
    )
  end

  defp prep_qlc(qFun, codeF, qdata0, qOpt, opt) do
    r_qlc_opt(unique: unique, cache: cache, join: join) = opt
    optz = r_optz(unique: unique, cache: cache, join_option: join, opt: opt)
    {qdata, sortInfo} = qlc_sort_info(qdata0, qOpt)
    qLC = r_qlc(lcf: qFun, codef: codeF, qdata: qdata, init_value: :not_a_list, optz: optz)

    r_prepared(
      qh: qLC,
      sort_info: sortInfo,
      is_unique_objects: unique,
      is_cached: cache !== false
    )
  end

  defp qlc_sort_info(qdata0, qOpt) do
    f = fn
      {_QNum, _GoI, _SI, :fil} = qd, info ->
        {qd, info}

      {_QNum, _GoI, _SI, {:gen, r_join()}} = qd, info ->
        {qd, info}

      {qNum, goI, sI, {:gen, prepLE0}}, info ->
        prepLE = sort_info(prepLE0, qNum, qOpt)
        qd = {qNum, goI, sI, {:gen, prepLE}}

        i =
          for {{c, order}, what} <- r_prepared(prepLE, :sort_info2),
              what === [],
              column <- equal_template_columns(qOpt, {qNum, c}) do
            {{column, order}, [{:traverse, qNum, c}]}
          end

        {qd, [i | info]}
    end

    {qdata, sortInfoL} = :lists.mapfoldl(f, [], qdata0)

    sortInfo0 =
      for pos <- constant_columns(qOpt, 0),
          ord <- orders(:yes) do
        {{pos, ord}, [:template]}
      end ++ :lists.append(sortInfoL)

    sortInfo = family_union(sortInfo0)
    {qdata, sortInfo}
  end

  defp sort_info(r_prepared(sort_info: sI, sorted: s) = prep, qNum, qOpt) do
    sI1 =
      for _ <- [:EFE_DUMMY_GEN],
          s !== :no,
          is_integer(sz = size_of_qualifier(qOpt, qNum)),
          sz > 0,
          (nConstCols = size_of_constant_prefix(qOpt, qNum)) < sz,
          c <- [nConstCols + 1],
          ord <- orders(s) do
        {{c, ord}, []}
      end ++
        for pos <- constant_columns(qOpt, qNum),
            ord <- orders(:yes) do
          {{pos, ord}, []}
        end ++
        for {posOrd, _} <- sI do
          {posOrd, []}
        end

    sI2 = :lists.usort(sI1)
    r_prepared(prep, sort_info2: sI2)
  end

  defp orders(:ascending = o) do
    [o]
  end

  defp orders(:yes) do
    [:ascending]
  end

  defp sort_unique(
         true,
         r_qlc_sort(fs_opts: sortOptions, keypos: :sort) = sort
       ) do
    r_qlc_sort(sort,
      unique: false,
      fs_opts: :lists.keydelete(:unique, 1, :lists.delete(:unique, sortOptions))
    )
  end

  defp sort_unique(_, sort) do
    sort
  end

  defp sort_tmpdir(s, r_qlc_opt(tmpdir: '')) do
    s
  end

  defp sort_tmpdir(s, opt) do
    r_qlc_sort(s, tmpdir: r_qlc_opt(opt, :tmpdir))
  end

  defp short_list(l) do
    r_prepared(qh: r_qlc_list(l: l), sorted: :yes, is_unique_objects: true, is_cached: true)
  end

  defp find_const_positions(
         iF,
         lU_fun,
         {keyEquality, posFun},
         r_qlc_opt(max_lookup: max, lookup: lookup)
       )
       when is_function(lU_fun) and is_function(posFun) and
              is_function(iF) and lookup !== false do
    case call(iF, :keypos, :undefined, []) do
      :undefined ->
        indices = call(iF, :indices, :undefined, [])
        find_const_position_idx(indices, keyEquality, posFun, max, [])

      keyPos ->
        case pos_vals(keyPos, keyEquality, posFun.(keyPos), max) do
          false ->
            find_const_position_idx(iF.(:indices), keyEquality, posFun, max, [])

          posValuesSkip ->
            posValuesSkip
        end
    end
  end

  defp find_const_positions(_IF, _LU_fun, _KE_PosFun, _Opt0) do
    {false, {:some, []}}
  end

  defp find_const_position_idx([i | is], keyEquality, posFun, max, l0) do
    case pos_vals(i, keyEquality, posFun.(i), max) do
      false ->
        find_const_position_idx(is, keyEquality, posFun, max, l0)

      {{_Pos, values}, _SkipFils} = posValuesFils ->
        l = [{length(values), posValuesFils} | l0]
        find_const_position_idx(is, keyEquality, posFun, max, l)
    end
  end

  defp find_const_position_idx(_, _KeyEquality, _PosFun, _Max, []) do
    {false, {:some, []}}
  end

  defp find_const_position_idx(_, _KeyEquality, _PosFun, _Max, l) do
    [{_, pVF} | _] = :lists.sort(l)
    pVF
  end

  defp pos_vals(pos, :==, {:usort_needed, values, skipFils}, max) do
    pos_vals_max(pos, :lists.usort(values), skipFils, max)
  end

  defp pos_vals(pos, :"=:=", {:usort_needed, values, skipFils}, max) do
    pos_vals_max(pos, :lists.sort(nub(values)), skipFils, max)
  end

  defp pos_vals(pos, _KeyEquality, {:values, values, skipFils}, max) do
    pos_vals_max(pos, values, skipFils, max)
  end

  defp pos_vals(_Pos, _KeyEquality, _T, _Max) do
    false
  end

  defp nub([]) do
    []
  end

  defp nub([e | l]) do
    case :lists.member(e, es = nub(l)) do
      true ->
        es

      false ->
        [e | es]
    end
  end

  defp pos_vals_max(pos, values, skip, max)
       when max === -1 or
              max >= length(values) do
    {{pos, values}, skip}
  end

  defp pos_vals_max(_Pos, _Value, _Skip, _Max) do
    false
  end

  defp prep_join(prep, qOpt, opt) do
    case join_opt(qOpt) do
      :undefined ->
        check_join_option(opt)
        prep

      equalMatch ->
        {ix, m} =
          case equalMatch do
            {nEqual, nMatch} ->
              pref_join(nEqual, nMatch, prep, qOpt, opt)

            eM ->
              pref_join(eM, eM, prep, qOpt, opt)
          end

        sI = family_union(r_prepared(prep, :sort_info) ++ m)
        r_prepared(prep, join: {ix, m}, sort_info: sI)
    end
  end

  defp pref_join(equal, match, prep, qOpt, r_qlc_opt(join: joinOpt)) do
    jQs =
      for {keyEquality, qCsL} <- [{:==, equal}, {:"=:=", match}],
          qCs <- qCsL do
        {keyEquality, qCs}
      end

    ixL =
      for _ <- [:EFE_DUMMY_GEN],
          joinOpt === :any or joinOpt === :lookup,
          {kE, qCs} <- jQs do
        pref_lookup_join(kE, qCs, prep, qOpt)
      end

    mL =
      for _ <- [:EFE_DUMMY_GEN],
          joinOpt === :any or joinOpt === :merge,
          {kE, qCs} <- jQs do
        pref_merge_join(kE, qCs, prep, qOpt)
      end

    {:lists.usort(:lists.append(ixL)), :lists.usort(:lists.append(mL))}
  end

  defp pref_lookup_join(keyEquality, {[{q1, c1}, {q2, c2}], skip}, prep, qOpt)
       when is_integer(c1) and is_integer(c2) do
    r_prepared(qh: r_qlc(qdata: qData)) = prep
    is1 = lookup_qual_data(qData, q1, keyEquality)

    lu2 =
      for iC1 <- is1, iC1 === c1 do
        pref_lookup_join2(q2, c2, q1, c1, skip, qOpt, keyEquality)
      end

    is2 = lookup_qual_data(qData, q2, keyEquality)

    lu1 =
      for iC2 <- is2, iC2 === c2 do
        pref_lookup_join2(q1, c1, q2, c2, skip, qOpt, keyEquality)
      end

    family(lu1 ++ lu2)
  end

  defp pref_lookup_join(kE, [{_, cs1}, {_, cs2}] = l, prep, qOpt)
       when is_list(cs1) and is_list(cs2) do
    :lists.append(
      for qC <- selections_no_skip(l) do
        pref_lookup_join(kE, qC, prep, qOpt)
      end
    )
  end

  defp lookup_qual_data(qData, qNum, keyEquality) do
    case :lists.keysearch(qNum, 1, qData) do
      {:value, {^qNum, _, _, {:gen, prepLE}}} ->
        join_indices(prepLE, keyEquality)
    end
  end

  defp join_indices(
         r_prepared(
           qh:
             r_qlc_table(
               info_fun: iF,
               lookup_fun: lU_fun,
               key_equality: keyEquality,
               lu_vals: :undefined
             )
         ),
         kE
       )
       when (is_function(lU_fun) and
               kE === keyEquality) or (kE === :"=:=" and keyEquality === :undefined) do
    kpL =
      case call(iF, :keypos, :undefined, []) do
        :undefined ->
          []

        kp ->
          [kp]
      end

    case call(iF, :indices, :undefined, []) do
      :undefined ->
        kpL

      is0 ->
        :lists.usort(kpL ++ is0)
    end
  end

  defp join_indices(_Prep, _KeyEquality) do
    []
  end

  defp pref_lookup_join2(q1, c1, q2, c2, skip, qOpt, keyEquality) do
    templCols = compared_template_columns(qOpt, {q1, c1}, keyEquality)
    {{q1, c1, q2, c2}, {:lookup_join, templCols, keyEquality, skip}}
  end

  defp pref_merge_join(kE, {[{q1, c1}, {q2, c2}], skip}, prep, qOpt)
       when is_integer(c1) and is_integer(c2) do
    r_prepared(qh: r_qlc(qdata: qData)) = prep
    sort1 = merge_qual_data(qData, q1)
    sort2 = merge_qual_data(qData, q2)
    merge = pref_merge(kE, q1, c1, q2, c2, skip, sort1, sort2, qOpt)
    family_union(merge)
  end

  defp pref_merge_join(kE, [{_, cs1}, {_, cs2}] = l, prep, qOpt)
       when is_list(cs1) and is_list(cs2) do
    :lists.append(
      for qC <- selections_no_skip(l) do
        pref_merge_join(kE, qC, prep, qOpt)
      end
    )
  end

  defp selections_no_skip(l) do
    for c <- all_selections(l) do
      {c, {:some, []}}
    end
  end

  defp merge_qual_data(qData, qNum) do
    case :lists.keysearch(qNum, 1, qData) do
      {:value, {^qNum, _, _, {:gen, prepLE}}} ->
        r_prepared(sort_info2: sortInfo) = prepLE
        sortInfo
    end
  end

  defp pref_merge(kE, q1, c1, q2, c2, skip, sort1, sort2, qOpt) do
    col1 = {q1, c1}
    col2 = {q2, c2}

    doSort =
      for {{_QNum, col} = qC, sortL} <- [{col1, sort1}, {col2, sort2}],
          :lists.keymember({col, :ascending}, 1, sortL) === false do
        qC
      end

    j = [{{q1, c1, q2, c2}, {:merge_join, doSort, kE, skip}}]

    for column <- equal_template_columns(qOpt, col1) do
      {{column, :ascending}, j}
    end ++ [{:other, j}]
  end

  defp table_sort_info(r_qlc_table(info_fun: iF)) do
    case call(iF, :is_sorted_key, :undefined, []) do
      :undefined ->
        {[], :no}

      false ->
        {[], :no}

      true ->
        case call(iF, :keypos, :undefined, []) do
          :undefined ->
            {[], :no}

          keyPos ->
            {[{{keyPos, :ascending}, []}], :no}
        end
    end
  end

  defp sort_sort_info(r_qlc_sort(keypos: :sort, order: ord0)) do
    {[], sort_order(ord0)}
  end

  defp sort_sort_info(r_qlc_sort(keypos: {:keysort, kp0}, order: ord0)) do
    kp =
      case kp0 do
        [pos | _] ->
          pos

        _ ->
          kp0
      end

    {[{{kp, sort_order(ord0)}, []}], :no}
  end

  defp sort_order(f) when is_function(f) do
    :no
  end

  defp sort_order(order) do
    order
  end

  defp check_join_option(r_qlc_opt(join: :any)) do
    :ok
  end

  defp check_join_option(r_qlc_opt(join: join)) do
    :erlang.error(:no_join_to_carry_out, [{:join, join}])
  end

  defp check_lookup_option(r_qlc_opt(lookup: true), false) do
    :erlang.error(
      :no_lookup_to_carry_out,
      [{:lookup, true}]
    )
  end

  defp check_lookup_option(_QOpt, _LuV) do
    :ok
  end

  defp compared_template_columns(qOpt, qNumColumn, keyEquality) do
    qOpt.(:template).(qNumColumn, keyEquality)
  end

  defp equal_template_columns(qOpt, qNumColumn) do
    qOpt.(:template).(qNumColumn, :==)
  end

  defp size_of_constant_prefix(qOpt, qNum) do
    qOpt.(:n_leading_constant_columns).(qNum)
  end

  defp constants(qOpt, qNum) do
    qOpt.(:constants).(qNum)
  end

  defp equal_constants(qOpt, qNum) do
    qOpt.(:equal_constants).(qNum)
  end

  defp join_opt(qOpt) do
    qOpt.(:join)
  end

  defp match_specs(qOpt, qNum) do
    qOpt.(:match_specs).(qNum)
  end

  defp constant_columns(qOpt, qNum) do
    qOpt.(:constant_columns).(qNum)
  end

  defp size_of_qualifier(qOpt, qNum) do
    qOpt.(:size).(qNum)
  end

  defp opt_le(
         r_prepared(qh: r_simple_qlc(le: lE0, optz: optz0) = qLC) = prep0,
         genNum
       ) do
    case lE0 do
      r_prepared(qh: r_simple_qlc(p: lE_Pvar, le: lE2, optz: optz2)) ->
        cachez =
          case r_optz(optz2, :cache) do
            false ->
              r_optz(optz0, :cache)

            cache2 ->
              cache2
          end

        optz =
          r_optz(optz0,
            cache: cachez,
            unique:
              :erlang.or(
                r_optz(optz0, :unique),
                r_optz(optz2, :unique)
              )
          )

        pVar =
          cond do
            lE_Pvar === :SQV ->
              r_simple_qlc(qLC, :p)

            true ->
              lE_Pvar
          end

        prep = r_prepared(prep0, qh: r_simple_qlc(qLC, p: pVar, le: lE2, optz: optz))
        opt_le(prep, genNum)

      _ ->
        optz1 = no_cache_of_first_generator(optz0, genNum)

        case {opt_le(lE0, 1), optz1} do
          {lE, r_optz(unique: false, cache: false)} ->
            lE

          {lE, _} ->
            r_prepared(prep0, qh: r_simple_qlc(qLC, le: lE, optz: optz1))
        end
    end
  end

  defp opt_le(
         r_prepared(
           qh: r_qlc(),
           lu_skip_quals: lU_SkipQuals0
         ) = prep0,
         genNum
       ) do
    r_prepared(qh: r_qlc(qdata: qdata0, optz: optz0) = qLC) = prep0
    r_optz(join_option: joinOption, opt: opt) = optz0
    ^joinOption = r_optz(optz0, :join_option)

    {lU_QNum, join, joinSkipFs, doSort} =
      opt_join(r_prepared(prep0, :join), joinOption, qdata0, opt, lU_SkipQuals0)

    {lU_Skip, lU_SkipQuals} =
      :lists.partition(
        fn {qNum, _Fs} ->
          qNum === lU_QNum
        end,
        lU_SkipQuals0
      )

    lU_SkipFs =
      :lists.flatmap(
        fn {_QNum, fs} ->
          fs
        end,
        lU_SkipQuals
      )

    qdata1 =
      cond do
        lU_Skip === [] ->
          qdata0

        true ->
          activate_join_lookup_filter(lU_QNum, qdata0)
      end

    qdata2 =
      skip_lookup_filters(
        qdata1,
        lU_SkipFs ++ joinSkipFs
      )

    f = fn
      {qNum, goI, sI, {:gen, r_prepared() = prepLE}}, genNum1 ->
        newPrepLE = maybe_sort(prepLE, qNum, doSort, opt)
        {{qNum, goI, sI, {:gen, opt_le(newPrepLE, genNum1)}}, genNum1 + 1}

      qd, genNum1 ->
        {qd, genNum1}
    end

    {qdata, _} = :lists.mapfoldl(f, 1, qdata2)
    optz1 = no_cache_of_first_generator(optz0, genNum)
    optz = r_optz(optz1, fast_join: join)
    r_prepared(prep0, qh: r_qlc(qLC, qdata: qdata, optz: optz))
  end

  defp opt_le(r_prepared(qh: r_qlc_append(hl: hL)) = prep, genNum) do
    hs =
      for h <- hL do
        opt_le(h, genNum)
      end

    r_prepared(prep, qh: r_qlc_append(hl: hs))
  end

  defp opt_le(r_prepared(qh: r_qlc_sort(h: h) = sort) = prep, genNum) do
    r_prepared(prep, qh: r_qlc_sort(sort, h: opt_le(h, genNum)))
  end

  defp opt_le(prep, _GenNum) do
    prep
  end

  defp no_cache_of_first_generator(optz, genNum) when genNum > 1 do
    optz
  end

  defp no_cache_of_first_generator(optz, 1) do
    r_optz(optz, cache: false)
  end

  defp maybe_sort(lE, qNum, doSort, opt) do
    case :lists.keyfind(qNum, 1, doSort) do
      {^qNum, col} ->
        r_qlc_opt(tmpdir: tmpDir, tmpdir_usage: tmpUsage) = opt

        sortOpts =
          for dir <- [tmpDir], dir !== '' do
            {:tmpdir, dir}
          end

        sort =
          r_qlc_sort(
            h: lE,
            keypos: {:keysort, col},
            unique: false,
            compressed: [],
            order: :ascending,
            fs_opts: sortOpts,
            tmpdir_usage: tmpUsage,
            tmpdir: tmpDir
          )

        r_prepared(qh: sort, sorted: :no, join: :no)

      false ->
        lE
    end
  end

  defp skip_lookup_filters(qdata, []) do
    qdata
  end

  defp skip_lookup_filters(qdata0, lU_SkipFs) do
    for {qNum, goI, _, _} = qd <- qdata0 do
      case :lists.member(qNum, lU_SkipFs) do
        true ->
          {qNum, goI, -1, :fil}

        false ->
          qd
      end
    end
  end

  defp activate_join_lookup_filter(qNum, qdata) do
    {_, goI2, sI2, {:gen, prep2}} = :lists.keyfind(qNum, 1, qdata)
    table2 = r_prepared(prep2, :qh)
    nPrep2 = r_prepared(prep2, qh: r_qlc_table(table2, ms: :no_match_spec))
    :lists.keyreplace(qNum, 1, qdata, {qNum, goI2, sI2, {:gen, nPrep2}})
  end

  defp opt_join(join, joinOption, qdata, opt, lU_SkipQuals) do
    {ix0, m0} = join
    ix1 = opt_join_lu(ix0, qdata, lU_SkipQuals)
    ix = :lists.reverse(:lists.keysort(2, ix1))

    case ix do
      [{{q1, c1, q2, c2}, skip, kE, lU_fun} | _] ->
        j = r_qlc_join(kind: {:lookup, kE, lU_fun}, q1: q1, c1: c1, q2: q2, c2: c2, opt: opt)
        {q2, j, skip, []}

      [] ->
        m = opt_join_merge(m0)

        case m do
          [
            {{q1, c1, q2, c2}, {:merge_join, doSort, kE, skip}}
            | _
          ] ->
            j = r_qlc_join(kind: {:merge, kE}, opt: opt, q1: q1, c1: c1, q2: q2, c2: c2)
            {:not_a_qnum, j, skip, doSort}

          [] when joinOption === :nested_loop ->
            {:not_a_qnum, :no, [], []}

          _ when joinOption !== :any ->
            :erlang.error(:cannot_carry_out_join, [joinOption])

          _ ->
            {:not_a_qnum, :no, [], []}
        end
    end
  end

  defp opt_join_lu(
         [
           {{_Q1, _C1, q2, _C2} = j, [{:lookup_join, _KEols, jKE, skip0} | _]}
           | lJ
         ],
         qdata,
         lU_SkipQuals
       ) do
    {^q2, _, _, {:gen, prep2}} = :lists.keyfind(q2, 1, qdata)
    r_qlc_table(ms: mS, key_equality: kE, lookup_fun: lU_fun) = r_prepared(prep2, :qh)

    case mS !== :no_match_spec and :lists.keymember(q2, 1, lU_SkipQuals) === false do
      true ->
        opt_join_lu(lJ, qdata, lU_SkipQuals)

      false ->
        skip = skip_if_possible(jKE, kE, skip0)
        [{j, skip, kE, lU_fun} | opt_join_lu(lJ, qdata, lU_SkipQuals)]
    end
  end

  defp opt_join_lu([], _Qdata, _LU_SkipQuals) do
    []
  end

  defp opt_join_merge(m) do
    l =
      for {_KpOrder_or_other, mJ} <- m,
          {qCs, {:merge_join, doSort, kE, skip0}} <- mJ,
          skip <- [skip_if_possible(kE, :==, skip0)] do
        {-length(doSort), length(skip), {qCs, {:merge_join, doSort, kE, skip}}}
      end

    :lists.reverse(
      for {_, _, j} <- :lists.sort(l) do
        j
      end
    )
  end

  defp skip_if_possible(:"=:=", :==, _) do
    []
  end

  defp skip_if_possible(_, _, {_SkipTag, skip}) do
    skip
  end

  defp setup_qlc(prep, setup) do
    post0 = []
    setup_le(prep, post0, setup)
  end

  defp setup_le(r_prepared(qh: r_simple_qlc(le: lE, optz: optz)), post0, setup) do
    {objs, post, localPost} = setup_le(lE, post0, setup)
    unique_cache(objs, post, localPost, optz)
  end

  defp setup_le(
         r_prepared(qh: r_qlc(lcf: qFun, qdata: qdata, init_value: v, optz: optz)),
         post0,
         setup
       ) do
    {goTo, firstState, post, localPost} = setup_quals(qdata, post0, setup, optz)

    objs = fn ->
      qFun.(firstState, v, goTo)
    end

    unique_cache(objs, post, localPost, optz)
  end

  defp setup_le(r_prepared(qh: r_qlc_table(post_fun: postFun) = table), post, setup) do
    h = table_handle(table, post, setup)
    {h, [postFun | post], []}
  end

  defp setup_le(r_prepared(qh: r_qlc_append(hl: prepL)), post0, setup) do
    f = fn prep, {post1, lPost1} ->
      {objs, post2, lPost2} = setup_le(prep, post1, setup)
      {objs, {post2, lPost1 ++ lPost2}}
    end

    {objsL, {post, localPost}} = :lists.mapfoldl(f, {post0, []}, prepL)

    {fn ->
       append_loop(objsL, 0)
     end, post, localPost}
  end

  defp setup_le(
         r_prepared(
           qh:
             r_qlc_sort(
               h: prep,
               keypos: kp,
               unique: unique,
               compressed: compressed,
               order: order,
               fs_opts: sortOptions0,
               tmpdir_usage: tmpUsage,
               tmpdir: tmpDir
             )
         ),
         post0,
         setup
       ) do
    sortOptions =
      sort_options_global_tmp(
        sortOptions0,
        tmpDir
      )

    lF = fn objs ->
      sort_list(objs, order, unique, kp, sortOptions, post0)
    end

    case setup_le(prep, post0, setup) do
      {l, post, localPost} when is_list(l) ->
        {lF.(l), post, localPost}

      {objs, post, localPost} ->
        fF = fn objs1 ->
          file_sort_handle(objs1, kp, sortOptions, tmpDir, compressed, post, localPost)
        end

        sort_handle(objs, lF, fF, sortOptions, post, localPost, {tmpUsage, :sorting})
    end
  end

  defp setup_le(r_prepared(qh: r_qlc_list(l: l, ms: mS)), post, _Setup)
       when :no_match_spec === mS or l === [] do
    {l, post, []}
  end

  defp setup_le(r_prepared(qh: r_qlc_list(l: l, ms: mS)), post, _Setup)
       when is_list(l) do
    {:ets.match_spec_run(l, :ets.match_spec_compile(mS)), post, []}
  end

  defp setup_le(r_prepared(qh: r_qlc_list(l: h0, ms: mS)), post0, setup) do
    {objs0, post, localPost} = setup_le(h0, post0, setup)

    objs =
      :ets.match_spec_run(
        objs0,
        :ets.match_spec_compile(mS)
      )

    {objs, post, localPost}
  end

  defp setup_quals(qdata, post0, setup, optz) do
    {goTo0, post1, localPost0} = setup_quals(0, qdata, [], post0, [], setup)
    goTo1 = :lists.keysort(1, goTo0)
    firstState0 = next_state(qdata)

    {goTo2, firstState, post, localPost1} =
      case r_optz(optz, :fast_join) do
        r_qlc_join(kind: {:merge, _KE}, c1: c1, c2: c2, opt: opt) = mJ ->
          mF = fn _Rev, {h1, wH1}, {h2, wH2} ->
            fn ->
              merge_join(wH1.(h1), c1, wH2.(h2), c2, opt)
            end
          end

          setup_join(mJ, qdata, goTo1, firstState0, mF, post1)

        r_qlc_join(kind: {:lookup, _KE, luF}, c1: c1, c2: c2) = lJ ->
          lF = fn rev, {h1, wH1}, {h2, wH2} ->
            {h, w} =
              cond do
                rev ->
                  {h2, wH2}

                true ->
                  {h1, wH1}
              end

            fn ->
              lookup_join(w.(h), c1, luF, c2, rev)
            end
          end

          setup_join(lJ, qdata, goTo1, firstState0, lF, post1)

        :no ->
          {flat_goto(goTo1), firstState0, post1, []}
      end

    goTo = :erlang.list_to_tuple(goTo2)
    {goTo, firstState, post, localPost0 ++ localPost1}
  end

  defp setup_quals(genLoopS, [{_QNum, goI, -1, :fil} | qdata], gs, p, lP, setup) do
    setup_quals(genLoopS, qdata, [{goI, [-1, -1]} | gs], p, lP, setup)
  end

  defp setup_quals(genLoopS, [{_QNum, goI, _SI, :fil} | qdata], gs, p, lP, setup) do
    setup_quals(genLoopS, qdata, [{goI, [genLoopS, next_state(qdata)]} | gs], p, lP, setup)
  end

  defp setup_quals(genLoopS, [{_QNum, goI, _SI, {:gen, r_join()}} | qdata], gs, p, lP, setup) do
    setup_quals(genLoopS, qdata, [{goI, [-1, -1, -1]} | gs], p, lP, setup)
  end

  defp setup_quals(genLoopS, [{_QNum, goI, sI, {:gen, lE}} | qdata], gs, p, lP, setup) do
    {v, nP, lP1} = setup_le(lE, p, setup)

    setup_quals(
      sI + 1,
      qdata,
      [{goI, [genLoopS, next_state(qdata), v]} | gs],
      nP,
      lP ++ lP1,
      setup
    )
  end

  defp setup_quals(genLoopS, [], gs, p, lP, _Setup) do
    {[{1, [genLoopS]} | gs], p, lP}
  end

  defp setup_join(j, qdata, goTo0, firstState0, joinFun, post0) do
    r_qlc_join(q1: qNum1a, q2: qNum2a, opt: opt) = j

    {{_QN, jGoI, jSI, _}, rev, qNum1, qNum2, wH1, wH2, _CsFun} =
      find_join_data(qdata, qNum1a, qNum2a)

    [{goI1, sI1}] =
      for {qNum, goI, sI, _} <- qdata,
          qNum === qNum1 do
        {goI, sI}
      end

    [{goI2, sI2}] =
      for {qNum, goI, sI, _} <- qdata,
          qNum === qNum2 do
        {goI, sI}
      end

    [h1] =
      for {goI, [_Back, _Forth, h]} <- goTo0,
          goI === goI1 do
        h
      end

    [{backH2, h2}] =
      for {goI, [back, _Forth, h]} <- goTo0,
          goI === goI2 do
        {back, h}
      end

    h0 = joinFun.(rev, {h1, wH1}, {h2, wH2})
    {h, post, localPost} = unique_cache(h0, post0, [], join_unique_cache(opt))

    [jBack] =
      for {goI, [back, _, _]} <- goTo0,
          goI === goI1 do
        back
      end

    jForth = next_after(qdata, sI1, qNum2)

    goTo1 =
      :lists.map(
        fn
          {goI, _} when goI === jGoI ->
            {jGoI, [jBack, jForth, h]}

          {goI, _} when goI === goI1 or goI === goI2 ->
            {goI, [-1, -1, -1]}

          go ->
            go
        end,
        goTo0
      )

    goTo =
      :lists.map(
        fn
          s when s === sI1 ->
            jSI

          s when s === sI2 ->
            next_after(qdata, s, qNum2)

          s when s === sI1 + 1 ->
            jSI + 1

          s when s === sI2 + 1 and sI1 + 1 === backH2 ->
            jSI + 1

          s when s === sI2 + 1 ->
            backH2

          s ->
            s
        end,
        flat_goto(goTo1)
      )

    firstState =
      cond do
        sI1 === firstState0 ->
          jSI

        true ->
          firstState0
      end

    {goTo, firstState, post, localPost}
  end

  defp join_unique_cache(r_qlc_opt(cache: cache, unique: unique) = opt) do
    r_optz(cache: cache, unique: unique, opt: opt)
  end

  defp flat_goto(goTo) do
    :lists.flatmap(
      fn {_, l} ->
        l
      end,
      goTo
    )
  end

  defp next_after([{_, _, s, _} | qdata], s, qNum2) do
    case qdata do
      [{^qNum2, _, _, _} | qdata1] ->
        next_state(qdata1)

      _ ->
        next_state(qdata)
    end
  end

  defp next_after([_ | qdata], s, qNum2) do
    next_after(qdata, s, qNum2)
  end

  defp next_state([{_, _, _, {:gen, r_join()}} | qdata]) do
    next_state(qdata)
  end

  defp next_state([{_, _, -1, :fil} | qdata]) do
    next_state(qdata)
  end

  defp next_state([{_, _, s, _} | _]) do
    s
  end

  defp next_state([]) do
    template_state()
  end

  defp find_join_data(qdata, qNum1, qNum2) do
    [qRev] =
      for {_QN, _GoI, _SI, {:gen, r_join(q1: qN1, q2: qN2, wh1: h1, wh2: h2, cs_fun: csF)}} = q <-
            qdata,
          (cond do
             qN1 === qNum1 and qN2 === qNum2 ->
               not rev = false

             qN1 === qNum2 and qN2 === qNum1 ->
               rev = true

             true ->
               rev = false
           end) do
        {q, rev, qN1, qN2, h1, h2, csF}
      end

    qRev
  end

  defp table_handle(
         r_qlc_table(
           trav_fun: traverseFun,
           trav_MS: travMS,
           pre_fun: preFun,
           lookup_fun: luF,
           parent_fun: parentFun,
           lu_vals: luVals,
           ms: mS
         ),
         post,
         setup
       ) do
    r_setup(parent: parent) = setup

    parentValue =
      cond do
        parentFun === :undefined ->
          :undefined

        parent === self() ->
          try do
            parentFun.()
          catch
            class, reason ->
              post_funs(post)
              :erlang.raise(class, reason, __STACKTRACE__)
          end

        true ->
          case monitor_request(
                 parent,
                 {:parent_fun, parentFun}
               ) do
            :error ->
              post_funs(post)
              exit(:normal)

            {:value, value} ->
              value

            {:parent_fun_caught, class, reason, stacktrace} ->
              post_funs(post)
              :erlang.raise(class, reason, stacktrace)
          end
      end

    stopFun =
      cond do
        parent === self() ->
          :undefined

        true ->
          cursor = r_qlc_cursor(c: {self(), parent})

          fn ->
            delete_cursor(cursor)
          end
      end

    preFunArgs = [{:parent_value, parentValue}, {:stop_fun, stopFun}]
    _ = call(preFun, preFunArgs, :ok, post)

    case luVals do
      {pos, vals} when mS === :no_match_spec ->
        luF.(pos, vals)

      {pos, vals} ->
        case luF.(pos, vals) do
          [] ->
            []

          objs when is_list(objs) ->
            :ets.match_spec_run(objs, :ets.match_spec_compile(mS))

          error ->
            post_funs(post)
            throw_error(error)
        end

      _ when not travMS ->
        ^mS = :no_match_spec
        traverseFun

      _ when mS === :no_match_spec ->
        fn ->
          traverseFun.([{:"$1", [], [:"$1"]}])
        end

      _ ->
        fn ->
          traverseFun.(mS)
        end
    end
  end

  defp open_file(fileName, extra, post) do
    case :file.open(
           fileName,
           [[:read, :raw, :binary] | extra]
         ) do
      {:ok, fd} ->
        {fn ->
           case :file.position(fd, :bof) do
             {:ok, 0} ->
               tF = fn
                 [], _ ->
                   []

                 ts, c when is_list(ts) ->
                   :lists.reverse(ts, c)
               end

               file_loop_read(<<>>, 64 * 1024, {fd, fileName}, tF)

             error ->
               file_error(fileName, error)
           end
         end, fd}

      error ->
        post_funs(post)
        throw_file_error(fileName, error)
    end
  end

  defp file_loop(bin0, fd_FName, ts0, tF) do
    case (try do
            file_loop2(bin0, ts0)
          catch
            _, _ ->
              {_Fd, fileName} = fd_FName
              :erlang.error({:bad_object, fileName})
          end) do
      {:terms, <<size::size(4)-unit(8), b::bytes>> = bin, []} ->
        file_loop_read(bin, size - byte_size(b) + 4, fd_FName, tF)

      {:terms, <<size::size(4)-unit(8), _::bytes>> = bin, ts} ->
        c = fn ->
          file_loop_read(bin, size + 4, fd_FName, tF)
        end

        tF.(ts, c)

      {:terms, b, ts} ->
        c = fn ->
          file_loop_read(b, 64 * 1024, fd_FName, tF)
        end

        tF.(ts, c)

      error ->
        error
    end
  end

  defp file_loop2(
         <<size::size(4)-unit(8), b::size(size)-bytes, bin::bytes>>,
         ts
       ) do
    file_loop2(bin, [:erlang.binary_to_term(b) | ts])
  end

  defp file_loop2(bin, ts) do
    {:terms, bin, ts}
  end

  defp file_loop_read(b, minBytesToRead, {fd, fileName} = fd_FName, tF) do
    bytesToRead = :erlang.max(64 * 1024, minBytesToRead)

    case :file.read(fd, bytesToRead) do
      {:ok, bin} when byte_size(b) === 0 ->
        file_loop(bin, fd_FName, [], tF)

      {:ok, bin} ->
        case b do
          <<size::size(4)-unit(8), tl::bytes>>
          when byte_size(bin) + byte_size(tl) >= size ->
            {b1, b2} =
              :erlang.split_binary(
                bin,
                size - byte_size(tl)
              )

            foo = fn [t], fun ->
              [t | fun]
            end

            case file_loop(:erlang.list_to_binary([b, b1]), fd_FName, [], foo) do
              [t | fun] ->
                true = is_function(fun)
                file_loop(b2, fd_FName, [t], tF)

              error ->
                error
            end

          _ ->
            file_loop(:erlang.list_to_binary([b, bin]), fd_FName, [], tF)
        end

      :eof when byte_size(b) === 0 ->
        tF.([], :foo)

      :eof ->
        :erlang.error({:bad_object, fileName})

      error ->
        file_error(fileName, error)
    end
  end

  defp sort_cursor_input(h, noObjects) do
    fn
      :close ->
        :ok

      :read ->
        sort_cursor_input_read(h, noObjects)
    end
  end

  defp sort_cursor_list_output(tmpDir, z, unique) do
    fn
      :close ->
        {:terms, []}

      {:value, noObjects} ->
        fn
          bTerms when unique or length(bTerms) === noObjects ->
            fn
              :close ->
                {:terms, bTerms}

              bTerms1 ->
                sort_cursor_file(bTerms ++ bTerms1, tmpDir, z)
            end

          bTerms ->
            sort_cursor_file(bTerms, tmpDir, z)
        end
    end
  end

  defp sort_cursor_file(bTerms, tmpDir, z) do
    fName = tmp_filename(tmpDir)

    case :file.open(
           fName,
           [[:write, :raw, :binary] | z]
         ) do
      {:ok, fd} ->
        wFun = write_terms(fName, fd)
        wFun.(bTerms)

      error ->
        throw_file_error(fName, error)
    end
  end

  defp sort_options_global_tmp(s, '') do
    s
  end

  defp sort_options_global_tmp(s, tmpDir) do
    [{:tmpdir, tmpDir} | :lists.keydelete(:tmpdir, 1, s)]
  end

  defp tmp_filename(tmpDirOpt) do
    u = '_'
    node = node()
    pid = :os.getpid()
    unique = :erlang.unique_integer()
    f = :lists.concat([:qlc, u, node, u, pid, u, unique])

    tmpDir =
      case tmpDirOpt do
        '' ->
          {:ok, curDir} = :file.get_cwd()
          curDir

        tDir ->
          tDir
      end

    :filename.join(:filename.absname(tmpDir), f)
  end

  defp write_terms(fileName, fd) do
    fn
      :close ->
        _ = :file.close(fd)
        {:file, fileName}

      bTerms ->
        case :file.write(fd, size_bin(bTerms, [])) do
          :ok ->
            write_terms(fileName, fd)

          error ->
            _ = :file.close(fd)
            throw_file_error(fileName, error)
        end
    end
  end

  defp size_bin([], l) do
    l
  end

  defp size_bin([binTerm | binTerms], l) do
    size_bin(
      binTerms,
      [
        [l, <<byte_size(binTerm)::size(4)-unit(8)>>]
        | binTerm
      ]
    )
  end

  defp sort_cursor_input_read([], noObjects) do
    {:end_of_input, noObjects}
  end

  defp sort_cursor_input_read([object | cont], noObjects) do
    {[:erlang.term_to_binary(object)], sort_cursor_input(cont, noObjects + 1)}
  end

  defp sort_cursor_input_read(f, noObjects) do
    case f.() do
      objects when is_list(objects) ->
        sort_cursor_input_read(objects, noObjects)

      term ->
        throw_error(term)
    end
  end

  defp unique_cache(l, post, localPost, optz) when is_list(l) do
    case r_optz(optz, :unique) do
      true ->
        {unique_sort_list(l), post, localPost}

      false ->
        {l, post, localPost}
    end
  end

  defp unique_cache(h, post, localPost, r_optz(unique: false, cache: false)) do
    {h, post, localPost}
  end

  defp unique_cache(h, post, localPost, r_optz(unique: true, cache: false)) do
    e = :ets.new(:qlc, [:set, :private])

    {fn ->
       no_dups(h, e)
     end, [del_table(e) | post], localPost}
  end

  defp unique_cache(h, post, localPost, r_optz(unique: false, cache: true)) do
    e = :ets.new(:qlc, [:set, :private])
    {l, p} = unique_cache_post(e)

    {fn ->
       cache(h, e, localPost)
     end, [p | post], [l]}
  end

  defp unique_cache(h, post, localPost, r_optz(unique: true, cache: true)) do
    uT = :ets.new(:qlc, [:bag, :private])
    mT = :ets.new(:qlc, [:set, :private])
    {l1, p1} = unique_cache_post(uT)
    {l2, p2} = unique_cache_post(mT)

    {fn ->
       ucache(h, uT, mT, localPost)
     end, [[p1, p2] | post], [l1, l2]}
  end

  defp unique_cache(h, post, localPost, r_optz(unique: false, cache: :list) = optz) do
    ref = make_ref()
    f = del_lcache(ref)
    r_qlc_opt(tmpdir: tmpDir, max_list: maxList, tmpdir_usage: tmpUsage) = r_optz(optz, :opt)

    {fn ->
       lcache(h, ref, localPost, tmpDir, maxList, tmpUsage)
     end, [f | post], [f]}
  end

  defp unique_cache(h, post0, localPost0, r_optz(unique: true, cache: :list) = optz) do
    r_qlc_opt(tmpdir: tmpDir, max_list: maxList, tmpdir_usage: tmpUsage) = r_optz(optz, :opt)

    size =
      cond do
        maxList >= 1 <<< 31 ->
          1 <<< (31 - 1)

        maxList === 0 ->
          1

        true ->
          maxList
      end

    sortOptions = [{:size, size}, {:tmpdir, tmpDir}]
    uSortOptions = [{:unique, true} | sortOptions]
    tmpUsageM = {tmpUsage, :caching}

    lF1 = fn objs ->
      :lists.ukeysort(1, objs)
    end

    fF1 = fn objs ->
      file_sort_handle(objs, {:keysort, 1}, uSortOptions, tmpDir, [], post0, localPost0)
    end

    {uH, post1, localPost1} =
      sort_handle(tag_objects(h, 1), lF1, fF1, uSortOptions, post0, localPost0, tmpUsageM)

    lF2 = fn objs ->
      :lists.keysort(2, objs)
    end

    fF2 = fn objs ->
      file_sort_handle(objs, {:keysort, 2}, sortOptions, tmpDir, [], post1, localPost1)
    end

    {sH, post, localPost} = sort_handle(uH, lF2, fF2, sortOptions, post1, localPost1, tmpUsageM)

    cond do
      is_list(sH) ->
        {untag_objects2(sH), post, localPost}

      true ->
        {fn ->
           untag_objects(sH)
         end, post, localPost}
    end
  end

  defp unique_cache_post(e) do
    {empty_table(e), del_table(e)}
  end

  defp unique_sort_list(l) do
    e = :ets.new(:qlc, [:set, :private])
    unique_list(l, e)
  end

  defp unique_list([], e) do
    true = :ets.delete(e)
    []
  end

  defp unique_list([object | objects], e) do
    case :ets.member(e, object) do
      false ->
        true = :ets.insert(e, {object})
        [object | unique_list(objects, e)]

      true ->
        unique_list(objects, e)
    end
  end

  defp sort_list(l, cFun, true, :sort, _SortOptions, _Post)
       when is_function(cFun) do
    :lists.usort(cFun, l)
  end

  defp sort_list(l, cFun, false, :sort, _SortOptions, _Post)
       when is_function(cFun) do
    :lists.sort(cFun, l)
  end

  defp sort_list(l, :ascending, true, :sort, _SortOptions, _Post) do
    :lists.usort(l)
  end

  defp sort_list(l, :descending, true, :sort, _SortOptions, _Post) do
    :lists.reverse(:lists.usort(l))
  end

  defp sort_list(l, :ascending, false, :sort, _SortOptions, _Post) do
    :lists.sort(l)
  end

  defp sort_list(l, :descending, false, :sort, _SortOptions, _Post) do
    :lists.reverse(:lists.sort(l))
  end

  defp sort_list(l, order, unique, {:keysort, kp}, _SortOptions, _Post)
       when is_integer(kp) and is_atom(order) do
    case {order, unique} do
      {:ascending, true} ->
        :lists.ukeysort(kp, l)

      {:ascending, false} ->
        :lists.keysort(kp, l)

      {:descending, true} ->
        :lists.reverse(:lists.ukeysort(kp, l))

      {:descending, false} ->
        :lists.reverse(:lists.keysort(kp, l))
    end
  end

  defp sort_list(l, _Order, _Unique, sort, sortOptions, post) do
    in__ = fn _ ->
      {l,
       fn _ ->
         :end_of_input
       end}
    end

    out = sort_list_output([])
    tSortOptions = [{:format, :term} | sortOptions]
    do_sort(in__, out, sort, tSortOptions, post)
  end

  defp sort_list_output(l) do
    fn
      :close ->
        :lists.append(:lists.reverse(l))

      terms when is_list(terms) ->
        sort_list_output([terms | l])
    end
  end

  defp sort_handle(h, listFun, fileFun, sortOptions, post, localPost, tmpUsageM) do
    size =
      case :lists.keyfind(:size, 1, sortOptions) do
        {:size, size0} ->
          size0

        false ->
          default_option(:size)
      end

    sort_cache(h, [], size, {listFun, fileFun, post, localPost, tmpUsageM})
  end

  defp sort_cache([], cL, _Sz, {lF, _FF, post, localPost, _TmpUsageM}) do
    {lF.(:lists.reverse(cL)), post, localPost}
  end

  defp sort_cache(objs, cL, sz, c) when sz < 0 do
    sort_cache2(objs, cL, false, c)
  end

  defp sort_cache([object | cont], cL, sz0, c) do
    sz = decr_list_size(sz0, object)
    sort_cache(cont, [object | cL], sz, c)
  end

  defp sort_cache(f, cL, sz, c) do
    case f.() do
      objects when is_list(objects) ->
        sort_cache(objects, cL, sz, c)

      term ->
        {_LF, _FF, post, _LocalPost, _TmpUsageM} = c
        post_funs(post)
        throw_error(term)
    end
  end

  defp sort_cache2([], cL, _X, {lF, _FF, post, localPost, _TmpUsageM}) do
    {lF.(:lists.reverse(cL)), post, localPost}
  end

  defp sort_cache2([object | cont], cL, _, c) do
    sort_cache2(cont, [object | cL], true, c)
  end

  defp sort_cache2(f, cL, false, c) do
    case f.() do
      objects when is_list(objects) ->
        sort_cache2(objects, cL, true, c)

      term ->
        {_LF, _FF, post, _LocalPost, _TmpUsageM} = c
        post_funs(post)
        throw_error(term)
    end
  end

  defp sort_cache2(_Cont, _CL, true, {_LF, _FF, post, _LocalPost, {:not_allowed, m}}) do
    post_funs(post)
    throw_reason({:tmpdir_usage, m})
  end

  defp sort_cache2(cont, cL, true, {_LF, fF, _Post, _LocalPost, {tmpUsage, m}}) do
    maybe_error_logger(tmpUsage, m)
    fF.(:lists.reverse(cL, cont))
  end

  defp file_sort_handle(h, kp, sortOptions, tmpDir, compressed, post, localPost) do
    in__ = sort_cursor_input(h, 0)

    unique =
      :lists.member(
        :unique,
        sortOptions
      ) or :lists.keymember(:unique, 1, sortOptions)

    out = sort_cursor_list_output(tmpDir, compressed, unique)
    reply = do_sort(in__, out, kp, sortOptions, post)

    case reply do
      {:file, fileName} ->
        {f, fd} = open_file(fileName, compressed, post)

        p = fn ->
          _ = :file.close(fd)
          _ = :file.delete(fileName)
        end

        {f, [p | post], localPost}

      {:terms, bTerms} ->
        try do
          {for b <- bTerms do
             :erlang.binary_to_term(b)
           end, post, localPost}
        catch
          class, reason ->
            post_funs(post)
            :erlang.raise(class, reason, __STACKTRACE__)
        end
    end
  end

  defp do_sort(in__, out, sort, sortOptions, post) do
    try do
      case do_sort(in__, out, sort, sortOptions) do
        {:error, reason} ->
          throw_reason(reason)

        reply ->
          reply
      end
    catch
      class, term ->
        post_funs(post)
        :erlang.raise(class, term, __STACKTRACE__)
    end
  end

  defp do_sort(in__, out, :sort, sortOptions) do
    :file_sorter.sort(in__, out, sortOptions)
  end

  defp do_sort(in__, out, {:keysort, keyPos}, sortOptions) do
    :file_sorter.keysort(keyPos, in__, out, sortOptions)
  end

  defp del_table(ets) do
    fn ->
      true = :ets.delete(ets)
    end
  end

  defp empty_table(ets) do
    fn ->
      true = :ets.delete_all_objects(ets)
    end
  end

  defp append_loop([[_ | _] = l], _N) do
    l
  end

  defp append_loop([f], _N) do
    f.()
  end

  defp append_loop([l | hs], n) do
    append_loop(l, n, hs)
  end

  defp append_loop([], n, hs) do
    append_loop(hs, n)
  end

  defp append_loop([object | cont], n, hs) do
    [object | append_loop(cont, n + 1, hs)]
  end

  defp append_loop(f, 0, hs) do
    case f.() do
      [] ->
        append_loop(hs, 0)

      [object | cont] ->
        [object | append_loop(cont, 1, hs)]

      term ->
        term
    end
  end

  defp append_loop(f, _N, hs) do
    fn ->
      append_loop(f, 0, hs)
    end
  end

  defp no_dups([] = cont, uTab) do
    true = :ets.delete_all_objects(uTab)
    cont
  end

  defp no_dups([object | cont], uTab) do
    case :ets.member(uTab, object) do
      false ->
        true = :ets.insert(uTab, {object})

        [
          object
          | fn ->
              no_dups(cont, uTab)
            end
        ]

      true ->
        no_dups(cont, uTab)
    end
  end

  defp no_dups(f, uTab) do
    case f.() do
      objects when is_list(objects) ->
        no_dups(objects, uTab)

      term ->
        term
    end
  end

  defp cache(h, mTab, localPost) do
    case :ets.member(mTab, 0) do
      false ->
        true = :ets.insert(mTab, {0})
        cache(h, mTab, 1, localPost)

      true ->
        cache_recall(mTab, 1)
    end
  end

  defp cache([] = cont, _MTab, _SeqNo, localPost) do
    local_post(localPost)
    cont
  end

  defp cache([object | cont], mTab, seqNo, localPost) do
    true = :ets.insert(mTab, {seqNo, object})

    [
      object
      | fn ->
          cache(cont, mTab, seqNo + 1, localPost)
        end
    ]
  end

  defp cache(f, mTab, seqNo, localPost) do
    case f.() do
      objects when is_list(objects) ->
        cache(objects, mTab, seqNo, localPost)

      term ->
        term
    end
  end

  defp cache_recall(mTab, seqNo) do
    case :ets.lookup(mTab, seqNo) do
      [] = cont ->
        cont

      [{^seqNo, object}] ->
        [
          object
          | fn ->
              cache_recall(mTab, seqNo + 1)
            end
        ]
    end
  end

  defp ucache(h, uTab, mTab, localPost) do
    case :ets.member(mTab, 0) do
      false ->
        true = :ets.insert(mTab, {0})
        ucache(h, uTab, mTab, 1, localPost)

      true ->
        ucache_recall(uTab, mTab, 1)
    end
  end

  defp ucache([] = cont, _UTab, _MTab, _SeqNo, localPost) do
    local_post(localPost)
    cont
  end

  defp ucache([object | cont], uTab, mTab, seqNo, localPost) do
    hash = :erlang.phash2(object)

    case :ets.lookup(uTab, hash) do
      [] ->
        ucache3(object, cont, hash, uTab, mTab, seqNo, localPost)

      hashSeqObjects ->
        case :lists.keymember(object, 3, hashSeqObjects) do
          true ->
            ucache(cont, uTab, mTab, seqNo, localPost)

          false ->
            ucache3(object, cont, hash, uTab, mTab, seqNo, localPost)
        end
    end
  end

  defp ucache(f, uTab, mTab, seqNo, localPost) do
    case f.() do
      objects when is_list(objects) ->
        ucache(objects, uTab, mTab, seqNo, localPost)

      term ->
        term
    end
  end

  defp ucache3(object, cont, hash, uTab, mTab, seqNo, localPost) do
    true = :ets.insert(uTab, {hash, seqNo, object})
    true = :ets.insert(mTab, {seqNo, hash})

    [
      object
      | fn ->
          ucache(cont, uTab, mTab, seqNo + 1, localPost)
        end
    ]
  end

  defp ucache_recall(uTab, mTab, seqNo) do
    case :ets.lookup(mTab, seqNo) do
      [] = cont ->
        cont

      [{^seqNo, hash}] ->
        object =
          case :ets.lookup(uTab, hash) do
            [{^hash, ^seqNo, object0}] ->
              object0

            hashSeqObjects ->
              {^hash, ^seqNo, object0} = :lists.keyfind(seqNo, 2, hashSeqObjects)
              object0
          end

        [
          object
          | fn ->
              ucache_recall(uTab, mTab, seqNo + 1)
            end
        ]
    end
  end

  defp lcache(h, ref, localPost, tmpDir, maxList, tmpUsage) do
    key = {ref, :"$_qlc_cache_tmpfiles_"}

    case :erlang.get(key) do
      :undefined ->
        lcache1(h, {key, localPost, tmpDir, maxList, tmpUsage}, maxList, [])

      {:file, _Fd, _TmpFile, f} ->
        f.()

      l when is_list(l) ->
        l
    end
  end

  defp lcache1([] = cont, {key, localPost, _TmpDir, _MaxList, _TmpUsage}, _Sz, acc) do
    local_post(localPost)

    case :erlang.get(key) do
      :undefined ->
        :erlang.put(key, :lists.reverse(acc))
        cont

      {:file, fd, tmpFile, _F} ->
        case lcache_write(fd, tmpFile, acc) do
          :ok ->
            cont

          error ->
            error
        end
    end
  end

  defp lcache1(h, state, sz, acc) when sz < 0 do
    {key, localPost, tmpDir, maxList, tmpUsage} = state

    getFile =
      case :erlang.get(key) do
        {:file, fd0, tmpFile, _F} ->
          {tmpFile, fd0}

        :undefined when tmpUsage === :not_allowed ->
          :erlang.error({:tmpdir_usage, :caching})

        :undefined ->
          maybe_error_logger(tmpUsage, :caching)
          fName = tmp_filename(tmpDir)
          {f, fd0} = open_file(fName, [:write], localPost)
          :erlang.put(key, {:file, fd0, fName, f})
          {fName, fd0}
      end

    case getFile do
      {fileName, fd} ->
        case lcache_write(fd, fileName, acc) do
          :ok ->
            lcache1(h, state, maxList, [])

          error ->
            error
        end

      error ->
        error
    end
  end

  defp lcache1([object | cont], state, sz0, acc) do
    sz = decr_list_size(sz0, object)
    [object | lcache2(cont, state, sz, [object | acc])]
  end

  defp lcache1(f, state, sz, acc) do
    case f.() do
      objects when is_list(objects) ->
        lcache1(objects, state, sz, acc)

      term ->
        term
    end
  end

  defp lcache2([object | cont], state, sz0, acc)
       when sz0 >= 0 do
    sz = decr_list_size(sz0, object)
    [object | lcache2(cont, state, sz, [object | acc])]
  end

  defp lcache2(cont, state, sz, acc) do
    fn ->
      lcache1(cont, state, sz, acc)
    end
  end

  defp lcache_write(fd, fileName, l) do
    write_binary_terms(t2b(l, []), fd, fileName)
  end

  defp t2b([], bs) do
    bs
  end

  defp t2b([t | ts], bs) do
    t2b(ts, [:erlang.term_to_binary(t) | bs])
  end

  defp del_lcache(ref) do
    fn ->
      key = {ref, :"$_qlc_cache_tmpfiles_"}

      case :erlang.get(key) do
        :undefined ->
          :ok

        {:file, fd, tmpFile, _F} ->
          _ = :file.close(fd)
          _ = :file.delete(tmpFile)
          :erlang.erase(key)

        _L ->
          :erlang.erase(key)
      end
    end
  end

  defp tag_objects([object | cont], t) do
    [{object, t} | tag_objects2(cont, t + 1)]
  end

  defp tag_objects([] = cont, _T) do
    cont
  end

  defp tag_objects(f, t) do
    case f.() do
      objects when is_list(objects) ->
        tag_objects(objects, t)

      term ->
        term
    end
  end

  defp tag_objects2([object | cont], t) do
    [{object, t} | tag_objects2(cont, t + 1)]
  end

  defp tag_objects2(objects, t) do
    fn ->
      tag_objects(objects, t)
    end
  end

  defp untag_objects([] = objs) do
    objs
  end

  defp untag_objects([{object, _N} | cont]) do
    [object | untag_objects2(cont)]
  end

  defp untag_objects(f) do
    case f.() do
      objects when is_list(objects) ->
        untag_objects(objects)

      term ->
        term
    end
  end

  defp untag_objects2([{object, _N} | cont]) do
    [object | untag_objects2(cont)]
  end

  defp untag_objects2([] = cont) do
    cont
  end

  defp untag_objects2(objects) do
    fn ->
      untag_objects(objects)
    end
  end

  Record.defrecord(:r_m, :m,
    id: :undefined,
    tmpdir: :undefined,
    max_list: :undefined,
    tmp_usage: :undefined
  )

  defp merge_join([] = cont, _C1, _T2, _C2, _Opt) do
    cont
  end

  defp merge_join([e1 | l1], c1, l2, c2, opt) do
    r_qlc_opt(tmpdir: tmpDir, max_list: maxList, tmpdir_usage: tmpUsage) = opt
    m = r_m(id: merge_join_id(), tmpdir: tmpDir, max_list: maxList, tmp_usage: tmpUsage)
    merge_join2(e1, :erlang.element(c1, e1), l1, c1, l2, c2, m)
  end

  defp merge_join(f1, c1, l2, c2, opt) do
    case f1.() do
      l1 when is_list(l1) ->
        merge_join(l1, c1, l2, c2, opt)

      t1 ->
        t1
    end
  end

  defp merge_join1(_E2, _K2, [] = cont, _C1, _L2, _C2, m) do
    end_merge_join(cont, m)
  end

  defp merge_join1(e2, k2, [e1 | l1], c1, l2, c2, m) do
    k1 = :erlang.element(c1, e1)

    cond do
      k1 == k2 ->
        same_keys2(e1, k1, l1, c1, l2, c2, e2, m)

      k1 > k2 ->
        merge_join2(e1, k1, l1, c1, l2, c2, m)

      true ->
        merge_join1(e2, k2, l1, c1, l2, c2, m)
    end
  end

  defp merge_join1(e2, k2, f1, c1, l2, c2, m) do
    case f1.() do
      l1 when is_list(l1) ->
        merge_join1(e2, k2, l1, c1, l2, c2, m)

      t1 ->
        t1
    end
  end

  defp merge_join2(_E1, _K1, _L1, _C1, [] = cont, _C2, m) do
    end_merge_join(cont, m)
  end

  defp merge_join2(e1, k1, l1, c1, [e2 | l2], c2, m) do
    k2 = :erlang.element(c2, e2)

    cond do
      k1 == k2 ->
        same_keys2(e1, k1, l1, c1, l2, c2, e2, m)

      k1 > k2 ->
        merge_join2(e1, k1, l1, c1, l2, c2, m)

      true ->
        merge_join1(e2, k2, l1, c1, l2, c2, m)
    end
  end

  defp merge_join2(e1, k1, l1, c1, f2, c2, m) do
    case f2.() do
      l2 when is_list(l2) ->
        merge_join2(e1, k1, l1, c1, l2, c2, m)

      t2 ->
        t2
    end
  end

  defp same_keys2(e1, k1, l1, c1, [], _C2, e2_0, m) do
    cont = fn _L1b ->
      end_merge_join([], m)
    end

    loop_same_keys(e1, k1, l1, c1, [e2_0], cont, m)
  end

  defp same_keys2(e1, k1, l1, c1, [e2 | l2] = l2_0, c2, e2_0, m) do
    k2 = :erlang.element(c2, e2)

    cond do
      k1 == k2 ->
        same_keys1(e1, k1, l1, c1, e2, c2, e2_0, l2, m)

      k1 < k2 ->
        [
          [e1 | e2_0]
          | fn ->
              same_loop1(l1, k1, c1, e2_0, l2_0, c2, m)
            end
        ]
    end
  end

  defp same_keys2(e1, k1, l1, c1, f2, c2, e2_0, m) do
    case f2.() do
      l2 when is_list(l2) ->
        same_keys2(e1, k1, l1, c1, l2, c2, e2_0, m)

      t2 ->
        cont = fn _L1b ->
          t2
        end

        loop_same_keys(e1, k1, l1, c1, [e2_0], cont, m)
    end
  end

  defp same_loop1([], _K1_0, _C1, _E2_0, _L2, _C2, m) do
    end_merge_join([], m)
  end

  defp same_loop1([e1 | l1], k1_0, c1, e2_0, l2, c2, m) do
    k1 = :erlang.element(c1, e1)

    cond do
      k1 == k1_0 ->
        [
          [e1 | e2_0]
          | fn ->
              same_loop1(l1, k1_0, c1, e2_0, l2, c2, m)
            end
        ]

      k1_0 < k1 ->
        merge_join2(e1, k1, l1, c1, l2, c2, m)
    end
  end

  defp same_loop1(f1, k1_0, c1, e2_0, l2, c2, m) do
    case f1.() do
      l1 when is_list(l1) ->
        same_loop1(l1, k1_0, c1, e2_0, l2, c2, m)

      t1 ->
        t1
    end
  end

  defp same_keys1(e1_0, k1_0, [] = l1, c1, e2, c2, e2_0, l2, m) do
    [
      [[e1_0 | e2_0], [e1_0 | e2]]
      | fn ->
          same_keys(k1_0, e1_0, l1, c1, l2, c2, m)
        end
    ]
  end

  defp same_keys1(e1_0, k1_0, [e1 | _] = l1, c1, e2, c2, e2_0, l2, m) do
    k1 = :erlang.element(c1, e1)

    cond do
      k1_0 == k1 ->
        e2s = [e2, e2_0]
        sz0 = decr_list_size(r_m(m, :max_list), e2s)
        same_keys_cache(e1_0, k1_0, l1, c1, l2, c2, e2s, sz0, m)

      k1_0 < k1 ->
        [
          [[e1_0 | e2_0], [e1_0 | e2]]
          | fn ->
              same_keys(k1_0, e1_0, l1, c1, l2, c2, m)
            end
        ]
    end
  end

  defp same_keys1(e1_0, k1_0, f1, c1, e2, c2, e2_0, l2, m) do
    case f1.() do
      l1 when is_list(l1) ->
        same_keys1(e1_0, k1_0, l1, c1, e2, c2, e2_0, l2, m)

      t1 ->
        cont = fn ->
          t1
        end

        loop_same(e1_0, [e2, e2_0], cont)
    end
  end

  defp same_keys(_K1, _E1, _L1, _C1, [] = cont, _C2, m) do
    end_merge_join(cont, m)
  end

  defp same_keys(k1, e1, l1, c1, [e2 | l2], c2, m) do
    k2 = :erlang.element(c2, e2)

    cond do
      k1 == k2 ->
        [
          [e1 | e2]
          | fn ->
              same_keys(k1, e1, l1, c1, l2, c2, m)
            end
        ]

      k1 < k2 ->
        merge_join1(e2, k2, l1, c1, l2, c2, m)
    end
  end

  defp same_keys(k1, e1, l1, c1, f2, c2, m) do
    case f2.() do
      l2 when is_list(l2) ->
        same_keys(k1, e1, l1, c1, l2, c2, m)

      t2 ->
        t2
    end
  end

  defp same_keys_cache(e1, k1, l1, c1, [], _C2, e2s, _Sz, m) do
    cont = fn _L1b ->
      end_merge_join([], m)
    end

    loop_same_keys(e1, k1, l1, c1, e2s, cont, m)
  end

  defp same_keys_cache(e1, k1, l1, c1, l2, c2, e2s, sz0, m)
       when sz0 < 0 do
    case init_merge_join(m) do
      :ok ->
        sz = r_m(m, :max_list)

        c = fn ->
          same_keys_file(e1, k1, l1, c1, l2, c2, [], sz, m)
        end

        write_same_keys(e1, e2s, m, c)

      error ->
        error
    end
  end

  defp same_keys_cache(e1, k1, l1, c1, [e2 | l2], c2, e2s, sz0, m) do
    k2 = :erlang.element(c2, e2)

    cond do
      k1 == k2 ->
        sz = decr_list_size(sz0, e2)
        same_keys_cache(e1, k1, l1, c1, l2, c2, [e2 | e2s], sz, m)

      k1 < k2 ->
        cont = fn l1b ->
          merge_join1(e2, k2, l1b, c1, l2, c2, m)
        end

        loop_same_keys(e1, k1, l1, c1, e2s, cont, m)
    end
  end

  defp same_keys_cache(e1, k1, l1, c1, f2, c2, e2s, sz, m) do
    case f2.() do
      l2 when is_list(l2) ->
        same_keys_cache(e1, k1, l1, c1, l2, c2, e2s, sz, m)

      t2 ->
        cont = fn _L1b ->
          t2
        end

        loop_same_keys(e1, k1, l1, c1, e2s, cont, m)
    end
  end

  defp loop_same_keys(e1, _K1, [], _C1, e2s, _Cont, m) do
    end_merge_join(loop_same(e1, e2s, []), m)
  end

  defp loop_same_keys(e1, k1, l1, c1, e2s, cont, m) do
    loop_same(e1, e2s, fn ->
      loop_keys(k1, l1, c1, e2s, cont, m)
    end)
  end

  defp loop_same(_E1, [], l) do
    l
  end

  defp loop_same(e1, [e2 | e2s], l) do
    loop_same(e1, e2s, [[e1 | e2] | l])
  end

  defp loop_keys(k, [e1 | l1] = l1_0, c1, e2s, cont, m) do
    k1 = :erlang.element(c1, e1)

    cond do
      k1 == k ->
        loop_same_keys(e1, k1, l1, c1, e2s, cont, m)

      k1 > k ->
        cont.(l1_0)
    end
  end

  defp loop_keys(_K, [] = l1, _C1, _Es2, cont, _M) do
    cont.(l1)
  end

  defp loop_keys(k, f1, c1, e2s, cont, m) do
    case f1.() do
      l1 when is_list(l1) ->
        loop_keys(k, l1, c1, e2s, cont, m)

      t1 ->
        t1
    end
  end

  defp same_keys_file(e1, k1, l1, c1, [], _C2, e2s, _Sz, m) do
    cont = fn _L1b ->
      end_merge_join([], m)
    end

    same_keys_file_write(e1, k1, l1, c1, e2s, m, cont)
  end

  defp same_keys_file(e1, k1, l1, c1, l2, c2, e2s, sz0, m)
       when sz0 < 0 do
    sz = r_m(m, :max_list)

    c = fn ->
      same_keys_file(e1, k1, l1, c1, l2, c2, [], sz, m)
    end

    write_same_keys(e1, e2s, m, c)
  end

  defp same_keys_file(e1, k1, l1, c1, [e2 | l2], c2, e2s, sz0, m) do
    k2 = :erlang.element(c2, e2)

    cond do
      k1 == k2 ->
        sz = decr_list_size(sz0, e2)
        same_keys_file(e1, k1, l1, c1, l2, c2, [e2 | e2s], sz, m)

      k1 < k2 ->
        cont = fn l1b ->
          merge_join1(e2, k2, l1b, c1, l2, c2, m)
        end

        same_keys_file_write(e1, k1, l1, c1, e2s, m, cont)
    end
  end

  defp same_keys_file(e1, k1, l1, c1, f2, c2, e2s, sz, m) do
    case f2.() do
      l2 when is_list(l2) ->
        same_keys_file(e1, k1, l1, c1, l2, c2, e2s, sz, m)

      t2 ->
        cont = fn _L1b ->
          t2
        end

        same_keys_file_write(e1, k1, l1, c1, e2s, m, cont)
    end
  end

  defp same_keys_file_write(e1, k1, l1, c1, e2s, m, cont) do
    c = fn ->
      loop_keys_file(k1, l1, c1, cont, m)
    end

    write_same_keys(e1, e2s, m, c)
  end

  defp write_same_keys(_E1, [], _M, cont) do
    cont.()
  end

  defp write_same_keys(e1, es2, m, cont) do
    write_same_keys(e1, es2, m, [], cont)
  end

  defp write_same_keys(_E1, [], m, e2s, objs) do
    case write_merge_join(m, e2s) do
      :ok ->
        objs

      error ->
        error
    end
  end

  defp write_same_keys(e1, [e2 | e2s0], m, e2s, objs) do
    bE2 = :erlang.term_to_binary(e2)
    write_same_keys(e1, e2s0, m, [bE2 | e2s], [[e1 | e2] | objs])
  end

  defp loop_keys_file(k, [e1 | l1] = l1_0, c1, cont, m) do
    k1 = :erlang.element(c1, e1)

    cond do
      k1 == k ->
        c = fn ->
          loop_keys_file(k1, l1, c1, cont, m)
        end

        read_merge_join(m, e1, c)

      k1 > k ->
        cont.(l1_0)
    end
  end

  defp loop_keys_file(_K, [] = l1, _C1, cont, _M) do
    cont.(l1)
  end

  defp loop_keys_file(k, f1, c1, cont, m) do
    case f1.() do
      l1 when is_list(l1) ->
        loop_keys_file(k, l1, c1, cont, m)

      t1 ->
        t1
    end
  end

  defp end_merge_join(reply, m) do
    end_merge_join(m)
    reply
  end

  defp init_merge_join(r_m(id: mergeId, tmpdir: tmpDir, tmp_usage: tmpUsage)) do
    case tmp_merge_file(mergeId) do
      {fd, fileName} ->
        case :file.position(fd, :bof) do
          {:ok, 0} ->
            case :file.truncate(fd) do
              :ok ->
                :ok

              error ->
                file_error(fileName, error)
            end

          error ->
            file_error(fileName, error)
        end

      :none when tmpUsage === :not_allowed ->
        :erlang.error({:tmpdir_usage, :joining})

      :none ->
        maybe_error_logger(tmpUsage, :joining)
        fName = tmp_filename(tmpDir)

        case :file.open(
               fName,
               [:raw, :binary, :read, :write]
             ) do
          {:ok, fd} ->
            tmpFiles = :erlang.get(:"$_qlc_merge_join_tmpfiles_")
            :erlang.put(:"$_qlc_merge_join_tmpfiles_", [{mergeId, fd, fName} | tmpFiles])
            :ok

          error ->
            file_error(fName, error)
        end
    end
  end

  defp write_merge_join(r_m(id: mergeId), bTerms) do
    {fd, fileName} = tmp_merge_file(mergeId)
    write_binary_terms(bTerms, fd, fileName)
  end

  defp read_merge_join(r_m(id: mergeId), e1, cont) do
    {fd, fileName} = tmp_merge_file(mergeId)

    case :file.position(fd, :bof) do
      {:ok, 0} ->
        fun = fn
          [], _ ->
            cont.()

          ts, c when is_list(ts) ->
            join_read_terms(e1, ts, c)
        end

        file_loop_read(<<>>, 64 * 1024, {fd, fileName}, fun)

      error ->
        file_error(fileName, error)
    end
  end

  defp join_read_terms(_E1, [], objs) do
    objs
  end

  defp join_read_terms(e1, [e2 | e2s], objs) do
    join_read_terms(e1, e2s, [[e1 | e2] | objs])
  end

  defp end_merge_join(r_m(id: mergeId)) do
    case tmp_merge_file(mergeId) do
      :none ->
        :ok

      {fd, fileName} ->
        _ = :file.close(fd)
        _ = :file.delete(fileName)

        :erlang.put(
          :"$_qlc_merge_join_tmpfiles_",
          :lists.keydelete(mergeId, 1, :erlang.get(:"$_qlc_merge_join_tmpfiles_"))
        )
    end
  end

  defp end_all_merge_joins() do
    :lists.foreach(
      fn id ->
        end_merge_join(r_m(id: id))
      end,
      for {id, _Fd, _FileName} <- :lists.flatten([:erlang.get(:"$_qlc_merge_join_tmpfiles_")]) do
        id
      end
    )

    :erlang.erase(:"$_qlc_merge_join_tmpfiles_")
  end

  defp merge_join_id() do
    case :erlang.get(:"$_qlc_merge_join_tmpfiles_") do
      :undefined ->
        :erlang.put(:"$_qlc_merge_join_tmpfiles_", [])

      _ ->
        :ok
    end

    make_ref()
  end

  defp tmp_merge_file(mergeId) do
    tmpFiles = :erlang.get(:"$_qlc_merge_join_tmpfiles_")

    case :lists.keyfind(mergeId, 1, tmpFiles) do
      {^mergeId, fd, fileName} ->
        {fd, fileName}

      false ->
        :none
    end
  end

  defp decr_list_size(sz0, e) when is_integer(sz0) do
    sz0 - :erlang.external_size(e)
  end

  defp lookup_join([e1 | l1], c1, luF, c2, rev) do
    k1 = :erlang.element(c1, e1)

    case luF.(c2, [k1]) do
      [] ->
        lookup_join(l1, c1, luF, c2, rev)

      [e2] when rev ->
        [
          [e2 | e1]
          | fn ->
              lookup_join(l1, c1, luF, c2, rev)
            end
        ]

      [e2] ->
        [
          [e1 | e2]
          | fn ->
              lookup_join(l1, c1, luF, c2, rev)
            end
        ]

      e2s when is_list(e2s) and rev ->
        for e2 <- e2s do
          [e2 | e1]
        end ++
          fn ->
            lookup_join(l1, c1, luF, c2, rev)
          end

      e2s when is_list(e2s) ->
        for e2 <- e2s do
          [e1 | e2]
        end ++
          fn ->
            lookup_join(l1, c1, luF, c2, rev)
          end

      term ->
        term
    end
  end

  defp lookup_join([] = cont, _C1, _LuF, _C2, _Rev) do
    cont
  end

  defp lookup_join(f1, c1, luF, c2, rev) do
    case f1.() do
      l1 when is_list(l1) ->
        lookup_join(l1, c1, luF, c2, rev)

      t1 ->
        t1
    end
  end

  defp maybe_error_logger(:allowed, _) do
    :ok
  end

  defp maybe_error_logger(name, why) do
    [
      [_, _, {:qlc, :maybe_error_logger, _, _}]
      | stacktrace
    ] = expand_stacktrace()

    trimmer = fn m, _F, _A ->
      m === :erl_eval
    end

    formater = fn term, i ->
      :io_lib.print(term, i, 80, -1)
    end

    x = :erl_error.format_stacktrace(1, stacktrace, trimmer, formater)

    apply(:error_logger, name, [
      'qlc: temporary file was needed for ~w\n~ts\n',
      [why, :lists.flatten(x)]
    ])
  end

  defp expand_stacktrace() do
    d = :erlang.system_flag(:backtrace_depth, 8)

    try do
      expand_stacktrace(:erlang.max(1, d))
    after
      :erlang.system_flag(:backtrace_depth, d)
    end
  end

  defp expand_stacktrace(d) do
    _ = :erlang.system_flag(:backtrace_depth, d)

    {:EXIT, {:foo, stacktrace}} =
      try do
        :erlang.error(:foo)
      catch
        :error, e -> {:EXIT, {e, __STACKTRACE__}}
        :exit, e -> {:EXIT, e}
        e -> e
      end

    l =
      :lists.takewhile(
        fn {m, _, _, _} ->
          m !== :qlc
        end,
        :lists.reverse(stacktrace)
      )

    cond do
      length(l) < 3 and length(stacktrace) === d ->
        expand_stacktrace(d + 5)

      true ->
        stacktrace
    end
  end

  defp write_binary_terms(bTerms, fd, fileName) do
    case :file.write(fd, size_bin(bTerms, [])) do
      :ok ->
        :ok

      error ->
        file_error(fileName, error)
    end
  end

  defp post_funs(l) do
    end_all_merge_joins()
    local_post(l)
  end

  defp local_post(l) do
    :lists.foreach(
      fn
        :undefined ->
          :ok

        f ->
          try do
            f.()
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end
      end,
      l
    )
  end

  defp call(:undefined, _Arg, default, _Post) do
    default
  end

  defp call(fun, arg, _Default, post) do
    try do
      fun.(arg)
    catch
      class, reason ->
        post_funs(post)
        :erlang.raise(class, reason, __STACKTRACE__)
    end
  end

  defp grd(:undefined, _Arg) do
    false
  end

  defp grd(fun, arg) do
    case fun.(arg) do
      true ->
        true

      _ ->
        false
    end
  end

  defp anno0() do
    anno(0)
  end

  defp anno1() do
    anno(1)
  end

  defp anno(l) do
    :erl_anno.new(l)
  end

  defp family(l) do
    :sofs.to_external(:sofs.relation_to_family(:sofs.relation(l)))
  end

  defp family_union(l) do
    r = :sofs.relation(l, [{:atom, [:atom]}])
    :sofs.to_external(:sofs.family_union(:sofs.relation_to_family(r)))
  end

  defp file_error(file, {:error, reason}) do
    :erlang.error({:file_error, file, reason})
  end

  defp throw_file_error(file, {:error, reason}) do
    throw_reason({:file_error, file, reason})
  end

  defp throw_reason(reason) do
    throw_error(:erlang.error(reason))
  end

  defp throw_error(error) do
    throw(error)
  end

  defp error(reason) do
    {:error, :qlc, reason}
  end
end
