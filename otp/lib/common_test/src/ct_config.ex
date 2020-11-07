defmodule :m_ct_config do
  use Bitwise
  require Record

  Record.defrecord(:r_conn, :conn,
    handle: :undefined,
    targetref: :undefined,
    address: :undefined,
    callback: :undefined
  )

  Record.defrecord(:r_testspec, :testspec,
    spec_dir: :undefined,
    nodes: [],
    init: [],
    label: [],
    profile: [],
    logdir: ['.'],
    logopts: [],
    basic_html: [],
    esc_chars: [],
    verbosity: [],
    silent_connections: [],
    cover: [],
    cover_stop: [],
    config: [],
    userconfig: [],
    event_handler: [],
    ct_hooks: [],
    enable_builtin_hooks: true,
    release_shell: false,
    include: [],
    auto_compile: [],
    abort_if_missing_suites: [],
    stylesheet: [],
    multiply_timetraps: [],
    scale_timetraps: [],
    create_priv_dir: [],
    alias: [],
    tests: [],
    unknown: [],
    merge_tests: true
  )

  Record.defrecord(:r_cover, :cover,
    app: :none,
    local_only: false,
    level: :details,
    excl_mods: [],
    incl_mods: [],
    cross: [],
    src: []
  )

  Record.defrecord(:r_conn_log, :conn_log,
    header: true,
    client: :undefined,
    name: :undefined,
    address: :undefined,
    conn_pid: :undefined,
    action: :undefined,
    module: :undefined
  )

  Record.defrecord(:r_ct_conf, :ct_conf,
    key: :undefined,
    value: :undefined,
    handler: :undefined,
    config: :undefined,
    name: :_UNDEF,
    default: false
  )

  def start(mode) do
    case :erlang.whereis(:ct_config_server) do
      :undefined ->
        me = self()

        pid =
          spawn_link(fn ->
            do_start(me)
          end)

        receive do
          {^pid, :started} ->
            pid

          {^pid, error} ->
            exit(error)
        end

      pid ->
        case :ct_util.get_mode() do
          :interactive when mode == :interactive ->
            pid

          :interactive ->
            {:error, :interactive_mode}

          _OtherMode ->
            pid
        end
    end
  end

  defp do_start(parent) do
    :erlang.process_flag(:trap_exit, true)
    :ct_util.mark_process()
    :erlang.register(:ct_config_server, self())
    :ct_util.create_table(:ct_attributes, :bag, r_ct_conf(:key))
    {:ok, startDir} = :file.get_cwd()

    opts =
      case :ct_util.read_opts() do
        {:ok, opts1} ->
          opts1

        error ->
          send(parent, {self(), error})
          exit(error)
      end

    case read_config_files(opts) do
      :ok ->
        send(parent, {self(), :started})
        loop(startDir)

      readError ->
        send(parent, {self(), readError})
        exit(readError)
    end
  end

  def stop() do
    case :erlang.whereis(:ct_config_server) do
      :undefined ->
        :ok

      _ ->
        call({:stop})
    end
  end

  defp call(msg) do
    mRef =
      :erlang.monitor(
        :process,
        :erlang.whereis(:ct_config_server)
      )

    ref = make_ref()
    send(:ct_config_server, {msg, {self(), ref}})

    receive do
      {^ref, result} ->
        :erlang.demonitor(mRef, [:flush])
        result

      {:DOWN, ^mRef, :process, _, reason} ->
        {:error, {:ct_util_server_down, reason}}
    end
  end

  defp return({to, ref}, result) do
    send(to, {ref, result})
    :ok
  end

  defp loop(startDir) do
    receive do
      {{:require, name, key}, from} ->
        result = do_require(name, key)
        return(from, result)
        loop(startDir)

      {{:set_default_config, {config, scope}}, from} ->
        _ = set_config(config, {true, scope})
        return(from, :ok)
        loop(startDir)

      {{:set_default_config, {name, config, scope}}, from} ->
        _ = set_config(name, config, {true, scope})
        return(from, :ok)
        loop(startDir)

      {{:delete_default_config, scope}, from} ->
        delete_config({true, scope})
        return(from, :ok)
        loop(startDir)

      {{:update_config, {name, newConfig}}, from} ->
        update_conf(name, newConfig)
        return(from, :ok)
        loop(startDir)

      {{:reload_config, keyOrName}, from} ->
        newValue = reload_conf(keyOrName)
        return(from, newValue)
        loop(startDir)

      {{:stop}, from} ->
        :ets.delete(:ct_attributes)
        :ok = :file.set_cwd(startDir)
        return(from, :ok)
    end
  end

  def set_default_config(newConfig, scope) do
    call({:set_default_config, {newConfig, scope}})
  end

  def set_default_config(name, newConfig, scope) do
    call({:set_default_config, {name, newConfig, scope}})
  end

  def delete_default_config(scope) do
    call({:delete_default_config, scope})
  end

  def update_config(name, config) do
    call({:update_config, {name, config}})
  end

  def reload_config(keyOrName) do
    call({:reload_config, keyOrName})
  end

  defp process_default_configs(opts) do
    :lists.flatmap(
      fn
        {:config, [_ | _] = fileOrFiles} ->
          case {:io_lib.printable_unicode_list(fileOrFiles),
                :io_lib.printable_unicode_list(hd(fileOrFiles))} do
            {false, true} ->
              fileOrFiles

            {true, false} ->
              [fileOrFiles]

            _ ->
              []
          end

        _ ->
          []
      end,
      opts
    )
  end

  defp process_user_configs(opts, acc) do
    case :lists.keytake(:userconfig, 1, opts) do
      false ->
        :lists.reverse(acc)

      {:value, {:userconfig, config = [{_, _} | _]}, newOpts} ->
        acc1 =
          :lists.map(
            fn
              {_Callback, []} = cfg ->
                cfg

              {callback, files = [file | _]} when is_list(file) ->
                {callback, files}

              {callback, file = [c | _]} when is_integer(c) ->
                {callback, [file]}
            end,
            config
          )

        process_user_configs(
          newOpts,
          :lists.reverse(acc1) ++ acc
        )

      {:value, {:userconfig, {callback, []}}, newOpts} ->
        process_user_configs(newOpts, [{callback, []} | acc])

      {:value, {:userconfig, {callback, files = [file | _]}}, newOpts}
      when is_list(file) ->
        process_user_configs(newOpts, [{callback, files} | acc])

      {:value, {:userconfig, {callback, file = [c | _]}}, newOpts}
      when is_integer(c) ->
        process_user_configs(
          newOpts,
          [{callback, [file]} | acc]
        )
    end
  end

  def get_config_file_list(opts) do
    defaultConfigs = process_default_configs(opts)

    cfgFiles =
      cond do
        defaultConfigs == [] ->
          []

        true ->
          [{:ct_config_plain, defaultConfigs}]
      end ++ process_user_configs(opts, [])

    cfgFiles
  end

  def add_default_callback(opts) do
    case :lists.keytake(:config, 1, opts) do
      {:value, {:config, [file | _] = files}, noConfigOpts}
      when is_integer(file) !== true ->
        [
          {:config, :lists.flatmap(&add_def_cb/1, files)}
          | noConfigOpts
        ]

      {:value, {:config, file}, noConfigOpts} ->
        [{:config, add_def_cb(file)} | noConfigOpts]

      false ->
        opts
    end
  end

  defp add_def_cb([]) do
    []
  end

  defp add_def_cb(config) when is_tuple(config) do
    [config]
  end

  defp add_def_cb([h | _T] = config) when is_integer(h) do
    [{:ct_config_plain, [config]}]
  end

  def read_config_files(opts) do
    addCallback = fn
      callBack, [] ->
        [{callBack, []}]

      callBack, [f | _] = files when is_integer(f) ->
        [{callBack, files}]

      callBack, [f | _] = files when is_list(f) ->
        :lists.map(
          fn x ->
            {callBack, x}
          end,
          files
        )
    end

    configFiles =
      case :lists.keyfind(:config, 1, opts) do
        {:config, configLists} ->
          :lists.foldr(
            fn {callback, files}, acc ->
              addCallback.(callback, files) ++ acc
            end,
            [],
            configLists
          )

        false ->
          []
      end

    read_config_files_int(configFiles, &store_config/3)
  end

  defp read_config_files_int([{callback, file} | files], funToSave) do
    case callback.read_config(file) do
      {:ok, config} ->
        _ = funToSave.(config, callback, file)
        read_config_files_int(files, funToSave)

      {:error, {errorName, errorDetail}} ->
        {:user_error, {errorName, file, errorDetail}}

      {:error, errorName, errorDetail} ->
        {:user_error, {errorName, file, errorDetail}}
    end
  end

  defp read_config_files_int([], _FunToSave) do
    :ok
  end

  defp read_config_files(configFiles, funToSave) do
    case read_config_files_int(configFiles, funToSave) do
      {:user_error, error} ->
        {:error, error}

      :ok ->
        :ok
    end
  end

  defp store_config(config, callback, file) when is_tuple(config) do
    store_config([config], callback, file)
  end

  defp store_config(config, callback, file) when is_list(config) do
    for {key, val} <- config do
      :ets.insert(
        :ct_attributes,
        r_ct_conf(key: key, value: val, handler: callback, config: file, default: false)
      )
    end
  end

  defp keyfindall(key, pos, list) do
    for e <- list, :erlang.element(pos, e) === key do
      e
    end
  end

  defp rewrite_config(config, callback, file) do
    oldRows =
      :ets.match_object(
        :ct_attributes,
        r_ct_conf(handler: callback, config: file, _: :_)
      )

    :ets.match_delete(
      :ct_attributes,
      r_ct_conf(handler: callback, config: file, _: :_)
    )

    updater = fn {key, value} ->
      case keyfindall(key, r_ct_conf(:key), oldRows) do
        [] ->
          :ets.insert(
            :ct_attributes,
            r_ct_conf(key: key, value: value, handler: callback, config: file)
          )

        rowsToUpdate ->
          inserter = fn row ->
            :ets.insert(
              :ct_attributes,
              r_ct_conf(row, value: value)
            )
          end

          :lists.foreach(inserter, rowsToUpdate)
      end
    end

    for {key, value} <- config do
      updater.({key, value})
    end
  end

  defp set_config(config, default) do
    set_config(:_UNDEF, config, default)
  end

  defp set_config(name, config, default) do
    for {key, val} <- config do
      :ets.insert(
        :ct_attributes,
        r_ct_conf(key: key, value: val, name: name, default: default)
      )
    end
  end

  def get_config(keyOrName) do
    get_config(keyOrName, :undefined, [])
  end

  def get_config(keyOrName, default) do
    get_config(keyOrName, default, [])
  end

  def get_config(keyOrName, default, opts)
      when is_atom(keyOrName) do
    case get_config({keyOrName}, default, opts) do
      {{^keyOrName}, val} ->
        {keyOrName, val}

      [{{^keyOrName}, _Val} | _] = res ->
        for {{k}, val} <- res, k == keyOrName do
          {k, val}
        end

      else__ ->
        else__
    end
  end

  def get_config({deepKey, subKey}, default, opts)
      when is_tuple(deepKey) do
    get_config(:erlang.append_element(deepKey, subKey), default, opts)
  end

  def get_config(keyOrName, default, opts)
      when is_tuple(keyOrName) do
    case lookup_config(:erlang.element(1, keyOrName)) do
      [] ->
        format_value([default], keyOrName, opts)

      vals ->
        newVals =
          :lists.map(
            fn {val} ->
              get_config(tl(:erlang.tuple_to_list(keyOrName)), val, default, opts)
            end,
            vals
          )

        format_value(newVals, keyOrName, opts)
    end
  end

  defp get_config([], vals, _Default, _Opts) do
    vals
  end

  defp get_config([[]], vals, default, opts) do
    get_config([], vals, default, opts)
  end

  defp get_config([subKeys], vals, default, _Opts)
       when is_list(subKeys) do
    case do_get_config(subKeys, vals, []) do
      {:ok, subVals} ->
        for {_, subVal} <- subVals do
          subVal
        end

      _ ->
        default
    end
  end

  defp get_config([key | rest], vals, default, opts) do
    case do_get_config([key], vals, []) do
      {:ok, [{^key, newVals}]} ->
        get_config(rest, newVals, default, opts)

      _ ->
        default
    end
  end

  defp do_get_config([key | _], available, _Mapped) when not is_list(available) do
    {:error, {:not_available, key}}
  end

  defp do_get_config([key | required], available, mapped) do
    case :lists.keysearch(key, 1, available) do
      {:value, {^key, value}} ->
        newAvailable = :lists.keydelete(key, 1, available)
        newMapped = [{key, value} | mapped]
        do_get_config(required, newAvailable, newMapped)

      false ->
        {:error, {:not_available, key}}
    end
  end

  defp do_get_config([], _Available, mapped) do
    {:ok, :lists.reverse(mapped)}
  end

  def get_all_config() do
    :ets.select(
      :ct_attributes,
      [
        {r_ct_conf(name: :"$1", key: :"$2", value: :"$3", default: :"$4", _: :_), [],
         [{{:"$1", :"$2", :"$3", :"$4"}}]}
      ]
    )
  end

  defp lookup_config(keyOrName) do
    case lookup_name(keyOrName) do
      [] ->
        lookup_key(keyOrName)

      values ->
        values
    end
  end

  defp lookup_name(name) do
    :ets.select(
      :ct_attributes,
      [{r_ct_conf(value: :"$1", name: name, _: :_), [], [{{:"$1"}}]}]
    )
  end

  defp lookup_key(key) do
    :ets.select(
      :ct_attributes,
      [{r_ct_conf(key: key, value: :"$1", name: :_UNDEF, _: :_), [], [{{:"$1"}}]}]
    )
  end

  defp format_value([subVal | _] = subVals, keyOrName, opts) do
    case {:lists.member(:all, opts), :lists.member(:element, opts)} do
      {true, true} ->
        for val <- subVals do
          {keyOrName, val}
        end

      {true, false} ->
        for val <- subVals do
          val
        end

      {false, true} ->
        {keyOrName, subVal}

      {false, false} ->
        subVal
    end
  end

  defp lookup_handler_for_config({key, _Subkey}) do
    lookup_handler_for_config(key)
  end

  defp lookup_handler_for_config(keyOrName) do
    case lookup_handler_for_name(keyOrName) do
      [] ->
        lookup_handler_for_key(keyOrName)

      values ->
        values
    end
  end

  defp lookup_handler_for_name(name) do
    :ets.select(
      :ct_attributes,
      [{r_ct_conf(handler: :"$1", config: :"$2", name: name, _: :_), [], [{{:"$1", :"$2"}}]}]
    )
  end

  defp lookup_handler_for_key(key) do
    :ets.select(
      :ct_attributes,
      [{r_ct_conf(handler: :"$1", config: :"$2", key: key, _: :_), [], [{{:"$1", :"$2"}}]}]
    )
  end

  defp update_conf(name, newConfig) do
    old =
      :ets.select(
        :ct_attributes,
        [{r_ct_conf(name: name, _: :_), [], [:"$_"]}]
      )

    :lists.foreach(
      fn oldElem ->
        newElem = r_ct_conf(oldElem, value: newConfig)
        :ets.delete_object(:ct_attributes, oldElem)
        :ets.insert(:ct_attributes, newElem)
      end,
      old
    )

    :ok
  end

  defp reload_conf(keyOrName) do
    case lookup_handler_for_config(keyOrName) do
      [] ->
        :undefined

      handlerList ->
        handlerList2 = :lists.usort(handlerList)

        case read_config_files(
               handlerList2,
               &rewrite_config/3
             ) do
          :ok ->
            get_config(keyOrName)

          error ->
            error
        end
    end
  end

  def release_allocated() do
    allocated =
      :ets.select(
        :ct_attributes,
        [{r_ct_conf(name: :"$1", _: :_), [{:"=/=", :"$1", :_UNDEF}], [:"$_"]}]
      )

    release_allocated(allocated)
  end

  defp release_allocated([h | t]) do
    :ets.delete_object(:ct_attributes, h)
    :ets.insert(:ct_attributes, r_ct_conf(h, name: :_UNDEF))
    release_allocated(t)
  end

  defp release_allocated([]) do
    :ok
  end

  defp allocate(name, key) do
    ref = make_ref()

    case get_config(key, ref, [:all, :element]) do
      [{_, ^ref}] ->
        {:error, {:not_available, key}}

      configs ->
        associate(name, key, configs)
        :ok
    end
  end

  defp associate(:_UNDEF, _Key, _Configs) do
    :ok
  end

  defp associate(name, {key, subKeys}, configs)
       when is_atom(key) and is_list(subKeys) do
    associate_int(name, configs, 'true')
  end

  defp associate(name, _Key, configs) do
    associate_int(name, configs, :os.getenv('COMMON_TEST_ALIAS_TOP'))
  end

  defp associate_int(name, configs, 'true') do
    :lists.foreach(
      fn {k, _Config} ->
        cs =
          :ets.match_object(
            :ct_attributes,
            r_ct_conf(key: :erlang.element(1, k), name: :_UNDEF, _: :_)
          )

        for c <- cs do
          :ets.insert(:ct_attributes, r_ct_conf(c, name: name))
        end
      end,
      configs
    )
  end

  defp associate_int(name, configs, _) do
    :lists.foreach(
      fn {k, config} ->
        key =
          cond do
            is_tuple(k) ->
              :erlang.element(1, k)

            is_atom(k) ->
              k
          end

        cs =
          :ets.match_object(
            :ct_attributes,
            r_ct_conf(key: key, name: :_UNDEF, _: :_)
          )

        for c <- cs do
          :ets.insert(
            :ct_attributes,
            r_ct_conf(c, name: name, value: config)
          )
        end
      end,
      configs
    )
  end

  defp delete_config(default) do
    :ets.match_delete(
      :ct_attributes,
      r_ct_conf(default: default, _: :_)
    )

    :ok
  end

  def require(key) when is_atom(key) or is_tuple(key) do
    allocate(:_UNDEF, key)
  end

  def require(key) do
    {:error, {:invalid, key}}
  end

  def require(name, key)
      when (is_atom(name) and
              is_atom(key)) or is_tuple(key) do
    call({:require, name, key})
  end

  def require(name, keys) do
    {:error, {:invalid, {name, keys}}}
  end

  defp do_require(name, key) do
    case get_key_from_name(name) do
      {:error, _} ->
        allocate(name, key)

      {:ok, nameKey}
      when nameKey == key or
             (is_tuple(key) and :erlang.element(1, key) == nameKey) ->
        r = make_ref()

        case get_config(key, r, []) do
          ^r ->
            {:error, {:not_available, key}}

          {:error, _} = error ->
            error

          _Error ->
            :ok
        end

      {:ok, otherKey} ->
        {:error, {:name_in_use, name, otherKey}}
    end
  end

  def encrypt_config_file(srcFileName, encryptFileName) do
    case get_crypt_key_from_file() do
      {:error, _} = e ->
        e

      key ->
        encrypt_config_file(srcFileName, encryptFileName, {:key, key})
    end
  end

  def get_key_from_name(name) do
    case :ets.select(
           :ct_attributes,
           [{r_ct_conf(name: name, key: :"$1", _: :_), [], [:"$1"]}]
         ) do
      [key | _] ->
        {:ok, key}

      _ ->
        {:error, {:no_such_name, name}}
    end
  end

  def encrypt_config_file(srcFileName, encryptFileName, {:file, keyFile}) do
    case get_crypt_key_from_file(keyFile) do
      {:error, _} = e ->
        e

      key ->
        encrypt_config_file(srcFileName, encryptFileName, {:key, key})
    end
  end

  def encrypt_config_file(srcFileName, encryptFileName, {:key, key}) do
    _ = :crypto.start()
    {cryptoKey, iVec} = make_crypto_key(key)

    case :file.read_file(srcFileName) do
      {:ok, bin0} ->
        bin1 = :erlang.term_to_binary({srcFileName, bin0})

        bin2 =
          case rem(byte_size(bin1), 8) do
            0 ->
              bin1

            n ->
              :erlang.list_to_binary([bin1, random_bytes(8 - n)])
          end

        encBin = :crypto.crypto_one_time(:des_ede3_cbc, cryptoKey, iVec, bin2, true)

        case :file.write_file(encryptFileName, encBin) do
          :ok ->
            :io.format('~ts --(encrypt)--> ~ts~n', [srcFileName, encryptFileName])
            :ok

          {:error, reason} ->
            {:error, {reason, encryptFileName}}
        end

      {:error, reason} ->
        {:error, {reason, srcFileName}}
    end
  end

  def decrypt_config_file(encryptFileName, targetFileName) do
    case get_crypt_key_from_file() do
      {:error, _} = e ->
        e

      key ->
        decrypt_config_file(encryptFileName, targetFileName, {:key, key})
    end
  end

  def decrypt_config_file(encryptFileName, targetFileName, {:file, keyFile}) do
    case get_crypt_key_from_file(keyFile) do
      {:error, _} = e ->
        e

      key ->
        decrypt_config_file(encryptFileName, targetFileName, {:key, key})
    end
  end

  def decrypt_config_file(encryptFileName, targetFileName, {:key, key}) do
    _ = :crypto.start()
    {cryptoKey, iVec} = make_crypto_key(key)

    case :file.read_file(encryptFileName) do
      {:ok, bin} ->
        decBin = :crypto.crypto_one_time(:des_ede3_cbc, cryptoKey, iVec, bin, false)

        case (try do
                :erlang.binary_to_term(decBin)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            {:error, :bad_file}

          {_SrcFile, srcBin} ->
            case targetFileName do
              :undefined ->
                {:ok, srcBin}

              _ ->
                case :file.write_file(targetFileName, srcBin) do
                  :ok ->
                    :io.format('~ts --(decrypt)--> ~ts~n', [encryptFileName, targetFileName])
                    :ok

                  {:error, reason} ->
                    {:error, {reason, targetFileName}}
                end
            end
        end

      {:error, reason} ->
        {:error, {reason, encryptFileName}}
    end
  end

  def get_crypt_key_from_file(file) do
    case :file.read_file(file) do
      {:ok, bin} ->
        case (try do
                :string.lexemes(
                  :erlang.binary_to_list(bin),
                  [?\n, [?\r, ?\n]]
                )
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          [key] ->
            key

          _ ->
            {:error, {:bad_crypt_file, file}}
        end

      {:error, reason} ->
        {:error, {reason, file}}
    end
  end

  def get_crypt_key_from_file() do
    cwdFile = :filename.join('.', '.ct_config.crypt')

    {result, fullName} =
      case :file.read_file(cwdFile) do
        {:ok, bin} ->
          {bin, cwdFile}

        _ ->
          case :init.get_argument(:home) do
            {:ok, [[home]]} ->
              homeFile = :filename.join(home, '.ct_config.crypt')

              case :file.read_file(homeFile) do
                {:ok, bin} ->
                  {bin, homeFile}

                _ ->
                  {{:error, :no_crypt_file}, :noent}
              end

            _ ->
              {{:error, :no_crypt_file}, :noent}
          end
      end

    case fullName do
      :noent ->
        result

      _ ->
        case (try do
                :string.lexemes(
                  :erlang.binary_to_list(result),
                  [?\n, [?\r, ?\n]]
                )
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          [key] ->
            :io.format('~nCrypt key file: ~ts~n', [fullName])
            key

          _ ->
            {:error, {:bad_crypt_file, fullName}}
        end
    end
  end

  defp make_crypto_key(string) do
    <<k1::size(8)-binary, k2::size(8)-binary>> = first = :erlang.md5(string)

    <<k3::size(8)-binary, iVec::size(8)-binary>> =
      :erlang.md5([
        first
        | :lists.reverse(string)
      ])

    key = <<k1::binary, k2::binary, k3::binary>>
    {key, iVec}
  end

  defp random_bytes(n) do
    random_bytes_1(n, [])
  end

  defp random_bytes_1(0, acc) do
    acc
  end

  defp random_bytes_1(n, acc) do
    random_bytes_1(n - 1, [:rand.uniform(255) | acc])
  end

  defp check_callback_load(callback) do
    case :code.is_loaded(callback) do
      {:file, _Filename} ->
        check_exports(callback)

      false ->
        case :code.load_file(callback) do
          {:module, ^callback} ->
            check_exports(callback)

          {:error, error} ->
            {:error, error}
        end
    end
  end

  defp check_exports(callback) do
    fs = callback.module_info(:exports)

    case {:lists.member({:check_parameter, 1}, fs), :lists.member({:read_config, 1}, fs)} do
      {true, true} ->
        {:ok, callback}

      _ ->
        {:error, :missing_callback_functions}
    end
  end

  def check_config_files(configs) do
    configChecker = fn
      {callback, [f | _R] = files} ->
        case check_callback_load(callback) do
          {:ok, ^callback} ->
            cond do
              is_integer(f) ->
                callback.check_parameter(files)

              is_list(f) ->
                :lists.map(
                  fn file ->
                    callback.check_parameter(file)
                  end,
                  files
                )
            end

          {:error, why} ->
            {:error, {:callback, {callback, why}}}
        end

      {callback, []} ->
        case check_callback_load(callback) do
          {:ok, ^callback} ->
            callback.check_parameter([])

          {:error, why} ->
            {:error, {:callback, {callback, why}}}
        end
    end

    :lists.keysearch(:error, 1, :lists.flatten(:lists.map(configChecker, configs)))
  end

  defp prepare_user_configs([callbackMod | userConfigs], acc, :new) do
    prepare_user_configs(
      userConfigs,
      [{:erlang.list_to_atom(callbackMod), []} | acc],
      :cur
    )
  end

  defp prepare_user_configs(['and' | userConfigs], acc, _) do
    prepare_user_configs(userConfigs, acc, :new)
  end

  defp prepare_user_configs([configString | userConfigs], [{lastMod, lastList} | acc], :cur) do
    prepare_user_configs(userConfigs, [{lastMod, [configString | lastList]} | acc], :cur)
  end

  defp prepare_user_configs([], acc, _) do
    acc
  end

  def prepare_config_list(args) do
    configFiles =
      case :lists.keysearch(:ct_config, 1, args) do
        {:value, {:ct_config, files}} ->
          [
            {:ct_config_plain,
             for f <- files do
               :filename.absname(f)
             end}
          ]

        false ->
          []
      end

    userConfigs =
      case :lists.keysearch(:userconfig, 1, args) do
        {:value, {:userconfig, userConfigFiles}} ->
          prepare_user_configs(userConfigFiles, [], :new)

        false ->
          []
      end

    configFiles ++ userConfigs
  end

  def add_config(callback, []) do
    read_config_files([{callback, []}], &store_config/3)
  end

  def add_config(callback, [file | _Files] = config)
      when is_list(file) do
    :lists.foreach(
      fn cfgStr ->
        read_config_files([{callback, cfgStr}], &store_config/3)
      end,
      config
    )
  end

  def add_config(callback, [c | _] = config) when is_integer(c) do
    read_config_files([{callback, config}], &store_config/3)
  end

  def remove_config(callback, config) do
    :ets.match_delete(
      :ct_attributes,
      r_ct_conf(handler: callback, config: config, _: :_)
    )

    :ok
  end
end
