defmodule :m_ssh_options do
  use Bitwise
  require Record

  Record.defrecord(:r_ssh, :ssh,
    role: :undefined,
    peer: :undefined,
    local: :undefined,
    c_vsn: :undefined,
    s_vsn: :undefined,
    c_version: :undefined,
    s_version: :undefined,
    c_keyinit: :undefined,
    s_keyinit: :undefined,
    send_ext_info: :undefined,
    recv_ext_info: :undefined,
    algorithms: :undefined,
    send_mac: :none,
    send_mac_key: :undefined,
    send_mac_size: 0,
    recv_mac: :none,
    recv_mac_key: :undefined,
    recv_mac_size: 0,
    encrypt: :none,
    encrypt_cipher: :undefined,
    encrypt_keys: :undefined,
    encrypt_block_size: 8,
    encrypt_ctx: :undefined,
    decrypt: :none,
    decrypt_cipher: :undefined,
    decrypt_keys: :undefined,
    decrypt_block_size: 8,
    decrypt_ctx: :undefined,
    compress: :none,
    compress_ctx: :undefined,
    decompress: :none,
    decompress_ctx: :undefined,
    c_lng: :none,
    s_lng: :none,
    user_ack: true,
    timeout: :infinity,
    shared_secret: :undefined,
    exchanged_hash: :undefined,
    session_id: :undefined,
    opts: [],
    send_sequence: 0,
    recv_sequence: 0,
    keyex_key: :undefined,
    keyex_info: :undefined,
    random_length_padding: 15,
    user: :undefined,
    service: :undefined,
    userauth_quiet_mode: :undefined,
    userauth_methods: :undefined,
    userauth_supported_methods: :undefined,
    userauth_pubkeys: :undefined,
    kb_tries_left: 0,
    userauth_preference: :undefined,
    available_host_keys: :undefined,
    pwdfun_user_state: :undefined,
    authenticated: false
  )

  Record.defrecord(:r_alg, :alg,
    kex: :undefined,
    hkey: :undefined,
    send_mac: :undefined,
    recv_mac: :undefined,
    encrypt: :undefined,
    decrypt: :undefined,
    compress: :undefined,
    decompress: :undefined,
    c_lng: :undefined,
    s_lng: :undefined,
    send_ext_info: :undefined,
    recv_ext_info: :undefined
  )

  Record.defrecord(:r_ssh_pty, :ssh_pty,
    term: '',
    width: 80,
    height: 25,
    pixel_width: 1024,
    pixel_height: 768,
    modes: <<>>
  )

  Record.defrecord(:r_circ_buf_entry, :circ_buf_entry,
    module: :undefined,
    line: :undefined,
    function: :undefined,
    pid: self(),
    value: :undefined
  )

  Record.defrecord(:r_file_info, :file_info,
    size: :undefined,
    type: :undefined,
    access: :undefined,
    atime: :undefined,
    mtime: :undefined,
    ctime: :undefined,
    mode: :undefined,
    links: :undefined,
    major_device: :undefined,
    minor_device: :undefined,
    inode: :undefined,
    uid: :undefined,
    gid: :undefined
  )

  Record.defrecord(:r_file_descriptor, :file_descriptor,
    module: :undefined,
    data: :undefined
  )

  def get_value(class, key, opts, _CallerMod, _CallerLine)
      when is_map(opts) do
    case class do
      :internal_options ->
        :maps.get(key, :maps.get(:internal_options, opts))

      :socket_options ->
        :proplists.get_value(
          key,
          :maps.get(:socket_options, opts)
        )

      :user_options ->
        :maps.get(key, opts)
    end
  end

  def get_value(class, key, opts, _CallerMod, _CallerLine) do
    :erlang.error({:bad_options, class, key, opts, _CallerMod, _CallerLine})
  end

  def get_value(:socket_options, key, opts, defFun, _CallerMod, _CallerLine)
      when is_map(opts) do
    :proplists.get_value(key, :maps.get(:socket_options, opts), defFun)
  end

  def get_value(class, key, opts, defFun, callerMod, callerLine)
      when is_map(opts) do
    try do
      get_value(class, key, opts, callerMod, callerLine)
    catch
      :error, {:badkey, ^key} ->
        defFun.()
    else
      :undefined ->
        defFun.()

      value ->
        value
    end
  end

  def get_value(class, key, opts, _DefFun, _CallerMod, _CallerLine) do
    :erlang.error({:bad_options, class, key, opts, _CallerMod, _CallerLine})
  end

  def put_value(:user_options, keyVal, opts, _CallerMod, _CallerLine)
      when is_map(opts) do
    put_user_value(keyVal, opts)
  end

  def put_value(:internal_options, keyVal, opts, _CallerMod, _CallerLine)
      when is_map(opts) do
    internalOpts = :maps.get(:internal_options, opts)
    %{opts | :internal_options => put_internal_value(keyVal, internalOpts)}
  end

  def put_value(:socket_options, keyVal, opts, _CallerMod, _CallerLine)
      when is_map(opts) do
    socketOpts = :maps.get(:socket_options, opts)
    %{opts | :socket_options => put_socket_value(keyVal, socketOpts)}
  end

  defp put_user_value(l, opts) when is_list(l) do
    :lists.foldl(&put_user_value/2, opts, l)
  end

  defp put_user_value({key, value}, opts) do
    %{opts | key => value}
  end

  defp put_internal_value(l, intOpts) when is_list(l) do
    :lists.foldl(&put_internal_value/2, intOpts, l)
  end

  defp put_internal_value({key, value}, intOpts) do
    %{intOpts | key => value}
  end

  defp put_socket_value(l, sockOpts) when is_list(l) do
    l ++ sockOpts
  end

  defp put_socket_value({key, value}, sockOpts) do
    [{key, value} | sockOpts]
  end

  defp put_socket_value(a, sockOpts) when is_atom(a) do
    [a | sockOpts]
  end

  def delete_key(:internal_options, key, opts, _CallerMod, _CallerLine)
      when is_map(opts) do
    internalOpts = :maps.get(:internal_options, opts)
    %{opts | :internal_options => :maps.remove(key, internalOpts)}
  end

  def handle_options(role, propList0) do
    handle_options(role, propList0, %{
      :socket_options => [],
      :internal_options => %{},
      :user_options => []
    })
  end

  defp handle_options(role, optsList0, opts0)
       when is_map(opts0) and
              is_list(optsList0) do
    optsList1 = :proplists.unfold(optsList0)

    try do
      optionDefinitions = default(role)
      roleCnfs = :application.get_env(:ssh, cnf_key(role), [])

      {initialMap, optsList2} =
        :maps.fold(
          fn k, %{:default => vd}, {m, pL} ->
            case config_val(k, roleCnfs, optsList1) do
              {:ok, v1} ->
                {%{
                   m
                   | k => v1,
                     :user_options => [
                       {k, v1}
                       | :maps.get(
                           :user_options,
                           m
                         )
                     ]
                 }, [{k, v1} | pL]}

              {:append, v1} ->
                newVal = :maps.get(k, m, []) ++ v1

                {%{
                   m
                   | k => newVal,
                     :user_options => [
                       {k, newVal}
                       | :lists.keydelete(
                           k,
                           1,
                           :maps.get(
                             :user_options,
                             m
                           )
                         )
                     ]
                 },
                 [
                   {k, newVal}
                   | :lists.keydelete(
                       k,
                       1,
                       pL
                     )
                 ]}

              :undefined ->
                {%{m | k => vd}, pL}
            end
          end,
          {%{
             opts0
             | :user_options =>
                 :maps.get(
                   :user_options,
                   opts0
                 )
           },
           for {k, v} <- optsList1,
               not :maps.is_key(k, opts0) do
             {k, v}
           end},
          optionDefinitions
        )

      final_preferred_algorithms(
        :lists.foldl(
          fn kV, vals ->
            save(kV, optionDefinitions, vals)
          end,
          initialMap,
          optsList2
        )
      )
    catch
      :error, {eO, kV, reason}
      when eO == :eoptions or
             eO == :eerl_env ->
        cond do
          reason == :undefined ->
            {:error, {eO, kV}}

          is_list(reason) ->
            {:error, {eO, {kV, :lists.flatten(reason)}}}

          true ->
            {:error, {eO, {kV, reason}}}
        end
    end
  end

  defp cnf_key(:server) do
    :server_options
  end

  defp cnf_key(:client) do
    :client_options
  end

  defp config_val(:modify_algorithms = key, roleCnfs, opts) do
    v =
      case :application.get_env(:ssh, key) do
        {:ok, v0} ->
          v0

        _ ->
          []
      end ++ :proplists.get_value(key, roleCnfs, []) ++ :proplists.get_value(key, opts, [])

    case v do
      [] ->
        :undefined

      _ ->
        {:append, v}
    end
  end

  defp config_val(key, roleCnfs, opts) do
    case :lists.keysearch(key, 1, opts) do
      {:value, {_, v}} ->
        {:ok, v}

      false ->
        case :lists.keysearch(key, 1, roleCnfs) do
          {:value, {_, v}} ->
            {:ok, v}

          false ->
            :application.get_env(:ssh, key)
        end
    end
  end

  defp check_fun(key, defs) do
    case :ssh_connection_handler.prohibited_sock_option(key) do
      false ->
        %{:chk => fun} = :maps.get(key, defs)
        fun

      true ->
        fn _, _ ->
          :forbidden
        end
    end
  end

  defp save({:allow_user_interaction, v}, opts, vals) do
    save({:user_interaction, v}, opts, vals)
  end

  defp save(inet, defs, optMap)
       when inet == :inet or
              inet == :inet6 do
    save({:inet, inet}, defs, optMap)
  end

  defp save({inet, true}, defs, optMap)
       when inet == :inet or inet == :inet6 do
    save({:inet, inet}, defs, optMap)
  end

  defp save({inet, false}, _Defs, optMap)
       when inet == :inet or inet == :inet6 do
    optMap
  end

  defp save({key, value}, defs, optMap)
       when is_map(optMap) do
    try do
      check_fun(key, defs).(value)
    catch
      :error, {:badkey, :inet} ->
        %{optMap | :socket_options => [value | :maps.get(:socket_options, optMap)]}

      :error, {:badkey, ^key} ->
        %{optMap | :socket_options => [{key, value} | :maps.get(:socket_options, optMap)]}

      :error, {:check, {badValue, extra}} ->
        :erlang.error({:eoptions, {key, badValue}, extra})
    else
      true ->
        %{optMap | key => value}

      {true, modifiedValue} ->
        %{optMap | key => modifiedValue}

      false ->
        :erlang.error({:eoptions, {key, value}, 'Bad value'})

      :forbidden ->
        :erlang.error(
          {:eoptions, {key, value},
           :io_lib.format(
             'The option \'~s\' is used internally. The user is not allowed to specify this option.',
             [key]
           )}
        )
    end
  end

  defp save(opt, _Defs, optMap) when is_map(optMap) do
    %{optMap | :socket_options => [opt | :maps.get(:socket_options, optMap)]}
  end

  def keep_user_options(type, opts) do
    defs = default(type)

    :maps.filter(
      fn key, _Value ->
        try do
          %{:class => class} = :maps.get(key, defs)
          class == :user_option
        catch
          _, _ ->
            false
        end
      end,
      opts
    )
  end

  def keep_set_options(type, opts) do
    defs = default(type)

    :maps.filter(
      fn key, value ->
        try do
          %{:default => defVal} = :maps.get(key, defs)
          defVal !== value
        catch
          _, _ ->
            false
        end
      end,
      opts
    )
  end

  def default(:server) do
    %{
      default(:common)
      | :subsystems => %{
          :default => [:ssh_sftpd.subsystem_spec([])],
          :chk => fn l ->
            is_list(l) and
              :lists.all(
                fn
                  {name, {cB, args}} ->
                    check_string(name) and is_atom(cB) and is_list(args)

                  _ ->
                    false
                end,
                l
              )
          end,
          :class => :user_option
        },
        :shell => %{
          :default => {:shell, :start, []},
          :chk => fn
            {m, f, a} ->
              is_atom(m) and is_atom(f) and is_list(a)

            :disabled ->
              true

            v ->
              check_function1(v) or check_function2(v)
          end,
          :class => :user_option
        },
        :exec => %{
          :default => :undefined,
          :chk => fn
            {:direct, v} ->
              check_function1(v) or check_function2(v) or check_function3(v)

            :disabled ->
              true

            {m, f, a} ->
              is_atom(m) and is_atom(f) and is_list(a)

            v ->
              check_function1(v) or check_function2(v) or check_function3(v)
          end,
          :class => :user_option
        },
        :ssh_cli => %{
          :default => :undefined,
          :chk => fn
            {cb, as} ->
              is_atom(cb) and is_list(as)

            v ->
              v == :no_cli
          end,
          :class => :user_option
        },
        :tcpip_tunnel_out => %{
          :default => false,
          :chk => fn v ->
            :erlang.is_boolean(v)
          end,
          :class => :user_option
        },
        :tcpip_tunnel_in => %{
          :default => false,
          :chk => fn v ->
            :erlang.is_boolean(v)
          end,
          :class => :user_option
        },
        :system_dir => %{
          :default => '/etc/ssh',
          :chk => fn v ->
            check_string(v) and check_dir(v)
          end,
          :class => :user_option
        },
        :auth_method_kb_interactive_data => %{
          :default => :undefined,
          :chk => fn
            {s1, s2, s3, b} ->
              check_string(s1) and check_string(s2) and check_string(s3) and is_boolean(b)

            f ->
              check_function3(f) or check_function4(f)
          end,
          :class => :user_option
        },
        :user_passwords => %{
          :default => [],
          :chk => fn v ->
            is_list(v) and
              :lists.all(
                fn {s1, s2} ->
                  check_string(s1) and check_string(s2)
                end,
                v
              )
          end,
          :class => :user_option
        },
        :pk_check_user => %{
          :default => false,
          :chk => fn v ->
            :erlang.is_boolean(v)
          end,
          :class => :user_option
        },
        :password => %{
          :default => :undefined,
          :chk => fn v ->
            check_string(v)
          end,
          :class => :user_option
        },
        :dh_gex_groups => %{
          :default => :undefined,
          :chk => fn v ->
            check_dh_gex_groups(v)
          end,
          :class => :user_option
        },
        :dh_gex_limits => %{
          :default => {0, :infinity},
          :chk => fn
            {i1, i2} ->
              check_pos_integer(i1) and check_pos_integer(i2) and i1 < i2

            _ ->
              false
          end,
          :class => :user_option
        },
        :pwdfun => %{
          :default => :undefined,
          :chk => fn v ->
            check_function4(v) or check_function2(v)
          end,
          :class => :user_option
        },
        :negotiation_timeout => %{
          :default => 2 * 60 * 1000,
          :chk => fn v ->
            check_timeout(v)
          end,
          :class => :user_option
        },
        :hello_timeout => %{
          :default => 30 * 1000,
          :chk => &check_timeout/1,
          :class => :user_option
        },
        :max_sessions => %{
          :default => :infinity,
          :chk => fn v ->
            check_pos_integer(v)
          end,
          :class => :user_option
        },
        :max_channels => %{
          :default => :infinity,
          :chk => fn v ->
            check_pos_integer(v)
          end,
          :class => :user_option
        },
        :parallel_login => %{
          :default => false,
          :chk => fn v ->
            :erlang.is_boolean(v)
          end,
          :class => :user_option
        },
        :minimal_remote_max_packet_size => %{
          :default => 0,
          :chk => fn v ->
            check_pos_integer(v)
          end,
          :class => :user_option
        },
        :failfun => %{
          :default => fn _, _, _ ->
            :void
          end,
          :chk => fn v ->
            check_function3(v) or check_function2(v)
          end,
          :class => :user_option
        },
        :connectfun => %{
          :default => fn _, _, _ ->
            :void
          end,
          :chk => fn v ->
            check_function3(v)
          end,
          :class => :user_option
        },
        :infofun => %{
          :default => fn _, _, _ ->
            :void
          end,
          :chk => fn v ->
            check_function3(v) or check_function2(v)
          end,
          :class => :undoc_user_option
        }
    }
  end

  def default(:client) do
    %{
      default(:common)
      | :dsa_pass_phrase => %{
          :default => :undefined,
          :chk => fn v ->
            check_string(v)
          end,
          :class => :user_option
        },
        :rsa_pass_phrase => %{
          :default => :undefined,
          :chk => fn v ->
            check_string(v)
          end,
          :class => :user_option
        },
        :ecdsa_pass_phrase => %{
          :default => :undefined,
          :chk => fn v ->
            check_string(v)
          end,
          :class => :user_option
        },
        :silently_accept_hosts => %{
          :default => false,
          :chk => fn v ->
            check_silently_accept_hosts(v)
          end,
          :class => :user_option
        },
        :user_interaction => %{
          :default => true,
          :chk => fn v ->
            :erlang.is_boolean(v)
          end,
          :class => :user_option
        },
        :save_accepted_host => %{
          :default => true,
          :chk => fn v ->
            :erlang.is_boolean(v)
          end,
          :class => :user_option
        },
        :dh_gex_limits => %{
          :default => {1024, 6144, 8192},
          :chk => fn
            {min, i, max} ->
              :lists.all(&check_pos_integer/1, [min, i, max])

            _ ->
              false
          end,
          :class => :user_option
        },
        :connect_timeout => %{
          :default => :infinity,
          :chk => fn v ->
            check_timeout(v)
          end,
          :class => :user_option
        },
        :user => %{
          :default =>
            (
              env =
                case :os.type() do
                  {:win32, _} ->
                    'USERNAME'

                  {:unix, _} ->
                    'LOGNAME'
                end

              case :os.getenv(env) do
                false ->
                  case :os.getenv('USER') do
                    false ->
                      :undefined

                    user ->
                      user
                  end

                user ->
                  user
              end
            ),
          :chk => fn v ->
            check_string(v)
          end,
          :class => :user_option
        },
        :password => %{
          :default => :undefined,
          :chk => fn v ->
            check_string(v)
          end,
          :class => :user_option
        },
        :quiet_mode => %{
          :default => false,
          :chk => fn v ->
            :erlang.is_boolean(v)
          end,
          :class => :user_option
        },
        :keyboard_interact_fun => %{
          :default => :undefined,
          :chk => fn v ->
            check_function3(v)
          end,
          :class => :undoc_user_option
        }
    }
  end

  def default(:common) do
    %{
      :user_dir => %{
        :default => false,
        :chk => fn v ->
          check_string(v) and check_dir(v)
        end,
        :class => :user_option
      },
      :pref_public_key_algs => %{
        :default => :undefined,
        :chk => fn v ->
          check_pref_public_key_algs(v)
        end,
        :class => :user_option
      },
      :preferred_algorithms => %{
        :default => :ssh.default_algorithms(),
        :chk => fn v ->
          check_preferred_algorithms(v)
        end,
        :class => :user_option
      },
      :modify_algorithms => %{
        :default => :undefined,
        :chk => fn v ->
          check_modify_algorithms(v)
        end,
        :class => :user_option
      },
      :id_string => %{
        :default =>
          try do
            {:ok, [_ | _] = vSN} = :application.get_key(:ssh, :vsn)
            'Erlang/' ++ vSN
          catch
            _, _ ->
              ''
          end,
        :chk => fn
          :random ->
            {true, {:random, 2, 5}}

          {:random, i1, i2} ->
            check_pos_integer(i1) and check_pos_integer(i2) and i1 <= i2

          v ->
            check_string(v)
        end,
        :class => :user_option
      },
      :key_cb => %{
        :default => {:ssh_file, []},
        :chk => fn
          {mod, opts} ->
            is_atom(mod) and is_list(opts)

          mod when is_atom(mod) ->
            {true, {mod, []}}

          _ ->
            false
        end,
        :class => :user_option
      },
      :profile => %{
        :default => :default,
        :chk => fn v ->
          :erlang.is_atom(v)
        end,
        :class => :user_option
      },
      :idle_time => %{
        :default => :infinity,
        :chk => fn v ->
          check_timeout(v)
        end,
        :class => :user_option
      },
      :disconnectfun => %{
        :default => fn _ ->
          :void
        end,
        :chk => fn v ->
          check_function1(v)
        end,
        :class => :user_option
      },
      :unexpectedfun => %{
        :default => fn _, _ ->
          :report
        end,
        :chk => fn v ->
          check_function2(v)
        end,
        :class => :user_option
      },
      :ssh_msg_debug_fun => %{
        :default => fn _, _, _, _ ->
          :void
        end,
        :chk => fn v ->
          check_function4(v)
        end,
        :class => :user_option
      },
      :rekey_limit => %{
        :default => {3_600_000, 1_024_000_000},
        :chk => fn
          {:infinity, :infinity} ->
            true

          {mins, :infinity} when is_integer(mins) and mins > 0 ->
            {true, {mins * 60 * 1000, :infinity}}

          {:infinity, bytes}
          when is_integer(bytes) and
                 bytes >= 0 ->
            true

          {mins, bytes}
          when is_integer(mins) and mins > 0 and
                 is_integer(bytes) and bytes >= 0 ->
            {true, {mins * 60 * 1000, bytes}}

          :infinity ->
            {true, {3_600_000, :infinity}}

          bytes when is_integer(bytes) and bytes >= 0 ->
            {true, {3_600_000, bytes}}

          _ ->
            false
        end,
        :class => :user_option
      },
      :auth_methods => %{
        :default => 'publickey,keyboard-interactive,password',
        :chk => fn as ->
          try do
            sup = :string.tokens('publickey,keyboard-interactive,password', ',')
            new = :string.tokens(as, ',')

            [] ==
              for x <- new, not :lists.member(x, sup) do
                x
              end
          catch
            _, _ ->
              false
          end
        end,
        :class => :user_option
      },
      :send_ext_info => %{:default => true, :chk => &:erlang.is_boolean/1, :class => :user_option},
      :recv_ext_info => %{:default => true, :chk => &:erlang.is_boolean/1, :class => :user_option},
      :transport => %{
        :default => {:tcp, :gen_tcp, :tcp_closed},
        :chk => fn {a, b, c} ->
          is_atom(a) and is_atom(b) and is_atom(c)
        end,
        :class => :undoc_user_option
      },
      :vsn => %{
        :default => {2, 0},
        :chk => fn
          {maj, min} ->
            check_non_neg_integer(maj) and check_non_neg_integer(min)

          _ ->
            false
        end,
        :class => :undoc_user_option
      },
      :tstflg => %{
        :default => [],
        :chk => fn v ->
          :erlang.is_list(v)
        end,
        :class => :undoc_user_option
      },
      :user_dir_fun => %{
        :default => :undefined,
        :chk => fn v ->
          check_function1(v)
        end,
        :class => :undoc_user_option
      },
      :max_random_length_padding => %{
        :default => 15,
        :chk => fn v ->
          check_non_neg_integer(v)
        end,
        :class => :undoc_user_option
      }
    }
  end

  defp error_in_check(badValue, extra) do
    :erlang.error({:check, {badValue, extra}})
  end

  defp check_timeout(:infinity) do
    true
  end

  defp check_timeout(i) do
    check_pos_integer(i)
  end

  defp check_pos_integer(i) do
    is_integer(i) and i > 0
  end

  defp check_non_neg_integer(i) do
    is_integer(i) and i >= 0
  end

  defp check_function1(f) do
    is_function(f, 1)
  end

  defp check_function2(f) do
    is_function(f, 2)
  end

  defp check_function3(f) do
    is_function(f, 3)
  end

  defp check_function4(f) do
    is_function(f, 4)
  end

  defp check_pref_public_key_algs(v) do
    pKs = :ssh_transport.supported_algorithms(:public_key)

    cHK = fn a, ack ->
      case :lists.member(a, pKs) do
        true ->
          case :lists.member(a, ack) do
            false ->
              [a | ack]

            true ->
              ack
          end

        false ->
          error_in_check(a, 'Not supported public key')
      end
    end

    case :lists.foldr(
           fn
             :ssh_dsa, ack ->
               cHK.(:"ssh-dss", ack)

             :ssh_rsa, ack ->
               cHK.(:"ssh-rsa", ack)

             x, ack ->
               cHK.(x, ack)
           end,
           [],
           v
         ) do
      ^v ->
        true

      [] ->
        false

      v1 ->
        {true, v1}
    end
  end

  defp check_dir(dir) do
    case :file.read_file_info(dir) do
      {:ok, r_file_info(type: :directory, access: access)} ->
        case access do
          :read ->
            true

          :read_write ->
            true

          _ ->
            error_in_check(dir, :eacces)
        end

      {:ok, r_file_info()} ->
        error_in_check(dir, :enotdir)

      {:error, error} ->
        error_in_check(dir, error)
    end
  end

  defp check_string(s) do
    is_list(s)
  end

  defp check_dh_gex_groups({:file, file}) when is_list(file) do
    case :file.consult(file) do
      {:ok, groupDefs} ->
        check_dh_gex_groups(groupDefs)

      {:error, error} ->
        error_in_check({:file, file}, error)
    end
  end

  defp check_dh_gex_groups({:ssh_moduli_file, file}) when is_list(file) do
    case :file.open(file, [:read]) do
      {:ok, d} ->
        try do
          read_moduli_file(d, 1, [])
        catch
          _, _ ->
            error_in_check({:ssh_moduli_file, file}, 'Bad format in file ' ++ file)
        else
          {:ok, moduli} ->
            check_dh_gex_groups(moduli)

          {:error, error} ->
            error_in_check({:ssh_moduli_file, file}, error)
        after
          :file.close(d)
        end

      {:error, error} ->
        error_in_check({:ssh_moduli_file, file}, error)
    end
  end

  defp check_dh_gex_groups(l0) when is_list(l0) and is_tuple(hd(l0)) do
    {true,
     collect_per_size(
       :lists.foldl(
         fn
           {n, g, p}, acc
           when is_integer(n) and n > 0 and
                  is_integer(g) and g > 0 and
                  is_integer(p) and p > 0 ->
             [{n, {g, p}} | acc]

           {n, {g, p}}, acc
           when is_integer(n) and
                  n > 0 and
                  is_integer(g) and
                  g > 0 and
                  is_integer(p) and
                  p > 0 ->
             [{n, {g, p}} | acc]

           {n, gPs}, acc when is_list(gPs) ->
             :lists.foldr(
               fn {gi, pi}, acci
                  when is_integer(gi) and
                         gi > 0 and
                         is_integer(pi) and
                         pi > 0 ->
                 [
                   {n, {gi, pi}}
                   | acci
                 ]
               end,
               acc,
               gPs
             )
         end,
         [],
         l0
       )
     )}
  end

  defp check_dh_gex_groups(_) do
    false
  end

  defp collect_per_size(l) do
    :lists.foldr(
      fn
        {sz, gP}, [{sz, gPs} | acc] ->
          [{sz, [gP | gPs]} | acc]

        {sz, gP}, acc ->
          [{sz, [gP]} | acc]
      end,
      [],
      :lists.sort(l)
    )
  end

  defp read_moduli_file(d, i, acc) do
    case :io.get_line(d, '') do
      {:error, error} ->
        {:error, error}

      :eof ->
        {:ok, acc}

      '#' ++ _ ->
        read_moduli_file(d, i + 1, acc)

      <<"#", _::binary>> ->
        read_moduli_file(d, i + 1, acc)

      data ->
        line =
          cond do
            is_binary(data) ->
              :erlang.binary_to_list(data)

            is_list(data) ->
              data
          end

        try do
          [_Time, _Class, _Tests, _Tries, size, g, p] = :string.tokens(line, ' \r\n')

          m =
            {:erlang.list_to_integer(size),
             {:erlang.list_to_integer(g), :erlang.list_to_integer(p, 16)}}

          read_moduli_file(d, i + 1, [m | acc])
        catch
          _, _ ->
            read_moduli_file(d, i + 1, acc)
        end
    end
  end

  defp check_silently_accept_hosts(b) when is_boolean(b) do
    true
  end

  defp check_silently_accept_hosts(f) when is_function(f, 2) do
    true
  end

  defp check_silently_accept_hosts({false, s}) when is_atom(s) do
    valid_hash(s)
  end

  defp check_silently_accept_hosts({s, f}) when is_function(f, 2) do
    valid_hash(s)
  end

  defp check_silently_accept_hosts(_) do
    false
  end

  defp valid_hash(s) do
    valid_hash(
      s,
      :proplists.get_value(:hashs, :crypto.supports())
    )
  end

  defp valid_hash(s, ss) when is_atom(s) do
    :lists.member(
      s,
      [:md5, :sha, :sha224, :sha256, :sha384, :sha512]
    ) and :lists.member(s, ss)
  end

  defp valid_hash(l, ss) when is_list(l) do
    :lists.all(
      fn s ->
        valid_hash(s, ss)
      end,
      l
    )
  end

  defp valid_hash(x, _) do
    error_in_check(x, 'Expect atom or list in fingerprint spec')
  end

  def initial_default_algorithms(defList, modList) do
    {true, l0} = check_modify_algorithms(modList)
    rm_non_supported(false, eval_ops(defList, l0))
  end

  defp check_modify_algorithms(m) when is_list(m) do
    for op_KVs <- m,
        not is_tuple(op_KVs) or :erlang.size(op_KVs) !== 2 or
          not :lists.member(
            :erlang.element(
              1,
              op_KVs
            ),
            [:append, :prepend, :rm]
          ) do
      error_in_check(op_KVs, 'Bad modify_algorithms')
    end

    {true,
     for {op, kVs} <- m do
       {op, normalize_mod_algs(kVs, false)}
     end}
  end

  defp check_modify_algorithms(_) do
    error_in_check(:modify_algorithms, 'Bad option value. List expected.')
  end

  defp normalize_mod_algs(kVs, useDefaultAlgs) do
    normalize_mod_algs(:ssh_transport.algo_classes(), kVs, [], useDefaultAlgs)
  end

  defp normalize_mod_algs([k | ks], kVs0, acc, useDefaultAlgs) do
    {vs1, kVs} =
      case :lists.keytake(k, 1, kVs0) do
        {:value, {^k, vs0}, kVs1} ->
          {vs0, kVs1}

        false ->
          {[], kVs0}
      end

    vs = normalize_mod_alg_list(k, vs1, useDefaultAlgs)
    normalize_mod_algs(ks, kVs, [{k, vs} | acc], useDefaultAlgs)
  end

  defp normalize_mod_algs([], [], acc, _) do
    :lists.reverse(acc)
  end

  defp normalize_mod_algs([], [{k, _} | _], _, _) do
    case :ssh_transport.algo_class(k) do
      true ->
        error_in_check(k, 'Duplicate key')

      false ->
        error_in_check(k, 'Unknown key')
    end
  end

  defp normalize_mod_algs([], [x | _], _, _) do
    error_in_check(x, 'Bad list element')
  end

  defp normalize_mod_alg_list(k, vs, useDefaultAlgs) do
    normalize_mod_alg_list(
      k,
      :ssh_transport.algo_two_spec_class(k),
      vs,
      def_alg(k, useDefaultAlgs)
    )
  end

  defp normalize_mod_alg_list(_K, _, [], default) do
    default
  end

  defp normalize_mod_alg_list(k, true, [{:client2server, l1}], [_, {:server2client, l2}]) do
    [nml1(k, {:client2server, l1}), {:server2client, l2}]
  end

  defp normalize_mod_alg_list(k, true, [{:server2client, l2}], [{:client2server, l1}, _]) do
    [{:client2server, l1}, nml1(k, {:server2client, l2})]
  end

  defp normalize_mod_alg_list(k, true, [{:server2client, l2}, {:client2server, l1}], _) do
    [nml1(k, {:client2server, l1}), nml1(k, {:server2client, l2})]
  end

  defp normalize_mod_alg_list(k, true, [{:client2server, l1}, {:server2client, l2}], _) do
    [nml1(k, {:client2server, l1}), nml1(k, {:server2client, l2})]
  end

  defp normalize_mod_alg_list(k, true, l0, _) do
    l = nml(k, l0)
    [{:client2server, l}, {:server2client, l}]
  end

  defp normalize_mod_alg_list(k, false, l, _) do
    nml(k, l)
  end

  defp nml1(k, {t, v})
       when t == :client2server or
              t == :server2client do
    {t, nml({k, t}, v)}
  end

  defp nml(k, l) do
    for v <- l, not is_atom(v) do
      error_in_check(k, 'Bad value for this key')
    end

    case l -- :lists.usort(l) do
      [] ->
        :ok

      dups ->
        error_in_check({k, dups}, 'Duplicates')
    end

    l
  end

  defp def_alg(k, false) do
    case :ssh_transport.algo_two_spec_class(k) do
      false ->
        []

      true ->
        [{:client2server, []}, {:server2client, []}]
    end
  end

  defp def_alg(k, true) do
    :ssh_transport.default_algorithms(k)
  end

  def check_preferred_algorithms(algs) when is_list(algs) do
    check_input_ok(algs)
    {true, normalize_mod_algs(algs, true)}
  end

  def check_preferred_algorithms(_) do
    error_in_check(:modify_algorithms, 'Bad option value. List expected.')
  end

  defp check_input_ok(algs) do
    for kVs <- algs,
        not is_tuple(kVs) or :erlang.size(kVs) !== 2 do
      error_in_check(kVs, 'Bad preferred_algorithms')
    end
  end

  defp final_preferred_algorithms(options0) do
    result =
      case :ssh_options.get_value(:user_options, :modify_algorithms, options0, :ssh_options, 1134) do
        :undefined ->
          rm_non_supported(
            true,
            :ssh_options.get_value(
              :user_options,
              :preferred_algorithms,
              options0,
              :ssh_options,
              1137
            )
          )

        modAlgs ->
          rm_non_supported(
            false,
            eval_ops(
              :ssh_options.get_value(
                :user_options,
                :preferred_algorithms,
                options0,
                :ssh_options,
                1140
              ),
              modAlgs
            )
          )
      end

    error_if_empty(result)

    options1 =
      :ssh_options.put_value(
        :user_options,
        {:preferred_algorithms, result},
        options0,
        :ssh_options,
        1144
      )

    case :ssh_options.get_value(
           :user_options,
           :pref_public_key_algs,
           options1,
           :ssh_options,
           1145
         ) do
      :undefined ->
        :ssh_options.put_value(
          :user_options,
          {:pref_public_key_algs, :proplists.get_value(:public_key, result)},
          options1,
          :ssh_options,
          1147
        )

      _ ->
        options1
    end
  end

  defp eval_ops(prefAlgs, modAlgs) do
    :lists.foldl(&eval_op/2, prefAlgs, modAlgs)
  end

  defp eval_op({op, algKVs}, prefAlgs) do
    eval_op(op, algKVs, prefAlgs, [])
  end

  defp eval_op(op, [{c, l1} | t1], [{c, l2} | t2], acc) do
    eval_op(op, t1, t2, [{c, eval_op(op, l1, l2, [])} | acc])
  end

  defp eval_op(_, [], [], acc) do
    :lists.reverse(acc)
  end

  defp eval_op(:rm, opt, pref, [])
       when is_list(opt) and
              is_list(pref) do
    pref -- opt
  end

  defp eval_op(:append, opt, pref, [])
       when is_list(opt) and
              is_list(pref) do
    (pref -- opt) ++ opt
  end

  defp eval_op(:prepend, opt, pref, [])
       when is_list(opt) and
              is_list(pref) do
    opt ++ (pref -- opt)
  end

  defp rm_non_supported(unsupIsErrorFlg, kVs) do
    for {k, vs} <- kVs do
      {k, rmns(k, vs, unsupIsErrorFlg)}
    end
  end

  defp rmns(k, vs, unsupIsErrorFlg) do
    case :ssh_transport.algo_two_spec_class(k) do
      false ->
        rm_unsup(vs, :ssh_transport.supported_algorithms(k), unsupIsErrorFlg, k)

      true ->
        for {{c, vsx}, {c, sup}} <-
              :lists.zip(
                vs,
                :ssh_transport.supported_algorithms(k)
              ) do
          {c, rm_unsup(vsx, sup, unsupIsErrorFlg, {k, c})}
        end
    end
  end

  defp rm_unsup(a, b, flg, errInf) do
    case a -- b do
      unsup = [_ | _] when flg == true ->
        :erlang.error(
          {:eoptions, {:preferred_algorithms, {errInf, unsup}}, 'Unsupported value(s) found'}
        )

      unsup ->
        a -- unsup
    end
  end

  defp error_if_empty([{k, []} | _]) do
    :erlang.error({:eoptions, k, 'Empty resulting algorithm list'})
  end

  defp error_if_empty([{k, [{:client2server, []}, {:server2client, []}]}]) do
    :erlang.error({:eoptions, k, 'Empty resulting algorithm list'})
  end

  defp error_if_empty([{k, [{:client2server, []} | _]} | _]) do
    :erlang.error({:eoptions, {k, :client2server}, 'Empty resulting algorithm list'})
  end

  defp error_if_empty([{k, [[_, {:server2client, []}] | _]} | _]) do
    :erlang.error({:eoptions, {k, :server2client}, 'Empty resulting algorithm list'})
  end

  defp error_if_empty([_ | t]) do
    error_if_empty(t)
  end

  defp error_if_empty([]) do
    :ok
  end
end
