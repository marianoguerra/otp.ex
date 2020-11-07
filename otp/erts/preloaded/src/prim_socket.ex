defmodule :m_prim_socket do
  use Bitwise

  def on_load() do
    on_load(%{})
  end

  def on_load(extra) when is_map(extra) do
    pid = :erts_internal.spawn_system_process(:socket_registry, :start, [])

    debugFilename =
      case :os.get_env_var('ESOCK_DEBUG_FILENAME') do
        '*' ->
          '/tmp/esock-dbg-??????'

        f ->
          f
      end

    useRegistry =
      case :os.get_env_var('ESOCK_USE_SOCKET_REGISTRY') do
        'true' ->
          true

        'false' ->
          false

        _ ->
          :undefined
      end

    extra_1 =
      case useRegistry do
        :undefined ->
          %{extra | :registry => pid}

        _ ->
          %{extra | :registry => pid, :use_registry => useRegistry}
      end

    extra_2 =
      case debugFilename do
        false ->
          extra_1

        _ ->
          %{
            extra_1
            | :debug => true,
              :socket_debug => true,
              :debug_filename => enc_path(debugFilename)
          }
      end

    :ok =
      :erlang.load_nif(
        :erlang.atom_to_list(:prim_socket),
        extra_2
      )

    pT =
      put_supports_table(
        :protocols,
        fn protocols ->
          protocols_table(protocols)
        end
      )

    _ =
      put_supports_table(
        :options,
        fn options ->
          options_table(options, pT)
        end
      )

    _ =
      put_supports_table(
        :msg_flags,
        fn flags ->
          flags
        end
      )

    :ok
  end

  defp put_supports_table(tag, mkTable) do
    table =
      try do
        nif_supports(tag)
      catch
        :error, :notsup ->
          %{}
      else
        data ->
          :maps.from_list(mkTable.(data))
      end

    p_put(tag, table)
    table
  end

  defp protocols_table([{names, num} | protocols]) do
    [{num, names} | protocols_table(protocols, names, num)]
  end

  defp protocols_table([]) do
    []
  end

  defp protocols_table(protocols, [name | names], num) do
    [{name, num} | protocols_table(protocols, names, num)]
  end

  defp protocols_table(protocols, [], _Num) do
    protocols_table(protocols)
  end

  defp options_table([], _PT) do
    []
  end

  defp options_table([{:socket, levelOpts} | options], pT) do
    options_table(options, pT, :socket, levelOpts, [:socket])
  end

  defp options_table([{levelNum, levelOpts} | options], pT) do
    levels = :maps.get(levelNum, pT)
    options_table(options, pT, levelNum, levelOpts, levels)
  end

  defp options_table(options, pT, _Level, [], _Levels) do
    options_table(options, pT)
  end

  defp options_table(options, pT, level, [levelOpt | levelOpts], levels) do
    levelOptNum =
      case levelOpt do
        {opt, optNum} ->
          {level, optNum}

        opt when is_atom(opt) ->
          :undefined
      end

    options_table(options, pT, level, levelOpts, levels, opt, levelOptNum, levels)
  end

  defp options_table(options, pT, level, levelOpts, levels, _Opt, _LevelOptNum, []) do
    options_table(options, pT, level, levelOpts, levels)
  end

  defp options_table(options, pT, level, levelOpts, levels, opt, levelOptNum, [l | ls]) do
    [
      {{l, opt}, levelOptNum}
      | options_table(options, pT, level, levelOpts, levels, opt, levelOptNum, ls)
    ]
  end

  def info() do
    nif_info()
  end

  def info(sockRef) do
    %{:protocol => numProtocol} = info = nif_info(sockRef)

    case p_get(:protocols) do
      %{^numProtocol => [protocol | _]} ->
        %{info | :protocol => protocol}

      %{} ->
        info
    end
  end

  def debug(d) do
    nif_command(%{:command => :debug, :data => d})
  end

  def socket_debug(d) do
    nif_command(%{:command => :socket_debug, :data => d})
  end

  def use_registry(d) do
    nif_command(%{:command => :use_registry, :data => d})
  end

  def supports() do
    nif_supports()
  end

  def supports(:protocols) do
    :maps.fold(
      fn
        name, _Num, acc when is_atom(name) ->
          [{name, true} | acc]

        num, _Names, acc when is_integer(num) ->
          acc
      end,
      [],
      p_get(:protocols)
    )
  end

  def supports(:options) do
    :maps.fold(
      fn {_Level, _Opt} = option, value, acc ->
        [{option, is_supported_option(option, value)} | acc]
      end,
      [],
      p_get(:options)
    )
  end

  def supports(:msg_flags) do
    :maps.fold(
      fn name, num, acc ->
        [{name, num !== 0} | acc]
      end,
      [],
      p_get(:msg_flags)
    )
  end

  def supports(key) do
    nif_supports(key)
  end

  def supports(:options, level) when is_atom(level) do
    :maps.fold(
      fn {l, opt} = option, value, acc ->
        cond do
          l === level ->
            [{opt, is_supported_option(option, value)} | acc]

          true ->
            acc
        end
      end,
      [],
      p_get(:options)
    )
  end

  def supports(:options, _Level) do
    []
  end

  def supports(_Key1, _Key2) do
    []
  end

  defp is_supported_option({:socket, :peek_off}, _Value) do
    false
  end

  defp is_supported_option(_Option, {_NumLevel, _NumOpt}) do
    true
  end

  defp is_supported_option(_Option, :undefined) do
    false
  end

  def is_supported(key1) do
    get_is_supported(key1, nif_supports())
  end

  def is_supported(:protocols = tab, name) when is_atom(name) do
    p_get_is_supported(tab, name, fn _ ->
      true
    end)
  end

  def is_supported(:protocols, _Name) do
    false
  end

  def is_supported(:options = tab, {_Level, _Opt} = option) do
    p_get_is_supported(tab, option, fn value ->
      is_supported_option(option, value)
    end)
  end

  def is_supported(:options, _Option) do
    false
  end

  def is_supported(:msg_flags = tab, flag) do
    p_get_is_supported(tab, flag, fn value ->
      value !== 0
    end)
  end

  def is_supported(key1, key2) do
    get_is_supported(key2, nif_supports(key1))
  end

  defp p_get_is_supported(tab, key, fun) do
    case p_get(tab) do
      %{^key => val} ->
        fun.(val)

      %{} ->
        false
    end
  end

  defp get_is_supported(key, supported) do
    case :lists.keyfind(key, 1, supported) do
      false ->
        false

      {_, value} when is_boolean(value) ->
        value
    end
  end

  def open(fD, opts) when is_map(opts) do
    try do
      case opts do
        %{:protocol => protocol} ->
          numProtocol = enc_protocol(protocol)
          %{opts | :protocol => numProtocol}

        %{} ->
          opts
      end
    catch
      reason ->
        {:error, reason}
    else
      eOpts ->
        case nif_open(fD, eOpts) do
          {:invalid, reason} ->
            cond do
              reason === :domain or reason === :type ->
                {:error, {:invalid, {:options, opts, reason}}}
            end

          result ->
            result
        end
    end
  end

  def open(domain, type, protocol, opts)
      when is_map(opts) do
    try do
      {enc_protocol(protocol),
       case opts do
         %{:netns => path} when is_list(path) ->
           %{opts | :netns => enc_path(path)}

         _ ->
           opts
       end}
    catch
      reason ->
        {:error, reason}
    else
      {numProtocol, eOpts} ->
        case nif_open(domain, type, numProtocol, eOpts) do
          {:invalid, reason} ->
            {:error,
             {:invalid,
              {reason,
               case reason do
                 :domain ->
                   domain

                 :type ->
                   type
               end}}}

          result ->
            result
        end
    end
  end

  def bind(sockRef, addr) do
    try do
      enc_sockaddr(addr)
    catch
      reason ->
        {:error, reason}
    else
      eAddr ->
        case nif_bind(sockRef, eAddr) do
          {:invalid, reason} ->
            case reason do
              :sockaddr ->
                {:error, {:invalid, {reason, addr}}}
            end

          result ->
            result
        end
    end
  end

  def bind(sockRef, addrs, action) when is_list(addrs) do
    try do
      for addr <- addrs do
        enc_sockaddr(addr)
      end
    catch
      reason ->
        {:error, reason}
    else
      eAddrs ->
        case nif_bind(sockRef, eAddrs, action) do
          {:invalid, reason} ->
            case reason do
              {:sockaddr, n} ->
                {:error, {:invalid, {:sockaddr, :lists.nth(n, addrs)}}}
            end

          result ->
            result
        end
    end
  end

  def connect(sockRef, connectRef, sockAddr) do
    try do
      enc_sockaddr(sockAddr)
    catch
      reason ->
        {:error, reason}
    else
      eSockAddr ->
        case nif_connect(sockRef, connectRef, eSockAddr) do
          {:invalid, reason} ->
            case reason do
              :sockaddr ->
                {:error, {:invalid, {reason, sockAddr}}}
            end

          result ->
            result
        end
    end
  end

  def connect(sockRef) do
    nif_connect(sockRef)
  end

  def listen(sockRef, backlog) do
    nif_listen(sockRef, backlog)
  end

  def accept(listenSockRef, accRef) do
    nif_accept(listenSockRef, accRef)
  end

  def send(sockRef, sendRef, data, flags) do
    try do
      enc_msg_flags(flags)
    catch
      reason ->
        {:error, reason}
    else
      eFlags ->
        nif_send(sockRef, sendRef, data, eFlags)
    end
  end

  def sendto(sockRef, sendRef, data, to, flags) do
    try do
      {enc_sockaddr(to), enc_msg_flags(flags)}
    catch
      reason ->
        {:error, reason}
    else
      {eTo, eFlags} ->
        case nif_sendto(sockRef, sendRef, data, eTo, eFlags) do
          {:invalid, reason} ->
            case reason do
              :sockaddr ->
                {:error, {:invalid, {:sockaddr, to}}}
            end

          result ->
            result
        end
    end
  end

  def sendmsg(sockRef, sendRef, msg, flags) do
    try do
      {enc_msg(msg), enc_msg_flags(flags)}
    catch
      reason ->
        {:error, reason}
    else
      {eMsg, eFlags} ->
        case nif_sendmsg(sockRef, sendRef, eMsg, eFlags) do
          {:invalid, reason} ->
            cond do
              reason === :addr or reason === :iov or
                  reason === :ctrl ->
                {:error, {:invalid, {:msg, msg, reason}}}
            end

          result ->
            result
        end
    end
  end

  def recv(sockRef, recvRef, length, flags) do
    try do
      enc_msg_flags(flags)
    catch
      reason ->
        {:error, reason}
    else
      eFlags ->
        nif_recv(sockRef, recvRef, length, eFlags)
    end
  end

  def recvfrom(sockRef, recvRef, length, flags) do
    try do
      enc_msg_flags(flags)
    catch
      reason ->
        {:error, reason}
    else
      eFlags ->
        nif_recvfrom(sockRef, recvRef, length, eFlags)
    end
  end

  def recvmsg(sockRef, recvRef, bufSz, ctrlSz, flags) do
    try do
      enc_msg_flags(flags)
    catch
      reason ->
        {:error, reason}
    else
      eFlags ->
        case nif_recvmsg(sockRef, recvRef, bufSz, ctrlSz, eFlags) do
          {:ok, %{:ctrl => []}} = result ->
            result

          {:ok, %{:ctrl => cmsgs} = msg} ->
            {:ok, %{msg | :ctrl => dec_cmsgs(cmsgs, p_get(:protocols))}}

          result ->
            result
        end
    end
  end

  def close(sockRef) do
    nif_close(sockRef)
  end

  def finalize_close(sockRef) do
    nif_finalize_close(sockRef)
  end

  def shutdown(sockRef, how) do
    nif_shutdown(sockRef, how)
  end

  def setopt(sockRef, option, value) do
    nativeValue = 0
    setopt_common(sockRef, option, value, nativeValue)
  end

  def setopt_native(sockRef, option, value) do
    nativeValue = 1
    setopt_common(sockRef, option, value, nativeValue)
  end

  defp setopt_common(sockRef, option, value, nativeValue) do
    case enc_sockopt(option, nativeValue) do
      :undefined ->
        {:error, {:invalid, {:socket_option, option}}}

      :invalid ->
        {:error, {:invalid, {:socket_option, option}}}

      {numLevel, numOpt} ->
        case nif_setopt(sockRef, numLevel, numOpt, value, nativeValue) do
          {:invalid, reason} ->
            case reason do
              :socket_option ->
                {:error, {:invalid, {:socket_option, option}}}

              :value ->
                {:error, {:invalid, {:socket_option, option, value}}}
            end

          result ->
            result
        end
    end
  end

  def getopt(sockRef, option) do
    nativeValue = 0

    case enc_sockopt(option, nativeValue) do
      :undefined ->
        {:error, {:invalid, {:socket_option, option}}}

      :invalid ->
        {:error, {:invalid, {:socket_option, option}}}

      {numLevel, numOpt} ->
        case nif_getopt(sockRef, numLevel, numOpt) do
          {:invalid, reason} ->
            case reason do
              :socket_option ->
                {:error, {:invalid, {:socket_option, option}}}
            end

          result ->
            getopt_result(result, option)
        end
    end
  end

  defp getopt_result({:ok, val} = result, option) do
    case option do
      {:socket, :protocol} ->
        cond do
          is_atom(val) ->
            result

          is_integer(val) ->
            case p_get(:protocols) do
              %{^val => [protocol | _]} ->
                {:ok, protocol}

              %{} ->
                result
            end
        end

      _ ->
        result
    end
  end

  defp getopt_result(error, _Option) do
    error
  end

  def getopt_native(sockRef, option, valueSpec) do
    nativeValue = 1

    case enc_sockopt(option, nativeValue) do
      :undefined ->
        {:error, {:invalid, {:socket_option, option}}}

      :invalid ->
        {:error, {:invalid, {:socket_option, option}}}

      {numLevel, numOpt} ->
        case nif_getopt(sockRef, numLevel, numOpt, valueSpec) do
          {:invalid, reason} ->
            case reason do
              :value ->
                {:error, {:invalid, {:value_spec, valueSpec}}}
            end

          result ->
            result
        end
    end
  end

  def sockname(ref) do
    nif_sockname(ref)
  end

  def peername(ref) do
    nif_peername(ref)
  end

  def cancel(sRef, op, ref) do
    nif_cancel(sRef, op, ref)
  end

  defp enc_protocol({:raw, protoNum}) when is_integer(protoNum) do
    protoNum
  end

  defp enc_protocol(:default) do
    0
  end

  defp enc_protocol(proto) when is_atom(proto) do
    case p_get(:protocols) do
      %{^proto => num} ->
        num

      %{} ->
        throw({:invalid, {:protocol, proto}})
    end
  end

  defp enc_protocol(proto) when is_integer(proto) do
    proto
  end

  defp enc_protocol(proto) do
    :erlang.error({:invalid, {:protocol, proto}})
  end

  def enc_sockaddr(%{:family => :inet} = sockAddr) do
    :maps.merge(%{:port => 0, :addr => :any}, sockAddr)
  end

  def enc_sockaddr(%{:family => :inet6} = sockAddr) do
    :maps.merge(
      %{:port => 0, :addr => :any, :flowinfo => 0, :scope_id => 0},
      sockAddr
    )
  end

  def enc_sockaddr(%{:family => :local, :path => path} = sockAddr) do
    cond do
      is_list(path) and 0 <= length(path) and
          length(path) <= 255 ->
        binPath = enc_path(path)
        enc_sockaddr(%{sockAddr | :path => binPath})

      is_binary(path) and 0 <= byte_size(path) and
          byte_size(path) <= 255 ->
        sockAddr

      true ->
        :erlang.error({:invalid, {:sockaddr, sockAddr, :path}})
    end
  end

  def enc_sockaddr(%{:family => :local} = sockAddr) do
    :erlang.error({:invalid, {:sockaddr, sockAddr, :path}})
  end

  def enc_sockaddr(%{:family => _} = sockAddr) do
    sockAddr
  end

  def enc_sockaddr(sockAddr) do
    :erlang.error({:invalid, {:sockaddr, sockAddr, :map_or_family}})
  end

  defp enc_path(path) do
    case :unicode.characters_to_binary(
           path,
           :file.native_name_encoding()
         ) do
      {:error, _Bin, _Rest} ->
        throw({:invalid, {:path, path}})

      {:incomplete, _Bin1, _Bin2} ->
        throw({:invalid, {:path, path}})

      binPath when is_binary(binPath) ->
        binPath
    end
  end

  defp enc_msg_flags([]) do
    0
  end

  defp enc_msg_flags(flags) do
    enc_msg_flags(flags, p_get(:msg_flags), 0)
  end

  defp enc_msg_flags([], _Table, val) do
    val
  end

  defp enc_msg_flags([flag | flags], table, val)
       when is_atom(flag) do
    case table do
      %{^flag => v} ->
        enc_msg_flags(flags, table, val ||| v)

      %{} ->
        throw({:invalid, {:msg_flag, flag}})
    end
  end

  defp enc_msg_flags([flag | flags], table, val)
       when is_integer(flag) and 0 <= flag do
    enc_msg_flags(flags, table, val ||| flag)
  end

  defp enc_msg_flags(flags, _Table, _Val) do
    :erlang.error({:invalid, {:msg_flags, flags}})
  end

  defp enc_msg(%{:ctrl => []} = m) do
    enc_msg(:maps.remove(:ctrl, m))
  end

  defp enc_msg(%{:iov => iOV} = m)
       when is_list(iOV) and
              iOV !== [] do
    :maps.map(
      fn
        :iov, iov ->
          :erlang.iolist_to_iovec(iov)

        :addr, addr ->
          enc_sockaddr(addr)

        :ctrl, cmsgs ->
          enc_cmsgs(cmsgs, p_get(:protocols))

        _, v ->
          v
      end,
      m
    )
  end

  defp enc_msg(m) do
    :erlang.error({:invalid, {:msg, m}})
  end

  defp enc_cmsgs(cmsgs, protocols) do
    for %{:level => level} = cmsg <- cmsgs do
      cond do
        is_atom(level) ->
          case protocols do
            %{} when level === :socket ->
              cmsg

            %{^level => l} ->
              %{cmsg | :level => l}

            %{} ->
              throw({:invalid, {:protocol, level}})
          end

        true ->
          cmsg
      end
    end
  end

  defp dec_cmsgs(cmsgs, protocols) do
    for %{:level => level} = cmsg <- cmsgs do
      case protocols do
        %{^level => [l | _]} when is_integer(level) ->
          %{cmsg | :level => l}

        %{} ->
          cmsg
      end
    end
  end

  defp enc_sockopt({:otp = level, opt}, 0 = _NativeValue) do
    case (case opt do
            :debug ->
              1001

            :iow ->
              1002

            :controlling_process ->
              1003

            :rcvbuf ->
              1004

            :rcvctrlbuf ->
              1006

            :sndctrlbuf ->
              1007

            :fd ->
              1008

            :meta ->
              1009

            :use_registry ->
              1010

            :domain ->
              1999

            _ ->
              :invalid
          end) do
      :invalid ->
        :invalid

      numOpt ->
        {level, numOpt}
    end
  end

  defp enc_sockopt({numLevel, numOpt} = numOption, nativeValue)
       when is_integer(numLevel) and is_integer(numOpt) and
              nativeValue !== 0 do
    numOption
  end

  defp enc_sockopt({level, numOpt}, nativeValue)
       when is_atom(level) and is_integer(numOpt) and
              nativeValue !== 0 do
    cond do
      level === :socket ->
        {:socket, numOpt}

      true ->
        case p_get(:protocols) do
          %{^level => numLevel} ->
            {numLevel, numOpt}

          %{} ->
            :invalid
        end
    end
  end

  defp enc_sockopt({level, opt} = option, _NativeValue)
       when is_atom(level) and is_atom(opt) do
    case p_get(:options) do
      %{^option => numOpt} ->
        numOpt

      %{} ->
        :invalid
    end
  end

  defp enc_sockopt(option, _NativeValue) do
    :erlang.error({:invalid, {:socket_option, option}})
  end

  defp p_put(name, value) do
    :persistent_term.put({:prim_socket, name}, value)
  end

  def p_get(name) do
    :persistent_term.get({:prim_socket, name})
  end

  defp nif_info() do
    :erlang.nif_error(:undef)
  end

  defp nif_info(_SRef) do
    :erlang.nif_error(:undef)
  end

  defp nif_command(_Command) do
    :erlang.nif_error(:undef)
  end

  defp nif_supports() do
    :erlang.nif_error(:undef)
  end

  defp nif_supports(_Key) do
    :erlang.nif_error(:undef)
  end

  defp nif_open(_FD, _Opts) do
    :erlang.nif_error(:undef)
  end

  defp nif_open(_Domain, _Type, _Protocol, _Opts) do
    :erlang.nif_error(:undef)
  end

  defp nif_bind(_SRef, _SockAddr) do
    :erlang.nif_error(:undef)
  end

  defp nif_bind(_SRef, _SockAddrs, _Action) do
    :erlang.nif_error(:undef)
  end

  defp nif_connect(_SRef) do
    :erlang.nif_error(:undef)
  end

  defp nif_connect(_SRef, _ConnectRef, _SockAddr) do
    :erlang.nif_error(:undef)
  end

  defp nif_listen(_SRef, _Backlog) do
    :erlang.nif_error(:undef)
  end

  defp nif_accept(_SRef, _Ref) do
    :erlang.nif_error(:undef)
  end

  defp nif_send(_SockRef, _SendRef, _Data, _Flags) do
    :erlang.nif_error(:undef)
  end

  defp nif_sendto(_SRef, _SendRef, _Data, _Dest, _Flags) do
    :erlang.nif_error(:undef)
  end

  defp nif_sendmsg(_SRef, _SendRef, _Msg, _Flags) do
    :erlang.nif_error(:undef)
  end

  defp nif_recv(_SRef, _RecvRef, _Length, _Flags) do
    :erlang.nif_error(:undef)
  end

  defp nif_recvfrom(_SRef, _RecvRef, _Length, _Flags) do
    :erlang.nif_error(:undef)
  end

  defp nif_recvmsg(_SRef, _RecvRef, _BufSz, _CtrlSz, _Flags) do
    :erlang.nif_error(:undef)
  end

  defp nif_close(_SRef) do
    :erlang.nif_error(:undef)
  end

  defp nif_finalize_close(_SRef) do
    :erlang.nif_error(:undef)
  end

  defp nif_shutdown(_SRef, _How) do
    :erlang.nif_error(:undef)
  end

  defp nif_setopt(_Ref, _Lev, _Opt, _Val, _NativeVal) do
    :erlang.nif_error(:undef)
  end

  defp nif_getopt(_Ref, _Lev, _Opt) do
    :erlang.nif_error(:undef)
  end

  defp nif_getopt(_Ref, _Lev, _Opt, _ValSpec) do
    :erlang.nif_error(:undef)
  end

  defp nif_sockname(_Ref) do
    :erlang.nif_error(:undef)
  end

  defp nif_peername(_Ref) do
    :erlang.nif_error(:undef)
  end

  defp nif_cancel(_SRef, _Op, _Ref) do
    :erlang.nif_error(:undef)
  end
end
