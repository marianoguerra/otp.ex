defmodule :m_socket do
  use Bitwise
  import Kernel, except: [send: 2]

  def number_of() do
    :socket_registry.number_of()
  end

  def which_sockets() do
    :socket_registry.which_sockets(fn _ ->
      true
    end)
  end

  def which_sockets(domain)
      when domain === :inet or domain === :inet6 do
    :socket_registry.which_sockets(fn
      %{:domain => d}
      when d === domain ->
        true

      _ ->
        false
    end)
  end

  def which_sockets(type)
      when type === :stream or type === :dgram or type === :seqpacket do
    :socket_registry.which_sockets(fn
      %{:type => t}
      when t === type ->
        true

      _ ->
        false
    end)
  end

  def which_sockets(proto)
      when proto === :sctp or proto === :tcp or proto === :udp do
    :socket_registry.which_sockets(fn
      %{:protocol => p}
      when p === proto ->
        true

      _ ->
        false
    end)
  end

  def which_sockets(cTRL) when is_pid(cTRL) do
    :socket_registry.which_sockets(fn
      %{:ctrl => c}
      when c === cTRL ->
        true

      _ ->
        false
    end)
  end

  def which_sockets(filter) when is_function(filter, 1) do
    :socket_registry.which_sockets(filter)
  end

  def which_sockets(other) do
    :erlang.error(:badarg, [other])
  end

  def info() do
    :prim_socket.info()
  end

  def debug(d) when is_boolean(d) do
    :prim_socket.debug(d)
  end

  def debug(d) do
    :erlang.error(:badarg, [d])
  end

  def socket_debug(d) when is_boolean(d) do
    :prim_socket.socket_debug(d)
  end

  def socket_debug(d) do
    :erlang.error(:badarg, [d])
  end

  def use_registry(d) when is_boolean(d) do
    :prim_socket.use_registry(d)
  end

  def info({:"$socket", sockRef}) when is_reference(sockRef) do
    :prim_socket.info(sockRef)
  end

  def info(socket) do
    :erlang.error(:badarg, [socket])
  end

  def supports() do
    for key1 <- [:options, :msg_flags, :protocols] do
      {key1, supports(key1)}
    end ++ :prim_socket.supports()
  end

  def supports(key) do
    :prim_socket.supports(key)
  end

  def supports(key1, key2) do
    :prim_socket.supports(key1, key2)
  end

  def is_supported(key1) do
    :prim_socket.is_supported(key1)
  end

  def is_supported(key1, key2) do
    :prim_socket.is_supported(key1, key2)
  end

  def is_supported(:options, level, opt)
      when is_atom(level) and
             is_atom(opt) do
    is_supported(:options, {level, opt})
  end

  def open(fD) when is_integer(fD) do
    open(fD, %{})
  end

  def open(fD) do
    :erlang.error(:badarg, [fD])
  end

  def open(fD, opts)
      when is_integer(fD) and
             is_map(opts) do
    case :prim_socket.open(fD, opts) do
      {:ok, sockRef} ->
        socket = {:"$socket", sockRef}
        {:ok, socket}

      {:error, _} = eRROR ->
        eRROR
    end
  end

  def open(domain, type) do
    open(domain, type, 0)
  end

  def open(domain, type, opts) when is_map(opts) do
    open(domain, type, 0, opts)
  end

  def open(domain, type, protocol) do
    open(domain, type, protocol, %{})
  end

  def open(domain, type, protocol, opts)
      when is_map(opts) do
    case :prim_socket.open(domain, type, protocol, opts) do
      {:ok, sockRef} ->
        socket = {:"$socket", sockRef}
        {:ok, socket}

      {:error, _} = eRROR ->
        eRROR
    end
  end

  def open(domain, type, protocol, opts) do
    :erlang.error(:badarg, [domain, type, protocol, opts])
  end

  def bind({:"$socket", sockRef} = socket, addr)
      when is_reference(sockRef) do
    cond do
      is_map(addr) ->
        :prim_socket.bind(sockRef, addr)

      addr === :any or addr === :broadcast or
          addr === :loopback ->
        case :prim_socket.getopt(sockRef, {:otp, :domain}) do
          {:ok, domain} when domain === :inet or domain === :inet6 ->
            :prim_socket.bind(
              sockRef,
              %{:family => domain, :addr => addr}
            )

          {:ok, _Domain} ->
            {:error, :eafnosupport}

          {:error, _} = eRROR ->
            eRROR
        end

      true ->
        :erlang.error(:badarg, [socket, addr])
    end
  end

  def bind(socket, addr) do
    :erlang.error(:badarg, [socket, addr])
  end

  def bind({:"$socket", sockRef}, addrs, action)
      when is_reference(sockRef) and is_list(addrs) and (action === :add or action === :remove) do
    :prim_socket.bind(sockRef, addrs, action)
  end

  def bind(socket, addrs, action) do
    :erlang.error(:badarg, [socket, addrs, action])
  end

  def connect(socket, sockAddr) do
    connect(socket, sockAddr, :infinity)
  end

  def connect({:"$socket", sockRef} = socket, sockAddr, timeout)
      when is_reference(sockRef) do
    case deadline(timeout) do
      :badarg = reason ->
        :erlang.error(reason, [socket, sockAddr, timeout])

      :nowait ->
        selectHandle = make_ref()
        connect_nowait(sockRef, sockAddr, selectHandle)

      :select_handle ->
        selectHandle = timeout
        connect_nowait(sockRef, sockAddr, selectHandle)

      deadline ->
        connect_deadline(sockRef, sockAddr, deadline)
    end
  end

  def connect(socket, sockAddr, timeout) do
    :erlang.error(:badarg, [socket, sockAddr, timeout])
  end

  defp connect_nowait(sockRef, sockAddr, selectHandle) do
    case :prim_socket.connect(sockRef, selectHandle, sockAddr) do
      :select ->
        {:select, {:select_info, :connect, selectHandle}}

      result ->
        result
    end
  end

  defp connect_deadline(sockRef, sockAddr, deadline) do
    ref = make_ref()

    case :prim_socket.connect(sockRef, ref, sockAddr) do
      :select ->
        timeout = timeout(deadline)

        receive do
          {:"$socket", _Socket, :select, ^ref} ->
            :prim_socket.connect(sockRef)

          {:"$socket", _Socket, :abort, {^ref, reason}} ->
            {:error, reason}
        after
          timeout ->
            cancel(sockRef, :connect, ref)
            {:error, :timeout}
        end

      result ->
        result
    end
  end

  def connect({:"$socket", sockRef}) when is_reference(sockRef) do
    :prim_socket.connect(sockRef)
  end

  def connect(socket) do
    :erlang.error(:badarg, [socket])
  end

  def listen(socket) do
    listen(socket, 5)
  end

  def listen({:"$socket", sockRef}, backlog)
      when is_reference(sockRef) and is_integer(backlog) do
    :prim_socket.listen(sockRef, backlog)
  end

  def listen(socket, backlog) do
    :erlang.error(:badarg, [socket, backlog])
  end

  def accept(listenSocket) do
    accept(listenSocket, :infinity)
  end

  def accept({:"$socket", lSockRef} = listenSocket, timeout)
      when is_reference(lSockRef) do
    case deadline(timeout) do
      :badarg = reason ->
        :erlang.error(reason, [listenSocket, timeout])

      :nowait ->
        selectHandle = make_ref()
        accept_nowait(lSockRef, selectHandle)

      :select_handle ->
        selectHandle = timeout
        accept_nowait(lSockRef, selectHandle)

      deadline ->
        accept_deadline(lSockRef, deadline)
    end
  end

  def accept(listenSocket, timeout) do
    :erlang.error(:badarg, [listenSocket, timeout])
  end

  defp accept_nowait(lSockRef, selectHandle) do
    case :prim_socket.accept(lSockRef, selectHandle) do
      :select ->
        {:select, {:select_info, :accept, selectHandle}}

      result ->
        accept_result(lSockRef, selectHandle, result)
    end
  end

  defp accept_deadline(lSockRef, deadline) do
    accRef = make_ref()

    case :prim_socket.accept(lSockRef, accRef) do
      :select ->
        timeout = timeout(deadline)

        receive do
          {:"$socket", {:"$socket", ^lSockRef}, :select, ^accRef} ->
            accept_deadline(lSockRef, deadline)

          {:"$socket", _Socket, :abort, {^accRef, reason}} ->
            {:error, reason}
        after
          timeout ->
            cancel(lSockRef, :accept, accRef)
            {:error, :timeout}
        end

      result ->
        accept_result(lSockRef, accRef, result)
    end
  end

  defp accept_result(lSockRef, accRef, result) do
    case result do
      {:ok, sockRef} ->
        socket = {:"$socket", sockRef}
        {:ok, socket}

      {:error, _} = eRROR ->
        cancel(lSockRef, :accept, accRef)
        eRROR
    end
  end

  def send(socket, data) do
    send(socket, data, [], :infinity)
  end

  def send(socket, data, flags) when is_list(flags) do
    send(socket, data, flags, :infinity)
  end

  def send(socket, data, timeout) do
    send(socket, data, [], timeout)
  end

  def send(socket, [bin], flags, timeout)
      when is_binary(bin) do
    send(socket, bin, flags, timeout)
  end

  def send(socket, data, flags, timeout)
      when is_list(data) do
    bin = :erlang.list_to_binary(data)
    send(socket, bin, flags, timeout)
  end

  def send({:"$socket", sockRef} = socket, data, flags, timeout)
      when is_reference(sockRef) and is_binary(data) and
             is_list(flags) do
    to = :undefined

    case deadline(timeout) do
      :badarg = reason ->
        :erlang.error(reason, [socket, data, flags, timeout])

      :nowait ->
        selectHandle = make_ref()
        send_common_nowait(sockRef, data, to, flags, selectHandle, :send)

      :select_handle ->
        selectHandle = timeout
        send_common_nowait(sockRef, data, to, flags, selectHandle, :send)

      deadline ->
        send_common_deadline(sockRef, data, to, flags, deadline, :send)
    end
  end

  def send(socket, data, flags, timeout) do
    :erlang.error(:badarg, [socket, data, flags, timeout])
  end

  defp send_common_nowait(sockRef, data, to, flags, selectHandle, sendName) do
    case (case sendName do
            :send ->
              :prim_socket.send(sockRef, selectHandle, data, flags)

            :sendto ->
              :prim_socket.sendto(sockRef, selectHandle, data, to, flags)
          end) do
      :ok ->
        :ok

      {:ok, written} ->
        <<_::size(written)-binary, rest::binary>> = data
        {:ok, {[rest], {:select_info, sendName, selectHandle}}}

      :select ->
        {:select, {:select_info, sendName, selectHandle}}

      {:error, reason} ->
        send_common_error(reason, data)
    end
  end

  defp send_common_deadline(sockRef, data, to, flags, deadline, sendName) do
    selectHandle = make_ref()

    case (case sendName do
            :send ->
              :prim_socket.send(sockRef, selectHandle, data, flags)

            :sendto ->
              :prim_socket.sendto(sockRef, selectHandle, data, to, flags)
          end) do
      :ok ->
        :ok

      {:ok, written} ->
        timeout = timeout(deadline)

        receive do
          {:"$socket", _Socket, :select, ^selectHandle} when written > 0 ->
            <<_::size(written)-binary, rest::binary>> = data
            send_common_deadline(sockRef, rest, to, flags, deadline, sendName)

          {:"$socket", _Socket, :select, ^selectHandle} ->
            send_common_deadline(sockRef, data, to, flags, deadline, sendName)

          {:"$socket", _Socket, :abort, {^selectHandle, reason}} ->
            send_common_error(reason, data)
        after
          timeout ->
            _ = cancel(sockRef, sendName, selectHandle)
            send_common_error(:timeout, data)
        end

      :select ->
        timeout = timeout(deadline)

        receive do
          {:"$socket", _Socket, :select, ^selectHandle} ->
            send_common_deadline(sockRef, data, to, flags, deadline, sendName)

          {:"$socket", _Socket, :abort, {^selectHandle, reason}} ->
            send_common_error(reason, data)
        after
          timeout ->
            _ = cancel(sockRef, sendName, selectHandle)
            send_common_error(:timeout, data)
        end

      {:error, reason} ->
        send_common_error(reason, data)
    end
  end

  defp send_common_error(reason, data) do
    {:error, {reason, [data]}}
  end

  def sendto(socket, data, dest) do
    sendto(socket, data, dest, [])
  end

  def sendto(socket, data, dest, flags) when is_list(flags) do
    sendto(socket, data, dest, flags, :infinity)
  end

  def sendto(socket, data, dest, timeout) do
    sendto(socket, data, dest, [], timeout)
  end

  def sendto(socket, [bin], dest, flags, timeout)
      when is_binary(bin) do
    sendto(socket, bin, dest, flags, timeout)
  end

  def sendto(socket, data, dest, flags, timeout)
      when is_list(data) do
    bin = :erlang.list_to_binary(data)
    sendto(socket, bin, dest, flags, timeout)
  end

  def sendto({:"$socket", sockRef} = socket, data, dest, flags, timeout)
      when is_reference(sockRef) and is_binary(data) and
             is_list(flags) do
    case deadline(timeout) do
      :badarg = reason ->
        :erlang.error(
          reason,
          [socket, data, dest, flags, timeout]
        )

      :nowait ->
        selectHandle = make_ref()
        send_common_nowait(sockRef, data, dest, flags, selectHandle, :sendto)

      :select_handle ->
        selectHandle = timeout
        send_common_nowait(sockRef, data, dest, flags, selectHandle, :sendto)

      deadline ->
        send_common_deadline(sockRef, data, dest, flags, deadline, :sendto)
    end
  end

  def sendto(socket, data, dest, flags, timeout) do
    :erlang.error(
      :badarg,
      [socket, data, dest, flags, timeout]
    )
  end

  def sendmsg(socket, msg) do
    sendmsg(socket, msg, [], :infinity)
  end

  def sendmsg(socket, msg, flags) when is_list(flags) do
    sendmsg(socket, msg, flags, :infinity)
  end

  def sendmsg(socket, msg, timeout) do
    sendmsg(socket, msg, [], timeout)
  end

  def sendmsg({:"$socket", sockRef} = socket, msg, flags, timeout)
      when is_reference(sockRef) and is_map(msg) and
             is_list(flags) do
    case deadline(timeout) do
      :badarg = reason ->
        :erlang.error(reason, [socket, msg, flags, timeout])

      :nowait ->
        selectHandle = make_ref()
        sendmsg_nowait(sockRef, msg, flags, selectHandle)

      :select_handle ->
        selectHandle = timeout
        sendmsg_nowait(sockRef, msg, flags, selectHandle)

      deadline ->
        sendmsg_deadline(sockRef, msg, flags, deadline)
    end
  end

  def sendmsg(socket, msg, flags, timeout) do
    :erlang.error(:badarg, [socket, msg, flags, timeout])
  end

  defp sendmsg_nowait(sockRef, msg, flags, selectHandle) do
    case :prim_socket.sendmsg(sockRef, selectHandle, msg, flags) do
      :ok ->
        :ok

      {:ok, written} when is_integer(written) and written > 0 ->
        _ = cancel(sockRef, :sendmsg, selectHandle)
        {:ok, sendmsg_rest(:maps.get(:iov, msg), written)}

      :select ->
        {:select, {:select_info, :sendmsg, selectHandle}}

      {:error, _} = eRROR ->
        eRROR
    end
  end

  defp sendmsg_deadline(sockRef, msg, flags, deadline) do
    selectHandle = make_ref()

    case :prim_socket.sendmsg(sockRef, selectHandle, msg, flags) do
      :ok ->
        :ok

      {:ok, written} when is_integer(written) and written > 0 ->
        _ = cancel(sockRef, :sendmsg, selectHandle)
        {:ok, sendmsg_rest(:maps.get(:iov, msg), written)}

      :select ->
        timeout = timeout(deadline)

        receive do
          {:"$socket", {:"$socket", ^sockRef}, :select, ^selectHandle} ->
            sendmsg_deadline(sockRef, msg, flags, deadline)

          {:"$socket", _Socket, :abort, {^selectHandle, reason}} ->
            {:error, reason}
        after
          timeout ->
            _ = cancel(sockRef, :sendmsg, selectHandle)
            {:error, :timeout}
        end

      {:error, _} = eRROR ->
        eRROR
    end
  end

  defp sendmsg_rest([b | iOVec], written)
       when written >= byte_size(b) do
    sendmsg_rest(iOVec, written - byte_size(b))
  end

  defp sendmsg_rest([b | iOVec], written) do
    <<_::size(written)-binary, rest::binary>> = b
    [rest | iOVec]
  end

  def recv(socket) do
    recv(socket, 0)
  end

  def recv(socket, length) do
    recv(socket, length, [], :infinity)
  end

  def recv(socket, length, flags) when is_list(flags) do
    recv(socket, length, flags, :infinity)
  end

  def recv(socket, length, timeout) do
    recv(socket, length, [], timeout)
  end

  def recv({:"$socket", sockRef} = socket, length, flags, timeout)
      when is_reference(sockRef) and is_integer(length) and
             length >= 0 and is_list(flags) do
    case deadline(timeout) do
      :badarg = reason ->
        :erlang.error(reason, [socket, length, flags, timeout])

      :nowait ->
        selectHandle = make_ref()
        recv_nowait(sockRef, length, flags, selectHandle, <<>>)

      :select_handle ->
        selectHandle = timeout
        recv_nowait(sockRef, length, flags, selectHandle, <<>>)

      deadline ->
        recv_deadline(sockRef, length, flags, deadline, <<>>)
    end
  end

  def recv(socket, length, flags, timeout) do
    :erlang.error(:badarg, [socket, length, flags, timeout])
  end

  defp recv_nowait(sockRef, length, flags, selectHandle, acc) do
    case :prim_socket.recv(sockRef, selectHandle, length, flags) do
      {:more, bin} ->
        {:ok, bincat(acc, bin)}

      {:select, bin} ->
        {:ok, {bincat(acc, bin), {:select_info, :recv, selectHandle}}}

      :select ->
        cond do
          byte_size(acc) === 0 ->
            {:select, {:select_info, :recv, selectHandle}}

          true ->
            {:ok, {acc, {:select_info, :recv, selectHandle}}}
        end

      result ->
        recv_result(acc, result)
    end
  end

  defp recv_deadline(sockRef, length, flags, deadline, acc) do
    selectHandle = make_ref()

    case :prim_socket.recv(sockRef, selectHandle, length, flags) do
      {:more, bin} ->
        timeout = timeout(deadline)

        cond do
          0 < timeout ->
            recv_deadline(sockRef, length, flags, deadline, bincat(acc, bin))

          true ->
            {:ok, bincat(acc, bin)}
        end

      {:select, bin} ->
        timeout = timeout(deadline)

        receive do
          {:"$socket", {:"$socket", ^sockRef}, :select, ^selectHandle} ->
            cond do
              0 < timeout ->
                recv_deadline(sockRef, length - byte_size(bin), flags, deadline, bincat(acc, bin))

              true ->
                {:error, {:timeout, bincat(acc, bin)}}
            end

          {:"$socket", _Socket, :abort, {^selectHandle, reason}} ->
            {:error, {reason, bincat(acc, bin)}}
        after
          timeout ->
            cancel(sockRef, :recv, selectHandle)
            {:error, {:timeout, bincat(acc, bin)}}
        end

      :select when length === 0 and 0 < byte_size(acc) ->
        cancel(sockRef, :recv, selectHandle)
        {:ok, acc}

      :select ->
        timeout = timeout(deadline)

        receive do
          {:"$socket", {:"$socket", ^sockRef}, :select, ^selectHandle} ->
            cond do
              0 < timeout ->
                recv_deadline(sockRef, length, flags, deadline, acc)

              true ->
                recv_error(acc, :timeout)
            end

          {:"$socket", _Socket, :abort, {^selectHandle, reason}} ->
            recv_error(acc, reason)
        after
          timeout ->
            cancel(sockRef, :recv, selectHandle)
            recv_error(acc, :timeout)
        end

      result ->
        recv_result(acc, result)
    end
  end

  defp recv_result(acc, result) do
    case result do
      {:ok, bin} ->
        {:ok, bincat(acc, bin)}

      {:error, _} = eRROR when byte_size(acc) === 0 ->
        eRROR

      {:error, reason} ->
        {:error, {reason, acc}}
    end
  end

  defp recv_error(acc, reason) do
    cond do
      byte_size(acc) === 0 ->
        {:error, reason}

      true ->
        {:error, {reason, acc}}
    end
  end

  def recvfrom(socket) do
    recvfrom(socket, 0)
  end

  def recvfrom(socket, bufSz) do
    recvfrom(socket, bufSz, [], :infinity)
  end

  def recvfrom(socket, flags, timeout) when is_list(flags) do
    recvfrom(socket, 0, flags, timeout)
  end

  def recvfrom(socket, bufSz, flags) when is_list(flags) do
    recvfrom(socket, bufSz, flags, :infinity)
  end

  def recvfrom(socket, bufSz, timeout) do
    recvfrom(socket, bufSz, [], timeout)
  end

  def recvfrom({:"$socket", sockRef} = socket, bufSz, flags, timeout)
      when is_reference(sockRef) and is_integer(bufSz) and
             0 <= bufSz and is_list(flags) do
    case deadline(timeout) do
      :badarg = reason ->
        :erlang.error(reason, [socket, bufSz, flags, timeout])

      :nowait ->
        selectHandle = make_ref()
        recvfrom_nowait(sockRef, bufSz, selectHandle, flags)

      :select_handle ->
        selectHandle = timeout
        recvfrom_nowait(sockRef, bufSz, selectHandle, flags)

      deadline ->
        recvfrom_deadline(sockRef, bufSz, flags, deadline)
    end
  end

  def recvfrom(socket, bufSz, flags, timeout) do
    :erlang.error(:badarg, [socket, bufSz, flags, timeout])
  end

  defp recvfrom_nowait(sockRef, bufSz, selectHandle, flags) do
    case :prim_socket.recvfrom(sockRef, selectHandle, bufSz, flags) do
      :select ->
        {:select, {:select_info, :recvfrom, selectHandle}}

      result ->
        recvfrom_result(result)
    end
  end

  defp recvfrom_deadline(sockRef, bufSz, flags, deadline) do
    selectHandle = make_ref()

    case :prim_socket.recvfrom(sockRef, selectHandle, bufSz, flags) do
      :select ->
        timeout = timeout(deadline)

        receive do
          {:"$socket", {:"$socket", ^sockRef}, :select, ^selectHandle} ->
            recvfrom_deadline(sockRef, bufSz, flags, deadline)

          {:"$socket", _Socket, :abort, {^selectHandle, reason}} ->
            {:error, reason}
        after
          timeout ->
            cancel(sockRef, :recvfrom, selectHandle)
            {:error, :timeout}
        end

      result ->
        recvfrom_result(result)
    end
  end

  defp recvfrom_result(result) do
    case result do
      {:ok, {_Source, _NewData}} = oK ->
        oK

      {:error, _Reason} = eRROR ->
        eRROR
    end
  end

  def recvmsg(socket) do
    recvmsg(socket, 0, 0, [], :infinity)
  end

  def recvmsg(socket, flags) when is_list(flags) do
    recvmsg(socket, 0, 0, flags, :infinity)
  end

  def recvmsg(socket, timeout) do
    recvmsg(socket, 0, 0, [], timeout)
  end

  def recvmsg(socket, flags, timeout) when is_list(flags) do
    recvmsg(socket, 0, 0, flags, timeout)
  end

  def recvmsg(socket, bufSz, ctrlSz)
      when is_integer(bufSz) and is_integer(ctrlSz) do
    recvmsg(socket, bufSz, ctrlSz, [], :infinity)
  end

  def recvmsg({:"$socket", sockRef} = socket, bufSz, ctrlSz, flags, timeout)
      when is_reference(sockRef) and is_integer(bufSz) and
             0 <= bufSz and is_integer(ctrlSz) and 0 <= ctrlSz and
             is_list(flags) do
    case deadline(timeout) do
      :badarg = reason ->
        :erlang.error(
          reason,
          [socket, bufSz, ctrlSz, flags, timeout]
        )

      :nowait ->
        selectHandle = make_ref()
        recvmsg_nowait(sockRef, bufSz, ctrlSz, flags, selectHandle)

      :select_handle ->
        selectHandle = timeout
        recvmsg_nowait(sockRef, bufSz, ctrlSz, flags, selectHandle)

      deadline ->
        recvmsg_deadline(sockRef, bufSz, ctrlSz, flags, deadline)
    end
  end

  def recvmsg(socket, bufSz, ctrlSz, flags, timeout) do
    :erlang.error(
      :badarg,
      [socket, bufSz, ctrlSz, flags, timeout]
    )
  end

  defp recvmsg_nowait(sockRef, bufSz, ctrlSz, flags, selectHandle) do
    case :prim_socket.recvmsg(sockRef, selectHandle, bufSz, ctrlSz, flags) do
      :select ->
        {:select, {:select_info, :recvmsg, selectHandle}}

      result ->
        recvmsg_result(result)
    end
  end

  defp recvmsg_deadline(sockRef, bufSz, ctrlSz, flags, deadline) do
    selectHandle = make_ref()

    case :prim_socket.recvmsg(sockRef, selectHandle, bufSz, ctrlSz, flags) do
      :select ->
        timeout = timeout(deadline)

        receive do
          {:"$socket", {:"$socket", ^sockRef}, :select, ^selectHandle} ->
            recvmsg_deadline(sockRef, bufSz, ctrlSz, flags, deadline)

          {:"$socket", _Socket, :abort, {^selectHandle, reason}} ->
            {:error, reason}
        after
          timeout ->
            cancel(sockRef, :recvmsg, selectHandle)
            {:error, :timeout}
        end

      {:error, :ealready = reason} ->
        :erlang.error(reason)

      result ->
        recvmsg_result(result)
    end
  end

  defp recvmsg_result(result) do
    case result do
      {:ok, _Msg} = oK ->
        oK

      {:error, _Reason} = eRROR ->
        eRROR
    end
  end

  def close({:"$socket", sockRef}) when is_reference(sockRef) do
    case :prim_socket.close(sockRef) do
      :ok ->
        :prim_socket.finalize_close(sockRef)

      {:ok, closeRef} ->
        receive do
          {:"$socket", {:"$socket", ^sockRef}, :close, ^closeRef} ->
            :prim_socket.finalize_close(sockRef)
        end

      {:error, _} = eRROR ->
        eRROR
    end
  end

  def close(socket) do
    :erlang.error(:badarg, [socket])
  end

  def shutdown({:"$socket", sockRef}, how) when is_reference(sockRef) do
    :prim_socket.shutdown(sockRef, how)
  end

  def shutdown(socket, how) do
    :erlang.error(:badarg, [socket, how])
  end

  def setopt({:"$socket", sockRef}, socketOption, value)
      when is_reference(sockRef) do
    :prim_socket.setopt(sockRef, socketOption, value)
  end

  def setopt(socket, socketOption, value) do
    :erlang.error(:badarg, [socket, socketOption, value])
  end

  def setopt(socket, level, opt, value)
      when is_integer(opt) and is_binary(value) do
    setopt_native(socket, {level, opt}, value)
  end

  def setopt(socket, level, opt, value) do
    setopt(socket, {level, opt}, value)
  end

  def setopt_native({:"$socket", sockRef}, socketOption, value)
      when is_reference(sockRef) do
    :prim_socket.setopt_native(sockRef, socketOption, value)
  end

  def setopt_native(socket, socketOption, value) do
    :erlang.error(:badarg, [socket, socketOption, value])
  end

  def getopt({:"$socket", sockRef}, socketOption)
      when is_reference(sockRef) do
    :prim_socket.getopt(sockRef, socketOption)
  end

  def getopt(socket, level, {nativeOpt, valueSpec})
      when is_integer(nativeOpt) do
    getopt_native(socket, {level, nativeOpt}, valueSpec)
  end

  def getopt(socket, level, opt) do
    getopt(socket, {level, opt})
  end

  def getopt_native({:"$socket", sockRef}, socketOption, valueSpec) do
    :prim_socket.getopt_native(sockRef, socketOption, valueSpec)
  end

  def sockname({:"$socket", sockRef}) when is_reference(sockRef) do
    :prim_socket.sockname(sockRef)
  end

  def sockname(socket) do
    :erlang.error(:badarg, [socket])
  end

  def peername({:"$socket", sockRef}) when is_reference(sockRef) do
    :prim_socket.peername(sockRef)
  end

  def peername(socket) do
    :erlang.error(:badarg, [socket])
  end

  def cancel({:"$socket", sockRef}, {:select_info, tag, ref})
      when is_reference(sockRef) do
    cancel(sockRef, tag, ref)
  end

  def cancel(socket, selectInfo) do
    :erlang.error(:badarg, [socket, selectInfo])
  end

  defp cancel(sockRef, op, opRef) do
    case :prim_socket.cancel(sockRef, op, opRef) do
      :select_sent ->
        flush_select_msg(sockRef, opRef)
        _ = flush_abort_msg(sockRef, opRef)
        :ok

      :not_found ->
        _ = flush_abort_msg(sockRef, opRef)
        {:error, {:invalid, {:select_info, op, opRef}}}

      result ->
        _ = flush_abort_msg(sockRef, opRef)
        result
    end
  end

  defp flush_select_msg(sockRef, ref) do
    receive do
      {:"$socket", {:"$socket", ^sockRef}, :select, ^ref} ->
        :ok
    after
      0 ->
        :ok
    end
  end

  defp flush_abort_msg(sockRef, ref) do
    receive do
      {:"$socket", {:"$socket", ^sockRef}, :abort, {^ref, reason}} ->
        reason
    after
      0 ->
        :ok
    end
  end

  defp deadline(timeout) do
    case timeout do
      :nowait ->
        timeout

      :infinity ->
        timeout

      0 ->
        :zero

      _ when is_integer(timeout) and 0 < timeout ->
        timestamp() + timeout

      _ when is_reference(timeout) ->
        :select_handle

      _ ->
        :badarg
    end
  end

  defp timeout(deadline) do
    case deadline do
      :infinity ->
        deadline

      :zero ->
        0

      _ ->
        now = timestamp()

        cond do
          deadline > now ->
            deadline - now

          true ->
            0
        end
    end
  end

  defp timestamp() do
    :erlang.monotonic_time(:milli_seconds)
  end

  defp bincat(<<>>, <<_::binary>> = b) do
    b
  end

  defp bincat(<<_::binary>> = a, <<>>) do
    a
  end

  defp bincat(<<_::binary>> = a, <<_::binary>> = b) do
    <<a::binary, b::binary>>
  end
end
