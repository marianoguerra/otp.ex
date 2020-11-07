defmodule :m_inet_sctp do
  use Bitwise
  require Record

  Record.defrecord(:r_sctp_initmsg, :sctp_initmsg,
    num_ostreams: :undefined,
    max_instreams: :undefined,
    max_attempts: :undefined,
    max_init_timeo: :undefined
  )

  Record.defrecord(:r_sctp_sndrcvinfo, :sctp_sndrcvinfo,
    stream: :undefined,
    ssn: :undefined,
    flags: :undefined,
    ppid: :undefined,
    context: :undefined,
    timetolive: :undefined,
    tsn: :undefined,
    cumtsn: :undefined,
    assoc_id: :undefined
  )

  Record.defrecord(:r_sctp_assoc_change, :sctp_assoc_change,
    state: :cant_assoc,
    error: 0,
    outbound_streams: 0,
    inbound_streams: 0,
    assoc_id: 0
  )

  Record.defrecord(:r_sctp_paddr_change, :sctp_paddr_change,
    addr: [0, 0, 0, 0],
    state: :addr_available,
    error: 0,
    assoc_id: 0
  )

  Record.defrecord(:r_sctp_remote_error, :sctp_remote_error, error: 0, assoc_id: 0, data: [])

  Record.defrecord(:r_sctp_send_failed, :sctp_send_failed,
    flags: false,
    error: 0,
    info: :EFE_TODO_NESTED_RECORD,
    assoc_id: 0,
    data: <<>>
  )

  Record.defrecord(:r_sctp_shutdown_event, :sctp_shutdown_event, assoc_id: 0)

  Record.defrecord(:r_sctp_adaptation_event, :sctp_adaptation_event,
    adaptation_ind: 0,
    assoc_id: 0
  )

  Record.defrecord(:r_sctp_pdapi_event, :sctp_pdapi_event,
    indication: :partial_delivery_aborted,
    assoc_id: 0
  )

  Record.defrecord(:r_sctp_rtoinfo, :sctp_rtoinfo,
    assoc_id: :undefined,
    initial: :undefined,
    max: :undefined,
    min: :undefined
  )

  Record.defrecord(:r_sctp_assocparams, :sctp_assocparams,
    assoc_id: :undefined,
    asocmaxrxt: :undefined,
    number_peer_destinations: :undefined,
    peer_rwnd: :undefined,
    local_rwnd: :undefined,
    cookie_life: :undefined
  )

  Record.defrecord(:r_sctp_prim, :sctp_prim,
    assoc_id: :undefined,
    addr: :undefined
  )

  Record.defrecord(:r_sctp_setpeerprim, :sctp_setpeerprim,
    assoc_id: :undefined,
    addr: :undefined
  )

  Record.defrecord(:r_sctp_setadaptation, :sctp_setadaptation, adaptation_ind: :undefined)

  Record.defrecord(:r_sctp_paddrparams, :sctp_paddrparams,
    assoc_id: :undefined,
    address: :undefined,
    hbinterval: :undefined,
    pathmaxrxt: :undefined,
    pathmtu: :undefined,
    sackdelay: :undefined,
    flags: :undefined
  )

  Record.defrecord(:r_sctp_event_subscribe, :sctp_event_subscribe,
    data_io_event: :undefined,
    association_event: :undefined,
    address_event: :undefined,
    send_failure_event: :undefined,
    peer_error_event: :undefined,
    shutdown_event: :undefined,
    partial_delivery_event: :undefined,
    adaptation_layer_event: :undefined,
    authentication_event: :undefined
  )

  Record.defrecord(:r_sctp_assoc_value, :sctp_assoc_value,
    assoc_id: :undefined,
    assoc_value: :undefined
  )

  Record.defrecord(:r_sctp_paddrinfo, :sctp_paddrinfo,
    assoc_id: :undefined,
    address: :undefined,
    state: :undefined,
    cwnd: :undefined,
    srtt: :undefined,
    rto: :undefined,
    mtu: :undefined
  )

  Record.defrecord(:r_sctp_status, :sctp_status,
    assoc_id: :undefined,
    state: :undefined,
    rwnd: :undefined,
    unackdata: :undefined,
    penddata: :undefined,
    instrms: :undefined,
    outstrms: :undefined,
    fragmentation_point: :undefined,
    primary: :undefined
  )

  Record.defrecord(:r_connect_opts, :connect_opts, ifaddr: :any, port: 0, fd: -1, opts: [])

  Record.defrecord(:r_listen_opts, :listen_opts,
    ifaddr: :any,
    port: 0,
    backlog: 5,
    fd: -1,
    opts: []
  )

  Record.defrecord(:r_udp_opts, :udp_opts, ifaddr: :any, port: 0, fd: -1, opts: [{:active, true}])

  Record.defrecord(:r_sctp_opts, :sctp_opts,
    ifaddr: :undefined,
    port: 0,
    fd: -1,
    type: :seqpacket,
    opts: [
      {:mode, :binary},
      {:buffer, 65536},
      {:sndbuf, 65536},
      {:recbuf, 1024},
      {:sctp_events, :undefined}
    ]
  )

  def getserv(port) when is_integer(port) do
    {:ok, port}
  end

  def getserv(name) when is_atom(name) do
    :inet.getservbyname(name, :sctp)
  end

  def getserv(_) do
    {:error, :einval}
  end

  def getaddr(address) do
    :inet.getaddr(address, :inet)
  end

  def getaddr(address, timer) do
    :inet.getaddr_tm(address, :inet, timer)
  end

  def translate_ip(iP) do
    :inet.translate_ip(iP, :inet)
  end

  def open(opts) do
    case :inet.sctp_options(opts, :inet_sctp) do
      {:ok, r_sctp_opts(fd: fd, ifaddr: addr, port: port, type: type, opts: sOs)} ->
        :inet.open(fd, addr, port, sOs, :sctp, :inet, type, :inet_sctp)

      error ->
        error
    end
  end

  def close(s) do
    :prim_inet.close(s)
  end

  def listen(s, flag) do
    :prim_inet.listen(s, flag)
  end

  def peeloff(s, assocId) do
    case :prim_inet.peeloff(s, assocId) do
      {:ok, newS} = result ->
        :inet_db.register_socket(newS, :inet_sctp)
        result

      error ->
        error
    end
  end

  def connect(s, addr, port, opts, timer) do
    case :prim_inet.chgopts(s, opts) do
      :ok ->
        case :prim_inet.getopt(s, :active) do
          {:ok, active} ->
            timeout =
              cond do
                timer === :nowait ->
                  :infinity

                true ->
                  :inet.timeout(timer)
              end

            case :prim_inet.connect(s, addr, port, timeout) do
              :ok when timer !== :nowait ->
                connect_get_assoc(s, addr, port, active, timer)

              okOrErr1 ->
                okOrErr1
            end

          err2 ->
            err2
        end

      err3 ->
        err3
    end
  end

  defp connect_get_assoc(s, addr, port, false, timer) do
    case recv(s, :inet.timeout(timer)) do
      {:ok, {^addr, ^port, _, r_sctp_assoc_change(state: st) = ev}} ->
        cond do
          st === :comm_up ->
            {:ok, ev}

          true ->
            {:error, ev}
        end

      {:ok, msg} ->
        {:error, msg}

      {:error, _} = error ->
        error
    end
  end

  defp connect_get_assoc(s, addr, port, active, timer) do
    timeout = :inet.timeout(timer)

    receive do
      {:sctp, ^s, ^addr, ^port, {_, r_sctp_assoc_change(state: st) = ev}} ->
        setOptRes =
          case active do
            :once ->
              :prim_inet.setopt(s, :active, :once)

            _ ->
              :ok
          end

        case {st, setOptRes} do
          {:comm_up, :ok} ->
            {:ok, ev}

          {_, :ok} ->
            {:error, ev}

          {_, error} ->
            error
        end
    after
      timeout ->
        {:error, :timeout}
    end
  end

  def sendmsg(s, sRI, data) do
    :prim_inet.sendmsg(s, sRI, data)
  end

  def send(s, assocId, stream, data) do
    case :prim_inet.getopts(
           s,
           [{:sctp_default_send_param, r_sctp_sndrcvinfo(assoc_id: assocId)}]
         ) do
      {:ok,
       [
         {:sctp_default_send_param,
          r_sctp_sndrcvinfo(flags: flags, context: context, ppid: pPID, timetolive: tTL)}
       ]} ->
        :prim_inet.sendmsg(
          s,
          r_sctp_sndrcvinfo(
            flags: flags,
            context: context,
            ppid: pPID,
            timetolive: tTL,
            assoc_id: assocId,
            stream: stream
          ),
          data
        )

      _ ->
        :prim_inet.sendmsg(s, r_sctp_sndrcvinfo(assoc_id: assocId, stream: stream), data)
    end
  end

  def recv(s, timeout) do
    :prim_inet.recvfrom(s, 0, timeout)
  end
end
