defmodule :m_gen_sctp do
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

  def open() do
    open([])
  end

  def open(opts0) when is_list(opts0) do
    {mod, opts} = :inet.sctp_module(opts0)

    case mod.open(opts) do
      {:error, :badarg} ->
        :erlang.error(:badarg, [opts])

      {:error, :einval} ->
        :erlang.error(:badarg, [opts])

      result ->
        result
    end
  end

  def open(port) when is_integer(port) do
    open([{:port, port}])
  end

  def open(x) do
    :erlang.error(:badarg, [x])
  end

  def open(port, opts)
      when is_integer(port) and
             is_list(opts) do
    open([{:port, port} | opts])
  end

  def open(port, opts) do
    :erlang.error(:badarg, [port, opts])
  end

  def close(s) when is_port(s) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        mod.close(s)

      {:error, :closed} ->
        :ok
    end
  end

  def close(s) do
    :erlang.error(:badarg, [s])
  end

  def listen(s, backlog)
      when (is_port(s) and
              is_boolean(backlog)) or
             (is_port(s) and is_integer(backlog)) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        mod.listen(s, backlog)

      error ->
        error
    end
  end

  def listen(s, flag) do
    :erlang.error(:badarg, [s, flag])
  end

  def peeloff(s, r_sctp_assoc_change(assoc_id: assocId)) when is_port(s) do
    peeloff(s, assocId)
  end

  def peeloff(s, assocId)
      when is_port(s) and
             is_integer(assocId) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        mod.peeloff(s, assocId)

      error ->
        error
    end
  end

  def connect(s, addr, port, opts) do
    connect(s, addr, port, opts, :infinity)
  end

  def connect(s, addr, port, opts, timeout) do
    case do_connect(s, addr, port, opts, timeout, true) do
      :badarg ->
        :erlang.error(:badarg, [s, addr, port, opts, timeout])

      result ->
        result
    end
  end

  def connect_init(s, addr, port, opts) do
    connect_init(s, addr, port, opts, :infinity)
  end

  def connect_init(s, addr, port, opts, timeout) do
    case do_connect(s, addr, port, opts, timeout, false) do
      :badarg ->
        :erlang.error(:badarg, [s, addr, port, opts, timeout])

      result ->
        result
    end
  end

  defp do_connect(s, addr, port, opts, timeout, connWait)
       when is_port(s) and is_list(opts) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        case mod.getserv(port) do
          {:ok, ^port} ->
            try do
              :inet.start_timer(timeout)
            catch
              :error, :badarg ->
                :badarg
            else
              timer ->
                try do
                  mod.getaddr(addr, timer)
                else
                  {:ok, iP} ->
                    connectTimer =
                      cond do
                        connWait == false ->
                          :nowait

                        true ->
                          timer
                      end

                    mod.connect(s, iP, port, opts, connectTimer)

                  error ->
                    error
                after
                  _ = :inet.stop_timer(timer)
                end
            end

          error ->
            error
        end

      error ->
        error
    end
  end

  defp do_connect(_S, _Addr, _Port, _Opts, _Timeout, _ConnWait) do
    :badarg
  end

  def eof(s, r_sctp_assoc_change(assoc_id: assocId)) when is_port(s) do
    eof_or_abort(s, assocId, :eof)
  end

  def eof(s, assoc) do
    :erlang.error(:badarg, [s, assoc])
  end

  def abort(s, r_sctp_assoc_change(assoc_id: assocId)) when is_port(s) do
    eof_or_abort(s, assocId, :abort)
  end

  def abort(s, assoc) do
    :erlang.error(:badarg, [s, assoc])
  end

  defp eof_or_abort(s, assocId, action) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        mod.sendmsg(s, r_sctp_sndrcvinfo(assoc_id: assocId, flags: [action]), <<>>)

      error ->
        error
    end
  end

  def send(s, r_sctp_sndrcvinfo() = sRI, data) when is_port(s) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        mod.sendmsg(s, sRI, data)

      error ->
        error
    end
  end

  def send(s, sRI, data) do
    :erlang.error(:badarg, [s, sRI, data])
  end

  def send(s, r_sctp_assoc_change(assoc_id: assocId), stream, data)
      when is_port(s) and is_integer(stream) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        mod.send(s, assocId, stream, data)

      error ->
        error
    end
  end

  def send(s, assocId, stream, data)
      when is_port(s) and
             is_integer(assocId) and
             is_integer(stream) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        mod.send(s, assocId, stream, data)

      error ->
        error
    end
  end

  def send(s, assocChange, stream, data) do
    :erlang.error(:badarg, [s, assocChange, stream, data])
  end

  def recv(s) do
    recv(s, :infinity)
  end

  def recv(s, timeout) when is_port(s) do
    case :inet_db.lookup_socket(s) do
      {:ok, mod} ->
        mod.recv(s, timeout)

      error ->
        error
    end
  end

  def recv(s, timeout) do
    :erlang.error(:badarg, [s, timeout])
  end

  def error_string(0) do
    :ok
  end

  def error_string(1) do
    'Invalid Stream Identifier'
  end

  def error_string(2) do
    'Missing Mandatory Parameter'
  end

  def error_string(3) do
    'Stale Cookie Error'
  end

  def error_string(4) do
    'Out of Resource'
  end

  def error_string(5) do
    'Unresolvable Address'
  end

  def error_string(6) do
    'Unrecognized Chunk Type'
  end

  def error_string(7) do
    'Invalid Mandatory Parameter'
  end

  def error_string(8) do
    'Unrecognized Parameters'
  end

  def error_string(9) do
    'No User Data'
  end

  def error_string(10) do
    'Cookie Received While Shutting Down'
  end

  def error_string(11) do
    'Restart of an Association with New Addresses'
  end

  def error_string(12) do
    'User Initiated Abort'
  end

  def error_string(13) do
    'Protocol Violation'
  end

  def error_string(n) when is_integer(n) do
    :unknown_error
  end

  def error_string(x) do
    :erlang.error(:badarg, [x])
  end

  def controlling_process(s, pid) when is_port(s) and is_pid(pid) do
    :inet.udp_controlling_process(s, pid)
  end

  def controlling_process(s, pid) do
    :erlang.error(:badarg, [s, pid])
  end
end
