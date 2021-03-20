defmodule :m_ssh_system_sup do
  use Bitwise
  @behaviour :supervisor
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

  def start_link(role, address, port, profile, options) do
    name = make_name(address, port, profile)

    :supervisor.start_link({:local, name}, :ssh_system_sup, [
      role,
      address,
      port,
      profile,
      options
    ])
  end

  def init([:server, address, port, profile, options]) do
    supFlags = %{strategy: :one_for_one, intensity: 0, period: 3600}

    childSpecs =
      case :ssh_options.get_value(
             :internal_options,
             :connected_socket,
             options,
             fn ->
               :undefined
             end,
             :ssh_system_sup,
             66
           ) do
        :undefined ->
          [
            %{
              id: id(:ssh_acceptor_sup, address, port, profile),
              start: {:ssh_acceptor_sup, :start_link, [address, port, profile, options]},
              restart: :transient,
              type: :supervisor
            }
          ]

        _ ->
          []
      end

    {:ok, {supFlags, childSpecs}}
  end

  def init([:client, _Address, _Port, _Profile, _Options]) do
    supFlags = %{strategy: :one_for_one, intensity: 0, period: 3600}
    childSpecs = []
    {:ok, {supFlags, childSpecs}}
  end

  def stop_listener(systemSup) do
    {name, acceptorSup, _, _} =
      lookup(
        :ssh_acceptor_sup,
        systemSup
      )

    case :supervisor.terminate_child(acceptorSup, name) do
      :ok ->
        :supervisor.delete_child(acceptorSup, name)

      error ->
        error
    end
  end

  def stop_listener(address, port, profile) do
    stop_listener(system_supervisor(address, port, profile))
  end

  def stop_system(:server, sysSup) do
    try do
      :sshd_sup.stop_child(sysSup)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  def stop_system(:client, sysSup) do
    try do
      :sshc_sup.stop_child(sysSup)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  def stop_system(:server, address, port, profile) do
    try do
      :sshd_sup.stop_child(address, port, profile)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    :ok
  end

  def get_options(sup, address, port, profile) do
    try do
      {:ok, %{start: {:ssh_acceptor_sup, :start_link, [^address, ^port, ^profile, options]}}} =
        :supervisor.get_childspec(
          sup,
          id(
            :ssh_acceptor_sup,
            address,
            port,
            profile
          )
        )

      {:ok, options}
    catch
      _, _ ->
        {:error, :not_found}
    end
  end

  def system_supervisor(address, port, profile) do
    name = make_name(address, port, profile)
    :erlang.whereis(name)
  end

  def subsystem_supervisor(systemSup) do
    {_, child, _, _} = lookup(:ssh_subsystem_sup, systemSup)
    child
  end

  def channel_supervisor(systemSup) do
    :ssh_subsystem_sup.channel_supervisor(subsystem_supervisor(systemSup))
  end

  def connection_supervisor(systemSup) do
    :ssh_subsystem_sup.connection_supervisor(subsystem_supervisor(systemSup))
  end

  def acceptor_supervisor(systemSup) do
    {_, child, _, _} = lookup(:ssh_acceptor_sup, systemSup)
    child
  end

  def start_subsystem(systemSup, role, address, port, profile, options) do
    subsystemSpec = %{
      id: make_ref(),
      start: {:ssh_subsystem_sup, :start_link, [role, address, port, profile, options]},
      restart: :temporary,
      type: :supervisor
    }

    :supervisor.start_child(systemSup, subsystemSpec)
  end

  def stop_subsystem(systemSup, subSys) do
    case (try do
            :lists.keyfind(subSys, 2, :supervisor.which_children(systemSup))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      false ->
        {:error, :not_found}

      {id, _, _, _} ->
        spawn(fn ->
          :supervisor.terminate_child(systemSup, id)
          :supervisor.delete_child(systemSup, id)
        end)

        :ok

      {:EXIT, {:noproc, _}} ->
        :ok

      {:EXIT, {:shutdown, _}} ->
        :ok
    end
  end

  defp id(sup, address, port, profile) do
    {sup, address, port, profile}
  end

  defp make_name(address, port, profile) do
    :erlang.list_to_atom(
      :lists.flatten(
        :io_lib.format(
          'ssh_system_~s_~p_~p_sup',
          [fmt_host(address), port, profile]
        )
      )
    )
  end

  defp fmt_host(iP) when is_tuple(iP) do
    :inet.ntoa(iP)
  end

  defp fmt_host(a) when is_atom(a) do
    a
  end

  defp fmt_host(s) when is_list(s) do
    s
  end

  defp lookup(supModule, systemSup) do
    :lists.keyfind([supModule], 4, :supervisor.which_children(systemSup))
  end
end
