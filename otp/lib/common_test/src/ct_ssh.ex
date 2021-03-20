defmodule :m_ct_ssh do
  use Bitwise
  require Record

  Record.defrecord(:r_state, :state,
    ssh_ref: :undefined,
    conn_type: :undefined,
    target: :undefined
  )

  def connect(keyOrName) do
    connect(keyOrName, :host)
  end

  def connect(keyOrName, connType) when is_atom(connType) do
    connect(keyOrName, connType, [])
  end

  def connect(keyOrName, extraOpts) when is_list(extraOpts) do
    connect(keyOrName, :host, extraOpts)
  end

  def connect(keyOrName, connType, extraOpts) do
    case :ct.get_config(keyOrName) do
      :undefined ->
        log(heading(:connect, keyOrName), 'Failed: ~tp\n', [{:not_available, keyOrName}])
        {:error, {:not_available, keyOrName}}

      sSHData ->
        allOpts = extraOpts ++ sSHData

        {connType1, addr, allOpts1} =
          case connType do
            :host ->
              case :proplists.get_value(
                     :ssh,
                     allOpts
                   ) do
                :undefined ->
                  case :proplists.get_value(
                         :sftp,
                         allOpts
                       ) do
                    :undefined ->
                      log(
                        heading(
                          :connect,
                          keyOrName
                        ),
                        'No host information specified!\n',
                        []
                      )

                    sFTPAddr ->
                      {:sftp, sFTPAddr, allOpts}
                  end

                sSHAddr ->
                  {:ssh, sSHAddr, allOpts}
              end

            _ ->
              case :proplists.get_value(
                     connType,
                     allOpts
                   ) do
                :undefined when connType == :ssh ->
                  case :proplists.get_value(
                         :sftp,
                         allOpts
                       ) do
                    :undefined ->
                      {:ssh, :undefined, allOpts}

                    sFTPAddr ->
                      try_log(
                        heading(
                          :connect,
                          keyOrName
                        ),
                        'Note: Opening ssh connection to sftp host.\n',
                        []
                      )

                      {:ssh, sFTPAddr,
                       [
                         {:ssh, sFTPAddr}
                         | :proplists.delete(
                             :sftp,
                             allOpts
                           )
                       ]}
                  end

                :undefined when connType == :sftp ->
                  case :proplists.get_value(
                         :ssh,
                         allOpts
                       ) do
                    :undefined ->
                      {:sftp, :undefined, allOpts}

                    sSHAddr ->
                      try_log(
                        heading(
                          :connect,
                          keyOrName
                        ),
                        'Note: Opening sftp connection to ssh host.\n',
                        []
                      )

                      {:sftp, sSHAddr,
                       [
                         {:sftp, sSHAddr}
                         | :proplists.delete(
                             :ssh,
                             allOpts
                           )
                       ]}
                  end

                sSHorSFTPAddr ->
                  {connType, sSHorSFTPAddr, allOpts}
              end
          end

        case {addr, :proplists.get_value(:port, allOpts1)} do
          {:undefined, _} ->
            log(heading(:connect, keyOrName), 'Failed: ~tp\n', [
              {:not_available, {keyOrName, connType1}}
            ])

            {:error, {:not_available, {keyOrName, connType1}}}

          {_, :undefined} ->
            try_log(heading(:connect, keyOrName), 'Opening ~w connection to ~tp:22\n', [
              connType1,
              addr
            ])

            :ct_gen_conn.start(keyOrName, {connType1, addr, 22}, allOpts1, :ct_ssh)

          {_, port} ->
            try_log(heading(:connect, keyOrName), 'Opening ~w connection to ~tp:~w\n', [
              connType1,
              addr,
              port
            ])

            :ct_gen_conn.start(keyOrName, {connType1, addr, port}, allOpts1, :ct_ssh)
        end
    end
  end

  def disconnect(sSH) do
    case get_handle(sSH) do
      {:ok, pid} ->
        try_log(heading(:disconnect, sSH), 'Handle: ~p', [pid], 5000)

        case :ct_gen_conn.stop(pid) do
          {:error, {:process_down, ^pid, :noproc}} ->
            {:error, :already_closed}

          result ->
            result
        end

      error ->
        error
    end
  end

  def session_open(sSH) do
    call(sSH, {:session_open, 10000})
  end

  def session_open(sSH, timeout) do
    call(sSH, {:session_open, timeout})
  end

  def session_close(sSH, channelId) do
    call(sSH, {:session_close, channelId})
  end

  def exec(sSH, command) do
    exec(sSH, :undefined, command, 10000)
  end

  def exec(sSH, command, timeout) when is_list(command) do
    exec(sSH, :undefined, command, timeout)
  end

  def exec(sSH, channelId, command)
      when is_integer(channelId) do
    exec(sSH, channelId, command, 10000)
  end

  def exec(sSH, channelId, command, timeout) do
    call(sSH, {:exec, channelId, command, timeout})
  end

  def receive_response(sSH, channelId) do
    receive_response(sSH, channelId, :close, 10000)
  end

  def receive_response(sSH, channelId, end__) when is_function(end__) do
    receive_response(sSH, channelId, end__, 10000)
  end

  def receive_response(sSH, channelId, timeout)
      when is_integer(timeout) do
    receive_response(sSH, channelId, :close, timeout)
  end

  def receive_response(sSH, channelId, end__, timeout) do
    call(
      sSH,
      {:receive_response, channelId, end__, timeout}
    )
  end

  def send(sSH, channelId, data) do
    send(sSH, channelId, 0, data, 10000)
  end

  def send(sSH, channelId, data, timeout)
      when is_integer(timeout) do
    send(sSH, channelId, 0, data, timeout)
  end

  def send(sSH, channelId, type, data)
      when is_integer(type) do
    send(sSH, channelId, type, data, 10000)
  end

  def send(sSH, channelId, type, data, timeout) do
    call(sSH, {:send, channelId, type, data, timeout})
  end

  def send_and_receive(sSH, channelId, data) do
    send_and_receive(sSH, channelId, 0, data, :close, 10000)
  end

  def send_and_receive(sSH, channelId, data, end__)
      when is_function(end__) do
    send_and_receive(sSH, channelId, 0, data, end__, 10000)
  end

  def send_and_receive(sSH, channelId, data, timeout)
      when is_integer(timeout) do
    send_and_receive(sSH, channelId, 0, data, :close, timeout)
  end

  def send_and_receive(sSH, channelId, type, data)
      when is_integer(type) do
    send_and_receive(sSH, channelId, type, data, :close, 10000)
  end

  def send_and_receive(sSH, channelId, data, end__, timeout)
      when is_integer(timeout) do
    send_and_receive(sSH, channelId, 0, data, end__, timeout)
  end

  def send_and_receive(sSH, channelId, type, data, timeout)
      when is_integer(type) do
    send_and_receive(sSH, channelId, type, data, :close, timeout)
  end

  def send_and_receive(sSH, channelId, type, data, end__)
      when is_function(end__) do
    send_and_receive(sSH, channelId, type, data, end__, 10000)
  end

  def send_and_receive(sSH, channelId, type, data, end__, timeout) do
    call(
      sSH,
      {:send_and_receive, channelId, type, data, end__, timeout}
    )
  end

  def subsystem(sSH, channelId, subsystem) do
    subsystem(sSH, channelId, subsystem, 10000)
  end

  def subsystem(sSH, channelId, subsystem, timeout) do
    call(sSH, {:subsystem, channelId, subsystem, timeout})
  end

  def shell(sSH, channelId) do
    shell(sSH, channelId, 10000)
  end

  def shell(sSH, channelId, timeout) do
    call(sSH, {:shell, channelId, timeout})
  end

  def sftp_connect(sSH) do
    call(sSH, :sftp_connect)
  end

  def read_file(sSH, file) do
    call(sSH, {:read_file, :sftp, file})
  end

  def read_file(sSH, server, file) do
    call(sSH, {:read_file, server, file})
  end

  def write_file(sSH, file, iolist) do
    call(sSH, {:write_file, :sftp, file, iolist})
  end

  def write_file(sSH, server, file, iolist) do
    call(sSH, {:write_file, server, file, iolist})
  end

  def list_dir(sSH, path) do
    call(sSH, {:list_dir, :sftp, path})
  end

  def list_dir(sSH, server, path) do
    call(sSH, {:list_dir, server, path})
  end

  def open(sSH, file, mode) do
    call(sSH, {:open, :sftp, file, mode})
  end

  def open(sSH, server, file, mode) do
    call(sSH, {:open, server, file, mode})
  end

  def opendir(sSH, path) do
    call(sSH, {:opendir, :sftp, path})
  end

  def opendir(sSH, server, path) do
    call(sSH, {:opendir, server, path})
  end

  def close(sSH, handle) do
    call(sSH, {:close, :sftp, handle})
  end

  def close(sSH, server, handle) do
    call(sSH, {:close, server, handle})
  end

  def read(sSH, handle, len) do
    call(sSH, {:read, :sftp, handle, len})
  end

  def read(sSH, server, handle, len) do
    call(sSH, {:read, server, handle, len})
  end

  def pread(sSH, handle, position, length) do
    call(sSH, {:pread, :sftp, handle, position, length})
  end

  def pread(sSH, server, handle, position, length) do
    call(sSH, {:pread, server, handle, position, length})
  end

  def aread(sSH, handle, len) do
    call(sSH, {:aread, :sftp, handle, len})
  end

  def aread(sSH, server, handle, len) do
    call(sSH, {:aread, server, handle, len})
  end

  def apread(sSH, handle, position, length) do
    call(sSH, {:apread, :sftp, handle, position, length})
  end

  def apread(sSH, server, handle, position, length) do
    call(sSH, {:apread, server, handle, position, length})
  end

  def write(sSH, handle, data) do
    call(sSH, {:write, :sftp, handle, data})
  end

  def write(sSH, server, handle, data) do
    call(sSH, {:write, server, handle, data})
  end

  def pwrite(sSH, handle, position, data) do
    call(sSH, {:pwrite, :sftp, handle, position, data})
  end

  def pwrite(sSH, server, handle, position, data) do
    call(sSH, {:pwrite, server, handle, position, data})
  end

  def awrite(sSH, handle, data) do
    call(sSH, {:awrite, :sftp, handle, data})
  end

  def awrite(sSH, server, handle, data) do
    call(sSH, {:awrite, server, handle, data})
  end

  def apwrite(sSH, handle, position, data) do
    call(sSH, {:apwrite, :sftp, handle, position, data})
  end

  def apwrite(sSH, server, handle, position, data) do
    call(sSH, {:apwrite, server, handle, position, data})
  end

  def position(sSH, handle, location) do
    call(sSH, {:position, :sftp, handle, location})
  end

  def position(sSH, server, handle, location) do
    call(sSH, {:position, server, handle, location})
  end

  def read_file_info(sSH, name) do
    call(sSH, {:read_file_info, :sftp, name})
  end

  def read_file_info(sSH, server, name) do
    call(sSH, {:read_file_info, server, name})
  end

  def get_file_info(sSH, handle) do
    call(sSH, {:get_file_info, :sftp, handle})
  end

  def get_file_info(sSH, server, handle) do
    call(sSH, {:get_file_info, server, handle})
  end

  def read_link_info(sSH, name) do
    call(sSH, {:read_link_info, :sftp, name})
  end

  def read_link_info(sSH, server, name) do
    call(sSH, {:read_link_info, server, name})
  end

  def write_file_info(sSH, name, info) do
    call(sSH, {:write_file_info, :sftp, name, info})
  end

  def write_file_info(sSH, server, name, info) do
    call(sSH, {:write_file_info, server, name, info})
  end

  def read_link(sSH, name) do
    call(sSH, {:read_link, :sftp, name})
  end

  def read_link(sSH, server, name) do
    call(sSH, {:read_link, server, name})
  end

  def make_symlink(sSH, name, target) do
    call(sSH, {:make_symlink, :sftp, name, target})
  end

  def make_symlink(sSH, server, name, target) do
    call(sSH, {:make_symlink, server, name, target})
  end

  def rename(sSH, oldName, newName) do
    call(sSH, {:rename, :sftp, oldName, newName})
  end

  def rename(sSH, server, oldName, newName) do
    call(sSH, {:rename, server, oldName, newName})
  end

  def delete(sSH, name) do
    call(sSH, {:delete, :sftp, name})
  end

  def delete(sSH, server, name) do
    call(sSH, {:delete, server, name})
  end

  def make_dir(sSH, name) do
    call(sSH, {:make_dir, :sftp, name})
  end

  def make_dir(sSH, server, name) do
    call(sSH, {:make_dir, server, name})
  end

  def del_dir(sSH, name) do
    call(sSH, {:del_dir, :sftp, name})
  end

  def del_dir(sSH, server, name) do
    call(sSH, {:del_dir, server, name})
  end

  def init(keyOrName, {connType, addr, port}, allOpts) do
    user = :proplists.get_value(:user, allOpts)

    password =
      case :proplists.get_value(
             :password,
             allOpts
           ) do
        :undefined ->
          ''

        pwd ->
          pwd
      end

    allOpts1 =
      case :proplists.get_value(
             :connect_timeout,
             allOpts
           ) do
        :undefined ->
          [{:connect_timeout, 10000} | allOpts]

        _ ->
          allOpts
      end

    options =
      :lists.foldl(
        fn
          {:ssh, _}, opts ->
            opts

          {:sftp, _}, opts ->
            opts

          {:port, _}, opts ->
            opts

          {:silently_accept_hosts, _}, opts ->
            opts

          {:user_interaction, _}, opts ->
            opts

          opt = {key, _}, opts ->
            case :lists.keymember(key, 1, opts) do
              true ->
                opts

              false ->
                [opt | opts]
            end

          _, opts ->
            opts
        end,
        [],
        allOpts1
      )

    finalOptions = [{:silently_accept_hosts, true}, {:user_interaction, false} | options]
    _ = :crypto.start()
    _ = :ssh.start()

    result =
      case connType do
        :ssh ->
          :ssh.connect(addr, port, finalOptions)

        :sftp ->
          :ssh_sftp.start_channel(addr, port, finalOptions)
      end

    case result do
      error = {:error, _} ->
        error

      ok ->
        sSHRef = :erlang.element(2, ok)

        try_log(
          heading(:init, keyOrName),
          'Opened ~w connection:\nHost: ~tp (~p)\nUser: ~tp\nPassword: ~p\n',
          [
            connType,
            addr,
            port,
            user,
            :lists.duplicate(
              :string.length(password),
              ?*
            )
          ]
        )

        {:ok, sSHRef, r_state(ssh_ref: sSHRef, conn_type: connType, target: keyOrName)}
    end
  end

  def handle_msg(:sftp_connect, state) do
    r_state(ssh_ref: sSHRef, target: target) = state
    try_log(heading(:sftp_connect, target), 'SSH Ref: ~p', [sSHRef])
    {:ssh_sftp.start_channel(sSHRef), state}
  end

  def handle_msg({:session_open, tO}, state) do
    r_state(ssh_ref: sSHRef, target: target) = state
    try_log(heading(:session_open, target), 'SSH Ref: ~p, Timeout: ~p', [sSHRef, tO])
    {:ssh_connection.session_channel(sSHRef, tO), state}
  end

  def handle_msg({:session_close, chn}, state) do
    r_state(ssh_ref: sSHRef, target: target) = state
    try_log(heading(:session_close, target), 'SSH Ref: ~p, Chn: ~p', [sSHRef, chn])
    {:ssh_connection.close(sSHRef, chn), state}
  end

  def handle_msg({:exec, chn, command, tO}, state) do
    r_state(ssh_ref: sSHRef, target: target) = state

    chn1 =
      cond do
        chn == :undefined ->
          try_log(heading(:exec, target), 'Opening channel for exec, SSH Ref: ~p', [sSHRef])

          case :ssh_connection.session_channel(sSHRef, tO) do
            {:ok, c} ->
              c

            cErr ->
              cErr
          end

        true ->
          chn
      end

    case chn1 do
      {:error, _} = chnError ->
        log(heading(:exec, target), 'Opening channel failed: ~tp', [chnError])
        {chnError, state}

      _ ->
        try_log(heading(:exec, target), 'SSH Ref: ~p, Chn: ~p, Command: ~tp, Timeout: ~p', [
          sSHRef,
          chn1,
          command,
          tO
        ])

        case :ssh_connection.exec(sSHRef, chn1, command, tO) do
          :success ->
            result = do_recv_response(sSHRef, chn1, [], :close, tO)
            :ssh_connection.close(sSHRef, chn1)
            {result, state}

          other ->
            {{:error, other}, state}
        end
    end
  end

  def handle_msg({:receive_response, chn, end__, tO}, state) do
    r_state(ssh_ref: sSHRef, target: target) = state

    try_log(heading(:receive_response, target), 'SSH Ref: ~p, Chn: ~p, Timeout: ~p', [
      sSHRef,
      chn,
      tO
    ])

    result = do_recv_response(sSHRef, chn, [], end__, tO)
    {result, state}
  end

  def handle_msg({:send, chn, type, data, tO}, state) do
    r_state(ssh_ref: sSHRef, target: target) = state

    try_log(heading(:send, target), 'SSH Ref: ~p, Chn: ~p, Type: ~p, Timeout: ~p~nData: ~tp', [
      sSHRef,
      chn,
      type,
      tO,
      data
    ])

    result = :ssh_connection.send(sSHRef, chn, type, data, tO)
    {result, state}
  end

  def handle_msg(
        {:send_and_receive, chn, type, data, end__, tO},
        state
      ) do
    r_state(ssh_ref: sSHRef, target: target) = state

    try_log(
      heading(:send_and_receive, target),
      'SSH Ref: ~p, Chn: ~p, Type: ~p, Timeout: ~p~nData: ~tp',
      [sSHRef, chn, type, tO, data]
    )

    case :ssh_connection.send(sSHRef, chn, type, data, tO) do
      :ok ->
        result = do_recv_response(sSHRef, chn, [], end__, tO)
        {result, state}

      error ->
        {error, state}
    end
  end

  def handle_msg({:subsystem, chn, subsystem, tO}, state) do
    r_state(ssh_ref: sSHRef, target: target) = state

    try_log(heading(:subsystem, target), 'SSH Ref: ~p, Chn: ~p, Subsys: ~tp, Timeout: ~p', [
      sSHRef,
      chn,
      subsystem,
      tO
    ])

    result = :ssh_connection.subsystem(sSHRef, chn, subsystem, tO)
    {result, state}
  end

  def handle_msg({:shell, chn, tO}, state) do
    r_state(ssh_ref: sSHRef, target: target) = state
    try_log(heading(:shell, target), 'SSH Ref: ~p, Chn: ~p, Timeout: ~p', [sSHRef, chn, tO])
    result = :ssh_connection.shell(sSHRef, chn)
    {result, state}
  end

  def handle_msg(
        {:read_file, srv, file} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.read_file(ref(srv, sSHRef), file), s}
  end

  def handle_msg(
        {:write_file, srv, file, iolist} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.write_file(ref(srv, sSHRef), file, iolist), s}
  end

  def handle_msg(
        {:list_dir, srv, path} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.list_dir(ref(srv, sSHRef), path), s}
  end

  def handle_msg(
        {:open, srv, file, mode} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.open(ref(srv, sSHRef), file, mode), s}
  end

  def handle_msg(
        {:opendir, srv, path} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.opendir(ref(srv, sSHRef), path), s}
  end

  def handle_msg(
        {:close, srv, handle} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.close(ref(srv, sSHRef), handle), s}
  end

  def handle_msg(
        {:read, srv, handle, len} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.read(ref(srv, sSHRef), handle, len), s}
  end

  def handle_msg(
        {:pread, srv, handle, position, length} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.pread(ref(srv, sSHRef), handle, position, length), s}
  end

  def handle_msg(
        {:aread, srv, handle, len} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.aread(ref(srv, sSHRef), handle, len), s}
  end

  def handle_msg(
        {:apread, srv, handle, position, length} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.apread(ref(srv, sSHRef), handle, position, length), s}
  end

  def handle_msg(
        {:write, srv, handle, data} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.write(ref(srv, sSHRef), handle, data), s}
  end

  def handle_msg(
        {:pwrite, srv, handle, position, data} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.pwrite(ref(srv, sSHRef), handle, position, data), s}
  end

  def handle_msg(
        {:awrite, srv, handle, data} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.awrite(ref(srv, sSHRef), handle, data), s}
  end

  def handle_msg(
        {:apwrite, srv, handle, position, data} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.apwrite(ref(srv, sSHRef), handle, position, data), s}
  end

  def handle_msg(
        {:position, srv, handle, location} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.position(ref(srv, sSHRef), handle, location), s}
  end

  def handle_msg(
        {:read_file_info, srv, name} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.read_file_info(ref(srv, sSHRef), name), s}
  end

  def handle_msg(
        {:get_file_info, srv, handle} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.get_file_info(ref(srv, sSHRef), handle), s}
  end

  def handle_msg(
        {:read_link_info, srv, name} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.read_link_info(ref(srv, sSHRef), name), s}
  end

  def handle_msg(
        {:write_file_info, srv, name, info} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.write_file_info(ref(srv, sSHRef), name, info), s}
  end

  def handle_msg(
        {:read_link, srv, name} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.read_link(ref(srv, sSHRef), name), s}
  end

  def handle_msg(
        {:make_symlink, srv, name, target} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.make_symlink(ref(srv, sSHRef), name, target), s}
  end

  def handle_msg(
        {:rename, srv, oldName, newName} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.rename(ref(srv, sSHRef), oldName, newName), s}
  end

  def handle_msg(
        {:delete, srv, name} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.delete(ref(srv, sSHRef), name), s}
  end

  def handle_msg(
        {:make_dir, srv, name} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.make_dir(ref(srv, sSHRef), name), s}
  end

  def handle_msg(
        {:del_dir, srv, name} = cmd,
        s = r_state(ssh_ref: sSHRef)
      ) do
    try_log(heading(:sftp, r_state(s, :target)), 'SSH Ref: ~p, Server: ~p~nCmd: ~tp', [
      sSHRef,
      ref(srv, sSHRef),
      mod(cmd)
    ])

    {:ssh_sftp.del_dir(ref(srv, sSHRef), name), s}
  end

  def reconnect(_Addr, _State) do
    {:error, :no_reconnection_of_ssh}
  end

  def close(sSHRef) do
    disconnect(sSHRef)
  end

  def terminate(sSHRef, state) do
    case r_state(state, :conn_type) do
      :ssh ->
        try_log(heading(:disconnect_ssh, r_state(state, :target)), 'SSH Ref: ~p', [sSHRef], 5000)
        :ssh.close(sSHRef)

      :sftp ->
        try_log(
          heading(:disconnect_sftp, r_state(state, :target)),
          'SFTP Ref: ~p',
          [sSHRef],
          5000
        )

        :ssh_sftp.stop_channel(sSHRef)
    end
  end

  defp do_recv_response(sSH, chn, data, end__, timeout) do
    receive do
      {:ssh_cm, ^sSH, {:open, ^chn, remoteChn, {:session}}} ->
        debug('RECVD open')
        {:ok, {:open, chn, remoteChn, {:session}}}

      {:ssh_cm, ^sSH, {:closed, ^chn}} ->
        :ssh_connection.close(sSH, chn)
        debug('CLSD~n~p ~p', [sSH, chn])
        {:ok, data}

      {:ssh_cm, ^sSH, {:data, ^chn, _, newData}} ->
        :ssh_connection.adjust_window(sSH, chn, :erlang.size(newData))
        debug('RECVD~n~tp', [:erlang.binary_to_list(newData)])
        dataAcc = data ++ :erlang.binary_to_list(newData)

        cond do
          is_function(end__) ->
            case end__.(dataAcc) do
              true ->
                {:ok, dataAcc}

              false ->
                do_recv_response(sSH, chn, dataAcc, end__, timeout)
            end

          true ->
            do_recv_response(sSH, chn, dataAcc, end__, timeout)
        end

      {:ssh_cm, ^sSH, {:eof, ^chn}} ->
        debug('RECVD EOF~n~p ~p', [sSH, chn])
        {:ok, data}

      {:ssh_cm, ^sSH, {:exit_signal, ^chn, signal, err, _Lang}} ->
        debug('RECVD exit_signal~n~p ~p~n~p ~p', [sSH, chn, signal, err])
        do_recv_response(sSH, chn, data, end__, timeout)

      {:ssh_cm, ^sSH, {:exit_status, ^chn, status}} ->
        debug('RECVD exit_status~n~p ~p~n~p', [sSH, chn, status])
        do_recv_response(sSH, chn, data, end__, timeout)

      other ->
        debug('UNEXPECTED MESSAGE~n~p ~p~n~tp', [sSH, chn, other])
        do_recv_response(sSH, chn, data, end__, timeout)
    after
      timeout ->
        case end__ do
          :timeout ->
            {:ok, data}

          _ ->
            {:timeout, data}
        end
    end
  end

  defp get_handle(sSH) when is_pid(sSH) do
    {:ok, sSH}
  end

  defp get_handle(sSH) do
    case :ct_util.get_connection(sSH, :ct_ssh) do
      {:ok, {pid, _}} ->
        {:ok, pid}

      {:error, :no_registered_connection} ->
        connect(sSH)

      error ->
        error
    end
  end

  defp call(sSH, msg) do
    call(sSH, msg, :infinity)
  end

  defp call(sSH, msg, timeout) do
    case get_handle(sSH) do
      {:ok, pid} ->
        :ct_gen_conn.call(pid, msg, timeout)

      error ->
        error
    end
  end

  defp ref(:sftp, sSHRef) do
    sSHRef
  end

  defp ref(server, _) do
    server
  end

  defp mod(cmd) do
    [op, _Server | args] = :erlang.tuple_to_list(cmd)
    :erlang.list_to_tuple([op | args])
  end

  defp heading(function, ref) do
    :io_lib.format('ct_ssh:~tw ~tp', [function, ref])
  end

  defp log(heading, str, args) do
    :ct_gen_conn.log(heading, str, args)
  end

  defp try_log(heading, str, args) do
    try_log(heading, str, args, :infinity)
  end

  defp try_log(heading, str, args, timeout) do
    case :ct_util.is_silenced(:ssh, timeout) do
      true ->
        :ok

      false ->
        :ct_gen_conn.log(heading, str, args)

      _Error ->
        :ok
    end
  end

  defp debug(str) do
    debug(str, [])
  end

  defp debug(_Str, _Args) do
    :ok
  end
end
