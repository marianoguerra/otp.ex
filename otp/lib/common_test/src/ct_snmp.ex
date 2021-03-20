defmodule :m_ct_snmp do
  use Bitwise
  require Record

  Record.defrecord(:r_varbind, :varbind,
    oid: :undefined,
    variabletype: :undefined,
    value: :undefined,
    org_index: :undefined
  )

  Record.defrecord(:r_ivarbind, :ivarbind,
    status: :noError,
    mibentry: :undefined,
    varbind: :undefined
  )

  Record.defrecord(:r_asn1_type, :asn1_type,
    bertype: :undefined,
    lo: :undefined,
    hi: :undefined,
    assocList: [],
    imported: false,
    aliasname: :undefined,
    implied: false,
    display_hint: :undefined
  )

  Record.defrecord(:r_table_info, :table_info,
    nbr_of_cols: :undefined,
    defvals: [],
    status_col: :undefined,
    not_accessible: :undefined,
    index_types: :undefined,
    first_accessible: 1,
    first_own_index: :undefined
  )

  Record.defrecord(:r_variable_info, :variable_info, defval: :undefined)

  Record.defrecord(:r_me, :me,
    oid: :undefined,
    entrytype: :undefined,
    aliasname: :undefined,
    asn1_type: :undefined,
    access: :undefined,
    mfa: :undefined,
    imported: false,
    assocList: [],
    description: :undefined,
    units: :undefined
  )

  Record.defrecord(:r_trap, :trap,
    trapname: :undefined,
    enterpriseoid: :undefined,
    specificcode: :undefined,
    oidobjects: :undefined,
    description: :undefined
  )

  Record.defrecord(:r_notification, :notification,
    trapname: :undefined,
    oid: :undefined,
    oidobjects: :undefined,
    description: :undefined
  )

  Record.defrecord(:r_mib, :mib,
    misc: [],
    mib_format_version: '3.3',
    name: '',
    module_identity: :undefined,
    mes: [],
    asn1_types: [],
    traps: [],
    variable_infos: [],
    table_infos: [],
    imports: :undefined
  )

  Record.defrecord(:r_module_identity, :module_identity,
    last_updated: :undefined,
    organization: :undefined,
    contact_info: :undefined,
    description: :undefined,
    revisions: :undefined
  )

  Record.defrecord(:r_message, :message,
    version: :undefined,
    community: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_v3_hdr, :v3_hdr,
    msgID: :undefined,
    msgMaxSize: :undefined,
    msgFlags: :undefined,
    msgSecurityModel: :undefined,
    msgSecurityParameters: :undefined,
    hdr_size: :undefined
  )

  Record.defrecord(:r_scopedPdu, :scopedPdu,
    contextEngineID: :undefined,
    contextName: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_usmSecurityParameters, :usmSecurityParameters,
    msgAuthoritativeEngineID: :undefined,
    msgAuthoritativeEngineBoots: :undefined,
    msgAuthoritativeEngineTime: :undefined,
    msgUserName: :undefined,
    msgAuthenticationParameters: :undefined,
    msgPrivacyParameters: :undefined
  )

  Record.defrecord(:r_pdu, :pdu,
    type: :undefined,
    request_id: :undefined,
    error_status: :undefined,
    error_index: :undefined,
    varbinds: :undefined
  )

  Record.defrecord(:r_trappdu, :trappdu,
    enterprise: :undefined,
    agent_addr: :undefined,
    generic_trap: :undefined,
    specific_trap: :undefined,
    time_stamp: :undefined,
    varbinds: :undefined
  )

  Record.defrecord(:r_snmp_variables, :snmp_variables,
    name: :undefined,
    value: :undefined
  )

  Record.defrecord(:r_snmp_incr_timer, :snmp_incr_timer,
    wait_for: :timer.seconds(5),
    factor: 2,
    incr: 0,
    max_retries: :infinity
  )

  Record.defrecord(:r_snmpa_notification_delivery_info, :snmpa_notification_delivery_info,
    tag: :undefined,
    mod: :undefined,
    extra: :undefined
  )

  Record.defrecord(:r_hostent, :hostent,
    h_name: :undefined,
    h_aliases: [],
    h_addrtype: :undefined,
    h_length: :undefined,
    h_addr_list: []
  )

  def start(config, mgrAgentConfName) do
    start(config, mgrAgentConfName, :undefined)
  end

  def start(config, mgrAgentConfName, snmpAppConfName) do
    startManager =
      :ct.get_config(
        {mgrAgentConfName, :start_manager},
        true
      )

    startAgent =
      :ct.get_config(
        {mgrAgentConfName, :start_agent},
        false
      )

    sysName =
      :ct.get_config(
        {mgrAgentConfName, :agent_sysname},
        'ct_test'
      )

    {:ok, hostName} = :inet.gethostname()
    {:ok, addr} = :inet.getaddr(hostName, :inet)
    iP = :erlang.tuple_to_list(addr)

    agentManagerIP =
      :ct.get_config(
        {mgrAgentConfName, :agent_manager_ip},
        iP
      )

    prepare_snmp_env()

    setup_agent(
      startAgent,
      mgrAgentConfName,
      snmpAppConfName,
      config,
      sysName,
      agentManagerIP,
      iP
    )

    setup_manager(startManager, mgrAgentConfName, snmpAppConfName, config, agentManagerIP)
    :ok = start_application(:snmp)
    manager_register(startManager, mgrAgentConfName)
  end

  defp start_application(app) do
    case :application.start(app) do
      {:error, {:already_started, ^app}} ->
        :ok

      else__ ->
        else__
    end
  end

  def stop(config) do
    privDir = :test_server.lookup_config(:priv_dir, config)
    :ok = :application.stop(:snmp)
    :ok = :application.stop(:mnesia)
    mgrDir = :filename.join(privDir, 'mgr')
    confDir = :filename.join(privDir, 'conf')
    dbDir = :filename.join(privDir, 'db')

    try do
      del_dir(mgrDir)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    try do
      del_dir(confDir)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    try do
      del_dir(dbDir)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  def get_values(agent, oids, mgrAgentConfName) do
    [uid | _] = agent_conf(agent, mgrAgentConfName)
    {:ok, snmpReply, _} = :snmpm.sync_get2(uid, target_name(agent), oids)
    snmpReply
  end

  def get_next_values(agent, oids, mgrAgentConfName) do
    [uid | _] = agent_conf(agent, mgrAgentConfName)
    {:ok, snmpReply, _} = :snmpm.sync_get_next2(uid, target_name(agent), oids)
    snmpReply
  end

  def set_values(agent, varsAndVals, mgrAgentConfName, config) do
    privDir = :test_server.lookup_config(:priv_dir, config)
    [uid | _] = agent_conf(agent, mgrAgentConfName)

    oids =
      :lists.map(
        fn {oid, _, _} ->
          oid
        end,
        varsAndVals
      )

    targetName = target_name(agent)
    {:ok, snmpGetReply, _} = :snmpm.sync_get2(uid, targetName, oids)
    {:ok, snmpSetReply, _} = :snmpm.sync_set2(uid, targetName, varsAndVals)

    case snmpSetReply do
      {:noError, 0, _} when privDir != false ->
        log(privDir, agent, snmpGetReply, varsAndVals)

      _ ->
        :set_failed_or_user_did_not_want_to_log
    end

    snmpSetReply
  end

  def set_info(config) do
    privDir = :test_server.lookup_config(:priv_dir, config)
    setLogFile = :filename.join(privDir, 'ct_snmp_set.log')

    case :file.consult(setLogFile) do
      {:ok, setInfo} ->
        :ok = delete_file(setLogFile)
        :lists.reverse(setInfo)

      _ ->
        []
    end
  end

  def register_users(mgrAgentConfName, users) do
    case setup_users(users) do
      :ok ->
        snmpVals = :ct.get_config(mgrAgentConfName)

        oldUsers =
          :ct.get_config(
            {mgrAgentConfName, :users},
            []
          )

        newSnmpVals = :lists.keystore(:users, 1, snmpVals, {:users, users ++ oldUsers})
        :ct_config.update_config(mgrAgentConfName, newSnmpVals)
        :ok

      error ->
        error
    end
  end

  def register_agents(mgrAgentConfName, managedAgents) do
    case setup_managed_agents(
           mgrAgentConfName,
           managedAgents
         ) do
      :ok ->
        snmpVals = :ct.get_config(mgrAgentConfName)

        oldAgents =
          :ct.get_config(
            {mgrAgentConfName, :managed_agents},
            []
          )

        newSnmpVals =
          :lists.keystore(
            :managed_agents,
            1,
            snmpVals,
            {:managed_agents, managedAgents ++ oldAgents}
          )

        :ct_config.update_config(mgrAgentConfName, newSnmpVals)
        :ok

      error ->
        error
    end
  end

  def register_usm_users(mgrAgentConfName, usmUsers) do
    engineID =
      :ct.get_config(
        {mgrAgentConfName, :engine_id},
        'mgrEngine'
      )

    case setup_usm_users(usmUsers, engineID) do
      :ok ->
        snmpVals = :ct.get_config(mgrAgentConfName)

        oldUsmUsers =
          :ct.get_config(
            {mgrAgentConfName, :usm_users},
            []
          )

        newSnmpVals =
          :lists.keystore(:usm_users, 1, snmpVals, {:usm_users, usmUsers ++ oldUsmUsers})

        :ct_config.update_config(mgrAgentConfName, newSnmpVals)
        :ok

      error ->
        error
    end
  end

  def unregister_users(mgrAgentConfName) do
    users =
      for {id, _} <- :ct.get_config({mgrAgentConfName, :users}, []) do
        id
      end

    unregister_users(mgrAgentConfName, users)
  end

  def unregister_users(mgrAgentConfName, users) do
    takedown_users(users)
    snmpVals = :ct.get_config(mgrAgentConfName)

    allUsers =
      :ct.get_config(
        {mgrAgentConfName, :users},
        []
      )

    remainingUsers =
      :lists.filter(
        fn {id, _} ->
          not :lists.member(id, users)
        end,
        allUsers
      )

    newSnmpVals = :lists.keyreplace(:users, 1, snmpVals, {:users, remainingUsers})
    :ct_config.update_config(mgrAgentConfName, newSnmpVals)
    :ok
  end

  def unregister_agents(mgrAgentConfName) do
    managedAgents =
      for {agentName, _} <-
            :ct.get_config(
              {mgrAgentConfName, :managed_agents},
              []
            ) do
        agentName
      end

    unregister_agents(mgrAgentConfName, managedAgents)
  end

  def unregister_agents(mgrAgentConfName, managedAgents) do
    takedown_managed_agents(mgrAgentConfName, managedAgents)
    snmpVals = :ct.get_config(mgrAgentConfName)

    allAgents =
      :ct.get_config(
        {mgrAgentConfName, :managed_agents},
        []
      )

    remainingAgents =
      :lists.filter(
        fn {name, _} ->
          not :lists.member(name, managedAgents)
        end,
        allAgents
      )

    newSnmpVals =
      :lists.keyreplace(:managed_agents, 1, snmpVals, {:managed_agents, remainingAgents})

    :ct_config.update_config(mgrAgentConfName, newSnmpVals)
    :ok
  end

  def unregister_usm_users(mgrAgentConfName) do
    usmUsers =
      for {id, _} <-
            :ct.get_config(
              {mgrAgentConfName, :usm_users},
              []
            ) do
        id
      end

    unregister_usm_users(mgrAgentConfName, usmUsers)
  end

  def unregister_usm_users(mgrAgentConfName, usmUsers) do
    engineID =
      :ct.get_config(
        {mgrAgentConfName, :engine_id},
        'mgrEngine'
      )

    takedown_usm_users(usmUsers, engineID)
    snmpVals = :ct.get_config(mgrAgentConfName)

    allUsmUsers =
      :ct.get_config(
        {mgrAgentConfName, :usm_users},
        []
      )

    remainingUsmUsers =
      :lists.filter(
        fn {id, _} ->
          not :lists.member(id, usmUsers)
        end,
        allUsmUsers
      )

    newSnmpVals = :lists.keyreplace(:usm_users, 1, snmpVals, {:usm_users, remainingUsmUsers})
    :ct_config.update_config(mgrAgentConfName, newSnmpVals)
    :ok
  end

  def load_mibs(mibs) do
    :snmpa.load_mibs(:snmp_master_agent, mibs)
  end

  def unload_mibs(mibs) do
    :snmpa.unload_mibs(:snmp_master_agent, mibs)
  end

  defp prepare_snmp_env() do
    _ = :application.load(:snmp)
    :application.unset_env(:snmp, :agent)
  end

  defp setup_manager(false, _, _, _, _) do
    :ok
  end

  defp setup_manager(true, mgrConfName, snmpConfName, config, iP) do
    privDir = :test_server.lookup_config(:priv_dir, config)

    maxMsgSize =
      :ct.get_config(
        {mgrConfName, :max_msg_size},
        484
      )

    port = :ct.get_config({mgrConfName, :mgr_port}, 5000)
    engineID = :ct.get_config({mgrConfName, :engine_id}, 'mgrEngine')
    mgrDir = :filename.join(privDir, 'mgr')
    users = []
    agents = []
    usms = []
    :ok = make_dir(mgrDir)

    :snmp_config.write_manager_snmp_files(
      mgrDir,
      iP,
      port,
      maxMsgSize,
      engineID,
      users,
      agents,
      usms
    )

    snmpEnv =
      merge_snmp_conf(
        [
          {:config, [{:dir, mgrDir}, {:db_dir, mgrDir}, {:verbosity, :trace}]},
          {:server, [{:verbosity, :trace}]},
          {:net_if, [{:verbosity, :trace}]},
          {:versions, [:v1, :v2, :v3]}
        ],
        :ct.get_config({snmpConfName, :manager})
      )

    :application.set_env(:snmp, :manager, snmpEnv)
  end

  defp setup_agent(false, _, _, _, _, _, _) do
    :ok
  end

  defp setup_agent(true, agentConfName, snmpConfName, config, sysName, managerIP, agentIP) do
    :ok = start_application(:mnesia)
    privDir = :test_server.lookup_config(:priv_dir, config)

    vsns =
      :ct.get_config(
        {agentConfName, :agent_vsns},
        [:v2]
      )

    trapUdp =
      :ct.get_config(
        {agentConfName, :agent_trap_udp},
        5000
      )

    agentUdp =
      :ct.get_config(
        {agentConfName, :agent_udp},
        4000
      )

    notifType =
      :ct.get_config(
        {agentConfName, :agent_notify_type},
        :trap
      )

    secType =
      :ct.get_config(
        {agentConfName, :agent_sec_type},
        :none
      )

    passwd =
      :ct.get_config(
        {agentConfName, :agent_passwd},
        ''
      )

    agentEngineID =
      :ct.get_config(
        {agentConfName, :agent_engine_id},
        'agentEngine'
      )

    agentMaxMsgSize =
      :ct.get_config(
        {agentConfName, :agent_max_msg_size},
        484
      )

    confDir = :filename.join(privDir, 'conf')
    dbDir = :filename.join(privDir, 'db')
    :ok = make_dir(confDir)
    :ok = make_dir(dbDir)

    :snmp_config.write_agent_snmp_files(
      confDir,
      vsns,
      managerIP,
      trapUdp,
      agentIP,
      agentUdp,
      sysName,
      notifType,
      secType,
      passwd,
      agentEngineID,
      agentMaxMsgSize
    )

    override_default_configuration(config, agentConfName)

    snmpEnv =
      merge_snmp_conf(
        [
          {:db_dir, dbDir},
          {:config, [{:dir, confDir}, {:verbosity, :trace}]},
          {:agent_type, :master},
          {:agent_verbosity, :trace},
          {:net_if, [{:verbosity, :trace}]},
          {:versions, vsns}
        ],
        :ct.get_config({snmpConfName, :agent})
      )

    :application.set_env(:snmp, :agent, snmpEnv)
  end

  defp merge_snmp_conf(defaults, :undefined) do
    defaults
  end

  defp merge_snmp_conf(
         [def__ = {key, defList = [p | _]} | defParams],
         userParams
       )
       when is_tuple(p) do
    case :lists.keysearch(key, 1, userParams) do
      false ->
        [def__ | merge_snmp_conf(defParams, userParams)]

      {:value, {^key, userList}} ->
        defList1 =
          for {subKey, val} <- defList,
              :lists.keysearch(subKey, 1, userList) == false do
            {subKey, val}
          end

        [
          {key, defList1 ++ userList}
          | merge_snmp_conf(
              defParams,
              :lists.keydelete(key, 1, userParams)
            )
        ]
    end
  end

  defp merge_snmp_conf([def__ = {key, _} | defParams], userParams) do
    case :lists.keysearch(key, 1, userParams) do
      false ->
        [def__ | merge_snmp_conf(defParams, userParams)]

      {:value, _} ->
        merge_snmp_conf(defParams, userParams)
    end
  end

  defp merge_snmp_conf([], userParams) do
    userParams
  end

  defp manager_register(false, _) do
    :ok
  end

  defp manager_register(true, mgrAgentConfName) do
    agents =
      :ct.get_config(
        {mgrAgentConfName, :managed_agents},
        []
      )

    users = :ct.get_config({mgrAgentConfName, :users}, [])

    usmUsers =
      :ct.get_config(
        {mgrAgentConfName, :usm_users},
        []
      )

    engineID =
      :ct.get_config(
        {mgrAgentConfName, :engine_id},
        'mgrEngine'
      )

    setup_usm_users(usmUsers, engineID)
    setup_users(users)
    setup_managed_agents(mgrAgentConfName, agents)
  end

  defp setup_users(users) do
    while_ok(
      fn {id, [module, data]} ->
        :snmpm.register_user(id, module, data)
      end,
      users
    )
  end

  defp setup_managed_agents(agentConfName, agents) do
    fun = fn {agentName, [uid, agentIp, agentUdpPort, agentConf0]} ->
      newAgentIp =
        case agentIp do
          ipTuple when is_tuple(ipTuple) ->
            ipTuple

          hostName when is_list(hostName) ->
            {:ok, hostent} = :inet.gethostbyname(hostName)
            [ipTuple | _] = r_hostent(hostent, :h_addr_list)
            ipTuple
        end

      agentConf =
        case :lists.keymember(:engine_id, 1, agentConf0) do
          true ->
            agentConf0

          false ->
            defaultEngineID =
              :ct.get_config(
                {agentConfName, :agent_engine_id},
                'agentEngine'
              )

            [{:engine_id, defaultEngineID} | agentConf0]
        end

      :snmpm.register_agent(uid, target_name(agentName), [
        {:address, newAgentIp},
        {:port, agentUdpPort}
        | agentConf
      ])
    end

    while_ok(fun, agents)
  end

  defp setup_usm_users(usmUsers, engineID) do
    while_ok(
      fn {usmUser, conf} ->
        :snmpm.register_usm_user(engineID, usmUser, conf)
      end,
      usmUsers
    )
  end

  defp takedown_users(users) do
    :lists.foreach(
      fn id ->
        :snmpm.unregister_user(id)
      end,
      users
    )
  end

  defp takedown_managed_agents(mgrAgentConfName, managedAgents) do
    :lists.foreach(
      fn agentName ->
        [uid | _] = agent_conf(agentName, mgrAgentConfName)
        :snmpm.unregister_agent(uid, target_name(agentName))
      end,
      managedAgents
    )
  end

  defp takedown_usm_users(usmUsers, engineID) do
    :lists.foreach(
      fn id ->
        :snmpm.unregister_usm_user(engineID, id)
      end,
      usmUsers
    )
  end

  defp log(privDir, agent, {_, _, varbinds}, newVarsAndVals) do
    fun = fn r_varbind(oid: oid, variabletype: type, value: value) ->
      {oid, type, value}
    end

    oldVarsAndVals = :lists.map(fun, varbinds)
    file = :filename.join(privDir, 'ct_snmp_set.log')
    {:ok, fd} = :file.open(file, [:write, :append])
    :io.format(fd, '~p.~n', [{agent, oldVarsAndVals, newVarsAndVals}])
    :ok = :file.close(fd)
    :ok
  end

  defp del_dir(dir) do
    {:ok, files} = :file.list_dir(dir)

    fullPathFiles =
      :lists.map(
        fn file ->
          :filename.join(dir, file)
        end,
        files
      )

    :lists.foreach(&:file.delete/1, fullPathFiles)
    :ok = delete_dir(dir)
    :ok
  end

  defp agent_conf(agent, mgrAgentConfName) do
    agents = :ct.get_config({mgrAgentConfName, :managed_agents})

    case :lists.keysearch(agent, 1, agents) do
      {:value, {^agent, agentConf}} ->
        agentConf

      _ ->
        exit({:error, {:unknown_agent, agent, agents}})
    end
  end

  defp override_default_configuration(config, mgrAgentConfName) do
    override_contexts(
      config,
      :ct.get_config(
        {mgrAgentConfName, :agent_contexts},
        :undefined
      )
    )

    override_community(
      config,
      :ct.get_config(
        {mgrAgentConfName, :agent_community},
        :undefined
      )
    )

    override_sysinfo(
      config,
      :ct.get_config(
        {mgrAgentConfName, :agent_sysinfo},
        :undefined
      )
    )

    override_vacm(
      config,
      :ct.get_config(
        {mgrAgentConfName, :agent_vacm},
        :undefined
      )
    )

    override_usm(
      config,
      :ct.get_config(
        {mgrAgentConfName, :agent_usm},
        :undefined
      )
    )

    override_notify(
      config,
      :ct.get_config(
        {mgrAgentConfName, :agent_notify_def},
        :undefined
      )
    )

    override_target_address(
      config,
      :ct.get_config(
        {mgrAgentConfName, :agent_target_address_def},
        :undefined
      )
    )

    override_target_params(
      config,
      :ct.get_config(
        {mgrAgentConfName, :agent_target_param_def},
        :undefined
      )
    )
  end

  defp override_contexts(_, :undefined) do
    :ok
  end

  defp override_contexts(config, {:data_dir_file, file}) do
    dir = :test_server.lookup_config(:data_dir, config)
    fullPathFile = :filename.join(dir, file)
    {:ok, contextInfo} = :file.consult(fullPathFile)
    override_contexts(config, contextInfo)
  end

  defp override_contexts(config, contexts) do
    dir =
      :filename.join(
        :test_server.lookup_config(
          :priv_dir,
          config
        ),
        'conf'
      )

    file = :filename.join(dir, 'context.conf')
    :ok = delete_file(file)
    :ok = :snmp_config.write_agent_context_config(dir, '', contexts)
  end

  defp override_sysinfo(_, :undefined) do
    :ok
  end

  defp override_sysinfo(config, {:data_dir_file, file}) do
    dir = :test_server.lookup_config(:data_dir, config)
    fullPathFile = :filename.join(dir, file)
    {:ok, sysInfo} = :file.consult(fullPathFile)
    override_sysinfo(config, sysInfo)
  end

  defp override_sysinfo(config, sysInfo) do
    dir =
      :filename.join(
        :test_server.lookup_config(
          :priv_dir,
          config
        ),
        'conf'
      )

    file = :filename.join(dir, 'standard.conf')
    :ok = delete_file(file)
    :ok = :snmp_config.write_agent_standard_config(dir, '', sysInfo)
  end

  defp override_target_address(_, :undefined) do
    :ok
  end

  defp override_target_address(config, {:data_dir_file, file}) do
    dir = :test_server.lookup_config(:data_dir, config)
    fullPathFile = :filename.join(dir, file)
    {:ok, targetAddressConf} = :file.consult(fullPathFile)
    override_target_address(config, targetAddressConf)
  end

  defp override_target_address(config, targetAddressConf) do
    dir =
      :filename.join(
        :test_server.lookup_config(
          :priv_dir,
          config
        ),
        'conf'
      )

    file = :filename.join(dir, 'target_addr.conf')
    :ok = delete_file(file)
    :ok = :snmp_config.write_agent_target_addr_config(dir, '', targetAddressConf)
  end

  defp override_target_params(_, :undefined) do
    :ok
  end

  defp override_target_params(config, {:data_dir_file, file}) do
    dir = :test_server.lookup_config(:data_dir, config)
    fullPathFile = :filename.join(dir, file)
    {:ok, targetParamsConf} = :file.consult(fullPathFile)
    override_target_params(config, targetParamsConf)
  end

  defp override_target_params(config, targetParamsConf) do
    dir =
      :filename.join(
        :test_server.lookup_config(
          :priv_dir,
          config
        ),
        'conf'
      )

    file = :filename.join(dir, 'target_params.conf')
    :ok = delete_file(file)
    :ok = :snmp_config.write_agent_target_params_config(dir, '', targetParamsConf)
  end

  defp override_notify(_, :undefined) do
    :ok
  end

  defp override_notify(config, {:data_dir_file, file}) do
    dir = :test_server.lookup_config(:data_dir, config)
    fullPathFile = :filename.join(dir, file)
    {:ok, notifyConf} = :file.consult(fullPathFile)
    override_notify(config, notifyConf)
  end

  defp override_notify(config, notifyConf) do
    dir =
      :filename.join(
        :test_server.lookup_config(
          :priv_dir,
          config
        ),
        'conf'
      )

    file = :filename.join(dir, 'notify.conf')
    :ok = delete_file(file)
    :ok = :snmp_config.write_agent_notify_config(dir, '', notifyConf)
  end

  defp override_usm(_, :undefined) do
    :ok
  end

  defp override_usm(config, {:data_dir_file, file}) do
    dir = :test_server.lookup_config(:data_dir, config)
    fullPathFile = :filename.join(dir, file)
    {:ok, usmConf} = :file.consult(fullPathFile)
    override_usm(config, usmConf)
  end

  defp override_usm(config, usmConf) do
    dir =
      :filename.join(
        :test_server.lookup_config(
          :priv_dir,
          config
        ),
        'conf'
      )

    file = :filename.join(dir, 'usm.conf')
    :ok = delete_file(file)
    :ok = :snmp_config.write_agent_usm_config(dir, '', usmConf)
  end

  defp override_community(_, :undefined) do
    :ok
  end

  defp override_community(config, {:data_dir_file, file}) do
    dir = :test_server.lookup_config(:data_dir, config)
    fullPathFile = :filename.join(dir, file)
    {:ok, communityConf} = :file.consult(fullPathFile)
    override_community(config, communityConf)
  end

  defp override_community(config, communityConf) do
    dir =
      :filename.join(
        :test_server.lookup_config(
          :priv_dir,
          config
        ),
        'conf'
      )

    file = :filename.join(dir, 'community.conf')
    :ok = delete_file(file)
    :ok = :snmp_config.write_agent_community_config(dir, '', communityConf)
  end

  defp override_vacm(_, :undefined) do
    :ok
  end

  defp override_vacm(config, {:data_dir_file, file}) do
    dir = :test_server.lookup_config(:data_dir, config)
    fullPathFile = :filename.join(dir, file)
    {:ok, vacmConf} = :file.consult(fullPathFile)
    override_vacm(config, vacmConf)
  end

  defp override_vacm(config, vacmConf) do
    dir =
      :filename.join(
        :test_server.lookup_config(
          :priv_dir,
          config
        ),
        'conf'
      )

    file = :filename.join(dir, 'vacm.conf')
    :ok = delete_file(file)
    :ok = :snmp_config.write_agent_vacm_config(dir, '', vacmConf)
  end

  defp target_name(agent) do
    :erlang.atom_to_list(agent)
  end

  defp while_ok(fun, [h | t]) do
    case fun.(h) do
      :ok ->
        while_ok(fun, t)

      error ->
        error
    end
  end

  defp while_ok(_Fun, []) do
    :ok
  end

  defp delete_file(fileName) do
    case :file.delete(fileName) do
      {:error, :enoent} ->
        :ok

      else__ ->
        else__
    end
  end

  defp make_dir(dir) do
    case :file.make_dir(dir) do
      {:error, :eexist} ->
        :ok

      else__ ->
        else__
    end
  end

  defp delete_dir(dir) do
    case :file.del_dir(dir) do
      {:error, :enoent} ->
        :ok

      else__ ->
        else__
    end
  end
end
