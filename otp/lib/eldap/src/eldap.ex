defmodule :m_eldap do
  use Bitwise
  import Kernel, except: [send: 2]
  import :lists, only: [concat: 1]
  @vc :"$Id$ "
  require Record

  Record.defrecord(:r_eldap_search, :eldap_search,
    base: [],
    filter: [],
    scope: :wholeSubtree,
    deref: :derefAlways,
    attributes: [],
    types_only: false,
    timeout: 0
  )

  Record.defrecord(:r_eldap_search_result, :eldap_search_result,
    entries: [],
    referrals: []
  )

  Record.defrecord(:r_eldap_entry, :eldap_entry,
    object_name: '',
    attributes: []
  )

  Record.defrecord(:r_eldap, :eldap,
    version: 3,
    host: :undefined,
    port: 389,
    fd: :undefined,
    prev_fd: :undefined,
    binddn: '',
    passwd: :undefined,
    id: 0,
    log: :undefined,
    timeout: :infinity,
    anon_auth: false,
    ldaps: false,
    using_tls: false,
    tls_opts: [],
    tcp_opts: []
  )

  def open(hosts) do
    open(hosts, [])
  end

  def open(hosts, opts)
      when is_list(hosts) and
             is_list(opts) do
    self = self()

    pid =
      spawn_link(fn ->
        init(hosts, opts, self)
      end)

    recv(pid)
  end

  def start_tls(handle, tlsOptions) do
    start_tls(handle, tlsOptions, :infinity)
  end

  def start_tls(handle, tlsOptions, timeout) do
    start_tls(handle, tlsOptions, timeout, :asn1_NOVALUE)
  end

  def start_tls(handle, tlsOptions, timeout, controls) do
    send(
      handle,
      {:start_tls, tlsOptions, timeout, controls}
    )

    recv(handle)
  end

  def modify_password(handle, dn, newPasswd) do
    modify_password(handle, dn, newPasswd, [])
  end

  def modify_password(handle, dn, newPasswd, oldPasswd)
      when is_pid(handle) and is_list(dn) and
             is_list(newPasswd) and is_list(oldPasswd) do
    modify_password(handle, dn, newPasswd, oldPasswd, :asn1_NOVALUE)
  end

  def modify_password(handle, dn, newPasswd, oldPasswd, controls)
      when is_pid(handle) and is_list(dn) and
             is_list(newPasswd) and is_list(oldPasswd) do
    send(
      handle,
      {:passwd_modify, optional(dn), optional(newPasswd), optional(oldPasswd), controls}
    )

    recv(handle)
  end

  def getopts(handle, optNames)
      when is_pid(handle) and
             is_list(optNames) do
    send(handle, {:getopts, optNames})
    recv(handle)
  end

  def close(handle) when is_pid(handle) do
    send(handle, :close)
    :ok
  end

  def controlling_process(handle, pid)
      when is_pid(handle) and
             is_pid(pid) do
    :erlang.link(pid)
    send(handle, {:cnt_proc, pid})
    recv(handle)
  end

  def simple_bind(handle, dn, passwd) when is_pid(handle) do
    simple_bind(handle, dn, passwd, :asn1_NOVALUE)
  end

  def simple_bind(handle, dn, passwd, controls)
      when is_pid(handle) do
    send(handle, {:simple_bind, dn, passwd, controls})
    recv(handle)
  end

  def add(handle, entry, attributes)
      when is_pid(handle) and is_list(entry) and
             is_list(attributes) do
    add(handle, entry, attributes, :asn1_NOVALUE)
  end

  def add(handle, entry, attributes, controls)
      when is_pid(handle) and is_list(entry) and
             is_list(attributes) do
    send(
      handle,
      {:add, entry, add_attrs(attributes), controls}
    )

    recv(handle)
  end

  defp add_attrs(attrs) do
    f = fn {type, vals}
           when is_list(type) and
                  is_list(vals) ->
      {:AddRequest_attributes, type, vals}
    end

    case (try do
            :lists.map(f, attrs)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, _} ->
        throw({:error, :attribute_values})

      else__ ->
        else__
    end
  end

  def delete(handle, entry)
      when is_pid(handle) and
             is_list(entry) do
    delete(handle, entry, :asn1_NOVALUE)
  end

  def delete(handle, entry, controls)
      when is_pid(handle) and
             is_list(entry) do
    send(handle, {:delete, entry, controls})
    recv(handle)
  end

  def modify(handle, object, mods)
      when is_pid(handle) and
             is_list(object) and is_list(mods) do
    modify(handle, object, mods, :asn1_NOVALUE)
  end

  def modify(handle, object, mods, controls)
      when is_pid(handle) and is_list(object) and
             is_list(mods) do
    send(handle, {:modify, object, mods, controls})
    recv(handle)
  end

  def mod_add(type, values)
      when is_list(type) and
             is_list(values) do
    m(:add, type, values)
  end

  def mod_delete(type, values)
      when is_list(type) and
             is_list(values) do
    m(:delete, type, values)
  end

  def mod_replace(type, values)
      when is_list(type) and
             is_list(values) do
    m(:replace, type, values)
  end

  defp m(operation, type, values) do
    r_ModifyRequest_changes_SEQOF(
      operation: operation,
      modification: r_PartialAttribute(type: type, vals: values)
    )
  end

  def modify_dn(handle, entry, newRDN, delOldRDN, newSup)
      when is_pid(handle) and is_list(entry) and
             is_list(newRDN) and is_atom(delOldRDN) and
             is_list(newSup) do
    modify_dn(handle, entry, newRDN, delOldRDN, newSup, :asn1_NOVALUE)
  end

  defp modify_dn(handle, entry, newRDN, delOldRDN, newSup, controls)
       when is_pid(handle) and is_list(entry) and
              is_list(newRDN) and is_atom(delOldRDN) and
              is_list(newSup) do
    send(
      handle,
      {:modify_dn, entry, newRDN, bool_p(delOldRDN), optional(newSup), controls}
    )

    recv(handle)
  end

  defp bool_p(bool) when is_boolean(bool) do
    bool
  end

  defp optional([]) do
    :asn1_NOVALUE
  end

  defp optional(value) do
    value
  end

  def search(handle, x)
      when (is_pid(handle) and
              elem(x, 0) === :eldap_search) or
             is_list(x) do
    search(handle, x, :asn1_NOVALUE)
  end

  def search(handle, a, controls)
      when is_pid(handle) and
             elem(a, 0) === :eldap_search do
    call_search(handle, a, controls)
  end

  def search(handle, l, controls)
      when is_pid(handle) and
             is_list(l) do
    case (try do
            parse_search_args(l)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, emsg} ->
        {:error, emsg}

      a when elem(a, 0) === :eldap_search ->
        call_search(handle, a, controls)
    end
  end

  defp call_search(handle, a, controls) do
    send(handle, {:search, a, controls})
    recv(handle)
  end

  defp parse_search_args(args) do
    parse_search_args(
      args,
      r_eldap_search(scope: :wholeSubtree, deref: :derefAlways)
    )
  end

  defp parse_search_args([{:base, base} | t], a) do
    parse_search_args(t, r_eldap_search(a, base: base))
  end

  defp parse_search_args([{:filter, filter} | t], a) do
    parse_search_args(t, r_eldap_search(a, filter: filter))
  end

  defp parse_search_args([{:scope, scope} | t], a) do
    parse_search_args(t, r_eldap_search(a, scope: scope))
  end

  defp parse_search_args([{:deref, deref} | t], a) do
    parse_search_args(t, r_eldap_search(a, deref: deref))
  end

  defp parse_search_args([{:attributes, attrs} | t], a) do
    parse_search_args(t, r_eldap_search(a, attributes: attrs))
  end

  defp parse_search_args([{:types_only, typesOnly} | t], a) do
    parse_search_args(t, r_eldap_search(a, types_only: typesOnly))
  end

  defp parse_search_args([{:timeout, timeout} | t], a)
       when is_integer(timeout) do
    parse_search_args(t, r_eldap_search(a, timeout: timeout))
  end

  defp parse_search_args([h | _], _) do
    throw({:error, {:unknown_arg, h}})
  end

  defp parse_search_args([], a) do
    a
  end

  def baseObject() do
    :baseObject
  end

  def singleLevel() do
    :singleLevel
  end

  def wholeSubtree() do
    :wholeSubtree
  end

  def neverDerefAliases() do
    :neverDerefAliases
  end

  def derefInSearching() do
    :derefInSearching
  end

  def derefFindingBaseObj() do
    :derefFindingBaseObj
  end

  def derefAlways() do
    :derefAlways
  end

  def unquote(:and)(listOfFilters) when is_list(listOfFilters) do
    {:and, listOfFilters}
  end

  def unquote(:or)(listOfFilters) when is_list(listOfFilters) do
    {:or, listOfFilters}
  end

  def unquote(:not)(filter) when is_tuple(filter) do
    {:not, filter}
  end

  def equalityMatch(desc, value) do
    {:equalityMatch, av_assert(desc, value)}
  end

  def greaterOrEqual(desc, value) do
    {:greaterOrEqual, av_assert(desc, value)}
  end

  def lessOrEqual(desc, value) do
    {:lessOrEqual, av_assert(desc, value)}
  end

  def approxMatch(desc, value) do
    {:approxMatch, av_assert(desc, value)}
  end

  defp av_assert(desc, value) do
    r_AttributeValueAssertion(attributeDesc: desc, assertionValue: value)
  end

  def present(attribute) when is_list(attribute) do
    {:present, attribute}
  end

  def substrings(type, subStr)
      when is_list(type) and
             is_list(subStr) do
    ss = v_substr(subStr)
    {:substrings, r_SubstringFilter(type: type, substrings: ss)}
  end

  def extensibleMatch(matchValue, optArgs) do
    matchingRuleAssertion =
      mra(
        optArgs,
        r_MatchingRuleAssertion(matchValue: matchValue)
      )

    {:extensibleMatch, matchingRuleAssertion}
  end

  defp mra([{:matchingRule, val} | t], ack)
       when is_list(val) do
    mra(t, r_MatchingRuleAssertion(ack, matchingRule: val))
  end

  defp mra([{:type, val} | t], ack) when is_list(val) do
    mra(t, r_MatchingRuleAssertion(ack, type: val))
  end

  defp mra([{:dnAttributes, true} | t], ack) do
    mra(t, r_MatchingRuleAssertion(ack, dnAttributes: 'TRUE'))
  end

  defp mra([{:dnAttributes, false} | t], ack) do
    mra(t, r_MatchingRuleAssertion(ack, dnAttributes: 'FALSE'))
  end

  defp mra([h | _], _) do
    throw({:error, {:extensibleMatch_arg, h}})
  end

  defp mra([], ack) do
    ack
  end

  defp init(hosts, opts, cpid) do
    data = parse_args(opts, cpid, r_eldap())

    case try_connect(hosts, data) do
      {:ok, data2} ->
        send(cpid, {:ok, self()})
        :eldap.loop(cpid, data2)

      else__ ->
        send(cpid, else__)
        :erlang.unlink(cpid)
        exit(else__)
    end
  end

  defp parse_args([{:port, port} | t], cpid, data)
       when is_integer(port) do
    parse_args(t, cpid, r_eldap(data, port: port))
  end

  defp parse_args([{:timeout, timeout} | t], cpid, data)
       when is_integer(timeout) and timeout > 0 do
    parse_args(t, cpid, r_eldap(data, timeout: timeout))
  end

  defp parse_args([{:anon_auth, true} | t], cpid, data) do
    parse_args(t, cpid, r_eldap(data, anon_auth: true))
  end

  defp parse_args([{:anon_auth, _} | t], cpid, data) do
    parse_args(t, cpid, data)
  end

  defp parse_args([{:ssl, true} | t], cpid, data) do
    parse_args(t, cpid, r_eldap(data, ldaps: true, using_tls: true))
  end

  defp parse_args([{:ssl, _} | t], cpid, data) do
    parse_args(t, cpid, data)
  end

  defp parse_args([{:sslopts, opts} | t], cpid, data)
       when is_list(opts) do
    parse_args(
      t,
      cpid,
      r_eldap(data, ldaps: true, using_tls: true, tls_opts: opts ++ r_eldap(data, :tls_opts))
    )
  end

  defp parse_args([{:sslopts, _} | t], cpid, data) do
    parse_args(t, cpid, data)
  end

  defp parse_args([{:tcpopts, opts} | t], cpid, data)
       when is_list(opts) do
    parse_args(t, cpid, r_eldap(data, tcp_opts: tcp_opts(opts, cpid, r_eldap(data, :tcp_opts))))
  end

  defp parse_args([{:log, f} | t], cpid, data)
       when is_function(f) do
    parse_args(t, cpid, r_eldap(data, log: f))
  end

  defp parse_args([{:log, _} | t], cpid, data) do
    parse_args(t, cpid, data)
  end

  defp parse_args([h | _], cpid, _) do
    send(cpid, {:error, {:wrong_option, h}})
    :erlang.unlink(cpid)
    exit(:wrong_option)
  end

  defp parse_args([], _, data) do
    data
  end

  defp tcp_opts([opt | opts], cpid, acc) do
    key =
      cond do
        is_atom(opt) ->
          opt

        is_tuple(opt) ->
          :erlang.element(1, opt)
      end

    case :lists.member(
           key,
           [:active, :binary, :deliver, :list, :mode, :packet]
         ) do
      false ->
        tcp_opts(opts, cpid, [opt | acc])

      true ->
        tcp_opts_error(opt, cpid)
    end
  end

  defp tcp_opts([], _Cpid, acc) do
    acc
  end

  defp tcp_opts_error(opt, cpid) do
    send(
      cpid,
      {:error,
       {{:forbidden_tcp_option, opt},
        'This option affects the eldap functionality and can\'t be set by user'}}
    )

    :erlang.unlink(cpid)
    exit(:forbidden_tcp_option)
  end

  defp try_connect([host | hosts], data) do
    tcpOpts = [{:packet, :asn1}, {:active, false}]

    try do
      do_connect(host, data, tcpOpts)
    catch
      _, err ->
        log2(data, 'Connect: ~p failed ~p~n', [host, err])
        try_connect(hosts, data)
    else
      {:ok, fd} ->
        {:ok, r_eldap(data, host: host, fd: fd)}

      err ->
        log2(data, 'Connect: ~p failed ~p~n', [host, err])
        try_connect(hosts, data)
    end
  end

  defp try_connect([], _) do
    {:error, 'connect failed'}
  end

  defp do_connect(host, data, opts)
       when r_eldap(data, :ldaps) == false do
    :gen_tcp.connect(
      host,
      r_eldap(data, :port),
      opts ++ r_eldap(data, :tcp_opts),
      r_eldap(data, :timeout)
    )
  end

  defp do_connect(host, data, opts)
       when r_eldap(data, :ldaps) == true do
    :ssl.connect(
      host,
      r_eldap(data, :port),
      opts ++ r_eldap(data, :tls_opts) ++ r_eldap(data, :tcp_opts),
      r_eldap(data, :timeout)
    )
  end

  def loop(cpid, data) do
    receive do
      {from, {:search, a, controls}} ->
        {res, newData} = do_search(data, a, controls)
        send(from, res)
        :eldap.loop(cpid, newData)

      {from, {:modify, obj, mod, controls}} ->
        {res, newData} = do_modify(data, obj, mod, controls)
        send(from, res)
        :eldap.loop(cpid, newData)

      {from, {:modify_dn, obj, newRDN, delOldRDN, newSup, controls}} ->
        {res, newData} = do_modify_dn(data, obj, newRDN, delOldRDN, newSup, controls)
        send(from, res)
        :eldap.loop(cpid, newData)

      {from, {:add, entry, attrs, controls}} ->
        {res, newData} = do_add(data, entry, attrs, controls)
        send(from, res)
        :eldap.loop(cpid, newData)

      {from, {:delete, entry, controls}} ->
        {res, newData} = do_delete(data, entry, controls)
        send(from, res)
        :eldap.loop(cpid, newData)

      {from, {:simple_bind, dn, passwd, controls}} ->
        {res, newData} = do_simple_bind(data, dn, passwd, controls)
        send(from, res)
        :eldap.loop(cpid, newData)

      {from, {:cnt_proc, newCpid}} ->
        :erlang.unlink(cpid)
        send(from, :ok)
        true
        :eldap.loop(newCpid, data)

      {from, {:start_tls, tlsOptions, timeout, controls}} ->
        {res, newData} = do_start_tls(data, tlsOptions, timeout, controls)
        send(from, res)
        :eldap.loop(cpid, newData)

      {from, {:passwd_modify, dn, newPasswd, oldPasswd, controls}} ->
        {res, newData} = do_passwd_modify(data, dn, newPasswd, oldPasswd, controls)
        send(from, res)
        :eldap.loop(cpid, newData)

      {_From, :close} ->
        try do
          do_unbind(data)
        catch
          {:gen_tcp_error, _TcpErr} ->
            :ok
        else
          {:no_reply, _NewData} ->
            :ok
        end

        :erlang.unlink(cpid)
        exit(:closed)

      {from, {:getopts, optNames}} ->
        result =
          try do
            for optName <- optNames do
              case optName do
                :port ->
                  {:port, r_eldap(data, :port)}

                :log ->
                  {:log, r_eldap(data, :log)}

                :timeout ->
                  {:timeout, r_eldap(data, :timeout)}

                :ssl ->
                  {:ssl, r_eldap(data, :ldaps)}

                {:sslopts, sslOptNames} when r_eldap(data, :using_tls) == true ->
                  case :ssl.getopts(r_eldap(data, :fd), sslOptNames) do
                    {:ok, sslOptVals} ->
                      {:sslopts, sslOptVals}

                    {:error, reason} ->
                      throw({:error, reason})
                  end

                {:sslopts, _} ->
                  throw({:error, :no_tls})

                {:tcpopts, tcpOptNames} ->
                  case :inet.getopts(r_eldap(data, :fd), tcpOptNames) do
                    {:ok, tcpOptVals} ->
                      {:tcpopts, tcpOptVals}

                    {:error, posix} ->
                      throw({:error, posix})
                  end
              end
            end
          catch
            error ->
              error

            class, error ->
              {:error, {class, error}}
          else
            optsList ->
              {:ok, optsList}
          end

        send(from, result)
        :eldap.loop(cpid, data)

      {^cpid, :EXIT, reason} ->
        true
        exit(reason)

      _XX ->
        true
        :eldap.loop(cpid, data)
    end
  end

  defp do_start_tls(data = r_eldap(using_tls: true), _, _, _) do
    {{:error, :tls_already_started}, data}
  end

  defp do_start_tls(data = r_eldap(fd: fD), tlsOptions, timeout, controls) do
    case (try do
            exec_start_tls(data, controls)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, newData} ->
        case :ssl.connect(fD, tlsOptions, timeout) do
          {:ok, sslSocket} ->
            {:ok, r_eldap(newData, prev_fd: fD, fd: sslSocket, using_tls: true)}

          {:error, error} ->
            {{:error, error}, data}
        end

      {{:ok, val}, newData} ->
        {{:ok, val}, newData}

      {:error, error} ->
        {{:error, error}, data}

      else__ ->
        {{:error, else__}, data}
    end
  end

  defp exec_start_tls(data, controls) do
    req = r_ExtendedRequest(requestName: '1.3.6.1.4.1.1466.20037')
    reply = request(r_eldap(data, :fd), data, r_eldap(data, :id), {:extendedReq, req, controls})
    exec_extended_req_reply(data, reply)
  end

  defp exec_extended_req_reply(data, {:ok, msg})
       when r_LDAPMessage(msg, :messageID) == r_eldap(data, :id) do
    case r_LDAPMessage(msg, :protocolOp) do
      {:extendedResp, result} ->
        case r_ExtendedResponse(result, :resultCode) do
          :success ->
            {:ok, data}

          :referral ->
            {{:ok, {:referral, r_ExtendedResponse(result, :referral)}}, data}

          error ->
            {:error, {:response, error}}
        end

      other ->
        {:error, other}
    end
  end

  defp exec_extended_req_reply(_, error) do
    {:error, error}
  end

  defp do_simple_bind(data, :anon, :anon, controls) do
    do_the_simple_bind(data, '', '', controls)
  end

  defp do_simple_bind(data, dn, _Passwd, _)
       when dn == '' and
              r_eldap(data, :anon_auth) == false do
    {{:error, :anonymous_auth}, data}
  end

  defp do_simple_bind(data, _Dn, passwd, _)
       when passwd == '' and
              r_eldap(data, :anon_auth) == false do
    {{:error, :anonymous_auth}, data}
  end

  defp do_simple_bind(data, dn, passwd, controls) do
    do_the_simple_bind(data, dn, passwd, controls)
  end

  defp do_the_simple_bind(data, dn, passwd, controls) do
    case (try do
            exec_simple_bind(
              r_eldap(data, binddn: dn, passwd: passwd, id: bump_id(data)),
              controls
            )
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, newData} ->
        {:ok, newData}

      {{:ok, val}, newData} ->
        {{:ok, val}, newData}

      {:error, emsg} ->
        {{:error, emsg}, data}

      else__ ->
        {{:error, else__}, data}
    end
  end

  defp exec_simple_bind(data, controls) do
    req =
      r_BindRequest(
        version: r_eldap(data, :version),
        name: r_eldap(data, :binddn),
        authentication: {:simple, r_eldap(data, :passwd)}
      )

    log2(data, 'bind request = ~p~n', [req])
    reply = request(r_eldap(data, :fd), data, r_eldap(data, :id), {:bindRequest, req, controls})
    log2(data, 'bind reply = ~p~n', [reply])
    exec_simple_bind_reply(data, reply)
  end

  defp exec_simple_bind_reply(data, {:ok, msg})
       when r_LDAPMessage(msg, :messageID) == r_eldap(data, :id) do
    case r_LDAPMessage(msg, :protocolOp) do
      {:bindResponse, result} ->
        case r_BindResponse(result, :resultCode) do
          :success ->
            {:ok, data}

          :referral ->
            {{:ok, {:referral, r_BindResponse(result, :referral)}}, data}

          error ->
            {:error, error}
        end

      other ->
        {:error, other}
    end
  end

  defp exec_simple_bind_reply(_, error) do
    {:error, error}
  end

  defp do_search(data, a, controls) do
    case (try do
            do_search_0(data, a, controls)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, emsg} ->
        {ldap_closed_p(data, emsg), data}

      {:EXIT, error} ->
        {ldap_closed_p(data, error), data}

      {{:ok, val}, newData} ->
        {{:ok, val}, newData}

      {:ok, res, ref, newData} ->
        {{:ok, polish(res, ref)}, newData}

      {{:error, reason}, newData} ->
        {{:error, reason}, newData}

      else__ ->
        {ldap_closed_p(data, else__), data}
    end
  end

  defp polish(res, ref) do
    r = polish_result(res)
    r_eldap_search_result(entries: r, referrals: ref)
  end

  defp polish_result([h | t])
       when elem(h, 0) === :SearchResultEntry do
    objectName = r_SearchResultEntry(h, :objectName)

    f = fn {_, a, v} ->
      {a, v}
    end

    attrs = :lists.map(f, r_SearchResultEntry(h, :attributes))

    [
      r_eldap_entry(object_name: objectName, attributes: attrs)
      | polish_result(t)
    ]
  end

  defp polish_result([]) do
    []
  end

  defp do_search_0(data, a, controls) do
    req =
      r_SearchRequest(
        baseObject: r_eldap_search(a, :base),
        scope: v_scope(r_eldap_search(a, :scope)),
        derefAliases: v_deref(r_eldap_search(a, :deref)),
        sizeLimit: 0,
        timeLimit: v_timeout(r_eldap_search(a, :timeout)),
        typesOnly: v_bool(r_eldap_search(a, :types_only)),
        filter: v_filter(r_eldap_search(a, :filter)),
        attributes: v_attributes(r_eldap_search(a, :attributes))
      )

    id = bump_id(data)
    collect_search_responses(r_eldap(data, id: id), req, id, controls)
  end

  defp collect_search_responses(data, req, iD, controls) do
    s = r_eldap(data, :fd)
    log2(data, 'search request = ~p~n', [req])
    send_request(s, data, iD, {:searchRequest, req, controls})
    resp = recv_response(s, data)
    log2(data, 'search reply = ~p~n', [resp])
    collect_search_responses(data, s, iD, resp, [], [])
  end

  defp collect_search_responses(data, s, iD, {:ok, msg}, acc, ref)
       when elem(msg, 0) === :LDAPMessage do
    case r_LDAPMessage(msg, :protocolOp) do
      {:searchResDone, r} ->
        case r_LDAPResult(r, :resultCode) do
          :success ->
            log2(data, 'search reply = searchResDone ~n', [])
            {:ok, acc, ref, data}

          :referral ->
            {{:ok, {:referral, r_LDAPResult(r, :referral)}}, data}

          reason ->
            {{:error, reason}, data}
        end

      {:searchResEntry, r}
      when elem(r, 0) === :SearchResultEntry ->
        resp = recv_response(s, data)
        log2(data, 'search reply = ~p~n', [resp])
        collect_search_responses(data, s, iD, resp, [r | acc], ref)

      {:searchResRef, r} ->
        resp = recv_response(s, data)
        log2(data, 'search reply = ~p~n', [resp])
        collect_search_responses(data, s, iD, resp, acc, [r | ref])

      else__ ->
        throw({:error, else__})
    end
  end

  defp collect_search_responses(_, _, _, else__, _, _) do
    throw({:error, else__})
  end

  defp do_add(data, entry, attrs, controls) do
    case (try do
            do_add_0(data, entry, attrs, controls)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, emsg} ->
        {ldap_closed_p(data, emsg), data}

      {:EXIT, error} ->
        {ldap_closed_p(data, error), data}

      {:ok, newData} ->
        {:ok, newData}

      {{:ok, val}, newData} ->
        {{:ok, val}, newData}

      else__ ->
        {ldap_closed_p(data, else__), data}
    end
  end

  defp do_add_0(data, entry, attrs, controls) do
    req = r_AddRequest(entry: entry, attributes: attrs)
    s = r_eldap(data, :fd)
    id = bump_id(data)
    log2(data, 'add request = ~p~n', [req])
    resp = request(s, data, id, {:addRequest, req, controls})
    log2(data, 'add reply = ~p~n', [resp])
    check_reply(r_eldap(data, id: id), resp, :addResponse)
  end

  defp do_delete(data, entry, controls) do
    case (try do
            do_delete_0(data, entry, controls)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, emsg} ->
        {ldap_closed_p(data, emsg), data}

      {:EXIT, error} ->
        {ldap_closed_p(data, error), data}

      {:ok, newData} ->
        {:ok, newData}

      {{:ok, val}, newData} ->
        {{:ok, val}, newData}

      else__ ->
        {ldap_closed_p(data, else__), data}
    end
  end

  defp do_delete_0(data, entry, controls) do
    s = r_eldap(data, :fd)
    id = bump_id(data)
    log2(data, 'del request = ~p~n', [entry])
    resp = request(s, data, id, {:delRequest, entry, controls})
    log2(data, 'del reply = ~p~n', [resp])
    check_reply(r_eldap(data, id: id), resp, :delResponse)
  end

  defp do_modify(data, obj, mod, controls) do
    case (try do
            do_modify_0(data, obj, mod, controls)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, emsg} ->
        {ldap_closed_p(data, emsg), data}

      {:EXIT, error} ->
        {ldap_closed_p(data, error), data}

      {:ok, newData} ->
        {:ok, newData}

      {{:ok, val}, newData} ->
        {{:ok, val}, newData}

      else__ ->
        {ldap_closed_p(data, else__), data}
    end
  end

  defp do_modify_0(data, obj, mod, controls) do
    v_modifications(mod)
    req = r_ModifyRequest(object: obj, changes: mod)
    s = r_eldap(data, :fd)
    id = bump_id(data)
    log2(data, 'modify request = ~p~n', [req])
    resp = request(s, data, id, {:modifyRequest, req, controls})
    log2(data, 'modify reply = ~p~n', [resp])
    check_reply(r_eldap(data, id: id), resp, :modifyResponse)
  end

  defp do_passwd_modify(data, dn, newPasswd, oldPasswd, controls) do
    case (try do
            do_passwd_modify_0(data, dn, newPasswd, oldPasswd, controls)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, emsg} ->
        {ldap_closed_p(data, emsg), data}

      {:EXIT, error} ->
        {ldap_closed_p(data, error), data}

      {:ok, newData} ->
        {:ok, newData}

      {{:ok, val}, newData} ->
        {{:ok, val}, newData}

      {:ok, passwd, newData} ->
        {{:ok, passwd}, newData}

      else__ ->
        {ldap_closed_p(data, else__), data}
    end
  end

  defp do_passwd_modify_0(data, dn, newPasswd, oldPasswd, controls) do
    req = r_PasswdModifyRequestValue(userIdentity: dn, oldPasswd: oldPasswd, newPasswd: newPasswd)
    log2(data, 'modify password request = ~p~n', [req])
    {:ok, bytes} = :ELDAPv3.encode(:PasswdModifyRequestValue, req)
    extReq = r_ExtendedRequest(requestName: '1.3.6.1.4.1.4203.1.11.1', requestValue: bytes)
    id = bump_id(data)
    log2(data, 'extended request = ~p~n', [extReq])
    reply = request(r_eldap(data, :fd), data, id, {:extendedReq, extReq, controls})
    log2(data, 'modify password reply = ~p~n', [reply])
    exec_passwd_modify_reply(r_eldap(data, id: id), reply)
  end

  defp exec_passwd_modify_reply(data, {:ok, msg})
       when r_LDAPMessage(msg, :messageID) == r_eldap(data, :id) do
    case r_LDAPMessage(msg, :protocolOp) do
      {:extendedResp, result} ->
        case r_ExtendedResponse(result, :resultCode) do
          :success ->
            case r_ExtendedResponse(result, :responseValue) do
              :asn1_NOVALUE ->
                {:ok, data}

              value ->
                case :ELDAPv3.decode(
                       :PasswdModifyResponseValue,
                       value
                     ) do
                  {:ok, r_PasswdModifyResponseValue(genPasswd: passwd)} ->
                    {:ok, passwd, data}

                  error ->
                    throw(error)
                end
            end

          :referral ->
            {{:ok, {:referral, r_ExtendedResponse(result, :referral)}}, data}

          error ->
            {:error, {:response, error}}
        end

      other ->
        {:error, other}
    end
  end

  defp exec_passwd_modify_reply(_, error) do
    {:error, error}
  end

  defp do_modify_dn(data, entry, newRDN, delOldRDN, newSup, controls) do
    case (try do
            do_modify_dn_0(data, entry, newRDN, delOldRDN, newSup, controls)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, emsg} ->
        {ldap_closed_p(data, emsg), data}

      {:EXIT, error} ->
        {ldap_closed_p(data, error), data}

      {:ok, newData} ->
        {:ok, newData}

      {{:ok, val}, newData} ->
        {{:ok, val}, newData}

      else__ ->
        {ldap_closed_p(data, else__), data}
    end
  end

  defp do_modify_dn_0(data, entry, newRDN, delOldRDN, newSup, controls) do
    req =
      r_ModifyDNRequest(
        entry: entry,
        newrdn: newRDN,
        deleteoldrdn: delOldRDN,
        newSuperior: newSup
      )

    s = r_eldap(data, :fd)
    id = bump_id(data)
    log2(data, 'modify DN request = ~p~n', [req])
    resp = request(s, data, id, {:modDNRequest, req, controls})
    log2(data, 'modify DN reply = ~p~n', [resp])
    check_reply(r_eldap(data, id: id), resp, :modDNResponse)
  end

  defp do_unbind(data) do
    req = ''
    log2(data, 'unbind request = ~p (has no reply)~n', [req])

    _ =
      case r_eldap(data, :using_tls) do
        true ->
          send_request(r_eldap(data, :fd), data, r_eldap(data, :id), {:unbindRequest, req})
          :ssl.close(r_eldap(data, :fd))

        false ->
          oldTrapExit = :erlang.process_flag(:trap_exit, true)

          try do
            send_request(r_eldap(data, :fd), data, r_eldap(data, :id), {:unbindRequest, req})
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

          try do
            :gen_tcp.close(r_eldap(data, :fd))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end

          receive do
            {:EXIT, _From, _Reason} ->
              :ok
          after
            0 ->
              :ok
          end

          :erlang.process_flag(:trap_exit, oldTrapExit)
      end

    {:no_reply,
     r_eldap(data,
       binddn: r_eldap(r_eldap(), :binddn),
       passwd: r_eldap(r_eldap(), :passwd),
       fd: r_eldap(r_eldap(), :fd),
       using_tls: false
     )}
  end

  defp request(s, data, iD, request) do
    send_request(s, data, iD, request)
    recv_response(s, data)
  end

  defp send_request(s, data, id, {t, p}) do
    send_the_LDAPMessage(s, data, r_LDAPMessage(messageID: id, protocolOp: {t, p}))
  end

  defp send_request(s, data, id, {t, p, :asn1_NOVALUE}) do
    send_the_LDAPMessage(s, data, r_LDAPMessage(messageID: id, protocolOp: {t, p}))
  end

  defp send_request(s, data, id, {t, p, controls0}) do
    controls =
      for {:control, f1, f2, f3} <- controls0 do
        r_Control(controlType: f1, criticality: f2, controlValue: f3)
      end

    send_the_LDAPMessage(
      s,
      data,
      r_LDAPMessage(messageID: id, protocolOp: {t, p}, controls: controls)
    )
  end

  defp send_the_LDAPMessage(s, data, lDAPMessage) do
    {:ok, bytes} =
      :ELDAPv3.encode(
        :LDAPMessage,
        lDAPMessage
      )

    case do_send(s, data, bytes) do
      {:error, reason} ->
        throw({:gen_tcp_error, reason})

      else__ ->
        else__
    end
  end

  defp do_send(s, data, bytes)
       when r_eldap(data, :using_tls) == false do
    :gen_tcp.send(s, bytes)
  end

  defp do_send(s, data, bytes)
       when r_eldap(data, :using_tls) == true do
    :ssl.send(s, bytes)
  end

  defp do_recv(s, r_eldap(using_tls: false, timeout: timeout), len) do
    :gen_tcp.recv(s, len, timeout)
  end

  defp do_recv(s, r_eldap(using_tls: true, timeout: timeout), len) do
    :ssl.recv(s, len, timeout)
  end

  defp recv_response(s, data) do
    case do_recv(s, data, 0) do
      {:ok, packet} ->
        case :ELDAPv3.decode(:LDAPMessage, packet) do
          {:ok, resp} ->
            {:ok, resp}

          error ->
            throw(error)
        end

      {:error, reason} ->
        throw({:gen_tcp_error, reason})
    end
  end

  defp check_reply(data, {:ok, msg}, op)
       when r_LDAPMessage(msg, :messageID) == r_eldap(data, :id) do
    case r_LDAPMessage(msg, :protocolOp) do
      {^op, result} ->
        case r_LDAPResult(result, :resultCode) do
          :success ->
            {:ok, data}

          :referral ->
            {{:ok, {:referral, r_LDAPResult(result, :referral)}}, data}

          error ->
            {:error, error}
        end

      other ->
        {:error, other}
    end
  end

  defp check_reply(_, error, _) do
    {:error, error}
  end

  defp v_filter({:and, l}) do
    {:and, l}
  end

  defp v_filter({:or, l}) do
    {:or, l}
  end

  defp v_filter({:not, l}) do
    {:not, l}
  end

  defp v_filter({:equalityMatch, aV}) do
    {:equalityMatch, aV}
  end

  defp v_filter({:greaterOrEqual, aV}) do
    {:greaterOrEqual, aV}
  end

  defp v_filter({:lessOrEqual, aV}) do
    {:lessOrEqual, aV}
  end

  defp v_filter({:approxMatch, aV}) do
    {:approxMatch, aV}
  end

  defp v_filter({:present, a}) do
    {:present, a}
  end

  defp v_filter({:substrings, s})
       when elem(s, 0) === :SubstringFilter do
    {:substrings, s}
  end

  defp v_filter({:extensibleMatch, s})
       when elem(s, 0) === :MatchingRuleAssertion do
    {:extensibleMatch, s}
  end

  defp v_filter(_Filter) do
    throw({:error, concat(['unknown filter: ', _Filter])})
  end

  defp v_modifications(mods) do
    f = fn {_, op, _} ->
      case :lists.member(op, [:add, :delete, :replace]) do
        true ->
          true

        _ ->
          throw({:error, {:mod_operation, op}})
      end
    end

    :lists.foreach(f, mods)
  end

  defp v_substr([{key, str} | t])
       when (is_list(str) and
               key == :initial) or
              key == :any or key == :final do
    [{key, str} | v_substr(t)]
  end

  defp v_substr([h | _]) do
    throw({:error, {:substring_arg, h}})
  end

  defp v_substr([]) do
    []
  end

  defp v_scope(:baseObject) do
    :baseObject
  end

  defp v_scope(:singleLevel) do
    :singleLevel
  end

  defp v_scope(:wholeSubtree) do
    :wholeSubtree
  end

  defp v_scope(_Scope) do
    throw({:error, concat(['unknown scope: ', _Scope])})
  end

  defp v_deref(dR = :neverDerefAliases) do
    dR
  end

  defp v_deref(dR = :derefInSearching) do
    dR
  end

  defp v_deref(dR = :derefFindingBaseObj) do
    dR
  end

  defp v_deref(dR = :derefAlways) do
    dR
  end

  defp v_bool(true) do
    true
  end

  defp v_bool(false) do
    false
  end

  defp v_bool(_Bool) do
    throw({:error, concat(['not Boolean: ', _Bool])})
  end

  defp v_timeout(i) when is_integer(i) and i >= 0 do
    i
  end

  defp v_timeout(_I) do
    throw({:error, concat(['timeout not positive integer: ', _I])})
  end

  defp v_attributes(attrs) do
    f = fn
      a when is_list(a) ->
        a

      a ->
        throw({:error, concat(['attribute not String: ', a])})
    end

    :lists.map(f, attrs)
  end

  defp log2(data, str, args) do
    log(data, str, args, 2)
  end

  defp log(data, str, args, level)
       when is_function(r_eldap(data, :log)) do
    try do
      r_eldap(data, :log).(level, str, args)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp log(_, _, _, _) do
    :ok
  end

  defp send(to, msg) do
    send(to, {self(), msg})
    :ok
  end

  defp recv(from) do
    receive do
      {^from, msg} ->
        msg

      {:EXIT, ^from, reason} ->
        {:error, {:internal_error, reason}}
    end
  end

  defp ldap_closed_p(data, emsg) when r_eldap(data, :using_tls) == true do
    case (try do
            :ssl.sockname(r_eldap(data, :fd))
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:error, _} ->
        _ = :ssl.close(r_eldap(data, :fd))
        {:error, :ldap_closed}

      {:ok, _} ->
        {:error, emsg}

      _ ->
        {:error, :ldap_closed}
    end
  end

  defp ldap_closed_p(data, emsg) do
    case :inet.port(r_eldap(data, :fd)) do
      {:error, _} ->
        {:error, :ldap_closed}

      _ ->
        {:error, emsg}
    end
  end

  defp bump_id(data) do
    r_eldap(data, :id) + 1
  end

  def parse_dn('') do
    {:ok, []}
  end

  def parse_dn([h | _] = str) when h !== ?, do
    case (try do
            parse_name(str, [])
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:EXIT, reason} ->
        {:parse_error, :internal_error, reason}

      else__ ->
        else__
    end
  end

  defp parse_name('', acc) do
    {:ok, :lists.reverse(acc)}
  end

  defp parse_name([?, | t], acc) do
    parse_name(t, acc)
  end

  defp parse_name(str, acc) do
    {rest, nameComponent} = parse_name_component(str)
    parse_name(rest, [nameComponent | acc])
  end

  defp parse_name_component(str) do
    parse_name_component(str, [])
  end

  defp parse_name_component(str, acc) do
    case parse_attribute_type_and_value(str) do
      {[?+ | rest], aTV} ->
        parse_name_component(rest, [aTV | acc])

      {rest, aTV} ->
        {rest, :lists.reverse([aTV | acc])}
    end
  end

  defp parse_attribute_type_and_value(str) do
    case parse_attribute_type(str) do
      {_Rest, []} ->
        parse_error(:expecting_attribute_type, str)

      {rest, type} ->
        rest2 = parse_equal_sign(rest)
        {rest3, value} = parse_attribute_value(rest2)
        {rest3, {:attribute_type_and_value, type, value}}
    end
  end

  defp parse_attribute_type([h | t])
       when (h >= ?a and h <= ?z) or
              (h >= ?A and h <= ?Z) do
    {rest, keyChars} = parse_keychars(t)
    {rest, [h | keyChars]}
  end

  defp parse_attribute_type([h | _] = str) when h >= ?0 and h <= ?9 do
    parse_oid(str)
  end

  defp parse_attribute_type(str) do
    parse_error(:invalid_attribute_type, str)
  end

  defp parse_attribute_value([[?#, x, y] | t])
       when (x >= ?0 and x <= ?9) or
              (x >= ?a and x <= ?f) or
              (x >= ?A and x <= ?F and y >= ?0 and
                 y <= ?9) or
              (y >= ?a and y <= ?f) or
              (y >= ?A and y <= ?F) do
    {rest, hexString} = parse_hexstring(t)
    {rest, [[?#, x, y] | hexString]}
  end

  defp parse_attribute_value([?" | t]) do
    {rest, quotation} = parse_quotation(t)
    {rest, [?" | quotation]}
  end

  defp parse_attribute_value(str) do
    parse_string(str)
  end

  defp parse_hexstring(str) do
    parse_hexstring(str, [])
  end

  defp parse_hexstring([[x, y] | t], acc)
       when (x >= ?0 and x <= ?9) or
              (x >= ?a and x <= ?f) or
              (x >= ?A and x <= ?F and y >= ?0 and
                 y <= ?9) or
              (y >= ?a and y <= ?f) or
              (y >= ?A and y <= ?F) do
    parse_hexstring(t, [[y, x] | acc])
  end

  defp parse_hexstring(t, acc) do
    {t, :lists.reverse(acc)}
  end

  defp parse_quotation([?" | t]) do
    {t, [?"]}
  end

  defp parse_quotation(str) do
    parse_quotation(str, [])
  end

  defp parse_quotation([?" | t], acc) do
    {t, :lists.reverse([?" | acc])}
  end

  defp parse_quotation([x | t], acc) when x !== ?\\ and x !== ?" do
    parse_quotation(t, [x | acc])
  end

  defp parse_quotation([[?\\, x] | t], acc)
       when x == ?, or x == ?= or
              x == ?+ or x == ?< or x == ?> or
              x == ?# or x == ?; do
    parse_quotation(t, [[x, ?\\] | acc])
  end

  defp parse_quotation([[?\\, ?\\] | t], acc) do
    parse_quotation(t, [[?\\, ?\\] | acc])
  end

  defp parse_quotation([[?\\, ?"] | t], acc) do
    parse_quotation(t, [[?", ?\\] | acc])
  end

  defp parse_quotation([[?\\, x, y] | t], acc)
       when (x >= ?0 and
               x <= ?9) or
              (x >= ?a and x <= ?f) or
              (x >= ?A and x <= ?F and y >= ?0 and
                 y <= ?9) or
              (y >= ?a and y <= ?f) or
              (y >= ?A and y <= ?F) do
    parse_quotation(t, [[y, x, ?\\] | acc])
  end

  defp parse_quotation(t, _) do
    parse_error(:expecting_double_quote_mark, t)
  end

  defp parse_string(str) do
    parse_string(str, [])
  end

  defp parse_string('', acc) do
    {'', :lists.reverse(acc)}
  end

  defp parse_string([h | t], acc)
       when h !== ?, and h !== ?= and
              h !== ?+ and h !== ?< and h !== ?> and
              h !== ?# and h !== ?; and h !== ?\\ and
              h !== ?" do
    parse_string(t, [h | acc])
  end

  defp parse_string([[?\\, x] | t], acc)
       when x == ?, or x == ?= or
              x == ?+ or x == ?< or x == ?> or
              x == ?# or x == ?; do
    parse_string(t, [[x, ?\\] | acc])
  end

  defp parse_string([[?\\, ?\\] | t], acc) do
    parse_string(t, [[?\\, ?\\] | acc])
  end

  defp parse_string([[?\\, ?"] | t], acc) do
    parse_string(t, [[?", ?\\] | acc])
  end

  defp parse_string([[?\\, x, y] | t], acc)
       when (x >= ?0 and
               x <= ?9) or
              (x >= ?a and x <= ?f) or
              (x >= ?A and x <= ?F and y >= ?0 and
                 y <= ?9) or
              (y >= ?a and y <= ?f) or
              (y >= ?A and y <= ?F) do
    parse_string(t, [[y, x, ?\\] | acc])
  end

  defp parse_string(t, acc) do
    {t, :lists.reverse(acc)}
  end

  defp parse_equal_sign([?= | t]) do
    t
  end

  defp parse_equal_sign(t) do
    parse_error(:expecting_equal_sign, t)
  end

  defp parse_keychars(str) do
    parse_keychars(str, [])
  end

  defp parse_keychars([h | t], acc)
       when (h >= ?a and h <= ?z) or
              (h >= ?A and h <= ?Z) do
    parse_keychars(t, [h | acc])
  end

  defp parse_keychars([h | t], acc) when h >= ?0 and h <= ?9 do
    parse_keychars(t, [h | acc])
  end

  defp parse_keychars([?- | t], acc) do
    parse_keychars(t, [?- | acc])
  end

  defp parse_keychars(t, acc) do
    {t, :lists.reverse(acc)}
  end

  defp parse_oid(str) do
    parse_oid(str, [])
  end

  defp parse_oid([[h, ?.] | t], acc)
       when h >= ?0 and
              h <= ?9 do
    parse_oid(t, [[?., h] | acc])
  end

  defp parse_oid([h | t], acc) when h >= ?0 and h <= ?9 do
    parse_oid(t, [h | acc])
  end

  defp parse_oid(t, acc) do
    {t, :lists.reverse(acc)}
  end

  defp parse_error(emsg, rest) do
    throw({:parse_error, emsg, rest})
  end

  def parse_ldap_url('ldap://' ++ rest1 = str) do
    {rest2, hostPort} = parse_hostport(rest1)
    {sdn, rest3} = split_string(rm_leading_slash(rest2), ??)

    case parse_dn(sdn) do
      {:parse_error, :internal_error, _Reason} ->
        {:parse_error, :internal_error, {str, []}}

      {:parse_error, emsg, tail} ->
        head = get_head(str, tail)
        {:parse_error, emsg, {head, tail}}

      {:ok, dN} ->
        {_Rest4, attributes} = parse_attributes(rest3)
        {:ok, hostPort, dN, attributes}
    end
  end

  defp rm_leading_slash([?/ | tail]) do
    tail
  end

  defp rm_leading_slash(tail) do
    tail
  end

  defp parse_attributes([?? | tail]) do
    case split_string(tail, ??) do
      {[], attributes} ->
        {[], {:attributes, :string.lexemes(attributes, ',')}}

      {attributes, rest} ->
        {rest, {:attributes, :string.lexemes(attributes, ',')}}
    end
  end

  defp parse_hostport(str) do
    {hostPort, rest} = split_string(str, ?/)

    case split_string(hostPort, ?:) do
      {shost, []} ->
        {rest, {parse_host(rest, shost), 389}}

      {shost, [?: | sport]} ->
        {rest, {parse_host(rest, shost), parse_port(rest, sport)}}
    end
  end

  defp parse_port(rest, sport) do
    try do
      :erlang.list_to_integer(sport)
    catch
      _, _ ->
        parse_error(:parsing_port, rest)
    end
  end

  defp parse_host(rest, shost) do
    case (try do
            validate_host(shost)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:parse_error, emsg, _} ->
        parse_error(emsg, rest)

      host ->
        host
    end
  end

  defp validate_host(shost) do
    case :inet_parse.address(shost) do
      {:ok, host} ->
        host

      _ ->
        case :inet_parse.domain(shost) do
          true ->
            shost

          _ ->
            parse_error(:parsing_host, shost)
        end
    end
  end

  defp split_string(str, key) do
    pred = fn
      x when x == key ->
        false

      _ ->
        true
    end

    :lists.splitwith(pred, str)
  end

  defp get_head(str, tail) do
    get_head(str, tail, [])
  end

  defp get_head([h | tail], tail, rhead) do
    :lists.reverse([h | rhead])
  end

  defp get_head([h | rest], tail, rhead) do
    get_head(rest, tail, [h | rhead])
  end
end
