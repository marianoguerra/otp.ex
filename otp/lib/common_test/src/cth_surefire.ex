defmodule :m_cth_surefire do
  use Bitwise
  require Record

  Record.defrecord(:r_state, :state,
    filepath: :undefined,
    axis: :undefined,
    properties: :undefined,
    package: :undefined,
    hostname: :undefined,
    curr_suite: :undefined,
    curr_suite_ts: :undefined,
    curr_group: [],
    curr_log_dir: :undefined,
    timer: :undefined,
    tc_log: :undefined,
    url_base: :undefined,
    test_cases: [],
    test_suites: []
  )

  Record.defrecord(:r_testcase, :testcase,
    log: :undefined,
    url: :undefined,
    group: :undefined,
    classname: :undefined,
    name: :undefined,
    time: :undefined,
    result: :undefined,
    timestamp: :undefined
  )

  Record.defrecord(:r_testsuite, :testsuite,
    errors: :undefined,
    failures: :undefined,
    skipped: :undefined,
    hostname: :undefined,
    name: :undefined,
    tests: :undefined,
    time: :undefined,
    timestamp: :undefined,
    id: :undefined,
    package: :undefined,
    properties: :undefined,
    testcases: :undefined,
    log: :undefined,
    url: :undefined
  )

  def id(opts) do
    case :proplists.get_value(:path, opts) do
      :undefined ->
        'junit_report.xml'

      path ->
        :filename.absname(path)
    end
  end

  def init(path, opts) do
    {:ok, host} = :inet.gethostname()

    r_state(
      filepath: path,
      hostname: :proplists.get_value(:hostname, opts, host),
      package: :proplists.get_value(:package, opts),
      axis: :proplists.get_value(:axis, opts, []),
      properties: :proplists.get_value(:properties, opts, []),
      url_base: :proplists.get_value(:url_base, opts),
      timer: :os.timestamp()
    )
  end

  def pre_init_per_suite(suite, skipOrFail, r_state(test_cases: []) = state)
      when is_tuple(skipOrFail) do
    {skipOrFail,
     init_tc(
       r_state(state,
         curr_suite: suite,
         curr_suite_ts: :os.timestamp()
       ),
       skipOrFail
     )}
  end

  def pre_init_per_suite(suite, config, r_state(test_cases: []) = state) do
    tcLog = :proplists.get_value(:tc_logfile, config)
    currLogDir = :filename.dirname(tcLog)

    path =
      case r_state(state, :filepath) do
        'junit_report.xml' ->
          rootDir = get_test_root(tcLog)
          :filename.join(rootDir, 'junit_report.xml')

        p ->
          p
      end

    {config,
     init_tc(
       r_state(state,
         filepath: path,
         curr_suite: suite,
         curr_suite_ts: :os.timestamp(),
         curr_log_dir: currLogDir
       ),
       config
     )}
  end

  def pre_init_per_suite(suite, config, state) do
    pre_init_per_suite(suite, config, close_suite(state))
  end

  def post_init_per_suite(_Suite, config, result, state) do
    {result, end_tc(:init_per_suite, config, result, state)}
  end

  def pre_end_per_suite(_Suite, config, state) do
    {config, init_tc(state, config)}
  end

  def post_end_per_suite(_Suite, config, result, state) do
    {result, end_tc(:end_per_suite, config, result, state)}
  end

  def pre_init_per_group(_Suite, group, config, state) do
    {config,
     init_tc(
       r_state(state,
         curr_group: [
           group
           | r_state(state, :curr_group)
         ]
       ),
       config
     )}
  end

  def post_init_per_group(_Suite, _Group, config, result, state) do
    {result, end_tc(:init_per_group, config, result, state)}
  end

  def pre_end_per_group(_Suite, _Group, config, state) do
    {config, init_tc(state, config)}
  end

  def post_end_per_group(_Suite, _Group, config, result, state) do
    newState = end_tc(:end_per_group, config, result, state)
    {result, r_state(newState, curr_group: tl(r_state(newState, :curr_group)))}
  end

  def pre_init_per_testcase(_Suite, _TC, config, state) do
    {config, init_tc(state, config)}
  end

  def post_end_per_testcase(_Suite, tC, config, result, state) do
    {result, end_tc(tC, config, result, state)}
  end

  def on_tc_fail(_Suite, _TC, _Res, state = r_state(test_cases: [])) do
    state
  end

  def on_tc_fail(_Suite, _TC, res, state) do
    tCs = r_state(state, :test_cases)
    tC = hd(tCs)
    newTC = r_testcase(tC, result: {:fail, :lists.flatten(:io_lib.format('~tp', [res]))})
    r_state(state, test_cases: [newTC | tl(tCs)])
  end

  def on_tc_skip(suite, {configFunc, _GrName}, res, state) do
    on_tc_skip(suite, configFunc, res, state)
  end

  def on_tc_skip(suite, tc, res, state0) do
    tcStr = :erlang.atom_to_list(tc)

    state =
      case r_state(state0, :test_cases) do
        [r_testcase(name: ^tcStr) | tCs] ->
          r_state(state0, test_cases: tCs)

        _ ->
          state0
      end

    do_tc_skip(
      res,
      end_tc(tc, [], res, init_tc(set_suite(suite, state), []))
    )
  end

  defp do_tc_skip(res, state) do
    tCs = r_state(state, :test_cases)
    tC = hd(tCs)
    newTC = r_testcase(tC, result: {:skipped, :lists.flatten(:io_lib.format('~tp', [res]))})
    r_state(state, test_cases: [newTC | tl(tCs)])
  end

  defp init_tc(state, config) when is_list(config) == false do
    r_state(state, timer: :os.timestamp(), tc_log: '')
  end

  defp init_tc(state, config) do
    r_state(state,
      timer: :os.timestamp(),
      tc_log: :proplists.get_value(:tc_logfile, config, [])
    )
  end

  defp end_tc(func, config, res, state) when is_atom(func) do
    end_tc(:erlang.atom_to_list(func), config, res, state)
  end

  defp end_tc(
         name,
         _Config,
         _Res,
         state =
           r_state(
             curr_suite: suite,
             curr_group: groups,
             curr_log_dir: currLogDir,
             timer: tS,
             tc_log: log0,
             url_base: urlBase
           )
       ) do
    log =
      case log0 do
        '' ->
          lowerSuiteName = :string.lowercase(:erlang.atom_to_list(suite))

          :filename.join(
            currLogDir,
            lowerSuiteName ++ '.' ++ name ++ '.html'
          )

        _ ->
          log0
      end

    url = make_url(urlBase, log)
    className = :erlang.atom_to_list(suite)

    pGroup =
      :lists.concat(
        :lists.join(
          '.',
          :lists.reverse(groups)
        )
      )

    timeTakes =
      :io_lib.format(
        '~f',
        [
          :timer.now_diff(
            :os.timestamp(),
            tS
          ) / 1_000_000
        ]
      )

    r_state(state,
      test_cases: [
        r_testcase(
          log: log,
          url: url,
          timestamp: now_to_string(tS),
          classname: className,
          group: pGroup,
          name: name,
          time: timeTakes,
          result: :passed
        )
        | r_state(state, :test_cases)
      ],
      tc_log: ''
    )
  end

  defp set_suite(suite, r_state(curr_suite: :undefined) = state) do
    r_state(state,
      curr_suite: suite,
      curr_suite_ts: :os.timestamp()
    )
  end

  defp set_suite(_, state) do
    state
  end

  defp close_suite(r_state(test_cases: []) = state) do
    state
  end

  defp close_suite(
         r_state(
           test_cases: tCs,
           url_base: urlBase
         ) = state
       ) do
    {total, fail, skip} = count_tcs(tCs, 0, 0, 0)

    timeTaken =
      :timer.now_diff(
        :os.timestamp(),
        r_state(state, :curr_suite_ts)
      ) / 1_000_000

    suiteLog = :filename.join(r_state(state, :curr_log_dir), 'suite.log.html')
    suiteUrl = make_url(urlBase, suiteLog)

    suite =
      r_testsuite(
        name: :erlang.atom_to_list(r_state(state, :curr_suite)),
        package: r_state(state, :package),
        hostname: r_state(state, :hostname),
        time: :io_lib.format('~f', [timeTaken]),
        timestamp: now_to_string(r_state(state, :curr_suite_ts)),
        errors: 0,
        failures: fail,
        skipped: skip,
        tests: total,
        testcases: :lists.reverse(tCs),
        log: suiteLog,
        url: suiteUrl
      )

    r_state(state,
      curr_suite: :undefined,
      test_cases: [],
      test_suites: [suite | r_state(state, :test_suites)]
    )
  end

  def terminate(state = r_state(test_cases: [])) do
    {:ok, d} =
      :file.open(
        r_state(state, :filepath),
        [:write, {:encoding, :utf8}]
      )

    :io.format(d, '<?xml version="1.0" encoding= "UTF-8" ?>', [])
    :io.format(d, '~ts', [to_xml(state)])

    try do
      :file.sync(d)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end

    try do
      :file.close(d)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  def terminate(state) do
    terminate(close_suite(state))
  end

  defp to_xml(
         r_testcase(
           group: group,
           classname: cL,
           log: l,
           url: u,
           name: n,
           time: t,
           timestamp: tS,
           result: r
         )
       ) do
    [
      '<testcase ',
      for _ <- [:EFE_DUMMY_GEN], group != '' do
        ['group="', group, '" ']
      end,
      'name="',
      n,
      '" time="',
      t,
      '" timestamp="',
      tS,
      '" ',
      for _ <- [:EFE_DUMMY_GEN],
          u != :undefined do
        ['url="', u, '" ']
      end,
      'log="',
      l,
      '">',
      case r do
        :passed ->
          []

        {:skipped, reason} ->
          [
            '<skipped type="skip" message="Test ',
            n,
            ' in ',
            cL,
            ' skipped!">',
            sanitize(reason),
            '</skipped>'
          ]

        {:fail, reason} ->
          [
            '<failure message="Test ',
            n,
            ' in ',
            cL,
            ' failed!" type="crash">',
            sanitize(reason),
            '</failure>'
          ]
      end,
      '</testcase>'
    ]
  end

  defp to_xml(
         r_testsuite(
           package: p,
           hostname: h,
           errors: e,
           failures: f,
           skipped: s,
           time: time,
           timestamp: tS,
           tests: t,
           name: n,
           testcases: cases,
           log: log,
           url: url
         )
       ) do
    [
      '<testsuite ',
      for _ <- [:EFE_DUMMY_GEN], p != :undefined do
        ['package="', p, '" ']
      end,
      'hostname="',
      h,
      '" name="',
      n,
      '" time="',
      time,
      '" timestamp="',
      tS,
      '" errors="',
      :erlang.integer_to_list(e),
      '" failures="',
      :erlang.integer_to_list(f),
      '" skipped="',
      :erlang.integer_to_list(s),
      '" tests="',
      :erlang.integer_to_list(t),
      '" ',
      for _ <- [:EFE_DUMMY_GEN],
          url != :undefined do
        ['url="', url, '" ']
      end,
      'log="',
      log,
      '">',
      for case__ <- cases do
        to_xml(case__)
      end,
      '</testsuite>'
    ]
  end

  defp to_xml(r_state(test_suites: testSuites, axis: axis, properties: props)) do
    [
      '<testsuites>',
      properties_to_xml(axis, props),
      for testSuite <- testSuites do
        to_xml(testSuite)
      end,
      '</testsuites>'
    ]
  end

  defp properties_to_xml([], []) do
    []
  end

  defp properties_to_xml(axis, props) do
    [
      '<properties>',
      for {name, value} <- axis do
        ['<property name="', name, '" axis="yes" value="', value, '" />']
      end,
      for {name, value} <- props do
        ['<property name="', name, '" value="', value, '" />']
      end,
      '</properties>'
    ]
  end

  defp sanitize([?> | t]) do
    '&gt;' ++ sanitize(t)
  end

  defp sanitize([?< | t]) do
    '&lt;' ++ sanitize(t)
  end

  defp sanitize([?" | t]) do
    '&quot;' ++ sanitize(t)
  end

  defp sanitize([?' | t]) do
    '&apos;' ++ sanitize(t)
  end

  defp sanitize([?& | t]) do
    '&amp;' ++ sanitize(t)
  end

  defp sanitize([h | t]) do
    [h | sanitize(t)]
  end

  defp sanitize([]) do
    []
  end

  defp now_to_string(now) do
    {{yY, mM, dD}, {hH, mi, sS}} = :calendar.now_to_local_time(now)
    :io_lib.format('~w-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B', [yY, mM, dD, hH, mi, sS])
  end

  defp make_url(:undefined, _) do
    :undefined
  end

  defp make_url(_, []) do
    :undefined
  end

  defp make_url(urlBase0, log) do
    urlBase = :string.trim(urlBase0, :trailing, [?/])
    relativeLog = get_relative_log_url(log)
    :lists.flatten(:lists.join(?/, [urlBase, relativeLog]))
  end

  defp get_test_root(log) do
    logParts = :filename.split(log)
    :filename.join(:lists.sublist(logParts, 1, length(logParts) - 3))
  end

  defp get_relative_log_url(log) do
    logParts = :filename.split(log)
    start = length(logParts) - 3
    length = 3 + 1

    :lists.flatten(
      :lists.join(
        ?/,
        :lists.sublist(logParts, start, length)
      )
    )
  end

  defp count_tcs([r_testcase(name: confCase) | tCs], ok, f, s)
       when confCase == 'init_per_suite' or confCase == 'end_per_suite' or
              confCase == 'init_per_group' or
              confCase == 'end_per_group' do
    count_tcs(tCs, ok, f, s)
  end

  defp count_tcs([r_testcase(result: :passed) | tCs], ok, f, s) do
    count_tcs(tCs, ok + 1, f, s)
  end

  defp count_tcs([r_testcase(result: {:fail, _}) | tCs], ok, f, s) do
    count_tcs(tCs, ok, f + 1, s)
  end

  defp count_tcs([r_testcase(result: {:skipped, _}) | tCs], ok, f, s) do
    count_tcs(tCs, ok, f, s + 1)
  end

  defp count_tcs([r_testcase(result: {:auto_skipped, _}) | tCs], ok, f, s) do
    count_tcs(tCs, ok, f, s + 1)
  end

  defp count_tcs([], ok, f, s) do
    {ok + f + s, f, s}
  end
end
