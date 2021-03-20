defmodule :m_eunit_surefire do
  use Bitwise
  @behaviour :eunit_listener
  require Record

  Record.defrecord(:r_test, :test,
    f: :undefined,
    desc: :undefined,
    timeout: :undefined,
    location: :undefined,
    line: 0
  )

  Record.defrecord(:r_group, :group,
    desc: :undefined,
    order: :undefined,
    timeout: :undefined,
    context: :undefined,
    spawn: :undefined,
    tests: :undefined
  )

  Record.defrecord(:r_context, :context, setup: :undefined, cleanup: :undefined, process: :local)

  Record.defrecord(:r_testcase, :testcase,
    name: :undefined,
    description: :undefined,
    result: :undefined,
    time: :undefined,
    output: :undefined
  )

  Record.defrecord(:r_testsuite, :testsuite,
    id: 0,
    name: <<>>,
    time: 0,
    output: <<>>,
    succeeded: 0,
    failed: 0,
    aborted: 0,
    skipped: 0,
    testcases: []
  )

  Record.defrecord(:r_state, :state, verbose: false, indent: 0, xmldir: '.', testsuites: [])

  def start() do
    start([])
  end

  def start(options) do
    :eunit_listener.start(:eunit_surefire, options)
  end

  def init(options) do
    xMLDir = :proplists.get_value(:dir, options, '.')
    st = r_state(verbose: :proplists.get_bool(:verbose, options), xmldir: xMLDir, testsuites: [])

    receive do
      {:start, _Reference} ->
        st
    end
  end

  def terminate({:ok, _Data}, st) do
    testSuites = r_state(st, :testsuites)
    xmlDir = r_state(st, :xmldir)
    write_reports(testSuites, xmlDir)
    :ok
  end

  def terminate({:error, _Reason}, _St) do
    :ok
  end

  def handle_begin(kind, data, st)
      when kind == :group or
             kind == :test do
    newId = :proplists.get_value(:id, data)

    case newId do
      [] ->
        st

      [groupId] ->
        desc = :proplists.get_value(:desc, data)
        testSuite = r_testsuite(id: groupId, name: desc)

        r_state(st,
          testsuites:
            store_suite(
              testSuite,
              r_state(st, :testsuites)
            )
        )

      _ ->
        st
    end
  end

  def handle_end(:group, data, st) do
    case :proplists.get_value(:id, data) do
      [] ->
        st

      [groupId | _] ->
        testSuites = r_state(st, :testsuites)

        testSuite =
          lookup_suite_by_group_id(
            groupId,
            testSuites
          )

        time = :proplists.get_value(:time, data)
        output = :proplists.get_value(:output, data)
        newTestSuite = r_testsuite(testSuite, time: time, output: output)
        r_state(st, testsuites: store_suite(newTestSuite, testSuites))
    end
  end

  def handle_end(:test, data, st) do
    [groupId | _] = :proplists.get_value(:id, data)
    testSuites = r_state(st, :testsuites)

    testSuite =
      lookup_suite_by_group_id(
        groupId,
        testSuites
      )

    name =
      format_name(
        :proplists.get_value(:source, data),
        :proplists.get_value(:line, data)
      )

    desc = format_desc(:proplists.get_value(:desc, data))
    result = :proplists.get_value(:status, data)
    time = :proplists.get_value(:time, data)
    output = :proplists.get_value(:output, data)
    testCase = r_testcase(name: name, description: desc, time: time, output: output)
    newTestSuite = add_testcase_to_testsuite(result, testCase, testSuite)
    r_state(st, testsuites: store_suite(newTestSuite, testSuites))
  end

  def handle_cancel(:group, data, st) do
    case :proplists.get_value(:reason, data) do
      {:abort, {somethingFailed, exception}}
      when somethingFailed === :setup_failed or
             somethingFailed === :cleanup_failed ->
        [groupId | _] = :proplists.get_value(:id, data)
        testSuites = r_state(st, :testsuites)

        testSuite =
          lookup_suite_by_group_id(
            groupId,
            testSuites
          )

        name =
          case somethingFailed do
            :setup_failed ->
              'fixture setup '

            :cleanup_failed ->
              'fixture cleanup '
          end ++
            :io_lib.format(
              '~w',
              [:proplists.get_value(:id, data)]
            )

        desc = format_desc(:proplists.get_value(:desc, data))
        testCase = r_testcase(name: name, description: desc, time: 0, output: <<>>)
        newTestSuite = add_testcase_to_testsuite({:error, exception}, testCase, testSuite)
        r_state(st, testsuites: store_suite(newTestSuite, testSuites))

      _ ->
        st
    end
  end

  def handle_cancel(:test, data, st) do
    [groupId | _] = :proplists.get_value(:id, data)
    testSuites = r_state(st, :testsuites)

    testSuite =
      lookup_suite_by_group_id(
        groupId,
        testSuites
      )

    name =
      format_name(
        :proplists.get_value(:source, data),
        :proplists.get_value(:line, data)
      )

    desc = format_desc(:proplists.get_value(:desc, data))
    reason = :proplists.get_value(:reason, data)

    testCase =
      r_testcase(name: name, description: desc, result: {:skipped, reason}, time: 0, output: <<>>)

    newTestSuite =
      r_testsuite(testSuite,
        skipped: r_testsuite(testSuite, :skipped) + 1,
        testcases: [
          testCase
          | r_testsuite(testSuite, :testcases)
        ]
      )

    r_state(st, testsuites: store_suite(newTestSuite, testSuites))
  end

  defp format_name({module, function, _Arity}, line) do
    :lists.flatten([
      :erlang.atom_to_list(module),
      ':',
      :erlang.integer_to_list(line),
      ' ',
      :erlang.atom_to_list(function)
    ])
  end

  defp format_desc(:undefined) do
    ''
  end

  defp format_desc(desc) when is_binary(desc) do
    :erlang.binary_to_list(desc)
  end

  defp format_desc(desc) when is_list(desc) do
    desc
  end

  defp lookup_suite_by_group_id(groupId, testSuites) do
    r_testsuite() = :lists.keyfind(groupId, r_testsuite(:id), testSuites)
  end

  defp store_suite(r_testsuite(id: groupId) = testSuite, testSuites) do
    :lists.keystore(groupId, r_testsuite(:id), testSuites, testSuite)
  end

  defp add_testcase_to_testsuite(:ok, testCaseTmp, testSuite) do
    testCase = r_testcase(testCaseTmp, result: :ok)

    r_testsuite(testSuite,
      succeeded: r_testsuite(testSuite, :succeeded) + 1,
      testcases: [testCase | r_testsuite(testSuite, :testcases)]
    )
  end

  defp add_testcase_to_testsuite({:error, exception}, testCaseTmp, testSuite) do
    case exception do
      {:error, {assertionException, _}, _}
      when assertionException == :assertion_failed or
             assertionException == :assertMatch_failed or
             assertionException == :assertEqual_failed or
             assertionException == :assertException_failed or
             assertionException == :assertCmd_failed or
             assertionException == :assertCmdOutput_failed ->
        testCase = r_testcase(testCaseTmp, result: {:failed, exception})

        r_testsuite(testSuite,
          failed: r_testsuite(testSuite, :failed) + 1,
          testcases: [testCase | r_testsuite(testSuite, :testcases)]
        )

      _ ->
        testCase = r_testcase(testCaseTmp, result: {:aborted, exception})

        r_testsuite(testSuite,
          aborted: r_testsuite(testSuite, :aborted) + 1,
          testcases: [testCase | r_testsuite(testSuite, :testcases)]
        )
    end
  end

  defp write_reports(testSuites, xmlDir) do
    :lists.foreach(
      fn testSuite ->
        write_report(testSuite, xmlDir)
      end,
      testSuites
    )
  end

  defp write_report(r_testsuite(name: name) = testSuite, xmlDir) do
    filename =
      :filename.join(
        xmlDir,
        :lists.flatten(['TEST-', escape_suitename(name)], '.xml')
      )

    case :file.open(
           filename,
           [:write, {:encoding, :utf8}]
         ) do
      {:ok, fileDescriptor} ->
        try do
          write_report_to(testSuite, fileDescriptor)
        after
          :file.close(fileDescriptor)
        end

      {:error, _Reason} = error ->
        throw(error)
    end
  end

  defp write_report_to(testSuite, fileDescriptor) do
    write_header(fileDescriptor)
    write_start_tag(testSuite, fileDescriptor)

    write_testcases(
      :lists.reverse(r_testsuite(testSuite, :testcases)),
      fileDescriptor
    )

    write_end_tag(fileDescriptor)
  end

  defp write_header(fileDescriptor) do
    :io.format(fileDescriptor, '~ts~ts', ["<?xml version=\"1.0\" encoding=\"UTF-8\" ?>", "\n"])
  end

  defp write_start_tag(
         r_testsuite(
           name: name,
           time: time,
           succeeded: succeeded,
           failed: failed,
           skipped: skipped,
           aborted: aborted
         ),
         fileDescriptor
       ) do
    total = succeeded + failed + skipped + aborted

    startTag = [
      "<testsuite tests=\"",
      :erlang.integer_to_list(total),
      "\" failures=\"",
      :erlang.integer_to_list(failed),
      "\" errors=\"",
      :erlang.integer_to_list(aborted),
      "\" skipped=\"",
      :erlang.integer_to_list(skipped),
      "\" time=\"",
      format_time(time),
      "\" name=\"",
      escape_attr(name),
      "\">",
      "\n"
    ]

    :io.format(fileDescriptor, '~ts', [startTag])
  end

  defp write_testcases([], _FileDescriptor) do
    :void
  end

  defp write_testcases([testCase | tail], fileDescriptor) do
    write_testcase(testCase, fileDescriptor)
    write_testcases(tail, fileDescriptor)
  end

  defp write_end_tag(fileDescriptor) do
    :io.format(fileDescriptor, '~ts~ts', ["</testsuite>", "\n"])
  end

  defp write_testcase(
         r_testcase(
           name: name,
           description: description,
           result: result,
           time: time,
           output: output
         ),
         fileDescriptor
       ) do
    descriptionAttr =
      case description do
        [] ->
          []

        _ ->
          [" (", escape_attr(description), ")"]
      end

    startTag = [
      "  ",
      "<testcase time=\"",
      format_time(time),
      "\" name=\"",
      escape_attr(name),
      descriptionAttr,
      "\""
    ]

    contentAndEndTag =
      case {result, output} do
        {:ok, <<>>} ->
          ["/>", "\n"]

        _ ->
          [
            ">",
            "\n",
            format_testcase_result(result),
            format_testcase_output(output),
            "  ",
            "</testcase>",
            "\n"
          ]
      end

    :io.format(fileDescriptor, '~ts~ts', [startTag, contentAndEndTag])
  end

  defp format_testcase_result(:ok) do
    [<<>>]
  end

  defp format_testcase_result({:failed, {:error, {type, _}, _} = exception})
       when is_atom(type) do
    [
      "  ",
      "  ",
      "<failure type=\"",
      escape_attr(:erlang.atom_to_list(type)),
      "\">",
      "\n",
      "::",
      escape_text(
        :eunit_lib.format_exception(
          exception,
          100
        )
      ),
      "  ",
      "  ",
      "</failure>",
      "\n"
    ]
  end

  defp format_testcase_result({:failed, term}) do
    [
      "  ",
      "  ",
      "<failure type=\"unknown\">",
      "\n",
      escape_text(:io_lib.write(term)),
      "  ",
      "  ",
      "</failure>",
      "\n"
    ]
  end

  defp format_testcase_result({:aborted, {class, _Term, _Trace} = exception})
       when is_atom(class) do
    [
      "  ",
      "  ",
      "<error type=\"",
      escape_attr(:erlang.atom_to_list(class)),
      "\">",
      "\n",
      "::",
      escape_text(
        :eunit_lib.format_exception(
          exception,
          100
        )
      ),
      "  ",
      "  ",
      "</error>",
      "\n"
    ]
  end

  defp format_testcase_result({:aborted, term}) do
    [
      "  ",
      "  ",
      "<error type=\"unknown\">",
      "\n",
      escape_text(:io_lib.write(term)),
      "  ",
      "  ",
      "</error>",
      "\n"
    ]
  end

  defp format_testcase_result({:skipped, {:abort, error}})
       when is_tuple(error) do
    [
      "  ",
      "  ",
      "<skipped type=\"",
      escape_attr(
        :erlang.atom_to_list(
          :erlang.element(
            1,
            error
          )
        )
      ),
      "\">",
      "\n",
      escape_text(:eunit_lib.format_error(error)),
      "  ",
      "  ",
      "</skipped>",
      "\n"
    ]
  end

  defp format_testcase_result({:skipped, {type, term}}) when is_atom(type) do
    [
      "  ",
      "  ",
      "<skipped type=\"",
      escape_attr(:erlang.atom_to_list(type)),
      "\">",
      "\n",
      escape_text(:io_lib.write(term)),
      "  ",
      "  ",
      "</skipped>",
      "\n"
    ]
  end

  defp format_testcase_result({:skipped, :timeout}) do
    ["  ", "  ", "<skipped type=\"timeout\"/>", "\n"]
  end

  defp format_testcase_result({:skipped, term}) do
    [
      "  ",
      "  ",
      "<skipped type=\"unknown\">",
      "\n",
      escape_text(:io_lib.write(term)),
      "  ",
      "  ",
      "</skipped>",
      "\n"
    ]
  end

  defp format_testcase_output(output) do
    ["  ", "  ", "<system-out>", escape_text(output), "\n", "  ", "  ", "</system-out>", "\n"]
  end

  defp format_time(time) do
    format_time_s(:lists.reverse(:erlang.integer_to_list(time)))
  end

  defp format_time_s([digit]) do
    ['0.00', digit]
  end

  defp format_time_s([digit1, digit2]) do
    ['0.0', digit2, digit1]
  end

  defp format_time_s([digit1, digit2, digit3]) do
    ['0.', digit3, digit2, digit1]
  end

  defp format_time_s([digit1, digit2, digit3 | tail]) do
    [:lists.reverse(tail), ?., digit3, digit2, digit1]
  end

  defp escape_suitename(binary) when is_binary(binary) do
    escape_suitename(:erlang.binary_to_list(binary))
  end

  defp escape_suitename('module \'' ++ string) do
    escape_suitename(string)
  end

  defp escape_suitename(string) do
    escape_suitename(string, [])
  end

  defp escape_suitename([], acc) do
    :lists.reverse(acc)
  end

  defp escape_suitename([?\s | tail], acc) do
    escape_suitename(tail, [?_ | acc])
  end

  defp escape_suitename([?' | tail], acc) do
    escape_suitename(tail, acc)
  end

  defp escape_suitename([?" | tail], acc) do
    escape_suitename(tail, acc)
  end

  defp escape_suitename([?/ | tail], acc) do
    escape_suitename(tail, [?: | acc])
  end

  defp escape_suitename([?\\ | tail], acc) do
    escape_suitename(tail, [?: | acc])
  end

  defp escape_suitename([char | tail], acc) when char < ?! do
    escape_suitename(tail, acc)
  end

  defp escape_suitename([char | tail], acc) when char > ?~ do
    escape_suitename(tail, acc)
  end

  defp escape_suitename([char | tail], acc) do
    escape_suitename(tail, [char | acc])
  end

  defp escape_text(text) when is_binary(text) do
    escape_text(:erlang.binary_to_list(text))
  end

  defp escape_text(text) do
    escape_xml(to_utf8(:lists.flatten(text)), [], false)
  end

  defp escape_attr(text) when is_binary(text) do
    escape_attr(:erlang.binary_to_list(text))
  end

  defp escape_attr(text) do
    escape_xml(to_utf8(:lists.flatten(text)), [], true)
  end

  defp escape_xml([], acc, _ForAttr) do
    :lists.reverse(acc)
  end

  defp escape_xml([?< | tail], acc, forAttr) do
    escape_xml(tail, [?;, ?t, ?l, ?& | acc], forAttr)
  end

  defp escape_xml([?> | tail], acc, forAttr) do
    escape_xml(tail, [?;, ?t, ?g, ?& | acc], forAttr)
  end

  defp escape_xml([?& | tail], acc, forAttr) do
    escape_xml(tail, [?;, ?p, ?m, ?a, ?& | acc], forAttr)
  end

  defp escape_xml([?" | tail], acc, true) do
    escape_xml(tail, [?;, ?t, ?o, ?u, ?q, ?& | acc], true)
  end

  defp escape_xml([char | tail], acc, forAttr)
       when char == ?\n or
              char == ?\r or char == ?\t do
    escape_xml(tail, [char | acc], forAttr)
  end

  defp escape_xml([char | tail], acc, forAttr)
       when 0 <= char and
              char <= 31 do
    escape_xml(tail, acc, forAttr)
  end

  defp escape_xml([char | tail], acc, forAttr)
       when is_integer(char) do
    escape_xml(tail, [char | acc], forAttr)
  end

  defp to_utf8(desc) when is_binary(desc) do
    case :unicode.characters_to_list(desc) do
      {_, _, _} ->
        :unicode.characters_to_list(desc, :latin1)

      x ->
        x
    end
  end

  defp to_utf8(desc) when is_list(desc) do
    try do
      to_utf8(:erlang.list_to_binary(desc))
    catch
      _, _ ->
        desc
    end
  end
end
