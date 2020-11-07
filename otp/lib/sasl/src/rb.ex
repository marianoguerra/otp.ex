defmodule :m_rb do
  use Bitwise
  @behaviour :gen_server
  require Record

  Record.defrecord(:r_state, :state,
    dir: :undefined,
    data: :undefined,
    device: :undefined,
    max: :undefined,
    type: :undefined,
    abort: :undefined,
    log: :undefined
  )

  def start() do
    start([])
  end

  def start(options) do
    :supervisor.start_child(
      :sasl_sup,
      {:rb_server, {:rb, :start_link, [options]}, :temporary, :brutal_kill, :worker, [:rb]}
    )
  end

  def start_link(options) do
    :gen_server.start_link({:local, :rb_server}, :rb, options, [])
  end

  def stop() do
    :supervisor.terminate_child(:sasl_sup, :rb_server)
  end

  def rescan() do
    rescan([])
  end

  def rescan(options) do
    call({:rescan, options})
  end

  def list() do
    list(:all)
  end

  def list(type) do
    call({:list, type})
  end

  def log_list() do
    log_list(:all)
  end

  def log_list(type) do
    call({:log_list, type})
  end

  def show() do
    call(:show)
  end

  def show(number) when is_integer(number) do
    call({:show_number, number})
  end

  def show(type) when is_atom(type) do
    call({:show_type, type})
  end

  def grep(regExp) do
    call({:grep, regExp})
  end

  def filter(filters) when is_list(filters) do
    call({:filter, filters})
  end

  def filter(filters, fDates)
      when is_list(filters) and is_tuple(fDates) do
    call({:filter, {filters, fDates}})
  end

  def start_log(fileName) do
    call({:start_log, fileName})
  end

  def stop_log() do
    call(:stop_log)
  end

  def h() do
    help()
  end

  def help() do
    :io.format('~nReport Browser Tool - usage~n')
    :io.format('===========================~n')
    :io.format('rb:start()         - start the rb_server with default options~n')
    :io.format('rb:start(Options)  - where Options is a list of:~n')
    print_options()
    :io.format('rb:h()             - print this help~n')
    :io.format('rb:help()          - print this help~n')
    :io.format('rb:list()          - list all reports~n')
    :io.format('rb:list(Type)      - list all reports of type Type~n')
    :io.format('rb:log_list()      - log list of all reports~n')
    :io.format('rb:log_list(Type)  - log list of all reports of type Type~n')
    :io.format('      currently supported types are:~n')
    print_types()
    :io.format('rb:grep(RegExp)      - print reports containing RegExp.~n')
    :io.format('                     RegExp must be a valid argument for ~n')
    :io.format('                     the function re:run/2 or re:run/3.~n')
    :io.format('rb:filter(Filters) - print reports matching Filters.~n')
    :io.format('                     reports must be proplists.~n')
    :io.format('      Filters is a list of tuples of the following form:~n')
    print_filters()
    :io.format('rb:filter(Filters, Dates)  -~n')
    :io.format('      same as rb:filter/1 but accepts date ranges to filter reports.~n')
    :io.format('      Dates must be of the following form:~n')
    print_dates()
    :io.format('rb:rescan()        - rescans the report directory with same~n')
    :io.format('                     options.~n')
    :io.format('rb:rescan(Options) - rescans the report directory with new~n')
    :io.format('                     options. Options is same as in start/1.~n')
    :io.format('rb:show(Number)    - print report no Number~n')
    :io.format('rb:show(Type)      - print all reports of type Type~n')
    :io.format('rb:show()          - print all reports~n')
    :io.format('rb:start_log(File) - redirect all reports to file or io_device~n')
    :io.format('rb:stop_log()      - close the log file and redirect to~n')
    :io.format('                     standard_io~n')
    :io.format('rb:stop            - stop the rb_server~n')
  end

  defp call(req) do
    :gen_server.call(:rb_server, req, :infinity)
  end

  defp print_options() do
    :io.format('      {start_log, FileName}~n')
    :io.format('         - default: standard_io~n')
    :io.format('      {max, MaxNoOfReports}~n')
    :io.format('         - MaxNoOfReports should be an integer or \'all\'~n')
    :io.format('         - default: all~n')
    :io.format('      {report_dir, DirString}~n')
    :io.format('         - DirString should be a string without trailing~n')
    :io.format('         - directory delimiter.~n')
    :io.format('         - default: {sasl, error_logger_mf_dir}~n')
    :io.format('      {type, ReportType}~n')
    :io.format('         - ReportType should be a supported type, \'all\'~n')
    :io.format('         - or a list of supported types~n')
    :io.format('         - default: all~n')
    :io.format('      {abort_on_error, Bool}~n')
    :io.format('         - Bool: true | false~n')
    :io.format('         - default: false~n')
  end

  defp print_types() do
    :io.format('         - crash_report~n')
    :io.format('         - supervisor_report~n')
    :io.format('         - progress~n')
    :io.format('         - error~n')
  end

  defp print_filters() do
    :io.format('      - {Key, Value}~n')
    :io.format('        includes report containing {Key, Value}~n')
    :io.format('      - {Key, Value, no}~n')
    :io.format('        excludes report containing {Key, Value}~n')
    :io.format('      - {Key, RegExp, re}~n')
    :io.format('        RegExp must be a valid argument for ~n')
    :io.format('        the function re:run/2 or re:run/3.~n')
    :io.format('      - {Key, RegExp, re, no}~n')
    :io.format('        excludes report containing {Key, RegExp}~n')
  end

  defp print_dates() do
    :io.format('      - {StartDate, EndDate}~n')
    :io.format('        StartDate = EndDate = {{Y,M,D},{H,M,S}} ~n')
    :io.format('        prints the reports with date between StartDate and EndDate~n')
    :io.format('      - {StartDate, from}~n')
    :io.format('        prints the reports with date greater than StartDate~n')
    :io.format('      - {EndDate, to}~n')
    :io.format('        prints the reports with date lesser than StartDate~n')
  end

  def init(options) do
    :erlang.process_flag(:priority, :low)
    :erlang.process_flag(:trap_exit, true)
    log = get_option(options, :start_log, :standard_io)
    device = open_log_file(log)
    dir = get_report_dir(options)
    max = get_option(options, :max, :all)
    type = get_option(options, :type, :all)
    abort = get_option(options, :abort_on_error, false)
    data = scan_files(dir ++ '/', max, type)

    {:ok,
     r_state(
       dir: dir ++ '/',
       data: data,
       device: device,
       max: max,
       type: type,
       abort: abort,
       log: log
     )}
  end

  def handle_call({:rescan, options}, _From, state) do
    {device, log1} =
      case get_option(options, :start_log, {:undefined}) do
        {:undefined} ->
          {r_state(state, :device), r_state(state, :log)}

        log ->
          close_device(r_state(state, :device))
          {open_log_file(log), log}
      end

    max = get_option(options, :max, r_state(state, :max))
    type = get_option(options, :type, r_state(state, :type))
    abort = get_option(options, :abort_on_error, false)
    data = scan_files(r_state(state, :dir), max, type)

    newState =
      r_state(state, data: data, max: max, type: type, device: device, abort: abort, log: log1)

    {:reply, :ok, newState}
  end

  def handle_call(_, _From, r_state(data: :undefined)) do
    {:reply, {:error, :no_data}, r_state()}
  end

  def handle_call({:list, type}, _From, state) do
    print_list(:standard_io, r_state(state, :data), type)
    {:reply, :ok, state}
  end

  def handle_call({:log_list, type}, _From, state) do
    print_list(r_state(state, :device), r_state(state, :data), type)
    {:reply, :ok, state}
  end

  def handle_call({:start_log, fileName}, _From, state) do
    newDevice = open_log_file(fileName)
    {:reply, :ok, r_state(state, device: newDevice)}
  end

  def handle_call(:stop_log, _From, state) do
    close_device(r_state(state, :device))
    {:reply, :ok, r_state(state, device: :standard_io)}
  end

  def handle_call({:show_number, number}, _From, state) do
    r_state(dir: dir, data: data, device: device, abort: abort, log: log) = state
    newDevice = print_report_by_num(dir, data, number, device, abort, log)
    {:reply, :ok, r_state(state, device: newDevice)}
  end

  def handle_call({:show_type, type}, _From, state) do
    r_state(dir: dir, data: data, device: device, abort: abort, log: log) = state
    newDevice = print_typed_reports(dir, data, type, device, abort, log)
    {:reply, :ok, r_state(state, device: newDevice)}
  end

  def handle_call(:show, _From, state) do
    r_state(dir: dir, data: data, device: device, abort: abort, log: log) = state
    newDevice = print_all_reports(dir, data, device, abort, log)
    {:reply, :ok, r_state(state, device: newDevice)}
  end

  def handle_call({:grep, regExp}, _From, state) do
    r_state(dir: dir, data: data, device: device, abort: abort, log: log) = state

    try do
      print_grep_reports(dir, data, regExp, device, abort, log)
    catch
      :error, error ->
        {:reply, {:error, error}, state}
    else
      newDevice ->
        {:reply, :ok, r_state(state, device: newDevice)}
    end
  end

  def handle_call({:filter, filters}, _From, state) do
    r_state(dir: dir, data: data, device: device, abort: abort, log: log) = state

    try do
      filter_all_reports(dir, data, filters, device, abort, log)
    catch
      :error, error ->
        {:reply, {:error, error}, state}
    else
      newDevice ->
        {:reply, :ok, r_state(state, device: newDevice)}
    end
  end

  def terminate(_Reason, r_state(device: device)) do
    close_device(device)
  end

  def handle_cast(_Msg, state) do
    {:noreply, state}
  end

  def handle_info(_Info, state) do
    {:noreply, state}
  end

  def code_change(_OldVsn, state, _Extra) do
    {:ok, state}
  end

  defp open_log_file(:standard_io) do
    :standard_io
  end

  defp open_log_file(fd)
       when is_atom(fd) and
              fd !== :standard_error do
    case :erlang.whereis(fd) do
      :undefined ->
        :io.format('rb: Registered name not found \'~ts\'.~n', [fd])
        :io.format('rb: Using standard_io~n')
        open_log_file(:standard_io)

      pid ->
        open_log_file(pid)
    end
  end

  defp open_log_file(fd) when is_pid(fd) do
    fd
  end

  defp open_log_file(fileName) when is_list(fileName) do
    case :file.open(
           fileName,
           [:write, :append, {:encoding, :utf8}]
         ) do
      {:ok, fd} ->
        fd

      error ->
        :io.format('rb: Cannot open file \'~ts\' (~w).~n', [fileName, error])
        :io.format('rb: Using standard_io~n')
        :standard_io
    end
  end

  defp open_log_file(:standard_error) do
    :io.format('rb: Using standard_io~n')
    :standard_io
  end

  defp close_device(fd) when is_pid(fd) do
    try do
      :file.close(fd)
    catch
      :error, e -> {:EXIT, {e, __STACKTRACE__}}
      :exit, e -> {:EXIT, e}
      e -> e
    end
  end

  defp close_device(_) do
    :ok
  end

  defp get_option(options, key, default) do
    case :lists.keysearch(key, 1, options) do
      {:value, {_Key, value}} ->
        value

      _ ->
        default
    end
  end

  defp get_report_dir(options) do
    case :lists.keysearch(:report_dir, 1, options) do
      {:value, {_Key, rptDir}} ->
        rptDir

      _ ->
        case (try do
                :application.get_env(:sasl, :error_logger_mf_dir)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:ok, dir} ->
            dir

          _ ->
            exit('cannot locate report directory')
        end
    end
  end

  defp scan_files(rptDir, max, type) do
    case :file.open(rptDir ++ '/index', [:raw, :read]) do
      {:ok, fd} ->
        case (try do
                :file.read(fd, 1)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:ok, [lastWritten]} ->
            :ok = :file.close(fd)
            files = make_file_list(rptDir, lastWritten)
            scan_files(rptDir, files, max, type)

          _X ->
            _ = :file.close(fd)
            exit('cannot read the index file')
        end

      _X ->
        exit('cannot read the index file')
    end
  end

  defp make_file_list(dir, firstFileNo) do
    case :file.list_dir(dir) do
      {:ok, fileNames} ->
        fileNumbers =
          :lists.zf(
            fn name ->
              case (try do
                      :erlang.list_to_integer(name)
                    catch
                      :error, e -> {:EXIT, {e, __STACKTRACE__}}
                      :exit, e -> {:EXIT, e}
                      e -> e
                    end) do
                int when is_integer(int) ->
                  {true, int}

                _ ->
                  false
              end
            end,
            fileNames
          )

        shift(:lists.sort(fileNumbers), firstFileNo)

      _ ->
        exit({:bad_directory, dir})
    end
  end

  defp shift(list, first) do
    shift(list, first, [])
  end

  defp shift([h | t], h, res) do
    [h | res] ++ :lists.reverse(t)
  end

  defp shift([h | t], first, res) do
    shift(t, first, [h | res])
  end

  defp shift([], _, res) do
    res
  end

  defp scan_files(dir, files, max, type) do
    scan_files(dir, 1, files, [], max, type)
  end

  defp scan_files(_Dir, _, [], res, _Max, _Type) do
    res
  end

  defp scan_files(_Dir, _, _Files, res, max, _Type)
       when max <= 0 do
    res
  end

  defp scan_files(dir, no, [h | t], res, max, type) do
    data = get_report_data_from_file(dir, no, h, max, type)
    len = length(data)
    newMax = dec_max(max, len)
    newNo = no + len
    newData = data ++ res
    scan_files(dir, newNo, t, newData, newMax, type)
  end

  defp dec_max(:all, _) do
    :all
  end

  defp dec_max(x, y) do
    x - y
  end

  defp get_report_data_from_file(dir, no, fileNr, max, type) do
    fname = :erlang.integer_to_list(fileNr)
    fileName = :lists.concat([dir, fname])

    case :file.open(fileName, [:read]) do
      {:ok, fd} when is_pid(fd) ->
        read_reports(no, fd, fname, max, type)

      _ ->
        [{no, :unknown, 'Can\'t open file ' ++ fname, '???', fname, 0}]
    end
  end

  defp read_reports(no, fd, fname, max, type) do
    :io.format('rb: reading report...')

    case (try do
            read_reports(fd, [], type)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, res} ->
        :ok = :file.close(fd)
        :io.format('done.~n')

        newRes =
          cond do
            length(res) > max ->
              :lists.sublist(res, 1, max)

            true ->
              res
          end

        add_report_data(newRes, no, fname)

      {:error, [problem | res]} ->
        _ = :file.close(fd)
        :io.format('Error: ~tp~n', [problem])
        :io.format('Salvaged ~p entries from corrupt report file ~ts...~n', [length(res), fname])

        newRes =
          cond do
            length([problem | res]) > max ->
              :lists.sublist([problem | res], 1, max)

            true ->
              [problem | res]
          end

        add_report_data(newRes, no, fname)

      else__ ->
        :io.format('err ~tp~n', [else__])
        [{no, :unknown, 'Can\'t read reports from file ' ++ fname, '???', fname, 0}]
    end
  end

  defp add_report_data(res, no, fName) do
    add_report_data(res, no, fName, [])
  end

  defp add_report_data([{type, shortDescr, date, filePos} | t], no, fName, res) do
    add_report_data(t, no + 1, fName, [{no, type, shortDescr, date, fName, filePos} | res])
  end

  defp add_report_data([], _No, _FName, res) do
    res
  end

  defp read_reports(fd, res, type) do
    {:ok, filePos} = :file.position(fd, :cur)

    case (try do
            read_report(fd)
          catch
            :error, e -> {:EXIT, {e, __STACKTRACE__}}
            :exit, e -> {:EXIT, e}
            e -> e
          end) do
      {:ok, report} ->
        realType = get_type(report)
        {shortDescr, date} = get_short_descr(report)
        rep = {realType, shortDescr, date, filePos}

        cond do
          type == :all ->
            read_reports(fd, [rep | res], type)

          realType == type ->
            read_reports(fd, [rep | res], type)

          is_list(type) ->
            case :lists.member(realType, type) do
              true ->
                read_reports(fd, [rep | res], type)

              _ ->
                read_reports(fd, res, type)
            end

          true ->
            read_reports(fd, res, type)
        end

      {:error, error} ->
        {:error, [{:unknown, error, [], filePos} | res]}

      :eof ->
        {:ok, res}

      {:EXIT, reason} ->
        [{:unknown, reason, [], filePos} | res]
    end
  end

  defp read_report(fd) do
    case :io.get_chars(fd, :"", 2) do
      [hi, lo] ->
        size = get_int16(hi, lo)

        case :io.get_chars(fd, :"", size) do
          :eof ->
            {:error, 'Premature end of file'}

          list ->
            bin = :erlang.list_to_binary(list)
            ref = make_ref()

            case (try do
                    {ref, :erlang.binary_to_term(bin)}
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              {:EXIT, _} ->
                {:error, 'Incomplete erlang term in log'}

              {^ref, term} ->
                {:ok, term}
            end
        end

      :eof ->
        :eof
    end
  end

  defp get_int16(hi, lo) do
    (hi <<< 8 &&& 65280) ||| (lo &&& 255)
  end

  defp get_type({_Time, {:error_report, _Pid, {_, :crash_report, _}}}) do
    :crash_report
  end

  defp get_type({_Time, {:error_report, _Pid, {_, :supervisor_report, _}}}) do
    :supervisor_report
  end

  defp get_type({_Time, {:info_report, _Pid, {_, :progress, _}}}) do
    :progress
  end

  defp get_type({_Time, {type, _, _}}) do
    type
  end

  defp get_type(_) do
    :unknown
  end

  defp get_short_descr({{date, time}, {:error_report, pid, {_, :crash_report, rep}}}) do
    [ownRep | _] = rep

    name =
      case :lists.keysearch(:registered_name, 1, ownRep) do
        {:value, {_Key, []}} ->
          case :lists.keysearch(:initial_call, 1, ownRep) do
            {:value, {_K, {m, _F, _A}}} ->
              m

            _ ->
              pid
          end

        {:value, {_Key, n}} ->
          n

        _ ->
          pid
      end

    {name, date_str(date, time)}
  end

  defp get_short_descr({{date, time}, {:error_report, pid, {_, :supervisor_report, rep}}}) do
    name =
      case :lists.keysearch(:supervisor, 1, rep) do
        {:value, {_Key, n}} when is_atom(n) ->
          n

        _ ->
          pid
      end

    {name, date_str(date, time)}
  end

  defp get_short_descr({{date, time}, {_Type, pid, _}}) do
    {pid, date_str(date, time)}
  end

  defp get_short_descr(_) do
    {:"???", '???'}
  end

  defp date_str({y, mo, d} = date, {h, mi, s} = time) do
    case :application.get_env(:sasl, :utc_log) do
      {:ok, true} ->
        {{yY, moMo, dD}, {hH, miMi, sS}} = local_time_to_universal_time({date, time})

        :lists.flatten(
          :io_lib.format(
            '~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w UTC',
            [yY, moMo, dD, hH, miMi, sS]
          )
        )

      _ ->
        :lists.flatten(
          :io_lib.format('~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w', [y, mo, d, h, mi, s])
        )
    end
  end

  defp local_time_to_universal_time({date, time}) do
    case :calendar.local_time_to_universal_time_dst({date, time}) do
      [uCT] ->
        uCT

      [uCT1, _UCT2] ->
        uCT1

      [] ->
        {date, time}
    end
  end

  defp print_list(fd, data0, type) do
    modifier = :misc_supp.modifier(fd)
    header = {'No', 'Type', 'Process', 'Date     Time'}
    {descrWidth, dateWidth, data} = find_widths(data0, modifier, 7, 13, [])
    format = :lists.concat(['~4s~20s ~', descrWidth, 's~20s~n'])
    :io.format(fd, format, :erlang.tuple_to_list(header))
    :io.format(fd, format, ['==', '====', '=======', '====     ===='])
    print_list(fd, data, type, descrWidth, dateWidth, modifier)
  end

  defp print_list(_, [], _, _, _, _) do
    true
  end

  defp print_list(fd, [h | t], type, width, dateWidth, modifier) do
    print_one_report(fd, h, type, width, dateWidth, modifier)
    print_list(fd, t, type, width, dateWidth, modifier)
  end

  defp find_widths([], _Modifier, descrWidth, dateWidth, data) do
    {descrWidth + 1, dateWidth + 1, :lists.reverse(data)}
  end

  defp find_widths([h | t], modifier, descrWidth, dateWidth, data) do
    descrTerm = :erlang.element(3, h)
    descr = :io_lib.format('~' ++ modifier ++ 'w', [descrTerm])
    descrTry = :string.length(descr)

    newDescrWidth =
      cond do
        descrTry > descrWidth ->
          descrTry

        true ->
          descrWidth
      end

    dateTry = :string.length(:erlang.element(4, h))

    newDateWitdh =
      cond do
        dateTry > dateWidth ->
          dateTry

        true ->
          dateWidth
      end

    newH = :erlang.setelement(3, h, descr)
    find_widths(t, modifier, newDescrWidth, newDateWitdh, [newH | data])
  end

  defp print_one_report(
         fd,
         {no, realType, shortDescr, date, _Fname, _FilePos},
         wantedType,
         width,
         dateWidth,
         modifier
       ) do
    cond do
      wantedType == :all ->
        print_short_descr(fd, no, realType, shortDescr, date, width, dateWidth, modifier)

      wantedType == realType ->
        print_short_descr(fd, no, realType, shortDescr, date, width, dateWidth, modifier)

      true ->
        :ok
    end
  end

  defp print_short_descr(fd, no, type, shortDescr, date, width, dateWidth, modifier) do
    format = :lists.concat(['~4w~20', modifier, 'w ~', width, modifier, 's~', dateWidth, 's~n'])
    :io.format(fd, format, [no, type, shortDescr, date])
  end

  defp print_report_by_num(dir, data, number, device, abort, log) do
    {_, device1} = print_report(dir, data, number, device, abort, log)
    device1
  end

  defp print_typed_reports(_Dir, [], _Type, device, _Abort, _Log) do
    device
  end

  defp print_typed_reports(dir, data, type, device, abort, log) do
    {next, device1} =
      case :erlang.element(
             2,
             hd(data)
           ) do
        ^type ->
          print_report(dir, data, :erlang.element(1, hd(data)), device, abort, log)

        _ ->
          {:proceed, device}
      end

    cond do
      next == :abort ->
        device1

      true ->
        print_typed_reports(dir, tl(data), type, device1, abort, log)
    end
  end

  defp print_all_reports(_Dir, [], device, _Abort, _Log) do
    device
  end

  defp print_all_reports(dir, data, device, abort, log) do
    {next, device1} = print_report(dir, data, :erlang.element(1, hd(data)), device, abort, log)

    cond do
      next == :abort ->
        device1

      true ->
        print_all_reports(dir, tl(data), device1, abort, log)
    end
  end

  defp print_report(dir, data, number, device, abort, log) do
    case find_report(data, number) do
      {fname, filePosition} ->
        fileName = :lists.concat([dir, fname])

        case :file.open(fileName, [:read]) do
          {:ok, fd} ->
            read_rep(fd, filePosition, device, abort, log)

          _ ->
            :io.format('rb: can\'t open file ~tp~n', [fname])
            {:proceed, device}
        end

      :no_report ->
        {:proceed, device}
    end
  end

  defp find_report(
         [
           {no, _Type, _Descr, _Date, fname, filePosition}
           | _T
         ],
         no
       ) do
    {fname, filePosition}
  end

  defp find_report([_H | t], no) do
    find_report(t, no)
  end

  defp find_report([], no) do
    :io.format('There is no report with number ~p.~n', [no])
    :no_report
  end

  defp print_grep_reports(_Dir, [], _RegExp, device, _Abort, _Log) do
    device
  end

  defp print_grep_reports(dir, data, regExp, device, abort, log) do
    {next, device1} =
      print_grep_report(dir, data, :erlang.element(1, hd(data)), device, regExp, abort, log)

    cond do
      next == :abort ->
        device1

      true ->
        print_grep_reports(dir, tl(data), regExp, device1, abort, log)
    end
  end

  defp print_grep_report(dir, data, number, device, regExp, abort, log) do
    {fname, filePosition} = find_report(data, number)
    fileName = :lists.concat([dir, fname])

    case :file.open(fileName, [:read]) do
      {:ok, fd} when is_pid(fd) ->
        check_rep(fd, filePosition, device, regExp, number, abort, log)

      _ ->
        :io.format('rb: can\'t open file ~tp~n', [fname])
        {:proceed, device}
    end
  end

  defp check_rep(fd, filePosition, device, regExp, number, abort, log) do
    case read_rep_msg(fd, filePosition) do
      {date, msg} ->
        msgStr = :lists.flatten(:io_lib.format('~tp', [msg]))

        case run_re(msgStr, regExp) do
          :match ->
            :io.format('Found match in report number ~w~n', [number])

            case (try do
                    :rb_format_supp.print(date, msg, device)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              {:EXIT, _} ->
                handle_bad_form(date, msg, device, abort, log)

              _ ->
                {:proceed, device}
            end

          _ ->
            {:proceed, device}
        end

      _ ->
        :io.format('rb: Cannot read from file~n')
        {:proceed, device}
    end
  end

  defp run_re(subject, {regexp, options}) do
    run_re(subject, regexp, options)
  end

  defp run_re(subject, regexp) do
    run_re(subject, regexp, [])
  end

  defp run_re(subject, regexp, options) do
    case :re.run(subject, regexp, [:unicode | options -- [:unicode]]) do
      :nomatch ->
        :nomatch

      _ ->
        :match
    end
  end

  defp filter_all_reports(_Dir, [], _Filters, device, _Abort, _Log) do
    device
  end

  defp filter_all_reports(dir, data, filters, device, abort, log) do
    {next, device1} =
      filter_report(dir, data, filters, :erlang.element(1, hd(data)), device, abort, log)

    cond do
      next == :abort ->
        device1

      true ->
        filter_all_reports(dir, tl(data), filters, device1, abort, log)
    end
  end

  defp filter_report(dir, data, filters, number, device, abort, log) do
    case find_report(data, number) do
      {fname, filePosition} ->
        fileName = :lists.concat([dir, fname])

        case :file.open(fileName, [:read]) do
          {:ok, fd} ->
            filter_rep(filters, fd, filePosition, device, abort, log)

          _ ->
            :io.format('rb: can\'t open file ~tp~n', [fname])
            {:proceed, device}
        end

      :no_report ->
        {:proceed, device}
    end
  end

  defp filter_rep({filters, fDates}, fd, filePosition, device, abort, log) do
    repMsg = read_rep_msg(fd, filePosition)

    case repMsg do
      {_DateStr, {date, _Msg}} ->
        case compare_dates(date, fDates) do
          true ->
            print_filter_report(repMsg, filters, device, abort, log)

          _ ->
            {:proceed, device}
        end

      _ ->
        :io.format('rb: Cannot read from file~n')
        {:proceed, device}
    end
  end

  defp filter_rep(filters, fd, filePosition, device, abort, log) do
    repMsg = read_rep_msg(fd, filePosition)

    case repMsg do
      {date, msg} ->
        print_filter_report({date, msg}, filters, device, abort, log)

      _ ->
        :io.format('rb: Cannot read from file~n')
        {:proceed, device}
    end
  end

  defp filter_report([], _Msg) do
    true
  end

  defp filter_report([{key, value} | t], msg) do
    case :proplists.get_value(key, msg) do
      ^value ->
        filter_report(t, msg)

      _ ->
        false
    end
  end

  defp filter_report([{key, value, :no} | t], msg) do
    case :proplists.get_value(key, msg) do
      ^value ->
        false

      _ ->
        filter_report(t, msg)
    end
  end

  defp filter_report([{key, regExp, :re} | t], msg) do
    case :proplists.get_value(key, msg) do
      :undefined ->
        false

      value ->
        subject = :lists.flatten(:io_lib.format('~tp', [value]))

        case run_re(subject, regExp) do
          :match ->
            filter_report(t, msg)

          _ ->
            false
        end
    end
  end

  defp filter_report([{key, regExp, :re, :no} | t], msg) do
    case :proplists.get_value(key, msg) do
      :undefined ->
        true

      value ->
        subject = :lists.flatten(:io_lib.format('~tp', [value]))

        case run_re(subject, regExp) do
          :match ->
            false

          _ ->
            filter_report(t, msg)
        end
    end
  end

  defp get_compare_dates(date, compareDate) do
    case :application.get_env(:sasl, :utc_log) do
      {:ok, true} ->
        {local_time_to_universal_time(date), local_time_to_universal_time(compareDate)}

      _ ->
        {date, compareDate}
    end
  end

  defp get_compare_dates(date, from, to) do
    case :application.get_env(:sasl, :utc_log) do
      {:ok, true} ->
        {local_time_to_universal_time(date), local_time_to_universal_time(from),
         local_time_to_universal_time(to)}

      _ ->
        {date, from, to}
    end
  end

  defp compare_dates(date, {compareDate, :from}) do
    {date2, dateFrom} = get_compare_dates(date, compareDate)

    :calendar.datetime_to_gregorian_seconds(date2) >=
      :calendar.datetime_to_gregorian_seconds(dateFrom)
  end

  defp compare_dates(date, {compareDate, :to}) do
    {date2, dateTo} = get_compare_dates(date, compareDate)

    :calendar.datetime_to_gregorian_seconds(date2) <=
      :calendar.datetime_to_gregorian_seconds(dateTo)
  end

  defp compare_dates(date, {from, to}) do
    {date2, dateFrom, dateTo} = get_compare_dates(date, from, to)

    :calendar.datetime_to_gregorian_seconds(date2) >=
      :calendar.datetime_to_gregorian_seconds(dateFrom) and
      :calendar.datetime_to_gregorian_seconds(date2) <=
        :calendar.datetime_to_gregorian_seconds(dateTo)
  end

  defp print_filter_report({date, msg}, filters, device, abort, log) do
    {_D, m} = msg
    {_, _, m2} = m

    case m2 do
      {_, _, report} ->
        case filter_report(filters, report) do
          true ->
            case (try do
                    :rb_format_supp.print(date, msg, device)
                  catch
                    :error, e -> {:EXIT, {e, __STACKTRACE__}}
                    :exit, e -> {:EXIT, e}
                    e -> e
                  end) do
              {:EXIT, _} ->
                handle_bad_form(date, msg, device, abort, log)

              _ ->
                {:proceed, device}
            end

          _ ->
            {:proceed, device}
        end

      _ ->
        {:proceed, device}
    end
  end

  defp read_rep(fd, filePosition, device, abort, log) do
    case read_rep_msg(fd, filePosition) do
      {date, msg} ->
        case (try do
                :rb_format_supp.print(date, msg, device)
              catch
                :error, e -> {:EXIT, {e, __STACKTRACE__}}
                :exit, e -> {:EXIT, e}
                e -> e
              end) do
          {:EXIT, _} ->
            handle_bad_form(date, msg, device, abort, log)

          _ ->
            {:proceed, device}
        end

      _ ->
        :io.format('rb: Cannot read from file~n')
        {:proceed, device}
    end
  end

  defp handle_bad_form(date, msg, device, abort, log) do
    :io.format(
      'rb: ERROR! A report on bad form was encountered. ' ++
        'It cannot be printed to the log.~n~n'
    )

    :io.format('Details:~n~p ~tp~n~n', [date, msg])

    case {abort, device, open_log_file(log)} do
      {true, :standard_io, :standard_io} ->
        :io.format('rb: Logging aborted.~n')
        {:abort, device}

      {false, :standard_io, :standard_io} ->
        :io.format('rb: Logging resumed...~n~n')
        {:proceed, device}

      {_, _, :standard_io} ->
        :io.format('rb: Can not reopen ~tp. Logging aborted.~n', [log])
        {:abort, device}

      {true, _, newDevice} ->
        :io.format(
          newDevice,
          '~n~n************************* RB ERROR ************************~n' ++
            'A report on bad form was encountered here and the logging~n' ++
            'process was aborted. Note that there may well be remaining~n' ++
            'reports that haven\'t yet been logged. Please see the rb~n' ++
            'manual for more info.~n' ++
            '***********************************************************~n',
          []
        )

        :io.format('rb: Logging aborted.~n')
        {:abort, newDevice}

      {false, _, newDevice} ->
        :io.format(newDevice, '~n   ********* RB: UNPRINTABLE REPORT ********~n~n', [])
        :io.format('rb: Logging resumed...~n~n')
        {:proceed, newDevice}
    end
  end

  defp read_rep_msg(fd, filePosition) do
    {:ok, _} = :file.position(fd, {:bof, filePosition})

    res =
      case (try do
              read_report(fd)
            catch
              :error, e -> {:EXIT, {e, __STACKTRACE__}}
              :exit, e -> {:EXIT, e}
              e -> e
            end) do
        {:ok, report} ->
          {_ShortDescr, date} = get_short_descr(report)
          {date, report}

        _ ->
          :error
      end

    :ok = :file.close(fd)
    res
  end
end
