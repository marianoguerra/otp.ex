defmodule :m_etop_txt do
  use Bitwise
  import :etop, only: [loadinfo: 2, meminfo: 2]
  @author :"siri@erix.ericsson.se"
  require Record

  Record.defrecord(:r_etop_info, :etop_info,
    now: {0, 0, 0},
    n_procs: 0,
    wall_clock: :undefined,
    runtime: :undefined,
    run_queue: 0,
    alloc_areas: [],
    memi: [{:total, 0}, {:processes, 0}, {:ets, 0}, {:atom, 0}, {:code, 0}, {:binary, 0}],
    procinfo: []
  )

  Record.defrecord(:r_etop_proc_info, :etop_proc_info,
    pid: :undefined,
    mem: 0,
    reds: 0,
    name: :undefined,
    runtime: 0,
    cf: :undefined,
    mq: 0
  )

  Record.defrecord(:r_opts, :opts,
    node: node(),
    port: 8415,
    accum: false,
    intv: 5000,
    lines: 10,
    width: 700,
    height: 340,
    sort: :runtime,
    tracing: :on,
    out_mod: :etop_txt,
    out_proc: :undefined,
    server: :undefined,
    host: :undefined,
    tracer: :undefined,
    store: :undefined,
    accum_tab: :undefined,
    remote: :undefined
  )

  def stop(pid) do
    send(pid, :stop)
  end

  def init(config) do
    loop(r_etop_info(), config)
  end

  defp loop(prev, config) do
    info = do_update(prev, config)

    receive do
      :stop ->
        :stopped

      {:dump, fd} ->
        do_update(fd, info, prev, config)
        loop(info, config)

      {:config, _, config1} ->
        loop(info, config1)
    after
      r_opts(config, :intv) ->
        loop(info, config)
    end
  end

  defp do_update(prev, config) do
    info = :etop.update(config)
    do_update(:standard_io, info, prev, config)
  end

  def do_update(fd, info, prev, config) do
    {cpu, nProcs, rQ, clock} = loadinfo(info, prev)
    :io.nl(fd)
    writedoubleline(fd)

    case r_etop_info(info, :memi) do
      :undefined ->
        :io.fwrite(fd, ' ~-72w~10s~n Load:  cpu  ~8w~n        procs~8w~n        runq ~8w~n', [
          r_opts(config, :node),
          clock,
          cpu,
          nProcs,
          rQ
        ])

      memi ->
        [tot, procs, atom, bin, code, ets] =
          meminfo(
            memi,
            [:total, :processes, :atom, :binary, :code, :ets]
          )

        :io.fwrite(
          fd,
          ' ~-72w~10s~n Load:  cpu  ~8w               Memory:  total    ~8w    binary   ~8w~n        procs~8w                        processes~8w    code     ~8w~n        runq ~8w                        atom     ~8w    ets      ~8w~n',
          [r_opts(config, :node), clock, cpu, tot, bin, nProcs, procs, code, rQ, atom, ets]
        )
    end

    :io.nl(fd)
    writepinfo_header(fd)
    writesingleline(fd)
    writepinfo(fd, r_etop_info(info, :procinfo), modifier(fd))
    writedoubleline(fd)
    :io.nl(fd)
    info
  end

  defp writepinfo_header(fd) do
    :io.fwrite(
      fd,
      'Pid            Name or Initial Func    Time    Reds  Memory    MsgQ Current Function~n',
      []
    )
  end

  defp writesingleline(fd) do
    :io.fwrite(
      fd,
      '----------------------------------------------------------------------------------------~n',
      []
    )
  end

  defp writedoubleline(fd) do
    :io.fwrite(
      fd,
      '========================================================================================~n',
      []
    )
  end

  defp writepinfo(
         fd,
         [
           r_etop_proc_info(
             pid: pid,
             mem: mem,
             reds: reds,
             name: name,
             runtime: time,
             cf: mFA,
             mq: mQ
           )
           | t
         ],
         modifier
       ) do
    :io.fwrite(fd, proc_format(modifier), [
      pid,
      to_string(name, modifier),
      time,
      reds,
      mem,
      mQ,
      to_string(
        mFA,
        modifier
      )
    ])

    writepinfo(fd, t, modifier)
  end

  defp writepinfo(_Fd, [], _) do
    :ok
  end

  defp proc_format(modifier) do
    '~-15w~-20' ++ modifier ++ 's~8w~8w~8w~8w ~-20' ++ modifier ++ 's~n'
  end

  defp to_string({m, f, a}, modifier) do
    :io_lib.format('~w:~' ++ modifier ++ 'w/~w', [m, f, a])
  end

  defp to_string(other, modifier) do
    :io_lib.format('~' ++ modifier ++ 'w', [other])
  end

  defp modifier(device) do
    case encoding(device) do
      :latin1 ->
        ''

      _ ->
        't'
    end
  end

  defp encoding(device) do
    case :io.getopts(device) do
      list when is_list(list) ->
        :proplists.get_value(:encoding, list, :latin1)

      _ ->
        :latin1
    end
  end
end
