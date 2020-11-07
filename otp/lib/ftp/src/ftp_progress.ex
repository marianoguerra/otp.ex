defmodule :m_ftp_progress do
  use Bitwise
  require Record

  Record.defrecord(:r_file_info, :file_info,
    size: :undefined,
    type: :undefined,
    access: :undefined,
    atime: :undefined,
    mtime: :undefined,
    ctime: :undefined,
    mode: :undefined,
    links: :undefined,
    major_device: :undefined,
    minor_device: :undefined,
    inode: :undefined,
    uid: :undefined,
    gid: :undefined
  )

  Record.defrecord(:r_file_descriptor, :file_descriptor,
    module: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_progress, :progress,
    file: :undefined,
    cb_module: :undefined,
    cb_function: :undefined,
    init_progress_term: :undefined,
    current_progress_term: :undefined
  )

  def start_link(:ignore) do
    :ignore
  end

  def start_link(options) do
    spawn_link(:ftp_progress, :init, [options])
  end

  def report(pid, report) do
    send(pid, {:progress_report, report})
    :ok
  end

  def stop(pid) do
    send(pid, :stop)
    :ok
  end

  def init(options) do
    loop(progress(options))
  end

  defp loop(progress) do
    receive do
      {:progress_report, report} ->
        newProgress = report_progress(report, progress)
        loop(newProgress)

      :stop ->
        :ok
    end
  end

  defp progress({cBModule, cBFunction, initProgressTerm})
       when is_atom(cBModule) and is_atom(cBFunction) do
    r_progress(
      cb_module: cBModule,
      cb_function: cBFunction,
      init_progress_term: initProgressTerm,
      current_progress_term: initProgressTerm
    )
  end

  defp report_progress({:local_file, file}, progress) do
    {:ok, fileInfo} = :file.read_file_info(file)

    report_progress(
      {:file_size, r_file_info(fileInfo, :size)},
      r_progress(progress, file: file)
    )
  end

  defp report_progress({:remote_file, file}, progress) do
    report_progress(
      {:file_size, :unknown},
      r_progress(progress, file: file)
    )
  end

  defp report_progress(
         size,
         r_progress(
           file: file,
           cb_module: cBModule,
           cb_function: cBFunction,
           current_progress_term: term,
           init_progress_term: initTerm
         ) = progress
       ) do
    newProgressTerm = apply(cBModule, cBFunction, [term, file, size])

    case size do
      {:transfer_size, 0} ->
        r_progress(progress,
          current_progress_term: initTerm,
          file: :undefined
        )

      _ ->
        r_progress(progress, current_progress_term: newProgressTerm)
    end
  end
end
