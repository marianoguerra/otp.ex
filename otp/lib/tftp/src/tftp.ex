defmodule :m_tftp do
  use Bitwise
  require Record

  Record.defrecord(:r_tftp_msg_req, :tftp_msg_req,
    access: :undefined,
    filename: :undefined,
    mode: :undefined,
    options: :undefined,
    local_filename: :undefined
  )

  Record.defrecord(:r_tftp_msg_data, :tftp_msg_data,
    block_no: :undefined,
    data: :undefined
  )

  Record.defrecord(:r_tftp_msg_ack, :tftp_msg_ack, block_no: :undefined)

  Record.defrecord(:r_tftp_msg_error, :tftp_msg_error,
    code: :undefined,
    text: :undefined,
    details: :undefined
  )

  Record.defrecord(:r_tftp_msg_oack, :tftp_msg_oack, options: :undefined)

  Record.defrecord(:r_config, :config,
    parent_pid: self(),
    udp_socket: :undefined,
    udp_options: [:binary, {:reuseaddr, true}, {:active, :once}],
    udp_host: 'localhost',
    udp_port: 69,
    port_policy: :random,
    use_tsize: false,
    max_tsize: :infinity,
    max_conn: :infinity,
    rejected: [],
    polite_ack: false,
    debug_level: :none,
    timeout: :undefined,
    user_options: [],
    callbacks: [],
    logger: :tftp_logger,
    max_retries: 5
  )

  Record.defrecord(:r_callback, :callback,
    regexp: :undefined,
    internal: :undefined,
    module: :undefined,
    state: :undefined,
    block_no: :undefined,
    count: :undefined
  )

  def read_file(remoteFilename, localFilename, options) do
    :tftp_engine.client_start(:read, remoteFilename, localFilename, options)
  end

  def write_file(remoteFilename, localFilename, options) do
    :tftp_engine.client_start(:write, remoteFilename, localFilename, options)
  end

  def start(options) do
    :tftp_engine.daemon_start(options)
  end

  def info(pid) do
    :tftp_engine.info(pid)
  end

  def change_config(pid, options) do
    :tftp_engine.change_config(pid, options)
  end

  def start() do
    :application.start(:tftp)
  end

  def stop() do
    :application.stop(:tftp)
  end

  def start_standalone(options) do
    start(options)
  end

  def start_service(options) do
    :tftp_sup.start_child(options)
  end

  def stop_service(pid) do
    :tftp_sup.stop_child(pid)
  end

  def services() do
    :tftp_sup.which_children()
  end

  def service_info(pid) do
    info(pid)
  end
end
