defmodule :m_ttb_autostart do
  use Bitwise
  @behaviour :gen_server
  def start_link() do
    :gen_server.start_link(:ttb_autostart, :no_args, [])
  end

  def delete_config() do
    :file.delete('ttb_autostart.bin')
  end

  def read_config() do
    case :file.read_file('ttb_autostart.bin') do
      {:ok, data} ->
        {:ok, :erlang.binary_to_term(data)}

      error ->
        error
    end
  end

  def write_config(data) do
    :file.write_file('ttb_autostart.bin', :erlang.term_to_binary(data))
  end

  def init(:no_args) do
    case :application.get_env(
           :runtime_tools,
           :ttb_autostart_module
         ) do
      {:ok, _} ->
        :ok

      :undefined ->
        :application.set_env(:runtime_tools, :ttb_autostart_module, :ttb_autostart)
    end

    :observer_backend.ttb_resume_trace()
    {:ok, :no_args, 10000}
  end

  def handle_call(_, _, _) do
    {:noreply, :no_args}
  end

  def handle_cast(_, _) do
    {:noreply, :no_args}
  end

  def handle_info(:timeout, _) do
    {:stop, :normal, :no_args}
  end

  def terminate(_, _) do
    :ok
  end

  def code_change(_, _, _) do
    {:ok, :no_args}
  end
end
