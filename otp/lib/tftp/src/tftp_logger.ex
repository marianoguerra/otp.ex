defmodule :m_tftp_logger do
  use Bitwise

  def behaviour_info(:callbacks) do
    [{:error_msg, 2}, {:warning_msg, 2}, {:info_msg, 2}]
  end

  def behaviour_info(_) do
    :undefined
  end

  def error_msg(format, data) do
    {format2, data2} = add_timestamp(format, data)
    :error_logger.error_msg(format2, data2)
  end

  def warning_msg(format, data) do
    {format2, data2} = add_timestamp(format, data)
    :error_logger.warning_msg(format2, data2)
  end

  def info_msg(format, data) do
    {format2, data2} = add_timestamp(format, data)
    :io.format(format2, data2)
  end

  defp add_timestamp(format, data) do
    time = :erlang.timestamp()
    {{_Y, _Mo, _D}, {h, mi, s}} = :calendar.now_to_universal_time(time)
    {'~s:~s:~s tftp: ' ++ format, [t(h), t(mi), t(s) | data]}
  end

  defp t(int) do
    case :erlang.integer_to_list(int) do
      [single] ->
        [?0, single]

      multi ->
        multi
    end
  end
end
