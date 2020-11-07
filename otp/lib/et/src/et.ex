defmodule :m_et do
  use Bitwise

  def trace_me(detailLevel, fromTo, label, contents)
      when is_integer(detailLevel) do
    :et.trace_me(detailLevel, fromTo, fromTo, label, contents)
  end

  def trace_me(detailLevel, _From, _To, _Label, _Contents)
      when is_integer(detailLevel) do
    :hopefully_traced
  end

  def phone_home(detailLevel, fromTo, label, contents) do
    :et.trace_me(detailLevel, fromTo, fromTo, label, contents)
  end

  def phone_home(detailLevel, from, to, label, contents) do
    :et.trace_me(detailLevel, from, to, label, contents)
  end

  def report_event(detailLevel, fromTo, label, contents) do
    :et.trace_me(detailLevel, fromTo, fromTo, label, contents)
  end

  def report_event(detailLevel, from, to, label, contents)
      when is_integer(detailLevel) do
    :et.trace_me(detailLevel, from, to, label, contents)
  end
end
