defmodule :m_calendar do
  use Bitwise

  def date_to_gregorian_days(year, month, day)
      when is_integer(day) and
             day > 0 do
    last = last_day_of_the_month(year, month)

    cond do
      day <= last ->
        dy(year) + dm(month) + df(year, month) + day - 1
    end
  end

  def date_to_gregorian_days({year, month, day}) do
    date_to_gregorian_days(year, month, day)
  end

  def datetime_to_gregorian_seconds({date, time}) do
    86400 * date_to_gregorian_days(date) + time_to_seconds(time)
  end

  def day_of_the_week(year, month, day) do
    rem(date_to_gregorian_days(year, month, day) + 5, 7) + 1
  end

  def day_of_the_week({year, month, day}) do
    day_of_the_week(year, month, day)
  end

  def gregorian_days_to_date(days) do
    {year, dayOfYear} = day_to_year(days)
    {month, dayOfMonth} = year_day_to_date(year, dayOfYear)
    {year, month, dayOfMonth}
  end

  def gregorian_seconds_to_datetime(secs) when secs >= 0 do
    days = div(secs, 86400)
    rest = rem(secs, 86400)
    {gregorian_days_to_date(days), seconds_to_time(rest)}
  end

  def is_leap_year(y) when is_integer(y) and y >= 0 do
    is_leap_year1(y)
  end

  defp is_leap_year1(year)
       when rem(year, 4) === 0 and
              rem(year, 100) > 0 do
    true
  end

  defp is_leap_year1(year) when rem(year, 400) === 0 do
    true
  end

  defp is_leap_year1(_) do
    false
  end

  def iso_week_number() do
    {date, _} = local_time()
    iso_week_number(date)
  end

  def iso_week_number({year, month, day}) do
    d = date_to_gregorian_days({year, month, day})
    w01_1_Year = gregorian_days_of_iso_w01_1(year)
    w01_1_NextYear = gregorian_days_of_iso_w01_1(year + 1)

    cond do
      w01_1_Year <= d and d < w01_1_NextYear ->
        {year, div(d - w01_1_Year, 7) + 1}

      d < w01_1_Year ->
        pWN =
          case day_of_the_week(year - 1, 1, 1) do
            4 ->
              53

            _ ->
              case day_of_the_week(year - 1, 12, 31) do
                4 ->
                  53

                _ ->
                  52
              end
          end

        {year - 1, pWN}

      w01_1_NextYear <= d ->
        {year + 1, 1}
    end
  end

  def last_day_of_the_month(y, m) when is_integer(y) and y >= 0 do
    last_day_of_the_month1(y, m)
  end

  defp last_day_of_the_month1(_, 4) do
    30
  end

  defp last_day_of_the_month1(_, 6) do
    30
  end

  defp last_day_of_the_month1(_, 9) do
    30
  end

  defp last_day_of_the_month1(_, 11) do
    30
  end

  defp last_day_of_the_month1(y, 2) do
    case is_leap_year(y) do
      true ->
        29

      _ ->
        28
    end
  end

  defp last_day_of_the_month1(_, m)
       when is_integer(m) and m > 0 and
              m < 13 do
    31
  end

  def local_time() do
    :erlang.localtime()
  end

  def local_time_to_universal_time(dateTime) do
    :erlang.localtime_to_universaltime(dateTime)
  end

  def local_time_to_universal_time(dateTime, isDst) do
    :erlang.localtime_to_universaltime(dateTime, isDst)
  end

  def local_time_to_universal_time_dst(dateTime) do
    utDst =
      :erlang.localtime_to_universaltime(
        dateTime,
        true
      )

    ut = :erlang.localtime_to_universaltime(dateTime, false)
    ltDst = :erlang.universaltime_to_localtime(utDst)
    lt = :erlang.universaltime_to_localtime(ut)

    case {ltDst, lt} do
      {^dateTime, ^dateTime} when utDst !== ut ->
        [utDst, ut]

      {^dateTime, _} ->
        [utDst]

      {_, ^dateTime} ->
        [ut]

      {_, _} ->
        []
    end
  end

  def now_to_datetime({mSec, sec, _uSec}) do
    system_time_to_datetime(mSec * 1_000_000 + sec)
  end

  def now_to_universal_time(now) do
    now_to_datetime(now)
  end

  def now_to_local_time({mSec, sec, _uSec}) do
    :erlang.universaltime_to_localtime(now_to_universal_time({mSec, sec, _uSec}))
  end

  def rfc3339_to_system_time(dateTimeString) do
    rfc3339_to_system_time(dateTimeString, [])
  end

  def rfc3339_to_system_time(dateTimeString, options) do
    unit = :proplists.get_value(:unit, options, :second)

    [
      y1,
      y2,
      y3,
      y4,
      ?-,
      mon1,
      mon2,
      ?-,
      d1,
      d2,
      _T,
      h1,
      h2,
      ?:,
      min1,
      min2,
      ?:,
      s1,
      s2
      | timeStr
    ] = dateTimeString

    hour = :erlang.list_to_integer([h1, h2])
    min = :erlang.list_to_integer([min1, min2])
    sec = :erlang.list_to_integer([s1, s2])
    year = :erlang.list_to_integer([y1, y2, y3, y4])
    month = :erlang.list_to_integer([mon1, mon2])
    day = :erlang.list_to_integer([d1, d2])
    dateTime = {{year, month, day}, {hour, min, sec}}

    isFractionChar = fn c ->
      (c >= ?0 and c <= ?9) or c === ?.
    end

    {fractionStr, utcOffset} = :lists.splitwith(isFractionChar, timeStr)
    time = datetime_to_system_time(dateTime)
    secs = time - offset_string_adjustment(time, :second, utcOffset)
    check(dateTimeString, options, secs)
    scaledEpoch = :erlang.convert_time_unit(secs, :second, unit)

    scaledEpoch +
      copy_sign(
        fraction(unit, fractionStr),
        scaledEpoch
      )
  end

  def seconds_to_daystime(secs) do
    days0 = div(secs, 86400)
    secs0 = rem(secs, 86400)

    cond do
      secs0 < 0 ->
        {days0 - 1, seconds_to_time(secs0 + 86400)}

      true ->
        {days0, seconds_to_time(secs0)}
    end
  end

  def seconds_to_time(secs) when secs >= 0 and secs < 86400 do
    secs0 = rem(secs, 86400)
    hour = div(secs0, 3600)
    secs1 = rem(secs0, 3600)
    minute = div(secs1, 60)
    second = rem(secs1, 60)
    {hour, minute, second}
  end

  def system_time_to_local_time(time, timeUnit) do
    universalDate =
      system_time_to_universal_time(
        time,
        timeUnit
      )

    :erlang.universaltime_to_localtime(universalDate)
  end

  def system_time_to_universal_time(time, timeUnit) do
    secs = :erlang.convert_time_unit(time, timeUnit, :second)
    system_time_to_datetime(secs)
  end

  def system_time_to_rfc3339(time) do
    system_time_to_rfc3339(time, [])
  end

  def system_time_to_rfc3339(time, options) do
    unit = :proplists.get_value(:unit, options, :second)
    offsetOption = :proplists.get_value(:offset, options, '')
    t = :proplists.get_value(:time_designator, options, ?T)
    adjustmentSecs = offset_adjustment(time, unit, offsetOption)
    offset = offset(offsetOption, adjustmentSecs)
    adjustment = :erlang.convert_time_unit(adjustmentSecs, :second, unit)
    adjustedTime = time + adjustment
    factor = factor(unit)
    secs = div(adjustedTime, factor)
    check(time, options, secs)
    dateTime = system_time_to_datetime(secs)
    {{year, month, day}, {hour, min, sec}} = dateTime
    fractionStr = fraction_str(factor, adjustedTime)

    l = [
      pad4(year),
      '-',
      pad2(month),
      '-',
      pad2(day),
      [t],
      pad2(hour),
      ':',
      pad2(min),
      ':',
      pad2(sec),
      fractionStr,
      offset
    ]

    :lists.append(l)
  end

  def time_difference(
        {{y1, mo1, d1}, {h1, mi1, s1}},
        {{y2, mo2, d2}, {h2, mi2, s2}}
      ) do
    secs =
      datetime_to_gregorian_seconds({{y2, mo2, d2}, {h2, mi2, s2}}) -
        datetime_to_gregorian_seconds({{y1, mo1, d1}, {h1, mi1, s1}})

    seconds_to_daystime(secs)
  end

  def time_to_seconds({h, m, s})
      when is_integer(h) and
             is_integer(m) and is_integer(s) do
    h * 3600 + m * 60 + s
  end

  def universal_time() do
    :erlang.universaltime()
  end

  def universal_time_to_local_time(dateTime) do
    :erlang.universaltime_to_localtime(dateTime)
  end

  def valid_date(y, m, d)
      when is_integer(y) and
             is_integer(m) and is_integer(d) do
    valid_date1(y, m, d)
  end

  defp valid_date1(y, m, d)
       when y >= 0 and m > 0 and m < 13 and
              d > 0 do
    d <= last_day_of_the_month(y, m)
  end

  defp valid_date1(_, _, _) do
    false
  end

  def valid_date({y, m, d}) do
    valid_date(y, m, d)
  end

  defp day_to_year(dayOfEpoch) when dayOfEpoch >= 0 do
    yMax = div(dayOfEpoch, 365)
    yMin = div(dayOfEpoch, 366)
    {y1, d1} = dty(yMin, yMax, dayOfEpoch, dy(yMin), dy(yMax))
    {y1, dayOfEpoch - d1}
  end

  defp dty(min, max, _D1, dMin, _DMax) when min == max do
    {min, dMin}
  end

  defp dty(min, max, d1, dMin, dMax) do
    diff = max - min
    mid = min + div(diff * (d1 - dMin), dMax - dMin)

    midLength =
      case is_leap_year(mid) do
        true ->
          366

        false ->
          365
      end

    case dy(mid) do
      d2 when d1 < d2 ->
        newMax = mid - 1
        dty(min, newMax, d1, dMin, dy(newMax))

      d2 when d1 - d2 >= midLength ->
        newMin = mid + 1
        dty(newMin, max, d1, dy(newMin), dMax)

      d2 ->
        {mid, d2}
    end
  end

  defp gregorian_days_of_iso_w01_1(year) do
    d0101 = date_to_gregorian_days(year, 1, 1)
    dOW = day_of_the_week(year, 1, 1)

    cond do
      dOW <= 4 ->
        d0101 - dOW + 1

      true ->
        d0101 + 7 - dOW + 1
    end
  end

  defp year_day_to_date(year, dayOfYear) do
    extraDay =
      case is_leap_year(year) do
        true ->
          1

        false ->
          0
      end

    {month, day} = year_day_to_date2(extraDay, dayOfYear)
    {month, day + 1}
  end

  defp year_day_to_date2(_, day) when day < 31 do
    {1, day}
  end

  defp year_day_to_date2(e, day) when 31 <= day and day < 59 + e do
    {2, day - 31}
  end

  defp year_day_to_date2(e, day) when 59 + e <= day and day < 90 + e do
    {3, day - (59 + e)}
  end

  defp year_day_to_date2(e, day)
       when 90 + e <= day and
              day < 120 + e do
    {4, day - (90 + e)}
  end

  defp year_day_to_date2(e, day)
       when 120 + e <= day and
              day < 151 + e do
    {5, day - (120 + e)}
  end

  defp year_day_to_date2(e, day)
       when 151 + e <= day and
              day < 181 + e do
    {6, day - (151 + e)}
  end

  defp year_day_to_date2(e, day)
       when 181 + e <= day and
              day < 212 + e do
    {7, day - (181 + e)}
  end

  defp year_day_to_date2(e, day)
       when 212 + e <= day and
              day < 243 + e do
    {8, day - (212 + e)}
  end

  defp year_day_to_date2(e, day)
       when 243 + e <= day and
              day < 273 + e do
    {9, day - (243 + e)}
  end

  defp year_day_to_date2(e, day)
       when 273 + e <= day and
              day < 304 + e do
    {10, day - (273 + e)}
  end

  defp year_day_to_date2(e, day)
       when 304 + e <= day and
              day < 334 + e do
    {11, day - (304 + e)}
  end

  defp year_day_to_date2(e, day) when 334 + e <= day do
    {12, day - (334 + e)}
  end

  defp dy(y) when y <= 0 do
    0
  end

  defp dy(y) do
    x = y - 1
    div(x, 4) - div(x, 100) + div(x, 400) + x * 365 + 366
  end

  defp dm(1) do
    0
  end

  defp dm(2) do
    31
  end

  defp dm(3) do
    59
  end

  defp dm(4) do
    90
  end

  defp dm(5) do
    120
  end

  defp dm(6) do
    151
  end

  defp dm(7) do
    181
  end

  defp dm(8) do
    212
  end

  defp dm(9) do
    243
  end

  defp dm(10) do
    273
  end

  defp dm(11) do
    304
  end

  defp dm(12) do
    334
  end

  defp df(_, month) when month < 3 do
    0
  end

  defp df(year, _) do
    case is_leap_year(year) do
      true ->
        1

      false ->
        0
    end
  end

  defp check(_Arg, _Options, secs)
       when secs >= -(719_528 * 86400) and
              secs < 2_932_897 * 86400 do
    :ok
  end

  defp check(arg, options, _Secs) do
    :erlang.error({:badarg, [arg, options]})
  end

  defp datetime_to_system_time(dateTime) do
    datetime_to_gregorian_seconds(dateTime) - 719_528 * 86400
  end

  defp system_time_to_datetime(seconds) do
    gregorian_seconds_to_datetime(seconds + 719_528 * 86400)
  end

  defp offset(offsetOption, secs0)
       when offsetOption === '' or
              is_integer(offsetOption) do
    sign =
      case secs0 < 0 do
        true ->
          ?-

        false ->
          ?+
      end

    secs = abs(secs0)
    hour = div(secs, 3600)
    min = div(rem(secs, 3600), 60)
    [sign | :lists.append([pad2(hour), ':', pad2(min)])]
  end

  defp offset(offsetOption, _Secs) do
    offsetOption
  end

  defp offset_adjustment(time, unit, '') do
    local_offset(time, unit)
  end

  defp offset_adjustment(time, unit, offsetString)
       when is_list(offsetString) do
    offset_string_adjustment(time, unit, offsetString)
  end

  defp offset_adjustment(_Time, unit, offset) when is_integer(offset) do
    :erlang.convert_time_unit(offset, unit, :second)
  end

  defp offset_string_adjustment(_Time, _Unit, 'Z') do
    0
  end

  defp offset_string_adjustment(_Time, _Unit, 'z') do
    0
  end

  defp offset_string_adjustment(_Time, _Unit, tz) do
    [sign, h1, h2, ?:, m1, m2] = tz
    hour = :erlang.list_to_integer([h1, h2])
    min = :erlang.list_to_integer([m1, m2])
    adjustment = 3600 * hour + 60 * min

    case sign do
      ?- ->
        -adjustment

      ?+ ->
        adjustment
    end
  end

  defp local_offset(systemTime, unit) do
    universalTime =
      system_time_to_universal_time(
        systemTime,
        unit
      )

    localTime = :erlang.universaltime_to_localtime(universalTime)
    localSecs = datetime_to_gregorian_seconds(localTime)
    universalSecs = datetime_to_gregorian_seconds(universalTime)
    localSecs - universalSecs
  end

  defp fraction_str(1, _Time) do
    ''
  end

  defp fraction_str(factor, time) do
    fraction = rem(time, factor)
    s = :erlang.integer_to_list(abs(fraction))
    [?. | pad(log10(factor) - length(s), s)]
  end

  defp fraction(:second, _) do
    0
  end

  defp fraction(_, '') do
    0
  end

  defp fraction(unit, fractionStr) do
    round(
      factor(unit) *
        :erlang.list_to_float([
          ?0
          | fractionStr
        ])
    )
  end

  defp copy_sign(n1, n2) when n2 < 0 do
    -n1
  end

  defp copy_sign(n1, _N2) do
    n1
  end

  defp factor(:second) do
    1
  end

  defp factor(:millisecond) do
    1000
  end

  defp factor(:microsecond) do
    1_000_000
  end

  defp factor(:nanosecond) do
    1_000_000_000
  end

  defp log10(1000) do
    3
  end

  defp log10(1_000_000) do
    6
  end

  defp log10(1_000_000_000) do
    9
  end

  defp pad(0, s) do
    s
  end

  defp pad(i, s) do
    [?0 | pad(i - 1, s)]
  end

  defp pad2(n) when n < 10 do
    [?0 | :erlang.integer_to_list(n)]
  end

  defp pad2(n) do
    :erlang.integer_to_list(n)
  end

  defp pad4(n) when n < 10 do
    [?0, ?0, ?0 | :erlang.integer_to_list(n)]
  end

  defp pad4(n) when n < 100 do
    [?0, ?0 | :erlang.integer_to_list(n)]
  end

  defp pad4(n) when n < 1000 do
    [?0 | :erlang.integer_to_list(n)]
  end

  defp pad4(n) do
    :erlang.integer_to_list(n)
  end
end
